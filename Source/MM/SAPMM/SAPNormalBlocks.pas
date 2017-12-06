{****************************************************************************************

  SAPMM v1.01 /17.06.2013/

  Normal blocks have the size from cMinMediumBlock to cMaxMediumBlock

  Working with normal (medium) blocks

****************************************************************************************}

unit SAPNormalBlocks;

interface

{$Include SAPOptions.inc}

uses SAPDefs;

// Normal block allocation. Block is always 8-aligned
function AllocateNormalBlock(mm: PSAPThreadMM; size: LongWord): Pointer;

// Put block into free blocks list
procedure InsertIntoFreeList(mm: PSAPThreadMM; p: Pointer);

implementation

uses
  SAPOSBlocks,
  SAPList,
  SAPErrCodes
;

//------------------------------------------------------------
// Allocation

function TryAllocateFreeBlock(mm: PSAPThreadMM; size: LongWord): Pointer;
forward;

function AllocateNormalBlock(mm: PSAPThreadMM; size: LongWord): Pointer;
var
  os_block: PSAPOSBlock;
  p: PSAPAllocatedBlock;
  psize: ^ LongWord;
  f: PSAPFreeBlock;
  inx: LongWord;
begin
  if size <= cMaxMediumBlock then
  begin
    inx := (size + cMediumBlockStep - 1) div cMediumBlockStep;
    p := Pointer(mm.medium_table[inx]);
    if p <> nil then
    begin
      UntieFreeBlock(mm.medium_table[inx], Pointer(p));
      p.tags := p.tags + [sap_allocated];
      Result := p;
      Exit;
    end;
  end;

  Result := TryAllocateFreeBlock(mm, size);
  if Result <> nil then // success
    Exit;

  if size > cOSBlockSize - SizeOf(TSAPOSBlock) then
  begin
    // Allocate whole block directly from OS
    os_block := SAPAllocateOSBlock(mm, size + SizeOf(TSAPOSBlock));
    if os_block = nil then 
      Exit;

    p := Pointer(LongWord(os_block) + SizeOf(TSAPOSBlock));
    p.size := size;   // to determine allocated size
    p.tags := [sap_allocated, sap_os_block_first, sap_os_block_last];
    p.magic := cMagic;
    p.block_magic := cNormalBlockMagic;
    p.block_mm := mm;

    // Set the size at the end of the block
    psize := Pointer(LongWord(p) + size - SizeOf(LongWord));
    psize^ := size;

    Result := p;

    {$IFDEF SAP_STAT}
    Dec(mm.no_get_normal);
    Inc(mm.no_get_os);
    {$ENDIF SAP_STAT}

    Exit;
  end;

// Allocate new OS block (of standard size), add it to free blocks list,
// then try to allocate needed normal block again
  os_block := SAPAllocateOSBlock(mm, cOSBlockSize);
  if os_block = nil then 
    Exit;

  f := Pointer(LongWord(os_block) + SizeOf(TSAPOSBlock));

  f.block.size := cOSBlockSize - SizeOf(TSAPOSBlock);
  f.block.tags := [sap_os_block_first, sap_os_block_last];
  f.block.magic := cMagic;
  f.block.block_magic := cNormalBlockMagic;
  f.block.block_mm := mm;

  psize := Pointer(LongWord(f) + f.block.size - SizeOf(LongWord));
  psize^ := f.block.size;

  // Add to free blocks list
  if mm.free_blocks = nil then
  begin
    f.next := f;
    f.prev := f;
    mm.free_blocks := f;
  end
  else begin
    f.next := mm.free_blocks;
    f.prev := mm.free_blocks.prev;
    f.next.prev := f;
    f.prev.next := f;
    mm.free_blocks := f;
  end;

  // Try to allocate block again
  Result := TryAllocateFreeBlock(mm, size);
end;

function TryAllocateFreeBlock(mm: PSAPThreadMM; size: LongWord): Pointer;
var
  delta: LongWord;
  x: PSAPFreeBlock;
  p: PSAPAllocatedBlock;
  psize: ^ LongWord;
begin
  Result := nil;

  if mm.free_blocks = nil then 
    Exit;

  x := mm.free_blocks;
  repeat

    {$IFDEF SAP_STAT}
    Inc(mm.no_alloc_find_cycles);
    {$ENDIF SAP_STAT}

    if x.block.size >= size then
    begin
      // Found suitable block
      delta := x.block.size - size;

      if delta < cMinMediumBlock then 
      begin
        // Allocate whole block
        x.block.tags := x.block.tags + [sap_allocated];

        // untie from list
        if x.next = x then // was the single block in list
          mm.free_blocks := nil
        else begin
          x.next.prev := x.prev;
          x.prev.next:= x.next;
          mm.free_blocks := x.next; // change free blocks list start pointer for the next search
        end;

        // Should not set the size at the end of the block!
        Result := x;
        Exit;
      end;

      // cut a piece of block from the end, so that the list is not changed
      p := Pointer(LongWord(x) + delta);
      p.size := size;
      p.tags := x.block.tags + [sap_allocated] - [sap_os_block_first];
      p.magic := cMagic;
      p.block_magic := cNormalBlockMagic;
      p.block_mm := mm;

      // Set the size at the end of the block
      psize := Pointer(LongWord(p) + size - SizeOf(LongWord));
      psize^ := size;

      x.block.size := delta;
      x.block.tags := x.block.tags - [sap_os_block_last];

      psize := Pointer(LongWord(x) + delta - SizeOf(LongWord));
      psize^ := delta;

      if delta <= cMaxMediumBlock then
      begin
        UntieFreeBlock(mm.free_blocks, x);
        TieFreeBlockToSizeList(mm, x, delta);
      end;

      Result := p;
      Exit;
    end;

    x := x.next;
  until x = mm.free_blocks;
end;

//--------------------------------------------------
// Deallocate

// Checks free block, returns 0 if everything is OK, otherwise return error code
function CheckFreeBlock(f: PSAPFreeBlock): Integer;
var
  size: LongWord;
  psize: ^ LongWord;
begin
// Checking alignment
  if LongWord(f) and (cAlignment - 1) <> 0 then
  begin
    Result := cErrInvPtrAlignment; // pointer is not aligned at cAlignment
    Exit;
  end;

// Checking size
  size := f.block.size;
  if size and (cAlignment - 1) <> 0 then
  begin
    Result := cErrInvSizeAlignment; // size is not aligned at cAlignment
    Exit;
  end;

  psize := Pointer(LongWord(f) + size - SizeOf(LongWord));

//TODO: should check the pointer here

  if psize^ <> size then
  begin
    Result := cErrSizesAreDifferent; // sizes at the beginning and at the end of the blocks differ

    Exit;
  end;

  if not (sap_allocated in f.block.tags) then
  begin
    if f.next = nil then
    begin
      Result := cErrDebugCheck1;
      Exit;
    end;

    if f.prev = nil then
    begin
      Result := cErrDebugCheck2;
      Exit;
    end;

    if f.next.prev <> f then
    begin
      Result := cErrDebugCheck3; // free blocks list error
      Exit;
    end;

    if f.prev.next <> f then
    begin
      Result := cErrDebugCheck4; // free blocks list error
      Exit;
    end;

  end;

  Result := 0;
end;

//--------------------------------------------

// Returns free block, succeeding given block, or nil
function GetNextFreeBlock(f: PSAPFreeBlock): PSAPFreeBlock;
var
  next: PSAPFreeBlock;
begin
  Result := nil;

  if sap_os_block_last in f.block.tags then 
    Exit;

  next := Pointer(LongWord(f) + f.block.size);

  if sap_allocated in next.block.tags then 
    Exit;

  {$IFDEF SAP_CHECKMAGIC}
  if CheckFreeBlock(next) <> 0 then
    Exit;
  {$ENDIF SAP_CHECKMAGIC}

  Result := next;
end;

// Return free block, preceding given block, or nil
function GetPrevFreeBlock(f: PSAPFreeBlock): PSAPFreeBlock;
var
  psize: ^ LongWord;
  prev: PSAPFreeBlock;
begin
  Result := nil;

  if sap_os_block_first in f.block.tags then 
    Exit;

  psize := Pointer(LongWord(f) - SizeOf(LongWord));

  if psize^ and (cAlignment - 1) <> 0 then 
    Exit;

  prev := Pointer(LongWord(f) - psize^);

  if sap_allocated in prev.block.tags then 
    Exit;

  {$IFDEF SAP_CHECKMAGIC}
  if CheckFreeBlock(prev) <> 0 then
    Exit;
  {$ENDIF SAP_CHECKMAGIC}

  Result := prev;
end;

procedure SetEndSize(f: PSAPFreeBlock); inline;
var
  psize: ^LongWord;
begin
  psize := Pointer(LongWord(f) + f.block.size - SizeOf(LongWord));
  psize^ := f.block.size;
end;

procedure InsertIntoFreeList(mm: PSAPThreadMM; p: Pointer);
var
  f: PSAPFreeBlock;
  next: PSAPFreeBlock;
  prev: PSAPFreeBlock;
  pred_size: LongWord;
begin
  f := Pointer(LongWord(p) - SizeOf(TSAPAllocatedBlock));

  next := GetNextFreeBlock(f);
  prev := GetPrevFreeBlock(f);

  if (prev <> nil) and (next <> nil) then
  begin
    // 1. Insertion with both sides merge
    // previous block is grown, next block is deleted from list

    pred_size := prev.block.size;
    Inc(prev.block.size, f.block.size + next.block.size);
    prev.block.tags := prev.block.tags + next.block.tags * [sap_os_block_last];

    SetEndSize(prev);

    UntieFreeBlockFromSizeList(mm, next, next.block.size);
    MoveFreeBlockToSizeList(mm, prev, pred_size);

    SAPCheckAndFreeOSBlock(mm, prev);

    Exit;
  end;

  if next <> nil then
  begin
    // 2. Merge with next block
    // Block must be grown and inserted into the list, next block is deleted from list

    Inc(f.block.size, next.block.size);
    f.block.tags := f.block.tags - [sap_allocated] + next.block.tags * [sap_os_block_last];

    UntieFreeBlockFromSizeList(mm, next, next.block.size);
    TieFreeBlockToSizeList(mm, f, f.block.size);

    SetEndSize(f);

    SAPCheckAndFreeOSBlock(mm, f);

    Exit;
  end;

  if prev <> nil then
  begin
    // 3. Merge with previous block

    pred_size := prev.block.size;
    Inc(prev.block.size, f.block.size);
    prev.block.tags := prev.block.tags + f.block.tags * [sap_os_block_last];

    SetEndSize(prev);

    MoveFreeBlockToSizeList(mm, prev, pred_size);

    SAPCheckAndFreeOSBlock(mm, prev);

    Exit;
  end;

  // 4. Insertion without merge
  f.block.tags := f.block.tags - [sap_allocated];

  TieFreeBlockToSizeList(mm, f, f.block.size);

  SAPCheckAndFreeOSBlock(mm, f);
end;

end.
