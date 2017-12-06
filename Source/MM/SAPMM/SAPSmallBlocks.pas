{****************************************************************************************

  SAPMM v1.01 /17.06.2013/

  Working with small blocks.
  Small blocks have the size <= cMaxSmallBlockSize.
  Small blocks are arranged in size lists with a step of cSmallBlockStep
  Small blocks are allocated by groups ("pieces"), each time group grows twice in size, 
  to the limit of cSmallBlocksMaxPieceSize bytes in group

****************************************************************************************}

unit SAPSmallBlocks;

interface

{$Include SAPOptions.inc}

uses SAPDefs;

function AllocateSmallBlock(mm: PSAPThreadMM; size: LongWord): PSAPSmallAllocatedBlock; inline;

procedure FreeSmallBlock(mm: PSAPThreadMM; block: PSAPSmallAllocatedBlock); inline;

procedure FreeSmallBlocks(mm: PSAPThreadMM; h: PSAPSmallBlocksHeader);

//----------------------------------

function AllocateSmallBlocks(mm: PSAPThreadMM; inx: LongWord): PSAPSmallBlocksHeader;

procedure InitSmallBlocksTable(mm: PSAPThreadMM);

implementation

uses SAPNormalBlocks, SAPErrCodes;

//function AllocateSmallBlocks(mm: PSAPThreadMM; inx: LongWord): PSAPSmallBlocksHeader;
//forward;

procedure InitSmallBlocksTable(mm: PSAPThreadMM);
var
  i: Integer;
  block_size: LongWord;
begin
  for i := Low(mm.small_table) to High(mm.small_table) do
  begin
    block_size := (i + 1) * cSmallBlockStep;
    mm.small_table[i].block_size := block_size;

    mm.small_table[i].max_blocks_in_piece :=
      (cSmallBlocksMaxPieceSize - SizeOf(TSAPSmallBlocksHeader)) div block_size;

    if block_size <= 128 then
      mm.small_table[i].blocks_in_piece := 256
    else
      mm.small_table[i].blocks_in_piece := 64
    ;
  end;
end;

function AllocateSmallBlock(mm: PSAPThreadMM; size: LongWord): PSAPSmallAllocatedBlock;
var
  inx: Integer;
  h: PSAPSmallBlocksHeader;
  f: PSAPSmallFreeBlock;
begin

  inx := (size + SizeOf(TSAPSmallAllocatedBlock)) div cSmallBlockStep;

  h := mm.small_table[inx].headers;
  if h = nil then
  begin
    h := AllocateSmallBlocks(mm, inx);
    if h = nil then
    begin
      Result := nil;
      Exit;
    end;
  end;

  f := h.blocks;

  h.blocks := f.next;
  Dec(h.free_count);

  if h.blocks = nil then
  begin
//    assert(mm.small_table[inx] = h);

    // delete from list
    mm.small_table[inx].headers := h.next;
    if h.next <> nil then 
      h.next.prev := nil;

    h.next := nil;
    h.prev := nil;
  end;

  f.block.tags := f.block.tags + [sap_allocated];

  Result := PSAPSmallAllocatedBlock(f);
end;

function AllocateSmallBlocks(mm: PSAPThreadMM; inx: LongWord): PSAPSmallBlocksHeader;
var
  block_size: LongWord;
  blocks_in_piece: LongWord;
  size: LongWord;
  p: Pointer;
  h: PSAPSmallBlocksHeader;
  x: PSAPSmallFreeBlock;
  i: Integer;
begin
  Result := nil;

  block_size := (inx + 1) * cSmallBlockStep;
  blocks_in_piece := mm.small_table[inx].blocks_in_piece;

  size :=
    ( block_size * blocks_in_piece        // space for the blocks
    + SizeOf(TSAPSmallBlocksHeader)       // small block header
    + SizeOf(TSAPAllocatedBlock)          // allocated block header
    + SizeOf(LongWord)                    // size at the end of the block
    + (cNormalAlignment - 1)              // alignment
    ) and not (cNormalAlignment - 1);

  p := AllocateNormalBlock(mm, size);
  if p = nil then 
    Exit;

  // Recalc for the next allocation
  if blocks_in_piece < mm.small_table[inx].max_blocks_in_piece then
  begin
    mm.small_table[inx].blocks_in_piece := blocks_in_piece * 2;
    if mm.small_table[inx].blocks_in_piece > mm.small_table[inx].max_blocks_in_piece then
      mm.small_table[inx].blocks_in_piece := mm.small_table[inx].max_blocks_in_piece;
  end;

  PSAPAllocatedBlock(p).tags := PSAPAllocatedBlock(p).tags + [sap_small_blocks_piece];

  h := Pointer(LongWord(p) + SizeOf(TSAPAllocatedBlock));

  h.header_magic := cSmallBlockHeaderMagic;
  h.block_mm := mm;
  h.block_size := block_size;
  h.blocks_no := blocks_in_piece;
  h.free_count := blocks_in_piece;

  h.blocks := nil;
  h.prev := nil;

  h.next := mm.small_table[inx].headers;
  mm.small_table[inx].headers := h;

  // Prepare the list of allocated small blocks
  x := Pointer(LongWord(h) + SizeOf(TSAPSmallBlocksHeader));

  for i := 1 to blocks_in_piece do
  begin

    x.block.magic := cMagic;

    x.block.tags := [sap_small_block];
    x.block.ofs := LongWord(x) - LongWord(h);

    x.header := h;

    x.next := h.blocks;
    h.blocks := x;

    x := Pointer(LongWord(x) + block_size);
  end;

  Result := h;
end;

procedure FreeSmallBlock(mm: PSAPThreadMM; block: PSAPSmallAllocatedBlock);
var
  h: PSAPSmallBlocksHeader;
  inx: Integer;
begin
  h := Pointer(LongWord(block) - block.ofs);

  {$IFDEF SAP_CHECKMAGIC}
  Assert(h.header_magic = cSmallBlockHeaderMagic);
  {$ENDIF}

  block.tags := block.tags - [sap_allocated];
  Inc(h.free_count);

  PSAPSmallFreeBlock(block).header := h;

  if h.blocks = nil then
  begin
    // insert block into the list of small blocks of appropriate size
    inx := h.block_size div cSmallBlockStep - 1;

    h.next := mm.small_table[inx].headers;
    h.prev := nil;

    if h.next <> nil then 
      h.next.prev := h;

    mm.small_table[inx].headers := h;
  end;

  PSAPSmallFreeBlock(block).next := h.blocks;
  h.blocks := PSAPSmallFreeBlock(block);

(* Never delete blocks. Maybe should delete if there are too much of them
  (if there are peak allocations of blocks of the same size)
*)
  // if all blocks of the group are free and the group is not single
  if (h.free_count = h.blocks_no) and ((h.next <> nil) or (h.prev <> nil)) then
    FreeSmallBlocks(mm, h);
//*)
end;

procedure FreeSmallBlocks(mm: PSAPThreadMM; h: PSAPSmallBlocksHeader);
var
  inx: Integer;
  p: PSAPAllocatedBlock;
begin
  inx := h.block_size div cSmallBlockStep - 1;
  if mm.small_table[inx].headers = h then
  begin
    mm.small_table[inx].headers := h.next;
    if h.next <> nil then 
      h.next.prev := nil;
  end
  else
  begin
    h.prev.next := h.next;
    if h.next <> nil then 
      h.next.prev := h.prev;
  end;

  p := Pointer(LongWord(h) - SizeOf(TSAPAllocatedBlock));
  p.tags := p.tags - [sap_small_blocks_piece];

  InsertIntoFreeList(mm, h);
end;

end.