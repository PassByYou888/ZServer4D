{****************************************************************************************

  SAPMM v1.01 /17.06.2013/

  Working with large (OS) blocks

****************************************************************************************}

unit SAPOSBlocks;

interface

{$Include SAPOptions.inc}

uses SAPDefs;

var
  total_os_blocks: LongWord; // number of total OS blocks, allocated for all threads

function SAPAllocateOSBlock(mm: PSAPThreadMM; size: LongWord): PSAPOSBlock;

function SAPCheckAndFreeOSBlock(mm: PSAPThreadMM; f: PSAPFreeBlock): Boolean;

procedure SAPFreeOSBlock(mm: PSAPThreadMM; os: PSAPOSBlock);

implementation

uses Windows,
  SAPList

//, SAPDebugUtils
;

var
  os_blocks_pool: PSAPOSBlock;
  blocks_in_pool: LongWord;
  CritSect: _RTL_CRITICAL_SECTION;

//---------------------------------------------------

procedure FreeOSBlockToWindows(os: PSAPOSBlock);
forward;

procedure AddOSBlockToPool(os: PSAPOSBlock);
forward;

function GetOSBlockFromPool(size: LongWord): PSAPOSBlock;
forward;

procedure ClearPool;
forward;

//---------------------------------------------------

procedure InterlockedIncrement(var Value: LongWord);
asm
  lock inc [Value]
end;

procedure InterlockedDecrement(var Value: LongWord);
asm
  lock dec [Value]
end;

function SAPAllocateOSBlock(mm: PSAPThreadMM; size: LongWord): PSAPOSBlock;
var
  p: PSAPOSBlock;
begin
  Result := GetOSBlockFromPool(size);
  if Result = nil then Exit;

{$IFDEF SAP_STAT}
  Inc(mm.no_os_alloc);
{$ENDIF}

  p := Result;
  p.size := size;

  // Add block to list
  if mm.os_blocks = nil then
  begin
    p.next := nil;
    p.prev := nil;
    mm.os_blocks := p;
  end
  else begin
    // Insert as a second block
    p.next := mm.os_blocks.next;
    p.prev := mm.os_blocks;

    mm.os_blocks.next := p;

    if p.next <> nil then 
      p.next.prev := p;
  end;

  Inc(mm.no_os_blocks);
end; 

function SAPCheckAndFreeOSBlock(mm: PSAPThreadMM; f: PSAPFreeBlock): Boolean;
const
  cOSFree = [sap_os_block_first, sap_os_block_last];
var
  os: PSAPOSBlock;
begin
  Result := false;
  if f.block.tags * cOSFree <> cOSFree then Exit;

  os := Pointer(LongWord(f) - SizeOf(TSAPOSBlock));

  // Single block is not freed 
  if (mm.os_blocks = os) and (os.next = nil) then 
    Exit;

  UntieFreeBlock(mm.free_blocks, f);

  SAPFreeOSBlock(mm, os);

  Result := true;
end;

procedure SAPFreeOSBlock(mm: PSAPThreadMM; os: PSAPOSBlock);
begin
  if os.next <> nil then os.next.prev := os.prev;
  if os.prev <> nil then os.prev.next := os.next;

  if mm.os_blocks = os then mm.os_blocks := os.next;

  Dec(mm.no_os_blocks);

{$IFDEF SAP_STAT}
  Inc(mm.no_os_free);
{$ENDIF}

  // If block is not of standard size (larger than cOSBlockSize) 
  // or too many free blocks in pool, return the block directly to the OS
  if (os.size <> cOSBlockSize) or (blocks_in_pool > cMaxOSBlocksInPool) then
    FreeOSBlockToWindows(os)
  else
    AddOSBlockToPool(os);
end;

//-----------------------------------------------

procedure FreeOSBlockToWindows(os: PSAPOSBlock);
begin
  VirtualFree(os, 0, Windows.MEM_RELEASE);

  InterlockedDecrement(total_os_blocks);
end;

procedure AddOSBlockToPool(os: PSAPOSBlock);
begin
  os.next := nil;
  os.prev := nil;

  Windows.EnterCriticalSection(CritSect);

    os.pool_next := os_blocks_pool;
    os_blocks_pool := os;
    Inc(blocks_in_pool);

  Windows.LeaveCriticalSection(CritSect);
end;

function GetOSBlockFromPool(size: LongWord): PSAPOSBlock;
begin
  if (size <> cOSBlockSize) or (os_blocks_pool = nil) then
  begin
    Result := Windows.VirtualAlloc(nil, size, Windows.MEM_RESERVE + Windows.MEM_COMMIT, Windows.PAGE_READWRITE);
    if Result = nil then
    begin
      ClearPool;

      Result := Windows.VirtualAlloc(nil, size, Windows.MEM_RESERVE + Windows.MEM_COMMIT, Windows.PAGE_READWRITE);
      if Result = nil then Exit;
    end;

    InterlockedIncrement(total_os_blocks);

    Exit;
  end;

  Windows.EnterCriticalSection(CritSect);

    Result := os_blocks_pool;

    if Result <> nil then begin
      os_blocks_pool := os_blocks_pool.pool_next;
      Dec(blocks_in_pool);
    end;

  Windows.LeaveCriticalSection(CritSect);

  if Result = nil then
  begin
    Result := Windows.VirtualAlloc(nil, size, Windows.MEM_RESERVE + Windows.MEM_COMMIT, Windows.PAGE_READWRITE);
    if Result = nil then 
      Exit;

    InterlockedIncrement(total_os_blocks);
  end;
end;


// Returns ALL OS blocks from pool to the OS. This logic is used 
// if the MM could not allocate the block of non-standard size (larger than cOSBlockSize) 
// This is done in unsafe way (without sync), not to slow down other threads
procedure ClearPool;
var
  os: PSAPOSBlock;
  next: PSAPOSBlock;
begin
  asm
    mov	eax, 0
    lock xchg os_blocks_pool,eax
    mov os, eax
  end;

  while os <> nil do
  begin
    next := os.pool_next;

    VirtualFree(os, 0, Windows.MEM_RELEASE);
    InterlockedDecrement(total_os_blocks);

    os := next;
  end;
end;

initialization
  total_os_blocks := 0;
  os_blocks_pool := nil;
  blocks_in_pool := 0;

  Windows.InitializeCriticalSection(CritSect);
end.
