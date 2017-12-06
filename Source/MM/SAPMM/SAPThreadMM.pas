{****************************************************************************************

  SAPMM v1.01 /17.06.2013/

  Thread memory management

****************************************************************************************}

unit SAPThreadMM;

interface

{$Include SAPOptions.inc}

uses
  SAPDefs,
  SAPSmallBlocks,
  SAPSystem
  ;

// Create thread MM
function CreateThreadMM: PSAPThreadMM;

// Release all freed memory to OS on thread termination.
// If all blocks allocated by thread MM are released to OS, thread MM is destroyed,
// otherwise thread MM is kept with "zombie" flag set up
procedure ThreadReleaseMM(mm: PSAPThreadMM);

// Allocate memory for thread
function ThreadGetMem(mm: PSAPThreadMM; size: Integer): Pointer; inline;

// Free memory block p via given thread MM
// ! Check, that memory was allocated exactly by this MM, is NOT performed !
function ThreadFreeMem(mm: PSAPThreadMM; p: Pointer): Integer; inline;

// Reallocates memory block. Cases when size <= 0 and p = nil must be performed at the higher level before calling this method
function ThreadReallocMem(mm: PSAPThreadMM; var p: Pointer; size: LongWord): Integer; inline;

// Returns allocated block size without service info
function GetAllocatedSize(p: Pointer): LongWord; inline;

// Returns thread MM
function GetThreadMM: Pointer;

// Sync when freeing memory allocated from another thread MM
procedure LockForFreeFromOtherThread(mm: PSAPThreadMM); inline;
procedure UnlockForFreeFromOtherThread(mm: PSAPThreadMM); inline;

// If memory block was allocated by one thread MM and freed in another thread MM, 
// it is put into special list instead of freeing "in place".
// All blocks from this list are freed by the following procedure
// It is called on each call of Get, Free, Realloc, and also when MM is destroyed
procedure FreeAllBlocksFreedInOtherThread(mm: PSAPThreadMM);

// Called before memory leak check to free all not freed blocks, allocated from threads, 
// which are already terminated (their MMs were put into "zombie" MMs list)
procedure CheckAndReleaseZombiMM;

// Locks MMs list for enumeration or changing
procedure LockMMsList;
procedure UnlockMMsList;

// TLS addr for getting thread MM
var
  TLSIndex: LongWord;
  TLSOffset: LongWord;

var // variables are declared in interface for statistics
    // they may only be used between LockMMsList/UnlockMMsList calls
  work_mm: PSAPThreadMM  = nil;  // List of active MMs
  zombi_mm: PSAPThreadMM = nil;  // List of MMs, left from terminated threads 
                                 // which still have used memory (this may be either a memory leak 
                                 // or memory, which is now used by another thread)

var
  memory_leaks_add: TSAPTags; // When memory leak check is performed, 
                              // sap_used_in_leaks_reporting value is included into TSapTags
                              // Before this moment this set is empty

implementation

uses
  Windows,
  SAPOSBlocks,
  SAPNormalBlocks,
  SAPList,
  SAPErrCodes,
  SAPRelease,
  SAPReportMemoryLeaks
//, SAPDebugUtils
;

var
  HeapHandle: THandle = 0; // for allocation memory for ThreadMM

//----------------

var
  // CS for adding or deleting thread MMs
  gThreadMMsListCS: _RTL_CRITICAL_SECTION;

procedure LockMMsList;
begin
  Windows.EnterCriticalSection(gThreadMMsListCS);
end;

procedure UnlockMMsList;
begin
  Windows.LeaveCriticalSection(gThreadMMsListCS);
end;

//------------------------------------------------------------

function GetThreadMM: Pointer;
asm
  mov eax,TLSOffset
  mov ecx,fs:[$00000018]
  mov eax,[ecx+eax]
  mov Result, eax
end;
{ slower old implementation
function GetThreadMM: Pointer;
begin
  Result := TlsGetValue(TLSIndex);
end; }

procedure TieToMMList(var list: PSAPThreadMM; x: PSAPThreadMM);
begin
  if list = nil then
  begin
    x.next := nil;
    x.prev := nil;
    list := x;
  end
  else begin
    // Вставить вторым
    x.next := list.next;
    x.prev := list;
    if x.next <> nil then x.next.prev := x;

    list.next := x;
  end;
end;

procedure UntieFromMMList(var list: PSAPThreadMM; x: PSAPThreadMM);
begin
  if x.next <> nil then x.next.prev := x.prev;
  if x.prev <> nil then x.prev.next := x.next;

  if list = x then list := x.next;
end;

function CreateThreadMM: PSAPThreadMM;
var
  x: PSAPThreadMM;
begin
  x := Windows.HeapAlloc(HeapHandle, Windows.HEAP_ZERO_MEMORY, SizeOf(TSAPThreadMM));
  Windows.TlsSetValue(TLSIndex, x);

  Windows.InitializeCriticalSection(x.crit_sect_for_other_threads);

  x.magic := cMagicMM;
  x.thread_id := GetCurrentThreadId; // for statistics and debug
  InitSmallBlocksTable(x);

  LockMMsList;
    TieToMMList(work_mm, x);
  UnlockMMsList;

  Result := x;
end;

procedure ThreadReleaseMM(mm: PSAPThreadMM);
var
  done: Boolean;
begin
  if mm.blocks_to_free <> nil then 
    FreeAllBlocksFreedInOtherThread(mm);

  done := TryToReleaseAllOSBlocks(mm);

  CheckAndReleaseZombiMM;

  LockMMsList;

    UntieFromMMList(work_mm, mm);

    if not done then
      TieToMMList(zombi_mm, mm);

  UnlockMMsList;

  if done then
    Windows.HeapFree(HeapHandle, 0, mm);
end;

//----------------------------------------------------------

procedure LockForFreeFromOtherThread(mm: PSAPThreadMM); {inline;}
begin
  Windows.EnterCriticalSection(mm.crit_sect_for_other_threads);
end;

procedure UnlockForFreeFromOtherThread(mm: PSAPThreadMM); {inline;}
begin
  Windows.LeaveCriticalSection(mm.crit_sect_for_other_threads);
end;

// Called when each thread MM is destroyed
// Checks the list of "zombie" ММs,
// if MM has not empty list "blocks_to_free", then frees it
// Code is optimized not to hold critical section for too long
procedure CheckAndReleaseZombiMM;
var
  done: Boolean;
  x: PSAPThreadMM;
  released: Boolean;
begin
  done := False;

  while not done do
  begin
    done := True;

    LockMMsList;
      x := zombi_mm;
      while x <> nil do
      begin
        if x.blocks_to_free <> nil then
        begin
          UntieFromMMList(zombi_mm, x);
          done := False;
          break;
        end;
        x := x.next;
      end;
    UnlockMMsList;

    if not done then
    begin
      FreeAllBlocksFreedInOtherThread(x);

      released := TryToReleaseAllOSBlocks(x);
      if released then
        Windows.HeapFree(HeapHandle, 0, x)
      else
      begin
        LockMMsList;
          TieToMMList(zombi_mm, x);
        UnlockMMsList;
      end;

    end;
  end;
end;

//----------------------------------------------------------

function ThreadGetMem(mm: PSAPThreadMM; size: Integer): Pointer;
var
  s: PSAPSmallAllocatedBlock;
  block_size: LongWord;
  p: Pointer;
begin
  // statistics
  Inc(mm.no_get);

  if mm.blocks_to_free <> nil then
    FreeAllBlocksFreedInOtherThread(mm);

//  1. Allocate the block from small blocks list
  if size <= cMaxSmallBlockSize - SizeOf(TSAPSmallAllocatedBlock) then
  begin
    s := AllocateSmallBlock(mm, size);

    {$IFDEF SAP_MEMORYLEAKS}
    s.tags := s.tags + memory_leaks_add;
    {$ENDIF}

    {$IFDEF SAP_STATIP}
    SaveIP(s.ips);
    {$ENDIF}

    Result := Pointer(LongWord(s) + SizeOf(TSAPSmallAllocatedBlock));

    {$IFDEF SAP_STAT}
    Inc(mm.no_get_small);
    {$ENDIF}

    Exit;
  end;

// 2. Allocate block from free blocks list

  // Add service part size and alignment
  block_size :=
    (  size                      // allocated size
    + SizeOf(TSAPAllocatedBlock) // header of allocated block
    + SizeOf(LongWord)           // size at the end of the block
    + (cNormalAlignment - 1)     // alignment
    ) and not (cNormalAlignment - 1);

  p := AllocateNormalBlock(mm, block_size);

  {$IFDEF SAP_STAT}
  Inc(mm.no_get_normal);
  {$ENDIF}

  if p = nil then
  begin
    // What should I do here?
    Result := nil;
    Exit;
  end;

  {$IFDEF SAP_MEMORYLEAKS}
  PSAPAllocatedBlock(p).tags := PSAPAllocatedBlock(p).tags + memory_leaks_add;
  {$ENDIF}

  {$IFDEF SAP_STATIP}
  SaveIP(PSAPAllocatedBlock(p).ips);
  {$ENDIF}

  Result := Pointer(LongWord(p) + SizeOf(TSAPAllocatedBlock));
end;

function ThreadFreeMem(mm: PSAPThreadMM; p: Pointer): Integer;
var
  small: PSAPSmallAllocatedBlock;
begin
  if mm.blocks_to_free <> nil then 
    FreeAllBlocksFreedInOtherThread(mm);

  if LongWord(p) and (cAlignment - 1) <> 0 then
  begin
    Result := cErrInvPtrAlignment; // pointer is not aligned to cAlignment
    Exit;
  end;

  small := PSAPSmallAllocatedBlock(LongWord(p) - SizeOf(TSAPSmallAllocatedBlock));
  if small.magic <> cMagic then
  begin
    Result := cErrWrongMagic; // wrong magic number
    Exit;
  end;

  if not (sap_allocated in small.tags) then
  begin
    Result := cErrNotAllocated;
    Exit;
  end;

  Inc(mm.no_free);

// 1. Free small block
  if sap_small_block in small.tags then
  begin
    FreeSmallBlock(mm, small);
    Result := 0;
    Exit;
  end;

// 2. Insert into free blocks list
  InsertIntoFreeList(mm, p);
  Result := 0;
end;

//----------------------------------------------
// Reallocate

// Get the size of allocated block without service info size
function GetAllocatedSize(p: Pointer): LongWord; inline;
var
  s: PSAPSmallAllocatedBlock;
  h: PSAPSmallBlocksHeader;
  b: PSAPAllocatedBlock;
begin
  s := Pointer(LongWord(p) - SizeOf(TSAPSmallAllocatedBlock));

  if sap_small_block in s.tags then
  begin
    h := Pointer(LongWord(s) - s.ofs);
    if h.header_magic <> cSmallBlockHeaderMagic then
    begin
      Result := 0;
      Exit;
    end;
    Result := h.block_size - SizeOf(TSAPSmallAllocatedBlock);
  end
  else begin
    b := Pointer(LongWord(p) - SizeOf(TSAPAllocatedBlock));
    if b.block_magic <> cNormalBlockMagic then
    begin
      Result := 0;
      Exit;
    end;
    Result := b.size - cBlockOverhead;
  end;
end;

function ThreadReallocMem(mm: PSAPThreadMM; var p: Pointer; size: LongWord): Integer; inline;
var
  new_p: Pointer;
  block_size: LongWord;
  move_size: LongWord;
begin
  {$IFDEF SAP_STAT}
  Inc(mm.no_realloc);
  {$ENDIF}

  if mm.blocks_to_free <> nil then 
    FreeAllBlocksFreedInOtherThread(mm);

  block_size := GetAllocatedSize(p);

  if (block_size >= size) and ((block_size - size <= 64) or (size > block_size shr 1)) then
  begin
    // Keep block "in place" if:
    //  1. difference is small
    //  2. if is shrunk into not less than 50% of current block size
    {$IFDEF SAP_STAT}
    Inc(mm.no_realloc_at_place);
    {$ENDIF}
    Result := 0;
    Exit;
  end;

  if block_size < size then
    size := size + size shr 2; // allocate 25% more than required to reduce relocations

  new_p := ThreadGetMem(mm, size);

  if new_p <> nil then 
  begin
    if size > block_size then
    begin
      {$IFDEF SAP_STAT}
      Inc(mm.no_realloc_extend);
      {$ENDIF}
      move_size := block_size;
    end
    else begin
      {$IFDEF SAP_STAT}
      Inc(mm.no_realloc_shrink);
      {$ENDIF}
      move_size := size
    end;

    {$IFDEF SAP_STAT}
    Inc(mm.no_realloc_copy_bytes, move_size);
    {$ENDIF}
    // System.Move(p^, new_p^, move_size);
    SAPMoveMemory(new_p, p, move_size);
  end;

  Result := ThreadFreeMem(mm, p);
  p := new_p;
end;

procedure FreeAllBlocksFreedInOtherThread(mm: PSAPThreadMM);
var
  x: PSAPBlocksFreedInOtherThreads;
  next: PSAPBlocksFreedInOtherThreads;
begin
  LockForFreeFromOtherThread(mm);
    x := mm.blocks_to_free;
    mm.blocks_to_free := nil;
  UnlockForFreeFromOtherThread(mm);

  while x <> nil do
  begin
    next := x.next;
    ThreadFreeMem(mm, x);
    {$IFDEF SAP_STAT}
    Inc(mm.no_free_from_other_thread);
    {$ENDIF}
    x := next;
  end;
end;

procedure CreateHeap;
begin
  if HeapHandle = 0 then
    HeapHandle := Windows.HeapCreate(0, 32 * 1024, 0); // initial memory: 32 KB
end;

initialization
  memory_leaks_add := [];
  CreateHeap;
  TLSIndex := Windows.TlsAlloc;
  TLSOffset := TLSIndex * 4 + $0e10;

  Windows.InitializeCriticalSection(gThreadMMsListCS);
end.
