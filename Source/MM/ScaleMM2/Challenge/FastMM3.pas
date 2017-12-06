{

Fast Memory Manager 3.04

Description:
 A fast replacement memory manager for Borland Delphi Win32 applications that
 scales well under multi-threaded situations, is not prone to memory
 fragmentation, and supports shared memory without the use of external .DLL
 files.

Advantages:
 - Fast
 - Low overhead
 - Highly aligned memory blocks (GetMem always returns blocks aligned on 16 byte
   boundaries which is great for use with SSE/SSE2)
 - Good scaling under multi-threaded apps
 - Intelligent realloc (Reserves extra space if a block is upsized so that
   future upsizes can be done in place.)
 - Resistant to address space fragmentation
 - No external DLL required when sharing memory between the application and
   external libraries (provided both use this memory manager)
 - Optionally reports small block memory leaks on program shutdown. (This check
   is only performed if Delphi is currently running on the machine, so end users
   won't be bothered by the error message.)

Usage:
 Place this unit as the very first unit under the "uses" section in your
 project's .dpr file. Requires Delphi 7 or later (Win32) compiler. May compile
 under Delphi 5/6 with NoMMX defined (not tested). When sharing memory between
 an application and a DLL (e.g. when passing a long string or dynamic array to a
 DLL function), both the main application and the DLL must be compiled using
 this memory manager. The are some conditional defines (specified below) which
 may be used to tweak the memory manager.

License:
 This work is copyright Professional Software Development / Pierre le Riche. It
 is released under the Mozilla Public Licence 1.1 (MPL 1.1). The licence terms
 are described on this page: http://www.mozilla.org/MPL/MPL-1.1.html. There is
 no obligation to pay for FastMM, but if you find it useful or you would like to
 support further development, a donation would be much appreciated. My banking
 details are:
   Country: South Africa
   Bank: ABSA Bank Ltd
   Branch: Somerset West
   Branch Code: 334-712
   Account Name: PSD (Distribution)
   Account No.: 4041827693
   Swift Code: ABSAZAJJ
 My PayPal account is:
   bof@psd.co.za

Contact Details:
 Below are my contact details if you would like to get in touch with me. If you
 use this memory manager I would like to hear from you: please e-mail me your
 comments - good and bad.
 Snailmail:
   PO Box 2514
   Somerset West
   7129
   South Africa
 E-mail:
   plr@psd.co.za
 Webpage:
   fastmm.sourceforge.net

Disclaimer:
 FastMM has been tested extensively with both single and muiltithreaded
 applications, but unfortunately I am not in a position to make any
 guarantees. Use it at your own risk.

Change log:
 Version 1.00 (28 June 2004):
  First version (called PSDMemoryManager). Based on RecyclerMM (free block
  stack approach) by Eric Grange.
 Version 2.00 (3 November 2004):
  Complete redesign and rewrite from scratch. Name changed to FastMM to reflect
  this fact. Uses a linked-list approach. Is faster, has less memory overhead,
  and will now catch most bad pointers on FreeMem calls.
 Version 2.01 (9 November 2004):
  Added CountThreadContentions option. Counts the number of times any thread
  had to wait for another to perform a memory manager operation. (For
  statistical purposes).
 Version 2.02 (14 November 2004):
  1) Added option to no longer free the last empty batch or chunk. Increases
   residual memory usage slightly, but possibly saves many VirtualAlloc and
   GetChunk calls (on by default).
  2) Added an option never to downsize blocks on ReallocMem. Significantly
   increases speed of reallocations, but can dramatically increase memory
   consumption.
  3) Added NoMMX option to disable the use of MMX (for very old CPUs).
 Version 2.03 (15 November 2004):
  Added an option to count all GetMem and FreeMem calls. (For statistical
  purposes.)
 Version 2.04 (10 January 2005):
  Optionally reports memory leaks on program shutdown. (Provided Delphi is
  running on the machine.)
 Version 2.05 (18 January 2005)
  Added a sleep(10) in spinlock loops to avoid livelocks in heavy multithreaded
  usage.
 Version 3.0 (1 March 2005):
  Another rewrite. Reduced the memory overhead by: (a) not having a separate
  memory area for the linked list of free blocks (uses space inside free blocks
  themselves) (b) batch managers are allocated as part of chunks (c) block size
  lookup table size reduced. This should make FastMM more CPU cache friendly.
  Still to do: Implement pointer check option as well as a custom move library
  (currently uses system.move for reallocs). Combine this with the FastMove
  library for best performance.
 Version 3.01 (1 April 2005):
  Fixed a bug affecting Windows 9X. Seems Win9X doesn't like VirtualAlloc calls
  where the pointer doesn't point to the start of the block.
  (WinNT/2K/XP/2003 was not affected)
 Version 3.02 (15 May 2005):
  Some speed optimizations.)
 Version 3.03 (31 May 2005)
  Disabled checking for memory leaks when shutting down a DLL that was sharing
  the main application's MM.
 Version 3.04 (17 June 2005)
  Minor speed optimizations.
 }

unit FastMM3;

interface

{$WARN SYMBOL_PLATFORM OFF}

{Remove the . in the next line to enable debug mode. Uses (mostly) Pascal code
 instead of asm (slower, for debugging the MM only).}
{.$define DebugMM}

{Remove the . in the next line to disable reporting of memory leaks. Memory
 leaks are only reported if the Delphi IDE is currently running on the computer
 (so as not to alarm end users).}
{.$define NoMemoryLeakReporting}


implementation

uses
  Windows;

const
  {The number of small block types}
  NumSmallBlockTypes = 64;
  {The size of the largest small block handled. Must be a multiple of 16.}
  MaximumSmallBlockSize = 32752;
  {The size of a chunk group}
  ChunkGroupSize = 16 * 65536;
  {The initial usage bitmap}
  InitialUsageBitmap = $ffff;
  {Blocks of this size or smaller will never be downsized. Blocks greater than
   this size will be downsized if reallocmem is called with a new size which
   is less than a quarter of the current block size.}
  DownsizeMinimumSize = 128;
  {When a pointer is reallocated and the new size is larger than the old size,
   the new requested size is multiplied with the factor (1 + 1 / 2^x + y)
   and rounded to facilitate faster subsequent reallocations.}
  UpsizePaddingFactor = 2;
  UpsizePaddingAdd = 64;
  {The granularity of large blocks. Large blocks are allocated directly by
   VirtualAlloc. Address space granularity under Windows is 64K, so allocating
   large blocks in smaller chunks just wastes address space.}
  LargeBlockGranularity = 64 * 1024;
  {Hexadecimal characters}
  LHexTable: ShortString = '0123456789ABCDEF';
  {Sleep times when a resource (batch or small block manager) is in use}
  InitialSleepTime = 0;
  {Used when the resource is still in use after the first sleep}
  AdditionalSleepTime = 10;

type

  {Pointers to the types declared below}
  PSmallBlockType = ^TSmallBlockType;
  PSmallBlockManager = ^TSmallBlockManager;
  PLargeBlockManager = ^TLargeBlockManager;

  {Small block type (Size = 16 bytes)}
  TSmallBlockType = packed record
    {The chunk that is current being used to serve blocks in sequential order}
    CurrentSequentialFeedChunk: PSmallBlockManager;
    {The first partially free chunk for the given small block type}
    FirstPartiallyFreeChunk: PSmallBlockManager;
    {The offset of the last block that was served sequentially}
    LastSequentiallyFedBlockOffset: Word;
    {The block size for this block type}
    BlockSize: word;
    {True = Block type is locked}
    BlockTypeLocked: boolean;
    {Padding}
    Reserved1: byte;
    {The initial CurrentSequentialFeedOffset. Chosen so that
     CurrentSequentialFeedOffset will be zero after the last available block
     is taken}
    InitialSequentialFeedOffset: Word;
  end;

  {Small block manager (Size = 32 bytes)}
  TSmallBlockManager = packed record
    {----------Common manager fields----------------}
    {The block type}
    BlockType: PSmallBlockType;
    {---------------Small block manager fields---------------}
    {Pointer to the first free block inside this chunk}
    FirstFreeBlockOffset: word;
    {The number of blocks allocated in this chunk}
    BlocksInUse: word;
    {The previous and next chunks that has free blocks of
     the same size.}
    PreviousPartiallyFreeChunk: PSmallBlockManager;
    NextPartiallyFreeChunk: PSmallBlockManager;
    {The index of this chunk}
    ChunkIndex: byte;
    {Padding}
    Reserved1: array [0..2] of byte;
    {---------Chunk group info - only valid for ChunkIndex = 0---------}
    {A chunk group consists of 32 chunks}
    ChunkGroupUsageBitmap: Cardinal;
    {Points to chunk 0 of the previous chunk group with free chunks}
    PreviousChunkGroupWithFreeChunks: PSmallBlockManager;
    {Points to chunk 0 of the next chunk group with free chunks}
    NextChunkGroupWithFreeChunks: PSmallBlockManager;
  end;

  {The chunk manager structure for large blocks (Size = 12 bytes)}
  TLargeBlockManager = packed record
    {----------Common manager fields----------------}
    {The block type}
    BlockType: PSmallBlockType;
    {----------Large Block Manager Fields--------------}
    {The size requested by the user program}
    LargeBlockRequestedSize: Cardinal;
    {Allocated block size}
    LargeBlockAvailableSize: Cardinal;
  end;

const
  {The minimum offset of the first small block inside a chunk}
  MinimumSmallBlockOffset = SizeOf(TSmallBlockManager);
  {The offset of a large block from the start of the large block manager}
  LargeBlockStartOffset = (SizeOf(TLargeBlockManager) + 15) and -16;

var
  {--------------Chunk Managers---------------}
  {The dummy small block manager - used in place of nil pointers to avoid
   pointer checks}
  DummyChunkManager: TSmallBlockManager;
  {The first chunk group with available chunks}
  FirstChunkGroupWithFreeChunks: PSmallBlockManager = @DummyChunkManager;
  {Are the chunk groups currently locked?}
  ChunkGroupsLocked: boolean;
  {---------------Block type info-----------------}
  {The block types}
  BlockTypes: packed array[0..NumSmallBlockTypes - 1] of TSmallBlockType = (
    {16 byte jumps}
    (BlockSize: 16), (BlockSize: 32), (BlockSize: 48), (BlockSize: 64),
    (BlockSize: 80), (BlockSize: 96), (BlockSize: 112), (BlockSize: 128),
    (BlockSize: 144), (BlockSize: 160), (BlockSize: 176), (BlockSize: 192),
    (BlockSize: 208), (BlockSize: 224), (BlockSize: 240), (BlockSize: 256),
    {32 byte jumps}
    (BlockSize: 288), (BlockSize: 320), (BlockSize: 352), (BlockSize: 384),
    (BlockSize: 416), (BlockSize: 448), (BlockSize: 480), (BlockSize: 512),
    (BlockSize: 544), (BlockSize: 576), (BlockSize: 608), (BlockSize: 640),
    (BlockSize: 672), (BlockSize: 704), (BlockSize: 736), (BlockSize: 768),
    {48 byte jumps}
    (BlockSize: 816), (BlockSize: 864), (BlockSize: 912), (BlockSize: 960),
    {arbitrary jumps}
    (BlockSize: 992), (BlockSize: 1056), (BlockSize: 1168), (BlockSize: 1280),
    (BlockSize: 1392), (BlockSize: 1520), (BlockSize: 1632), (BlockSize: 1760),
    (BlockSize: 1920), (BlockSize: 2112), (BlockSize: 2336), (BlockSize: 2608),
    (BlockSize: 2976), (BlockSize: 3440), (BlockSize: 3840), (BlockSize: 4352),
    (BlockSize: 5024), (BlockSize: 5456), (BlockSize: 5952), (BlockSize: 6544),
    (BlockSize: 7264), (BlockSize: 8176), (BlockSize: 9344), (BlockSize: 10912),
    (BlockSize: 13088), (BlockSize: 16368), (BlockSize: 21824),
    (BlockSize: MaximumSmallBlockSize));
  {Lookup table to convert an allocation size to a small block type.}
  AllocationSizeToSmallBlockTypeIndexTimes2: packed array[0..(MaximumSmallBlockSize - 1) shr 4] of byte;
  {--------------Other info--------------}
  {The memory manager that was replaced}
  OldMemoryManager: TMemoryManager;
  {The replacement memory manager}
  NewMemoryManager: TMemoryManager;
  {A string uniquely identifying the current process (for sharing memory managers)}
  UniqueProcessIDString: ShortString = '????????_PID_FastMM'#0;
  {The handle of the MM window}
  MMWindow: HWND;
  {Is the MM in place a shared memory manager?}
  OwnsMMWindow: Boolean;

{$ifdef DEBUGMM}
{Compare [AAddress], CompareVal:
 If Equal: [AAddress] := NewVal and result = CompareVal
 If Unequal: Result := [AAddress]}
function LockCmpxchg(CompareVal, NewVal: byte; AAddress: PByte): Byte;
asm
  {On entry:
    al = CompareVal,
    dl = NewVal,
    ecx = AAddress}
  lock cmpxchg [ecx], dl
end;
{$endif}

{$ifdef DEBUGMM}
{Gets the first set bit and resets it, returning the bit index}
function FindFirstSetBit(ACardinal: Cardinal): Cardinal;
asm
  {On entry:
    eax = Cardinal}
  bsf eax, eax
end;
{$endif}

{$ifdef DEBUGMM}
{Allocates a chunk and returns a pointer to it. Returns 0 if out of memory.
 (Pascal version)}
function GetChunk: PSmallBlockManager;
var
  LChunkIndex, LUsageBitmap: Cardinal;
  LPChunkGroup: PSmallBlockManager;
begin
  {Lock the batch managers}
  if IsMultiThread then
  begin
    while LockCmpxchg(0, 1, @ChunkGroupsLocked) <> 0 do
    begin
      Windows.Sleep(InitialSleepTime);
      if LockCmpxchg(0, 1, @ChunkGroupsLocked) = 0 then
        break;
      Windows.Sleep(AdditionalSleepTime);
    end;
  end;
  {Get the first chunk group with space}
  LPChunkGroup := FirstChunkGroupWithFreeChunks;
  {Is there one?}
  if LPChunkGroup <> @DummyChunkManager then
  begin
    {Get the old usage bitmap}
    LUsageBitmap := LPChunkGroup.ChunkGroupUsageBitmap;
    {Get the chunk index}
    LChunkIndex := FindFirstSetBit(LUsageBitmap);
    {Update the usage bitmap}
    LUsageBitmap := LUsageBitmap xor (1 shl LChunkIndex);
    {Set the new usage bitmap}
    LPChunkGroup.ChunkGroupUsageBitmap := LUsageBitmap;
    {Is this chunk group now full?}
    if LUsageBitmap = 0 then
    begin
      {Remove this chunk group from the linked list}
      FirstChunkGroupWithFreeChunks := LPChunkGroup.NextChunkGroupWithFreeChunks;
      FirstChunkGroupWithFreeChunks.PreviousChunkGroupWithFreeChunks := @DummyChunkManager;
    end;
    {Chunk groups are no longer locked}
    ChunkGroupsLocked := False;
    {Adjust the result for the correct chunk index}
    Result := PSmallBlockManager(Cardinal(LPChunkGroup) + (LChunkIndex shl 16));
    {Set the chunk index and pointer to the first chunk for the returned chunk}
    Result.ChunkIndex := byte(LChunkIndex);
  end
  else
  begin
    {No available chunks - allocate a batch}
    Result := VirtualAlloc(nil, ChunkGroupSize, MEM_COMMIT or MEM_TOP_DOWN,
      PAGE_READWRITE);
    {Did the allocation fail?}
    if Result = nil then
    begin
      {Chunk groups are no longer locked}
      ChunkGroupsLocked := False;
      exit;
    end;
    {Set up the chunk group manager}
    FirstChunkGroupWithFreeChunks := Result;
    Result.ChunkGroupUsageBitmap := (InitialUsageBitmap xor 1); //Only the first chunk is currently used
    {No other chunk groups with space}
    Result.PreviousChunkGroupWithFreeChunks := @DummyChunkManager;
    Result.NextChunkGroupWithFreeChunks := @DummyChunkManager;
    {Chunk groups are no longer locked}
    ChunkGroupsLocked := False;
  end;
  {Initialize the chunk}
  Result.FirstFreeBlockOffset := 0;
  Result.BlocksInUse := 1;
  Result.PreviousPartiallyFreeChunk := @DummyChunkManager;
  Result.NextPartiallyFreeChunk := @DummyChunkManager;
end;
{$else}
{Allocates a chunk and returns a pointer to it. Returns 0 if out of memory.
 (asm version)}
function GetChunk: PSmallBlockManager;
asm
  {Is this program multithreaded?}
  cmp IsMultiThread, False
  je @BatchesLocked
  {Lock the batch managers}
@LockLoop:
  xor eax, eax
  mov dl, 1
  {Attempt to lock the batches}
  lock cmpxchg ChunkGroupsLocked, dl
  jz @BatchesLocked
  {Couldn't lock the batches - sleep and try again}
  push False
  push InitialSleepTime
  call Windows.SleepEx
  {Try again}
  xor eax, eax
  mov dl, 1
  {Attempt to lock the batches}
  lock cmpxchg ChunkGroupsLocked, dl
  jz @BatchesLocked
  {Couldn't lock the batches - sleep and try again}
  push False
  push AdditionalSleepTime
  call Windows.SleepEx
  {Try again}
  jmp @LockLoop
  {**Align branch target here**}
@BatchesLocked:
  {Get the first chunk group with space}
  mov edx, FirstChunkGroupWithFreeChunks
  {Is there one?}
  cmp edx, offset DummyChunkManager
  je @AllocateBatch
  {Get the old usage bitmap}
  mov ecx, TSmallBlockManager([edx]).ChunkGroupUsageBitmap
  {Get the chunk index: Find the set bit and put the index in eax}
  bsf eax, ecx
  {Reset this bit}
  btr ecx, eax
  {Store the new usage bitmap}
  mov TSmallBlockManager([edx]).ChunkGroupUsageBitmap, ecx
  {Is the batch now fully used?}
  test ecx, ecx
  jnz @ChunkGroupNotFull
  {The batch is full - remove it from the partially free list, and make the
   next batch the first entry}
  mov ecx, TSmallBlockManager([edx]).NextChunkGroupWithFreeChunks
  mov FirstChunkGroupWithFreeChunks, ecx
  mov TSmallBlockManager([ecx]).PreviousChunkGroupWithFreeChunks, offset DummyChunkManager
@ChunkGroupNotFull:
  {Save the bit number}
  mov ecx, eax
  {Multiply the bit number by 64K}
  shl eax, 16
  {Add the start address of the batch}
  add eax, edx
  {Store the chunk number}
  mov TSmallBlockManager([eax]).ChunkIndex, cl
@GetChunkDone:
  {Unlock the batch managers}
  mov ChunkGroupsLocked, 0
  {Set common chunk properties}
  mov TSmallBlockManager([eax]).FirstFreeBlockOffset, 0
  mov TSmallBlockManager([eax]).BlocksInUse, 1
  mov TSmallBlockManager([eax]).PreviousPartiallyFreeChunk, offset DummyChunkManager
  mov TSmallBlockManager([eax]).NextPartiallyFreeChunk, offset DummyChunkManager
  {Done}
  ret
@AllocateBatch:
  {Call virtualalloc to allocate a batch}
  push PAGE_READWRITE
  push MEM_COMMIT or MEM_TOP_DOWN
  push 32 * 65536
  push 0
  call VirtualAlloc
  {Out of memory?}
  test eax, eax
  jnz @AllocateOK
  {Unlock the batch managers}
  mov ChunkGroupsLocked, 0
  ret
@AllocateOK:
  {Update settings}
  mov FirstChunkGroupWithFreeChunks, eax
  mov TSmallBlockManager([eax]).ChunkGroupUsageBitmap, $fffffffe
  mov TSmallBlockManager([eax]).PreviousChunkGroupWithFreeChunks, offset DummyChunkManager
  mov TSmallBlockManager([eax]).NextChunkGroupWithFreeChunks, offset DummyChunkManager
  jmp @GetChunkDone
end;
{$endif}

{$ifdef DEBUGMM}
{Frees a 64K chunk (pascal version)}
procedure FreeChunk(APSmallBlockManager: PSmallBlockManager);
var
  LChunkIndex: Cardinal;
  LPChunkGroup: PSmallBlockManager;
  LOldUsageBitmap, LNewUsageBitmap: Cardinal;
  LPPrevManager, LPNextManager: PSmallBlockManager;
begin
  {Get the chunk index}
  LChunkIndex := APSmallBlockManager.ChunkIndex;
  {Get the address of the first chunk}
  LPChunkGroup := Pointer(Cardinal(APSmallBlockManager) - (LChunkIndex shl 16));
  {Lock the chunk type}
  if IsMultiThread then
  begin
    while LockCmpxchg(0, 1, @ChunkGroupsLocked) <> 0 do
    begin
      Windows.Sleep(InitialSleepTime);
      if LockCmpxchg(0, 1, @ChunkGroupsLocked) = 0 then
        break;
      Windows.Sleep(AdditionalSleepTime);
    end;
  end;
  {Get the old usage bitmap}
  LOldUsageBitmap := LPChunkGroup.ChunkGroupUsageBitmap;
  LNewUsageBitmap := LOldUsageBitmap or (1 shl LChunkIndex);
  {Set the new usage bitmap}
  LPChunkGroup.ChunkGroupUsageBitmap := LNewUsageBitmap;
  {Are all chunks now unused?}
  if LNewUsageBitmap <> InitialUsageBitmap then
  begin
    {Was this chunk group fully used? If so - put it back in the chunk list as
     the first chunk group with space}
    if LOldUsageBitmap = 0 then
    begin
      LPChunkGroup.PreviousChunkGroupWithFreeChunks := @DummyChunkManager;
      LPChunkGroup.NextChunkGroupWithFreeChunks := FirstChunkGroupWithFreeChunks;
      FirstChunkGroupWithFreeChunks.PreviousChunkGroupWithFreeChunks := LPChunkGroup;
      FirstChunkGroupWithFreeChunks := LPChunkGroup;
    end;
    {Chunk type is no longer locked}
    ChunkGroupsLocked := False;
  end
  else
  begin
    {Remove this chunk group}
    LPPrevManager := LPChunkGroup.PreviousChunkGroupWithFreeChunks;
    LPNextManager := LPChunkGroup.NextChunkGroupWithFreeChunks;
    LPNextManager.PreviousChunkGroupWithFreeChunks := LPPrevManager;
    if LPPrevManager <> @DummyChunkManager then
      LPPrevManager.NextChunkGroupWithFreeChunks := LPNextManager
    else
      FirstChunkGroupWithFreeChunks := LPNextManager;
    {Chunk groups are no longer locked}
    ChunkGroupsLocked := False;
    {Free the memory}
    VirtualFree(LPChunkGroup, 0, MEM_RELEASE);
  end;
end;
{$else}
{Frees a 64K chunk (pascal version)}
procedure FreeChunk(APSmallBlockManager: PSmallBlockManager);
asm
  {On entry: eax = APSmallBlockManager}
  {Save the manager in ecx}
  mov ecx, eax
  {Lock the batch managers}
  cmp IsMultiThread, False
  je @BatchesLocked
@LockLoop:
  xor eax, eax
  mov dl, 1
  {Attempt to lock the batches}
  lock cmpxchg ChunkGroupsLocked, dl
  jz @BatchesLocked
  {Couldn't lock the batches - sleep and try again}
  push ecx
  push False
  push InitialSleepTime
  call Windows.SleepEx
  pop ecx
  {Try again}
  xor eax, eax
  mov dl, 1
  {Attempt to lock the batches}
  lock cmpxchg ChunkGroupsLocked, dl
  jz @BatchesLocked
  {Couldn't lock the batches - sleep and try again}
  push ecx
  push False
  push AdditionalSleepTime
  call Windows.SleepEx
  pop ecx
  {Try again}
  jmp @LockLoop
  {**Align jump target**}
  nop
@BatchesLocked:
  {Get the bit number in ecx}
  mov edx, ecx
  movzx ecx, TSmallBlockManager([ecx]).ChunkIndex
  {Get the batch manager for this chunk in edx}
  mov eax, ecx
  shl eax, 16
  sub edx, eax
  {eax = the or value}
  mov eax, 1
  shl eax, cl
  {Get the old usage bitmap in ecx}
  mov ecx, TSmallBlockManager[edx].ChunkGroupUsageBitmap
  {Get the new usage bitmap in eax}
  or eax, ecx
  {Store the new usage bitmap}
  mov TSmallBlockManager[edx].ChunkGroupUsageBitmap, eax
  {Is the whole batch now free?}
  cmp eax, -1
  je @BatchEmpty
  {Was the batch previously completely used?}
  test ecx, ecx
  jnz @FreeChunkDone
  {Add the batch back into the linked list as the first batch manager}
  mov eax, FirstChunkGroupWithFreeChunks
  mov TSmallBlockManager([eax]).PreviousChunkGroupWithFreeChunks, edx
  mov TSmallBlockManager([edx]).NextChunkGroupWithFreeChunks, eax
  mov TSmallBlockManager([edx]).PreviousChunkGroupWithFreeChunks, offset DummyChunkManager
  mov FirstChunkGroupWithFreeChunks, edx
@FreeChunkDone:
  {Unlock the batch managers}
  mov ChunkGroupsLocked, False
  ret
@BatchEmpty:
  {Remove the batch from the linked list}
  mov eax, TSmallBlockManager([edx]).PreviousChunkGroupWithFreeChunks
  mov ecx, TSmallBlockManager([edx]).NextChunkGroupWithFreeChunks
  mov TSmallBlockManager([ecx]).PreviousChunkGroupWithFreeChunks, eax
  cmp eax, offset DummyChunkManager
  jne @NotFirstBatch
  {Is the first batch}
  mov FirstChunkGroupWithFreeChunks, ecx
  jmp @WasFirstBatch
@NotFirstBatch:
  mov TSmallBlockManager([eax]).NextChunkGroupWithFreeChunks, ecx
@WasFirstBatch:
  {Unlock the batch managers}
  mov ChunkGroupsLocked, False
  {Free the chunk group}
  push MEM_RELEASE
  push 0
  push edx
  call VirtualFree
end;
{$endif}

{$ifdef DEBUGMM}
{Replacement for SysGetMem (pascal version)}
function FastGetMem(ASize: Integer): Pointer;
var
  LPSmallBlockType: PSmallBlockType;
  LSequentialOffset, LNewFirstFreeBlock: Word;
  LAllocSize: Cardinal;
  LPSmallBlockManager, LPNewFirstManager: PSmallBlockManager;
begin
  if Cardinal(ASize) <= MaximumSmallBlockSize then
  begin
    LPSmallBlockType := Pointer(
      AllocationSizeToSmallBlockTypeIndexTimes2[(ASize - 1) shr 4] * 8
      + Cardinal(@BlockTypes));
    {Lock the block type}
    if IsMultiThread then
    begin
      while LockCmpxchg(0, 1, @LPSmallBlockType.BlockTypeLocked) <> 0 do
      begin
        Windows.Sleep(InitialSleepTime);
        if LockCmpxchg(0, 1, @LPSmallBlockType.BlockTypeLocked) = 0 then
          break;
        Windows.Sleep(AdditionalSleepTime);
      end;
    end;
    {Try to feed a small block sequentially}
    LSequentialOffset := LPSmallBlockType.LastSequentiallyFedBlockOffset;
    Inc(LSequentialOffset, LPSmallBlockType.BlockSize);
    {Can another block fit?}
    if LSequentialOffset <> 0 then
    begin
      {Increment the current sequential pointer}
      LPSmallBlockType.LastSequentiallyFedBlockOffset := LSequentialOffset;
      {Set the result}
      Result := Pointer(Cardinal(LPSmallBlockType.CurrentSequentialFeedChunk) or LSequentialOffset);
      {Increment the number of used blocks in the sequential serve chunk}
      Inc(LPSmallBlockType.CurrentSequentialFeedChunk.BlocksInUse);
    end
    else
    begin
      {Get the first partially free chunk}
      LPSmallBlockManager :=  LPSmallBlockType.FirstPartiallyFreeChunk;
      {Increment the number of used blocks}
      Inc(LPSmallBlockManager.BlocksInUse);
      {Is the chunk valid?}
      if LPSmallBlockManager <> @DummyChunkManager then
      begin
        {Get the first free pointer}
        Result := Pointer(Cardinal(LPSmallBlockManager) or LPSmallBlockManager.FirstFreeBlockOffset);
        {Get the next first free block}
        LNewFirstFreeBlock := PWord(Result)^;
        LPSmallBlockManager.FirstFreeBlockOffset := LNewFirstFreeBlock;
        {Is the chunk now full?}
        if LNewFirstFreeBlock = 0 then
        begin
          {Chunk is full - remove it from the partially free list}
          LPNewFirstManager := LPSmallBlockManager.NextPartiallyFreeChunk;
          LPSmallBlockType.FirstPartiallyFreeChunk := LPNewFirstManager;
          LPNewFirstManager.PreviousPartiallyFreeChunk := @DummyChunkManager;
        end;
      end
      else
      begin
        {Try to allocate a chunk}
        LPSmallBlockManager := GetChunk;
        if LPSmallBlockManager <> nil then
        begin
          {Set up this manager}
          LPSmallBlockManager.BlockType := LPSmallBlockType;
          LPSmallBlockManager.FirstFreeBlockOffset := 0;
          LPSmallBlockManager.BlocksInUse := 1;
          LPSmallBlockManager.PreviousPartiallyFreeChunk := @DummyChunkManager;
          LPSmallBlockManager.NextPartiallyFreeChunk := @DummyChunkManager;
          {Set it up for sequential block serving}
          LPSmallBlockType.CurrentSequentialFeedChunk := LPSmallBlockManager;
          Result := Pointer(Cardinal(LPSmallBlockManager) or LPSmallBlockType.InitialSequentialFeedOffset);
          LPSmallBlockType.LastSequentiallyFedBlockOffset := word(Result);
        end
        else
        begin
          {Out of memory}
          Result := nil;
        end;
      end;
    end;
    {Unlock the block type}
    LPSmallBlockType.BlockTypeLocked := False;
  end
  else
  begin
    {Larger block: Add the size of the large block manager and round up to the
     next 64K boundary}
    LAllocSize := (ASize + (LargeBlockStartOffset + LargeBlockGranularity - 1)) and -LargeBlockGranularity;
    {Allocate directly through VirtualAlloc}
    Result := VirtualAlloc(nil, LAllocSize, MEM_COMMIT,
      PAGE_READWRITE);
    if Result <> nil then
    begin
      {Set the large block properties}
      PLargeBlockManager(Result).LargeBlockRequestedSize := ASize;
      PLargeBlockManager(Result).LargeBlockAvailableSize := LAllocSize - LargeBlockStartOffset;
      {Set the result}
      Inc(PByte(Result), LargeBlockStartOffset);
    end;
  end;
end;
{$else}
{Replacement for SysGetMem (asm version)}
function FastGetMem(ASize: Integer): Pointer;
asm
  {On entry:
    eax = ASize}
  {Get the index into AllocationSizeToSmallBlockTypeIndexTimes2}
  lea edx, [eax - 1]
  shr edx, 4
  {Is it a large block?}
  cmp eax, MaximumSmallBlockSize
  {Get the IsMultiThread variable in cl}
  mov cl, IsMultiThread
  {Is it a large block?}
  ja @LargeBlock
  {Multi-threaded?}
  test cl, cl
  {Get the pointer to the block type in edx}
  movzx edx, byte ptr AllocationSizeToSmallBlockTypeIndexTimes2[edx]
  lea edx, BlockTypes[edx * 8]
  {Lock the block type}
  jnz @LockBlockTypeLoop
@BlockTypeLocked:
  {Try to feed a small block sequentially}
  mov cx, TSmallBlockType([edx]).LastSequentiallyFedBlockOffset
  add cx, TSmallBlockType([edx]).BlockSize
  jz @NoSequentialFeed
  {Get the pointer to the current sequential feed chunk}
  mov eax, TSmallBlockType([edx]).CurrentSequentialFeedChunk
  {Update the last sequential feed offset}
  mov TSmallBlockType([edx]).LastSequentiallyFedBlockOffset, cx
  {Increment the number of blocks in use}
  inc TSmallBlockManager([eax]).BlocksInUse
  {Set the result}
  mov ax, cx
@SmallBlockDone:
  mov TSmallBlockType([edx]).BlockTypeLocked, False
  db $f3
  ret
  {**align jump target here**}
  db $66
  nop
@NoSequentialFeed:
  {save ebx}
  push ebx
  {Get the block type in ebx}
  mov ebx, edx
  {Get the first partially free chunk in edx}
  mov edx, TSmallBlockType([edx]).FirstPartiallyFreeChunk
  mov eax, edx
  {Increment the number of blocks in use}
  inc TSmallBlockManager([edx]).BlocksInUse
  {Get the first free pointer}
  mov ax, TSmallBlockManager([edx]).FirstFreeBlockOffset
  {Is this a valid chunk?}
  cmp edx, offset DummyChunkManager
  je @AllocateChunk
  {Get the next first free block}
  mov cx, [eax]
  {Store the new first free block}
  mov TSmallBlockManager([edx]).FirstFreeBlockOffset, cx
  {Is the new first free block nil?}
  test cx, cx
  jnz @AllocateDone
  mov ecx, TSmallBlockManager([edx]).NextPartiallyFreeChunk
  mov TSmallBlockType([ebx]).FirstPartiallyFreeChunk, ecx
  mov TSmallBlockManager([ecx]).PreviousPartiallyFreeChunk, offset DummyChunkManager
@AllocateDone:
  mov TSmallBlockType([ebx]).BlockTypeLocked, False
  pop ebx
  db $f3
  ret
  {**align jump target here**}
@AllocateChunk:
  {Get a chunk}
  call GetChunk
  {Out of memory?}
  test eax, eax
  jz @AllocateDone
  {Set up this manager}
  mov TSmallBlockManager([eax]).BlockType, ebx
  mov TSmallBlockManager([eax]).FirstFreeBlockOffset, 0
  mov TSmallBlockManager([eax]).BlocksInUse, 1
  mov TSmallBlockManager([eax]).PreviousPartiallyFreeChunk, offset DummyChunkManager
  mov TSmallBlockManager([eax]).NextPartiallyFreeChunk, offset DummyChunkManager
  {Set it up for sequential block serving}
  mov TSmallBlockType([ebx]).CurrentSequentialFeedChunk, eax
  mov ax, TSmallBlockType([ebx]).InitialSequentialFeedOffset
  mov TSmallBlockType([ebx]).LastSequentiallyFedBlockOffset, ax
  jmp @AllocateDone
  {**align jump target here**}
  db $66, $66
  nop
@LockBlockTypeLoop:
  mov eax, $100
  {Attempt to grab the block type}
  lock cmpxchg TSmallBlockType([edx]).BlockTypeLocked, ah
  je @BlockTypeLocked
  {Couldn't grab the block type - sleep and try again}
  push edx
  push InitialSleepTime
  call Windows.Sleep
  pop edx
  {Try again}
  mov eax, $100
  {Attempt to grab the block type}
  lock cmpxchg TSmallBlockType([edx]).BlockTypeLocked, ah
  je @BlockTypeLocked
  {Couldn't grab the block type - sleep and try again}
  push edx
  push AdditionalSleepTime
  call Windows.Sleep
  pop edx
  {Try again}
  jmp @LockBlockTypeLoop
  {**align jump target here**}
@LargeBlock:
  {Save esi and ebx}
  push ebx
  push esi
  {Large block: Round up to the next boundary, adding the size of the
   chunk manager}
  lea ebx, [eax + LargeBlockStartOffset + LargeBlockGranularity - 1]
  and ebx, -LargeBlockGranularity
  {Save the requested size in esi}
  mov esi, eax
  {Call VirtualAlloc directly}
  push PAGE_READWRITE
  push MEM_COMMIT
  push ebx
  push 0
  call VirtualAlloc
  {Out of memory?}
  test eax, eax
  jz @LargeAllocDone
  {Flag the block type as a large block}
  mov TLargeBlockManager([eax]).BlockType, 0
  mov TLargeBlockManager([eax]).LargeBlockRequestedSize, esi
  sub ebx, LargeBlockStartOffset
  mov TLargeBlockManager([eax]).LargeBlockAvailableSize, ebx
  add eax, LargeBlockStartOffset
@LargeAllocDone:
  pop esi
  pop ebx
end;
{$endif}

{$ifdef DEBUGMM}
{Replacement for SysFreeMem (pascal version)}
function FastFreeMem(APointer: Pointer): Integer;
var
  LPSmallBlockManager, LPOldFirstManager, LPPrevManager,
    LPNextManager: PSmallBlockManager;
  LPSmallBlockType: PSmallBlockType;
  LOldFirstFreeBlockOffset: Word;
begin
  {Get a pointer to the block manager}
  LPSmallBlockManager := PSmallBlockManager(Cardinal(APointer) and $ffff0000);
  {Get the block type index}
  LPSmallBlockType := LPSmallBlockManager.BlockType;
  {Is it a large or small block chunk?}
  if LPSmallBlockType <> nil then
  begin
    {Small Block: Lock the block type}
    if IsMultiThread then
    begin
      while (LockCmpxchg(0, 1, @LPSmallBlockType.BlockTypeLocked) <> 0) do
      begin
        Windows.Sleep(InitialSleepTime);
        if LockCmpxchg(0, 1, @LPSmallBlockType.BlockTypeLocked) = 0 then
          break;
        Windows.Sleep(AdditionalSleepTime);
      end;
    end;
    {Get the old first free block}
    LOldFirstFreeBlockOffset := LPSmallBlockManager.FirstFreeBlockOffset;
    {Store the old first free pointer}
    PWord(APointer)^ := LOldFirstFreeBlockOffset;
    {Store this as the new first free block}
    LPSmallBlockManager.FirstFreeBlockOffset := word(APointer);
    {Was the chunk previously full?}
    if (LOldFirstFreeBlockOffset = 0) then
    begin
      {Insert this as the first partially free chunk for the block size}
      LPOldFirstManager := LPSmallBlockType.FirstPartiallyFreeChunk;
      LPSmallBlockManager.NextPartiallyFreeChunk := LPOldFirstManager;
      LPOldFirstManager.PreviousPartiallyFreeChunk := LPSmallBlockManager;
      LPSmallBlockManager.PreviousPartiallyFreeChunk := @DummyChunkManager;
      LPSmallBlockType.FirstPartiallyFreeChunk := LPSmallBlockManager;
    end;
    {Decrement the number of allocated blocks}
    Dec(LPSmallBlockManager.BlocksInUse);
    {Is the entire chunk now free? -> Free it.}
    if (LPSmallBlockManager.BlocksInUse = 0) then
    begin
      {Get the previous and next chunk managers}
      LPPrevManager := LPSmallBlockManager.PreviousPartiallyFreeChunk;
      LPNextManager := LPSmallBlockManager.NextPartiallyFreeChunk;
      {Remove this manager}
      if LPPrevManager <> @DummyChunkManager then
        LPPrevManager.NextPartiallyFreeChunk := LPNextManager
      else
        LPSmallBlockType.FirstPartiallyFreeChunk := LPNextManager;
      LPNextManager.PreviousPartiallyFreeChunk := LPPrevManager;
      {Is this the sequential feed chunk? If so, stop sequential feeding}
      if (LPSmallBlockType.CurrentSequentialFeedChunk = LPSmallBlockManager) then
        LPSmallBlockType.LastSequentiallyFedBlockOffset := word(-SmallInt(LPSmallBlockType.BlockSize));
      {Unlock this block type}
      LPSmallBlockType.BlockTypeLocked := False;
      {Release this chunk}
      FreeChunk(LPSmallBlockManager);
    end
    else
    begin
      {Unlock this block type}
      LPSmallBlockType.BlockTypeLocked := False;
    end;
    {No error}
    Result := 0;
  end
  else
  begin
    {Large block: Free it through VirtualFree}
    Dec(PByte(APointer), LargeBlockStartOffset);
    if VirtualFree(APointer, 0, MEM_RELEASE) then
      Result := 0
    else
      Result := -1;
  end;
end;
{$else}
{Replacement for SysFreeMem (asm version)}
function FastFreeMem(APointer: Pointer): Integer;
asm
  {One entry:
    eax = APointer}
  {Get the small block manager in ecx}
  mov ecx, eax
  mov cx, 0
  {Get the block type in edx}
  mov edx, TSmallBlockManager([ecx]).BlockType
  {Only large blocks have an offset of 16}
  cmp ax, LargeBlockStartOffset
  {Save ebx}
  push ebx
  {Save the pointer in ebx}
  mov ebx, eax
  {Get the IsMultiThread variable in al}
  mov al, IsMultiThread
  {Large block?}
  je @LargeBlock
  {Lock the block type}
  test al, al
  jne @LockBlockTypeLoop
@BlockTypeLocked:
  {State: ebx = APointer; ecx = SmallBlockManager, edx = BlockType}
  {Decrement the number of allocated blocks: Is the entire chunk now
   free? -> Free it.}
  sub TSmallBlockManager([ecx]).BlocksInUse, 1
  {Get the old first free block}
  mov ax, TSmallBlockManager([ecx]).FirstFreeBlockOffset
  {Chunk empty?}
  jz @ChunkIsEmpty
  {Was the chunk previously full?}
  test ax, ax
  {Store the old first free pointer}
  mov [ebx], ax
  {Store this as the new first free block}
  mov TSmallBlockManager([ecx]).FirstFreeBlockOffset, bx
  {Was the chunk previously full?}
  jz @BlockWasFull
@FreeDone:
  {Unlock this block type}
  mov TSmallBlockType([edx]).BlockTypeLocked, False
  pop ebx
  xor eax, eax
  db $f3
  ret
  {**Align Branch Target**}
  db $66
  nop
@BlockWasFull:
  {Insert this as the first partially free chunk for the block size}
  mov eax, TSmallBlockType([edx]).FirstPartiallyFreeChunk
  mov TSmallBlockType([edx]).FirstPartiallyFreeChunk, ecx
  mov TSmallBlockManager([ecx]).PreviousPartiallyFreeChunk, offset DummyChunkManager
  mov TSmallBlockManager([ecx]).NextPartiallyFreeChunk, eax
  mov TSmallBlockManager([eax]).PreviousPartiallyFreeChunk, ecx
  jmp @FreeDone
{**Align target**}
  db $66, $66
  nop
@ChunkIsEmpty:
  {No free blocks? -> Was not in the linked list. Has to be the sequential feed
   chunk}
  test ax, ax
  jz @IsSequentialServeChunk
  {Get the previous and next chunk managers}
  mov eax, TSmallBlockManager([ecx]).PreviousPartiallyFreeChunk
  mov ebx, TSmallBlockManager([ecx]).NextPartiallyFreeChunk
  {Remove the chunk}
  mov TSmallBlockManager([ebx]).PreviousPartiallyFreeChunk, eax
  cmp eax, offset DummyChunkManager
  je @NoPreviousManager
  mov TSmallBlockManager([eax]).NextPartiallyFreeChunk, ebx
  jmp @IsPreviousManager
{**Align target**}
  db $66
  nop
@NoPreviousManager:
  mov TSmallBlockType([edx]).FirstPartiallyFreeChunk, ebx
@IsPreviousManager:
  {Was this the sequential serve chunk?}
  cmp TSmallBlockType([edx]).CurrentSequentialFeedChunk, ecx
  jne @NotSequentialServeChunk
@IsSequentialServeChunk:
  {Disable sequential feeding}
  mov ax, TSmallBlockType([edx]).BlockSize
  neg ax
  mov TSmallBlockType([edx]).LastSequentiallyFedBlockOffset, ax
{**Align target**}
  db $66
  nop
@NotSequentialServeChunk:
  {Not the sequential serve chunk: free it}
  {Unlock this block type}
  mov TSmallBlockType([edx]).BlockTypeLocked, False
  {Release this chunk}
  mov eax, ecx
  call FreeChunk
  pop ebx
  xor eax, eax
  db $f3
  ret
{**Align target**}
@LockBlockTypeLoop:
  mov ax, $100
  {Attempt to grab the block type}
  lock cmpxchg TSmallBlockType([edx]).BlockTypeLocked, ah
  je @BlockTypeLocked
  {Couldn't grab the block type - sleep and try again}
  push ecx
  push edx
  push InitialSleepTime
  call Windows.Sleep
  pop edx
  pop ecx
  {Try again}
  mov ax, $100
  {Attempt to grab the block type}
  lock cmpxchg TSmallBlockType([edx]).BlockTypeLocked, ah
  je @BlockTypeLocked
  {Couldn't grab the block type - sleep and try again}
  push ecx
  push edx
  push AdditionalSleepTime
  call Windows.Sleep
  pop edx
  pop ecx
  {Try again}
  jmp @LockBlockTypeLoop
  {**align jump target here**}
  db $66
  nop
@LargeBlock:
  pop ebx
  {Larger block: Free the block through VirtualFree}
  push MEM_RELEASE
  push 0
  push ecx
  call VirtualFree
  {VirtualFree returns >0 if all is ok}
  cmp eax, 1
  {Return 0 on all ok}
  sbb eax, eax
end;
{$endif}

{$ifdef DEBUGMM}
{Replacement for SysReallocMem (Pascal version)}
function FastReallocMem(APointer: Pointer; ANewSize: Integer): Pointer;
var
  LOldBlockSize, LOldUserSize, LNewBlockSize, LMoveSize: Cardinal;
  LPSmallBlockManager: PSmallBlockManager;
  LPSmallBlockType: PSmallBlockType;
begin
  {Get a pointer to the block manager}
  LPSmallBlockManager := PSmallBlockManager(Cardinal(APointer) and $ffff0000);
  {Get the block type index}
  LPSmallBlockType := LPSmallBlockManager.BlockType;
  {Is it a large or small block chunk?}
  if LPSmallBlockType <> nil then
  begin
    {Get the current block size}
    LOldBlockSize := LPSmallBlockType.BlockSize;
    {We don't track the allocated size for small blocks - assume its the same
     as the block size.}
    LOldUserSize := LOldBlockSize;
  end
  else
  begin
    {It's a large block chunk: Get the current block size}
    LOldBlockSize := PLargeBlockManager(LPSmallBlockManager).LargeBlockAvailableSize;
    {Get the size allocated by the user}
    LOldUserSize := PLargeBlockManager(LPSmallBlockManager).LargeBlockRequestedSize;
  end;
  {Is it an upsize or a downsize?}
  if Cardinal(ANewSize) > LOldBlockSize then
  begin
    {This pointer is being reallocated to a larger block and therefore it is
     logical to assume that it may be enlarged again. Since reallocations are
     expensive, we pad the requested size to avoid unnecessary future move
     operations.}
    LNewBlockSize := Cardinal(ANewSize)
      + (Cardinal(ANewSize) shr UpsizePaddingFactor)
      + UpsizePaddingAdd;
    {Allocate the new block}
    Result := FastGetMem(LNewBlockSize);
    if Result = nil then
      exit;
    {If its a large block - store the actual user requested size}
    if LNewBlockSize > MaximumSmallBlockSize then
      PLargeBlockManager(Cardinal(Result) and $ffff0000).LargeBlockRequestedSize := ANewSize;
    {The number of bytes to move is the old user size}
    LMoveSize := LOldUserSize;
  end
  else
  begin
    {It's a downsize: do we need to reallocate?}
    if (LOldBlockSize > DownsizeMinimumSize)
      and (Cardinal(ANewSize) * 4 < LOldBlockSize) then
    begin
      {The block is less than half of the old size, and the current size is
       greater than the minimum block size allowing a downsize: reallocate}
      Result := FastGetMem(ANewSize);
      if Result = nil then
        exit;
      {The number of bytes to move is the new size}
      LMoveSize := ANewSize;
    end
    else
    begin
      {No need to reallocate}
      Result := APointer;
      {Large block? -> Update the requested size}
      if LOldBlockSize > MaximumSmallBlockSize then
        PLargeBlockManager(Cardinal(Result) and $ffff0000).LargeBlockRequestedSize := ANewSize;
      exit;
    end
  end;
  {Do the move operation}
  System.Move(APointer^, Result^, LMoveSize);
  {Free the old block}
  FastFreeMem(APointer);
end;
{$else}
{Replacement for SysReallocMem (asm version)}
function FastReallocMem(APointer: Pointer; ANewSize: Integer): Pointer;
asm
  {On entry:
    eax = APointer
    edx = ANewSize}
  {Only large blocks have an offset of 16}
  cmp ax, LargeBlockStartOffset
  {Get the small block manager in ecx}
  mov ecx, eax
  mov cx, 0
  {save registers}
  push ebx
  push esi
  push edi
  {Get the block type in edi}
  mov edi, TSmallBlockManager([ecx]).BlockType
  {Save the original pointer in ebx}
  mov ebx, eax
  {Save the new size in esi}
  mov esi, edx
  {Large block?}
  jz @LargeBlock
  {Get the current allocated size in ecx}
  movzx ecx, TSmallBlockType([edi]).BlockSize
  {We don't track the requested size for small blocks - assume its the same
   as the allocated size.}
  mov edi, ecx
@GotOldBlockDetails:
  {Current block size in ecx, user allocated size in edi}
  cmp esi, ecx
  jle @ReallocDownSize
  {This pointer is being reallocated to a larger block and therefore it is
   logical to assume that it may be enlarged again. Since reallocations are
   expensive, we pad the requested size to avoid unnecessary future move
   operations.}
  {Add padding to the requested size}
  shr edx, UpsizePaddingFactor
  lea eax, [esi + edx + UpsizePaddingAdd]
  {Save the new block size}
  push eax
  {Allocate the new block}
  call FastGetMem
  pop edx
  {Success?}
  test eax, eax
  jz @ReallocDone
  {The move count = the old requested size}
  mov ecx, edi
  {If its a large block - store the actual user requested size}
  cmp edx, MaximumSmallBlockSize
  jle @MoveData
  mov TLargeBlockManager([eax - 16]).LargeBlockRequestedSize, esi
@MoveData:
  {Result = eax, move size = ecx}
  {Save the result}
  mov esi, eax
  {Move the data}
  mov edx, eax
  mov eax, ebx
  call System.Move
  {Free the old pointer}
  mov eax, ebx
  call FastFreeMem
  {Restore the result}
  mov eax, esi
@ReallocDone:
  pop edi
  pop esi
  pop ebx
  ret
{Align branch target}
  nop
@ReallocDownSize:
  {It's a downsize: do we need to reallocate?}
  cmp ecx, DownsizeMinimumSize
  jle @DownsizeSetNewRequestedSize
  lea edx, [esi + esi]
  add edx, edx
  cmp edx, edi
  ja @DownsizeSetNewRequestedSize
  mov eax, esi
  call FastGetMem
  test eax, eax
  je @ReallocDone
  mov ecx, esi
  jmp @MoveData
  {Align branch target}
@LargeBlock:
  {Get the size allocated by the user}
  mov edi, TLargeBlockManager([ecx]).LargeBlockRequestedSize;
  {It's a large block chunk: Get the current block size}
  mov ecx, TLargeBlockManager([ecx]).LargeBlockAvailableSize;
  jmp @GotOldBlockDetails
  {Align branch target}
@DownsizeSetNewRequestedSize:
  {Is it a large block?}
  cmp ecx, MaximumSmallBlockSize
  jle @ReallocDone
  {If its a large block - store the actual user requested size}
  mov TLargeBlockManager([eax - 16]).LargeBlockRequestedSize, esi
  jmp @ReallocDone
end;
{$endif}

{Sets up and installs the memory manager}
procedure InstallMemoryManager;
var
  LPreviousBlockSize, LBlockTypeIndex, LAllocInd, i, LCurrentProcessID,
    LBlocksPerChunk: Cardinal;
  LBlockType: PSmallBlockType;
{$ifdef DebugMM}
  k: Cardinal;
{$endif}
begin
  {Build a string identifying the current process}
  LCurrentProcessID := GetCurrentProcessId;
  for i := 0 to 7 do
  begin
    UniqueProcessIDString[8 - i] :=
      LHexTable[1 + ((LCurrentProcessID shr (i * 4)) and $F)];
  end;
  {Is the replacement memory manager already installed for this process?}
  MMWindow := FindWindow('STATIC', PChar(@UniqueProcessIDString[1]));
  if MMWindow = 0 then
  begin
    {Not installed yet: Initialize the small block type info}
    LPreviousBlockSize := 0;
    for LBlockTypeIndex := 0 to NumSmallBlockTypes - 1 do
    begin
      LBlockType := @BlockTypes[LBlockTypeIndex];
      {How many blocks per chunk?}
      LBlocksPerChunk := (65536 - MinimumSmallBlockOffset) div LBlockType.BlockSize;
      {Set the initial value of CurrentSequentialFeedPointer}
      LBlockType.InitialSequentialFeedOffset := 65536 - LBlocksPerChunk * LBlockType.BlockSize;
      {Make sure no blocks can be served sequentially}
      LBlockType.LastSequentiallyFedBlockOffset := Word(- SmallInt(LBlockType.BlockSize));
      {No chunk manager for this type yet}
      LBlockType.FirstPartiallyFreeChunk := @DummyChunkManager;
      {Update the block size lookup table}
      for LAllocInd := (LPreviousBlockSize shr 4) to (LBlockType.BlockSize - 1) shr 4 do
        AllocationSizeToSmallBlockTypeIndexTimes2[LAllocInd] := LBlockTypeIndex * 2;
      {Update previous block size}
      LPreviousBlockSize := LBlockType.BlockSize;
    end;
{$ifdef DebugMM}
    {Check the block size lookup table for validity}
    for k := 1 to MaximumSmallBlockSize do
    begin
      LBlockType := Pointer(AllocationSizeToSmallBlockTypeIndexTimes2[(k - 1) shr 4] * 8 + Cardinal(@BlockTypes));
      if (LBlockType.BlockSize < k) then
        System.Error(reInvalidPtr);
      if (LBlockType <> @BlockTypes[0]) then
      begin
        Dec(LBlockType, 1);
        if (LBlockType.BlockSize >= k) then
          System.Error(reInvalidPtr);
      end;
    end;
{$endif}
    {No memory manager installed yet - create the invisible window}
    MMWindow := CreateWindow('STATIC',
      PChar(@UniqueProcessIDString[1]),
      WS_POPUP, 0, 0, 0, 0,
      0, 0, LCurrentProcessID, nil);
    {The window data is a pointer to this memory manager}
    SetWindowLong(MMWindow, GWL_USERDATA, Integer(@NewMemoryManager));
    {We will be using this memory manager}
    NewMemoryManager.GetMem := FastGetMem;
    NewMemoryManager.FreeMem := FastFreeMem;
    NewMemoryManager.ReallocMem := FastReallocMem;
    {Owns the MMWindow}
    OwnsMMWindow := True;
  end
  else
  begin
    {Get the address of the shared memory manager}
    NewMemoryManager := PMemoryManager(GetWindowLong(MMWindow, GWL_USERDATA))^;
    {The MMWindow is owned by the main program (not this DLL)}
    OwnsMMWindow := False;
  end;
  {Save the old memory manager}
  GetMemoryManager(OldMemoryManager);
  {Replace the memory manager with either this one or the shared one.}
  SetMemoryManager(NewMemoryManager);
end;

procedure UninstallMemoryManager;
begin
  {Is this the owner of the shared MM window?}
  if OwnsMMWindow then
    DestroyWindow(MMWindow);
  {Restore the old memory manager}
  SetMemoryManager(OldMemoryManager);
end;

{$ifndef NoMemoryLeakReporting}
function DelphiIsRunning: boolean;
begin
  Result := (FindWindow('TPropertyInspector', nil) <> 0)
    and (FindWindow('TAppBuilder', nil) <> 0);
end;

{Converts an integer to string at the buffer location, returning the new
 buffer position. Only does 5 digits.}
function CardinalToStrBuf(ACardinal: Cardinal; ABuffer: PChar): PChar;
asm
  push ebx
  push esi
  mov ecx, edx
  xor esi, esi
  {10000s}
  xor edx, edx
  mov ebx, 10000
  div ebx
  mov esi, eax
  add al, '0'
  mov [ecx], al
  cmp esi, 1
  sbb ecx, -1
  {1000s}
  mov eax, edx
  xor edx, edx
  mov ebx, 1000
  div ebx
  or esi, eax
  add al, '0'
  mov [ecx], al
  cmp esi, 1
  sbb ecx, -1
  {100s}
  mov eax, edx
  xor edx, edx
  mov ebx, 100
  div ebx
  or esi, eax
  add al, '0'
  mov [ecx], al
  cmp esi, 1
  sbb ecx, -1
  {10s}
  mov eax, edx
  xor edx, edx
  mov ebx, 10
  div ebx
  or esi, eax
  add al, '0'
  mov [ecx], al
  cmp esi, 1
  sbb ecx, -1
  {1s}
  add dl, '0'
  mov [ecx], dl
  {Return the next position}
  lea eax, [ecx + 1]
  pop esi
  pop ebx
end;

procedure CheckForMemoryLeaks;
const
  LeakMessageHeader = 'This application has leaked memory. The leaks in (count x block size) format are:'#13#10#13#10;
  LeakMessageFooter = '.'#13#10#13#10'You may use a tool like MemProof to help you track down the source of these leaks. '
    + 'Steps to use MemProof:'#13#10'  1) Remove FastMM from the project.'#13#10'  2) Enable TD32 debug info in compiler options.'#13#10
    + '  3) Build (not compile) the application.'#13#10'  4) Ensure that the MemProof search directories are configured correctly.'#13#10
    + '  5) Run the application inside MemProof.'#13#10
    + 'MemProof is freeware and can be downloaded from http://www.automatedqa.com/downloads/memproof.'#13#10#13#10
    + 'Note: This memory leak check is only performed if Delphi is currently running. To disable this check completely, define "NoMemoryLeakReporting".'#0;
var
  LHasLeaks, LSequentialFeedChunkChecked: boolean;
  LBlockTypeInd: byte;
  LNumLeaks: integer;
  LChunkManager: PSmallBlockManager;
  LMsgPtr: PChar;
  LLeakMessage: array[0..4095] of char;
begin
  {No leaks have been found so far}
  LHasLeaks := False;
  {Prepare the memory leak message}
  System.Move(LeakMessageHeader, LLeakMessage[0], Length(LeakMessageHeader));
  {Get the pointer to the output message}
  LMsgPtr := @LLeakMessage[Length(LeakMessageHeader)];
  {Check all the small block types for leaks}
  for LBlockTypeInd := 0 to NumSmallBlockTypes - 1 do
  begin
    LNumLeaks := 0;
    LSequentialFeedChunkChecked := False;
    {Get the chunk manager}
    LChunkManager := BlockTypes[LBlockTypeInd].FirstPartiallyFreeChunk;
    {Not the dummy chunk?}
    while LChunkManager <> @DummyChunkManager do
    begin
      {Has the sequential feed page been checked?}
      LSequentialFeedChunkChecked := LSequentialFeedChunkChecked
        or (LChunkManager = BlockTypes[LBlockTypeInd].CurrentSequentialFeedChunk);
      {Any leaks?}
      Inc(LNumLeaks, LChunkManager.BlocksInUse);
      {Next chunk}
      LChunkManager := LChunkManager.NextPartiallyFreeChunk;
    end;
    if (not LSequentialFeedChunkChecked)
      and (BlockTypes[LBlockTypeInd].LastSequentiallyFedBlockOffset <> Word(- SmallInt(BlockTypes[LBlockTypeInd].BlockSize))) then
    begin
      Inc(LNumLeaks, BlockTypes[LBlockTypeInd].CurrentSequentialFeedChunk.BlocksInUse);
    end;
    {Any leaks?}
    if LNumLeaks > 0 then
    begin
      if not LHasLeaks then
      begin
        LHasLeaks := True;
      end
      else
      begin
        LMsgPtr^ := ',';
        Inc(LMsgPtr);
        LMsgPtr^ := ' ';
        Inc(LMsgPtr);
      end;
      LMsgPtr^ := '(';
      Inc(LMsgPtr);
      LMsgPtr := CardinalToStrBuf(LNumLeaks, LMsgPtr);
      LMsgPtr^ := 'x';
      Inc(LMsgPtr);
      LMsgPtr := CardinalToStrBuf(BlockTypes[LBlockTypeInd].BlockSize, LMsgPtr);
      LMsgPtr^ := ')';
      Inc(LMsgPtr);
    end;
  end;
  {Display the leak message if required}
  if LHasLeaks then
  begin
    {Set the message footer}
    System.Move(LeakMessageFooter, LMsgPtr^, Length(LeakMessageFooter));
    {Show the message}
    MessageBox(0, LLeakMessage, 'FastMM: Memory Leak Detected', MB_OK	or MB_ICONERROR or MB_TASKMODAL);
  end;
end;
{$endif}

initialization
  {Has another MM been set, or has the Borland MM been used? If so, this file
   is not the first unit in the uses clause of the project's .dpr file.}
  if IsMemoryManagerSet or (GetHeapStatus.TotalAllocated <> 0) then
    System.Error(reInvalidPtr);
  {Install the memory manager}
  InstallMemoryManager;

finalization
{$ifndef NoMemoryLeakReporting}
  {Check for memory leaks}
  if OwnsMMWindow and DelphiIsRunning then
    CheckForMemoryLeaks;
{$endif}
  {Restore the old memory manager}
  UninstallMemoryManager;

end.
