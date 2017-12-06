{

Fast Memory Manager 2.07

Description:
 A fast replacement memory manager for Borland Delphi Win32 applications that
 scales well under multi-threaded situations, is not prone to memory
 fragmentation, and supports shared memory without the use of external .DLL
 files.

Advantages:
 - Fast
 - Low overhead (2 bytes per block)
 - Highly aligned memory blocks (GetMem always returns blocks aligned on 16 byte
   boundaries which is great for use with SSE/SSE2)
 - Good scaling under multi-threaded apps
 - Will catch most bad pointers on freemem calls (helps during debugging).
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
 Version 2.06 (20 April 2005)
  Fixed a memory leak that occurred when FastMM was used inside a .DLL file
  with the option AlwaysFreeChunks disabled.
 Version 2.07 (31 May 2005)
  Disabled checking for memory leaks when shutting down a DLL that was sharing
  the main application's MM.
}

unit FastMM;

interface

{Remove the . in the next line to disable MMX and use a (slower) alternative
 move routine. This will allow this memory manager to be used with older
 80486 and original Pentium CPUs.}
{.$define NoMMX}

{Remove the . in the next line to disable pointer checks for a slight
 performance increase (at the cost of not catching bad pointers on
 freemem calls).}
{.$define NoPointerChecks}

{Remove the . in the next line to enable debug mode. Uses (mostly) Pascal code
 instead of asm (slower, for debugging the MM only).}
{.$define DebugMM}

{Remove the . in the next line to always free empty chunks. This lowers the
 residual memory consumption, but may in certain cicumstances cause many more
 GetChunk/FreeChunk calls than would otherwise have been necessary (which could
 impact performance). If this options is left off, the last empty 64K chunk for
 each block size as well as the last empty 2M batch is never freed (except
 during process termination of course).}
{.$define AlwaysFreeChunks}

{Remove the . in the next line to never downsize blocks. When this is enabled
 a ReallocMem request will always be performed in place if the new size is
 smaller than the old size. This provides a nice speed boost when doing
 frequent reallocations, but at the cost of increased memory consumption.}
{.$define NeverDownsizeOnReallocMem}

{Remove the . in the next line to count thread contentions. No real purpose
 other than to provide an interesting statistic.}
{.$define CountThreadContentions}

{Remove the . in the next line to count getmem and freemem operations
 (ReallocMem will thus count as 2 operations). No real purpose other than to
 provide an interesting statistic.}
{.$define CountGetMemAndFreeMemOperations}

{Remove the . in the next line to disable reporting of memory leaks. Memory
 leaks are only reported if the Delphi IDE is currently running on the computer
 (so as not to alarm end users).}
{.$define NoMemoryLeakReporting}

{Suppress platform warnings}
{$WARN SYMBOL_PLATFORM OFF}

{$ifdef CountThreadContentions}
var
  {The number of small block operations (freemem/getmem) and the number of
   times that a thread had to wait for another thread when attempting a small
   block operation}
  SmallBlockOperations, SmallBlockContentions: Int64;
  {The number of batch operations (GetChunk/FreeChunk) and the number of times
   that a thread had to wait for another thread when attempting to allocate or
   free a chunk.}
  BatchOperations, BatchContentions: Int64;
{$endif}

{$ifdef CountGetMemAndFreeMemOperations}
var
  {The total number of GetMem and FreeMem operations. ReallocMem will count
   as either 0 (inplace), 1 (free) or 2 (resize) operations. This counter has
   no real purpose other than to give you an indication of how memory management
   intensive your application is.}
  TotalGetMemAndFreeMemOperations: Int64;
{$endif}

implementation

uses Windows;

const
  {The number of small block types}
  NumSmallBlockTypes = 64;
  {The number of chunks addressable by this process. VirtualAlloc has a 64K
   granularity, so chunks are always aligned on a 64K boundary}
  MaxNumChunks = 32768; // = 2GB / 64K
  {The size of an allocation chunk}
  ChunkSize = 64 * 1024;
  {The number chunks per batch}
  NumChunksPerBatch = 32;
  {The size of a batch}
  BatchSize = ChunkSize * NumChunksPerBatch;
  {The maximum number of batches.}
  MaxNumBatches = MaxNumChunks div NumChunksPerBatch;
  {The size of the largest small block handled. Must be a multiple of 16.}
  MaximumSmallBlockSize = 32752;
{$ifndef NeverDownsizeOnReallocMem}
  {Block sizes smaller than this will never be downsized. Blocks of this size
   or greater will be downsized if reallocmem is called with a new size which
   is less than a quarter of the current block size.}
  MinimumBlockSizeToAllowDownsizing = 48;
{$endif}
  {When a pointer is reallocated and the new size is larger than the old size,
   the new requested size is multiplied with the factor (1 + 1 / 2^x) to
   facilitate faster subsequent reallocations.}
  ReallocPaddingFactor = 2;
  {The block type index for large blocks}
  LargeBlockTypeIndex = 100;
  {The granularity of large blocks. Large blocks are allocated directly by
   VirtualAlloc. Address space granularity under Windows is 64K, so allocating
   large blocks in smaller chunks just wastes address space.}
  LargeBlockGranularity = 64 * 1024;
  {Hexadecimal characters}
  LHexTable: ShortString = '0123456789ABCDEF';
  {Special purpose allocation IDs}
  LastBlockMarker = 65535;
  AllocatedBlockMarker = ord('a') + ord('b') * 256;
  {Sleep times when a resource (batch or small block manager) is in use}
  InitialSleepTime = 0;
  {Used when the resource is still in use after the first sleep}
  AdditionalSleepTime = 10;

type

  {Chunk Manager - 16 bytes in size}
  PChunkManager = ^TChunkManager;
  TChunkManager = packed record
    {The block type}
    BlockType: byte;
    {Padding byte}
    Reserved1: byte;
    {The batch managing this chunk}
    BatchIndex: word;
    {The large/small block info structures}
    case integer of
      0: {Small Block Chunk}
      (
        {The previous and next chunks that has free blocks of
         the same size.}
        PreviousPartiallyFreeChunk: PChunkManager;
        NextPartiallyFreeChunk: PChunkManager;
        {The first free block number.}
        FirstFreeBlockNumber: word;
        {The number of blocks allocated in this chunk}
        BlocksInUse: word;
      );
      1: {Large Block Chunk}
      (
        {The size requested by the user program}
        LargeBlockRequestedSize: Cardinal;
        {Allocated block size}
        LargeBlockAllocatedSize: Cardinal;
      );
  end;

var
  {---------------Small block type info-----------------}
  {The first partially free chunk for the given block size}
  FirstPartiallyFreeChunk: array[0..NumSmallBlockTypes - 1] of PChunkManager;
  {This is 2^32 / BlockSize. Multiply by this value and take the high dword
   to get the block number from an offset into the chunk}
  BlockSizeInverse: array[0..NumSmallBlockTypes - 1] of Cardinal;
  {All the different small block sizes. Block sizes must be a multiple of 16.
   Block sizes below are hand-picked to ensure minimal memory wastage and a
   reasonable progression of block sizes.}
  BlockSize: array[0..NumSmallBlockTypes - 1] of word = (
    {16 byte jumps}
    16, 32, 48, 64, 80, 96, 112, 128, 144, 160, 176, 192, 208, 224, 240, 256,
    {32 byte jumps}
     288, 320, 352, 384, 416, 448, 480, 512, 544, 576, 608, 640, 672, 704, 736, 768,
    {48 byte jumps}
    816, 864, 912, 960, 1008,
    {arbitrary jumps}
    1072, 1168, 1280, 1392, 1520, 1632, 1760, 1920, 2096, 2336, 2608, 2976,
    3440, 3840, 4352, 5024, 5456, 5952, 6544, 7264, 8176, 9344, 10912, 13088,
    16368, 21824, MaximumSmallBlockSize);
  {The highest block number offset in the chunk. Corresponds to the address of
   the last block number.}
  HighestBlockNumberOffset: array[0..NumSmallBlockTypes - 1] of word;
  {The offset of the start of the blocks. Follows after the chunk manager and
   blocks list, aligned to a 16-byte boundary}
  FirstBlockOffset: array[0..NumSmallBlockTypes - 1] of word;
  {Indicates whether a block type is locked or not}
  BlockTypeLocked: array[0..NumSmallBlockTypes - 1] of boolean;
  {Lookup table to convert an allocation size to a block type index. Calculate
   (allocation size - 1) div 16 to get an index into the table.}
  AllocationSizeToBlockType: array[0..MaximumSmallBlockSize div 16 - 1] of byte;
  {-------------------Batch info----------------------}
  {Are the batch managers locked?}
  BatchManagersLocked: boolean;
  {Batch manager usage bitmaps (4 * 1025 = 4K)}
  BatchUsageBitmap: array[0..MaxNumBatches] of Cardinal;
  {The address of teh start of each batch (4 * 1025 = 4K)}
  BatchStartAddress: array[0..MaxNumBatches] of Pointer;
  {The previous batch manager in the linked list (2 * 1025 = 2K)}
  PreviousBatchIndex: array[0..MaxNumBatches] of word;
  {The next batch manager in the linked list. NextBatchIndex[0] points to the
   first batch that has unallocated chunks. (2 * 1025 = 2K)}
  NextBatchIndex: array[0..MaxNumBatches] of word;
  {-----------------Chunk info---------------------}
  {The dummy chunk manager - used in place of nil chunk pointers to avoid
   pointer checks}
  DummyChunkManager: TChunkManager;
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

{Moves data from source to dest in 16 byte chunks (rounded up). Does not
 support overlapping. Count must be > 0}
procedure Move16(ASource, ADestination: Pointer; ACount: Cardinal);
asm
  {On entry:
    eax = ASource
    edx = ADestination
    ecx = ACount}
  {Round up to the nearest 16 bytes}
  add ecx, 15
  and ecx, -16
  {Convert the counter to negative based}
  add eax, ecx
  add edx, ecx
  neg ecx
  {Is there an uneven number of 16 byte blocks to copy?}
  test ecx, 16
  jz @MoveLoop
{$ifndef NoMMX}
  {Uneven number of 16-byte blocks: Move the first 16 bytes}
  movq mm0, [eax + ecx]
  movq mm1, [eax + ecx + 8]
  movq [edx + ecx], mm0
  movq [edx + ecx + 8], mm1
  add ecx, 16
  jns @Done
  {Do blocks of 32 bytes}
@MoveLoop:
  movq mm0, [eax + ecx]
  movq mm1, [eax + ecx + 8]
  movq mm2, [eax + ecx + 16]
  movq mm3, [eax + ecx + 24]
  movq [edx + ecx], mm0
  movq [edx + ecx + 8], mm1
  movq [edx + ecx + 16], mm2
  movq [edx + ecx + 24], mm3
  add ecx, 32
  js @MoveLoop
@Done:
  {Exits mmx state}
  emms
{$else}
  {Uneven number of 16-byte blocks: Move the first 16 bytes}
  fild qword ptr [eax + ecx + 8]
  fild qword ptr [eax + ecx]
  fistp qword ptr [edx + ecx]
  fistp qword ptr [edx + ecx + 8]
  add ecx, 16
  jns @Done
  {Do blocks of 32 bytes}
@MoveLoop:
  fild qword ptr [eax + ecx + 24]
  fild qword ptr [eax + ecx + 16]
  fild qword ptr [eax + ecx + 8]
  fild qword ptr [eax + ecx]
  fistp qword ptr [edx + ecx]
  fistp qword ptr [edx + ecx + 8]
  fistp qword ptr [edx + ecx + 16]
  fistp qword ptr [edx + ecx + 24]
  add ecx, 32
  js @MoveLoop
@Done:
{$endif}
end;

{$ifdef DebugMM}
{Locks the batch managers so only this thread can access them}
procedure LockBatchManagers;
asm
{$ifdef CountThreadContentions}
  add dword ptr BatchOperations, 1
  adc dword ptr BatchOperations[4], 0
{$endif}
@LockLoop:
  xor al, al
  mov dl, 1
  {Attempt to lock the batches}
  lock cmpxchg BatchManagersLocked, dl
  jz @Done
  {Couldn't lock the batches - sleep and try again}
{$ifdef CountThreadContentions}
  add dword ptr BatchContentions, 1
  adc dword ptr BatchContentions[4], 0
{$endif}
  push False
  push InitialSleepTime
  call Windows.SleepEx
  {Try again}
  xor al, al
  mov dl, 1
  {Attempt to lock the batches}
  lock cmpxchg BatchManagersLocked, dl
  jz @Done
  {Couldn't lock the batches - sleep and try again}
{$ifdef CountThreadContentions}
  add dword ptr BatchContentions, 1
  adc dword ptr BatchContentions[4], 0
{$endif}
  push False
  push AdditionalSleepTime
  call Windows.SleepEx
  jmp @LockLoop
@Done:
end;
{$endif}

{$ifdef DebugMM}
{Allocates a 64K chunk and returns its index. Returns 0 if out of memory.
 (Pascal version)}
function GetChunk: PChunkManager;
var
  i, LBatchIndex: Cardinal;
  LPChunk: PChunkManager;
begin
  {Lock the batch managers}
  LockBatchManagers;
  {Is there a batch manager with space?}
  if NextBatchIndex[0] > 0 then
  begin
    {Find the unused block number}
    asm
      {Get a pointer to the usage bitmap}
      movzx eax, word ptr [NextBatchIndex]
      {Find a set bit and put the index in ecx}
      bsf ecx, dword ptr [BatchUsageBitmap + eax * 4]
      {Reset this bit}
      btr dword ptr [BatchUsageBitmap + eax * 4], ecx
      {Multiply the bit number by 64K}
      shl ecx, 16
      {Add the start address of the batch}
      add ecx, dword ptr [BatchStartAddress + eax * 4]
      {Store the result}
      mov Result, ecx
    end;
    {Is this batch manager now full?}
    if BatchUsageBitmap[NextBatchIndex[0]] = 0 then
    begin
      {Remove this batch manager}
      NextBatchIndex[0] := NextBatchIndex[NextBatchIndex[0]];
      PreviousBatchIndex[NextBatchIndex[0]] := 0;
    end;
  end
  else
  begin
    {No available chunks - allocate a batch}
    Result := VirtualAlloc(nil, BatchSize, MEM_COMMIT, PAGE_READWRITE);
    if Result <> nil then
    begin
      {The batch manager index is the address divided by 2M + 1}
      LBatchIndex := Cardinal(Result) shr 21 + 1;
      {Update the chunk index to batch index translation table}
      LPChunk := Result;
      for i := 0 to NumChunksPerBatch - 1 do
      begin
        LPChunk.BatchIndex := LBatchIndex;
        LPChunk := PChunkManager(Cardinal(LPChunk) + 65536);
      end;
      {Set up the batch manager (it is the new first batch manager)}
      BatchStartAddress[LBatchIndex] := Result;
      BatchUsageBitmap[LBatchIndex] := $fffffffe; //Only the first chunk is currently used
      PreviousBatchIndex[LBatchIndex] := 0;
      NextBatchIndex[LBatchIndex] := 0;
      NextBatchIndex[0] := LBatchIndex;
    end;
  end;
  {Batch managers are no longer locked}
  BatchManagersLocked := False;
end;
{$else}
{Allocates a 64K chunk and returns its index. Returns 0 if out of memory.
 (asm version)}
function GetChunk: PChunkManager;
asm
  {Lock the batch managers}
{$ifdef CountThreadContentions}
  add dword ptr BatchOperations, 1
  adc dword ptr BatchOperations[4], 0
{$endif}
@LockLoop:
  xor al, al
  mov dl, 1
  {Attempt to lock the batches}
  lock cmpxchg BatchManagersLocked, dl
  jz @BatchesLocked
  {Couldn't lock the batches - sleep and try again}
{$ifdef CountThreadContentions}
  add dword ptr BatchContentions, 1
  adc dword ptr BatchContentions[4], 0
{$endif}
  push False
  push InitialSleepTime
  call Windows.SleepEx
  {Try again}
  xor al, al
  mov dl, 1
  {Attempt to lock the batches}
  lock cmpxchg BatchManagersLocked, dl
  jz @BatchesLocked
  {Couldn't lock the batches - sleep and try again}
{$ifdef CountThreadContentions}
  add dword ptr BatchContentions, 1
  adc dword ptr BatchContentions[4], 0
{$endif}
  push False
  push AdditionalSleepTime
  call Windows.SleepEx
  {Try again}
  jmp @LockLoop
@BatchesLocked:
  {Get the index of the first batch with space in edx}
  movzx edx, word ptr [NextBatchIndex]
  {Is there a batch with space?}
  test edx, edx
  jz @AllocateBatch
  {Get the current usage bitmap}
  mov ecx, dword ptr [BatchUsageBitmap + edx * 4]
  {Find the set bit and put the index in eax}
  bsf eax, ecx
  {Reset this bit}
  btr ecx, eax
  {Store the new usage bitmap}
  mov dword ptr [BatchUsageBitmap + edx * 4], ecx
  {Multiply the bit number by 64K}
  shl eax, 16
  {Add the start address of the batch}
  add eax, dword ptr [BatchStartAddress + edx * 4]
  {Is the batch now fully used?}
  test ecx, ecx
  jnz @GetChunkDone
  {The batch is full - remove it from the partially free list, and make the
   next batch the first entry}
  movzx ecx, word ptr [NextBatchIndex + edx + edx];
  mov word ptr NextBatchIndex, cx
  mov word ptr [PreviousBatchIndex + ecx + ecx], 0
@GetChunkDone:
  {Unlock the batch managers}
  mov BatchManagersLocked, False
  {Done}
  ret
@AllocateBatch:
  {Call virtualalloc to allocate a batch}
  push PAGE_READWRITE
  push MEM_COMMIT
  push BatchSize
  push 0
  call VirtualAlloc
  {Out of memory?}
  test eax, eax
  jz @GetChunkDone
  {Get the batch index in edx: The batch manager index is the address
   divided by 2M + 1}
  mov edx, eax
  shr edx, 21
  inc edx
  {Set up the batch manager: start address}
  mov dword ptr [BatchStartAddress + edx * 4], eax
  {Only one chunk used so far}
  mov dword ptr [BatchUsageBitmap + edx * 4], $fffffffe
  {No previous batch}
  mov word ptr [PreviousBatchIndex + edx + edx], 0
  {No next batch}
  mov word ptr [NextBatchIndex + edx + edx], 0
  {Set up the batch manager (it is the new first batch manager)}
  mov word ptr [NextBatchIndex], dx
  {Set the batch index in all the chunks}
  mov TChunkManager([eax]).BatchIndex, dx
  mov TChunkManager([eax + $010000]).BatchIndex, dx
  mov TChunkManager([eax + $020000]).BatchIndex, dx
  mov TChunkManager([eax + $030000]).BatchIndex, dx
  mov TChunkManager([eax + $040000]).BatchIndex, dx
  mov TChunkManager([eax + $050000]).BatchIndex, dx
  mov TChunkManager([eax + $060000]).BatchIndex, dx
  mov TChunkManager([eax + $070000]).BatchIndex, dx
  mov TChunkManager([eax + $080000]).BatchIndex, dx
  mov TChunkManager([eax + $090000]).BatchIndex, dx
  mov TChunkManager([eax + $0A0000]).BatchIndex, dx
  mov TChunkManager([eax + $0B0000]).BatchIndex, dx
  mov TChunkManager([eax + $0C0000]).BatchIndex, dx
  mov TChunkManager([eax + $0D0000]).BatchIndex, dx
  mov TChunkManager([eax + $0E0000]).BatchIndex, dx
  mov TChunkManager([eax + $0F0000]).BatchIndex, dx
  mov TChunkManager([eax + $100000]).BatchIndex, dx
  mov TChunkManager([eax + $110000]).BatchIndex, dx
  mov TChunkManager([eax + $120000]).BatchIndex, dx
  mov TChunkManager([eax + $130000]).BatchIndex, dx
  mov TChunkManager([eax + $140000]).BatchIndex, dx
  mov TChunkManager([eax + $150000]).BatchIndex, dx
  mov TChunkManager([eax + $160000]).BatchIndex, dx
  mov TChunkManager([eax + $170000]).BatchIndex, dx
  mov TChunkManager([eax + $180000]).BatchIndex, dx
  mov TChunkManager([eax + $190000]).BatchIndex, dx
  mov TChunkManager([eax + $1A0000]).BatchIndex, dx
  mov TChunkManager([eax + $1B0000]).BatchIndex, dx
  mov TChunkManager([eax + $1C0000]).BatchIndex, dx
  mov TChunkManager([eax + $1D0000]).BatchIndex, dx
  mov TChunkManager([eax + $1E0000]).BatchIndex, dx
  mov TChunkManager([eax + $1F0000]).BatchIndex, dx
  {Unlock the batch managers}
  mov BatchManagersLocked, False
end;
{$endif}

{$ifdef DebugMM}
{Frees a 64K chunk (pascal version)}
procedure FreeChunk(APChunkManager: PChunkManager);
var
  LOldUsageBitmap, LNewUsageBitmap, LBatchIndex, LBitNumber: Cardinal;
begin
  {Lock the batch managers}
  LockBatchManagers;
  {Get the batch manager index for this chunk}
  LBatchIndex := APChunkManager.BatchIndex;
  {Get the bit number from the chunk index}
  LBitNumber := (Cardinal(APChunkManager) - Cardinal(BatchStartAddress[LBatchIndex])) shr 16;
  {Get the old usage bitmap}
  LOldUsageBitmap := BatchUsageBitmap[LBatchIndex];
  LNewUsageBitmap := LOldUsageBitmap or (1 shl LBitNumber);
  {Set the new usage bitmap}
  BatchUsageBitmap[LBatchIndex] := LNewUsageBitmap;
  {Are all chunks now unused?}
  if LNewUsageBitmap <> $ffffffff then
  begin
    {Was this batch fully used? If so - put it back in the chunk list as the
     first batch with space}
    if LOldUsageBitmap = 0 then
    begin
      PreviousBatchIndex[NextBatchIndex[0]] := LBatchIndex;
      NextBatchIndex[LBatchIndex] := NextBatchIndex[0];
      PreviousBatchIndex[LBatchIndex] := 0;
      NextBatchIndex[0] := LBatchIndex;
    end;
    {Batch managers are no longer locked}
    BatchManagersLocked := False;
  end
  else
  begin
{$ifndef AlwaysFreeChunks}
    {Remove this batch manager if it is not the last batch in the list with free space}
    if NextBatchIndex[LBatchIndex] <> 0 then
    begin
{$endif}
      PreviousBatchIndex[NextBatchIndex[LBatchIndex]] := PreviousBatchIndex[LBatchIndex];
      NextBatchIndex[PreviousBatchIndex[LBatchIndex]] := NextBatchIndex[LBatchIndex];
      {Batch managers are no longer locked}
      BatchManagersLocked := False;
      {Free the memory allocated by this batch}
      VirtualFree(BatchStartAddress[LBatchIndex], 0, MEM_RELEASE);
{$ifndef AlwaysFreeChunks}
    end
    else
    begin
      {Batch managers are no longer locked}
      BatchManagersLocked := False;
    end;
{$endif}
  end;
end;
{$else}
{Frees a 64K chunk (asm version)}
procedure FreeChunk(APChunkManager: PChunkManager);
asm
  {On entry: eax = APChunkManager}
{$ifdef CountThreadContentions}
  add dword ptr BatchOperations, 1
  adc dword ptr BatchOperations[4], 0
{$endif}
  {Save ebx}
  push ebx
  {ebx = chunk to free}
  mov ebx, eax
  {Lock the batch managers}
@LockLoop:
  xor al, al
  mov dl, 1
  {Attempt to lock the batches}
  lock cmpxchg BatchManagersLocked, dl
  jz @BatchesLocked
  {Couldn't lock the batches - sleep and try again}
{$ifdef CountThreadContentions}
  add dword ptr BatchContentions, 1
  adc dword ptr BatchContentions[4], 0
{$endif}
  push False
  push InitialSleepTime
  call Windows.SleepEx
  {Try again}
  xor al, al
  mov dl, 1
  {Attempt to lock the batches}
  lock cmpxchg BatchManagersLocked, dl
  jz @BatchesLocked
  {Couldn't lock the batches - sleep and try again}
{$ifdef CountThreadContentions}
  add dword ptr BatchContentions, 1
  adc dword ptr BatchContentions[4], 0
{$endif}
  push False
  push AdditionalSleepTime
  call Windows.SleepEx
  {Try again}
  jmp @LockLoop
@BatchesLocked:
  {Get the batch manager index for this chunk in edx}
  movzx edx, TChunkManager([ebx]).BatchIndex
  {Get the bit number from the chunk index in ecx}
  mov ecx, ebx
  sub ecx, dword ptr [BatchStartAddress + edx * 4]
  shr ecx, 16
  {eax = the or value}
  mov eax, 1
  shl eax, cl
  {Get the old usage bitmap in ecx}
  mov ecx, dword ptr [BatchUsageBitmap + edx * 4]
  {Get the new usage bitmap in eax}
  or eax, ecx
  {Store the new usage bitmap}
  mov dword ptr [BatchUsageBitmap + edx * 4], eax
  {Is the whole batch now free?}
  cmp eax, -1
  je @BatchEmpty
  {Was the batch previously completely used?}
  test ecx, ecx
  jnz @FreeChunkDone
  {Add the batch back into the linked list as the first batch manager}
  movzx eax, word ptr [NextBatchIndex]
  mov word ptr [PreviousBatchIndex + eax + eax], dx
  mov word ptr [NextBatchIndex + edx + edx], ax
  mov word ptr [PreviousBatchIndex + edx + edx], 0
  mov word ptr [NextBatchIndex], dx
@FreeChunkDone:
  {Unlock the batch managers}
  mov BatchManagersLocked, False
  pop ebx
  ret
@BatchEmpty:
  {Get the next partially free batch index}
  movzx eax, word ptr [NextBatchIndex + edx + edx]
{$ifndef AlwaysFreeChunks}
  {Is this the last batch in the linked list? Do not free if it is.}
  test eax, eax
  jz @FreeChunkDone
{$endif}
  {Remove the batch from the linked list}
  movzx ecx, word ptr [PreviousBatchIndex + edx + edx]
  mov word ptr [NextBatchIndex + ecx + ecx], ax
  mov word ptr [PreviousBatchIndex + eax + eax], cx
  {Unlock the batch managers}
  mov BatchManagersLocked, False
  {Get the start address of the batch}
  mov eax, dword ptr [BatchStartAddress + edx * 4]
  {Free the chunk}
  push MEM_RELEASE
  push 0
  push eax
  call VirtualFree
  {Restore ebx}
  pop ebx
end;
{$endif}

{$ifdef DebugMM}
{Locks the given block type so only this thread can access it}
procedure LockBlockType(ABlockTypeIndex: Cardinal);
asm
  {On entry: eax = Block Type Index}
{$ifdef CountThreadContentions}
  add dword ptr SmallBlockOperations, 1
  adc dword ptr SmallBlockOperations[4], 0
{$endif}
  {ecx = Block Type Index}
  mov ecx, eax
@LockLoop:
  xor al, al
  mov dl, 1
  {Attempt to grab the block type}
  lock cmpxchg [ecx + BlockTypeLocked], dl
  jz @Done
  {Couldn't grab the block type - sleep and try again}
{$ifdef CountThreadContentions}
  add dword ptr SmallBlockContentions, 1
  adc dword ptr SmallBlockContentions[4], 0
{$endif}
  push ecx
  push False
  push InitialSleepTime
  call Windows.SleepEx
  pop ecx
  {Try again}
  xor al, al
  mov dl, 1
  {Attempt to grab the block type}
  lock cmpxchg [ecx + BlockTypeLocked], dl
  jz @Done
  {Couldn't grab the block type - sleep and try again}
{$ifdef CountThreadContentions}
  add dword ptr SmallBlockContentions, 1
  adc dword ptr SmallBlockContentions[4], 0
{$endif}
  push ecx
  push False
  push AdditionalSleepTime
  call Windows.SleepEx
  pop ecx
  {Try again}
  jmp @LockLoop
@Done:
end;
{$endif}

{$ifdef DebugMM}
{Replacement for SysGetMem (pascal version)}
function FastGetMem(ASize: Integer): Pointer;
var
  LSmallBlockTypeIndex, LBlockNumber, LNewFirstFreeBlockNumber,
    LAllocSize: Cardinal;
  LPChunkManager, LPNewFirstChunkManager: PChunkManager;
  LBlockListPointer, LBlockListOffset: integer;
begin
{$ifdef CountGetMemAndFreeMemOperations}
  {Increment the number of getmem/freemem operations}
  Inc(TotalGetMemAndFreeMemOperations);
{$endif}
  if Cardinal(ASize) <= MaximumSmallBlockSize then
  begin
    {Get the block type index}
    LSmallBlockTypeIndex := AllocationSizeToBlockType[(ASize - 1) shr 4];
    {Lock the block type}
    LockBlockType(LSmallBlockTypeIndex);
    {Get the first manager for this block type}
    LPChunkManager := FirstPartiallyFreeChunk[LSmallBlockTypeIndex];
    {Is there a chunk with space?}
    if LPChunkManager <> @DummyChunkManager then
    begin
      {Increment the number of allocated blocks}
      Inc(LPChunkManager.BlocksInUse);
      {Get the block number}
      LBlockNumber := LPChunkManager.FirstFreeBlockNumber;
      {Get a pointer to the block in the linked list}
      LBlockListPointer := Integer(LPChunkManager)
        + SizeOf(TChunkManager) + Integer(LBlockNumber) * 2;
      {Get the new first free block number}
      LNewFirstFreeBlockNumber := PWord(LBlockListPointer)^;
{$ifndef NoPointerChecks}
      {Mark the block as allocated}
      PWord(LBlockListPointer)^ := AllocatedBlockMarker;
{$endif}
      {Set the new first free block number}
      LPChunkManager.FirstFreeBlockNumber := LNewFirstFreeBlockNumber;
      {Is the chunk now full?}
      if LNewFirstFreeBlockNumber = LastBlockMarker then
      begin
        {Chunk is full - remove it from the partially free list}
        LPNewFirstChunkManager := LPChunkManager.NextPartiallyFreeChunk;
        FirstPartiallyFreeChunk[LSmallBlockTypeIndex] := LPNewFirstChunkManager;
        LPNewFirstChunkManager.PreviousPartiallyFreeChunk := @DummyChunkManager;
      end;
      {Get the pointer to this block}
      Result := Pointer(Cardinal(LPChunkManager)
        + FirstBlockOffset[LSmallBlockTypeIndex]
        + LBlockNumber * BlockSize[LSmallBlockTypeIndex]);
    end
    else
    begin
      {Try to allocate a chunk}
      LPChunkManager := GetChunk;
      if LPChunkManager <> nil then
      begin
        {Set up this manager}
        LPChunkManager.BlockType := LSmallBlockTypeIndex;
        LPChunkManager.PreviousPartiallyFreeChunk := @DummyChunkManager;
        LPChunkManager.NextPartiallyFreeChunk := @DummyChunkManager;
        LPChunkManager.FirstFreeBlockNumber := 1;
        LPChunkManager.BlocksInUse := 1;
        {Make this the first manager for this block type}
        FirstPartiallyFreeChunk[LSmallBlockTypeIndex] := LPChunkManager;
        {Configure the available blocks stack}
        LBlockListPointer := integer(LPChunkManager) + HighestBlockNumberOffset[LSmallBlockTypeIndex];
        LBlockListOffset := integer(LPChunkManager) + SizeOf(TChunkManager) - LBlockListPointer;
        LBlockNumber := 1;
        while LBlockListOffset < 0 do
        begin
          PWord(LBlockListPointer + LBlockListOffset)^ := LBlockNumber;
          Inc(LBlockListOffset, 2);
          Inc(LBlockNumber);
        end;
        {Mark the last block}
        PWord(LBlockListPointer)^ := LastBlockMarker;
{$ifndef NoPointerChecks}
        {Mark the first block as allocated}
        PWord(Cardinal(LPChunkManager) + SizeOf(TChunkManager))^ := AllocatedBlockMarker;
{$endif}
        {Set the result pointer}
        Result := Pointer(Cardinal(LPChunkManager) + FirstBlockOffset[LSmallBlockTypeIndex]);
      end
      else
      begin
        {Out of memory}
        Result := nil;
      end;
    end;
    {Unlock this block type}
    BlockTypeLocked[LSmallBlockTypeIndex] := False;
  end
  else
  begin
    {Larger block: Add the size of the chunk manager and round up to the next
     64K boundary}
    LAllocSize := (ASize + (LargeBlockGranularity - 1 + SizeOf(TChunkManager))) and -LargeBlockGranularity;
    {Allocate directly through VirtualAlloc}
    Result := VirtualAlloc(nil, LAllocSize, MEM_COMMIT or MEM_TOP_DOWN, PAGE_READWRITE);
    if Result <> nil then
    begin
      {Get the chunk manager}
      LPChunkManager := Result;
      {Flag the block type as a large block}
      LPChunkManager.BlockType := LargeBlockTypeIndex;
      LPChunkManager.LargeBlockRequestedSize := ASize;
      LPChunkManager.LargeBlockAllocatedSize := LAllocSize - SizeOf(TChunkManager);
      {Set the result}
      Inc(PByte(Result), SizeOf(TChunkManager));
    end;
  end;
end;
{$else}
{Replacement for SysGetMem (asm version)}
function FastGetMem(ASize: Integer): Pointer;
{$ifndef NoMMX}
const
  {The linked list initialization numbers}
  InitVal0: Int64 = $0004000300020001;
  InitVal1: Int64 = $0008000700060005;
  {The MMX linked list initialization add number}
  InitAdd: Int64 = $0008000800080008;
{$endif}
asm
  {On entry:
    eax = the requested block size}
{$ifdef CountGetMemAndFreeMemOperations}
  {Increment the number of getmem/freemem operations}
  add dword ptr TotalGetMemAndFreeMemOperations, 1;
  adc dword ptr TotalGetMemAndFreeMemOperations[4], 0;
{$endif}
  {Save registers}
  push ebx
  {Is it a small or large block?}
  cmp eax, MaximumSmallBlockSize
  ja @LargeBlock
{$ifdef CountThreadContentions}
  add dword ptr SmallBlockOperations, 1
  adc dword ptr SmallBlockOperations[4], 0
{$endif}
  {Subtract 1 byte and divide the requested size by 16 to get the block size
   lookup number}
  dec eax
  shr eax, 4
  {Get the block type in ebx}
  movzx ebx, byte ptr AllocationSizeToBlockType[eax]
@LockBlockTypeLoop:
  xor al, al
  mov cl, 1
  {Attempt to grab the block type}
  lock cmpxchg byte ptr [BlockTypeLocked + ebx], cl
  je @GotLockOnBlockType
  {Couldn't grab the block type - sleep and try again}
{$ifdef CountThreadContentions}
  add dword ptr SmallBlockContentions, 1
  adc dword ptr SmallBlockContentions[4], 0
{$endif}
  push False
  push InitialSleepTime
  call Windows.SleepEx
  {Try again}
  xor al, al
  mov cl, 1
  {Attempt to grab the block type}
  lock cmpxchg byte ptr [BlockTypeLocked + ebx], cl
  je @GotLockOnBlockType
  {Couldn't grab the block type - sleep and try again}
{$ifdef CountThreadContentions}
  add dword ptr SmallBlockContentions, 1
  adc dword ptr SmallBlockContentions[4], 0
{$endif}
  push False
  push AdditionalSleepTime
  call Windows.SleepEx
  {Try again}
  jmp @LockBlockTypeLoop
@GotLockOnBlockType:
  {Get the first chunk manager with space in eax}
  mov eax, dword ptr [FirstPartiallyFreeChunk + ebx * 4]
  {Is the chunk valid?}
  cmp eax, offset DummyChunkManager
  je @AllocateChunk
  {Increment the number of allocated blocks}
  inc TChunkManager([eax]).BlocksInUse;
  {Get the block number in edx}
  movzx edx, TChunkManager([eax]).FirstFreeBlockNumber
  {Get the new first free block number in ecx}
  movzx ecx, word ptr [eax + Type(TChunkManager) + edx * 2]
{$ifndef NoPointerChecks}
  {Flag this block as allocated}
  mov word ptr [eax + Type(TChunkManager) + edx * 2], AllocatedBlockMarker
{$endif}
  {Set the new first free block number}
  mov TChunkManager([eax]).FirstFreeBlockNumber, cx
  {Are there no longer any free blocks? If so, remove the chunk from the list}
  cmp cx, LastBlockMarker
  jne @ChunkNotFull
  {Chunk is full - remove it from the partially free list}
  {Get the next chunk manager in ecx}
  mov ecx, TChunkManager([eax]).NextPartiallyFreeChunk
  mov dword ptr [FirstPartiallyFreeChunk + ebx * 4], ecx
  mov TChunkManager([ecx]).PreviousPartiallyFreeChunk, offset DummyChunkManager
@ChunkNotFull:
  {Set the result pointer}
  {Get the block size in ax}
  mov ax, word ptr [BlockSize + ebx + ebx]
  {Multiply with the block number}
  mul ax, dx
  {Add the first block offset}
  add ax, word ptr [FirstBlockOffset + ebx + ebx]
@AllocateDone:
  {Unlock the block type.}
  mov byte ptr [BlockTypeLocked + ebx], 0
  {Restore ebx}
  pop ebx
  {Done}
  ret
@AllocateChunk:
  {ebx = pointer to block type}
  {Get a chunk}
  call GetChunk
  {Out of memory?}
  test eax, eax
  jz @AllocateDone
  {Set up this manager}
  mov TChunkManager([eax]).BlockType, bl
  mov TChunkManager([eax]).PreviousPartiallyFreeChunk, offset DummyChunkManager
  mov TChunkManager([eax]).NextPartiallyFreeChunk, offset DummyChunkManager
  mov dword ptr TChunkManager([eax]).FirstFreeBlockNumber, $00010001
  {Make this the first manager for this block type}
  mov dword ptr [FirstPartiallyFreeChunk + ebx * 4], eax
  {Configure the available blocks stack}
  {point edx to the last block number}
  mov edx, eax
  mov dx, word ptr [HighestBlockNumberOffset + ebx + ebx]
  {Get the loop counter in ecx}
  lea ecx, [eax + Type(TChunkManager)]
  sub ecx, edx
{$ifndef NoMMX}
  movq mm0, InitVal0
  movq mm1, InitVal1
  movq mm2, InitAdd
@ListInitLoop:
  movq [edx + ecx], mm0
  movq [edx + ecx + 8], mm1
  paddw mm0, mm2
  paddw mm1, mm2
  add ecx, 16
  js @ListInitLoop
  {Exit mmx state}
  emms
{$else}
  push eax
  mov eax, $00020001
@ListInitLoop:
  mov [edx + ecx], eax
  add eax, $00020002
  mov [edx + ecx + 4], eax
  add eax, $00020002
  add ecx, 8
  js @ListInitLoop
  pop eax
{$endif}
{$ifndef NoPointerChecks}
  {Mark the first block as allocated}
  mov word ptr [eax + Type(TChunkManager)], AllocatedBlockMarker
{$endif}
  {Mark the last block}
  mov word ptr [edx], LastBlockMarker;
  {Set the result pointer}
  mov ax, word ptr [FirstBlockOffset + ebx * 2]
  {Done}
  jmp @AllocateDone
@LargeBlock:
  {Save esi}
  push esi
  {Large block: Round up to the next boundary, adding the size of the
   chunk manager}
  lea ebx, [eax + LargeBlockGranularity - 1 + Type(TChunkManager)]
  and ebx, -LargeBlockGranularity
  {Save the requested size in esi}
  mov esi, eax
  {Call VirtualAlloc directly}
  push PAGE_READWRITE
  push MEM_COMMIT or MEM_TOP_DOWN
  push ebx
  push 0
  call VirtualAlloc
  {Out of memory?}
  test eax, eax
  jz @LargeAllocDone
  {Flag the block type as a large block}
  mov TChunkManager([eax]).BlockType, LargeBlockTypeIndex
  mov TChunkManager([eax]).LargeBlockRequestedSize, esi
  sub ebx, Type(TChunkManager)
  mov TChunkManager([eax]).LargeBlockAllocatedSize, ebx
  add eax, Type(TChunkManager)
@LargeAllocDone:
  pop esi
  pop ebx
end;
{$endif}

{$ifdef DebugMM}
{Replacement for SysFreeMem (pascal version)}
function FastFreeMem(APointer: Pointer): Integer;
var
  LSmallBlockTypeIndex, LBlockNumber: Cardinal;
  LPChunkManager, LPOldFirstManager, LPPrevManager, LPNextManager: PChunkManager;
  LPBlockListPointer: PWord;
begin
{$ifdef CountGetMemAndFreeMemOperations}
  {Increment the number of getmem/freemem operations}
  Inc(TotalGetMemAndFreeMemOperations);
{$endif}
  {Get the chunk manager}
  LPChunkManager := PChunkManager(Cardinal(APointer) and $ffff0000);
  {Get the block type index}
  LSmallBlockTypeIndex := LPChunkManager.BlockType;
  {Is it a large or small block chunk?}
  if LSmallBlockTypeIndex < NumSmallBlockTypes then
  begin
    {Small Block}
    {Lock this block type for multithreaded apps}
    LockBlockType(LSmallBlockTypeIndex);
    {Determine the block number}
    LBlockNumber := ((Cardinal(APointer) and $ffff) - FirstBlockOffset[LSmallBlockTypeIndex])
      div BlockSize[LSmallBlockTypeIndex];
    {Get a pointer to the block in the linked list}
    LPBlockListPointer := PWord(Cardinal(LPChunkManager) + SizeOf(TChunkManager)
      + LBlockNumber * 2);
{$ifndef NoPointerChecks}
    {Was this block allocated?}
    if LPBlockListPointer^ <> AllocatedBlockMarker then
    begin
      {Unlock this block type}
      BlockTypeLocked[LSmallBlockTypeIndex] := False;
      {No error}
      Result := -1;
      {abort}
      exit;
    end;
{$endif}
    {Update the next available block}
    LPBlockListPointer^ := LPChunkManager.FirstFreeBlockNumber;
    {Was this chunk previously full?}
    if LPChunkManager.FirstFreeBlockNumber = LastBlockMarker then
    begin
      {Insert this as the first partially free chunk for the block size}
      LPOldFirstManager := FirstPartiallyFreeChunk[LSmallBlockTypeIndex];
      LPChunkManager.NextPartiallyFreeChunk := LPOldFirstManager;
      LPOldFirstManager.PreviousPartiallyFreeChunk := LPChunkManager;
      LPChunkManager.PreviousPartiallyFreeChunk := @DummyChunkManager;
      FirstPartiallyFreeChunk[LSmallBlockTypeIndex] := LPChunkManager;
    end;
    {Set this as the first available block}
    LPChunkManager.FirstFreeBlockNumber := LBlockNumber;
    {Decrement the number of allocated blocks}
    Dec(LPChunkManager.BlocksInUse);
    {Is the entire chunk now free?}
    if (LPChunkManager.BlocksInUse = 0) then
    begin
      {Get the next chunk manager}
      LPNextManager := LPChunkManager.NextPartiallyFreeChunk;
{$ifndef AlwaysFreeChunks}
      {Is this the last one? Free it if not.}
      if LPNextManager <> @DummyChunkManager then
      begin
{$endif}
        {Remove this manager}
        LPPrevManager := LPChunkManager.PreviousPartiallyFreeChunk;
        if LPPrevManager <> @DummyChunkManager then
          LPPrevManager.NextPartiallyFreeChunk := LPNextManager
        else
          FirstPartiallyFreeChunk[LSmallBlockTypeIndex] := LPNextManager;
        LPNextManager.PreviousPartiallyFreeChunk := LPPrevManager;
        {Unlock this block type}
        BlockTypeLocked[LSmallBlockTypeIndex] := False;
        {Release this chunk}
        FreeChunk(LPChunkManager);
{$ifndef AlwaysFreeChunks}
      end
      else
      begin
        BlockTypeLocked[LSmallBlockTypeIndex] := False;
      end;
{$endif}
    end
    else
    begin
      {Unlock this block type}
      BlockTypeLocked[LSmallBlockTypeIndex] := False;
    end;
    {No error}
    Result := 0;
  end
  else
  begin
    {Large block: Free it through VirtualFree}
    Dec(PByte(APointer), SizeOf(TChunkManager));
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
  {On entry: eax = APointer}
{$ifdef CountGetMemAndFreeMemOperations}
  {Increment the number of getmem/freemem operations}
  add dword ptr TotalGetMemAndFreeMemOperations, 1;
  adc dword ptr TotalGetMemAndFreeMemOperations[4], 0;
{$endif}
  {save registers}
  push ebx
  push esi
  {Get the chunk manager in esi}
  mov esi, eax
  and esi, $ffff0000
  {Get the block type in ecx}
  movzx ecx, TChunkManager([esi]).BlockType
  {Save the pointer in ebx}
  mov ebx, eax
  {Is it a small block type?}
  cmp cl, NumSmallBlockTypes
  jae @LargeBlock
{$ifdef CountThreadContentions}
  add dword ptr SmallBlockOperations, 1
  adc dword ptr SmallBlockOperations[4], 0
{$endif}
  {Lock the block type}
@LockBlockTypeLoop:
  xor al, al
  mov dl, 1
  {Attempt to grab the block type}
  lock cmpxchg byte ptr [BlockTypeLocked + ecx], dl
  je @GotLockOnBlockType
  {Couldn't grab the block type - sleep and try again}
{$ifdef CountThreadContentions}
  add dword ptr SmallBlockContentions, 1
  adc dword ptr SmallBlockContentions[4], 0
{$endif}
  push ecx
  push False
  push InitialSleepTime
  call Windows.SleepEx
  pop ecx
  {Try again}
  xor al, al
  mov dl, 1
  {Attempt to grab the block type}
  lock cmpxchg byte ptr [BlockTypeLocked + ecx], dl
  je @GotLockOnBlockType
  {Couldn't grab the block type - sleep and try again}
{$ifdef CountThreadContentions}
  add dword ptr SmallBlockContentions, 1
  adc dword ptr SmallBlockContentions[4], 0
{$endif}
  push ecx
  push False
  push AdditionalSleepTime
  call Windows.SleepEx
  pop ecx
  {Try again}
  jmp @LockBlockTypeLoop
@GotLockOnBlockType:
  {Get the block number in edx}
  sub bx, word ptr [FirstBlockOffset + ecx * 2]
  movzx eax, bx
  mul dword ptr [BlockSizeInverse + ecx * 4]
{$ifndef NoPointerChecks}
  mov eax, -1
  cmp word ptr [esi + Type(TChunkManager) + edx * 2], AllocatedBlockMarker
  jne @DoneResultAlreadySet
{$endif}
  {Update the next available block}
  mov ax, TChunkManager([esi]).FirstFreeBlockNumber
  mov word ptr [esi + Type(TChunkManager) + edx * 2], ax
  {Set this as the new first free block}
  mov TChunkManager([esi]).FirstFreeBlockNumber, dx
  {Decrement the allocated block count. Is the entire chunk now free?}
  dec TChunkManager([esi]).BlocksInUse
  jz @ChunkEmpty
  {Was this chunk previously full?}
  cmp ax, LastBlockMarker
  je @ChunkWasFull
@SmallFreeDone:
  xor eax, eax
@DoneResultAlreadySet:
  mov byte ptr [BlockTypeLocked + ecx], 0
  pop esi
  pop ebx
  ret
@ChunkWasFull:
  {Insert this as the first partially free chunk for the block size}
  mov TChunkManager([esi]).PreviousPartiallyFreeChunk, offset DummyChunkManager
  mov eax, dword ptr [FirstPartiallyFreeChunk + ecx * 4]
  mov TChunkManager([esi]).NextPartiallyFreeChunk, eax
  mov TChunkManager([eax]).PreviousPartiallyFreeChunk, esi
  mov dword ptr [FirstPartiallyFreeChunk + ecx * 4], esi
  jmp @SmallFreeDone
@ChunkEmpty:
  mov edx, TChunkManager([esi]).NextPartiallyFreeChunk
{$ifndef AlwaysFreeChunks}
  {Is this the last partially free chunk for this block type? -> We don't free
   the last one}
  cmp edx, offset DummyChunkManager
  je @SmallFreeDone
{$endif}
  {Remove this manager}
  mov eax, TChunkManager([esi]).PreviousPartiallyFreeChunk
  mov TChunkManager([edx]).PreviousPartiallyFreeChunk, eax
  cmp eax, offset DummyChunkManager
  je @FirstManager
  mov TChunkManager([eax]).NextPartiallyFreeChunk, edx
  jmp @NotFirstManager
@FirstManager:
  mov dword ptr [FirstPartiallyFreeChunk + ecx * 4], edx
@NotFirstManager:
  {Unlock the block type}
  mov byte ptr [BlockTypeLocked + ecx], 0
  {Free this chunk}
  mov eax, esi
  call FreeChunk
  xor eax, eax
  pop esi
  pop ebx
  ret
@LargeBlock:
  {Larger block: Free the block through VirtualFree}
  push MEM_RELEASE
  push 0
  push esi
  call VirtualFree
  {VirtualFree returns >0 if all is ok}
  cmp eax, 1
  {Return 0 on all ok}
  sbb eax, eax
  {Restore registers}
  pop esi
  pop ebx
end;
{$endif}

{$ifdef DebugMM}
{Replacement for SysReallocMem (pascal version)}
function FastReallocMem(APointer: Pointer; ASize: Integer): Pointer;
var
  LCurrentBlockSize, LUserAllocatedSize, LNewUserAllocatedSize: integer;
  LBlockTypeIndex, LMoveSize: Cardinal;
  LPChunkManager: PChunkManager;
begin
  {The manager index is the address divided by 64K}
  LPChunkManager := PChunkManager(Cardinal(APointer) and $ffff0000);
  {Get the block type index}
  LBlockTypeIndex := LPChunkManager.BlockType;
  {Is it a large or small block chunk?}
  if LBlockTypeIndex < NumSmallBlockTypes then
  begin
    {Small Block: Get the current block size}
    LCurrentBlockSize := BlockSize[LBlockTypeIndex];
    {We don't track the allocated size for small blocks - assume its the same
     as the block size.}
    LUserAllocatedSize := LCurrentBlockSize;
  end
  else
  begin
    {It's a large block chunk: Get the current block size}
    LCurrentBlockSize := LPChunkManager.LargeBlockAllocatedSize;
    {Get the size allocated by the user}
    LUserAllocatedSize := LPChunkManager.LargeBlockRequestedSize;
  end;
  {Do we need to do a physical reallocation? We only reallocate if the new
   size is larger than the current block size, or if the new size is less
   than a quarter of the block size (and it's not already the smallest
   available block size.)}
  if (ASize <= LCurrentBlockSize)
{$ifndef NeverDownsizeOnReallocMem}
    and ((ASize >= (LCurrentBlockSize shr 2))
      or (LCurrentBlockSize < MinimumBlockSizeToAllowDownsizing))
{$endif}
  then
  begin
    {No need to reallocate}
    Result := APointer;
    LNewUserAllocatedSize := LCurrentBlockSize;
  end
  else
  begin
    {Determine the number of bytes to move across}
    if ASize > LUserAllocatedSize then
    begin
      LMoveSize := LUserAllocatedSize;
      {This pointer is being reallocated to a larger block and therefore it is
       logical to assume that it may be enlarged again. Since reallocations are
       expensive, we pad the requested size to avoid unnecessary future move
       operations.}
      LNewUserAllocatedSize := ASize + ASize shr ReallocPaddingFactor;
    end
    else
    begin
      LMoveSize := ASize;
      LNewUserAllocatedSize := ASize;
    end;
    {Attempt to allocate the new block}
    Result := FastGetMem(LNewUserAllocatedSize);
    if Result <> nil then
    begin
      {Move the data across}
      Move16(APointer, Result, LMoveSize);
      {Free the old block}
      FastFreeMem(APointer);
    end;
  end;
  {Is the new block a large block?}
  if (LNewUserAllocatedSize > MaximumSmallBlockSize) then
  begin
    {Set the correct requested size in the manager}
    LPChunkManager := PChunkManager(Cardinal(Result) and $ffff0000);
    LPChunkManager.LargeBlockRequestedSize := ASize;
  end;
end;
{$else}
{Replacement for SysReallocMem (asm version)}
function FastReallocMem(APointer: Pointer; ASize: Integer): Pointer;
asm
  {On entry:
    eax = pointer
    edx = requested size}
  {Save registers}
  push ebx
  {Get the chunk manager in ebx}
  mov ebx, eax
  and ebx, $ffff0000
  {Get the block type index in ecx}
  movzx ecx, TChunkManager([ebx]).BlockType
  {Is it a large or small block chunk?}
  cmp cl, NumSmallBlockTypes
  jae @LargeBlock
  {Small block - get current size in ecx}
  movzx ecx, word ptr [BlockSize + ecx + ecx]
  {The user requested size is the same for small blocks}
  mov ebx, ecx
@GotOldBlockDetails:
  {Current block size in ecx, User allocated size in ebx:
   Do we need to do a physical reallocation? We only reallocate if the new
   size is larger than the current block size, or if the new size is less
   than a quarter of the block size (and it's not already the smallest
   available block size.)}
  {New block larger? - must realloc if so}
  cmp edx, ecx
  jg @MustRealloc
{$ifndef NeverDownsizeOnReallocMem}
  {Current block size too small - no realloc}
  cmp ecx, MinimumBlockSizeToAllowDownsizing
  jl @NoRealloc
  {Less than 25% of block used - realloc}
  shr ecx, 2
  cmp edx, ecx
  jl @MustRealloc
  add ecx, ecx
  add ecx, ecx
@NoRealloc:
{$endif}
  {The pointer remains the same, but if this is a large block, we must update
  the user size}
  cmp ecx, MaximumSmallBlockSize
  jng @NoReallocDone
  {Set the correct user size in the manager}
  mov ebx, eax
  and ebx, $ffff0000
  mov TChunkManager([ebx]).LargeBlockRequestedSize, edx
@NoReallocDone:
  pop ebx
  ret
@LargeBlock:
  {It's a large block chunk: Get the current block size}
  mov ecx, TChunkManager([ebx]).LargeBlockAllocatedSize
  {Get the size requested by the user}
  mov ebx, TChunkManager([ebx]).LargeBlockRequestedSize
  jmp @GotOldBlockDetails
@MustRealloc:
  {Save registers}
  push esi
  push edi
  push ebp
  {Default new block size = the requested size}
  mov esi, edx
  {Determine the number of bytes to move across: grow or shrink?}
  cmp edx, ebx
  jb @Shrink
  {Pad the block size for an upsize. Bytes to move is the old block size}
  shr esi, ReallocPaddingFactor
  add esi, edx
  jmp @DoGetMem
@Shrink:
  {Shrink - byte move count is the new block size}
  mov ebx, edx
@DoGetMem:
  {eax = old pointer, edx = requested size, esi = new block size, ebx = move count}
  {Save data}
  mov ebp, eax
  mov edi, edx
  {ebp = old pointer, edi = requested size, esi = new block size, ebx = move count}
  mov eax, esi
  call FastGetMem
  {Failed?}
  test eax, eax
  jz @ReallocDone
  {Move the data across}
  mov ecx, ebx
  mov ebx, eax
  mov edx, eax
  mov eax, ebp
  call Move16
  {ebp = old pointer, edi = requested size, esi = new block size, ebx = new pointer}
  {Free the old block}
  mov eax, ebp
  call FastFreeMem
  {Return the new pointer}
  mov eax, ebx
  {Is the new block a large block?}
  cmp esi, MaximumSmallBlockSize
  jng @ReallocDone
  {Set the correct user size in the manager}
  and ebx, $ffff0000
  mov TChunkManager([ebx]).LargeBlockRequestedSize, edi
@ReallocDone:
  pop ebp
  pop edi
  pop esi
  pop ebx
end;
{$endif}

{Sets up and installs the memory manager}
procedure InstallMemoryManager;
var
  LPreviousBlockSize, LBlockTypeIndex, LAllocInd: word;
  i, LCurrentProcessID, LBlocksPerChunk: Cardinal;
{$ifdef DebugMM}
  j, blkoff, k: Cardinal;
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
      {No chunk manager for this type yet}
      FirstPartiallyFreeChunk[LBlockTypeIndex] := @DummyChunkManager;
      {Get the number of blocks per chunk. The block list and chunk manager is
       allocated in the same chunk as the blocks themselves, so there is a two
       byte overhead per block.}
      LBlocksPerChunk := (65536 - SizeOf(TChunkManager))
        div (BlockSize[LBlockTypeIndex] + 2);
      {Align the block start to 16 bytes, and check that it still fits}
      if (((SizeOf(TChunkManager) + LBlocksPerChunk * 2 + 15) and -16)
        + BlockSize[LBlockTypeIndex] * LBlocksPerChunk) > 65536 then
      begin
        {The last block is dropped due to alignment requirements}
        Dec(LBlocksPerChunk);
      end;
      {Set the highest block index}
      HighestBlockNumberOffset[LBlockTypeIndex] := SizeOf(TChunkManager) + (LBlocksPerChunk - 1) * 2;
      {Set the offset of the first block}
      FirstBlockOffset[LBlockTypeIndex] := (SizeOf(TChunkManager) + LBlocksPerChunk * 2 + 15) and -16;
      {Update the block size lookup table}
      for LAllocInd := (LPreviousBlockSize shr 4) to (BlockSize[LBlockTypeIndex] shr 4) - 1 do
        AllocationSizeToBlockType[LAllocInd] := LBlockTypeIndex;
      {Update previous block size}
      LPreviousBlockSize := BlockSize[LBlockTypeIndex];
      {Set the reciprocal block size scaled by 2^32. Scaling by 2^32 gives us at
       least 5 digits accuracy when stored as a cardinal, which is sufficient for
       this case since the maximum multiplier is 5 digits.}
      BlockSizeInverse[LBlockTypeIndex] :=
        ($100000000 + LPreviousBlockSize - 1) div LPreviousBlockSize;
{$ifdef DebugMM}
      {Check the validity of the block size inverse}
      for j := 0 to LBlocksPerChunk - 1 do
      begin
        blkoff := j * BlockSize[LBlockTypeIndex];
        asm
          mov eax, blkoff
          movzx edx, LBlockTypeIndex
          mov edx, dword ptr [BlockSizeInverse + edx * 4]
          mul edx
          mov k, edx
        end;
        if k <> j then
          System.Error(reInvalidPtr);
      end;
{$endif}
    end;
{$ifdef DebugMM}
    {Check the block size lookup table for validity}
    for k := 1 to MaximumSmallBlockSize do
    begin
      LBlockTypeIndex := AllocationSizeToBlockType[(k - 1) shr 4];
      if (BlockSize[LBlockTypeIndex] < k) then
        System.Error(reInvalidPtr);
      if (LBlockTypeIndex > 0) and (BlockSize[LBlockTypeIndex - 1] >= k) then
        System.Error(reInvalidPtr);
    end;
    {Check that the first block offset is valid}
    if (HighestBlockNumberOffset[LBlockTypeIndex] + 2) > FirstBlockOffset[LBlockTypeIndex] then
      System.Error(reInvalidPtr);
    {Check that the block size is a multiple of 16}
    if BlockSize[LBlockTypeIndex] <> (BlockSize[LBlockTypeIndex] div 16 * 16) then
      System.Error(reInvalidPtr);
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
var
  LBlockTypeInd: byte;
  LChunkManager, LNextChunkManager: PChunkManager;
  LBatchIndex: integer;
begin
  {Is this the owner of the shared MM window?}
  if OwnsMMWindow then
  begin
    DestroyWindow(MMWindow);
{$ifndef AlwaysFreeChunks}
    {Free all empty chunks}
    for LBlockTypeInd := 0 to NumSmallBlockTypes - 1 do
    begin
      LNextChunkManager := FirstPartiallyFreeChunk[LBlockTypeInd];
      while LNextChunkManager <> nil do
      begin
        LChunkManager := LNextChunkManager;
        LNextChunkManager := LNextChunkManager.NextPartiallyFreeChunk;
        {Should we free the chunk?}
        if LChunkManager.BlocksInUse = 0 then
          FreeChunk(LChunkManager);
      end;
    end;
    {Free all empty batches}
    LBatchIndex := NextBatchIndex[0];
    while LBatchIndex > 0 do
    begin
      if BatchUsageBitmap[LBatchIndex] = $ffffffff then
        VirtualFree(BatchStartAddress[LBatchIndex], 0, MEM_RELEASE);
      LBatchIndex := NextBatchIndex[LBatchIndex];
    end;
{$endif}
  end;
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
  LHasLeaks: boolean;
  LBlockTypeInd: byte;
  LNumLeaks: integer;
  LChunkManager: PChunkManager;
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
    LChunkManager := FirstPartiallyFreeChunk[LBlockTypeInd];
    while LChunkManager <> nil do
    begin
      Inc(LNumLeaks, LChunkManager.BlocksInUse);
      LChunkManager := LChunkManager.NextPartiallyFreeChunk;
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
      LMsgPtr := CardinalToStrBuf(BlockSize[LBlockTypeInd], LMsgPtr);
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
