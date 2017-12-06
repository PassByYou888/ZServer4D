{PSD Memory Manager:
 Based on ideas used in RecyclerMM written by Eric Grange (egrange@glscene.org).
 Go to www.glscene.org for the original version and the full licence agreement.
 This work is copyright Eric Grange and Pierre le Riche and is distributed under
 the GPL licence. LGPL or MPL 1.1 licence requests may be granted on the
 following conditions:
  1) You are a good supporter of open-source software (purely subjective
   decision to be made by Eric Grange)
  2) This MM may not be used in commercial client relationship management,
   point-of-sale, restaurant or reservations software. (This is to protect the
   interests of the company I work for - after all, they paid me to program
   this.)
 If you use this memory manager please e-mail me your comments - good and bad.
 My e-mail address is pleriche@psd.co.za.

 Changes by Pierre le Riche:
  1) Major redesign and rewrite in BASM. Requires a CPU with MMX support.
  2) Batch and chunk managers are now statically allocated and there is no
   longer any dependency on the Borland MM.
  3) Reallocmem automatically allocates 25% more memory than is requested if a
   memory block is grown (this improves the odds that a future realloc can be
   done in place). This counters one of the drawbacks of the stack-based
   architecture - adjacent memory blocks cannot be joined.
  4) Actual requested block size for small blocks is not stored (saves memory
   and some computational overhead, but slows down the move operation on
   realloc).
  5) Some safety measures (e.g. bad pointer checks) and options removed for
   added speed.
  6) Hand-picked small block sizes for a good small block size spread with
   minimal memory wastage.
  7) Added a block size lookup table for faster determination of the
   appropriate small block type for getmem requests.

  Last change: 28 June 2004, version 1.0
}

unit PSDMemoryManager deprecated;

interface

const
  {The number of small block types}
  NumSmallBlockTypes = 48;
  {The number of chunks addressable by this process. VirtualAlloc has a 64K
   granularity, so chunks are always aligned on a 64K boundary}
  MaxNumChunks = 32768; // = 2GB / 64K

type

  {-------------Usage snapshot structures-----------------}

  {Status of a 64K chunk}
  TChunkStatus = (csUnallocated, csAllocated, csReserved, csSysAllocated,
    csSysReserved);

  {Info for a 64K memory block}
  TChunkInfo = packed record
    {The status of this chunk}
    ChunkStatus: TChunkStatus;
    {The block type index for this chunk}
    BlockTypeIndex: byte;
    {The number of bytes in this 64K chunk allocated by the user (only relevant
     for chunks with status csAllocated)}
    UserAllocatedSize: Cardinal;
  end;

  {Info for a small block type}
  TSmallBlockInfo = packed record
    {The size of blocks of this type}
    BlockSize: Word;
    {The number of chunks used for this block type}
    ChunksAllocated: Word;
    {The number of available blocks in chunks reserved for this small block
     type}
    AvailableBlockCount: Cardinal;
    {The number of allocated blocks}
    UserAllocatedBlocks: Cardinal;
  end;

  {A usage snapshot of the memory manager (returned by GetUsageSnapshot)}
  TPSDMemoryManagerUsageSnapshot = packed record
    {Total memory allocation from the virtual memory pool}
    TotalVirtualAllocated: Cardinal;
    {The total user allocated memory size}
    TotalUserAllocated: Cardinal;
    {Chunk allocation statistics}
    ChunkBatchesAllocated: Word; //2MB blocks
    ChunksUsed: Word; //Number of 64K blocks used in the allocated batches
    {Memory map info (memory block info for all 64K blocks starting at address
     0 in 64K increments)}
    ChunkInfo: array[0..MaxNumChunks - 1] of TChunkInfo;
    {Small block allocation type info}
    SmallBlockInfo: array[0..NumSmallBlockTypes - 1] of TSmallBlockInfo;
  end;

{Returns a snapshot of the current memory usage of this process}
function GetUsageSnapshot: TPSDMemoryManagerUsageSnapshot;

implementation

uses Windows;

const
  {The size of an allocation chunk}
  ChunkSize = 64 * 1024;
  {The number chunks per batch}
  NumChunksPerBatch = 32;
  {The size of a batch}
  BatchSize = ChunkSize * NumChunksPerBatch;
  {The maximum number of batches.}
  MaxNumBatches = MaxNumChunks div NumChunksPerBatch;
  {The size of the largest small block handled. Must be a multiple of 16.}
  MaximumSmallBlockSize = 65520;
  {Block sizes smaller than this will never be downsized. Blocks of this size
   or greater will be downsized if reallocmem is called with a new size which
   is less than a quarter of the current block size.}
  MinimumBlockSizeToAllowDownsizing = 48;
  {When a pointer is reallocated and the new size is larger than the old size,
   the new requested size is multiplied with the factor (1 + 1 / 2^x) to
   facilitate faster subsequent reallocations.}
  ReallocPaddingFactor = 2;
  {The block type index for large blocks}
  LargeBlockTypeIndex = 100;
  {The granularity of large blocks. Large blocks are allocated directly by
   VirtualAlloc}
  LargeBlockGranularity = 32 * 1024; //32K
  {Hexadecimal characters}
  LHexTable: ShortString = '0123456789ABCDEF';

type

  {The various small block sizes supported (Record size = 16 bytes)}
  TSmallBlockType = packed record
    {The index of the first manager that manages blocks of this size}
    FirstManagerIndex: word;
    {True when this block type is locked (for multi-threaded apps)}
    BlockTypeLocked: boolean;
    Reserved1: byte;
    {The number of bytes per block}
    BlockSize: word;
    Reserved2: array[0..1] of byte;
    {This is 2^32 / BlockSize. Multiply by this value and take the high dword
     to get the block number from an offset into the chunk}
    BlockSizeInverse: Cardinal;
    {The initial stack pointer for a newly allocated chunk. Points to block 1,
     since block 0 is automatically returned for a getmem on a new chunk.}
    InitialStackPointer: word;
    Reserved3: array[0..1] of byte;
  end;

  {Manages a batch of chunks - 32 chunks per batch = 2MB (Structure size = 16
   bytes)}
  TBatchManager = packed record
    {The bitmap of used chunks (1 = chunk is used)}
    UsageBitmap: Cardinal;
    {The start chunk index for this batch}
    FirstChunkIndex: word;
    {The previous batch manager in the linked list}
    PreviousBatchIndex: word;
    {The next batch manager in the linked list}
    NextBatchIndex: word;
    {Padding}
    Reserved: array[0..5] of byte;
  end;

  {Chunk manager structure (Size = 8 bytes)}
  TChunkManager = packed record
    {The chunk manager type}
    case integer of
      0: {Small Block}
      (
        {The current stack pointer (points to the next free block, wraps to 0
         if the chunk is full)}
        FreeBlockStackPointer: word;
        {The index of the next small block manager that
         manages the same size blocks as this manager}
        NextChunkIndex: word;
        {The index of the previous small block manager that
         manages the same size blocks as this manager}
        PreviousChunkIndex: word;
        {The type of blocks managed}
        BlockTypeIndex: byte;
        {Padding}
        Reserved1: boolean;
      );
      1: {Large Block}
      (
        {The size requested by the user program}
        UserAllocatedSize: Cardinal;
        {The number of chunks allocated by this large block manager}
        Num32KBlocksAllocated: word;
        {Unused reserved byte}
        LargeBlockID: byte;
        {Padding}
        Reserved2: byte;
      );
  end;

var
  {Are the batch managers locked?}
  BatchManagersLocked: boolean;
  {The first batch manager in the linked list of batch managers}
  FirstBatchManagerIndex: word;
  {The batch manager linked list (Size = 1025 * 16 ~ 16K)}
  BatchManagers: packed array[0..MaxNumBatches] of TBatchManager;
  {Lookup table to convert the chunk index to a batch index
   (Size = 32768 * 2 = 64K)}
  ChunkIndexToBatchIndex: packed array[0..MaxNumChunks - 1] of word;
  {The array of small block memory managers. We assign a small block manager
   for each of the possible chunks. (Size = 32768 * 8 = 256K.)}
  ChunkManagers: packed array[0..MaxNumChunks - 1] of TChunkManager;
  {All the different small block types. Block sizes must be a multiple of 16.
   Block sizes below are hand-picked to ensure minimal memory wastage and a
   reasonable progression of block sizes.}
  SmallBlockTypes: packed array[0..NumSmallBlockTypes - 1] of TSmallBlockType =
  (
    (BlockSize: 16), (BlockSize: 32), (BlockSize: 48), (BlockSize: 64),
    (BlockSize: 80), (BlockSize: 96), (BlockSize: 112), (BlockSize: 128),
    (BlockSize: 144), (BlockSize: 160), (BlockSize: 176), (BlockSize: 192),
    (BlockSize: 208), (BlockSize: 224), (BlockSize: 240), (BlockSize: 256),
    (BlockSize: 272), (BlockSize: 304), (BlockSize: 336), (BlockSize: 368),
    (BlockSize: 400), (BlockSize: 432), (BlockSize: 464), (BlockSize: 496),
    (BlockSize: 544), (BlockSize: 592), (BlockSize: 640), (BlockSize: 704),
    (BlockSize: 768), (BlockSize: 816), (BlockSize: 880), (BlockSize: 976),
    (BlockSize: 1072), (BlockSize: 1168), (BlockSize: 1280), (BlockSize: 1392),
    (BlockSize: 1520), (BlockSize: 1632), (BlockSize: 1760), (BlockSize: 1920),
    (BlockSize: 2112), (BlockSize: 2976), (BlockSize: 5456), (BlockSize: 9360),
    (BlockSize: 13104), (BlockSize: 21840), (BlockSize: 32752),
    (BlockSize: MaximumSmallBlockSize)
  );
  {Lookup table to convert an allocation size to a block type index. Divide
   the allocation size by 16 to get an index into the table. Size = 2047 bytes}
  AllocationSizeToBlockType: packed array[0..MaximumSmallBlockSize div 16 - 1] of byte;
  {The old memory manager}
  OldMemoryManager: TMemoryManager;
  {The replacement memory manager}
  NewMemoryManager: TMemoryManager;
  {A string uniquely identifying the current process (for sharing memory managers)}
  UniqueProcessIDString: shortstring = '????????_PID_MemoryManager'#0;
  {The handle of the MM window}
  MMWindow: HWND;
  {Is the MM in place a shared memory manager?}
  OwnsMMWindow: boolean;

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
end;

{$IFDEF DEBUGMM}
{Locks the batch managers so only this thread can access them}
procedure LockBatchManagers;
asm
@LockLoop:
  xor al, al
  mov dl, 1
  {Attempt to lock the batches}
  lock cmpxchg BatchManagersLocked, dl
  jz @Done
  {Couldn't lock the batches - sleep and try again}
  push 0
  call sleep
  {Try again}
  jmp @LockLoop
@Done:
end;
{$ENDIF}

{$IFDEF DEBUGMM}
{Allocates a 64K chunk and returns its index. Returns 0 if out of memory.
 (Pascal version)}
function GetChunk: Cardinal;
var
  i, LBatchIndex: Cardinal;
begin
  {Lock the batch managers}
  LockBatchManagers;
  {Get the first batch manager}
  LBatchIndex := FirstBatchManagerIndex;
  {Is this batch manager valid, but there's no free chunks?}
  if (BatchManagers[LBatchIndex].UsageBitmap = 0)
    and (LBatchIndex > 0) then
  begin
    {Step through the list until we find a manager with available chunks}
    while True do
    begin
      {Get the next manager}
      LBatchIndex := BatchManagers[LBatchIndex].NextBatchIndex;
      {Was there a next manager?}
      if LBatchIndex = 0 then
        break;
      {Does this batch manager have an unused chunk?}
      if BatchManagers[LBatchIndex].UsageBitmap <> 0 then
      begin
        {This batch manager has space, but it is not the first manager in
         the chain - move it to the front of the linked list}
        BatchManagers[BatchManagers[LBatchIndex].PreviousBatchIndex].NextBatchIndex :=
          BatchManagers[LBatchIndex].NextBatchIndex;
        BatchManagers[BatchManagers[LBatchIndex].NextBatchIndex].PreviousBatchIndex :=
          BatchManagers[LBatchIndex].PreviousBatchIndex;
        BatchManagers[LBatchIndex].PreviousBatchIndex := 0;
        BatchManagers[LBatchIndex].NextBatchIndex := FirstBatchManagerIndex;
        BatchManagers[FirstBatchManagerIndex].PreviousBatchIndex := LBatchIndex;
        FirstBatchManagerIndex := LBatchIndex;
        {Break the loop}
        break;
      end;
    end;
  end;
  {Was a batch manager with an unused chunk found?}
  if LBatchIndex > 0 then
  begin
    {The batch manager has an unused block - find it and flag it as used}
    asm
      {Get a pointer to the usage bitmap}
      mov eax, LBatchIndex
      add eax, eax
      lea eax, [BatchManagers + eax * 8]
      {Find a set bit and put the index in ecx}
      bsf ecx, TBatchManager.UsageBitmap[eax]
      {Reset this bit}
      btr TBatchManager.UsageBitmap[eax], ecx
      {Get the first chunk number for this batch in eax}
      movzx eax, TBatchManager.FirstChunkIndex[eax]
      {Add the available bit number}
      add eax, ecx
      mov Result, eax
    end;
  end
  else
  begin
    {No available chunks - allocate a batch}
    Result := Cardinal(VirtualAlloc(nil, BatchSize, MEM_COMMIT or MEM_TOP_DOWN,
      PAGE_READWRITE));
    if Result <> 0 then
    begin
      {The batch manager index is the address divided by 2M + 1}
      LBatchIndex := Cardinal(Result) shr 21 + 1;
      {The chunk index is the address divided by 64K}
      Result := Result shr 16;
      {Update the chunk index to batch index translation table}
      for i := 0 to NumChunksPerBatch - 1 do
        ChunkIndexToBatchIndex[Result + i] := LBatchIndex;
      {Set up the batch manager (it is the new first batch manager)}
      BatchManagers[LBatchIndex].FirstChunkIndex := Result;
      BatchManagers[LBatchIndex].UsageBitmap := $fffffffe; //Only the first chunk is currently used
      BatchManagers[LBatchIndex].PreviousBatchIndex := 0;
      BatchManagers[LBatchIndex].NextBatchIndex := FirstBatchManagerIndex;
      BatchManagers[FirstBatchManagerIndex].PreviousBatchIndex := LBatchIndex;
      FirstBatchManagerIndex := LBatchIndex;
    end;
  end;
  {Batch managers are no longer locked}
  BatchManagersLocked := False;
end;
{$ELSE}
{Allocates a 64K chunk and returns its index. Returns 0 if out of memory.
 (asm version)}
function GetChunk: Cardinal;
asm
  {Save ebx and esi}
  push ebx
  push esi
  {esi = @BatchManagers}
  mov esi, offset BatchManagers
  {Lock the batch managers}
@LockLoop:
  xor al, al
  mov dl, 1
  {Attempt to lock the batches}
  lock cmpxchg BatchManagersLocked, dl
  jz @BatchesLocked
  {Couldn't lock the batches - sleep and try again}
  push 0
  call sleep
  {Try again}
  jmp @LockLoop
@BatchesLocked:
  {Get the first batch manager in ecx}
  movzx ecx, FirstBatchManagerIndex
  {Multiply by 16: ecx = BatchIndex * 16}
  shl ecx, 4
  jz @AllocateBatch
  {Get the usage bitmap in ecx}
  {Any free chunks in the first batch?}
  cmp TBatchManager.UsageBitmap[esi + ecx], 0
  jnz @FoundBatch
@SearchForBatchWithSpace:
  {Get the next batch manager in the chain}
  movzx ecx, TBatchManager.NextBatchIndex[esi + ecx]
  {End of the chain?}
  shl ecx, 4
  jz @AllocateBatch
  {Any free blocks?}
  cmp TBatchManager.UsageBitmap[esi + ecx], 0
  je @SearchForBatchWithSpace
  {This batch manager has space, but it is not the first manager in the chain}
  {Remove it from the linked list}
  movzx edx, TBatchManager.NextBatchIndex[esi + ecx]
  movzx eax, TBatchManager.PreviousBatchIndex[esi + ecx]
  lea ebx, [edx + edx]
  mov TBatchManager.PreviousBatchIndex[esi + ebx * 8], ax
  lea ebx, [eax + eax]
  mov TBatchManager.NextBatchIndex[esi + ebx * 8], dx
  {Insert it as the first manager}
  mov TBatchManager.PreviousBatchIndex[esi + ecx], 0
  movzx eax, FirstBatchManagerIndex
  mov TBatchManager.NextBatchIndex[esi + ecx], ax
  add eax, eax
  mov edx, ecx
  shr edx, 4
  mov TBatchManager.PreviousBatchIndex[esi + eax * 8], dx
  mov FirstBatchManagerIndex, dx
@FoundBatch:
  {Found a batch with space: ecx = batch index * 16}
  {Find a set bit and put the index in edx}
  bsf edx, TBatchManager.UsageBitmap[esi + ecx]
  {Reset this bit}
  btr TBatchManager.UsageBitmap[esi + ecx], edx
  {Get the first chunk number for this batch in eax}
  movzx eax, TBatchManager.FirstChunkIndex[esi + ecx]
  {Add the available bit number}
  add eax, edx
@GetChunkDone:
  {Unlock the batch managers}
  mov BatchManagersLocked, 0
  {Restore registers}
  pop esi
  pop ebx
  {Done}
  ret
  {We need to allocate a new batch}
@AllocateBatch:
  {Call virtualalloc to allocate a batch}
  push PAGE_READWRITE
  push MEM_COMMIT or MEM_TOP_DOWN
  push BatchSize
  push 0
  call VirtualAlloc
  {Out of memory?}
  test eax, eax
  jz @GetChunkDone
  {Get the batch index in ecx: The batch manager index is the address
   divided by 2M + 1}
  mov ecx, eax
  shr ecx, 21
  inc ecx
  {Convert the result into a chunk index}
  shr eax, 16
  {Point edx to ChunkIndexToBatchIndex[eax * 2]}
  lea edx, [ChunkIndexToBatchIndex + eax * 2]
  {Update the chunk index to batch index translation table: we need to store 32
   copies of cx starting at ChunkIndexToBatchIndex[eax * 2]}
  movd mm0, ecx
  punpcklwd mm0, mm0 //cx duplicated across low 32 bits
  punpckldq mm0, mm0 //cx duplicated across all 64 bits
  {Store}
  movq [edx], mm0
  movq [edx + 8], mm0
  movq [edx + 16], mm0
  movq [edx + 24], mm0
  movq [edx + 32], mm0
  movq [edx + 40], mm0
  movq [edx + 48], mm0
  movq [edx + 56], mm0
  {Exit mmx machine state}
  emms
  {Get the batch index in ebx}
  mov ebx, ecx
  {Convert ecx to batch manager index * 16}
  shl ecx, 4
  {Set up the batch manager (it is the new first batch manager)}
  movzx edx, FirstBatchManagerIndex
  mov FirstBatchManagerIndex, bx
  mov TBatchManager.FirstChunkIndex[esi + ecx], ax
  mov TBatchManager.UsageBitmap[esi + ecx], -2 //Only the first chunk is currently used
  mov TBatchManager.PreviousBatchIndex[esi + ecx], 0
  mov TBatchManager.NextBatchIndex[esi + ecx], dx
  add edx, edx
  mov TBatchManager.PreviousBatchIndex[esi + edx * 8], bx
  jmp @GetChunkDone
end;
{$ENDIF}

{$IFDEF DEBUGMM}
{Frees a 64K chunk (pascal version)}
procedure FreeChunk(AChunkIndex: Cardinal);
var
  LBatchIndex, LBitNumber: Cardinal;
begin
  {Lock the batch managers}
  LockBatchManagers;
  {Get the batch manager index for this chunk}
  LBatchIndex := ChunkIndexToBatchIndex[AChunkIndex];
  {Get the bit number from the chunk index}
  LBitNumber := AChunkIndex - BatchManagers[LBatchIndex].FirstChunkIndex;
  {Flag this chunk as unused}
  BatchManagers[LBatchIndex].UsageBitmap :=
    BatchManagers[LBatchIndex].UsageBitmap or (1 shl LBitNumber);
  {Are all chunks unused?}
  if BatchManagers[LBatchIndex].UsageBitmap = $ffffffff then
  begin
    {Remove this batch manager}
    BatchManagers[BatchManagers[LBatchIndex].NextBatchIndex].PreviousBatchIndex :=
      BatchManagers[LBatchIndex].PreviousBatchIndex;
    BatchManagers[BatchManagers[LBatchIndex].PreviousBatchIndex].NextBatchIndex :=
      BatchManagers[LBatchIndex].NextBatchIndex;
    if FirstBatchManagerIndex = LBatchIndex then
      FirstBatchManagerIndex := BatchManagers[LBatchIndex].NextBatchIndex;
    {Free the memory allocated by this batch}
    VirtualFree(Pointer(BatchManagers[LBatchIndex].FirstChunkIndex shl 16),
      0, MEM_RELEASE);
  end;
  {Batch managers are no longer locked}
  BatchManagersLocked := False;
end;
{$ELSE}
{Frees a 64K chunk (asm version)}
procedure FreeChunk(AChunkIndex: Cardinal);
asm
  {On entry: eax = AChunkIndex}
  {Save ebx and esi}
  push ebx
  push esi
  {ebx = chunk to free}
  mov ebx, eax
  {esi = @BatchManagers}
  mov esi, offset BatchManagers
  {Lock the batch managers}
@LockLoop:
  xor al, al
  mov dl, 1
  {Attempt to lock the batches}
  lock cmpxchg BatchManagersLocked, dl
  jz @BatchesLocked
  {Couldn't lock the batches - sleep and try again}
  push 0
  call sleep
  {Try again}
  jmp @LockLoop
@BatchesLocked:
  {Get the batch manager index for this chunk}
  movzx eax, word ptr [ChunkIndexToBatchIndex + ebx * 2]
  {Multiply by 16: eax = BatchIndex * 16}
  shl eax, 4
  {Get the bit number from the chunk index}
  mov ecx, ebx
  sub cx, TBatchManager.FirstChunkIndex[esi + eax]
  {Update the usage bitmap}
  mov edx, 1
  shl edx, cl
  mov ecx, TBatchManager.UsageBitmap[esi + eax]
  or ecx, edx
  mov TBatchManager.UsageBitmap[esi + eax], ecx
  {Is the batch completely empty now?}
  cmp ecx, -1
  jne @DontFreeBatch
  {Remove this batch manager}
  movzx edx, TBatchManager.NextBatchIndex[esi + eax]
  movzx ecx, TBatchManager.PreviousBatchIndex[esi + eax]
  lea ebx, [edx + edx]
  mov TBatchManager.PreviousBatchIndex[esi + ebx * 8], cx
  lea ebx, [ecx + ecx]
  mov TBatchManager.NextBatchIndex[esi + ebx * 8], dx
  {Was it the first batch manager?}
  mov ecx, eax
  shr ecx, 4
  cmp cx, FirstBatchManagerIndex
  jne @WasNotFirstManager
  mov FirstBatchManagerIndex, dx
@WasNotFirstManager:
  {Get the first chunk index}
  movzx ecx, TBatchManager.FirstChunkIndex[esi + eax]
  {Convert to a pointer}
  shl ecx, 16
  {Free the block}
  push MEM_RELEASE
  push 0
  push ecx
  call VirtualFree
@DontFreeBatch:
  {Unlock the batch managers}
  mov BatchManagersLocked, 0
  {Restore ebx and esi}
  pop esi
  pop ebx
  {Done}
  ret
end;
{$ENDIF}

{Locks the given block type so only this thread can access it}
procedure LockBlockType(ABlockTypeIndex: Cardinal);
asm
  {On entry: eax = Block Type Index}
  {ecx = Block Type Index * 8}
  lea ecx, [eax * 8]
@LockLoop:
  xor al, al
  mov dl, 1
  {Attempt to grab the block type}
  lock cmpxchg TSmallBlockType.BlockTypeLocked[SmallBlockTypes + ecx + ecx], dl
  jz @Done
  {Couldn't grab the block type - sleep and try again}
  push ecx
  push 0
  call sleep
  pop ecx
  {Try again}
  jmp @LockLoop
@Done:
end;

{$IFDEF DEBUGMM}
{Replacement for SysGetMem (pascal version)}
function FastGetMem(ASize: Integer): Pointer;
var
  LSmallBlockTypeIndex, LChunkIndex, LBlockNumber, LAllocSize: Cardinal;
  LBlockStack: PWord;
begin
  if Cardinal(ASize) <= MaximumSmallBlockSize then
  begin
    {Get the block type index}
    LSmallBlockTypeIndex := AllocationSizeToBlockType[(ASize - 1) shr 4];
    {Lock this block type for multithreaded apps}
    LockBlockType(LSmallBlockTypeIndex);
    {Get the first manager for this block type}
    LChunkIndex := SmallBlockTypes[LSmallBlockTypeIndex].FirstManagerIndex;
    {Is this manager valid, but there's no free blocks?}
    if (ChunkManagers[LChunkIndex].FreeBlockStackPointer = 0)
      and (LChunkIndex > 0) then
    begin
      {Step through the list until we find a manager with available blocks}
      while True do
      begin
        {Get the next manager}
        LChunkIndex := ChunkManagers[LChunkIndex].NextChunkIndex;
        {Was there a next manager?}
        if LChunkIndex = 0 then
          break;
        {Does this chunk manager have space?}
        if ChunkManagers[LChunkIndex].FreeBlockStackPointer <> 0 then
        begin
          {This chunk manager has space, but it is not the first manager in
           the chain - move it to the front of the linked list for this block
           type}
          ChunkManagers[ChunkManagers[LChunkIndex].PreviousChunkIndex].NextChunkIndex := ChunkManagers[LChunkIndex].NextChunkIndex;
          ChunkManagers[ChunkManagers[LChunkIndex].NextChunkIndex].PreviousChunkIndex := ChunkManagers[LChunkIndex].PreviousChunkIndex;
          ChunkManagers[LChunkIndex].PreviousChunkIndex := 0;
          ChunkManagers[LChunkIndex].NextChunkIndex := SmallBlockTypes[LSmallBlockTypeIndex].FirstManagerIndex;
          ChunkManagers[SmallBlockTypes[LSmallBlockTypeIndex].FirstManagerIndex].PreviousChunkIndex := LChunkIndex;
          SmallBlockTypes[LSmallBlockTypeIndex].FirstManagerIndex := LChunkIndex;
          {Break the loop}
          break;
        end;
      end;
    end;
    {Was a manager with a free block found?}
    if LChunkIndex <> 0 then
    begin
      {Get the base address of this block}
      Result := Pointer(LChunkIndex shl 16);
      {Found a chunk with an available block: Get the available block number}
      LBlockNumber := PWord(Cardinal(Result)
        + ChunkManagers[LChunkIndex].FreeBlockStackPointer)^;
      {Adjust the offset for the block number}
      Result := Pointer(Cardinal(Result) + LBlockNumber * SmallBlockTypes[LSmallBlockTypeIndex].BlockSize);
      {Decrement the number of available blocks in this chunk}
      Inc(ChunkManagers[LChunkIndex].FreeBlockStackPointer, 2);
    end
    else
    begin
      {Try to allocate a chunk}
      LChunkIndex := GetChunk;
      if LChunkIndex > 0 then
      begin
        {Set up this manager}
        ChunkManagers[LChunkIndex].BlockTypeIndex := LSmallBlockTypeIndex;
        ChunkManagers[LChunkIndex].PreviousChunkIndex := 0;
        ChunkManagers[LChunkIndex].NextChunkIndex := SmallBlockTypes[LSmallBlockTypeIndex].FirstManagerIndex;
        {Point to block 1, since block 0 is already used}
        ChunkManagers[LChunkIndex].FreeBlockStackPointer := SmallBlockTypes[LSmallBlockTypeIndex].InitialStackPointer;
        {Set up the old first manager}
        ChunkManagers[SmallBlockTypes[LSmallBlockTypeIndex].FirstManagerIndex].PreviousChunkIndex := LChunkIndex;
        {Make this the first manager for this block type}
        SmallBlockTypes[LSmallBlockTypeIndex].FirstManagerIndex := LChunkIndex;
        {Set the result pointer}
        Result := Pointer(LChunkIndex shl 16);
        {Configure the available blocks stack}
        LBlockNumber := 1;
        LBlockStack := PWord(Cardinal(Result) + SmallBlockTypes[LSmallBlockTypeIndex].InitialStackPointer);
        repeat
          LBlockStack^ := LBlockNumber;
          Inc(LBlockStack);
          Inc(LBlockNumber);
        until smallint(LBlockStack) >= 0;
      end
      else
      begin
        {Out of memory}
        Result := nil;
      end;
    end;
    {Unlock this block type}
    SmallBlockTypes[LSmallBlockTypeIndex].BlockTypeLocked := False;
  end
  else
  begin
    {Larger block: Round up to the next 32K boundary}
    LAllocSize := (ASize + $00007fff) and $ffff8000;
    {Allocate directly through VirtualAlloc}
    Result := VirtualAlloc(nil, LAllocSize, MEM_COMMIT, PAGE_READWRITE);
    if Result <> nil then
    begin
      {The manager index is the address divided by 64K}
      LChunkIndex := Cardinal(Result) shr 16;
      {Flag the block type as a large block}
      ChunkManagers[LChunkIndex].LargeBlockID := LargeBlockTypeIndex;
      ChunkManagers[LChunkIndex].Num32KBlocksAllocated := LAllocSize shr 15;
      ChunkManagers[LChunkIndex].UserAllocatedSize := ASize;
    end;
  end;
end;
{$ELSE}
{Replacement for SysGetMem (asm version)}
function FastGetMem(ASize: Integer): Pointer;
const
  {Initial stack values}
  InitialStackValues: array[0..6] of word =
    ($fffe, $ffff, $0000, $0001, $0002, $0003, $0004);
  {The stack initialization add number}
  StackInitAdd: Int64 = $0004000400040004;
asm
  {On entry: eax = requested size}
  {Save registers}
  push ebx
  push esi
  push edi
  {Point esi to ChunkManagers}
  mov esi, offset ChunkManagers
  {Is it a small or large block?}
  cmp eax, MaximumSmallBlockSize
  jg @LargeBlock
  {Point edi to SmallBlockTypes}
  mov edi, offset SmallBlockTypes
  {Subtract 1 byte and divide the requested size by 16 to get the block size
   lookup number}
  dec eax
  shr eax, 4
  {Get the block type in ebx}
  movzx ebx, byte ptr AllocationSizeToBlockType[eax]
  {ebx = block type * 16}
  shl ebx, 4
  {Lock the block type}
@LockBlockTypeLoop:
  xor al, al
  mov cl, 1
  {Attempt to grab the block type}
  lock cmpxchg TSmallBlockType.BlockTypeLocked[edi + ebx], cl
  je @GotLockOnBlockType
  {Couldn't grab the block type - sleep and try again}
  push 0
  call sleep
  jmp @LockBlockTypeLoop
@GotLockOnBlockType:
  {Get the first chunk index in ecx}
  movzx ecx, TSmallBlockType.FirstManagerIndex[edi + ebx]
  {Is there a first chunk manager?}
  test ecx, ecx
  jz @AllocateChunk
  {Any free blocks in the first manager?}
  cmp TChunkManager.FreeBlockStackPointer[esi + ecx * 8], 0
  jnz @FoundChunk
@SearchForChunkWithSpace:
  {Get the next manager in the chain}
  movzx ecx, TChunkManager.NextChunkIndex[esi + ecx * 8]
  {End of the chain?}
  test ecx, ecx
  jz @AllocateChunk
  {Any free blocks?}
  cmp TChunkManager.FreeBlockStackPointer[esi + ecx * 8], 0
  je @SearchForChunkWithSpace
  {This chunk manager has space, but it is not the first manager in the chain
   - move it to the front of the linked list for this block type}
  {Remove it from the linked list}
  movzx edx, TChunkManager.NextChunkIndex[esi + ecx * 8]
  movzx eax, TChunkManager.PreviousChunkIndex[esi + ecx * 8]
  mov TChunkManager.PreviousChunkIndex[esi + ecx * 8], 0
  mov TChunkManager.PreviousChunkIndex[esi + edx * 8], ax
  mov TChunkManager.NextChunkIndex[esi + eax * 8], dx
  {Insert it as the first manager}
  movzx eax, TSmallBlockType.FirstManagerIndex[edi + ebx]
  mov TChunkManager.NextChunkIndex[esi + ecx * 8], ax
  mov TChunkManager.PreviousChunkIndex[esi + eax * 8], cx
  mov TSmallBlockType.FirstManagerIndex[edi + ebx], cx
  {Has space - allocate a block}
@FoundChunk:
  {Get the free block stack pointer for this chunk in edx and add 2}
  mov edx, 2
  xadd TChunkManager.FreeBlockStackPointer[esi + ecx * 8], dx
  {Chunk number in ecx, stack pointer in edx, block type * 16 in ebx}
  mov eax, ecx
  shl eax, 16 // eax = Chunk Index * 64K
  {Get the block number in edx}
  movzx edx, word ptr [eax + edx] // = Chunk Index * 64K + Stack Pointer
  {Multiply the block number with the block size}
  movzx ecx, TSmallBlockType.BlockSize[edi + ebx]
  imul edx, ecx
  {Add to the chunk offset}
  add eax, edx
@AllocateDone:
  {Unlock the block type}
  mov TSmallBlockType.BlockTypeLocked[edi + ebx], 0
  {Restore registers}
  pop edi
  pop esi
  pop ebx
  {Done}
  ret
  {We need to allocate a new chunk}
@AllocateChunk:
  {ebx = pointer to block type}
  {Get a chunk}
  call GetChunk
  {Out of memory?}
  test eax, eax
  jz @AllocateDone
  {Get the small block type index in ecx}
  mov ecx, ebx
  shr ecx, 4
  {Set the chunk manager data}
  mov TChunkManager.BlockTypeIndex[esi + eax * 8], cl //block type index
  movzx ecx, TSmallBlockType.InitialStackPointer[edi + ebx]
  mov TChunkManager.FreeBlockStackPointer[esi + eax * 8], cx //initial stack pointer
  {Insert it as the first manager}
  mov TChunkManager.PreviousChunkIndex[esi + eax * 8], 0
  movzx edx, TSmallBlockType.FirstManagerIndex[edi + ebx]
  mov TChunkManager.NextChunkIndex[esi + eax * 8], dx
  mov TChunkManager.PreviousChunkIndex[esi + edx * 8], ax
  mov TSmallBlockType.FirstManagerIndex[edi + ebx], ax
  {Set the result pointer}
  shl eax, 16 // = chunk * 64K
  {8-byte align the storing of block numbers}
  mov edx, offset InitialStackValues + 6
  sub edx, ecx
  and ecx, -8
  add edx, ecx
  {Get the initial value to store}
  movq mm0, [edx]
@BuildStackLoop:
  movq [eax + ecx], mm0
  paddw mm0, StackInitAdd
  add cx, 8
  {Initial cx may be 0 for the largest block size, so we check the sign of cx}
  js @BuildStackLoop
  {Exit mmx state}
  emms
  jmp @AllocateDone
@LargeBlock:
  {It is a large block}
  {Save the requested size in edi}
  mov edi, eax
  {Get the size to allocate in ebx: Granularity = 32K}
  lea ebx, [eax + $7fff]
  and ebx, $ffff8000
  {Call VirtualAlloc directly}
  push PAGE_READWRITE
  push MEM_COMMIT
  push ebx
  push 0
  call VirtualAlloc
  {Out of memory?}
  test eax, eax
  jz @LargeAllocDone
  {Convert to a chunk index * 8}
  mov edx, eax
  shr edx, 13
  shr ebx, 15
  mov TChunkManager.LargeBlockID[esi + edx], LargeBlockTypeIndex
  mov TChunkManager.Num32KBlocksAllocated[esi + edx], bx
  mov TChunkManager.UserAllocatedSize[esi + edx], edi
@LargeAllocDone:
  pop edi
  pop esi
  pop ebx
end;
{$ENDIF}

{$IFDEF DEBUGMM}
{Replacement for SysFreeMem (pascal version)}
function FastFreeMem(APointer: Pointer): Integer;
var
  LSmallBlockTypeIndex, LChunkIndex, LBlockNumber: Cardinal;
begin
  {The manager index is the address divided by 64K}
  LChunkIndex := Cardinal(APointer) shr 16;
  {Get the block type index}
  LSmallBlockTypeIndex := ChunkManagers[LChunkIndex].BlockTypeIndex;
  {Is it a large or small block chunk?}
  if LSmallBlockTypeIndex <> LargeBlockTypeIndex then
  begin
    {Small Block}
    {Default return value = no error}
    Result := 0;
    {Lock this block type for multithreaded apps}
    LockBlockType(LSmallBlockTypeIndex);
    {Will the entire chunk be empty after this free?}
    if ChunkManagers[LChunkIndex].FreeBlockStackPointer <> SmallBlockTypes[LSmallBlockTypeIndex].InitialStackPointer then
    begin
      {Determine the block number}
      LBlockNumber := (Cardinal(APointer) and $ffff) div SmallBlockTypes[LSmallBlockTypeIndex].BlockSize;
      {Decrement the stack pointer}
      Dec(ChunkManagers[LChunkIndex].FreeBlockStackPointer, 2);
      {Add this block to the available block stack}
      PWord((Cardinal(APointer) and $ffff0000) + ChunkManagers[LChunkIndex].FreeBlockStackPointer)^ := LBlockNumber;
    end
    else
    begin
      {Remove this manager}
      ChunkManagers[ChunkManagers[LChunkIndex].NextChunkIndex].PreviousChunkIndex :=
        ChunkManagers[LChunkIndex].PreviousChunkIndex;
      ChunkManagers[ChunkManagers[LChunkIndex].PreviousChunkIndex].NextChunkIndex :=
        ChunkManagers[LChunkIndex].NextChunkIndex;
      {Was this the first manager? If so, set the next manager as the first one.}
      if SmallBlockTypes[LSmallBlockTypeIndex].FirstManagerIndex = LChunkIndex then
        SmallBlockTypes[LSmallBlockTypeIndex].FirstManagerIndex := ChunkManagers[LChunkIndex].NextChunkIndex;
      {Release this chunk}
      FreeChunk(LChunkIndex);
    end;
    {Unlock this block type}
    SmallBlockTypes[LSmallBlockTypeIndex].BlockTypeLocked := False;
  end
  else
  begin
    {Larger block: Remove the large block type flag}
    ChunkManagers[LChunkIndex].LargeBlockID := 0;
    {Free the block through VirtualFree}
    if VirtualFree(APointer, 0, MEM_RELEASE) then
      Result := 0
    else
      Result := -1;
  end;
end;
{$ELSE}
{Replacement for SysFreeMem (asm version)}
function FastFreeMem(APointer: Pointer): Integer;
asm
  {On entry: eax = pointer}
  {Set ecx = chunk index}
  mov ecx, eax
  shr ecx, 16
  {Save edi and ebx}
  push edi
  push ebx
  {edi points to ChunkManagers}
  mov edi, offset ChunkManagers
  {ebx = block type}
  movzx ebx, TChunkManager.BlockTypeIndex[edi + ecx * 8]
  cmp ebx, LargeBlockTypeIndex
  je @LargeBlock
  {Set ebx = block type * 16}
  shl ebx, 4
  {Save esi and ebp}
  push esi
  push ebp
  {Set esi = APointer}
  mov esi, eax
  {Set ebp = offset SmallBlockTypes}
  mov ebp, offset SmallBlockTypes
  {Lock the block type}
@LockBlockTypeLoop:
  xor al, al
  mov dl, 1
  {Attempt to grab the block type}
  lock cmpxchg TSmallBlockType.BlockTypeLocked[ebp + ebx], dl
  je @GotLockOnBlockType
  {Couldn't grab the block type - sleep and try again}
  push ecx
  push 0
  call sleep
  pop ecx
  jmp @LockBlockTypeLoop
@GotLockOnBlockType:
  {Register status: ecx = chunk index, ebx = block type * 16, esi = APointer}
  {Determine the block number}
  movzx eax, si //eax = low word of pointer
  {Get the current stack pointer (esi points to stack entry)}
  mov si, TChunkManager.FreeBlockStackPointer[edi + ecx * 8]
  {Is the entire chunk now empty?}
  cmp si, TSmallBlockType.InitialStackPointer[ebp + ebx]
  je @ChunkEmpty
  {Divide by the block size through multiplying with inverse}
  mul TSmallBlockType.BlockSizeInverse[ebp + ebx] //block number in edx
  {Increment the free block count}
  sub si, 2
  {Store the new free block count}
  mov TChunkManager.FreeBlockStackPointer[edi + ecx * 8], si
  {Store the freed block number on the stack}
  mov [esi], dx
@SmallFreeDone:
  {Unlock the block type}
  mov TSmallBlockType.BlockTypeLocked[ebp + ebx], 0
  {Restore registers}
  pop ebp
  pop esi
  pop ebx
  pop edi
  {All OK}
  xor eax, eax
  {Done}
  ret
@ChunkEmpty:
  {Chunk is empty: Remove this manager}
  movzx esi, TChunkManager.NextChunkIndex[edi + ecx * 8]
  movzx edx, TChunkManager.PreviousChunkIndex[edi + ecx * 8]
  mov TChunkManager.PreviousChunkIndex[edi + esi * 8], dx
  mov TChunkManager.NextChunkIndex[edi + edx * 8], si
  {Was this the first manager? If so, set the next manager as the first one.}
  movzx edx, TSmallBlockType.FirstManagerIndex[ebp + ebx]
  cmp edx, ecx
  jne @NotFirstManager
  mov TSmallBlockType.FirstManagerIndex[ebp + ebx], si
@NotFirstManager:
  {Release this chunk}
  mov eax, ecx
  call FreeChunk
  jmp @SmallFreeDone
@LargeBlock:
  {Reset the block type - not a large block anymore}
  mov TChunkManager.BlockTypeIndex[edi + ecx * 8], 0
  {Larger block: Free the block through VirtualFree}
  push MEM_RELEASE
  push 0
  push eax
  call VirtualFree
  {VirtualFree returns >0 if all is ok}
  cmp eax, 1
  {Return 0 on all ok}
  sbb eax, eax
  {Restore registers}
  pop ebx
  pop edi
end;
{$ENDIF}

{$IFDEF DEBUGMM}
{Replacement for SysReallocMem (pascal version)}
function FastReallocMem(APointer: Pointer; ASize: Integer): Pointer;
var
  LCurrentBlockSize, LUserAllocatedSize, LNewUserAllocatedSize: integer;
  LSmallBlockTypeIndex, LChunkIndex, LMoveSize: Cardinal;
begin
  {The manager index is the address divided by 64K}
  LChunkIndex := Cardinal(APointer) shr 16;
  {Get the block type index}
  LSmallBlockTypeIndex := ChunkManagers[LChunkIndex].BlockTypeIndex;
  {Is it a large or small block chunk?}
  if LSmallBlockTypeIndex <> LargeBlockTypeIndex then
  begin
    {Small Block: Get the current block size}
    LCurrentBlockSize := SmallBlockTypes[LSmallBlockTypeIndex].BlockSize;
    {We don't track the allocated size for small blocks - assume its the same
     as the block size.}
    LUserAllocatedSize := LCurrentBlockSize;
  end
  else
  begin
    {It's a large block chunk: Get the current block size}
    LCurrentBlockSize := ChunkManagers[LChunkIndex].Num32KBlocksAllocated shl 15;
    {Get the size allocated by the user}
    LUserAllocatedSize := ChunkManagers[LChunkIndex].UserAllocatedSize;
  end;
  {Do we need to do a physical reallocation? We only reallocate if the new
   size is larger than the current block size, or if the new size is less
   than a quarter of the block size (and it's not already the smallest
   available block size.)}
  if (ASize <= LCurrentBlockSize)
    and ((ASize >= (LCurrentBlockSize shr 2))
      or (LCurrentBlockSize < MinimumBlockSizeToAllowDownsizing)) then
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
    {Set the correct user size in the manager}
    LChunkIndex := Cardinal(Result) shr 16;
    ChunkManagers[LChunkIndex].UserAllocatedSize := ASize;
  end;
end;
{$ELSE}
{Replacement for SysReallocMem (asm version)}
function FastReallocMem(APointer: Pointer; ASize: Integer): Pointer;
asm
  {On entry:
    eax = pointer
    edx = requested size}
  {Save registers}
  push ebx
  {Get the chunk manager index in ebx}
  mov ebx, eax
  shr ebx, 16
  {Get the block type index in ecx}
  movzx ecx, TChunkManager.BlockTypeIndex[ChunkManagers + ebx * 8]
  {Is it a large block?}
  cmp ecx, LargeBlockTypeIndex
  je @LargeBlock
  {Small block - get current size in ecx}
  shl ecx, 4
  movzx ecx, TSmallBlockType.BlockSize[SmallBlockTypes + ecx]
  {The user allocated size is the same for small blocks}
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
  {The pointer remains the same, but if this is a large block, we must update
  the user size}
  cmp ecx, MaximumSmallBlockSize
  jng @NoReallocDone
  {Set the correct user size in the manager}
  mov ebx, eax
  shr ebx, 16
  mov TChunkManager.UserAllocatedSize[ChunkManagers + ebx * 8], edx
@NoReallocDone:
  pop ebx
  ret
@LargeBlock:
  {Large block - get the number of allocated chunks in ecx}
  movzx ecx, TChunkManager.Num32KBlocksAllocated[ChunkManagers + ebx * 8]
  {Convert to bytes}
  shl ecx, 15
  {Get the user allocated size}
  mov ebx, TChunkManager.UserAllocatedSize[ChunkManagers + ebx * 8]
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
  shr ebx, 16
  mov TChunkManager.UserAllocatedSize[ChunkManagers + ebx * 8], edi
@ReallocDone:
  pop ebp
  pop edi
  pop esi
  pop ebx
end;
{$ENDIF}

{Initializes the small block type structure}
procedure InstallMemoryManager;
var
  LPreviousBlockSize, LBlockTypeIndex, LAllocInd: word;
  i, LCurrentProcessID, LBlocksPerChunk: Cardinal;
{$ifdef DEBUGMM}
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
    for LBlockTypeIndex := 0 to high(SmallBlockTypes) do
    begin
      {Get the number of blocks per chunk. The block stack is allocated in the
       same chunk as the blocks themselves, so there is a two byte overhead per
       block}
      LBlocksPerChunk := 65536 div (SmallBlockTypes[LBlockTypeIndex].BlockSize + 2);
      {Update the block size lookup table}
      for LAllocInd := (LPreviousBlockSize shr 4) to (SmallBlockTypes[LBlockTypeIndex].BlockSize shr 4) - 1 do
        AllocationSizeToBlockType[LAllocInd] := LBlockTypeIndex;
      {Update previous block size}
      LPreviousBlockSize := SmallBlockTypes[LBlockTypeIndex].BlockSize;
      {Set the reciprocal block size scaled by 2^32. Scaling by 2^32 gives us at
       least 5 digits accuracy when stored as a cardinal, which is sufficient for
       this case since the maximum multiplier is 5 digits.}
      SmallBlockTypes[LBlockTypeIndex].BlockSizeInverse :=
        ($100000000 + LPreviousBlockSize - 1) div LPreviousBlockSize;
      {Calculate the initial stack pointer for newly allocated chunks. The
       initial pointer points to block 1 (not 0), since block 0 is
       automatically used when a new chunk is allocated}
      SmallBlockTypes[LBlockTypeIndex].InitialStackPointer :=
        word(65538 - LBlocksPerChunk * 2);
{$ifdef DEBUGMM}
      {Do a sanity check}
      for j := 0 to LBlocksPerChunk - 1 do
      begin
        blkoff := j * SmallBlockTypes[LBlockTypeIndex].BlockSize;
        asm
          mov eax, blkoff
          movzx edx, LBlockTypeIndex
          add edx, edx
          mov edx, TSmallBlockType.BlockSizeInverse[SmallBlockTypes + edx * 8]
          mul edx
          mov k, edx
        end;
        if k <> j then
          System.Error(reInvalidPtr);
      end;
{$endif}
    end;
{$ifdef DEBUGMM}
    {Check the block size lookup table for validity}
    for k := 1 to 65520 do
    begin
      LBlockTypeIndex := AllocationSizeToBlockType[(k - 1) shr 4];
      if (SmallBlockTypes[LBlockTypeIndex].BlockSize < k) then
        System.Error(reInvalidPtr);
      if (LBlockTypeIndex > 0) and (SmallBlockTypes[LBlockTypeIndex - 1].BlockSize >= k) then
        System.Error(reInvalidPtr);
    end;
{$endif}
    {Flag all the chunks as used in batch manager 0 (dummy manager)}
    BatchManagers[0].UsageBitmap := 0;
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

{Returns a snapshot of the current memory usage by this process.}
function GetUsageSnapshot: TPSDMemoryManagerUsageSnapshot;
var
  LBatchManagerIndex, LChunkIndex, LChunkManagerIndex, LAvailableBlockCount,
    LUsedBlockCount: Word;
  LSmallBlockTypeIndex: byte;
  LChunkInUse: boolean;
  i, j: integer;
  LUserSize: Cardinal;
  LMBI: TMemoryBasicInformation;
begin
  {Clear the result structure}
  FillChar(Result, SizeOf(Result), 0);
  {Lock all the small block types}
  for i := 0 to NumSmallBlockTypes - 1 do
  begin
    LockBlockType(i);
    Result.SmallBlockInfo[i].BlockSize := SmallBlockTypes[i].BlockSize;
  end;
  {Get all the allocated chunks: small blocks first}
  LBatchManagerIndex := FirstBatchManagerIndex;
  while LBatchManagerIndex > 0 do
  begin
    {Increment the number of batches allocated}
    Inc(Result.ChunkBatchesAllocated);
    {Add to the total virtual allocated space}
    Inc(Result.TotalVirtualAllocated, ChunkSize * NumChunksPerBatch);
    {Step through all the chunks for the batch}
    for LChunkIndex := 0 to NumChunksPerBatch - 1 do
    begin
      {Get the absolute chunk index}
      LChunkManagerIndex := BatchManagers[LBatchManagerIndex].FirstChunkIndex + LChunkIndex;
      {Is this chunk in use or not?}
      LChunkInUse := (BatchManagers[LBatchManagerIndex].UsageBitmap and (1 shl LChunkIndex)) = 0;
      if LChunkInUse then
      begin
        Inc(Result.ChunksUsed);
        {In use by a small block type}
        Result.ChunkInfo[LChunkManagerIndex].ChunkStatus := csAllocated;
        {Get the allocation details for the chunk}
        LSmallBlockTypeIndex := ChunkManagers[LChunkManagerIndex].BlockTypeIndex;
        LUsedBlockCount := Word(SmallInt(ChunkManagers[LChunkManagerIndex].FreeBlockStackPointer)
          - SmallInt(SmallBlockTypes[LSmallBlockTypeIndex].InitialStackPointer) + 2) shr 1;
        LAvailableBlockCount := Word(65538 - Integer(SmallBlockTypes[LSmallBlockTypeIndex].InitialStackPointer)) shr 1;
        {Add to the totals}
        Inc(Result.TotalUserAllocated, LUsedBlockCount * SmallBlockTypes[LSmallBlockTypeIndex].BlockSize);
        {Set the user allocated size for the chunk}
        Result.ChunkInfo[LChunkManagerIndex].UserAllocatedSize := LUsedBlockCount * SmallBlockTypes[LSmallBlockTypeIndex].BlockSize;
        Result.ChunkInfo[LChunkManagerIndex].BlockTypeIndex := LSmallBlockTypeIndex;
        {Add to the small block details}
        Inc(Result.SmallBlockInfo[LSmallBlockTypeIndex].AvailableBlockCount, LAvailableBlockCount);
        Inc(Result.SmallBlockInfo[LSmallBlockTypeIndex].UserAllocatedBlocks, LUsedBlockCount);
        Inc(Result.SmallBlockInfo[LSmallBlockTypeIndex].ChunksAllocated);
      end
      else
      begin
        {Reserved for small block types}
        Result.ChunkInfo[LChunkManagerIndex].ChunkStatus := csReserved;
      end;
    end;
    {Get the next batch manager}
    LBatchManagerIndex := BatchManagers[LBatchManagerIndex].NextBatchIndex;
  end;
  {Get all the allocated large blocks}
  for i := 0 to MaxNumChunks - 1 do
  begin
    {Is this chunk a large block chunk?}
    if ChunkManagers[i].LargeBlockID = LargeBlockTypeIndex then
    begin
      LUserSize := ChunkManagers[i].UserAllocatedSize;
      Inc(Result.TotalVirtualAllocated, ChunkManagers[i].Num32KBlocksAllocated * 32 * 1024);
      Inc(Result.TotalUserAllocated, LUserSize);
      for j := 0 to ChunkManagers[i].Num32KBlocksAllocated do
      begin
        LChunkIndex := i + (j shr 1);
        Result.ChunkInfo[LChunkIndex].ChunkStatus := csAllocated;
        Result.ChunkInfo[LChunkIndex].BlockTypeIndex := LargeBlockTypeIndex;
        if LUserSize >= 32768 then
        begin
          Inc(Result.ChunkInfo[LChunkIndex].UserAllocatedSize, 32768);
          Dec(LUserSize, 32768);
        end
        else
        begin
          Inc(Result.ChunkInfo[LChunkIndex].UserAllocatedSize, LUserSize);
          LUserSize := 0;
        end;
      end;
    end
    else
    begin
      {If the chunk is not allocated by this MM, what is its status?}
      if Result.ChunkInfo[i].ChunkStatus = csUnallocated then
      begin
        {Get all the reserved memory blocks and windows allocated memory blocks, etc.}
        VirtualQuery(Pointer(i * ChunkSize), LMBI, SizeOf(LMBI));
        if LMBI.State = MEM_COMMIT then
          Result.ChunkInfo[i].ChunkStatus := csSysAllocated
        else
          if LMBI.State = MEM_RESERVE then
            Result.ChunkInfo[i].ChunkStatus := csSysReserved;
      end;
    end;
  end;
  {Unlock all the small block types}
  for i := 0 to NumSmallBlockTypes - 1 do
    SmallBlockTypes[i].BlockTypeLocked := False;
end;

initialization
  {Has the Borland MM been used?}
  if GetHeapStatus.TotalAllocated <> 0 then
    System.Error(reInvalidPtr);
  {Install the memory manager}
  InstallMemoryManager;

finalization
  {Restore the old memory manager}
  UninstallMemoryManager;

end.
