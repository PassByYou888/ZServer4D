unit smmSmallMemory;

interface

{$Include smmOptions.inc}

uses
  smmTypes, smmStatistics;

type
  PSmallMemHeader        = ^TSmallMemHeader;
  PSmallMemBlock         = ^TSmallMemBlock;
  PSmallMemBlockList     = ^TSmallMemBlockList;
  PSmallMemThreadManager = ^TSmallMemThreadManager;
  PSmallMemHeaderFree    = ^TSmallMemHeaderFree;

//? {$A-} { all object/record must be packed }

  /// Header appended to the beginning of every allocated memory block
  TSmallMemHeader = object
    {$IFDEF SCALEMM_MAGICTEST}
    Magic1: NativeInt;
    Magic2: NativeInt;
    {$ELSE}
      {$IFDEF Align16Bytes}
        {$ifndef CPUX64}
        Filer1: Pointer;  // 16 bytes aligned for 32 bit compiler
        Filer2: Pointer;
        {$endif}
      {$ENDIF}
    {$ENDIF}

    Size: NativeUInt;
    /// the memory block handler which owns this memory block
    /// must be last item of header (same negative offset from mem as TBaseMemHeader)
    OwnerBlock : PSmallMemBlock;

    procedure CheckMem;
  end;

  TSmallMemHeaderFree = object
    {$IFDEF SCALEMM_MAGICTEST}
    Magic1: NativeInt;
    Magic2: NativeInt;
    {$ELSE}
      {$IFDEF Align16Bytes}
        {$ifndef CPUX64}
        Filer1: Pointer;  // 16 bytes aligned for 32 bit compiler
        Filer2: Pointer;
        {$endif}
      {$ENDIF}
    {$ENDIF}

    Size: NativeUInt;
    /// the memory block handler which owns this memory block
    OwnerBlock: PSmallMemBlock;

    //Extra data of free item:---------------------------------
    /// linked to next single memory item (other thread freem mem)
    NextFreeItem : PSmallMemHeaderFree;
  end;

  TSplitRecord = record
    case Byte of
      1: (CompleteRef: NativeInt);
      2: (DummyRef: Int16;
          Counter : Int16;)
  end;

  /// memory block handler
  TSmallMemBlock = object
    /// Thread owner, must be first item of block (same offset as PBaseBlockMemory)
    OwnerManager: PSmallMemThreadManager;
    /// the memory block list which owns this memory block handler
    OwnerList: PSmallMemBlockList;

    FNextThreadFreedBlock: PSmallMemBlock;
    {$IFDEF Align8Bytes}
      {$ifndef CPUX64}
      Filer1: Pointer;  // 8 bytes aligned   -> 12 + 4 = 16 (32bit)
      {$endif}
    {$ENDIF}
    {$IFDEF Align16Bytes}
      Filer1: Pointer;  // 16 bytes aligned  -> 12 + 4 = 16 (32bit)
                                              //24 + 8 = 32 (64bit)
    {$ENDIF}
    FFirstThreadFreed: PSmallMemHeaderFree;
    //FThreadFreedCount: Integer;   //negative = lock
    FLockReference: TSplitRecord;   //ABA threading lock

    /// how much mem is used, max is C_ARRAYSIZE
    FUsageCount: NativeUInt;
    /// how much mem is free
    FFreedIndex: NativeUInt;
    /// internal storage of the memory blocks
    // - will contain array[0..C_ARRAYSIZE-1] of memory items,
    // i.e. (FItemSize + SizeOf(TSmallMemHeader)) * C_ARRAYSIZE bytes
    FMemoryArray: Pointer;
    { TODO -oAM : bitmap/bitarray instead of full pointer array? }
    FFreedArray: array[0..C_ARRAYSIZE] of PSmallMemHeader;

    /// link to the next list with free memory
    FNextMemBlock: PSmallMemBlock;
    /// link to the previous list with free memory
    // - double linked to be able for fast removal of one block
    FPreviousMemBlock: PSmallMemBlock;
    /// link to the next list with freed memory, in case this list has no more freed mem
    FNextFreedMemBlock: PSmallMemBlock;
    /// link to the previous list with freed memory
    FPreviousFreedMemBlock: PSmallMemBlock;

    FLock: NativeInt;
    {$IFDEF SCALEMM_DEBUG}
      OwnerThreadId: NativeUInt;
//      {$IFDEF Align8Bytes}
//        {$ifndef CPUX64}
//        Filler1: Pointer;  // 8 bytes aligned
//        {$endif}
//      {$ENDIF}
    {$ELSE}
      {$IFDEF Align8Bytes}
        {$ifndef CPUX64}
        Filler2: Pointer;  // 8 bytes aligned   -> 188 + 4 = 192 (32bit)
        {$endif}
      {$ENDIF}
      {$IFDEF Align16Bytes}
        Filler3: Pointer;  // 16 bytes aligned
      {$ENDIF}
    {$ENDIF}

    function  GetUsedMemoryItem: PSmallMemHeader;    {$ifdef HASINLINE}inline;{$ENDIF}
    procedure FreeMem(aMemoryItem: PSmallMemHeader); {$ifdef HASINLINE}inline;{$ENDIF}

    procedure Lock;   {$ifdef HASINLINE}inline;{$ENDIF}
    procedure UnLock; {$ifdef HASINLINE}inline;{$ENDIF}
    procedure ThreadFreeMem(aMemoryItem: PSmallMemHeader);

    function  IsFree: Boolean;
    procedure FreeBlockMemory;

    procedure CheckMem(aDirection: TScanDirection = sdBoth);
    procedure DumpToFile(aFile: THandle; aTotalStats, aSingleStats: PSmallMemBlockListStats);
  end;

  /// memory block list
  // - current size if 16 bytes (this is a packed object)
  TSmallMemBlockList = object
    /// the per-thread memory manager which created this block
    OwnerManager: PSmallMemThreadManager;

    /// list containing freed memory (which this block owns)
    // - used to implement a fast caching of memory blocks
    FFirstFreedMemBlock: PSmallMemBlock;
    /// list containing all memory this block owns
    FFirstMemBlock: PSmallMemBlock;

    /// size of memory items (32, 64 etc bytes)
    //FItemSize : word;
    FItemSize : NativeUInt;
    /// recursive check when we alloc memory for this blocksize (new memory list)
    FRecursive: Boolean;

    {$ifdef CPUX64}
    // for faster "array[0..7] of TSmallMemBlockList" calc
    // (for 32 bits, the TSmallMemBlockList instance size if 16 bytes)
    FFiller: array[1..sizeof(NativeInt)-4] of byte;
    {$endif}

    procedure AddNewMemoryBlock;
    function  GetMemFromNewBlock: Pointer;
    function  GetBlockSize: Integer;

    procedure CheckMem;
    procedure DumpToFile(aFile: THandle; aTotalStats, aSingleStats: PSmallMemBlockListStats);
  end;

  /// handles per-thread memory managment
  TSmallMemThreadManager = object
  public
    SizeType   : TSizeType;
    OwnerThread: PBaseThreadManager;
    {$IFDEF Align16Bytes}
      {$ifndef CPUX64}
      Filer1: Int32;
      Filer2: Int32;    // 16 bytes aligned   -> 40 + 8 = 48 (32bit)
      {$endif}                                 //80 (64bit)
    {$ENDIF}
  private
    FFirstThreadFreeBlock: PSmallMemBlock;
    FDummyCounter: NativeInt;

    /// array with memory per block size of 32 bytes (mini blocks)
    // - i.e. 32, 64, 96, 128, 160, 192, 224 bytes
    FMiniMemoryBlocks: array[0..6] of TSmallMemBlockList;
    /// array with memory per block size of 256 bytes (small blocks)
    // - i.e. 256,512,768,1024,1280,1536,1792[,2048] bytes
    FSmallMemoryBlocks: array[0..MAX_SMALLMEMBLOCK] of TSmallMemBlockList;

    //FLock: NativeInt;
    //procedure Lock;
    //procedure UnLock;
  public
    procedure Init;
    procedure Reset;

    function  IsMemoryFromOtherThreadsPresent: Boolean; {$ifdef HASINLINE}inline;{$ENDIF}
    procedure FreeThreadFreedMem;
    procedure ReleaseAllFreeMem;

    procedure MoveAllMemToOtherManager(aOtherManager: PSmallMemThreadManager);
    function  GetBlockListOfSize(aItemSize: NativeUInt): PSmallMemBlockList;

    procedure CheckAllMem;
    procedure CheckMem(aMemory: Pointer);
    procedure DumpToFile(aFile: THandle; aTotalStats, aSingleStats: PThreadMemManagerStats);

    function GetMem(aSize: NativeUInt): Pointer;                       {$ifdef HASINLINE}inline;{$ENDIF}
    function FreeMem(aMemory: Pointer; aRecursive: Boolean = false): NativeInt;                     {$ifdef HASINLINE}inline;{$ENDIF}
    function ReallocMem(aMemory: Pointer; aSize: NativeUInt): Pointer; //{$ifdef HASINLINE}inline;{$ENDIF}
  end;

implementation

uses
  smmGlobal, ScaleMM2, smmFunctions,
  smmMediumMemory{needed for inline},
  smmLargeMemory{needed for inline};

{ TSmallMemHeader }

procedure TSmallMemHeader.CheckMem;
begin
  {$IFDEF SCALEMM_DEBUG}
  if Self.OwnerBlock.OwnerThreadId > 1 then
    Assert(Self.OwnerBlock.OwnerThreadId = GetCurrentThreadId);
  {$ENDIF}

  {$IFDEF SCALEMM_MAGICTEST}
  Assert(Magic1 = 1234567890);
  Assert(Magic2 = 1122334455);
  {$ENDIF}
  Assert(Size > 0);
  Assert(OwnerBlock <> nil);
  Assert(OwnerBlock.OwnerList <> nil);
  Assert(OwnerBlock.OwnerList.FItemSize = Size);
end;

{ TSmallMemBlock }

procedure TSmallMemBlock.CheckMem(aDirection: TScanDirection = sdBoth);
begin
  {$IFDEF SCALEMM_DEBUG}
  if (Self.OwnerThreadId > 1) then // and (Self.OwnerThreadId < MaxInt) then
  begin
    Assert(Self.OwnerThreadId = GetCurrentThreadId);
    Assert(Self.OwnerManager.OwnerThread.FThreadId = GetCurrentThreadId);
    Assert(Self.OwnerList.OwnerManager.OwnerThread.FThreadId = GetCurrentThreadId);
  end;
  {$ENDIF}

  //Scale_CheckMem(@Self); owner can be different thread (medium block)

  Assert(FMemoryArray <> nil);
  Assert(FMemoryArray = Pointer(NativeUInt(@Self) + SizeOf(Self)) );

  {$IFDEF SCALEMM_DEBUG}
  if (OwnerThreadId = 1) then //or (Self.OwnerThreadId = MaxInt) then
  begin
    Assert(OwnerManager <> nil);
    Assert(OwnerManager.OwnerThread <> nil);
    Assert(OwnerThreadId = OwnerManager.OwnerThread.FThreadId);
  end
  else
  begin
    Assert(OwnerThreadId = GetCurrentThreadId);
    Assert(FMemoryArray <> nil);
    Assert(FMemoryArray = Pointer(NativeUInt(@Self) + SizeOf(Self)) );

    Assert(OwnerList <> nil);
    Assert(OwnerList <> Pointer(1));
    Assert(OwnerList.OwnerManager <> nil);
    Assert(OwnerList.OwnerManager.OwnerThread <> nil);
    Assert(not OwnerList.OwnerManager.OwnerThread.FThreadTerminated);
    Assert(OwnerManager <> nil);
    Assert(OwnerManager <> Pointer(2));
    Assert(OwnerManager = OwnerList.OwnerManager);

    Assert(FFreedIndex <= NativeUInt(Length(FFreedArray)));
    Assert(FUsageCount <= NativeUInt(Length(FFreedArray)));
    Assert(FMemoryArray <> nil);
  end;
  {$ENDIF}

  if FPreviousMemBlock <> nil then
  begin
    Assert(FPreviousMemBlock.FNextMemBlock = @Self);
    {$IFDEF SCALEMM_DEBUG}
    Assert(FPreviousMemBlock.OwnerThreadId = Self.OwnerThreadId);
    {$ENDIF}
  end;
  if FPreviousFreedMemBlock <> nil then
  begin
    {$IFDEF SCALEMM_DEBUG}
    if Self.OwnerThreadId > 1 then
      Assert( (FPreviousFreedMemBlock.FFreedIndex <> 0) or (FPreviousFreedMemBlock.FUsageCount < C_ARRAYSIZE) ); //must have something free or available
    {$ENDIF}
    //Assert(FPreviousFreedMemBlock.FNextFreedMemBlock = @Self);   todo
    {$IFDEF SCALEMM_DEBUG}
    Assert(FPreviousFreedMemBlock.OwnerThreadId = Self.OwnerThreadId);
    if FPreviousFreedMemBlock.OwnerThreadId = $80808080 then
      FPreviousFreedMemBlock := nil;
    {$ENDIF}
  end;
  if (aDirection in [sdBoth, sdPrevious]) then
  begin
    if FPreviousMemBlock <> nil then
      FPreviousMemBlock.CheckMem(sdNone);
    if FPreviousFreedMemBlock <> nil then
        FPreviousFreedMemBlock.CheckMem(sdNone);
  end;

  if FNextMemBlock <> nil then
  begin
    Assert(FNextMemBlock.FPreviousMemBlock = @Self);
    {$IFDEF SCALEMM_DEBUG}
    Assert(FNextMemBlock.OwnerThreadId = Self.OwnerThreadId);
    {$ENDIF}
  end;
  if FNextFreedMemBlock <> nil then
  begin
    //Assert( (FNextFreedMemBlock.FFreedIndex <> 0) or (FNextFreedMemBlock.FUsageCount < C_ARRAYSIZE) ); //must have something free or available
    //if Self.OwnerManager.OwnerThread.FThreadId > 1 then
      Assert(FNextFreedMemBlock.FPreviousFreedMemBlock = @Self);
    {$IFDEF SCALEMM_DEBUG}
    Assert(FNextFreedMemBlock.OwnerThreadId = Self.OwnerThreadId);
    {$ENDIF}
  end;
  if (aDirection in [sdBoth, sdNext]) then
  begin
    if FNextMemBlock <> nil then
      FNextMemBlock.CheckMem(sdNone);
    if FNextFreedMemBlock <> nil then
        FNextFreedMemBlock.CheckMem(sdNone);
  end;
end;

procedure TSmallMemBlock.DumpToFile(aFile: THandle; aTotalStats, aSingleStats: PSmallMemBlockListStats);
begin
  WriteToFile(aFile, '  - SmallBlock   @ ');
  WriteNativeUIntToHexBuf(aFile, NativeUInt(@Self));
  WriteToFile(aFile, ', used count: ');
  WriteNativeUIntToStrBuf(aFile, FUsageCount);
  WriteToFile(aFile, ', used size: ');
  WriteNativeUIntToStrBuf(aFile, FUsageCount * OwnerList.FItemSize);
  WriteToFile(aFile, ' - free count: ');
  WriteNativeUIntToStrBuf(aFile, C_ARRAYSIZE - FUsageCount);
  WriteToFile(aFile, ', free size: ');
  WriteNativeUIntToStrBuf(aFile, (C_ARRAYSIZE - FUsageCount) * OwnerList.FItemSize);
  WriteToFile(aFile, ' - total count: ');
  WriteNativeUIntToStrBuf(aFile, C_ARRAYSIZE);
  WriteToFile(aFile, ', total size: ');
  WriteNativeUIntToStrBuf(aFile, C_ARRAYSIZE * OwnerList.FItemSize);
  WriteToFile(aFile, #13#10);

  Inc(aTotalStats.ArrayBlockCount);
  Inc(aSingleStats.ArrayBlockCount);
  Inc(aTotalStats.TotalUsageCount, FUsageCount);
  Inc(aSingleStats.TotalUsageCount, FUsageCount);
end;

procedure TSmallMemBlock.FreeBlockMemory;
{$IFDEF SCALEMM_DEBUG}
var
  pOwnerList: PSmallMemBlockList;
{$ENDIF}
begin
  if OwnerList.FFirstMemBlock = @Self then
    Exit; //keep one block

  {$IFDEF SCALEMM_DEBUG}
  Self.CheckMem(sdBoth);
  {$ENDIF}

  if OwnerList.FFirstFreedMemBlock = @Self then
    OwnerList.FFirstFreedMemBlock := Self.FNextFreedMemBlock;
  if OwnerList.FFirstMemBlock = @Self then
    OwnerList.FFirstMemBlock := Self.FNextMemBlock;

  // remove ourselves from linked list
  if FPreviousMemBlock <> nil then
    FPreviousMemBlock.FNextMemBlock := Self.FNextMemBlock;
  if FPreviousFreedMemBlock <> nil then
    FPreviousFreedMemBlock.FNextFreedMemBlock := Self.FNextFreedMemBlock;
  if FNextMemBlock <> nil then
    FNextMemBlock.FPreviousMemBlock := Self.FPreviousMemBlock;
  if FNextFreedMemBlock <> nil then
    FNextFreedMemBlock.FPreviousFreedMemBlock := Self.FPreviousFreedMemBlock;

  if OwnerList.FFirstFreedMemBlock <> nil then
  begin
    OwnerList.FFirstFreedMemBlock.FPreviousFreedMemBlock := nil;
    OwnerList.FFirstFreedMemBlock.CheckMem(sdBoth);
  end;
  if OwnerList.FFirstMemBlock <> nil then
  begin
    OwnerList.FFirstMemBlock.FPreviousMemBlock := nil;
    OwnerList.FFirstMemBlock.CheckMem(sdBoth);
  end;

  {$IFDEF SCALEMM_DEBUG}
  FPreviousFreedMemBlock := nil;
  FNextMemBlock := nil;
  FNextFreedMemBlock := nil;
  FPreviousMemBlock := nil;
  Self.CheckMem;
  OwnerList.CheckMem;
  OwnerThreadId := MaxInt;
  pOwnerList := Self.OwnerList;
  {$ENDIF}
  FMemoryArray  := nil;
  OwnerList     := nil;
  OwnerManager  := nil;

  //release medium block
  Scale_FreeMem(@Self);

  {$IFDEF SCALEMM_DEBUG}
  pOwnerList.CheckMem;
  {$ENDIF}
end;

procedure TSmallMemBlock.FreeMem(aMemoryItem: PSmallMemHeader);
{$IFDEF SCALEMM_DEBUG}
var ol: PSmallMemBlockList;
{$ENDIF}
begin
  {$IFDEF SCALEMM_MAGICTEST}
  Assert(aMemoryItem.Magic1 = 1234567890); //must be in use!
  {$ENDIF}
  {$IFDEF SCALEMM_DEBUG}
  aMemoryItem.CheckMem;
  CheckMem;
  OwnerList.CheckMem;
  {$ENDIF}
  //check double free
  if (aMemoryItem.Size and 1 <> 0) then
    Error(reInvalidPtr);

  // free mem block
  FFreedArray[FFreedIndex] := aMemoryItem;
  inc(FFreedIndex);
  if FFreedIndex > High(FFreedArray) then
    Error(reInvalidPtr);

  {$IFDEF SCALEMM_DEBUG}
  aMemoryItem.CheckMem;
  CheckMem;
  OwnerList.CheckMem;
  ol := OwnerList;
  {$ENDIF}

  // first free item of block?
  // then we add this block to (linked) list with available mem
  if FFreedIndex = 1 then
  begin
    if OwnerList.OwnerManager.OwnerThread.FThreadId <= 1 then Exit;
    {$IFDEF SCALEMM_DEBUG}
    Assert(Self.OwnerThreadId = GetCurrentThreadId);
    {$ENDIF}

    if ({self}FNextFreedMemBlock = nil) and       //not already in free list?
       ({self}FPreviousFreedMemBlock = nil) then
    with OwnerList^ do  //faster
    if {Owner}FFirstFreedMemBlock <> @Self then
    begin
      Assert(FNextFreedMemBlock = nil);
      Assert(FPreviousFreedMemBlock = nil);

      {Self.}FNextFreedMemBlock := {Owner}FFirstFreedMemBlock;   //link to first list
      {Self.}FPreviousFreedMemBlock := nil;
      if {Self}FNextFreedMemBlock <> nil then
        {Self}FNextFreedMemBlock.FPreviousFreedMemBlock := @Self; //back link
      {Owner}FFirstFreedMemBlock := @Self; //replace first list
    end;
  end
  else
    // all memory available?
    if (FFreedIndex = C_ARRAYSIZE) and (FLock = 0) then
      FreeBlockMemory;

  {$IFDEF SCALEMM_DEBUG}
  ol.CheckMem;
  {$ENDIF}
end;

function TSmallMemBlock.GetUsedMemoryItem: PSmallMemHeader;
begin
  {$IFDEF SCALEMM_DEBUG}
  Self.CheckMem;
  OwnerList.CheckMem;
  Assert(Self.OwnerThreadId = GetCurrentThreadId);
  {$ENDIF}

  if FFreedIndex > 0 then  //free items left?
  begin
    dec(FFreedIndex);
    Result := FFreedArray[FFreedIndex];

    if (FFreedIndex = 0) and              //no free items left?
       (FUsageCount >= C_ARRAYSIZE) then  //and fully used?
    begin
      Assert(OwnerList.FFirstFreedMemBlock = @Self);
      // set next free memlist
      OwnerList.FFirstFreedMemBlock := {Self.}FNextFreedMemBlock;
      // first one has no previous
      if {Self.}FNextFreedMemBlock <> nil then
        {Self.}FNextFreedMemBlock.FPreviousFreedMemBlock := nil;
      // remove from free list
      {Self.}FPreviousFreedMemBlock := nil;
      {Self.}FNextFreedMemBlock     := nil;
    end;
  end
  else  //otherwise use remaining unused mem
  begin
    Assert(FUsageCount < C_ARRAYSIZE);

    Result       := Pointer( NativeUInt(FMemoryArray) +
                             (FUsageCount * (OwnerList.FItemSize + SizeOf(TSmallMemHeader))) );
    inc(FUsageCount);
    Result.OwnerBlock  := @Self;
    Result.Size        := OwnerList.FItemSize;
    {$IFDEF SCALEMM_MAGICTEST}
    Result.Magic1 := 1234567890;
    Result.Magic2 := 1122334455;
    {$ENDIF}

    if (FUsageCount >= C_ARRAYSIZE) and   //fully used?
       (FFreedIndex = 0) then             //no free items left?
    begin
      Assert(OwnerList.FFirstFreedMemBlock = @Self);
      // set next free memlist
      OwnerList.FFirstFreedMemBlock := {Self.}FNextFreedMemBlock;
      // first one has no previous
      if {Self.}FNextFreedMemBlock <> nil then
        {Self.}FNextFreedMemBlock.FPreviousFreedMemBlock := nil;
      // remove from free list
      {Self.}FPreviousFreedMemBlock := nil;
      {Self.}FNextFreedMemBlock     := nil;
    end;
  end;

  {$IFDEF SCALEMM_DEBUG}
  Result.CheckMem;
  Self.CheckMem;
  OwnerList.CheckMem;
  {$ENDIF}
end;

function TSmallMemBlock.IsFree: Boolean;
begin
  Result := (FFreedIndex = C_ARRAYSIZE);
end;

procedure TSmallMemBlock.Lock;
begin
  while not CAS32(0, 1, @FLock) do
    Sleep(0);
end;

procedure TSmallMemBlock.UnLock;
begin
  Assert(FLock = 1);
  FLock := 0;
end;

(*
procedure TSmallMemBlock.UnLock;
begin
  Assert(FThreadFreedCount < 0);
  //unlock
  //if not CAS32(FThreadFreedCount, Abs(FThreadFreedCount+1), @FThreadFreedCount) then
  //  Assert(False);
  FThreadFreedCount := Abs(FThreadFreedCount+1);
end;
*)

(*
procedure TSmallMemBlock.Lock;
var
  iCount, iNewCount: Integer;
begin
  //unlock
  repeat
    iCount    := FThreadFreedCount;
    iNewCount := (iCount + 1) * -1;
    if (iCount >= 0) and
       CAS32(iCount, iNewCount, @FThreadFreedCount)
    then
      Break;
    //small wait: try to swith to other pending thread (if any) else direct continue
    if not SwitchToThread then
      sleep(0);

    //try again
    iCount    := FThreadFreedCount;
    iNewCount := (iCount + 1) * -1;
    if (iCount >= 0) and
       CAS32(iCount, iNewCount, @FThreadFreedCount)
    then
      Break;
    //wait some longer: force swith to any other thread
    sleep(1);
  until False;
end;
*)

procedure TSmallMemBlock.ThreadFreeMem(aMemoryItem: PSmallMemHeader);
var
  fb : PSmallMemBlock;
  cnt, cnt2: NativeInt;
  fh : PSmallMemHeaderFree;
  ref1, ref2: TSplitRecord;
  {$IFDEF SCALEMM_FILLFREEMEM}
  temppointer: Pointer;
  ol: PSmallMemBlockList;
  {$ENDIF}
begin
  {$IFDEF SCALEMM_MAGICTEST}
  Assert(aMemoryItem.Magic1 = 1234567890); //must be in use!
  {$ENDIF}

  Lock;

    {$IFDEF SCALEMM_FILLFREEMEM}
    //reset old mem
    temppointer := Pointer(NativeUInt(aMemoryItem) + SizeOf(TSmallMemHeader)); //do no reset header
    ol := OwnerList;
    if NativeUInt(ol) > 10 then   //can be changed in other thread?
      FillChar( temppointer^, ol.FItemSize, $80)
    else
      FillChar( temppointer^, 32, $80);
    {$ENDIF}

  //repeat
    fh   := FFirstThreadFreed;
    ref1 := FLockReference;

    {$IFDEF Align8Bytes}
    Assert( NativeUInt(@Self.FFirstThreadFreed) AND 7 = 0);
    {$ENDIF}
    {$IFDEF Align16Bytes}         //$1387B38
    Assert( NativeUInt(@Self.FFirstThreadFreed) AND 15 = 0);
    {$ENDIF}

    if ref1.DummyRef >= 1 shl 14 then //Int16(UInt16(-1)) then
      ref2.DummyRef := 0
    else
      ref2.DummyRef := ref1.DummyRef+1;  //change something for CAS
    ref2.Counter  := ref1.Counter+1;     //increase number of free items

    PSmallMemHeaderFree(aMemoryItem).NextFreeItem := fh;

    FLockReference    := ref2;
    FFirstThreadFreed := PSmallMemHeaderFree(aMemoryItem);
  //until CAS(fh, ref1.CompleteRef,
  //          PSmallMemHeaderFree(aMemoryItem), ref2.CompleteRef,
  //          Self.FFirstThreadFreed);

  //some pending? then notify master thread object
  //if ref2.Counter = 1 then
  if ref2.Counter = 4 then
  begin
    if NativeUInt(Self.OwnerManager) > 2 then //not global manager
    begin
      repeat
        {$IFDEF Align8Bytes}
        Assert( NativeUInt(@Self.OwnerManager.FFirstThreadFreeBlock) AND 7 = 0);
        {$ENDIF}
        {$IFDEF Align16Bytes}
        Assert( NativeUInt(@Self.OwnerManager.FFirstThreadFreeBlock) AND 15 = 0);
        {$ENDIF}

        fb  := Self.OwnerManager.FFirstThreadFreeBlock;
        cnt := Self.OwnerManager.FDummyCounter;

        Self.FNextThreadFreedBlock := fb;
        cnt2 := cnt+1;

        //if NativeUInt(Self.OwnerManager) <= 2 then Break;   //not global manager
      until CAS(fb, cnt, @Self, cnt2, Self.OwnerManager.FFirstThreadFreeBlock);

      if Self.OwnerManager.OwnerThread.FThreadTerminated then
        GlobalManager.IncSmallInterthreadMem;
    end;
  end;

  Unlock;
end;

{ TSmallMemBlockList }

procedure TSmallMemBlockList.AddNewMemoryBlock;
var
  pm: PSmallMemBlock;
begin
  {$IFDEF SCALEMM_DEBUG}
  Self.CheckMem;
  {$ENDIF}
  Assert(Self.OwnerManager.OwnerThread.FThreadId = GetCurrentThreadId);

  FRecursive := True;
  // get block from cache
  pm := GlobalManager.GetSmallBlockMemory(FItemSize);
  if pm = nil then
  begin
    // create own one
    pm := PThreadMemManager(OwnerManager.OwnerThread).FMediumMemManager
            .GetMem( GetBlockSize );
//    with pm^ do // put zero only to needed properties
//    begin
//      fillchar(FNextFreedMemBlock,SizeOf(FNextFreedMemBlock)+
//        SizeOf(FPreviousFreedMemBlock)+SizeOf(FUsageCount)+
//        SizeOf({$ifdef USEBITMAP}FFreed{$else}FFreedIndex{$endif}),0);
    fillchar(pm^, SizeOf(pm^), 0);

    pm.FMemoryArray := Pointer(NativeUInt(pm) + SizeOf(pm^));
    {$IFDEF Align8Bytes}
    Assert( NativeUInt(pm) AND 7 = 0);
    Assert( NativeUInt(pm.FMemoryArray) AND 7 = 0);
    {$ENDIF}
    {$IFDEF Align16Bytes}
    Assert( NativeUInt(pm) AND 15 = 0);
    Assert( NativeUInt(pm.FMemoryArray) AND 15 = 0);
    {$ENDIF}
    {$IFDEF SCALEMM_DEBUG}
    pm.OwnerList     := Pointer(1);
    pm.OwnerManager  := Pointer(2);
    pm.OwnerThreadId := 2;
    {$ENDIF}
  end;

  {$IFDEF SCALEMM_DEBUG}
  Self.CheckMem;
  {$ENDIF}

  // init
  with pm^ do
  begin
    pm.Lock;
      {$IFDEF SCALEMM_DEBUG}
      Assert(pm.OwnerThreadId <= 2);
      Assert(pm.OwnerManager = Pointer(2));
      Assert(pm.OwnerList = Pointer(1));
      pm.OwnerThreadId := GetCurrentThreadId;
      {$ENDIF}
      pm.OwnerManager      := Self.OwnerManager;
      pm.OwnerList         := @Self;
    pm.UnLock;

    // set new memlist as first, add link to current item
    {pm.}FNextMemBlock     := {self}FFirstMemBlock;
    // back link to new first item
    if {self}FFirstMemBlock <> nil then
      {self}FFirstMemBlock.FPreviousMemBlock := pm;
    {self}FFirstMemBlock   := pm;
    {pm.}FPreviousMemBlock := nil;

    // if block has already some freed memory (previous used block from cache)
    // then add to used list
    if {pm.}FFreedIndex > 0 then
    begin
      {pm.}FNextFreedMemBlock     := {Self}FFirstFreedMemBlock;  // link to first list
      {pm.}FPreviousFreedMemBlock := nil;
      if {pm}FNextFreedMemBlock <> nil then
        {pm}FNextFreedMemBlock.FPreviousFreedMemBlock := pm; // back link
      {Self}FFirstFreedMemBlock   := pm;                     // replace first list
    end
    else
    begin
      FNextFreedMemBlock := nil;
      FPreviousFreedMemBlock := nil;
    end;
  end;
  FRecursive := False;

  {$IFDEF SCALEMM_DEBUG}
  pm.CheckMem;
  Self.CheckMem;
  {$ENDIF}
end;

procedure TSmallMemBlockList.CheckMem;
begin
  if FFirstMemBlock <> nil then
  begin
    Assert(FFirstMemBlock.FPreviousMemBlock = nil);
    FFirstMemBlock.CheckMem;
    Assert(FFirstMemBlock.OwnerList <> nil);
    Assert(FFirstMemBlock.OwnerList = @Self);
  end;

  if FFirstFreedMemBlock <> nil then
  begin
    Assert(FFirstFreedMemBlock.FPreviousFreedMemBlock = nil);
    if OwnerManager.OwnerThread.FThreadId > 1 then
      FFirstFreedMemBlock.CheckMem;
    Assert(FFirstFreedMemBlock.OwnerList <> nil);
    Assert(FFirstFreedMemBlock.OwnerList = @Self);
  end;

  Assert(OwnerManager <> nil);
  Assert(OwnerManager.OwnerThread <> nil);
  //Assert(not OwnerManager.OwnerThread.FThreadTerminated);  can occur when thread is freed (TThreadMemManager.ProcessFreedMemFromOtherThreads)
  if OwnerManager.OwnerThread.FThreadId = 1 then
  begin
    //is global mem
  end
  else
    Assert(OwnerManager.OwnerThread.FThreadId = GetCurrentThreadId);
end;

procedure TSmallMemBlockList.DumpToFile(aFile: THandle; aTotalStats, aSingleStats: PSmallMemBlockListStats);
var next: PSmallMemBlock;
begin
  WriteToFile(aFile, '- SmallBlockList @ ');
  WriteNativeUIntToHexBuf(aFile, NativeUInt(@Self));
  WriteToFile(aFile, ', itemsize = ');
  WriteNativeUIntToStrBuf(aFile, Self.FItemSize);
  WriteToFile(aFile, ', memsize = ');
  WriteNativeUIntToStrBuf(aFile, Self.GetBlockSize);
  WriteToFile(aFile, #13#10);

  aTotalStats.ItemSize  := FItemSize;
  aSingleStats.ItemSize := FItemSize;
  aSingleStats.ArrayBlockSize := GetBlockSize;
  aSingleStats.ArrayBlockSize := GetBlockSize;

  next := FFirstMemBlock;
  while next <> nil do
  begin
    next.DumpToFile(aFile, aTotalStats, aSingleStats);
    next := next.FNextMemBlock;
  end;
end;

function TSmallMemBlockList.GetBlockSize: Integer;
begin
  Result := SizeOf(TSmallMemBlock) +
            (FItemSize + SizeOf(TSmallMemHeader) + 1) * C_ARRAYSIZE;
end;

function TSmallMemBlockList.GetMemFromNewBlock: Pointer;
var
  pm: PSmallMemBlock;
begin
  // store: first time init?
  if FFirstMemBlock = nil then
    AddNewMemoryBlock;

  pm := FFirstMemBlock;
  while (pm.FUsageCount >= C_ARRAYSIZE) and
        (pm.FFreedIndex = 0) do              //otherwise "pm.GetUsedMemoryItem" will fail
  begin
    AddNewMemoryBlock;
    pm := FFirstMemBlock;
  end;

  // get mem from list
  with pm^ do
  begin
    // space left?
    if FUsageCount < C_ARRAYSIZE then
    begin
      // calc next item
      //Result := FMemoryArray;
      //FMemoryArray := Pointer( NativeUInt(FMemoryArray) +
      //                         (FItemSize + SizeOf(TSmallMemHeader)) );
      //calculate by multiply to reduce cache writes?
      Result       := Pointer( NativeUInt(FMemoryArray) +
                               (FUsageCount * (FItemSize + SizeOf(TSmallMemHeader))) );
      inc(FUsageCount);
      // startheader = link to memlist
      PSmallMemHeader(Result).OwnerBlock  := pm;
      PSmallMemHeader(Result).Size        := FItemSize;
    end
    else
      Result := pm.GetUsedMemoryItem;
  end;

  {$IFDEF SCALEMM_MAGICTEST}
  TSmallMemHeader(Result^).Magic1 := 1234567890;
  TSmallMemHeader(Result^).Magic2 := 1122334455;
  {$ENDIF}
  {$IFDEF SCALEMM_DEBUG}
  TSmallMemHeader(Result^).CheckMem;
  Self.CheckMem;
  {$ENDIF}
  Result  := Pointer(NativeUInt(Result) + SizeOf(TSmallMemHeader));
end;

{ TSmallMemThreadManager }

procedure TSmallMemThreadManager.CheckAllMem;
var
  i: Integer;
begin
  for i := Low(Self.FMiniMemoryBlocks) to High(Self.FMiniMemoryBlocks) do
    FMiniMemoryBlocks[i].CheckMem;
  for i := Low(Self.FSmallMemoryBlocks) to High(Self.FSmallMemoryBlocks) do
    FSmallMemoryBlocks[i].CheckMem;
end;

procedure TSmallMemThreadManager.CheckMem(aMemory: Pointer);
var
  ph: PSmallMemHeader;
begin
  Assert(Self.OwnerThread.FThreadId = GetCurrentThreadId);

  ph  := PSmallMemHeader(NativeUInt(aMemory) - SizeOf(TSmallMemHeader));
  ph.CheckMem;
  Assert(ph.OwnerBlock <> nil);
  ph.OwnerBlock.CheckMem(sdBoth);
end;

procedure TSmallMemThreadManager.DumpToFile(aFile: THandle; aTotalStats, aSingleStats: PThreadMemManagerStats);
var
  i: Integer;
begin
  WriteToFile(aFile, 'TSmallMemThreadManager'#13#10);

  for i := Low(Self.FMiniMemoryBlocks) to High(Self.FMiniMemoryBlocks) do
    FMiniMemoryBlocks[i].DumpToFile(aFile,
      @aTotalStats.SmallMemoryStats.FMiniMemoryBlocks[i],
      @aSingleStats.SmallMemoryStats.FMiniMemoryBlocks[i]);
  for i := Low(Self.FSmallMemoryBlocks) to High(Self.FSmallMemoryBlocks) do
    FSmallMemoryBlocks[i].DumpToFile(aFile,
      @aTotalStats.SmallMemoryStats.FSmallMemoryBlocks[i],
      @aSingleStats.SmallMemoryStats.FSmallMemoryBlocks[i]);
end;

function TSmallMemThreadManager.FreeMem(aMemory: Pointer; aRecursive: Boolean): NativeInt;
var
  ph: PSmallMemHeader;
begin
  if not aRecursive and (FFirstThreadFreeBlock <> nil) then
    FreeThreadFreedMem;

  {$ifdef SCALEMM_DEBUG} Result := 0; try {$ENDIF}
  ph  := PSmallMemHeader(NativeUInt(aMemory) - SizeOf(TSmallMemHeader));

  {$IFDEF SCALEMM_MAGICTEST}
  Assert(ph.Magic1 = 1234567890); //must be in use!
  {$ENDIF}
  {$IFDEF SCALEMM_DEBUG}
  ph.CheckMem;
  {$ENDIF}
  //check double free
  if (ph.Size and 1 <> 0) then
    Error(reInvalidPtr);

  // block obtained via Scale_GetMem()
  Assert(ph.OwnerBlock <> nil);
  ph.OwnerBlock.FreeMem(ph);
  Result := 0;

  {$ifdef SCALEMM_DEBUG} except sleep(0); end; {$ENDIF}
end;

procedure TSmallMemThreadManager.FreeThreadFreedMem;
var
  //fb : PSmallMemBlock;
  ob: PSmallMemBlock;
  freeblock, nextblock, nextblock2: PSmallMemBlock;
  pfreemem, nextmem, nextmem2: PSmallMemHeaderFree;
  p:pointer;
  cnt, cnt2: NativeInt;
  ref1, ref2: TSplitRecord;
begin
  if FFirstThreadFreeBlock = nil then Exit;

  //get linked list of blocks with free memory
  //Lock;
  repeat
    freeblock := FFirstThreadFreeBlock;
    cnt       := FDummyCounter;
    if cnt >= 1 shl 30 then
      cnt2 := 0
    else
      cnt2 := cnt+1;

    {$IFDEF Align8Bytes}
    Assert( NativeUInt(@FFirstThreadFreeBlock) AND 7 = 0);
    {$ENDIF}
    {$IFDEF Align16Bytes}
    Assert( NativeUInt(@FFirstThreadFreeBlock) AND 15 = 0);
    {$ENDIF}
  until CAS(freeblock, cnt, nil, cnt2, FFirstThreadFreeBlock);  //threadsafe replace FFirstThreadFreeBlock with nil
  //FFirstThreadFreeBlock := nil;
  //UnLock;

  //we have a linked list of small blocks...
  nextblock := freeblock;
  while nextblock <> nil do
  begin
    (*
    nextblock.Lock;
    pfreemem := nextblock.FFirstThreadFreed;
    nextblock.FFirstThreadFreed := nil;
    nextblock.FThreadFreedCount := -1;  //will be made 0 in unlock
    nextblock2 := nextblock.FNextThreadFreedBlock;
    nextblock.UnLock;
    *)
    //get linked list of small mem items of a block
    nextblock.Lock;
    //repeat
      pfreemem      := nextblock.FFirstThreadFreed;
      ref1          := nextblock.FLockReference;

      //change something for CAS
      if ref1.DummyRef >= 1 shl 14 then //Int16(UInt16(-1)) then
        ref2.DummyRef := 0
      else
        ref2.DummyRef := ref1.DummyRef+1;
      //no more items are free
      ref2.Counter  := 0;

      {$IFDEF Align8Bytes}
      Assert( NativeUInt(@nextblock.FFirstThreadFreed) AND 7 = 0);
      {$ENDIF}
      {$IFDEF Align16Bytes}
      Assert( NativeUInt(@nextblock.FFirstThreadFreed) AND 15 = 0);
      {$ENDIF}
      nextblock.FLockReference    := ref2;
      nextblock.FFirstThreadFreed := nil;
    //until CAS(pfreemem, ref1.CompleteRef, nil, ref2.CompleteRef,
    //          nextblock.FFirstThreadFreed);

    //get next block (for next loop iteration)
    nextblock2 := nextblock.FNextThreadFreedBlock;
    nextblock.UnLock;
    //nextblock  := nextblock2;

    nextmem := pfreemem;
    while nextmem <> nil do
    begin
      p := Pointer(NativeUInt(nextmem) + SizeOf(TSmallMemHeader));
      nextmem2 := nextmem;
      nextmem  := nextmem.NextFreeItem;

      if nextmem2.OwnerBlock.OwnerManager = @Self then    //owner can be changed in the mean time!
      begin
        //todo: lock?
        Self.FreeMem(p, True);
      end
      else
      begin
        ob := nextmem2.OwnerBlock;
        //ownerblock can be changed in the meantime?
        //if NativeUInt(ob) > 1000 then
          ob.ThreadFreeMem( PSmallMemHeader(nextmem2) )
        //retry next loop
        (*
        else
        begin
          repeat
            {$IFDEF Align8Bytes}
            Assert( NativeUInt(@Self.FFirstThreadFreeBlock) AND 7 = 0);
            {$ENDIF}
            {$IFDEF Align16Bytes}
            Assert( NativeUInt(@Self.FFirstThreadFreeBlock) AND 15 = 0);
            {$ENDIF}

            fb  := Self.FFirstThreadFreeBlock;
            cnt := Self.FDummyCounter;
            nextblock.FNextThreadFreedBlock := fb;
            cnt2 := cnt+1;

            //if NativeUInt(ob) <= 2 then Break;   //not global manager
          until CAS(fb, cnt, nextblock, cnt2, Self.FFirstThreadFreeBlock);

          nextblock.FFirstThreadFreed
          Break;
        end;
        *)
      end;
    end;

//    if nextblock.IsFree and       can be fully freed by self.freemem(p)
//       (nextblock.OwnerManager = @Self) then
//    begin
      //nextblock.UnLock;
//      nextblock.FreeBlockMemory;
//    end
//    else ;
      //nextblock.UnLock;
    nextblock := nextblock2;
  end;
end;

function TSmallMemThreadManager.GetBlockListOfSize(
  aItemSize: NativeUInt): PSmallMemBlockList;
begin
  Result := nil;

  if aItemSize < (length(FMiniMemoryBlocks)*32) then
    // blocks of 32: 32, 64, 96, 128, 160, 192, 224
    Result := @FMiniMemoryBlocks[aItemSize shr 5]
  else if aItemSize < (length(FSmallMemoryBlocks)*256) then
    // blocks of 256: 256,512,768,1024,1280,1536,1792[,2048] bytes
    Result := @FSmallMemoryBlocks[aItemSize shr 8]
  else
  begin
    // not allocated by this unit (should not happen)
    Assert(false);
    Exit;
  end;
end;

function TSmallMemThreadManager.GetMem(aSize: NativeUInt): Pointer;
var
  bm: PSmallMemBlockList;
  iNewSize: NativeUInt;
begin
  if FFirstThreadFreeBlock <> nil then
    FreeThreadFreedMem;

  bm := nil;
  iNewSize := aSize + (SizeOf(TSmallMemHeader) - 1);

  if iNewSize <= ((length(FMiniMemoryBlocks)*32)-1) then
    // blocks of 32: 32, 64, 96, 128, 160, 192, 224
    bm := @FMiniMemoryBlocks[(iNewSize) shr 5]
  else if iNewSize <= ((length(FSmallMemoryBlocks)*256)-1) then
    // blocks of 256: 256,512,768,1024,1280,1536,1792 bytes
    bm := @FSmallMemoryBlocks[(iNewSize) shr 8]
  else
    Error(reInvalidPtr);

  with bm^ do
  begin
    if FFirstFreedMemBlock <> nil then
    begin
    // first get from freed mem (fastest because most chance?)
      Result := FFirstFreedMemBlock.GetUsedMemoryItem;
      Result := Pointer(NativeUInt(Result) + SizeOf(TSmallMemHeader));
    end
    else
      // from normal list
      Result := GetMemFromNewBlock;
  end;

  {$IFDEF SCALEMM_DEBUG}
  PSmallMemHeader(NativeUInt(Result) - SizeOf(TSmallMemHeader)).CheckMem;
  PSmallMemHeader(NativeUInt(Result) - SizeOf(TSmallMemHeader)).OwnerBlock.CheckMem;
  PSmallMemHeader(NativeUInt(Result) - SizeOf(TSmallMemHeader)).OwnerBlock.OwnerList.CheckMem;
  bm.CheckMem;
  {$ENDIF}
end;

procedure TSmallMemThreadManager.Init;
var i, j: NativeUInt;
begin
  SizeType := stSmall;

  {$IFDEF Align8Bytes}         //   $3D001C      $3D0014
  Assert( NativeUInt(@Self) AND 7 = 0);
  {$ENDIF}
  {$IFDEF Align16Bytes}              //28
  Assert( NativeUInt(@Self) AND 15 = 0);
  {$ENDIF}

  Fillchar(Self, SizeOf(Self), 0);
  j := 32;
  for i := Low(FMiniMemoryBlocks) to High(FMiniMemoryBlocks) do
  begin // 32, 64, 96, 128, 160, 192, 224 bytes
    FMiniMemoryBlocks[i].OwnerManager := @Self;
    FMiniMemoryBlocks[i].FItemSize := j;
    inc(j,32);
  end;
  assert(j=256);
  for i := Low(FSmallMemoryBlocks) to High(FSmallMemoryBlocks) do
  begin // 256,512,768,1024,1280,1536,1792 bytes
    FSmallMemoryBlocks[i].OwnerManager := @Self;
    FSmallMemoryBlocks[i].FItemSize := j;
    inc(j,256);
  end;
  //assert(j=2304);
  assert(j=2560);
end;

function TSmallMemThreadManager.IsMemoryFromOtherThreadsPresent: Boolean;
begin
  Result := (FFirstThreadFreeBlock <> nil);
end;

(*
procedure TSmallMemThreadManager.Lock;
begin
  //LOCK
  while not CAS32(0, 1, @FLock) do
  begin
    //small wait: try to swith to other pending thread (if any) else direct continue
    if not SwitchToThread then
      sleep(0);
    //try again
    if CAS32(0, 1, @FLock) then
      Break;
    //wait some longer: force swith to any other thread
    sleep(1);
  end;
end;
*)

procedure TSmallMemThreadManager.MoveAllMemToOtherManager(
  aOtherManager: PSmallMemThreadManager);
var
  i: NativeUInt;

  procedure __ProcessBlockMem(aOldBlock, aGlobalBlock: PSmallMemBlockList);
  var
    allmem, tempmem, prevmem, prevfree,
    firstmem, lastmem, lastfreemem, firstfreemem: PSmallMemBlock;
  begin
    allmem        := aOldBlock.FFirstMemBlock;
    aOldBlock.FFirstMemBlock := nil;
    aOldBlock.FFirstFreedMemBlock := nil;
    firstfreemem  := nil; lastfreemem   := nil;
    firstmem      := nil; lastmem       := nil;
    prevmem       := nil; prevfree      := nil;

    // scan all memoryblocks and filter unused blocks
    while allmem <> nil do
    begin
      allmem.Lock;

      // fully free, no mem in use?
      if (allmem.FFreedIndex = allmem.FUsageCount) then
      begin
        // next one
        tempmem := allmem;
        allmem  := allmem.FNextMemBlock;

        // remove from linked list
        if tempmem.FPreviousMemBlock <> nil then
          tempmem.FPreviousMemBlock.FNextMemBlock := tempmem.FNextMemBlock;
        if tempmem.FPreviousFreedMemBlock <> nil then
          tempmem.FPreviousFreedMemBlock.FNextFreedMemBlock := tempmem.FNextFreedMemBlock;
        if tempmem.FNextMemBlock <> nil then
          tempmem.FNextMemBlock.FPreviousMemBlock := tempmem.FPreviousMemBlock;
        if tempmem.FNextFreedMemBlock <> nil then
          tempmem.FNextFreedMemBlock.FPreviousFreedMemBlock := tempmem.FPreviousFreedMemBlock;

        //if allmem <> nil then
        //  allmem.Lock;
        // dispose
        Assert(Self.OwnerThread.FThreadId = GetCurrentThreadId);
        tempmem.UnLock;
        PThreadMemManager(Self.OwnerThread).FMediumMemManager.FreeMemOnlyMarked(tempmem);
//        PThreadMemManager(Self.OwnerThread).FMediumMemManager.FreeMemNoCheck(tempmem);
//        Scale_FreeMem(tempmem);

        Continue;
      end
      else
      // some items in use (in other thread? or mem leak?)
      begin
        if firstmem = nil then
          firstmem := allmem;

        // first item of list?
        if firstfreemem = nil then
        begin
          if allmem.FFreedIndex > 0 then
            firstfreemem := allmem
        end
        else
        begin
          // else add to list (link to previous)
          {
          lastinusemem.FNextFreedMemBlock := allmem;
          allmem.FPreviousFreedMemBlock := lastinusemem;

          if firstinusemem.FNextFreedMemBlock = nil then
            firstinusemem.FNextFreedMemBlock := allmem;
          }
        end;

        if allmem.FFreedIndex > 0 then
          lastfreemem := allmem;
        lastmem := allmem;
      end;

      //allmem.Lock;
        {$IFDEF SCALEMM_DEBUG}
        Assert(allmem.OwnerThreadId <> 0);
        allmem.OwnerThreadId := 1;
        {$ENDIF}
        allmem.OwnerManager  := aOtherManager;
        allmem.OwnerList     := aGlobalBlock;
        // next one
        tempmem := allmem.FNextMemBlock;
        //allmem.FNextMemBlock := nil;
      allmem.UnLock;

      if prevmem <> nil then
      begin
        prevmem.FNextMemBlock    := allmem;
        allmem.FPreviousMemBlock := prevmem;
      end
      else
      begin
        allmem.FPreviousMemBlock := nil;
        allmem.FNextMemBlock     := nil;
      end;
      prevmem := allmem;

      if allmem.FFreedIndex > 0 then
      begin
        if prevfree <> nil then
        begin
          prevfree.FNextFreedMemBlock   := allmem;
          allmem.FPreviousFreedMemBlock := prevfree;
        end
        else
        begin
          allmem.FPreviousFreedMemBlock := nil;
          allmem.FNextFreedMemBlock     := nil;
        end;
        prevfree := prevmem;
      end
      else
      begin
        allmem.FPreviousFreedMemBlock := nil;
        allmem.FNextFreedMemBlock     := nil;
      end;

      allmem  := tempmem;
    end;

    if firstmem <> nil then
    begin
      assert(lastmem <> nil);
      if lastmem <> nil then
      begin
        lastmem.FNextMemBlock := aGlobalBlock.FFirstMemBlock;
        if aGlobalBlock.FFirstMemBlock <> nil then
          aGlobalBlock.FFirstMemBlock.FPreviousMemBlock := lastmem;
      end;
      firstmem.FPreviousMemBlock := nil;
      aGlobalBlock.FFirstMemBlock := firstmem;
    end;

    if firstfreemem <> nil then
    begin
      assert(lastfreemem <> nil);
      // add freemem list to front (replace first item, link previous to last item)
      if aGlobalBlock.FFirstFreedMemBlock <> nil then
        aGlobalBlock.FFirstFreedMemBlock.FPreviousFreedMemBlock := lastfreemem;
      lastfreemem.FNextFreedMemBlock  := aGlobalBlock.FFirstFreedMemBlock;
      aGlobalBlock.FFirstFreedMemBlock := firstfreemem;
      firstfreemem.FPreviousFreedMemBlock := nil;
    end;

    {$IFDEF SCALEMM_DEBUG}
    aGlobalBlock.CheckMem;
    {$ENDIF}
  end;

begin
  Assert(Self.OwnerThread.FThreadId = GetCurrentThreadId);
  for i := Low(Self.FMiniMemoryBlocks) to High(Self.FMiniMemoryBlocks) do
    __ProcessBlockMem( @FMiniMemoryBlocks[i],   @aOtherManager.FMiniMemoryBlocks[i]);
  for i := Low(Self.FSmallMemoryBlocks) to High(Self.FSmallMemoryBlocks) do
    __ProcessBlockMem( @FSmallMemoryBlocks[i],  @aOtherManager.FSmallMemoryBlocks[i]);
end;

const
  SmallBlockDownsizeCheckAdder = 64;
  SmallBlockUpsizeAdder = 32;

function TSmallMemThreadManager.ReallocMem(aMemory: Pointer;
  aSize: NativeUInt): Pointer;
var
  ph: PSmallMemHeader;
  {$IFDEF SCALEMM_DEBUG}
  p: Pointer;
  {$ENDIF}
begin
  if FFirstThreadFreeBlock <> nil then
    FreeThreadFreedMem;

  ph := PSmallMemHeader(NativeUInt(aMemory) - SizeOf(TSmallMemHeader));
  {$IFDEF SCALEMM_DEBUG}
  Assert(ph.OwnerBlock.OwnerList <> nil);
  p  := Pointer(NativeUInt(aMemory) - SizeOf(TSmallMemHeader));
  PSmallMemHeader(p).CheckMem;
  {$ENDIF}

  with ph^ do
  begin
    //downscaling, new size smaller than current size
    if (NativeUInt(aSize) <= Size) then
    begin
      if NativeUInt(aSize) + SmallBlockDownsizeCheckAdder >= (Size shr 2) then
      begin
        Result := aMemory; // no resize needed up to 1/4 the current item size
      end
      else
      // too much downscaling: use move
      begin
        Result := Self.GetMem(aSize); // new mem
        Move(aMemory^, Result^, aSize); // copy (use smaller new size)
        //Self.FreeMem(aMemory); // free old mem
        ph.OwnerBlock.FreeMem(ph);
      end;
    end
    else
    //upscaling, new size bigger than current size
    begin
      if aSize <= C_MAX_SMALLMEM_SIZE then   //1 till 2048
        Result := Self.GetMem(aSize + SmallBlockUpsizeAdder)
      else  //bigger mem?
        Result := PThreadMemManager(Self.OwnerThread).GetMem(aSize + SmallBlockUpsizeAdder); // new mem
      Move(aMemory^, Result^, Size); // copy (use smaller old size)
      //Self.FreeMem(aMemory); // free old mem
      ph.OwnerBlock.FreeMem(ph);
    end;
  end;

  {$IFDEF SCALEMM_DEBUG}
  p := PBaseMemHeader(NativeUInt(Result) - SizeOf(TBaseMemHeader));
  if PBaseMemHeader(p).OwnerBlock.OwnerManager = @Self then
  begin
    p := Pointer(NativeUInt(Result) - SizeOf(TSmallMemHeader));
    PSmallMemHeader(p).CheckMem;
    if PSmallMemHeader(p).OwnerBlock <> nil then
      Assert( PSmallMemHeader(p).OwnerBlock.OwnerList <> nil );
  end;
  {$ENDIF}
end;

procedure TSmallMemThreadManager.ReleaseAllFreeMem;
var
  i: Integer;
  first,
  block: PSmallMemBlock;
begin
  //check pending thread free mem
  for i := 0 to High(FMiniMemoryBlocks) do
  begin
    block := FMiniMemoryBlocks[i].FFirstMemBlock;
    while block <> nil do
    begin
      //pending thread free mem?
      if block.FLockReference.Counter > 0 then
      begin
        //add to linked list
        block.FNextThreadFreedBlock := FFirstThreadFreeBlock;
        FFirstThreadFreeBlock       := block;
      end;
    end;
  end;
  for i := 0 to High(FSmallMemoryBlocks) do
  begin
    block := FSmallMemoryBlocks[i].FFirstMemBlock;
    while block <> nil do
    begin
      //pending thread free mem?
      if block.FLockReference.Counter > 0 then
      begin
        //add to linked list
        block.FNextThreadFreedBlock := FFirstThreadFreeBlock;
        FFirstThreadFreeBlock       := block;
      end;
    end;
  end;

  //free pending thread free mem
  FreeThreadFreedMem;

  //remove first cached block (if complete free)
  for i := 0 to High(FMiniMemoryBlocks) do
  begin
    first := FMiniMemoryBlocks[i].FFirstMemBlock;
    //block := first;

    FMiniMemoryBlocks[i].FFirstMemBlock := nil;  //reset so it will be really freed (see TSmallMemBlock.FreeBlockMemory)
    if (first <> nil) then
    begin
      if first.FFreedIndex = C_ARRAYSIZE then  //complete free?
      begin
         //block := first.FNextMemBlock;
         first.FreeBlockMemory;
      end;
    end;
  end;
  for i := 0 to High(FSmallMemoryBlocks) do
  begin
    first := FSmallMemoryBlocks[i].FFirstMemBlock;
    FSmallMemoryBlocks[i].FFirstMemBlock := nil;  //reset so it will be really freed (see TSmallMemBlock.FreeBlockMemory)
    if (first <> nil) then
      if first.FFreedIndex = C_ARRAYSIZE then  //complete free?
        first.FreeBlockMemory;
  end;
end;

procedure TSmallMemThreadManager.Reset;
var
  i: NativeUInt;

  procedure __ResetBlocklist(aBlocklist: PSmallMemBlockList);
  begin
    aBlocklist.FFirstFreedMemBlock := nil;
    aBlocklist.FFirstMemBlock := nil;
    aBlocklist.FRecursive := False;
  end;

begin
  for i := Low(FMiniMemoryBlocks) to High(FMiniMemoryBlocks) do
    __ResetBlocklist(@FMiniMemoryBlocks[i]);
  for i := Low(FSmallMemoryBlocks) to High(FSmallMemoryBlocks) do
    __ResetBlocklist(@FSmallMemoryBlocks[i]);
end;

(*
procedure TSmallMemThreadManager.UnLock;
begin
  //UNLOCK
  if not CAS32(1, 0, @FLock) then
    Assert(False);
  //FLock := False;
end;
*)

initialization
  {$IFDEF Align8Bytes}
    {$IF (SizeOf(TSmallMemHeader) AND 7 <> 0) }
        {$MESSAGE ERROR 'not aligned'}
    {$IFEND}
    {$IF (SizeOf(TSmallMemBlock)  AND 7 <> 0) }
        {$MESSAGE ERROR 'not aligned'}
    {$IFEND}
  {$ENDIF}

  {$IFDEF Align16Bytes}
    {$IF (SizeOf(TSmallMemHeader) AND 15 <> 0) }
        {$MESSAGE ERROR 'not aligned'}
    {$IFEND}
    {$IF (SizeOf(TSmallMemBlock)  AND 15 <> 0) }
        {$MESSAGE ERROR 'not aligned'}
    {$IFEND}
  {$ENDIF}

  {$IF SizeOf(TBaseMemHeader) <> SizeOf(TSmallMemHeader) }
      {$MESSAGE ERROR 'wrong header sizes'}
  {$IFEND}

end.
