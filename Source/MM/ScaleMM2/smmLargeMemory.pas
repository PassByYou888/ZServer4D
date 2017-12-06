unit smmLargeMemory;

interface

{$Include smmOptions.inc}

uses
  smmTypes, smmStatistics;

type
  PLargeHeader           = ^TLargeHeader;
  PLargeBlockMemory      = ^TLargeBlockMemory;
  PLargeMemThreadManager = ^TLargeMemThreadManager;
  //PLargeThreadManagerOffset = ^TLargeThreadManagerOffset;

  TLargeHeader = record
    {$IFDEF SCALEMM_MAGICTEST}
    Magic1     : NativeInt;
    Magic2     : NativeInt;
    {$ELSE}
      {$IFDEF Align16Bytes}
        {$ifndef CPUX64}
        Filer1: Pointer;  // 16 bytes aligned for 32 bit compiler
        Filer2: Pointer;
        {$endif}
      {$ENDIF}
    {$ENDIF}

    Size       : NativeUInt;
    OwnerBlock : PBaseThreadManagerOffset;
  end;

  TLargeBlockMemory = object
    OwnerManager: PLargeMemThreadManager;
    Size        : NativeUInt;
    //PreviousMem,
    //NextMem: PLargeBlockMemory;

    {$IFDEF Align16Bytes}
      {$ifndef CPUX64}
      Filer1: Pointer;  // 16 bytes aligned for 32 bit compiler
      Filer2: Pointer;
      {$endif}
    {$ENDIF}
  end;

  TLargeThreadManagerOffset = packed
                              {$if CompilerVersion >= 18} //Delphi 2007 and higher?
                              record {$ELSE} object {$ifend}
  public
    //SizeType    : TSizeType;
    Filler1, Filler2, Filler3: Byte;  //offset of 1 to distinguish of being medium block
    OwnerThread: PBaseThreadManager;
  end;

  TLargeMemThreadManager = object
  protected
    //FFirstMem: PLargeBlockMemory;
    FAllocCount, FAllocSize: NativeUInt;
  public
    SizeType   : TSizeType;
    OwnerThread: PBaseThreadManager;
  public
    procedure Init;

    function GetMem(aSize: NativeUInt) : Pointer;
    function FreeMem(aMemory: Pointer): NativeInt;
    //function ReallocMem(aMemory: Pointer; aSize: NativeUInt): Pointer;

    procedure CheckMem(aMemory: Pointer = nil);
    procedure DumpToFile(aFile: THandle; aTotalStats, aSingleStats: PThreadMemManagerStats);

    function GetMemWithHeader(aSize: NativeUInt) : Pointer;
    function FreeMemWithHeader(aMemory: Pointer): NativeInt;
    function ReallocMemWithHeader(aMemory: Pointer; aSize: NativeUInt): Pointer;
  end;

implementation

uses
  smmMediumMemory,
  smmSmallMemory{needed for inline},
  smmFunctions, ScaleMM2;

{ TLargeMemThreadManager }

procedure TLargeMemThreadManager.CheckMem(aMemory: Pointer);
begin
  { TODO -oAM : check if valid memory (large)}
end;

procedure TLargeMemThreadManager.DumpToFile(aFile: THandle; aTotalStats,
  aSingleStats: PThreadMemManagerStats);
begin
  WriteToFile(aFile, 'TLargeMemThreadManager'#13#10);
  WriteToFile(aFile, '- Alloc count: ');
  WriteNativeUIntToStrBuf(aFile, FAllocCount);
  WriteToFile(aFile, ', Alloc size: ');
  WriteNativeUIntToStrBuf(aFile, FAllocSize);

  Inc(aTotalStats.LargeMemoryStats.AllocCount, FAllocCount);
  Inc(aTotalStats.LargeMemoryStats.AllocSize, FAllocSize);
  Inc(aSingleStats.LargeMemoryStats.AllocCount, FAllocCount);
  Inc(aSingleStats.LargeMemoryStats.AllocSize, FAllocSize);
end;

function TLargeMemThreadManager.FreeMem(aMemory: Pointer): NativeInt;
var
  pblock: PLargeBlockMemory;
  meminfo: TMemoryBasicInformation;
  pendingSize: NativeUInt;
begin
  Result := 0;
  pblock := aMemory;
  Assert(pblock.Size > C_MAX_MEDIUMMEM_SIZE);
  Dec(FAllocCount);
  Dec(FAllocSize, pblock.Size);

  VirtualQuery(pblock, meminfo, SizeOf(meminfo));
  //1 big complete block?
  if meminfo.RegionSize >= pblock.Size then
  begin
    if not VirtualFree(pblock, 0, MEM_RELEASE) then
      //Result := 1;
      System.Error(reInvalidPtr);
  end
  else
  //consist of multiple blocks? (due to inplace resize) then free each virtual block
  begin
    pendingSize := pblock.Size;
    repeat
      if not VirtualFree(pblock, 0, MEM_RELEASE) then
        System.Error(reInvalidPtr);
      Dec(pendingSize, meminfo.RegionSize);
      if pendingSize <= 0 then Break;

      //next block
      pblock := PLargeBlockMemory( NativeUInt(pblock) + meminfo.RegionSize );
      VirtualQuery(pblock, meminfo, SizeOf(meminfo));
    until False;
  end
end;

function TLargeMemThreadManager.FreeMemWithHeader(aMemory: Pointer): NativeInt;
var
  pblock : PLargeBlockMemory;
begin
  pblock := PLargeBlockMemory(NativeUInt(aMemory) - SizeOf(TLargeBlockMemory) - SizeOf(TLargeHeader));

  Dec(FAllocCount);
  Dec(FAllocSize, pblock.Size);

  {
  if pblock.PreviousMem <> nil then
    pblock.PreviousMem.NextMem := pblock.NextMem;
  if pblock.NextMem <> nil then
    pblock.NextMem.PreviousMem := pblock.PreviousMem;
  if FFirstMem = pblock then
  begin
    FFirstMem := pblock.NextMem;
    if FFirstMem <> nil then
      FFirstMem.PreviousMem := nil;
  end;

  pblock.NextMem := nil;
  pblock.PreviousMem := nil;
  }

  Result := Self.FreeMem(pblock);
end;

function TLargeMemThreadManager.GetMem(aSize: NativeUInt): Pointer;
begin
  Result := VirtualAlloc( nil,
                          aSize,
                          MEM_COMMIT, // {$ifdef AlwaysAllocateTopDown} or MEM_TOP_DOWN{$endif},  medium blocks cannot use mem > 2gb
                          PAGE_READWRITE);

  if Result = nil then
    System.Error(reOutOfMemory);

  Inc(FAllocCount);
  Inc(FAllocSize, aSize);
  //if NativeUInt(Result) > NativeUInt(1 shl 31) then        more than 2gb possible!
  //  System.Error(reInvalidPtr);
end;

const
  {The granularity of large blocks}
  LargeBlockGranularity = 65536;

function TLargeMemThreadManager.GetMemWithHeader(aSize: NativeUInt): Pointer;
var
  iAllocSize: NativeUInt;
  pheader: PLargeHeader;
  pblock : PLargeBlockMemory;
  pthreadoffset: PBaseThreadManagerOffset;
begin
  iAllocSize    := aSize + SizeOf(TLargeBlockMemory) + SizeOf(TLargeHeader);
  iAllocSize    := (iAllocSize + LargeBlockGranularity) and -LargeBlockGranularity; //round to 64k
  if iAllocSize <= C_MAX_MEDIUMMEM_SIZE then
    iAllocSize  := C_MAX_MEDIUMMEM_SIZE + SizeOf(TLargeHeader);
  //block
  pblock        := Self.GetMem(iAllocSize);
  pblock.OwnerManager  := @Self;
  pblock.Size          := iAllocSize;

  //first item
  pheader            := PLargeHeader( NativeUInt(pblock) + SizeOf(TLargeBlockMemory));
  pthreadoffset      := PBaseThreadManagerOffset(NativeUInt(Self.OwnerThread) or 2);
  //pheader.OwnerBlock := pblock;
  pheader.OwnerBlock := pthreadoffset;
  pheader.Size       := iAllocSize - SizeOf(TLargeBlockMemory) - SizeOf(TLargeHeader);

  Result := Pointer(NativeUInt(pheader) + SizeOf(TLargeHeader));

  {$IFDEF SCALEMM_MAGICTEST}
  Assert(pheader.Magic1 = 0);  //not in use!
  pheader.Magic1 := 123456789; //mark in use
  {$ENDIF}
  Assert( NativeUInt(Result) AND 3 = 0);
  {$IFDEF Align8Bytes}
  Assert( NativeUInt(Result) AND 7 = 0);
  {$ENDIF}
  {$IFDEF Align16Bytes}
  Assert( NativeUInt(Result) AND 15 = 0);
  {$ENDIF}

  Inc(FAllocCount);
  Inc(FAllocSize, pblock.Size);
  {
  pblock.PreviousMem := nil;
  pblock.NextMem := FFirstMem;
  if FFirstMem <> nil then
    FFirstMem.PreviousMem := pblock;
  FFirstMem := pblock;
  }
end;

procedure TLargeMemThreadManager.Init;
begin
  SizeType := stLarge;
end;

function TLargeMemThreadManager.ReallocMemWithHeader(aMemory: Pointer;
  aSize: NativeUInt): Pointer;
var
  iAllocSize, iOldSize, iExtraSize: NativeUInt;
  pblock,
  pnextblock : PLargeBlockMemory;
  meminfo: TMemoryBasicInformation;
  pheader: PLargeHeader;
begin
  pblock     := PLargeBlockMemory(NativeUInt(aMemory) - SizeOf(TLargeBlockMemory) - SizeOf(TLargeHeader));
  iAllocSize := aSize + SizeOf(TLargeBlockMemory) + SizeOf(TLargeHeader);
  iOldSize   := pblock.Size - SizeOf(TLargeBlockMemory) - SizeOf(TLargeHeader);

  //upscale?
  if iAllocSize > pblock.Size then
  begin
    //iAllocSize := iAllocSize + (iAllocSize shr 2);       //add 1/4 extra  -> alread done!?
    iAllocSize := (iAllocSize + LargeBlockGranularity) and -LargeBlockGranularity; //round to 64k
    Inc(FAllocSize, iAllocSize - pblock.Size);

    //try to expand current mem (in place)
    pnextblock := PLargeBlockMemory( NativeUInt(pblock) + pblock.Size );
    VirtualQuery(pnextblock, meminfo, SizeOf(meminfo));
    //next mem is free?
    if (meminfo.State = MEM_FREE) then
    begin
      iExtraSize := iAllocSize - pblock.Size;
      //Enough mem to grow in place?
      if (meminfo.RegionSize >= iExtraSize) then
      begin
        {Attempy to reserve the address range (which will fail if another
         thread has just reserved it) and commit it immediately afterwards.}
        if (VirtualAlloc(pnextblock, iExtraSize, MEM_RESERVE, PAGE_READWRITE) <> nil)
          and (VirtualAlloc(pnextblock, iExtraSize, MEM_COMMIT, PAGE_READWRITE) <> nil) then
        begin
          pblock.Size := iAllocSize;
          //update first item size
          pheader      := PLargeHeader( NativeUInt(pblock) + SizeOf(TLargeBlockMemory));
          pheader.Size := iAllocSize - SizeOf(TLargeBlockMemory) - SizeOf(TLargeHeader);

          Result := aMemory;
          Exit;
        end;
      end;
    end;

    Result  := GetMemWithHeader(iAllocSize);
    Move(aMemory^, Result^, iOldSize); // copy (use smaller old size)
    Self.FreeMem(pblock);
  end
  //downscale: less than 1/2? No realloc needed
  else if iAllocSize > (pblock.Size shr 1) then
    Result := aMemory
  else
  //too much downscale
  begin
    Result := PThreadMemManager(Self.OwnerThread).GetMem(iAllocSize); //possible "medium" or "small" mem!
    Move(aMemory^, Result^, aSize); // copy (use smaller new size)
    Self.FreeMem(pblock);
  end;
end;

initialization
  {$IFDEF Align8Bytes}
  Assert( SizeOf(TLargeHeader) AND 7 = 0);
  Assert( SizeOf(TLargeBlockMemory) AND 7 = 0);
  {$ENDIF}
  {$IFDEF Align16Bytes}
  Assert( SizeOf(TLargeHeader) AND 15 = 0);
  Assert( SizeOf(TLargeBlockMemory) AND 15 = 0);
  {$ENDIF}
  Assert( SizeOf(TLargeHeader) = SizeOf(TBaseMemHeader) );

end.
