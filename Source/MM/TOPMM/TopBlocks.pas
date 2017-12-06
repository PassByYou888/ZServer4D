{****************************************************************************************

  TOPMEMORY v3.55 - HIGH PERFORMANCE DELPHI MEMORY MANAGER  (C) 2008 Ivo Tops, Topsoftware

  TopBlocks wrap OSMemory blocks

****************************************************************************************}
unit TopBlocks;

interface

{$IFNDEF TOPDEBUG} // Debugging off unless you use the TOPDEBUG directive
{$D-,L-}
{$ENDIF}
{$X+}

{$DEFINE TOPALIGN}

uses
  TopLocalObjects,
  TopSortedList,
  TopLib_CopyMemory;

const
  cMaxAppBlocks = 255;
  cBlockHeaderSize = 4;
  cAppBlockHeaderSize = 2;
  cMaxAlignment = 16;
  MaxCard = $FFFFFFFF;

const cFastIndexArray: array[0..cMaxAppBlocks - 1] of byte = (
	0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 
	11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 
	21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 
	31, 32, 33,	34, 35, 36, 37, 38, 39, 40, 
	41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 
	51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 
	61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 
	71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 
	81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 
	91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 
	101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 
	111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 
	121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 
	131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 
	141, 142, 143, 144, 145, 146, 147, 148, 149, 150, 
	151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 
	161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 
	171, 172, 173, 174, 175, 176, 177, 178, 179, 180, 
	181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 
	191, 192, 193, 194, 195, 196, 197, 198, 199, 200, 
	201, 202, 203, 204, 205, 206, 207, 208, 209, 210, 
	211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 
	221, 222, 223, 224, 225, 226, 227, 228, 229, 230, 
	231, 232, 233, 234, 235, 236, 237, 238, 239, 240, 
	241, 242, 243, 244, 245, 246, 247, 248, 249, 250, 
	251, 252, 253, 254);


type
  TByteArray = array[0..MaxInt div (SizeOf(Byte)) - 1] of Byte;
  PByteArray = ^TByteArray;

type
  TBoolArray = array[0..MaxInt div (SizeOf(Boolean)) - 1] of Boolean;
  PBoolArray = ^TBoolArray;

type
  TOSBlock = class(TLocalObject)
  private
    FOSBlockSize: Cardinal;
    FAppBlockSize: Cardinal;
    FOSBlockPointer: Pointer;
    FOSBlockPointerUnAligned: Pointer;
    FFreeListStart: Byte;
    FSizeManager: Pointer;
    FSMIndex: Byte;
    FPoolListID: Byte;
    FAppBlockList: PByteArray;
    FAppBlocks: Byte;
    FVirtual: Boolean;
    FUniqueMode: Boolean;
    FIsAlreadyZero: Boolean;
    FFreedByOtherThreadList: PByteArray;
    FFreedByOtherThreadBlocks: Byte;
    FFreedByOtherThreadListCapacity: Byte;
    procedure SetBlockPointer; {$IF COMPILERVERSION>=18}inline; {$IFEND}
    function Alignment: Cardinal; {$IF COMPILERVERSION>=18}inline; {$IFEND}
    procedure AllocOSBlock;
    function ReAllocOSBlock(const OldSize, NewSize: Cardinal): Boolean;
    function RealOSBlockPointer: Pointer; {$IF COMPILERVERSION>=18}inline; {$IFEND}
    procedure CheckCapacity;
  public
    constructor Create(const SMIndex: Byte; const SizeManager: Pointer; const AppBlocks, AppBlockSize: Cardinal; const Uniquemode: Boolean); reintroduce;
    destructor Destroy; override;
    // For Reporting
    procedure AddPointersOfAllAppBlocksInUse(const AList: TTopsortedList);
    procedure AddAppblockToFreeList(const AppBlock: Cardinal);
    function AddFreedByOtherThreadListBlock(const Loc: Pointer): Boolean;
    //
    procedure FreeOSBlock;
    //
    function OBGetMem(const Size: Cardinal; out Loc: Pointer): Boolean;
    procedure OBFreeMem(const Loc: Pointer); {$IF COMPILERVERSION>=18}inline; {$IFEND}
    function OBResize(const Size: Cardinal; out Loc: Pointer): Boolean; // Resize already allocated data
    //
    property OSBlockPointer: Pointer read FOSBlockPointer;
    property OSBlockSize: Cardinal read FOSBlockSize;
    property AppBlockSize: Cardinal read FAppBlockSize;
    property FreeListStart: Byte read FFreeListStart;
    property AppBlocks: Byte read FAppBlocks;
    //
    // List that contains appblocks freed from other threads and have to be released from our own context
    property FreedByOtherThreadList: PByteArray read FFreedByOtherThreadList;
    property FreedByOtherThreadBlocks: Byte read FFreedByOtherThreadBlocks write FFreedByOtherThreadBlocks;
    //
    property SizeManager: Pointer read FSizeManager write FSizeManager;
    property UniqueMode: Boolean read FUniqueMode;
    property SMIndex: Byte read FSMIndex;
    property PoolID: Byte read FPoolListID;
    //
    function IsEmpty: Boolean; {$IF COMPILERVERSION>=18}inline; {$IFEND}
    function IsFull: Boolean; {$IF COMPILERVERSION>=18}inline; {$IFEND}
    function IsAllocated: Boolean; {$IF COMPILERVERSION>=18}inline; {$IFEND}
    property IsAlreadyZero: Boolean read FIsAlreadyZero write FIsAlreadyZero; // Is the data we have zero'd?
  end;

implementation

uses
  TopManagers,
  TopLib_SSE2,
  Windows,
  TopInstall;


function lMin(const A, B: Integer): Integer; {$IF COMPILERVERSION>=18}inline; {$IFEND}
begin
  if A < B then    Result := A  else    Result := B;
end;

function lMax(const A, B: Integer): Integer; {$IF COMPILERVERSION>=18}inline; {$IFEND}
begin
  if A > B then    Result := A  else    Result := B;
end;

procedure DetermineBlocksizes;
var
  I, J, K: Integer;
  lAppBlockSize, lNewSize: Cardinal;
  lStartCount: Byte;
  lNewCount, lBest: Byte;
  lBestWaste, lWaste: Cardinal;
  lParts: Double;
begin
  for I := 0 to cMaxManagers - 1 do
  begin
    lAppBlockSize := cSMIndexSizes[I];
    lStartCount := cSMIndexStart[I];
    lParts := (cSMMaxAppBlocks[I] - lStartCount) div (cGrowLen - (cMaxManagers - I));
    cBlockSizes[I, 0] := lStartCount;
    for J := 1 to cGrowlen do
    begin
      lNewCount := lMin(cSMMaxAppBlocks[I], Round(lParts * J + lStartCount));
      lNewSize := lNewCount * lAppBlockSize + cBlockHeaderSize + cMaxAlignment;
      if lNewSize >= cVirtualAbove then
      begin
        lBestWaste := MaxCard;
        lBest := lNewCount;
        for K := cBlockSizes[I, J - 1] to lMin(cSMMaxAppBlocks[I], lNewCount + Trunc(lParts + 1)) do
        begin
          lNewSize := Cardinal(K) * lAppBlockSize + cBlockHeaderSize + cMaxAlignment;
          lWaste := lNewSize - ((lNewSize div cWinAllocSize) * cWinAllocSize);
          if lWaste <= lBestWaste then
          begin
            lBestWaste := lWaste;
            lBest := K;
          end;
        end;
        lNewCount := lBest;
      end;
      cBlockSizes[I, J] := lNewCount;
    end;
  end;
end;


function TOSBlock.IsEmpty: Boolean;
begin
  Result := FFreeListStart = 0;
end;

function TOSBlock.IsFull: Boolean;
begin
  Result := FFreeListStart = FAppBlocks;
end;


procedure TOSBlock.AddAppblockToFreeList(const AppBlock: Cardinal);
{$IFDEF TOPDEBUG}
var
  J: Integer;
begin
  if not (FFreeListStart > 0) then
    DebugError('TBL.AAF');
  if not (IsAllocated) then
    DebugError('Block has no OS data allocated so we should not be here trying to free it');
  if not (FFreeListStart > 0) then
    DebugError('All Data in this OS block is already free, application is probably doing double free of same pointer');
{$ELSE}
begin
{$ENDIF}
{$IFDEF TOPDEBUG}
  if not (AppBlock < FAppblocks) then
    DebugError('Pointer to free does not lie in correct range');
  for J := fFreeListStart to FAppblocks - 1 do
    if FAppBlockList[J] = AppBlock then
      DebugError('Memory passed to be freed was already freed!'); //<<- Error in application that uses TopMemory
  // Zero Memory in Debugmode
  FillChar(Pointer(Cardinal(FOSBlockPointer) + Cardinal(AppBlock) * Cardinal(FAppBlockSize))^, FAppBlockSize, 0);
{$ENDIF}
  Dec(FFreeListStart);
  FAppBlockList[FFreeListStart] := AppBlock;
end;

function TOSBlock.Alignment: Cardinal;
begin
{$IFDEF TOPALIGN}
  if FAppBlockSize > cMaxAlignment - 1 then
    Result := cMaxAlignment else
    if FAppBlockSize > 7 then
      Result := 8 else
      if FAppBlockSize > 3 then
        Result := 4
      else
        Result := 1;
{$ELSE}
  Result := 1;
{$ENDIF}
end;

function TOSBlock.RealOSBlockPointer: Pointer;
begin
  if Assigned(FOSBlockPointerUnAligned) then
    Result := Pointer(Cardinal(FOSBlockPointerUnAligned) - cBlockHeaderSize)
  else
    Result := nil;
end;

function TOSBlock.IsAllocated: Boolean;
begin
  Result := FOSBlockPointer <> nil;
end;

procedure TOSBlock.AllocOSBlock;
var
  I: Integer;
  lMark: Cardinal;
begin
{$IFDEF TOPDEBUG}
  if not (FOSBlockPointer = nil) then DebugError('TBL.ALC');
{$ENDIF}
  // Above certain size we use VirtualMemAlloc, smaller sizes use WinHeapManager
  FVirtual := FOSBlockSize > cVirtualAbove;
  // Allocate
  if FOSBlockSize < MaxCard - cBlockHeaderSize - Alignment then
  begin
    for I := 0 to 1 do
    begin
      if FVirtual then
      begin
        FOSBlockPointerUnAligned := TopVirtualMemAlloc(FOSBlockSize + Alignment - 1 + cBlockHeaderSize);
        if assigned(FOSBlockPointerUnAligned) then
        begin
          FOSBlockPointerUnAligned := Pointer(Cardinal(FOSBlockPointerUnAligned) + cBlockHeaderSize);
          FIsAlreadyZero := True; // Virtual Alloc delivers zero'd data
        end;
      end
      else
      begin
        FOSBlockPointerUnAligned := TopLocalMemAlloc(FOSBlockSize + Alignment - 1 + cBlockHeaderSize);
        if assigned(FOSBlockPointerUnAligned) then
        begin
          FOSBlockPointerUnAligned := Pointer(Cardinal(FOSBlockPointerUnAligned) + cBlockHeaderSize);
          FIsAlreadyZero := False;
        end;
      end;
      // If Alloc Fails we clear the pool and try again
      if not assigned(FOSBlockPointerUnAligned) then TopMM.GlobalPool.ClearPool else Break;
    end;
  end else FOSBlockPointerUnAligned := nil;
  //
  if FOSBlockPointerUnAligned <> nil then
  begin
    FOSBlockPointer := Pointer(Cardinal(FOSBlockPointerUnAligned) + Alignment - ((Cardinal(FOSBlockPointerUnAligned) + cAppBlockHeaderSize - 1) mod Alignment) - 1);
    SetBlockPointer;
  end
  else
    FOSBlockPointer := nil;
  // NIL result in Pointer on alloc will be raised as an exception by Delphi later
  //
  if FOSBlockPointer <> nil then
  begin
    // Mark our blockheaders
    lMark := Cardinal(FOSBlockPointer);
    for I := 0 to FAppBlocks - 1 do
    begin
      Word(Pointer(lMark)^) := Word(Word(SMIndex) or (Word(I) shl 8));
      lMark := lMark + FAppBlockSize;
    end;
  end;
end;

procedure TOSBlock.FreeOSBlock;
begin
{$IFDEF TOPDEBUG}
  // Normally the Block is empty when this routine is called. If not we are probably closing down and
  // this is a memory leak in the program that uses TopMM and should be fixed there.
  if not (IsEmpty) then
    DebugError('Attempt to free OS block with allocated data in it'); // <<-- Memory not yet freed by application that uses TopMemory!
{$ENDIF}
  // Release memory to OS
  if (IsAllocated) then
  begin
    // Free OSMemory in block
    if FVirtual then
      TopVirtualMemFree(RealOSBlockPointer)
    else
      TopLocalMemFree(RealOSBlockPointer);
    //
    FOSBlockPointerUnAligned := nil;
    FOSBlockPointer := nil;
  end;
end;

constructor TOSBlock.Create(const SMIndex: Byte; const SizeManager: Pointer; const AppBlocks, AppBlockSize: Cardinal; const Uniquemode: Boolean);
begin
  inherited Create;
  // Check input
{$IFDEF TOPDEBUG}
  if not (AppBlockSize > 0) then
    DebugError('AppBlockSize must be above zero');
  if not (SizeManager <> nil) then
    DebugError('SizeManager must not be nil');
{$ENDIF}
  //
  FSMIndex := SMIndex;
  FVirtual := False;
  FSizeManager := SizeManager;
  FOSBlockPointer := nil;
  FAppBlockSize := AppBlockSize;
  FPoolListID := Random(cMaxBlockLists);
  //
  FUniqueMode := Uniquemode;
  //
  if FUniqueMode then
    FAppBlocks := 1
  else
    FAppBlocks := AppBlocks;
  //
  FOSBlockSize := FAppBlocksize * FAppBlocks;
  // List for crossthread stuff
  FFreedByOtherThreadBlocks := 0;
  FFreedByOtherThreadListCapacity := 0;
  FFreedByOtherThreadList := nil;
  //
  FAppBlockList := TopLocalMemAlloc((FAppBlocks) * (SizeOf(Byte)));
  // List with Free blocks
  FFreeListStart := 0;
  TopMoveMemory(@FAppBlockList[0], @cFastIndexArray[0], FAppBlocks * SizeOf(Byte)); //for I := FFreeListStart to FAppBlocks - 1 do  FAPPBlockList[I] := I;
  // Allocate data needed
  AllocOSBlock;
end;

destructor TOSBlock.Destroy;
begin
  FreeOSBlock;
  //
  TopLocalMemFree(FAppBlockList);
  //
  if FFreedByOtherThreadList <> nil then TopLocalMemFree(FFreedByOtherThreadList);
  //
  inherited Destroy;
end;

procedure TOSBlock.CheckCapacity;
var
  FixByteMax: Integer;
begin
  // Allocate extra capacity if needed
  if FFreedByOtherThreadBlocks > FFreedByOtherThreadListCapacity then
  begin
    // We have a Byte Maximum that the generic routine does not take into account. It might allocate > 255
    FixByteMax := FFreedByOtherThreadListCapacity;
    // Enlarge data area
    FFreedByOtherThreadList := FixCapacity(Pointer(FFreedByOtherThreadList), FixByteMax, SizeOf(Byte));
    // Store new maximum capacity. Excess will not be used
    if FixByteMax >= cMaxAppBlocks then
      FFreedByOtherThreadListCapacity := cMaxAppBlocks
    else
      FFreedByOtherThreadListCapacity := FixByteMax;
  end;
end;

procedure TOSBlock.OBFreeMem(const Loc: Pointer);
begin
  // Add AppBlock to Freelist
  AddAppblockToFreeList((Cardinal(Loc) - Cardinal(FOSBlockPointer)) div Cardinal(FAppBlockSize));
  FIsAlreadyZero := False;
end;

function TOSBlock.OBGetMem(const Size: Cardinal; out Loc: Pointer): Boolean;
var
  SizeChanged: Boolean;
  OldSize: Cardinal;
begin
{$IFDEF TOPDEBUG}
  if Size > FAppBlockSize - cAppBlockHeaderSize then
    DebugError('TBL.SFA');
{$ENDIF}
  Loc := nil;
  SizeChanged := False;
  //
  // Check if there are still free appblocks available
  Result := not IsFull;
  // Indien er een blok vrij is dan pakken we dat
  if Result then
  begin
    if not IsAllocated then
    begin
      // Alloc mem
      // check if size has to be changed (if this is uniqueblockmode)
      if (Size <> FAppblocksize - cAppBlockHeaderSize) and (FUniqueMode) then
      begin
        FOSBlockSize :=  Size + cAppBlockHeaderSize;
        FAppBlockSize := Size + cAppBlockHeaderSize;
      end;
      AllocOSBlock;
    end
    else
    begin
      // we are allocated
      // check if size has to be changed (if this is uniqueblockmode)
      if (Size > FAppblocksize - cAppBlockHeaderSize) and (FUniqueMode) then
      begin
        OldSize := FOSBlockSize - cAppBlockHeaderSize;
        FAppBlockSize :=  Size + cAppBlockHeaderSize;
        if not ReAllocOSBlock(OldSize, Size) then
        begin
          // ReAlloc failed, new pointer will be nill, existing data is still present
          FAppBlockSize := OldSize;
          Result := False;
          Exit;
        end;
        SizeChanged := True;
      end;
    end;
    // Pointer to first free appblock within osblock and then move the listpointer to the next free block
    if IsAllocated then
    begin
      if SizeChanged then
      begin
        Loc := FOSBlockPointer;
        FFreeListStart := 1; // Fixed for Full Uniqueblocks
      end
      else
      begin
        Loc := Pointer(Cardinal(FOSBlockPointer) + Cardinal(FAppBlockList[FFreeListStart] * (FAppBlockSize)));
        // Word(Loc^) := Word(Word(SMIndex) or (Word(FAppBlockList[FFreeListStart]) shl 8)); already done once in alloc of block
        Inc(FFreeListStart);
      end;
    end else
      Result := False; // alloc failed (unique mode)
  end;
{$IFDEF TOPDEBUG}
  if not ((Loc = nil) or (Cardinal(Loc) >= Cardinal(FOSBlockPointer))) then
    DebugError('Error #1 getting memory');
  if not ((Loc = nil) or (Cardinal(Loc) <= Cardinal(FOSBlockPointer) + Cardinal(OSBlockSize - FAppblocksize))) then
    DebugError('Error #2 getting memory');
{$ENDIF}
end;

function TOSBlock.ReAllocOSBlock(const OldSize, NewSize: Cardinal): Boolean;
var
  OldPointer, OldPointerUnAligned: Pointer;
  AlignDiff: Integer;
  CopySize: Cardinal;
begin
  Result := True;
  //
  OldPointer := FOSBlockPointer;
  OldPointerUnAligned := FOSBlockPointerUnAligned;
  FOSBlockSize := NewSize;
  if FVirtual then
  begin
    FOSBlockPointerUnAligned := TopVirtualMemReAlloc(RealOSBlockPointer, OldSize + Alignment - 1 + cBlockHeaderSize, FOSBlockSize + Alignment - 1 + cBlockHeaderSize);
    if assigned(FOSBlockPointerUnAligned) then FOSBlockPointerUnAligned := Pointer(Cardinal(FOSBlockPointerUnAligned) + cBlockHeaderSize);
  end
  else
  begin
    FOSBlockPointerUnAligned := TopLocalMemReAlloc(RealOSBlockPointer, FOSBlockSize + Alignment - 1 + cBlockHeaderSize, OldSize + Alignment - 1 + cBlockHeaderSize);
    if assigned(FOSBlockPointerUnAligned) then FOSBlockPointerUnAligned := Pointer(Cardinal(FOSBlockPointerUnAligned) + cBlockHeaderSize);
  end;
  // On failure return to old situation
  if FOSBlockPointerUnAligned = nil then
  begin
    Result := False;
    FOSBlockSize := OldSize;
    FOSBlockPointerUnAligned := OldPointerUnAligned; // Old memory stays if realloc failed
  end else // Inform PLL of new Situation
  begin
    FOSBlockPointer := Pointer(Cardinal(FOSBlockPointerUnAligned) + Alignment - ((Cardinal(FOSBlockPointerUnAligned) + cAppBlockHeaderSize - 1) mod Alignment) - 1);
    // Check if re-allocated memory has different alignment. If so, bad luck and we have to move the data
    AlignDiff := Integer(Cardinal(OldPointer) - Cardinal(OldPointerUnAligned)) - Integer((Cardinal(FOSBlockPointer) - Cardinal(FOSBlockPointerUnAligned)));
    if AlignDiff <> 0 then
    begin
      CopySize := OldSize;
      if NewSize < OldSize then CopySize := NewSize;
      if AlignDiff > 0 then
        TopMoveMemory(FOSBlockPointer, Pointer(Cardinal(FOSBlockPointer) + Cardinal(AlignDiff)), CopySize)
      else
        TopMoveMemory(FOSBlockPointer, Pointer(Cardinal(FOSBlockPointer) - Cardinal(-AlignDiff)), CopySize);
    end;
  end;
  // Info PLL
  if IsAllocated then SetBlockPointer;
end;


procedure TOSBlock.SetBlockPointer;
begin
  Cardinal(Pointer(Cardinal(FOSBlockPointer) - cBlockHeaderSize)^) := Cardinal(Self);
end;

function TOSBlock.AddFreedByOtherThreadListBlock(const Loc: Pointer): Boolean;
begin
  // We do not know whether the block is used and cannot peek because the thread is running
  // Only error we detect is if too many blocks have been freed
  Result := FFreedByOtherThreadBlocks < cMaxAppBlocks;
  if Result then // Add to list to be freed later by the thread
  begin
    Inc(FFreedByOtherThreadBlocks);
    CheckCapacity;
    FFreedByOtherThreadList[FFreedByOtherThreadBlocks - 1] := Cardinal(Cardinal(Loc) - Cardinal(FOSBlockPointer)) div (FAppBlockSize);
  end;
end;

procedure TOSBlock.AddPointersOfAllAppBlocksInUse(const AList: TTopsortedList);
var
  I: Integer;
  BlockList: PByteArray;
  PoolSM: TPoolSM;
begin
  // We do not have a list of Used Blocks, Only of Free Blocks. Do some looping to make it
  BlockList := TopLocalMemZeroAlloc((FAppBlocks) * (SizeOf(Byte)));
  try
    // Blocks ready to be issued are free
    for I := FreeListStart to FAppBlocks - 1 do
      BlockList[FAppBlockList[I]] := 1; // Mark as Free;
    // Blocks in the ToBeFreed List are Officialy Free. Lock and Look at them
    if FFreedByOtherThreadBlocks > 0 then
    begin
      // Make sure block is not shifting between owners
      PoolSM := TopMM.GlobalPool.GetSizeManagerByIndex(SMIndex);
      PoolSM.LockList(PoolID);
      try
        for I := 0 to FFreedByOtherThreadBlocks - 1 do
          BlockList[FFreedByOtherThreadList[I]] := 1; // Mark as Free;
      finally
        PoolSM.UnLockList(PoolID);
      end;
    end;
    // report the leaks
    for I := 0 to FAppBlocks - 1 do
      if BlockList[I] = 0 then // Check if not in the QBuf
        if not (TSizeManager(SizeManager).InQBuf(Pointer(Cardinal(Cardinal(FOSBlockPointer) + Cardinal(I) * FAppBlockSize)))) then
         AList.Add(Cardinal(Cardinal(FOSBlockPointer) + Cardinal(I) * FAppBlockSize+cAppBlockHeaderSize), Pointer(FAppBlockSize));
  finally
    TopLocalMemFree(BlockList);
  end;
end;

function TOSBlock.OBResize(const Size: Cardinal; out Loc: Pointer): Boolean;
var OldSize: Cardinal;
begin
{$IFDEF TOPDEBUG}
  if (not FUniqueMode) or (not IsFull) then
    DebugError('Resize only possible for unique mode already allocated blocks');
{$ENDIF}
  //
  OldSize := FOSBlockSize;
  FAppBlockSize := Size;
  //
  Result := ReAllocOSBlock(OldSize, Size);
  if not Result then
  begin
    // ReAlloc failed, existing data is still present
    FAppBlockSize := OldSize;
    Result := False;
  end;
  //
  Loc := FOSBlockPointer;
end;


initialization
  DetermineBlocksizes;

end.

