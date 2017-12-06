{****************************************************************************************

  TOPMEMORY v1.52 - HIGH PERFORMANCE DELPHI MEMORY MANAGER  (C) 2004 Ivo Tops, Topsoftware

  TopBlocks wrap OSMemory blocks

****************************************************************************************}
unit TopBlocks;

interface

uses
  TopLocalObjects,TopCopyMemory;

const
  {$IFDEF TOPSPEED  }
  cMaxAppBlocks = 255; // MaxNumber of Appblocks in an OSBlock (also restricted by using Byte for some structures)
  {$ELSE}
  cMaxAppBlocks = 128;
  {$ENDIF}

type
  TBoolArray = array[0..MaxInt div (SizeOf(Boolean)) - 1] of Boolean;
  PBoolArray = ^TBoolArray;

type
  TOSBlock = class(TLocalObject)
  private
    FOSBlockSize: Integer;
    FAppBlockSize: Integer;
    FOSBlockPointer: Pointer;
    FFreeListStart: Byte;
    FSizeManager: Pointer;
    FSMIndex: Byte;
    FPoolListID: Byte;
    FAppBlockList: PByteArray;
    FAppBlocks: Byte;
    FVirtual: Boolean;
    FUniqueMode: Boolean;
    FFreedByOtherThreadList: PByteArray;
    FFreedByOtherThreadBlocks: Byte;
    FFreedByOtherThreadListCapacity: Byte;
    procedure AllocOSBlock;
    function ReAllocOSBlock(const OldSize, NewSize: Integer): Boolean;
    function AddAppblockToFreeList(const AppBlock: Cardinal): Boolean;
    function RoundAppBlocks(const AppBlocks: Integer; const AppBlockSize: Integer): Byte;
    procedure CheckCapacity;
  public
    constructor Create(const SMIndex: Byte; const SizeManager: Pointer; const AppBlocks, AppBlockSize: Integer; const Uniquemode: Boolean); reintroduce;
    destructor Destroy; override;
    //
    function AddFreedByOtherThreadListBlock(const Loc: Pointer): Boolean;
    //
    procedure FreeOSBlock;
    //
    function OBGetMem(const Size: Integer; out Loc: Pointer): Boolean;
    function OBFreeMem(const Loc: Pointer): Boolean; // True if block is empty after free
    function OBResize(const Size: Integer; out Loc: Pointer): Boolean; // Resize already allocated data
    //
    property OSBlockPointer: Pointer read FOSBlockPointer;
    property OSBlockSize: Integer read FOSBlockSize;
    property AppBlockSize: Integer read FAppBlockSize;
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
    function IsEmpty: Boolean;
    function IsFull: Boolean;
    function IsAllocated: Boolean;
  end;

implementation

uses Windows,
  TopManagers,
  TopInstall
{$IFDEF TOPDEBUG}
  , TopPointerLookupList
{$ENDIF};

function TOSBlock.AddAppblockToFreeList(const AppBlock: Cardinal): Boolean;
begin
{$IFDEF TOPDEBUG}
  if not (FFreeListStart > 0) then
    DebugError('TML.GTM');
{$ENDIF}
  Result := FFreeListStart > 0;
  if Result then
  begin
    Dec(FFreeListStart);
    FAppBlockList[FFreeListStart] := AppBlock;
  end;
end;

procedure TOSBlock.AllocOSBlock;
begin
  // Check if not already allocated
  if not IsAllocated then
  begin
    // Above certain size we use VirtualMemAlloc, smaller sizes use WinHeapManager
    FVirtual := FOSBlockSize > cVirtualAbove;
    // Allocate
    if FVirtual then
      FOSBlockPointer := TopVirtualMemAlloc(FOSBlockSize)
    else
      FOSBlockPointer := TopLocalMemAlloc(FOSBlockSize);
    // NIL result in Pointer on alloc will be raised as an exception by Delphi later
    //
    // Inform PLL if something changed concerning OSBlocks
    if IsAllocated then
      TThreadManager(TSizeManager(FSizeManager).ThreadManager).PLL.AddBlock(self, False);
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
    // Inform PLL block is no longer allocated (before actual removal or the memory could be re-issued in between multithreading)
    TThreadManager(TSizeManager(FSizeManager).ThreadManager).PLL.RemoveBlock(FOSBlockPointer, False);
    // Free OSMemory in block
    if FVirtual then
      TopVirtualMemFree(FOSBlockPointer)
    else
      TopLocalMemFree(FOSBlockPointer);
    //
    FOSBlockPointer := nil;
  end;
end;

constructor TOSBlock.Create(const SMIndex: Byte; const SizeManager: Pointer; const AppBlocks, AppBlockSize: Integer; const Uniquemode: Boolean);
var
  I: Integer;
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
    FAppBlocks := RoundAppBlocks(AppBlocks, AppBlockSize);
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
  for I := FFreeListStart to FAppBlocks - 1 do
    FAPPBlockList[I] := I;
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
var FixByteMax: Integer;
begin
  // Allocate extra capacity if needed
  if FFreedByOtherThreadBlocks > FFreedByOtherThreadListCapacity then
  begin
    // We have a Byte Maximum that the generic routine does not take into account. It might allocate > 255
    FixByteMax := FFreedByOtherThreadListCapacity;
    // Enlarge data area
    FFreedByOtherThreadList := FixCapacity(Pointer(FFreedByOtherThreadList), FixByteMax, SizeOf(Byte));
    // Store new maximum capacity. Excess will not be used
    if FixByteMax > cMaxAppBlocks then
      FFreedByOtherThreadListCapacity := cMaxAppBlocks
    else
      FFreedByOtherThreadListCapacity := FixByteMax;
  end;
end;

function TOSBlock.OBFreeMem(const Loc: Pointer): Boolean; // returns true if osblock is no longer full
var
  I: Integer;
{$IFDEF TOPDEBUG}
  J: Integer;
{$ENDIF}
begin
{$IFDEF TOPDEBUG}
  if not (IsAllocated) then
    DebugError('Block has no OS data allocated so we should not be here trying to free it');
  if not (FFreeListStart > 0) then
    DebugError('All Data in this OS block is already free, application is probably doing double free of same pointer');
{$ENDIF}
  // Bepalen waar dit blok in de lijst zit
  I := Integer(Cardinal(Loc) - Cardinal(FOSBlockPointer)) div (FAppBlockSize);
{$IFDEF TOPDEBUG}
  if not ((I >= 0) and (I < FAppblocks)) then
    DebugError('Pointer to free does not lie in correct range');
  for J := fFreeListStart to FAppblocks - 1 do
    if FAppBlockList[J] = I then
      DebugError('Memory passed to be freed was already freed!'); //<<- Error in application that uses TopMemory
{$ENDIF}
   // Add AppBlock to Freelist
  Result := AddAppblockToFreeList(I);
end;

function TOSBlock.OBGetMem(const Size: Integer; out Loc: Pointer): Boolean;
var
  SizeChanged: Boolean;
  OldSize: Integer;
begin
  Loc := nil;
  SizeChanged := False;
  //
  // Check if there are still free appblocks available
  Result := not IsFull;
  // Indien er een blok vrij is dan pakken we dat
  if Result then
  begin
    if FOSBlockPointer = nil then
    begin
      // Alloc mem
      // check if size has to be changed (if this is uniqueblockmode)
      if (Size <> Fappblocksize) and (FUniqueMode) then
      begin
        FOSBlockSize := Size;
        FAppBlockSize := Size;
      end;
      AllocOSBlock;
    end
    else
    begin
      // we are allocated
      // check if size has to be changed (if this is uniqueblockmode)
      if (Size <> Fappblocksize) and (FUniqueMode) then
      begin
        OldSize := FOSBlockSize;
        FAppBlockSize := Size;
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
        Loc := FOSBlockPointer
      else
      begin
        Loc := Pointer(Cardinal(FOSBlockPointer) + Cardinal(FAppBlockList[FFreeListStart] * (FAppBlockSize)));
        Inc(FFreeListStart);
      end;
    end;
  end;
{$IFDEF TOPDEBUG}
  if not ((Loc = nil) or (Cardinal(Loc) >= Cardinal(FOSBlockPointer))) then
    DebugError('Error #1 getting memory');
  if not ((Loc = nil) or (Cardinal(Loc) <= Cardinal(FOSBlockPointer) + Cardinal(OSBlockSize - FAppblocksize))) then
    DebugError('Error #2 getting memory');
{$ENDIF}
end;

function TOSBlock.IsEmpty: Boolean;
begin
  Result := FFreeListStart = 0;
end;

function TOSBlock.IsFull: Boolean;
begin
  Result := FFreeListStart = FAppBlocks;
end;

function TOSBlock.IsAllocated: Boolean;
begin
  Result := FOSBlockPointer <> nil;
end;

function TOSBlock.ReAllocOSBlock(const OldSize, NewSize: Integer): Boolean;
var
  OldPointer: Pointer;
begin
  Result := True;
  OldPointer := FOSBlockPointer;
  // Remove block from PLL. We might free some memory during re-alloc and if the block is
  // still in the PLL Topmm will think it is still our memory if windows has quickyl re-issued the memory
  TThreadManager(TSizeManager(FSizeManager).ThreadManager).PLL.RemoveBlock(OldPointer, False);
  //
  FOSBlockSize := NewSize;
  if FVirtual then
    FOSBlockPointer := TopVirtualMemReAlloc(FOSBlockPointer, OldSize, FOSBlockSize)
  else
    FOSBlockPointer := TopLocalMemReAlloc(FOSBlockPointer, FOSBlockSize);
  // On failure return to old situation
  if FOSBlockPointer = nil then
  begin
    Result := False;
    FOSBlockSize := OldSize;
    FOSBlockPointer := OldPointer; // Old memory stays if realloc failed
  end;
  // Inform PLL of new Situation
  if IsAllocated then
    TThreadManager(TSizeManager(FSizeManager).ThreadManager).PLL.AddBlock(self, False);
end;

function TOSBlock.RoundAppBlocks(const AppBlocks: Integer; const AppBlockSize: Integer): Byte;
var
  MemSize, MemSizeOk: Integer;
begin
  // Guard Boundary of byte
  if AppBlocks < cMaxAppBlocks then
    Result := AppBlocks
  else
    Result := cMaxAppBlocks;
  //
  MemSize := Result * AppBlockSize;
  MemSizeOk := (MemSize div cWinPageSize) * cWinPageSize;
  // Onyl roundof if will be allocated in virtual memory. For Heap this is not necessary
  if (MemSize > cVirtualAbove) and (Result > 1) and (MemSize > MemSizeOk) then
  begin
    repeat
      Dec(Result);
      MemSize := MemSize - AppBlockSize;
    until (Result = 1) or (MemSize <= MemSizeOk);
  end;
end;

function TOSBlock.AddFreedByOtherThreadListBlock(const Loc: Pointer): Boolean;
var Index: Integer;
begin
  // TM should be locked
  Index := Integer(Cardinal(Loc) - Cardinal(FOSBlockPointer)) div (FAppBlockSize);
  // We do not know whether the block is used and cannot peek because the thread is running
  // Only error we detect is if too many blocks have been freed
  Result := FFreedByOtherThreadBlocks < cMaxAppBlocks;
  if Result then // Add to list to be freed later by the thread
  begin
    Inc(FFreedByOtherThreadBlocks);
    CheckCapacity;
    FFreedByOtherThreadList[FFreedByOtherThreadBlocks - 1] := Index;
  end;
end;

function TOSBlock.OBResize(const Size: Integer; out Loc: Pointer): Boolean;
var OldSize: Integer;
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

end.

