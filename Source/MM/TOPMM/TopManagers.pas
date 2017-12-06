{****************************************************************************************

TOPMEMORY v3.55 - HIGH PERFORMANCE DELPHI MEMORY MANAGER  (C) 2008 Ivo Tops, Topsoftware

  TopManagers contains;

  TSizeManager   = Manager for blocks of a specific size
  TThreadManager = ThreadMemory Manager with a list of SizeManagers
  TThreadManagerList = All ThreadManagers
  TPoolSM = SizeManager for Pool
  TPoolTM = ThreadManager for Pool

****************************************************************************************}
unit TopManagers;

{$B-}
interface

{$IFNDEF TOPDEBUG} // Debugging off unless you use the TOPDEBUG directive
//{$D-,L-}
{$ENDIF}
{$X+}

{.DEFINE TOPDISABLEPOOL}

uses
  TopCS,
  TopBlocks,
  TopSortedList,
  TopPointerList,
  TopLocalObjects,
  TopLib_CopyMemory,
  Windows;

const
  cMaxManagers = 23;
  cSMIndexQBuf1 = 22;
  cSMIndexQBuf2 = 18;
  cMaxBlockLists = 8;


type
  TOSBlockList = class(TTopPointerList)
  private
    function Get(Index: Integer): TOSBlock; {$IF COMPILERVERSION>=18}inline; {$IFEND}
    procedure Put(Index: Integer; const Value: TOSBlock); {$IF COMPILERVERSION>=18}inline; {$IFEND}
  public
    property Items[Index: Integer]: TOSBlock read Get write Put; default;
  end;

type
  TPoolSMBlock = packed record
    MinBlocks: Integer;
    FullPoolBlocks: Integer;
    List: TTopPointerList;
    Lock: _RTL_CRITICAL_SECTION;
  end;

type
  RQRec = record
    UseBuf: Boolean;
    QMem: Pointer;
    QBlock: TOSBlock;
  end;

const
  cSMIndexSizes: array[0..cMaxManagers] of Cardinal =
  (8, 16, 32, 48, 64, 96, 128, 192, 256, 384, 496, 768, 1008, 1456, 2176, 3120
    , 4368, 8176, 16368, 32752, 65520, 131040, 262080, 0);
  cSMIndexStart: array[0..cMaxManagers] of Byte =
  (64, 60, 56, 52, 48, 44, 40, 36, 32, 28, 24
    , 20, 16, 14, 12, 10, 8, 6, 4, 4, 2, 2, 1, 1);
const
  cSMSizeStop: array[-1..cMaxManagers] of Cardinal =
  (0, 5, 13, 25, 41, 57, 81, 113, 161, 225, 321, 441, 633
    , 889, 1233, 1817, 2649, 3745, 6273, 12273, 24561, 49137, 98281, 196561, MaxCard);
const
  cSMMaxAppBlocks: array[-1..cMaxManagers] of Byte =
  (255, 245, 235, 225, 215, 205, 195, 185, 175, 165, 155, 145, 135
    , 125, 115, 105, 95, 85, 75, 65, 55, 45, 25, 10, 1);


type
  TSizeManagerBase = class(TLocalObject)
  private
    FSMIndex: Byte;
    FAppBlockSize: Cardinal;
    FThreadManager: Pointer;
    FUniqueBlockmode: Boolean;
  protected
    procedure CollectLeaks(const ALeaks: TTopSortedList); virtual; abstract;
  public
    constructor Create(const SMIndex: Byte; const AppBlockSize: Cardinal; const AThreadManager: Pointer; const UniqueBlockMode: Boolean); reintroduce;
    //
    procedure Clear; virtual; abstract;
    //
    property AppBlockSize: Cardinal read FAppBlockSize;
    property SMIndex: Byte read FSMIndex;
    //
    property ThreadManager: Pointer read FThreadManager;

  end;

type
  TSizeManager = class(TSizeManagerBase)
  private
    FBlockToStart: Integer;
    FFullBlocks: Integer;
    FOSBlockList: TOSBlockList;
    FStartAt: Byte;
    QBuf: array[0..1] of RQRec;
    procedure FreeQbuf;
    function AddBlock(const SpecificSize: Cardinal = 0): Integer;
    procedure RemoveBlock(const Index: Integer);
  protected
    procedure CollectLeaks(const ALeaks: TTopSortedList); override;
  public
    constructor Create(const SMIndex: Byte; const AThreadManager: Pointer; const UniqueBlockMode: Boolean = False); reintroduce;
    destructor Destroy; override;
    //
    function SMGetMem(const Size: Cardinal; const AZeroMemory: Boolean = False): Pointer;
    procedure SMFreeMem(const Loc: Pointer; const Block: TOSBlock; const AQBuf: Boolean = True);
    function SMReAllocMem(const Size: Cardinal; const Block: TOSBlock): Pointer;
    function InQBuf(const APointer: Pointer): Boolean; {$IF COMPILERVERSION>=18}inline; {$IFEND}
    //
    procedure Clear; override;
  end;

type
  TSizeManagerList = class(TTopPointerList)
  private
    function Get(Index: Integer): TSizeManagerBase; {$IF COMPILERVERSION>=18}inline; {$IFEND}
    procedure Put(Index: Integer; const Value: TSizeManagerBase); {$IF COMPILERVERSION>=18}inline; {$IFEND}
  public
    property Items[Index: Integer]: TSizeManagerBase read Get write Put; default;
  end;

type
  TPoolSM = class(TSizeManagerBase)
  private
    FBlockList: array[0..cMaxBlockLists - 1] of TPoolSMBlock;
  protected
    procedure ProcessAppBlocksFreedFromOtherThread(const Block: TOSBlock);
    function FreeBlocks(const ListIndex: Byte; const Amount: Integer): Boolean;
    procedure CollectLeaks(const ALeaks: TTopSortedList); override;
  public
    constructor Create(const SMIndex: Byte; const AThreadManager: Pointer; const UniqueBlockMode: Boolean = False); reintroduce;
    destructor Destroy; override;
    // Poolblocks give and take
    procedure AddBlockToPool(const Block: TOSBlock);
    procedure AddBlocksToPool(const Blocks: TOSBlockList);
    function GetEmptyBlock(const StartAt: Byte; out Block: TOSBlock; const NewOwner: Pointer): Boolean; // Block with room, not totally empty
    //
    procedure SMFreeMem(const Loc: Pointer; const Block: TOSBlock);
    //
    procedure LockList(const PoolID: Byte); {$IF COMPILERVERSION>=18}inline; {$IFEND}
    procedure UnLockList(const PoolID: Byte); {$IF COMPILERVERSION>=18}inline; {$IFEND}
    // Maintenance
    procedure ManagePoolSize(const AFreeAll: Boolean = False);
    //
    procedure Clear; override;
  end;

type
  TThreadManager = class(TCSObject)
  private
    FSequenceID: Cardinal;
    FThreadHandle: THandle;
    // List voor SizeManagers
    FSManagerList: TSizeManagerList;
    // List voor gemarkeerde blokken
    FMarkedBlockList: TTopPointerList;
    FBlocksToFree: TTopSortedList;
    // Delphi?
    FDelphi: Boolean;
    procedure CreateList; virtual;
    procedure Destroylist;
    function GetSizeManager(const Size: Cardinal): TSizeManager; {$IF COMPILERVERSION>=18}inline; {$IFEND}
    function GetSizeManagerByIndex(const Index: Byte): TSizeManager; {$IF COMPILERVERSION>=18}inline; {$IFEND}
    function GetIsPoolManager: Boolean;
  public
    constructor Create(const ASequenceID: Cardinal); reintroduce;
    destructor Destroy; override;
    //
    procedure TMFreeMem(const P: Pointer; const OSBlock: TOSBlock); {$IF COMPILERVERSION>=18}inline; {$IFEND}
    function TMGetMem(const Size: Cardinal; const AZeroMemory: Boolean = False): Pointer; {$IF COMPILERVERSION>=18}inline; {$IFEND}
    function TMReallocMem(const P: Pointer; const OSBlock: TOSBlock; const Size: Cardinal): Pointer;
    //
    procedure DataFreedByOtherThreadInBlock(const Block: TOSBlock); {$IF COMPILERVERSION>=18}inline; {$IFEND}
    procedure FreeDataInMarkedBlocks;
    function MarkedBlocksPresent: Boolean; {$IF COMPILERVERSION>=18}inline; {$IFEND}
    //
    procedure Clear; // All Blocks To Pool
    //
    property IsPoolManager: Boolean read GetIsPoolManager;
    property ThreadHandle: THandle read FThreadHandle write FThreadHandle; // Thread using this manager
    property IsDelphiThread: Boolean read FDelphi write FDelphi;
    property SequenceID: Cardinal read FSequenceID;
  end;

type
  TPoolTM = class(TThreadManager)
  private
    procedure CreateList; override;
  protected
    procedure AddBlockToPool(const SMIndex: Byte; const OSBlock: TOSBlock);
    function GetBlockFromPool(const StartAt: Byte; const SMIndex: Byte; const NewOwner: TSizeManager; out OSBlock: TOSBlock): Boolean;
    //
    function GetSizeManager(const Size: Cardinal): TPoolSM; reintroduce; {$IF COMPILERVERSION>=18}inline; {$IFEND}
  public
    function GetSizeManagerByIndex(const Index: Byte): TPoolSM; reintroduce; {$IF COMPILERVERSION>=18}inline; {$IFEND}
    //
    procedure ClearPool; // Free all empty blocks
  end;

type
  TTManagerEntry = packed record
    ThreadManager: TThreadManager;
    FreeList: Integer;
  end;
  TTManagerArray = array[0..MaxInt div (SizeOf(TTManagerEntry)) - 1] of TTManagerEntry;
  PTManagerArray = ^TTManagerArray;

type
  TThreadManagerList = class(TCSObject)
  private
    FFreeListStart: Integer;
    FFreeManagersList: TTopPointerList;
    FAllManagers: TTopPointerList;
    FNonDelphiManagersList: TTopPointerList;
    FLeakList: TTopSortedList;
    FGLobalPool: TPoolTM;
    //
    function AllThreadManagersUsed: Boolean; {$IF COMPILERVERSION>=18}inline; {$IFEND}
    //
    procedure AddNonDelphiManagerToList(const AManager: TThreadManager);
    procedure RemoveNonDelphiManagerFromList(const AManager: TThreadManager);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    //
    procedure TMLReallocMem(const ThreadManager: TThreadManager; const OSBlock: TOSBlock; const P: Pointer; const NewSize: Cardinal; out ReAllocResult: Pointer);
    procedure FreeAppBlockFromOtherThread(const Block: TOSBlock; const Loc: Pointer);
    //
    procedure Clear; // Clean up / Move to pool as much as possible
    //
    function ReserveThreadManager(const ADelphiThread: Boolean): TThreadManager;
    procedure ReleaseThreadManager(const AManager: TThreadManager);
    //
    function RegisterMemoryLeak(P: Pointer): Boolean;
    function UnregisterMemoryLeak(P: Pointer): Boolean;
    procedure CollectLeaks(const ALeaks: TTopsortedList);
    procedure ReportLeaks;
    //
    procedure MarkAsDelphiThread(const AManager: TThreadManager);
    procedure DetectDeadThreads;
    //
    property GlobalPool: TPoolTM read FGlobalPool; // ThreadManager reserved for Pool Actions
  end;

const
  cGrowLen = 30;
var
  cBlockSizes: array[0..cMaxManagers - 1, 0..cGrowLen] of Word;

// windows api call definition
{$EXTERNALSYM OpenThread}
function OpenThread(dwDesiredAccess: DWord; bInheritHandle: Bool; dwThreadID: DWord): THandle; stdcall;
function OpenThread; external kernel32 Name 'OpenThread';

const
  THREAD_QUERY_INFORMATION: Dword = $0040;

implementation

uses
  TopReporting,
  TopInstall,
  TopLib_SSE2,
  SysUtils;

function TSizeManagerList.Get(Index: Integer): TSizeManagerBase;
begin
  Result := inherited Get(Index);
end;

procedure TPoolSM.LockList(const PoolID: Byte);
begin
  EnterCriticalSection(FBlockList[PoolID].Lock);
end;

procedure TPoolSM.UnLockList(const PoolID: Byte);
begin
  LeaveCriticalSection(FBlockList[PoolID].Lock);
end;

function TThreadManager.GetSizeManagerByIndex(const Index: Byte): TSizeManager;
begin
  Result := TSizeManager(FSManagerList[Index]);
end;


function TThreadManager.GetIsPoolManager: Boolean;
begin
  Result := Self = TopMM.GlobalPool;
end;


procedure TSizeManagerList.Put(Index: Integer; const Value: TSizeManagerBase);
begin
  inherited Put(Index, Value);
end;

function TThreadManager.MarkedBlocksPresent: Boolean;
begin
  Result := FMarkedBlockList.Flag = True;
end;

function TPoolTM.GetSizeManager(const Size: Cardinal): TPoolSM;
begin
  Result := TPoolSM(inherited GetSizeManager(Size));
end;

function TThreadManagerList.AllThreadManagersUsed: Boolean;
begin
  Result := FFreeListStart = FFreeManagersList.Count;
end;

{ TOSBlockList }

function TOSBlockList.Get(Index: Integer): TOSBlock;
begin
  Result := inherited Get(Index);
end;

function TSizeManager.InQBuf(const APointer: Pointer): Boolean;
begin
  Result := (QBuf[0].QMem = APointer) or (QBuf[1].QMem = APointer);
end;


procedure TOSBlockList.Put(Index: Integer; const Value: TOSBlock);
begin
  inherited Put(Index, Value);
end;

function TPoolTM.GetSizeManagerByIndex(const Index: Byte): TPoolSM;
begin
  Result := TPoolSM(inherited GetSizeManagerByIndex(Index));
end;

procedure TThreadManager.TMFreeMem(const P: Pointer; const OSBlock: TOSBlock);
begin
  TSizeManager(OSBlock.Sizemanager).SMFreeMem(P, OSBlock);
end;

function TThreadManager.GetSizeManager(const Size: Cardinal): TSizeManager;
begin
  if Size <= cSMSizeStop[9] then
  begin
    if Size <= cSMSizeStop[5] then
    begin
      if Size <= cSMSizeStop[3] then
      begin
        if Size <= cSMSizeStop[1] then
        begin
          if Size <= cSMSizeStop[0] then
            Result := TSizeManager(FSManagerList[0])
          else
            Result := TSizeManager(FSManagerList[1]);
        end else
          if Size <= cSMSizeStop[2] then
            Result := TSizeManager(FSManagerList[2])
          else
            Result := TSizeManager(FSManagerList[3]);
      end else
        if Size <= cSMSizeStop[4] then
          Result := TSizeManager(FSManagerList[4])
        else
          Result := TSizeManager(FSManagerList[5]);
    end else
      if Size <= cSMSizeStop[7] then
      begin
        if Size <= cSMSizeStop[6] then
          Result := TSizeManager(FSManagerList[6])
        else
          Result := TSizeManager(FSManagerList[7]);
      end else
        if Size <= cSMSizeStop[8] then
          Result := TSizeManager(FSManagerList[8])
        else
          Result := TSizeManager(FSManagerList[9]);
  end else
    if Size <= cSMSizeStop[13] then
    begin
      if Size <= cSMSizeStop[11] then
      begin
        if Size <= cSMSizeStop[10] then
          Result := TSizeManager(FSManagerList[10])
        else
          Result := TSizeManager(FSManagerList[11]);
      end else
        if Size <= cSMSizeStop[12] then
          Result := TSizeManager(FSManagerList[12])
        else
          Result := TSizeManager(FSManagerList[13]);
    end else if Size <= cSMSizeStop[17] then
    begin
      if Size <= cSMSizeStop[15] then
      begin
        if Size <= cSMSizeStop[14] then
          Result := TSizeManager(FSManagerList[14])
        else
          Result := TSizeManager(FSManagerList[15]);
      end else
        if Size <= cSMSizeStop[16] then
          Result := TSizeManager(FSManagerList[16])
        else
          Result := TSizeManager(FSManagerList[17]);
    end else
      if Size <= cSMSizeStop[21] then
      begin
        if Size <= cSMSizeStop[19] then
        begin
          if Size <= cSMSizeStop[18] then
            Result := TSizeManager(FSManagerList[18])
          else
            Result := TSizeManager(FSManagerList[19]);
        end else
          if Size <= cSMSizeStop[20] then
            Result := TSizeManager(FSManagerList[20])
          else
            Result := TSizeManager(FSManagerList[21]);
      end else
        if Size <= cSMSizeStop[22] then
          Result := TSizeManager(FSManagerList[22])
        else
          result := TSizeManager(FSManagerList[cMaxManagers]);
end;


function TThreadManager.TMGetMem(const Size: Cardinal; const AZeroMemory: Boolean): Pointer;
begin
  // Check for CrossThread DeAllocations
  if MarkedBlocksPresent then FreeDataInMarkedBlocks;
{$IFDEF TOPDEBUG}
  if IsPoolManager then DebugError('GetMem from Pool is not allowed');
{$ENDIF}
  //
  Result := GetSizeManager(Size).SMGetMem(Size, AZeroMemory);
end;

function TSizeManager.AddBlock(const SpecificSize: Cardinal): Integer;
var
  OSBlock: TOSBlock;
  AppBlockSizeWanted: Integer;
  AppBlocksWanted: Integer;
begin
  // Check size for uniquemode
  if SpecificSize = 0 then
  begin
    AppBlockSizeWanted := FAppBlockSize;
    // every new block larger then the one before
    if FOSBlockList.Count < cGrowLen then AppBlocksWanted := cBlockSizes[FSMIndex, FOSBlockList.Count] else AppBlocksWanted := cBlockSizes[FSMIndex, cGrowLen];
  end
  else
  begin
    AppBlockSizeWanted := SpecificSize;
    AppBlocksWanted := 1;
  end;
  // Try to get block from pool
{$IFNDEF TOPDISABLEPOOL}
  if not TopMM.GlobalPool.GetBlockFromPool(FStartAt, FSMIndex, Self, OSBlock) then
{$ENDIF}
    OSBlock := TOSBlock.Create(FSMIndex, self, APPBlocksWanted, AppBlockSizeWanted, FUniqueBlockmode);
{$IFDEF TOPDEBUG}
  if OSBlock.SMindex <> FSMIndex then
    DebugError('New block has non-matching SizeManager Index');
  if OSBlock.UniqueMode <> FUniqueBlockmode then
    DebugError('New block has non-matching Uniquemode');
{$ENDIF}
  // Add Block to list
  Result := FOSBlockList.Add(OSBlock);
end;


procedure TSizeManager.Clear;
var
  PoolSizeManager: TPoolSM;
begin
  if FOSBlockList.Count > 0 then
  begin
    FreeQBuf;
    //
    // Move all OSBlocks to Pool or free them if pool is filled
    //
    // Get PoolSizeManager for blocks
    PoolSizeManager := TopMM.GlobalPool.GetSizeManagerByIndex(FSMIndex);
    //
{$IFDEF TOPDEBUG}
    if PoolSizeManager.Appblocksize <> FAppblocksize then DebugError('SM.WRPLM');
{$ENDIF}
    // Move the blocks
    PoolSizeManager.AddBlocksToPool(FOSBlockList);
    // Clear the list
    FBlockToStart := 0;
    FFullBlocks := 0;
    FOSBlockList.Clear;
  end;
end;

procedure TSizeManager.CollectLeaks(const ALeaks: TTopSortedList);
var
  I: Integer;
begin
  for I := 0 to FOSBlockList.Count - 1 do
  begin
    with TOSBlock(FOSBlockList[I]) do
    begin
      if not IsEmpty then AddPointersOfAllAppBlocksInUse(ALeaks);
    end;
  end;
end;

constructor TSizeManager.Create(const SMIndex: Byte; const AThreadManager: Pointer; const UniqueBlockMode: Boolean);
begin
  inherited Create(SMIndex, cSMIndexSizes[SMIndex], AThreadManager, UniqueBlockMode);
  FBlockToStart := 0;
  FFullBlocks := 0;
  FOSBlockList := TOSBlockList.Create(True, True); // for performance do not check dupes
  FStartAt := Byte(Random(cMaxBlockLists));
  QBuf[0].QMem := nil;
  QBuf[1].QMem := nil;
  QBuf[0].UseBuf := SMIndex < cSMIndexQBuf1;
  QBuf[1].UseBuf := SMIndex < cSMIndexQBuf2;
end;

procedure TSizeManager.FreeQbuf;
begin
  if assigned(QBuf[0].QMem) then begin SMFreeMem(QBuf[0].QMem, QBuf[0].QBlock, False); QBuf[0].QMem := nil; end;
  if assigned(QBuf[1].QMem) then begin SMFreeMem(QBuf[1].QMem, QBuf[1].QBlock, False); QBuf[1].QMem := nil; end;
end;


destructor TSizeManager.Destroy;
var
  I: Integer;
begin
  FreeQbuf;
  //
  for I := 0 to FOSBlockList.Count - 1 do FOSBlockList[I].Free;
  //
  FOSBlockList.Free;
  //
  inherited Destroy;
end;

procedure TSizeManager.RemoveBlock(const Index: Integer);
begin
{$IFDEF TOPDEBUG}
  if not (FOSBlockList.Count > 0) then
    DebugError('SizeManager has no blocks, invalid removal attempt');
{$ENDIF}
  if FOSBlockList[Index].IsFull then Dec(FFullBlocks);
  // Remove block from local list
  FOSBlockList.DeleteByIndex(Index);
  // Make sure currentblockpointer is still valid
  if (FBlockToStart >= Index) and (FBlockToStart > 0) then Dec(FBlockToStart);
end;

procedure TSizeManager.SMFreeMem(const Loc: Pointer; const Block: TOSBlock; const AQBuf: Boolean);
var
  I: Integer;
begin
{$IFDEF TOPDEBUG}
  if block = nil then DebugError('Invalid block passed to SMFreemem procedure');
{$ENDIF}
  if (not FUniqueBlockmode) then
  begin
//    if AQBuf and (Block.FreeListStart < 3) then // Do not QBuf if we can free the block
//    begin
//      I := 0;
//      if (Assigned(QBuf[0].QMem) and (QBuf[0].QBlock = Block)) then Inc(I);
//      if (Assigned(QBuf[1].QMem) and (QBuf[1].QBlock = Block)) then Inc(I);
//      if (I + 1) = Block.FreeListStart then
//      begin
//        I := 1; // Do not QBuf, free items and then the entire block
//        FreeQbuf;
//      end else I := 0;
//    end else I := 0;
    //
    if AQBuf then
    begin
      if QBuf[0].UseBuf and (not (Assigned(QBuf[0].QMem))) then
      begin
        QBuf[0].QMem := Loc;
        QBuf[0].QBlock := Block;
        Exit;
      end else if QBuf[1].UseBuf and (not (Assigned(QBuf[1].QMem))) then
      begin
        QBuf[1].QMem := Loc;
        QBuf[1].QBlock := Block;
        Exit;
      end;
    end;
  end;
  // Correct fullblockcounter
  if Block.isFull then Dec(FFullBlocks);
  // free the memory
  Block.OBFreeMem(Loc);
  // Indien blok nu leeg misschien OS memory vrijgeven of block naar Pool
  if Block.IsEmpty then
  begin
{$IFDEF TOPDEBUG}
    if not (FFullBlocks <= FOSBlockList.Count) then DebugError('FullBlocks counter is invalid #1');
    if not (FFullBlocks >= 0) then DebugError('FullBlocks counter is invalid #2');
{$ENDIF}
    // In Uniqueblockmode geven we alles terug aan OS (blokken zijn te groot om niet aan OS terug te geven) en houden we de tosblocks zelf
    if (FUniqueBlockmode) then
    begin
      Block.FreeOSBlock;
      if FOSBlockList[FBlockToStart].IsFull then FOSBlockList.Find(Block, FBlockToStart); // Move ptr to empty data if pointing to full block}
    end
    else
    begin // If not uniqueblock we move to pool.
      if FOSBlockList.Find(Block, I) then
      begin
        RemoveBlock(I);
{$IFDEF TOPDISABLEPOOL}
        Block.Free; //  free it (not using the pool)
{$ELSE}
        TopMM.FGlobalPool.AddBlockToPool(FSMIndex, Block); // Locking of pool is done inside this procedure
{$ENDIF}
      end;
    end;
  end
  else // block has free space now, move ptr if current block is full
    if FOSBlockList[FBlockToStart].IsFull then FOSBlockList.Find(Block, FBlockToStart); // Move ptr to empty data if pointing to full block
end;

function TSizeManager.SMGetMem(const Size: Cardinal; const AZeroMemory: Boolean): Pointer;
begin
  Result := nil;
  //
  if (not FUniqueBlockmode) then
  begin
    if (Assigned(QBuf[0].QMem)) then
    begin
      Result := QBuf[0].QMem;
      QBuf[0].QMem := nil;
    end else if (Assigned(QBuf[1].QMem)) then
    begin
      Result := QBuf[1].QMem;
      QBuf[1].QMem := nil;
    end;
  end;
  //
  if Assigned(Result) then
  begin
    if AZeroMemory then TopFillMemory(Pointer(Cardinal(Result) + cAppBlockHeaderSize), Size, 0);
    Exit;
  end;
  // Check if there are blocks with data available and if so find one
  if (FFullBlocks < FOSBlockList.Count) then
  begin
    // Try to get memory from one of the OSBlocks
    repeat
      // Try to allocate
      if not FOSBlockList[FBlockToStart].OBGetMem(Size, Result) then
      begin
        // check for special case where alloc is too large in unique mode
        // and must have failed because of lack of memory
        if FUniqueBlockmode and (not FOSBlockList[FBlockToStart].IsFull) then Exit;
        // If not successfull try next block
        Inc(FBlockToStart);
        if FBlockToStart = FOSBlockList.Count then FBlockToStart := 0;
      end
      else
      begin
        // Statcounters
        if FOSBlockList[FBlockToStart].IsFull then Inc(FFullBlocks);
      end;
    until (Result <> nil);
  end;
  //
  if Result = nil then
  begin
    // Allocate new OS Block for Size
    if FUniqueBlockmode then
      FBlockToStart := AddBlock(Size + cAppBlockHeaderSize + (Size shr 4)) // OverAlloc 6.5%
    else
      FBlockToStart := AddBlock;
    // Get space from fresh new block
    FOSBlockList[FBlockToStart].OBGetMem(Size, Result);
    // Statcounters
    if FOSBlockList[FBlockToStart].IsFull then Inc(FFullBlocks);
  end;
  //
  if AZeroMemory and (Result <> nil) and (not FOSBlockList[FBlockToStart].IsAlreadyZero) then TopFillMemory(Pointer(Cardinal(Result) + cAppBlockHeaderSize), Size, 0);
end;


procedure TThreadManager.CreateList;
begin
  FSManagerList := TSizeManagerList.Create(False, True, cMaxManagers + 1); // Do not check duplicates for performance
  FSManagerList[0] := TSizeManager.Create(0, self);
  FSManagerList[1] := TSizeManager.Create(1, self);
  FSManagerList[2] := TSizeManager.Create(2, self);
  FSManagerList[3] := TSizeManager.Create(3, self);
  FSManagerList[4] := TSizeManager.Create(4, self);
  FSManagerList[5] := TSizeManager.Create(5, self);
  FSManagerList[6] := TSizeManager.Create(6, self);
  FSManagerList[7] := TSizeManager.Create(7, self);
  FSManagerList[8] := TSizeManager.Create(8, self);
  FSManagerList[9] := TSizeManager.Create(9, self);
  FSManagerList[10] := TSizeManager.Create(10, self);
  FSManagerList[11] := TSizeManager.Create(11, self);
  FSManagerList[12] := TSizeManager.Create(12, self);
  FSManagerList[13] := TSizeManager.Create(13, self);
  FSManagerList[14] := TSizeManager.Create(14, self);
  FSManagerList[15] := TSizeManager.Create(15, self);
  FSManagerList[16] := TSizeManager.Create(16, self);
  FSManagerList[17] := TSizeManager.Create(17, self);
  FSManagerList[18] := TSizeManager.Create(18, self);
  FSManagerList[19] := TSizeManager.Create(19, self);
  FSManagerList[20] := TSizeManager.Create(20, self);
  FSManagerList[21] := TSizeManager.Create(21, self);
  FSManagerList[22] := TSizeManager.Create(22, self);
  // Large block handler
  FSManagerList[cMaxManagers] := TSizeManager.Create(cMaxManagers, self, True);
end;

destructor TThreadManager.Destroy;
begin
  DestroyList;
  //
  FMarkedBlockList.Free;
  FBlocksToFree.Free;
  //
  inherited Destroy;
end;

function TThreadManager.TMReallocMem(const P: Pointer; const OSBlock: TOSBlock; const Size: Cardinal): Pointer;
var
  OldSize: Cardinal;
begin
  // Check for CrossThread DeAllocations
  if MarkedBlocksPresent then FreeDataInMarkedBlocks;
{$IFDEF TOPDEBUG}
  if IsPoolManager then
    DebugError('ReAlloc should never be called inside a pool manager block');
  if not ((P <> nil) and (Size > 0)) then
    DebugError('Incorrect Re-Alloc parameters passed');
{$ENDIF}
    // only if realloc is larger then excess space a true realloc is needed.
  OLDSize := OSBlock.AppBlockSize;
  if Oldsize < Size + cAppBlockHeaderSize then
  begin
    // In Uniquemode we now do a OS-ReAlloc as all Blockdata is for this allocation
    if OSBlock.UniqueMode then
    begin
      Result := TSizeManager(OSBlock.SizeManager).SMReAllocMem(Size + cAppBlockHeaderSize + (Size shr 4), OSBlock); // OverAlloc 6.5%
    end
    else
    begin // We have to move to a new block as this block is of wrong size for new alloc
      Result := TMGetMem(Size);
      if Result <> nil then
      begin
        // MemoryCopy
        TopMoveMemory(Pointer(Cardinal(Result) + cAppBlockHeaderSize), Pointer(Cardinal(P) + cAppBlockHeaderSize), OldSize - cAppBlockHeaderSize);
        TMFreeMem(P, OSBlock);
      end;
    end;
  end
  else
  begin
    if Oldsize >  Size + cAppBlockHeaderSize then
    begin
       //  In Uniquemode we now shrink the OSBlock if >50% is returned (we try in place, so no memory move)
      if OSBlock.UniqueMode then begin
        if (OldSize shr 1) > Size then
          Result := TSizeManager(OSBlock.SizeManager).SMReAllocMem(Size + cAppBlockHeaderSize, OSBlock) // No OverAlloc on shrinks
        else
          Result := P;
      end else begin
         // If new alloc is significantly smaller as original (<50%) we move the data to a smaller block (moving memory is a very expensive operation)
        if (OSBlock.AppBlockSize shr 1) >= Size then
        begin
          // Allocate new area
          Result := TMGetMem(Size);
          if Result <> nil then
          begin
            TopMoveMemory(Pointer(Cardinal(Result) + cAppBlockHeaderSize), Pointer(Cardinal(P) + cAppBlockHeaderSize), Size);
            TMFreeMem(P, OSBlock);
          end;
        end else
          Result := P;
      end;
    end else
      Result := P;
  end;
end;

procedure TThreadManager.Clear;
var
  I: Integer;
  R: Integer;
begin
  // Only call this from the context of the thread that has been given this manager
  //
  // Clear all SizeManagers of Excess Stuff
  // Start this loop random in each thread so we are not blocking each other when many threads finish simultaneously
  R := Random(cMaxManagers + 1);
  // All Managers from R to FSManagers
  for I := R to cMaxManagers do FSManagerList[I].Clear;
  // All Managers from  Zero to R - 1
  for I := 0 to R - 1 do FSManagerList[I].Clear;
  // All blocks have gone now, clear Marked Block list
  FMarkedBlockList.Clear;
end;

{ TThreadManagerList }

procedure TThreadManagerList.Clear;
var
  I: Integer;
begin
  // Clear all unused ThreadManagers (frees up some OS data and reclaims crossthread freed memory)
  Lock;
  try
    // Walk list for all UNUSED threadmanagers, others can only be cleared from their own context and will do so upon being released
    for I := FFreeListStart to FFreeManagersList.Count - 1 do
      TThreadManager(FFreeManagersList[I]).Clear;
  finally
    UnLock;
  end;
end;

procedure TThreadManagerList.CollectLeaks(const ALeaks: TTopsortedList);
var
  I, M: Integer;
  lPoolSM: TPoolSM;
  lSM: TSizeManager;
begin
  Lock;
  try
    GLobalPool.Lock;
    try
      // Collect all pointers in use at the moment in the Pool
      with GlobalPool do
      begin
        for I := 0 to cMaxManagers do
        begin
          lPoolSM := GetSizeManagerByIndex(I);
          lPoolSM.CollectLeaks(ALeaks);
        end;
      end;
      // Collect all pointers in use at the moment in the ThreadManagers
      for M := 0 to FAllManagers.Count - 1 do
      begin
        with TThreadManager(FAllManagers[M]) do
        begin
          for I := 0 to cMaxManagers do
          begin
            lSM := GetSizeManagerByIndex(I);
            lSM.CollectLeaks(ALeaks);
          end;
        end;
      end;
    finally
      GlobalPool.Unlock;
    end;
  finally
    UnLock;
  end;
end;

constructor TThreadManagerList.Create;
begin
  inherited Create;
  Randomize;
  FFreeManagersList := TTopPointerList.Create(False, True);
  FNonDelphiManagersList := TTopPointerList.Create(True);
  FLeakList := TTopSortedList.Create(True);
  FFreeListStart := 0;
  FGlobalPool := TPoolTM.Create(0);
  FAllManagers := TTopPointerList.Create(False);
end;

destructor TThreadManagerList.Destroy;
var
  I: Integer;
begin
  Lock;
  try
    // Destroy unused managers, others will be reported as leaks
    for I := FFreeManagersList.Count - 1 downto FFreeListStart do
      TThreadManager(FFreeManagersList[I]).Free;
    // List is freed
  finally
    UnLock;
  end;
  //
  FreeAndNil(FFreeManagersList);
  FreeAndNil(FGlobalPool);
  FreeAndNil(FLeakList);
  FreeAndNil(FNonDelphiManagersList);
  //
  inherited Destroy;
end;

procedure TThreadManagerList.DetectDeadThreads;
var
  I: Integer;
  T: TTopSortedList;
begin
  T := nil;
  try
    Lock;
    try
      // Walk the used managers and check deceased
      for I := 0 to FNonDelphiManagersList.Count - 1 do
      begin
        if not IsThreadAlive(TThreadManager(FNonDelphiManagersList[I]).ThreadHandle) then
        begin
          if not Assigned(T) then T := TTopSortedList.Create(False);
          T.Add(TThreadManager(FNonDelphiManagersList[I]));
        end;
      end;
    finally
      UnLock;
    end;
      // free the deceased ones
    if Assigned(T) then
      for I := 0 to T.Count - 1 do
        ReleaseThreadManager(TThreadManager(T[I].Index));
  finally
    FreeAndNil(T);
  end;
end;

constructor TThreadManager.Create(const ASequenceID: Cardinal);
begin
  inherited Create;
  CreateList;
  FSequenceID := ASequenceID;
  FMarkedBlockList := TTopPointerList.Create(False, True);
  FBlocksToFree := TTopSortedList.Create(False, True);
  ThreadHandle := 0;
  FDelphi := False;
end;

procedure TThreadManagerList.ReleaseThreadManager(const AManager: TThreadManager);
{$IFDEF TOPDEBUG}
var I: Integer;
{$ENDIF}
begin
  if assigned(AManager) then
  begin
{$IFDEF TOPDEBUG}
    for I := FFreeListStart to FFreeManagersList.Count - 1 do
      if TThreadManager(FFreeManagersList[I]) = AManager then
        DebugError('ThreadManager returned twice?!');
{$ENDIF}
    // Move all remaining blocks To Pool
    AManager.Clear;
    // Add To ThreadManagerList FreeList
    Lock;
    try
      Dec(FFreeListStart);
      FFreeManagersList.Items[FFreeListStart] := AManager;
      if not AManager.IsDelphiThread then RemoveNonDelphiManagerFromList(AManager);
    finally
      UnLock;
    end;
  end;
end;

procedure TThreadManagerList.ReportLeaks;
var
  AllLeaks: TTopSortedList;

  procedure RemoveAndReportExpectedLeaks(AAllLeaks: TTopSortedList);
  var
    ExpectedFound, I: Integer;
  begin
    // remove all expected leaks
    ExpectedFound := 0;
    for I := 0 to FLeakList.Count - 1 do
      if AAllLeaks.Delete(FLeakList[I].Index) then
        Inc(ExpectedFound);
    // report number of expected leaks
    if ExpectedFound > 0 then
      ReportString(IntToStr(ExpectedFound) + ' of ' + IntToStr(FLeakList.Count) + ' registered memory leaks found.');
  end;

  function ReportThreads: Boolean; // result TRUE when 1 or more delphi threads still running
  var
    DelphiThreads, NotDelphiThreads: Integer;
  begin
    Result := False;
    // report threads not freed yet
    NotDelphiThreads := FNonDelphiManagersList.Count;
    DelphiThreads := FFreeListStart - 1 - NotDelphiThreads; // -2 because Main and Managerthread must be excluded.
    // report
    if DelphiThreads > 0 then
    begin
      Result := True;
      if DelphiThreads = 1 then
        ReportString('1 Delphi TThread (or descendant) has not finished before Application Exit.')
      else
        ReportString(IntToStr(DelphiThreads) + ' Delphi TThreads (or descendants) have not finished before Application Exit.');
    end;
    if NotDelphiThreads > 0 then
    begin
      if NotDelphiThreads = 1 then
        ReportString('1 Non Delphi Thread is still running at Application Exit (this does not have to be an error)')
      else
        ReportString(IntToStr(NotDelphiThreads) + ' Non Delphi Threads are still running at Application Exit (this does not have to be an error)');
    end;
  end;

begin
  // Do we need to report?
  if ReportMemoryLeaksOnShutdown and (ReportMemoryLeaksToLogFile or ReportMemoryLeaksToIDE) then
  try
    AllLeaks := TTopSortedList.Create(False, True); // for performance do not check for dupes in non sorted list
    try
      Lock;
      try
        // First report all threads that have not returned their memory
        // wait some extra time if there are still delphi threads running
        if ReportThreads then Sleep(250);
        //
        // Collect all leaks (list not yet sorted)
        CollectLeaks(AllLeaks);
        // Sort the list
        AllLeaks.Sorted := True;
        // Remove expected leaks
        RemoveAndReportExpectedLeaks(AllLeaks);
      finally
        UnLock;
      end;
      // Group and report the leaks
      OutputLeaks(AllLeaks);
    finally
      AllLeaks.Free;
    end;
  except
    // Eat Errors as this is not entirely threadsafe (although app should have stopped everything by now)
  end;
end;

function TThreadManagerList.ReserveThreadManager(const ADelphiThread: Boolean): TThreadManager;
begin
  Lock;
  try
    // Try to reserve an existing manager
    if not AllThreadManagersUsed then
      Result := TThreadManager(FFreeManagersList[FFreeListStart])
    else
    begin
      // Make new Manager;
      Result := TThreadManager.Create(FFreeManagersList.Count);
      FFreeManagersList.Add(Pointer(0)); // Create space for when it's returned
      FAllManagers.Add(Result);
    end;
    Inc(FFreeListStart);
    // Add to list if non delphi
    Result.IsDelphiThread := ADelphiThread;
    if not ADelphiThread then AddNonDelphiManagerToList(Result);
  finally
    UnLock;
  end;
end;

procedure TThreadManagerList.AddNonDelphiManagerToList(const AManager: TThreadManager);
begin
  // Make sure we have a lock when calling this routine
  FNonDelphiManagersList.Add(AManager);
  // Get ahandle on it so we can check if it's alive
  try
    AManager.ThreadHandle := OpenThread(THREAD_QUERY_INFORMATION, BOOL(False), GetCurrentThreadID);
  except
    AManager.ThreadHandle := 0;
  end;
end;

procedure TThreadManagerList.RemoveNonDelphiManagerFromList(const AManager: TThreadManager);
var
  Idx: Integer;
begin
  // Make sure we have a lock when calling this routine
  if FNonDelphiManagersList.Find(AManager, Idx) then
  begin
    try
      // close handle
      try
        if AManager.ThreadHandle <> 0 then CloseHandle(AManager.ThreadHandle);
      except
      // ignore errors
      end;
    finally
      FNonDelphiManagersList.DeleteByIndex(Idx);
    end;
  end;
end;


procedure TThreadManagerList.TMLReallocMem(const ThreadManager: TThreadManager; const OSBlock: TOSBlock; const P: Pointer; const NewSize: Cardinal; out ReAllocResult: Pointer);
var
  GetMemResult: Pointer;
  CopySize: Cardinal;
begin
  ReAllocResult := nil;
  // ReAlloc of existing block
  if TSizeManager(OSBlock.SizeManager).ThreadManager = ThreadManager then
  begin
    ReAllocResult := ThreadManager.TMReAllocMem(P, OSBlock, NewSize); // ReAllocResult nil is failure, Result False means block was not in this manager
  end else
  begin
    // copy data if block to small or new size is far below normal size for block
    if (OSBlock.AppBlockSize < NewSize + cAppBlockHeaderSize) or (NewSize < (OSBlock.AppBlockSize shr 2)) then
    begin
      // Claim new space in our own manager, copy data from other block and free in other Manager
      GetMemResult := ThreadManager.TMGetMem(NewSize);
      if GetMemResult <> nil then
      begin
        // Copy all content to new block (or as much as possible if block is smaller)
        // Determine copysize (old size minus alignment shift. can be calculated from pointers)
        CopySize := OSBlock.AppBlockSize - cAppBlockHeaderSize;
        if NewSize < CopySize then
          CopySize := NewSize;
        // MemoryCopy
        TopMoveMemory(Pointer(Cardinal(GetMemResult) + cAppBlockHeaderSize), Pointer(Cardinal(P) + cAppBlockHeaderSize), CopySize);
        // Set result pointer to newly allocated spot
        ReAllocResult := GetMemResult;
        // Free old data
        FreeAppBlockFromOtherThread(OSBlock, P);
      end else ReAllocResult := nil;
    end
    else
      ReAllocResult := P; // Reallocated size fits within already allocated block
  end;
end;

function TThreadManagerList.RegisterMemoryLeak(P: Pointer): Boolean;
begin
  Result := True;
  //
  Lock;
  try
    FLeakList.add(P);
  finally
    UnLock;
  end;
end;

function TThreadManagerList.UnregisterMemoryLeak(P: Pointer): Boolean;
begin
  Lock;
  try
    Result := FLeakList.Delete(P);
  finally
    UnLock;
  end;
end;

procedure TThreadManager.Destroylist;
var
  I: Integer;
begin
  // Free all SizeManagers
  for I := 0 to cMaxManagers do
    FSManagerList[I].Free;
  //
  FSManagerList.Free;
end;

procedure TPoolTM.AddBlockToPool(const SMIndex: Byte; const OSBlock: TOSBlock);
var
  PoolSizeManager: TPoolSM;
  lLockID: Byte;
begin
{$IFDEF TOPDEBUG}
  if (not OSBlock.IsEmpty) then
    DebugError('PM.ABTP');
{$ENDIF}
  // Use AppBlockSize of SizeManager as Block might have been shrunken in uniquemode. Still has to go to Correct SizeManager in pool
  PoolSizeManager := GetSizeManagerByIndex(SMIndex);
  //
  lLockID := OSBlock.PoolID;
  // Upper cap on fully empty blocks in pool
{  if OSBlock.IsEmpty and (PoolSizeManager.FBlockList[lLockID].List.Count - PoolSizeManager.FBlockList[lLockID].FullPoolBlocks > ((cMaxManagers - SMIndex) shl 3)) then
  begin
    OSBlock.Free;
    Exit;
  end;}
  //
  PoolSizeManager.LockList(lLockID);
  try
    PoolSizeManager.AddBlockToPool(OSBlock);
  finally
    PoolSizeManager.UnLockList(lLockID);
  end;
end;

function TPoolTM.GetBlockFromPool(const StartAt: Byte; const SMIndex: Byte; const NewOwner: TSizeManager; out OSBlock: TOSBlock): Boolean;
begin
  // Get Block from SizeManager that has blocks this size
  Result := GetSizeManagerByIndex(SMIndex).GetEmptyBlock(StartAt, OSBlock, NewOwner);
end;

procedure TPoolSM.AddBlockToPool(const Block: TOSBlock);
begin
  // Caller must have size manager locked
  // Block should not be listed as having blocks freed by other thread in current threadmanager
  // Set Pool as New Owner for Block;
  Block.SizeManager := self;
  // Add block to end of list
  FBlockList[Block.PoolID].List.Add(Block);
  // Full block and flag setting
  if Block.IsFull then Inc(FBlockList[Block.PoolID].FullPoolBlocks) else FBlockList[Block.PoolID].List.Flag := True;
end;

function TPoolSM.GetEmptyBlock(const StartAt: Byte; out Block: TOSBlock; const NewOwner: Pointer): Boolean;
var
  X, C: Byte;
  I: Integer;
begin
  // Caller must have pool size manager locked
{$IFDEF TOPDEBUG}
  if (not TThreadManager(FThreadManager).IsPoolManager) then
    DebugError('SM.GEB');
{$ENDIF}
  Result := False;
  // Start op een random positie om zoveel mogelijk niet in elkaars weg te zitten
  X := StartAt;
  C := 0;
  repeat
    if FBlockList[X].List.Flag = True then
    begin
      LockList(X);
      try
        for I := FBlockList[X].List.Count - 1 downto 0 do
          if not TOSBlock(FBlockList[X].List[I]).IsFull then
          begin
            // Niet vol blok gevonden
            Result := True;
            Block := TOSBlock(FBlockList[X].List[I]);
            Block.SizeManager := NewOwner;
            // verwijder uit pool
            FBlockList[X].List.DeleteByIndex(I);
            // Flag setting
            if FBlockList[X].List.Count = FBlockList[X].FullPoolBlocks then FBlockList[X].List.Flag := False;
            // MinMax
            if (FBlockList[X].List.Count - FBlockList[X].FullPoolBlocks) < (FBlockList[X].MinBlocks) then FBlockList[X].MinBlocks := FBlockList[X].List.Count - FBlockList[X].FullPoolBlocks;
            // Ready
            Break;
          end;
      finally
        UnLockList(X);
      end;
    end;
    //
    if not Result then
    begin
      Inc(X);
      if X = cMaxBlockLists then X := 0;
      Inc(C);
    end;
    //
  until (Result = True) or (C = cMaxBlockLists);
end;

procedure TPoolSM.AddBlocksToPool(const Blocks: TOSBlockList);
var
  I: Integer;
begin
  // Should be already locked by caller
{$IFDEF TOPDEBUG}
  if (not TThreadManager(FThreadManager).IsPoolManager) then DebugError('SM.ABSTP');
{$ENDIF}
  // Walk all blocks and add to pool.
  for I := 0 to Blocks.Count - 1 do
  begin
    LockList(Blocks[I].PoolID);
    try
      // Free data in there already marked to be freed from other threads
      if Blocks[I].FreedByOtherThreadBlocks > 0 then ProcessAppBlocksFreedFromOtherThread(Blocks[I]);
      // Move to pool
      AddBlockToPool(Blocks[I]);
    finally
      UnLockList(Blocks[I].PoolID);
    end;
  end;
end;

function TSizeManager.SMReAllocMem(const Size: Cardinal; const Block: TOSBlock): Pointer;
begin
{$IFDEF TOPDEBUG}
  if not Block.UniqueMode then
    DebugError('SM.BNU');
{$ENDIF}
  // ReAlloc data in a Unique mode block
  // If not succeeded we report a nil pointer for the result
  if not Block.OBResize(Size, Result) then Result := nil;
end;

procedure TThreadManager.DataFreedByOtherThreadInBlock(const Block: TOSBlock);
begin
  // Caller must have TM locked
  FMarkedBlockList.Add(Block);
  FMarkedBlockList.Flag := True;
end;

procedure TThreadManager.FreeDataInMarkedBlocks;
var
  Loc: Pointer;
  I: Integer;

  procedure FreeBlockData(const Block: TOSBlock);
  var
    BlocksToDo, J: Integer;
  begin
    BlocksToDo := Block.FreedByOtherThreadBlocks;
    // Set to zero right now because block might be destroyed at end, we won't know
    Block.FreedByOtherThreadBlocks := 0;
    // Andere thread heeft misschien geheugen als vrij aangemerkt. Hier verwerken we dit netjes
    // zoals we ook normale blokken verwerken bij vrijgave.
    for J := BlocksToDo - 1 downto 0 do
    begin
      Loc := Pointer(Cardinal(Block.OSBlockPointer) + Cardinal(Block.FreedByOtherThreadList[J] * Block.AppBlockSize));
        // Zorg dat de laatste niet gedaan wordt (pool niet gelocked, anders deadlock)
      if (J = 0) and (Block.FreeListStart = 1) then
        FBlocksToFree.Add(Loc, Block)
      else
        TSizeManager(Block.SizeManager).SMFreeMem(Loc, Block);
    end;
  end;

begin
  FBlocksToFree.Clear;
  FMarkedBlockList.Flag := False;
  //
  Lock;
  try
    for I := FMarkedBlockList.Count - 1 downto 0 do
    begin
      FreeBlockData(TOSBlock(FMarkedBlockList[I]));
      FMarkedBlockList.DeleteByIndex(I);
    end;
  finally
    UnLock;
  end;
  // Free last appblocks (will move to pool) outside Threadlock (deadlock if within)
  for I := 0 to FBlocksToFree.Count - 1 do
    TSizeManager(TOSBlock(FBlocksToFree[I].Obj).SizeManager).SMFreeMem(Pointer(FBlocksToFree[I].Index), TOSBlock(FBlocksToFree[I].Obj));
end;


procedure TPoolTM.ClearPool;
var
  I: Integer;
begin
  for I := cMaxManagers downto 0 do TPoolSM(FSManagerList[I]).ManagePoolSize(True);
end;

procedure TPoolTM.CreateList;
begin
  FSManagerList := TSizeManagerList.Create(False, True, cMaxManagers + 1);
  FSManagerList[0] := TPoolSM.Create(0, self);
  FSManagerList[1] := TPoolSM.Create(1, self);
  FSManagerList[2] := TPoolSM.Create(2, self);
  FSManagerList[3] := TPoolSM.Create(3, self);
  FSManagerList[4] := TPoolSM.Create(4, self);
  FSManagerList[5] := TPoolSM.Create(5, self);
  FSManagerList[6] := TPoolSM.Create(6, self);
  FSManagerList[7] := TPoolSM.Create(7, self);
  FSManagerList[8] := TPoolSM.Create(8, self);
  FSManagerList[9] := TPoolSM.Create(9, self);
  FSManagerList[10] := TPoolSM.Create(10, self);
  FSManagerList[11] := TPoolSM.Create(11, self);
  FSManagerList[12] := TPoolSM.Create(12, self);
  FSManagerList[13] := TPoolSM.Create(13, self);
  FSManagerList[14] := TPoolSM.Create(14, self);
  FSManagerList[15] := TPoolSM.Create(15, self);
  FSManagerList[16] := TPoolSM.Create(16, self);
  FSManagerList[17] := TPoolSM.Create(17, self);
  FSManagerList[18] := TPoolSM.Create(18, self);
  FSManagerList[19] := TPoolSM.Create(19, self);
  FSManagerList[20] := TPoolSM.Create(20, self);
  FSManagerList[21] := TPoolSM.Create(21, self);
  FSManagerList[22] := TPoolSM.Create(22, self);
  // Large block handler
  FSManagerList[cMaxManagers] := TPoolSM.Create(cMaxManagers, self, True);
end;

constructor TPoolSM.Create(const SMIndex: Byte; const AThreadManager: Pointer; const UniqueBlockMode: Boolean);
var
  I: Integer;
begin
  inherited Create(SMIndex, cSMIndexSizes[SMIndex], AThreadManager, UniqueBlockMode);
  //
  for I := 0 to cMaxBlockLists - 1 do
  begin
    FillChar(FBlockList[I], SizeOf(TPoolSMBlock), 0);
    FBlockList[I].List := TTopPointerList.Create(False, True); // for performance do not check for dupes in non sorted list
    //InitializeCriticalSectionAndSpinCount(FBlockList[I].Lock, 500);
    InitializeCriticalSection(FBlockList[I].Lock);
  end;
end;

constructor TSizeManagerBase.Create(const SMIndex: Byte; const AppBlockSize: Cardinal; const AThreadManager: Pointer; const UniqueBlockMode: Boolean);
begin
  inherited Create;
  FSMIndex := SMIndex;
  FAppBlockSize := AppBlockSize;
  FThreadManager := AThreadManager;
  FUniqueBlockMode := UniqueBlockMode;
end;

procedure TThreadManagerList.FreeAppBlockFromOtherThread(const Block: TOSBlock; const Loc: Pointer);
var
  TM: TThreadManager;
  PoolSM: TPoolSM;
begin
  // Make sure block is not shifting between owners
  PoolSM := TopMM.GlobalPool.GetSizeManagerByIndex(Block.SMIndex);
  PoolSM.LockList(Block.PoolID);
  try
    TM := TThreadManager(TSizeManagerBase(Block.SizeManager).FThreadManager);
    // Free directly if in Pool
    if not TM.IsPoolManager then
    begin
      TM.Lock;
      try
        Block.AddFreedByOtherThreadListBlock(Loc);
        // Also Add Block to ThreadMarkedBlockList if this was the first block added
        if Block.FreedByOtherThreadBlocks = 1 then TThreadManager(TSizeManager(Block.SizeManager).ThreadManager).DataFreedByOtherThreadInBlock(Block);
      finally
        TM.UnLock;
      end;
    end
    else
      PoolSM.SMFreeMem(Loc, Block);
  finally
    PoolSM.UnLockList(Block.PoolID);
  end;
end;

procedure TThreadManagerList.MarkAsDelphiThread(const AManager: TThreadManager);
begin
  Lock;
  try
    RemoveNonDelphiManagerFromList(AManager); // Is Delphi, so Remove
    AManager.IsDelphiThread := True;
  finally
    UnLock;
  end;
end;


//procedure TThreadManagerList.PreAllocThreadManagers;
//var
//  I: Integer;
//begin
//  Lock;
//  try
//    for I := 0 to NumberOfProcessors - 1 do FFreeManagersList.Add(TThreadManager.Create); // Create space for when it's returned
//    FFreeListStart := FFreeListStart + Integer(NumberOfProcessors);
//  finally
//    UnLock;
//  end;
//end;

destructor TPoolSM.Destroy;
var
  I, J: Integer;
begin
  for I := 0 to cMaxBlockLists - 1 do
  begin
    LockList(I);
    try
      for J := 0 to FBlockList[I].List.Count - 1 do
        TOSBlock(FBlockList[I].List[J]).Free;
    finally
      UnLockList(I);
    end;
    DeleteCriticalSection(FBlockList[I].Lock);
    FBlockList[I].List.Free;
  end;
  //
  inherited;
end;


function TPoolSM.FreeBlocks(const ListIndex: Byte; const Amount: Integer): Boolean; // Result True = Has FreeBlocks left
var
  J, BlocksFreed: Integer;
  Block: TOSBlock;
begin
  // Caller should have pool locked
  // Free [Amount] empty blocks
  BlocksFreed := 0;
  //
  for J := FBlockList[ListIndex].List.Count - 1 downto 0 do
  begin
    if BlocksFreed < Amount then
    begin
      Block := TOSBlock(FBlockList[ListIndex].List[J]);
      if Block.IsEmpty then
      begin
        // verwijder uit pool and delete
        FBlockList[ListIndex].List.DeleteByIndex(J);
        Block.Free;
        Inc(BlocksFreed);
      end;
    end else Break; // Freed enough blocks
  end;
  //
  Result := FBlockList[ListIndex].FullPoolBlocks < FBlockList[ListIndex].List.Count;
end;

procedure TPoolSM.ManagePoolSize(const AFreeAll: Boolean);
var
  I: Integer;
begin
  for I := 0 to cMaxBlockLists - 1 do
  begin
    // only lock and work if necessary
    if FBlockList[I].List.Flag then
    begin
      LockList(I);
      try
        // Free minimum amount continually present during last interval
        if AFreeAll then FBlockList[I].MinBlocks := MaxInt;
        //
        FBlockList[I].List.Flag := FreeBlocks(I, FBlockList[I].MinBlocks);
        // reset
        FBlockList[I].MinBlocks := FBlockList[I].List.Count;
      finally
        UnLockList(I);
      end;
    end;
  end;
end;

procedure TPoolSM.ProcessAppBlocksFreedFromOtherThread(const Block: TOSBlock);
var
  I: Integer;
begin
  // Walk blocks
  for I := Block.FreedByOtherThreadBlocks - 1 downto 0 do Block.AddAppblockToFreeList(Block.FreedByOtherThreadList[I]);
  //
  Block.FreedByOtherThreadBlocks := 0;
end;

procedure TPoolSM.Clear;
var I: Integer;
begin
  for I := 0 to cMaxBlockLists - 1 do
  begin
    LockList(I);
    try
      // Free all empty blocks
      FBlockList[I].List.Flag := FreeBlocks(I, MaxInt);
    finally
      UnLockList(I);
    end;
  end;
end;


procedure TPoolSM.CollectLeaks(const ALeaks: TTopSortedList);
var
  I, X: Integer;
begin
  for X := 0 to cMaxBlockLists - 1 do
  begin
    LockList(X);
    try
      for I := 0 to FBlockList[X].List.Count - 1 do
      begin
        with TOSBlock(FBlockList[X].List[I]) do
        begin
          if not IsEmpty then AddPointersOfAllAppBlocksInUse(ALeaks);
        end;
      end;
    finally
      UnLockList(X);
    end;
  end;
end;

procedure TPoolSM.SMFreeMem(const Loc: Pointer; const Block: TOSBlock);
begin
  Block.OBFreeMem(Loc);
  // no longer full, then record stat and set Flag to free block present
  if Block.FreeListStart = Block.AppBlocks - 1 then
  begin
    Dec(FBlockList[Block.PoolID].FullPoolBlocks);
    FBlockList[Block.PoolID].List.Flag := True;
  end;
end;

{procedure InitSizes;
var
  I: Integer;
begin
  cSMSizeStop[-1] := 0;
  for I := 0 to cMaxManagers - 1 do cSMSizeStop[I] := cSMIndexSizes[I] - (cSMIndexSizes[I] shr 2) - cAppBlockHeaderSize;
  cSMSizeStop[cMaxManagers] := MaxCard;
end;}


end.

