{****************************************************************************************

  TOPMEMORY v1.51 - HIGH PERFORMANCE DELPHI MEMORY MANAGER  (C) 2004 Ivo Tops, Topsoftware

  TopManagers contains;

  TSizeManager   = Manager for blocks of a specific size
  TThreadManager = ThreadMemory Manager with a list of SizeManagers
  TThreadManagerList = All ThreadManagers
  TPoolSM = SizeManager for Pool
  TPoolTM = ThreadManager for Pool

****************************************************************************************}
unit TopManagers;

interface

uses
  TopCS,
  TopBlocks,
  TopSortedList,
  TopLocalObjects,
  TopCopyMemory,
  TopPointerLookupList,
  Windows;

const
  cMaxManagers = 19;
  cMaxBlockLists = 4;

type
  TOSBlockList = class(TTopSortedList)
  private
    function Get(Index: Integer): TOSBlock;
    procedure Put(Index: Integer; const Value: TOSBlock);
  public
    property Items[Index: Integer]: TOSBlock read Get write Put; default;
  end;

type
  TPoolSMBlock = packed record
    MinBlocks: Integer;
    FullBlocks: Integer;
    List: TTopsortedList;
    Lock: _RTL_CRITICAL_SECTION;
  end;

type
  TSizeManagerBase = class(TLocalObject)
  private
    FSMIndex: Byte;
    FAppBlockSize: Integer;
    FThreadManager: Pointer;
    FUniqueBlockmode: Boolean;
  protected
    procedure FreeBlock(const Block: TOSBlock);
  public
    constructor Create(const SMIndex: Byte; const AppBlockSize: Integer; const AThreadManager: Pointer; const UniqueBlockMode: Boolean); reintroduce;
    //
    procedure Clear; virtual; abstract;
    //
    property AppBlockSize: Integer read FAppBlockSize;
    property SMIndex: Byte read FSMIndex;
    //
    property ThreadManager: Pointer read FThreadManager;

  end;

type
  TSizeManager = class(TSizeManagerBase)
  private
    FBlockToStart: Integer;
    FInitialAppBlocks: Integer;
    FFullBlocks: Integer;
    FOSBlockList: TOSBlockList;
    FStartAt: Byte;
    function AddBlock(const SpecificSize: Integer = 0): Integer;
    procedure RemoveBlock(const Index: Integer);
  public
    constructor Create(const SMIndex: Byte; const AThreadManager: Pointer; const AppBlockSize: Integer; const AppBlocks: Integer; const UniqueBlockMode: Boolean = False); reintroduce;
    destructor Destroy; override;
    //
    function SMGetMem(Size: Integer): Pointer;
    function SMFreeMem(const Loc: Pointer; const Block: TOSBlock): Boolean;
    function SMReAllocMem(const Size: Integer; const Block: TOSBlock): Pointer;
    //
    procedure Clear; override;
  end;

type
  TSizeManagerList = class(TTopSortedList)
  private
    function Get(Index: Integer): TSizeManagerBase;
    procedure Put(Index: Integer; const Value: TSizeManagerBase);
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
  public
    constructor Create(const SMIndex: Byte; const AppBlockSize: Integer; const AThreadManager: Pointer; const UniqueBlockMode: Boolean = False); reintroduce;
    destructor Destroy; override;
    // Poolblocks give and take
    procedure AddBlockToPool(const Block: TOSBlock);
    procedure AddBlocksToPool(const Blocks: TOSBlockList);
    function GetEmptyBlock(const StartAt: Byte; out Block: TOSBlock; const NewOwner: Pointer): Boolean; // Block with room, not totally empty
    //
    function SMFreeMem(const Loc: Pointer; const Block: TOSBlock): Boolean;
    //
    procedure LockList(const PoolID: Byte);
    procedure UnLockList(const PoolID: Byte);
    // Maintenance
    procedure ManagePoolSize;
    //
    procedure Clear; override;
  end;

type
  TThreadManager = class(TCSObject)
  private
    FPLL: TPointerLookupList;
    // List voor SizeManagers
    FSManagerList: TSizeManagerList;
    // List voor gemarkeerde blokken
    FMarkedBlockList: TTopSortedList;
    FBlocksToFree: TTopSortedList;
    procedure CreateList; virtual;
    procedure Destroylist;
    function GetSizeManager(const Size: Integer): TSizeManager;
    function GetSizeManagerByIndex(const Index: Byte): TSizeManager;
    function GetIsPoolManager: Boolean;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    //
    function TMFreeMem(const P: Pointer; out FreeResult: Integer; const OSBlock: TOSBlock = nil): Boolean;
    function TMGetMem(const Size: Integer): Pointer;
    function TMReallocMem(const P: Pointer; const Size: Integer; out Found: Boolean): Pointer;
    //
    procedure DataFreedByOtherThreadInBlock(const Block: TOSBlock);
    procedure FreeDataInMarkedBlocks;
    function MarkedBlocksPresent: Boolean;
    //
    procedure Clear; // All Blocks To Pool
    //
    property IsPoolManager: Boolean read GetIsPoolManager;
    property PLL: TPointerLookupList read FPLL; // OSBLocks for this manager
  end;

type
  TPoolTM = class(TThreadManager)
  private
    procedure CreateList; override;
  protected
    procedure AddBlockToPool(const SMIndex: Byte; const OSBlock: TOSBlock);
    function GetBlockFromPool(const StartAt: Byte; const SMIndex: Byte; const NewOwner: TSizeManager; out OSBlock: TOSBlock): Boolean;
    //
    function GetSizeManager(const Size: Integer): TPoolSM; reintroduce;
  public
    function GetSizeManagerByIndex(const Index: Byte): TPoolSM; reintroduce;
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
    FTManagerList: TTopSortedList;
    FGlobalPLL: TMasterPLL;
    FGLobalPool: TPoolTM;
    function AllThreadManagersUsed: Boolean;
    function FreeAppBlockFromOtherThread(const Block: TOSBlock; const Loc: Pointer): Boolean;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    //
    function TMLFreeMem(const ThreadManager: TThreadManager; const P: Pointer; out FreeResult: Integer): Boolean;
    function TMLGetMem(const ThreadManager: TThreadManager; Size: Integer): Pointer;
    function TMLReallocMem(const ThreadManager: TThreadManager; const P: Pointer; const NewSize: Integer; out ReAllocResult: Pointer): Boolean;
    //
    procedure Clear; // Clean up / Move to pool as much as possible
    //
    function ReserveThreadManager(out ThreadManagerIndex: Integer): TThreadManager;
    procedure ReleaseThreadManager(const Manager: TThreadManager; const ManagerIndex: Integer);
    //
    property GlobalPLL: TMasterPLL read FGlobalPLL; // ALL OSBlocks
    property GlobalPool: TPoolTM read FGlobalPool; // ThreadManager reserved for Pool Actions
  end;

implementation

uses TopInstall
{$IFDEF TOPDEBUG}
  ,
  SysUtils
{$ENDIF};

function TSizeManager.AddBlock(const SpecificSize: Integer): Integer;
var
  OSBlock: TOSBlock;
  AppBlockSizeWanted: Integer;
  AppBlocksWanted: Integer;
begin
  // Check size for uniquemode
  if SpecificSize = 0 then
  begin
    AppBlockSizeWanted := FAppBlockSize;
    AppBlocksWanted := FInitialAppBlocks * (1 + FOSBlockList.Count); // every new block larger then the one before
  end
  else
  begin
    AppBlockSizeWanted := SpecificSize;
    AppBlocksWanted := 1;
  end;
  // Try to get block from pool
  if not TopMM.GlobalPool.GetBlockFromPool(FStartAt, FSMIndex, Self, OSBlock) then
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
    // Move all OSBlocks to Pool or free them if pool is filled
    //
    // Get PoolSizeManager for blocks
    PoolSizeManager := TopMM.GlobalPool.GetSizeManagerByIndex(FSMIndex);
    //
{$IFDEF TOPDEBUG}
    if PoolSizeManager.Appblocksize <> FAppblocksize then
      DebugError('SM.WRPLM');
{$ENDIF}
    // Move the blocks
    PoolSizeManager.AddBlocksToPool(FOSBlockList);
    // Clear the list
    FBlockToStart := 0;
    FFullBlocks := 0;
    FOSBlockList.Clear;
  end;
end;

constructor TSizeManager.Create(const SMIndex: Byte; const AThreadManager: Pointer; const AppBlockSize: Integer; const AppBlocks: Integer; const UniqueBlockMode: Boolean);
var
  lAppBlocks: Integer;
begin
  inherited Create(SMIndex, AppBlockSize, AThreadManager, UniqueBlockMode);
{$IFDEF TOPSPEED}
  lAppBlocks := AppBlocks * 4;
{$ELSE}
  lAppBlocks := AppBlocks;
{$ENDIF}
  if lAppBlocks > cMaxAppBlocks then
    lAppBlocks := cMaxAppBlocks;
  if FUniqueBlockmode then
    lAppBlocks := 1;
  FBlockToStart := 0;
  FFullBlocks := 0;
  FInitialAppBlocks := lAppBlocks;
  FOSBlockList := TOSBlockList.Create(False);
  FStartAt := Random(cMaxBlockLists);
end;

destructor TSizeManager.Destroy;
var
  I: Integer;
begin
  for I := 0 to FOSBlockList.Count - 1 do
    FOSBlockList[I].Free;
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
  if FOSBlockList[Index].IsFull then
    Dec(FFullBlocks);
  // Remove from Thread PLL (MasterPLL does not care who the block owns)
  // Pool does not have a local PLL
  if (not TThreadManager(FThreadManager).IsPoolManager) then
    if (FOSBlockList[Index].OSBlockPointer <> nil) then
      TThreadManager(FThreadManager).PLL.RemoveBlock(FOSBlockList[Index].OSBlockPointer, True);
  // Remove block from local list
  FOSBlockList.DeleteByIndex(Index);
  // Make sure currentblockpointer is still valid
  if (FBlockToStart >= Index) and (FBlockToStart > 0) then
    Dec(FBlockToStart);
end;

function TSizeManager.SMFreeMem(const Loc: Pointer; const Block: TOSBlock): Boolean;
var
  Index: Integer;
begin
{$IFDEF TOPDEBUG}
  if block = nil then
    DebugError('Invalid block passed to SMFreemem procedure');
{$ENDIF}
  Result := Block.OBFreeMem(Loc);
  //
  if Result then
  begin
    // Correct fullblockcounter
    if Block.FreeListStart = Block.AppBlocks - 1 then
      Dec(FFullBlocks);
    // Indien blok nu leeg misschien OS memory vrijgeven of block naar Pool
    if Block.IsEmpty then
    begin
{$IFDEF TOPDEBUG}
      if not (FFullBlocks <= FOSBlockList.Count) then
        DebugError('FullBlocks counter is invalid #1');
      if not (FFullBlocks >= 0) then
        DebugError('FullBlocks counter is invalid #2');
{$ENDIF}
      // In Uniqueblockmode geven we alles terug aan OS (blokken zijn te groot om niet aan OS terug te geven) en houden we de tosblocks zelf
      if (FUniqueBlockmode) then
      begin
        Block.FreeOSBlock;
        if FOSBlockList[FBlockToStart].IsFull then
          FOSBlockList.Find(Block, FBlockToStart); // Move ptr to empty data if pointing to full block}
      end else
      begin // If not uniqueblock we in principle move to pool. Only some very small blocks we keep for performance
        if ((FSMIndex < 8) and ((FOSBlockList.Count - FFullBlocks) > (8 - FSMIndex))) or (FSMIndex >= 8) then
          if FOSBlockList.Find(Block, Index) then
          begin
            RemoveBlock(Index);
            TopMM.FGlobalPool.AddBlockToPool(FSMIndex, Block); // Locking of pool is done inside this procedure
            //Block.Free;
          end;
      end;
    end
    else // block has free space now, move ptr if current block is full
      if FOSBlockList[FBlockToStart].IsFull then
        FOSBlockList.Find(Block, FBlockToStart); // Move ptr to empty data if pointing to full block
  end;
end;

function TSizeManager.SMGetMem(Size: Integer): Pointer;
begin
  Result := nil;
  // Check if there are blocks with data available and if so find one
  if (FFullBlocks < FOSBlockList.Count) then
  begin
    // Try to get memory from one of the OSBlocks
    repeat
      // Try to allocate
      if not FOSBlockList[FBlockToStart].OBGetMem(Size, Result) then
      begin
        // If not successfull try next block
        Inc(FBlockToStart);
        if FBlockToStart = FOSBlockList.Count then
          FBlockToStart := 0;
      end
      else
      begin
        // Statcounters
        if FOSBlockList[FBlockToStart].IsFull then
          Inc(FFullBlocks);
      end;
    until (Result <> nil);
  end;
  //
  if Result = nil then
  begin
    // Allocate new OS Block for Size
    if FUniqueBlockmode then
      FBlockToStart := AddBlock(Size)
    else
      FBlockToStart := AddBlock;
    // Get space from fresh new block
    FOSBlockList[FBlockToStart].OBGetMem(Size, Result);
    // Statcounters
    if FOSBlockList[FBlockToStart].IsFull then
      Inc(FFullBlocks);
  end;
  //
{$IFDEF TOPDEBUG}
  if Result = nil then
    DebugError('SizeManager Allocation failed');
{$ENDIF}
end;

procedure TThreadManager.CreateList;
begin
  FSManagerList := TSizeManagerList.Create(False);
  FSManagerList.Capacity := cMaxManagers + 1;
  FSManagerList[0] := TSizeManager.Create(0, self, 4, 16);
  FSManagerList[1] := TSizeManager.Create(1, self, 8, 16);
  FSManagerList[2] := TSizeManager.Create(2, self, 16, 16);
  FSManagerList[3] := TSizeManager.Create(3, self, 32, 16);
  FSManagerList[4] := TSizeManager.Create(4, self, 64, 8);
  FSManagerList[5] := TSizeManager.Create(5, self, 128, 8);
  FSManagerList[6] := TSizeManager.Create(6, self, 256, 8);
  FSManagerList[7] := TSizeManager.Create(7, self, 512, 4);
  FSManagerList[8] := TSizeManager.Create(8, self, 1024, 4);
  FSManagerList[9] := TSizeManager.Create(9, self, 2048, 4);
  FSManagerList[10] := TSizeManager.Create(10, self, 4096, 4);
  FSManagerList[11] := TSizeManager.Create(11, self, 8192, 4);
  FSManagerList[12] := TSizeManager.Create(12, self, 16384, 4);
  FSManagerList[13] := TSizeManager.Create(13, self, 16384 * 2, 2);
  FSManagerList[14] := TSizeManager.Create(14, self, 16384 * 4, 2);
  FSManagerList[15] := TSizeManager.Create(15, self, 16384 * 8, 2);
  FSManagerList[16] := TSizeManager.Create(16, self, 262144, 1);
  FSManagerList[17] := TSizeManager.Create(17, self, 262144 * 2, 1);
  FSManagerList[18] := TSizeManager.Create(18, self, 262144 * 4, 1);
  // Large block handler
  FSManagerList[cMaxManagers] := TSizeManager.Create(cMaxManagers, self, MaxInt, 1, True);
{$IFDEF TOPDEBUG}
  if cMaxManagers <> 19 then DebugError('cMaxManagers hould be correct #2');
{$ENDIF}
end;

destructor TThreadManager.Destroy;
begin
  DestroyList;
  //
  FPLL.Free;
  //
  FMarkedBlockList.Free;
  FBlocksToFree.Free;
  //
  inherited Destroy;
end;

function TThreadManager.GetSizeManager(const Size: Integer): TSizeManager;
var
  ListPos: Integer;

  function IntLog2(Value: Integer): Integer;
  asm
           BSR EAX, EAX
  end;

begin
  // Simple hash function in one bucket  (log2, but less precise and faster than Math unit function)
  ListPos := IntLog2(Size) - 1;
  // Check boundaries
  if ListPos < 0 then
    ListPos := 0 else
    if ListPos > cMaxManagers then
      ListPos := cMaxManagers;
  // Check of we got the exact correct manager and otherwise move to the correct one
  repeat
    if FSManagerList[ListPos].AppBlockSize >= Size then
    begin
      if (ListPos > 0) and (FSManagerList[ListPos - 1].AppBlockSize >= Size) then
      begin
        Dec(ListPos);
      end
      else
        Break;
    end
    else
      Inc(ListPos);
  until (ListPos = 0) or (ListPos = cMaxManagers); // We break if correct one found
{$IFDEF TOPDEBUG}
  if not ((ListPos >= 0) and (ListPos <= cMaxManagers)) then
    DebugError('Wrong list from hashroutine');
{$ENDIF}
  //
  Result := TSizeManager(FSManagerList[ListPos]);
  //
{$IFDEF TOPDEBUG}
  if not ((Result.FAppblocksize >= Size)) then
    DebugError('Selected SizeManager has wrong AppSize');
{$ENDIF}
end;

function TThreadManager.TMGetMem(const Size: Integer): Pointer;
begin
{$IFDEF TOPDEBUG}
  if IsPoolManager then
    DebugError('GetMem from Pool is not allowed');
{$ENDIF}
  //
  Result := GetSizeManager(Size).SMGetMem(Size);
end;

function TThreadManager.TMFreeMem(const P: Pointer; out FreeResult: Integer; const OSBlock: TOSBlock): Boolean;
var
  FMOSBlock: TOSBlock;
begin
{$IFDEF TOPDEBUG}
  if IsPoolManager then
    DebugError('FreeMem in Pool is not allowed');
{$ENDIF}
  //
  if (OSBlock <> nil) or (FPLL.FindBlock(Cardinal(P), FMOSBlock)) then
  begin
    if OSBlock <> nil then
      FMOSBlock := OSBlock;
{$IFDEF TOPDEBUG}
    if FMOSBlock.IsEmpty then
      DebugError('Block returned is empy, so how can we free data in it? '); // <<-- Probably application doing double free of same pointer
    if TThreadManager(TSizeManagerBase(FMOSBlock.SizeManager).FThreadManager) <> self then
      DebugError('Block is owned by our own manager!');
{$ENDIF}
    Result := TSizeManager(FMOSBlock.Sizemanager).SMFreeMem(P, FMOSBlock);
    if Result then
      FreeResult := 0 // No Error result for Delphi
    else
      FreeResult := 11; // Will become Invalid Ptr Operation in Delphi
  end
  else
  begin
    Result := False;
    FreeResult := -1; // Unknown pointer, will be retried in OldMemory Manager
  end;
end;

function TThreadManager.TMReallocMem(const P: Pointer; const Size: Integer; out Found: Boolean): Pointer;
var
  OldSize: Integer;
  OSBlock: TOSBlock;
  Dummy: Integer;
begin
{$IFDEF TOPDEBUG}
  if IsPoolManager then
    DebugError('ReAlloc should never be called inside a pool manager block');
{$ENDIF}
  //
{$IFDEF TOPDEBUG}
  if not ((P <> nil) and (Size > 0)) then
    DebugError('Incorrect Re-Alloc parameters passed');
{$ENDIF}
  Result := nil;
  Found := FPLL.FindBlock(Cardinal(P), OSBlock);
  if Found then
  begin
    // only if realloc is larger then excess space a true realloc is needed
    OLDSize := OSBlock.AppBlockSize;
    if Oldsize < Size then
    begin
      // In Uniquemode we now do a OS-ReAlloc as all Blockdata is for this allocation
      if OSBlock.UniqueMode then
      begin
        Result := TSizeManager(OSBlock.SizeManager).SMReAllocMem(Size, OSBlock);
      end
      else
      begin // We have to move to a new block as this block is of wrong size for new alloc
        Result := TMGetMem(Size);
        if Result <> nil then
        begin
          // MemoryCopy
          if OldSize < 32 then
          begin
            for Dummy := 0 to OldSize - 1 do
              TByteArray(Result^)[Dummy] := TByteArray(P^)[Dummy];
          end else
            MMXCopyMemory(Result, P, Oldsize);
          TMFreeMem(P, Dummy, OSBlock);
        end;
      end;
    end
    else
    begin
      if Oldsize > Size then
      begin
        //  In Uniquemode we now shrink the OSBlock if >50% is returned
        if OSBlock.UniqueMode then
        begin
          if (OldSize shr {$IFDEF TOPSPEED}2{$ELSE}1{$ENDIF}) > Size then
            Result := TSizeManager(OSBlock.SizeManager).SMReAllocMem(Size, OSBlock) else result := P;
        end
        else
        begin
          // If new alloc is significantly smaller as original (<50%) we move the data
          if (OSBlock.AppBlockSize shr {$IFDEF TOPSPEED}3{$ELSE}1{$ENDIF}) > Size then
          begin
            // Allocate new area
            Result := TMGetMem(Size);
            if Result <> nil then
            begin
               // MemoryCopy
              if Size < 32 then
              begin
                for Dummy := 0 to Size - 1 do
                  TByteArray(Result^)[Dummy] := TByteArray(P^)[Dummy];
              end else
                MMXCopyMemory(Result, P, Size);
              TMFreeMem(P, Dummy, OSBlock);
            end;
          end
          else
            Result := P;
        end;
      end
      else
        Result := P;
    end;
  end;
end;

procedure TThreadManager.Clear;
var
  I: Integer;
  R: Integer;
begin
  // Only call this from the context of the thread that has been given this manager
  //
  // Clear local PLL in one stroke as we will give all blocks away
  FPLL.Clear;
  //
  // Clear all SizeManagers of Excess Stuff
  // Start this loop random in each thread so we are not blocking each other when many threads finish simultaneously
  R := Random(cMaxManagers + 1);
  // All Managers from R to FSManagers
  for I := R to cMaxManagers do
    FSManagerList[I].Clear;
  // All Managers from  Zero to R - 1
  for I := 0 to R - 1 do
    FSManagerList[I].Clear;
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
    for I := FFreeListStart to FTManagerList.Count - 1 do
      TThreadManager(FTManagerList[FTManagerList[I].Index].Obj).Clear;
  finally
    UnLock;
  end;
end;

constructor TThreadManagerList.Create;
begin
  inherited Create;
  FTManagerList := TTopSortedList.Create(False);
  FFreeListStart := 0;
  FGlobalPLL := TMasterPLL.Create;
  FGLobalPool := TPoolTM.Create;
end;

destructor TThreadManagerList.Destroy;
var
  I: Integer;
begin
  Lock;
  try
    // Destroy managers
    for I := FTManagerList.Count - 1 downto 0 do
      TThreadManager(FTManagerList[I].Obj).Free;
    // List is freed
    FTManagerList.Free;
  finally
    UnLock;
  end;
  //
  FGlobalPool.Free;
  FGlobalPLL.Free;
  //
  inherited Destroy;
end;

constructor TThreadManager.Create;
begin
  inherited Create;
  FPLL := TPointerLookupList.Create;
  CreateList;
  FMarkedBlockList := TTopSortedList.Create(True);
  FBlocksToFree := TTopSortedList.Create(False);
end;

procedure TThreadManagerList.ReleaseThreadManager(const Manager: TThreadManager; const ManagerIndex: Integer);
var
  BufManager: Pointer;
begin
{$IFDEF TOPDEBUG}
  if TThreadManager(FTManagerList[ManagerIndex].Obj) <> Manager then
    DebugError('ThreadManager does not reside at the passed index in the list');
{$ENDIF}
  // Move all remaining blocks To Pool
  // TODO: We can skip this if the application is closing
  try
    Manager.Clear;
  except
   // Whatever errors come from a clear, we do not allow our list to become corrupted by that
  end;
  //
  // Add To ThreadManagerList FreeList
  Lock;
  try
    Dec(FFreeListStart);
    BufManager := FTManagerList[FFreeListStart].Obj;
    FTManagerList.SetValue(FFreeListStart, ManagerIndex, BufManager);
  finally
    UnLock;
  end;
end;

function TThreadManagerList.ReserveThreadManager(out ThreadManagerIndex: Integer): TThreadManager;
begin
  Lock;
  try
    // Try to reserve an existing manager
    if not AllThreadManagersUsed then
    begin
      ThreadManagerIndex := FTManagerList[FFreeListStart].Index;
      Result := TThreadManager(FTManagerList[ThreadManagerIndex].Obj);
      Inc(FFreeListStart);
    end
    else
    begin
      // Make new Manager;
      ThreadManagerIndex := FTManagerList.Add(FTManagerList.Count, nil);
      Inc(FFreeListStart);
      Result := TThreadManager.Create;
      FTManagerList.SetValue(ThreadManagerIndex, ThreadManagerIndex, Result);
    end;
  finally
    UnLock;
  end;
end;

function TThreadManagerList.TMLFreeMem(const ThreadManager: TThreadManager; const P: Pointer; out FreeResult: Integer): Boolean;
var
  OSBlock: TOSBlock;
begin
  // Try to free in current ThreadManager.
  Result := ThreadManager.TMFreeMem(P, FreeResult);
  // Memory not in current ThreadManager, try Global List of all Blocks
  if not Result then
  begin
    // Check if it belongs to another thread
    if FGlobalPLL.FindBlock(Cardinal(P), OSBlock) then
    begin
{$IFDEF TOPDEBUG}
      if (not (Cardinal(P) < (Cardinal(OSBlock.OSBlockPointer) + Cardinal(OSBlock.OSBlockSize)))) then
        DebugError('Selected block does not contain memory in which pointer resides');
{$ENDIF}
      Result := FreeAppBlockFromOtherThread(OSBlock, P);
      if Result then
        FreeResult := 0
      else
        FreeResult := 11;
    end;
  end;
end;

function TThreadManagerList.AllThreadManagersUsed: Boolean;
begin
  Result := FFreeListStart = FTManagerList.Count;
end;

function TThreadManagerList.TMLGetMem(const ThreadManager: TThreadManager; Size: Integer): Pointer;
begin
{$IFDEF TOPSPEED}
  Result := ThreadManager.TMGetMem(Size * 2); // Overallocate so realloc is faster
{$ELSE}
  Result := ThreadManager.TMGetMem(Size);
{$ENDIF}
end;

function TThreadManagerList.TMLReallocMem(const ThreadManager: TThreadManager; const P: Pointer; const NewSize: Integer; out ReAllocResult: Pointer): Boolean;
var
  OSBlock: TOSBlock;
  GetMemResult: Pointer;
  CopySize: Integer;
  Dummy: Integer;
begin
  ReAllocResult := nil;
  Result := False;
  // ReAlloc of existing block
  if (P <> nil) and (NewSize > 0) then
  begin
    ReAllocResult := ThreadManager.TMReAllocMem(P, NewSize, Result); // Result nil is failure, Found False means block is not in this manager
    if not Result then
    begin
      // Check if data belongs to another thread
      if FGlobalPLL.FindBlock(Cardinal(P), OSBlock) then
      begin
        Result := True;
        // copy data if block to small or new size is far below normal size for block
        if (OSBlock.AppBlockSize < NewSize) or (NewSize < (OSBlock.AppBlockSize shr {$IFDEF TOPSPEED}3{$ELSE}1{$ENDIF})) then
        begin
            // Claim new space in our own manager, copy data from other block and free in other Manager
          GetMemResult := TMLGetMem(ThreadManager, NewSize);
          if GetMemResult <> nil then
          begin
              // Copy all content to new block (or as much as possible if block is smaller)
            CopySize := OSBlock.AppBlockSize;
            if NewSize < CopySize then
              CopySize := NewSize;
                // MemoryCopy
            if CopySize < 32 then
            begin
              for Dummy := 0 to CopySize - 1 do
                TByteArray(GetMemResult^)[Dummy] := TByteArray(P^)[Dummy];
            end else
              MMXCopyMemory(GetMemResult, P, CopySize);
            // Set result pointer to newly allocated spot
            ReAllocResult := GetMemResult;
            // Free old data
            FreeAppBlockFromOtherThread(OSBlock, P);
          end;
        end
        else
          ReAllocResult := P; // Reallocated size fits within already allocated block
      end;
    end;
  end
  else
  begin
    // GetMem disguised as ReAlloc
    if (P = nil) and (NewSize > 0) then
    begin
      Result := True;
      ReAllocResult := TMLGetMem(ThreadManager, NewSize);
    end
    else
    begin
      // FreeMem disguised as ReAlloc
      if (P <> nil) and (NewSize = 0) then
        Result := TMLFreeMem(ThreadManager, P, Dummy); // Dummy could contain Error 11 Invalid Ptr Op
    end;
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
begin
{$IFDEF TOPDEBUG}
  if (not OSBlock.IsEmpty) then
    DebugError('PM.ABTP');
{$ENDIF}
  // Use AppBlockSize of SizeManager as Block might have been shrunken in uniquemode. Still has to go to Correct SizeManager in pool
  PoolSizeManager := GetSizeManagerByIndex(SMIndex);
  //
  PoolSizeManager.LockList(OSBlock.PoolID);
  try
    PoolSizeManager.AddBlockToPool(OSBlock);
  finally
    PoolSizeManager.UnLockList(OSBlock.PoolID);
  end
end;

function TPoolTM.GetBlockFromPool(const StartAt: Byte; const SMIndex: Byte; const NewOwner: TSizeManager; out OSBlock: TOSBlock): Boolean;
begin
  OSBlock := nil;
  // Get Block from SizeManager that has blocks this size
  Result := GetSizeManagerByIndex(SMIndex).GetEmptyBlock(StartAt, OSBlock, NewOwner);
  // Add new block to local PLL, but skip MasterPLL as it already knows the block and is not interested in it's owner
  // Do this ouside the lock to make lock contention as short as possible
  if Result then
    if OSBlock.OSBlockPointer <> nil then
      TThreadManager(NewOwner.FThreadManager).PLL.AddBlock(OSBlock, True);
end;

procedure TPoolSM.AddBlockToPool(const Block: TOSBlock);
begin
  // Caller must have size manager locked
  // Block should not be listed as having blocks freed by other thread in current threadmanager
  // Set Pool as New Owner for Block;
  Block.SizeManager := self;
  // Add block to end of list
  FBlockList[Block.PoolID].List.Add(Block, Block);
  // Full block and flag setting
  if Block.IsFull then Inc(FBlockList[Block.PoolID].FullBlocks) else FBlockList[Block.PoolID].List.Flag := True;
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
  Block := nil;
  Result := False;
  // Start op een random positie om zoveel mogelijk niet in elkaars weg te zitten
  X := StartAt;
  C := 0;
  repeat
    if FBlockList[X].List.Flag then
    begin
      LockList(X);
      try
        for I := FBlockList[X].List.Count - 1 downto 0 do
          if not TOSBlock(FBlockList[X].List[I].Obj).IsFull then
          begin
            // Niet vol blok gevonden
            Result := True;
            Block := TOSBlock(FBlockList[X].List[I].Obj);
            Block.SizeManager := NewOwner;
            // verwijder uit pool
            FBlockList[X].List.DeleteByIndex(I);
            // Flag setting
            if FBlockList[X].List.Count = FBlockList[X].FullBlocks then FBlockList[X].List.Flag := False;
            // MinMax
            if (FBlockList[X].List.Count - FBlockList[X].FullBlocks) < (FBlockList[X].MinBlocks) then FBlockList[X].MinBlocks := FBlockList[X].List.Count - FBlockList[X].FullBlocks;
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
      if X = CMaxBlockLists then
        X := 0;
      Inc(C);
    end;
    //
  until (Result = True) or (C = cMaxBlockLists)
end;

procedure TPoolSM.AddBlocksToPool(const Blocks: TOSBlockList);
var
  I: Integer;
begin
  // Should be already locked by caller
{$IFDEF TOPDEBUG}
  if (not TThreadManager(FThreadManager).IsPoolManager) then
    DebugError('SM.ABSTP');
{$ENDIF}
  // Walk all blocks and add to pool. If pool is filled and the block is empty then free the block
  for I := 0 to Blocks.Count - 1 do
  begin
    LockList(Blocks[I].PoolID);
    try
      // Free data in there from other threads
      if Blocks[I].FreedByOtherThreadBlocks > 0 then
        ProcessAppBlocksFreedFromOtherThread(Blocks[I]);
      // Move to pool
      AddBlockToPool(Blocks[I]);
    finally
      UnLockList(Blocks[I].PoolID);
    end;
  end;
end;

function TSizeManager.SMReAllocMem(const Size: Integer; const Block: TOSBlock): Pointer;
begin
{$IFDEF TOPDEBUG}
  if not Block.UniqueMode then
    DebugError('SM.BNU');
{$ENDIF}
  // ReAlloc data in a Unique mode block
  Block.OBResize(Size, Result);
end;

procedure TThreadManager.DataFreedByOtherThreadInBlock(const Block: TOSBlock);
begin
  // Caller must have TM locked
  FMarkedBlockList.Add(Block, Block);
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
      FreeBlockData(TOSBlock(FMarkedBlockList[I].Obj));
      FMarkedBlockList.DeleteByIndex(I);
    end;
  finally
    UnLock;
  end;
  // Free last appblocks (will move to pool) outside Threadlock (deadlock if within)
  for I := 0 to FBlocksToFree.Count - 1 do
    TSizeManager(TOSBlock(FBlocksToFree[I].Obj).SizeManager).SMFreeMem(Pointer(FBlocksToFree[I].Index), TOSBlock(FBlocksToFree[I].Obj));
end;

function TThreadManager.GetIsPoolManager: Boolean;
begin
  Result := Self = TopMM.GlobalPool;
end;

procedure TPoolTM.CreateList;
begin
  FSManagerList := TSizeManagerList.Create(False);
  FSManagerList.Capacity := cMaxManagers + 1;
  FSManagerList[0] := TPoolSM.Create(0, 4, self);
  FSManagerList[1] := TPoolSM.Create(1, 8, self);
  FSManagerList[2] := TPoolSM.Create(2, 16, self);
  FSManagerList[3] := TPoolSM.Create(3, 32, self);
  FSManagerList[4] := TPoolSM.Create(4, 64, self);
  FSManagerList[5] := TPoolSM.Create(5, 128, self);
  FSManagerList[6] := TPoolSM.Create(6, 256, self);
  FSManagerList[7] := TPoolSM.Create(7, 512, self);
  FSManagerList[8] := TPoolSM.Create(8, 1024, self);
  FSManagerList[9] := TPoolSM.Create(9, 2048, self);
  FSManagerList[10] := TPoolSM.Create(10, 4096, self);
  FSManagerList[11] := TPoolSM.Create(11, 8192, self);
  FSManagerList[12] := TPoolSM.Create(12, 16384, self);
  FSManagerList[13] := TPoolSM.Create(13, 16384 * 2, self);
  FSManagerList[14] := TPoolSM.Create(14, 16384 * 4, self);
  FSManagerList[15] := TPoolSM.Create(15, 16384 * 8, self);
  FSManagerList[16] := TPoolSM.Create(16, 262144, self);
  FSManagerList[17] := TPoolSM.Create(17, 262144 * 2, self);
  FSManagerList[18] := TPoolSM.Create(18, 262144 * 4, self);
  // Large block handler
  FSManagerList[cMaxManagers] := TPoolSM.Create(cMaxManagers, MaxInt, self, True);
{$IFDEF TOPDEBUG}
  if cMaxManagers <> 19 then DebugError('cMaxManagers hould be correct #1');
{$ENDIF}
end;

constructor TPoolSM.Create(const SMIndex: Byte; const AppBlockSize: Integer; const AThreadManager: Pointer; const UniqueBlockMode: Boolean);
var
  I: Integer;
begin
  inherited Create(SMIndex, AppBlockSize, AThreadManager, UniqueBlockMode);
  //
  for I := 0 to cMaxBlockLists - 1 do
  begin
    ZeroMemory(@FBlockList[I], SizeOf(TPoolSMBlock));
    FBlockList[I].List := TTopsortedList.Create(False);
    InitializeCriticalSection(FBlockList[I].Lock);
  end;
end;

constructor TSizeManagerBase.Create(const SMIndex: Byte;
  const AppBlockSize: Integer; const AThreadManager: Pointer; const UniqueBlockMode: Boolean);
begin
  inherited Create;
  FSMIndex := SMIndex;
  FAppBlockSize := AppBlockSize;
  FThreadManager := AThreadManager;
  FUniqueBlockMode := UniqueBlockMode;
end;

procedure TSizeManagerBase.FreeBlock(const Block: TOSBlock);
begin
  // Blok is al uit lokale PLL gegooid
  TopMM.GlobalPLL.RemoveBlock(Block.OSBlockPointer);
  Block.Free;
end;

function TThreadManagerList.FreeAppBlockFromOtherThread(const Block: TOSBlock; const Loc: Pointer): Boolean;
var TM: TThreadManager;
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
        Result := Block.AddFreedByOtherThreadListBlock(Loc);
        // Also Add Block to ThreadMarkedBlockList if this was the first block added
        if Result and (Block.FreedByOtherThreadBlocks = 1) then
          TThreadManager(TSizeManager(Block.SizeManager).ThreadManager).DataFreedByOtherThreadInBlock(Block);
      finally
        TM.UnLock;
      end;
    end else
      Result := PoolSM.SMFreeMem(Loc, Block);
  finally
    PoolSM.UnLockList(Block.PoolID);
  end;
end;

{ TOSBlockList }
function TOSBlockList.Get(Index: Integer): TOSBlock;
begin
  Result := Pointer(GetInteger(Index));
end;

procedure TOSBlockList.Put(Index: Integer; const Value: TOSBlock);
begin
  SetValue(Index, Value);
end;

destructor TPoolSM.Destroy;
var
  I, J: Integer;
begin
  for I := 0 to cMaxBlockLists - 1 do
  begin
    LockList(I);
    try
      for J := 0 to FBlockList[I].List.Count - 1 do
        TOSBlock(FBlockList[I].List[J].Obj).Free;
    finally
      UnLockList(I);
    end;
    DeleteCriticalSection(FBlockList[I].Lock);
    FBlockList[I].List.Free;
  end;
  //
  inherited;
end;

function TPoolTM.GetSizeManager(const Size: Integer): TPoolSM;
begin
  Result := TPoolSM(inherited GetSizeManager(Size));
end;

function TPoolTM.GetSizeManagerByIndex(const Index: Byte): TPoolSM;
begin
  Result := TPoolSM(inherited GetSizeManagerByIndex(Index));
end;

function TThreadManager.GetSizeManagerByIndex(const Index: Byte): TSizeManager;
begin
  Result := TSizeManager(FSManagerList[Index]);
end;

function TPoolSM.FreeBlocks(const ListIndex: Byte; const Amount: Integer): Boolean; // Result True = Has FreeBlocks left
var
  J, BlocksFreed: Integer;
  Block: TOSBlock;
begin
  // Caller sgould have pool locked
  // Free [Amount] empty blocks
  BlocksFreed := 0;
  for J := FBlockList[ListIndex].List.Count - 1 downto 0 do
  begin
    if BlocksFreed < Amount then
    begin
      if TOSBlock(FBlockList[ListIndex].List[J].Obj).IsEmpty then
      begin
        Block := TOSBlock(FBlockList[ListIndex].List[J].Obj);
          // verwijder uit pool
        FBlockList[ListIndex].List.DeleteByIndex(J);
        FreeBlock(Block);
        Inc(BlocksFreed);
      end;
    end else Break; // Freed enough blocks
  end;
  //
  Result := FBlockList[ListIndex].FullBlocks < FBlockList[ListIndex].List.Count;
end;

procedure TPoolSM.ManagePoolSize;
var
  I: Integer;
begin
  for I := 0 to cMaxBlockLists - 1 do
    if FBlockList[I].List.Flag then
    begin
      LockList(I);
      try
      // Free minimum amount continually present during last interval
        FBlockList[I].List.Flag := FreeBlocks(I, FBlockList[I].MinBlocks);
       // reset
        FBlockList[I].MinBlocks := FBlockList[I].List.Count;
      finally
        UnLockList(I);
      end;
    end;
end;

procedure TPoolSM.ProcessAppBlocksFreedFromOtherThread(
  const Block: TOSBlock);
var
  I: Integer;
  Loc: Pointer;
begin
  // Walk blocks
  for I := Block.FreedByOtherThreadBlocks - 1 downto 0 do
  begin
    Loc := Pointer(Cardinal(Block.OSBlockPointer) + Cardinal(Block.FreedByOtherThreadList[I] * Block.AppBlockSize));
    Block.OBFreeMem(Loc);
  end;
  //
  Block.FreedByOtherThreadBlocks := 0;
end;

procedure TPoolSM.LockList(const PoolID: Byte);
begin
  EnterCriticalSection(FBlockList[PoolID].Lock);
end;

procedure TPoolSM.UnLockList(const PoolID: Byte);
begin
  LeaveCriticalSection(FBlockList[PoolID].Lock);
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

function TThreadManager.MarkedBlocksPresent: Boolean;
begin
  Result := FMarkedBlockList.Flag;
end;

function TPoolSM.SMFreeMem(const Loc: Pointer;
  const Block: TOSBlock): Boolean;
begin
  Result := Block.OBFreeMem(Loc);
  // no longer full, then record stat and set Flag to free block present
  if Block.FreeListStart = Block.AppBlocks - 1 then
  begin
    Dec(FBlockList[Block.PoolID].FullBlocks);
    FBlockList[Block.PoolID].List.Flag := True;
  end;
end;

{ TSizeManagerList }

function TSizeManagerList.Get(Index: Integer): TSizeManagerBase;
begin
  Result := Pointer(GetInteger(Index));
end;

procedure TSizeManagerList.Put(Index: Integer; const Value: TSizeManagerBase);
begin
  SetValue(Index, Value);
end;

end.

