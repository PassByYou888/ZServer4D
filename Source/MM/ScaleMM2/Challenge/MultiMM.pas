unit MultiMM;

// version 1.1 - 5/9/01
// moved thread manager creation into crit section

// Robert Lee

// MultiMM, in conjunction with HPMM is intended to be used as a drop-in memory
// manager replacement for Delphi/C++ Builder.

// note that MultiMM is not generally compatible with third party memory
// monitors and may not handle misbehaving code correctly.
// Use the built-in manager for debugging/leak detection first, then switch

// Please send any bug reports to: rhlee@optimalcode.com

interface

uses HPMM;

var
  PrevMemoryManager:TMemoryManager;
  MemoryManager:TMemoryManager;

type

  TMainMemManager=class(TMemManager)
  public
    procedure Transfer(var TransferArray:TTransferArray);
    constructor Create;
    destructor Destroy; override;
  end;

  TThreadMemManager=class(TBlockManager)
  private
    IsUsed:boolean;
    procedure EmptyPools;
    procedure Transfer(var TransferArray: TTransferArray);
  protected
    function IsAvailable(sizeIndex: integer): Boolean; override;
    procedure Allocate(Size:integer); override;
    procedure MakeAvailable(SizeIndex:integer; var count:integer); override;
    procedure ReturnToPool(Block: PFreeBlock); override;
    procedure FreeManager;
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  MainManager:TMainMemManager;

implementation

type
  BOOL  = LongBool;

  TRTLCriticalSection = record
    DebugInfo: Pointer;
    LockCount: Longint;
    RecursionCount: Longint;
    OwningThread: Integer;
    LockSemaphore: Integer;
    Reserved: DWORD;
  end;

const
  kernel = 'kernel32.dll';
  PAGE_NOACCESS  = 1;
  PAGE_READWRITE = 4;

type THandle = Integer;

function FlushInstructionCache(hProcess: THandle; const lpBaseAddress:
  Pointer; dwSize: DWORD): BOOL; stdcall;
  external kernel name 'FlushInstructionCache';
function GetCurrentProcess: THandle; stdcall;
  external kernel name 'GetCurrentProcess';
function VirtualProtect(lpAddress:pointer; dwSize, flNewProtect: DWORD;
  var lpflOldProtect:DWord):BOOL; stdcall;
  external kernel name 'VirtualProtect';
procedure InitializeCriticalSection(var lpCriticalSection: TRTLCriticalSection); stdcall;
  external kernel name 'InitializeCriticalSection';
procedure EnterCriticalSection(var lpCriticalSection: TRTLCriticalSection); stdcall;
  external kernel name 'EnterCriticalSection';
procedure LeaveCriticalSection(var lpCriticalSection: TRTLCriticalSection); stdcall;
  external kernel name 'LeaveCriticalSection';
procedure DeleteCriticalSection(var lpCriticalSection: TRTLCriticalSection); stdcall;
  external kernel name 'DeleteCriticalSection';
procedure ExitThread(ExitCode: Integer); stdcall;
  external kernel name 'ExitThread';


var
  heapLock      : TRTLCriticalSection;
  ThreadLock      : TRTLCriticalSection;

{ TThreadMemManager }

function TThreadMemManager.IsAvailable(sizeIndex: integer): Boolean;
begin
  result:=(BlockPools[SizeIndex].Count>0);
  if not result then
    if MainManager.BlockPools[SizeIndex].Count>0 then  // read only
    begin
      result:=true;
      Allocate(1 shl SizeIndex);
    end;
end;

procedure TThreadMemManager.ReturnToPool(Block:PFreeBlock);
var Pool:PBlockPool;
begin
  Inherited ReturnToPool(Block);
  Pool:=@BlockPools[Block.SizeIndex];
  if Pool.Count>Pool.HiLimit then
    Allocate(0);
end;

constructor TThreadMemManager.Create;
begin
  inherited Create;
  IsUsed:=false;
end;

procedure TThreadMemManager.Transfer(var TransferArray: TTransferArray);
begin
  try
    EnterCriticalSection(heapLock);
    MainManager.Transfer(TransferArray);
  finally
    LeaveCriticalSection(heapLock);
  end;
end;

procedure TThreadMemManager.Allocate(Size:integer);
var
  j:integer;
  TransferArray:TTransferArray;
  Pool:PBlockPool;
  SizeIndex:integer;
begin
  SizeIndex:=RoundUpAlign(Size);
  for j:=MinTableIndex to MaxTableIndex do
  begin
    Pool:=@BlockPools[j];
    if Pool.Count>Pool.MaxCount then
      TransferArray[j].Count:=Pool.MaxCount-Pool.Count
    else
      TransferArray[j].Count:=0;
  end;
  GiveTransfer(TransferArray);
  for j:=MinTableIndex to MaxTableIndex do
  begin
    Pool:=@BlockPools[j];
    if Pool.Count<Pool.MinCount then
      TransferArray[j].Count:=Pool.Count-Pool.MinCount;
  end;
  if TransferArray[SizeIndex].Count=0 then
    dec(TransferArray[SizeIndex].Count);
  Transfer(TransferArray);
  TakeTransfer(TransferArray);
end;

procedure TThreadMemManager.FreeManager;
begin
  IsUsed:=false;
end;

procedure TThreadMemManager.EmptyPools;
var
  j:integer;
  TransferArray:TTransferArray;
begin
  for j:=MinTableIndex to MaxTableIndex do
    TransferArray[j].Count:=-BlockPools[j].Count;
  GiveTransfer(TransferArray);
  Transfer(TransferArray);
end;

procedure TThreadMemManager.MakeAvailable(SizeIndex: integer; var count: integer);
begin
  Count:=BlockPools[SizeIndex].Count;
end;

destructor TThreadMemManager.Destroy;
begin
  EmptyPools;
  inherited Destroy;
end;

{ TMainMemManager }

type
  PMemManagerArray=^TMemManagerArray;
  TMemManagerArray=array[0..(maxint div Sizeof(TThreadMemManager))-1] of TThreadMemManager;

var
  MemManagerList:PMemManagerArray;
  ManagerCapacity:integer;
  ManagerCount:integer;

threadvar MemManagerIndex:integer;

constructor TMainMemManager.Create;
begin
  inherited Create;
  ManagerCapacity:=100;
  MemManagerList:=GetMem(ManagerCapacity*SizeOf(TMemManager));
  ManagerCount:=0;
end;

destructor TMainMemManager.Destroy;
var i:integer;
begin
  for i:=1 to ManagerCount do
    MemManagerList[i].Free;
  FreeMem(MemManagerList);
  inherited Destroy;
end;

procedure TMainMemManager.Transfer(var TransferArray: TTransferArray);
begin
  TakeTransfer(TransferArray);
  GiveTransfer(TransferArray);
end;

{ }

function InitMemManagerIndex:integer;
begin
  try
    EnterCriticalSection(ThreadLock);
    Result:=1;  // Leave zero blank as uninitialized flag
    while (Result<=ManagerCount) and TThreadMemManager(MemManagerList[Result]).IsUsed do
      inc(Result);
    if (Result>ManagerCount) then
    begin
      inc(ManagerCount);
      if Result=ManagerCapacity then
      begin
        MainManager.ReAllocMem(MemManagerList,ManagerCapacity*2*Sizeof(TMemManager));
        inc(ManagerCapacity);
      end;
      MemManagerList[Result]:=TThreadMemManager.Create;
    end;
    TThreadMemManager(MemManagerList[Result]).IsUsed:=true;
  finally
    LeaveCriticalSection(ThreadLock);
  end;
end;

function GetMem(MinSize:integer):Pointer;
var Index:integer;
begin
  if IsMultiThread then
  begin
    Index:=MemManagerIndex;
    if Index=0 then
    begin
      Index:=InitMemManagerIndex;
      MemManagerIndex:=Index;
    end;
    Result:=MemManagerList[Index].GetMem(MinSize);
  end
  else
    Result:=MainManager.GetMem(MinSize);
end;

function FreeMem(addr:pointer):integer;
var Index:integer;
begin
  if IsMultiThread then
  begin
    Index:=MemManagerIndex;
    if Index=0 then
    begin
      Index:=InitMemManagerIndex;
      MemManagerIndex:=Index;
    end;
    Result:=MemManagerList[Index].FreeMem(addr);
  end
  else
    Result:=MainManager.FreeMem(addr);
  if Result<>0 then
    result:=PrevMemoryManager.FreeMem(addr);
end;

function ReAllocMem(addr:pointer; Size:integer):Pointer;
var Index:integer;
begin
  if IsMultiThread then
  begin
    Index:=MemManagerIndex;
    if Index=0 then
    begin
      Index:=InitMemManagerIndex;
      MemManagerIndex:=Index;
    end;
    Result:=MemManagerList[Index].ReAllocMem(addr,Size);
  end
  else
    Result:=MainManager.ReAllocMem(addr,Size);
  if not assigned(Result) then
  begin
    Result:=GetMem(size);
    move(addr^,Result^,size);
    if PrevMemoryManager.FreeMem(addr)<>0 then
    begin
      FreeMem(Result);
      Result:=nil;
    end;
  end;
end;

procedure NewEndThread(exitCode:integer); register // ensure that calling convension matches EndThread
// Free up Manager assigned to thread
var
  index:integer;
begin
  index:=MemManagerIndex;
  //TThreadMemManager(MemManagerList[Index]).EmptyPools;
  TThreadMemManager(MemManagerList[Index]).Allocate(0);
  if index>0 then
    TThreadMemManager(MemManagerList[Index]).IsUsed:=false;
// code of original EndThread;
  ExitThread(exitCode);
end;

type
  PJump=^TJump;
  TJump=packed record
    OpCode:byte;
    Distance:integer;
  end;

var
  OldCode:TJump;
  NewCode:TJump=(OpCode:$E9; Distance:0);


procedure PatchEndThread;
// redirect calls to System.EndThread to NewNewThread
var
  EndThreadAddr:PJump;
  OldProtect,Protect:DWord;
begin
  EndThreadAddr:=Pointer(@EndThread);
  NewCode.Distance:=Integer(@NewEndThread)-(Integer(@EndThread)+5);
  VirtualProtect(EndThreadAddr,5,PAGE_READWRITE,OldProtect);
  OldCode:=EndThreadAddr^;
  EndThreadAddr^:=NewCode;
  VirtualProtect(EndThreadAddr,5,OldProtect,Protect);
  FlushInstructionCache(GetCurrentProcess, EndThreadAddr,5);
end;

procedure UnPatchEndThread;
var
  EndThreadAddr:PJump;
  OldProtect,Protect:DWord;
begin
  EndThreadAddr:=Pointer(@EndThread);
  NewCode.Distance:=Integer(@NewEndThread)-(Integer(@EndThread)+5);
  VirtualProtect(EndThreadAddr,5,PAGE_READWRITE,OldProtect);
  EndThreadAddr^:=OldCode;
  VirtualProtect(EndThreadAddr,5,OldProtect,Protect);
  FlushInstructionCache(GetCurrentProcess, EndThreadAddr,5);
end;


initialization
  InitializeCriticalSection(HeapLock);
  InitializeCriticalSection(ThreadLock);
  MainManager:=TMainMemManager.Create;
  GetMemoryManager(PrevMemoryManager);
  PatchEndThread;
  MemoryManager.GetMem:=GetMem;
  MemoryManager.ReAllocMem:=ReAllocMem;
  MemoryManager.FreeMem:=FreeMem;
  SetMemoryManager(MemoryManager);
finalization
  SetMemoryManager(PrevMemoryManager);
  MainManager.Free;
// Block Manager
  DeleteCriticalSection(HeapLock);
  DeleteCriticalSection(ThreadLock);
  UnPatchEndThread; // just to be tidy
end.
