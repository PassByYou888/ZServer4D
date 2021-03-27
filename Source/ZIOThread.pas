{ * ZIOThread                                                                  * }
{ ****************************************************************************** }
{ * https://zpascal.net                                                        * }
{ * https://github.com/PassByYou888/zAI                                        * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/PascalString                               * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zChinese                                   * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/FFMPEG-Header                              * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/InfiniteIoT                                * }
{ * https://github.com/PassByYou888/FastMD5                                    * }
{ ****************************************************************************** }
unit ZIOThread;

{$INCLUDE zDefine.inc}

interface

uses
{$IFDEF FPC}
  FPCGenericStructlist,
{$ENDIF FPC}
  CoreClasses, DoStatusIO;

type
  TIOData = class;
  TIODataState = (idsDone, idsRunning, idsReady, idsInited);

  TIOThread_Call = procedure(Sender: TIOData);
  TIOThread_Method = procedure(Sender: TIOData) of object;
{$IFDEF FPC}
  TIOThread_Proc = procedure(Sender: TIOData) is nested;
{$ELSE FPC}
  TIOThread_Proc = reference to procedure(Sender: TIOData);
{$ENDIF FPC}

  TIOData = class
  private
    FState: TIODataState;
    FOnCall: TIOThread_Call;
    FOnMethod: TIOThread_Method;
    FOnProc: TIOThread_Proc;
  public
    Data: Pointer;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Process; virtual;
    property OnCall: TIOThread_Call read FOnCall write FOnCall;
    property OnMethod: TIOThread_Method read FOnMethod write FOnMethod;
    property OnProc: TIOThread_Proc read FOnProc write FOnProc;
  end;

  TIODataQueue_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TIOData>;

  TIODataQueue = class(TIODataQueue_Decl)
  public
    procedure Clean;
  end;

  TIO_Interface = interface
    function Count(): Integer;
    property QueueCount: Integer read Count;
    procedure EnQueue(Queue_: TIODataQueue); overload;
    procedure EnQueue(IOData: TIOData); overload;
    procedure EnQueueC(IOData: TIOData; Data: Pointer; OnCall: TIOThread_Call);
    procedure EnQueueM(IOData: TIOData; Data: Pointer; OnMethod: TIOThread_Method);
    procedure EnQueueP(IOData: TIOData; Data: Pointer; OnProc: TIOThread_Proc);
    function DeQueue(wait_: Boolean): TIOData; overload;
    procedure DeQueue(wait_: Boolean; Queue_: TIODataQueue); overload;
    procedure Wait;
  end;

  TIO_Thread = class(TCoreClassInterfacedObject, TIO_Interface)
  protected
    FCritical: TCritical;
    FThRunning: TAtomBool;
    FThNum: Integer;
    FQueue: TIODataQueue;
    procedure ThRun(Sender: TCompute);
  public
    constructor Create(ThNum_: Integer);
    destructor Destroy; override;
    procedure Reset();
    procedure ClearQueue();
    procedure ThEnd();

    function Count(): Integer;
    property QueueCount: Integer read Count;
    procedure EnQueue(Queue_: TIODataQueue); overload;
    procedure EnQueue(IOData: TIOData); overload;
    procedure EnQueueC(IOData: TIOData; Data: Pointer; OnCall: TIOThread_Call);
    procedure EnQueueM(IOData: TIOData; Data: Pointer; OnMethod: TIOThread_Method);
    procedure EnQueueP(IOData: TIOData; Data: Pointer; OnProc: TIOThread_Proc);
    function DeQueue(wait_: Boolean): TIOData; overload;
    procedure DeQueue(wait_: Boolean; Queue_: TIODataQueue); overload;
    procedure Wait;

    class procedure Test();
  end;

  TIO_Direct = class(TCoreClassInterfacedObject, TIO_Interface)
  protected
    FCritical: TCritical;
    FQueue: TIODataQueue;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Reset();
    procedure ClearQueue();

    function Count(): Integer;
    property QueueCount: Integer read Count;
    procedure EnQueue(Queue_: TIODataQueue); overload;
    procedure EnQueue(IOData: TIOData); overload;
    procedure EnQueueC(IOData: TIOData; Data: Pointer; OnCall: TIOThread_Call);
    procedure EnQueueM(IOData: TIOData; Data: Pointer; OnMethod: TIOThread_Method);
    procedure EnQueueP(IOData: TIOData; Data: Pointer; OnProc: TIOThread_Proc);
    function DeQueue(wait_: Boolean): TIOData; overload;
    procedure DeQueue(wait_: Boolean; Queue_: TIODataQueue); overload;
    procedure Wait;

    class procedure Test();
  end;

  TPost_ThreadPool = class;

  TPost_Thread = class
  private
    FOwner: TPost_ThreadPool;
    FBindTh: TCompute;
    FPost: TThreadPost;
    FActivted: TAtomBool;
    procedure ThRun(thSender: TCompute);
  public
    constructor Create(Owner_: TPost_ThreadPool);
    destructor Destroy; override;

    property BindTh: TCompute read FBindTh;
    property Post: TThreadPost read FPost;

    procedure PostC1(OnSync: TThreadPostCall1);
    procedure PostC2(Data1: Pointer; OnSync: TThreadPostCall2);
    procedure PostC3(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant; OnSync: TThreadPostCall3);
    procedure PostC4(Data1: Pointer; Data2: TCoreClassObject; OnSync: TThreadPostCall4);

    procedure PostM1(OnSync: TThreadPostMethod1);
    procedure PostM2(Data1: Pointer; OnSync: TThreadPostMethod2);
    procedure PostM3(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant; OnSync: TThreadPostMethod3);
    procedure PostM4(Data1: Pointer; Data2: TCoreClassObject; OnSync: TThreadPostMethod4);

    procedure PostP1(OnSync: TThreadPostProc1);
    procedure PostP2(Data1: Pointer; OnSync: TThreadPostProc2);
    procedure PostP3(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant; OnSync: TThreadPostProc3);
    procedure PostP4(Data1: Pointer; Data2: TCoreClassObject; OnSync: TThreadPostProc4);
  end;

  TPost_ThreadPool_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TPost_Thread>;

  TPost_ThreadPool = class(TPost_ThreadPool_Decl)
  private
    FCritical: TCritical;
    FQueueOptimized: Boolean;
    FNextID: Integer;
    procedure AddTh(th: TPost_Thread);
    procedure RemoveTh(th: TPost_Thread);
  public
    constructor Create(ThNum_: Integer);
    destructor Destroy; override;
    property QueueOptimized: Boolean read FQueueOptimized write FQueueOptimized;

    function ThNum: Integer;
    function TaskNum: Integer;

    procedure Wait(); overload;
    procedure Wait(th: TPost_Thread); overload;

    function Next_Thread: TPost_Thread;
    function MinLoad_Thread: TPost_Thread;
    function IDLE_Thread: TPost_Thread;

    procedure DoTestCall();
    class procedure Test();
  end;

procedure Test_IOData_Call(Sender: TIOData);

implementation

procedure Test_IOData_Call(Sender: TIOData);
begin
  TCompute.Sleep(0);
end;

constructor TIOData.Create;
begin
  inherited Create;
  FState := idsInited;
  FOnCall := nil;
  FOnMethod := nil;
  FOnProc := nil;
  Data := nil;
end;

destructor TIOData.Destroy;
begin
  inherited Destroy;
end;

procedure TIOData.Process;
begin
  try
    if Assigned(FOnCall) then
        FOnCall(Self);
    if Assigned(FOnMethod) then
        FOnMethod(Self);
    if Assigned(FOnProc) then
        FOnProc(Self);
  except
  end;
end;

procedure TIODataQueue.Clean;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      DisposeObject(items[i]);
  inherited Clear;
end;

procedure TIO_Thread.ThRun(Sender: TCompute);
var
  i: Integer;
  d: TIOData;
  LTK, L: TTimeTick;
begin
  AtomInc(FThNum);
  LTK := GetTimeTick();
  while FThRunning.V do
    begin
      FCritical.Lock;
      d := nil;
      if FQueue.Count > 0 then
        for i := 0 to FQueue.Count - 1 do
          if FQueue[i].FState = idsReady then
            begin
              d := FQueue[i];
              break;
            end;

      if d <> nil then
        begin
          d.FState := idsRunning;
          FCritical.UnLock;
          d.Process;
          FCritical.Lock;
          d.FState := idsDone;
          FCritical.UnLock;
          LTK := GetTimeTick();
        end
      else
        begin
          FCritical.UnLock;
          L := GetTimeTick() - LTK;
          if L > 1000 then
              TCompute.Sleep(1);
        end;
    end;
  AtomDec(FThNum);
end;

constructor TIO_Thread.Create(ThNum_: Integer);
var
  i: Integer;
begin
  inherited Create;
  FCritical := TCritical.Create;
  FThRunning := TAtomBool.Create(True);
  FThNum := 0;
  FQueue := TIODataQueue.Create;

  for i := 0 to ThNum_ - 1 do
      TCompute.RunM({$IFDEF FPC}@{$ENDIF FPC}ThRun);
  while FThNum < ThNum_ do
      TCompute.Sleep(1);
end;

destructor TIO_Thread.Destroy;
begin
  ThEnd();
  FCritical.Free;
  FThRunning.Free;
  DisposeObject(FQueue);
  inherited Destroy;
end;

procedure TIO_Thread.Reset;
var
  n, i: Integer;
begin
  n := FThNum;
  ThEnd();
  FThNum := 0;
  for i := 0 to n - 1 do
      TCompute.RunM({$IFDEF FPC}@{$ENDIF FPC}ThRun);
  while FThNum < n do
      TCompute.Sleep(1);
end;

procedure TIO_Thread.ClearQueue();
var
  i: Integer;
begin
  FCritical.Lock;
  i := 0;
  while i < FQueue.Count do
    if FQueue[i].FState = idsDone then
      begin
        DisposeObject(FQueue[i]);
        FQueue.Delete(i);
      end
    else
        inc(i);
  FCritical.UnLock;
end;

procedure TIO_Thread.ThEnd();
var
  i: Integer;
begin
  FThRunning.V := False;
  while FThNum > 0 do
      TCompute.Sleep(1);
  FCritical.Lock;
  for i := 0 to FQueue.Count - 1 do
      DisposeObject(FQueue[i]);
  FQueue.Clear;
  FCritical.UnLock;
end;

function TIO_Thread.Count(): Integer;
begin
  FCritical.Lock;
  Result := FQueue.Count;
  FCritical.UnLock;
end;

procedure TIO_Thread.EnQueue(Queue_: TIODataQueue);
var
  i: Integer;
begin
  FCritical.Lock;
  for i := 0 to Queue_.Count - 1 do
    begin
      Queue_[i].FState := idsReady;
      FQueue.Add(Queue_[i]);
    end;
  FCritical.UnLock;
end;

procedure TIO_Thread.EnQueue(IOData: TIOData);
begin
  if IOData.FState <> idsInited then
      RaiseInfo('illegal error.');
  IOData.FState := idsReady;
  FCritical.Lock;
  FQueue.Add(IOData);
  FCritical.UnLock;
end;

procedure TIO_Thread.EnQueueC(IOData: TIOData; Data: Pointer; OnCall: TIOThread_Call);
begin
  IOData.Data := Data;
  IOData.FOnCall := OnCall;
  EnQueue(IOData);
end;

procedure TIO_Thread.EnQueueM(IOData: TIOData; Data: Pointer; OnMethod: TIOThread_Method);
begin
  IOData.Data := Data;
  IOData.FOnMethod := OnMethod;
  EnQueue(IOData);
end;

procedure TIO_Thread.EnQueueP(IOData: TIOData; Data: Pointer; OnProc: TIOThread_Proc);
begin
  IOData.Data := Data;
  IOData.FOnProc := OnProc;
  EnQueue(IOData);
end;

function TIO_Thread.DeQueue(wait_: Boolean): TIOData;
var
  n: Integer;
begin
  repeat
    Result := nil;
    FCritical.Lock;
    n := FQueue.Count;
    if n > 0 then
      if FQueue[0].FState = idsDone then
        begin
          Result := FQueue[0];
          FQueue.Delete(0);
        end;
    FCritical.UnLock;
    if (wait_) and (n = 0) then
        TCompute.Sleep(1);
  until (not wait_) or (n = 0) or (Result <> nil);
end;

procedure TIO_Thread.DeQueue(wait_: Boolean; Queue_: TIODataQueue);
var
  n, doneNum: Integer;
  i: Integer;
begin
  repeat
    doneNum := 0;
    FCritical.Lock;
    n := FQueue.Count;
    if n > 0 then
      begin
        i := 0;
        while i < FQueue.Count do
          begin
            if FQueue[i].FState = idsDone then
              begin
                Queue_.Add(FQueue[i]);
                FQueue.Delete(i);
                inc(doneNum);
              end
            else
                inc(i);
          end;
      end;
    FCritical.UnLock;
    if (wait_) and (n = 0) then
        TCompute.Sleep(1);
  until (not wait_) or (n = 0) or (doneNum = 0);
end;

procedure TIO_Thread.Wait;
begin
  while Count > 0 do
      TCompute.Sleep(1);
end;

class procedure TIO_Thread.Test();
var
  i: Integer;
  d: TIOData;
begin
  with TIO_Thread.Create(5) do
    begin
      for i := 0 to 1000 - 1 do
          EnQueueC(TIOData.Create, nil, {$IFDEF FPC}@{$ENDIF FPC}Test_IOData_Call);
      while Count > 0 do
          DeQueue(True).Free;
      Free;
    end;
end;

constructor TIO_Direct.Create;
begin
  inherited Create;
  FCritical := TCritical.Create;
  FQueue := TIODataQueue.Create;
end;

destructor TIO_Direct.Destroy;
begin
  ClearQueue();
  FCritical.Free;
  DisposeObject(FQueue);
  inherited Destroy;
end;

procedure TIO_Direct.Reset;
begin
  ClearQueue;
end;

procedure TIO_Direct.ClearQueue;
var
  i: Integer;
begin
  FCritical.Lock;
  for i := 0 to FQueue.Count - 1 do
      DisposeObject(FQueue[i]);
  FQueue.Clear;
  FCritical.UnLock;
end;

function TIO_Direct.Count: Integer;
begin
  FCritical.Lock;
  Result := FQueue.Count;
  FCritical.UnLock;
end;

procedure TIO_Direct.EnQueue(Queue_: TIODataQueue);
var
  i: Integer;
begin
  FCritical.Lock;
  for i := 0 to Queue_.Count - 1 do
    begin
      Queue_[i].FState := idsRunning;
      Queue_[i].Process;
      Queue_[i].FState := idsDone;
      FQueue.Add(Queue_[i]);
    end;
  FCritical.UnLock;
end;

procedure TIO_Direct.EnQueue(IOData: TIOData);
begin
  if IOData.FState <> idsInited then
      RaiseInfo('illegal error.');

  FCritical.Lock;
  IOData.FState := idsRunning;
  IOData.Process;
  IOData.FState := idsDone;
  FQueue.Add(IOData);
  FCritical.UnLock;
end;

procedure TIO_Direct.EnQueueC(IOData: TIOData; Data: Pointer; OnCall: TIOThread_Call);
begin
  IOData.Data := Data;
  IOData.FOnCall := OnCall;
  EnQueue(IOData);
end;

procedure TIO_Direct.EnQueueM(IOData: TIOData; Data: Pointer; OnMethod: TIOThread_Method);
begin
  IOData.Data := Data;
  IOData.FOnMethod := OnMethod;
  EnQueue(IOData);
end;

procedure TIO_Direct.EnQueueP(IOData: TIOData; Data: Pointer; OnProc: TIOThread_Proc);
begin
  IOData.Data := Data;
  IOData.FOnProc := OnProc;
  EnQueue(IOData);
end;

function TIO_Direct.DeQueue(wait_: Boolean): TIOData;
var
  n: Integer;
begin
  repeat
    Result := nil;
    FCritical.Lock;
    n := FQueue.Count;
    if n > 0 then
      if FQueue[0].FState = idsDone then
        begin
          Result := FQueue[0];
          FQueue.Delete(0);
        end;
    FCritical.UnLock;
  until (not wait_) or (n = 0) or (Result <> nil);
end;

procedure TIO_Direct.DeQueue(wait_: Boolean; Queue_: TIODataQueue);
var
  n, doneNum: Integer;
  i: Integer;
begin
  repeat
    doneNum := 0;
    FCritical.Lock;
    n := FQueue.Count;
    if n > 0 then
      begin
        i := 0;
        while i < FQueue.Count do
          begin
            if FQueue[i].FState = idsDone then
              begin
                Queue_.Add(FQueue[i]);
                FQueue.Delete(i);
                inc(doneNum);
              end
            else
                inc(i);
          end;
      end;
    FCritical.UnLock;
  until (not wait_) or (n = 0) or (doneNum = 0);
end;

procedure TIO_Direct.Wait;
begin
  while Count > 0 do
      TCompute.Sleep(1);
end;

class procedure TIO_Direct.Test;
var
  i: Integer;
  d: TIOData;
begin
  with TIO_Direct.Create do
    begin
      for i := 0 to 1000 - 1 do
          EnQueueC(TIOData.Create, nil, {$IFDEF FPC}@{$ENDIF FPC}Test_IOData_Call);
      while Count > 0 do
          DeQueue(True).Free;
      Free;
    end;
end;

procedure TPost_Thread.ThRun(thSender: TCompute);
var
  L: Integer;
  LastTK, IdleTK: TTimeTick;
begin
  FBindTh := thSender;
  FPost := TThreadPost.Create(thSender.ThreadID);
  FPost.OneStep := False;
  FPost.ResetRandomSeed := False;
  FActivted := TAtomBool.Create(True);

  FOwner.AddTh(Self);

  LastTK := GetTimeTick();
  while FActivted.V do
    begin
      L := FPost.Progress(FPost.ThreadID);
      if L > 0 then
          LastTK := GetTimeTick()
      else
        begin
          IdleTK := GetTimeTick() - LastTK;
          if IdleTK > 1000 then
              TCompute.Sleep(1);
        end;
    end;

  FBindTh := nil;
  DisposeObjectAndNil(FPost);
  DisposeObjectAndNil(FActivted);

  FOwner.RemoveTh(Self);
  Free;
end;

constructor TPost_Thread.Create(Owner_: TPost_ThreadPool);
begin
  inherited Create;
  FOwner := Owner_;
  FBindTh := nil;
  FPost := nil;
  FActivted := nil;
  TCompute.RunM(nil, Self, {$IFDEF FPC}@{$ENDIF FPC}ThRun);
end;

destructor TPost_Thread.Destroy;
begin
  inherited Destroy;
end;

procedure TPost_Thread.PostC1(OnSync: TThreadPostCall1);
begin
  FPost.PostC1(OnSync);
end;

procedure TPost_Thread.PostC2(Data1: Pointer; OnSync: TThreadPostCall2);
begin
  FPost.PostC2(Data1, OnSync);
end;

procedure TPost_Thread.PostC3(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant; OnSync: TThreadPostCall3);
begin
  FPost.PostC3(Data1, Data2, Data3, OnSync);
end;

procedure TPost_Thread.PostC4(Data1: Pointer; Data2: TCoreClassObject; OnSync: TThreadPostCall4);
begin
  FPost.PostC4(Data1, Data2, OnSync);
end;

procedure TPost_Thread.PostM1(OnSync: TThreadPostMethod1);
begin
  FPost.PostM1(OnSync);
end;

procedure TPost_Thread.PostM2(Data1: Pointer; OnSync: TThreadPostMethod2);
begin
  FPost.PostM2(Data1, OnSync);
end;

procedure TPost_Thread.PostM3(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant; OnSync: TThreadPostMethod3);
begin
  FPost.PostM3(Data1, Data2, Data3, OnSync);
end;

procedure TPost_Thread.PostM4(Data1: Pointer; Data2: TCoreClassObject; OnSync: TThreadPostMethod4);
begin
  FPost.PostM4(Data1, Data2, OnSync);
end;

procedure TPost_Thread.PostP1(OnSync: TThreadPostProc1);
begin
  FPost.PostP1(OnSync);
end;

procedure TPost_Thread.PostP2(Data1: Pointer; OnSync: TThreadPostProc2);
begin
  FPost.PostP2(Data1, OnSync);
end;

procedure TPost_Thread.PostP3(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant; OnSync: TThreadPostProc3);
begin
  FPost.PostP3(Data1, Data2, Data3, OnSync);
end;

procedure TPost_Thread.PostP4(Data1: Pointer; Data2: TCoreClassObject; OnSync: TThreadPostProc4);
begin
  FPost.PostP4(Data1, Data2, OnSync);
end;

procedure TPost_ThreadPool.AddTh(th: TPost_Thread);
begin
  FCritical.Lock;
  Add(th);
  FCritical.UnLock;
end;

procedure TPost_ThreadPool.RemoveTh(th: TPost_Thread);
var
  i: Integer;
begin
  FCritical.Lock;
  i := 0;
  while i < Count do
    begin
      if items[i] = th then
          Delete(i)
      else
          inc(i);
    end;
  FCritical.UnLock;
end;

constructor TPost_ThreadPool.Create(ThNum_: Integer);
var
  i: Integer;
begin
  inherited Create;
  FCritical := TCritical.Create;
  FNextID := 0;
  FQueueOptimized := True;
  for i := 0 to ThNum_ - 1 do
      TPost_Thread.Create(Self);
  while ThNum() < ThNum_ do
      TCompute.Sleep(1);
end;

destructor TPost_ThreadPool.Destroy;
var
  i: Integer;
begin
  FCritical.Lock;
  for i := 0 to Count - 1 do
      items[i].FActivted.V := False;
  FCritical.UnLock;

  while ThNum > 0 do
      TCompute.Sleep(1);

  DisposeObject(FCritical);
  inherited Destroy;
end;

function TPost_ThreadPool.ThNum: Integer;
begin
  FCritical.Lock;
  Result := Count;
  FCritical.UnLock;
end;

function TPost_ThreadPool.TaskNum: Integer;
var
  i: Integer;
begin
  FCritical.Lock;
  Result := 0;
  for i := 0 to Count - 1 do
      inc(Result, items[i].FPost.Count);
  FCritical.UnLock;
end;

procedure TPost_ThreadPool.Wait;
begin
  while TaskNum > 0 do
      TCompute.Sleep(1);
end;

procedure TPost_ThreadPool.Wait(th: TPost_Thread);
begin
  while th.FPost.Count > 0 do
      TCompute.Sleep(1);
end;

function TPost_ThreadPool.Next_Thread: TPost_Thread;
begin
  if ThNum = 0 then
      RaiseInfo('pool is empty.');
  FCritical.Acquire;
  try
    if FNextID >= Count then
        FNextID := 0;
    Result := items[FNextID];
    inc(FNextID);
  finally
      FCritical.Release;
  end;
end;

function TPost_ThreadPool.MinLoad_Thread: TPost_Thread;
var
  i, id_: Integer;
  th: TPost_Thread;
begin
  if ThNum = 0 then
      RaiseInfo('pool is empty.');
  FCritical.Acquire;
  try
    for i := 0 to Count - 1 do
      if (not items[i].FPost.Busy) then
        begin
          if (FQueueOptimized) and (i < Count - 1) then
              Move(i, Count - 1);
          Result := items[i];
          exit;
        end;

    th := items[0];
    id_ := 0;
    for i := 1 to Count - 1 do
      if items[i].FPost.Count < th.FPost.Count then
        begin
          th := items[i];
          id_ := i;
        end;
    if (FQueueOptimized) and (id_ < Count - 1) then
        Move(id_, Count - 1);
    Result := th;
  finally
      FCritical.Release;
  end;
end;

function TPost_ThreadPool.IDLE_Thread: TPost_Thread;
var
  i: Integer;
begin
  if ThNum = 0 then
      RaiseInfo('pool is empty.');
  FCritical.Acquire;
  Result := nil;
  try
    for i := 0 to Count - 1 do
      if not items[i].FPost.Busy then
          exit(items[i]);
  finally
      FCritical.Release;
  end;
end;

procedure TPost_ThreadPool.DoTestCall;
begin
  DoStatus('current post thread: %d', [TCompute.CurrentThread.ThreadID]);
end;

class procedure TPost_ThreadPool.Test;
var
  pool: TPost_ThreadPool;
  i: Integer;
begin
  pool := TPost_ThreadPool.Create(2);
  for i := 0 to 9 do
      pool.Next_Thread.PostM1({$IFDEF FPC}@{$ENDIF FPC}pool.DoTestCall);
  pool.Wait;
  for i := 0 to 9 do
      pool.MinLoad_Thread.PostM1({$IFDEF FPC}@{$ENDIF FPC}pool.DoTestCall);
  pool.Wait;
  DisposeObject(pool);
end;

end.
