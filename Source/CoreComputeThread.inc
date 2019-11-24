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
type
  TComputeDispatch = record
    OnRunCall: TRunWithThreadCall;
    OnRunMethod: TRunWithThreadMethod;
    OnRunProc: TRunWithThreadProc;
    OnRunCall_NP: TRunWithThreadCall_NP;
    OnRunMethod_NP: TRunWithThreadMethod_NP;
    OnRunProc_NP: TRunWithThreadProc_NP;
    OnDoneCall: TRunWithThreadCall;
    OnDoneMethod: TRunWithThreadMethod;
    OnDoneProc: TRunWithThreadProc;
    UserData: Pointer;
    UserObject: TCoreClassObject;
    procedure Init;
    procedure AssignTo(th: TComputeThread);
  end;

  PComputeDispatchData = ^TComputeDispatch;

  TCoreComputeThreadPool = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TComputeThread>;

  TParallelOverflow = record
    ActivtedParallel: Integer;
    procedure Acquire;
    procedure Release;
    function Busy(): Boolean; inline;
  end;

var
  CoreThreadPool: TCoreComputeThreadPool;
  ComputeDispatchCritical: TCritical;
  ComputeThreadTaskRunning: TAtomInteger;
  ParallelGranularity: Integer;
  MaxActivtedParallel: Integer;
  ParallelOverflow: TParallelOverflow;

procedure TComputeDispatch.Init;
begin
  OnRunCall := nil;
  OnRunMethod := nil;
  OnRunProc := nil;
  OnRunCall_NP := nil;
  OnRunMethod_NP := nil;
  OnRunProc_NP := nil;
  OnDoneCall := nil;
  OnDoneMethod := nil;
  OnDoneProc := nil;
  UserData := nil;
  UserObject := nil;
end;

procedure TComputeDispatch.AssignTo(th: TComputeThread);
begin
  th.OnRunCall := OnRunCall;
  th.OnRunMethod := OnRunMethod;
  th.OnRunProc := OnRunProc;
  th.OnRunCall_NP := OnRunCall_NP;
  th.OnRunMethod_NP := OnRunMethod_NP;
  th.OnRunProc_NP := OnRunProc_NP;
  th.OnDoneCall := OnDoneCall;
  th.OnDoneMethod := OnDoneMethod;
  th.OnDoneProc := OnDoneProc;
  th.UserData := UserData;
  th.UserObject := UserObject;
end;

procedure TParallelOverflow.Acquire;
begin
  while Busy() do
      TCoreClassThread.Sleep(1);
  AtomInc(ActivtedParallel);
end;

procedure TParallelOverflow.Release;
begin
  AtomDec(ActivtedParallel);
end;

function TParallelOverflow.Busy(): Boolean;
begin
  Result := ActivtedParallel >= MaxActivtedParallel;
end;

function PickOrCreateThread(): TComputeThread;
begin
  Result := TComputeThread.Create;
  CoreThreadPool.Add(Result);
end;

procedure PostComputeDispatchData(var data: TComputeDispatch);
var
  th: TComputeThread;
begin
  ComputeDispatchCritical.Acquire;
  inc(ComputeThreadTaskRunning.LockP()^);
  ComputeThreadTaskRunning.Unlock;
  th := PickOrCreateThread();
  data.AssignTo(th);
  th.Start();
  ComputeDispatchCritical.Release;
end;

procedure InitCoreThreadPool(Thread_Num: Integer);
var
  th: TComputeThread;
begin
  CoreThreadPool := TCoreComputeThreadPool.Create;
  ComputeThreadTaskRunning := TAtomInteger.Create(0);
  ParallelGranularity := Thread_Num;
  ComputeDispatchCritical := TCritical.Create;
{$IFDEF SystemParallel}
  MaxActivtedParallel := 1;
{$ELSE SystemParallel}
  MaxActivtedParallel := CpuCount;
{$ENDIF SystemParallel}
  ParallelOverflow.ActivtedParallel := 0;
end;

procedure FreeCoreThreadPool;
begin
  while TComputeThread.ActivtedTask() > 0 do
      CheckThreadSynchronize(1);

  CoreThreadPool.Free;
  CoreThreadPool := nil;

  ComputeThreadTaskRunning.Free;
  ComputeThreadTaskRunning := nil;

  ComputeDispatchCritical.Free;
  ComputeDispatchCritical := nil;
end;

procedure TComputeThread.Execute;
begin
  try
{$IFDEF MT19937SeedOnTComputeThreadIs0} SetMT19937Seed(0); {$ELSE MT19937SeedOnTComputeThreadIs0} MT19937Randomize(); {$ENDIF MT19937SeedOnTComputeThreadIs0}
    if Assigned(OnRunCall) then
        OnRunCall(Self);
    if Assigned(OnRunMethod) then
        OnRunMethod(Self);
    if Assigned(OnRunProc) then
        OnRunProc(Self);
    if Assigned(OnRunCall_NP) then
        OnRunCall_NP();
    if Assigned(OnRunMethod_NP) then
        OnRunMethod_NP();
    if Assigned(OnRunProc_NP) then
        OnRunProc_NP();
  except
  end;
  if Assigned(OnDoneCall) or Assigned(OnDoneMethod) or Assigned(OnDoneProc) then
      Synchronize({$IFDEF FPC}@{$ENDIF FPC}Done_Sync);

  Dec(ComputeThreadTaskRunning.LockP()^);
  ComputeThreadTaskRunning.Unlock();

  ComputeDispatchCritical.Acquire;
  CoreThreadPool.Remove(Self);
  ComputeDispatchCritical.Release;

  RemoveMT19937Thread(Self);
end;

procedure TComputeThread.Done_Sync;
begin
  try
    if Assigned(OnDoneCall) then
        OnDoneCall(Self);
    if Assigned(OnDoneMethod) then
        OnDoneMethod(Self);
    if Assigned(OnDoneProc) then
        OnDoneProc(Self);
  except
  end;
end;

constructor TComputeThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
  OnRunCall := nil;
  OnRunMethod := nil;
  OnRunProc := nil;
  OnRunCall_NP := nil;
  OnRunMethod_NP := nil;
  OnRunProc_NP := nil;
  OnDoneCall := nil;
  OnDoneMethod := nil;
  OnDoneProc := nil;
  UserData := nil;
  UserObject := nil;
end;

destructor TComputeThread.Destroy;
begin
  inherited Destroy;
end;

class function TComputeThread.ActivtedTask(): Integer;
begin
  ComputeDispatchCritical.Acquire;
  Result := CoreThreadPool.Count;
  ComputeDispatchCritical.Release;
end;

class function TComputeThread.WaitTask(): Integer;
begin
  Result := ComputeThreadTaskRunning.V - ActivtedTask();
end;

class function TComputeThread.TotalTask(): Integer;
begin
  Result := ComputeThreadTaskRunning.V;
end;

class function TComputeThread.State(): string;
begin
  Result := Format('total: %d Activted: %d Waiting: %d', [TotalTask(), ActivtedTask(), WaitTask()]);
end;

class procedure TComputeThread.RunC(const data: Pointer; const Obj: TCoreClassObject; const OnRun, OnDone: TRunWithThreadCall);
var
  Dispatch_: TComputeDispatch;
begin
  Dispatch_.Init;
  Dispatch_.OnRunCall := OnRun;
  Dispatch_.OnDoneCall := OnDone;
  Dispatch_.UserData := data;
  Dispatch_.UserObject := Obj;
  PostComputeDispatchData(Dispatch_);
end;

class procedure TComputeThread.RunC(const data: Pointer; const Obj: TCoreClassObject; const OnRun: TRunWithThreadCall);
var
  Dispatch_: TComputeDispatch;
begin
  Dispatch_.Init;
  Dispatch_.OnRunCall := OnRun;
  Dispatch_.UserData := data;
  Dispatch_.UserObject := Obj;
  PostComputeDispatchData(Dispatch_);
end;

class procedure TComputeThread.RunC(const OnRun: TRunWithThreadCall);
var
  Dispatch_: TComputeDispatch;
begin
  Dispatch_.Init;
  Dispatch_.OnRunCall := OnRun;
  Dispatch_.UserData := nil;
  Dispatch_.UserObject := nil;
  PostComputeDispatchData(Dispatch_);
end;

class procedure TComputeThread.RunC_NP(const OnRun: TRunWithThreadCall_NP);
var
  Dispatch_: TComputeDispatch;
begin
  Dispatch_.Init;
  Dispatch_.OnRunCall_NP := OnRun;
  Dispatch_.UserData := nil;
  Dispatch_.UserObject := nil;
  PostComputeDispatchData(Dispatch_);
end;

class procedure TComputeThread.RunM(const data: Pointer; const Obj: TCoreClassObject; const OnRun, OnDone: TRunWithThreadMethod);
var
  Dispatch_: TComputeDispatch;
begin
  Dispatch_.Init;
  Dispatch_.OnRunMethod := OnRun;
  Dispatch_.OnDoneMethod := OnDone;
  Dispatch_.UserData := data;
  Dispatch_.UserObject := Obj;
  PostComputeDispatchData(Dispatch_);
end;

class procedure TComputeThread.RunM(const data: Pointer; const Obj: TCoreClassObject; const OnRun: TRunWithThreadMethod);
var
  Dispatch_: TComputeDispatch;
begin
  Dispatch_.Init;
  Dispatch_.OnRunMethod := OnRun;
  Dispatch_.UserData := data;
  Dispatch_.UserObject := Obj;
  PostComputeDispatchData(Dispatch_);
end;

class procedure TComputeThread.RunM(const OnRun: TRunWithThreadMethod);
var
  Dispatch_: TComputeDispatch;
begin
  Dispatch_.Init;
  Dispatch_.OnRunMethod := OnRun;
  Dispatch_.UserData := nil;
  Dispatch_.UserObject := nil;
  PostComputeDispatchData(Dispatch_);
end;

class procedure TComputeThread.RunM_NP(const OnRun: TRunWithThreadMethod_NP);
var
  Dispatch_: TComputeDispatch;
begin
  Dispatch_.Init;
  Dispatch_.OnRunMethod_NP := OnRun;
  Dispatch_.UserData := nil;
  Dispatch_.UserObject := nil;
  PostComputeDispatchData(Dispatch_);
end;

class procedure TComputeThread.RunP(const data: Pointer; const Obj: TCoreClassObject; const OnRun, OnDone: TRunWithThreadProc);
var
  Dispatch_: TComputeDispatch;
begin
  Dispatch_.Init;
  Dispatch_.OnRunProc := OnRun;
  Dispatch_.OnDoneProc := OnDone;
  Dispatch_.UserData := data;
  Dispatch_.UserObject := Obj;
  PostComputeDispatchData(Dispatch_);
end;

class procedure TComputeThread.RunP(const data: Pointer; const Obj: TCoreClassObject; const OnRun: TRunWithThreadProc);
var
  Dispatch_: TComputeDispatch;
begin
  Dispatch_.Init;
  Dispatch_.OnRunProc := OnRun;
  Dispatch_.UserData := data;
  Dispatch_.UserObject := Obj;
  PostComputeDispatchData(Dispatch_);
end;

class procedure TComputeThread.RunP(const OnRun: TRunWithThreadProc);
var
  Dispatch_: TComputeDispatch;
begin
  Dispatch_.Init;
  Dispatch_.OnRunProc := OnRun;
  Dispatch_.UserData := nil;
  Dispatch_.UserObject := nil;
  PostComputeDispatchData(Dispatch_);
end;

class procedure TComputeThread.RunP_NP(const OnRun: TRunWithThreadProc_NP);
var
  Dispatch_: TComputeDispatch;
begin
  Dispatch_.Init;
  Dispatch_.OnRunProc_NP := OnRun;
  Dispatch_.UserData := nil;
  Dispatch_.UserObject := nil;
  PostComputeDispatchData(Dispatch_);
end;
