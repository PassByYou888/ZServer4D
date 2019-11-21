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
{$IFNDEF FPC} OnRunProc: TRunWithThreadProc; {$ENDIF FPC}
    OnRunCall_NP: TRunWithThreadCall_NP;
    OnRunMethod_NP: TRunWithThreadMethod_NP;
{$IFNDEF FPC} OnRunProc_NP: TRunWithThreadProc_NP; {$ENDIF FPC}
    OnDoneCall: TRunWithThreadCall;
    OnDoneMethod: TRunWithThreadMethod;
{$IFNDEF FPC} OnDoneProc: TRunWithThreadProc; {$ENDIF FPC}
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
  ComputeThreadTaskRunning: Integer;
  ParallelGranularity: Integer;
  ComputeDispatchCritical: TCritical;
  MaxActivtedParallel: Integer;
  ParallelOverflow: TParallelOverflow;

procedure TComputeDispatch.Init;
begin
  OnRunCall := nil;
  OnRunMethod := nil;
{$IFNDEF FPC} OnRunProc := nil; {$ENDIF FPC}
  OnRunCall_NP := nil;
  OnRunMethod_NP := nil;
{$IFNDEF FPC} OnRunProc_NP := nil; {$ENDIF FPC}
  OnDoneCall := nil;
  OnDoneMethod := nil;
{$IFNDEF FPC} OnDoneProc := nil; {$ENDIF FPC}
  UserData := nil;
  UserObject := nil;
end;

procedure TComputeDispatch.AssignTo(th: TComputeThread);
begin
  th.OnRunCall := OnRunCall;
  th.OnRunMethod := OnRunMethod;
{$IFNDEF FPC} th.OnRunProc := OnRunProc; {$ENDIF FPC}
  th.OnRunCall_NP := OnRunCall_NP;
  th.OnRunMethod_NP := OnRunMethod_NP;
{$IFNDEF FPC} th.OnRunProc_NP := OnRunProc_NP; {$ENDIF FPC}
  th.OnDoneCall := OnDoneCall;
  th.OnDoneMethod := OnDoneMethod;
{$IFNDEF FPC} th.OnDoneProc := OnDoneProc; {$ENDIF FPC}
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
  AtomInc(ComputeThreadTaskRunning);
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
  ComputeThreadTaskRunning := 0;
  ParallelGranularity := Thread_Num;
  ComputeDispatchCritical := TCritical.Create;
  MaxActivtedParallel := 2;
  ParallelOverflow.ActivtedParallel := 0;
end;

procedure FreeCoreThreadPool;
var
  i: Integer;
begin
  repeat
    ComputeDispatchCritical.Acquire;
    i := CoreThreadPool.Count;
    ComputeDispatchCritical.Release;
  until i = 0;

  CoreThreadPool.Free;
  CoreThreadPool := nil;

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
{$IFNDEF FPC}
    if Assigned(OnRunProc) then
        OnRunProc(Self);
{$ENDIF FPC}
    if Assigned(OnRunCall_NP) then
        OnRunCall_NP();
    if Assigned(OnRunMethod_NP) then
        OnRunMethod_NP();
{$IFNDEF FPC}
    if Assigned(OnRunProc_NP) then
        OnRunProc_NP();
{$ENDIF FPC}
  except
  end;
  if Assigned(OnDoneCall) or Assigned(OnDoneMethod){$IFNDEF FPC} or Assigned(OnDoneProc){$ENDIF FPC} then
      Synchronize({$IFDEF FPC}@{$ENDIF FPC}Done_Sync);
  AtomDec(ComputeThreadTaskRunning);

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
{$IFNDEF FPC}
    if Assigned(OnDoneProc) then
        OnDoneProc(Self);
{$ENDIF FPC}
  except
  end;
end;

constructor TComputeThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
  OnRunCall := nil;
  OnRunMethod := nil;
{$IFNDEF FPC} OnRunProc := nil; {$ENDIF FPC}
  OnRunCall_NP := nil;
  OnRunMethod_NP := nil;
{$IFNDEF FPC} OnRunProc_NP := nil; {$ENDIF FPC}
  OnDoneCall := nil;
  OnDoneMethod := nil;
{$IFNDEF FPC} OnDoneProc := nil; {$ENDIF FPC}
  UserData := nil;
  UserObject := nil;
end;

destructor TComputeThread.Destroy;
begin
  inherited Destroy;
end;

class function TComputeThread.ActivtedTask(): Integer;
begin
  Result := ComputeThreadTaskRunning;
end;

class function TComputeThread.TotalTask(): Integer;
begin
  Result := ComputeThreadTaskRunning;
end;

class function TComputeThread.State(): SystemString;
begin
  Result := Format('activted task: %d', [ComputeThreadTaskRunning]);
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

{$IFNDEF FPC}


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

{$ENDIF FPC}
