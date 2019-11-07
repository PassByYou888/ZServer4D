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
  end;

  PComputeDispatchData = ^TComputeDispatch;

  TCoreComputeThreadPool = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TComputeThread>;
  TComputeDispatchDataPool = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<PComputeDispatchData>;

  TComputeDispatchTh = class(TCoreClassThread)
  protected
    function PickedIdleThread: TComputeThread; inline;
    procedure Execute; override;
  end;

var
  CoreThreadPool: TCoreComputeThreadPool;
  CoreThreadRunning_: Boolean;
  ComputeThreadDispatchSleep: Integer;
  ComputeThreadTaskRunning: Integer;

  // dispatch compute
  ComputeDispatchCritical: TCritical;
  ComputeDispatchDataPool: TComputeDispatchDataPool;
  ComputeDispatchTh: TComputeDispatchTh;

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

function TComputeDispatchTh.PickedIdleThread: TComputeThread;
var
  i: Integer;
begin
  while CoreThreadRunning_ do
    begin
      for i := 0 to CoreThreadPool.Count - 1 do
        begin
          if not CoreThreadPool[i].Running then
            begin
              Result := CoreThreadPool[i];
              exit;
            end;
        end;
      TCoreClassThread.Sleep(ComputeThreadDispatchSleep);
    end;
  Result := nil;
end;

procedure TComputeDispatchTh.Execute;
var
  p: PComputeDispatchData;
  cTh: TComputeThread;
  i: Integer;
begin
  while CoreThreadRunning_ do
    begin
      if ComputeDispatchDataPool.Count > 0 then
        begin
          ComputeDispatchCritical.Acquire;
          p := ComputeDispatchDataPool[0];
          ComputeDispatchDataPool.Delete(0);
          ComputeDispatchCritical.Release;

          if not CoreThreadRunning_ then
              break;

          cTh := PickedIdleThread();

          if cTh = nil then
              break;

          cTh.OnRunCall := p^.OnRunCall;
          cTh.OnRunMethod := p^.OnRunMethod;
{$IFNDEF FPC} cTh.OnRunProc := p^.OnRunProc; {$ENDIF FPC}
          //
          cTh.OnRunCall_NP := p^.OnRunCall_NP;
          cTh.OnRunMethod_NP := p^.OnRunMethod_NP;
{$IFNDEF FPC} cTh.OnRunProc_NP := p^.OnRunProc_NP; {$ENDIF FPC}
          //
          cTh.OnDoneCall := p^.OnDoneCall;
          cTh.OnDoneMethod := p^.OnDoneMethod;
{$IFNDEF FPC} cTh.OnDoneProc := p^.OnDoneProc; {$ENDIF FPC}
          //
          cTh.UserData := p^.UserData;
          cTh.UserObject := p^.UserObject;
          dispose(p);
          cTh.Running := True;
        end
      else
          TCoreClassThread.Sleep(ComputeThreadDispatchSleep);
    end;

  ComputeDispatchCritical.Acquire;
  for i := 0 to ComputeDispatchDataPool.Count - 1 do
      dispose(ComputeDispatchDataPool[i]);
  ComputeDispatchDataPool.Clear;
  ComputeDispatchCritical.Release;

  ComputeDispatchTh := nil;
end;

procedure PostComputeDispatchData(const data: TComputeDispatch);
var
  p: PComputeDispatchData;
begin
  new(p);
  p^ := data;
  ComputeDispatchCritical.Acquire;
  ComputeDispatchDataPool.Add(p);
  ComputeDispatchCritical.Release;
end;

procedure InitCoreThreadPool(Thread_Num: Integer);
var
  th: TComputeThread;
  i: Integer;
begin
  CoreThreadPool := TCoreComputeThreadPool.Create;
  CoreThreadRunning_ := True;
  ComputeThreadDispatchSleep := 10;
  ComputeThreadTaskRunning := 0;
  for i := 0 to Thread_Num - 1 do
    begin
      th := TComputeThread.Create;
      CoreThreadPool.Add(th);
    end;
  ComputeDispatchCritical := TCritical.Create;
  ComputeDispatchDataPool := TComputeDispatchDataPool.Create;
  ComputeDispatchTh := TComputeDispatchTh.Create(True);
  ComputeDispatchTh.FreeOnTerminate := True;
  ComputeDispatchTh.Suspended := False;
end;

procedure SetCoreThreadDispatch(sleep_: Integer);
begin
  ComputeThreadDispatchSleep := sleep_;
end;

procedure FreeCoreThreadPool;
var
  th: TComputeThread;
begin
  if not CoreThreadRunning_ then
      exit;

  CoreThreadRunning_ := False;

  while (CoreThreadPool.Count > 0) do
    begin
      th := CoreThreadPool.First;
      if not th.Running then
          th.Running := True;
      CheckSynchronize();
    end;
  while (ComputeDispatchTh <> nil) do
      CheckSynchronize();

  CoreThreadPool.Clear;
  ComputeDispatchTh := nil;

  CoreThreadPool.Free;
  CoreThreadPool := nil;

  ComputeDispatchCritical.Free;
  ComputeDispatchCritical := nil;

  ComputeDispatchDataPool.Free;
  ComputeDispatchDataPool := nil;
end;

procedure TComputeThread.Execute;
var
  RunningTimeTick: TTimeTick;
begin
  RunningTimeTick := GetTimeTick;
  while True do
    begin
      while (not Running) and (GetTimeTick - RunningTimeTick < 100) do
          TComputeThread.Sleep(1);

      while not Running do
          TComputeThread.Sleep(ComputeThreadDispatchSleep);

      if not CoreThreadRunning_ then
          break;

      RunningTimeTick := GetTimeTick;

      SetMT19937Seed(0);
      AtomInc(ComputeThreadTaskRunning);
      try
        if Assigned(OnRunCall) then
            OnRunCall(Self);
        if Assigned(OnRunMethod) then
            OnRunMethod(Self);
{$IFNDEF FPC}
        if Assigned(OnRunProc) then
            OnRunProc(Self);
{$ENDIF FPC}
        //
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
      Running := False;
    end;

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

  try
      Synchronize({$IFDEF FPC}@{$ENDIF FPC}Halt_Sync);
  except
  end;
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

procedure TComputeThread.Halt_Sync;
begin
  if (CoreThreadPool <> nil) and (CoreThreadPool.Count > 0) then
      CoreThreadPool.Remove(Self);
  InternalCritical_.Free;
end;

function TComputeThread.GetRunning(): Boolean;
begin
  InternalCritical_.Acquire;
  Result := InternalRunning_;
  InternalCritical_.Release;
end;

procedure TComputeThread.SetRunning(const Value: Boolean);
begin
  InternalCritical_.Acquire;
  InternalRunning_ := Value;
  InternalCritical_.Release;
end;

constructor TComputeThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;

  InternalCritical_ := TCritical.Create;
  InternalRunning_ := False;

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

  Suspended := False;
end;

class function TComputeThread.ActivtedTask(): Integer;
begin
  Result := ComputeThreadTaskRunning;
end;

class function TComputeThread.WaitingTask(): Integer;
begin
  ComputeDispatchCritical.Acquire;
  Result := ComputeDispatchDataPool.Count;
  ComputeDispatchCritical.Release;
end;

class function TComputeThread.TotalTask(): Integer;
begin
  Result := TComputeThread.ActivtedTask() + TComputeThread.WaitingTask();
end;

class function TComputeThread.State(): SystemString;
begin
  Result := Format('total task: %d, activted task: %d, queue task: %d Instance(MT19937): %d',
    [TComputeThread.TotalTask, TComputeThread.ActivtedTask, TComputeThread.WaitingTask, MT19937InstanceNum]);
end;

class procedure TComputeThread.RunC(const data: Pointer; const Obj: TCoreClassObject; const OnRun, OnDone: TRunWithThreadCall);
var
  Dispatch_: TComputeDispatch;
begin
  if not CoreThreadRunning_ then
      exit;
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
  if not CoreThreadRunning_ then
      exit;
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
  if not CoreThreadRunning_ then
      exit;
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
  if not CoreThreadRunning_ then
      exit;
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
  if not CoreThreadRunning_ then
      exit;
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
  if not CoreThreadRunning_ then
      exit;
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
  if not CoreThreadRunning_ then
      exit;
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
  if not CoreThreadRunning_ then
      exit;
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
  if not CoreThreadRunning_ then
      exit;
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
  if not CoreThreadRunning_ then
      exit;
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
  if not CoreThreadRunning_ then
      exit;
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
  if not CoreThreadRunning_ then
      exit;
  Dispatch_.Init;
  Dispatch_.OnRunProc_NP := OnRun;
  Dispatch_.UserData := nil;
  Dispatch_.UserObject := nil;
  PostComputeDispatchData(Dispatch_);
end;

{$ENDIF FPC}
