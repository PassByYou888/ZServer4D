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
    procedure AssignTo(th: TCompute);
  end;

  PComputeDispatchData = ^TComputeDispatch;

  TCoreComputeThreadPool = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TCompute>;
  TComputeDispatchPool = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<PComputeDispatchData>;

  TParallelOverflow = record
  public
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
  ComputeDispatchPool: TComputeDispatchPool;
  IdleComputeThreadSum: TAtomInt;

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

procedure TComputeDispatch.AssignTo(th: TCompute);
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
  Result := (MaxActivtedParallel > 0) and (ActivtedParallel >= MaxActivtedParallel);
end;

function PickOrCreateThread(): TCompute;
begin
  Result := TCompute.Create;
  CoreThreadPool.Add(Result);
end;

procedure PostComputeDispatchData(var Data: TComputeDispatch);
var
  tk: TTimeTick;
  done: Boolean;
  th: TCompute;
begin
  // check for idle thread, and again run.
  if IdleComputeThreadSum.V > 0 then
    begin
      ComputeDispatchCritical.Acquire;
      ComputeDispatchPool.Add(@Data);
      ComputeDispatchCritical.Release;
      tk := GetTimeTick();
      while (IdleComputeThreadSum.V > 0) and (GetTimeTick() - tk < 5) do
        begin
          ComputeDispatchCritical.Acquire;
          done := ComputeDispatchPool.IndexOf(@Data) < 0;
          ComputeDispatchCritical.Release;
          if done then
              exit;
        end;
      ComputeDispatchCritical.Acquire;
      done := ComputeDispatchPool.IndexOf(@Data) < 0;
      if not done then
          ComputeDispatchPool.Remove(@Data);
      ComputeDispatchCritical.Release;
      if done then
          exit;
    end;

  // create thread
  ComputeDispatchCritical.Acquire;
  inc(ComputeThreadTaskRunning.LockP()^);
  ComputeThreadTaskRunning.Unlock;
  th := PickOrCreateThread();
  Data.AssignTo(th);
  th.Start();
  ComputeDispatchCritical.Release;
end;

procedure InitCoreThreadPool(Thread_Num: Integer);
var
  th: TCompute;
begin
  CoreThreadPool := TCoreComputeThreadPool.Create;
  ComputeThreadTaskRunning := TAtomInteger.Create(0);
  ParallelGranularity := Thread_Num;
  ComputeDispatchCritical := TCritical.Create;
  MaxActivtedParallel := 0;
  ParallelOverflow.ActivtedParallel := 0;
  ComputeDispatchPool := TComputeDispatchPool.Create;
  IdleComputeThreadSum := TAtomInt.Create(0);
end;

procedure FreeCoreThreadPool;
begin
  while TCompute.ActivtedTask() > 0 do
      CheckThreadSynchronize(1);

  CoreThreadPool.Free;
  CoreThreadPool := nil;

  ComputeThreadTaskRunning.Free;
  ComputeThreadTaskRunning := nil;

  ComputeDispatchCritical.Free;
  ComputeDispatchCritical := nil;

  ComputeDispatchPool.Free;
  ComputeDispatchPool := nil;

  IdleComputeThreadSum.Free;
  IdleComputeThreadSum := nil;
end;

procedure TCompute.Execute;
var
  tk: TTimeTick;
  NoTask: Boolean;
  i: Integer;
begin
  FRndInstance := InternalMT19937__();
  AtomInc(PMD19937Core(FRndInstance)^.Instance);

  while True do
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

      // check for idle thread, and again run.
      tk := GetTimeTick;
      NoTask := True;
      inc(IdleComputeThreadSum.LockP()^);
      IdleComputeThreadSum.Unlock();
      for i := 1 to 100 do
        begin
          while NoTask and (GetTimeTick - tk < 10) do
            begin
              ComputeDispatchCritical.Acquire;
              if ComputeDispatchPool.Count > 0 then
                begin
                  ComputeDispatchPool[0]^.AssignTo(Self);
                  ComputeDispatchPool.Delete(0);
                  NoTask := False;
                end;
              ComputeDispatchCritical.Release;
            end;
          if not NoTask then
              break;
          // little delay
          Sleep(1);
        end;
      dec(IdleComputeThreadSum.LockP()^);
      IdleComputeThreadSum.Unlock();
      if NoTask then
          break;
    end;

  dec(ComputeThreadTaskRunning.LockP()^);
  ComputeThreadTaskRunning.Unlock();

  ComputeDispatchCritical.Acquire;
  CoreThreadPool.Remove(Self);
  ComputeDispatchCritical.Release;

  AtomDec(PMD19937Core(FRndInstance)^.Instance);
  FRndInstance := nil;
  RemoveMT19937Thread(Self);
end;

procedure TCompute.Done_Sync;
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

constructor TCompute.Create;
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
  FRndInstance := nil;
end;

destructor TCompute.Destroy;
begin
  inherited Destroy;
end;

class function TCompute.ActivtedTask(): Integer;
begin
  ComputeDispatchCritical.Acquire;
  Result := CoreThreadPool.Count;
  ComputeDispatchCritical.Release;
end;

class function TCompute.WaitTask(): Integer;
begin
  Result := IdleComputeThreadSum.V;
end;

class function TCompute.TotalTask(): Integer;
begin
  Result := ComputeThreadTaskRunning.V;
end;

class function TCompute.State(): string;
begin
  Result := Format('total: %d Activted: %d Waiting: %d Granularity: %d MaxParallel: %d/%d',
    [TotalTask(), ActivtedTask(), WaitTask(), ParallelGranularity, ParallelOverflow.ActivtedParallel, MaxActivtedParallel]);
end;

class function TCompute.GetParallelGranularity: Integer;
begin
  Result := ParallelGranularity;
end;

class function TCompute.GetMaxActivtedParallel: Integer;
begin
  Result := MaxActivtedParallel;
end;

type
  TSyncTmp = class
  private
    OnRunC: TRunWithThreadCall_NP;
    OnRunM: TRunWithThreadMethod_NP;
    OnRunP: TRunWithThreadProc_NP;
    procedure DoSync;
  public
    constructor Create;
  end;

procedure TSyncTmp.DoSync;
begin
  try
    if Assigned(OnRunC) then
        OnRunC();
    if Assigned(OnRunM) then
        OnRunM();
    if Assigned(OnRunP) then
        OnRunP();
  except
  end;
  Free;
end;

constructor TSyncTmp.Create;
begin
  inherited Create;
  OnRunC := nil;
  OnRunM := nil;
  OnRunP := nil;
end;

class procedure TCompute.Sync(const OnRun_: TRunWithThreadProc_NP);
var
  tmp: TSyncTmp;
begin
  tmp := TSyncTmp.Create;
  tmp.OnRunP := OnRun_;
  TCompute.Synchronize(TCompute.CurrentThread, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoSync);
end;

class procedure TCompute.Sync(const Thread_: TThread; OnRun_: TRunWithThreadProc_NP);
var
  tmp: TSyncTmp;
begin
  tmp := TSyncTmp.Create;
  tmp.OnRunP := OnRun_;
  TCompute.Synchronize(Thread_, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoSync);
end;

class procedure TCompute.SyncC(OnRun_: TRunWithThreadCall_NP);
var
  tmp: TSyncTmp;
begin
  tmp := TSyncTmp.Create;
  tmp.OnRunC := OnRun_;
  TCompute.Synchronize(TCompute.CurrentThread, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoSync);
end;

class procedure TCompute.SyncC(const Thread_: TThread; OnRun_: TRunWithThreadCall_NP);
var
  tmp: TSyncTmp;
begin
  tmp := TSyncTmp.Create;
  tmp.OnRunC := OnRun_;
  TCompute.Synchronize(Thread_, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoSync);
end;

class procedure TCompute.SyncM(OnRun_: TRunWithThreadMethod_NP);
var
  tmp: TSyncTmp;
begin
  tmp := TSyncTmp.Create;
  tmp.OnRunM := OnRun_;
  TCompute.Synchronize(TCompute.CurrentThread, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoSync);
end;

class procedure TCompute.SyncM(const Thread_: TThread; OnRun_: TRunWithThreadMethod_NP);
var
  tmp: TSyncTmp;
begin
  tmp := TSyncTmp.Create;
  tmp.OnRunM := OnRun_;
  TCompute.Synchronize(Thread_, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoSync);
end;

class procedure TCompute.SyncP(const OnRun_: TRunWithThreadProc_NP);
var
  tmp: TSyncTmp;
begin
  tmp := TSyncTmp.Create;
  tmp.OnRunP := OnRun_;
  TCompute.Synchronize(TCompute.CurrentThread, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoSync);
end;

class procedure TCompute.SyncP(const Thread_: TThread; OnRun_: TRunWithThreadProc_NP);
var
  tmp: TSyncTmp;
begin
  tmp := TSyncTmp.Create;
  tmp.OnRunP := OnRun_;
  TCompute.Synchronize(Thread_, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoSync);
end;

class procedure TCompute.RunC(const Data: Pointer; const Obj: TCoreClassObject; const OnRun, OnDone: TRunWithThreadCall);
var
  Dispatch_: TComputeDispatch;
begin
  Dispatch_.Init;
  Dispatch_.OnRunCall := OnRun;
  Dispatch_.OnDoneCall := OnDone;
  Dispatch_.UserData := Data;
  Dispatch_.UserObject := Obj;
  PostComputeDispatchData(Dispatch_);
end;

class procedure TCompute.RunC(const Data: Pointer; const Obj: TCoreClassObject; const OnRun: TRunWithThreadCall);
var
  Dispatch_: TComputeDispatch;
begin
  Dispatch_.Init;
  Dispatch_.OnRunCall := OnRun;
  Dispatch_.UserData := Data;
  Dispatch_.UserObject := Obj;
  PostComputeDispatchData(Dispatch_);
end;

class procedure TCompute.RunC(const OnRun: TRunWithThreadCall);
var
  Dispatch_: TComputeDispatch;
begin
  Dispatch_.Init;
  Dispatch_.OnRunCall := OnRun;
  Dispatch_.UserData := nil;
  Dispatch_.UserObject := nil;
  PostComputeDispatchData(Dispatch_);
end;

class procedure TCompute.RunC_NP(const OnRun: TRunWithThreadCall_NP);
var
  Dispatch_: TComputeDispatch;
begin
  Dispatch_.Init;
  Dispatch_.OnRunCall_NP := OnRun;
  Dispatch_.UserData := nil;
  Dispatch_.UserObject := nil;
  PostComputeDispatchData(Dispatch_);
end;

class procedure TCompute.RunM(const Data: Pointer; const Obj: TCoreClassObject; const OnRun, OnDone: TRunWithThreadMethod);
var
  Dispatch_: TComputeDispatch;
begin
  Dispatch_.Init;
  Dispatch_.OnRunMethod := OnRun;
  Dispatch_.OnDoneMethod := OnDone;
  Dispatch_.UserData := Data;
  Dispatch_.UserObject := Obj;
  PostComputeDispatchData(Dispatch_);
end;

class procedure TCompute.RunM(const Data: Pointer; const Obj: TCoreClassObject; const OnRun: TRunWithThreadMethod);
var
  Dispatch_: TComputeDispatch;
begin
  Dispatch_.Init;
  Dispatch_.OnRunMethod := OnRun;
  Dispatch_.UserData := Data;
  Dispatch_.UserObject := Obj;
  PostComputeDispatchData(Dispatch_);
end;

class procedure TCompute.RunM(const OnRun: TRunWithThreadMethod);
var
  Dispatch_: TComputeDispatch;
begin
  Dispatch_.Init;
  Dispatch_.OnRunMethod := OnRun;
  Dispatch_.UserData := nil;
  Dispatch_.UserObject := nil;
  PostComputeDispatchData(Dispatch_);
end;

class procedure TCompute.RunM_NP(const OnRun: TRunWithThreadMethod_NP);
var
  Dispatch_: TComputeDispatch;
begin
  Dispatch_.Init;
  Dispatch_.OnRunMethod_NP := OnRun;
  Dispatch_.UserData := nil;
  Dispatch_.UserObject := nil;
  PostComputeDispatchData(Dispatch_);
end;

class procedure TCompute.RunP(const Data: Pointer; const Obj: TCoreClassObject; const OnRun, OnDone: TRunWithThreadProc);
var
  Dispatch_: TComputeDispatch;
begin
  Dispatch_.Init;
  Dispatch_.OnRunProc := OnRun;
  Dispatch_.OnDoneProc := OnDone;
  Dispatch_.UserData := Data;
  Dispatch_.UserObject := Obj;
  PostComputeDispatchData(Dispatch_);
end;

class procedure TCompute.RunP(const Data: Pointer; const Obj: TCoreClassObject; const OnRun: TRunWithThreadProc);
var
  Dispatch_: TComputeDispatch;
begin
  Dispatch_.Init;
  Dispatch_.OnRunProc := OnRun;
  Dispatch_.UserData := Data;
  Dispatch_.UserObject := Obj;
  PostComputeDispatchData(Dispatch_);
end;

class procedure TCompute.RunP(const OnRun: TRunWithThreadProc);
var
  Dispatch_: TComputeDispatch;
begin
  Dispatch_.Init;
  Dispatch_.OnRunProc := OnRun;
  Dispatch_.UserData := nil;
  Dispatch_.UserObject := nil;
  PostComputeDispatchData(Dispatch_);
end;

class procedure TCompute.RunP_NP(const OnRun: TRunWithThreadProc_NP);
var
  Dispatch_: TComputeDispatch;
begin
  Dispatch_.Init;
  Dispatch_.OnRunProc_NP := OnRun;
  Dispatch_.UserData := nil;
  Dispatch_.UserObject := nil;
  PostComputeDispatchData(Dispatch_);
end;

class procedure TCompute.ProgressPost();
begin
  MainThreadProgress.Progress();
end;

class procedure TCompute.PostC1(OnSync: TThreadProgressPostCall1);
begin
  MainThreadProgress.PostC1(OnSync);
end;

class procedure TCompute.PostC2(Data1: Pointer; OnSync: TThreadProgressPostCall2);
begin
  MainThreadProgress.PostC2(Data1, OnSync);
end;

class procedure TCompute.PostC3(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant; OnSync: TThreadProgressPostCall3);
begin
  MainThreadProgress.PostC3(Data1, Data2, Data3, OnSync);
end;

class procedure TCompute.PostM1(OnSync: TThreadProgressPostMethod1);
begin
  MainThreadProgress.PostM1(OnSync);
end;

class procedure TCompute.PostM2(Data1: Pointer; OnSync: TThreadProgressPostMethod2);
begin
  MainThreadProgress.PostM2(Data1, OnSync);
end;

class procedure TCompute.PostM3(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant; OnSync: TThreadProgressPostMethod3);
begin
  MainThreadProgress.PostM3(Data1, Data2, Data3, OnSync);
end;

class procedure TCompute.PostP1(OnSync: TThreadProgressPostProc1);
begin
  MainThreadProgress.PostP1(OnSync);
end;

class procedure TCompute.PostP2(Data1: Pointer; OnSync: TThreadProgressPostProc2);
begin
  MainThreadProgress.PostP2(Data1, OnSync);
end;

class procedure TCompute.PostP3(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant; OnSync: TThreadProgressPostProc3);
begin
  MainThreadProgress.PostP3(Data1, Data2, Data3, OnSync);
end;
