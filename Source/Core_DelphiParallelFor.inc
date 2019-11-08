{$IFDEF SystemParallel}


procedure DelphiParallelFor(b, e: Integer; OnFor: TDelphiParallelForProcedure32);
begin
  TParallel.&For(b, e, OnFor);
end;

procedure DelphiParallelFor(b, e: Int64; OnFor: TDelphiParallelForProcedure64);
begin
  TParallel.&For(b, e, OnFor);
end;
{$ELSE SystemParallel}


type
  TDelphiParallelThData32 = record
    b, e: Integer;
    Completed: ^Integer;
    OnFor: TDelphiParallelForProcedure32;
  end;

  PDelphiParallelThData32 = ^TDelphiParallelThData32;

procedure DelphiParallelTh32(ThSender: TComputeThread);
var
  p: PDelphiParallelThData32;
  Pass: Integer;
begin
  p := ThSender.UserData;
  for Pass := p^.b to p^.e do
      p^.OnFor(Pass);
  AtomInc(p^.Completed^, p^.e - p^.b + 1);
  dispose(p);
end;

procedure DelphiParallelFor(b, e: Integer; OnFor: TDelphiParallelForProcedure32);
var
  Total, Depth, Completed, StepTotal, stepW, Pass, w: Integer;
  p: PDelphiParallelThData32;
begin
  if b > e then
      Swap(b, e);

  Depth := ParallelGranularity;
  Total := e - b + 1;

  Completed := 0;

  if (Total < Depth) then
    begin
      for Pass := b to e do
        begin
          new(p);
          p^.b := Pass;
          p^.e := Pass;
          p^.Completed := @Completed;
          p^.OnFor := OnFor;
          TComputeThread.RunC(p, nil, DelphiParallelTh32);
        end;
    end
  else
    begin
      stepW := Total div Depth;
      StepTotal := Total div stepW;
      if Total mod stepW > 0 then
          inc(StepTotal);

      for Pass := 0 to StepTotal - 1 do
        begin
          w := stepW * Pass;
          new(p);
          if w + stepW <= Total then
            begin
              p^.b := w + b;
              p^.e := w + stepW + b - 1;
            end
          else
            begin
              p^.b := w + b;
              p^.e := Total + b - 1;
            end;
          p^.Completed := @Completed;
          p^.OnFor := OnFor;
          TComputeThread.RunC(p, nil, DelphiParallelTh32);
        end;
    end;

  while Completed < Total do
      TThread.Sleep(1);
end;

type
  TDelphiParallelThData64 = record
    b, e: Int64;
    Completed: ^Int64;
    OnFor: TDelphiParallelForProcedure64;
  end;

  PDelphiParallelThData64 = ^TDelphiParallelThData64;

procedure DelphiParallelTh64(ThSender: TComputeThread);
var
  p: PDelphiParallelThData32;
  Pass: Integer;
begin
  p := ThSender.UserData;
  for Pass := p^.b to p^.e do
      p^.OnFor(Pass);
  AtomInc(p^.Completed^, p^.e - p^.b + 1);
  dispose(p);
end;

procedure DelphiParallelFor(b, e: Int64; OnFor: TDelphiParallelForProcedure64);
var
  Total, Depth, Completed, StepTotal, stepW, Pass, w: Int64;
  p: PDelphiParallelThData64;
begin
  if b > e then
      Swap(b, e);

  Depth := ParallelGranularity;
  Total := e - b + 1;

  Completed := 0;

  if (Total < Depth) then
    begin
      for Pass := b to e do
        begin
          new(p);
          p^.b := Pass;
          p^.e := Pass;
          p^.Completed := @Completed;
          p^.OnFor := OnFor;
          TComputeThread.RunC(p, nil, DelphiParallelTh64);
        end;
    end
  else
    begin
      stepW := Total div Depth;
      StepTotal := Total div stepW;
      if Total mod stepW > 0 then
          inc(StepTotal);

      for Pass := 0 to StepTotal - 1 do
        begin
          w := stepW * Pass;
          new(p);
          if w + stepW <= Total then
            begin
              p^.b := w + b;
              p^.e := w + stepW + b - 1;
            end
          else
            begin
              p^.b := w + b;
              p^.e := Total + b - 1;
            end;
          p^.Completed := @Completed;
          p^.OnFor := OnFor;
          TComputeThread.RunC(p, nil, DelphiParallelTh64);
        end;
    end;

  while Completed < Total do
      TThread.Sleep(1);
end;
{$ENDIF SystemParallel}
