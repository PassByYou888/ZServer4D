{$IFDEF SystemParallel}


procedure DelphiParallelFor(b, e: Integer; OnFor: TDelphiParallelForProcedure32);
var
  i_: Integer;
begin
  if ParallelOverflow.Busy() then
    begin
      for i_ := b to e do
          OnFor(i_);
      exit;
    end;
  ParallelOverflow.Acquire;
  try
      TParallel.&For(b, e, OnFor);
  finally
      ParallelOverflow.Release;
  end;
end;

procedure DelphiParallelFor(b, e: Int64; OnFor: TDelphiParallelForProcedure64);
var
  i_: Int64;
begin
  if ParallelOverflow.Busy() then
    begin
      for i_ := b to e do
          OnFor(i_);
      exit;
    end;
  ParallelOverflow.Acquire;
  try
      TParallel.&For(b, e, OnFor);
  finally
      ParallelOverflow.Release;
  end;
end;
{$ELSE SystemParallel}


type
  TDelphiParallelThData32 = record
    b, e: Integer;
    Completed: ^Integer;
    OnFor: TDelphiParallelForProcedure32;
    Critical: TCritical;
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

  p^.Critical.Acquire;
  AtomInc(p^.Completed^, p^.e - p^.b + 1);
  p^.Critical.Release;
  dispose(p);
end;

procedure DelphiParallelFor(b, e: Integer; OnFor: TDelphiParallelForProcedure32);
var
  Total, Depth, Completed, StepTotal, stepW, Pass, w: Integer;
  p: PDelphiParallelThData32;
  i_: Integer;
  Critical: TCritical;
begin
  if b > e then
      Swap(b, e);
  if ParallelOverflow.Busy() then
    begin
      for i_ := b to e do
          OnFor(i_);
      exit;
    end;
  ParallelOverflow.Acquire;
  try
    Depth := ParallelGranularity;
    Total := e - b + 1;
    Critical := TCritical.Create;

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
            p^.Critical := Critical;
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
            p^.Critical := Critical;
            TComputeThread.RunC(p, nil, DelphiParallelTh32);
          end;
      end;

    repeat
      TThread.Sleep(1);
      Critical.Acquire;
      w := Completed;
      Critical.Release;
    until w >= Total;

    Critical.Free;
  finally
      ParallelOverflow.Release;
  end;
end;

type
  TDelphiParallelThData64 = record
    b, e: Int64;
    Completed: ^Int64;
    OnFor: TDelphiParallelForProcedure64;
    Critical: TCritical;
  end;

  PDelphiParallelThData64 = ^TDelphiParallelThData64;

procedure DelphiParallelTh64(ThSender: TComputeThread);
var
  p: PDelphiParallelThData64;
  Pass: Int64;
begin
  p := ThSender.UserData;
  for Pass := p^.b to p^.e do
      p^.OnFor(Pass);

  p^.Critical.Acquire;
  AtomInc(p^.Completed^, p^.e - p^.b + 1);
  p^.Critical.Release;
  dispose(p);
end;

procedure DelphiParallelFor(b, e: Int64; OnFor: TDelphiParallelForProcedure64);
var
  Total, Depth, Completed, StepTotal, stepW, Pass, w: Int64;
  p: PDelphiParallelThData64;
  i_: Int64;
  Critical: TCritical;
begin
  if b > e then
      Swap(b, e);
  if ParallelOverflow.Busy() then
    begin
      for i_ := b to e do
          OnFor(i_);
      exit;
    end;
  ParallelOverflow.Acquire;
  try
    Depth := ParallelGranularity;
    Total := e - b + 1;
    Critical := TCritical.Create;

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
            p^.Critical := Critical;
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
            p^.Critical := Critical;
            TComputeThread.RunC(p, nil, DelphiParallelTh64);
          end;
      end;

    repeat
      TThread.Sleep(1);
      Critical.Acquire;
      w := Completed;
      Critical.Release;
    until w >= Total;

    Critical.Free;
  finally
      ParallelOverflow.Release;
  end;
end;
{$ENDIF SystemParallel}
