type
  TFPCParallelThData = record
    b, e: NativeInt;
    Completed: ^NativeInt;
    OnFor: TFPCParallelForProcedure;
  end;

  PFPCParallelThData = ^TFPCParallelThData;

procedure FPCParallelTh(ThSender: TComputeThread);
var
  p: PFPCParallelThData;
  pass: NativeInt;
begin
  p := ThSender.UserData;
  for pass := p^.b to p^.e do
      p^.OnFor(pass);
  AtomInc(p^.Completed^, p^.e - p^.b + 1);
  dispose(p);
end;

procedure FPCParallelFor(OnFor: TFPCParallelForProcedure; b, e: NativeInt);
var
  Total, Depth, Completed, StepTotal, stepW, pass, w: NativeInt;
  p: PFPCParallelThData;
begin
  if b > e then
      Swap(b, e);

  Depth := ParallelGranularity;
  Total := e - b + 1;

  Completed := 0;

  if (Total < Depth) then
    begin
      for pass := b to e do
        begin
          new(p);
          p^.b := pass;
          p^.e := pass;
          p^.Completed := @Completed;
          p^.OnFor := OnFor;
          TComputeThread.RunC(p, nil, @FPCParallelTh);
        end;
    end
  else
    begin
      stepW := Total div Depth;
      StepTotal := Total div stepW;
      if Total mod stepW > 0 then
          inc(StepTotal);

      for pass := 0 to StepTotal - 1 do
        begin
          w := stepW * pass;
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
          TComputeThread.RunC(p, nil, @FPCParallelTh);
        end;
    end;

  while Completed < Total do
      TThread.Sleep(1);
end;

