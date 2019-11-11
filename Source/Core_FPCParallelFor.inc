type
  TFPCParallelThData32 = record
    b, e: Integer;
    Completed: ^Integer;
    OnFor: TFPCParallelForProcedureInt32;
  end;

  PFPCParallelThData32 = ^TFPCParallelThData32;

procedure FPCParallelTh32(ThSender: TComputeThread);
var
  p: PFPCParallelThData32;
  pass: Integer;
begin
  p := ThSender.UserData;
  for pass := p^.b to p^.e do
      p^.OnFor(pass);
  AtomInc(p^.Completed^, p^.e - p^.b + 1);
  dispose(p);
end;

procedure FPCParallelFor(OnFor: TFPCParallelForProcedureInt32; b, e: Integer);
var
  Total, Depth, Completed, StepTotal, stepW, pass, w: Integer;
  p: PFPCParallelThData32;
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
          TComputeThread.RunC(p, nil, @FPCParallelTh32);
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
          TComputeThread.RunC(p, nil, @FPCParallelTh32);
        end;
    end;

  while Completed < Total do
      TThread.Sleep(1);
end;

type
  TFPCParallelThData64 = record
    b, e: Int64;
    Completed: ^Int64;
    OnFor: TFPCParallelForProcedureInt64;
  end;

  PFPCParallelThData64 = ^TFPCParallelThData64;

procedure FPCParallelTh64(ThSender: TComputeThread);
var
  p: PFPCParallelThData64;
  pass: Int64;
begin
  p := ThSender.UserData;
  for pass := p^.b to p^.e do
      p^.OnFor(pass);
  AtomInc(p^.Completed^, p^.e - p^.b + 1);
  dispose(p);
end;

procedure FPCParallelFor(OnFor: TFPCParallelForProcedureInt64; b, e: Int64);
var
  Total, Depth, Completed, StepTotal, stepW, pass, w: Int64;
  p: PFPCParallelThData64;
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
          TComputeThread.RunC(p, nil, @FPCParallelTh64);
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
          TComputeThread.RunC(p, nil, @FPCParallelTh64);
        end;
    end;

  while Completed < Total do
      TThread.Sleep(1);
end;
