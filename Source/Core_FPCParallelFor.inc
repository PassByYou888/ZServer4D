type
  TFPCParallelThData32_Block = record
    b, e: Integer;
    Completed: ^Integer;
    OnFor: TFPCParallelForProcedure32;
    Critical: TCritical;
  end;

  PFPCParallelThData32_Block = ^TFPCParallelThData32_Block;

procedure FPCParallelTh32_Block(ThSender: TCompute);
var
  p: PFPCParallelThData32_Block;
  Pass: Integer;
begin
  p := ThSender.UserData;
  Pass := p^.b;
  while Pass <= p^.e do
    begin
      try
          p^.OnFor(Pass);
      except
      end;
      inc(Pass);
    end;

  p^.Critical.Acquire;
  AtomInc(p^.Completed^, p^.e - p^.b + 1);
  p^.Critical.Release;
  dispose(p);
end;

procedure FPCParallelFor_Block(parallel: Boolean; b, e: Integer; OnFor: TFPCParallelForProcedure32);
var
  Total, Depth, Completed, StepTotal, stepW, Pass, w: Integer;
  p: PFPCParallelThData32_Block;
  i: Integer;
  Critical: TCritical;
begin
  if b > e then
      exit;
  if (not parallel) or (not WorkInParallelCore.V) or ParallelOverflow.Busy() then
    begin
      i := b;
      while i <= e do
        begin
          try
              OnFor(i);
          except
          end;
          inc(i);
        end;
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
        Pass := b;
        while Pass <= e do
          begin
            new(p);
            p^.b := Pass;
            p^.e := Pass;
            p^.Completed := @Completed;
            p^.OnFor := OnFor;
            p^.Critical := Critical;
            TCompute.RunC(p, nil, @FPCParallelTh32_Block);
            inc(Pass);
          end;
      end
    else
      begin
        stepW := Total div Depth;
        StepTotal := Total div stepW;
        if Total mod stepW > 0 then
            inc(StepTotal);

        Pass := 0;
        while Pass < StepTotal do
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
            TCompute.RunC(p, nil, @FPCParallelTh32_Block);
            inc(Pass);
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
  TFPCParallelThData64_Block = record
    b, e: Int64;
    Completed: ^Int64;
    OnFor: TFPCParallelForProcedure64;
    Critical: TCritical;
  end;

  PFPCParallelThData64_Block = ^TFPCParallelThData64_Block;

procedure FPCParallelTh64_Block(ThSender: TCompute);
var
  p: PFPCParallelThData64_Block;
  Pass: Int64;
begin
  p := ThSender.UserData;
  Pass := p^.b;
  while Pass <= p^.e do
    begin
      try
          p^.OnFor(Pass);
      except
      end;
      inc(Pass);
    end;

  p^.Critical.Acquire;
  AtomInc(p^.Completed^, p^.e - p^.b + 1);
  p^.Critical.Release;
  dispose(p);
end;

procedure FPCParallelFor_Block(parallel: Boolean; b, e: Int64; OnFor: TFPCParallelForProcedure64);
var
  Total, Depth, Completed, StepTotal, stepW, Pass, w: Int64;
  p: PFPCParallelThData64_Block;
  i: Int64;
  Critical: TCritical;
begin
  if b > e then
      exit;
  if (not parallel) or (not WorkInParallelCore.V) or ParallelOverflow.Busy() then
    begin
      i := b;
      while i <= e do
        begin
          try
              OnFor(i);
          except
          end;
          inc(i);
        end;
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
        Pass := b;
        while Pass <= e do
          begin
            new(p);
            p^.b := Pass;
            p^.e := Pass;
            p^.Completed := @Completed;
            p^.OnFor := OnFor;
            p^.Critical := Critical;
            TCompute.RunC(p, nil, @FPCParallelTh64_Block);
            inc(Pass);
          end;
      end
    else
      begin
        stepW := Total div Depth;
        StepTotal := Total div stepW;
        if Total mod stepW > 0 then
            inc(StepTotal);

        Pass := 0;
        while Pass < StepTotal do
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
            TCompute.RunC(p, nil, @FPCParallelTh64_Block);
            inc(Pass);
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
  TFPCParallelThData32_Fold = record
    Pass: Int64;
    Total, Granularity: Integer;
    Completed: Boolean;
    OnFor: TFPCParallelForProcedure32;
    Critical: TCritical;
  end;

  PFPCParallelThData32_Fold = ^TFPCParallelThData32_Fold;

procedure FPCParallelTh32_Fold(ThSender: TCompute);
var
  p: PFPCParallelThData32_Fold;
begin
  p := ThSender.UserData;
  with p^ do
    while Pass <= Total do
      begin
        try
            OnFor(Pass);
        except
        end;
        inc(Pass, Granularity);
      end;

  p^.Critical.Acquire;
  p^.Completed := True;
  p^.Critical.Release;
end;

procedure FPCParallelFor_Fold(parallel: Boolean; b, e: Integer; OnFor: TFPCParallelForProcedure32);
var
  p: PFPCParallelThData32_Fold;
  i, Depth: Integer;
  Critical: TCritical;
  states: array of TFPCParallelThData32_Fold;
  Completed: Boolean;
begin
  if b > e then
      exit;
  if (not parallel) or (not WorkInParallelCore.V) or ParallelOverflow.Busy() then
    begin
      i := b;
      while i <= e do
        begin
          try
              OnFor(i);
          except
          end;
          inc(i);
        end;
      exit;
    end;
  ParallelOverflow.Acquire;
  try
    Critical := TCritical.Create;
    Depth := Min(ParallelGranularity, e - b + 1);
    SetLength(states, Depth);

    i := 0;
    while i < Depth do
      begin
        p := @states[i];
        p^.Pass := b + i;
        p^.Total := e;
        p^.Granularity := ParallelGranularity;
        p^.Completed := False;
        p^.OnFor := OnFor;
        p^.Critical := Critical;
        TCompute.RunC(p, nil, @FPCParallelTh32_Fold);
        inc(i);
      end;

    repeat
      TThread.Sleep(1);
      Critical.Acquire;
      Completed := True;
      i := 0;
      while i < Length(states) do
        begin
          Completed := Completed and states[i].Completed;
          inc(i);
        end;
      Critical.Release;
    until Completed;

    Critical.Free;
  finally
      ParallelOverflow.Release;
  end;
end;

type
  TFPCParallelThData64_Fold = record
    Pass: Int64;
    Total, Granularity: Int64;
    Completed: Boolean;
    OnFor: TFPCParallelForProcedure64;
    Critical: TCritical;
  end;

  PFPCParallelThData64_Fold = ^TFPCParallelThData64_Fold;

procedure FPCParallelTh64_Fold(ThSender: TCompute);
var
  p: PFPCParallelThData64_Fold;
begin
  p := ThSender.UserData;
  with p^ do
    while Pass <= Total do
      begin
        try
            OnFor(Pass);
        except
        end;
        inc(Pass, Granularity);
      end;

  p^.Critical.Acquire;
  p^.Completed := True;
  p^.Critical.Release;
end;

procedure FPCParallelFor_Fold(parallel: Boolean; b, e: Int64; OnFor: TFPCParallelForProcedure64);
var
  p: PFPCParallelThData64_Fold;
  i, Depth: Int64;
  Critical: TCritical;
  states: array of TFPCParallelThData64_Fold;
  Completed: Boolean;
begin
  if b > e then
      exit;
  if (not parallel) or (not WorkInParallelCore.V) or ParallelOverflow.Busy() then
    begin
      i := b;
      while i <= e do
        begin
          try
              OnFor(i);
          except
          end;
          inc(i);
        end;
      exit;
    end;
  ParallelOverflow.Acquire;
  try
    Critical := TCritical.Create;
    Depth := Min(ParallelGranularity, e - b + 1);
    SetLength(states, Depth);
    i := 0;
    while i < Depth do
      begin
        p := @states[i];
        p^.Pass := b + i;
        p^.Total := e;
        p^.Granularity := ParallelGranularity;
        p^.Completed := False;
        p^.OnFor := OnFor;
        p^.Critical := Critical;
        TCompute.RunC(p, nil, @FPCParallelTh64_Fold);
        inc(i);
      end;

    repeat
      TThread.Sleep(1);
      Critical.Acquire;
      Completed := True;
      i := 0;
      while i < Length(states) do
        begin
          Completed := Completed and states[i].Completed;
          inc(i);
        end;
      Critical.Release;
    until Completed;

    Critical.Free;
  finally
      ParallelOverflow.Release;
  end;
end;

procedure FPCParallelFor(parallel: Boolean; b, e: Integer; OnFor: TFPCParallelForProcedure32);
begin
{$IFDEF FoldParallel}
  FPCParallelFor_Fold(parallel, b, e, OnFor);
{$ELSE FoldParallel}
  FPCParallelFor_Block(parallel, b, e, OnFor);
{$ENDIF FoldParallel}
end;

procedure FPCParallelFor(parallel: Boolean; b, e: Int64; OnFor: TFPCParallelForProcedure64);
begin
{$IFDEF FoldParallel}
  FPCParallelFor_Fold(parallel, b, e, OnFor);
{$ELSE FoldParallel}
  FPCParallelFor_Block(parallel, b, e, OnFor);
{$ENDIF FoldParallel}
end;

procedure FPCParallelFor(b, e: Integer; OnFor: TFPCParallelForProcedure32);
begin
  FPCParallelFor(True, b, e, OnFor);
end;

procedure FPCParallelFor(b, e: Int64; OnFor: TFPCParallelForProcedure64);
begin
  FPCParallelFor(True, b, e, OnFor);
end;

procedure FPCParallelFor(OnFor: TFPCParallelForProcedure32; b, e: Integer);
begin
  FPCParallelFor(b, e, OnFor);
end;

procedure FPCParallelFor(OnFor: TFPCParallelForProcedure64; b, e: Int64);
begin
  FPCParallelFor(b, e, OnFor);
end;

procedure FPCParallelFor(parallel: Boolean; OnFor: TFPCParallelForProcedure32; b, e: Integer);
begin
  FPCParallelFor(parallel, b, e, OnFor);
end;

procedure FPCParallelFor(parallel: Boolean; OnFor: TFPCParallelForProcedure64; b, e: Int64);
begin
  FPCParallelFor(parallel, b, e, OnFor);
end;

procedure ParallelFor(parallel: Boolean; b, e: Integer; OnFor: TFPCParallelForProcedure32);
begin
  FPCParallelFor(parallel, b, e, OnFor);
end;

procedure ParallelFor(parallel: Boolean; b, e: Int64; OnFor: TFPCParallelForProcedure64);
begin
  FPCParallelFor(parallel, b, e, OnFor);
end;

procedure ParallelFor(b, e: Integer; OnFor: TFPCParallelForProcedure32);
begin
  FPCParallelFor(True, b, e, OnFor);
end;

procedure ParallelFor(b, e: Int64; OnFor: TFPCParallelForProcedure64);
begin
  FPCParallelFor(True, b, e, OnFor);
end;

procedure ParallelFor(OnFor: TFPCParallelForProcedure32; b, e: Integer);
begin
  FPCParallelFor(b, e, OnFor);
end;

procedure ParallelFor(OnFor: TFPCParallelForProcedure64; b, e: Int64);
begin
  FPCParallelFor(b, e, OnFor);
end;

procedure ParallelFor(parallel: Boolean; OnFor: TFPCParallelForProcedure32; b, e: Integer);
begin
  FPCParallelFor(parallel, b, e, OnFor);
end;

procedure ParallelFor(parallel: Boolean; OnFor: TFPCParallelForProcedure64; b, e: Int64);
begin
  FPCParallelFor(parallel, b, e, OnFor);
end;
