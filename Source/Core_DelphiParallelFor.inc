{$IFDEF SystemParallel}


procedure DelphiParallelFor(parallel: Boolean; b, e: Integer; OnFor: TDelphiParallelForProcedure32);
var
  i: Integer;
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
      TParallel.&For(b, e, OnFor);
  finally
      ParallelOverflow.Release;
  end;
end;

procedure DelphiParallelFor(parallel: Boolean; b, e: Int64; OnFor: TDelphiParallelForProcedure64);
var
  i: Int64;
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
      TParallel.&For(b, e, OnFor);
  finally
      ParallelOverflow.Release;
  end;
end;
{$ELSE SystemParallel}


type
  TDelphiParallelThData32_Block = record
    b, e: Integer;
    Completed: ^Integer;
    OnFor: TDelphiParallelForProcedure32;
    Critical: TCritical;
  end;

  PDelphiParallelThData32_Block = ^TDelphiParallelThData32_Block;

procedure DelphiParallelTh32_Block(ThSender: TCompute);
var
  p: PDelphiParallelThData32_Block;
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

procedure DelphiParallelFor_Block(parallel: Boolean; b, e: Integer; OnFor: TDelphiParallelForProcedure32);
var
  Total, Depth, Completed, StepTotal, stepW, Pass, w: Integer;
  p: PDelphiParallelThData32_Block;
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
            TCompute.RunC(p, nil, DelphiParallelTh32_Block);
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
            TCompute.RunC(p, nil, DelphiParallelTh32_Block);
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
  TDelphiParallelThData64_Block = record
    b, e: Int64;
    Completed: ^Int64;
    OnFor: TDelphiParallelForProcedure64;
    Critical: TCritical;
  end;

  PDelphiParallelThData64_Block = ^TDelphiParallelThData64_Block;

procedure DelphiParallelTh64_Block(ThSender: TCompute);
var
  p: PDelphiParallelThData64_Block;
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

procedure DelphiParallelFor_Block(parallel: Boolean; b, e: Int64; OnFor: TDelphiParallelForProcedure64);
var
  Total, Depth, Completed, StepTotal, stepW, Pass, w: Int64;
  p: PDelphiParallelThData64_Block;
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
            TCompute.RunC(p, nil, DelphiParallelTh64_Block);
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
            TCompute.RunC(p, nil, DelphiParallelTh64_Block);
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
  TDelphiParallelThData32_Fold = record
    Pass: Int64;
    Total, Granularity: Integer;
    Completed: Boolean;
    OnFor: TDelphiParallelForProcedure32;
    Critical: TCritical;
  end;

  PDelphiParallelThData32_Fold = ^TDelphiParallelThData32_Fold;

procedure DelphiParallelTh32_Fold(ThSender: TCompute);
var
  p: PDelphiParallelThData32_Fold;
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

procedure DelphiParallelFor_Fold(parallel: Boolean; b, e: Integer; OnFor: TDelphiParallelForProcedure32);
var
  p: PDelphiParallelThData32_Fold;
  i, Depth: Integer;
  Critical: TCritical;
  states: array of TDelphiParallelThData32_Fold;
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
        TCompute.RunC(p, nil, DelphiParallelTh32_Fold);
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
  TDelphiParallelThData64_Fold = record
    Pass: Int64;
    Total, Granularity: Int64;
    Completed: Boolean;
    OnFor: TDelphiParallelForProcedure64;
    Critical: TCritical;
  end;

  PDelphiParallelThData64_Fold = ^TDelphiParallelThData64_Fold;

procedure DelphiParallelTh64_Fold(ThSender: TCompute);
var
  p: PDelphiParallelThData64_Fold;
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

procedure DelphiParallelFor_Fold(parallel: Boolean; b, e: Int64; OnFor: TDelphiParallelForProcedure64);
var
  p: PDelphiParallelThData64_Fold;
  i, Depth: Int64;
  Critical: TCritical;
  states: array of TDelphiParallelThData64_Fold;
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
        TCompute.RunC(p, nil, DelphiParallelTh64_Fold);
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

procedure DelphiParallelFor(parallel: Boolean; b, e: Integer; OnFor: TDelphiParallelForProcedure32);
begin
{$IFDEF FoldParallel}
  DelphiParallelFor_Fold(parallel, b, e, OnFor);
{$ELSE FoldParallel}
  DelphiParallelFor_Block(parallel, b, e, OnFor);
{$ENDIF FoldParallel}
end;

procedure DelphiParallelFor(parallel: Boolean; b, e: Int64; OnFor: TDelphiParallelForProcedure64);
begin
{$IFDEF FoldParallel}
  DelphiParallelFor_Fold(parallel, b, e, OnFor);
{$ELSE FoldParallel}
  DelphiParallelFor_Block(parallel, b, e, OnFor);
{$ENDIF FoldParallel}
end;

{$ENDIF SystemParallel}


procedure DelphiParallelFor(b, e: Integer; OnFor: TDelphiParallelForProcedure32);
begin
  DelphiParallelFor(True, b, e, OnFor);
end;

procedure DelphiParallelFor(b, e: Int64; OnFor: TDelphiParallelForProcedure64);
begin
  DelphiParallelFor(True, b, e, OnFor);
end;

procedure DelphiParallelFor(OnFor: TDelphiParallelForProcedure32; b, e: Integer);
begin
  DelphiParallelFor(b, e, OnFor);
end;

procedure DelphiParallelFor(OnFor: TDelphiParallelForProcedure64; b, e: Int64);
begin
  DelphiParallelFor(b, e, OnFor);
end;

procedure DelphiParallelFor(parallel: Boolean; OnFor: TDelphiParallelForProcedure32; b, e: Integer);
begin
  DelphiParallelFor(parallel, b, e, OnFor);
end;

procedure DelphiParallelFor(parallel: Boolean; OnFor: TDelphiParallelForProcedure64; b, e: Int64);
begin
  DelphiParallelFor(parallel, b, e, OnFor);
end;

procedure ParallelFor(parallel: Boolean; b, e: Integer; OnFor: TDelphiParallelForProcedure32);
begin
  DelphiParallelFor(parallel, b, e, OnFor);
end;

procedure ParallelFor(parallel: Boolean; b, e: Int64; OnFor: TDelphiParallelForProcedure64);
begin
  DelphiParallelFor(parallel, b, e, OnFor);
end;

procedure ParallelFor(b, e: Integer; OnFor: TDelphiParallelForProcedure32);
begin
  DelphiParallelFor(True, b, e, OnFor);
end;

procedure ParallelFor(b, e: Int64; OnFor: TDelphiParallelForProcedure64);
begin
  DelphiParallelFor(True, b, e, OnFor);
end;

procedure ParallelFor(OnFor: TDelphiParallelForProcedure32; b, e: Integer);
begin
  DelphiParallelFor(b, e, OnFor);
end;

procedure ParallelFor(OnFor: TDelphiParallelForProcedure64; b, e: Int64);
begin
  DelphiParallelFor(b, e, OnFor);
end;

procedure ParallelFor(parallel: Boolean; OnFor: TDelphiParallelForProcedure32; b, e: Integer);
begin
  DelphiParallelFor(parallel, b, e, OnFor);
end;

procedure ParallelFor(parallel: Boolean; OnFor: TDelphiParallelForProcedure64; b, e: Int64);
begin
  DelphiParallelFor(parallel, b, e, OnFor);
end;
