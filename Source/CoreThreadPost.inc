procedure TThreadPostData.Init;
begin
  OnCall1 := nil;
  OnCall2 := nil;
  OnCall3 := nil;
  OnCall4 := nil;
  OnMethod1 := nil;
  OnMethod2 := nil;
  OnMethod3 := nil;
  OnMethod4 := nil;
  OnProc1 := nil;
  OnProc2 := nil;
  OnProc3 := nil;
  OnProc4 := nil;
  Data1 := nil;
  Data2 := nil;
  Data3 := NULL;
end;

procedure TThreadPost.FreeThreadProgressPostData(p: TThreadPostDataOrder.PT_);
begin
  Dispose(p);
end;

constructor TThreadPost.Create(ThreadID_: TThreadID);
begin
  inherited Create;
  FCritical := TCritical.Create;
  FThreadID := ThreadID_;
  FSyncPool := TThreadPostDataOrder.Create;
  FSyncPool.OnFreeOrderStruct := {$IFDEF FPC}@{$ENDIF FPC}FreeThreadProgressPostData;
  FProgressing := TAtomBool.Create(False);
  FOneStep := True;
  FResetRandomSeed := False;
end;

destructor TThreadPost.Destroy;
begin
  FCritical.Acquire;
  FSyncPool.Clear;
  FSyncPool.Clear;
  FCritical.Release;
  DisposeObject(FSyncPool);
  FCritical.Free;
  FProgressing.Free;
  inherited Destroy;
end;

function TThreadPost.Count: Integer;
begin
  FCritical.Acquire;
  Result := FSyncPool.Num;
  FCritical.Release;
end;

function TThreadPost.Busy: Boolean;
begin
  Result := (Count > 0) or (FProgressing.V);
end;

function TThreadPost.Progress(ThreadID_: TThreadID): Integer;
var
  i: Integer;
  temp: TThreadPostDataOrder;
  t_: TThreadPostData;
begin
  Result := 0;
  if ThreadID_ <> FThreadID then
      exit;

  if FOneStep then
    begin
      if FSyncPool.Current <> nil then
        begin
          FProgressing.V := True;
          FCritical.Acquire;
          t_ := FSyncPool.Current^.Data^;
          FSyncPool.Next;
          FCritical.Release;

          if FResetRandomSeed then
              SetMT19937Seed(0);
          try
            if Assigned(t_.OnCall1) then
                t_.OnCall1();
            if Assigned(t_.OnCall2) then
                t_.OnCall2(t_.Data1);
            if Assigned(t_.OnCall3) then
                t_.OnCall3(t_.Data1, t_.Data2, t_.Data3);
            if Assigned(t_.OnCall4) then
                t_.OnCall4(t_.Data1, t_.Data2);

            if Assigned(t_.OnMethod1) then
                t_.OnMethod1();
            if Assigned(t_.OnMethod2) then
                t_.OnMethod2(t_.Data1);
            if Assigned(t_.OnMethod3) then
                t_.OnMethod3(t_.Data1, t_.Data2, t_.Data3);
            if Assigned(t_.OnMethod4) then
                t_.OnMethod4(t_.Data1, t_.Data2);

            if Assigned(t_.OnProc1) then
                t_.OnProc1();
            if Assigned(t_.OnProc2) then
                t_.OnProc2(t_.Data1);
            if Assigned(t_.OnProc3) then
                t_.OnProc3(t_.Data1, t_.Data2, t_.Data3);
            if Assigned(t_.OnProc4) then
                t_.OnProc4(t_.Data1, t_.Data2);
          except
          end;

          FProgressing.V := False;
          Result := 1;
        end;
    end
  else
    while (not FProgressing.V) and (Count > 0) do
      begin
        FProgressing.V := True;
        FCritical.Acquire;
        temp := FSyncPool;
        FSyncPool := TThreadPostDataOrder.Create;
        FCritical.Release;
        Result := temp.Num;
        while temp.Current <> nil do
          begin
            if FResetRandomSeed then
                SetMT19937Seed(0);
            try
              if Assigned(temp.Current^.Data^.OnCall1) then
                  temp.Current^.Data^.OnCall1();
              if Assigned(temp.Current^.Data^.OnCall2) then
                  temp.Current^.Data^.OnCall2(temp.Current^.Data^.Data1);
              if Assigned(temp.Current^.Data^.OnCall3) then
                  temp.Current^.Data^.OnCall3(temp.Current^.Data^.Data1, temp.Current^.Data^.Data2, temp.Current^.Data^.Data3);
              if Assigned(temp.Current^.Data^.OnCall4) then
                  temp.Current^.Data^.OnCall4(temp.Current^.Data^.Data1, temp.Current^.Data^.Data2);

              if Assigned(temp.Current^.Data^.OnMethod1) then
                  temp.Current^.Data^.OnMethod1();
              if Assigned(temp.Current^.Data^.OnMethod2) then
                  temp.Current^.Data^.OnMethod2(temp.Current^.Data^.Data1);
              if Assigned(temp.Current^.Data^.OnMethod3) then
                  temp.Current^.Data^.OnMethod3(temp.Current^.Data^.Data1, temp.Current^.Data^.Data2, temp.Current^.Data^.Data3);
              if Assigned(temp.Current^.Data^.OnMethod4) then
                  temp.Current^.Data^.OnMethod4(temp.Current^.Data^.Data1, temp.Current^.Data^.Data2);

              if Assigned(temp.Current^.Data^.OnProc1) then
                  temp.Current^.Data^.OnProc1();
              if Assigned(temp.Current^.Data^.OnProc2) then
                  temp.Current^.Data^.OnProc2(temp.Current^.Data^.Data1);
              if Assigned(temp.Current^.Data^.OnProc3) then
                  temp.Current^.Data^.OnProc3(temp.Current^.Data^.Data1, temp.Current^.Data^.Data2, temp.Current^.Data^.Data3);
              if Assigned(temp.Current^.Data^.OnProc4) then
                  temp.Current^.Data^.OnProc4(temp.Current^.Data^.Data1, temp.Current^.Data^.Data2);
            except
            end;
            temp.Next;
          end;
        DisposeObject(temp);
        FProgressing.V := False;
      end;
end;

function TThreadPost.Progress(Thread_: TThread): Integer;
begin
  Result := Progress(Thread_.ThreadID);
end;

function TThreadPost.Progress(): Integer;
begin
  Result := Progress(TThread.CurrentThread);
end;

procedure TThreadPost.PostC1(OnSync: TThreadPostCall1);
var
  t_: TThreadPostData;
begin
  t_.Init();
  t_.OnCall1 := OnSync;
  FCritical.Acquire;
  FSyncPool.Push(t_);
  FCritical.Release;
end;

procedure TThreadPost.PostC2(Data1: Pointer; OnSync: TThreadPostCall2);
var
  t_: TThreadPostData;
begin
  t_.Init();
  t_.Data1 := Data1;
  t_.OnCall2 := OnSync;
  FCritical.Acquire;
  FSyncPool.Push(t_);
  FCritical.Release;
end;

procedure TThreadPost.PostC3(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant; OnSync: TThreadPostCall3);
var
  t_: TThreadPostData;
begin
  t_.Init();
  t_.Data1 := Data1;
  t_.Data2 := Data2;
  t_.Data3 := Data3;
  t_.OnCall3 := OnSync;
  FCritical.Acquire;
  FSyncPool.Push(t_);
  FCritical.Release;
end;

procedure TThreadPost.PostC4(Data1: Pointer; Data2: TCoreClassObject; OnSync: TThreadPostCall4);
var
  t_: TThreadPostData;
begin
  t_.Init();
  t_.Data1 := Data1;
  t_.Data2 := Data2;
  t_.OnCall4 := OnSync;
  FCritical.Acquire;
  FSyncPool.Push(t_);
  FCritical.Release;
end;

procedure TThreadPost.PostM1(OnSync: TThreadPostMethod1);
var
  t_: TThreadPostData;
begin
  t_.Init();
  t_.OnMethod1 := OnSync;
  FCritical.Acquire;
  FSyncPool.Push(t_);
  FCritical.Release;
end;

procedure TThreadPost.PostM2(Data1: Pointer; OnSync: TThreadPostMethod2);
var
  t_: TThreadPostData;
begin
  t_.Init();
  t_.Data1 := Data1;
  t_.OnMethod2 := OnSync;
  FCritical.Acquire;
  FSyncPool.Push(t_);
  FCritical.Release;
end;

procedure TThreadPost.PostM3(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant; OnSync: TThreadPostMethod3);
var
  t_: TThreadPostData;
begin
  t_.Init();
  t_.Data1 := Data1;
  t_.Data2 := Data2;
  t_.Data3 := Data3;
  t_.OnMethod3 := OnSync;
  FCritical.Acquire;
  FSyncPool.Push(t_);
  FCritical.Release;
end;

procedure TThreadPost.PostM4(Data1: Pointer; Data2: TCoreClassObject; OnSync: TThreadPostMethod4);
var
  t_: TThreadPostData;
begin
  t_.Init();
  t_.Data1 := Data1;
  t_.Data2 := Data2;
  t_.OnMethod4 := OnSync;
  FCritical.Acquire;
  FSyncPool.Push(t_);
  FCritical.Release;
end;

procedure TThreadPost.PostP1(OnSync: TThreadPostProc1);
var
  t_: TThreadPostData;
begin
  t_.Init();
  t_.OnProc1 := OnSync;
  FCritical.Acquire;
  FSyncPool.Push(t_);
  FCritical.Release;
end;

procedure TThreadPost.PostP2(Data1: Pointer; OnSync: TThreadPostProc2);
var
  t_: TThreadPostData;
begin
  t_.Init();
  t_.Data1 := Data1;
  t_.OnProc2 := OnSync;
  FCritical.Acquire;
  FSyncPool.Push(t_);
  FCritical.Release;
end;

procedure TThreadPost.PostP3(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant; OnSync: TThreadPostProc3);
var
  t_: TThreadPostData;
begin
  t_.Init();
  t_.Data1 := Data1;
  t_.Data2 := Data2;
  t_.Data3 := Data3;
  t_.OnProc3 := OnSync;
  FCritical.Acquire;
  FSyncPool.Push(t_);
  FCritical.Release;
end;

procedure TThreadPost.PostP4(Data1: Pointer; Data2: TCoreClassObject; OnSync: TThreadPostProc4);
var
  t_: TThreadPostData;
begin
  t_.Init();
  t_.Data1 := Data1;
  t_.Data2 := Data2;
  t_.OnProc4 := OnSync;
  FCritical.Acquire;
  FSyncPool.Push(t_);
  FCritical.Release;
end;
