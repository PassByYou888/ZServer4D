procedure TThreadProgressPostData.Init;
begin
  OnCall1 := nil;
  OnCall2 := nil;
  OnCall3 := nil;
  OnMethod1 := nil;
  OnMethod2 := nil;
  OnMethod3 := nil;
  OnProc1 := nil;
  OnProc2 := nil;
  OnProc3 := nil;
  Data1 := nil;
  Data2 := nil;
  Data3 := NULL;
end;

constructor TThreadProgressPost.Create(ThreadID_: TThreadID);
begin
  inherited Create;
  FCritical := TCritical.Create;
  FThreadID := ThreadID_;
  FSyncPool := TThreadProgressPostDataList.Create;
  FProgressing := TAtomBool.Create(False);
  FOneStep := True;
end;

destructor TThreadProgressPost.Destroy;
var
  i: Integer;
begin
  FCritical.Acquire;
  for i := 0 to FSyncPool.Count - 1 do
      Dispose(PThreadProgressPostData(FSyncPool[i]));
  FSyncPool.Clear;
  FCritical.Release;
  DisposeObject(FSyncPool);
  FCritical.Free;
  FProgressing.Free;
  inherited Destroy;
end;

function TThreadProgressPost.Count: Integer;
begin
  FCritical.Acquire;
  Result := FSyncPool.Count;
  FCritical.Release;
end;

function TThreadProgressPost.Busy: Boolean;
begin
  Result := (Count > 0) or (FProgressing.V);
end;

function TThreadProgressPost.Progress(ThreadID_: TThreadID): Integer;
var
  i: Integer;
  temp: TThreadProgressPostDataList;
  p: PThreadProgressPostData;
begin
  Result := 0;
  if ThreadID_ <> FThreadID then
      exit;

  if FOneStep then
    begin
      if Count > 0 then
        begin
          FProgressing.V := True;
          FCritical.Acquire;
          p := FSyncPool[0];
          FSyncPool.Delete(0);
          FCritical.Release;
          try
            if Assigned(p^.OnCall1) then
                p^.OnCall1();
            if Assigned(p^.OnCall2) then
                p^.OnCall2(p^.Data1);
            if Assigned(p^.OnCall3) then
                p^.OnCall3(p^.Data1, p^.Data2, p^.Data3);

            if Assigned(p^.OnMethod1) then
                p^.OnMethod1();
            if Assigned(p^.OnMethod2) then
                p^.OnMethod2(p^.Data1);
            if Assigned(p^.OnMethod3) then
                p^.OnMethod3(p^.Data1, p^.Data2, p^.Data3);

            if Assigned(p^.OnProc1) then
                p^.OnProc1();
            if Assigned(p^.OnProc2) then
                p^.OnProc2(p^.Data1);
            if Assigned(p^.OnProc3) then
                p^.OnProc3(p^.Data1, p^.Data2, p^.Data3);
          except
          end;
          Dispose(p);
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
        FSyncPool := TThreadProgressPostDataList.Create;
        FCritical.Release;
        Result := temp.Count;
        for i := 0 to temp.Count - 1 do
          begin
            p := temp[i];
            try
              if Assigned(p^.OnCall1) then
                  p^.OnCall1();
              if Assigned(p^.OnCall2) then
                  p^.OnCall2(p^.Data1);
              if Assigned(p^.OnCall3) then
                  p^.OnCall3(p^.Data1, p^.Data2, p^.Data3);

              if Assigned(p^.OnMethod1) then
                  p^.OnMethod1();
              if Assigned(p^.OnMethod2) then
                  p^.OnMethod2(p^.Data1);
              if Assigned(p^.OnMethod3) then
                  p^.OnMethod3(p^.Data1, p^.Data2, p^.Data3);

              if Assigned(p^.OnProc1) then
                  p^.OnProc1();
              if Assigned(p^.OnProc2) then
                  p^.OnProc2(p^.Data1);
              if Assigned(p^.OnProc3) then
                  p^.OnProc3(p^.Data1, p^.Data2, p^.Data3);
            except
            end;
            Dispose(p);
          end;
        DisposeObject(temp);
        FProgressing.V := False;
      end;
end;

function TThreadProgressPost.Progress(Thread_: TThread): Integer;
begin
  Result := Progress(Thread_.ThreadID);
end;

function TThreadProgressPost.Progress(): Integer;
begin
  Result := Progress(TThread.CurrentThread);
end;

procedure TThreadProgressPost.PostC1(OnSync: TThreadProgressPostCall1);
var
  p: PThreadProgressPostData;
begin
  new(p);
  p^.Init();
  p^.OnCall1 := OnSync;
  FCritical.Acquire;
  FSyncPool.Add(p);
  FCritical.Release;
end;

procedure TThreadProgressPost.PostC2(Data1: Pointer; OnSync: TThreadProgressPostCall2);
var
  p: PThreadProgressPostData;
begin
  new(p);
  p^.Init();
  p^.Data1 := Data1;
  p^.OnCall2 := OnSync;
  FCritical.Acquire;
  FSyncPool.Add(p);
  FCritical.Release;
end;

procedure TThreadProgressPost.PostC3(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant; OnSync: TThreadProgressPostCall3);
var
  p: PThreadProgressPostData;
begin
  new(p);
  p^.Init();
  p^.Data1 := Data1;
  p^.Data2 := Data2;
  p^.Data3 := Data3;
  p^.OnCall3 := OnSync;
  FCritical.Acquire;
  FSyncPool.Add(p);
  FCritical.Release;
end;

procedure TThreadProgressPost.PostM1(OnSync: TThreadProgressPostMethod1);
var
  p: PThreadProgressPostData;
begin
  new(p);
  p^.Init();
  p^.OnMethod1 := OnSync;
  FCritical.Acquire;
  FSyncPool.Add(p);
  FCritical.Release;
end;

procedure TThreadProgressPost.PostM2(Data1: Pointer; OnSync: TThreadProgressPostMethod2);
var
  p: PThreadProgressPostData;
begin
  new(p);
  p^.Init();
  p^.Data1 := Data1;
  p^.OnMethod2 := OnSync;
  FCritical.Acquire;
  FSyncPool.Add(p);
  FCritical.Release;
end;

procedure TThreadProgressPost.PostM3(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant; OnSync: TThreadProgressPostMethod3);
var
  p: PThreadProgressPostData;
begin
  new(p);
  p^.Init();
  p^.Data1 := Data1;
  p^.Data2 := Data2;
  p^.Data3 := Data3;
  p^.OnMethod3 := OnSync;
  FCritical.Acquire;
  FSyncPool.Add(p);
  FCritical.Release;
end;

procedure TThreadProgressPost.PostP1(OnSync: TThreadProgressPostProc1);
var
  p: PThreadProgressPostData;
begin
  new(p);
  p^.Init();
  p^.OnProc1 := OnSync;
  FCritical.Acquire;
  FSyncPool.Add(p);
  FCritical.Release;
end;

procedure TThreadProgressPost.PostP2(Data1: Pointer; OnSync: TThreadProgressPostProc2);
var
  p: PThreadProgressPostData;
begin
  new(p);
  p^.Init();
  p^.Data1 := Data1;
  p^.OnProc2 := OnSync;
  FCritical.Acquire;
  FSyncPool.Add(p);
  FCritical.Release;
end;

procedure TThreadProgressPost.PostP3(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant; OnSync: TThreadProgressPostProc3);
var
  p: PThreadProgressPostData;
begin
  new(p);
  p^.Init();
  p^.Data1 := Data1;
  p^.Data2 := Data2;
  p^.Data3 := Data3;
  p^.OnProc3 := OnSync;
  FCritical.Acquire;
  FSyncPool.Add(p);
  FCritical.Release;
end;
