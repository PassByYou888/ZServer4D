procedure TOrderStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.DoFree(p: POrderStruct_);
begin
  try
    if Assigned(FOnFreeOrderStruct) then
        FOnFreeOrderStruct(p^.Data);
    Dispose(p);
  except
  end;
end;

constructor TOrderStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.Create;
begin
  inherited Create;
  FFirst := nil;
  FLast := nil;
  FNum := 0;
  FOnFreeOrderStruct := nil;
end;

destructor TOrderStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TOrderStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.Clear;
var
  p, tmp: POrderStruct_;
begin
  p := FFirst;
  while p <> nil do
    begin
      tmp := p^.Next;
      DoFree(p);
      p := tmp;
    end;
  FFirst := nil;
  FLast := nil;
  FNum := 0;
end;

procedure TOrderStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.Next;
var
  tmp: POrderStruct_;
begin
  if FFirst <> nil then
    begin
      tmp := FFirst^.Next;
      DoFree(FFirst);
      FFirst := tmp;
      if FFirst = nil then
          FLast := nil;
      Dec(FNum);
    end;
end;

procedure TOrderStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.Push(Data: T_);
var
  p: POrderStruct_;
begin
  new(p);
  p^.Data := Data;
  p^.Next := nil;

  Inc(FNum);
  if (FFirst = nil) and (FLast = nil) then
    begin
      FFirst := p;
      FLast := p;
    end
  else if FLast <> nil then
    begin
      FLast^.Next := p;
      FLast := p;
    end;
end;

procedure TOrderPtrStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.DoFree(p: POrderPtrStruct_);
begin
  try
    if Assigned(FOnFreeOrderStruct) then
        FOnFreeOrderStruct(p^.Data)
    else
        Dispose(p^.Data);
    Dispose(p);
  except
  end;
end;

constructor TOrderPtrStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.Create;
begin
  inherited Create;
  FFirst := nil;
  FLast := nil;
  FNum := 0;
  FOnFreeOrderStruct := nil;
end;

destructor TOrderPtrStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TOrderPtrStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.Clear;
var
  p, tmp: POrderPtrStruct_;
begin
  p := FFirst;
  while p <> nil do
    begin
      tmp := p^.Next;
      DoFree(p);
      p := tmp;
    end;
  FFirst := nil;
  FLast := nil;
  FNum := 0;
end;

procedure TOrderPtrStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.Next;
var
  tmp: POrderPtrStruct_;
begin
  if FFirst <> nil then
    begin
      tmp := FFirst^.Next;
      DoFree(FFirst);
      FFirst := tmp;
      if FFirst = nil then
          FLast := nil;
      Dec(FNum);
    end;
end;

procedure TOrderPtrStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.Push(Data: T_);
var
  p: POrderPtrStruct_;
begin
  new(p);
  new(p^.Data);
  p^.Data^ := Data;
  p^.Next := nil;

  Inc(FNum);
  if (FFirst = nil) and (FLast = nil) then
    begin
      FFirst := p;
      FLast := p;
    end
  else if FLast <> nil then
    begin
      FLast^.Next := p;
      FLast := p;
    end;
end;

procedure TOrderPtrStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.PushPtr(Data: PT_);
var
  p: POrderPtrStruct_;
begin
  new(p);
  p^.Data := Data;
  p^.Next := nil;

  Inc(FNum);
  if (FFirst = nil) and (FLast = nil) then
    begin
      FFirst := p;
      FLast := p;
    end
  else if FLast <> nil then
    begin
      FLast^.Next := p;
      FLast := p;
    end;
end;

procedure TCriticalOrderStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.DoFree(p: POrderStruct_);
begin
  try
    if Assigned(FOnFreeCriticalOrderStruct) then
        FOnFreeCriticalOrderStruct(p^.Data);
    Dispose(p);
  except
  end;
end;

constructor TCriticalOrderStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.Create;
begin
  inherited Create;
  FCritical := TCritical.Create;
  FFirst := nil;
  FLast := nil;
  FNum := 0;
  FOnFreeCriticalOrderStruct := nil;
end;

destructor TCriticalOrderStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.Destroy;
begin
  Clear;
  FCritical.Free;
  inherited Destroy;
end;

procedure TCriticalOrderStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.Clear;
var
  p, tmp: POrderStruct_;
begin
  FCritical.Lock;
  p := FFirst;
  while p <> nil do
    begin
      tmp := p^.Next;
      DoFree(p);
      p := tmp;
    end;
  FFirst := nil;
  FLast := nil;
  FNum := 0;
  FCritical.UnLock;
end;

function TCriticalOrderStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.GetCurrent: POrderStruct_;
begin
  FCritical.Lock;
  Result := FFirst;
  FCritical.UnLock;
end;

procedure TCriticalOrderStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.Next;
var
  tmp: POrderStruct_;
begin
  FCritical.Lock;
  if FFirst <> nil then
    begin
      tmp := FFirst^.Next;
      DoFree(FFirst);
      FFirst := tmp;
      if FFirst = nil then
          FLast := nil;
      Dec(FNum);
    end;
  FCritical.UnLock;
end;

procedure TCriticalOrderStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.Push(Data: T_);
var
  p: POrderStruct_;
begin
  new(p);
  p^.Data := Data;
  p^.Next := nil;

  FCritical.Lock;
  Inc(FNum);
  if (FFirst = nil) and (FLast = nil) then
    begin
      FFirst := p;
      FLast := p;
    end
  else if FLast <> nil then
    begin
      FLast^.Next := p;
      FLast := p;
    end;
  FCritical.UnLock;
end;

function TCriticalOrderStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.GetNum: NativeInt;
begin
  FCritical.Lock;
  Result := FNum;
  FCritical.UnLock;
end;

procedure TCriticalOrderPtrStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.DoFree(p: POrderPtrStruct_);
begin
  try
    if Assigned(FOnFreeCriticalOrderStruct) then
        FOnFreeCriticalOrderStruct(p^.Data)
    else
        Dispose(p^.Data);
    Dispose(p);
  except
  end;
end;

constructor TCriticalOrderPtrStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.Create;
begin
  inherited Create;
  FCritical := TCritical.Create;
  FFirst := nil;
  FLast := nil;
  FNum := 0;
  FOnFreeCriticalOrderStruct := nil;
end;

destructor TCriticalOrderPtrStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.Destroy;
begin
  Clear;
  FCritical.Free;
  inherited Destroy;
end;

procedure TCriticalOrderPtrStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.Clear;
var
  p, tmp: POrderPtrStruct_;
begin
  FCritical.Lock;
  p := FFirst;
  while p <> nil do
    begin
      tmp := p^.Next;
      DoFree(p);
      p := tmp;
    end;
  FFirst := nil;
  FLast := nil;
  FNum := 0;
  FCritical.UnLock;
end;

function TCriticalOrderPtrStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.GetCurrent: POrderPtrStruct_;
begin
  FCritical.Lock;
  Result := FFirst;
  FCritical.UnLock;
end;

procedure TCriticalOrderPtrStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.Next;
var
  tmp: POrderPtrStruct_;
begin
  FCritical.Lock;
  if FFirst <> nil then
    begin
      tmp := FFirst^.Next;
      DoFree(FFirst);
      FFirst := tmp;
      if FFirst = nil then
          FLast := nil;
      Dec(FNum);
    end;
  FCritical.UnLock;
end;

procedure TCriticalOrderPtrStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.Push(Data: T_);
var
  p: POrderPtrStruct_;
begin
  new(p);
  new(p^.Data);
  p^.Data^ := Data;
  p^.Next := nil;

  FCritical.Lock;
  Inc(FNum);
  if (FFirst = nil) and (FLast = nil) then
    begin
      FFirst := p;
      FLast := p;
    end
  else if FLast <> nil then
    begin
      FLast^.Next := p;
      FLast := p;
    end;
  FCritical.UnLock;
end;

procedure TCriticalOrderPtrStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.PushPtr(Data: PT_);
var
  p: POrderPtrStruct_;
begin
  new(p);
  p^.Data := Data;
  p^.Next := nil;

  FCritical.Lock;
  Inc(FNum);
  if (FFirst = nil) and (FLast = nil) then
    begin
      FFirst := p;
      FLast := p;
    end
  else if FLast <> nil then
    begin
      FLast^.Next := p;
      FLast := p;
    end;
  FCritical.UnLock;
end;

function TCriticalOrderPtrStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.GetNum: NativeInt;
begin
  FCritical.Lock;
  Result := FNum;
  FCritical.UnLock;
end;
