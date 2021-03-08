procedure TOrderStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.DoFree(p: POrederStruct_);
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
  p, tmp: POrederStruct_;
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

function TOrderStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.Current: PT_;
begin
  if FFirst <> nil then
      Result := FFirst^.Data
  else
      Result := nil;
end;

procedure TOrderStruct{$IFNDEF FPC}<T_>{$ENDIF FPC}.Next;
var
  tmp: POrederStruct_;
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
  p: POrederStruct_;
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
