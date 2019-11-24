function TAtomVar.GetValue: T_;
begin
  Critical.Acquire;
  Result := FValue__;
  Critical.Release;
end;

procedure TAtomVar.SetValue(const Value_: T_);
begin
  Critical.Acquire;
  FValue__ := Value_;
  Critical.Release;
end;

function TAtomVar.GetValueP: PT_;
begin
  Result := @FValue__;
end;

constructor TAtomVar.Create(Value_: T_);
begin
  inherited Create;
  FValue__ := Value_;
  Critical := TCritical.Create;
end;

destructor TAtomVar.Destroy;
begin
  Critical.Free;
  inherited Destroy;
end;

function TAtomVar.Lock: T_;
begin
  Critical.Acquire;
  Result := FValue__;
end;

function TAtomVar.LockP: PT_;
begin
  Critical.Acquire;
  Result := @FValue__;
end;

procedure TAtomVar.UnLock(const Value_: T_);
begin
  FValue__ := Value_;
  Critical.Release;
end;

procedure TAtomVar.UnLock(const Value_: PT_);
begin
  FValue__ := Value_^;
  Critical.Release;
end;

procedure TAtomVar.UnLock();
begin
  Critical.Release;
end;
