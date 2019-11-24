function TAtomVar<T_>.GetValue: T_;
begin
  Critical.Acquire;
  Result := FValue__;
  Critical.Release;
end;

procedure TAtomVar<T_>.SetValue(const Value_: T_);
begin
  Critical.Acquire;
  FValue__ := Value_;
  Critical.Release;
end;

function TAtomVar<T_>.GetValueP: PT_;
begin
  Result := @FValue__;
end;

constructor TAtomVar<T_>.Create(Value_: T_);
begin
  inherited Create;
  FValue__ := Value_;
  Critical := TCritical.Create;
end;

destructor TAtomVar<T_>.Destroy;
begin
  Critical.Free;
  inherited Destroy;
end;

function TAtomVar<T_>.Lock: T_;
begin
  Critical.Acquire;
  Result := FValue__;
end;

function TAtomVar<T_>.LockP: PT_;
begin
  Critical.Acquire;
  Result := @FValue__;
end;

procedure TAtomVar<T_>.UnLock(const Value_: T_);
begin
  FValue__ := Value_;
  Critical.Release;
end;

procedure TAtomVar<T_>.UnLock(const Value_: PT_);
begin
  FValue__ := Value_^;
  Critical.Release;
end;

procedure TAtomVar<T_>.UnLock();
begin
  Critical.Release;
end;
