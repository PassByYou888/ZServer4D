// used Critical Simulate Atomic with TMonitor.Enter(obj) and TMonitor.Exit(obj)
// if CriticalSimulateAtomic defined so performance to be reduced

{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ ****************************************************************************** }

type
  PCriticalLock = ^TCriticalLock;

  TCriticalLock = record
    Obj: TObject;
    Critical: TCriticalSection;
  end;

var
  CoreCritical: TCriticalSection = nil;
  CriticalList: TCoreClassList   = nil;

procedure InitCriticalLock; {$IFDEF INLINE_ASM} inline; {$ENDIF}
begin
  CoreCritical := TCriticalSection.Create;
  CriticalList := TCoreClassList.Create;
end;

procedure FreeCriticalLock; {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  i: Integer;
begin
  for i := 0 to CriticalList.Count - 1 do
    begin
      DisposeObject(PCriticalLock(CriticalList[i])^.Critical);
      Dispose(PCriticalLock(CriticalList[i]));
    end;

  DisposeObject(CoreCritical);
  DisposeObject(CriticalList);
end;

function GetCriticalLock(Obj: TObject): PCriticalLock; {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  i : Integer;
  pi: Integer;
  p : PCriticalLock;
begin
  Result := nil;
  pi := -1;
  p := nil;
  i := 0;
  while i < CriticalList.Count do
    begin
      if PCriticalLock(CriticalList[i])^.Obj = Obj then
        begin
          Result := PCriticalLock(CriticalList[i]);
          break;
        end
      else
        begin
          if (p = nil) and (PCriticalLock(CriticalList[i])^.Obj = nil) then
            begin
              p := PCriticalLock(CriticalList[i]);
              pi := i;
            end;
          inc(i);
        end;
    end;
  if Result = nil then
    begin
      if p <> nil then
        begin
          p^.Obj := Obj;
          Result := p;
          if pi > 0 then
              CriticalList.Move(pi, 0);
        end
      else
        begin
          new(p);
          p^.Obj := Obj;
          p^.Critical := TCriticalSection.Create;
          CriticalList.Insert(0, p);
          Result := p;
        end;
    end;
end;

procedure _LockCriticalObj(Obj: TObject); {$IFDEF INLINE_ASM} inline; {$ENDIF}
begin
  CoreCritical.Acquire;
  if Obj = nil then
      GetCriticalLock(CoreCritical)^.Critical.Acquire
  else
      GetCriticalLock(Obj)^.Critical.Acquire;
  CoreCritical.Release;
end;

procedure _UnLockCriticalObj(Obj: TObject); {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  p: PCriticalLock;
begin
  CoreCritical.Acquire;
  if Obj = nil then
      p := GetCriticalLock(CoreCritical)
  else
      p := GetCriticalLock(Obj);
  p^.Critical.Release;
  p^.Obj := nil;
  CoreCritical.Release;
end;
