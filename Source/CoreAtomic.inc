// used Critical Simulate Atomic with TMonitor.Enter(obj) and TMonitor.Exit(obj)
// if CriticalSimulateAtomic defined so performance to be reduced

{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }

type
  PCriticalLock = ^TCriticalLock;

  TCriticalLock = packed record
    Obj: TObject;
    Critical: TCriticalSection;
  end;

var
  CoreCritical: TCriticalSection;
  CriticalList: TCoreClassList;

procedure InitCriticalLock;
begin
  CoreCritical := TCriticalSection.Create;
  CriticalList := TCoreClassList.Create;
end;

procedure FreeCriticalLock;
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

procedure GetCriticalLock(Obj: TObject; var output: PCriticalLock);
var
  i, pi: Integer;
  p: PCriticalLock;
begin
  CoreCritical.Acquire;
  try
    output := nil;
    pi := -1;
    p := nil;
    i := 0;
    while i < CriticalList.Count do
      begin
        if PCriticalLock(CriticalList[i])^.Obj = Obj then
          begin
            output := PCriticalLock(CriticalList[i]);
            Break;
          end
        else
          begin
            if (p = nil) and (PCriticalLock(CriticalList[i])^.Obj = nil) then
              begin
                p := PCriticalLock(CriticalList[i]);
                pi := i;
              end;
            Inc(i);
          end;
      end;
    if output = nil then
      begin
        if p <> nil then
          begin
            p^.Obj := Obj;
            output := p;
            if pi > 0 then
                CriticalList.Move(pi, 0);
          end
        else
          begin
            new(p);
            p^.Obj := Obj;
            p^.Critical := TCriticalSection.Create;
            CriticalList.Insert(0, p);
            output := p;
          end;
      end;
  finally
      CoreCritical.Release;
  end;
end;

procedure _LockCriticalObj(Obj: TObject);
var
  p: PCriticalLock;
begin
  if Obj = nil then
      GetCriticalLock(CoreCritical, p)
  else
      GetCriticalLock(Obj, p);
  p^.Critical.Acquire;
end;

procedure _UnLockCriticalObj(Obj: TObject);
var
  p: PCriticalLock;
begin
  if Obj = nil then
      GetCriticalLock(CoreCritical, p)
  else
      GetCriticalLock(Obj, p);
  p^.Obj := nil;
  p^.Critical.Release;
end; 
