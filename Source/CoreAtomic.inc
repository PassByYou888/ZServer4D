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
  CoreCritical, incrementCritical: TCriticalSection;
  CriticalList: TCoreClassList;

procedure InitCriticalLock;
begin
  CoreCritical := TCriticalSection.Create;
  incrementCritical := TCriticalSection.Create;
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
  DisposeObject(incrementCritical);
  DisposeObject(CriticalList);
end;

procedure GetCriticalLock(const Obj: TObject; var output: PCriticalLock);
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
            inc(i);
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
  GetCriticalLock(Obj, p);
  if p^.Critical = nil then
      p^.Critical := TCriticalSection.Create;
  p^.Critical.Acquire;
end;

procedure _UnLockCriticalObj(Obj: TObject);
var
  p: PCriticalLock;
begin
  GetCriticalLock(Obj, p);
  p^.Obj := nil;
  try
      p^.Critical.Release;
  except
    DisposeObject(p^.Critical);
    p^.Critical := nil;
  end;
end;

procedure AtomInc(var x: Int64);
begin
{$IFDEF FPC}
  CoreCritical.Acquire;
  inc(x);
  CoreCritical.Release;
{$ELSE FPC}
  System.AtomicIncrement(x);
{$ENDIF FPC}
end;

procedure AtomInc(var x: Int64; const v: Int64);
begin
{$IFDEF FPC}
  CoreCritical.Acquire;
  inc(x, v);
  CoreCritical.Release;
{$ELSE FPC}
  System.AtomicIncrement(x, v);
{$ENDIF FPC}
end;

procedure AtomDec(var x: Int64);
begin
{$IFDEF FPC}
  CoreCritical.Acquire;
  dec(x);
  CoreCritical.Release;
{$ELSE FPC}
  System.AtomicDecrement(x);
{$ENDIF FPC}
end;

procedure AtomDec(var x: Int64; const v: Int64);
begin
{$IFDEF FPC}
  CoreCritical.Acquire;
  dec(x, v);
  CoreCritical.Release;
{$ELSE FPC}
  System.AtomicDecrement(x, v);
{$ENDIF FPC}
end;

procedure AtomInc(var x: UInt64);
begin
{$IFDEF FPC}
  CoreCritical.Acquire;
  inc(x);
  CoreCritical.Release;
{$ELSE FPC}
  System.AtomicIncrement(x);
{$ENDIF FPC}
end;

procedure AtomInc(var x: UInt64; const v: UInt64);
begin
{$IFDEF FPC}
  CoreCritical.Acquire;
  inc(x, v);
  CoreCritical.Release;
{$ELSE FPC}
  System.AtomicIncrement(x, v);
{$ENDIF FPC}
end;

procedure AtomDec(var x: UInt64);
begin
{$IFDEF FPC}
  CoreCritical.Acquire;
  dec(x);
  CoreCritical.Release;
{$ELSE FPC}
  System.AtomicDecrement(x);
{$ENDIF FPC}
end;

procedure AtomDec(var x: UInt64; const v: UInt64);
begin
{$IFDEF FPC}
  CoreCritical.Acquire;
  dec(x, v);
  CoreCritical.Release;
{$ELSE FPC}
  System.AtomicDecrement(x, v);
{$ENDIF FPC}
end;

procedure AtomInc(var x: Integer);
begin
{$IFDEF FPC}
  CoreCritical.Acquire;
  inc(x);
  CoreCritical.Release;
{$ELSE FPC}
  System.AtomicIncrement(x);
{$ENDIF FPC}
end;

procedure AtomInc(var x: Integer; const v: Integer);
begin
{$IFDEF FPC}
  CoreCritical.Acquire;
  inc(x, v);
  CoreCritical.Release;
{$ELSE FPC}
  System.AtomicIncrement(x, v);
{$ENDIF FPC}
end;

procedure AtomDec(var x: Integer);
begin
{$IFDEF FPC}
  CoreCritical.Acquire;
  dec(x);
  CoreCritical.Release;
{$ELSE FPC}
  System.AtomicDecrement(x);
{$ENDIF FPC}
end;

procedure AtomDec(var x: Integer; const v: Integer);
begin
{$IFDEF FPC}
  CoreCritical.Acquire;
  dec(x, v);
  CoreCritical.Release;
{$ELSE FPC}
  System.AtomicDecrement(x, v);
{$ENDIF FPC}
end;

procedure AtomInc(var x: Cardinal);
begin
{$IFDEF FPC}
  CoreCritical.Acquire;
  inc(x);
  CoreCritical.Release;
{$ELSE FPC}
  System.AtomicIncrement(x);
{$ENDIF FPC}
end;

procedure AtomInc(var x: Cardinal; const v: Cardinal);
begin
{$IFDEF FPC}
  CoreCritical.Acquire;
  inc(x, v);
  CoreCritical.Release;
{$ELSE FPC}
  System.AtomicIncrement(x, v);
{$ENDIF FPC}
end;

procedure AtomDec(var x: Cardinal);
begin
{$IFDEF FPC}
  CoreCritical.Acquire;
  dec(x);
  CoreCritical.Release;
{$ELSE FPC}
  System.AtomicDecrement(x);
{$ENDIF FPC}
end;

procedure AtomDec(var x: Cardinal; const v: Cardinal);
begin
{$IFDEF FPC}
  CoreCritical.Acquire;
  dec(x, v);
  CoreCritical.Release;
{$ELSE FPC}
  System.AtomicDecrement(x, v);
{$ENDIF FPC}
end;
