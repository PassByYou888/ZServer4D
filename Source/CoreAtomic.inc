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
  TCritical = TCriticalSection;

  PCritical_Struct = ^TCritical_Struct;

  TCritical_Struct = record
    Obj: TObject;
    Critical: TCritical;
  end;

var
  CoreCritical: TCritical;
  incrementCritical: TCritical;
  CriticalList: TCoreClassList;

procedure InitCriticalLock;
begin
  CoreCritical := TCritical.Create;
  incrementCritical := TCritical.Create;
  CriticalList := TCoreClassList.Create;
end;

procedure FreeCriticalLock;
var
  i: Integer;
begin
  for i := 0 to CriticalList.Count - 1 do
    begin
      DisposeObject(PCritical_Struct(CriticalList[i])^.Critical);
      Dispose(PCritical_Struct(CriticalList[i]));
    end;

  DisposeObject(CoreCritical);
  DisposeObject(incrementCritical);
  DisposeObject(CriticalList);
end;

procedure GetCriticalLock(const Obj: TObject; var output: PCritical_Struct);
var
  i, pi: Integer;
  p: PCritical_Struct;
begin
  CoreCritical.Acquire;
  try
    output := nil;
    pi := -1;
    p := nil;
    i := 0;
    while i < CriticalList.Count do
      begin
        if PCritical_Struct(CriticalList[i])^.Obj = Obj then
          begin
            output := PCritical_Struct(CriticalList[i]);
            Break;
          end
        else
          begin
            if (p = nil) and (PCritical_Struct(CriticalList[i])^.Obj = nil) then
              begin
                p := PCritical_Struct(CriticalList[i]);
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
            p^.Critical := TCritical.Create;
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
  p: PCritical_Struct;
begin
  GetCriticalLock(Obj, p);
  p^.Critical.Acquire;
end;

procedure _UnLockCriticalObj(Obj: TObject);
var
  p: PCritical_Struct;
begin
  GetCriticalLock(Obj, p);
  p^.Obj := nil;
  try
      p^.Critical.Release;
  except
      p^.Critical := TCritical.Create;
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
