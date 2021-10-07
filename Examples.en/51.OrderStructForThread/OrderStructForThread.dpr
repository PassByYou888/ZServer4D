program OrderStructForThread;
{$APPTYPE CONSOLE}
{$R *.res}


uses
  System.SysUtils, CoreClasses, DoStatusIO;

{  This demo demonstrates the programming paradigm that thread 2 sends data to thread 1 and processes it  }
{  The tcriticalorderstruct sequence structure is thread safe  }

type
  {  If you use the FPC compiler, declaring generics requires the specify prefix  }
  // TDemoOrderStruct = specialize TCriticalOrderStruct<Integer>;
  {  Add compiler judgment here to declare  }
  TDemoOrderStruct = {$IFDEF FPC}specialize {$ENDIF FPC} TCriticalOrderStruct<Integer>;

var
  Activted: TAtomBool;
  ThNum: Integer;
  Order: TDemoOrderStruct;

procedure th1();
begin
  while Activted.V do
    begin
      if Order.Current <> nil then
        begin
          DoStatus('Completed: %d', [Order.Current^.Data]);
          Order.Next;
        end
      else
          TCompute.Sleep(1);
    end;
  AtomDec(ThNum);
end;

procedure th2();
var
  i: Integer;
begin
  for i := 1 to 15 do
    begin
      Order.Push(i);
      TCompute.Sleep(100);
    end;
  Activted.V := False;
  AtomDec(ThNum);
end;

begin
  Activted := TAtomBool.Create(True);
  Order := TDemoOrderStruct.Create;
  ThNum := 2;
  TCompute.RunC_NP(th1);
  TCompute.RunC_NP(th2);
  while ThNum > 0 do
    begin
      DoStatus;
      TCompute.Sleep(1);
    end;
  Activted.Free;
  Order.Free;
  DoStatus('The compute thread has finished safely. Press enter to exit');
  readln;
end.
