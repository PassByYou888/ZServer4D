program OrderStructForThread;
{$APPTYPE CONSOLE}
{$R *.res}


uses
  System.SysUtils, CoreClasses, DoStatusIO;

// 该demo演示了线程2往线程1发数据并进行处理的编程范式
// TCriticalOrderStruct 序列结构是线程安全的

type
  // 如果使用fpc编译器，声明泛型需要specialize前缀
  // TDemoOrderStruct = specialize TCriticalOrderStruct<Integer>;
  // 这里加入编译器判断来声明
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
          DoStatus('完成: %d', [Order.Current^.Data]);
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
  DoStatus('Compute线程已经安全结束，按下回车键退出.');
  readln;
end.
