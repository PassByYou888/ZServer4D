program AsyncStateDemo;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  Variants,
  CoreClasses,
  DoStatusIO;

// 简单演示在线程中使用安全状态机
procedure StateDemo();
var
  State: TAtomBool;
begin
  // 线程安全:状态变量
  State := TAtomBool.Create(False);
  // 工作线程
  TCompute.RunP_NP(procedure
    var
      i: Cardinal;
    begin
      for i := 0 to $FFFFFFFF do
          nop;
      DoStatus('完成一个周期');
      // 改变状态变量
      State.V := True;
    end);
  DoStatus('等待循环周期完成.');
  while not State.V do
    begin
      DoStatus();
      TCompute.Sleep(1);
    end;
  DisposeObject(State);
end;

// 简单演示在线程中使用Post机制
// post等同于把代码发送给一个我们构建的目标线程来执行,post不会挑线性类型,不区分主线程/子线程,任何线程都能使用
procedure PostDemo();
var
  P: TThreadProgressPost; // TThreadProgressPost,Post机制支持
  done: TAtomBool;        // 线程安全:状态变量
  over: boolean;          // local变量,在调用完成后会自动销毁,故此,我们必须在这里做一个监视work_结束状态
begin
  // 线程安全:状态变量
  done := TAtomBool.Create(False);
  P := TThreadProgressPost.Create(0);
  // 工作线程
  TCompute.RunP_NP(procedure
    begin
      P.ThreadID := TCompute.CurrentThread.ThreadID;
      while not done.V do
        begin
          P.Progress(); // post主循环
          TCompute.Sleep(1);
        end;
      over := True;
    end);
  // 往工作线程发任务
  // 这些任务会严格按输入顺序执行
  P.PostP1(procedure
    begin
      DoStatus(1);
    end);
  P.PostP1(procedure
    begin
      DoStatus(2);
    end);
  TCompute.RunP_NP(procedure
    var
      i: Integer;
    begin
      for i := 3 to 20 do
        begin
          TCompute.Sleep(100);
          P.PostP3(nil, nil, i, procedure(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant)
            begin
              DoStatus(VarToStr(Data3));
            end);
        end;
      // 工作线程结束
      P.PostP1(procedure
        begin
          done.V := True;
        end);
    end);
  // 等执行完
  while not over do
    begin
      DoStatus();
      TCompute.Sleep(1);
    end;
  DisposeObjectAndNil(P);
  DisposeObjectAndNil(done);
end;

begin
  StateDemo();
  PostDemo();
  DoStatus('回车键结束程序.');
  readln;

end.
