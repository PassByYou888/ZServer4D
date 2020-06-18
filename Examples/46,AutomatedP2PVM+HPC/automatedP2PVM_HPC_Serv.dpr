program automatedP2PVM_HPC_Serv;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  CoreClasses,
  PascalStrings,
  UnicodeMixedLib,
  DoStatusIO,
  CommunicationFramework,
  PhysicsIO,
  DataFrameEngine,
  TextParsing,
  zExpression,
  opCode;

// 这里演示了hpc怎样负载大规模计算
procedure cmd_runExp(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
begin
  // nop函数在主线程区段
  nop;
  // RunHPC_StreamP被调用时会IO暂停反馈,并立即将请求发往子线程执行
  // RunHPC_StreamP由hpc的专用内核线程池调度,安全调度,不是delphi/fpc内置线程池
  RunHPC_StreamP(Sender, nil, nil, InData, OutData, procedure(ThSender: THPC_Stream; ThInData, ThOutData: TDataFrameEngine)
    var
      num: Integer;
      exp: U_String;
      i: Integer;
      tk: TTimeTick;
      op: TOpCode;
    begin
      // 以下代码都在线程区段执行
      tk := GetTimeTick;
      num := ThInData.Reader.ReadInteger;
      exp := ThInData.Reader.ReadString;
      op := BuildAsOpCode(tsPascal, exp);
      for i := 0 to num - 1 do
          op.Execute();
      // 往ThOutData里面丢数据,即表示要反馈.这里是反馈执行num数量的表达式时间
      ThOutData.WriteUInt64(GetTimeTick - tk);
    end); // 到这一步执行完成后,IO会恢复反馈
end;

// AutomatedP2PVM是一种极简化的p2pVM应用范式,它有能力用极短代码驱动p2pVM框架
// AutomatedP2PVM也是云服务器3.0的通讯地基
procedure RunServ;
var
  phyServ: TPhysicsServer;
  vm_serv1, vm_serv2, vm_serv3, vm_serv4: TCommunicationFrameworkWithP2PVM_Server; // 由p2pVM驱动的通讯隧道,可以在上面套各种类似双通道,数据库,文件传输应用,见VM的相关demo
begin
  vm_serv1 := TCommunicationFrameworkWithP2PVM_Server.Create;
  vm_serv2 := TCommunicationFrameworkWithP2PVM_Server.Create;
  vm_serv3 := TCommunicationFrameworkWithP2PVM_Server.Create;
  vm_serv4 := TCommunicationFrameworkWithP2PVM_Server.Create;

  phyServ := TPhysicsServer.Create;
  phyServ.AutomatedP2PVMServiceBind.AddService(vm_serv1, '::', 99);    // ::等同于0:0:0:0:0:0 是p2pVM中的虚拟Ipv6,不是真实物理地址,该地址任意给
  phyServ.AutomatedP2PVMServiceBind.AddService(vm_serv2, '::', 199);   // ::等同于0:0:0:0:0:0 是p2pVM中的虚拟Ipv6,不是真实物理地址,该地址任意给
  phyServ.AutomatedP2PVMServiceBind.AddService(vm_serv3, '::', 299);   // ::等同于0:0:0:0:0:0 是p2pVM中的虚拟Ipv6,不是真实物理地址,该地址任意给
  phyServ.AutomatedP2PVMServiceBind.AddService(vm_serv4, '88::', 399); // 88::等同于88:0:0:0:0:0 是p2pVM中的虚拟Ipv6,不是真实物理地址,该地址任意给
  phyServ.AutomatedP2PVMService := True;
  phyServ.AutomatedP2PVMAuthToken := '123456';
  vm_serv4.RegisterStream('runExp').OnExecuteCall := cmd_runExp; // 注册指令
  phyServ.StartService('', 9799);

  while True do
    begin
      phyServ.Progress;
      DoStatus();
      CheckThreadSynchronize(1);
    end;
end;

begin
  RunServ;

end.
