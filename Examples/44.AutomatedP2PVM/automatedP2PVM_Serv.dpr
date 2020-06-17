program automatedP2PVM_Serv;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  CoreClasses,
  PascalStrings,
  UnicodeMixedLib,
  DoStatusIO,
  CommunicationFramework,
  PhysicsIO;

// AutomatedP2PVM是一种极简化的p2pVM应用范式,它有能力用极短代码驱动p2pVM框架
// AutomatedP2PVM也是云服务器3.0的通讯地基
procedure RunServ;
var
  phyServ: TPhysicsServer;
  vm_serv1, vm_serv2, vm_serv3: TCommunicationFrameworkWithP2PVM_Server; // 由p2pVM驱动的通讯隧道,可以在上面套各种类似双通道,数据库,文件传输应用,见VM的相关demo
begin
  vm_serv1 := TCommunicationFrameworkWithP2PVM_Server.Create;
  vm_serv2 := TCommunicationFrameworkWithP2PVM_Server.Create;
  vm_serv3 := TCommunicationFrameworkWithP2PVM_Server.Create;

  phyServ := TPhysicsServer.Create;
  phyServ.AutomatedP2PVMServiceBind.AddService(vm_serv1, '::', 99);
  phyServ.AutomatedP2PVMServiceBind.AddService(vm_serv2, '::', 199);
  phyServ.AutomatedP2PVMServiceBind.AddService(vm_serv3, '::', 299);
  phyServ.AutomatedP2PVMService := True;
  phyServ.AutomatedP2PVMAuthToken := '123456';

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
