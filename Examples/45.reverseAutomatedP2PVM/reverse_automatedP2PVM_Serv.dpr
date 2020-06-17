program reverse_automatedP2PVM_Serv;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  CoreClasses,
  PascalStrings,
  UnicodeMixedLib,
  DoStatusIO,
  CommunicationFramework,
  PhysicsIO;

// reverse为反向AutomatedP2PVM模型,见 31.reverseP2PVM 的相关demo
procedure RunServ;
var
  phyServ: TPhysicsServer;

  vm_cli1, vm_cli2, vm_cli3: TCommunicationFrameworkWithP2PVM_Client; // 由p2pVM驱动的通讯隧道,可以在上面套各种类似双通道,数据库,文件传输应用,见VM的相关demo
begin
  vm_cli1 := TCommunicationFrameworkWithP2PVM_Client.Create;
  vm_cli2 := TCommunicationFrameworkWithP2PVM_Client.Create;
  vm_cli3 := TCommunicationFrameworkWithP2PVM_Client.Create;

  phyServ := TPhysicsServer.Create;
  phyServ.AutomatedP2PVMClientBind.AddClient(vm_cli1, '::', 99);
  phyServ.AutomatedP2PVMClientBind.AddClient(vm_cli2, '::', 199);
  phyServ.AutomatedP2PVMClientBind.AddClient(vm_cli3, '::', 299);
  phyServ.AutomatedP2PVMClient := True;
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
