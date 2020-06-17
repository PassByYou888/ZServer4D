program automatedP2PVM_Cli;

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
procedure RunAutomatedP2PVM_Client;
var
  phyCli: TPhysicsClient;
  vm_cli1, vm_cli2, vm_cli3: TCommunicationFrameworkWithP2PVM_Client; // 由p2pVM驱动的通讯隧道,可以在上面套各种类似双通道,数据库,文件传输应用,见VM的相关demo
  tk: TTimeTick;
begin
  vm_cli1 := TCommunicationFrameworkWithP2PVM_Client.Create;
  vm_cli2 := TCommunicationFrameworkWithP2PVM_Client.Create;
  vm_cli3 := TCommunicationFrameworkWithP2PVM_Client.Create;

  phyCli := TPhysicsClient.Create;
  phyCli.AutomatedP2PVMClientBind.AddClient(vm_cli1, '::', 99);
  phyCli.AutomatedP2PVMClientBind.AddClient(vm_cli2, '::', 199);
  phyCli.AutomatedP2PVMClientBind.AddClient(vm_cli3, '::', 299);
  phyCli.AutomatedP2PVMClient := True;
  phyCli.AutomatedP2PVMAuthToken := '123456';
  phyCli.Connect('127.0.0.1', 9799);

  tk := GetTimeTick;
  while GetTimeTick - tk < 5000 do
    begin
      phyCli.Progress;
      DoStatus();
      CheckThreadSynchronize(1);
    end;

  phyCli.Disconnect;
  disposeObject(vm_cli1);
  disposeObject(vm_cli2);
  disposeObject(vm_cli3);
  disposeObject(phyCli);
end;

begin
  RunAutomatedP2PVM_Client();
end.
