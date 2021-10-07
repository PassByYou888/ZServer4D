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

{  Automated p2pvm is a very simplified p2pvm application paradigm, which has the ability to drive the p2pvm framework with very short code  }
{  Automated P2P VM is also the communication foundation of ECS 3.0  }
procedure RunServ;
var
  phyServ: TPhysicsServer;
  vm_serv1, vm_serv2, vm_serv3: TCommunicationFrameworkWithP2PVM_Server; {  The communication tunnel driven by p2pvm can be set with various similar dual channel, database and file transmission applications. See the relevant demo of VM  }
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
