program FragmentServiceDemo;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  CoreClasses,
  UnicodeMixedLib,
  DoStatusIO,
  CommunicationFramework,
  PhysicsIO,
  CommunicationFrameworkDoubleTunnelIO_NoAuth;

var
  logic_recv, logic_send: TCommunicationFrameworkWithP2PVM_Server;
  logic: TCommunicationFramework_DoubleTunnelService_NoAuth;
  phyServ: TPhysicsServer;

begin
  logic_recv := TCommunicationFrameworkWithP2PVM_Server.Create;
  logic_send := TCommunicationFrameworkWithP2PVM_Server.Create;
  logic_recv.QuietMode := True;
  logic_send.QuietMode := True;

  logic := TCommunicationFramework_DoubleTunnelService_NoAuth.Create(logic_recv, logic_send);
  logic.RegisterCommand;
  logic.FileReceiveDirectory := umlGetCurrentPath;

  phyServ := TPhysicsServer.Create;
  phyServ.QuietMode := True;
  phyServ.AutomatedP2PVMServiceBind.AddService(logic_recv, '::', 99);
  phyServ.AutomatedP2PVMServiceBind.AddService(logic_send, '::', 98);
  phyServ.AutomatedP2PVMService := True;
  phyServ.AutomatedP2PVMAuthToken := '123456';

  if phyServ.StartService('', 9799) then
      DoStatus('ÕìÌý¶Ë¿Ú³É¹¦: %d', [9799]);

  while True do
    begin
      phyServ.Progress;
      logic.Progress;
      CheckThreadSynchronize(1);
    end;

end.
