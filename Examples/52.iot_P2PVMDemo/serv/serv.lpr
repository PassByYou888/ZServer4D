program serv;

{$mode objfpc}{$H+}

uses
  jemalloc4p,
  //cthreads,
  Classes, SysUtils, CoreClasses, PascalStrings, UnicodeMixedLib,
  DataFrameEngine, DoStatusIO, CommunicationFramework,
  CommunicationFrameworkDoubleTunnelIO_NoAuth, PhysicsIO,
  zExpression;

var
  phyIO: TPhysicsServer;
  recvIO, sendIO: TCommunicationFrameworkWithP2PVM_Server;
  doubleServ: TDTService_NoAuth;

procedure cmd_runExp(sender: TPeerIO; InData, OutData: TDFE);
begin
  OutData.WriteString(EStr(InData.R.ReadString));
end;

procedure InitEnvir;
begin
  phyIO := TPhysicsServer.Create;

  recvIO := TCommunicationFrameworkWithP2PVM_Server.Create;
  recvIO.StartService('::', 1);
  recvIO.RegisterStream('runExp').OnExecuteCall := @cmd_runExp;

  sendIO := TCommunicationFrameworkWithP2PVM_Server.Create;
  sendIO.StartService('::', 2);

  phyIO.AutomatedP2PVMBindService.AddService(recvIO);
  phyIO.AutomatedP2PVMBindService.AddService(sendIO);
  phyIO.AutomatedP2PVMService := True;
  phyIO.AutomatedP2PVMAuthToken := 'IOT_p2pVM';

  if phyIO.StartService('0.0.0.0', 7189) then
      DoStatus('Listening 7189 successed.');

  doubleServ := TDTService_NoAuth.Create(recvIO, sendIO);
  doubleServ.RegisterCommand;

  // mainloop
  while True do
    begin
      CheckThreadSynchronize;
      phyIO.Progress;
      doubleServ.Progress;
      TCompute.Sleep(1);
    end;
end;

begin
  writeln('IOT p2pVM expression Service.');
  InitEnvir;

end.
