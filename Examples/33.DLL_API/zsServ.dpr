program zsServ;

{$APPTYPE CONSOLE}


uses
  CoreClasses,
  DoStatusIO,
  PhysicsIO,
  CommunicationFramework,
  CommunicationTest;

procedure runserv;
var
  serv: TPhysicsServer;
  test: TCommunicationTestIntf;
begin
  serv := TPhysicsServer.Create;

  test := TCommunicationTestIntf.Create;
  test.RegCmd(serv);

  if serv.StartService('0.0.0.0', 8191) then
      DoStatus('test service ok.');

  while true do
    begin
      serv.Progress;
      CoreClasses.CheckThreadSynchronize(10);
    end;
end;

begin
  runserv;

end.
