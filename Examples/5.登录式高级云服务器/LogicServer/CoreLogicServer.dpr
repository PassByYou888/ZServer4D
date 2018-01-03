program CoreLogicServer;

uses
  Vcl.Forms,
  CoreLogicServerFrm in 'CoreLogicServerFrm.pas' {CoreLogicServerForm},
  ConnectManagerServerFrm in '..\ManagerServer\ConnectManagerServerFrm.pas' {ConnectManagerServerForm},
  ManagerServer_ClientIntf in '..\ManagerServer\ManagerServer_ClientIntf.pas',
  DBClientIntf in '..\DBServer\DBClientIntf.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TCoreLogicServerForm, CoreLogicServerForm);
  Application.Run;
end.
