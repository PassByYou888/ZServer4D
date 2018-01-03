program DBServer;

uses
  Vcl.Forms,
  DBServerFrm in 'DBServerFrm.pas' {DBServerForm},
  ConnectManagerServerFrm in '..\ManagerServer\ConnectManagerServerFrm.pas' {ConnectManagerServerForm},
  ManagerServer_ClientIntf in '..\ManagerServer\ManagerServer_ClientIntf.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDBServerForm, DBServerForm);
  Application.Run;
end.
