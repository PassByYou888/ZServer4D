program LogicServer;

{$R *.dres}

uses
  Vcl.Forms,
  LogicServerFrm in 'LogicServerFrm.pas' {LogicServerForm},
  ConnectManagerServerFrm in '..\ManagerServer\ConnectManagerServerFrm.pas' {ConnectManagerServerForm},
  FileStoreClientIntf in '..\FileStoreServer\FileStoreClientIntf.pas',
  DataStoreClientIntf in '..\DataStoreServer\DataStoreClientIntf.pas',
  CommonServiceDefine in '..\common\CommonServiceDefine.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TLogicServerForm, LogicServerForm);
  Application.Run;
end.
