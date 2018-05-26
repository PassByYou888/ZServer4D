program DataStoreServer;

uses
  Vcl.Forms,
  DataStoreServerFrm in 'DataStoreServerFrm.pas' {DataStoreServerForm},
  DataStoreClientIntf in 'DataStoreClientIntf.pas',
  ConnectManagerServerFrm in '..\ManagerServer\ConnectManagerServerFrm.pas' {ConnectManagerServerForm},
  CommonServiceDefine in '..\common\CommonServiceDefine.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDataStoreServerForm, DataStoreServerForm);
  Application.Run;
end.
