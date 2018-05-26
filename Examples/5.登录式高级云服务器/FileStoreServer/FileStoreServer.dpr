program FileStoreServer;

uses
  Vcl.Forms,
  FileStoreServerFrm in 'FileStoreServerFrm.pas' {FileStoreServiceForm},
  FileStoreClientIntf in 'FileStoreClientIntf.pas',
  ConnectManagerServerFrm in '..\ManagerServer\ConnectManagerServerFrm.pas' {ConnectManagerServerForm},
  CommonServiceDefine in '..\common\CommonServiceDefine.pas',
  DataStoreClientIntf in '..\DataStoreServer\DataStoreClientIntf.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFileStoreServiceForm, FileStoreServiceForm);
  Application.Run;
end.
