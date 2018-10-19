program DataStoreViewClient;

uses
  Vcl.Forms,
  DBViewCliFrm in 'DBViewCliFrm.pas' {DBViewCliForm},
  CommonServiceDefine in '..\common\CommonServiceDefine.pas',
  DataStoreClientIntf in '..\DataStoreServer\DataStoreClientIntf.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDBViewCliForm, DBViewCliForm);
  Application.Run;
end.
