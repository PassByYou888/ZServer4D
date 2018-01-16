program FOGComputeServer;

uses
  Vcl.Forms,
  FOGComputeServerFrm in 'FOGComputeServerFrm.pas' {FOGComputeServerForm},
  ConnectManagerServerFrm in '..\ManagerServer\ConnectManagerServerFrm.pas' {ConnectManagerServerForm},
  CommonServiceDefine in '..\common\CommonServiceDefine.pas',
  opCode in 'opCode.pas',
  zExpression in 'zExpression.pas',
  FOGComputeClientIntf in 'FOGComputeClientIntf.pas',
  DataStoreClientIntf in '..\DataStoreServer\DataStoreClientIntf.pas';

{$R *.res}

begin
  //ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFOGComputeServerForm, FOGComputeServerForm);
  Application.Run;
end.
