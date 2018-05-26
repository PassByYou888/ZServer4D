program ManagerServer;

uses
  Vcl.Forms,
  ManagerServerFrm in 'ManagerServerFrm.pas' {ManagerServerForm},
  ConnectManagerServerFrm in 'ConnectManagerServerFrm.pas' {ConnectManagerServerForm},
  CommonServiceDefine in '..\common\CommonServiceDefine.pas';

{$R *.res}

begin
//  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TManagerServerForm, ManagerServerForm);
  Application.Run;
end.
