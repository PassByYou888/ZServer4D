program IOSPaymentQueryServer;

uses
  Vcl.Forms,
  IOSPaymentQueryServerFrm in 'IOSPaymentQueryServerFrm.pas' {IOSPaymentQueryServerForm},
  ConnectManagerServerFrm in '..\ManagerServer\ConnectManagerServerFrm.pas' {ConnectManagerServerForm},
  CommonServiceDefine in '..\common\CommonServiceDefine.pas';

{$R *.res}

begin
  //ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TIOSPaymentQueryServerForm, IOSPaymentQueryServerForm);
  Application.Run;
end.
