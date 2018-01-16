program ZSGateway;

{$R *.dres}


uses
  Vcl.Forms,
  System.SysUtils,
  zsGatewayConfigureFrm in 'zsGatewayConfigureFrm.pas' {zsGatewayConfigureForm},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}


begin
  if FindProcessCount(ExtractFileName(Application.ExeName)) > 1 then
      exit;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Windows10 Dark');
  Application.CreateForm(TzsGatewayConfigureForm, zsGatewayConfigureForm);
  Application.Run;

end.
