program ZSGatewayMiniClient;






{$R *.dres}

uses
  Vcl.Forms,
  System.SysUtils,
  zsGatewayMiniClientConfigureFrm in 'zsGatewayMiniClientConfigureFrm.pas' {zsGatewayMiniClientConfigureForm},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}


begin
  if FindProcessCount(ExtractFileName(Application.ExeName)) > 1 then
      exit;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Windows10 SlateGray');
  Application.CreateForm(TzsGatewayMiniClientConfigureForm, zsGatewayMiniClientConfigureForm);
  Application.Run;

end.
