program ZSGatewayMiniServ;




{$R *.dres}

uses
  Vcl.Forms,
  System.SysUtils,
  zsGatewayMiniServConfigureFrm in 'zsGatewayMiniServConfigureFrm.pas' {zsGatewayMiniServConfigureForm},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}


begin
  if FindProcessCount(ExtractFileName(Application.ExeName)) > 1 then
      exit;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Windows10 SlateGray');
  Application.CreateForm(TzsGatewayMiniServConfigureForm, zsGatewayMiniServConfigureForm);
  Application.Run;

end.
