program ZSGatewayMultiClient;

{$R *.dres}

uses
  Vcl.Forms,
  ZSGatewayMultiClientConfigureFrm in 'ZSGatewayMultiClientConfigureFrm.pas' {ZSGatewayMultiClientConfigureForm},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Windows10 Dark');
  Application.CreateForm(TZSGatewayMultiClientConfigureForm, ZSGatewayMultiClientConfigureForm);
  Application.Run;
end.
