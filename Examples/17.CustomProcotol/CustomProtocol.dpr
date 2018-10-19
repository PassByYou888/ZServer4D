program CustomProtocol;

uses
  Vcl.Forms,
  CustomProtocolFrm in 'CustomProtocolFrm.pas' {CustomProtocolForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TCustomProtocolForm, CustomProtocolForm);
  Application.Run;
end.
