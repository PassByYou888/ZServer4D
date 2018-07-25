program ADRestoreClient;

uses
  Vcl.Forms,
  ADRestoreCliFrm in 'ADRestoreCliFrm.pas' {AuthDoubleTunnelClientForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TAuthDoubleTunnelClientForm, AuthDoubleTunnelClientForm);
  Application.Run;
end.
