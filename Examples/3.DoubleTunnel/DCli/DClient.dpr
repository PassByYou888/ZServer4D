program DClient;

uses
  Vcl.Forms,
  DCliFrm in 'DCliFrm.pas' {DoubleTunnelClientForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDoubleTunnelClientForm, DoubleTunnelClientForm);
  Application.Run;
end.
