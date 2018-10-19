program VMDoubleAuthClient;

uses
  Vcl.Forms,
  DCliFrm in 'DCliFrm.pas' {AuthDoubleTunnelClientForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TAuthDoubleTunnelClientForm, AuthDoubleTunnelClientForm);
  Application.Run;
end.
