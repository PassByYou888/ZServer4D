program VirtualAuth_Client;

uses
  Vcl.Forms,
  VACliFrm in 'VACliFrm.pas' {AuthDoubleTunnelClientForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TAuthDoubleTunnelClientForm, AuthDoubleTunnelClientForm);
  Application.Run;
end.
