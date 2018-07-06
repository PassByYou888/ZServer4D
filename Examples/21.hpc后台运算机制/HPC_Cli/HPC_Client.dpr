program HPC_Client;

uses
  Vcl.Forms,
  HPC_CliFrm in 'HPC_CliFrm.pas' {DoubleTunnelClientForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDoubleTunnelClientForm, DoubleTunnelClientForm);
  Application.Run;
end.
