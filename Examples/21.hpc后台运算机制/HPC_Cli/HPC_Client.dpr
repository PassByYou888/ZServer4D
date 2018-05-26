program HPC_Client;

uses
  Vcl.Forms,
  HPC_CliFrm in 'HPC_CliFrm.pas' {DoubleTunnelClientForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  AApplication.CreateForm(TDoubleTunnelClientForm, DoubleTunnelClientForm);
  pplication.Run;
end.
