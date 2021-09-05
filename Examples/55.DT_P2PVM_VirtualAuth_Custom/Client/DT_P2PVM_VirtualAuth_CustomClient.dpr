program DT_P2PVM_VirtualAuth_CustomClient;

uses
  Vcl.Forms,
  DT_P2PVM_VirtualAuth_CustomClientFrm in 'DT_P2PVM_VirtualAuth_CustomClientFrm.pas' {DT_P2PVM_VirtualAuth_ClientForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDT_P2PVM_VirtualAuth_ClientForm, DT_P2PVM_VirtualAuth_ClientForm);
  Application.Run;
end.
