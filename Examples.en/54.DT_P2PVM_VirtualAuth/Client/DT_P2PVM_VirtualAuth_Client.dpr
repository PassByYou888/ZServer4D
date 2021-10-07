program DT_P2PVM_VirtualAuth_Client;

uses
  Vcl.Forms,
  DT_P2PVM_VirtualAuth_ClientFrm in 'DT_P2PVM_VirtualAuth_ClientFrm.pas' {DT_P2PVM_VirtualAuth_ClientForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDT_P2PVM_VirtualAuth_ClientForm, DT_P2PVM_VirtualAuth_ClientForm);
  Application.Run;
end.
