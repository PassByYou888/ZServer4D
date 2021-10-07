program DT_P2PVM_Client;

uses
  Vcl.Forms,
  DT_P2PVM_ClientFrm in 'DT_P2PVM_ClientFrm.pas' {DT_P2PVM_ClientForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDT_P2PVM_ClientForm, DT_P2PVM_ClientForm);
  Application.Run;
end.
