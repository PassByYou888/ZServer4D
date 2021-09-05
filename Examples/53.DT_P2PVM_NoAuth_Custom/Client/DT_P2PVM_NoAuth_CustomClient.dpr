program DT_P2PVM_NoAuth_CustomClient;

uses
  Vcl.Forms,
  DT_P2PVM_NoAuth_CustomClientFrm in 'DT_P2PVM_NoAuth_CustomClientFrm.pas' {DT_P2PVM_NoAuth_ClientForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDT_P2PVM_NoAuth_ClientForm, DT_P2PVM_NoAuth_ClientForm);
  Application.Run;
end.
