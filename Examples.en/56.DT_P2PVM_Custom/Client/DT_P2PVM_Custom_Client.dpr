program DT_P2PVM_Custom_Client;

uses
  Vcl.Forms,
  DT_P2PVM_CustomClientFrm in 'DT_P2PVM_CustomClientFrm.pas' {DT_P2PVM_CustomClientForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDT_P2PVM_CustomClientForm, DT_P2PVM_CustomClientForm);
  Application.Run;
end.
