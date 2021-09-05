program DT_P2PVM_Custom_Server;

uses
  Vcl.Forms,
  DT_P2PVM_CustomServerFrm in 'DT_P2PVM_CustomServerFrm.pas' {DT_P2PVM_CustomServerForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDT_P2PVM_CustomServerForm, DT_P2PVM_CustomServerForm);
  Application.Run;
end.
