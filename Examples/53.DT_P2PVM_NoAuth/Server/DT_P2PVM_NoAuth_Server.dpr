program DT_P2PVM_NoAuth_Server;

uses
  Vcl.Forms,
  DT_P2PVM_NoAuth_ServerFrm in 'DT_P2PVM_NoAuth_ServerFrm.pas' {DT_P2PVM_NoAuth_ServerForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDT_P2PVM_NoAuth_ServerForm, DT_P2PVM_NoAuth_ServerForm);
  Application.Run;
end.
