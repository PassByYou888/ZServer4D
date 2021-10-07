program DT_P2PVM_Server;

uses
  Vcl.Forms,
  DT_P2PVM_ServerFrm in 'DT_P2PVM_ServerFrm.pas' {DT_P2PVM_ServerForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDT_P2PVM_ServerForm, DT_P2PVM_ServerForm);
  Application.Run;
end.
