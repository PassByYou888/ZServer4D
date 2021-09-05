program DT_P2PVM_VirtualAuth_Server;

uses
  Vcl.Forms,
  DT_P2PVM_VirtualAuth_ServerFrm in 'DT_P2PVM_VirtualAuth_ServerFrm.pas' {DT_P2PVM_VirtualAuth_ServerForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDT_P2PVM_VirtualAuth_ServerForm, DT_P2PVM_VirtualAuth_ServerForm);
  Application.Run;
end.
