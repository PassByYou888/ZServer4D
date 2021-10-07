program DT_P2PVM_VirtualAuth_CustomServer;

uses
  Vcl.Forms,
  DT_P2PVM_VirtualAuth_CustomServerFrm in 'DT_P2PVM_VirtualAuth_CustomServerFrm.pas' {DT_P2PVM_VirtualAuth_ServerForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDT_P2PVM_VirtualAuth_ServerForm, DT_P2PVM_VirtualAuth_ServerForm);
  Application.Run;
end.
