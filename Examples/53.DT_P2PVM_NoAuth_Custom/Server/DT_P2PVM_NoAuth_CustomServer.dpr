program DT_P2PVM_NoAuth_CustomServer;

uses
  Vcl.Forms,
  DT_P2PVM_NoAuth_CustomServerFrm in 'DT_P2PVM_NoAuth_CustomServerFrm.pas' {DT_P2PVM_NoAuth_ServerForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDT_P2PVM_NoAuth_ServerForm, DT_P2PVM_NoAuth_ServerForm);
  Application.Run;
end.
