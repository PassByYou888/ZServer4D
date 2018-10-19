program DRClient;

uses
  Vcl.Forms,
  DRCliFrm in 'DRCliFrm.pas' {DRClientForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDRClientForm, DRClientForm);
  Application.Run;
end.
