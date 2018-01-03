program AFDRClient;

uses
  Vcl.Forms,
  AFDRCliFrm in 'AFDRCliFrm.pas' {DRClientForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDRClientForm, DRClientForm);
  Application.Run;
end.
