program AFDRServer;

uses
  Vcl.Forms,
  AFDRServFrm in 'AFDRServFrm.pas' {DRServerForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDRServerForm, DRServerForm);
  Application.Run;
end.
