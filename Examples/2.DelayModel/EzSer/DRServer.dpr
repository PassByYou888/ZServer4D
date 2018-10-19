program DRServer;

uses
  Vcl.Forms,
  DRServFrm in 'DRServFrm.pas' {DRServerForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDRServerForm, DRServerForm);
  Application.Run;
end.
