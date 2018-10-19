program ZDBLocal;

uses
  Vcl.Forms,
  ZDBmanagerFrm in 'ZDBmanagerFrm.pas' {ZDBmanagerForm};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TZDBmanagerForm, ZDBmanagerForm);
  Application.Run;
end.
