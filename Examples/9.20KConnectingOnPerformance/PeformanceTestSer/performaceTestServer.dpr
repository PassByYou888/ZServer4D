program performaceTestServer;

uses
  Vcl.Forms,
  PeformanceTestServFrm in 'PeformanceTestServFrm.pas' {EZServerForm};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TEZServerForm, EZServerForm);
  Application.Run;
end.
