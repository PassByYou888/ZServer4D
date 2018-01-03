program MHDemo;

uses
  Vcl.Forms,
  MHMainFrm in 'MHMainFrm.pas' {MHMainForm};

{$R *.res}


begin
  System.ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMHMainForm, MHMainForm);
  Application.Run;

end.
