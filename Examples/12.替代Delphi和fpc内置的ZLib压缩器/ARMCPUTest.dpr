program ARMCPUTest;

uses
  System.StartUpCopy,
  FMX.Forms,
  ARMCPUTestFrm in 'ARMCPUTestFrm.pas' {Form1};

{$R *.res}


begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
