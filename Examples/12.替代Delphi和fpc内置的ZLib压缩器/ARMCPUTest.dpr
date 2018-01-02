program ARMCPUTest;

uses
  System.StartUpCopy,
  FMX.Forms,
  ARMCPUTestFrm in 'ARMCPUTestFrm.pas' {Form1},
  CoreClasses in '..\..\Source\CoreClasses.pas',
  CoreCompress in '..\..\Source\CoreCompress.pas',
  MemoryStream64 in '..\..\Source\MemoryStream64.pas',
  PascalStrings in '..\..\Source\PascalStrings.pas',
  UnicodeMixedLib in '..\..\Source\UnicodeMixedLib.pas',
  DoStatusIO in '..\..\Source\DoStatusIO.pas';

{$R *.res}


begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;

end.
