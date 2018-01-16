program DMDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  DMDemoFrm in 'DMDemoFrm.pas' {DMDemoForm},
  UserDataModuleUnit in 'UserDataModuleUnit.pas',
  NumberBase in '..\..\Source\NumberBase.pas';

{$R *.res}


begin
  System.ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TDMDemoForm, DMDemoForm);
  Application.Run;

end.
