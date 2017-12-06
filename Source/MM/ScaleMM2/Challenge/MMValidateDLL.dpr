program MMValidateDLL;

uses
  Forms,
  MainDLLValidation in 'MainDLLValidation.pas' {Form1},
  GeneralFunctions in 'GeneralFunctions.pas',
  SystemInfoUnit in 'SystemInfoUnit.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
