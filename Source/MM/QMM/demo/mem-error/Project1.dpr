program Project1;

uses
  QMM in '..\..\QMM.pas',
  QMMErrorUtils in '..\..\QMMErrorUtils.pas',
  Forms,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
