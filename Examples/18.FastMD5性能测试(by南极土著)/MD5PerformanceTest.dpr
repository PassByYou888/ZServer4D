program MD5PerformanceTest;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  Fast_MD5 in 'Fast_MD5.pas',
  qdigest in 'qdigest.pas',
  FlyUtils.CnMD5 in 'FlyUtils.CnMD5.pas',
  FlyUtils.CnXXX.Common in 'FlyUtils.CnXXX.Common.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
