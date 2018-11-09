program DatasetSimOnFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  DatasetOnFMXMainFrm in 'DatasetOnFMXMainFrm.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
