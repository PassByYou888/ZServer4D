program CustomStableIOClientDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  CustomStableIOClientOnFMXMainFrm in 'CustomStableIOClientOnFMXMainFrm.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
