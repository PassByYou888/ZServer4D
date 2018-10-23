program StreamClientOnFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  StreamDemoFrm in 'StreamDemoFrm.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
