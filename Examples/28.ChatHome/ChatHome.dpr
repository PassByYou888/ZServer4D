program ChatHome;

uses
  System.StartUpCopy,
  FMX.Forms,
  chatHomeFrm in 'chatHomeFrm.pas' {Form3};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
