program XLanMobile;

uses
  System.StartUpCopy,
  FMX.Forms,
  MobileFrm in 'MobileFrm.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
