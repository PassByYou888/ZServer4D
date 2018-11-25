program XNATMobileServer;

uses
  System.StartUpCopy,
  FMX.Forms,
  XNATMobileDeviceFrm in 'XNATMobileDeviceFrm.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
