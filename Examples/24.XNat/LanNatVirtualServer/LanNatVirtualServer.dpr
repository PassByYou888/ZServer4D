program LanNatVirtualServer;

uses
  System.StartUpCopy,
  FMX.Forms,
  VirtualServFrm in 'VirtualServFrm.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
