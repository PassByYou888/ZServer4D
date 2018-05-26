program MyCloudClientDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  MyCloudClientFrm in 'MyCloudClientFrm.pas' {Form1},
  MyCloudClientAPI in '..\MyCloudServiceDemo\MyCloudClientAPI.pas',
  ManagerClientAPI in '..\QueryAPI\ManagerClientAPI.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
