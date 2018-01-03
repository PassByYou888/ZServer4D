program FMXCloudClient;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMXCloudClientFrm in 'FMXCloudClientFrm.pas' {FMXCloudClientForm},
  ManagerServerQueryClient in 'ManagerServerQueryClient.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFMXCloudClientForm, FMXCloudClientForm);
  Application.Run;
end.
