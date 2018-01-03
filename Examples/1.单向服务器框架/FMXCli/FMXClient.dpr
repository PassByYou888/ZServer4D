program FMXClient;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMXCliFrm in 'FMXCliFrm.pas' {FMXClientForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFMXClientForm, FMXClientForm);
  Application.Run;
end.
