program FMXDRClient;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMXDRCliFrm in 'FMXDRCliFrm.pas' {FMXDRClientForm};

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.Landscape, TFormOrientation.InvertedLandscape];
  Application.CreateForm(TFMXDRClientForm, FMXDRClientForm);
  Application.Run;
end.
