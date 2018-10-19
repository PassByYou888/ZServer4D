program VMDoubleAuthFMXClient;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMXAuthDoubleTunnelCliFrm in 'FMXAuthDoubleTunnelCliFrm.pas' {FMXAuthDoubleClientForm};

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.Landscape, TFormOrientation.InvertedLandscape];
  Application.CreateForm(TFMXAuthDoubleClientForm, FMXAuthDoubleClientForm);
  Application.Run;

end.
