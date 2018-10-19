program FMXDClient;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMXDoubleTunnelCliFrm in 'FMXDoubleTunnelCliFrm.pas' {FMXDoubleClientForm};

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.Landscape, TFormOrientation.InvertedLandscape];
  Application.CreateForm(TFMXDoubleClientForm, FMXDoubleClientForm);
  Application.Run;
end.
