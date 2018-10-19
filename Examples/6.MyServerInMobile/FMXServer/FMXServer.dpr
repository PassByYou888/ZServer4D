program FMXServer;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMXServFrm in 'FMXServFrm.pas' {FMXClientForm};


{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.Landscape, TFormOrientation.InvertedLandscape];
  Application.CreateForm(TFMXClientForm, FMXClientForm);
  Application.Run;
end.
