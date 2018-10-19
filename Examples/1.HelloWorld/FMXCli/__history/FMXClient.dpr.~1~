program FMXClient;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMXCliFrm in 'FMXCliFrm.pas' {FMXClientForm};

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.Landscape, TFormOrientation.InvertedLandscape];
  Application.CreateForm(TFMXClientForm, FMXClientForm);
  Application.Run;
end.
