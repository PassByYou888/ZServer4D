program FMXBatchPictureDataClient;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMXBatchDataClientFrm in 'FMXBatchDataClientFrm.pas' {FMXBatchDataClientForm};

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.Landscape, TFormOrientation.InvertedLandscape];
  Application.CreateForm(TFMXBatchDataClientForm, FMXBatchDataClientForm);
  Application.Run;
end.
