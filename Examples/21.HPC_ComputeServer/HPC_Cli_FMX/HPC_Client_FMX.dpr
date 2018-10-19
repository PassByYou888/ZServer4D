program HPC_Client_FMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  HPC_FMXCliFrm in 'HPC_FMXCliFrm.pas' {FMXDoubleClientForm};

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.Landscape, TFormOrientation.InvertedLandscape];
  Application.CreateForm(TFMXDoubleClientForm, FMXDoubleClientForm);
  Application.Run;
end.
