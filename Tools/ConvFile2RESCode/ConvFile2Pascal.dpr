program ConvFile2Pascal;

uses
  Vcl.Forms,
  ConvFileFrm in 'ConvFileFrm.pas' {ConvFileForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TConvFileForm, ConvFileForm);
  Application.Run;
end.
