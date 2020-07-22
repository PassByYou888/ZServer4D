program CopyPtrSpeed;

uses
  Vcl.Forms,
  CopyPtrSpeedFrm in 'CopyPtrSpeedFrm.pas' {CopyPtrSpeedForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TCopyPtrSpeedForm, CopyPtrSpeedForm);
  Application.Run;
end.
