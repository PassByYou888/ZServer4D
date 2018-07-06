program PascalCodeUnification;

uses
  Vcl.Forms,
  PascalCodeUnificationFrm in 'PascalCodeUnificationFrm.pas' {PascalCodeUnificationForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TPascalCodeUnificationForm, PascalCodeUnificationForm);
  Application.Run;
end.
