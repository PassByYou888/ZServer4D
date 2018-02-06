program StringTranslate;

uses
  Vcl.Forms,
  StringTranslateFrm in 'StringTranslateFrm.pas' {StringTranslateForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TStringTranslateForm, StringTranslateForm);
  Application.Run;
end.
