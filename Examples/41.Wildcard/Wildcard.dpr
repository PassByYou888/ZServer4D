program Wildcard;

uses
  Vcl.Forms,
  WildcardFrm in 'WildcardFrm.pas' {WildcardForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TWildcardForm, WildcardForm);
  Application.Run;
end.
