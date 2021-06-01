program InternetCli;

uses
  Vcl.Forms,
  InternetCliFrm in 'InternetCliFrm.pas' {InternetCliForm},
  CardFrm in 'CardFrm.pas' {CardForm},
  QueryFrm in 'QueryFrm.pas' {QueryForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TInternetCliForm, InternetCliForm);
  Application.CreateForm(TCardForm, CardForm);
  Application.CreateForm(TQueryForm, QueryForm);
  Application.Run;
end.
