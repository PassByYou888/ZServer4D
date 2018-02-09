program ZSTranslateClient;

uses
  Vcl.Forms,
  ZSClientFrm in 'ZSClientFrm.pas' {ZSClientForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TZSClientForm, ZSClientForm);
  Application.Run;
end.
