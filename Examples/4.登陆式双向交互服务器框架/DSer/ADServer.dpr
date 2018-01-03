program ADServer;

uses
  Vcl.Forms,
  ADServFrm in 'ADServFrm.pas' {AuthDoubleServerForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TAuthDoubleServerForm, AuthDoubleServerForm);
  Application.Run;
end.
