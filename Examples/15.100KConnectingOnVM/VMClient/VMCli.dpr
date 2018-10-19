program VMCli;

uses
  Vcl.Forms,
  VMCliFrm in 'VMCliFrm.pas' {VMCliForm};

{$R *.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TVMCliForm, VMCliForm);
  Application.Run;

end.
