program ClientToLinuxServer;

uses
  Vcl.Forms,
  EzCliFrm in 'EzCliFrm.pas' {EZClientForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TEZClientForm, EZClientForm);
  Application.Run;
end.
