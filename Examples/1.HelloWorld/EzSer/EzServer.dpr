program EzServer;

uses
  Vcl.Forms,
  EzServFrm in 'EzServFrm.pas' {EZServerForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TEZServerForm, EZServerForm);
  Application.Run;
end.
