program DServer;

uses
  Vcl.Forms,
  DServFrm in 'DServFrm.pas' {DoubleServerForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDoubleServerForm, DoubleServerForm);
  Application.Run;
end.
