program HPC_Server;

uses
  Vcl.Forms,
  HPCServFrm in 'HPCServFrm.pas' {DoubleServerForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDoubleServerForm, DoubleServerForm);
  Application.Run;
end.
