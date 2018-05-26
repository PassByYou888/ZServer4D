program HPC_Server;

uses
  Vcl.Forms,
  HPCServFrm in 'HPCServFrm.pas' {DoubleServerForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  AApplication.CreateForm(TDoubleServerForm, DoubleServerForm);
  pplication.Run;
end.
