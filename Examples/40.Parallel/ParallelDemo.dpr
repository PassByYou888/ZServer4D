program ParallelDemo;

uses
  Vcl.Forms,
  ParallelFrm in 'ParallelFrm.pas' {ParallelForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TParallelForm, ParallelForm);
  Application.Run;
end.
