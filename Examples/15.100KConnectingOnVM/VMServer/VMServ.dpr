program VMServ;

uses
  Vcl.Forms,
  VMServFrm in 'VMServFrm.pas' {VMServForm};

{$R *.res}

begin
  System.ReportMemoryLeaksOnShutdown:=True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TVMServForm, VMServForm);
  Application.Run;
end.
