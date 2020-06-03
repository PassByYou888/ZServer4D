program AsyncClient;

uses
  Vcl.Forms,
  asyncCliFrm in 'asyncCliFrm.pas' {AsyncClientForm};

{$R *.res}

begin
  System.ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TAsyncClientForm, AsyncClientForm);
  Application.Run;
end.
