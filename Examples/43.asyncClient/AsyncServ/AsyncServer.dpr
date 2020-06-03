program AsyncServer;

uses
  Vcl.Forms,
  AsyncServFrm in 'AsyncServFrm.pas' {AsyncServerForm};

{$R *.res}


begin
  System.ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TAsyncServerForm, AsyncServerForm);
  Application.Run;

end.
