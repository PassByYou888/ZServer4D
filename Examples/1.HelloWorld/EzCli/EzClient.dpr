program EzClient;

uses
  Vcl.Forms,
  EzCliFrm in 'EzCliFrm.pas' {EZClientForm};

{$R *.res}

begin
  System.ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TEZClientForm, EZClientForm);
  Application.Run;
end.
