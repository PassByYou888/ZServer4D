program delphiTest;

uses
  jemalloc4p in '..\Source\jemalloc4p.pas',
  Vcl.Forms,
  delphiTestFrm in 'delphiTestFrm.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
