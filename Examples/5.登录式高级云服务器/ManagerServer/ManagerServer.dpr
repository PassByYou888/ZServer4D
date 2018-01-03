program ManagerServer;

uses
  Vcl.Forms,
  ManagerServerFrm in 'ManagerServerFrm.pas' {ManagerServerForm};


{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TManagerServerForm, ManagerServerForm);
  Application.Run;
end.
