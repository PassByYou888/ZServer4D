program ZDBServiceDemo;

uses
  Vcl.Forms,
  DBStoreServiceFrm in 'DBStoreServiceFrm.pas' {DBStoreServiceForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDBStoreServiceForm, DBStoreServiceForm);
  Application.Run;
end.
