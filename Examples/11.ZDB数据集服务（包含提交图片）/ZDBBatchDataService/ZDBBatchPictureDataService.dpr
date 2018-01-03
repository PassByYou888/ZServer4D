program ZDBBatchPictureDataService;

uses
  Vcl.Forms,
  ZDBBatchDataServiceFrm in 'ZDBBatchDataServiceFrm.pas' {ZDBBatchDataServiceForm};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TZDBBatchDataServiceForm, ZDBBatchDataServiceForm);
  Application.Run;
end.
