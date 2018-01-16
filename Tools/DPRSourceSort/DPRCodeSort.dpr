program DPRCodeSort;

uses
  System.StartUpCopy,
  FMX.Forms,
  SortFrm in 'SortFrm.pas' {SortForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TSortForm, SortForm);
  Application.Run;
end.
