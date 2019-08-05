program zExpressionSupport;

uses
  System.StartUpCopy,
  FMX.Forms,
  zExpressionSupportMainFrm in 'zExpressionSupportMainFrm.pas' {zExpressionSupportMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TzExpressionSupportMainForm, zExpressionSupportMainForm);
  Application.Run;
end.
