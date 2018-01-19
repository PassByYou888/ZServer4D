program ACFMXClient;





uses
  System.StartUpCopy,
  FMX.Forms,
  AC2LogicFrm in 'AC2LogicFrm.pas' {AC2LogicForm},
  AC2LoginFrm in 'AC2LoginFrm.pas' {AC2LoginForm},
  AC2ClientGlobal in 'AC2ClientGlobal.pas',
  AC2LogicMobileClient in 'AC2LogicMobileClient.pas',
  AC2ManagerServerMobileClient in 'AC2ManagerServerMobileClient.pas',
  AC2KeepAwakeUnit in 'AC2KeepAwakeUnit.pas',
  AC2ProgressFrm in 'AC2ProgressFrm.pas' {AC2ProgressForm},
  CommonServiceDefine in '..\Common\CommonServiceDefine.pas',
  FOGComputeClientIntf in '..\FOGComputeServer\FOGComputeClientIntf.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.Landscape, TFormOrientation.InvertedLandscape];
  Application.CreateForm(TAC2LoginForm, AC2LoginForm);
  InitGlobalResource;
  Application.Run;
end.


