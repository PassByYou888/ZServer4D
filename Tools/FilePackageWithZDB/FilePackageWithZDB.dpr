program FilePackageWithZDB;

uses
  Vcl.Forms,
  BuildIndexPackageOptFrm in 'BuildIndexPackageOptFrm.pas' {BuildIndexPackageOptForm},
  FilePackageWithZDBMainFrm in 'FilePackageWithZDBMainFrm.pas' {FilePackageWithZDBMainForm},
  NewDBOptFrm in 'NewDBOptFrm.pas' {NewDBOptForm},
  ObjectDataManagerFrameUnit in 'ObjectDataManagerFrameUnit.pas' {ObjectDataManagerFrame: TFrame},
  ObjectDataTreeFrameUnit in 'ObjectDataTreeFrameUnit.pas' {ObjectDataTreeFrame: TFrame};

{$R *.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFilePackageWithZDBMainForm, FilePackageWithZDBMainForm);
  Application.CreateForm(TBuildIndexPackageOptForm, BuildIndexPackageOptForm);
  Application.CreateForm(TNewDBOptForm, NewDBOptForm);
  Application.Run;
end.
