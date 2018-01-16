program FilePackageWithZDB;

uses
  Vcl.Forms,
  FilePackageWithZDBMainFrm in 'FilePackageWithZDBMainFrm.pas' {FilePackageWithZDBMainForm},
  ObjectDataManagerFrameUnit in 'ObjectDataManagerFrameUnit.pas' {ObjectDataManagerFrame: TFrame},
  ObjectDataTreeFrameUnit in 'ObjectDataTreeFrameUnit.pas' {ObjectDataTreeFrame: TFrame};

{$R *.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFilePackageWithZDBMainForm, FilePackageWithZDBMainForm);
  Application.Run;

end.
