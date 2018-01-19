program FilePackageWithZDB;

uses
  Vcl.Forms,
  FilePackageWithZDBMainFrm in 'FilePackageWithZDBMainFrm.pas' {FilePackageWithZDBMainForm},
  ObjectDataManagerFrameUnit in 'ObjectDataManagerFrameUnit.pas' {ObjectDataManagerFrame: TFrame},
  ObjectDataTreeFrameUnit in 'ObjectDataTreeFrameUnit.pas' {ObjectDataTreeFrame: TFrame},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}


begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Windows10 SlateGray');
  Application.CreateForm(TFilePackageWithZDBMainForm, FilePackageWithZDBMainForm);
  Application.Run;

end.
