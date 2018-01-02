program ZDBLocal;

uses
  Vcl.Forms,
  ZDBmanagerFrm in 'ZDBmanagerFrm.pas' {ZDBmanagerForm},
  Cadencer in '..\..\Source\Cadencer.pas',
  CoreCipher in '..\..\Source\CoreCipher.pas',
  CoreClasses in '..\..\Source\CoreClasses.pas',
  DataFrameEngine in '..\..\Source\DataFrameEngine.pas',
  DBCompressPackageForFile in '..\..\Source\DBCompressPackageForFile.pas',
  DoStatusIO in '..\..\Source\DoStatusIO.pas',
  Geometry2DUnit in '..\..\Source\Geometry2DUnit.pas',
  Geometry3DUnit in '..\..\Source\Geometry3DUnit.pas',
  GeometryLib in '..\..\Source\GeometryLib.pas',
  GeometryRotationUnit in '..\..\Source\GeometryRotationUnit.pas',
  ItemStream in '..\..\Source\ItemStream.pas',
  JsonDataObjects in '..\..\Source\JsonDataObjects.pas',
  LibraryManager in '..\..\Source\LibraryManager.pas',
  ListEngine in '..\..\Source\ListEngine.pas',
  MemoryStream64 in '..\..\Source\MemoryStream64.pas',
  NotifyObjectBase in '..\..\Source\NotifyObjectBase.pas',
  ObjectData in '..\..\Source\ObjectData.pas',
  ObjectDataManager in '..\..\Source\ObjectDataManager.pas',
  PascalStrings in '..\..\Source\PascalStrings.pas',
  StreamList in '..\..\Source\StreamList.pas',
  TextDataEngine in '..\..\Source\TextDataEngine.pas',
  TextParsing in '..\..\Source\TextParsing.pas',
  TextTable in '..\..\Source\TextTable.pas',
  UnicodeMixedLib in '..\..\Source\UnicodeMixedLib.pas',
  ZDBEngine in '..\..\Source\ZDBEngine.pas',
  ZDBLocalManager in '..\..\Source\ZDBLocalManager.pas',
  CoreCompress in '..\..\Source\CoreCompress.pas',
  MH_ZDB in '..\..\Source\MH_ZDB.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TZDBmanagerForm, ZDBmanagerForm);
  Application.Run;
end.
