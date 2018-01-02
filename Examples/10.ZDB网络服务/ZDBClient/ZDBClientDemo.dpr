program ZDBClientDemo;

uses
  Vcl.Forms,
  dbclientfrm in 'dbclientfrm.pas' {Form1},
  Cadencer in '..\..\..\Source\Cadencer.pas',
  CommunicationFramework in '..\..\..\Source\CommunicationFramework.pas',
  CommunicationFrameworkDataStoreService in '..\..\..\Source\CommunicationFrameworkDataStoreService.pas',
  CommunicationFrameworkDataStoreService_NoAuth in '..\..\..\Source\CommunicationFrameworkDataStoreService_NoAuth.pas',
  CommunicationFrameworkDataStoreService_VirtualAuth in '..\..\..\Source\CommunicationFrameworkDataStoreService_VirtualAuth.pas',
  CommunicationFrameworkDataStoreServiceCommon in '..\..\..\Source\CommunicationFrameworkDataStoreServiceCommon.pas',
  CommunicationFrameworkDoubleTunnelIO in '..\..\..\Source\CommunicationFrameworkDoubleTunnelIO.pas',
  CommunicationFrameworkDoubleTunnelIO_NoAuth in '..\..\..\Source\CommunicationFrameworkDoubleTunnelIO_NoAuth.pas',
  CommunicationFrameworkDoubleTunnelIO_ServMan in '..\..\..\Source\CommunicationFrameworkDoubleTunnelIO_ServMan.pas',
  CommunicationFrameworkDoubleTunnelIO_VirtualAuth in '..\..\..\Source\CommunicationFrameworkDoubleTunnelIO_VirtualAuth.pas',
  CommunicationFrameworkIO in '..\..\..\Source\CommunicationFrameworkIO.pas',
  CommunicationTest in '..\..\..\Source\CommunicationTest.pas',
  CoreCipher in '..\..\..\Source\CoreCipher.pas',
  CoreClasses in '..\..\..\Source\CoreClasses.pas',
  CoreCompress in '..\..\..\Source\CoreCompress.pas',
  DataFrameEngine in '..\..\..\Source\DataFrameEngine.pas',
  DBCompressPackageForFile in '..\..\..\Source\DBCompressPackageForFile.pas',
  DoStatusIO in '..\..\..\Source\DoStatusIO.pas',
  Geometry2DUnit in '..\..\..\Source\Geometry2DUnit.pas',
  Geometry3DUnit in '..\..\..\Source\Geometry3DUnit.pas',
  GeometryLib in '..\..\..\Source\GeometryLib.pas',
  GeometryRotationUnit in '..\..\..\Source\GeometryRotationUnit.pas',
  ItemStream in '..\..\..\Source\ItemStream.pas',
  JsonDataObjects in '..\..\..\Source\JsonDataObjects.pas',
  LibraryManager in '..\..\..\Source\LibraryManager.pas',
  ListEngine in '..\..\..\Source\ListEngine.pas',
  MemoryStream64 in '..\..\..\Source\MemoryStream64.pas',
  MH in '..\..\..\Source\MH.pas',
  MH_1 in '..\..\..\Source\MH_1.pas',
  MH_2 in '..\..\..\Source\MH_2.pas',
  MH_3 in '..\..\..\Source\MH_3.pas',
  MH_ZDB in '..\..\..\Source\MH_ZDB.pas',
  NotifyObjectBase in '..\..\..\Source\NotifyObjectBase.pas',
  ObjectData in '..\..\..\Source\ObjectData.pas',
  ObjectDataManager in '..\..\..\Source\ObjectDataManager.pas',
  Optimize.Move.Win32 in '..\..\..\Source\Optimize.Move.Win32.pas',
  PascalStrings in '..\..\..\Source\PascalStrings.pas',
  StreamList in '..\..\..\Source\StreamList.pas',
  TextDataEngine in '..\..\..\Source\TextDataEngine.pas',
  TextParsing in '..\..\..\Source\TextParsing.pas',
  TextTable in '..\..\..\Source\TextTable.pas',
  UnicodeMixedLib in '..\..\..\Source\UnicodeMixedLib.pas',
  ZDBEngine in '..\..\..\Source\ZDBEngine.pas',
  ZDBLocalManager in '..\..\..\Source\ZDBLocalManager.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
