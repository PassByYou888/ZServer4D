program ZDBBatchDataService;

uses
  Vcl.Forms,
  ZDBBatchDataServiceFrm in 'ZDBBatchDataServiceFrm.pas' {ZDBBatchDataServiceForm},
  Cadencer in '..\..\..\Source\Cadencer.pas',
  CommunicationFramework in '..\..\..\Source\CommunicationFramework.pas',
  CommunicationFrameworkDataStoreService_NoAuth in '..\..\..\Source\CommunicationFrameworkDataStoreService_NoAuth.pas',
  CommunicationFrameworkDataStoreServiceCommon in '..\..\..\Source\CommunicationFrameworkDataStoreServiceCommon.pas',
  CommunicationFrameworkDoubleTunnelIO in '..\..\..\Source\CommunicationFrameworkDoubleTunnelIO.pas',
  CommunicationFrameworkDoubleTunnelIO_NoAuth in '..\..\..\Source\CommunicationFrameworkDoubleTunnelIO_NoAuth.pas',
  CommunicationFrameworkDoubleTunnelIO_ServMan in '..\..\..\Source\CommunicationFrameworkDoubleTunnelIO_ServMan.pas',
  CommunicationFrameworkIO in '..\..\..\Source\CommunicationFrameworkIO.pas',
  CommunicationTest in '..\..\..\Source\CommunicationTest.pas',
  CoreCipher in '..\..\..\Source\CoreCipher.pas',
  CoreClasses in '..\..\..\Source\CoreClasses.pas',
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
  NotifyObjectBase in '..\..\..\Source\NotifyObjectBase.pas',
  ObjectData in '..\..\..\Source\ObjectData.pas',
  ObjectDataManager in '..\..\..\Source\ObjectDataManager.pas',
  PascalStrings in '..\..\..\Source\PascalStrings.pas',
  StreamList in '..\..\..\Source\StreamList.pas',
  TextDataEngine in '..\..\..\Source\TextDataEngine.pas',
  TextParsing in '..\..\..\Source\TextParsing.pas',
  TextTable in '..\..\..\Source\TextTable.pas',
  UnicodeMixedLib in '..\..\..\Source\UnicodeMixedLib.pas',
  ZDBEngine in '..\..\..\Source\ZDBEngine.pas',
  ZDBLocalManager in '..\..\..\Source\ZDBLocalManager.pas',
  CommunicationFramework_Client_CrossSocket in '..\..\..\Source\CrossSocket\CommunicationFramework_Client_CrossSocket.pas',
  CommunicationFramework_Server_CrossSocket in '..\..\..\Source\CrossSocket\CommunicationFramework_Server_CrossSocket.pas',
  CommunicationFramework_Client_ICS in '..\..\..\Source\ICS\CommunicationFramework_Client_ICS.pas',
  CommunicationFramework_Server_ICS in '..\..\..\Source\ICS\CommunicationFramework_Server_ICS.pas',
  CommunicationFramework_Server_ICSCustomSocket in '..\..\..\Source\ICS\CommunicationFramework_Server_ICSCustomSocket.pas',
  CommunicationFramework_Client_Indy in '..\..\..\Source\Indy\CommunicationFramework_Client_Indy.pas',
  CommunicationFramework_Server_Indy in '..\..\..\Source\Indy\CommunicationFramework_Server_Indy.pas',
  CommunicationFrameworkDataStoreService in '..\..\..\Source\CommunicationFrameworkDataStoreService.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TZDBBatchDataServiceForm, ZDBBatchDataServiceForm);
  Application.Run;
end.
