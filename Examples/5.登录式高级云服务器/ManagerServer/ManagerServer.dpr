program ManagerServer;

uses
  Vcl.Forms,
  ManagerServerFrm in 'ManagerServerFrm.pas' {ManagerServerForm},
  ManagerServer_ClientIntf in 'ManagerServer_ClientIntf.pas',
  ConnectManagerServerFrm in 'ConnectManagerServerFrm.pas' {ConnectManagerServerForm},
  Cadencer in '..\..\..\Source\Cadencer.pas',
  CommunicationFramework_Client_Indy in '..\..\..\Source\Indy\CommunicationFramework_Client_Indy.pas',
  CommunicationFramework_Server_Indy in '..\..\..\Source\Indy\CommunicationFramework_Server_Indy.pas',
  CommunicationFramework_Client_ICS in '..\..\..\Source\ICS\CommunicationFramework_Client_ICS.pas',
  CommunicationFramework_Server_ICS in '..\..\..\Source\ICS\CommunicationFramework_Server_ICS.pas',
  CommunicationFramework_Server_ICSCustomSocket in '..\..\..\Source\ICS\CommunicationFramework_Server_ICSCustomSocket.pas',
  CommunicationFramework_Client_CrossSocket in '..\..\..\Source\CrossSocket\CommunicationFramework_Client_CrossSocket.pas',
  CommunicationFramework_Server_CrossSocket in '..\..\..\Source\CrossSocket\CommunicationFramework_Server_CrossSocket.pas',
  CommunicationFramework in '..\..\..\Source\CommunicationFramework.pas',
  CommunicationFrameworkDoubleTunnelIO in '..\..\..\Source\CommunicationFrameworkDoubleTunnelIO.pas',
  CommunicationFrameworkDoubleTunnelIO_NoAuth in '..\..\..\Source\CommunicationFrameworkDoubleTunnelIO_NoAuth.pas',
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
  CoreCompress in '..\..\..\Source\CoreCompress.pas',
  MH_ZDB in '..\..\..\Source\MH_ZDB.pas';


{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TManagerServerForm, ManagerServerForm);
  Application.Run;
end.
