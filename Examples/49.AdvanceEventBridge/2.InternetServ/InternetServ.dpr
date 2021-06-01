program InternetServ;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  ZS_JsonDataObjects,
  CoreClasses,
  PascalStrings,
  DoStatusIO,
  UnicodeMixedLib,
  MemoryStream64,
  ListEngine,
  DataFrameEngine,
  NotifyObjectBase,
  CommunicationFramework,
  PhysicsIO,
  CommunicationFrameworkDoubleTunnelIO_NoAuth,
  ZDBLocalManager,
  ZDBEngine,
  ObjectDataManager,
  ObjectData,
  CommunicationFrameworkDataStoreService_NoAuth;

const
  IsQuiet: Boolean = False;
  DatastoreServiceHost: SystemString = '127.0.0.1';

var
  DatabasePhyCli: TPhysicsClient;
  DatabaseRecvTunnel, DatabaseSendTunnel: TCommunicationFrameworkWithP2PVM_Client;
  DatabaseClient: TDataStoreClient_NoAuth;
  DatabaseConnecting: Boolean;

  InternetPhyServ: TPhysicsServer;
  InternetRecvTunnel, InternetSendTunnel: TCommunicationFrameworkWithP2PVM_Server;
  InternetService: TCommunicationFramework_DoubleTunnelService_NoAuth;

procedure InitDatabaseCli;
begin
  DatabaseRecvTunnel := TCommunicationFrameworkWithP2PVM_Client.Create;
  DatabaseRecvTunnel.QuietMode := IsQuiet;

  DatabaseSendTunnel := TCommunicationFrameworkWithP2PVM_Client.Create;
  DatabaseSendTunnel.QuietMode := IsQuiet;

  DatabaseClient := TDataStoreClient_NoAuth.Create(DatabaseRecvTunnel, DatabaseSendTunnel);
  DatabaseClient.RegisterCommand;

  DatabasePhyCli := TPhysicsClient.Create;
  DatabasePhyCli.AutomatedP2PVMClientBind.AddClient(DatabaseSendTunnel, '::', 1);
  DatabasePhyCli.AutomatedP2PVMClientBind.AddClient(DatabaseRecvTunnel, '::', 2);
  DatabasePhyCli.AutomatedP2PVMAuthToken := 'datastore';

  DatabaseConnecting := False;
end;

{$REGION 'p2pVM自动化的异步连接握手事件，很多时候它都用匿名函数回调'}


procedure ConnectToDatabaseService_TunnelStateEvent(const cState: Boolean);
begin
  if cState then
      DoStatus('connection for Database %s done.', [DatastoreServiceHost]);
  DatabaseConnecting := False;
end;

procedure ConnectToDatabasePhysicsService_DoneEvent(Sender: TCommunicationFramework; P_IO: TPeerIO);
begin
  // 当全部p2pVM的连接完成后，绑定两个通道
  DatabaseClient.TunnelLinkC(ConnectToDatabaseService_TunnelStateEvent);
end;

procedure ConnectToDatabasePhysicsService_StateEvent(const cState: Boolean);
begin
  if not cState then
      DatabaseConnecting := False;
end;

procedure DoDelayConnectToDatabaseService;
begin
  // 该事件是在p2pVM全部连接完成后触发
  DatabasePhyCli.OnAutomatedP2PVMClientConnectionDone_C := ConnectToDatabasePhysicsService_DoneEvent;
  // 该事件是在物理连接成功或失败时触发
  DatabasePhyCli.AsyncConnectC(DatastoreServiceHost, 9238, ConnectToDatabasePhysicsService_StateEvent);
end;

procedure InitOrCheckConnectToDatabaseService;
begin
  if DatabaseConnecting then
      exit;
  if DatabasePhyCli.RemoteInited then
      exit;
  DatabaseConnecting := True;
  ProgressCadencer.PostExecuteC_NP(2, DoDelayConnectToDatabaseService);
end;
{$ENDREGION 'p2pVM自动化的异步连接握手事件，很多时候它都用匿名函数回调'}
{$REGION '异步事件桥'}


procedure cmd_MyAutoSave(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
var
  dbName: U_String;
  dataID: Cardinal;
  stream: TMS64;
  Bridge: TStreamEventBridge;
begin
  if not DatabaseClient.LinkOk then
      exit;
  if not DatabasePhyCli.Connected then
      exit;
  dbName := InData.R.ReadString;
  dataID := InData.R.ReadCardinal;
  stream := TMS64.Create;
  InData.R.ReadStream(stream);
  DatabaseClient.BeginAssembleStream;
  DatabaseClient.PostAssembleStream(dbName, stream, dataID, True);
  // TStreamEventBridge是通过异步事件，把Datastore服务器反馈的数据直接转发给客户端
  // TStreamEventBridge的实现原理为延迟反馈，细节技术去看文档和相关demo
  Bridge := TStreamEventBridge.Create(Sender, True);
  DatabaseClient.GetPostAssembleStreamStateM(Bridge.DoStreamEvent);
  DatabaseClient.EndAssembleStream;
end;

procedure cmd_MyBeginSave(Sender: TPeerIO; InData: TDataFrameEngine);
begin
  if not DatabaseClient.LinkOk then
      exit;
  if not DatabasePhyCli.Connected then
      exit;
  DatabaseClient.BeginAssembleStream;
end;

procedure cmd_MySave(Sender: TPeerIO; InData: TDataFrameEngine);
var
  dbName: U_String;
  dataID: Cardinal;
  stream: TMS64;
begin
  if not DatabaseClient.LinkOk then
      exit;
  if not DatabasePhyCli.Connected then
      exit;
  dbName := InData.R.ReadString;
  dataID := InData.R.ReadCardinal;
  stream := TMS64.Create;
  InData.R.ReadStream(stream);
  DatabaseClient.PostAssembleStream(dbName, stream, dataID, True);
end;

procedure cmd_MySaveState(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
var
  Bridge: TStreamEventBridge;
begin
  if not DatabaseClient.LinkOk then
      exit;
  if not DatabasePhyCli.Connected then
      exit;
  Bridge := TStreamEventBridge.Create(Sender, True);
  DatabaseClient.GetPostAssembleStreamStateM(Bridge.DoStreamEvent);
end;

procedure cmd_MyEndSave(Sender: TPeerIO; InData: TDataFrameEngine);
begin
  if not DatabaseClient.LinkOk then
      exit;
  if not DatabasePhyCli.Connected then
      exit;
  DatabaseClient.EndAssembleStream;
end;

type
  // 使用异步事件桥接口来自DatastoreService查询
  TMyQueryBridge = class(TCustomEventBridge)
  public
    procedure DoFillQueryData(dataBaseName_, pipeN: SystemString; StorePos: Int64; ID: Cardinal; DataSour: TMemoryStream64);
    procedure DoQueryDone(dataBaseName_, outN, pipeN: SystemString; TotalResult: Int64);
  end;

procedure TMyQueryBridge.DoFillQueryData(dataBaseName_, pipeN: SystemString; StorePos: Int64; ID: Cardinal; DataSour: TMemoryStream64);
var
  js: TJsonObject;
begin
  if CheckIO then
    begin
      try
        js := TJsonObject.Create;
        DataSour.Position := 0;
        js.LoadFromStream(DataSour);
        js.I64['Pos'] := StorePos;
        IO.OutDataFrame.WriteJson(js);
        js.Free;
      except
      end;
    end;
end;

procedure TMyQueryBridge.DoQueryDone(dataBaseName_, outN, pipeN: SystemString; TotalResult: Int64);
begin
  if CheckIO then
      IO.ContinueResultSend;
  Free;
end;

procedure cmd_MyQuery(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
var
  dbName: U_String;
  Reverse: Boolean;
  MaxResult: Int64;
  vl: THashVariantList;
  Bridge: TMyQueryBridge;
begin
  Sender.PauseResultSend;

  dbName := InData.Reader.ReadString;
  Reverse := InData.Reader.ReadBool;
  MaxResult := InData.Reader.ReadInt64;
  vl := THashVariantList.Create;
  InData.Reader.ReadVariantList(vl);

  Bridge := TMyQueryBridge.Create(Sender);
  DatabaseClient.QueryDBM('MyJsonQuery', True, False, True, Reverse, dbName, '', 1.0, 0, MaxResult, vl,
    Bridge.DoFillQueryData, Bridge.DoQueryDone);

  vl.Free;
end;

type
  // 使用异步事件桥接口来自DatastoreService下载
  TMyDownloadBridge = class(TCustomEventBridge)
  public
    procedure DoDownloadDone(dataBaseName_: SystemString; dStorePos: Int64; stream: TMemoryStream64);
  end;

procedure TMyDownloadBridge.DoDownloadDone(dataBaseName_: SystemString; dStorePos: Int64; stream: TMemoryStream64);
begin
  if CheckIO then
    begin
      IO.OutDataFrame.WriteStream(stream);
      IO.ContinueResultSend;
    end;
  Free;
end;

procedure cmd_MyDownload(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
var
  dbName: U_String;
  StorePos: Int64;
  Bridge: TMyDownloadBridge;
begin
  Sender.PauseResultSend;

  dbName := InData.Reader.ReadString;
  StorePos := InData.Reader.ReadInt64;

  Bridge := TMyDownloadBridge.Create(Sender);
  DatabaseClient.DownloadAssembleStreamM(dbName, StorePos, True, Bridge.DoDownloadDone);
end;

procedure cmd_MyDelete(Sender: TPeerIO; InData: TDataFrameEngine);
var
  dbName: U_String;
  StorePos: Int64;
begin
  dbName := InData.Reader.ReadString;
  StorePos := InData.Reader.ReadInt64;
  DatabaseClient.DeleteData(dbName, StorePos);
end;

{$ENDREGION '异步事件桥'}


procedure InitInternetServ;
begin
  InternetRecvTunnel := TCommunicationFrameworkWithP2PVM_Server.Create;
  InternetRecvTunnel.QuietMode := IsQuiet;

  // 数据条目存储
  InternetRecvTunnel.RegisterStream('MyAutoSave').OnExecuteCall := cmd_MyAutoSave;

  // 批量数据条目存储
  InternetRecvTunnel.RegisterDirectStream('MyBeginSave').OnExecuteCall := cmd_MyBeginSave;
  InternetRecvTunnel.RegisterDirectStream('MySave').OnExecuteCall := cmd_MySave;
  InternetRecvTunnel.RegisterStream('MySaveState').OnExecuteCall := cmd_MySaveState;
  InternetRecvTunnel.RegisterDirectStream('MyEndSave').OnExecuteCall := cmd_MyEndSave;

  // 查询条目
  InternetRecvTunnel.RegisterStream('MyQuery').OnExecuteCall := cmd_MyQuery;

  // 指定条目下载
  InternetRecvTunnel.RegisterStream('MyDownload').OnExecuteCall := cmd_MyDownload;

  // 删除条目
  InternetRecvTunnel.RegisterDirectStream('MyDelete').OnExecuteCall := cmd_MyDelete;

  InternetSendTunnel := TCommunicationFrameworkWithP2PVM_Server.Create;
  InternetSendTunnel.SendDataCompressed := True;
  InternetSendTunnel.QuietMode := IsQuiet;

  InternetService := TCommunicationFramework_DoubleTunnelService_NoAuth.Create(InternetRecvTunnel, InternetSendTunnel);
  InternetService.RegisterCommand;

  InternetPhyServ := TPhysicsServer.Create;
  InternetPhyServ.QuietMode := IsQuiet;
  InternetPhyServ.AutomatedP2PVMServiceBind.AddService(InternetRecvTunnel, '::', 111);
  InternetPhyServ.AutomatedP2PVMServiceBind.AddService(InternetSendTunnel, '::', 112);
  InternetPhyServ.AutomatedP2PVMAuthToken := 'internet';
  if InternetPhyServ.StartService('', 11088) then
      DoStatus('internet service listening port 11088 successed.')
  else
      DoStatus('internet service open port 11088 failed.');
end;

procedure DoMainLoop;
begin
  while True do
    begin
      DatabasePhyCli.Progress;
      DatabaseClient.Progress;

      InternetPhyServ.Progress;
      InternetService.Progress;

      // 断线重连机制
      InitOrCheckConnectToDatabaseService;

      TCompute.Sleep(1);
    end;
end;

begin
  InitDatabaseCli;
  InitInternetServ;
  DoMainLoop;

end.
