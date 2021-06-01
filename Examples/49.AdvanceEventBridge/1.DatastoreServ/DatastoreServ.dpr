program DatastoreServ;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,
  ZS_JsonDataObjects,
  CoreClasses,
  PascalStrings,
  DoStatusIO,
  UnicodeMixedLib,
  DataFrameEngine,
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

type
  TMyDataStoreService = class(TDataStoreService_NoAuth)
  public
    constructor Create(RecvTunnel_, SendTunnel_: TCommunicationFrameworkServer); override;
    destructor Destroy; override;
    procedure cmd_MyJsonQuery(dPipe: TZDBPipeline; var qState: TQueryState; var Allowed: Boolean);
  end;

constructor TMyDataStoreService.Create(RecvTunnel_, SendTunnel_: TCommunicationFrameworkServer);
begin
  inherited Create(RecvTunnel_, SendTunnel_);
  RegisterQueryCall('MyJsonQuery').OnPipelineQuery := cmd_MyJsonQuery;
end;

destructor TMyDataStoreService.Destroy;
begin
  inherited Destroy;
end;

procedure TMyDataStoreService.cmd_MyJsonQuery(dPipe: TZDBPipeline; var qState: TQueryState; var Allowed: Boolean);
var
  js: TJsonObject;
begin
  Allowed := False;
  if qState.ID <> 1 then
      exit;
  // ZDB自带高速json的机制在遍历会产生json的实例cache，从而避免了反复load，达到加速查询遍历的作用
  // 处于演示，这里我们使用stream，每次遍历都要Load，然后判断条件，这样更符合演示ZDB的原理
  js := TJsonObject.Create;
  try
    Allowed := True;
    js.LoadFromStream(qState.Cache, TEncoding.UTF8);
    if dPipe.Values.Exists('Name') then
        Allowed := Allowed and umlSearchMatch(dPipe.Values.S['Name'], js.S['Name']);
    if dPipe.Values.Exists('Phone') then
        Allowed := Allowed and umlSearchMatch(dPipe.Values.S['Phone'], js.S['Phone']);
    if dPipe.Values.Exists('Comment') then
        Allowed := Allowed and umlSearchMatch(dPipe.Values.S['Comment'], js.S['Comment']);
  except
      Allowed := False;
  end;
  js.Free;
end;

var
  DatabasePhyServ: TPhysicsServer;
  DatabaseRecvTunnel, DatabaseSendTunnel: TCommunicationFrameworkWithP2PVM_Server;
  DatabaseService: TMyDataStoreService;

procedure InitServ;
begin
  DatabaseRecvTunnel := TCommunicationFrameworkWithP2PVM_Server.Create;
  DatabaseRecvTunnel.QuietMode := IsQuiet;

  DatabaseSendTunnel := TCommunicationFrameworkWithP2PVM_Server.Create;
  DatabaseSendTunnel.QuietMode := IsQuiet;
  // BigStreamMemorySwapSpace，当BigStream发送的数据为TMemoryStream or TMemoryStream64，会启动文件转存，从而达到释放内存的作用
  // 该选项主要用于应对突然爆发的数据条目下载请求：数以千计的BigStream传输任务
  DatabaseSendTunnel.BigStreamMemorySwapSpace := True;
  // BigStreamSwapSpaceTriggerSize，这是文件转存条件触发参数，TMemoryStream的内存占用空间必须达到1024*1024才会启动文件转存
  DatabaseSendTunnel.BigStreamSwapSpaceTriggerSize := 1024 * 1024;

  DatabaseService := TMyDataStoreService.Create(DatabaseRecvTunnel, DatabaseSendTunnel);
  DatabaseService.ZDBLocal.RootPath := umlCombinePath(ZDBLocalManager_SystemRootPath, 'Database');
  umlCreateDirectory(DatabaseService.ZDBLocal.RootPath);
  DatabaseService.RegisterCommand;
  DoStatus('ZDB V1.0 Root directory %s', [DatabaseService.ZDBLocal.RootPath]);

  DatabasePhyServ := TPhysicsServer.Create;
  DatabasePhyServ.QuietMode := IsQuiet;
  DatabasePhyServ.AutomatedP2PVMServiceBind.AddService(DatabaseRecvTunnel, '::', 1);
  DatabasePhyServ.AutomatedP2PVMServiceBind.AddService(DatabaseSendTunnel, '::', 2);
  DatabasePhyServ.AutomatedP2PVMAuthToken := 'datastore';
  if DatabasePhyServ.StartService('', 9238) then
      DoStatus('database service listening port 9238 successed.')
  else
      DoStatus('database service open port 9238 failed.');
end;

procedure DoMainLoop;
begin
  while True do
    begin
      DatabaseService.Progress;
      DatabasePhyServ.Progress;
      CheckThreadSynchronize();
      TCompute.Sleep(10);
    end;
end;

begin
  InitServ();
  DoMainLoop();

end.
