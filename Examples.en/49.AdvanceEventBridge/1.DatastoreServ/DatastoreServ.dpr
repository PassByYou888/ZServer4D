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
  {  ZDB has its own high-speed JSON mechanism, which will generate JSON instance cache during traversal, so as to avoid repeated load and accelerate query traversal  }
  {  In the demonstration, we use stream, load every time we traverse, and then judge the conditions, which is more in line with the principle of ZDB  }
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
  {  Bigstreammemoryswapspace: when the data sent by bigstream is tmemorystream or tmemorystream64, file transfer will be started to free memory  }
  {  This option is mainly used to deal with the sudden outbreak of data entry download requests: thousands of bigstream transmission tasks  }
  DatabaseSendTunnel.BigStreamMemorySwapSpace := True;
  {  Bigstreamswapspacetriggersize, which is the trigger parameter of file transfer condition. Tmemorystream must occupy 1024 * 1024 memory space to start file transfer  }
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
