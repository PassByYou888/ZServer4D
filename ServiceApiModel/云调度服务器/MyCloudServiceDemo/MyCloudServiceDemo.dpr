program MyCloudServiceDemo;

{$APPTYPE CONSOLE}

{$R *.res}

(*
  云服务器标准群集模型
  支持运行平台：
  Windows：x86/x64 需要2012及以上服务器操作系统
  Linux：x64 建议使用Ubuntu16.04或以上版本操作系统（Linux操作系统不支持DIOCP和ICS接口，只能使用CrossSocket+Indy作为服务器接口）

  注意：
  这是云服务器做二次开发的标准模型
  MyServer命名请用替换功能将它替换成自定义名字，以便区分
  在二次开发完成服务器后，需要封装一套客户端的API协议库，否则你的工程将来会难以维护
  与调度服务器断线后会自动尝试重连，灾难自动恢复
*)

uses
  System.SysUtils,
  System.Classes,
  System.Variants,
  PascalStrings,
  CoreClasses,
  DoStatusIO,
  ListEngine,
  NotifyObjectBase,
  TextDataEngine,
  CommunicationFramework,
  CommunicationFramework_Server_CrossSocket,
  CommunicationFramework_Client_CrossSocket,
  CommunicationFrameworkDoubleTunnelIO_VirtualAuth,
  CommunicationFrameworkDoubleTunnelIO_NoAuth,
  CommunicationFrameworkDoubleTunnelIO,
  CommunicationFrameworkDataStoreService_VirtualAuth,
  CommunicationFrameworkDataStoreService_NoAuth,
  CommunicationFrameworkDataStoreService,
  CommunicationFrameworkDoubleTunnelIO_ServMan,
  DataFrameEngine,
  UnicodeMixedLib,
  MemoryStream64,
  MyCloudClientAPI in 'MyCloudClientAPI.pas';

var
  // 云服务器端口，双通道（自定义）
  DEFAULT_MYSERVICE_RECVPORT: WORD = 11505;
  DEFAULT_MYSERVICE_SENDPORT: WORD = 11506;

  // 云服务器类型（自定义）
  MY_SERVERTYPE: TServerType = TServerType.stLogic;

  // 全局变量，绑定的IP，支持IPV6
  BIND_IP: string;
  // 全局变量，接收通道的端口号
  RECEIVE_PORT: string;
  // 全局变量，发送通道的端口号
  SEND_PORT: string;

type
  (*
    TMyServer可以继承的服务器框架有6种，每种框架都要对应的客户端框架，做完服务器开发，需要自己封装一套与之对应的客户端
    TCommunicationFramework_DoubleTunnelService_NoAuth: 无身份验证双通道框架，适用于服务器通讯，也适用于客户端访问，注意安全
    TCommunicationFramework_DoubleTunnelService: 有身份验证双通道框架，适用于客户端登录
    TCommunicationFramework_DoubleTunnelService_VirtualAuth: 有身份验证双通道框架，但是身份证验证需要二次开发
    TDataStoreService_VirtualAuth: 有身份证验证的数据库框架，但是身份证验证模块需要二次开发
    TDataStoreService_NoAuth: 无身份证验证的数据库框架，适用于服务器通讯，也适用于客户端访问，注意安全
    TDataStoreService: 有身份验证双通道数据库框架，适用于客户端登录
  *)
  TMyServer = class(TCommunicationFramework_DoubleTunnelService_NoAuth, IServerManager_ClientPoolNotify)
  public
    RecvService, SendService: TCommunicationFrameworkServer;
    ManagerClient: TServerManager_ClientPool;
    AntiTimeTick: Double;

    constructor Create;
    destructor Destroy; override;

    // 主循环api
    procedure Progress; override;
    procedure CadencerProgress(Sender: TObject; const deltaTime, newTime: Double); override;

    // 注册命令
    procedure Command_MyAPI(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
    procedure RegisterCommand; override;
    procedure UnRegisterCommand; override;

    // Service开关
    procedure StartService;
    procedure StopService;

    // 远程ManagerServer别的服务器注册或离线时触发回调接口
    procedure ServerConfigChange(Sender: TServerManager_Client; ConfigData: TSectionTextData);
    procedure ServerOffline(Sender: TServerManager_Client; RegAddr: SystemString; ServerType: TServerType);
  end;

constructor TMyServer.Create;
begin
  RecvService := TCommunicationFramework_Server_CrossSocket.Create;
  RecvService.PrintParams['AntiIdle'] := False;

  SendService := TCommunicationFramework_Server_CrossSocket.Create;

  ManagerClient := TServerManager_ClientPool.Create(TCommunicationFramework_Client_CrossSocket, Self);
  inherited Create(RecvService, SendService);
end;

destructor TMyServer.Destroy;
begin
  DisposeObject(RecvService);
  DisposeObject(SendService);
  DisposeObject(ManagerClient);
  inherited;
end;

procedure TMyServer.Progress;
begin
  ManagerClient.Progress;
  inherited Progress;

  // 绿色节能机制
  if RecvService.Count + SendService.Count + ManagerClient.Count > 0 then
      CoreClasses.CheckThreadSynchronize(1)
  else
      CoreClasses.CheckThreadSynchronize(100);
end;

procedure TMyServer.CadencerProgress(Sender: TObject; const deltaTime, newTime: Double);
begin
  inherited CadencerProgress(Sender, deltaTime, newTime);

  // 每隔5秒往调度中心发送一条负载信息
  AntiTimeTick := AntiTimeTick + deltaTime;
  if AntiTimeTick > 5.0 then
    begin
      AntiTimeTick := 0;
      // 这里的负载信息是真实的连接数
      // 负载信息也可以是cpu的占用率，内存的占用率，带宽的占用率，细节请自行实现
      // 调度中心会把最小的负载服务器地址告诉客户端，从而实现分布式负载
      // 当服务器宕机，调度中心会自动从调度表中删除本台服务器的信息，从而加强后台稳定性
      ManagerClient.AntiIdle(RecvTunnel.Count + SendTunnel.Count);
    end;
end;

// 注册命令
procedure TMyServer.Command_MyAPI(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
begin
  OutData.WriteInteger(InData.Reader.ReadInteger + InData.Reader.ReadInteger);
end;

procedure TMyServer.RegisterCommand;
begin
  inherited RegisterCommand;
  // 在此处注册自己的命令
  RecvTunnel.RegisterStream('MyAPI').OnExecute := Command_MyAPI;
end;

procedure TMyServer.UnRegisterCommand;
begin
  inherited UnRegisterCommand;
  // 在此处删除自己的注册命令
  RecvTunnel.DeleteRegistedCMD('MyAPI');
end;

procedure TMyServer.StartService;
begin
  StopService;
  if RecvService.StartService(BIND_IP, umlStrToInt(RECEIVE_PORT, DEFAULT_MYSERVICE_RECVPORT)) then
      DoStatus('Receive tunnel ready Ok! bind:%s port:%s', [TranslateBindAddr(BIND_IP), RECEIVE_PORT])
  else
      DoStatus('Receive tunnel Failed! bind:%s port:%s', [TranslateBindAddr(BIND_IP), RECEIVE_PORT]);

  if SendService.StartService(BIND_IP, umlStrToInt(SEND_PORT, DEFAULT_MYSERVICE_SENDPORT)) then
      DoStatus('Send tunnel ready Ok! bind:%s port:%s', [TranslateBindAddr(BIND_IP), SEND_PORT])
  else
      DoStatus('Send tunnel Failed! bind:%s port:%s', [TranslateBindAddr(BIND_IP), SEND_PORT]);;

  RegisterCommand;
  AntiTimeTick := 0;
end;

procedure TMyServer.StopService;
begin
  try
    RecvService.StopService;
    SendService.StopService;
  except
  end;
  UnRegisterCommand;
end;

// 远程ManagerServer别的服务器注册或离线时触发回调接口
procedure TMyServer.ServerConfigChange(Sender: TServerManager_Client; ConfigData: TSectionTextData);
begin
end;

procedure TMyServer.ServerOffline(Sender: TServerManager_Client; RegAddr: SystemString; ServerType: TServerType);
begin
end;

// 全局的实例变量
var
  Server: TMyServer;

  // 延迟开启端口侦听
procedure PostExecute_DelayStartService(Sender: TNPostExecute);
begin
  Server.StartService;
end;

// 延迟向调度中心注册
procedure PostExecute_DelayRegService(Sender: TNPostExecute);
  procedure AutoConnectManagerServer(AClients: TServerManager_ClientPool; ManServAddr, RegAddr: SystemString;
    RegRecvPort, RegSendPort: WORD; ServerType: TServerType);
  begin
    AClients.BuildClientAndConnect(serverType2Str(ServerType),
      ManServAddr, RegAddr,
      DEFAULT_MANAGERSERVICE_SENDPORT, DEFAULT_MANAGERSERVICE_RECVPORT,
      RegRecvPort, RegSendPort, ServerType);
  end;

begin
  AutoConnectManagerServer(Server.ManagerClient,
    Sender.Data3, Sender.Data4,
    umlStrToInt(SEND_PORT, DEFAULT_MYSERVICE_SENDPORT),
    umlStrToInt(RECEIVE_PORT, DEFAULT_MYSERVICE_RECVPORT), MY_SERVERTYPE);
end;

// 处理启动参数
procedure FillParameter;
var
  i, pcount: Integer;
  p1, p2: SystemString;

  delayStartService: Boolean;
  delayStartServiceTime: Double;

  delayReg: Boolean;
  delayRegTime: Double;
  ManServAddr: SystemString;
  RegAddr: SystemString;
begin
  RECEIVE_PORT := IntToStr(DEFAULT_MYSERVICE_RECVPORT);
  SEND_PORT := IntToStr(DEFAULT_MYSERVICE_SENDPORT);

  delayStartService := True;
  delayStartServiceTime := 0.1;

  delayRegTime := 1.0;

  try
    pcount := ParamCount;
    for i := 1 to pcount do
      begin
        p1 := ParamStr(i);
        if p1 <> '' then
          begin
            if umlMultipleMatch(['Recv:*', 'r:*', 'Receive:*', '-r:*', '-recv:*', '-receive:*'], p1) then
              begin
                p2 := umlDeleteFirstStr(p1, ':');
                if umlIsNumber(p2) then
                    RECEIVE_PORT := p2;
              end;

            if umlMultipleMatch(['Send:*', 's:*', '-s:*', '-Send:*'], p1) then
              begin
                p2 := umlDeleteFirstStr(p1, ':');
                if umlIsNumber(p2) then
                    SEND_PORT := p2;
              end;

            if umlMultipleMatch(['ipv6', '-6', '-ipv6', '-v6'], p1) then
              begin
                BIND_IP := '::';
              end;

            if umlMultipleMatch(['ipv4', '-4', '-ipv4', '-v4'], p1) then
              begin
                BIND_IP := '0.0.0.0';
              end;

            if umlMultipleMatch(['ipv4+ipv6', '-4+6', '-ipv4+ipv6', '-v4+v6', 'ipv6+ipv4', '-ipv6+ipv4', '-6+4', '-v6+v4'], p1) then
              begin
                BIND_IP := '';
              end;

            if umlMultipleMatch(['DelayStart:*', 'DelayService:*', '-DelayStart:*', '-DelayService:*'], p1) then
              begin
                delayStartService := True;
                p2 := umlDeleteFirstStr(p1, ':');
                if umlIsNumber(p2) then
                    delayStartServiceTime := umlStrToFloat(p2, 1);
              end;

            if umlMultipleMatch(['DelayStart', 'DelayService', 'AutoStart', 'AutoService',
              '-DelayStart', '-DelayService', '-AutoStart', '-AutoService'], p1) then
              begin
                delayStartService := True;
                delayStartServiceTime := 1.0;
              end;

            if umlMultipleMatch(['Server:*', 'Manager:*', 'ManServ:*', 'ManServer:*',
              '-Server:*', '-Manager:*', '-ManServ:*', '-ManServer:*'], p1) then
              begin
                ManServAddr := umlTrimSpace(umlDeleteFirstStr(p1, ':'));
              end;

            if umlMultipleMatch(['RegAddress:*', 'RegistedAddress:*', 'RegAddr:*', 'RegistedAddr:*',
              '-RegAddress:*', '-RegistedAddress:*', '-RegAddr:*', '-RegistedAddr:*'], p1) then
              begin
                delayReg := True;
                RegAddr := umlTrimSpace(umlDeleteFirstStr(p1, ':'));
              end;

            if umlMultipleMatch(['DelayRegManager:*', 'DelayReg:*', 'DelayRegisted:*', 'DelayRegMan:*',
              '-DelayRegManager:*', '-DelayReg:*', '-DelayRegisted:*', '-DelayRegMan:*'], p1) then
              begin
                delayReg := True;
                p2 := umlDeleteFirstStr(p1, ':');
                if umlIsNumber(p2) then
                    delayRegTime := umlStrToFloat(p2, 1);
              end;
          end;
      end;
  except
  end;

  if delayStartService then
    begin
      with Server.ProgressEngine.PostExecute(delayStartServiceTime) do
          OnExecuteCall := PostExecute_DelayStartService;
    end;

  if delayReg then
    with Server.ProgressEngine.PostExecute(delayRegTime) do
      begin
        Data3 := ManServAddr;
        Data4 := RegAddr;
        OnExecuteCall := PostExecute_DelayRegService;
      end;

  DoStatus('');
end;

begin
  Server := TMyServer.Create;

  // 处理启动参数
  FillParameter;

  // 主循环
  while True do
      Server.Progress; // 这里有绿色节能机制

  Server.StopService;
  DisposeObject(Server);

end.
