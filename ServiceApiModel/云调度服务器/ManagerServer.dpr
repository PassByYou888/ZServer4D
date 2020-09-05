program ManagerServer;

{$APPTYPE CONSOLE}

{$R *.res}

(*
  调度服务器
  支持运行平台：
  Windows：x86/x64 需要2012及以上服务器操作系统
  Linux：x64 建议使用Ubuntu16.04或以上版本操作系统（Linux操作系统不支持DIOCP和ICS接口，只能使用CrossSocket+Indy作为服务器接口）

  注意：
  调度服务器无需做二次开发，它本身就被设计成了一种标准系统
  调度服务器具有灾难自动恢复机制
  调度服务器具有云服务器群集支持能力
  调度服务器具有分布式部署能力，调度中心可以在阿里云，亚马逊，腾讯云分布式部署，良好抵御流量轰炸
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
  CommunicationFrameworkDoubleTunnelIO_ServMan,
  DataFrameEngine,
  UnicodeMixedLib,
  MemoryStream64,
  ManagerClientAPI in 'QueryAPI\ManagerClientAPI.pas';

var
  // 全局变量，绑定的IP，支持IPV6
  BIND_IP: string = '0.0.0.0';
  // 全局变量，查询服务器的侦听端口
  QUERY_PORT: string = '10888';
  // 全局变量，接收通道的端口号
  RECEIVE_PORT: string = '13336';
  // 全局变量，发送通道的端口号
  SEND_PORT: string = '13335';

type
  TManagerServer = class(TCoreClassInterfacedObject)
  public
    RecvService, SendService: TCommunicationFrameworkServer;
    AccessService: TCommunicationFrameworkServer;
    ManagerService: TServerManager;

    // 客户端的遍历查询api
    procedure Command_Query(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
    // 客户端的遍历查询api，返回最小负载
    procedure Command_QueryMinLoad(Sender: TPeerClient; InData, OutData: TDataFrameEngine);

    constructor Create;
    destructor Destroy; override;

    // 主循环api
    procedure Progress;

    // Service开关
    procedure StartService;
    procedure StopService;
  end;

  // 客户端的遍历查询api
procedure TManagerServer.Command_Query(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
var
  servType: TServerType;
  i: Integer;
  ns: TListString;
  vl: THashVariantList;
begin
  servType := TServerType(InData.Reader.ReadByte);

  ns := TListString.Create;
  ManagerService.ServerConfig.GetSectionList(ns);

  for i := 0 to ns.Count - 1 do
    begin
      vl := ManagerService.ServerConfig.VariantList[ns[i]];
      if servType = vl.GetDefaultValue('Type', TServerType.stUnknow) then
        begin
          OutData.WriteVariantList(vl);
        end;
    end;

  DisposeObject(ns);
end;

// 客户端的遍历查询api，返回最小负载
procedure TManagerServer.Command_QueryMinLoad(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
var
  servType: TServerType;
  i: Integer;
  ns: TListString;
  vl, mvl: THashVariantList;
begin
  servType := TServerType(InData.Reader.ReadByte);

  ns := TListString.Create;
  ManagerService.ServerConfig.GetSectionList(ns);

  mvl := nil;
  for i := 0 to ns.Count - 1 do
    begin
      vl := ManagerService.ServerConfig.VariantList[ns[i]];
      if servType = vl.GetDefaultValue('Type', TServerType.stUnknow) then
        begin
          if (mvl = nil) or (mvl['WorkLoad'] > vl['WorkLoad']) then
              mvl := vl;
        end;
    end;
  if mvl <> nil then
      OutData.WriteVariantList(mvl);

  DisposeObject(ns);
end;

constructor TManagerServer.Create;
begin
  RecvService := TCommunicationFramework_Server_CrossSocket.Create;
  RecvService.PrintParams['AntiIdle'] := False;
  // 如果登录进来的客户端1分钟无响应，就踢掉
  RecvService.IdleTimeout := 60 * 1000;

  SendService := TCommunicationFramework_Server_CrossSocket.Create;

  AccessService := TCommunicationFramework_Server_CrossSocket.Create;
  // access 客户端查询是5秒无响应自动断线
  AccessService.IdleTimeout := 5000;
  AccessService.RegisterStream('Query').OnExecute := Command_Query;
  AccessService.RegisterStream('QueryMinLoad').OnExecute := Command_QueryMinLoad;

  ManagerService := TServerManager.Create(RecvService, SendService, TCommunicationFramework_Client_CrossSocket);
  ManagerService.RegisterCommand;
end;

destructor TManagerServer.Destroy;
begin
  DisposeObject(ManagerService);
  DisposeObject(AccessService);
  DisposeObject(RecvService);
  DisposeObject(SendService);
  inherited;
end;

procedure TManagerServer.Progress;
begin
  ManagerService.Progress;
  AccessService.Progress;

  if RecvService.Count + SendService.Count + AccessService.Count > 0 then
      CoreClasses.CheckThreadSynchronize(1)
  else
      CoreClasses.CheckThreadSynchronize(100);
end;

procedure TManagerServer.StartService;
begin
  StopService;
  if AccessService.StartService(BIND_IP, umlStrToInt(QUERY_PORT, CDEFAULT_MANAGERSERVICE_QUERYPORT)) then
      DoStatus('Manager Access Service ready Ok! bind:%s port:%s', [TranslateBindAddr(BIND_IP), QUERY_PORT])
  else
      DoStatus('Manager Access Service Failed! bind:%s port:%s', [TranslateBindAddr(BIND_IP), QUERY_PORT]);

  if RecvService.StartService(BIND_IP, umlStrToInt(RECEIVE_PORT, DEFAULT_MANAGERSERVICE_RECVPORT)) then
      DoStatus('Receive tunnel ready Ok! bind:%s port:%s', [TranslateBindAddr(BIND_IP), RECEIVE_PORT])
  else
      DoStatus('Receive tunnel Failed! bind:%s port:%s', [TranslateBindAddr(BIND_IP), RECEIVE_PORT]);

  if SendService.StartService(BIND_IP, umlStrToInt(SEND_PORT, DEFAULT_MANAGERSERVICE_SENDPORT)) then
      DoStatus('Send tunnel ready Ok! bind:%s port:%s', [TranslateBindAddr(BIND_IP), SEND_PORT])
  else
      DoStatus('Send tunnel Failed! bind:%s port:%s', [TranslateBindAddr(BIND_IP), SEND_PORT]);;
end;

procedure TManagerServer.StopService;
begin
  try
    AccessService.StopService;
    RecvService.StopService;
    SendService.StopService;
  except
  end;
end;

var
  MServer: TManagerServer;

  // 延迟回调事件
procedure PostExecute_DelayStartService(Sender: TNPostExecute);
begin
  MServer.StartService;
end;

// 延迟回调事件
procedure PostExecute_DelayRegService(Sender: TNPostExecute);
  function AutoConnectManagerServer(AClients: TServerManager_ClientPool; ManServAddr, RegAddr: SystemString;
    RegRecvPort, RegSendPort: WORD; serverType: TServerType): Boolean;
  begin
    Result := AClients.BuildClientAndConnect(serverType2Str(serverType),
      ManServAddr, RegAddr,
      DEFAULT_MANAGERSERVICE_SENDPORT, DEFAULT_MANAGERSERVICE_RECVPORT,
      RegRecvPort, RegSendPort, serverType);
  end;

begin
  if AutoConnectManagerServer(MServer.ManagerService.ServManClientPool,
    Sender.Data3, Sender.Data4,
    umlStrToInt(SEND_PORT, DEFAULT_MANAGERSERVICE_SENDPORT),
    umlStrToInt(RECEIVE_PORT, DEFAULT_MANAGERSERVICE_RECVPORT), TServerType.stManager) then
    begin
      MServer.ManagerService.ServManClientPool.ServerConfig.Merge(MServer.ManagerService.ServerConfig);
    end;
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
  DoStatus('Default File Receive directory %s', [MServer.ManagerService.FileReceiveDirectory]);

  RECEIVE_PORT := IntToStr(DEFAULT_MANAGERSERVICE_RECVPORT);
  SEND_PORT := IntToStr(DEFAULT_MANAGERSERVICE_SENDPORT);
  QUERY_PORT := IntToStr(CDEFAULT_MANAGERSERVICE_QUERYPORT);

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

            if umlMultipleMatch(['Query:*', 'q:*', '-q:*', '-Query:*'], p1) then
              begin
                p2 := umlDeleteFirstStr(p1, ':');
                if umlIsNumber(p2) then
                    QUERY_PORT := p2;
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

            if umlMultipleMatch(['MServer:*', 'Manager:*', 'ManServ:*', 'ManServer:*',
              '-MServer:*', '-Manager:*', '-ManServ:*', '-ManServer:*'], p1) then
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
      with MServer.ManagerService.ProgressEngine.PostExecute(delayStartServiceTime) do
          OnExecuteCall := PostExecute_DelayStartService;
    end;

  if delayReg then
    with MServer.ManagerService.ProgressEngine.PostExecute(delayRegTime) do
      begin
        Data3 := ManServAddr;
        Data4 := RegAddr;
        OnExecuteCall := PostExecute_DelayRegService;
      end;

  DoStatus('');
end;

begin
  MServer := TManagerServer.Create;

  // fill parameter
  FillParameter;

  // maihloop
  while True do
      MServer.Progress; // green power progress on MServer

  MServer.StopService;
  DisposeObject(MServer);

end.
