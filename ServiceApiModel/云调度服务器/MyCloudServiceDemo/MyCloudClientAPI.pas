unit MyCloudClientAPI;

interface

uses Variants, SysUtils, Types, DateUtils,
  CoreClasses, TextDataEngine, ListEngine, CommunicationFramework,
  DoStatusIO, UnicodeMixedLib, DataFrameEngine, Cadencer,
  NotifyObjectBase,
  CommunicationFramework_Client_Indy,
  PascalStrings, MemoryStream64,
  CommunicationFrameworkDoubleTunnelIO_VirtualAuth,
  CommunicationFrameworkDoubleTunnelIO_NoAuth,
  CommunicationFrameworkDoubleTunnelIO,
  CommunicationFrameworkDataStoreService_VirtualAuth,
  CommunicationFrameworkDataStoreService_NoAuth,
  CommunicationFrameworkDataStoreService;

type
  (*
    TMyCloudClient可以继承的客户端基础框架有6种，每种框架都要对应的服务器端框架
    TCommunicationFramework_DoubleTunnelClient_NoAuth: 无身份验证双通道框架
    TCommunicationFramework_DoubleTunnelClient: 有身份验证双通道框架
    TCommunicationFramework_DoubleTunnelClient_VirtualAuth: 有身份验证双通道框架，但是身份证验证需要二次开发
    TDataStoreClient_VirtualAuth: 有身份证验证的数据库框架，但是身份证验证模块需要二次开发
    TDataStoreClient_NoAuth: 无身份证验证的数据库框架
    TDataStoreClient: 有身份验证双通道数据库框架
  *)
  TMyCloudClient = class(TCommunicationFramework_DoubleTunnelClient_NoAuth)
  protected
  public
    NetRecvTunnelIntf, NetSendTunnelIntf: TCommunicationFrameworkClient;

    constructor Create(ClientClass: TCommunicationFrameworkClientClass);
    destructor Destroy; override;

    procedure RegisterCommand; override;
    procedure UnRegisterCommand; override;

    // 假如使用带有身份证验证的框架，在Connect最后一步需要做一次身份证验证才能进行TunnelLink操作，具体细节请到代码中去查看
    function Connect(addr: SystemString; const FogCliRecvPort, FogCliSendPort: Word): Boolean; override;

    procedure Disconnect; override;

    function MyAPI(a, b: Integer): Integer;
  end;

implementation

constructor TMyCloudClient.Create(ClientClass: TCommunicationFrameworkClientClass);
begin
  NetRecvTunnelIntf := ClientClass.Create;
  NetSendTunnelIntf := ClientClass.Create;
  NetSendTunnelIntf.PrintParams['AntiIdle'] := False;
  inherited Create(NetRecvTunnelIntf, NetSendTunnelIntf);
  RegisterCommand;
end;

destructor TMyCloudClient.Destroy;
begin
  UnRegisterCommand;
  Disconnect;
  DisposeObject(NetRecvTunnelIntf);
  DisposeObject(NetSendTunnelIntf);
  inherited Destroy;
end;

procedure TMyCloudClient.RegisterCommand;
begin
  inherited RegisterCommand;
end;

procedure TMyCloudClient.UnRegisterCommand;
begin
  inherited UnRegisterCommand;
end;

function TMyCloudClient.Connect(addr: SystemString; const FogCliRecvPort, FogCliSendPort: Word): Boolean;
var
  t: Cardinal;
begin
  Result := False;
  Disconnect;

  if not NetSendTunnelIntf.Connect(addr, FogCliSendPort) then
    begin
      DoStatus('connect %s failed!', [addr]);
      exit;
    end;
  if not NetRecvTunnelIntf.Connect(addr, FogCliRecvPort) then
    begin
      DoStatus('connect %s failed!', [addr]);
      exit;
    end;

  if not Connected then
      exit;

  t := TCoreClassThread.GetTickCount + 4000;
  while not RemoteInited do
    begin
      if TCoreClassThread.GetTickCount > t then
          break;
      if not Connected then
          break;
      Progress;
    end;

  if Connected then
    begin
      DoStatus('connect fog compute service "%s" ok!', [addr]);

      // 假如使用带有身份证验证的框架，这里需要做一次身份证验证才能进行TunnelLink操作
      // if UserLogin(UserID, Passwd) then
      // Result := TunnelLink;

      // 因为模型的演示使用NoAuth框架，我们直接做TunnelLink就行了
      Result := TunnelLink;
    end;
end;

procedure TMyCloudClient.Disconnect;
begin
  NetSendTunnelIntf.Disconnect;
  NetRecvTunnelIntf.Disconnect;
end;

function TMyCloudClient.MyAPI(a, b: Integer): Integer;
var
  SendDE, ResultDE: TDataFrameEngine;
begin
  Result := 0;
  if not Connected then
      exit;
  if not LinkOk then
      exit;

  SendDE := TDataFrameEngine.Create;
  ResultDE := TDataFrameEngine.Create;

  SendDE.WriteInteger(a);
  SendDE.WriteInteger(b);

  SendTunnel.WaitSendStreamCmd('MyAPI', SendDE, ResultDE, 5000);

  Result := ResultDE.Reader.ReadInteger;

  DisposeObject([SendDE, ResultDE]);
end;

end.
