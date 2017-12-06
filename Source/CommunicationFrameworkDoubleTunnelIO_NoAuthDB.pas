unit CommunicationFrameworkDoubleTunnelIO_NoAuthDB;

interface

{$I zDefine.inc}


uses Classes,
  CoreClasses, ListEngine, UnicodeMixedLib, DataFrameEngine, MemoryStream64, CommunicationFramework, TextDataEngine,
  DoStatusIO, Cadencer, NotifyObjectBase, PascalStrings, CoreCipher, ZDBEngine, ItemStream,
  CommunicationFrameworkDoubleTunnelIO_NoAuth, ZDBLocalManager,
  CommunicationFrameworkDoubleTunnelIO_ServMan;

type
  TDTIO_PrimaryService = class;

  TDTIO_PrimaryService_PeerClientSendTunnel = class;

  TDTIO_PrimaryService_PeerClientRecvTunnel = class(TPeerClientUserDefineForRecvTunnel_NoAuth)
  public
    constructor Create(AOwner: TPeerClient); override;
    destructor Destroy; override;

    function SendTunnelDefine: TDTIO_PrimaryService_PeerClientSendTunnel;
  end;

  TDTIO_PrimaryService_PeerClientSendTunnel = class(TPeerClientUserDefineForSendTunnel_NoAuth)
  public
    constructor Create(AOwner: TPeerClient); override;
    destructor Destroy; override;

    function RecvTunnelDefine: TDTIO_PrimaryService_PeerClientRecvTunnel;
  end;

  TDTIO_PrimaryService = class(TCommunicationFramework_DoubleTunnelService_NoAuth)
  private
    FZDBLocal: TZDBLocalManager;
  public
    constructor Create(ARecvTunnel, ASendTunnel: TCommunicationFrameworkServer);
    destructor Destroy; override;

    procedure RegisterCommand; override;
    procedure UnRegisterCommand; override;

    procedure Progress; override;

    function GetDBUserDefine(RecvCli: TPeerClient): TDTIO_PrimaryService_PeerClientRecvTunnel;
  end;

  TDTIO_PrimaryClient = class(TCommunicationFramework_DoubleTunnelClient_NoAuth, IServerManager_ClientPoolNotify)
  protected
    // server manager notify
    procedure ServerConfigChange(Sender: TServerManager_Client; ConfigData: TSectionTextData);
    procedure ServerOffline(Sender: TServerManager_Client; RegAddr: string; ServerType: TServerType);
  public
    ServManClientPool: TServerManager_ClientPool;

    constructor Create(ARecvTunnel, ASendTunnel: TCommunicationFrameworkClient; AClientPoolDefaultClass: TCommunicationFrameworkClientClass);
    destructor Destroy; override;

    procedure RegisterCommand; override;
    procedure UnRegisterCommand; override;

    procedure Progress; override;
  end;

implementation

constructor TDTIO_PrimaryService_PeerClientRecvTunnel.Create(AOwner: TPeerClient);
begin
  inherited Create(AOwner);
end;

destructor TDTIO_PrimaryService_PeerClientRecvTunnel.Destroy;
begin
  inherited Destroy;
end;

function TDTIO_PrimaryService_PeerClientRecvTunnel.SendTunnelDefine: TDTIO_PrimaryService_PeerClientSendTunnel;
begin
  Result := SendTunnel as TDTIO_PrimaryService_PeerClientSendTunnel;
end;

constructor TDTIO_PrimaryService_PeerClientSendTunnel.Create(AOwner: TPeerClient);
begin
  inherited Create(AOwner);
end;

destructor TDTIO_PrimaryService_PeerClientSendTunnel.Destroy;
begin
  inherited Destroy;
end;

function TDTIO_PrimaryService_PeerClientSendTunnel.RecvTunnelDefine: TDTIO_PrimaryService_PeerClientRecvTunnel;
begin
  Result := RecvTunnel as TDTIO_PrimaryService_PeerClientRecvTunnel;
end;

constructor TDTIO_PrimaryService.Create(ARecvTunnel, ASendTunnel: TCommunicationFrameworkServer);
begin
  inherited Create(ARecvTunnel, ASendTunnel);
  FRecvTunnel.PeerClientUserDefineClass := TDTIO_PrimaryService_PeerClientRecvTunnel;
  FSendTunnel.PeerClientUserDefineClass := TDTIO_PrimaryService_PeerClientSendTunnel;

  FZDBLocal := TZDBLocalManager.Create;

  SwitchAsDefaultPerformance;
end;

destructor TDTIO_PrimaryService.Destroy;
begin
  DisposeObject([FZDBLocal]);
  inherited Destroy;
end;

procedure TDTIO_PrimaryService.RegisterCommand;
begin
  inherited RegisterCommand;
end;

procedure TDTIO_PrimaryService.UnRegisterCommand;
begin
  inherited UnRegisterCommand;
end;

procedure TDTIO_PrimaryService.Progress;
begin
  inherited Progress;
end;

function TDTIO_PrimaryService.GetDBUserDefine(RecvCli: TPeerClient): TDTIO_PrimaryService_PeerClientRecvTunnel;
begin
  Result := RecvCli.UserDefine as TDTIO_PrimaryService_PeerClientRecvTunnel;
end;

procedure TDTIO_PrimaryClient.ServerConfigChange(Sender: TServerManager_Client; ConfigData: TSectionTextData);
begin

end;

procedure TDTIO_PrimaryClient.ServerOffline(Sender: TServerManager_Client; RegAddr: string; ServerType: TServerType);
begin

end;

constructor TDTIO_PrimaryClient.Create(ARecvTunnel, ASendTunnel: TCommunicationFrameworkClient; AClientPoolDefaultClass: TCommunicationFrameworkClientClass);
begin
  inherited Create(ARecvTunnel, ASendTunnel);
  ServManClientPool := TServerManager_ClientPool.Create(AClientPoolDefaultClass, Self);
end;

destructor TDTIO_PrimaryClient.Destroy;
begin
  DisposeObject(ServManClientPool);
  inherited Destroy;
end;

procedure TDTIO_PrimaryClient.RegisterCommand;
begin
  inherited RegisterCommand;
end;

procedure TDTIO_PrimaryClient.UnRegisterCommand;
begin
  inherited UnRegisterCommand;
end;

procedure TDTIO_PrimaryClient.Progress;
begin
  inherited Progress;
  ServManClientPool.Progress;
end;

end.
