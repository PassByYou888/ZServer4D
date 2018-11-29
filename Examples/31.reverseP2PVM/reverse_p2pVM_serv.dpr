program reverse_p2pVM_serv;

{$APPTYPE CONSOLE}

{$R *.res}

{
  P2PVM是基于IO工作的虚拟机，它对服务器和客户端模型没有要求，
  简单来说，
  P2PVM服务器和客户端可以工作于物理客户端也能工作于物理服务器
  只要有IO存在，P2PVM就能工作
  我们如果基于P2PVM搭建后台服务器，伸缩空间将会非常灵活，不局限于任何网络环境
}

uses
  SysUtils,
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF MSWINDOWS}
  CoreClasses,
  PascalStrings,
  UnicodeMixedLib,
  CommunicationFramework,
  PhysicsIO,
  DoStatusIO;

{
  该Demo会先创建一个物理客户端，然后在物理客户端基础上运行P2PVM的服务器
}
type
  TMyP2PVM_Server = class(TCommunicationFrameworkWithP2PVM_Server)
  public
  end;

var
  MyP2PVM_Server: TMyP2PVM_Server;

type
  TMyPhysics_Client = class(TPhysicsClient)
  public
    procedure p2pVMTunnelAuth(Sender: TPeerIO; const Token: SystemString; var Accept: Boolean); override;
    procedure p2pVMTunnelOpenBefore(Sender: TPeerIO; p2pVMTunnel: TCommunicationFrameworkWithP2PVM); override;
    procedure p2pVMTunnelOpen(Sender: TPeerIO; p2pVMTunnel: TCommunicationFrameworkWithP2PVM); override;
    procedure p2pVMTunnelOpenAfter(Sender: TPeerIO; p2pVMTunnel: TCommunicationFrameworkWithP2PVM); override;
    procedure p2pVMTunnelClose(Sender: TPeerIO; p2pVMTunnel: TCommunicationFrameworkWithP2PVM); override;
  end;

procedure TMyPhysics_Client.p2pVMTunnelAuth(Sender: TPeerIO; const Token: SystemString; var Accept: Boolean);
begin
  inherited;
  // p2pVM的握手步骤1:远程验证
  Accept := True;
end;

procedure TMyPhysics_Client.p2pVMTunnelOpenBefore(Sender: TPeerIO; p2pVMTunnel: TCommunicationFrameworkWithP2PVM);
begin
  inherited;
  DoStatus('P2PVM OpenBefore 请求端IP: %s', [Sender.PeerIP]);
  Sender.p2pVM.QuietMode := True;
  Sender.p2pVM.InstallLogicFramework(MyP2PVM_Server);
end;

procedure TMyPhysics_Client.p2pVMTunnelOpen(Sender: TPeerIO; p2pVMTunnel: TCommunicationFrameworkWithP2PVM);
begin
  inherited;
  DoStatus('P2PVM Open 请求端IP: %s', [Sender.PeerIP]);
end;

procedure TMyPhysics_Client.p2pVMTunnelOpenAfter(Sender: TPeerIO; p2pVMTunnel: TCommunicationFrameworkWithP2PVM);
begin
  inherited;
  DoStatus('P2PVM OpenAfter 请求端IP: %s', [Sender.PeerIP]);
end;

procedure TMyPhysics_Client.p2pVMTunnelClose(Sender: TPeerIO; p2pVMTunnel: TCommunicationFrameworkWithP2PVM);
begin
  inherited;
  DoStatus('P2PVM Close 请求端IP: %s', [Sender.PeerIP]);
  Sender.p2pVM.UninstallLogicFramework(MyP2PVM_Server);
end;

var
  MyPhysics_Client: TMyPhysics_Client;

begin
  MyP2PVM_Server := TMyP2PVM_Server.Create;
  MyP2PVM_Server.StartService('::', 99);
  MyP2PVM_Server.QuietMode := True;

  MyPhysics_Client := TMyPhysics_Client.Create;

  while True do
    begin
{$IFDEF MSWINDOWS}
      SetConsoleTitle(PWideChar(Format('P2PVM 服务器在线: %d', [MyP2PVM_Server.Count])));
{$ENDIF MSWINDOWS}
      // 自动重连
      if not MyPhysics_Client.RemoteInited then
        if not MyPhysics_Client.Connect('127.0.0.1', 19899) then
          begin
            CoreClasses.CheckThreadSynchronize(100);
            continue;
          end;

      // 在p2pVM被InstallLogicFramework后，progress会自动调用MyP2PVM_Server.Progress
      if MyPhysics_Client.Connected then
          MyPhysics_Client.Progress;

      // 绿色环保，给cpu节能
      CoreClasses.CheckThreadSynchronize(10);
    end;

end.
