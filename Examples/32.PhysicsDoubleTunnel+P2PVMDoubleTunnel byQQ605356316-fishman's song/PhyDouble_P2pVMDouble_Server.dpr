program PhyDouble_P2pVMDouble_Server;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  DoStatusIO, CoreClasses, NotifyObjectBase,
  CommunicationFramework,
  CommunicationFramework_Server_CrossSocket,
  CommunicationFrameworkDoubleTunnelIO_VirtualAuth,
  CommunicationFrameworkDoubleTunnelIO_NoAuth;

type
  TMyTunnel = class(TCommunicationFramework_Server_CrossSocket)
  private
    P2PVMRecvServer, P2PVMSendServer: TCommunicationFramework;
  public
    // vm 隧道刚被创建时
    procedure p2pVMTunnelOpenBefore(Sender: TPeerClient; p2pVMTunnel: TCommunicationFrameworkWithP2PVM); override;
    // 隧道关闭，该事件是在发生远程请求，或则断线时触发
    procedure p2pVMTunnelClose(Sender: TPeerClient; p2pVMTunnel: TCommunicationFrameworkWithP2PVM); override;
    procedure setP2PVMServer(AP2PVMRecvServer, AP2PVMSendServer: TCommunicationFramework);
  end;

  TMyServer = class(TCommunicationFramework_DoubleTunnelService_VirtualAuth)
  protected
    procedure UserLinkSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth); override;
    procedure UserOut(UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth); override;
    procedure UserAuth(Sender: TVirtualAuthIO); override;
  public
    procedure RegisterCommand; override;
  end;

  TMyVMServer = class(TCommunicationFramework_DoubleTunnelService_NoAuth)
  public
    procedure RegisterCommand; override;
  end;

  { TMyServer }

procedure TMyServer.RegisterCommand;
begin
  inherited;
  RecvTunnel.RegisterDirectConsole('helloWorld_Console').OnExecuteProc := procedure(Sender: TPeerClient; InData: string)
    begin
      if not GetUserDefineRecvTunnel(Sender).LoginSuccessed then
        exit;
      if not GetUserDefineRecvTunnel(Sender).LinkOk then
        exit;
      DoStatus('client: %s', [InData]);
    end;
end;

procedure TMyServer.UserAuth(Sender: TVirtualAuthIO);
begin
  inherited;
  with ProgressEngine.PostExecuteP(3.0,
    procedure(Sender: TNPostExecute)
    var
      AuthIO: TVirtualAuthIO;
    begin
      AuthIO := TVirtualAuthIO(Sender.Data1);
      if not AuthIO.Online then
      begin
        AuthIO.Bye;
        exit;
      end;

      if SameText(AuthIO.UserID, 'Test') and SameText(AuthIO.Passwd, 'Test') then
        AuthIO.Accept // 接受用户登录
      else
      begin
        AuthIO.Reject; // 拒绝用户登录
      end;
    end) do
    Data1 := Sender;
end;

procedure TMyServer.UserLinkSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth);
begin
  inherited;
  DoStatus('user link: ' + UserDefineIO.UserID);
end;

procedure TMyServer.UserOut(UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth);
begin
  inherited;
  if UserDefineIO.LoginSuccessed then
    DoStatus('user out: ' + UserDefineIO.UserID);
end;

procedure RunServer;
var
  SendTunnel: TCommunicationFramework_Server_CrossSocket;
  RecvTunnel: TMyTunnel;
  MyVMServer: TMyVMServer;
  MyServer: TMyServer;
begin
  MyVMServer := TMyVMServer.Create(TCommunicationFrameworkWithP2PVM_Server.Create,
    TCommunicationFrameworkWithP2PVM_Server.Create);
  MyVMServer.RecvTunnel.StartService('::', 2);
  MyVMServer.SendTunnel.StartService('::', 1);
  MyVMServer.RegisterCommand;

  RecvTunnel := TMyTunnel.Create;
  RecvTunnel.setP2PVMServer(MyVMServer.RecvTunnel, MyVMServer.SendTunnel);
  SendTunnel := TCommunicationFramework_Server_CrossSocket.Create;
  MyServer := TMyServer.Create(RecvTunnel, SendTunnel);
  if RecvTunnel.StartService('', 10002) and SendTunnel.StartService('', 10001) then
  begin
    MyServer.RegisterCommand;
    DoStatus('listen successful');
    while True do
    begin
      MyServer.Progress;
      MyVMServer.Progress;
      if RecvTunnel.Count + SendTunnel.Count > 0 then
      begin
        CoreClasses.CheckThreadSynchronize(1);
      end
      else
      begin
        CoreClasses.CheckThreadSynchronize(100);
      end;
    end;
  end
  else
  begin
    DoStatus('listen failed!');
  end;
end;

{ TMyVMTunnel }

procedure TMyTunnel.p2pVMTunnelClose(Sender: TPeerClient; p2pVMTunnel: TCommunicationFrameworkWithP2PVM);
begin
  DoStatus('UnInstallLogicFramework');
  Sender.p2pVM.UnInstallLogicFramework(P2PVMRecvServer);
  Sender.p2pVM.UnInstallLogicFramework(P2PVMSendServer);
  inherited;
end;

procedure TMyTunnel.p2pVMTunnelOpenBefore(Sender: TPeerClient; p2pVMTunnel: TCommunicationFrameworkWithP2PVM);
begin
  inherited;
  DoStatus('InstallLogicFramework');
  Sender.p2pVM.InstallLogicFramework(P2PVMRecvServer);
  Sender.p2pVM.InstallLogicFramework(P2PVMSendServer);
end;

procedure TMyTunnel.setP2PVMServer(AP2PVMRecvServer, AP2PVMSendServer: TCommunicationFramework);
begin
  P2PVMRecvServer := AP2PVMRecvServer;
  P2PVMSendServer := AP2PVMSendServer;
end;

{ TMyVMServer }

procedure TMyVMServer.RegisterCommand;
begin
  inherited;
  RecvTunnel.RegisterDirectConsole('helloWorld_Console').OnExecuteProc := procedure(Sender: TPeerClient; InData: string)
    begin
      if not GetUserDefineRecvTunnel(Sender).LinkOk then
        exit;
      DoStatus('clientvm: %s', [InData]);
    end;
end;

begin
  try
    RunServer;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
