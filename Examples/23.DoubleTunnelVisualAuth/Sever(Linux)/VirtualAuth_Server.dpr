program VirtualAuth_Server;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,
  CoreClasses, ListEngine, UnicodeMixedLib, DoStatusIO,
  DataFrameEngine, MemoryStream64, PascalStrings, CoreCipher, NotifyObjectBase, Cadencer,
  CommunicationFramework,
  CommunicationFrameworkDoubleTunnelIO_VirtualAuth,
  CommunicationFramework_Server_CrossSocket;

type
  TMyService = class(TCommunicationFramework_DoubleTunnelService_VirtualAuth)
  protected
    procedure UserAuth(Sender: TVirtualAuthIO); override;
  end;

procedure OtherServerReponse(Sender: TNPostExecute);
var
  AuthIO: TVirtualAuthIO;
begin
  AuthIO := TVirtualAuthIO(Sender.Data1);
  // 在访问其他服务器的过程中，我们等待验证的用户可能已经断线，因此我们需要判断一下
  if not AuthIO.Online then
    begin
      AuthIO.Bye; // TVirtualAuthIO中的bye等同于Free，如果我们不Bye，会造成内存泄漏
      exit;
    end;

  // TVirtualAuthIO中的Accept和Reject方法只能被调用一次，完成后它会被自动释放
  if SameText(AuthIO.UserID, 'Test') and SameText(AuthIO.Passwd, 'Test') then
      AuthIO.Accept // 接受用户登录
  else
      AuthIO.Reject; // 拒绝用户登录
end;

procedure TMyService.UserAuth(Sender: TVirtualAuthIO); // TVirtualAuthIO有两种工作模式
begin
  inherited UserAuth(Sender);

  // 第一种是，立即认证，直接在下面写入实现
  (*
    if SameText(Sender.UserID, 'Test') and SameText(Sender.Passwd, 'Test') then
    Sender.Accept // 接受用户登录
    else
    Sender.Reject; // 拒绝用户登录
  *)
  // 第二种工作模式是延迟认证，在第二种模式中，我们什么都不用做，保存TVirtualAuthIO的实例，待认远程证完成，我们再反馈给客户端
  with ProgressEngine.PostExecuteC(3.0, OtherServerReponse) do
      Data1 := Sender;
end;

procedure RunServer;
var
  RecvTunnel, SendTunnel: TCommunicationFramework_Server_CrossSocket;
  Service: TMyService;
begin
  RecvTunnel := TCommunicationFramework_Server_CrossSocket.Create;
  SendTunnel := TCommunicationFramework_Server_CrossSocket.Create;
  SendTunnel.StartService('0.0.0.0', 9816);
  RecvTunnel.StartService('0.0.0.0', 9815);
  if SendTunnel.StartedService and RecvTunnel.StartedService then
    begin
      Service := TMyService.Create(RecvTunnel, SendTunnel);
      Service.RegisterCommand;

      while True do
        begin
          TCoreClassThread.Sleep(1);
          Service.Progress;
          try
              CoreClasses.CheckThreadSynchronize;
          except
          end;
        end;
    end
  else
    begin
      DisposeObject([RecvTunnel, SendTunnel]);
    end;
end;

begin
  try
    { TODO -oUser -cConsole Main : Insert code here }
      RunServer;
  except
    on E: Exception do
        Writeln(E.ClassName, ': ', E.Message);
  end;

end.
