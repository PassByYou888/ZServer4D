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
  {  In the process of accessing other servers, the user waiting for authentication may be disconnected, so we need to judge  }
  if not AuthIO.Online then
    begin
      AuthIO.Bye; {  Bye in tvirtualuthio is equivalent to free. If we don't bye, it will cause memory leakage  }
      exit;
    end;

  {  The accept and reject methods in tvirtualuthio can only be called once and will be automatically released after completion  }
  if SameText(AuthIO.UserID, 'Test') and SameText(AuthIO.Passwd, 'Test') then
      AuthIO.Accept {  Accept user login  }
  else
      AuthIO.Reject; {  Deny user login  }
end;

procedure TMyService.UserAuth(Sender: TVirtualAuthIO); {  Tvirtualuthio has two working modes  }
begin
  inherited UserAuth(Sender);

  {  The first is to authenticate immediately and write the implementation directly below  }
  {  if SameText(Sender.UserID, 'Test') and SameText(Sender.Passwd, 'Test') then
Sender. Accept / / accept user login
else
Sender.Reject; //  Deny user login  }
  {  The second working mode is delayed authentication. In the second mode, we don't have to do anything. Save the instance of tvirtualuthio. After the remote authentication is completed, we will feed it back to the client  }
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
