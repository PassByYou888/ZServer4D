unit ADServFrm;


interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  PascalStrings,
  CommunicationFramework,
  CommunicationFramework_Server_ICS,
  CommunicationFramework_Server_Indy,
  CommunicationFramework_Server_CrossSocket, DoStatusIO, CoreClasses,
  DataFrameEngine, CommunicationFrameworkDoubleTunnelIO;

type
  TAuthDoubleServerForm = class;

  TMyVM_Tunnel = class(TCommunicationFramework_Server_CrossSocket)
  public
    procedure p2pVMTunnelAuth(Sender: TPeerIO; const Token: SystemString; var Accept: Boolean); override;
    {  When the VM tunnel was first created  }
    procedure p2pVMTunnelOpenBefore(Sender: TPeerClient; p2pVMTunnel: TCommunicationFrameworkWithP2PVM); override;
    {  The tunnel has successfully shaken hands  }
    procedure p2pVMTunnelOpen(Sender: TPeerClient; p2pVMTunnel: TCommunicationFrameworkWithP2PVM); override;
    {  Triggered by a delay of one second after a successful tunnel handshake  }
    procedure p2pVMTunnelOpenAfter(Sender: TPeerClient; p2pVMTunnel: TCommunicationFrameworkWithP2PVM); override;
    {  The tunnel is closed. This event is triggered when a remote request or disconnection occurs  }
    procedure p2pVMTunnelClose(Sender: TPeerClient; p2pVMTunnel: TCommunicationFrameworkWithP2PVM); override;
  end;

  TMyService = class(TCommunicationFramework_DoubleTunnelService)
  private
    f: TAuthDoubleServerForm;
  protected
    procedure UserRegistedSuccess(UserID: string); override;
    procedure UserLinkSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel); override;
    procedure UserOut(UserDefineIO: TPeerClientUserDefineForRecvTunnel); override;
  protected
    // reg cmd
    procedure cmd_helloWorld_Console(Sender: TPeerClient; InData: string);
    procedure cmd_helloWorld_Stream(Sender: TPeerClient; InData: TDataFrameEngine);
    procedure cmd_helloWorld_Stream_Result(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
  public
    procedure RegisterCommand; override;
    procedure UnRegisterCommand; override;
  end;

  TAuthDoubleServerForm = class(TForm)
    Memo1: TMemo;
    StartServiceButton: TButton;
    Timer1: TTimer;
    ChangeCaptionButton: TButton;
    GetClientValueButton: TButton;
    TimeLabel: TLabel;
    procedure StartServiceButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ChangeCaptionButtonClick(Sender: TObject);
    procedure GetClientValueButtonClick(Sender: TObject);
  private
    { Private declarations }
    procedure DoStatusNear(AText: string; const ID: Integer);
  public
    { Public declarations }

    {  VM tunnel  }
    {  The VM tunnel can carry the work of the entire protocol stack in normal operation  }
    {  Here, we will bind recvtunnel + sendtunnel in vmtunnel at the same time, and use only one link to implement dual channel service  }
    VMTunnel: TMyVM_Tunnel;

    {  ZS normal communication framework  }
    RecvTunnel: TCommunicationFrameworkWithP2PVM_Server;
    SendTunnel: TCommunicationFrameworkWithP2PVM_Server;
    Service: TMyService;
  end;

var
  AuthDoubleServerForm: TAuthDoubleServerForm;

implementation

{$R *.dfm}

procedure TMyVM_Tunnel.p2pVMTunnelAuth(Sender: TPeerIO; const Token: SystemString; var Accept: Boolean);
begin
  Accept := True;
end;

procedure TMyVM_Tunnel.p2pVMTunnelClose(Sender: TPeerClient; p2pVMTunnel: TCommunicationFrameworkWithP2PVM);
begin
  {  When this event is triggered, the VM tunnel has been released  }

  {  Restore binding  }
  Sender.p2pVM.UnInstallLogicFramework(AuthDoubleServerForm.RecvTunnel);
  Sender.p2pVM.UnInstallLogicFramework(AuthDoubleServerForm.SendTunnel);
  inherited p2pVMTunnelClose(Sender, p2pVMTunnel);
end;

procedure TMyVM_Tunnel.p2pVMTunnelOpenBefore(Sender: TPeerClient; p2pVMTunnel: TCommunicationFrameworkWithP2PVM);
begin
  inherited;
  {  When this event is triggered, the VM tunnel has been established, but no handshake is held  }

  {  If the bound channel is a server type, it can be one to many  }
  {  One to many is the same server, which can be bound to multiple VM tunnels to realize VM virtual tunnel service  }
  {  The VM virtual tunnel service does not limit the number of links. A virtual tunnel can carry more than 1 million links  }

  {  Once the tunnel is established successfully, vmtunnel can also send and receive commands normally. The tunnel binding of recvtunnel + sendtunnel will not affect vmtunnel  }
  {  Once the tunnel is established successfully, the vmtunnel protocol will change. It is not a special case. Do not cancel the tunnel easily  }

  {  Bind the receive channel to the VM tunnel  }
  Sender.p2pVM.InstallLogicFramework(AuthDoubleServerForm.RecvTunnel);
  {  Bind send channel to VM tunnel  }
  Sender.p2pVM.InstallLogicFramework(AuthDoubleServerForm.SendTunnel);
end;

procedure TMyVM_Tunnel.p2pVMTunnelOpen(Sender: TPeerClient; p2pVMTunnel: TCommunicationFrameworkWithP2PVM);
begin
  inherited;
  {  When this event is triggered, the VM has successfully shaken hands  }
end;

procedure TMyVM_Tunnel.p2pVMTunnelOpenAfter(Sender: TPeerClient; p2pVMTunnel: TCommunicationFrameworkWithP2PVM);
begin
  inherited;
  {  When this event is triggered, the VM has successfully shaken hands and 1 second has elapsed  }
end;

procedure TMyService.UserLinkSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel);
begin
  inherited UserLinkSuccess(UserDefineIO);
  DoStatus('user link success!');
end;

procedure TMyService.UserOut(UserDefineIO: TPeerClientUserDefineForRecvTunnel);
begin
  inherited UserOut(UserDefineIO);
  DoStatus('user out!');
end;

procedure TMyService.UserRegistedSuccess(UserID: string);
begin
  inherited UserRegistedSuccess(UserID);
end;

procedure TMyService.cmd_helloWorld_Console(Sender: TPeerClient; InData: string);
var
  UserIO: TPeerClientUserDefineForRecvTunnel;
begin
  UserIO := GetUserDefineRecvTunnel(Sender);

  {  User not logged in successfully  }
  if not UserIO.LoginSuccessed then
      exit;
  {  Channels not merged  }
  if not UserIO.LinkOK then
      exit;

  DoStatus('client: %s', [InData]);
end;

procedure TMyService.cmd_helloWorld_Stream(Sender: TPeerClient; InData: TDataFrameEngine);
var
  UserIO: TPeerClientUserDefineForRecvTunnel;
begin
  UserIO := GetUserDefineRecvTunnel(Sender);

  {  User not logged in successfully  }
  if not UserIO.LoginSuccessed then
      exit;
  {  Channels not merged  }
  if not UserIO.LinkOK then
      exit;

  DoStatus('client: %s', [InData.Reader.ReadString]);
end;

procedure TMyService.cmd_helloWorld_Stream_Result(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
var
  UserIO: TPeerClientUserDefineForRecvTunnel;
begin
  UserIO := GetUserDefineRecvTunnel(Sender);

  {  User not logged in successfully  }
  if not UserIO.LoginSuccessed then
      exit;
  {  Channels not merged  }
  if not UserIO.LinkOK then
      exit;

  OutData.WriteString('result 654321');
end;

procedure TMyService.RegisterCommand;
begin
  inherited RegisterCommand;
  RecvTunnel.RegisterDirectConsole('helloWorld_Console').OnExecute := cmd_helloWorld_Console;
  RecvTunnel.RegisterDirectStream('helloWorld_Stream').OnExecute := cmd_helloWorld_Stream;
  RecvTunnel.RegisterStream('helloWorld_Stream_Result').OnExecute := cmd_helloWorld_Stream_Result;
end;

procedure TMyService.UnRegisterCommand;
begin
  inherited UnRegisterCommand;
  RecvTunnel.UnRegisted('helloWorld_Console');
  RecvTunnel.UnRegisted('helloWorld_Stream');
  RecvTunnel.UnRegisted('helloWorld_Stream_Result');
end;

procedure TAuthDoubleServerForm.ChangeCaptionButtonClick(Sender: TObject);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  de.WriteString('change caption as hello World,from server!');
  {  The broadcast method does not distinguish whether the client has logged in or whether the dual channel has been successfully established  }
  SendTunnel.BroadcastDirectStreamCmd('ChangeCaption', de);
  disposeObject(de);
end;

procedure TAuthDoubleServerForm.DoStatusNear(AText: string; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
end;

procedure TAuthDoubleServerForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatusNear);

  VMTunnel := TMyVM_Tunnel.Create;

  RecvTunnel := TCommunicationFrameworkWithP2PVM_Server.Create;
  SendTunnel := TCommunicationFrameworkWithP2PVM_Server.Create;
  Service := TMyService.Create(RecvTunnel, SendTunnel);

  {  By default, tmyservice will not save user information to userdb, and many useless directories will be generated every time you exit the server  }
  {  When we open allowsaveuserinfo, all user information will be permanently recorded  }
  {  Note: in the future, when we want to maintain the user database, we can only do it through programming. Directly managing files is anti-human  }
  Service.AllowSaveUserInfo := True;

  {  When the dual channel server with authentication is started, the user database must be read manually. This step will use a lot of swap memory. If the size of userdb is 300m, 2G memory overhead is required for reading  }
  {  After the user logs in, in order to speed up the retrieval of user data, all user information is stored in memory. If the size of userdb is 300m, the running memory overhead is about 1g  }
  {  If there are too many users, such as more than 100000, the memory of X86 platform is not enough. You need x64  }
  {  Loaduserdb uses a high-speed hash table internally for search. It reads very fast but consumes memory  }
  Service.LoadUserDB;

  Service.f := self;
  Service.AllowRegisterNewUser := True;

  StartServiceButtonClick(nil);
end;

procedure TAuthDoubleServerForm.FormDestroy(Sender: TObject);
begin
  disposeObject([VMTunnel, RecvTunnel, SendTunnel, Service]);
  DeleteDoStatusHook(self);
end;

procedure TAuthDoubleServerForm.GetClientValueButtonClick(Sender: TObject);
begin
  SendTunnel.ProgressPeerIOP(procedure(PeerClient: TPeerClient)
    var
      c: TPeerClient;
      de: TDataFrameEngine;
    begin
      c := PeerClient;
      {  If the client does not log in successfully  }
      if TPeerClientUserDefineForSendTunnel(c.UserDefine).RecvTunnel = nil then
          exit;
      {  As above, if the client is not logged in  }
      if not TPeerClientUserDefineForSendTunnel(c.UserDefine).RecvTunnel.LinkOK then
          exit;

      de := TDataFrameEngine.Create;
      de.WriteString('change caption as hello World,from server!');
      c.SendStreamCmdP('GetClientValue', de,
        procedure(Sender: TPeerClient; ResultData: TDataFrameEngine)
        begin
          if ResultData.Count > 0 then
              DoStatus('getClientValue [%s] response:%s', [c.GetPeerIP, ResultData.Reader.ReadString]);
        end);
      disposeObject(de);
    end);
end;

procedure TAuthDoubleServerForm.StartServiceButtonClick(Sender: TObject);
begin
  {  Based on the official crosssocket documentation, if the host interface is empty, bind all IPv6 + IPv4 IP addresses  }
  {  If the host interface is 0.0.0.0, bind all IPv4 addresses,:: bind all IPv6 addresses  }
  if VMTunnel.StartService('', 9899) then
      DoStatus('VM tunnel port 9899 listening succeeded')
  else
      DoStatus('VM tunnel port 9899 listening failed, system occupied');

  {  VM only supports IPv6 address listening. VM listening does not affect the operating system and is done in the VM tunnel  }
  {  The VM's tunnel service can listen to more than one at a time  }
  {  We listen on Ports 1 and 2 in the VM  }
  SendTunnel.StartService('::', 1);
  SendTunnel.StartService('::', 11);
  SendTunnel.StartService('::', 111);
  SendTunnel.StartService('::', 1111);
  SendTunnel.StartService('::33:ff', 1111);

  RecvTunnel.StartService('::', 2);
  RecvTunnel.StartService('::12:3e:87ef', 2);
  RecvTunnel.StartService('::4:ff08:e302', 2);

  Service.UnRegisterCommand;
  Service.RegisterCommand;
end;

procedure TAuthDoubleServerForm.Timer1Timer(Sender: TObject);
begin
  VMTunnel.Progress;
  Service.Progress;
  TimeLabel.Caption := Format('sync time:%f', [Service.CadencerEngine.UpdateCurrentTime]);
end;

end.

