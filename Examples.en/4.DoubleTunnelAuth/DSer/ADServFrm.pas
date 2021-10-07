unit ADServFrm;


interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  CommunicationFramework,
  CommunicationFramework_Server_ICS,
  CommunicationFramework_Server_Indy,
  CommunicationFramework_Server_CrossSocket, DoStatusIO, CoreClasses,
  DataFrameEngine, CommunicationFrameworkDoubleTunnelIO, CommunicationFrameworkDoubleTunnelIO_VirtualAuth;

type
  TAuthDoubleServerForm = class;

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
    RecvTunnel: TCommunicationFramework_Server_ICS;
    SendTunnel: TCommunicationFramework_Server_ICS;
    Service   : TMyService;
  end;

var
  AuthDoubleServerForm: TAuthDoubleServerForm;

implementation

{$R *.dfm}


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

  RecvTunnel := TCommunicationFramework_Server_ICS.Create;
  SendTunnel := TCommunicationFramework_Server_ICS.Create;
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
end;

procedure TAuthDoubleServerForm.FormDestroy(Sender: TObject);
begin
  disposeObject([RecvTunnel, SendTunnel, Service]);
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
  {  Based on the official ICs document, the bound host interface cannot be empty. You need to specify IPv4 or IPv6  }
  if SendTunnel.StartService('0.0.0.0', 9816) then
      DoStatus('listen send service success')
  else
      DoStatus('listen send service failed!');
  SendTunnel.IDCounter := 100;

  if RecvTunnel.StartService('0.0.0.0', 9815) then
      DoStatus('listen Recv service success')
  else
      DoStatus('listen Recv service failed!');

  Service.UnRegisterCommand;
  Service.RegisterCommand;
end;

procedure TAuthDoubleServerForm.Timer1Timer(Sender: TObject);
begin
  Service.Progress;
  timeLabel.Caption:=Format('sync time:%f', [Service.CadencerEngine.UpdateCurrentTime]);
end;

end.

