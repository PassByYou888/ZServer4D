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

  // 用户未登录成功
  if not UserIO.LoginSuccessed then
      exit;
  // 通道未合并
  if not UserIO.LinkOK then
      exit;

  DoStatus('client: %s', [InData]);
end;

procedure TMyService.cmd_helloWorld_Stream(Sender: TPeerClient; InData: TDataFrameEngine);
var
  UserIO: TPeerClientUserDefineForRecvTunnel;
begin
  UserIO := GetUserDefineRecvTunnel(Sender);

  // 用户未登录成功
  if not UserIO.LoginSuccessed then
      exit;
  // 通道未合并
  if not UserIO.LinkOK then
      exit;

  DoStatus('client: %s', [InData.Reader.ReadString]);
end;

procedure TMyService.cmd_helloWorld_Stream_Result(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
var
  UserIO: TPeerClientUserDefineForRecvTunnel;
begin
  UserIO := GetUserDefineRecvTunnel(Sender);

  // 用户未登录成功
  if not UserIO.LoginSuccessed then
      exit;
  // 通道未合并
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
  // 广播方法不会区分客户端是否有登录，是否建立成功了双通道
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

  // 默认情况下，TMyService不会保存用户信息到UserDB，在每次退出服务器都会产生许多没用的目录
  // 当我们将CanSaveUserInfo打开以后，所有的用户信息都将被永久记录
  // 注意：未来当我们要维护时用户数据库时，只能通过编程来干，直接管理文件是反人类的
  Service.CanSaveUserInfo := True;

  // 带验证的双通道服务器启动时 必须手动读取用户数据库 这一步会大量使用交换内存 如果UserDB大小为300M 读取时大概需要2G内存开销
  // 在用户登录后，为了加快用户资料检索，所有用户信息存在与内存中，如果UserDB大小为300M 运行时大概需要1G的内存开销
  // 如果用户太多，诸如超过10万，那么x86平台的内存是不够用的，你需要x64
  // LoadUserDB内部使用了是高速Hash表进行搜索，读取非常快 但非常消耗内存
  Service.LoadUserDB;

  Service.f := self;
  Service.CanRegisterNewUser := True;
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
      // 如果客户端没有登录成功
      if TPeerClientUserDefineForSendTunnel(c.UserDefine).RecvTunnel = nil then
          exit;
      // 和上列一样，如果客户端没有登录
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
  // 基于ICS官方文档，绑定Host接口不能为空，需要指定IPV4 or IPV6
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
