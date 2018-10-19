unit ADRestoreServFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  CommunicationFramework,
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
  public
    procedure RegisterCommand; override;
    procedure UnRegisterCommand; override;
  end;

  TAuthDoubleServerForm = class(TForm)
    Memo1: TMemo;
    StartServiceButton: TButton;
    Timer1: TTimer;
    procedure StartServiceButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    procedure DoStatusNear(AText: string; const ID: Integer);
  public
    { Public declarations }
    RecvTunnel: TCommunicationFramework_Server_CrossSocket;
    SendTunnel: TCommunicationFramework_Server_CrossSocket;
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

procedure TMyService.RegisterCommand;
begin
  inherited RegisterCommand;
end;

procedure TMyService.UnRegisterCommand;
begin
  inherited UnRegisterCommand;
end;

procedure TAuthDoubleServerForm.DoStatusNear(AText: string; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
end;

procedure TAuthDoubleServerForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatusNear);

  RecvTunnel := TCommunicationFramework_Server_CrossSocket.Create;
  SendTunnel := TCommunicationFramework_Server_CrossSocket.Create;
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
end;

end.
