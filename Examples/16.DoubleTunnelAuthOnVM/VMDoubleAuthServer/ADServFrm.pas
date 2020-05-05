unit ADServFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  CommunicationFramework,
  CommunicationFramework_Server_ICS,
  CommunicationFramework_Server_Indy,
  CommunicationFramework_Server_CrossSocket, DoStatusIO, CoreClasses,
  DataFrameEngine, CommunicationFrameworkDoubleTunnelIO;

type
  TAuthDoubleServerForm = class;

  TMyVM_Tunnel = class(TCommunicationFramework_Server_CrossSocket)
  public
    // vm 隧道刚被创建时
    procedure p2pVMTunnelOpenBefore(Sender: TPeerClient; p2pVMTunnel: TCommunicationFrameworkWithP2PVM); override;
    // 隧道已经握手成功
    procedure p2pVMTunnelOpen(Sender: TPeerClient; p2pVMTunnel: TCommunicationFrameworkWithP2PVM); override;
    // 隧道握手成功后延迟一秒所触发
    procedure p2pVMTunnelOpenAfter(Sender: TPeerClient; p2pVMTunnel: TCommunicationFrameworkWithP2PVM); override;
    // 隧道关闭，该事件是在发生远程请求，或则断线时触发
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

    // vm隧道
    // vm隧道可以在正常工作中，同时带起整个协议栈的工作
    // 我们在这里会将RecvTunnel+SendTunnel同时绑定在VMTunnel中，只用一个链接实现双通道服务
    VMTunnel: TMyVM_Tunnel;

    // zs正常的通讯框架
    RecvTunnel: TCommunicationFrameworkWithP2PVM_Server;
    SendTunnel: TCommunicationFrameworkWithP2PVM_Server;
    Service   : TMyService;
  end;

var
  AuthDoubleServerForm: TAuthDoubleServerForm;

implementation

{$R *.dfm}


procedure TMyVM_Tunnel.p2pVMTunnelClose(Sender: TPeerClient; p2pVMTunnel: TCommunicationFrameworkWithP2PVM);
begin
  // 触发该事件时，vm隧道已经解除

  // 恢复绑定
  Sender.p2pVM.UnInstallLogicFramework(AuthDoubleServerForm.RecvTunnel);
  Sender.p2pVM.UnInstallLogicFramework(AuthDoubleServerForm.SendTunnel);
  inherited p2pVMTunnelClose(Sender, p2pVMTunnel);
end;

procedure TMyVM_Tunnel.p2pVMTunnelOpenBefore(Sender: TPeerClient; p2pVMTunnel: TCommunicationFrameworkWithP2PVM);
begin
  inherited;
  // 触发该事件时，vm隧道已经建立，但是未握手

  // 如果绑定的的通道是服务器类型，可以一对多
  // 一对多是同一个服务器，可以绑定到多个vm隧道中，从而实现vm虚拟隧道服务
  // 而vm虚拟隧道服务对链接数是不限制的，一个虚拟隧道可以带100万以上链接

  // 隧道一旦建立成功，VMTunnel也能正常收发命令，RecvTunnel+SendTunnel的隧道绑定，并不会影响VMTunnel
  // 隧道一旦建立成功，VMTunnel的协议就会发生变化，不是特殊情况，不要轻易解除隧道

  // 将接收通道绑定到vm隧道中
  Sender.p2pVM.InstallLogicFramework(AuthDoubleServerForm.RecvTunnel);
  // 将发送通道绑定到vm隧道中
  Sender.p2pVM.InstallLogicFramework(AuthDoubleServerForm.SendTunnel);
end;

procedure TMyVM_Tunnel.p2pVMTunnelOpen(Sender: TPeerClient; p2pVMTunnel: TCommunicationFrameworkWithP2PVM);
begin
  inherited;
  // 触发该事件时，vm已成功过握手
end;

procedure TMyVM_Tunnel.p2pVMTunnelOpenAfter(Sender: TPeerClient; p2pVMTunnel: TCommunicationFrameworkWithP2PVM);
begin
  inherited;
  // 触发该事件时，vm已成功过握手，并且经过了1秒
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

  VMTunnel := TMyVM_Tunnel.Create;

  RecvTunnel := TCommunicationFrameworkWithP2PVM_Server.Create;
  SendTunnel := TCommunicationFrameworkWithP2PVM_Server.Create;
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
  // 基于CrosssSocket官方文档，绑定如果Host接口如果为空，绑定所有IPV6+IPV4的IP地址
  // 如果Host接口为0.0.0.0绑定所有IPV4地址，::绑定所有IPV6地址
  if VMTunnel.StartService('', 9899) then
      DoStatus('vm隧道端口 9899 侦听成功')
  else
      DoStatus('vm隧道端口 9899 侦听失败，系统占用');

  // VM只支持ipv6格式的地址侦听，VM侦听不影响操作系统，都是在VM隧道中做的侦听
  // VM的隧道服务侦听可以一次侦听多个
  // 我们在vm中侦听1和2两个端口
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
