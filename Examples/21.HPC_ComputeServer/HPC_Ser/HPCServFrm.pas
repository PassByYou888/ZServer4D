unit HPCServFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Threading,
  CommunicationFramework,
  CommunicationFramework_Server_ICS,
  CommunicationFramework_Server_Indy,
  CommunicationFramework_Server_CrossSocket, DoStatusIO, CoreClasses,
  DataFrameEngine, CommunicationFrameworkDoubleTunnelIO_NoAuth;

type
  TDoubleServerForm = class;

  TMyService = class(TCommunicationFramework_DoubleTunnelService_NoAuth)
  private
    f: TDoubleServerForm;
  protected
    procedure UserLinkSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth); override;
    procedure UserOut(UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth); override;
  protected
    // reg cmd
    procedure cmd_helloWorld_Stream_Result(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
  public
    procedure RegisterCommand; override;
    procedure UnRegisterCommand; override;
  end;

  TDoubleServerForm = class(TForm)
    Memo1: TMemo;
    StartServiceButton: TButton;
    Timer1: TTimer;
    ChangeCaptionButton: TButton;
    GetClientValueButton: TButton;
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
    RecvTunnel: TCommunicationFramework_Server_CrossSocket;
    SendTunnel: TCommunicationFramework_Server_CrossSocket;
    Service: TMyService;
  end;

var
  DoubleServerForm: TDoubleServerForm;

implementation

{$R *.dfm}


procedure TMyService.UserLinkSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth);
begin
  inherited UserLinkSuccess(UserDefineIO);
  DoStatus('user link success!');
end;

procedure TMyService.UserOut(UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth);
begin
  inherited UserOut(UserDefineIO);
  DoStatus('user out!');
end;

procedure TMyService.cmd_helloWorld_Stream_Result(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
begin
  // hpc延迟后台运算机制演示，机制非常简单，可以大规模堆砌工程化代码
  CommunicationFramework.RunHPC_StreamP(Sender, nil, nil, InData, OutData,
    procedure(ThSender: THPC_Stream; ThInData, ThOutData: TDataFrameEngine)
    begin
      // 如果在你的后台服务器框架有调度中心服务器：ManagerServer
      TCompute.sync(procedure
        begin
          // 我们要在主进程的运行地带告诉调度中心服务器，我开始做大规模运算工作了
        end);

      // hpc延迟后台运算与传统处理差异在于我们需要使用ThInData,ThOutData来辨别和反馈数据

      // 在hpc后台计算可以使用doStatus
      DoStatus('run compute thread');

      // 这里的代码是工作于线程地带的
      ThOutData.WriteString('result 654321');

      // 如果需要同步到主线程，需要使用
      TCompute.sync(procedure
        begin
          // 这里是主进程的同步地带，比如文件操作，zdb数据库操作等等
        end);

      // 在hpc的后台延迟线程中，并行化是安全的
      // ParallelFor优于Delphi内置的TParallel.For
      // ParallelFor优于fpc内置的mtprocs
      ParallelFor(0, 10000, procedure(pass: Integer)
        begin
          // 在并行处理中，因为emb的设计问题，我们无法使用 TThread.Synchronize 方法
          // 在并行处理中，我们可以使用zServer内核原子锁
          LockObject(Sender);
          UnLockObject(Sender);

          // 在hpc后台计算可以使用doStatus
          if pass mod 1000 = 1 then
              DoStatus('run compute thread:%d', [pass]);
        end);

      // 如果在你的后台服务器框架有调度中心服务器：ManagerServer
      TCompute.sync(procedure
        begin
          // 我们要在主进程的运行地带告诉调度中心服务器，我的大规模计算工作做完了
        end);

      // 最后，在延迟运行结束后，ThOutData将被发送给客户端，然后释放掉临时内存
    end);
end;

procedure TMyService.RegisterCommand;
begin
  inherited RegisterCommand;
  RecvTunnel.RegisterStream('helloWorld_Stream_Result').OnExecute := cmd_helloWorld_Stream_Result;
end;

procedure TMyService.UnRegisterCommand;
begin
  inherited UnRegisterCommand;
  RecvTunnel.UnRegisted('helloWorld_Stream_Result');
end;

procedure TDoubleServerForm.ChangeCaptionButtonClick(Sender: TObject);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  de.WriteString('change caption as hello World,from server!');
  SendTunnel.BroadcastDirectStreamCmd('ChangeCaption', de);
  disposeObject(de);
end;

procedure TDoubleServerForm.DoStatusNear(AText: string; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
end;

procedure TDoubleServerForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatusNear);

  RecvTunnel := TCommunicationFramework_Server_CrossSocket.Create;
  SendTunnel := TCommunicationFramework_Server_CrossSocket.Create;
  Service := TMyService.Create(RecvTunnel, SendTunnel);
  Service.f := self;
end;

procedure TDoubleServerForm.FormDestroy(Sender: TObject);
begin
  disposeObject([RecvTunnel, SendTunnel, Service]);
  DeleteDoStatusHook(self);
end;

procedure TDoubleServerForm.GetClientValueButtonClick(Sender: TObject);
begin
  SendTunnel.ProgressPeerIOP(procedure(PeerClient: TPeerClient)
    var
      c: TPeerClient;
      de: TDataFrameEngine;
    begin
      c := PeerClient;
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

procedure TDoubleServerForm.StartServiceButtonClick(Sender: TObject);
begin
  // 基于CrosssSocket官方文档，绑定字符串如果为空，绑定IPV6+IPV4
  if SendTunnel.StartService('', 9816) then
      DoStatus('listen send service success')
  else
      DoStatus('listen send service failed!');
  SendTunnel.IDCounter := 100;

  if RecvTunnel.StartService('', 9815) then
      DoStatus('listen Recv service success')
  else
      DoStatus('listen Recv service failed!');

  Service.RegisterCommand;
end;

procedure TDoubleServerForm.Timer1Timer(Sender: TObject);
begin
  Service.Progress;
end;

end.
