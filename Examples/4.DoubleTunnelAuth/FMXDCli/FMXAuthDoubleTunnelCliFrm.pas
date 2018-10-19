unit FMXAuthDoubleTunnelCliFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Edit,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,

  CommunicationFramework_Client_Indy, DataFrameEngine,
  CommunicationFramework, CoreClasses, DoStatusIO,
  CommunicationFrameworkDoubleTunnelIO;

type
  TFMXAuthDoubleClientForm = class(TForm)
    Memo1: TMemo;
    connectButton: TButton;
    HostEdit: TEdit;
    HelloWorldBtn: TButton;
    Timer1: TTimer;
    UserEdit: TEdit;
    PasswdEdit: TEdit;
    RegUserButton: TButton;
    AsyncButton: TButton;
    timeLabel: TLabel;
    FixedTimeButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure connectButtonClick(Sender: TObject);
    procedure HelloWorldBtnClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure RegUserButtonClick(Sender: TObject);
    procedure AsyncButtonClick(Sender: TObject);
    procedure FixedTimeButtonClick(Sender: TObject);
  private
    { Private declarations }
    procedure DoStatusNear(AText: string; const ID: Integer);

    procedure cmd_ChangeCaption(Sender: TPeerClient; InData: TDataFrameEngine);
    procedure cmd_GetClientValue(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
  public
    { Public declarations }
    RecvTunnel: TCommunicationFramework_Client_Indy;
    SendTunnel: TCommunicationFramework_Client_Indy;
    client    : TCommunicationFramework_DoubleTunnelClient;
  end;

var
  FMXAuthDoubleClientForm: TFMXAuthDoubleClientForm;

implementation

{$R *.fmx}

{ TFMXClientForm }

procedure TFMXAuthDoubleClientForm.cmd_ChangeCaption(Sender: TPeerClient; InData: TDataFrameEngine);
begin
  DoStatus('Change Caption:%s', [InData.Reader.ReadString]);
end;

procedure TFMXAuthDoubleClientForm.cmd_GetClientValue(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
begin
  OutData.WriteString('getclientvalue:abc');
end;

procedure TFMXAuthDoubleClientForm.connectButtonClick(Sender: TObject);
begin
  SendTunnel.Connect(HostEdit.Text, 9815);
  RecvTunnel.Connect(HostEdit.Text, 9816);

  // 检查双通道是否都已经成功链接，确保完成了对称加密等等初始化工作
  while (not client.RemoteInited) and (client.Connected) do
    begin
      TThread.Sleep(10);
      client.Progress;
    end;

  if client.Connected then
    begin
      // 嵌套式匿名函数支持
      client.UserLoginP(UserEdit.Text, PasswdEdit.Text,
        procedure(const State: Boolean)
        begin
          if State then
              client.TunnelLinkP(
              procedure(const State: Boolean)
              begin
                DoStatus('double tunnel link success!');
              end)
        end);
    end;
end;

procedure TFMXAuthDoubleClientForm.DoStatusNear(AText: string; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
  Memo1.GoToTextEnd;
end;

procedure TFMXAuthDoubleClientForm.FixedTimeButtonClick(Sender: TObject);
begin
  // 高速同步，不经过Progress触发
  // 这样干是将时间的延迟率降低到最小
  client.SendTunnel.SyncOnResult := True;
  client.SyncCadencer;
  client.SendTunnel.WaitP(1000, procedure(const cState: Boolean)
    begin
      // 因为打开了SyncOnResult后，匿名函数会出现嵌套死锁
      // 我们现在关闭它，以保证匿名函数的嵌套执行
      client.SendTunnel.SyncOnResult := False;
    end);
end;

procedure TFMXAuthDoubleClientForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatusNear);

  RecvTunnel := TCommunicationFramework_Client_Indy.Create;
  SendTunnel := TCommunicationFramework_Client_Indy.Create;
  client := TCommunicationFramework_DoubleTunnelClient.Create(RecvTunnel, SendTunnel);

  client.RegisterCommand;

  // 注册可以由服务器发起的通讯指令
  client.RecvTunnel.RegisterDirectStream('ChangeCaption').OnExecute := cmd_ChangeCaption;
  client.RecvTunnel.RegisterStream('GetClientValue').OnExecute := cmd_GetClientValue;
end;

procedure TFMXAuthDoubleClientForm.FormDestroy(Sender: TObject);
begin
  DisposeObject(client);
  DeleteDoStatusHook(self);
end;

procedure TFMXAuthDoubleClientForm.HelloWorldBtnClick(Sender: TObject);
var
  SendDe, ResultDE: TDataFrameEngine;
begin
  // 往服务器发送一条console形式的hello world指令
  client.SendTunnel.SendDirectConsoleCmd('helloWorld_Console', '');

  // 往服务器发送一条stream形式的hello world指令
  SendDe := TDataFrameEngine.Create;
  SendDe.WriteString('directstream 123456');
  client.SendTunnel.SendDirectStreamCmd('helloWorld_Stream', SendDe);
  DisposeObject([SendDe]);

  // 异步方式发送，并且接收Stream指令，反馈以proc回调触发
  SendDe := TDataFrameEngine.Create;
  SendDe.WriteString('123456');
  client.SendTunnel.SendStreamCmdP('helloWorld_Stream_Result', SendDe,
    procedure(Sender: TPeerClient; ResultData: TDataFrameEngine)
    begin
      if ResultData.Count > 0 then
          DoStatus('server response:%s', [ResultData.Reader.ReadString]);
    end);
  DisposeObject([SendDe]);

  // 阻塞方式发送，并且接收Stream指令
  SendDe := TDataFrameEngine.Create;
  ResultDE := TDataFrameEngine.Create;
  SendDe.WriteString('123456');
  client.SendTunnel.WaitSendStreamCmd('helloWorld_Stream_Result', SendDe, ResultDE, 5000);
  if ResultDE.Count > 0 then
      DoStatus('server response:%s', [ResultDE.Reader.ReadString]);
  DisposeObject([SendDe, ResultDE]);
end;

procedure TFMXAuthDoubleClientForm.RegUserButtonClick(Sender: TObject);
begin
  SendTunnel.Connect(HostEdit.Text, 9815);
  RecvTunnel.Connect(HostEdit.Text, 9816);

  // 检查双通道是否都已经成功链接，确保完成了对称加密等等初始化工作
  while (not client.RemoteInited) and (client.Connected) do
    begin
      TThread.Sleep(10);
      client.Progress;
    end;

  if client.Connected then
      client.RegisterUser(UserEdit.Text, PasswdEdit.Text);

  SendTunnel.Disconnect;
  RecvTunnel.Disconnect;
end;

procedure TFMXAuthDoubleClientForm.AsyncButtonClick(Sender: TObject);
begin
  // 方法2，异步式双通道链接
  client.AsyncConnectP(HostEdit.Text, 9816, 9815,
    procedure(const cState: Boolean)
    begin
      if cState then
        begin
          // 嵌套式匿名函数支持
          client.UserLoginP(UserEdit.Text, PasswdEdit.Text,
            procedure(const lState: Boolean)
            begin
              if lState then
                begin
                  client.TunnelLinkP(
                    procedure(const tState: Boolean)
                    begin
                      if tState then
                          DoStatus('double tunnel link success!')
                      else
                          DoStatus('double tunnel link failed!');
                    end)
                end
              else
                begin
                  if lState then
                      DoStatus('login success!')
                  else
                      DoStatus('login failed!');
                end;
            end);
        end
      else
        begin
          if cState then
              DoStatus('connected success!')
          else
              DoStatus('connected failed!');
        end;
    end);
end;

procedure TFMXAuthDoubleClientForm.Timer1Timer(Sender: TObject);
begin
  client.Progress;
  TimeLabel.text := Format('sync time:%f', [client.CadencerEngine.UpdateCurrentTime]);
end;

end.
