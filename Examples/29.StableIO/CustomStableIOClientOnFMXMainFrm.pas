unit CustomStableIOClientOnFMXMainFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ScrollBox, FMX.Memo,
  FMX.StdCtrls, FMX.Edit, FMX.Controls.Presentation, FMX.Layouts,
  CoreClasses, ZDBEngine, ZDBLocalManager, DoStatusIO, DataFrameEngine, PascalStrings,
  ListEngine, UnicodeMixedLib, MemoryStream64,
  CommunicationFramework, CommunicationTest, XNATPhysics;

type
  TMyClient = class(TCommunicationFramework_StableClient)
  protected
    procedure DoConnected(Sender: TPeerIO); override;
    procedure DoDisconnect(Sender: TPeerIO); override;
  public
    Test: TCommunicationTestIntf;
    constructor Create; override;
    destructor Destroy; override;
    function isOffline: Boolean;
  end;

  TForm1 = class(TForm)
    Layout1: TLayout;
    Label1: TLabel;
    HostEdit: TEdit;
    connectButton: TButton;
    Memo1: TMemo;
    RunTestButton: TButton;
    DisconnectButton: TButton;
    Timer1: TTimer;
    IOStateLabel: TLabel;
    InfoLabel: TLabel;
    procedure connectButtonClick(Sender: TObject);
    procedure DisconnectButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RunTestButtonClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    MyClient: TMyClient;
    procedure backcall_DoStatus(AText: SystemString; const ID: Integer);
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}


constructor TMyClient.Create;
begin
  inherited;
  Test := TCommunicationTestIntf.Create;
  Test.RegCmd(Self);
end;

destructor TMyClient.Destroy;
begin
  DisposeObject(Test);
  inherited;
end;

procedure TMyClient.DoConnected(Sender: TPeerIO);
begin
  inherited;
end;

procedure TMyClient.DoDisconnect(Sender: TPeerIO);
begin
  inherited;
end;

function TMyClient.isOffline: Boolean;
begin
  Result := (not StableClientIO.Activted) or
    ((StableClientIO.Activted) and (StableClientIO.WaitConnecting));
end;

procedure TForm1.backcall_DoStatus(AText: SystemString; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
  Memo1.GoToTextEnd;
end;

procedure TForm1.connectButtonClick(Sender: TObject);
begin
  MyClient.AsyncConnectP(HostEdit.Text, 11977, procedure(const cState: Boolean)
    begin
    end);
end;

procedure TForm1.DisconnectButtonClick(Sender: TObject);
begin
  MyClient.Disconnect;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(Self, backcall_DoStatus);
  MyClient := TMyClient.Create;

  // 重要参数
  // 我们使用XNAT作为StableIO使用的物理客户端，懒，不想去定义各个平台使用的不同通讯接口
  MyClient.PhysicsClient := TXPhysicsClient.Create;

  // 重要参数
  // 当客户端与服务器连接后，会自动进入序列包的工作模式
  // 序列包会使用正常情况下的2-3倍内存
  // LimitSequencePacketMemoryUsage 的作用是限制序列包工作模式的内存使用
  // 0表示无限制使用内存，1024表示限制为1kb内存
  // 如果要给限制，建议在服务器端给64*1024*1024，表示限制最大的保持64M的序列表内存开销，如果客户端可以直接不限制
  // 当超出限制，虚化的IO连接会被强制关闭，并且回收内存
  // 在正常情况下，只有2种场景会大量使用序列包内存
  // 一:offline的模式因为发送的数据无法到达目标，会一直积累在内存中，直到超出内存安全限制或则达到离线时间限制
  // 二:在高速网络下，比如本地对本地或则千兆内，收发大型CompleteBuffer，BigStream，在收发的过程中，序列包通讯模式会使用正常内存开销的2-3倍容量
  // 使用StableIO机制，建议使用64位操作系统，Windows or linux，并且确保配置了足够使用的内存
  MyClient.LimitSequencePacketMemoryUsage := 0;

  // 无关紧要
  // MyClient 在首次连接时，失败，会一直重试，这是在后台以异步方式自动化进行的
  MyClient.AutomatedConnection := True;

  // 无关紧要
  // MyClient 释放时，自动释放物理服务器TXPhysicsClient
  MyClient.AutoFreePhysicsClient := True;

  // 无关紧要
  // MyClient 主循环处理时，也处理物理服务器TXPhysicsClient
  MyClient.AutoProgressPhysicsClient := True;
end;

procedure TForm1.RunTestButtonClick(Sender: TObject);
begin
  if MyClient.Connected then
      MyClient.Test.ExecuteAsyncTestWithBigStream(MyClient.ClientIO);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  discard, recv, send, sequmem: string;
begin
  MyClient.Progress;

  // IOBusy 是适用于任何平台的IO状态检查机制，当IO有数据在处理时，就会返回true
  if MyClient.IOBusy then
      IOStateLabel.Text := 'IO Busy...'
  else
      IOStateLabel.Text := 'IO IDLE';

  discard := Format(
    'discard: %d, size: %s', [MyClient.Statistics[TStatisticsType.stSequencePacketDiscard],
    umlSizeToStr(MyClient.Statistics[TStatisticsType.stSequencePacketDiscardSize]).Text]);

  recv := Format('received: %d', [MyClient.Statistics[TStatisticsType.stReceiveSize]]);
  send := Format('sending: %d', [MyClient.Statistics[TStatisticsType.stSendSize]]);
  sequmem := Format('swap memory: %s', [umlSizeToStr(MyClient.ClientIO.SequencePacketUsagePhysicsMemory).Text]);

  InfoLabel.Text := Format('%s'#13#10'%s'#13#10'%s'#13#10'%s'#13#10'StopCommunicationTimeTick: %f ',
    [recv, send, discard, sequmem, MyClient.StopCommunicationTimeTick * 0.001]);
end;

end.
 
