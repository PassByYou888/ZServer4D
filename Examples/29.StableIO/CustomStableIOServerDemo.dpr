program CustomStableIOServerDemo;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  SysUtils,
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF MSWINDOWS}
  Classes,
  Variants,
  CoreClasses,
  DoStatusIO,
  DataFrameEngine,
  PascalStrings,
  ListEngine,
  UnicodeMixedLib,
  MemoryStream64,
  CommunicationFramework,
  CommunicationTest,
  XNATPhysics;

{
  StableIO原理
  StableIO通过截获物理IO(任意synapse,corss,diocp,indy,ico的IO)的收发事件，独立了一个稳定的IO，叫StableIO
  StableIO是虚化的，它不会像物理IO那样，断线以后，实例和现场都被破坏了，StableIO在断线以后，仍然还在工作，StableIO处于一种offline的工作模式。
  当物理IO重连，StableIO会从offline中离开，进入online模式，并还原现场

  StableIO的服务器必须是，TCommunicationFramework_StableServer
  StableIO的客户端必须是，TCommunicationFramework_StableClient
  这两者必须对号，才能进行自动化的断线重连和恢复现场能力

  普通客户端也能直接使用StableIO服务器，但是不具有断线重连功能

  运行StableIO的Demo，使用物理网络系统更容易测试和验证
}

type
  TMyServer = class(TCommunicationFramework_StableServer)
  public
    Test: TCommunicationTestIntf;
    constructor Create; override;
    destructor Destroy; override;
  end;

constructor TMyServer.Create;
begin
  inherited;
  Test := TCommunicationTestIntf.Create;
  Test.RegCmd(Self);
end;

destructor TMyServer.Destroy;
begin
  DisposeObject(Test);
  inherited;
end;

procedure MainLoop;
var
  MyServer: TMyServer;
  iostate, discard, recv, send, sequmem, n: string;
begin
  MyServer := TMyServer.Create;

  // 重要参数
  // 我们使用XNAT作为StableIO使用的物理服务器，因为作者很懒，不想去定义各个平台使用的不同通讯接口
  MyServer.PhysicsServer := TXPhysicsServer.Create;

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
  MyServer.LimitSequencePacketMemoryUsage := 0;

  // 重要参数
  // 客户端离线后，StableIO会开启offline的工作模式，所有对offline的数据发送，都会暂存到内存中
  // 这个参数是允许客户端离线多久，单位是毫秒
  // 如果是高频率的网络数据收发，积累时间太长，会让内存崩溃
  // 我们这里使用5分钟的离线限制，在拔掉网线的5分钟内，重连都能恢复现场
  MyServer.OfflineTimeout := 5 * 60 * 1000;

  // 无关紧要
  // myserver释放时，自动释放物理服务器TXPhysicsServer
  MyServer.AutoFreePhysicsServer := True;

  // 无关紧要
  // myserver主循环处理时，也处理物理服务器TXPhysicsServer
  MyServer.AutoProgressPhysicsServer := True;

  if MyServer.StartService('0.0.0.0', 11977) then
      DoStatus('stableIO listen success!');

  while True do
    begin
      MyServer.Progress;
{$IFDEF MSWINDOWS}
      // IOBusy 是适用于任何平台的IO状态检查机制，当IO有数据在处理时，就会返回true
      if MyServer.IOBusy then
          iostate := 'Busy'
      else
          iostate := 'Idle';
      // SetConsoleTitle('Server IDLE');

      discard := Format(
        'discard: %d, size: %s', [MyServer.Statistics[TStatisticsType.stSequencePacketDiscard],
        umlSizeToStr(MyServer.Statistics[TStatisticsType.stSequencePacketDiscardSize]).Text]);

      recv := Format('received: %d', [MyServer.Statistics[TStatisticsType.stReceiveSize]]);
      send := Format('sending: %d', [MyServer.Statistics[TStatisticsType.stSendSize]]);
      sequmem := Format('swap memory: %s', [umlSizeToStr(MyServer.Statistics[TStatisticsType.stSequencePacketMemoryOnSending]).Text]);

      SetConsoleTitle(PWideChar(Format('%s - IO:%d PIO:%d - %s - %s - %s - %s',
        [iostate, MyServer.Count, MyServer.PhysicsServer.Count, recv, send, discard, sequmem])));
{$ENDIF MSWINDOWS}
      CoreClasses.CheckThreadSynchronize(10);
    end;
end;

begin
  try
      MainLoop;
  except
    on E: Exception do
        Writeln(E.ClassName, ': ', E.Message);
  end;

end.
 
