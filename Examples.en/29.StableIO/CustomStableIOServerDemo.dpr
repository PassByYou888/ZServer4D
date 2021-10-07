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

{  Stable IO principle
Stable IO intercepts the transceiver events of physical IO (IO of any synapse, corss, diocp, Indy and ICO), and is independent of a stable IO called stable io
Stable IO is virtualized. It will not be like physical io. After disconnection, the instance and site are destroyed. Stable IO is still working after disconnection. Stable IO is in an offline working mode.
When the physical IO is reconnected, the stable IO will leave the offline mode, enter the online mode, and restore the site
The server for stableio must be the Tcommunication framework_ StableServer
The client of stableio must be the Tcommunication framework_ StableClient
The two must be numbered before automatic disconnection reconnection and field capacity recovery can be carried out
Ordinary clients can also directly use the stable IO server, but they do not have the function of disconnection and reconnection
Running the demo of stableio makes it easier to test and verify using the physical network system  }

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

  {  Important parameters  }
  {  We use xnat as the physical server for stable IO, because the author is lazy and doesn't want to define different communication interfaces used by various platforms  }
  MyServer.PhysicsServer := TXPhysicsServer.Create;

  {  Important parameters  }
  {  When the client connects with the server, it will automatically enter the working mode of sequence package  }
  {  The sequence package will use 2-3 times the memory under normal conditions  }
  {  The function of limitsequencepacketmemoryusage is to limit the memory usage of sequence package working mode  }
  {  0 means unlimited use of memory, 1024 means limited to 1KB of memory  }
  {  If you want to give a limit, it is recommended to give 64 * 1024 * 1024 on the server side, which means to limit the maximum memory overhead of 64M sequence table. If the client can not limit it directly  }
  {  When the limit is exceeded, the virtualized IO connection will be forcibly closed and memory will be reclaimed  }
  {  Under normal circumstances, only two scenarios will use a lot of sequence package memory  }
  {  1： In offline mode, because the data sent cannot reach the target, it will accumulate in memory until it exceeds the memory security limit or reaches the offline time limit  }
  {  2： In high-speed networks, such as local to local or Gigabit, when sending and receiving large completebuffer and bigstream, the sequence packet communication mode will use 2-3 times the normal memory overhead  }
  {  Using the stable IO mechanism, it is recommended to use a 64 bit operating system, windows or Linux, and ensure that sufficient memory is configured  }
  MyServer.LimitSequencePacketMemoryUsage := 0;

  {  Important parameters  }
  {  When the client is offline, the stableio will start the offline working mode, and all data sent to the offline will be temporarily stored in memory  }
  {  This parameter is how long the client is allowed to go offline, in milliseconds  }
  {  If it is high-frequency network data sending and receiving, the accumulation time is too long, which will crash the memory  }
  {  We use the 5-minute offline limit here. Within 5 minutes after unplugging the network cable, reconnection can be restored to the scene  }
  MyServer.OfflineTimeout := 5 * 60 * 1000;

  {  be of no great importance  }
  {  When Myserver is released, the physical server txphysicsserver is automatically released  }
  MyServer.AutoFreePhysicsServer := True;

  {  be of no great importance  }
  {  During the Myserver main loop processing, the physical server txphysicsserver is also processed  }
  MyServer.AutoProgressPhysicsServer := True;

  if MyServer.StartService('0.0.0.0', 11977) then
      DoStatus('stableIO listen success!');

  while True do
    begin
      MyServer.Progress;
{$IFDEF MSWINDOWS}
      {  Iobusy is an IO status checking mechanism applicable to any platform. When IO has data to process, it will return true  }
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
 
