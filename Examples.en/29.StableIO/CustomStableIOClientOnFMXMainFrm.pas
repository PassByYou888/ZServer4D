unit CustomStableIOClientOnFMXMainFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ScrollBox, FMX.Memo,
  FMX.StdCtrls, FMX.Edit, FMX.Controls.Presentation, FMX.Layouts,
  CoreClasses, ZDBEngine, ZDBLocalManager, DoStatusIO, DataFrameEngine, PascalStrings,
  ListEngine, UnicodeMixedLib, MemoryStream64,
  CommunicationFramework, CommunicationTest, XNATPhysics, FMX.Memo.Types;

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

  {  Important parameters  }
  {  We use xnat as the physical client for stable io. We are lazy and don't want to define different communication interfaces used by various platforms  }
  MyClient.PhysicsClient := TXPhysicsClient.Create;

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
  MyClient.LimitSequencePacketMemoryUsage := 0;

  {  be of no great importance  }
  {  When myclient connects for the first time, it fails and will try again all the time. This is done asynchronously and automatically in the background  }
  MyClient.AutomatedConnection := True;

  {  be of no great importance  }
  {  When myclient is released, the physical server txphysicsclient is automatically released  }
  MyClient.AutoFreePhysicsClient := True;

  {  be of no great importance  }
  {  During the myclient main loop processing, the physical server txphysicsclient is also processed  }
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

  {  Iobusy is an IO status checking mechanism applicable to any platform. When IO has data to process, it will return true  }
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
 
