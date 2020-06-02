unit XNATMobileDeviceFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, System.TypInfo,
  FMX.TabControl, FMX.Edit, FMX.Layouts, FMX.ListBox,
  CoreClasses, PascalStrings, UnicodeMixedLib, CommunicationFramework,
  XNATMappingOnVirutalService, XNATPhysics, CommunicationTest, DoStatusIO, NotifyObjectBase;

type
  TForm1 = class(TForm)
    Timer1: TTimer;
    OpenButton: TButton;
    InfoLabel: TLabel;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    LogMemo: TMemo;
    Layout1: TLayout;
    HostEdit: TEdit;
    Label1: TLabel;
    LogCheckBox: TCheckBox;
    InfoListBox: TListBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure OpenButtonClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    XCli: TXNAT_Mapping;
    server: TCommunicationFramework_StableServer;
    server_test: TCommunicationTestIntf;
    procedure DoStatusIntf(AText: SystemString; const ID: Integer);
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}


procedure TForm1.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(Self, DoStatusIntf);

  XCli := TXNAT_Mapping.Create;

  {
    穿透协议压缩选项
    建议使用场景:
    如果代理的数据已经压缩过，或则使用https这类方式加密过，压缩会无效，甚至压缩后数据更大
    如果时裸数据协议，比如ftp,不带s的http,tennet，压缩开关可以打开，可以小幅提速
  }
  XCli.ProtocolCompressed := True;

  server := XCli.AddMappingServer('my18888', 5).StableIO; // 将公网服务器的18888端口反向代理到成为本地服务器，物理连接只有1个

  server.OfflineTimeout := 3 * 60 * 1000;        // 离线重连技术，在离线后3分钟就断开stableIO的实例
  server.PhysicsServer.TimeOutIDLE := 60 * 1000; // 物理客户端60秒无响应就是离线状态

  server.PhysicsServer.QuietMode := True;
  server.QuietMode := False;

  server_test := TCommunicationTestIntf.Create;
  server_test.RegCmd(server);
end;

procedure TForm1.DoStatusIntf(AText: SystemString; const ID: Integer);
begin
  if not LogCheckBox.IsChecked then
      exit;
  LogMemo.Lines.Add(AText);
  LogMemo.GoToTextEnd;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  while server.Count > 0 do
      DisposeObject(server.FirstIO);
  DisposeObject(XCli);
end;

procedure TForm1.OpenButtonClick(Sender: TObject);
begin
  XCli.Host := HostEdit.Text; // 公网服务器的IP
  XCli.Port := '7890';        // 公网服务器的端口号
  XCli.AuthToken := '123456';             // 协议验证字符串
  // 启动内网穿透
  // 在启动了内网穿透服务器后，本地服务器会自动StartService，本地服务器不会侦听任何端口
  XCli.OpenTunnel;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
  procedure PrintServerState(const arry: array of TCommunicationFramework);
  var
    buff: array [TStatisticsType] of Int64;
    comm: TCommunicationFramework;
    st: TStatisticsType;
    i: Integer;
    v: Int64;
    n: string;
  begin
    for st := low(TStatisticsType) to high(TStatisticsType) do
        buff[st] := 0;

    for comm in arry do
      begin
        for st := low(TStatisticsType) to high(TStatisticsType) do
            buff[st] := buff[st] + comm.Statistics[st];
      end;

    while InfoListBox.Count < Ord(high(TStatisticsType)) + 1 do
        TListBoxItem.Create(InfoListBox).Parent := InfoListBox;

    for st := low(TStatisticsType) to high(TStatisticsType) do
      begin
        v := buff[st];
        n := IntToStr(v);
        InfoListBox.ListItems[Ord(st)].Text := GetEnumName(TypeInfo(TStatisticsType), Ord(st)) + ' : ' + n;
      end;
  end;
begin
  if XCli <> nil then
    begin
      InfoLabel.Text := PFormat('connection: %d' + #13#10 + 'physics: %d', [server.Count, server.PhysicsServer.Count]);
      XCli.Progress;
      server.Progress;
    end;
  PrintServerState([server]);
end;

end.
