unit XNATMobileDeviceFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, System.TypInfo,
  FMX.TabControl, FMX.Edit, FMX.Layouts, FMX.ListBox,
  CoreClasses, PascalStrings, UnicodeMixedLib, CommunicationFramework,
  XNATMappingOnVirutalService, XNATPhysics, CommunicationTest, DoStatusIO, NotifyObjectBase,
  FMX.Memo.Types;

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
    XCli: TXNAT_VS_Mapping;
    server: TCommunicationFrameworkServer;
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

  XCli := TXNAT_VS_Mapping.Create;

  {  Penetration protocol compression options
Recommended usage scenarios:
If the agent's data has been compressed or encrypted using HTTPS, the compression will be invalid, and even the compressed data will be larger
If the time raw data protocol, such as FTP, HTTP without s, TenneT, the compression switch can be turned on and the speed can be increased slightly  }
  XCli.ProtocolCompressed := True;

  server := XCli.AddMappingServer('my18888', 5); {  Reverse proxy the 18888 port of the public network server to become a local server. There is only one physical connection  }

{  server.OfflineTimeout := 3 * 60 * 1000;        //  Offline reconnection technology, disconnect the instance of stableio 3 minutes after offline  }
{  server.PhysicsServer.TimeOutIDLE := 60 * 1000; //  The physical client is offline if it does not respond for 60 seconds  }

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
  XCli.Host := HostEdit.Text; {  IP address of public network server  }
  XCli.Port := '7890';        {  Port number of public network server  }
  XCli.AuthToken := '123456';             {  Protocol Validation string  }
  {  Start intranet penetration  }
  {  After the intranet penetration server is started, the local server will automatically start service, and the local server will not listen on any port  }
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
      InfoLabel.Text := PFormat('connection: %d' + #13#10 + 'physics: %d', [server.Count, server.Count]);
      XCli.Progress;
      server.Progress;
    end;
  PrintServerState([server]);
end;

end.
