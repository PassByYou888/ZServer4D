unit MobileFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  CoreClasses, PascalStrings, UnicodeMixedLib, CommunicationFramework,
  xNATClient, DoStatusIO;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Timer1: TTimer;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    XCli: TXNATClient;
    procedure DoStatusIntf(AText: SystemString; const ID: Integer);
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}


procedure TForm1.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(Self, DoStatusIntf);

  XCli := TXNATClient.Create;
  XCli.RemoteTunnelAddr := '192.168.2.77';          // 公网服务器的IP
  XCli.RemoteTunnelPort := '7890';               // 公网服务器的端口号
  XCli.AuthToken := '123456';                    // 协议验证字符串
  XCli.AddMapping('192.168.2.77', '80', 'web8000'); // 将公网服务器的8000端口反向代理到本地80端口
  XCli.OpenTunnel;                               // 启动内网穿透
end;

procedure TForm1.DoStatusIntf(AText: SystemString; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
  Memo1.GoToLineEnd;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  DisposeObject(XCli);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if XCli <> nil then
      XCli.Progress;
end;

end.
