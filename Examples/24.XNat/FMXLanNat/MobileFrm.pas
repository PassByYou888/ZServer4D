unit MobileFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  CoreClasses, PascalStrings, UnicodeMixedLib, CommunicationFramework,
  XNATPhysics, XNATClient, DoStatusIO;

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

  {
    穿透协议压缩选项
    建议使用场景:
    如果代理的数据已经压缩过，或则使用https这类方式加密过，压缩会无效，甚至压缩后数据更大
    如果是裸数据协议，比如ftp,不带s的http,tennet，压缩开关可以打开，可以小幅提速
  }
  XCli.ProtocolCompressed := True;

  XCli.Host := '127.0.0.1';                           // 公网服务器的IP
  XCli.Port := '7890';                                // 公网服务器的端口号
  XCli.AuthToken := '123456';                         // 协议验证字符串
  XCli.AddMapping('127.0.0.1', '80', 'web8000', 100); // 将公网服务器的8000端口反向代理到本地80端口
  XCli.OpenTunnel;                                    // 启动内网穿透
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
 
