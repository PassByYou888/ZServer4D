unit VirtualServFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls,
  CoreClasses, PascalStrings, UnicodeMixedLib, CommunicationFramework,
  XNATMappingOnVirutalService, XNATPhysics, CommunicationTest, DoStatusIO, NotifyObjectBase;

type
  TForm2 = class(TForm)
    Memo1: TMemo;
    Timer1: TTimer;
    TestButton: TButton;
    OpenButton: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure OpenButtonClick(Sender: TObject);
    procedure TestButtonClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    XCli: TXNAT_Mapping;
    server: TCommunicationFrameworkServer;
    server_test: TCommunicationTestIntf;

    // 模拟器测试客户端
    // 我们模拟测试可以开两个app来干，处于方便直接内置实现
    client: TCommunicationFrameworkClient;
    client_test: TCommunicationTestIntf;
    procedure DoStatusIntf(AText: SystemString; const ID: Integer);
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}


procedure TForm2.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(Self, DoStatusIntf);

  XCli := TXNAT_Mapping.Create;

  {
    穿透协议压缩选项
    建议使用场景:
    如果代理的数据已经压缩过，或则使用https这类方式加密过，压缩会无效，甚至压缩后数据更大
    如果是裸数据协议，比如ftp,不带s的http,tennet，压缩开关可以打开，可以小幅提速
  }
  XCli.ProtocolCompressed := True;

  XCli.Host := '127.0.0.1';       // 公网服务器的IP
  XCli.Port := '7890';                  // 公网服务器的端口号
  XCli.AuthToken := '123456';                       // 协议验证字符串
  server := XCli.AddMappingServer('web8000', 1000); // 将公网服务器的8000端口反向代理到成为本地服务器

  server_test := TCommunicationTestIntf.Create;
  server_test.RegCmd(server);

  client := TXPhysicsClient.Create;
  client_test := TCommunicationTestIntf.Create;
  client_test.RegCmd(client);
end;

procedure TForm2.DoStatusIntf(AText: SystemString; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
  Memo1.GoToTextEnd;
end;

procedure TForm2.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  DisposeObject(XCli);
end;

procedure TForm2.OpenButtonClick(Sender: TObject);
begin
  // 启动内网穿透
  // 在启动了内网穿透服务器后，本地服务器会自动StartService，本地服务器不会侦听任何端口
  XCli.OpenTunnel;
end;

procedure TForm2.TestButtonClick(Sender: TObject);
begin
  // 模拟测试：连接到公网服务器

  // 这是内置的客户端访问内置的服务器，客户端有阻塞机制，注意死循环
  // 避开死循环的方法直接使用异步方式
  client.AsyncConnectP('127.0.0.1', 8000, procedure(const cState: Boolean)
    begin
      if cState then
        begin
          client_test.ExecuteAsyncTestWithBigStream(client.ClientIO);
        end;
    end);
end;

procedure TForm2.Timer1Timer(Sender: TObject);
begin
  if XCli <> nil then
    begin
      XCli.Progress;
      client.Progress;
    end;
end;

end.
 
