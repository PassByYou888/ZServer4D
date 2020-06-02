unit CustomProtocolFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,

  CommunicationFramework, PascalStrings,
  CommunicationFramework_Server_CrossSocket, CommunicationFramework_Client_CrossSocket,
  DoStatusIO, MemoryStream64, CoreClasses,

  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IDGlobal;

type
  // TPeerClientUserSpecial是每个客户端p2p链接后的特殊实例接口
  // 我们也可以通过继承TPeerClientUserDefine达到同样的功能
  TMyPeerClientUserSpecial = class(TPeerClientUserSpecial)
  public
    myBuffer: TMemoryStream64;

    constructor Create(AOwner: TPeerClient); override;
    destructor Destroy; override;

    // 这里是同步事件，我们在此处实现对叠加写入的碎片化缓冲区处理过程
    procedure Progress; override;
  end;

  TMyServer = class(TCommunicationFramework_Server_CrossSocket)
  public
    // 从服务器获取外部定制化处理缓冲区接口
    // 这里的buffer全部是碎片化缓冲区
    procedure OnReceiveBuffer(Sender: TPeerIO; const buffer: PByte; const Size: NativeInt; var FillDone: Boolean); override;
  end;

  TMyClient = class(TCommunicationFramework_Client_CrossSocket)
  public
    myBuffer: TMemoryStream64;

    constructor Create; override;
    destructor Destroy; override;
    // 从服务器获取外部定制化处理缓冲区接口
    // 这里的buffer全部是碎片化缓冲区
    procedure OnReceiveBuffer(const buffer: PByte; const Size: NativeInt; var FillDone: Boolean); override;
  end;

  TCustomProtocolForm = class(TForm)
    Memo: TMemo;
    Timer: TTimer;
    Panel1: TPanel;
    connectOnIndyButton: TButton;
    SendDataOnIndyButton: TButton;
    IdTCPClient1: TIdTCPClient;
    connectOnZServerButton: TButton;
    SendDataOnZServerButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure connectOnIndyButtonClick(Sender: TObject);
    procedure connectOnZServerButtonClick(Sender: TObject);
    procedure SendDataOnIndyButtonClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SendDataOnZServerButtonClick(Sender: TObject);
  private
  public
    // 自定义协议的服务器
    myServer: TMyServer;

    // 自定义协议的客户端
    myClient: TMyClient;

    procedure DoStatusMethod(AText: SystemString; const ID: Integer);
  end;

var
  CustomProtocolForm: TCustomProtocolForm;

implementation

{$R *.dfm}


constructor TMyPeerClientUserSpecial.Create(AOwner: TPeerClient);
begin
  inherited;
  myBuffer := TMemoryStream64.Create;

end;

destructor TMyPeerClientUserSpecial.Destroy;
begin
  DisposeObject(myBuffer);

  inherited;
end;

procedure TMyPeerClientUserSpecial.Progress;
begin
  inherited;
  // 这里是同步事件，我们在此处实现对叠加写入的碎片化缓冲区处理过程

  // 先检查缓冲区是不是空
  if myBuffer.Size > 0 then
    begin
      // 如果缓冲区不是空
      // 我们打印接收到数据内容
      DoStatus(format('receive [%s] [%d] ', [Owner.PeerIP, Owner.ID]), myBuffer.Memory, myBuffer.Size, 16);
      // 我们将接收到的数据原封不动的反馈给发送方
      Owner.WriteCustomBuffer(myBuffer.Memory, myBuffer.Size);

      // 清空缓冲区，为下一次处理做准备
      myBuffer.Clear;
    end;
end;

procedure TMyServer.OnReceiveBuffer(Sender: TPeerIO; const buffer: PByte; const Size: NativeInt; var FillDone: Boolean);
begin
  // FillDone的作用
  // 如果FillDone是True，内核会认为，你已经处理过这个缓冲区了，当退出该事件后，不会再进行处理
  // 如果FillDone是False，内核会在退出该事件后，按ZS的正常机制进行处理，包括使用秘钥拖密，解压
  // 在Protocol := cpCustom情况下，FillDone都是True，如果Protocol := cpZServer，该事件不会被触发

  // 从服务器获取外部定制化处理缓冲区接口
  // 这里的buffer全部是碎片化缓冲区
  // 我们将碎片缓冲区追加写入到myBuffer
  TMyPeerClientUserSpecial(Sender.UserSpecial).myBuffer.WritePtr(buffer, Size);
end;

constructor TMyClient.Create;
begin
  inherited Create;
  myBuffer := TMemoryStream64.Create;
end;

destructor TMyClient.Destroy;
begin
  DisposeObject(myBuffer);
  inherited Destroy;
end;

procedure TMyClient.OnReceiveBuffer(const buffer: PByte; const Size: NativeInt; var FillDone: Boolean);
begin
  // FillDone的作用
  // 如果FillDone是True，内核会认为，你已经处理过这个缓冲区了，当退出该事件后，不会再进行处理
  // 如果FillDone是False，内核会在退出该事件后，按ZS的正常机制进行处理，包括使用秘钥拖密，解压
  // 在Protocol := cpCustom情况下，FillDone都是True，如果Protocol := cpZServer，该事件不会被触发

  // 从服务器获取外部定制化处理缓冲区接口
  // 这里的buffer全部是碎片化缓冲区
  // 我们将碎片缓冲区追加写入到myBuffer
  myBuffer.WritePtr(buffer, Size);
end;

procedure TCustomProtocolForm.connectOnIndyButtonClick(Sender: TObject);
begin
  IdTCPClient1.Connect;

  if IdTCPClient1.Connected then
      DoStatus('connect on indy ok!');
end;

procedure TCustomProtocolForm.connectOnZServerButtonClick(Sender: TObject);
begin
  myClient.AsyncConnectP('127.0.0.1', 9989, procedure(const cState: Boolean)
    begin
      if cState then
          DoStatus('ZServer自定义客户端连接成功');
    end);
end;

procedure TCustomProtocolForm.DoStatusMethod(AText: SystemString; const ID: Integer);
begin
  Memo.Lines.Add(AText);
end;

procedure TCustomProtocolForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  DisposeObject(myServer);
  DisposeObject(myClient);
end;

procedure TCustomProtocolForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(Self, DoStatusMethod);
  myServer := TMyServer.Create;
  // 使用自定义通讯协议
  myServer.Protocol := cpCustom;
  // 指定客户端的p2p实例接口
  myServer.PeerClientUserSpecialClass := TMyPeerClientUserSpecial;
  myServer.StartService('', 9989);

  myClient := TMyClient.Create;
  myClient.Protocol := cpCustom;
end;

procedure TCustomProtocolForm.TimerTimer(Sender: TObject);
var
  iBuf: TIdBytes;
begin
  myServer.Progress;
  myClient.Progress;

  if IdTCPClient1.Connected then
    begin
      // 检查来自服务器的反馈
      if IdTCPClient1.IOHandler.InputBuffer.Size > 0 then
        begin
          // 当我们收到反馈，我们打印服务器的反馈
          IdTCPClient1.IOHandler.InputBuffer.ExtractToBytes(iBuf);
          IdTCPClient1.IOHandler.InputBuffer.Clear;
          DoStatus(format('response ', []), @iBuf[0], length(iBuf), 16);
        end;
    end;

  if myClient.Connected then
    begin
      if myClient.myBuffer.Size > 0 then
        begin
          DoStatus(format('response ', []), myClient.myBuffer.Memory, myClient.myBuffer.Size, 16);
          myClient.myBuffer.Clear;
        end;
    end;
end;

procedure TCustomProtocolForm.SendDataOnIndyButtonClick(Sender: TObject);
var
  d: UInt64;
begin
  d := ($ABCDEF1234567890);
  // 我们用indy接口往服务器发送一个uint变量
  IdTCPClient1.IOHandler.WriteBufferOpen;
  // 这里要注意一下:如果开转换参数，indy使用的大端字节序的转换(早期indy版本为了兼容非intel架构的设计)，所以，我们要关闭转换
  IdTCPClient1.IOHandler.Write(d, False);
  IdTCPClient1.IOHandler.WriteBufferFlush;
  IdTCPClient1.IOHandler.WriteBufferClose;
end;

procedure TCustomProtocolForm.SendDataOnZServerButtonClick(Sender: TObject);
var
  d: UInt64;
begin
  d := ($ABCDEF1234567890);
  myClient.WriteBuffer(@d, SizeOf(d));
end;

end.
 
