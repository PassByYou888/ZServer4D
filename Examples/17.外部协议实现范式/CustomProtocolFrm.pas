unit CustomProtocolFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,

  CommunicationFramework, PascalStrings,
  CommunicationFramework_Server_CrossSocket, DoStatusIO, MemoryStream64, CoreClasses,

  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdGlobal;

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
    procedure FillCustomBuffer(Sender: TPeerClient; const Th: TCoreClassThread; const Buffer: PByte; const Size: NativeInt; var Done: Boolean); override;
  end;

  TCustomProtocolForm = class(TForm)
    Memo: TMemo;
    Timer: TTimer;
    Panel1: TPanel;
    connectButton: TButton;
    WriteStringButton: TButton;
    IdTCPClient1: TIdTCPClient;
    procedure FormCreate(Sender: TObject);
    procedure connectButtonClick(Sender: TObject);
    procedure WriteStringButtonClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    myServer: TMyServer;
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

procedure TMyServer.FillCustomBuffer(Sender: TPeerClient; const Th: TCoreClassThread; const Buffer: PByte; const Size: NativeInt; var Done: Boolean);
begin
  // 从服务器获取外部定制化处理缓冲区接口
  // 这里的buffer全部是碎片化缓冲区

  // 我们做自己的定制化协议 done要设置为是true
  Done := True;

  if Size <= 0 then
      exit;

  // 根据线程状态判断是否同步化处理碎片缓冲区
  if Th <> nil then
      Th.Synchronize(Th,
      procedure
      begin
        // 我们将碎片缓冲区追加写入到myBuffer
        TMyPeerClientUserSpecial(Sender.UserSpecial).myBuffer.WritePtr(Buffer, Size);
      end)
  else
    begin
      // 我们将碎片缓冲区追加写入到myBuffer
      TMyPeerClientUserSpecial(Sender.UserSpecial).myBuffer.WritePtr(Buffer, Size);
    end;
end;

procedure TCustomProtocolForm.connectButtonClick(Sender: TObject);
begin
  IdTCPClient1.Connect;

  if IdTCPClient1.Connected then
      DoStatus('connect ok!');
end;

procedure TCustomProtocolForm.DoStatusMethod(AText: SystemString; const ID: Integer);
begin
  Memo.Lines.Add(AText);
end;

procedure TCustomProtocolForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(Self, DoStatusMethod);
  myServer := TMyServer.Create;
  // 指定客户端的p2p实例接口
  myServer.PeerClientUserSpecialClass := TMyPeerClientUserSpecial;
  myServer.StartService('', 9989);
end;

procedure TCustomProtocolForm.TimerTimer(Sender: TObject);
var
  iBuf: TIdBytes;
begin
  myServer.ProgressBackground;

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
end;

procedure TCustomProtocolForm.WriteStringButtonClick(Sender: TObject);
var
  d: UInt64;
begin
  d := $FFFFFF1234567890;
  // 我们用indy接口往服务器发送一个uint变量
  IdTCPClient1.IOHandler.WriteBufferOpen;
  IdTCPClient1.IOHandler.Write(d);
  IdTCPClient1.IOHandler.WriteBufferFlush;
  IdTCPClient1.IOHandler.WriteBufferClose;
end;

end.
