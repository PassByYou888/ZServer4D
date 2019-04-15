unit EzCliFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  ZS_JsonDataObjects,
  CommunicationFramework,
  DoStatusIO, CoreClasses,
  CommunicationFramework_Client_CrossSocket,
  CommunicationFramework_Client_ICS,
  Cadencer, DataFrameEngine, UnicodeMixedLib,
  CommunicationFramework_Client_Indy;

type
  TEZClientForm = class(TForm)
    Memo1: TMemo;
    ConnectButton: TButton;
    HostEdit: TLabeledEdit;
    Timer1: TTimer;
    HelloWorldBtn: TButton;
    sendMiniStreamButton: TButton;
    SendBigStreamButton: TButton;
    SendCompletebufferButton: TButton;
    procedure ConnectButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure HelloWorldBtnClick(Sender: TObject);
    procedure sendMiniStreamButtonClick(Sender: TObject);
    procedure SendBigStreamButtonClick(Sender: TObject);
    procedure SendCompletebufferButtonClick(Sender: TObject);
  private
    { Private declarations }
    procedure DoStatusNear(AText: string; const ID: Integer);
    procedure BackCall_helloWorld_Stream_Result(Sender: TPeerClient; ResultData: TDataFrameEngine);
  public
    { Public declarations }
    client: TCommunicationFrameworkClient;
  end;

var
  EZClientForm: TEZClientForm;

implementation

{$R *.dfm}


procedure TEZClientForm.DoStatusNear(AText: string; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
end;

procedure TEZClientForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatusNear);
  client := TCommunicationFramework_Client_CrossSocket.Create;
end;

procedure TEZClientForm.FormDestroy(Sender: TObject);
begin
  DisposeObject(client);
  DeleteDoStatusHook(self);
end;

procedure TEZClientForm.BackCall_helloWorld_Stream_Result(Sender: TPeerClient; ResultData: TDataFrameEngine);
begin
  if ResultData.Count > 0 then
      DoStatus('server response:%s', [ResultData.Reader.ReadString]);
end;

procedure TEZClientForm.HelloWorldBtnClick(Sender: TObject);
var
  SendDe, ResultDE: TDataFrameEngine;
  js: TJsonObject;
begin
  // 往服务器发送一条console形式的hello world指令
  client.SendDirectConsoleCmd('helloWorld_Console', '');

  // 往服务器发送一条stream形式的hello world指令
  SendDe := TDataFrameEngine.Create;
  SendDe.WriteString('directstream 123456');
  client.SendDirectStreamCmd('helloWorld_Stream', SendDe);
  DisposeObject([SendDe]);

  // 异步方式发送，并且接收Stream指令，反馈以方法回调触发
  SendDe := TDataFrameEngine.Create;
  SendDe.WriteString('123456');
  client.SendStreamCmdM('helloWorld_Stream_Result', SendDe, BackCall_helloWorld_Stream_Result);
  DisposeObject([SendDe]);

  // 异步方式发送，并且接收Stream指令，反馈以proc回调触发
  SendDe := TDataFrameEngine.Create;
  SendDe.WriteString('123456');
  client.SendStreamCmdP('helloWorld_Stream_Result', SendDe,
    procedure(Sender: TPeerClient; ResultData: TDataFrameEngine)
    begin
      if ResultData.Count > 0 then
          DoStatus('server response:%s', [ResultData.Reader.ReadString]);
    end);
  DisposeObject([SendDe]);

  // 阻塞方式发送，并且接收Stream指令
  SendDe := TDataFrameEngine.Create;
  ResultDE := TDataFrameEngine.Create;
  SendDe.WriteString('123456');
  client.WaitSendStreamCmd('helloWorld_Stream_Result', SendDe, ResultDE, 5000);
  if ResultDE.Count > 0 then
      DoStatus('server response:%s', [ResultDE.Reader.ReadString]);
  DisposeObject([SendDe, ResultDE]);

  // json收发
  js := TJsonObject.Create;
  js.S['中文测试'] := '你好世界';
  SendDe := TDataFrameEngine.Create;
  SendDe.WriteJson(js);
  client.SendDirectStreamCmd('Json_Stream', SendDe);
  DisposeObject([js, SendDe]);
end;

procedure TEZClientForm.SendBigStreamButtonClick(Sender: TObject);
var
  ms: TMemoryStream;
  p: PInt64;
  i: Integer;
begin
  // 在ms中包含了16M大型数据，在服务器端相当于执行了1条普通命令
  ms := TMemoryStream.Create;
  ms.SetSize(16 * 1024 * 1024);

  DoStatus('创建16M临时大数据流');
  p := ms.Memory;
  for i := 1 to ms.Size div SizeOf(Int64) do
    begin
      p^ := Random(MaxInt);
      inc(p);
    end;

  DoStatus('计算临时大数据流md5');
  DoStatus('bigstream md5:' + umlMD5Char(ms.Memory, ms.Size).Text);

  // 往服务器发送一条Big Stream形式的指令
  client.SendBigStream('Test128MBigStream', ms, True);
end;

procedure TEZClientForm.SendCompletebufferButtonClick(Sender: TObject);
var
  buff: Pointer;
  p: PInt64;
  i: Integer;
begin
  // 在ms中包含了16M大型数据，在服务器端相当于执行了1条普通命令
  buff := GetMemory(16 * 1024 * 1024);

  DoStatus('创建128M临时大数据流');
  p := buff;
  for i := 1 to (16 * 1024 * 1024) div SizeOf(Int64) do
    begin
      p^ := Random(MaxInt);
      inc(p);
    end;

  DoStatus('计算临时大数据流md5');
  DoStatus('complete buffer md5:' + umlMD5String(buff, 16 * 1024 * 1024).Text);

  // 往服务器发送一条CompleteBuffer形式的指令
  // 最后的布尔参数表示是否在完成发送后释放buff
  client.SendCompleteBuffer('TestCompleteBuffer', buff, 16 * 1024 * 1024, True);
end;

procedure TEZClientForm.sendMiniStreamButtonClick(Sender: TObject);
var
  ms: TMemoryStream;
  SendDe: TDataFrameEngine;
  p: PInt64;
  i: Integer;
begin
  // 在SendDE中包含了4M大型数据，在服务器端相当于执行了512条普通命令
  ms := TMemoryStream.Create;
  ms.SetSize(4 * 1024 * 1024);

  p := ms.Memory;
  for i := 1 to ms.Size div SizeOf(Int64) do
    begin
      p^ := Random(MaxInt);
      inc(p);
    end;

  DoStatus('mini stream md5:' + umlMD5Char(ms.Memory, ms.Size).Text);

  // 往服务器发送一条direct stream形式的指令
  SendDe := TDataFrameEngine.Create;
  SendDe.WriteStream(ms);
  client.SendDirectStreamCmd('TestMiniStream', SendDe);
  DisposeObject([SendDe, ms]);
end;

procedure TEZClientForm.Timer1Timer(Sender: TObject);
begin
  client.Progress;
end;

procedure TEZClientForm.ConnectButtonClick(Sender: TObject);
begin
  // 方法1，阻塞式链接
  // if client.Connect(HostEdit.Text, 9818) then
  // DoStatus('链接成功')
  // else
  // DoStatus('链接失败');

  // 方法2，异步高速链接
  client.AsyncConnectP(HostEdit.Text, 9818, procedure(const cState: Boolean)
    begin
      if cState then
        begin
          DoStatus('链接成功');
          DoStatus('current client id: %d', [client.ClientIO.ID]);
        end
      else
          DoStatus('链接失败');
    end);

end;

end.
