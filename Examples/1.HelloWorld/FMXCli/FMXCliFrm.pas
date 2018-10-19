unit FMXCliFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Edit,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,

  CommunicationFramework_Client_Indy, DataFrameEngine,
  CommunicationFramework, CoreClasses, DoStatusIO, UnicodeMixedLib;

type
  TFMXClientForm = class(TForm)
    Memo1: TMemo;
    connectButton: TButton;
    HostEdit: TEdit;
    HelloWorldBtn: TButton;
    Timer1: TTimer;
    SendMiniStreamButton: TButton;
    Send128MBigStreamButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure connectButtonClick(Sender: TObject);
    procedure HelloWorldBtnClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure SendMiniStreamButtonClick(Sender: TObject);
    procedure Send128MBigStreamButtonClick(Sender: TObject);
  private
    { Private declarations }
    procedure DoStatusNear(AText: string; const ID: Integer);
    procedure BackCall_helloWorld_Stream_Result(Sender: TPeerClient; ResultData: TDataFrameEngine);
  public
    { Public declarations }
    client: TCommunicationFramework_Client_Indy;
  end;

var
  FMXClientForm: TFMXClientForm;

implementation

{$R *.fmx}

{ TFMXClientForm }

procedure TFMXClientForm.BackCall_helloWorld_Stream_Result(Sender: TPeerClient; ResultData: TDataFrameEngine);
begin
  if ResultData.Count > 0 then
      DoStatus('server response:%s', [ResultData.Reader.ReadString]);
end;

procedure TFMXClientForm.connectButtonClick(Sender: TObject);
begin
  // 方法1，阻塞式链接
  // client.Connect(HostEdit.Text, 9818);

  // 方法2，异步高速链接
  client.AsyncConnectP(HostEdit.Text, 9818, procedure(const cState: Boolean)
    begin
      if cState then
          DoStatus('链接成功')
      else
          DoStatus('链接失败');
    end);
end;

procedure TFMXClientForm.DoStatusNear(AText: string; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
  Memo1.GoToTextEnd;
end;

procedure fmx_Indy_ProgressBackgroundProc;
begin
  Application.ProcessMessages;
end;

procedure TFMXClientForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatusNear);
  client := TCommunicationFramework_Client_Indy.Create;
  ProgressBackgroundProc := fmx_Indy_ProgressBackgroundProc;
end;

procedure TFMXClientForm.FormDestroy(Sender: TObject);
begin
  DisposeObject(client);
  DeleteDoStatusHook(self);
end;

procedure TFMXClientForm.HelloWorldBtnClick(Sender: TObject);
var
  SendDe, ResultDE: TDataFrameEngine;
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

end;

procedure TFMXClientForm.Send128MBigStreamButtonClick(Sender: TObject);
var
  ms: TMemoryStream;
  p : PInt64;
  i : Integer;
begin
  // 在ms中包含了16M大型数据，在服务器端相当于执行了1条普通命令
  ms := TMemoryStream.Create;
  ms.Size := 16 * 1024 * 1024;

  DoStatus('创建16M临时大数据流');
  p := ms.Memory;
  for i := 1 to ms.Size div SizeOf(Int64) do
    begin
      p^ := Random(MaxInt);
      inc(p);
    end;

  DoStatus('计算临时大数据流md5');
  DoStatus('md5:' + umlMD5Char(ms.Memory, ms.Size).Text);

  // 往服务器发送一条Big Stream形式的指令
  client.SendBigStream('Test128MBigStream', ms, True);
end;

procedure TFMXClientForm.SendMiniStreamButtonClick(Sender: TObject);
var
  ms    : TMemoryStream;
  SendDe: TDataFrameEngine;
  p     : PInt64;
  i     : Integer;
begin
  // 在SendDE中包含了512k大型数据，在服务器端相当于执行了512条普通命令
  ms := TMemoryStream.Create;
  ms.Size := 512 * 1024;

  p := ms.Memory;
  for i := 1 to ms.Size div SizeOf(Int64) do
    begin
      p^ := Random(MaxInt);
      inc(p);
    end;

  DoStatus(umlMD5Char(ms.Memory, ms.Size).Text);

  // 往服务器发送一条direct stream形式的指令
  SendDe := TDataFrameEngine.Create;
  SendDe.WriteStream(ms);
  client.SendDirectStreamCmd('TestMiniStream', SendDe);
  DisposeObject([SendDe, ms]);
end;

procedure TFMXClientForm.Timer1Timer(Sender: TObject);
begin
  client.Progress;
end;

end.
