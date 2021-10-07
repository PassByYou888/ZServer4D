unit EzCliFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  ZJson,
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
  // client := TCommunicationFramework_Client_ICS.Create;
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
  js: TZ_JsonObject;
begin
  {  Send a hello world command in the form of console to the server  }
  client.SendDirectConsoleCmd('helloWorld_Console', '');

  {  Send a hello world instruction in the form of stream to the server  }
  SendDe := TDataFrameEngine.Create;
  SendDe.WriteString('directstream 123456');
  client.SendDirectStreamCmd('helloWorld_Stream', SendDe);
  DisposeObject([SendDe]);

  {  Send and receive stream instructions asynchronously, and the feedback is triggered by method callback  }
  SendDe := TDataFrameEngine.Create;
  SendDe.WriteString('123456');
  client.SendStreamCmdM('helloWorld_Stream_Result', SendDe, BackCall_helloWorld_Stream_Result);
  DisposeObject([SendDe]);

  {  The stream instruction is sent asynchronously and received, and the feedback is triggered by proc callback  }
  SendDe := TDataFrameEngine.Create;
  SendDe.WriteString('123456');
  client.SendStreamCmdP('helloWorld_Stream_Result', SendDe,
    procedure(Sender: TPeerClient; ResultData: TDataFrameEngine)
    begin
      if ResultData.Count > 0 then
          DoStatus('server response:%s', [ResultData.Reader.ReadString]);
    end);
  DisposeObject([SendDe]);

  {  Send in blocking mode and receive stream instruction  }
  SendDe := TDataFrameEngine.Create;
  ResultDE := TDataFrameEngine.Create;
  SendDe.WriteString('123456');
  client.WaitSendStreamCmd('helloWorld_Stream_Result', SendDe, ResultDE, 5000);
  if ResultDE.Count > 0 then
      DoStatus('server response:%s', [ResultDE.Reader.ReadString]);
  DisposeObject([SendDe, ResultDE]);

  {  JSON transceiver  }
  js := TZ_JsonObject.Create;
  js.S['Chinese test'] := 'Hello world';
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
  {  The MS contains 16m large data, which is equivalent to executing a common command on the server  }
  ms := TMemoryStream.Create;
  ms.SetSize(16 * 1024 * 1024);

  DoStatus('Create 16m temporary big data stream');
  p := ms.Memory;
  for i := 1 to ms.Size div SizeOf(Int64) do
    begin
      p^ := Random(MaxInt);
      inc(p);
    end;

  DoStatus('Calculate temporary big data flow MD5');
  DoStatus('bigstream md5:' + umlMD5Char(ms.Memory, ms.Size).Text);

  {  Send an instruction in the form of big stream to the server  }
  client.SendBigStream('Test128MBigStream', ms, True);
end;

procedure TEZClientForm.SendCompletebufferButtonClick(Sender: TObject);
var
  buff: Pointer;
  p: PInt64;
  i: Integer;
begin
  {  The MS contains 16m large data, which is equivalent to executing a common command on the server  }
  buff := GetMemory(16 * 1024 * 1024);

  DoStatus('Create 128M temporary big data stream');
  p := buff;
  for i := 1 to (16 * 1024 * 1024) div SizeOf(Int64) do
    begin
      p^ := Random(MaxInt);
      inc(p);
    end;

  DoStatus('Calculate temporary big data flow MD5');
  DoStatus('complete buffer md5:' + umlMD5String(buff, 16 * 1024 * 1024).Text);

  {  Send an instruction in the form of completebuffer to the server  }
  {  The last Boolean parameter indicates whether to release buff after sending  }
  client.SendCompleteBuffer('TestCompleteBuffer', buff, 16 * 1024 * 1024, True);
end;

procedure TEZClientForm.sendMiniStreamButtonClick(Sender: TObject);
var
  ms: TMemoryStream;
  SendDe: TDataFrameEngine;
  p: PInt64;
  i: Integer;
begin
  {  Sendde contains 4m large data, which is equivalent to 512 ordinary commands on the server side  }
  ms := TMemoryStream.Create;
  ms.SetSize(4 * 1024 * 1024);

  p := ms.Memory;
  for i := 1 to ms.Size div SizeOf(Int64) do
    begin
      p^ := Random(MaxInt);
      inc(p);
    end;

  DoStatus('mini stream md5:' + umlMD5Char(ms.Memory, ms.Size).Text);

  {  Send an instruction in the form of direct stream to the server  }
  SendDe := TDataFrameEngine.Create;
  SendDe.WriteStream(ms);
  client.SendDirectStreamCmd('TestMiniStream', SendDe);
  DisposeObject([SendDe, ms]);
end;

procedure TEZClientForm.Timer1Timer(Sender: TObject);
begin
  CheckThreadSync;
  client.Progress;
end;

procedure TEZClientForm.ConnectButtonClick(Sender: TObject);
begin
  {  Method 1, blocking link  }
  // if client.Connect(HostEdit.Text, 9818) then
  {  Dostatus ('link succeeded ')  }
  // else
  {  Dostatus ('link failed ');  }

  {  Method 2, asynchronous high-speed link  }
  client.AsyncConnectP(HostEdit.Text, 9818, procedure(const cState: Boolean)
    begin
      if cState then
        begin
          DoStatus('Link successful');
          DoStatus('current client id: %d', [client.ClientIO.ID]);
        end
      else
          DoStatus('link failure');
    end);

end;

end.
