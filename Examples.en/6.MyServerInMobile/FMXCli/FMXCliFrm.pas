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
  client.Connect(HostEdit.Text, 9818);
end;

procedure TFMXClientForm.DoStatusNear(AText: string; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
end;

procedure TFMXClientForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatusNear);
  client := TCommunicationFramework_Client_Indy.Create;
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

end;

procedure TFMXClientForm.Send128MBigStreamButtonClick(Sender: TObject);
var
  ms: TMemoryStream;
  p : PInt64;
  i : Integer;
begin
  {  The MS contains 128M large data, which is equivalent to executing a common command on the server  }
  ms := TMemoryStream.Create;
  ms.Size := (128 * 1024 * 1024);

  DoStatus('Create 128M temporary big data stream');
  p := ms.Memory;
  for i := 1 to ms.Size div SizeOf(Int64) do
    begin
      p^ := Random(MaxInt);
      inc(p);
    end;

  DoStatus('Calculate temporary big data flow MD5');
  DoStatus('md5:' + umlMD5Char(ms.Memory, ms.Size).Text);

  {  Send an instruction in the form of big stream to the server  }
  client.SendBigStream('Test128MBigStream', ms, True);
end;

procedure TFMXClientForm.SendMiniStreamButtonClick(Sender: TObject);
var
  ms    : TMemoryStream;
  SendDe: TDataFrameEngine;
  p     : PInt64;
  i     : Integer;
begin
  {  Sendde contains 512k large data, which is equivalent to 512 ordinary commands on the server side  }
  ms := TMemoryStream.Create;
  ms.Size := (512 * 1024);

  p := ms.Memory;
  for i := 1 to ms.Size div SizeOf(Int64) do
    begin
      p^ := Random(MaxInt);
      inc(p);
    end;

  DoStatus(umlMD5Char(ms.Memory, ms.Size).Text);

  {  Send an instruction in the form of direct stream to the server  }
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
