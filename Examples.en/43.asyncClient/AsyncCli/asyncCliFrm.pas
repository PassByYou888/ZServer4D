unit asyncCliFrm;


interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,

  CoreClasses,
  DoStatusIO,
  Cadencer, DataFrameEngine, UnicodeMixedLib,
  PhysicsIO,
  CommunicationFramework;

type
  TAsyncClientForm = class(TForm)
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
    client: TCommunicationFrameworkClient;
    procedure DoStatusNear(AText: string; const ID: Integer);
    procedure BackCall_helloWorld_Stream_Result(Sender: TPeerClient; ResultData: TDataFrameEngine);
  public
  end;

var
  AsyncClientForm: TAsyncClientForm;

implementation

{$R *.dfm}


procedure TAsyncClientForm.DoStatusNear(AText: string; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
end;

procedure TAsyncClientForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatusNear);
  client := TPhysicsClient.Create;
end;

procedure TAsyncClientForm.FormDestroy(Sender: TObject);
begin
  DisposeObject(client);
  DeleteDoStatusHook(self);
end;

procedure TAsyncClientForm.BackCall_helloWorld_Stream_Result(Sender: TPeerClient; ResultData: TDataFrameEngine);
begin
  if ResultData.Count > 0 then
      DoStatus('server response:%s', [ResultData.Reader.ReadString]);
end;

procedure TAsyncClientForm.HelloWorldBtnClick(Sender: TObject);
begin
  {  Generally speaking, if tcompute.postp1 is used to avoid the main thread of the card, once the threading mechanism is used, the waitsendxxx method should be avoided on the client as much as possible  }
  TCompute.RunP(procedure(thSender: TCompute)
    var
      busy_: TAtomBool; {  Open up a state machine to detect whether asynchronous work ends. Tatom starts with thread safe variable state machines  }
    begin
      busy_ := TAtomBool.Create(True); {  Open up a state machine to detect whether asynchronous work ends. Tatom starts with thread safe variable state machines  }

      {  Throw the asynchronous method in the thread to the main thread  }
      {  Tthread.synchronize will get stuck in nested mode, which will not happen to tcompute.postxx  }
      TCompute.PostP1(procedure
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
              busy_.V := False;
            end);
          DisposeObject([SendDe]);
        end);

      while busy_.V do
          TCompute.Sleep(1);
      busy_.Free;
      DoStatus('End of thread');
    end);
end;

procedure TAsyncClientForm.SendBigStreamButtonClick(Sender: TObject);
begin
  {  Generally speaking, if tcompute.postp1 is used to avoid the main thread of the card, once the threading mechanism is used, the waitsendxxx method should be avoided on the client as much as possible  }
  TCompute.RunP(procedure(thSender: TCompute)
    var
      busy_: TAtomBool; {  Open up a state machine to detect whether asynchronous work ends. Tatom starts with thread safe variable state machines  }
    begin
      busy_ := TAtomBool.Create(True); {  Open up a state machine to detect whether asynchronous work ends. Tatom starts with thread safe variable state machines  }

      {  Throw the asynchronous method in the thread to the main thread  }
      {  Tthread.synchronize will get stuck in nested mode, which will not happen to tcompute.postxx  }
      TCompute.PostP1(procedure
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

          {  Waitp is asynchronous and other feedback, because bigstream needs time to complete, we can't do busy_ V operation  }
          client.WaitP(0, procedure(const wState: Boolean)
            begin
              busy_.V := False;
            end);
        end);

      while busy_.V do
          TCompute.Sleep(1);
      busy_.Free;
      DoStatus('End of thread');
    end);
end;

procedure TAsyncClientForm.SendCompletebufferButtonClick(Sender: TObject);
begin
  {  Generally speaking, if tcompute.postp1 is used to avoid the main thread of the card, once the threading mechanism is used, the waitsendxxx method should be avoided on the client as much as possible  }
  TCompute.RunP(procedure(thSender: TCompute)
    var
      busy_: TAtomBool; {  Open up a state machine to detect whether asynchronous work ends. Tatom starts with thread safe variable state machines  }
    begin
      busy_ := TAtomBool.Create(True); {  Open up a state machine to detect whether asynchronous work ends. Tatom starts with thread safe variable state machines  }

      {  Throw the asynchronous method in the thread to the main thread  }
      {  Tthread.synchronize will get stuck in nested mode, which will not happen to tcompute.postxx  }
      TCompute.PostP1(procedure
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

          {  Waitp is asynchronous feedback. Because sendcompletebuffer takes time to complete, we can't do busy_ V operation  }
          client.WaitP(0, procedure(const wState: Boolean)
            begin
              busy_.V := False;
            end);
        end);
      while busy_.V do
          TCompute.Sleep(1);
      busy_.Free;
      DoStatus('End of thread');
    end);
end;

procedure TAsyncClientForm.sendMiniStreamButtonClick(Sender: TObject);
begin
  {  Generally speaking, if tcompute.postp1 is used to avoid the main thread of the card, once the threading mechanism is used, the waitsendxxx method should be avoided on the client as much as possible  }
  TCompute.RunP(procedure(thSender: TCompute)
    var
      busy_: TAtomBool; {  Open up a state machine to detect whether asynchronous work ends. Tatom starts with thread safe variable state machines  }
    begin
      busy_ := TAtomBool.Create(True); {  Open up a state machine to detect whether asynchronous work ends. Tatom starts with thread safe variable state machines  }

      {  Throw the asynchronous method in the thread to the main thread  }
      {  Tthread.synchronize will get stuck in nested mode, which will not happen to tcompute.postxx  }
      TCompute.PostP1(procedure
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

          {  Waitp is asynchronous and other feedback, because senddirectstreamcmd takes time to complete, we can't do busy_ V operation  }
          client.WaitP(0, procedure(const wState: Boolean)
            begin
              busy_.V := False;
            end);
        end);
      while busy_.V do
          TCompute.Sleep(1);
      busy_.Free;
      DoStatus('End of thread');
    end);
end;

procedure TAsyncClientForm.Timer1Timer(Sender: TObject);
begin
  DoStatus();
  client.Progress;
  {  To schedule the main thread queue main loop, you must have this main loop to use asynchronous technology  }
  {  Tcompute.progresspost main loop is a card free mechanism built with atomic lock, and its design idea is close to STL  }
  {  Tthread.synchronize will not get stuck in tcompute.progresspost  }
  {  Tcompute.progresspost is thread safe. It will be executed in strict order of triggering  }
  TCompute.ProgressPost;
end;

procedure TAsyncClientForm.ConnectButtonClick(Sender: TObject);
begin
  {  Zserver4d itself is an asynchronous framework. In most cases, it can be operated directly on the main thread  }
  {  Occasionally, we also need to use threads. Here we demonstrate that using state machine mechanism + tcompute to complete work asynchronously in threads is better than using waitsendxxx method  }
  TCompute.RunP_NP(procedure
    var
      busy_: TAtomBool; {  Open up a state machine to detect whether asynchronous work ends. Tatom starts with thread safe variable state machines  }
    begin
      busy_ := TAtomBool.Create(True); {  Open up a state machine to detect whether asynchronous work ends. Tatom starts with thread safe variable state machines  }

      {  Throw the asynchronous method in the thread to the main thread  }
      {  Tthread.synchronize will get stuck in nested mode, which will not happen to tcompute.postxx  }
      TCompute.PostP1(procedure
        begin
          client.AsyncConnectP(HostEdit.Text, 9818, procedure(const cState: Boolean)
            begin
              if cState then
                begin
                  DoStatus('Link successful');
                  DoStatus('current client id: %d', [client.ClientIO.ID]);
                end
              else
                  DoStatus('link failure');
              busy_.V := False;
            end)
        end);

      while busy_.V do
          TCompute.Sleep(1);
      busy_.Free;
      DoStatus('End of thread');
    end);
end;

end.

