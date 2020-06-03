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
  // 一般来说,如果使用 TCompute.PostP1 都是为了避免卡主线程, 一旦使用了线程机制, 应该尽量避免在客户端使用WaitSendXXX方法
  TCompute.RunP(procedure(thSender: TCompute)
    var
      busy_: TAtomBool; // 开辟一个状态机,用于检测异步工作是否结束. TAtom开头都是线程安全的变量状态机
    begin
      busy_ := TAtomBool.Create(True); // 开辟一个状态机,用于检测异步工作是否结束. TAtom开头都是线程安全的变量状态机

      // 把线程中的异步方法抛给主线程
      // TThread.Synchronize会出现嵌套式卡死, TCompute.PostXX不会出现这种情况
      TCompute.PostP1(procedure
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
              busy_.V := False;
            end);
          DisposeObject([SendDe]);
        end);

      while busy_.V do
          TCompute.Sleep(1);
      busy_.Free;
      DoStatus('线程结束.');
    end);
end;

procedure TAsyncClientForm.SendBigStreamButtonClick(Sender: TObject);
begin
  // 一般来说,如果使用 TCompute.PostP1 都是为了避免卡主线程, 一旦使用了线程机制, 应该尽量避免在客户端使用WaitSendXXX方法
  TCompute.RunP(procedure(thSender: TCompute)
    var
      busy_: TAtomBool; // 开辟一个状态机,用于检测异步工作是否结束. TAtom开头都是线程安全的变量状态机
    begin
      busy_ := TAtomBool.Create(True); // 开辟一个状态机,用于检测异步工作是否结束. TAtom开头都是线程安全的变量状态机

      // 把线程中的异步方法抛给主线程
      // TThread.Synchronize会出现嵌套式卡死, TCompute.PostXX不会出现这种情况
      TCompute.PostP1(procedure
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

          // waitP是异步等反馈,因为BigStream需要时间完成,我们不能直接做 busy_.V 操作
          client.WaitP(0, procedure(const wState: Boolean)
            begin
              busy_.V := False;
            end);
        end);

      while busy_.V do
          TCompute.Sleep(1);
      busy_.Free;
      DoStatus('线程结束.');
    end);
end;

procedure TAsyncClientForm.SendCompletebufferButtonClick(Sender: TObject);
begin
  // 一般来说,如果使用 TCompute.PostP1 都是为了避免卡主线程, 一旦使用了线程机制, 应该尽量避免在客户端使用WaitSendXXX方法
  TCompute.RunP(procedure(thSender: TCompute)
    var
      busy_: TAtomBool; // 开辟一个状态机,用于检测异步工作是否结束. TAtom开头都是线程安全的变量状态机
    begin
      busy_ := TAtomBool.Create(True); // 开辟一个状态机,用于检测异步工作是否结束. TAtom开头都是线程安全的变量状态机

      // 把线程中的异步方法抛给主线程
      // TThread.Synchronize会出现嵌套式卡死, TCompute.PostXX不会出现这种情况
      TCompute.PostP1(procedure
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

          // waitP是异步等反馈,因为SendCompleteBuffer需要时间完成,我们不能直接做 busy_.V 操作
          client.WaitP(0, procedure(const wState: Boolean)
            begin
              busy_.V := False;
            end);
        end);
      while busy_.V do
          TCompute.Sleep(1);
      busy_.Free;
      DoStatus('线程结束.');
    end);
end;

procedure TAsyncClientForm.sendMiniStreamButtonClick(Sender: TObject);
begin
  // 一般来说,如果使用 TCompute.PostP1 都是为了避免卡主线程, 一旦使用了线程机制, 应该尽量避免在客户端使用WaitSendXXX方法
  TCompute.RunP(procedure(thSender: TCompute)
    var
      busy_: TAtomBool; // 开辟一个状态机,用于检测异步工作是否结束. TAtom开头都是线程安全的变量状态机
    begin
      busy_ := TAtomBool.Create(True); // 开辟一个状态机,用于检测异步工作是否结束. TAtom开头都是线程安全的变量状态机

      // 把线程中的异步方法抛给主线程
      // TThread.Synchronize会出现嵌套式卡死, TCompute.PostXX不会出现这种情况
      TCompute.PostP1(procedure
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

          // waitP是异步等反馈,因为SendDirectStreamCmd需要时间完成,我们不能直接做 busy_.V 操作
          client.WaitP(0, procedure(const wState: Boolean)
            begin
              busy_.V := False;
            end);
        end);
      while busy_.V do
          TCompute.Sleep(1);
      busy_.Free;
      DoStatus('线程结束.');
    end);
end;

procedure TAsyncClientForm.Timer1Timer(Sender: TObject);
begin
  DoStatus();
  client.Progress;
  // 调度主线程队列主循环,要使用异步技术,必须有这个主循环
  // TCompute.ProgressPost主循环是使用原子锁构建的无卡机制,设计思路接近STL
  // TCompute.ProgressPost不会出现TThread.Synchronize卡死情况
  // TCompute.ProgressPost是线程安全的,它会严格按照触发先后顺序执行
  TCompute.ProgressPost;
end;

procedure TAsyncClientForm.ConnectButtonClick(Sender: TObject);
begin
  // ZServer4D本身就是异步框架,多数情况下,直接在主线程操作即可
  // 偶尔我们也需要使用到线程,这里演示了使用状态机机制+TCompute在线程中等异步完成工作,这种机制优于使用WaitSendXXX方法
  TCompute.RunP_NP(procedure
    var
      busy_: TAtomBool; // 开辟一个状态机,用于检测异步工作是否结束. TAtom开头都是线程安全的变量状态机
    begin
      busy_ := TAtomBool.Create(True); // 开辟一个状态机,用于检测异步工作是否结束. TAtom开头都是线程安全的变量状态机

      // 把线程中的异步方法抛给主线程
      // TThread.Synchronize会出现嵌套式卡死, TCompute.PostXX不会出现这种情况
      TCompute.PostP1(procedure
        begin
          client.AsyncConnectP(HostEdit.Text, 9818, procedure(const cState: Boolean)
            begin
              if cState then
                begin
                  DoStatus('链接成功');
                  DoStatus('current client id: %d', [client.ClientIO.ID]);
                end
              else
                  DoStatus('链接失败');
              busy_.V := False;
            end)
        end);

      while busy_.V do
          TCompute.Sleep(1);
      busy_.Free;
      DoStatus('线程结束.');
    end);
end;

end.
