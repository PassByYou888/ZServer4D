program EZLinuxServ;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,
  System.Classes,
  CommunicationFramework,
  CommunicationFramework_Server_Indy,
  CommunicationFramework_Server_CrossSocket,
  DoStatusIO, CoreClasses,
  DataFrameEngine, UnicodeMixedLib, MemoryStream64;

(*
  经过实际测试：Linux下indy用系统原子锁会锁不住线程，我们临界区模拟原子锁和内存屏障即可解决问题
  经过实际测试：Linux下crossSocket的高并发问题现在已修复, 技术细节和机制问题我已在CommunicationFramework_Server_CrossSocket.pas中写明

  EzLinuxServ工程使用delphi xe10.1.2所编写
  请更换delphi xe10.2.2或以上版本，如果我们在平台下拉项会找不到linux，就新建一个console工程，将代码复制过去即可
*)

type
  TMySpecialDefine = class(TPeerClientUserSpecial)
  public
    tempStream: TMemoryStream64;
    constructor Create(AOwner: TPeerClient); override;
    destructor Destroy; override;
  end;

  TMyServer = class(TCommunicationFramework_Server_CrossSocket)
  private
    procedure cmd_helloWorld_Console(Sender: TPeerClient; InData: string);
    procedure cmd_helloWorld_Stream(Sender: TPeerClient; InData: TDataFrameEngine);
    procedure cmd_helloWorld_Stream_Result(Sender: TPeerClient; InData, OutData: TDataFrameEngine);

    procedure cmd_TestMiniStream(Sender: TPeerClient; InData: TDataFrameEngine);

    procedure cmd_Test128MBigStream(Sender: TPeerClient; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64);

    procedure cmd_TestCompleteBuffer(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
  public
  end;

constructor TMySpecialDefine.Create(AOwner: TPeerClient);
begin
  inherited Create(AOwner);
  tempStream := TMemoryStream64.Create;
  DoStatus('%s connected', [Owner.PeerIP]);
end;

destructor TMySpecialDefine.Destroy;
begin
  DoStatus('%s disconnect', [Owner.PeerIP]);
  DisposeObject(tempStream);
  inherited Destroy;
end;

procedure TMyServer.cmd_helloWorld_Console(Sender: TPeerClient; InData: string);
begin
  DoStatus('client: %s', [InData]);
end;

procedure TMyServer.cmd_helloWorld_Stream(Sender: TPeerClient; InData: TDataFrameEngine);
begin
  DoStatus('client: %s', [InData.Reader.ReadString]);
end;

procedure TMyServer.cmd_helloWorld_Stream_Result(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
begin
  OutData.WriteString('result 654321');
end;

procedure TMyServer.cmd_TestMiniStream(Sender: TPeerClient; InData: TDataFrameEngine);
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  InData.Reader.ReadStream(ms);

  DoStatus(umlMD5Char(ms.Memory, ms.Size).Text);

  DisposeObject(ms);
end;

procedure TMyServer.cmd_Test128MBigStream(Sender: TPeerClient; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64);
var
  tempStream: TMemoryStream64;
begin
  tempStream := TMySpecialDefine(Sender.UserSpecial).tempStream;
  tempStream.CopyFrom(InData, InData.Size);

  // bigstream complete
  if tempStream.Size = BigStreamTotal then
    begin
      Sender.Print('bigsteram finish');
      Sender.Print('bigsteram md5:' + umlMD5Char(tempStream.Memory, tempStream.Size).Text);
      tempStream.Clear;
    end;
end;

procedure TMyServer.cmd_TestCompleteBuffer(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
begin
  Sender.Print('Complete buffer md5: %s', [umlMD5String(InData, DataSize).Text]);
end;

// 主循环
var
  server: TMyServer;

begin
  server := TMyServer.Create;
  server.PeerClientUserSpecialClass := TMySpecialDefine;

  // 更改最大completeBuffer，这里只用于测试，正常运行服务器，这里一般给4M就可以了
  server.MaxCompleteBufferSize := 128 * 1024 * 1024;

  server.RegisterDirectConsole('helloWorld_Console').OnExecute := server.cmd_helloWorld_Console;
  server.RegisterDirectStream('helloWorld_Stream').OnExecute := server.cmd_helloWorld_Stream;
  server.RegisterStream('helloWorld_Stream_Result').OnExecute := server.cmd_helloWorld_Stream_Result;
  server.RegisterDirectStream('TestMiniStream').OnExecute := server.cmd_TestMiniStream;
  server.RegisterBigStream('Test128MBigStream').OnExecute := server.cmd_Test128MBigStream;
  // 注册Completebuffer指令
  server.RegisterCompleteBuffer('TestCompleteBuffer').OnExecute := server.cmd_TestCompleteBuffer;

  if server.StartService('0.0.0.0', 9818) then
      DoStatus('start service success')
  else
      DoStatus('start service failed!');

  // 进入主循环
  while true do
    begin
      server.Progress;

      // 延迟同步检查
      try
          CoreClasses.CheckThreadSynchronize(10);
      except
      end;
    end;

end.
