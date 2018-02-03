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
  Windows下一切正常
  优于Delphi没有支持linux->atomic的api，Indy在Linux需要打开zDefine.inc->CriticalSimulateAtomic，用互斥区模拟原子锁和原子内存屏障(跑并行性能会降低)

  Linux下CrossSocket无法处理数据接收，无法通过Debug分析，linux的gdb调试符号有点问题

  后续版本，我有时间后就会修正linux下的高并发问题
*)

type
  // TPeerClientUserSpecial是基于每用户链接后自动创建的实例
  // 使用时候请注意释放内存
  // TPeerClientUserDefine用于Auth,DB等等服务
  // TPeerClientUserSpecial的作用是与高级服务的Auth,DB发生冲突时，对开发者提供独享实例
  TMySpecialDefine = class(TPeerClientUserSpecial)
  public
    tempStream: TMemoryStream64;
    constructor Create(AOwner: TPeerClient); override;
    destructor Destroy; override;
  end;

  // 基于控制台的测试服务器框架，在这里我们可以用indy
  // 经过测试 还算稳定
  TMyServer = class(TCommunicationFramework_Server_Indy)
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
procedure MainLook;
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

  // 基于CrosssSocket官方文档，绑定字符串如果为空，绑定IPV6+IPV4
  if server.StartService('', 9818) then
      DoStatus('start service success')
  else
      DoStatus('start service failed!');

  // 进入主循环
  while true do
      server.ProgressBackground;
end;

begin
  MainLook;

end.
