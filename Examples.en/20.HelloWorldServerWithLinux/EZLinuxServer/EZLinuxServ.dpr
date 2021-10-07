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

{  After practical testing: Indy can't lock threads with the system atomic lock under Linux. We can solve the problem by simulating the atomic lock and memory barrier in the critical area
After practical testing: the high concurrency problem of crosssocket under Linux has been repaired. I have found the technical details and mechanism problems in the communication framework_ Server_ Crosssocket.pas
Ezlinuxserv project is written in Delphi xe10.1.2
Please replace Delphi xe10.2.2 or above. If we can't find Linux in the platform drop-down item, create a new console project and copy the code  }

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

{  Main cycle  }
var
  server: TMyServer;

begin
  server := TMyServer.Create;
  server.PeerClientUserSpecialClass := TMySpecialDefine;

  {  Change the maximum completebuffer. It is only used for testing. The server runs normally. It is generally 4m here  }
  server.MaxCompleteBufferSize := 128 * 1024 * 1024;

  server.RegisterDirectConsole('helloWorld_Console').OnExecute := server.cmd_helloWorld_Console;
  server.RegisterDirectStream('helloWorld_Stream').OnExecute := server.cmd_helloWorld_Stream;
  server.RegisterStream('helloWorld_Stream_Result').OnExecute := server.cmd_helloWorld_Stream_Result;
  server.RegisterDirectStream('TestMiniStream').OnExecute := server.cmd_TestMiniStream;
  server.RegisterBigStream('Test128MBigStream').OnExecute := server.cmd_Test128MBigStream;
  {  Register the completebuffer directive  }
  server.RegisterCompleteBuffer('TestCompleteBuffer').OnExecute := server.cmd_TestCompleteBuffer;

  if server.StartService('0.0.0.0', 9818) then
      DoStatus('start service success')
  else
      DoStatus('start service failed!');

  {  Enter the main cycle  }
  while true do
    begin
      server.Progress;

      {  Delay synchronization check  }
      try
          CoreClasses.CheckThreadSynchronize(10);
      except
      end;
    end;

end.
