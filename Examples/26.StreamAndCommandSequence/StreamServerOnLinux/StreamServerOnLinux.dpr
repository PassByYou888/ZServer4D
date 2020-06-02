program StreamServerOnLinux;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  SysUtils,
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF MSWINDOWS}
  CoreClasses,
  PascalStrings,
  UnicodeMixedLib,
  DoStatusIO,
  MemoryStream64,
  DataFrameEngine,
  CommunicationFramework,
  XNATPhysics,
  CommunicationFrameworkDoubleTunnelIO_NoAuth;

{
  演示Stream的接收与发送
}

type
  TSingleTunnelServer_UserSpecial = class(TPeerIOUserSpecial)
  public
    mybigstream: TMemoryStream64;
    mycompletebuffer: TMemoryStream64;
    mydataframebuffer: TMemoryStream64;
    mydataframebuffer_direct: TMemoryStream64;
    constructor Create(AOwner: TPeerIO); override;
    destructor Destroy; override;
  end;

  TSingleTunnelServer = class(TXPhysicsServer)
  public
    procedure cmd_MyBigStreamInit(Sender: TPeerIO; InData: TDataFrameEngine);
    procedure cmd_MyBigStream(Sender: TPeerIO; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64);
    procedure cmd_MyCompleteBuffer(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
    procedure cmd_MyDataframe(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
    procedure cmd_MyDataframe_Direct(Sender: TPeerIO; InData: TDataFrameEngine);

    procedure cmd_1(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
    procedure cmd_2(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
    procedure cmd_3(Sender: TPeerIO; InData, OutData: TDataFrameEngine);

    constructor Create; override;
    destructor Destroy; override;

    procedure DoIOConnectAfter(Sender: TPeerIO); override;
    procedure DoIODisconnect(Sender: TPeerIO); override;
  end;

constructor TSingleTunnelServer_UserSpecial.Create(AOwner: TPeerIO);
begin
  inherited;
  mybigstream := TMemoryStream64.Create;
  mycompletebuffer := TMemoryStream64.Create;
  mydataframebuffer := TMemoryStream64.Create;
  mydataframebuffer_direct := TMemoryStream64.Create;
end;

destructor TSingleTunnelServer_UserSpecial.Destroy;
begin
  DisposeObject(mybigstream);
  DisposeObject(mycompletebuffer);
  DisposeObject(mydataframebuffer);
  DisposeObject(mydataframebuffer_direct);
  inherited;
end;

procedure TSingleTunnelServer.cmd_MyBigStreamInit(Sender: TPeerIO; InData: TDataFrameEngine);
begin
  TSingleTunnelServer_UserSpecial(Sender.UserSpecial).mybigstream.Clear;
end;

procedure TSingleTunnelServer.cmd_MyBigStream(Sender: TPeerIO; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64);
begin
  TSingleTunnelServer_UserSpecial(Sender.UserSpecial).mybigstream.CopyFrom(InData, InData.Size);
end;

procedure TSingleTunnelServer.cmd_MyCompleteBuffer(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
begin
  with TSingleTunnelServer_UserSpecial(Sender.UserSpecial) do
    begin
      mycompletebuffer.Clear;
      mycompletebuffer.WritePtr(InData, DataSize);
    end;
end;

procedure TSingleTunnelServer.cmd_MyDataframe(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
begin
  with TSingleTunnelServer_UserSpecial(Sender.UserSpecial) do
    begin
      mydataframebuffer.Clear;
      InData.Reader.ReadStream(mydataframebuffer);
    end;
end;

procedure TSingleTunnelServer.cmd_MyDataframe_Direct(Sender: TPeerIO; InData: TDataFrameEngine);
begin
  with TSingleTunnelServer_UserSpecial(Sender.UserSpecial) do
    begin
      mydataframebuffer.Clear;
      InData.Reader.ReadStream(mydataframebuffer);
    end;
end;

procedure TSingleTunnelServer.cmd_1(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
begin

end;

procedure TSingleTunnelServer.cmd_2(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
begin

end;

procedure TSingleTunnelServer.cmd_3(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
begin

end;

constructor TSingleTunnelServer.Create;
begin
  inherited;
  MaxCompleteBufferSize := 128 * 1024 * 1024;
  UserSpecialClass := TSingleTunnelServer_UserSpecial;

  RegisterDirectStream('MyBigStreamInit').OnExecute := cmd_MyBigStreamInit;
  RegisterBigStream('MyBigStream').OnExecute := cmd_MyBigStream;
  RegisterCompleteBuffer('MyCompleteBuffer').OnExecute := cmd_MyCompleteBuffer;
  RegisterStream('MyDataframe').OnExecute := cmd_MyDataframe;
  RegisterDirectStream('MyDataframe_Direct').OnExecute := cmd_MyDataframe_Direct;
  RegisterStream('1').OnExecute := cmd_1;
  RegisterStream('2').OnExecute := cmd_2;
  RegisterStream('3').OnExecute := cmd_3;
end;

destructor TSingleTunnelServer.Destroy;
begin
  UnRegisted('MyBigStreamInit');
  UnRegisted('MyBigStream');
  UnRegisted('MyCompleteBuffer');
  UnRegisted('MyDataframe');
  UnRegisted('MyDataframe_Direct');
  UnRegisted('1');
  UnRegisted('2');
  UnRegisted('3');
  inherited;
end;

procedure TSingleTunnelServer.DoIOConnectAfter(Sender: TPeerIO);
begin
  inherited;
  Sender.Print('SingleTunnel connected');
end;

procedure TSingleTunnelServer.DoIODisconnect(Sender: TPeerIO);
begin
  inherited;
  Sender.Print('SingleTunnel disconnect');
end;

type
  TDoubleTunnelServer = class(TCommunicationFramework_DoubleTunnelService_NoAuth)
  protected
    procedure UserLinkSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth); override;
    procedure UserOut(UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth); override;
  public
    procedure cmd_MyBatchStream(Sender: TPeerIO; InData: TDataFrameEngine);
    procedure RegisterCommand; override;
    procedure UnRegisterCommand; override;
  end;

procedure TDoubleTunnelServer.cmd_MyBatchStream(Sender: TPeerIO; InData: TDataFrameEngine);
var
  new_stream, m: TMemoryStream64;
  i: integer;
  rIO: TPeerClientUserDefineForRecvTunnel_NoAuth;
begin
  rIO := GetUserDefineRecvTunnel(Sender);

  for i := 0 to Sender.UserDefine.BigStreamBatchList.Count - 1 do
    begin
      m := Sender.UserDefine.BigStreamBatchList[i]^.Source;
      new_stream := TMemoryStream64.Create;
      new_stream.WritePtr(m.Memory, m.Size);
      PostBatchStream(rIO.SendTunnel.Owner, new_stream, True);
    end;

  rIO.SendTunnel.Owner.SendDirectStreamCmd('MyBatchStreamOver');
  ClearBatchStream(rIO.SendTunnel.Owner);
end;

procedure TDoubleTunnelServer.RegisterCommand;
begin
  inherited;
  RecvTunnel.RegisterDirectStream('MyBatchStream').OnExecute := cmd_MyBatchStream;
end;

procedure TDoubleTunnelServer.UnRegisterCommand;
begin
  inherited;
  RecvTunnel.UnRegisted('MyBatchStream');
end;

procedure TDoubleTunnelServer.UserLinkSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth);
begin
  inherited;
  UserDefineIO.Owner.Print('double tunnel link success');
end;

procedure TDoubleTunnelServer.UserOut(UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth);
begin
  inherited;
  UserDefineIO.Owner.Print('double tunnel user out');
end;

procedure RunServer;
var
  SingleTunnelServer: TSingleTunnelServer;
  DoubleTunnelServer: TDoubleTunnelServer;
begin
  SingleTunnelServer := TSingleTunnelServer.Create;
  if SingleTunnelServer.StartService('0.0.0.0', 19991) then
      DoStatus('single tunnel listen ok')
  else
      RaiseInfo('single tunnel listen failed');

  DoubleTunnelServer := TDoubleTunnelServer.Create(TXPhysicsServer.Create, TXPhysicsServer.Create);
  DoubleTunnelServer.RegisterCommand;

  if DoubleTunnelServer.RecvTunnel.StartService('0.0.0.0', 19992) then
      DoStatus('DoubleTunnelServer listen ok')
  else
      RaiseInfo('DoubleTunnelServer listen failed');

  if DoubleTunnelServer.SendTunnel.StartService('0.0.0.0', 19993) then
      DoStatus('DoubleTunnelServer listen ok')
  else
      RaiseInfo('DoubleTunnelServer listen failed');

  while True do
    begin
      SingleTunnelServer.Progress;
      DoubleTunnelServer.Progress;
      CoreClasses.CheckThreadSynchronize(1);

{$IFDEF MSWINDOWS}
      if SingleTunnelServer.IOBusy or DoubleTunnelServer.RecvTunnel.IOBusy or DoubleTunnelServer.SendTunnel.IOBusy then
          SetConsoleTitle('Busy')
      else
        SetConsoleTitle('IDLE');
{$ENDIF MSWINDOWS}
    end;
end;

begin
  try
      RunServer;
  except
    on E: Exception do
        Writeln(E.ClassName, ': ', E.Message);
  end;

end.
 
