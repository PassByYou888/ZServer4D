program reverse_p2pVM_client;

{$APPTYPE CONSOLE}

{$R *.res}

{
  P2PVM是基于IO工作的虚拟机，它对服务器和客户端模型没有要求，
  简单来说，
  P2PVM服务器和客户端可以工作于物理客户端也能工作于物理服务器
  只要有IO存在，P2PVM就能工作
  我们如果基于P2PVM搭建后台服务器，伸缩空间将会非常灵活，不局限于任何网络环境
}

uses
  SysUtils,
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF MSWINDOWS}
  CoreClasses,
  PascalStrings,
  UnicodeMixedLib,
  CommunicationFramework,
  NotifyObjectBase,
  PhysicsIO,
  DoStatusIO;

{
  该Demo会先创建一个物理服务器，然后当客户端连接后，主动发起p2pVM的握手，当握手成功以后，做2万个p2pVM客户连接
}

var
  P2PVMConnectionDone, P2PVMConnectionWait: Integer;

type
  TMyPhysics_Server_Special = class(TPeerIOUserSpecial)
  public
    MyP2PVM_ClientArray: array of TCommunicationFrameworkWithP2PVM_Client;
    constructor Create(AOwner: TPeerIO); override;
    destructor Destroy; override;
    procedure Progress; override;
    procedure PhysicsVMBuildAuthToken_Result;
  end;

constructor TMyPhysics_Server_Special.Create(AOwner: TPeerIO);
var
  i: Integer;
begin
  inherited;
  Setlength(MyP2PVM_ClientArray, 20000);
  for i := 0 to length(MyP2PVM_ClientArray) - 1 do
    begin
      MyP2PVM_ClientArray[i] := TCommunicationFrameworkWithP2PVM_Client.Create;
      MyP2PVM_ClientArray[i].QuietMode := True;
    end;
end;

destructor TMyPhysics_Server_Special.Destroy;
var
  i: Integer;
begin
  for i := 0 to length(MyP2PVM_ClientArray) - 1 do
      DisposeObject(MyP2PVM_ClientArray[i]);

  Setlength(MyP2PVM_ClientArray, 0);
  inherited;
end;

procedure TMyPhysics_Server_Special.Progress;
var
  i: Integer;
begin
  inherited;
  for i := 0 to length(MyP2PVM_ClientArray) - 1 do
    begin
      if MyP2PVM_ClientArray[i].RemoteInited then
          Inc(P2PVMConnectionDone)
      else
          Inc(P2PVMConnectionWait);
      MyP2PVM_ClientArray[i].Progress;
    end;
end;

procedure TMyPhysics_Server_Special.PhysicsVMBuildAuthToken_Result;
begin
  Owner.OpenP2PVMTunnelP(500000, True, '', procedure(const cState: Boolean)
    var
      i: Integer;
    begin
      Owner.p2pVM.QuietMode := True;
      for i := 0 to length(MyP2PVM_ClientArray) - 1 do
        begin
          Owner.p2pVM.InstallLogicFramework(MyP2PVM_ClientArray[i]);
          MyP2PVM_ClientArray[i].AsyncConnect('::', 99);
        end;
    end);
end;

type
  TMyPhysics_Server = class(TPhysicsServer)
  public
    constructor Create; override;
    procedure DoIOConnectAfter(Sender: TPeerIO); override;
  end;

constructor TMyPhysics_Server.Create;
begin
  inherited;
  UserSpecialClass := TMyPhysics_Server_Special;
end;

procedure TMyPhysics_Server.DoIOConnectAfter(Sender: TPeerIO);
begin
  inherited;
  // 这个事件表示，当物理客户端链接完成
  // 我们使用延迟引擎，抛送一个2秒的后置事件，然后开始握手
  Sender.BuildP2PAuthTokenP(TMyPhysics_Server_Special(Sender.UserSpecial).PhysicsVMBuildAuthToken_Result);
end;

var
  MyPhysics_Server: TMyPhysics_Server;

begin
  MyPhysics_Server := TMyPhysics_Server.Create;

  MyPhysics_Server.StartService('0.0.0.0', 19899);

  while True do
    begin
      P2PVMConnectionDone := 0;
      P2PVMConnectionWait := 0;
      MyPhysics_Server.Progress;
{$IFDEF MSWINDOWS}
      SetConsoleTitle(PWideChar(Format('P2PVM客户端状态 完成连接: %d 半开连接: %d', [P2PVMConnectionDone, P2PVMConnectionWait])));
{$ENDIF MSWINDOWS}
      CoreClasses.CheckThreadSynchronize(10);
    end;

end.
