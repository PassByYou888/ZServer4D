program reverse_p2pVM_client;

{$APPTYPE CONSOLE}

{$R *.res}

{  P2P VM is a virtual machine based on Io. It has no requirements for server and client models,
In short,
The P2P VM server and client can work on the physical client as well as the physical server
As long as IO exists, P2P VM can work
If we build a background server based on P2P VM, the expansion space will be very flexible and not limited to any network environment  }

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

{  The demo will first create a physical server, and then actively initiate the p2pvm handshake after the client is connected. After the handshake is successful, 20000 p2pvm client connections will be made  }

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
  {  This event indicates when the physical client link is completed  }
  {  We use the delay engine, throw a 2-second post event, and then start shaking hands  }
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
      SetConsoleTitle(PWideChar(Format('P2P VM client status completed connection: %d half open connection: %d', [P2PVMConnectionDone, P2PVMConnectionWait])));
{$ENDIF MSWINDOWS}
      CoreClasses.CheckThreadSynchronize(10);
    end;

end.
