unit CommunicationFramework_Client_DIOCP;

{$I ..\zDefine.inc}

interface

uses SysUtils, Classes,
  PascalStrings,
  CommunicationFramework, CoreClasses, UnicodeMixedLib, MemoryStream64, NotifyObjectBase,
  diocp_tcp_client;

type
  TPeerIOWithDIOCPClient               = class;
  TCommunicationFramework_Client_DIOCP = class;

  TIocpClientContextIntf_WithDCli = class(TIocpRemoteContext)
  private
    Link          : TPeerIOWithDIOCPClient;
    OwnerFramework: TCommunicationFramework_Client_DIOCP;
  protected
    procedure OnConnected; override;
    procedure OnDisconnected; override;
    procedure OnConnectFail; override;
    procedure OnRecvBuffer(buf: Pointer; len: Cardinal; ErrCode: WORD); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TPeerIOWithDIOCPClient = class(TPeerClient)
  private
    Link         : TIocpClientContextIntf_WithDCli;
    SendingStream: TMemoryStream64;
  public
    procedure CreateAfter; override;
    destructor Destroy; override;

    function Connected: Boolean; override;
    procedure Disconnect; override;
    procedure SendByteBuffer(const buff: PByte; const Size: NativeInt); override;
    procedure WriteBufferOpen; override;
    procedure WriteBufferFlush; override;
    procedure WriteBufferClose; override;
    function GetPeerIP: SystemString; override;
    procedure Progress; override;
  end;

  TCommunicationFramework_Client_DIOCP = class(TCommunicationFrameworkClient)
  private
    DIOCPClientPool: TDiocpTcpClient;
    DCIntf         : TIocpClientContextIntf_WithDCli;

    FOnAsyncConnectNotifyCall  : TStateCall;
    FOnAsyncConnectNotifyMethod: TStateMethod;
    FOnAsyncConnectNotifyProc  : TStateProc;
  protected
    procedure DCDoConnected(Sender: TIocpClientContextIntf_WithDCli);
    procedure DCDoDisconnect(Sender: TIocpClientContextIntf_WithDCli);
    procedure DCDoConnectFailed(Sender: TIocpClientContextIntf_WithDCli);
    procedure DCDoRecvBuffer(buf: Pointer; len: Cardinal; ErrCode: WORD);

    procedure DoConnected(Sender: TPeerClient); override;
    procedure DoDisconnect(Sender: TPeerClient); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure TriggerDoConnectFailed; override;
    procedure TriggerDoConnectFinished; override;

    function Connected: Boolean; override;
    function ClientIO: TPeerClient; override;
    procedure TriggerQueueData(v: PQueueData); override;
    procedure ProgressBackground; override;

    procedure AsyncConnect(Addr: SystemString; Port: WORD; OnResult: TStateCall); overload; override;
    procedure AsyncConnect(Addr: SystemString; Port: WORD; OnResult: TStateMethod); overload; override;
    procedure AsyncConnect(Addr: SystemString; Port: WORD; OnResult: TStateProc); overload; override;

    function Connect(Addr: SystemString; Port: WORD): Boolean; overload; override;
    procedure Disconnect; override;
  end;

implementation

procedure TIocpClientContextIntf_WithDCli.OnConnected;
begin
  inherited OnConnected;
  OwnerFramework.DCDoConnected(Self);
end;

procedure TIocpClientContextIntf_WithDCli.OnDisconnected;
begin
  inherited OnDisconnected;
  OwnerFramework.DCDoDisconnect(Self);
end;

procedure TIocpClientContextIntf_WithDCli.OnConnectFail;
begin
  inherited OnConnectFail;
  OwnerFramework.DCDoConnectFailed(Self);
end;

procedure TIocpClientContextIntf_WithDCli.OnRecvBuffer(buf: Pointer; len: Cardinal; ErrCode: WORD);
begin
  OwnerFramework.DCDoRecvBuffer(buf, len, ErrCode);
  inherited OnRecvBuffer(buf, len, ErrCode);
end;

constructor TIocpClientContextIntf_WithDCli.Create;
begin
  inherited Create;
  Link := nil;
end;

destructor TIocpClientContextIntf_WithDCli.Destroy;
var
  peerio: TPeerIOWithDIOCPClient;
begin
  if Link <> nil then
    begin
      peerio := Link;
      Link := nil;
      DisposeObject(peerio);
    end;
  inherited Destroy;
end;

procedure TPeerIOWithDIOCPClient.CreateAfter;
begin
  inherited CreateAfter;
  Link := nil;
  SendingStream := TMemoryStream64.Create;
end;

destructor TPeerIOWithDIOCPClient.Destroy;
var
  cintf: TIocpClientContextIntf_WithDCli;
begin
  if Link <> nil then
    begin
      cintf := Link;
      Link := nil;
      DisposeObject(cintf);
    end;

  DisposeObject(SendingStream);
  inherited Destroy;
end;

function TPeerIOWithDIOCPClient.Connected: Boolean;
begin
  Result := (Link <> nil) and (Link.Active);
end;

procedure TPeerIOWithDIOCPClient.Disconnect;
begin
  if Link <> nil then
      Link.Close;
end;

procedure TPeerIOWithDIOCPClient.SendByteBuffer(const buff: PByte; const Size: NativeInt);
begin
  if not Connected then
      exit;

  if Size > 0 then
      SendingStream.WritePtr(buff, Size);
end;

procedure TPeerIOWithDIOCPClient.WriteBufferOpen;
begin
  WriteBufferFlush;
end;

procedure TPeerIOWithDIOCPClient.WriteBufferFlush;
begin
  if SendingStream.Size > 0 then
    begin
      Link.PostWSASendRequest(SendingStream.Memory, SendingStream.Size, True);
      SendingStream.Clear;
    end;
end;

procedure TPeerIOWithDIOCPClient.WriteBufferClose;
begin
  WriteBufferFlush;
end;

function TPeerIOWithDIOCPClient.GetPeerIP: SystemString;
begin
  if Connected then
      Result := Link.host + ' ' + IntToStr(Link.Port)
  else
      Result := '';
end;

procedure TPeerIOWithDIOCPClient.Progress;
begin
  inherited Progress;
  ProcessAllSendCmd(nil, False, False);
end;

procedure TCommunicationFramework_Client_DIOCP.DCDoConnected(Sender: TIocpClientContextIntf_WithDCli);
begin
  Sender.Link.Print('connected addr: %s port: %d', [Sender.host, Sender.Port]);
  DoConnected(Sender.Link);
end;

procedure TCommunicationFramework_Client_DIOCP.DCDoDisconnect(Sender: TIocpClientContextIntf_WithDCli);
begin
  Sender.Link.Print('disconnect with %s port: %d', [Sender.host, Sender.Port]);
  DoDisconnect(Sender.Link);
  TriggerDoConnectFailed;
end;

procedure TCommunicationFramework_Client_DIOCP.DCDoConnectFailed(Sender: TIocpClientContextIntf_WithDCli);
begin
  Sender.Link.Print('connect failed form addr: %s port: %d', [Sender.host, Sender.Port]);
  TriggerDoConnectFailed;
end;

procedure TCommunicationFramework_Client_DIOCP.DCDoRecvBuffer(buf: Pointer; len: Cardinal; ErrCode: WORD);
begin
  DCIntf.Link.SaveReceiveBuffer(buf, len);
  DCIntf.Link.FillRecvBuffer(TThread.CurrentThread, True, True);
end;

procedure TCommunicationFramework_Client_DIOCP.DoConnected(Sender: TPeerClient);
begin
  inherited DoConnected(Sender);
end;

procedure TCommunicationFramework_Client_DIOCP.DoDisconnect(Sender: TPeerClient);
begin
  inherited DoDisconnect(Sender);
end;

constructor TCommunicationFramework_Client_DIOCP.Create;
begin
  inherited Create;

  DIOCPClientPool := TDiocpTcpClient.Create(nil);
  DIOCPClientPool.RegisterContextClass(TIocpClientContextIntf_WithDCli);
  DIOCPClientPool.NoDelayOption := True;

  DCIntf := TIocpClientContextIntf_WithDCli(DIOCPClientPool.Add);
  DCIntf.OwnerFramework := Self;
  DCIntf.Link := TPeerIOWithDIOCPClient.Create(Self, DCIntf);
  DCIntf.Link.Link := DCIntf;

  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := nil;
  FOnAsyncConnectNotifyProc := nil;
end;

destructor TCommunicationFramework_Client_DIOCP.Destroy;
begin
  Disconnect;
  DisposeObject(DCIntf);
  inherited Destroy;
end;

procedure TCommunicationFramework_Client_DIOCP.TriggerDoConnectFailed;
begin
  inherited TriggerDoConnectFailed;

  try
    if Assigned(FOnAsyncConnectNotifyCall) then
        FOnAsyncConnectNotifyCall(False);
    if Assigned(FOnAsyncConnectNotifyMethod) then
        FOnAsyncConnectNotifyMethod(False);
    if Assigned(FOnAsyncConnectNotifyProc) then
        FOnAsyncConnectNotifyProc(False);
  except
  end;

  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := nil;
  FOnAsyncConnectNotifyProc := nil;
end;

procedure TCommunicationFramework_Client_DIOCP.TriggerDoConnectFinished;
begin
  inherited TriggerDoConnectFinished;

  try
    if Assigned(FOnAsyncConnectNotifyCall) then
        FOnAsyncConnectNotifyCall(True);
    if Assigned(FOnAsyncConnectNotifyMethod) then
        FOnAsyncConnectNotifyMethod(True);
    if Assigned(FOnAsyncConnectNotifyProc) then
        FOnAsyncConnectNotifyProc(True);
  except
  end;

  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := nil;
  FOnAsyncConnectNotifyProc := nil;
end;

function TCommunicationFramework_Client_DIOCP.Connected: Boolean;
begin
  Result := (ClientIO <> nil) and (ClientIO.Connected);
end;

function TCommunicationFramework_Client_DIOCP.ClientIO: TPeerClient;
begin
  Result := DCIntf.Link;
end;

procedure TCommunicationFramework_Client_DIOCP.TriggerQueueData(v: PQueueData);
begin
  if not Connected then
    begin
      DisposeQueueData(v);
      exit;
    end;

  ClientIO.PostQueueData(v);
  ClientIO.ProcessAllSendCmd(nil, False, False);
end;

procedure TCommunicationFramework_Client_DIOCP.ProgressBackground;
begin
  inherited ProgressBackground;
  CheckSynchronize;
end;

procedure TCommunicationFramework_Client_DIOCP.AsyncConnect(Addr: SystemString; Port: WORD; OnResult: TStateCall);
begin
  DCIntf.Link.Link := nil;
  DisposeObject(DCIntf.Link);
  DCIntf.Link := nil;
  Disconnect;

  DCIntf := TIocpClientContextIntf_WithDCli(DIOCPClientPool.Add);
  DCIntf.OwnerFramework := Self;
  DCIntf.Link := TPeerIOWithDIOCPClient.Create(Self, DCIntf);
  DCIntf.Link.Link := DCIntf;

  FOnAsyncConnectNotifyCall := OnResult;
  FOnAsyncConnectNotifyMethod := nil;
  FOnAsyncConnectNotifyProc := nil;

  DCIntf.host := Addr;
  DCIntf.Port := Port;
  DCIntf.ConnectASync;
end;

procedure TCommunicationFramework_Client_DIOCP.AsyncConnect(Addr: SystemString; Port: WORD; OnResult: TStateMethod);
begin
  DCIntf.Link.Link := nil;
  DisposeObject(DCIntf.Link);
  DCIntf.Link := nil;
  Disconnect;

  DCIntf := TIocpClientContextIntf_WithDCli(DIOCPClientPool.Add);
  DCIntf.OwnerFramework := Self;
  DCIntf.Link := TPeerIOWithDIOCPClient.Create(Self, DCIntf);
  DCIntf.Link.Link := DCIntf;

  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := OnResult;
  FOnAsyncConnectNotifyProc := nil;

  DCIntf.host := Addr;
  DCIntf.Port := Port;
  DCIntf.ConnectASync;
end;

procedure TCommunicationFramework_Client_DIOCP.AsyncConnect(Addr: SystemString; Port: WORD; OnResult: TStateProc);
begin
  DCIntf.Link.Link := nil;
  DisposeObject(DCIntf.Link);
  DCIntf.Link := nil;
  Disconnect;

  DCIntf := TIocpClientContextIntf_WithDCli(DIOCPClientPool.Add);
  DCIntf.OwnerFramework := Self;
  DCIntf.Link := TPeerIOWithDIOCPClient.Create(Self, DCIntf);
  DCIntf.Link.Link := DCIntf;

  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := nil;
  FOnAsyncConnectNotifyProc := OnResult;

  DCIntf.host := Addr;
  DCIntf.Port := Port;
  DCIntf.ConnectASync;
end;

function TCommunicationFramework_Client_DIOCP.Connect(Addr: SystemString; Port: WORD): Boolean;
var
  t: TTimeTickValue;
begin
  DCIntf.Link.Link := nil;
  DisposeObject(DCIntf.Link);
  DCIntf.Link := nil;
  Disconnect;

  DCIntf := TIocpClientContextIntf_WithDCli(DIOCPClientPool.Add);
  DCIntf.OwnerFramework := Self;
  DCIntf.Link := TPeerIOWithDIOCPClient.Create(Self, DCIntf);
  DCIntf.Link.Link := DCIntf;

  DIOCPClientPool.Open;

  Result := False;

  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := nil;
  FOnAsyncConnectNotifyProc := nil;

  DCIntf.host := Addr;
  DCIntf.Port := Port;
  try
      DCIntf.Connect;
  except
    Result := False;
    exit;
  end;

  t := GetTimeTick + 5000;

  while DCIntf.Active and (not DCIntf.Link.RemoteExecutedForConnectInit) and (GetTimeTick < t) do
      ProgressBackground;
end;

procedure TCommunicationFramework_Client_DIOCP.Disconnect;
begin
  if Connected then
      ClientIO.Disconnect;

  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := nil;
  FOnAsyncConnectNotifyProc := nil;
end;

initialization

finalization

end.
