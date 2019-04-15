{ ****************************************************************************** }
{ * DIOCP Support                                                              * }
{ * written by QQ 600585@qq.com                                                * }
{ * https://zpascal.net                                                        * }
{ * https://github.com/PassByYou888/zAI                                        * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/PascalString                               * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zChinese                                   * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/FFMPEG-Header                              * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/InfiniteIoT                                * }
{ * https://github.com/PassByYou888/FastMD5                                    * }
{ ****************************************************************************** }
(*
  update history
*)
unit CommunicationFramework_Client_DIOCP;

{$INCLUDE ..\zDefine.inc}

interface

uses SysUtils, Classes,
  PascalStrings,
  CommunicationFramework, CoreClasses, UnicodeMixedLib, MemoryStream64, NotifyObjectBase,
  diocp_tcp_client;

type
  TDIOCPClient_PeerIO = class;
  TCommunicationFramework_Client_DIOCP = class;

  TIocpClientContextIntf_WithDCli = class(TIocpRemoteContext)
  private
    Link: TDIOCPClient_PeerIO;
    OwnerFramework: TCommunicationFramework_Client_DIOCP;
  protected
    procedure OnConnected; override;
    procedure OnDisconnected; override;
    procedure OnConnectFail; override;
    procedure OnRecvBuffer(Buf: Pointer; Len: Cardinal; ErrCode: Word); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TDIOCPClient_PeerIO = class(TPeerIO)
  private
    Link: TIocpClientContextIntf_WithDCli;
    SendingStream: TMemoryStream64;
    CanTriggerDoDisconnect: Boolean;
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
    DCIntf: TIocpClientContextIntf_WithDCli;

    FOnAsyncConnectNotifyCall: TStateCall;
    FOnAsyncConnectNotifyMethod: TStateMethod;
    FOnAsyncConnectNotifyProc: TStateProc;
  protected
    procedure DCDoConnected(Sender: TIocpClientContextIntf_WithDCli);
    procedure DCDoDisconnect(Sender: TIocpClientContextIntf_WithDCli);
    procedure DCDoConnectFailed(Sender: TIocpClientContextIntf_WithDCli);
    procedure DCDoRecvBuffer(Buf: Pointer; Len: Cardinal; ErrCode: Word);

    procedure DoConnected(Sender: TPeerIO); override;
    procedure DoDisconnect(Sender: TPeerIO); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure TriggerDoConnectFailed; override;
    procedure TriggerDoConnectFinished; override;

    function Connected: Boolean; override;
    function ClientIO: TPeerIO; override;
    procedure TriggerQueueData(v: PQueueData); override;
    procedure Progress; override;

    procedure AsyncConnectC(addr: SystemString; Port: Word; OnResult: TStateCall); overload; override;
    procedure AsyncConnectM(addr: SystemString; Port: Word; OnResult: TStateMethod); overload; override;
    procedure AsyncConnectP(addr: SystemString; Port: Word; OnResult: TStateProc); overload; override;

    function Connect(addr: SystemString; Port: Word): Boolean; overload; override;
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

procedure TIocpClientContextIntf_WithDCli.OnRecvBuffer(Buf: Pointer; Len: Cardinal; ErrCode: Word);
begin
  OwnerFramework.DCDoRecvBuffer(Buf, Len, ErrCode);
  inherited OnRecvBuffer(Buf, Len, ErrCode);
end;

constructor TIocpClientContextIntf_WithDCli.Create;
begin
  inherited Create;
  Link := nil;
end;

destructor TIocpClientContextIntf_WithDCli.Destroy;
begin
  if Link <> nil then
    begin
      Link.Link := nil;
      Link := nil;
    end;
  inherited Destroy;
end;

procedure TDIOCPClient_PeerIO.CreateAfter;
begin
  inherited CreateAfter;
  Link := nil;
  SendingStream := TMemoryStream64.Create;
  CanTriggerDoDisconnect := True;
end;

destructor TDIOCPClient_PeerIO.Destroy;
var
  cintf: TIocpClientContextIntf_WithDCli;
begin
  if Link <> nil then
    begin
      cintf := Link;
      Link := nil;
      cintf.Link := nil;
      if CanTriggerDoDisconnect then
          TCommunicationFramework_Client_DIOCP(FOwnerFramework).DoDisconnect(Self);
      cintf.Close();
    end;

  DisposeObject(SendingStream);
  inherited Destroy;
end;

function TDIOCPClient_PeerIO.Connected: Boolean;
begin
  Result := (Link <> nil) and (Link.Active);
end;

procedure TDIOCPClient_PeerIO.Disconnect;
var
  cintf: TIocpClientContextIntf_WithDCli;
begin
  if Link <> nil then
    begin
      cintf := Link;
      Link := nil;
      cintf.Link := nil;
      if CanTriggerDoDisconnect then
          TCommunicationFramework_Client_DIOCP(FOwnerFramework).DoDisconnect(Self);
      cintf.Close();
    end;
  DisposeObject(Self);
end;

procedure TDIOCPClient_PeerIO.SendByteBuffer(const buff: PByte; const Size: NativeInt);
begin
  if not Connected then
      Exit;

  if Size > 0 then
      SendingStream.WritePtr(buff, Size);
end;

procedure TDIOCPClient_PeerIO.WriteBufferOpen;
begin
  WriteBufferFlush;
end;

procedure TDIOCPClient_PeerIO.WriteBufferFlush;
begin
  if SendingStream.Size > 0 then
    begin
      Link.PostWSASendRequest(SendingStream.Memory, SendingStream.Size, True);
      SendingStream.Clear;
    end;
end;

procedure TDIOCPClient_PeerIO.WriteBufferClose;
begin
  WriteBufferFlush;
end;

function TDIOCPClient_PeerIO.GetPeerIP: SystemString;
begin
  if Connected then
      Result := Link.Host + ' ' + IntToStr(Link.Port)
  else
      Result := '';
end;

procedure TDIOCPClient_PeerIO.Progress;
begin
  inherited Progress;
  ProcessAllSendCmd(nil, False, False);
end;

procedure TCommunicationFramework_Client_DIOCP.DCDoConnected(Sender: TIocpClientContextIntf_WithDCli);
begin
  if Sender.Link <> nil then
    begin
      Sender.Link.Print('connected addr: %s port: %d', [Sender.Host, Sender.Port]);
      DoConnected(Sender.Link);
    end;
end;

procedure TCommunicationFramework_Client_DIOCP.DCDoDisconnect(Sender: TIocpClientContextIntf_WithDCli);
begin
  if Sender.Link <> nil then
    begin
      DisposeObject(Sender.Link);
      Sender.Link := TDIOCPClient_PeerIO.Create(Self, Sender);
      Sender.Link.Link := Sender;
      TriggerDoConnectFailed;
    end;
end;

procedure TCommunicationFramework_Client_DIOCP.DCDoConnectFailed(Sender: TIocpClientContextIntf_WithDCli);
begin
  TriggerDoConnectFailed;
end;

procedure TCommunicationFramework_Client_DIOCP.DCDoRecvBuffer(Buf: Pointer; Len: Cardinal; ErrCode: Word);
begin
  TCoreClassThread.Synchronize(TCoreClassThread.CurrentThread, procedure
    begin
      DCIntf.Link.SaveReceiveBuffer(Buf, Len);
      DCIntf.Link.FillRecvBuffer(TCoreClassThread.CurrentThread, False, False);
    end);
end;

procedure TCommunicationFramework_Client_DIOCP.DoConnected(Sender: TPeerIO);
begin
  inherited DoConnected(Sender);
end;

procedure TCommunicationFramework_Client_DIOCP.DoDisconnect(Sender: TPeerIO);
begin
  inherited DoDisconnect(Sender);
end;

constructor TCommunicationFramework_Client_DIOCP.Create;
begin
  inherited Create;
  FEnabledAtomicLockAndMultiThread := False;

  DIOCPClientPool := TDiocpTcpClient.Create(nil);
  DIOCPClientPool.RegisterContextClass(TIocpClientContextIntf_WithDCli);
  DIOCPClientPool.NoDelayOption := True;
  DIOCPClientPool.DisableAutoConnect := True;
  DIOCPClientPool.TrigerDisconnectEventAfterNoneConnected := False;
  DIOCPClientPool.Open;

  DCIntf := TIocpClientContextIntf_WithDCli(DIOCPClientPool.Add);
  DCIntf.OwnerFramework := Self;
  DCIntf.Link := TDIOCPClient_PeerIO.Create(Self, DCIntf);
  DCIntf.Link.Link := DCIntf;

  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := nil;
  FOnAsyncConnectNotifyProc := nil;
end;

destructor TCommunicationFramework_Client_DIOCP.Destroy;
begin
  Disconnect;
  DIOCPClientPool.Close;
  DisposeObject(DIOCPClientPool);
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

function TCommunicationFramework_Client_DIOCP.ClientIO: TPeerIO;
begin
  Result := DCIntf.Link;
end;

procedure TCommunicationFramework_Client_DIOCP.TriggerQueueData(v: PQueueData);
begin
  if not Connected then
    begin
      DisposeQueueData(v);
      Exit;
    end;

  ClientIO.PostQueueData(v);
  ClientIO.ProcessAllSendCmd(nil, False, False);
end;

procedure TCommunicationFramework_Client_DIOCP.Progress;
begin
  inherited Progress;
  try
      CoreClasses.CheckThreadSynchronize;
  except
  end;
end;

procedure TCommunicationFramework_Client_DIOCP.AsyncConnectC(addr: SystemString; Port: Word; OnResult: TStateCall);
begin
  if (DCIntf.Link <> nil) then
      DCIntf.Link.CanTriggerDoDisconnect := Connected;
  DisposeObject(DCIntf.Link);
  DIOCPClientPool.RemoveAllContext;
  DCIntf := TIocpClientContextIntf_WithDCli(DIOCPClientPool.Add);
  DCIntf.OwnerFramework := Self;
  DCIntf.Link := TDIOCPClient_PeerIO.Create(Self, DCIntf);
  DCIntf.Link.Link := DCIntf;

  FOnAsyncConnectNotifyCall := OnResult;
  FOnAsyncConnectNotifyMethod := nil;
  FOnAsyncConnectNotifyProc := nil;

  DCIntf.Host := addr;
  DCIntf.Port := Port;
  DCIntf.ConnectASync;
end;

procedure TCommunicationFramework_Client_DIOCP.AsyncConnectM(addr: SystemString; Port: Word; OnResult: TStateMethod);
begin
  if (DCIntf.Link <> nil) then
      DCIntf.Link.CanTriggerDoDisconnect := Connected;
  DisposeObject(DCIntf.Link);
  DIOCPClientPool.RemoveAllContext;
  DCIntf := TIocpClientContextIntf_WithDCli(DIOCPClientPool.Add);
  DCIntf.OwnerFramework := Self;
  DCIntf.Link := TDIOCPClient_PeerIO.Create(Self, DCIntf);
  DCIntf.Link.Link := DCIntf;

  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := OnResult;
  FOnAsyncConnectNotifyProc := nil;

  DCIntf.Host := addr;
  DCIntf.Port := Port;
  DCIntf.ConnectASync;
end;

procedure TCommunicationFramework_Client_DIOCP.AsyncConnectP(addr: SystemString; Port: Word; OnResult: TStateProc);
begin
  if (DCIntf.Link <> nil) then
      DCIntf.Link.CanTriggerDoDisconnect := Connected;
  DisposeObject(DCIntf.Link);
  DIOCPClientPool.RemoveAllContext;
  DCIntf := TIocpClientContextIntf_WithDCli(DIOCPClientPool.Add);
  DCIntf.OwnerFramework := Self;
  DCIntf.Link := TDIOCPClient_PeerIO.Create(Self, DCIntf);
  DCIntf.Link.Link := DCIntf;

  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := nil;
  FOnAsyncConnectNotifyProc := OnResult;

  DCIntf.Host := addr;
  DCIntf.Port := Port;
  DCIntf.ConnectASync;
end;

function TCommunicationFramework_Client_DIOCP.Connect(addr: SystemString; Port: Word): Boolean;
var
  t: TTimeTick;
begin
  if (DCIntf.Link <> nil) then
      DCIntf.Link.CanTriggerDoDisconnect := Connected;
  DisposeObject(DCIntf.Link);
  DIOCPClientPool.RemoveAllContext;
  DCIntf := TIocpClientContextIntf_WithDCli(DIOCPClientPool.Add);
  DCIntf.OwnerFramework := Self;
  DCIntf.Link := TDIOCPClient_PeerIO.Create(Self, DCIntf);
  DCIntf.Link.Link := DCIntf;

  Result := False;

  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := nil;
  FOnAsyncConnectNotifyProc := nil;

  DCIntf.Host := addr;
  DCIntf.Port := Port;
  try
      DCIntf.Connect;
  except
    Result := False;
    Exit;
  end;

  t := GetTimeTick + 5000;

  while DCIntf.Active and (not DCIntf.Link.RemoteExecutedForConnectInit) and (GetTimeTick < t) do
      Progress;
end;

procedure TCommunicationFramework_Client_DIOCP.Disconnect;
begin
  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := nil;
  FOnAsyncConnectNotifyProc := nil;

  if (DCIntf.Link <> nil) then
      DCIntf.Link.CanTriggerDoDisconnect := Connected;
  DisposeObject(DCIntf.Link);
  DIOCPClientPool.RemoveAllContext;
  DCIntf := TIocpClientContextIntf_WithDCli(DIOCPClientPool.Add);
  DCIntf.OwnerFramework := Self;
  DCIntf.Link := TDIOCPClient_PeerIO.Create(Self, DCIntf);
  DCIntf.Link.Link := DCIntf;
end;

initialization

finalization

end.
