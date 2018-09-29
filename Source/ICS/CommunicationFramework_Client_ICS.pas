{ ****************************************************************************** }
{ * ics support                                                                * }
{ * written by QQ 600585@qq.com                                                * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }
(*
  update history
*)
unit CommunicationFramework_Client_ICS;

{$INCLUDE ..\zDefine.inc}

interface

uses Windows, SysUtils, Classes, Messages,
  PascalStrings,
  OverByteIcsWSocket,
  CommunicationFramework_Server_ICSCustomSocket,
  CommunicationFramework, CoreClasses, DoStatusIO;

type
  TCommunicationFramework_Client_ICS = class;

  TPeerClientIntfForICS = class(TPeerIO)
  public
    function Context: TCommunicationFramework_Client_ICS;

    function Connected: Boolean; override;
    procedure Disconnect; override;
    procedure SendByteBuffer(const buff: PByte; const Size: NativeInt); override;
    procedure WriteBufferOpen; override;
    procedure WriteBufferFlush; override;
    procedure WriteBufferClose; override;
    function GetPeerIP: SystemString; override;

    procedure ContinueResultSend; override;
  end;

  TClientICSContextIntf = class(TCustomICSContextIntf)
  end;

  TCommunicationFramework_Client_ICS = class(TCommunicationFrameworkClient)
  protected
    FDriver: TClientICSContextIntf;
    FClient: TPeerClientIntfForICS;

    FAsyncConnecting: Boolean;
    FOnAsyncConnectNotifyCall: TStateCall;
    FOnAsyncConnectNotifyMethod: TStateMethod;
    FOnAsyncConnectNotifyProc: TStateProc;

    procedure DataAvailable(Sender: TObject; ErrCode: Word);
    procedure SessionClosed(Sender: TObject; ErrCode: Word);
    procedure SessionConnectedAndCreateContext(Sender: TObject; ErrCode: Word);
    procedure AsyncConnect(addr: SystemString; Port: Word; OnResultCall: TStateCall; OnResultMethod: TStateMethod; OnResultProc: TStateProc); overload;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure TriggerDoConnectFailed; override;
    procedure TriggerDoConnectFinished; override;

    procedure AsyncConnectC(addr: SystemString; Port: Word; OnResult: TStateCall); overload; override;
    procedure AsyncConnectM(addr: SystemString; Port: Word; OnResult: TStateMethod); overload; override;
    procedure AsyncConnectP(addr: SystemString; Port: Word; OnResult: TStateProc); overload; override;

    function Connect(Host, Port: SystemString; AWaitTimeOut: TTimeTickValue): Boolean; overload;
    function Connect(Host, Port: SystemString): Boolean; overload;
    function Connect(addr: SystemString; Port: Word): Boolean; overload; override;
    procedure Disconnect; override;

    function Connected: Boolean; override;
    function ClientIO: TPeerIO; override;

    procedure TriggerQueueData(v: PQueueData); override;

    procedure Progress; override;
  end;

implementation

function TPeerClientIntfForICS.Context: TCommunicationFramework_Client_ICS;
begin
  Result := ClientIntf as TCommunicationFramework_Client_ICS;
end;

function TPeerClientIntfForICS.Connected: Boolean;
begin
  Result := Context.Connected;
end;

procedure TPeerClientIntfForICS.Disconnect;
begin
  Context.Disconnect;
end;

function TPeerClientIntfForICS.GetPeerIP: SystemString;
begin
  Result := Context.FDriver.addr;
end;

procedure TPeerClientIntfForICS.ContinueResultSend;
begin
  inherited ContinueResultSend;
  ProcessAllSendCmd(nil, False, False);
end;

procedure TPeerClientIntfForICS.SendByteBuffer(const buff: PByte; const Size: NativeInt);
begin
  if Connected then
      Context.FDriver.Send(buff, Size);
end;

procedure TPeerClientIntfForICS.WriteBufferClose;
begin
  if Connected then
      Context.FDriver.TryToSend;
end;

procedure TPeerClientIntfForICS.WriteBufferFlush;
begin
  if Connected then
      Context.FDriver.TryToSend;
end;

procedure TPeerClientIntfForICS.WriteBufferOpen;
begin
end;

procedure TCommunicationFramework_Client_ICS.DataAvailable(Sender: TObject; ErrCode: Word);
var
  BuffCount: Integer;
  buff: TBytes;
begin
  // increment receive
  BuffCount := FDriver.RcvdCount;
  if BuffCount <= 0 then
      BuffCount := 255 * 255;
  SetLength(buff, BuffCount);
  BuffCount := FDriver.Receive(@buff[0], BuffCount);
  if BuffCount > 0 then
    begin
      try
        FClient.SaveReceiveBuffer(@buff[0], BuffCount);
        FClient.FillRecvBuffer(nil, False, False);
      except
          FDriver.Close;
      end;
    end;
end;

procedure TCommunicationFramework_Client_ICS.SessionClosed(Sender: TObject; ErrCode: Word);
begin
  if FAsyncConnecting then
      TriggerDoConnectFailed;
  FClient.Print('client disonnect for %s:%s', [FDriver.addr, FDriver.Port]);
  DoDisconnect(FClient);
end;

procedure TCommunicationFramework_Client_ICS.SessionConnectedAndCreateContext(Sender: TObject; ErrCode: Word);
begin
  FClient.Print('client connected %s:%s', [FDriver.addr, FDriver.Port]);
  DoConnected(FClient);
end;

procedure TCommunicationFramework_Client_ICS.AsyncConnect(addr: SystemString; Port: Word; OnResultCall: TStateCall; OnResultMethod: TStateMethod; OnResultProc: TStateProc);
begin
  Disconnect;

  FDriver.OnSessionConnected := SessionConnectedAndCreateContext;
  FAsyncConnecting := True;
  FOnAsyncConnectNotifyCall := OnResultCall;
  FOnAsyncConnectNotifyMethod := OnResultMethod;
  FOnAsyncConnectNotifyProc := OnResultProc;

  FDriver.Proto := 'tcp';
  FDriver.Port := IntToStr(Port);
  FDriver.addr := addr;

  try
      FDriver.Connect;
  except
      TriggerDoConnectFailed;
  end;
end;

constructor TCommunicationFramework_Client_ICS.Create;
begin
  inherited Create;
  FDriver := TClientICSContextIntf.Create(nil);
  FDriver.MultiThreaded := False;
  FDriver.KeepAliveOnOff := TSocketKeepAliveOnOff.wsKeepAliveOnCustom;
  FDriver.KeepAliveTime := 1 * 1000;
  FDriver.KeepAliveInterval := 1 * 1000;
  FDriver.OnDataAvailable := DataAvailable;
  FDriver.OnSessionClosed := SessionClosed;
  FClient := TPeerClientIntfForICS.Create(Self, Self);

  FAsyncConnecting := False;
  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := nil;
  FOnAsyncConnectNotifyProc := nil;
end;

destructor TCommunicationFramework_Client_ICS.Destroy;
begin
  Disconnect;
  DisposeObject(FDriver);
  DisposeObject(FClient);
  inherited Destroy;
end;

procedure TCommunicationFramework_Client_ICS.TriggerDoConnectFailed;
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

  FDriver.OnSessionConnected := nil;

  FAsyncConnecting := False;
end;

procedure TCommunicationFramework_Client_ICS.TriggerDoConnectFinished;
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

  FDriver.OnSessionConnected := nil;

  FAsyncConnecting := False;
end;

procedure TCommunicationFramework_Client_ICS.AsyncConnectC(addr: SystemString; Port: Word; OnResult: TStateCall);
begin
  AsyncConnect(addr, Port, OnResult, nil, nil);
end;

procedure TCommunicationFramework_Client_ICS.AsyncConnectM(addr: SystemString; Port: Word; OnResult: TStateMethod);
begin
  AsyncConnect(addr, Port, nil, OnResult, nil);
end;

procedure TCommunicationFramework_Client_ICS.AsyncConnectP(addr: SystemString; Port: Word; OnResult: TStateProc);
begin
  AsyncConnect(addr, Port, nil, nil, OnResult);
end;

function TCommunicationFramework_Client_ICS.Connect(Host, Port: SystemString; AWaitTimeOut: TTimeTickValue): Boolean;
var
  AStopTime: TTimeTickValue;
begin
  Disconnect;

  FDriver.OnSessionConnected := nil;
  FAsyncConnecting := False;
  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := nil;
  FOnAsyncConnectNotifyProc := nil;

  FDriver.Proto := 'tcp';
  FDriver.Port := Port;
  FDriver.addr := Host;

  try
      FDriver.Connect;
  except
    Result := False;
    exit;
  end;

  AStopTime := GetTimeTickCount + AWaitTimeOut;

  while not(FDriver.State in [wsConnected]) do
    begin
      Progress;

      if (GetTimeTickCount >= AStopTime) then
          Break;
      if FDriver.State in [wsClosed] then
          Break;

      TThread.Sleep(1);
    end;

  Result := FDriver.State in [wsConnected];

  if Result then
      DoConnected(FClient);

  while (not RemoteInited) and (FDriver.State in [wsConnected]) do
    begin
      Progress;

      if (GetTimeTickCount >= AStopTime) then
        begin
          FDriver.Close;
          Break;
        end;
      if FDriver.State in [wsClosed] then
          Break;

      TThread.Sleep(1);
    end;

  Result := (RemoteInited);

  if Result then
      FClient.Print('client connected %s:%s', [FDriver.addr, FDriver.Port]);
end;

function TCommunicationFramework_Client_ICS.Connect(Host, Port: SystemString): Boolean;
begin
  Result := Connect(Host, Port, 5000);
end;

function TCommunicationFramework_Client_ICS.Connect(addr: SystemString; Port: Word): Boolean;
begin
  Result := Connect(addr, IntToStr(Port), 5000);
end;

procedure TCommunicationFramework_Client_ICS.Disconnect;
begin
  FDriver.Close;
  DisposeObject(FClient);
  FClient := TPeerClientIntfForICS.Create(Self, Self);
end;

function TCommunicationFramework_Client_ICS.Connected: Boolean;
begin
  Result := (FDriver.State in [wsConnected]);
end;

function TCommunicationFramework_Client_ICS.ClientIO: TPeerIO;
begin
  Result := FClient;
end;

procedure TCommunicationFramework_Client_ICS.TriggerQueueData(v: PQueueData);
begin
  if not Connected then
    begin
      DisposeQueueData(v);
      exit;
    end;

  FClient.PostQueueData(v);
  FClient.ProcessAllSendCmd(nil, False, False);
  inherited Progress;
end;

procedure TCommunicationFramework_Client_ICS.Progress;
begin
  FClient.ProcessAllSendCmd(nil, False, False);

  inherited Progress;

  try
      FDriver.ProcessMessages;
  except
  end;
end;

initialization

finalization

end.
