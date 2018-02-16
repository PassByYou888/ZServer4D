{ ****************************************************************************** }
{ * ics support                                                                * }
{ * written by QQ 600585@qq.com                                                * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ ****************************************************************************** }
(*
  update history
*)
unit CommunicationFramework_Server_ICSCustomSocket;

{$I ..\zDefine.inc}

interface

uses
  Messages, Windows,
  SysUtils, Classes,
  OverByteIcsWSocket, OverbyteIcsWinsock;

type
  TCustomICSContextIntf = class(TCustomWSocket)
  public
    property PortNum;
    property Handle;
    property HSocket;
    property BufSize;
    property Text;
    property AllSent;
    property Addr;
    property Port;
    property Proto;
    property LocalAddr;
    property LocalPort;
    property PeerPort;
    property PeerAddr;
    property DnsResult;
    property DnsResultList;
    property State;
    property ReadCount;
    property RcvdCount;
    property LastError;
    property MultiThreaded;
    property MultiCast;
    property MultiCastAddrStr;
    property MultiCastIpTTL;
    property ReuseAddr;
    property ComponentOptions;
    property ListenBacklog;
    property ReqVerLow;
    property ReqVerHigh;

    property OnDataAvailable;
    property OnDataSent;
    property OnSendData;
    property OnSessionClosed;
    property OnSessionAvailable;
    property OnSessionConnected;
    property OnChangeState;

    property OnDnsLookupDone;
    property OnError;
    property OnBgException;
    property SendFlags;
    property LingerOnOff;
    property LingerTimeout;
    property KeepAliveOnOff;
    property KeepAliveTime;
    property KeepAliveInterval;
  end;

  TCustomICSSocketServer = class;
  TCustomICSContext      = class;
  TCustomICSContextClass = class of TCustomICSContext;

  TCustomICSContextCreate  = procedure(Sender: TObject; Client: TCustomICSContext) of object;
  TCustomICSContextConnect = procedure(Sender: TObject; Client: TCustomICSContext; Error: Word) of object;

  TCustomICSContext = class(TCustomICSContextIntf)
  protected
    FServer           : TCustomICSSocketServer;
    FPeerAddr         : string;
    FPeerPort         : string;
    FSessionClosedFlag: Boolean;
  public
    procedure TriggerSessionClosed(ErrCode: Word); override;
    procedure Dup(NewHSocket: TSocket); override;
    function GetPeerAddr: string; override;
    function GetPeerPort: string; override;
    property Server: TCustomICSSocketServer read FServer write FServer;
  end;

  TCustomICSSocketServer = class(TCustomICSContextIntf)
  protected
    FCloseCLOSEDMsgID           : uint;
    FCLIENT_THREAD_PROCESS_MSGID: uint;

    FClientClass       : TCustomICSContextClass;
    FOnClientCreate    : TCustomICSContextCreate;
    FOnClientConnect   : TCustomICSContextConnect;
    FOnClientDisconnect: TCustomICSContextConnect;

    procedure WndProc(var MsgRec: TMessage); override;
    procedure Notification(AComponent: TComponent; operation: TOperation); override;
    procedure TriggerSessionAvailable(Error: Word); override;
    procedure TriggerClientCreate(Client: TCustomICSContext); virtual;
    procedure TriggerClientConnect(Client: TCustomICSContext; Error: Word); virtual;
    procedure TriggerClientDisconnect(Client: TCustomICSContext; Error: Word); virtual;
    procedure WMClientClosed(var msg: TMessage);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    property ClientClass: TCustomICSContextClass read FClientClass write FClientClass;

    property OnClientDisconnect: TCustomICSContextConnect read FOnClientDisconnect write FOnClientDisconnect;
    property OnClientConnect: TCustomICSContextConnect read FOnClientConnect write FOnClientConnect;
    property OnClientCreate: TCustomICSContextCreate read FOnClientCreate write FOnClientCreate;
    property CLIENT_THREAD_PROCESS_MSGID: uint read FCLIENT_THREAD_PROCESS_MSGID;
  end;

function WSAInfo: string;

function WSAIPList: TStrings;

procedure ProcessICSMessages;

implementation

function WSAInfo: string;
var
  _D   : TWSADATA;
  ipLst: TStrings;
begin
  _D := WinsockInfo;

  ipLst := LocalIPList(TSocketFamily.sfAny);

  Result := Format('Version:%D' + #13#10 + 'High Version:%D' + #13#10 + 'Description:%S' + #13#10 + 'System Status:%S' + #13#10 + 'Vendor Information:%S' + #13#10 +
    'Max Sockets:%D' + #13#10 + 'Max UDP:%D' + #13#10 + 'local host name:%s' + #13#10 + 'Local IP list:' + #13#10 + '%s', [
    _D.wVersion, _D.wHighVersion,
    StrPas(_D.szDescription),
    StrPas(_D.szSystemStatus),
    StrPas(_D.lpVendorInfo),
    _D.iMaxSockets,
    _D.iMaxUdpDg,
    LocalHostName,
    ipLst.Text]);

end;

function WSAIPList: TStrings;
begin
  Result := LocalIPList(TSocketFamily.sfAny);
end;

var
  ICSMessageProcessing: Boolean = False;

procedure ProcessICSMessages;
var
  msg: TMsg;
begin
  if ICSMessageProcessing then
      exit;

  ICSMessageProcessing := True;
  try
    while PeekMessage(msg, 0, 0, 0, PM_REMOVE) do
      begin
        try
          TranslateMessage(msg);
          DispatchMessage(msg);
        except
        end;
      end;
  except
  end;
  ICSMessageProcessing := False;
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

constructor TCustomICSSocketServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FClientClass := TCustomICSContext;
  FCloseCLOSEDMsgID := FWndHandler.AllocateMsgHandler(Self);
  FCLIENT_THREAD_PROCESS_MSGID := FWndHandler.AllocateMsgHandler(Self);
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

destructor TCustomICSSocketServer.Destroy;
begin
  { And finally destroy ourself }
  FWndHandler.UnregisterMessage(FCloseCLOSEDMsgID);
  FWndHandler.UnregisterMessage(FCLIENT_THREAD_PROCESS_MSGID);
  inherited Destroy;
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }
{ Message handler }

procedure TCustomICSSocketServer.WndProc(var MsgRec: TMessage);
begin
  with MsgRec do
    begin
      if msg = FCloseCLOSEDMsgID then
        begin
          { We *MUST* handle all exception to avoid application shutdown }
          try
              WMClientClosed(MsgRec)
          except
            on E: Exception do
                HandleBackGroundException(E);
          end;
        end
      else
          inherited WndProc(MsgRec);
    end;
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }
{ Called by destructor when child component (a clients) is create or }
{ destroyed. }

procedure TCustomICSSocketServer.Notification(AComponent: TComponent; operation: TOperation);
begin
  inherited Notification(AComponent, operation);
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }
{ Called when a session is available, that is when a client is connecting }

procedure TCustomICSSocketServer.TriggerSessionAvailable(Error: Word);
var
  Client    : TCustomICSContext;
  NewHSocket: TSocket;
begin
  { Call parent event handler }
  inherited TriggerSessionAvailable(Error);
  { In case of error, do nothing }
  if Error <> 0 then
      exit;

  NewHSocket := Accept;
  Client := FClientClass.Create(Self);
  TriggerClientCreate(Client);
  Client.Server := Self;
  Client.Dup(NewHSocket);
  TriggerClientConnect(Client, Error);
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

procedure TCustomICSSocketServer.TriggerClientConnect(Client: TCustomICSContext; Error: Word);
begin
  if Assigned(FOnClientConnect) then
      FOnClientConnect(Self, Client, Error);
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

procedure TCustomICSSocketServer.TriggerClientCreate(Client: TCustomICSContext);
begin
  if Assigned(FOnClientCreate) then
      FOnClientCreate(Self, Client);
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

procedure TCustomICSSocketServer.TriggerClientDisconnect(Client: TCustomICSContext; Error: Word);
begin
  if Assigned(FOnClientDisconnect) then
      FOnClientDisconnect(Self, Client, Error);
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }
{ Client has closed. Remove it from client list and destroy component. }

procedure TCustomICSSocketServer.WMClientClosed(var msg: TMessage);
var
  Client: TCustomICSContext;
begin
  Client := TCustomICSContext(msg.LParam);
  try
      TriggerClientDisconnect(Client, msg.WParam);
  finally
    { Calling Destroy will automatically remove client from list because }
    { we installed a notification handler. }
      Client.Destroy;
  end;
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }
{ *                                                                           * }
{ *                            TCustomICSContext                                 * }
{ *                                                                           * }
{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }
{ Triggered when socket is closed. Need to inform server socket to update }
{ client list and trigger client disconnect event. }

procedure TCustomICSContext.TriggerSessionClosed(ErrCode: Word);
begin
  if not FSessionClosedFlag then
    begin
      FSessionClosedFlag := True;
      inherited TriggerSessionClosed(ErrCode);
      if Assigned(FServer) then
          PostMessage(Server.Handle, Server.FCloseCLOSEDMsgID, ErrCode, NativeInt(Self));
    end;
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }
{ This override base class GetPeerAddr. It return cached value. }

function TCustomICSContext.GetPeerAddr: string;
begin
  Result := FPeerAddr;
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }
{ This override base class GetPeerPort. It return cached value. }

function TCustomICSContext.GetPeerPort: string;
begin
  Result := FPeerPort;
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }
{ Override base class. Dup is called when a client is assigned to a }
{ TWSocket. Assigning HSocket property will call Dup. }

procedure TCustomICSContext.Dup(NewHSocket: TSocket);
begin
  if not(wsoTcpNoDelay in ComponentOptions) then
      ComponentOptions := ComponentOptions + [wsoNoHttp10Tunnel, wsoTcpNoDelay];
  inherited Dup(NewHSocket);
  { Cache PeerAddr value }
  FPeerAddr := inherited GetPeerAddr;
  FPeerPort := inherited GetPeerPort;
end;

end.
