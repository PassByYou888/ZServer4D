{ ****************************************************************************** }
{ * ics support                                                                * }
{ * written by QQ 600585@qq.com                                                * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
(* https://github.com/PassByYou888/ZServer4D *)
{ ****************************************************************************** }
(*
  update history
*)
unit CommunicationFramework_Client_ICS;

{$I ..\..\zDefine.inc}

interface

uses Windows, SysUtils, Classes, Messages,
  OverByteIcsWSocket,
  CommunicationFramework_Server_ICSCustomSocket,
  CommunicationFramework, CoreClasses, DoStatusIO;

type
  TCommunicationFramework_Client_ICS = class;

  TPeerClientIntfForICS = class(TPeerClient)
  public
    function Context: TCommunicationFramework_Client_ICS;

    function Connected: Boolean; override;
    procedure Disconnect; override;
    procedure SendByteBuffer(buff: PByte; size: Integer); override;
    procedure WriteBufferOpen; override;
    procedure WriteBufferFlush; override;
    procedure WriteBufferClose; override;
    function GetPeerIP: string; override;

    procedure ContinueResultSend; override;
  end;

  TClientICSContextIntf = class(TCustomICSContextIntf)
  end;

  TCommunicationFramework_Client_ICS = class(TCommunicationFrameworkClient)
  protected
    FDriver: TClientICSContextIntf;
    FClient: TPeerClientIntfForICS;

    procedure DataAvailable(Sender: TObject; ErrCode: Word);
    procedure SessionClosed(Sender: TObject; ErrCode: Word);
    procedure SessionConnected(Sender: TObject; ErrCode: Word);
  public
    constructor Create; override;
    destructor Destroy; override;

    function Connect(Host, Port: string; AWaitTimeOut: TTimeTickValue): Boolean; overload;
    function Connect(Host, Port: string): Boolean; overload;
    function Connect(Addr: string; Port: Word): Boolean; overload; override;
    procedure Disconnect; override;
    function Connected: Boolean; override;
    function ClientIO: TPeerClient; override;

    procedure TriggerQueueData(v: PQueueData); override;
    procedure ProgressBackground; override;
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

function TPeerClientIntfForICS.GetPeerIP: string;
begin
  Result := Context.FDriver.Addr;
end;

procedure TPeerClientIntfForICS.ContinueResultSend;
begin
  inherited ContinueResultSend;
  ProcessAllSendCmd(nil, False, False);
end;

procedure TPeerClientIntfForICS.SendByteBuffer(buff: PByte; size: Integer);
begin
  if Connected then
      Context.FDriver.Send(buff, size);
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
  buff     : TBytes;
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
        FClient.ReceivedBuffer.Position := FClient.ReceivedBuffer.size;
        FClient.ReceivedBuffer.Write(buff[0], BuffCount);
        FClient.FillRecvBuffer(nil, False, False);
      except
          FDriver.Close;
      end;
    end;
end;

procedure TCommunicationFramework_Client_ICS.SessionClosed(Sender: TObject; ErrCode: Word);
begin
  DoDisconnect(FClient);
  FClient.Print('client disonnect for %s:%s', [FDriver.Addr, FDriver.Port]);
end;

procedure TCommunicationFramework_Client_ICS.SessionConnected(Sender: TObject; ErrCode: Word);
begin
end;

constructor TCommunicationFramework_Client_ICS.Create;
begin
  inherited Create;
  FDriver := TClientICSContextIntf.Create(nil);
  FDriver.MultiThreaded := False;
  FDriver.OnDataAvailable := DataAvailable;
  FDriver.OnSessionConnected := SessionConnected;
  FDriver.OnSessionClosed := SessionClosed;
  FClient := TPeerClientIntfForICS.Create(Self, Self);
end;

destructor TCommunicationFramework_Client_ICS.Destroy;
begin
  Disconnect;
  // DisposeObject(FDriver);
  inherited Destroy;
end;

function TCommunicationFramework_Client_ICS.Connect(Host, Port: string; AWaitTimeOut: TTimeTickValue): Boolean;
var
  AStopTime: TTimeTickValue;
begin
  Disconnect;

  FDriver.Proto := 'tcp';
  FDriver.Port := Port;
  FDriver.Addr := Host;

  try
      FDriver.Connect;
  except
  end;

  AStopTime := GetTimeTickCount + AWaitTimeOut;

  while not(FDriver.State in [wsConnected]) do
    begin
      ProgressBackground;

      if (GetTimeTickCount >= AStopTime) then
          break;
      if FDriver.State in [wsClosed] then
          break;

      TThread.Sleep(1);
    end;

  Result := FDriver.State in [wsConnected];

  if Result then
      DoConnected(FClient);

  while (not RemoteInited) and (FDriver.State in [wsConnected]) do
    begin
      ProgressBackground;

      if (GetTimeTickCount >= AStopTime) then
          break;
      if FDriver.State in [wsClosed] then
          break;

      TThread.Sleep(1);
    end;

  Result := (RemoteInited);

  if Result then
      FClient.Print('client connected %s:%s', [FDriver.Addr, FDriver.Port]);
end;

function TCommunicationFramework_Client_ICS.Connect(Host, Port: string): Boolean;
begin
  Result := Connect(Host, Port, 5000);
end;

function TCommunicationFramework_Client_ICS.Connect(Addr: string; Port: Word): Boolean;
begin
  Result := Connect(Addr, IntToStr(Port), 5000);
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

function TCommunicationFramework_Client_ICS.ClientIO: TPeerClient;
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
  inherited ProgressBackground;
end;

procedure TCommunicationFramework_Client_ICS.ProgressBackground;
begin
  FClient.ProcessAllSendCmd(nil, False, False);

  inherited ProgressBackground;

  try
      FDriver.ProcessMessages;
  except
  end;
end;

initialization

finalization

end.
