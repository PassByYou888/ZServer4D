unit CommunicationFramework_Server_Indy;

interface

uses CommunicationFramework,
  CoreClasses,
  DataFrameEngine, ListEngine, MemoryStream64,

  Classes, SysUtils,

  IdCustomTCPServer, IdTCPServer, IdYarn, IdSchedulerOfThread,
  IDGlobal, IdBaseComponent, IdComponent, IdTCPConnection, IdContext;

type
  TCommunicationFramework_Server_Context = class;

  TContextIntfForServer = class(TPeerClient)
  public
    function Context: TCommunicationFramework_Server_Context;
    function Connected: Boolean; override;
    procedure Disconnect; override;
    procedure SendByteBuffer(buff: PByte; size: Integer); override;
    procedure WriteBufferOpen; override;
    procedure WriteBufferFlush; override;
    procedure WriteBufferClose; override;
    function GetPeerIP: string; override;
  end;

  TCommunicationFramework_Server_Context = class(TIdServerContext)
    ClientIntf: TContextIntfForServer;
    LastTimeTick: TTimeTickValue;
    constructor Create(AConnection: TIdTCPConnection; AYarn: TIdYarn; AList: TIdContextThreadList = nil); override;
    destructor Destroy; override;
  end;

  TCommunicationFramework_Server_Indy = class(TCommunicationFrameworkServer)
  protected
    FDriver: TIdTCPServer;
  protected
    function GetPort: Word;
    procedure SetPort(const Value: Word);
    procedure SetIdleTimeout(const Value: TTimeTickValue); override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ProgressBackground; override;
    procedure TriggerQueueData(v: PQueueData); override;
    property Port: Word read GetPort write SetPort;
    procedure StartListening;
    procedure StopListening;

    procedure Indy_Connect(AContext: TIdContext);
    procedure Indy_ContextCreated(AContext: TIdContext);
    procedure Indy_Disconnect(AContext: TIdContext);
    procedure Indy_Exception(AContext: TIdContext; AException: Exception);
    procedure Indy_Execute(AContext: TIdContext);

    property Driver: TIdTCPServer read FDriver;
  end;

implementation

uses UnicodeMixedLib, PascalStrings, DoStatusIO;

function ToIDBytes(p: PByte; size: Integer): TIdBytes;
var
  i: Integer;
begin
  SetLength(Result, size);
  for i := 0 to size - 1 do
    begin
      Result[i] := p^;
      inc(p);
    end;
end;

function TContextIntfForServer.Context: TCommunicationFramework_Server_Context;
begin
  Result := ClientIntf as TCommunicationFramework_Server_Context;
end;

function TContextIntfForServer.Connected: Boolean;
begin
  if Context <> nil then
      Result := Context.Connection.Connected
  else
      Result := False;
end;

procedure TContextIntfForServer.Disconnect;
begin
  Context.Connection.Disconnect;
end;

procedure TContextIntfForServer.SendByteBuffer(buff: PByte; size: Integer);
begin
  if size > 0 then
    begin
      Context.Connection.IOHandler.Write(ToIDBytes(buff, size));
      Context.LastTimeTick := GetTimeTickCount;
    end;
end;

procedure TContextIntfForServer.WriteBufferOpen;
begin
  Context.Connection.IOHandler.WriteBufferOpen;
end;

procedure TContextIntfForServer.WriteBufferFlush;
begin
  Context.Connection.IOHandler.WriteBufferFlush;
end;

procedure TContextIntfForServer.WriteBufferClose;
begin
  Context.Connection.IOHandler.WriteBufferClose;
end;

function TContextIntfForServer.GetPeerIP: string;
begin
  Result := Context.Binding.PeerIP;
end;

constructor TCommunicationFramework_Server_Context.Create(AConnection: TIdTCPConnection; AYarn: TIdYarn; AList: TIdContextThreadList);
begin
  inherited Create(AConnection, AYarn, AList);
  LastTimeTick := GetTimeTickCount;
end;

destructor TCommunicationFramework_Server_Context.Destroy;
begin
  if ClientIntf <> nil then
    begin
      DisposeObject(ClientIntf);
      ClientIntf := nil;
    end;
  inherited Destroy;
end;

function TCommunicationFramework_Server_Indy.GetPort: Word;
begin
  Result := FDriver.DefaultPort;
end;

procedure TCommunicationFramework_Server_Indy.SetPort(const Value: Word);
begin
  FDriver.DefaultPort := Value;
end;

procedure TCommunicationFramework_Server_Indy.SetIdleTimeout(const Value: TTimeTickValue);
begin
  inherited SetIdleTimeout(Value);
  FDriver.TerminateWaitTime := IdleTimeout;
end;

constructor TCommunicationFramework_Server_Indy.Create;
begin
  inherited Create;
  FDriver := TIdTCPServer.Create(nil);
  FDriver.UseNagle := False;
  FDriver.MaxConnections := 200;
  FDriver.ContextClass := TCommunicationFramework_Server_Context;
  FDriver.TerminateWaitTime := IdleTimeout;

  FDriver.OnConnect := Indy_Connect;
  FDriver.OnContextCreated := Indy_ContextCreated;
  FDriver.OnDisconnect := Indy_Disconnect;
  FDriver.OnException := Indy_Exception;
  FDriver.OnExecute := Indy_Execute;
end;

destructor TCommunicationFramework_Server_Indy.Destroy;
begin
  DisposeObject(FDriver);
  inherited Destroy;
end;

procedure TCommunicationFramework_Server_Indy.ProgressBackground;
begin
  inherited ProgressBackground;
end;

procedure TCommunicationFramework_Server_Indy.TriggerQueueData(v: PQueueData);
begin
  if Exists(v^.Client) then
      v^.Client.PostQueueData(v)
  else
      DisposeQueueData(v);
end;

procedure TCommunicationFramework_Server_Indy.StartListening;
begin
  if not FDriver.Active then
    begin
      FDriver.Active := True;
    end;
end;

procedure TCommunicationFramework_Server_Indy.StopListening;
begin
  if FDriver.Active then
    begin
      FDriver.Active := False;
    end;
end;

procedure TCommunicationFramework_Server_Indy.Indy_Connect(AContext: TIdContext);
var
  c: TCommunicationFramework_Server_Context;
begin
  c := TCommunicationFramework_Server_Context(AContext);
  c.ClientIntf := TContextIntfForServer.Create(Self, c);
  DoConnected(c.ClientIntf);
end;

procedure TCommunicationFramework_Server_Indy.Indy_ContextCreated(AContext: TIdContext);
var
  c: TCommunicationFramework_Server_Context;
begin
  c := TCommunicationFramework_Server_Context(AContext);
end;

procedure TCommunicationFramework_Server_Indy.Indy_Disconnect(AContext: TIdContext);
var
  c: TCommunicationFramework_Server_Context;
begin
  c := TCommunicationFramework_Server_Context(AContext);
  DoDisconnect(c.ClientIntf);
  if c.ClientIntf <> nil then
    begin
      DisposeObject(c.ClientIntf);
      c.ClientIntf := nil;
    end;
end;

procedure TCommunicationFramework_Server_Indy.Indy_Exception(AContext: TIdContext; AException: Exception);
var
  c: TCommunicationFramework_Server_Context;
begin
  c := TCommunicationFramework_Server_Context(AContext);
  if c.ClientIntf <> nil then
    begin
      DisposeObject(c.ClientIntf);
      c.ClientIntf := nil;
    end;
end;

procedure TCommunicationFramework_Server_Indy.Indy_Execute(AContext: TIdContext);
var
  c: TCommunicationFramework_Server_Context;
  t: TTimeTickValue;
begin
  c := TCommunicationFramework_Server_Context(AContext);
  if c.ClientIntf = nil then
      Exit;

  try
    c.Connection.IOHandler.CheckForDataOnSource(1);
    while c.Connection.IOHandler.InputBuffer.size > 0 do
      begin
        c.LastTimeTick := GetTimeTickCount;
        c.ClientIntf.ReceivedBuffer.Position := c.ClientIntf.ReceivedBuffer.size;
        c.Connection.IOHandler.InputBuffer.ExtractToStream(c.ClientIntf.ReceivedBuffer);
        try
          c.ClientIntf.FillRecvBuffer(TIdYarnOfThread(AContext.Yarn).Thread, True, False);
          c.Connection.IOHandler.CheckForDataOnSource(0);
        except
          if c.ClientIntf <> nil then
            begin
              DisposeObject(c.ClientIntf);
              c.ClientIntf := nil;
            end;
        end;
      end;

    if c.ClientIntf = nil then
        Exit;

    try
      c.ClientIntf.ProcessAllSendCmd(TIdYarnOfThread(AContext.Yarn).Thread, True, False);
      t := GetTimeTickCount + 15000;
      while (c.ClientIntf <> nil) and (c.Connection.Connected) and (c.ClientIntf.WaitOnResult) do
        begin
          c.Connection.IOHandler.CheckForDataOnSource(0);
          if c.Connection.IOHandler.InputBuffer.size > 0 then
            begin
              t := GetTimeTickCount + 15000;
              c.LastTimeTick := GetTimeTickCount;
              c.ClientIntf.ReceivedBuffer.Position := c.ClientIntf.ReceivedBuffer.size;
              c.Connection.IOHandler.InputBuffer.ExtractToStream(c.ClientIntf.ReceivedBuffer);
              try
                  c.ClientIntf.FillRecvBuffer(TIdYarnOfThread(AContext.Yarn).Thread, True, False);
              except
                if c.ClientIntf <> nil then
                  begin
                    DisposeObject(c.ClientIntf);
                    c.ClientIntf := nil;
                  end;
              end;
            end
          else if GetTimeTickCount > t then
            begin
              if c.ClientIntf <> nil then
                begin
                  DisposeObject(c.ClientIntf);
                  c.ClientIntf := nil;
                end;
              break;
            end;
        end;
    except
      if c.ClientIntf <> nil then
        begin
          DisposeObject(c.ClientIntf);
          c.ClientIntf := nil;
        end;
    end;
  except
    if c.ClientIntf <> nil then
      begin
        DisposeObject(c.ClientIntf);
        c.ClientIntf := nil;
      end;
  end;

  if c.ClientIntf = nil then
      Exit;

  if IdleTimeout > 0 then
    begin
      if GetTimeTickCount - c.LastTimeTick > IdleTimeout then
        begin
          if c.ClientIntf <> nil then
            begin
              DisposeObject(c.ClientIntf);
              c.ClientIntf := nil;
            end;
        end;
    end;
end;

end.
