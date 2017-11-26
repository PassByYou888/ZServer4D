unit CommunicationFramework_Server_Indy;

{$WARNINGS OFF}
{$HINTS OFF}

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
    RealSend    : Boolean;
    RealSendBuff: TMemoryStream64;

    constructor Create(AOwnerFramework: TCommunicationFramework; AClientIntf: TCoreClassObject); override;
    destructor Destroy; override;

    function Context: TCommunicationFramework_Server_Context;
    procedure ProcesRealSendBuff;

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
    function StartService(Host: string; Port: Word): Boolean;

    procedure Indy_Connect(AContext: TIdContext);
    procedure Indy_ContextCreated(AContext: TIdContext);
    procedure Indy_Disconnect(AContext: TIdContext);
    procedure Indy_Exception(AContext: TIdContext; AException: Exception);

    procedure Indy_Execute(AContext: TIdContext);

    function WaitSendConsoleCmd(Client: TPeerClient; Cmd: string; ConsoleData: string; TimeOut: TTimeTickValue): string; overload; override;
    procedure WaitSendStreamCmd(Client: TPeerClient; Cmd: string; StreamData, ResultData: TDataFrameEngine; TimeOut: TTimeTickValue); overload; override;

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

constructor TContextIntfForServer.Create(AOwnerFramework: TCommunicationFramework; AClientIntf: TCoreClassObject);
begin
  inherited Create(AOwnerFramework, AClientIntf);
  RealSend := False;
  RealSendBuff := TMemoryStream64.Create;
end;

destructor TContextIntfForServer.Destroy;
begin
  DisposeObject(RealSendBuff);
  inherited Destroy;
end;

function TContextIntfForServer.Context: TCommunicationFramework_Server_Context;
begin
  Result := ClientIntf as TCommunicationFramework_Server_Context;
end;

procedure TContextIntfForServer.ProcesRealSendBuff;
begin
  if (RealSend) and (RealSendBuff.size > 0) then
    begin
      LockObject(RealSendBuff);
      try
        Context.Connection.IOHandler.WriteBufferOpen;
        Context.Connection.IOHandler.Write(ToIDBytes(RealSendBuff.Memory, RealSendBuff.size));
        Context.Connection.IOHandler.WriteBufferFlush;
        Context.Connection.IOHandler.WriteBufferClose;
      except
      end;

      RealSendBuff.Clear;
      UnLockObject(RealSendBuff);
    end;
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
      if RealSend then
        begin
          Context.Connection.IOHandler.Write(ToIDBytes(buff, size));
          Context.LastTimeTick := GetTimeTickCount;
        end
      else
        begin
          LockObject(RealSendBuff);
          try
            if RealSendBuff.size > 0 then
                RealSendBuff.Position := RealSendBuff.size;
            RealSendBuff.WritePtr(buff, size);
          except
          end;
          UnLockObject(RealSendBuff);
        end;
    end;
end;

procedure TContextIntfForServer.WriteBufferOpen;
begin
  if RealSend then
      Context.Connection.IOHandler.WriteBufferOpen;
end;

procedure TContextIntfForServer.WriteBufferFlush;
begin
  if RealSend then
      Context.Connection.IOHandler.WriteBufferFlush;
end;

procedure TContextIntfForServer.WriteBufferClose;
begin
  if RealSend then
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
  FDriver.MaxConnections := 20;
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
  try
    while Count > 0 do
      begin
        Items[0].Disconnect;
        CheckSynchronize;
      end;
  except
  end;

  try
    if FDriver.Active then
        FDriver.StopListening;
  except
  end;

  DisposeObject(FDriver);
  inherited Destroy;
end;

procedure TCommunicationFramework_Server_Indy.ProgressBackground;
begin
  inherited ProgressBackground;
end;

procedure TCommunicationFramework_Server_Indy.TriggerQueueData(v: PQueueData);
begin
  v^.Client.PostQueueData(v);
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
      try
          FDriver.Active := False;
      except
      end;

      CheckSynchronize;
    end;
end;

function TCommunicationFramework_Server_Indy.StartService(Host: string; Port: Word): Boolean;
begin
  FDriver.DefaultPort := Port;
  Result := False;
  try
    if not FDriver.Active then
      begin
        FDriver.Active := True;
        Result := True;
      end;
  except
      Result := False;
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
  t: TTimeTickValue;
  c: TCommunicationFramework_Server_Context;
begin
  c := TCommunicationFramework_Server_Context(AContext);

  if c.ClientIntf = nil then
      exit;

  TCoreClassThread.Synchronize(TIdYarnOfThread(AContext.Yarn).Thread,
    procedure
    begin
      c.ClientIntf.RealSend := False;
      c.ClientIntf.Progress;
      c.ClientIntf.RealSend := True;
      c.ClientIntf.ProcesRealSendBuff;
      c.ClientIntf.RealSend := False;
    end);

  try
    c.ClientIntf.ProcessAllSendCmd(TIdYarnOfThread(AContext.Yarn).Thread, True, True);

    t := GetTimeTickCount + 5000;
    while (c.ClientIntf <> nil) and (c.Connection.Connected) and (c.ClientIntf.WaitOnResult) do
      begin
        TCoreClassThread.Synchronize(TIdYarnOfThread(AContext.Yarn).Thread,
          procedure
          begin
            c.ClientIntf.RealSend := False;
            c.ClientIntf.Progress;
            c.ClientIntf.RealSend := True;
            c.ClientIntf.ProcesRealSendBuff;
            c.ClientIntf.RealSend := False;
          end);

        c.Connection.IOHandler.CheckForDataOnSource(10);
        if c.Connection.IOHandler.InputBuffer.size > 0 then
          begin
            t := GetTimeTickCount + 5000;
            c.LastTimeTick := GetTimeTickCount;
            c.ClientIntf.ReceivedBuffer.Position := c.ClientIntf.ReceivedBuffer.size;
            c.Connection.IOHandler.InputBuffer.ExtractToStream(c.ClientIntf.ReceivedBuffer);
            try
                c.ClientIntf.FillRecvBuffer(TIdYarnOfThread(AContext.Yarn).Thread, True, True);
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

  if c.ClientIntf = nil then
      exit;
  c.ClientIntf.RealSend := False;

  try
    c.Connection.IOHandler.CheckForDataOnSource(10);
    while c.Connection.IOHandler.InputBuffer.size > 0 do
      begin
        c.LastTimeTick := GetTimeTickCount;
        c.ClientIntf.ReceivedBuffer.Position := c.ClientIntf.ReceivedBuffer.size;
        c.Connection.IOHandler.InputBuffer.ExtractToStream(c.ClientIntf.ReceivedBuffer);
        try
          c.ClientIntf.FillRecvBuffer(TIdYarnOfThread(AContext.Yarn).Thread, True, True);
          c.Connection.IOHandler.CheckForDataOnSource(10);
        except
          if c.ClientIntf <> nil then
            begin
              DisposeObject(c.ClientIntf);
              c.ClientIntf := nil;
            end;
        end;
      end;
    if c.ClientIntf = nil then
        exit;
  except
    if c.ClientIntf <> nil then
      begin
        DisposeObject(c.ClientIntf);
        c.ClientIntf := nil;
      end;
  end;

  if c.ClientIntf = nil then
      exit;

  c.ClientIntf.RealSend := False;

  TCoreClassThread.Synchronize(TIdYarnOfThread(AContext.Yarn).Thread,
    procedure
    begin
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
    end);
end;

function TCommunicationFramework_Server_Indy.WaitSendConsoleCmd(Client: TPeerClient; Cmd: string; ConsoleData: string; TimeOut: TTimeTickValue): string;
begin
  Result := '';
  RaiseInfo('no Suppport IndyServer');
end;

procedure TCommunicationFramework_Server_Indy.WaitSendStreamCmd(Client: TPeerClient; Cmd: string; StreamData, ResultData: TDataFrameEngine; TimeOut: TTimeTickValue);
begin
  RaiseInfo('no Suppport IndyServer');
end;

end.
