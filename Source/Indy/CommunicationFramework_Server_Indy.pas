{ ****************************************************************************** }
{ * IndyInterface                                                              * }
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
  INDY Server的最大连接被限制到20
  update history
*)
unit CommunicationFramework_Server_Indy;
(*
  update history
*)

{$INCLUDE ..\zDefine.inc}

interface

uses CommunicationFramework,
  CoreClasses,
  PascalStrings,
  DataFrameEngine, ListEngine, MemoryStream64,

  Classes, SysUtils,

  IdCustomTCPServer, IdTCPServer, IdYarn, IdSchedulerOfThread, IdSocketHandle,
  IDGlobal, IdBaseComponent, IdComponent, IdTCPConnection, IdContext;

type
  TCommunicationFramework_Server_Context = class;

  TIDServer_PeerIO = class(TPeerIO)
  public
    RealSendBuff: TMemoryStream64;

    procedure CreateAfter; override;
    destructor Destroy; override;

    function Context: TCommunicationFramework_Server_Context;
    procedure ProcesRealSendBuff;

    function Connected: Boolean; override;
    procedure Disconnect; override;
    procedure SendByteBuffer(const buff: PByte; const Size: NativeInt); override;
    procedure WriteBufferOpen; override;
    procedure WriteBufferFlush; override;
    procedure WriteBufferClose; override;
    function GetPeerIP: SystemString; override;
  end;

  TCommunicationFramework_Server_Context = class(TIdServerContext)
  public
    ClientIntf: TIDServer_PeerIO;
    LastTimeTick: TTimeTick;
    constructor Create(AConnection: TIdTCPConnection; AYarn: TIdYarn; AList: TIdContextThreadList = nil); override;
    destructor Destroy; override;
  end;

  TCommunicationFramework_Server_Indy = class(TCommunicationFrameworkServer)
  protected
    FDriver: TIdTCPServer;

    function GetPort: Word;
    procedure SetPort(const Value: Word);
    procedure SetIdleTimeout(const Value: TTimeTick); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Progress; override;

    procedure TriggerQueueData(v: PQueueData); override;

    property Port: Word read GetPort write SetPort;

    procedure StopService; override;
    function StartService(Host: SystemString; Port: Word): Boolean; override;

    procedure Indy_Connect(AContext: TIdContext);
    procedure Indy_ContextCreated(AContext: TIdContext);
    procedure Indy_Disconnect(AContext: TIdContext);
    procedure Indy_Exception(AContext: TIdContext; AException: Exception);

    procedure Indy_Execute(AContext: TIdContext);

    function WaitSendConsoleCmd(p_io: TPeerIO; const Cmd, ConsoleData: SystemString; Timeout: TTimeTick): SystemString; override;
    procedure WaitSendStreamCmd(p_io: TPeerIO; const Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; Timeout: TTimeTick); override;

    property driver: TIdTCPServer read FDriver;
  end;

implementation

uses UnicodeMixedLib, DoStatusIO;

function ToIDBytes(p: PByte; Size: Integer): TIdBytes;
begin
  SetLength(Result, Size);
  CopyPtr(p, @Result[0], Size);
end;

procedure TIDServer_PeerIO.CreateAfter;
begin
  inherited CreateAfter;
  RealSendBuff := TMemoryStream64.Create;
end;

destructor TIDServer_PeerIO.Destroy;
begin
  DisposeObject(RealSendBuff);
  inherited Destroy;
end;

function TIDServer_PeerIO.Context: TCommunicationFramework_Server_Context;
begin
  Result := IOInterface as TCommunicationFramework_Server_Context;
end;

procedure TIDServer_PeerIO.ProcesRealSendBuff;
var
  buff: ^TIdBytes;
begin
  new(buff);
  SetLength(buff^, 0);
  TCoreClassThread.Synchronize(TCoreClassThread.CurrentThread, procedure
    begin
      if RealSendBuff.Size > 0 then
        begin
          SetLength(buff^, RealSendBuff.Size);
          CopyPtr(RealSendBuff.Memory, @buff^[0], RealSendBuff.Size);
          RealSendBuff.Clear;
        end;
    end);

  if length(buff^) > 0 then
    begin
      Context.Connection.IOHandler.WriteBufferOpen;
      Context.Connection.IOHandler.write(buff^);
      Context.Connection.IOHandler.WriteBufferFlush;
      Context.Connection.IOHandler.WriteBufferClose;
      SetLength(buff^, 0);
    end;
  Dispose(buff);
end;

function TIDServer_PeerIO.Connected: Boolean;
begin
  if Context <> nil then
      Result := Context.Connection.Connected
  else
      Result := False;
end;

procedure TIDServer_PeerIO.Disconnect;
begin
  Context.Connection.Disconnect;
  inherited Disconnect;
end;

procedure TIDServer_PeerIO.SendByteBuffer(const buff: PByte; const Size: NativeInt);
begin
  if Size > 0 then
    begin
      RealSendBuff.Position := RealSendBuff.Size;
      RealSendBuff.WritePtr(buff, Size);
    end;
end;

procedure TIDServer_PeerIO.WriteBufferOpen;
begin
end;

procedure TIDServer_PeerIO.WriteBufferFlush;
begin
end;

procedure TIDServer_PeerIO.WriteBufferClose;
begin
end;

function TIDServer_PeerIO.GetPeerIP: SystemString;
begin
  Result := Context.Binding.PeerIP;
end;

constructor TCommunicationFramework_Server_Context.Create(AConnection: TIdTCPConnection; AYarn: TIdYarn; AList: TIdContextThreadList);
begin
  inherited Create(AConnection, AYarn, AList);
  LastTimeTick := GetTimeTick;
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

procedure TCommunicationFramework_Server_Indy.SetIdleTimeout(const Value: TTimeTick);
begin
  inherited SetIdleTimeout(Value);
  FDriver.TerminateWaitTime := IdleTimeout;
end;

constructor TCommunicationFramework_Server_Indy.Create;
begin
  inherited CreateCustomHashPool(128);
  FEnabledAtomicLockAndMultiThread := False;

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
  ProgressPeerIOP(procedure(cli: TPeerIO)
    begin
      try
          cli.Disconnect;
      except
      end;
    end);

  try
    while Count > 0 do
      begin
        CheckThreadSynchronize(1);
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

procedure TCommunicationFramework_Server_Indy.Progress;
begin
  inherited Progress;
end;

procedure TCommunicationFramework_Server_Indy.TriggerQueueData(v: PQueueData);
var
  c: TPeerIO;
begin
  c := PeerIO[v^.IO_ID];
  if c <> nil then
      c.PostQueueData(v)
  else
      DisposeQueueData(v);
end;

procedure TCommunicationFramework_Server_Indy.StopService;
begin
  if FDriver.Active then
    begin
      try
          FDriver.Active := False;
      except
      end;

      CheckThreadSynchronize;
    end;
end;

function TCommunicationFramework_Server_Indy.StartService(Host: SystemString; Port: Word): Boolean;
var
  bIP: TIdSocketHandle;
begin
  FDriver.Bindings.Clear;
  bIP := FDriver.Bindings.Add;
  bIP.IP := Host;
  if IsIPV6(Host) then
      bIP.IPVersion := TIdIPVersion.Id_IPv6
  else
      bIP.IPVersion := TIdIPVersion.Id_IPv4;

  bIP.Port := Port;

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
begin
  TCoreClassThread.Synchronize(TIdYarnOfThread(AContext.Yarn).Thread,
    procedure
    var
      c: TCommunicationFramework_Server_Context;
    begin
      c := TCommunicationFramework_Server_Context(AContext);
      c.ClientIntf := TIDServer_PeerIO.Create(Self, c);
      c.Binding.SetKeepAliveValues(True, 2000, 2);
    end);
end;

procedure TCommunicationFramework_Server_Indy.Indy_ContextCreated(AContext: TIdContext);
var
  c: TCommunicationFramework_Server_Context;
begin
  c := TCommunicationFramework_Server_Context(AContext);
end;

procedure TCommunicationFramework_Server_Indy.Indy_Disconnect(AContext: TIdContext);
begin
  TCoreClassThread.Synchronize(TIdYarnOfThread(AContext.Yarn).Thread,
    procedure
    var
      c: TCommunicationFramework_Server_Context;
    begin
      c := TCommunicationFramework_Server_Context(AContext);
      if c.ClientIntf <> nil then
        begin
          DisposeObject(c.ClientIntf);
          c.ClientIntf := nil;
        end;
    end);
end;

procedure TCommunicationFramework_Server_Indy.Indy_Exception(AContext: TIdContext; AException: Exception);
begin
  TCoreClassThread.Synchronize(TIdYarnOfThread(AContext.Yarn).Thread,
    procedure
    var
      c: TCommunicationFramework_Server_Context;
    begin
      c := TCommunicationFramework_Server_Context(AContext);
      if c.ClientIntf <> nil then
        begin
          DisposeObject(c.ClientIntf);
          c.ClientIntf := nil;
        end;
    end);
end;

procedure TCommunicationFramework_Server_Indy.Indy_Execute(AContext: TIdContext);
var
  t: TTimeTick;
  c: TCommunicationFramework_Server_Context;
  iBuf: TIdBytes;
begin
  c := TCommunicationFramework_Server_Context(AContext);

  if c.ClientIntf = nil then
      Exit;

  c.ClientIntf.ProcesRealSendBuff;

  try
    TCoreClassThread.Synchronize(TIdYarnOfThread(AContext.Yarn).Thread,
      procedure
      begin
        c.ClientIntf.ProcessAllSendCmd(TIdYarnOfThread(AContext.Yarn).Thread, False, False);
      end);

    t := GetTimeTick + 5000;
    while (c.ClientIntf <> nil) and (c.Connection.Connected) and (c.ClientIntf.WaitOnResult) do
      begin
        c.ClientIntf.ProcesRealSendBuff;

        c.Connection.IOHandler.CheckForDataOnSource(10);
        if c.Connection.IOHandler.InputBuffer.Size > 0 then
          begin
            t := GetTimeTick + 5000;
            c.LastTimeTick := GetTimeTick;
            c.Connection.IOHandler.InputBuffer.ExtractToBytes(iBuf);
            c.Connection.IOHandler.InputBuffer.Clear;

            TCoreClassThread.Synchronize(TIdYarnOfThread(AContext.Yarn).Thread,
              procedure
              begin
                c.ClientIntf.SaveReceiveBuffer(@iBuf[0], length(iBuf));
                c.ClientIntf.FillRecvBuffer(TIdYarnOfThread(AContext.Yarn).Thread, False, False);
              end);
            SetLength(iBuf, 0);
          end
        else if GetTimeTick > t then
          begin
            if c.ClientIntf <> nil then
              begin
                DisposeObject(c.ClientIntf);
                c.ClientIntf := nil;
              end;
            Break;
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

  try
    c.Connection.IOHandler.CheckForDataOnSource(10);
    while c.Connection.IOHandler.InputBuffer.Size > 0 do
      begin
        c.LastTimeTick := GetTimeTick;
        c.Connection.IOHandler.InputBuffer.ExtractToBytes(iBuf);
        c.Connection.IOHandler.InputBuffer.Clear;
        try
          TCoreClassThread.Synchronize(TIdYarnOfThread(AContext.Yarn).Thread,
            procedure
            begin
              c.ClientIntf.SaveReceiveBuffer(@iBuf[0], length(iBuf));
              c.ClientIntf.FillRecvBuffer(TIdYarnOfThread(AContext.Yarn).Thread, False, False);
            end);
          SetLength(iBuf, 0);
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
        Exit;
  except
    if c.ClientIntf <> nil then
      begin
        DisposeObject(c.ClientIntf);
        c.ClientIntf := nil;
      end;
  end;

  if c.ClientIntf = nil then
      Exit;
end;

function TCommunicationFramework_Server_Indy.WaitSendConsoleCmd(p_io: TPeerIO; const Cmd, ConsoleData: SystemString; Timeout: TTimeTick): SystemString;
begin
  Result := '';
  RaiseInfo('WaitSend no Suppport IndyServer');
end;

procedure TCommunicationFramework_Server_Indy.WaitSendStreamCmd(p_io: TPeerIO; const Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; Timeout: TTimeTick);
begin
  RaiseInfo('WaitSend no Suppport IndyServer');
end;

end.
