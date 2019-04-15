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
  DIOCP Server的最大连接被限制到20000
  update history
*)
unit CommunicationFramework_Server_DIOCP;

{$INCLUDE ..\zDefine.inc}

interface

uses SysUtils, Classes,
  PascalStrings,
  CommunicationFramework, CoreClasses, UnicodeMixedLib, MemoryStream64, DataFrameEngine,
  diocp_tcp_server;

type
  TDIOCPServer_PeerIO = class;

  TIocpClientContextIntf_WithDServ = class(TIocpClientContext)
  private
    Link: TDIOCPServer_PeerIO;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TDIOCPServer_PeerIO = class(TPeerIO)
  private
    Link: TIocpClientContextIntf_WithDServ;
    lastSendBufferTag: Integer;
    WasSending: Boolean;
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
    function WriteBufferEmpty: Boolean; override;
    procedure Progress; override;
  end;

  TCommunicationFramework_Server_DIOCP = class(TCommunicationFrameworkServer)
  private
  protected
    FDIOCPServer: TDiocpTcpServer;

    procedure DIOCP_IOAccept(pvSocket: THandle; pvAddr: String; pvPort: Integer; var vAllowAccept: Boolean);
    procedure DIOCP_IOConnected(pvClientContext: TIocpClientContext);
    procedure DIOCP_IODisconnect(pvClientContext: TIocpClientContext);
    procedure DIOCP_IOSend(pvContext: TIocpClientContext; pvRequest: TIocpSendRequest);
    procedure DIOCP_IOSendCompleted(pvContext: TIocpClientContext; pvBuff: Pointer; Len: Cardinal; pvBufferTag: Integer; pvTagData: Pointer; pvErrorCode: Integer);
    procedure DIOCP_IOReceive(pvClientContext: TIocpClientContext; Buf: Pointer; Len: Cardinal; ErrCode: Integer);
  public
    constructor Create; override;
    destructor Destroy; override;

    function StartService(Host: SystemString; Port: Word): Boolean; override;
    procedure StopService; override;

    procedure TriggerQueueData(v: PQueueData); override;
    procedure Progress; override;

    function WaitSendConsoleCmd(p_io: TPeerIO; const Cmd, ConsoleData: SystemString; Timeout: TTimeTick): SystemString; override;
    procedure WaitSendStreamCmd(p_io: TPeerIO; const Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; Timeout: TTimeTick); override;
  end;

implementation

constructor TIocpClientContextIntf_WithDServ.Create;
begin
  inherited Create;
  Link := nil;
end;

destructor TIocpClientContextIntf_WithDServ.Destroy;
var
  peerio: TDIOCPServer_PeerIO;
begin
  if Link <> nil then
    begin
      peerio := Link;
      Link := nil;
      DisposeObject(peerio);
    end;
  inherited Destroy;
end;

procedure TDIOCPServer_PeerIO.CreateAfter;
begin
  inherited CreateAfter;
  Link := nil;
  lastSendBufferTag := 0;
  WasSending := False;
  SendingStream := TMemoryStream64.Create;
end;

destructor TDIOCPServer_PeerIO.Destroy;
var
  clink: TIocpClientContextIntf_WithDServ;
begin
  if Link <> nil then
    begin
      clink := Link;
      Link := nil;
      clink.Link := nil;
      clink.PostWSACloseRequest;
    end;

  DisposeObject(SendingStream);
  inherited Destroy;
end;

function TDIOCPServer_PeerIO.Connected: Boolean;
begin
  Result := (Link <> nil);
end;

procedure TDIOCPServer_PeerIO.Disconnect;
var
  clink: TIocpClientContextIntf_WithDServ;
begin
  if Link <> nil then
    begin
      clink := Link;
      Link := nil;
      clink.Link := nil;
      clink.PostWSACloseRequest;
    end;
  DisposeObject(Self);
end;

procedure TDIOCPServer_PeerIO.SendByteBuffer(const buff: PByte; const Size: NativeInt);
begin
  if not Connected then
      Exit;

  if Size > 0 then
      SendingStream.WritePtr(buff, Size);
end;

procedure TDIOCPServer_PeerIO.WriteBufferOpen;
begin
  WriteBufferFlush;
end;

procedure TDIOCPServer_PeerIO.WriteBufferFlush;
begin
  if SendingStream.Size > 0 then
    begin
      inc(lastSendBufferTag);
      WasSending := True;
      // 因为DIOCP的发送是基于数据队列
      // 把所有的预置数据以队列方式fill后再发
      // 这里我用flush方式后置化发送数据，做到每次发送出去的是一个块，一般来说，这里被zs触发时，都是一个ip包左右的大小
      Link.PostWSASendRequest(SendingStream.Memory, SendingStream.Size, True, lastSendBufferTag);
      SendingStream.Clear;
    end;
end;

procedure TDIOCPServer_PeerIO.WriteBufferClose;
begin
  WriteBufferFlush;
end;

function TDIOCPServer_PeerIO.GetPeerIP: SystemString;
begin
  if Connected then
      Result := Link.RemoteAddr + ' ' + IntToStr(Link.RemotePort)
  else
      Result := '';
end;

function TDIOCPServer_PeerIO.WriteBufferEmpty: Boolean;
begin
  Result := not WasSending;
end;

procedure TDIOCPServer_PeerIO.Progress;
begin
  inherited Progress;
  ProcessAllSendCmd(nil, False, False);
end;

procedure TCommunicationFramework_Server_DIOCP.DIOCP_IOAccept(pvSocket: THandle; pvAddr: String; pvPort: Integer; var vAllowAccept: Boolean);
begin
  vAllowAccept := Count < 20000;
end;

procedure TCommunicationFramework_Server_DIOCP.DIOCP_IOConnected(pvClientContext: TIocpClientContext);
begin
  TCoreClassThread.Synchronize(TCoreClassThread.CurrentThread, procedure
    begin
      TIocpClientContextIntf_WithDServ(pvClientContext).Link := TDIOCPServer_PeerIO.Create(Self, pvClientContext);
      TIocpClientContextIntf_WithDServ(pvClientContext).Link.Link := TIocpClientContextIntf_WithDServ(pvClientContext);
    end);
end;

procedure TCommunicationFramework_Server_DIOCP.DIOCP_IODisconnect(pvClientContext: TIocpClientContext);
begin
  if TIocpClientContextIntf_WithDServ(pvClientContext).Link = nil then
      Exit;

  TCoreClassThread.Synchronize(TCoreClassThread.CurrentThread, procedure
    begin
      DisposeObject(TIocpClientContextIntf_WithDServ(pvClientContext).Link);
    end);
end;

procedure TCommunicationFramework_Server_DIOCP.DIOCP_IOSend(pvContext: TIocpClientContext; pvRequest: TIocpSendRequest);
begin
end;

procedure TCommunicationFramework_Server_DIOCP.DIOCP_IOSendCompleted(pvContext: TIocpClientContext; pvBuff: Pointer; Len: Cardinal; pvBufferTag: Integer; pvTagData: Pointer; pvErrorCode: Integer);
var
  peerio: TDIOCPServer_PeerIO;
begin
  if TIocpClientContextIntf_WithDServ(pvContext).Link = nil then
      Exit;
  peerio := TIocpClientContextIntf_WithDServ(pvContext).Link;
  if peerio.lastSendBufferTag = pvBufferTag then
      peerio.WasSending := False;
end;

procedure TCommunicationFramework_Server_DIOCP.DIOCP_IOReceive(pvClientContext: TIocpClientContext; Buf: Pointer; Len: Cardinal; ErrCode: Integer);
begin
  if TIocpClientContextIntf_WithDServ(pvClientContext).Link = nil then
      Exit;

  TCoreClassThread.Synchronize(TCoreClassThread.CurrentThread, procedure
    begin
      // zs内核在新版本已经完全支持100%的异步解析
      // 经过简单分析，这个事件被上锁保护了，似乎调度有点延迟
      // 这里的性能热点不太好找，diocp的瓶颈主要是卡在这一步
      TIocpClientContextIntf_WithDServ(pvClientContext).Link.SaveReceiveBuffer(Buf, Len);
      TIocpClientContextIntf_WithDServ(pvClientContext).Link.FillRecvBuffer(TCoreClassThread.CurrentThread, False, False);
    end);
end;

constructor TCommunicationFramework_Server_DIOCP.Create;
begin
  inherited Create;
  FEnabledAtomicLockAndMultiThread := False;

  FDIOCPServer := TDiocpTcpServer.Create(nil);

  FDIOCPServer.OnContextAccept := DIOCP_IOAccept;
  FDIOCPServer.OnContextConnected := DIOCP_IOConnected;
  FDIOCPServer.OnContextDisconnected := DIOCP_IODisconnect;
  FDIOCPServer.OnSendRequestResponse := DIOCP_IOSend;
  FDIOCPServer.OnSendBufferCompleted := DIOCP_IOSendCompleted;
  FDIOCPServer.OnDataReceived := DIOCP_IOReceive;

  FDIOCPServer.WorkerCount := 4;
  FDIOCPServer.MaxSendingQueueSize := 20000;
  FDIOCPServer.NoDelayOption := True;
  FDIOCPServer.KeepAlive := True;
  FDIOCPServer.KeepAliveTime := 5000;
  FDIOCPServer.RegisterContextClass(TIocpClientContextIntf_WithDServ);
end;

destructor TCommunicationFramework_Server_DIOCP.Destroy;
begin
  StopService;
  DisposeObject(FDIOCPServer);
  inherited Destroy;
end;

function TCommunicationFramework_Server_DIOCP.StartService(Host: SystemString; Port: Word): Boolean;
begin
  FDIOCPServer.DefaultListenAddress := Host;
  FDIOCPServer.Port := Port;

  try
    FDIOCPServer.Active := True;
    Result := True;
  except
      Result := False;
  end;
end;

procedure TCommunicationFramework_Server_DIOCP.StopService;
begin
  FDIOCPServer.Active := False;
end;

procedure TCommunicationFramework_Server_DIOCP.TriggerQueueData(v: PQueueData);
var
  c: TPeerIO;
begin
  c := peerio[v^.IO_ID];
  if c <> nil then
    begin
      c.PostQueueData(v);
      c.ProcessAllSendCmd(nil, False, False);
    end
  else
      DisposeQueueData(v);
end;

procedure TCommunicationFramework_Server_DIOCP.Progress;
begin
  inherited Progress;
  CoreClasses.CheckThreadSynchronize;
end;

function TCommunicationFramework_Server_DIOCP.WaitSendConsoleCmd(p_io: TPeerIO; const Cmd, ConsoleData: SystemString; Timeout: TTimeTick): SystemString;
begin
  Result := '';
  RaiseInfo('WaitSend no Suppport CrossSocket');
end;

procedure TCommunicationFramework_Server_DIOCP.WaitSendStreamCmd(p_io: TPeerIO; const Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; Timeout: TTimeTick);
begin
  RaiseInfo('WaitSend no Suppport CrossSocket');
end;

initialization

finalization

end.
