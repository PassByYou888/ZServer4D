{ ****************************************************************************** }
{ * ics support                                                                * }
{ * written by QQ 600585@qq.com                                                * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
(* https://github.com/PassByYou888/ZServer4D *)
{ ****************************************************************************** }
(*
  update history
*)
unit CommunicationFramework_Server_ICS;

{$I ..\zDefine.inc}

interface

uses Windows, SysUtils, Classes, Messages,
  OverByteIcsWSocket,
  PascalStrings,
  CommunicationFramework_Server_ICSCustomSocket,
  CommunicationFramework, CoreClasses, DoStatusIO, DataFrameEngine;

type
  TCommunicationFramework_Server_ICS = class;
  TICSContext                        = class;

  TICSSocketThread_Server = class(TThread)
    FContext: TICSContext;
    FThreadAttached: Boolean;
    FClientLoopMessageTerminated: Boolean;
    FCommunicationFramework: TCommunicationFramework_Server_ICS;
    LastProcessHandle: THandle;
    LastProcessMsgID: UINT;

    constructor Create(CreateSuspended: Boolean);
    destructor Destroy; override;

    procedure Execute; override;

    procedure ProcessThreadTrigger;
  end;

  TPeerClientIntfForICS = class(TPeerIO)
  public
    FContext: TICSContext;

    function Connected: Boolean; override;
    procedure Disconnect; override;
    procedure SendByteBuffer(const buff: PByte; const Size: NativeInt); override;
    procedure WriteBufferOpen; override;
    procedure WriteBufferFlush; override;
    procedure WriteBufferClose; override;
    function GetPeerIP: SystemString; override;

    procedure ContinueResultSend; override;
  end;

  TICSContext = class(TCustomICSContext)
  protected
    FTRIGGER_THREAD_PROCESS_MSGID: UINT;
    FICSSocketThread             : TICSSocketThread_Server;
    FClientIntf                  : TPeerClientIntfForICS;
    FLastActiveTime              : TTimeTickValue;
    FClientThreadPause           : Boolean;
    FTimeOut                     : TTimeTickValue;
    ThreadAttachAborted          : Boolean;
  protected
    // thread sync interface
    procedure WndProc(var MsgRec: TMessage); override;

    procedure ClientDataAvailable(Sender: TObject; Error: Word);
    procedure ClientBgException(Sender: TObject; E: Exception; var CanClose: Boolean);
    procedure ClientSendData(Sender: TObject; BytesSent: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function ProcessClientActiveTime: Boolean;
    procedure SetClientThreadContinue;
    procedure SetClientThreadPause;
  end;

  TCommunicationFramework_Server_ICS = class(TCommunicationFrameworkServer)
  private
    FDriver        : TCustomICSSocketServer;
    FBindHost      : SystemString;
    FBindPort      : Word;
    FStartedService: Boolean;

    procedure ClientConnectEvent(Sender: TObject; Client: TCustomICSContext; Error: Word);
    procedure ClientCreateContextEvent(Sender: TObject; Client: TCustomICSContext);
    procedure ClientDisconnectEvent(Sender: TObject; Client: TCustomICSContext; Error: Word);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure StopService; override;
    function StartService(Host: SystemString; Port: Word): Boolean; override;

    procedure TriggerQueueData(v: PQueueData); override;

    procedure ProgressBackground; override;

    function WaitSendConsoleCmd(Client: TPeerIO; const Cmd, ConsoleData: SystemString; TimeOut: TTimeTickValue): SystemString; override;
    procedure WaitSendStreamCmd(Client: TPeerIO; const Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; TimeOut: TTimeTickValue); override;

    property StartedService: Boolean read FStartedService;
    property Driver: TCustomICSSocketServer read FDriver;
    property BindHost: SystemString read FBindHost;
    property BindPort: Word read FBindPort;
  end;

var
  ICSThreadCount, ICSCreatedClientCount: Integer;
  sThSection                           : TRTLCriticalSection;
  ICSThreadList                        : TCoreClassListForObj;

implementation

constructor TICSSocketThread_Server.Create(CreateSuspended: Boolean);
begin
  FThreadAttached := False;
  FCommunicationFramework := nil;
  FContext := nil;
  LastProcessHandle := 0;
  LastProcessMsgID := 0;
  inherited Create(CreateSuspended);
end;

destructor TICSSocketThread_Server.Destroy;
begin
  try
    if FContext <> nil then
        FContext.FICSSocketThread := nil;
  except
  end;

  try
      inherited Destroy;
  except
  end;
end;

procedure TICSSocketThread_Server.Execute;
var
  MsgRec: TMsg;
  i     : Integer;
begin
  FContext.ThreadAttachAborted := False;
  try
      FContext.ThreadAttach;
  except
    FContext.ThreadAttachAborted := True;
    Exit;
  end;

  FThreadAttached := True;
  FClientLoopMessageTerminated := False;

  EnterCriticalSection(sThSection);
  try
    ICSThreadList.Add(Self);
    ICSThreadCount := ICSThreadList.Count;
  finally
      LeaveCriticalSection(sThSection);
  end;

  while (FContext <> nil) do
    begin
      LastProcessHandle := FContext.Handle;
      LastProcessMsgID := FContext.FTRIGGER_THREAD_PROCESS_MSGID;

      while (FContext <> nil) and (FContext.State in [wsConnected]) and (FContext.FClientThreadPause) and (not FClientLoopMessageTerminated) do
        begin
          try
            if not GetMessage(MsgRec, FContext.Handle, 0, 0) then
              begin
                FClientLoopMessageTerminated := True;
                break;
              end;
            TranslateMessage(MsgRec);
            DispatchMessage(MsgRec)
          except
              FClientLoopMessageTerminated := True;
          end;
        end;

      try
        if (not FClientLoopMessageTerminated) and (FContext <> nil) and (FContext.FClientIntf <> nil) and (FContext.State in [wsConnected]) then
          begin
            try
                FContext.FClientIntf.ProcessAllSendCmd(Self, True, False);
            except
                FClientLoopMessageTerminated := True;
            end;
          end;

        if FContext <> nil then
          begin
            FContext.ProcessClientActiveTime;
            if FContext <> nil then
              begin
                if FClientLoopMessageTerminated then
                  begin
                    if FContext.State in [wsConnected] then
                        FContext.CloseDelayed
                    else
                        break;
                  end;
              end
            else
                FClientLoopMessageTerminated := True;

            if FContext <> nil then
              begin
                try
                  try
                    if GetMessage(MsgRec, FContext.Handle, 0, 0) then
                      begin
                        try
                            TranslateMessage(MsgRec);
                        except
                            FClientLoopMessageTerminated := True;
                        end;

                        try
                            DispatchMessage(MsgRec)
                        except
                            FClientLoopMessageTerminated := True;
                        end;
                      end
                    else
                      begin
                        FClientLoopMessageTerminated := True;
                        break;
                      end;
                  except
                      FClientLoopMessageTerminated := True;
                  end;

                except
                    FClientLoopMessageTerminated := True;
                end;
              end;
          end;
      except
          FClientLoopMessageTerminated := True;
      end;
    end;

  if FContext <> nil then
    begin
      FContext.FICSSocketThread := nil;
      try
          FContext.CloseDelayed;
      except
      end;
      FContext := nil;
    end;

  EnterCriticalSection(sThSection);
  try
    i := 0;
    while i < ICSThreadList.Count do
      begin
        if ICSThreadList[i] = Self then
            ICSThreadList.Delete(i)
        else
            Inc(i);
      end;
    ICSThreadCount := ICSThreadList.Count;
  finally
      LeaveCriticalSection(sThSection);
  end;

  FThreadAttached := False;
end;

procedure TICSSocketThread_Server.ProcessThreadTrigger;
begin
  if FContext <> nil then
      PostMessage(FContext.Handle, FContext.FTRIGGER_THREAD_PROCESS_MSGID, 0, 0)
  else
    begin
      if LastProcessHandle <> 0 then
          PostMessage(LastProcessHandle, $12, 0, 0);
    end;
end;

function TPeerClientIntfForICS.Connected: Boolean;
begin
  if FContext <> nil then
      Result := (FContext.State in [wsConnected])
  else
      Result := False;
end;

procedure TPeerClientIntfForICS.Disconnect;
begin
  if FContext <> nil then
    begin
      if FContext.FICSSocketThread <> nil then
        begin
          FContext.FICSSocketThread.FClientLoopMessageTerminated := True;
          FContext.FICSSocketThread.ProcessThreadTrigger;
        end
      else
          FContext.CloseDelayed;
    end;
end;

procedure TPeerClientIntfForICS.SendByteBuffer(const buff: PByte; const Size: NativeInt);
begin
  if Connected then
    if Size > 0 then
        FContext.Send(buff, Size);
end;

procedure TPeerClientIntfForICS.WriteBufferOpen;
begin
end;

procedure TPeerClientIntfForICS.WriteBufferFlush;
begin
  try
    if Connected then
        FContext.TryToSend;
  except
  end;
end;

procedure TPeerClientIntfForICS.WriteBufferClose;
begin
  try
    if Connected then
        FContext.TryToSend;
  except
  end;
end;

function TPeerClientIntfForICS.GetPeerIP: SystemString;
begin
  if FContext <> nil then
      Result := FContext.PeerAddr
  else
      Result := '';
end;

procedure TPeerClientIntfForICS.ContinueResultSend;
begin
  inherited ContinueResultSend;
  ProcessAllSendCmd(nil, False, False);
  // FContext.FICSSocketThread.ProcessThreadTrigger;
end;

procedure TICSContext.WndProc(var MsgRec: TMessage);
begin
  if MsgRec.Msg <> FTRIGGER_THREAD_PROCESS_MSGID then
      inherited WndProc(MsgRec);
end;

procedure TICSContext.ClientDataAvailable(Sender: TObject; Error: Word);
var
  BuffCount: Integer;
  buff     : TBytes;
begin
  FLastActiveTime := GetTimeTickCount;

  // increment receive
  BuffCount := RcvdCount;
  if BuffCount <= 0 then
      BuffCount := 255 * 255;
  SetLength(buff, BuffCount);
  BuffCount := Receive(@buff[0], BuffCount);
  if BuffCount > 0 then
    begin
      try
        FClientIntf.SaveReceiveBuffer(@buff[0], BuffCount);
        FClientIntf.FillRecvBuffer(FICSSocketThread, True, False);
      except
          Close;
      end;
    end;
end;

procedure TICSContext.ClientBgException(Sender: TObject; E: Exception; var CanClose: Boolean);
begin
  FICSSocketThread.FClientLoopMessageTerminated := True;
  FICSSocketThread.ProcessThreadTrigger;
  CloseDelayed;
end;

procedure TICSContext.ClientSendData(Sender: TObject; BytesSent: Integer);
begin
  FLastActiveTime := GetTimeTickCount;
end;

constructor TICSContext.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Assert(AOwner is TCustomICSSocketServer);
  FTRIGGER_THREAD_PROCESS_MSGID := TCustomICSSocketServer(AOwner).CLIENT_THREAD_PROCESS_MSGID;

  FICSSocketThread := nil;
  FClientIntf := nil;

  FLastActiveTime := GetTimeTickCount;
  FClientThreadPause := False;

  FTimeOut := 0;

  OnDataAvailable := ClientDataAvailable;
  OnBgException := ClientBgException;
  OnSendData := ClientSendData;

  MultiThreaded := True;
  Inc(ICSCreatedClientCount);
end;

destructor TICSContext.Destroy;
begin
  if FICSSocketThread <> nil then
    begin
      try
        FICSSocketThread.FContext := nil;
        FICSSocketThread.FClientLoopMessageTerminated := True;
      except
      end;

      try
      except
          FICSSocketThread.ProcessThreadTrigger;
      end;
    end;

  if FClientIntf <> nil then
    begin
      DisposeObject(FClientIntf);
      FClientIntf := nil;
    end;

  Dec(ICSCreatedClientCount);

  try
      inherited Destroy;
  except
  end;
end;

function TICSContext.ProcessClientActiveTime: Boolean;
begin
  Result := False;
  if (FTimeOut > 0) and (GetTimeTickCount - FLastActiveTime > FTimeOut) then
    begin
      if FICSSocketThread <> nil then
        begin
          FICSSocketThread.FClientLoopMessageTerminated := True;
          FICSSocketThread.ProcessThreadTrigger;
        end;
      Result := True;
    end;
end;

procedure TICSContext.SetClientThreadContinue;
begin
  FClientThreadPause := False;
  try
      PostMessage(Handle, FTRIGGER_THREAD_PROCESS_MSGID, 0, 0);
  except
  end;
end;

procedure TICSContext.SetClientThreadPause;
begin
  FClientThreadPause := True;
  try
      PostMessage(Handle, FTRIGGER_THREAD_PROCESS_MSGID, 0, 0);
  except
  end;
end;

procedure TCommunicationFramework_Server_ICS.ClientConnectEvent(Sender: TObject; Client: TCustomICSContext; Error: Word);
begin
  DoStatus(Format('accept connect %s:%s ', [Client.GetPeerAddr, Client.GetPeerPort]));
  Client.KeepAliveOnOff := TSocketKeepAliveOnOff.wsKeepAliveOnCustom;
  Client.KeepAliveTime := 1 * 1000;     // 从心跳检查到断开的空闲时间
  Client.KeepAliveInterval := 1 * 1000; // 心跳检查间隔
end;

procedure TCommunicationFramework_Server_ICS.ClientCreateContextEvent(Sender: TObject; Client: TCustomICSContext);
var
  cli: TICSContext;
  t  : TTimeTickValue;
begin
  if Count > 500 then
    begin
      cli.Close;
      Exit;
    end;
  try
    cli := Client as TICSContext;
    cli.FTimeOut := IdleTimeout;

    cli.ThreadDetach;
    cli.FICSSocketThread := TICSSocketThread_Server.Create(True);
    cli.FICSSocketThread.FreeOnTerminate := True;
    cli.FICSSocketThread.FContext := cli;
    cli.FICSSocketThread.FCommunicationFramework := Self;
    cli.FICSSocketThread.Suspended := False;

    cli.FClientIntf := TPeerClientIntfForICS.Create(Self, cli);
    cli.FClientIntf.FContext := cli;

    t := GetTimeTickCount + 5000;
    while (cli.FICSSocketThread <> nil) and (not cli.FICSSocketThread.FThreadAttached) do
      begin
        TThread.Sleep(1);
        if GetTimeTickCount > t then
            break;
        if cli.ThreadAttachAborted then
            break;
      end;

    if cli.ThreadAttachAborted then
        cli.Close;
  except
      cli.Close;
  end;
end;

procedure TCommunicationFramework_Server_ICS.ClientDisconnectEvent(Sender: TObject; Client: TCustomICSContext; Error: Word);
var
  cli: TICSContext;
begin
  cli := Client as TICSContext;

  if cli <> nil then
    if cli.FClientIntf <> nil then
        cli.FClientIntf.Print('disconnect %s:%s', [Client.GetPeerAddr, Client.GetPeerPort]);

  if cli.FICSSocketThread <> nil then
    begin
      try
        cli.FICSSocketThread.FContext := nil;
        cli.FICSSocketThread.FClientLoopMessageTerminated := True;
      except
      end;

      try
          cli.FICSSocketThread.ProcessThreadTrigger;
      except
      end;
    end;

  if cli.FClientIntf <> nil then
    begin
      DisposeObject(cli.FClientIntf);
      cli.FClientIntf := nil;
    end;
end;

constructor TCommunicationFramework_Server_ICS.Create;
var
  r: TCommandStreamMode;
begin
  inherited Create;
  FDriver := TCustomICSSocketServer.Create(nil);
  FDriver.MultiThreaded := True;

  // client interface
  FDriver.OnClientCreate := ClientCreateContextEvent;
  FDriver.OnClientDisconnect := ClientDisconnectEvent;
  FDriver.OnClientConnect := ClientConnectEvent;
  FDriver.ClientClass := TICSContext;

  FStartedService := False;
  FBindHost := '';
  FBindPort := 0;
end;

destructor TCommunicationFramework_Server_ICS.Destroy;
begin
  StopService;
  try
      DisposeObject(FDriver);
  except
  end;
  inherited Destroy;
end;

function TCommunicationFramework_Server_ICS.StartService(Host: SystemString; Port: Word): Boolean;
begin
  try
    // open listen
    FDriver.Proto := 'tcp';
    FDriver.Port := IntToStr(Port);
    FDriver.Addr := Host;

    FDriver.Listen;
    Result := True;
    FStartedService := True;

    FBindHost := Host;
    FBindPort := Port;
  except
      Result := False;
  end;
end;

procedure TCommunicationFramework_Server_ICS.StopService;
begin
  while Count > 0 do
    begin
      ProgressPerClient(procedure(cli: TPeerIO)
        begin
          cli.Disconnect;
        end);
      ProgressBackground;
    end;

  try
    FDriver.Close;
    FStartedService := False;
  except
  end;
  FBindHost := '';
  FBindPort := 0;
end;

procedure TCommunicationFramework_Server_ICS.TriggerQueueData(v: PQueueData);
begin
  if Exists(v^.Client) then
    begin
      v^.Client.PostQueueData(v);
      TPeerClientIntfForICS(v.Client).FContext.FICSSocketThread.ProcessThreadTrigger;
    end
  else
      DisposeQueueData(v);
end;

procedure TCommunicationFramework_Server_ICS.ProgressBackground;
var
  i: Integer;
begin
  ProgressPerClient(procedure(cli: TPeerIO)
    begin
      TPeerClientIntfForICS(cli).FContext.ProcessClientActiveTime;
    end);

  inherited ProgressBackground;

  try
      FDriver.ProcessMessages;
  except
  end;
end;

function TCommunicationFramework_Server_ICS.WaitSendConsoleCmd(Client: TPeerIO; const Cmd, ConsoleData: SystemString; TimeOut: TTimeTickValue): SystemString;
begin
  Result := '';
  RaiseInfo('WaitSend no Suppport ICSServer');
end;

procedure TCommunicationFramework_Server_ICS.WaitSendStreamCmd(Client: TPeerIO; const Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; TimeOut: TTimeTickValue);
begin
  RaiseInfo('WaitSend no Suppport ICSServer');
end;

initialization

InitializeCriticalSection(sThSection);
ICSThreadList := TCoreClassListForObj.Create;
ICSThreadCount := 0;
ICSCreatedClientCount := 0;

finalization

DeleteCriticalSection(sThSection);
DisposeObject(ICSThreadList);

end.
