{ ****************************************************************************** }
{ * FPC Synpase service Support, Max Connection:100                            * }
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
  Synapse Server的最大连接被限制到100
  update history
*)

unit CommunicationFramework_Server_Synapse;

{$INCLUDE ..\zDefine.inc}

interface

uses SysUtils, Classes,
  PascalStrings, CommunicationFramework, CoreClasses, MemoryStream64, DataFrameEngine,
  synsock, blcksock;

type
  TCommunicationFramework_Server_Synapse = class;
  TSynapseSockTh = class;

  TSynapseServer_PeerIO = class(TPeerIO)
  protected
    SockTh: TSynapseSockTh;
    LastPeerIP: SystemString;
    SendBuffQueue: TCoreClassListForObj;
    CurrentBuff: TMemoryStream64;
  public
    procedure CreateAfter; override;
    destructor Destroy; override;

    function Connected: Boolean; override;
    procedure Disconnect; override;
    procedure SendByteBuffer(const buff: PByte; const Size: nativeInt); override;
    procedure WriteBufferOpen; override;
    procedure WriteBufferFlush; override;
    procedure WriteBufferClose; override;
    function GetPeerIP: SystemString; override;
    function WriteBufferEmpty: Boolean; override;
    procedure Progress; override;
  end;

  TSynapseListenTh = class(TCoreClassThread)
  public
    Server: TCommunicationFramework_Server_Synapse;
    LSock: TTCPBlockSocket;
    Activted: Boolean;
    Listen: Boolean;
    procedure Sync_CreateIO;
    procedure Execute; override;
  end;

  TSynapseSockTh = class(TCoreClassThread)
  public
    ClientSockID: TSocket;
    Activted: Boolean;
    IO: TSynapseServer_PeerIO;
    Sock: TTCPBlockSocket;
    CurrentSendBuff: TMemoryStream64;
    Recv_Buff: Pointer;
    Recv_Siz: Integer;
    procedure Sync_PickBuff;
    procedure Sync_FillReceivedBuff;
    procedure Sync_CloseIO;
    procedure Execute; override;
  end;

  TCommunicationFramework_Server_Synapse = class(TCommunicationFrameworkServer)
  protected
    FListenTh: TSynapseListenTh;
    procedure All_Disconnect(PeerClient: TPeerIO);
  public
    constructor Create; override;
    destructor Destroy; override;

    function StartService(Host: SystemString; Port: Word): Boolean; override;
    procedure StopService; override;

    procedure TriggerQueueData(v: PQueueData); override;
    procedure Progress; override;

    procedure CloseAll;

    function WaitSendConsoleCmd(p_io: TPeerIO;
      const Cmd, ConsoleData: SystemString; Timeout: TTimeTick): SystemString; override;
    procedure WaitSendStreamCmd(p_io: TPeerIO;
      const Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; Timeout: TTimeTick); override;
  end;

implementation

procedure TSynapseServer_PeerIO.CreateAfter;
begin
  inherited CreateAfter;
  SockTh := nil;
  LastPeerIP := '';
  SendBuffQueue := TCoreClassListForObj.Create;
  CurrentBuff := TMemoryStream64.Create;
end;

destructor TSynapseServer_PeerIO.Destroy;
var
  i: Integer;
begin
  if SockTh <> nil then
    begin
      SockTh.IO := nil;
      SockTh.Activted := False;
    end;

  for i := 0 to SendBuffQueue.Count - 1 do
      DisposeObject(SendBuffQueue[i]);

  DisposeObject(SendBuffQueue);

  DisposeObject(CurrentBuff);
  inherited Destroy;
end;

function TSynapseServer_PeerIO.Connected: Boolean;
begin
  Result := (SockTh <> nil) and (SockTh.Activted);
end;

procedure TSynapseServer_PeerIO.Disconnect;
begin
  if SockTh <> nil then
    begin
      SockTh.IO := nil;
      SockTh.Activted := False;
    end;
  DisposeObject(Self);
end;

procedure TSynapseServer_PeerIO.SendByteBuffer(const buff: PByte; const Size: nativeInt);
begin
  if not Connected then
      Exit;

  if Size > 0 then
      CurrentBuff.write(Pointer(buff)^, Size);
end;

procedure TSynapseServer_PeerIO.WriteBufferOpen;
begin
  if not Connected then
      Exit;
  CurrentBuff.Clear;
end;

procedure TSynapseServer_PeerIO.WriteBufferFlush;
begin
  if not Connected then
      Exit;
  if CurrentBuff.Size > 0 then
    begin
      SendBuffQueue.Add(CurrentBuff);
      CurrentBuff := TMemoryStream64.Create;
    end;
end;

procedure TSynapseServer_PeerIO.WriteBufferClose;
begin
  if not Connected then
      Exit;
  CurrentBuff.Clear;
end;

function TSynapseServer_PeerIO.GetPeerIP: SystemString;
begin
  if Connected then
    begin
      Result := SockTh.Sock.GetRemoteSinIP;
      LastPeerIP := Result;
    end
  else
      Result := LastPeerIP;
end;

function TSynapseServer_PeerIO.WriteBufferEmpty: Boolean;
begin
  Result := (SendBuffQueue.Count = 0) and (SockTh <> nil) and (SockTh.CurrentSendBuff = nil);
end;

procedure TSynapseServer_PeerIO.Progress;
begin
  inherited Progress;
  ProcessAllSendCmd(nil, False, False);
end;

procedure TSynapseListenTh.Sync_CreateIO;
var
  CurrentAcceptSockTh: TSynapseSockTh;
begin
  CurrentAcceptSockTh := TSynapseSockTh.Create(True);
  CurrentAcceptSockTh.ClientSockID := LSock.Accept;
  CurrentAcceptSockTh.Activted := False;
  CurrentAcceptSockTh.IO := TSynapseServer_PeerIO.Create(Server, CurrentAcceptSockTh);
  CurrentAcceptSockTh.IO.SockTh := CurrentAcceptSockTh;
  CurrentAcceptSockTh.Suspended := False;

  while not CurrentAcceptSockTh.Activted do
      CoreClasses.CheckThreadSynchronize(1);
end;

procedure TSynapseListenTh.Execute;
var
  ClientSock: TSocket;
begin
  FreeOnTerminate := True;
  LSock := TTCPBlockSocket.Create;
  LSock.CreateSocket;
  Activted := True;

  while Activted do
    if (Listen) and (Server.Count <= 100) then
      begin
        try
          if LSock.CanRead(1000) then
              SyncMethod(Self, True, {$IFDEF FPC}@{$ENDIF FPC}Sync_CreateIO);
        except
            Listen := False;
        end;
      end;

  LSock.CloseSocket;
  DisposeObject(LSock);
  Server.FListenTh := nil;
end;

procedure TSynapseSockTh.Sync_PickBuff;
begin
  if (IO.SendBuffQueue.Count > 0) and (IO <> nil) then
    begin
      CurrentSendBuff := TMemoryStream64(IO.SendBuffQueue[0]);
      IO.SendBuffQueue.Delete(0);
    end;
end;

procedure TSynapseSockTh.Sync_FillReceivedBuff;
begin
  if (IO <> nil) then
    begin
      IO.SaveReceiveBuffer(Recv_Buff, Recv_Siz);
      IO.FillRecvBuffer(Self, True, True);
    end;
end;

procedure TSynapseSockTh.Sync_CloseIO;
begin
  if IO <> nil then
    begin
      IO.SockTh := nil;
      DisposeObject(IO);
    end;
  DisposeObject(Sock);
end;

procedure TSynapseSockTh.Execute;
const
  memSiz: Integer = 1024 * 1024;
begin
  FreeOnTerminate := True;
  Sock := TTCPBlockSocket.Create;
  Sock.Socket := ClientSockID;
  Sock.GetSins;
  Activted := True;

  Recv_Buff := System.GetMemory(memSiz);
  while (Activted) and (IO <> nil) do
    begin
      try
        while Activted and (IO.SendBuffQueue.Count > 0) do
          begin
            CurrentSendBuff := nil;
            SyncMethod(Self, True, {$IFDEF FPC}@{$ENDIF FPC}Sync_PickBuff);

            if CurrentSendBuff <> nil then
              begin
                Sock.SendBuffer(CurrentSendBuff.Memory, CurrentSendBuff.Size);
                DisposeObject(CurrentSendBuff);
                CurrentSendBuff := nil;
              end;
          end;

        if Activted and Sock.CanRead(100) then
          begin
            Recv_Siz := Sock.RecvBuffer(Recv_Buff, memSiz);

            if Sock.LastError <> 0 then
                break;

            if (Activted) and (Recv_Siz > 0) then
                TCoreClassThread.Synchronize(Self, {$IFDEF FPC}@{$ENDIF FPC}Sync_FillReceivedBuff);
          end;
      except
          Activted := False;
      end;
    end;
  System.FreeMemory(Recv_Buff);
  SyncMethod(Self, True, {$IFDEF FPC}@{$ENDIF FPC}Sync_CloseIO);
end;

procedure TCommunicationFramework_Server_Synapse.All_Disconnect(PeerClient: TPeerIO);
begin
  PeerClient.Disconnect;
end;

constructor TCommunicationFramework_Server_Synapse.Create;
begin
  inherited Create;
  FEnabledAtomicLockAndMultiThread := False;

  FListenTh := TSynapseListenTh.Create(True);
  FListenTh.Server := Self;
  FListenTh.Activted := False;
  FListenTh.Suspended := False;
  while not FListenTh.Activted do
      CoreClasses.CheckThreadSynchronize(1);
end;

destructor TCommunicationFramework_Server_Synapse.Destroy;
begin
  StopService;
  FListenTh.Activted := False;
  while FListenTh <> nil do
      CoreClasses.CheckThreadSynchronize(1);
  inherited Destroy;
end;

function TCommunicationFramework_Server_Synapse.StartService(Host: SystemString; Port: Word): Boolean;
begin
  try
    FListenTh.LSock.SetLinger(True, 10000);
    FListenTh.LSock.EnableReuse(True); // fixed by.qq47324905
    FListenTh.LSock.Bind(Host, IntToStr(Port));
    FListenTh.LSock.Listen;
    FListenTh.Listen := True;
    Result := FListenTh.LSock.LastError = 0;
  except
    FListenTh.LSock.SetLinger(False, 0);
    Result := False;
    FListenTh.Listen := False;
  end;
end;

procedure TCommunicationFramework_Server_Synapse.StopService;
begin
  CloseAll;
  FListenTh.LSock.SetLinger(False, 0);
  FListenTh.Listen := False;
end;

procedure TCommunicationFramework_Server_Synapse.TriggerQueueData(v: PQueueData);
var
  c: TPeerIO;
begin
  c := PeerIO[v^.IO_ID];
  if c <> nil then
    begin
      c.PostQueueData(v);
      c.ProcessAllSendCmd(nil, False, False);
    end
  else
      DisposeQueueData(v);
end;

procedure TCommunicationFramework_Server_Synapse.Progress;
begin
  inherited Progress;
end;

procedure TCommunicationFramework_Server_Synapse.CloseAll;
begin
  ProgressPeerIOM({$IFDEF FPC}@{$ENDIF FPC}All_Disconnect);
end;

function TCommunicationFramework_Server_Synapse.WaitSendConsoleCmd(p_io: TPeerIO;
  const Cmd, ConsoleData: SystemString; Timeout: TTimeTick): SystemString;
begin
  Result := '';
  RaiseInfo('WaitSend no Suppport');
end;

procedure TCommunicationFramework_Server_Synapse.WaitSendStreamCmd(p_io: TPeerIO;
  const Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; Timeout: TTimeTick);
begin
  RaiseInfo('WaitSend no Suppport');
end;

initialization

finalization

end.
