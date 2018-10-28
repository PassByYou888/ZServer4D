{ ****************************************************************************** }
{ * FPC Synpase service Support, Max Connection:100                            * }
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

unit CommunicationFramework_Server_Synapse;

{$INCLUDE ..\zDefine.inc}

interface

uses SysUtils, Classes,
  PascalStrings, CommunicationFramework, CoreClasses, MemoryStream64, DataFrameEngine,
  synsock, blcksock;

type
  TCommunicationFramework_Server_Synapse = class;
  TSynapseSockTh = class;

  TPeerIOWithSynapseServer = class(TPeerIO)
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
    Server: TCommunicationFramework_Server_Synapse;
    LSock: TTCPBlockSocket;
    Activted: Boolean;
    Listen: Boolean;
    CurrentAcceptSockTh: TSynapseSockTh;
    procedure Sync_CreateIO;
    procedure Execute; override;
  end;

  TSynapseSockTh = class(TCoreClassThread)
    ClientSockID: TSocket;
    Activted: Boolean;
    IO: TPeerIOWithSynapseServer;
    Sock: TTCPBlockSocket;
    CurrentSendBuff: TMemoryStream64;
    procedure PickSendBuff;
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

    function WaitSendConsoleCmd(Client: TPeerIO;
      const Cmd, ConsoleData: SystemString; Timeout: TTimeTickValue): SystemString; override;
    procedure WaitSendStreamCmd(Client: TPeerIO;
      const Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; Timeout: TTimeTickValue); override;
  end;

implementation

procedure TPeerIOWithSynapseServer.CreateAfter;
begin
  inherited CreateAfter;
  SockTh := nil;
  LastPeerIP := '';
  SendBuffQueue := TCoreClassListForObj.Create;
  CurrentBuff := TMemoryStream64.Create;
end;

destructor TPeerIOWithSynapseServer.Destroy;
var
  i: Integer;
begin
  for i := 0 to SendBuffQueue.Count - 1 do
      DisposeObject(SendBuffQueue[i]);

  DisposeObject(SendBuffQueue);

  DisposeObject(CurrentBuff);
  inherited Destroy;
end;

function TPeerIOWithSynapseServer.Connected: Boolean;
begin
  Result := (SockTh <> nil) and (SockTh.Activted);
end;

procedure TPeerIOWithSynapseServer.Disconnect;
begin
  if SockTh <> nil then
      SockTh.Activted := False;
end;

procedure TPeerIOWithSynapseServer.SendByteBuffer(const buff: PByte; const Size: nativeInt);
begin
  if not Connected then
      Exit;

  if Size > 0 then
      CurrentBuff.write(Pointer(buff)^, Size);
end;

procedure TPeerIOWithSynapseServer.WriteBufferOpen;
begin
  if not Connected then
      Exit;
  CurrentBuff.Clear;
end;

procedure TPeerIOWithSynapseServer.WriteBufferFlush;
begin
  if not Connected then
      Exit;
  if CurrentBuff.Size > 0 then
    begin
      SendBuffQueue.Add(CurrentBuff);
      CurrentBuff := TMemoryStream64.Create;
    end;
end;

procedure TPeerIOWithSynapseServer.WriteBufferClose;
begin
  if not Connected then
      Exit;
  CurrentBuff.Clear;
end;

function TPeerIOWithSynapseServer.GetPeerIP: SystemString;
begin
  if Connected then
    begin
      Result := SockTh.Sock.GetRemoteSinIP;
      LastPeerIP := Result;
    end
  else
      Result := LastPeerIP;
end;

function TPeerIOWithSynapseServer.WriteBufferEmpty: Boolean;
begin
  Result := SendBuffQueue.Count = 0;
end;

procedure TPeerIOWithSynapseServer.Progress;
begin
  inherited Progress;
  ProcessAllSendCmd(nil, False, False);
end;

procedure TSynapseListenTh.Sync_CreateIO;
begin
  CurrentAcceptSockTh := TSynapseSockTh.Create(True);
  CurrentAcceptSockTh.ClientSockID := LSock.Accept;
  CurrentAcceptSockTh.Activted := False;
  CurrentAcceptSockTh.IO := TPeerIOWithSynapseServer.Create(Server, CurrentAcceptSockTh);
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

procedure TSynapseSockTh.PickSendBuff;
begin
  if IO.SendBuffQueue.Count > 0 then
    begin
      CurrentSendBuff := TMemoryStream64(IO.SendBuffQueue[0]);
      IO.SendBuffQueue.Delete(0);
    end;
end;

procedure TSynapseSockTh.Execute;
const
  memSiz: Integer = 1024 * 1024;
var
  buff: Pointer;
  siz: Integer;
begin
  FreeOnTerminate := True;
  Sock := TTCPBlockSocket.Create;
  Sock.Socket := ClientSockID;
  Sock.GetSins;
  Activted := True;

  buff := System.GetMemory(memSiz);
  while Activted do
    begin
      try
        while Activted and (IO.SendBuffQueue.Count > 0) do
          begin
            CurrentSendBuff := nil;
            SyncMethod(Self, True, {$IFDEF FPC}@{$ENDIF FPC}PickSendBuff);

            if CurrentSendBuff <> nil then
              begin
                Sock.SendBuffer(CurrentSendBuff.Memory, CurrentSendBuff.Size);
                DisposeObject(CurrentSendBuff);
              end;
          end;

        if Activted and Sock.CanRead(100) then
          begin
            siz := Sock.RecvBuffer(buff, memSiz);

            if Sock.LastError <> 0 then
                break;

            if (Activted) and (siz > 0) then
              begin
                IO.SaveReceiveBuffer(buff, siz);
                IO.FillRecvBuffer(Self, True, True);
              end;
          end;
      except
          Activted := False;
      end;
    end;
  System.FreeMemory(buff);
  IO.SockTh := nil;
  DisposeObject(IO);
  DisposeObject(Sock);
end;

procedure TCommunicationFramework_Server_Synapse.All_Disconnect(PeerClient: TPeerIO);
begin
  PeerClient.Disconnect;
end;

constructor TCommunicationFramework_Server_Synapse.Create;
begin
  inherited Create;
  FEnabledAtomicLockAndMultiThread := True;

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
  c := PeerIO[v^.ClientID];
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
  ProgressPerClientM({$IFDEF FPC}@{$ENDIF FPC}All_Disconnect);
end;

function TCommunicationFramework_Server_Synapse.WaitSendConsoleCmd(Client: TPeerIO;
  const Cmd, ConsoleData: SystemString; Timeout: TTimeTickValue): SystemString;
begin
  Result := '';
  RaiseInfo('WaitSend no Suppport');
end;

procedure TCommunicationFramework_Server_Synapse.WaitSendStreamCmd(Client: TPeerIO;
  const Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; Timeout: TTimeTickValue);
begin
  RaiseInfo('WaitSend no Suppport');
end;

initialization

finalization

end.
