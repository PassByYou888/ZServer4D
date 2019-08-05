{ ****************************************************************************** }
{ * FPC Synpase client Support                                                 * }
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
  update history
*)

unit CommunicationFramework_Client_Synapse;

{$INCLUDE ..\zDefine.inc}

interface

uses SysUtils, Classes,
  PascalStrings, CommunicationFramework, CoreClasses, UnicodeMixedLib, MemoryStream64, NotifyObjectBase,
  synsock, blcksock;

type
  TCommunicationFramework_Client_Synapse = class;

  TSynapseClient_PeerIO = class(TPeerIO)
  protected
    LastPeerIP: SystemString;
    SendBuffQueue: TCoreClassListForObj;
    CurrentBuff: TMemoryStream64;
  public
    procedure CreateAfter; override;
    destructor Destroy; override;

    function Context: TCommunicationFramework_Client_Synapse;

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

  TCommunicationFramework_Client_Synapse = class(TCommunicationFrameworkClient)
  private
    Sock: TTCPBlockSocket;
    InternalClient: TSynapseClient_PeerIO;
    SockConnected: Boolean;
  protected
    procedure DoConnected(Sender: TPeerIO); override;
    procedure DoDisconnect(Sender: TPeerIO); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    function Connected: Boolean; override;
    function ClientIO: TPeerIO; override;
    procedure TriggerQueueData(v: PQueueData); override;
    procedure Progress; override;

    function Connect(addr: SystemString; Port: Word): Boolean; overload; override;
    function Connect(Host: SystemString; Port: SystemString): Boolean; overload;
    procedure Disconnect; override;
  end;

implementation


procedure TSynapseClient_PeerIO.CreateAfter;
begin
  inherited CreateAfter;
  LastPeerIP := '';
  SendBuffQueue := TCoreClassListForObj.Create;
  CurrentBuff := TMemoryStream64.Create;
end;

destructor TSynapseClient_PeerIO.Destroy;
var
  i: Integer;
begin
  for i := 0 to SendBuffQueue.Count - 1 do
      DisposeObject(SendBuffQueue[i]);

  DisposeObject(SendBuffQueue);

  DisposeObject(CurrentBuff);
  inherited Destroy;
end;

function TSynapseClient_PeerIO.Context: TCommunicationFramework_Client_Synapse;
begin
  Result := OwnerFramework as TCommunicationFramework_Client_Synapse;
end;

function TSynapseClient_PeerIO.Connected: Boolean;
begin
  Result := Context.SockConnected;
end;

procedure TSynapseClient_PeerIO.Disconnect;
begin
  Context.Disconnect;
end;

procedure TSynapseClient_PeerIO.SendByteBuffer(const buff: PByte; const Size: nativeInt);
begin
  if not Connected then
      Exit;

  if Size > 0 then
      CurrentBuff.write(Pointer(buff)^, Size);
end;

procedure TSynapseClient_PeerIO.WriteBufferOpen;
begin
  if not Connected then
      Exit;
  CurrentBuff.Clear;
end;

procedure TSynapseClient_PeerIO.WriteBufferFlush;
begin
  if not Connected then
      Exit;
  if CurrentBuff.Size > 0 then
    begin
      SendBuffQueue.Add(CurrentBuff);
      CurrentBuff := TMemoryStream64.Create;
    end;
end;

procedure TSynapseClient_PeerIO.WriteBufferClose;
begin
  if not Connected then
      Exit;
  CurrentBuff.Clear;
end;

function TSynapseClient_PeerIO.GetPeerIP: SystemString;
begin
  if Connected then
    begin
      Result := Context.Sock.GetRemoteSinIP;
      LastPeerIP := Result;
    end
  else
      Result := LastPeerIP;
end;

function TSynapseClient_PeerIO.WriteBufferEmpty: Boolean;
begin
  Result := SendBuffQueue.Count = 0;
end;

procedure TSynapseClient_PeerIO.Progress;
begin
  inherited Progress;
  ProcessAllSendCmd(nil, False, False);
end;

procedure TCommunicationFramework_Client_Synapse.DoConnected(Sender: TPeerIO);
begin
  inherited DoConnected(Sender);
end;

procedure TCommunicationFramework_Client_Synapse.DoDisconnect(Sender: TPeerIO);
begin
  inherited DoDisconnect(Sender);
end;

constructor TCommunicationFramework_Client_Synapse.Create;
begin
  inherited Create;
  FEnabledAtomicLockAndMultiThread := False;

  Sock := TTCPBlockSocket.Create;
  Sock.Family := TSocketFamily.SF_IP4;
  Sock.CreateSocket;
  InternalClient := TSynapseClient_PeerIO.Create(Self, Sock);
  SockConnected := False;
end;

destructor TCommunicationFramework_Client_Synapse.Destroy;
begin
  Sock.CloseSocket;
  DisposeObject(InternalClient);
  DisposeObject(Sock);
  inherited Destroy;
end;

function TCommunicationFramework_Client_Synapse.Connected: Boolean;
begin
  Result := SockConnected;
end;

function TCommunicationFramework_Client_Synapse.ClientIO: TPeerIO;
begin
  Result := InternalClient;
end;

procedure TCommunicationFramework_Client_Synapse.TriggerQueueData(v: PQueueData);
begin
  if not Connected then
    begin
      DisposeQueueData(v);
      Exit;
    end;

  ClientIO.PostQueueData(v);
  ClientIO.ProcessAllSendCmd(nil, False, False);
end;

procedure TCommunicationFramework_Client_Synapse.Progress;
const
  memSiz: Integer = 64 * 1024;
label go_recv;
var
  CurrentSendBuff: TMemoryStream64;
  buff: Pointer;
  siz: Integer;
begin
  inherited Progress;

  if not Connected then
      Exit;

  while (InternalClient.SendBuffQueue.Count > 0) do
    begin
      CurrentSendBuff := TMemoryStream64(InternalClient.SendBuffQueue[0]);
      InternalClient.SendBuffQueue.Delete(0);

      try
          Sock.SendBuffer(CurrentSendBuff.Memory, CurrentSendBuff.Size);
      except
        Disconnect;
        DisposeObject(CurrentSendBuff);
        Exit;
      end;
      DisposeObject(CurrentSendBuff);
    end;

  buff := System.GetMemory(memSiz);

go_recv:
  try
    if Sock.CanRead(1) then
        siz := Sock.RecvBuffer(buff, memSiz)
    else
        siz := 0;
  except
    Disconnect;
    System.FreeMemory(buff);
    Exit;
  end;

  if Sock.LastError <> 0 then
    begin
      Disconnect;
      System.FreeMemory(buff);
      Exit;
    end;

  if (siz > 0) then
    begin
      InternalClient.SaveReceiveBuffer(buff, siz);
      InternalClient.FillRecvBuffer(nil, False, False);
      goto go_recv;
    end;

  System.FreeMemory(buff);
end;

function TCommunicationFramework_Client_Synapse.Connect(addr: SystemString; Port: Word): Boolean;
var
  AStopTime: TTimeTick;
begin
  Result := False;
  try
    Sock.Connect(addr, IntToStr(Port));
    SockConnected := Sock.LastError = 0;
    if not SockConnected then
      begin
        Sock.CloseSocket;
        if Sock.Family = TSocketFamily.SF_IP4 then
            Sock.Family := TSocketFamily.SF_IP6
        else
            Sock.Family := TSocketFamily.SF_IP4;
        Sock.CreateSocket;
        Sock.Connect(addr, IntToStr(Port));
        SockConnected := Sock.LastError = 0;
        if not SockConnected then
            Exit;
      end;

    DoConnected(InternalClient);

    AStopTime := GetTimeTick + 3000;

    while (not RemoteInited) and Connected do
      begin
        Progress;
        if GetTimeTick > AStopTime then
          begin
            Disconnect;
            Exit;
          end;
      end;

    Result := RemoteInited;
  except
      Result := False;
  end;
end;

function TCommunicationFramework_Client_Synapse.Connect(Host: SystemString; Port: SystemString): Boolean;
begin
  Result := Connect(Host, umlStrToInt(Port, 0));
end;

procedure TCommunicationFramework_Client_Synapse.Disconnect;
begin
  if SockConnected then
      DoDisconnect(InternalClient);
  SockConnected := False;
  Sock.CloseSocket;
  Sock.CreateSocket;
  DisposeObject(InternalClient);
  InternalClient := TSynapseClient_PeerIO.Create(Self, Sock);
end;

initialization

finalization

end.
