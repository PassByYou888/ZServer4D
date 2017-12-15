{ ****************************************************************************** }
{ * CrossSocket support                                                        * }
{ * written by QQ 600585@qq.com                                                * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
(* https://github.com/PassByYou888/ZServer4D *)
{ ****************************************************************************** }
(*
  update history
*)
unit CommunicationFramework_Client_CrossSocket;

{$I ..\zDefine.inc}

interface

uses SysUtils, Classes,
  Net.CrossSocket, Net.SocketAPI, Net.CrossSocket.Base,
  CommunicationFramework_Server_CrossSocket,
  CommunicationFramework, CoreClasses, UnicodeMixedLib, MemoryStream64;

type
  TCommunicationFramework_Client_CrossSocket = class;

  TContextIntfForClient = class(TContextIntfForServer)
  public
    OwnerClient: TCommunicationFramework_Client_CrossSocket;
  end;

  TCommunicationFramework_Client_CrossSocket = class(TCommunicationFrameworkClient)
  private
    ClientIOIntf: TContextIntfForClient;
  protected
    procedure DoConnected(Sender: TPeerClient); override;
    procedure DoDisconnect(Sender: TPeerClient); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    function Connected: Boolean; override;
    function ClientIO: TPeerClient; override;
    procedure TriggerQueueData(v: PQueueData); override;
    procedure ProgressBackground; override;

    function Connect(Addr: string; port: Word): Boolean; overload; override;
    function Connect(host: string; port: string): Boolean; overload;
    procedure Disconnect; override;
  end;

implementation

type
  TGlobalCrossSocketClientPool = class
  public
    Driver                   : TDriverEngine;
    CurrentBuildIntf         : TCommunicationFramework_Client_CrossSocket;
    LastCompleted, LastResult: Boolean;
    LastConnection           : ICrossConnection;

    constructor Create;
    destructor Destroy;

    procedure DoConnected(Sender: TObject; AConnection: ICrossConnection);
    procedure DoDisconnect(Sender: TObject; AConnection: ICrossConnection);
    procedure DoReceived(Sender: TObject; AConnection: ICrossConnection; ABuf: Pointer; ALen: Integer);
    procedure DoSent(Sender: TObject; AConnection: ICrossConnection; ABuf: Pointer; ALen: Integer);

    function BuildConnect(Addr: string; port: Word; BuildIntf: TCommunicationFramework_Client_CrossSocket): Boolean;
  end;

var
  ClientPool: TGlobalCrossSocketClientPool = nil;

constructor TCommunicationFramework_Client_CrossSocket.Create;
var
  r: TCommandStreamMode;
begin
  inherited Create;
  if ClientPool = nil then
      ClientPool := TGlobalCrossSocketClientPool.Create;
end;

destructor TCommunicationFramework_Client_CrossSocket.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

function TCommunicationFramework_Client_CrossSocket.Connected: Boolean;
begin
  Result := (ClientIO <> nil) and (ClientIO.Connected);
end;

function TCommunicationFramework_Client_CrossSocket.ClientIO: TPeerClient;
begin
  Result := ClientIOIntf;
end;

procedure TCommunicationFramework_Client_CrossSocket.TriggerQueueData(v: PQueueData);
begin
  if not Connected then
    begin
      DisposeQueueData(v);
      exit;
    end;

  ClientIOIntf.PostQueueData(v);
  ClientIOIntf.FillRecvBuffer(nil, False, False);
end;

procedure TCommunicationFramework_Client_CrossSocket.ProgressBackground;
var
  i: Integer;
begin
  if not Connected then
    begin
      if ClientIOIntf <> nil then
        begin
          DisposeObject(ClientIOIntf);
          ClientIOIntf := nil;
        end;
    end;

  try
    if (IdleTimeout > 0) and (GetTimeTickCount - ClientIOIntf.LastActiveTime > IdleTimeout) then
        Disconnect;
  except
  end;

  try
    if ClientIOIntf <> nil then
        ClientIOIntf.FillRecvBuffer(nil, False, False);
    if ClientIOIntf <> nil then
        ClientIOIntf.ProcessAllSendCmd(nil, False, False);
  except
  end;

  inherited ProgressBackground;

  CheckSynchronize;
end;

function TCommunicationFramework_Client_CrossSocket.Connect(Addr: string; port: Word): Boolean;
begin
  Result := ClientPool.BuildConnect(Addr, port, Self);
end;

function TCommunicationFramework_Client_CrossSocket.Connect(host: string; port: string): Boolean;
begin
  Result := Connect(host, umlStrToInt(port, 0));
end;

procedure TCommunicationFramework_Client_CrossSocket.Disconnect;
begin
  if Connected then
      ClientIO.Disconnect;
end;

procedure TCommunicationFramework_Client_CrossSocket.DoConnected(Sender: TPeerClient);
begin
  inherited DoConnected(Sender);
end;

procedure TCommunicationFramework_Client_CrossSocket.DoDisconnect(Sender: TPeerClient);
begin
  inherited DoDisconnect(Sender);
end;

constructor TGlobalCrossSocketClientPool.Create;
begin
  inherited Create;
  Driver := TDriverEngine.Create(0);
  Driver.OnConnected := DoConnected;
  Driver.OnDisconnected := DoDisconnect;
  Driver.OnReceived := DoReceived;
  Driver.OnSent := DoSent;
  CurrentBuildIntf := nil;
end;

destructor TGlobalCrossSocketClientPool.Destroy;
begin
  inherited Destroy;
end;

procedure TGlobalCrossSocketClientPool.DoConnected(Sender: TObject; AConnection: ICrossConnection);
var
  cli: TContextIntfForClient;
begin
  cli := TContextIntfForClient.Create(CurrentBuildIntf, AConnection.ConnectionIntf);
  cli.LastActiveTime := GetTimeTickCount;
  cli.OwnerClient := CurrentBuildIntf;

  AConnection.UserObject := cli;

  cli.OwnerClient.ClientIOIntf := cli;

  CurrentBuildIntf := nil;
end;

procedure TGlobalCrossSocketClientPool.DoDisconnect(Sender: TObject; AConnection: ICrossConnection);
var
  cli: TContextIntfForClient;
begin
  TThread.Synchronize(TThread.CurrentThread,
    procedure
    begin
      cli := AConnection.UserObject as TContextIntfForClient;
      if cli = nil then
          exit;

      cli.ClientIntf := nil;
      AConnection.UserObject := nil;

      if cli.OwnerClient <> nil then
          cli.OwnerClient.DoDisconnect(cli);
    end);
end;

procedure TGlobalCrossSocketClientPool.DoReceived(Sender: TObject; AConnection: ICrossConnection; ABuf: Pointer; ALen: Integer);
var
  cli: TContextIntfForClient;
begin
  if ALen <= 0 then
      exit;

  cli := AConnection.UserObject as TContextIntfForClient;
  if cli = nil then
      exit;

  if (cli.ClientIntf = nil) then
      exit;

  while cli.AllSendProcessing do
      TThread.Sleep(1);

  TThread.Synchronize(TThread.CurrentThread,
    procedure
    begin
      try
        cli.LastActiveTime := GetTimeTickCount;
        cli.ReceivedBuffer.Position := cli.ReceivedBuffer.size;
        cli.ReceivedBuffer.Write(ABuf^, ALen);
        cli.FillRecvBuffer(nil, False, False);
      except
      end;
    end);
end;

procedure TGlobalCrossSocketClientPool.DoSent(Sender: TObject; AConnection: ICrossConnection; ABuf: Pointer; ALen: Integer);
var
  cli: TContextIntfForClient;
begin
  cli := AConnection.UserObject as TContextIntfForClient;
  if cli = nil then
      exit;

  if (cli.ClientIntf = nil) then
      exit;

  cli.LastActiveTime := GetTimeTickCount;
end;

function TGlobalCrossSocketClientPool.BuildConnect(Addr: string; port: Word; BuildIntf: TCommunicationFramework_Client_CrossSocket): Boolean;
var
  dt: TTimeTickValue;
begin
  CurrentBuildIntf := BuildIntf;

  LastResult := False;
  LastCompleted := False;
  LastConnection := nil;

  ICrossSocket(Driver).Connect(Addr, port,
    procedure(AConnection: ICrossConnection; ASuccess: Boolean)
    begin
      LastCompleted := True;
      LastResult := ASuccess;
      if LastResult then
          LastConnection := AConnection;
    end);

  dt := GetTimeTick + 1000;
  while (not LastCompleted) and (GetTimeTick < dt) do
      CheckSynchronize(1);

  if LastResult then
    begin
      BuildIntf.DoConnected(BuildIntf.ClientIOIntf);
    end;

  dt := GetTimeTick + 1000;
  while (LastCompleted) and (LastResult) and (not BuildIntf.RemoteInited) do
    begin
      BuildIntf.ProgressBackground;
      if GetTimeTick > dt then
        begin
          if LastConnection <> nil then
              LastConnection.Disconnect;
          exit(BuildConnect(Addr, port, BuildIntf));
        end;
    end;

  Result := BuildIntf.RemoteInited;
end;

initialization

finalization

end.
