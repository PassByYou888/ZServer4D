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
  CommunicationFramework, CoreClasses, UnicodeMixedLib, MemoryStream64,
  NotifyObjectBase;

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

    procedure AsyncConnect(Addr: string; port: Word);
    function Connect(Addr: string; port: Word): Boolean; overload; override;
    function Connect(host: string; port: string): Boolean; overload;
    procedure Disconnect; override;
  end;

  TGlobalCrossSocketClientPool = class
  public
    Driver                   : TDriverEngine;
    LastCompleted, LastResult: Boolean;
    LastConnection           : ICrossConnection;

    constructor Create;
    destructor Destroy;

    procedure CloseAllConnection;

    procedure DoDisconnect(Sender: TObject; AConnection: ICrossConnection);
    procedure DoReceived(Sender: TObject; AConnection: ICrossConnection; ABuf: Pointer; ALen: Integer);
    procedure DoSent(Sender: TObject; AConnection: ICrossConnection; ABuf: Pointer; ALen: Integer);

    function BuildConnect(Addr: string; port: Word; BuildIntf: TCommunicationFramework_Client_CrossSocket): Boolean;
    procedure BuildAsyncConnect(Addr: string; port: Word; BuildIntf: TCommunicationFramework_Client_CrossSocket; StateProc: TStateProc);
  end;

var
  ClientPool: TGlobalCrossSocketClientPool = nil;

implementation


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

procedure TCommunicationFramework_Client_CrossSocket.AsyncConnect(Addr: string; port: Word);
begin
  ClientPool.BuildAsyncConnect(Addr, port, Self, nil);
end;

function TCommunicationFramework_Client_CrossSocket.Connect(Addr: string; port: Word): Boolean;
begin
  Result := ClientPool.BuildConnect(Addr, port, Self);
end;

function TCommunicationFramework_Client_CrossSocket.Connect(host: string; port: string): Boolean;
begin
  Disconnect;
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
  Driver.OnDisconnected := DoDisconnect;
  Driver.OnReceived := DoReceived;
  Driver.OnSent := DoSent;
end;

destructor TGlobalCrossSocketClientPool.Destroy;
begin
  inherited Destroy;
end;

procedure TGlobalCrossSocketClientPool.CloseAllConnection;
begin
  ICrossSocket(Driver).CloseAllConnections;
end;

procedure TGlobalCrossSocketClientPool.DoDisconnect(Sender: TObject; AConnection: ICrossConnection);
begin
  TThread.Synchronize(TThread.CurrentThread,
    procedure
    var
      cli: TContextIntfForClient;
    begin
      if AConnection.UserObject is TContextIntfForClient then
        begin
          cli := AConnection.UserObject as TContextIntfForClient;

          cli.ClientIntf := nil;
          AConnection.UserObject := nil;

          if cli.OwnerClient <> nil then
            begin
              try
                  cli.OwnerClient.DoDisconnect(cli);
              except
              end;
            end;
        end;
    end);
end;

procedure TGlobalCrossSocketClientPool.DoReceived(Sender: TObject; AConnection: ICrossConnection; ABuf: Pointer; ALen: Integer);
begin
  if ALen <= 0 then
      exit;
  if not(AConnection.UserObject is TContextIntfForClient) then
      exit;

  TThread.Synchronize(TThread.CurrentThread,
    procedure
    var
      cli: TContextIntfForClient;
    begin
      try
        cli := AConnection.UserObject as TContextIntfForClient;

        if (cli.ClientIntf = nil) then
            exit;

        while cli.AllSendProcessing do
            TThread.Sleep(1);

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
  if not(AConnection.UserObject is TContextIntfForClient) then
      exit;

  cli := AConnection.UserObject as TContextIntfForClient;

  if (cli.ClientIntf = nil) then
      exit;

  cli.LastActiveTime := GetTimeTickCount;
end;

function TGlobalCrossSocketClientPool.BuildConnect(Addr: string; port: Word; BuildIntf: TCommunicationFramework_Client_CrossSocket): Boolean;
var
  dt : TTimeTickValue;
  cli: TContextIntfForClient;
  i  : Integer;
  lst: TCrossConnections;
begin
  LastResult := False;
  LastCompleted := False;
  LastConnection := nil;

  if BuildIntf.ClientIOIntf <> nil then
    begin
      try
        if BuildIntf.ClientIOIntf.Context <> nil then
            BuildIntf.ClientIOIntf.Context.Close;
      except
      end;
      while BuildIntf.ClientIOIntf <> nil do
        begin
          CheckSynchronize(10);
          BuildIntf.ProgressBackground;
        end;
    end;

  ICrossSocket(Driver).Connect(Addr, port,
    procedure(AConnection: ICrossConnection; ASuccess: Boolean)
    begin
      LastCompleted := True;
      LastResult := ASuccess;
      if LastResult then
          LastConnection := AConnection;
    end);

  TThread.Sleep(3);

  dt := GetTimeTick + 1000;
  while (not LastCompleted) and (GetTimeTick < dt) do
      CheckSynchronize(2);

  if LastResult then
    begin
      cli := TContextIntfForClient.Create(BuildIntf, LastConnection.ConnectionIntf);
      cli.LastActiveTime := GetTimeTickCount;
      cli.OwnerClient := BuildIntf;

      LastConnection.UserObject := cli;

      cli.OwnerClient.ClientIOIntf := cli;
      BuildIntf.DoConnected(cli);
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

procedure TGlobalCrossSocketClientPool.BuildAsyncConnect(Addr: string; port: Word; BuildIntf: TCommunicationFramework_Client_CrossSocket; StateProc: TStateProc);
begin
  try
    if BuildIntf.ClientIOIntf <> nil then
      begin
        try
          if BuildIntf.ClientIOIntf.Context <> nil then
              BuildIntf.ClientIOIntf.Context.Close;
        except
        end;
        while BuildIntf.ClientIOIntf <> nil do
          begin
            try
                BuildIntf.ProgressBackground;
            except
            end;
          end;
      end;
  except
    if Assigned(StateProc) then
        StateProc(False);
    exit;
  end;

  ICrossSocket(Driver).Connect(Addr, port,
    procedure(AConnection: ICrossConnection; ASuccess: Boolean)
    var
      dt: TTimeTickValue;
      cli: TContextIntfForClient;
      i: Integer;
      lst: TCrossConnections;
    begin
      if ASuccess then
        begin
          cli := TContextIntfForClient.Create(BuildIntf, AConnection.ConnectionIntf);
          cli.LastActiveTime := GetTimeTickCount;
          cli.OwnerClient := BuildIntf;

          AConnection.UserObject := cli;

          cli.OwnerClient.ClientIOIntf := cli;

          TThread.Synchronize(TThread.CurrentThread,
            procedure
            begin
              BuildIntf.ProgressPost.PostExecute(0,
                procedure(Sender: TNPostExecute)
                begin
                  BuildIntf.DoConnected(cli);
                end);
            end);
        end
      else
        begin
          BuildAsyncConnect(Addr, port, BuildIntf, StateProc);
        end;
    end);
end;

initialization

finalization

end.
