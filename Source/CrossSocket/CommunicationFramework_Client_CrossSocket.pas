{ ****************************************************************************** }
{ * CrossSocket support                                                        * }
{ * written by QQ 600585@qq.com                                                * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ ****************************************************************************** }
(*
  update history
*)
unit CommunicationFramework_Client_CrossSocket;

{$I ..\zDefine.inc}

interface

uses SysUtils, Classes,
  Net.CrossSocket, Net.SocketAPI, Net.CrossSocket.Base,
  PascalStrings,
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

    FOnAsyncConnectNotifyCall  : TStateCall;
    FOnAsyncConnectNotifyMethod: TStateMethod;
    FOnAsyncConnectNotifyProc  : TStateProc;

    procedure AsyncConnect(Addr: SystemString; Port: Word; OnResultCall: TStateCall; OnResultMethod: TStateMethod; OnResultProc: TStateProc); overload;
  protected
    procedure DoConnected(Sender: TPeerIO); override;
    procedure DoDisconnect(Sender: TPeerIO); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure TriggerDoConnectFailed; override;
    procedure TriggerDoConnectFinished; override;

    function Connected: Boolean; override;
    function ClientIO: TPeerIO; override;
    procedure TriggerQueueData(v: PQueueData); override;
    procedure ProgressBackground; override;

    procedure AsyncConnect(Addr: SystemString; Port: Word); overload;
    procedure AsyncConnect(Addr: SystemString; Port: Word; OnResult: TStateCall); overload; override;
    procedure AsyncConnect(Addr: SystemString; Port: Word; OnResult: TStateMethod); overload; override;
    procedure AsyncConnect(Addr: SystemString; Port: Word; OnResult: TStateProc); overload; override;

    function Connect(Addr: SystemString; Port: Word): Boolean; overload; override;
    function Connect(host: SystemString; Port: SystemString): Boolean; overload;
    procedure Disconnect; override;
  end;

  TGlobalCrossSocketClientPool = class
  private
    Driver                   : TDriverEngine;
    LastCompleted, LastResult: Boolean;
    LastConnection           : ICrossConnection;
  public
    autoReconnect: Boolean;

    constructor Create;
    destructor Destroy; override;

    procedure CloseAllConnection;

    procedure DoDisconnect(Sender: TObject; AConnection: ICrossConnection);
    procedure DoReceived(Sender: TObject; AConnection: ICrossConnection; ABuf: Pointer; ALen: Integer);
    procedure DoSent(Sender: TObject; AConnection: ICrossConnection; ABuf: Pointer; ALen: Integer);

    function BuildConnect(Addr: SystemString; Port: Word; BuildIntf: TCommunicationFramework_Client_CrossSocket): Boolean;
    procedure BuildAsyncConnect(Addr: SystemString; Port: Word; BuildIntf: TCommunicationFramework_Client_CrossSocket);
  end;

var
  ClientPool: TGlobalCrossSocketClientPool = nil;

implementation


procedure TCommunicationFramework_Client_CrossSocket.AsyncConnect(Addr: SystemString; Port: Word; OnResultCall: TStateCall; OnResultMethod: TStateMethod; OnResultProc: TStateProc);
begin
  FOnAsyncConnectNotifyCall := OnResultCall;
  FOnAsyncConnectNotifyMethod := OnResultMethod;
  FOnAsyncConnectNotifyProc := OnResultProc;

  ClientPool.BuildAsyncConnect(Addr, Port, Self);
end;

procedure TCommunicationFramework_Client_CrossSocket.DoConnected(Sender: TPeerIO);
begin
  inherited DoConnected(Sender);
end;

procedure TCommunicationFramework_Client_CrossSocket.DoDisconnect(Sender: TPeerIO);
begin
  inherited DoDisconnect(Sender);
end;

constructor TCommunicationFramework_Client_CrossSocket.Create;
begin
  inherited Create;
  if ClientPool = nil then
      ClientPool := TGlobalCrossSocketClientPool.Create;

  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := nil;
  FOnAsyncConnectNotifyProc := nil;
end;

destructor TCommunicationFramework_Client_CrossSocket.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

procedure TCommunicationFramework_Client_CrossSocket.TriggerDoConnectFailed;
begin
  inherited TriggerDoConnectFailed;

  try
    if Assigned(FOnAsyncConnectNotifyCall) then
        FOnAsyncConnectNotifyCall(False);
    if Assigned(FOnAsyncConnectNotifyMethod) then
        FOnAsyncConnectNotifyMethod(False);
    if Assigned(FOnAsyncConnectNotifyProc) then
        FOnAsyncConnectNotifyProc(False);
  except
  end;

  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := nil;
  FOnAsyncConnectNotifyProc := nil;
end;

procedure TCommunicationFramework_Client_CrossSocket.TriggerDoConnectFinished;
begin
  inherited TriggerDoConnectFinished;

  try
    if Assigned(FOnAsyncConnectNotifyCall) then
        FOnAsyncConnectNotifyCall(True);
    if Assigned(FOnAsyncConnectNotifyMethod) then
        FOnAsyncConnectNotifyMethod(True);
    if Assigned(FOnAsyncConnectNotifyProc) then
        FOnAsyncConnectNotifyProc(True);
  except
  end;

  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := nil;
  FOnAsyncConnectNotifyProc := nil;
end;

function TCommunicationFramework_Client_CrossSocket.Connected: Boolean;
begin
  Result := (ClientIO <> nil) and (ClientIO.Connected);
end;

function TCommunicationFramework_Client_CrossSocket.ClientIO: TPeerIO;
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

procedure TCommunicationFramework_Client_CrossSocket.AsyncConnect(Addr: SystemString; Port: Word);
begin
  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := nil;
  FOnAsyncConnectNotifyProc := nil;

  ClientPool.BuildAsyncConnect(Addr, Port, Self);
end;

procedure TCommunicationFramework_Client_CrossSocket.AsyncConnect(Addr: SystemString; Port: Word; OnResult: TStateCall);
begin
  AsyncConnect(Addr, Port, OnResult, nil, nil);
end;

procedure TCommunicationFramework_Client_CrossSocket.AsyncConnect(Addr: SystemString; Port: Word; OnResult: TStateMethod);
begin
  AsyncConnect(Addr, Port, nil, OnResult, nil);
end;

procedure TCommunicationFramework_Client_CrossSocket.AsyncConnect(Addr: SystemString; Port: Word; OnResult: TStateProc);
begin
  AsyncConnect(Addr, Port, nil, nil, OnResult);
end;

function TCommunicationFramework_Client_CrossSocket.Connect(Addr: SystemString; Port: Word): Boolean;
begin
  Result := ClientPool.BuildConnect(Addr, Port, Self);
end;

function TCommunicationFramework_Client_CrossSocket.Connect(host: SystemString; Port: SystemString): Boolean;
begin
  Result := Connect(host, umlStrToInt(Port, 0));
end;

procedure TCommunicationFramework_Client_CrossSocket.Disconnect;
begin
  if Connected then
      ClientIO.Disconnect;

  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := nil;
  FOnAsyncConnectNotifyProc := nil;
end;

constructor TGlobalCrossSocketClientPool.Create;
begin
  inherited Create;
  Driver := TDriverEngine.Create(0);
  Driver.OnDisconnected := DoDisconnect;
  Driver.OnReceived := DoReceived;
  Driver.OnSent := DoSent;

  autoReconnect := False;
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

        if cli = nil then
            exit;

        if (cli.ClientIntf = nil) then
            exit;

        while cli.AllSendProcessing do
            TThread.Sleep(1);

        cli.LastActiveTime := GetTimeTickCount;
        cli.SaveReceiveBuffer(ABuf, ALen);
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

function TGlobalCrossSocketClientPool.BuildConnect(Addr: SystemString; Port: Word; BuildIntf: TCommunicationFramework_Client_CrossSocket): Boolean;
var
  dt : TTimeTickValue;
  cli: TContextIntfForClient;
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

  ICrossSocket(Driver).Connect(Addr, Port,
    procedure(AConnection: ICrossConnection; ASuccess: Boolean)
    begin
      LastCompleted := True;
      LastResult := ASuccess;
      if LastResult then
          LastConnection := AConnection;
    end);

  TThread.Sleep(10);

  dt := GetTimeTick + 1000;
  while (not LastCompleted) and (GetTimeTick < dt) do
      CheckSynchronize(5);

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
          break;
        end;
    end;

  Result := BuildIntf.RemoteInited;

  if (not Result) and (autoReconnect) then
      Result := BuildConnect(Addr, Port, BuildIntf);
end;

procedure TGlobalCrossSocketClientPool.BuildAsyncConnect(Addr: SystemString; Port: Word; BuildIntf: TCommunicationFramework_Client_CrossSocket);
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
    BuildIntf.TriggerDoConnectFailed;
    exit;
  end;

  ICrossSocket(Driver).Connect(Addr, Port,
    procedure(AConnection: ICrossConnection; ASuccess: Boolean)
    var
      cli: TContextIntfForClient;
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
              BuildIntf.ProgressPost.PostExecute(0, procedure(Sender: TNPostExecute)
                begin
                  BuildIntf.DoConnected(cli);
                end);
            end);
        end
      else
        begin
          if autoReconnect then
            begin
              BuildAsyncConnect(Addr, Port, BuildIntf);
              exit;
            end;
          BuildIntf.TriggerDoConnectFailed;
        end;
    end);
end;

initialization

finalization

end.
