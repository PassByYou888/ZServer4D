{ ****************************************************************************** }
{ * CrossSocket support                                                        * }
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
unit CommunicationFramework_Client_CrossSocket;

{$INCLUDE ..\zDefine.inc}

{
  CrossSocket 客户端的高性能模式，试验阶段!
  在高性能模式下，多核会被充分调动，但是异步IO会发生的中断情况
  关闭该选项后，客户端会极其稳定，IO调度会多核，但是数据吞吐全在主线来干
  该选项对多连接，并发数无任何影响，只影响cpu工作能力
}
{$UNDEF CrossSocketClient_HighPerformance}

interface

uses SysUtils, Classes,
  NET.CrossSocket, NET.SocketAPI, NET.CrossSocket.Base,
  PascalStrings,
  CommunicationFramework_Server_CrossSocket,
  CommunicationFramework, CoreClasses, UnicodeMixedLib, MemoryStream64,
  NotifyObjectBase;

type
  TCommunicationFramework_Client_CrossSocket = class;

  TCrossSocketClient_PeerIO = class(TCrossSocketServer_PeerIO)
  public
    OwnerClient: TCommunicationFramework_Client_CrossSocket;
  end;

  TCommunicationFramework_Client_CrossSocket = class(TCommunicationFrameworkClient)
  private
    ClientIOIntf: TCrossSocketClient_PeerIO;

    FOnAsyncConnectNotifyCall: TStateCall;
    FOnAsyncConnectNotifyMethod: TStateMethod;
    FOnAsyncConnectNotifyProc: TStateProc;

    procedure AsyncConnect(addr: SystemString; Port: Word; OnResultCall: TStateCall; OnResultMethod: TStateMethod; OnResultProc: TStateProc); overload;
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
    procedure Progress; override;

    procedure AsyncConnect(addr: SystemString; Port: Word); overload;
    procedure AsyncConnectC(addr: SystemString; Port: Word; OnResult: TStateCall); overload; override;
    procedure AsyncConnectM(addr: SystemString; Port: Word; OnResult: TStateMethod); overload; override;
    procedure AsyncConnectP(addr: SystemString; Port: Word; OnResult: TStateProc); overload; override;

    function Connect(addr: SystemString; Port: Word): Boolean; overload; override;
    function Connect(Host: SystemString; Port: SystemString): Boolean; overload;
    procedure Disconnect; override;
  end;

  TGlobalCrossSocketClientPool = class
  private
    LastCompleted, LastResult: Boolean;
    LastConnection: ICrossConnection;
  public
    driver: TDriverEngine;
    AutoReconnect: Boolean;

    constructor Create;
    destructor Destroy; override;

    procedure CloseAllConnection;

    procedure DoDisconnect(Sender: TObject; AConnection: ICrossConnection);
    procedure DoReceived(Sender: TObject; AConnection: ICrossConnection; aBuf: Pointer; ALen: Integer);
    procedure DoSent(Sender: TObject; AConnection: ICrossConnection; aBuf: Pointer; ALen: Integer);

    function BuildConnect(addr: SystemString; Port: Word; BuildIntf: TCommunicationFramework_Client_CrossSocket): Boolean;
    procedure BuildAsyncConnect(addr: SystemString; Port: Word; BuildIntf: TCommunicationFramework_Client_CrossSocket);
  end;

var
  ClientPool: TGlobalCrossSocketClientPool = nil;

implementation


procedure TCommunicationFramework_Client_CrossSocket.AsyncConnect(addr: SystemString; Port: Word; OnResultCall: TStateCall; OnResultMethod: TStateMethod; OnResultProc: TStateProc);
begin
  FOnAsyncConnectNotifyCall := OnResultCall;
  FOnAsyncConnectNotifyMethod := OnResultMethod;
  FOnAsyncConnectNotifyProc := OnResultProc;

  ClientPool.BuildAsyncConnect(addr, Port, Self);
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
  FEnabledAtomicLockAndMultiThread := {$IFDEF CrossSocketClient_HighPerformance}True; {$ELSE}False; {$ENDIF}
  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := nil;
  FOnAsyncConnectNotifyProc := nil;
end;

destructor TCommunicationFramework_Client_CrossSocket.Destroy;
begin
  Disconnect;
  if (ClientIO <> nil) then
      TCrossSocketClient_PeerIO(ClientIO).OwnerClient := nil;
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
      Exit;
    end;

  ClientIOIntf.PostQueueData(v);
  ClientIOIntf.FillRecvBuffer(nil, False, False);
end;

procedure TCommunicationFramework_Client_CrossSocket.Progress;
begin
  inherited Progress;

  try
      CoreClasses.CheckThreadSynchronize;
  except
  end;
end;

procedure TCommunicationFramework_Client_CrossSocket.AsyncConnect(addr: SystemString; Port: Word);
begin
  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := nil;
  FOnAsyncConnectNotifyProc := nil;

  ClientPool.BuildAsyncConnect(addr, Port, Self);
end;

procedure TCommunicationFramework_Client_CrossSocket.AsyncConnectC(addr: SystemString; Port: Word; OnResult: TStateCall);
begin
  AsyncConnect(addr, Port, OnResult, nil, nil);
end;

procedure TCommunicationFramework_Client_CrossSocket.AsyncConnectM(addr: SystemString; Port: Word; OnResult: TStateMethod);
begin
  AsyncConnect(addr, Port, nil, OnResult, nil);
end;

procedure TCommunicationFramework_Client_CrossSocket.AsyncConnectP(addr: SystemString; Port: Word; OnResult: TStateProc);
begin
  AsyncConnect(addr, Port, nil, nil, OnResult);
end;

function TCommunicationFramework_Client_CrossSocket.Connect(addr: SystemString; Port: Word): Boolean;
begin
  Result := ClientPool.BuildConnect(addr, Port, Self);
end;

function TCommunicationFramework_Client_CrossSocket.Connect(Host: SystemString; Port: SystemString): Boolean;
begin
  Result := Connect(Host, umlStrToInt(Port, 0));
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
  driver := TDriverEngine.Create(0);
  driver.OnDisconnected := DoDisconnect;
  driver.OnReceived := DoReceived;
  driver.OnSent := DoSent;

  AutoReconnect := False;
end;

destructor TGlobalCrossSocketClientPool.Destroy;
begin
  DisposeObject(driver);
  inherited Destroy;
end;

procedure TGlobalCrossSocketClientPool.CloseAllConnection;
begin
  ICrossSocket(driver).CloseAllConnections;
end;

procedure TGlobalCrossSocketClientPool.DoDisconnect(Sender: TObject; AConnection: ICrossConnection);
begin
  TCoreClassThread.Synchronize(TCoreClassThread.CurrentThread,
    procedure
    var
      cli: TCrossSocketClient_PeerIO;
    begin
      if AConnection.UserObject is TCrossSocketClient_PeerIO then
        begin
          cli := AConnection.UserObject as TCrossSocketClient_PeerIO;

          cli.IOInterface := nil;
          AConnection.UserObject := nil;

          if cli.OwnerClient <> nil then
            begin
              try
                cli.OwnerClient.DoDisconnect(cli);
                cli.OwnerClient.ClientIOIntf := nil;
                DisposeObject(cli);
              except
              end;
            end;
        end;
      AConnection.UserObject := nil;
    end);
end;

procedure TGlobalCrossSocketClientPool.DoReceived(Sender: TObject; AConnection: ICrossConnection; aBuf: Pointer; ALen: Integer);
var
  cli: TCrossSocketClient_PeerIO;
begin
  if ALen <= 0 then
      Exit;
  if not(AConnection.UserObject is TCrossSocketClient_PeerIO) then
      Exit;

  cli := AConnection.UserObject as TCrossSocketClient_PeerIO;

  if cli = nil then
      Exit;

  if (cli.IOInterface = nil) then
      Exit;

  if cli.OwnerClient.FEnabledAtomicLockAndMultiThread then
    begin
      cli.SaveReceiveBuffer(aBuf, ALen);
      cli.FillRecvBuffer(TCoreClassThread.CurrentThread, True, True);
    end
  else
    begin
      TCoreClassThread.Synchronize(TCoreClassThread.CurrentThread, procedure
        begin
          try
            cli.SaveReceiveBuffer(aBuf, ALen);
            cli.FillRecvBuffer(nil, False, False);
          except
          end;
        end);
    end;
end;

procedure TGlobalCrossSocketClientPool.DoSent(Sender: TObject; AConnection: ICrossConnection; aBuf: Pointer; ALen: Integer);
var
  cli: TCrossSocketClient_PeerIO;
begin
  if not(AConnection.UserObject is TCrossSocketClient_PeerIO) then
      Exit;

  cli := AConnection.UserObject as TCrossSocketClient_PeerIO;

  if (cli.IOInterface = nil) then
      Exit;
end;

function TGlobalCrossSocketClientPool.BuildConnect(addr: SystemString; Port: Word; BuildIntf: TCommunicationFramework_Client_CrossSocket): Boolean;
var
  dt: TTimeTick;
  cli: TCrossSocketClient_PeerIO;
begin
  LastResult := False;
  LastCompleted := False;
  LastConnection := nil;

  if BuildIntf.ClientIOIntf <> nil then
      CoreClasses.CheckThreadSynchronize(10);

  if BuildIntf.ClientIOIntf <> nil then
    begin
      try
        if BuildIntf.ClientIOIntf.Context <> nil then
            BuildIntf.ClientIOIntf.Context.Close;
      except
      end;
      while BuildIntf.ClientIOIntf <> nil do
        begin
          CheckThreadSynchronize(10);
          BuildIntf.Progress;
        end;
    end;

  ICrossSocket(driver).Connect(addr, Port,
    procedure(AConnection: ICrossConnection; ASuccess: Boolean)
    begin
      LastCompleted := True;
      LastResult := ASuccess;
      if LastResult then
          LastConnection := AConnection;
    end);

  TCoreClassThread.Sleep(10);

  dt := GetTimeTick + 2000;
  while (not LastCompleted) and (GetTimeTick < dt) do
      CheckThreadSynchronize(5);

  if LastResult then
    begin
      cli := TCrossSocketClient_PeerIO.Create(BuildIntf, LastConnection.ConnectionIntf);
      cli.OwnerClient := BuildIntf;

      LastConnection.UserObject := cli;
      cli.OwnerClient.ClientIOIntf := cli;
      BuildIntf.DoConnected(cli);
    end;

  dt := GetTimeTick + 2000;
  while (LastCompleted) and (LastResult) and (not BuildIntf.RemoteInited) do
    begin
      BuildIntf.Progress;
      if GetTimeTick > dt then
        begin
          if LastConnection <> nil then
              LastConnection.Disconnect;
          Break;
        end;
    end;

  Result := BuildIntf.RemoteInited;

  if (not Result) and (AutoReconnect) then
      Result := BuildConnect(addr, Port, BuildIntf);
end;

procedure TGlobalCrossSocketClientPool.BuildAsyncConnect(addr: SystemString; Port: Word; BuildIntf: TCommunicationFramework_Client_CrossSocket);
begin
  try
    if BuildIntf.ClientIOIntf <> nil then
        CoreClasses.CheckThreadSynchronize(10);
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
                BuildIntf.Progress;
            except
            end;
          end;
      end;
  except
    BuildIntf.TriggerDoConnectFailed;
    Exit;
  end;

  ICrossSocket(driver).Connect(addr, Port,
    procedure(AConnection: ICrossConnection; ASuccess: Boolean)
    begin
      if ASuccess then
        begin
          TCoreClassThread.Synchronize(TCoreClassThread.CurrentThread,
            procedure
            var
              cli: TCrossSocketClient_PeerIO;
            begin
              cli := TCrossSocketClient_PeerIO.Create(BuildIntf, AConnection.ConnectionIntf);
              cli.OwnerClient := BuildIntf;
              AConnection.UserObject := cli;
              cli.OwnerClient.ClientIOIntf := cli;
              BuildIntf.DoConnected(cli);
            end);
        end
      else
        begin
          if AutoReconnect then
            begin
              BuildAsyncConnect(addr, Port, BuildIntf);
              Exit;
            end;
          BuildIntf.TriggerDoConnectFailed;
        end;
    end);
end;

initialization

ClientPool := TGlobalCrossSocketClientPool.Create;

finalization

DisposeObject(ClientPool);
ClientPool := nil;

end.

