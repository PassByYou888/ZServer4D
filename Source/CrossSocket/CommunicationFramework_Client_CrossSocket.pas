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

interface

uses SysUtils, Classes,
  NET.CrossSocket, NET.SocketAPI, NET.CrossSocket.Base,
  PascalStrings,
  CommunicationFramework_Server_CrossSocket,
  CommunicationFramework, CoreClasses, UnicodeMixedLib, MemoryStream64,
  NotifyObjectBase;

type
  TCommunicationFramework_Client_CrossSocket = class;

  TContextIntfForClient = class(TPeerIOWithCrossSocketServer)
  public
    OwnerClient: TCommunicationFramework_Client_CrossSocket;
  end;

  TCommunicationFramework_Client_CrossSocket = class(TCommunicationFrameworkClient)
  private
    ClientIOIntf: TContextIntfForClient;

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
  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := nil;
  FOnAsyncConnectNotifyProc := nil;
end;

destructor TCommunicationFramework_Client_CrossSocket.Destroy;
begin
  Disconnect;
  if (ClientIO <> nil) then
      TContextIntfForClient(ClientIO).OwnerClient := nil;
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
  if not Connected then
    begin
      if ClientIOIntf <> nil then
        begin
          DisposeObject(ClientIOIntf);
          ClientIOIntf := nil;
        end;
    end;

  try
    if (Connected) and (IdleTimeout > 0) and (GetTimeTickCount - ClientIOIntf.LastActiveTime > IdleTimeout) then
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

                DisposeObject(cli);
                cli.OwnerClient.ClientIOIntf := nil;
              except
              end;
            end;

        end;
    end);
end;

procedure TGlobalCrossSocketClientPool.DoReceived(Sender: TObject; AConnection: ICrossConnection; aBuf: Pointer; ALen: Integer);
var
  cli: TContextIntfForClient;
begin
  if ALen <= 0 then
      Exit;
  if not(AConnection.UserObject is TContextIntfForClient) then
      Exit;

  TThread.Synchronize(TThread.CurrentThread,
    procedure()
    begin
      try
        cli := AConnection.UserObject as TContextIntfForClient;

        if cli = nil then
            Exit;

        if (cli.ClientIntf = nil) then
            Exit;

        while cli.AllSendProcessing do
            TThread.Sleep(1);

        cli.LastActiveTime := GetTimeTickCount;
        cli.SaveReceiveBuffer(aBuf, ALen);
        cli.FillRecvBuffer(nil, False, False);
      except
      end;
    end);
end;

procedure TGlobalCrossSocketClientPool.DoSent(Sender: TObject; AConnection: ICrossConnection; aBuf: Pointer; ALen: Integer);
var
  cli: TContextIntfForClient;
begin
  if not(AConnection.UserObject is TContextIntfForClient) then
      Exit;

  cli := AConnection.UserObject as TContextIntfForClient;

  if (cli.ClientIntf = nil) then
      Exit;

  cli.LastActiveTime := GetTimeTickCount;
end;

function TGlobalCrossSocketClientPool.BuildConnect(addr: SystemString; Port: Word; BuildIntf: TCommunicationFramework_Client_CrossSocket): Boolean;
var
  dt: TTimeTickValue;
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

  TThread.Sleep(10);

  dt := GetTimeTick + 1000;
  while (not LastCompleted) and (GetTimeTick < dt) do
      CheckThreadSynchronize(5);

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
              BuildIntf.ProgressPost.PostExecuteP(0, procedure(Sender: TNPostExecute)
                begin
                  BuildIntf.DoConnected(cli);
                end);
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
