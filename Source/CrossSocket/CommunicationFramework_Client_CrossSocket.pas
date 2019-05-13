{ ****************************************************************************** }
{ * CrossSocket support                                                        * }
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

  TCrossSocketClient_PeerIO = class(TCrossSocketServer_PeerIO)
  public
    OwnerClient: TCommunicationFramework_Client_CrossSocket;
    procedure CreateAfter; override;
    destructor Destroy; override;
    procedure Disconnect; override;
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
    procedure DoSendBuffResult(AConnection: ICrossConnection; ASuccess: Boolean);

    function BuildConnect(addr: SystemString; Port: Word; BuildIntf: TCommunicationFramework_Client_CrossSocket): Boolean;
    procedure BuildAsyncConnect(addr: SystemString; Port: Word; BuildIntf: TCommunicationFramework_Client_CrossSocket);
  end;

var
  ClientPool: TGlobalCrossSocketClientPool = nil;

implementation

procedure TCrossSocketClient_PeerIO.CreateAfter;
begin
  inherited CreateAfter;
  OwnerClient := nil;
end;

destructor TCrossSocketClient_PeerIO.Destroy;
begin
  if OwnerClient <> nil then
    begin
      OwnerClient.DoDisconnect(Self);
      OwnerClient.ClientIOIntf := nil;
    end;
  OwnerClient := nil;
  inherited Destroy;
end;

procedure TCrossSocketClient_PeerIO.Disconnect;
begin
  if OwnerClient <> nil then
    begin
      OwnerClient.DoDisconnect(Self);
      OwnerClient.ClientIOIntf := nil;
    end;
  OwnerClient := nil;
  inherited Disconnect;
end;

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
  FEnabledAtomicLockAndMultiThread := False;
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
    begin
      ClientIO.Disconnect;
    end;

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

  AutoReconnect := False;

  ClientPool := Self;
end;

destructor TGlobalCrossSocketClientPool.Destroy;
begin
  try
      ICrossSocket(driver).DisconnectAll;
  except
  end;
  DisposeObject(driver);
  ClientPool := nil;
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
      p_io: TCrossSocketClient_PeerIO;
    begin
      if AConnection.UserObject is TCrossSocketClient_PeerIO then
        begin
          p_io := TCrossSocketClient_PeerIO(AConnection.UserObject);

          if p_io = nil then
              Exit;

          if p_io.OwnerClient <> nil then
            begin
              try
                  DisposeObject(p_io);
              except
              end;
            end;

          p_io.IOInterface := nil;
          AConnection.UserObject := nil;
        end;
    end);
end;

procedure TGlobalCrossSocketClientPool.DoReceived(Sender: TObject; AConnection: ICrossConnection; aBuf: Pointer; ALen: Integer);
var
  p_io: TCrossSocketClient_PeerIO;
begin
  if ALen <= 0 then
      Exit;
  if not(AConnection.UserObject is TCrossSocketClient_PeerIO) then
      Exit;

  p_io := TCrossSocketClient_PeerIO(AConnection.UserObject);

  if p_io = nil then
      Exit;

  if (p_io.IOInterface = nil) then
      Exit;

  TCoreClassThread.Synchronize(TCoreClassThread.CurrentThread, procedure
    begin
      try
        p_io.SaveReceiveBuffer(aBuf, ALen);
        p_io.FillRecvBuffer(nil, False, False);
      except
      end;
    end);
end;

procedure TGlobalCrossSocketClientPool.DoSendBuffResult(AConnection: ICrossConnection; ASuccess: Boolean);
var
  p_io: TCrossSocketClient_PeerIO;
begin
  if not(AConnection.UserObject is TCrossSocketClient_PeerIO) then
      Exit;

  p_io := TCrossSocketClient_PeerIO(AConnection.UserObject);

  if p_io = nil then
      Exit;
  p_io.SendBuffResult(ASuccess);
end;

function TGlobalCrossSocketClientPool.BuildConnect(addr: SystemString; Port: Word; BuildIntf: TCommunicationFramework_Client_CrossSocket): Boolean;
var
  dt: TTimeTick;
  p_io: TCrossSocketClient_PeerIO;
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

  dt := GetTimeTick + 3000;
  while (not LastCompleted) and (GetTimeTick < dt) do
      CheckThreadSynchronize(5);

  if LastResult then
    begin
      p_io := TCrossSocketClient_PeerIO.Create(BuildIntf, LastConnection.ConnectionIntf);
      p_io.OwnerClient := BuildIntf;
      LastConnection.UserObject := p_io;
      p_io.OwnerClient.ClientIOIntf := p_io;
      p_io.OnSendBackcall := DoSendBuffResult;
      BuildIntf.DoConnected(p_io);
    end;

  dt := GetTimeTick + 4000;
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
          TCoreClassThread.Synchronize(TCoreClassThread.CurrentThread, procedure
            var
              p_io: TCrossSocketClient_PeerIO;
            begin
              p_io := TCrossSocketClient_PeerIO.Create(BuildIntf, AConnection.ConnectionIntf);
              p_io.OwnerClient := BuildIntf;
              AConnection.UserObject := p_io;
              p_io.OwnerClient.ClientIOIntf := p_io;
              p_io.OnSendBackcall := DoSendBuffResult;
              BuildIntf.DoConnected(p_io);
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

TGlobalCrossSocketClientPool.Create;

finalization

DisposeObject(ClientPool);

end.
