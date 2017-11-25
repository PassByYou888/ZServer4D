unit CommunicationFramework_Server_CrossSocket;

interface

uses SysUtils, Classes,
  Net.CrossSocket, Net.SocketAPI, Net.CrossSocket.Base, Net.CrossServer,
  CommunicationFramework, CoreClasses, UnicodeMixedLib, MemoryStream64;

type
  TContextIntfForServer = class(TPeerClient)
  public
    LastActiveTime: TTimeTickValue;
    Sending       : Boolean;
    SendBuffQueue : TCoreClassListForObj;
    CurrentBuff   : TMemoryStream64;

    constructor Create(AOwnerFramework: TCommunicationFramework; AClientIntf: TCoreClassObject); override;
    destructor Destroy; override;

    function Context: TCrossConnection;
    function Connected: Boolean; override;
    procedure Disconnect; override;
    procedure SendBuffResult(AConnection: ICrossConnection; ASuccess: Boolean);
    procedure SendByteBuffer(buff: PByte; size: Integer); override;
    procedure WriteBufferOpen; override;
    procedure WriteBufferFlush; override;
    procedure WriteBufferClose; override;
    function GetPeerIP: string; override;
    function WriteBufferEmpty: Boolean; override;
  end;

  TDriverEngine = TCrossSocket;

  TCommunicationFramework_Server_CrossSocket = class(TCommunicationFrameworkServer)
  private
    FDriver        : TDriverEngine;
    FStartedService: Boolean;
    FBindHost      : string;
    FBindPort      : Word;

    procedure DoConnected(Sender: TObject; AConnection: ICrossConnection); virtual;
    procedure DoDisconnect(Sender: TObject; AConnection: ICrossConnection); virtual;
    procedure DoReceived(Sender: TObject; AConnection: ICrossConnection; ABuf: Pointer; ALen: Integer); virtual;
    procedure DoSent(Sender: TObject; AConnection: ICrossConnection; ABuf: Pointer; ALen: Integer); virtual;
  public
    constructor Create;
    destructor Destroy; override;

    function StartService(Host: string; Port: Word): Boolean;
    function StopService: Boolean;

    procedure TriggerQueueData(v: PQueueData); override;
    procedure ProgressBackground; override;

    property StartedService: Boolean read FStartedService;
    property Driver: TDriverEngine read FDriver;
    property BindPort: Word read FBindPort;
    property BindHost: string read FBindHost;
  end;

implementation

constructor TContextIntfForServer.Create(AOwnerFramework: TCommunicationFramework; AClientIntf: TCoreClassObject);
begin
  inherited Create(AOwnerFramework, AClientIntf);
  LastActiveTime := GetTimeTickCount;
  Sending := False;
  SendBuffQueue := TCoreClassListForObj.Create;
  CurrentBuff := TMemoryStream64.Create;
end;

destructor TContextIntfForServer.Destroy;
var
  i: Integer;
begin
  for i := 0 to SendBuffQueue.Count - 1 do
      disposeObject(SendBuffQueue[i]);

  disposeObject(SendBuffQueue);

  disposeObject(CurrentBuff);
  inherited Destroy;
end;

function TContextIntfForServer.Context: TCrossConnection;
begin
  Result := ClientIntf as TCrossConnection;
end;

function TContextIntfForServer.Connected: Boolean;
begin
  Result := (ClientIntf <> nil) and
    (Context.ConnectStatus = TConnectStatus.csConnected);
end;

procedure TContextIntfForServer.Disconnect;
begin
  if not Connected then
      exit;
  Context.Disconnect;
end;

procedure TContextIntfForServer.SendBuffResult(AConnection: ICrossConnection; ASuccess: Boolean);
begin
  LastActiveTime := GetTimeTickCount;

  // 为避免使用非页面交换内存，将io内核发送进度同步到主线程来发
  TThread.Synchronize(nil,
    procedure
    var
      i: Integer;
      m: TMemoryStream64;
    begin
      try
        if (ASuccess and Connected) then
          begin
            if SendBuffQueue.Count > 0 then
              begin
                m := TMemoryStream64(SendBuffQueue[0]);

                // WSASend吞吐发送时，会复制一份副本，这里有内存拷贝，拷贝限制为32k，已在底层框架做了碎片预裁剪
                // 注意：事件式回调发送的buff总量最后会根据堆栈大小决定
                // 感谢ak47 qq512757165 的测试报告
                Context.SendBuf(m.Memory, m.size, SendBuffResult);

                // 释放内存
                disposeObject(m);
                // 释放队列
                SendBuffQueue.Delete(0);
              end
            else
              begin
                Sending := False;
              end;
          end
        else
          begin
            // 释放队列空间
            for i := 0 to SendBuffQueue.Count - 1 do
                disposeObject(SendBuffQueue[i]);
            SendBuffQueue.Clear;

            Sending := False;

            Print('send failed!');
            Disconnect;
          end;
      except
        Print('send failed!');
        Disconnect;
      end;
    end);
end;

procedure TContextIntfForServer.SendByteBuffer(buff: PByte; size: Integer);
begin
  if not Connected then
      exit;

  LastActiveTime := GetTimeTickCount;

  // 避免大量零碎数据消耗系统资源，这里需要做个碎片收集
  // 在flush中实现精确异步发送和校验
  if size > 0 then
      CurrentBuff.Write(Pointer(buff)^, size);
end;

procedure TContextIntfForServer.WriteBufferOpen;
begin
  if not Connected then
      exit;
  LastActiveTime := GetTimeTickCount;
  CurrentBuff.Clear;
end;

procedure TContextIntfForServer.WriteBufferFlush;
var
  ms: TMemoryStream64;
begin
  if not Connected then
      exit;
  LastActiveTime := GetTimeTickCount;

  if Sending then
    begin
      if CurrentBuff.size > 0 then
        begin
          ms := TMemoryStream64.Create;
          CurrentBuff.Position := 0;
          ms.CopyFrom(CurrentBuff, CurrentBuff.size);
          ms.Position := 0;
          SendBuffQueue.Add(ms);
        end;
    end
  else
    begin
      // WSASend吞吐发送时，会复制一份副本，这里有内存拷贝，拷贝限制为32k，已在底层框架做了碎片预裁剪
      // 注意：事件式回调发送的buff总量最后会根据堆栈大小决定
      // 感谢ak47 qq512757165 的测试报告
      Sending := True;
      Context.SendBuf(CurrentBuff.Memory, CurrentBuff.size, SendBuffResult);
    end;
  CurrentBuff.Clear;
end;

procedure TContextIntfForServer.WriteBufferClose;
begin
  if not Connected then
      exit;
  CurrentBuff.Clear;
end;

function TContextIntfForServer.GetPeerIP: string;
begin
  if Connected then
      Result := Context.PeerAddr
  else
      Result := '';
end;

function TContextIntfForServer.WriteBufferEmpty: Boolean;
begin
  Result := not Sending;
end;

procedure TCommunicationFramework_Server_CrossSocket.DoConnected(Sender: TObject; AConnection: ICrossConnection);
var
  cli: TContextIntfForServer;
begin
  TThread.Synchronize(nil,
    procedure
    begin
      try
        cli := TContextIntfForServer.Create(Self, AConnection.ConnectionIntf);
        cli.LastActiveTime := GetTimeTickCount;
        AConnection.UserObject := cli;
      except
      end;
    end);
end;

procedure TCommunicationFramework_Server_CrossSocket.DoDisconnect(Sender: TObject; AConnection: ICrossConnection);
var
  cli: TContextIntfForServer;
begin
  TThread.Synchronize(nil,
    procedure
    begin
      cli := AConnection.UserObject as TContextIntfForServer;
      if cli <> nil then
        begin
          try
            cli.ClientIntf := nil;
            AConnection.UserObject := nil;
            disposeObject(cli);
          except
          end;
        end;
    end);
end;

procedure TCommunicationFramework_Server_CrossSocket.DoReceived(Sender: TObject; AConnection: ICrossConnection; ABuf: Pointer; ALen: Integer);
var
  cli: TContextIntfForServer;
begin
  if AConnection.UserObject = nil then
      exit;

  cli := AConnection.UserObject as TContextIntfForServer;
  if cli.ClientIntf = nil then
      exit;

  cli.LastActiveTime := GetTimeTickCount;
  TThread.Synchronize(nil,
    procedure
    begin
      try
        cli.ReceivedBuffer.Position := cli.ReceivedBuffer.size;
        cli.ReceivedBuffer.Write(ABuf^, ALen);
        cli.FillRecvBuffer(nil, False, False);
      except
      end;
    end);
end;

procedure TCommunicationFramework_Server_CrossSocket.DoSent(Sender: TObject; AConnection: ICrossConnection; ABuf: Pointer; ALen: Integer);
var
  cli: TContextIntfForServer;
begin
  if AConnection.UserObject = nil then
      exit;

  cli := AConnection.UserObject as TContextIntfForServer;
  if cli.ClientIntf = nil then
      exit;
  cli.LastActiveTime := GetTimeTickCount;
end;

constructor TCommunicationFramework_Server_CrossSocket.Create;
var
  r: TCommandStreamMode;
begin
  inherited Create;
  FDriver := TDriverEngine.Create(4);
  FDriver.OnConnected := DoConnected;
  FDriver.OnDisconnected := DoDisconnect;
  FDriver.OnReceived := DoReceived;
  FDriver.OnSent := DoSent;
  FStartedService := False;
  FBindPort := 0;
  FBindHost := '';
end;

destructor TCommunicationFramework_Server_CrossSocket.Destroy;
begin
  StopService;
  try
      disposeObject(FDriver);
  except
  end;
  inherited Destroy;
end;

function TCommunicationFramework_Server_CrossSocket.StartService(Host: string; Port: Word): Boolean;
var
  completed, successed: Boolean;
begin
  StopService;

  completed := False;
  successed := False;
  try
    ICrossSocket(FDriver).Listen(Host, Port,
      procedure(Listen: ICrossListen; ASuccess: Boolean)
      begin
        completed := True;
        successed := ASuccess;
      end);

    while not completed do
        CheckSynchronize(5);

    FBindPort := Port;
    FBindHost := Host;
    Result := successed;
    FStartedService := Result;
  except
      Result := False;
  end;
end;

function TCommunicationFramework_Server_CrossSocket.StopService: Boolean;
begin
  try
    try
        ICrossSocket(FDriver).CloseAll;
    except
    end;
    Result := True;
    FStartedService := False;
  except
      Result := False;
  end;
end;

procedure TCommunicationFramework_Server_CrossSocket.TriggerQueueData(v: PQueueData);
begin
  (*
    TThread.Synchronize(nil,
    procedure
    begin
    end);
  *)
  if not Exists(v^.Client) then
    begin
      DisposeQueueData(v);
      exit;
    end;

  v^.Client.PostQueueData(v);
  v^.Client.ProcessAllSendCmd(nil, False, False);
end;

procedure TCommunicationFramework_Server_CrossSocket.ProgressBackground;
var
  i: Integer;
  c: TContextIntfForServer;
begin
  try
    try
      for i := 0 to Count - 1 do
        begin
          c := TContextIntfForServer(Items[i]);
          if c <> nil then
            begin
              if (IdleTimeout > 0) and (GetTimeTickCount - c.LastActiveTime > IdleTimeout) then
                  c.Disconnect
              else
                begin
                  if c.Connected then
                      c.ProcessAllSendCmd(nil, False, False);
                end;
            end;
        end;
    except
    end;
  finally
  end;

  inherited ProgressBackground;

  CheckSynchronize;
end;

initialization

finalization

end.
