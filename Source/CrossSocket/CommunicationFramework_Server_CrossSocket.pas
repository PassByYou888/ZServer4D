{ ****************************************************************************** }
{ * CrossSocket support                                                        * }
{ * written by QQ 600585@qq.com                                                * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ ****************************************************************************** }
(*
  update history
*)
unit CommunicationFramework_Server_CrossSocket;

{$I ..\zDefine.inc}

interface

uses SysUtils, Classes,
  Net.CrossSocket, Net.SocketAPI, Net.CrossSocket.Base, Net.CrossServer,
  PascalStrings, DoStatusIO,
  CommunicationFramework, CoreClasses, UnicodeMixedLib, MemoryStream64,
  DataFrameEngine;

type
  TContextIntfForServer = class(TPeerIO)
  public
    LastActiveTime: TTimeTickValue;
    Sending       : Boolean;
    SendBuffQueue : TCoreClassListForObj;
    CurrentBuff   : TMemoryStream64;
    DelayBuffPool : TCoreClassListForObj;

    procedure CreateAfter; override;
    destructor Destroy; override;

    procedure FreeDelayBuffPool; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Context: TCrossConnection; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    //
    function Connected: Boolean; override;
    procedure Disconnect; override;
    procedure SendBuffResult(AConnection: ICrossConnection; ASuccess: Boolean);
    procedure SendByteBuffer(const buff: PByte; const Size: NativeInt); override;
    procedure WriteBufferOpen; override;
    procedure WriteBufferFlush; override;
    procedure WriteBufferClose; override;
    function GetPeerIP: SystemString; override;
    function WriteBufferEmpty: Boolean; override;
    procedure Progress; override;
  end;

  TDriverEngine = class(TCrossSocket)
  protected type
    { Because crossSocket at the bottom of the external support of the interface is a little less, to change the interface is more troublesome, temporarily put aside, after a period of time to do again to do it. }
    PCrossSocketIntfStruct = ^TCrossSocketIntfStruct;

    TCrossSocketIntfStruct = record
      IO: TContextIntfForServer;
    end;
  protected
    function CreateListen(AOwner: ICrossSocket; AListenSocket: THandle;
      AFamily, ASockType, AProtocol: Integer): ICrossListen; override;
  end;

  TCommunicationFramework_Server_CrossSocket = class(TCommunicationFrameworkServer)
  private
    FDriver        : TDriverEngine;
    FStartedService: Boolean;
    FBindHost      : SystemString;
    FBindPort      : Word;
  protected
    procedure DoConnected(Sender: TObject; AConnection: ICrossConnection);
    procedure DoDisconnect(Sender: TObject; AConnection: ICrossConnection);
    procedure DoReceived(Sender: TObject; AConnection: ICrossConnection; ABuf: Pointer; ALen: Integer);
    procedure DoSent(Sender: TObject; AConnection: ICrossConnection; ABuf: Pointer; ALen: Integer);
  public
    constructor Create; overload; override;
    constructor Create(maxThPool: Word); overload;
    destructor Destroy; override;

    function StartService(Host: SystemString; Port: Word): Boolean; override;
    procedure StopService; override;

    procedure TriggerQueueData(v: PQueueData); override;
    procedure ProgressBackground; override;

    function WaitSendConsoleCmd(Client: TPeerIO; const Cmd, ConsoleData: SystemString; TimeOut: TTimeTickValue): SystemString; override;
    procedure WaitSendStreamCmd(Client: TPeerIO; const Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; TimeOut: TTimeTickValue); override;

    property StartedService: Boolean read FStartedService;
    property Driver: TDriverEngine read FDriver;
    property BindPort: Word read FBindPort;
    property BindHost: SystemString read FBindHost;
  end;

implementation

procedure TContextIntfForServer.CreateAfter;
begin
  inherited CreateAfter;
  LastActiveTime := GetTimeTickCount;
  Sending := False;
  SendBuffQueue := TCoreClassListForObj.Create;
  CurrentBuff := TMemoryStream64.Create;
  DelayBuffPool := TCoreClassListForObj.Create;
end;

destructor TContextIntfForServer.Destroy;
var
  i: Integer;
begin
  FreeDelayBuffPool;

  for i := 0 to SendBuffQueue.Count - 1 do
      disposeObject(SendBuffQueue[i]);

  disposeObject(SendBuffQueue);

  disposeObject(CurrentBuff);
  disposeObject(DelayBuffPool);
  inherited Destroy;
end;

procedure TContextIntfForServer.FreeDelayBuffPool;
var
  i: Integer;
begin
  for i := 0 to DelayBuffPool.Count - 1 do
      disposeObject(DelayBuffPool[i]);
  DelayBuffPool.Clear;
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
  if ClientIntf <> nil then
    begin
      try
          Context.Disconnect;
      except
      end;
    end;
end;

procedure TContextIntfForServer.SendBuffResult(AConnection: ICrossConnection; ASuccess: Boolean);
begin
  LastActiveTime := GetTimeTickCount;

  { In order to avoid using non - Page exchange memory, the IO kernel is sent to the main thread to send the progress }
  TThread.Synchronize(nil,
    procedure
    var
      i: Integer;
      m: TMemoryStream64;
      isConn: Boolean;
    begin
      { Since Send under Linux does not have a copy of the copy buffer, we need to delay the release of our own buffer }
      { When the send completion state returns, we release all temporary buffers }
      FreeDelayBuffPool;

      isConn := False;
      try
        isConn := Connected;
        if (ASuccess and isConn) then
          begin
            if SendBuffQueue.Count > 0 then
              begin
                m := TMemoryStream64(SendBuffQueue[0]);

                { When WSASend huff and puff is sent, a copy is copied. There is a copy of the memory, the copy is limited to 32K, and the shards are cut in the bottom frame. }
                { Note: the total amount of buff sent by the event callback will finally be determined according to the stack size }
                { Thank you for the test report of AK47 qq512757165 }
                Context.SendBuf(m.Memory, m.Size, SendBuffResult);

                { Since Send under Linux does not have a copy of the copy buffer, we need to delay the release of our own buffer }
                DelayBuffPool.Add(m);

                { Release queue }
                SendBuffQueue.Delete(0);
              end
            else
              begin
                Sending := False;
              end;
          end
        else
          begin
            { Release queue space }
            for i := 0 to SendBuffQueue.Count - 1 do
                disposeObject(SendBuffQueue[i]);
            SendBuffQueue.Clear;

            Sending := False;

            if isConn then
              begin
                Print('send failed!');
                Disconnect;
              end;
          end;
      except
        Print('send failed!');
        Disconnect;
      end;
    end);
end;

procedure TContextIntfForServer.SendByteBuffer(const buff: PByte; const Size: NativeInt);
begin
  if not Connected then
      exit;

  LastActiveTime := GetTimeTickCount;

  { Avoid a large amount of fragmented data to consume system resources, and there is a need to do a collection of fragments }
  { Accurate asynchronous transmission and verification in flush }
  if Size > 0 then
      CurrentBuff.Write(Pointer(buff)^, Size);
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

  { At Ubuntu 16.04 TLS, when the receiving thread is running in the program, we call to send API, when the data out is wrong }
  { We change the mechanism to check to send, check whether the thread is being received: avoid duplex work }
  if Sending or ReceiveProcessing then
    begin
      if CurrentBuff.Size > 0 then
        begin
          { Completion of optimization }
          ms := CurrentBuff;
          ms.Position := 0;
          SendBuffQueue.Add(ms);
          CurrentBuff := TMemoryStream64.Create;
        end;
    end
  else
    begin
      { When WSASend huff and puff is sent, a copy is copied. There is a copy of the memory, the copy is limited to 32K, and the shards are cut in the bottom frame. }
      { Note: the total amount of buff sent by the event callback will finally be determined according to the stack size }
      { Thank you for the test report of AK47 qq512757165 }
      Sending := True;
      Context.SendBuf(CurrentBuff.Memory, CurrentBuff.Size, SendBuffResult);

      { Since Send under Linux does not have a copy of the copy buffer, we need to delay the release of our own buffer }
      DelayBuffPool.Add(CurrentBuff);
      CurrentBuff := TMemoryStream64.Create;
    end;
end;

procedure TContextIntfForServer.WriteBufferClose;
begin
  if not Connected then
      exit;
  CurrentBuff.Clear;
end;

function TContextIntfForServer.GetPeerIP: SystemString;
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

procedure TContextIntfForServer.Progress;
var
  m: TMemoryStream64;
begin
  { Check free }
  if (OwnerFramework.IdleTimeout > 0) and (GetTimeTickCount - LastActiveTime > OwnerFramework.IdleTimeout) then
    begin
      Disconnect;
      exit;
    end;

  inherited Progress;

  ProcessAllSendCmd(nil, False, False);

  { At Ubuntu 16.04 TLS, when the receiving thread is running in the program, we call to send API, when the data out is wrong }
  { We change the mechanism to check to send, check whether the thread is being received: avoid duplex work }
  if (not Sending) and (SendBuffQueue.Count > 0) then
    begin
      Sending := True;
      LastActiveTime := GetTimeTickCount;
      m := TMemoryStream64(SendBuffQueue[0]);
      { Release queue }
      SendBuffQueue.Delete(0);

      { When WSASend huff and puff is sent, a copy is copied. There is a copy of the memory, the copy is limited to 32K, and the shards are cut in the bottom frame. }
      { Note: the total amount of buff sent by the event callback will finally be determined according to the stack size }
      { Thank you for the test report of AK47 qq512757165 }
      Context.SendBuf(m.Memory, m.Size, SendBuffResult);

      { Since Send under Linux does not have a copy of the copy buffer, we need to delay the release of our own buffer }
      DelayBuffPool.Add(m);
    end;
end;

function TDriverEngine.CreateListen(AOwner: ICrossSocket; AListenSocket: THandle; AFamily, ASockType, AProtocol: Integer): ICrossListen;
begin
  Result := inherited CreateListen(AOwner, AListenSocket, AFamily, ASockType, AProtocol);
end;

procedure TCommunicationFramework_Server_CrossSocket.DoConnected(Sender: TObject; AConnection: ICrossConnection);
var
  cli: TContextIntfForServer;
begin
  cli := TContextIntfForServer.Create(Self, AConnection.ConnectionIntf);
  cli.LastActiveTime := GetTimeTickCount;
  AConnection.UserObject := cli;
end;

procedure TCommunicationFramework_Server_CrossSocket.DoDisconnect(Sender: TObject; AConnection: ICrossConnection);
begin
  TThread.Synchronize(TThread.CurrentThread,
    procedure
    var
      cli: TContextIntfForServer;
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
  if ALen <= 0 then
      exit;

  if AConnection.UserObject = nil then
      exit;

  try
    cli := AConnection.UserObject as TContextIntfForServer;
    if cli.ClientIntf = nil then
        exit;

    cli.LastActiveTime := GetTimeTickCount;

    { The ZS kernel has fully supported 100% asynchronous parsing data in the new version }
    { In the new version of the ZS kernel, CrossSocket is an asynchronous framework of 100%, no longer semi asynchronous processing of data streams }
    // by 2018-1-29
    cli.SaveReceiveBuffer(ABuf, ALen);
    cli.FillRecvBuffer(TThread.CurrentThread, True, True);
  except
  end;
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

constructor TCommunicationFramework_Server_CrossSocket.Create(maxThPool: Word);
begin
  inherited Create;
  FDriver := TDriverEngine.Create(maxThPool);
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

function TCommunicationFramework_Server_CrossSocket.StartService(Host: SystemString; Port: Word): Boolean;
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

procedure TCommunicationFramework_Server_CrossSocket.StopService;
begin
  try
    try
        ICrossSocket(FDriver).CloseAll;
    except
    end;
    FStartedService := False;
  except
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
begin
  inherited ProgressBackground;

  CheckSynchronize;
end;

function TCommunicationFramework_Server_CrossSocket.WaitSendConsoleCmd(Client: TPeerIO; const Cmd, ConsoleData: SystemString; TimeOut: TTimeTickValue): SystemString;
begin
  Result := '';
  RaiseInfo('WaitSend no Suppport CrossSocket');
end;

procedure TCommunicationFramework_Server_CrossSocket.WaitSendStreamCmd(Client: TPeerIO; const Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; TimeOut: TTimeTickValue);
begin
  RaiseInfo('WaitSend no Suppport CrossSocket');
end;

initialization

finalization

end.
