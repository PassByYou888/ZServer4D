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
unit CommunicationFramework_Server_CrossSocket;

{$INCLUDE ..\zDefine.inc}

{
  CrossSocket Server(服务器端)的高性能模式，还在试验阶段
  服务器需要稳定，该选项在服务器端请小心使用
  在高性能模式下，多核会被充分调动，但是异步IO会发生的中断情况（通俗来说就是容易掉线），zs的暴力做法：发现状态不对，直接丢弃IO(服务器会正常关闭该IO，不做错误处理)
  关闭该选项后，服务器会极其稳定，IO调度会多核，但是数据吞吐全在主线来干
  该选项对多连接，并发数无任何影响，只影响cpu工作能力
}
{$UNDEF CrossSocketServer_HighPerformance}

interface

uses SysUtils, Classes,
  NET.CrossSocket, NET.SocketAPI, NET.CrossSocket.Base, NET.CrossServer,
  PascalStrings, DoStatusIO,
  CommunicationFramework, CoreClasses, UnicodeMixedLib, MemoryStream64,
  DataFrameEngine;

type
  TPeerIOWithCrossSocketServer = class(TPeerIO)
  public
    LastPeerIP: SystemString;
    Sending: Boolean;
    SendBuffQueue: TCoreClassListForObj;
    CurrentBuff: TMemoryStream64;
    LastSendingBuff: TMemoryStream64;

    procedure CreateAfter; override;
    destructor Destroy; override;
    function Context: TCrossConnection;
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

  TDriverEngine = TCrossSocket;

  TCommunicationFramework_Server_CrossSocket = class(TCommunicationFrameworkServer)
  private
    FDriver: TDriverEngine;
    FStartedService: Boolean;
    FBindHost: SystemString;
    FBindPort: Word;
  protected
    procedure DoConnected(Sender: TObject; AConnection: ICrossConnection);
    procedure DoDisconnect(Sender: TObject; AConnection: ICrossConnection);
    procedure DoReceived(Sender: TObject; AConnection: ICrossConnection; aBuf: Pointer; ALen: Integer);
    procedure DoSent(Sender: TObject; AConnection: ICrossConnection; aBuf: Pointer; ALen: Integer);
  public
    constructor Create; override;
    constructor CreateTh(maxThPool: Word);
    destructor Destroy; override;

    function StartService(Host: SystemString; Port: Word): Boolean; override;
    procedure StopService; override;

    procedure TriggerQueueData(v: PQueueData); override;
    procedure Progress; override;

    function WaitSendConsoleCmd(Client: TPeerIO; const Cmd, ConsoleData: SystemString; Timeout: TTimeTickValue): SystemString; override;
    procedure WaitSendStreamCmd(Client: TPeerIO; const Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; Timeout: TTimeTickValue); override;

    property StartedService: Boolean read FStartedService;
    property driver: TDriverEngine read FDriver;
    property BindPort: Word read FBindPort;
    property BindHost: SystemString read FBindHost;
  end;

implementation

procedure TPeerIOWithCrossSocketServer.CreateAfter;
begin
  inherited CreateAfter;
  LastPeerIP := '';
  Sending := False;
  SendBuffQueue := TCoreClassListForObj.Create;
  CurrentBuff := TMemoryStream64.Create;
  LastSendingBuff := nil;
end;

destructor TPeerIOWithCrossSocketServer.Destroy;
var
  i: Integer;
begin
  for i := 0 to SendBuffQueue.Count - 1 do
      DisposeObject(SendBuffQueue[i]);

  if LastSendingBuff <> nil then
    begin
      DisposeObject(LastSendingBuff);
      LastSendingBuff := nil;
    end;

  DisposeObject(CurrentBuff);
  DisposeObject(SendBuffQueue);

  inherited Destroy;
end;

function TPeerIOWithCrossSocketServer.Context: TCrossConnection;
begin
  Result := ClientIntf as TCrossConnection;
end;

function TPeerIOWithCrossSocketServer.Connected: Boolean;
begin
  Result := (ClientIntf <> nil) and
    (Context.ConnectStatus = TConnectStatus.csConnected);
end;

procedure TPeerIOWithCrossSocketServer.Disconnect;
begin
  if ClientIntf <> nil then
    begin
      try
          Context.Disconnect;
      except
      end;
    end;
end;

procedure TPeerIOWithCrossSocketServer.SendBuffResult(AConnection: ICrossConnection; ASuccess: Boolean);
var
  isConn: Boolean;
begin
  if (AConnection = nil) or (not ASuccess) then
    begin
      PrintError('send failed!');
      Sending := False;
      DelayFree(0);
      exit;
    end;

  isConn := AConnection.ConnectStatus = TConnectStatus.csConnected;

  TCoreClassThread.Synchronize(TCoreClassThread.Current, procedure
    var
      i: Integer;
    begin
      DisposeObject(LastSendingBuff);
      LastSendingBuff := nil;

      if isConn then
        begin
          try
            if (ASuccess and isConn) then
              begin
                if SendBuffQueue.Count > 0 then
                  begin
                    // 将发送队列拾取出来
                    LastSendingBuff := TMemoryStream64(SendBuffQueue[0]);
                    // 删除队列，下次回调时后置式释放
                    SendBuffQueue.Delete(0);

                    if Context <> nil then
                        Context.SendBuf(LastSendingBuff.Memory, LastSendingBuff.Size, SendBuffResult)
                    else
                        SendBuffResult(nil, False);
                  end
                else
                  begin
                    Sending := False;
                  end;
              end;
          except
          end;
        end
      else
        begin
          Sending := False;
        end;
    end);
end;

procedure TPeerIOWithCrossSocketServer.SendByteBuffer(const buff: PByte; const Size: NativeInt);
begin
  // 避免大量零碎数据消耗流量资源，碎片收集
  // 在flush中实现精确异步发送和校验
  if Size > 0 then
      CurrentBuff.write(Pointer(buff)^, Size);
end;

procedure TPeerIOWithCrossSocketServer.WriteBufferOpen;
begin
end;

procedure TPeerIOWithCrossSocketServer.WriteBufferFlush;
begin
  if Sending then
    begin
      if CurrentBuff.Size = 0 then
          exit;

      SendBuffQueue.Add(CurrentBuff);
      CurrentBuff := TMemoryStream64.Create;
    end
  else
    begin
      Sending := True;
      LastSendingBuff := CurrentBuff;
      Context.SendBuf(LastSendingBuff.Memory, LastSendingBuff.Size, SendBuffResult);
      CurrentBuff := TMemoryStream64.Create;
    end;
end;

procedure TPeerIOWithCrossSocketServer.WriteBufferClose;
begin
  WriteBufferFlush;
end;

function TPeerIOWithCrossSocketServer.GetPeerIP: SystemString;
begin
  if Connected then
    begin
      Result := Context.PeerAddr;
      LastPeerIP := Result;
    end
  else
      Result := LastPeerIP;
end;

function TPeerIOWithCrossSocketServer.WriteBufferEmpty: Boolean;
begin
  Result := not Sending;
end;

procedure TPeerIOWithCrossSocketServer.Progress;
begin
  inherited Progress;
  ProcessAllSendCmd(nil, False, False);
end;

procedure TCommunicationFramework_Server_CrossSocket.DoConnected(Sender: TObject; AConnection: ICrossConnection);
begin
  TCoreClassThread.Synchronize(TCoreClassThread.CurrentThread, procedure
    var
      cli: TPeerIOWithCrossSocketServer;
    begin
      cli := TPeerIOWithCrossSocketServer.Create(self, AConnection.ConnectionIntf);
      AConnection.UserObject := cli;
    end);
end;

procedure TCommunicationFramework_Server_CrossSocket.DoDisconnect(Sender: TObject; AConnection: ICrossConnection);
begin
  if AConnection.UserObject is TPeerIOWithCrossSocketServer then
    begin
      TCoreClassThread.Synchronize(TCoreClassThread.CurrentThread, procedure
        var
          cli: TPeerIOWithCrossSocketServer;
        begin
          cli := TPeerIOWithCrossSocketServer(AConnection.UserObject);
          cli.ClientIntf := nil;
          AConnection.UserObject := nil;

          cli.DelayFree;
        end);
    end;
end;

procedure TCommunicationFramework_Server_CrossSocket.DoReceived(Sender: TObject; AConnection: ICrossConnection; aBuf: Pointer; ALen: Integer);
var
  cli: TPeerIOWithCrossSocketServer;
begin
  if ALen <= 0 then
      exit;

  if AConnection.UserObject = nil then
      exit;

  if FEnabledAtomicLockAndMultiThread then
    begin
      try
        cli := AConnection.UserObject as TPeerIOWithCrossSocketServer;
        if cli.ClientIntf = nil then
            exit;

        cli.SaveReceiveBuffer(aBuf, ALen);
        cli.FillRecvBuffer(TCoreClassThread.CurrentThread, True, True);
      except
      end;
    end
  else
    begin
      TCoreClassThread.Synchronize(TCoreClassThread.CurrentThread, procedure
        begin
          try
            cli := AConnection.UserObject as TPeerIOWithCrossSocketServer;
            if cli.ClientIntf = nil then
                exit;

            cli.SaveReceiveBuffer(aBuf, ALen);
            cli.FillRecvBuffer(nil, False, False);
          except
          end;
        end);
    end;
end;

procedure TCommunicationFramework_Server_CrossSocket.DoSent(Sender: TObject; AConnection: ICrossConnection; aBuf: Pointer; ALen: Integer);
var
  cli: TPeerIOWithCrossSocketServer;
begin
  if AConnection.UserObject = nil then
      exit;

  cli := AConnection.UserObject as TPeerIOWithCrossSocketServer;
  if cli.ClientIntf = nil then
      exit;
end;

constructor TCommunicationFramework_Server_CrossSocket.Create;
begin
  CreateTh({$IFDEF CrossSocketServer_HighPerformance}CPUCount{$ELSE}2{$ENDIF});
end;

constructor TCommunicationFramework_Server_CrossSocket.CreateTh(maxThPool: Word);
begin
  inherited Create;
  FEnabledAtomicLockAndMultiThread := {$IFDEF CrossSocketServer_HighPerformance}True; {$ELSE}False; {$ENDIF}
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
      DisposeObject(FDriver);
  except
  end;
  inherited Destroy;
end;

function TCommunicationFramework_Server_CrossSocket.StartService(Host: SystemString; Port: Word): Boolean;
var
  Completed, Successed: Boolean;
begin
  StopService;

  Completed := False;
  Successed := False;
  try
    ICrossSocket(FDriver).Listen(Host, Port,
      procedure(Listen: ICrossListen; ASuccess: Boolean)
      begin
        Completed := True;
        Successed := ASuccess;
      end);

    while not Completed do
        CheckThreadSynchronize(5);

    FBindPort := Port;
    FBindHost := Host;
    Result := Successed;
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

procedure TCommunicationFramework_Server_CrossSocket.Progress;
begin
  inherited Progress;
end;

function TCommunicationFramework_Server_CrossSocket.WaitSendConsoleCmd(Client: TPeerIO; const Cmd, ConsoleData: SystemString; Timeout: TTimeTickValue): SystemString;
begin
  Result := '';
  RaiseInfo('WaitSend no Suppport CrossSocket');
end;

procedure TCommunicationFramework_Server_CrossSocket.WaitSendStreamCmd(Client: TPeerIO; const Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; Timeout: TTimeTickValue);
begin
  RaiseInfo('WaitSend no Suppport CrossSocket');
end;

initialization

finalization

end.
