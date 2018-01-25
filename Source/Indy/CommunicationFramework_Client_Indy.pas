{ ****************************************************************************** }
{ * IndyInterface                                                              * }
{ * written by QQ 600585@qq.com                                                * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
(* https://github.com/PassByYou888/ZServer4D *)
{ ****************************************************************************** }
(*
  update history
*)
unit CommunicationFramework_Client_Indy;

{$I ..\zDefine.inc}

interface

uses CommunicationFramework, CoreClasses,
  DataFrameEngine, ListEngine, MemoryStream64,

  Classes, SysUtils,
  IdTCPClient, IdYarn, IdStack,
  IDGlobal, IdBaseComponent, IdComponent, IdTCPConnection, IdContext,
  DoStatusIO, UnicodeMixedLib, PascalStrings;

type
  TCommunicationFramework_Client_Indy = class;

  TClientIntf = class(TPeerClient)
  public
    function Context: TIdTCPClient;

    function Connected: Boolean; override;
    procedure Disconnect; override;
    procedure SendByteBuffer(const buff: PByte; const Size: NativeInt); override;
    procedure WriteBufferOpen; override;
    procedure WriteBufferFlush; override;
    procedure WriteBufferClose; override;
    function GetPeerIP: string; override;
  end;

  TCommunicationFramework_Client_Indy = class(TCommunicationFrameworkClient)
  protected
    FDriver     : TIdTCPClient;
    ClientIntf  : TClientIntf;
    FProgressing: Boolean;

    FOnAsyncConnectNotifyCall  : TStateCall;
    FOnAsyncConnectNotifyMethod: TStateMethod;
    FOnAsyncConnectNotifyProc  : TStateProc;

    procedure AsyncConnect(Addr: SystemString; Port: Word; OnResultCall: TStateCall; OnResultMethod: TStateMethod; OnResultProc: TStateProc); overload;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure TriggerDoConnectFailed; override;
    procedure TriggerDoConnectFinished; override;

    function Connected: Boolean; override;
    function ClientIO: TPeerClient; override;
    procedure ProgressBackground; override;
    procedure TriggerQueueData(v: PQueueData); override;

    procedure AsyncConnect(Addr: SystemString; Port: Word; OnResult: TStateCall); overload; override;
    procedure AsyncConnect(Addr: SystemString; Port: Word; OnResult: TStateMethod); overload; override;
    procedure AsyncConnect(Addr: SystemString; Port: Word; OnResult: TStateProc); overload; override;
    function Connect(Addr: string; Port: Word): Boolean; override;
    procedure Disconnect; override;
  end;

procedure CheckIPV6(hostName: string; Port: Word);

var
  DefaultIPVersion: TIdIPVersion;

implementation

procedure CheckIPV6(hostName: string; Port: Word);
var
  cli: TIdTCPClient;
begin
  cli := TIdTCPClient.Create(nil);
  cli.host := hostName;
  cli.Port := Port;
  cli.BoundIP := '';
  cli.BoundPort := 0;
  cli.IPVersion := TIdIPVersion.Id_IPv6;
  cli.ReuseSocket := TIdReuseSocket.rsFalse;
  cli.UseNagle := False;

  try
    cli.Connect;
    if cli.Connected then
      begin
        DefaultIPVersion := TIdIPVersion.Id_IPv6;
        cli.Disconnect;
      end
    else
        DefaultIPVersion := TIdIPVersion.Id_IPv4;
  except
  end;

  disposeObject(cli);
end;

function ToIDBytes(p: PByte; Size: Integer): TIdBytes; inline;
var
  i: Integer;
begin
  SetLength(Result, Size);
  for i := 0 to Size - 1 do
    begin
      Result[i] := p^;
      inc(p);
    end;
end;

function TClientIntf.Context: TIdTCPClient;
begin
  Result := ClientIntf as TIdTCPClient;
end;

function TClientIntf.Connected: Boolean;
begin
  Result := Context.Connected;
end;

procedure TClientIntf.Disconnect;
begin
  Context.Disconnect;
end;

procedure TClientIntf.SendByteBuffer(const buff: PByte; const Size: NativeInt);
begin
  if Size > 0 then
      Context.IOHandler.Write(ToIDBytes(buff, Size));
end;

procedure TClientIntf.WriteBufferOpen;
begin
  Context.IOHandler.WriteBufferOpen;
end;

procedure TClientIntf.WriteBufferFlush;
begin
  Context.IOHandler.WriteBufferFlush;
end;

procedure TClientIntf.WriteBufferClose;
begin
  Context.IOHandler.WriteBufferClose;
end;

function TClientIntf.GetPeerIP: string;
begin
  Result := Context.host;
end;

procedure TCommunicationFramework_Client_Indy.AsyncConnect(Addr: SystemString; Port: Word; OnResultCall: TStateCall; OnResultMethod: TStateMethod; OnResultProc: TStateProc);
begin
  Disconnect;

  disposeObject(ClientIntf);

  FDriver := TIdTCPClient.Create(nil);
  ClientIntf := TClientIntf.Create(Self, FDriver);
  FProgressing := False;

  if IsIPV4(Addr) then
      FDriver.IPVersion := TIdIPVersion.Id_IPv4
  else if IsIPV6(Addr) then
      FDriver.IPVersion := TIdIPVersion.Id_IPv6
  else
      FDriver.IPVersion := DefaultIPVersion;

  FDriver.host := Addr;
  FDriver.Port := Port;
  FDriver.BoundIP := '';
  FDriver.BoundPort := 0;
  FDriver.ReuseSocket := TIdReuseSocket.rsFalse;
  FDriver.UseNagle := False;
  ProgressBackground;

  FOnAsyncConnectNotifyCall := OnResultCall;
  FOnAsyncConnectNotifyMethod := OnResultMethod;
  FOnAsyncConnectNotifyProc := OnResultProc;

  FDriver.ConnectTimeout := 500;
  try
    FDriver.Connect;
    ProgressBackground;
  except
    if (IsIPV4(Addr)) or (IsIPV6(Addr)) then
      begin
        FDriver := TIdTCPClient.Create(nil);
        FDriver.IPVersion := DefaultIPVersion;
        FDriver.ConnectTimeout := 0;
        FDriver.ReadTimeout := -1;
        FDriver.UseNagle := False;

        disposeObject(ClientIntf);
        ClientIntf := TClientIntf.Create(Self, FDriver);

        TriggerDoConnectFailed;

        exit;
      end
    else
      begin
        if DefaultIPVersion = TIdIPVersion.Id_IPv4 then
            DefaultIPVersion := TIdIPVersion.Id_IPv6
        else
            DefaultIPVersion := TIdIPVersion.Id_IPv4;

        try
          FDriver.Connect;
          ProgressBackground;
        except
          if (not IsIPV4(Addr)) and (not IsIPV6(Addr)) then
            begin
              if DefaultIPVersion = TIdIPVersion.Id_IPv4 then
                  DefaultIPVersion := TIdIPVersion.Id_IPv6
              else
                  DefaultIPVersion := TIdIPVersion.Id_IPv4;
            end;

          FDriver := TIdTCPClient.Create(nil);
          FDriver.IPVersion := DefaultIPVersion;
          FDriver.ConnectTimeout := 0;
          FDriver.ReadTimeout := -1;
          FDriver.UseNagle := False;

          disposeObject(ClientIntf);
          ClientIntf := TClientIntf.Create(Self, FDriver);

          TriggerDoConnectFailed;

          exit;
        end;
      end;
  end;

  if not FDriver.Connected then
    begin
      TriggerDoConnectFailed;
      exit;
    end;

  DoConnected(ClientIntf);
end;

constructor TCommunicationFramework_Client_Indy.Create;
begin
  inherited Create;

  FDriver := TIdTCPClient.Create(nil);
  FDriver.IPVersion := DefaultIPVersion;
  FDriver.ConnectTimeout := 0;
  FDriver.ReadTimeout := -1;
  FDriver.UseNagle := False;

  ClientIntf := TClientIntf.Create(Self, FDriver);
  FProgressing := False;

  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := nil;
  FOnAsyncConnectNotifyProc := nil;
end;

destructor TCommunicationFramework_Client_Indy.Destroy;
begin
  try
      FDriver.Disconnect;
  except
  end;

  try
    // disposeObject(FDriver);
      FDriver := nil;
  except
  end;

  try
      disposeObject(ClientIntf);
  except
  end;

  inherited Destroy;
end;

procedure TCommunicationFramework_Client_Indy.TriggerDoConnectFailed;
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

procedure TCommunicationFramework_Client_Indy.TriggerDoConnectFinished;
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

function TCommunicationFramework_Client_Indy.Connected: Boolean;
begin
  try
      Result := FDriver.Connected;
  except
    FDriver := TIdTCPClient.Create(nil);
    FDriver.IPVersion := DefaultIPVersion;
    FDriver.ConnectTimeout := 0;
    FDriver.ReadTimeout := -1;
    FDriver.UseNagle := False;

    disposeObject(ClientIntf);

    ClientIntf := TClientIntf.Create(Self, FDriver);
    Result := False;
  end;
end;

function TCommunicationFramework_Client_Indy.ClientIO: TPeerClient;
begin
  Result := ClientIntf;
end;

procedure TCommunicationFramework_Client_Indy.ProgressBackground;
var
  t   : TTimeTickValue;
  iBuf: TIdBytes;
begin
  if FProgressing then
      exit;

  if Connected then
    begin
      FProgressing := True;

      FDriver.IOHandler.CheckForDataOnSource(2);
      try
        while FDriver.IOHandler.InputBuffer.Size > 0 do
          begin
            FDriver.IOHandler.InputBuffer.ExtractToBytes(iBuf);
            FDriver.IOHandler.InputBuffer.Clear;
            ClientIntf.SaveReceiveBuffer(@iBuf[0], Length(iBuf));
            SetLength(iBuf, 0);
            try
                ClientIntf.FillRecvBuffer(nil, False, False);
            except
              ClientIntf.Disconnect;
              FProgressing := False;
              exit;
            end;
            inherited ProgressBackground;
            FDriver.IOHandler.CheckForDataOnSource(5);
          end;

        try
          if Connected then
            begin
              ClientIntf.ProcessAllSendCmd(nil, False, False);
              t := GetTimeTickCount + 15000;
              while (ClientIntf <> nil) and (Connected) and (ClientIntf.WaitOnResult) do
                begin
                  FDriver.IOHandler.CheckForDataOnSource(5);
                  if FDriver.IOHandler.InputBuffer.Size > 0 then
                    begin
                      t := GetTimeTickCount + 15000;

                      FDriver.IOHandler.InputBuffer.ExtractToBytes(iBuf);
                      FDriver.IOHandler.InputBuffer.Clear;
                      ClientIntf.SaveReceiveBuffer(@iBuf[0], Length(iBuf));
                      SetLength(iBuf, 0);
                      try
                          ClientIntf.FillRecvBuffer(nil, False, False);
                      except
                        ClientIntf.Disconnect;
                        FProgressing := False;
                        exit;
                      end;
                    end
                  else if GetTimeTickCount > t then
                    begin
                      ClientIntf.Disconnect;
                      break;
                    end;
                  inherited ProgressBackground;
                end;

            end;
        except
          ClientIntf.Disconnect;
          FProgressing := False;
          exit;
        end;

      finally
          FProgressing := False;
      end;

      try
          inherited ProgressBackground;
      except
        ClientIntf.Disconnect;
        FProgressing := False;
      end;
    end;
end;

procedure TCommunicationFramework_Client_Indy.TriggerQueueData(v: PQueueData);
begin
  if Connected then
    begin
      ClientIntf.PostQueueData(v);
    end
  else
      DisposeQueueData(v);
end;

procedure TCommunicationFramework_Client_Indy.AsyncConnect(Addr: SystemString; Port: Word; OnResult: TStateCall);
begin
  AsyncConnect(Addr, Port, OnResult, nil, nil);
end;

procedure TCommunicationFramework_Client_Indy.AsyncConnect(Addr: SystemString; Port: Word; OnResult: TStateMethod);
begin
  AsyncConnect(Addr, Port, nil, OnResult, nil);
end;

procedure TCommunicationFramework_Client_Indy.AsyncConnect(Addr: SystemString; Port: Word; OnResult: TStateProc);
begin
  AsyncConnect(Addr, Port, nil, nil, OnResult);
end;

function TCommunicationFramework_Client_Indy.Connect(Addr: string; Port: Word): Boolean;
var
  t: TTimeTickValue;
begin
  Result := False;

  Disconnect;

  disposeObject(ClientIntf);

  FDriver := TIdTCPClient.Create(nil);
  ClientIntf := TClientIntf.Create(Self, FDriver);
  FProgressing := False;

  if IsIPV4(Addr) then
      FDriver.IPVersion := TIdIPVersion.Id_IPv4
  else if IsIPV6(Addr) then
      FDriver.IPVersion := TIdIPVersion.Id_IPv6
  else
      FDriver.IPVersion := DefaultIPVersion;

  FDriver.host := Addr;
  FDriver.Port := Port;
  FDriver.BoundIP := '';
  FDriver.BoundPort := 0;
  FDriver.ReuseSocket := TIdReuseSocket.rsFalse;
  FDriver.UseNagle := False;
  ProgressBackground;

  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := nil;
  FOnAsyncConnectNotifyProc := nil;

  FDriver.ConnectTimeout := 500;
  try
    FDriver.Connect;
    ProgressBackground;
  except
    if (IsIPV4(Addr)) or (IsIPV6(Addr)) then
      begin
        FDriver := TIdTCPClient.Create(nil);
        FDriver.IPVersion := DefaultIPVersion;
        FDriver.ConnectTimeout := 0;
        FDriver.ReadTimeout := -1;
        FDriver.UseNagle := False;

        disposeObject(ClientIntf);
        ClientIntf := TClientIntf.Create(Self, FDriver);
        exit;
      end
    else
      begin
        if DefaultIPVersion = TIdIPVersion.Id_IPv4 then
            DefaultIPVersion := TIdIPVersion.Id_IPv6
        else
            DefaultIPVersion := TIdIPVersion.Id_IPv4;

        try
          FDriver.Connect;
          ProgressBackground;
        except
          if (not IsIPV4(Addr)) and (not IsIPV6(Addr)) then
            begin
              if DefaultIPVersion = TIdIPVersion.Id_IPv4 then
                  DefaultIPVersion := TIdIPVersion.Id_IPv6
              else
                  DefaultIPVersion := TIdIPVersion.Id_IPv4;
            end;

          FDriver := TIdTCPClient.Create(nil);
          FDriver.IPVersion := DefaultIPVersion;
          FDriver.ConnectTimeout := 0;
          FDriver.ReadTimeout := -1;
          FDriver.UseNagle := False;

          disposeObject(ClientIntf);
          ClientIntf := TClientIntf.Create(Self, FDriver);
          exit;
        end;
      end;
  end;

  if not FDriver.Connected then
      exit;

  DoConnected(ClientIntf);

  t := GetTimeTickCount + 3000;
  while (not RemoteInited) and (FDriver.Connected) and (GetTimeTickCount < t) do
    begin
      ProgressBackground;
      FDriver.IOHandler.CheckForDataOnSource(100);
    end;

  Result := (RemoteInited) and (FDriver.Connected);
end;

procedure TCommunicationFramework_Client_Indy.Disconnect;
begin
  if not Connected then
      exit;

  FDriver.Disconnect;
end;

initialization

DefaultIPVersion := TIdIPVersion.Id_IPv4;

finalization

end.
