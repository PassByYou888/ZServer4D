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

{$WARNINGS OFF}
{$HINTS OFF}

interface

uses CommunicationFramework, CoreClasses,
  DataFrameEngine, ListEngine, MemoryStream64,

  Classes, SysUtils,
  IdTCPClient, IdYarn,
  IDGlobal, IdBaseComponent, IdComponent, IdTCPConnection, IdContext,
  DoStatusIO, UnicodeMixedLib, PascalStrings;

type
  TCommunicationFramework_Client_Indy = class;

  TClientIntf = class(TPeerClient)
  public
    function Context: TIdTCPClient;

    function Connected: Boolean; override;
    procedure Disconnect; override;
    procedure SendByteBuffer(Buff: PByte; size: Integer); override;
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
    LastTimtTick: Cardinal;
  public
    constructor Create; override;
    destructor Destroy; override;

    function Connected: Boolean; override;
    function ClientIO: TPeerClient; override;
    procedure ProgressBackground; override;
    procedure TriggerQueueData(v: PQueueData); override;

    function Connect(Addr: string; port: Word): Boolean; override;
    procedure Disconnect; override;
  end;

procedure CheckIPV6(hostName: string; port: Word);

var
  DefaultIPVersion: TIdIPVersion;

implementation

procedure CheckIPV6(hostName: string; port: Word);
var
  cli: TIdTCPClient;
begin
  cli := TIdTCPClient.Create(nil);
  cli.host := hostName;
  cli.port := port;
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

function ToIDBytes(p: PByte; size: Integer): TIdBytes; inline;
var
  i: Integer;
begin
  SetLength(Result, size);
  for i := 0 to size - 1 do
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

procedure TClientIntf.SendByteBuffer(Buff: PByte; size: Integer);
begin
  if size > 0 then
      Context.IOHandler.Write(ToIDBytes(Buff, size));
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
  t: TTimeTickValue;
begin
  if FProgressing then
      exit;

  if Connected then
    begin
      FProgressing := True;

      FDriver.IOHandler.CheckForDataOnSource(1);
      try
        while FDriver.IOHandler.InputBuffer.size > 0 do
          begin
            ClientIntf.ReceivedBuffer.Position := ClientIntf.ReceivedBuffer.size;
            FDriver.IOHandler.InputBuffer.ExtractToStream(ClientIntf.ReceivedBuffer);
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
                  if FDriver.IOHandler.InputBuffer.size > 0 then
                    begin
                      t := GetTimeTickCount + 15000;

                      ClientIntf.ReceivedBuffer.Position := ClientIntf.ReceivedBuffer.size;
                      FDriver.IOHandler.InputBuffer.ExtractToStream(ClientIntf.ReceivedBuffer);
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

        try
            inherited ProgressBackground;
        except
          ClientIntf.Disconnect;
          FProgressing := False;
          exit;
        end;

      finally
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

function TCommunicationFramework_Client_Indy.Connect(Addr: string; port: Word): Boolean;
var
  t: TTimeTickValue;
begin
  Result := False;

  Disconnect;

  disposeObject(ClientIntf);

  ClientIntf := TClientIntf.Create(Self, FDriver);
  FProgressing := False;

  // debug used
  // if umlTrimSpace(host).Same('2607:f0d0:3802:b2::3') then
  // DefaultIPVersion := TIdIPVersion.Id_IPv4;
  // host := '169.45.214.146';

  FDriver.host := Addr;
  FDriver.port := port;
  FDriver.BoundIP := '';
  FDriver.BoundPort := 0;
  FDriver.IPVersion := DefaultIPVersion;
  FDriver.ReuseSocket := TIdReuseSocket.rsFalse;
  FDriver.UseNagle := False;
  ProgressBackground;

  try
    t := GetTimeTickCount;

    repeat
      try
        FDriver.Connect;
        TThread.Sleep(10);
        ProgressBackground;
      except
      end;

      if (GetTimeTickCount - t > 5000) then
          break;
    until FDriver.Connected;

    if FDriver.Connected then
        DoConnected(ClientIntf);

    while (not RemoteInited) and (FDriver.Connected) and (GetTimeTickCount - t < 2000) do
        ProgressBackground;

    Result := (RemoteInited) and (FDriver.Connected);
  except
    Result := False;

    if DefaultIPVersion = TIdIPVersion.Id_IPv4 then
        DefaultIPVersion := TIdIPVersion.Id_IPv6
    else
        DefaultIPVersion := TIdIPVersion.Id_IPv4;

    FDriver := TIdTCPClient.Create(nil);
    FDriver.host := Addr;
    FDriver.port := port;
    FDriver.BoundIP := '';
    FDriver.BoundPort := 0;
    FDriver.IPVersion := DefaultIPVersion;
    FDriver.ConnectTimeout := 0;
    FDriver.ReadTimeout := -1;
    FDriver.UseNagle := False;

    disposeObject(ClientIntf);
    ClientIntf := TClientIntf.Create(Self, FDriver);

    try
      t := GetTimeTickCount;

      repeat
        try
          FDriver.Connect;
          TThread.Sleep(10);
          ProgressBackground;
        except
        end;

        if (GetTimeTickCount - t > 5000) then
            break;
      until FDriver.Connected;

      if FDriver.Connected then
          DoConnected(ClientIntf);

      while (not RemoteInited) and (FDriver.Connected) and (GetTimeTickCount - t < 2000) do
          ProgressBackground;
      Result := (RemoteInited) and (FDriver.Connected);
    except
      if DefaultIPVersion = TIdIPVersion.Id_IPv4 then
          DefaultIPVersion := TIdIPVersion.Id_IPv6
      else
          DefaultIPVersion := TIdIPVersion.Id_IPv4;

      FDriver := TIdTCPClient.Create(nil);
      FDriver.IPVersion := DefaultIPVersion;
      FDriver.ConnectTimeout := 0;
      FDriver.ReadTimeout := -1;
      FDriver.UseNagle := False;

      disposeObject(ClientIntf);
      ClientIntf := TClientIntf.Create(Self, FDriver);
    end;
  end;
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
