unit FOGComputeClientIntf;

interface

uses Variants, SysUtils, Types, DateUtils,
  CommunicationFrameworkDoubleTunnelIO_NoAuth,
  CoreClasses, TextDataEngine, ListEngine, CommunicationFramework,
  DoStatusIO, UnicodeMixedLib, DataFrameEngine, Cadencer,
  NotifyObjectBase,
  CommunicationFramework_Client_Indy,
  PascalStrings, MemoryStream64;

type
  TFogCompute_DoubleTunnelClient = class(TCommunicationFramework_DoubleTunnelClient_NoAuth)
  protected
  public
    NetRecvTunnelIntf, NetSendTunnelIntf: TCommunicationFrameworkClient;

    constructor Create(ClientClass: TCommunicationFrameworkClientClass);
    destructor Destroy; override;

    procedure RegisterCommand; override;
    procedure UnRegisterCommand; override;

    function Connect(addr: SystemString; const FogCliRecvPort, FogCliSendPort: Word): Boolean; override;
    procedure Disconnect; override;

    procedure SimulateCompute5Sec(exp: string; OnResult: TStreamProc);
  end;

implementation

constructor TFogCompute_DoubleTunnelClient.Create(ClientClass: TCommunicationFrameworkClientClass);
begin
  NetRecvTunnelIntf := ClientClass.Create;
  NetSendTunnelIntf := ClientClass.Create;
  NetSendTunnelIntf.PrintParams['AntiIdle'] := False;
  inherited Create(NetRecvTunnelIntf, NetSendTunnelIntf);

  SwitchAsMaxPerformance;
end;

destructor TFogCompute_DoubleTunnelClient.Destroy;
begin
  Disconnect;
  DisposeObject(NetRecvTunnelIntf);
  DisposeObject(NetSendTunnelIntf);
  inherited Destroy;
end;

procedure TFogCompute_DoubleTunnelClient.RegisterCommand;
begin
  inherited RegisterCommand;
end;

procedure TFogCompute_DoubleTunnelClient.UnRegisterCommand;
begin
  inherited UnRegisterCommand;
end;

function TFogCompute_DoubleTunnelClient.Connect(addr: SystemString; const FogCliRecvPort, FogCliSendPort: Word): Boolean;
var
  t: Cardinal;
begin
  Result := False;
  Disconnect;

  if not NetSendTunnelIntf.Connect(addr, FogCliSendPort) then
    begin
      DoStatus('connect %s failed!', [addr]);
      exit;
    end;
  if not NetRecvTunnelIntf.Connect(addr, FogCliRecvPort) then
    begin
      DoStatus('connect %s failed!', [addr]);
      exit;
    end;

  if not Connected then
      exit;

  t := TCoreClassThread.GetTickCount + 4000;
  while not RemoteInited do
    begin
      if TCoreClassThread.GetTickCount > t then
          break;
      if not Connected then
          break;
      Progress;
    end;

  if Connected then
    begin
      DoStatus('connect fog compute service "%s" ok!', [addr]);
      Result := TunnelLink;
    end;
end;

procedure TFogCompute_DoubleTunnelClient.Disconnect;
begin
  NetSendTunnelIntf.Disconnect;
  NetRecvTunnelIntf.Disconnect;
end;

procedure TFogCompute_DoubleTunnelClient.SimulateCompute5Sec(exp: string; OnResult: TStreamProc);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  de.WriteString(exp);
  SendTunnel.SendStreamCmd('SimulateCompute5Sec', de, OnResult);
  DisposeObject(de);
end;

end.
