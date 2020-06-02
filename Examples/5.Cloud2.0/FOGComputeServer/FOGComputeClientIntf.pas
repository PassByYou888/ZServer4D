unit FOGComputeClientIntf;

interface

uses Variants, SysUtils, Types, DateUtils,
  CommunicationFrameworkDoubleTunnelIO_NoAuth,
  CoreClasses, TextDataEngine, ListEngine, CommunicationFramework,
  DoStatusIO, UnicodeMixedLib, DataFrameEngine, Cadencer,
  NotifyObjectBase,
  XNATPhysics,
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
  SendTunnel.SendStreamCmdP('SimulateCompute5Sec', de, OnResult);
  DisposeObject(de);
end;

end.
 
