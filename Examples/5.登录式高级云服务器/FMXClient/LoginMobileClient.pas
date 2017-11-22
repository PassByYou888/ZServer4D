unit LoginMobileClient;

interface

uses System.SysUtils, System.IOUtils,
  CommunicationFrameworkDoubleTunnelIO,
  CoreClasses, TextDataEngine, ListEngine, CommunicationFramework,
  DoStatusIO, UnicodeMixedLib, DataFrameEngine,
  NotifyObjectBase, MemoryStream64;

type
  ILoginBackCallInterface = interface
    procedure LoginDisconnect;
  end;

  TRegGuestStateProc = reference to procedure(const UserName, UserPasswd: string; const State: Boolean);

  TLoginClientBase = class(TCommunicationFramework_DoubleTunnelClient)
  protected
    LoginBackCallInterface: ILoginBackCallInterface;
    LastConnectState      : Boolean;
  protected
    procedure ClientConnected(Sender: TCommunicationFrameworkClient); override;
    procedure Disconnect_PostExecute(Sender: TNPostExecute);
    procedure ClientDisconnect(Sender: TCommunicationFrameworkClient); override;
  public
    NetRecvTunnelIntf, NetSendTunnelIntf: TCommunicationFrameworkClient;
    AutoReconnect                       : Boolean;
    constructor Create(ALoginBackCallInterface: ILoginBackCallInterface; ARecvTunnel, ASendTunnel: TCommunicationFrameworkClient);
    destructor Destroy; override;

    procedure Progress; override;

    procedure RegisterCommand; override;
    procedure UnRegisterCommand; override;

    procedure Disconnect;

    procedure AntiIdle();

    procedure RegGuestUser(const OnProc: TRegGuestStateProc);
  end;

implementation

procedure TLoginClientBase.ClientConnected(Sender: TCommunicationFrameworkClient);
begin
end;

procedure TLoginClientBase.Disconnect_PostExecute(Sender: TNPostExecute);
begin
  LoginBackCallInterface.LoginDisconnect;
end;

procedure TLoginClientBase.ClientDisconnect(Sender: TCommunicationFrameworkClient);
begin
  if AutoReconnect then
      ProgressEngine.PostExecute(0.5, Disconnect_PostExecute);
end;

constructor TLoginClientBase.Create(ALoginBackCallInterface: ILoginBackCallInterface; ARecvTunnel, ASendTunnel: TCommunicationFrameworkClient);
begin
  NetRecvTunnelIntf := ARecvTunnel;
  NetSendTunnelIntf := ASendTunnel;
  inherited Create(NetRecvTunnelIntf, NetSendTunnelIntf);
  LoginBackCallInterface := ALoginBackCallInterface;
  NetSendTunnelIntf.PrintParams['AntiIdle'] := False;
  LastConnectState := False;
  AutoReconnect := False;
end;

destructor TLoginClientBase.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

procedure TLoginClientBase.Progress;
begin
  inherited Progress;

  if not NetSendTunnelIntf.Connected then
    begin
      if LastConnectState then
          NetSendTunnelIntf.TriggerDoDisconnect;
      LastConnectState := False;
      exit;
    end;

  if NetSendTunnelIntf.ClientIO.StopCommunicationTime > 10 * 1000 then
    begin
      AntiIdle;
      if NetSendTunnelIntf.Wait(1000) = '' then
        begin
          if LastConnectState then
              NetSendTunnelIntf.TriggerDoDisconnect;
          LastConnectState := False;
          exit;
        end;
      NetSendTunnelIntf.ClientIO.SetLastCommunicationTimeAsCurrent;
    end;
end;

procedure TLoginClientBase.RegisterCommand;
begin
  inherited RegisterCommand;
end;

procedure TLoginClientBase.UnRegisterCommand;
begin
  inherited UnRegisterCommand;
end;

procedure TLoginClientBase.Disconnect;
begin
  UnRegisterCommand;
  if NetSendTunnelIntf.ClientIO <> nil then
      NetSendTunnelIntf.ClientIO.Disconnect;
  if NetRecvTunnelIntf.ClientIO <> nil then
      NetRecvTunnelIntf.ClientIO.Disconnect;
end;

procedure TLoginClientBase.AntiIdle;
var
  sendDE: TDataFrameEngine;
begin
  sendDE := TDataFrameEngine.Create;
  SendTunnel.SendDirectStreamCmd('AntiIdle', sendDE);
  DisposeObject(sendDE);
end;

procedure TLoginClientBase.RegGuestUser(const OnProc: TRegGuestStateProc);
var
  sendDE: TDataFrameEngine;
begin
  sendDE := TDataFrameEngine.Create;
  SendTunnel.SendStreamCmd('RegGuestUser', sendDE,
    procedure(Sender: TPeerClient; ResultData: TDataFrameEngine)
    var
      r: Boolean;
      UserName, UserPasswd: string;
    begin
      if ResultData.Count > 0 then
        begin
          r := ResultData.Reader.ReadBool;
          if r then
            begin
              UserName := ResultData.Reader.ReadString;
              UserPasswd := ResultData.Reader.ReadString;
            end;
          if Assigned(OnProc) then
              OnProc(UserName, UserPasswd, r);
        end;
    end);

  DisposeObject(sendDE);
end;

end.
