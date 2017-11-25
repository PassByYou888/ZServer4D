unit ManagerServer_ClientIntf;

interface

uses System.IOUtils, SysUtils,
  CommunicationFrameworkDoubleTunnelIO_NoAuth,
  CoreClasses, TextDataEngine, ListEngine, CommunicationFramework,
  DoStatusIO, UnicodeMixedLib, DataFrameEngine,
  CommunicationFramework_Client_CrossSocket,
  CommunicationFramework_Client_ICS, NotifyObjectBase;

type
  TManagerClients = class;

  TManCliConnectInfo = record
    RegName, ManServAddr, RegAddr: string;
    RegRecvPort, RegSendPort: Word;
    ServerType: Byte;
  end;

  TManagerClient = class(TCommunicationFramework_DoubleTunnelClient_NoAuth)
  protected
    procedure PostExecute_RegServer(Sender: TNPostExecute);
    procedure Command_RegServer(Sender: TPeerClient; InData: TDataFrameEngine);

    procedure PostExecute_Offline(Sender: TNPostExecute);
    procedure Command_Offline(Sender: TPeerClient; InData: TDataFrameEngine);
  public
    Owner                               : TManagerClients;
    NetRecvTunnelIntf, NetSendTunnelIntf: TCommunicationFramework_Client_CrossSocket;
    ConnectInfo                         : TManCliConnectInfo;
    ReconnectTotal                      : Integer;
    constructor Create(AOwner: TManagerClients);
    destructor Destroy; override;

    procedure RegisterCommand; override;
    procedure UnRegisterCommand; override;

    function Connect(addr: string): Boolean;
    procedure Disconnect;

    procedure AntiIdle(workLoad: Word);
    function EnabledServer(const RegName, ManServAddr, RegAddr: string; const RegRecvPort, RegSendPort: Word; ServerType: Byte): Boolean;
  end;

  TManagerClient_ServerConfigChange = procedure(Sender: TManagerClient; ConfigData: TSectionTextData) of object;
  TManagerClient_ServerOffline      = procedure(Sender: TManagerClient; RegAddr: string; ServerType: Byte) of object;

  TManagerClients = class(TCoreClassPersistent)
  protected
    FClientList  : TCoreClassListForObj;
    AntiIdleIsRun: Boolean;

    function GetItems(index: Integer): TManagerClient;
    procedure SetItems(index: Integer; const Value: TManagerClient);
  public
    ServerConfig                    : TSectionTextData;
    LastManagerServerAddr           : string;
    LastRegAddr                     : string;
    LastRegRecvPort, LastRegSendPort: Word;
    OnServerConfigChange            : TManagerClient_ServerConfigChange;
    OnServerOffline                 : TManagerClient_ServerOffline;

    constructor Create;
    destructor Destroy; override;

    function ActivtedCount: Integer;
    function Count: Integer;
    property Items[index: Integer]: TManagerClient read GetItems write SetItems; default;

    procedure Clear;

    procedure Progress;
    procedure AntiIdle(workLoad: Word);
    function BuildClientAndConnect(const RegName, ManServAddr, RegAddr: string; const RegRecvPort, RegSendPort: Word; ServerType: Byte): Boolean;
  end;

const
  cDBServer        = 1;
  cCoreLogicServer = 2;
  cManagerServer   = 3;
  cPayService      = 61;
  cPayQueryService = 88;
  cUnknowServer    = 99;

function serverType2Str(t: Byte): string;

implementation

function serverType2Str(t: Byte): string;
begin
  case t of
    cDBServer: Result := 'DB_Server';
    cCoreLogicServer: Result := 'CoreLogic_Server';
    cManagerServer: Result := 'manager_Server';
    cPayService: Result := 'payment Server';
    cPayQueryService: Result := 'payment query Server';
    else Result := 'unknow_server';
  end;
end;

procedure TManagerClient.PostExecute_RegServer(Sender: TNPostExecute);
var
  te: TSectionTextData;
begin
  te := Sender.Data2 as TSectionTextData;
  if Assigned(Owner.OnServerConfigChange) then
      Owner.OnServerConfigChange(Self, te);
  DisposeObject(te);
end;

procedure TManagerClient.Command_RegServer(Sender: TPeerClient; InData: TDataFrameEngine);
var
  te: TSectionTextData;
begin
  te := TSectionTextData.Create;
  InData.Reader.ReadSectionText(te);
  Owner.ServerConfig.Merge(te);
  with Sender.OwnerFramework.ProgressPost.PostExecute(nil) do
    begin
      Data1 := Sender;
      Data2 := te;
      OnExecuteMethod := PostExecute_RegServer;
    end;
end;

procedure TManagerClient.PostExecute_Offline(Sender: TNPostExecute);
var
  RegAddr   : string;
  ServerType: Byte;

  ns: TCoreClassStringList;
  i : Integer;
  vl: THashVariantList;
begin
  RegAddr := Sender.DataEng.Reader.ReadString;
  ServerType := Sender.DataEng.Reader.ReadByte;

  ns := TCoreClassStringList.Create;
  Owner.ServerConfig.ReBuildList;
  Owner.ServerConfig.GetSectionList(ns);

  for i := 0 to ns.Count - 1 do
    begin
      vl := Owner.ServerConfig.VariantList[ns[i]];
      if SameText(string(vl.GetDefaultValue('Host', RegAddr)), RegAddr) and
        (Byte(vl.GetDefaultValue('Type', ServerType)) = ServerType) then
          Owner.ServerConfig.Delete(ns[i]);
    end;

  DisposeObject(ns);
  Owner.ServerConfig.ReBuildList;

  if Assigned(Owner.OnServerOffline) then
      Owner.OnServerOffline(Self, RegAddr, ServerType);
end;

procedure TManagerClient.Command_Offline(Sender: TPeerClient; InData: TDataFrameEngine);
begin
  Sender.OwnerFramework.ProgressPost.PostExecute(InData, PostExecute_Offline);
end;

constructor TManagerClient.Create(AOwner: TManagerClients);
begin
  Owner := AOwner;
  NetRecvTunnelIntf := TCommunicationFramework_Client_CrossSocket.Create;
  NetSendTunnelIntf := TCommunicationFramework_Client_CrossSocket.Create;
  NetSendTunnelIntf.PrintParams['AntiIdle'] := False;
  ConnectInfo.RegName := '';
  ConnectInfo.ManServAddr := '';
  ConnectInfo.RegAddr := '';
  ConnectInfo.RegRecvPort := 0;
  ConnectInfo.RegSendPort := 0;
  ConnectInfo.ServerType := cUnknowServer;
  ReconnectTotal := 0;
  inherited Create(NetRecvTunnelIntf, NetSendTunnelIntf);
end;

destructor TManagerClient.Destroy;
begin
  Disconnect;
  DisposeObject(NetRecvTunnelIntf);
  DisposeObject(NetSendTunnelIntf);
  inherited Destroy;
end;

procedure TManagerClient.RegisterCommand;
begin
  inherited RegisterCommand;
  NetRecvTunnelIntf.RegisterDirectStream('RegServer').OnExecute := Command_RegServer;
  NetRecvTunnelIntf.RegisterDirectStream('Offline').OnExecute := Command_Offline;
end;

procedure TManagerClient.UnRegisterCommand;
begin
  inherited UnRegisterCommand;
  NetRecvTunnelIntf.DeleteRegistedCMD('RegServer');
  NetRecvTunnelIntf.DeleteRegistedCMD('Offline');
end;

function TManagerClient.Connect(addr: string): Boolean;
var
  t: Cardinal;
begin
  Result := False;
  Disconnect;
  RegisterCommand;

  if not NetSendTunnelIntf.Connect(addr, '13335') then
    begin
      DoStatus('connect %s failed!', [addr]);
      exit;
    end;
  if not NetRecvTunnelIntf.Connect(addr, '13336') then
    begin
      DoStatus('connect %s failed!', [addr]);
      exit;
    end;

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
      DoStatus('connect manager service "%s" ok!', [addr]);
      Result := TunnelLink;
    end;
end;

procedure TManagerClient.Disconnect;
begin
  UnRegisterCommand;
  NetSendTunnelIntf.Disconnect;
  NetRecvTunnelIntf.Disconnect;
end;

procedure TManagerClient.AntiIdle(workLoad: Word);
var
  sendDE: TDataFrameEngine;
begin
  sendDE := TDataFrameEngine.Create;
  sendDE.WriteWORD(workLoad);
  SendTunnel.SendDirectStreamCmd('AntiIdle', sendDE);
  DisposeObject(sendDE);
end;

function TManagerClient.EnabledServer(const RegName, ManServAddr, RegAddr: string; const RegRecvPort, RegSendPort: Word; ServerType: Byte): Boolean;
var
  SendData, ResultData: TDataFrameEngine;
begin
  ConnectInfo.RegName := RegName;
  ConnectInfo.ManServAddr := ManServAddr;
  ConnectInfo.RegAddr := RegAddr;
  ConnectInfo.RegRecvPort := RegRecvPort;
  ConnectInfo.RegSendPort := RegSendPort;
  ConnectInfo.ServerType := ServerType;

  Result := False;

  SendData := TDataFrameEngine.Create;
  ResultData := TDataFrameEngine.Create;

  SendData.WriteString(ManServAddr);
  SendData.WriteString(RegName);
  SendData.WriteString(RegAddr);
  SendData.WriteWORD(RegRecvPort);
  SendData.WriteWORD(RegSendPort);
  SendData.WriteWORD(0);
  SendData.WriteByte(ServerType);

  DoStatus('send enabled cmd:%s %s [n:%s][addr:%s][r:%d][s:%d][w:%d]', [ManServAddr, serverType2Str(ServerType), RegName, RegAddr, RegRecvPort, RegSendPort, 0]);

  SendTunnel.WaitSendStreamCmd('EnabledServer', SendData, ResultData, 5000);

  if ResultData.Count = 2 then
    begin
      Result := ResultData.Reader.ReadBool;
      DoStatus(ResultData.Reader.ReadString);
    end;

  DisposeObject(SendData);
  DisposeObject(ResultData);
end;

function TManagerClients.GetItems(index: Integer): TManagerClient;
begin
  Result := FClientList[index] as TManagerClient;
end;

procedure TManagerClients.SetItems(index: Integer; const Value: TManagerClient);
begin
  FClientList[index] := Value;
end;

constructor TManagerClients.Create;
begin
  inherited Create;
  FClientList := TCoreClassListForObj.Create;
  LastManagerServerAddr := '';
  LastRegAddr := '';
  LastRegRecvPort := 0;
  LastRegSendPort := 0;
  AntiIdleIsRun := False;
  ServerConfig := TSectionTextData.Create;
  OnServerConfigChange := nil;
  OnServerOffline := nil;
end;

destructor TManagerClients.Destroy;
begin
  Clear;
  DisposeObject(ServerConfig);
  DisposeObject(FClientList);
  inherited Destroy;
end;

function TManagerClients.ActivtedCount: Integer;
var
  i: Integer;
  c: TManagerClient;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    begin
      c := Items[i];
      if (c.LinkOk) and (c.Connected) then
          inc(Result);
    end;
end;

function TManagerClients.Count: Integer;
begin
  Result := FClientList.Count;
end;

procedure TManagerClients.Clear;
var
  i: Integer;
begin
  try
    for i := 0 to Count - 1 do
      begin
        Items[i].Disconnect;
        DisposeObject(Items[i]);
      end;
  except
  end;
  FClientList.Clear;
end;

procedure TManagerClients.Progress;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Items[i].Progress;
end;

procedure TManagerClients.AntiIdle(workLoad: Word);
var
  i       : Integer;
  conninfo: TManCliConnectInfo;
  c       : TManagerClient;
begin
  if AntiIdleIsRun then
      exit;
  AntiIdleIsRun := True;
  try
    i := 0;
    while i < Count do
      begin
        c := Items[i];
        if (not c.LinkOk) or (not c.Connected) then
          begin
            conninfo := c.ConnectInfo;
            inc(c.ReconnectTotal);

            if c.Connect(conninfo.ManServAddr) then
              begin
                DoStatus('reconnect call enabled api 2 send cmd:%s %s [n:%s][addr:%s][r:%d][s:%d][w:%d]',
                  [LastManagerServerAddr, serverType2Str(conninfo.ServerType), conninfo.RegName, LastRegAddr, LastRegRecvPort, LastRegSendPort, 0]);
                if c.EnabledServer(conninfo.RegName, conninfo.ManServAddr, conninfo.RegAddr, conninfo.RegRecvPort, conninfo.RegSendPort, conninfo.ServerType) then
                  begin
                    inc(i);
                    continue;
                  end;
                c.Disconnect;
              end;
          end
        else
            c.AntiIdle(workLoad);
        inc(i);
      end;
  except
  end;
  AntiIdleIsRun := False;
end;

function TManagerClients.BuildClientAndConnect(const RegName, ManServAddr, RegAddr: string; const RegRecvPort, RegSendPort: Word; ServerType: Byte): Boolean;
var
  c: TManagerClient;
  i: Integer;
begin
  Result := False;

  for i := 0 to Count - 1 do
    begin
      c := Items[i];
      if SameText(c.ConnectInfo.ManServAddr, ManServAddr) and
        SameText(c.ConnectInfo.RegAddr, RegAddr) and
        (c.ConnectInfo.RegRecvPort = RegRecvPort) and
        (c.ConnectInfo.RegSendPort = RegSendPort) and
        (c.ConnectInfo.ServerType = ServerType) then
        begin
          c.ReconnectTotal := 0;
          exit;
        end;
    end;

  c := TManagerClient.Create(Self);
  if c.Connect(ManServAddr) then
    begin
      DoStatus('call enabled api 2 send cmd:%s %s [n:%s][addr:%s][r:%d][s:%d][w:%d]', [ManServAddr, serverType2Str(ServerType), RegName, RegAddr, RegRecvPort, RegSendPort, 0]);
      Result := c.EnabledServer(RegName, ManServAddr, RegAddr, RegRecvPort, RegSendPort, ServerType);
      if Result then
        begin
          FClientList.Add(c);
          LastManagerServerAddr := ManServAddr;
          LastRegAddr := RegAddr;
          LastRegRecvPort := RegRecvPort;
          LastRegSendPort := RegSendPort;
        end;
    end
  else
      DisposeObject(c);
end;

end.
