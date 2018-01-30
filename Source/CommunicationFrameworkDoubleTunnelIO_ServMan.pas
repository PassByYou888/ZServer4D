unit CommunicationFrameworkDoubleTunnelIO_ServMan;

interface

{$I zDefine.inc}


uses
  SysUtils, TypInfo,

  CommunicationFrameworkDoubleTunnelIO_NoAuth,
  CoreClasses, TextDataEngine, ListEngine, CommunicationFramework, DoStatusIO, UnicodeMixedLib,
  DataFrameEngine,

  NotifyObjectBase, CoreCipher, PascalStrings, MemoryStream64;

{$I ServerManTypeDefine.inc}


type
  TServerManager_ClientPool = class;
  TServerManager_Client     = class;
  TServerManager            = class;

  IServerManager_ClientPoolNotify = interface
    procedure ServerConfigChange(Sender: TServerManager_Client; ConfigData: TSectionTextData);
    procedure ServerOffline(Sender: TServerManager_Client; RegAddr: SystemString; ServerType: TServerType);
  end;

  TServerManager_ClientConnectInfo = record
    RegName, ManServAddr, RegAddr: SystemString;
    ManCliRecvPort, ManCliSendPort, RegRecvPort, RegSendPort: word;
    ServerType: TServerType;
  end;

  TServerManager_Client = class(TCommunicationFramework_DoubleTunnelClient_NoAuth)
  protected
    procedure PostExecute_RegServer(Sender: TNPostExecute);
    procedure Command_RegServer(Sender: TPeerIO; InData: TDataFrameEngine);

    procedure PostExecute_Offline(Sender: TNPostExecute);
    procedure Command_Offline(Sender: TPeerIO; InData: TDataFrameEngine);
  public
    Owner                               : TServerManager_ClientPool;
    NetRecvTunnelIntf, NetSendTunnelIntf: TCommunicationFrameworkClient;
    ConnectInfo                         : TServerManager_ClientConnectInfo;
    ReconnectTotal                      : Integer;

    constructor Create(AOwner: TServerManager_ClientPool);
    destructor Destroy; override;

    procedure RegisterCommand; override;
    procedure UnRegisterCommand; override;

    function ConnectAndLink(addr: SystemString; const RecvPort, SendPort: word): Boolean;

    procedure AntiIdle(WorkLoad: word);
    function EnabledServer(const RegName, ManServAddr, RegAddr: SystemString; const RegRecvPort, RegSendPort: word; ServerType: TServerType): Boolean;
  end;

  TServerManager_ClientPool = class(TCoreClassPersistent)
  protected
    FClientList  : TCoreClassListForObj;
    AntiIdleIsRun: Boolean;

    function GetItems(index: Integer): TServerManager_Client;
    procedure SetItems(index: Integer; const Value: TServerManager_Client);
  public
    ServerConfig                            : TSectionTextData;
    LastManagerServerAddr                   : SystemString;
    LastManServRecvPort, LastManServSendPort: word;
    LastRegAddr                             : SystemString;
    LastRegRecvPort, LastRegSendPort        : word;
    DefaultClientClass                      : TCommunicationFrameworkClientClass;
    NotifyIntf                              : IServerManager_ClientPoolNotify;

    constructor Create(AClientClass: TCommunicationFrameworkClientClass; ANotifyIntf: IServerManager_ClientPoolNotify);
    destructor Destroy; override;

    function ActivtedCount: Integer;
    function Count: Integer;
    property Items[index: Integer]: TServerManager_Client read GetItems write SetItems; default;

    procedure Clear;

    procedure Progress; virtual;
    procedure AntiIdle(WorkLoad: word);

    function BuildClientAndConnect(const RegName, ManServAddr, RegAddr: SystemString;
      const ManCliRecvPort, ManCliSendPort, RegRecvPort, RegSendPort: word; ServerType: TServerType): Boolean;
  end;

  TServerManager_SendTunnelData = class(TPeerClientUserDefineForSendTunnel_NoAuth)
  public
    constructor Create(AOwner: TPeerIO); override;
    destructor Destroy; override;
  end;

  TServerManager_RecvTunnelData = class(TPeerClientUserDefineForRecvTunnel_NoAuth)
  public
    ManServAddr, RegName, RegAddr: SystemString;
    RegRecvPort, RegSendPort     : word;
    LastEnabled                  : TTimeTickValue;
    WorkLoad                     : word;
    ServerType                   : TServerType;
    SuccessEnabled               : Boolean;
  public
    constructor Create(AOwner: TPeerIO); override;
    destructor Destroy; override;

    procedure WriteConfig(t: TSectionTextData);
    function MakeRegName: SystemString;
  end;

  TServerManager = class(TCommunicationFramework_DoubleTunnelService_NoAuth, IServerManager_ClientPoolNotify)
  protected
    procedure UserLinkSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth); override;
    procedure UserOut(UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth); override;
    procedure PostExecute_ServerOffline(Sender: TNPostExecute);
    procedure PostExecute_RegServer(Sender: TNPostExecute);
  protected
    procedure Command_EnabledServer(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
    procedure PostExecute_Disconnect(Sender: TNPostExecute);
    procedure Command_AntiIdle(Sender: TPeerIO; InData: TDataFrameEngine);
  protected
    procedure ServerConfigChange(Sender: TServerManager_Client; ConfigData: TSectionTextData);
    procedure ServerOffline(Sender: TServerManager_Client; RegAddr: SystemString; ServerType: TServerType);
  public
    ServerConfig     : TSectionTextData;
    ServManClientPool: TServerManager_ClientPool;
    LastTimeTick     : TTimeTickValue;

    constructor Create(ARecvTunnel, ASendTunnel: TCommunicationFrameworkServer; AClientPoolDefaultClass: TCommunicationFrameworkClientClass);
    destructor Destroy; override;

    procedure RegisterCommand; override;
    procedure UnRegisterCommand; override;

    procedure Progress; override;
  end;

function serverType2Str(t: TServerType): SystemString;

implementation

function serverType2Str(t: TServerType): SystemString;
begin
  Result := GetEnumName(TypeInfo(TServerType), Ord(t));
end;

procedure TServerManager_Client.PostExecute_RegServer(Sender: TNPostExecute);
var
  te: TSectionTextData;
begin
  te := Sender.Data2 as TSectionTextData;
  if Assigned(Owner.NotifyIntf) then
      Owner.NotifyIntf.ServerConfigChange(Self, te);

  DisposeObject(te);
end;

procedure TServerManager_Client.Command_RegServer(Sender: TPeerIO; InData: TDataFrameEngine);
var
  te: TSectionTextData;
begin
  te := TSectionTextData.Create;
  InData.Reader.ReadSectionText(te);
  Owner.ServerConfig.Merge(te);
  with ProgressEngine.PostExecute(nil) do
    begin
      Data1 := Sender;
      Data2 := te;
      {$IFDEF FPC}
      OnExecuteMethod := @PostExecute_RegServer;
      {$ELSE}
      OnExecuteMethod := PostExecute_RegServer;
      {$ENDIF}
    end;
end;

procedure TServerManager_Client.PostExecute_Offline(Sender: TNPostExecute);
var
  RegAddr   : SystemString;
  ServerType: TServerType;

  ns: TCoreClassStringList;
  i : Integer;
  vl: THashVariantList;
begin
  RegAddr := Sender.DataEng.Reader.ReadString;
  ServerType := TServerType(Sender.DataEng.Reader.ReadByte);

  ns := TCoreClassStringList.Create;
  Owner.ServerConfig.ReBuildList;
  Owner.ServerConfig.GetSectionList(ns);

  for i := 0 to ns.Count - 1 do
    begin
      vl := Owner.ServerConfig.VariantList[ns[i]];
      if SameText(SystemString(vl.GetDefaultValue('Host', RegAddr)), RegAddr) and
        (TServerType(byte(vl.GetDefaultValue('Type', ServerType))) = ServerType) then
          Owner.ServerConfig.Delete(ns[i]);
    end;

  DisposeObject(ns);
  Owner.ServerConfig.ReBuildList;

  if Assigned(Owner.NotifyIntf) then
      Owner.NotifyIntf.ServerOffline(Self, RegAddr, ServerType);
end;

procedure TServerManager_Client.Command_Offline(Sender: TPeerIO; InData: TDataFrameEngine);
begin
  {$IFDEF FPC}
  ProgressEngine.PostExecute(InData, @PostExecute_Offline);
  {$ELSE}
  ProgressEngine.PostExecute(InData, PostExecute_Offline);
  {$ENDIF}
end;

constructor TServerManager_Client.Create(AOwner: TServerManager_ClientPool);
begin
  Owner := AOwner;
  NetRecvTunnelIntf := Owner.DefaultClientClass.Create;
  NetSendTunnelIntf := Owner.DefaultClientClass.Create;
  NetSendTunnelIntf.PrintParams['AntiIdle'] := False;
  ConnectInfo.RegName := '';
  ConnectInfo.ManServAddr := '';
  ConnectInfo.RegAddr := '';
  ConnectInfo.RegRecvPort := 0;
  ConnectInfo.RegSendPort := 0;
  ConnectInfo.ServerType := TServerType.stUnknow;
  ReconnectTotal := 0;
  inherited Create(NetRecvTunnelIntf, NetSendTunnelIntf);

  RegisterCommand;

  SwitchAsDefaultPerformance;
end;

destructor TServerManager_Client.Destroy;
begin
  UnRegisterCommand;
  Disconnect;
  DisposeObject(NetRecvTunnelIntf);
  DisposeObject(NetSendTunnelIntf);
  inherited Destroy;
end;

procedure TServerManager_Client.RegisterCommand;
begin
  inherited RegisterCommand;
  {$IFDEF FPC}
  NetRecvTunnelIntf.RegisterDirectStream('RegServer').OnExecute := @Command_RegServer;
  NetRecvTunnelIntf.RegisterDirectStream('Offline').OnExecute := @Command_Offline;
  {$ELSE}
  NetRecvTunnelIntf.RegisterDirectStream('RegServer').OnExecute := Command_RegServer;
  NetRecvTunnelIntf.RegisterDirectStream('Offline').OnExecute := Command_Offline;
  {$ENDIF}
end;

procedure TServerManager_Client.UnRegisterCommand;
begin
  inherited UnRegisterCommand;
  NetRecvTunnelIntf.DeleteRegistedCMD('RegServer');
  NetRecvTunnelIntf.DeleteRegistedCMD('Offline');
end;

function TServerManager_Client.ConnectAndLink(addr: SystemString; const RecvPort, SendPort: word): Boolean;
begin
  Result := inherited Connect(addr, RecvPort, SendPort);

  if Result then
      Result := TunnelLink;

  if Result then
    begin
      ConnectInfo.ManServAddr := addr;
      ConnectInfo.ManCliRecvPort := RecvPort;
      ConnectInfo.ManCliSendPort := SendPort;
    end;
end;

procedure TServerManager_Client.AntiIdle(WorkLoad: word);
var
  SendDE: TDataFrameEngine;
begin
  SendDE := TDataFrameEngine.Create;
  SendDE.WriteWORD(WorkLoad);
  SendTunnel.SendDirectStreamCmd('AntiIdle', SendDE);
  DisposeObject(SendDE);
end;

function TServerManager_Client.EnabledServer(const RegName, ManServAddr, RegAddr: SystemString; const RegRecvPort, RegSendPort: word; ServerType: TServerType): Boolean;
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
  SendData.WriteByte(byte(ServerType));

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

function TServerManager_ClientPool.GetItems(index: Integer): TServerManager_Client;
begin
  Result := FClientList[index] as TServerManager_Client;
end;

procedure TServerManager_ClientPool.SetItems(index: Integer; const Value: TServerManager_Client);
begin
  FClientList[index] := Value;
end;

constructor TServerManager_ClientPool.Create(AClientClass: TCommunicationFrameworkClientClass; ANotifyIntf: IServerManager_ClientPoolNotify);
begin
  inherited Create;
  FClientList := TCoreClassListForObj.Create;
  LastManagerServerAddr := '';
  LastManServRecvPort := 0;
  LastManServSendPort := 0;
  LastRegAddr := '';
  LastRegRecvPort := 0;
  LastRegSendPort := 0;
  AntiIdleIsRun := False;
  ServerConfig := TSectionTextData.Create;
  DefaultClientClass := AClientClass;
  NotifyIntf := ANotifyIntf;
end;

destructor TServerManager_ClientPool.Destroy;
begin
  Clear;
  DisposeObject(ServerConfig);
  DisposeObject(FClientList);
  inherited Destroy;
end;

function TServerManager_ClientPool.ActivtedCount: Integer;
var
  i: Integer;
  c: TServerManager_Client;
begin
  Result := 0;
  for i := 0 to Count - 1 do
    begin
      c := Items[i];
      if (c.LinkOk) and (c.Connected) then
          inc(Result);
    end;
end;

function TServerManager_ClientPool.Count: Integer;
begin
  Result := FClientList.Count;
end;

procedure TServerManager_ClientPool.Clear;
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

procedure TServerManager_ClientPool.Progress;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Items[i].Progress;
end;

procedure TServerManager_ClientPool.AntiIdle(WorkLoad: word);
var
  i       : Integer;
  conninfo: TServerManager_ClientConnectInfo;
  c       : TServerManager_Client;
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

            if c.ConnectAndLink(conninfo.ManServAddr, conninfo.ManCliRecvPort, conninfo.ManCliSendPort) then
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
            c.AntiIdle(WorkLoad);
        inc(i);
      end;
  except
  end;
  AntiIdleIsRun := False;
end;

function TServerManager_ClientPool.BuildClientAndConnect(const RegName, ManServAddr, RegAddr: SystemString;
  const ManCliRecvPort, ManCliSendPort, RegRecvPort, RegSendPort: word; ServerType: TServerType): Boolean;
var
  c: TServerManager_Client;
  i: Integer;
begin
  Result := False;

  for i := 0 to Count - 1 do
    begin
      c := Items[i];
      if SameText(c.ConnectInfo.ManServAddr, ManServAddr) and
        (c.ConnectInfo.ManCliRecvPort = ManCliRecvPort) and
        (c.ConnectInfo.ManCliSendPort = ManCliSendPort) and
        SameText(c.ConnectInfo.RegAddr, RegAddr) and
        (c.ConnectInfo.RegRecvPort = RegRecvPort) and
        (c.ConnectInfo.RegSendPort = RegSendPort) then
        begin
          c.ReconnectTotal := 0;
          exit;
        end;
    end;

  c := TServerManager_Client.Create(Self);
  if c.ConnectAndLink(ManServAddr, ManCliRecvPort, ManCliSendPort) then
    begin
      DoStatus('call enabled api 2 send cmd:%s %s [n:%s][addr:%s][r:%d][s:%d][w:%d]', [ManServAddr, serverType2Str(ServerType), RegName, RegAddr, RegRecvPort, RegSendPort, 0]);
      Result := c.EnabledServer(RegName, ManServAddr, RegAddr, RegRecvPort, RegSendPort, ServerType);
      if Result then
        begin
          FClientList.Add(c);
          LastManagerServerAddr := ManServAddr;
          LastManServRecvPort := ManCliRecvPort;
          LastManServSendPort := ManCliSendPort;
          LastRegAddr := RegAddr;
          LastRegRecvPort := RegRecvPort;
          LastRegSendPort := RegSendPort;
        end;
    end
  else
      DisposeObject(c);
end;

constructor TServerManager_SendTunnelData.Create(AOwner: TPeerIO);
begin
  inherited Create(AOwner);
end;

destructor TServerManager_SendTunnelData.Destroy;
begin
  inherited Destroy;
end;

constructor TServerManager_RecvTunnelData.Create(AOwner: TPeerIO);
begin
  inherited Create(AOwner);
  RegName := '';
  RegAddr := '';
  RegRecvPort := 0;
  RegSendPort := 0;
  LastEnabled := GetTimeTickCount;
  WorkLoad := 0;
  ServerType := TServerType.stUnknow;
  SuccessEnabled := False;
end;

destructor TServerManager_RecvTunnelData.Destroy;
begin
  inherited Destroy;
end;

procedure TServerManager_RecvTunnelData.WriteConfig(t: TSectionTextData);
var
  m: TServerManager;
  n: SystemString;
begin
  m := DoubleTunnelService as TServerManager;
  n := MakeRegName;

  t.SetDefaultValue(n, 'Name', RegName);
  t.SetDefaultValue(n, 'ManagerServer', ManServAddr);
  t.SetDefaultValue(n, 'Host', RegAddr);
  t.SetDefaultValue(n, 'RecvPort', RegRecvPort);
  t.SetDefaultValue(n, 'SendPort', RegSendPort);
  t.SetDefaultValue(n, 'LastEnabled', LastEnabled);
  t.SetDefaultValue(n, 'WorkLoad', WorkLoad);
  t.SetDefaultValue(n, 'Type', ServerType);
end;

function TServerManager_RecvTunnelData.MakeRegName: SystemString;
begin
  Result := Format('%s_%s_%d_%d', [serverType2Str(ServerType), RegAddr, RegRecvPort, RegSendPort]);
end;

procedure TServerManager.UserLinkSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth);
begin
  UserDefineIO.Owner.Print('channel link success[r%d]<->[s%d]', [UserDefineIO.Owner.ID, UserDefineIO.SendTunnelID]);
  inherited UserLinkSuccess(UserDefineIO);
end;

procedure TServerManager.UserOut(UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth);
var
  cli: TServerManager_RecvTunnelData;
  i  : Integer;
  ns : TCoreClassStringList;
  vl : THashVariantList;
begin
  cli := UserDefineIO as TServerManager_RecvTunnelData;

  UserDefineIO.Owner.Print('%s [n:%s][addr:%s][r:%d][s:%d][w:%d] offline!', [serverType2Str(cli.ServerType),
    umlCharReplace(cli.RegName, ' ', '_').Text,
    umlCharReplace(cli.RegAddr, ' ', '_').Text,
    cli.RegRecvPort, cli.RegSendPort, cli.WorkLoad]);

  ServerConfig.Delete(cli.MakeRegName);

  with ProgressEngine.PostExecute do
    begin
      DataEng.WriteString(cli.RegAddr);
      DataEng.WriteByte(byte(cli.ServerType));
      {$IFDEF FPC}
      OnExecuteMethod := @PostExecute_ServerOffline;
      {$ELSE}
      OnExecuteMethod := PostExecute_ServerOffline;
      {$ENDIF}
    end;

  if cli.ServerType = TServerType.stManager then
    begin
      // delete local configure
      ns := TCoreClassStringList.Create;
      ServerConfig.GetSectionList(ns);

      for i := 0 to ns.Count - 1 do
        begin
          vl := ServerConfig.VariantList[ns[i]];
          if SameText(SystemString(vl.GetDefaultValue('ManagerServer', '')), cli.RegAddr) then
              ServerConfig.Delete(ns[i]);
        end;
      DisposeObject(ns);

      // sync all client
      {$IFDEF FPC}
      ProgressEngine.PostExecute(nil, @PostExecute_RegServer);
      {$ELSE}
      ProgressEngine.PostExecute(nil, PostExecute_RegServer);
      {$ENDIF}
    end;

  inherited UserOut(UserDefineIO);
end;

procedure TServerManager.PostExecute_ServerOffline(Sender: TNPostExecute);
begin
  SendTunnel.BroadcastSendDirectStreamCmd('Offline', Sender.DataEng);
end;

procedure TServerManager.PostExecute_RegServer(Sender: TNPostExecute);
var
  IDPool: TClientIDPool;
  pid   : Cardinal;
  SendDE: TDataFrameEngine;
  peer  : TPeerIO;
  c     : TServerManager_RecvTunnelData;
begin
  // fixed local connect info
  FRecvTunnel.GetClientIDPool(IDPool);
  for pid in IDPool do
    begin
      peer := RecvTunnel.ClientFromID[pid];
      if (peer <> nil) then
        begin
          c := (peer.UserDefine as TServerManager_RecvTunnelData);
          if c.SuccessEnabled then
              c.WriteConfig(ServerConfig);
        end;
    end;

  SendDE := TDataFrameEngine.Create;
  SendDE.WriteSectionText(ServerConfig);
  SendTunnel.BroadcastSendDirectStreamCmd('RegServer', SendDE);
  DisposeObject(SendDE);
end;

procedure TServerManager.Command_EnabledServer(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
var
  IDPool : TClientIDPool;
  pid    : Cardinal;
  peer   : TPeerIO;
  cli    : TServerManager_RecvTunnelData;
  SendDE : TDataFrameEngine;
  i      : Integer;
  listcli: TServerManager_RecvTunnelData;
begin
  cli := Sender.UserDefine as TServerManager_RecvTunnelData;
  if not cli.LinkOk then
    begin
      OutData.WriteBool(False);
      OutData.WriteString('nolink');
      exit;
    end;

  cli.ManServAddr := InData.Reader.ReadString;
  cli.RegName := InData.Reader.ReadString;
  cli.RegAddr := InData.Reader.ReadString;
  cli.RegRecvPort := InData.Reader.ReadWord;
  cli.RegSendPort := InData.Reader.ReadWord;
  cli.LastEnabled := GetTimeTickCount;
  cli.WorkLoad := InData.Reader.ReadWord;
  cli.ServerType := TServerType(InData.Reader.ReadByte);
  cli.SuccessEnabled := True;

  try
    FRecvTunnel.GetClientIDPool(IDPool);
    for pid in IDPool do
      begin
        peer := RecvTunnel.ClientFromID[pid];
        if (peer <> nil) then
          begin
            listcli := (peer.UserDefine as TServerManager_RecvTunnelData);
            if listcli = cli then
                continue;
            if SameText(listcli.RegAddr, cli.RegAddr) and (listcli.RegRecvPort = cli.RegRecvPort) and (listcli.RegSendPort = cli.RegSendPort)
              and (listcli.ServerType = cli.ServerType) then
              begin
                cli.SuccessEnabled := False;
                break;
              end;
            if (listcli.ServerType = cli.ServerType) and (cli.ServerType in climitationsServerType) then
              begin
                cli.SuccessEnabled := False;
                break;
              end;
          end;
      end;

  except
  end;

  if not cli.SuccessEnabled then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(Format('exists %s same server configure!!', [cli.MakeRegName]));
      {$IFDEF FPC}
      with ProgressEngine.PostExecute(InData, @PostExecute_Disconnect) do
      {$ELSE}
      with ProgressEngine.PostExecute(InData, PostExecute_Disconnect) do
        {$ENDIF}
        begin
          Data1 := Sender;
          Data2 := cli;
        end;
      exit;
    end;

  cli.WriteConfig(ServerConfig);

  SendDE := TDataFrameEngine.Create;
  SendDE.WriteSectionText(ServerConfig);
  SendTunnel.BroadcastSendDirectStreamCmd('RegServer', SendDE);
  DisposeObject(SendDE);

  OutData.WriteBool(True);
  OutData.WriteString(Format('[n:%s][addr:%s][r:%d][s:%d][w:%d] registed!', [cli.RegName, cli.RegAddr, cli.RegRecvPort, cli.RegSendPort, cli.WorkLoad]));

  Sender.Print('%s [n:%s][addr:%s][r:%d][s:%d][w:%d] registed', [serverType2Str(cli.ServerType), cli.RegName, cli.RegAddr, cli.RegRecvPort, cli.RegSendPort, cli.WorkLoad]);
end;

procedure TServerManager.PostExecute_Disconnect(Sender: TNPostExecute);
var
  c: TPeerIO;
begin
  c := Sender.Data1 as TPeerIO;
  c.Disconnect;
end;

procedure TServerManager.Command_AntiIdle(Sender: TPeerIO; InData: TDataFrameEngine);
var
  cli: TServerManager_RecvTunnelData;
begin
  cli := Sender.UserDefine as TServerManager_RecvTunnelData;

  cli.WorkLoad := InData.Reader.ReadWord;

  cli.LastEnabled := GetTimeTickCount;

  if cli.LinkOk then
      cli.WriteConfig(ServerConfig);
end;

procedure TServerManager.ServerConfigChange(Sender: TServerManager_Client; ConfigData: TSectionTextData);
var
  SendDE: TDataFrameEngine;
begin
  if ServerConfig.Same(ConfigData) then
      exit;

  ServerConfig.Merge(ConfigData);

  SendDE := TDataFrameEngine.Create;
  SendDE.WriteSectionText(ServerConfig);
  SendTunnel.BroadcastSendDirectStreamCmd('RegServer', SendDE);
  DisposeObject(SendDE);
end;

procedure TServerManager.ServerOffline(Sender: TServerManager_Client; RegAddr: SystemString; ServerType: TServerType);
var
  IDPool                 : TClientIDPool;
  pid                    : Cardinal;
  peer                   : TPeerIO;
  ns                     : TCoreClassStringList;
  i                      : Integer;
  vl                     : THashVariantList;
  c                      : TServerManager_RecvTunnelData;
  existedSameOnlineServer: Boolean;
  SendDE                 : TDataFrameEngine;
begin
  existedSameOnlineServer := False;
  FRecvTunnel.GetClientIDPool(IDPool);
  for pid in IDPool do
    begin
      peer := RecvTunnel.ClientFromID[pid];
      if (peer <> nil) then
        begin
          c := peer.UserDefine as TServerManager_RecvTunnelData;
          if SameText(c.RegAddr, RegAddr) and (c.ServerType = ServerType) then
              existedSameOnlineServer := True;
        end;
    end;

  if not existedSameOnlineServer then
    begin
      // delete local configure
      ns := TCoreClassStringList.Create;
      ServerConfig.GetSectionList(ns);

      for i := 0 to ns.Count - 1 do
        begin
          vl := ServerConfig.VariantList[ns[i]];
          if SameText(SystemString(vl.GetDefaultValue('Host', RegAddr)), RegAddr) and
            (TServerType(byte(vl.GetDefaultValue('Type', ServerType))) = ServerType) then
              ServerConfig.Delete(ns[i]);
        end;
      DisposeObject(ns);
      ServerConfig.ReBuildList;
    end;

  // sync all client
  for pid in IDPool do
    begin
      peer := RecvTunnel.ClientFromID[pid];
      if (peer <> nil) then
        begin
          c := (peer.UserDefine as TServerManager_RecvTunnelData);
          if c.SuccessEnabled then
              c.WriteConfig(ServerConfig);
        end;
    end;

  ServerConfig.ReBuildList;

  SendDE := TDataFrameEngine.Create;
  SendDE.WriteSectionText(ServerConfig);
  SendTunnel.BroadcastSendDirectStreamCmd('RegServer', SendDE);
  DisposeObject(SendDE);
end;

constructor TServerManager.Create(ARecvTunnel, ASendTunnel: TCommunicationFrameworkServer; AClientPoolDefaultClass: TCommunicationFrameworkClientClass);
begin
  inherited Create(ARecvTunnel, ASendTunnel);

  FRecvTunnel.PeerClientUserDefineClass := TServerManager_RecvTunnelData;
  FSendTunnel.PeerClientUserDefineClass := TServerManager_SendTunnelData;

  ServerConfig := TSectionTextData.Create;
  ServManClientPool := TServerManager_ClientPool.Create(AClientPoolDefaultClass, Self);

  LastTimeTick := GetTimeTick;

  SwitchAsDefaultPerformance;
end;

destructor TServerManager.Destroy;
begin
  DisposeObject(ServerConfig);
  DisposeObject(ServManClientPool);
  inherited Destroy;
end;

procedure TServerManager.RegisterCommand;
begin
  inherited RegisterCommand;
  {$IFDEF FPC}
  FRecvTunnel.RegisterStream('EnabledServer').OnExecute := @Command_EnabledServer;
  FRecvTunnel.RegisterDirectStream('AntiIdle').OnExecute := @Command_AntiIdle;
  {$ELSE}
  FRecvTunnel.RegisterStream('EnabledServer').OnExecute := Command_EnabledServer;
  FRecvTunnel.RegisterDirectStream('AntiIdle').OnExecute := Command_AntiIdle;
  {$ENDIF}
end;

procedure TServerManager.UnRegisterCommand;
begin
  inherited UnRegisterCommand;
  FRecvTunnel.DeleteRegistedCMD('EnabledServer');
  FRecvTunnel.DeleteRegistedCMD('AntiIdle');
end;

procedure TServerManager.Progress;
var
  IDPool: TClientIDPool;
  pid   : Cardinal;
  peer  : TPeerIO;
  i     : Integer;
  cli   : TServerManager_RecvTunnelData;
begin
  ServManClientPool.Progress;
  inherited Progress;

  if GetTimeTick - LastTimeTick > 5000 then
    begin
      try
        FRecvTunnel.GetClientIDPool(IDPool);
        for pid in IDPool do
          begin
            peer := RecvTunnel.ClientFromID[pid];
            if (peer <> nil) then
              begin
                cli := peer.UserDefine as TServerManager_RecvTunnelData;
                if GetTimeTickCount - cli.LastEnabled > 5 * 60000 then
                  begin
                    ServerConfig.Delete(cli.MakeRegName);
                    cli.Owner.Disconnect;
                  end;
              end;
          end;
      except
      end;

      ServManClientPool.AntiIdle(RecvTunnel.Count + SendTunnel.Count);
    end;
end;

end.
