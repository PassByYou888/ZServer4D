unit ManagerServerFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, System.TypInfo,

  CommunicationFrameworkDoubleTunnelIO_NoAuth,
  CoreClasses, TextDataEngine, ListEngine, CommunicationFramework,
  DoStatusIO, UnicodeMixedLib, DataFrameEngine,

  ManagerServer_ClientIntf, ConnectManagerServerFrm,
  CommunicationFramework_Server_ICSCustomSocket, Vcl.AppEvnts,
  CommunicationFramework_Server_CrossSocket, NotifyObjectBase,
  CommunicationFramework_Server_ICS;

type
  TManagerServerForm = class;

  TManagerServer_SendTunnelData = class(TPeerClientUserDefineForSendTunnel_NoAuth)
  public
    constructor Create(AOwner: TPeerClient); override;
    destructor Destroy; override;
  end;

  TManagerServer_RecvTunnelData = class(TPeerClientUserDefineForRecvTunnel_NoAuth)
  protected
    ManServAddr     : string;
    RegName, RegAddr: string;
    RegRecvPort     : word;
    RegSendPort     : word;
    LastEnabled     : TTimeTickValue;
    WorkLoad        : word;
    ServerType      : byte;
    SuccessEnabled  : Boolean;
  public
    constructor Create(AOwner: TPeerClient); override;
    destructor Destroy; override;

    procedure WriteConfig(t: TSectionTextData);
    function MakeRegName: string;
  end;

  TManagerServer_DoubleTunnelService = class(TCommunicationFramework_DoubleTunnelService_NoAuth)
  private
    FManagerWindow: TManagerServerForm;
  protected
    procedure UserLinkSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth); override;
    procedure UserOut(UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth); override;
    procedure PostExecute_ServerOffline(Sender: TNPostExecute);
    procedure PostExecute_RegServer(Sender: TNPostExecute);
  protected
    procedure Command_EnabledServer(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
    procedure PostExecute_Disconnect(Sender: TNPostExecute);
    procedure Command_AntiIdle(Sender: TPeerClient; InData: TDataFrameEngine);
  public
    constructor Create(ARecvTunnel, ASendTunnel: TCommunicationFrameworkServer);
    destructor Destroy; override;

    procedure RegisterCommand; override;
    procedure UnRegisterCommand; override;
  end;

  TManagerServerForm = class(TForm)
    ProgressTimer: TTimer;
    TopPanel: TPanel;
    StartServiceButton: TButton;
    StopServiceButton: TButton;
    Bevel1: TBevel;
    connectButton: TButton;
    AntiIdleCheckTimer: TTimer;
    RefreshServerListButton: TButton;
    StatusCheckBox: TCheckBox;
    PageControl: TPageControl;
    StatusTabSheet: TTabSheet;
    Memo: TMemo;
    ConnectTreeTabSheet: TTabSheet;
    TreeView: TTreeView;
    Bevel2: TBevel;
    Bevel3: TBevel;
    AppEvents: TApplicationEvents;
    OptTabSheet: TTabSheet;
    BindIPEdit: TLabeledEdit;
    procedure ProgressTimerTimer(Sender: TObject);
    procedure AntiIdleCheckTimerTimer(Sender: TObject);
    procedure StartServiceButtonClick(Sender: TObject);
    procedure StopServiceButtonClick(Sender: TObject);
    procedure connectButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure RefreshServerListButtonClick(Sender: TObject);
    procedure AppEventsException(Sender: TObject; E: Exception);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    RecvService, SendService: TCommunicationFramework_Server_CrossSocket;
    AccessService           : TCommunicationFramework_Server_CrossSocket;
    ManagerService          : TManagerServer_DoubleTunnelService;

    // game server list
    ServerConfig: TSectionTextData;

    FManagerClients: TManagerClients;

    procedure DoStatusNear(AText: string; const ID: Integer);

    function GetPathTreeNode(_Value, _Split: string; _TreeView: TTreeView; _RN: TTreeNode): TTreeNode;

    procedure Command_Query(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
    procedure ManagerClient_ServerConfigChange(Sender: TManagerClient; ConfigData: TSectionTextData);
    procedure ManagerClient_ServerOffline(Sender: TManagerClient; RegAddr: string; ServerType: byte);

    procedure PostExecute_DelayStartService(Sender: TNPostExecute);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure StartService;
    procedure StopService;
  end;

var
  ManagerServerForm: TManagerServerForm;

implementation

{$R *.dfm}


constructor TManagerServer_SendTunnelData.Create(AOwner: TPeerClient);
begin
  inherited Create(AOwner);
end;

destructor TManagerServer_SendTunnelData.Destroy;
begin
  inherited Destroy;
end;

constructor TManagerServer_RecvTunnelData.Create(AOwner: TPeerClient);
begin
  inherited Create(AOwner);
  RegName := '';
  RegAddr := '';
  RegRecvPort := 0;
  RegSendPort := 0;
  LastEnabled := GetTimeTickCount;
  WorkLoad := 0;
  ServerType := cUnknowServer;
  SuccessEnabled := False;
end;

destructor TManagerServer_RecvTunnelData.Destroy;
begin
  inherited Destroy;
end;

procedure TManagerServer_RecvTunnelData.WriteConfig(t: TSectionTextData);
var
  m: TManagerServer_DoubleTunnelService;
  n: string;
begin
  m := DoubleTunnelService as TManagerServer_DoubleTunnelService;
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

function TManagerServer_RecvTunnelData.MakeRegName: string;
begin
  Result := Format('%s_%s_%d_%d', [serverType2Str(ServerType), RegAddr, RegRecvPort, RegSendPort]);
end;

procedure TManagerServer_DoubleTunnelService.UserLinkSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth);
begin
  UserDefineIO.Owner.Print('channel link success[r%d]<->[s%d]', [UserDefineIO.Owner.ID, UserDefineIO.SendTunnelID]);
  inherited UserLinkSuccess(UserDefineIO);
end;

procedure TManagerServer_DoubleTunnelService.UserOut(UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth);
var
  cli: TManagerServer_RecvTunnelData;
  i  : Integer;
  ns : TCoreClassStringList;
  vl : THashVariantList;
begin
  cli := UserDefineIO as TManagerServer_RecvTunnelData;
  if FManagerWindow <> nil then
    begin
      UserDefineIO.Owner.Print('%s [n:%s][addr:%s][r:%d][s:%d][w:%d] offline!', [serverType2Str(cli.ServerType),
        umlCharReplace(cli.RegName, ' ', '_').Text,
        umlCharReplace(cli.RegAddr, ' ', '_').Text,
        cli.RegRecvPort, cli.RegSendPort, cli.WorkLoad]);

      FManagerWindow.ServerConfig.Delete(cli.MakeRegName);

      // 通知全网客户端，这台服务器离线

      with ProgressEngine.PostExecute do
        begin
          DataEng.WriteString(cli.RegAddr);
          DataEng.WriteString(cli.ServerType);
          OnExecuteMethod := PostExecute_ServerOffline;
        end;

      // 如果管理服务器离线，矫正本地配置表
      if cli.ServerType = cManagerServer then
        begin
          // 删除本地配置
          ns := TCoreClassStringList.Create;
          FManagerWindow.ServerConfig.GetSectionList(ns);

          for i := 0 to ns.Count - 1 do
            begin
              vl := FManagerWindow.ServerConfig.VariantList[ns[i]];
              if SameText(string(vl.GetDefaultValue('ManagerServer', '')), cli.RegAddr) then
                  FManagerWindow.ServerConfig.Delete(ns[i]);
            end;
          DisposeObject(ns);

          // 同步所有客户端
          ProgressEngine.PostExecute(nil, PostExecute_RegServer);
        end;
    end;

  inherited UserOut(UserDefineIO);
end;

procedure TManagerServer_DoubleTunnelService.PostExecute_ServerOffline(Sender: TNPostExecute);
begin
  SendTunnel.BroadcastSendDirectStreamCmd('Offline', Sender.DataEng);
end;

procedure TManagerServer_DoubleTunnelService.PostExecute_RegServer(Sender: TNPostExecute);
var
  i     : Integer;
  SendDE: TDataFrameEngine;
  c     : TManagerServer_RecvTunnelData;
begin
  // 矫正本地连接
  RecvTunnel.LockClients;
  for i := 0 to RecvTunnel.Count - 1 do
    begin
      c := (RecvTunnel[i].UserDefine as TManagerServer_RecvTunnelData);
      if c.SuccessEnabled then
          c.WriteConfig(FManagerWindow.ServerConfig);
    end;
  RecvTunnel.UnLockClients;

  SendDE := TDataFrameEngine.Create;
  SendDE.WriteSectionText(FManagerWindow.ServerConfig);
  SendTunnel.BroadcastSendDirectStreamCmd('RegServer', SendDE);
  DisposeObject(SendDE);
end;

procedure TManagerServer_DoubleTunnelService.Command_EnabledServer(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
var
  cli    : TManagerServer_RecvTunnelData;
  SendDE : TDataFrameEngine;
  i      : Integer;
  listcli: TManagerServer_RecvTunnelData;
begin
  cli := Sender.UserDefine as TManagerServer_RecvTunnelData;
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
  cli.ServerType := InData.Reader.ReadByte;
  cli.SuccessEnabled := True;

  try
    for i := 0 to RecvTunnel.Count - 1 do
      begin
        listcli := (RecvTunnel[i].UserDefine as TManagerServer_RecvTunnelData);
        if listcli = cli then
            continue;
        if SameText(listcli.RegAddr, cli.RegAddr) and (listcli.RegRecvPort = cli.RegRecvPort) and (listcli.RegSendPort = cli.RegSendPort)
          and (listcli.ServerType = cli.ServerType) then
          begin
            cli.SuccessEnabled := False;
            break;
          end;
      end;
  except
  end;

  if not cli.SuccessEnabled then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(Format('%s same server configure!!', [cli.MakeRegName]));
      with ProgressEngine.PostExecute(InData, PostExecute_Disconnect) do
        begin
          Data1 := Sender;
          Data2 := cli;
        end;
      exit;
    end;

  cli.WriteConfig(FManagerWindow.ServerConfig);

  SendDE := TDataFrameEngine.Create;
  SendDE.WriteSectionText(FManagerWindow.ServerConfig);
  SendTunnel.BroadcastSendDirectStreamCmd('RegServer', SendDE);
  DisposeObject(SendDE);

  OutData.WriteBool(True);
  OutData.WriteString(Format('[n:%s][addr:%s][r:%d][s:%d][w:%d] registed!', [cli.RegName, cli.RegAddr, cli.RegRecvPort, cli.RegSendPort, cli.WorkLoad]));

  Sender.Print('%s [n:%s][addr:%s][r:%d][s:%d][w:%d] registed', [serverType2Str(cli.ServerType), cli.RegName, cli.RegAddr, cli.RegRecvPort, cli.RegSendPort, cli.WorkLoad]);
end;

procedure TManagerServer_DoubleTunnelService.PostExecute_Disconnect(Sender: TNPostExecute);
var
  c: TPeerClient;
begin
  c := Sender.Data1 as TPeerClient;
  c.Disconnect;
end;

procedure TManagerServer_DoubleTunnelService.Command_AntiIdle(Sender: TPeerClient; InData: TDataFrameEngine);
var
  cli: TManagerServer_RecvTunnelData;
begin
  cli := Sender.UserDefine as TManagerServer_RecvTunnelData;

  cli.WorkLoad := InData.Reader.ReadWord;

  cli.LastEnabled := GetTimeTickCount;

  if cli.LinkOk and (FManagerWindow <> nil) then
      cli.WriteConfig(FManagerWindow.ServerConfig);
end;

constructor TManagerServer_DoubleTunnelService.Create(ARecvTunnel, ASendTunnel: TCommunicationFrameworkServer);
begin
  inherited Create(ARecvTunnel, ASendTunnel);
  FRecvTunnel.PeerClientUserDefineClass := TManagerServer_RecvTunnelData;
  FSendTunnel.PeerClientUserDefineClass := TManagerServer_SendTunnelData;
end;

destructor TManagerServer_DoubleTunnelService.Destroy;
begin
  inherited Destroy;
end;

procedure TManagerServer_DoubleTunnelService.RegisterCommand;
begin
  inherited RegisterCommand;
  FRecvTunnel.RegisterStream('EnabledServer').OnExecute := Command_EnabledServer;
  FRecvTunnel.RegisterDirectStream('AntiIdle').OnExecute := Command_AntiIdle;
end;

procedure TManagerServer_DoubleTunnelService.UnRegisterCommand;
begin
  inherited UnRegisterCommand;
  FRecvTunnel.DeleteRegistedCMD('EnabledServer');
  FRecvTunnel.DeleteRegistedCMD('AntiIdle');
end;

procedure TManagerServerForm.ProgressTimerTimer(Sender: TObject);
begin
  try
    ManagerService.Progress;
    AccessService.ProgressBackground;
    FManagerClients.Progress;
    ProcessICSMessages;
  except
  end;
end;

procedure TManagerServerForm.AntiIdleCheckTimerTimer(Sender: TObject);
var
  i  : Integer;
  cli: TManagerServer_RecvTunnelData;
begin
  try
    if Memo.Lines.Count > 5000 then
        Memo.Clear;
  except
  end;

  try
    for i := 0 to RecvService.Count - 1 do
      begin
        cli := RecvService[i].UserDefine as TManagerServer_RecvTunnelData;
        if GetTimeTickCount - cli.LastEnabled > 5 * 60000 then
          begin
            ServerConfig.Delete(cli.MakeRegName);
            cli.Owner.Disconnect;
          end;
      end;
  except
  end;

  try
    FManagerClients.AntiIdle(RecvService.Count + SendService.Count);
    Caption := Format('Server Manager...(total server:%d)', [ManagerService.TotalLinkCount + 1]);
  except
  end;
end;

procedure TManagerServerForm.StartServiceButtonClick(Sender: TObject);
begin
  StartService;
end;

procedure TManagerServerForm.StopServiceButtonClick(Sender: TObject);
begin
  StopService;
end;

procedure TManagerServerForm.connectButtonClick(Sender: TObject);
begin
  if ShowAndConnectManagerServer(FManagerClients, 13336, 13335, cManagerServer) then
    begin
      FManagerClients.ServerConfig.Merge(ServerConfig);
    end;
end;

procedure TManagerServerForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  StopService;
  Action := caFree;
end;

procedure TManagerServerForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  StopService;
  CanClose := True;
end;

procedure TManagerServerForm.RefreshServerListButtonClick(Sender: TObject);
var
  i : Integer;
  ns: TCoreClassStringList;
  vl: THashVariantList;

  ManServAddr     : string;
  RegName, RegAddr: string;
  RegRecvPort     : word;
  RegSendPort     : word;
  LastEnabled     : UInt64;
  WorkLoad        : word;
  ServerType      : byte;
  SuccessEnabled  : Boolean;

  vDBServer, vCoreLogicServer, vManagerServer, vPayService, vPayQueryService, vUnknowServer: byte;

  n: string;
  c: TManagerClient;

  function GetServTypStat(t: byte): Integer;
  begin
    case ServerType of
      cDBServer: Result := (vDBServer);
      cCoreLogicServer: Result := (vCoreLogicServer);
      cManagerServer: Result := (vManagerServer);
      cPayService: Result := (vPayService);
      cPayQueryService: Result := (vPayQueryService);
      else Result := vUnknowServer;
    end;
  end;

  procedure PrintServerState(prefix: string; const arry: array of TCommunicationFramework);
  var
    buff: array [TStatisticsType] of Int64;
    comm: TCommunicationFramework;
    st  : TStatisticsType;
    i   : Integer;
    v   : Int64;
  begin
    for st := low(TStatisticsType) to high(TStatisticsType) do
        buff[st] := 0;

    for comm in arry do
      begin
        for st := low(TStatisticsType) to high(TStatisticsType) do
            buff[st] := buff[st] + comm.Statistics[st];
      end;

    for i := 0 to FManagerClients.Count - 1 do
      begin
        comm := FManagerClients[i].RecvTunnel;
        for st := low(TStatisticsType) to high(TStatisticsType) do
            buff[st] := buff[st] + comm.Statistics[st];

        comm := FManagerClients[i].SendTunnel;
        for st := low(TStatisticsType) to high(TStatisticsType) do
            buff[st] := buff[st] + comm.Statistics[st];
      end;

    for st := low(TStatisticsType) to high(TStatisticsType) do
      begin
        v := buff[st];
        GetPathTreeNode(prefix + '/' + GetEnumName(TypeInfo(TStatisticsType), Ord(st)) + ' : ' + IntToStr(v), '/', TreeView, nil);
      end;
  end;

  procedure PrintServerCMDStatistics(prefix: string; const arry: array of TCommunicationFramework);
  var
    RecvLst, SendLst, ExecuteConsumeLst: THashVariantList;
    comm                               : TCommunicationFramework;
    i                                  : Integer;
    lst                                : TListString;
  begin
    RecvLst := THashVariantList.Create;
    SendLst := THashVariantList.Create;
    ExecuteConsumeLst := THashVariantList.Create;
    for comm in arry do
      begin
        RecvLst.IncValue(comm.CmdRecvStatistics);
        SendLst.IncValue(comm.CmdSendStatistics);
        ExecuteConsumeLst.SetMax(comm.CmdMaxExecuteConsumeStatistics);
      end;

    for i := 0 to FManagerClients.Count - 1 do
      begin
        comm := FManagerClients[i].RecvTunnel;
        RecvLst.IncValue(comm.CmdRecvStatistics);
        SendLst.IncValue(comm.CmdSendStatistics);
        ExecuteConsumeLst.SetMax(comm.CmdMaxExecuteConsumeStatistics);

        comm := FManagerClients[i].SendTunnel;
        RecvLst.IncValue(comm.CmdRecvStatistics);
        SendLst.IncValue(comm.CmdSendStatistics);
        ExecuteConsumeLst.SetMax(comm.CmdMaxExecuteConsumeStatistics);
      end;

    lst := TListString.Create;
    RecvLst.GetNameList(lst);
    for i := 0 to lst.Count - 1 do
        GetPathTreeNode(prefix + '/Receive/' + lst[i] + ' : ' + VarToStr(RecvLst[lst[i]]), '/', TreeView, nil);
    DisposeObject(lst);

    lst := TListString.Create;
    SendLst.GetNameList(lst);
    for i := 0 to lst.Count - 1 do
        GetPathTreeNode(prefix + '/Send/' + lst[i] + ' : ' + VarToStr(SendLst[lst[i]]), '/', TreeView, nil);
    DisposeObject(lst);

    lst := TListString.Create;
    ExecuteConsumeLst.GetNameList(lst);
    for i := 0 to lst.Count - 1 do
        GetPathTreeNode(prefix + '/CPU Consume(max)/' + lst[i] + ' : ' + VarToStr(ExecuteConsumeLst[lst[i]]) + 'ms', '/', TreeView, nil);
    DisposeObject(lst);

    DisposeObject([RecvLst, SendLst]);
  end;

begin
  ns := TCoreClassStringList.Create;
  ServerConfig.GetSectionList(ns);

  TreeView.Items.BeginUpdate;
  TreeView.Items.Clear;

  vDBServer := 0;
  vCoreLogicServer := 0;
  vManagerServer := 0;
  vPayService := 0;
  vPayQueryService := 0;
  vUnknowServer := 0;

  for i := 0 to ns.Count - 1 do
    begin
      vl := ServerConfig.VariantList[ns[i]];

      ServerType := vl.GetDefaultValue('Type', cUnknowServer);

      case ServerType of
        cDBServer: inc(vDBServer);
        cCoreLogicServer: inc(vCoreLogicServer);
        cManagerServer: inc(vManagerServer);
        cPayService: inc(vPayService);
        cPayQueryService: inc(vPayQueryService);
        else inc(vUnknowServer);
      end;
    end;

  for i := 0 to ns.Count - 1 do
    begin
      vl := ServerConfig.VariantList[ns[i]];

      try
        RegName := vl.GetDefaultValue('Name', '');
        ManServAddr := vl.GetDefaultValue('ManagerServer', '');
        RegAddr := vl.GetDefaultValue('Host', '');
        RegRecvPort := vl.GetDefaultValue('RecvPort', 0);
        RegSendPort := vl.GetDefaultValue('SendPort', 0);
        LastEnabled := vl.GetDefaultValue('LastEnabled', GetTimeTickCount);
        WorkLoad := vl.GetDefaultValue('WorkLoad', 0);
        ServerType := vl.GetDefaultValue('Type', cUnknowServer);

        n := Format('Local Manager Server Configure/%s(%d)/(%d)%s/registed name: %s', [serverType2Str(ServerType), GetServTypStat(ServerType), i, RegAddr, RegName]);
        GetPathTreeNode(n, '/', TreeView, nil);

        n := Format('Local Manager Server Configure/%s(%d)/(%d)%s/Receive Port: %d', [serverType2Str(ServerType), GetServTypStat(ServerType), i, RegAddr, RegRecvPort]);
        GetPathTreeNode(n, '/', TreeView, nil);

        n := Format('Local Manager Server Configure/%s(%d)/(%d)%s/Send Port: %d', [serverType2Str(ServerType), GetServTypStat(ServerType), i, RegAddr, RegSendPort]);
        GetPathTreeNode(n, '/', TreeView, nil);

        n := Format('Local Manager Server Configure/%s(%d)/(%d)%s/WorkLoad: %d', [serverType2Str(ServerType), GetServTypStat(ServerType), i, RegAddr, WorkLoad]);
        GetPathTreeNode(n, '/', TreeView, nil);

        n := Format('Local Manager Server Configure/%s(%d)/(%d)%s/last active %d second ago', [serverType2Str(ServerType), GetServTypStat(ServerType), i, RegAddr, Round((GetTimeTickCount - LastEnabled) / 1000)]);
        GetPathTreeNode(n, '/', TreeView, nil);
      except
      end;
    end;

  ns.Clear;
  FManagerClients.ServerConfig.GetSectionList(ns);

  vDBServer := 0;
  vCoreLogicServer := 0;
  vManagerServer := 0;
  vPayService := 0;
  vPayQueryService := 0;
  vUnknowServer := 0;

  for i := 0 to ns.Count - 1 do
    begin
      vl := FManagerClients.ServerConfig.VariantList[ns[i]];

      ServerType := vl.GetDefaultValue('Type', cUnknowServer);

      case ServerType of
        cDBServer: inc(vDBServer);
        cCoreLogicServer: inc(vCoreLogicServer);
        cManagerServer: inc(vManagerServer);
        else inc(vUnknowServer);
      end;
    end;

  for i := 0 to ns.Count - 1 do
    begin
      vl := FManagerClients.ServerConfig.VariantList[ns[i]];

      try
        RegName := vl.GetDefaultValue('Name', '');
        ManServAddr := vl.GetDefaultValue('ManagerServer', '');
        RegAddr := vl.GetDefaultValue('Host', '');
        RegRecvPort := vl.GetDefaultValue('RecvPort', 0);
        RegSendPort := vl.GetDefaultValue('SendPort', 0);
        LastEnabled := vl.GetDefaultValue('LastEnabled', GetTimeTickCount);
        WorkLoad := vl.GetDefaultValue('WorkLoad', 0);
        ServerType := vl.GetDefaultValue('Type', cUnknowServer);

        n := Format('Remote Server Configure/%s(%d)/(%d)%s/registed name: %s', [serverType2Str(ServerType), GetServTypStat(ServerType), i, RegAddr, RegName]);
        GetPathTreeNode(n, '/', TreeView, nil);

        n := Format('Remote Server Configure/%s(%d)/(%d)%s/Receive Port: %d', [serverType2Str(ServerType), GetServTypStat(ServerType), i, RegAddr, RegRecvPort]);
        GetPathTreeNode(n, '/', TreeView, nil);

        n := Format('Remote Server Configure/%s(%d)/(%d)%s/Send Port: %d', [serverType2Str(ServerType), GetServTypStat(ServerType), i, RegAddr, RegSendPort]);
        GetPathTreeNode(n, '/', TreeView, nil);

        n := Format('Remote Server Configure/%s(%d)/(%d)%s/WorkLoad: %d', [serverType2Str(ServerType), GetServTypStat(ServerType), i, RegAddr, WorkLoad]);
        GetPathTreeNode(n, '/', TreeView, nil);

        n := Format('Remote Server Configure/%s(%d)/(%d)%s/last active %d second ago', [serverType2Str(ServerType), GetServTypStat(ServerType), i, RegAddr, Round((GetTimeTickCount - LastEnabled) / 1000)]);
        GetPathTreeNode(n, '/', TreeView, nil);
      except
      end;
    end;

  for i := 0 to FManagerClients.Count - 1 do
    begin
      c := FManagerClients[i];
      try
        n := Format('connected Manager server(%d)/%d - %s/registed name: %s', [FManagerClients.Count, i + 1, c.ConnectInfo.ManServAddr, c.ConnectInfo.RegName]);
        GetPathTreeNode(n, '/', TreeView, nil);

        n := Format('connected Manager server(%d)/%d - %s/registed address: %s', [FManagerClients.Count, i + 1, c.ConnectInfo.ManServAddr, c.ConnectInfo.RegAddr]);
        GetPathTreeNode(n, '/', TreeView, nil);

        n := Format('connected Manager server(%d)/%d - %s/registed receive Port: %d', [FManagerClients.Count, i + 1, c.ConnectInfo.ManServAddr, c.ConnectInfo.RegRecvPort]);
        GetPathTreeNode(n, '/', TreeView, nil);

        n := Format('connected Manager server(%d)/%d - %s/registed send Port: %d', [FManagerClients.Count, i + 1, c.ConnectInfo.ManServAddr, c.ConnectInfo.RegSendPort]);
        GetPathTreeNode(n, '/', TreeView, nil);

        n := Format('connected Manager server(%d)/%d - %s/registed type: %s', [FManagerClients.Count, i + 1, c.ConnectInfo.ManServAddr, serverType2Str(c.ConnectInfo.ServerType)]);
        GetPathTreeNode(n, '/', TreeView, nil);

        n := Format('connected Manager server(%d)/%d - %s/connected: %s', [FManagerClients.Count, i + 1, c.ConnectInfo.ManServAddr, BoolToStr(c.Connected, True)]);
        GetPathTreeNode(n, '/', TreeView, nil);

        n := Format('connected Manager server(%d)/%d - %s/reconnect total: %d', [FManagerClients.Count, i + 1, c.ConnectInfo.ManServAddr, c.ReconnectTotal]);
        GetPathTreeNode(n, '/', TreeView, nil);
      except
      end;
    end;

  PrintServerState('Service Statistics', [RecvService, SendService, AccessService]);

  PrintServerCMDStatistics('Command Statistics', [RecvService, SendService, AccessService]);

  TreeView.Items.EndUpdate;
  DisposeObject(ns);
end;

procedure TManagerServerForm.DoStatusNear(AText: string; const ID: Integer);
begin
  if StatusCheckBox.Checked then
    begin
      Memo.Lines.Append(AText);
    end;
end;

function TManagerServerForm.GetPathTreeNode(_Value, _Split: string; _TreeView: TTreeView; _RN: TTreeNode): TTreeNode;
var
  Rep_Int : Integer;
  _Postfix: string;
begin
  _Postfix := umlGetFirstStr(_Value, _Split);
  if _Value = '' then
      Result := _RN
  else if _RN = nil then
    begin
      if _TreeView.Items.Count > 0 then
        begin
          for Rep_Int := 0 to _TreeView.Items.Count - 1 do
            begin
              if (_TreeView.Items[Rep_Int].Parent = _RN) and (umlMultipleMatch(True, _Postfix, _TreeView.Items[Rep_Int].Text)) then
                begin
                  Result := GetPathTreeNode(umlDeleteFirstStr(_Value, _Split), _Split, _TreeView, _TreeView.Items[Rep_Int]);
                  Result.Expand(False);
                  exit;
                end;
            end;
        end;
      Result := _TreeView.Items.AddChild(_RN, _Postfix);
      with Result do
        begin
          ImageIndex := -1;
          StateIndex := -1;
          SelectedIndex := -1;
          Data := nil;
        end;
      Result := GetPathTreeNode(umlDeleteFirstStr(_Value, _Split), _Split, _TreeView, Result);
    end
  else
    begin
      if (_RN.Count > 0) then
        begin
          for Rep_Int := 0 to _RN.Count - 1 do
            begin
              if (_RN.Item[Rep_Int].Parent = _RN) and (umlMultipleMatch(True, _Postfix, _RN.Item[Rep_Int].Text)) then
                begin
                  Result := GetPathTreeNode(umlDeleteFirstStr(_Value, _Split), _Split, _TreeView, _RN.Item[Rep_Int]);
                  Result.Expand(False);
                  exit;
                end;
            end;
        end;
      Result := _TreeView.Items.AddChild(_RN, _Postfix);
      with Result do
        begin
          ImageIndex := -1;
          StateIndex := -1;
          SelectedIndex := -1;
          Data := nil;
        end;
      Result := GetPathTreeNode(umlDeleteFirstStr(_Value, _Split), _Split, _TreeView, Result);
    end;
end;

procedure TManagerServerForm.AppEventsException(Sender: TObject; E: Exception);
begin
  DoStatus(E.ToString);
end;

procedure TManagerServerForm.Command_Query(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
var
  i : Integer;
  ns: TCoreClassStringList;
  vl: THashVariantList;
begin
  ns := TCoreClassStringList.Create;
  ServerConfig.GetSectionList(ns);

  for i := 0 to ns.Count - 1 do
    begin
      vl := ServerConfig.VariantList[ns[i]];
      OutData.WriteVariantList(vl);
    end;

  DisposeObject(ns);
end;

procedure TManagerServerForm.ManagerClient_ServerConfigChange(Sender: TManagerClient; ConfigData: TSectionTextData);
var
  SendDE: TDataFrameEngine;
begin
  if ServerConfig.Same(ConfigData) then
      exit;

  ServerConfig.Merge(ConfigData);

  SendDE := TDataFrameEngine.Create;
  SendDE.WriteSectionText(ServerConfig);
  SendService.BroadcastSendDirectStreamCmd('RegServer', SendDE);
  DisposeObject(SendDE);
end;

procedure TManagerServerForm.ManagerClient_ServerOffline(Sender: TManagerClient; RegAddr: string; ServerType: byte);
var
  ns                     : TCoreClassStringList;
  i                      : Integer;
  vl                     : THashVariantList;
  c                      : TManagerServer_RecvTunnelData;
  existedSameOnlineServer: Boolean;
  SendDE                 : TDataFrameEngine;
begin
  // 判断离线通知服务器是否属于已连接的客户端
  existedSameOnlineServer := False;
  RecvService.LockClients;
  for i := 0 to RecvService.Count - 1 do
    begin
      c := RecvService.Items[i].UserDefine as TManagerServer_RecvTunnelData;
      if SameText(c.RegAddr, RegAddr) and (c.ServerType = ServerType) then
          existedSameOnlineServer := True;
    end;
  RecvService.UnLockClients;

  if not existedSameOnlineServer then
    begin
      // 删除本地配置
      ns := TCoreClassStringList.Create;
      ServerConfig.GetSectionList(ns);

      for i := 0 to ns.Count - 1 do
        begin
          vl := ServerConfig.VariantList[ns[i]];
          if SameText(string(vl.GetDefaultValue('Host', RegAddr)), RegAddr) and
            (byte(vl.GetDefaultValue('Type', ServerType)) = ServerType) then
              ServerConfig.Delete(ns[i]);
        end;
      DisposeObject(ns);
      ServerConfig.ReBuildList;
    end;

  // 同步所有客户端
  // 矫正本地连接
  RecvService.LockClients;
  for i := 0 to RecvService.Count - 1 do
    begin
      c := (RecvService[i].UserDefine as TManagerServer_RecvTunnelData);
      if c.SuccessEnabled then
          c.WriteConfig(ServerConfig);
    end;
  RecvService.UnLockClients;

  ServerConfig.ReBuildList;

  SendDE := TDataFrameEngine.Create;
  SendDE.WriteSectionText(ServerConfig);
  SendService.BroadcastSendDirectStreamCmd('RegServer', SendDE);
  DisposeObject(SendDE);
end;

procedure TManagerServerForm.PostExecute_DelayStartService(Sender: TNPostExecute);
begin
  StartService;
end;

constructor TManagerServerForm.Create(AOwner: TComponent);
var
  i, pcount: Integer;
  p1, p2   : string;

  delayStartService    : Boolean;
  delayStartServiceTime: Double;
begin
  inherited Create(AOwner);
  AddDoStatusHook(Self, DoStatusNear);

  RecvService := TCommunicationFramework_Server_CrossSocket.Create;
  RecvService.PrintParams['AntiIdle'] := False;

  SendService := TCommunicationFramework_Server_CrossSocket.Create;

  AccessService := TCommunicationFramework_Server_CrossSocket.Create;
  AccessService.IdleTimeout := 5000;
  AccessService.RegisterStream('Query').OnExecute := Command_Query;

  ManagerService := TManagerServer_DoubleTunnelService.Create(RecvService, SendService);
  ManagerService.CanStatus := True;
  ManagerService.RegisterCommand;

  Memo.Lines.Add(WSAInfo);
  Memo.Lines.Add(Format('File Receive directory %s', [ManagerService.FileReceiveDirectory]));

  ServerConfig := TSectionTextData.Create;

  FManagerClients := TManagerClients.Create;
  FManagerClients.OnServerConfigChange := ManagerClient_ServerConfigChange;
  FManagerClients.OnServerOffline := ManagerClient_ServerOffline;

  delayStartService := False;
  delayStartServiceTime := 1.0;
  try
    pcount := ParamCount;
    for i := 1 to pcount do
      begin
        p1 := ParamStr(i);
        if p1 <> '' then
          begin
            if umlMultipleMatch(['NoStatus', 'NoInfo', '-NoStatus', '-NoInfo'], p1) then
              begin
                StatusCheckBox.Checked := False;
              end;

            if umlMultipleMatch(['ipv6', '-6', '-ipv6', '-v6'], p1) then
              begin
                BindIPEdit.Text := '::';
              end;

            if umlMultipleMatch(['ipv4', '-4', '-ipv4', '-v4'], p1) then
              begin
                BindIPEdit.Text := '0.0.0.0';
              end;

            if umlMultipleMatch(['ipv4+ipv6', '-4+6', '-ipv4+ipv6', '-v4+v6', 'ipv6+ipv4', '-ipv6+ipv4', '-6+4', '-v6+v4'], p1) then
              begin
                BindIPEdit.Text := '';
              end;

            if umlMultipleMatch(['DelayStart:*', 'DelayService:*',
              '-DelayStart:*', '-DelayService:*'], p1) then
              begin
                delayStartService := True;
                p2 := umlDeleteFirstStr(p1, ':');
                if umlIsNumber(p2) then
                    delayStartServiceTime := umlStrToInt(p2, 1);
              end;

            if umlMultipleMatch(['DelayStart', 'DelayService', 'AutoStart', 'AutoService',
              '-DelayStart', '-DelayService', '-AutoStart', '-AutoService'], p1) then
              begin
                delayStartService := True;
                delayStartServiceTime := 1.0;
              end;
          end;
      end;
  except
  end;

  if delayStartService then
    begin
      with ManagerService.ProgressEngine.PostExecute(delayStartServiceTime) do
          OnExecuteMethod := PostExecute_DelayStartService;
    end;

  DoStatus('');
end;

destructor TManagerServerForm.Destroy;
begin
  StopService;

  DisposeObject(RecvService);
  DisposeObject(SendService);
  DisposeObject(AccessService);
  DisposeObject(ManagerService);

  DisposeObject(ServerConfig);

  DisposeObject(FManagerClients);

  DeleteDoStatusHook(Self);
  try
      inherited Destroy;
  except
  end;
end;

procedure TManagerServerForm.StartService;
begin
  StopService;
  if AccessService.StartService(BindIPEdit.Text, 8388) then
      DoStatus('Manager Access Service ready Ok! bind:%s port:%s', [TranslateBindAddr(BindIPEdit.Text), '8388'])
  else
      MessageDlg(Format('Manager Access Service Failed! bind:%s port:%s', [TranslateBindAddr(BindIPEdit.Text), '8388']),
      mtError, [mbYes], 0);

  if RecvService.StartService(BindIPEdit.Text, 13335) then
      DoStatus('Manager Receive tunnel ready Ok! bind:%s port:%s', [TranslateBindAddr(BindIPEdit.Text), '13335'])
  else
      MessageDlg(Format('Manager Receive tunnel Failed! bind:%s port:%s', [TranslateBindAddr(BindIPEdit.Text), '13335']),
      mtError, [mbYes], 0);

  if SendService.StartService(BindIPEdit.Text, 13336) then
      DoStatus('Manager Send tunnel ready Ok! bind:%s port:%s', [TranslateBindAddr(BindIPEdit.Text), '13336'])
  else
      MessageDlg(Format('Manager Send tunnel Failed! bind:%s port:%s', [TranslateBindAddr(BindIPEdit.Text), '13336']),
      mtError, [mbYes], 0);

  // RecvService.IDCounter := 100;
  ManagerService.FManagerWindow := Self;
end;

procedure TManagerServerForm.StopService;
begin
  ManagerService.FManagerWindow := nil;
  try
    AccessService.StopService;
    RecvService.StopService;
    SendService.StopService;
    FManagerClients.Clear;
  except
  end;
end;

initialization

end.
