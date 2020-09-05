unit DataStoreServerFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  System.TypInfo,

  DoStatusIO, CoreClasses, DataFrameEngine, TextDataEngine, ListEngine,
  PascalStrings, UnicodeMixedLib,

  CommunicationFramework,
  CommunicationFrameworkIO,
  CommunicationFramework_Server_ICSCustomSocket, ConnectManagerServerFrm,
  CommunicationFrameworkDoubleTunnelIO,
  CommunicationFrameworkDoubleTunnelIO_NoAuth,
  Vcl.ComCtrls, Vcl.AppEvnts,
  CommunicationFramework_Server_CrossSocket,
  CommunicationFramework_Client_CrossSocket,
  NotifyObjectBase, CommunicationFramework_Server_ICS,
  CommunicationFrameworkDoubleTunnelIO_ServMan,
  CommunicationFrameworkDataStoreService_NoAuth, ZDBEngine, ZDBLocalManager,
  DataStoreClientIntf, CommonServiceDefine;

type
  TDataStoreServerForm = class;

  TPerUserHallSendTunnel = class(TDataStoreService_PeerClientSendTunnel_NoAuth)
  protected
  public
    constructor Create(AOwner: TPeerClient); override;
    destructor Destroy; override;
  end;

  TPerUserHallRecvTunnel = class(TDataStoreService_PeerClientRecvTunnel_NoAuth)
  protected
  public
    CurrentWorkload: Word;

    constructor Create(AOwner: TPeerClient); override;
    destructor Destroy; override;
  end;

  TDataStoreDoubleTunnelService = class(TDataStoreService_NoAuth)
  protected
    procedure UserLinkSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth); override;
    procedure UserOut(UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth); override;
  protected
    procedure Command_AntiIdle(Sender: TPeerClient; InData: TDataFrameEngine);
  public
    constructor Create(ARecvTunnel, ASendTunnel: TCommunicationFrameworkServer);
    destructor Destroy; override;

    procedure RegisterCommand; override;
    procedure UnRegisterCommand; override;
  end;

  TDataStoreServerForm = class(TForm, IServerManager_ClientPoolNotify)
    TopPanel: TPanel;
    ProgressTimer: TTimer;
    AntiIDLETimer: TTimer;
    PageControl: TPageControl;
    StatusTabSheet: TTabSheet;
    Memo: TMemo;
    ConnectTreeTabSheet: TTabSheet;
    TreeView: TTreeView;
    StartServiceButton: TButton;
    StopServiceButton: TButton;
    Bevel1: TBevel;
    connectButton: TButton;
    Bevel3: TBevel;
    RefreshServerListButton: TButton;
    Bevel2: TBevel;
    StatusCheckBox: TCheckBox;
    AppEvents: TApplicationEvents;
    OptTabSheet: TTabSheet;
    BindIPEdit: TLabeledEdit;
    RecvPortEdit: TLabeledEdit;
    SendPortEdit: TLabeledEdit;
    DBWatchTimer: TTimer;
    DBWatchTabSheet: TTabSheet;
    WatchMemo: TMemo;
    procedure StartServiceButtonClick(Sender: TObject);
    procedure StopServiceButtonClick(Sender: TObject);
    procedure connectButtonClick(Sender: TObject);
    procedure RefreshServerListButtonClick(Sender: TObject);
    procedure ProgressTimerTimer(Sender: TObject);
    procedure AntiIDLETimerTimer(Sender: TObject);
    procedure AppEventsException(Sender: TObject; E: Exception);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure DBWatchTimerTimer(Sender: TObject);
  private
    { Private declarations }
    FDBRecvTunnel    : TCommunicationFramework_Server_CrossSocket;
    FDBSendTunnel    : TCommunicationFramework_Server_CrossSocket;
    FDataStoreService: TDataStoreDoubleTunnelService;
    FManagerClients  : TServerManager_ClientPool;

    procedure DoStatusNear(AText: SystemString; const ID: Integer);
    function GetPathTreeNode(_Value, _Split: SystemString; _TreeView: TTreeView; _RN: TTreeNode): TTreeNode;

    procedure ServerConfigChange(Sender: TServerManager_Client; ConfigData: TSectionTextData);
    procedure ServerOffline(Sender: TServerManager_Client; RegAddr: SystemString; ServerType: TServerType);

    procedure PostExecute_DelayStartService(Sender: TNPostExecute);
    procedure PostExecute_DelayRegService(Sender: TNPostExecute);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure StartService;
    procedure StopService;
  end;

var
  DataStoreServerForm: TDataStoreServerForm;

implementation

{$R *.dfm}


constructor TPerUserHallSendTunnel.Create(AOwner: TPeerClient);
begin
  inherited Create(AOwner);
end;

destructor TPerUserHallSendTunnel.Destroy;
begin
  inherited Destroy;
end;

constructor TPerUserHallRecvTunnel.Create(AOwner: TPeerClient);
begin
  inherited Create(AOwner);
  CurrentWorkload := 0;
end;

destructor TPerUserHallRecvTunnel.Destroy;
begin
  inherited Destroy;
end;

procedure TDataStoreDoubleTunnelService.UserLinkSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth);
begin
  inherited UserLinkSuccess(UserDefineIO);
end;

procedure TDataStoreDoubleTunnelService.UserOut(UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth);
begin
  inherited UserOut(UserDefineIO);
end;

procedure TDataStoreDoubleTunnelService.Command_AntiIdle(Sender: TPeerClient; InData: TDataFrameEngine);
var
  cli: TPerUserHallRecvTunnel;
begin
  cli := Sender.UserDefine as TPerUserHallRecvTunnel;
  if not cli.LinkOk then
      exit;

  cli.CurrentWorkload := InData.Reader.ReadWord;
  // 最大负载
end;

constructor TDataStoreDoubleTunnelService.Create(ARecvTunnel, ASendTunnel: TCommunicationFrameworkServer);
var
  ph: SystemString;
begin
  inherited Create(ARecvTunnel, ASendTunnel);
  FRecvTunnel.PeerClientUserDefineClass := TPerUserHallRecvTunnel;
  FSendTunnel.PeerClientUserDefineClass := TPerUserHallSendTunnel;

  ph := umlCombinePath(FileReceiveDirectory, 'DB');
  umlCreateDirectory(ph);

  ZDBLocal.RootPath := ph;
  ZDBLocal.LoadDB(False);

  ZDBLocal.InitDB(cUserLoginInfoDB);
  ZDBLocal.InitDB(cUserMessageDB);
  ZDBLocal.InitDB(cLogDB);
  ZDBLocal.InitDB(cFogComputeDB);

  SwitchAsMaxPerformance;
end;

destructor TDataStoreDoubleTunnelService.Destroy;
begin
  inherited Destroy;
end;

procedure TDataStoreDoubleTunnelService.RegisterCommand;
begin
  inherited RegisterCommand;

  FRecvTunnel.RegisterDirectStream('AntiIdle').OnExecute := Command_AntiIdle;
end;

procedure TDataStoreDoubleTunnelService.UnRegisterCommand;
begin
  inherited UnRegisterCommand;

  FRecvTunnel.DeleteRegistedCMD('AntiIdle');
end;

procedure TDataStoreServerForm.StartServiceButtonClick(Sender: TObject);
begin
  StartService;
end;

procedure TDataStoreServerForm.StopServiceButtonClick(Sender: TObject);
begin
  StopService;
end;

procedure TDataStoreServerForm.connectButtonClick(Sender: TObject);
begin
  ShowAndConnectManagerServer(FManagerClients, umlStrToInt(SendPortEdit.Text, cDataStorePrimary_SendPort), umlStrToInt(RecvPortEdit.Text, cDataStorePrimary_RecvPort), TServerType.stDataStore);
end;

procedure TDataStoreServerForm.RefreshServerListButtonClick(Sender: TObject);
var
  i : Integer;
  ns: TCoreClassStringList;
  vl: THashVariantList;

  ManServAddr     : SystemString;
  RegName, RegAddr: SystemString;
  RegRecvPort     : Word;
  RegSendPort     : Word;
  LastEnabled     : UInt64;
  WorkLoad        : Word;
  ServerType      : TServerType;
  SuccessEnabled  : Boolean;

  vServerVal: array [TServerType] of Integer;

  n: SystemString;
  c: TServerManager_Client;

  function GetServTypStat(t: TServerType): Integer;
  begin
    Result := vServerVal[t];
  end;

  procedure PrintServerState(prefix: SystemString; const arry: array of TCommunicationFramework);
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

  procedure PrintServerCMDStatistics(prefix: SystemString; const arry: array of TCommunicationFramework);
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
  FManagerClients.ServerConfig.GetSectionList(ns);

  TreeView.Items.BeginUpdate;
  TreeView.Items.Clear;

  for ServerType := low(TServerType) to high(TServerType) do
      vServerVal[ServerType] := 0;

  for i := 0 to ns.Count - 1 do
    begin
      vl := FManagerClients.ServerConfig.VariantList[ns[i]];

      ServerType := vl.GetDefaultValue('Type', TServerType.stUnknow);
      inc(vServerVal[ServerType]);
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
        ServerType := vl.GetDefaultValue('Type', TServerType.stUnknow);

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

  PrintServerState('Service Statistics', [FDBRecvTunnel, FDBSendTunnel]);

  PrintServerCMDStatistics('Command Statistics', [FDBRecvTunnel, FDBSendTunnel]);

  TreeView.Items.EndUpdate;
  DisposeObject(ns);
end;

procedure TDataStoreServerForm.ProgressTimerTimer(Sender: TObject);
begin
  try
    FDataStoreService.Progress;
    FManagerClients.Progress;
    ProcessICSMessages;
  except
  end;
end;

procedure TDataStoreServerForm.AntiIDLETimerTimer(Sender: TObject);
begin
  try
    if Memo.Lines.Count > 5000 then
        Memo.Clear;

    FManagerClients.AntiIdle(FDBRecvTunnel.Count + FDBSendTunnel.Count);

    Caption := Format('Data Store Service...(activted Client:%d)', [FDataStoreService.TotalLinkCount]);
  except
  end;
end;

procedure TDataStoreServerForm.AppEventsException(Sender: TObject; E: Exception);
begin
  DoStatus(E.ToString);
end;

procedure TDataStoreServerForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  StopService;
  Action := caFree;
end;

procedure TDataStoreServerForm.DoStatusNear(AText: SystemString; const ID: Integer);
begin
  if StatusCheckBox.Checked then
    begin
      Memo.Lines.Append(AText);
    end;
end;

function TDataStoreServerForm.GetPathTreeNode(_Value, _Split: SystemString; _TreeView: TTreeView; _RN: TTreeNode): TTreeNode;
var
  Rep_Int : Integer;
  _Postfix: SystemString;
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

procedure TDataStoreServerForm.ServerConfigChange(Sender: TServerManager_Client; ConfigData: TSectionTextData);
begin
end;

procedure TDataStoreServerForm.ServerOffline(Sender: TServerManager_Client; RegAddr: SystemString; ServerType: TServerType);
begin
end;

procedure TDataStoreServerForm.PostExecute_DelayStartService(Sender: TNPostExecute);
begin
  StartService;
end;

procedure TDataStoreServerForm.PostExecute_DelayRegService(Sender: TNPostExecute);
begin
  AutoConnectManagerServer(FManagerClients,
    Sender.Data3, Sender.Data4, umlStrToInt(SendPortEdit.Text, cDataStorePrimary_SendPort), umlStrToInt(RecvPortEdit.Text, cDataStorePrimary_RecvPort), TServerType.stDataStore);
end;

constructor TDataStoreServerForm.Create(AOwner: TComponent);
var
  i, pcount: Integer;
  p1, p2   : SystemString;

  delayStartService    : Boolean;
  delayStartServiceTime: Double;

  delayReg    : Boolean;
  delayRegTime: Double;
  ManServAddr : SystemString;
  RegAddr     : SystemString;
begin
  inherited Create(AOwner);
  AddDoStatusHook(Self, DoStatusNear);

  FDBRecvTunnel := TCommunicationFramework_Server_CrossSocket.Create;
  FDBRecvTunnel.PrintParams['AntiIdle'] := False;
  FDBSendTunnel := TCommunicationFramework_Server_CrossSocket.Create;

  FDataStoreService := TDataStoreDoubleTunnelService.Create(FDBRecvTunnel, FDBSendTunnel);

  FDataStoreService.RegisterCommand;

  FManagerClients := TServerManager_ClientPool.Create(TCommunicationFramework_Client_CrossSocket, Self);

  Memo.Lines.Add(WSAInfo);
  Memo.Lines.Add(Format('File Receive directory %s', [FDataStoreService.FileReceiveDirectory]));
  Memo.Lines.Add(Format('Database directory %s', [FDataStoreService.ZDBLocal.RootPath]));

  RecvPortEdit.Text := IntToStr(cDataStorePrimary_RecvPort);
  SendPortEdit.Text := IntToStr(cDataStorePrimary_SendPort);

  delayStartService := False;
  delayStartServiceTime := 1;
  delayReg := False;
  delayRegTime := 1;
  ManServAddr := '127.0.0.1';
  RegAddr := '127.0.0.1';

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

            if umlMultipleMatch(['Recv:*', 'r:*', 'Receive:*', '-r:*', '-recv:*', '-receive:*'], p1) then
              begin
                p2 := umlDeleteFirstStr(p1, ':');
                if umlIsNumber(p2) then
                    RecvPortEdit.Text := p2;
              end;

            if umlMultipleMatch(['Send:*', 's:*', '-s:*', '-Send:*'], p1) then
              begin
                p2 := umlDeleteFirstStr(p1, ':');
                if umlIsNumber(p2) then
                    SendPortEdit.Text := p2;
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

            if umlMultipleMatch(['ManagerServer:*', 'Manager:*', 'ManServ:*', 'ManServer:*',
              '-ManagerServer:*', '-Manager:*', '-ManServ:*', '-ManServer:*'], p1) then
              begin
                ManServAddr := umlTrimSpace(umlDeleteFirstStr(p1, ':'));
              end;

            if umlMultipleMatch(['RegAddress:*', 'RegistedAddress:*', 'RegAddr:*', 'RegistedAddr:*',
              '-RegAddress:*', '-RegistedAddress:*', '-RegAddr:*', '-RegistedAddr:*'], p1) then
              begin
                RegAddr := umlTrimSpace(umlDeleteFirstStr(p1, ':'));
              end;

            if umlMultipleMatch(['DelayRegManager:*', 'DelayReg:*', 'DelayRegisted:*', 'DelayRegMan:*',
              '-DelayRegManager:*', '-DelayReg:*', '-DelayRegisted:*', '-DelayRegMan:*'], p1) then
              begin
                delayReg := True;
                p2 := umlDeleteFirstStr(p1, ':');
                if umlIsNumber(p2) then
                    delayRegTime := umlStrToInt(p2, 1);
              end;
          end;
      end;
  except
  end;

  if delayStartService then
    begin
      with FDataStoreService.ProgressEngine.PostExecute(delayStartServiceTime) do
          OnExecuteMethod := PostExecute_DelayStartService;
    end;

  if delayReg then
    begin
      with FDataStoreService.ProgressEngine.PostExecute(delayRegTime) do
        begin
          Data3 := ManServAddr;
          Data4 := RegAddr;
          OnExecuteMethod := PostExecute_DelayRegService;
        end;
    end;

  DoStatus('');
end;

procedure TDataStoreServerForm.DBWatchTimerTimer(Sender: TObject);
var
  i  : Integer;
  lst: TCoreClassListForObj;
  db : TZDBLMStore;
  pl : TZDBPipeline;
begin
  lst := TCoreClassListForObj.Create;
  FDataStoreService.ZDBLocal.GetDBList(lst);

  WatchMemo.Lines.BeginUpdate;
  WatchMemo.Lines.Clear;

  i := Round(FDataStoreService.PostCounterOfPerSec);

  WatchMemo.Lines.Add(Format('平均每秒收到 %d 条增删改操作', [i]));

  WatchMemo.Lines.Add('活跃数据库...');
  for i := 0 to lst.Count - 1 do
    begin
      db := TZDBLMStore(lst[i]);
      WatchMemo.Lines.Add(Format('库 %s 条目:%d 大小:%s 缓存 %s', [db.name, db.Count, umlSizeToStr(db.DBEngine.Size).Text, db.CacheAnnealingState]));
    end;

  lst.Clear;
  WatchMemo.Lines.Add('正在工作的查询管线...');
  FDataStoreService.ZDBLocal.GetPipeList(lst);
  for i := 0 to lst.Count - 1 do
    begin
      pl := TZDBPipeline(lst[i]);
      WatchMemo.Lines.Add(Format('管线 %s 每秒爬取%d次', [pl.PipelineName, Round(pl.QueryCounterOfPerSec)]));
    end;

  DisposeObject(lst);
  WatchMemo.Lines.EndUpdate;
end;

destructor TDataStoreServerForm.Destroy;
begin
  DisposeObject(FDBRecvTunnel);
  DisposeObject(FDBSendTunnel);
  DisposeObject(FDataStoreService);

  DisposeObject(FManagerClients);

  DeleteDoStatusHook(Self);
  inherited Destroy;
end;

procedure TDataStoreServerForm.StartService;
begin
  StopService;
  if FDBRecvTunnel.StartService(BindIPEdit.Text, umlStrToInt(RecvPortEdit.Text, cDataStorePrimary_RecvPort)) then
      DoStatus('Receive tunnel ready Ok! bind:%s port:%s', [TranslateBindAddr(BindIPEdit.Text), RecvPortEdit.Text])
  else
      MessageDlg(Format('Receive tunnel Failed! bind:%s port:%s', [TranslateBindAddr(BindIPEdit.Text), RecvPortEdit.Text]),
      mtError, [mbYes], 0);

  if FDBSendTunnel.StartService(BindIPEdit.Text, umlStrToInt(SendPortEdit.Text, cDataStorePrimary_SendPort)) then
      DoStatus('Send tunnel ready Ok! bind:%s port:%s', [TranslateBindAddr(BindIPEdit.Text), SendPortEdit.Text])
  else
      MessageDlg(Format('Send tunnel Failed! bind:%s port:%s', [TranslateBindAddr(BindIPEdit.Text), SendPortEdit.Text]),
      mtError, [mbYes], 0);

  FDBRecvTunnel.IDCounter := 110;
end;

procedure TDataStoreServerForm.StopService;
begin
  try
    FDBRecvTunnel.StopService;
    FDBSendTunnel.StopService;
    FManagerClients.Clear;
  except
  end;
end;

initialization

end.
