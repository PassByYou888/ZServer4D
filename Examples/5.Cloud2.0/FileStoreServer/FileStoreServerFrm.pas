unit FileStoreServerFrm;

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
  CommunicationFrameworkDoubleTunnelIO_ServMan, FileStoreClientIntf,
  CommonServiceDefine, DataStoreClientIntf;

type
  TPerUserHallSendTunnel = class(TPeerClientUserDefineForSendTunnel_NoAuth)
  protected
  public
    constructor Create(AOwner: TPeerClient); override;
    destructor Destroy; override;
  end;

  TPerUserHallRecvTunnel = class(TPeerClientUserDefineForRecvTunnel_NoAuth)
  protected
  public
    Registed          : Boolean;
    HallServerAddr    : string;
    HallServerRecvPort: Word;
    HallServerSendPort: Word;
    HallWorkload      : Word;

    constructor Create(AOwner: TPeerClient); override;
    destructor Destroy; override;
  end;

  TFileStoreDoubleTunnelService = class(TCommunicationFramework_DoubleTunnelService_NoAuth)
  private
    UserCheckStates: TSectionTextData;
  protected
    procedure UserLinkSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth); override;
    procedure UserOut(UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth); override;
  protected
    procedure Command_RegHallServer(Sender: TPeerClient; InData, OutData: TDataFrameEngine);

    procedure Command_UserIsLock(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
    procedure Command_UserLock(Sender: TPeerClient; InData: TDataFrameEngine);
    procedure Command_UserUnLock(Sender: TPeerClient; InData: TDataFrameEngine);

    procedure Command_AntiIdle(Sender: TPeerClient; InData: TDataFrameEngine);
  public
    constructor Create(ARecvTunnel, ASendTunnel: TCommunicationFrameworkServer);
    destructor Destroy; override;

    procedure RegisterCommand; override;
    procedure UnRegisterCommand; override;
  end;

  TFileStoreServiceForm = class(TForm, IServerManager_ClientPoolNotify)
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
    procedure StartServiceButtonClick(Sender: TObject);
    procedure StopServiceButtonClick(Sender: TObject);
    procedure connectButtonClick(Sender: TObject);
    procedure RefreshServerListButtonClick(Sender: TObject);
    procedure ProgressTimerTimer(Sender: TObject);
    procedure AntiIDLETimerTimer(Sender: TObject);
    procedure AppEventsException(Sender: TObject; E: Exception);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    FDBRecvTunnel    : TCommunicationFramework_Server_CrossSocket;
    FDBSendTunnel    : TCommunicationFramework_Server_CrossSocket;
    FFileStoreService: TFileStoreDoubleTunnelService;
    FManagerClients  : TServerManager_ClientPool;
    FDataStoreClient : TDataStore_DoubleTunnelClient;

    procedure DoStatusNear(AText: string; const ID: Integer);
    function GetPathTreeNode(_Value, _Split: string; _TreeView: TTreeView; _RN: TTreeNode): TTreeNode;

    procedure ServerConfigChange(Sender: TServerManager_Client; ConfigData: TSectionTextData);
    procedure ServerOffline(Sender: TServerManager_Client; RegAddr: string; ServerType: TServerType);

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
  FileStoreServiceForm: TFileStoreServiceForm;

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
  Registed := False;
  HallServerAddr := '';
  HallServerRecvPort := 0;
  HallServerSendPort := 0;
  HallWorkload := 0;
end;

destructor TPerUserHallRecvTunnel.Destroy;
begin
  inherited Destroy;
end;

procedure TFileStoreDoubleTunnelService.UserLinkSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth);
begin
  inherited UserLinkSuccess(UserDefineIO);
end;

procedure TFileStoreDoubleTunnelService.UserOut(UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth);
begin
  inherited UserOut(UserDefineIO);
end;

procedure TFileStoreDoubleTunnelService.Command_RegHallServer(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
var
  cli: TPerUserHallRecvTunnel;
begin
  cli := Sender.UserDefine as TPerUserHallRecvTunnel;
  if not cli.LinkOk then
    begin
      OutData.WriteBool(True);
      OutData.WriteString(Format('no link!', []));
      exit;
    end;

  cli.Registed := True;
  cli.HallServerAddr := InData.Reader.ReadString;
  cli.HallServerRecvPort := InData.Reader.ReadWord;
  cli.HallServerSendPort := InData.Reader.ReadWord;
  Sender.Print('reg Hall server:%s recv:%d send:%d', [cli.HallServerAddr, cli.HallServerRecvPort, cli.HallServerSendPort]);

  OutData.WriteBool(True);
  OutData.WriteString(Format('reg Hall server:%s recv:%d send:%d Successed!', [cli.HallServerAddr, cli.HallServerRecvPort, cli.HallServerSendPort]));
end;

procedure TFileStoreDoubleTunnelService.Command_UserIsLock(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
var
  UserID: string;
  locked: Boolean;
begin
  UserID := InData.Reader.ReadString;
  try
    locked := UserCheckStates.GetDefaultValue(UserID, 'Lock', False);
    OutData.WriteBool(locked);
  except
    UserCheckStates.SetDefaultValue(UserID, 'Lock', False);
    OutData.WriteBool(False);
  end;
end;

procedure TFileStoreDoubleTunnelService.Command_UserLock(Sender: TPeerClient; InData: TDataFrameEngine);
var
  UserID: string;
begin
  UserID := InData.Reader.ReadString;

  if UserCheckStates.GetDefaultValue(UserID, 'Lock', False) = False then
    begin
      UserCheckStates.SetDefaultValue(UserID, 'Lock', True);
      Sender.Print('lock %s success', [UserID]);
    end
  else
    begin
      Sender.Print('lock %s failed', [UserID]);
    end;
end;

procedure TFileStoreDoubleTunnelService.Command_UserUnLock(Sender: TPeerClient; InData: TDataFrameEngine);
var
  UserID: string;
begin
  UserID := InData.Reader.ReadString;

  if UserCheckStates.GetDefaultValue(UserID, 'Lock', True) = True then
    begin
      UserCheckStates.SetDefaultValue(UserID, 'Lock', False);
      Sender.Print('unlock %s success', [UserID]);
    end
  else
    begin
      Sender.Print('unlock %s failed', [UserID]);
    end;
end;

procedure TFileStoreDoubleTunnelService.Command_AntiIdle(Sender: TPeerClient; InData: TDataFrameEngine);
var
  cli: TPerUserHallRecvTunnel;
begin
  cli := Sender.UserDefine as TPerUserHallRecvTunnel;
  if cli.LinkOk then
    begin
      cli.HallWorkload := InData.Reader.ReadWord;
      // ×î´ó¸ºÔØ
    end;
end;

constructor TFileStoreDoubleTunnelService.Create(ARecvTunnel, ASendTunnel: TCommunicationFrameworkServer);
begin
  inherited Create(ARecvTunnel, ASendTunnel);
  FRecvTunnel.PeerClientUserDefineClass := TPerUserHallRecvTunnel;
  FSendTunnel.PeerClientUserDefineClass := TPerUserHallSendTunnel;
  UserCheckStates := TSectionTextData.Create(1024);

  SwitchAsMaxPerformance;
end;

destructor TFileStoreDoubleTunnelService.Destroy;
begin
  disposeObject(UserCheckStates);
  inherited Destroy;
end;

procedure TFileStoreDoubleTunnelService.RegisterCommand;
begin
  inherited RegisterCommand;
  FRecvTunnel.RegisterStream('RegHallServer').OnExecute := Command_RegHallServer;

  FRecvTunnel.RegisterStream('UserIsLock').OnExecute := Command_UserIsLock;
  FRecvTunnel.RegisterDirectStream('UserLock').OnExecute := Command_UserLock;
  FRecvTunnel.RegisterDirectStream('UserUnLock').OnExecute := Command_UserUnLock;

  FRecvTunnel.RegisterDirectStream('AntiIdle').OnExecute := Command_AntiIdle;
end;

procedure TFileStoreDoubleTunnelService.UnRegisterCommand;
begin
  inherited UnRegisterCommand;
  FRecvTunnel.DeleteRegistedCMD('RegHallServer');

  FRecvTunnel.DeleteRegistedCMD('UserIsLock');
  FRecvTunnel.DeleteRegistedCMD('UserLock');
  FRecvTunnel.DeleteRegistedCMD('UserUnLock');

  FRecvTunnel.DeleteRegistedCMD('AntiIdle');
end;

procedure TFileStoreServiceForm.StartServiceButtonClick(Sender: TObject);
begin
  StartService;
end;

procedure TFileStoreServiceForm.StopServiceButtonClick(Sender: TObject);
begin
  StopService;
end;

procedure TFileStoreServiceForm.AppEventsException(Sender: TObject; E: Exception);
begin
  DoStatus(E.ToString);
end;

procedure TFileStoreServiceForm.connectButtonClick(Sender: TObject);
begin
  ShowAndConnectManagerServer(FManagerClients, umlStrToInt(SendPortEdit.Text, cFileStore_SendPort), umlStrToInt(RecvPortEdit.Text, cFileStore_RecvPort), TServerType.stFileStore);
end;

procedure TFileStoreServiceForm.RefreshServerListButtonClick(Sender: TObject);
var
  i : Integer;
  ns: TCoreClassStringList;
  vl: THashVariantList;

  ManServAddr     : string;
  RegName, RegAddr: string;
  RegRecvPort     : Word;
  RegSendPort     : Word;
  LastEnabled     : UInt64;
  WorkLoad        : Word;
  ServerType      : TServerType;
  SuccessEnabled  : Boolean;

  vServerVal: array [TServerType] of Integer;

  n: string;
  c: TServerManager_Client;

  function GetServTypStat(t: TServerType): Integer;
  begin
    Result := vServerVal[t];
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
    disposeObject(lst);

    lst := TListString.Create;
    SendLst.GetNameList(lst);
    for i := 0 to lst.Count - 1 do
        GetPathTreeNode(prefix + '/Send/' + lst[i] + ' : ' + VarToStr(SendLst[lst[i]]), '/', TreeView, nil);
    disposeObject(lst);

    lst := TListString.Create;
    ExecuteConsumeLst.GetNameList(lst);
    for i := 0 to lst.Count - 1 do
        GetPathTreeNode(prefix + '/CPU Consume(max)/' + lst[i] + ' : ' + VarToStr(ExecuteConsumeLst[lst[i]]) + 'ms', '/', TreeView, nil);
    disposeObject(lst);

    disposeObject([RecvLst, SendLst]);
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
  disposeObject(ns);
end;

procedure TFileStoreServiceForm.ProgressTimerTimer(Sender: TObject);
begin
  try
    FFileStoreService.Progress;
    FManagerClients.Progress;
    FDataStoreClient.Progress;
    ProcessICSMessages;
  except
  end;
end;

procedure TFileStoreServiceForm.AntiIDLETimerTimer(Sender: TObject);
begin
  try
    if Memo.Lines.Count > 5000 then
        Memo.Clear;

    FManagerClients.AntiIdle(FDBRecvTunnel.Count + FDBSendTunnel.Count);
    FDataStoreClient.AntiIdle(FDBRecvTunnel.Count + FDBSendTunnel.Count);

    Caption := Format('FileStore Service...(activted hall:%d)', [FFileStoreService.TotalLinkCount]);
  except
  end;
end;

procedure TFileStoreServiceForm.DoStatusNear(AText: string; const ID: Integer);
begin
  if StatusCheckBox.Checked then
    begin
      Memo.Lines.Append(AText);
    end;
  FDataStoreClient.PostLogInfo('FileStore', AText);
end;

procedure TFileStoreServiceForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  StopService;
  Action := caFree;
end;

function TFileStoreServiceForm.GetPathTreeNode(_Value, _Split: string; _TreeView: TTreeView; _RN: TTreeNode): TTreeNode;
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

procedure TFileStoreServiceForm.ServerConfigChange(Sender: TServerManager_Client; ConfigData: TSectionTextData);
var
  ns: TCoreClassStringList;
  i : Integer;
  vl: THashVariantList;

  DBServAddr, RegAddr                                   : string;
  DBCliRecvPort, DBCliSendPort, RegRecvPort, RegSendPort: Word;

  DataStoreServAddr                         : string;
  DataStoreCliRecvPort, DataStoreCliSendPort: Word;
begin
  if FManagerClients.Count = 0 then
      exit;

  ns := TCoreClassStringList.Create;
  ConfigData.GetSectionList(ns);

  for i := 0 to ns.Count - 1 do
    begin
      vl := ConfigData.VariantList[ns[i]];

      if not FDataStoreClient.Connected then
        if vl.GetDefaultValue('Type', TServerType.stUnknow) = TServerType.stDataStore then
          begin
            DataStoreServAddr := vl.GetDefaultValue('Host', '');
            DataStoreCliSendPort := vl.GetDefaultValue('SendPort', cFileStore_SendPort);
            DataStoreCliRecvPort := vl.GetDefaultValue('RecvPort', cFileStore_RecvPort);

            FDataStoreClient.Connect(DataStoreServAddr, DataStoreCliRecvPort, DataStoreCliSendPort);
          end;
    end;
  disposeObject(ns);
end;

procedure TFileStoreServiceForm.ServerOffline(Sender: TServerManager_Client; RegAddr: string; ServerType: TServerType);
begin
end;

procedure TFileStoreServiceForm.PostExecute_DelayStartService(Sender: TNPostExecute);
begin
  StartService;
end;

procedure TFileStoreServiceForm.PostExecute_DelayRegService(Sender: TNPostExecute);
begin
  AutoConnectManagerServer(FManagerClients,
    Sender.Data3, Sender.Data4, umlStrToInt(SendPortEdit.Text, cFileStore_SendPort), umlStrToInt(RecvPortEdit.Text, cFileStore_RecvPort), TServerType.stFileStore);
end;

constructor TFileStoreServiceForm.Create(AOwner: TComponent);
var
  i, pcount: Integer;
  p1, p2   : string;

  delayStartService    : Boolean;
  delayStartServiceTime: Double;

  delayReg    : Boolean;
  delayRegTime: Double;
  ManServAddr : string;
  RegAddr     : string;
begin
  inherited Create(AOwner);
  AddDoStatusHook(Self, DoStatusNear);

  FDBRecvTunnel := TCommunicationFramework_Server_CrossSocket.Create;
  FDBRecvTunnel.PrintParams['AntiIdle'] := False;
  FDBSendTunnel := TCommunicationFramework_Server_CrossSocket.Create;

  FFileStoreService := TFileStoreDoubleTunnelService.Create(FDBRecvTunnel, FDBSendTunnel);

  FFileStoreService.RegisterCommand;

  FManagerClients := TServerManager_ClientPool.Create(TCommunicationFramework_Client_CrossSocket, Self);

  FDataStoreClient := TDataStore_DoubleTunnelClient.Create(TCommunicationFramework_Client_CrossSocket);
  FDataStoreClient.RegisterCommand;

  Memo.Lines.Add(WSAInfo);
  Memo.Lines.Add(Format('File Receive directory %s', [FFileStoreService.FileReceiveDirectory]));

  RecvPortEdit.Text := IntToStr(cFileStore_RecvPort);
  SendPortEdit.Text := IntToStr(cFileStore_SendPort);

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
      with FFileStoreService.ProgressEngine.PostExecute(delayStartServiceTime) do
          OnExecuteMethod := PostExecute_DelayStartService;
    end;

  if delayReg then
    begin
      with FFileStoreService.ProgressEngine.PostExecute(delayRegTime) do
        begin
          Data3 := ManServAddr;
          Data4 := RegAddr;
          OnExecuteMethod := PostExecute_DelayRegService;
        end;
    end;

  DoStatus('');
end;

destructor TFileStoreServiceForm.Destroy;
begin
  disposeObject(FDBRecvTunnel);
  disposeObject(FDBSendTunnel);
  disposeObject(FFileStoreService);

  disposeObject(FManagerClients);
  disposeObject(FDataStoreClient);

  DeleteDoStatusHook(Self);
  inherited Destroy;
end;

procedure TFileStoreServiceForm.StartService;
begin
  StopService;
  if FDBRecvTunnel.StartService(BindIPEdit.Text, umlStrToInt(RecvPortEdit.Text, cFileStore_RecvPort)) then
      DoStatus('Receive tunnel ready Ok! bind:%s port:%s', [TranslateBindAddr(BindIPEdit.Text), RecvPortEdit.Text])
  else
      MessageDlg(Format('Receive tunnel Failed! bind:%s port:%s', [TranslateBindAddr(BindIPEdit.Text), RecvPortEdit.Text]),
      mtError, [mbYes], 0);

  if FDBSendTunnel.StartService(BindIPEdit.Text, umlStrToInt(SendPortEdit.Text, cFileStore_SendPort)) then
      DoStatus('Send tunnel ready Ok! bind:%s port:%s', [TranslateBindAddr(BindIPEdit.Text), SendPortEdit.Text])
  else
      MessageDlg(Format('Send tunnel Failed! bind:%s port:%s', [TranslateBindAddr(BindIPEdit.Text), SendPortEdit.Text]),
      mtError, [mbYes], 0);

  FDBRecvTunnel.IDCounter := 110;
end;

procedure TFileStoreServiceForm.StopService;
begin
  try
    FDBRecvTunnel.StopService;
    FDBSendTunnel.StopService;
    FManagerClients.Clear;
    FDataStoreClient.Disconnect;
  except
  end;
end;

initialization

end.
