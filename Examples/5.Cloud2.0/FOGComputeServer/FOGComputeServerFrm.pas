unit FOGComputeServerFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, System.TypInfo, Vcl.AppEvnts,
  Vcl.ComCtrls,

  Cadencer, CommunicationFramework,
  CommunicationFramework_Client_ICS,
  CommunicationFramework_Server_ICS,
  CommunicationFramework_Server_ICSCustomSocket,
  CommunicationFrameworkDoubleTunnelIO, CommunicationFrameworkDoubleTunnelIO_NoAuth, CommunicationFrameworkIO,
  CoreClasses, DataFrameEngine, DoStatusIO, ListEngine,
  PascalStrings, UnicodeMixedLib,

  ConnectManagerServerFrm,
  TextDataEngine, NotifyObjectBase,
  MemoryStream64,
  CommunicationFramework_Server_CrossSocket,
  CommunicationFramework_Client_CrossSocket,
  CommunicationFrameworkDoubleTunnelIO_ServMan, CommonServiceDefine,
  DataStoreClientIntf;

type
  TFOGComputeService = class(TCommunicationFramework_DoubleTunnelService_NoAuth)
  protected
    procedure UserOut(UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth); virtual;
    procedure Delay_Compute(Sender: TNPostExecute);
    procedure Command_SimulateCompute5Sec(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
  public
    constructor Create(ARecvTunnel, ASendTunnel: TCommunicationFrameworkServer);
    destructor Destroy; override;

    procedure RegisterCommand; override;
    procedure UnRegisterCommand; override;
  end;

  TFOGComputeServerForm = class(TForm, IServerManager_ClientPoolNotify)
    TopPanel: TPanel;
    ProgressTimer: TTimer;
    AntiIDLETimer: TTimer;
    StartServiceButton: TButton;
    StopServiceButton: TButton;
    Bevel1: TBevel;
    connectButton: TButton;
    Bevel3: TBevel;
    RefreshServerListButton: TButton;
    Bevel2: TBevel;
    StatusCheckBox: TCheckBox;
    PageControl: TPageControl;
    StatusTabSheet: TTabSheet;
    Memo: TMemo;
    OptionsTabSheet: TTabSheet;
    RecvPortEdit: TLabeledEdit;
    SendPortEdit: TLabeledEdit;
    BindIPEdit: TLabeledEdit;
    ConnectTreeTabSheet: TTabSheet;
    TreeView: TTreeView;
    procedure StartServiceButtonClick(Sender: TObject);
    procedure StopServiceButtonClick(Sender: TObject);
    procedure connectButtonClick(Sender: TObject);
    procedure RefreshServerListButtonClick(Sender: TObject);
    procedure ProgressTimerTimer(Sender: TObject);
    procedure AntiIDLETimerTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FFOGComputeRecvTunnel: TCommunicationFramework_Server_CrossSocket;
    FFOGComputeSendTunnel: TCommunicationFramework_Server_CrossSocket;
    FFOGComputeService   : TFOGComputeService;
    FManagerClients      : TServerManager_ClientPool;
    FDataStoreClient     : TDataStore_DoubleTunnelClient;

    procedure DoStatusNear(AText: string; const ID: Integer);
    function GetPathTreeNode(_Value, _Split: string; _TreeView: TTreeView; _RN: TTreeNode): TTreeNode;

    procedure ServerConfigChange(Sender: TServerManager_Client; ConfigData: TSectionTextData);
    procedure ServerOffline(Sender: TServerManager_Client; RegAddr: string; ServerType: TServerType);
  protected
    procedure PostExecute_DelayStartService(Sender: TNPostExecute);
    procedure PostExecute_DelayRegService(Sender: TNPostExecute);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure StartService;
    procedure StopService;
  end;

var
  FOGComputeServerForm: TFOGComputeServerForm;

implementation

{$R *.dfm}


uses zExpression, TextParsing;

procedure TFOGComputeService.UserOut(UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth);
begin
  DoStatus('%s 用户已离线', [UserDefineIO.Owner.GetPeerIP]);
  inherited UserOut(UserDefineIO);
end;

procedure TFOGComputeService.Delay_Compute(Sender: TNPostExecute);
var
  v: Variant;
begin
  // 1.5秒后，客户端离线状态检查
  if RecvTunnel.Exists(Sender.Data1) then
    begin
      // 客户端未离线时，使用句法引擎计算一次表达式结果
      v := EvaluateExpressionValue(Sender.DataEng.ReadString(0));
      // 往延迟数据结构写入反馈值，相当于OutData
      if VarIsNull(v) then
          TPeerClient(Sender.Data1).OutDataFrame.WriteString('表达式无效')
      else
        begin
          TPeerClient(Sender.Data1).OutDataFrame.WriteString(VarToStr(v));
          FOGComputeServerForm.FDataStoreClient.PostFogComputeInfo('匿名用户', '匿名用户', Sender.DataEng.ReadString(0), VarToStr(v));
        end;
      // 立即将计算结果反馈给客户端
      TPeerClient(Sender.Data1).ContinueResultSend;
    end;
end;

procedure TFOGComputeService.Command_SimulateCompute5Sec(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
begin
  // 雾计算模拟

  // 暂停反馈
  Sender.PauseResultSend;

  // 延迟引擎，抛送一个1.5秒后执行的时间
  // 延迟抛送是模拟后台线程，或则跨服务计算，待计算完成，再继续反馈结果
  ProgressEngine.PostExecuteM(1.5, InData, Delay_Compute).Data1 := Sender;
end;

constructor TFOGComputeService.Create(ARecvTunnel, ASendTunnel: TCommunicationFrameworkServer);
begin
  inherited Create(ARecvTunnel, ASendTunnel);

  SwitchAsMaxSecurity;
end;

destructor TFOGComputeService.Destroy;
begin
  inherited Destroy;
end;

procedure TFOGComputeService.RegisterCommand;
begin
  inherited RegisterCommand;
  FRecvTunnel.RegisterStream('SimulateCompute5Sec').OnExecute := Command_SimulateCompute5Sec;
end;

procedure TFOGComputeService.UnRegisterCommand;
begin
  inherited UnRegisterCommand;
  FRecvTunnel.DeleteRegistedCMD('SimulateCompute5Sec');
end;

procedure TFOGComputeServerForm.StartServiceButtonClick(Sender: TObject);
begin
  StartService;
end;

procedure TFOGComputeServerForm.StopServiceButtonClick(Sender: TObject);
begin
  StopService;
end;

procedure TFOGComputeServerForm.connectButtonClick(Sender: TObject);
begin
  ShowAndConnectManagerServer(FManagerClients, umlStrToInt(SendPortEdit.Text, cFOGCompute_SendPort), umlStrToInt(RecvPortEdit.Text, cFOGCompute_RecvPort), TServerType.stFOGCompute);
end;

procedure TFOGComputeServerForm.RefreshServerListButtonClick(Sender: TObject);
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

  PrintServerState('Service Statistics', [FFOGComputeRecvTunnel, FFOGComputeSendTunnel]);

  PrintServerCMDStatistics('Command Statistics', [FFOGComputeRecvTunnel, FFOGComputeSendTunnel]);

  TreeView.Items.EndUpdate;
  disposeObject(ns);
end;

procedure TFOGComputeServerForm.ProgressTimerTimer(Sender: TObject);
begin
  try
    FFOGComputeService.Progress;
    FManagerClients.Progress;
    FDataStoreClient.Progress;
    ProcessICSMessages;
  except
  end;
end;

procedure TFOGComputeServerForm.AntiIDLETimerTimer(Sender: TObject);
begin
  try
    if Memo.Lines.Count > 5000 then
        Memo.Clear;

    FManagerClients.AntiIdle(FFOGComputeService.TotalLinkCount);
    FDataStoreClient.AntiIdle(FFOGComputeService.TotalLinkCount);
    Caption := Format('FOG Compute Service...(Online:%d)', [FFOGComputeService.TotalLinkCount]);
  except
  end;
end;

procedure TFOGComputeServerForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  StopService;
  Action := caFree;
end;

procedure TFOGComputeServerForm.DoStatusNear(AText: string; const ID: Integer);
begin
  if StatusCheckBox.Checked then
    begin
      Memo.Lines.Append(AText);
    end;
  FDataStoreClient.PostLogInfo('FogCompute', AText);
end;

function TFOGComputeServerForm.GetPathTreeNode(_Value, _Split: string; _TreeView: TTreeView; _RN: TTreeNode): TTreeNode;
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

procedure TFOGComputeServerForm.ServerOffline(Sender: TServerManager_Client; RegAddr: string; ServerType: TServerType);
begin
end;

procedure TFOGComputeServerForm.ServerConfigChange(Sender: TServerManager_Client; ConfigData: TSectionTextData);
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

procedure TFOGComputeServerForm.PostExecute_DelayStartService(Sender: TNPostExecute);
begin
  StartService;
end;

procedure TFOGComputeServerForm.PostExecute_DelayRegService(Sender: TNPostExecute);
begin
  AutoConnectManagerServer(FManagerClients,
    Sender.Data3, Sender.Data4, umlStrToInt(SendPortEdit.Text, cFOGCompute_SendPort), umlStrToInt(RecvPortEdit.Text, cFOGCompute_RecvPort), TServerType.stFOGCompute);
end;

constructor TFOGComputeServerForm.Create(AOwner: TComponent);
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

  FFOGComputeRecvTunnel := TCommunicationFramework_Server_CrossSocket.Create;
  FFOGComputeRecvTunnel.PrintParams['AntiIdle'] := False;
  FFOGComputeSendTunnel := TCommunicationFramework_Server_CrossSocket.Create;

  FFOGComputeService := TFOGComputeService.Create(FFOGComputeRecvTunnel, FFOGComputeSendTunnel);

  FFOGComputeService.RegisterCommand;

  FManagerClients := TServerManager_ClientPool.Create(TCommunicationFramework_Client_CrossSocket, Self);

  FDataStoreClient := TDataStore_DoubleTunnelClient.Create(TCommunicationFramework_Client_CrossSocket);
  FDataStoreClient.RegisterCommand;

  Memo.Lines.Add(WSAInfo);

  RecvPortEdit.Text := IntToStr(cFOGCompute_RecvPort);
  SendPortEdit.Text := IntToStr(cFOGCompute_SendPort);

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
      with FFOGComputeService.ProgressEngine.PostExecute(delayStartServiceTime) do
          OnExecuteMethod := PostExecute_DelayStartService;
    end;

  if delayReg then
    begin
      with FFOGComputeService.ProgressEngine.PostExecute(delayRegTime) do
        begin
          Data3 := ManServAddr;
          Data4 := RegAddr;
          OnExecuteMethod := PostExecute_DelayRegService;
        end;
    end;

  DoStatus('');
end;

destructor TFOGComputeServerForm.Destroy;
begin
  disposeObject(FManagerClients);
  disposeObject(FDataStoreClient);

  DeleteDoStatusHook(Self);
  try
      inherited Destroy;
  except
  end;
end;

procedure TFOGComputeServerForm.StartService;
begin
  StopService;
  if FFOGComputeRecvTunnel.StartService(BindIPEdit.Text, umlStrToInt(RecvPortEdit.Text, cFOGCompute_RecvPort)) then
      DoStatus('Receive tunnel ready Ok! bind:%s port:%s', [TranslateBindAddr(BindIPEdit.Text), RecvPortEdit.Text])
  else
      MessageDlg(Format('Receive tunnel Failed! bind:%s port:%s', [TranslateBindAddr(BindIPEdit.Text), RecvPortEdit.Text]),
      mtError, [mbYes], 0);

  if FFOGComputeSendTunnel.StartService(BindIPEdit.Text, umlStrToInt(SendPortEdit.Text, cFOGCompute_SendPort)) then
      DoStatus('Send tunnel ready Ok! bind:%s port:%s', [TranslateBindAddr(BindIPEdit.Text), SendPortEdit.Text])
  else
      MessageDlg(Format('Send tunnel Failed! bind:%s port:%s', [TranslateBindAddr(BindIPEdit.Text), SendPortEdit.Text]),
      mtError, [mbYes], 0);

  FFOGComputeRecvTunnel.IDCounter := 110;
end;

procedure TFOGComputeServerForm.StopService;
begin
  try
    FFOGComputeRecvTunnel.StopService;
    FFOGComputeSendTunnel.StopService;
    FManagerClients.Clear;
    FDataStoreClient.Disconnect;
  except
  end;
end;

initialization

end.
