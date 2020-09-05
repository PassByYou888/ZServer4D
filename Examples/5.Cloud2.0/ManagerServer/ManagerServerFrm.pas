unit ManagerServerFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,
  System.TypInfo,

  CommunicationFrameworkDoubleTunnelIO_NoAuth,
  CoreClasses, TextDataEngine, ListEngine, CommunicationFramework,
  DoStatusIO, UnicodeMixedLib, DataFrameEngine,

  ConnectManagerServerFrm,
  CommunicationFramework_Server_ICSCustomSocket, Vcl.AppEvnts,
  CommunicationFramework_Server_CrossSocket, NotifyObjectBase,
  CommunicationFramework_Client_CrossSocket,
  CommunicationFramework_Server_ICS, CoreCipher,
  CommunicationFrameworkDoubleTunnelIO_ServMan, PascalStrings,
  CommonServiceDefine;

type
  TManagerServerForm = class(TForm)
    ProgressTimer: TTimer;
    TopPanel: TPanel;
    StartServiceButton: TButton;
    StopServiceButton: TButton;
    Bevel1: TBevel;
    connectButton: TButton;
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
    RecvPortEdit: TLabeledEdit;
    SendPortEdit: TLabeledEdit;
    QueryPortEdit: TLabeledEdit;
    procedure ProgressTimerTimer(Sender: TObject);
    procedure StartServiceButtonClick(Sender: TObject);
    procedure StopServiceButtonClick(Sender: TObject);
    procedure connectButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure AppEventsException(Sender: TObject; E: Exception);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure RefreshServerListButtonClick(Sender: TObject);
  private
    { Private declarations }
    RecvService, SendService: TCommunicationFramework_Server_CrossSocket;
    AccessService           : TCommunicationFramework_Server_CrossSocket;
    ManagerService          : TServerManager;

    procedure DoStatusNear(AText: SystemString; const ID: Integer);

    function GetPathTreeNode(_Value, _Split: SystemString; _TreeView: TTreeView; _RN: TTreeNode): TTreeNode;

    procedure Command_Query(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
    procedure Command_QueryMinLoad(Sender: TPeerClient; InData, OutData: TDataFrameEngine);

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
  ManagerServerForm: TManagerServerForm;

implementation

{$R *.dfm}


procedure TManagerServerForm.ProgressTimerTimer(Sender: TObject);
begin
  try
    ManagerService.Progress;
    AccessService.Progress;
    ProcessICSMessages;

    try
      if Memo.Lines.Count > 5000 then
          Memo.Clear;
    except
    end;

    Caption := Format('Server Manager...(total server:%d)', [ManagerService.TotalLinkCount + 1]);
  except
  end;
end;

procedure TManagerServerForm.RefreshServerListButtonClick(Sender: TObject);
var
  i : Integer;
  ns: TCoreClassStringList;
  vl: THashVariantList;

  ManServAddr     : SystemString;
  RegName, RegAddr: SystemString;
  RegRecvPort     : word;
  RegSendPort     : word;
  LastEnabled     : UInt64;
  WorkLoad        : word;
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

    for i := 0 to ManagerService.ServManClientPool.Count - 1 do
      begin
        comm := ManagerService.ServManClientPool[i].RecvTunnel;
        for st := low(TStatisticsType) to high(TStatisticsType) do
            buff[st] := buff[st] + comm.Statistics[st];

        comm := ManagerService.ServManClientPool[i].SendTunnel;
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

    for i := 0 to ManagerService.ServManClientPool.Count - 1 do
      begin
        comm := ManagerService.ServManClientPool[i].RecvTunnel;
        RecvLst.IncValue(comm.CmdRecvStatistics);
        SendLst.IncValue(comm.CmdSendStatistics);
        ExecuteConsumeLst.SetMax(comm.CmdMaxExecuteConsumeStatistics);

        comm := ManagerService.ServManClientPool[i].SendTunnel;
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
  ManagerService.ServerConfig.GetSectionList(ns);

  TreeView.Items.BeginUpdate;
  TreeView.Items.Clear;

  for ServerType := low(TServerType) to high(TServerType) do
      vServerVal[ServerType] := 0;

  for i := 0 to ns.Count - 1 do
    begin
      vl := ManagerService.ServerConfig.VariantList[ns[i]];

      ServerType := vl.GetDefaultValue('Type', TServerType.stUnknow);
      inc(vServerVal[ServerType]);
    end;

  for i := 0 to ns.Count - 1 do
    begin
      vl := ManagerService.ServerConfig.VariantList[ns[i]];

      try
        RegName := vl.GetDefaultValue('Name', '');
        ManServAddr := vl.GetDefaultValue('ManagerServer', '');
        RegAddr := vl.GetDefaultValue('Host', '');
        RegRecvPort := vl.GetDefaultValue('RecvPort', 0);
        RegSendPort := vl.GetDefaultValue('SendPort', 0);
        LastEnabled := vl.GetDefaultValue('LastEnabled', GetTimeTickCount);
        WorkLoad := vl.GetDefaultValue('WorkLoad', 0);
        ServerType := vl.GetDefaultValue('Type', TServerType.stUnknow);

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
  ManagerService.ServManClientPool.ServerConfig.GetSectionList(ns);
  for i := 0 to ns.Count - 1 do
    begin
      vl := ManagerService.ServManClientPool.ServerConfig.VariantList[ns[i]];

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

  for i := 0 to ManagerService.ServManClientPool.Count - 1 do
    begin
      c := ManagerService.ServManClientPool[i];
      try
        n := Format('connected Manager server(%d)/%d - %s/registed name: %s', [ManagerService.ServManClientPool.Count, i + 1, c.ConnectInfo.ManServAddr, c.ConnectInfo.RegName]);
        GetPathTreeNode(n, '/', TreeView, nil);

        n := Format('connected Manager server(%d)/%d - %s/registed address: %s', [ManagerService.ServManClientPool.Count, i + 1, c.ConnectInfo.ManServAddr, c.ConnectInfo.RegAddr]);
        GetPathTreeNode(n, '/', TreeView, nil);

        n := Format('connected Manager server(%d)/%d - %s/registed receive Port: %d', [ManagerService.ServManClientPool.Count, i + 1, c.ConnectInfo.ManServAddr, c.ConnectInfo.RegRecvPort]);
        GetPathTreeNode(n, '/', TreeView, nil);

        n := Format('connected Manager server(%d)/%d - %s/registed send Port: %d', [ManagerService.ServManClientPool.Count, i + 1, c.ConnectInfo.ManServAddr, c.ConnectInfo.RegSendPort]);
        GetPathTreeNode(n, '/', TreeView, nil);

        n := Format('connected Manager server(%d)/%d - %s/registed type: %s', [ManagerService.ServManClientPool.Count, i + 1, c.ConnectInfo.ManServAddr, serverType2Str(c.ConnectInfo.ServerType)]);
        GetPathTreeNode(n, '/', TreeView, nil);

        n := Format('connected Manager server(%d)/%d - %s/connected: %s', [ManagerService.ServManClientPool.Count, i + 1, c.ConnectInfo.ManServAddr, BoolToStr(c.Connected, True)]);
        GetPathTreeNode(n, '/', TreeView, nil);

        n := Format('connected Manager server(%d)/%d - %s/reconnect total: %d', [ManagerService.ServManClientPool.Count, i + 1, c.ConnectInfo.ManServAddr, c.ReconnectTotal]);
        GetPathTreeNode(n, '/', TreeView, nil);
      except
      end;
    end;

  PrintServerState('Service Statistics', [RecvService, SendService, AccessService]);

  PrintServerCMDStatistics('Command Statistics', [RecvService, SendService, AccessService]);

  TreeView.Items.EndUpdate;
  DisposeObject(ns);
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
  if ShowAndConnectManagerServer(ManagerService.ServManClientPool, cManagerService_SendPort, cManagerService_RecvPort, TServerType.stManager) then
    begin
      ManagerService.ServManClientPool.ServerConfig.Merge(ManagerService.ServerConfig);
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

procedure TManagerServerForm.DoStatusNear(AText: SystemString; const ID: Integer);
begin
  if StatusCheckBox.Checked then
    begin
      Memo.Lines.Append(AText);
    end;
end;

function TManagerServerForm.GetPathTreeNode(_Value, _Split: SystemString; _TreeView: TTreeView; _RN: TTreeNode): TTreeNode;
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

procedure TManagerServerForm.AppEventsException(Sender: TObject; E: Exception);
begin
  DoStatus(E.ToString);
end;

procedure TManagerServerForm.Command_Query(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
var
  servType: TServerType;
  i       : Integer;
  ns      : TListString;
  vl      : THashVariantList;
begin
  servType := TServerType(InData.Reader.ReadByte);

  ns := TListString.Create;
  ManagerService.ServerConfig.GetSectionList(ns);

  for i := 0 to ns.Count - 1 do
    begin
      vl := ManagerService.ServerConfig.VariantList[ns[i]];
      if servType = vl.GetDefaultValue('Type', TServerType.stUnknow) then
        begin
          OutData.WriteVariantList(vl);
        end;
    end;

  DisposeObject(ns);
end;

procedure TManagerServerForm.Command_QueryMinLoad(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
var
  servType: TServerType;
  i       : Integer;
  ns      : TListString;
  vl, mvl : THashVariantList;
begin
  servType := TServerType(InData.Reader.ReadByte);

  ns := TListString.Create;
  ManagerService.ServerConfig.GetSectionList(ns);

  mvl := nil;
  for i := 0 to ns.Count - 1 do
    begin
      vl := ManagerService.ServerConfig.VariantList[ns[i]];
      if servType = vl.GetDefaultValue('Type', TServerType.stUnknow) then
        begin
          if (mvl = nil) or (mvl['WorkLoad'] > vl['WorkLoad']) then
              mvl := vl;
        end;
    end;
  if mvl <> nil then
      OutData.WriteVariantList(mvl);

  DisposeObject(ns);
end;

procedure TManagerServerForm.PostExecute_DelayStartService(Sender: TNPostExecute);
begin
  StartService;
end;

procedure TManagerServerForm.PostExecute_DelayRegService(Sender: TNPostExecute);
begin
  if AutoConnectManagerServer(ManagerService.ServManClientPool,
    Sender.Data3, Sender.Data4, umlStrToInt(SendPortEdit.Text, cManagerService_SendPort), umlStrToInt(RecvPortEdit.Text, cManagerService_RecvPort), TServerType.stManager) then
    begin
      ManagerService.ServManClientPool.ServerConfig.Merge(ManagerService.ServerConfig);
    end;
end;

constructor TManagerServerForm.Create(AOwner: TComponent);
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

  RecvService := TCommunicationFramework_Server_CrossSocket.Create;
  RecvService.PrintParams['AntiIdle'] := False;

  SendService := TCommunicationFramework_Server_CrossSocket.Create;

  AccessService := TCommunicationFramework_Server_CrossSocket.Create;
  AccessService.IdleTimeout := 5000;
  AccessService.RegisterStream('Query').OnExecute := Command_Query;
  AccessService.RegisterStream('QueryMinLoad').OnExecute := Command_QueryMinLoad;

  ManagerService := TServerManager.Create(RecvService, SendService, TCommunicationFramework_Client_CrossSocket);
  ManagerService.RegisterCommand;

  Memo.Lines.Add(WSAInfo);
  Memo.Lines.Add(Format('File Receive directory %s', [ManagerService.FileReceiveDirectory]));

  RecvPortEdit.Text := IntToStr(cManagerService_RecvPort);
  SendPortEdit.Text := IntToStr(cManagerService_SendPort);
  QueryPortEdit.Text := IntToStr(cManagerService_QueryPort);

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

            if umlMultipleMatch(['Query:*', 'q:*', '-q:*', '-Query:*'], p1) then
              begin
                p2 := umlDeleteFirstStr(p1, ':');
                if umlIsNumber(p2) then
                    QueryPortEdit.Text := p2;
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
      with ManagerService.ProgressEngine.PostExecute(delayStartServiceTime) do
          OnExecuteMethod := PostExecute_DelayStartService;
    end;

  if delayReg then
    begin
      with ManagerService.ProgressEngine.PostExecute(delayRegTime) do
        begin
          Data3 := ManServAddr;
          Data4 := RegAddr;
          OnExecuteMethod := PostExecute_DelayRegService;
        end;
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

  DeleteDoStatusHook(Self);
  try
      inherited Destroy;
  except
  end;
end;

procedure TManagerServerForm.StartService;
begin
  StopService;
  if AccessService.StartService(BindIPEdit.Text, umlStrToInt(QueryPortEdit.Text, cManagerService_QueryPort)) then
      DoStatus('Manager Access Service ready Ok! bind:%s port:%s', [TranslateBindAddr(BindIPEdit.Text), QueryPortEdit.Text])
  else
      MessageDlg(Format('Manager Access Service Failed! bind:%s port:%s', [TranslateBindAddr(BindIPEdit.Text), QueryPortEdit.Text]),
      mtError, [mbYes], 0);

  if RecvService.StartService(BindIPEdit.Text, umlStrToInt(RecvPortEdit.Text, cManagerService_RecvPort)) then
      DoStatus('Receive tunnel ready Ok! bind:%s port:%s', [TranslateBindAddr(BindIPEdit.Text), RecvPortEdit.Text])
  else
      MessageDlg(Format('Receive tunnel Failed! bind:%s port:%s', [TranslateBindAddr(BindIPEdit.Text), RecvPortEdit.Text]),
      mtError, [mbYes], 0);

  if SendService.StartService(BindIPEdit.Text, umlStrToInt(SendPortEdit.Text, cManagerService_SendPort)) then
      DoStatus('Send tunnel ready Ok! bind:%s port:%s', [TranslateBindAddr(BindIPEdit.Text), SendPortEdit.Text])
  else
      MessageDlg(Format('Send tunnel Failed! bind:%s port:%s', [TranslateBindAddr(BindIPEdit.Text), SendPortEdit.Text]),
      mtError, [mbYes], 0);
end;

procedure TManagerServerForm.StopService;
begin
  try
    AccessService.StopService;
    RecvService.StopService;
    SendService.StopService;
  except
  end;
end;

initialization

end.
