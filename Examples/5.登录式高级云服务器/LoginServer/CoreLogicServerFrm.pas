unit CoreLogicServerFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls, System.TypInfo,
  System.DateUtils, Vcl.AppEvnts,

  CommunicationFramework_Server_ICSCustomSocket,
  CommunicationFrameworkDoubleTunnelIO, CommunicationFrameworkDoubleTunnelIO_NoAuth,
  CommunicationFrameworkIO,
  ConnectManagerServerFrm, DataFrameEngine, ListEngine, Cadencer,
  CoreClasses, DoStatusIO, ManagerServer_ClientIntf, CommunicationFramework,
  UnicodeMixedLib, TextDataEngine, MemoryStream64,
  DBClientIntf, DBCompressPackageForFile,
  NotifyObjectBase,
  CommunicationFramework_Server_CrossSocket,
  CommunicationFramework_Client_ICS, CommunicationFramework_Server_ICS;

type
  TCoreLogicServerForm = class;
  TLoginService        = class;

  TPerUserLoginSendTunnel = class(TPeerClientUserDefineForSendTunnel)
  protected
  public
    constructor Create(AOwner: TPeerClient); override;
    destructor Destroy; override;
  end;

  TPerUserLoginRecvTunnel = class(TPeerClientUserDefineForRecvTunnel)
  protected
    LastAccessHomeCli               : Cardinal; // homeserver->send tunnel->id
    LastGenPlayerIDInHome           : Cardinal;
    DownloadDBClient, UploadDBClient: TDB_DoubleTunnelClient;

    DisableHomeList: TListString;
  public
    constructor Create(AOwner: TPeerClient); override;
    destructor Destroy; override;
  end;

  PGetFileUserData = ^TGetFileUserData;

  TGetFileUserData = record
    client: TPeerClient;
    InData, OutData: TDataFrameEngine;
  end;

  TLoginService = class(TCommunicationFramework_DoubleTunnelService)
  private
    lastRegUserSerialNo: Integer;
  protected
    procedure UserLoginFileDownloadComplete(const UserData: Pointer; const UserObject: TCoreClassObject; Stream: TCoreClassStream; const fileName: string);
    procedure UserLoginQueryComplete(Sender: TDBFileInfoList);
    procedure UserLockQueryComplete(Sender: TDBUserStateList);
    // 登录钩子，重新从数据库下载一份客户端配置文件副本
    procedure Command_UserLogin(Sender: TPeerClient; InData, OutData: TDataFrameEngine); override;

    procedure UserRegistedSuccess(UserID: string); override;
    procedure UserLinkSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel); override;
    procedure UserLoginSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel); override;
    procedure UserOut(UserDefineIO: TPeerClientUserDefineForRecvTunnel); override;
  protected
    procedure UserRegQueryComplete(Sender: TDBFileInfoList);

    procedure UserRegGuestUserQueryComplete(Sender: TDBFileInfoList);
    procedure Command_RegGuestUser(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
  public
    CoreLogicForm: TCoreLogicServerForm;
    constructor Create(ARecvTunnel, ASendTunnel: TCommunicationFrameworkServer);
    destructor Destroy; override;

    procedure RegisterCommand; override;
    procedure UnRegisterCommand; override;
  end;

  TCoreLogicServerForm = class(TForm, IDBClientInterface)
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
    FLoginRecvTunnel: TCommunicationFramework_Server_CrossSocket;
    FLoginSendTunnel: TCommunicationFramework_Server_CrossSocket;
    FLoginService   : TLoginService;

    FManagerClients: TManagerClients;
    FDBClients     : TDB_DoubleTunnelClients;

    procedure DoStatusNear(AText: string; const ID: Integer);
    function GetPathTreeNode(_Value, _Split: string; _TreeView: TTreeView; _RN: TTreeNode): TTreeNode;
    procedure ManagerClient_ServerConfigChange(Sender: TManagerClient; ConfigData: TSectionTextData);

    procedure PostExecute_DelayStartService(Sender: TNPostExecute);
    procedure PostExecute_DelayRegService(Sender: TNPostExecute);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure StartService;
    procedure StopService;
  end;

var
  CoreLogicServerForm: TCoreLogicServerForm;

implementation

{$R *.dfm}


constructor TPerUserLoginSendTunnel.Create(AOwner: TPeerClient);
begin
  inherited Create(AOwner);
end;

destructor TPerUserLoginSendTunnel.Destroy;
begin
  inherited Destroy;
end;

constructor TPerUserLoginRecvTunnel.Create(AOwner: TPeerClient);
begin
  inherited Create(AOwner);
  LastAccessHomeCli := 0;
  LastGenPlayerIDInHome := 0;
  DownloadDBClient := nil;
  UploadDBClient := nil;
  DisableHomeList := TListString.Create;
end;

destructor TPerUserLoginRecvTunnel.Destroy;
begin
  DisposeObject(DisableHomeList);
  inherited Destroy;
end;

procedure TLoginService.UserLoginFileDownloadComplete(const UserData: Pointer; const UserObject: TCoreClassObject; Stream: TCoreClassStream; const fileName: string);
var
  p: PGetFileUserData;
begin
  p := UserData;
  if RecvTunnel.Exists(p^.client) then
    begin
      if UnPackStreamAsUser(Stream) then
        begin
          inherited Command_UserLogin(p^.client, p^.InData, p^.OutData);
          p^.client.ContinueResultSend;
        end
      else
          p^.client.Disconnect;
    end;
  dispose(p);
  umlDeleteFile(fileName);
end;

procedure TLoginService.UserLoginQueryComplete(Sender: TDBFileInfoList);
var
  SendTunnelID      : Cardinal;
  UserID, UserPasswd: string;

  LoginCli: TPerUserLoginRecvTunnel;
  p       : PGetFileUserData;
begin
  if RecvTunnel.Exists(Sender.PeerClientData) and (Sender.Count > 0) then
    begin
      LoginCli := Sender.PeerClientData.UserDefine as TPerUserLoginRecvTunnel;
      LoginCli.DownloadDBClient := Sender[Sender.Count - 1]^.client;
      LoginCli.UploadDBClient := Sender[0]^.client;

      Sender.InData.Reader.index := 0;
      SendTunnelID := Sender.InData.Reader.ReadCardinal;
      UserID := Sender.InData.Reader.ReadString;
      UserPasswd := Sender.InData.Reader.ReadString;
      Sender.InData.Reader.index := 0;

      if Sender[Sender.Count - 1]^.FileLastTime <> 0 then
        begin
          new(p);
          p^.client := Sender.PeerClientData;
          p^.InData := Sender.InData;
          p^.OutData := Sender.OutData;

          LoginCli.DownloadDBClient.GetFile(Sender.fileName, FPublicPath, p, nil, UserLoginFileDownloadComplete);
        end
      else
        begin
          Sender.OutData.WriteBool(False);
          Sender.OutData.WriteString(Format('user %s no in DBServer', [UserID]));

          Sender.PeerClientData.Print('user %s no in DBServer', [UserID]);
          Sender.PeerClientData.ContinueResultSend;
        end;
    end;
end;

procedure TLoginService.UserLockQueryComplete(Sender: TDBUserStateList);
var
  i     : Integer;
  locked: Boolean;

  SendTunnelID      : Cardinal;
  UserID, UserPasswd: string;

  FileInfoLst: TDBFileInfoList;

  LoginCli: TPerUserLoginRecvTunnel;
begin
  if RecvTunnel.Exists(Sender.PeerClientData) and (Sender.Count > 0) then
    begin
      locked := False;

      for i := 0 to Sender.Count - 1 do
        if Sender[i]^.userState then
          begin
            locked := True;
            break;
          end;

      Sender.InData.Reader.index := 0;
      SendTunnelID := Sender.InData.Reader.ReadCardinal;
      UserID := Sender.InData.Reader.ReadString;
      UserPasswd := Sender.InData.Reader.ReadString;
      Sender.InData.Reader.index := 0;

      if not locked then
        begin
          FileInfoLst := CoreLogicForm.FDBClients.QueryDBFile(UserID + '.LoginPackage', UserLoginQueryComplete);
          FileInfoLst.PeerClientData := Sender.PeerClientData;
          FileInfoLst.InData := Sender.InData;
          FileInfoLst.OutData := Sender.OutData;
        end
      else
        begin
          Sender.OutData.WriteBool(False);
          Sender.OutData.WriteString(Format('user %s is locked', [UserID]));
          Sender.PeerClientData.Print('user %s is locked', [UserID]);
          Sender.PeerClientData.ContinueResultSend;
        end;
    end;
end;

procedure TLoginService.Command_UserLogin(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
var
  SendTunnelID      : Cardinal;
  UserID, UserPasswd: string;
  UserStateLst      : TDBUserStateList;
begin
  SendTunnelID := InData.Reader.ReadCardinal;
  UserID := InData.Reader.ReadString;
  UserPasswd := InData.Reader.ReadString;
  InData.Reader.index := 0;

  if UserOnline(UserID) then
    begin
      Sender.Disconnect;
      exit;
    end;

  UserStateLst := CoreLogicForm.FDBClients.QueryUserIsLock(UserID, UserLockQueryComplete);
  UserStateLst.PeerClientData := Sender;
  UserStateLst.InData := Sender.InDataFrame;
  UserStateLst.OutData := Sender.OutDataFrame;

  // 暂停反馈
  Sender.PauseResultSend;
end;

procedure TLoginService.UserRegistedSuccess(UserID: string);
begin
end;

procedure TLoginService.UserLinkSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel);
begin
  inherited UserLinkSuccess(UserDefineIO);
end;

procedure TLoginService.UserLoginSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel);
var
  LoginCli: TPerUserLoginRecvTunnel;
begin
  LoginCli := UserDefineIO as TPerUserLoginRecvTunnel;

  if (LoginCli.DownloadDBClient <> nil) then
      LoginCli.DownloadDBClient.UserLock(LoginCli.UserID);
end;

procedure TLoginService.UserOut(UserDefineIO: TPeerClientUserDefineForRecvTunnel);
var
  LoginCli: TPerUserLoginRecvTunnel;
  sendDE  : TDataFrameEngine;
  ms      : TMemoryStream64;
begin
  LoginCli := UserDefineIO as TPerUserLoginRecvTunnel;

  if (LoginCli.UploadDBClient <> nil) and (LoginCli.LinkOk) then
    begin
      ms := TMemoryStream64.Create;
      if PackUserAsStream(LoginCli.UserID, ms) then
          LoginCli.UploadDBClient.PostFile(LoginCli.UserID + '.LoginPackage', ms)
      else
          DisposeObject(ms);
    end;

  if LoginCli.DownloadDBClient <> nil then
      LoginCli.DownloadDBClient.UserUnLock(LoginCli.UserID);

  inherited UserOut(UserDefineIO);
end;

procedure TLoginService.UserRegQueryComplete(Sender: TDBFileInfoList);
var
  LoginCli: TPerUserLoginRecvTunnel;
  ms      : TMemoryStream64;
begin
  if RecvTunnel.Exists(Sender.PeerClientData) and (Sender.Count > 0) then
    begin
      LoginCli := Sender.PeerClientData.UserDefine as TPerUserLoginRecvTunnel;
      LoginCli.DownloadDBClient := Sender[Sender.Count - 1].client;
      LoginCli.UploadDBClient := Sender[0].client;

      if Sender[Sender.Count - 1].FileLastTime <> 0 then
        begin
          (Sender.OutData.Data[0] as TDataFrameByte).Buffer := 0;
        end
      else
        begin
          ms := TMemoryStream64.Create;
          if PackUserAsStream(Sender.OutData.ReadString(1), ms) then
              LoginCli.UploadDBClient.PostFile(umlGetFileName(Sender.fileName), ms)
          else
              DisposeObject(ms);
        end;
      Sender.PeerClientData.ContinueResultSend;
    end;
end;

procedure TLoginService.UserRegGuestUserQueryComplete(Sender: TDBFileInfoList);
var
  LoginCli: TPerUserLoginRecvTunnel;
  i, j    : Integer;

  UserID: string;
  l     : TDBFileInfoList;
  te    : TSectionTextData;
  ms    : TMemoryStream64;
begin
  if RecvTunnel.Exists(Sender.PeerClientData) and (Sender.Count > 0) then
    begin
      LoginCli := Sender.PeerClientData.UserDefine as TPerUserLoginRecvTunnel;

      for i := 0 to Sender.Count - 1 do
        if Sender[i]^.FileLastTime <> 0 then
          begin
            // 用户已经存在，轮询注册
            j := Sender.SerialNo + 1;
            repeat
              UserID := Format('guest%.5d', [j]);
              inc(j);
            until not ExistsUser(UserID);

            l := CoreLogicForm.FDBClients.QueryDBFile(UserID + '.LoginPackage', UserRegGuestUserQueryComplete);
            l.PeerClientData := Sender.PeerClientData;
            l.InData := Sender.InData;
            l.OutData := Sender.OutData;
            l.SerialNo := j - 1;
            exit;
          end;

      lastRegUserSerialNo := Sender.SerialNo + 1;

      UserID := Format('guest%.5d', [Sender.SerialNo]);
      te := TSectionTextData.Create;
      te.Hit['UserInfo', 'Alias'] := Format('别名%.5d', [Sender.SerialNo]);

      if RegUser(UserID, UserID, te) then
        begin
          ms := TMemoryStream64.Create;
          if PackUserAsStream(UserID, ms) then
              Sender[Sender.Count - 1]^.client.PostFile(UserID + '.LoginPackage', ms)
          else
              DisposeObject(ms);

          Sender.OutData.WriteBool(True);
          Sender.OutData.WriteString(UserID);
          Sender.OutData.WriteString(UserID);
          Sender.PeerClientData.ContinueResultSend;
        end
      else
        begin
          // 本地注册失败，再到远程去轮询
          j := Sender.SerialNo + 1;
          repeat
            UserID := Format('guest%.5d', [j]);
            inc(j);
          until not ExistsUser(UserID);

          l := CoreLogicForm.FDBClients.QueryDBFile(UserID + '.LoginPackage', UserRegGuestUserQueryComplete);
          l.PeerClientData := Sender.PeerClientData;
          l.InData := Sender.InData;
          l.OutData := Sender.OutData;
          l.SerialNo := j - 1;
        end;
      DisposeObject(te);
    end;
end;

procedure TLoginService.Command_RegGuestUser(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
var
  UserID: string;
  j     : Integer;
  l     : TDBFileInfoList;
begin
  j := lastRegUserSerialNo;
  repeat
    UserID := Format('guest%.5d', [j]);
    inc(j);
  until not ExistsUser(UserID);

  l := CoreLogicForm.FDBClients.QueryDBFile(UserID + '.LoginPackage', UserRegGuestUserQueryComplete);
  l.PeerClientData := Sender;
  l.InData := Sender.InDataFrame;
  l.OutData := Sender.OutDataFrame;
  l.SerialNo := j - 1;

  // 暂停反馈
  Sender.PauseResultSend;
end;

constructor TLoginService.Create(ARecvTunnel, ASendTunnel: TCommunicationFrameworkServer);
begin
  inherited Create(ARecvTunnel, ASendTunnel);

  FRecvTunnel.PeerClientUserDefineClass := TPerUserLoginRecvTunnel;
  FSendTunnel.PeerClientUserDefineClass := TPerUserLoginSendTunnel;
  CoreLogicForm := nil;

  CanSaveUserInfo := True;
  LoadUserDB;
  lastRegUserSerialNo := 1;
end;

destructor TLoginService.Destroy;
begin
  inherited Destroy;
end;

procedure TLoginService.RegisterCommand;
begin
  inherited RegisterCommand;
  FRecvTunnel.RegisterStream('RegGuestUser').OnExecute := Command_RegGuestUser;
end;

procedure TLoginService.UnRegisterCommand;
begin
  inherited UnRegisterCommand;
  FRecvTunnel.DeleteRegistedCMD('RegGuestUser');
end;

procedure TCoreLogicServerForm.StartServiceButtonClick(Sender: TObject);
begin
  StartService;
end;

procedure TCoreLogicServerForm.StopServiceButtonClick(Sender: TObject);
begin
  StopService;
end;

procedure TCoreLogicServerForm.AppEventsException(Sender: TObject;
  E: Exception);
begin
  DoStatus(E.ToString);
end;

procedure TCoreLogicServerForm.connectButtonClick(Sender: TObject);
begin
  ShowAndConnectManagerServer(FManagerClients, umlStrToInt(SendPortEdit.Text, 3338), umlStrToInt(RecvPortEdit.Text, 3339), cCoreLogicServer);
end;

procedure TCoreLogicServerForm.RefreshServerListButtonClick(Sender: TObject);
var
  i       : Integer;
  LoginCli: TPerUserLoginRecvTunnel;
  ns      : TCoreClassStringList;
  vl      : THashVariantList;

  ManServAddr     : string;
  RegName, RegAddr: string;
  RegRecvPort     : Word;
  RegSendPort     : Word;
  LastEnabled     : UInt64;
  workload        : Word;
  ServerType      : byte;

  vDBServer, vCoreLogicServer, vManagerServer, vPayService, vPayQueryService, vUnknowServer: byte;

  n    : string;
  DBCli: TDB_DoubleTunnelClient;
  c    : TManagerClient;

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

    for i := 0 to FDBClients.Count - 1 do
      begin
        comm := FDBClients[i].RecvTunnel;
        for st := low(TStatisticsType) to high(TStatisticsType) do
            buff[st] := buff[st] + comm.Statistics[st];

        comm := FDBClients[i].SendTunnel;
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

    for i := 0 to FDBClients.Count - 1 do
      begin
        comm := FDBClients[i].RecvTunnel;
        RecvLst.IncValue(comm.CmdRecvStatistics);
        SendLst.IncValue(comm.CmdSendStatistics);
        ExecuteConsumeLst.SetMax(comm.CmdMaxExecuteConsumeStatistics);

        comm := FDBClients[i].SendTunnel;
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
  TreeView.Items.BeginUpdate;
  TreeView.Items.Clear;

  for i := 0 to FLoginRecvTunnel.Count - 1 do
    begin
      LoginCli := FLoginRecvTunnel[i].UserDefine as TPerUserLoginRecvTunnel;
      // 如果验证客户端登录并且link成功
      if (LoginCli.LoginSuccessed) and (LoginCli.SendTunnel <> nil) then
        begin
        end;
    end;

  vDBServer := 0;
  vCoreLogicServer := 0;
  vManagerServer := 0;
  vPayService := 0;
  vPayQueryService := 0;
  vUnknowServer := 0;

  ns := TCoreClassStringList.Create;
  FManagerClients.ServerConfig.GetSectionList(ns);

  for i := 0 to ns.Count - 1 do
    begin
      vl := FManagerClients.ServerConfig.VariantList[ns[i]];

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
      vl := FManagerClients.ServerConfig.VariantList[ns[i]];

      try
        RegName := vl.GetDefaultValue('Name', '');
        ManServAddr := vl.GetDefaultValue('ManagerServer', '');
        RegAddr := vl.GetDefaultValue('Host', '');
        RegRecvPort := vl.GetDefaultValue('RecvPort', 0);
        RegSendPort := vl.GetDefaultValue('SendPort', 0);
        LastEnabled := vl.GetDefaultValue('LastEnabled', GetTimeTickCount);
        workload := vl.GetDefaultValue('WorkLoad', 0);
        ServerType := vl.GetDefaultValue('Type', cUnknowServer);

        n := Format('Remote Server Configure/%s(%d)/(%d)%s/registed name: %s', [serverType2Str(ServerType), GetServTypStat(ServerType), i, RegAddr, RegName]);
        GetPathTreeNode(n, '/', TreeView, nil);

        n := Format('Remote Server Configure/%s(%d)/(%d)%s/Receive Port: %d', [serverType2Str(ServerType), GetServTypStat(ServerType), i, RegAddr, RegRecvPort]);
        GetPathTreeNode(n, '/', TreeView, nil);

        n := Format('Remote Server Configure/%s(%d)/(%d)%s/Send Port: %d', [serverType2Str(ServerType), GetServTypStat(ServerType), i, RegAddr, RegSendPort]);
        GetPathTreeNode(n, '/', TreeView, nil);

        n := Format('Remote Server Configure/%s(%d)/(%d)%s/WorkLoad: %d', [serverType2Str(ServerType), GetServTypStat(ServerType), i, RegAddr, workload]);
        GetPathTreeNode(n, '/', TreeView, nil);

        n := Format('Remote Server Configure/%s(%d)/(%d)%s/last active %d second ago', [serverType2Str(ServerType), GetServTypStat(ServerType), i, RegAddr, Round((GetTimeTickCount - LastEnabled) / 1000)]);
        GetPathTreeNode(n, '/', TreeView, nil);
      except
      end;
    end;

  for i := 0 to FDBClients.Count - 1 do
    begin
      DBCli := FDBClients[i];
      try
        n := Format('connected DB server(%d)/%d - %s/registed host: %s', [FDBClients.Count, i + 1, DBCli.ConnectInfo.DBServAddr, DBCli.ConnectInfo.RegAddr]);
        GetPathTreeNode(n, '/', TreeView, nil);
        n := Format('connected DB server(%d)/%d - %s/receive port: %d', [FDBClients.Count, i + 1, DBCli.ConnectInfo.DBServAddr, DBCli.ConnectInfo.RegRecvPort]);
        GetPathTreeNode(n, '/', TreeView, nil);
        n := Format('connected DB server(%d)/%d - %s/Send port: %d', [FDBClients.Count, i + 1, DBCli.ConnectInfo.DBServAddr, DBCli.ConnectInfo.RegSendPort]);
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

  PrintServerState('Service Statistics', [FLoginRecvTunnel, FLoginSendTunnel]);

  PrintServerCMDStatistics('Command Statistics', [FLoginRecvTunnel, FLoginSendTunnel]);

  TreeView.Items.EndUpdate;
  DisposeObject(ns);
end;

procedure TCoreLogicServerForm.ProgressTimerTimer(Sender: TObject);
begin
  try
    FLoginService.Progress;
    FManagerClients.Progress;
    FDBClients.Progress;
    ProcessICSMessages;
  except
  end;
end;

procedure TCoreLogicServerForm.AntiIDLETimerTimer(Sender: TObject);
begin
  try
    if Memo.Lines.Count > 5000 then
        Memo.Clear;

    FManagerClients.AntiIdle(FLoginRecvTunnel.Count + FLoginSendTunnel.Count);
    FDBClients.AntiIdle(FLoginRecvTunnel.Count + FLoginSendTunnel.Count);
    Caption := Format('CoreLogic service...(user:%d)', [FLoginService.TotalLinkCount]);
  except
  end;
end;

procedure TCoreLogicServerForm.DoStatusNear(AText: string; const ID: Integer);
begin
  if StatusCheckBox.Checked then
      Memo.Lines.Append(AText);
end;

procedure TCoreLogicServerForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  StopService;
  Action := caFree;
end;

function TCoreLogicServerForm.GetPathTreeNode(_Value, _Split: string; _TreeView: TTreeView; _RN: TTreeNode): TTreeNode;
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

procedure TCoreLogicServerForm.ManagerClient_ServerConfigChange(Sender: TManagerClient; ConfigData: TSectionTextData);
var
  ns: TCoreClassStringList;
  i : Integer;
  vl: THashVariantList;

  DBServAddr, RegAddr                                   : string;
  DBCliRecvPort, DBCliSendPort, RegRecvPort, RegSendPort: Word;
begin
  if FManagerClients.Count = 0 then
      exit;

  ns := TCoreClassStringList.Create;
  ConfigData.GetSectionList(ns);

  for i := 0 to ns.Count - 1 do
    begin
      vl := ConfigData.VariantList[ns[i]];
      if vl.GetDefaultValue('Type', cUnknowServer) = cDBServer then
        begin
          DBServAddr := vl.GetDefaultValue('Host', '');
          DBCliSendPort := vl.GetDefaultValue('SendPort', 5731);
          DBCliRecvPort := vl.GetDefaultValue('RecvPort', 5732);
          RegAddr := FManagerClients.LastRegAddr;
          RegRecvPort := FLoginSendTunnel.BindPort;
          RegSendPort := FLoginRecvTunnel.BindPort;

          FDBClients.BuildClientAndConnect(DBServAddr, RegAddr, DBCliRecvPort, DBCliSendPort, RegRecvPort, RegSendPort);
        end;
    end;
  DisposeObject(ns);
end;

procedure TCoreLogicServerForm.PostExecute_DelayStartService(Sender: TNPostExecute);
begin
  StartService;
end;

procedure TCoreLogicServerForm.PostExecute_DelayRegService(Sender: TNPostExecute);
begin
  AutoConnectManagerServer(FManagerClients,
    Sender.Data3, Sender.Data4, umlStrToInt(SendPortEdit.Text, 3338), umlStrToInt(RecvPortEdit.Text, 3339), cCoreLogicServer);
end;

constructor TCoreLogicServerForm.Create(AOwner: TComponent);
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

  FLoginRecvTunnel := TCommunicationFramework_Server_CrossSocket.Create;
  FLoginRecvTunnel.IdleTimeout := 5 * 60 * 1000;
  FLoginRecvTunnel.PrintParams['AntiIdle'] := False;
  FLoginSendTunnel := TCommunicationFramework_Server_CrossSocket.Create;

  FLoginService := TLoginService.Create(FLoginRecvTunnel, FLoginSendTunnel);
  FLoginService.CanStatus := True;
  FLoginService.RegisterCommand;

  FLoginService.CoreLogicForm := Self;

  FManagerClients := TManagerClients.Create;
  FManagerClients.OnServerConfigChange := ManagerClient_ServerConfigChange;

  FDBClients := TDB_DoubleTunnelClients.Create(Self);

  Memo.Lines.Add(WSAInfo);
  Memo.Lines.Add(Format('LoginService Root directory %s', [FLoginService.RootPath]));

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
      with FLoginService.ProgressEngine.PostExecute(delayStartServiceTime) do
          OnExecute := PostExecute_DelayStartService;
    end;

  if delayReg then
    begin
      with FLoginService.ProgressEngine.PostExecute(delayRegTime) do
        begin
          Data3 := ManServAddr;
          Data4 := RegAddr;
          OnExecute := PostExecute_DelayRegService;
        end;
    end;

  DoStatus('');
end;

destructor TCoreLogicServerForm.Destroy;
begin
  DisposeObject(FLoginRecvTunnel);
  DisposeObject(FLoginSendTunnel);
  DisposeObject(FLoginService);

  DisposeObject(FManagerClients);
  DisposeObject(FDBClients);

  DeleteDoStatusHook(Self);
  try
      inherited Destroy;
  except
  end;
end;

procedure TCoreLogicServerForm.StartService;
begin
  StopService;

  if FLoginRecvTunnel.StartService(BindIPEdit.Text, umlStrToInt(RecvPortEdit.Text, 3339)) then
      DoStatus('Login Receive tunnel ready Ok! bind:%s port:%s', [TranslateBindAddr(BindIPEdit.Text), RecvPortEdit.Text])
  else
      MessageDlg(Format('Login Receive tunnel Failed! bind:%s port:%s', [TranslateBindAddr(BindIPEdit.Text), RecvPortEdit.Text]),
      mtError, [mbYes], 0);

  if FLoginSendTunnel.StartService(BindIPEdit.Text, umlStrToInt(SendPortEdit.Text, 3338)) then
      DoStatus('Login Send tunnel ready Ok! bind:%s port:%s', [TranslateBindAddr(BindIPEdit.Text), SendPortEdit.Text])
  else
      MessageDlg(Format('Login Send tunnel Failed! bind:%s port:%s', [TranslateBindAddr(BindIPEdit.Text), SendPortEdit.Text]),
      mtError, [mbYes], 0);

  FLoginRecvTunnel.IDCounter := 10;
end;

procedure TCoreLogicServerForm.StopService;
begin
  try
    FLoginRecvTunnel.StopService;
    FLoginSendTunnel.StopService;

    FManagerClients.Clear;
    FDBClients.Clear;
  except
  end;
end;

initialization


end.
