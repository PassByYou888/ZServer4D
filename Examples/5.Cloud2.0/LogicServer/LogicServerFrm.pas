unit LogicServerFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls,
  System.DateUtils, System.TypInfo, Vcl.AppEvnts,

  CommunicationFramework_Server_ICSCustomSocket,
  CommunicationFrameworkDoubleTunnelIO, CommunicationFrameworkDoubleTunnelIO_NoAuth,
  CommunicationFrameworkIO,
  ConnectManagerServerFrm, DataFrameEngine, ListEngine, Cadencer,
  CoreClasses, DoStatusIO, CommunicationFramework,
  UnicodeMixedLib, TextDataEngine, MemoryStream64,
  FilePackage,
  NotifyObjectBase,
  CommunicationFramework_Server_CrossSocket,
  CommunicationFramework_Client_CrossSocket,
  CommunicationFramework_Client_ICS, CommunicationFramework_Server_ICS,
  CommunicationFrameworkDoubleTunnelIO_ServMan,
  CommunicationFrameworkDoubleTunnelIO_VirtualAuth, FileStoreClientIntf,
  PascalStrings, DataStoreClientIntf, ZDBEngine, ZDBLocalManager,
  ObjectData, ObjectDataManager, ItemStream, CommonServiceDefine;

type
  TLogicServerForm = class;
  TLogicService    = class;
  TPayService      = class;

  TPerUserPaySendTunnel = class(TPeerClientUserDefineForSendTunnel_NoAuth)
  protected
  public
    PayService: TPayService;

    constructor Create(AOwner: TPeerClient); override;
    destructor Destroy; override;
  end;

  TPerUserPayRecvTunnel = class(TPeerClientUserDefineForRecvTunnel_NoAuth)
  protected
  public
    PayService : TPayService;
    PayWorkload: Word;
    constructor Create(AOwner: TPeerClient); override;
    destructor Destroy; override;
  end;

  TPayService = class(TCommunicationFramework_DoubleTunnelService_NoAuth)
  protected
    procedure UserLinkSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth); override;
    procedure UserOut(UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth); override;
  public
    LogicService: TLogicService;
    constructor Create(ARecvTunnel, ASendTunnel: TCommunicationFrameworkServer);
    destructor Destroy; override;

    procedure RegisterCommand; override;
    procedure UnRegisterCommand; override;
  end;

  TPerUserLogicSendTunnel = class(TPeerClientUserDefineForSendTunnel)
  protected
  public
    constructor Create(AOwner: TPeerClient); override;
    destructor Destroy; override;
  end;

  TPerUserLogicRecvTunnel = class(TPeerClientUserDefineForRecvTunnel)
  protected
    LastAccessHomeCli               : Cardinal; // homeserver->send tunnel->id
    LastGenPlayerIDInHome           : Cardinal;
    DownloadDBClient, UploadDBClient: TFileStoreService_DoubleTunnelClient;

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

  TLogicService = class(TCommunicationFramework_DoubleTunnelService)
  private
    PortraitStream     : TMemoryStream64;
    lastRegUserSerialNo: Integer;
    LogicFileDB        : TObjectDataManagerOfCache;
    FileMD5Cache       : THashList;
  protected
    procedure UserLoginFileDownloadComplete(const UserData: Pointer; const UserObject: TCoreClassObject; Stream: TCoreClassStream; const fileName: string);
    procedure UserLoginQueryComplete(Sender: TStoreFileInfoList);
    procedure UserLockQueryComplete(Sender: TFileStoreUserStateList);
    // 登录钩子，重新从数据库下载一份客户端配置文件副本
    procedure Command_UserLogin(Sender: TPeerClient; InData, OutData: TDataFrameEngine); override;

    procedure UserRegistedSuccess(UserID: string); override;
    procedure UserLinkSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel); override;
    procedure UserLoginSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel); override;
    procedure UserOut(UserDefineIO: TPeerClientUserDefineForRecvTunnel); override;

    procedure MD5CacheDataNotifyProc(p: Pointer);
  protected
    procedure UserRegQueryComplete(Sender: TStoreFileInfoList);

    procedure UserRegGuestUserQueryComplete(Sender: TStoreFileInfoList);
    procedure Command_RegGuestUser(Sender: TPeerClient; InData, OutData: TDataFrameEngine);

    procedure Command_GetLogicFileList(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
    procedure Command_GetLogicFileMD5(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
    procedure Command_GetLogicFile(Sender: TPeerClient; InData: TDataFrameEngine);

    procedure Command_GetAdvertisementFileList(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
    procedure Command_GetAdvertisementFileMD5(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
    procedure Command_GetAdvertisementFile(Sender: TPeerClient; InData: TDataFrameEngine);

    procedure GetUserInfoFileDownloadComplete(const UserData: Pointer; const UserObject: TCoreClassObject; Stream: TCoreClassStream; const fileName: string);
    procedure GetUserInfoQueryComplete(Sender: TStoreFileInfoList);
    procedure Command_GetUserInfo(Sender: TPeerClient; InData, OutData: TDataFrameEngine);

    procedure Command_ChangeAlias(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
  public
    LogicForm: TLogicServerForm;
    constructor Create(ARecvTunnel, ASendTunnel: TCommunicationFrameworkServer);
    destructor Destroy; override;

    procedure RegisterCommand; override;
    procedure UnRegisterCommand; override;

    procedure UserChange(UserID: string);
  end;

  TLogicServerForm = class(TForm, IServerManager_ClientPoolNotify, IFileStoreClientInterface)
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
    PayRecvPortEdit: TLabeledEdit;
    PaySendPortEdit: TLabeledEdit;
    procedure StartServiceButtonClick(Sender: TObject);
    procedure StopServiceButtonClick(Sender: TObject);
    procedure connectButtonClick(Sender: TObject);
    procedure RefreshServerListButtonClick(Sender: TObject);
    procedure ProgressTimerTimer(Sender: TObject);
    procedure AntiIDLETimerTimer(Sender: TObject);
    procedure AppEventsException(Sender: TObject; E: Exception);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FPayRecvTunnel: TCommunicationFramework_Server_CrossSocket;
    FPaySendTunnel: TCommunicationFramework_Server_CrossSocket;
    FPayService   : TPayService;

    FLogicRecvTunnel: TCommunicationFramework_Server_CrossSocket;
    FLogicSendTunnel: TCommunicationFramework_Server_CrossSocket;
    FLogicService   : TLogicService;

    FManagerClients     : TServerManager_ClientPool;
    FFileStoreClientPool: TFileStoreService_DoubleTunnelClientPool;
    FDataStoreClient    : TDataStore_DoubleTunnelClient;

    procedure DoStatusNear(AText: string; const ID: Integer);
    function GetPathTreeNode(_Value, _Split: string; _TreeView: TTreeView; _RN: TTreeNode): TTreeNode;

    procedure ServerConfigChange(Sender: TServerManager_Client; ConfigData: TSectionTextData);
    procedure ServerOffline(Sender: TServerManager_Client; RegAddr: string; ServerType: TServerType);

    procedure PostExecute_DelayStartService(Sender: TNPostExecute);
    procedure PostExecute_DelayRegService(Sender: TNPostExecute);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure StartService;
    procedure StopService;
  end;

var
  LogicServerForm: TLogicServerForm;

implementation

{$R *.dfm}


constructor TPerUserPaySendTunnel.Create(AOwner: TPeerClient);
begin
  inherited Create(AOwner);
end;

destructor TPerUserPaySendTunnel.Destroy;
begin
  inherited Destroy;
end;

constructor TPerUserPayRecvTunnel.Create(AOwner: TPeerClient);
begin
  inherited Create(AOwner);
end;

destructor TPerUserPayRecvTunnel.Destroy;
begin
  inherited Destroy;
end;

procedure TPayService.UserLinkSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth);
begin
  inherited UserLinkSuccess(UserDefineIO);
  (UserDefineIO as TPerUserPayRecvTunnel).PayService := Self;
  ((UserDefineIO as TPerUserPayRecvTunnel).SendTunnel as TPerUserPaySendTunnel).PayService := Self;
end;

procedure TPayService.UserOut(UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth);
begin
  inherited UserOut(UserDefineIO);
end;

constructor TPayService.Create(ARecvTunnel, ASendTunnel: TCommunicationFrameworkServer);
begin
  inherited Create(ARecvTunnel, ASendTunnel);
  FRecvTunnel.PeerClientUserDefineClass := TPerUserPayRecvTunnel;
  FSendTunnel.PeerClientUserDefineClass := TPerUserPaySendTunnel;

  SwitchAsMaxSecurity;

  LogicService := nil;
end;

destructor TPayService.Destroy;
begin
  inherited Destroy;
end;

procedure TPayService.RegisterCommand;
begin
  inherited RegisterCommand;
end;

procedure TPayService.UnRegisterCommand;
begin
  inherited UnRegisterCommand;
end;

constructor TPerUserLogicSendTunnel.Create(AOwner: TPeerClient);
begin
  inherited Create(AOwner);
end;

destructor TPerUserLogicSendTunnel.Destroy;
begin
  inherited Destroy;
end;

constructor TPerUserLogicRecvTunnel.Create(AOwner: TPeerClient);
begin
  inherited Create(AOwner);
  LastAccessHomeCli := 0;
  LastGenPlayerIDInHome := 0;
  DownloadDBClient := nil;
  UploadDBClient := nil;
  DisableHomeList := TListString.Create;
end;

destructor TPerUserLogicRecvTunnel.Destroy;
begin
  DisposeObject(DisableHomeList);
  inherited Destroy;
end;

procedure TLogicService.UserLoginFileDownloadComplete(const UserData: Pointer; const UserObject: TCoreClassObject; Stream: TCoreClassStream; const fileName: string);
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

procedure TLogicService.UserLoginQueryComplete(Sender: TStoreFileInfoList);
var
  SendTunnelID      : Cardinal;
  UserID, UserPasswd: string;

  LogicCli: TPerUserLogicRecvTunnel;
  p       : PGetFileUserData;
begin
  if RecvTunnel.Exists(Sender.PeerClientData) and (Sender.Count > 0) then
    begin
      LogicCli := Sender.PeerClientData.UserDefine as TPerUserLogicRecvTunnel;
      LogicCli.DownloadDBClient := Sender[Sender.Count - 1]^.client;
      LogicCli.UploadDBClient := Sender[0]^.client;

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

          LogicCli.DownloadDBClient.GetFileM(Sender.fileName, FPublicPath, p, nil, UserLoginFileDownloadComplete);
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

procedure TLogicService.UserLockQueryComplete(Sender: TFileStoreUserStateList);
var
  i     : Integer;
  locked: Boolean;

  SendTunnelID      : Cardinal;
  UserID, UserPasswd: string;

  FileInfoLst: TStoreFileInfoList;

  LogicCli: TPerUserLogicRecvTunnel;
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
          FileInfoLst := LogicForm.FFileStoreClientPool.QueryDBFile(UserID + '.loginPackage', UserLoginQueryComplete);
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

procedure TLogicService.Command_UserLogin(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
var
  SendTunnelID      : Cardinal;
  UserID, UserPasswd: string;
  UserStateLst      : TFileStoreUserStateList;
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

  UserStateLst := LogicForm.FFileStoreClientPool.QueryUserIsLock(UserID, UserLockQueryComplete);
  UserStateLst.PeerClientData := Sender;
  UserStateLst.InData := Sender.InDataFrame;
  UserStateLst.OutData := Sender.OutDataFrame;

  // 暂停反馈
  Sender.PauseResultSend;
end;

procedure TLogicService.UserRegistedSuccess(UserID: string);
var
  fn: string;
begin
  // save default user portrait
  fn := umlCombineFileName(GetUserPath(UserID), 'portrait.png');
  PortraitStream.SaveToFile(fn);
end;

procedure TLogicService.UserLinkSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel);
begin
  inherited UserLinkSuccess(UserDefineIO);
end;

procedure TLogicService.UserLoginSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel);
var
  LogicCli: TPerUserLogicRecvTunnel;
begin
  LogicCli := UserDefineIO as TPerUserLogicRecvTunnel;
  try
      LogicForm.FDataStoreClient.PostUserLoginInfo(LogicCli.UserID, string(LogicCli.UserConfigFile.Hit['UserInfo', 'Alias']),
      Format('%s (%s)登录', [string(LogicCli.UserConfigFile.Hit['UserInfo', 'Alias']), UserDefineIO.Owner.GetPeerIP]));
  except
  end;

  if (LogicCli.DownloadDBClient <> nil) then
      LogicCli.DownloadDBClient.UserLock(LogicCli.UserID);
end;

procedure TLogicService.UserOut(UserDefineIO: TPeerClientUserDefineForRecvTunnel);
var
  LogicCli: TPerUserLogicRecvTunnel;
  sendDE  : TDataFrameEngine;
  ms      : TMemoryStream64;
begin
  LogicCli := UserDefineIO as TPerUserLogicRecvTunnel;

  try
      LogicForm.FDataStoreClient.PostUserLoginInfo(LogicCli.UserID, string(LogicCli.UserConfigFile.Hit['UserInfo', 'Alias']),
      Format('%s (%s) 离线', [string(LogicCli.UserConfigFile.Hit['UserInfo', 'Alias']), UserDefineIO.Owner.GetPeerIP]));
  except
  end;

  if (LogicCli.UploadDBClient <> nil) and (LogicCli.LinkOk) then
    begin
      ms := TMemoryStream64.Create;
      if PackUserAsStream(LogicCli.UserID, ms) then
          LogicCli.UploadDBClient.PostFile(LogicCli.UserID + '.loginPackage', ms, True)
      else
          DisposeObject(ms);
    end;

  if LogicCli.DownloadDBClient <> nil then
      LogicCli.DownloadDBClient.UserUnLock(LogicCli.UserID);

  inherited UserOut(UserDefineIO);
end;

procedure TLogicService.MD5CacheDataNotifyProc(p: Pointer);
begin
  dispose(UnicodeMixedLib.PMD5(p));
end;

procedure TLogicService.UserRegQueryComplete(Sender: TStoreFileInfoList);
var
  LogicCli: TPerUserLogicRecvTunnel;
  ms      : TMemoryStream64;
begin
  if RecvTunnel.Exists(Sender.PeerClientData) and (Sender.Count > 0) then
    begin
      LogicCli := Sender.PeerClientData.UserDefine as TPerUserLogicRecvTunnel;
      LogicCli.DownloadDBClient := Sender[Sender.Count - 1].client;
      LogicCli.UploadDBClient := Sender[0].client;

      if Sender[Sender.Count - 1].FileLastTime <> 0 then
        begin
          (Sender.OutData.Data[0] as TDataFrameByte).Buffer := 0;
        end
      else
        begin
          ms := TMemoryStream64.Create;
          if PackUserAsStream(Sender.OutData.ReadString(1), ms) then
              LogicCli.UploadDBClient.PostFile(umlGetFileName(Sender.fileName), ms, True)
          else
              DisposeObject(ms);
        end;
      Sender.PeerClientData.ContinueResultSend;
    end;
end;

procedure TLogicService.UserRegGuestUserQueryComplete(Sender: TStoreFileInfoList);
var
  LogicCli: TPerUserLogicRecvTunnel;
  i, j    : Integer;

  UserID: string;
  l     : TStoreFileInfoList;
  te    : TSectionTextData;
  ms    : TMemoryStream64;
begin
  if RecvTunnel.Exists(Sender.PeerClientData) and (Sender.Count > 0) then
    begin
      LogicCli := Sender.PeerClientData.UserDefine as TPerUserLogicRecvTunnel;

      for i := 0 to Sender.Count - 1 do
        if Sender[i]^.FileLastTime <> 0 then
          begin
            // 用户已经存在，轮询注册
            j := Sender.SerialNo + 1;
            repeat
              UserID := Format('guest%.5d', [j]);
              inc(j);
            until not ExistsUser(UserID);

            l := LogicForm.FFileStoreClientPool.QueryDBFile(UserID + '.loginPackage', UserRegGuestUserQueryComplete);
            l.PeerClientData := Sender.PeerClientData;
            l.InData := Sender.InData;
            l.OutData := Sender.OutData;
            l.SerialNo := j - 1;
            exit;
          end;

      lastRegUserSerialNo := Sender.SerialNo + 1;

      // 完成注册，创建用户基本信息
      UserID := Format('guest%.5d', [Sender.SerialNo]);
      te := TSectionTextData.Create;
      te.Hit['UserInfo', 'Alias'] := Format('游客%.5d', [Sender.SerialNo]);

      if RegUser(UserID, UserID, te) then
        begin
          ms := TMemoryStream64.Create;
          if PackUserAsStream(UserID, ms) then
              Sender[Sender.Count - 1]^.client.PostFile(UserID + '.loginPackage', ms, True)
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

          l := LogicForm.FFileStoreClientPool.QueryDBFile(UserID + '.loginPackage', UserRegGuestUserQueryComplete);
          l.PeerClientData := Sender.PeerClientData;
          l.InData := Sender.InData;
          l.OutData := Sender.OutData;
          l.SerialNo := j - 1;
        end;
      DisposeObject(te);
    end;
end;

procedure TLogicService.Command_RegGuestUser(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
var
  UserID: string;
  j     : Integer;
  l     : TStoreFileInfoList;
begin
  j := lastRegUserSerialNo;
  repeat
    UserID := Format('guest%.5d', [j]);
    inc(j);
  until not ExistsUser(UserID);

  l := LogicForm.FFileStoreClientPool.QueryDBFile(UserID + '.loginPackage', UserRegGuestUserQueryComplete);
  l.PeerClientData := Sender;
  l.InData := Sender.InDataFrame;
  l.OutData := Sender.OutDataFrame;
  l.SerialNo := j - 1;

  // 暂停反馈
  Sender.PauseResultSend;
end;

procedure TLogicService.Command_GetLogicFileList(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
var
  itmSR   : TItemSearch;
  LogicCli: TPerUserLogicRecvTunnel;
begin
  LogicCli := Sender.UserDefine as TPerUserLogicRecvTunnel;
  if not LogicCli.LinkOk then
      exit;

  if LogicFileDB.ItemFindFirst('/LogicFile', '', itmSR) then
    begin
      repeat
          OutData.WriteString(itmSR.Name.Text);
      until not LogicFileDB.ItemFindNext(itmSR);
    end;
end;

procedure TLogicService.Command_GetLogicFileMD5(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
var
  fn      : string;
  md5Ptr  : UnicodeMixedLib.PMD5;
  itm     : TItemStream;
  LogicCli: TPerUserLogicRecvTunnel;
begin
  LogicCli := Sender.UserDefine as TPerUserLogicRecvTunnel;
  if not LogicCli.LinkOk then
      exit;

  fn := InData.Reader.ReadString;

  if FileMD5Cache.Exists(fn) then
    begin
      md5Ptr := UnicodeMixedLib.PMD5(FileMD5Cache[fn]);
      OutData.WriteMD5(md5Ptr^);
      exit;
    end;

  if not LogicFileDB.ItemExists('/LogicFile', fn) then
    begin
      Sender.Print('no Exists Logic File:%s' + fn);
      exit;
    end;
  itm := TItemStream.Create(LogicFileDB, '/LogicFile', fn);
  new(md5Ptr);
  md5Ptr^ := umlStreamMD5(itm);
  OutData.WriteMD5(md5Ptr^);
  FileMD5Cache.Add(fn, md5Ptr);
  DisposeObject(itm);
end;

procedure TLogicService.Command_GetLogicFile(Sender: TPeerClient; InData: TDataFrameEngine);
var
  fn         : string;
  callBackPtr: UInt64;
  itm        : TItemStream;
  LogicCli   : TPerUserLogicRecvTunnel;
  sendDE     : TDataFrameEngine;
begin
  LogicCli := Sender.UserDefine as TPerUserLogicRecvTunnel;
  if not LogicCli.LinkOk then
      exit;

  fn := InData.Reader.ReadString;
  callBackPtr := InData.Reader.ReadPointer;

  if not LogicFileDB.ItemExists('/LogicFile', fn) then
    begin
      Sender.Print('no Exists Logic File:%s' + fn);
      exit;
    end;

  itm := TItemStream.Create(LogicFileDB, '/LogicFile', fn);

  PostBatchStream(LogicCli.SendTunnel.Owner, itm, True);

  sendDE := TDataFrameEngine.Create;
  sendDE.WritePointer(callBackPtr);
  LogicCli.SendTunnel.Owner.SendDirectStreamCmd('SendLogicFileCompleted', sendDE);
  DisposeObject(sendDE);

  ClearBatchStream(LogicCli.SendTunnel.Owner);
end;

procedure TLogicService.Command_GetAdvertisementFileList(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
var
  itmSR   : TItemSearch;
  LogicCli: TPerUserLogicRecvTunnel;
begin
  LogicCli := Sender.UserDefine as TPerUserLogicRecvTunnel;
  if not LogicCli.LinkOk then
      exit;

  if LogicFileDB.ItemFindFirst('/Advertisement', '', itmSR) then
    begin
      repeat
          OutData.WriteString(itmSR.Name.Text);
      until not LogicFileDB.ItemFindNext(itmSR);
    end;
end;

procedure TLogicService.Command_GetAdvertisementFileMD5(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
var
  fn      : string;
  md5Ptr  : UnicodeMixedLib.PMD5;
  itm     : TItemStream;
  LogicCli: TPerUserLogicRecvTunnel;
begin
  LogicCli := Sender.UserDefine as TPerUserLogicRecvTunnel;
  if not LogicCli.LinkOk then
      exit;

  fn := InData.Reader.ReadString;

  if FileMD5Cache.Exists(fn) then
    begin
      md5Ptr := UnicodeMixedLib.PMD5(FileMD5Cache[fn]);
      OutData.WriteMD5(md5Ptr^);
      exit;
    end;

  if not LogicFileDB.ItemExists('/Advertisement', fn) then
    begin
      Sender.Print('no Exists Advertisement File:%s' + fn);
      exit;
    end;
  itm := TItemStream.Create(LogicFileDB, '/Advertisement', fn);
  new(md5Ptr);
  md5Ptr^ := umlStreamMD5(itm);
  OutData.WriteMD5(md5Ptr^);
  FileMD5Cache.Add(fn, md5Ptr);
  DisposeObject(itm);
end;

procedure TLogicService.Command_GetAdvertisementFile(Sender: TPeerClient; InData: TDataFrameEngine);
var
  fn         : string;
  callBackPtr: UInt64;
  itm        : TItemStream;
  LogicCli   : TPerUserLogicRecvTunnel;
  sendDE     : TDataFrameEngine;
begin
  LogicCli := Sender.UserDefine as TPerUserLogicRecvTunnel;
  if not LogicCli.LinkOk then
      exit;

  fn := InData.Reader.ReadString;
  callBackPtr := InData.Reader.ReadPointer;

  if not LogicFileDB.ItemExists('/Advertisement', fn) then
    begin
      Sender.Print('no Exists Advertisement File:%s' + fn);
      exit;
    end;

  itm := TItemStream.Create(LogicFileDB, '/Advertisement', fn);

  PostBatchStream(LogicCli.SendTunnel.Owner, itm, True);

  sendDE := TDataFrameEngine.Create;
  sendDE.WritePointer(callBackPtr);
  LogicCli.SendTunnel.Owner.SendDirectStreamCmd('SendAdvertisementFileCompleted', sendDE);
  DisposeObject(sendDE);

  ClearBatchStream(LogicCli.SendTunnel.Owner);
end;

procedure TLogicService.GetUserInfoFileDownloadComplete(const UserData: Pointer; const UserObject: TCoreClassObject; Stream: TCoreClassStream; const fileName: string);
var
  p : PGetFileUserData;
  m : TMemoryStream64;
  te: TSectionTextData;
begin
  p := UserData;
  if RecvTunnel.Exists(p^.client) then
    begin
      m := TMemoryStream64.Create;

      Stream.Position := 0;
      if ExtractFileInDB(Stream, '/', 'User.Config', m) then
        begin
          te := TSectionTextData.Create;
          m.Position := 0;
          te.LoadFromStream(m);
          te.VariantList['UserInfo'].Delete('Password');
          p^.OutData.WriteBool(True);
          p^.OutData.WriteSectionText(te);
          DisposeObject(te);
        end
      else
          p^.OutData.WriteBool(False);

      DisposeObject(m);
      p^.client.ContinueResultSend;
    end;
  dispose(p);
  umlDeleteFile(fileName);
end;

procedure TLogicService.GetUserInfoQueryComplete(Sender: TStoreFileInfoList);
var
  cli: TFileStoreService_DoubleTunnelClient;
  p  : PGetFileUserData;
begin
  if RecvTunnel.Exists(Sender.PeerClientData) and (Sender.Count > 0) then
    begin
      cli := Sender[Sender.Count - 1]^.client;

      if Sender[Sender.Count - 1]^.FileLastTime <> 0 then
        begin
          new(p);
          p^.client := Sender.PeerClientData;
          p^.InData := Sender.InData;
          p^.OutData := Sender.OutData;

          cli.GetFileM(Sender.fileName, FPublicPath, p, nil, GetUserInfoFileDownloadComplete);
        end
      else
        begin
          Sender.OutData.WriteBool(False);
          Sender.PeerClientData.ContinueResultSend;
        end;
    end;
end;

procedure TLogicService.Command_GetUserInfo(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
var
  UserID  : string;
  cli     : TPeerClientUserDefineForRecvTunnel;
  l       : TStoreFileInfoList;
  LogicCli: TPerUserLogicRecvTunnel;
  te      : TSectionTextData;
begin
  LogicCli := Sender.UserDefine as TPerUserLogicRecvTunnel;
  if not LogicCli.LinkOk then
      exit;

  UserID := InData.Reader.ReadString;
  cli := GetUserDefineIO(UserID);

  if cli <> nil then
    begin
      OutData.WriteBool(True);
      te := TSectionTextData.Create;
      te.Assign(cli.UserConfigFile);
      te.VariantList['UserInfo'].Delete('Password');
      OutData.WriteSectionText(te);
      DisposeObject(te);
    end
  else if ExistsUser(UserID) and umlFileExists(GetUserFile(UserID, 'User.Config')) then
    begin
      OutData.WriteBool(True);
      te := TSectionTextData.Create;
      te.LoadFromFile(GetUserFile(UserID, 'User.Config'));
      te.VariantList['UserInfo'].Delete('Password');
      OutData.WriteSectionText(te);
      DisposeObject(te);
    end
  else
    begin
      l := LogicForm.FFileStoreClientPool.QueryDBFile(UserID + '.loginPackage', GetUserInfoQueryComplete);
      l.PeerClientData := Sender;
      l.InData := Sender.InDataFrame;
      l.OutData := Sender.OutDataFrame;
      Sender.PauseResultSend;
    end;
end;

procedure TLogicService.Command_ChangeAlias(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
var
  LogicCli: TPerUserLogicRecvTunnel;
  alias   : string;
begin
  LogicCli := Sender.UserDefine as TPerUserLogicRecvTunnel;
  if not LogicCli.LinkOk then
      exit;

  alias := InData.Reader.ReadString;

  LogicCli.UserConfigFile.Hit['UserInfo', 'Alias'] := alias;

  OutData.WriteBool(True);

  UserChange(LogicCli.UserID);
end;

constructor TLogicService.Create(ARecvTunnel, ASendTunnel: TCommunicationFrameworkServer);
var
  itm: TItemStream;
begin
  inherited Create(ARecvTunnel, ASendTunnel);

  LogicFileDB := TObjectDataManagerOfCache.CreateAsStream(umlGetResourceStream('LogicFile.ox'), '', 0, True, False, True);
  FileMD5Cache := THashList.Create;
  FileMD5Cache.AutoFreeData := True;
  FileMD5Cache.OnFreePtr := MD5CacheDataNotifyProc;

  PortraitStream := TMemoryStream64.Create;

  if not LogicFileDB.ItemExists('/Portrait', 'defaultPortrait.png') then
    begin
      DoStatus('no default Portrait:', ['defaultPortrait.png']);
    end;
  itm := TItemStream.Create(LogicFileDB, '/Portrait', 'defaultPortrait.png');
  PortraitStream.CopyFrom(itm, itm.Size);
  DisposeObject(itm);

  FRecvTunnel.PeerClientUserDefineClass := TPerUserLogicRecvTunnel;
  FSendTunnel.PeerClientUserDefineClass := TPerUserLogicSendTunnel;
  LogicForm := nil;

  CanSaveUserInfo := True;
  LoadUserDB;
  lastRegUserSerialNo := 1;

  SwitchAsDefaultPerformance;
end;

destructor TLogicService.Destroy;
begin
  DisposeObject(PortraitStream);
  DisposeObject(LogicFileDB);
  DisposeObject(FileMD5Cache);
  inherited Destroy;
end;

procedure TLogicService.RegisterCommand;
begin
  inherited RegisterCommand;
  FRecvTunnel.RegisterStream('RegGuestUser').OnExecute := Command_RegGuestUser;

  FRecvTunnel.RegisterStream('GetLogicFileList').OnExecute := Command_GetLogicFileList;
  FRecvTunnel.RegisterStream('GetLogicFileMD5').OnExecute := Command_GetLogicFileMD5;
  FRecvTunnel.RegisterDirectStream('GetLogicFile').OnExecute := Command_GetLogicFile;

  FRecvTunnel.RegisterStream('GetAdvertisementFileList').OnExecute := Command_GetAdvertisementFileList;
  FRecvTunnel.RegisterStream('GetAdvertisementFileMD5').OnExecute := Command_GetAdvertisementFileMD5;
  FRecvTunnel.RegisterDirectStream('GetAdvertisementFile').OnExecute := Command_GetAdvertisementFile;

  FRecvTunnel.RegisterStream('GetUserInfo').OnExecute := Command_GetUserInfo;
  FRecvTunnel.RegisterStream('ChangeAlias').OnExecute := Command_ChangeAlias;
end;

procedure TLogicService.UnRegisterCommand;
begin
  inherited UnRegisterCommand;
  FRecvTunnel.DeleteRegistedCMD('RegGuestUser');

  FRecvTunnel.DeleteRegistedCMD('GetLogicFileList');
  FRecvTunnel.DeleteRegistedCMD('GetLogicFileMD5');
  FRecvTunnel.DeleteRegistedCMD('GetLogicFile');

  FRecvTunnel.DeleteRegistedCMD('GetAdvertisementFileList');
  FRecvTunnel.DeleteRegistedCMD('GetAdvertisementFileMD5');
  FRecvTunnel.DeleteRegistedCMD('GetAdvertisementFile');

  FRecvTunnel.DeleteRegistedCMD('GetUserInfo');
  FRecvTunnel.DeleteRegistedCMD('ChangeAlias');
end;

procedure TLogicService.UserChange(UserID: string);
begin
  SendTunnel.BroadcastDirectConsoleCmd('UserChange', UserID);
end;

procedure TLogicServerForm.StartServiceButtonClick(Sender: TObject);
begin
  StartService;
end;

procedure TLogicServerForm.StopServiceButtonClick(Sender: TObject);
begin
  StopService;
end;

procedure TLogicServerForm.AppEventsException(Sender: TObject; E: Exception);
begin
  DoStatus(E.ToString);
end;

procedure TLogicServerForm.connectButtonClick(Sender: TObject);
begin
  if ShowAndConnectManagerServer(FManagerClients, umlStrToInt(SendPortEdit.Text, cLogicService_SendPort), umlStrToInt(RecvPortEdit.Text, cLogicService_RecvPort), TServerType.stLogic) then
    begin
      // reg pay service
      FManagerClients.BuildClientAndConnect(serverType2Str(TServerType.stPayment),
        FManagerClients.LastManagerServerAddr,
        FManagerClients.LastRegAddr, FManagerClients.LastManServRecvPort, FManagerClients.LastManServSendPort,
        umlStrToInt(PaySendPortEdit.Text, cLogicService_PaySendPort), umlStrToInt(PayRecvPortEdit.Text, cLogicService_PayRecvPort), TServerType.stPayment);
    end;
end;

procedure TLogicServerForm.RefreshServerListButtonClick(Sender: TObject);
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

  PrintServerState('Service Statistics', [FPayRecvTunnel, FPaySendTunnel, FLogicRecvTunnel, FLogicSendTunnel]);

  PrintServerCMDStatistics('Command Statistics', [FPayRecvTunnel, FPaySendTunnel, FLogicRecvTunnel, FLogicSendTunnel]);

  TreeView.Items.EndUpdate;
  DisposeObject(ns);
end;

procedure TLogicServerForm.ProgressTimerTimer(Sender: TObject);
begin
  try
    FPayService.Progress;
    FLogicService.Progress;
    FManagerClients.Progress;
    FFileStoreClientPool.Progress;
    FDataStoreClient.Progress;
    ProcessICSMessages;
  except
  end;
end;

procedure TLogicServerForm.AntiIDLETimerTimer(Sender: TObject);
begin
  try
    if Memo.Lines.Count > 5000 then
        Memo.Clear;

    FManagerClients.AntiIdle(FLogicRecvTunnel.Count + FLogicSendTunnel.Count);
    FFileStoreClientPool.AntiIdle(FLogicRecvTunnel.Count + FLogicSendTunnel.Count);
    Caption := Format('Logic service...(user:%d)', [FLogicService.TotalLinkCount]);
  except
  end;
end;

procedure TLogicServerForm.DoStatusNear(AText: string; const ID: Integer);
begin
  if StatusCheckBox.Checked then
      Memo.Lines.Append(AText);
  FDataStoreClient.PostLogInfo('Logic', AText);
end;

procedure TLogicServerForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  StopService;
  Action := caFree;
end;

function TLogicServerForm.GetPathTreeNode(_Value, _Split: string; _TreeView: TTreeView; _RN: TTreeNode): TTreeNode;
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

procedure TLogicServerForm.ServerConfigChange(Sender: TServerManager_Client; ConfigData: TSectionTextData);
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
      if vl.GetDefaultValue('Type', TServerType.stUnknow) = TServerType.stFileStore then
        begin
          DBServAddr := vl.GetDefaultValue('Host', '');
          DBCliSendPort := vl.GetDefaultValue('SendPort', cFileStore_SendPort);
          DBCliRecvPort := vl.GetDefaultValue('RecvPort', cFileStore_RecvPort);
          RegAddr := FManagerClients.LastRegAddr;
          RegRecvPort := FLogicSendTunnel.BindPort;
          RegSendPort := FLogicRecvTunnel.BindPort;

          FFileStoreClientPool.BuildClientAndConnect(DBServAddr, RegAddr, DBCliRecvPort, DBCliSendPort, RegRecvPort, RegSendPort);
        end;

      if not FDataStoreClient.Connected then
        if vl.GetDefaultValue('Type', TServerType.stUnknow) = TServerType.stDataStore then
          begin
            DataStoreServAddr := vl.GetDefaultValue('Host', '');
            DataStoreCliSendPort := vl.GetDefaultValue('SendPort', cFileStore_SendPort);
            DataStoreCliRecvPort := vl.GetDefaultValue('RecvPort', cFileStore_RecvPort);

            FDataStoreClient.Connect(DataStoreServAddr, DataStoreCliRecvPort, DataStoreCliSendPort);
          end;
    end;
  DisposeObject(ns);
end;

procedure TLogicServerForm.ServerOffline(Sender: TServerManager_Client; RegAddr: string; ServerType: TServerType);
begin
end;

procedure TLogicServerForm.PostExecute_DelayStartService(Sender: TNPostExecute);
begin
  StartService;
end;

procedure TLogicServerForm.PostExecute_DelayRegService(Sender: TNPostExecute);
begin
  if AutoConnectManagerServer(FManagerClients,
    Sender.Data3, Sender.Data4, umlStrToInt(SendPortEdit.Text, cLogicService_SendPort), umlStrToInt(RecvPortEdit.Text, cLogicService_RecvPort), TServerType.stLogic) then
    begin
      // reg pay service
      FManagerClients.BuildClientAndConnect(serverType2Str(TServerType.stPayment),
        FManagerClients.LastManagerServerAddr,
        FManagerClients.LastRegAddr, FManagerClients.LastManServRecvPort, FManagerClients.LastManServSendPort,
        umlStrToInt(PaySendPortEdit.Text, cLogicService_PaySendPort), umlStrToInt(PayRecvPortEdit.Text, cLogicService_PayRecvPort), TServerType.stPayment);
    end;
end;

constructor TLogicServerForm.Create(AOwner: TComponent);
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

  FPayRecvTunnel := TCommunicationFramework_Server_CrossSocket.Create;
  FPayRecvTunnel.IdleTimeout := 60 * 1000;
  FPayRecvTunnel.PrintParams['AntiIdle'] := False;
  FPaySendTunnel := TCommunicationFramework_Server_CrossSocket.Create;

  FPayService := TPayService.Create(FPayRecvTunnel, FPaySendTunnel);
  FPayService.RegisterCommand;

  FLogicRecvTunnel := TCommunicationFramework_Server_CrossSocket.Create;
  FLogicRecvTunnel.IdleTimeout := 5 * 60 * 1000;
  FLogicRecvTunnel.PrintParams['AntiIdle'] := False;
  FLogicSendTunnel := TCommunicationFramework_Server_CrossSocket.Create;

  FLogicService := TLogicService.Create(FLogicRecvTunnel, FLogicSendTunnel);
  FLogicService.RegisterCommand;

  FLogicService.LogicForm := Self;

  FManagerClients := TServerManager_ClientPool.Create(TCommunicationFramework_Client_CrossSocket, Self);

  FFileStoreClientPool := TFileStoreService_DoubleTunnelClientPool.Create(Self);

  FDataStoreClient := TDataStore_DoubleTunnelClient.Create(TCommunicationFramework_Client_CrossSocket);
  FDataStoreClient.RegisterCommand;

  Memo.Lines.Add(WSAInfo);
  Memo.Lines.Add(Format('LogicService Root directory %s', [FLogicService.RootPath]));

  RecvPortEdit.Text := IntToStr(cLogicService_RecvPort);
  SendPortEdit.Text := IntToStr(cLogicService_SendPort);

  PayRecvPortEdit.Text := IntToStr(cLogicService_PayRecvPort);
  PaySendPortEdit.Text := IntToStr(cLogicService_PaySendPort);

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

            if umlMultipleMatch(['PayRecv:*', 'PayR:*', 'PayReceive:*', '-PayR:*', '-PayRecv:*', '-PayReceive:*'], p1) then
              begin
                p2 := umlDeleteFirstStr(p1, ':');
                if umlIsNumber(p2) then
                    PayRecvPortEdit.Text := p2;
              end;

            if umlMultipleMatch(['PaySend:*', 'PayS:*', '-PayS:*', '-PaySend:*'], p1) then
              begin
                p2 := umlDeleteFirstStr(p1, ':');
                if umlIsNumber(p2) then
                    PaySendPortEdit.Text := p2;
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
      with FLogicService.ProgressEngine.PostExecute(delayStartServiceTime) do
          OnExecuteMethod := PostExecute_DelayStartService;
    end;

  if delayReg then
    begin
      with FLogicService.ProgressEngine.PostExecute(delayRegTime) do
        begin
          Data3 := ManServAddr;
          Data4 := RegAddr;
          OnExecuteMethod := PostExecute_DelayRegService;
        end;
    end;

  DoStatus('');
end;

destructor TLogicServerForm.Destroy;
begin
  DisposeObject(FPayRecvTunnel);
  DisposeObject(FPaySendTunnel);
  DisposeObject(FPayService);

  DisposeObject(FLogicRecvTunnel);
  DisposeObject(FLogicSendTunnel);
  DisposeObject(FLogicService);

  DisposeObject(FManagerClients);
  DisposeObject(FFileStoreClientPool);
  DisposeObject(FDataStoreClient);

  DeleteDoStatusHook(Self);
  try
      inherited Destroy;
  except
  end;
end;

procedure TLogicServerForm.StartService;
begin
  StopService;

  if FPayRecvTunnel.StartService(BindIPEdit.Text, umlStrToInt(PayRecvPortEdit.Text, cLogicService_PayRecvPort)) then
      DoStatus('Pay Receive tunnel ready Ok! bind:%s port:%s', [TranslateBindAddr(BindIPEdit.Text), PayRecvPortEdit.Text])
  else
      MessageDlg(Format('Pay Receive tunnel Failed! bind:%s port:%s', [TranslateBindAddr(BindIPEdit.Text), PayRecvPortEdit.Text]),
      mtError, [mbYes], 0);

  if FPaySendTunnel.StartService(BindIPEdit.Text, umlStrToInt(PaySendPortEdit.Text, cLogicService_PaySendPort)) then
      DoStatus('Pay Send tunnel ready Ok! bind:%s port:%s', [TranslateBindAddr(BindIPEdit.Text), PaySendPortEdit.Text])
  else
      MessageDlg(Format('Pay Send tunnel Failed! bind:%s port:%s', [TranslateBindAddr(BindIPEdit.Text), PaySendPortEdit.Text]),
      mtError, [mbYes], 0);
  FPayRecvTunnel.IDCounter := 51;

  if FLogicRecvTunnel.StartService(BindIPEdit.Text, umlStrToInt(RecvPortEdit.Text, cLogicService_RecvPort)) then
      DoStatus('Logic Receive tunnel ready Ok! bind:%s port:%s', [TranslateBindAddr(BindIPEdit.Text), RecvPortEdit.Text])
  else
      MessageDlg(Format('Logic Receive tunnel Failed! bind:%s port:%s', [TranslateBindAddr(BindIPEdit.Text), RecvPortEdit.Text]),
      mtError, [mbYes], 0);

  if FLogicSendTunnel.StartService(BindIPEdit.Text, umlStrToInt(SendPortEdit.Text, cLogicService_SendPort)) then
      DoStatus('Logic Send tunnel ready Ok! bind:%s port:%s', [TranslateBindAddr(BindIPEdit.Text), SendPortEdit.Text])
  else
      MessageDlg(Format('Logic Send tunnel Failed! bind:%s port:%s', [TranslateBindAddr(BindIPEdit.Text), SendPortEdit.Text]),
      mtError, [mbYes], 0);

  FLogicRecvTunnel.IDCounter := 10;
end;

procedure TLogicServerForm.StopService;
begin
  try
    FPayRecvTunnel.StopService;
    FPaySendTunnel.StopService;

    FLogicRecvTunnel.StopService;
    FLogicSendTunnel.StopService;

    FManagerClients.Clear;
    FFileStoreClientPool.Clear;

    FDataStoreClient.Disconnect;
  except
  end;
end;

initialization


end.
