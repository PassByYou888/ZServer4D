{ ****************************************************************************** }
{ * double tunnel IO framework(incl Auth and File service)                     * }
{ * written by QQ 600585@qq.com                                                * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
(* https://github.com/PassByYou888/ZServer4D *)
{ ****************************************************************************** }

unit CommunicationFrameworkDoubleTunnelIO;

interface

{$I zDefine.inc}


uses CoreClasses,
  ListEngine, UnicodeMixedLib,
  DataFrameEngine, MemoryStream64, CommunicationFramework, TextDataEngine,
  DoStatusIO, Cadencer, NotifyObjectBase, DBCompressPackageForFile,
  ObjectDataManager, CoreCipher, PascalStrings;

type
  TCommunicationFramework_DoubleTunnelService = class;
  TPeerClientUserDefineForRecvTunnel          = class;

  TPeerClientUserDefineForSendTunnel = class(TPeerClientUserDefine)
  public
    RecvTunnel         : TPeerClientUserDefineForRecvTunnel;
    RecvTunnelID       : Cardinal;
    DoubleTunnelService: TCommunicationFramework_DoubleTunnelService;

    constructor Create(AOwner: TPeerClient); override;
    destructor Destroy; override;
  end;

  TPeerClientUserDefineForRecvTunnel = class(TPeerClientUserDefine)
  public
    SendTunnel             : TPeerClientUserDefineForSendTunnel;
    SendTunnelID           : Cardinal;
    UserFlag, UserID       : string;
    UserPath               : string;
    UserConfigFile         : TSectionTextData;
    DoubleTunnelService    : TCommunicationFramework_DoubleTunnelService;
    UserDBIntf             : THashVariantList;
    LoginSuccessed         : Boolean;
    FCurrentFileStream     : TCoreClassStream;
    FCurrentReceiveFileName: string;

    WaitLink      : Boolean;
    WaitLinkSendID: Cardinal;

    constructor Create(AOwner: TPeerClient); override;
    destructor Destroy; override;

    function MakeFilePath(fn: string): string;
    function GetUserID: string;

    procedure SaveConfigFile; virtual;

    function LinkOK: Boolean;
  end;

  TFileComplete = procedure(const UserData: Pointer; const UserObject: TCoreClassObject; Stream: TCoreClassStream; const fileName: string) of object;

  PRemoteFileBackcall = ^TRemoteFileBackcall;

  TRemoteFileBackcall = record
    UserData: Pointer;
    UserObject: TCoreClassObject;
    OnComplete: TFileComplete;
  end;

  TCommunicationFramework_DoubleTunnelService = class(TCoreClassInterfacedObject)
  protected
    FRecvTunnel, FSendTunnel: TCommunicationFrameworkServer;
    FRootPath, FPublicPath  : string;
    FUserDB                 : TSectionTextData;
    FCanRegisterNewUser     : Boolean;
    FCanSaveUserInfo        : Boolean;
    FCanStatus              : Boolean;
    FLoginUserList          : THashVariantList;
    FLoginUserDefineIOList  : THashObjectList;
    FCadencerEngine         : TCadencer;
    FProgressEngine         : TNProgressPost;
  protected
    procedure UserLogin(UserID, UserPasswd: string); virtual;
    procedure UserRegistedSuccess(UserID: string); virtual;
    procedure UserLoginSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel); virtual;
    procedure UserLinkSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel); virtual;
    procedure UserCreateDirectorySuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel; dn: string); virtual;
    procedure UserPostFileSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel; fn: string); virtual;
    procedure UserOut(UserDefineIO: TPeerClientUserDefineForRecvTunnel); virtual;
  protected
    // registed server command
    procedure Command_UserLogin(Sender: TPeerClient; InData, OutData: TDataFrameEngine); virtual;
    procedure Command_RegisterUser(Sender: TPeerClient; InData, OutData: TDataFrameEngine); virtual;
    procedure Command_TunnelLink(Sender: TPeerClient; InData, OutData: TDataFrameEngine); virtual;

    procedure Command_ChangePasswd(Sender: TPeerClient; InData, OutData: TDataFrameEngine); virtual;
    procedure Command_CustomNewUser(Sender: TPeerClient; InData, OutData: TDataFrameEngine); virtual;

    procedure Command_ProcessStoreQueueCMD(Sender: TPeerClient; InData: TDataFrameEngine); virtual;

    procedure Command_GetPublicFileList(Sender: TPeerClient; InData, OutData: TDataFrameEngine); virtual;
    procedure Command_GetPrivateFileList(Sender: TPeerClient; InData, OutData: TDataFrameEngine); virtual;
    procedure Command_GetPrivateDirectoryList(Sender: TPeerClient; InData, OutData: TDataFrameEngine); virtual;

    procedure Command_GetUserPrivateFileList(Sender: TPeerClient; InData, OutData: TDataFrameEngine); virtual;
    procedure Command_GetUserPrivateDirectoryList(Sender: TPeerClient; InData, OutData: TDataFrameEngine); virtual;

    procedure Command_CreatePrivateDirectory(Sender: TPeerClient; InData, OutData: TDataFrameEngine); virtual;

    procedure Command_GetPublicFile(Sender: TPeerClient; InData, OutData: TDataFrameEngine); virtual;
    procedure Command_GetPrivateFile(Sender: TPeerClient; InData, OutData: TDataFrameEngine); virtual;
    procedure Command_GetPrivateFileOfBatch(Sender: TPeerClient; InData: TDataFrameEngine); virtual;

    procedure Command_GetUserPrivateFile(Sender: TPeerClient; InData, OutData: TDataFrameEngine); virtual;
    procedure Command_GetUserPrivateFileOfBatch(Sender: TPeerClient; InData: TDataFrameEngine); virtual;

    procedure Command_PostPublicFileInfo(Sender: TPeerClient; InData: TDataFrameEngine); virtual;
    procedure Command_PostPrivateFileInfo(Sender: TPeerClient; InData: TDataFrameEngine); virtual;
    procedure Command_PostFile(Sender: TPeerClient; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64); virtual;
    procedure Command_PostFileOver(Sender: TPeerClient; InData: TDataFrameEngine); virtual;

    procedure Command_GetCurrentCadencer(Sender: TPeerClient; InData, OutData: TDataFrameEngine); virtual;
  public
    constructor Create(ARecvTunnel, ASendTunnel: TCommunicationFrameworkServer);
    destructor Destroy; override;

    procedure SwitchServiceAsMaxPerformance;
    procedure SwitchServiceAsMaxSafe;
    procedure SwitchServiceAsDefaultPerformance;

    procedure Progress; virtual;
    procedure CadencerProgress(Sender: TObject; const deltaTime, newTime: Double); virtual;

    // load USERDB
    // load need execute in rootpath set completed after
    procedure LoadUserDB;

    procedure SaveUserDB;

    function RegUser(AUserID, APasswd: string; AUserConfigFile: TSectionTextData): Boolean;
    function ExistsUser(AUserID: string): Boolean;
    function GetUserPath(AUserID: string): string;
    function GetUserFile(AUserID, AUserFileName: string): string;
    function GetUserDefineIO(AUserID: string): TPeerClientUserDefineForRecvTunnel;
    function UserOnline(AUserID: string): Boolean;

    function PackUserAsFile(AUserID, aPackFile: string): Boolean;
    function PackUserAsStream(AUserID: string; PackStream: TCoreClassStream): Boolean;
    function UnPackFileAsUser(aPackFile: string): Boolean;
    function UnPackStreamAsUser(aPackStream: TCoreClassStream): Boolean;

    // only work in direct command
    // if user online immediate execution
    // if user offline store to notify queue
    procedure PostStoreQueueCMD(ToUserID: string; cmd: string; InData: TDataFrameEngine);

    procedure RegisterCommand; virtual;
    procedure UnRegisterCommand; virtual;
    function MakeUserFlag: string;
    function GetUserDefineRecvTunnel(RecvCli: TPeerClient): TPeerClientUserDefineForRecvTunnel;

    function TotalLinkCount: Integer;

    property LoginUserList: THashVariantList read FLoginUserList;

    property CanRegisterNewUser: Boolean read FCanRegisterNewUser write FCanRegisterNewUser;
    property CanSaveUserInfo: Boolean read FCanSaveUserInfo write FCanSaveUserInfo;
    property CanStatus: Boolean read FCanStatus write FCanStatus;

    property RootPath: string read FRootPath write FRootPath;
    property PublicPath: string read FPublicPath write FPublicPath;

    property CadencerEngine: TCadencer read FCadencerEngine;
    property ProgressEngine: TNProgressPost read FProgressEngine;

    property RecvTunnel: TCommunicationFrameworkServer read FRecvTunnel;
    property SendTunnel: TCommunicationFrameworkServer read FSendTunnel;
  end;

  TCommunicationFramework_DoubleTunnelClient = class;

  TPeerClientUserDefineForDoubleTunnelClient = class(TPeerClientUserDefine)
  public
    Client: TCommunicationFramework_DoubleTunnelClient;

    constructor Create(AOwner: TPeerClient); override;
    destructor Destroy; override;
  end;

  TCommunicationFramework_DoubleTunnelClient = class(TCoreClassInterfacedObject, ICommunicationFrameworkClientInterface)
  protected
    FSendTunnel, FRecvTunnel     : TCommunicationFrameworkClient;
    FCurrentStream               : TCoreClassStream;
    FCurrentReceiveStreamFileName: string;
    FLinkOk                      : Boolean;
    FWaitCommandTimeout          : Cardinal;

    FRecvFileing       : Boolean;
    FRecvFileOfBatching: Boolean;
    FRecvFileName      : string;

    FCadencerEngine  : TCadencer;
    FLastCadencerTime: Double;
    FServerDelay     : Double;

    FProgressEngine: TNProgressPost;
  protected
    // registed client command
    procedure Command_FileInfo(Sender: TPeerClient; InData: TDataFrameEngine); virtual;
    procedure Command_PostFile(Sender: TPeerClient; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64); virtual;
    procedure Command_PostFileOver(Sender: TPeerClient; InData: TDataFrameEngine); virtual;
    procedure Command_PostFileOfBatchOver(Sender: TPeerClient; InData: TDataFrameEngine); virtual;

    procedure GetFile_StreamParamResult(Sender: TPeerClient; Param1: Pointer; Param2: TObject; InData, ResultData: TDataFrameEngine);
    procedure GetPrivateFile_StreamParamResult(Sender: TPeerClient; Param1: Pointer; Param2: TObject; InData, ResultData: TDataFrameEngine);
    procedure GetUserPrivateFile_StreamParamResult(Sender: TPeerClient; Param1: Pointer; Param2: TObject; InData, ResultData: TDataFrameEngine);

    // GetCurrentCadencer result proc
    procedure GetCurrentCadencer_StreamResult(Sender: TPeerClient; ResultData: TDataFrameEngine);
  protected
    // client notify interface
    procedure ClientConnected(Sender: TCommunicationFrameworkClient); virtual;
    procedure ClientDisconnect(Sender: TCommunicationFrameworkClient); virtual;
  public
    constructor Create(ARecvTunnel, ASendTunnel: TCommunicationFrameworkClient);
    destructor Destroy; override;

    function Connected: Boolean; virtual;

    procedure SwitchServiceAsMaxPerformance;
    procedure SwitchServiceAsMaxSafe;
    procedure SwitchServiceAsDefaultPerformance;

    procedure Progress; virtual;
    procedure CadencerProgress(Sender: TObject; const deltaTime, newTime: Double); virtual;

    // Block mode
    function UserLogin(UserID, Passwd: string): Boolean; overload; virtual;
    function RegisterUser(UserID, Passwd: string): Boolean; overload; virtual;
    function TunnelLink: Boolean; overload; virtual;

    // unblock mode
    {$IFNDEF FPC}
    procedure UserLogin(UserID, Passwd: string; OnProc: TStateProc); overload; virtual;
    procedure RegisterUser(UserID, Passwd: string; OnProc: TStateProc); overload; virtual;
    procedure TunnelLink(OnProc: TStateProc); overload; virtual;
    {$ENDIF}
    procedure SyncCadencer; virtual;

    // Block mode
    function ChnagePassword(oldPasswd, newPasswd: string): Boolean;
    function CustomNewUser(AUserID, APasswd: string; AUserConfigFile: TSectionTextData): Boolean;

    procedure ProcessStoreQueueCMD;

    procedure GetPublicFileList(Filter: string; lst: TCoreClassStrings);

    procedure GetPrivateFileList(Filter, directoryName: string; lst: TCoreClassStrings); overload;
    procedure GetPrivateFileList(Filter: string; lst: TCoreClassStrings); overload;

    procedure GetUserPrivateFileList(UserID, Filter, directoryName: string; lst: TCoreClassStrings); overload;
    procedure GetUserPrivateFileList(UserID, Filter: string; lst: TCoreClassStrings); overload;

    procedure GetPrivateDirectoryList(Filter, directoryName: string; lst: TCoreClassStrings); overload;
    procedure GetPrivateDirectoryList(Filter: string; lst: TCoreClassStrings); overload;

    procedure GetUserPrivateDirectoryList(UserID, Filter, directoryName: string; lst: TCoreClassStrings); overload;
    procedure GetUserPrivateDirectoryList(UserID, Filter: string; lst: TCoreClassStrings); overload;

    function CreatePrivateDirectory(directoryName: string): Boolean;

    function GetPublicFile(fileName, saveToPath: string): Boolean; overload;
    procedure GetPublicFile(fileName, saveToPath: string;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileComplete); overload;

    function GetPrivateFile(fileName, directoryName, saveToPath: string): Boolean; overload;
    function GetPrivateFile(fileName, saveToPath: string): Boolean; overload;
    procedure GetPrivateFile(fileName, directoryName, saveToPath: string;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileComplete); overload;

    procedure GetPrivateFileOfBatch(Filter, directoryName, saveToPath: string); overload;
    procedure GetPrivateFileOfBatch(Filter, directoryName, saveToPath: string;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnAllComplete: TFileComplete); overload;

    function GetUserPrivateFile(UserID, fileName, directoryName, saveToPath: string): Boolean; overload;
    function GetUserPrivateFile(UserID, fileName, saveToPath: string): Boolean; overload;
    procedure GetUserPrivateFile(UserID, fileName, directoryName, saveToPath: string;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileComplete); overload;

    procedure GetUserPrivateFileOfBatch(UserID, Filter, directoryName, saveToPath: string); overload;
    procedure GetUserPrivateFileOfBatch(UserID, Filter, directoryName, saveToPath: string;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnAllComplete: TFileComplete); overload;

    // UnBlock mode
    procedure PostFileToPublic(fileName: string);
    procedure PostFileToPrivate(fileName, directoryName: string); overload;
    procedure PostFileToPrivate(fileName: string); overload;
    procedure PostStreamToPrivate(FileFlagName, directoryName: string; Stream: TCoreClassStream; DoneFreeStream: Boolean);

    procedure RegisterCommand; virtual;
    procedure UnRegisterCommand; virtual;

    property LinkOK: Boolean read FLinkOk;
    property WaitCommandTimeout: Cardinal read FWaitCommandTimeout write FWaitCommandTimeout;
    property RecvFileing: Boolean read FRecvFileing;
    property RecvFileOfBatching: Boolean read FRecvFileOfBatching;
    property RecvFileName: string read FRecvFileName;

    property CadencerEngine: TCadencer read FCadencerEngine;
    property ServerDelay: Double read FServerDelay;
    property ProgressEngine: TNProgressPost read FProgressEngine;

    property RecvTunnel: TCommunicationFrameworkClient read FRecvTunnel;
    property SendTunnel: TCommunicationFrameworkClient read FSendTunnel;

    function RemoteInited: Boolean;
  end;

implementation

uses SysUtils;

constructor TPeerClientUserDefineForSendTunnel.Create(AOwner: TPeerClient);
begin
  inherited Create(AOwner);
  RecvTunnel := nil;
  RecvTunnelID := 0;
  DoubleTunnelService := nil;
end;

destructor TPeerClientUserDefineForSendTunnel.Destroy;
begin
  if (DoubleTunnelService <> nil) and (RecvTunnelID > 0) and (RecvTunnel <> nil) then
    begin
      if DoubleTunnelService.FRecvTunnel.Exists(RecvTunnelID) then
          DoubleTunnelService.FRecvTunnel.ClientFromID[RecvTunnelID].Disconnect;
    end;
  inherited Destroy;
end;

constructor TPeerClientUserDefineForRecvTunnel.Create(AOwner: TPeerClient);
begin
  inherited Create(AOwner);
  SendTunnel := nil;
  SendTunnelID := 0;
  UserFlag := '';
  UserID := '';
  UserPath := '';
  UserConfigFile := TSectionTextData.Create;
  DoubleTunnelService := nil;
  UserDBIntf := nil;
  LoginSuccessed := False;
  WaitLink := True;
  WaitLinkSendID := 0;
  FCurrentFileStream := nil;
  FCurrentReceiveFileName := '';
end;

destructor TPeerClientUserDefineForRecvTunnel.Destroy;
begin
  if LoginSuccessed then
    begin
      if (DoubleTunnelService <> nil) then
        begin
          SaveConfigFile;
          DoubleTunnelService.UserOut(Self);
        end;

      if (DoubleTunnelService <> nil) and (SendTunnelID > 0) and (SendTunnel <> nil) then
        begin
          if DoubleTunnelService.FSendTunnel.Exists(SendTunnelID) then
              DoubleTunnelService.FSendTunnel.ClientFromID[SendTunnelID].Disconnect;
        end;

      LockObject(DoubleTunnelService.FLoginUserList);
      try
          DoubleTunnelService.FLoginUserList.Delete(UserID);
      except
      end;
      UnLockObject(DoubleTunnelService.FLoginUserList);

      LockObject(DoubleTunnelService.FLoginUserDefineIOList);
      try
          DoubleTunnelService.FLoginUserDefineIOList.Delete(UserID);
      except
      end;
      UnLockObject(DoubleTunnelService.FLoginUserDefineIOList);
    end;

  try
      DisposeObject(UserConfigFile);
  except
  end;

  if FCurrentFileStream <> nil then
    begin
      DisposeObject(FCurrentFileStream);
      FCurrentFileStream := nil;
    end;

  inherited Destroy;
end;

function TPeerClientUserDefineForRecvTunnel.MakeFilePath(fn: string): string;
begin
  Result := umlCombinePath(UserPath, fn);
end;

function TPeerClientUserDefineForRecvTunnel.GetUserID: string;
begin
  Result := UserConfigFile.GetDefaultValue('UserInfo', 'UserID', '');
end;

procedure TPeerClientUserDefineForRecvTunnel.SaveConfigFile;
begin
  UserConfigFile.SaveToFile(MakeFilePath('User.Config'));
end;

function TPeerClientUserDefineForRecvTunnel.LinkOK: Boolean;
begin
  Result := SendTunnel <> nil;
end;

procedure TCommunicationFramework_DoubleTunnelService.UserLogin(UserID, UserPasswd: string);
begin
end;

procedure TCommunicationFramework_DoubleTunnelService.UserRegistedSuccess(UserID: string);
begin
end;

procedure TCommunicationFramework_DoubleTunnelService.UserLoginSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel);
begin
end;

procedure TCommunicationFramework_DoubleTunnelService.UserLinkSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel);
begin
end;

procedure TCommunicationFramework_DoubleTunnelService.UserCreateDirectorySuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel; dn: string);
begin
end;

procedure TCommunicationFramework_DoubleTunnelService.UserPostFileSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel; fn: string);
begin
end;

procedure TCommunicationFramework_DoubleTunnelService.UserOut(UserDefineIO: TPeerClientUserDefineForRecvTunnel);
begin
end;

procedure TCommunicationFramework_DoubleTunnelService.Command_UserLogin(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
var
  SendTunnelID      : Cardinal;
  UserID, UserPasswd: string;
  UserDefineIO      : TPeerClientUserDefineForRecvTunnel;
begin
  SendTunnelID := InData.Reader.ReadCardinal;
  UserID := InData.Reader.ReadString;
  UserPasswd := InData.Reader.ReadString;

  UserLogin(UserID, UserPasswd);

  UserDefineIO := GetUserDefineRecvTunnel(Sender);

  if not FSendTunnel.Exists(SendTunnelID) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(Format('send tunnel Illegal:%d', [SendTunnelID]));
      Exit;
    end;

  if not FUserDB.Exists(UserID) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(Format('user name Invalid:%s', [UserID]));
      Exit;
    end;

  LockObject(FLoginUserList);
  if FLoginUserList.Exists(UserID) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(Format('user already online:%s', [UserID]));
      UnLockObject(FLoginUserList);
      Exit;
    end;
  UnLockObject(FLoginUserList);

  try
    if not ComparePassword(TCipherStyle.csDES64, UserPasswd, string(FUserDB.GetDefaultValue(UserID, 'password', ''))) then
      begin
        OutData.WriteBool(False);
        OutData.WriteString(Format('password error', []));
        Exit;
      end;
  except
    OutData.WriteBool(False);
    OutData.WriteString(Format('password error', []));
    Exit;
  end;

  UserDefineIO.UserDBIntf := FUserDB.VariantList[UserID];
  UserDefineIO.UserDBIntf['LastLoginTime'] := DateTimeToStr(Now);
  UserDefineIO.UserFlag := UserDefineIO.UserDBIntf['UserFlag'];
  UserDefineIO.UserID := UserID;
  UserDefineIO.UserPath := umlCombinePath(FRootPath, UserDefineIO.UserFlag);
  UserDefineIO.DoubleTunnelService := Self;
  UserDefineIO.LoginSuccessed := True;

  if umlFileExists(UserDefineIO.MakeFilePath('User.Config')) then
      UserDefineIO.UserConfigFile.LoadFromFile(UserDefineIO.MakeFilePath('User.Config'));

  UserDefineIO.UserConfigFile.Hit['UserInfo', 'UserID'] := UserID;
  UserDefineIO.UserConfigFile.Hit['UserInfo', 'Password'] := string(FUserDB.GetDefaultValue(UserID, 'password', ''));

  LockObject(FLoginUserList);
  FLoginUserList[UserID] := Now;
  UnLockObject(FLoginUserList);

  LockObject(FLoginUserDefineIOList);
  FLoginUserDefineIOList[UserID] := UserDefineIO;
  UnLockObject(FLoginUserDefineIOList);

  UserDefineIO.WaitLink := True;
  UserDefineIO.WaitLinkSendID := SendTunnelID;

  OutData.WriteBool(True);
  OutData.WriteString(Format('success Login:%s', [UserID]));
  OutData.WriteString(UserDefineIO.UserFlag);

  UserLoginSuccess(UserDefineIO);
end;

procedure TCommunicationFramework_DoubleTunnelService.Command_RegisterUser(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
var
  SendTunnelID      : Cardinal;
  UserID, UserPasswd: string;
  UserDefineIO      : TPeerClientUserDefineForRecvTunnel;
begin
  UserDefineIO := GetUserDefineRecvTunnel(Sender);

  if not FCanRegisterNewUser then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(Format('disable user register in server', []));
      Exit;
    end;

  SendTunnelID := InData.Reader.ReadCardinal;
  UserID := InData.Reader.ReadString;
  UserPasswd := InData.Reader.ReadString;

  if not FSendTunnel.Exists(SendTunnelID) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(Format('send tunnel Illegal:%d', [SendTunnelID]));
      Exit;
    end;

  if umlExistsLimitChar(UserID, '[]:'#13#10#9#8#0) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(Format('user name Illegal:%s', [UserID]));
      Exit;
    end;

  if FUserDB.Exists(UserID) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(Format('user already registed:%s', [UserID]));
      Exit;
    end;

  LockObject(FLoginUserList);
  if FLoginUserList.Exists(UserID) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(Format('user already online:%s', [UserID]));
      UnLockObject(FLoginUserList);
      Exit;
    end;
  UnLockObject(FLoginUserList);

  UserDefineIO.UserFlag := MakeUserFlag;
  UserDefineIO.UserID := UserID;
  UserDefineIO.UserPath := umlCombinePath(FRootPath, UserDefineIO.UserFlag);
  umlCreateDirectory(UserDefineIO.UserPath);
  UserDefineIO.UserDBIntf := FUserDB.VariantList[UserID];
  UserDefineIO.UserDBIntf['UserFlag'] := UserDefineIO.UserFlag;
  UserDefineIO.UserDBIntf['password'] := GeneratePassword(TCipherStyle.csDES64, UserPasswd).Text;
  UserDefineIO.UserDBIntf['RegTime'] := DateTimeToStr(Now);
  UserDefineIO.DoubleTunnelService := Self;
  UserDefineIO.LoginSuccessed := True;

  UserDefineIO.UserConfigFile.Hit['UserInfo', 'UserID'] := UserID;
  UserDefineIO.UserConfigFile.Hit['UserInfo', 'Password'] := UserDefineIO.UserDBIntf['password'];
  UserDefineIO.SaveConfigFile;

  if FCanSaveUserInfo then
      SaveUserDB;

  LockObject(FLoginUserList);
  FLoginUserList[UserID] := Now;
  UnLockObject(FLoginUserList);

  LockObject(FLoginUserDefineIOList);
  FLoginUserDefineIOList[UserID] := UserDefineIO;
  UnLockObject(FLoginUserDefineIOList);

  UserDefineIO.WaitLink := True;
  UserDefineIO.WaitLinkSendID := SendTunnelID;

  OutData.WriteBool(True);
  OutData.WriteString(Format('success registed:%s', [UserID]));
  OutData.WriteString(UserDefineIO.UserFlag);

  UserDefineIO.SaveConfigFile;
  UserRegistedSuccess(UserID);
end;

procedure TCommunicationFramework_DoubleTunnelService.Command_TunnelLink(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
var
  RecvID, SendID: Cardinal;
  UserDefineIO  : TPeerClientUserDefineForRecvTunnel;
begin
  RecvID := InData.Reader.ReadCardinal;
  SendID := InData.Reader.ReadCardinal;

  UserDefineIO := GetUserDefineRecvTunnel(Sender);

  if not UserDefineIO.LoginSuccessed then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(Format('need login or register', []));
      Exit;
    end;

  if not FSendTunnel.Exists(SendID) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(Format('send tunnel Illegal:%d', [SendID]));
      Exit;
    end;

  if not FRecvTunnel.Exists(RecvID) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(Format('recv tunnel Illegal:%d', [RecvID]));
      Exit;
    end;

  if Sender.ID <> RecvID then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(Format('recv tunnel Illegal:%d-%d', [Sender.ID, RecvID]));
      Exit;
    end;

  UserDefineIO.SendTunnel := FSendTunnel.ClientFromID[SendID].UserDefine as TPeerClientUserDefineForSendTunnel;
  UserDefineIO.SendTunnelID := SendID;
  UserDefineIO.SendTunnel.RecvTunnel := UserDefineIO;
  UserDefineIO.SendTunnel.RecvTunnelID := RecvID;
  UserDefineIO.SendTunnel.DoubleTunnelService := Self;

  OutData.WriteBool(True);
  OutData.WriteString(Format('tunnel link success! recv:%d <-> send:%d', [RecvID, SendID]));

  UserLinkSuccess(UserDefineIO);
end;

procedure TCommunicationFramework_DoubleTunnelService.Command_ChangePasswd(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
var
  UserDefineIO        : TPeerClientUserDefineForRecvTunnel;
  oldPasswd, newPasswd: string;
begin
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LoginSuccessed then
      Exit;
  if UserDefineIO.SendTunnel = nil then
      Exit;

  oldPasswd := InData.Reader.ReadString;
  newPasswd := InData.Reader.ReadString;

  try
    if not ComparePassword(TCipherStyle.csDES64, oldPasswd, string(FUserDB.GetDefaultValue(UserDefineIO.UserID, 'password', ''))) then
      begin
        OutData.WriteBool(False);
        OutData.WriteString(Format('password error', []));
        Exit;
      end;
  except
    OutData.WriteBool(False);
    OutData.WriteString(Format('password error', []));
    Exit;
  end;

  UserDefineIO.UserDBIntf['password'] := GeneratePassword(TCipherStyle.csDES64, newPasswd).Text;
  if FCanSaveUserInfo then
      SaveUserDB;

  OutData.WriteBool(True);
  OutData.WriteString(Format('password change success', []));
end;

procedure TCommunicationFramework_DoubleTunnelService.Command_CustomNewUser(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
var
  UserDefineIO  : TPeerClientUserDefineForRecvTunnel;
  UserID, Passwd: string;
  UserConfig    : TSectionTextData;
begin
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LoginSuccessed then
      Exit;
  if UserDefineIO.SendTunnel = nil then
      Exit;

  UserID := InData.Reader.ReadString;
  Passwd := InData.Reader.ReadString;
  UserConfig := TSectionTextData.Create;
  InData.Reader.ReadSectionText(UserConfig);

  OutData.WriteBool(RegUser(UserID, Passwd, UserConfig));

  DisposeObject(UserConfig);
end;

procedure TCommunicationFramework_DoubleTunnelService.Command_ProcessStoreQueueCMD(Sender: TPeerClient; InData: TDataFrameEngine);
var
  UserDefineIO: TPeerClientUserDefineForRecvTunnel;
  fs          : umlStringDynArray;
  fn          : string;
  cmd         : string;
  de, de2     : TDataFrameEngine;
  Stream      : TCoreClassFileStream;
begin
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LoginSuccessed then
      Exit;
  if UserDefineIO.SendTunnel = nil then
      Exit;

  fs := umlGetFileListWithFullPath(UserDefineIO.UserPath);

  for fn in fs do
    begin
      if umlMultipleMatch(True, '*.queue', fn) then
        begin
          try
            de := TDataFrameEngine.Create;
            Stream := TCoreClassFileStream.Create(fn, fmOpenRead or fmShareDenyWrite);
            de.DecodeFrom(Stream);
            DisposeObject(Stream);
            cmd := de.Reader.ReadString;
            de2 := TDataFrameEngine.Create;
            de.Reader.ReadDataFrame(de2);
            FSendTunnel.SendDirectStreamCmd(UserDefineIO.SendTunnel.Owner, cmd, de2);
            DisposeObject(de2);
            DisposeObject(de);
          except
          end;
          umlDeleteFile(fn);
        end;
    end;
end;

procedure TCommunicationFramework_DoubleTunnelService.Command_GetPublicFileList(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
var
  UserDefineIO: TPeerClientUserDefineForRecvTunnel;
  Filter      : string;
  fs          : umlStringDynArray;
  i           : Integer;
  n           : string;
begin
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LoginSuccessed then
      Exit;
  if UserDefineIO.SendTunnel = nil then
      Exit;

  Filter := InData.Reader.ReadString;

  fs := umlGetFileListWithFullPath(FPublicPath);
  for i := low(fs) to high(fs) do
    begin
      n := umlGetFileName(fs[i]);
      if umlMultipleMatch(Filter, n) then
          OutData.WriteString(n);
    end;
end;

procedure TCommunicationFramework_DoubleTunnelService.Command_GetPrivateFileList(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
var
  UserDefineIO: TPeerClientUserDefineForRecvTunnel;
  Filter, dn  : string;
  fs          : umlStringDynArray;
  i           : Integer;
  n           : string;
begin
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LoginSuccessed then
      Exit;
  if UserDefineIO.SendTunnel = nil then
      Exit;

  Filter := InData.Reader.ReadString;
  dn := InData.Reader.ReadString;

  fs := umlGetFileListWithFullPath(umlCombinePath(UserDefineIO.UserPath, dn));
  for i := low(fs) to high(fs) do
    begin
      n := umlGetFileName(fs[i]);
      if umlMultipleMatch(Filter, n) then
          OutData.WriteString(n);
    end;
end;

procedure TCommunicationFramework_DoubleTunnelService.Command_GetPrivateDirectoryList(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
var
  UserDefineIO: TPeerClientUserDefineForRecvTunnel;
  Filter, dn  : string;
  fs          : umlStringDynArray;
  i           : Integer;
  n           : string;
begin
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LoginSuccessed then
      Exit;
  if UserDefineIO.SendTunnel = nil then
      Exit;

  Filter := InData.Reader.ReadString;
  dn := InData.Reader.ReadString;

  fs := umlGetDirListWithFullPath(umlCombinePath(UserDefineIO.UserPath, dn));
  for i := low(fs) to high(fs) do
    begin
      n := umlGetFileName(fs[i]);
      if umlMultipleMatch(Filter, n) then
          OutData.WriteString(n);
    end;
end;

procedure TCommunicationFramework_DoubleTunnelService.Command_GetUserPrivateFileList(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
var
  UserDefineIO      : TPeerClientUserDefineForRecvTunnel;
  UserID, Filter, dn: string;
  fs                : umlStringDynArray;
  i                 : Integer;
  n                 : string;
begin
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LoginSuccessed then
      Exit;
  if UserDefineIO.SendTunnel = nil then
      Exit;

  UserID := InData.Reader.ReadString;
  Filter := InData.Reader.ReadString;
  dn := InData.Reader.ReadString;

  if not ExistsUser(UserID) then
      Exit;

  fs := umlGetFileListWithFullPath(umlCombinePath(GetUserPath(UserID), dn));
  for i := low(fs) to high(fs) do
    begin
      n := umlGetFileName(fs[i]);
      if umlMultipleMatch(Filter, n) then
          OutData.WriteString(n);
    end;
end;

procedure TCommunicationFramework_DoubleTunnelService.Command_GetUserPrivateDirectoryList(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
var
  UserDefineIO      : TPeerClientUserDefineForRecvTunnel;
  UserID, Filter, dn: string;
  fs                : umlStringDynArray;
  i                 : Integer;
  n                 : string;
begin
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LoginSuccessed then
      Exit;
  if UserDefineIO.SendTunnel = nil then
      Exit;

  UserID := InData.Reader.ReadString;
  Filter := InData.Reader.ReadString;
  dn := InData.Reader.ReadString;

  if not ExistsUser(UserID) then
      Exit;

  fs := umlGetDirListWithFullPath(umlCombinePath(GetUserPath(UserID), dn));
  for i := low(fs) to high(fs) do
    begin
      n := umlGetFileName(fs[i]);
      if umlMultipleMatch(Filter, n) then
          OutData.WriteString(n);
    end;
end;

procedure TCommunicationFramework_DoubleTunnelService.Command_CreatePrivateDirectory(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
var
  UserDefineIO: TPeerClientUserDefineForRecvTunnel;
  dn, fulldn  : string;
begin
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LoginSuccessed then
      Exit;
  if UserDefineIO.SendTunnel = nil then
      Exit;

  dn := InData.Reader.ReadString;

  fulldn := umlCombinePath(UserDefineIO.UserPath, dn);

  try
    if not umlDirectoryExists(fulldn) then
      begin
        umlCreateDirectory(fulldn);
        OutData.WriteBool(umlDirectoryExists(fulldn));
        UserCreateDirectorySuccess(UserDefineIO, fulldn);
      end
    else
      begin
        OutData.WriteBool(True);
      end;
  except
      OutData.WriteBool(False);
  end;
end;

procedure TCommunicationFramework_DoubleTunnelService.Command_GetPublicFile(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
var
  UserDefineIO                : TPeerClientUserDefineForRecvTunnel;
  fullfn, fileName, remoteinfo: string;
  RemoteBackcallAddr          : UInt64;
  sendDE                      : TDataFrameEngine;
  fs                          : TCoreClassFileStream;
  md5                         : UnicodeMixedLib.TMD5;
begin
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LoginSuccessed then
      Exit;
  if UserDefineIO.SendTunnel = nil then
      Exit;

  fileName := InData.Reader.ReadString;
  remoteinfo := InData.Reader.ReadString;
  RemoteBackcallAddr := InData.Reader.ReadUInt64;

  fullfn := umlCombinePath(FPublicPath, fileName);
  if not umlFileExists(fullfn) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(Format('filename invailed %s', [fileName]));
      Exit;
    end;

  try
      fs := TCoreClassFileStream.Create(fullfn, fmOpenRead or fmShareDenyWrite);
  except
      Exit;
  end;

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteString(fileName);
  sendDE.WriteInt64(fs.Size);
  sendDE.WriteString(remoteinfo);
  UserDefineIO.SendTunnel.Owner.SendDirectStreamCmd('FileInfo', sendDE);
  DisposeObject(sendDE);

  md5 := umlStreamMD5(fs);

  fs.Position := 0;
  UserDefineIO.SendTunnel.Owner.SendBigStream('PostFile', fs, True);

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteMD5(md5);
  sendDE.WriteUInt64(RemoteBackcallAddr);
  UserDefineIO.SendTunnel.Owner.SendDirectStreamCmd('PostFileOver', sendDE);
  DisposeObject(sendDE);

  OutData.WriteBool(True);
  OutData.WriteString(Format('post %s to send tunnel', [fileName]));
end;

procedure TCommunicationFramework_DoubleTunnelService.Command_GetPrivateFile(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
var
  UserDefineIO                    : TPeerClientUserDefineForRecvTunnel;
  fullfn, fileName, dn, remoteinfo: string;
  RemoteBackcallAddr              : UInt64;
  sendDE                          : TDataFrameEngine;
  fs                              : TCoreClassFileStream;
  md5                             : UnicodeMixedLib.TMD5;
begin
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LoginSuccessed then
      Exit;
  if UserDefineIO.SendTunnel = nil then
      Exit;

  fileName := InData.Reader.ReadString;
  dn := InData.Reader.ReadString;
  remoteinfo := InData.Reader.ReadString;
  RemoteBackcallAddr := InData.Reader.ReadUInt64;

  fullfn := umlCombinePath(umlCombinePath(UserDefineIO.UserPath, dn), fileName);
  if not umlFileExists(fullfn) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(Format('filename invailed %s', [fileName]));
      Exit;
    end;

  try
      fs := TCoreClassFileStream.Create(fullfn, fmOpenRead or fmShareDenyWrite);
  except
      Exit;
  end;

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteString(fileName);
  sendDE.WriteInt64(fs.Size);
  sendDE.WriteString(remoteinfo);
  UserDefineIO.SendTunnel.Owner.SendDirectStreamCmd('FileInfo', sendDE);
  DisposeObject(sendDE);

  md5 := umlStreamMD5(fs);

  fs.Position := 0;
  UserDefineIO.SendTunnel.Owner.SendBigStream('PostFile', fs, True);

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteMD5(md5);
  sendDE.WriteUInt64(RemoteBackcallAddr);
  UserDefineIO.SendTunnel.Owner.SendDirectStreamCmd('PostFileOver', sendDE);
  DisposeObject(sendDE);

  OutData.WriteBool(True);
  OutData.WriteString(Format('post %s to send tunnel', [fileName]));
end;

procedure TCommunicationFramework_DoubleTunnelService.Command_GetPrivateFileOfBatch(Sender: TPeerClient; InData: TDataFrameEngine);
var
  UserDefineIO                 : TPeerClientUserDefineForRecvTunnel;
  flst                         : umlStringDynArray;
  Filter, fn, dn, remoteinfo   : string;
  RemoteBackcallAddr           : UInt64;
  sendDE, PostFileOfBatchSendDE: TDataFrameEngine;
  fs                           : TCoreClassFileStream;
  md5                          : UnicodeMixedLib.TMD5;
begin
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LoginSuccessed then
      Exit;
  if UserDefineIO.SendTunnel = nil then
      Exit;

  Filter := InData.Reader.ReadString;
  dn := InData.Reader.ReadString;
  remoteinfo := InData.Reader.ReadString;
  RemoteBackcallAddr := InData.Reader.ReadUInt64;

  if umlDirectoryExists(umlCombinePath(UserDefineIO.UserPath, dn)) then
      flst := umlGetFileListWithFullPath(umlCombinePath(UserDefineIO.UserPath, dn))
  else
      SetLength(flst, 0);

  PostFileOfBatchSendDE := TDataFrameEngine.Create;
  PostFileOfBatchSendDE.WriteUInt64(RemoteBackcallAddr);
  for fn in flst do
    if umlMultipleMatch(Filter, umlGetFileName(fn)) then
      begin
        try
            fs := TCoreClassFileStream.Create(fn, fmOpenRead or fmShareDenyWrite);
        except
            break;
        end;

        sendDE := TDataFrameEngine.Create;
        sendDE.WriteString(umlGetFileName(fn));
        sendDE.WriteInt64(fs.Size);
        sendDE.WriteString(remoteinfo);
        UserDefineIO.SendTunnel.Owner.SendDirectStreamCmd('FileInfo', sendDE);
        DisposeObject(sendDE);

        md5 := umlStreamMD5(fs);

        fs.Position := 0;
        UserDefineIO.SendTunnel.Owner.SendBigStream('PostFile', fs, True);

        sendDE := TDataFrameEngine.Create;
        sendDE.WriteMD5(md5);
        sendDE.WriteUInt64(0);
        UserDefineIO.SendTunnel.Owner.SendDirectStreamCmd('PostFileOver', sendDE);
        DisposeObject(sendDE);

        PostFileOfBatchSendDE.WriteString(umlGetFileName(fn));
      end;

  UserDefineIO.SendTunnel.Owner.SendDirectStreamCmd('PostFileOfBatchOver', PostFileOfBatchSendDE);
  DisposeObject(PostFileOfBatchSendDE);
end;

procedure TCommunicationFramework_DoubleTunnelService.Command_GetUserPrivateFile(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
var
  UserDefineIO                            : TPeerClientUserDefineForRecvTunnel;
  fullfn, UserID, fileName, dn, remoteinfo: string;
  RemoteBackcallAddr                      : UInt64;
  sendDE                                  : TDataFrameEngine;
  fs                                      : TCoreClassFileStream;
  md5                                     : UnicodeMixedLib.TMD5;
begin
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LoginSuccessed then
      Exit;
  if UserDefineIO.SendTunnel = nil then
      Exit;

  UserID := InData.Reader.ReadString;
  fileName := InData.Reader.ReadString;
  dn := InData.Reader.ReadString;
  remoteinfo := InData.Reader.ReadString;
  RemoteBackcallAddr := InData.Reader.ReadUInt64;

  if not ExistsUser(UserID) then
      Exit;

  fullfn := umlCombinePath(umlCombinePath(GetUserPath(UserID), dn), fileName);
  if not umlFileExists(fullfn) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(Format('filename invailed %s', [fileName]));
      Exit;
    end;

  try
      fs := TCoreClassFileStream.Create(fullfn, fmOpenRead or fmShareDenyWrite);
  except
      Exit;
  end;

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteString(fileName);
  sendDE.WriteInt64(fs.Size);
  sendDE.WriteString(remoteinfo);
  UserDefineIO.SendTunnel.Owner.SendDirectStreamCmd('FileInfo', sendDE);
  DisposeObject(sendDE);

  md5 := umlStreamMD5(fs);

  fs.Position := 0;
  UserDefineIO.SendTunnel.Owner.SendBigStream('PostFile', fs, True);

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteMD5(md5);
  sendDE.WriteUInt64(RemoteBackcallAddr);
  UserDefineIO.SendTunnel.Owner.SendDirectStreamCmd('PostFileOver', sendDE);
  DisposeObject(sendDE);

  OutData.WriteBool(True);
  OutData.WriteString(Format('post %s to send tunnel', [fileName]));

end;

procedure TCommunicationFramework_DoubleTunnelService.Command_GetUserPrivateFileOfBatch(Sender: TPeerClient; InData: TDataFrameEngine);
var
  UserDefineIO                      : TPeerClientUserDefineForRecvTunnel;
  flst                              : umlStringDynArray;
  UserID, Filter, fn, dn, remoteinfo: string;
  RemoteBackcallAddr                : UInt64;
  sendDE, PostFileOfBatchSendDE     : TDataFrameEngine;
  fs                                : TCoreClassFileStream;
  md5                               : UnicodeMixedLib.TMD5;
begin
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LoginSuccessed then
      Exit;
  if UserDefineIO.SendTunnel = nil then
      Exit;

  UserID := InData.Reader.ReadString;
  Filter := InData.Reader.ReadString;
  dn := InData.Reader.ReadString;
  remoteinfo := InData.Reader.ReadString;
  RemoteBackcallAddr := InData.Reader.ReadUInt64;

  if ExistsUser(UserID) then
      Exit;

  if umlDirectoryExists(umlCombinePath(GetUserPath(UserID), dn)) then
      flst := umlGetFileListWithFullPath(umlCombinePath(GetUserPath(UserID), dn))
  else
      SetLength(flst, 0);

  PostFileOfBatchSendDE := TDataFrameEngine.Create;
  PostFileOfBatchSendDE.WriteUInt64(RemoteBackcallAddr);
  for fn in flst do
    if umlMultipleMatch(Filter, umlGetFileName(fn)) then
      begin
        try
            fs := TCoreClassFileStream.Create(fn, fmOpenRead or fmShareDenyWrite);
        except
            break;
        end;

        sendDE := TDataFrameEngine.Create;
        sendDE.WriteString(umlGetFileName(fn));
        sendDE.WriteInt64(fs.Size);
        sendDE.WriteString(remoteinfo);
        UserDefineIO.SendTunnel.Owner.SendDirectStreamCmd('FileInfo', sendDE);
        DisposeObject(sendDE);

        md5 := umlStreamMD5(fs);

        fs.Position := 0;
        UserDefineIO.SendTunnel.Owner.SendBigStream('PostFile', fs, True);

        sendDE := TDataFrameEngine.Create;
        sendDE.WriteMD5(md5);
        sendDE.WriteUInt64(0);
        UserDefineIO.SendTunnel.Owner.SendDirectStreamCmd('PostFileOver', sendDE);
        DisposeObject(sendDE);

        PostFileOfBatchSendDE.WriteString(umlGetFileName(fn));
      end;

  UserDefineIO.SendTunnel.Owner.SendDirectStreamCmd('PostFileOfBatchOver', PostFileOfBatchSendDE);
  DisposeObject(PostFileOfBatchSendDE);
end;

procedure TCommunicationFramework_DoubleTunnelService.Command_PostPublicFileInfo(Sender: TPeerClient; InData: TDataFrameEngine);
var
  UserDefineIO: TPeerClientUserDefineForRecvTunnel;
  fn          : string;
  fsize       : Int64;
  fullfn      : string;
begin
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LoginSuccessed then
    begin
      Sender.Disconnect;
      Exit;
    end;
  if UserDefineIO.SendTunnel = nil then
    begin
      Sender.Disconnect;
      Exit;
    end;

  if UserDefineIO.FCurrentFileStream <> nil then
    begin
      DisposeObject(UserDefineIO.FCurrentFileStream);
      UserDefineIO.FCurrentFileStream := nil;
    end;

  fn := InData.Reader.ReadString;
  fsize := InData.Reader.ReadInt64;

  fullfn := umlCombinePath(FPublicPath, fn);
  UserDefineIO.FCurrentReceiveFileName := fullfn;
  try
    UserDefineIO.FCurrentFileStream := TCoreClassFileStream.Create(fullfn, fmCreate);
    Sender.Print(Format('preprocess user:%s post to public: %s', [UserDefineIO.UserID, fullfn]));
  except
    Sender.Print(Format('create public file failed! user:%s post to public: %s', [UserDefineIO.UserID, fullfn]));
    UserDefineIO.FCurrentFileStream := nil;
  end;
end;

procedure TCommunicationFramework_DoubleTunnelService.Command_PostPrivateFileInfo(Sender: TPeerClient; InData: TDataFrameEngine);
var
  UserDefineIO: TPeerClientUserDefineForRecvTunnel;
  fn, dn      : string;
  fsize       : Int64;
  fullfn      : string;
begin
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LoginSuccessed then
    begin
      Sender.Disconnect;
      Exit;
    end;
  if UserDefineIO.SendTunnel = nil then
    begin
      Sender.Disconnect;
      Exit;
    end;

  if UserDefineIO.FCurrentFileStream <> nil then
    begin
      DisposeObject(UserDefineIO.FCurrentFileStream);
      UserDefineIO.FCurrentFileStream := nil;
    end;

  fn := InData.Reader.ReadString;
  dn := InData.Reader.ReadString;
  fsize := InData.Reader.ReadInt64;

  if not umlDirectoryExists(umlCombinePath(UserDefineIO.UserPath, dn)) then
    begin
      umlCreateDirectory(umlCombinePath(UserDefineIO.UserPath, dn));
      Exit;
    end;

  fullfn := umlCombinePath(umlCombinePath(UserDefineIO.UserPath, dn), fn);
  UserDefineIO.FCurrentReceiveFileName := fullfn;

  try
    UserDefineIO.FCurrentFileStream := TCoreClassFileStream.Create(fullfn, fmCreate);
    Sender.Print(Format('preprocess user:%s post to private:%s', [UserDefineIO.UserID, fullfn]));
  except
    Sender.Print(Format('create private file failed! user:%s post to public: %s', [UserDefineIO.UserID, fullfn]));
    UserDefineIO.FCurrentFileStream := nil;
  end;
end;

procedure TCommunicationFramework_DoubleTunnelService.Command_PostFile(Sender: TPeerClient; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64);
var
  UserDefineIO: TPeerClientUserDefineForRecvTunnel;
begin
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LoginSuccessed then
    begin
      Sender.Disconnect;
      Exit;
    end;
  if UserDefineIO.SendTunnel = nil then
    begin
      Sender.Disconnect;
      Exit;
    end;

  if UserDefineIO.FCurrentFileStream <> nil then
    begin
      InData.Position := 0;
      if InData.Size > 0 then
          UserDefineIO.FCurrentFileStream.CopyFrom(InData, InData.Size);
    end;
end;

procedure TCommunicationFramework_DoubleTunnelService.Command_PostFileOver(Sender: TPeerClient; InData: TDataFrameEngine);
var
  UserDefineIO  : TPeerClientUserDefineForRecvTunnel;
  ClientMD5, md5: UnicodeMixedLib.TMD5;
  fn            : string;
begin
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LoginSuccessed then
    begin
      Sender.Disconnect;
      Exit;
    end;
  if UserDefineIO.SendTunnel = nil then
    begin
      Sender.Disconnect;
      Exit;
    end;

  ClientMD5 := InData.Reader.ReadMD5;

  if UserDefineIO.FCurrentFileStream <> nil then
    begin
      md5 := umlStreamMD5(UserDefineIO.FCurrentFileStream);
      fn := UserDefineIO.FCurrentReceiveFileName;
      DisposeObject(UserDefineIO.FCurrentFileStream);
      UserDefineIO.FCurrentFileStream := nil;

      if umlMD5Compare(md5, ClientMD5) then
        begin
          Sender.Print(Format('Received File Completed:%s', [fn]));
          UserPostFileSuccess(UserDefineIO, fn);
        end
      else
        begin
          Sender.Print(Format('File data error:%s', [fn]));
          umlDeleteFile(fn);
        end;
    end;
end;

procedure TCommunicationFramework_DoubleTunnelService.Command_GetCurrentCadencer(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
begin
  FCadencerEngine.Progress;
  OutData.WriteDouble(FCadencerEngine.CurrentTime);
end;

constructor TCommunicationFramework_DoubleTunnelService.Create(ARecvTunnel, ASendTunnel: TCommunicationFrameworkServer);
begin
  inherited Create;
  FRecvTunnel := ARecvTunnel;
  FRecvTunnel.PeerClientUserDefineClass := TPeerClientUserDefineForRecvTunnel;
  FSendTunnel := ASendTunnel;
  FSendTunnel.PeerClientUserDefineClass := TPeerClientUserDefineForSendTunnel;

  FRootPath := umlCurrentPath;
  FPublicPath := FRootPath;
  FUserDB := TSectionTextData.Create(20 * 10000);
  FCanRegisterNewUser := False;
  FCanSaveUserInfo := False;
  FCanStatus := True;
  FLoginUserList := THashVariantList.Create(8192);
  FLoginUserDefineIOList := THashObjectList.Create(False, 8192);

  FCadencerEngine := TCadencer.Create;
  {$IFDEF FPC}
  FCadencerEngine.OnProgress := @CadencerProgress;
  {$ELSE}
  FCadencerEngine.OnProgress := CadencerProgress;
  {$ENDIF}
  FProgressEngine := TNProgressPost.Create;

  SwitchServiceAsDefaultPerformance;
end;

destructor TCommunicationFramework_DoubleTunnelService.Destroy;
begin
  DisposeObject(FLoginUserList);
  DisposeObject(FLoginUserDefineIOList);
  DisposeObject(FUserDB);
  DisposeObject(FCadencerEngine);
  DisposeObject(FProgressEngine);
  inherited Destroy;
end;

procedure TCommunicationFramework_DoubleTunnelService.SwitchServiceAsMaxPerformance;
begin
  FRecvTunnel.SwitchMaxPerformance;
  FSendTunnel.SwitchMaxPerformance;
end;

procedure TCommunicationFramework_DoubleTunnelService.SwitchServiceAsMaxSafe;
begin
  FRecvTunnel.SwitchMaxSafe;
  FSendTunnel.SwitchMaxSafe;
end;

procedure TCommunicationFramework_DoubleTunnelService.SwitchServiceAsDefaultPerformance;
begin
  FRecvTunnel.SwitchDefaultPerformance;
  FSendTunnel.SwitchDefaultPerformance;
end;

procedure TCommunicationFramework_DoubleTunnelService.Progress;
begin
  FCadencerEngine.Progress;
  FRecvTunnel.ProgressBackground;
  FSendTunnel.ProgressBackground;
end;

procedure TCommunicationFramework_DoubleTunnelService.CadencerProgress(Sender: TObject; const deltaTime, newTime: Double);
begin
  FProgressEngine.Progress(deltaTime);
end;

procedure TCommunicationFramework_DoubleTunnelService.LoadUserDB;
var
  i  : Integer;
  cli: TPeerClientUserDefineForRecvTunnel;
begin
  if umlFileExists(umlCombinePath(FRootPath, 'UserDB')) then
    begin
      FUserDB.LoadFromFile(umlCombinePath(FRootPath, 'UserDB'));

      FRecvTunnel.LockClients;
      for i := 0 to FRecvTunnel.Count - 1 do
        begin
          cli := GetUserDefineRecvTunnel(FRecvTunnel[i]);
          if cli.LoginSuccessed then
              cli.UserDBIntf := FUserDB.VariantList[cli.UserID];
        end;
      FRecvTunnel.UnLockClients;
    end;
end;

procedure TCommunicationFramework_DoubleTunnelService.SaveUserDB;
var
  i  : Integer;
  cli: TPeerClientUserDefineForRecvTunnel;
begin
  FUserDB.SaveToFile(umlCombinePath(FRootPath, 'UserDB'));

  FRecvTunnel.LockClients;
  for i := 0 to FRecvTunnel.Count - 1 do
    begin
      cli := GetUserDefineRecvTunnel(FRecvTunnel[i]);
      if cli.LoginSuccessed then
          cli.UserDBIntf := FUserDB.VariantList[cli.UserID];
    end;
  FRecvTunnel.UnLockClients;
end;

function TCommunicationFramework_DoubleTunnelService.RegUser(AUserID, APasswd: string; AUserConfigFile: TSectionTextData): Boolean;
var
  AUserFlag, AUserPath: string;
  AUserDBIntf         : THashVariantList;
  te                  : TSectionTextData;
begin
  Result := False;
  if umlExistsLimitChar(AUserID, '[]:'#13#10#9#8#0) then
      Exit;

  if FUserDB.Exists(AUserID) then
      Exit;

  LockObject(FLoginUserList);
  if FLoginUserList.Exists(AUserID) then
    begin
      UnLockObject(FLoginUserList);
      Exit;
    end;
  UnLockObject(FLoginUserList);

  AUserFlag := MakeUserFlag;

  AUserPath := umlCombinePath(FRootPath, AUserFlag);
  umlCreateDirectory(AUserPath);

  AUserDBIntf := FUserDB.VariantList[AUserID];
  AUserDBIntf['UserFlag'] := AUserFlag;
  AUserDBIntf['password'] := GeneratePassword(TCipherStyle.csDES64, APasswd).Text;

  if AUserConfigFile <> nil then
    begin
      AUserConfigFile.Hit['UserInfo', 'UserID'] := AUserID;
      AUserConfigFile.Hit['UserInfo', 'Password'] := AUserDBIntf['password'];
      AUserConfigFile.SaveToFile(umlCombinePath(AUserPath, 'User.Config'));
    end
  else
    begin
      te := TSectionTextData.Create;
      te.Hit['UserInfo', 'UserID'] := AUserID;
      te.Hit['UserInfo', 'Password'] := AUserDBIntf['password'];
      te.SaveToFile(umlCombinePath(AUserPath, 'User.Config'));
      DisposeObject(te);
    end;

  if FCanSaveUserInfo then
      SaveUserDB;

  UserRegistedSuccess(AUserID);
  Result := True;
end;

function TCommunicationFramework_DoubleTunnelService.ExistsUser(AUserID: string): Boolean;
begin
  Result := FUserDB.Exists(AUserID);
end;

function TCommunicationFramework_DoubleTunnelService.GetUserPath(AUserID: string): string;
var
  AUserFlag  : string;
  AUserDBIntf: THashVariantList;
begin
  Result := '';
  if not ExistsUser(AUserID) then
      Exit;

  AUserDBIntf := FUserDB.VariantList[AUserID];
  AUserFlag := AUserDBIntf.GetDefaultValue('UserFlag', '');
  Result := umlCombinePath(FRootPath, AUserFlag);
end;

function TCommunicationFramework_DoubleTunnelService.GetUserFile(AUserID, AUserFileName: string): string;
begin
  Result := umlCombineFileName(GetUserPath(AUserID), AUserFileName);
end;

function TCommunicationFramework_DoubleTunnelService.GetUserDefineIO(AUserID: string): TPeerClientUserDefineForRecvTunnel;
begin
  Result := TPeerClientUserDefineForRecvTunnel(FLoginUserDefineIOList[AUserID]);
end;

function TCommunicationFramework_DoubleTunnelService.UserOnline(AUserID: string): Boolean;
begin
  Result := GetUserDefineIO(AUserID) <> nil;
end;

function TCommunicationFramework_DoubleTunnelService.PackUserAsFile(AUserID, aPackFile: string): Boolean;
var
  cli: TPeerClientUserDefineForRecvTunnel;
begin
  Result := False;
  if not ExistsUser(AUserID) then
      Exit;
  cli := GetUserDefineIO(AUserID);
  if cli <> nil then
      cli.SaveConfigFile;

  BatchImportPathToDBFile(GetUserPath(AUserID), '*', aPackFile);

  Result := True;
end;

function TCommunicationFramework_DoubleTunnelService.PackUserAsStream(AUserID: string; PackStream: TCoreClassStream): Boolean;
var
  cli: TPeerClientUserDefineForRecvTunnel;
begin
  Result := False;
  if not ExistsUser(AUserID) then
      Exit;
  cli := GetUserDefineIO(AUserID);
  if cli <> nil then
      cli.SaveConfigFile;

  BatchImportPathToDBStream(GetUserPath(AUserID), '*', PackStream);

  if not ExistsFileInDB(PackStream, '/', 'User.Config') then
      empty;

  Result := True;
end;

function TCommunicationFramework_DoubleTunnelService.UnPackFileAsUser(aPackFile: string): Boolean;
var
  fs: TCoreClassFileStream;
begin
  fs := TCoreClassFileStream.Create(aPackFile, fmOpenRead or fmShareDenyWrite);
  Result := UnPackStreamAsUser(fs);
  DisposeObject(fs);
end;

function TCommunicationFramework_DoubleTunnelService.UnPackStreamAsUser(aPackStream: TCoreClassStream): Boolean;
var
  dbEng: TObjectDataManager;
  m    : TMemoryStream64;
  te   : TSectionTextData;

  AUserID, APasswd: string;
  AUserDBIntf     : THashVariantList;
begin
  aPackStream.Position := 0;
  dbEng := TObjectDataManager.CreateAsStream(aPackStream, '', ObjectDataMarshal.ID, True, False, False);
  m := TMemoryStream64.Create;
  te := TSectionTextData.Create;

  try
    Result := ExtractFileInDB(dbEng, '/', 'User.Config', m);
    if Result then
      begin
        m.Position := 0;
        te.LoadFromStream(m);

        AUserID := te.GetDefaultValue('UserInfo', 'UserID', '');
        APasswd := te.GetDefaultValue('UserInfo', 'Password', '');

        if not UserOnline(AUserID) then
          begin
            if not ExistsUser(AUserID) then
              begin
                DoStatus('Register new user "%s" From Pack Stream', [AUserID]);
                RegUser(AUserID, APasswd, nil);
                ExtractDBToPath(dbEng, GetUserPath(AUserID));
                Result := True;
              end
            else
              begin
                DoStatus('update user "%s" From Pack Stream', [AUserID]);
                ExtractDBToPath(dbEng, GetUserPath(AUserID));

                AUserDBIntf := FUserDB.VariantList[AUserID];
                AUserDBIntf['password'] := APasswd;

                SaveUserDB;
                Result := True;
              end;
          end
        else
            DoStatus('un pack error, User is Online:%s', [AUserID]);
      end
    else
        DoStatus('unpack error,no exists file user.config in pack Stream', []);
  except
      Result := False;
  end;

  DisposeObject(te);
  DisposeObject(m);
  DisposeObject(dbEng);
end;

procedure TCommunicationFramework_DoubleTunnelService.PostStoreQueueCMD(ToUserID: string; cmd: string; InData: TDataFrameEngine);
var
  UserDefineIO: TPeerClientUserDefineForRecvTunnel;
  d           : Double;
  p           : PInt64;
  UserPath    : string;
  fn          : string;
  de          : TDataFrameEngine;
  fs          : TCoreClassFileStream;
begin
  UserDefineIO := GetUserDefineIO(ToUserID);
  if UserDefineIO <> nil then
    begin
      FSendTunnel.SendDirectStreamCmd(UserDefineIO.SendTunnel.Owner, cmd, InData);
    end
  else if ExistsUser(ToUserID) then
    begin
      UserPath := GetUserPath(ToUserID);

      repeat
        TCoreClassThread.Sleep(1);
        d := Now;
        p := @d;
        fn := umlCombinePath(UserPath, IntToHex(p^, 16) + '.queue');
      until not umlFileExists(fn);

      de := TDataFrameEngine.Create;
      de.WriteString(cmd);
      de.WriteDataFrame(InData);
      fs := TCoreClassFileStream.Create(fn, fmCreate);
      de.EncodeToCompressed(fs);
      DisposeObject(de);
      DisposeObject(fs);
    end;
end;

procedure TCommunicationFramework_DoubleTunnelService.RegisterCommand;
begin
  {$IFDEF FPC}
  FRecvTunnel.RegisterStream('UserLogin').OnExecute := @Command_UserLogin;
  FRecvTunnel.RegisterStream('RegisterUser').OnExecute := @Command_RegisterUser;
  FRecvTunnel.RegisterStream('TunnelLink').OnExecute := @Command_TunnelLink;
  FRecvTunnel.RegisterStream('ChangePasswd').OnExecute := @Command_ChangePasswd;
  FRecvTunnel.RegisterStream('CustomNewUser').OnExecute := @Command_CustomNewUser;
  FRecvTunnel.RegisterDirectStream('ProcessStoreQueueCMD').OnExecute := @Command_ProcessStoreQueueCMD;
  FRecvTunnel.RegisterStream('GetPublicFileList').OnExecute := @Command_GetPublicFileList;
  FRecvTunnel.RegisterStream('GetPrivateFileList').OnExecute := @Command_GetPrivateFileList;
  FRecvTunnel.RegisterStream('GetPrivateDirectoryList').OnExecute := @Command_GetPrivateDirectoryList;
  FRecvTunnel.RegisterStream('GetUserPrivateFileList').OnExecute := @Command_GetUserPrivateFileList;
  FRecvTunnel.RegisterStream('GetUserPrivateDirectoryList').OnExecute := @Command_GetUserPrivateDirectoryList;
  FRecvTunnel.RegisterStream('CreatePrivateDirectory').OnExecute := @Command_CreatePrivateDirectory;
  FRecvTunnel.RegisterStream('GetPublicFile').OnExecute := @Command_GetPublicFile;
  FRecvTunnel.RegisterStream('GetPrivateFile').OnExecute := @Command_GetPrivateFile;
  FRecvTunnel.RegisterDirectStream('GetPrivateFileOfBatch').OnExecute := @Command_GetPrivateFileOfBatch;
  FRecvTunnel.RegisterStream('GetUserPrivateFile').OnExecute := @Command_GetUserPrivateFile;
  FRecvTunnel.RegisterDirectStream('GetUserPrivateFileOfBatch').OnExecute := @Command_GetUserPrivateFileOfBatch;
  FRecvTunnel.RegisterDirectStream('PostPublicFileInfo').OnExecute := @Command_PostPublicFileInfo;
  FRecvTunnel.RegisterDirectStream('PostPrivateFileInfo').OnExecute := @Command_PostPrivateFileInfo;
  FRecvTunnel.RegisterBigStream('PostFile').OnExecute := @Command_PostFile;
  FRecvTunnel.RegisterDirectStream('PostFileOver').OnExecute := @Command_PostFileOver;
  FRecvTunnel.RegisterStream('GetCurrentCadencer').OnExecute := @Command_GetCurrentCadencer;

  {$ELSE}
  FRecvTunnel.RegisterStream('UserLogin').OnExecute := Command_UserLogin;
  FRecvTunnel.RegisterStream('RegisterUser').OnExecute := Command_RegisterUser;
  FRecvTunnel.RegisterStream('TunnelLink').OnExecute := Command_TunnelLink;
  FRecvTunnel.RegisterStream('ChangePasswd').OnExecute := Command_ChangePasswd;
  FRecvTunnel.RegisterStream('CustomNewUser').OnExecute := Command_CustomNewUser;
  FRecvTunnel.RegisterDirectStream('ProcessStoreQueueCMD').OnExecute := Command_ProcessStoreQueueCMD;
  FRecvTunnel.RegisterStream('GetPublicFileList').OnExecute := Command_GetPublicFileList;
  FRecvTunnel.RegisterStream('GetPrivateFileList').OnExecute := Command_GetPrivateFileList;
  FRecvTunnel.RegisterStream('GetPrivateDirectoryList').OnExecute := Command_GetPrivateDirectoryList;
  FRecvTunnel.RegisterStream('GetUserPrivateFileList').OnExecute := Command_GetUserPrivateFileList;
  FRecvTunnel.RegisterStream('GetUserPrivateDirectoryList').OnExecute := Command_GetUserPrivateDirectoryList;
  FRecvTunnel.RegisterStream('CreatePrivateDirectory').OnExecute := Command_CreatePrivateDirectory;
  FRecvTunnel.RegisterStream('GetPublicFile').OnExecute := Command_GetPublicFile;
  FRecvTunnel.RegisterStream('GetPrivateFile').OnExecute := Command_GetPrivateFile;
  FRecvTunnel.RegisterDirectStream('GetPrivateFileOfBatch').OnExecute := Command_GetPrivateFileOfBatch;
  FRecvTunnel.RegisterStream('GetUserPrivateFile').OnExecute := Command_GetUserPrivateFile;
  FRecvTunnel.RegisterDirectStream('GetUserPrivateFileOfBatch').OnExecute := Command_GetUserPrivateFileOfBatch;
  FRecvTunnel.RegisterDirectStream('PostPublicFileInfo').OnExecute := Command_PostPublicFileInfo;
  FRecvTunnel.RegisterDirectStream('PostPrivateFileInfo').OnExecute := Command_PostPrivateFileInfo;
  FRecvTunnel.RegisterBigStream('PostFile').OnExecute := Command_PostFile;
  FRecvTunnel.RegisterDirectStream('PostFileOver').OnExecute := Command_PostFileOver;
  FRecvTunnel.RegisterStream('GetCurrentCadencer').OnExecute := Command_GetCurrentCadencer;
  {$ENDIF}
end;

procedure TCommunicationFramework_DoubleTunnelService.UnRegisterCommand;
begin
  FRecvTunnel.DeleteRegistedCMD('UserLogin');
  FRecvTunnel.DeleteRegistedCMD('RegisterUser');
  FRecvTunnel.DeleteRegistedCMD('TunnelLink');

  FRecvTunnel.DeleteRegistedCMD('ChangePasswd');
  FRecvTunnel.DeleteRegistedCMD('CustomNewUser');

  FRecvTunnel.DeleteRegistedCMD('ProcessStoreQueueCMD');

  FRecvTunnel.DeleteRegistedCMD('GetPublicFileList');
  FRecvTunnel.DeleteRegistedCMD('GetPrivateFileList');
  FRecvTunnel.DeleteRegistedCMD('GetPrivateDirectoryList');

  FRecvTunnel.DeleteRegistedCMD('GetUserPrivateFileList');
  FRecvTunnel.DeleteRegistedCMD('GetUserPrivateDirectoryList');

  FRecvTunnel.DeleteRegistedCMD('CreatePrivateDirectory');

  FRecvTunnel.DeleteRegistedCMD('GetPublicFile');
  FRecvTunnel.DeleteRegistedCMD('GetPrivateFile');
  FRecvTunnel.DeleteRegistedCMD('GetPrivateFileOfBatch');

  FRecvTunnel.DeleteRegistedCMD('GetUserPrivateFile');
  FRecvTunnel.DeleteRegistedCMD('GetUserPrivateFileOfBatch');

  FRecvTunnel.DeleteRegistedCMD('PostPublicFileInfo');
  FRecvTunnel.DeleteRegistedCMD('PostPrivateFileInfo');
  FRecvTunnel.DeleteRegistedCMD('PostFile');
  FRecvTunnel.DeleteRegistedCMD('PostFileOver');

  FRecvTunnel.DeleteRegistedCMD('GetCurrentCadencer');
end;

function TCommunicationFramework_DoubleTunnelService.MakeUserFlag: string;
var
  d: Double;
  p: PInt64;
begin
  repeat
    TCoreClassThread.Sleep(1);
    d := Now;
    p := @d;
    Result := IntToHex(p^, 16);
  until not umlDirectoryExists(umlCombinePath(FRootPath, Result));
end;

function TCommunicationFramework_DoubleTunnelService.GetUserDefineRecvTunnel(RecvCli: TPeerClient): TPeerClientUserDefineForRecvTunnel;
begin
  Result := RecvCli.UserDefine as TPeerClientUserDefineForRecvTunnel;
end;

function TCommunicationFramework_DoubleTunnelService.TotalLinkCount: Integer;
begin
  Result := FLoginUserDefineIOList.Count;
end;

constructor TPeerClientUserDefineForDoubleTunnelClient.Create(AOwner: TPeerClient);
begin
  inherited Create(AOwner);
  Client := nil;
end;

destructor TPeerClientUserDefineForDoubleTunnelClient.Destroy;
begin
  if Client <> nil then
    begin
      if Client.FCurrentStream <> nil then
        begin
          DisposeObject(Client.FCurrentStream);
          Client.FCurrentStream := nil;
        end;
      Client.FLinkOk := False;
    end;
  inherited Destroy;
end;

procedure TCommunicationFramework_DoubleTunnelClient.Command_FileInfo(Sender: TPeerClient; InData: TDataFrameEngine);
var
  fn        : string;
  fsize     : Int64;
  remoteinfo: string;
  fullfn    : string;
begin
  if FCurrentStream <> nil then
    begin
      DisposeObject(FCurrentStream);
      FCurrentStream := nil;
    end;

  fn := InData.Reader.ReadString;
  fsize := InData.Reader.ReadInt64;
  remoteinfo := InData.Reader.ReadString;

  if not umlDirectoryExists(remoteinfo) then
      umlCreateDirectory(remoteinfo);

  fullfn := umlCombinePath(remoteinfo, fn);
  FCurrentReceiveStreamFileName := fullfn;
  try
      FCurrentStream := TCoreClassFileStream.Create(fullfn, fmCreate);
  except
      FRecvTunnel.ClientIO.Disconnect;
  end;
end;

procedure TCommunicationFramework_DoubleTunnelClient.Command_PostFile(Sender: TPeerClient; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64);
begin
  if FCurrentStream <> nil then
    begin
      InData.Position := 0;
      if InData.Size > 0 then
          FCurrentStream.CopyFrom(InData, InData.Size);
    end;
end;

procedure TCommunicationFramework_DoubleTunnelClient.Command_PostFileOver(Sender: TPeerClient; InData: TDataFrameEngine);
var
  servMD5, md5      : UnicodeMixedLib.TMD5;
  RemoteBackcallAddr: UInt64;
  p                 : PRemoteFileBackcall;
  fn                : string;
begin
  servMD5 := InData.Reader.ReadMD5;
  RemoteBackcallAddr := InData.Reader.ReadUInt64;
  p := Pointer(RemoteBackcallAddr);
  fn := FCurrentReceiveStreamFileName;

  if FCurrentStream <> nil then
    begin
      md5 := umlStreamMD5(FCurrentStream);
      if umlMD5Compare(servMD5, md5) then
          Sender.Print(Format('Receive %s ok', [umlGetFileName(fn).Text]))
      else
          Sender.Print(Format('Receive %s failed!', [umlGetFileName(fn).Text]));

      try
        if p <> nil then
          begin
            if Assigned(p^.OnComplete) then
              begin
                FCurrentStream.Position := 0;
                p^.OnComplete(p^.UserData, p^.UserObject, FCurrentStream, fn);
              end;
            dispose(p);
          end;
      except
      end;

      DisposeObject(FCurrentStream);
      FCurrentStream := nil;
    end;

  FRecvFileing := False;
end;

procedure TCommunicationFramework_DoubleTunnelClient.Command_PostFileOfBatchOver(Sender: TPeerClient; InData: TDataFrameEngine);
var
  RemoteBackcallAddr: UInt64;
  p                 : PRemoteFileBackcall;
  fn                : string;
begin
  RemoteBackcallAddr := InData.Reader.ReadUInt64;
  p := Pointer(RemoteBackcallAddr);
  fn := '';
  while InData.Reader.NotEnd do
    if fn <> '' then
        fn := fn + #13#10 + InData.Reader.ReadString
    else
        fn := InData.Reader.ReadString;

  InData.Reader.index := 0;

  FRecvFileOfBatching := False;

  try
    if p <> nil then
      begin
        if Assigned(p^.OnComplete) then
            p^.OnComplete(p^.UserData, p^.UserObject, nil, fn);
        dispose(p);
      end;
  except
  end;
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetFile_StreamParamResult(Sender: TPeerClient; Param1: Pointer; Param2: TObject; InData, ResultData: TDataFrameEngine);
var
  p: PRemoteFileBackcall;
begin
  if ResultData.Count > 0 then
    begin
      if ResultData.Reader.ReadBool then
        begin
          FRecvFileing := True;
          FRecvFileName := InData.ReadString(0);
          Exit;
        end;
      Sender.Print('GetPublicFile failed:%s', [ResultData.Reader.ReadString]);
    end;

  p := Param1;
  dispose(p);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPrivateFile_StreamParamResult(Sender: TPeerClient; Param1: Pointer; Param2: TObject; InData, ResultData: TDataFrameEngine);
var
  p: PRemoteFileBackcall;
begin
  if ResultData.Count > 0 then
    begin
      if ResultData.Reader.ReadBool then
        begin
          FRecvFileing := True;
          FRecvFileName := InData.ReadString(0);
          Exit;
        end;
      Sender.Print('GetPrivateFile failed:%s', [ResultData.Reader.ReadString]);
    end;

  p := Param1;
  dispose(p);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetUserPrivateFile_StreamParamResult(Sender: TPeerClient; Param1: Pointer; Param2: TObject; InData, ResultData: TDataFrameEngine);
var
  p: PRemoteFileBackcall;
begin
  if ResultData.Count > 0 then
    begin
      if ResultData.Reader.ReadBool then
        begin
          FRecvFileing := True;
          FRecvFileName := InData.ReadString(0);
          Exit;
        end;
      Sender.Print('GetUserPrivateFile failed:%s', [ResultData.Reader.ReadString]);
    end;

  p := Param1;
  dispose(p);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetCurrentCadencer_StreamResult(Sender: TPeerClient; ResultData: TDataFrameEngine);
var
  servTime: Double;
begin
  servTime := ResultData.Reader.ReadDouble;

  FCadencerEngine.Progress;
  FServerDelay := FCadencerEngine.CurrentTime - FLastCadencerTime;

  FCadencerEngine.CurrentTime := servTime + FServerDelay;
  FCadencerEngine.Progress;
end;

procedure TCommunicationFramework_DoubleTunnelClient.ClientConnected(Sender: TCommunicationFrameworkClient);
begin
end;

procedure TCommunicationFramework_DoubleTunnelClient.ClientDisconnect(Sender: TCommunicationFrameworkClient);
begin
end;

constructor TCommunicationFramework_DoubleTunnelClient.Create(ARecvTunnel, ASendTunnel: TCommunicationFrameworkClient);
begin
  inherited Create;
  FRecvTunnel := ARecvTunnel;
  FRecvTunnel.NotyifyInterface := Self;
  FRecvTunnel.PeerClientUserDefineClass := TPeerClientUserDefineForDoubleTunnelClient;

  FSendTunnel := ASendTunnel;
  FSendTunnel.NotyifyInterface := Self;
  FSendTunnel.PeerClientUserDefineClass := TPeerClientUserDefineForDoubleTunnelClient;

  FCurrentStream := nil;
  FCurrentReceiveStreamFileName := '';
  FLinkOk := False;
  FWaitCommandTimeout := 8000;

  FRecvFileing := False;
  FRecvFileOfBatching := False;
  FRecvFileName := '';

  FCadencerEngine := TCadencer.Create;
  {$IFDEF FPC}
  FCadencerEngine.OnProgress := @CadencerProgress;
  {$ELSE}
  FCadencerEngine.OnProgress := CadencerProgress;
  {$ENDIF}
  FLastCadencerTime := 0;
  FServerDelay := 0;

  FProgressEngine := TNProgressPost.Create;

  SwitchServiceAsDefaultPerformance;
end;

destructor TCommunicationFramework_DoubleTunnelClient.Destroy;
begin
  if FCurrentStream <> nil then
    begin
      DisposeObject(FCurrentStream);
      FCurrentStream := nil;
    end;

  DisposeObject(FCadencerEngine);
  DisposeObject(FProgressEngine);
  inherited Destroy;
end;

function TCommunicationFramework_DoubleTunnelClient.Connected: Boolean;
begin
  Result := FSendTunnel.Connected and FRecvTunnel.Connected;
end;

procedure TCommunicationFramework_DoubleTunnelClient.SwitchServiceAsMaxPerformance;
begin
  FRecvTunnel.SwitchMaxPerformance;
  FSendTunnel.SwitchMaxPerformance;
end;

procedure TCommunicationFramework_DoubleTunnelClient.SwitchServiceAsMaxSafe;
begin
  FRecvTunnel.SwitchMaxSafe;
  FSendTunnel.SwitchMaxSafe;
end;

procedure TCommunicationFramework_DoubleTunnelClient.SwitchServiceAsDefaultPerformance;
begin
  FRecvTunnel.SwitchDefaultPerformance;
  FSendTunnel.SwitchDefaultPerformance;
end;

procedure TCommunicationFramework_DoubleTunnelClient.Progress;
begin
  try
    FCadencerEngine.Progress;

    if Connected then
      begin
        FRecvTunnel.ProgressBackground;
        FSendTunnel.ProgressBackground;
      end
    else
      begin
        FLinkOk := False;
      end;
  except
  end;
end;

procedure TCommunicationFramework_DoubleTunnelClient.CadencerProgress(Sender: TObject; const deltaTime, newTime: Double);
begin
  FProgressEngine.Progress(deltaTime);
end;

function TCommunicationFramework_DoubleTunnelClient.UserLogin(UserID, Passwd: string): Boolean;
var
  sendDE, resDE: TDataFrameEngine;
begin
  Result := False;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;
  sendDE := TDataFrameEngine.Create;
  resDE := TDataFrameEngine.Create;

  sendDE.WriteCardinal(FRecvTunnel.RemoteID);
  sendDE.WriteString(UserID);
  sendDE.WriteString(Passwd);
  FSendTunnel.WaitSendStreamCmd('UserLogin', sendDE, resDE, FWaitCommandTimeout * 2);

  if resDE.Count > 0 then
    begin
      Result := resDE.ReadBool(0);
      FSendTunnel.ClientIO.Print(resDE.ReadString(1));
    end;

  DisposeObject(sendDE);
  DisposeObject(resDE);
end;

function TCommunicationFramework_DoubleTunnelClient.RegisterUser(UserID, Passwd: string): Boolean;
var
  sendDE, resDE: TDataFrameEngine;
begin
  Result := False;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;
  sendDE := TDataFrameEngine.Create;
  resDE := TDataFrameEngine.Create;

  sendDE.WriteCardinal(FRecvTunnel.RemoteID);
  sendDE.WriteString(UserID);
  sendDE.WriteString(Passwd);
  FSendTunnel.WaitSendStreamCmd('RegisterUser', sendDE, resDE, FWaitCommandTimeout * 2);

  if resDE.Count > 0 then
    begin
      Result := resDE.ReadBool(0);
      FSendTunnel.ClientIO.Print(resDE.ReadString(1));
    end;

  DisposeObject(sendDE);
  DisposeObject(resDE);
end;

function TCommunicationFramework_DoubleTunnelClient.TunnelLink: Boolean;
var
  sendDE, resDE: TDataFrameEngine;
begin
  if FLinkOk then
      Exit(True);
  Result := False;
  FLinkOk := False;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  SyncCadencer;

  sendDE := TDataFrameEngine.Create;
  resDE := TDataFrameEngine.Create;

  sendDE.WriteCardinal(FSendTunnel.RemoteID);
  sendDE.WriteCardinal(FRecvTunnel.RemoteID);

  FSendTunnel.WaitSendStreamCmd('TunnelLink', sendDE, resDE, FWaitCommandTimeout * 2);

  if resDE.Count > 0 then
    begin
      Result := resDE.ReadBool(0);
      FSendTunnel.ClientIO.Print(resDE.ReadString(1));

      if Result then
        begin
          TPeerClientUserDefineForDoubleTunnelClient(FSendTunnel.ClientIO.UserDefine).Client := Self;
          TPeerClientUserDefineForDoubleTunnelClient(FRecvTunnel.ClientIO.UserDefine).Client := Self;
          FLinkOk := True;
        end;
    end;

  DisposeObject(sendDE);
  DisposeObject(resDE);
end;

{$IFNDEF FPC}

procedure TCommunicationFramework_DoubleTunnelClient.UserLogin(UserID, Passwd: string; OnProc: TStateProc);
var
  sendDE: TDataFrameEngine;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;
  sendDE := TDataFrameEngine.Create;

  sendDE.WriteCardinal(FRecvTunnel.RemoteID);
  sendDE.WriteString(UserID);
  sendDE.WriteString(Passwd);
  FSendTunnel.SendStreamCmd('UserLogin', sendDE,
    procedure(Sender: TPeerClient; ResultData: TDataFrameEngine)
    var
      r: Boolean;
    begin
      r := False;
      if ResultData.Count > 0 then
        begin
          r := ResultData.ReadBool(0);
          FSendTunnel.ClientIO.Print(ResultData.ReadString(1));
        end;
      if Assigned(OnProc) then
          OnProc(r);
    end);

  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.RegisterUser(UserID, Passwd: string; OnProc: TStateProc);
var
  sendDE: TDataFrameEngine;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;
  sendDE := TDataFrameEngine.Create;

  sendDE.WriteCardinal(FRecvTunnel.RemoteID);
  sendDE.WriteString(UserID);
  sendDE.WriteString(Passwd);
  FSendTunnel.SendStreamCmd('RegisterUser', sendDE,
    procedure(Sender: TPeerClient; ResultData: TDataFrameEngine)
    var
      r: Boolean;
    begin
      r := False;
      if ResultData.Count > 0 then
        begin
          r := ResultData.ReadBool(0);
          FSendTunnel.ClientIO.Print(ResultData.ReadString(1));
        end;
      if Assigned(OnProc) then
          OnProc(r);
    end);

  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.TunnelLink(OnProc: TStateProc);
var
  sendDE: TDataFrameEngine;
begin
  if FLinkOk then
      Exit;

  FLinkOk := False;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  SyncCadencer;

  sendDE := TDataFrameEngine.Create;

  sendDE.WriteCardinal(FSendTunnel.RemoteID);
  sendDE.WriteCardinal(FRecvTunnel.RemoteID);

  FSendTunnel.SendStreamCmd('TunnelLink', sendDE,
    procedure(Sender: TPeerClient; ResultData: TDataFrameEngine)
    var
      r: Boolean;
    begin
      r := False;
      if ResultData.Count > 0 then
        begin
          r := ResultData.ReadBool(0);
          FSendTunnel.ClientIO.Print(ResultData.ReadString(1));

          if r then
            begin
              TPeerClientUserDefineForDoubleTunnelClient(FSendTunnel.ClientIO.UserDefine).Client := Self;
              TPeerClientUserDefineForDoubleTunnelClient(FRecvTunnel.ClientIO.UserDefine).Client := Self;
              FLinkOk := True;
            end;
        end;
      if Assigned(OnProc) then
          OnProc(r);
    end);

  DisposeObject(sendDE);
end;
{$ENDIF}


procedure TCommunicationFramework_DoubleTunnelClient.SyncCadencer;
var
  sendDE: TDataFrameEngine;
begin
  sendDE := TDataFrameEngine.Create;

  FCadencerEngine.Progress;
  FLastCadencerTime := FCadencerEngine.CurrentTime;
  FServerDelay := 0;
  sendDE.WriteDouble(FLastCadencerTime);
  {$IFDEF FPC}
  FSendTunnel.SendStreamCmd('GetCurrentCadencer', sendDE, @GetCurrentCadencer_StreamResult);
  {$ELSE}
  FSendTunnel.SendStreamCmd('GetCurrentCadencer', sendDE, GetCurrentCadencer_StreamResult);
  {$ENDIF}
  DisposeObject(sendDE);
end;

function TCommunicationFramework_DoubleTunnelClient.ChnagePassword(oldPasswd, newPasswd: string): Boolean;
var
  sendDE, resDE: TDataFrameEngine;
begin
  Result := False;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;
  sendDE := TDataFrameEngine.Create;
  resDE := TDataFrameEngine.Create;

  sendDE.WriteString(oldPasswd);
  sendDE.WriteString(newPasswd);

  FSendTunnel.WaitSendStreamCmd('ChangePasswd', sendDE, resDE, FWaitCommandTimeout);

  if resDE.Count = 2 then
      Result := resDE.ReadBool(0);

  DisposeObject(sendDE);
  DisposeObject(resDE);
end;

function TCommunicationFramework_DoubleTunnelClient.CustomNewUser(AUserID, APasswd: string; AUserConfigFile: TSectionTextData): Boolean;
var
  sendDE, resDE: TDataFrameEngine;
begin
  Result := False;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;
  sendDE := TDataFrameEngine.Create;
  resDE := TDataFrameEngine.Create;

  sendDE.WriteString(AUserID);
  sendDE.WriteString(APasswd);
  sendDE.WriteSectionText(AUserConfigFile);

  FSendTunnel.WaitSendStreamCmd('CustomNewUser', sendDE, resDE, FWaitCommandTimeout);

  if resDE.Count > 0 then
      Result := resDE.ReadBool(0);

  DisposeObject(sendDE);
  DisposeObject(resDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.ProcessStoreQueueCMD;
var
  sendDE: TDataFrameEngine;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;
  sendDE := TDataFrameEngine.Create;

  FSendTunnel.SendDirectStreamCmd('ProcessStoreQueueCMD', sendDE);

  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPublicFileList(Filter: string; lst: TCoreClassStrings);
var
  sendDE, resDE: TDataFrameEngine;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;
  sendDE := TDataFrameEngine.Create;
  resDE := TDataFrameEngine.Create;

  sendDE.WriteString(Filter);

  FSendTunnel.WaitSendStreamCmd('GetPublicFileList', sendDE, resDE, FWaitCommandTimeout);

  while not resDE.Reader.IsEnd do
      lst.Add(resDE.Reader.ReadString);

  DisposeObject(sendDE);
  DisposeObject(resDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPrivateFileList(Filter, directoryName: string; lst: TCoreClassStrings);
var
  sendDE, resDE: TDataFrameEngine;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;
  sendDE := TDataFrameEngine.Create;
  resDE := TDataFrameEngine.Create;

  sendDE.WriteString(Filter);
  sendDE.WriteString(directoryName);

  FSendTunnel.WaitSendStreamCmd('GetPrivateFileList', sendDE, resDE, FWaitCommandTimeout);

  while not resDE.Reader.IsEnd do
      lst.Add(resDE.Reader.ReadString);

  DisposeObject(sendDE);
  DisposeObject(resDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPrivateFileList(Filter: string; lst: TCoreClassStrings);
begin
  GetPrivateFileList(Filter, '', lst);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetUserPrivateFileList(UserID, Filter, directoryName: string; lst: TCoreClassStrings);
var
  sendDE, resDE: TDataFrameEngine;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;
  sendDE := TDataFrameEngine.Create;
  resDE := TDataFrameEngine.Create;

  sendDE.WriteString(UserID);
  sendDE.WriteString(Filter);
  sendDE.WriteString(directoryName);

  FSendTunnel.WaitSendStreamCmd('GetUserPrivateFileList', sendDE, resDE, FWaitCommandTimeout);

  while not resDE.Reader.IsEnd do
      lst.Add(resDE.Reader.ReadString);

  DisposeObject(sendDE);
  DisposeObject(resDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetUserPrivateFileList(UserID, Filter: string; lst: TCoreClassStrings);
begin
  GetUserPrivateFileList(UserID, Filter, '', lst);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPrivateDirectoryList(Filter, directoryName: string; lst: TCoreClassStrings);
var
  sendDE, resDE: TDataFrameEngine;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;
  sendDE := TDataFrameEngine.Create;
  resDE := TDataFrameEngine.Create;

  sendDE.WriteString(Filter);
  sendDE.WriteString(directoryName);

  FSendTunnel.WaitSendStreamCmd('GetPrivateDirectoryList', sendDE, resDE, FWaitCommandTimeout);

  while not resDE.Reader.IsEnd do
      lst.Add(resDE.Reader.ReadString);

  DisposeObject(sendDE);
  DisposeObject(resDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPrivateDirectoryList(Filter: string; lst: TCoreClassStrings);
begin
  GetPrivateDirectoryList(Filter, '', lst);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetUserPrivateDirectoryList(UserID, Filter, directoryName: string; lst: TCoreClassStrings);
var
  sendDE, resDE: TDataFrameEngine;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;
  sendDE := TDataFrameEngine.Create;
  resDE := TDataFrameEngine.Create;

  sendDE.WriteString(UserID);
  sendDE.WriteString(Filter);
  sendDE.WriteString(directoryName);

  FSendTunnel.WaitSendStreamCmd('GetUserPrivateDirectoryList', sendDE, resDE, FWaitCommandTimeout);

  while not resDE.Reader.IsEnd do
      lst.Add(resDE.Reader.ReadString);

  DisposeObject(sendDE);
  DisposeObject(resDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetUserPrivateDirectoryList(UserID, Filter: string; lst: TCoreClassStrings);
begin
  GetUserPrivateDirectoryList(UserID, Filter, '', lst);
end;

function TCommunicationFramework_DoubleTunnelClient.CreatePrivateDirectory(directoryName: string): Boolean;
var
  sendDE, resDE: TDataFrameEngine;
begin
  Result := False;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;
  sendDE := TDataFrameEngine.Create;
  resDE := TDataFrameEngine.Create;

  sendDE.WriteString(directoryName);

  FSendTunnel.WaitSendStreamCmd('CreatePrivateDirectory', sendDE, resDE, FWaitCommandTimeout);

  if resDE.Count > 0 then
    begin
      Result := resDE.Reader.ReadBool;
      FSendTunnel.ClientIO.Print(resDE.Reader.ReadString);
    end;

  DisposeObject(sendDE);
  DisposeObject(resDE);
end;

function TCommunicationFramework_DoubleTunnelClient.GetPublicFile(fileName, saveToPath: string): Boolean;
var
  sendDE, resDE: TDataFrameEngine;
begin
  Result := False;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDataFrameEngine.Create;
  resDE := TDataFrameEngine.Create;

  sendDE.WriteString(fileName);
  sendDE.WriteString(saveToPath);
  sendDE.WriteUInt64(0);

  FSendTunnel.WaitSendStreamCmd('GetPublicFile', sendDE, resDE, FWaitCommandTimeout);

  if resDE.Count > 0 then
    begin
      Result := resDE.Reader.ReadBool;
      FSendTunnel.ClientIO.Print(resDE.Reader.ReadString);

      if Result then
        begin
          FRecvFileing := True;
          FRecvFileName := fileName;
        end;
    end;

  DisposeObject(sendDE);
  DisposeObject(resDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPublicFile(fileName, saveToPath: string;
const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileComplete);
var
  sendDE: TDataFrameEngine;
  p     : PRemoteFileBackcall;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDataFrameEngine.Create;

  sendDE.WriteString(fileName);
  sendDE.WriteString(saveToPath);
  new(p);
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.OnComplete := OnComplete;
  sendDE.WriteUInt64(UInt64(Pointer(p)));

  {$IFDEF FPC}
  FSendTunnel.SendStreamCmd('GetPublicFile', sendDE, p, nil, @GetFile_StreamParamResult);
  {$ELSE}
  FSendTunnel.SendStreamCmd('GetPublicFile', sendDE, p, nil, GetFile_StreamParamResult);
  {$ENDIF}
  DisposeObject(sendDE);
end;

function TCommunicationFramework_DoubleTunnelClient.GetPrivateFile(fileName, directoryName, saveToPath: string): Boolean;
var
  sendDE, resDE: TDataFrameEngine;
begin
  Result := False;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDataFrameEngine.Create;
  resDE := TDataFrameEngine.Create;

  sendDE.WriteString(fileName);
  sendDE.WriteString(directoryName);
  sendDE.WriteString(saveToPath);
  sendDE.WriteUInt64(0);

  FSendTunnel.WaitSendStreamCmd('GetPrivateFile', sendDE, resDE, FWaitCommandTimeout);

  if resDE.Count > 0 then
    begin
      Result := resDE.Reader.ReadBool;
      FSendTunnel.ClientIO.Print(resDE.Reader.ReadString);
      if Result then
        begin
          FRecvFileing := True;
          FRecvFileName := fileName;
        end;
    end;

  DisposeObject(sendDE);
  DisposeObject(resDE);
end;

function TCommunicationFramework_DoubleTunnelClient.GetPrivateFile(fileName, saveToPath: string): Boolean;
begin
  Result := GetPrivateFile(fileName, '', saveToPath);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPrivateFile(fileName, directoryName, saveToPath: string;
const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileComplete);
var
  sendDE: TDataFrameEngine;
  p     : PRemoteFileBackcall;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDataFrameEngine.Create;

  sendDE.WriteString(fileName);
  sendDE.WriteString(directoryName);
  sendDE.WriteString(saveToPath);
  new(p);
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.OnComplete := OnComplete;
  sendDE.WriteUInt64(UInt64(Pointer(p)));

  {$IFDEF FPC}
  FSendTunnel.SendStreamCmd('GetPrivateFile', sendDE, p, nil, @GetPrivateFile_StreamParamResult);
  {$ELSE}
  FSendTunnel.SendStreamCmd('GetPrivateFile', sendDE, p, nil, GetPrivateFile_StreamParamResult);
  {$ENDIF}
  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPrivateFileOfBatch(Filter, directoryName, saveToPath: string);
var
  sendDE: TDataFrameEngine;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDataFrameEngine.Create;

  sendDE.WriteString(Filter);
  sendDE.WriteString(directoryName);
  sendDE.WriteString(saveToPath);
  sendDE.WriteUInt64(0);

  FRecvFileOfBatching := True;
  FSendTunnel.SendDirectStreamCmd('GetPrivateFileOfBatch', sendDE);

  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPrivateFileOfBatch(Filter, directoryName, saveToPath: string;
const UserData: Pointer; const UserObject: TCoreClassObject; const OnAllComplete: TFileComplete);
var
  sendDE: TDataFrameEngine;
  p     : PRemoteFileBackcall;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDataFrameEngine.Create;

  sendDE.WriteString(Filter);
  sendDE.WriteString(directoryName);
  sendDE.WriteString(saveToPath);
  new(p);
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.OnComplete := OnAllComplete;
  sendDE.WriteUInt64(UInt64(Pointer(p)));

  FRecvFileOfBatching := True;
  FSendTunnel.SendDirectStreamCmd('GetPrivateFileOfBatch', sendDE);

  DisposeObject(sendDE);
end;

function TCommunicationFramework_DoubleTunnelClient.GetUserPrivateFile(UserID, fileName, directoryName, saveToPath: string): Boolean;
var
  sendDE, resDE: TDataFrameEngine;
begin
  Result := False;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDataFrameEngine.Create;
  resDE := TDataFrameEngine.Create;

  sendDE.WriteString(UserID);
  sendDE.WriteString(fileName);
  sendDE.WriteString(directoryName);
  sendDE.WriteString(saveToPath);
  sendDE.WriteUInt64(0);

  FSendTunnel.WaitSendStreamCmd('GetUserPrivateFile', sendDE, resDE, FWaitCommandTimeout);

  if resDE.Count > 0 then
    begin
      Result := resDE.Reader.ReadBool;
      FSendTunnel.ClientIO.Print(resDE.Reader.ReadString);
      if Result then
        begin
          FRecvFileing := True;
          FRecvFileName := fileName;
        end;
    end;

  DisposeObject(sendDE);
  DisposeObject(resDE);
end;

function TCommunicationFramework_DoubleTunnelClient.GetUserPrivateFile(UserID, fileName, saveToPath: string): Boolean;
begin
  Result := GetUserPrivateFile(UserID, fileName, '', saveToPath);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetUserPrivateFile(UserID, fileName, directoryName, saveToPath: string;
const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileComplete);
var
  sendDE: TDataFrameEngine;
  p     : PRemoteFileBackcall;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDataFrameEngine.Create;

  sendDE.WriteString(UserID);
  sendDE.WriteString(fileName);
  sendDE.WriteString(directoryName);
  sendDE.WriteString(saveToPath);
  new(p);
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.OnComplete := OnComplete;
  sendDE.WriteUInt64(UInt64(Pointer(p)));

  {$IFDEF FPC}
  FSendTunnel.SendStreamCmd('GetUserPrivateFile', sendDE, p, nil, @GetUserPrivateFile_StreamParamResult);
  {$ELSE}
  FSendTunnel.SendStreamCmd('GetUserPrivateFile', sendDE, p, nil, GetUserPrivateFile_StreamParamResult);
  {$ENDIF}
  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetUserPrivateFileOfBatch(UserID, Filter, directoryName, saveToPath: string);
var
  sendDE: TDataFrameEngine;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDataFrameEngine.Create;

  sendDE.WriteString(UserID);
  sendDE.WriteString(Filter);
  sendDE.WriteString(directoryName);
  sendDE.WriteString(saveToPath);
  sendDE.WriteUInt64(0);

  FRecvFileOfBatching := True;
  FSendTunnel.SendDirectStreamCmd('GetUserPrivateFileOfBatch', sendDE);

  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetUserPrivateFileOfBatch(UserID, Filter, directoryName, saveToPath: string;
const UserData: Pointer; const UserObject: TCoreClassObject; const OnAllComplete: TFileComplete);
var
  sendDE: TDataFrameEngine;
  p     : PRemoteFileBackcall;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDataFrameEngine.Create;

  sendDE.WriteString(UserID);
  sendDE.WriteString(Filter);
  sendDE.WriteString(directoryName);
  sendDE.WriteString(saveToPath);
  new(p);
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.OnComplete := OnAllComplete;
  sendDE.WriteUInt64(UInt64(Pointer(p)));

  FRecvFileOfBatching := True;
  FSendTunnel.SendDirectStreamCmd('GetUserPrivateFileOfBatch', sendDE);

  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.PostFileToPublic(fileName: string);
var
  sendDE: TDataFrameEngine;
  fs    : TCoreClassFileStream;
  md5   : UnicodeMixedLib.TMD5;
begin
  if not umlFileExists(fileName) then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  fs := TCoreClassFileStream.Create(fileName, fmOpenRead or fmShareDenyWrite);

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteString(umlGetFileName(fileName));
  sendDE.WriteInt64(fs.Size);
  FSendTunnel.SendDirectStreamCmd('PostPublicFileInfo', sendDE);
  DisposeObject(sendDE);

  md5 := umlStreamMD5(fs);

  fs.Position := 0;
  FSendTunnel.SendBigStream('PostFile', fs, True);

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteMD5(md5);
  FSendTunnel.SendDirectStreamCmd('PostFileOver', sendDE);
  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.PostFileToPrivate(fileName, directoryName: string);
var
  sendDE: TDataFrameEngine;
  fs    : TCoreClassFileStream;
  md5   : UnicodeMixedLib.TMD5;
begin
  if not umlFileExists(fileName) then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  fs := TCoreClassFileStream.Create(fileName, fmOpenRead or fmShareDenyWrite);

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteString(umlGetFileName(fileName));
  sendDE.WriteString(directoryName);
  sendDE.WriteInt64(fs.Size);
  FSendTunnel.SendDirectStreamCmd('PostPrivateFileInfo', sendDE);
  DisposeObject(sendDE);

  md5 := umlStreamMD5(fs);

  fs.Position := 0;
  FSendTunnel.SendBigStream('PostFile', fs, True);

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteMD5(md5);
  FSendTunnel.SendDirectStreamCmd('PostFileOver', sendDE);
  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.PostFileToPrivate(fileName: string);
begin
  PostFileToPrivate(fileName, '');
end;

procedure TCommunicationFramework_DoubleTunnelClient.PostStreamToPrivate(FileFlagName, directoryName: string; Stream: TCoreClassStream; DoneFreeStream: Boolean);
var
  sendDE: TDataFrameEngine;
  md5   : UnicodeMixedLib.TMD5;
begin
  if not FSendTunnel.Connected then
    begin
      if DoneFreeStream then
          DisposeObject(Stream);
      Exit;
    end;
  if not FRecvTunnel.Connected then
    begin
      if DoneFreeStream then
          DisposeObject(Stream);
      Exit;
    end;

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteString(FileFlagName);
  sendDE.WriteString(directoryName);
  sendDE.WriteInt64(Stream.Size);
  FSendTunnel.SendDirectStreamCmd('PostPrivateFileInfo', sendDE);
  DisposeObject(sendDE);

  md5 := umlStreamMD5(Stream);

  Stream.Position := 0;
  FSendTunnel.SendBigStream('PostFile', Stream, DoneFreeStream);

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteMD5(md5);
  FSendTunnel.SendDirectStreamCmd('PostFileOver', sendDE);
  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.RegisterCommand;
begin
  {$IFDEF FPC}
  FRecvTunnel.RegisterDirectStream('FileInfo').OnExecute := @Command_FileInfo;
  FRecvTunnel.RegisterBigStream('PostFile').OnExecute := @Command_PostFile;
  FRecvTunnel.RegisterDirectStream('PostFileOver').OnExecute := @Command_PostFileOver;
  FRecvTunnel.RegisterDirectStream('PostFileOfBatchOver').OnExecute := @Command_PostFileOfBatchOver;
  {$ELSE}
  FRecvTunnel.RegisterDirectStream('FileInfo').OnExecute := Command_FileInfo;
  FRecvTunnel.RegisterBigStream('PostFile').OnExecute := Command_PostFile;
  FRecvTunnel.RegisterDirectStream('PostFileOver').OnExecute := Command_PostFileOver;
  FRecvTunnel.RegisterDirectStream('PostFileOfBatchOver').OnExecute := Command_PostFileOfBatchOver;
  {$ENDIF}
end;

procedure TCommunicationFramework_DoubleTunnelClient.UnRegisterCommand;
begin
  FRecvTunnel.DeleteRegistedCMD('FileInfo');
  FRecvTunnel.DeleteRegistedCMD('PostFile');
  FRecvTunnel.DeleteRegistedCMD('PostFileOver');
  FRecvTunnel.DeleteRegistedCMD('PostFileOfBatchOver');
end;

function TCommunicationFramework_DoubleTunnelClient.RemoteInited: Boolean;
begin
  Result := FSendTunnel.RemoteInited and FRecvTunnel.RemoteInited;
end;

end.
