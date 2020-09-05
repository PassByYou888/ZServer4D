{ ****************************************************************************** }
{ * double tunnel IO framework(incl Auth and File service)                     * }
{ * written by QQ 600585@qq.com                                                * }
{ * https://zpascal.net                                                        * }
{ * https://github.com/PassByYou888/zAI                                        * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/PascalString                               * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zChinese                                   * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/FFMPEG-Header                              * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/InfiniteIoT                                * }
{ * https://github.com/PassByYou888/FastMD5                                    * }
{ ****************************************************************************** }

unit CommunicationFrameworkDoubleTunnelIO;

{$INCLUDE zDefine.inc}

interface


uses CoreClasses,
  ListEngine, UnicodeMixedLib,
  DataFrameEngine, MemoryStream64, CommunicationFramework, TextDataEngine,
  DoStatusIO, Cadencer, NotifyObjectBase, FilePackage,
  ObjectDataManager, CoreCipher, PascalStrings;

type
  TCommunicationFramework_DoubleTunnelService = class;
  TPeerClientUserDefineForRecvTunnel = class;

  TPeerClientUserDefineForSendTunnel = class(TPeerClientUserDefine)
  public
    RecvTunnel: TPeerClientUserDefineForRecvTunnel;
    RecvTunnelID: Cardinal;
    DoubleTunnelService: TCommunicationFramework_DoubleTunnelService;

    constructor Create(Owner_: TPeerIO); override;
    destructor Destroy; override;
    function LinkOk: Boolean;
    property BindOk: Boolean read LinkOk;
  end;

  TPeerClientUserDefineForRecvTunnel = class(TPeerClientUserDefine)
  public
    SendTunnel: TPeerClientUserDefineForSendTunnel;
    SendTunnelID: Cardinal;
    UserFlag, UserID: SystemString;
    UserPath: SystemString;
    UserConfigFile: TSectionTextData;
    DoubleTunnelService: TCommunicationFramework_DoubleTunnelService;
    UserDBIntf: THashVariantList;
    LoginSuccessed: Boolean;
    FCurrentFileStream: TCoreClassStream;
    FCurrentReceiveFileName: SystemString;

    WaitLink: Boolean;
    WaitLinkSendID: Cardinal;

    constructor Create(Owner_: TPeerIO); override;
    destructor Destroy; override;

    function MakeFilePath(fn: SystemString): SystemString;
    function GetUserID: SystemString;

    procedure SaveConfigFile; virtual;

    function LinkOk: Boolean;
    property BindOk: Boolean read LinkOk;
    property CurrentFileStream: TCoreClassStream read FCurrentFileStream write FCurrentFileStream;
    property CurrentReceiveFileName: SystemString read FCurrentReceiveFileName write FCurrentReceiveFileName;
  end;

  TOnLinkSuccess = procedure(Sender: TCommunicationFramework_DoubleTunnelService; UserDefineIO: TPeerClientUserDefineForRecvTunnel) of object;
  TOnUserOut = procedure(Sender: TCommunicationFramework_DoubleTunnelService; UserDefineIO: TPeerClientUserDefineForRecvTunnel) of object;

  TCommunicationFramework_DoubleTunnelService = class(TCoreClassInterfacedObject)
  protected
    FRecvTunnel, FSendTunnel: TCommunicationFrameworkServer;
    FRootPath, FPublicPath: SystemString;
    FUserDB: TSectionTextData;
    FCanRegisterNewUser: Boolean;
    FCanSaveUserInfo: Boolean;
    FLoginUserList: THashVariantList;
    FLoginUserDefineIOList: THashObjectList;
    FCadencerEngine: TCadencer;
    FProgressEngine: TNProgressPost;
    { event }
    FOnLinkSuccess: TOnLinkSuccess;
    FOnUserOut: TOnUserOut;
  protected
    { virtual event }
    procedure UserRegistedSuccess(UserID: SystemString); virtual;
    procedure UserLoginSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel); virtual;
    procedure UserLinkSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel); virtual;
    procedure UserCreateDirectorySuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel; dn: SystemString); virtual;
    procedure UserPostFileSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel; fn: SystemString); virtual;
    procedure UserOut(UserDefineIO: TPeerClientUserDefineForRecvTunnel); virtual;
  protected
    { registed server command }
    procedure Command_UserLogin(Sender: TPeerIO; InData, OutData: TDataFrameEngine); virtual;
    procedure Command_RegisterUser(Sender: TPeerIO; InData, OutData: TDataFrameEngine); virtual;
    procedure Command_TunnelLink(Sender: TPeerIO; InData, OutData: TDataFrameEngine); virtual;

    procedure Command_ChangePasswd(Sender: TPeerIO; InData, OutData: TDataFrameEngine); virtual;
    procedure Command_CustomNewUser(Sender: TPeerIO; InData, OutData: TDataFrameEngine); virtual;

    procedure Command_ProcessStoreQueueCMD(Sender: TPeerIO; InData: TDataFrameEngine); virtual;

    procedure Command_GetPublicFileList(Sender: TPeerIO; InData, OutData: TDataFrameEngine); virtual;
    procedure Command_GetPrivateFileList(Sender: TPeerIO; InData, OutData: TDataFrameEngine); virtual;
    procedure Command_GetPrivateDirectoryList(Sender: TPeerIO; InData, OutData: TDataFrameEngine); virtual;

    procedure Command_CreatePrivateDirectory(Sender: TPeerIO; InData, OutData: TDataFrameEngine); virtual;

    procedure Command_GetPublicFileInfo(Sender: TPeerIO; InData, OutData: TDataFrameEngine); virtual;
    procedure Command_GetPrivateFileInfo(Sender: TPeerIO; InData, OutData: TDataFrameEngine); virtual;

    procedure Command_GetPublicFileMD5(Sender: TPeerIO; InData, OutData: TDataFrameEngine); virtual;
    procedure Command_GetPrivateFileMD5(Sender: TPeerIO; InData, OutData: TDataFrameEngine); virtual;

    procedure Command_GetPublicFile(Sender: TPeerIO; InData, OutData: TDataFrameEngine); virtual;
    procedure Command_GetPrivateFile(Sender: TPeerIO; InData, OutData: TDataFrameEngine); virtual;
    procedure Command_GetUserPrivateFile(Sender: TPeerIO; InData, OutData: TDataFrameEngine); virtual;

    procedure Command_PostPublicFileInfo(Sender: TPeerIO; InData: TDataFrameEngine); virtual;
    procedure Command_PostPrivateFileInfo(Sender: TPeerIO; InData: TDataFrameEngine); virtual;
    procedure Command_PostFile(Sender: TPeerIO; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64); virtual;
    procedure Command_PostFileOver(Sender: TPeerIO; InData: TDataFrameEngine); virtual;

    procedure Command_GetPublicFileFragmentData(Sender: TPeerIO; InData, OutData: TDataFrameEngine); virtual;
    procedure Command_GetPrivateFileFragmentData(Sender: TPeerIO; InData, OutData: TDataFrameEngine); virtual;

    procedure Command_GetCurrentCadencer(Sender: TPeerIO; InData, OutData: TDataFrameEngine); virtual;

    procedure Command_NewBatchStream(Sender: TPeerIO; InData: TDataFrameEngine); virtual;
    procedure Command_PostBatchStream(Sender: TPeerIO; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64); virtual;
    procedure Command_ClearBatchStream(Sender: TPeerIO; InData: TDataFrameEngine); virtual;
    procedure Command_PostBatchStreamDone(Sender: TPeerIO; InData: TDataFrameEngine); virtual;
    procedure Command_GetBatchStreamState(Sender: TPeerIO; InData, OutData: TDataFrameEngine); virtual;
  public
    constructor Create(RecvTunnel_, SendTunnel_: TCommunicationFrameworkServer);
    destructor Destroy; override;

    procedure SwitchAsMaxPerformance;
    procedure SwitchAsMaxSecurity;
    procedure SwitchAsDefaultPerformance;

    procedure Progress; virtual;
    procedure CadencerProgress(Sender: TObject; const deltaTime, newTime: Double); virtual;

    {
      load USERDB
      load need execute in rootpath set completed after
    }
    procedure LoadUserDB;

    procedure SaveUserDB;

    function RegUser(UsrID, UsrPasswd: SystemString; AUserConfigFile: TSectionTextData): Boolean;
    function ExistsUser(UsrID: SystemString): Boolean;
    function GetUserPath(UsrID: SystemString): SystemString;
    function GetUserFile(UsrID, AUserFileName: SystemString): SystemString;
    function GetUserDefineIO(UsrID: SystemString): TPeerClientUserDefineForRecvTunnel;
    function UserOnline(UsrID: SystemString): Boolean;

    function PackUserAsFile(UsrID, packageFile: SystemString): Boolean;
    function PackUserAsStream(UsrID: SystemString; packageStream: TCoreClassStream): Boolean;
    function UnPackFileAsUser(packageFile: SystemString): Boolean;
    function UnPackStreamAsUser(packageStream: TCoreClassStream): Boolean;

    {
      only work in direct command
      if user online immediate execution
      if user offline store to notify queue
    }
    procedure PostStoreQueueCMD(ToUserID: SystemString; Cmd: SystemString; InData: TDataFrameEngine);

    procedure RegisterCommand; virtual;
    procedure UnRegisterCommand; virtual;

    function MakeUserFlag: SystemString;
    function GetUserDefineRecvTunnel(RecvCli: TPeerIO): TPeerClientUserDefineForRecvTunnel;

    function TotalLinkCount: Integer;

    procedure PostBatchStream(cli: TPeerIO; stream: TCoreClassStream; doneFreeStream: Boolean); overload;
    procedure PostBatchStreamC(cli: TPeerIO; stream: TCoreClassStream; doneFreeStream: Boolean; OnCompletedBackcall: TStateCall); overload;
    procedure PostBatchStreamM(cli: TPeerIO; stream: TCoreClassStream; doneFreeStream: Boolean; OnCompletedBackcall: TStateMethod); overload;
    procedure PostBatchStreamP(cli: TPeerIO; stream: TCoreClassStream; doneFreeStream: Boolean; OnCompletedBackcall: TStateProc); overload;
    procedure ClearBatchStream(cli: TPeerIO);
    procedure GetBatchStreamStateM(cli: TPeerIO; OnResult: TStreamMethod); overload;
    procedure GetBatchStreamStateP(cli: TPeerIO; OnResult: TStreamProc); overload;
    property LoginUserList: THashVariantList read FLoginUserList;

    property CanRegisterNewUser: Boolean read FCanRegisterNewUser write FCanRegisterNewUser;
    property CanSaveUserInfo: Boolean read FCanSaveUserInfo write FCanSaveUserInfo;

    { private store space }
    property RootPath: SystemString read FRootPath write FRootPath;
    { public store space }
    property PublicPath: SystemString read FPublicPath write FPublicPath;

    property CadencerEngine: TCadencer read FCadencerEngine;

    property ProgressEngine: TNProgressPost read FProgressEngine;
    property ProgressPost: TNProgressPost read FProgressEngine;
    property PostProgress: TNProgressPost read FProgressEngine;
    property PostRun: TNProgressPost read FProgressEngine;
    property PostExecute: TNProgressPost read FProgressEngine;

    property RecvTunnel: TCommunicationFrameworkServer read FRecvTunnel;
    property SendTunnel: TCommunicationFrameworkServer read FSendTunnel;

    property OnLinkSuccess: TOnLinkSuccess read FOnLinkSuccess write FOnLinkSuccess;
    property OnUserOut: TOnUserOut read FOnUserOut write FOnUserOut;
  end;

  TCommunicationFramework_DoubleTunnelClient = class;

  TClientUserDefineForSendTunnel = class;

  TClientUserDefineForRecvTunnel = class(TPeerClientUserDefine)
  public
    Client: TCommunicationFramework_DoubleTunnelClient;
    SendTunnel: TClientUserDefineForSendTunnel;

    constructor Create(Owner_: TPeerIO); override;
    destructor Destroy; override;
  end;

  TClientUserDefineForSendTunnel = class(TPeerClientUserDefine)
  public
    Client: TCommunicationFramework_DoubleTunnelClient;
    RecvTunnel: TClientUserDefineForRecvTunnel;

    constructor Create(Owner_: TPeerIO); override;
    destructor Destroy; override;
  end;

  TGetFileInfoCall = procedure(const UserData: Pointer; const UserObject: TCoreClassObject;
    const fileName: SystemString; const Existed: Boolean; const fSiz: Int64);
  TGetFileInfoMethod = procedure(const UserData: Pointer; const UserObject: TCoreClassObject;
    const fileName: SystemString; const Existed: Boolean; const fSiz: Int64) of object;
  TFileMD5Call = procedure(const UserData: Pointer; const UserObject: TCoreClassObject;
    const fileName: SystemString; const StartPos, EndPos: Int64; const MD5: TMD5);
  TFileMD5Method = procedure(const UserData: Pointer; const UserObject: TCoreClassObject;
    const fileName: SystemString; const StartPos, EndPos: Int64; const MD5: TMD5) of object;
  TFileCompleteCall = procedure(const UserData: Pointer; const UserObject: TCoreClassObject;
    stream: TCoreClassStream; const fileName: SystemString);
  TFileCompleteMethod = procedure(const UserData: Pointer; const UserObject: TCoreClassObject;
    stream: TCoreClassStream; const fileName: SystemString) of object;
  TFileFragmentDataCall = procedure(const UserData: Pointer; const UserObject: TCoreClassObject;
    const fileName: SystemString; const StartPos, EndPos: Int64; const DataPtr: Pointer; const DataSize: Int64; const MD5: TMD5);
  TFileFragmentDataMethod = procedure(const UserData: Pointer; const UserObject: TCoreClassObject;
    const fileName: SystemString; const StartPos, EndPos: Int64; const DataPtr: Pointer; const DataSize: Int64; const MD5: TMD5) of object;

{$IFDEF FPC}
  TGetFileInfoProc = procedure(const UserData: Pointer; const UserObject: TCoreClassObject;
    const fileName: SystemString; const Existed: Boolean; const fSiz: Int64) is nested;
  TFileMD5Proc = procedure(const UserData: Pointer; const UserObject: TCoreClassObject;
    const fileName: SystemString; const StartPos, EndPos: Int64; const MD5: TMD5) is nested;
  TFileCompleteProc = procedure(const UserData: Pointer; const UserObject: TCoreClassObject; stream:
    TCoreClassStream; const fileName: SystemString) is nested;
  TFileFragmentDataProc = procedure(const UserData: Pointer; const UserObject: TCoreClassObject;
    const fileName: SystemString; const StartPos, EndPos: Int64; const DataPtr: Pointer; const DataSize: Int64; const MD5: TMD5) is nested;
{$ELSE FPC}
  TGetFileInfoProc = reference to procedure(const UserData: Pointer; const UserObject: TCoreClassObject;
    const fileName: SystemString; const Existed: Boolean; const fSiz: Int64);
  TFileMD5Proc = reference to procedure(const UserData: Pointer; const UserObject: TCoreClassObject;
    const fileName: SystemString; const StartPos, EndPos: Int64; const MD5: TMD5);
  TFileCompleteProc = reference to procedure(const UserData: Pointer; const UserObject: TCoreClassObject; stream:
    TCoreClassStream; const fileName: SystemString);
  TFileFragmentDataProc = reference to procedure(const UserData: Pointer; const UserObject: TCoreClassObject;
    const fileName: SystemString; const StartPos, EndPos: Int64; const DataPtr: Pointer; const DataSize: Int64; const MD5: TMD5);
{$ENDIF FPC}

  TCommunicationFramework_DoubleTunnelClient = class(TCoreClassInterfacedObject, ICommunicationFrameworkClientInterface)
  protected
    FSendTunnel, FRecvTunnel: TCommunicationFrameworkClient;
    FCurrentStream: TCoreClassStream;
    FCurrentReceiveStreamFileName: SystemString;
    FLinkOk: Boolean;
    FWaitCommandTimeout: Cardinal;

    FRecvFileing: Boolean;
    FRecvFileOfBatching: Boolean;
    FRecvFileName: SystemString;

    FCadencerEngine: TCadencer;
    FLastCadencerTime: Double;
    FServerDelay: Double;

    FProgressEngine: TNProgressPost;
  public
    { registed client command }
    procedure Command_FileInfo(Sender: TPeerIO; InData: TDataFrameEngine); virtual;
    procedure Command_PostFile(Sender: TPeerIO; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64); virtual;
    procedure Command_PostFileOver(Sender: TPeerIO; InData: TDataFrameEngine); virtual;
    procedure Command_PostFileFragmentData(Sender: TPeerIO; InData: PByte; DataSize: NativeInt); virtual;

    procedure GetPublicFileInfo_StreamParamResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, ResultData: TDataFrameEngine); virtual;
    procedure GetPrivateFileInfo_StreamParamResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, ResultData: TDataFrameEngine); virtual;

    procedure GetPublicFileMD5_StreamParamResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, ResultData: TDataFrameEngine); virtual;
    procedure GetPrivateFileMD5_StreamParamResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, ResultData: TDataFrameEngine); virtual;

    procedure GetPublicFile_StreamParamResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, ResultData: TDataFrameEngine); virtual;
    procedure GetPrivateFile_StreamParamResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, ResultData: TDataFrameEngine); virtual;

    { Downloading file fragment data from the server asynchronously and triggering notifications when completed }
    procedure GetFileFragmentData_StreamParamResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, ResultData: TDataFrameEngine); virtual;

    { GetCurrentCadencer result proc }
    procedure GetCurrentCadencer_StreamResult(Sender: TPeerIO; ResultData: TDataFrameEngine); virtual;

    { batch stream suppport }
    procedure Command_NewBatchStream(Sender: TPeerIO; InData: TDataFrameEngine); virtual;
    procedure Command_PostBatchStream(Sender: TPeerIO; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64); virtual;
    procedure Command_ClearBatchStream(Sender: TPeerIO; InData: TDataFrameEngine); virtual;
    procedure Command_PostBatchStreamDone(Sender: TPeerIO; InData: TDataFrameEngine); virtual;
    procedure Command_GetBatchStreamState(Sender: TPeerIO; InData, OutData: TDataFrameEngine); virtual;
  protected
    { client notify interface }
    procedure ClientConnected(Sender: TCommunicationFrameworkClient); virtual;
    procedure ClientDisconnect(Sender: TCommunicationFrameworkClient); virtual;
  protected
    { async connect support }
    FAsyncConnectAddr: SystemString;
    FAsyncConnRecvPort, FAsyncConnSendPort: Word;
    FAsyncOnResultCall: TStateCall;
    FAsyncOnResultMethod: TStateMethod;
    FAsyncOnResultProc: TStateProc;
    procedure AsyncSendConnectResult(const cState: Boolean);
    procedure AsyncRecvConnectResult(const cState: Boolean);
    procedure UserLogin_OnResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, ResultData: TDataFrameEngine);
    procedure UserLogin_OnFailed(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDataFrameEngine);
    procedure RegisterUser_OnResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, ResultData: TDataFrameEngine);
    procedure RegisterUser_OnFailed(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDataFrameEngine);
    procedure TunnelLink_OnResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, ResultData: TDataFrameEngine);
    procedure TunnelLink_OnFailed(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDataFrameEngine);
  public
    constructor Create(RecvTunnel_, SendTunnel_: TCommunicationFrameworkClient);
    destructor Destroy; override;

    function Connected: Boolean; virtual;

    procedure SwitchAsMaxPerformance;
    procedure SwitchAsMaxSecurity;
    procedure SwitchAsDefaultPerformance;

    procedure Progress; virtual;
    procedure CadencerProgress(Sender: TObject; const deltaTime, newTime: Double); virtual;

    { sync connect }
    function Connect(addr: SystemString; const RecvPort, SendPort: Word): Boolean; virtual;

    { async connection }
    procedure AsyncConnectC(addr: SystemString; const RecvPort, SendPort: Word; OnResult: TStateCall); overload; virtual;
    procedure AsyncConnectM(addr: SystemString; const RecvPort, SendPort: Word; OnResult: TStateMethod); overload; virtual;
    procedure AsyncConnectP(addr: SystemString; const RecvPort, SendPort: Word; OnResult: TStateProc); overload; virtual;
    { parameter async connection }
    procedure AsyncConnectC(addr: SystemString; const RecvPort, SendPort: Word; Param1: Pointer; Param2: TObject; OnResult: TParamStateCall); overload;
    procedure AsyncConnectM(addr: SystemString; const RecvPort, SendPort: Word; Param1: Pointer; Param2: TObject; OnResult: TParamStateMethod); overload;
    procedure AsyncConnectP(addr: SystemString; const RecvPort, SendPort: Word; Param1: Pointer; Param2: TObject; OnResult: TParamStateProc); overload;
    procedure Disconnect; virtual;

    { sync mode }
    function UserLogin(UserID, passwd: SystemString): Boolean; virtual;
    function RegisterUser(UserID, passwd: SystemString): Boolean; virtual;
    function TunnelLink: Boolean; virtual;

    { async mode }
    procedure UserLoginC(UserID, passwd: SystemString; OnCall: TStateCall); virtual;
    procedure UserLoginM(UserID, passwd: SystemString; OnMethod: TStateMethod); virtual;
    procedure UserLoginP(UserID, passwd: SystemString; OnProc: TStateProc); virtual;
    procedure RegisterUserC(UserID, passwd: SystemString; OnCall: TStateCall); virtual;
    procedure RegisterUserM(UserID, passwd: SystemString; OnMethod: TStateMethod); virtual;
    procedure RegisterUserP(UserID, passwd: SystemString; OnProc: TStateProc); virtual;
    procedure TunnelLinkC(OnCall: TStateCall); virtual;
    procedure TunnelLinkM(OnMethod: TStateMethod); virtual;
    procedure TunnelLinkP(OnProc: TStateProc); virtual;
    procedure SyncCadencer; virtual;

    { sync mode }
    function ChnagePassword(oldPasswd, newPasswd: SystemString): Boolean;
    function CustomNewUser(UsrID, UsrPasswd: SystemString; AUserConfigFile: TSectionTextData): Boolean;

    procedure ProcessStoreQueueCMD;

    procedure GetPublicFileList(Filter: SystemString; lst: TCoreClassStrings);
    procedure GetPrivateFileList(Filter, DirectoryName: SystemString; lst: TCoreClassStrings); overload;
    procedure GetPrivateFileList(Filter: SystemString; lst: TCoreClassStrings); overload;
    procedure GetPrivateDirectoryList(Filter, DirectoryName: SystemString; lst: TCoreClassStrings); overload;
    procedure GetPrivateDirectoryList(Filter: SystemString; lst: TCoreClassStrings); overload;
    function CreatePrivateDirectory(DirectoryName: SystemString): Boolean;

    { remote file information }
    procedure GetPublicFileInfoC(fileName: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TGetFileInfoCall); overload;
    procedure GetPublicFileInfoM(fileName: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TGetFileInfoMethod); overload;
    procedure GetPublicFileInfoP(fileName: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TGetFileInfoProc); overload;
    procedure GetPrivateFileInfoC(fileName, DirectoryName: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TGetFileInfoCall); overload;
    procedure GetPrivateFileInfoM(fileName, DirectoryName: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TGetFileInfoMethod); overload;
    procedure GetPrivateFileInfoP(fileName, DirectoryName: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TGetFileInfoProc); overload;

    { remote md5 support with public store space }
    procedure GetPublicFileMD5C(fileName: SystemString; const StartPos, EndPos: Int64;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileMD5Call); overload;
    procedure GetPublicFileMD5M(fileName: SystemString; const StartPos, EndPos: Int64;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileMD5Method); overload;
    procedure GetPublicFileMD5P(fileName: SystemString; const StartPos, EndPos: Int64;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileMD5Proc); overload;

    { remote md5 support with private store space }
    procedure GetPrivateFileMD5C(fileName, DirectoryName: SystemString; const StartPos, EndPos: Int64;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileMD5Call); overload;
    procedure GetPrivateFileMD5M(fileName, DirectoryName: SystemString; const StartPos, EndPos: Int64;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileMD5Method); overload;
    procedure GetPrivateFileMD5P(fileName, DirectoryName: SystemString; const StartPos, EndPos: Int64;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileMD5Proc); overload;

    { normal download with public store space }
    function GetPublicFile(fileName, saveToPath: SystemString): Boolean; overload;
    procedure GetPublicFileC(fileName, saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteCall); overload;
    procedure GetPublicFileM(fileName, saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteMethod); overload;
    procedure GetPublicFileP(fileName, saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteProc); overload;

    { restore download with public store space }
    function GetPublicFile(fileName: SystemString; StartPos: Int64; saveToPath: SystemString): Boolean; overload;
    procedure GetPublicFileC(fileName: SystemString; StartPos: Int64; saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteCall); overload;
    procedure GetPublicFileM(fileName: SystemString; StartPos: Int64; saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteMethod); overload;
    procedure GetPublicFileP(fileName: SystemString; StartPos: Int64; saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteProc); overload;

    { normal download with private store space }
    function GetPrivateFile(fileName, DirectoryName, saveToPath: SystemString): Boolean; overload;
    function GetPrivateFile(fileName, saveToPath: SystemString): Boolean; overload;
    procedure GetPrivateFileC(fileName, DirectoryName, saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteCall); overload;
    procedure GetPrivateFileM(fileName, DirectoryName, saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteMethod); overload;
    procedure GetPrivateFileP(fileName, DirectoryName, saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteProc); overload;

    { restore download with private store space }
    function GetPrivateFile(fileName: SystemString; StartPos: Int64; DirectoryName, saveToPath: SystemString): Boolean; overload;
    function GetPrivateFile(fileName: SystemString; StartPos: Int64; saveToPath: SystemString): Boolean; overload;
    procedure GetPrivateFileC(fileName: SystemString; StartPos: Int64; DirectoryName, saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteCall); overload;
    procedure GetPrivateFileM(fileName: SystemString; StartPos: Int64; DirectoryName, saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteMethod); overload;
    procedure GetPrivateFileP(fileName: SystemString; StartPos: Int64; DirectoryName, saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteProc); overload;

    { normal download with custom user }
    function GetUserPrivateFile(UserID, fileName, DirectoryName, saveToPath: SystemString): Boolean; overload;
    function GetUserPrivateFile(UserID, fileName, saveToPath: SystemString): Boolean; overload;
    procedure GetUserPrivateFileC(UserID, fileName, DirectoryName, saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteCall); overload;
    procedure GetUserPrivateFileM(UserID, fileName, DirectoryName, saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteMethod); overload;
    procedure GetUserPrivateFileP(UserID, fileName, DirectoryName, saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteProc); overload;

    { restore download with custom user }
    function GetUserPrivateFile(UserID, fileName: SystemString; StartPos: Int64; DirectoryName, saveToPath: SystemString): Boolean; overload;
    function GetUserPrivateFile(UserID, fileName: SystemString; StartPos: Int64; saveToPath: SystemString): Boolean; overload;
    procedure GetUserPrivateFileC(UserID, fileName: SystemString; StartPos: Int64; DirectoryName, saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteCall); overload;
    procedure GetUserPrivateFileM(UserID, fileName: SystemString; StartPos: Int64; DirectoryName, saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteMethod); overload;
    procedure GetUserPrivateFileP(UserID, fileName: SystemString; StartPos: Int64; DirectoryName, saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteProc); overload;

    { public file fragment }
    procedure GetPublicFileFragmentDataC(fileName: SystemString; StartPos, EndPos: Int64;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnCompleteCall: TFileFragmentDataCall); overload;
    procedure GetPublicFileFragmentDataM(fileName: SystemString; StartPos, EndPos: Int64;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnCompleteMethod: TFileFragmentDataMethod); overload;
    procedure GetPublicFileFragmentDataP(fileName: SystemString; StartPos, EndPos: Int64;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnCompleteProc: TFileFragmentDataProc); overload;
    { Private file fragment }
    procedure GetPrivateFileFragmentDataC(fileName: SystemString; StartPos, EndPos: Int64;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnCompleteCall: TFileFragmentDataCall); overload;
    procedure GetPrivateFileFragmentDataM(fileName: SystemString; StartPos, EndPos: Int64;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnCompleteMethod: TFileFragmentDataMethod); overload;
    procedure GetPrivateFileFragmentDataP(fileName: SystemString; StartPos, EndPos: Int64;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnCompleteProc: TFileFragmentDataProc); overload;

    { upload file to public store space }
    procedure PostFileToPublic(fileName: SystemString); overload;
    { restore upload file to public store space }
    procedure PostFileToPublic(fileName: SystemString; StartPos: Int64); overload;
    { upload file to private store space }
    procedure PostFileToPrivate(fileName, DirectoryName: SystemString); overload;
    procedure PostFileToPrivate(fileName: SystemString); overload;
    { restore upload file to private store space }
    procedure PostFileToPrivate(fileName, DirectoryName: SystemString; StartPos: Int64); overload;
    procedure PostFileToPrivate(fileName: SystemString; StartPos: Int64); overload;
    { upload stream to private store space }
    procedure PostStreamToPrivate(RemoteFileName, DirectoryName: SystemString; stream: TCoreClassStream; doneFreeStream: Boolean); overload;
    { restore upload stream to private store space }
    procedure PostStreamToPrivate(RemoteFileName, DirectoryName: SystemString; stream: TCoreClassStream; StartPos: Int64; doneFreeStream: Boolean); overload;

    { batch stream suppport }
    procedure PostBatchStream(stream: TCoreClassStream; doneFreeStream: Boolean); overload;
    procedure PostBatchStreamC(stream: TCoreClassStream; doneFreeStream: Boolean; OnCompletedBackcall: TStateCall); overload;
    procedure PostBatchStreamM(stream: TCoreClassStream; doneFreeStream: Boolean; OnCompletedBackcall: TStateMethod); overload;
    procedure PostBatchStreamP(stream: TCoreClassStream; doneFreeStream: Boolean; OnCompletedBackcall: TStateProc); overload;
    procedure ClearBatchStream;
    procedure GetBatchStreamStateM(OnResult: TStreamMethod); overload;
    procedure GetBatchStreamStateP(OnResult: TStreamProc); overload;
    function GetBatchStreamState(ResultData: TDataFrameEngine; ATimeOut: TTimeTick): Boolean; overload;

    procedure RegisterCommand; virtual;
    procedure UnRegisterCommand; virtual;

    property LinkOk: Boolean read FLinkOk;
    property BindOk: Boolean read FLinkOk;
    property WaitCommandTimeout: Cardinal read FWaitCommandTimeout write FWaitCommandTimeout;
    property RecvFileing: Boolean read FRecvFileing;
    property RecvFileOfBatching: Boolean read FRecvFileOfBatching;
    property RecvFileName: SystemString read FRecvFileName;

    property CadencerEngine: TCadencer read FCadencerEngine;
    property ServerDelay: Double read FServerDelay;

    property ProgressEngine: TNProgressPost read FProgressEngine;
    property ProgressPost: TNProgressPost read FProgressEngine;
    property PostProgress: TNProgressPost read FProgressEngine;
    property PostRun: TNProgressPost read FProgressEngine;
    property PostExecute: TNProgressPost read FProgressEngine;

    property RecvTunnel: TCommunicationFrameworkClient read FRecvTunnel;
    property SendTunnel: TCommunicationFrameworkClient read FSendTunnel;

    function RemoteInited: Boolean;
  end;

  PGetFileInfoStruct = ^TGetFileInfoStruct;

  TGetFileInfoStruct = record
    UserData: Pointer;
    UserObject: TCoreClassObject;
    fileName: SystemString;
    OnCompleteCall: TGetFileInfoCall;
    OnCompleteMethod: TGetFileInfoMethod;
    OnCompleteProc: TGetFileInfoProc;
    procedure Init;
  end;

  PFileMD5Struct = ^TFileMD5Struct;

  TFileMD5Struct = record
    UserData: Pointer;
    UserObject: TCoreClassObject;
    fileName: SystemString;
    StartPos, EndPos: Int64;
    OnCompleteCall: TFileMD5Call;
    OnCompleteMethod: TFileMD5Method;
    OnCompleteProc: TFileMD5Proc;
    procedure Init;
  end;

  PRemoteFileBackcall = ^TRemoteFileBackcall;

  TRemoteFileBackcall = record
    UserData: Pointer;
    UserObject: TCoreClassObject;
    OnCompleteCall: TFileCompleteCall;
    OnCompleteMethod: TFileCompleteMethod;
    OnCompleteProc: TFileCompleteProc;
    procedure Init;
  end;

  PFileFragmentDataBackcall = ^TFileFragmentDataBackcall;

  TFileFragmentDataBackcall = record
    UserData: Pointer;
    UserObject: TCoreClassObject;
    fileName: SystemString;
    StartPos, EndPos: Int64;
    OnCompleteCall: TFileFragmentDataCall;
    OnCompleteMethod: TFileFragmentDataMethod;
    OnCompleteProc: TFileFragmentDataProc;
  end;

implementation

uses SysUtils;

procedure TGetFileInfoStruct.Init;
begin
  UserData := nil;
  UserObject := nil;
  fileName := '';
  OnCompleteCall := nil;
  OnCompleteMethod := nil;
  OnCompleteProc := nil;
end;

procedure TFileMD5Struct.Init;
begin
  UserData := nil;
  UserObject := nil;
  fileName := '';
  StartPos := 0;
  EndPos := 0;
  OnCompleteCall := nil;
  OnCompleteMethod := nil;
  OnCompleteProc := nil;
end;

procedure TRemoteFileBackcall.Init;
begin
  UserData := nil;
  UserObject := nil;
  OnCompleteCall := nil;
  OnCompleteMethod := nil;
  OnCompleteProc := nil;
end;

constructor TPeerClientUserDefineForSendTunnel.Create(Owner_: TPeerIO);
begin
  inherited Create(Owner_);
  RecvTunnel := nil;
  RecvTunnelID := 0;
  DoubleTunnelService := nil;
end;

destructor TPeerClientUserDefineForSendTunnel.Destroy;
begin
  if (DoubleTunnelService <> nil) and (RecvTunnelID > 0) and (RecvTunnel <> nil) then
    begin
      if DoubleTunnelService.FRecvTunnel.Exists(RecvTunnelID) then
          DoubleTunnelService.FRecvTunnel.PeerIO[RecvTunnelID].Disconnect;
    end;
  inherited Destroy;
end;

function TPeerClientUserDefineForSendTunnel.LinkOk: Boolean;
begin
  Result := DoubleTunnelService <> nil;
end;

constructor TPeerClientUserDefineForRecvTunnel.Create(Owner_: TPeerIO);
begin
  inherited Create(Owner_);
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
              DoubleTunnelService.FSendTunnel.PeerIO[SendTunnelID].Disconnect;
        end;

      try
          DoubleTunnelService.FLoginUserList.Delete(UserID);
      except
      end;

      try
          DoubleTunnelService.FLoginUserDefineIOList.Delete(UserID);
      except
      end;
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

function TPeerClientUserDefineForRecvTunnel.MakeFilePath(fn: SystemString): SystemString;
begin
  Result := umlCombineFileName(UserPath, fn);
end;

function TPeerClientUserDefineForRecvTunnel.GetUserID: SystemString;
begin
  Result := UserConfigFile.GetDefaultValue('UserInfo', 'UserID', '');
end;

procedure TPeerClientUserDefineForRecvTunnel.SaveConfigFile;
begin
  UserConfigFile.SaveToFile(MakeFilePath('User.Config'));
end;

function TPeerClientUserDefineForRecvTunnel.LinkOk: Boolean;
begin
  Result := SendTunnel <> nil;
end;

procedure TCommunicationFramework_DoubleTunnelService.UserRegistedSuccess(UserID: SystemString);
begin
end;

procedure TCommunicationFramework_DoubleTunnelService.UserLoginSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel);
begin
end;

procedure TCommunicationFramework_DoubleTunnelService.UserLinkSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel);
begin
  if Assigned(FOnLinkSuccess) then
      FOnLinkSuccess(Self, UserDefineIO);
end;

procedure TCommunicationFramework_DoubleTunnelService.UserCreateDirectorySuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel; dn: SystemString);
begin
end;

procedure TCommunicationFramework_DoubleTunnelService.UserPostFileSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel; fn: SystemString);
begin
end;

procedure TCommunicationFramework_DoubleTunnelService.UserOut(UserDefineIO: TPeerClientUserDefineForRecvTunnel);
begin
  if Assigned(FOnUserOut) then
      FOnUserOut(Self, UserDefineIO);
end;

procedure TCommunicationFramework_DoubleTunnelService.Command_UserLogin(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
var
  SendTunnelID: Cardinal;
  UserID, UserPasswd: SystemString;
  UserDefineIO: TPeerClientUserDefineForRecvTunnel;
begin
  SendTunnelID := InData.Reader.ReadCardinal;
  UserID := InData.Reader.ReadString;
  UserPasswd := InData.Reader.ReadString;

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

  try
    if not CompareQuantumCryptographyPassword(UserPasswd, SystemString(FUserDB.GetDefaultValue(UserID, 'password', ''))) then
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

  if FLoginUserDefineIOList.Exists(UserID) then
      TPeerClientUserDefineForRecvTunnel(FLoginUserDefineIOList[UserID]).Owner.Disconnect;

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
  UserDefineIO.UserConfigFile.Hit['UserInfo', 'Password'] := SystemString(FUserDB.GetDefaultValue(UserID, 'password', ''));

  FLoginUserList[UserID] := Now;

  FLoginUserDefineIOList[UserID] := UserDefineIO;

  UserDefineIO.WaitLink := True;
  UserDefineIO.WaitLinkSendID := SendTunnelID;

  OutData.WriteBool(True);
  OutData.WriteString(Format('success Login:%s', [UserID]));
  OutData.WriteString(UserDefineIO.UserFlag);

  UserLoginSuccess(UserDefineIO);
end;

procedure TCommunicationFramework_DoubleTunnelService.Command_RegisterUser(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
var
  SendTunnelID: Cardinal;
  UserID, UserPasswd: SystemString;
  UserDefineIO: TPeerClientUserDefineForRecvTunnel;
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

  if umlExistsChar(UserID, '[]:'#13#10#9#8#0) then
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

  if FLoginUserList.Exists(UserID) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(Format('user already online:%s', [UserID]));
      Exit;
    end;

  UserDefineIO.UserFlag := MakeUserFlag;
  UserDefineIO.UserID := UserID;
  UserDefineIO.UserPath := umlCombinePath(FRootPath, UserDefineIO.UserFlag);
  umlCreateDirectory(UserDefineIO.UserPath);
  UserDefineIO.UserDBIntf := FUserDB.VariantList[UserID];
  UserDefineIO.UserDBIntf['UserFlag'] := UserDefineIO.UserFlag;
  UserDefineIO.UserDBIntf['password'] := GenerateQuantumCryptographyPassword(UserPasswd).Text;
  UserDefineIO.UserDBIntf['RegTime'] := DateTimeToStr(Now);
  UserDefineIO.DoubleTunnelService := Self;
  UserDefineIO.LoginSuccessed := True;

  UserDefineIO.UserConfigFile.Hit['UserInfo', 'UserID'] := UserID;
  UserDefineIO.UserConfigFile.Hit['UserInfo', 'Password'] := UserDefineIO.UserDBIntf['password'];
  UserDefineIO.SaveConfigFile;

  if FCanSaveUserInfo then
      SaveUserDB;

  FLoginUserList[UserID] := Now;

  FLoginUserDefineIOList[UserID] := UserDefineIO;

  UserDefineIO.WaitLink := True;
  UserDefineIO.WaitLinkSendID := SendTunnelID;

  OutData.WriteBool(True);
  OutData.WriteString(Format('success registed:%s', [UserID]));
  OutData.WriteString(UserDefineIO.UserFlag);

  UserDefineIO.SaveConfigFile;
  UserRegistedSuccess(UserID);
end;

procedure TCommunicationFramework_DoubleTunnelService.Command_TunnelLink(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
var
  RecvID, SendID: Cardinal;
  UserDefineIO: TPeerClientUserDefineForRecvTunnel;
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

  UserDefineIO.SendTunnel := FSendTunnel.PeerIO[SendID].UserDefine as TPeerClientUserDefineForSendTunnel;
  UserDefineIO.SendTunnelID := SendID;
  UserDefineIO.SendTunnel.RecvTunnel := UserDefineIO;
  UserDefineIO.SendTunnel.RecvTunnelID := RecvID;
  UserDefineIO.SendTunnel.DoubleTunnelService := Self;

  OutData.WriteBool(True);
  OutData.WriteString(Format('tunnel link success! recv:%d <-> send:%d', [RecvID, SendID]));

  UserLinkSuccess(UserDefineIO);
end;

procedure TCommunicationFramework_DoubleTunnelService.Command_ChangePasswd(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
var
  UserDefineIO: TPeerClientUserDefineForRecvTunnel;
  oldPasswd, newPasswd: SystemString;
begin
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LoginSuccessed then
      Exit;
  if UserDefineIO.SendTunnel = nil then
      Exit;

  oldPasswd := InData.Reader.ReadString;
  newPasswd := InData.Reader.ReadString;

  try
    if not CompareQuantumCryptographyPassword(oldPasswd, SystemString(FUserDB.GetDefaultValue(UserDefineIO.UserID, 'password', ''))) then
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

  UserDefineIO.UserDBIntf['password'] := GenerateQuantumCryptographyPassword(newPasswd).Text;
  if FCanSaveUserInfo then
      SaveUserDB;

  OutData.WriteBool(True);
  OutData.WriteString(Format('password change success', []));
end;

procedure TCommunicationFramework_DoubleTunnelService.Command_CustomNewUser(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
var
  UserDefineIO: TPeerClientUserDefineForRecvTunnel;
  UserID, passwd: SystemString;
  UserConfig: TSectionTextData;
begin
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LoginSuccessed then
      Exit;
  if UserDefineIO.SendTunnel = nil then
      Exit;

  UserID := InData.Reader.ReadString;
  passwd := InData.Reader.ReadString;
  UserConfig := TSectionTextData.Create;
  InData.Reader.ReadSectionText(UserConfig);

  OutData.WriteBool(RegUser(UserID, passwd, UserConfig));

  DisposeObject(UserConfig);
end;

procedure TCommunicationFramework_DoubleTunnelService.Command_ProcessStoreQueueCMD(Sender: TPeerIO; InData: TDataFrameEngine);
var
  UserDefineIO: TPeerClientUserDefineForRecvTunnel;
  fs: U_StringArray;
  fn: SystemString;
  Cmd: SystemString;
  de, de2: TDataFrameEngine;
  stream: TCoreClassFileStream;
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
            stream := TCoreClassFileStream.Create(fn, fmOpenRead or fmShareDenyNone);
            de.DecodeFrom(stream);
            DisposeObject(stream);
            Cmd := de.Reader.ReadString;
            de2 := TDataFrameEngine.Create;
            de.Reader.ReadDataFrame(de2);
            FSendTunnel.SendDirectStreamCmd(UserDefineIO.SendTunnel.Owner, Cmd, de2);
            DisposeObject(de2);
            DisposeObject(de);
          except
          end;
          umlDeleteFile(fn);
        end;
    end;
end;

procedure TCommunicationFramework_DoubleTunnelService.Command_GetPublicFileList(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
var
  UserDefineIO: TPeerClientUserDefineForRecvTunnel;
  Filter: SystemString;
  fs: U_StringArray;
  i: Integer;
  n: SystemString;
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

procedure TCommunicationFramework_DoubleTunnelService.Command_GetPrivateFileList(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
var
  UserDefineIO: TPeerClientUserDefineForRecvTunnel;
  Filter, dn: SystemString;
  fs: U_StringArray;
  i: Integer;
  n: SystemString;
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

procedure TCommunicationFramework_DoubleTunnelService.Command_GetPrivateDirectoryList(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
var
  UserDefineIO: TPeerClientUserDefineForRecvTunnel;
  Filter, dn: SystemString;
  fs: U_StringArray;
  i: Integer;
  n: SystemString;
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

procedure TCommunicationFramework_DoubleTunnelService.Command_CreatePrivateDirectory(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
var
  UserDefineIO: TPeerClientUserDefineForRecvTunnel;
  dn, fulldn: SystemString;
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

procedure TCommunicationFramework_DoubleTunnelService.Command_GetPublicFileInfo(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
var
  UserDefineIO: TPeerClientUserDefineForRecvTunnel;
  fullfn, fileName: SystemString;
begin
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LoginSuccessed then
      Exit;
  if UserDefineIO.SendTunnel = nil then
      Exit;
  fileName := InData.Reader.ReadString;

  fullfn := umlCombineFileName(FPublicPath, fileName);
  if umlFileExists(fullfn) then
    begin
      OutData.WriteBool(True);
      OutData.WriteInt64(umlGetFileSize(fullfn));
    end
  else
    begin
      OutData.WriteBool(False);
      OutData.WriteInt64(0);
    end;
end;

procedure TCommunicationFramework_DoubleTunnelService.Command_GetPrivateFileInfo(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
var
  UserDefineIO: TPeerClientUserDefineForRecvTunnel;
  fullfn, fileName, dn: SystemString;
begin
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LoginSuccessed then
      Exit;
  if UserDefineIO.SendTunnel = nil then
      Exit;
  fileName := InData.Reader.ReadString;
  dn := InData.Reader.ReadString;

  fullfn := umlCombineFileName(umlCombinePath(UserDefineIO.UserPath, dn), fileName);
  if umlFileExists(fullfn) then
    begin
      OutData.WriteBool(True);
      OutData.WriteInt64(umlGetFileSize(fullfn));
    end
  else
    begin
      OutData.WriteBool(False);
      OutData.WriteInt64(0);
    end;
end;

procedure TCommunicationFramework_DoubleTunnelService.Command_GetPublicFileMD5(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
var
  UserDefineIO: TPeerClientUserDefineForRecvTunnel;
  fullfn, fileName: SystemString;
  StartPos, EndPos: Int64;
  fs: TCoreClassFileStream;
  MD5: TMD5;
begin
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LoginSuccessed then
      Exit;
  if UserDefineIO.SendTunnel = nil then
      Exit;
  fileName := InData.Reader.ReadString;
  StartPos := InData.Reader.ReadInt64;
  EndPos := InData.Reader.ReadInt64;

  fullfn := umlCombineFileName(FPublicPath, fileName);
  if not umlFileExists(fullfn) then
    begin
      OutData.WriteBool(False);
      Exit;
    end;

  try
      fs := TCoreClassFileStream.Create(fullfn, fmOpenRead or fmShareDenyNone);
  except
    OutData.WriteBool(False);
    DisposeObject(fs);
    Exit;
  end;

  if (EndPos > fs.Size) then
      EndPos := fs.Size;

  if (EndPos = StartPos) or (EndPos = 0) then
      MD5 := umlStreamMD5(fs)
  else
      MD5 := umlStreamMD5(fs, StartPos, EndPos);

  OutData.WriteBool(True);
  OutData.WriteMD5(MD5);
  DisposeObject(fs);
end;

procedure TCommunicationFramework_DoubleTunnelService.Command_GetPrivateFileMD5(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
var
  UserDefineIO: TPeerClientUserDefineForRecvTunnel;
  fullfn, fileName, dn: SystemString;
  StartPos, EndPos: Int64;
  fs: TCoreClassFileStream;
  MD5: TMD5;
begin
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LoginSuccessed then
      Exit;
  if UserDefineIO.SendTunnel = nil then
      Exit;
  fileName := InData.Reader.ReadString;
  dn := InData.Reader.ReadString;
  StartPos := InData.Reader.ReadInt64;
  EndPos := InData.Reader.ReadInt64;

  fullfn := umlCombineFileName(umlCombinePath(UserDefineIO.UserPath, dn), fileName);
  if not umlFileExists(fullfn) then
    begin
      OutData.WriteBool(False);
      Exit;
    end;

  try
      fs := TCoreClassFileStream.Create(fullfn, fmOpenRead or fmShareDenyNone);
  except
    OutData.WriteBool(False);
    DisposeObject(fs);
    Exit;
  end;

  if (EndPos > fs.Size) then
      EndPos := fs.Size;

  if (EndPos = StartPos) or (EndPos = 0) then
      MD5 := umlStreamMD5(fs)
  else
      MD5 := umlStreamMD5(fs, StartPos, EndPos);

  OutData.WriteBool(True);
  OutData.WriteMD5(MD5);
  DisposeObject(fs);
end;

procedure TCommunicationFramework_DoubleTunnelService.Command_GetPublicFile(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
var
  UserDefineIO: TPeerClientUserDefineForRecvTunnel;
  fullfn, fileName, remoteinfo: SystemString;
  StartPos: Int64;
  RemoteBackcallAddr: UInt64;
  sendDE: TDataFrameEngine;
  fs: TCoreClassFileStream;
  MD5: TMD5;
begin
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LoginSuccessed then
      Exit;
  if UserDefineIO.SendTunnel = nil then
      Exit;

  fileName := InData.Reader.ReadString;
  StartPos := InData.Reader.ReadInt64;
  remoteinfo := InData.Reader.ReadString;
  RemoteBackcallAddr := InData.Reader.ReadPointer;

  fullfn := umlCombineFileName(FPublicPath, fileName);
  if not umlFileExists(fullfn) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(Format('filename invailed %s', [fileName]));
      Exit;
    end;

  try
      fs := TCoreClassFileStream.Create(fullfn, fmOpenRead or fmShareDenyNone);
  except
      Exit;
  end;

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteString(fileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteInt64(fs.Size);
  sendDE.WriteString(remoteinfo);
  UserDefineIO.SendTunnel.Owner.SendDirectStreamCmd(C_FileInfo, sendDE);
  DisposeObject(sendDE);

  MD5 := umlStreamMD5(fs);

  fs.Position := 0;
  UserDefineIO.SendTunnel.Owner.SendBigStream(C_PostFile, fs, StartPos, True);

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteMD5(MD5);
  sendDE.WritePointer(RemoteBackcallAddr);
  UserDefineIO.SendTunnel.Owner.SendDirectStreamCmd(C_PostFileOver, sendDE);
  DisposeObject(sendDE);

  OutData.WriteBool(True);
  OutData.WriteString(Format('post %s to send tunnel', [fileName]));
end;

procedure TCommunicationFramework_DoubleTunnelService.Command_GetPrivateFile(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
var
  UserDefineIO: TPeerClientUserDefineForRecvTunnel;
  fullfn, fileName, dn, remoteinfo: SystemString;
  StartPos: Int64;
  RemoteBackcallAddr: UInt64;
  sendDE: TDataFrameEngine;
  fs: TCoreClassFileStream;
  MD5: TMD5;
begin
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LoginSuccessed then
      Exit;
  if UserDefineIO.SendTunnel = nil then
      Exit;

  fileName := InData.Reader.ReadString;
  dn := InData.Reader.ReadString;
  StartPos := InData.Reader.ReadInt64;
  remoteinfo := InData.Reader.ReadString;
  RemoteBackcallAddr := InData.Reader.ReadPointer;

  fullfn := umlCombineFileName(umlCombinePath(UserDefineIO.UserPath, dn), fileName);
  if not umlFileExists(fullfn) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(Format('filename invailed %s', [fileName]));
      Exit;
    end;

  try
      fs := TCoreClassFileStream.Create(fullfn, fmOpenRead or fmShareDenyNone);
  except
      Exit;
  end;

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteString(fileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteInt64(fs.Size);
  sendDE.WriteString(remoteinfo);
  UserDefineIO.SendTunnel.Owner.SendDirectStreamCmd(C_FileInfo, sendDE);
  DisposeObject(sendDE);

  MD5 := umlStreamMD5(fs);

  fs.Position := 0;
  UserDefineIO.SendTunnel.Owner.SendBigStream(C_PostFile, fs, StartPos, True);

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteMD5(MD5);
  sendDE.WritePointer(RemoteBackcallAddr);
  UserDefineIO.SendTunnel.Owner.SendDirectStreamCmd(C_PostFileOver, sendDE);
  DisposeObject(sendDE);

  OutData.WriteBool(True);
  OutData.WriteString(Format('post %s to send tunnel', [fileName]));
end;

procedure TCommunicationFramework_DoubleTunnelService.Command_GetUserPrivateFile(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
var
  UserDefineIO: TPeerClientUserDefineForRecvTunnel;
  fullfn, UserID, fileName, dn, remoteinfo: SystemString;
  StartPos: Int64;
  RemoteBackcallAddr: UInt64;
  sendDE: TDataFrameEngine;
  fs: TCoreClassFileStream;
  MD5: TMD5;
begin
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LoginSuccessed then
      Exit;
  if UserDefineIO.SendTunnel = nil then
      Exit;

  UserID := InData.Reader.ReadString;
  fileName := InData.Reader.ReadString;
  dn := InData.Reader.ReadString;
  StartPos := InData.Reader.ReadInt64;
  remoteinfo := InData.Reader.ReadString;
  RemoteBackcallAddr := InData.Reader.ReadPointer;

  if not ExistsUser(UserID) then
      Exit;

  fullfn := umlCombineFileName(umlCombinePath(GetUserPath(UserID), dn), fileName);
  if not umlFileExists(fullfn) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(Format('filename invailed %s', [fileName]));
      Exit;
    end;

  try
      fs := TCoreClassFileStream.Create(fullfn, fmOpenRead or fmShareDenyNone);
  except
      Exit;
  end;

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteString(fileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteInt64(fs.Size);
  sendDE.WriteString(remoteinfo);
  UserDefineIO.SendTunnel.Owner.SendDirectStreamCmd(C_FileInfo, sendDE);
  DisposeObject(sendDE);

  MD5 := umlStreamMD5(fs);

  fs.Position := 0;
  UserDefineIO.SendTunnel.Owner.SendBigStream(C_PostFile, fs, StartPos, True);

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteMD5(MD5);
  sendDE.WritePointer(RemoteBackcallAddr);
  UserDefineIO.SendTunnel.Owner.SendDirectStreamCmd(C_PostFileOver, sendDE);
  DisposeObject(sendDE);

  OutData.WriteBool(True);
  OutData.WriteString(Format('post %s to send tunnel', [fileName]));

end;

procedure TCommunicationFramework_DoubleTunnelService.Command_PostPublicFileInfo(Sender: TPeerIO; InData: TDataFrameEngine);
var
  UserDefineIO: TPeerClientUserDefineForRecvTunnel;
  fn: SystemString;
  StartPos: Int64;
  FSize: Int64;
  fullfn: SystemString;
begin
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LoginSuccessed then
    begin
      Sender.DelayClose();
      Exit;
    end;
  if UserDefineIO.SendTunnel = nil then
    begin
      Sender.DelayClose();
      Exit;
    end;

  if UserDefineIO.FCurrentFileStream <> nil then
    begin
      DisposeObject(UserDefineIO.FCurrentFileStream);
      UserDefineIO.FCurrentFileStream := nil;
    end;

  fn := InData.Reader.ReadString;
  StartPos := InData.Reader.ReadInt64;
  FSize := InData.Reader.ReadInt64;

  fullfn := umlCombineFileName(FPublicPath, fn);
  UserDefineIO.FCurrentReceiveFileName := fullfn;
  try
    if (StartPos > 0) and (umlFileExists(fullfn)) then
      begin
        UserDefineIO.FCurrentFileStream := TCoreClassFileStream.Create(fullfn, fmOpenReadWrite);
        if StartPos <= UserDefineIO.FCurrentFileStream.Size then
            UserDefineIO.FCurrentFileStream.Position := StartPos
        else
            UserDefineIO.FCurrentFileStream.Position := UserDefineIO.FCurrentFileStream.Size;
        Sender.Print(Format('preprocess user:%s restore post to public: %s', [UserDefineIO.UserID, fullfn]));
      end
    else
      begin
        UserDefineIO.FCurrentFileStream := TCoreClassFileStream.Create(fullfn, fmCreate);
        Sender.Print(Format('preprocess user:%s normal post to public: %s', [UserDefineIO.UserID, fullfn]));
      end;
  except
    Sender.Print(Format('public file failed! user:%s post to public: %s', [UserDefineIO.UserID, fullfn]));
    UserDefineIO.FCurrentFileStream := nil;
  end;
end;

procedure TCommunicationFramework_DoubleTunnelService.Command_PostPrivateFileInfo(Sender: TPeerIO; InData: TDataFrameEngine);
var
  UserDefineIO: TPeerClientUserDefineForRecvTunnel;
  fn, dn: SystemString;
  StartPos: Int64;
  FSize: Int64;
  fullfn: SystemString;
begin
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LoginSuccessed then
    begin
      Sender.DelayClose();
      Exit;
    end;
  if UserDefineIO.SendTunnel = nil then
    begin
      Sender.DelayClose();
      Exit;
    end;

  if UserDefineIO.FCurrentFileStream <> nil then
    begin
      DisposeObject(UserDefineIO.FCurrentFileStream);
      UserDefineIO.FCurrentFileStream := nil;
    end;

  fn := InData.Reader.ReadString;
  dn := InData.Reader.ReadString;
  StartPos := InData.Reader.ReadInt64;
  FSize := InData.Reader.ReadInt64;

  if not umlDirectoryExists(umlCombinePath(UserDefineIO.UserPath, dn)) then
    begin
      umlCreateDirectory(umlCombinePath(UserDefineIO.UserPath, dn));
      Exit;
    end;

  fullfn := umlCombineFileName(umlCombinePath(UserDefineIO.UserPath, dn), fn);
  UserDefineIO.FCurrentReceiveFileName := fullfn;

  try
    if (StartPos > 0) and (umlFileExists(fullfn)) then
      begin
        UserDefineIO.FCurrentFileStream := TCoreClassFileStream.Create(fullfn, fmOpenReadWrite);
        if StartPos <= UserDefineIO.FCurrentFileStream.Size then
            UserDefineIO.FCurrentFileStream.Position := StartPos
        else
            UserDefineIO.FCurrentFileStream.Position := UserDefineIO.FCurrentFileStream.Size;
        Sender.Print(Format('preprocess user:%s restore post to private: %s', [UserDefineIO.UserID, fullfn]));
      end
    else
      begin
        UserDefineIO.FCurrentFileStream := TCoreClassFileStream.Create(fullfn, fmCreate);
        Sender.Print(Format('preprocess user:%s normal post to private: %s', [UserDefineIO.UserID, fullfn]));
      end;
  except
    Sender.Print(Format('create private file failed! user:%s post to private: %s', [UserDefineIO.UserID, fullfn]));
    UserDefineIO.FCurrentFileStream := nil;
  end;
end;

procedure TCommunicationFramework_DoubleTunnelService.Command_PostFile(Sender: TPeerIO; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64);
var
  UserDefineIO: TPeerClientUserDefineForRecvTunnel;
begin
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LoginSuccessed then
    begin
      Sender.DelayClose();
      Exit;
    end;
  if UserDefineIO.SendTunnel = nil then
    begin
      Sender.DelayClose();
      Exit;
    end;

  if UserDefineIO.FCurrentFileStream <> nil then
    begin
      InData.Position := 0;
      if InData.Size > 0 then
          UserDefineIO.FCurrentFileStream.CopyFrom(InData, InData.Size);
    end;
end;

procedure TCommunicationFramework_DoubleTunnelService.Command_PostFileOver(Sender: TPeerIO; InData: TDataFrameEngine);
var
  UserDefineIO: TPeerClientUserDefineForRecvTunnel;
  ClientMD5, MD5: TMD5;
  fn: SystemString;
begin
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LoginSuccessed then
    begin
      Sender.DelayClose();
      Exit;
    end;
  if UserDefineIO.SendTunnel = nil then
    begin
      Sender.DelayClose();
      Exit;
    end;

  ClientMD5 := InData.Reader.ReadMD5;

  if UserDefineIO.FCurrentFileStream <> nil then
    begin
      MD5 := umlStreamMD5(UserDefineIO.FCurrentFileStream);
      fn := UserDefineIO.FCurrentReceiveFileName;
      DisposeObject(UserDefineIO.FCurrentFileStream);
      UserDefineIO.FCurrentFileStream := nil;

      if umlMD5Compare(MD5, ClientMD5) then
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

procedure TCommunicationFramework_DoubleTunnelService.Command_GetPublicFileFragmentData(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
var
  UserDefineIO: TPeerClientUserDefineForRecvTunnel;
  fullfn, fileName: SystemString;
  StartPos, EndPos, siz, fp: Int64;
  RemoteBackcallAddr: UInt64;
  fs: TCoreClassFileStream;
  mem_: TMemoryStream64;
  MD5: TMD5;
begin
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LinkOk then
      Exit;

  fileName := InData.Reader.ReadString;
  StartPos := InData.Reader.ReadInt64;
  EndPos := InData.Reader.ReadInt64;
  RemoteBackcallAddr := InData.Reader.ReadPointer;

  fullfn := umlCombineFileName(FPublicPath, fileName);
  if not umlFileExists(fullfn) then
    begin
      OutData.WriteBool(False);
      Exit;
    end;

  try
      fs := TCoreClassFileStream.Create(fullfn, fmOpenRead or fmShareDenyNone);
  except
    OutData.WriteBool(False);
    DisposeObject(fs);
    Exit;
  end;

  if EndPos < StartPos then
      Swap(EndPos, StartPos);

  if (EndPos > fs.Size) then
      EndPos := fs.Size;

  siz := EndPos - StartPos;
  if siz <= 0 then
    begin
      OutData.WriteBool(False);
      DisposeObject(fs);
      Exit;
    end;

  fs.Position := StartPos;
  mem_ := TMemoryStream64.Create;
  mem_.WriteUInt64(RemoteBackcallAddr);
  mem_.WriteInt64(StartPos);
  mem_.WriteInt64(EndPos);
  mem_.WriteInt64(siz);
  fp := mem_.Position;
  mem_.CopyFrom(fs, siz);
  MD5 := umlStreamMD5(mem_, fp, mem_.Size);
  mem_.WriteMD5(MD5);

  DisposeObject(fs);
  UserDefineIO.SendTunnel.Owner.SendCompleteBuffer(C_PostFileFragmentData, mem_.Memory, mem_.Size, True);
  mem_.DiscardMemory;
  DisposeObject(mem_);

  OutData.WriteBool(True);
end;

procedure TCommunicationFramework_DoubleTunnelService.Command_GetPrivateFileFragmentData(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
var
  UserDefineIO: TPeerClientUserDefineForRecvTunnel;
  fullfn, fileName: SystemString;
  StartPos, EndPos, siz, fp: Int64;
  RemoteBackcallAddr: UInt64;
  fs: TCoreClassFileStream;
  mem_: TMemoryStream64;
  MD5: TMD5;
begin
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LinkOk then
      Exit;

  fileName := InData.Reader.ReadString;
  StartPos := InData.Reader.ReadInt64;
  EndPos := InData.Reader.ReadInt64;
  RemoteBackcallAddr := InData.Reader.ReadPointer;

  fullfn := umlCombineFileName(UserDefineIO.UserPath, fileName);
  if not umlFileExists(fullfn) then
    begin
      OutData.WriteBool(False);
      Exit;
    end;

  try
      fs := TCoreClassFileStream.Create(fullfn, fmOpenRead or fmShareDenyNone);
  except
    OutData.WriteBool(False);
    DisposeObject(fs);
    Exit;
  end;

  if EndPos < StartPos then
      Swap(EndPos, StartPos);

  if (EndPos > fs.Size) then
      EndPos := fs.Size;

  siz := EndPos - StartPos;
  if siz <= 0 then
    begin
      OutData.WriteBool(False);
      DisposeObject(fs);
      Exit;
    end;

  fs.Position := StartPos;
  mem_ := TMemoryStream64.Create;
  mem_.WriteUInt64(RemoteBackcallAddr);
  mem_.WriteInt64(StartPos);
  mem_.WriteInt64(EndPos);
  mem_.WriteInt64(siz);
  fp := mem_.Position;
  mem_.CopyFrom(fs, siz);
  MD5 := umlStreamMD5(mem_, fp, mem_.Size);
  mem_.WriteMD5(MD5);

  DisposeObject(fs);
  UserDefineIO.SendTunnel.Owner.SendCompleteBuffer(C_PostFileFragmentData, mem_.Memory, mem_.Size, True);
  mem_.DiscardMemory;
  DisposeObject(mem_);

  OutData.WriteBool(True);
end;

procedure TCommunicationFramework_DoubleTunnelService.Command_GetCurrentCadencer(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
begin
  FCadencerEngine.Progress;
  OutData.WriteDouble(FCadencerEngine.CurrentTime);
end;

procedure TCommunicationFramework_DoubleTunnelService.Command_NewBatchStream(Sender: TPeerIO; InData: TDataFrameEngine);
var
  RT: TPeerClientUserDefineForRecvTunnel;
  p: PBigStreamBatchPostData;
begin
  RT := GetUserDefineRecvTunnel(Sender);
  if not RT.LinkOk then
      Exit;
  p := RT.BigStreamBatchList.NewPostData;
  p^.RemoteMD5 := InData.Reader.ReadMD5;
  p^.CompletedBackcallPtr := InData.Reader.ReadPointer;
end;

procedure TCommunicationFramework_DoubleTunnelService.Command_PostBatchStream(Sender: TPeerIO; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64);
var
  RT: TPeerClientUserDefineForRecvTunnel;
  p: PBigStreamBatchPostData;
  de: TDataFrameEngine;
begin
  RT := GetUserDefineRecvTunnel(Sender);
  if not RT.LinkOk then
      Exit;

  if Sender.UserDefine.BigStreamBatchList.Count > 0 then
    begin
      p := RT.BigStreamBatchList.Last;
      p^.Source.Position := p^.Source.Size;
      p^.Source.CopyFrom(InData, InData.Size);
      if (p^.Source.Size >= BigStreamTotal) then
        begin
          p^.Source.Position := 0;
          p^.SourceMD5 := umlStreamMD5(p^.Source);

          if p^.CompletedBackcallPtr <> 0 then
            begin
              de := TDataFrameEngine.Create;
              de.WriteMD5(p^.RemoteMD5);
              de.WriteMD5(p^.SourceMD5);
              de.WritePointer(p^.CompletedBackcallPtr);
              RT.SendTunnel.Owner.SendDirectStreamCmd(C_PostBatchStreamDone, de);
              DisposeObject(de);
            end;
        end;
    end;
end;

procedure TCommunicationFramework_DoubleTunnelService.Command_ClearBatchStream(Sender: TPeerIO; InData: TDataFrameEngine);
var
  RT: TPeerClientUserDefineForRecvTunnel;
begin
  RT := GetUserDefineRecvTunnel(Sender);
  if not RT.LinkOk then
      Exit;
  RT.BigStreamBatchList.Clear;
end;

procedure TCommunicationFramework_DoubleTunnelService.Command_PostBatchStreamDone(Sender: TPeerIO; InData: TDataFrameEngine);
var
  RT: TPeerClientUserDefineForRecvTunnel;
  rMD5, sMD5: TMD5;
  backCallVal: UInt64;
  backCallValPtr: POnStateStruct;
  MD5Verify: Boolean;
begin
  RT := GetUserDefineRecvTunnel(Sender);
  if not RT.LinkOk then
      Exit;

  rMD5 := InData.Reader.ReadMD5;
  sMD5 := InData.Reader.ReadMD5;
  backCallVal := InData.Reader.ReadPointer;

  backCallValPtr := POnStateStruct(Pointer(backCallVal));
  MD5Verify := umlMD5Compare(rMD5, sMD5);

  if backCallValPtr = nil then
      Exit;

  try
    if Assigned(backCallValPtr^.OnCall) then
        backCallValPtr^.OnCall(MD5Verify);
  except
  end;

  try
    if Assigned(backCallValPtr^.OnMethod) then
        backCallValPtr^.OnMethod(MD5Verify);
  except
  end;

  try
    if Assigned(backCallValPtr^.OnProc) then
        backCallValPtr^.OnProc(MD5Verify);
  except
  end;

  try
      Dispose(backCallValPtr);
  except
  end;
end;

procedure TCommunicationFramework_DoubleTunnelService.Command_GetBatchStreamState(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
var
  RT: TPeerClientUserDefineForRecvTunnel;
  i: Integer;
  p: PBigStreamBatchPostData;

  de: TDataFrameEngine;
begin
  RT := GetUserDefineRecvTunnel(Sender);
  if not RT.LinkOk then
      Exit;

  for i := 0 to RT.BigStreamBatchList.Count - 1 do
    begin
      p := RT.BigStreamBatchList[i];
      de := TDataFrameEngine.Create;
      p^.Encode(de);
      OutData.WriteDataFrame(de);
      DisposeObject(de);
    end;
end;

constructor TCommunicationFramework_DoubleTunnelService.Create(RecvTunnel_, SendTunnel_: TCommunicationFrameworkServer);
begin
  inherited Create;
  FRecvTunnel := RecvTunnel_;
  FRecvTunnel.PeerClientUserDefineClass := TPeerClientUserDefineForRecvTunnel;
  FSendTunnel := SendTunnel_;
  FSendTunnel.PeerClientUserDefineClass := TPeerClientUserDefineForSendTunnel;

  FRootPath := umlCurrentPath;
  FPublicPath := FRootPath;
  FUserDB := TSectionTextData.Create(20 * 10000);
  FCanRegisterNewUser := False;
  FCanSaveUserInfo := False;
  FLoginUserList := THashVariantList.CustomCreate(8192);
  FLoginUserDefineIOList := THashObjectList.CustomCreate(False, 8192);

  FCadencerEngine := TCadencer.Create;
  FCadencerEngine.OnProgress := {$IFDEF FPC}@{$ENDIF FPC}CadencerProgress;
  FProgressEngine := TNProgressPost.Create;

  SwitchAsDefaultPerformance;

  FRecvTunnel.PrefixName := 'Double.Received';
  FSendTunnel.PrefixName := 'Double.Sending';

  FOnLinkSuccess := nil;
  FOnUserOut := nil;
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

procedure TCommunicationFramework_DoubleTunnelService.SwitchAsMaxPerformance;
begin
  FRecvTunnel.SwitchMaxPerformance;
  FSendTunnel.SwitchMaxPerformance;
end;

procedure TCommunicationFramework_DoubleTunnelService.SwitchAsMaxSecurity;
begin
  FRecvTunnel.SwitchMaxSecurity;
  FSendTunnel.SwitchMaxSecurity;
end;

procedure TCommunicationFramework_DoubleTunnelService.SwitchAsDefaultPerformance;
begin
  FRecvTunnel.SwitchDefaultPerformance;
  FSendTunnel.SwitchDefaultPerformance;
end;

procedure TCommunicationFramework_DoubleTunnelService.Progress;
begin
  FCadencerEngine.Progress;
  FRecvTunnel.Progress;
  FSendTunnel.Progress;
end;

procedure TCommunicationFramework_DoubleTunnelService.CadencerProgress(Sender: TObject; const deltaTime, newTime: Double);
begin
  FProgressEngine.Progress(deltaTime);
end;

procedure TCommunicationFramework_DoubleTunnelService.LoadUserDB;
var
  IO_Array: TIO_Array;
  pcid: Cardinal;
  cli: TPeerClientUserDefineForRecvTunnel;
begin
  if umlFileExists(umlCombineFileName(FRootPath, C_UserDB)) then
    begin
      FUserDB.LoadFromFile(umlCombineFileName(FRootPath, C_UserDB));

      FRecvTunnel.GetIO_Array(IO_Array);
      for pcid in IO_Array do
        begin
          cli := GetUserDefineRecvTunnel(FRecvTunnel.PeerIO[pcid]);
          if (cli <> nil) and (cli.LoginSuccessed) then
              cli.UserDBIntf := FUserDB.VariantList[cli.UserID];
        end;
    end;
end;

procedure TCommunicationFramework_DoubleTunnelService.SaveUserDB;
var
  IO_Array: TIO_Array;
  pcid: Cardinal;
  cli: TPeerClientUserDefineForRecvTunnel;
begin
  FUserDB.SaveToFile(umlCombineFileName(FRootPath, C_UserDB));

  FRecvTunnel.GetIO_Array(IO_Array);
  for pcid in IO_Array do
    begin
      cli := GetUserDefineRecvTunnel(FRecvTunnel.PeerIO[pcid]);
      if (cli <> nil) and (cli.LoginSuccessed) then
          cli.UserDBIntf := FUserDB.VariantList[cli.UserID];
    end;
end;

function TCommunicationFramework_DoubleTunnelService.RegUser(UsrID, UsrPasswd: SystemString; AUserConfigFile: TSectionTextData): Boolean;
var
  AUserFlag, AUserPath: SystemString;
  AUserDBIntf: THashVariantList;
  te: TSectionTextData;
begin
  Result := False;
  if umlExistsChar(UsrID, '[]:'#13#10#9#8#0) then
      Exit;

  if FUserDB.Exists(UsrID) then
      Exit;

  if FLoginUserList.Exists(UsrID) then
      Exit;

  AUserFlag := MakeUserFlag;

  AUserPath := umlCombinePath(FRootPath, AUserFlag);
  umlCreateDirectory(AUserPath);

  AUserDBIntf := FUserDB.VariantList[UsrID];
  AUserDBIntf['UserFlag'] := AUserFlag;
  AUserDBIntf['password'] := GenerateQuantumCryptographyPassword(UsrPasswd).Text;

  if AUserConfigFile <> nil then
    begin
      AUserConfigFile.Hit['UserInfo', 'UserID'] := UsrID;
      AUserConfigFile.Hit['UserInfo', 'Password'] := AUserDBIntf['password'];
      AUserConfigFile.SaveToFile(umlCombineFileName(AUserPath, 'User.Config'));
    end
  else
    begin
      te := TSectionTextData.Create;
      te.Hit['UserInfo', 'UserID'] := UsrID;
      te.Hit['UserInfo', 'Password'] := AUserDBIntf['password'];
      te.SaveToFile(umlCombineFileName(AUserPath, 'User.Config'));
      DisposeObject(te);
    end;

  if FCanSaveUserInfo then
      SaveUserDB;

  UserRegistedSuccess(UsrID);
  Result := True;
end;

function TCommunicationFramework_DoubleTunnelService.ExistsUser(UsrID: SystemString): Boolean;
begin
  Result := FUserDB.Exists(UsrID);
end;

function TCommunicationFramework_DoubleTunnelService.GetUserPath(UsrID: SystemString): SystemString;
var
  AUserFlag: SystemString;
  AUserDBIntf: THashVariantList;
begin
  Result := '';
  if not ExistsUser(UsrID) then
      Exit;

  AUserDBIntf := FUserDB.VariantList[UsrID];
  AUserFlag := AUserDBIntf.GetDefaultValue('UserFlag', '');
  Result := umlCombinePath(FRootPath, AUserFlag);
end;

function TCommunicationFramework_DoubleTunnelService.GetUserFile(UsrID, AUserFileName: SystemString): SystemString;
begin
  Result := umlCombineFileName(GetUserPath(UsrID), AUserFileName);
end;

function TCommunicationFramework_DoubleTunnelService.GetUserDefineIO(UsrID: SystemString): TPeerClientUserDefineForRecvTunnel;
begin
  Result := TPeerClientUserDefineForRecvTunnel(FLoginUserDefineIOList[UsrID]);
end;

function TCommunicationFramework_DoubleTunnelService.UserOnline(UsrID: SystemString): Boolean;
begin
  Result := GetUserDefineIO(UsrID) <> nil;
end;

function TCommunicationFramework_DoubleTunnelService.PackUserAsFile(UsrID, packageFile: SystemString): Boolean;
var
  cli: TPeerClientUserDefineForRecvTunnel;
begin
  Result := False;
  if not ExistsUser(UsrID) then
      Exit;
  cli := GetUserDefineIO(UsrID);
  if cli <> nil then
      cli.SaveConfigFile;

  BatchImportPathToDBFile(GetUserPath(UsrID), '*', packageFile);

  Result := True;
end;

function TCommunicationFramework_DoubleTunnelService.PackUserAsStream(UsrID: SystemString; packageStream: TCoreClassStream): Boolean;
var
  cli: TPeerClientUserDefineForRecvTunnel;
begin
  Result := False;
  if not ExistsUser(UsrID) then
      Exit;
  cli := GetUserDefineIO(UsrID);
  if cli <> nil then
      cli.SaveConfigFile;

  BatchImportPathToDBStream(GetUserPath(UsrID), '*', packageStream);

  Result := True;
end;

function TCommunicationFramework_DoubleTunnelService.UnPackFileAsUser(packageFile: SystemString): Boolean;
var
  fs: TCoreClassFileStream;
begin
  fs := TCoreClassFileStream.Create(packageFile, fmOpenRead or fmShareDenyNone);
  Result := UnPackStreamAsUser(fs);
  DisposeObject(fs);
end;

function TCommunicationFramework_DoubleTunnelService.UnPackStreamAsUser(packageStream: TCoreClassStream): Boolean;
var
  dbEng: TObjectDataManager;
  M: TMemoryStream64;
  te: TSectionTextData;

  UsrID, UsrPasswd: SystemString;
  AUserDBIntf: THashVariantList;
begin
  packageStream.Position := 0;
  dbEng := TObjectDataManager.CreateAsStream(packageStream, '', ObjectDataMarshal.ID, True, False, False);
  M := TMemoryStream64.Create;
  te := TSectionTextData.Create;

  try
    Result := ExtractFileInDB(dbEng, '/', 'User.Config', M);
    if Result then
      begin
        M.Position := 0;
        te.LoadFromStream(M);

        UsrID := te.GetDefaultValue('UserInfo', 'UserID', '');
        UsrPasswd := te.GetDefaultValue('UserInfo', 'Password', '');

        if not UserOnline(UsrID) then
          begin
            if not ExistsUser(UsrID) then
              begin
                DoStatus('Register new user "%s" From Pack Stream', [UsrID]);
                RegUser(UsrID, UsrPasswd, nil);
                ExtractDBToPath(dbEng, GetUserPath(UsrID));
                Result := True;
              end
            else
              begin
                DoStatus('update user "%s" From Pack Stream', [UsrID]);
                ExtractDBToPath(dbEng, GetUserPath(UsrID));

                AUserDBIntf := FUserDB.VariantList[UsrID];
                AUserDBIntf['password'] := UsrPasswd;

                SaveUserDB;
                Result := True;
              end;
          end
        else
            DoStatus('un pack error, User is Online:%s', [UsrID]);
      end
    else
        DoStatus('unpack error,no exists file user.config in pack Stream', []);
  except
      Result := False;
  end;

  DisposeObject(te);
  DisposeObject(M);
  DisposeObject(dbEng);
end;

procedure TCommunicationFramework_DoubleTunnelService.PostStoreQueueCMD(ToUserID: SystemString; Cmd: SystemString; InData: TDataFrameEngine);
var
  UserDefineIO: TPeerClientUserDefineForRecvTunnel;
  d: Double;
  p: PInt64;
  UserPath: SystemString;
  fn: SystemString;
  de: TDataFrameEngine;
  fs: TCoreClassFileStream;
begin
  UserDefineIO := GetUserDefineIO(ToUserID);
  if UserDefineIO <> nil then
    begin
      FSendTunnel.SendDirectStreamCmd(UserDefineIO.SendTunnel.Owner, Cmd, InData);
    end
  else if ExistsUser(ToUserID) then
    begin
      UserPath := GetUserPath(ToUserID);

      repeat
        TCoreClassThread.Sleep(1);
        d := Now;
        p := @d;
        fn := umlCombineFileName(UserPath, IntToHex(p^, 16) + '.queue');
      until not umlFileExists(fn);

      de := TDataFrameEngine.Create;
      de.WriteString(Cmd);
      de.WriteDataFrame(InData);
      fs := TCoreClassFileStream.Create(fn, fmCreate);
      de.EncodeAsZLib(fs);
      DisposeObject(de);
      DisposeObject(fs);
    end;
end;

procedure TCommunicationFramework_DoubleTunnelService.RegisterCommand;
begin
  FRecvTunnel.RegisterStream(C_UserLogin).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_UserLogin;
  FRecvTunnel.RegisterStream(C_RegisterUser).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_RegisterUser;
  FRecvTunnel.RegisterStream(C_TunnelLink).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_TunnelLink;
  FRecvTunnel.RegisterStream(C_ChangePasswd).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_ChangePasswd;
  FRecvTunnel.RegisterStream(C_CustomNewUser).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_CustomNewUser;
  FRecvTunnel.RegisterDirectStream(C_ProcessStoreQueueCMD).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_ProcessStoreQueueCMD;
  FRecvTunnel.RegisterStream(C_GetPublicFileList).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_GetPublicFileList;
  FRecvTunnel.RegisterStream(C_GetPrivateFileList).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_GetPrivateFileList;
  FRecvTunnel.RegisterStream(C_GetPrivateDirectoryList).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_GetPrivateDirectoryList;
  FRecvTunnel.RegisterStream(C_CreatePrivateDirectory).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_CreatePrivateDirectory;
  FRecvTunnel.RegisterStream(C_GetPublicFileInfo).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_GetPublicFileInfo;
  FRecvTunnel.RegisterStream(C_GetPrivateFileInfo).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_GetPrivateFileInfo;
  FRecvTunnel.RegisterStream(C_GetPublicFileMD5).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_GetPublicFileMD5;
  FRecvTunnel.RegisterStream(C_GetPrivateFileMD5).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_GetPrivateFileMD5;
  FRecvTunnel.RegisterStream(C_GetPublicFile).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_GetPublicFile;
  FRecvTunnel.RegisterStream(C_GetPrivateFile).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_GetPrivateFile;
  FRecvTunnel.RegisterStream(C_GetUserPrivateFile).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_GetUserPrivateFile;
  FRecvTunnel.RegisterDirectStream(C_PostPublicFileInfo).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_PostPublicFileInfo;
  FRecvTunnel.RegisterDirectStream(C_PostPrivateFileInfo).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_PostPrivateFileInfo;
  FRecvTunnel.RegisterBigStream(C_PostFile).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_PostFile;
  FRecvTunnel.RegisterDirectStream(C_PostFileOver).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_PostFileOver;

  FRecvTunnel.RegisterStream(C_GetPublicFileFragmentData).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_GetPublicFileFragmentData;
  FRecvTunnel.RegisterStream(C_GetPrivateFileFragmentData).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_GetPrivateFileFragmentData;

  FRecvTunnel.RegisterStream(C_GetCurrentCadencer).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_GetCurrentCadencer;
  FRecvTunnel.RegisterDirectStream(C_NewBatchStream).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_NewBatchStream;
  FRecvTunnel.RegisterBigStream(C_PostBatchStream).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_PostBatchStream;
  FRecvTunnel.RegisterDirectStream(C_ClearBatchStream).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_ClearBatchStream;
  FRecvTunnel.RegisterDirectStream(C_PostBatchStreamDone).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_PostBatchStreamDone;
  FRecvTunnel.RegisterStream(C_GetBatchStreamState).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_GetBatchStreamState;
end;

procedure TCommunicationFramework_DoubleTunnelService.UnRegisterCommand;
begin
  FRecvTunnel.DeleteRegistedCMD(C_UserLogin);
  FRecvTunnel.DeleteRegistedCMD(C_RegisterUser);
  FRecvTunnel.DeleteRegistedCMD(C_TunnelLink);

  FRecvTunnel.DeleteRegistedCMD(C_ChangePasswd);
  FRecvTunnel.DeleteRegistedCMD(C_CustomNewUser);

  FRecvTunnel.DeleteRegistedCMD(C_ProcessStoreQueueCMD);

  FRecvTunnel.DeleteRegistedCMD(C_GetPublicFileList);
  FRecvTunnel.DeleteRegistedCMD(C_GetPrivateFileList);
  FRecvTunnel.DeleteRegistedCMD(C_GetPrivateDirectoryList);

  FRecvTunnel.DeleteRegistedCMD(C_GetUserPrivateFileList);
  FRecvTunnel.DeleteRegistedCMD(C_GetUserPrivateDirectoryList);

  FRecvTunnel.DeleteRegistedCMD(C_CreatePrivateDirectory);

  FRecvTunnel.DeleteRegistedCMD(C_GetPublicFileInfo);
  FRecvTunnel.DeleteRegistedCMD(C_GetPrivateFileInfo);

  FRecvTunnel.DeleteRegistedCMD(C_GetPublicFileMD5);
  FRecvTunnel.DeleteRegistedCMD(C_GetPrivateFileMD5);

  FRecvTunnel.DeleteRegistedCMD(C_GetPublicFile);
  FRecvTunnel.DeleteRegistedCMD(C_GetPrivateFile);
  FRecvTunnel.DeleteRegistedCMD(C_GetUserPrivateFile);

  FRecvTunnel.DeleteRegistedCMD(C_PostPublicFileInfo);
  FRecvTunnel.DeleteRegistedCMD(C_PostPrivateFileInfo);
  FRecvTunnel.DeleteRegistedCMD(C_PostFile);
  FRecvTunnel.DeleteRegistedCMD(C_PostFileOver);

  FRecvTunnel.DeleteRegistedCMD(C_GetPublicFileFragmentData);
  FRecvTunnel.DeleteRegistedCMD(C_GetPrivateFileFragmentData);

  FRecvTunnel.DeleteRegistedCMD(C_GetCurrentCadencer);

  FRecvTunnel.DeleteRegistedCMD(C_NewBatchStream);
  FRecvTunnel.DeleteRegistedCMD(C_PostBatchStream);
  FRecvTunnel.DeleteRegistedCMD(C_ClearBatchStream);
  FRecvTunnel.DeleteRegistedCMD(C_PostBatchStreamDone);
  FRecvTunnel.DeleteRegistedCMD(C_GetBatchStreamState);
end;

function TCommunicationFramework_DoubleTunnelService.MakeUserFlag: SystemString;
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

function TCommunicationFramework_DoubleTunnelService.GetUserDefineRecvTunnel(RecvCli: TPeerIO): TPeerClientUserDefineForRecvTunnel;
begin
  if RecvCli <> nil then
      Result := RecvCli.UserDefine as TPeerClientUserDefineForRecvTunnel
  else
      Result := nil;
end;

function TCommunicationFramework_DoubleTunnelService.TotalLinkCount: Integer;
begin
  Result := FLoginUserDefineIOList.Count;
end;

procedure TCommunicationFramework_DoubleTunnelService.PostBatchStream(cli: TPeerIO; stream: TCoreClassStream; doneFreeStream: Boolean);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;

  de.WriteMD5(umlStreamMD5(stream));
  de.WritePointer(0);
  cli.SendDirectStreamCmd(C_NewBatchStream, de);
  DisposeObject(de);

  cli.SendBigStream(C_PostBatchStream, stream, doneFreeStream);
end;

procedure TCommunicationFramework_DoubleTunnelService.PostBatchStreamC(cli: TPeerIO; stream: TCoreClassStream; doneFreeStream: Boolean; OnCompletedBackcall: TStateCall);
var
  de: TDataFrameEngine;
  p: POnStateStruct;
begin
  de := TDataFrameEngine.Create;

  p := nil;

  if Assigned(OnCompletedBackcall) then
    begin
      new(p);
      p^.Init;
      p^.OnCall := OnCompletedBackcall;
    end;

  de.WriteMD5(umlStreamMD5(stream));
  de.WritePointer(p);
  cli.SendDirectStreamCmd(C_NewBatchStream, de);
  DisposeObject(de);

  cli.SendBigStream(C_PostBatchStream, stream, doneFreeStream);
end;

procedure TCommunicationFramework_DoubleTunnelService.PostBatchStreamM(cli: TPeerIO; stream: TCoreClassStream; doneFreeStream: Boolean; OnCompletedBackcall: TStateMethod);
var
  de: TDataFrameEngine;
  p: POnStateStruct;
begin
  de := TDataFrameEngine.Create;

  p := nil;

  if Assigned(OnCompletedBackcall) then
    begin
      new(p);
      p^.Init;
      p^.OnMethod := OnCompletedBackcall;
    end;

  de.WriteMD5(umlStreamMD5(stream));
  de.WritePointer(p);
  cli.SendDirectStreamCmd(C_NewBatchStream, de);
  DisposeObject(de);

  cli.SendBigStream(C_PostBatchStream, stream, doneFreeStream);
end;

procedure TCommunicationFramework_DoubleTunnelService.PostBatchStreamP(cli: TPeerIO; stream: TCoreClassStream; doneFreeStream: Boolean; OnCompletedBackcall: TStateProc);
var
  de: TDataFrameEngine;
  p: POnStateStruct;
begin
  de := TDataFrameEngine.Create;

  p := nil;

  if Assigned(OnCompletedBackcall) then
    begin
      new(p);
      p^.Init;
      p^.OnProc := OnCompletedBackcall;
    end;

  de.WriteMD5(umlStreamMD5(stream));
  de.WritePointer(p);
  cli.SendDirectStreamCmd(C_NewBatchStream, de);
  DisposeObject(de);

  cli.SendBigStream(C_PostBatchStream, stream, doneFreeStream);
end;

procedure TCommunicationFramework_DoubleTunnelService.ClearBatchStream(cli: TPeerIO);
var
  de: TDataFrameEngine;
  p: POnStateStruct;
begin
  de := TDataFrameEngine.Create;
  cli.SendDirectStreamCmd(C_ClearBatchStream, de);
  DisposeObject(de);
end;

procedure TCommunicationFramework_DoubleTunnelService.GetBatchStreamStateM(cli: TPeerIO; OnResult: TStreamMethod);
var
  de: TDataFrameEngine;
  p: POnStateStruct;
begin
  de := TDataFrameEngine.Create;
  cli.SendStreamCmdM(C_GetBatchStreamState, de, OnResult);
  DisposeObject(de);
end;

procedure TCommunicationFramework_DoubleTunnelService.GetBatchStreamStateP(cli: TPeerIO; OnResult: TStreamProc);
var
  de: TDataFrameEngine;
  p: POnStateStruct;
begin
  de := TDataFrameEngine.Create;
  cli.SendStreamCmdP(C_GetBatchStreamState, de, OnResult);
  DisposeObject(de);
end;

constructor TClientUserDefineForRecvTunnel.Create(Owner_: TPeerIO);
begin
  inherited Create(Owner_);
  Client := nil;
  SendTunnel := nil;
end;

destructor TClientUserDefineForRecvTunnel.Destroy;
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

constructor TClientUserDefineForSendTunnel.Create(Owner_: TPeerIO);
begin
  inherited Create(Owner_);
  Client := nil;
  RecvTunnel := nil;
end;

destructor TClientUserDefineForSendTunnel.Destroy;
begin
  if Client <> nil then
      Client.FLinkOk := False;
  inherited Destroy;
end;

procedure TCommunicationFramework_DoubleTunnelClient.Command_FileInfo(Sender: TPeerIO; InData: TDataFrameEngine);
var
  fn: SystemString;
  StartPos: Int64;
  FSize: Int64;
  remoteinfo: SystemString;
  fullfn: SystemString;
begin
  if FCurrentStream <> nil then
    begin
      DisposeObject(FCurrentStream);
      FCurrentStream := nil;
    end;

  fn := InData.Reader.ReadString;
  StartPos := InData.Reader.ReadInt64;
  FSize := InData.Reader.ReadInt64;
  remoteinfo := InData.Reader.ReadString;

  if not umlDirectoryExists(remoteinfo) then
      umlCreateDirectory(remoteinfo);

  fullfn := umlCombineFileName(remoteinfo, fn);
  FCurrentReceiveStreamFileName := fullfn;
  try
    if StartPos > 0 then
      begin
        FCurrentStream := TCoreClassFileStream.Create(fullfn, fmOpenReadWrite);
        FCurrentStream.Position := StartPos;
      end
    else
        FCurrentStream := TCoreClassFileStream.Create(fullfn, fmCreate);
  except
      FRecvTunnel.ClientIO.DelayClose();
  end;
end;

procedure TCommunicationFramework_DoubleTunnelClient.Command_PostFile(Sender: TPeerIO; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64);
begin
  if FCurrentStream <> nil then
    begin
      InData.Position := 0;
      if InData.Size > 0 then
          FCurrentStream.CopyFrom(InData, InData.Size);
    end;
end;

procedure TCommunicationFramework_DoubleTunnelClient.Command_PostFileOver(Sender: TPeerIO; InData: TDataFrameEngine);
var
  servMD5, MD5: TMD5;
  RemoteBackcallAddr: UInt64;
  p: PRemoteFileBackcall;
  fn: SystemString;
begin
  servMD5 := InData.Reader.ReadMD5;
  RemoteBackcallAddr := InData.Reader.ReadPointer;
  p := Pointer(RemoteBackcallAddr);
  fn := FCurrentReceiveStreamFileName;

  if FCurrentStream <> nil then
    begin
      MD5 := umlStreamMD5(FCurrentStream);
      if umlMD5Compare(servMD5, MD5) then
          Sender.Print(Format('Receive %s ok', [umlGetFileName(fn).Text]))
      else
          Sender.Print(Format('Receive %s failed!', [umlGetFileName(fn).Text]));

      try
        if p <> nil then
          begin
            if Assigned(p^.OnCompleteCall) then
              begin
                FCurrentStream.Position := 0;
                p^.OnCompleteCall(p^.UserData, p^.UserObject, FCurrentStream, fn);
              end;
            if Assigned(p^.OnCompleteMethod) then
              begin
                FCurrentStream.Position := 0;
                p^.OnCompleteMethod(p^.UserData, p^.UserObject, FCurrentStream, fn);
              end;
            if Assigned(p^.OnCompleteProc) then
              begin
                FCurrentStream.Position := 0;
                p^.OnCompleteProc(p^.UserData, p^.UserObject, FCurrentStream, fn);
              end;
            Dispose(p);
          end;
      except
      end;

      DisposeObject(FCurrentStream);
      FCurrentStream := nil;
    end;

  FRecvFileing := False;
end;

procedure TCommunicationFramework_DoubleTunnelClient.Command_PostFileFragmentData(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
var
  mem_: TMemoryStream64;
  StartPos, EndPos, siz: Int64;
  RemoteBackcallAddr: UInt64;
  p: PFileFragmentDataBackcall;
  fp: Pointer;
  MD5: TMD5;
begin
  mem_ := TMemoryStream64.Create;
  mem_.SetPointerWithProtectedMode(InData, DataSize);
  RemoteBackcallAddr := mem_.ReadUInt64;
  StartPos := mem_.ReadInt64;
  EndPos := mem_.ReadInt64;
  siz := mem_.ReadInt64;
  fp := mem_.PositionAsPtr;
  mem_.Position := mem_.Position + siz;
  MD5 := mem_.ReadMD5;
  DisposeObject(mem_);

  p := Pointer(RemoteBackcallAddr);
  if p <> nil then
    begin
      try
        if Assigned(p^.OnCompleteCall) then
            p^.OnCompleteCall(p^.UserData, p^.UserObject, p^.fileName, p^.StartPos, p^.EndPos, fp, siz, MD5);
        if Assigned(p^.OnCompleteMethod) then
            p^.OnCompleteMethod(p^.UserData, p^.UserObject, p^.fileName, p^.StartPos, p^.EndPos, fp, siz, MD5);
        if Assigned(p^.OnCompleteProc) then
            p^.OnCompleteProc(p^.UserData, p^.UserObject, p^.fileName, p^.StartPos, p^.EndPos, fp, siz, MD5);
      except
      end;
      p^.fileName := '';
      Dispose(p);
    end;
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPublicFileInfo_StreamParamResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, ResultData: TDataFrameEngine);
var
  p: PGetFileInfoStruct;
  Existed: Boolean;
  fSiz: Int64;
begin
  p := PGetFileInfoStruct(Param1);
  Existed := ResultData.Reader.ReadBool;
  fSiz := ResultData.Reader.ReadInt64;
  if p <> nil then
    begin
      if Assigned(p^.OnCompleteCall) then
          p^.OnCompleteCall(p^.UserData, p^.UserObject, p^.fileName, Existed, fSiz);
      if Assigned(p^.OnCompleteMethod) then
          p^.OnCompleteMethod(p^.UserData, p^.UserObject, p^.fileName, Existed, fSiz);
      if Assigned(p^.OnCompleteProc) then
          p^.OnCompleteProc(p^.UserData, p^.UserObject, p^.fileName, Existed, fSiz);
      p^.fileName := '';
      Dispose(p);
    end;
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPrivateFileInfo_StreamParamResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, ResultData: TDataFrameEngine);
var
  p: PGetFileInfoStruct;
  Existed: Boolean;
  fSiz: Int64;
begin
  p := PGetFileInfoStruct(Param1);
  Existed := ResultData.Reader.ReadBool;
  fSiz := ResultData.Reader.ReadInt64;
  if p <> nil then
    begin
      if Assigned(p^.OnCompleteCall) then
          p^.OnCompleteCall(p^.UserData, p^.UserObject, p^.fileName, Existed, fSiz);
      if Assigned(p^.OnCompleteMethod) then
          p^.OnCompleteMethod(p^.UserData, p^.UserObject, p^.fileName, Existed, fSiz);
      if Assigned(p^.OnCompleteProc) then
          p^.OnCompleteProc(p^.UserData, p^.UserObject, p^.fileName, Existed, fSiz);
      p^.fileName := '';
      Dispose(p);
    end;
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPublicFileMD5_StreamParamResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, ResultData: TDataFrameEngine);
var
  p: PFileMD5Struct;
  successed: Boolean;
  MD5: TMD5;
begin
  p := PFileMD5Struct(Param1);
  successed := ResultData.Reader.ReadBool;
  if successed then
      MD5 := ResultData.Reader.ReadMD5
  else
      MD5 := NullMD5;
  if p <> nil then
    begin
      if Assigned(p^.OnCompleteCall) then
          p^.OnCompleteCall(p^.UserData, p^.UserObject, p^.fileName, p^.StartPos, p^.EndPos, MD5);
      if Assigned(p^.OnCompleteMethod) then
          p^.OnCompleteMethod(p^.UserData, p^.UserObject, p^.fileName, p^.StartPos, p^.EndPos, MD5);
      if Assigned(p^.OnCompleteProc) then
          p^.OnCompleteProc(p^.UserData, p^.UserObject, p^.fileName, p^.StartPos, p^.EndPos, MD5);
      p^.fileName := '';
      Dispose(p);
    end;
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPrivateFileMD5_StreamParamResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, ResultData: TDataFrameEngine);
var
  p: PFileMD5Struct;
  successed: Boolean;
  MD5: TMD5;
begin
  p := PFileMD5Struct(Param1);
  successed := ResultData.Reader.ReadBool;
  if successed then
      MD5 := ResultData.Reader.ReadMD5
  else
      MD5 := NullMD5;
  if p <> nil then
    begin
      if Assigned(p^.OnCompleteCall) then
          p^.OnCompleteCall(p^.UserData, p^.UserObject, p^.fileName, p^.StartPos, p^.EndPos, MD5);
      if Assigned(p^.OnCompleteMethod) then
          p^.OnCompleteMethod(p^.UserData, p^.UserObject, p^.fileName, p^.StartPos, p^.EndPos, MD5);
      if Assigned(p^.OnCompleteProc) then
          p^.OnCompleteProc(p^.UserData, p^.UserObject, p^.fileName, p^.StartPos, p^.EndPos, MD5);
      p^.fileName := '';
      Dispose(p);
    end;
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPublicFile_StreamParamResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, ResultData: TDataFrameEngine);
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
  Dispose(p);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPrivateFile_StreamParamResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, ResultData: TDataFrameEngine);
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
  Dispose(p);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetFileFragmentData_StreamParamResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, ResultData: TDataFrameEngine);
var
  p: PFileFragmentDataBackcall;
begin
  if ResultData.Count > 0 then
    begin
      if ResultData.Reader.ReadBool then
          Exit;
    end;

  p := Param1;
  Dispose(p);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetCurrentCadencer_StreamResult(Sender: TPeerIO; ResultData: TDataFrameEngine);
var
  servTime: Double;
begin
  servTime := ResultData.Reader.ReadDouble;

  FCadencerEngine.Progress;
  FServerDelay := FCadencerEngine.CurrentTime - FLastCadencerTime;

  FCadencerEngine.CurrentTime := servTime + FServerDelay;
  FCadencerEngine.Progress;
end;

procedure TCommunicationFramework_DoubleTunnelClient.Command_NewBatchStream(Sender: TPeerIO; InData: TDataFrameEngine);
var
  RT: TClientUserDefineForRecvTunnel;
  p: PBigStreamBatchPostData;
begin
  if not LinkOk then
      Exit;
  RT := Sender.UserDefine as TClientUserDefineForRecvTunnel;
  p := RT.BigStreamBatchList.NewPostData;
  p^.RemoteMD5 := InData.Reader.ReadMD5;
  p^.CompletedBackcallPtr := InData.Reader.ReadPointer;
end;

procedure TCommunicationFramework_DoubleTunnelClient.Command_PostBatchStream(Sender: TPeerIO; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64);
var
  RT: TClientUserDefineForRecvTunnel;
  p: PBigStreamBatchPostData;
  de: TDataFrameEngine;
begin
  if not LinkOk then
      Exit;
  RT := Sender.UserDefine as TClientUserDefineForRecvTunnel;

  if Sender.UserDefine.BigStreamBatchList.Count > 0 then
    begin
      p := RT.BigStreamBatchList.Last;
      p^.Source.Position := p^.Source.Size;
      p^.Source.CopyFrom(InData, InData.Size);
      if (p^.Source.Size >= BigStreamTotal) then
        begin
          p^.Source.Position := 0;
          p^.SourceMD5 := umlStreamMD5(p^.Source);

          if p^.CompletedBackcallPtr <> 0 then
            begin
              de := TDataFrameEngine.Create;
              de.WriteMD5(p^.RemoteMD5);
              de.WriteMD5(p^.SourceMD5);
              de.WritePointer(p^.CompletedBackcallPtr);
              SendTunnel.SendDirectStreamCmd(C_PostBatchStreamDone, de);
              DisposeObject(de);
            end;
        end;
    end;
end;

procedure TCommunicationFramework_DoubleTunnelClient.Command_ClearBatchStream(Sender: TPeerIO; InData: TDataFrameEngine);
var
  RT: TClientUserDefineForRecvTunnel;
  p: PBigStreamBatchPostData;
  de: TDataFrameEngine;
begin
  if not LinkOk then
      Exit;
  RT := Sender.UserDefine as TClientUserDefineForRecvTunnel;
  RT.BigStreamBatchList.Clear;
end;

procedure TCommunicationFramework_DoubleTunnelClient.Command_PostBatchStreamDone(Sender: TPeerIO; InData: TDataFrameEngine);
var
  RT: TClientUserDefineForRecvTunnel;
  rMD5, sMD5: TMD5;
  backCallVal: UInt64;
  backCallValPtr: POnStateStruct;
  MD5Verify: Boolean;
begin
  if not LinkOk then
      Exit;
  RT := Sender.UserDefine as TClientUserDefineForRecvTunnel;

  rMD5 := InData.Reader.ReadMD5;
  sMD5 := InData.Reader.ReadMD5;
  backCallVal := InData.Reader.ReadPointer;

  backCallValPtr := POnStateStruct(Pointer(backCallVal));
  MD5Verify := umlMD5Compare(rMD5, sMD5);

  if backCallValPtr = nil then
      Exit;

  try
    if Assigned(backCallValPtr^.OnCall) then
        backCallValPtr^.OnCall(MD5Verify);
  except
  end;

  try
    if Assigned(backCallValPtr^.OnMethod) then
        backCallValPtr^.OnMethod(MD5Verify);
  except
  end;

  try
    if Assigned(backCallValPtr^.OnProc) then
        backCallValPtr^.OnProc(MD5Verify);
  except
  end;

  try
      Dispose(backCallValPtr);
  except
  end;
end;

procedure TCommunicationFramework_DoubleTunnelClient.Command_GetBatchStreamState(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
var
  RT: TClientUserDefineForRecvTunnel;
  i: Integer;
  p: PBigStreamBatchPostData;

  de: TDataFrameEngine;
begin
  if not LinkOk then
      Exit;
  RT := Sender.UserDefine as TClientUserDefineForRecvTunnel;

  for i := 0 to RT.BigStreamBatchList.Count - 1 do
    begin
      p := RT.BigStreamBatchList[i];
      de := TDataFrameEngine.Create;
      p^.Encode(de);
      OutData.WriteDataFrame(de);
      DisposeObject(de);
    end;
end;

procedure TCommunicationFramework_DoubleTunnelClient.ClientConnected(Sender: TCommunicationFrameworkClient);
begin
end;

procedure TCommunicationFramework_DoubleTunnelClient.ClientDisconnect(Sender: TCommunicationFrameworkClient);
begin
end;

procedure TCommunicationFramework_DoubleTunnelClient.AsyncSendConnectResult(const cState: Boolean);
begin
  if not cState then
    begin
      try
        if Assigned(FAsyncOnResultCall) then
            FAsyncOnResultCall(False);
        if Assigned(FAsyncOnResultMethod) then
            FAsyncOnResultMethod(False);
        if Assigned(FAsyncOnResultProc) then
            FAsyncOnResultProc(False);
      except
      end;
      FAsyncConnectAddr := '';
      FAsyncConnRecvPort := 0;
      FAsyncConnSendPort := 0;
      FAsyncOnResultCall := nil;
      FAsyncOnResultMethod := nil;
      FAsyncOnResultProc := nil;
      Exit;
    end;

  RecvTunnel.AsyncConnectM(FAsyncConnectAddr, FAsyncConnRecvPort, {$IFDEF FPC}@{$ENDIF FPC}AsyncRecvConnectResult);
end;

procedure TCommunicationFramework_DoubleTunnelClient.AsyncRecvConnectResult(const cState: Boolean);
begin
  if not cState then
      SendTunnel.Disconnect;

  try
    if Assigned(FAsyncOnResultCall) then
        FAsyncOnResultCall(cState);
    if Assigned(FAsyncOnResultMethod) then
        FAsyncOnResultMethod(cState);
    if Assigned(FAsyncOnResultProc) then
        FAsyncOnResultProc(cState);
  except
  end;

  FAsyncConnectAddr := '';
  FAsyncConnRecvPort := 0;
  FAsyncConnSendPort := 0;
  FAsyncOnResultCall := nil;
  FAsyncOnResultMethod := nil;
  FAsyncOnResultProc := nil;
end;

procedure TCommunicationFramework_DoubleTunnelClient.UserLogin_OnResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, ResultData: TDataFrameEngine);
var
  r: Boolean;
  p: POnStateStruct;
begin
  p := Param1;
  r := False;
  if ResultData.Count > 0 then
    begin
      r := ResultData.ReadBool(0);
      FSendTunnel.ClientIO.Print(ResultData.ReadString(1));
    end;

  if Assigned(p^.OnCall) then
      p^.OnCall(r);
  if Assigned(p^.OnMethod) then
      p^.OnMethod(r);
  if Assigned(p^.OnProc) then
      p^.OnProc(r);

  Dispose(p);
end;

procedure TCommunicationFramework_DoubleTunnelClient.UserLogin_OnFailed(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDataFrameEngine);
var
  p: POnStateStruct;
begin
  p := Param1;
  if Assigned(p^.OnCall) then
      p^.OnCall(False);
  if Assigned(p^.OnMethod) then
      p^.OnMethod(False);
  if Assigned(p^.OnProc) then
      p^.OnProc(False);

  Dispose(p);
end;

procedure TCommunicationFramework_DoubleTunnelClient.RegisterUser_OnResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, ResultData: TDataFrameEngine);
var
  r: Boolean;
  p: POnStateStruct;
begin
  p := Param1;
  r := False;
  if ResultData.Count > 0 then
    begin
      r := ResultData.ReadBool(0);
      FSendTunnel.ClientIO.Print(ResultData.ReadString(1));
    end;

  if Assigned(p^.OnCall) then
      p^.OnCall(r);
  if Assigned(p^.OnMethod) then
      p^.OnMethod(r);
  if Assigned(p^.OnProc) then
      p^.OnProc(r);

  Dispose(p);
end;

procedure TCommunicationFramework_DoubleTunnelClient.RegisterUser_OnFailed(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDataFrameEngine);
var
  p: POnStateStruct;
begin
  p := Param1;
  if Assigned(p^.OnCall) then
      p^.OnCall(False);
  if Assigned(p^.OnMethod) then
      p^.OnMethod(False);
  if Assigned(p^.OnProc) then
      p^.OnProc(False);

  Dispose(p);
end;

procedure TCommunicationFramework_DoubleTunnelClient.TunnelLink_OnResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, ResultData: TDataFrameEngine);
var
  r: Boolean;
  p: POnStateStruct;
begin
  p := Param1;
  r := False;
  if ResultData.Count > 0 then
    begin
      r := ResultData.ReadBool(0);
      FSendTunnel.ClientIO.Print(ResultData.ReadString(1));

      if r then
        begin
          TClientUserDefineForSendTunnel(FSendTunnel.ClientIO.UserDefine).Client := Self;
          TClientUserDefineForSendTunnel(FSendTunnel.ClientIO.UserDefine).RecvTunnel := TClientUserDefineForRecvTunnel(FRecvTunnel.ClientIO.UserDefine);

          TClientUserDefineForRecvTunnel(FRecvTunnel.ClientIO.UserDefine).Client := Self;
          TClientUserDefineForRecvTunnel(FRecvTunnel.ClientIO.UserDefine).SendTunnel := TClientUserDefineForSendTunnel(FSendTunnel.ClientIO.UserDefine);

          FLinkOk := True;
        end;
    end;

  if Assigned(p^.OnCall) then
      p^.OnCall(r);
  if Assigned(p^.OnMethod) then
      p^.OnMethod(r);
  if Assigned(p^.OnProc) then
      p^.OnProc(r);

  Dispose(p);
end;

procedure TCommunicationFramework_DoubleTunnelClient.TunnelLink_OnFailed(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDataFrameEngine);
var
  p: POnStateStruct;
begin
  p := Param1;
  if Assigned(p^.OnCall) then
      p^.OnCall(False);
  if Assigned(p^.OnMethod) then
      p^.OnMethod(False);
  if Assigned(p^.OnProc) then
      p^.OnProc(False);

  Dispose(p);
end;

constructor TCommunicationFramework_DoubleTunnelClient.Create(RecvTunnel_, SendTunnel_: TCommunicationFrameworkClient);
begin
  inherited Create;
  FRecvTunnel := RecvTunnel_;
  FRecvTunnel.NotyifyInterface := Self;
  FRecvTunnel.PeerClientUserDefineClass := TClientUserDefineForRecvTunnel;

  FSendTunnel := SendTunnel_;
  FSendTunnel.NotyifyInterface := Self;
  FSendTunnel.PeerClientUserDefineClass := TClientUserDefineForSendTunnel;

  FCurrentStream := nil;
  FCurrentReceiveStreamFileName := '';
  FLinkOk := False;
  FWaitCommandTimeout := 8000;

  FRecvFileing := False;
  FRecvFileOfBatching := False;
  FRecvFileName := '';

  FCadencerEngine := TCadencer.Create;
  FCadencerEngine.OnProgress := {$IFDEF FPC}@{$ENDIF FPC}CadencerProgress;
  FLastCadencerTime := 0;
  FServerDelay := 0;

  FProgressEngine := TNProgressPost.Create;

  FAsyncConnectAddr := '';
  FAsyncConnRecvPort := 0;
  FAsyncConnSendPort := 0;
  FAsyncOnResultCall := nil;
  FAsyncOnResultMethod := nil;
  FAsyncOnResultProc := nil;
  { }
  SwitchAsDefaultPerformance;

  FRecvTunnel.PrefixName := 'Double.Received';
  FSendTunnel.PrefixName := 'Double.Sending';
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

procedure TCommunicationFramework_DoubleTunnelClient.SwitchAsMaxPerformance;
begin
  FRecvTunnel.SwitchMaxPerformance;
  FSendTunnel.SwitchMaxPerformance;
end;

procedure TCommunicationFramework_DoubleTunnelClient.SwitchAsMaxSecurity;
begin
  FRecvTunnel.SwitchMaxSecurity;
  FSendTunnel.SwitchMaxSecurity;
end;

procedure TCommunicationFramework_DoubleTunnelClient.SwitchAsDefaultPerformance;
begin
  FRecvTunnel.SwitchDefaultPerformance;
  FSendTunnel.SwitchDefaultPerformance;
end;

procedure TCommunicationFramework_DoubleTunnelClient.Progress;
begin
  try
    FCadencerEngine.Progress;

    FRecvTunnel.Progress;
    FSendTunnel.Progress;

    if not Connected then
        FLinkOk := False;
  except
  end;
end;

procedure TCommunicationFramework_DoubleTunnelClient.CadencerProgress(Sender: TObject; const deltaTime, newTime: Double);
begin
  FProgressEngine.Progress(deltaTime);
end;

function TCommunicationFramework_DoubleTunnelClient.Connect(addr: SystemString; const RecvPort, SendPort: Word): Boolean;
var
  t: Cardinal;
begin
  Result := False;
  Disconnect;

  if not FSendTunnel.Connect(addr, SendPort) then
    begin
      DoStatus('connect %s failed!', [addr]);
      Exit;
    end;
  if not FRecvTunnel.Connect(addr, RecvPort) then
    begin
      DoStatus('connect %s failed!', [addr]);
      Exit;
    end;

  t := GetTimeTick + 10000;
  while not RemoteInited do
    begin
      if TCoreClassThread.GetTickCount > t then
          Break;
      if not Connected then
          Break;
      Progress;
    end;

  Result := Connected;
end;

procedure TCommunicationFramework_DoubleTunnelClient.AsyncConnectC(addr: SystemString; const RecvPort, SendPort: Word; OnResult: TStateCall);
begin
  Disconnect;
  FAsyncConnectAddr := addr;
  FAsyncConnRecvPort := RecvPort;
  FAsyncConnSendPort := SendPort;
  FAsyncOnResultCall := OnResult;
  FAsyncOnResultMethod := nil;
  FAsyncOnResultProc := nil;
  SendTunnel.AsyncConnectM(FAsyncConnectAddr, FAsyncConnSendPort, {$IFDEF FPC}@{$ENDIF FPC}AsyncSendConnectResult);
end;

procedure TCommunicationFramework_DoubleTunnelClient.AsyncConnectM(addr: SystemString; const RecvPort, SendPort: Word; OnResult: TStateMethod);
begin
  Disconnect;
  FAsyncConnectAddr := addr;
  FAsyncConnRecvPort := RecvPort;
  FAsyncConnSendPort := SendPort;
  FAsyncOnResultCall := nil;
  FAsyncOnResultMethod := OnResult;
  FAsyncOnResultProc := nil;
  SendTunnel.AsyncConnectM(FAsyncConnectAddr, FAsyncConnSendPort, {$IFDEF FPC}@{$ENDIF FPC}AsyncSendConnectResult);
end;

procedure TCommunicationFramework_DoubleTunnelClient.AsyncConnectP(addr: SystemString; const RecvPort, SendPort: Word; OnResult: TStateProc);
begin
  Disconnect;
  FAsyncConnectAddr := addr;
  FAsyncConnRecvPort := RecvPort;
  FAsyncConnSendPort := SendPort;
  FAsyncOnResultCall := nil;
  FAsyncOnResultMethod := nil;
  FAsyncOnResultProc := OnResult;

  SendTunnel.AsyncConnectM(FAsyncConnectAddr, FAsyncConnSendPort, {$IFDEF FPC}@{$ENDIF FPC}AsyncSendConnectResult);
end;

procedure TCommunicationFramework_DoubleTunnelClient.AsyncConnectC(addr: SystemString; const RecvPort, SendPort: Word; Param1: Pointer; Param2: TObject; OnResult: TParamStateCall);
var
  ParamBridge: TStateParamBridge;
begin
  ParamBridge := TStateParamBridge.Create;
  ParamBridge.Param1 := Param1;
  ParamBridge.Param2 := Param2;
  ParamBridge.OnNotifyC := OnResult;
  AsyncConnectM(addr, RecvPort, SendPort, {$IFDEF FPC}@{$ENDIF FPC}ParamBridge.DoStateResult);
end;

procedure TCommunicationFramework_DoubleTunnelClient.AsyncConnectM(addr: SystemString; const RecvPort, SendPort: Word; Param1: Pointer; Param2: TObject; OnResult: TParamStateMethod);
var
  ParamBridge: TStateParamBridge;
begin
  ParamBridge := TStateParamBridge.Create;
  ParamBridge.Param1 := Param1;
  ParamBridge.Param2 := Param2;
  ParamBridge.OnNotifyM := OnResult;
  AsyncConnectM(addr, RecvPort, SendPort, {$IFDEF FPC}@{$ENDIF FPC}ParamBridge.DoStateResult);
end;

procedure TCommunicationFramework_DoubleTunnelClient.AsyncConnectP(addr: SystemString; const RecvPort, SendPort: Word; Param1: Pointer; Param2: TObject; OnResult: TParamStateProc);
var
  ParamBridge: TStateParamBridge;
begin
  ParamBridge := TStateParamBridge.Create;
  ParamBridge.Param1 := Param1;
  ParamBridge.Param2 := Param2;
  ParamBridge.OnNotifyP := OnResult;
  AsyncConnectM(addr, RecvPort, SendPort, {$IFDEF FPC}@{$ENDIF FPC}ParamBridge.DoStateResult);
end;

procedure TCommunicationFramework_DoubleTunnelClient.Disconnect;
begin
  if FSendTunnel.ClientIO <> nil then
      FSendTunnel.Disconnect;

  if FRecvTunnel.ClientIO <> nil then
      FRecvTunnel.Disconnect;

  FAsyncConnectAddr := '';
  FAsyncConnRecvPort := 0;
  FAsyncConnSendPort := 0;
  FAsyncOnResultCall := nil;
  FAsyncOnResultMethod := nil;
  FAsyncOnResultProc := nil;
end;

function TCommunicationFramework_DoubleTunnelClient.UserLogin(UserID, passwd: SystemString): Boolean;
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
  sendDE.WriteString(passwd);
  FSendTunnel.WaitSendStreamCmd(C_UserLogin, sendDE, resDE, FWaitCommandTimeout * 2);

  if resDE.Count > 0 then
    begin
      Result := resDE.ReadBool(0);
      FSendTunnel.ClientIO.Print(resDE.ReadString(1));
    end;

  DisposeObject(sendDE);
  DisposeObject(resDE);
end;

function TCommunicationFramework_DoubleTunnelClient.RegisterUser(UserID, passwd: SystemString): Boolean;
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
  sendDE.WriteString(passwd);
  FSendTunnel.WaitSendStreamCmd(C_RegisterUser, sendDE, resDE, FWaitCommandTimeout * 2);

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

  FSendTunnel.WaitSendStreamCmd(C_TunnelLink, sendDE, resDE, FWaitCommandTimeout * 2);

  if resDE.Count > 0 then
    begin
      Result := resDE.ReadBool(0);
      FSendTunnel.ClientIO.Print(resDE.ReadString(1));

      if Result then
        begin
          TClientUserDefineForSendTunnel(FSendTunnel.ClientIO.UserDefine).Client := Self;
          TClientUserDefineForSendTunnel(FSendTunnel.ClientIO.UserDefine).RecvTunnel := TClientUserDefineForRecvTunnel(FRecvTunnel.ClientIO.UserDefine);

          TClientUserDefineForRecvTunnel(FRecvTunnel.ClientIO.UserDefine).Client := Self;
          TClientUserDefineForRecvTunnel(FRecvTunnel.ClientIO.UserDefine).SendTunnel := TClientUserDefineForSendTunnel(FSendTunnel.ClientIO.UserDefine);
          FLinkOk := True;
        end;
    end;

  DisposeObject(sendDE);
  DisposeObject(resDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.UserLoginC(UserID, passwd: SystemString; OnCall: TStateCall);
var
  sendDE: TDataFrameEngine;
  p: POnStateStruct;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;
  sendDE := TDataFrameEngine.Create;

  sendDE.WriteCardinal(FRecvTunnel.RemoteID);
  sendDE.WriteString(UserID);
  sendDE.WriteString(passwd);

  new(p);
  p^.Init;
  p^.OnCall := OnCall;
  FSendTunnel.SendStreamCmdM(C_UserLogin, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}UserLogin_OnResult, {$IFDEF FPC}@{$ENDIF FPC}UserLogin_OnFailed);
  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.UserLoginM(UserID, passwd: SystemString; OnMethod: TStateMethod);
var
  sendDE: TDataFrameEngine;
  p: POnStateStruct;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;
  sendDE := TDataFrameEngine.Create;

  sendDE.WriteCardinal(FRecvTunnel.RemoteID);
  sendDE.WriteString(UserID);
  sendDE.WriteString(passwd);

  new(p);
  p^.Init;
  p^.OnMethod := OnMethod;
  FSendTunnel.SendStreamCmdM(C_UserLogin, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}UserLogin_OnResult, {$IFDEF FPC}@{$ENDIF FPC}UserLogin_OnFailed);
  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.UserLoginP(UserID, passwd: SystemString; OnProc: TStateProc);
var
  sendDE: TDataFrameEngine;
  p: POnStateStruct;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;
  sendDE := TDataFrameEngine.Create;

  sendDE.WriteCardinal(FRecvTunnel.RemoteID);
  sendDE.WriteString(UserID);
  sendDE.WriteString(passwd);

  new(p);
  p^.Init;
  p^.OnProc := OnProc;
  FSendTunnel.SendStreamCmdM(C_UserLogin, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}UserLogin_OnResult, {$IFDEF FPC}@{$ENDIF FPC}UserLogin_OnFailed);
  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.RegisterUserC(UserID, passwd: SystemString; OnCall: TStateCall);
var
  sendDE: TDataFrameEngine;
  p: POnStateStruct;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;
  sendDE := TDataFrameEngine.Create;

  sendDE.WriteCardinal(FRecvTunnel.RemoteID);
  sendDE.WriteString(UserID);
  sendDE.WriteString(passwd);

  new(p);
  p^.Init;
  p^.OnCall := OnCall;
  FSendTunnel.SendStreamCmdM(C_RegisterUser, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}RegisterUser_OnResult, {$IFDEF FPC}@{$ENDIF FPC}RegisterUser_OnFailed);
  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.RegisterUserM(UserID, passwd: SystemString; OnMethod: TStateMethod);
var
  sendDE: TDataFrameEngine;
  p: POnStateStruct;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;
  sendDE := TDataFrameEngine.Create;

  sendDE.WriteCardinal(FRecvTunnel.RemoteID);
  sendDE.WriteString(UserID);
  sendDE.WriteString(passwd);

  new(p);
  p^.Init;
  p^.OnMethod := OnMethod;
  FSendTunnel.SendStreamCmdM(C_RegisterUser, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}RegisterUser_OnResult, {$IFDEF FPC}@{$ENDIF FPC}RegisterUser_OnFailed);
  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.RegisterUserP(UserID, passwd: SystemString; OnProc: TStateProc);
var
  sendDE: TDataFrameEngine;
  p: POnStateStruct;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;
  sendDE := TDataFrameEngine.Create;

  sendDE.WriteCardinal(FRecvTunnel.RemoteID);
  sendDE.WriteString(UserID);
  sendDE.WriteString(passwd);

  new(p);
  p^.Init;
  p^.OnProc := OnProc;
  FSendTunnel.SendStreamCmdM(C_RegisterUser, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}RegisterUser_OnResult, {$IFDEF FPC}@{$ENDIF FPC}RegisterUser_OnFailed);
  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.TunnelLinkC(OnCall: TStateCall);
var
  sendDE: TDataFrameEngine;
  p: POnStateStruct;
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
  new(p);
  p^.Init;
  p^.OnCall := OnCall;
  FSendTunnel.SendStreamCmdM(C_TunnelLink, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}TunnelLink_OnResult, {$IFDEF FPC}@{$ENDIF FPC}TunnelLink_OnFailed);
  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.TunnelLinkM(OnMethod: TStateMethod);
var
  sendDE: TDataFrameEngine;
  p: POnStateStruct;
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
  new(p);
  p^.Init;
  p^.OnMethod := OnMethod;
  FSendTunnel.SendStreamCmdM(C_TunnelLink, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}TunnelLink_OnResult, {$IFDEF FPC}@{$ENDIF FPC}TunnelLink_OnFailed);
  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.TunnelLinkP(OnProc: TStateProc);
var
  sendDE: TDataFrameEngine;
  p: POnStateStruct;
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
  new(p);
  p^.Init;
  p^.OnProc := OnProc;
  FSendTunnel.SendStreamCmdM(C_TunnelLink, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}TunnelLink_OnResult, {$IFDEF FPC}@{$ENDIF FPC}TunnelLink_OnFailed);
  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.SyncCadencer;
var
  sendDE: TDataFrameEngine;
begin
  sendDE := TDataFrameEngine.Create;

  FCadencerEngine.Progress;
  FLastCadencerTime := FCadencerEngine.CurrentTime;
  FServerDelay := 0;
  sendDE.WriteDouble(FLastCadencerTime);
  FSendTunnel.SendStreamCmdM(C_GetCurrentCadencer, sendDE, {$IFDEF FPC}@{$ENDIF FPC}GetCurrentCadencer_StreamResult);
  DisposeObject(sendDE);
end;

function TCommunicationFramework_DoubleTunnelClient.ChnagePassword(oldPasswd, newPasswd: SystemString): Boolean;
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

  FSendTunnel.WaitSendStreamCmd(C_ChangePasswd, sendDE, resDE, FWaitCommandTimeout);

  if resDE.Count = 2 then
      Result := resDE.ReadBool(0);

  DisposeObject(sendDE);
  DisposeObject(resDE);
end;

function TCommunicationFramework_DoubleTunnelClient.CustomNewUser(UsrID, UsrPasswd: SystemString; AUserConfigFile: TSectionTextData): Boolean;
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

  sendDE.WriteString(UsrID);
  sendDE.WriteString(UsrPasswd);
  sendDE.WriteSectionText(AUserConfigFile);

  FSendTunnel.WaitSendStreamCmd(C_CustomNewUser, sendDE, resDE, FWaitCommandTimeout);

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

  FSendTunnel.SendDirectStreamCmd(C_ProcessStoreQueueCMD, sendDE);

  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPublicFileList(Filter: SystemString; lst: TCoreClassStrings);
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

  FSendTunnel.WaitSendStreamCmd(C_GetPublicFileList, sendDE, resDE, FWaitCommandTimeout);

  while not resDE.Reader.IsEnd do
      lst.Add(resDE.Reader.ReadString);

  DisposeObject(sendDE);
  DisposeObject(resDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPrivateFileList(Filter, DirectoryName: SystemString; lst: TCoreClassStrings);
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
  sendDE.WriteString(DirectoryName);

  FSendTunnel.WaitSendStreamCmd(C_GetPrivateFileList, sendDE, resDE, FWaitCommandTimeout);

  while not resDE.Reader.IsEnd do
      lst.Add(resDE.Reader.ReadString);

  DisposeObject(sendDE);
  DisposeObject(resDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPrivateFileList(Filter: SystemString; lst: TCoreClassStrings);
begin
  GetPrivateFileList(Filter, '', lst);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPrivateDirectoryList(Filter, DirectoryName: SystemString; lst: TCoreClassStrings);
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
  sendDE.WriteString(DirectoryName);

  FSendTunnel.WaitSendStreamCmd(C_GetPrivateDirectoryList, sendDE, resDE, FWaitCommandTimeout);

  while not resDE.Reader.IsEnd do
      lst.Add(resDE.Reader.ReadString);

  DisposeObject(sendDE);
  DisposeObject(resDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPrivateDirectoryList(Filter: SystemString; lst: TCoreClassStrings);
begin
  GetPrivateDirectoryList(Filter, '', lst);
end;

function TCommunicationFramework_DoubleTunnelClient.CreatePrivateDirectory(DirectoryName: SystemString): Boolean;
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

  sendDE.WriteString(DirectoryName);

  FSendTunnel.WaitSendStreamCmd(C_CreatePrivateDirectory, sendDE, resDE, FWaitCommandTimeout);

  if resDE.Count > 0 then
    begin
      Result := resDE.Reader.ReadBool;
      FSendTunnel.ClientIO.Print(resDE.Reader.ReadString);
    end;

  DisposeObject(sendDE);
  DisposeObject(resDE);
end;

{ remote file exists }
procedure TCommunicationFramework_DoubleTunnelClient.GetPublicFileInfoC(fileName: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TGetFileInfoCall);
var
  sendDE: TDataFrameEngine;
  p: PGetFileInfoStruct;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteString(fileName);
  new(p);
  p^.Init;
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.fileName := fileName;
  p^.OnCompleteCall := OnComplete;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetPublicFileInfo, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetPublicFileInfo_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPublicFileInfoM(fileName: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TGetFileInfoMethod);
var
  sendDE: TDataFrameEngine;
  p: PGetFileInfoStruct;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteString(fileName);
  new(p);
  p^.Init;
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.fileName := fileName;
  p^.OnCompleteMethod := OnComplete;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetPublicFileInfo, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetPublicFileInfo_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPublicFileInfoP(fileName: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TGetFileInfoProc);
var
  sendDE: TDataFrameEngine;
  p: PGetFileInfoStruct;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteString(fileName);
  new(p);
  p^.Init;
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.fileName := fileName;
  p^.OnCompleteProc := OnComplete;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetPublicFileInfo, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetPublicFileInfo_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPrivateFileInfoC(fileName, DirectoryName: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TGetFileInfoCall);
var
  sendDE: TDataFrameEngine;
  p: PGetFileInfoStruct;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteString(fileName);
  sendDE.WriteString(DirectoryName);
  new(p);
  p^.Init;
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.fileName := fileName;
  p^.OnCompleteCall := OnComplete;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetPrivateFileInfo, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetPrivateFileInfo_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPrivateFileInfoM(fileName, DirectoryName: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TGetFileInfoMethod);
var
  sendDE: TDataFrameEngine;
  p: PGetFileInfoStruct;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteString(fileName);
  sendDE.WriteString(DirectoryName);
  new(p);
  p^.Init;
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.fileName := fileName;
  p^.OnCompleteMethod := OnComplete;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetPrivateFileInfo, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetPrivateFileInfo_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPrivateFileInfoP(fileName, DirectoryName: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TGetFileInfoProc);
var
  sendDE: TDataFrameEngine;
  p: PGetFileInfoStruct;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteString(fileName);
  sendDE.WriteString(DirectoryName);
  new(p);
  p^.Init;
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.fileName := fileName;
  p^.OnCompleteProc := OnComplete;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetPrivateFileInfo, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetPrivateFileInfo_StreamParamResult);
  DisposeObject(sendDE);
end;

{ }
{ remote md5 support with public store space }
procedure TCommunicationFramework_DoubleTunnelClient.GetPublicFileMD5C(fileName: SystemString; const StartPos, EndPos: Int64;
  const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileMD5Call);
var
  sendDE: TDataFrameEngine;
  p: PFileMD5Struct;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteString(fileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteInt64(EndPos);
  new(p);
  p^.Init;
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.fileName := fileName;
  p^.StartPos := StartPos;
  p^.EndPos := EndPos;
  p^.OnCompleteCall := OnComplete;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetPublicFileMD5, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetPublicFileMD5_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPublicFileMD5M(fileName: SystemString; const StartPos, EndPos: Int64;
  const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileMD5Method);
var
  sendDE: TDataFrameEngine;
  p: PFileMD5Struct;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteString(fileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteInt64(EndPos);
  new(p);
  p^.Init;
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.fileName := fileName;
  p^.StartPos := StartPos;
  p^.EndPos := EndPos;
  p^.OnCompleteMethod := OnComplete;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetPublicFileMD5, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetPublicFileMD5_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPublicFileMD5P(fileName: SystemString; const StartPos, EndPos: Int64;
  const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileMD5Proc);
var
  sendDE: TDataFrameEngine;
  p: PFileMD5Struct;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteString(fileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteInt64(EndPos);
  new(p);
  p^.Init;
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.fileName := fileName;
  p^.StartPos := StartPos;
  p^.EndPos := EndPos;
  p^.OnCompleteProc := OnComplete;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetPublicFileMD5, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetPublicFileMD5_StreamParamResult);
  DisposeObject(sendDE);
end;

{ remote md5 support with private store space }
procedure TCommunicationFramework_DoubleTunnelClient.GetPrivateFileMD5C(fileName, DirectoryName: SystemString; const StartPos, EndPos: Int64;
  const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileMD5Call);
var
  sendDE: TDataFrameEngine;
  p: PFileMD5Struct;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteString(fileName);
  sendDE.WriteString(DirectoryName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteInt64(EndPos);
  new(p);
  p^.Init;
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.fileName := fileName;
  p^.StartPos := StartPos;
  p^.EndPos := EndPos;
  p^.OnCompleteCall := OnComplete;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetPrivateFileMD5, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetPrivateFileMD5_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPrivateFileMD5M(fileName, DirectoryName: SystemString; const StartPos, EndPos: Int64;
  const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileMD5Method);
var
  sendDE: TDataFrameEngine;
  p: PFileMD5Struct;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteString(fileName);
  sendDE.WriteString(DirectoryName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteInt64(EndPos);
  new(p);
  p^.Init;
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.fileName := fileName;
  p^.StartPos := StartPos;
  p^.EndPos := EndPos;
  p^.OnCompleteMethod := OnComplete;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetPrivateFileMD5, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetPrivateFileMD5_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPrivateFileMD5P(fileName, DirectoryName: SystemString; const StartPos, EndPos: Int64;
  const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileMD5Proc);
var
  sendDE: TDataFrameEngine;
  p: PFileMD5Struct;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteString(fileName);
  sendDE.WriteString(DirectoryName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteInt64(EndPos);
  new(p);
  p^.Init;
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.fileName := fileName;
  p^.StartPos := StartPos;
  p^.EndPos := EndPos;
  p^.OnCompleteProc := OnComplete;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetPrivateFileMD5, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetPrivateFileMD5_StreamParamResult);
  DisposeObject(sendDE);
end;

function TCommunicationFramework_DoubleTunnelClient.GetPublicFile(fileName, saveToPath: SystemString): Boolean;
begin
  Result := GetPublicFile(fileName, 0, saveToPath);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPublicFileC(fileName, saveToPath: SystemString;
  const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteCall);
begin
  GetPublicFileC(fileName, 0, saveToPath, UserData, UserObject, OnComplete);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPublicFileM(fileName, saveToPath: SystemString;
  const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteMethod);
begin
  GetPublicFileM(fileName, 0, saveToPath, UserData, UserObject, OnComplete);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPublicFileP(fileName, saveToPath: SystemString;
  const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteProc);
begin
  GetPublicFileP(fileName, 0, saveToPath, UserData, UserObject, OnComplete);
end;

{ restore download from public }
function TCommunicationFramework_DoubleTunnelClient.GetPublicFile(fileName: SystemString; StartPos: Int64; saveToPath: SystemString): Boolean;
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
  sendDE.WriteInt64(StartPos);
  sendDE.WriteString(saveToPath);
  sendDE.WritePointer(0);

  FSendTunnel.WaitSendStreamCmd(C_GetPublicFile, sendDE, resDE, FWaitCommandTimeout);

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

procedure TCommunicationFramework_DoubleTunnelClient.GetPublicFileC(fileName: SystemString; StartPos: Int64; saveToPath: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteCall);
var
  sendDE: TDataFrameEngine;
  p: PRemoteFileBackcall;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDataFrameEngine.Create;

  sendDE.WriteString(fileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteString(saveToPath);
  new(p);
  p^.Init;
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.OnCompleteCall := OnComplete;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetPublicFile, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetPublicFile_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPublicFileM(fileName: SystemString; StartPos: Int64; saveToPath: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteMethod);
var
  sendDE: TDataFrameEngine;
  p: PRemoteFileBackcall;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDataFrameEngine.Create;

  sendDE.WriteString(fileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteString(saveToPath);
  new(p);
  p^.Init;
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.OnCompleteMethod := OnComplete;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetPublicFile, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetPublicFile_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPublicFileP(fileName: SystemString; StartPos: Int64; saveToPath: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteProc);
var
  sendDE: TDataFrameEngine;
  p: PRemoteFileBackcall;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDataFrameEngine.Create;

  sendDE.WriteString(fileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteString(saveToPath);
  new(p);
  p^.Init;
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.OnCompleteProc := OnComplete;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetPublicFile, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetPublicFile_StreamParamResult);
  DisposeObject(sendDE);
end;

function TCommunicationFramework_DoubleTunnelClient.GetPrivateFile(fileName, DirectoryName, saveToPath: SystemString): Boolean;
begin
  Result := GetPrivateFile(fileName, 0, DirectoryName, saveToPath);
end;

function TCommunicationFramework_DoubleTunnelClient.GetPrivateFile(fileName, saveToPath: SystemString): Boolean;
begin
  Result := GetPrivateFile(fileName, '', saveToPath);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPrivateFileC(fileName, DirectoryName, saveToPath: SystemString;
  const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteCall);
begin
  GetPrivateFileC(fileName, 0, DirectoryName, saveToPath, UserData, UserObject, OnComplete);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPrivateFileM(fileName, DirectoryName, saveToPath: SystemString;
  const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteMethod);
begin
  GetPrivateFileM(fileName, 0, DirectoryName, saveToPath, UserData, UserObject, OnComplete);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPrivateFileP(fileName, DirectoryName, saveToPath: SystemString;
  const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteProc);
begin
  GetPrivateFileP(fileName, 0, DirectoryName, saveToPath, UserData, UserObject, OnComplete);
end;

{ restore download from user }
function TCommunicationFramework_DoubleTunnelClient.GetPrivateFile(fileName: SystemString; StartPos: Int64; DirectoryName, saveToPath: SystemString): Boolean;
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
  sendDE.WriteString(DirectoryName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteString(saveToPath);
  sendDE.WritePointer(0);

  FSendTunnel.WaitSendStreamCmd(C_GetPrivateFile, sendDE, resDE, FWaitCommandTimeout);

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

function TCommunicationFramework_DoubleTunnelClient.GetPrivateFile(fileName: SystemString; StartPos: Int64; saveToPath: SystemString): Boolean;
begin
  Result := GetPrivateFile(fileName, StartPos, '', saveToPath);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPrivateFileC(fileName: SystemString; StartPos: Int64; DirectoryName, saveToPath: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteCall);
var
  sendDE: TDataFrameEngine;
  p: PRemoteFileBackcall;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDataFrameEngine.Create;

  sendDE.WriteString(fileName);
  sendDE.WriteString(DirectoryName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteString(saveToPath);
  new(p);
  p^.Init;
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.OnCompleteCall := OnComplete;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetPrivateFile, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetPrivateFile_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPrivateFileM(fileName: SystemString; StartPos: Int64; DirectoryName, saveToPath: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteMethod);
var
  sendDE: TDataFrameEngine;
  p: PRemoteFileBackcall;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDataFrameEngine.Create;

  sendDE.WriteString(fileName);
  sendDE.WriteString(DirectoryName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteString(saveToPath);
  new(p);
  p^.Init;
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.OnCompleteMethod := OnComplete;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetPrivateFile, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetPrivateFile_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPrivateFileP(fileName: SystemString; StartPos: Int64; DirectoryName, saveToPath: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteProc);
var
  sendDE: TDataFrameEngine;
  p: PRemoteFileBackcall;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDataFrameEngine.Create;

  sendDE.WriteString(fileName);
  sendDE.WriteString(DirectoryName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteString(saveToPath);
  new(p);
  p^.Init;
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.OnCompleteProc := OnComplete;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetPrivateFile, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetPrivateFile_StreamParamResult);
  DisposeObject(sendDE);
end;

function TCommunicationFramework_DoubleTunnelClient.GetUserPrivateFile(UserID, fileName, DirectoryName, saveToPath: SystemString): Boolean;
begin
  Result := GetUserPrivateFile(UserID, fileName, 0, DirectoryName, saveToPath);
end;

function TCommunicationFramework_DoubleTunnelClient.GetUserPrivateFile(UserID, fileName, saveToPath: SystemString): Boolean;
begin
  Result := GetUserPrivateFile(UserID, fileName, '', saveToPath);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetUserPrivateFileC(UserID, fileName, DirectoryName, saveToPath: SystemString;
  const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteCall);
begin
  GetUserPrivateFileC(UserID, fileName, 0, DirectoryName, saveToPath, UserData, UserObject, OnComplete);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetUserPrivateFileM(UserID, fileName, DirectoryName, saveToPath: SystemString;
  const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteMethod);
begin
  GetUserPrivateFileM(UserID, fileName, 0, DirectoryName, saveToPath, UserData, UserObject, OnComplete);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetUserPrivateFileP(UserID, fileName, DirectoryName, saveToPath: SystemString;
  const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteProc);
begin
  GetUserPrivateFileP(UserID, fileName, 0, DirectoryName, saveToPath, UserData, UserObject, OnComplete);
end;

{ restore download with custom user }
function TCommunicationFramework_DoubleTunnelClient.GetUserPrivateFile(UserID, fileName: SystemString; StartPos: Int64; DirectoryName, saveToPath: SystemString): Boolean;
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
  sendDE.WriteString(DirectoryName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteString(saveToPath);
  sendDE.WritePointer(0);

  FSendTunnel.WaitSendStreamCmd(C_GetUserPrivateFile, sendDE, resDE, FWaitCommandTimeout);

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

function TCommunicationFramework_DoubleTunnelClient.GetUserPrivateFile(UserID, fileName: SystemString; StartPos: Int64; saveToPath: SystemString): Boolean;
begin
  Result := GetUserPrivateFile(UserID, fileName, StartPos, '', saveToPath);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetUserPrivateFileC(UserID, fileName: SystemString; StartPos: Int64; DirectoryName, saveToPath: SystemString;
  const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteCall);
var
  sendDE: TDataFrameEngine;
  p: PRemoteFileBackcall;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDataFrameEngine.Create;

  sendDE.WriteString(UserID);
  sendDE.WriteString(fileName);
  sendDE.WriteString(DirectoryName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteString(saveToPath);
  new(p);
  p^.Init;
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.OnCompleteCall := OnComplete;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetUserPrivateFile, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetPrivateFile_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetUserPrivateFileM(UserID, fileName: SystemString; StartPos: Int64; DirectoryName, saveToPath: SystemString;
  const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteMethod);
var
  sendDE: TDataFrameEngine;
  p: PRemoteFileBackcall;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDataFrameEngine.Create;

  sendDE.WriteString(UserID);
  sendDE.WriteString(fileName);
  sendDE.WriteString(DirectoryName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteString(saveToPath);
  new(p);
  p^.Init;
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.OnCompleteMethod := OnComplete;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetUserPrivateFile, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetPrivateFile_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetUserPrivateFileP(UserID, fileName: SystemString; StartPos: Int64; DirectoryName, saveToPath: SystemString;
  const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteProc);
var
  sendDE: TDataFrameEngine;
  p: PRemoteFileBackcall;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDataFrameEngine.Create;

  sendDE.WriteString(UserID);
  sendDE.WriteString(fileName);
  sendDE.WriteString(DirectoryName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteString(saveToPath);
  new(p);
  p^.Init;
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.OnCompleteProc := OnComplete;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetUserPrivateFile, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetPrivateFile_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPublicFileFragmentDataC(fileName: SystemString; StartPos, EndPos: Int64;
  const UserData: Pointer; const UserObject: TCoreClassObject; const OnCompleteCall: TFileFragmentDataCall);
var
  sendDE: TDataFrameEngine;
  p: PFileFragmentDataBackcall;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  new(p);
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.fileName := fileName;
  p^.StartPos := StartPos;
  p^.EndPos := EndPos;
  p^.OnCompleteCall := OnCompleteCall;
  p^.OnCompleteMethod := nil;
  p^.OnCompleteProc := nil;

  sendDE := TDataFrameEngine.Create;

  sendDE.WriteString(fileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteInt64(EndPos);
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetPublicFileFragmentData, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetFileFragmentData_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPublicFileFragmentDataM(fileName: SystemString; StartPos, EndPos: Int64;
  const UserData: Pointer; const UserObject: TCoreClassObject; const OnCompleteMethod: TFileFragmentDataMethod);
var
  sendDE: TDataFrameEngine;
  p: PFileFragmentDataBackcall;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  new(p);
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.fileName := fileName;
  p^.StartPos := StartPos;
  p^.EndPos := EndPos;
  p^.OnCompleteCall := nil;
  p^.OnCompleteMethod := OnCompleteMethod;
  p^.OnCompleteProc := nil;

  sendDE := TDataFrameEngine.Create;

  sendDE.WriteString(fileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteInt64(EndPos);
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetPublicFileFragmentData, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetFileFragmentData_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPublicFileFragmentDataP(fileName: SystemString; StartPos, EndPos: Int64;
  const UserData: Pointer; const UserObject: TCoreClassObject; const OnCompleteProc: TFileFragmentDataProc);
var
  sendDE: TDataFrameEngine;
  p: PFileFragmentDataBackcall;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  new(p);
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.fileName := fileName;
  p^.StartPos := StartPos;
  p^.EndPos := EndPos;
  p^.OnCompleteCall := nil;
  p^.OnCompleteMethod := nil;
  p^.OnCompleteProc := OnCompleteProc;

  sendDE := TDataFrameEngine.Create;

  sendDE.WriteString(fileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteInt64(EndPos);
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetPublicFileFragmentData, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetFileFragmentData_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPrivateFileFragmentDataC(fileName: SystemString; StartPos, EndPos: Int64;
  const UserData: Pointer; const UserObject: TCoreClassObject; const OnCompleteCall: TFileFragmentDataCall);
var
  sendDE: TDataFrameEngine;
  p: PFileFragmentDataBackcall;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  new(p);
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.fileName := fileName;
  p^.StartPos := StartPos;
  p^.EndPos := EndPos;
  p^.OnCompleteCall := OnCompleteCall;
  p^.OnCompleteMethod := nil;
  p^.OnCompleteProc := nil;

  sendDE := TDataFrameEngine.Create;

  sendDE.WriteString(fileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteInt64(EndPos);
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetPrivateFileFragmentData, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetFileFragmentData_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPrivateFileFragmentDataM(fileName: SystemString; StartPos, EndPos: Int64;
  const UserData: Pointer; const UserObject: TCoreClassObject; const OnCompleteMethod: TFileFragmentDataMethod);
var
  sendDE: TDataFrameEngine;
  p: PFileFragmentDataBackcall;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  new(p);
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.fileName := fileName;
  p^.StartPos := StartPos;
  p^.EndPos := EndPos;
  p^.OnCompleteCall := nil;
  p^.OnCompleteMethod := OnCompleteMethod;
  p^.OnCompleteProc := nil;

  sendDE := TDataFrameEngine.Create;

  sendDE.WriteString(fileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteInt64(EndPos);
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetPrivateFileFragmentData, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetFileFragmentData_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPrivateFileFragmentDataP(fileName: SystemString; StartPos, EndPos: Int64;
  const UserData: Pointer; const UserObject: TCoreClassObject; const OnCompleteProc: TFileFragmentDataProc);
var
  sendDE: TDataFrameEngine;
  p: PFileFragmentDataBackcall;
begin
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  new(p);
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.fileName := fileName;
  p^.StartPos := StartPos;
  p^.EndPos := EndPos;
  p^.OnCompleteCall := nil;
  p^.OnCompleteMethod := nil;
  p^.OnCompleteProc := OnCompleteProc;

  sendDE := TDataFrameEngine.Create;

  sendDE.WriteString(fileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteInt64(EndPos);
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetPrivateFileFragmentData, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetFileFragmentData_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.PostFileToPublic(fileName: SystemString);
begin
  PostFileToPublic(fileName, 0);
end;

procedure TCommunicationFramework_DoubleTunnelClient.PostFileToPublic(fileName: SystemString; StartPos: Int64);
var
  sendDE: TDataFrameEngine;
  fs: TCoreClassFileStream;
  MD5: TMD5;
begin
  if not umlFileExists(fileName) then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  fs := TCoreClassFileStream.Create(fileName, fmOpenRead or fmShareDenyNone);

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteString(umlGetFileName(fileName));
  sendDE.WriteInt64(StartPos);
  sendDE.WriteInt64(fs.Size);
  FSendTunnel.SendDirectStreamCmd(C_PostPublicFileInfo, sendDE);
  DisposeObject(sendDE);

  MD5 := umlStreamMD5(fs);

  FSendTunnel.SendBigStream(C_PostFile, fs, StartPos, True);

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteMD5(MD5);
  FSendTunnel.SendDirectStreamCmd(C_PostFileOver, sendDE);
  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.PostFileToPrivate(fileName, DirectoryName: SystemString);
begin
  PostFileToPrivate(fileName, DirectoryName, 0);
end;

procedure TCommunicationFramework_DoubleTunnelClient.PostFileToPrivate(fileName: SystemString);
begin
  PostFileToPrivate(fileName, '');
end;

procedure TCommunicationFramework_DoubleTunnelClient.PostFileToPrivate(fileName, DirectoryName: SystemString; StartPos: Int64);
var
  sendDE: TDataFrameEngine;
  fs: TCoreClassFileStream;
  MD5: TMD5;
begin
  if not umlFileExists(fileName) then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  fs := TCoreClassFileStream.Create(fileName, fmOpenRead or fmShareDenyNone);

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteString(umlGetFileName(fileName));
  sendDE.WriteString(DirectoryName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteInt64(fs.Size);
  FSendTunnel.SendDirectStreamCmd(C_PostPrivateFileInfo, sendDE);
  DisposeObject(sendDE);

  MD5 := umlStreamMD5(fs);

  FSendTunnel.SendBigStream(C_PostFile, fs, StartPos, True);

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteMD5(MD5);
  FSendTunnel.SendDirectStreamCmd(C_PostFileOver, sendDE);
  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.PostFileToPrivate(fileName: SystemString; StartPos: Int64);
begin
  PostFileToPrivate(fileName, '', StartPos);
end;

procedure TCommunicationFramework_DoubleTunnelClient.PostStreamToPrivate(RemoteFileName, DirectoryName: SystemString; stream: TCoreClassStream; doneFreeStream: Boolean);
begin
  PostStreamToPrivate(RemoteFileName, DirectoryName, stream, 0, doneFreeStream);
end;

procedure TCommunicationFramework_DoubleTunnelClient.PostStreamToPrivate(RemoteFileName, DirectoryName: SystemString; stream: TCoreClassStream; StartPos: Int64; doneFreeStream: Boolean);
var
  sendDE: TDataFrameEngine;
  MD5: TMD5;
begin
  if not FSendTunnel.Connected then
    begin
      if doneFreeStream then
          DisposeObject(stream);
      Exit;
    end;
  if not FRecvTunnel.Connected then
    begin
      if doneFreeStream then
          DisposeObject(stream);
      Exit;
    end;

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteString(RemoteFileName);
  sendDE.WriteString(DirectoryName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteInt64(stream.Size);
  FSendTunnel.SendDirectStreamCmd(C_PostPrivateFileInfo, sendDE);
  DisposeObject(sendDE);

  MD5 := umlStreamMD5(stream);

  FSendTunnel.SendBigStream(C_PostFile, stream, StartPos, doneFreeStream);

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteMD5(MD5);
  FSendTunnel.SendDirectStreamCmd(C_PostFileOver, sendDE);
  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.PostBatchStream(stream: TCoreClassStream; doneFreeStream: Boolean);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;

  de.WriteMD5(umlStreamMD5(stream));
  de.WritePointer(0);
  SendTunnel.SendDirectStreamCmd(C_NewBatchStream, de);
  DisposeObject(de);

  SendTunnel.SendBigStream(C_PostBatchStream, stream, doneFreeStream);
end;

procedure TCommunicationFramework_DoubleTunnelClient.PostBatchStreamC(stream: TCoreClassStream; doneFreeStream: Boolean; OnCompletedBackcall: TStateCall);
var
  de: TDataFrameEngine;
  p: POnStateStruct;
begin
  de := TDataFrameEngine.Create;

  p := nil;

  if Assigned(OnCompletedBackcall) then
    begin
      new(p);
      p^.Init;
      p^.OnCall := OnCompletedBackcall;
    end;

  de.WriteMD5(umlStreamMD5(stream));
  de.WritePointer(p);
  SendTunnel.SendDirectStreamCmd(C_NewBatchStream, de);
  DisposeObject(de);

  SendTunnel.SendBigStream(C_PostBatchStream, stream, doneFreeStream);
end;

procedure TCommunicationFramework_DoubleTunnelClient.PostBatchStreamM(stream: TCoreClassStream; doneFreeStream: Boolean; OnCompletedBackcall: TStateMethod);
var
  de: TDataFrameEngine;
  p: POnStateStruct;
begin
  de := TDataFrameEngine.Create;

  p := nil;

  if Assigned(OnCompletedBackcall) then
    begin
      new(p);
      p^.Init;
      p^.OnMethod := OnCompletedBackcall;
    end;

  de.WriteMD5(umlStreamMD5(stream));
  de.WritePointer(p);
  SendTunnel.SendDirectStreamCmd(C_NewBatchStream, de);
  DisposeObject(de);

  SendTunnel.SendBigStream(C_PostBatchStream, stream, doneFreeStream);
end;

procedure TCommunicationFramework_DoubleTunnelClient.PostBatchStreamP(stream: TCoreClassStream; doneFreeStream: Boolean; OnCompletedBackcall: TStateProc);
var
  de: TDataFrameEngine;
  p: POnStateStruct;
begin
  de := TDataFrameEngine.Create;

  p := nil;

  if Assigned(OnCompletedBackcall) then
    begin
      new(p);
      p^.Init;
      p^.OnProc := OnCompletedBackcall;
    end;

  de.WriteMD5(umlStreamMD5(stream));
  de.WritePointer(p);
  SendTunnel.SendDirectStreamCmd(C_NewBatchStream, de);
  DisposeObject(de);

  SendTunnel.SendBigStream(C_PostBatchStream, stream, doneFreeStream);
end;

procedure TCommunicationFramework_DoubleTunnelClient.ClearBatchStream;
var
  de: TDataFrameEngine;
  p: POnStateStruct;
begin
  de := TDataFrameEngine.Create;
  SendTunnel.SendDirectStreamCmd(C_ClearBatchStream, de);
  DisposeObject(de);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetBatchStreamStateM(OnResult: TStreamMethod);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  SendTunnel.SendStreamCmdM(C_GetBatchStreamState, de, OnResult);
  DisposeObject(de);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetBatchStreamStateP(OnResult: TStreamProc);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  SendTunnel.SendStreamCmdP(C_GetBatchStreamState, de, OnResult);
  DisposeObject(de);
end;

function TCommunicationFramework_DoubleTunnelClient.GetBatchStreamState(ResultData: TDataFrameEngine; ATimeOut: TTimeTick): Boolean;
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  SendTunnel.WaitSendStreamCmd(C_GetBatchStreamState, de, ResultData, ATimeOut);
  Result := ResultData.Count > 0;
  DisposeObject(de);
end;

procedure TCommunicationFramework_DoubleTunnelClient.RegisterCommand;
begin
  FRecvTunnel.RegisterDirectStream(C_FileInfo).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_FileInfo;
  FRecvTunnel.RegisterBigStream(C_PostFile).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_PostFile;
  FRecvTunnel.RegisterDirectStream(C_PostFileOver).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_PostFileOver;
  FRecvTunnel.RegisterCompleteBuffer(C_PostFileFragmentData).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_PostFileFragmentData;

  FRecvTunnel.RegisterDirectStream(C_NewBatchStream).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_NewBatchStream;
  FRecvTunnel.RegisterBigStream(C_PostBatchStream).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_PostBatchStream;
  FRecvTunnel.RegisterDirectStream(C_ClearBatchStream).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_ClearBatchStream;
  FRecvTunnel.RegisterDirectStream(C_PostBatchStreamDone).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_PostBatchStreamDone;
  FRecvTunnel.RegisterStream(C_GetBatchStreamState).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_GetBatchStreamState;
end;

procedure TCommunicationFramework_DoubleTunnelClient.UnRegisterCommand;
begin
  FRecvTunnel.DeleteRegistedCMD(C_FileInfo);
  FRecvTunnel.DeleteRegistedCMD(C_PostFile);
  FRecvTunnel.DeleteRegistedCMD(C_PostFileOver);
  FRecvTunnel.DeleteRegistedCMD(C_PostFileFragmentData);

  FRecvTunnel.DeleteRegistedCMD(C_NewBatchStream);
  FRecvTunnel.DeleteRegistedCMD(C_PostBatchStream);
  FRecvTunnel.DeleteRegistedCMD(C_ClearBatchStream);
  FRecvTunnel.DeleteRegistedCMD(C_PostBatchStreamDone);
  FRecvTunnel.DeleteRegistedCMD(C_GetBatchStreamState);
end;

function TCommunicationFramework_DoubleTunnelClient.RemoteInited: Boolean;
begin
  Result := FSendTunnel.RemoteInited and FRecvTunnel.RemoteInited;
end;

end.
