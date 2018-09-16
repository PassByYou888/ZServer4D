{ ****************************************************************************** }
{ * double tunnel IO framework(incl Auth and File service)                     * }
{ * written by QQ 600585@qq.com                                                * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }
(*
  update history

  2018-1 added file support anonymous function
*)

unit CommunicationFrameworkDoubleTunnelIO;

interface

{$INCLUDE zDefine.inc}


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
    RecvTunnel: TPeerClientUserDefineForRecvTunnel;
    RecvTunnelID: Cardinal;
    DoubleTunnelService: TCommunicationFramework_DoubleTunnelService;

    constructor Create(AOwner: TPeerIO); override;
    destructor Destroy; override;
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

    constructor Create(AOwner: TPeerIO); override;
    destructor Destroy; override;

    function MakeFilePath(fn: SystemString): SystemString;
    function GetUserID: SystemString;

    procedure SaveConfigFile; virtual;

    function LinkOk: Boolean;
  end;

  TCommunicationFramework_DoubleTunnelService = class(TCoreClassInterfacedObject)
  protected
    FRecvTunnel, FSendTunnel: TCommunicationFrameworkServer;
    FRootPath, FPublicPath: SystemString;
    FUserDB: TSectionTextData;
    FCanRegisterNewUser: Boolean;
    FCanSaveUserInfo: Boolean;
    FCanStatus: Boolean;
    FLoginUserList: THashVariantList;
    FLoginUserDefineIOList: THashObjectList;
    FCadencerEngine: TCadencer;
    FProgressEngine: TNProgressPost;
  protected
    procedure UserRegistedSuccess(UserID: SystemString); virtual;
    procedure UserLoginSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel); virtual;
    procedure UserLinkSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel); virtual;
    procedure UserCreateDirectorySuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel; dn: SystemString); virtual;
    procedure UserPostFileSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel; fn: SystemString); virtual;
    procedure UserOut(UserDefineIO: TPeerClientUserDefineForRecvTunnel); virtual;
  protected
    // registed server command
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

    procedure Command_GetCurrentCadencer(Sender: TPeerIO; InData, OutData: TDataFrameEngine); virtual;

    procedure Command_NewBatchStream(Sender: TPeerIO; InData: TDataFrameEngine); virtual;
    procedure Command_PostBatchStream(Sender: TPeerIO; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64); virtual;
    procedure Command_ClearBatchStream(Sender: TPeerIO; InData: TDataFrameEngine); virtual;
    procedure Command_PostBatchStreamDone(Sender: TPeerIO; InData: TDataFrameEngine); virtual;
    procedure Command_GetBatchStreamState(Sender: TPeerIO; InData, OutData: TDataFrameEngine); virtual;
  public
    constructor Create(ARecvTunnel, ASendTunnel: TCommunicationFrameworkServer);
    destructor Destroy; override;

    procedure SwitchAsMaxPerformance;
    procedure SwitchAsMaxSafe;
    procedure SwitchAsDefaultPerformance;

    procedure Progress; virtual;
    procedure CadencerProgress(Sender: TObject; const deltaTime, newTime: Double); virtual;

    // load USERDB
    // load need execute in rootpath set completed after
    procedure LoadUserDB;

    procedure SaveUserDB;

    function RegUser(AUserID, APasswd: SystemString; AUserConfigFile: TSectionTextData): Boolean;
    function ExistsUser(AUserID: SystemString): Boolean;
    function GetUserPath(AUserID: SystemString): SystemString;
    function GetUserFile(AUserID, AUserFileName: SystemString): SystemString;
    function GetUserDefineIO(AUserID: SystemString): TPeerClientUserDefineForRecvTunnel;
    function UserOnline(AUserID: SystemString): Boolean;

    function PackUserAsFile(AUserID, aPackFile: SystemString): Boolean;
    function PackUserAsStream(AUserID: SystemString; PackStream: TCoreClassStream): Boolean;
    function UnPackFileAsUser(aPackFile: SystemString): Boolean;
    function UnPackStreamAsUser(aPackStream: TCoreClassStream): Boolean;

    // only work in direct command
    // if user online immediate execution
    // if user offline store to notify queue
    procedure PostStoreQueueCMD(ToUserID: SystemString; Cmd: SystemString; InData: TDataFrameEngine);

    procedure RegisterCommand; virtual;
    procedure UnRegisterCommand; virtual;

    function MakeUserFlag: SystemString;
    function GetUserDefineRecvTunnel(RecvCli: TPeerIO): TPeerClientUserDefineForRecvTunnel;

    function TotalLinkCount: Integer;

    procedure PostBatchStream(cli: TPeerIO; stream: TCoreClassStream; doneFreeStream: Boolean); overload;
    procedure PostBatchStream(cli: TPeerIO; stream: TCoreClassStream; doneFreeStream: Boolean; OnCompletedBackcall: TStateCall); overload;
    procedure PostBatchStream(cli: TPeerIO; stream: TCoreClassStream; doneFreeStream: Boolean; OnCompletedBackcall: TStateMethod); overload;
{$IFNDEF FPC} procedure PostBatchStream(cli: TPeerIO; stream: TCoreClassStream; doneFreeStream: Boolean; OnCompletedBackcall: TStateProc); overload; {$ENDIF}
    procedure ClearBatchStream(cli: TPeerIO);
    procedure GetBatchStreamState(cli: TPeerIO; OnResult: TStreamMethod); overload;
{$IFNDEF FPC} procedure GetBatchStreamState(cli: TPeerIO; OnResult: TStreamProc); overload; {$ENDIF}
    property LoginUserList: THashVariantList read FLoginUserList;

    property CanRegisterNewUser: Boolean read FCanRegisterNewUser write FCanRegisterNewUser;
    property CanSaveUserInfo: Boolean read FCanSaveUserInfo write FCanSaveUserInfo;
    property CanStatus: Boolean read FCanStatus write FCanStatus;

    // private store space
    property RootPath: SystemString read FRootPath write FRootPath;
    // public store space
    property PublicPath: SystemString read FPublicPath write FPublicPath;

    property CadencerEngine: TCadencer read FCadencerEngine;
    property ProgressEngine: TNProgressPost read FProgressEngine;

    property RecvTunnel: TCommunicationFrameworkServer read FRecvTunnel;
    property SendTunnel: TCommunicationFrameworkServer read FSendTunnel;
  end;

  TCommunicationFramework_DoubleTunnelClient = class;

  TClientUserDefineForSendTunnel = class;

  TClientUserDefineForRecvTunnel = class(TPeerClientUserDefine)
  public
    Client: TCommunicationFramework_DoubleTunnelClient;
    SendTunnel: TClientUserDefineForSendTunnel;

    constructor Create(AOwner: TPeerIO); override;
    destructor Destroy; override;
  end;

  TClientUserDefineForSendTunnel = class(TPeerClientUserDefine)
  public
    Client: TCommunicationFramework_DoubleTunnelClient;
    RecvTunnel: TClientUserDefineForRecvTunnel;

    constructor Create(AOwner: TPeerIO); override;
    destructor Destroy; override;
  end;

  TGetFileInfoCall = procedure(const UserData: Pointer; const UserObject: TCoreClassObject;
    const fileName: SystemString; const Existed: Boolean; const fSiz: Int64);
  TGetFileInfoMethod = procedure(const UserData: Pointer; const UserObject: TCoreClassObject;
    const fileName: SystemString; const Existed: Boolean; const fSiz: Int64) of object;
  TFileMD5Call = procedure(const UserData: Pointer; const UserObject: TCoreClassObject;
    const fileName: SystemString; const StartPos, EndPos: Int64; const MD5: UnicodeMixedLib.TMD5);
  TFileMD5Method = procedure(const UserData: Pointer; const UserObject: TCoreClassObject;
    const fileName: SystemString; const StartPos, EndPos: Int64; const MD5: UnicodeMixedLib.TMD5) of object;
  TFileCompleteCall   = procedure(const UserData: Pointer; const UserObject: TCoreClassObject; stream: TCoreClassStream; const fileName: SystemString);
  TFileCompleteMethod = procedure(const UserData: Pointer; const UserObject: TCoreClassObject; stream: TCoreClassStream; const fileName: SystemString) of object;
{$IFNDEF FPC}
  TGetFileInfoProc = reference to procedure(const UserData: Pointer; const UserObject: TCoreClassObject;
    const fileName: SystemString; const Existed: Boolean; const fSiz: Int64);
  TFileMD5Proc = reference to procedure(const UserData: Pointer; const UserObject: TCoreClassObject;
    const fileName: SystemString; const StartPos, EndPos: Int64; const MD5: UnicodeMixedLib.TMD5);
  TFileCompleteProc = reference to procedure(const UserData: Pointer; const UserObject: TCoreClassObject; stream: TCoreClassStream; const fileName: SystemString);
{$ENDIF}

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
  protected
    // registed client command
    procedure Command_FileInfo(Sender: TPeerIO; InData: TDataFrameEngine); virtual;
    procedure Command_PostFile(Sender: TPeerIO; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64); virtual;
    procedure Command_PostFileOver(Sender: TPeerIO; InData: TDataFrameEngine); virtual;

    procedure GetPublicFileInfo_StreamParamResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, ResultData: TDataFrameEngine);
    procedure GetPrivateFileInfo_StreamParamResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, ResultData: TDataFrameEngine);

    procedure GetPublicFileMD5_StreamParamResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, ResultData: TDataFrameEngine);
    procedure GetPrivateFileMD5_StreamParamResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, ResultData: TDataFrameEngine);

    procedure GetPublicFile_StreamParamResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, ResultData: TDataFrameEngine);
    procedure GetPrivateFile_StreamParamResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, ResultData: TDataFrameEngine);

    // GetCurrentCadencer result proc
    procedure GetCurrentCadencer_StreamResult(Sender: TPeerIO; ResultData: TDataFrameEngine);

    // batch stream suppport
    procedure Command_NewBatchStream(Sender: TPeerIO; InData: TDataFrameEngine); virtual;
    procedure Command_PostBatchStream(Sender: TPeerIO; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64); virtual;
    procedure Command_ClearBatchStream(Sender: TPeerIO; InData: TDataFrameEngine); virtual;
    procedure Command_PostBatchStreamDone(Sender: TPeerIO; InData: TDataFrameEngine); virtual;
    procedure Command_GetBatchStreamState(Sender: TPeerIO; InData, OutData: TDataFrameEngine); virtual;
  protected
    // client notify interface
    procedure ClientConnected(Sender: TCommunicationFrameworkClient); virtual;
    procedure ClientDisconnect(Sender: TCommunicationFrameworkClient); virtual;
  protected
    // async connect support
    FAsyncConnectAddr: SystemString;
    FAsyncConnRecvPort, FAsyncConnSendPort: Word;
    FAsyncOnResultCall: TStateCall;
    FAsyncOnResultMethod: TStateMethod;
{$IFNDEF FPC}
    FAsyncOnResultProc: TStateProc;
{$ENDIF}
    procedure AsyncSendConnectResult(const cState: Boolean);
    procedure AsyncRecvConnectResult(const cState: Boolean);
  public
    constructor Create(ARecvTunnel, ASendTunnel: TCommunicationFrameworkClient);
    destructor Destroy; override;

    function Connected: Boolean; virtual;

    procedure SwitchAsMaxPerformance;
    procedure SwitchAsMaxSafe;
    procedure SwitchAsDefaultPerformance;

    procedure Progress; virtual;
    procedure CadencerProgress(Sender: TObject; const deltaTime, newTime: Double); virtual;

    // sync connect
    function Connect(addr: SystemString; const RecvPort, SendPort: Word): Boolean; virtual;

    // async
    procedure AsyncConnect(addr: SystemString; const RecvPort, SendPort: Word; OnResult: TStateCall); overload; virtual;
    procedure AsyncConnect(addr: SystemString; const RecvPort, SendPort: Word; OnResult: TStateMethod); overload; virtual;
{$IFNDEF FPC} procedure AsyncConnect(addr: SystemString; const RecvPort, SendPort: Word; OnResult: TStateProc); overload; virtual; {$ENDIF}
    //
    procedure Disconnect; virtual;

    // Block mode
    function UserLogin(UserID, passwd: SystemString): Boolean; overload; virtual;
    function RegisterUser(UserID, passwd: SystemString): Boolean; overload; virtual;
    function TunnelLink: Boolean; overload; virtual;

    // unblock mode
{$IFNDEF FPC}
    procedure UserLogin(UserID, passwd: SystemString; OnProc: TStateProc); overload; virtual;
    procedure RegisterUser(UserID, passwd: SystemString; OnProc: TStateProc); overload; virtual;
    procedure TunnelLink(OnProc: TStateProc); overload; virtual;
{$ENDIF}
    procedure SyncCadencer; virtual;

    // Block mode
    function ChnagePassword(oldPasswd, newPasswd: SystemString): Boolean;
    function CustomNewUser(AUserID, APasswd: SystemString; AUserConfigFile: TSectionTextData): Boolean;

    procedure ProcessStoreQueueCMD;

    procedure GetPublicFileList(Filter: SystemString; lst: TCoreClassStrings);
    procedure GetPrivateFileList(Filter, DirectoryName: SystemString; lst: TCoreClassStrings); overload;
    procedure GetPrivateFileList(Filter: SystemString; lst: TCoreClassStrings); overload;
    procedure GetPrivateDirectoryList(Filter, DirectoryName: SystemString; lst: TCoreClassStrings); overload;
    procedure GetPrivateDirectoryList(Filter: SystemString; lst: TCoreClassStrings); overload;
    function CreatePrivateDirectory(DirectoryName: SystemString): Boolean;

    // remote file information
    procedure GetPublicFileInfo(fileName: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TGetFileInfoCall); overload;
    procedure GetPublicFileInfo(fileName: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TGetFileInfoMethod); overload;
{$IFNDEF FPC} procedure GetPublicFileInfo(fileName: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TGetFileInfoProc); overload; {$ENDIF FPC}
    procedure GetPrivateFileInfo(fileName, DirectoryName: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TGetFileInfoCall); overload;
    procedure GetPrivateFileInfo(fileName, DirectoryName: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TGetFileInfoMethod); overload;
{$IFNDEF FPC} procedure GetPrivateFileInfo(fileName, DirectoryName: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TGetFileInfoProc); overload; {$ENDIF FPC}
    //
    // remote md5 support with public store space
    procedure GetPublicFileMD5(fileName: SystemString; const StartPos, EndPos: Int64;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileMD5Call); overload;
    procedure GetPublicFileMD5(fileName: SystemString; const StartPos, EndPos: Int64;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileMD5Method); overload;
{$IFNDEF FPC} procedure GetPublicFileMD5(fileName: SystemString; const StartPos, EndPos: Int64;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileMD5Proc); overload; {$ENDIF FPC}
    // remote md5 support with private store space
    procedure GetPrivateFileMD5(fileName, DirectoryName: SystemString; const StartPos, EndPos: Int64;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileMD5Call); overload;
    procedure GetPrivateFileMD5(fileName, DirectoryName: SystemString; const StartPos, EndPos: Int64;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileMD5Method); overload;
{$IFNDEF FPC} procedure GetPrivateFileMD5(fileName, DirectoryName: SystemString; const StartPos, EndPos: Int64;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileMD5Proc); overload; {$ENDIF FPC}
    //
    // normal download with public store space
    function GetPublicFile(fileName, saveToPath: SystemString): Boolean; overload;
    procedure GetPublicFile(fileName, saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteCall); overload;
    procedure GetPublicFile(fileName, saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteMethod); overload;
{$IFNDEF FPC} procedure GetPublicFile(fileName, saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteProc); overload; {$ENDIF}
    // restore download with public store space
    function GetPublicFile(fileName: SystemString; StartPos: Int64; saveToPath: SystemString): Boolean; overload;
    procedure GetPublicFile(fileName: SystemString; StartPos: Int64; saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteCall); overload;
    procedure GetPublicFile(fileName: SystemString; StartPos: Int64; saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteMethod); overload;
{$IFNDEF FPC} procedure GetPublicFile(fileName: SystemString; StartPos: Int64; saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteProc); overload; {$ENDIF}
    //
    // normal download with private store space
    function GetPrivateFile(fileName, DirectoryName, saveToPath: SystemString): Boolean; overload;
    function GetPrivateFile(fileName, saveToPath: SystemString): Boolean; overload;
    procedure GetPrivateFile(fileName, DirectoryName, saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteCall); overload;
    procedure GetPrivateFile(fileName, DirectoryName, saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteMethod); overload;
{$IFNDEF FPC} procedure GetPrivateFile(fileName, DirectoryName, saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteProc); overload; {$ENDIF}
    // restore download with private store space
    function GetPrivateFile(fileName: SystemString; StartPos: Int64; DirectoryName, saveToPath: SystemString): Boolean; overload;
    function GetPrivateFile(fileName: SystemString; StartPos: Int64; saveToPath: SystemString): Boolean; overload;
    procedure GetPrivateFile(fileName: SystemString; StartPos: Int64; DirectoryName, saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteCall); overload;
    procedure GetPrivateFile(fileName: SystemString; StartPos: Int64; DirectoryName, saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteMethod); overload;
{$IFNDEF FPC} procedure GetPrivateFile(fileName: SystemString; StartPos: Int64; DirectoryName, saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteProc); overload; {$ENDIF}
    //
    // normal download with custom user
    function GetUserPrivateFile(UserID, fileName, DirectoryName, saveToPath: SystemString): Boolean; overload;
    function GetUserPrivateFile(UserID, fileName, saveToPath: SystemString): Boolean; overload;
    procedure GetUserPrivateFile(UserID, fileName, DirectoryName, saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteCall); overload;
    procedure GetUserPrivateFile(UserID, fileName, DirectoryName, saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteMethod); overload;
{$IFNDEF FPC} procedure GetUserPrivateFile(UserID, fileName, DirectoryName, saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteProc); overload; {$ENDIF}
    // restore download with custom user
    function GetUserPrivateFile(UserID, fileName: SystemString; StartPos: Int64; DirectoryName, saveToPath: SystemString): Boolean; overload;
    function GetUserPrivateFile(UserID, fileName: SystemString; StartPos: Int64; saveToPath: SystemString): Boolean; overload;
    procedure GetUserPrivateFile(UserID, fileName: SystemString; StartPos: Int64; DirectoryName, saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteCall); overload;
    procedure GetUserPrivateFile(UserID, fileName: SystemString; StartPos: Int64; DirectoryName, saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteMethod); overload;
{$IFNDEF FPC} procedure GetUserPrivateFile(UserID, fileName: SystemString; StartPos: Int64; DirectoryName, saveToPath: SystemString;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteProc); overload; {$ENDIF}
    //
    // upload file to public store space
    procedure PostFileToPublic(fileName: SystemString); overload;
    // restore upload file to public store space
    procedure PostFileToPublic(fileName: SystemString; StartPos: Int64); overload;
    // upload file to private store space
    procedure PostFileToPrivate(fileName, DirectoryName: SystemString); overload;
    procedure PostFileToPrivate(fileName: SystemString); overload;
    // restore upload file to private store space
    procedure PostFileToPrivate(fileName, DirectoryName: SystemString; StartPos: Int64); overload;
    procedure PostFileToPrivate(fileName: SystemString; StartPos: Int64); overload;
    // upload stream to private store space
    procedure PostStreamToPrivate(RemoteFileName, DirectoryName: SystemString; stream: TCoreClassStream; doneFreeStream: Boolean); overload;
    // restore upload stream to private store space
    procedure PostStreamToPrivate(RemoteFileName, DirectoryName: SystemString; stream: TCoreClassStream; StartPos: Int64; doneFreeStream: Boolean); overload;

    // batch stream suppport
    procedure PostBatchStream(stream: TCoreClassStream; doneFreeStream: Boolean); overload;
    procedure PostBatchStream(stream: TCoreClassStream; doneFreeStream: Boolean; OnCompletedBackcall: TStateCall); overload;
    procedure PostBatchStream(stream: TCoreClassStream; doneFreeStream: Boolean; OnCompletedBackcall: TStateMethod); overload;
{$IFNDEF FPC} procedure PostBatchStream(stream: TCoreClassStream; doneFreeStream: Boolean; OnCompletedBackcall: TStateProc); overload; {$ENDIF}
    procedure ClearBatchStream;
    procedure GetBatchStreamState(OnResult: TStreamMethod); overload;
{$IFNDEF FPC} procedure GetBatchStreamState(OnResult: TStreamProc); overload; {$ENDIF}
    function GetBatchStreamState(ResultData: TDataFrameEngine; ATimeOut: TTimeTickValue): Boolean; overload;

    procedure RegisterCommand; virtual;
    procedure UnRegisterCommand; virtual;

    property LinkOk: Boolean read FLinkOk;
    property WaitCommandTimeout: Cardinal read FWaitCommandTimeout write FWaitCommandTimeout;
    property RecvFileing: Boolean read FRecvFileing;
    property RecvFileOfBatching: Boolean read FRecvFileOfBatching;
    property RecvFileName: SystemString read FRecvFileName;

    property CadencerEngine: TCadencer read FCadencerEngine;
    property ServerDelay: Double read FServerDelay;
    property ProgressEngine: TNProgressPost read FProgressEngine;

    property RecvTunnel: TCommunicationFrameworkClient read FRecvTunnel;
    property SendTunnel: TCommunicationFrameworkClient read FSendTunnel;

    function RemoteInited: Boolean;
  end;

implementation

uses SysUtils;

type
  PPostBatchBackcallData = ^TPostBatchBackcallData;

  TPostBatchBackcallData = packed record
    OnCall: TStateCall;
    OnMethod: TStateMethod;
{$IFNDEF FPC} OnProc: TStateProc; {$ENDIF}
    procedure Init;
  end;

  PGetFileInfoStruct = ^TGetFileInfoStruct;

  TGetFileInfoStruct = packed record
    UserData: Pointer;
    UserObject: TCoreClassObject;
    fileName: SystemString;
    OnCompleteCall: TGetFileInfoCall;
    OnCompleteMethod: TGetFileInfoMethod;
{$IFNDEF FPC} OnCompleteProc: TGetFileInfoProc; {$ENDIF}
    procedure Init;
  end;

  PFileMD5Struct = ^TFileMD5Struct;

  TFileMD5Struct = packed record
    UserData: Pointer;
    UserObject: TCoreClassObject;
    fileName: SystemString;
    StartPos, EndPos: Int64;
    OnCompleteCall: TFileMD5Call;
    OnCompleteMethod: TFileMD5Method;
{$IFNDEF FPC} OnCompleteProc: TFileMD5Proc; {$ENDIF}
    procedure Init;
  end;

  PRemoteFileBackcall = ^TRemoteFileBackcall;

  TRemoteFileBackcall = packed record
    UserData: Pointer;
    UserObject: TCoreClassObject;
    OnCompleteCall: TFileCompleteCall;
    OnCompleteMethod: TFileCompleteMethod;
{$IFNDEF FPC} OnCompleteProc: TFileCompleteProc; {$ENDIF}
    procedure Init;
  end;

procedure TPostBatchBackcallData.Init;
begin
  OnCall := nil;
  OnMethod := nil;
{$IFNDEF FPC}
  OnProc := nil;
{$ENDIF}
end;

procedure TGetFileInfoStruct.Init;
begin
  UserData := nil;
  UserObject := nil;
  fileName := '';
  OnCompleteCall := nil;
  OnCompleteMethod := nil;
{$IFNDEF FPC} OnCompleteProc := nil; {$ENDIF}
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
{$IFNDEF FPC} OnCompleteProc := nil; {$ENDIF}
end;

procedure TRemoteFileBackcall.Init;
begin
  UserData := nil;
  UserObject := nil;
  OnCompleteCall := nil;
  OnCompleteMethod := nil;
{$IFNDEF FPC} OnCompleteProc := nil; {$ENDIF}
end;

constructor TPeerClientUserDefineForSendTunnel.Create(AOwner: TPeerIO);
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

constructor TPeerClientUserDefineForRecvTunnel.Create(AOwner: TPeerIO);
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
end;

procedure TCommunicationFramework_DoubleTunnelService.UserCreateDirectorySuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel; dn: SystemString);
begin
end;

procedure TCommunicationFramework_DoubleTunnelService.UserPostFileSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel; fn: SystemString);
begin
end;

procedure TCommunicationFramework_DoubleTunnelService.UserOut(UserDefineIO: TPeerClientUserDefineForRecvTunnel);
begin
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
    if not ComparePassword(TCipherStyle.csDES64, UserPasswd, SystemString(FUserDB.GetDefaultValue(UserID, 'password', ''))) then
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
  UserDefineIO.UserConfigFile.Hit['UserInfo', 'Password'] := SystemString(FUserDB.GetDefaultValue(UserID, 'password', ''));

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

  UserDefineIO.SendTunnel := FSendTunnel.ClientFromID[SendID].UserDefine as TPeerClientUserDefineForSendTunnel;
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
    if not ComparePassword(TCipherStyle.csDES64, oldPasswd, SystemString(FUserDB.GetDefaultValue(UserDefineIO.UserID, 'password', ''))) then
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
            stream := TCoreClassFileStream.Create(fn, fmOpenRead or fmShareDenyWrite);
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
  MD5: UnicodeMixedLib.TMD5;
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
      fs := TCoreClassFileStream.Create(fullfn, fmOpenRead or fmShareDenyWrite);
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
  MD5: UnicodeMixedLib.TMD5;
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
      fs := TCoreClassFileStream.Create(fullfn, fmOpenRead or fmShareDenyWrite);
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
  MD5: UnicodeMixedLib.TMD5;
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
      fs := TCoreClassFileStream.Create(fullfn, fmOpenRead or fmShareDenyWrite);
  except
      Exit;
  end;

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteString(fileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteInt64(fs.Size);
  sendDE.WriteString(remoteinfo);
  UserDefineIO.SendTunnel.Owner.SendDirectStreamCmd('FileInfo', sendDE);
  DisposeObject(sendDE);

  MD5 := umlStreamMD5(fs);

  fs.Position := 0;
  UserDefineIO.SendTunnel.Owner.SendBigStream('PostFile', fs, StartPos, True);

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteMD5(MD5);
  sendDE.WritePointer(RemoteBackcallAddr);
  UserDefineIO.SendTunnel.Owner.SendDirectStreamCmd('PostFileOver', sendDE);
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
  MD5: UnicodeMixedLib.TMD5;
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
      fs := TCoreClassFileStream.Create(fullfn, fmOpenRead or fmShareDenyWrite);
  except
      Exit;
  end;

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteString(fileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteInt64(fs.Size);
  sendDE.WriteString(remoteinfo);
  UserDefineIO.SendTunnel.Owner.SendDirectStreamCmd('FileInfo', sendDE);
  DisposeObject(sendDE);

  MD5 := umlStreamMD5(fs);

  fs.Position := 0;
  UserDefineIO.SendTunnel.Owner.SendBigStream('PostFile', fs, StartPos, True);

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteMD5(MD5);
  sendDE.WritePointer(RemoteBackcallAddr);
  UserDefineIO.SendTunnel.Owner.SendDirectStreamCmd('PostFileOver', sendDE);
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
  MD5: UnicodeMixedLib.TMD5;
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
      fs := TCoreClassFileStream.Create(fullfn, fmOpenRead or fmShareDenyWrite);
  except
      Exit;
  end;

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteString(fileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteInt64(fs.Size);
  sendDE.WriteString(remoteinfo);
  UserDefineIO.SendTunnel.Owner.SendDirectStreamCmd('FileInfo', sendDE);
  DisposeObject(sendDE);

  MD5 := umlStreamMD5(fs);

  fs.Position := 0;
  UserDefineIO.SendTunnel.Owner.SendBigStream('PostFile', fs, StartPos, True);

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteMD5(MD5);
  sendDE.WritePointer(RemoteBackcallAddr);
  UserDefineIO.SendTunnel.Owner.SendDirectStreamCmd('PostFileOver', sendDE);
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

procedure TCommunicationFramework_DoubleTunnelService.Command_PostFileOver(Sender: TPeerIO; InData: TDataFrameEngine);
var
  UserDefineIO: TPeerClientUserDefineForRecvTunnel;
  ClientMD5, MD5: UnicodeMixedLib.TMD5;
  fn: SystemString;
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
              RT.SendTunnel.Owner.SendDirectStreamCmd('PostBatchStreamDone', de);
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
  rMD5, sMD5: UnicodeMixedLib.TMD5;
  backCallVal: UInt64;
  backCallValPtr: PPostBatchBackcallData;
  MD5Verify: Boolean;
begin
  RT := GetUserDefineRecvTunnel(Sender);
  if not RT.LinkOk then
      Exit;

  rMD5 := InData.Reader.ReadMD5;
  sMD5 := InData.Reader.ReadMD5;
  backCallVal := InData.Reader.ReadPointer;

  backCallValPtr := PPostBatchBackcallData(Pointer(backCallVal));
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

{$IFNDEF FPC}
  try
    if Assigned(backCallValPtr^.OnProc) then
        backCallValPtr^.OnProc(MD5Verify);
  except
  end;
{$ENDIF}
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
  FCadencerEngine.OnProgress := {$IFDEF FPC}@{$ENDIF FPC}CadencerProgress;
  FProgressEngine := TNProgressPost.Create;

  SwitchAsDefaultPerformance;
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

procedure TCommunicationFramework_DoubleTunnelService.SwitchAsMaxSafe;
begin
  FRecvTunnel.SwitchMaxSafe;
  FSendTunnel.SwitchMaxSafe;
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
  IDPool: TClientIDPool;
  pcid: Cardinal;
  cli: TPeerClientUserDefineForRecvTunnel;
begin
  if umlFileExists(umlCombineFileName(FRootPath, 'UserDB')) then
    begin
      FUserDB.LoadFromFile(umlCombineFileName(FRootPath, 'UserDB'));

      FRecvTunnel.GetIO_IDArray(IDPool);
      for pcid in IDPool do
        begin
          cli := GetUserDefineRecvTunnel(FRecvTunnel.ClientFromID[pcid]);
          if (cli <> nil) and (cli.LoginSuccessed) then
              cli.UserDBIntf := FUserDB.VariantList[cli.UserID];
        end;
    end;
end;

procedure TCommunicationFramework_DoubleTunnelService.SaveUserDB;
var
  IDPool: TClientIDPool;
  pcid: Cardinal;
  cli: TPeerClientUserDefineForRecvTunnel;
begin
  FUserDB.SaveToFile(umlCombineFileName(FRootPath, 'UserDB'));

  FRecvTunnel.GetIO_IDArray(IDPool);
  for pcid in IDPool do
    begin
      cli := GetUserDefineRecvTunnel(FRecvTunnel.ClientFromID[pcid]);
      if (cli <> nil) and (cli.LoginSuccessed) then
          cli.UserDBIntf := FUserDB.VariantList[cli.UserID];
    end;
end;

function TCommunicationFramework_DoubleTunnelService.RegUser(AUserID, APasswd: SystemString; AUserConfigFile: TSectionTextData): Boolean;
var
  AUserFlag, AUserPath: SystemString;
  AUserDBIntf: THashVariantList;
  te: TSectionTextData;
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
      AUserConfigFile.SaveToFile(umlCombineFileName(AUserPath, 'User.Config'));
    end
  else
    begin
      te := TSectionTextData.Create;
      te.Hit['UserInfo', 'UserID'] := AUserID;
      te.Hit['UserInfo', 'Password'] := AUserDBIntf['password'];
      te.SaveToFile(umlCombineFileName(AUserPath, 'User.Config'));
      DisposeObject(te);
    end;

  if FCanSaveUserInfo then
      SaveUserDB;

  UserRegistedSuccess(AUserID);
  Result := True;
end;

function TCommunicationFramework_DoubleTunnelService.ExistsUser(AUserID: SystemString): Boolean;
begin
  Result := FUserDB.Exists(AUserID);
end;

function TCommunicationFramework_DoubleTunnelService.GetUserPath(AUserID: SystemString): SystemString;
var
  AUserFlag: SystemString;
  AUserDBIntf: THashVariantList;
begin
  Result := '';
  if not ExistsUser(AUserID) then
      Exit;

  AUserDBIntf := FUserDB.VariantList[AUserID];
  AUserFlag := AUserDBIntf.GetDefaultValue('UserFlag', '');
  Result := umlCombinePath(FRootPath, AUserFlag);
end;

function TCommunicationFramework_DoubleTunnelService.GetUserFile(AUserID, AUserFileName: SystemString): SystemString;
begin
  Result := umlCombineFileName(GetUserPath(AUserID), AUserFileName);
end;

function TCommunicationFramework_DoubleTunnelService.GetUserDefineIO(AUserID: SystemString): TPeerClientUserDefineForRecvTunnel;
begin
  Result := TPeerClientUserDefineForRecvTunnel(FLoginUserDefineIOList[AUserID]);
end;

function TCommunicationFramework_DoubleTunnelService.UserOnline(AUserID: SystemString): Boolean;
begin
  Result := GetUserDefineIO(AUserID) <> nil;
end;

function TCommunicationFramework_DoubleTunnelService.PackUserAsFile(AUserID, aPackFile: SystemString): Boolean;
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

function TCommunicationFramework_DoubleTunnelService.PackUserAsStream(AUserID: SystemString; PackStream: TCoreClassStream): Boolean;
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

  Result := True;
end;

function TCommunicationFramework_DoubleTunnelService.UnPackFileAsUser(aPackFile: SystemString): Boolean;
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
  M: TMemoryStream64;
  te: TSectionTextData;

  AUserID, APasswd: SystemString;
  AUserDBIntf: THashVariantList;
begin
  aPackStream.Position := 0;
  dbEng := TObjectDataManager.CreateAsStream(aPackStream, '', ObjectDataMarshal.ID, True, False, False);
  M := TMemoryStream64.Create;
  te := TSectionTextData.Create;

  try
    Result := ExtractFileInDB(dbEng, '/', 'User.Config', M);
    if Result then
      begin
        M.Position := 0;
        te.LoadFromStream(M);

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
  FRecvTunnel.RegisterStream('UserLogin').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_UserLogin;
  FRecvTunnel.RegisterStream('RegisterUser').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_RegisterUser;
  FRecvTunnel.RegisterStream('TunnelLink').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_TunnelLink;
  FRecvTunnel.RegisterStream('ChangePasswd').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_ChangePasswd;
  FRecvTunnel.RegisterStream('CustomNewUser').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_CustomNewUser;
  FRecvTunnel.RegisterDirectStream('ProcessStoreQueueCMD').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_ProcessStoreQueueCMD;
  FRecvTunnel.RegisterStream('GetPublicFileList').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_GetPublicFileList;
  FRecvTunnel.RegisterStream('GetPrivateFileList').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_GetPrivateFileList;
  FRecvTunnel.RegisterStream('GetPrivateDirectoryList').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_GetPrivateDirectoryList;
  FRecvTunnel.RegisterStream('CreatePrivateDirectory').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_CreatePrivateDirectory;
  FRecvTunnel.RegisterStream('GetPublicFileInfo').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_GetPublicFileInfo;
  FRecvTunnel.RegisterStream('GetPrivateFileInfo').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_GetPrivateFileInfo;
  FRecvTunnel.RegisterStream('GetPublicFileMD5').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_GetPublicFileMD5;
  FRecvTunnel.RegisterStream('GetPrivateFileMD5').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_GetPrivateFileMD5;
  FRecvTunnel.RegisterStream('GetPublicFile').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_GetPublicFile;
  FRecvTunnel.RegisterStream('GetPrivateFile').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_GetPrivateFile;
  FRecvTunnel.RegisterStream('GetUserPrivateFile').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_GetUserPrivateFile;
  FRecvTunnel.RegisterDirectStream('PostPublicFileInfo').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_PostPublicFileInfo;
  FRecvTunnel.RegisterDirectStream('PostPrivateFileInfo').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_PostPrivateFileInfo;
  FRecvTunnel.RegisterBigStream('PostFile').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_PostFile;
  FRecvTunnel.RegisterDirectStream('PostFileOver').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_PostFileOver;
  FRecvTunnel.RegisterStream('GetCurrentCadencer').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_GetCurrentCadencer;
  FRecvTunnel.RegisterDirectStream('NewBatchStream').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_NewBatchStream;
  FRecvTunnel.RegisterBigStream('PostBatchStream').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_PostBatchStream;
  FRecvTunnel.RegisterDirectStream('ClearBatchStream').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_ClearBatchStream;
  FRecvTunnel.RegisterDirectStream('PostBatchStreamDone').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_PostBatchStreamDone;
  FRecvTunnel.RegisterStream('GetBatchStreamState').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_GetBatchStreamState;
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

  FRecvTunnel.DeleteRegistedCMD('GetPublicFileInfo');
  FRecvTunnel.DeleteRegistedCMD('GetPrivateFileInfo');

  FRecvTunnel.DeleteRegistedCMD('GetPublicFileMD5');
  FRecvTunnel.DeleteRegistedCMD('GetPrivateFileMD5');

  FRecvTunnel.DeleteRegistedCMD('GetPublicFile');
  FRecvTunnel.DeleteRegistedCMD('GetPrivateFile');
  FRecvTunnel.DeleteRegistedCMD('GetUserPrivateFile');

  FRecvTunnel.DeleteRegistedCMD('PostPublicFileInfo');
  FRecvTunnel.DeleteRegistedCMD('PostPrivateFileInfo');
  FRecvTunnel.DeleteRegistedCMD('PostFile');
  FRecvTunnel.DeleteRegistedCMD('PostFileOver');

  FRecvTunnel.DeleteRegistedCMD('GetCurrentCadencer');

  FRecvTunnel.DeleteRegistedCMD('NewBatchStream');
  FRecvTunnel.DeleteRegistedCMD('PostBatchStream');
  FRecvTunnel.DeleteRegistedCMD('ClearBatchStream');
  FRecvTunnel.DeleteRegistedCMD('PostBatchStreamDone');
  FRecvTunnel.DeleteRegistedCMD('GetBatchStreamState');
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
  cli.SendDirectStreamCmd('NewBatchStream', de);
  DisposeObject(de);

  cli.SendBigStream('PostBatchStream', stream, doneFreeStream);
end;

procedure TCommunicationFramework_DoubleTunnelService.PostBatchStream(cli: TPeerIO; stream: TCoreClassStream; doneFreeStream: Boolean; OnCompletedBackcall: TStateCall);
var
  de: TDataFrameEngine;
  p: PPostBatchBackcallData;
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
  cli.SendDirectStreamCmd('NewBatchStream', de);
  DisposeObject(de);

  cli.SendBigStream('PostBatchStream', stream, doneFreeStream);
end;

procedure TCommunicationFramework_DoubleTunnelService.PostBatchStream(cli: TPeerIO; stream: TCoreClassStream; doneFreeStream: Boolean;
  OnCompletedBackcall: TStateMethod);
var
  de: TDataFrameEngine;
  p: PPostBatchBackcallData;
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
  cli.SendDirectStreamCmd('NewBatchStream', de);
  DisposeObject(de);

  cli.SendBigStream('PostBatchStream', stream, doneFreeStream);
end;

{$IFNDEF FPC}


procedure TCommunicationFramework_DoubleTunnelService.PostBatchStream(cli: TPeerIO; stream: TCoreClassStream; doneFreeStream: Boolean; OnCompletedBackcall: TStateProc);
var
  de: TDataFrameEngine;
  p: PPostBatchBackcallData;
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
  cli.SendDirectStreamCmd('NewBatchStream', de);
  DisposeObject(de);

  cli.SendBigStream('PostBatchStream', stream, doneFreeStream);
end;
{$ENDIF}


procedure TCommunicationFramework_DoubleTunnelService.ClearBatchStream(cli: TPeerIO);
var
  de: TDataFrameEngine;
  p: PPostBatchBackcallData;
begin
  de := TDataFrameEngine.Create;
  cli.SendDirectStreamCmd('ClearBatchStream', de);
  DisposeObject(de);
end;

procedure TCommunicationFramework_DoubleTunnelService.GetBatchStreamState(cli: TPeerIO; OnResult: TStreamMethod);
var
  de: TDataFrameEngine;
  p: PPostBatchBackcallData;
begin
  de := TDataFrameEngine.Create;
  cli.SendStreamCmd('GetBatchStreamState', de, OnResult);
  DisposeObject(de);
end;

{$IFNDEF FPC}


procedure TCommunicationFramework_DoubleTunnelService.GetBatchStreamState(cli: TPeerIO; OnResult: TStreamProc);
var
  de: TDataFrameEngine;
  p: PPostBatchBackcallData;
begin
  de := TDataFrameEngine.Create;
  cli.SendStreamCmd('GetBatchStreamState', de, OnResult);
  DisposeObject(de);
end;
{$ENDIF}


constructor TClientUserDefineForRecvTunnel.Create(AOwner: TPeerIO);
begin
  inherited Create(AOwner);
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

constructor TClientUserDefineForSendTunnel.Create(AOwner: TPeerIO);
begin
  inherited Create(AOwner);
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
      FRecvTunnel.ClientIO.Disconnect;
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
  servMD5, MD5: UnicodeMixedLib.TMD5;
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
{$IFNDEF FPC}
            if Assigned(p^.OnCompleteProc) then
              begin
                FCurrentStream.Position := 0;
                p^.OnCompleteProc(p^.UserData, p^.UserObject, FCurrentStream, fn);
              end;
{$ENDIF}
            Dispose(p);
          end;
      except
      end;

      DisposeObject(FCurrentStream);
      FCurrentStream := nil;
    end;

  FRecvFileing := False;
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
{$IFNDEF FPC}
      if Assigned(p^.OnCompleteProc) then
          p^.OnCompleteProc(p^.UserData, p^.UserObject, p^.fileName, Existed, fSiz);
{$ENDIF FPC}
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
{$IFNDEF FPC}
      if Assigned(p^.OnCompleteProc) then
          p^.OnCompleteProc(p^.UserData, p^.UserObject, p^.fileName, Existed, fSiz);
{$ENDIF FPC}
      p^.fileName := '';
      Dispose(p);
    end;
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPublicFileMD5_StreamParamResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, ResultData: TDataFrameEngine);
var
  p: PFileMD5Struct;
  successed: Boolean;
  MD5: UnicodeMixedLib.TMD5;
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
{$IFNDEF FPC}
      if Assigned(p^.OnCompleteProc) then
          p^.OnCompleteProc(p^.UserData, p^.UserObject, p^.fileName, p^.StartPos, p^.EndPos, MD5);
{$ENDIF FPC}
      p^.fileName := '';
      Dispose(p);
    end;
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPrivateFileMD5_StreamParamResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, ResultData: TDataFrameEngine);
var
  p: PFileMD5Struct;
  successed: Boolean;
  MD5: UnicodeMixedLib.TMD5;
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
{$IFNDEF FPC}
      if Assigned(p^.OnCompleteProc) then
          p^.OnCompleteProc(p^.UserData, p^.UserObject, p^.fileName, p^.StartPos, p^.EndPos, MD5);
{$ENDIF FPC}
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
              SendTunnel.SendDirectStreamCmd('PostBatchStreamDone', de);
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
  rMD5, sMD5: UnicodeMixedLib.TMD5;
  backCallVal: UInt64;
  backCallValPtr: PPostBatchBackcallData;
  MD5Verify: Boolean;
begin
  if not LinkOk then
      Exit;
  RT := Sender.UserDefine as TClientUserDefineForRecvTunnel;

  rMD5 := InData.Reader.ReadMD5;
  sMD5 := InData.Reader.ReadMD5;
  backCallVal := InData.Reader.ReadPointer;

  backCallValPtr := PPostBatchBackcallData(Pointer(backCallVal));
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

{$IFNDEF FPC}
  try
    if Assigned(backCallValPtr^.OnProc) then
        backCallValPtr^.OnProc(MD5Verify);
  except
  end;
{$ENDIF}
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
{$IFNDEF FPC}
        if Assigned(FAsyncOnResultProc) then
            FAsyncOnResultProc(False);
{$ENDIF}
      except
      end;
      FAsyncConnectAddr := '';
      FAsyncConnRecvPort := 0;
      FAsyncConnSendPort := 0;
      FAsyncOnResultCall := nil;
      FAsyncOnResultMethod := nil;
{$IFNDEF FPC}
      FAsyncOnResultProc := nil;
{$ENDIF}
      Exit;
    end;

  RecvTunnel.AsyncConnect(FAsyncConnectAddr, FAsyncConnRecvPort, {$IFDEF FPC}@{$ENDIF FPC}AsyncRecvConnectResult);
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
{$IFNDEF FPC}
    if Assigned(FAsyncOnResultProc) then
        FAsyncOnResultProc(cState);
{$ENDIF}
  except
  end;

  FAsyncConnectAddr := '';
  FAsyncConnRecvPort := 0;
  FAsyncConnSendPort := 0;
  FAsyncOnResultCall := nil;
  FAsyncOnResultMethod := nil;
{$IFNDEF FPC}
  FAsyncOnResultProc := nil;
{$ENDIF}
end;

constructor TCommunicationFramework_DoubleTunnelClient.Create(ARecvTunnel, ASendTunnel: TCommunicationFrameworkClient);
begin
  inherited Create;
  FRecvTunnel := ARecvTunnel;
  FRecvTunnel.NotyifyInterface := Self;
  FRecvTunnel.PeerClientUserDefineClass := TClientUserDefineForRecvTunnel;

  FSendTunnel := ASendTunnel;
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
{$IFNDEF FPC}
  FAsyncOnResultProc := nil;
{$ENDIF}
  //
  SwitchAsDefaultPerformance;
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

procedure TCommunicationFramework_DoubleTunnelClient.SwitchAsMaxSafe;
begin
  FRecvTunnel.SwitchMaxSafe;
  FSendTunnel.SwitchMaxSafe;
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

procedure TCommunicationFramework_DoubleTunnelClient.AsyncConnect(addr: SystemString; const RecvPort, SendPort: Word; OnResult: TStateCall);
begin
  Disconnect;
  FAsyncConnectAddr := addr;
  FAsyncConnRecvPort := RecvPort;
  FAsyncConnSendPort := SendPort;
  FAsyncOnResultCall := OnResult;
  FAsyncOnResultMethod := nil;
{$IFNDEF FPC}
  FAsyncOnResultProc := nil;
{$ENDIF}
  SendTunnel.AsyncConnect(FAsyncConnectAddr, FAsyncConnSendPort, {$IFDEF FPC}@{$ENDIF FPC}AsyncSendConnectResult);
end;

procedure TCommunicationFramework_DoubleTunnelClient.AsyncConnect(addr: SystemString; const RecvPort, SendPort: Word; OnResult: TStateMethod);
begin
  Disconnect;
  FAsyncConnectAddr := addr;
  FAsyncConnRecvPort := RecvPort;
  FAsyncConnSendPort := SendPort;
  FAsyncOnResultCall := nil;
  FAsyncOnResultMethod := OnResult;
{$IFNDEF FPC}
  FAsyncOnResultProc := nil;
{$ENDIF}
  SendTunnel.AsyncConnect(FAsyncConnectAddr, FAsyncConnSendPort, {$IFDEF FPC}@{$ENDIF FPC}AsyncSendConnectResult);
end;

{$IFNDEF FPC}


procedure TCommunicationFramework_DoubleTunnelClient.AsyncConnect(addr: SystemString; const RecvPort, SendPort: Word; OnResult: TStateProc);
begin
  Disconnect;
  FAsyncConnectAddr := addr;
  FAsyncConnRecvPort := RecvPort;
  FAsyncConnSendPort := SendPort;
  FAsyncOnResultCall := nil;
  FAsyncOnResultMethod := nil;
  FAsyncOnResultProc := OnResult;

  SendTunnel.AsyncConnect(FAsyncConnectAddr, FAsyncConnSendPort, AsyncSendConnectResult);
end;
{$ENDIF}


procedure TCommunicationFramework_DoubleTunnelClient.Disconnect;
begin
  if FSendTunnel.ClientIO <> nil then
      FSendTunnel.ClientIO.Disconnect;

  if FRecvTunnel.ClientIO <> nil then
      FRecvTunnel.ClientIO.Disconnect;

  FAsyncConnectAddr := '';
  FAsyncConnRecvPort := 0;
  FAsyncConnSendPort := 0;
  FAsyncOnResultCall := nil;
  FAsyncOnResultMethod := nil;
{$IFNDEF FPC}
  FAsyncOnResultProc := nil;
{$ENDIF}
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
  FSendTunnel.WaitSendStreamCmd('UserLogin', sendDE, resDE, FWaitCommandTimeout * 2);

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

{$IFNDEF FPC}


procedure TCommunicationFramework_DoubleTunnelClient.UserLogin(UserID, passwd: SystemString; OnProc: TStateProc);
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
  sendDE.WriteString(passwd);
  FSendTunnel.SendStreamCmd('UserLogin', sendDE,
    procedure(Sender: TPeerIO; ResultData: TDataFrameEngine)
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

procedure TCommunicationFramework_DoubleTunnelClient.RegisterUser(UserID, passwd: SystemString; OnProc: TStateProc);
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
  sendDE.WriteString(passwd);
  FSendTunnel.SendStreamCmd('RegisterUser', sendDE,
    procedure(Sender: TPeerIO; ResultData: TDataFrameEngine)
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
    procedure(Sender: TPeerIO; ResultData: TDataFrameEngine)
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
              TClientUserDefineForSendTunnel(FSendTunnel.ClientIO.UserDefine).Client := Self;
              TClientUserDefineForSendTunnel(FSendTunnel.ClientIO.UserDefine).RecvTunnel := TClientUserDefineForRecvTunnel(FRecvTunnel.ClientIO.UserDefine);

              TClientUserDefineForRecvTunnel(FRecvTunnel.ClientIO.UserDefine).Client := Self;
              TClientUserDefineForRecvTunnel(FRecvTunnel.ClientIO.UserDefine).SendTunnel := TClientUserDefineForSendTunnel(FSendTunnel.ClientIO.UserDefine);
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
  FSendTunnel.SendStreamCmd('GetCurrentCadencer', sendDE, {$IFDEF FPC}@{$ENDIF FPC}GetCurrentCadencer_StreamResult);
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

  FSendTunnel.WaitSendStreamCmd('ChangePasswd', sendDE, resDE, FWaitCommandTimeout);

  if resDE.Count = 2 then
      Result := resDE.ReadBool(0);

  DisposeObject(sendDE);
  DisposeObject(resDE);
end;

function TCommunicationFramework_DoubleTunnelClient.CustomNewUser(AUserID, APasswd: SystemString; AUserConfigFile: TSectionTextData): Boolean;
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

  FSendTunnel.WaitSendStreamCmd('GetPublicFileList', sendDE, resDE, FWaitCommandTimeout);

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

  FSendTunnel.WaitSendStreamCmd('GetPrivateFileList', sendDE, resDE, FWaitCommandTimeout);

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

  FSendTunnel.WaitSendStreamCmd('GetPrivateDirectoryList', sendDE, resDE, FWaitCommandTimeout);

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

  FSendTunnel.WaitSendStreamCmd('CreatePrivateDirectory', sendDE, resDE, FWaitCommandTimeout);

  if resDE.Count > 0 then
    begin
      Result := resDE.Reader.ReadBool;
      FSendTunnel.ClientIO.Print(resDE.Reader.ReadString);
    end;

  DisposeObject(sendDE);
  DisposeObject(resDE);
end;

// remote file exists
procedure TCommunicationFramework_DoubleTunnelClient.GetPublicFileInfo(fileName: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TGetFileInfoCall);
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

  FSendTunnel.SendStreamCmd('GetPublicFileInfo', sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetPublicFileInfo_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPublicFileInfo(fileName: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TGetFileInfoMethod);
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

  FSendTunnel.SendStreamCmd('GetPublicFileInfo', sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetPublicFileInfo_StreamParamResult);
  DisposeObject(sendDE);
end;

{$IFNDEF FPC}


procedure TCommunicationFramework_DoubleTunnelClient.GetPublicFileInfo(fileName: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TGetFileInfoProc);
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

  FSendTunnel.SendStreamCmd('GetPublicFileInfo', sendDE, p, nil, GetPublicFileInfo_StreamParamResult);
  DisposeObject(sendDE);
end;
{$ENDIF FPC}


procedure TCommunicationFramework_DoubleTunnelClient.GetPrivateFileInfo(fileName, DirectoryName: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TGetFileInfoCall);
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

  FSendTunnel.SendStreamCmd('GetPrivateFileInfo', sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetPrivateFileInfo_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPrivateFileInfo(fileName, DirectoryName: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TGetFileInfoMethod);
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

  FSendTunnel.SendStreamCmd('GetPrivateFileInfo', sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetPrivateFileInfo_StreamParamResult);
  DisposeObject(sendDE);
end;

{$IFNDEF FPC}


procedure TCommunicationFramework_DoubleTunnelClient.GetPrivateFileInfo(fileName, DirectoryName: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TGetFileInfoProc);
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

  FSendTunnel.SendStreamCmd('GetPrivateFileInfo', sendDE, p, nil, GetPrivateFileInfo_StreamParamResult);
  DisposeObject(sendDE);
end;
{$ENDIF FPC}


//
// remote md5 support with public store space
procedure TCommunicationFramework_DoubleTunnelClient.GetPublicFileMD5(fileName: SystemString; const StartPos, EndPos: Int64;
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

  FSendTunnel.SendStreamCmd('GetPublicFileMD5', sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetPublicFileMD5_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPublicFileMD5(fileName: SystemString; const StartPos, EndPos: Int64;
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

  FSendTunnel.SendStreamCmd('GetPublicFileMD5', sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetPublicFileMD5_StreamParamResult);
  DisposeObject(sendDE);
end;

{$IFNDEF FPC} procedure TCommunicationFramework_DoubleTunnelClient.GetPublicFileMD5(fileName: SystemString; const StartPos, EndPos: Int64;
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

  FSendTunnel.SendStreamCmd('GetPublicFileMD5', sendDE, p, nil, GetPublicFileMD5_StreamParamResult);
  DisposeObject(sendDE);
end;
{$ENDIF FPC}


// remote md5 support with private store space
procedure TCommunicationFramework_DoubleTunnelClient.GetPrivateFileMD5(fileName, DirectoryName: SystemString; const StartPos, EndPos: Int64;
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

  FSendTunnel.SendStreamCmd('GetPrivateFileMD5', sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetPrivateFileMD5_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPrivateFileMD5(fileName, DirectoryName: SystemString; const StartPos, EndPos: Int64;
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

  FSendTunnel.SendStreamCmd('GetPrivateFileMD5', sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetPrivateFileMD5_StreamParamResult);
  DisposeObject(sendDE);
end;

{$IFNDEF FPC} procedure TCommunicationFramework_DoubleTunnelClient.GetPrivateFileMD5(fileName, DirectoryName: SystemString; const StartPos, EndPos: Int64;
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

  FSendTunnel.SendStreamCmd('GetPrivateFileMD5', sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetPrivateFileMD5_StreamParamResult);
  DisposeObject(sendDE);
end;
{$ENDIF FPC}


function TCommunicationFramework_DoubleTunnelClient.GetPublicFile(fileName, saveToPath: SystemString): Boolean;
begin
  Result := GetPublicFile(fileName, 0, saveToPath);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPublicFile(fileName, saveToPath: SystemString;
const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteCall);
begin
  GetPublicFile(fileName, 0, saveToPath, UserData, UserObject, OnComplete);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPublicFile(fileName, saveToPath: SystemString;
const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteMethod);
begin
  GetPublicFile(fileName, 0, saveToPath, UserData, UserObject, OnComplete);
end;

{$IFNDEF FPC}


procedure TCommunicationFramework_DoubleTunnelClient.GetPublicFile(fileName, saveToPath: SystemString;
const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteProc);
begin
  GetPublicFile(fileName, 0, saveToPath, UserData, UserObject, OnComplete);
end;
{$ENDIF}


// restore download from public
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

procedure TCommunicationFramework_DoubleTunnelClient.GetPublicFile(fileName: SystemString; StartPos: Int64; saveToPath: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteCall);
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

  FSendTunnel.SendStreamCmd('GetPublicFile', sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetPublicFile_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPublicFile(fileName: SystemString; StartPos: Int64; saveToPath: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteMethod);
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

  FSendTunnel.SendStreamCmd('GetPublicFile', sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetPublicFile_StreamParamResult);
  DisposeObject(sendDE);
end;

{$IFNDEF FPC}


procedure TCommunicationFramework_DoubleTunnelClient.GetPublicFile(fileName: SystemString; StartPos: Int64; saveToPath: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteProc);
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

  FSendTunnel.SendStreamCmd('GetPublicFile', sendDE, p, nil, GetPublicFile_StreamParamResult);
  DisposeObject(sendDE);
end;

{$ENDIF}


function TCommunicationFramework_DoubleTunnelClient.GetPrivateFile(fileName, DirectoryName, saveToPath: SystemString): Boolean;
begin
  Result := GetPrivateFile(fileName, 0, DirectoryName, saveToPath);
end;

function TCommunicationFramework_DoubleTunnelClient.GetPrivateFile(fileName, saveToPath: SystemString): Boolean;
begin
  Result := GetPrivateFile(fileName, '', saveToPath);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPrivateFile(fileName, DirectoryName, saveToPath: SystemString;
const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteCall);
begin
  GetPrivateFile(fileName, 0, DirectoryName, saveToPath, UserData, UserObject, OnComplete);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPrivateFile(fileName, DirectoryName, saveToPath: SystemString;
const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteMethod);
begin
  GetPrivateFile(fileName, 0, DirectoryName, saveToPath, UserData, UserObject, OnComplete);
end;

{$IFNDEF FPC}


procedure TCommunicationFramework_DoubleTunnelClient.GetPrivateFile(fileName, DirectoryName, saveToPath: SystemString;
const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteProc);
begin
  GetPrivateFile(fileName, 0, DirectoryName, saveToPath, UserData, UserObject, OnComplete);
end;
{$ENDIF}


// restore download from user
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

function TCommunicationFramework_DoubleTunnelClient.GetPrivateFile(fileName: SystemString; StartPos: Int64; saveToPath: SystemString): Boolean;
begin
  Result := GetPrivateFile(fileName, StartPos, '', saveToPath);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPrivateFile(fileName: SystemString; StartPos: Int64; DirectoryName, saveToPath: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteCall);
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

  FSendTunnel.SendStreamCmd('GetPrivateFile', sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetPrivateFile_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetPrivateFile(fileName: SystemString; StartPos: Int64; DirectoryName, saveToPath: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteMethod);
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

  FSendTunnel.SendStreamCmd('GetPrivateFile', sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetPrivateFile_StreamParamResult);
  DisposeObject(sendDE);
end;

{$IFNDEF FPC}


procedure TCommunicationFramework_DoubleTunnelClient.GetPrivateFile(fileName: SystemString; StartPos: Int64; DirectoryName, saveToPath: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteProc);
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

  FSendTunnel.SendStreamCmd('GetPrivateFile', sendDE, p, nil, GetPrivateFile_StreamParamResult);
  DisposeObject(sendDE);
end;

{$ENDIF}


function TCommunicationFramework_DoubleTunnelClient.GetUserPrivateFile(UserID, fileName, DirectoryName, saveToPath: SystemString): Boolean;
begin
  Result := GetUserPrivateFile(UserID, fileName, 0, DirectoryName, saveToPath);
end;

function TCommunicationFramework_DoubleTunnelClient.GetUserPrivateFile(UserID, fileName, saveToPath: SystemString): Boolean;
begin
  Result := GetUserPrivateFile(UserID, fileName, '', saveToPath);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetUserPrivateFile(UserID, fileName, DirectoryName, saveToPath: SystemString;
const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteCall);
begin
  GetUserPrivateFile(UserID, fileName, 0, DirectoryName, saveToPath, UserData, UserObject, OnComplete);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetUserPrivateFile(UserID, fileName, DirectoryName, saveToPath: SystemString;
const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteMethod);
begin
  GetUserPrivateFile(UserID, fileName, 0, DirectoryName, saveToPath, UserData, UserObject, OnComplete);
end;

{$IFNDEF FPC}


procedure TCommunicationFramework_DoubleTunnelClient.GetUserPrivateFile(UserID, fileName, DirectoryName, saveToPath: SystemString;
const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileCompleteProc);
begin
  GetUserPrivateFile(UserID, fileName, 0, DirectoryName, saveToPath, UserData, UserObject, OnComplete);
end;
{$ENDIF}


// restore download with custom user
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

function TCommunicationFramework_DoubleTunnelClient.GetUserPrivateFile(UserID, fileName: SystemString; StartPos: Int64; saveToPath: SystemString): Boolean;
begin
  Result := GetUserPrivateFile(UserID, fileName, StartPos, '', saveToPath);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetUserPrivateFile(UserID, fileName: SystemString; StartPos: Int64; DirectoryName, saveToPath: SystemString;
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

  FSendTunnel.SendStreamCmd('GetUserPrivateFile', sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetPrivateFile_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetUserPrivateFile(UserID, fileName: SystemString; StartPos: Int64; DirectoryName, saveToPath: SystemString;
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

  FSendTunnel.SendStreamCmd('GetUserPrivateFile', sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetPrivateFile_StreamParamResult);
  DisposeObject(sendDE);
end;

{$IFNDEF FPC} procedure TCommunicationFramework_DoubleTunnelClient.GetUserPrivateFile(UserID, fileName: SystemString; StartPos: Int64; DirectoryName, saveToPath: SystemString;
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

  FSendTunnel.SendStreamCmd('GetUserPrivateFile', sendDE, p, nil, GetPrivateFile_StreamParamResult);
  DisposeObject(sendDE);
end;
{$ENDIF}


procedure TCommunicationFramework_DoubleTunnelClient.PostFileToPublic(fileName: SystemString);
begin
  PostFileToPublic(fileName, 0);
end;

procedure TCommunicationFramework_DoubleTunnelClient.PostFileToPublic(fileName: SystemString; StartPos: Int64);
var
  sendDE: TDataFrameEngine;
  fs: TCoreClassFileStream;
  MD5: UnicodeMixedLib.TMD5;
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
  sendDE.WriteInt64(StartPos);
  sendDE.WriteInt64(fs.Size);
  FSendTunnel.SendDirectStreamCmd('PostPublicFileInfo', sendDE);
  DisposeObject(sendDE);

  MD5 := umlStreamMD5(fs);

  FSendTunnel.SendBigStream('PostFile', fs, StartPos, True);

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteMD5(MD5);
  FSendTunnel.SendDirectStreamCmd('PostFileOver', sendDE);
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
  MD5: UnicodeMixedLib.TMD5;
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
  sendDE.WriteString(DirectoryName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteInt64(fs.Size);
  FSendTunnel.SendDirectStreamCmd('PostPrivateFileInfo', sendDE);
  DisposeObject(sendDE);

  MD5 := umlStreamMD5(fs);

  FSendTunnel.SendBigStream('PostFile', fs, StartPos, True);

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteMD5(MD5);
  FSendTunnel.SendDirectStreamCmd('PostFileOver', sendDE);
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
  MD5: UnicodeMixedLib.TMD5;
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
  FSendTunnel.SendDirectStreamCmd('PostPrivateFileInfo', sendDE);
  DisposeObject(sendDE);

  MD5 := umlStreamMD5(stream);

  FSendTunnel.SendBigStream('PostFile', stream, StartPos, doneFreeStream);

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteMD5(MD5);
  FSendTunnel.SendDirectStreamCmd('PostFileOver', sendDE);
  DisposeObject(sendDE);
end;

procedure TCommunicationFramework_DoubleTunnelClient.PostBatchStream(stream: TCoreClassStream; doneFreeStream: Boolean);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;

  de.WriteMD5(umlStreamMD5(stream));
  de.WritePointer(0);
  SendTunnel.SendDirectStreamCmd('NewBatchStream', de);
  DisposeObject(de);

  SendTunnel.SendBigStream('PostBatchStream', stream, doneFreeStream);
end;

procedure TCommunicationFramework_DoubleTunnelClient.PostBatchStream(stream: TCoreClassStream; doneFreeStream: Boolean; OnCompletedBackcall: TStateCall);
var
  de: TDataFrameEngine;
  p: PPostBatchBackcallData;
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
  SendTunnel.SendDirectStreamCmd('NewBatchStream', de);
  DisposeObject(de);

  SendTunnel.SendBigStream('PostBatchStream', stream, doneFreeStream);
end;

procedure TCommunicationFramework_DoubleTunnelClient.PostBatchStream(stream: TCoreClassStream; doneFreeStream: Boolean; OnCompletedBackcall: TStateMethod);
var
  de: TDataFrameEngine;
  p: PPostBatchBackcallData;
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
  SendTunnel.SendDirectStreamCmd('NewBatchStream', de);
  DisposeObject(de);

  SendTunnel.SendBigStream('PostBatchStream', stream, doneFreeStream);
end;

{$IFNDEF FPC}


procedure TCommunicationFramework_DoubleTunnelClient.PostBatchStream(stream: TCoreClassStream; doneFreeStream: Boolean; OnCompletedBackcall: TStateProc);
var
  de: TDataFrameEngine;
  p: PPostBatchBackcallData;
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
  SendTunnel.SendDirectStreamCmd('NewBatchStream', de);
  DisposeObject(de);

  SendTunnel.SendBigStream('PostBatchStream', stream, doneFreeStream);
end;
{$ENDIF}


procedure TCommunicationFramework_DoubleTunnelClient.ClearBatchStream;
var
  de: TDataFrameEngine;
  p: PPostBatchBackcallData;
begin
  de := TDataFrameEngine.Create;
  SendTunnel.SendDirectStreamCmd('ClearBatchStream', de);
  DisposeObject(de);
end;

procedure TCommunicationFramework_DoubleTunnelClient.GetBatchStreamState(OnResult: TStreamMethod);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  SendTunnel.SendStreamCmd('GetBatchStreamState', de, OnResult);
  DisposeObject(de);
end;

{$IFNDEF FPC}


procedure TCommunicationFramework_DoubleTunnelClient.GetBatchStreamState(OnResult: TStreamProc);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  SendTunnel.SendStreamCmd('GetBatchStreamState', de, OnResult);
  DisposeObject(de);
end;
{$ENDIF}


function TCommunicationFramework_DoubleTunnelClient.GetBatchStreamState(ResultData: TDataFrameEngine; ATimeOut: TTimeTickValue): Boolean;
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  SendTunnel.WaitSendStreamCmd('GetBatchStreamState', de, ResultData, ATimeOut);
  Result := ResultData.Count > 0;
  DisposeObject(de);
end;

procedure TCommunicationFramework_DoubleTunnelClient.RegisterCommand;
begin
  FRecvTunnel.RegisterDirectStream('FileInfo').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_FileInfo;
  FRecvTunnel.RegisterBigStream('PostFile').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_PostFile;
  FRecvTunnel.RegisterDirectStream('PostFileOver').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_PostFileOver;
  FRecvTunnel.RegisterDirectStream('NewBatchStream').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_NewBatchStream;
  FRecvTunnel.RegisterBigStream('PostBatchStream').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_PostBatchStream;
  FRecvTunnel.RegisterDirectStream('ClearBatchStream').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_ClearBatchStream;
  FRecvTunnel.RegisterDirectStream('PostBatchStreamDone').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_PostBatchStreamDone;
  FRecvTunnel.RegisterStream('GetBatchStreamState').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_GetBatchStreamState;
end;

procedure TCommunicationFramework_DoubleTunnelClient.UnRegisterCommand;
begin
  FRecvTunnel.DeleteRegistedCMD('FileInfo');
  FRecvTunnel.DeleteRegistedCMD('PostFile');
  FRecvTunnel.DeleteRegistedCMD('PostFileOver');

  FRecvTunnel.DeleteRegistedCMD('NewBatchStream');
  FRecvTunnel.DeleteRegistedCMD('PostBatchStream');
  FRecvTunnel.DeleteRegistedCMD('ClearBatchStream');
  FRecvTunnel.DeleteRegistedCMD('PostBatchStreamDone');
  FRecvTunnel.DeleteRegistedCMD('GetBatchStreamState');
end;

function TCommunicationFramework_DoubleTunnelClient.RemoteInited: Boolean;
begin
  Result := FSendTunnel.RemoteInited and FRecvTunnel.RemoteInited;
end;

end.
