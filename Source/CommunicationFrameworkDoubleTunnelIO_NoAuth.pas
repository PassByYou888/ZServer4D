{ ****************************************************************************** }
{ * double tunnel IO framework(incl File service)                              * }
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

unit CommunicationFrameworkDoubleTunnelIO_NoAuth;

{$INCLUDE zDefine.inc}

interface

uses
{$IFDEF FPC}
  FPCGenericStructlist,
{$ENDIF FPC}
  CoreClasses,
  ListEngine, UnicodeMixedLib,
  DataFrameEngine, MemoryStream64, CommunicationFramework, PhysicsIO,
  TextDataEngine, DoStatusIO, Cadencer, NotifyObjectBase, PascalStrings;

type
  TDTService_NoAuth = class;
  TPeerClientUserDefineForRecvTunnel_NoAuth = class;
  TDTService_NoAuthClass = class of TDTService_NoAuth;

  TPeerClientUserDefineForSendTunnel_NoAuth = class(TPeerIOUserDefine)
  public
    RecvTunnel: TPeerClientUserDefineForRecvTunnel_NoAuth;
    RecvTunnelID: Cardinal;
    DoubleTunnelService: TDTService_NoAuth;

    constructor Create(Owner_: TPeerIO); override;
    destructor Destroy; override;

    function LinkOk: Boolean;
    property BindOk: Boolean read LinkOk;
  end;

  TPeerClientUserDefineForRecvTunnel_NoAuth = class(TPeerIOUserDefine)
  private
    FCurrentFileStream: TCoreClassStream;
    FCurrentReceiveFileName: SystemString;
  public
    SendTunnel: TPeerClientUserDefineForSendTunnel_NoAuth;
    SendTunnelID: Cardinal;
    DoubleTunnelService: TDTService_NoAuth;

    constructor Create(Owner_: TPeerIO); override;
    destructor Destroy; override;

    function LinkOk: Boolean;
    property BindOk: Boolean read LinkOk;
    property CurrentFileStream: TCoreClassStream read FCurrentFileStream write FCurrentFileStream;
    property CurrentReceiveFileName: SystemString read FCurrentReceiveFileName write FCurrentReceiveFileName;
  end;

  TNoAuth_OnLinkSuccess = procedure(Sender: TDTService_NoAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth) of object;
  TNoAuth_OnUserOut = procedure(Sender: TDTService_NoAuth; UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth) of object;

  TDTService_NoAuth = class(TCoreClassInterfacedObject)
  protected
    FRecvTunnel, FSendTunnel: TCommunicationFrameworkServer;
    FCadencerEngine: TCadencer;
    FProgressEngine: TNProgressPost;
    FFileSystem: Boolean;
    FFileReceiveDirectory: SystemString;
    { event }
    FOnLinkSuccess: TNoAuth_OnLinkSuccess;
    FOnUserOut: TNoAuth_OnUserOut;
  protected
    { virtual event }
    procedure UserLinkSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth); virtual;
    procedure UserOut(UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth); virtual;
    procedure UserPostFileSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth; fn: SystemString); virtual;
  protected
    { registed server command }
    procedure Command_TunnelLink(Sender: TPeerIO; InData, OutData: TDataFrameEngine); virtual;
    procedure Command_GetCurrentCadencer(Sender: TPeerIO; InData, OutData: TDataFrameEngine); virtual;

    procedure Command_GetFileTime(Sender: TPeerIO; InData, OutData: TDataFrameEngine); virtual;
    procedure Command_GetFileInfo(Sender: TPeerIO; InData, OutData: TDataFrameEngine); virtual;
    procedure Command_GetFileMD5(Sender: TPeerIO; InData, OutData: TDataFrameEngine); virtual;
    procedure Command_GetFile(Sender: TPeerIO; InData, OutData: TDataFrameEngine); virtual;
    procedure Command_GetFileAs(Sender: TPeerIO; InData, OutData: TDataFrameEngine); virtual;
    procedure Command_PostFileInfo(Sender: TPeerIO; InData: TDataFrameEngine); virtual;
    procedure Command_PostFile(Sender: TPeerIO; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64); virtual;
    procedure Command_PostFileOver(Sender: TPeerIO; InData: TDataFrameEngine); virtual;
    procedure Command_GetFileFragmentData(Sender: TPeerIO; InData, OutData: TDataFrameEngine); virtual;

    procedure Command_NewBatchStream(Sender: TPeerIO; InData: TDataFrameEngine); virtual;
    procedure Command_PostBatchStream(Sender: TPeerIO; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64); virtual;
    procedure Command_ClearBatchStream(Sender: TPeerIO; InData: TDataFrameEngine); virtual;
    procedure Command_PostBatchStreamDone(Sender: TPeerIO; InData: TDataFrameEngine); virtual;
    procedure Command_GetBatchStreamState(Sender: TPeerIO; InData, OutData: TDataFrameEngine); virtual;
  public
    constructor Create(RecvTunnel_, SendTunnel_: TCommunicationFrameworkServer); virtual;
    destructor Destroy; override;

    procedure SwitchAsMaxPerformance;
    procedure SwitchAsMaxSecurity;
    procedure SwitchAsDefaultPerformance;

    procedure Progress; virtual;
    procedure CadencerProgress(Sender: TObject; const deltaTime, newTime: Double); virtual;

    procedure RegisterCommand; virtual;
    procedure UnRegisterCommand; virtual;

    function GetUserDefineRecvTunnel(RecvCli: TPeerIO): TPeerClientUserDefineForRecvTunnel_NoAuth;

    function TotalLinkCount: Integer;

    procedure PostBatchStream(cli: TPeerIO; stream: TCoreClassStream; doneFreeStream: Boolean); overload;
    procedure PostBatchStreamC(cli: TPeerIO; stream: TCoreClassStream; doneFreeStream: Boolean; OnCompletedBackcall: TStateCall); overload;
    procedure PostBatchStreamM(cli: TPeerIO; stream: TCoreClassStream; doneFreeStream: Boolean; OnCompletedBackcall: TStateMethod); overload;
    procedure PostBatchStreamP(cli: TPeerIO; stream: TCoreClassStream; doneFreeStream: Boolean; OnCompletedBackcall: TStateProc); overload;
    procedure ClearBatchStream(cli: TPeerIO);
    procedure GetBatchStreamStateM(cli: TPeerIO; OnResult: TStreamMethod); overload;
    procedure GetBatchStreamStateM(cli: TPeerIO; Param1: Pointer; Param2: TObject; OnResult: TStreamParamMethod); overload;
    procedure GetBatchStreamStateP(cli: TPeerIO; OnResult: TStreamProc); overload;
    procedure GetBatchStreamStateP(cli: TPeerIO; Param1: Pointer; Param2: TObject; OnResult: TStreamParamProc); overload;

    property CadencerEngine: TCadencer read FCadencerEngine;

    property ProgressEngine: TNProgressPost read FProgressEngine;
    property ProgressPost: TNProgressPost read FProgressEngine;
    property PostProgress: TNProgressPost read FProgressEngine;
    property PostRun: TNProgressPost read FProgressEngine;
    property PostExecute: TNProgressPost read FProgressEngine;

    property FileSystem: Boolean read FFileSystem write FFileSystem;
    property FileReceiveDirectory: SystemString read FFileReceiveDirectory write FFileReceiveDirectory;
    property PublicFileDirectory: SystemString read FFileReceiveDirectory write FFileReceiveDirectory;

    property RecvTunnel: TCommunicationFrameworkServer read FRecvTunnel;
    property SendTunnel: TCommunicationFrameworkServer read FSendTunnel;

    property OnLinkSuccess: TNoAuth_OnLinkSuccess read FOnLinkSuccess write FOnLinkSuccess;
    property OnUserOut: TNoAuth_OnUserOut read FOnUserOut write FOnUserOut;
  end;

  TDTClient_NoAuth = class;
  TClientUserDefineForSendTunnel_NoAuth = class;
  TDTClient_NoAuthClass = class of TDTClient_NoAuth;

  TClientUserDefineForRecvTunnel_NoAuth = class(TPeerIOUserDefine)
  public
    Client: TDTClient_NoAuth;
    SendTunnel: TClientUserDefineForSendTunnel_NoAuth;

    constructor Create(Owner_: TPeerIO); override;
    destructor Destroy; override;
  end;

  TClientUserDefineForSendTunnel_NoAuth = class(TPeerIOUserDefine)
  public
    Client: TDTClient_NoAuth;
    RecvTunnel: TClientUserDefineForRecvTunnel_NoAuth;

    constructor Create(Owner_: TPeerIO); override;
    destructor Destroy; override;
  end;

  TGetFileInfoCall_NoAuth = procedure(const UserData: Pointer; const UserObject: TCoreClassObject;
    const fileName: SystemString; const Existed: Boolean; const fSiz: Int64);
  TGetFileInfoMethod_NoAuth = procedure(const UserData: Pointer; const UserObject: TCoreClassObject;
    const fileName: SystemString; const Existed: Boolean; const fSiz: Int64) of object;
  TFileMD5Call_NoAuth = procedure(const UserData: Pointer; const UserObject: TCoreClassObject;
    const fileName: SystemString; const StartPos, EndPos: Int64; const MD5: TMD5);
  TFileMD5Method_NoAuth = procedure(const UserData: Pointer; const UserObject: TCoreClassObject;
    const fileName: SystemString; const StartPos, EndPos: Int64; const MD5: TMD5) of object;
  TFileCompleteCall_NoAuth = procedure(const UserData: Pointer; const UserObject: TCoreClassObject;
    stream: TCoreClassStream; const fileName: SystemString);
  TFileCompleteMethod_NoAuth = procedure(const UserData: Pointer; const UserObject: TCoreClassObject;
    stream: TCoreClassStream; const fileName: SystemString) of object;
  TFileFragmentDataCall_NoAuth = procedure(const UserData: Pointer; const UserObject: TCoreClassObject;
    const fileName: SystemString; const StartPos, EndPos: Int64; const DataPtr: Pointer; const DataSize: Int64; const MD5: TMD5);
  TFileFragmentDataMethod_NoAuth = procedure(const UserData: Pointer; const UserObject: TCoreClassObject;
    const fileName: SystemString; const StartPos, EndPos: Int64; const DataPtr: Pointer; const DataSize: Int64; const MD5: TMD5) of object;

{$IFDEF FPC}
  TGetFileInfoProc_NoAuth = procedure(const UserData: Pointer; const UserObject: TCoreClassObject;
    const fileName: SystemString; const Existed: Boolean; const fSiz: Int64) is nested;
  TFileMD5Proc_NoAuth = procedure(const UserData: Pointer; const UserObject: TCoreClassObject;
    const fileName: SystemString; const StartPos, EndPos: Int64; const MD5: TMD5) is nested;
  TFileCompleteProc_NoAuth = procedure(const UserData: Pointer; const UserObject: TCoreClassObject; stream: TCoreClassStream;
    const fileName: SystemString) is nested;
  TFileFragmentDataProc_NoAuth = procedure(const UserData: Pointer; const UserObject: TCoreClassObject;
    const fileName: SystemString; const StartPos, EndPos: Int64; const DataPtr: Pointer; const DataSize: Int64; const MD5: TMD5) is nested;
{$ELSE FPC}
  TGetFileInfoProc_NoAuth = reference to procedure(const UserData: Pointer; const UserObject: TCoreClassObject;
    const fileName: SystemString; const Existed: Boolean; const fSiz: Int64);
  TFileMD5Proc_NoAuth = reference to procedure(const UserData: Pointer; const UserObject: TCoreClassObject;
    const fileName: SystemString; const StartPos, EndPos: Int64; const MD5: TMD5);
  TFileCompleteProc_NoAuth = reference to procedure(const UserData: Pointer; const UserObject: TCoreClassObject; stream: TCoreClassStream;
    const fileName: SystemString);
  TFileFragmentDataProc_NoAuth = reference to procedure(const UserData: Pointer; const UserObject: TCoreClassObject;
    const fileName: SystemString; const StartPos, EndPos: Int64; const DataPtr: Pointer; const DataSize: Int64; const MD5: TMD5);
{$ENDIF FPC}

  TDTClient_NoAuth = class(TCoreClassInterfacedObject, ICommunicationFrameworkClientInterface)
  protected
    FSendTunnel, FRecvTunnel: TCommunicationFrameworkClient;
    FFileSystem: Boolean;
    FAutoFreeTunnel: Boolean;
    FLinkOk: Boolean;
    FWaitCommandTimeout: Cardinal;

    FCurrentStream: TCoreClassStream;
    FCurrentReceiveStreamFileName: SystemString;

    FCadencerEngine: TCadencer;
    FProgressEngine: TNProgressPost;

    FLastCadencerTime: Double;
    FServerDelay: Double;
  protected
    { client notify interface }
    procedure ClientConnected(Sender: TCommunicationFrameworkClient); virtual;
    procedure ClientDisconnect(Sender: TCommunicationFrameworkClient); virtual;
  public
    { registed client command }
    procedure Command_FileInfo(Sender: TPeerIO; InData: TDataFrameEngine); virtual;
    procedure Command_PostFile(Sender: TPeerIO; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64); virtual;
    procedure Command_PostFileOver(Sender: TPeerIO; InData: TDataFrameEngine); virtual;
    procedure Command_PostFileFragmentData(Sender: TPeerIO; InData: PByte; DataSize: NativeInt); virtual;

    procedure GetCurrentCadencer_StreamResult(Sender: TPeerIO; Result_: TDataFrameEngine); virtual;

    procedure GetFileInfo_StreamParamResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, Result_: TDataFrameEngine); virtual;
    procedure GetFileMD5_StreamParamResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, Result_: TDataFrameEngine); virtual;

    { Downloading files from the server asynchronously and triggering notifications when completed }
    procedure GetFile_StreamParamResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, Result_: TDataFrameEngine); virtual;

    { Downloading file fragment data from the server asynchronously and triggering notifications when completed }
    procedure GetFileFragmentData_StreamParamResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, Result_: TDataFrameEngine); virtual;

    { batch stream suppport }
    procedure Command_NewBatchStream(Sender: TPeerIO; InData: TDataFrameEngine); virtual;
    procedure Command_PostBatchStream(Sender: TPeerIO; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64); virtual;
    procedure Command_ClearBatchStream(Sender: TPeerIO; InData: TDataFrameEngine); virtual;
    procedure Command_PostBatchStreamDone(Sender: TPeerIO; InData: TDataFrameEngine); virtual;
    procedure Command_GetBatchStreamState(Sender: TPeerIO; InData, OutData: TDataFrameEngine); virtual;
  protected
    { async connect support }
    FAsyncConnectAddr: SystemString;
    FAsyncConnRecvPort, FAsyncConnSendPort: Word;
    FAsyncOnResultCall: TStateCall;
    FAsyncOnResultMethod: TStateMethod;
    FAsyncOnResultProc: TStateProc;
    procedure AsyncSendConnectResult(const cState: Boolean);
    procedure AsyncRecvConnectResult(const cState: Boolean);
    procedure TunnelLink_OnResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDataFrameEngine);
    procedure TunnelLink_OnFailed(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDataFrameEngine);
  public
    constructor Create(RecvTunnel_, SendTunnel_: TCommunicationFrameworkClient); virtual;
    destructor Destroy; override;

    property FileSystem: Boolean read FFileSystem;
    // free recveive+send tunnel from destroy, default is false
    property AutoFreeTunnel: Boolean read FAutoFreeTunnel write FAutoFreeTunnel;

    function Connected: Boolean; virtual;

    procedure SwitchAsMaxPerformance;
    procedure SwitchAsMaxSecurity;
    procedure SwitchAsDefaultPerformance;

    procedure Progress; virtual;
    procedure CadencerProgress(Sender: TObject; const deltaTime, newTime: Double); virtual;

    { sync connect }
    function Connect(addr: SystemString; const RecvPort, SendPort: Word): Boolean; overload; virtual;

    { async connection }
    procedure AsyncConnectC(addr: SystemString; const RecvPort, SendPort: Word; OnResult: TStateCall); overload; virtual;
    procedure AsyncConnectM(addr: SystemString; const RecvPort, SendPort: Word; OnResult: TStateMethod); overload; virtual;
    procedure AsyncConnectP(addr: SystemString; const RecvPort, SendPort: Word; OnResult: TStateProc); overload; virtual;
    { parameter async connection }
    procedure AsyncConnectC(addr: SystemString; const RecvPort, SendPort: Word; Param1: Pointer; Param2: TObject; OnResult: TParamStateCall); overload;
    procedure AsyncConnectM(addr: SystemString; const RecvPort, SendPort: Word; Param1: Pointer; Param2: TObject; OnResult: TParamStateMethod); overload;
    procedure AsyncConnectP(addr: SystemString; const RecvPort, SendPort: Word; Param1: Pointer; Param2: TObject; OnResult: TParamStateProc); overload;
    procedure Disconnect; virtual;

    { sync mode TunnelLink }
    function TunnelLink: Boolean; virtual;

    { async mode TunnelLink }
    procedure TunnelLinkC(OnCall: TStateCall); virtual;
    procedure TunnelLinkM(OnMethod: TStateMethod); virtual;
    procedure TunnelLinkP(OnProc: TStateProc); virtual;

    { async mode SyncCadencer }
    procedure SyncCadencer; virtual;

    { remote file time }
    procedure GetFileTimeM(RemoteFilename: SystemString; OnCallResult: TStreamMethod); overload;
    procedure GetFileTimeP(RemoteFilename: SystemString; OnCallResult: TStreamProc); overload;
    { remote file information }
    procedure GetFileInfoC(fileName: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TGetFileInfoCall_NoAuth); overload;
    procedure GetFileInfoM(fileName: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TGetFileInfoMethod_NoAuth); overload;
    procedure GetFileInfoP(fileName: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TGetFileInfoProc_NoAuth); overload;

    { remote md5 support with public store space }
    procedure GetFileMD5C(fileName: SystemString; const StartPos, EndPos: Int64;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileMD5Call_NoAuth); overload;
    procedure GetFileMD5M(fileName: SystemString; const StartPos, EndPos: Int64;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileMD5Method_NoAuth); overload;
    procedure GetFileMD5P(fileName: SystemString; const StartPos, EndPos: Int64;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileMD5Proc_NoAuth); overload;

    { normal download }
    procedure GetFileC(fileName, saveToPath: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnCompleteCall: TFileCompleteCall_NoAuth); overload;
    procedure GetFileM(fileName, saveToPath: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnCompleteMethod: TFileCompleteMethod_NoAuth); overload;
    procedure GetFileP(fileName, saveToPath: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnCompleteProc: TFileCompleteProc_NoAuth); overload;
    procedure GetFileAsC(fileName, saveFileName, saveToPath: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnCompleteCall: TFileCompleteCall_NoAuth); overload;
    procedure GetFileAsM(fileName, saveFileName, saveToPath: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnCompleteMethod: TFileCompleteMethod_NoAuth); overload;
    procedure GetFileAsP(fileName, saveFileName, saveToPath: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnCompleteProc: TFileCompleteProc_NoAuth); overload;
    { Synchronously waiting to download files from the server to complete }
    function GetFile(fileName, saveToPath: SystemString): Boolean; overload;
    { restore download }
    procedure GetFileC(fileName: SystemString; StartPos: Int64; saveToPath: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnCompleteCall: TFileCompleteCall_NoAuth); overload;
    procedure GetFileM(fileName: SystemString; StartPos: Int64; saveToPath: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnCompleteMethod: TFileCompleteMethod_NoAuth); overload;
    procedure GetFileP(fileName: SystemString; StartPos: Int64; saveToPath: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnCompleteProc: TFileCompleteProc_NoAuth); overload;
    procedure GetFileAsC(fileName, saveFileName: SystemString; StartPos: Int64; saveToPath: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnCompleteCall: TFileCompleteCall_NoAuth); overload;
    procedure GetFileAsM(fileName, saveFileName: SystemString; StartPos: Int64; saveToPath: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnCompleteMethod: TFileCompleteMethod_NoAuth); overload;
    procedure GetFileAsP(fileName, saveFileName: SystemString; StartPos: Int64; saveToPath: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnCompleteProc: TFileCompleteProc_NoAuth); overload;
    { Synchronously waiting to restore download files from the server to complete }
    function GetFile(fileName: SystemString; StartPos: Int64; saveToPath: SystemString): Boolean; overload;

    { file fragment }
    procedure GetFileFragmentDataC(fileName: SystemString; StartPos, EndPos: Int64;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnCompleteCall: TFileFragmentDataCall_NoAuth); overload;
    procedure GetFileFragmentDataM(fileName: SystemString; StartPos, EndPos: Int64;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnCompleteMethod: TFileFragmentDataMethod_NoAuth); overload;
    procedure GetFileFragmentDataP(fileName: SystemString; StartPos, EndPos: Int64;
      const UserData: Pointer; const UserObject: TCoreClassObject; const OnCompleteProc: TFileFragmentDataProc_NoAuth); overload;

    { automated download and verify }
    procedure AutomatedDownloadFileC(remoteFile, localFile: U_String; OnDownloadDone: TFileCompleteCall_NoAuth);
    procedure AutomatedDownloadFileM(remoteFile, localFile: U_String; OnDownloadDone: TFileCompleteMethod_NoAuth);
    procedure AutomatedDownloadFileP(remoteFile, localFile: U_String; OnDownloadDone: TFileCompleteProc_NoAuth);

    { Uploading local files asynchronously }
    procedure PostFile(fileName: SystemString); overload;
    { restore Uploading local files asynchronously }
    procedure PostFile(fileName: SystemString; StartPos: Int64); overload;
    { Upload an Stream asynchronously and automatically release Stream after completion }
    procedure PostFile(fn: SystemString; stream: TCoreClassStream; doneFreeStream: Boolean); overload;
    { restore Upload an Stream asynchronously and automatically release Stream after completion }
    procedure PostFile(fn: SystemString; stream: TCoreClassStream; StartPos: Int64; doneFreeStream: Boolean); overload;

    { automated Upload and verify }
    procedure AutomatedUploadFile(localFile: U_String);

    { batch stream suppport }
    procedure PostBatchStream(stream: TCoreClassStream; doneFreeStream: Boolean); overload;
    procedure PostBatchStreamC(stream: TCoreClassStream; doneFreeStream: Boolean; OnCompletedBackcall: TStateCall); overload;
    procedure PostBatchStreamM(stream: TCoreClassStream; doneFreeStream: Boolean; OnCompletedBackcall: TStateMethod); overload;
    procedure PostBatchStreamP(stream: TCoreClassStream; doneFreeStream: Boolean; OnCompletedBackcall: TStateProc); overload;
    procedure ClearBatchStream;
    procedure GetBatchStreamStateM(OnResult: TStreamMethod); overload;
    procedure GetBatchStreamStateM(Param1: Pointer; Param2: TObject; OnResult: TStreamParamMethod); overload;
    procedure GetBatchStreamStateP(OnResult: TStreamProc); overload;
    procedure GetBatchStreamStateP(Param1: Pointer; Param2: TObject; OnResult: TStreamParamProc); overload;
    function GetBatchStreamState(Result_: TDataFrameEngine; ATimeOut: TTimeTick): Boolean; overload;

    procedure RegisterCommand; virtual;
    procedure UnRegisterCommand; virtual;

    property LinkOk: Boolean read FLinkOk;
    property BindOk: Boolean read FLinkOk;
    property WaitCommandTimeout: Cardinal read FWaitCommandTimeout write FWaitCommandTimeout;

    property CadencerEngine: TCadencer read FCadencerEngine;

    property ProgressEngine: TNProgressPost read FProgressEngine;
    property ProgressPost: TNProgressPost read FProgressEngine;
    property PostProgress: TNProgressPost read FProgressEngine;
    property PostRun: TNProgressPost read FProgressEngine;
    property PostExecute: TNProgressPost read FProgressEngine;

    property ServerDelay: Double read FServerDelay;

    function RemoteInited: Boolean;

    property RecvTunnel: TCommunicationFrameworkClient read FRecvTunnel;
    property SendTunnel: TCommunicationFrameworkClient read FSendTunnel;
  end;

  TDT_P2PVM_NoAuth_OnState = record
    OnCall: TStateCall;
    OnMethod: TStateMethod;
    OnProc: TStateProc;
    procedure Init;
  end;

  PDT_P2PVM_NoAuth_OnState = ^TDT_P2PVM_NoAuth_OnState;

  TDT_P2PVM_NoAuth_Service = class(TCoreClassObject)
  private
    function GetQuietMode: Boolean;
    procedure SetQuietMode(const Value: Boolean);
  public
    RecvTunnel, SendTunnel: TCommunicationFrameworkWithP2PVM_Server;
    DTService: TDTService_NoAuth;
    PhysicsTunnel: TPhysicsServer;

    constructor Create(ServiceClass_: TDTService_NoAuthClass);
    destructor Destroy; override;
    procedure Progress; virtual;
    procedure StartService(ListenAddr, ListenPort, Auth: SystemString);
    procedure StopService;
    property QuietMode: Boolean read GetQuietMode write SetQuietMode;
  end;

  TDT_P2PVM_NoAuth_ServicePool = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TDT_P2PVM_NoAuth_Service>;

  TDT_P2PVM_NoAuth_Client = class(TCoreClassObject)
  private
    OnConnectResultState: TDT_P2PVM_NoAuth_OnState;
    Connecting: Boolean;
    Reconnection: Boolean;
    procedure DoConnectionResult(const state: Boolean);
    procedure DoAutomatedP2PVMClientConnectionDone(Sender: TCommunicationFramework; P_IO: TPeerIO);
    procedure DoTunnelLinkResult(const state: Boolean);

    function GetQuietMode: Boolean;
    procedure SetQuietMode(const Value: Boolean);
  public
    RecvTunnel, SendTunnel: TCommunicationFrameworkWithP2PVM_Client;
    DTClient: TDTClient_NoAuth;
    PhysicsTunnel: TPhysicsClient;
    LastAddr, LastPort, LastAuth: SystemString;
    AutomatedConnection: Boolean;

    constructor Create(ClientClass_: TDTClient_NoAuthClass);
    destructor Destroy; override;
    procedure Progress; virtual;
    procedure Connect(addr, Port, Auth: SystemString);
    procedure Connect_C(addr, Port, Auth: SystemString; OnResult: TStateCall);
    procedure Connect_M(addr, Port, Auth: SystemString; OnResult: TStateMethod);
    procedure Connect_P(addr, Port, Auth: SystemString; OnResult: TStateProc);
    procedure Disconnect;
    property QuietMode: Boolean read GetQuietMode write SetQuietMode;
  end;

  TDT_P2PVM_NoAuth_ClientPool = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TDT_P2PVM_NoAuth_Client>;

  TDT_P2PVM_NoAuth_Custom_Service = class;
  TDT_P2PVM_NoAuth_Custom_Service_Class = class of TDT_P2PVM_NoAuth_Custom_Service;

  TDT_P2PVM_NoAuth_Custom_Service = class(TCoreClassInterfacedObject)
  private
    function GetQuietMode: Boolean;
    procedure SetQuietMode(const Value: Boolean);
  public
    // bind
    Bind_PhysicsTunnel: TCommunicationFrameworkServer;
    Bind_P2PVM_Recv_IP6: SystemString;
    Bind_P2PVM_Recv_Port: Word;
    Bind_P2PVM_Send_IP6: SystemString;
    Bind_P2PVM_Send_Port: Word;
    // local
    RecvTunnel, SendTunnel: TCommunicationFrameworkWithP2PVM_Server;
    DTService: TDTService_NoAuth;

    constructor Create(ServiceClass_: TDTService_NoAuthClass; PhysicsTunnel_: TCommunicationFrameworkServer;
      P2PVM_Recv_Name_, P2PVM_Recv_IP6_, P2PVM_Recv_Port_,
      P2PVM_Send_Name_, P2PVM_Send_IP6_, P2PVM_Send_Port_: SystemString); virtual;
    destructor Destroy; override;
    procedure Progress; virtual;
    procedure StartService(); virtual;
    procedure StopService(); virtual;
    property QuietMode: Boolean read GetQuietMode write SetQuietMode;
  end;

  TDT_P2PVM_NoAuth_Custom_ServicePool = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TDT_P2PVM_NoAuth_Custom_Service>;

  TDT_P2PVM_NoAuth_Custom_Client = class;
  TDT_P2PVM_NoAuth_Custom_Client_Class = class of TDT_P2PVM_NoAuth_Custom_Client;
  TOn_DT_P2PVM_NoAuth_Custom_Client_TunnelLink = procedure(Sender: TDT_P2PVM_NoAuth_Custom_Client) of object;

  TDT_P2PVM_NoAuth_Custom_Client = class(TCoreClassInterfacedObject)
  private
    OnConnectResultState: TDT_P2PVM_NoAuth_OnState;
    Connecting: Boolean;
    Reconnection: Boolean;

    function GetQuietMode: Boolean;
    procedure SetQuietMode(const Value: Boolean);
  public
    // bind
    Bind_PhysicsTunnel: TCommunicationFrameworkClient;
    Bind_P2PVM_Recv_IP6: SystemString;
    Bind_P2PVM_Recv_Port: Word;
    Bind_P2PVM_Send_IP6: SystemString;
    Bind_P2PVM_Send_Port: Word;
    // local
    RecvTunnel, SendTunnel: TCommunicationFrameworkWithP2PVM_Client;
    DTClient: TDTClient_NoAuth;
    AutomatedConnection: Boolean;
    OnTunnelLink: TOn_DT_P2PVM_NoAuth_Custom_Client_TunnelLink;

    constructor Create(ClientClass_: TDTClient_NoAuthClass; PhysicsTunnel_: TCommunicationFrameworkClient;
      P2PVM_Recv_Name_, P2PVM_Recv_IP6_, P2PVM_Recv_Port_,
      P2PVM_Send_Name_, P2PVM_Send_IP6_, P2PVM_Send_Port_: SystemString); virtual;
    destructor Destroy; override;
    procedure Progress; virtual;
    procedure DoTunnelLinkResult(const state: Boolean); virtual;
    procedure AutoCheckPhysicsTunnelAndConnect;
    procedure Connect(); virtual;
    procedure Connect_C(OnResult: TStateCall); virtual;
    procedure Connect_M(OnResult: TStateMethod); virtual;
    procedure Connect_P(OnResult: TStateProc); virtual;
    procedure Disconnect; virtual;
    property QuietMode: Boolean read GetQuietMode write SetQuietMode;
  end;

  TDT_P2PVM_NoAuth_Custom_ClientPool = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TDT_P2PVM_NoAuth_Custom_Client>;

  PGetFileInfoStruct_NoAuth = ^TGetFileInfoStruct_NoAuth;

  TGetFileInfoStruct_NoAuth = record
    UserData: Pointer;
    UserObject: TCoreClassObject;
    fileName: SystemString;
    OnCompleteCall: TGetFileInfoCall_NoAuth;
    OnCompleteMethod: TGetFileInfoMethod_NoAuth;
    OnCompleteProc: TGetFileInfoProc_NoAuth;
  end;

  PFileMD5Struct_NoAuth = ^TFileMD5Struct_NoAuth;

  TFileMD5Struct_NoAuth = record
    UserData: Pointer;
    UserObject: TCoreClassObject;
    fileName: SystemString;
    StartPos, EndPos: Int64;
    OnCompleteCall: TFileMD5Call_NoAuth;
    OnCompleteMethod: TFileMD5Method_NoAuth;
    OnCompleteProc: TFileMD5Proc_NoAuth;
  end;

  PRemoteFileBackcall_NoAuth = ^TRemoteFileBackcall_NoAuth;

  TRemoteFileBackcall_NoAuth = record
    UserData: Pointer;
    UserObject: TCoreClassObject;
    OnCompleteCall: TFileCompleteCall_NoAuth;
    OnCompleteMethod: TFileCompleteMethod_NoAuth;
    OnCompleteProc: TFileCompleteProc_NoAuth;
  end;

  PFileFragmentDataBackcall_NoAuth = ^TFileFragmentDataBackcall_NoAuth;

  TFileFragmentDataBackcall_NoAuth = record
    UserData: Pointer;
    UserObject: TCoreClassObject;
    fileName: SystemString;
    StartPos, EndPos: Int64;
    OnCompleteCall: TFileFragmentDataCall_NoAuth;
    OnCompleteMethod: TFileFragmentDataMethod_NoAuth;
    OnCompleteProc: TFileFragmentDataProc_NoAuth;
  end;

  TAutomatedDownloadFile_Struct_NoAuth = class
  private
    remoteFile, localFile: SystemString;
    OnDownloadDoneC: TFileCompleteCall_NoAuth;
    OnDownloadDoneM: TFileCompleteMethod_NoAuth;
    OnDownloadDoneP: TFileCompleteProc_NoAuth;
    Client: TDTClient_NoAuth;
    r_fileName: SystemString;
    r_fileExisted: Boolean;
    r_fileSize: Int64;
    r_fileMD5: UnicodeMixedLib.TMD5;
    l_fileMD5: UnicodeMixedLib.TMD5;
    procedure DoComplete(const UserData: Pointer; const UserObject: TCoreClassObject; stream: TCoreClassStream; const fileName: SystemString);
    procedure DoResult_GetFileInfo(const UserData: Pointer; const UserObject: TCoreClassObject; const fileName: SystemString; const Existed: Boolean; const fSiz: Int64);
    procedure DoResult_GetFileMD5(const UserData: Pointer; const UserObject: TCoreClassObject; const fileName: SystemString; const StartPos, EndPos: Int64; const MD5: UnicodeMixedLib.TMD5);
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TAutomatedUploadFile_Struct_NoAuth = class
  private
    localFile: SystemString;
    Client: TDTClient_NoAuth;
    r_fileName: SystemString;
    r_fileExisted: Boolean;
    r_fileSize: Int64;
    r_fileMD5: UnicodeMixedLib.TMD5;
    procedure DoResult_GetFileInfo(const UserData: Pointer; const UserObject: TCoreClassObject; const fileName: SystemString; const Existed: Boolean; const fSiz: Int64);
    procedure DoResult_GetFileMD5(const UserData: Pointer; const UserObject: TCoreClassObject; const fileName: SystemString; const StartPos, EndPos: Int64; const MD5: UnicodeMixedLib.TMD5);
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TCommunicationFramework_DoubleTunnelService_NoAuth = TDTService_NoAuth;
  TCommunicationFramework_DoubleTunnelClient_NoAuth = TDTClient_NoAuth;

implementation

uses SysUtils;

procedure TAutomatedDownloadFile_Struct_NoAuth.DoComplete(const UserData: Pointer; const UserObject: TCoreClassObject; stream: TCoreClassStream; const fileName: SystemString);
begin
  try
    if Assigned(OnDownloadDoneC) then
        OnDownloadDoneC(UserData, UserObject, stream, fileName);
    if Assigned(OnDownloadDoneM) then
        OnDownloadDoneM(UserData, UserObject, stream, fileName);
    if Assigned(OnDownloadDoneP) then
        OnDownloadDoneP(UserData, UserObject, stream, fileName);
  except
  end;
  Free;
end;

procedure TAutomatedDownloadFile_Struct_NoAuth.DoResult_GetFileInfo(const UserData: Pointer; const UserObject: TCoreClassObject;
  const fileName: SystemString; const Existed: Boolean; const fSiz: Int64);
begin
  r_fileExisted := Existed;

  if Existed then
    begin
      r_fileName := fileName;
      r_fileSize := fSiz;
      if not umlFileExists(localFile) then
          Client.GetFileAsM(remoteFile, umlGetFileName(localFile), 0, umlGetFilePath(localFile), nil, nil, {$IFDEF FPC}@{$ENDIF FPC}DoComplete)
      else if fSiz >= umlGetFileSize(localFile) then
          Client.GetFileMD5M(umlGetFileName(remoteFile), 0, umlGetFileSize(localFile), nil, nil, {$IFDEF FPC}@{$ENDIF FPC}DoResult_GetFileMD5)
      else
          Client.GetFileAsM(remoteFile, umlGetFileName(localFile), 0, umlGetFilePath(localFile), nil, nil, {$IFDEF FPC}@{$ENDIF FPC}DoComplete);
    end
  else
    begin
      DoStatus('no found remote file: "%s" ', [remoteFile]);
      Free;
    end;
end;

procedure TAutomatedDownloadFile_Struct_NoAuth.DoResult_GetFileMD5(const UserData: Pointer; const UserObject: TCoreClassObject;
  const fileName: SystemString; const StartPos, EndPos: Int64; const MD5: UnicodeMixedLib.TMD5);
begin
  r_fileMD5 := MD5;
  l_fileMD5 := umlFileMD5(localFile);
  if umlMD5Compare(l_fileMD5, MD5) then
    begin
      if r_fileSize = umlGetFileSize(localFile) then
          DoComplete(nil, nil, nil, localFile)
      else
          Client.GetFileAsM(fileName, umlGetFileName(localFile), umlGetFileSize(localFile), umlGetFilePath(localFile), nil, nil, {$IFDEF FPC}@{$ENDIF FPC}DoComplete);
    end
  else
      Client.GetFileAsM(fileName, umlGetFileName(localFile), 0, umlGetFilePath(localFile), nil, nil, {$IFDEF FPC}@{$ENDIF FPC}DoComplete);
end;

constructor TAutomatedDownloadFile_Struct_NoAuth.Create;
begin
  inherited Create;
  remoteFile := '';
  localFile := '';
  OnDownloadDoneC := nil;
  OnDownloadDoneM := nil;
  OnDownloadDoneP := nil;
  Client := nil;
  r_fileName := '';
  r_fileExisted := False;
  r_fileSize := -1;
  r_fileMD5 := NullMD5;
  l_fileMD5 := NullMD5;
end;

destructor TAutomatedDownloadFile_Struct_NoAuth.Destroy;
begin
  remoteFile := '';
  localFile := '';
  r_fileName := '';
  inherited Destroy;
end;

procedure TAutomatedUploadFile_Struct_NoAuth.DoResult_GetFileInfo(const UserData: Pointer; const UserObject: TCoreClassObject;
  const fileName: SystemString; const Existed: Boolean; const fSiz: Int64);
begin
  r_fileExisted := Existed;

  if Existed then
    begin
      r_fileName := fileName;
      r_fileSize := fSiz;
      if r_fileSize <= umlGetFileSize(localFile) then
          Client.GetFileMD5M(umlGetFileName(localFile), 0, r_fileSize, nil, nil, {$IFDEF FPC}@{$ENDIF FPC}DoResult_GetFileMD5)
      else
          Client.PostFile(localFile);
    end
  else
      Client.PostFile(localFile);
end;

procedure TAutomatedUploadFile_Struct_NoAuth.DoResult_GetFileMD5(const UserData: Pointer; const UserObject: TCoreClassObject;
  const fileName: SystemString; const StartPos, EndPos: Int64; const MD5: UnicodeMixedLib.TMD5);
begin
  r_fileMD5 := MD5;
  if umlMD5Compare(r_fileMD5, umlFileMD5(localFile, 0, r_fileSize)) then
    begin
      if umlGetFileSize(localFile) > r_fileSize then
          Client.PostFile(fileName, r_fileSize);
    end
  else
      Client.PostFile(localFile);
  Free;
end;

constructor TAutomatedUploadFile_Struct_NoAuth.Create;
begin
  inherited Create;
  localFile := '';
  Client := nil;
  r_fileName := '';
  r_fileExisted := False;
  r_fileSize := -1;
  r_fileMD5 := NullMD5;
end;

destructor TAutomatedUploadFile_Struct_NoAuth.Destroy;
begin
  localFile := '';
  r_fileName := '';
  inherited Destroy;
end;

constructor TPeerClientUserDefineForSendTunnel_NoAuth.Create(Owner_: TPeerIO);
begin
  inherited Create(Owner_);
  RecvTunnel := nil;
  RecvTunnelID := 0;
  DoubleTunnelService := nil;
end;

destructor TPeerClientUserDefineForSendTunnel_NoAuth.Destroy;
begin
  if (DoubleTunnelService <> nil) and (RecvTunnelID > 0) and (RecvTunnel <> nil) then
    begin
      if DoubleTunnelService.FRecvTunnel.Exists(RecvTunnelID) then
          DoubleTunnelService.FRecvTunnel.PeerIO[RecvTunnelID].Disconnect;
    end;
  inherited Destroy;
end;

function TPeerClientUserDefineForSendTunnel_NoAuth.LinkOk: Boolean;
begin
  Result := DoubleTunnelService <> nil;
end;

constructor TPeerClientUserDefineForRecvTunnel_NoAuth.Create(Owner_: TPeerIO);
begin
  inherited Create(Owner_);
  SendTunnel := nil;
  SendTunnelID := 0;
  DoubleTunnelService := nil;
  FCurrentFileStream := nil;
  FCurrentReceiveFileName := '';
end;

destructor TPeerClientUserDefineForRecvTunnel_NoAuth.Destroy;
begin
  if DoubleTunnelService <> nil then
    begin
      DoubleTunnelService.UserOut(Self);

      if (DoubleTunnelService <> nil) and (SendTunnelID > 0) and (SendTunnel <> nil) then
        begin
          if DoubleTunnelService.FSendTunnel.Exists(SendTunnelID) then
              DoubleTunnelService.FSendTunnel.PeerIO[SendTunnelID].Disconnect;
        end;

      DoubleTunnelService := nil;
    end;

  if FCurrentFileStream <> nil then
      DisposeObject(FCurrentFileStream);
  FCurrentFileStream := nil;
  inherited Destroy;
end;

function TPeerClientUserDefineForRecvTunnel_NoAuth.LinkOk: Boolean;
begin
  Result := DoubleTunnelService <> nil;
end;

procedure TDTService_NoAuth.UserLinkSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth);
begin
  try
    if Assigned(FOnLinkSuccess) then
        FOnLinkSuccess(Self, UserDefineIO);
  except
  end;
end;

procedure TDTService_NoAuth.UserOut(UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth);
begin
  try
    if Assigned(FOnUserOut) then
        FOnUserOut(Self, UserDefineIO);
  except
  end;
end;

procedure TDTService_NoAuth.UserPostFileSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth; fn: SystemString);
begin
end;

procedure TDTService_NoAuth.Command_TunnelLink(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
var
  RecvID, SendID: Cardinal;
  UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth;
begin
  RecvID := InData.Reader.ReadCardinal;
  SendID := InData.Reader.ReadCardinal;

  UserDefineIO := GetUserDefineRecvTunnel(Sender);

  if not FSendTunnel.Exists(SendID) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(Format('send tunnel Illegal:%d', [SendID]));
      OutData.WriteBool(FFileSystem);
      Exit;
    end;

  if not FRecvTunnel.Exists(RecvID) then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(Format('recv tunnel Illegal:%d', [RecvID]));
      OutData.WriteBool(FFileSystem);
      Exit;
    end;

  if Sender.ID <> RecvID then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(Format('recv tunnel Illegal:%d-%d', [Sender.ID, RecvID]));
      OutData.WriteBool(FFileSystem);
      Exit;
    end;

  UserDefineIO.SendTunnel := FSendTunnel.PeerIO[SendID].UserDefine as TPeerClientUserDefineForSendTunnel_NoAuth;
  UserDefineIO.SendTunnelID := SendID;
  UserDefineIO.DoubleTunnelService := Self;

  UserDefineIO.SendTunnel.RecvTunnel := UserDefineIO;
  UserDefineIO.SendTunnel.RecvTunnelID := RecvID;
  UserDefineIO.SendTunnel.DoubleTunnelService := Self;

  OutData.WriteBool(True);
  OutData.WriteString(Format('tunnel link success! recv:%d <-> send:%d', [RecvID, SendID]));
  OutData.WriteBool(FFileSystem);

  UserLinkSuccess(UserDefineIO);
end;

procedure TDTService_NoAuth.Command_GetCurrentCadencer(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
begin
  FCadencerEngine.Progress;
  OutData.WriteDouble(FCadencerEngine.CurrentTime);
end;

procedure TDTService_NoAuth.Command_GetFileTime(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
var
  UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth;
  fullfn, fileName: SystemString;
begin
  if not FFileSystem then
      Exit;
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LinkOk then
      Exit;

  fileName := InData.Reader.ReadString;
  fullfn := umlCombineFileName(FFileReceiveDirectory, fileName);
  if not umlFileExists(fullfn) then
    begin
      OutData.WriteBool(False);
      Exit;
    end;
  OutData.WriteBool(True);
  OutData.WriteString(fileName);
  OutData.WriteDouble(umlGetFileTime(fullfn));
end;

procedure TDTService_NoAuth.Command_GetFileInfo(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
var
  UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth;
  fullfn, fileName: SystemString;
begin
  if not FFileSystem then
      Exit;
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LinkOk then
      Exit;

  fileName := InData.Reader.ReadString;

  fullfn := umlCombineFileName(FFileReceiveDirectory, fileName);
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

procedure TDTService_NoAuth.Command_GetFileMD5(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
var
  UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth;
  fullfn, fileName: SystemString;
  StartPos, EndPos: Int64;
  fs: TCoreClassFileStream;
  MD5: TMD5;
begin
  if not FFileSystem then
      Exit;
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LinkOk then
      Exit;

  fileName := InData.Reader.ReadString;
  StartPos := InData.Reader.ReadInt64;
  EndPos := InData.Reader.ReadInt64;

  fullfn := umlCombineFileName(FFileReceiveDirectory, fileName);
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
      MD5 := umlFileMD5(fullfn)
  else
      MD5 := umlStreamMD5(fs, StartPos, EndPos);

  OutData.WriteBool(True);
  OutData.WriteMD5(MD5);
  DisposeObject(fs);
end;

procedure TDTService_NoAuth.Command_GetFile(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
var
  UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth;
  fullfn, fileName, remoteinfo: SystemString;
  StartPos: Int64;
  RemoteBackcallAddr: UInt64;
  sendDE: TDataFrameEngine;
  fs: TCoreClassFileStream;
  MD5: TMD5;
begin
  if not FFileSystem then
      Exit;
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LinkOk then
      Exit;

  fileName := InData.Reader.ReadString;
  StartPos := InData.Reader.ReadInt64;
  remoteinfo := InData.Reader.ReadString;
  RemoteBackcallAddr := InData.Reader.ReadPointer;

  fullfn := umlCombineFileName(FFileReceiveDirectory, fileName);
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

  MD5 := umlFileMD5(fullfn);

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

procedure TDTService_NoAuth.Command_GetFileAs(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
var
  UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth;
  fullfn, fileName, saveFileName, remoteinfo: SystemString;
  StartPos: Int64;
  RemoteBackcallAddr: UInt64;
  sendDE: TDataFrameEngine;
  fs: TCoreClassFileStream;
  MD5: TMD5;
begin
  if not FFileSystem then
      Exit;
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LinkOk then
      Exit;

  fileName := InData.Reader.ReadString;
  saveFileName := InData.Reader.ReadString;
  StartPos := InData.Reader.ReadInt64;
  remoteinfo := InData.Reader.ReadString;
  RemoteBackcallAddr := InData.Reader.ReadPointer;

  fullfn := umlCombineFileName(FFileReceiveDirectory, fileName);
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
  sendDE.WriteString(saveFileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteInt64(fs.Size);
  sendDE.WriteString(remoteinfo);
  UserDefineIO.SendTunnel.Owner.SendDirectStreamCmd(C_FileInfo, sendDE);
  DisposeObject(sendDE);

  MD5 := umlFileMD5(fullfn);

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

procedure TDTService_NoAuth.Command_PostFileInfo(Sender: TPeerIO; InData: TDataFrameEngine);
var
  UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth;
  fn: SystemString;
  StartPos: Int64;
  FSize: Int64;
  fullfn: SystemString;
begin
  if not FFileSystem then
      Exit;
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LinkOk then
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

  fullfn := umlCombineFileName(FFileReceiveDirectory, fn);
  UserDefineIO.FCurrentReceiveFileName := fullfn;
  try
    if (StartPos > 0) and (umlFileExists(fullfn)) then
      begin
        UserDefineIO.FCurrentFileStream := TCoreClassFileStream.Create(fullfn, fmOpenReadWrite);
        if StartPos <= UserDefineIO.FCurrentFileStream.Size then
            UserDefineIO.FCurrentFileStream.Position := StartPos
        else
            UserDefineIO.FCurrentFileStream.Position := UserDefineIO.FCurrentFileStream.Size;
        Sender.Print(Format('restore post to public: %s', [fullfn]));
      end
    else
      begin
        UserDefineIO.FCurrentFileStream := TCoreClassFileStream.Create(fullfn, fmCreate);
        Sender.Print(Format('normal post to public: %s', [fullfn]));
      end;
  except
    Sender.Print('post file failed: %s', [fullfn]);
    UserDefineIO.FCurrentFileStream := nil;
  end;
end;

procedure TDTService_NoAuth.Command_PostFile(Sender: TPeerIO; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64);
var
  UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth;
begin
  if not FFileSystem then
      Exit;
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LinkOk then
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

procedure TDTService_NoAuth.Command_PostFileOver(Sender: TPeerIO; InData: TDataFrameEngine);
var
  UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth;
  ClientMD5, MD5: TMD5;
  fn: SystemString;
begin
  if not FFileSystem then
      Exit;
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LinkOk then
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
          Sender.Print('Received File Completed:%s', [fn]);
          UserPostFileSuccess(UserDefineIO, fn);
        end
      else
        begin
          Sender.Print('File md5 error:%s', [fn]);
          umlDeleteFile(fn);
        end;
    end;
end;

procedure TDTService_NoAuth.Command_GetFileFragmentData(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
var
  UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth;
  fullfn, fileName: SystemString;
  StartPos, EndPos, siz, fp: Int64;
  RemoteBackcallAddr: UInt64;
  fs: TCoreClassFileStream;
  mem_: TMemoryStream64;
  MD5: TMD5;
begin
  if not FFileSystem then
      Exit;
  UserDefineIO := GetUserDefineRecvTunnel(Sender);
  if not UserDefineIO.LinkOk then
      Exit;

  fileName := InData.Reader.ReadString;
  StartPos := InData.Reader.ReadInt64;
  EndPos := InData.Reader.ReadInt64;
  RemoteBackcallAddr := InData.Reader.ReadPointer;

  fullfn := umlCombineFileName(FFileReceiveDirectory, fileName);
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

procedure TDTService_NoAuth.Command_NewBatchStream(Sender: TPeerIO; InData: TDataFrameEngine);
var
  RT: TPeerClientUserDefineForRecvTunnel_NoAuth;
  p: PBigStreamBatchPostData;
begin
  RT := GetUserDefineRecvTunnel(Sender);
  if not RT.LinkOk then
      Exit;
  p := RT.BigStreamBatchList.NewPostData;
  p^.RemoteMD5 := InData.Reader.ReadMD5;
  p^.CompletedBackcallPtr := InData.Reader.ReadPointer;
end;

procedure TDTService_NoAuth.Command_PostBatchStream(Sender: TPeerIO; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64);
var
  RT: TPeerClientUserDefineForRecvTunnel_NoAuth;
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

procedure TDTService_NoAuth.Command_ClearBatchStream(Sender: TPeerIO; InData: TDataFrameEngine);
var
  RT: TPeerClientUserDefineForRecvTunnel_NoAuth;
begin
  RT := GetUserDefineRecvTunnel(Sender);
  if not RT.LinkOk then
      Exit;
  RT.BigStreamBatchList.Clear;
end;

procedure TDTService_NoAuth.Command_PostBatchStreamDone(Sender: TPeerIO; InData: TDataFrameEngine);
var
  RT: TPeerClientUserDefineForRecvTunnel_NoAuth;
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

procedure TDTService_NoAuth.Command_GetBatchStreamState(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
var
  RT: TPeerClientUserDefineForRecvTunnel_NoAuth;
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

constructor TDTService_NoAuth.Create(RecvTunnel_, SendTunnel_: TCommunicationFrameworkServer);
begin
  inherited Create;
  FRecvTunnel := RecvTunnel_;
  FRecvTunnel.PeerClientUserDefineClass := TPeerClientUserDefineForRecvTunnel_NoAuth;
  FSendTunnel := SendTunnel_;
  FSendTunnel.PeerClientUserDefineClass := TPeerClientUserDefineForSendTunnel_NoAuth;

  FRecvTunnel.DoubleChannelFramework := Self;
  FSendTunnel.DoubleChannelFramework := Self;

  FCadencerEngine := TCadencer.Create;
  FCadencerEngine.OnProgress := {$IFDEF FPC}@{$ENDIF FPC}CadencerProgress;
  FProgressEngine := TNProgressPost.Create;

  FFileSystem := True;
  FFileReceiveDirectory := umlCurrentPath;

  if not umlDirectoryExists(FFileReceiveDirectory) then
      umlCreateDirectory(FFileReceiveDirectory);

  SwitchAsDefaultPerformance;

  FOnLinkSuccess := nil;
  FOnUserOut := nil;
end;

destructor TDTService_NoAuth.Destroy;
begin
  DisposeObject(FCadencerEngine);
  DisposeObject(FProgressEngine);
  inherited Destroy;
end;

procedure TDTService_NoAuth.SwitchAsMaxPerformance;
begin
  FRecvTunnel.SwitchMaxPerformance;
  FSendTunnel.SwitchMaxPerformance;
end;

procedure TDTService_NoAuth.SwitchAsMaxSecurity;
begin
  FRecvTunnel.SwitchMaxSecurity;
  FSendTunnel.SwitchMaxSecurity;
end;

procedure TDTService_NoAuth.SwitchAsDefaultPerformance;
begin
  FRecvTunnel.SwitchDefaultPerformance;
  FSendTunnel.SwitchDefaultPerformance;
end;

procedure TDTService_NoAuth.Progress;
begin
  FCadencerEngine.Progress;
  FRecvTunnel.Progress;
  FSendTunnel.Progress;
end;

procedure TDTService_NoAuth.CadencerProgress(Sender: TObject; const deltaTime, newTime: Double);
begin
  FProgressEngine.Progress(deltaTime);
end;

procedure TDTService_NoAuth.RegisterCommand;
begin
  FRecvTunnel.RegisterStream(C_TunnelLink).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_TunnelLink;
  FRecvTunnel.RegisterStream(C_GetCurrentCadencer).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_GetCurrentCadencer;

  FRecvTunnel.RegisterStream(C_GetFileTime).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_GetFileTime;
  FRecvTunnel.RegisterStream(C_GetFileInfo).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_GetFileInfo;
  FRecvTunnel.RegisterStream(C_GetFileMD5).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_GetFileMD5;
  FRecvTunnel.RegisterStream(C_GetFile).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_GetFile;
  FRecvTunnel.RegisterStream(C_GetFileAs).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_GetFileAs;
  FRecvTunnel.RegisterDirectStream(C_PostFileInfo).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_PostFileInfo;
  FRecvTunnel.RegisterBigStream(C_PostFile).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_PostFile;
  FRecvTunnel.RegisterDirectStream(C_PostFileOver).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_PostFileOver;
  FRecvTunnel.RegisterStream(C_GetFileFragmentData).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_GetFileFragmentData;

  FRecvTunnel.RegisterDirectStream(C_NewBatchStream).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_NewBatchStream;
  FRecvTunnel.RegisterBigStream(C_PostBatchStream).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_PostBatchStream;
  FRecvTunnel.RegisterDirectStream(C_ClearBatchStream).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_ClearBatchStream;
  FRecvTunnel.RegisterDirectStream(C_PostBatchStreamDone).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_PostBatchStreamDone;
  FRecvTunnel.RegisterStream(C_GetBatchStreamState).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_GetBatchStreamState;
end;

procedure TDTService_NoAuth.UnRegisterCommand;
begin
  FRecvTunnel.DeleteRegistedCMD(C_TunnelLink);
  FRecvTunnel.DeleteRegistedCMD(C_GetCurrentCadencer);

  FRecvTunnel.DeleteRegistedCMD(C_GetFileTime);
  FRecvTunnel.DeleteRegistedCMD(C_GetFileInfo);
  FRecvTunnel.DeleteRegistedCMD(C_GetFileMD5);
  FRecvTunnel.DeleteRegistedCMD(C_GetFile);
  FRecvTunnel.DeleteRegistedCMD(C_GetFileAs);
  FRecvTunnel.DeleteRegistedCMD(C_PostFileInfo);
  FRecvTunnel.DeleteRegistedCMD(C_PostFile);
  FRecvTunnel.DeleteRegistedCMD(C_PostFileOver);
  FRecvTunnel.DeleteRegistedCMD(C_GetFileFragmentData);

  FRecvTunnel.DeleteRegistedCMD(C_NewBatchStream);
  FRecvTunnel.DeleteRegistedCMD(C_PostBatchStream);
  FRecvTunnel.DeleteRegistedCMD(C_ClearBatchStream);
  FRecvTunnel.DeleteRegistedCMD(C_PostBatchStreamDone);
  FRecvTunnel.DeleteRegistedCMD(C_GetBatchStreamState);
end;

function TDTService_NoAuth.GetUserDefineRecvTunnel(RecvCli: TPeerIO): TPeerClientUserDefineForRecvTunnel_NoAuth;
begin
  Result := RecvCli.UserDefine as TPeerClientUserDefineForRecvTunnel_NoAuth;
end;

function TDTService_NoAuth.TotalLinkCount: Integer;
begin
  Result := RecvTunnel.Count;
end;

procedure TDTService_NoAuth.PostBatchStream(cli: TPeerIO; stream: TCoreClassStream; doneFreeStream: Boolean);
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

procedure TDTService_NoAuth.PostBatchStreamC(cli: TPeerIO; stream: TCoreClassStream; doneFreeStream: Boolean; OnCompletedBackcall: TStateCall);
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

procedure TDTService_NoAuth.PostBatchStreamM(cli: TPeerIO; stream: TCoreClassStream; doneFreeStream: Boolean; OnCompletedBackcall: TStateMethod);
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

procedure TDTService_NoAuth.PostBatchStreamP(cli: TPeerIO; stream: TCoreClassStream; doneFreeStream: Boolean; OnCompletedBackcall: TStateProc);
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

procedure TDTService_NoAuth.ClearBatchStream(cli: TPeerIO);
var
  de: TDataFrameEngine;
  p: POnStateStruct;
begin
  de := TDataFrameEngine.Create;
  cli.SendDirectStreamCmd(C_ClearBatchStream, de);
  DisposeObject(de);
end;

procedure TDTService_NoAuth.GetBatchStreamStateM(cli: TPeerIO; OnResult: TStreamMethod);
var
  de: TDataFrameEngine;
  p: POnStateStruct;
begin
  de := TDataFrameEngine.Create;
  cli.SendStreamCmdM(C_GetBatchStreamState, de, OnResult);
  DisposeObject(de);
end;

procedure TDTService_NoAuth.GetBatchStreamStateM(cli: TPeerIO; Param1: Pointer; Param2: TObject; OnResult: TStreamParamMethod);
var
  de: TDataFrameEngine;
  p: POnStateStruct;
begin
  de := TDataFrameEngine.Create;
  cli.SendStreamCmdM(C_GetBatchStreamState, de, Param1, Param2, OnResult);
  DisposeObject(de);
end;

procedure TDTService_NoAuth.GetBatchStreamStateP(cli: TPeerIO; OnResult: TStreamProc);
var
  de: TDataFrameEngine;
  p: POnStateStruct;
begin
  de := TDataFrameEngine.Create;
  cli.SendStreamCmdP(C_GetBatchStreamState, de, OnResult);
  DisposeObject(de);
end;

procedure TDTService_NoAuth.GetBatchStreamStateP(cli: TPeerIO; Param1: Pointer; Param2: TObject; OnResult: TStreamParamProc);
var
  de: TDataFrameEngine;
  p: POnStateStruct;
begin
  de := TDataFrameEngine.Create;
  cli.SendStreamCmdP(C_GetBatchStreamState, de, Param1, Param2, OnResult);
  DisposeObject(de);
end;

constructor TClientUserDefineForRecvTunnel_NoAuth.Create(Owner_: TPeerIO);
begin
  inherited Create(Owner_);
  Client := nil;
  SendTunnel := nil;
end;

destructor TClientUserDefineForRecvTunnel_NoAuth.Destroy;
begin
  if Client <> nil then
      Client.FLinkOk := False;
  inherited Destroy;
end;

constructor TClientUserDefineForSendTunnel_NoAuth.Create(Owner_: TPeerIO);
begin
  inherited Create(Owner_);
  Client := nil;
  RecvTunnel := nil;
end;

destructor TClientUserDefineForSendTunnel_NoAuth.Destroy;
begin
  if Client <> nil then
      Client.FLinkOk := False;
  inherited Destroy;
end;

{ client notify interface }
procedure TDTClient_NoAuth.ClientConnected(Sender: TCommunicationFrameworkClient);
begin
end;

procedure TDTClient_NoAuth.ClientDisconnect(Sender: TCommunicationFrameworkClient);
begin
end;

procedure TDTClient_NoAuth.Command_FileInfo(Sender: TPeerIO; InData: TDataFrameEngine);
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
    Sender.Print('post file failed: %s', [fullfn]);
    { FRecvTunnel.ClientIO.Disconnect; }
    FCurrentStream := nil;
  end;
end;

procedure TDTClient_NoAuth.Command_PostFile(Sender: TPeerIO; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64);
begin
  if FCurrentStream <> nil then
    begin
      InData.Position := 0;
      if InData.Size > 0 then
        begin
          FCurrentStream.Position := FCurrentStream.Size;
          FCurrentStream.CopyFrom(InData, InData.Size);
        end;
    end;
end;

procedure TDTClient_NoAuth.Command_PostFileOver(Sender: TPeerIO; InData: TDataFrameEngine);
var
  servMD5, MD5: TMD5;
  RemoteBackcallAddr: UInt64;
  p: PRemoteFileBackcall_NoAuth;
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
end;

procedure TDTClient_NoAuth.Command_PostFileFragmentData(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
var
  mem_: TMemoryStream64;
  StartPos, EndPos, siz: Int64;
  RemoteBackcallAddr: UInt64;
  p: PFileFragmentDataBackcall_NoAuth;
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

procedure TDTClient_NoAuth.GetCurrentCadencer_StreamResult(Sender: TPeerIO; Result_: TDataFrameEngine);
var
  servTime: Double;
begin
  servTime := Result_.Reader.ReadDouble;

  FCadencerEngine.Progress;
  FServerDelay := FCadencerEngine.CurrentTime - FLastCadencerTime;

  FCadencerEngine.CurrentTime := servTime + FServerDelay;
  FCadencerEngine.Progress;
end;

procedure TDTClient_NoAuth.GetFileInfo_StreamParamResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, Result_: TDataFrameEngine);
var
  p: PGetFileInfoStruct_NoAuth;
  Existed: Boolean;
  fSiz: Int64;
begin
  p := PGetFileInfoStruct_NoAuth(Param1);
  Existed := Result_.Reader.ReadBool;
  fSiz := Result_.Reader.ReadInt64;
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

procedure TDTClient_NoAuth.GetFileMD5_StreamParamResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, Result_: TDataFrameEngine);
var
  p: PFileMD5Struct_NoAuth;
  successed: Boolean;
  MD5: TMD5;
begin
  p := PFileMD5Struct_NoAuth(Param1);
  successed := Result_.Reader.ReadBool;
  if successed then
      MD5 := Result_.Reader.ReadMD5
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

procedure TDTClient_NoAuth.GetFile_StreamParamResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, Result_: TDataFrameEngine);
var
  p: PRemoteFileBackcall_NoAuth;
begin
  if Result_.Count > 0 then
    begin
      if Result_.Reader.ReadBool then
          Exit;
      Sender.Print('get file failed:%s', [Result_.Reader.ReadString]);
    end;

  p := Param1;
  Dispose(p);
end;

procedure TDTClient_NoAuth.GetFileFragmentData_StreamParamResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, Result_: TDataFrameEngine);
var
  p: PFileFragmentDataBackcall_NoAuth;
begin
  if Result_.Count > 0 then
    begin
      if Result_.Reader.ReadBool then
          Exit;
    end;

  p := Param1;
  Dispose(p);
end;

procedure TDTClient_NoAuth.Command_NewBatchStream(Sender: TPeerIO; InData: TDataFrameEngine);
var
  RT: TClientUserDefineForRecvTunnel_NoAuth;
  p: PBigStreamBatchPostData;
begin
  if not LinkOk then
      Exit;
  RT := Sender.UserDefine as TClientUserDefineForRecvTunnel_NoAuth;
  p := RT.BigStreamBatchList.NewPostData;
  p^.RemoteMD5 := InData.Reader.ReadMD5;
  p^.CompletedBackcallPtr := InData.Reader.ReadPointer;
end;

procedure TDTClient_NoAuth.Command_PostBatchStream(Sender: TPeerIO; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64);
var
  RT: TClientUserDefineForRecvTunnel_NoAuth;
  p: PBigStreamBatchPostData;
  de: TDataFrameEngine;
begin
  if not LinkOk then
      Exit;
  RT := Sender.UserDefine as TClientUserDefineForRecvTunnel_NoAuth;

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

procedure TDTClient_NoAuth.Command_ClearBatchStream(Sender: TPeerIO; InData: TDataFrameEngine);
var
  RT: TClientUserDefineForRecvTunnel_NoAuth;
  p: PBigStreamBatchPostData;
  de: TDataFrameEngine;
begin
  if not LinkOk then
      Exit;
  RT := Sender.UserDefine as TClientUserDefineForRecvTunnel_NoAuth;
  RT.BigStreamBatchList.Clear;
end;

procedure TDTClient_NoAuth.Command_PostBatchStreamDone(Sender: TPeerIO; InData: TDataFrameEngine);
var
  RT: TClientUserDefineForRecvTunnel_NoAuth;
  rMD5, sMD5: TMD5;
  backCallVal: UInt64;
  backCallValPtr: POnStateStruct;
  MD5Verify: Boolean;
begin
  if not LinkOk then
      Exit;
  RT := Sender.UserDefine as TClientUserDefineForRecvTunnel_NoAuth;

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

procedure TDTClient_NoAuth.Command_GetBatchStreamState(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
var
  RT: TClientUserDefineForRecvTunnel_NoAuth;
  i: Integer;
  p: PBigStreamBatchPostData;

  de: TDataFrameEngine;
begin
  if not LinkOk then
      Exit;
  RT := Sender.UserDefine as TClientUserDefineForRecvTunnel_NoAuth;

  for i := 0 to RT.BigStreamBatchList.Count - 1 do
    begin
      p := RT.BigStreamBatchList[i];
      de := TDataFrameEngine.Create;
      p^.Encode(de);
      OutData.WriteDataFrame(de);
      DisposeObject(de);
    end;
end;

procedure TDTClient_NoAuth.AsyncSendConnectResult(const cState: Boolean);
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

procedure TDTClient_NoAuth.AsyncRecvConnectResult(const cState: Boolean);
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

procedure TDTClient_NoAuth.TunnelLink_OnResult(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, Result_: TDataFrameEngine);
var
  r: Boolean;
  p: POnStateStruct;
begin
  p := Param1;
  r := False;
  if Result_.Count > 0 then
    begin
      r := Result_.ReadBool(0);
      FSendTunnel.ClientIO.Print(Result_.ReadString(1));

      if r then
        begin
          if Result_.Count >= 2 then
              FFileSystem := Result_.ReadBool(2)
          else
              FFileSystem := True;
          TClientUserDefineForSendTunnel_NoAuth(FSendTunnel.ClientIO.UserDefine).Client := Self;
          TClientUserDefineForSendTunnel_NoAuth(FSendTunnel.ClientIO.UserDefine).RecvTunnel := TClientUserDefineForRecvTunnel_NoAuth(FRecvTunnel.ClientIO.UserDefine);

          TClientUserDefineForRecvTunnel_NoAuth(FRecvTunnel.ClientIO.UserDefine).Client := Self;
          TClientUserDefineForRecvTunnel_NoAuth(FRecvTunnel.ClientIO.UserDefine).SendTunnel := TClientUserDefineForSendTunnel_NoAuth(FSendTunnel.ClientIO.UserDefine);

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

procedure TDTClient_NoAuth.TunnelLink_OnFailed(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDataFrameEngine);
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

constructor TDTClient_NoAuth.Create(RecvTunnel_, SendTunnel_: TCommunicationFrameworkClient);
begin
  inherited Create;
  FRecvTunnel := RecvTunnel_;
  FRecvTunnel.NotyifyInterface := Self;
  FRecvTunnel.PeerClientUserDefineClass := TClientUserDefineForRecvTunnel_NoAuth;

  FSendTunnel := SendTunnel_;
  FSendTunnel.NotyifyInterface := Self;
  FSendTunnel.PeerClientUserDefineClass := TClientUserDefineForSendTunnel_NoAuth;

  FRecvTunnel.DoubleChannelFramework := Self;
  FSendTunnel.DoubleChannelFramework := Self;

  FFileSystem := False;

  FAutoFreeTunnel := False;

  FLinkOk := False;
  FWaitCommandTimeout := 5000;

  FCurrentStream := nil;
  FCurrentReceiveStreamFileName := '';

  FCadencerEngine := TCadencer.Create;
  FCadencerEngine.OnProgress := {$IFDEF FPC}@{$ENDIF FPC}CadencerProgress;
  FProgressEngine := TNProgressPost.Create;

  FLastCadencerTime := 0;
  FServerDelay := 0;

  FAsyncConnectAddr := '';
  FAsyncConnRecvPort := 0;
  FAsyncConnSendPort := 0;
  FAsyncOnResultCall := nil;
  FAsyncOnResultMethod := nil;
  FAsyncOnResultProc := nil;

  SwitchAsDefaultPerformance;
end;

destructor TDTClient_NoAuth.Destroy;
begin
  if FAutoFreeTunnel then
    begin
      DisposeObjectAndNil(FRecvTunnel);
      DisposeObjectAndNil(FSendTunnel);
    end;
  DisposeObject([FCadencerEngine, FProgressEngine]);

  inherited Destroy;
end;

function TDTClient_NoAuth.Connected: Boolean;
begin
  try
      Result := FSendTunnel.Connected and FRecvTunnel.Connected;
  except
      Result := False;
  end;
end;

procedure TDTClient_NoAuth.SwitchAsMaxPerformance;
begin
  FRecvTunnel.SwitchMaxPerformance;
  FSendTunnel.SwitchMaxPerformance;
end;

procedure TDTClient_NoAuth.SwitchAsMaxSecurity;
begin
  FRecvTunnel.SwitchMaxSecurity;
  FSendTunnel.SwitchMaxSecurity;
end;

procedure TDTClient_NoAuth.SwitchAsDefaultPerformance;
begin
  FRecvTunnel.SwitchDefaultPerformance;
  FSendTunnel.SwitchDefaultPerformance;
end;

procedure TDTClient_NoAuth.Progress;
var
  p2pVMDone: Boolean;
begin
  FCadencerEngine.Progress;

  try
    p2pVMDone := False;

    if (not p2pVMDone) and (FRecvTunnel is TCommunicationFrameworkWithP2PVM_Client) then
      if FRecvTunnel.ClientIO <> nil then
        begin
          FRecvTunnel.ProgressWaitSend(FRecvTunnel.ClientIO);
          p2pVMDone := True;
        end;
    FRecvTunnel.Progress;

    if (not p2pVMDone) and (FSendTunnel is TCommunicationFrameworkWithP2PVM_Client) then
      if FSendTunnel.ClientIO <> nil then
        begin
          FSendTunnel.ProgressWaitSend(FSendTunnel.ClientIO);
          p2pVMDone := True;
        end;
    FSendTunnel.Progress;

    if not Connected then
        FLinkOk := False;
  except
  end;
end;

procedure TDTClient_NoAuth.CadencerProgress(Sender: TObject; const deltaTime, newTime: Double);
begin
  FProgressEngine.Progress(deltaTime);
end;

function TDTClient_NoAuth.Connect(addr: SystemString; const RecvPort, SendPort: Word): Boolean;
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

procedure TDTClient_NoAuth.AsyncConnectC(addr: SystemString; const RecvPort, SendPort: Word; OnResult: TStateCall);
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

procedure TDTClient_NoAuth.AsyncConnectM(addr: SystemString; const RecvPort, SendPort: Word; OnResult: TStateMethod);
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

procedure TDTClient_NoAuth.AsyncConnectP(addr: SystemString; const RecvPort, SendPort: Word; OnResult: TStateProc);
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

procedure TDTClient_NoAuth.AsyncConnectC(addr: SystemString; const RecvPort, SendPort: Word; Param1: Pointer; Param2: TObject; OnResult: TParamStateCall);
var
  ParamBridge: TStateParamBridge;
begin
  ParamBridge := TStateParamBridge.Create;
  ParamBridge.Param1 := Param1;
  ParamBridge.Param2 := Param2;
  ParamBridge.OnNotifyC := OnResult;
  AsyncConnectM(addr, RecvPort, SendPort, {$IFDEF FPC}@{$ENDIF FPC}ParamBridge.DoStateResult);
end;

procedure TDTClient_NoAuth.AsyncConnectM(addr: SystemString; const RecvPort, SendPort: Word; Param1: Pointer; Param2: TObject; OnResult: TParamStateMethod);
var
  ParamBridge: TStateParamBridge;
begin
  ParamBridge := TStateParamBridge.Create;
  ParamBridge.Param1 := Param1;
  ParamBridge.Param2 := Param2;
  ParamBridge.OnNotifyM := OnResult;
  AsyncConnectM(addr, RecvPort, SendPort, {$IFDEF FPC}@{$ENDIF FPC}ParamBridge.DoStateResult);
end;

procedure TDTClient_NoAuth.AsyncConnectP(addr: SystemString; const RecvPort, SendPort: Word; Param1: Pointer; Param2: TObject; OnResult: TParamStateProc);
var
  ParamBridge: TStateParamBridge;
begin
  ParamBridge := TStateParamBridge.Create;
  ParamBridge.Param1 := Param1;
  ParamBridge.Param2 := Param2;
  ParamBridge.OnNotifyP := OnResult;
  AsyncConnectM(addr, RecvPort, SendPort, {$IFDEF FPC}@{$ENDIF FPC}ParamBridge.DoStateResult);
end;

procedure TDTClient_NoAuth.Disconnect;
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

function TDTClient_NoAuth.TunnelLink: Boolean;
var
  sendDE, resDE: TDataFrameEngine;
begin
  if FLinkOk then
      Exit(True);
  FLinkOk := False;
  Result := False;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  SyncCadencer;

  sendDE := TDataFrameEngine.Create;
  resDE := TDataFrameEngine.Create;

  sendDE.WriteCardinal(FSendTunnel.RemoteID);
  sendDE.WriteCardinal(FRecvTunnel.RemoteID);

  FSendTunnel.WaitSendStreamCmd(C_TunnelLink, sendDE, resDE, FWaitCommandTimeout);

  if resDE.Count > 0 then
    begin
      Result := resDE.ReadBool(0);
      FSendTunnel.ClientIO.Print(resDE.ReadString(1));

      if Result then
        begin
          if resDE.Count >= 2 then
              FFileSystem := resDE.ReadBool(2)
          else
              FFileSystem := True;
          TClientUserDefineForSendTunnel_NoAuth(FSendTunnel.ClientIO.UserDefine).Client := Self;
          TClientUserDefineForSendTunnel_NoAuth(FSendTunnel.ClientIO.UserDefine).RecvTunnel := TClientUserDefineForRecvTunnel_NoAuth(FRecvTunnel.ClientIO.UserDefine);

          TClientUserDefineForRecvTunnel_NoAuth(FRecvTunnel.ClientIO.UserDefine).Client := Self;
          TClientUserDefineForRecvTunnel_NoAuth(FRecvTunnel.ClientIO.UserDefine).SendTunnel := TClientUserDefineForSendTunnel_NoAuth(FSendTunnel.ClientIO.UserDefine);

          FLinkOk := True;
        end;
    end;

  DisposeObject(sendDE);
  DisposeObject(resDE);
end;

procedure TDTClient_NoAuth.TunnelLinkC(OnCall: TStateCall);
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

procedure TDTClient_NoAuth.TunnelLinkM(OnMethod: TStateMethod);
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

procedure TDTClient_NoAuth.TunnelLinkP(OnProc: TStateProc);
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

procedure TDTClient_NoAuth.SyncCadencer;
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

procedure TDTClient_NoAuth.GetFileTimeM(RemoteFilename: SystemString; OnCallResult: TStreamMethod);
var
  sendDE: TDataFrameEngine;
begin
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteString(RemoteFilename);
  FSendTunnel.SendStreamCmdM(C_GetFileTime, sendDE, OnCallResult);
  DisposeObject(sendDE);
end;

procedure TDTClient_NoAuth.GetFileTimeP(RemoteFilename: SystemString; OnCallResult: TStreamProc);
var
  sendDE: TDataFrameEngine;
begin
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteString(RemoteFilename);
  FSendTunnel.SendStreamCmdP(C_GetFileTime, sendDE, OnCallResult);
  DisposeObject(sendDE);
end;

{ remote file exists }
procedure TDTClient_NoAuth.GetFileInfoC(fileName: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TGetFileInfoCall_NoAuth);
var
  sendDE: TDataFrameEngine;
  p: PGetFileInfoStruct_NoAuth;
begin
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteString(fileName);
  new(p);
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.fileName := fileName;
  p^.OnCompleteCall := OnComplete;
  p^.OnCompleteMethod := nil;
  p^.OnCompleteProc := nil;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetFileInfo, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetFileInfo_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient_NoAuth.GetFileInfoM(fileName: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TGetFileInfoMethod_NoAuth);
var
  sendDE: TDataFrameEngine;
  p: PGetFileInfoStruct_NoAuth;
begin
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteString(fileName);
  new(p);
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.fileName := fileName;
  p^.OnCompleteCall := nil;
  p^.OnCompleteMethod := OnComplete;
  p^.OnCompleteProc := nil;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetFileInfo, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetFileInfo_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient_NoAuth.GetFileInfoP(fileName: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TGetFileInfoProc_NoAuth);
var
  sendDE: TDataFrameEngine;
  p: PGetFileInfoStruct_NoAuth;
begin
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteString(fileName);
  new(p);
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.fileName := fileName;
  p^.OnCompleteCall := nil;
  p^.OnCompleteMethod := nil;
  p^.OnCompleteProc := OnComplete;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetFileInfo, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetFileInfo_StreamParamResult);
  DisposeObject(sendDE);
end;

{ remote md5 support with public store space }
procedure TDTClient_NoAuth.GetFileMD5C(fileName: SystemString; const StartPos, EndPos: Int64;
  const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileMD5Call_NoAuth);
var
  sendDE: TDataFrameEngine;
  p: PFileMD5Struct_NoAuth;
begin
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteString(fileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteInt64(EndPos);
  new(p);
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.fileName := fileName;
  p^.StartPos := StartPos;
  p^.EndPos := EndPos;
  p^.OnCompleteCall := OnComplete;
  p^.OnCompleteMethod := nil;
  p^.OnCompleteProc := nil;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetFileMD5, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetFileMD5_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient_NoAuth.GetFileMD5M(fileName: SystemString; const StartPos, EndPos: Int64;
  const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileMD5Method_NoAuth);
var
  sendDE: TDataFrameEngine;
  p: PFileMD5Struct_NoAuth;
begin
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteString(fileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteInt64(EndPos);
  new(p);
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.fileName := fileName;
  p^.StartPos := StartPos;
  p^.EndPos := EndPos;
  p^.OnCompleteCall := nil;
  p^.OnCompleteMethod := OnComplete;
  p^.OnCompleteProc := nil;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetFileMD5, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetFileMD5_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient_NoAuth.GetFileMD5P(fileName: SystemString; const StartPos, EndPos: Int64;
  const UserData: Pointer; const UserObject: TCoreClassObject; const OnComplete: TFileMD5Proc_NoAuth);
var
  sendDE: TDataFrameEngine;
  p: PFileMD5Struct_NoAuth;
begin
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteString(fileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteInt64(EndPos);
  new(p);
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.fileName := fileName;
  p^.StartPos := StartPos;
  p^.EndPos := EndPos;
  p^.OnCompleteCall := nil;
  p^.OnCompleteMethod := nil;
  p^.OnCompleteProc := OnComplete;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetFileMD5, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetFileMD5_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient_NoAuth.GetFileC(fileName, saveToPath: SystemString;
  const UserData: Pointer; const UserObject: TCoreClassObject; const OnCompleteCall: TFileCompleteCall_NoAuth);
begin
  GetFileC(fileName, 0, saveToPath, UserData, UserObject, OnCompleteCall);
end;

procedure TDTClient_NoAuth.GetFileM(fileName, saveToPath: SystemString;
  const UserData: Pointer; const UserObject: TCoreClassObject; const OnCompleteMethod: TFileCompleteMethod_NoAuth);
begin
  GetFileM(fileName, 0, saveToPath, UserData, UserObject, OnCompleteMethod);
end;

procedure TDTClient_NoAuth.GetFileP(fileName, saveToPath: SystemString;
  const UserData: Pointer; const UserObject: TCoreClassObject; const OnCompleteProc: TFileCompleteProc_NoAuth);
begin
  GetFileP(fileName, 0, saveToPath, UserData, UserObject, OnCompleteProc);
end;

procedure TDTClient_NoAuth.GetFileAsC(fileName, saveFileName, saveToPath: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnCompleteCall: TFileCompleteCall_NoAuth);
begin
  GetFileAsC(fileName, saveFileName, 0, saveToPath, UserData, UserObject, OnCompleteCall);
end;

procedure TDTClient_NoAuth.GetFileAsM(fileName, saveFileName, saveToPath: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnCompleteMethod: TFileCompleteMethod_NoAuth);
begin
  GetFileAsM(fileName, saveFileName, 0, saveToPath, UserData, UserObject, OnCompleteMethod);
end;

procedure TDTClient_NoAuth.GetFileAsP(fileName, saveFileName, saveToPath: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnCompleteProc: TFileCompleteProc_NoAuth);
begin
  GetFileAsP(fileName, saveFileName, 0, saveToPath, UserData, UserObject, OnCompleteProc);
end;

function TDTClient_NoAuth.GetFile(fileName, saveToPath: SystemString): Boolean;
begin
  Result := GetFile(fileName, 0, saveToPath);
end;

{ restore download }
procedure TDTClient_NoAuth.GetFileC(fileName: SystemString; StartPos: Int64; saveToPath: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnCompleteCall: TFileCompleteCall_NoAuth);
var
  sendDE: TDataFrameEngine;
  p: PRemoteFileBackcall_NoAuth;
begin
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDataFrameEngine.Create;

  sendDE.WriteString(fileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteString(saveToPath);
  new(p);
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.OnCompleteCall := OnCompleteCall;
  p^.OnCompleteMethod := nil;
  p^.OnCompleteProc := nil;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetFile, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetFile_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient_NoAuth.GetFileM(fileName: SystemString; StartPos: Int64; saveToPath: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnCompleteMethod: TFileCompleteMethod_NoAuth);
var
  sendDE: TDataFrameEngine;
  p: PRemoteFileBackcall_NoAuth;
begin
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDataFrameEngine.Create;

  sendDE.WriteString(fileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteString(saveToPath);
  new(p);
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.OnCompleteCall := nil;
  p^.OnCompleteMethod := OnCompleteMethod;
  p^.OnCompleteProc := nil;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetFile, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetFile_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient_NoAuth.GetFileP(fileName: SystemString; StartPos: Int64; saveToPath: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnCompleteProc: TFileCompleteProc_NoAuth);
var
  sendDE: TDataFrameEngine;
  p: PRemoteFileBackcall_NoAuth;
begin
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDataFrameEngine.Create;

  sendDE.WriteString(fileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteString(saveToPath);
  new(p);
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.OnCompleteCall := nil;
  p^.OnCompleteMethod := nil;
  p^.OnCompleteProc := OnCompleteProc;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetFile, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetFile_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient_NoAuth.GetFileAsC(fileName, saveFileName: SystemString; StartPos: Int64; saveToPath: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnCompleteCall: TFileCompleteCall_NoAuth);
var
  sendDE: TDataFrameEngine;
  p: PRemoteFileBackcall_NoAuth;
begin
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDataFrameEngine.Create;

  sendDE.WriteString(fileName);
  sendDE.WriteString(saveFileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteString(saveToPath);
  new(p);
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.OnCompleteCall := OnCompleteCall;
  p^.OnCompleteMethod := nil;
  p^.OnCompleteProc := nil;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetFileAs, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetFile_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient_NoAuth.GetFileAsM(fileName, saveFileName: SystemString; StartPos: Int64; saveToPath: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnCompleteMethod: TFileCompleteMethod_NoAuth);
var
  sendDE: TDataFrameEngine;
  p: PRemoteFileBackcall_NoAuth;
begin
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDataFrameEngine.Create;

  sendDE.WriteString(fileName);
  sendDE.WriteString(saveFileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteString(saveToPath);
  new(p);
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.OnCompleteCall := nil;
  p^.OnCompleteMethod := OnCompleteMethod;
  p^.OnCompleteProc := nil;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetFileAs, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetFile_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient_NoAuth.GetFileAsP(fileName, saveFileName: SystemString; StartPos: Int64; saveToPath: SystemString; const UserData: Pointer; const UserObject: TCoreClassObject; const OnCompleteProc: TFileCompleteProc_NoAuth);
var
  sendDE: TDataFrameEngine;
  p: PRemoteFileBackcall_NoAuth;
begin
  if not FFileSystem then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  sendDE := TDataFrameEngine.Create;

  sendDE.WriteString(fileName);
  sendDE.WriteString(saveFileName);
  sendDE.WriteInt64(StartPos);
  sendDE.WriteString(saveToPath);
  new(p);
  p^.UserData := UserData;
  p^.UserObject := UserObject;
  p^.OnCompleteCall := nil;
  p^.OnCompleteMethod := nil;
  p^.OnCompleteProc := OnCompleteProc;
  sendDE.WritePointer(p);

  FSendTunnel.SendStreamCmdM(C_GetFileAs, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetFile_StreamParamResult);
  DisposeObject(sendDE);
end;

{ Synchronously waiting to download files from the server to complete }
function TDTClient_NoAuth.GetFile(fileName: SystemString; StartPos: Int64; saveToPath: SystemString): Boolean;
var
  sendDE, resDE: TDataFrameEngine;
begin
  Result := False;
  if not FFileSystem then
      Exit;
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

  FSendTunnel.WaitSendStreamCmd(C_GetFile, sendDE, resDE, FWaitCommandTimeout);

  if resDE.Count > 0 then
    begin
      Result := resDE.Reader.ReadBool;
      FSendTunnel.ClientIO.Print(resDE.Reader.ReadString);
    end;

  DisposeObject(sendDE);
  DisposeObject(resDE);
end;

procedure TDTClient_NoAuth.GetFileFragmentDataC(fileName: SystemString; StartPos, EndPos: Int64;
  const UserData: Pointer; const UserObject: TCoreClassObject; const OnCompleteCall: TFileFragmentDataCall_NoAuth);
var
  sendDE: TDataFrameEngine;
  p: PFileFragmentDataBackcall_NoAuth;
begin
  if not FFileSystem then
      Exit;
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

  FSendTunnel.SendStreamCmdM(C_GetFileFragmentData, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetFileFragmentData_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient_NoAuth.GetFileFragmentDataM(fileName: SystemString; StartPos, EndPos: Int64;
  const UserData: Pointer; const UserObject: TCoreClassObject; const OnCompleteMethod: TFileFragmentDataMethod_NoAuth);
var
  sendDE: TDataFrameEngine;
  p: PFileFragmentDataBackcall_NoAuth;
begin
  if not FFileSystem then
      Exit;
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

  FSendTunnel.SendStreamCmdM(C_GetFileFragmentData, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetFileFragmentData_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient_NoAuth.GetFileFragmentDataP(fileName: SystemString; StartPos, EndPos: Int64;
  const UserData: Pointer; const UserObject: TCoreClassObject; const OnCompleteProc: TFileFragmentDataProc_NoAuth);
var
  sendDE: TDataFrameEngine;
  p: PFileFragmentDataBackcall_NoAuth;
begin
  if not FFileSystem then
      Exit;
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

  FSendTunnel.SendStreamCmdM(C_GetFileFragmentData, sendDE, p, nil, {$IFDEF FPC}@{$ENDIF FPC}GetFileFragmentData_StreamParamResult);
  DisposeObject(sendDE);
end;

procedure TDTClient_NoAuth.AutomatedDownloadFileC(remoteFile, localFile: U_String; OnDownloadDone: TFileCompleteCall_NoAuth);
var
  tmp: TAutomatedDownloadFile_Struct_NoAuth;
begin
  if not FFileSystem then
      Exit;
  tmp := TAutomatedDownloadFile_Struct_NoAuth.Create;
  tmp.remoteFile := remoteFile;
  tmp.localFile := localFile;
  tmp.OnDownloadDoneC := OnDownloadDone;
  tmp.Client := Self;

  GetFileInfoM(umlGetFileName(remoteFile), nil, nil, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoResult_GetFileInfo);
end;

procedure TDTClient_NoAuth.AutomatedDownloadFileM(remoteFile, localFile: U_String; OnDownloadDone: TFileCompleteMethod_NoAuth);
var
  tmp: TAutomatedDownloadFile_Struct_NoAuth;
begin
  if not FFileSystem then
      Exit;
  tmp := TAutomatedDownloadFile_Struct_NoAuth.Create;
  tmp.remoteFile := remoteFile;
  tmp.localFile := localFile;
  tmp.OnDownloadDoneM := OnDownloadDone;
  tmp.Client := Self;

  GetFileInfoM(umlGetFileName(remoteFile), nil, nil, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoResult_GetFileInfo);
end;

procedure TDTClient_NoAuth.AutomatedDownloadFileP(remoteFile, localFile: U_String; OnDownloadDone: TFileCompleteProc_NoAuth);
var
  tmp: TAutomatedDownloadFile_Struct_NoAuth;
begin
  if not FFileSystem then
      Exit;
  tmp := TAutomatedDownloadFile_Struct_NoAuth.Create;
  tmp.remoteFile := remoteFile;
  tmp.localFile := localFile;
  tmp.OnDownloadDoneP := OnDownloadDone;
  tmp.Client := Self;

  GetFileInfoM(umlGetFileName(remoteFile), nil, nil, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoResult_GetFileInfo);
end;

procedure TDTClient_NoAuth.PostFile(fileName: SystemString);
var
  sendDE: TDataFrameEngine;
  fs: TCoreClassFileStream;
  MD5: TMD5;
begin
  if not FFileSystem then
      Exit;
  if not umlFileExists(fileName) then
      Exit;
  if not FSendTunnel.Connected then
      Exit;
  if not FRecvTunnel.Connected then
      Exit;

  fs := TCoreClassFileStream.Create(fileName, fmOpenRead or fmShareDenyNone);

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteString(umlGetFileName(fileName));
  sendDE.WriteInt64(0);
  sendDE.WriteInt64(fs.Size);
  FSendTunnel.SendDirectStreamCmd(C_PostFileInfo, sendDE);
  DisposeObject(sendDE);

  MD5 := umlFileMD5(fileName);

  fs.Position := 0;
  FSendTunnel.SendBigStream(C_PostFile, fs, True);

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteMD5(MD5);
  FSendTunnel.SendDirectStreamCmd(C_PostFileOver, sendDE);
  DisposeObject(sendDE);
end;

procedure TDTClient_NoAuth.PostFile(fileName: SystemString; StartPos: Int64);
var
  sendDE: TDataFrameEngine;
  fs: TCoreClassFileStream;
  MD5: TMD5;
begin
  if not FFileSystem then
      Exit;
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
  FSendTunnel.SendDirectStreamCmd(C_PostFileInfo, sendDE);
  DisposeObject(sendDE);

  MD5 := umlFileMD5(fileName);

  fs.Position := 0;
  FSendTunnel.SendBigStream(C_PostFile, fs, StartPos, True);

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteMD5(MD5);
  FSendTunnel.SendDirectStreamCmd(C_PostFileOver, sendDE);
  DisposeObject(sendDE);
end;

procedure TDTClient_NoAuth.PostFile(fn: SystemString; stream: TCoreClassStream; doneFreeStream: Boolean);
var
  sendDE: TDataFrameEngine;
  MD5: TMD5;
begin
  if (not FSendTunnel.Connected) or (not FRecvTunnel.Connected) or (not FFileSystem) then
    begin
      if doneFreeStream then
          DisposeObject(stream);
      Exit;
    end;

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteString(umlGetFileName(fn));
  sendDE.WriteInt64(0);
  sendDE.WriteInt64(stream.Size);
  FSendTunnel.SendDirectStreamCmd(C_PostFileInfo, sendDE);
  DisposeObject(sendDE);

  stream.Position := 0;
  MD5 := umlStreamMD5(stream);

  stream.Position := 0;
  FSendTunnel.SendBigStream(C_PostFile, stream, doneFreeStream);

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteMD5(MD5);
  FSendTunnel.SendDirectStreamCmd(C_PostFileOver, sendDE);
  DisposeObject(sendDE);
end;

procedure TDTClient_NoAuth.PostFile(fn: SystemString; stream: TCoreClassStream; StartPos: Int64; doneFreeStream: Boolean);
var
  sendDE: TDataFrameEngine;
  MD5: TMD5;
begin
  if (not FSendTunnel.Connected) or (not FRecvTunnel.Connected) or (not FFileSystem) then
    begin
      if doneFreeStream then
          DisposeObject(stream);
      Exit;
    end;

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteString(umlGetFileName(fn));
  sendDE.WriteInt64(StartPos);
  sendDE.WriteInt64(stream.Size);
  FSendTunnel.SendDirectStreamCmd(C_PostFileInfo, sendDE);
  DisposeObject(sendDE);

  stream.Position := 0;
  MD5 := umlStreamMD5(stream);

  stream.Position := 0;
  FSendTunnel.SendBigStream(C_PostFile, stream, StartPos, doneFreeStream);

  sendDE := TDataFrameEngine.Create;
  sendDE.WriteMD5(MD5);
  FSendTunnel.SendDirectStreamCmd(C_PostFileOver, sendDE);
  DisposeObject(sendDE);
end;

procedure TDTClient_NoAuth.AutomatedUploadFile(localFile: U_String);
var
  tmp: TAutomatedUploadFile_Struct_NoAuth;
begin
  if not FFileSystem then
      Exit;
  tmp := TAutomatedUploadFile_Struct_NoAuth.Create;
  tmp.localFile := localFile;
  tmp.Client := Self;

  GetFileInfoM(umlGetFileName(localFile), nil, nil, {$IFDEF FPC}@{$ENDIF FPC}tmp.DoResult_GetFileInfo);
end;

procedure TDTClient_NoAuth.PostBatchStream(stream: TCoreClassStream; doneFreeStream: Boolean);
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

procedure TDTClient_NoAuth.PostBatchStreamC(stream: TCoreClassStream; doneFreeStream: Boolean; OnCompletedBackcall: TStateCall);
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

procedure TDTClient_NoAuth.PostBatchStreamM(stream: TCoreClassStream; doneFreeStream: Boolean; OnCompletedBackcall: TStateMethod);
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

procedure TDTClient_NoAuth.PostBatchStreamP(stream: TCoreClassStream; doneFreeStream: Boolean; OnCompletedBackcall: TStateProc);
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

procedure TDTClient_NoAuth.ClearBatchStream;
var
  de: TDataFrameEngine;
  p: POnStateStruct;
begin
  de := TDataFrameEngine.Create;
  SendTunnel.SendDirectStreamCmd(C_ClearBatchStream, de);
  DisposeObject(de);
end;

procedure TDTClient_NoAuth.GetBatchStreamStateM(OnResult: TStreamMethod);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  SendTunnel.SendStreamCmdM(C_GetBatchStreamState, de, OnResult);
  DisposeObject(de);
end;

procedure TDTClient_NoAuth.GetBatchStreamStateM(Param1: Pointer; Param2: TObject; OnResult: TStreamParamMethod);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  SendTunnel.SendStreamCmdM(C_GetBatchStreamState, de, Param1, Param2, OnResult);
  DisposeObject(de);
end;

procedure TDTClient_NoAuth.GetBatchStreamStateP(OnResult: TStreamProc);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  SendTunnel.SendStreamCmdP(C_GetBatchStreamState, de, OnResult);
  DisposeObject(de);
end;

procedure TDTClient_NoAuth.GetBatchStreamStateP(Param1: Pointer; Param2: TObject; OnResult: TStreamParamProc);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  SendTunnel.SendStreamCmdP(C_GetBatchStreamState, de, Param1, Param2, OnResult);
  DisposeObject(de);
end;

function TDTClient_NoAuth.GetBatchStreamState(Result_: TDataFrameEngine; ATimeOut: TTimeTick): Boolean;
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  SendTunnel.WaitSendStreamCmd(C_GetBatchStreamState, de, Result_, ATimeOut);
  Result := Result_.Count > 0;
  DisposeObject(de);
end;

procedure TDTClient_NoAuth.RegisterCommand;
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

procedure TDTClient_NoAuth.UnRegisterCommand;
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

function TDTClient_NoAuth.RemoteInited: Boolean;
begin
  Result := FSendTunnel.RemoteInited and FRecvTunnel.RemoteInited;
end;

procedure TDT_P2PVM_NoAuth_OnState.Init;
begin
  OnCall := nil;
  OnMethod := nil;
  OnProc := nil;
end;

function TDT_P2PVM_NoAuth_Service.GetQuietMode: Boolean;
begin
  Result := RecvTunnel.QuietMode and SendTunnel.QuietMode and PhysicsTunnel.QuietMode;
end;

procedure TDT_P2PVM_NoAuth_Service.SetQuietMode(const Value: Boolean);
begin
  RecvTunnel.QuietMode := Value;
  SendTunnel.QuietMode := Value;
  PhysicsTunnel.QuietMode := Value;
end;

constructor TDT_P2PVM_NoAuth_Service.Create(ServiceClass_: TDTService_NoAuthClass);
begin
  inherited Create;
  RecvTunnel := TCommunicationFrameworkWithP2PVM_Server.Create;
  RecvTunnel.QuietMode := True;

  SendTunnel := TCommunicationFrameworkWithP2PVM_Server.Create;
  SendTunnel.QuietMode := True;

  DTService := ServiceClass_.Create(RecvTunnel, SendTunnel);
  DTService.RegisterCommand;
  DTService.SwitchAsDefaultPerformance;

  PhysicsTunnel := TPhysicsServer.Create;
  PhysicsTunnel.QuietMode := True;
  PhysicsTunnel.AutomatedP2PVMBindService.AddService(RecvTunnel);
  PhysicsTunnel.AutomatedP2PVMBindService.AddService(SendTunnel);
  PhysicsTunnel.AutomatedP2PVMService := True;

  RecvTunnel.PrefixName := 'NA';
  RecvTunnel.Name := 'R';
  SendTunnel.PrefixName := 'NA';
  SendTunnel.Name := 'S';
  PhysicsTunnel.PrefixName := 'Physics';
  PhysicsTunnel.Name := 'p2pVM';
end;

destructor TDT_P2PVM_NoAuth_Service.Destroy;
begin
  StopService;
  DisposeObject(RecvTunnel);
  DisposeObject(SendTunnel);
  DisposeObject(DTService);
  DisposeObject(PhysicsTunnel);
  inherited Destroy;
end;

procedure TDT_P2PVM_NoAuth_Service.Progress;
begin
  DTService.Progress;
  PhysicsTunnel.Progress;
end;

procedure TDT_P2PVM_NoAuth_Service.StartService(ListenAddr, ListenPort, Auth: SystemString);
begin
  StopService;
  RecvTunnel.StartService('::', 1);
  SendTunnel.StartService('::', 2);
  PhysicsTunnel.AutomatedP2PVMAuthToken := Auth;
  if PhysicsTunnel.StartService(ListenAddr, umlStrToInt(ListenPort)) then
      DoStatus('listening %s:%s ok.', [TranslateBindAddr(ListenAddr), ListenPort])
  else
      DoStatus('listening %s:%s failed!', [TranslateBindAddr(ListenAddr), ListenPort]);
end;

procedure TDT_P2PVM_NoAuth_Service.StopService;
begin
  PhysicsTunnel.StopService;
  RecvTunnel.StopService;
  SendTunnel.StopService;
end;

procedure TDT_P2PVM_NoAuth_Client.DoConnectionResult(const state: Boolean);
begin
  if not state then
    begin
      Connecting := False;

      if Assigned(OnConnectResultState.OnCall) then
          OnConnectResultState.OnCall(state);
      if Assigned(OnConnectResultState.OnMethod) then
          OnConnectResultState.OnMethod(state);
      if Assigned(OnConnectResultState.OnProc) then
          OnConnectResultState.OnProc(state);
      OnConnectResultState.Init;
    end;

  PhysicsTunnel.PrintParam('DT Physics Connect %s', umlBoolToStr(state));
end;

procedure TDT_P2PVM_NoAuth_Client.DoAutomatedP2PVMClientConnectionDone(Sender: TCommunicationFramework; P_IO: TPeerIO);
begin
  DTClient.TunnelLinkM({$IFDEF FPC}@{$ENDIF FPC}DoTunnelLinkResult);
  PhysicsTunnel.Print('DT p2pVM done.');
end;

procedure TDT_P2PVM_NoAuth_Client.DoTunnelLinkResult(const state: Boolean);
begin
  if Assigned(OnConnectResultState.OnCall) then
      OnConnectResultState.OnCall(state);
  if Assigned(OnConnectResultState.OnMethod) then
      OnConnectResultState.OnMethod(state);
  if Assigned(OnConnectResultState.OnProc) then
      OnConnectResultState.OnProc(state);
  OnConnectResultState.Init;
  Connecting := False;

  if state then
    begin
      if AutomatedConnection then
          Reconnection := True;
    end;
end;

function TDT_P2PVM_NoAuth_Client.GetQuietMode: Boolean;
begin
  Result := RecvTunnel.QuietMode and SendTunnel.QuietMode and PhysicsTunnel.QuietMode;
end;

procedure TDT_P2PVM_NoAuth_Client.SetQuietMode(const Value: Boolean);
begin
  RecvTunnel.QuietMode := Value;
  SendTunnel.QuietMode := Value;
  PhysicsTunnel.QuietMode := Value;
end;

constructor TDT_P2PVM_NoAuth_Client.Create(ClientClass_: TDTClient_NoAuthClass);
begin
  inherited Create;
  OnConnectResultState.Init;
  Connecting := False;
  Reconnection := False;

  RecvTunnel := TCommunicationFrameworkWithP2PVM_Client.Create;
  RecvTunnel.QuietMode := True;

  SendTunnel := TCommunicationFrameworkWithP2PVM_Client.Create;
  SendTunnel.QuietMode := True;

  DTClient := ClientClass_.Create(RecvTunnel, SendTunnel);
  DTClient.RegisterCommand;
  DTClient.SwitchAsDefaultPerformance;

  PhysicsTunnel := TPhysicsClient.Create;
  PhysicsTunnel.QuietMode := True;
  PhysicsTunnel.AutomatedP2PVMBindClient.AddClient(SendTunnel, '::', 1);
  PhysicsTunnel.AutomatedP2PVMBindClient.AddClient(RecvTunnel, '::', 2);
  PhysicsTunnel.AutomatedP2PVMClient := True;
  PhysicsTunnel.AutomatedP2PVMClientDelayBoot := 0;

  LastAddr := '';
  LastPort := '';
  LastAuth := '';

  AutomatedConnection := True;

  RecvTunnel.PrefixName := 'NA';
  RecvTunnel.Name := 'R';
  SendTunnel.PrefixName := 'NA';
  SendTunnel.Name := 'S';
  PhysicsTunnel.PrefixName := 'Physics';
  PhysicsTunnel.Name := 'p2pVM';
end;

destructor TDT_P2PVM_NoAuth_Client.Destroy;
begin
  Disconnect;
  DisposeObject(RecvTunnel);
  DisposeObject(SendTunnel);
  DisposeObject(DTClient);
  DisposeObject(PhysicsTunnel);
  inherited Destroy;
end;

procedure TDT_P2PVM_NoAuth_Client.Progress;
begin
  DTClient.Progress;
  PhysicsTunnel.Progress;

  if (AutomatedConnection) and ((not PhysicsTunnel.Connected) or (not DTClient.LinkOk)) and (not Connecting) and (Reconnection) then
      Connect(LastAddr, LastPort, LastAuth);
end;

procedure TDT_P2PVM_NoAuth_Client.Connect(addr, Port, Auth: SystemString);
begin
  if Connecting then
      Exit;
  Connecting := True;
  if not Reconnection then
    begin
      LastAddr := addr;
      LastPort := Port;
      LastAuth := Auth;
    end;
  PhysicsTunnel.AutomatedP2PVMAuthToken := Auth;
  OnConnectResultState.Init;
  PhysicsTunnel.OnAutomatedP2PVMClientConnectionDone_M := {$IFDEF FPC}@{$ENDIF FPC}DoAutomatedP2PVMClientConnectionDone;
  PhysicsTunnel.AsyncConnectM(addr, umlStrToInt(Port), {$IFDEF FPC}@{$ENDIF FPC}DoConnectionResult);
end;

procedure TDT_P2PVM_NoAuth_Client.Connect_C(addr, Port, Auth: SystemString; OnResult: TStateCall);
begin
  if Connecting then
      Exit;
  Connecting := True;
  if not Reconnection then
    begin
      LastAddr := addr;
      LastPort := Port;
      LastAuth := Auth;
    end;
  PhysicsTunnel.AutomatedP2PVMAuthToken := Auth;
  OnConnectResultState.Init;
  OnConnectResultState.OnCall := OnResult;
  PhysicsTunnel.OnAutomatedP2PVMClientConnectionDone_M := {$IFDEF FPC}@{$ENDIF FPC}DoAutomatedP2PVMClientConnectionDone;
  PhysicsTunnel.AsyncConnectM(addr, umlStrToInt(Port), {$IFDEF FPC}@{$ENDIF FPC}DoConnectionResult);
end;

procedure TDT_P2PVM_NoAuth_Client.Connect_M(addr, Port, Auth: SystemString; OnResult: TStateMethod);
begin
  if Connecting then
      Exit;
  Connecting := True;
  if not Reconnection then
    begin
      LastAddr := addr;
      LastPort := Port;
      LastAuth := Auth;
    end;
  PhysicsTunnel.AutomatedP2PVMAuthToken := Auth;
  OnConnectResultState.Init;
  OnConnectResultState.OnMethod := OnResult;
  PhysicsTunnel.OnAutomatedP2PVMClientConnectionDone_M := {$IFDEF FPC}@{$ENDIF FPC}DoAutomatedP2PVMClientConnectionDone;
  PhysicsTunnel.AsyncConnectM(addr, umlStrToInt(Port), {$IFDEF FPC}@{$ENDIF FPC}DoConnectionResult);
end;

procedure TDT_P2PVM_NoAuth_Client.Connect_P(addr, Port, Auth: SystemString; OnResult: TStateProc);
begin
  if Connecting then
      Exit;
  Connecting := True;
  if not Reconnection then
    begin
      LastAddr := addr;
      LastPort := Port;
      LastAuth := Auth;
    end;
  PhysicsTunnel.AutomatedP2PVMAuthToken := Auth;
  OnConnectResultState.Init;
  OnConnectResultState.OnProc := OnResult;
  PhysicsTunnel.OnAutomatedP2PVMClientConnectionDone_M := {$IFDEF FPC}@{$ENDIF FPC}DoAutomatedP2PVMClientConnectionDone;
  PhysicsTunnel.AsyncConnectM(addr, umlStrToInt(Port), {$IFDEF FPC}@{$ENDIF FPC}DoConnectionResult);
end;

procedure TDT_P2PVM_NoAuth_Client.Disconnect;
begin
  Connecting := False;
  Reconnection := False;
  LastAddr := '';
  LastPort := '';
  LastAuth := '';
  PhysicsTunnel.Disconnect;
end;

function TDT_P2PVM_NoAuth_Custom_Service.GetQuietMode: Boolean;
begin
  Result := RecvTunnel.QuietMode and SendTunnel.QuietMode;
end;

procedure TDT_P2PVM_NoAuth_Custom_Service.SetQuietMode(const Value: Boolean);
begin
  RecvTunnel.QuietMode := Value;
  SendTunnel.QuietMode := Value;
end;

constructor TDT_P2PVM_NoAuth_Custom_Service.Create(ServiceClass_: TDTService_NoAuthClass; PhysicsTunnel_: TCommunicationFrameworkServer;
  P2PVM_Recv_Name_, P2PVM_Recv_IP6_, P2PVM_Recv_Port_,
  P2PVM_Send_Name_, P2PVM_Send_IP6_, P2PVM_Send_Port_: SystemString);
begin
  inherited Create;

  Bind_PhysicsTunnel := PhysicsTunnel_;
  Bind_P2PVM_Recv_IP6 := P2PVM_Recv_IP6_;
  Bind_P2PVM_Recv_Port := umlStrToInt(P2PVM_Recv_Port_);
  Bind_P2PVM_Send_IP6 := P2PVM_Send_IP6_;
  Bind_P2PVM_Send_Port := umlStrToInt(P2PVM_Send_Port_);

  RecvTunnel := TCommunicationFrameworkWithP2PVM_Server.Create;
  RecvTunnel.QuietMode := PhysicsTunnel_.QuietMode;
  RecvTunnel.PrefixName := 'NA';
  RecvTunnel.Name := P2PVM_Recv_Name_;

  SendTunnel := TCommunicationFrameworkWithP2PVM_Server.Create;
  SendTunnel.QuietMode := PhysicsTunnel_.QuietMode;
  SendTunnel.PrefixName := 'NA';
  SendTunnel.Name := P2PVM_Send_Name_;

  DTService := ServiceClass_.Create(RecvTunnel, SendTunnel);
  DTService.RegisterCommand;
  DTService.SwitchAsDefaultPerformance;

  Bind_PhysicsTunnel.AutomatedP2PVMServiceBind.AddService(RecvTunnel);
  Bind_PhysicsTunnel.AutomatedP2PVMServiceBind.AddService(SendTunnel);
  Bind_PhysicsTunnel.AutomatedP2PVMService := True;
  StartService();
end;

destructor TDT_P2PVM_NoAuth_Custom_Service.Destroy;
begin
  StopService;
  Bind_PhysicsTunnel.AutomatedP2PVMServiceBind.RemoveService(RecvTunnel);
  Bind_PhysicsTunnel.AutomatedP2PVMServiceBind.RemoveService(SendTunnel);
  DisposeObject(RecvTunnel);
  DisposeObject(SendTunnel);
  DisposeObject(DTService);
  inherited Destroy;
end;

procedure TDT_P2PVM_NoAuth_Custom_Service.Progress;
begin
  Bind_PhysicsTunnel.Progress;
  DTService.Progress;
end;

procedure TDT_P2PVM_NoAuth_Custom_Service.StartService;
begin
  RecvTunnel.StartService(Bind_P2PVM_Recv_IP6, Bind_P2PVM_Recv_Port);
  SendTunnel.StartService(Bind_P2PVM_Send_IP6, Bind_P2PVM_Send_Port);
end;

procedure TDT_P2PVM_NoAuth_Custom_Service.StopService;
begin
  RecvTunnel.StopService;
  RecvTunnel.StopService;
end;

function TDT_P2PVM_NoAuth_Custom_Client.GetQuietMode: Boolean;
begin
  Result := RecvTunnel.QuietMode and SendTunnel.QuietMode;
end;

procedure TDT_P2PVM_NoAuth_Custom_Client.SetQuietMode(const Value: Boolean);
begin
  RecvTunnel.QuietMode := Value;
  SendTunnel.QuietMode := Value;
end;

constructor TDT_P2PVM_NoAuth_Custom_Client.Create(ClientClass_: TDTClient_NoAuthClass; PhysicsTunnel_: TCommunicationFrameworkClient;
  P2PVM_Recv_Name_, P2PVM_Recv_IP6_, P2PVM_Recv_Port_,
  P2PVM_Send_Name_, P2PVM_Send_IP6_, P2PVM_Send_Port_: SystemString);
begin
  inherited Create;
  // internal
  OnConnectResultState.Init;
  Connecting := False;
  Reconnection := False;

  // bind
  Bind_PhysicsTunnel := PhysicsTunnel_;
  Bind_P2PVM_Recv_IP6 := P2PVM_Recv_IP6_;
  Bind_P2PVM_Recv_Port := umlStrToInt(P2PVM_Recv_Port_);
  Bind_P2PVM_Send_IP6 := P2PVM_Send_IP6_;
  Bind_P2PVM_Send_Port := umlStrToInt(P2PVM_Send_Port_);

  // local
  RecvTunnel := TCommunicationFrameworkWithP2PVM_Client.Create;
  RecvTunnel.QuietMode := PhysicsTunnel_.QuietMode;
  RecvTunnel.PrefixName := 'NA';
  RecvTunnel.Name := P2PVM_Recv_Name_;
  SendTunnel := TCommunicationFrameworkWithP2PVM_Client.Create;
  SendTunnel.QuietMode := PhysicsTunnel_.QuietMode;
  SendTunnel.PrefixName := 'NA';
  SendTunnel.Name := P2PVM_Send_Name_;
  DTClient := ClientClass_.Create(RecvTunnel, SendTunnel);
  DTClient.RegisterCommand;
  DTClient.SwitchAsDefaultPerformance;
  AutomatedConnection := True;
  OnTunnelLink := nil;

  // automated p2pVM
  Bind_PhysicsTunnel.AutomatedP2PVMBindClient.AddClient(RecvTunnel, Bind_P2PVM_Recv_IP6, Bind_P2PVM_Recv_Port);
  Bind_PhysicsTunnel.AutomatedP2PVMBindClient.AddClient(SendTunnel, Bind_P2PVM_Send_IP6, Bind_P2PVM_Send_Port);
  Bind_PhysicsTunnel.AutomatedP2PVMClient := True;
  Bind_PhysicsTunnel.AutomatedP2PVMClientDelayBoot := 0;
end;

destructor TDT_P2PVM_NoAuth_Custom_Client.Destroy;
begin
  Disconnect;
  DisposeObject(RecvTunnel);
  DisposeObject(SendTunnel);
  DisposeObject(DTClient);
  inherited Destroy;
end;

procedure TDT_P2PVM_NoAuth_Custom_Client.Progress;
begin
  Bind_PhysicsTunnel.Progress;
  DTClient.Progress;
  if (AutomatedConnection) and (Bind_PhysicsTunnel.RemoteInited) and (Bind_PhysicsTunnel.AutomatedP2PVMClientConnectionDone(Bind_PhysicsTunnel.ClientIO))
    and (not Connecting) and (Reconnection) and (not DTClient.LinkOk) then
      Connect();
end;

procedure TDT_P2PVM_NoAuth_Custom_Client.DoTunnelLinkResult(const state: Boolean);
begin
  if Assigned(OnConnectResultState.OnCall) then
      OnConnectResultState.OnCall(state);
  if Assigned(OnConnectResultState.OnMethod) then
      OnConnectResultState.OnMethod(state);
  if Assigned(OnConnectResultState.OnProc) then
      OnConnectResultState.OnProc(state);
  OnConnectResultState.Init;
  Connecting := False;

  if state then
    begin
      if AutomatedConnection then
          Reconnection := True;

      if Assigned(OnTunnelLink) then
          OnTunnelLink(Self);
    end;
end;

procedure TDT_P2PVM_NoAuth_Custom_Client.AutoCheckPhysicsTunnelAndConnect;
begin
  AutomatedConnection := True;
  Reconnection := True;
end;

procedure TDT_P2PVM_NoAuth_Custom_Client.Connect;
begin
  if Connecting then
      Exit;
  Connecting := True;
  if not Bind_PhysicsTunnel.RemoteInited then
    begin
      Connecting := False;
      Exit;
    end;
  OnConnectResultState.Init;
  DTClient.TunnelLinkM({$IFDEF FPC}@{$ENDIF FPC}DoTunnelLinkResult);
end;

procedure TDT_P2PVM_NoAuth_Custom_Client.Connect_C(OnResult: TStateCall);
begin
  if Connecting then
      Exit;
  Connecting := True;
  if not Bind_PhysicsTunnel.RemoteInited then
    begin
      Connecting := False;
      OnResult(False);
      Exit;
    end;
  OnConnectResultState.Init;
  OnConnectResultState.OnCall := OnResult;
  DTClient.TunnelLinkM({$IFDEF FPC}@{$ENDIF FPC}DoTunnelLinkResult);
end;

procedure TDT_P2PVM_NoAuth_Custom_Client.Connect_M(OnResult: TStateMethod);
begin
  if Connecting then
      Exit;
  Connecting := True;
  if not Bind_PhysicsTunnel.RemoteInited then
    begin
      Connecting := False;
      OnResult(False);
      Exit;
    end;
  OnConnectResultState.Init;
  OnConnectResultState.OnMethod := OnResult;
  DTClient.TunnelLinkM({$IFDEF FPC}@{$ENDIF FPC}DoTunnelLinkResult);
end;

procedure TDT_P2PVM_NoAuth_Custom_Client.Connect_P(OnResult: TStateProc);
begin
  if Connecting then
      Exit;
  Connecting := True;
  if not Bind_PhysicsTunnel.RemoteInited then
    begin
      Connecting := False;
      OnResult(False);
      Exit;
    end;
  OnConnectResultState.Init;
  OnConnectResultState.OnProc := OnResult;
  DTClient.TunnelLinkM({$IFDEF FPC}@{$ENDIF FPC}DoTunnelLinkResult);
end;

procedure TDT_P2PVM_NoAuth_Custom_Client.Disconnect;
begin
  Connecting := False;
  Reconnection := False;
  DTClient.Disconnect;
end;

end.
