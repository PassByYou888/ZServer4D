{ ****************************************************************************** }
{ * communication framework written by QQ 600585@qq.com                        * }
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
  2018-9-11 vm auth model, user protocol model
  2017-12-6 added TBigStreamBatchList
  2017-11-28  support anonymous function
*)

unit CommunicationFramework;

{$INCLUDE zDefine.inc}


interface

uses Classes, SysUtils, Variants, TypInfo,
  CoreClasses, ListEngine, UnicodeMixedLib, DoStatusIO,
  DataFrameEngine, MemoryStream64, PascalStrings, CoreCipher, NotifyObjectBase, Cadencer;

type
  TPeerIO = class;

  TIPV4 = array [0 .. 3] of Byte;
  PIPV4 = ^TIPV4;

  TIPV6 = array [0 .. 7] of Word;
  PIPV6 = ^TIPV6;

  TConsoleMethod = procedure(Sender: TPeerIO; ResultData: SystemString) of object;
  TStreamMethod = procedure(Sender: TPeerIO; ResultData: TDataFrameEngine) of object;
  TStreamParamMethod = procedure(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, ResultData: TDataFrameEngine) of object;

  TStateCall = procedure(const State: Boolean);
  TStateMethod = procedure(const State: Boolean) of object;

  TNotifyCall = procedure();
  TNotifyMethod = procedure() of object;

{$IFNDEF FPC}
  TConsoleProc = reference to procedure(Sender: TPeerIO; ResultData: SystemString);
  TStreamProc = reference to procedure(Sender: TPeerIO; ResultData: TDataFrameEngine);
  TStreamParamProc = reference to procedure(Sender: TPeerIO; Param1: Pointer; Param2: TObject; InData, ResultData: TDataFrameEngine);
  TStateProc = reference to procedure(const State: Boolean);
  TNotifyProc = reference to procedure();
{$ENDIF FPC}
  TQueueState = (qsUnknow, qsSendConsoleCMD, qsSendStreamCMD, qsSendDirectConsoleCMD, qsSendDirectStreamCMD, qsSendBigStream, qsSendCompleteBuffer);

  TQueueData = record
    State: TQueueState;
    IO_ID: Cardinal;
    Cmd: SystemString;
    Cipher: TCipherSecurity;
    //
    ConsoleData: SystemString;
    OnConsoleMethod: TConsoleMethod;
{$IFNDEF FPC} OnConsoleProc: TConsoleProc; {$ENDIF FPC}
    //
    StreamData: TCoreClassStream;
    OnStreamMethod: TStreamMethod;
    OnStreamParamMethod: TStreamParamMethod;
{$IFNDEF FPC}
    OnStreamProc: TStreamProc;
    OnStreamParamProc: TStreamParamProc;
{$ENDIF FPC}
    //
    BigStreamStartPos: Int64;
    BigStream: TCoreClassStream;

    buffer: PByte;
    BufferSize: NativeInt;

    DoneAutoFree: Boolean;

    Param1: Pointer;
    Param2: TObject;
  end;

  PQueueData = ^TQueueData;

  TCommandStreamCall = procedure(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
  TCommandConsoleCall = procedure(Sender: TPeerIO; InData: SystemString; var OutData: SystemString);
  TCommandDirectStreamCall = procedure(Sender: TPeerIO; InData: TDataFrameEngine);
  TCommandDirectConsoleCall = procedure(Sender: TPeerIO; InData: SystemString);
  TCommandBigStreamCall = procedure(Sender: TPeerIO; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64);
  TCommandCompleteBufferCall = procedure(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);

  TCommandStreamMethod = procedure(Sender: TPeerIO; InData, OutData: TDataFrameEngine) of object;
  TCommandConsoleMethod = procedure(Sender: TPeerIO; InData: SystemString; var OutData: SystemString) of object;
  TCommandDirectStreamMethod = procedure(Sender: TPeerIO; InData: TDataFrameEngine) of object;
  TCommandDirectConsoleMethod = procedure(Sender: TPeerIO; InData: SystemString) of object;
  TCommandBigStreamMethod = procedure(Sender: TPeerIO; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64) of object;
  TCommandCompleteBufferMethod = procedure(Sender: TPeerIO; InData: PByte; DataSize: NativeInt) of object;

{$IFNDEF FPC}
  TCommandStreamProc = reference to procedure(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
  TCommandConsoleProc = reference to procedure(Sender: TPeerIO; InData: SystemString; var OutData: SystemString);
  TCommandDirectStreamProc = reference to procedure(Sender: TPeerIO; InData: TDataFrameEngine);
  TCommandDirectConsoleProc = reference to procedure(Sender: TPeerIO; InData: SystemString);
  TCommandBigStreamProc = reference to procedure(Sender: TPeerIO; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64);
  TCommandCompleteBufferProc = reference to procedure(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
{$ENDIF FPC}

  TCommandStream = class(TCoreClassInterfacedObject)
  protected
    FOnExecuteCall: TCommandStreamCall;
    FOnExecuteMethod: TCommandStreamMethod;
{$IFNDEF FPC} FOnExecuteProc: TCommandStreamProc; {$ENDIF FPC}
  public
    constructor Create;
    destructor Destroy; override;

    function Execute(Sender: TPeerIO; InData, OutData: TDataFrameEngine): Boolean;
    property OnExecute: TCommandStreamMethod read FOnExecuteMethod write FOnExecuteMethod;
    property OnExecuteCall: TCommandStreamCall read FOnExecuteCall write FOnExecuteCall;
    property OnExecuteMethod: TCommandStreamMethod read FOnExecuteMethod write FOnExecuteMethod;
{$IFNDEF FPC} property OnExecuteProc: TCommandStreamProc read FOnExecuteProc write FOnExecuteProc; {$ENDIF FPC}
  end;

  TCommandConsole = class(TCoreClassInterfacedObject)
  protected
    FOnExecuteCall: TCommandConsoleCall;
    FOnExecuteMethod: TCommandConsoleMethod;
{$IFNDEF FPC} FOnExecuteProc: TCommandConsoleProc; {$ENDIF FPC}
  public
    constructor Create;
    destructor Destroy; override;

    function Execute(Sender: TPeerIO; InData: SystemString; var OutData: SystemString): Boolean;
    property OnExecute: TCommandConsoleMethod read FOnExecuteMethod write FOnExecuteMethod;
    property OnExecuteCall: TCommandConsoleCall read FOnExecuteCall write FOnExecuteCall;
    property OnExecuteMethod: TCommandConsoleMethod read FOnExecuteMethod write FOnExecuteMethod;
{$IFNDEF FPC} property OnExecuteProc: TCommandConsoleProc read FOnExecuteProc write FOnExecuteProc; {$ENDIF FPC}
  end;

  TCommandDirectStream = class(TCoreClassInterfacedObject)
  protected
    FOnExecuteCall: TCommandDirectStreamCall;
    FOnExecuteMethod: TCommandDirectStreamMethod;
{$IFNDEF FPC} FOnExecuteProc: TCommandDirectStreamProc; {$ENDIF FPC}
  public
    constructor Create;
    destructor Destroy; override;

    function Execute(Sender: TPeerIO; InData: TDataFrameEngine): Boolean;
    property OnExecute: TCommandDirectStreamMethod read FOnExecuteMethod write FOnExecuteMethod;
    property OnExecuteCall: TCommandDirectStreamCall read FOnExecuteCall write FOnExecuteCall;
    property OnExecuteMethod: TCommandDirectStreamMethod read FOnExecuteMethod write FOnExecuteMethod;
{$IFNDEF FPC} property OnExecuteProc: TCommandDirectStreamProc read FOnExecuteProc write FOnExecuteProc; {$ENDIF FPC}
  end;

  TCommandDirectConsole = class(TCoreClassInterfacedObject)
  protected
    FOnExecuteCall: TCommandDirectConsoleCall;
    FOnExecuteMethod: TCommandDirectConsoleMethod;
{$IFNDEF FPC} FOnExecuteProc: TCommandDirectConsoleProc; {$ENDIF FPC}
  public
    constructor Create;
    destructor Destroy; override;

    function Execute(Sender: TPeerIO; InData: SystemString): Boolean;
    property OnExecute: TCommandDirectConsoleMethod read FOnExecuteMethod write FOnExecuteMethod;
    property OnExecuteCall: TCommandDirectConsoleCall read FOnExecuteCall write FOnExecuteCall;
    property OnExecuteMethod: TCommandDirectConsoleMethod read FOnExecuteMethod write FOnExecuteMethod;
{$IFNDEF FPC} property OnExecuteProc: TCommandDirectConsoleProc read FOnExecuteProc write FOnExecuteProc; {$ENDIF FPC}
  end;

  TCommandBigStream = class(TCoreClassInterfacedObject)
  protected
    FOnExecuteCall: TCommandBigStreamCall;
    FOnExecuteMethod: TCommandBigStreamMethod;
{$IFNDEF FPC} FOnExecuteProc: TCommandBigStreamProc; {$ENDIF FPC}
  public
    constructor Create;
    destructor Destroy; override;

    function Execute(Sender: TPeerIO; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64): Boolean;
    property OnExecute: TCommandBigStreamMethod read FOnExecuteMethod write FOnExecuteMethod;
    property OnExecuteCall: TCommandBigStreamCall read FOnExecuteCall write FOnExecuteCall;
    property OnExecuteMethod: TCommandBigStreamMethod read FOnExecuteMethod write FOnExecuteMethod;
{$IFNDEF FPC} property OnExecuteProc: TCommandBigStreamProc read FOnExecuteProc write FOnExecuteProc; {$ENDIF FPC}
  end;

  TCommandCompleteBuffer = class(TCoreClassInterfacedObject)
  protected
    FOnExecuteCall: TCommandCompleteBufferCall;
    FOnExecuteMethod: TCommandCompleteBufferMethod;
{$IFNDEF FPC} FOnExecuteProc: TCommandCompleteBufferProc; {$ENDIF FPC}
  public
    constructor Create;
    destructor Destroy; override;

    function Execute(Sender: TPeerIO; InData: PByte; DataSize: NativeInt): Boolean;
    property OnExecute: TCommandCompleteBufferMethod read FOnExecuteMethod write FOnExecuteMethod;
    property OnExecuteCall: TCommandCompleteBufferCall read FOnExecuteCall write FOnExecuteCall;
    property OnExecuteMethod: TCommandCompleteBufferMethod read FOnExecuteMethod write FOnExecuteMethod;
{$IFNDEF FPC} property OnExecuteProc: TCommandCompleteBufferProc read FOnExecuteProc write FOnExecuteProc; {$ENDIF FPC}
  end;

  TCommunicationFramework = class;

  PBigStreamBatchPostData = ^TBigStreamBatchPostData;

  TBigStreamBatchPostData = record
    Source: TMemoryStream64;
    CompletedBackcallPtr: UInt64;
    RemoteMD5: UnicodeMixedLib.TMD5;
    SourceMD5: UnicodeMixedLib.TMD5;
    index: Integer;
    DBStorePos: Int64;

    procedure Init;
    procedure Encode(d: TDataFrameEngine);
    procedure Decode(d: TDataFrameEngine);
  end;

  TBigStreamBatchList = class(TCoreClassObject)
  protected
    FOwner: TPeerIO;
    FList: TCoreClassList;
    function GetItems(const index: Integer): PBigStreamBatchPostData;
  public
    constructor Create(AOwner: TPeerIO);
    destructor Destroy; override;

    procedure Clear;
    function Count: Integer;
    property Items[const index: Integer]: PBigStreamBatchPostData read GetItems; default;
    function NewPostData: PBigStreamBatchPostData;
    function First: PBigStreamBatchPostData;
    function Last: PBigStreamBatchPostData;
    procedure DeleteLast;
    procedure Delete(const index: Integer);
  end;

  TPeerIOUserDefine = class(TCoreClassInterfacedObject)
  protected
    FOwner: TPeerIO;
    FWorkPlatform: TExecutePlatform;
    FBigStreamBatchList: TBigStreamBatchList;
  public
    constructor Create(AOwner: TPeerIO); virtual;
    destructor Destroy; override;

    procedure Progress; virtual;

    property Owner: TPeerIO read FOwner;
    property WorkPlatform: TExecutePlatform read FWorkPlatform;
    property BigStreamBatchList: TBigStreamBatchList read FBigStreamBatchList;
  end;

  TPeerIOUserDefineClass = class of TPeerIOUserDefine;

  TPeerIOUserSpecial = class(TCoreClassInterfacedObject)
  protected
    FOwner: TPeerIO;
  public
    constructor Create(AOwner: TPeerIO); virtual;
    destructor Destroy; override;

    procedure Progress; virtual;

    property Owner: TPeerIO read FOwner;
  end;

  TPeerIOUserSpecialClass = class of TPeerIOUserSpecial;

  TPeerClientUserDefine = class(TPeerIOUserDefine);
  TPeerClientUserSpecial = class(TPeerIOUserSpecial);

  TInternalSendByteBuffer = procedure(const Sender: TPeerIO; const buff: PByte; siz: NativeInt) of object;
  TInternalSaveReceiveBuffer = procedure(const Sender: TPeerIO; const buff: Pointer; siz: Int64) of object;
  TInternalProcessReceiveBuffer = procedure(const Sender: TPeerIO; const ACurrentActiveThread: TCoreClassThread; const RecvSync, SendSync: Boolean) of object;
  TInternalProcessAllSendCmd = procedure(const Sender: TPeerIO; const ACurrentActiveThread: TCoreClassThread; const RecvSync, SendSync: Boolean) of object;
  TInternalClientCreate = procedure(const Sender: TPeerIO) of object;
  TInternalClientDestory = procedure(const Sender: TPeerIO) of object;

  TCommunicationFrameworkWithP2PVM = class;
  TCommunicationFrameworkWithP2PVM_Server = class;
  TCommunicationFrameworkWithP2PVM_Client = class;

  TPeerIO = class(TCoreClassInterfacedObject)
  protected
    FLockedObject: TCoreClassObject;
    FOwnerFramework: TCommunicationFramework;
    FClientIntf: TCoreClassObject;
    FID: Cardinal;
    FHeadToken, FTailToken: Cardinal;
    FConsoleToken: Byte;
    FStreamToken: Byte;
    FDirectConsoleToken: Byte;
    FDirectStreamToken: Byte;
    FBigStreamToken: Byte;
    FCompleteBufferToken: Byte;
    FReceivedBuffer: TMemoryStream64;
    FReceivedBuffer_Busy: TMemoryStream64;
    FBigStreamReceiveProcessing: Boolean;
    FBigStreamTotal: Int64;
    FBigStreamCompleted: Int64;
    FBigStreamCmd: SystemString;
    FBigStreamReceive: TCoreClassStream;
    FBigStreamSending: TCoreClassStream;
    FBigStreamSendState: Int64;
    FBigStreamSendDoneTimeFree: Boolean;
    FCompleteBufferReceiveProcessing: Boolean;
    FCompleteBufferTotal: Cardinal;
    FCompleteBufferCompressedSize: Cardinal;
    FCompleteBufferCompleted: Cardinal;
    FCompleteBufferCmd: SystemString;
    FCompleteBufferReceiveStream: TMemoryStream64;
    FCurrentQueueData: PQueueData;
    FWaitOnResult: Boolean;
    FCurrentPauseResultSend_CommDataType: Byte;
    FCanPauseResultSend: Boolean;
    FPauseResultSend: Boolean;
    FReceiveTriggerRuning: Boolean;
    FReceiveDataCipherSecurity: TCipherSecurity;
    FResultDataBuffer: TMemoryStream64;
    FSendDataCipherSecurity: TCipherSecurity;
    FAllSendProcessing: Boolean;
    FReceiveProcessing: Boolean;
    FQueueList: TCoreClassList;
    FLastCommunicationTime: TTimeTickValue;
    FCipherKey: TCipherKeyBuffer;
    FRemoteExecutedForConnectInit: Boolean;
    FInCmd: SystemString;
    FInText, FOutText: SystemString;
    FInDataFrame, FOutDataFrame: TDataFrameEngine;
    ResultText: SystemString;
    ResultDataFrame: TDataFrameEngine;
    FSyncPick: PQueueData;
    FWaitSendBusy: Boolean;
    FReceiveCommandRuning: Boolean;
    FReceiveResultRuning: Boolean;
    FProgressRunning: Boolean;
    FTimeOutProcessDone: Boolean;
  protected
    // private vm and protocol stack support
    FP2PVMTunnel: TCommunicationFrameworkWithP2PVM;
    // vm auth token buffer
    FP2PAuthToken: TBytes;
    // vm hook
    OnInternalSendByteBuffer: TInternalSendByteBuffer;
    OnInternalSaveReceiveBuffer: TInternalSaveReceiveBuffer;
    OnInternalProcessReceiveBuffer: TInternalProcessReceiveBuffer;
    OnInternalProcessAllSendCmd: TInternalProcessAllSendCmd;
    OnCreate: TInternalClientCreate;
    OnDestroy: TInternalClientDestory;
  protected
    // p2p vm: auth model result
    OnVMBuildAuthModelResultCall: TNotifyCall;
    OnVMBuildAuthModelResultMethod: TNotifyMethod;
{$IFNDEF FPC} OnVMBuildAuthModelResultProc: TNotifyProc; {$ENDIF FPC}
    // p2p vm: auth result
    OnVMAuthResultCall: TStateCall;
    OnVMAuthResultMethod: TStateMethod;
{$IFNDEF FPC} OnVMAuthResultProc: TStateProc; {$ENDIF FPC}
    procedure P2PVMAuthSuccess(Sender: TCommunicationFrameworkWithP2PVM);
  protected
    // user
    FUserData: Pointer;
    FUserValue: Variant;
    FUserVariants: THashVariantList;
    FUserObjects: THashObjectList;
    FUserAutoFreeObjects: THashObjectList;
    FUserDefine: TPeerIOUserDefine;
    FUserSpecial: TPeerIOUserSpecial;

    function GetUserVariants: THashVariantList;
    function GetUserObjects: THashObjectList;
    function GetUserAutoFreeObjects: THashObjectList;
  protected
    procedure InternalSendByteBuffer(const buff: PByte; siz: NativeInt);

    procedure SendInteger(v: Integer);
    procedure SendCardinal(v: Cardinal);
    procedure SendInt64(v: Int64);
    procedure SendByte(v: Byte);
    procedure SendWord(v: Word);
    procedure SendVerifyCode(buff: Pointer; siz: NativeInt);
    procedure SendEncryptBuffer(buff: PByte; siz: NativeInt; cs: TCipherSecurity);
    procedure SendEncryptMemoryStream(stream: TMemoryStream64; cs: TCipherSecurity);

    procedure InternalSendConsoleBuff(buff: TMemoryStream64; cs: TCipherSecurity);
    procedure InternalSendStreamBuff(buff: TMemoryStream64; cs: TCipherSecurity);
    procedure InternalSendDirectConsoleBuff(buff: TMemoryStream64; cs: TCipherSecurity);
    procedure InternalSendDirectStreamBuff(buff: TMemoryStream64; cs: TCipherSecurity);
    procedure InternalSendBigStreamHeader(Cmd: SystemString; streamSiz: Int64);
    procedure InternalSendBigStreamBuff(var Queue: TQueueData);
    procedure InternalSendCompleteBufferHeader(Cmd: SystemString; buffSiz, compSiz: Cardinal);
    procedure InternalSendCompleteBufferBuff(var Queue: TQueueData);

    procedure Sync_InternalSendResultData;
    procedure Sync_InternalSendConsoleCmd;
    procedure Sync_InternalSendStreamCmd;
    procedure Sync_InternalSendDirectConsoleCmd;
    procedure Sync_InternalSendDirectStreamCmd;
    procedure Sync_InternalSendBigStreamCmd;
    procedure Sync_InternalSendCompleteBufferCmd;

    procedure Sync_ExecuteConsole;
    procedure Sync_ExecuteStream;
    procedure Sync_ExecuteDirectConsole;
    procedure Sync_ExecuteDirectStream;
    procedure Sync_SendConsoleResult;
    procedure Sync_SendStreamResult;
    procedure ExecuteDataFrame(ACurrentActiveThread: TCoreClassThread; const RecvSync, SendSync: Boolean; CommDataType: Byte; DataFrame: TDataFrameEngine);

    procedure Sync_ExecuteBigStream;
    function FillBigStreamBuffer(ACurrentActiveThread: TCoreClassThread; const Sync: Boolean): Boolean;

    procedure Sync_ExecuteCompleteBuffer;
    function FillCompleteBufferBuffer(ACurrentActiveThread: TCoreClassThread; const Sync: Boolean): Boolean;

    procedure Sync_ExecuteResult;
    function FillWaitOnResultBuffer(ACurrentActiveThread: TCoreClassThread; const RecvSync, SendSync: Boolean): Boolean;

    procedure InternalSaveReceiveBuffer(const buff: Pointer; siz: Int64);
    procedure InternalProcessReceiveBuffer(const ACurrentActiveThread: TCoreClassThread; const RecvSync, SendSync: Boolean);
    procedure InternalProcessAllSendCmd(const ACurrentActiveThread: TCoreClassThread; const RecvSync, SendSync: Boolean);

    procedure InternalCloseP2PVMTunnel;
  public
    // external interface
    function Connected: Boolean; virtual; abstract;
    procedure Disconnect; virtual; abstract;
    procedure SendByteBuffer(const buff: PByte; const Size: NativeInt); virtual; abstract;
    procedure WriteBufferOpen; virtual; abstract;
    procedure WriteBufferFlush; virtual; abstract;
    procedure WriteBufferClose; virtual; abstract;
    function GetPeerIP: SystemString; virtual; abstract;
    function WriteBufferEmpty: Boolean; virtual;
  public
    constructor Create(AOwnerFramework: TCommunicationFramework; AClientIntf: TCoreClassObject);
    procedure CreateAfter; virtual;
    destructor Destroy; override;

    { p2pVM Tunnel support }
    property p2pVM: TCommunicationFrameworkWithP2PVM read FP2PVMTunnel;
    property p2pVMTunnel: TCommunicationFrameworkWithP2PVM read FP2PVMTunnel;
    { p2pVM build safe Auth token }
    procedure BuildP2PAuthToken; overload;
    procedure BuildP2PAuthTokenC(const OnResult: TNotifyCall); overload;
    procedure BuildP2PAuthTokenM(const OnResult: TNotifyMethod); overload;
{$IFNDEF FPC} procedure BuildP2PAuthTokenP(const OnResult: TNotifyProc); overload; {$ENDIF FPC}
    { p2pVM Open Tunnel }
    procedure OpenP2PVMTunnel(vmHashPoolLen: Integer; SendRemoteRequest: Boolean; const AuthToken: SystemString); overload;
    procedure OpenP2PVMTunnel(SendRemoteRequest: Boolean; const AuthToken: SystemString); overload;
    procedure OpenP2PVMTunnelC(SendRemoteRequest: Boolean; const AuthToken: SystemString; OnResult: TStateCall); overload;
    procedure OpenP2PVMTunnelM(SendRemoteRequest: Boolean; const AuthToken: SystemString; OnResult: TStateMethod); overload;
{$IFNDEF FPC} procedure OpenP2PVMTunnelP(SendRemoteRequest: Boolean; const AuthToken: SystemString; OnResult: TStateProc); overload; {$ENDIF FPC}
    procedure OpenP2PVMTunnelC(vmHashPoolLen: Integer; SendRemoteRequest: Boolean; const AuthToken: SystemString; OnResult: TStateCall); overload;
    procedure OpenP2PVMTunnelM(vmHashPoolLen: Integer; SendRemoteRequest: Boolean; const AuthToken: SystemString; OnResult: TStateMethod); overload;
{$IFNDEF FPC} procedure OpenP2PVMTunnelP(vmHashPoolLen: Integer; SendRemoteRequest: Boolean; const AuthToken: SystemString; OnResult: TStateProc); overload; {$ENDIF FPC}
    procedure OpenP2PVMTunnel; overload;
    { p2pVM Close Tunnel }
    procedure CloseP2PVMTunnel;

    procedure PrintError(v: SystemString);
    procedure Print(v: SystemString); overload;
    procedure Print(v: SystemString; const Args: array of const); overload;
    procedure PrintCommand(v: SystemString; Args: SystemString);
    procedure PrintParam(v: SystemString; Args: SystemString);

    { asynchronous io }
    procedure LockIO;
    procedure UnLockIO;
    procedure IO_SyncMethod(t: TCoreClassThread; Sync: Boolean; proc: TThreadMethod);

    { progress IO }
    procedure Progress; virtual;
    { delay close on no Data activted time }
    procedure DelayClose; overload;
    { delay close on custom delay of ms time }
    procedure DelayClose(const t: double); overload;
    { delay free on custom delay of ms time }
    procedure DelayFree; overload;
    procedure DelayFree(const t: double); overload;
    //
    procedure SaveReceiveBuffer(const p: Pointer; siz: Int64);
    procedure FillRecvBuffer(const ACurrentActiveThread: TCoreClassThread; const RecvSync, SendSync: Boolean);
    procedure ProcessAllSendCmd(const ACurrentActiveThread: TCoreClassThread; const RecvSync, SendSync: Boolean);
    procedure PostQueueData(p: PQueueData);

    // custom protocol
    procedure WriteCustomBuffer(const buffer: PByte; const Size: NativeInt);

    // delay reponse
    procedure PauseResultSend; virtual;
    procedure ContinueResultSend; virtual;
    // ContinueResultSend use it
    property InText: SystemString read FInText;
    property OutText: SystemString read FOutText write FOutText;
    property InDataFrame: TDataFrameEngine read FInDataFrame;
    property OutDataFrame: TDataFrameEngine read FOutDataFrame;
    function ResultSendIsPaused: Boolean;

    // state
    property CurrentBigStreamCommand: SystemString read FBigStreamCmd;
    property CurrentCommand: SystemString read FInCmd;
    property WaitOnResult: Boolean read FWaitOnResult;
    property AllSendProcessing: Boolean read FAllSendProcessing;
    property BigStreamReceiveing: Boolean read FBigStreamReceiveProcessing;
    property WaitSendBusy: Boolean read FWaitSendBusy;
    property ReceiveProcessing: Boolean read FReceiveProcessing;
    property ReceiveCommandRuning: Boolean read FReceiveCommandRuning;
    property ReceiveResultRuning: Boolean read FReceiveResultRuning;
    function GetBigStreamReceiveState(var Total, Complete: Int64): Boolean;
    function GetBigStreamSendingState(var Total, Complete: Int64): Boolean;
    //
    // framework
    property OwnerFramework: TCommunicationFramework read FOwnerFramework;
    property ClientIntf: TCoreClassObject read FClientIntf write FClientIntf;
    property ID: Cardinal read FID;
    property CipherKey: TCipherKeyBuffer read FCipherKey;
    function CipherKeyPtr: PCipherKeyBuffer;
    property SendCipherSecurity: TCipherSecurity read FSendDataCipherSecurity write FSendDataCipherSecurity;
    property RemoteExecutedForConnectInit: Boolean read FRemoteExecutedForConnectInit write FRemoteExecutedForConnectInit;

    // remote
    property PeerIP: SystemString read GetPeerIP;

    // user define
    property UserVariants: THashVariantList read GetUserVariants;
    property UserObjects: THashObjectList read GetUserObjects;
    property UserAutoFreeObjects: THashObjectList read GetUserAutoFreeObjects;
    property UserData: Pointer read FUserData write FUserData;
    property UserValue: Variant read FUserValue write FUserValue;
    property UserDefine: TPeerIOUserDefine read FUserDefine;
    property UserSpecial: TPeerIOUserSpecial read FUserSpecial;

    // hash code
    procedure GenerateHashCode(const hs: THashSecurity; buff: Pointer; siz: Integer; var output: TBytes);
    function VerifyHashCode(const hs: THashSecurity; buff: Pointer; siz: Integer; var Code: TBytes): Boolean;
    //
    // encrypt
    procedure Encrypt(cs: TCipherSecurity; DataPtr: Pointer; Size: Cardinal; var k: TCipherKeyBuffer; enc: Boolean);
    //
    // timeout
    function StopCommunicationTime: TTimeTickValue;
    procedure UpdateLastCommunicationTime;
    property LastCommunicationTime: TTimeTickValue read FLastCommunicationTime;
    //
    // queue data
    property CurrentQueueData: PQueueData read FCurrentQueueData;

    // send cmd and result method
    procedure SendConsoleCmdM(Cmd: SystemString; ConsoleData: SystemString; OnResult: TConsoleMethod); overload;
    procedure SendStreamCmdM(Cmd: SystemString; StreamData: TCoreClassStream; OnResult: TStreamMethod; DoneAutoFree: Boolean); overload;
    procedure SendStreamCmdM(Cmd: SystemString; StreamData: TDataFrameEngine; OnResult: TStreamMethod); overload;
    procedure SendStreamCmdM(Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamMethod); overload;

    // send cmd and result proc
{$IFNDEF FPC}
    procedure SendConsoleCmdP(Cmd: SystemString; ConsoleData: SystemString; OnResult: TConsoleProc); overload;
    procedure SendStreamCmdP(Cmd: SystemString; StreamData: TCoreClassStream; OnResult: TStreamProc; DoneAutoFree: Boolean); overload;
    procedure SendStreamCmdP(Cmd: SystemString; StreamData: TDataFrameEngine; OnResult: TStreamProc); overload;
    procedure SendStreamCmdP(Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamProc); overload;
{$ENDIF FPC}
    //
    // direct send cmd
    procedure SendDirectConsoleCmd(Cmd: SystemString; ConsoleData: SystemString);
    procedure SendDirectStreamCmd(Cmd: SystemString; StreamData: TCoreClassStream; DoneAutoFree: Boolean); overload;
    procedure SendDirectStreamCmd(Cmd: SystemString; StreamData: TDataFrameEngine); overload;
    procedure SendDirectStreamCmd(Cmd: SystemString); overload;

    // wait send cmd
    function WaitSendConsoleCmd(Cmd: SystemString; ConsoleData: SystemString; Timeout: TTimeTickValue): SystemString;
    procedure WaitSendStreamCmd(Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; Timeout: TTimeTickValue);

    // send bigstream
    procedure SendBigStream(Cmd: SystemString; BigStream: TCoreClassStream; StartPos: Int64; DoneAutoFree: Boolean); overload;
    procedure SendBigStream(Cmd: SystemString; BigStream: TCoreClassStream; DoneAutoFree: Boolean); overload;

    // send complete buffer
    procedure SendCompleteBuffer(Cmd: SystemString; buff: PByte; BuffSize: NativeInt; DoneAutoFree: Boolean);
  end;

  TPeerIOClass = class of TPeerIO;

  TPeerClient = TPeerIO;
  TPeerClientClass = TPeerIOClass;

  TPeerClientNotify = procedure(Sender: TPeerIO) of object;
  TPeerClientCMDNotify = procedure(Sender: TPeerIO; Cmd: SystemString; var Allow: Boolean) of object;

  TStatisticsType = (
    stReceiveSize, stSendSize,
    stRequest, stResponse,
    stConsole, stStream, stDirestConsole, stDirestStream, stReceiveBigStream, stSendBigStream, stReceiveCompleteBuffer, stSendCompleteBuffer,
    stExecConsole, stExecStream, stExecDirestConsole, stExecDirestStream, stExecBigStream, stExecCompleteBuffer,
    stTriggerConnect, stTriggerDisconnect,
    stTotalCommandExecute, stTotalCommandSend, stTotalCommandReg,
    stEncrypt, stCompress, stGenerateHash,
    stPause, stContinue,
    stLock, stUnLock,
    stPrint);

  TPerClientListCall = procedure(PeerClient: TPeerIO);
  TPerClientListMethod = procedure(PeerClient: TPeerIO) of object;
{$IFNDEF FPC} TPerClientListProc = reference to procedure(PeerClient: TPeerIO); {$ENDIF FPC}
  TIO_Array = array of Cardinal;

  ICommunicationFrameworkVMInterface = interface
    procedure p2pVMTunnelAuth(Sender: TPeerIO; const Token: SystemString; var Accept: Boolean);
    procedure p2pVMTunnelOpenBefore(Sender: TPeerIO; p2pVMTunnel: TCommunicationFrameworkWithP2PVM);
    procedure p2pVMTunnelOpen(Sender: TPeerIO; p2pVMTunnel: TCommunicationFrameworkWithP2PVM);
    procedure p2pVMTunnelOpenAfter(Sender: TPeerIO; p2pVMTunnel: TCommunicationFrameworkWithP2PVM);
    procedure p2pVMTunnelClose(Sender: TPeerIO; p2pVMTunnel: TCommunicationFrameworkWithP2PVM);
  end;

  TCommunicationProtocol = (cpZServer, cpCustom);

  TProgressOnCommunicationFramework = procedure(Sender: TCommunicationFramework) of object;

  TCommunicationFramework = class(TCoreClassInterfacedObject)
  protected
    FCommandList: THashObjectList;
    FPeerIO_HashPool: TUInt32HashObjectList;
    FIDCounter: Cardinal;
    FOnConnected: TPeerClientNotify;
    FOnDisconnect: TPeerClientNotify;
    FOnExecuteCommand: TPeerClientCMDNotify;
    FOnSendCommand: TPeerClientCMDNotify;
    FPeerIOUserDefineClass: TPeerIOUserDefineClass;
    FPeerIOUserSpecialClass: TPeerIOUserSpecialClass;
    FIdleTimeOut: TTimeTickValue;
    FSendDataCompressed: Boolean;
    FCompleteBufferCompressed: Boolean;
    FUsedParallelEncrypt: Boolean;
    FSyncOnResult: Boolean;
    FSyncOnCompleteBuffer: Boolean;
    FEnabledAtomicLockAndMultiThread: Boolean;
    FQuietMode: Boolean;
    FCipherSecurity: TCipherSecurity;
    FHashSecurity: THashSecurity;
    FMaxCompleteBufferSize: Cardinal;
    FPrintParams: THashVariantList;
    FPostProgress: TNProgressPostWithCadencer;
    FFrameworkIsServer: Boolean;
    FFrameworkIsClient: Boolean;
    FFrameworkInfo: SystemString;
    FOnProgressRuning: Boolean;
    FOnProgress: TProgressOnCommunicationFramework;
    FCMDWithThreadRuning: Integer;
    FVMInterface: ICommunicationFrameworkVMInterface;
    FProtocol: TCommunicationProtocol;
  protected
    procedure DoPrint(const v: SystemString); virtual;

    function GetIdleTimeOut: TTimeTickValue; virtual;
    procedure SetIdleTimeOut(const Value: TTimeTickValue); virtual;

    procedure DoConnected(Sender: TPeerIO); virtual;
    procedure DoDisconnect(Sender: TPeerIO); virtual;

    function CanExecuteCommand(Sender: TPeerIO; Cmd: SystemString): Boolean; virtual;
    function CanSendCommand(Sender: TPeerIO; Cmd: SystemString): Boolean; virtual;
    function CanRegCommand(Sender: TCommunicationFramework; Cmd: SystemString): Boolean; virtual;

    procedure DelayClose(Sender: TNPostExecute);
    procedure DelayFree(Sender: TNPostExecute);
    procedure DelayExecuteOnResultState(Sender: TNPostExecute);
    procedure DelayExecuteOnCompleteBufferState(Sender: TNPostExecute);

    procedure Internal_ProgressPerClient(PeerClient: TPeerIO);
    // user protocol support
    procedure FillCustomBuffer(Sender: TPeerIO; const th: TCoreClassThread; const buffer: PByte; const Size: NativeInt; var Customed: Boolean); virtual;
  protected
    // private vm and protocol stack support
    procedure InternalSendByteBuffer(const Sender: TPeerIO; const buff: PByte; siz: NativeInt); virtual;
    procedure InternalSaveReceiveBuffer(const Sender: TPeerIO; const buff: Pointer; siz: Int64); virtual;
    procedure InternalProcessReceiveBuffer(const Sender: TPeerIO; const ACurrentActiveThread: TCoreClassThread; const RecvSync, SendSync: Boolean); virtual;
    procedure InternalProcessAllSendCmd(const Sender: TPeerIO; const ACurrentActiveThread: TCoreClassThread; const RecvSync, SendSync: Boolean); virtual;
    procedure InternalClientCreate(const Sender: TPeerIO); virtual;
    procedure InternalClientDestroy(const Sender: TPeerIO); virtual;

    procedure CommandResult_BuildP2PAuthToken(Sender: TPeerIO; ResultData: TDataFrameEngine);
    procedure Command_BuildP2PAuthToken(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
    procedure Command_InitP2PTunnel(Sender: TPeerIO; InData: SystemString);
    procedure Command_CloseP2PTunnel(Sender: TPeerIO; InData: SystemString);

    procedure VMAuthSuccessAfterDelayExecute(Sender: TNPostExecute);
    procedure VMAuthSuccessDelayExecute(Sender: TNPostExecute);
    procedure VMAuthFailedDelayExecute(Sender: TNPostExecute);
  public
    Statistics: array [TStatisticsType] of Int64;
    CmdRecvStatistics: THashVariantList;
    CmdSendStatistics: THashVariantList;
    CmdMaxExecuteConsumeStatistics: THashVariantList;
  public
    constructor Create(HashPoolLen: Integer);
    procedure CreateAfter; virtual;
    destructor Destroy; override;

    // user protocol support
    property Protocol: TCommunicationProtocol read FProtocol write FProtocol;
    procedure WriteCustomBuffer(p_io: TPeerIO; const buffer: PByte; const Size: NativeInt);
    //
    // p2pVM backcall interface
    property VMInterface: ICommunicationFrameworkVMInterface read FVMInterface write FVMInterface;
    procedure p2pVMTunnelAuth(Sender: TPeerIO; const Token: SystemString; var Accept: Boolean); virtual;
    procedure p2pVMTunnelOpenBefore(Sender: TPeerIO; p2pVMTunnel: TCommunicationFrameworkWithP2PVM); virtual;
    procedure p2pVMTunnelOpen(Sender: TPeerIO; p2pVMTunnel: TCommunicationFrameworkWithP2PVM); virtual;
    procedure p2pVMTunnelOpenAfter(Sender: TPeerIO; p2pVMTunnel: TCommunicationFrameworkWithP2PVM); virtual;
    procedure p2pVMTunnelClose(Sender: TPeerIO; p2pVMTunnel: TCommunicationFrameworkWithP2PVM); virtual;
    //
    // Security support
    procedure SwitchMaxPerformance; virtual;
    procedure SwitchMaxSecurity; virtual;
    procedure SwitchDefaultPerformance; virtual;
    //
    // atomic lock
    procedure LockClients; virtual;
    procedure UnLockClients; virtual;
    //
    // delay run support
    property ProgressEngine: TNProgressPostWithCadencer read FPostProgress;
    property ProgressPost: TNProgressPostWithCadencer read FPostProgress;
    property PostProgress: TNProgressPostWithCadencer read FPostProgress;
    property PostRun: TNProgressPostWithCadencer read FPostProgress;
    property PostExecute: TNProgressPostWithCadencer read FPostProgress;

    // framework token
    property FrameworkIsServer: Boolean read FFrameworkIsServer;
    property FrameworkIsClient: Boolean read FFrameworkIsClient;
    property FrameworkInfo: SystemString read FFrameworkInfo;

    // mainLoop
    procedure Progress; virtual;
    property OnProgress: TProgressOnCommunicationFramework read FOnProgress write FOnProgress;

    // seealso filler allclient,safe works
    procedure ProgressPerClientC(OnBackcall: TPerClientListCall); overload;
    procedure ProgressPerClientM(OnBackcall: TPerClientListMethod); overload;
{$IFNDEF FPC} procedure ProgressPerClientP(OnBackcall: TPerClientListProc); overload; {$ENDIF FPC}
    //
    // seealso filler allclient,fast
    procedure FastProgressPerClientC(OnBackcall: TPerClientListCall); overload;
    procedure FastProgressPerClientM(OnBackcall: TPerClientListMethod); overload;
{$IFNDEF FPC} procedure FastProgressPerClientP(OnBackcall: TPerClientListProc); overload; {$ENDIF FPC}
    //
    // PeerIO id array
    procedure GetIO_Array(out IO_Array: TIO_Array);
    //
    // block progress
    procedure ProgressWaitSend(p_io: TPeerIO); virtual;
    //
    // print
    procedure PrintParam(v: SystemString; Args: SystemString);
    //
    // register command with server/client
    function DeleteRegistedCMD(Cmd: SystemString): Boolean;
    function UnRegisted(Cmd: SystemString): Boolean;
    function RegisterConsole(Cmd: SystemString): TCommandConsole;
    function RegisterStream(Cmd: SystemString): TCommandStream;
    function RegisterDirectStream(Cmd: SystemString): TCommandDirectStream;
    function RegisterDirectConsole(Cmd: SystemString): TCommandDirectConsole;
    function RegisterBigStream(Cmd: SystemString): TCommandBigStream;
    function RegisterCompleteBuffer(Cmd: SystemString): TCommandCompleteBuffer;
    function ExistsRegistedCmd(Cmd: SystemString): Boolean;
    procedure PrintRegistedCMD;
    //
    // execute command with local
    function ExecuteConsole(Sender: TPeerIO; Cmd: SystemString; const InData: SystemString; var OutData: SystemString): Boolean; virtual;
    function ExecuteStream(Sender: TPeerIO; Cmd: SystemString; InData, OutData: TDataFrameEngine): Boolean; virtual;
    function ExecuteDirectStream(Sender: TPeerIO; Cmd: SystemString; InData: TDataFrameEngine): Boolean; virtual;
    function ExecuteDirectConsole(Sender: TPeerIO; Cmd: SystemString; const InData: SystemString): Boolean; virtual;
    function ExecuteBigStream(Sender: TPeerIO; Cmd: SystemString; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64): Boolean; virtual;
    function ExecuteCompleteBuffer(Sender: TPeerIO; Cmd: SystemString; InData: PByte; DataSize: NativeInt): Boolean; virtual;
    //
    // misc
    function FirstClient: TPeerIO;
    function LastClient: TPeerIO;
    property OnConnected: TPeerClientNotify read FOnConnected write FOnConnected;
    property OnDisconnect: TPeerClientNotify read FOnDisconnect write FOnDisconnect;
    property OnExecuteCommand: TPeerClientCMDNotify read FOnExecuteCommand write FOnExecuteCommand;
    property OnSendCommand: TPeerClientCMDNotify read FOnSendCommand write FOnSendCommand;

    // p2p options
    property UsedParallelEncrypt: Boolean read FUsedParallelEncrypt write FUsedParallelEncrypt;
    property SyncOnResult: Boolean read FSyncOnResult write FSyncOnResult;
    property SyncOnCompleteBuffer: Boolean read FSyncOnCompleteBuffer write FSyncOnCompleteBuffer;
    property EnabledAtomicLockAndMultiThread: Boolean read FEnabledAtomicLockAndMultiThread write FEnabledAtomicLockAndMultiThread;
    property QuietMode: Boolean read FQuietMode write FQuietMode;
    property CipherSecurity: TCipherSecurity read FCipherSecurity;
    property IdleTimeOut: TTimeTickValue read GetIdleTimeOut write SetIdleTimeOut;
    property TimeOutIDLE: TTimeTickValue read GetIdleTimeOut write SetIdleTimeOut;
    property SendDataCompressed: Boolean read FSendDataCompressed write FSendDataCompressed;
    property CompleteBufferCompressed: Boolean read FCompleteBufferCompressed write FCompleteBufferCompressed;
    property HashSecurity: THashSecurity read FHashSecurity;
    property MaxCompleteBufferSize: Cardinal read FMaxCompleteBufferSize write FMaxCompleteBufferSize;

    // state
    property CMDWithThreadRuning: Integer read FCMDWithThreadRuning;

    // hash pool
    property PeerIO_HashPool: TUInt32HashObjectList read FPeerIO_HashPool;

    // custom struct: user custom instance one
    property PeerClientUserDefineClass: TPeerIOUserDefineClass read FPeerIOUserDefineClass write FPeerIOUserDefineClass;
    property PeerIOUserDefineClass: TPeerIOUserDefineClass read FPeerIOUserDefineClass write FPeerIOUserDefineClass;
    property UserDefineClass: TPeerIOUserDefineClass read FPeerIOUserDefineClass write FPeerIOUserDefineClass;
    property ExternalDefineClass: TPeerIOUserDefineClass read FPeerIOUserDefineClass write FPeerIOUserDefineClass;
    // custom special struct: user custom instance two
    property PeerClientUserSpecialClass: TPeerIOUserSpecialClass read FPeerIOUserSpecialClass write FPeerIOUserSpecialClass;
    property PeerIOUserSpecialClass: TPeerIOUserSpecialClass read FPeerIOUserSpecialClass write FPeerIOUserSpecialClass;
    property UserSpecialClass: TPeerIOUserSpecialClass read FPeerIOUserSpecialClass write FPeerIOUserSpecialClass;
    property ExternalSpecialClass: TPeerIOUserSpecialClass read FPeerIOUserSpecialClass write FPeerIOUserSpecialClass;

    // misc
    property IDCounter: Cardinal read FIDCounter write FIDCounter;
    property PrintParams: THashVariantList read FPrintParams;
  end;

  TCommunicationFrameworkServer = class(TCommunicationFramework)
  protected
    procedure DoPrint(const v: SystemString); override;
    function CanExecuteCommand(Sender: TPeerIO; Cmd: SystemString): Boolean; override;
    function CanSendCommand(Sender: TPeerIO; Cmd: SystemString): Boolean; override;
    function CanRegCommand(Sender: TCommunicationFramework; Cmd: SystemString): Boolean; override;

    procedure Command_CipherModel(Sender: TPeerIO; InData, OutData: TDataFrameEngine); virtual;
    procedure Command_Wait(Sender: TPeerIO; InData: SystemString; var OutData: SystemString); virtual;

    procedure InternalClientCreate(const Sender: TPeerIO); override;
    procedure InternalClientDestroy(const Sender: TPeerIO); override;
  protected
    FillSync_Sender: TPeerIO;
    FillSync_Buffer: PByte;
    FillSync_BufferSize: NativeInt;
    procedure SyncFillCustomBuffer;
    procedure FillCustomBuffer(Sender: TPeerIO; const th: TCoreClassThread; const buffer: PByte; const Size: NativeInt; var Customed: Boolean); override;
  public
    constructor Create; virtual;
    constructor CreateCustomHashPool(HashPoolLen: Integer); virtual;
    destructor Destroy; override;

    procedure Disconnect(ID: Cardinal); overload;
    procedure Disconnect(ID: Cardinal; delay: double); overload;

    // OnReceiveBuffer work on Protocol is cpCustom
    procedure OnReceiveBuffer(Sender: TPeerIO; const buffer: PByte; const Size: NativeInt); virtual;
    procedure WriteBuffer(p_io: TPeerIO; const buffer: PByte; const Size: NativeInt);

    // external service method
    procedure StopService; virtual;
    function StartService(Host: SystemString; Port: Word): Boolean; virtual;
    procedure TriggerQueueData(v: PQueueData); virtual;

    // service framework support
    procedure DoClientConnectBefore(Sender: TPeerIO); virtual;
    procedure DoClientConnectAfter(Sender: TPeerIO); virtual;
    procedure DoClientDisconnect(Sender: TPeerIO); virtual;

    // send cmd method
    procedure SendConsoleCmdM(p_io: TPeerIO; const Cmd, ConsoleData: SystemString; OnResult: TConsoleMethod); overload;
    procedure SendStreamCmdM(p_io: TPeerIO; const Cmd: SystemString; StreamData: TCoreClassStream; OnResult: TStreamMethod; DoneAutoFree: Boolean); overload;
    procedure SendStreamCmdM(p_io: TPeerIO; const Cmd: SystemString; StreamData: TDataFrameEngine; OnResult: TStreamMethod); overload;
    procedure SendStreamCmdM(p_io: TPeerIO; const Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamMethod); overload;

    // send cmd proc
{$IFNDEF FPC}
    procedure SendConsoleCmdP(p_io: TPeerIO; const Cmd, ConsoleData: SystemString; OnResult: TConsoleProc); overload;
    procedure SendStreamCmdP(p_io: TPeerIO; const Cmd: SystemString; StreamData: TCoreClassStream; OnResult: TStreamProc; DoneAutoFree: Boolean); overload;
    procedure SendStreamCmdP(p_io: TPeerIO; const Cmd: SystemString; StreamData: TDataFrameEngine; OnResult: TStreamProc); overload;
    procedure SendStreamCmdP(p_io: TPeerIO; const Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamProc); overload;
{$ENDIF FPC}
    // send direct cmd
    procedure SendDirectConsoleCmd(p_io: TPeerIO; const Cmd, ConsoleData: SystemString); overload;
    procedure SendDirectStreamCmd(p_io: TPeerIO; const Cmd: SystemString; StreamData: TCoreClassStream; DoneAutoFree: Boolean); overload;
    procedure SendDirectStreamCmd(p_io: TPeerIO; const Cmd: SystemString; StreamData: TDataFrameEngine); overload;
    procedure SendDirectStreamCmd(p_io: TPeerIO; const Cmd: SystemString); overload;

    // wait send
    function WaitSendConsoleCmd(p_io: TPeerIO; const Cmd, ConsoleData: SystemString; Timeout: TTimeTickValue): SystemString; overload; virtual;
    procedure WaitSendStreamCmd(p_io: TPeerIO; const Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; Timeout: TTimeTickValue); overload; virtual;

    // send bigstream
    procedure SendBigStream(p_io: TPeerIO; const Cmd: SystemString; BigStream: TCoreClassStream; StartPos: Int64; DoneAutoFree: Boolean); overload;
    procedure SendBigStream(p_io: TPeerIO; const Cmd: SystemString; BigStream: TCoreClassStream; DoneAutoFree: Boolean); overload;

    // send complete buffer
    procedure SendCompleteBuffer(p_io: TPeerIO; const Cmd: SystemString; buff: PByte; BuffSize: NativeInt; DoneAutoFree: Boolean); overload;

    // send used IO bind ID ,return method
    procedure SendConsoleCmdM(IO_ID: Cardinal; const Cmd, ConsoleData: SystemString; OnResult: TConsoleMethod); overload;
    procedure SendStreamCmdM(IO_ID: Cardinal; const Cmd: SystemString; StreamData: TCoreClassStream; OnResult: TStreamMethod; DoneAutoFree: Boolean); overload;
    procedure SendStreamCmdM(IO_ID: Cardinal; const Cmd: SystemString; StreamData: TDataFrameEngine; OnResult: TStreamMethod); overload;
    procedure SendStreamCmdM(IO_ID: Cardinal; const Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamMethod); overload;

    // send used IO bind ID,return proc
{$IFNDEF FPC}
    procedure SendConsoleCmdP(IO_ID: Cardinal; const Cmd, ConsoleData: SystemString; OnResult: TConsoleProc); overload;
    procedure SendStreamCmdP(IO_ID: Cardinal; const Cmd: SystemString; StreamData: TCoreClassStream; OnResult: TStreamProc; DoneAutoFree: Boolean); overload;
    procedure SendStreamCmdP(IO_ID: Cardinal; const Cmd: SystemString; StreamData: TDataFrameEngine; OnResult: TStreamProc); overload;
    procedure SendStreamCmdP(IO_ID: Cardinal; const Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamProc); overload;
{$ENDIF FPC}
    // direct send used IO BIND ID
    procedure SendDirectConsoleCmd(IO_ID: Cardinal; const Cmd, ConsoleData: SystemString); overload;
    procedure SendDirectStreamCmd(IO_ID: Cardinal; const Cmd: SystemString; StreamData: TCoreClassStream; DoneAutoFree: Boolean); overload;
    procedure SendDirectStreamCmd(IO_ID: Cardinal; const Cmd: SystemString; StreamData: TDataFrameEngine); overload;
    procedure SendDirectStreamCmd(IO_ID: Cardinal; const Cmd: SystemString); overload;

    // wait send
    function WaitSendConsoleCmd(IO_ID: Cardinal; const Cmd, ConsoleData: SystemString; Timeout: TTimeTickValue): SystemString; overload;
    procedure WaitSendStreamCmd(IO_ID: Cardinal; const Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; Timeout: TTimeTickValue); overload;

    // send bigstream
    procedure SendBigStream(IO_ID: Cardinal; const Cmd: SystemString; BigStream: TCoreClassStream; StartPos: Int64; DoneAutoFree: Boolean); overload;
    procedure SendBigStream(IO_ID: Cardinal; const Cmd: SystemString; BigStream: TCoreClassStream; DoneAutoFree: Boolean); overload;

    // send complete buffer
    procedure SendCompleteBuffer(IO_ID: Cardinal; const Cmd: SystemString; buff: PByte; BuffSize: NativeInt; DoneAutoFree: Boolean); overload;

    // Broadcast to all IO
    procedure BroadcastDirectConsoleCmd(Cmd: SystemString; ConsoleData: SystemString);
    procedure BroadcastSendDirectStreamCmd(Cmd: SystemString; StreamData: TDataFrameEngine);

    function Count: Integer;
    function Exists(p_io: TCoreClassObject): Boolean; overload;
    function Exists(p_io: TPeerIO): Boolean; overload;
    function Exists(p_io: TPeerIOUserDefine): Boolean; overload;
    function Exists(p_io: TPeerIOUserSpecial): Boolean; overload;

    function Exists(IO_ID: Cardinal): Boolean; overload;
    function GetPeerIO(ID: Cardinal): TPeerIO;
    property IO[ID: Cardinal]: TPeerIO read GetPeerIO; default;
    property PeerIO[ID: Cardinal]: TPeerIO read GetPeerIO;
  end;

  TCommunicationFrameworkServerClass = class of TCommunicationFrameworkServer;

  TCommunicationFrameworkClient = class;

  ICommunicationFrameworkClientInterface = interface
    procedure ClientConnected(Sender: TCommunicationFrameworkClient);
    procedure ClientDisconnect(Sender: TCommunicationFrameworkClient);
  end;

  TCommunicationFrameworkClient = class(TCommunicationFramework)
  protected
    FNotyifyInterface: ICommunicationFrameworkClientInterface;

    FConnectInitWaiting: Boolean;
    FConnectInitWaitingTimeout: TTimeTickValue;
    FAsyncConnectTimeout: TTimeTickValue;

    procedure DoPrint(const v: SystemString); override;

    procedure StreamResult_CipherModel(Sender: TPeerIO; ResultData: TDataFrameEngine);

    procedure DoConnected(Sender: TPeerIO); override;
    procedure DoDisconnect(Sender: TPeerIO); override;

    function CanExecuteCommand(Sender: TPeerIO; Cmd: SystemString): Boolean; override;
    function CanSendCommand(Sender: TPeerIO; Cmd: SystemString): Boolean; override;
    function CanRegCommand(Sender: TCommunicationFramework; Cmd: SystemString): Boolean; override;
  protected
    FillSync_Buffer: PByte;
    FillSync_BufferSize: NativeInt;
    procedure SyncFillCustomBuffer;
    procedure FillCustomBuffer(Sender: TPeerIO; const th: TCoreClassThread; const buffer: PByte; const Size: NativeInt; var Customed: Boolean); override;
  protected
    // async wait support
    FWaiting: Boolean;
    FWaitingTimeOut: TTimeTickValue;
    FOnWaitResultCall: TStateCall;
    FOnWaitResultMethod: TStateMethod;
{$IFNDEF FPC} FOnWaitResultProc: TStateProc; {$ENDIF FPC}
    procedure ConsoleResult_Wait(Sender: TPeerIO; ResultData: SystemString);
    function FixedTimeout(const t: TTimeTickValue): TTimeTickValue;
  public
    constructor Create; virtual;

    // OnReceiveBuffer work on Protocol is cpCustom
    procedure OnReceiveBuffer(const buffer: PByte; const Size: NativeInt); virtual;
    procedure WriteBuffer(const buffer: PByte; const Size: NativeInt);

    // mainLoop
    procedure Progress; override;

    // trigger io OnDisconnect event
    procedure TriggerDoDisconnect;

    // external io support,state
    function Connected: Boolean; virtual;
    // external io support,intf
    function ClientIO: TPeerIO; virtual;
    // external io support,intf
    procedure TriggerQueueData(v: PQueueData); virtual;

    // async connect support
    procedure TriggerDoConnectFailed; virtual;
    procedure TriggerDoConnectFinished; virtual;

    property AsyncConnectTimeout: TTimeTickValue read FAsyncConnectTimeout write FAsyncConnectTimeout;
    procedure AsyncConnectC(addr: SystemString; Port: Word; OnResult: TStateCall); virtual;
    procedure AsyncConnectM(addr: SystemString; Port: Word; OnResult: TStateMethod); virtual;
{$IFNDEF FPC} procedure AsyncConnectP(addr: SystemString; Port: Word; OnResult: TStateProc); virtual; {$ENDIF FPC}
    function Connect(addr: SystemString; Port: Word): Boolean; virtual;
    procedure Disconnect; virtual;

    // sync KeepAlive
    function Wait(ATimeOut: TTimeTickValue): SystemString; overload;
    // async KeepAlive
    function WaitC(ATimeOut: TTimeTickValue; OnResult: TStateCall): Boolean; overload;
    function WaitM(ATimeOut: TTimeTickValue; OnResult: TStateMethod): Boolean; overload;
{$IFNDEF FPC} function WaitP(ATimeOut: TTimeTickValue; OnResult: TStateProc): Boolean; overload; {$ENDIF FPC}
    // command queue state
    function WaitSendBusy: Boolean;
    function LastQueueData: PQueueData;
    function LastQueueCmd: SystemString;
    function QueueCmdCount: Integer;
    //
    // send cmd method
    procedure SendConsoleCmdM(Cmd: SystemString; ConsoleData: SystemString; OnResult: TConsoleMethod); overload;
    procedure SendStreamCmdM(Cmd: SystemString; StreamData: TCoreClassStream; OnResult: TStreamMethod; DoneAutoFree: Boolean); overload;
    procedure SendStreamCmdM(Cmd: SystemString; StreamData: TDataFrameEngine; OnResult: TStreamMethod); overload;
    procedure SendStreamCmdM(Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamMethod); overload;

    // send cmd proc
{$IFNDEF FPC}
    procedure SendConsoleCmdP(Cmd: SystemString; ConsoleData: SystemString; OnResult: TConsoleProc); overload;
    procedure SendStreamCmdP(Cmd: SystemString; StreamData: TCoreClassStream; OnResult: TStreamProc; DoneAutoFree: Boolean); overload;
    procedure SendStreamCmdP(Cmd: SystemString; StreamData: TDataFrameEngine; OnResult: TStreamProc); overload;
    procedure SendStreamCmdP(Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamProc); overload;
{$ENDIF FPC}
    // send direct cmd
    procedure SendDirectConsoleCmd(Cmd: SystemString; ConsoleData: SystemString);
    procedure SendDirectStreamCmd(Cmd: SystemString; StreamData: TCoreClassStream; DoneAutoFree: Boolean); overload;
    procedure SendDirectStreamCmd(Cmd: SystemString; StreamData: TDataFrameEngine); overload;
    procedure SendDirectStreamCmd(Cmd: SystemString); overload;

    // wait send
    function WaitSendConsoleCmd(Cmd: SystemString; ConsoleData: SystemString; Timeout: TTimeTickValue): SystemString; virtual;
    procedure WaitSendStreamCmd(Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; Timeout: TTimeTickValue); virtual;

    // send bigstream
    procedure SendBigStream(Cmd: SystemString; BigStream: TCoreClassStream; StartPos: Int64; DoneAutoFree: Boolean); overload;
    procedure SendBigStream(Cmd: SystemString; BigStream: TCoreClassStream; DoneAutoFree: Boolean); overload;

    // send complete buffer
    procedure SendCompleteBuffer(Cmd: SystemString; buff: PByte; BuffSize: NativeInt; DoneAutoFree: Boolean); overload;

    property NotyifyInterface: ICommunicationFrameworkClientInterface read FNotyifyInterface write FNotyifyInterface;
    // remote service ID
    // success ID > 0
    // failed! ID = 0
    function RemoteID: Cardinal;
    function RemoteKey: TCipherKeyBuffer;
    function RemoteInited: Boolean;
  end;

  TCommunicationFrameworkClientClass = class of TCommunicationFrameworkClient;

  Pp2pVMFragmentPackage = ^Tp2pVMFragmentPackage;

  Tp2pVMFragmentPackage = record
  public
    buffSiz: Cardinal;
    frameworkID: Cardinal;
    p2pID: Cardinal;
    pkType: Byte;
    buff: PByte;
  private
    procedure Init;
    function FillReceiveBuff(stream: TMemoryStream64): Integer;
    procedure BuildSendBuff(stream: TMemoryStream64);
  end;

  TPeerClientWithP2PVM = class(TPeerIO)
  private
    FLinkVM: TCommunicationFrameworkWithP2PVM;
    FRealSendBuff: TMemoryStream64;
    FSendQueue: TCoreClassList;
    FRemote_frameworkID: Cardinal;
    FRemote_p2pID: Cardinal;
    FIP: TIPV6;
    FPort: Word;
    FDestroyTimeNotify: Boolean;
  public
    procedure CreateAfter; override;
    destructor Destroy; override;
    function Connected: Boolean; override;
    procedure Disconnect; override;
    procedure SendByteBuffer(const buff: PByte; const Size: NativeInt); override;
    procedure WriteBufferOpen; override;
    procedure WriteBufferFlush; override;
    procedure WriteBufferClose; override;
    function GetPeerIP: SystemString; override;
    function WriteBufferEmpty: Boolean; override;
    procedure Progress; override;

    property LinkVM: TCommunicationFrameworkWithP2PVM read FLinkVM;
    property Remote_frameworkID: Cardinal read FRemote_frameworkID;
    property Remote_p2pID: Cardinal read FRemote_p2pID;
  end;

  // p2p VM listen service
  Pp2pVMListen = ^Tp2pVMListen;

  Tp2pVMListen = record
    frameworkID: Cardinal;
    ListenHost: TIPV6;
    ListenPort: Word;
    Listening: Boolean;
  end;

  TCommunicationFrameworkWithP2PVM_Server = class(TCommunicationFrameworkServer)
  protected
    procedure Connecting(SenderVM: TCommunicationFrameworkWithP2PVM;
      const Remote_frameworkID, frameworkID: Cardinal; const ipv6: TIPV6; const Port: Word; var Allowed: Boolean); virtual;
    procedure ListenState(SenderVM: TCommunicationFrameworkWithP2PVM; const ipv6: TIPV6; const Port: Word; const State: Boolean); virtual;
  private
    FFrameworkListenPool: TCoreClassList;
    FLinkVMPool: TUInt32HashObjectList;
    FFrameworkWithVM_ID: Cardinal;

    procedure ProgressDisconnectClient(PeerClient: TPeerIO);
    // internal Listen state
    function ListenCount: Integer;
    function GetListen(const index: Integer): Pp2pVMListen;
    function FindListen(const ipv6: TIPV6; const Port: Word): Pp2pVMListen;
    function FindListening(const ipv6: TIPV6; const Port: Word): Pp2pVMListen;
    procedure DeleteListen(const ipv6: TIPV6; const Port: Word);
    procedure ClearListen;
  public
    constructor Create; overload; override;
    constructor Create(HashPoolLen: Integer; frameworkID: Cardinal); overload; virtual;
    destructor Destroy; override;

    // mainLoop
    procedure Progress; override;

    // intf
    procedure TriggerQueueData(v: PQueueData); override;

    procedure CloseAllClient;

    // service method
    procedure ProgressStopServiceWithPerVM(SenderVM: TCommunicationFrameworkWithP2PVM);
    procedure StopService; override;
    function StartService(Host: SystemString; Port: Word): Boolean; override;

    // no blockMode
    function WaitSendConsoleCmd(p_io: TPeerIO; const Cmd, ConsoleData: SystemString; Timeout: TTimeTickValue): SystemString; override;
    procedure WaitSendStreamCmd(p_io: TPeerIO; const Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; Timeout: TTimeTickValue); override;
  end;

  TCommunicationFrameworkWithP2PVM_Client = class(TCommunicationFrameworkClient)
  protected
    procedure InternalClientCreate(const Sender: TPeerIO); override;
    procedure InternalClientDestroy(const Sender: TPeerIO); override;
    procedure VMConnectSuccessed(SenderVM: TCommunicationFrameworkWithP2PVM; Remote_frameworkID, Remote_p2pID, frameworkID: Cardinal); virtual;
    procedure VMDisconnect(SenderVM: TCommunicationFrameworkWithP2PVM); virtual;
  protected
    FLinkVM: TCommunicationFrameworkWithP2PVM;
    FFrameworkWithVM_ID: Cardinal;
    FVMClient: TPeerClientWithP2PVM;
    FVMConnected: Boolean;
    FDestroying: Boolean;

    FOnAsyncConnectNotifyCall: TStateCall;
    FOnAsyncConnectNotifyMethod: TStateMethod;
{$IFNDEF FPC} FOnAsyncConnectNotifyProc: TStateProc; {$ENDIF FPC}
  public
    constructor Create; overload; override;
    constructor Create(frameworkID: Cardinal); overload;
    destructor Destroy; override;

    procedure TriggerDoConnectFailed; override;
    procedure TriggerDoConnectFinished; override;

    function Connected: Boolean; override;
    function ClientIO: TPeerIO; override;
    procedure Progress; override;
    procedure TriggerQueueData(v: PQueueData); override;

    procedure AsyncConnect(addr: SystemString; Port: Word); overload;
    procedure AsyncConnectC(addr: SystemString; Port: Word; OnResult: TStateCall); overload; override;
    procedure AsyncConnectM(addr: SystemString; Port: Word; OnResult: TStateMethod); overload; override;
{$IFNDEF FPC} procedure AsyncConnectP(addr: SystemString; Port: Word; OnResult: TStateProc); overload; override; {$ENDIF FPC}
    function Connect(addr: SystemString; Port: Word): Boolean; override;
    procedure Disconnect; override;

    procedure ProgressWaitSend(p_io: TPeerIO); override;

    property LinkVM: TCommunicationFrameworkWithP2PVM read FLinkVM;
    property FrameworkWithVM_ID: Cardinal read FFrameworkWithVM_ID;
    property VMClient: TPeerClientWithP2PVM read FVMClient;
  end;

  TCommunicationFrameworkListCall = procedure(Sender: TCommunicationFramework);
  TCommunicationFrameworkListMethod = procedure(Sender: TCommunicationFramework) of object;
{$IFNDEF FPC} TCommunicationFrameworkListProc = reference to procedure(Sender: TCommunicationFramework); {$ENDIF FPC}
  TP2PVMAuthSuccessMethod = procedure(Sender: TCommunicationFrameworkWithP2PVM) of object;

  TCommunicationFrameworkWithP2PVM = class(TCoreClassObject)
  private type
    TOnEcho = record
      OnEchoCall: TStateCall;
      OnEchoMethod: TStateMethod;
{$IFNDEF FPC} OnEchoProc: TStateProc; {$ENDIF FPC}
      Timeout: TTimeTickValue;
    end;

    POnEcho = ^TOnEcho;
  private const
    c_p2pVM_echoing = $01;
    c_p2pVM_echo = $02;
    c_p2pVM_AuthSuccessed = $09;
    c_p2pVM_Listen = $10;
    c_p2pVM_ListenState = $11;
    c_p2pVM_Connecting = $20;
    c_p2pVM_ConnectedReponse = $21;
    c_p2pVM_Disconnect = $40;
    c_p2pVM_LogicFragmentData = $54;
    c_p2pVM_PhysicsFragmentData = $64;
  private
    FPhysicsTunnel: TPeerIO;
    FAuthWaiting: Boolean;
    FAuthed: Boolean;
    FAuthSending: Boolean;
    FFrameworkPool: TUInt32HashObjectList;
    FFrameworkListenPool: TCoreClassList;
    FMaxVMFragmentSize: Cardinal;
    FMaxRealBuffer: Cardinal;
    FQuietMode: Boolean;
    FReceiveStream: TMemoryStream64;
    FSendStream: TMemoryStream64;
    FWaitEchoList: TCoreClassList;
    FVMID: Cardinal;
    OnAuthSuccessOnesNotify: TP2PVMAuthSuccessMethod;
  private
    procedure Hook_SendByteBuffer(const Sender: TPeerIO; const buff: PByte; siz: NativeInt);
    procedure Hook_SaveReceiveBuffer(const Sender: TPeerIO; const buff: Pointer; siz: Int64);
    procedure SyncProcessReceiveBuff;
    procedure Hook_ProcessReceiveBuffer(const Sender: TPeerIO; const ACurrentActiveThread: TCoreClassThread; const RecvSync, SendSync: Boolean);
    procedure Hook_ClientDestroy(const Sender: TPeerIO);

    procedure SendVMBuffer(const buff: Pointer; const siz: NativeInt);
    procedure DisconnectWithVM(c: TPeerIO);
    //
    procedure ReceivedEchoing(const frameworkID, p2pID: Cardinal; const buff: PByte; const siz: Cardinal);
    procedure ReceivedEcho(const frameworkID, p2pID: Cardinal; const buff: PByte; const siz: Cardinal);
    procedure ReceivedListen(const frameworkID, p2pID: Cardinal; const buff: PByte; const siz: Cardinal);
    procedure ReceivedListenState(const frameworkID, p2pID: Cardinal; const buff: PByte; const siz: Cardinal);
    procedure ReceivedConnecting(const frameworkID, p2pID: Cardinal; const buff: PByte; const siz: Cardinal);
    procedure ReceivedConnectedReponse(const frameworkID, p2pID: Cardinal; const buff: PByte; const siz: Cardinal);
    procedure ReceivedDisconnect(const frameworkID, p2pID: Cardinal; const buff: PByte; const siz: Cardinal);
    procedure ReceivedLogicFragmentData(const frameworkID, p2pID: Cardinal; const buff: PByte; const siz: Cardinal);
    procedure ReceivedOriginFragmentData(const frameworkID, p2pID: Cardinal; const buff: PByte; const siz: Cardinal);
    //
    procedure DoProcessPerClientFragmentSend(PeerClient: TPeerIO);
    procedure DoPerClientClose(PeerClient: TPeerIO);
  public
    constructor Create(HashPoolLen: Integer);
    destructor Destroy; override;

    procedure Progress;
    //
    procedure ProgressCommunicationFrameworkC(OnBackcall: TCommunicationFrameworkListCall); overload;
    procedure ProgressCommunicationFrameworkM(OnBackcall: TCommunicationFrameworkListMethod); overload;
{$IFNDEF FPC} procedure ProgressCommunicationFrameworkP(OnBackcall: TCommunicationFrameworkListProc); overload; {$ENDIF FPC}
    //
    // p2p VM physics tunnel support
    procedure OpenP2PVMTunnel(c: TPeerIO);
    procedure CloseP2PVMTunnel;
    //
    // p2p VM logic CommunicationFramework support
    procedure InstallLogicFramework(c: TCommunicationFramework);
    procedure UninstallLogicFramework(c: TCommunicationFramework);
    function CreateLogicClient: TCommunicationFrameworkWithP2PVM_Client;
    //
    // p2p VM Peformance support
    // MaxVMFragmentSize see also MTU
    property MaxVMFragmentSize: Cardinal read FMaxVMFragmentSize write FMaxVMFragmentSize;
    property MaxRealBuffer: Cardinal read FMaxRealBuffer write FMaxRealBuffer;
    property QuietMode: Boolean read FQuietMode write FQuietMode;

    // p2p VM safe Support
    procedure AuthWaiting;
    procedure AuthVM; overload;
    property WasAuthed: Boolean read FAuthed;
    procedure AuthSuccessed;
    //
    // p2p VM echo support and keepalive
    procedure echoing(const OnEchoPtr: POnEcho; Timeout: TTimeTickValue); overload;
    procedure echoingC(OnResult: TStateCall; Timeout: TTimeTickValue); overload;
    procedure echoingM(OnResult: TStateMethod; Timeout: TTimeTickValue); overload;
{$IFNDEF FPC} procedure echoingP(OnResult: TStateProc; Timeout: TTimeTickValue); overload; {$ENDIF FPC}
    procedure echoBuffer(const buff: Pointer; const siz: NativeInt);
    //
    // p2p VM simulate with network listen
    procedure Listen(const frameworkID: Cardinal; const ipv6: TIPV6; const Port: Word; const Listening: Boolean);
    procedure ListenState(const frameworkID: Cardinal; const ipv6: TIPV6; const Port: Word; const Listening: Boolean);
    //
    // p2p VM simulate connecting
    procedure Connecting(const Remote_frameworkID, frameworkID, p2pID: Cardinal; const ipv6: TIPV6; const Port: Word);
    procedure ConnectedReponse(const Remote_frameworkID, Remote_p2pID, frameworkID, p2pID: Cardinal);
    procedure Disconnect(const Remote_frameworkID, Remote_p2pID: Cardinal);
    //
    // p2p VM Listen Query
    function ListenCount: Integer;
    function GetListen(const index: Integer): Pp2pVMListen;
    function FindListen(const ipv6: TIPV6; const Port: Word): Pp2pVMListen;
    function FindListening(const ipv6: TIPV6; const Port: Word): Pp2pVMListen;
    procedure DeleteListen(const ipv6: TIPV6; const Port: Word);
    procedure ClearListen;
    //
    // p2p VM operaton
    procedure CloseAllClientIO;
    procedure CloseAllServerIO;
  end;

  TProgressBackgroundProc = procedure();
  TProgressBackgroundMethod = procedure() of object;

var
  // communication data token
  c_DefaultConsoleToken: Byte = $F1;
  c_DefaultStreamToken: Byte = $2F;
  c_DefaultDirectConsoleToken: Byte = $F3;
  c_DefaultDirectStreamToken: Byte = $4F;
  c_DefaultBigStreamToken: Byte = $F5;
  c_DefaultCompleteBufferToken: Byte = $6F;

  // user custom header verify token
  c_DataHeadToken: Cardinal = $F0F0F0F0;
  // user custom tail verify token
  c_DataTailToken: Cardinal = $F1F1F1F1;

  // dostatus id
  c_DefaultDoStatusID: Integer = $0FFFFFFF;

  // vm auth token size
  C_VMAuthSize: Integer = 256;

const
  // system command
  C_BuildP2PAuthToken = '__@BuildP2PAuthToken';
  C_InitP2PTunnel = '__@InitP2PTunnel';
  C_CloseP2PTunnel = '__@CloseP2PTunnel';
  C_CipherModel = '__@CipherModel';
  C_Wait = '__@Wait';

var
  // global progress backcall
  ProgressBackgroundProc: TProgressBackgroundProc = nil;
  ProgressBackgroundMethod: TProgressBackgroundMethod = nil;

procedure DisposeQueueData(const v: PQueueData);
procedure InitQueueData(var v: TQueueData);
function NewQueueData: PQueueData;

function BuildP2PVMPackage(buffSiz, frameworkID, p2pID: Cardinal; pkType: Byte; buff: PByte): Pp2pVMFragmentPackage;
procedure FreeP2PVMPackage(p: Pp2pVMFragmentPackage);

function IsSystemCMD(const Cmd: U_String): Boolean;

function StrToIPv4(const s: U_String; var Success: Boolean): TIPV4;
function IPv4ToStr(const AIcsIPv4Addr: TIPV4): U_String;
function StrToIPv6(const s: U_String; var Success: Boolean; var ScopeID: Cardinal): TIPV6; overload;
function StrToIPv6(const s: U_String; var Success: Boolean): TIPV6; overload;
function IPv6ToStr(const IPv6Addr: TIPV6): U_String;
function IsIPv4(const s: U_String): Boolean;
function IsIPV6(const s: U_String): Boolean;

function CompareIPV4(const IP1, ip2: TIPV4): Boolean;
function CompareIPV6(const IP1, ip2: TIPV6): Boolean;

function TranslateBindAddr(addr: SystemString): SystemString;

procedure SyncMethod(t: TCoreClassThread; Sync: Boolean; proc: TThreadMethod);
procedure DoExecuteResult(c: TPeerIO; const QueuePtr: PQueueData; const AResultText: SystemString; AResultDF: TDataFrameEngine);

{$IFNDEF FPC}
function WaitSendConsoleCmdInThread(th: TCoreClassThread; Cf: TCommunicationFrameworkClient; Cmd: SystemString; ConsoleData: SystemString; Timeout: TTimeTickValue): SystemString;
procedure WaitSendStreamCmdInThread(th: TCoreClassThread; Cf: TCommunicationFrameworkClient; Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; Timeout: TTimeTickValue);
{$ENDIF FPC}


{ HPC compute support }
type
  TStreamCmdThread = class;

  TRunWithThreadStreamCall = procedure(Sender: TStreamCmdThread; ThInData, ThOutData: TDataFrameEngine);
  TRunWithThreadStreamMethod = procedure(Sender: TStreamCmdThread; ThInData, ThOutData: TDataFrameEngine) of object;
{$IFNDEF FPC} TRunWithThreadStreamProc = reference to procedure(Sender: TStreamCmdThread; ThInData, ThOutData: TDataFrameEngine); {$ENDIF FPC}

  TStreamCmdThread = class(TCoreClassThread)
  protected
    OnRunWithThreadCall: TRunWithThreadStreamCall;
    OnRunWithThreadMethod: TRunWithThreadStreamMethod;
{$IFNDEF FPC} OnRunWithThreadProc: TRunWithThreadStreamProc; {$ENDIF FPC}
    procedure Execute; override;
    procedure Done_Sync;
  public
    Framework: TCommunicationFramework;
    WorkID: Cardinal;
    UserData: Pointer;
    UserObject: TCoreClassObject;
    InData, OutData: TDataFrameEngine;

    constructor Create;
  end;

procedure RunStreamWithDelayThreadC(Sender: TPeerIO;
  const UserData: Pointer; const UserObject: TCoreClassObject;
  const InData, OutData: TDataFrameEngine; const OnRunWithThread: TRunWithThreadStreamCall); overload;

procedure RunStreamWithDelayThreadM(Sender: TPeerIO;
  const UserData: Pointer; const UserObject: TCoreClassObject;
  const InData, OutData: TDataFrameEngine; const OnRunWithThread: TRunWithThreadStreamMethod); overload;

{$IFNDEF FPC}
procedure RunStreamWithDelayThreadP(Sender: TPeerIO;
  const UserData: Pointer; const UserObject: TCoreClassObject;
  const InData, OutData: TDataFrameEngine; const OnRunWithThread: TRunWithThreadStreamProc); overload;
{$ENDIF FPC}


implementation

type
  // block state
  TWaitSendConsoleCmdIntf = class(TCoreClassObject)
  public
    NewResult: SystemString;
    Done: Boolean;
    constructor Create;
    procedure WaitSendConsoleResultEvent(p_io: TPeerIO; ResultData: SystemString);
  end;

  // block state
  TWaitSendStreamCmdIntf = class(TCoreClassObject)
  public
    NewResult: TDataFrameEngine;
    Done: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure WaitSendStreamResultEvent(p_io: TPeerIO; ResultData: TDataFrameEngine);
  end;

constructor TWaitSendConsoleCmdIntf.Create;
begin
  NewResult := '';
  Done := False;
end;

procedure TWaitSendConsoleCmdIntf.WaitSendConsoleResultEvent(p_io: TPeerIO; ResultData: SystemString);
begin
  NewResult := ResultData;
  Done := True;
end;

constructor TWaitSendStreamCmdIntf.Create;
begin
  NewResult := TDataFrameEngine.Create;
  Done := False;
end;

destructor TWaitSendStreamCmdIntf.Destroy;
begin
  DisposeObject(NewResult);
  inherited Destroy;
end;

procedure TWaitSendStreamCmdIntf.WaitSendStreamResultEvent(p_io: TPeerIO; ResultData: TDataFrameEngine);
begin
  NewResult.Assign(ResultData);
  Done := True;
end;

procedure DisposeQueueData(const v: PQueueData);
begin
  if v = nil then
      exit;
  if v^.DoneAutoFree then
    begin
      try
        if v^.StreamData <> nil then
            DisposeObject(v^.StreamData);

        if v^.BigStream <> nil then
            DisposeObject(v^.BigStream);

        if v^.buffer <> nil then
          begin
            if v^.BufferSize > 0 then
                FreeMem(v^.buffer, v^.BufferSize)
            else
                System.FreeMemory(v^.buffer);
          end;
      except
      end;
    end;

  Dispose(v);
end;

procedure InitQueueData(var v: TQueueData);
begin
  v.State := qsUnknow;
  v.IO_ID := 0;
  v.Cmd := '';
  v.Cipher := TCipherSecurity.csNone;
  v.ConsoleData := '';
  v.OnConsoleMethod := nil;
{$IFNDEF FPC} v.OnConsoleProc := nil; {$ENDIF FPC}
  v.StreamData := nil;
  v.OnStreamMethod := nil;
  v.OnStreamParamMethod := nil;
{$IFNDEF FPC}
  v.OnStreamProc := nil;
  v.OnStreamParamProc := nil;
{$ENDIF FPC}
  v.BigStreamStartPos := 0;
  v.BigStream := nil;
  v.buffer := nil;
  v.BufferSize := 0;
  v.DoneAutoFree := True;
  v.Param1 := nil;
  v.Param2 := nil;
end;

function NewQueueData: PQueueData;
begin
  new(Result);
  InitQueueData(Result^);
end;

function BuildP2PVMPackage(buffSiz, frameworkID, p2pID: Cardinal; pkType: Byte; buff: PByte): Pp2pVMFragmentPackage;
var
  p: Pp2pVMFragmentPackage;
begin
  new(p);
  p^.buffSiz := buffSiz;
  p^.frameworkID := frameworkID;
  p^.p2pID := p2pID;
  p^.pkType := pkType;
  if (buff <> nil) and (p^.buffSiz > 0) then
    begin
      p^.buff := GetMemory(p^.buffSiz);
      CopyPtr(buff, p^.buff, p^.buffSiz);
    end
  else
      p^.buff := nil;

  Result := p;
end;

procedure FreeP2PVMPackage(p: Pp2pVMFragmentPackage);
begin
  if (p^.buff <> nil) and (p^.buffSiz > 0) then
      FreeMem(p^.buff, p^.buffSiz);
  Dispose(p);
end;

function IsSystemCMD(const Cmd: U_String): Boolean;
begin
  Result := Cmd.Same(C_CipherModel, C_BuildP2PAuthToken, C_InitP2PTunnel, C_CloseP2PTunnel, C_Wait);
end;

function StrToIPv4(const s: U_String; var Success: Boolean): TIPV4;
var
  n: U_String;
  i: Integer;
  dotCount: Integer;
  NumVal: Integer;
  Len: Integer;
  Ch: Char;
begin
  FillPtrByte(@Result[0], SizeOf(Result), 0);
  Success := False;
  n := umlDeleteChar(s, [#32, #0, #9, #13, #10]);
  Len := n.Len;
  if Len < 6 then
      exit;
  dotCount := 0;
  NumVal := -1;
  for i := 1 to Len do
    begin
      Ch := n[i];
      if CharIn(Ch, c0to9) then
        begin
          if NumVal < 0 then
              NumVal := Ord(Ch) - Ord('0')
          else
              NumVal := NumVal * 10 + Ord(Ch) - Ord('0');
          if NumVal > 255 then
              exit;
        end
      else if Ch = '.' then
        begin
          if (NumVal > -1) and (dotCount < 3) then
              Result[dotCount] := NumVal
          else
              exit;
          inc(dotCount);
          NumVal := -1;
        end
      else
          exit;
    end;

  if (NumVal > -1) and (dotCount = 3) then
    begin
      Result[dotCount] := NumVal;
      Success := True;
    end;
end;

function IPv4ToStr(const AIcsIPv4Addr: TIPV4): U_String;
begin
  Result.Text := IntToStr(AIcsIPv4Addr[0]) + '.' + IntToStr(AIcsIPv4Addr[1]) + '.' + IntToStr(AIcsIPv4Addr[2]) + '.' + IntToStr(AIcsIPv4Addr[3]);
end;

function StrToIPv6(const s: U_String; var Success: Boolean; var ScopeID: Cardinal): TIPV6;
const
  Colon = ':';
  Percent = '%';
var
  n: U_String;
  ColonCnt: Integer;
  i: Integer;
  NumVal: Integer;
  Ch: Char;
  SLen: Integer;
  OmitPos: Integer;
  OmitCnt: Integer;
  PartCnt: Byte;
  ScopeFlag: Boolean;
begin
  FillPtrByte(@Result[0], SizeOf(Result), 0);
  Success := False;
  n := umlDeleteChar(s, [#32, #0, #9, #13, #10]);
  SLen := n.Len;
  if (SLen < 1) or (SLen > (4 * 8) + 7) then
      exit;
  ColonCnt := 0;
  for i := 1 to SLen do
    if (n[i] = Colon) then
        inc(ColonCnt);
  if ColonCnt > 7 then
      exit;
  OmitPos := n.GetPos('::') - 1;
  if OmitPos > -1 then
      OmitCnt := 8 - ColonCnt
  else begin
      OmitCnt := 0; // Make the compiler happy
      if (n.First = Colon) or (n.Last = Colon) then
          exit;
    end;
  NumVal := -1;
  ColonCnt := 0;
  PartCnt := 0;
  i := 0;
  ScopeID := 0;
  ScopeFlag := False;
  while i < SLen do
    begin
      Ch := n.buff[i];

      if Ch = Percent then
        begin
          if ScopeFlag then
              exit
          else
              ScopeFlag := True;

          PartCnt := 0;
          if NumVal > -1 then
            begin
              Result[ColonCnt] := NumVal;
              NumVal := -1;
            end;
        end
      else if Ch = Colon then
        begin
          if ScopeFlag then
              exit;
          PartCnt := 0;
          if NumVal > -1 then
            begin
              Result[ColonCnt] := NumVal;
              NumVal := -1;
            end;
          if (OmitPos = i) then
            begin
              inc(ColonCnt, OmitCnt);
              inc(i);
            end;
          inc(ColonCnt);
          if ColonCnt > 7 then
              exit;
        end
      else if CharIn(Ch, c0to9) then
        begin
          inc(PartCnt);
          if NumVal < 0 then
              NumVal := (Ord(Ch) - Ord('0'))
          else if ScopeFlag then
              NumVal := NumVal * 10 + (Ord(Ch) - Ord('0'))
          else
              NumVal := NumVal * 16 + (Ord(Ch) - Ord('0'));
          if (NumVal > high(Word)) or (PartCnt > 4) then
              exit;
        end
      else if CharIn(Ch, cAtoZ) then
        begin
          if ScopeFlag then
              exit;
          inc(PartCnt);
          if NumVal < 0 then
              NumVal := ((Ord(Ch) and 15) + 9)
          else
              NumVal := NumVal * 16 + ((Ord(Ch) and 15) + 9);
          if (NumVal > high(Word)) or (PartCnt > 4) then
              exit;
        end
      else
          exit;

      inc(i);
    end;

  if (NumVal > -1) and (ColonCnt > 1) then
    begin
      if not ScopeFlag then
        begin
          Result[ColonCnt] := NumVal;
        end
      else
          ScopeID := NumVal;
    end;
  Success := ColonCnt > 1;
end;

function StrToIPv6(const s: U_String; var Success: Boolean): TIPV6;
var
  SI: Cardinal;
begin
  Result := StrToIPv6(s, Success, SI);
end;

function IPv6ToStr(const IPv6Addr: TIPV6): U_String;
var
  i: Integer;
  Zeros1, Zeros2: set of Byte;
  Zeros1Cnt, Zeros2Cnt: Byte;
  OmitFlag: Boolean;
  ipv: SystemString;
begin
  ipv := '';
  Zeros1 := [];
  Zeros2 := [];
  Zeros1Cnt := 0;
  Zeros2Cnt := 0;
  for i := low(IPv6Addr) to high(IPv6Addr) do
    begin
      if IPv6Addr[i] = 0 then
        begin
          Include(Zeros1, i);
          inc(Zeros1Cnt);
        end
      else if Zeros1Cnt > Zeros2Cnt then
        begin
          Zeros2Cnt := Zeros1Cnt;
          Zeros2 := Zeros1;
          Zeros1 := [];
          Zeros1Cnt := 0;
        end;
    end;
  if Zeros1Cnt > Zeros2Cnt then
    begin
      Zeros2 := Zeros1;
      Zeros2Cnt := Zeros1Cnt;
    end;

  if Zeros2Cnt = 0 then
    begin
      for i := low(IPv6Addr) to high(IPv6Addr) do
        begin
          if i = 0 then
              ipv := IntToHex(IPv6Addr[i], 1)
          else
              ipv := ipv + ':' + IntToHex(IPv6Addr[i], 1);
        end;
    end
  else begin
      OmitFlag := False;
      for i := low(IPv6Addr) to high(IPv6Addr) do
        begin
          if not(i in Zeros2) then
            begin
              if OmitFlag then
                begin
                  if ipv = '' then
                      ipv := '::'
                  else
                      ipv := ipv + ':';
                  OmitFlag := False;
                end;
              if i < high(IPv6Addr) then
                  ipv := ipv + IntToHex(IPv6Addr[i], 1) + ':'
              else
                  ipv := ipv + IntToHex(IPv6Addr[i], 1);
            end
          else
              OmitFlag := True;
        end;
      if OmitFlag then
        begin
          if ipv = '' then
              ipv := '::'
          else
              ipv := ipv + ':';
        end;
      if ipv = '' then
          ipv := '::';
    end;
  Result.Text := LowerCase(ipv);
end;

function IsIPv4(const s: U_String): Boolean;
var
  n: U_String;
  i: Integer;
  DotCnt: Integer;
  NumVal: Integer;
  Ch: Char;
begin
  n := umlDeleteChar(s, [#32, #0, #9, #13, #10]);
  Result := False;
  DotCnt := 0;
  NumVal := -1;
  for i := 1 to n.Len do
    begin
      Ch := n[i];
      if CharIn(Ch, c0to9) then
        begin
          if NumVal = -1 then
              NumVal := Ord(Ch) - Ord('0')
          else
              NumVal := NumVal * 10 + Ord(Ch) - Ord('0');
          if NumVal > 255 then
              exit;
        end
      else if Ch = '.' then
        begin
          inc(DotCnt);
          if (DotCnt > 3) or (NumVal = -1) then
              exit;
          NumVal := -1;
        end
      else
          exit;
    end;

  Result := DotCnt = 3;
end;

function IsIPV6(const s: U_String): Boolean;
var
  ScopeID: Cardinal;
begin
  StrToIPv6(s, Result, ScopeID);
end;

function CompareIPV4(const IP1, ip2: TIPV4): Boolean;
begin
  Result := PCardinal(@IP1[0])^ = PCardinal(@ip2[0])^;
end;

function CompareIPV6(const IP1, ip2: TIPV6): Boolean;
begin
  Result := (PUInt64(@IP1[0])^ = PUInt64(@ip2[0])^) and (PUInt64(@IP1[4])^ = PUInt64(@ip2[4])^);
end;

function TranslateBindAddr(addr: SystemString): SystemString;
begin
  addr := umlTrimSpace(addr);
  if addr = '' then
      Result := 'IPv4+IPv6'
  else if addr = '127.0.0.1' then
      Result := 'Local IPv4'
  else if addr = '::1' then
      Result := 'Local IPv6'
  else if addr = '0.0.0.0' then
      Result := 'All IPv4'
  else if addr = '::' then
      Result := 'All IPv6'
  else if IsIPv4(addr) then
      Result := PFormat('Custom IPv4(%s)', [addr])
  else if IsIPV6(addr) then
      Result := PFormat('Custom IPv6(%s)', [addr])
  else
      Result := addr;
end;

procedure SyncMethod(t: TCoreClassThread; Sync: Boolean; proc: TThreadMethod);
begin
  if Sync then
    begin
      try
          TCoreClassThread.Synchronize(t, proc);
      except
      end;
    end
  else
    begin
      try
          proc();
      except
      end;
    end;
end;

procedure DoExecuteResult(c: TPeerIO; const QueuePtr: PQueueData; const AResultText: SystemString; AResultDF: TDataFrameEngine);
var
  aInData: TDataFrameEngine;
begin
  if QueuePtr = nil then
      exit;

  c.FReceiveResultRuning := True;

  try
    if Assigned(QueuePtr^.OnConsoleMethod) then
      begin
        c.PrintCommand('execute console on result cmd: %s', QueuePtr^.Cmd);
        try
            QueuePtr^.OnConsoleMethod(c, AResultText);
        except
        end;
      end;
{$IFNDEF FPC}
    if Assigned(QueuePtr^.OnConsoleProc) then
      begin
        c.PrintCommand('execute console on result(proc) cmd: %s', QueuePtr^.Cmd);
        try
            QueuePtr^.OnConsoleProc(c, AResultText);
        except
        end;
      end;
{$ENDIF FPC}
    if Assigned(QueuePtr^.OnStreamMethod) then
      begin
        c.PrintCommand('execute stream on result cmd: %s', QueuePtr^.Cmd);
        try
          AResultDF.Reader.index := 0;
          QueuePtr^.OnStreamMethod(c, AResultDF);
        except
        end;
      end;
    if Assigned(QueuePtr^.OnStreamParamMethod) then
      begin
        c.PrintCommand('execute stream on param result cmd: %s', QueuePtr^.Cmd);
        try
          AResultDF.Reader.index := 0;
          aInData := TDataFrameEngine.Create;
          QueuePtr^.StreamData.Position := 0;
          aInData.DecodeFrom(QueuePtr^.StreamData, True);
          QueuePtr^.OnStreamParamMethod(c, QueuePtr^.Param1, QueuePtr^.Param2, aInData, AResultDF);
          DisposeObject(aInData);
        except
        end;
      end;
{$IFNDEF FPC}
    if Assigned(QueuePtr^.OnStreamProc) then
      begin
        c.PrintCommand('execute stream on result(proc) cmd: %s', QueuePtr^.Cmd);
        try
          AResultDF.Reader.index := 0;
          QueuePtr^.OnStreamProc(c, AResultDF);
        except
        end;
      end;
    if Assigned(QueuePtr^.OnStreamParamProc) then
      begin
        c.PrintCommand('execute stream on result(parameter + proc) cmd: %s', QueuePtr^.Cmd);
        try
          AResultDF.Reader.index := 0;
          aInData := TDataFrameEngine.Create;
          QueuePtr^.StreamData.Position := 0;
          aInData.DecodeFrom(QueuePtr^.StreamData, True);
          QueuePtr^.OnStreamParamProc(c, QueuePtr^.Param1, QueuePtr^.Param2, aInData, AResultDF);
          DisposeObject(aInData);
        except
        end;
      end;
{$ENDIF FPC}
  finally
      c.FReceiveResultRuning := False;
  end;
end;

{$IFNDEF FPC}


function WaitSendConsoleCmdInThread(th: TCoreClassThread; Cf: TCommunicationFrameworkClient; Cmd: SystemString; ConsoleData: SystemString; Timeout: TTimeTickValue): SystemString;
var
  waitIntf: TWaitSendConsoleCmdIntf;
  timetick: TTimeTickValue;
  r: Boolean;
begin
  Result := '';
  if Cf.ClientIO = nil then
      exit;
  if not Cf.Connected then
      exit;

  r := True;
  TCoreClassThread.Synchronize(th,
    procedure
    begin
      r := Cf.CanSendCommand(Cf.ClientIO, Cmd);
    end);
  if not r then
      exit;

  TCoreClassThread.Synchronize(th,
    procedure
    begin
      Cf.ClientIO.PrintCommand('Begin Wait console cmd: %s', Cmd);
    end);

  timetick := GetTimeTickCount + Timeout;
  while Cf.ClientIO.WaitOnResult or Cf.ClientIO.BigStreamReceiveing do
    begin
      TCoreClassThread.Synchronize(th,
        procedure
        begin
          Cf.Progress;
        end);

      if not Cf.Connected then
          exit;
      if (Timeout > 0) and (GetTimeTickCount > timetick) then
          exit;
      th.Sleep(1);
    end;

  try
    waitIntf := TWaitSendConsoleCmdIntf.Create;
    waitIntf.Done := False;
    waitIntf.NewResult := '';
    TCoreClassThread.Synchronize(th,
      procedure
      begin
        Cf.SendConsoleCmdM(Cmd, ConsoleData, waitIntf.WaitSendConsoleResultEvent);
      end);

    while not waitIntf.Done do
      begin
        TCoreClassThread.Synchronize(th,
          procedure
          begin
            Cf.Progress;
          end);

        if not Cf.Connected then
            Break;

        if (Timeout > 0) and (GetTimeTickCount > timetick) then
            Break;
        th.Sleep(1);
      end;
    Result := waitIntf.NewResult;
    if waitIntf.Done then
        DisposeObject(waitIntf);

    TCoreClassThread.Synchronize(th,
      procedure
      begin
        Cf.ClientIO.PrintCommand('End Wait console cmd: %s', Cmd);
      end);
  except
      Result := '';
  end;
end;

procedure WaitSendStreamCmdInThread(th: TCoreClassThread; Cf: TCommunicationFrameworkClient; Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; Timeout: TTimeTickValue);
var
  waitIntf: TWaitSendStreamCmdIntf;
  timetick: TTimeTickValue;
  r: Boolean;
begin
  if Cf.ClientIO = nil then
      exit;
  if not Cf.Connected then
      exit;

  r := True;
  TCoreClassThread.Synchronize(th,
    procedure
    begin
      r := Cf.CanSendCommand(Cf.ClientIO, Cmd);
    end);
  if not r then
      exit;

  TCoreClassThread.Synchronize(th,
    procedure
    begin
      Cf.ClientIO.PrintCommand('Begin Wait Stream cmd: %s', Cmd);
    end);

  timetick := GetTimeTickCount + Timeout;

  if Cf.ClientIO.WaitOnResult then
    begin
      while Cf.ClientIO.WaitOnResult or Cf.ClientIO.BigStreamReceiveing do
        begin
          TCoreClassThread.Synchronize(th,
            procedure
            begin
              Cf.Progress;
            end);
          if not Cf.Connected then
              exit;
          if (Timeout > 0) and (GetTimeTickCount > timetick) then
              exit;
          th.Sleep(1);
        end;
    end;
  try
    waitIntf := TWaitSendStreamCmdIntf.Create;
    waitIntf.Done := False;

    TCoreClassThread.Synchronize(th,
      procedure
      begin
        Cf.SendStreamCmdM(Cmd, StreamData, waitIntf.WaitSendStreamResultEvent);
      end);

    while not waitIntf.Done do
      begin
        TCoreClassThread.Synchronize(th,
          procedure
          begin
            Cf.Progress;
          end);
        if not Cf.Connected then
            Break;
        if (Timeout > 0) and (GetTimeTickCount > timetick) then
            Break;
        th.Sleep(1);
      end;
    if waitIntf.Done then
      begin
        ResultData.Assign(waitIntf.NewResult);
        DisposeObject(waitIntf);
      end;

    TCoreClassThread.Synchronize(th,
      procedure
      begin
        Cf.ClientIO.PrintCommand('End Wait Stream cmd: %s', Cmd);
      end);
  except
  end;
end;
{$ENDIF FPC}


procedure TStreamCmdThread.Execute;
begin
  try
    if Assigned(OnRunWithThreadCall) then
        OnRunWithThreadCall(Self, InData, OutData);
    if Assigned(OnRunWithThreadMethod) then
        OnRunWithThreadMethod(Self, InData, OutData);
{$IFNDEF FPC}
    if Assigned(OnRunWithThreadProc) then
        OnRunWithThreadProc(Self, InData, OutData);
{$ENDIF FPC}
  except
  end;

  Synchronize({$IFDEF FPC}@{$ENDIF FPC}Done_Sync);
  DisposeObject([InData, OutData]);
end;

procedure TStreamCmdThread.Done_Sync;
var
  p_io: TPeerIO;
begin
  dec(Framework.FCMDWithThreadRuning);

  try
    if Framework is TCommunicationFrameworkServer then
        p_io := TCommunicationFrameworkServer(Framework).PeerIO[WorkID]
    else
        p_io := TCommunicationFrameworkClient(Framework).ClientIO;

    if p_io <> nil then
      begin
        p_io.OutDataFrame.Assign(OutData);
        p_io.ContinueResultSend;
      end;
  finally
  end;
end;

constructor TStreamCmdThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;

  OnRunWithThreadCall := nil;
  OnRunWithThreadMethod := nil;
{$IFNDEF FPC}
  OnRunWithThreadProc := nil;
{$ENDIF FPC}
  Framework := nil;
  WorkID := 0;
  UserData := nil;
  UserObject := nil;
  InData := nil;
  OutData := nil;
end;

procedure RunStreamWithDelayThreadC(Sender: TPeerIO;
const UserData: Pointer; const UserObject: TCoreClassObject;
const InData, OutData: TDataFrameEngine; const OnRunWithThread: TRunWithThreadStreamCall);
var
  t: TStreamCmdThread;
begin
  Sender.PauseResultSend;
  t := TStreamCmdThread.Create;
  t.FreeOnTerminate := True;

  t.OnRunWithThreadCall := OnRunWithThread;

  t.Framework := Sender.OwnerFramework;
  t.WorkID := Sender.ID;
  t.UserData := UserData;
  t.UserObject := UserObject;
  t.InData := TDataFrameEngine.Create;
  if InData <> nil then
      t.InData.Assign(InData);
  t.OutData := TDataFrameEngine.Create;
  if OutData <> nil then
      t.OutData.Assign(OutData);

  AtomInc(Sender.OwnerFramework.FCMDWithThreadRuning);

  t.Suspended := False;
end;

procedure RunStreamWithDelayThreadM(Sender: TPeerIO;
const UserData: Pointer; const UserObject: TCoreClassObject;
const InData, OutData: TDataFrameEngine; const OnRunWithThread: TRunWithThreadStreamMethod);
var
  t: TStreamCmdThread;
begin
  Sender.PauseResultSend;
  t := TStreamCmdThread.Create;
  t.FreeOnTerminate := True;

  t.OnRunWithThreadMethod := OnRunWithThread;

  t.Framework := Sender.OwnerFramework;
  t.WorkID := Sender.ID;
  t.UserData := UserData;
  t.UserObject := UserObject;
  t.InData := TDataFrameEngine.Create;
  if InData <> nil then
      t.InData.Assign(InData);
  t.OutData := TDataFrameEngine.Create;
  if OutData <> nil then
      t.OutData.Assign(OutData);

  AtomInc(Sender.OwnerFramework.FCMDWithThreadRuning);

  t.Suspended := False;
end;

{$IFNDEF FPC}


procedure RunStreamWithDelayThreadP(Sender: TPeerIO;
const UserData: Pointer; const UserObject: TCoreClassObject;
const InData, OutData: TDataFrameEngine; const OnRunWithThread: TRunWithThreadStreamProc);
var
  t: TStreamCmdThread;
begin
  Sender.PauseResultSend;
  t := TStreamCmdThread.Create;
  t.FreeOnTerminate := True;

  t.OnRunWithThreadProc := OnRunWithThread;

  t.Framework := Sender.OwnerFramework;
  t.WorkID := Sender.ID;
  t.UserData := UserData;
  t.UserObject := UserObject;
  t.InData := TDataFrameEngine.Create;
  if InData <> nil then
      t.InData.Assign(InData);
  t.OutData := TDataFrameEngine.Create;
  if OutData <> nil then
      t.OutData.Assign(OutData);

  AtomInc(Sender.OwnerFramework.FCMDWithThreadRuning);

  t.Suspended := False;
end;
{$ENDIF FPC}


constructor TCommandStream.Create;
begin
  inherited Create;

  FOnExecuteCall := nil;
  FOnExecuteMethod := nil;
{$IFNDEF FPC} FOnExecuteProc := nil; {$ENDIF FPC}
end;

destructor TCommandStream.Destroy;
begin
  inherited Destroy;
end;

function TCommandStream.Execute(Sender: TPeerIO; InData, OutData: TDataFrameEngine): Boolean;
begin
  Result := False;
  try
    if Assigned(FOnExecuteCall) then
        FOnExecuteCall(Sender, InData, OutData)
    else if Assigned(FOnExecuteMethod) then
        FOnExecuteMethod(Sender, InData, OutData)
{$IFNDEF FPC}
    else if Assigned(FOnExecuteProc) then
        FOnExecuteProc(Sender, InData, OutData)
{$ENDIF FPC}
    else
        Result := False;
  except
      Result := False;
  end;
end;

constructor TCommandConsole.Create;
begin
  inherited Create;

  FOnExecuteCall := nil;
  FOnExecuteMethod := nil;
{$IFNDEF FPC} FOnExecuteProc := nil; {$ENDIF FPC}
end;

destructor TCommandConsole.Destroy;
begin
  inherited Destroy;
end;

function TCommandConsole.Execute(Sender: TPeerIO; InData: SystemString; var OutData: SystemString): Boolean;
begin
  Result := False;
  try
    if Assigned(FOnExecuteCall) then
        FOnExecuteCall(Sender, InData, OutData)
    else if Assigned(FOnExecuteMethod) then
        FOnExecuteMethod(Sender, InData, OutData)
{$IFNDEF FPC}
    else if Assigned(FOnExecuteProc) then
        FOnExecuteProc(Sender, InData, OutData)
{$ENDIF FPC}
    else
        Result := False;
  except
      Result := False;
  end;
end;

constructor TCommandDirectStream.Create;
begin
  inherited Create;

  FOnExecuteCall := nil;
  FOnExecuteMethod := nil;
{$IFNDEF FPC} FOnExecuteProc := nil; {$ENDIF FPC}
end;

destructor TCommandDirectStream.Destroy;
begin
  inherited Destroy;
end;

function TCommandDirectStream.Execute(Sender: TPeerIO; InData: TDataFrameEngine): Boolean;
begin
  Result := True;
  try
    if Assigned(FOnExecuteCall) then
        FOnExecuteCall(Sender, InData)
    else if Assigned(FOnExecuteMethod) then
        FOnExecuteMethod(Sender, InData)
{$IFNDEF FPC}
    else if Assigned(FOnExecuteProc) then
        FOnExecuteProc(Sender, InData)
{$ENDIF FPC}
    else
        Result := False;
  except
      Result := False;
  end;
end;

constructor TCommandDirectConsole.Create;
begin
  inherited Create;

  FOnExecuteCall := nil;
  FOnExecuteMethod := nil;
{$IFNDEF FPC}
  FOnExecuteProc := nil;
{$ENDIF FPC}
end;

destructor TCommandDirectConsole.Destroy;
begin
  inherited Destroy;
end;

function TCommandDirectConsole.Execute(Sender: TPeerIO; InData: SystemString): Boolean;
begin
  Result := True;
  try
    if Assigned(FOnExecuteCall) then
        FOnExecuteCall(Sender, InData)
    else if Assigned(FOnExecuteMethod) then
        FOnExecuteMethod(Sender, InData)
{$IFNDEF FPC}
    else if Assigned(FOnExecuteProc) then
        FOnExecuteProc(Sender, InData)
{$ENDIF FPC}
    else
        Result := False;
  except
      Result := False;
  end;
end;

constructor TCommandBigStream.Create;
begin
  inherited Create;

  FOnExecuteCall := nil;
  FOnExecuteMethod := nil;
{$IFNDEF FPC}
  FOnExecuteProc := nil;
{$ENDIF FPC}
end;

destructor TCommandBigStream.Destroy;
begin
  inherited Destroy;
end;

function TCommandBigStream.Execute(Sender: TPeerIO; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64): Boolean;
begin
  Result := True;
  try
    if Assigned(FOnExecuteCall) then
        FOnExecuteCall(Sender, InData, BigStreamTotal, BigStreamCompleteSize)
    else if Assigned(FOnExecuteMethod) then
        FOnExecuteMethod(Sender, InData, BigStreamTotal, BigStreamCompleteSize)
{$IFNDEF FPC}
    else if Assigned(FOnExecuteProc) then
        FOnExecuteProc(Sender, InData, BigStreamTotal, BigStreamCompleteSize)
{$ENDIF FPC}
    else
        Result := False;
  except
      Result := False;
  end;
end;

constructor TCommandCompleteBuffer.Create;
begin
  inherited Create;

  FOnExecuteCall := nil;
  FOnExecuteMethod := nil;
{$IFNDEF FPC}
  FOnExecuteProc := nil;
{$ENDIF FPC}
end;

destructor TCommandCompleteBuffer.Destroy;
begin
  inherited Destroy;
end;

function TCommandCompleteBuffer.Execute(Sender: TPeerIO; InData: PByte; DataSize: NativeInt): Boolean;
begin
  Result := True;
  try
    if Assigned(FOnExecuteCall) then
        FOnExecuteCall(Sender, InData, DataSize)
    else if Assigned(FOnExecuteMethod) then
        FOnExecuteMethod(Sender, InData, DataSize)
{$IFNDEF FPC}
    else if Assigned(FOnExecuteProc) then
        FOnExecuteProc(Sender, InData, DataSize)
{$ENDIF FPC}
    else
        Result := False;
  except
      Result := False;
  end;
end;

procedure TBigStreamBatchPostData.Init;
begin
  Source := nil;
  CompletedBackcallPtr := 0;
  RemoteMD5 := NullMD5;
  SourceMD5 := NullMD5;
  index := -1;
  DBStorePos := 0;
end;

procedure TBigStreamBatchPostData.Encode(d: TDataFrameEngine);
begin
  d.WriteMD5(RemoteMD5);
  d.WriteMD5(SourceMD5);
  d.WriteInteger(index);
  d.WriteInt64(DBStorePos);
end;

procedure TBigStreamBatchPostData.Decode(d: TDataFrameEngine);
begin
  Source := nil;
  CompletedBackcallPtr := 0;
  RemoteMD5 := d.Reader.ReadMD5;
  SourceMD5 := d.Reader.ReadMD5;
  index := d.Reader.ReadInteger;
  DBStorePos := d.Reader.ReadInt64;
end;

function TBigStreamBatchList.GetItems(const index: Integer): PBigStreamBatchPostData;
begin
  Result := PBigStreamBatchPostData(FList[index]);
end;

constructor TBigStreamBatchList.Create(AOwner: TPeerIO);
begin
  inherited Create;
  FOwner := AOwner;
  FList := TCoreClassList.Create;
end;

destructor TBigStreamBatchList.Destroy;
begin
  Clear;
  DisposeObject(FList);
  inherited Destroy;
end;

procedure TBigStreamBatchList.Clear;
var
  i: Integer;
  p: PBigStreamBatchPostData;
begin
  for i := 0 to FList.Count - 1 do
    begin
      p := PBigStreamBatchPostData(FList[i]);
      DisposeObject(p^.Source);
      Dispose(p);
    end;

  FList.Clear;
end;

function TBigStreamBatchList.Count: Integer;
begin
  Result := FList.Count;
end;

function TBigStreamBatchList.NewPostData: PBigStreamBatchPostData;
begin
  new(Result);
  Result^.Init;
  Result^.Source := TMemoryStream64.Create;
  Result^.index := FList.Add(Result);
end;

function TBigStreamBatchList.First: PBigStreamBatchPostData;
begin
  Result := PBigStreamBatchPostData(FList[0]);
end;

function TBigStreamBatchList.Last: PBigStreamBatchPostData;
begin
  Result := PBigStreamBatchPostData(FList[FList.Count - 1]);
end;

procedure TBigStreamBatchList.DeleteLast;
begin
  if FList.Count > 0 then
      Delete(FList.Count - 1);
end;

procedure TBigStreamBatchList.Delete(const index: Integer);
var
  p: PBigStreamBatchPostData;
  i: Integer;
begin
  p := PBigStreamBatchPostData(FList[index]);
  DisposeObject(p^.Source);
  Dispose(p);
  FList.Delete(index);

  for i := 0 to FList.Count - 1 do
    begin
      p := PBigStreamBatchPostData(FList[i]);
      p^.index := i;
    end;
end;

constructor TPeerIOUserDefine.Create(AOwner: TPeerIO);
begin
  inherited Create;
  FOwner := AOwner;
  FWorkPlatform := TExecutePlatform.epUnknow;
  FBigStreamBatchList := TBigStreamBatchList.Create(Owner);
end;

destructor TPeerIOUserDefine.Destroy;
begin
  DisposeObject(FBigStreamBatchList);
  inherited Destroy;
end;

procedure TPeerIOUserDefine.Progress;
begin
end;

constructor TPeerIOUserSpecial.Create(AOwner: TPeerIO);
begin
  inherited Create;
  FOwner := AOwner;
end;

destructor TPeerIOUserSpecial.Destroy;
begin
  inherited Destroy;
end;

procedure TPeerIOUserSpecial.Progress;
begin
end;

procedure TPeerIO.P2PVMAuthSuccess(Sender: TCommunicationFrameworkWithP2PVM);
begin
  FOwnerFramework.ProgressPost.PostExecuteM(0, {$IFDEF FPC}@{$ENDIF FPC}FOwnerFramework.VMAuthSuccessDelayExecute).Data3 := ID;
end;

function TPeerIO.GetUserVariants: THashVariantList;
begin
  if FUserVariants = nil then
      FUserVariants := THashVariantList.Create;

  Result := FUserVariants;
end;

function TPeerIO.GetUserObjects: THashObjectList;
begin
  if FUserObjects = nil then
      FUserObjects := THashObjectList.Create(False);

  Result := FUserObjects;
end;

function TPeerIO.GetUserAutoFreeObjects: THashObjectList;
begin
  if FUserAutoFreeObjects = nil then
      FUserAutoFreeObjects := THashObjectList.Create(True);

  Result := FUserAutoFreeObjects;
end;

procedure TPeerIO.InternalSendByteBuffer(const buff: PByte; siz: NativeInt);
begin
  UpdateLastCommunicationTime;
  OnInternalSendByteBuffer(Self, buff, siz);
end;

procedure TPeerIO.SendInteger(v: Integer);
begin
  InternalSendByteBuffer(@v, C_Integer_Size);
end;

procedure TPeerIO.SendCardinal(v: Cardinal);
begin
  InternalSendByteBuffer(@v, C_Cardinal_Size);
end;

procedure TPeerIO.SendInt64(v: Int64);
begin
  InternalSendByteBuffer(@v, C_Int64_Size);
end;

procedure TPeerIO.SendByte(v: Byte);
begin
  InternalSendByteBuffer(@v, C_Byte_Size);
end;

procedure TPeerIO.SendWord(v: Word);
begin
  InternalSendByteBuffer(@v, C_Word_Size);
end;

procedure TPeerIO.SendVerifyCode(buff: Pointer; siz: NativeInt);
var
  headBuff: array [0 .. 2] of Byte;
  Code: TBytes;
begin
  GenerateHashCode(FOwnerFramework.FHashSecurity, buff, siz, Code);

  headBuff[0] := Byte(FOwnerFramework.FHashSecurity);
  PWORD(@headBuff[1])^ := length(Code);
  InternalSendByteBuffer(@headBuff[0], 3);
  if length(Code) > 0 then
      InternalSendByteBuffer(@Code[0], length(Code));
end;

procedure TPeerIO.SendEncryptBuffer(buff: PByte; siz: NativeInt; cs: TCipherSecurity);
begin
  SendByte(Byte(cs));
  Encrypt(cs, buff, siz, FCipherKey, True);
  InternalSendByteBuffer(buff, siz);
end;

procedure TPeerIO.SendEncryptMemoryStream(stream: TMemoryStream64; cs: TCipherSecurity);
begin
  SendEncryptBuffer(stream.Memory, stream.Size, cs);
end;

procedure TPeerIO.InternalSendConsoleBuff(buff: TMemoryStream64; cs: TCipherSecurity);
begin
  WriteBufferOpen;
  SendCardinal(FHeadToken);
  SendByte(Byte(FConsoleToken));
  SendCardinal(Cardinal(buff.Size));

  SendVerifyCode(buff.Memory, buff.Size);
  SendEncryptMemoryStream(buff, cs);
  SendCardinal(FTailToken);

  WriteBufferFlush;
  WriteBufferClose;
end;

procedure TPeerIO.InternalSendStreamBuff(buff: TMemoryStream64; cs: TCipherSecurity);
begin
  WriteBufferOpen;
  SendCardinal(FHeadToken);
  SendByte(Byte(FStreamToken));
  SendCardinal(Cardinal(buff.Size));

  SendVerifyCode(buff.Memory, buff.Size);
  SendEncryptMemoryStream(buff, cs);
  SendCardinal(FTailToken);

  WriteBufferFlush;
  WriteBufferClose;
end;

procedure TPeerIO.InternalSendDirectConsoleBuff(buff: TMemoryStream64; cs: TCipherSecurity);
begin
  WriteBufferOpen;
  SendCardinal(FHeadToken);
  SendByte(Byte(FDirectConsoleToken));
  SendCardinal(Cardinal(buff.Size));

  SendVerifyCode(buff.Memory, buff.Size);
  SendEncryptMemoryStream(buff, cs);
  SendCardinal(FTailToken);

  WriteBufferFlush;
  WriteBufferClose;
end;

procedure TPeerIO.InternalSendDirectStreamBuff(buff: TMemoryStream64; cs: TCipherSecurity);
begin
  WriteBufferOpen;
  SendCardinal(FHeadToken);
  SendByte(Byte(FDirectStreamToken));
  SendCardinal(Cardinal(buff.Size));

  SendVerifyCode(buff.Memory, buff.Size);
  SendEncryptMemoryStream(buff, cs);
  SendCardinal(FTailToken);

  WriteBufferFlush;
  WriteBufferClose;
end;

procedure TPeerIO.InternalSendBigStreamHeader(Cmd: SystemString; streamSiz: Int64);
var
  buff: TBytes;
begin
  WriteBufferOpen;

  SendCardinal(FHeadToken);
  SendByte(FBigStreamToken);
  SendInt64(streamSiz);
  buff := TPascalString(Cmd).Bytes;
  SendCardinal(Cardinal(length(buff)));
  InternalSendByteBuffer(@buff[0], length(buff));
  SendCardinal(FTailToken);
  try
    WriteBufferFlush;
    WriteBufferClose;
  except
  end;
end;

procedure TPeerIO.InternalSendBigStreamBuff(var Queue: TQueueData);
const
  ChunkSize = 64 * 1024;
var
  StartPos, EndPos: Int64;
  tmpPos: Int64;
  j: Int64;
  Num: Int64;
  Rest: Int64;
  buff: TBytes;
begin
  InternalSendBigStreamHeader(Queue.Cmd, Queue.BigStream.Size - Queue.BigStreamStartPos);

  WriteBufferOpen;

  StartPos := Queue.BigStreamStartPos;
  EndPos := Queue.BigStream.Size;
  tmpPos := StartPos;
  { Calculate number of full chunks that will fit into the buffer }
  Num := (EndPos - StartPos) div ChunkSize;
  { Calculate remaining bytes }
  Rest := (EndPos - StartPos) mod ChunkSize;
  { init buffer }
  SetLength(buff, ChunkSize);
  { Process full chunks }
  j := 0;
  while j < Num do
    begin
      if not Connected then
          exit;

      Queue.BigStream.Position := tmpPos;
      Queue.BigStream.read(buff[0], ChunkSize);
      inc(tmpPos, ChunkSize);

      InternalSendByteBuffer(@buff[0], ChunkSize);

      WriteBufferFlush;

      if Queue.BigStream.Size - tmpPos > ChunkSize * 8 then
        begin
          WriteBufferClose;

          FBigStreamSending := Queue.BigStream;
          FBigStreamSendState := tmpPos;
          FBigStreamSendDoneTimeFree := Queue.DoneAutoFree;
          Queue.BigStream := nil;
          exit;
        end;
      inc(j);
    end;

  { Process remaining bytes }
  if Rest > 0 then
    begin
      Queue.BigStream.Position := tmpPos;
      Queue.BigStream.read(buff[0], Rest);
      tmpPos := tmpPos + Rest;

      InternalSendByteBuffer(@buff[0], Rest);
    end;

  WriteBufferFlush;
  WriteBufferClose;
end;

procedure TPeerIO.InternalSendCompleteBufferHeader(Cmd: SystemString; buffSiz, compSiz: Cardinal);
var
  buff: TBytes;
begin
  SendCardinal(FHeadToken);
  SendByte(FCompleteBufferToken);
  SendCardinal(buffSiz);
  SendCardinal(compSiz);
  buff := TPascalString(Cmd).Bytes;
  SendCardinal(Cardinal(length(buff)));
  InternalSendByteBuffer(@buff[0], length(buff));
  SendCardinal(FTailToken);
end;

procedure TPeerIO.InternalSendCompleteBufferBuff(var Queue: TQueueData);
var
  sour, dest: TMemoryStream64;
begin
  WriteBufferOpen;
  if FOwnerFramework.FCompleteBufferCompressed then
    begin
      sour := TMemoryStream64.Create;
      sour.SetPointerWithProtectedMode(Queue.buffer, Queue.BufferSize);
      dest := TMemoryStream64.Create;
      FastCompressStream(sour, dest);
      InternalSendCompleteBufferHeader(Queue.Cmd, Queue.BufferSize, dest.Size);
      InternalSendByteBuffer(dest.Memory, dest.Size);
      DisposeObject(sour);
      DisposeObject(dest);
    end
  else
    begin
      InternalSendCompleteBufferHeader(Queue.Cmd, Queue.BufferSize, 0);
      InternalSendByteBuffer(Queue.buffer, Queue.BufferSize);
    end;
  WriteBufferFlush;
  WriteBufferClose;
end;

procedure TPeerIO.Sync_InternalSendResultData;
begin
  if FResultDataBuffer.Size > 0 then
    begin
      WriteBufferOpen;
      InternalSendByteBuffer(FResultDataBuffer.Memory, FResultDataBuffer.Size);
      FResultDataBuffer.Clear;
      WriteBufferFlush;
      WriteBufferClose;
    end;
end;

procedure TPeerIO.Sync_InternalSendConsoleCmd;
var
  df: TDataFrameEngine;
  stream: TMemoryStream64;
begin
  df := TDataFrameEngine.Create;
  stream := TMemoryStream64.Create;

  df.WriteString(FSyncPick^.Cmd);
  df.WriteString(FSyncPick^.ConsoleData);

  if FOwnerFramework.FSendDataCompressed then
      df.EncodeAsZLib(stream, True)
  else
      df.EncodeTo(stream, True);

  InternalSendConsoleBuff(stream, FSyncPick^.Cipher);

  DisposeObject(df);
  DisposeObject(stream);

  if FOwnerFramework.FSendDataCompressed then
      AtomInc(FOwnerFramework.Statistics[TStatisticsType.stCompress]);

  PrintCommand('internal send console cmd:%s', FSyncPick^.Cmd);
end;

procedure TPeerIO.Sync_InternalSendStreamCmd;
var
  df: TDataFrameEngine;
  stream: TMemoryStream64;
begin
  df := TDataFrameEngine.Create;
  stream := TMemoryStream64.Create;

  df.WriteString(FSyncPick^.Cmd);
  df.WriteStream(FSyncPick^.StreamData);

  if FOwnerFramework.FSendDataCompressed then
      df.EncodeAsZLib(stream, True)
  else
      df.EncodeTo(stream, True);

  InternalSendStreamBuff(stream, FSyncPick^.Cipher);

  DisposeObject(df);
  DisposeObject(stream);

  if FOwnerFramework.FSendDataCompressed then
      AtomInc(FOwnerFramework.Statistics[TStatisticsType.stCompress]);

  PrintCommand('internal send stream cmd:%s', FSyncPick^.Cmd);
end;

procedure TPeerIO.Sync_InternalSendDirectConsoleCmd;
var
  df: TDataFrameEngine;
  stream: TMemoryStream64;
begin
  df := TDataFrameEngine.Create;
  stream := TMemoryStream64.Create;

  df.WriteString(FSyncPick^.Cmd);
  df.WriteString(FSyncPick^.ConsoleData);

  if FOwnerFramework.FSendDataCompressed then
      df.EncodeAsZLib(stream, True)
  else
      df.EncodeTo(stream, True);

  InternalSendDirectConsoleBuff(stream, FSyncPick^.Cipher);

  DisposeObject(df);
  DisposeObject(stream);

  if FOwnerFramework.FSendDataCompressed then
      AtomInc(FOwnerFramework.Statistics[TStatisticsType.stCompress]);

  PrintCommand('internal send direct console cmd:%s', FSyncPick^.Cmd);
end;

procedure TPeerIO.Sync_InternalSendDirectStreamCmd;
var
  df: TDataFrameEngine;
  stream: TMemoryStream64;
begin
  df := TDataFrameEngine.Create;
  stream := TMemoryStream64.Create;

  df.WriteString(FSyncPick^.Cmd);
  df.WriteStream(FSyncPick^.StreamData);

  if FOwnerFramework.FSendDataCompressed then
      df.EncodeAsZLib(stream, True)
  else
      df.EncodeTo(stream, True);

  InternalSendDirectStreamBuff(stream, FSyncPick^.Cipher);

  DisposeObject(df);
  DisposeObject(stream);

  if FOwnerFramework.FSendDataCompressed then
      AtomInc(FOwnerFramework.Statistics[TStatisticsType.stCompress]);

  PrintCommand('internal send direct stream cmd:%s', FSyncPick^.Cmd);
end;

procedure TPeerIO.Sync_InternalSendBigStreamCmd;
begin
  InternalSendBigStreamBuff(FSyncPick^);
  AtomInc(FOwnerFramework.Statistics[TStatisticsType.stExecBigStream]);

  PrintCommand('internal send bigstream cmd:%s', FSyncPick^.Cmd);
end;

procedure TPeerIO.Sync_InternalSendCompleteBufferCmd;
begin
  InternalSendCompleteBufferBuff(FSyncPick^);
  AtomInc(FOwnerFramework.Statistics[TStatisticsType.stExecCompleteBuffer]);

  PrintCommand('internal send complete buffer cmd:%s', FSyncPick^.Cmd);
end;

procedure TPeerIO.Sync_ExecuteConsole;
var
  d: TTimeTickValue;
begin
  FReceiveCommandRuning := True;
  PrintCommand('execute console cmd:%s', FInCmd);

  d := GetTimeTickCount;
  FOwnerFramework.ExecuteConsole(Self, FInCmd, FInText, FOutText);
  FReceiveCommandRuning := False;

  FOwnerFramework.CmdMaxExecuteConsumeStatistics.SetMax(FInCmd, GetTimeTickCount - d);

  AtomInc(FOwnerFramework.Statistics[TStatisticsType.stExecConsole]);
  FOwnerFramework.CmdRecvStatistics.IncValue(FInCmd, 1);
end;

procedure TPeerIO.Sync_ExecuteStream;
var
  d: TTimeTickValue;
begin
  FReceiveCommandRuning := True;
  PrintCommand('execute stream cmd:%s', FInCmd);

  d := GetTimeTickCount;
  FOwnerFramework.ExecuteStream(Self, FInCmd, FInDataFrame, FOutDataFrame);
  FReceiveCommandRuning := False;

  FOwnerFramework.CmdMaxExecuteConsumeStatistics.SetMax(FInCmd, GetTimeTickCount - d);

  AtomInc(FOwnerFramework.Statistics[TStatisticsType.stExecStream]);
  FOwnerFramework.CmdRecvStatistics.IncValue(FInCmd, 1);
end;

procedure TPeerIO.Sync_ExecuteDirectConsole;
var
  d: TTimeTickValue;
begin
  FReceiveCommandRuning := True;
  PrintCommand('execute direct console cmd:%s', FInCmd);

  d := GetTimeTickCount;
  FOwnerFramework.ExecuteDirectConsole(Self, FInCmd, FInText);
  FReceiveCommandRuning := False;

  FOwnerFramework.CmdMaxExecuteConsumeStatistics.SetMax(FInCmd, GetTimeTickCount - d);

  AtomInc(FOwnerFramework.Statistics[TStatisticsType.stExecDirestConsole]);
  FOwnerFramework.CmdRecvStatistics.IncValue(FInCmd, 1);
end;

procedure TPeerIO.Sync_ExecuteDirectStream;
var
  d: TTimeTickValue;
begin
  FReceiveCommandRuning := True;
  PrintCommand('execute direct stream cmd:%s', FInCmd);

  d := GetTimeTickCount;
  FOwnerFramework.ExecuteDirectStream(Self, FInCmd, FInDataFrame);
  FReceiveCommandRuning := False;

  FOwnerFramework.CmdMaxExecuteConsumeStatistics.SetMax(FInCmd, GetTimeTickCount - d);

  AtomInc(FOwnerFramework.Statistics[TStatisticsType.stExecDirestStream]);
  FOwnerFramework.CmdRecvStatistics.IncValue(FInCmd, 1);
end;

procedure TPeerIO.Sync_SendConsoleResult;
var
  buff: TBytes;
begin
  buff := TPascalString(FOutText).Bytes;
  WriteBufferOpen;

  SendCardinal(FHeadToken);
  SendInteger(length(buff));

  SendVerifyCode(@buff[0], length(buff));

  SendEncryptBuffer(@buff[0], length(buff), FReceiveDataCipherSecurity);
  SendCardinal(FTailToken);

  WriteBufferFlush;
  WriteBufferClose;

  AtomInc(FOwnerFramework.Statistics[TStatisticsType.stResponse]);
end;

procedure TPeerIO.Sync_SendStreamResult;
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  FOutDataFrame.EncodeTo(m64, True);

  WriteBufferOpen;
  SendCardinal(FHeadToken);
  SendInteger(m64.Size);

  SendVerifyCode(m64.Memory, m64.Size);

  SendEncryptBuffer(m64.Memory, m64.Size, FReceiveDataCipherSecurity);
  SendCardinal(FTailToken);
  DisposeObject(m64);

  WriteBufferFlush;
  WriteBufferClose;
  AtomInc(FOwnerFramework.Statistics[TStatisticsType.stResponse]);
end;

procedure TPeerIO.ExecuteDataFrame(ACurrentActiveThread: TCoreClassThread; const RecvSync, SendSync: Boolean; CommDataType: Byte; DataFrame: TDataFrameEngine);
begin
  FInCmd := DataFrame.Reader.ReadString;

  if CommDataType = FConsoleToken then
    begin
      FInText := DataFrame.Reader.ReadString;
      FOutText := '';

      FCanPauseResultSend := True;

      FReceiveTriggerRuning := True;
      IO_SyncMethod(ACurrentActiveThread, RecvSync, {$IFDEF FPC}@{$ENDIF FPC}Sync_ExecuteConsole);
      FReceiveTriggerRuning := False;

      FCanPauseResultSend := False;

      if FPauseResultSend then
        begin
          FCurrentPauseResultSend_CommDataType := CommDataType;
          exit;
        end;
      if not Connected then
          exit;

      IO_SyncMethod(ACurrentActiveThread, SendSync, {$IFDEF FPC}@{$ENDIF FPC}Sync_SendConsoleResult);
    end
  else if CommDataType = FStreamToken then
    begin
      FInDataFrame.Clear;
      FOutDataFrame.Clear;
      DataFrame.Reader.ReadDataFrame(FInDataFrame);

      FCanPauseResultSend := True;

      FReceiveTriggerRuning := True;
      IO_SyncMethod(ACurrentActiveThread, RecvSync, {$IFDEF FPC}@{$ENDIF FPC}Sync_ExecuteStream);
      FReceiveTriggerRuning := False;

      FCanPauseResultSend := False;

      if FPauseResultSend then
        begin
          FCurrentPauseResultSend_CommDataType := CommDataType;
          exit;
        end;

      if not Connected then
          exit;

      IO_SyncMethod(ACurrentActiveThread, SendSync, {$IFDEF FPC}@{$ENDIF FPC}Sync_SendStreamResult);
    end
  else if CommDataType = FDirectConsoleToken then
    begin
      FInText := DataFrame.Reader.ReadString;

      FReceiveTriggerRuning := True;
      IO_SyncMethod(ACurrentActiveThread, RecvSync, {$IFDEF FPC}@{$ENDIF FPC}Sync_ExecuteDirectConsole);
      FReceiveTriggerRuning := False;
    end
  else if CommDataType = FDirectStreamToken then
    begin
      FInDataFrame.Clear;
      FOutDataFrame.Clear;
      DataFrame.Reader.ReadDataFrame(FInDataFrame);

      FReceiveTriggerRuning := True;
      IO_SyncMethod(ACurrentActiveThread, RecvSync, {$IFDEF FPC}@{$ENDIF FPC}Sync_ExecuteDirectStream);
      FReceiveTriggerRuning := False;
    end;
end;

procedure TPeerIO.Sync_ExecuteBigStream;
var
  d: TTimeTickValue;
begin
  FReceiveCommandRuning := True;
  d := GetTimeTickCount;
  FOwnerFramework.ExecuteBigStream(Self, FBigStreamCmd, FBigStreamReceive, FBigStreamTotal, FBigStreamCompleted);
  FReceiveCommandRuning := False;
  FOwnerFramework.CmdMaxExecuteConsumeStatistics.SetMax(FInCmd, GetTimeTickCount - d);

  if FBigStreamTotal = FBigStreamCompleted then
    begin
      FOwnerFramework.CmdRecvStatistics.IncValue(FBigStreamCmd, 1);
      PrintCommand('Big Stream complete with cmd:%s', FBigStreamCmd);
    end;
end;

function TPeerIO.FillBigStreamBuffer(ACurrentActiveThread: TCoreClassThread; const Sync: Boolean): Boolean;
var
  leftSize: Int64;
  tmpStream: TMemoryStream64;
begin
  leftSize := FBigStreamTotal - FBigStreamCompleted;
  if leftSize > FReceivedBuffer.Size then
    begin
      FReceivedBuffer.Position := 0;
      FBigStreamCompleted := FBigStreamCompleted + FReceivedBuffer.Size;
      FBigStreamReceive := FReceivedBuffer;

      IO_SyncMethod(ACurrentActiveThread, Sync, {$IFDEF FPC}@{$ENDIF FPC}Sync_ExecuteBigStream);

      FReceivedBuffer.Clear;
      Result := False;
    end
  else
    begin
      FReceivedBuffer.Position := 0;
      tmpStream := TMemoryStream64.Create;
      tmpStream.CopyFrom(FReceivedBuffer, leftSize);
      tmpStream.Position := 0;
      FBigStreamCompleted := FBigStreamTotal;
      FBigStreamReceive := tmpStream;

      IO_SyncMethod(ACurrentActiveThread, Sync, {$IFDEF FPC}@{$ENDIF FPC}Sync_ExecuteBigStream);

      tmpStream.Clear;
      if FReceivedBuffer.Size - leftSize > 0 then
          tmpStream.WritePtr(FReceivedBuffer.PositionAsPtr(leftSize), FReceivedBuffer.Size - leftSize);
      DisposeObject(FReceivedBuffer);
      FReceivedBuffer := tmpStream;
      Result := True;

      FBigStreamTotal := 0;
      FBigStreamCompleted := 0;
      FBigStreamCmd := '';
      FBigStreamReceiveProcessing := False;

      FReceivedBuffer.Position := 0;
    end;
  FBigStreamReceive := nil;
end;

procedure TPeerIO.Sync_ExecuteCompleteBuffer;
var
  d: TTimeTickValue;
begin
  if FOwnerFramework.FSyncOnCompleteBuffer then
    begin
      FReceiveCommandRuning := True;
      d := GetTimeTickCount;

      FOwnerFramework.ExecuteCompleteBuffer(Self, FCompleteBufferCmd, FCompleteBufferReceiveStream.Memory, FCompleteBufferReceiveStream.Size);

      FReceiveCommandRuning := False;
      FOwnerFramework.CmdMaxExecuteConsumeStatistics.SetMax(FInCmd, GetTimeTickCount - d);

      FOwnerFramework.CmdRecvStatistics.IncValue(FCompleteBufferCmd, 1);
      PrintCommand('execute complete buffer cmd:%s', FCompleteBufferCmd);
    end
  else
    begin
      FCompleteBufferReceiveStream.Position := 0;
      with FOwnerFramework.ProgressPost.PostExecute() do
        begin
          Data3 := FID;
          Data4 := FCompleteBufferCmd;
          Data1 := FCompleteBufferReceiveStream;
          OnExecuteMethod := {$IFDEF FPC}@{$ENDIF FPC}FOwnerFramework.DelayExecuteOnCompleteBufferState;
        end;

      FCompleteBufferReceiveStream := TMemoryStream64.Create
    end;
end;

function TPeerIO.FillCompleteBufferBuffer(ACurrentActiveThread: TCoreClassThread; const Sync: Boolean): Boolean;
var
  leftSize: Cardinal;
  tmpStream: TMemoryStream64;

  dest: TMemoryStream64;
begin
  leftSize := FCompleteBufferTotal - FCompleteBufferCompleted;
  if leftSize > FReceivedBuffer.Size then
    begin
      FCompleteBufferCompleted := FCompleteBufferCompleted + FReceivedBuffer.Size;

      FReceivedBuffer.Position := 0;
      FCompleteBufferReceiveStream.Position := FCompleteBufferReceiveStream.Size;
      FCompleteBufferReceiveStream.WritePtr(FReceivedBuffer.Memory, FReceivedBuffer.Size);

      FReceivedBuffer.Clear;
      Result := False;
    end
  else
    begin
      FReceivedBuffer.Position := 0;
      FCompleteBufferReceiveStream.Position := FCompleteBufferReceiveStream.Size;
      FCompleteBufferReceiveStream.WritePtr(FReceivedBuffer.Memory, leftSize);
      FCompleteBufferReceiveStream.Position := 0;

      tmpStream := TMemoryStream64.Create;
      if FReceivedBuffer.Size - leftSize > 0 then
          tmpStream.WritePtr(FReceivedBuffer.PositionAsPtr(leftSize), FReceivedBuffer.Size - leftSize);
      DisposeObject(FReceivedBuffer);
      FReceivedBuffer := tmpStream;

      if FCompleteBufferCompressedSize > 0 then
        begin
          dest := TMemoryStream64.Create;
          DecompressStream(FCompleteBufferReceiveStream, dest);
          DisposeObject(FCompleteBufferReceiveStream);
          dest.Position := 0;
          FCompleteBufferReceiveStream := dest;
        end;

      IO_SyncMethod(ACurrentActiveThread, Sync, {$IFDEF FPC}@{$ENDIF FPC}Sync_ExecuteCompleteBuffer);
      FCompleteBufferReceiveStream.Clear;

      Result := True;

      FCompleteBufferTotal := 0;
      FCompleteBufferCompressedSize := 0;
      FCompleteBufferCompleted := 0;
      FCompleteBufferCmd := '';
      FCompleteBufferReceiveProcessing := False;

      FReceivedBuffer.Position := 0;
    end;
end;

procedure TPeerIO.Sync_ExecuteResult;
begin
  if FCurrentQueueData = nil then
      exit;

  if (FOwnerFramework.FSyncOnResult) then
    begin
      DoExecuteResult(Self, FCurrentQueueData, ResultText, ResultDataFrame);
      exit;
    end;

  with FOwnerFramework.ProgressPost.PostExecute() do
    begin
      DataEng.Assign(ResultDataFrame);
      Data4 := FID;
      Data5 := FCurrentQueueData;
      Data3 := ResultText;
      OnExecuteMethod := {$IFDEF FPC}@{$ENDIF FPC}FOwnerFramework.DelayExecuteOnResultState;
    end;
  FCurrentQueueData := nil;
end;

function TPeerIO.FillWaitOnResultBuffer(ACurrentActiveThread: TCoreClassThread; const RecvSync, SendSync: Boolean): Boolean;
var
  dHead, dTail: Cardinal;
  dSize: Integer;
  dHashSecurity: Byte;
  dHashSiz: Word;
  dHash: TBytes;
  dCipherSecurity: Byte;
  tmpStream: TMemoryStream64;
  buff: TBytes;
begin
  Result := False;
  if not FWaitOnResult then
      exit;
  if FCurrentQueueData = nil then
      exit;

  FReceivedBuffer.Position := 0;

  // 0: head token
  if (FReceivedBuffer.Size - FReceivedBuffer.Position < C_Cardinal_Size) then
      exit;
  FReceivedBuffer.read(dHead, C_Cardinal_Size);
  if dHead <> FHeadToken then
    begin
      Print('Header Illegal');
      DelayClose();
      exit;
    end;

  // 1: data len
  if (FReceivedBuffer.Size - FReceivedBuffer.Position < C_Integer_Size) then
      exit;
  FReceivedBuffer.read(dSize, C_Integer_Size);

  // 2:verify code header
  if (FReceivedBuffer.Size - FReceivedBuffer.Position < 3) then
      exit;
  FReceivedBuffer.read(dHashSecurity, C_Byte_Size);
  FReceivedBuffer.read(dHashSiz, C_Word_Size);

  // 3:verify code body
  if (FReceivedBuffer.Size - FReceivedBuffer.Position < dHashSiz) then
      exit;
  SetLength(dHash, dHashSiz);
  if length(dHash) > 0 then
      FReceivedBuffer.read(dHash[0], dHashSiz);

  // 4: use Encrypt state
  if (FReceivedBuffer.Size - FReceivedBuffer.Position < C_Byte_Size) then
      exit;
  FReceivedBuffer.read(dCipherSecurity, C_Byte_Size);

  // 5:process buff and tail token
  if (FReceivedBuffer.Size - FReceivedBuffer.Position < dSize + C_Cardinal_Size) then
      exit;
  SetLength(buff, dSize);
  if length(buff) > 0 then
      FReceivedBuffer.read(buff[0], dSize);

  // 6: tail token
  FReceivedBuffer.read(dTail, C_Cardinal_Size);
  if dTail <> FTailToken then
    begin
      Print('tail token error!');
      DelayClose();
      exit;
    end;

  FReceiveDataCipherSecurity := TCipherSecurity(dCipherSecurity);

  try
    if length(buff) > 0 then
      begin
        Encrypt(FReceiveDataCipherSecurity, @buff[0], dSize, FCipherKey, False);
        if not VerifyHashCode(THashSecurity(dHashSecurity), @buff[0], dSize, dHash) then
          begin
            Print('verify data error!');
            DelayClose();
            exit;
          end;
      end;
  except
    Print('Encrypt error!');
    DelayClose();
    exit;
  end;

  // stripped stream
  tmpStream := TMemoryStream64.Create;
  if FReceivedBuffer.Size - FReceivedBuffer.Position > 0 then
      tmpStream.CopyFrom(FReceivedBuffer, FReceivedBuffer.Size - FReceivedBuffer.Position);
  DisposeObject(FReceivedBuffer);
  FReceivedBuffer := tmpStream;
  FReceivedBuffer.Position := 0;

{$IFDEF FPC}
  if Assigned(FCurrentQueueData^.OnConsoleMethod) then
{$ELSE}
  if Assigned(FCurrentQueueData^.OnConsoleMethod) or
    Assigned(FCurrentQueueData^.OnConsoleProc) then
{$ENDIF}
    begin
      try
        ResultText := umlStringOf(buff).Text;
        SetLength(buff, 0);
        ResultDataFrame.Clear;
      except
        Print('WaitOnResultBuffer console data error!');
        DelayClose();
        exit;
      end;

      IO_SyncMethod(ACurrentActiveThread, RecvSync, {$IFDEF FPC}@{$ENDIF FPC}Sync_ExecuteResult);

      AtomInc(FOwnerFramework.Statistics[TStatisticsType.stResponse]);
    end
  else
{$IFDEF FPC}
    if Assigned(FCurrentQueueData^.OnStreamMethod) or
      Assigned(FCurrentQueueData^.OnStreamParamMethod) then
{$ELSE}
    if Assigned(FCurrentQueueData^.OnStreamMethod) or
      Assigned(FCurrentQueueData^.OnStreamParamMethod) or
      Assigned(FCurrentQueueData^.OnStreamProc) or
      Assigned(FCurrentQueueData^.OnStreamParamProc) then
{$ENDIF}
      begin
        ResultDataFrame.Clear;
        try
          ResultDataFrame.DecodeFromBytes(buff, True);
          SetLength(buff, 0);
          ResultText := '';
        except
          Print('WaitOnResultBuffer stream error!');
          DelayClose();
          exit;
        end;

        IO_SyncMethod(ACurrentActiveThread, RecvSync, {$IFDEF FPC}@{$ENDIF FPC}Sync_ExecuteResult);

        AtomInc(FOwnerFramework.Statistics[TStatisticsType.stResponse]);
      end;

  FWaitOnResult := False;

  if FCurrentQueueData <> nil then
    begin
      DisposeQueueData(FCurrentQueueData);
      FCurrentQueueData := nil;
    end;

  Result := True;
end;

procedure TPeerIO.InternalSaveReceiveBuffer(const buff: Pointer; siz: Int64);
begin
  UpdateLastCommunicationTime;
  AtomInc(FOwnerFramework.Statistics[TStatisticsType.stReceiveSize], siz);

  LockIO;
  try
    if FReceiveProcessing or FAllSendProcessing then
      begin
        FReceivedBuffer_Busy.Position := FReceivedBuffer_Busy.Size;
        FReceivedBuffer_Busy.WritePtr(buff, siz);
      end
    else
      begin
        FReceivedBuffer.Position := FReceivedBuffer.Size;
        if FReceivedBuffer_Busy.Size > 0 then
          begin
            FReceivedBuffer.WritePtr(FReceivedBuffer_Busy.Memory, FReceivedBuffer_Busy.Size);
            FReceivedBuffer_Busy.Clear;
          end;
        FReceivedBuffer.WritePtr(buff, siz);
      end;
  finally
      UnLockIO;
  end;
end;

procedure TPeerIO.InternalProcessReceiveBuffer(const ACurrentActiveThread: TCoreClassThread; const RecvSync, SendSync: Boolean);
var
  rState: Boolean;
  dHead, dTail: Cardinal;
  dID: Byte;
  dSize: Cardinal;
  dHashSecurity: Byte;
  dHashSiz: Word;
  dHash: TBytes;
  dCipherSecurity: Byte;
  tmpStream: TMemoryStream64;
  df: TDataFrameEngine;
  buff: TBytes;
  Total: Int64;
  sourSiz, compSiz: Cardinal;
  BreakAndDisconnect: Boolean;
begin
  if FAllSendProcessing or
    FReceiveProcessing or
    FPauseResultSend or
    (FResultDataBuffer.Size > 0) or
    FReceiveTriggerRuning or
    (FBigStreamSending <> nil) then
    begin
      exit;
    end;

  FReceiveProcessing := True;

  LockIO;

  BreakAndDisconnect := False;

  try
    while ((FReceivedBuffer.Size > 0) or (FReceivedBuffer_Busy.Size > 0)) and (Connected) do
      begin
        if FReceivedBuffer_Busy.Size > 0 then
          begin
            FReceivedBuffer.Position := FReceivedBuffer.Size;
            FReceivedBuffer.WritePtr(FReceivedBuffer_Busy.Memory, FReceivedBuffer_Busy.Size);
            FReceivedBuffer_Busy.Clear;
          end;

        FReceivedBuffer.Position := 0;

        if FWaitOnResult then
          begin
            rState := FillWaitOnResultBuffer(ACurrentActiveThread, RecvSync, SendSync);

            if rState then
                Continue
            else
                Break;
          end;

        if FBigStreamReceiveProcessing then
          begin
            rState := FillBigStreamBuffer(ACurrentActiveThread, RecvSync);

            if rState then
                Continue
            else
                Break;
          end;

        if FCompleteBufferReceiveProcessing then
          begin
            rState := FillCompleteBufferBuffer(ACurrentActiveThread, RecvSync);

            if rState then
                Continue
            else
                Break;
          end;

        // 0: head token
        if (FReceivedBuffer.Size - FReceivedBuffer.Position < C_Cardinal_Size + C_Byte_Size) then
            Break;
        FReceivedBuffer.read(dHead, C_Cardinal_Size);
        if dHead <> FHeadToken then
          begin
            BreakAndDisconnect := True;
            Break;
          end;
        // 1: data type
        FReceivedBuffer.read(dID, C_Byte_Size);

        if dID = FBigStreamToken then
          begin
            // 2:stream size
            if (FReceivedBuffer.Size - FReceivedBuffer.Position < C_Int64_Size) then
                Break;
            FReceivedBuffer.read(Total, C_Int64_Size);

            // 3:command len
            if (FReceivedBuffer.Size - FReceivedBuffer.Position < C_Cardinal_Size) then
                Break;
            FReceivedBuffer.read(dSize, C_Cardinal_Size);

            // 4:command and tial token
            if (FReceivedBuffer.Size - FReceivedBuffer.Position < dSize + C_Cardinal_Size) then
                Break;
            SetLength(buff, dSize);
            if dSize > 0 then
                FReceivedBuffer.read(buff[0], dSize);

            // 5: process tail token
            FReceivedBuffer.read(dTail, C_Cardinal_Size);
            if dTail <> FTailToken then
              begin
                PrintError('tail error!');
                BreakAndDisconnect := True;
                Break;
              end;

            FBigStreamTotal := Total;
            FBigStreamCompleted := 0;
            FBigStreamCmd := umlStringOf(buff).Text;
            FBigStreamReceiveProcessing := True;
            SetLength(buff, 0);

            // stripped stream
            tmpStream := TMemoryStream64.Create;
            if FReceivedBuffer.Size - FReceivedBuffer.Position > 0 then
                tmpStream.CopyFrom(FReceivedBuffer, FReceivedBuffer.Size - FReceivedBuffer.Position);
            DisposeObject(FReceivedBuffer);
            FReceivedBuffer := tmpStream;

            AtomInc(FOwnerFramework.Statistics[TStatisticsType.stReceiveBigStream]);
          end
        else if dID = FCompleteBufferToken then
          begin
            // 1:complete buff size
            if (FReceivedBuffer.Size - FReceivedBuffer.Position < C_Cardinal_Size * 3) then
                Break;
            FReceivedBuffer.read(sourSiz, C_Cardinal_Size);
            FReceivedBuffer.read(compSiz, C_Cardinal_Size);
            FReceivedBuffer.read(dSize, C_Cardinal_Size);

            // 2:command and tial token
            if (FReceivedBuffer.Size - FReceivedBuffer.Position < dSize + C_Cardinal_Size) then
                Break;
            SetLength(buff, dSize);
            if length(buff) > 0 then
                FReceivedBuffer.read(buff[0], dSize);

            // 3: process tail token
            FReceivedBuffer.read(dTail, C_Cardinal_Size);
            if dTail <> FTailToken then
              begin
                PrintError('tail error!');
                BreakAndDisconnect := True;
                Break;
              end;

            if (FOwnerFramework.FMaxCompleteBufferSize > 0) and (sourSiz > FOwnerFramework.FMaxCompleteBufferSize) then
              begin
                PrintError('Oversize of CompleteBuffer cmd: ' + umlStringOf(buff).Text);
                BreakAndDisconnect := True;
                Break;
              end;

            if compSiz > 0 then
                FCompleteBufferTotal := compSiz
            else
                FCompleteBufferTotal := sourSiz;
            FCompleteBufferCompressedSize := compSiz;
            FCompleteBufferCompleted := 0;
            FCompleteBufferCmd := umlStringOf(buff).Text;
            FCompleteBufferReceiveProcessing := True;
            FCompleteBufferReceiveStream.Clear;
            FCompleteBufferReceiveStream.Delta := 1024 * 8;
            SetLength(buff, 0);

            // stripped stream
            tmpStream := TMemoryStream64.Create;
            if FReceivedBuffer.Size - FReceivedBuffer.Position > 0 then
                tmpStream.CopyFrom(FReceivedBuffer, FReceivedBuffer.Size - FReceivedBuffer.Position);
            DisposeObject(FReceivedBuffer);
            FReceivedBuffer := tmpStream;

            AtomInc(FOwnerFramework.Statistics[TStatisticsType.stReceiveCompleteBuffer]);
          end
        else if dID in [FConsoleToken, FStreamToken, FDirectConsoleToken, FDirectStreamToken] then
          begin
            // 2: size
            if (FReceivedBuffer.Size - FReceivedBuffer.Position < C_Cardinal_Size) then
                Break;
            FReceivedBuffer.read(dSize, C_Cardinal_Size);

            // 3:verify code header
            if (FReceivedBuffer.Size - FReceivedBuffer.Position < 3) then
                Break;
            FReceivedBuffer.read(dHashSecurity, C_Byte_Size);
            FReceivedBuffer.read(dHashSiz, C_Word_Size);

            // 4:verify code body
            if (FReceivedBuffer.Size - FReceivedBuffer.Position < dHashSiz) then
                Break;
            SetLength(dHash, dHashSiz);
            if length(dHash) > 0 then
                FReceivedBuffer.read(dHash[0], dHashSiz);

            // 5: Encrypt style
            if (FReceivedBuffer.Size - FReceivedBuffer.Position < C_Byte_Size) then
                Break;
            FReceivedBuffer.read(dCipherSecurity, C_Byte_Size);

            // 6: process stream
            if (FReceivedBuffer.Size - FReceivedBuffer.Position < dSize + C_Cardinal_Size) then
                Break;
            tmpStream := TMemoryStream64.Create;
            tmpStream.SetPointerWithProtectedMode(FReceivedBuffer.PositionAsPtr, dSize);
            FReceivedBuffer.Position := FReceivedBuffer.Position + dSize;

            // 7: process tail token
            FReceivedBuffer.read(dTail, C_Cardinal_Size);
            if dTail <> FTailToken then
              begin
                PrintError('tail error!');
                BreakAndDisconnect := True;
                Break;
              end;

            FReceiveDataCipherSecurity := TCipherSecurity(dCipherSecurity);

            try
                Encrypt(FReceiveDataCipherSecurity, tmpStream.Memory, tmpStream.Size, FCipherKey, False);
            except
              PrintError('Encrypt error!');
              DisposeObject(tmpStream);
              BreakAndDisconnect := True;
              Break;
            end;

            if not VerifyHashCode(THashSecurity(dHashSecurity), tmpStream.Memory, tmpStream.Size, dHash) then
              begin
                PrintError('verify data error!');
                DisposeObject(tmpStream);
                BreakAndDisconnect := True;
                Break;
              end;

            df := TDataFrameEngine.Create;
            tmpStream.Position := 0;
            try
                df.DecodeFrom(tmpStream, True);
            except
              PrintError('decrypt dataFrame error!');
              DisposeObject(tmpStream);
              BreakAndDisconnect := True;
              Break;
            end;
            DisposeObject(tmpStream);

            // stripped stream
            tmpStream := TMemoryStream64.Create;
            if FReceivedBuffer.Size - FReceivedBuffer.Position > 0 then
                tmpStream.CopyFrom(FReceivedBuffer, FReceivedBuffer.Size - FReceivedBuffer.Position);
            DisposeObject(FReceivedBuffer);
            FReceivedBuffer := tmpStream;

            try
                ExecuteDataFrame(ACurrentActiveThread, RecvSync, SendSync, dID, df);
            except
              PrintError('run procedure on dataFrame error!');
              DisposeObject(df);
              BreakAndDisconnect := True;
              Break;
            end;
            DisposeObject(df);

            AtomInc(FOwnerFramework.Statistics[TStatisticsType.stRequest]);
          end
        else
          begin
            BreakAndDisconnect := True;
            Break;
          end;
      end;
  finally
    FReceivedBuffer.Position := FReceivedBuffer.Size;
    FReceiveProcessing := False;
    UnLockIO;

    if BreakAndDisconnect then
        DelayClose()
    else if FReceivedBuffer_Busy.Size > 0 then
        FillRecvBuffer(ACurrentActiveThread, RecvSync, SendSync)
    else
        ProcessAllSendCmd(ACurrentActiveThread, RecvSync, SendSync);
  end;
end;

procedure TPeerIO.InternalProcessAllSendCmd(const ACurrentActiveThread: TCoreClassThread; const RecvSync, SendSync: Boolean);
var
  p: PQueueData;
begin
  if FAllSendProcessing or
    FReceiveProcessing or
    FWaitOnResult or
    FBigStreamReceiveProcessing or
    (FBigStreamSending <> nil) or
    FReceiveTriggerRuning then
    begin
      exit;
    end;

  FAllSendProcessing := True;

  LockIO;

  if FResultDataBuffer.Size > 0 then
    begin
      IO_SyncMethod(ACurrentActiveThread, SendSync, {$IFDEF FPC}@{$ENDIF FPC}Sync_InternalSendResultData);
      FAllSendProcessing := False;
      UnLockIO;
      exit;
    end;

  try
    while FQueueList.Count > 0 do
      begin
        if not Connected then
            Break;
        if FWaitOnResult then
            Break;
        p := FQueueList[0];
        FCurrentQueueData := p;
        case p^.State of
          qsSendConsoleCMD:
            begin
              AtomInc(FOwnerFramework.Statistics[TStatisticsType.stConsole]);

              FSyncPick := p;
              FWaitOnResult := True;
              IO_SyncMethod(ACurrentActiveThread, SendSync, {$IFDEF FPC}@{$ENDIF FPC}Sync_InternalSendConsoleCmd);

              FSyncPick := nil;

              FQueueList.Delete(0);
              Break;
            end;
          qsSendStreamCMD:
            begin
              AtomInc(FOwnerFramework.Statistics[TStatisticsType.stStream]);

              FSyncPick := p;
              FWaitOnResult := True;
              IO_SyncMethod(ACurrentActiveThread, SendSync, {$IFDEF FPC}@{$ENDIF FPC}Sync_InternalSendStreamCmd);

              FSyncPick := nil;

              FQueueList.Delete(0);
              Break;
            end;
          qsSendDirectConsoleCMD:
            begin
              AtomInc(FOwnerFramework.Statistics[TStatisticsType.stDirestConsole]);

              FSyncPick := p;
              IO_SyncMethod(ACurrentActiveThread, SendSync, {$IFDEF FPC}@{$ENDIF FPC}Sync_InternalSendDirectConsoleCmd);

              FSyncPick := nil;

              DisposeQueueData(p);
              FQueueList.Delete(0);
            end;
          qsSendDirectStreamCMD:
            begin
              AtomInc(FOwnerFramework.Statistics[TStatisticsType.stDirestStream]);

              FSyncPick := p;
              IO_SyncMethod(ACurrentActiveThread, SendSync, {$IFDEF FPC}@{$ENDIF FPC}Sync_InternalSendDirectStreamCmd);

              FSyncPick := nil;

              DisposeQueueData(p);
              FQueueList.Delete(0);
            end;
          qsSendBigStream:
            begin
              AtomInc(FOwnerFramework.Statistics[TStatisticsType.stSendBigStream]);

              FSyncPick := p;
              IO_SyncMethod(ACurrentActiveThread, SendSync, {$IFDEF FPC}@{$ENDIF FPC}Sync_InternalSendBigStreamCmd);

              FSyncPick := nil;

              DisposeQueueData(p);
              FQueueList.Delete(0);

              if FBigStreamSending <> nil then
                  Break;
            end;
          qsSendCompleteBuffer:
            begin
              AtomInc(FOwnerFramework.Statistics[TStatisticsType.stSendCompleteBuffer]);

              FSyncPick := p;
              IO_SyncMethod(ACurrentActiveThread, SendSync, {$IFDEF FPC}@{$ENDIF FPC}Sync_InternalSendCompleteBufferCmd);

              FSyncPick := nil;

              DisposeQueueData(p);
              FQueueList.Delete(0);
            end;
        end;
      end;
  finally
    FAllSendProcessing := False;
    UnLockIO;
    if FReceivedBuffer_Busy.Size > 0 then
        FillRecvBuffer(ACurrentActiveThread, RecvSync, SendSync);
  end;
end;

procedure TPeerIO.InternalCloseP2PVMTunnel;
begin
  if FP2PVMTunnel <> nil then
    begin
      FOwnerFramework.p2pVMTunnelClose(Self, FP2PVMTunnel);
      FP2PVMTunnel.CloseP2PVMTunnel;
      DisposeObject(FP2PVMTunnel);
      FP2PVMTunnel := nil;
      SetLength(FP2PAuthToken, 0);
    end;
end;

function TPeerIO.WriteBufferEmpty: Boolean;
begin
  Result := True;
end;

constructor TPeerIO.Create(AOwnerFramework: TCommunicationFramework; AClientIntf: TCoreClassObject);
var
  kref: TInt64;
begin
  inherited Create;

  FLockedObject := TCoreClassObject.Create;
  FOwnerFramework := AOwnerFramework;
  FClientIntf := AClientIntf;

  FID := AOwnerFramework.FIDCounter;

  // only ID
  AtomInc(AOwnerFramework.FIDCounter);

  FOwnerFramework.LockClients;
  while (AOwnerFramework.FIDCounter = 0) or (AOwnerFramework.FPeerIO_HashPool.Exists(AOwnerFramework.FIDCounter)) do
      AtomInc(AOwnerFramework.FIDCounter);
  FOwnerFramework.UnLockClients;

  FHeadToken := c_DataHeadToken;
  FTailToken := c_DataTailToken;

  FConsoleToken := c_DefaultConsoleToken;
  FStreamToken := c_DefaultStreamToken;
  FDirectConsoleToken := c_DefaultDirectConsoleToken;
  FDirectStreamToken := c_DefaultDirectStreamToken;
  FBigStreamToken := c_DefaultBigStreamToken;
  FCompleteBufferToken := c_DefaultCompleteBufferToken;

  FReceivedBuffer := TMemoryStream64.Create;
  FReceivedBuffer_Busy := TMemoryStream64.Create;
  FBigStreamReceiveProcessing := False;
  FBigStreamTotal := 0;
  FBigStreamCompleted := 0;
  FBigStreamCmd := '';
  FBigStreamReceive := nil;
  FBigStreamSending := nil;
  FBigStreamSendState := -1;
  FBigStreamSendDoneTimeFree := False;

  FCompleteBufferReceiveProcessing := False;
  FCompleteBufferTotal := 0;
  FCompleteBufferCompressedSize := 0;
  FCompleteBufferCompleted := 0;
  FCompleteBufferCmd := '';
  FCompleteBufferReceiveStream := TMemoryStream64.Create;

  FCurrentQueueData := nil;
  FWaitOnResult := False;
  FPauseResultSend := False;
  FReceiveTriggerRuning := False;
  FReceiveDataCipherSecurity := TCipherSecurity.csNone;
  FResultDataBuffer := TMemoryStream64.Create;
  FSendDataCipherSecurity := FOwnerFramework.FCipherSecurity;
  FCanPauseResultSend := False;
  FQueueList := TCoreClassList.Create;
  UpdateLastCommunicationTime;

  // generate random key
  TMISC.GenerateRandomKey(kref, C_Int64_Size);
  TCipher.GenerateKey(FSendDataCipherSecurity, @kref, C_Int64_Size, FCipherKey);

  FRemoteExecutedForConnectInit := False;

  FAllSendProcessing := False;
  FReceiveProcessing := False;

  FInCmd := '';
  FInText := '';
  FOutText := '';
  FInDataFrame := TDataFrameEngine.Create;
  FOutDataFrame := TDataFrameEngine.Create;
  ResultText := '';
  ResultDataFrame := TDataFrameEngine.Create;
  FSyncPick := nil;

  FWaitSendBusy := False;
  FReceiveCommandRuning := False;
  FReceiveResultRuning := False;

  FProgressRunning := False;
  FTimeOutProcessDone := False;

  AtomInc(FOwnerFramework.Statistics[TStatisticsType.stTriggerConnect]);

  FP2PVMTunnel := nil;
  SetLength(FP2PAuthToken, $FF);
  FillPtrByte(@FP2PAuthToken[0], length(FP2PAuthToken), $0);

  OnInternalSendByteBuffer := {$IFDEF FPC}@{$ENDIF FPC}FOwnerFramework.InternalSendByteBuffer;
  OnInternalSaveReceiveBuffer := {$IFDEF FPC}@{$ENDIF FPC}FOwnerFramework.InternalSaveReceiveBuffer;
  OnInternalProcessReceiveBuffer := {$IFDEF FPC}@{$ENDIF FPC}FOwnerFramework.InternalProcessReceiveBuffer;
  OnInternalProcessAllSendCmd := {$IFDEF FPC}@{$ENDIF FPC}FOwnerFramework.InternalProcessAllSendCmd;
  OnCreate := {$IFDEF FPC}@{$ENDIF FPC}FOwnerFramework.InternalClientCreate;
  OnDestroy := {$IFDEF FPC}@{$ENDIF FPC}FOwnerFramework.InternalClientDestroy;
  //
  OnVMBuildAuthModelResultCall := nil;
  OnVMBuildAuthModelResultMethod := nil;
{$IFNDEF FPC} OnVMBuildAuthModelResultProc := nil; {$ENDIF FPC}
  OnVMAuthResultCall := nil;
  OnVMAuthResultMethod := nil;
{$IFNDEF FPC} OnVMAuthResultProc := nil; {$ENDIF FPC}
  FUserData := nil;
  FUserValue := Null;
  FUserVariants := nil;
  FUserObjects := nil;
  FUserAutoFreeObjects := nil;

  try
      FUserDefine := FOwnerFramework.FPeerIOUserDefineClass.Create(Self);
  except
  end;

  try
      FUserSpecial := FOwnerFramework.FPeerIOUserSpecialClass.Create(Self);
  except
  end;

  try
      OnCreate(Self);
  except
  end;

  try
      CreateAfter;
  except
  end;

  FOwnerFramework.LockClients;
  FOwnerFramework.FPeerIO_HashPool.Add(FID, Self, False);
  FOwnerFramework.UnLockClients;
end;

procedure TPeerIO.CreateAfter;
begin
end;

destructor TPeerIO.Destroy;
var
  i: Integer;
begin
  LockIO;
  try
      OnDestroy(Self);
  except
  end;
  UnLockIO;

  InternalCloseP2PVMTunnel;

  if (FCurrentQueueData <> nil) and (FWaitOnResult) then
    begin
      DisposeQueueData(FCurrentQueueData);
      FCurrentQueueData := nil;
    end;

  if (FBigStreamSending <> nil) and (FBigStreamSendDoneTimeFree) then
    begin
      DisposeObject(FBigStreamSending);
      FBigStreamSending := nil;
    end;

  AtomInc(FOwnerFramework.Statistics[TStatisticsType.stTriggerDisconnect]);

  FOwnerFramework.LockClients;
  FOwnerFramework.FPeerIO_HashPool.Delete(FID);
  FOwnerFramework.UnLockClients;

  LockIO;
  try
    for i := 0 to FQueueList.Count - 1 do
        DisposeQueueData(FQueueList[i]);
    FQueueList.Clear;
  finally
      UnLockIO;
  end;

  DisposeObject(FUserDefine);
  DisposeObject(FUserSpecial);

  DisposeObject(FQueueList);
  DisposeObject(FReceivedBuffer);
  DisposeObject(FReceivedBuffer_Busy);
  DisposeObject(FCompleteBufferReceiveStream);
  DisposeObject(FResultDataBuffer);
  DisposeObject(FInDataFrame);
  DisposeObject(FOutDataFrame);
  DisposeObject(ResultDataFrame);

  DisposeObject(FLockedObject);

  if FUserVariants <> nil then
      DisposeObject(FUserVariants);
  if FUserObjects <> nil then
      DisposeObject(FUserObjects);
  if FUserAutoFreeObjects <> nil then
      DisposeObject(FUserAutoFreeObjects);
  inherited Destroy;
end;

procedure TPeerIO.BuildP2PAuthToken;
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  de.WriteInteger(umlRandomRange(-maxInt, maxInt));
  SendStreamCmdM(C_BuildP2PAuthToken, de, {$IFDEF FPC}@{$ENDIF FPC}FOwnerFramework.CommandResult_BuildP2PAuthToken);
  DisposeObject(de);
  InternalProcessAllSendCmd(nil, False, False);
  OnVMBuildAuthModelResultCall := nil;
  OnVMBuildAuthModelResultMethod := nil;
{$IFNDEF FPC} OnVMBuildAuthModelResultProc := nil; {$ENDIF FPC}
end;

procedure TPeerIO.BuildP2PAuthTokenC(const OnResult: TNotifyCall);
begin
  BuildP2PAuthToken;
  OnVMBuildAuthModelResultCall := OnResult;
end;

procedure TPeerIO.BuildP2PAuthTokenM(const OnResult: TNotifyMethod);
begin
  BuildP2PAuthToken;
  OnVMBuildAuthModelResultMethod := OnResult;
end;

{$IFNDEF FPC}


procedure TPeerIO.BuildP2PAuthTokenP(const OnResult: TNotifyProc);
begin
  BuildP2PAuthToken;
  OnVMBuildAuthModelResultProc := OnResult;
end;
{$ENDIF FPC}


procedure TPeerIO.OpenP2PVMTunnel(vmHashPoolLen: Integer; SendRemoteRequest: Boolean; const AuthToken: SystemString);
begin
  if FP2PVMTunnel = nil then
    begin
      while (FQueueList.Count > 0) or (not WriteBufferEmpty) do
        begin
          PrintError('failed open p2pVM tunnel:io busy!');
          exit;
        end;

      FP2PVMTunnel := TCommunicationFrameworkWithP2PVM.Create(vmHashPoolLen);
      FP2PVMTunnel.FVMID := FID;

      FP2PVMTunnel.OpenP2PVMTunnel(Self);
      FP2PVMTunnel.AuthWaiting;

      FP2PVMTunnel.OnAuthSuccessOnesNotify := {$IFDEF FPC}@{$ENDIF FPC}P2PVMAuthSuccess;
      OnVMAuthResultCall := nil;
      OnVMAuthResultMethod := nil;
{$IFNDEF FPC} OnVMAuthResultProc := nil; {$ENDIF FPC}
      if SendRemoteRequest then
        begin
          SendDirectConsoleCmd(C_InitP2PTunnel, AuthToken);
          InternalProcessAllSendCmd(nil, False, False);
        end;
    end;
end;

procedure TPeerIO.OpenP2PVMTunnel(SendRemoteRequest: Boolean; const AuthToken: SystemString);
begin
  if FOwnerFramework.FFrameworkIsClient then
      OpenP2PVMTunnel(8192, SendRemoteRequest, AuthToken)
  else
      OpenP2PVMTunnel(16, SendRemoteRequest, AuthToken);
end;

procedure TPeerIO.OpenP2PVMTunnelC(SendRemoteRequest: Boolean; const AuthToken: SystemString; OnResult: TStateCall);
begin
  OpenP2PVMTunnel(SendRemoteRequest, AuthToken);
  OnVMAuthResultCall := OnResult;
  OnVMAuthResultMethod := nil;
{$IFNDEF FPC} OnVMAuthResultProc := nil; {$ENDIF FPC}
  FOwnerFramework.ProgressPost.PostExecuteM(5.0, {$IFDEF FPC}@{$ENDIF FPC}FOwnerFramework.VMAuthFailedDelayExecute).Data3 := FID;
end;

procedure TPeerIO.OpenP2PVMTunnelM(SendRemoteRequest: Boolean; const AuthToken: SystemString; OnResult: TStateMethod);
begin
  OpenP2PVMTunnel(SendRemoteRequest, AuthToken);
  OnVMAuthResultCall := nil;
  OnVMAuthResultMethod := OnResult;
{$IFNDEF FPC} OnVMAuthResultProc := nil; {$ENDIF FPC}
  FOwnerFramework.ProgressPost.PostExecuteM(5.0, {$IFDEF FPC}@{$ENDIF FPC}FOwnerFramework.VMAuthFailedDelayExecute).Data3 := FID;
end;

{$IFNDEF FPC}


procedure TPeerIO.OpenP2PVMTunnelP(SendRemoteRequest: Boolean; const AuthToken: SystemString; OnResult: TStateProc);
begin
  OpenP2PVMTunnel(SendRemoteRequest, AuthToken);
  OnVMAuthResultCall := nil;
  OnVMAuthResultMethod := nil;
  OnVMAuthResultProc := OnResult;
  FOwnerFramework.ProgressPost.PostExecuteM(5.0, FOwnerFramework.VMAuthFailedDelayExecute).Data3 := FID;
end;
{$ENDIF FPC}


procedure TPeerIO.OpenP2PVMTunnelC(vmHashPoolLen: Integer; SendRemoteRequest: Boolean; const AuthToken: SystemString; OnResult: TStateCall);
begin
  OpenP2PVMTunnel(vmHashPoolLen, SendRemoteRequest, AuthToken);
  OnVMAuthResultCall := OnResult;
  OnVMAuthResultMethod := nil;
{$IFNDEF FPC}
  OnVMAuthResultProc := nil;
{$ENDIF FPC}
  FOwnerFramework.ProgressPost.PostExecuteM(5.0, {$IFDEF FPC}@{$ENDIF FPC}FOwnerFramework.VMAuthFailedDelayExecute).Data3 := FID;
end;

procedure TPeerIO.OpenP2PVMTunnelM(vmHashPoolLen: Integer; SendRemoteRequest: Boolean; const AuthToken: SystemString; OnResult: TStateMethod);
begin
  OpenP2PVMTunnel(vmHashPoolLen, SendRemoteRequest, AuthToken);
  OnVMAuthResultCall := nil;
  OnVMAuthResultMethod := OnResult;
{$IFNDEF FPC}
  OnVMAuthResultProc := nil;
{$ENDIF FPC}
  FOwnerFramework.ProgressPost.PostExecuteM(5.0, {$IFDEF FPC}@{$ENDIF FPC}FOwnerFramework.VMAuthFailedDelayExecute).Data3 := FID;
end;

{$IFNDEF FPC}


procedure TPeerIO.OpenP2PVMTunnelP(vmHashPoolLen: Integer; SendRemoteRequest: Boolean; const AuthToken: SystemString; OnResult: TStateProc);
begin
  OpenP2PVMTunnel(vmHashPoolLen, SendRemoteRequest, AuthToken);
  OnVMAuthResultCall := nil;
  OnVMAuthResultMethod := nil;
  OnVMAuthResultProc := OnResult;
  FOwnerFramework.ProgressPost.PostExecuteM(5.0, FOwnerFramework.VMAuthFailedDelayExecute).Data3 := FID;
end;
{$ENDIF FPC}


procedure TPeerIO.OpenP2PVMTunnel;
begin
  OpenP2PVMTunnel(False, '');
end;

procedure TPeerIO.CloseP2PVMTunnel;
begin
  SendDirectConsoleCmd(C_CloseP2PTunnel, '');
end;

procedure TPeerIO.PrintError(v: SystemString);
var
  n: SystemString;
begin
  n := GetPeerIP;
  if n <> '' then
      OwnerFramework.DoPrint(Format('error:%s %s %s', [n, DateTimeToStr(Now), v]))
  else
      OwnerFramework.DoPrint(Format('error:%s %s', [DateTimeToStr(Now), v]));
end;

procedure TPeerIO.Print(v: SystemString);
var
  n: SystemString;
begin
  n := GetPeerIP;
  if n <> '' then
      OwnerFramework.DoPrint(Format('%s %s %s', [n, DateTimeToStr(Now), v]))
  else
      OwnerFramework.DoPrint(Format('%s %s', [DateTimeToStr(Now), v]));
end;

procedure TPeerIO.Print(v: SystemString; const Args: array of const);
begin
  Print(Format(v, Args));
end;

procedure TPeerIO.PrintCommand(v: SystemString; Args: SystemString);
begin
  try
    if (not OwnerFramework.FQuietMode) and (OwnerFramework.FPrintParams.GetDefaultValue(Args, True) = True) then
        Print(Format(v, [Args]));
  except
      Print(Format(v, [Args]));
  end;
  AtomInc(FOwnerFramework.Statistics[TStatisticsType.stPrint]);
end;

procedure TPeerIO.PrintParam(v: SystemString; Args: SystemString);
begin
  try
    if (OwnerFramework.FPrintParams.GetDefaultValue(Args, True) = True) then
        Print(Format(v, [Args]));
  except
      Print(Format(v, [Args]));
  end;
  AtomInc(FOwnerFramework.Statistics[TStatisticsType.stPrint]);
end;

procedure TPeerIO.LockIO;
begin
  if FOwnerFramework.FEnabledAtomicLockAndMultiThread then
      LockObject(FLockedObject);
end;

procedure TPeerIO.UnLockIO;
begin
  if FOwnerFramework.FEnabledAtomicLockAndMultiThread then
      UnLockObject(FLockedObject);
end;

procedure TPeerIO.IO_SyncMethod(t: TCoreClassThread; Sync: Boolean; proc: TThreadMethod);
begin
  if FOwnerFramework.FEnabledAtomicLockAndMultiThread then
      SyncMethod(t, Sync, proc)
  else
    begin
      try
          proc();
      except
      end;
    end;
end;

procedure TPeerIO.Progress;
var
  SendBufferSize: Integer;
  buff: TBytes;
  SendDone: Boolean;
begin
  // anti dead loop
  if FProgressRunning then
      exit;

  // anti dead loop
  FProgressRunning := True;

  if FP2PVMTunnel <> nil then
    begin
      try
          FP2PVMTunnel.Progress;
      except
      end;
    end;

  try
      FUserDefine.Progress;
  except
  end;

  try
      FUserSpecial.Progress;
  except
  end;

  if FOwnerFramework.Protocol = TCommunicationProtocol.cpZServer then
    begin
      if FAllSendProcessing or FReceiveProcessing or FWaitOnResult then
        begin
          FProgressRunning := False;
          exit;
        end;

      if (FBigStreamSending <> nil) and (WriteBufferEmpty) then
        begin
          SendBufferSize := 1 * 1024 * 1024; // cycle send size 4M

          try
            SendDone := FBigStreamSending.Size - FBigStreamSendState <= SendBufferSize;

            if SendDone then
                SendBufferSize := FBigStreamSending.Size - FBigStreamSendState;

            SetLength(buff, SendBufferSize);
            FBigStreamSending.Position := FBigStreamSendState;
            FBigStreamSending.read(buff[0], SendBufferSize);

            AtomInc(FBigStreamSendState, SendBufferSize);
          except
            DelayClose();
            FProgressRunning := False;
            exit;
          end;

          try
            WriteBufferOpen;
            InternalSendByteBuffer(@buff[0], SendBufferSize);
            WriteBufferFlush;
            WriteBufferClose;
            SetLength(buff, 0);
          except
            DelayClose();
            FProgressRunning := False;
            exit;
          end;

          if SendDone then
            begin
              if FBigStreamSendDoneTimeFree then
                  DisposeObject(FBigStreamSending);
              FBigStreamSending := nil;
              FBigStreamSendState := -1;
              FBigStreamSendDoneTimeFree := False;

              ProcessAllSendCmd(nil, False, False);
            end;
        end;
    end;

  if (not FTimeOutProcessDone) and (FOwnerFramework.FIdleTimeOut > 0) and (StopCommunicationTime > FOwnerFramework.FIdleTimeOut) then
    begin
      FTimeOutProcessDone := True;
      Print('IDLE TimeOut > %dms, one second delay do disconnect.', [FOwnerFramework.FIdleTimeOut]);
      DelayClose(1.0);
    end;

  // anti dead loop
  FProgressRunning := False;
end;

procedure TPeerIO.DelayClose;
begin
  DelayClose(0);
end;

procedure TPeerIO.DelayClose(const t: double);
begin
  FOwnerFramework.ProgressPost.PostExecuteM(t, {$IFDEF FPC}@{$ENDIF FPC}FOwnerFramework.DelayClose).Data3 := ID;
end;

procedure TPeerIO.DelayFree;
begin
  DelayFree(0);
end;

procedure TPeerIO.DelayFree(const t: double);
begin
  FOwnerFramework.ProgressPost.PostExecuteM(t, {$IFDEF FPC}@{$ENDIF FPC}FOwnerFramework.DelayFree).Data3 := ID;
end;

procedure TPeerIO.SaveReceiveBuffer(const p: Pointer; siz: Int64);
begin
  OnInternalSaveReceiveBuffer(Self, p, siz);
end;

procedure TPeerIO.FillRecvBuffer(const ACurrentActiveThread: TCoreClassThread; const RecvSync, SendSync: Boolean);
begin
  OnInternalProcessReceiveBuffer(Self, ACurrentActiveThread, RecvSync, SendSync);
end;

procedure TPeerIO.ProcessAllSendCmd(const ACurrentActiveThread: TCoreClassThread; const RecvSync, SendSync: Boolean);
begin
  OnInternalProcessAllSendCmd(Self, ACurrentActiveThread, RecvSync, SendSync);
end;

procedure TPeerIO.PostQueueData(p: PQueueData);
begin
  if FOwnerFramework.Protocol = cpZServer then
    begin
      FOwnerFramework.CmdSendStatistics.IncValue(p^.Cmd, 1);
      FQueueList.Add(p);
    end
  else
      DisposeQueueData(p);
end;

procedure TPeerIO.WriteCustomBuffer(const buffer: PByte; const Size: NativeInt);
begin
  WriteBufferOpen;
  InternalSendByteBuffer(buffer, Size);
  WriteBufferFlush;
  WriteBufferClose;
end;

procedure TPeerIO.PauseResultSend;
begin
  if FCanPauseResultSend then
    begin
      FPauseResultSend := True;
      AtomInc(FOwnerFramework.Statistics[TStatisticsType.stPause]);
    end;
end;

procedure TPeerIO.ContinueResultSend;
var
  headBuff: array [0 .. 2] of Byte;
  b: TBytes;
  buff: TMemoryStream64;
  dHead, dTail: Cardinal;
  Len: Integer;
  Code: TBytes;
  bCipherSecurity: Byte;
begin
  if not FPauseResultSend then
      exit;
  if FResultDataBuffer.Size > 0 then
      exit;

  AtomInc(FOwnerFramework.Statistics[TStatisticsType.stContinue]);

  if FCurrentPauseResultSend_CommDataType in [FConsoleToken, FStreamToken] then
    begin
      buff := TMemoryStream64.Create;

      if FCurrentPauseResultSend_CommDataType = FConsoleToken then
        begin
          b := TPascalString(FOutText).Bytes;
          buff.WritePtr(@b[0], length(b));
        end
      else
          FOutDataFrame.EncodeTo(buff, True);

      dHead := FHeadToken;
      dTail := FTailToken;
      Len := buff.Size;

      // generate hash source
      GenerateHashCode(FOwnerFramework.FHashSecurity, buff.Memory, buff.Size, Code);
      headBuff[0] := Byte(FOwnerFramework.FHashSecurity);
      PWORD(@headBuff[1])^ := length(Code);

      // generate encrypt data body
      bCipherSecurity := Byte(FReceiveDataCipherSecurity);
      Encrypt(FReceiveDataCipherSecurity, buff.Memory, buff.Size, FCipherKey, True);

      // result data header
      FResultDataBuffer.WritePtr(@dHead, C_Cardinal_Size);
      FResultDataBuffer.WritePtr(@Len, C_Integer_Size);

      // verify code
      FResultDataBuffer.WritePtr(@headBuff[0], 3);
      if length(Code) > 0 then
          FResultDataBuffer.WritePtr(@Code[0], length(Code));

      // data body
      FResultDataBuffer.WritePtr(@bCipherSecurity, C_Byte_Size);
      FResultDataBuffer.WritePtr(buff.Memory, Len);

      // data tail
      FResultDataBuffer.WritePtr(@dTail, C_Cardinal_Size);

      DisposeObject(buff);

      AtomInc(FOwnerFramework.Statistics[TStatisticsType.stResponse]);
    end;
  FPauseResultSend := False;
end;

function TPeerIO.ResultSendIsPaused: Boolean;
begin
  Result := FPauseResultSend;
end;

function TPeerIO.GetBigStreamReceiveState(var Total, Complete: Int64): Boolean;
begin
  Result := FBigStreamReceiveProcessing;
  Total := FBigStreamTotal;
  Complete := FBigStreamCompleted;
end;

function TPeerIO.GetBigStreamSendingState(var Total, Complete: Int64): Boolean;
begin
  if FBigStreamSending <> nil then
    begin
      Total := FBigStreamSending.Size;
      Result := True;
    end
  else
    begin
      Total := 0;
      Result := False;
    end;
  Complete := FBigStreamSendState;
end;

function TPeerIO.CipherKeyPtr: PCipherKeyBuffer;
begin
  Result := @FCipherKey;
end;

procedure TPeerIO.GenerateHashCode(const hs: THashSecurity; buff: Pointer; siz: Integer; var output: TBytes);
begin
  TCipher.GenerateHashByte(hs, buff, siz, output);
  AtomInc(FOwnerFramework.Statistics[TStatisticsType.stGenerateHash]);
end;

function TPeerIO.VerifyHashCode(const hs: THashSecurity; buff: Pointer; siz: Integer; var Code: TBytes): Boolean;
var
  buffCode: TBytes;
begin
  try
    GenerateHashCode(hs, buff, siz, buffCode);
    Result := TCipher.CompareHash(buffCode, Code);
  except
      Result := False;
  end;
end;

procedure TPeerIO.Encrypt(cs: TCipherSecurity; DataPtr: Pointer; Size: Cardinal; var k: TCipherKeyBuffer; enc: Boolean);
begin
  if FOwnerFramework.FUsedParallelEncrypt then
      SequEncryptCBC(cs, DataPtr, Size, k, enc, True)
  else
      SequEncryptCBCWithDirect(cs, DataPtr, Size, k, enc, True);

  if cs <> TCipherSecurity.csNone then
      AtomInc(FOwnerFramework.Statistics[TStatisticsType.stEncrypt]);
end;

function TPeerIO.StopCommunicationTime: TTimeTickValue;
begin
  Result := GetTimeTickCount - FLastCommunicationTime;
end;

procedure TPeerIO.UpdateLastCommunicationTime;
begin
  FLastCommunicationTime := GetTimeTickCount;
end;

procedure TPeerIO.SendConsoleCmdM(Cmd: SystemString; ConsoleData: SystemString; OnResult: TConsoleMethod);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendConsoleCmdM(Self, Cmd, ConsoleData, OnResult)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendConsoleCmdM(Cmd, ConsoleData, OnResult);
end;

procedure TPeerIO.SendStreamCmdM(Cmd: SystemString; StreamData: TCoreClassStream; OnResult: TStreamMethod; DoneAutoFree: Boolean);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendStreamCmdM(Self, Cmd, StreamData, OnResult, DoneAutoFree)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendStreamCmdM(Cmd, StreamData, OnResult, DoneAutoFree);
end;

procedure TPeerIO.SendStreamCmdM(Cmd: SystemString; StreamData: TDataFrameEngine; OnResult: TStreamMethod);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendStreamCmdM(Self, Cmd, StreamData, OnResult)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendStreamCmdM(Cmd, StreamData, OnResult);
end;

procedure TPeerIO.SendStreamCmdM(Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamMethod);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendStreamCmdM(Self, Cmd, StreamData, Param1, Param2, OnResult)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendStreamCmdM(Cmd, StreamData, Param1, Param2, OnResult);
end;

{$IFNDEF FPC}


procedure TPeerIO.SendConsoleCmdP(Cmd: SystemString; ConsoleData: SystemString; OnResult: TConsoleProc);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendConsoleCmdP(Self, Cmd, ConsoleData, OnResult)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendConsoleCmdP(Cmd, ConsoleData, OnResult);
end;

procedure TPeerIO.SendStreamCmdP(Cmd: SystemString; StreamData: TCoreClassStream; OnResult: TStreamProc; DoneAutoFree: Boolean);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendStreamCmdP(Self, Cmd, StreamData, OnResult, DoneAutoFree)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendStreamCmdP(Cmd, StreamData, OnResult, DoneAutoFree);
end;

procedure TPeerIO.SendStreamCmdP(Cmd: SystemString; StreamData: TDataFrameEngine; OnResult: TStreamProc);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendStreamCmdP(Self, Cmd, StreamData, OnResult)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendStreamCmdP(Cmd, StreamData, OnResult);
end;

procedure TPeerIO.SendStreamCmdP(Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamProc);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendStreamCmdP(Self, Cmd, StreamData, Param1, Param2, OnResult)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendStreamCmdP(Cmd, StreamData, Param1, Param2, OnResult);
end;
{$ENDIF FPC}


procedure TPeerIO.SendDirectConsoleCmd(Cmd: SystemString; ConsoleData: SystemString);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendDirectConsoleCmd(Self, Cmd, ConsoleData)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendDirectConsoleCmd(Cmd, ConsoleData);
end;

procedure TPeerIO.SendDirectStreamCmd(Cmd: SystemString; StreamData: TCoreClassStream; DoneAutoFree: Boolean);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendDirectStreamCmd(Self, Cmd, StreamData, DoneAutoFree)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendDirectStreamCmd(Cmd, StreamData, DoneAutoFree);
end;

procedure TPeerIO.SendDirectStreamCmd(Cmd: SystemString; StreamData: TDataFrameEngine);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendDirectStreamCmd(Self, Cmd, StreamData)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendDirectStreamCmd(Cmd, StreamData);
end;

procedure TPeerIO.SendDirectStreamCmd(Cmd: SystemString);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendDirectStreamCmd(Self, Cmd)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendDirectStreamCmd(Cmd);
end;

function TPeerIO.WaitSendConsoleCmd(Cmd: SystemString; ConsoleData: SystemString; Timeout: TTimeTickValue): SystemString;
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      Result := TCommunicationFrameworkServer(FOwnerFramework).WaitSendConsoleCmd(Self, Cmd, ConsoleData, Timeout)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      Result := TCommunicationFrameworkClient(FOwnerFramework).WaitSendConsoleCmd(Cmd, ConsoleData, Timeout)
  else
      Result := '';
end;

procedure TPeerIO.WaitSendStreamCmd(Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; Timeout: TTimeTickValue);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).WaitSendStreamCmd(Self, Cmd, StreamData, ResultData, Timeout)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).WaitSendStreamCmd(Cmd, StreamData, ResultData, Timeout);
end;

procedure TPeerIO.SendBigStream(Cmd: SystemString; BigStream: TCoreClassStream; StartPos: Int64; DoneAutoFree: Boolean);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendBigStream(Self, Cmd, BigStream, StartPos, DoneAutoFree)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendBigStream(Cmd, BigStream, StartPos, DoneAutoFree);
end;

procedure TPeerIO.SendBigStream(Cmd: SystemString; BigStream: TCoreClassStream; DoneAutoFree: Boolean);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendBigStream(Self, Cmd, BigStream, DoneAutoFree)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendBigStream(Cmd, BigStream, DoneAutoFree);
end;

procedure TPeerIO.SendCompleteBuffer(Cmd: SystemString; buff: PByte; BuffSize: NativeInt; DoneAutoFree: Boolean);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendCompleteBuffer(Self, Cmd, buff, BuffSize, DoneAutoFree)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendCompleteBuffer(Cmd, buff, BuffSize, DoneAutoFree)
end;

procedure TCommunicationFramework.DoPrint(const v: SystemString);
begin
  if not FQuietMode then
      DoStatus(v, c_DefaultDoStatusID);

  AtomInc(Statistics[TStatisticsType.stPrint]);
end;

function TCommunicationFramework.GetIdleTimeOut: TTimeTickValue;
begin
  Result := FIdleTimeOut;
end;

procedure TCommunicationFramework.SetIdleTimeOut(const Value: TTimeTickValue);
begin
  FIdleTimeOut := Value;
end;

procedure TCommunicationFramework.DoConnected(Sender: TPeerIO);
begin
  if Assigned(FOnConnected) then
      FOnConnected(Sender);
end;

procedure TCommunicationFramework.DoDisconnect(Sender: TPeerIO);
begin
  if Assigned(FOnDisconnect) then
      FOnDisconnect(Sender);
end;

function TCommunicationFramework.CanExecuteCommand(Sender: TPeerIO; Cmd: SystemString): Boolean;
begin
  Result := True;
  if Assigned(FOnExecuteCommand) then
    begin
      try
          FOnExecuteCommand(Sender, Cmd, Result);
      except
      end;
    end;
  if Result then
      AtomInc(Statistics[TStatisticsType.stTotalCommandExecute]);
end;

function TCommunicationFramework.CanSendCommand(Sender: TPeerIO; Cmd: SystemString): Boolean;
begin
  Result := True;
  if Assigned(FOnSendCommand) then
    begin
      try
          FOnSendCommand(Sender, Cmd, Result);
      except
      end;
    end;
  if Result then
      AtomInc(Statistics[TStatisticsType.stTotalCommandSend]);
end;

function TCommunicationFramework.CanRegCommand(Sender: TCommunicationFramework; Cmd: SystemString): Boolean;
begin
  Result := True;
  AtomInc(Statistics[TStatisticsType.stTotalCommandReg]);
end;

procedure TCommunicationFramework.DelayClose(Sender: TNPostExecute);
var
  IO_ID: Cardinal;
  c_IO: TPeerIO;
begin
  IO_ID := Sender.Data3;
  c_IO := TPeerIO(FPeerIO_HashPool[IO_ID]);
  if c_IO <> nil then
      c_IO.Disconnect;
end;

procedure TCommunicationFramework.DelayFree(Sender: TNPostExecute);
var
  IO_ID: Cardinal;
  c_IO: TPeerIO;
begin
  IO_ID := Sender.Data3;
  c_IO := TPeerIO(FPeerIO_HashPool[IO_ID]);
  if c_IO <> nil then
      DisposeObject(c_IO);
end;

procedure TCommunicationFramework.DelayExecuteOnResultState(Sender: TNPostExecute);
var
  p_io: TPeerIO;
  nQueue: PQueueData;
begin
  p_io := TPeerIO(FPeerIO_HashPool[Sender.Data4]);
  nQueue := PQueueData(Sender.Data5);

  if p_io <> nil then
    begin
      DoExecuteResult(p_io, nQueue, Sender.Data3, Sender.DataEng);
    end;

  DisposeQueueData(nQueue);
end;

procedure TCommunicationFramework.DelayExecuteOnCompleteBufferState(Sender: TNPostExecute);
var
  p_io: TPeerIO;
  Cmd: SystemString;
  CompleteBuff: TMemoryStream64;
begin
  p_io := TPeerIO(FPeerIO_HashPool[Sender.Data3]);
  Cmd := Sender.Data4;

  CompleteBuff := TMemoryStream64(Sender.Data1);
  if p_io <> nil then
      ExecuteCompleteBuffer(p_io, Cmd, CompleteBuff.Memory, CompleteBuff.Size);
  DisposeObject(CompleteBuff);
end;

procedure TCommunicationFramework.Internal_ProgressPerClient(PeerClient: TPeerIO);
begin
  PeerClient.Progress;
end;

procedure TCommunicationFramework.FillCustomBuffer(Sender: TPeerIO; const th: TCoreClassThread; const buffer: PByte; const Size: NativeInt; var Customed: Boolean);
begin
end;

procedure TCommunicationFramework.InternalSendByteBuffer(const Sender: TPeerIO; const buff: PByte; siz: NativeInt);
const
  FlushBuffSize = 16 * 1024; // flush size = 16k byte
var
  p: PByte;
begin
  if siz < 1 then
      exit;

  AtomInc(Statistics[TStatisticsType.stSendSize], siz);

  p := buff;

  // fill fragment
  while siz > FlushBuffSize do
    begin
      Sender.SendByteBuffer(p, FlushBuffSize);
      inc(p, FlushBuffSize);
      Sender.WriteBufferFlush;
      dec(siz, FlushBuffSize);
    end;

  if siz > 0 then
      Sender.SendByteBuffer(p, siz);
end;

procedure TCommunicationFramework.InternalSaveReceiveBuffer(const Sender: TPeerIO; const buff: Pointer; siz: Int64);
begin
  if siz > 0 then
      Sender.InternalSaveReceiveBuffer(buff, siz);
end;

procedure TCommunicationFramework.InternalProcessReceiveBuffer(const Sender: TPeerIO; const ACurrentActiveThread: TCoreClassThread; const RecvSync, SendSync: Boolean);
var
  Customed: Boolean;
begin
  Customed := FProtocol = cpCustom;

  if Customed then
    begin
      if RecvSync and (ACurrentActiveThread <> nil) then
          FillCustomBuffer(Sender, ACurrentActiveThread, Sender.FReceivedBuffer.Memory, Sender.FReceivedBuffer.Size, Customed)
      else
          FillCustomBuffer(Sender, nil, Sender.FReceivedBuffer.Memory, Sender.FReceivedBuffer.Size, Customed);
      Sender.FReceivedBuffer.Clear;
      exit;
    end;

  Sender.InternalProcessReceiveBuffer(ACurrentActiveThread, RecvSync, SendSync);
end;

procedure TCommunicationFramework.InternalProcessAllSendCmd(const Sender: TPeerIO; const ACurrentActiveThread: TCoreClassThread; const RecvSync, SendSync: Boolean);
begin
  Sender.InternalProcessAllSendCmd(ACurrentActiveThread, RecvSync, SendSync);
end;

procedure TCommunicationFramework.InternalClientCreate(const Sender: TPeerIO);
begin
end;

procedure TCommunicationFramework.InternalClientDestroy(const Sender: TPeerIO);
begin
end;

procedure TCommunicationFramework.CommandResult_BuildP2PAuthToken(Sender: TPeerIO; ResultData: TDataFrameEngine);
var
  i: Integer;
  arr: TDataFrameArrayInteger;
begin
  arr := ResultData.ReadArrayInteger(0);
  SetLength(Sender.FP2PAuthToken, arr.Count * 4);
  for i := 0 to arr.Count - 1 do
      PInteger(@Sender.FP2PAuthToken[i * 4])^ := arr[i];

  try
    if Assigned(Sender.OnVMBuildAuthModelResultCall) then
        Sender.OnVMBuildAuthModelResultCall();
    if Assigned(Sender.OnVMBuildAuthModelResultMethod) then
        Sender.OnVMBuildAuthModelResultMethod();
{$IFNDEF FPC}
    if Assigned(Sender.OnVMBuildAuthModelResultProc) then
        Sender.OnVMBuildAuthModelResultProc();
{$ENDIF FPC}
  except
  end;

  Sender.OnVMBuildAuthModelResultCall := nil;
  Sender.OnVMBuildAuthModelResultMethod := nil;
{$IFNDEF FPC} Sender.OnVMBuildAuthModelResultProc := nil; {$ENDIF FPC}
end;

procedure TCommunicationFramework.Command_BuildP2PAuthToken(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
var
  i: Integer;
  seed: Integer;
  arr: TDataFrameArrayInteger;
begin
  // build auth buffer
  seed := InData.Reader.ReadInteger;
  arr := OutData.WriteArrayInteger;
  for i := C_VMAuthSize - 1 downto 0 do
      arr.Add(TMISC.Ran03(seed));

  SetLength(Sender.FP2PAuthToken, arr.Count * 4);
  for i := 0 to arr.Count - 1 do
      PInteger(@Sender.FP2PAuthToken[i * 4])^ := arr[i];
end;

procedure TCommunicationFramework.Command_InitP2PTunnel(Sender: TPeerIO; InData: SystemString);
var
  Accept: Boolean;
begin
  if Sender.FP2PVMTunnel <> nil then
      exit;

  Accept := True;
  p2pVMTunnelAuth(Sender, InData, Accept);
  if not Accept then
      exit;
  Sender.OpenP2PVMTunnel(16, False, '');
  Sender.p2pVMTunnel.AuthVM;
  p2pVMTunnelOpenBefore(Sender, Sender.p2pVMTunnel);
end;

procedure TCommunicationFramework.Command_CloseP2PTunnel(Sender: TPeerIO; InData: SystemString);
begin
  Sender.InternalCloseP2PVMTunnel;
end;

procedure TCommunicationFramework.VMAuthSuccessAfterDelayExecute(Sender: TNPostExecute);
var
  PC: TPeerIO;
begin
  PC := TPeerIO(FPeerIO_HashPool[Sender.Data3]);
  if PC = nil then
      exit;

  try
    if Assigned(PC.OnVMAuthResultCall) then
        PC.OnVMAuthResultCall(True);
    if Assigned(PC.OnVMAuthResultMethod) then
        PC.OnVMAuthResultMethod(True);
{$IFNDEF FPC}
    if Assigned(PC.OnVMAuthResultProc) then
        PC.OnVMAuthResultProc(True);
{$ENDIF FPC}
  except
  end;

  PC.OnVMAuthResultCall := nil;
  PC.OnVMAuthResultMethod := nil;
{$IFNDEF FPC} PC.OnVMAuthResultProc := nil; {$ENDIF FPC}
  p2pVMTunnelOpenAfter(PC, PC.p2pVMTunnel);
end;

procedure TCommunicationFramework.VMAuthSuccessDelayExecute(Sender: TNPostExecute);
var
  PC: TPeerIO;
begin
  PC := TPeerIO(FPeerIO_HashPool[Sender.Data3]);
  if PC = nil then
      exit;

  ProgressPost.PostExecuteM(1.0, {$IFDEF FPC}@{$ENDIF FPC}VMAuthSuccessAfterDelayExecute).Data3 := PC.FID;
  p2pVMTunnelOpen(PC, PC.p2pVMTunnel);
end;

procedure TCommunicationFramework.VMAuthFailedDelayExecute(Sender: TNPostExecute);
var
  PC: TPeerIO;
begin
  PC := TPeerIO(FPeerIO_HashPool[Sender.Data3]);
  if PC = nil then
      exit;

  try
    if Assigned(PC.OnVMAuthResultCall) then
        PC.OnVMAuthResultCall(False);
    if Assigned(PC.OnVMAuthResultMethod) then
        PC.OnVMAuthResultMethod(False);
{$IFNDEF FPC}
    if Assigned(PC.OnVMAuthResultProc) then
        PC.OnVMAuthResultProc(False);
{$ENDIF FPC}
  except
  end;

  PC.OnVMAuthResultCall := nil;
  PC.OnVMAuthResultMethod := nil;
{$IFNDEF FPC} PC.OnVMAuthResultProc := nil; {$ENDIF FPC}
end;

constructor TCommunicationFramework.Create(HashPoolLen: Integer);
var
  st: TStatisticsType;
begin
  inherited Create;
  FCommandList := THashObjectList.Create(True, 128);
  FIDCounter := 1;
  FPeerIO_HashPool := TUInt32HashObjectList.Create(HashPoolLen);
  FPeerIO_HashPool.AutoFreeData := False;
  FPeerIO_HashPool.AccessOptimization := False;
  FOnConnected := nil;
  FOnDisconnect := nil;
  FOnExecuteCommand := nil;
  FOnSendCommand := nil;
  FIdleTimeOut := 0;
  FUsedParallelEncrypt := True;
  FSyncOnResult := False;
  FSyncOnCompleteBuffer := True;
  FEnabledAtomicLockAndMultiThread := True;
  FQuietMode := False;
  FCipherSecurity := TCipherSecurity.csNone;
  FSendDataCompressed := True;
  FCompleteBufferCompressed := False;
  FHashSecurity := THashSecurity.hsNone;
  FMaxCompleteBufferSize := 4 * 1024 * 1024; // 4M
  FPeerIOUserDefineClass := TPeerIOUserDefine;
  FPeerIOUserSpecialClass := TPeerIOUserSpecial;

  FPrintParams := THashVariantList.Create(128);
  FPrintParams.AutoUpdateDefaultValue := True;

  FPostProgress := TNProgressPostWithCadencer.Create;

  FFrameworkIsServer := True;
  FFrameworkIsClient := True;
  FFrameworkInfo := ClassName;

  FOnProgressRuning := False;
  FOnProgress := nil;

  FCMDWithThreadRuning := 0;

  FVMInterface := nil;

  FProtocol := cpZServer;

  for st := low(TStatisticsType) to high(TStatisticsType) do
      Statistics[st] := 0;
  CmdRecvStatistics := THashVariantList.Create(128);
  CmdSendStatistics := THashVariantList.Create(128);
  CmdMaxExecuteConsumeStatistics := THashVariantList.Create(128);

  RegisterStream(C_BuildP2PAuthToken).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_BuildP2PAuthToken;
  RegisterDirectConsole(C_InitP2PTunnel).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_InitP2PTunnel;
  RegisterDirectConsole(C_CloseP2PTunnel).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_CloseP2PTunnel;

  SwitchDefaultPerformance;

  CreateAfter;
end;

procedure TCommunicationFramework.CreateAfter;
begin
end;

destructor TCommunicationFramework.Destroy;
begin
  DeleteRegistedCMD(C_BuildP2PAuthToken);
  DeleteRegistedCMD(C_InitP2PTunnel);
  DeleteRegistedCMD(C_CloseP2PTunnel);
  DisposeObject(FCommandList);
  DisposeObject(FPeerIO_HashPool);
  DisposeObject(FPrintParams);
  DisposeObject(FPostProgress);
  DisposeObject([CmdRecvStatistics, CmdSendStatistics, CmdMaxExecuteConsumeStatistics]);
  inherited Destroy;
end;

procedure TCommunicationFramework.WriteCustomBuffer(p_io: TPeerIO; const buffer: PByte; const Size: NativeInt);
begin
  p_io.WriteCustomBuffer(buffer, Size);
end;

procedure TCommunicationFramework.p2pVMTunnelAuth(Sender: TPeerIO; const Token: SystemString; var Accept: Boolean);
begin
  if FVMInterface <> nil then
      FVMInterface.p2pVMTunnelAuth(Sender, Token, Accept);
end;

procedure TCommunicationFramework.p2pVMTunnelOpenBefore(Sender: TPeerIO; p2pVMTunnel: TCommunicationFrameworkWithP2PVM);
begin
  if FVMInterface <> nil then
      FVMInterface.p2pVMTunnelOpenBefore(Sender, p2pVMTunnel);
end;

procedure TCommunicationFramework.p2pVMTunnelOpen(Sender: TPeerIO; p2pVMTunnel: TCommunicationFrameworkWithP2PVM);
begin
  if FVMInterface <> nil then
      FVMInterface.p2pVMTunnelOpen(Sender, p2pVMTunnel);
end;

procedure TCommunicationFramework.p2pVMTunnelOpenAfter(Sender: TPeerIO; p2pVMTunnel: TCommunicationFrameworkWithP2PVM);
begin
  if FVMInterface <> nil then
      FVMInterface.p2pVMTunnelOpen(Sender, p2pVMTunnel);
end;

procedure TCommunicationFramework.p2pVMTunnelClose(Sender: TPeerIO; p2pVMTunnel: TCommunicationFrameworkWithP2PVM);
begin
  if FVMInterface <> nil then
      FVMInterface.p2pVMTunnelOpen(Sender, p2pVMTunnel);
end;

procedure TCommunicationFramework.SwitchMaxPerformance;
begin
  FUsedParallelEncrypt := False;
  FHashSecurity := THashSecurity.hsNone;
  FSendDataCompressed := False;
  FCipherSecurity := TCipherSecurity.csNone;
end;

procedure TCommunicationFramework.SwitchMaxSecurity;
type
  TCipherDef = array [0 .. 4] of TCipherSecurity;
const
  cc: TCipherDef = (csRC6, csSerpent, csMars, csRijndael, csTwoFish);
begin
  FUsedParallelEncrypt := True;
  FHashSecurity := THashSecurity.hsSHA512;
  FSendDataCompressed := True;
  FCipherSecurity := cc[umlRandomRange(0, 4)];
end;

procedure TCommunicationFramework.SwitchDefaultPerformance;
type
  TCipherDef = array [0 .. 9] of TCipherSecurity;
const
  cc: TCipherDef = (csDES64, csDES128, csDES192, csBlowfish, csLBC, csLQC, csRNG32, csRNG64, csLSC, csXXTea512);
begin
  FUsedParallelEncrypt := True;
  FHashSecurity := THashSecurity.hsFastMD5;
  FSendDataCompressed := True;
  FCipherSecurity := cc[umlRandomRange(0, 9)];
end;

procedure TCommunicationFramework.LockClients;
begin
  if FEnabledAtomicLockAndMultiThread then
      LockObject(FPeerIO_HashPool); // atomic lock
  AtomInc(Statistics[TStatisticsType.stLock]);
end;

procedure TCommunicationFramework.UnLockClients;
begin
  if FEnabledAtomicLockAndMultiThread then
      UnLockObject(FPeerIO_HashPool); // atomic lock
  AtomInc(Statistics[TStatisticsType.stUnLock]);
end;

procedure TCommunicationFramework.Progress;
begin
  if FOnProgressRuning then
      exit;

  // anti Dead loop
  FOnProgressRuning := True;

  try
    if Assigned(ProgressBackgroundProc) then
        ProgressBackgroundProc;
  except
  end;

  try
    if Assigned(ProgressBackgroundMethod) then
        ProgressBackgroundMethod;
  except
  end;

  ProgressPerClientM({$IFDEF FPC}@{$ENDIF FPC}Internal_ProgressPerClient);

  try
      ProgressPost.Progress;
  except
  end;

  try
    if Assigned(FOnProgress) then
        FOnProgress(Self);
  except
  end;

  // anti Dead loop
  FOnProgressRuning := False;
end;

procedure TCommunicationFramework.ProgressPerClientC(OnBackcall: TPerClientListCall);
var
  IO_Array: TIO_Array;
  pframeworkID: Cardinal;
  c: TPeerIO;
begin
  if (FPeerIO_HashPool.Count > 0) and (Assigned(OnBackcall)) then
    begin
      GetIO_Array(IO_Array);
      for pframeworkID in IO_Array do
        begin
          c := TPeerIO(FPeerIO_HashPool[pframeworkID]);
          if c <> nil then
            begin
              try
                  OnBackcall(c);
              except
              end;
            end;
        end;
    end;
end;

procedure TCommunicationFramework.ProgressPerClientM(OnBackcall: TPerClientListMethod);
var
  IO_Array: TIO_Array;
  pframeworkID: Cardinal;
  c: TPeerIO;
begin
  if (FPeerIO_HashPool.Count > 0) and (Assigned(OnBackcall)) then
    begin
      GetIO_Array(IO_Array);
      for pframeworkID in IO_Array do
        begin
          c := TPeerIO(FPeerIO_HashPool[pframeworkID]);
          if c <> nil then
            begin
              try
                  OnBackcall(c);
              except
              end;
            end;
        end;
    end;
end;

{$IFNDEF FPC}


procedure TCommunicationFramework.ProgressPerClientP(OnBackcall: TPerClientListProc);
var
  IO_Array: TIO_Array;
  pframeworkID: Cardinal;
  c: TPeerIO;
begin
  if (FPeerIO_HashPool.Count > 0) and (Assigned(OnBackcall)) then
    begin
      GetIO_Array(IO_Array);
      for pframeworkID in IO_Array do
        begin
          c := TPeerIO(FPeerIO_HashPool[pframeworkID]);
          if c <> nil then
            begin
              try
                  OnBackcall(c);
              except
              end;
            end;
        end;
    end;
end;
{$ENDIF FPC}


procedure TCommunicationFramework.FastProgressPerClientC(OnBackcall: TPerClientListCall);
var
  i: Integer;
  p: PUInt32HashListObjectStruct;
begin
  if (FPeerIO_HashPool.Count > 0) and (Assigned(OnBackcall)) then
    begin
      i := 0;
      p := FPeerIO_HashPool.FirstPtr;
      while i < FPeerIO_HashPool.Count do
        begin
          try
              OnBackcall(TPeerIO(p^.Data));
          except
          end;
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure TCommunicationFramework.FastProgressPerClientM(OnBackcall: TPerClientListMethod);
var
  i: Integer;
  p: PUInt32HashListObjectStruct;
begin
  if (FPeerIO_HashPool.Count > 0) and (Assigned(OnBackcall)) then
    begin
      i := 0;
      p := FPeerIO_HashPool.FirstPtr;
      while i < FPeerIO_HashPool.Count do
        begin
          try
              OnBackcall(TPeerIO(p^.Data));
          except
          end;
          inc(i);
          p := p^.Next;
        end;
    end;
end;

{$IFNDEF FPC}


procedure TCommunicationFramework.FastProgressPerClientP(OnBackcall: TPerClientListProc);
var
  i: Integer;
  p: PUInt32HashListObjectStruct;
begin
  if (FPeerIO_HashPool.Count > 0) and (Assigned(OnBackcall)) then
    begin
      i := 0;
      p := FPeerIO_HashPool.FirstPtr;
      while i < FPeerIO_HashPool.Count do
        begin
          try
              OnBackcall(TPeerIO(p^.Data));
          except
          end;
          inc(i);
          p := p^.Next;
        end;
    end;
end;

{$ENDIF FPC}


procedure TCommunicationFramework.GetIO_Array(out IO_Array: TIO_Array);
var
  i: Integer;
  p: PUInt32HashListObjectStruct;
begin
  LockClients;
  try
    SetLength(IO_Array, FPeerIO_HashPool.Count);
    if (FPeerIO_HashPool.Count > 0) then
      begin
        i := 0;
        p := FPeerIO_HashPool.FirstPtr;
        while i < FPeerIO_HashPool.Count do
          begin
            IO_Array[i] := TPeerIO(p^.Data).FID;
            inc(i);
            p := p^.Next;
          end;
      end;
  finally
      UnLockClients;
  end;
end;

procedure TCommunicationFramework.ProgressWaitSend(p_io: TPeerIO);
begin
  Progress;
end;

procedure TCommunicationFramework.PrintParam(v: SystemString; Args: SystemString);
begin
  try
    if (FPrintParams.GetDefaultValue(Args, True) = True) then
        DoPrint(Format(v, [Args]));
  except
      DoPrint(Format(v, [Args]));
  end;
end;

function TCommunicationFramework.DeleteRegistedCMD(Cmd: SystemString): Boolean;
begin
  Result := FCommandList.Exists(Cmd);
  FCommandList.Delete(Cmd);
end;

function TCommunicationFramework.UnRegisted(Cmd: SystemString): Boolean;
begin
  Result := FCommandList.Exists(Cmd);
  FCommandList.Delete(Cmd);
end;

function TCommunicationFramework.RegisterConsole(Cmd: SystemString): TCommandConsole;
begin
  if not CanRegCommand(Self, Cmd) then
    begin
      RaiseInfo(Format('Illegal Register', []));
      Result := nil;
      exit;
    end;

  if FCommandList.Exists(Cmd) then
    begin
      RaiseInfo(Format('exists cmd:%s', [Cmd]));
      Result := nil;
      exit;
    end;

  Result := TCommandConsole.Create;
  FCommandList[Cmd] := Result;

  CmdRecvStatistics.IncValue(Cmd, 0);
  CmdMaxExecuteConsumeStatistics[Cmd] := 0;
end;

function TCommunicationFramework.RegisterStream(Cmd: SystemString): TCommandStream;
begin
  if not CanRegCommand(Self, Cmd) then
    begin
      RaiseInfo(Format('Illegal Register', []));
      Result := nil;
      exit;
    end;

  if FCommandList.Exists(Cmd) then
    begin
      RaiseInfo(Format('exists cmd:%s', [Cmd]));
      Result := nil;
      exit;
    end;

  Result := TCommandStream.Create;
  FCommandList[Cmd] := Result;

  CmdRecvStatistics.IncValue(Cmd, 0);
  CmdMaxExecuteConsumeStatistics[Cmd] := 0;
end;

function TCommunicationFramework.RegisterDirectStream(Cmd: SystemString): TCommandDirectStream;
begin
  if not CanRegCommand(Self, Cmd) then
    begin
      RaiseInfo(Format('Illegal Register', []));
      Result := nil;
      exit;
    end;

  if FCommandList.Exists(Cmd) then
    begin
      RaiseInfo(Format('exists cmd:%s', [Cmd]));
      Result := nil;
      exit;
    end;

  Result := TCommandDirectStream.Create;
  FCommandList[Cmd] := Result;

  CmdRecvStatistics.IncValue(Cmd, 0);
  CmdMaxExecuteConsumeStatistics[Cmd] := 0;
end;

function TCommunicationFramework.RegisterDirectConsole(Cmd: SystemString): TCommandDirectConsole;
begin
  if not CanRegCommand(Self, Cmd) then
    begin
      RaiseInfo(Format('Illegal Register', []));
      Result := nil;
      exit;
    end;

  if FCommandList.Exists(Cmd) then
    begin
      RaiseInfo(Format('exists cmd:%s', [Cmd]));
      Result := nil;
      exit;
    end;

  Result := TCommandDirectConsole.Create;
  FCommandList[Cmd] := Result;

  CmdRecvStatistics.IncValue(Cmd, 0);
  CmdMaxExecuteConsumeStatistics[Cmd] := 0;
end;

function TCommunicationFramework.RegisterBigStream(Cmd: SystemString): TCommandBigStream;
begin
  if not CanRegCommand(Self, Cmd) then
    begin
      RaiseInfo(Format('Illegal Register', []));
      Result := nil;
      exit;
    end;

  if FCommandList.Exists(Cmd) then
    begin
      RaiseInfo(Format('exists cmd:%s', [Cmd]));
      Result := nil;
      exit;
    end;

  Result := TCommandBigStream.Create;
  FCommandList[Cmd] := Result;

  CmdRecvStatistics.IncValue(Cmd, 0);
  CmdMaxExecuteConsumeStatistics[Cmd] := 0;
end;

function TCommunicationFramework.RegisterCompleteBuffer(Cmd: SystemString): TCommandCompleteBuffer;
begin
  if not CanRegCommand(Self, Cmd) then
    begin
      RaiseInfo(Format('Illegal Register', []));
      Result := nil;
      exit;
    end;

  if FCommandList.Exists(Cmd) then
    begin
      RaiseInfo(Format('exists cmd:%s', [Cmd]));
      Result := nil;
      exit;
    end;

  Result := TCommandCompleteBuffer.Create;
  FCommandList[Cmd] := Result;

  CmdRecvStatistics.IncValue(Cmd, 0);
  CmdMaxExecuteConsumeStatistics[Cmd] := 0;
end;

function TCommunicationFramework.ExistsRegistedCmd(Cmd: SystemString): Boolean;
begin
  Result := FCommandList.Exists(Cmd);
end;

procedure TCommunicationFramework.PrintRegistedCMD;
var
  l: TListPascalString;
begin
  l := TListPascalString.Create;
  FCommandList.GetNameList(l);
  DoStatus(l);
  DisposeObject(l);
end;

function TCommunicationFramework.ExecuteConsole(Sender: TPeerIO; Cmd: SystemString; const InData: SystemString; var OutData: SystemString): Boolean;
var
  b: TCoreClassObject;
begin
  Result := False;
  if not CanExecuteCommand(Sender, Cmd) then
      exit;
  b := FCommandList[Cmd];
  if b = nil then
    begin
      Sender.PrintCommand('no exists console cmd:%s', Cmd);
      exit;
    end;
  if not b.InheritsFrom(TCommandConsole) then
    begin
      Sender.PrintCommand('Illegal interface in cmd:%s', Cmd);
      exit;
    end;
  Result := TCommandConsole(b).Execute(Sender, InData, OutData);
end;

function TCommunicationFramework.ExecuteStream(Sender: TPeerIO; Cmd: SystemString; InData, OutData: TDataFrameEngine): Boolean;
var
  b: TCoreClassObject;
begin
  Result := False;
  if not CanExecuteCommand(Sender, Cmd) then
      exit;
  b := FCommandList[Cmd];
  if b = nil then
    begin
      Sender.PrintCommand('no exists stream cmd:%s', Cmd);
      exit;
    end;
  if not b.InheritsFrom(TCommandStream) then
    begin
      Sender.PrintCommand('Illegal interface in cmd:%s', Cmd);
      exit;
    end;
  InData.Reader.index := 0;
  Result := TCommandStream(b).Execute(Sender, InData, OutData);
end;

function TCommunicationFramework.ExecuteDirectStream(Sender: TPeerIO; Cmd: SystemString; InData: TDataFrameEngine): Boolean;
var
  b: TCoreClassObject;
begin
  Result := False;
  if not CanExecuteCommand(Sender, Cmd) then
      exit;
  b := FCommandList[Cmd];
  if b = nil then
    begin
      Sender.PrintCommand('no exists direct console cmd:%s', Cmd);
      exit;
    end;
  if not b.InheritsFrom(TCommandDirectStream) then
    begin
      Sender.PrintCommand('Illegal interface in cmd:%s', Cmd);
      exit;
    end;
  InData.Reader.index := 0;
  Result := TCommandDirectStream(b).Execute(Sender, InData);
end;

function TCommunicationFramework.ExecuteDirectConsole(Sender: TPeerIO; Cmd: SystemString; const InData: SystemString): Boolean;
var
  b: TCoreClassObject;
begin
  Result := False;
  if not CanExecuteCommand(Sender, Cmd) then
      exit;
  b := FCommandList[Cmd];
  if b = nil then
    begin
      Sender.PrintCommand('no exists direct stream cmd:%s', Cmd);
      exit;
    end;
  if not b.InheritsFrom(TCommandDirectConsole) then
    begin
      Sender.PrintCommand('Illegal interface in cmd:%s', Cmd);
      exit;
    end;
  Result := TCommandDirectConsole(b).Execute(Sender, InData);
end;

function TCommunicationFramework.ExecuteBigStream(Sender: TPeerIO; Cmd: SystemString; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64): Boolean;
var
  b: TCoreClassObject;
begin
  Result := False;
  if not CanExecuteCommand(Sender, Cmd) then
      exit;
  b := FCommandList[Cmd];
  if b = nil then
    begin
      Sender.PrintCommand('no exists Big Stream cmd:%s', Cmd);
      exit;
    end;
  if not b.InheritsFrom(TCommandBigStream) then
    begin
      Sender.PrintCommand('Illegal interface in cmd:%s', Cmd);
      exit;
    end;
  Result := TCommandBigStream(b).Execute(Sender, InData, BigStreamTotal, BigStreamCompleteSize);
end;

function TCommunicationFramework.ExecuteCompleteBuffer(Sender: TPeerIO; Cmd: SystemString; InData: PByte; DataSize: NativeInt): Boolean;
var
  b: TCoreClassObject;
begin
  Result := False;
  if not CanExecuteCommand(Sender, Cmd) then
      exit;
  b := FCommandList[Cmd];
  if b = nil then
    begin
      Sender.PrintCommand('no exists complete buffer cmd:%s', Cmd);
      exit;
    end;
  if not b.InheritsFrom(TCommandCompleteBuffer) then
    begin
      Sender.PrintCommand('Illegal interface in cmd:%s', Cmd);
      exit;
    end;
  Result := TCommandCompleteBuffer(b).Execute(Sender, InData, DataSize);
end;

function TCommunicationFramework.FirstClient: TPeerIO;
begin
  Result := TPeerIO(FPeerIO_HashPool.First);
end;

function TCommunicationFramework.LastClient: TPeerIO;
begin
  Result := TPeerIO(FPeerIO_HashPool.Last);
end;

procedure TCommunicationFrameworkServer.DoPrint(const v: SystemString);
begin
  inherited DoPrint('server ' + v);
end;

function TCommunicationFrameworkServer.CanExecuteCommand(Sender: TPeerIO; Cmd: SystemString): Boolean;
begin
  if IsSystemCMD(Cmd) then
      Result := True
  else
      Result := inherited CanExecuteCommand(Sender, Cmd);
end;

function TCommunicationFrameworkServer.CanSendCommand(Sender: TPeerIO; Cmd: SystemString): Boolean;
begin
  Result := inherited CanSendCommand(Sender, Cmd);
end;

function TCommunicationFrameworkServer.CanRegCommand(Sender: TCommunicationFramework; Cmd: SystemString): Boolean;
begin
  if IsSystemCMD(Cmd) then
      Result := True
  else
      Result := inherited CanRegCommand(Sender, Cmd);
end;

procedure TCommunicationFrameworkServer.Command_CipherModel(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
begin
  try
      Sender.UserDefine.FWorkPlatform := TExecutePlatform(InData.Reader.ReadInteger);
  except
  end;

  OutData.WriteCardinal(Sender.ID);
  OutData.WriteByte(Byte(FCipherSecurity));
  OutData.WriteArrayByte.SetBuff(@Sender.FCipherKey[0], length(Sender.FCipherKey));

  Sender.FRemoteExecutedForConnectInit := True;

  DoClientConnectAfter(Sender);
end;

procedure TCommunicationFrameworkServer.Command_Wait(Sender: TPeerIO; InData: SystemString; var OutData: SystemString);
begin
  OutData := IntToHex(GetTimeTick, SizeOf(TTimeTickValue) * 2);
end;

procedure TCommunicationFrameworkServer.InternalClientCreate(const Sender: TPeerIO);
begin
  DoClientConnectBefore(Sender);
  inherited InternalClientCreate(Sender);
  if FProtocol = cpCustom then
      DoClientConnectAfter(Sender);
end;

procedure TCommunicationFrameworkServer.InternalClientDestroy(const Sender: TPeerIO);
begin
  DoClientDisconnect(Sender);
  inherited InternalClientDestroy(Sender);
end;

procedure TCommunicationFrameworkServer.SyncFillCustomBuffer;
begin
  OnReceiveBuffer(FillSync_Sender, FillSync_Buffer, FillSync_BufferSize);
end;

procedure TCommunicationFrameworkServer.FillCustomBuffer(Sender: TPeerIO; const th: TCoreClassThread; const buffer: PByte; const Size: NativeInt; var Customed: Boolean);
begin
  if Protocol = cpCustom then
    begin
      FillSync_Sender := Sender;
      FillSync_Buffer := buffer;
      FillSync_BufferSize := Size;
      SyncMethod(th, th <> nil, {$IFDEF FPC}@{$ENDIF FPC}SyncFillCustomBuffer);
    end;
end;

constructor TCommunicationFrameworkServer.Create;
begin
  CreateCustomHashPool(10 * 10000);
end;

constructor TCommunicationFrameworkServer.CreateCustomHashPool(HashPoolLen: Integer);
begin
  inherited Create(HashPoolLen);
  FSyncOnResult := True;
  FSyncOnCompleteBuffer := True;

  RegisterStream(C_CipherModel).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_CipherModel;
  RegisterConsole(C_Wait).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_Wait;

  FFrameworkIsServer := True;
  FFrameworkIsClient := False;
end;

destructor TCommunicationFrameworkServer.Destroy;
begin
  while FCMDWithThreadRuning > 0 do
      CheckThreadSynchronize(1);

  DeleteRegistedCMD(C_CipherModel);
  DeleteRegistedCMD(C_Wait);
  inherited Destroy;
end;

procedure TCommunicationFrameworkServer.Disconnect(ID: Cardinal);
begin
  Disconnect(ID, 0);
end;

procedure TCommunicationFrameworkServer.Disconnect(ID: Cardinal; delay: double);
var
  io_cli: TPeerIO;
begin
  io_cli := PeerIO[ID];
  if io_cli <> nil then
      io_cli.DelayClose(delay);
end;

procedure TCommunicationFrameworkServer.OnReceiveBuffer(Sender: TPeerIO; const buffer: PByte; const Size: NativeInt);
begin
end;

procedure TCommunicationFrameworkServer.WriteBuffer(p_io: TPeerIO; const buffer: PByte; const Size: NativeInt);
begin
  WriteCustomBuffer(p_io, buffer, Size);
end;

procedure TCommunicationFrameworkServer.StopService;
begin
end;

function TCommunicationFrameworkServer.StartService(Host: SystemString; Port: Word): Boolean;
begin
  Result := False;
end;

procedure TCommunicationFrameworkServer.TriggerQueueData(v: PQueueData);
begin
end;

procedure TCommunicationFrameworkServer.DoClientConnectBefore(Sender: TPeerIO);
begin
end;

procedure TCommunicationFrameworkServer.DoClientConnectAfter(Sender: TPeerIO);
begin
end;

procedure TCommunicationFrameworkServer.DoClientDisconnect(Sender: TPeerIO);
begin
end;

procedure TCommunicationFrameworkServer.SendConsoleCmdM(p_io: TPeerIO; const Cmd, ConsoleData: SystemString; OnResult: TConsoleMethod);
var
  p: PQueueData;
begin
  // init queue data
  if (p_io = nil) or (not p_io.Connected) then
      exit;
  if not CanSendCommand(p_io, Cmd) then
      exit;

  p_io.PrintCommand('Send Console cmd: %s', Cmd);

  p := NewQueueData;
  p^.State := TQueueState.qsSendConsoleCMD;
  p^.IO_ID := p_io.ID;
  p^.Cmd := Cmd;
  p^.Cipher := p_io.FSendDataCipherSecurity;
  p^.ConsoleData := ConsoleData;
  p^.OnConsoleMethod := OnResult;
  TriggerQueueData(p);

end;

procedure TCommunicationFrameworkServer.SendStreamCmdM(p_io: TPeerIO; const Cmd: SystemString; StreamData: TCoreClassStream; OnResult: TStreamMethod; DoneAutoFree: Boolean);
var
  p: PQueueData;
begin
  // init queue data
  if (p_io = nil) or (not p_io.Connected) then
      exit;
  if not CanSendCommand(p_io, Cmd) then
      exit;

  p_io.PrintCommand('Send Stream cmd: %s', Cmd);

  p := NewQueueData;
  p^.State := TQueueState.qsSendStreamCMD;
  p^.IO_ID := p_io.ID;
  p^.Cmd := Cmd;
  p^.Cipher := p_io.FSendDataCipherSecurity;
  p^.DoneAutoFree := DoneAutoFree;
  p^.StreamData := StreamData;
  p^.OnStreamMethod := OnResult;
  TriggerQueueData(p);
end;

procedure TCommunicationFrameworkServer.SendStreamCmdM(p_io: TPeerIO; const Cmd: SystemString; StreamData: TDataFrameEngine; OnResult: TStreamMethod);
var
  p: PQueueData;
begin
  // init queue data
  if (p_io = nil) or (not p_io.Connected) then
      exit;
  if not CanSendCommand(p_io, Cmd) then
      exit;

  p_io.PrintCommand('Send Stream cmd: %s', Cmd);

  p := NewQueueData;
  p^.State := TQueueState.qsSendStreamCMD;
  p^.IO_ID := p_io.ID;
  p^.Cmd := Cmd;
  p^.Cipher := p_io.FSendDataCipherSecurity;
  p^.DoneAutoFree := True;
  p^.StreamData := TMemoryStream64.Create;
  if StreamData <> nil then
      StreamData.EncodeTo(p^.StreamData, True)
  else
      TDataFrameEngine.BuildEmptyStream(p^.StreamData);
  p^.OnStreamMethod := OnResult;
  TriggerQueueData(p);
end;

procedure TCommunicationFrameworkServer.SendStreamCmdM(p_io: TPeerIO; const Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamMethod);
var
  p: PQueueData;
begin
  // init queue data
  if (p_io = nil) or (not p_io.Connected) then
      exit;
  if not CanSendCommand(p_io, Cmd) then
      exit;

  p_io.PrintCommand('Send Stream cmd: %s', Cmd);

  p := NewQueueData;
  p^.State := TQueueState.qsSendStreamCMD;
  p^.IO_ID := p_io.ID;
  p^.Cmd := Cmd;
  p^.Cipher := p_io.FSendDataCipherSecurity;
  p^.DoneAutoFree := True;
  p^.StreamData := TMemoryStream64.Create;
  if StreamData <> nil then
      StreamData.EncodeTo(p^.StreamData, True)
  else
      TDataFrameEngine.BuildEmptyStream(p^.StreamData);
  p^.OnStreamParamMethod := OnResult;
  p^.Param1 := p^.Param1;
  p^.Param2 := p^.Param2;
  TriggerQueueData(p);
end;

{$IFNDEF FPC}


procedure TCommunicationFrameworkServer.SendConsoleCmdP(p_io: TPeerIO; const Cmd, ConsoleData: SystemString; OnResult: TConsoleProc);
var
  p: PQueueData;
begin
  // init queue data
  if (p_io = nil) or (not p_io.Connected) then
      exit;
  if not CanSendCommand(p_io, Cmd) then
      exit;

  p_io.PrintCommand('Send Console cmd: %s', Cmd);

  p := NewQueueData;
  p^.State := TQueueState.qsSendConsoleCMD;
  p^.IO_ID := p_io.ID;
  p^.Cmd := Cmd;
  p^.Cipher := p_io.FSendDataCipherSecurity;
  p^.ConsoleData := ConsoleData;
  p^.OnConsoleProc := OnResult;
  TriggerQueueData(p);
end;

procedure TCommunicationFrameworkServer.SendStreamCmdP(p_io: TPeerIO; const Cmd: SystemString; StreamData: TCoreClassStream; OnResult: TStreamProc; DoneAutoFree: Boolean);
var
  p: PQueueData;
begin
  // init queue data
  if (p_io = nil) or (not p_io.Connected) then
      exit;
  if not CanSendCommand(p_io, Cmd) then
      exit;

  p_io.PrintCommand('Send Stream cmd: %s', Cmd);

  p := NewQueueData;
  p^.State := TQueueState.qsSendStreamCMD;
  p^.IO_ID := p_io.ID;
  p^.Cmd := Cmd;
  p^.Cipher := p_io.FSendDataCipherSecurity;
  p^.DoneAutoFree := DoneAutoFree;
  p^.StreamData := StreamData;
  p^.OnStreamProc := OnResult;
  TriggerQueueData(p);
end;

procedure TCommunicationFrameworkServer.SendStreamCmdP(p_io: TPeerIO; const Cmd: SystemString; StreamData: TDataFrameEngine; OnResult: TStreamProc);
var
  p: PQueueData;
begin
  // init queue data
  if (p_io = nil) or (not p_io.Connected) then
      exit;
  if not CanSendCommand(p_io, Cmd) then
      exit;

  p_io.PrintCommand('Send Stream cmd: %s', Cmd);

  p := NewQueueData;
  p^.State := TQueueState.qsSendStreamCMD;
  p^.IO_ID := p_io.ID;
  p^.Cmd := Cmd;
  p^.Cipher := p_io.FSendDataCipherSecurity;
  p^.DoneAutoFree := True;
  p^.StreamData := TMemoryStream64.Create;
  if StreamData <> nil then
      StreamData.EncodeTo(p^.StreamData, True)
  else
      TDataFrameEngine.BuildEmptyStream(p^.StreamData);
  p^.OnStreamProc := OnResult;
  TriggerQueueData(p);
end;

procedure TCommunicationFrameworkServer.SendStreamCmdP(p_io: TPeerIO; const Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamProc);
var
  p: PQueueData;
begin
  // init queue data
  if (p_io = nil) or (not p_io.Connected) then
      exit;
  if not CanSendCommand(p_io, Cmd) then
      exit;

  p_io.PrintCommand('Send Stream cmd: %s', Cmd);

  p := NewQueueData;
  p^.State := TQueueState.qsSendStreamCMD;
  p^.IO_ID := p_io.ID;
  p^.Cmd := Cmd;
  p^.Cipher := p_io.FSendDataCipherSecurity;
  p^.DoneAutoFree := True;
  p^.StreamData := TMemoryStream64.Create;
  if StreamData <> nil then
      StreamData.EncodeTo(p^.StreamData, True)
  else
      TDataFrameEngine.BuildEmptyStream(p^.StreamData);
  p^.OnStreamParamProc := OnResult;
  p^.Param1 := p^.Param1;
  p^.Param2 := p^.Param2;
  TriggerQueueData(p);
end;
{$ENDIF FPC}


procedure TCommunicationFrameworkServer.SendDirectConsoleCmd(p_io: TPeerIO; const Cmd, ConsoleData: SystemString);
var
  p: PQueueData;
begin
  // init queue data
  if (p_io = nil) or (not p_io.Connected) then
      exit;
  if not CanSendCommand(p_io, Cmd) then
      exit;
  p_io.PrintCommand('Send DirectConsole cmd: %s', Cmd);

  p := NewQueueData;
  p^.State := TQueueState.qsSendDirectConsoleCMD;
  p^.IO_ID := p_io.ID;
  p^.Cmd := Cmd;
  p^.Cipher := p_io.FSendDataCipherSecurity;
  p^.ConsoleData := ConsoleData;
  TriggerQueueData(p);
end;

procedure TCommunicationFrameworkServer.SendDirectStreamCmd(p_io: TPeerIO; const Cmd: SystemString; StreamData: TCoreClassStream; DoneAutoFree: Boolean);
var
  p: PQueueData;
begin
  // init queue data
  if (p_io = nil) or (not p_io.Connected) then
      exit;
  if not CanSendCommand(p_io, Cmd) then
      exit;
  p_io.PrintCommand('Send DirectStream cmd: %s', Cmd);

  p := NewQueueData;
  p^.State := TQueueState.qsSendDirectStreamCMD;
  p^.IO_ID := p_io.ID;
  p^.Cmd := Cmd;
  p^.Cipher := p_io.FSendDataCipherSecurity;
  p^.DoneAutoFree := DoneAutoFree;
  p^.StreamData := StreamData;
  TriggerQueueData(p);
end;

procedure TCommunicationFrameworkServer.SendDirectStreamCmd(p_io: TPeerIO; const Cmd: SystemString; StreamData: TDataFrameEngine);
var
  p: PQueueData;
begin
  // init queue data
  if (p_io = nil) or (not p_io.Connected) then
      exit;
  if not CanSendCommand(p_io, Cmd) then
      exit;

  p_io.PrintCommand('Send DirectStream cmd: %s', Cmd);

  p := NewQueueData;
  p^.State := TQueueState.qsSendDirectStreamCMD;
  p^.IO_ID := p_io.ID;
  p^.Cmd := Cmd;
  p^.Cipher := p_io.FSendDataCipherSecurity;
  p^.DoneAutoFree := True;
  p^.StreamData := TMemoryStream64.Create;
  if StreamData <> nil then
      StreamData.EncodeTo(p^.StreamData, True)
  else
      TDataFrameEngine.BuildEmptyStream(p^.StreamData);
  TriggerQueueData(p);
end;

procedure TCommunicationFrameworkServer.SendDirectStreamCmd(p_io: TPeerIO; const Cmd: SystemString);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  SendDirectStreamCmd(p_io, Cmd, de);
  DisposeObject(de);
end;

function TCommunicationFrameworkServer.WaitSendConsoleCmd(p_io: TPeerIO; const Cmd, ConsoleData: SystemString; Timeout: TTimeTickValue): SystemString;
var
  waitIntf: TWaitSendConsoleCmdIntf;
  timetick: TTimeTickValue;
begin
  if (p_io = nil) or (not p_io.Connected) then
      exit('');
  if not CanSendCommand(p_io, Cmd) then
      exit('');

  p_io.PrintCommand('Begin Wait Console cmd: %s', Cmd);

  timetick := GetTimeTickCount + Timeout;

  while p_io.WaitOnResult or p_io.BigStreamReceiveing or p_io.FWaitSendBusy do
    begin
      ProgressWaitSend(p_io);
      if not Exists(p_io) then
          exit;
      if (Timeout > 0) and (GetTimeTickCount > timetick) then
          exit('');
    end;

  if not Exists(p_io) then
      exit('');

  p_io.FWaitSendBusy := True;

  try
    waitIntf := TWaitSendConsoleCmdIntf.Create;
    waitIntf.Done := False;
    waitIntf.NewResult := '';
    SendConsoleCmdM(p_io, Cmd, ConsoleData, {$IFDEF FPC}@{$ENDIF FPC}waitIntf.WaitSendConsoleResultEvent);
    while not waitIntf.Done do
      begin
        ProgressWaitSend(p_io);
        if not Exists(p_io) then
            Break;
        if (Timeout > 0) and (GetTimeTickCount > timetick) then
            Break;
      end;
    Result := waitIntf.NewResult;
    if waitIntf.Done then
        DisposeObject(waitIntf);
    p_io.PrintCommand('End Wait Console cmd: %s', Cmd);
  except
      Result := '';
  end;

  if Exists(p_io) then
      p_io.FWaitSendBusy := False;
end;

procedure TCommunicationFrameworkServer.WaitSendStreamCmd(p_io: TPeerIO; const Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; Timeout: TTimeTickValue);
var
  waitIntf: TWaitSendStreamCmdIntf;
  timetick: Cardinal;
begin
  if (p_io = nil) or (not p_io.Connected) then
      exit;
  if not CanSendCommand(p_io, Cmd) then
      exit;

  p_io.PrintCommand('Begin Wait Stream cmd: %s', Cmd);

  timetick := GetTimeTickCount + Timeout;

  while p_io.WaitOnResult or p_io.BigStreamReceiveing or p_io.FWaitSendBusy do
    begin
      ProgressWaitSend(p_io);
      if not Exists(p_io) then
          exit;
      if (Timeout > 0) and (GetTimeTickCount > timetick) then
          exit;
    end;

  if not Exists(p_io) then
      exit;

  p_io.FWaitSendBusy := True;

  try
    waitIntf := TWaitSendStreamCmdIntf.Create;
    waitIntf.Done := False;
    SendStreamCmdM(p_io, Cmd, StreamData, {$IFDEF FPC}@{$ENDIF FPC}waitIntf.WaitSendStreamResultEvent);
    while not waitIntf.Done do
      begin
        ProgressWaitSend(p_io);
        if not Exists(p_io) then
            Break;
        if (Timeout > 0) and (GetTimeTickCount > timetick) then
            Break;
      end;
    if waitIntf.Done then
      begin
        ResultData.Assign(waitIntf.NewResult);
        DisposeObject(waitIntf);
      end;
    p_io.PrintCommand('End Wait Stream cmd: %s', Cmd);
  except
  end;

  if Exists(p_io) then
      p_io.FWaitSendBusy := False;
end;

procedure TCommunicationFrameworkServer.SendBigStream(p_io: TPeerIO; const Cmd: SystemString; BigStream: TCoreClassStream; StartPos: Int64; DoneAutoFree: Boolean);
var
  p: PQueueData;
begin
  // init queue data
  if (p_io = nil) or (not p_io.Connected) then
      exit;
  if not CanSendCommand(p_io, Cmd) then
      exit;
  p := NewQueueData;
  p^.State := TQueueState.qsSendBigStream;
  p^.IO_ID := p_io.ID;
  p^.Cmd := Cmd;
  p^.Cipher := p_io.FSendDataCipherSecurity;
  p^.BigStreamStartPos := StartPos;
  p^.BigStream := BigStream;
  p^.DoneAutoFree := DoneAutoFree;
  TriggerQueueData(p);
  p_io.PrintCommand('Send BigStream cmd: %s', Cmd);
end;

procedure TCommunicationFrameworkServer.SendBigStream(p_io: TPeerIO; const Cmd: SystemString; BigStream: TCoreClassStream; DoneAutoFree: Boolean);
begin
  SendBigStream(p_io, Cmd, BigStream, 0, DoneAutoFree);
end;

procedure TCommunicationFrameworkServer.SendCompleteBuffer(p_io: TPeerIO; const Cmd: SystemString; buff: PByte; BuffSize: NativeInt; DoneAutoFree: Boolean);
var
  p: PQueueData;
begin
  // init queue data
  if (p_io = nil) or (not p_io.Connected) then
      exit;
  if not CanSendCommand(p_io, Cmd) then
      exit;
  p_io.PrintCommand('Send complete buffer cmd: %s', Cmd);

  p := NewQueueData;
  p^.State := TQueueState.qsSendCompleteBuffer;
  p^.IO_ID := p_io.ID;
  p^.Cmd := Cmd;
  p^.Cipher := p_io.FSendDataCipherSecurity;
  p^.buffer := buff;
  p^.BufferSize := BuffSize;
  p^.DoneAutoFree := DoneAutoFree;
  TriggerQueueData(p);
end;

procedure TCommunicationFrameworkServer.SendConsoleCmdM(IO_ID: Cardinal; const Cmd, ConsoleData: SystemString; OnResult: TConsoleMethod);
begin
  SendConsoleCmdM(PeerIO[IO_ID], Cmd, ConsoleData, OnResult);
end;

procedure TCommunicationFrameworkServer.SendStreamCmdM(IO_ID: Cardinal; const Cmd: SystemString; StreamData: TCoreClassStream; OnResult: TStreamMethod;
DoneAutoFree: Boolean);
begin
  SendStreamCmdM(PeerIO[IO_ID], Cmd, StreamData, OnResult, DoneAutoFree);
end;

procedure TCommunicationFrameworkServer.SendStreamCmdM(IO_ID: Cardinal; const Cmd: SystemString; StreamData: TDataFrameEngine; OnResult: TStreamMethod);
begin
  SendStreamCmdM(PeerIO[IO_ID], Cmd, StreamData, OnResult);
end;

procedure TCommunicationFrameworkServer.SendStreamCmdM(IO_ID: Cardinal; const Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamMethod);
begin
  SendStreamCmdM(PeerIO[IO_ID], Cmd, StreamData, Param1, Param2, OnResult);
end;

{$IFNDEF FPC}


procedure TCommunicationFrameworkServer.SendConsoleCmdP(IO_ID: Cardinal; const Cmd, ConsoleData: SystemString; OnResult: TConsoleProc);
begin
  SendConsoleCmdP(PeerIO[IO_ID], Cmd, ConsoleData, OnResult);
end;

procedure TCommunicationFrameworkServer.SendStreamCmdP(IO_ID: Cardinal; const Cmd: SystemString; StreamData: TCoreClassStream; OnResult: TStreamProc;
DoneAutoFree: Boolean);
begin
  SendStreamCmdP(PeerIO[IO_ID], Cmd, StreamData, OnResult, DoneAutoFree);
end;

procedure TCommunicationFrameworkServer.SendStreamCmdP(IO_ID: Cardinal; const Cmd: SystemString; StreamData: TDataFrameEngine; OnResult: TStreamProc);
begin
  SendStreamCmdP(PeerIO[IO_ID], Cmd, StreamData, OnResult);
end;

procedure TCommunicationFrameworkServer.SendStreamCmdP(IO_ID: Cardinal; const Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamProc);
begin
  SendStreamCmdP(PeerIO[IO_ID], Cmd, StreamData, Param1, Param2, OnResult);
end;
{$ENDIF FPC}


procedure TCommunicationFrameworkServer.SendDirectConsoleCmd(IO_ID: Cardinal; const Cmd, ConsoleData: SystemString);
begin
  SendDirectConsoleCmd(PeerIO[IO_ID], Cmd, ConsoleData);
end;

procedure TCommunicationFrameworkServer.SendDirectStreamCmd(IO_ID: Cardinal; const Cmd: SystemString; StreamData: TCoreClassStream; DoneAutoFree: Boolean);
begin
  SendDirectStreamCmd(PeerIO[IO_ID], Cmd, StreamData, DoneAutoFree);
end;

procedure TCommunicationFrameworkServer.SendDirectStreamCmd(IO_ID: Cardinal; const Cmd: SystemString; StreamData: TDataFrameEngine);
begin
  SendDirectStreamCmd(PeerIO[IO_ID], Cmd, StreamData);
end;

procedure TCommunicationFrameworkServer.SendDirectStreamCmd(IO_ID: Cardinal; const Cmd: SystemString);
begin
  SendDirectStreamCmd(PeerIO[IO_ID], Cmd);
end;

function TCommunicationFrameworkServer.WaitSendConsoleCmd(IO_ID: Cardinal; const Cmd, ConsoleData: SystemString; Timeout: TTimeTickValue): SystemString;
begin
  Result := WaitSendConsoleCmd(PeerIO[IO_ID], Cmd, ConsoleData, Timeout);
end;

procedure TCommunicationFrameworkServer.WaitSendStreamCmd(IO_ID: Cardinal; const Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; Timeout: TTimeTickValue);
begin
  WaitSendStreamCmd(PeerIO[IO_ID], Cmd, StreamData, ResultData, Timeout);
end;

procedure TCommunicationFrameworkServer.SendBigStream(IO_ID: Cardinal; const Cmd: SystemString; BigStream: TCoreClassStream; StartPos: Int64; DoneAutoFree: Boolean);
begin
  SendBigStream(PeerIO[IO_ID], Cmd, BigStream, StartPos, DoneAutoFree);
end;

procedure TCommunicationFrameworkServer.SendBigStream(IO_ID: Cardinal; const Cmd: SystemString; BigStream: TCoreClassStream; DoneAutoFree: Boolean);
begin
  SendBigStream(PeerIO[IO_ID], Cmd, BigStream, DoneAutoFree);
end;

procedure TCommunicationFrameworkServer.SendCompleteBuffer(IO_ID: Cardinal; const Cmd: SystemString; buff: PByte; BuffSize: NativeInt; DoneAutoFree: Boolean);
begin
  SendCompleteBuffer(PeerIO[IO_ID], Cmd, buff, BuffSize, DoneAutoFree);
end;

procedure TCommunicationFrameworkServer.BroadcastDirectConsoleCmd(Cmd: SystemString; ConsoleData: SystemString);
var
  IO_ID: Cardinal;
  IO_Array: TIO_Array;
  p_io: TPeerIO;
begin
  GetIO_Array(IO_Array);
  for IO_ID in IO_Array do
    begin
      p_io := PeerIO[IO_ID];
      if p_io <> nil then
          SendDirectConsoleCmd(p_io, Cmd, ConsoleData);
    end;
end;

procedure TCommunicationFrameworkServer.BroadcastSendDirectStreamCmd(Cmd: SystemString; StreamData: TDataFrameEngine);
var
  IO_ID: Cardinal;
  IO_Array: TIO_Array;
  p_io: TPeerIO;
begin
  GetIO_Array(IO_Array);
  for IO_ID in IO_Array do
    begin
      p_io := PeerIO[IO_ID];
      if p_io <> nil then
          SendDirectStreamCmd(p_io, Cmd, StreamData);
    end;
end;

function TCommunicationFrameworkServer.Count: Integer;
begin
  Result := FPeerIO_HashPool.Count;
end;

function TCommunicationFrameworkServer.Exists(p_io: TCoreClassObject): Boolean;
begin
  if p_io is TPeerIO then
      Result := Exists(p_io as TPeerIO)
  else if p_io is TPeerIOUserDefine then
      Result := Exists(p_io as TPeerIOUserDefine)
  else if p_io is TPeerIOUserSpecial then
      Result := Exists(p_io as TPeerIOUserSpecial)
  else
      Result := False;
end;

function TCommunicationFrameworkServer.Exists(p_io: TPeerIO): Boolean;
begin
  Result := FPeerIO_HashPool.ExistsObject(p_io);
end;

function TCommunicationFrameworkServer.Exists(p_io: TPeerIOUserDefine): Boolean;
var
  i: Integer;
  p: PUInt32HashListObjectStruct;
begin
  Result := False;
  if (FPeerIO_HashPool.Count > 0) then
    begin
      i := 0;
      p := FPeerIO_HashPool.FirstPtr;
      while i < FPeerIO_HashPool.Count do
        begin
          if TPeerIO(p^.Data).FUserDefine = p_io then
            begin
              Result := True;
              exit;
            end;
          inc(i);
          p := p^.Next;
        end;
    end;
end;

function TCommunicationFrameworkServer.Exists(p_io: TPeerIOUserSpecial): Boolean;
var
  i: Integer;
  p: PUInt32HashListObjectStruct;
begin
  Result := False;
  if (FPeerIO_HashPool.Count > 0) then
    begin
      i := 0;
      p := FPeerIO_HashPool.FirstPtr;
      while i < FPeerIO_HashPool.Count do
        begin
          if TPeerIO(p^.Data).FUserSpecial = p_io then
            begin
              Result := True;
              exit;
            end;
          inc(i);
          p := p^.Next;
        end;
    end;
end;

function TCommunicationFrameworkServer.Exists(IO_ID: Cardinal): Boolean;
begin
  Result := FPeerIO_HashPool.Exists(IO_ID);
end;

function TCommunicationFrameworkServer.GetPeerIO(ID: Cardinal): TPeerIO;
begin
  Result := TPeerIO(FPeerIO_HashPool[ID]);
end;

procedure TCommunicationFrameworkClient.DoPrint(const v: SystemString);
begin
  inherited DoPrint('p_io ' + v);
end;

procedure TCommunicationFrameworkClient.StreamResult_CipherModel(Sender: TPeerIO; ResultData: TDataFrameEngine);
var
  arr: TDataFrameArrayByte;
begin
  if ResultData.Count > 0 then
    begin
      FPeerIO_HashPool.Delete(Sender.FID);
      // index 0: my remote id
      Sender.FID := ResultData.Reader.ReadCardinal;
      FPeerIO_HashPool.Add(Sender.FID, Sender, True);
      // index 1: used Encrypt
      Sender.SendCipherSecurity := TCipherSecurity(ResultData.Reader.ReadByte);

      // index 2:Encrypt CipherKey
      arr := ResultData.Reader.ReadArrayByte;
      SetLength(Sender.FCipherKey, arr.Count);
      arr.GetBuff(@Sender.FCipherKey[0]);

      Sender.RemoteExecutedForConnectInit := True;

      if FConnectInitWaiting then
          TriggerDoConnectFinished;
    end
  else
    begin
      if FConnectInitWaiting then
          TriggerDoConnectFailed;
    end;

  FConnectInitWaiting := False;
end;

procedure TCommunicationFrameworkClient.DoConnected(Sender: TPeerIO);
var
  de: TDataFrameEngine;
begin
  FConnectInitWaiting := True;
  if Protocol = cpZServer then
    begin
      FConnectInitWaitingTimeout := GetTimeTick + FAsyncConnectTimeout;

      ClientIO.SendCipherSecurity := TCipherSecurity.csNone;
      de := TDataFrameEngine.Create;
      de.WriteInteger(Integer(CurrentPlatform));
      SendStreamCmdM(C_CipherModel, de, {$IFDEF FPC}@{$ENDIF FPC}StreamResult_CipherModel);
      DisposeObject(de);

      inherited DoConnected(Sender);

      if FNotyifyInterface <> nil then
        begin
          try
              FNotyifyInterface.ClientConnected(Self);
          except
          end;
        end;
    end
  else
    begin
      ClientIO.SendCipherSecurity := TCipherSecurity.csNone;
      inherited DoConnected(Sender);
      if FNotyifyInterface <> nil then
        begin
          try
              FNotyifyInterface.ClientConnected(Self);
          except
          end;
        end;

      Sender.RemoteExecutedForConnectInit := True;
      TriggerDoConnectFinished;
      FConnectInitWaiting := False;
    end;
end;

procedure TCommunicationFrameworkClient.DoDisconnect(Sender: TPeerIO);
begin
  FPeerIO_HashPool.Delete(Sender.FID);
  Sender.FID := 0;
  Sender.FRemoteExecutedForConnectInit := False;

  try
      inherited DoDisconnect(Sender);
  except
  end;

  try
    if FNotyifyInterface <> nil then
        FNotyifyInterface.ClientDisconnect(Self);
  except
  end;
end;

function TCommunicationFrameworkClient.CanExecuteCommand(Sender: TPeerIO; Cmd: SystemString): Boolean;
begin
  Result := inherited CanExecuteCommand(Sender, Cmd);
end;

function TCommunicationFrameworkClient.CanSendCommand(Sender: TPeerIO; Cmd: SystemString): Boolean;
begin
  if IsSystemCMD(Cmd) then
      Result := True
  else
      Result := inherited CanSendCommand(Sender, Cmd);
end;

function TCommunicationFrameworkClient.CanRegCommand(Sender: TCommunicationFramework; Cmd: SystemString): Boolean;
begin
  Result := inherited CanRegCommand(Sender, Cmd);
end;

procedure TCommunicationFrameworkClient.SyncFillCustomBuffer;
begin
  OnReceiveBuffer(FillSync_Buffer, FillSync_BufferSize);
end;

procedure TCommunicationFrameworkClient.FillCustomBuffer(Sender: TPeerIO; const th: TCoreClassThread; const buffer: PByte; const Size: NativeInt; var Customed: Boolean);
begin
  if Protocol = cpCustom then
    begin
      FillSync_Buffer := buffer;
      FillSync_BufferSize := Size;
      SyncMethod(th, th <> nil, {$IFDEF FPC}@{$ENDIF FPC}SyncFillCustomBuffer);
    end;
end;

procedure TCommunicationFrameworkClient.ConsoleResult_Wait(Sender: TPeerIO; ResultData: SystemString);
begin
  if FWaiting then
    begin
      FWaiting := False;
      FWaitingTimeOut := 0;
      try
        if Assigned(FOnWaitResultCall) then
            FOnWaitResultCall(True);
        if Assigned(FOnWaitResultMethod) then
            FOnWaitResultMethod(True);
{$IFNDEF FPC}
        if Assigned(FOnWaitResultProc) then
            FOnWaitResultProc(True);
{$ENDIF FPC}
      except
      end;

      FOnWaitResultCall := nil;
      FOnWaitResultMethod := nil;
{$IFNDEF FPC} FOnWaitResultProc := nil; {$ENDIF FPC}
    end;
end;

function TCommunicationFrameworkClient.FixedTimeout(const t: TTimeTickValue): TTimeTickValue;
begin
  if t = 0 then
      Result := 1000 * 60 * 5
  else
      Result := t;
end;

constructor TCommunicationFrameworkClient.Create;
begin
  inherited Create(1);
  FNotyifyInterface := nil;
  FConnectInitWaiting := False;
  FConnectInitWaitingTimeout := 0;

  FWaiting := False;
  FWaitingTimeOut := 0;
  FAsyncConnectTimeout := 2000;
  FOnWaitResultCall := nil;
  FOnWaitResultMethod := nil;
{$IFNDEF FPC} FOnWaitResultProc := nil; {$ENDIF FPC}
  FFrameworkIsServer := False;
  FFrameworkIsClient := True;
end;

procedure TCommunicationFrameworkClient.OnReceiveBuffer(const buffer: PByte; const Size: NativeInt);
begin
end;

procedure TCommunicationFrameworkClient.WriteBuffer(const buffer: PByte; const Size: NativeInt);
begin
  WriteCustomBuffer(ClientIO, buffer, Size);
end;

procedure TCommunicationFrameworkClient.Progress;
begin
  inherited Progress;

  if (FConnectInitWaiting) and (GetTimeTick > FConnectInitWaitingTimeout) then
    begin
      FConnectInitWaiting := False;

      try
          TriggerDoConnectFailed;
      except
      end;

      try
        if Connected then
            Disconnect;
      except
      end;
    end;

  if (FWaiting) and (GetTimeTick > FWaitingTimeOut) then
    begin
      FWaiting := False;
      FWaitingTimeOut := 0;
      try
        if Assigned(FOnWaitResultCall) then
            FOnWaitResultCall(False);
        if Assigned(FOnWaitResultMethod) then
            FOnWaitResultMethod(False);
{$IFNDEF FPC}
        if Assigned(FOnWaitResultProc) then
            FOnWaitResultProc(False);
{$ENDIF FPC}
      except
      end;

      FOnWaitResultCall := nil;
      FOnWaitResultMethod := nil;
{$IFNDEF FPC} FOnWaitResultProc := nil; {$ENDIF FPC}
    end;
end;

procedure TCommunicationFrameworkClient.TriggerDoDisconnect;
begin
  DoDisconnect(ClientIO);
end;

function TCommunicationFrameworkClient.Connected: Boolean;
begin
  Result := False;
end;

function TCommunicationFrameworkClient.ClientIO: TPeerIO;
begin
  Result := nil;
end;

procedure TCommunicationFrameworkClient.TriggerQueueData(v: PQueueData);
begin
end;

procedure TCommunicationFrameworkClient.TriggerDoConnectFailed;
begin
  FConnectInitWaiting := False;
end;

procedure TCommunicationFrameworkClient.TriggerDoConnectFinished;
begin
  FConnectInitWaiting := False;
end;

procedure TCommunicationFrameworkClient.AsyncConnectC(addr: SystemString; Port: Word; OnResult: TStateCall);
var
  r: Boolean;
begin
  r := Connect(addr, Port);
  if Assigned(OnResult) then
      OnResult(r);
end;

procedure TCommunicationFrameworkClient.AsyncConnectM(addr: SystemString; Port: Word; OnResult: TStateMethod);
var
  r: Boolean;
begin
  r := Connect(addr, Port);
  if Assigned(OnResult) then
      OnResult(r);
end;

{$IFNDEF FPC}


procedure TCommunicationFrameworkClient.AsyncConnectP(addr: SystemString; Port: Word; OnResult: TStateProc);
var
  r: Boolean;
begin
  r := Connect(addr, Port);
  if Assigned(OnResult) then
      OnResult(r);
end;
{$ENDIF FPC}


function TCommunicationFrameworkClient.Connect(addr: SystemString; Port: Word): Boolean;
begin
  Result := False;
end;

procedure TCommunicationFrameworkClient.Disconnect;
begin
end;

// sync KeepAlive
function TCommunicationFrameworkClient.Wait(ATimeOut: TTimeTickValue): SystemString;
begin
  Result := '';
  if (ClientIO = nil) then
      exit;
  if (not Connected) then
      exit;

  Result := WaitSendConsoleCmd(C_Wait, '', FixedTimeout(ATimeOut));
end;

function TCommunicationFrameworkClient.WaitC(ATimeOut: TTimeTickValue; OnResult: TStateCall): Boolean;
begin
  Result := False;
  if (ClientIO = nil) then
      exit;
  if (not Connected) then
      exit;
  if (FWaiting) then
      exit;

  FWaiting := True;
  FWaitingTimeOut := GetTimeTick + FixedTimeout(ATimeOut);
  FOnWaitResultCall := OnResult;
  FOnWaitResultMethod := nil;
{$IFNDEF FPC} FOnWaitResultProc := nil; {$ENDIF FPC}
  SendConsoleCmdM(C_Wait, '', {$IFDEF FPC}@{$ENDIF FPC}ConsoleResult_Wait);
  Result := True;
end;

function TCommunicationFrameworkClient.WaitM(ATimeOut: TTimeTickValue; OnResult: TStateMethod): Boolean;
begin
  Result := False;
  if (ClientIO = nil) then
      exit;
  if (not Connected) then
      exit;
  if (FWaiting) then
      exit;

  FWaiting := True;
  FWaitingTimeOut := GetTimeTick + FixedTimeout(ATimeOut);
  FOnWaitResultCall := nil;
  FOnWaitResultMethod := OnResult;
{$IFNDEF FPC} FOnWaitResultProc := nil; {$ENDIF FPC}
  SendConsoleCmdM(C_Wait, '', {$IFDEF FPC}@{$ENDIF FPC}ConsoleResult_Wait);

  Result := True;
end;

{$IFNDEF FPC}


function TCommunicationFrameworkClient.WaitP(ATimeOut: TTimeTickValue; OnResult: TStateProc): Boolean;
begin
  Result := False;
  if (ClientIO = nil) then
      exit;
  if (not Connected) then
      exit;
  if (FWaiting) then
      exit;

  FWaiting := True;
  FWaitingTimeOut := GetTimeTick + FixedTimeout(ATimeOut);
  FOnWaitResultCall := nil;
  FOnWaitResultMethod := nil;
  FOnWaitResultProc := OnResult;
  SendConsoleCmdM(C_Wait, '', ConsoleResult_Wait);
  Result := True;
end;
{$ENDIF FPC}


function TCommunicationFrameworkClient.WaitSendBusy: Boolean;
begin
  Result := (ClientIO <> nil) and (ClientIO.WaitSendBusy);
end;

function TCommunicationFrameworkClient.LastQueueData: PQueueData;
begin
  Result := nil;
  if ClientIO = nil then
      exit;
  if ClientIO.FQueueList.Count = 0 then
      exit;
  Result := PQueueData(ClientIO.FQueueList[ClientIO.FQueueList.Count - 1]);
end;

function TCommunicationFrameworkClient.LastQueueCmd: SystemString;
var
  p: PQueueData;
begin
  p := LastQueueData;
  if p <> nil then
      Result := p^.Cmd
  else
      Result := '';
end;

function TCommunicationFrameworkClient.QueueCmdCount: Integer;
begin
  Result := 0;
  if ClientIO = nil then
      exit;
  Result := ClientIO.FQueueList.Count;
end;

procedure TCommunicationFrameworkClient.SendConsoleCmdM(Cmd, ConsoleData: SystemString; OnResult: TConsoleMethod);
var
  p: PQueueData;
begin
  if ClientIO = nil then
      exit;
  if not Connected then
      exit;
  if not CanSendCommand(ClientIO, Cmd) then
      exit;
  ClientIO.PrintCommand('Send Console cmd: %s', Cmd);

  // init queue data
  p := NewQueueData;
  p^.State := TQueueState.qsSendConsoleCMD;

  p^.Cmd := Cmd;
  p^.Cipher := ClientIO.FSendDataCipherSecurity;
  p^.ConsoleData := ConsoleData;
  p^.OnConsoleMethod := OnResult;
  TriggerQueueData(p);
end;

procedure TCommunicationFrameworkClient.SendStreamCmdM(Cmd: SystemString; StreamData: TCoreClassStream; OnResult: TStreamMethod; DoneAutoFree: Boolean);
var
  p: PQueueData;
begin
  if ClientIO = nil then
      exit;
  if not Connected then
      exit;
  if not CanSendCommand(ClientIO, Cmd) then
      exit;
  ClientIO.PrintCommand('Send Stream cmd: %s', Cmd);

  // init queue data
  p := NewQueueData;
  p^.State := TQueueState.qsSendStreamCMD;

  p^.Cmd := Cmd;
  p^.Cipher := ClientIO.FSendDataCipherSecurity;
  p^.DoneAutoFree := DoneAutoFree;
  p^.StreamData := StreamData;
  p^.OnStreamMethod := OnResult;
  TriggerQueueData(p);
end;

procedure TCommunicationFrameworkClient.SendStreamCmdM(Cmd: SystemString; StreamData: TDataFrameEngine; OnResult: TStreamMethod);
var
  p: PQueueData;
begin
  if ClientIO = nil then
      exit;
  if not Connected then
      exit;
  if not CanSendCommand(ClientIO, Cmd) then
      exit;
  ClientIO.PrintCommand('Send Stream cmd: %s', Cmd);

  // init queue data
  p := NewQueueData;
  p^.State := TQueueState.qsSendStreamCMD;

  p^.Cmd := Cmd;
  p^.Cipher := ClientIO.FSendDataCipherSecurity;
  p^.DoneAutoFree := True;
  p^.StreamData := TMemoryStream64.Create;
  if StreamData <> nil then
      StreamData.EncodeTo(p^.StreamData, True)
  else
      TDataFrameEngine.BuildEmptyStream(p^.StreamData);
  p^.OnStreamMethod := OnResult;
  TriggerQueueData(p);
end;

procedure TCommunicationFrameworkClient.SendStreamCmdM(Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamMethod);
var
  p: PQueueData;
begin
  if ClientIO = nil then
      exit;
  if not Connected then
      exit;
  if not CanSendCommand(ClientIO, Cmd) then
      exit;
  ClientIO.PrintCommand('Send Stream cmd: %s', Cmd);

  // init queue data
  p := NewQueueData;
  p^.State := TQueueState.qsSendStreamCMD;

  p^.Cmd := Cmd;
  p^.Cipher := ClientIO.FSendDataCipherSecurity;
  p^.DoneAutoFree := True;
  p^.StreamData := TMemoryStream64.Create;
  if StreamData <> nil then
      StreamData.EncodeTo(p^.StreamData, True)
  else
      TDataFrameEngine.BuildEmptyStream(p^.StreamData);
  p^.OnStreamParamMethod := OnResult;
  p^.Param1 := Param1;
  p^.Param2 := Param2;
  TriggerQueueData(p);
end;

{$IFNDEF FPC}


procedure TCommunicationFrameworkClient.SendConsoleCmdP(Cmd, ConsoleData: SystemString; OnResult: TConsoleProc);
var
  p: PQueueData;
begin
  if ClientIO = nil then
      exit;
  if not Connected then
      exit;
  if not CanSendCommand(ClientIO, Cmd) then
      exit;
  ClientIO.PrintCommand('Send Console cmd: %s', Cmd);

  // init queue data
  p := NewQueueData;
  p^.State := TQueueState.qsSendConsoleCMD;

  p^.Cmd := Cmd;
  p^.Cipher := ClientIO.FSendDataCipherSecurity;
  p^.ConsoleData := ConsoleData;
  p^.OnConsoleProc := OnResult;
  TriggerQueueData(p);
end;

procedure TCommunicationFrameworkClient.SendStreamCmdP(Cmd: SystemString; StreamData: TCoreClassStream; OnResult: TStreamProc; DoneAutoFree: Boolean);
var
  p: PQueueData;
begin
  if ClientIO = nil then
      exit;
  if not Connected then
      exit;
  if not CanSendCommand(ClientIO, Cmd) then
      exit;
  ClientIO.PrintCommand('Send Stream cmd: %s', Cmd);

  // init queue data
  p := NewQueueData;
  p^.State := TQueueState.qsSendStreamCMD;

  p^.Cmd := Cmd;
  p^.Cipher := ClientIO.FSendDataCipherSecurity;
  p^.DoneAutoFree := DoneAutoFree;
  p^.StreamData := StreamData;
  p^.OnStreamProc := OnResult;
  TriggerQueueData(p);
end;

procedure TCommunicationFrameworkClient.SendStreamCmdP(Cmd: SystemString; StreamData: TDataFrameEngine; OnResult: TStreamProc);
var
  p: PQueueData;
begin
  if ClientIO = nil then
      exit;
  if not Connected then
      exit;
  if not CanSendCommand(ClientIO, Cmd) then
      exit;
  ClientIO.PrintCommand('Send Stream cmd: %s', Cmd);

  // init queue data
  p := NewQueueData;
  p^.State := TQueueState.qsSendStreamCMD;

  p^.Cmd := Cmd;
  p^.Cipher := ClientIO.FSendDataCipherSecurity;
  p^.DoneAutoFree := True;
  p^.StreamData := TMemoryStream64.Create;
  if StreamData <> nil then
      StreamData.EncodeTo(p^.StreamData, True)
  else
      TDataFrameEngine.BuildEmptyStream(p^.StreamData);
  p^.OnStreamProc := OnResult;
  TriggerQueueData(p);
end;

procedure TCommunicationFrameworkClient.SendStreamCmdP(Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamProc);
var
  p: PQueueData;
begin
  if ClientIO = nil then
      exit;
  if not Connected then
      exit;
  if not CanSendCommand(ClientIO, Cmd) then
      exit;
  ClientIO.PrintCommand('Send Stream cmd: %s', Cmd);

  // init queue data
  p := NewQueueData;
  p^.State := TQueueState.qsSendStreamCMD;

  p^.Cmd := Cmd;
  p^.Cipher := ClientIO.FSendDataCipherSecurity;
  p^.DoneAutoFree := True;
  p^.StreamData := TMemoryStream64.Create;
  if StreamData <> nil then
      StreamData.EncodeTo(p^.StreamData, True)
  else
      TDataFrameEngine.BuildEmptyStream(p^.StreamData);
  p^.OnStreamParamProc := OnResult;
  p^.Param1 := Param1;
  p^.Param2 := Param2;
  TriggerQueueData(p);
end;
{$ENDIF FPC}


procedure TCommunicationFrameworkClient.SendDirectConsoleCmd(Cmd, ConsoleData: SystemString);
var
  p: PQueueData;
begin
  if ClientIO = nil then
      exit;
  if not Connected then
      exit;
  if not CanSendCommand(ClientIO, Cmd) then
      exit;
  ClientIO.PrintCommand('Send DirectConsole cmd: %s', Cmd);

  // init queue data
  p := NewQueueData;
  p^.State := TQueueState.qsSendDirectConsoleCMD;

  p^.Cmd := Cmd;
  p^.Cipher := ClientIO.FSendDataCipherSecurity;
  p^.ConsoleData := ConsoleData;
  TriggerQueueData(p);
end;

procedure TCommunicationFrameworkClient.SendDirectStreamCmd(Cmd: SystemString; StreamData: TCoreClassStream; DoneAutoFree: Boolean);
var
  p: PQueueData;
begin
  if ClientIO = nil then
      exit;
  if not Connected then
      exit;
  if not CanSendCommand(ClientIO, Cmd) then
      exit;
  ClientIO.PrintCommand('Send DirectStream cmd: %s', Cmd);

  // init queue data
  p := NewQueueData;
  p^.State := TQueueState.qsSendDirectStreamCMD;

  p^.Cmd := Cmd;
  p^.Cipher := ClientIO.FSendDataCipherSecurity;
  p^.DoneAutoFree := DoneAutoFree;
  p^.StreamData := StreamData;
  TriggerQueueData(p);
end;

procedure TCommunicationFrameworkClient.SendDirectStreamCmd(Cmd: SystemString; StreamData: TDataFrameEngine);
var
  p: PQueueData;
begin
  if ClientIO = nil then
      exit;
  if not Connected then
      exit;
  if not CanSendCommand(ClientIO, Cmd) then
      exit;
  ClientIO.PrintCommand('Send DirectStream cmd: %s', Cmd);

  // init queue data
  p := NewQueueData;
  p^.State := TQueueState.qsSendDirectStreamCMD;

  p^.Cmd := Cmd;
  p^.Cipher := ClientIO.FSendDataCipherSecurity;
  p^.DoneAutoFree := True;
  p^.StreamData := TMemoryStream64.Create;
  if StreamData <> nil then
      StreamData.EncodeTo(p^.StreamData, True)
  else
      TDataFrameEngine.BuildEmptyStream(p^.StreamData);
  TriggerQueueData(p);
end;

procedure TCommunicationFrameworkClient.SendDirectStreamCmd(Cmd: SystemString);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  SendDirectStreamCmd(Cmd, de);
  DisposeObject(de);
end;

function TCommunicationFrameworkClient.WaitSendConsoleCmd(Cmd: SystemString; ConsoleData: SystemString; Timeout: TTimeTickValue): SystemString;
var
  waitIntf: TWaitSendConsoleCmdIntf;
  timetick: TTimeTickValue;
begin
  Result := '';
  if ClientIO = nil then
      exit;
  if not Connected then
      exit;
  if not CanSendCommand(ClientIO, Cmd) then
      exit;
  ClientIO.PrintCommand('Begin Wait console cmd: %s', Cmd);

  timetick := GetTimeTickCount + Timeout;

  while ClientIO.WaitOnResult or ClientIO.BigStreamReceiveing or ClientIO.FWaitSendBusy do
    begin
      ProgressWaitSend(ClientIO);
      if not Connected then
          exit;
      if (Timeout > 0) and (GetTimeTickCount > timetick) then
          exit;
    end;

  if not Connected then
      exit('');

  ClientIO.FWaitSendBusy := True;

  try
    waitIntf := TWaitSendConsoleCmdIntf.Create;
    waitIntf.Done := False;
    waitIntf.NewResult := '';
    SendConsoleCmdM(Cmd, ConsoleData, {$IFDEF FPC}@{$ENDIF FPC}waitIntf.WaitSendConsoleResultEvent);
    while not waitIntf.Done do
      begin
        ProgressWaitSend(ClientIO);
        if not Connected then
            Break;
        if (Timeout > 0) and (GetTimeTickCount > timetick) then
            Break;
      end;
    Result := waitIntf.NewResult;
    try
      if ClientIO <> nil then
          ClientIO.PrintCommand('End Wait console cmd: %s', Cmd);
    except
    end;
    if waitIntf.Done then
        DisposeObject(waitIntf);
  except
      Result := '';
  end;

  if Connected then
      ClientIO.FWaitSendBusy := False;
end;

procedure TCommunicationFrameworkClient.WaitSendStreamCmd(Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; Timeout: TTimeTickValue);
var
  waitIntf: TWaitSendStreamCmdIntf;
  timetick: TTimeTickValue;
begin
  if ClientIO = nil then
      exit;
  if not Connected then
      exit;
  if not CanSendCommand(ClientIO, Cmd) then
      exit;

  ClientIO.PrintCommand('Begin Wait Stream cmd: %s', Cmd);

  timetick := GetTimeTickCount + Timeout;

  while ClientIO.WaitOnResult or ClientIO.BigStreamReceiveing or ClientIO.FWaitSendBusy do
    begin
      ProgressWaitSend(ClientIO);
      if not Connected then
          exit;
      if (Timeout > 0) and (GetTimeTickCount > timetick) then
          exit;
    end;

  if not Connected then
      exit;

  ClientIO.FWaitSendBusy := True;

  try
    waitIntf := TWaitSendStreamCmdIntf.Create;
    waitIntf.Done := False;
    SendStreamCmdM(Cmd, StreamData, {$IFDEF FPC}@{$ENDIF FPC}waitIntf.WaitSendStreamResultEvent);
    while not waitIntf.Done do
      begin
        ProgressWaitSend(ClientIO);
        if not Connected then
            Break;
        if (Timeout > 0) and (GetTimeTickCount > timetick) then
            Break;
      end;
    try
      if ClientIO <> nil then
          ClientIO.PrintCommand('End Wait Stream cmd: %s', Cmd);
    except
    end;

    if waitIntf.Done then
      begin
        ResultData.Assign(waitIntf.NewResult);
        DisposeObject(waitIntf);
      end;
  except
  end;

  if Connected then
      ClientIO.FWaitSendBusy := False;
end;

procedure TCommunicationFrameworkClient.SendBigStream(Cmd: SystemString; BigStream: TCoreClassStream; StartPos: Int64; DoneAutoFree: Boolean);
var
  p: PQueueData;
begin
  if ClientIO = nil then
      exit;
  if not Connected then
      exit;
  if not CanSendCommand(ClientIO, Cmd) then
      exit;
  // init queue data
  p := NewQueueData;
  p^.State := TQueueState.qsSendBigStream;

  p^.Cmd := Cmd;
  p^.Cipher := ClientIO.FSendDataCipherSecurity;
  p^.BigStreamStartPos := StartPos;
  p^.BigStream := BigStream;
  p^.DoneAutoFree := DoneAutoFree;
  TriggerQueueData(p);
  ClientIO.PrintCommand('Send BigStream cmd: %s', Cmd);
end;

procedure TCommunicationFrameworkClient.SendBigStream(Cmd: SystemString; BigStream: TCoreClassStream; DoneAutoFree: Boolean);
begin
  SendBigStream(Cmd, BigStream, 0, DoneAutoFree);
end;

procedure TCommunicationFrameworkClient.SendCompleteBuffer(Cmd: SystemString; buff: PByte; BuffSize: NativeInt; DoneAutoFree: Boolean);
var
  p: PQueueData;
begin
  if ClientIO = nil then
      exit;
  if not Connected then
      exit;
  if not CanSendCommand(ClientIO, Cmd) then
      exit;
  // init queue data
  p := NewQueueData;
  p^.State := TQueueState.qsSendCompleteBuffer;

  p^.Cmd := Cmd;
  p^.Cipher := ClientIO.FSendDataCipherSecurity;
  p^.buffer := buff;
  p^.BufferSize := BuffSize;
  p^.DoneAutoFree := DoneAutoFree;
  TriggerQueueData(p);
  ClientIO.PrintCommand('Send complete buffer cmd: %s', Cmd);
end;

function TCommunicationFrameworkClient.RemoteID: Cardinal;
begin
  if ClientIO <> nil then
      Result := ClientIO.FID
  else
      Result := 0;
end;

function TCommunicationFrameworkClient.RemoteKey: TCipherKeyBuffer;
begin
  Result := ClientIO.CipherKey;
end;

function TCommunicationFrameworkClient.RemoteInited: Boolean;
begin
  if ClientIO <> nil then
      Result := ClientIO.FRemoteExecutedForConnectInit
  else
      Result := False;
end;

procedure Tp2pVMFragmentPackage.Init;
begin
  buffSiz := 0;
  frameworkID := 0;
  p2pID := 0;
  pkType := 0;
  buff := nil;
end;

function Tp2pVMFragmentPackage.FillReceiveBuff(stream: TMemoryStream64): Integer;
begin
  Result := 0;
  if stream.Size < 13 then
    begin
      Init;
      exit;
    end;
  if stream.Size < PCardinal(stream.PositionAsPtr(0))^ + 13 then
    begin
      Init;
      exit;
    end;
  buffSiz := PCardinal(stream.PositionAsPtr(0))^;
  frameworkID := PCardinal(stream.PositionAsPtr(4))^;
  p2pID := PCardinal(stream.PositionAsPtr(8))^;
  pkType := PByte(stream.PositionAsPtr(12))^;
  if buffSiz > 0 then
      buff := stream.PositionAsPtr(13)
  else
      buff := nil;
  Result := buffSiz + 13;
end;

procedure Tp2pVMFragmentPackage.BuildSendBuff(stream: TMemoryStream64);
begin
  stream.WritePtr(@buffSiz, 4);
  stream.WritePtr(@frameworkID, 4);
  stream.WritePtr(@p2pID, 4);
  stream.WritePtr(@pkType, 1);
  if buffSiz > 0 then
      stream.WritePtr(buff, buffSiz);
end;

procedure TPeerClientWithP2PVM.CreateAfter;
begin
  inherited CreateAfter;
  FLinkVM := nil;
  FRealSendBuff := TMemoryStream64.Create;
  FSendQueue := TCoreClassList.Create;
  FRemote_frameworkID := 0;
  FRemote_p2pID := 0;
  FillPtrByte(@FIP, SizeOf(FIP), 0);
  FPort := 0;
  FDestroyTimeNotify := True;
end;

destructor TPeerClientWithP2PVM.Destroy;
var
  i: Integer;
begin
  if Connected then
    begin
      if not FOwnerFramework.FQuietMode then
          DoStatus('VMClient %d disconnect', [ID]);

      if (FDestroyTimeNotify) then
          FLinkVM.Disconnect(FRemote_frameworkID, FRemote_p2pID);
    end;

  for i := 0 to FSendQueue.Count - 1 do
      FreeP2PVMPackage(FSendQueue[i]);
  DisposeObject(FSendQueue);
  DisposeObject(FRealSendBuff);
  inherited Destroy;
end;

function TPeerClientWithP2PVM.Connected: Boolean;
begin
  if FLinkVM = nil then
      Result := False
  else if FOwnerFramework is TCommunicationFrameworkWithP2PVM_Server then
      Result := (FLinkVM.FPhysicsTunnel <> nil)
  else if FOwnerFramework is TCommunicationFrameworkWithP2PVM_Client then
      Result := TCommunicationFrameworkWithP2PVM_Client(FOwnerFramework).Connected
  else
      Result := False;
end;

procedure TPeerClientWithP2PVM.Disconnect;
begin
  if FLinkVM <> nil then
      FLinkVM.DisconnectWithVM(Self);
  DisposeObject(Self);
end;

procedure TPeerClientWithP2PVM.SendByteBuffer(const buff: PByte; const Size: NativeInt);
begin
  if Size <= 0 then
      exit;
  FRealSendBuff.Position := FRealSendBuff.Size;
  FRealSendBuff.WritePtr(buff, Size);
end;

procedure TPeerClientWithP2PVM.WriteBufferOpen;
begin
  FRealSendBuff.Clear;
end;

procedure TPeerClientWithP2PVM.WriteBufferFlush;
var
  p: PByte;
  siz: Integer;
begin
  if FRealSendBuff.Size <= 0 then
      exit;

  if FLinkVM <> nil then
    begin
      p := FRealSendBuff.Memory;
      siz := FRealSendBuff.Size;

      // fill fragment
      while siz > FLinkVM.FMaxVMFragmentSize do
        begin
          FSendQueue.Add(BuildP2PVMPackage(FLinkVM.FMaxVMFragmentSize, FRemote_frameworkID, FRemote_p2pID, FLinkVM.c_p2pVM_LogicFragmentData, p));
          inc(p, FLinkVM.FMaxVMFragmentSize);
          dec(siz, FLinkVM.FMaxVMFragmentSize);
        end;

      if siz > 0 then
          FSendQueue.Add(BuildP2PVMPackage(siz, FRemote_frameworkID, FRemote_p2pID, FLinkVM.c_p2pVM_LogicFragmentData, p));
    end;

  FRealSendBuff.Clear;
end;

procedure TPeerClientWithP2PVM.WriteBufferClose;
begin
  WriteBufferFlush;
end;

function TPeerClientWithP2PVM.GetPeerIP: SystemString;
begin
  Result := IPv6ToStr(FIP).Text;
end;

function TPeerClientWithP2PVM.WriteBufferEmpty: Boolean;
begin
  Result := FRealSendBuff.Size = 0;
end;

procedure TPeerClientWithP2PVM.Progress;
begin
  inherited Progress;
  ProcessAllSendCmd(nil, False, False);
end;

procedure TCommunicationFrameworkWithP2PVM_Server.Connecting(SenderVM: TCommunicationFrameworkWithP2PVM;
const Remote_frameworkID, frameworkID: Cardinal; const ipv6: TIPV6; const Port: Word; var Allowed: Boolean);
var
  p: Pp2pVMListen;
  LocalVMc: TPeerClientWithP2PVM;
begin
  if FLinkVMPool.Count = 0 then
    begin
      Allowed := False;
      exit;
    end;

  p := SenderVM.FindListen(ipv6, Port);
  Allowed := (p <> nil) and (p^.frameworkID = frameworkID);

  if Allowed then
    begin
      // build p_io
      LocalVMc := TPeerClientWithP2PVM.Create(Self, nil);
      LocalVMc.FLinkVM := SenderVM;
      LocalVMc.FRemote_frameworkID := Remote_frameworkID;
      LocalVMc.FRemote_p2pID := 0;
      LocalVMc.FIP := ipv6;
      LocalVMc.FPort := Port;

      // connected reponse
      SenderVM.ConnectedReponse(LocalVMc.FRemote_frameworkID, LocalVMc.FRemote_p2pID, frameworkID, LocalVMc.ID);

      if not FQuietMode then
          DoStatus('Virtual connecting with "%s port:%d"', [IPv6ToStr(ipv6).Text, Port]);
    end;
end;

procedure TCommunicationFrameworkWithP2PVM_Server.ListenState(SenderVM: TCommunicationFrameworkWithP2PVM; const ipv6: TIPV6; const Port: Word; const State: Boolean);
begin
  if not FQuietMode then
    begin
      if State then
          DoStatus('Virtual Addr: "%s Port:%d" Listen is open', [IPv6ToStr(ipv6).Text, Port])
      else
          DoStatus('Virtual Addr: "%s Port:%d" Listen close!', [IPv6ToStr(ipv6).Text, Port]);
    end;
end;

procedure TCommunicationFrameworkWithP2PVM_Server.ProgressDisconnectClient(PeerClient: TPeerIO);
begin
  TPeerClientWithP2PVM(PeerClient).FLinkVM.DisconnectWithVM(PeerClient);
  DisposeObject(PeerClient);
end;

function TCommunicationFrameworkWithP2PVM_Server.ListenCount: Integer;
begin
  Result := FFrameworkListenPool.Count;
end;

function TCommunicationFrameworkWithP2PVM_Server.GetListen(const index: Integer): Pp2pVMListen;
begin
  Result := FFrameworkListenPool[index];
end;

function TCommunicationFrameworkWithP2PVM_Server.FindListen(const ipv6: TIPV6; const Port: Word): Pp2pVMListen;
var
  i: Integer;
  p: Pp2pVMListen;
begin
  for i := 0 to FFrameworkListenPool.Count - 1 do
    begin
      p := FFrameworkListenPool[i];
      if (p^.ListenPort = Port) and (CompareIPV6(p^.ListenHost, ipv6)) then
        begin
          Result := p;
          exit;
        end;
    end;
  Result := nil;
end;

function TCommunicationFrameworkWithP2PVM_Server.FindListening(const ipv6: TIPV6; const Port: Word): Pp2pVMListen;
var
  i: Integer;
  p: Pp2pVMListen;
begin
  for i := 0 to FFrameworkListenPool.Count - 1 do
    begin
      p := FFrameworkListenPool[i];
      if (p^.Listening) and (p^.ListenPort = Port) and (CompareIPV6(p^.ListenHost, ipv6)) then
        begin
          Result := p;
          exit;
        end;
    end;
  Result := nil;
end;

procedure TCommunicationFrameworkWithP2PVM_Server.DeleteListen(const ipv6: TIPV6; const Port: Word);
var
  i: Integer;
  p: Pp2pVMListen;
begin
  i := 0;
  while i < FFrameworkListenPool.Count do
    begin
      p := FFrameworkListenPool[i];
      if (p^.ListenPort = Port) and (CompareIPV6(p^.ListenHost, ipv6)) then
        begin
          Dispose(p);
          FFrameworkListenPool.Delete(i);
        end
      else
          inc(i);
    end;
end;

procedure TCommunicationFrameworkWithP2PVM_Server.ClearListen;
var
  i: Integer;
begin
  for i := 0 to FFrameworkListenPool.Count - 1 do
      Dispose(Pp2pVMListen(FFrameworkListenPool[i]));
  FFrameworkListenPool.Clear;
end;

constructor TCommunicationFrameworkWithP2PVM_Server.Create;
begin
  Create(10 * 10000, 0);
end;

constructor TCommunicationFrameworkWithP2PVM_Server.Create(HashPoolLen: Integer; frameworkID: Cardinal);
begin
  inherited CreateCustomHashPool(HashPoolLen);
  FEnabledAtomicLockAndMultiThread := False;
  FFrameworkListenPool := TCoreClassList.Create;
  FLinkVMPool := TUInt32HashObjectList.Create;
  FFrameworkWithVM_ID := frameworkID;
  StopService;
end;

destructor TCommunicationFrameworkWithP2PVM_Server.Destroy;
var
  i: Integer;
  p: PUInt32HashListObjectStruct;
begin
  CloseAllClient;
  ClearListen;

  if (FLinkVMPool.Count > 0) then
    begin
      i := 0;
      p := FLinkVMPool.FirstPtr;
      while i < FLinkVMPool.Count do
        begin
          try
            (TCommunicationFrameworkWithP2PVM(p^.Data)).UninstallLogicFramework(Self);
          except
          end;
          inc(i);
          p := p^.Next;
        end;
    end;

  DisposeObject(FLinkVMPool);
  DisposeObject(FFrameworkListenPool);
  inherited Destroy;
end;

procedure TCommunicationFrameworkWithP2PVM_Server.Progress;
begin
  inherited Progress;
end;

procedure TCommunicationFrameworkWithP2PVM_Server.TriggerQueueData(v: PQueueData);
var
  c: TPeerIO;
begin
  c := PeerIO[v^.IO_ID];
  if c <> nil then
    begin
      c.PostQueueData(v);
      c.ProcessAllSendCmd(nil, False, False);
    end;
end;

procedure TCommunicationFrameworkWithP2PVM_Server.CloseAllClient;
begin
  ProgressPerClientM({$IFDEF FPC}@{$ENDIF FPC}ProgressDisconnectClient);
end;

procedure TCommunicationFrameworkWithP2PVM_Server.ProgressStopServiceWithPerVM(SenderVM: TCommunicationFrameworkWithP2PVM);
var
  i: Integer;
  p: Pp2pVMListen;
  lst: TCoreClassList;
begin
  lst := TCoreClassList.Create;

  for i := 0 to SenderVM.ListenCount - 1 do
    begin
      p := SenderVM.GetListen(i);
      if SenderVM.FFrameworkPool[p^.frameworkID] = Self then
          lst.Add(p);
    end;

  for i := 0 to lst.Count - 1 do
    begin
      p := lst[i];
      SenderVM.Listen(p^.frameworkID, p^.ListenHost, p^.ListenPort, False);
    end;
  DisposeObject(lst);
end;

procedure TCommunicationFrameworkWithP2PVM_Server.StopService;
var
  i: Integer;
  p: PUInt32HashListObjectStruct;
begin
  if (FLinkVMPool.Count > 0) then
    begin
      i := 0;
      p := FLinkVMPool.FirstPtr;
      while i < FLinkVMPool.Count do
        begin
          try
              ProgressStopServiceWithPerVM(TCommunicationFrameworkWithP2PVM(p^.Data));
          except
          end;
          inc(i);
          p := p^.Next;
        end;
    end;

  ClearListen;

  CloseAllClient;
end;

function TCommunicationFrameworkWithP2PVM_Server.StartService(Host: SystemString; Port: Word): Boolean;
var
  ipv6: TIPV6;
  SI: Cardinal;
  i: Integer;
  p: PUInt32HashListObjectStruct;
  LP: Pp2pVMListen;
begin
  Result := False;

  ipv6 := StrToIPv6(Host, Result, SI);

  if not Result then
      exit;

  LP := FindListen(ipv6, Port);
  if LP = nil then
    begin
      new(LP);
      LP^.frameworkID := FFrameworkWithVM_ID;
      LP^.ListenHost := ipv6;
      LP^.ListenPort := Port;
      LP^.Listening := True;
      FFrameworkListenPool.Add(LP);
    end
  else
      LP^.Listening := True;

  if (FLinkVMPool.Count > 0) then
    begin
      i := 0;
      p := FLinkVMPool.FirstPtr;
      while i < FLinkVMPool.Count do
        begin
          try
              TCommunicationFrameworkWithP2PVM(p^.Data).Listen(FFrameworkWithVM_ID, ipv6, Port, True);
          except
          end;
          inc(i);
          p := p^.Next;
        end;
    end
  else
    begin
      ListenState(nil, ipv6, Port, True);
    end;
  Result := True;
end;

function TCommunicationFrameworkWithP2PVM_Server.WaitSendConsoleCmd(p_io: TPeerIO; const Cmd, ConsoleData: SystemString; Timeout: TTimeTickValue): SystemString;
begin
  Result := '';
  RaiseInfo('WaitSend no Suppport VM server');
end;

procedure TCommunicationFrameworkWithP2PVM_Server.WaitSendStreamCmd(p_io: TPeerIO; const Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; Timeout: TTimeTickValue);
begin
  RaiseInfo('WaitSend no Suppport VM server');
end;

procedure TCommunicationFrameworkWithP2PVM_Client.InternalClientCreate(const Sender: TPeerIO);
begin
  inherited InternalClientCreate(Sender);
end;

procedure TCommunicationFrameworkWithP2PVM_Client.InternalClientDestroy(const Sender: TPeerIO);
begin
  FVMConnected := False;
  inherited InternalClientDestroy(Sender);
  FVMClient := nil;
  if FDestroying then
      exit;

  if FLinkVM = nil then
      RaiseInfo('no vm reference');

  FVMClient := TPeerClientWithP2PVM.Create(Self, nil);
  FVMClient.FLinkVM := FLinkVM;
end;

procedure TCommunicationFrameworkWithP2PVM_Client.VMConnectSuccessed(SenderVM: TCommunicationFrameworkWithP2PVM; Remote_frameworkID, Remote_p2pID, frameworkID: Cardinal);
begin
  FVMClient.FRemote_frameworkID := Remote_frameworkID;
  FVMClient.FRemote_p2pID := Remote_p2pID;

  FVMConnected := True;
  DoConnected(FVMClient);
end;

procedure TCommunicationFrameworkWithP2PVM_Client.VMDisconnect(SenderVM: TCommunicationFrameworkWithP2PVM);
begin
  FVMConnected := False;
  TriggerDoConnectFailed;
  FVMClient.Disconnect;
end;

constructor TCommunicationFrameworkWithP2PVM_Client.Create;
begin
  Create(0);
end;

constructor TCommunicationFrameworkWithP2PVM_Client.Create(frameworkID: Cardinal);
begin
  inherited Create;
  FEnabledAtomicLockAndMultiThread := False;
  FLinkVM := nil;
  FFrameworkWithVM_ID := frameworkID;
  FVMClient := nil;
  FVMConnected := False;
  FDestroying := False;

  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := nil;
{$IFNDEF FPC} FOnAsyncConnectNotifyProc := nil; {$ENDIF FPC}
end;

destructor TCommunicationFrameworkWithP2PVM_Client.Destroy;
begin
  FDestroying := True;
  if FVMClient <> nil then
      DisposeObject(FVMClient);
  if FLinkVM <> nil then
      FLinkVM.UninstallLogicFramework(Self);
  inherited Destroy;
end;

procedure TCommunicationFrameworkWithP2PVM_Client.TriggerDoConnectFailed;
begin
  inherited TriggerDoConnectFailed;

  try
    if Assigned(FOnAsyncConnectNotifyCall) then
        FOnAsyncConnectNotifyCall(False);
    if Assigned(FOnAsyncConnectNotifyMethod) then
        FOnAsyncConnectNotifyMethod(False);
{$IFNDEF FPC}
    if Assigned(FOnAsyncConnectNotifyProc) then
        FOnAsyncConnectNotifyProc(False);
{$ENDIF FPC}
  except
  end;

  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := nil;
{$IFNDEF FPC} FOnAsyncConnectNotifyProc := nil; {$ENDIF FPC}
end;

procedure TCommunicationFrameworkWithP2PVM_Client.TriggerDoConnectFinished;
begin
  inherited TriggerDoConnectFinished;

  try
    if Assigned(FOnAsyncConnectNotifyCall) then
        FOnAsyncConnectNotifyCall(True);
    if Assigned(FOnAsyncConnectNotifyMethod) then
        FOnAsyncConnectNotifyMethod(True);
{$IFNDEF FPC}
    if Assigned(FOnAsyncConnectNotifyProc) then
        FOnAsyncConnectNotifyProc(True);
{$ENDIF FPC}
  except
  end;

  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := nil;
{$IFNDEF FPC} FOnAsyncConnectNotifyProc := nil; {$ENDIF FPC}
end;

function TCommunicationFrameworkWithP2PVM_Client.Connected: Boolean;
begin
  Result := (FVMConnected) and (FVMClient <> nil);
end;

function TCommunicationFrameworkWithP2PVM_Client.ClientIO: TPeerIO;
begin
  Result := FVMClient;
end;

procedure TCommunicationFrameworkWithP2PVM_Client.Progress;
begin
  inherited Progress;
end;

procedure TCommunicationFrameworkWithP2PVM_Client.TriggerQueueData(v: PQueueData);
begin
  if Connected then
    begin
      FVMClient.PostQueueData(v);
      FVMClient.ProcessAllSendCmd(nil, False, False);
    end
  else
      DisposeQueueData(v);
end;

procedure TCommunicationFrameworkWithP2PVM_Client.AsyncConnect(addr: SystemString; Port: Word);
var
  r: Boolean;
  ipv6: TIPV6;
  p: Pp2pVMListen;
begin
  Disconnect;

  FVMConnected := False;

  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := nil;
{$IFNDEF FPC} FOnAsyncConnectNotifyProc := nil; {$ENDIF FPC}
  if (FLinkVM = nil) or (FLinkVM.FPhysicsTunnel = nil) then
    begin
      if not FQuietMode then
          DoStatus('no VM connect');
      TriggerDoConnectFailed;
      exit;
    end;

  if not FLinkVM.WasAuthed then
    begin
      if not FQuietMode then
          DoStatus('VM no auth');
      TriggerDoConnectFailed;
      exit;
    end;

  ipv6 := StrToIPv6(addr, r);

  if not r then
    begin
      if not FQuietMode then
          DoStatus('ipv6 format error! %s', [addr]);
      TriggerDoConnectFailed;
      exit;
    end;

  p := FLinkVM.FindListen(ipv6, Port);
  if p = nil then
    begin
      if not FQuietMode then
          DoStatus('no remote listen %s port:%d', [IPv6ToStr(ipv6).Text, Port]);
      TriggerDoConnectFailed;
      exit;
    end;

  FVMClient.FIP := ipv6;
  FVMClient.FPort := Port;

  FLinkVM.Connecting(p^.frameworkID, FFrameworkWithVM_ID, FVMClient.ID, ipv6, Port);
end;

procedure TCommunicationFrameworkWithP2PVM_Client.AsyncConnectC(addr: SystemString; Port: Word; OnResult: TStateCall);
var
  r: Boolean;
  ipv6: TIPV6;
  p: Pp2pVMListen;
begin
  Disconnect;

  FVMConnected := False;

  FOnAsyncConnectNotifyCall := OnResult;
  FOnAsyncConnectNotifyMethod := nil;
{$IFNDEF FPC} FOnAsyncConnectNotifyProc := nil; {$ENDIF FPC}
  if (FLinkVM = nil) or (FLinkVM.FPhysicsTunnel = nil) then
    begin
      if not FQuietMode then
          DoStatus('no VM connect');
      TriggerDoConnectFailed;
      exit;
    end;

  if not FLinkVM.WasAuthed then
    begin
      if not FQuietMode then
          DoStatus('VM no auth');
      TriggerDoConnectFailed;
      exit;
    end;

  ipv6 := StrToIPv6(addr, r);

  if not r then
    begin
      if not FQuietMode then
          DoStatus('ipv6 format error! %s', [addr]);
      TriggerDoConnectFailed;
      exit;
    end;

  p := FLinkVM.FindListen(ipv6, Port);
  if p = nil then
    begin
      if not FQuietMode then
          DoStatus('no remote listen %s port:%d', [IPv6ToStr(ipv6).Text, Port]);
      TriggerDoConnectFailed;
      exit;
    end;

  FVMClient.FIP := ipv6;
  FVMClient.FPort := Port;

  FLinkVM.Connecting(p^.frameworkID, FFrameworkWithVM_ID, FVMClient.ID, ipv6, Port);
end;

procedure TCommunicationFrameworkWithP2PVM_Client.AsyncConnectM(addr: SystemString; Port: Word; OnResult: TStateMethod);
var
  r: Boolean;
  ipv6: TIPV6;
  p: Pp2pVMListen;
begin
  Disconnect;

  FVMConnected := False;

  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := OnResult;
{$IFNDEF FPC} FOnAsyncConnectNotifyProc := nil; {$ENDIF FPC}
  if (FLinkVM = nil) or (FLinkVM.FPhysicsTunnel = nil) then
    begin
      if not FQuietMode then
          DoStatus('no VM connect');
      TriggerDoConnectFailed;
      exit;
    end;

  if not FLinkVM.WasAuthed then
    begin
      if not FQuietMode then
          DoStatus('VM no auth');
      TriggerDoConnectFailed;
      exit;
    end;

  ipv6 := StrToIPv6(addr, r);

  if not r then
    begin
      if not FQuietMode then
          DoStatus('ipv6 format error! %s', [addr]);
      TriggerDoConnectFailed;
      exit;
    end;

  p := FLinkVM.FindListen(ipv6, Port);
  if p = nil then
    begin
      if not FQuietMode then
          DoStatus('no remote listen %s port:%d', [IPv6ToStr(ipv6).Text, Port]);
      TriggerDoConnectFailed;
      exit;
    end;

  FVMClient.FIP := ipv6;
  FVMClient.FPort := Port;

  FLinkVM.Connecting(p^.frameworkID, FFrameworkWithVM_ID, FVMClient.ID, ipv6, Port);
end;

{$IFNDEF FPC}


procedure TCommunicationFrameworkWithP2PVM_Client.AsyncConnectP(addr: SystemString; Port: Word; OnResult: TStateProc);
var
  r: Boolean;
  ipv6: TIPV6;
  p: Pp2pVMListen;
begin
  Disconnect;

  FVMConnected := False;

  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := nil;
  FOnAsyncConnectNotifyProc := OnResult;

  if (FLinkVM = nil) or (FLinkVM.FPhysicsTunnel = nil) then
    begin
      if not FQuietMode then
          DoStatus('no VM connect');
      TriggerDoConnectFailed;
      exit;
    end;

  if not FLinkVM.WasAuthed then
    begin
      if not FQuietMode then
          DoStatus('VM no auth');
      TriggerDoConnectFailed;
      exit;
    end;

  ipv6 := StrToIPv6(addr, r);

  if not r then
    begin
      if not FQuietMode then
          DoStatus('ipv6 format error! %s', [addr]);
      TriggerDoConnectFailed;
      exit;
    end;

  p := FLinkVM.FindListen(ipv6, Port);
  if p = nil then
    begin
      if not FQuietMode then
          DoStatus('no remote listen %s port:%d', [IPv6ToStr(ipv6).Text, Port]);
      TriggerDoConnectFailed;
      exit;
    end;

  FVMClient.FIP := ipv6;
  FVMClient.FPort := Port;

  FLinkVM.Connecting(p^.frameworkID, FFrameworkWithVM_ID, FVMClient.ID, ipv6, Port);
end;
{$ENDIF FPC}


function TCommunicationFrameworkWithP2PVM_Client.Connect(addr: SystemString; Port: Word): Boolean;
var
  ipv6: TIPV6;
  p: Pp2pVMListen;
  t: TTimeTickValue;
begin
  Disconnect;

  Result := False;

  FVMConnected := False;
  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := nil;
{$IFNDEF FPC} FOnAsyncConnectNotifyProc := nil; {$ENDIF FPC}
  if (FLinkVM = nil) or (FLinkVM.FPhysicsTunnel = nil) then
    begin
      if not FQuietMode then
          DoStatus('no VM connect');
      exit;
    end;

  if not FLinkVM.WasAuthed then
    begin
      if not FQuietMode then
          DoStatus('VM no auth');
      exit;
    end;

  ipv6 := StrToIPv6(addr, Result);

  if not Result then
    begin
      if not FQuietMode then
          DoStatus('ipv6 format error! %s', [addr]);
      exit;
    end;

  p := FLinkVM.FindListen(ipv6, Port);
  if p = nil then
    begin
      if not FQuietMode then
          DoStatus('no remote listen %s port:%d', [IPv6ToStr(ipv6).Text, Port]);
      exit;
    end;

  FVMClient.FIP := ipv6;
  FVMClient.FPort := Port;
  FLinkVM.Connecting(p^.frameworkID, FFrameworkWithVM_ID, FVMClient.ID, ipv6, Port);

  t := GetTimeTick + 1000;
  while not FVMConnected do
    begin
      ProgressWaitSend(FVMClient);
      if GetTimeTick > t then
          Break;
    end;

  t := GetTimeTick + 2000;
  while (FVMConnected) and (not RemoteInited) do
    begin
      ProgressWaitSend(FVMClient);
      if GetTimeTick > t then
          Break;
    end;

  Result := (FVMConnected) and (RemoteInited);
end;

procedure TCommunicationFrameworkWithP2PVM_Client.Disconnect;
begin
  if Connected then
      FVMClient.Disconnect;
end;

procedure TCommunicationFrameworkWithP2PVM_Client.ProgressWaitSend(p_io: TPeerIO);
begin
  if FLinkVM <> nil then
    begin
      if FLinkVM.FPhysicsTunnel <> nil then
          FLinkVM.FPhysicsTunnel.OwnerFramework.Progress;

      FLinkVM.Progress;
    end;

  inherited ProgressWaitSend(p_io);
end;

procedure TCommunicationFrameworkWithP2PVM.Hook_SendByteBuffer(const Sender: TPeerIO; const buff: PByte; siz: NativeInt);
var
  t: Tp2pVMFragmentPackage;
begin
  if siz <= 0 then
      exit;

  if FAuthed then
    begin
      t.Init;
      t.buffSiz := siz;
      t.frameworkID := 0;
      t.p2pID := 0;
      t.pkType := c_p2pVM_PhysicsFragmentData;
      t.buff := buff;

      t.BuildSendBuff(FSendStream);
    end
  else
      FSendStream.WritePtr(buff, siz);
end;

procedure TCommunicationFrameworkWithP2PVM.Hook_SaveReceiveBuffer(const Sender: TPeerIO; const buff: Pointer; siz: Int64);
begin
  if siz <= 0 then
      exit;

  LockObject(Self);
  try
    FReceiveStream.Position := FReceiveStream.Size;
    FReceiveStream.WritePtr(buff, siz);
  finally
      UnLockObject(Self);
  end;
end;

procedure TCommunicationFrameworkWithP2PVM.SyncProcessReceiveBuff;
var
  i: Integer;
  LP: Pp2pVMListen;
  p64: Int64;
  SourStream: TMemoryStream64;
  fPk: Tp2pVMFragmentPackage;
  rPos: Integer;
begin
  if FReceiveStream.Size <= 0 then
      exit;
  // p2p auth
  if not FAuthed then
    begin
      if (FAuthWaiting) and (FReceiveStream.Size >= length(FPhysicsTunnel.FP2PAuthToken)) and
        (CompareMemory(@FPhysicsTunnel.FP2PAuthToken[0], FReceiveStream.Memory, length(FPhysicsTunnel.FP2PAuthToken))) then
        begin
          FSendStream.Clear;

          if not FAuthSending then
              AuthVM;

          FAuthWaiting := False;
          FAuthed := True;
          FAuthSending := False;

          // sync listen state
          for i := 0 to FFrameworkListenPool.Count - 1 do
            begin
              LP := FFrameworkListenPool[i];
              ListenState(LP^.frameworkID, LP^.ListenHost, LP^.ListenPort, LP^.Listening);
            end;

          // send auth successed token
          AuthSuccessed;

          // fill fragment buffer
          p64 := length(FPhysicsTunnel.FP2PAuthToken);
          SourStream := TMemoryStream64.Create;
          FReceiveStream.Position := p64;
          if FReceiveStream.Size - FReceiveStream.Position > 0 then
              SourStream.CopyFrom(FReceiveStream, FReceiveStream.Size - FReceiveStream.Position);
          DisposeObject(FReceiveStream);
          FReceiveStream := SourStream;

          if not FQuietMode then
              DoStatus('VM connect Auth Success');
        end
      else if FAuthWaiting then
          exit
      else
        begin
          // safe process fragment
          if FReceiveStream.Size >= length(FPhysicsTunnel.FP2PAuthToken) then
            begin
              FPhysicsTunnel.FOwnerFramework.InternalSaveReceiveBuffer(FPhysicsTunnel, FReceiveStream.Memory, FReceiveStream.Size);
              FReceiveStream.Clear;
              FPhysicsTunnel.FOwnerFramework.InternalProcessReceiveBuffer(FPhysicsTunnel, nil, False, False);
            end;
          exit;
        end;
    end;

  if FReceiveStream.Size < 13 then
      exit;

  LockObject(Self);

  try
    SourStream := TMemoryStream64.Create;
    p64 := 0;
    SourStream.SetPointerWithProtectedMode(FReceiveStream.PositionAsPtr(p64), FReceiveStream.Size - p64);

    while SourStream.Size > 0 do
      begin
        fPk.Init;
        rPos := fPk.FillReceiveBuff(SourStream);
        if rPos > 0 then
          begin
            // protocol support
            case fPk.pkType of
              c_p2pVM_echoing: ReceivedEchoing(fPk.frameworkID, fPk.p2pID, fPk.buff, fPk.buffSiz);
              c_p2pVM_echo: ReceivedEcho(fPk.frameworkID, fPk.p2pID, fPk.buff, fPk.buffSiz);
              c_p2pVM_AuthSuccessed:
                begin
                  if Assigned(OnAuthSuccessOnesNotify) then
                    begin
                      try
                          OnAuthSuccessOnesNotify(Self);
                      except
                      end;
                      OnAuthSuccessOnesNotify := nil;
                    end;
                end;
              c_p2pVM_Listen: ReceivedListen(fPk.frameworkID, fPk.p2pID, fPk.buff, fPk.buffSiz);
              c_p2pVM_ListenState: ReceivedListenState(fPk.frameworkID, fPk.p2pID, fPk.buff, fPk.buffSiz);
              c_p2pVM_Connecting: ReceivedConnecting(fPk.frameworkID, fPk.p2pID, fPk.buff, fPk.buffSiz);
              c_p2pVM_ConnectedReponse: ReceivedConnectedReponse(fPk.frameworkID, fPk.p2pID, fPk.buff, fPk.buffSiz);
              c_p2pVM_Disconnect: ReceivedDisconnect(fPk.frameworkID, fPk.p2pID, fPk.buff, fPk.buffSiz);
              c_p2pVM_LogicFragmentData: ReceivedLogicFragmentData(fPk.frameworkID, fPk.p2pID, fPk.buff, fPk.buffSiz);
              c_p2pVM_PhysicsFragmentData: ReceivedOriginFragmentData(fPk.frameworkID, fPk.p2pID, fPk.buff, fPk.buffSiz);
              else if not FQuietMode then
                  begin
                    DoStatus('VM protocol header errror');
                    DoStatus(@fPk, SizeOf(fPk), 40);
                  end;
            end;
            // fill buffer
            inc(p64, rPos);
            if FReceiveStream.Size - p64 >= 13 then
              begin
                SourStream.SetPointerWithProtectedMode(FReceiveStream.PositionAsPtr(p64), FReceiveStream.Size - p64);
              end
            else
                Break;
          end
        else
            Break;
      end;

    DisposeObject(SourStream);

    if p64 > 0 then
      begin
        SourStream := TMemoryStream64.Create;
        FReceiveStream.Position := p64;
        if FReceiveStream.Size - FReceiveStream.Position > 0 then
            SourStream.CopyFrom(FReceiveStream, FReceiveStream.Size - FReceiveStream.Position);
        DisposeObject(FReceiveStream);
        FReceiveStream := SourStream;
      end;
  finally
      UnLockObject(Self);
  end;
end;

procedure TCommunicationFrameworkWithP2PVM.Hook_ProcessReceiveBuffer(const Sender: TPeerIO; const ACurrentActiveThread: TCoreClassThread; const RecvSync, SendSync: Boolean);
begin
  Sender.IO_SyncMethod(ACurrentActiveThread, RecvSync, {$IFDEF FPC}@{$ENDIF FPC}SyncProcessReceiveBuff);
end;

procedure TCommunicationFrameworkWithP2PVM.Hook_ClientDestroy(const Sender: TPeerIO);
begin
  CloseP2PVMTunnel;
  Sender.FOwnerFramework.InternalClientDestroy(Sender);
end;

procedure TCommunicationFrameworkWithP2PVM.SendVMBuffer(const buff: Pointer; const siz: NativeInt);
begin
  FPhysicsTunnel.WriteBufferOpen;
  FPhysicsTunnel.OwnerFramework.InternalSendByteBuffer(FPhysicsTunnel, buff, siz);
  FPhysicsTunnel.WriteBufferFlush;
  FPhysicsTunnel.WriteBufferClose;
end;

procedure TCommunicationFrameworkWithP2PVM.DisconnectWithVM(c: TPeerIO);
begin
end;

procedure TCommunicationFrameworkWithP2PVM.ReceivedEchoing(const frameworkID, p2pID: Cardinal; const buff: PByte; const siz: Cardinal);
begin
  echoBuffer(buff, siz);
end;

procedure TCommunicationFrameworkWithP2PVM.ReceivedEcho(const frameworkID, p2pID: Cardinal; const buff: PByte; const siz: Cardinal);
type
  TBuf = array [0 .. 7] of Byte;
  PBuf = ^TBuf;
var
  p: PBuf;
  u64ptr: UInt64;
  echoPtr: POnEcho;
  i: Integer;
begin
  if siz <> SizeOf(TBuf) then
    begin
      if not FQuietMode then
          DoStatus('echoing protocol with buffer error!');
      if buff <> nil then
        if not FQuietMode then
            DoStatus(buff, siz, 40);
      exit;
    end;
  p := @buff^;
  u64ptr := PUInt64(@p^[0])^;
  echoPtr := Pointer(u64ptr);
  if echoPtr = nil then
      exit;

  i := 0;
  while i < FWaitEchoList.Count do
    begin
      if FWaitEchoList[i] = echoPtr then
        begin
          FWaitEchoList.Delete(i);
          try
            if Assigned(echoPtr^.OnEchoCall) then
                echoPtr^.OnEchoCall(True);
            if Assigned(echoPtr^.OnEchoMethod) then
                echoPtr^.OnEchoMethod(True);
{$IFNDEF FPC}
            if Assigned(echoPtr^.OnEchoProc) then
                echoPtr^.OnEchoProc(True);
{$ENDIF FPC}
          except
          end;

          try
              Dispose(echoPtr);
          except
          end;
        end
      else
          inc(i);
    end;
end;

procedure TCommunicationFrameworkWithP2PVM.ReceivedListen(const frameworkID, p2pID: Cardinal; const buff: PByte; const siz: Cardinal);
type
  TBuf = array [0 .. 18] of Byte;
  PBuf = ^TBuf;
var
  p: PBuf;
  ipv6: TIPV6;
  Port: Word;
  Listening: Boolean;
  LP: Pp2pVMListen;
begin
  if siz <> SizeOf(TBuf) then
    begin
      if not FQuietMode then
          DoStatus('listen protocol with buffer error!');
      if buff <> nil then
        if not FQuietMode then
            DoStatus(buff, siz, 40);
      exit;
    end;
  p := @buff^;
  ipv6 := PIPV6(@p^[0])^;
  Port := PWORD(@p^[16])^;
  Listening := PBoolean(@p^[18])^;

  if p2pID <> 0 then
    begin
      if not FQuietMode then
          DoStatus('listen protocol error! PeerClient ID:%d', [p2pID]);
      exit;
    end;

  LP := FindListen(ipv6, Port);
  if Listening then
    begin
      if LP = nil then
        begin
          new(LP);
          LP^.frameworkID := frameworkID;
          LP^.ListenHost := ipv6;
          LP^.ListenPort := Port;
          LP^.Listening := True;
          FFrameworkListenPool.Add(LP);
          ListenState(frameworkID, ipv6, Port, True);
        end
      else
        begin
          LP^.Listening := True;
          ListenState(frameworkID, ipv6, Port, True);
        end;
    end
  else
    begin
      DeleteListen(ipv6, Port);
      ListenState(frameworkID, ipv6, Port, False);
    end;
end;

procedure TCommunicationFrameworkWithP2PVM.ReceivedListenState(const frameworkID, p2pID: Cardinal; const buff: PByte; const siz: Cardinal);
type
  TBuf = array [0 .. 18] of Byte;
  PBuf = ^TBuf;
var
  c: TCommunicationFramework;
  p: PBuf;
  ipv6: TIPV6;
  Port: Word;
  Listening: Boolean;
  LP: Pp2pVMListen;
begin
  if siz <> SizeOf(TBuf) then
    begin
      if not FQuietMode then
          DoStatus('Virtual listen state protocol with buffer error!');
      if buff <> nil then
        if not FQuietMode then
            DoStatus(buff, siz, 40);
      exit;
    end;
  p := @buff^;
  ipv6 := PIPV6(@p^[0])^;
  Port := PWORD(@p^[16])^;
  Listening := PBoolean(@p^[18])^;

  if p2pID <> 0 then
    begin
      if not FQuietMode then
          DoStatus('Virtual listen state protocol error! PeerClient ID:%d', [p2pID]);
      exit;
    end;

  LP := FindListen(ipv6, Port);
  if Listening then
    begin
      if LP = nil then
        begin
          new(LP);
          LP^.frameworkID := frameworkID;
          LP^.ListenHost := ipv6;
          LP^.ListenPort := Port;
          LP^.Listening := True;
          FFrameworkListenPool.Add(LP);
        end
      else
        begin
          LP^.Listening := True;
        end;
      if not FQuietMode then
          DoStatus('Virtual Remote Listen state Activted "%s port:%d"', [IPv6ToStr(ipv6).Text, Port]);
    end
  else
    begin
      DeleteListen(ipv6, Port);
      if not FQuietMode then
          DoStatus('Virtual Remote Listen state Close "%s port:%d"', [IPv6ToStr(ipv6).Text, Port]);
    end;

  c := TCommunicationFramework(FFrameworkPool[frameworkID]);
  if c is TCommunicationFrameworkWithP2PVM_Server then
    begin
      TCommunicationFrameworkWithP2PVM_Server(c).ListenState(Self, ipv6, Port, Listening);
      ListenState(TCommunicationFrameworkWithP2PVM_Server(c).FFrameworkWithVM_ID, ipv6, Port, Listening);
    end;
end;

procedure TCommunicationFrameworkWithP2PVM.ReceivedConnecting(const frameworkID, p2pID: Cardinal; const buff: PByte; const siz: Cardinal);
type
  TBuf = array [0 .. 25] of Byte;
  PBuf = ^TBuf;
var
  c: TCommunicationFramework;
  p: PBuf;
  Remote_frameworkID: Cardinal;
  Remote_p2pID: Cardinal;
  ipv6: TIPV6;
  Port: Word;
  Allowed: Boolean;
begin
  if siz <> SizeOf(TBuf) then
    begin
      if not FQuietMode then
        begin
          DoStatus('connect request with buffer error!');
          if buff <> nil then
              DoStatus(buff, siz, 40);
        end;
      exit;
    end;
  p := @buff^;
  Remote_frameworkID := PCardinal(@p^[0])^;
  Remote_p2pID := PCardinal(@p^[4])^;
  ipv6 := PIPV6(@p^[8])^;
  Port := PWORD(@p^[24])^;

  if p2pID <> 0 then
    begin
      Disconnect(Remote_frameworkID, Remote_p2pID);
      if not FQuietMode then
          DoStatus('connect request with protocol error! PeerClient ID:%d', [p2pID]);
      exit;
    end;

  c := TCommunicationFramework(FFrameworkPool[frameworkID]);
  if c is TCommunicationFrameworkWithP2PVM_Server then
    begin
      Allowed := True;
      TCommunicationFrameworkWithP2PVM_Server(c).Connecting(Self, Remote_frameworkID, frameworkID, ipv6, Port, Allowed);

      if not Allowed then
        begin
          Disconnect(Remote_frameworkID, 0);
          exit;
        end;
    end
  else
    begin
      Disconnect(Remote_frameworkID, Remote_p2pID);
    end;
end;

procedure TCommunicationFrameworkWithP2PVM.ReceivedConnectedReponse(const frameworkID, p2pID: Cardinal; const buff: PByte; const siz: Cardinal);
type
  TBuf = array [0 .. 7] of Byte;
  PBuf = ^TBuf;
var
  c: TCommunicationFramework;
  p: PBuf;
  Remote_frameworkID: Cardinal;
  Remote_p2pID: Cardinal;
begin
  if siz <> SizeOf(TBuf) then
    begin
      if not FQuietMode then
        begin
          DoStatus('connect request with buffer error!');
          if buff <> nil then
              DoStatus(buff, siz, 40);
        end;
      exit;
    end;

  c := TCommunicationFramework(FFrameworkPool[frameworkID]);
  if c is TCommunicationFrameworkWithP2PVM_Client then
    begin
      p := @buff^;
      Remote_frameworkID := PCardinal(@p^[0])^;
      Remote_p2pID := PCardinal(@p^[4])^;

      // trigger connect reponse
      TCommunicationFrameworkWithP2PVM_Client(c).VMConnectSuccessed(Self, Remote_frameworkID, Remote_p2pID, frameworkID);

      if not FQuietMode then
          DoStatus('connect reponse from frameworkID[%d] p2pID[%d]', [Remote_frameworkID, Remote_p2pID]);
    end;
end;

procedure TCommunicationFrameworkWithP2PVM.ReceivedDisconnect(const frameworkID, p2pID: Cardinal; const buff: PByte; const siz: Cardinal);
var
  c: TCommunicationFramework;
  LocalVMc: TPeerClientWithP2PVM;
begin
  c := TCommunicationFramework(FFrameworkPool[frameworkID]);
  if c is TCommunicationFrameworkWithP2PVM_Client then
    begin
      TCommunicationFrameworkWithP2PVM_Client(c).FVMClient.FDestroyTimeNotify := False;
      TCommunicationFrameworkWithP2PVM_Client(c).VMDisconnect(Self);
    end
  else if c is TCommunicationFrameworkWithP2PVM_Server then
    begin
      LocalVMc := TPeerClientWithP2PVM(c.FPeerIO_HashPool[p2pID]);
      if LocalVMc = nil then
        begin
          if not FQuietMode then
              DoStatus('disconnect with protocol error! PeerClient ID:%d', [p2pID]);
          exit;
        end;
      LocalVMc.FDestroyTimeNotify := False;
      LocalVMc.Disconnect;
    end
  else
    begin
      if not FQuietMode then
          DoStatus('disconnect with protocol error! frameworkID:%d', [frameworkID]);
    end;
end;

procedure TCommunicationFrameworkWithP2PVM.ReceivedLogicFragmentData(const frameworkID, p2pID: Cardinal; const buff: PByte; const siz: Cardinal);
var
  c: TCommunicationFramework;
  LocalVMc: TPeerIO;
begin
  c := TCommunicationFramework(FFrameworkPool[frameworkID]);
  if c is TCommunicationFrameworkWithP2PVM_Server then
    begin
      LocalVMc := TPeerIO(c.FPeerIO_HashPool[p2pID]);
      if LocalVMc <> nil then
        begin
          LocalVMc.SaveReceiveBuffer(buff, siz);
          LocalVMc.FillRecvBuffer(nil, False, False);
        end
      else if not FQuietMode then
        begin
          DoStatus('fragment Data p2pID error: p2pID:%d buffer size:%d', [p2pID, siz]);
          DoStatus(buff, umlMin(siz, 164), 40);
        end;
    end
  else if c is TCommunicationFrameworkWithP2PVM_Client then
    begin
      LocalVMc := TCommunicationFrameworkWithP2PVM_Client(c).FVMClient;
      LocalVMc.SaveReceiveBuffer(buff, siz);
      LocalVMc.FillRecvBuffer(nil, False, False);
    end
  else if not FQuietMode then
    begin
      DoStatus('fragment Data frameworkID error: frameworkID:%d buffer size:%d', [frameworkID, siz]);
      DoStatus(buff, umlMin(siz, 164), 40);
    end;
end;

procedure TCommunicationFrameworkWithP2PVM.ReceivedOriginFragmentData(const frameworkID, p2pID: Cardinal; const buff: PByte; const siz: Cardinal);
begin
  if FPhysicsTunnel = nil then
      exit;
  FPhysicsTunnel.FOwnerFramework.InternalSaveReceiveBuffer(FPhysicsTunnel, buff, siz);
  FPhysicsTunnel.FOwnerFramework.InternalProcessReceiveBuffer(FPhysicsTunnel, nil, False, False);
end;

procedure TCommunicationFrameworkWithP2PVM.DoProcessPerClientFragmentSend(PeerClient: TPeerIO);
var
  p: Pp2pVMFragmentPackage;
begin
  if TPeerClientWithP2PVM(PeerClient).FLinkVM <> Self then
      exit;

  if TPeerClientWithP2PVM(PeerClient).FSendQueue.Count > 0 then
    begin
      p := TPeerClientWithP2PVM(PeerClient).FSendQueue[0];
      TPeerClientWithP2PVM(PeerClient).FSendQueue.Delete(0);
      p^.BuildSendBuff(FSendStream);
      FreeP2PVMPackage(p);
    end;
end;

procedure TCommunicationFrameworkWithP2PVM.DoPerClientClose(PeerClient: TPeerIO);
begin
  if TPeerClientWithP2PVM(PeerClient).FLinkVM = Self then
      PeerClient.Disconnect;
end;

constructor TCommunicationFrameworkWithP2PVM.Create(HashPoolLen: Integer);
begin
  inherited Create;
  FPhysicsTunnel := nil;

  FAuthWaiting := False;
  FAuthed := False;
  FAuthSending := False;

  FFrameworkPool := TUInt32HashObjectList.Create(HashPoolLen);
  FFrameworkPool.AutoFreeData := False;
  FFrameworkPool.AccessOptimization := False;

  FFrameworkListenPool := TCoreClassList.Create;

  FMaxVMFragmentSize := 200;
  FMaxRealBuffer := 2048 * 1024; // 2M

  FQuietMode := False;

  FReceiveStream := TMemoryStream64.Create;
  FSendStream := TMemoryStream64.Create;

  FWaitEchoList := TCoreClassList.Create;

  FVMID := 0;
  OnAuthSuccessOnesNotify := nil;
end;

destructor TCommunicationFrameworkWithP2PVM.Destroy;
var
  i: Integer;
  OnEchoPtr: POnEcho;
begin
  for i := 0 to FWaitEchoList.Count - 1 do
    begin
      OnEchoPtr := FWaitEchoList[i];
      Dispose(OnEchoPtr);
    end;
  FWaitEchoList.Clear;

  if FPhysicsTunnel <> nil then
      CloseP2PVMTunnel;

  ClearListen;

  DisposeObject(FWaitEchoList);
  DisposeObject(FReceiveStream);
  DisposeObject(FSendStream);
  DisposeObject(FFrameworkPool);
  DisposeObject(FFrameworkListenPool);
  inherited Destroy;
end;

procedure TCommunicationFrameworkWithP2PVM.Progress;
var
  i: Integer;
  p: PUInt32HashListObjectStruct;
  lsiz: Int64;
  OnEchoPtr: POnEcho;
begin
  if FPhysicsTunnel = nil then
      exit;

  // echo and keepalive simulate
  i := 0;
  while i < FWaitEchoList.Count do
    begin
      OnEchoPtr := FWaitEchoList[i];
      if OnEchoPtr^.Timeout < GetTimeTick then
        begin
          FWaitEchoList.Delete(i);

          try
            if Assigned(OnEchoPtr^.OnEchoCall) then
                OnEchoPtr^.OnEchoCall(False);
            if Assigned(OnEchoPtr^.OnEchoMethod) then
                OnEchoPtr^.OnEchoMethod(False);
{$IFNDEF FPC}
            if Assigned(OnEchoPtr^.OnEchoProc) then
                OnEchoPtr^.OnEchoProc(False);
{$ENDIF FPC}
          except
          end;

          try
              Dispose(OnEchoPtr);
          except
          end;
        end
      else
          inc(i);
    end;

  // real send buffer
  try
    if FSendStream.Size > 0 then
      begin
        SendVMBuffer(FSendStream.Memory, FSendStream.Size);
        FSendStream.Clear;
      end;
  except
  end;

  if not FAuthed then
      exit;

  // fragment package
  repeat
    lsiz := FSendStream.Size;
    if (FFrameworkPool.Count > 0) then
      begin
        i := 0;
        p := FFrameworkPool.FirstPtr;
        while i < FFrameworkPool.Count do
          begin
            TCommunicationFramework(p^.Data).FastProgressPerClientM({$IFDEF FPC}@{$ENDIF FPC}DoProcessPerClientFragmentSend);
            inc(i);
            p := p^.Next;
          end;
      end;
  until (FSendStream.Size = lsiz) or (FSendStream.Size > FMaxRealBuffer);

  if FSendStream.Size > 0 then
    begin
      SendVMBuffer(FSendStream.Memory, FSendStream.Size);
      FSendStream.Clear;
    end
end;

procedure TCommunicationFrameworkWithP2PVM.ProgressCommunicationFrameworkC(OnBackcall: TCommunicationFrameworkListCall);
var
  i: Integer;
  p: PUInt32HashListObjectStruct;
begin
  if (FFrameworkPool.Count > 0) and (Assigned(OnBackcall)) then
    begin
      i := 0;
      p := FFrameworkPool.FirstPtr;
      while i < FFrameworkPool.Count do
        begin
          try
              OnBackcall(TCommunicationFramework(p^.Data));
          except
          end;
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure TCommunicationFrameworkWithP2PVM.ProgressCommunicationFrameworkM(OnBackcall: TCommunicationFrameworkListMethod);
var
  i: Integer;
  p: PUInt32HashListObjectStruct;
begin
  if (FFrameworkPool.Count > 0) and (Assigned(OnBackcall)) then
    begin
      i := 0;
      p := FFrameworkPool.FirstPtr;
      while i < FFrameworkPool.Count do
        begin
          try
              OnBackcall(TCommunicationFramework(p^.Data));
          except
          end;
          inc(i);
          p := p^.Next;
        end;
    end;
end;

{$IFNDEF FPC}


procedure TCommunicationFrameworkWithP2PVM.ProgressCommunicationFrameworkP(OnBackcall: TCommunicationFrameworkListProc);
var
  i: Integer;
  p: PUInt32HashListObjectStruct;
begin
  if (FFrameworkPool.Count > 0) and (Assigned(OnBackcall)) then
    begin
      i := 0;
      p := FFrameworkPool.FirstPtr;
      while i < FFrameworkPool.Count do
        begin
          try
              OnBackcall(TCommunicationFramework(p^.Data));
          except
          end;
          inc(i);
          p := p^.Next;
        end;
    end;
end;

{$ENDIF FPC}


procedure TCommunicationFrameworkWithP2PVM.OpenP2PVMTunnel(c: TPeerIO);
begin
  FPhysicsTunnel := c;
  FAuthWaiting := False;
  FAuthed := False;
  FAuthSending := False;
  FReceiveStream.Clear;
  FSendStream.Clear;

  try
    FPhysicsTunnel.OnInternalSendByteBuffer := {$IFDEF FPC}@{$ENDIF FPC}Hook_SendByteBuffer;
    FPhysicsTunnel.OnInternalSaveReceiveBuffer := {$IFDEF FPC}@{$ENDIF FPC}Hook_SaveReceiveBuffer;
    FPhysicsTunnel.OnInternalProcessReceiveBuffer := {$IFDEF FPC}@{$ENDIF FPC}Hook_ProcessReceiveBuffer;
    FPhysicsTunnel.OnDestroy := {$IFDEF FPC}@{$ENDIF FPC}Hook_ClientDestroy;
  except
  end;

  if not FQuietMode then
      DoStatus('Open VM P2P Tunnel ' + FPhysicsTunnel.PeerIP);
end;

procedure TCommunicationFrameworkWithP2PVM.CloseP2PVMTunnel;
var
  i: Integer;
  OnEchoPtr: POnEcho;
  p: PUInt32HashListObjectStruct;
begin
  for i := 0 to FWaitEchoList.Count - 1 do
    begin
      OnEchoPtr := FWaitEchoList[i];
      Dispose(OnEchoPtr);
    end;
  FWaitEchoList.Clear;

  OnAuthSuccessOnesNotify := nil;

  if (FFrameworkPool.Count > 0) then
    begin
      i := 0;
      p := FFrameworkPool.FirstPtr;
      while i < FFrameworkPool.Count do
        begin
          if p^.Data is TCommunicationFrameworkWithP2PVM_Server then
            begin
              TCommunicationFramework(p^.Data).ProgressPerClientM({$IFDEF FPC}@{$ENDIF FPC}DoPerClientClose);
              TCommunicationFrameworkWithP2PVM_Server(p^.Data).FLinkVMPool.Delete(FVMID);
            end;
          inc(i);
          p := p^.Next;
        end;
    end;

  CloseAllClientIO;

  FAuthWaiting := False;
  FAuthed := False;
  FAuthSending := False;
  FReceiveStream.Clear;
  FSendStream.Clear;

  if FPhysicsTunnel = nil then
      exit;

  try
    FPhysicsTunnel.OnInternalSendByteBuffer := {$IFDEF FPC}@{$ENDIF FPC}FPhysicsTunnel.FOwnerFramework.InternalSendByteBuffer;
    FPhysicsTunnel.OnInternalSaveReceiveBuffer := {$IFDEF FPC}@{$ENDIF FPC}FPhysicsTunnel.FOwnerFramework.InternalSaveReceiveBuffer;
    FPhysicsTunnel.OnInternalProcessReceiveBuffer := {$IFDEF FPC}@{$ENDIF FPC}FPhysicsTunnel.FOwnerFramework.InternalProcessReceiveBuffer;
    FPhysicsTunnel.OnDestroy := {$IFDEF FPC}@{$ENDIF FPC}FPhysicsTunnel.FOwnerFramework.InternalClientDestroy;
  except
  end;

  if not FQuietMode then
      DoStatus('Close VM P2P Tunnel ' + FPhysicsTunnel.PeerIP);

  FPhysicsTunnel := nil;
end;

procedure TCommunicationFrameworkWithP2PVM.InstallLogicFramework(c: TCommunicationFramework);
var
  i: Integer;
  LP: Pp2pVMListen;
begin
  if c is TCommunicationFrameworkWithP2PVM_Server then
    begin
      if TCommunicationFrameworkWithP2PVM_Server(c).FFrameworkWithVM_ID <> 0 then
        begin
          if FFrameworkPool.Exists(TCommunicationFrameworkWithP2PVM_Server(c).FFrameworkWithVM_ID) then
              RaiseInfo('P2PVM server is installed');
        end
      else
        begin
          if FFrameworkPool.Count > 0 then
              TCommunicationFrameworkWithP2PVM_Server(c).FFrameworkWithVM_ID := FFrameworkPool.LastPtr^.u32
          else
              TCommunicationFrameworkWithP2PVM_Server(c).FFrameworkWithVM_ID := 1;
          while FFrameworkPool.Exists(TCommunicationFrameworkWithP2PVM_Server(c).FFrameworkWithVM_ID) do
              inc(TCommunicationFrameworkWithP2PVM_Server(c).FFrameworkWithVM_ID);
        end;

      TCommunicationFrameworkWithP2PVM_Server(c).FLinkVMPool.Add(FVMID, Self, True);

      FFrameworkPool.Add(TCommunicationFrameworkWithP2PVM_Server(c).FFrameworkWithVM_ID, c, True);

      for i := 0 to TCommunicationFrameworkWithP2PVM_Server(c).ListenCount - 1 do
        begin
          LP := TCommunicationFrameworkWithP2PVM_Server(c).GetListen(i);
          Listen(TCommunicationFrameworkWithP2PVM_Server(c).FFrameworkWithVM_ID, LP^.ListenHost, LP^.ListenPort, LP^.Listening);
        end;
    end
  else if c is TCommunicationFrameworkWithP2PVM_Client then
    begin
      if TCommunicationFrameworkWithP2PVM_Client(c).FFrameworkWithVM_ID <> 0 then
        begin
          if FFrameworkPool.Exists(TCommunicationFrameworkWithP2PVM_Client(c).FFrameworkWithVM_ID) then
              RaiseInfo('P2PVM client is installed');
        end
      else
        begin
          if FFrameworkPool.Count > 0 then
              TCommunicationFrameworkWithP2PVM_Client(c).FFrameworkWithVM_ID := FFrameworkPool.LastPtr^.u32
          else
              TCommunicationFrameworkWithP2PVM_Client(c).FFrameworkWithVM_ID := 1;
          while FFrameworkPool.Exists(TCommunicationFrameworkWithP2PVM_Client(c).FFrameworkWithVM_ID) do
              inc(TCommunicationFrameworkWithP2PVM_Client(c).FFrameworkWithVM_ID);
        end;

      TCommunicationFrameworkWithP2PVM_Client(c).FLinkVM := Self;
      TCommunicationFrameworkWithP2PVM_Client(c).FVMClient := TPeerClientWithP2PVM.Create(TCommunicationFrameworkWithP2PVM_Client(c), nil);
      TCommunicationFrameworkWithP2PVM_Client(c).FVMClient.FLinkVM := Self;

      FFrameworkPool.Add(TCommunicationFrameworkWithP2PVM_Client(c).FFrameworkWithVM_ID, c, True);
    end;
end;

procedure TCommunicationFrameworkWithP2PVM.UninstallLogicFramework(c: TCommunicationFramework);
var
  i: Integer;
  LP: Pp2pVMListen;
begin
  if c is TCommunicationFrameworkWithP2PVM_Server then
    begin
      TCommunicationFrameworkWithP2PVM_Server(c).FLinkVMPool.Delete(FVMID);
      FFrameworkPool.Delete(TCommunicationFrameworkWithP2PVM_Server(c).FFrameworkWithVM_ID);

      i := 0;
      while i < FFrameworkListenPool.Count do
        begin
          LP := FFrameworkListenPool[i];
          if LP^.frameworkID = TCommunicationFrameworkWithP2PVM_Server(c).FFrameworkWithVM_ID then
            begin
              Dispose(LP);
              FFrameworkListenPool.Delete(i);
            end
          else
              inc(i);
        end;
    end
  else if c is TCommunicationFrameworkWithP2PVM_Client then
    begin
      TCommunicationFrameworkWithP2PVM_Client(c).FLinkVM := nil;
      FFrameworkPool.Delete(TCommunicationFrameworkWithP2PVM_Client(c).FFrameworkWithVM_ID);
    end;
end;

function TCommunicationFrameworkWithP2PVM.CreateLogicClient: TCommunicationFrameworkWithP2PVM_Client;
var
  frameworkID: Cardinal;
begin
  if FFrameworkPool.Count > 0 then
      frameworkID := FFrameworkPool.LastPtr^.u32 + 1
  else
      frameworkID := 1;
  while FFrameworkPool.Exists(frameworkID) do
      inc(frameworkID);
  Result := TCommunicationFrameworkWithP2PVM_Client.Create(frameworkID);
  InstallLogicFramework(Result);
end;

procedure TCommunicationFrameworkWithP2PVM.AuthWaiting;
begin
  if FPhysicsTunnel = nil then
      exit;
  FAuthWaiting := True;
end;

procedure TCommunicationFrameworkWithP2PVM.AuthVM;
begin
  if FPhysicsTunnel = nil then
      exit;
  if not FAuthed then
    if not FAuthSending then
      begin
        FSendStream.WritePtr(@FPhysicsTunnel.FP2PAuthToken[0], length(FPhysicsTunnel.FP2PAuthToken));
        FAuthSending := True;
        FAuthWaiting := True;
      end;
end;

procedure TCommunicationFrameworkWithP2PVM.AuthSuccessed;
var
  p: Pp2pVMFragmentPackage;
begin
  p := BuildP2PVMPackage(0, 0, 0, c_p2pVM_AuthSuccessed, nil);

  FSendStream.Position := FSendStream.Size;
  p^.BuildSendBuff(FSendStream);
  FreeP2PVMPackage(p);
end;

procedure TCommunicationFrameworkWithP2PVM.echoing(const OnEchoPtr: POnEcho; Timeout: TTimeTickValue);
var
  u64ptr: UInt64;
  p: Pp2pVMFragmentPackage;
  i: Integer;
begin
  if (FPhysicsTunnel = nil) or (not WasAuthed) then
    begin
      if OnEchoPtr <> nil then
        begin
          i := 0;
          while i < FWaitEchoList.Count do
            begin
              if FWaitEchoList[i] = OnEchoPtr then
                  FWaitEchoList.Delete(i)
              else
                  inc(i);
            end;

          try
            if Assigned(OnEchoPtr^.OnEchoCall) then
                OnEchoPtr^.OnEchoCall(False);
            if Assigned(OnEchoPtr^.OnEchoMethod) then
                OnEchoPtr^.OnEchoMethod(False);
{$IFNDEF FPC}
            if Assigned(OnEchoPtr^.OnEchoProc) then
                OnEchoPtr^.OnEchoProc(False);
{$ENDIF FPC}
          except
          end;

          Dispose(OnEchoPtr);
        end;
      exit;
    end;

  u64ptr := UInt64(OnEchoPtr);
  p := BuildP2PVMPackage(8, 0, 0, c_p2pVM_echoing, @u64ptr);

  FSendStream.Position := FSendStream.Size;
  p^.BuildSendBuff(FSendStream);
  FreeP2PVMPackage(p);

  FWaitEchoList.Add(OnEchoPtr);
end;

procedure TCommunicationFrameworkWithP2PVM.echoingC(OnResult: TStateCall; Timeout: TTimeTickValue);
var
  p: POnEcho;
begin
  new(p);
  p^.OnEchoCall := OnResult;
  p^.OnEchoMethod := nil;
{$IFNDEF FPC} p^.OnEchoProc := nil; {$ENDIF FPC}
  p^.Timeout := GetTimeTick + Timeout;
  echoing(p, Timeout);
end;

procedure TCommunicationFrameworkWithP2PVM.echoingM(OnResult: TStateMethod; Timeout: TTimeTickValue);
var
  p: POnEcho;
begin
  new(p);
  p^.OnEchoCall := nil;
  p^.OnEchoMethod := OnResult;
{$IFNDEF FPC} p^.OnEchoProc := nil; {$ENDIF FPC}
  p^.Timeout := GetTimeTick + Timeout;
  echoing(p, Timeout);
end;

{$IFNDEF FPC}


procedure TCommunicationFrameworkWithP2PVM.echoingP(OnResult: TStateProc; Timeout: TTimeTickValue);
var
  p: POnEcho;
begin
  new(p);
  p^.OnEchoCall := nil;
  p^.OnEchoMethod := nil;
  p^.OnEchoProc := OnResult;
  p^.Timeout := GetTimeTick + Timeout;
  echoing(p, Timeout);
end;
{$ENDIF FPC}


procedure TCommunicationFrameworkWithP2PVM.echoBuffer(const buff: Pointer; const siz: NativeInt);
var
  p: Pp2pVMFragmentPackage;
begin
  if (FPhysicsTunnel = nil) or (not WasAuthed) then
      exit;
  p := BuildP2PVMPackage(siz, 0, 0, c_p2pVM_echo, buff);

  FSendStream.Position := FSendStream.Size;
  p^.BuildSendBuff(FSendStream);
  FreeP2PVMPackage(p);
end;

procedure TCommunicationFrameworkWithP2PVM.Listen(const frameworkID: Cardinal; const ipv6: TIPV6; const Port: Word; const Listening: Boolean);
var
  LP: Pp2pVMListen;
  c: TCommunicationFramework;
  RBuf: array [0 .. 18] of Byte;
  p: Pp2pVMFragmentPackage;
begin
  if (FPhysicsTunnel = nil) or (not WasAuthed) then
    begin
      LP := FindListen(ipv6, Port);
      if Listening then
        begin
          if LP = nil then
            begin
              new(LP);
              LP^.frameworkID := frameworkID;
              LP^.ListenHost := ipv6;
              LP^.ListenPort := Port;
              LP^.Listening := True;
              FFrameworkListenPool.Add(LP);
            end
          else
              LP^.Listening := True;
        end
      else
          DeleteListen(ipv6, Port);

      c := TCommunicationFramework(FFrameworkPool[frameworkID]);
      if c is TCommunicationFrameworkWithP2PVM_Server then
        begin
          TCommunicationFrameworkWithP2PVM_Server(c).ListenState(Self, ipv6, Port, Listening);
          ListenState(TCommunicationFrameworkWithP2PVM_Server(c).FFrameworkWithVM_ID, ipv6, Port, Listening);
        end;
    end
  else
    begin
      PIPV6(@RBuf[0])^ := ipv6;
      PWORD(@RBuf[16])^ := Port;
      PBoolean(@RBuf[18])^ := Listening;
      p := BuildP2PVMPackage(SizeOf(RBuf), frameworkID, 0, c_p2pVM_Listen, @RBuf[0]);

      FSendStream.Position := FSendStream.Size;
      p^.BuildSendBuff(FSendStream);
      FreeP2PVMPackage(p);
    end;
end;

procedure TCommunicationFrameworkWithP2PVM.ListenState(const frameworkID: Cardinal; const ipv6: TIPV6; const Port: Word; const Listening: Boolean);
var
  RBuf: array [0 .. 18] of Byte;
  p: Pp2pVMFragmentPackage;
begin
  if (FPhysicsTunnel = nil) or (not WasAuthed) then
      exit;
  PIPV6(@RBuf[0])^ := ipv6;
  PWORD(@RBuf[16])^ := Port;
  PBoolean(@RBuf[18])^ := Listening;
  p := BuildP2PVMPackage(SizeOf(RBuf), frameworkID, 0, c_p2pVM_ListenState, @RBuf[0]);

  FSendStream.Position := FSendStream.Size;
  p^.BuildSendBuff(FSendStream);
  FreeP2PVMPackage(p);
end;

procedure TCommunicationFrameworkWithP2PVM.Connecting(const Remote_frameworkID, frameworkID, p2pID: Cardinal; const ipv6: TIPV6; const Port: Word);
var
  RBuf: array [0 .. 25] of Byte;
  p: Pp2pVMFragmentPackage;
begin
  if (FPhysicsTunnel = nil) or (not WasAuthed) then
      exit;
  PCardinal(@RBuf[0])^ := frameworkID;
  PCardinal(@RBuf[4])^ := p2pID;
  PIPV6(@RBuf[8])^ := ipv6;
  PWORD(@RBuf[24])^ := Port;

  p := BuildP2PVMPackage(SizeOf(RBuf), Remote_frameworkID, 0, c_p2pVM_Connecting, @RBuf[0]);

  FSendStream.Position := FSendStream.Size;
  p^.BuildSendBuff(FSendStream);
  FreeP2PVMPackage(p);
end;

procedure TCommunicationFrameworkWithP2PVM.ConnectedReponse(const Remote_frameworkID, Remote_p2pID, frameworkID, p2pID: Cardinal);
var
  RBuf: array [0 .. 7] of Byte;
  p: Pp2pVMFragmentPackage;
begin
  if (FPhysicsTunnel = nil) or (not WasAuthed) then
      exit;
  PCardinal(@RBuf[0])^ := frameworkID;
  PCardinal(@RBuf[4])^ := p2pID;

  p := BuildP2PVMPackage(SizeOf(RBuf), Remote_frameworkID, Remote_p2pID, c_p2pVM_ConnectedReponse, @RBuf[0]);

  FSendStream.Position := FSendStream.Size;
  p^.BuildSendBuff(FSendStream);
  FreeP2PVMPackage(p);
end;

procedure TCommunicationFrameworkWithP2PVM.Disconnect(const Remote_frameworkID, Remote_p2pID: Cardinal);
var
  p: Pp2pVMFragmentPackage;
begin
  if (FPhysicsTunnel = nil) or (not WasAuthed) then
      exit;
  p := BuildP2PVMPackage(0, Remote_frameworkID, Remote_p2pID, c_p2pVM_Disconnect, nil);

  FSendStream.Position := FSendStream.Size;
  p^.BuildSendBuff(FSendStream);
  FreeP2PVMPackage(p);
end;

function TCommunicationFrameworkWithP2PVM.ListenCount: Integer;
begin
  Result := FFrameworkListenPool.Count;
end;

function TCommunicationFrameworkWithP2PVM.GetListen(const index: Integer): Pp2pVMListen;
begin
  Result := FFrameworkListenPool[index];
end;

function TCommunicationFrameworkWithP2PVM.FindListen(const ipv6: TIPV6; const Port: Word): Pp2pVMListen;
var
  i: Integer;
  p: Pp2pVMListen;
begin
  for i := 0 to FFrameworkListenPool.Count - 1 do
    begin
      p := FFrameworkListenPool[i];
      if (p^.ListenPort = Port) and (CompareIPV6(p^.ListenHost, ipv6)) then
        begin
          Result := p;
          exit;
        end;
    end;
  Result := nil;
end;

function TCommunicationFrameworkWithP2PVM.FindListening(const ipv6: TIPV6; const Port: Word): Pp2pVMListen;
var
  i: Integer;
  p: Pp2pVMListen;
begin
  for i := 0 to FFrameworkListenPool.Count - 1 do
    begin
      p := FFrameworkListenPool[i];
      if (p^.Listening) and (p^.ListenPort = Port) and (CompareIPV6(p^.ListenHost, ipv6)) then
        begin
          Result := p;
          exit;
        end;
    end;
  Result := nil;
end;

procedure TCommunicationFrameworkWithP2PVM.DeleteListen(const ipv6: TIPV6; const Port: Word);
var
  i: Integer;
  p: Pp2pVMListen;
begin
  i := 0;
  while i < FFrameworkListenPool.Count do
    begin
      p := FFrameworkListenPool[i];
      if (p^.ListenPort = Port) and (CompareIPV6(p^.ListenHost, ipv6)) then
        begin
          Dispose(p);
          FFrameworkListenPool.Delete(i);
        end
      else
          inc(i);
    end;
end;

procedure TCommunicationFrameworkWithP2PVM.ClearListen;
var
  i: Integer;
begin
  for i := 0 to FFrameworkListenPool.Count - 1 do
      Dispose(Pp2pVMListen(FFrameworkListenPool[i]));
  FFrameworkListenPool.Clear;
end;

procedure TCommunicationFrameworkWithP2PVM.CloseAllClientIO;
var
  i: Integer;
  p: PUInt32HashListObjectStruct;
begin
  if (FFrameworkPool.Count > 0) then
    begin
      i := 0;
      p := FFrameworkPool.FirstPtr;
      while i < FFrameworkPool.Count do
        begin
          if p^.Data is TCommunicationFrameworkWithP2PVM_Client then
              TCommunicationFramework(p^.Data).ProgressPerClientM({$IFDEF FPC}@{$ENDIF FPC}DoPerClientClose);
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure TCommunicationFrameworkWithP2PVM.CloseAllServerIO;
var
  i: Integer;
  p: PUInt32HashListObjectStruct;
begin
  if (FFrameworkPool.Count > 0) then
    begin
      i := 0;
      p := FFrameworkPool.FirstPtr;
      while i < FFrameworkPool.Count do
        begin
          if p^.Data is TCommunicationFrameworkWithP2PVM_Server then
              TCommunicationFramework(p^.Data).ProgressPerClientM({$IFDEF FPC}@{$ENDIF FPC}DoPerClientClose);
          inc(i);
          p := p^.Next;
        end;
    end;
end;

initialization

ProgressBackgroundProc := nil;
ProgressBackgroundMethod := nil;

finalization

end.
