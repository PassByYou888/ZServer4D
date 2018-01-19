{ ****************************************************************************** }
{ * communication framework written by QQ 600585@qq.com                        * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
(* https://github.com/PassByYou888/ZServer4D *)
{ ****************************************************************************** }
(*
  update history
  2017-11-28  support anonymous function
  2017-12-6 added TBigStreamBatchList
*)

unit CommunicationFramework;

interface

{$I zDefine.inc}


uses Classes, SysUtils, Variants, TypInfo,
  CoreClasses, ListEngine, UnicodeMixedLib, DoStatusIO,
  DataFrameEngine, MemoryStream64, PascalStrings, CoreCipher, NotifyObjectBase, Cadencer;

type
  TPeerClient = class;

  TConsoleMethod     = procedure(Sender: TPeerClient; ResultData: SystemString) of object;
  TStreamMethod      = procedure(Sender: TPeerClient; ResultData: TDataFrameEngine) of object;
  TStreamParamMethod = procedure(Sender: TPeerClient; Param1: Pointer; Param2: TObject; InData, ResultData: TDataFrameEngine) of object;

  TStateCall   = procedure(const State: Boolean);
  TStateMethod = procedure(const State: Boolean) of object;

  TNotifyCall   = procedure();
  TNotifyMethod = procedure() of object;

  {$IFNDEF FPC}
  TConsoleProc     = reference to procedure(Sender: TPeerClient; ResultData: SystemString);
  TStreamProc      = reference to procedure(Sender: TPeerClient; ResultData: TDataFrameEngine);
  TStreamParamProc = reference to procedure(Sender: TPeerClient; Param1: Pointer; Param2: TObject; InData, ResultData: TDataFrameEngine);
  TStateProc       = reference to procedure(const State: Boolean);
  TNotifyProc      = reference to procedure();
  {$ENDIF}
  TQueueState = (qsUnknow, qsSendConsoleCMD, qsSendStreamCMD, qsSendDirectConsoleCMD, qsSendDirectStreamCMD, qsSendBigStream);

  TQueueData = record
    State: TQueueState;
    Client: TPeerClient;
    Cmd: SystemString;
    Cipher: TCipherStyle;

    ConsoleData: SystemString;
    OnConsoleMethod: TConsoleMethod;
    {$IFNDEF FPC}
    OnConsoleProc: TConsoleProc;
    {$ENDIF}
    StreamData: TCoreClassStream;
    OnStreamMethod: TStreamMethod;
    OnStreamParamMethod: TStreamParamMethod;
    {$IFNDEF FPC}
    OnStreamProc: TStreamProc;
    OnStreamParamProc: TStreamParamProc;
    {$ENDIF}
    BigStream: TCoreClassStream;

    DoneFreeStream: Boolean;

    Param1: Pointer;
    Param2: TObject;
  end;

  PQueueData = ^TQueueData;

  TCommandStreamProc = procedure(Sender: TPeerClient; InData, OutData: TDataFrameEngine) of object;

  TCommandStreamMode = class(TCoreClassObject)
  private
  protected
    FCommand: SystemString;

    FOnCommandStreamProc: TCommandStreamProc;
  public
    constructor Create;
    destructor Destroy; override;

    function Execute(Sender: TPeerClient; InData, OutData: TDataFrameEngine): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    property Command: SystemString read FCommand write FCommand;
    property OnExecute: TCommandStreamProc read FOnCommandStreamProc write FOnCommandStreamProc;
  end;

  TCommandConsoleProc = procedure(Sender: TPeerClient; InData: SystemString; var OutData: SystemString) of object;

  TCommandConsoleMode = class(TCoreClassObject)
  private
  protected
    FCommand: SystemString;

    FOnCommandConsoleProc: TCommandConsoleProc;
  public
    constructor Create;
    destructor Destroy; override;

    function Execute(Sender: TPeerClient; InData: SystemString; var OutData: SystemString): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    property Command: SystemString read FCommand write FCommand;
    property OnExecute: TCommandConsoleProc read FOnCommandConsoleProc write FOnCommandConsoleProc;
  end;

  TCommandDirectStreamProc = procedure(Sender: TPeerClient; InData: TDataFrameEngine) of object;

  TCommandDirectStreamMode = class(TCoreClassObject)
  private
  protected
    FCommand: SystemString;

    FOnCommandDirectStreamProc: TCommandDirectStreamProc;
  public
    constructor Create;
    destructor Destroy; override;

    function Execute(Sender: TPeerClient; InData: TDataFrameEngine): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    property Command: SystemString read FCommand write FCommand;
    property OnExecute: TCommandDirectStreamProc read FOnCommandDirectStreamProc write FOnCommandDirectStreamProc;
  end;

  TCommandDirectConsoleProc = procedure(Sender: TPeerClient; InData: SystemString) of object;

  TCommandDirectConsoleMode = class(TCoreClassObject)
  private
  protected
    FCommand: SystemString;

    FOnCommandDirectConsoleProc: TCommandDirectConsoleProc;
  public
    constructor Create;
    destructor Destroy; override;

    function Execute(Sender: TPeerClient; InData: SystemString): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    property Command: SystemString read FCommand write FCommand;
    property OnExecute: TCommandDirectConsoleProc read FOnCommandDirectConsoleProc write FOnCommandDirectConsoleProc;
  end;

  TCommandBigStreamProc = procedure(Sender: TPeerClient; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64) of object;

  TCommandBigStreamMode = class(TCoreClassObject)
  private
  protected
    FCommand: SystemString;

    FOnCommandBigStreamProc: TCommandBigStreamProc;
  public
    constructor Create;
    destructor Destroy; override;

    function Execute(Sender: TPeerClient; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    property Command: SystemString read FCommand write FCommand;
    property OnExecute: TCommandBigStreamProc read FOnCommandBigStreamProc write FOnCommandBigStreamProc;
  end;

  TCommunicationFramework = class;

  PBigStreamBatchPostData = ^TBigStreamBatchPostData;

  TBigStreamBatchPostData = record
    Source: TMemoryStream64;
    CompletedBackcallPtr: UInt64;
    RemoteMD5: UnicodeMixedLib.TMD5;
    SourceMD5: UnicodeMixedLib.TMD5;
    Index: Integer;
    DBStorePos: Int64;

    procedure Init; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure Encode(d: TDataFrameEngine); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure Decode(d: TDataFrameEngine); {$IFDEF INLINE_ASM} inline; {$ENDIF}
  end;

  TBigStreamBatchList = class(TCoreClassInterfacedObject)
  private
    function GetItems(const Index: Integer): PBigStreamBatchPostData;
  protected
    FOwner: TPeerClient;
    FList : TCoreClassList;
  public
    constructor Create(AOwner: TPeerClient);
    destructor Destroy; override;

    procedure Clear; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Count: Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    property Items[const index: Integer]: PBigStreamBatchPostData read GetItems; default;
    function NewPostData: PBigStreamBatchPostData; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Last: PBigStreamBatchPostData; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure DeleteLast; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure Delete(const Index: Integer); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure RebuildMD5; {$IFDEF INLINE_ASM} inline; {$ENDIF}
  end;

  TPeerClientUserDefine = class(TCoreClassObject)
  protected
    FOwner             : TPeerClient;
    FWorkPlatform      : TExecutePlatform;
    FBigStreamBatchList: TBigStreamBatchList;
  public
    constructor Create(AOwner: TPeerClient); virtual;
    destructor Destroy; override;

    procedure Progress; virtual;

    property Owner: TPeerClient read FOwner;
    property WorkPlatform: TExecutePlatform read FWorkPlatform;
    property BigStreamBatchList: TBigStreamBatchList read FBigStreamBatchList;
  end;

  TPeerClientUserDefineClass = class of TPeerClientUserDefine;

  TPeerClientUserSpecial = class(TCoreClassInterfacedObject)
  protected
    FOwner: TPeerClient;
  public
    constructor Create(AOwner: TPeerClient); virtual;
    destructor Destroy; override;

    procedure Progress; virtual;

    property Owner: TPeerClient read FOwner;
  end;

  TPeerClientUserSpecialClass = class of TPeerClientUserSpecial;

  TPeerClient = class(TCoreClassInterfacedObject, IMemoryStream64WriteTrigger)
  private
    FOwnerFramework                                                                      : TCommunicationFramework;
    FClientIntf                                                                          : TCoreClassObject;
    FID                                                                                  : Cardinal;
    FHeadToken, FTailToken                                                               : Cardinal;
    FConsoleToken, FStreamToken, FDirectConsoleToken, FDirectStreamToken, FBigStreamToken: Byte;
    FReceivedBuffer                                                                      : TMemoryStream64OfWriteTrigger;
    FBigStreamReceiveProcessing                                                          : Boolean;
    FBigStreamTotal, FBigStreamCompleted                                                 : Int64;
    FBigStreamCmd                                                                        : SystemString;
    FBigStreamReceive                                                                    : TCoreClassStream;
    FBigStreamSending                                                                    : TCoreClassStream;
    FBigStreamSendState                                                                  : Int64; // stream current position
    FBigStreamSendDoneTimeFree                                                           : Boolean;
    FCurrentQueueData                                                                    : PQueueData;
    FWaitOnResult                                                                        : Boolean;
    FCurrentPauseResultSend_CommDataType                                                 : Byte;
    FCanPauseResultSend                                                                  : Boolean;
    FPauseResultSend                                                                     : Boolean;
    FRunReceiveTrigger                                                                   : Boolean;
    FReceiveDataCipherStyle                                                              : TCipherStyle;
    FResultDataBuffer                                                                    : TMemoryStream64;
    FSendDataCipherStyle                                                                 : TCipherStyle;
    FAllSendProcessing                                                                   : Boolean;
    FReceiveProcessing                                                                   : Boolean;
    FQueueList                                                                           : TCoreClassList;
    FLastCommunicationTimeTickCount                                                      : TTimeTickValue;
    FCipherKey                                                                           : TCipherKeyBuffer;
    FRemoteExecutedForConnectInit                                                        : Boolean;
    FInCmd                                                                               : SystemString;
    FInText, FOutText                                                                    : SystemString;
    FInDataFrame, FOutDataFrame                                                          : TDataFrameEngine;
    ResultText                                                                           : SystemString;
    ResultDataFrame                                                                      : TDataFrameEngine;
    FSyncPick                                                                            : PQueueData;
    FWaitSendBusy                                                                        : Boolean;
    FReceiveCommandRuning                                                                : Boolean;
    FReceiveResultRuning                                                                 : Boolean;
  private
    // user
    FUserData           : Pointer;
    FUserValue          : Variant;
    FUserVariants       : THashVariantList;
    FUserObjects        : THashObjectList;
    FUserAutoFreeObjects: THashObjectList;
    FUserDefine         : TPeerClientUserDefine;
    FUserSpecial        : TPeerClientUserSpecial;

    function GetUserVariants: THashVariantList;
    function GetUserObjects: THashObjectList;
    function GetUserAutoFreeObjects: THashObjectList;
  private
    procedure TriggerWrite64(Count: Int64);
  private
    procedure InternalSendByteBuffer(buff: PByte; Size: Integer); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    //
    procedure SendInteger(v: Integer); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure SendCardinal(v: Cardinal); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure SendInt64(v: Int64); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure SendByte(v: Byte); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure SendWord(v: Word); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure SendVerifyCode(buff: Pointer; siz: Integer); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure SendEncryptBuffer(buff: PByte; Size: Integer; cs: TCipherStyle); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure SendEncryptMemoryStream(stream: TMemoryStream64; cs: TCipherStyle); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    //
    procedure InternalSendConsoleBuff(buff: TMemoryStream64; cs: TCipherStyle); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure InternalSendStreamBuff(buff: TMemoryStream64; cs: TCipherStyle); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure InternalSendDirectConsoleBuff(buff: TMemoryStream64; cs: TCipherStyle); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure InternalSendDirectStreamBuff(buff: TMemoryStream64; cs: TCipherStyle); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure InternalSendBigStreamHeader(Cmd: SystemString; streamSiz: Int64); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure InternalSendBigStreamBuff(var Queue: TQueueData); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    //
    procedure Sync_InternalSendResultData;
    procedure Sync_InternalSendConsoleCmd;
    procedure Sync_InternalSendStreamCmd;
    procedure Sync_InternalSendDirectConsoleCmd;
    procedure Sync_InternalSendDirectStreamCmd;
    procedure Sync_InternalSendBigStreamCmd;

    procedure Sync_ExecuteConsole;
    procedure Sync_ExecuteStream;
    procedure Sync_ExecuteDirectConsole;
    procedure Sync_ExecuteDirectStream;
    procedure ExecuteDataFrame(ACurrentActiveThread: TCoreClassThread; const Sync: Boolean; CommDataType: Byte; dataFrame: TDataFrameEngine);

    procedure Sync_ExecuteBigStream;
    function FillBigStreamBuffer(ACurrentActiveThread: TCoreClassThread; const Sync: Boolean): Boolean;

    procedure Sync_ExecuteResult;
    function FillWaitOnResultBuffer(ACurrentActiveThread: TCoreClassThread; const RecvSync, SendSync: Boolean): Boolean;
  public
    // external interface
    function Connected: Boolean; virtual; abstract;
    procedure Disconnect; virtual; abstract;
    procedure SendByteBuffer(buff: PByte; Size: Integer); virtual; abstract;
    procedure WriteBufferOpen; virtual; abstract;
    procedure WriteBufferFlush; virtual; abstract;
    procedure WriteBufferClose; virtual; abstract;
    function GetPeerIP: SystemString; virtual; abstract;
    function WriteBufferEmpty: Boolean; virtual;
  public
    constructor Create(AOwnerFramework: TCommunicationFramework; AClientIntf: TCoreClassObject); virtual;
    destructor Destroy; override;

    procedure Print(v: SystemString); overload;
    procedure Print(v: SystemString; const Args: array of const); overload;
    procedure PrintCommand(v: SystemString; Args: SystemString);
    procedure PrintParam(v: SystemString; Args: SystemString);

    procedure Progress; virtual;

    property ReceivedBuffer: TMemoryStream64OfWriteTrigger read FReceivedBuffer;
    procedure FillRecvBuffer(ACurrentActiveThread: TCoreClassThread; const RecvSync, SendSync: Boolean);
    procedure PostQueueData(p: PQueueData);
    function ProcessAllSendCmd(ACurrentActiveThread: TCoreClassThread; const RecvSync, SendSync: Boolean): Integer;

    procedure PauseResultSend; virtual;
    procedure ContinueResultSend; virtual;

    property CurrentBigStreamCommand: SystemString read FBigStreamCmd;
    property CurrentCommand: SystemString read FInCmd;

    // ContinueResultSend use it
    property InText: SystemString read FInText;
    property OutText: SystemString read FOutText write FOutText;
    property InDataFrame: TDataFrameEngine read FInDataFrame;
    property OutDataFrame: TDataFrameEngine read FOutDataFrame;
    function ResultSendIsPaused: Boolean;

    // state
    property WaitOnResult: Boolean read FWaitOnResult;
    property AllSendProcessing: Boolean read FAllSendProcessing;
    property BigStreamProcessing: Boolean read FBigStreamReceiveProcessing;
    property WaitSendBusy: Boolean read FWaitSendBusy;
    property ReceiveCommandRuning: Boolean read FReceiveCommandRuning;
    property ReceiveResultRuning: Boolean read FReceiveResultRuning;
    function BigStreamIsSending: Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    //
    // framework
    property OwnerFramework: TCommunicationFramework read FOwnerFramework;
    property ClientIntf: TCoreClassObject read FClientIntf write FClientIntf;
    property ID: Cardinal read FID;
    property CipherKey: TCipherKeyBuffer read FCipherKey;
    function CipherKeyPtr: PCipherKeyBuffer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    property SendCipherStyle: TCipherStyle read FSendDataCipherStyle write FSendDataCipherStyle;
    property RemoteExecutedForConnectInit: Boolean read FRemoteExecutedForConnectInit;

    // remote
    property PeerIP: SystemString read GetPeerIP;

    // user define
    property UserVariants: THashVariantList read GetUserVariants;
    property UserObjects: THashObjectList read GetUserObjects;
    property UserAutoFreeObjects: THashObjectList read GetUserAutoFreeObjects;
    property UserData: Pointer read FUserData write FUserData;
    property UserValue: Variant read FUserValue write FUserValue;
    property UserDefine: TPeerClientUserDefine read FUserDefine;
    property UserSpecial: TPeerClientUserSpecial read FUserSpecial;

    // hash code
    procedure GenerateHashCode(hs: THashStyle; buff: Pointer; siz: Integer; var output: TBytes);
    function VerifyHashCode(hs: THashStyle; buff: Pointer; siz: Integer; var code: TBytes): Boolean;
    // encrypt
    procedure Encrypt(cs: TCipherStyle; DataPtr: Pointer; Size: Cardinal; var k: TCipherKeyBuffer; Enc: Boolean);

    // timeout
    function StopCommunicationTime: TTimeTickValue;
    procedure SetLastCommunicationTimeAsCurrent;

    // queue data
    property CurrentQueueData: PQueueData read FCurrentQueueData;

    // send cmd and method return
    procedure SendConsoleCmd(Cmd: SystemString; ConsoleData: SystemString; OnResult: TConsoleMethod); overload;
    procedure SendStreamCmd(Cmd: SystemString; StreamData: TCoreClassStream; OnResult: TStreamMethod; DoneFreeStream: Boolean); overload;
    procedure SendStreamCmd(Cmd: SystemString; StreamData: TDataFrameEngine; OnResult: TStreamMethod); overload;
    procedure SendStreamCmd(Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamMethod); overload;

    // send cmd and proc return
    {$IFNDEF FPC}
    procedure SendConsoleCmd(Cmd: SystemString; ConsoleData: SystemString; OnResult: TConsoleProc); overload;
    procedure SendStreamCmd(Cmd: SystemString; StreamData: TCoreClassStream; OnResult: TStreamProc; DoneFreeStream: Boolean); overload;
    procedure SendStreamCmd(Cmd: SystemString; StreamData: TDataFrameEngine; OnResult: TStreamProc); overload;
    procedure SendStreamCmd(Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamProc); overload;
    {$ENDIF}
    // direct send cmd
    procedure SendDirectConsoleCmd(Cmd: SystemString; ConsoleData: SystemString);
    procedure SendDirectStreamCmd(Cmd: SystemString; StreamData: TCoreClassStream; DoneFreeStream: Boolean); overload;
    procedure SendDirectStreamCmd(Cmd: SystemString; StreamData: TDataFrameEngine); overload;
    procedure SendDirectStreamCmd(Cmd: SystemString); overload;

    // wait send cmd
    function WaitSendConsoleCmd(Cmd: SystemString; ConsoleData: SystemString; TimeOut: TTimeTickValue): SystemString;
    procedure WaitSendStreamCmd(Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; TimeOut: TTimeTickValue);

    // send bigstream
    procedure SendBigStream(Cmd: SystemString; BigStream: TCoreClassStream; DoneFreeStream: Boolean);
  end;

  TPeerClientNotify    = procedure(Sender: TPeerClient) of object;
  TPeerClientCMDNotify = procedure(Sender: TPeerClient; Cmd: SystemString; var Allow: Boolean) of object;

  TStatisticsType = (
    stReceiveSize, stSendSize,
    stRequest, stResponse,
    stConsole, stStream, stDirestConsole, stDirestStream, stReceiveBigStream, stSendBigStream,
    stExecConsole, stExecStream, stExecDirestConsole, stExecDirestStream, stExecBigStream,
    stTriggerConnect, stTriggerDisconnect,
    stTotalCommandExecute, stTotalCommandSend, stTotalCommandReg,
    stEncrypt, stCompress, stGenerateHash,
    stPause, stContinue,
    stLock, stUnLock,
    stPrint, stIDCounter);

  TPerClientListCall   = procedure(PeerClient: TPeerClient);
  TPerClientListMethod = procedure(PeerClient: TPeerClient) of object;
  {$IFNDEF FPC}
  TPerClientListProc = reference to procedure(PeerClient: TPeerClient);
  {$ENDIF}
  TClientIDPool = array of Cardinal;

  TCommunicationFramework = class(TCoreClassInterfacedObject)
  private
    FCommandList               : THashObjectList;
    FPerClientHashList         : TUInt32HashObjectList;
    FIDCounter                 : Cardinal;
    FOnConnected               : TPeerClientNotify;
    FOnDisconnect              : TPeerClientNotify;
    FOnExecuteCommand          : TPeerClientCMDNotify;
    FOnSendCommand             : TPeerClientCMDNotify;
    FPeerClientUserDefineClass : TPeerClientUserDefineClass;
    FPeerClientUserSpecialClass: TPeerClientUserSpecialClass;
    FIdleTimeout               : TTimeTickValue;
    FSendDataCompressed        : Boolean;
    FUsedParallelEncrypt       : Boolean;
    FSyncOnResult              : Boolean;
    FAllowPrintCommand         : Boolean;
    FCipherStyle               : TCipherStyle;
    FHashStyle                 : THashStyle;
    FPrintParams               : THashVariantList;
    FProgressPost              : TNProgressPostWithCadencer;

    FOnPeerClientCreateNotify : TBackcalls;
    FOnPeerClientDestroyNotify: TBackcalls;
  protected
    procedure DoPrint(const v: SystemString); virtual;
  protected
    function GetIdleTimeout: TTimeTickValue; virtual;
    procedure SetIdleTimeout(const Value: TTimeTickValue); virtual;

    procedure DoConnected(Sender: TPeerClient); virtual;
    procedure DoDisconnect(Sender: TPeerClient); virtual;

    function CanExecuteCommand(Sender: TPeerClient; Cmd: SystemString): Boolean; virtual;
    function CanSendCommand(Sender: TPeerClient; Cmd: SystemString): Boolean; virtual;
    function CanRegCommand(Sender: TCommunicationFramework; Cmd: SystemString): Boolean; virtual;

    procedure DelayExecuteOnResultState(Sender: TNPostExecute);
  public
    Statistics                    : array [TStatisticsType] of Int64;
    CmdRecvStatistics             : THashVariantList;
    CmdSendStatistics             : THashVariantList;
    CmdMaxExecuteConsumeStatistics: THashVariantList;

    constructor Create;
    destructor Destroy; override;

    procedure SwitchMaxPerformance;
    procedure SwitchMaxSafe;
    procedure SwitchDefaultPerformance;

    procedure LockClients;
    procedure UnLockClients;

    property ProgressPost: TNProgressPostWithCadencer read FProgressPost;

    procedure ProgressBackground; virtual;

    procedure ProgressPerClient(OnProgress: TPerClientListCall); overload;
    procedure ProgressPerClient(OnProgress: TPerClientListMethod); overload;
    {$IFNDEF FPC}
    procedure ProgressPerClient(OnProgress: TPerClientListProc); overload;
    {$ENDIF}
    //
    procedure GetClientIDPool(out IDPool: TClientIDPool);

    procedure ProgressWaitSendOfClient(Client: TPeerClient); virtual;

    procedure PrintParam(v: SystemString; Args: SystemString);

    function DeleteRegistedCMD(Cmd: SystemString): Boolean;
    function UnRegisted(Cmd: SystemString): Boolean;

    function RegisterConsole(Cmd: SystemString): TCommandConsoleMode;
    function RegisterStream(Cmd: SystemString): TCommandStreamMode;
    function RegisterDirectStream(Cmd: SystemString): TCommandDirectStreamMode;
    function RegisterDirectConsole(Cmd: SystemString): TCommandDirectConsoleMode;
    function RegisterBigStream(Cmd: SystemString): TCommandBigStreamMode;

    function ExecuteConsole(Sender: TPeerClient; Cmd: SystemString; const InData: SystemString; var OutData: SystemString): Boolean; virtual;
    function ExecuteStream(Sender: TPeerClient; Cmd: SystemString; InData, OutData: TDataFrameEngine): Boolean; virtual;
    function ExecuteDirectStream(Sender: TPeerClient; Cmd: SystemString; InData: TDataFrameEngine): Boolean; virtual;
    function ExecuteDirectConsole(Sender: TPeerClient; Cmd: SystemString; const InData: SystemString): Boolean; virtual;
    function ExecuteBigStream(Sender: TPeerClient; Cmd: SystemString; InData: TCoreClassStream; FBigStreamTotal, BigStreamCompleteSize: Int64): Boolean; virtual;

    function FirstClient: TPeerClient;
    function LastClient: TPeerClient;

    property OnConnected: TPeerClientNotify read FOnConnected write FOnConnected;
    property OnDisconnect: TPeerClientNotify read FOnDisconnect write FOnDisconnect;
    property OnExecuteCommand: TPeerClientCMDNotify read FOnExecuteCommand write FOnExecuteCommand;
    property OnSendCommand: TPeerClientCMDNotify read FOnSendCommand write FOnSendCommand;

    property OnPeerClientCreateNotify: TBackcalls read FOnPeerClientCreateNotify;
    property OnPeerClientDestroyNotify: TBackcalls read FOnPeerClientDestroyNotify;

    property UsedParallelEncrypt: Boolean read FUsedParallelEncrypt write FUsedParallelEncrypt;
    property SyncOnResult: Boolean read FSyncOnResult write FSyncOnResult;
    property AllowPrintCommand: Boolean read FAllowPrintCommand write FAllowPrintCommand;
    property CipherStyle: TCipherStyle read FCipherStyle;
    property IdleTimeout: TTimeTickValue read GetIdleTimeout write SetIdleTimeout;
    property SendDataCompressed: Boolean read FSendDataCompressed;
    property HashStyle: THashStyle read FHashStyle;

    property PeerClientUserDefineClass: TPeerClientUserDefineClass read FPeerClientUserDefineClass write FPeerClientUserDefineClass;
    property PeerClientUserSpecialClass: TPeerClientUserSpecialClass read FPeerClientUserSpecialClass write FPeerClientUserSpecialClass;

    property IDCounter: Cardinal read FIDCounter write FIDCounter;
    property PrintParams: THashVariantList read FPrintParams;
  end;

  TCommunicationFrameworkServer = class(TCommunicationFramework)
  protected
    procedure DoPrint(const v: SystemString); override;
    function CanExecuteCommand(Sender: TPeerClient; Cmd: SystemString): Boolean; override;
    function CanSendCommand(Sender: TPeerClient; Cmd: SystemString): Boolean; override;
    function CanRegCommand(Sender: TCommunicationFramework; Cmd: SystemString): Boolean; override;

    procedure Command_ConnectedInit(Sender: TPeerClient; InData, OutData: TDataFrameEngine); virtual;
    procedure Command_Wait(Sender: TPeerClient; InData: SystemString; var OutData: SystemString); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure StopService; virtual; abstract;
    function StartService(Host: SystemString; Port: Word): Boolean; virtual; abstract;
    procedure TriggerQueueData(v: PQueueData); virtual; abstract;

    // send cmd method
    procedure SendConsoleCmd(Client: TPeerClient; Cmd: SystemString; ConsoleData: SystemString; OnResult: TConsoleMethod); overload;
    procedure SendStreamCmd(Client: TPeerClient; Cmd: SystemString; StreamData: TCoreClassStream; OnResult: TStreamMethod; DoneFreeStream: Boolean); overload;
    procedure SendStreamCmd(Client: TPeerClient; Cmd: SystemString; StreamData: TDataFrameEngine; OnResult: TStreamMethod); overload;
    procedure SendStreamCmd(Client: TPeerClient; Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamMethod); overload;

    // send cmd proc
    {$IFNDEF FPC}
    procedure SendConsoleCmd(Client: TPeerClient; Cmd: SystemString; ConsoleData: SystemString; OnResult: TConsoleProc); overload;
    procedure SendStreamCmd(Client: TPeerClient; Cmd: SystemString; StreamData: TCoreClassStream; OnResult: TStreamProc; DoneFreeStream: Boolean); overload;
    procedure SendStreamCmd(Client: TPeerClient; Cmd: SystemString; StreamData: TDataFrameEngine; OnResult: TStreamProc); overload;
    procedure SendStreamCmd(Client: TPeerClient; Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamProc); overload;
    {$ENDIF}
    // send direct cmd
    procedure SendDirectConsoleCmd(Client: TPeerClient; Cmd: SystemString; ConsoleData: SystemString); overload;
    procedure SendDirectStreamCmd(Client: TPeerClient; Cmd: SystemString; StreamData: TCoreClassStream; DoneFreeStream: Boolean); overload;
    procedure SendDirectStreamCmd(Client: TPeerClient; Cmd: SystemString; StreamData: TDataFrameEngine); overload;
    procedure SendDirectStreamCmd(Client: TPeerClient; Cmd: SystemString); overload;

    // wait send
    function WaitSendConsoleCmd(Client: TPeerClient; Cmd: SystemString; ConsoleData: SystemString; TimeOut: TTimeTickValue): SystemString; overload; virtual;
    procedure WaitSendStreamCmd(Client: TPeerClient; Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; TimeOut: TTimeTickValue); overload; virtual;

    // send bitstream
    procedure SendBigStream(Client: TPeerClient; Cmd: SystemString; BigStream: TCoreClassStream; DoneFreeStream: Boolean); overload;

    // send used client ID,return method
    procedure SendConsoleCmd(ClientID: Cardinal; Cmd: SystemString; ConsoleData: SystemString; OnResult: TConsoleMethod); overload;
    procedure SendStreamCmd(ClientID: Cardinal; Cmd: SystemString; StreamData: TCoreClassStream; OnResult: TStreamMethod; DoneFreeStream: Boolean); overload;
    procedure SendStreamCmd(ClientID: Cardinal; Cmd: SystemString; StreamData: TDataFrameEngine; OnResult: TStreamMethod); overload;
    procedure SendStreamCmd(ClientID: Cardinal; Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamMethod); overload;

    // send used client ID,return proc
    {$IFNDEF FPC}
    procedure SendConsoleCmd(ClientID: Cardinal; Cmd: SystemString; ConsoleData: SystemString; OnResult: TConsoleProc); overload;
    procedure SendStreamCmd(ClientID: Cardinal; Cmd: SystemString; StreamData: TCoreClassStream; OnResult: TStreamProc; DoneFreeStream: Boolean); overload;
    procedure SendStreamCmd(ClientID: Cardinal; Cmd: SystemString; StreamData: TDataFrameEngine; OnResult: TStreamProc); overload;
    procedure SendStreamCmd(ClientID: Cardinal; Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamProc); overload;
    {$ENDIF}
    // direct send used client ID
    procedure SendDirectConsoleCmd(ClientID: Cardinal; Cmd: SystemString; ConsoleData: SystemString); overload;
    procedure SendDirectStreamCmd(ClientID: Cardinal; Cmd: SystemString; StreamData: TCoreClassStream; DoneFreeStream: Boolean); overload;
    procedure SendDirectStreamCmd(ClientID: Cardinal; Cmd: SystemString; StreamData: TDataFrameEngine); overload;
    procedure SendDirectStreamCmd(ClientID: Cardinal; Cmd: SystemString); overload;

    // wait send
    function WaitSendConsoleCmd(ClientID: Cardinal; Cmd: SystemString; ConsoleData: SystemString; TimeOut: TTimeTickValue): SystemString; overload;
    procedure WaitSendStreamCmd(ClientID: Cardinal; Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; TimeOut: TTimeTickValue); overload;

    // send bitstream
    procedure SendBigStream(ClientID: Cardinal; Cmd: SystemString; BigStream: TCoreClassStream; DoneFreeStream: Boolean); overload;

    // Broadcast to all client
    procedure BroadcastDirectConsoleCmd(Cmd: SystemString; ConsoleData: SystemString);
    procedure BroadcastSendDirectStreamCmd(Cmd: SystemString; StreamData: TDataFrameEngine);

    function Count: Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Exists(cli: TCoreClassObject): Boolean; overload;
    function Exists(cli: TPeerClient): Boolean; overload;
    function Exists(cli: TPeerClientUserDefine): Boolean; overload;
    function Exists(cli: TPeerClientUserSpecial): Boolean; overload;

    function Exists(ClientID: Cardinal): Boolean; overload;
    function GetClientFromID(ID: Cardinal): TPeerClient;
    property ClientFromID[ID: Cardinal]: TPeerClient read GetClientFromID;
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

    FConnectInitWaiting       : Boolean;
    FConnectInitWaitingTimeout: TTimeTickValue;

    procedure DoPrint(const v: SystemString); override;

    procedure StreamResult_ConnectedInit(Sender: TPeerClient; ResultData: TDataFrameEngine); virtual;
    procedure DoConnected(Sender: TPeerClient); override;
    procedure DoDisconnect(Sender: TPeerClient); override;

    function CanExecuteCommand(Sender: TPeerClient; Cmd: SystemString): Boolean; override;
    function CanSendCommand(Sender: TPeerClient; Cmd: SystemString): Boolean; override;
    function CanRegCommand(Sender: TCommunicationFramework; Cmd: SystemString): Boolean; override;
  protected
    // async wait support
    FWaiting           : Boolean;
    FWaitingTimeOut    : TTimeTickValue;
    FOnWaitResultCall  : TStateCall;
    FOnWaitResultMethod: TStateMethod;
    {$IFNDEF FPC}
    FOnWaitResultProc: TStateProc;
    {$ENDIF}
    procedure ConsoleResult_Wait(Sender: TPeerClient; ResultData: SystemString);
  public
    constructor Create; virtual;

    procedure ProgressBackground; override;

    procedure TriggerDoDisconnect;

    function Connected: Boolean; virtual;
    function ClientIO: TPeerClient; virtual;
    procedure TriggerQueueData(v: PQueueData); virtual;

    // async connect support
    procedure TriggerDoConnectFailed; virtual;
    procedure TriggerDoConnectFinished; virtual;

    procedure AsyncConnect(Addr: SystemString; Port: Word; OnResult: TStateCall); overload; virtual;
    procedure AsyncConnect(Addr: SystemString; Port: Word; OnResult: TStateMethod); overload; virtual;
    {$IFNDEF FPC} procedure AsyncConnect(Addr: SystemString; Port: Word; OnResult: TStateProc); overload; virtual; {$ENDIF}
    function Connect(Addr: SystemString; Port: Word): Boolean; virtual;
    procedure Disconnect; virtual;

    // sync KeepAlive
    function Wait(ATimeOut: TTimeTickValue): string; overload;
    // async KeepAlive
    function Wait(ATimeOut: TTimeTickValue; OnResult: TStateCall): Boolean; overload;
    function Wait(ATimeOut: TTimeTickValue; OnResult: TStateMethod): Boolean; overload;
    {$IFNDEF FPC} function Wait(ATimeOut: TTimeTickValue; OnResult: TStateProc): Boolean; overload; {$ENDIF}
    //
    function WaitSendBusy: Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function LastQueueData: PQueueData; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function LastQueueCmd: SystemString; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function QueueCmdCount: Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    //
    // send cmd method
    procedure SendConsoleCmd(Cmd: SystemString; ConsoleData: SystemString; OnResult: TConsoleMethod); overload;
    procedure SendStreamCmd(Cmd: SystemString; StreamData: TCoreClassStream; OnResult: TStreamMethod; DoneFreeStream: Boolean); overload;
    procedure SendStreamCmd(Cmd: SystemString; StreamData: TDataFrameEngine; OnResult: TStreamMethod); overload;
    procedure SendStreamCmd(Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamMethod); overload;

    // send cmd proc
    {$IFNDEF FPC}
    procedure SendConsoleCmd(Cmd: SystemString; ConsoleData: SystemString; OnResult: TConsoleProc); overload;
    procedure SendStreamCmd(Cmd: SystemString; StreamData: TCoreClassStream; OnResult: TStreamProc; DoneFreeStream: Boolean); overload;
    procedure SendStreamCmd(Cmd: SystemString; StreamData: TDataFrameEngine; OnResult: TStreamProc); overload;
    procedure SendStreamCmd(Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamProc); overload;
    {$ENDIF}
    // send direct cmd
    procedure SendDirectConsoleCmd(Cmd: SystemString; ConsoleData: SystemString);
    procedure SendDirectStreamCmd(Cmd: SystemString; StreamData: TCoreClassStream; DoneFreeStream: Boolean); overload;
    procedure SendDirectStreamCmd(Cmd: SystemString; StreamData: TDataFrameEngine); overload;
    procedure SendDirectStreamCmd(Cmd: SystemString); overload;

    // wait send
    function WaitSendConsoleCmd(Cmd: SystemString; ConsoleData: SystemString; TimeOut: TTimeTickValue): SystemString; virtual;
    procedure WaitSendStreamCmd(Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; TimeOut: TTimeTickValue); virtual;

    // send bitstream
    procedure SendBigStream(Cmd: SystemString; BigStream: TCoreClassStream; DoneFreeStream: Boolean);

    property NotyifyInterface: ICommunicationFrameworkClientInterface read FNotyifyInterface write FNotyifyInterface;
    // remote service ID
    // success ID > 0
    // failed! ID = 0
    function RemoteID: Cardinal;
    function RemoteKey: TCipherKeyBuffer;
    function RemoteInited: Boolean;
  end;

  TCommunicationFrameworkClientClass = class of TCommunicationFrameworkClient;

  TProgressBackgroundProc = procedure();

  TIPV4 = array [0 .. 3] of Byte;
  TIPV6 = array [0 .. 7] of Word;

var
  // communication data token
  c_DefaultConsoleToken      : Byte = 11;
  c_DefaultStreamToken       : Byte = 22;
  c_DefaultDirectConsoleToken: Byte = 33;
  c_DefaultDirectStreamToken : Byte = 44;
  c_DefaultBigStreamToken    : Byte = 55;

  // user custom header verify token
  c_DataHeadToken: Cardinal = $F0F0F0F0;
  // user custom tail verify token
  c_DataTailToken: Cardinal = $F1F1F1F1;

  // dostatus print id
  c_DefaultDoStatusID: Integer = $FFFFFFFF;

  // global progress backcall
  ProgressBackgroundProc: TProgressBackgroundProc = nil;

procedure DisposeQueueData(v: PQueueData); {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure InitQueueData(var v: TQueueData); {$IFDEF INLINE_ASM} inline; {$ENDIF}
function NewQueueData: PQueueData; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function StrToIPv4(const S: umlString; var Success: Boolean): TIPV4; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function IPv4ToStr(const AIcsIPv4Addr: TIPV4): umlString; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function StrToIPv6(const S: umlString; var Success: Boolean; var ScopeID: Cardinal): TIPV6; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function IPv6ToStr(const IPv6Addr: TIPV6): umlString; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function IsIPv4(const S: umlString): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function IsIPV6(const S: umlString): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function TranslateBindAddr(const Addr: SystemString): SystemString; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure SyncMethod(t: TCoreClassThread; Sync: Boolean; proc: TThreadMethod); {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure DoExecuteResult(c: TPeerClient; QueuePtr: PQueueData; AResultText: SystemString; AResultDF: TDataFrameEngine); {$IFDEF INLINE_ASM} inline; {$ENDIF}

{$IFNDEF FPC}
function WaitSendConsoleCmdInThread(th: TCoreClassThread; cf: TCommunicationFrameworkClient; Cmd: SystemString; ConsoleData: SystemString; TimeOut: TTimeTickValue): SystemString;
procedure WaitSendStreamCmdInThread(th: TCoreClassThread; cf: TCommunicationFrameworkClient; Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; TimeOut: TTimeTickValue);
{$ENDIF}

procedure DefaultProgressBackgroundProc;

implementation

procedure DefaultProgressBackgroundProc;
begin
end;

procedure DisposeQueueData(v: PQueueData);
begin
  if v^.DoneFreeStream then
    begin
      if v^.StreamData <> nil then
          DisposeObject(v^.StreamData);

      if v^.BigStream <> nil then
          DisposeObject(v^.BigStream);
    end;

  Dispose(v);
end;

procedure InitQueueData(var v: TQueueData);
begin
  v.State := qsUnknow;
  v.Client := nil;
  v.Cmd := '';
  v.Cipher := TCipherStyle.csNone;
  v.ConsoleData := '';
  v.OnConsoleMethod := nil;
  {$IFNDEF FPC}
  v.OnConsoleProc := nil;
  {$ENDIF}
  v.StreamData := nil;
  v.OnStreamMethod := nil;
  v.OnStreamParamMethod := nil;
  {$IFNDEF FPC}
  v.OnStreamProc := nil;
  v.OnStreamParamProc := nil;
  {$ENDIF}
  v.BigStream := nil;
  v.DoneFreeStream := True;
  v.Param1 := nil;
  v.Param2 := nil;
end;

function NewQueueData: PQueueData;
begin
  New(Result);
  InitQueueData(Result^);
end;

function StrToIPv4(const S: umlString; var Success: Boolean): TIPV4;
var
  n       : umlString;
  i       : Integer;
  DotCount: Integer;
  NumVal  : Integer;
  len     : Integer;
  ch      : Char;
begin
  FillPtrByte(@Result[0], SizeOf(Result), 0);
  Success := False;
  n := umlDeleteChar(S, [#32, #0, #9, #13, #10]);
  len := n.len;
  if len < 6 then
      Exit;
  DotCount := 0;
  NumVal := -1;
  for i := 1 to len do
    begin
      ch := n[i];
      if CharIn(ch, c0to9) then
        begin
          if NumVal < 0 then
              NumVal := Ord(ch) - Ord('0')
          else
              NumVal := NumVal * 10 + Ord(ch) - Ord('0');
          if NumVal > 255 then
              Exit;
        end
      else if ch = '.' then
        begin
          if (NumVal > -1) and (DotCount < 3) then
              Result[DotCount] := NumVal
          else
              Exit;
          Inc(DotCount);
          NumVal := -1;
        end
      else
          Exit;
    end;

  if (NumVal > -1) and (DotCount = 3) then
    begin
      Result[DotCount] := NumVal;
      Success := True;
    end;
end;

function IPv4ToStr(const AIcsIPv4Addr: TIPV4): umlString;
begin
  Result.Text := IntToStr(AIcsIPv4Addr[0]) + '.' + IntToStr(AIcsIPv4Addr[1]) + '.' + IntToStr(AIcsIPv4Addr[2]) + '.' + IntToStr(AIcsIPv4Addr[3]);
end;

function StrToIPv6(const S: umlString; var Success: Boolean; var ScopeID: Cardinal): TIPV6;
const
  Colon   = ':';
  Percent = '%';
var
  n        : umlString;
  ColonCnt : Integer;
  i        : Integer;
  NumVal   : Integer;
  ch       : Char;
  SLen     : Integer;
  OmitPos  : Integer;
  OmitCnt  : Integer;
  PartCnt  : Byte;
  ScopeFlag: Boolean;
begin
  FillPtrByte(@Result[0], SizeOf(Result), 0);
  Success := False;
  n := umlDeleteChar(S, [#32, #0, #9, #13, #10]);
  SLen := n.len;
  if (SLen < 1) or (SLen > (4 * 8) + 7) then
      Exit;
  ColonCnt := 0;
  for i := 1 to SLen do
    if (n[i] = Colon) then
        Inc(ColonCnt);
  if ColonCnt > 7 then
      Exit;
  OmitPos := n.GetPos('::') - 1;
  if OmitPos > -1 then
      OmitCnt := 8 - ColonCnt
  else begin
      OmitCnt := 0; // Make the compiler happy
      if (n.First = Colon) or (n.Last = Colon) then
          Exit;
    end;
  NumVal := -1;
  ColonCnt := 0;
  PartCnt := 0;
  i := 0;
  ScopeID := 0;
  ScopeFlag := False;
  while i < SLen do
    begin
      ch := n.buff[i];

      if ch = Percent then
        begin
          if ScopeFlag then
              Exit
          else
              ScopeFlag := True;

          PartCnt := 0;
          if NumVal > -1 then
            begin
              Result[ColonCnt] := NumVal;
              NumVal := -1;
            end;
        end
      else if ch = Colon then
        begin
          if ScopeFlag then
              Exit;
          PartCnt := 0;
          if NumVal > -1 then
            begin
              Result[ColonCnt] := NumVal;
              NumVal := -1;
            end;
          if (OmitPos = i) then
            begin
              Inc(ColonCnt, OmitCnt);
              Inc(i);
            end;
          Inc(ColonCnt);
          if ColonCnt > 7 then
              Exit;
        end
      else if CharIn(ch, c0to9) then
        begin
          Inc(PartCnt);
          if NumVal < 0 then
              NumVal := (Ord(ch) - Ord('0'))
          else if ScopeFlag then
              NumVal := NumVal * 10 + (Ord(ch) - Ord('0'))
          else
              NumVal := NumVal * 16 + (Ord(ch) - Ord('0'));
          if (NumVal > high(Word)) or (PartCnt > 4) then
              Exit;
        end
      else if CharIn(ch, cAtoZ) then
        begin
          if ScopeFlag then
              Exit;
          Inc(PartCnt);
          if NumVal < 0 then
              NumVal := ((Ord(ch) and 15) + 9)
          else
              NumVal := NumVal * 16 + ((Ord(ch) and 15) + 9);
          if (NumVal > high(Word)) or (PartCnt > 4) then
              Exit;
        end
      else
          Exit;

      Inc(i);
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

function IPv6ToStr(const IPv6Addr: TIPV6): umlString;
var
  i                   : Integer;
  Zeros1, Zeros2      : set of Byte;
  Zeros1Cnt, Zeros2Cnt: Byte;
  OmitFlag            : Boolean;
  ipv                 : SystemString;
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
          Inc(Zeros1Cnt);
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

function IsIPv4(const S: umlString): Boolean;
var
  n     : umlString;
  i     : Integer;
  DotCnt: Integer;
  NumVal: Integer;
  ch    : Char;
begin
  n := umlDeleteChar(S, [#32, #0, #9, #13, #10]);
  Result := False;
  DotCnt := 0;
  NumVal := -1;
  for i := 1 to n.len do
    begin
      ch := n[i];
      if CharIn(ch, c0to9) then
        begin
          if NumVal = -1 then
              NumVal := Ord(ch) - Ord('0')
          else
              NumVal := NumVal * 10 + Ord(ch) - Ord('0');
          if NumVal > 255 then
              Exit;
        end
      else if ch = '.' then
        begin
          Inc(DotCnt);
          if (DotCnt > 3) or (NumVal = -1) then
              Exit;
          NumVal := -1;
        end
      else
          Exit;
    end;

  Result := DotCnt = 3;
end;

function IsIPV6(const S: umlString): Boolean;
var
  ScopeID: Cardinal;
begin
  StrToIPv6(S, Result, ScopeID);
end;

function TranslateBindAddr(const Addr: SystemString): SystemString;
begin
  if Addr = '' then
      Result := 'IPv4+IPv6'
  else if Addr = '127.0.0.1' then
      Result := 'Local IPv4'
  else if Addr = '::1' then
      Result := 'Local IPv6'
  else if Addr = '0.0.0.0' then
      Result := 'All IPv4'
  else if Addr = '::' then
      Result := 'All IPv6'
  else if IsIPv4(Addr) then
      Result := 'Custom IPv4'
  else if IsIPV6(Addr) then
      Result := 'Custom IPv6'
  else
      Result := Addr;
end;

procedure SyncMethod(t: TCoreClassThread; Sync: Boolean; proc: TThreadMethod);
begin
  if Sync then
    begin
      TCoreClassThread.Synchronize(t, proc);
    end
  else
    begin
      try
          proc;
      except
      end;
    end;
end;

procedure DoExecuteResult(c: TPeerClient; QueuePtr: PQueueData; AResultText: SystemString; AResultDF: TDataFrameEngine);
var
  AInData: TDataFrameEngine;
begin
  if QueuePtr = nil then
      Exit;

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
        c.PrintCommand('execute console on proc cmd: %s', QueuePtr^.Cmd);
        try
            QueuePtr^.OnConsoleProc(c, AResultText);
        except
        end;
      end;
    {$ENDIF}
    if Assigned(QueuePtr^.OnStreamMethod) then
      begin
        c.PrintCommand('execute stream on result cmd: %s', QueuePtr^.Cmd);
        try
          AResultDF.Reader.Index := 0;
          QueuePtr^.OnStreamMethod(c, AResultDF);
        except
        end;
      end;
    if Assigned(QueuePtr^.OnStreamParamMethod) then
      begin
        c.PrintCommand('execute stream on param result cmd: %s', QueuePtr^.Cmd);
        try
          AResultDF.Reader.Index := 0;
          AInData := TDataFrameEngine.Create;
          QueuePtr^.StreamData.Position := 0;
          AInData.DecodeFrom(QueuePtr^.StreamData, True);
          QueuePtr^.OnStreamParamMethod(c, QueuePtr^.Param1, QueuePtr^.Param2, AInData, AResultDF);
          DisposeObject(AInData);
        except
        end;
      end;
    {$IFNDEF FPC}
    if Assigned(QueuePtr^.OnStreamProc) then
      begin
        c.PrintCommand('execute stream on proc cmd: %s', QueuePtr^.Cmd);
        try
          AResultDF.Reader.Index := 0;
          QueuePtr^.OnStreamProc(c, AResultDF);
        except
        end;
      end;
    if Assigned(QueuePtr^.OnStreamParamProc) then
      begin
        c.PrintCommand('execute stream on param proc cmd: %s', QueuePtr^.Cmd);
        try
          AResultDF.Reader.Index := 0;
          AInData := TDataFrameEngine.Create;
          QueuePtr^.StreamData.Position := 0;
          AInData.DecodeFrom(QueuePtr^.StreamData, True);
          QueuePtr^.OnStreamParamProc(c, QueuePtr^.Param1, QueuePtr^.Param2, AInData, AResultDF);
          DisposeObject(AInData);
        except
        end;
      end;
    {$ENDIF}
  finally
      c.FReceiveResultRuning := False;
  end;
end;

type
  TWaitSendConsoleCmdIntf = class(TCoreClassObject)
  public
    NewResult: SystemString;
    Done     : Boolean;
    constructor Create;
    procedure WaitSendConsoleResultEvent(Client: TPeerClient; ResultData: SystemString);
  end;

  TWaitSendStreamCmdIntf = class(TCoreClassObject)
  public
    NewResult: TDataFrameEngine;
    Done     : Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure WaitSendStreamResultEvent(Client: TPeerClient; ResultData: TDataFrameEngine);
  end;

constructor TWaitSendConsoleCmdIntf.Create;
begin
  NewResult := '';
  Done := False;
end;

procedure TWaitSendConsoleCmdIntf.WaitSendConsoleResultEvent(Client: TPeerClient; ResultData: SystemString);
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

procedure TWaitSendStreamCmdIntf.WaitSendStreamResultEvent(Client: TPeerClient; ResultData: TDataFrameEngine);
begin
  NewResult.Assign(ResultData);
  Done := True;
end;

{$IFNDEF FPC}


function WaitSendConsoleCmdInThread(th: TCoreClassThread; cf: TCommunicationFrameworkClient; Cmd: SystemString; ConsoleData: SystemString; TimeOut: TTimeTickValue): SystemString;
var
  waitIntf: TWaitSendConsoleCmdIntf;
  timetick: TTimeTickValue;
  r       : Boolean;
begin
  Result := '';
  if cf.ClientIO = nil then
      Exit;
  if not cf.Connected then
      Exit;

  r := True;
  TCoreClassThread.Synchronize(th,
    procedure
    begin
      r := cf.CanSendCommand(cf.ClientIO, Cmd);
    end);
  if not r then
      Exit;

  TCoreClassThread.Synchronize(th,
    procedure
    begin
      cf.ClientIO.PrintCommand('Begin Wait console cmd: %s', Cmd);
    end);

  timetick := GetTimeTickCount + TimeOut;
  while cf.ClientIO.WaitOnResult or cf.ClientIO.BigStreamProcessing do
    begin
      TCoreClassThread.Synchronize(th,
        procedure
        begin
          cf.ProgressBackground;
        end);

      if not cf.Connected then
          Exit;
      if (TimeOut > 0) and (GetTimeTickCount > timetick) then
          Exit;
      th.Sleep(1);
    end;

  try
    waitIntf := TWaitSendConsoleCmdIntf.Create;
    waitIntf.Done := False;
    waitIntf.NewResult := '';
    TCoreClassThread.Synchronize(th,
      procedure
      begin
        cf.SendConsoleCmd(Cmd, ConsoleData, waitIntf.WaitSendConsoleResultEvent);
      end);

    while not waitIntf.Done do
      begin
        TCoreClassThread.Synchronize(th,
          procedure
          begin
            cf.ProgressBackground;
          end);

        if not cf.Connected then
            break;

        if (TimeOut > 0) and (GetTimeTickCount > timetick) then
            break;
        th.Sleep(1);
      end;
    Result := waitIntf.NewResult;
    if waitIntf.Done then
        DisposeObject(waitIntf);

    TCoreClassThread.Synchronize(th,
      procedure
      begin
        cf.ClientIO.PrintCommand('End Wait console cmd: %s', Cmd);
      end);
  except
      Result := '';
  end;
end;

procedure WaitSendStreamCmdInThread(th: TCoreClassThread; cf: TCommunicationFrameworkClient; Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; TimeOut: TTimeTickValue);
var
  waitIntf: TWaitSendStreamCmdIntf;
  timetick: TTimeTickValue;
  r       : Boolean;
begin
  if cf.ClientIO = nil then
      Exit;
  if not cf.Connected then
      Exit;

  r := True;
  TCoreClassThread.Synchronize(th,
    procedure
    begin
      r := cf.CanSendCommand(cf.ClientIO, Cmd);
    end);
  if not r then
      Exit;

  TCoreClassThread.Synchronize(th,
    procedure
    begin
      cf.ClientIO.PrintCommand('Begin Wait Stream cmd: %s', Cmd);
    end);

  timetick := GetTimeTickCount + TimeOut;

  if cf.ClientIO.WaitOnResult then
    begin
      while cf.ClientIO.WaitOnResult or cf.ClientIO.BigStreamProcessing do
        begin
          TCoreClassThread.Synchronize(th,
            procedure
            begin
              cf.ProgressBackground;
            end);
          if not cf.Connected then
              Exit;
          if (TimeOut > 0) and (GetTimeTickCount > timetick) then
              Exit;
          th.Sleep(1);
        end;
    end;
  try
    waitIntf := TWaitSendStreamCmdIntf.Create;
    waitIntf.Done := False;

    TCoreClassThread.Synchronize(th,
      procedure
      begin
        cf.SendStreamCmd(Cmd, StreamData, waitIntf.WaitSendStreamResultEvent);
      end);

    while not waitIntf.Done do
      begin
        TCoreClassThread.Synchronize(th,
          procedure
          begin
            cf.ProgressBackground;
          end);
        if not cf.Connected then
            break;
        if (TimeOut > 0) and (GetTimeTickCount > timetick) then
            break;
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
        cf.ClientIO.PrintCommand('End Wait Stream cmd: %s', Cmd);
      end);
  except
  end;
end;
{$ENDIF}


constructor TCommandStreamMode.Create;
begin
  inherited Create;
  FCommand := '';

  FOnCommandStreamProc := nil;
end;

destructor TCommandStreamMode.Destroy;
begin
  inherited Destroy;
end;

function TCommandStreamMode.Execute(Sender: TPeerClient; InData, OutData: TDataFrameEngine): Boolean;
begin
  Result := Assigned(FOnCommandStreamProc);
  try
    if Result then
        FOnCommandStreamProc(Sender, InData, OutData);
  except
  end;
end;

constructor TCommandConsoleMode.Create;
begin
  inherited Create;
  FCommand := '';

  FOnCommandConsoleProc := nil;
end;

destructor TCommandConsoleMode.Destroy;
begin
  inherited Destroy;
end;

function TCommandConsoleMode.Execute(Sender: TPeerClient; InData: SystemString; var OutData: SystemString): Boolean;
begin
  Result := Assigned(FOnCommandConsoleProc);
  try
    if Result then
        FOnCommandConsoleProc(Sender, InData, OutData);
  except
  end;
end;

constructor TCommandDirectStreamMode.Create;
begin
  inherited Create;
  FCommand := '';

  FOnCommandDirectStreamProc := nil;
end;

destructor TCommandDirectStreamMode.Destroy;
begin
  inherited Destroy;
end;

function TCommandDirectStreamMode.Execute(Sender: TPeerClient; InData: TDataFrameEngine): Boolean;
begin
  Result := Assigned(FOnCommandDirectStreamProc);
  try
    if Result then
        FOnCommandDirectStreamProc(Sender, InData);
  except
  end;
end;

constructor TCommandDirectConsoleMode.Create;
begin
  inherited Create;
  FCommand := '';

  FOnCommandDirectConsoleProc := nil;
end;

destructor TCommandDirectConsoleMode.Destroy;
begin
  inherited Destroy;
end;

function TCommandDirectConsoleMode.Execute(Sender: TPeerClient; InData: SystemString): Boolean;
begin
  Result := Assigned(FOnCommandDirectConsoleProc);
  try
    if Result then
        FOnCommandDirectConsoleProc(Sender, InData);
  except
  end;
end;

constructor TCommandBigStreamMode.Create;
begin
  inherited Create;
  FCommand := '';

  FOnCommandBigStreamProc := nil;
end;

destructor TCommandBigStreamMode.Destroy;
begin
  inherited Destroy;
end;

function TCommandBigStreamMode.Execute(Sender: TPeerClient; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64): Boolean;
begin
  Result := Assigned(FOnCommandBigStreamProc);
  try
    if Result then
        FOnCommandBigStreamProc(Sender, InData, BigStreamTotal, BigStreamCompleteSize);
  except
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

function TBigStreamBatchList.GetItems(const Index: Integer): PBigStreamBatchPostData;
begin
  Result := PBigStreamBatchPostData(FList[index]);
end;

constructor TBigStreamBatchList.Create(AOwner: TPeerClient);
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
  New(Result);
  Result^.Init;
  Result^.Source := TMemoryStream64.Create;
  Result^.Index := FList.Add(Result);
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

procedure TBigStreamBatchList.Delete(const Index: Integer);
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
      p^.Index := i;
    end;
end;

procedure TBigStreamBatchList.RebuildMD5;
var
  p: PBigStreamBatchPostData;
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    begin
      p := PBigStreamBatchPostData(FList[i]);
      p^.SourceMD5 := umlStreamMD5(p^.Source);
    end;
end;

constructor TPeerClientUserDefine.Create(AOwner: TPeerClient);
begin
  inherited Create;
  FOwner := AOwner;
  FWorkPlatform := TExecutePlatform.epUnknow;
  FBigStreamBatchList := TBigStreamBatchList.Create(Owner);
end;

destructor TPeerClientUserDefine.Destroy;
begin
  inherited Destroy;
  DisposeObject(FBigStreamBatchList);
end;

procedure TPeerClientUserDefine.Progress;
begin
end;

constructor TPeerClientUserSpecial.Create(AOwner: TPeerClient);
begin
  inherited Create;
  FOwner := AOwner;
end;

destructor TPeerClientUserSpecial.Destroy;
begin
  inherited Destroy;
end;

procedure TPeerClientUserSpecial.Progress;
begin
end;

function TPeerClient.GetUserVariants: THashVariantList;
begin
  if FUserVariants = nil then
      FUserVariants := THashVariantList.Create;

  Result := FUserVariants;
end;

function TPeerClient.GetUserObjects: THashObjectList;
begin
  if FUserObjects = nil then
      FUserObjects := THashObjectList.Create(False);

  Result := FUserObjects;
end;

function TPeerClient.GetUserAutoFreeObjects: THashObjectList;
begin
  if FUserAutoFreeObjects = nil then
      FUserAutoFreeObjects := THashObjectList.Create(True);

  Result := FUserAutoFreeObjects;
end;

procedure TPeerClient.TriggerWrite64(Count: Int64);
begin
  Inc(FOwnerFramework.Statistics[TStatisticsType.stReceiveSize], Count);
end;

procedure TPeerClient.InternalSendByteBuffer(buff: PByte; Size: Integer);
const
  FlushBuffSize = 16 * 1024; // flush size = 16k byte
begin
  FLastCommunicationTimeTickCount := GetTimeTickCount;

  if Size < 1 then
      Exit;

  // fill fragment
  while Size > FlushBuffSize do
    begin
      SendByteBuffer(buff, FlushBuffSize);
      Inc(buff, FlushBuffSize);
      Inc(FOwnerFramework.Statistics[TStatisticsType.stSendSize], FlushBuffSize);
      WriteBufferFlush;
      dec(Size, FlushBuffSize);
    end;

  if Size > 0 then
    begin
      SendByteBuffer(buff, Size);
      Inc(FOwnerFramework.Statistics[TStatisticsType.stSendSize], Size);
    end;
end;

procedure TPeerClient.SendInteger(v: Integer);
begin
  InternalSendByteBuffer(@v, umlIntegerLength);
end;

procedure TPeerClient.SendCardinal(v: Cardinal);
begin
  InternalSendByteBuffer(@v, umlCardinalLength);
end;

procedure TPeerClient.SendInt64(v: Int64);
begin
  InternalSendByteBuffer(@v, umlInt64Length);
end;

procedure TPeerClient.SendByte(v: Byte);
begin
  InternalSendByteBuffer(@v, umlByteLength);
end;

procedure TPeerClient.SendWord(v: Word);
begin
  InternalSendByteBuffer(@v, umlWordLength);
end;

procedure TPeerClient.SendVerifyCode(buff: Pointer; siz: Integer);
var
  headBuff: array [0 .. 2] of Byte;
  code    : TBytes;
begin
  GenerateHashCode(FOwnerFramework.FHashStyle, buff, siz, code);

  headBuff[0] := Byte(FOwnerFramework.FHashStyle);
  PWord(@headBuff[1])^ := Length(code);
  InternalSendByteBuffer(@headBuff[0], 3);
  InternalSendByteBuffer(@code[0], Length(code));
end;

procedure TPeerClient.SendEncryptBuffer(buff: PByte; Size: Integer; cs: TCipherStyle);
begin
  SendByte(Byte(cs));
  Encrypt(cs, buff, Size, FCipherKey, True);
  InternalSendByteBuffer(buff, Size);
end;

procedure TPeerClient.SendEncryptMemoryStream(stream: TMemoryStream64; cs: TCipherStyle);
begin
  SendEncryptBuffer(stream.Memory, stream.Size, cs);
end;

procedure TPeerClient.InternalSendConsoleBuff(buff: TMemoryStream64; cs: TCipherStyle);
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

procedure TPeerClient.InternalSendStreamBuff(buff: TMemoryStream64; cs: TCipherStyle);
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

procedure TPeerClient.InternalSendDirectConsoleBuff(buff: TMemoryStream64; cs: TCipherStyle);
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

procedure TPeerClient.InternalSendDirectStreamBuff(buff: TMemoryStream64; cs: TCipherStyle);
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

procedure TPeerClient.InternalSendBigStreamHeader(Cmd: SystemString; streamSiz: Int64);
var
  buff: TBytes;
begin
  WriteBufferOpen;

  SendCardinal(FHeadToken);
  SendByte(FBigStreamToken);
  SendInt64(streamSiz);
  buff := TPascalString(Cmd).Bytes;
  SendCardinal(Cardinal(Length(buff)));
  InternalSendByteBuffer(@buff[0], Length(buff));
  SendCardinal(FTailToken);
  try
    WriteBufferFlush;
    WriteBufferClose;
  except
  end;
end;

procedure TPeerClient.InternalSendBigStreamBuff(var Queue: TQueueData);
const
  ChunkSize = 64 * 1024;
var
  StartPos, EndPos: Int64;
  tmpPos          : Int64;
  j               : Int64;
  Num             : Int64;
  Rest            : Int64;
  buff            : TBytes;
begin
  InternalSendBigStreamHeader(Queue.Cmd, Queue.BigStream.Size);

  WriteBufferOpen;

  StartPos := 0;
  EndPos := Queue.BigStream.Size;
  tmpPos := StartPos;
  { Calculate number of full chunks that will fit into the buffer }
  Num := EndPos div ChunkSize;
  { Calculate remaining bytes }
  Rest := EndPos mod ChunkSize;
  { init buffer }
  SetLength(buff, ChunkSize);
  { Process full chunks }
  for j := 0 to Num - 1 do
    begin
      if not Connected then
          Exit;

      LockObject(Queue.BigStream);
      try
        Queue.BigStream.Position := tmpPos;
        Queue.BigStream.Read(buff[0], ChunkSize);
        tmpPos := tmpPos + ChunkSize;
      except
      end;
      UnLockObject(Queue.BigStream);

      try
          InternalSendByteBuffer(@buff[0], ChunkSize);
      except
      end;

      try
          WriteBufferFlush;
      except
      end;

      if Queue.BigStream.Size - tmpPos > ChunkSize * 8 then
        begin
          try
              WriteBufferClose;
          except
          end;

          FBigStreamSending := Queue.BigStream;
          FBigStreamSendState := tmpPos;
          FBigStreamSendDoneTimeFree := Queue.DoneFreeStream;
          Queue.BigStream := nil;
          Exit;
        end;
    end;

  { Process remaining bytes }
  if Rest > 0 then
    begin
      LockObject(Queue.BigStream);
      try
        Queue.BigStream.Position := tmpPos;
        Queue.BigStream.Read(buff[0], Rest);
        tmpPos := tmpPos + Rest;
      except
      end;
      UnLockObject(Queue.BigStream);

      try
          InternalSendByteBuffer(@buff[0], Rest);
      except
      end;
    end;

  try
      WriteBufferFlush;
  except
  end;

  try
      WriteBufferClose;
  except
  end;
end;

procedure TPeerClient.Sync_InternalSendResultData;
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

procedure TPeerClient.Sync_InternalSendConsoleCmd;
var
  df    : TDataFrameEngine;
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
      Inc(FOwnerFramework.Statistics[TStatisticsType.stCompress]);
end;

procedure TPeerClient.Sync_InternalSendStreamCmd;
var
  df    : TDataFrameEngine;
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
      Inc(FOwnerFramework.Statistics[TStatisticsType.stCompress]);
end;

procedure TPeerClient.Sync_InternalSendDirectConsoleCmd;
var
  df    : TDataFrameEngine;
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
      Inc(FOwnerFramework.Statistics[TStatisticsType.stCompress]);
end;

procedure TPeerClient.Sync_InternalSendDirectStreamCmd;
var
  df    : TDataFrameEngine;
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
      Inc(FOwnerFramework.Statistics[TStatisticsType.stCompress]);
end;

procedure TPeerClient.Sync_InternalSendBigStreamCmd;
begin
  FSyncPick^.BigStream.Position := 0;
  InternalSendBigStreamBuff(FSyncPick^);
  Inc(FOwnerFramework.Statistics[TStatisticsType.stExecBigStream]);
end;

procedure TPeerClient.Sync_ExecuteConsole;
var
  d: TTimeTickValue;
begin
  FReceiveCommandRuning := True;
  PrintCommand('execute console cmd:%s', FInCmd);

  d := GetTimeTickCount;
  FOwnerFramework.ExecuteConsole(Self, FInCmd, FInText, FOutText);
  FReceiveCommandRuning := False;

  FOwnerFramework.CmdMaxExecuteConsumeStatistics.SetMax(FInCmd, GetTimeTickCount - d);

  Inc(FOwnerFramework.Statistics[TStatisticsType.stExecConsole]);
  FOwnerFramework.CmdRecvStatistics.IncValue(FInCmd, 1);
end;

procedure TPeerClient.Sync_ExecuteStream;
var
  d: TTimeTickValue;
begin
  FReceiveCommandRuning := True;
  PrintCommand('execute stream cmd:%s', FInCmd);

  d := GetTimeTickCount;
  FOwnerFramework.ExecuteStream(Self, FInCmd, FInDataFrame, FOutDataFrame);
  FReceiveCommandRuning := False;

  FOwnerFramework.CmdMaxExecuteConsumeStatistics.SetMax(FInCmd, GetTimeTickCount - d);

  Inc(FOwnerFramework.Statistics[TStatisticsType.stExecStream]);
  FOwnerFramework.CmdRecvStatistics.IncValue(FInCmd, 1);
end;

procedure TPeerClient.Sync_ExecuteDirectConsole;
var
  d: TTimeTickValue;
begin
  FReceiveCommandRuning := True;
  PrintCommand('execute direct console cmd:%s', FInCmd);

  d := GetTimeTickCount;
  FOwnerFramework.ExecuteDirectConsole(Self, FInCmd, FInText);
  FReceiveCommandRuning := False;

  FOwnerFramework.CmdMaxExecuteConsumeStatistics.SetMax(FInCmd, GetTimeTickCount - d);

  Inc(FOwnerFramework.Statistics[TStatisticsType.stExecDirestConsole]);
  FOwnerFramework.CmdRecvStatistics.IncValue(FInCmd, 1);
end;

procedure TPeerClient.Sync_ExecuteDirectStream;
var
  d: TTimeTickValue;
begin
  FReceiveCommandRuning := True;
  PrintCommand('execute direct stream cmd:%s', FInCmd);

  d := GetTimeTickCount;
  FOwnerFramework.ExecuteDirectStream(Self, FInCmd, FInDataFrame);
  FReceiveCommandRuning := False;

  FOwnerFramework.CmdMaxExecuteConsumeStatistics.SetMax(FInCmd, GetTimeTickCount - d);

  Inc(FOwnerFramework.Statistics[TStatisticsType.stExecDirestStream]);
  FOwnerFramework.CmdRecvStatistics.IncValue(FInCmd, 1);
end;

procedure TPeerClient.ExecuteDataFrame(ACurrentActiveThread: TCoreClassThread; const Sync: Boolean; CommDataType: Byte; dataFrame: TDataFrameEngine);
var
  m64 : TMemoryStream64;
  buff: TBytes;
begin
  FInCmd := dataFrame.Reader.ReadString;

  if CommDataType = FConsoleToken then
    begin
      FInText := dataFrame.Reader.ReadString;
      FOutText := '';

      FCanPauseResultSend := True;

      FRunReceiveTrigger := True;
      {$IFDEF FPC}
      SyncMethod(ACurrentActiveThread, Sync, @Sync_ExecuteConsole);
      {$ELSE}
      SyncMethod(ACurrentActiveThread, Sync, Sync_ExecuteConsole);
      {$ENDIF}
      FRunReceiveTrigger := False;

      FCanPauseResultSend := False;

      if FPauseResultSend then
        begin
          FCurrentPauseResultSend_CommDataType := CommDataType;
          Exit;
        end;
      if not Connected then
          Exit;

      buff := TPascalString(FOutText).Bytes;
      WriteBufferOpen;

      SendCardinal(FHeadToken);
      SendInteger(Length(buff));

      SendVerifyCode(@buff[0], Length(buff));

      SendEncryptBuffer(@buff[0], Length(buff), FReceiveDataCipherStyle);
      SendCardinal(FTailToken);

      WriteBufferFlush;
      WriteBufferClose;

      Inc(FOwnerFramework.Statistics[TStatisticsType.stResponse]);
    end
  else if CommDataType = FStreamToken then
    begin
      FInDataFrame.Clear;
      FOutDataFrame.Clear;
      dataFrame.Reader.ReadDataFrame(FInDataFrame);

      FCanPauseResultSend := True;

      FRunReceiveTrigger := True;
      {$IFDEF FPC}
      SyncMethod(ACurrentActiveThread, Sync, @Sync_ExecuteStream);
      {$ELSE}
      SyncMethod(ACurrentActiveThread, Sync, Sync_ExecuteStream);
      {$ENDIF}
      FRunReceiveTrigger := False;

      FCanPauseResultSend := False;

      if FPauseResultSend then
        begin
          FCurrentPauseResultSend_CommDataType := CommDataType;
          Exit;
        end;

      if not Connected then
          Exit;

      m64 := TMemoryStream64.Create;
      FOutDataFrame.EncodeTo(m64, True);

      WriteBufferOpen;
      SendCardinal(FHeadToken);
      SendInteger(m64.Size);

      SendVerifyCode(m64.Memory, m64.Size);

      SendEncryptBuffer(m64.Memory, m64.Size, FReceiveDataCipherStyle);
      SendCardinal(FTailToken);
      DisposeObject(m64);

      WriteBufferFlush;
      WriteBufferClose;
      Inc(FOwnerFramework.Statistics[TStatisticsType.stResponse]);
    end
  else if CommDataType = FDirectConsoleToken then
    begin
      FInText := dataFrame.Reader.ReadString;

      FRunReceiveTrigger := True;
      {$IFDEF FPC}
      SyncMethod(ACurrentActiveThread, Sync, @Sync_ExecuteDirectConsole);
      {$ELSE}
      SyncMethod(ACurrentActiveThread, Sync, Sync_ExecuteDirectConsole);
      {$ENDIF}
      FRunReceiveTrigger := False;

      if not Connected then
          Exit;
    end
  else if CommDataType = FDirectStreamToken then
    begin
      FInDataFrame.Clear;
      FOutDataFrame.Clear;
      dataFrame.Reader.ReadDataFrame(FInDataFrame);

      FRunReceiveTrigger := True;
      {$IFDEF FPC}
      SyncMethod(ACurrentActiveThread, Sync, @Sync_ExecuteDirectStream);
      {$ELSE}
      SyncMethod(ACurrentActiveThread, Sync, Sync_ExecuteDirectStream);
      {$ENDIF}
      FRunReceiveTrigger := False;

      if not Connected then
          Exit;
    end;
end;

procedure TPeerClient.Sync_ExecuteBigStream;
var
  d: TTimeTickValue;
begin
  FReceiveCommandRuning := True;
  d := GetTimeTickCount;
  FOwnerFramework.ExecuteBigStream(Self, FBigStreamCmd, FBigStreamReceive, FBigStreamTotal, FBigStreamCompleted);
  FReceiveCommandRuning := False;
  FOwnerFramework.CmdMaxExecuteConsumeStatistics.SetMax(FInCmd, GetTimeTickCount - d);

  FOwnerFramework.CmdRecvStatistics.IncValue(FBigStreamCmd, 1);
end;

function TPeerClient.FillBigStreamBuffer(ACurrentActiveThread: TCoreClassThread; const Sync: Boolean): Boolean;
var
  leftSize : Int64;
  tmpStream: TMemoryStream64OfWriteTrigger;
begin
  leftSize := FBigStreamTotal - FBigStreamCompleted;
  if leftSize > FReceivedBuffer.Size then
    begin
      FReceivedBuffer.Position := 0;
      FBigStreamCompleted := FBigStreamCompleted + FReceivedBuffer.Size;
      FBigStreamReceive := FReceivedBuffer;

      {$IFDEF FPC}
      SyncMethod(ACurrentActiveThread, Sync, @Sync_ExecuteBigStream);
      {$ELSE}
      SyncMethod(ACurrentActiveThread, Sync, Sync_ExecuteBigStream);
      {$ENDIF}
      FReceivedBuffer.Clear;
      Result := False;
    end
  else
    begin
      FReceivedBuffer.Position := 0;
      tmpStream := TMemoryStream64OfWriteTrigger.Create(nil);
      tmpStream.CopyFrom(FReceivedBuffer, leftSize);
      tmpStream.Position := 0;
      FBigStreamCompleted := FBigStreamTotal;
      FBigStreamReceive := tmpStream;

      {$IFDEF FPC}
      SyncMethod(ACurrentActiveThread, Sync, @Sync_ExecuteBigStream);
      {$ELSE}
      SyncMethod(ACurrentActiveThread, Sync, Sync_ExecuteBigStream);
      {$ENDIF}
      PrintCommand('execute Big Stream cmd:%s', FBigStreamCmd);

      tmpStream.Clear;
      if FReceivedBuffer.Size - FReceivedBuffer.Position > 0 then
          tmpStream.CopyFrom(FReceivedBuffer, FReceivedBuffer.Size - FReceivedBuffer.Position);
      DisposeObject(FReceivedBuffer);
      FReceivedBuffer := tmpStream;
      FReceivedBuffer.Trigger := Self;
      Result := True;

      FBigStreamTotal := 0;
      FBigStreamCompleted := 0;
      FBigStreamCmd := '';
      FBigStreamReceiveProcessing := False;

      FReceivedBuffer.Position := 0;
    end;
  FBigStreamReceive := nil;
end;

procedure TPeerClient.Sync_ExecuteResult;
var
  AInData: TDataFrameEngine;
  nQueue : PQueueData;
begin
  if FCurrentQueueData = nil then
      Exit;

  if (FOwnerFramework.FSyncOnResult) then
    begin
      DoExecuteResult(Self, FCurrentQueueData, ResultText, ResultDataFrame);
    end
  else
    begin
      New(nQueue);
      nQueue^ := FCurrentQueueData^;
      InitQueueData(FCurrentQueueData^);

      with FOwnerFramework.ProgressPost.PostExecute do
        begin
          DataEng.Assign(ResultDataFrame);
          Data1 := Self;
          Data5 := nQueue;
          Data3 := ResultText;
          {$IFDEF FPC}
          OnExecuteMethod := @FOwnerFramework.DelayExecuteOnResultState;
          {$ELSE}
          OnExecuteMethod := FOwnerFramework.DelayExecuteOnResultState;
          {$ENDIF}
        end;
    end;
end;

function TPeerClient.FillWaitOnResultBuffer(ACurrentActiveThread: TCoreClassThread; const RecvSync, SendSync: Boolean): Boolean;
var
  dHead, dTail: Cardinal;
  dSize       : Integer;
  dHashStyle  : THashStyle;
  dHashSiz    : Word;
  dHash       : TBytes;
  dCipherStyle: Byte;
  tmpStream   : TMemoryStream64OfWriteTrigger;
  buff        : TBytes;
begin
  Result := False;
  if not FWaitOnResult then
      Exit;
  if FCurrentQueueData = nil then
      Exit;

  FReceivedBuffer.Position := 0;

  // 0: head token
  if (FReceivedBuffer.Size - FReceivedBuffer.Position < umlCardinalLength) then
      Exit;
  FReceivedBuffer.Read(dHead, umlCardinalLength);
  if dHead <> FHeadToken then
    begin
      Disconnect;
      Exit;
    end;

  // 1: data len
  if (FReceivedBuffer.Size - FReceivedBuffer.Position < umlIntegerLength) then
      Exit;
  FReceivedBuffer.Read(dSize, umlIntegerLength);

  // 2:verify code header
  if (FReceivedBuffer.Size - FReceivedBuffer.Position < 3) then
      Exit;
  FReceivedBuffer.Read(dHashStyle, umlByteLength);
  FReceivedBuffer.Read(dHashSiz, umlWordLength);

  // 3:verify code body
  if (FReceivedBuffer.Size - FReceivedBuffer.Position < dHashSiz) then
      Exit;
  SetLength(dHash, dHashSiz);
  FReceivedBuffer.Read(dHash[0], dHashSiz);

  // 4: use Encrypt state
  if (FReceivedBuffer.Size - FReceivedBuffer.Position < umlByteLength) then
      Exit;
  FReceivedBuffer.Read(dCipherStyle, umlByteLength);

  // 5:process buff and tail token
  if (FReceivedBuffer.Size - FReceivedBuffer.Position < dSize + umlCardinalLength) then
      Exit;
  SetLength(buff, dSize);
  FReceivedBuffer.Read(buff[0], dSize);

  // 6: tail token
  FReceivedBuffer.Read(dTail, umlCardinalLength);
  if dTail <> FTailToken then
    begin
      Print('tail token error!');
      Disconnect;
      Exit;
    end;

  FReceiveDataCipherStyle := TCipherStyle(dCipherStyle);

  try
      Encrypt(FReceiveDataCipherStyle, @buff[0], dSize, FCipherKey, False);
  except
    Print('Encrypt error!');
    Disconnect;
    Exit;
  end;

  if not VerifyHashCode(dHashStyle, @buff[0], dSize, dHash) then
    begin
      Print('verify data error!');
      Disconnect;
      Exit;
    end;

  {$IFDEF FPC}
  if Assigned(FCurrentQueueData^.OnConsoleMethod) then
  {$ELSE}
  if Assigned(FCurrentQueueData^.OnConsoleMethod) or
    Assigned(FCurrentQueueData^.OnConsoleProc) then
    {$ENDIF}
    begin

      try
          ResultText := PascalStringOfBytes(buff).Text;
      except
        Print('data error!');
        Disconnect;
        Exit;
      end;

      {$IFDEF FPC}
      SyncMethod(ACurrentActiveThread, RecvSync, @Sync_ExecuteResult);
      {$ELSE}
      SyncMethod(ACurrentActiveThread, RecvSync, Sync_ExecuteResult);
      {$ENDIF}
      ResultText := '';

      Inc(FOwnerFramework.Statistics[TStatisticsType.stResponse]);
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
        except
          Print('data error!');
          Disconnect;
          Exit;
        end;

        {$IFDEF FPC}
        SyncMethod(ACurrentActiveThread, RecvSync, @Sync_ExecuteResult);
        {$ELSE}
        SyncMethod(ACurrentActiveThread, RecvSync, Sync_ExecuteResult);
        {$ENDIF}
        ResultDataFrame.Clear;

        Inc(FOwnerFramework.Statistics[TStatisticsType.stResponse]);
      end;

  // stripped stream
  tmpStream := TMemoryStream64OfWriteTrigger.Create(nil);
  if FReceivedBuffer.Size - FReceivedBuffer.Position > 0 then
      tmpStream.CopyFrom(FReceivedBuffer, FReceivedBuffer.Size - FReceivedBuffer.Position);
  DisposeObject(FReceivedBuffer);
  FReceivedBuffer := tmpStream;
  FReceivedBuffer.Trigger := Self;

  FWaitOnResult := False;
  DisposeQueueData(FCurrentQueueData);
  FCurrentQueueData := nil;

  FReceivedBuffer.Position := 0;
  Result := True;
end;

function TPeerClient.WriteBufferEmpty: Boolean;
begin
  Result := True;
end;

constructor TPeerClient.Create(AOwnerFramework: TCommunicationFramework; AClientIntf: TCoreClassObject);
var
  i   : Integer;
  kref: TInt64;
begin
  inherited Create;
  FOwnerFramework := AOwnerFramework;
  FClientIntf := AClientIntf;

  FID := AOwnerFramework.FIDCounter;

  // only ID
  Inc(AOwnerFramework.FIDCounter);
  while (AOwnerFramework.FIDCounter = 0) or (AOwnerFramework.FPerClientHashList.Exists(AOwnerFramework.FIDCounter)) do
      Inc(AOwnerFramework.FIDCounter);

  FHeadToken := c_DataHeadToken;
  FTailToken := c_DataTailToken;

  FConsoleToken := c_DefaultConsoleToken;
  FStreamToken := c_DefaultStreamToken;
  FDirectConsoleToken := c_DefaultDirectConsoleToken;
  FDirectStreamToken := c_DefaultDirectStreamToken;
  FBigStreamToken := c_DefaultBigStreamToken;

  FReceivedBuffer := TMemoryStream64OfWriteTrigger.Create(Self);
  FBigStreamReceiveProcessing := False;
  FBigStreamTotal := 0;
  FBigStreamCompleted := 0;
  FBigStreamCmd := '';
  FBigStreamReceive := nil;
  FBigStreamSending := nil;
  FBigStreamSendState := -1;
  FBigStreamSendDoneTimeFree := False;

  FCurrentQueueData := nil;
  FWaitOnResult := False;
  FPauseResultSend := False;
  FRunReceiveTrigger := False;
  FReceiveDataCipherStyle := TCipherStyle.csNone;
  FResultDataBuffer := TMemoryStream64.Create;
  FSendDataCipherStyle := FOwnerFramework.FCipherStyle;
  FCanPauseResultSend := False;
  FQueueList := TCoreClassList.Create;
  FLastCommunicationTimeTickCount := GetTimeTickCount;

  // generate random key
  TMISC.GenerateRandomKey(kref, umlInt64Length);
  TCipher.GenerateKey(FSendDataCipherStyle, @kref, umlInt64Length, FCipherKey);

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

  FUserData := nil;
  FUserValue := NULL;
  FUserVariants := nil;
  FUserObjects := nil;
  FUserAutoFreeObjects := nil;
  FUserDefine := FOwnerFramework.FPeerClientUserDefineClass.Create(Self);
  FUserSpecial := FOwnerFramework.FPeerClientUserSpecialClass.Create(Self);

  LockObject(FOwnerFramework.FPerClientHashList);
  FOwnerFramework.FPerClientHashList.Add(FID, Self, False);
  UnLockObject(FOwnerFramework.FPerClientHashList);

  Inc(FOwnerFramework.Statistics[TStatisticsType.stTriggerConnect]);

  FOwnerFramework.FOnPeerClientCreateNotify.ExecuteBackcall(Self, FID, NULL, NULL);
end;

destructor TPeerClient.Destroy;
var
  i: Integer;
begin
  if (FBigStreamSending <> nil) and (FBigStreamSendDoneTimeFree) then
    begin
      DisposeObject(FBigStreamSending);
      FBigStreamSending := nil;
    end;

  FOwnerFramework.FOnPeerClientDestroyNotify.ExecuteBackcall(Self, FID, NULL, NULL);

  Inc(FOwnerFramework.Statistics[TStatisticsType.stTriggerDisconnect]);

  LockObject(FOwnerFramework.FPerClientHashList);
  FOwnerFramework.FPerClientHashList.Delete(FID);
  UnLockObject(FOwnerFramework.FPerClientHashList);

  LockObject(FQueueList);
  for i := 0 to FQueueList.Count - 1 do
      DisposeQueueData(FQueueList[i]);
  FQueueList.Clear;
  UnLockObject(FQueueList);

  DisposeObject([FUserDefine, FUserSpecial]);

  DisposeObject(FQueueList);
  DisposeObject(FReceivedBuffer);
  DisposeObject(FResultDataBuffer);
  DisposeObject(FInDataFrame);
  DisposeObject(FOutDataFrame);
  DisposeObject(ResultDataFrame);

  if FUserVariants <> nil then
      DisposeObject(FUserVariants);
  if FUserObjects <> nil then
      DisposeObject(FUserObjects);
  if FUserAutoFreeObjects <> nil then
      DisposeObject(FUserAutoFreeObjects);
  inherited Destroy;
end;

procedure TPeerClient.Print(v: SystemString);
var
  n: SystemString;
begin
  n := GetPeerIP;
  if n <> '' then
      OwnerFramework.DoPrint(Format('%s %s %s', [n, DateTimeToStr(now), v]))
  else
      OwnerFramework.DoPrint(Format('%s %s', [DateTimeToStr(now), v]));
end;

procedure TPeerClient.Print(v: SystemString; const Args: array of const);
begin
  Print(Format(v, Args));
end;

procedure TPeerClient.PrintCommand(v: SystemString; Args: SystemString);
begin
  try
    if (OwnerFramework.FAllowPrintCommand) and (OwnerFramework.FPrintParams.GetDefaultValue(Args, True) = True) then
        Print(Format(v, [Args]))
    else
        Inc(OwnerFramework.Statistics[TStatisticsType.stPrint]);
  except
      Print(Format(v, [Args]));
  end;
end;

procedure TPeerClient.PrintParam(v: SystemString; Args: SystemString);
begin
  try
    if (OwnerFramework.FPrintParams.GetDefaultValue(Args, True) = True) then
        Print(Format(v, [Args]));
  except
      Print(Format(v, [Args]));
  end;
end;

procedure TPeerClient.Progress;
var
  SendBufferSize: Integer;
  buff          : TBytes;
  SendDone      : Boolean;
begin
  try
      FUserDefine.Progress;
  except
  end;

  try
      FUserSpecial.Progress;
  except
  end;

  if FAllSendProcessing then
      Exit;
  if FReceiveProcessing then
      Exit;

  if (FBigStreamSending <> nil) and (WriteBufferEmpty) then
    begin
      SendBufferSize := 1 * 1024 * 1024; // cycle send size 1M

      LockObject(FBigStreamSending);

      try
        SendDone := FBigStreamSending.Size - FBigStreamSendState <= SendBufferSize;

        if SendDone then
            SendBufferSize := FBigStreamSending.Size - FBigStreamSendState;

        SetLength(buff, SendBufferSize);
        FBigStreamSending.Position := FBigStreamSendState;
        FBigStreamSending.Read(buff[0], SendBufferSize);

        Inc(FBigStreamSendState, SendBufferSize);

      except
        UnLockObject(FBigStreamSending);
        Disconnect;
        Exit;
      end;

      UnLockObject(FBigStreamSending);

      try
        WriteBufferOpen;
        InternalSendByteBuffer(@buff[0], SendBufferSize);
        WriteBufferFlush;
        WriteBufferClose;
      except
        Disconnect;
        Exit;
      end;

      if SendDone then
        begin
          if FBigStreamSendDoneTimeFree then
              DisposeObject(FBigStreamSending);
          FBigStreamSending := nil;
          FBigStreamSendState := -1;
          FBigStreamSendDoneTimeFree := False;
          ProcessAllSendCmd(nil, False, False);
          FillRecvBuffer(nil, False, False);
        end;
    end;
end;

procedure TPeerClient.FillRecvBuffer(ACurrentActiveThread: TCoreClassThread; const RecvSync, SendSync: Boolean);
var
  dHead, dTail: Cardinal;
  dID         : Byte;
  dSize       : Cardinal;
  dHashStyle  : THashStyle;
  dHashSiz    : Word;
  dHash       : TBytes;
  dCipherStyle: Byte;
  tmpStream   : TMemoryStream64OfWriteTrigger;
  df          : TDataFrameEngine;
  buff        : TBytes;
  Total       : Int64;
begin
  if FAllSendProcessing then
      Exit;
  if FReceiveProcessing then
      Exit;
  if FPauseResultSend then
      Exit;
  if FResultDataBuffer.Size > 0 then
      Exit;
  if FRunReceiveTrigger then
      Exit;
  if FBigStreamSending <> nil then
      Exit;

  FReceiveProcessing := True;
  try
    while (FReceivedBuffer.Size > 0) and (Connected) do
      begin

        FLastCommunicationTimeTickCount := GetTimeTickCount;

        FReceivedBuffer.Position := 0;

        if FWaitOnResult then
          begin
            if FillWaitOnResultBuffer(ACurrentActiveThread, RecvSync, SendSync) then
                Continue
            else
                break;
          end;

        if FBigStreamReceiveProcessing then
          begin
            if FillBigStreamBuffer(ACurrentActiveThread, RecvSync) then
                Continue
            else
                break;
          end;

        // 0: head token
        if (FReceivedBuffer.Size - FReceivedBuffer.Position < umlCardinalLength) then
            break;
        FReceivedBuffer.Read(dHead, umlCardinalLength);
        if dHead <> FHeadToken then
          begin
            Disconnect;
            break;
          end;

        // 1: data type
        if (FReceivedBuffer.Size - FReceivedBuffer.Position < umlByteLength) then
            break;
        FReceivedBuffer.Read(dID, umlByteLength);

        if dID in [FConsoleToken, FStreamToken, FDirectConsoleToken, FDirectStreamToken] then
          begin
            // 2: size
            if (FReceivedBuffer.Size - FReceivedBuffer.Position < umlCardinalLength) then
                break;
            FReceivedBuffer.Read(dSize, umlCardinalLength);

            // 3:verify code header
            if (FReceivedBuffer.Size - FReceivedBuffer.Position < 3) then
                break;
            FReceivedBuffer.Read(dHashStyle, umlByteLength);
            FReceivedBuffer.Read(dHashSiz, umlWordLength);

            // 4:verify code body
            if (FReceivedBuffer.Size - FReceivedBuffer.Position < dHashSiz) then
                break;
            SetLength(dHash, dHashSiz);
            FReceivedBuffer.Read(dHash[0], dHashSiz);

            // 5: Encrypt style
            if (FReceivedBuffer.Size - FReceivedBuffer.Position < umlByteLength) then
                break;
            FReceivedBuffer.Read(dCipherStyle, umlByteLength);

            // 6: process stream
            if (FReceivedBuffer.Size - FReceivedBuffer.Position < dSize + umlCardinalLength) then
                break;
            tmpStream := TMemoryStream64OfWriteTrigger.Create(nil);
            tmpStream.CopyFrom(FReceivedBuffer, dSize);

            // 7: process tail token
            FReceivedBuffer.Read(dTail, umlCardinalLength);
            if dTail <> FTailToken then
              begin
                Print('tail error!');
                Disconnect;
                break;
              end;

            FReceiveDataCipherStyle := TCipherStyle(dCipherStyle);

            try
                Encrypt(FReceiveDataCipherStyle, tmpStream.Memory, tmpStream.Size, FCipherKey, False);
            except
              Print('Encrypt error!');
              DisposeObject(tmpStream);
              Disconnect;
              break;
            end;

            if not VerifyHashCode(dHashStyle, tmpStream.Memory, tmpStream.Size, dHash) then
              begin
                Print('verify data error!');
                DisposeObject(tmpStream);
                Disconnect;
                break;
              end;

            df := TDataFrameEngine.Create;
            tmpStream.Position := 0;
            try
                df.DecodeFrom(tmpStream, True);
            except
              Print('DECode dataFrame error!');
              DisposeObject(tmpStream);
              Disconnect;
              break;
            end;
            DisposeObject(tmpStream);

            // stripped stream
            tmpStream := TMemoryStream64OfWriteTrigger.Create(nil);
            if FReceivedBuffer.Size - FReceivedBuffer.Position > 0 then
                tmpStream.CopyFrom(FReceivedBuffer, FReceivedBuffer.Size - FReceivedBuffer.Position);
            DisposeObject(FReceivedBuffer);
            FReceivedBuffer := tmpStream;
            FReceivedBuffer.Trigger := Self;

            try
                ExecuteDataFrame(ACurrentActiveThread, RecvSync, dID, df);
            except
            end;
            DisposeObject(df);

            Inc(FOwnerFramework.Statistics[TStatisticsType.stRequest]);
          end
        else if dID = FBigStreamToken then
          begin
            // 2:stream size
            if (FReceivedBuffer.Size - FReceivedBuffer.Position < umlInt64Length) then
                break;
            FReceivedBuffer.Read(Total, umlInt64Length);

            // 3:command len
            if (FReceivedBuffer.Size - FReceivedBuffer.Position < umlCardinalLength) then
                break;
            FReceivedBuffer.Read(dSize, umlCardinalLength);

            // 4:command and tial token
            if (FReceivedBuffer.Size - FReceivedBuffer.Position < dSize + umlCardinalLength) then
                break;
            SetLength(buff, dSize);
            FReceivedBuffer.Read(buff[0], dSize);

            // 5: process tail token
            FReceivedBuffer.Read(dTail, umlCardinalLength);
            if dTail <> FTailToken then
              begin
                Print('tail error!');
                Disconnect;
                break;
              end;

            FBigStreamTotal := Total;
            FBigStreamCompleted := 0;
            FBigStreamCmd := PascalStringOfBytes(buff).Text;
            FBigStreamReceiveProcessing := True;
            SetLength(buff, 0);

            // stripped stream
            tmpStream := TMemoryStream64OfWriteTrigger.Create(nil);
            if FReceivedBuffer.Size - FReceivedBuffer.Position > 0 then
                tmpStream.CopyFrom(FReceivedBuffer, FReceivedBuffer.Size - FReceivedBuffer.Position);
            DisposeObject(FReceivedBuffer);
            FReceivedBuffer := tmpStream;
            FReceivedBuffer.Trigger := Self;

            Inc(FOwnerFramework.Statistics[TStatisticsType.stReceiveBigStream]);
          end
        else
          begin
            Disconnect;
            break;
          end;
      end;
  finally
    FReceivedBuffer.Position := FReceivedBuffer.Size;
    FReceiveProcessing := False;
  end;
end;

procedure TPeerClient.PostQueueData(p: PQueueData);
begin
  FOwnerFramework.CmdSendStatistics.IncValue(p^.Cmd, 1);

  LockObject(FQueueList);
  FQueueList.Add(p);
  UnLockObject(FQueueList);
end;

function TPeerClient.ProcessAllSendCmd(ACurrentActiveThread: TCoreClassThread; const RecvSync, SendSync: Boolean): Integer;
var
  p: PQueueData;
begin
  Result := 0;
  if not Connected then
      Exit;

  if FAllSendProcessing then
      Exit;
  if FReceiveProcessing then
      Exit;
  if FWaitOnResult then
      Exit;
  if FBigStreamReceiveProcessing then
      Exit;
  if FBigStreamSending <> nil then
      Exit;

  if FResultDataBuffer.Size > 0 then
    begin
      {$IFDEF FPC}
      SyncMethod(ACurrentActiveThread, SendSync, @Sync_InternalSendResultData);
      {$ELSE}
      SyncMethod(ACurrentActiveThread, SendSync, Sync_InternalSendResultData);
      {$ENDIF}
    end;

  if FRunReceiveTrigger then
      Exit;

  FAllSendProcessing := True;

  LockObject(FQueueList);
  try
    while FQueueList.Count > 0 do
      begin
        if not Connected then
            break;
        if FWaitOnResult then
            break;
        p := FQueueList[0];
        FCurrentQueueData := p;
        case p^.State of
          qsSendConsoleCMD:
            begin
              Inc(FOwnerFramework.Statistics[TStatisticsType.stConsole]);

              FSyncPick := p;
              // wait result
              FWaitOnResult := True;
              {$IFDEF FPC}
              SyncMethod(ACurrentActiveThread, SendSync, @Sync_InternalSendConsoleCmd);
              {$ELSE}
              SyncMethod(ACurrentActiveThread, SendSync, Sync_InternalSendConsoleCmd);
              {$ENDIF}
              FSyncPick := nil;

              FQueueList.Delete(0);
              Inc(Result);
              break;
            end;
          qsSendStreamCMD:
            begin
              Inc(FOwnerFramework.Statistics[TStatisticsType.stStream]);

              FSyncPick := p;

              // wait result
              FWaitOnResult := True;

              {$IFDEF FPC}
              SyncMethod(ACurrentActiveThread, SendSync, @Sync_InternalSendStreamCmd);
              {$ELSE}
              SyncMethod(ACurrentActiveThread, SendSync, Sync_InternalSendStreamCmd);
              {$ENDIF}
              FSyncPick := nil;

              FQueueList.Delete(0);
              Inc(Result);
              break;
            end;
          qsSendDirectConsoleCMD:
            begin
              Inc(FOwnerFramework.Statistics[TStatisticsType.stDirestConsole]);

              FSyncPick := p;
              {$IFDEF FPC}
              SyncMethod(ACurrentActiveThread, SendSync, @Sync_InternalSendDirectConsoleCmd);
              {$ELSE}
              SyncMethod(ACurrentActiveThread, SendSync, Sync_InternalSendDirectConsoleCmd);
              {$ENDIF}
              FSyncPick := nil;

              DisposeQueueData(p);
              FQueueList.Delete(0);
              Inc(Result);
            end;
          qsSendDirectStreamCMD:
            begin
              Inc(FOwnerFramework.Statistics[TStatisticsType.stDirestStream]);

              FSyncPick := p;
              {$IFDEF FPC}
              SyncMethod(ACurrentActiveThread, SendSync, @Sync_InternalSendDirectStreamCmd);
              {$ELSE}
              SyncMethod(ACurrentActiveThread, SendSync, Sync_InternalSendDirectStreamCmd);
              {$ENDIF}
              FSyncPick := nil;

              DisposeQueueData(p);
              FQueueList.Delete(0);
              Inc(Result);
            end;
          qsSendBigStream:
            begin
              Inc(FOwnerFramework.Statistics[TStatisticsType.stSendBigStream]);

              FSyncPick := p;
              {$IFDEF FPC}
              SyncMethod(ACurrentActiveThread, SendSync, @Sync_InternalSendBigStreamCmd);
              {$ELSE}
              SyncMethod(ACurrentActiveThread, SendSync, Sync_InternalSendBigStreamCmd);
              {$ENDIF}
              FSyncPick := nil;

              DisposeQueueData(p);
              FQueueList.Delete(0);
              Inc(Result);

              if FBigStreamSending <> nil then
                  break;
            end;
        end;
      end;
  finally
    UnLockObject(FQueueList);
    FAllSendProcessing := False;
  end;
end;

procedure TPeerClient.PauseResultSend;
begin
  if FCanPauseResultSend then
    begin
      FPauseResultSend := True;
      Inc(FOwnerFramework.Statistics[TStatisticsType.stPause]);
    end;
end;

procedure TPeerClient.ContinueResultSend;
var
  headBuff    : array [0 .. 2] of Byte;
  b           : TBytes;
  buff        : TMemoryStream64;
  dHead, dTail: Cardinal;
  len         : Integer;
  code        : TBytes;
  bCipherStyle: Byte;
begin
  if not FPauseResultSend then
      Exit;
  if FResultDataBuffer.Size > 0 then
      Exit;

  Inc(FOwnerFramework.Statistics[TStatisticsType.stContinue]);

  if FCurrentPauseResultSend_CommDataType in [FConsoleToken, FStreamToken] then
    begin
      buff := TMemoryStream64.Create;

      if FCurrentPauseResultSend_CommDataType = FConsoleToken then
        begin
          b := TPascalString(FOutText).Bytes;
          buff.WritePtr(@b[0], Length(b));
        end
      else
          FOutDataFrame.EncodeTo(buff, True);

      dHead := FHeadToken;
      dTail := FTailToken;
      len := buff.Size;

      // generate hash source
      GenerateHashCode(FOwnerFramework.FHashStyle, buff.Memory, buff.Size, code);
      headBuff[0] := Byte(FOwnerFramework.FHashStyle);
      PWord(@headBuff[1])^ := Length(code);

      // generate encrypt data body
      bCipherStyle := Byte(FReceiveDataCipherStyle);
      Encrypt(FReceiveDataCipherStyle, buff.Memory, buff.Size, FCipherKey, True);

      // result data header
      FResultDataBuffer.WritePtr(@dHead, umlCardinalLength);
      FResultDataBuffer.WritePtr(@len, umlIntegerLength);

      // verify code
      FResultDataBuffer.WritePtr(@headBuff[0], 3);
      FResultDataBuffer.WritePtr(@code[0], Length(code));

      // data body
      FResultDataBuffer.WritePtr(@bCipherStyle, umlByteLength);
      FResultDataBuffer.WritePtr(buff.Memory, len);

      // data tail
      FResultDataBuffer.WritePtr(@dTail, umlCardinalLength);

      DisposeObject(buff);

      Inc(FOwnerFramework.Statistics[TStatisticsType.stResponse]);
    end;
  FPauseResultSend := False;
end;

function TPeerClient.ResultSendIsPaused: Boolean;
begin
  Result := FPauseResultSend;
end;

function TPeerClient.BigStreamIsSending: Boolean;
begin
  Result := (FBigStreamSending <> nil);
end;

function TPeerClient.CipherKeyPtr: PCipherKeyBuffer;
begin
  Result := @FCipherKey;
end;

procedure TPeerClient.GenerateHashCode(hs: THashStyle; buff: Pointer; siz: Integer; var output: TBytes);
begin
  TCipher.GenerateHashByte(hs, buff, siz, output);
  Inc(FOwnerFramework.Statistics[TStatisticsType.stGenerateHash]);
end;

function TPeerClient.VerifyHashCode(hs: THashStyle; buff: Pointer; siz: Integer; var code: TBytes): Boolean;
var
  buffCode: TBytes;
begin
  try
    GenerateHashCode(hs, buff, siz, buffCode);
    Result := TCipher.CompareHash(buffCode, code);
  except
      Result := False;
  end;
end;

procedure TPeerClient.Encrypt(cs: TCipherStyle; DataPtr: Pointer; Size: Cardinal; var k: TCipherKeyBuffer; Enc: Boolean);
begin
  if FOwnerFramework.FUsedParallelEncrypt then
      SequEncryptCBC(cs, DataPtr, Size, k, Enc, True)
  else
      SequEncryptCBCWithDirect(cs, DataPtr, Size, k, Enc, True);

  if cs <> TCipherStyle.csNone then
      Inc(FOwnerFramework.Statistics[TStatisticsType.stEncrypt]);
end;

function TPeerClient.StopCommunicationTime: TTimeTickValue;
begin
  Result := GetTimeTickCount - FLastCommunicationTimeTickCount;
end;

procedure TPeerClient.SetLastCommunicationTimeAsCurrent;
begin
  FLastCommunicationTimeTickCount := GetTimeTickCount;
end;

procedure TPeerClient.SendConsoleCmd(Cmd: SystemString; ConsoleData: SystemString; OnResult: TConsoleMethod);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendConsoleCmd(Self, Cmd, ConsoleData, OnResult)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendConsoleCmd(Cmd, ConsoleData, OnResult);
end;

procedure TPeerClient.SendStreamCmd(Cmd: SystemString; StreamData: TCoreClassStream; OnResult: TStreamMethod; DoneFreeStream: Boolean);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendStreamCmd(Self, Cmd, StreamData, OnResult, DoneFreeStream)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendStreamCmd(Cmd, StreamData, OnResult, DoneFreeStream);
end;

procedure TPeerClient.SendStreamCmd(Cmd: SystemString; StreamData: TDataFrameEngine; OnResult: TStreamMethod);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendStreamCmd(Self, Cmd, StreamData, OnResult)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendStreamCmd(Cmd, StreamData, OnResult);
end;

procedure TPeerClient.SendStreamCmd(Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamMethod);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendStreamCmd(Self, Cmd, StreamData, Param1, Param2, OnResult)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendStreamCmd(Cmd, StreamData, Param1, Param2, OnResult);
end;

{$IFNDEF FPC}


procedure TPeerClient.SendConsoleCmd(Cmd: SystemString; ConsoleData: SystemString; OnResult: TConsoleProc);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendConsoleCmd(Self, Cmd, ConsoleData, OnResult)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendConsoleCmd(Cmd, ConsoleData, OnResult);
end;

procedure TPeerClient.SendStreamCmd(Cmd: SystemString; StreamData: TCoreClassStream; OnResult: TStreamProc; DoneFreeStream: Boolean);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendStreamCmd(Self, Cmd, StreamData, OnResult, DoneFreeStream)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendStreamCmd(Cmd, StreamData, OnResult, DoneFreeStream);
end;

procedure TPeerClient.SendStreamCmd(Cmd: SystemString; StreamData: TDataFrameEngine; OnResult: TStreamProc);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendStreamCmd(Self, Cmd, StreamData, OnResult)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendStreamCmd(Cmd, StreamData, OnResult);
end;

procedure TPeerClient.SendStreamCmd(Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamProc);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendStreamCmd(Self, Cmd, StreamData, Param1, Param2, OnResult)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendStreamCmd(Cmd, StreamData, Param1, Param2, OnResult);
end;
{$ENDIF}


procedure TPeerClient.SendDirectConsoleCmd(Cmd: SystemString; ConsoleData: SystemString);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendDirectConsoleCmd(Self, Cmd, ConsoleData)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendDirectConsoleCmd(Cmd, ConsoleData);
end;

procedure TPeerClient.SendDirectStreamCmd(Cmd: SystemString; StreamData: TCoreClassStream; DoneFreeStream: Boolean);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendDirectStreamCmd(Self, Cmd, StreamData, DoneFreeStream)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendDirectStreamCmd(Cmd, StreamData, DoneFreeStream);
end;

procedure TPeerClient.SendDirectStreamCmd(Cmd: SystemString; StreamData: TDataFrameEngine);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendDirectStreamCmd(Self, Cmd, StreamData)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendDirectStreamCmd(Cmd, StreamData);
end;

procedure TPeerClient.SendDirectStreamCmd(Cmd: SystemString);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendDirectStreamCmd(Self, Cmd)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendDirectStreamCmd(Cmd);
end;

function TPeerClient.WaitSendConsoleCmd(Cmd: SystemString; ConsoleData: SystemString; TimeOut: TTimeTickValue): SystemString;
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      Result := TCommunicationFrameworkServer(FOwnerFramework).WaitSendConsoleCmd(Self, Cmd, ConsoleData, TimeOut)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      Result := TCommunicationFrameworkClient(FOwnerFramework).WaitSendConsoleCmd(Cmd, ConsoleData, TimeOut)
  else
      Result := '';
end;

procedure TPeerClient.WaitSendStreamCmd(Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; TimeOut: TTimeTickValue);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).WaitSendStreamCmd(Self, Cmd, StreamData, ResultData, TimeOut)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).WaitSendStreamCmd(Cmd, StreamData, ResultData, TimeOut);
end;

procedure TPeerClient.SendBigStream(Cmd: SystemString; BigStream: TCoreClassStream; DoneFreeStream: Boolean);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendBigStream(Self, Cmd, BigStream, DoneFreeStream)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendBigStream(Cmd, BigStream, DoneFreeStream);
end;

procedure TCommunicationFramework.DoPrint(const v: SystemString);
begin
  DoStatus(v, c_DefaultDoStatusID);
  Inc(Statistics[TStatisticsType.stPrint]);
end;

function TCommunicationFramework.GetIdleTimeout: TTimeTickValue;
begin
  Result := FIdleTimeout;
end;

procedure TCommunicationFramework.SetIdleTimeout(const Value: TTimeTickValue);
begin
  FIdleTimeout := Value;
end;

procedure TCommunicationFramework.DoConnected(Sender: TPeerClient);
begin
  if Assigned(FOnConnected) then
      FOnConnected(Sender);
end;

procedure TCommunicationFramework.DoDisconnect(Sender: TPeerClient);
begin
  if Assigned(FOnDisconnect) then
      FOnDisconnect(Sender);
end;

function TCommunicationFramework.CanExecuteCommand(Sender: TPeerClient; Cmd: SystemString): Boolean;
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
      Inc(Statistics[TStatisticsType.stTotalCommandExecute]);
end;

function TCommunicationFramework.CanSendCommand(Sender: TPeerClient; Cmd: SystemString): Boolean;
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
      Inc(Statistics[TStatisticsType.stTotalCommandSend]);
end;

function TCommunicationFramework.CanRegCommand(Sender: TCommunicationFramework; Cmd: SystemString): Boolean;
begin
  Result := True;
  Inc(Statistics[TStatisticsType.stTotalCommandReg]);
end;

procedure TCommunicationFramework.DelayExecuteOnResultState(Sender: TNPostExecute);
var
  cli   : TPeerClient;
  nQueue: PQueueData;
begin
  cli := TPeerClient(Sender.Data1);
  nQueue := PQueueData(Sender.Data5);

  if FPerClientHashList.ExistsObject(cli) then
      DoExecuteResult(cli, nQueue, Sender.Data3, Sender.DataEng);

  DisposeQueueData(nQueue);
end;

constructor TCommunicationFramework.Create;
var
  st: TStatisticsType;
begin
  inherited Create;
  FCommandList := THashObjectList.Create(True, 1024);
  FIDCounter := 1;
  FPerClientHashList := TUInt32HashObjectList.Create(1024);
  FPerClientHashList.AutoFreeData := False;
  FPerClientHashList.AccessOptimization := False;
  FOnConnected := nil;
  FOnDisconnect := nil;
  FOnExecuteCommand := nil;
  FOnSendCommand := nil;
  FIdleTimeout := 0;
  FUsedParallelEncrypt := True;
  FSyncOnResult := False;
  FAllowPrintCommand := True;
  FCipherStyle := TCipherStyle.csNone;
  FSendDataCompressed := True;
  FHashStyle := THashStyle.hsNone;
  FPeerClientUserDefineClass := TPeerClientUserDefine;
  FPeerClientUserSpecialClass := TPeerClientUserSpecial;

  FPrintParams := THashVariantList.Create(1024);
  FPrintParams.AutoUpdateDefaultValue := True;

  FProgressPost := TNProgressPostWithCadencer.Create;

  FOnPeerClientCreateNotify := TBackcalls.Create;
  FOnPeerClientDestroyNotify := TBackcalls.Create;

  for st := low(TStatisticsType) to high(TStatisticsType) do
      Statistics[st] := 0;
  CmdRecvStatistics := THashVariantList.Create(32);
  CmdSendStatistics := THashVariantList.Create(32);
  CmdMaxExecuteConsumeStatistics := THashVariantList.Create(32);

  SwitchDefaultPerformance;
end;

destructor TCommunicationFramework.Destroy;
begin
  DisposeObject(FCommandList);
  DisposeObject(FPerClientHashList);
  DisposeObject(FPrintParams);
  DisposeObject(FProgressPost);
  DisposeObject([FOnPeerClientCreateNotify, FOnPeerClientDestroyNotify]);
  DisposeObject([CmdRecvStatistics, CmdSendStatistics, CmdMaxExecuteConsumeStatistics]);
  inherited Destroy;
end;

procedure TCommunicationFramework.SwitchMaxPerformance;
begin
  FUsedParallelEncrypt := False;
  FHashStyle := THashStyle.hsNone;
  FSendDataCompressed := False;
  FCipherStyle := TCipherStyle.csNone;
end;

procedure TCommunicationFramework.SwitchMaxSafe;
begin
  FUsedParallelEncrypt := True;
  FHashStyle := THashStyle.hsSHA1;
  FSendDataCompressed := True;
  FCipherStyle := TCipherStyle.csDES192;
end;

procedure TCommunicationFramework.SwitchDefaultPerformance;
begin
  FUsedParallelEncrypt := True;
  FHashStyle := THashStyle.hsFastMD5;
  FSendDataCompressed := True;
  FCipherStyle := TCipherStyle.csBlowfish;
end;

procedure TCommunicationFramework.LockClients;
begin
  LockObject(FPerClientHashList);
  Inc(Statistics[TStatisticsType.stLock]);
end;

procedure TCommunicationFramework.UnLockClients;
begin
  UnLockObject(FPerClientHashList);
  Inc(Statistics[TStatisticsType.stUnLock]);
end;

procedure TCommunicationFramework.ProgressBackground;
var
  i: Integer;
  p: PUInt32HashListObjectStruct;
begin
  try
    if Assigned(ProgressBackgroundProc) then
        ProgressBackgroundProc;
  except
  end;

  if (FPerClientHashList.Count > 0) then
    begin
      i := 0;
      p := FPerClientHashList.FirstPtr;
      while i < FPerClientHashList.Count do
        begin
          try
              TPeerClient(p^.Data).Progress;
          except
          end;
          Inc(i);
          p := p^.next;
        end;
    end;

  try
      ProgressPost.Progress;
  except
  end;

  Statistics[TStatisticsType.stIDCounter] := FIDCounter;
end;

procedure TCommunicationFramework.ProgressPerClient(OnProgress: TPerClientListCall);
var
  IDPool: TClientIDPool;
  pcid  : Cardinal;
  c     : TPeerClient;
begin
  if (FPerClientHashList.Count > 0) and (Assigned(OnProgress)) then
    begin
      GetClientIDPool(IDPool);
      for pcid in IDPool do
        begin
          c := TPeerClient(FPerClientHashList[pcid]);
          if c <> nil then
            begin
              try
                  OnProgress(c);
              except
              end;
            end;
        end;
    end;
end;

procedure TCommunicationFramework.ProgressPerClient(OnProgress: TPerClientListMethod);
var
  IDPool: TClientIDPool;
  pcid  : Cardinal;
  c     : TPeerClient;
begin
  if (FPerClientHashList.Count > 0) and (Assigned(OnProgress)) then
    begin
      GetClientIDPool(IDPool);
      for pcid in IDPool do
        begin
          c := TPeerClient(FPerClientHashList[pcid]);
          if c <> nil then
            begin
              try
                  OnProgress(c);
              except
              end;
            end;
        end;
    end;
end;

{$IFNDEF FPC}


procedure TCommunicationFramework.ProgressPerClient(OnProgress: TPerClientListProc);
var
  IDPool: TClientIDPool;
  pcid  : Cardinal;
  c     : TPeerClient;
begin
  if (FPerClientHashList.Count > 0) and (Assigned(OnProgress)) then
    begin
      GetClientIDPool(IDPool);
      for pcid in IDPool do
        begin
          c := TPeerClient(FPerClientHashList[pcid]);
          if c <> nil then
            begin
              try
                  OnProgress(c);
              except
              end;
            end;
        end;
    end;
end;
{$ENDIF}


procedure TCommunicationFramework.GetClientIDPool(out IDPool: TClientIDPool);
var
  i: Integer;
  p: PUInt32HashListObjectStruct;
begin
  SetLength(IDPool, FPerClientHashList.Count);
  if (FPerClientHashList.Count > 0) then
    begin
      i := 0;
      p := FPerClientHashList.FirstPtr;
      while i < FPerClientHashList.Count do
        begin
          IDPool[i] := TPeerClient(p^.Data).FID;
          Inc(i);
          p := p^.next;
        end;
    end;
end;

procedure TCommunicationFramework.ProgressWaitSendOfClient(Client: TPeerClient);
begin
  ProgressBackground;
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

function TCommunicationFramework.RegisterConsole(Cmd: SystemString): TCommandConsoleMode;
begin
  if not CanRegCommand(Self, Cmd) then
    begin
      raiseInfo(Format('Illegal Register', []));
      Result := nil;
      Exit;
    end;

  if FCommandList.Exists(Cmd) then
    begin
      raiseInfo(Format('exists cmd:%s', [Cmd]));
      Result := nil;
      Exit;
    end;

  Result := TCommandConsoleMode.Create;
  FCommandList[Cmd] := Result;

  CmdRecvStatistics.IncValue(Cmd, 0);
  CmdMaxExecuteConsumeStatistics[Cmd] := 0;
end;

function TCommunicationFramework.RegisterStream(Cmd: SystemString): TCommandStreamMode;
begin
  if not CanRegCommand(Self, Cmd) then
    begin
      raiseInfo(Format('Illegal Register', []));
      Result := nil;
      Exit;
    end;

  if FCommandList.Exists(Cmd) then
    begin
      raiseInfo(Format('exists cmd:%s', [Cmd]));
      Result := nil;
      Exit;
    end;

  Result := TCommandStreamMode.Create;
  FCommandList[Cmd] := Result;

  CmdRecvStatistics.IncValue(Cmd, 0);
  CmdMaxExecuteConsumeStatistics[Cmd] := 0;
end;

function TCommunicationFramework.RegisterDirectStream(Cmd: SystemString): TCommandDirectStreamMode;
begin
  if not CanRegCommand(Self, Cmd) then
    begin
      raiseInfo(Format('Illegal Register', []));
      Result := nil;
      Exit;
    end;

  if FCommandList.Exists(Cmd) then
    begin
      raiseInfo(Format('exists cmd:%s', [Cmd]));
      Result := nil;
      Exit;
    end;

  Result := TCommandDirectStreamMode.Create;
  FCommandList[Cmd] := Result;

  CmdRecvStatistics.IncValue(Cmd, 0);
  CmdMaxExecuteConsumeStatistics[Cmd] := 0;
end;

function TCommunicationFramework.RegisterDirectConsole(Cmd: SystemString): TCommandDirectConsoleMode;
begin
  if not CanRegCommand(Self, Cmd) then
    begin
      raiseInfo(Format('Illegal Register', []));
      Result := nil;
      Exit;
    end;

  if FCommandList.Exists(Cmd) then
    begin
      raiseInfo(Format('exists cmd:%s', [Cmd]));
      Result := nil;
      Exit;
    end;

  Result := TCommandDirectConsoleMode.Create;
  FCommandList[Cmd] := Result;

  CmdRecvStatistics.IncValue(Cmd, 0);
  CmdMaxExecuteConsumeStatistics[Cmd] := 0;
end;

function TCommunicationFramework.RegisterBigStream(Cmd: SystemString): TCommandBigStreamMode;
begin
  if not CanRegCommand(Self, Cmd) then
    begin
      raiseInfo(Format('Illegal Register', []));
      Result := nil;
      Exit;
    end;

  if FCommandList.Exists(Cmd) then
    begin
      raiseInfo(Format('exists cmd:%s', [Cmd]));
      Result := nil;
      Exit;
    end;

  Result := TCommandBigStreamMode.Create;
  FCommandList[Cmd] := Result;

  CmdRecvStatistics.IncValue(Cmd, 0);
  CmdMaxExecuteConsumeStatistics[Cmd] := 0;
end;

function TCommunicationFramework.ExecuteConsole(Sender: TPeerClient; Cmd: SystemString; const InData: SystemString; var OutData: SystemString): Boolean;
var
  b: TCoreClassObject;
begin
  Result := False;
  if not CanExecuteCommand(Sender, Cmd) then
      Exit;
  b := FCommandList[Cmd];
  if b = nil then
    begin
      Sender.PrintCommand('no exists console cmd:%s', Cmd);
      Exit;
    end;
  if not b.InheritsFrom(TCommandConsoleMode) then
    begin
      Sender.PrintCommand('Illegal interface in cmd:%s', Cmd);
      Exit;
    end;
  Result := TCommandConsoleMode(b).Execute(Sender, InData, OutData);
end;

function TCommunicationFramework.ExecuteStream(Sender: TPeerClient; Cmd: SystemString; InData, OutData: TDataFrameEngine): Boolean;
var
  b: TCoreClassObject;
begin
  Result := False;
  if not CanExecuteCommand(Sender, Cmd) then
      Exit;
  b := FCommandList[Cmd];
  if b = nil then
    begin
      Sender.PrintCommand('no exists stream cmd:%s', Cmd);
      Exit;
    end;
  if not b.InheritsFrom(TCommandStreamMode) then
    begin
      Sender.PrintCommand('Illegal interface in cmd:%s', Cmd);
      Exit;
    end;
  InData.Reader.Index := 0;
  Result := TCommandStreamMode(b).Execute(Sender, InData, OutData);
end;

function TCommunicationFramework.ExecuteDirectStream(Sender: TPeerClient; Cmd: SystemString; InData: TDataFrameEngine): Boolean;
var
  b: TCoreClassObject;
begin
  Result := False;
  if not CanExecuteCommand(Sender, Cmd) then
      Exit;
  b := FCommandList[Cmd];
  if b = nil then
    begin
      Sender.PrintCommand('no exists direct console cmd:%s', Cmd);
      Exit;
    end;
  if not b.InheritsFrom(TCommandDirectStreamMode) then
    begin
      Sender.PrintCommand('Illegal interface in cmd:%s', Cmd);
      Exit;
    end;
  InData.Reader.Index := 0;
  Result := TCommandDirectStreamMode(b).Execute(Sender, InData);
end;

function TCommunicationFramework.ExecuteDirectConsole(Sender: TPeerClient; Cmd: SystemString; const InData: SystemString): Boolean;
var
  b: TCoreClassObject;
begin
  Result := False;
  if not CanExecuteCommand(Sender, Cmd) then
      Exit;
  b := FCommandList[Cmd];
  if b = nil then
    begin
      Sender.PrintCommand('no exists direct stream cmd:%s', Cmd);
      Exit;
    end;
  if not b.InheritsFrom(TCommandDirectConsoleMode) then
    begin
      Sender.PrintCommand('Illegal interface in cmd:%s', Cmd);
      Exit;
    end;
  Result := TCommandDirectConsoleMode(b).Execute(Sender, InData);
end;

function TCommunicationFramework.ExecuteBigStream(Sender: TPeerClient; Cmd: SystemString; InData: TCoreClassStream; FBigStreamTotal, BigStreamCompleteSize: Int64): Boolean;
var
  b: TCoreClassObject;
begin
  Result := False;
  if not CanExecuteCommand(Sender, Cmd) then
      Exit;
  b := FCommandList[Cmd];
  if b = nil then
    begin
      Sender.PrintCommand('no exists Big Stream cmd:%s', Cmd);
      Exit;
    end;
  if not b.InheritsFrom(TCommandBigStreamMode) then
    begin
      Sender.PrintCommand('Illegal interface in cmd:%s', Cmd);
      Exit;
    end;
  Result := TCommandBigStreamMode(b).Execute(Sender, InData, FBigStreamTotal, BigStreamCompleteSize);
end;

function TCommunicationFramework.FirstClient: TPeerClient;
begin
  Result := TPeerClient(FPerClientHashList.First);
end;

function TCommunicationFramework.LastClient: TPeerClient;
begin
  Result := TPeerClient(FPerClientHashList.Last);
end;

procedure TCommunicationFrameworkServer.DoPrint(const v: SystemString);
begin
  inherited DoPrint('server ' + v);
end;

function TCommunicationFrameworkServer.CanExecuteCommand(Sender: TPeerClient; Cmd: SystemString): Boolean;
begin
  if umlMultipleMatch('ConnectedInit', Cmd) then
    begin
      Result := True;
    end
  else
      Result := inherited CanExecuteCommand(Sender, Cmd);
end;

function TCommunicationFrameworkServer.CanSendCommand(Sender: TPeerClient; Cmd: SystemString): Boolean;
begin
  Result := inherited CanSendCommand(Sender, Cmd);
end;

function TCommunicationFrameworkServer.CanRegCommand(Sender: TCommunicationFramework; Cmd: SystemString): Boolean;
begin
  if umlMultipleMatch('ConnectedInit', Cmd) then
    begin
      Result := True;
    end
  else
      Result := inherited CanRegCommand(Sender, Cmd);
end;

procedure TCommunicationFrameworkServer.Command_ConnectedInit(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
begin
  try
      Sender.UserDefine.FWorkPlatform := TExecutePlatform(InData.Reader.ReadInteger);
  except
  end;

  OutData.WriteCardinal(Sender.ID);
  OutData.WriteByte(Byte(FCipherStyle));
  OutData.WriteArrayByte.SetBuff(@Sender.FCipherKey[0], Length(Sender.FCipherKey));

  Sender.FRemoteExecutedForConnectInit := True;
end;

procedure TCommunicationFrameworkServer.Command_Wait(Sender: TPeerClient; InData: SystemString; var OutData: SystemString);
begin
  OutData := IntToHex(GetTimeTick, SizeOf(TTimeTickValue) * 2);
end;

constructor TCommunicationFrameworkServer.Create;
begin
  inherited Create;
  {$IFDEF FPC}
  RegisterStream('ConnectedInit').OnExecute := @Command_ConnectedInit;
  RegisterConsole('Wait').OnExecute := @Command_Wait;
  {$ELSE}
  RegisterStream('ConnectedInit').OnExecute := Command_ConnectedInit;
  RegisterConsole('Wait').OnExecute := Command_Wait;
  {$ENDIF}
  PrintParams['Wait'] := False;
end;

destructor TCommunicationFrameworkServer.Destroy;
begin
  DeleteRegistedCMD('ConnectedInit');
  DeleteRegistedCMD('Wait');
  inherited Destroy;
end;

procedure TCommunicationFrameworkServer.SendConsoleCmd(Client: TPeerClient; Cmd, ConsoleData: SystemString; OnResult: TConsoleMethod);
var
  p: PQueueData;
begin
  // init queue data
  if (Client = nil) or (not Client.Connected) then
      Exit;
  if not CanSendCommand(Client, Cmd) then
      Exit;

  p := NewQueueData;
  p^.State := TQueueState.qsSendConsoleCMD;
  p^.Client := Client;
  p^.Cmd := Cmd;
  p^.Cipher := Client.FSendDataCipherStyle;
  p^.ConsoleData := ConsoleData;
  p^.OnConsoleMethod := OnResult;
  TriggerQueueData(p);

  Client.PrintCommand('Send Console cmd: %s', Cmd);
end;

procedure TCommunicationFrameworkServer.SendStreamCmd(Client: TPeerClient; Cmd: SystemString; StreamData: TCoreClassStream; OnResult: TStreamMethod; DoneFreeStream: Boolean);
var
  p: PQueueData;
begin
  // init queue data
  if (Client = nil) or (not Client.Connected) then
      Exit;
  if not CanSendCommand(Client, Cmd) then
      Exit;
  p := NewQueueData;
  p^.State := TQueueState.qsSendStreamCMD;
  p^.Client := Client;
  p^.Cmd := Cmd;
  p^.Cipher := Client.FSendDataCipherStyle;
  p^.DoneFreeStream := DoneFreeStream;
  p^.StreamData := StreamData;
  p^.OnStreamMethod := OnResult;
  TriggerQueueData(p);
  Client.PrintCommand('Send Stream cmd: %s', Cmd);
end;

procedure TCommunicationFrameworkServer.SendStreamCmd(Client: TPeerClient; Cmd: SystemString; StreamData: TDataFrameEngine; OnResult: TStreamMethod);
var
  p: PQueueData;
begin
  // init queue data
  if (Client = nil) or (not Client.Connected) then
      Exit;
  if not CanSendCommand(Client, Cmd) then
      Exit;
  p := NewQueueData;
  p^.State := TQueueState.qsSendStreamCMD;
  p^.Client := Client;
  p^.Cmd := Cmd;
  p^.Cipher := Client.FSendDataCipherStyle;
  p^.DoneFreeStream := True;
  p^.StreamData := TMemoryStream64.Create;
  if StreamData <> nil then
      StreamData.EncodeTo(p^.StreamData, True)
  else
      TDataFrameEngine.BuildEmptyStream(p^.StreamData);
  p^.OnStreamMethod := OnResult;
  TriggerQueueData(p);
  Client.PrintCommand('Send Stream cmd: %s', Cmd);
end;

procedure TCommunicationFrameworkServer.SendStreamCmd(Client: TPeerClient; Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamMethod);
var
  p: PQueueData;
begin
  // init queue data
  if (Client = nil) or (not Client.Connected) then
      Exit;
  if not CanSendCommand(Client, Cmd) then
      Exit;
  p := NewQueueData;
  p^.State := TQueueState.qsSendStreamCMD;
  p^.Client := Client;
  p^.Cmd := Cmd;
  p^.Cipher := Client.FSendDataCipherStyle;
  p^.DoneFreeStream := True;
  p^.StreamData := TMemoryStream64.Create;
  if StreamData <> nil then
      StreamData.EncodeTo(p^.StreamData, True)
  else
      TDataFrameEngine.BuildEmptyStream(p^.StreamData);
  p^.OnStreamParamMethod := OnResult;
  p^.Param1 := p^.Param1;
  p^.Param2 := p^.Param2;
  TriggerQueueData(p);
  Client.PrintCommand('Send Stream cmd: %s', Cmd);
end;

{$IFNDEF FPC}


procedure TCommunicationFrameworkServer.SendConsoleCmd(Client: TPeerClient; Cmd, ConsoleData: SystemString; OnResult: TConsoleProc);
var
  p: PQueueData;
begin
  // init queue data
  if (Client = nil) or (not Client.Connected) then
      Exit;
  if not CanSendCommand(Client, Cmd) then
      Exit;

  p := NewQueueData;
  p^.State := TQueueState.qsSendConsoleCMD;
  p^.Client := Client;
  p^.Cmd := Cmd;
  p^.Cipher := Client.FSendDataCipherStyle;
  p^.ConsoleData := ConsoleData;
  p^.OnConsoleProc := OnResult;
  TriggerQueueData(p);

  Client.PrintCommand('Send Console cmd: %s', Cmd);
end;

procedure TCommunicationFrameworkServer.SendStreamCmd(Client: TPeerClient; Cmd: SystemString; StreamData: TCoreClassStream; OnResult: TStreamProc; DoneFreeStream: Boolean);
var
  p: PQueueData;
begin
  // init queue data
  if (Client = nil) or (not Client.Connected) then
      Exit;
  if not CanSendCommand(Client, Cmd) then
      Exit;
  p := NewQueueData;
  p^.State := TQueueState.qsSendStreamCMD;
  p^.Client := Client;
  p^.Cmd := Cmd;
  p^.Cipher := Client.FSendDataCipherStyle;
  p^.DoneFreeStream := DoneFreeStream;
  p^.StreamData := StreamData;
  p^.OnStreamProc := OnResult;
  TriggerQueueData(p);
  Client.PrintCommand('Send Stream cmd: %s', Cmd);
end;

procedure TCommunicationFrameworkServer.SendStreamCmd(Client: TPeerClient; Cmd: SystemString; StreamData: TDataFrameEngine; OnResult: TStreamProc);
var
  p: PQueueData;
begin
  // init queue data
  if (Client = nil) or (not Client.Connected) then
      Exit;
  if not CanSendCommand(Client, Cmd) then
      Exit;
  p := NewQueueData;
  p^.State := TQueueState.qsSendStreamCMD;
  p^.Client := Client;
  p^.Cmd := Cmd;
  p^.Cipher := Client.FSendDataCipherStyle;
  p^.DoneFreeStream := True;
  p^.StreamData := TMemoryStream64.Create;
  if StreamData <> nil then
      StreamData.EncodeTo(p^.StreamData, True)
  else
      TDataFrameEngine.BuildEmptyStream(p^.StreamData);
  p^.OnStreamProc := OnResult;
  TriggerQueueData(p);
  Client.PrintCommand('Send Stream cmd: %s', Cmd);
end;

procedure TCommunicationFrameworkServer.SendStreamCmd(Client: TPeerClient; Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamProc);
var
  p: PQueueData;
begin
  // init queue data
  if (Client = nil) or (not Client.Connected) then
      Exit;
  if not CanSendCommand(Client, Cmd) then
      Exit;
  p := NewQueueData;
  p^.State := TQueueState.qsSendStreamCMD;
  p^.Client := Client;
  p^.Cmd := Cmd;
  p^.Cipher := Client.FSendDataCipherStyle;
  p^.DoneFreeStream := True;
  p^.StreamData := TMemoryStream64.Create;
  if StreamData <> nil then
      StreamData.EncodeTo(p^.StreamData, True)
  else
      TDataFrameEngine.BuildEmptyStream(p^.StreamData);
  p^.OnStreamParamProc := OnResult;
  p^.Param1 := p^.Param1;
  p^.Param2 := p^.Param2;
  TriggerQueueData(p);
  Client.PrintCommand('Send Stream cmd: %s', Cmd);
end;
{$ENDIF}


procedure TCommunicationFrameworkServer.SendDirectConsoleCmd(Client: TPeerClient; Cmd, ConsoleData: SystemString);
var
  p: PQueueData;
begin
  // init queue data
  if (Client = nil) or (not Client.Connected) then
      Exit;
  if not CanSendCommand(Client, Cmd) then
      Exit;
  p := NewQueueData;
  p^.State := TQueueState.qsSendDirectConsoleCMD;
  p^.Client := Client;
  p^.Cmd := Cmd;
  p^.Cipher := Client.FSendDataCipherStyle;
  p^.ConsoleData := ConsoleData;
  TriggerQueueData(p);
  Client.PrintCommand('Send DirectConsole cmd: %s', Cmd);
end;

procedure TCommunicationFrameworkServer.SendDirectStreamCmd(Client: TPeerClient; Cmd: SystemString; StreamData: TCoreClassStream; DoneFreeStream: Boolean);
var
  p: PQueueData;
begin
  // init queue data
  if (Client = nil) or (not Client.Connected) then
      Exit;
  if not CanSendCommand(Client, Cmd) then
      Exit;
  p := NewQueueData;
  p^.State := TQueueState.qsSendDirectStreamCMD;
  p^.Client := Client;
  p^.Cmd := Cmd;
  p^.Cipher := Client.FSendDataCipherStyle;
  p^.DoneFreeStream := DoneFreeStream;
  p^.StreamData := StreamData;
  TriggerQueueData(p);
  Client.PrintCommand('Send DirectStream cmd: %s', Cmd);
end;

procedure TCommunicationFrameworkServer.SendDirectStreamCmd(Client: TPeerClient; Cmd: SystemString; StreamData: TDataFrameEngine);
var
  p: PQueueData;
begin
  // init queue data
  if (Client = nil) or (not Client.Connected) then
      Exit;
  if not CanSendCommand(Client, Cmd) then
      Exit;
  p := NewQueueData;
  p^.State := TQueueState.qsSendDirectStreamCMD;
  p^.Client := Client;
  p^.Cmd := Cmd;
  p^.Cipher := Client.FSendDataCipherStyle;
  p^.DoneFreeStream := True;
  p^.StreamData := TMemoryStream64.Create;
  if StreamData <> nil then
      StreamData.EncodeTo(p^.StreamData, True)
  else
      TDataFrameEngine.BuildEmptyStream(p^.StreamData);
  TriggerQueueData(p);
  Client.PrintCommand('Send DirectStream cmd: %s', Cmd);
end;

procedure TCommunicationFrameworkServer.SendDirectStreamCmd(Client: TPeerClient; Cmd: SystemString);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  SendDirectStreamCmd(Client, Cmd, de);
  DisposeObject(de);
end;

function TCommunicationFrameworkServer.WaitSendConsoleCmd(Client: TPeerClient; Cmd: SystemString; ConsoleData: SystemString; TimeOut: TTimeTickValue): SystemString;
var
  waitIntf: TWaitSendConsoleCmdIntf;
  timetick: TTimeTickValue;
begin
  if (Client = nil) or (not Client.Connected) then
      Exit('');
  if not CanSendCommand(Client, Cmd) then
      Exit('');

  Client.PrintCommand('Begin Wait Console cmd: %s', Cmd);

  timetick := GetTimeTickCount + TimeOut;

  while Client.WaitOnResult or Client.BigStreamProcessing or Client.FWaitSendBusy do
    begin
      ProgressWaitSendOfClient(Client);
      if not Exists(Client) then
          Exit;
      if (TimeOut > 0) and (GetTimeTickCount > timetick) then
          Exit('');
    end;

  if not Exists(Client) then
      Exit('');

  Client.FWaitSendBusy := True;

  try
    waitIntf := TWaitSendConsoleCmdIntf.Create;
    waitIntf.Done := False;
    waitIntf.NewResult := '';
    {$IFDEF FPC}
    SendConsoleCmd(Client, Cmd, ConsoleData, @waitIntf.WaitSendConsoleResultEvent);
    {$ELSE}
    SendConsoleCmd(Client, Cmd, ConsoleData, waitIntf.WaitSendConsoleResultEvent);
    {$ENDIF}
    while not waitIntf.Done do
      begin
        ProgressWaitSendOfClient(Client);
        if not Exists(Client) then
            break;
        if (TimeOut > 0) and (GetTimeTickCount > timetick) then
            break;
      end;
    Result := waitIntf.NewResult;
    if waitIntf.Done then
        DisposeObject(waitIntf);
    Client.PrintCommand('End Wait Console cmd: %s', Cmd);
  except
      Result := '';
  end;

  if Exists(Client) then
      Client.FWaitSendBusy := False;
end;

procedure TCommunicationFrameworkServer.WaitSendStreamCmd(Client: TPeerClient; Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; TimeOut: TTimeTickValue);
var
  waitIntf: TWaitSendStreamCmdIntf;
  timetick: Cardinal;
begin
  if (Client = nil) or (not Client.Connected) then
      Exit;
  if not CanSendCommand(Client, Cmd) then
      Exit;

  Client.PrintCommand('Begin Wait Stream cmd: %s', Cmd);

  timetick := GetTimeTickCount + TimeOut;

  while Client.WaitOnResult or Client.BigStreamProcessing or Client.FWaitSendBusy do
    begin
      ProgressWaitSendOfClient(Client);
      if not Exists(Client) then
          Exit;
      if (TimeOut > 0) and (GetTimeTickCount > timetick) then
          Exit;
    end;

  if not Exists(Client) then
      Exit;

  Client.FWaitSendBusy := True;

  try
    waitIntf := TWaitSendStreamCmdIntf.Create;
    waitIntf.Done := False;
    {$IFDEF FPC}
    SendStreamCmd(Client, Cmd, StreamData, @waitIntf.WaitSendStreamResultEvent);
    {$ELSE}
    SendStreamCmd(Client, Cmd, StreamData, waitIntf.WaitSendStreamResultEvent);
    {$ENDIF}
    while not waitIntf.Done do
      begin
        ProgressWaitSendOfClient(Client);
        if not Exists(Client) then
            break;
        if (TimeOut > 0) and (GetTimeTickCount > timetick) then
            break;
      end;
    if waitIntf.Done then
      begin
        ResultData.Assign(waitIntf.NewResult);
        DisposeObject(waitIntf);
      end;
    Client.PrintCommand('End Wait Stream cmd: %s', Cmd);
  except
  end;

  if Exists(Client) then
      Client.FWaitSendBusy := False;
end;

procedure TCommunicationFrameworkServer.SendBigStream(Client: TPeerClient; Cmd: SystemString; BigStream: TCoreClassStream; DoneFreeStream: Boolean);
var
  p: PQueueData;
begin
  // init queue data
  if (Client = nil) or (not Client.Connected) then
      Exit;
  if not CanSendCommand(Client, Cmd) then
      Exit;
  p := NewQueueData;
  p^.State := TQueueState.qsSendBigStream;
  p^.Client := Client;
  p^.Cmd := Cmd;
  p^.Cipher := Client.FSendDataCipherStyle;
  p^.BigStream := BigStream;
  p^.DoneFreeStream := DoneFreeStream;
  TriggerQueueData(p);
  Client.PrintCommand('Send BigStream cmd: %s', Cmd);
end;

procedure TCommunicationFrameworkServer.SendConsoleCmd(ClientID: Cardinal; Cmd, ConsoleData: SystemString; OnResult: TConsoleMethod);
begin
  SendConsoleCmd(ClientFromID[ClientID], Cmd, ConsoleData, OnResult);
end;

procedure TCommunicationFrameworkServer.SendStreamCmd(ClientID: Cardinal; Cmd: SystemString; StreamData: TCoreClassStream; OnResult: TStreamMethod;
DoneFreeStream: Boolean);
begin
  SendStreamCmd(ClientFromID[ClientID], Cmd, StreamData, OnResult, DoneFreeStream);
end;

procedure TCommunicationFrameworkServer.SendStreamCmd(ClientID: Cardinal; Cmd: SystemString; StreamData: TDataFrameEngine; OnResult: TStreamMethod);
begin
  SendStreamCmd(ClientFromID[ClientID], Cmd, StreamData, OnResult);
end;

procedure TCommunicationFrameworkServer.SendStreamCmd(ClientID: Cardinal; Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamMethod);
begin
  SendStreamCmd(ClientFromID[ClientID], Cmd, StreamData, Param1, Param2, OnResult);
end;

{$IFNDEF FPC}


procedure TCommunicationFrameworkServer.SendConsoleCmd(ClientID: Cardinal; Cmd, ConsoleData: SystemString; OnResult: TConsoleProc);
begin
  SendConsoleCmd(ClientFromID[ClientID], Cmd, ConsoleData, OnResult);
end;

procedure TCommunicationFrameworkServer.SendStreamCmd(ClientID: Cardinal; Cmd: SystemString; StreamData: TCoreClassStream; OnResult: TStreamProc;
DoneFreeStream: Boolean);
begin
  SendStreamCmd(ClientFromID[ClientID], Cmd, StreamData, OnResult, DoneFreeStream);
end;

procedure TCommunicationFrameworkServer.SendStreamCmd(ClientID: Cardinal; Cmd: SystemString; StreamData: TDataFrameEngine; OnResult: TStreamProc);
begin
  SendStreamCmd(ClientFromID[ClientID], Cmd, StreamData, OnResult);
end;

procedure TCommunicationFrameworkServer.SendStreamCmd(ClientID: Cardinal; Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamProc);
begin
  SendStreamCmd(ClientFromID[ClientID], Cmd, StreamData, Param1, Param2, OnResult);
end;
{$ENDIF}


procedure TCommunicationFrameworkServer.SendDirectConsoleCmd(ClientID: Cardinal; Cmd, ConsoleData: SystemString);
begin
  SendDirectConsoleCmd(ClientFromID[ClientID], Cmd, ConsoleData);
end;

procedure TCommunicationFrameworkServer.SendDirectStreamCmd(ClientID: Cardinal; Cmd: SystemString; StreamData: TCoreClassStream; DoneFreeStream: Boolean);
begin
  SendDirectStreamCmd(ClientFromID[ClientID], Cmd, StreamData, DoneFreeStream);
end;

procedure TCommunicationFrameworkServer.SendDirectStreamCmd(ClientID: Cardinal; Cmd: SystemString; StreamData: TDataFrameEngine);
begin
  SendDirectStreamCmd(ClientFromID[ClientID], Cmd, StreamData);
end;

procedure TCommunicationFrameworkServer.SendDirectStreamCmd(ClientID: Cardinal; Cmd: SystemString);
begin
  SendDirectStreamCmd(ClientFromID[ClientID], Cmd);
end;

function TCommunicationFrameworkServer.WaitSendConsoleCmd(ClientID: Cardinal; Cmd, ConsoleData: SystemString; TimeOut: TTimeTickValue): SystemString;
begin
  Result := WaitSendConsoleCmd(ClientFromID[ClientID], Cmd, ConsoleData, TimeOut);
end;

procedure TCommunicationFrameworkServer.WaitSendStreamCmd(ClientID: Cardinal; Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; TimeOut: TTimeTickValue);
begin
  WaitSendStreamCmd(ClientFromID[ClientID], Cmd, StreamData, ResultData, TimeOut);
end;

procedure TCommunicationFrameworkServer.SendBigStream(ClientID: Cardinal; Cmd: SystemString; BigStream: TCoreClassStream; DoneFreeStream: Boolean);
begin
  SendBigStream(ClientFromID[ClientID], Cmd, BigStream, DoneFreeStream);
end;

procedure TCommunicationFrameworkServer.BroadcastDirectConsoleCmd(Cmd: SystemString; ConsoleData: SystemString);
var
  i: Integer;
  p: PUInt32HashListObjectStruct;
begin
  if (FPerClientHashList.Count > 0) then
    begin
      i := 0;
      p := FPerClientHashList.FirstPtr;
      while i < FPerClientHashList.Count do
        begin
          try
              TPeerClient(p^.Data).SendDirectConsoleCmd(Cmd, ConsoleData);
          except
          end;
          Inc(i);
          p := p^.next;
        end;
    end;
end;

procedure TCommunicationFrameworkServer.BroadcastSendDirectStreamCmd(Cmd: SystemString; StreamData: TDataFrameEngine);
var
  i: Integer;
  p: PUInt32HashListObjectStruct;
begin
  if (FPerClientHashList.Count > 0) then
    begin
      i := 0;
      p := FPerClientHashList.FirstPtr;
      while i < FPerClientHashList.Count do
        begin
          try
              TPeerClient(p^.Data).SendDirectStreamCmd(Cmd, StreamData);
          except
          end;
          Inc(i);
          p := p^.next;
        end;
    end;
end;

function TCommunicationFrameworkServer.Count: Integer;
begin
  Result := FPerClientHashList.Count;
end;

function TCommunicationFrameworkServer.Exists(cli: TCoreClassObject): Boolean;
begin
  if cli is TPeerClient then
      Result := Exists(cli as TPeerClient)
  else if cli is TPeerClientUserDefine then
      Result := Exists(cli as TPeerClientUserDefine)
  else if cli is TPeerClientUserSpecial then
      Result := Exists(cli as TPeerClientUserSpecial)
  else
      Result := False;
end;

function TCommunicationFrameworkServer.Exists(cli: TPeerClient): Boolean;
begin
  Result := FPerClientHashList.ExistsObject(cli);
end;

function TCommunicationFrameworkServer.Exists(cli: TPeerClientUserDefine): Boolean;
var
  i: Integer;
  p: PUInt32HashListObjectStruct;
begin
  Result := False;
  if (FPerClientHashList.Count > 0) then
    begin
      i := 0;
      p := FPerClientHashList.FirstPtr;
      while i < FPerClientHashList.Count do
        begin
          if TPeerClient(p^.Data).FUserDefine = cli then
            begin
              Result := True;
              Exit;
            end;
          Inc(i);
          p := p^.next;
        end;
    end;
end;

function TCommunicationFrameworkServer.Exists(cli: TPeerClientUserSpecial): Boolean;
var
  i: Integer;
  p: PUInt32HashListObjectStruct;
begin
  Result := False;
  if (FPerClientHashList.Count > 0) then
    begin
      i := 0;
      p := FPerClientHashList.FirstPtr;
      while i < FPerClientHashList.Count do
        begin
          if TPeerClient(p^.Data).FUserSpecial = cli then
            begin
              Result := True;
              Exit;
            end;
          Inc(i);
          p := p^.next;
        end;
    end;
end;

function TCommunicationFrameworkServer.Exists(ClientID: Cardinal): Boolean;
begin
  Result := FPerClientHashList.Exists(ClientID);
end;

function TCommunicationFrameworkServer.GetClientFromID(ID: Cardinal): TPeerClient;
begin
  Result := TPeerClient(FPerClientHashList[ID]);
end;

procedure TCommunicationFrameworkClient.DoPrint(const v: SystemString);
begin
  inherited DoPrint('client ' + v);
end;

procedure TCommunicationFrameworkClient.StreamResult_ConnectedInit(Sender: TPeerClient; ResultData: TDataFrameEngine);
var
  arr: TDataFrameArrayByte;
begin
  if ResultData.Count > 0 then
    begin
      FPerClientHashList.Delete(Sender.FID);
      // index 0: my remote id
      Sender.FID := ResultData.Reader.ReadCardinal;
      FPerClientHashList.Add(Sender.FID, Sender, True);
      // index 1: used Encrypt
      Sender.FSendDataCipherStyle := TCipherStyle(ResultData.Reader.ReadByte);

      // index 2:Encrypt CipherKey
      arr := ResultData.Reader.ReadArrayByte;
      SetLength(Sender.FCipherKey, arr.Count);
      arr.GetBuff(@Sender.FCipherKey[0]);

      Sender.FRemoteExecutedForConnectInit := True;

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

procedure TCommunicationFrameworkClient.DoConnected(Sender: TPeerClient);
var
  de: TDataFrameEngine;
begin
  FConnectInitWaiting := True;
  FConnectInitWaitingTimeout := GetTimeTick + 2000;

  ClientIO.FSendDataCipherStyle := TCipherStyle.csNone;
  de := TDataFrameEngine.Create;
  de.WriteInteger(Integer(CurrentPlatform));
  {$IFDEF FPC}
  SendStreamCmd('ConnectedInit', de, @StreamResult_ConnectedInit);
  {$ELSE}
  SendStreamCmd('ConnectedInit', de, StreamResult_ConnectedInit);
  {$ENDIF}
  DisposeObject(de);

  inherited DoConnected(Sender);

  if FNotyifyInterface <> nil then
    begin
      try
          FNotyifyInterface.ClientConnected(Self);
      except
      end;
    end;
end;

procedure TCommunicationFrameworkClient.DoDisconnect(Sender: TPeerClient);
begin
  FPerClientHashList.Delete(Sender.FID);
  Sender.FID := 0;
  Sender.FRemoteExecutedForConnectInit := False;

  try
      inherited DoDisconnect(Sender);
  except
  end;

  try
    if FNotyifyInterface <> nil then
      begin
        try
            FNotyifyInterface.ClientDisconnect(Self);
        except
        end;
      end;
  except
  end;
end;

function TCommunicationFrameworkClient.CanExecuteCommand(Sender: TPeerClient; Cmd: SystemString): Boolean;
begin
  Result := inherited CanExecuteCommand(Sender, Cmd);
end;

function TCommunicationFrameworkClient.CanSendCommand(Sender: TPeerClient; Cmd: SystemString): Boolean;
begin
  if umlMultipleMatch('ConnectedInit', Cmd) then
    begin
      Result := True;
    end
  else
      Result := inherited CanSendCommand(Sender, Cmd);
end;

function TCommunicationFrameworkClient.CanRegCommand(Sender: TCommunicationFramework; Cmd: SystemString): Boolean;
begin
  Result := inherited CanRegCommand(Sender, Cmd);
end;

procedure TCommunicationFrameworkClient.ConsoleResult_Wait(Sender: TPeerClient; ResultData: SystemString);
begin
  if FWaiting then
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
        {$ENDIF}
      except
      end;

      FOnWaitResultCall := nil;
      FOnWaitResultMethod := nil;
      {$IFNDEF FPC}
      FOnWaitResultProc := nil;
      {$ENDIF}
    end;
end;

constructor TCommunicationFrameworkClient.Create;
begin
  inherited Create;
  FNotyifyInterface := nil;
  FConnectInitWaiting := False;
  FConnectInitWaitingTimeout := 0;

  FWaiting := False;
  FWaitingTimeOut := 0;
  FOnWaitResultCall := nil;
  FOnWaitResultMethod := nil;
  {$IFNDEF FPC}
  FOnWaitResultProc := nil;
  {$ENDIF}
end;

procedure TCommunicationFrameworkClient.ProgressBackground;
begin
  inherited ProgressBackground;

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
        {$ENDIF}
      except
      end;

      FOnWaitResultCall := nil;
      FOnWaitResultMethod := nil;
      {$IFNDEF FPC}
      FOnWaitResultProc := nil;
      {$ENDIF}
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

function TCommunicationFrameworkClient.ClientIO: TPeerClient;
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

procedure TCommunicationFrameworkClient.AsyncConnect(Addr: SystemString; Port: Word; OnResult: TStateCall);
begin
  if Assigned(OnResult) then
      OnResult(False);
end;

procedure TCommunicationFrameworkClient.AsyncConnect(Addr: SystemString; Port: Word; OnResult: TStateMethod);
begin
  if Assigned(OnResult) then
      OnResult(False);
end;

{$IFNDEF FPC}


procedure TCommunicationFrameworkClient.AsyncConnect(Addr: SystemString; Port: Word; OnResult: TStateProc);
begin
  if Assigned(OnResult) then
      OnResult(False);
end;
{$ENDIF}


function TCommunicationFrameworkClient.Connect(Addr: SystemString; Port: Word): Boolean;
begin
  Result := False;
end;

procedure TCommunicationFrameworkClient.Disconnect;
begin
end;

// sync KeepAlive
function TCommunicationFrameworkClient.Wait(ATimeOut: TTimeTickValue): string;
begin
  Result := '';
  if (ClientIO = nil) then
      Exit;
  if (not Connected) then
      Exit;

  Result := WaitSendConsoleCmd('Wait', '', ATimeOut);
end;

function TCommunicationFrameworkClient.Wait(ATimeOut: TTimeTickValue; OnResult: TStateCall): Boolean;
begin
  Result := False;
  if (ClientIO = nil) then
      Exit;
  if (not Connected) then
      Exit;
  if (FWaiting) then
      Exit;

  FWaiting := True;
  FWaitingTimeOut := GetTimeTick + ATimeOut;
  FOnWaitResultCall := OnResult;
  FOnWaitResultMethod := nil;
  {$IFNDEF FPC}
  FOnWaitResultProc := nil;
  {$ENDIF}
  {$IFDEF FPC}
  SendConsoleCmd('Wait', '', @ConsoleResult_Wait);
  {$ELSE}
  SendConsoleCmd('Wait', '', ConsoleResult_Wait);
  {$ENDIF}
  Result := True;
end;

function TCommunicationFrameworkClient.Wait(ATimeOut: TTimeTickValue; OnResult: TStateMethod): Boolean;
begin
  Result := False;
  if (ClientIO = nil) then
      Exit;
  if (not Connected) then
      Exit;
  if (FWaiting) then
      Exit;

  FWaiting := True;
  FWaitingTimeOut := GetTimeTick + ATimeOut;
  FOnWaitResultCall := nil;
  FOnWaitResultMethod := OnResult;
  {$IFNDEF FPC}
  FOnWaitResultProc := nil;
  {$ENDIF}
  {$IFDEF FPC}
  SendConsoleCmd('Wait', '', @ConsoleResult_Wait);
  {$ELSE}
  SendConsoleCmd('Wait', '', ConsoleResult_Wait);
  {$ENDIF}
  Result := True;
end;

{$IFNDEF FPC}


function TCommunicationFrameworkClient.Wait(ATimeOut: TTimeTickValue; OnResult: TStateProc): Boolean;
begin
  Result := False;
  if (ClientIO = nil) then
      Exit;
  if (not Connected) then
      Exit;
  if (FWaiting) then
      Exit;

  FWaiting := True;
  FWaitingTimeOut := GetTimeTick + ATimeOut;
  FOnWaitResultCall := nil;
  FOnWaitResultMethod := nil;
  FOnWaitResultProc := OnResult;
  SendConsoleCmd('Wait', '', ConsoleResult_Wait);
  Result := True;
end;
{$ENDIF}


function TCommunicationFrameworkClient.WaitSendBusy: Boolean;
begin
  Result := (ClientIO <> nil) and (ClientIO.WaitSendBusy);
end;

function TCommunicationFrameworkClient.LastQueueData: PQueueData;
begin
  Result := nil;
  if ClientIO = nil then
      Exit;
  if ClientIO.FQueueList.Count = 0 then
      Exit;
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
      Exit;
  Result := ClientIO.FQueueList.Count;
end;

procedure TCommunicationFrameworkClient.SendConsoleCmd(Cmd, ConsoleData: SystemString; OnResult: TConsoleMethod);
var
  p: PQueueData;
begin
  if ClientIO = nil then
      Exit;
  if not Connected then
      Exit;
  if not CanSendCommand(ClientIO, Cmd) then
      Exit;
  // init queue data
  p := NewQueueData;
  p^.State := TQueueState.qsSendConsoleCMD;

  p^.Cmd := Cmd;
  p^.Cipher := ClientIO.FSendDataCipherStyle;
  p^.ConsoleData := ConsoleData;
  p^.OnConsoleMethod := OnResult;
  TriggerQueueData(p);
  ClientIO.PrintCommand('Send Console cmd: %s', Cmd);
end;

procedure TCommunicationFrameworkClient.SendStreamCmd(Cmd: SystemString; StreamData: TCoreClassStream; OnResult: TStreamMethod; DoneFreeStream: Boolean);
var
  p: PQueueData;
begin
  if ClientIO = nil then
      Exit;
  if not Connected then
      Exit;
  if not CanSendCommand(ClientIO, Cmd) then
      Exit;
  // init queue data
  p := NewQueueData;
  p^.State := TQueueState.qsSendStreamCMD;

  p^.Cmd := Cmd;
  p^.Cipher := ClientIO.FSendDataCipherStyle;
  p^.DoneFreeStream := DoneFreeStream;
  p^.StreamData := StreamData;
  p^.OnStreamMethod := OnResult;
  TriggerQueueData(p);
  ClientIO.PrintCommand('Send Stream cmd: %s', Cmd);
end;

procedure TCommunicationFrameworkClient.SendStreamCmd(Cmd: SystemString; StreamData: TDataFrameEngine; OnResult: TStreamMethod);
var
  p: PQueueData;
begin
  if ClientIO = nil then
      Exit;
  if not Connected then
      Exit;
  if not CanSendCommand(ClientIO, Cmd) then
      Exit;
  // init queue data
  p := NewQueueData;
  p^.State := TQueueState.qsSendStreamCMD;

  p^.Cmd := Cmd;
  p^.Cipher := ClientIO.FSendDataCipherStyle;
  p^.DoneFreeStream := True;
  p^.StreamData := TMemoryStream64.Create;
  if StreamData <> nil then
      StreamData.EncodeTo(p^.StreamData, True)
  else
      TDataFrameEngine.BuildEmptyStream(p^.StreamData);
  p^.OnStreamMethod := OnResult;
  TriggerQueueData(p);
  ClientIO.PrintCommand('Send Stream cmd: %s', Cmd);
end;

procedure TCommunicationFrameworkClient.SendStreamCmd(Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamMethod);
var
  p: PQueueData;
begin
  if ClientIO = nil then
      Exit;
  if not Connected then
      Exit;
  if not CanSendCommand(ClientIO, Cmd) then
      Exit;
  // init queue data
  p := NewQueueData;
  p^.State := TQueueState.qsSendStreamCMD;

  p^.Cmd := Cmd;
  p^.Cipher := ClientIO.FSendDataCipherStyle;
  p^.DoneFreeStream := True;
  p^.StreamData := TMemoryStream64.Create;
  if StreamData <> nil then
      StreamData.EncodeTo(p^.StreamData, True)
  else
      TDataFrameEngine.BuildEmptyStream(p^.StreamData);
  p^.OnStreamParamMethod := OnResult;
  p^.Param1 := Param1;
  p^.Param2 := Param2;
  TriggerQueueData(p);
  ClientIO.PrintCommand('Send Stream cmd: %s', Cmd);
end;

{$IFNDEF FPC}


procedure TCommunicationFrameworkClient.SendConsoleCmd(Cmd, ConsoleData: SystemString; OnResult: TConsoleProc);
var
  p: PQueueData;
begin
  if ClientIO = nil then
      Exit;
  if not Connected then
      Exit;
  if not CanSendCommand(ClientIO, Cmd) then
      Exit;
  // init queue data
  p := NewQueueData;
  p^.State := TQueueState.qsSendConsoleCMD;

  p^.Cmd := Cmd;
  p^.Cipher := ClientIO.FSendDataCipherStyle;
  p^.ConsoleData := ConsoleData;
  p^.OnConsoleProc := OnResult;
  TriggerQueueData(p);
  ClientIO.PrintCommand('Send Console cmd: %s', Cmd);
end;

procedure TCommunicationFrameworkClient.SendStreamCmd(Cmd: SystemString; StreamData: TCoreClassStream; OnResult: TStreamProc; DoneFreeStream: Boolean);
var
  p: PQueueData;
begin
  if ClientIO = nil then
      Exit;
  if not Connected then
      Exit;
  if not CanSendCommand(ClientIO, Cmd) then
      Exit;
  // init queue data
  p := NewQueueData;
  p^.State := TQueueState.qsSendStreamCMD;

  p^.Cmd := Cmd;
  p^.Cipher := ClientIO.FSendDataCipherStyle;
  p^.DoneFreeStream := DoneFreeStream;
  p^.StreamData := StreamData;
  p^.OnStreamProc := OnResult;
  TriggerQueueData(p);
  ClientIO.PrintCommand('Send Stream cmd: %s', Cmd);
end;

procedure TCommunicationFrameworkClient.SendStreamCmd(Cmd: SystemString; StreamData: TDataFrameEngine; OnResult: TStreamProc);
var
  p: PQueueData;
begin
  if ClientIO = nil then
      Exit;
  if not Connected then
      Exit;
  if not CanSendCommand(ClientIO, Cmd) then
      Exit;
  // init queue data
  p := NewQueueData;
  p^.State := TQueueState.qsSendStreamCMD;

  p^.Cmd := Cmd;
  p^.Cipher := ClientIO.FSendDataCipherStyle;
  p^.DoneFreeStream := True;
  p^.StreamData := TMemoryStream64.Create;
  if StreamData <> nil then
      StreamData.EncodeTo(p^.StreamData, True)
  else
      TDataFrameEngine.BuildEmptyStream(p^.StreamData);
  p^.OnStreamProc := OnResult;
  TriggerQueueData(p);
  ClientIO.PrintCommand('Send Stream cmd: %s', Cmd);
end;

procedure TCommunicationFrameworkClient.SendStreamCmd(Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamProc);
var
  p: PQueueData;
begin
  if ClientIO = nil then
      Exit;
  if not Connected then
      Exit;
  if not CanSendCommand(ClientIO, Cmd) then
      Exit;
  // init queue data
  p := NewQueueData;
  p^.State := TQueueState.qsSendStreamCMD;

  p^.Cmd := Cmd;
  p^.Cipher := ClientIO.FSendDataCipherStyle;
  p^.DoneFreeStream := True;
  p^.StreamData := TMemoryStream64.Create;
  if StreamData <> nil then
      StreamData.EncodeTo(p^.StreamData, True)
  else
      TDataFrameEngine.BuildEmptyStream(p^.StreamData);
  p^.OnStreamParamProc := OnResult;
  p^.Param1 := Param1;
  p^.Param2 := Param2;
  TriggerQueueData(p);
  ClientIO.PrintCommand('Send Stream cmd: %s', Cmd);
end;
{$ENDIF}


procedure TCommunicationFrameworkClient.SendDirectConsoleCmd(Cmd, ConsoleData: SystemString);
var
  p: PQueueData;
begin
  if ClientIO = nil then
      Exit;
  if not Connected then
      Exit;
  if not CanSendCommand(ClientIO, Cmd) then
      Exit;
  // init queue data
  p := NewQueueData;
  p^.State := TQueueState.qsSendDirectConsoleCMD;

  p^.Cmd := Cmd;
  p^.Cipher := ClientIO.FSendDataCipherStyle;
  p^.ConsoleData := ConsoleData;
  TriggerQueueData(p);
  ClientIO.PrintCommand('Send DirectConsole cmd: %s', Cmd);
end;

procedure TCommunicationFrameworkClient.SendDirectStreamCmd(Cmd: SystemString; StreamData: TCoreClassStream; DoneFreeStream: Boolean);
var
  p: PQueueData;
begin
  if ClientIO = nil then
      Exit;
  if not Connected then
      Exit;
  if not CanSendCommand(ClientIO, Cmd) then
      Exit;
  // init queue data
  p := NewQueueData;
  p^.State := TQueueState.qsSendDirectStreamCMD;

  p^.Cmd := Cmd;
  p^.Cipher := ClientIO.FSendDataCipherStyle;
  p^.DoneFreeStream := DoneFreeStream;
  p^.StreamData := StreamData;
  TriggerQueueData(p);
  ClientIO.PrintCommand('Send DirectStream cmd: %s', Cmd);
end;

procedure TCommunicationFrameworkClient.SendDirectStreamCmd(Cmd: SystemString; StreamData: TDataFrameEngine);
var
  p: PQueueData;
begin
  if ClientIO = nil then
      Exit;
  if not Connected then
      Exit;
  if not CanSendCommand(ClientIO, Cmd) then
      Exit;
  // init queue data
  p := NewQueueData;
  p^.State := TQueueState.qsSendDirectStreamCMD;

  p^.Cmd := Cmd;
  p^.Cipher := ClientIO.FSendDataCipherStyle;
  p^.DoneFreeStream := True;
  p^.StreamData := TMemoryStream64.Create;
  if StreamData <> nil then
      StreamData.EncodeTo(p^.StreamData, True)
  else
      TDataFrameEngine.BuildEmptyStream(p^.StreamData);
  TriggerQueueData(p);
  ClientIO.PrintCommand('Send DirectStream cmd: %s', Cmd);
end;

procedure TCommunicationFrameworkClient.SendDirectStreamCmd(Cmd: SystemString);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  SendDirectStreamCmd(Cmd, de);
  DisposeObject(de);
end;

function TCommunicationFrameworkClient.WaitSendConsoleCmd(Cmd: SystemString; ConsoleData: SystemString; TimeOut: TTimeTickValue): SystemString;
var
  waitIntf: TWaitSendConsoleCmdIntf;
  timetick: TTimeTickValue;
begin
  Result := '';
  if ClientIO = nil then
      Exit;
  if not Connected then
      Exit;
  if not CanSendCommand(ClientIO, Cmd) then
      Exit;
  ClientIO.PrintCommand('Begin Wait console cmd: %s', Cmd);

  timetick := GetTimeTickCount + TimeOut;

  while ClientIO.WaitOnResult or ClientIO.BigStreamProcessing or ClientIO.FWaitSendBusy do
    begin
      ProgressWaitSendOfClient(ClientIO);
      if not Connected then
          Exit;
      if (TimeOut > 0) and (GetTimeTickCount > timetick) then
          Exit;
    end;

  if not Connected then
      Exit('');

  ClientIO.FWaitSendBusy := True;

  try
    waitIntf := TWaitSendConsoleCmdIntf.Create;
    waitIntf.Done := False;
    waitIntf.NewResult := '';
    {$IFDEF FPC}
    SendConsoleCmd(Cmd, ConsoleData, @waitIntf.WaitSendConsoleResultEvent);
    {$ELSE}
    SendConsoleCmd(Cmd, ConsoleData, waitIntf.WaitSendConsoleResultEvent);
    {$ENDIF}
    while not waitIntf.Done do
      begin
        ProgressWaitSendOfClient(ClientIO);
        if not Connected then
            break;
        if (TimeOut > 0) and (GetTimeTickCount > timetick) then
            break;
      end;
    Result := waitIntf.NewResult;
    if waitIntf.Done then
        DisposeObject(waitIntf);
    ClientIO.PrintCommand('End Wait console cmd: %s', Cmd);
  except
      Result := '';
  end;

  if Connected then
      ClientIO.FWaitSendBusy := False;
end;

procedure TCommunicationFrameworkClient.WaitSendStreamCmd(Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; TimeOut: TTimeTickValue);
var
  waitIntf: TWaitSendStreamCmdIntf;
  timetick: TTimeTickValue;
begin
  if ClientIO = nil then
      Exit;
  if not Connected then
      Exit;
  if not CanSendCommand(ClientIO, Cmd) then
      Exit;

  ClientIO.PrintCommand('Begin Wait Stream cmd: %s', Cmd);

  timetick := GetTimeTickCount + TimeOut;

  while ClientIO.WaitOnResult or ClientIO.BigStreamProcessing or ClientIO.FWaitSendBusy do
    begin
      ProgressWaitSendOfClient(ClientIO);
      if not Connected then
          Exit;
      if (TimeOut > 0) and (GetTimeTickCount > timetick) then
          Exit;
    end;

  if not Connected then
      Exit;

  ClientIO.FWaitSendBusy := True;

  try
    waitIntf := TWaitSendStreamCmdIntf.Create;
    waitIntf.Done := False;
    {$IFDEF FPC}
    SendStreamCmd(Cmd, StreamData, @waitIntf.WaitSendStreamResultEvent);
    {$ELSE}
    SendStreamCmd(Cmd, StreamData, waitIntf.WaitSendStreamResultEvent);
    {$ENDIF}
    while not waitIntf.Done do
      begin
        ProgressWaitSendOfClient(ClientIO);
        if not Connected then
            break;
        if (TimeOut > 0) and (GetTimeTickCount > timetick) then
            break;
      end;
    if waitIntf.Done then
      begin
        ResultData.Assign(waitIntf.NewResult);
        DisposeObject(waitIntf);
      end;
    ClientIO.PrintCommand('End Wait Stream cmd: %s', Cmd);
  except
  end;

  if Connected then
      ClientIO.FWaitSendBusy := False;
end;

procedure TCommunicationFrameworkClient.SendBigStream(Cmd: SystemString; BigStream: TCoreClassStream; DoneFreeStream: Boolean);
var
  p: PQueueData;
begin
  if ClientIO = nil then
      Exit;
  if not Connected then
      Exit;
  if not CanSendCommand(ClientIO, Cmd) then
      Exit;
  // init queue data
  p := NewQueueData;
  p^.State := TQueueState.qsSendBigStream;

  p^.Cmd := Cmd;
  p^.Cipher := ClientIO.FSendDataCipherStyle;
  p^.BigStream := BigStream;
  p^.DoneFreeStream := DoneFreeStream;
  TriggerQueueData(p);
  ClientIO.PrintCommand('Send BigStream cmd: %s', Cmd);
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

initialization

{$IFDEF FPC}
  ProgressBackgroundProc := @DefaultProgressBackgroundProc;
{$ELSE}
  ProgressBackgroundProc := DefaultProgressBackgroundProc;
{$ENDIF}

finalization

end.
