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

  TIPV4 = array [0 .. 3] of Byte;
  PIPV4 = ^TIPV4;

  TIPV6 = array [0 .. 7] of Word;
  PIPV6 = ^TIPV6;

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
  TQueueState = (qsUnknow, qsSendConsoleCMD, qsSendStreamCMD, qsSendDirectConsoleCMD, qsSendDirectStreamCMD, qsSendBigStream, qsSendCompleteBuffer);

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

    Buffer: PByte;
    BufferSize: NativeInt;

    DoneAutoFree: Boolean;

    Param1: Pointer;
    Param2: TObject;
  end;

  PQueueData = ^TQueueData;

  TCommandStreamCall         = procedure(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
  TCommandConsoleCall        = procedure(Sender: TPeerClient; InData: SystemString; var OutData: SystemString);
  TCommandDirectStreamCall   = procedure(Sender: TPeerClient; InData: TDataFrameEngine);
  TCommandDirectConsoleCall  = procedure(Sender: TPeerClient; InData: SystemString);
  TCommandBigStreamCall      = procedure(Sender: TPeerClient; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64);
  TCommandCompleteBufferCall = procedure(Sender: TPeerClient; InData: PByte; DataSize: NativeInt);

  TCommandStreamMethod         = procedure(Sender: TPeerClient; InData, OutData: TDataFrameEngine) of object;
  TCommandConsoleMethod        = procedure(Sender: TPeerClient; InData: SystemString; var OutData: SystemString) of object;
  TCommandDirectStreamMethod   = procedure(Sender: TPeerClient; InData: TDataFrameEngine) of object;
  TCommandDirectConsoleMethod  = procedure(Sender: TPeerClient; InData: SystemString) of object;
  TCommandBigStreamMethod      = procedure(Sender: TPeerClient; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64) of object;
  TCommandCompleteBufferMethod = procedure(Sender: TPeerClient; InData: PByte; DataSize: NativeInt) of object;

  {$IFNDEF FPC}
  TCommandStreamProc         = reference to procedure(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
  TCommandConsoleProc        = reference to procedure(Sender: TPeerClient; InData: SystemString; var OutData: SystemString);
  TCommandDirectStreamProc   = reference to procedure(Sender: TPeerClient; InData: TDataFrameEngine);
  TCommandDirectConsoleProc  = reference to procedure(Sender: TPeerClient; InData: SystemString);
  TCommandBigStreamProc      = reference to procedure(Sender: TPeerClient; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64);
  TCommandCompleteBufferProc = reference to procedure(Sender: TPeerClient; InData: PByte; DataSize: NativeInt);
  {$ENDIF}

  TCommandStreamMode = class(TCoreClassInterfacedObject)
  protected
    FOnExecuteCall  : TCommandStreamCall;
    FOnExecuteMethod: TCommandStreamMethod;
    {$IFNDEF FPC}
    FOnExecuteProc: TCommandStreamProc;
    {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;

    function Execute(Sender: TPeerClient; InData, OutData: TDataFrameEngine): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    property OnExecute: TCommandStreamMethod read FOnExecuteMethod write FOnExecuteMethod;
    property OnExecuteCall: TCommandStreamCall read FOnExecuteCall write FOnExecuteCall;
    property OnExecuteMethod: TCommandStreamMethod read FOnExecuteMethod write FOnExecuteMethod;
    {$IFNDEF FPC}
    property OnExecuteProc: TCommandStreamProc read FOnExecuteProc write FOnExecuteProc;
    {$ENDIF}
  end;

  TCommandConsoleMode = class(TCoreClassInterfacedObject)
  protected
    FOnExecuteCall  : TCommandConsoleCall;
    FOnExecuteMethod: TCommandConsoleMethod;
    {$IFNDEF FPC}
    FOnExecuteProc: TCommandConsoleProc;
    {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;

    function Execute(Sender: TPeerClient; InData: SystemString; var OutData: SystemString): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    property OnExecute: TCommandConsoleMethod read FOnExecuteMethod write FOnExecuteMethod;
    property OnExecuteCall: TCommandConsoleCall read FOnExecuteCall write FOnExecuteCall;
    property OnExecuteMethod: TCommandConsoleMethod read FOnExecuteMethod write FOnExecuteMethod;
    {$IFNDEF FPC}
    property OnExecuteProc: TCommandConsoleProc read FOnExecuteProc write FOnExecuteProc;
    {$ENDIF}
  end;

  TCommandDirectStreamMode = class(TCoreClassInterfacedObject)
  protected
    FOnExecuteCall  : TCommandDirectStreamCall;
    FOnExecuteMethod: TCommandDirectStreamMethod;
    {$IFNDEF FPC}
    FOnExecuteProc: TCommandDirectStreamProc;
    {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;

    function Execute(Sender: TPeerClient; InData: TDataFrameEngine): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    property OnExecute: TCommandDirectStreamMethod read FOnExecuteMethod write FOnExecuteMethod;
    property OnExecuteCall: TCommandDirectStreamCall read FOnExecuteCall write FOnExecuteCall;
    property OnExecuteMethod: TCommandDirectStreamMethod read FOnExecuteMethod write FOnExecuteMethod;
    {$IFNDEF FPC}
    property OnExecuteProc: TCommandDirectStreamProc read FOnExecuteProc write FOnExecuteProc;
    {$ENDIF}
  end;

  TCommandDirectConsoleMode = class(TCoreClassInterfacedObject)
  protected
    FOnExecuteCall  : TCommandDirectConsoleCall;
    FOnExecuteMethod: TCommandDirectConsoleMethod;
    {$IFNDEF FPC}
    FOnExecuteProc: TCommandDirectConsoleProc;
    {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;

    function Execute(Sender: TPeerClient; InData: SystemString): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    property OnExecute: TCommandDirectConsoleMethod read FOnExecuteMethod write FOnExecuteMethod;
    property OnExecuteCall: TCommandDirectConsoleCall read FOnExecuteCall write FOnExecuteCall;
    property OnExecuteMethod: TCommandDirectConsoleMethod read FOnExecuteMethod write FOnExecuteMethod;
    {$IFNDEF FPC}
    property OnExecuteProc: TCommandDirectConsoleProc read FOnExecuteProc write FOnExecuteProc;
    {$ENDIF}
  end;

  TCommandBigStreamMode = class(TCoreClassInterfacedObject)
  protected
    FOnExecuteCall  : TCommandBigStreamCall;
    FOnExecuteMethod: TCommandBigStreamMethod;
    {$IFNDEF FPC}
    FOnExecuteProc: TCommandBigStreamProc;
    {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;

    function Execute(Sender: TPeerClient; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    property OnExecute: TCommandBigStreamMethod read FOnExecuteMethod write FOnExecuteMethod;
    property OnExecuteCall: TCommandBigStreamCall read FOnExecuteCall write FOnExecuteCall;
    property OnExecuteMethod: TCommandBigStreamMethod read FOnExecuteMethod write FOnExecuteMethod;
    {$IFNDEF FPC}
    property OnExecuteProc: TCommandBigStreamProc read FOnExecuteProc write FOnExecuteProc;
    {$ENDIF}
  end;

  TCommandCompleteBufferMode = class(TCoreClassInterfacedObject)
  protected
    FOnExecuteCall  : TCommandCompleteBufferCall;
    FOnExecuteMethod: TCommandCompleteBufferMethod;
    {$IFNDEF FPC}
    FOnExecuteProc: TCommandCompleteBufferProc;
    {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;

    function Execute(Sender: TPeerClient; InData: PByte; DataSize: NativeInt): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    property OnExecute: TCommandCompleteBufferMethod read FOnExecuteMethod write FOnExecuteMethod;
    property OnExecuteCall: TCommandCompleteBufferCall read FOnExecuteCall write FOnExecuteCall;
    property OnExecuteMethod: TCommandCompleteBufferMethod read FOnExecuteMethod write FOnExecuteMethod;
    {$IFNDEF FPC}
    property OnExecuteProc: TCommandCompleteBufferProc read FOnExecuteProc write FOnExecuteProc;
    {$ENDIF}
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
  end;

  TPeerClientUserDefine = class(TCoreClassInterfacedObject)
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

  TInternalSendByteBuffer       = procedure(const Sender: TPeerClient; const buff: PByte; Siz: NativeInt) of object;
  TInternalSaveReceiveBuffer    = procedure(const Sender: TPeerClient; const buff: Pointer; Siz: Int64) of object;
  TInternalProcessReceiveBuffer = procedure(const Sender: TPeerClient; const ACurrentActiveThread: TCoreClassThread; const RecvSync, SendSync: Boolean) of object;
  TInternalProcessAllSendCmd    = procedure(const Sender: TPeerClient; const ACurrentActiveThread: TCoreClassThread; const RecvSync, SendSync: Boolean) of object;
  TInternalClientCreate         = procedure(const Sender: TPeerClient) of object;
  TInternalClientDestory        = procedure(const Sender: TPeerClient) of object;

  TCommunicationFrameworkWithP2PVM        = class;
  TCommunicationFrameworkWithP2PVM_Server = class;
  TCommunicationFrameworkWithP2PVM_Client = class;

  TPeerClient = class(TCoreClassInterfacedObject, IMemoryStream64WriteTrigger)
  private
    FOwnerFramework                     : TCommunicationFramework;
    FClientIntf                         : TCoreClassObject;
    FID                                 : Cardinal;
    FHeadToken, FTailToken              : Cardinal;
    FConsoleToken                       : Byte;
    FStreamToken                        : Byte;
    FDirectConsoleToken                 : Byte;
    FDirectStreamToken                  : Byte;
    FBigStreamToken                     : Byte;
    FCompleteBufferToken                : Byte;
    FReceivedBuffer                     : TMemoryStream64OfWriteTrigger;
    FBigStreamReceiveProcessing         : Boolean;
    FBigStreamTotal                     : Int64;
    FBigStreamCompleted                 : Int64;
    FBigStreamCmd                       : SystemString;
    FBigStreamReceive                   : TCoreClassStream;
    FBigStreamSending                   : TCoreClassStream;
    FBigStreamSendState                 : Int64;
    FBigStreamSendDoneTimeFree          : Boolean;
    FCompleteBufferReceiveProcessing    : Boolean;
    FCompleteBufferTotal                : Int64;
    FCompleteBufferCompleted            : Int64;
    FCompleteBufferCmd                  : SystemString;
    FCompleteBufferReceiveStream        : TMemoryStream64;
    FCurrentQueueData                   : PQueueData;
    FWaitOnResult                       : Boolean;
    FCurrentPauseResultSend_CommDataType: Byte;
    FCanPauseResultSend                 : Boolean;
    FPauseResultSend                    : Boolean;
    FReceiveTriggerRuning               : Boolean;
    FReceiveDataCipherStyle             : TCipherStyle;
    FResultDataBuffer                   : TMemoryStream64;
    FSendDataCipherStyle                : TCipherStyle;
    FAllSendProcessing                  : Boolean;
    FReceiveProcessing                  : Boolean;
    FQueueList                          : TCoreClassList;
    FLastCommunicationTimeTickCount     : TTimeTickValue;
    FCipherKey                          : TCipherKeyBuffer;
    FRemoteExecutedForConnectInit       : Boolean;
    FInCmd                              : SystemString;
    FInText, FOutText                   : SystemString;
    FInDataFrame, FOutDataFrame         : TDataFrameEngine;
    ResultText                          : SystemString;
    ResultDataFrame                     : TDataFrameEngine;
    FSyncPick                           : PQueueData;
    FWaitSendBusy                       : Boolean;
    FReceiveCommandRuning               : Boolean;
    FReceiveResultRuning                : Boolean;
  private
    // private vm and protocol stack support
    FP2PVMTunnel                  : TCommunicationFrameworkWithP2PVM;
    OnInternalSendByteBuffer      : TInternalSendByteBuffer;
    OnInternalSaveReceiveBuffer   : TInternalSaveReceiveBuffer;
    OnInternalProcessReceiveBuffer: TInternalProcessReceiveBuffer;
    OnInternalProcessAllSendCmd   : TInternalProcessAllSendCmd;
    OnCreate                      : TInternalClientCreate;
    OnDestroy                     : TInternalClientDestory;
  private
    // p2p vm auth result support
    OnVMAuthResultCall  : TStateCall;
    OnVMAuthResultMethod: TStateMethod;
    {$IFNDEF FPC}
    OnVMAuthResultProc  : TStateProc;
    {$ENDIF}
    procedure P2PVMAuthSuccess(Sender: TCommunicationFrameworkWithP2PVM);
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
    // stream64 interface: triggerWrite
    procedure TriggerWrite64(Count: Int64);
  private
    procedure InternalSendByteBuffer(const buff: PByte; Siz: NativeInt); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    //
    procedure SendInteger(v: Integer); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure SendCardinal(v: Cardinal); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure SendInt64(v: Int64); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure SendByte(v: Byte); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure SendWord(v: Word); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure SendVerifyCode(buff: Pointer; Siz: NativeInt); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure SendEncryptBuffer(buff: PByte; Siz: NativeInt; cs: TCipherStyle); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure SendEncryptMemoryStream(stream: TMemoryStream64; cs: TCipherStyle); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    //
    procedure InternalSendConsoleBuff(buff: TMemoryStream64; cs: TCipherStyle); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure InternalSendStreamBuff(buff: TMemoryStream64; cs: TCipherStyle); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure InternalSendDirectConsoleBuff(buff: TMemoryStream64; cs: TCipherStyle); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure InternalSendDirectStreamBuff(buff: TMemoryStream64; cs: TCipherStyle); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure InternalSendBigStreamHeader(Cmd: SystemString; streamSiz: Int64); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure InternalSendBigStreamBuff(var Queue: TQueueData); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure InternalSendCompleteBufferHeader(Cmd: SystemString; buffSiz: NativeInt); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure InternalSendCompleteBufferBuff(var Queue: TQueueData); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    //
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
    procedure ExecuteDataFrame(ACurrentActiveThread: TCoreClassThread; const Sync: Boolean; CommDataType: Byte; dataFrame: TDataFrameEngine);

    procedure Sync_ExecuteBigStream;
    function FillBigStreamBuffer(ACurrentActiveThread: TCoreClassThread; const Sync: Boolean): Boolean;

    procedure Sync_ExecuteCompleteBuffer;
    function FillCompleteBufferBuffer(ACurrentActiveThread: TCoreClassThread; const Sync: Boolean): Boolean;

    procedure Sync_ExecuteResult;
    function FillWaitOnResultBuffer(ACurrentActiveThread: TCoreClassThread; const RecvSync, SendSync: Boolean): Boolean;

    procedure InternalSaveReceiveBuffer(const buff: Pointer; Siz: Int64); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure InternalProcessReceiveBuffer(const ACurrentActiveThread: TCoreClassThread; const RecvSync, SendSync: Boolean); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure InternalProcessAllSendCmd(const ACurrentActiveThread: TCoreClassThread; const RecvSync, SendSync: Boolean); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    //
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

    property p2pVM: TCommunicationFrameworkWithP2PVM read FP2PVMTunnel;
    property p2pVMTunnel: TCommunicationFrameworkWithP2PVM read FP2PVMTunnel;
    procedure OpenP2PVMTunnel(vmHashPoolLen: Integer; SendRemoteRequest: Boolean); overload;
    procedure OpenP2PVMTunnel(SendRemoteRequest: Boolean); overload;
    procedure OpenP2PVMTunnel(SendRemoteRequest: Boolean; OnResult: TStateCall); overload;
    procedure OpenP2PVMTunnel(SendRemoteRequest: Boolean; OnResult: TStateMethod); overload;
    {$IFNDEF FPC}
    procedure OpenP2PVMTunnel(SendRemoteRequest: Boolean; OnResult: TStateProc); overload;
    {$ENDIF}
    procedure OpenP2PVMTunnel; overload;
    procedure CloseP2PVMTunnel;

    procedure Print(v: SystemString); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure Print(v: SystemString; const Args: array of const); overload;
    procedure PrintCommand(v: SystemString; Args: SystemString); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure PrintParam(v: SystemString; Args: SystemString); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    //
    procedure Progress; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    //
    procedure SaveReceiveBuffer(const p: Pointer; Siz: Int64); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure FillRecvBuffer(const ACurrentActiveThread: TCoreClassThread; const RecvSync, SendSync: Boolean); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure ProcessAllSendCmd(const ACurrentActiveThread: TCoreClassThread; const RecvSync, SendSync: Boolean); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure PostQueueData(p: PQueueData); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    //
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
    procedure GenerateHashCode(hs: THashStyle; buff: Pointer; Siz: Integer; var output: TBytes);
    function VerifyHashCode(hs: THashStyle; buff: Pointer; Siz: Integer; var code: TBytes): Boolean;
    // encrypt
    procedure Encrypt(cs: TCipherStyle; DataPtr: Pointer; Size: Cardinal; var k: TCipherKeyBuffer; Enc: Boolean);

    // timeout
    function StopCommunicationTime: TTimeTickValue;
    procedure SetLastCommunicationTimeAsCurrent;

    // queue data
    property CurrentQueueData: PQueueData read FCurrentQueueData;

    // send cmd and method return
    procedure SendConsoleCmd(Cmd: SystemString; ConsoleData: SystemString; OnResult: TConsoleMethod); overload;
    procedure SendStreamCmd(Cmd: SystemString; StreamData: TCoreClassStream; OnResult: TStreamMethod; DoneAutoFree: Boolean); overload;
    procedure SendStreamCmd(Cmd: SystemString; StreamData: TDataFrameEngine; OnResult: TStreamMethod); overload;
    procedure SendStreamCmd(Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamMethod); overload;

    // send cmd and proc return
    {$IFNDEF FPC}
    procedure SendConsoleCmd(Cmd: SystemString; ConsoleData: SystemString; OnResult: TConsoleProc); overload;
    procedure SendStreamCmd(Cmd: SystemString; StreamData: TCoreClassStream; OnResult: TStreamProc; DoneAutoFree: Boolean); overload;
    procedure SendStreamCmd(Cmd: SystemString; StreamData: TDataFrameEngine; OnResult: TStreamProc); overload;
    procedure SendStreamCmd(Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamProc); overload;
    {$ENDIF}
    //
    // direct send cmd
    procedure SendDirectConsoleCmd(Cmd: SystemString; ConsoleData: SystemString);
    procedure SendDirectStreamCmd(Cmd: SystemString; StreamData: TCoreClassStream; DoneAutoFree: Boolean); overload;
    procedure SendDirectStreamCmd(Cmd: SystemString; StreamData: TDataFrameEngine); overload;
    procedure SendDirectStreamCmd(Cmd: SystemString); overload;

    // wait send cmd
    function WaitSendConsoleCmd(Cmd: SystemString; ConsoleData: SystemString; TimeOut: TTimeTickValue): SystemString;
    procedure WaitSendStreamCmd(Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; TimeOut: TTimeTickValue);

    // send bigstream
    procedure SendBigStream(Cmd: SystemString; BigStream: TCoreClassStream; DoneAutoFree: Boolean);

    // send complete buffer
    procedure SendCompleteBuffer(Cmd: SystemString; buff: PByte; buffSize: NativeInt; DoneAutoFree: Boolean);
  end;

  TPeerClientNotify    = procedure(Sender: TPeerClient) of object;
  TPeerClientCMDNotify = procedure(Sender: TPeerClient; Cmd: SystemString; var Allow: Boolean) of object;

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
    FQuietMode                 : Boolean;
    FCipherStyle               : TCipherStyle;
    FHashStyle                 : THashStyle;
    FMaxCompleteBufferSize     : NativeInt;
    FPrintParams               : THashVariantList;
    FProgressPost              : TNProgressPostWithCadencer;
    FFrameworkIsServer         : Boolean;
    FFrameworkIsClient         : Boolean;
    FFrameworkInfo             : string;
  protected
    procedure DoPrint(const v: SystemString); virtual;

    function GetIdleTimeout: TTimeTickValue; virtual;
    procedure SetIdleTimeout(const Value: TTimeTickValue); virtual;

    procedure DoConnected(Sender: TPeerClient); virtual;
    procedure DoDisconnect(Sender: TPeerClient); virtual;

    function CanExecuteCommand(Sender: TPeerClient; Cmd: SystemString): Boolean; virtual;
    function CanSendCommand(Sender: TPeerClient; Cmd: SystemString): Boolean; virtual;
    function CanRegCommand(Sender: TCommunicationFramework; Cmd: SystemString): Boolean; virtual;

    procedure DelayExecuteOnResultState(Sender: TNPostExecute);
  protected
    // private vm and protocol stack support
    procedure InternalSendByteBuffer(const Sender: TPeerClient; const buff: PByte; Siz: NativeInt); virtual;
    procedure InternalSaveReceiveBuffer(const Sender: TPeerClient; const buff: Pointer; Siz: Int64); virtual;
    procedure InternalProcessReceiveBuffer(const Sender: TPeerClient; const ACurrentActiveThread: TCoreClassThread; const RecvSync, SendSync: Boolean); virtual;
    procedure InternalProcessAllSendCmd(const Sender: TPeerClient; const ACurrentActiveThread: TCoreClassThread; const RecvSync, SendSync: Boolean); virtual;
    procedure InternalClientCreate(const Sender: TPeerClient); virtual;
    procedure InternalClientDestroy(const Sender: TPeerClient); virtual;

    procedure Command_InitP2PTunnel(Sender: TPeerClient; InData: SystemString);
    procedure Command_CloseP2PTunnel(Sender: TPeerClient; InData: SystemString);

    procedure VMAuthSuccessAfterDelayExecute(Sender: TNPostExecute);
    procedure VMAuthSuccessDelayExecute(Sender: TNPostExecute);
    procedure VMAuthFailedDelayExecute(Sender: TNPostExecute);
  public
    Statistics                    : array [TStatisticsType] of Int64;
    CmdRecvStatistics             : THashVariantList;
    CmdSendStatistics             : THashVariantList;
    CmdMaxExecuteConsumeStatistics: THashVariantList;

    constructor Create(HashPoolLen: Integer);
    destructor Destroy; override;

    procedure p2pVMTunnelOpenBefore(Sender: TPeerClient; p2pVMTunnel: TCommunicationFrameworkWithP2PVM); virtual;
    procedure p2pVMTunnelOpen(Sender: TPeerClient; p2pVMTunnel: TCommunicationFrameworkWithP2PVM); virtual;
    procedure p2pVMTunnelOpenAfter(Sender: TPeerClient; p2pVMTunnel: TCommunicationFrameworkWithP2PVM); virtual;
    procedure p2pVMTunnelClose(Sender: TPeerClient; p2pVMTunnel: TCommunicationFrameworkWithP2PVM); virtual;

    procedure SwitchMaxPerformance;
    procedure SwitchMaxSafe;
    procedure SwitchDefaultPerformance;

    procedure LockClients;
    procedure UnLockClients;

    property ProgressPost: TNProgressPostWithCadencer read FProgressPost;

    property FrameworkIsServer: Boolean read FFrameworkIsServer;
    property FrameworkIsClient: Boolean read FFrameworkIsClient;
    property FrameworkInfo: string read FFrameworkInfo;

    procedure ProgressBackground; virtual;

    procedure ProgressPerClient(OnProgress: TPerClientListCall); overload;
    procedure ProgressPerClient(OnProgress: TPerClientListMethod); overload;
    {$IFNDEF FPC}
    procedure ProgressPerClient(OnProgress: TPerClientListProc); overload;
    {$ENDIF}
    //
    procedure FastProgressPerClient(OnProgress: TPerClientListCall); overload;
    procedure FastProgressPerClient(OnProgress: TPerClientListMethod); overload;
    {$IFNDEF FPC}
    procedure FastProgressPerClient(OnProgress: TPerClientListProc); overload;
    {$ENDIF}
    //
    procedure GetClientIDPool(out IDPool: TClientIDPool); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    //
    procedure ProgressWaitSendOfClient(Client: TPeerClient); virtual;

    procedure PrintParam(v: SystemString; Args: SystemString); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    //
    function DeleteRegistedCMD(Cmd: SystemString): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function UnRegisted(Cmd: SystemString): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    //
    function RegisterConsole(Cmd: SystemString): TCommandConsoleMode; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function RegisterStream(Cmd: SystemString): TCommandStreamMode; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function RegisterDirectStream(Cmd: SystemString): TCommandDirectStreamMode; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function RegisterDirectConsole(Cmd: SystemString): TCommandDirectConsoleMode; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function RegisterBigStream(Cmd: SystemString): TCommandBigStreamMode; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function RegisterCompleteBuffer(Cmd: SystemString): TCommandCompleteBufferMode; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    //
    function ExecuteConsole(Sender: TPeerClient; Cmd: SystemString; const InData: SystemString; var OutData: SystemString): Boolean; virtual;
    function ExecuteStream(Sender: TPeerClient; Cmd: SystemString; InData, OutData: TDataFrameEngine): Boolean; virtual;
    function ExecuteDirectStream(Sender: TPeerClient; Cmd: SystemString; InData: TDataFrameEngine): Boolean; virtual;
    function ExecuteDirectConsole(Sender: TPeerClient; Cmd: SystemString; const InData: SystemString): Boolean; virtual;
    function ExecuteBigStream(Sender: TPeerClient; Cmd: SystemString; InData: TCoreClassStream; FBigStreamTotal, BigStreamCompleteSize: Int64): Boolean; virtual;
    function ExecuteCompleteBuffer(Sender: TPeerClient; Cmd: SystemString; InData: PByte; DataSize: NativeInt): Boolean; virtual;

    function FirstClient: TPeerClient; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function LastClient: TPeerClient; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    property OnConnected: TPeerClientNotify read FOnConnected write FOnConnected;
    property OnDisconnect: TPeerClientNotify read FOnDisconnect write FOnDisconnect;
    property OnExecuteCommand: TPeerClientCMDNotify read FOnExecuteCommand write FOnExecuteCommand;
    property OnSendCommand: TPeerClientCMDNotify read FOnSendCommand write FOnSendCommand;

    // p2p options
    property UsedParallelEncrypt: Boolean read FUsedParallelEncrypt write FUsedParallelEncrypt;
    property SyncOnResult: Boolean read FSyncOnResult write FSyncOnResult;
    property AllowPrintCommand: Boolean read FQuietMode write FQuietMode;
    property QuietMode: Boolean read FQuietMode write FQuietMode;
    property CipherStyle: TCipherStyle read FCipherStyle;
    property IdleTimeout: TTimeTickValue read GetIdleTimeout write SetIdleTimeout;
    property SendDataCompressed: Boolean read FSendDataCompressed;
    property HashStyle: THashStyle read FHashStyle;
    property MaxCompleteBufferSize: NativeInt read FMaxCompleteBufferSize write FMaxCompleteBufferSize;

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

    procedure InternalClientCreate(const Sender: TPeerClient); override;
    procedure InternalClientDestroy(const Sender: TPeerClient); override;
  public
    constructor Create; overload; virtual;
    constructor Create(HashPoolLen: Integer); overload; virtual;
    destructor Destroy; override;

    procedure StopService; virtual;
    function StartService(Host: SystemString; Port: Word): Boolean; virtual;
    procedure TriggerQueueData(v: PQueueData); virtual;

    // service framework support
    procedure DoClientConnectBefore(Sender: TPeerClient); virtual;
    procedure DoClientConnectAfter(Sender: TPeerClient); virtual;
    procedure DoClientDisconnect(Sender: TPeerClient); virtual;

    // send cmd method
    procedure SendConsoleCmd(Client: TPeerClient; Cmd: SystemString; ConsoleData: SystemString; OnResult: TConsoleMethod); overload;
    procedure SendStreamCmd(Client: TPeerClient; Cmd: SystemString; StreamData: TCoreClassStream; OnResult: TStreamMethod; DoneAutoFree: Boolean); overload;
    procedure SendStreamCmd(Client: TPeerClient; Cmd: SystemString; StreamData: TDataFrameEngine; OnResult: TStreamMethod); overload;
    procedure SendStreamCmd(Client: TPeerClient; Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamMethod); overload;

    // send cmd proc
    {$IFNDEF FPC}
    procedure SendConsoleCmd(Client: TPeerClient; Cmd: SystemString; ConsoleData: SystemString; OnResult: TConsoleProc); overload;
    procedure SendStreamCmd(Client: TPeerClient; Cmd: SystemString; StreamData: TCoreClassStream; OnResult: TStreamProc; DoneAutoFree: Boolean); overload;
    procedure SendStreamCmd(Client: TPeerClient; Cmd: SystemString; StreamData: TDataFrameEngine; OnResult: TStreamProc); overload;
    procedure SendStreamCmd(Client: TPeerClient; Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamProc); overload;
    {$ENDIF}
    // send direct cmd
    procedure SendDirectConsoleCmd(Client: TPeerClient; Cmd: SystemString; ConsoleData: SystemString); overload;
    procedure SendDirectStreamCmd(Client: TPeerClient; Cmd: SystemString; StreamData: TCoreClassStream; DoneAutoFree: Boolean); overload;
    procedure SendDirectStreamCmd(Client: TPeerClient; Cmd: SystemString; StreamData: TDataFrameEngine); overload;
    procedure SendDirectStreamCmd(Client: TPeerClient; Cmd: SystemString); overload;

    // wait send
    function WaitSendConsoleCmd(Client: TPeerClient; Cmd: SystemString; ConsoleData: SystemString; TimeOut: TTimeTickValue): SystemString; overload; virtual;
    procedure WaitSendStreamCmd(Client: TPeerClient; Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; TimeOut: TTimeTickValue); overload; virtual;

    // send bigstream
    procedure SendBigStream(Client: TPeerClient; Cmd: SystemString; BigStream: TCoreClassStream; DoneAutoFree: Boolean); overload;

    // send complete buffer
    procedure SendCompleteBuffer(Client: TPeerClient; Cmd: SystemString; buff: PByte; buffSize: NativeInt; DoneAutoFree: Boolean); overload;

    // send used client ID,return method
    procedure SendConsoleCmd(ClientID: Cardinal; Cmd: SystemString; ConsoleData: SystemString; OnResult: TConsoleMethod); overload;
    procedure SendStreamCmd(ClientID: Cardinal; Cmd: SystemString; StreamData: TCoreClassStream; OnResult: TStreamMethod; DoneAutoFree: Boolean); overload;
    procedure SendStreamCmd(ClientID: Cardinal; Cmd: SystemString; StreamData: TDataFrameEngine; OnResult: TStreamMethod); overload;
    procedure SendStreamCmd(ClientID: Cardinal; Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamMethod); overload;

    // send used client ID,return proc
    {$IFNDEF FPC}
    procedure SendConsoleCmd(ClientID: Cardinal; Cmd: SystemString; ConsoleData: SystemString; OnResult: TConsoleProc); overload;
    procedure SendStreamCmd(ClientID: Cardinal; Cmd: SystemString; StreamData: TCoreClassStream; OnResult: TStreamProc; DoneAutoFree: Boolean); overload;
    procedure SendStreamCmd(ClientID: Cardinal; Cmd: SystemString; StreamData: TDataFrameEngine; OnResult: TStreamProc); overload;
    procedure SendStreamCmd(ClientID: Cardinal; Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamProc); overload;
    {$ENDIF}
    // direct send used client ID
    procedure SendDirectConsoleCmd(ClientID: Cardinal; Cmd: SystemString; ConsoleData: SystemString); overload;
    procedure SendDirectStreamCmd(ClientID: Cardinal; Cmd: SystemString; StreamData: TCoreClassStream; DoneAutoFree: Boolean); overload;
    procedure SendDirectStreamCmd(ClientID: Cardinal; Cmd: SystemString; StreamData: TDataFrameEngine); overload;
    procedure SendDirectStreamCmd(ClientID: Cardinal; Cmd: SystemString); overload;

    // wait send
    function WaitSendConsoleCmd(ClientID: Cardinal; Cmd: SystemString; ConsoleData: SystemString; TimeOut: TTimeTickValue): SystemString; overload;
    procedure WaitSendStreamCmd(ClientID: Cardinal; Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; TimeOut: TTimeTickValue); overload;

    // send bigstream
    procedure SendBigStream(ClientID: Cardinal; Cmd: SystemString; BigStream: TCoreClassStream; DoneAutoFree: Boolean); overload;

    // send complete buffer
    procedure SendCompleteBuffer(ClientID: Cardinal; Cmd: SystemString; buff: PByte; buffSize: NativeInt; DoneAutoFree: Boolean); overload;

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
    FAsyncConnectTimeout      : TTimeTickValue;

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

    procedure TriggerDoDisconnect; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Connected: Boolean; virtual;
    function ClientIO: TPeerClient; virtual;
    procedure TriggerQueueData(v: PQueueData); virtual;

    // async connect support
    procedure TriggerDoConnectFailed; virtual;
    procedure TriggerDoConnectFinished; virtual;

    property AsyncConnectTimeout: TTimeTickValue read FAsyncConnectTimeout write FAsyncConnectTimeout;
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
    procedure SendStreamCmd(Cmd: SystemString; StreamData: TCoreClassStream; OnResult: TStreamMethod; DoneAutoFree: Boolean); overload;
    procedure SendStreamCmd(Cmd: SystemString; StreamData: TDataFrameEngine; OnResult: TStreamMethod); overload;
    procedure SendStreamCmd(Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamMethod); overload;

    // send cmd proc
    {$IFNDEF FPC}
    procedure SendConsoleCmd(Cmd: SystemString; ConsoleData: SystemString; OnResult: TConsoleProc); overload;
    procedure SendStreamCmd(Cmd: SystemString; StreamData: TCoreClassStream; OnResult: TStreamProc; DoneAutoFree: Boolean); overload;
    procedure SendStreamCmd(Cmd: SystemString; StreamData: TDataFrameEngine; OnResult: TStreamProc); overload;
    procedure SendStreamCmd(Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamProc); overload;
    {$ENDIF}
    // send direct cmd
    procedure SendDirectConsoleCmd(Cmd: SystemString; ConsoleData: SystemString);
    procedure SendDirectStreamCmd(Cmd: SystemString; StreamData: TCoreClassStream; DoneAutoFree: Boolean); overload;
    procedure SendDirectStreamCmd(Cmd: SystemString; StreamData: TDataFrameEngine); overload;
    procedure SendDirectStreamCmd(Cmd: SystemString); overload;

    // wait send
    function WaitSendConsoleCmd(Cmd: SystemString; ConsoleData: SystemString; TimeOut: TTimeTickValue): SystemString; virtual;
    procedure WaitSendStreamCmd(Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; TimeOut: TTimeTickValue); virtual;

    // send bigstream
    procedure SendBigStream(Cmd: SystemString; BigStream: TCoreClassStream; DoneAutoFree: Boolean);

    // send complete buffer
    procedure SendCompleteBuffer(Cmd: SystemString; buff: PByte; buffSize: NativeInt; DoneAutoFree: Boolean); overload;

    property NotyifyInterface: ICommunicationFrameworkClientInterface read FNotyifyInterface write FNotyifyInterface;
    // remote service ID
    // success ID > 0
    // failed! ID = 0
    function RemoteID: Cardinal; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function RemoteKey: TCipherKeyBuffer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function RemoteInited: Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
  end;

  TCommunicationFrameworkClientClass = class of TCommunicationFrameworkClient;

  Pp2pVMFragmentPackage = ^Tp2pVMFragmentPackage;

  Tp2pVMFragmentPackage = record
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

  TPeerClientWithP2PVM = class(TPeerClient)
  private
    FLinkVM            : TCommunicationFrameworkWithP2PVM;
    FRealSendBuff      : TMemoryStream64;
    FSendQueue         : TCoreClassList;
    FRemote_frameworkID: Cardinal;
    FRemote_p2pID      : Cardinal;
    FIP                : TIPV6;
    FPort              : Word;
    FDestroyTimeNotify : Boolean;
  protected
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
    FLinkVMPool         : TUInt32HashObjectList;
    FFrameworkWithVM_ID : Cardinal;

    procedure ProgressDisconnectClient(PeerClient: TPeerClient);
    // internal Listen state
    function ListenCount: Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function GetListen(const Index: Integer): Pp2pVMListen; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function FindListen(const ipv6: TIPV6; const Port: Word): Pp2pVMListen; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function FindListening(const ipv6: TIPV6; const Port: Word): Pp2pVMListen; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure DeleteListen(const ipv6: TIPV6; const Port: Word); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure ClearListen; {$IFDEF INLINE_ASM} inline; {$ENDIF}
  public
    constructor Create; overload; override;
    constructor Create(HashPoolLen: Integer; frameworkID: Cardinal); overload; virtual;
    destructor Destroy; override;

    procedure ProgressBackground; override;

    procedure TriggerQueueData(v: PQueueData); override;

    procedure CloseAllClient;

    procedure ProgressStopServiceWithPerVM(SenderVM: TCommunicationFrameworkWithP2PVM);
    procedure StopService; override;
    function StartService(Host: SystemString; Port: Word): Boolean; override;

    function WaitSendConsoleCmd(Client: TPeerClient; Cmd: SystemString; ConsoleData: SystemString; TimeOut: TTimeTickValue): SystemString; override;
    procedure WaitSendStreamCmd(Client: TPeerClient; Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; TimeOut: TTimeTickValue); override;
  end;

  TCommunicationFrameworkWithP2PVM_Client = class(TCommunicationFrameworkClient)
  protected
    procedure InternalClientCreate(const Sender: TPeerClient); override;
    procedure InternalClientDestroy(const Sender: TPeerClient); override;
    procedure VMConnectSuccessed(SenderVM: TCommunicationFrameworkWithP2PVM; Remote_frameworkID, Remote_p2pID, frameworkID: Cardinal); virtual;
    procedure VMDisconnect(SenderVM: TCommunicationFrameworkWithP2PVM); virtual;
  protected
    FLinkVM            : TCommunicationFrameworkWithP2PVM;
    FFrameworkWithVM_ID: Cardinal;
    FVMClient          : TPeerClientWithP2PVM;
    FVMConnected       : Boolean;
    FDestroying        : Boolean;

    FOnAsyncConnectNotifyCall  : TStateCall;
    FOnAsyncConnectNotifyMethod: TStateMethod;
    {$IFNDEF FPC}
    FOnAsyncConnectNotifyProc: TStateProc;
    {$ENDIF}
  public
    constructor Create; overload; override;
    constructor Create(frameworkID: Cardinal); overload; virtual;
    destructor Destroy; override;

    procedure TriggerDoConnectFailed; override;
    procedure TriggerDoConnectFinished; override;

    function Connected: Boolean; override;
    function ClientIO: TPeerClient; override;
    procedure ProgressBackground; override;
    procedure TriggerQueueData(v: PQueueData); override;

    procedure AsyncConnect(Addr: SystemString; Port: Word); overload;
    procedure AsyncConnect(Addr: SystemString; Port: Word; OnResult: TStateCall); overload; override;
    procedure AsyncConnect(Addr: SystemString; Port: Word; OnResult: TStateMethod); overload; override;
    {$IFNDEF FPC} procedure AsyncConnect(Addr: SystemString; Port: Word; OnResult: TStateProc); overload; override; {$ENDIF}
    function Connect(Addr: string; Port: Word): Boolean; override;
    procedure Disconnect; override;

    procedure ProgressWaitSendOfClient(Client: TPeerClient); override;

    property LinkVM: TCommunicationFrameworkWithP2PVM read FLinkVM;
    property FrameworkWithVM_ID: Cardinal read FFrameworkWithVM_ID;
    property VMClient: TPeerClientWithP2PVM read FVMClient;
  end;

  TCommunicationFrameworkListCall   = procedure(PeerFramework: TCommunicationFramework);
  TCommunicationFrameworkListMethod = procedure(PeerFramework: TCommunicationFramework) of object;
  {$IFNDEF FPC}
  TCommunicationFrameworkListProc = reference to procedure(PeerFramework: TCommunicationFramework);
  {$ENDIF}

  //
  TOnEcho = record
    OnEchoCall: TStateCall;
    OnEchoMethod: TStateMethod;
    {$IFNDEF FPC}
    OnEchoProc: TStateProc;
    {$ENDIF}
    TimeOut: TTimeTickValue;
  end;

  POnEcho = ^TOnEcho;

  TP2PVMAuthSuccessMethod = procedure(Sender: TCommunicationFrameworkWithP2PVM) of object;

  TCommunicationFrameworkWithP2PVM = class(TCoreClassInterfacedObject)
  private const
    c_p2pVM_echoing             = $01;
    c_p2pVM_echo                = $02;
    c_p2pVM_AuthSuccessed       = $09;
    c_p2pVM_Listen              = $10;
    c_p2pVM_ListenState         = $11;
    c_p2pVM_Connecting          = $20;
    c_p2pVM_ConnectedReponse    = $21;
    c_p2pVM_Disconnect          = $40;
    c_p2pVM_LogicFragmentData   = $54;
    c_p2pVM_PhysicsFragmentData = $64;
  private
    FPhysicsTunnel         : TPeerClient;
    FAuthWaiting           : Boolean;
    FAuthed                : Boolean;
    FAuthSending           : Boolean;
    FFrameworkPool         : TUInt32HashObjectList;
    FFrameworkListenPool   : TCoreClassList;
    FMaxVMFragmentSize     : Cardinal;
    FMinVMProgressSize     : Cardinal;
    FQuietMode             : Boolean;
    FReceiveStream         : TMemoryStream64;
    FSendStream            : TMemoryStream64;
    FWaitEchoList          : TCoreClassList;
    FVMID                  : Cardinal;
    OnAuthSuccessOnesNotify: TP2PVMAuthSuccessMethod;
  private
    procedure Hook_SendByteBuffer(const Sender: TPeerClient; const buff: PByte; Siz: NativeInt);
    procedure Hook_SaveReceiveBuffer(const Sender: TPeerClient; const buff: Pointer; Siz: Int64);
    procedure SyncProcessReceiveBuff;
    procedure Hook_ProcessReceiveBuffer(const Sender: TPeerClient; const ACurrentActiveThread: TCoreClassThread; const RecvSync, SendSync: Boolean);
    procedure Hook_ClientDestroy(const Sender: TPeerClient);

    procedure SendVMBuffer(const buff: Pointer; const Siz: NativeInt); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure DisconnectWithVM(c: TPeerClient); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    //
    procedure ReceivedEchoing(const frameworkID, p2pID: Cardinal; const buff: PByte; const Siz: Cardinal); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure ReceivedEcho(const frameworkID, p2pID: Cardinal; const buff: PByte; const Siz: Cardinal); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure ReceivedListen(const frameworkID, p2pID: Cardinal; const buff: PByte; const Siz: Cardinal); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure ReceivedListenState(const frameworkID, p2pID: Cardinal; const buff: PByte; const Siz: Cardinal); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure ReceivedConnecting(const frameworkID, p2pID: Cardinal; const buff: PByte; const Siz: Cardinal); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure ReceivedConnectedReponse(const frameworkID, p2pID: Cardinal; const buff: PByte; const Siz: Cardinal); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure ReceivedDisconnect(const frameworkID, p2pID: Cardinal; const buff: PByte; const Siz: Cardinal); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure ReceivedLogicFragmentData(const frameworkID, p2pID: Cardinal; const buff: PByte; const Siz: Cardinal); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure ReceivedOriginFragmentData(const frameworkID, p2pID: Cardinal; const buff: PByte; const Siz: Cardinal); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    //
    procedure DoProcessPerClientFragmentSend(PeerClient: TPeerClient);
    procedure DoPerClientClose(PeerClient: TPeerClient);
  public
    constructor Create(HashPoolLen: Integer);
    destructor Destroy; override;

    procedure Progress; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    //
    procedure ProgressCommunicationFramework(OnProgress: TCommunicationFrameworkListCall); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure ProgressCommunicationFramework(OnProgress: TCommunicationFrameworkListMethod); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    {$IFNDEF FPC} procedure ProgressCommunicationFramework(OnProgress: TCommunicationFrameworkListProc); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}    {$ENDIF}
    //
    // p2p VM physics tunnel support
    procedure OpenP2PVMTunnel(c: TPeerClient); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure CloseP2PVMTunnel; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    //
    // p2p VM logic CommunicationFramework support
    procedure InstallLogicFramework(c: TCommunicationFramework); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure UninstallLogicFramework(c: TCommunicationFramework); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function CreateLogicClient: TCommunicationFrameworkWithP2PVM_Client;
    //
    // p2p VM Peformance support
    property MaxVMFragmentSize: Cardinal read FMaxVMFragmentSize write FMaxVMFragmentSize;
    property MinVMProgressSize: Cardinal read FMinVMProgressSize write FMinVMProgressSize;
    property QuietMode: Boolean read FQuietMode write FQuietMode;

    // p2p VM safe Support
    procedure AuthWaiting; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure AuthVM; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    property WasAuthed: Boolean read FAuthed;
    procedure AuthSuccessed; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    //
    // p2p VM echo support
    procedure echoing(const OnEchoPtr: POnEcho; TimeOut: TTimeTickValue); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure echoing(OnResult: TStateCall; TimeOut: TTimeTickValue); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure echoing(OnResult: TStateMethod; TimeOut: TTimeTickValue); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    {$IFNDEF FPC}
    procedure echoing(OnResult: TStateProc; TimeOut: TTimeTickValue); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    {$ENDIF}
    procedure echoBuffer(const buff: Pointer; const Siz: NativeInt);
    //
    // p2p VM simulate with network listen
    procedure Listen(const frameworkID: Cardinal; const ipv6: TIPV6; const Port: Word; const Listening: Boolean); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure ListenState(const frameworkID: Cardinal; const ipv6: TIPV6; const Port: Word; const Listening: Boolean); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    //
    // p2p VM simulate connecting
    procedure Connecting(const Remote_frameworkID, frameworkID, p2pID: Cardinal; const ipv6: TIPV6; const Port: Word); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure ConnectedReponse(const Remote_frameworkID, Remote_p2pID, frameworkID, p2pID: Cardinal); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure Disconnect(const Remote_frameworkID, Remote_p2pID: Cardinal); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    //
    // p2p VM Listen Query
    function ListenCount: Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function GetListen(const Index: Integer): Pp2pVMListen; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function FindListen(const ipv6: TIPV6; const Port: Word): Pp2pVMListen; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function FindListening(const ipv6: TIPV6; const Port: Word): Pp2pVMListen; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure DeleteListen(const ipv6: TIPV6; const Port: Word); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure ClearListen; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    //
    // p2p VM operaton
    procedure CloseAllClientIO; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure CloseAllServerIO; {$IFDEF INLINE_ASM} inline; {$ENDIF}
  end;

  TProgressBackgroundProc = procedure();

var
  // communication data token
  c_DefaultConsoleToken       : Byte = $F1;
  c_DefaultStreamToken        : Byte = $2F;
  c_DefaultDirectConsoleToken : Byte = $F3;
  c_DefaultDirectStreamToken  : Byte = $4F;
  c_DefaultBigStreamToken     : Byte = $F5;
  c_DefaultCompleteBufferToken: Byte = $6F;

  // user custom header verify token
  c_DataHeadToken: Cardinal = $F0F0F0F0;
  // user custom tail verify token
  c_DataTailToken: Cardinal = $F1F1F1F1;

  // dostatus print id
  c_DefaultDoStatusID: Integer = $0FFFFFFF;

  // p2p VM auth
  P2PVMAuthToken: TBytes;

  // global progress backcall
  ProgressBackgroundProc: TProgressBackgroundProc = nil;

procedure DisposeQueueData(v: PQueueData); {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure InitQueueData(var v: TQueueData); {$IFDEF INLINE_ASM} inline; {$ENDIF}
function NewQueueData: PQueueData; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function BuildP2PVMPackage(buffSiz, frameworkID, p2pID: Cardinal; pkType: Byte; buff: PByte): Pp2pVMFragmentPackage; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure FreeP2PVMPackage(p: Pp2pVMFragmentPackage); {$IFDEF INLINE_ASM} inline; {$ENDIF}

function StrToIPv4(const S: umlString; var Success: Boolean): TIPV4; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function IPv4ToStr(const AIcsIPv4Addr: TIPV4): umlString; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function StrToIPv6(const S: umlString; var Success: Boolean; var ScopeID: Cardinal): TIPV6; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function StrToIPv6(const S: umlString; var Success: Boolean): TIPV6; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function IPv6ToStr(const IPv6Addr: TIPV6): umlString; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function IsIPv4(const S: umlString): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function IsIPV6(const S: umlString): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function CompareIPV4(const ip1, ip2: TIPV4): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function CompareIPV6(const ip1, ip2: TIPV6): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}

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
  if v^.DoneAutoFree then
    begin
      try
        if v^.StreamData <> nil then
            DisposeObject(v^.StreamData);

        if v^.BigStream <> nil then
            DisposeObject(v^.BigStream);

        if v^.Buffer <> nil then
          begin
            if v^.BufferSize > 0 then
                FreeMem(v^.Buffer, v^.BufferSize)
            else
                System.FreeMemory(v^.Buffer);
          end;
      except
      end;
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
  v.Buffer := nil;
  v.BufferSize := 0;
  v.DoneAutoFree := True;
  v.Param1 := nil;
  v.Param2 := nil;
end;

function NewQueueData: PQueueData;
begin
  New(Result);
  InitQueueData(Result^);
end;

function BuildP2PVMPackage(buffSiz, frameworkID, p2pID: Cardinal; pkType: Byte; buff: PByte): Pp2pVMFragmentPackage;
var
  p: Pp2pVMFragmentPackage;
begin
  New(p);
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

function StrToIPv6(const S: umlString; var Success: Boolean): TIPV6;
var
  si: Cardinal;
begin
  Result := StrToIPv6(S, Success, si);
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

function CompareIPV4(const ip1, ip2: TIPV4): Boolean;
begin
  Result := PCardinal(@ip1[0])^ = PCardinal(@ip2[0])^;
end;

function CompareIPV6(const ip1, ip2: TIPV6): Boolean;
begin
  Result := (PUInt64(@ip1[0])^ = PUInt64(@ip2[0])^) and (PUInt64(@ip1[4])^ = PUInt64(@ip2[4])^);
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

  FOnExecuteCall := nil;
  FOnExecuteMethod := nil;
  {$IFNDEF FPC}
  FOnExecuteProc := nil;
  {$ENDIF}
end;

destructor TCommandStreamMode.Destroy;
begin
  inherited Destroy;
end;

function TCommandStreamMode.Execute(Sender: TPeerClient; InData, OutData: TDataFrameEngine): Boolean;
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
      {$ENDIF}
    else
        Result := False;
  except
      Result := False;
  end;
end;

constructor TCommandConsoleMode.Create;
begin
  inherited Create;

  FOnExecuteCall := nil;
  FOnExecuteMethod := nil;
  {$IFNDEF FPC}
  FOnExecuteProc := nil;
  {$ENDIF}
end;

destructor TCommandConsoleMode.Destroy;
begin
  inherited Destroy;
end;

function TCommandConsoleMode.Execute(Sender: TPeerClient; InData: SystemString; var OutData: SystemString): Boolean;
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
      {$ENDIF}
    else
        Result := False;
  except
      Result := False;
  end;
end;

constructor TCommandDirectStreamMode.Create;
begin
  inherited Create;

  FOnExecuteCall := nil;
  FOnExecuteMethod := nil;
  {$IFNDEF FPC}
  FOnExecuteProc := nil;
  {$ENDIF}
end;

destructor TCommandDirectStreamMode.Destroy;
begin
  inherited Destroy;
end;

function TCommandDirectStreamMode.Execute(Sender: TPeerClient; InData: TDataFrameEngine): Boolean;
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
      {$ENDIF}
    else
        Result := False;
  except
      Result := False;
  end;
end;

constructor TCommandDirectConsoleMode.Create;
begin
  inherited Create;

  FOnExecuteCall := nil;
  FOnExecuteMethod := nil;
  {$IFNDEF FPC}
  FOnExecuteProc := nil;
  {$ENDIF}
end;

destructor TCommandDirectConsoleMode.Destroy;
begin
  inherited Destroy;
end;

function TCommandDirectConsoleMode.Execute(Sender: TPeerClient; InData: SystemString): Boolean;
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
      {$ENDIF}
    else
        Result := False;
  except
      Result := False;
  end;
end;

constructor TCommandBigStreamMode.Create;
begin
  inherited Create;

  FOnExecuteCall := nil;
  FOnExecuteMethod := nil;
  {$IFNDEF FPC}
  FOnExecuteProc := nil;
  {$ENDIF}
end;

destructor TCommandBigStreamMode.Destroy;
begin
  inherited Destroy;
end;

function TCommandBigStreamMode.Execute(Sender: TPeerClient; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64): Boolean;
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
      {$ENDIF}
    else
        Result := False;
  except
      Result := False;
  end;
end;

constructor TCommandCompleteBufferMode.Create;
begin
  inherited Create;

  FOnExecuteCall := nil;
  FOnExecuteMethod := nil;
  {$IFNDEF FPC}
  FOnExecuteProc := nil;
  {$ENDIF}
end;

destructor TCommandCompleteBufferMode.Destroy;
begin
  inherited Destroy;
end;

function TCommandCompleteBufferMode.Execute(Sender: TPeerClient; InData: PByte; DataSize: NativeInt): Boolean;
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
      {$ENDIF}
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

procedure TPeerClient.P2PVMAuthSuccess(Sender: TCommunicationFrameworkWithP2PVM);
begin
  {$IFDEF FPC}
  FOwnerFramework.ProgressPost.PostExecute(0, @FOwnerFramework.VMAuthSuccessDelayExecute).Data3 := ID;
  {$ELSE}
  FOwnerFramework.ProgressPost.PostExecute(0, FOwnerFramework.VMAuthSuccessDelayExecute).Data3 := ID;
  {$ENDIF}
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

procedure TPeerClient.InternalSendByteBuffer(const buff: PByte; Siz: NativeInt);
begin
  OnInternalSendByteBuffer(Self, buff, Siz);
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

procedure TPeerClient.SendVerifyCode(buff: Pointer; Siz: NativeInt);
var
  headBuff: array [0 .. 2] of Byte;
  code    : TBytes;
begin
  GenerateHashCode(FOwnerFramework.FHashStyle, buff, Siz, code);

  headBuff[0] := Byte(FOwnerFramework.FHashStyle);
  PWord(@headBuff[1])^ := Length(code);
  InternalSendByteBuffer(@headBuff[0], 3);
  InternalSendByteBuffer(@code[0], Length(code));
end;

procedure TPeerClient.SendEncryptBuffer(buff: PByte; Siz: NativeInt; cs: TCipherStyle);
begin
  SendByte(Byte(cs));
  Encrypt(cs, buff, Siz, FCipherKey, True);
  InternalSendByteBuffer(buff, Siz);
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

      LockObject(Queue.BigStream); // atomic lock
      try
        Queue.BigStream.Position := tmpPos;
        Queue.BigStream.Read(buff[0], ChunkSize);
        tmpPos := tmpPos + ChunkSize;
      except
      end;
      UnLockObject(Queue.BigStream); // atomic lock

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
          FBigStreamSendDoneTimeFree := Queue.DoneAutoFree;
          Queue.BigStream := nil;
          Exit;
        end;
    end;

  { Process remaining bytes }
  if Rest > 0 then
    begin
      LockObject(Queue.BigStream); // atomic lock
      try
        Queue.BigStream.Position := tmpPos;
        Queue.BigStream.Read(buff[0], Rest);
        tmpPos := tmpPos + Rest;
      except
      end;
      UnLockObject(Queue.BigStream); // atomic lock

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

procedure TPeerClient.InternalSendCompleteBufferHeader(Cmd: SystemString; buffSiz: NativeInt);
var
  buff: TBytes;
begin
  WriteBufferOpen;
  SendCardinal(FHeadToken);
  SendByte(FCompleteBufferToken);
  SendInt64(buffSiz);
  buff := TPascalString(Cmd).Bytes;
  SendCardinal(Cardinal(Length(buff)));
  InternalSendByteBuffer(@buff[0], Length(buff));
  SendCardinal(FTailToken);
  WriteBufferFlush;
  WriteBufferClose;
end;

procedure TPeerClient.InternalSendCompleteBufferBuff(var Queue: TQueueData);
begin
  InternalSendCompleteBufferHeader(Queue.Cmd, Queue.BufferSize);
  WriteBufferOpen;
  InternalSendByteBuffer(Queue.Buffer, Queue.BufferSize);
  WriteBufferFlush;
  WriteBufferClose;
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

procedure TPeerClient.Sync_InternalSendCompleteBufferCmd;
begin
  InternalSendCompleteBufferBuff(FSyncPick^);
  Inc(FOwnerFramework.Statistics[TStatisticsType.stExecCompleteBuffer]);
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

      FReceiveTriggerRuning := True;
      {$IFDEF FPC}
      SyncMethod(ACurrentActiveThread, Sync, @Sync_ExecuteConsole);
      {$ELSE}
      SyncMethod(ACurrentActiveThread, Sync, Sync_ExecuteConsole);
      {$ENDIF}
      FReceiveTriggerRuning := False;

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

      FReceiveTriggerRuning := True;
      {$IFDEF FPC}
      SyncMethod(ACurrentActiveThread, Sync, @Sync_ExecuteStream);
      {$ELSE}
      SyncMethod(ACurrentActiveThread, Sync, Sync_ExecuteStream);
      {$ENDIF}
      FReceiveTriggerRuning := False;

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

      FReceiveTriggerRuning := True;
      {$IFDEF FPC}
      SyncMethod(ACurrentActiveThread, Sync, @Sync_ExecuteDirectConsole);
      {$ELSE}
      SyncMethod(ACurrentActiveThread, Sync, Sync_ExecuteDirectConsole);
      {$ENDIF}
      FReceiveTriggerRuning := False;

      if not Connected then
          Exit;
    end
  else if CommDataType = FDirectStreamToken then
    begin
      FInDataFrame.Clear;
      FOutDataFrame.Clear;
      dataFrame.Reader.ReadDataFrame(FInDataFrame);

      FReceiveTriggerRuning := True;
      {$IFDEF FPC}
      SyncMethod(ACurrentActiveThread, Sync, @Sync_ExecuteDirectStream);
      {$ELSE}
      SyncMethod(ACurrentActiveThread, Sync, Sync_ExecuteDirectStream);
      {$ENDIF}
      FReceiveTriggerRuning := False;

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

  if FBigStreamTotal = FBigStreamCompleted then
    begin
      FOwnerFramework.CmdRecvStatistics.IncValue(FBigStreamCmd, 1);
      PrintCommand('Big Stream complete with cmd:%s', FBigStreamCmd);
    end;
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
      tmpStream.Clear;
      if FReceivedBuffer.Size - leftSize > 0 then
          tmpStream.WritePtr(FReceivedBuffer.PositionAsPtr(leftSize), FReceivedBuffer.Size - leftSize);
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

procedure TPeerClient.Sync_ExecuteCompleteBuffer;
var
  d: TTimeTickValue;
begin
  FReceiveCommandRuning := True;
  d := GetTimeTickCount;

  FOwnerFramework.ExecuteCompleteBuffer(Self, FCompleteBufferCmd, FCompleteBufferReceiveStream.Memory, FCompleteBufferReceiveStream.Size);

  FReceiveCommandRuning := False;
  FOwnerFramework.CmdMaxExecuteConsumeStatistics.SetMax(FInCmd, GetTimeTickCount - d);

  FOwnerFramework.CmdRecvStatistics.IncValue(FCompleteBufferCmd, 1);
  PrintCommand('execute complete buffer cmd:%s', FCompleteBufferCmd);
end;

function TPeerClient.FillCompleteBufferBuffer(ACurrentActiveThread: TCoreClassThread; const Sync: Boolean): Boolean;
var
  leftSize : Int64;
  tmpStream: TMemoryStream64OfWriteTrigger;
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

      {$IFDEF FPC}
      SyncMethod(ACurrentActiveThread, Sync, @Sync_ExecuteCompleteBuffer);
      {$ELSE}
      SyncMethod(ACurrentActiveThread, Sync, Sync_ExecuteCompleteBuffer);
      {$ENDIF}
      tmpStream := TMemoryStream64OfWriteTrigger.Create(nil);
      if FReceivedBuffer.Size - leftSize > 0 then
          tmpStream.WritePtr(FReceivedBuffer.PositionAsPtr(leftSize), FReceivedBuffer.Size - leftSize);
      DisposeObject(FReceivedBuffer);
      FReceivedBuffer := tmpStream;
      FReceivedBuffer.Trigger := Self;
      Result := True;

      FCompleteBufferTotal := 0;
      FCompleteBufferCompleted := 0;
      FCompleteBufferCmd := '';
      FCompleteBufferReceiveProcessing := False;
      FCompleteBufferReceiveStream.Clear;

      FReceivedBuffer.Position := 0;
    end;
end;

procedure TPeerClient.Sync_ExecuteResult;
var
  nQueue: PQueueData;
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

procedure TPeerClient.InternalSaveReceiveBuffer(const buff: Pointer; Siz: Int64);
begin
  LockObject(Self); // atomic lock
  try
    FReceivedBuffer.Position := FReceivedBuffer.Size;
    FReceivedBuffer.WritePtr(buff, Siz);
  finally
      UnLockObject(Self); // atomic lock
  end;
end;

procedure TPeerClient.InternalProcessReceiveBuffer(const ACurrentActiveThread: TCoreClassThread; const RecvSync, SendSync: Boolean);
var
  dHead, dTail    : Cardinal;
  dID             : Byte;
  dSize           : Cardinal;
  dHashStyle      : THashStyle;
  dHashSiz        : Word;
  dHash           : TBytes;
  dCipherStyle    : Byte;
  tmpStream       : TMemoryStream64OfWriteTrigger;
  df              : TDataFrameEngine;
  buff            : TBytes;
  Total           : Int64;
  NeedDisconnected: Boolean;
begin
  if FAllSendProcessing then
      Exit;
  if FReceiveProcessing then
      Exit;
  if FPauseResultSend then
      Exit;
  if FResultDataBuffer.Size > 0 then
      Exit;
  if FReceiveTriggerRuning then
      Exit;
  if FBigStreamSending <> nil then
      Exit;

  LockObject(Self); // atomic lock
  FReceiveProcessing := True;
  NeedDisconnected := False;
  try
    while (FReceivedBuffer.Size > 0) and (Connected) do
      begin

        FLastCommunicationTimeTickCount := GetTimeTickCount;

        FReceivedBuffer.Position := 0;

        if FWaitOnResult then
          begin
            UnLockObject(Self); // atomic lock
            if FillWaitOnResultBuffer(ACurrentActiveThread, RecvSync, SendSync) then
              begin
                LockObject(Self); // atomic lock
                Continue;
              end
            else
              begin
                LockObject(Self); // atomic lock
                break;
              end;
          end;

        if FBigStreamReceiveProcessing then
          begin
            UnLockObject(Self); // atomic lock
            if FillBigStreamBuffer(ACurrentActiveThread, RecvSync) then
              begin
                LockObject(Self); // atomic lock
                Continue;
              end
            else
              begin
                LockObject(Self); // atomic lock
                break;
              end;
          end;

        if FCompleteBufferReceiveProcessing then
          begin
            UnLockObject(Self); // atomic lock
            if FillCompleteBufferBuffer(ACurrentActiveThread, RecvSync) then
              begin
                LockObject(Self); // atomic lock
                Continue;
              end
            else
              begin
                LockObject(Self); // atomic lock
                break;
              end;
          end;

        // 0: head token
        if (FReceivedBuffer.Size - FReceivedBuffer.Position < umlCardinalLength) then
            break;
        FReceivedBuffer.Read(dHead, umlCardinalLength);
        if dHead <> FHeadToken then
          begin
            NeedDisconnected := True;
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
                NeedDisconnected := True;
                break;
              end;

            FReceiveDataCipherStyle := TCipherStyle(dCipherStyle);

            try
                Encrypt(FReceiveDataCipherStyle, tmpStream.Memory, tmpStream.Size, FCipherKey, False);
            except
              Print('Encrypt error!');
              DisposeObject(tmpStream);
              NeedDisconnected := True;
              break;
            end;

            if not VerifyHashCode(dHashStyle, tmpStream.Memory, tmpStream.Size, dHash) then
              begin
                Print('verify data error!');
                DisposeObject(tmpStream);
                NeedDisconnected := True;
                break;
              end;

            df := TDataFrameEngine.Create;
            tmpStream.Position := 0;
            try
                df.DecodeFrom(tmpStream, True);
            except
              Print('DECode dataFrame error!');
              DisposeObject(tmpStream);
              NeedDisconnected := True;
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

            UnLockObject(Self); // atomic lock
            try
                ExecuteDataFrame(ACurrentActiveThread, RecvSync, dID, df);
            except
            end;
            LockObject(Self); // atomic lock
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
                NeedDisconnected := True;
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
        else if dID = FCompleteBufferToken then
          begin
            // 2:complete buff size
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
                NeedDisconnected := True;
                break;
              end;

            if (FOwnerFramework.FMaxCompleteBufferSize > 0) and (Total > FOwnerFramework.FMaxCompleteBufferSize) then
              begin
                Print('Oversize of CompleteBuffer cmd: ' + PascalStringOfBytes(buff).Text);
                NeedDisconnected := True;
                break;
              end;

            FCompleteBufferTotal := Total;
            FCompleteBufferCompleted := 0;
            FCompleteBufferCmd := PascalStringOfBytes(buff).Text;
            FCompleteBufferReceiveProcessing := True;
            FCompleteBufferReceiveStream.Clear;
            SetLength(buff, 0);

            // stripped stream
            tmpStream := TMemoryStream64OfWriteTrigger.Create(nil);
            if FReceivedBuffer.Size - FReceivedBuffer.Position > 0 then
                tmpStream.CopyFrom(FReceivedBuffer, FReceivedBuffer.Size - FReceivedBuffer.Position);
            DisposeObject(FReceivedBuffer);
            FReceivedBuffer := tmpStream;
            FReceivedBuffer.Trigger := Self;

            Inc(FOwnerFramework.Statistics[TStatisticsType.stReceiveCompleteBuffer]);
          end
        else
          begin
            NeedDisconnected := True;
            break;
          end;
      end;
  finally
    FReceivedBuffer.Position := FReceivedBuffer.Size;
    FReceiveProcessing := False;
    UnLockObject(Self); // atomic lock

    if NeedDisconnected then
        Disconnect;
  end;
end;

procedure TPeerClient.InternalProcessAllSendCmd(const ACurrentActiveThread: TCoreClassThread; const RecvSync, SendSync: Boolean);
var
  p: PQueueData;
begin
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

  if FReceiveTriggerRuning then
      Exit;

  FAllSendProcessing := True;

  LockObject(FQueueList); // atomic lock
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

              if FBigStreamSending <> nil then
                  break;
            end;
          qsSendCompleteBuffer:
            begin
              Inc(FOwnerFramework.Statistics[TStatisticsType.stSendCompleteBuffer]);

              FSyncPick := p;
              {$IFDEF FPC}
              SyncMethod(ACurrentActiveThread, SendSync, @Sync_InternalSendCompleteBufferCmd);
              {$ELSE}
              SyncMethod(ACurrentActiveThread, SendSync, Sync_InternalSendCompleteBufferCmd);
              {$ENDIF}
              FSyncPick := nil;

              DisposeQueueData(p);
              FQueueList.Delete(0);
            end;
        end;
      end;
  finally
    UnLockObject(FQueueList); // atomic lock
    FAllSendProcessing := False;
  end;
end;

procedure TPeerClient.InternalCloseP2PVMTunnel;
begin
  if FP2PVMTunnel <> nil then
    begin
      FOwnerFramework.p2pVMTunnelClose(Self, FP2PVMTunnel);
      FP2PVMTunnel.CloseP2PVMTunnel;
      DisposeObject(FP2PVMTunnel);
      FP2PVMTunnel := nil;
    end;
end;

function TPeerClient.WriteBufferEmpty: Boolean;
begin
  Result := True;
end;

constructor TPeerClient.Create(AOwnerFramework: TCommunicationFramework; AClientIntf: TCoreClassObject);
var
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
  FCompleteBufferToken := c_DefaultCompleteBufferToken;

  FReceivedBuffer := TMemoryStream64OfWriteTrigger.Create(Self);
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
  FCompleteBufferCompleted := 0;
  FCompleteBufferCmd := '';
  FCompleteBufferReceiveStream := TMemoryStream64.Create;

  FCurrentQueueData := nil;
  FWaitOnResult := False;
  FPauseResultSend := False;
  FReceiveTriggerRuning := False;
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

  LockObject(FOwnerFramework.FPerClientHashList); // atomic lock
  FOwnerFramework.FPerClientHashList.Add(FID, Self, False);
  UnLockObject(FOwnerFramework.FPerClientHashList); // atomic lock

  Inc(FOwnerFramework.Statistics[TStatisticsType.stTriggerConnect]);

  FP2PVMTunnel := nil;
  {$IFDEF FPC}
  OnInternalSendByteBuffer := @FOwnerFramework.InternalSendByteBuffer;
  OnInternalSaveReceiveBuffer := @FOwnerFramework.InternalSaveReceiveBuffer;
  OnInternalProcessReceiveBuffer := @FOwnerFramework.InternalProcessReceiveBuffer;
  OnInternalProcessAllSendCmd := @FOwnerFramework.InternalProcessAllSendCmd;
  OnCreate := @FOwnerFramework.InternalClientCreate;
  OnDestroy := @FOwnerFramework.InternalClientDestroy;
  {$ELSE}
  OnInternalSendByteBuffer := FOwnerFramework.InternalSendByteBuffer;
  OnInternalSaveReceiveBuffer := FOwnerFramework.InternalSaveReceiveBuffer;
  OnInternalProcessReceiveBuffer := FOwnerFramework.InternalProcessReceiveBuffer;
  OnInternalProcessAllSendCmd := FOwnerFramework.InternalProcessAllSendCmd;
  OnCreate := FOwnerFramework.InternalClientCreate;
  OnDestroy := FOwnerFramework.InternalClientDestroy;
  {$ENDIF}
  //
  OnVMAuthResultCall := nil;
  OnVMAuthResultMethod := nil;
  {$IFNDEF FPC}
  OnVMAuthResultProc := nil;
  {$ENDIF}

  FUserData := nil;
  FUserValue := NULL;
  FUserVariants := nil;
  FUserObjects := nil;
  FUserAutoFreeObjects := nil;
  FUserDefine := FOwnerFramework.FPeerClientUserDefineClass.Create(Self);
  FUserSpecial := FOwnerFramework.FPeerClientUserSpecialClass.Create(Self);

  try
      OnCreate(Self);
  except
  end;

  CreateAfter;
end;

procedure TPeerClient.CreateAfter;
begin
end;

destructor TPeerClient.Destroy;
var
  i: Integer;
begin
  try
      OnDestroy(Self);
  except
  end;

  InternalCloseP2PVMTunnel;

  if (FBigStreamSending <> nil) and (FBigStreamSendDoneTimeFree) then
    begin
      DisposeObject(FBigStreamSending);
      FBigStreamSending := nil;
    end;

  Inc(FOwnerFramework.Statistics[TStatisticsType.stTriggerDisconnect]);

  LockObject(FOwnerFramework.FPerClientHashList); // atomic lock
  FOwnerFramework.FPerClientHashList.Delete(FID);
  UnLockObject(FOwnerFramework.FPerClientHashList); // atomic lock

  LockObject(FQueueList); // atomic lock
  try
    for i := 0 to FQueueList.Count - 1 do
        DisposeQueueData(FQueueList[i]);
    FQueueList.Clear;
  finally
      UnLockObject(FQueueList); // atomic lock
  end;

  DisposeObject([FUserDefine, FUserSpecial]);

  DisposeObject(FQueueList);
  DisposeObject(FReceivedBuffer);
  DisposeObject(FCompleteBufferReceiveStream);
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

procedure TPeerClient.OpenP2PVMTunnel(vmHashPoolLen: Integer; SendRemoteRequest: Boolean);
begin
  if FP2PVMTunnel = nil then
    begin
      FP2PVMTunnel := TCommunicationFrameworkWithP2PVM.Create(vmHashPoolLen);
      FP2PVMTunnel.FVMID := FID;

      FP2PVMTunnel.OpenP2PVMTunnel(Self);
      FP2PVMTunnel.AuthWaiting;

      {$IFDEF FPC}
      FP2PVMTunnel.OnAuthSuccessOnesNotify := @P2PVMAuthSuccess;
      {$ELSE}
      FP2PVMTunnel.OnAuthSuccessOnesNotify := P2PVMAuthSuccess;
      {$ENDIF}

      OnVMAuthResultCall := nil;
      OnVMAuthResultMethod := nil;
      {$IFNDEF FPC}
      OnVMAuthResultProc := nil;
      {$ENDIF}

      if SendRemoteRequest then
        begin
          SendDirectConsoleCmd('InitP2PTunnel', '');
          ProcessAllSendCmd(nil, False, False);
        end;
    end;
end;

procedure TPeerClient.OpenP2PVMTunnel(SendRemoteRequest: Boolean);
begin
  if FOwnerFramework.FFrameworkIsClient then
      OpenP2PVMTunnel(8192, SendRemoteRequest)
  else
      OpenP2PVMTunnel(16, SendRemoteRequest);
end;

procedure TPeerClient.OpenP2PVMTunnel(SendRemoteRequest: Boolean; OnResult: TStateCall);
begin
  OpenP2PVMTunnel(SendRemoteRequest);
  OnVMAuthResultCall := OnResult;
  OnVMAuthResultMethod := nil;
  {$IFNDEF FPC}
  OnVMAuthResultProc := nil;
  {$ENDIF}
  {$IFDEF FPC}
  FOwnerFramework.ProgressPost.PostExecute(3.0, @FOwnerFramework.VMAuthFailedDelayExecute).Data3 := FID;
  {$ELSE}
  FOwnerFramework.ProgressPost.PostExecute(3.0, FOwnerFramework.VMAuthFailedDelayExecute).Data3 := FID;
  {$ENDIF}
end;

procedure TPeerClient.OpenP2PVMTunnel(SendRemoteRequest: Boolean; OnResult: TStateMethod);
begin
  OpenP2PVMTunnel(SendRemoteRequest);
  OnVMAuthResultCall := nil;
  OnVMAuthResultMethod := OnResult;
  {$IFNDEF FPC}
  OnVMAuthResultProc := nil;
  {$ENDIF}
  {$IFDEF FPC}
  FOwnerFramework.ProgressPost.PostExecute(3.0, @FOwnerFramework.VMAuthFailedDelayExecute).Data3 := FID;
  {$ELSE}
  FOwnerFramework.ProgressPost.PostExecute(3.0, FOwnerFramework.VMAuthFailedDelayExecute).Data3 := FID;
  {$ENDIF}
end;

{$IFNDEF FPC}
procedure TPeerClient.OpenP2PVMTunnel(SendRemoteRequest: Boolean; OnResult: TStateProc);
begin
  OpenP2PVMTunnel(SendRemoteRequest);
  OnVMAuthResultCall := nil;
  OnVMAuthResultMethod := nil;
  OnVMAuthResultProc := OnResult;
  FOwnerFramework.ProgressPost.PostExecute(3.0, FOwnerFramework.VMAuthFailedDelayExecute).Data3 := FID;
end;
{$ENDIF}

procedure TPeerClient.OpenP2PVMTunnel;
begin
  OpenP2PVMTunnel(False);
end;

procedure TPeerClient.CloseP2PVMTunnel;
begin
  SendDirectConsoleCmd('CloseP2PTunnel', '');
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
    if (not OwnerFramework.FQuietMode) and (OwnerFramework.FPrintParams.GetDefaultValue(Args, True) = True) then
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

  if FAllSendProcessing then
      Exit;
  if FReceiveProcessing then
      Exit;

  if (FBigStreamSending <> nil) and (WriteBufferEmpty) then
    begin
      SendBufferSize := 1 * 1024 * 1024; // cycle send size 1M

      LockObject(FBigStreamSending); // atomic lock

      try
        SendDone := FBigStreamSending.Size - FBigStreamSendState <= SendBufferSize;

        if SendDone then
            SendBufferSize := FBigStreamSending.Size - FBigStreamSendState;

        SetLength(buff, SendBufferSize);
        FBigStreamSending.Position := FBigStreamSendState;
        FBigStreamSending.Read(buff[0], SendBufferSize);

        Inc(FBigStreamSendState, SendBufferSize);

      except
        UnLockObject(FBigStreamSending); // atomic lock
        Disconnect;
        Exit;
      end;

      UnLockObject(FBigStreamSending); // atomic lock

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

procedure TPeerClient.SaveReceiveBuffer(const p: Pointer; Siz: Int64);
begin
  OnInternalSaveReceiveBuffer(Self, p, Siz);
end;

procedure TPeerClient.FillRecvBuffer(const ACurrentActiveThread: TCoreClassThread; const RecvSync, SendSync: Boolean);
begin
  OnInternalProcessReceiveBuffer(Self, ACurrentActiveThread, RecvSync, SendSync);
end;

procedure TPeerClient.ProcessAllSendCmd(const ACurrentActiveThread: TCoreClassThread; const RecvSync, SendSync: Boolean);
begin
  OnInternalProcessAllSendCmd(Self, ACurrentActiveThread, RecvSync, SendSync);
end;

procedure TPeerClient.PostQueueData(p: PQueueData);
begin
  FOwnerFramework.CmdSendStatistics.IncValue(p^.Cmd, 1);

  LockObject(FQueueList); // atomic lock
  FQueueList.Add(p);
  UnLockObject(FQueueList); // atomic lock
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

procedure TPeerClient.GenerateHashCode(hs: THashStyle; buff: Pointer; Siz: Integer; var output: TBytes);
begin
  TCipher.GenerateHashByte(hs, buff, Siz, output);
  Inc(FOwnerFramework.Statistics[TStatisticsType.stGenerateHash]);
end;

function TPeerClient.VerifyHashCode(hs: THashStyle; buff: Pointer; Siz: Integer; var code: TBytes): Boolean;
var
  buffCode: TBytes;
begin
  try
    GenerateHashCode(hs, buff, Siz, buffCode);
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

procedure TPeerClient.SendStreamCmd(Cmd: SystemString; StreamData: TCoreClassStream; OnResult: TStreamMethod; DoneAutoFree: Boolean);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendStreamCmd(Self, Cmd, StreamData, OnResult, DoneAutoFree)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendStreamCmd(Cmd, StreamData, OnResult, DoneAutoFree);
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

procedure TPeerClient.SendStreamCmd(Cmd: SystemString; StreamData: TCoreClassStream; OnResult: TStreamProc; DoneAutoFree: Boolean);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendStreamCmd(Self, Cmd, StreamData, OnResult, DoneAutoFree)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendStreamCmd(Cmd, StreamData, OnResult, DoneAutoFree);
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

procedure TPeerClient.SendDirectStreamCmd(Cmd: SystemString; StreamData: TCoreClassStream; DoneAutoFree: Boolean);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendDirectStreamCmd(Self, Cmd, StreamData, DoneAutoFree)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendDirectStreamCmd(Cmd, StreamData, DoneAutoFree);
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

procedure TPeerClient.SendBigStream(Cmd: SystemString; BigStream: TCoreClassStream; DoneAutoFree: Boolean);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendBigStream(Self, Cmd, BigStream, DoneAutoFree)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendBigStream(Cmd, BigStream, DoneAutoFree);
end;

procedure TPeerClient.SendCompleteBuffer(Cmd: SystemString; buff: PByte; buffSize: NativeInt; DoneAutoFree: Boolean);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendCompleteBuffer(Self, Cmd, buff, buffSize, DoneAutoFree)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendCompleteBuffer(Cmd, buff, buffSize, DoneAutoFree)
end;

procedure TCommunicationFramework.DoPrint(const v: SystemString);
begin
  if not FQuietMode then
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

procedure TCommunicationFramework.InternalSendByteBuffer(const Sender: TPeerClient; const buff: PByte; Siz: NativeInt);
const
  FlushBuffSize = 16 * 1024; // flush size = 16k byte
var
  p: PByte;
begin
  Sender.FLastCommunicationTimeTickCount := GetTimeTickCount;

  if Siz < 1 then
      Exit;

  p := buff;

  // fill fragment
  while Siz > FlushBuffSize do
    begin
      Sender.SendByteBuffer(p, FlushBuffSize);
      Inc(p, FlushBuffSize);
      Inc(Statistics[TStatisticsType.stSendSize], FlushBuffSize);
      Sender.WriteBufferFlush;
      dec(Siz, FlushBuffSize);
    end;

  if Siz > 0 then
    begin
      Sender.SendByteBuffer(p, Siz);
      Inc(Statistics[TStatisticsType.stSendSize], Siz);
    end;
end;

procedure TCommunicationFramework.InternalSaveReceiveBuffer(const Sender: TPeerClient; const buff: Pointer; Siz: Int64);
begin
  Sender.InternalSaveReceiveBuffer(buff, Siz);
end;

procedure TCommunicationFramework.InternalProcessReceiveBuffer(const Sender: TPeerClient; const ACurrentActiveThread: TCoreClassThread; const RecvSync, SendSync: Boolean);
begin
  Sender.InternalProcessReceiveBuffer(ACurrentActiveThread, RecvSync, SendSync);
end;

procedure TCommunicationFramework.InternalProcessAllSendCmd(const Sender: TPeerClient; const ACurrentActiveThread: TCoreClassThread; const RecvSync, SendSync: Boolean);
begin
  Sender.InternalProcessAllSendCmd(ACurrentActiveThread, RecvSync, SendSync);
end;

procedure TCommunicationFramework.InternalClientCreate(const Sender: TPeerClient);
begin
end;

procedure TCommunicationFramework.InternalClientDestroy(const Sender: TPeerClient);
begin
end;

procedure TCommunicationFramework.Command_InitP2PTunnel(Sender: TPeerClient; InData: SystemString);
begin
  Sender.OpenP2PVMTunnel(16, False);
  Sender.p2pVMTunnel.AuthVM;
  p2pVMTunnelOpenBefore(Sender, Sender.p2pVMTunnel);
end;

procedure TCommunicationFramework.Command_CloseP2PTunnel(Sender: TPeerClient; InData: SystemString);
begin
  Sender.InternalCloseP2PVMTunnel;
end;

procedure TCommunicationFramework.VMAuthSuccessAfterDelayExecute(Sender: TNPostExecute);
var
  pc: TPeerClient;
begin
  pc := TPeerClient(FPerClientHashList[Sender.Data3]);
  if pc = nil then
      Exit;

  try
    if Assigned(pc.OnVMAuthResultCall) then
        pc.OnVMAuthResultCall(True);
    if Assigned(pc.OnVMAuthResultMethod) then
        pc.OnVMAuthResultMethod(True);
    {$IFNDEF FPC}
    if Assigned(pc.OnVMAuthResultProc) then
        pc.OnVMAuthResultProc(True);
    {$ENDIF}
  except
  end;

  pc.OnVMAuthResultCall := nil;
  pc.OnVMAuthResultMethod := nil;
  {$IFNDEF FPC}
  pc.OnVMAuthResultProc := nil;
  {$ENDIF}

  p2pVMTunnelOpenAfter(pc, pc.p2pVMTunnel);
end;

procedure TCommunicationFramework.VMAuthSuccessDelayExecute(Sender: TNPostExecute);
var
  pc: TPeerClient;
begin
  pc := TPeerClient(FPerClientHashList[Sender.Data3]);
  if pc = nil then
      Exit;

  {$IFDEF FPC}
  ProgressPost.PostExecute(1.0, @VMAuthSuccessAfterDelayExecute).Data3 := pc.FID;
  {$ELSE}
  ProgressPost.PostExecute(1.0, VMAuthSuccessAfterDelayExecute).Data3 := pc.FID;
  {$ENDIF}

  p2pVMTunnelOpen(pc, pc.p2pVMTunnel);
end;

procedure TCommunicationFramework.VMAuthFailedDelayExecute(Sender: TNPostExecute);
var
  pc: TPeerClient;
begin
  pc := TPeerClient(FPerClientHashList[Sender.Data3]);
  if pc = nil then
      Exit;

  try
    if Assigned(pc.OnVMAuthResultCall) then
        pc.OnVMAuthResultCall(False);
    if Assigned(pc.OnVMAuthResultMethod) then
        pc.OnVMAuthResultMethod(False);
    {$IFNDEF FPC}
    if Assigned(pc.OnVMAuthResultProc) then
        pc.OnVMAuthResultProc(False);
    {$ENDIF}
  except
  end;

  pc.OnVMAuthResultCall := nil;
  pc.OnVMAuthResultMethod := nil;
  {$IFNDEF FPC}
  pc.OnVMAuthResultProc := nil;
  {$ENDIF}
end;

constructor TCommunicationFramework.Create(HashPoolLen: Integer);
var
  st: TStatisticsType;
begin
  inherited Create;
  FCommandList := THashObjectList.Create(True, 128);
  FIDCounter := 1;
  FPerClientHashList := TUInt32HashObjectList.Create(HashPoolLen);
  FPerClientHashList.AutoFreeData := False;
  FPerClientHashList.AccessOptimization := False;
  FOnConnected := nil;
  FOnDisconnect := nil;
  FOnExecuteCommand := nil;
  FOnSendCommand := nil;
  FIdleTimeout := 0;
  FUsedParallelEncrypt := True;
  FSyncOnResult := False;
  FQuietMode := False;
  FCipherStyle := TCipherStyle.csNone;
  FSendDataCompressed := True;
  FHashStyle := THashStyle.hsNone;
  FMaxCompleteBufferSize := 4 * 1024 * 1024; // 4M
  FPeerClientUserDefineClass := TPeerClientUserDefine;
  FPeerClientUserSpecialClass := TPeerClientUserSpecial;

  FPrintParams := THashVariantList.Create(128);
  FPrintParams.AutoUpdateDefaultValue := True;

  FProgressPost := TNProgressPostWithCadencer.Create;

  FFrameworkIsServer := True;
  FFrameworkIsClient := True;
  FFrameworkInfo := ClassName;

  for st := low(TStatisticsType) to high(TStatisticsType) do
      Statistics[st] := 0;
  CmdRecvStatistics := THashVariantList.Create(32);
  CmdSendStatistics := THashVariantList.Create(32);
  CmdMaxExecuteConsumeStatistics := THashVariantList.Create(32);

  {$IFDEF FPC}
  RegisterDirectConsole('InitP2PTunnel').OnExecute := @Command_InitP2PTunnel;
  RegisterDirectConsole('CloseP2PTunnel').OnExecute := @Command_CloseP2PTunnel;
  {$ELSE}
  RegisterDirectConsole('InitP2PTunnel').OnExecute := Command_InitP2PTunnel;
  RegisterDirectConsole('CloseP2PTunnel').OnExecute := Command_CloseP2PTunnel;
  {$ENDIF}
  SwitchDefaultPerformance;
end;

destructor TCommunicationFramework.Destroy;
begin
  DeleteRegistedCMD('InitP2PTunnel');
  DeleteRegistedCMD('CloseP2PTunnel');
  DisposeObject(FCommandList);
  DisposeObject(FPerClientHashList);
  DisposeObject(FPrintParams);
  DisposeObject(FProgressPost);
  DisposeObject([CmdRecvStatistics, CmdSendStatistics, CmdMaxExecuteConsumeStatistics]);
  inherited Destroy;
end;

procedure TCommunicationFramework.p2pVMTunnelOpenBefore(Sender: TPeerClient; p2pVMTunnel: TCommunicationFrameworkWithP2PVM);
begin
end;

procedure TCommunicationFramework.p2pVMTunnelOpen(Sender: TPeerClient; p2pVMTunnel: TCommunicationFrameworkWithP2PVM);
begin
end;

procedure TCommunicationFramework.p2pVMTunnelOpenAfter(Sender: TPeerClient; p2pVMTunnel: TCommunicationFrameworkWithP2PVM);
begin
end;

procedure TCommunicationFramework.p2pVMTunnelClose(Sender: TPeerClient; p2pVMTunnel: TCommunicationFrameworkWithP2PVM);
begin
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
  LockObject(FPerClientHashList); // atomic lock
  Inc(Statistics[TStatisticsType.stLock]);
end;

procedure TCommunicationFramework.UnLockClients;
begin
  UnLockObject(FPerClientHashList); // atomic lock
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
  IDPool      : TClientIDPool;
  pframeworkID: Cardinal;
  c           : TPeerClient;
begin
  if (FPerClientHashList.Count > 0) and (Assigned(OnProgress)) then
    begin
      GetClientIDPool(IDPool);
      for pframeworkID in IDPool do
        begin
          c := TPeerClient(FPerClientHashList[pframeworkID]);
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
  IDPool      : TClientIDPool;
  pframeworkID: Cardinal;
  c           : TPeerClient;
begin
  if (FPerClientHashList.Count > 0) and (Assigned(OnProgress)) then
    begin
      GetClientIDPool(IDPool);
      for pframeworkID in IDPool do
        begin
          c := TPeerClient(FPerClientHashList[pframeworkID]);
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
  IDPool      : TClientIDPool;
  pframeworkID: Cardinal;
  c           : TPeerClient;
begin
  if (FPerClientHashList.Count > 0) and (Assigned(OnProgress)) then
    begin
      GetClientIDPool(IDPool);
      for pframeworkID in IDPool do
        begin
          c := TPeerClient(FPerClientHashList[pframeworkID]);
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


procedure TCommunicationFramework.FastProgressPerClient(OnProgress: TPerClientListCall);
var
  i: Integer;
  p: PUInt32HashListObjectStruct;
begin
  if (FPerClientHashList.Count > 0) and (Assigned(OnProgress)) then
    begin
      i := 0;
      p := FPerClientHashList.FirstPtr;
      while i < FPerClientHashList.Count do
        begin
          try
              OnProgress(TPeerClient(p^.Data));
          except
          end;
          Inc(i);
          p := p^.next;
        end;
    end;
end;

procedure TCommunicationFramework.FastProgressPerClient(OnProgress: TPerClientListMethod);
var
  i: Integer;
  p: PUInt32HashListObjectStruct;
begin
  if (FPerClientHashList.Count > 0) and (Assigned(OnProgress)) then
    begin
      i := 0;
      p := FPerClientHashList.FirstPtr;
      while i < FPerClientHashList.Count do
        begin
          try
              OnProgress(TPeerClient(p^.Data));
          except
          end;
          Inc(i);
          p := p^.next;
        end;
    end;
end;

{$IFNDEF FPC}


procedure TCommunicationFramework.FastProgressPerClient(OnProgress: TPerClientListProc);
var
  i: Integer;
  p: PUInt32HashListObjectStruct;
begin
  if (FPerClientHashList.Count > 0) and (Assigned(OnProgress)) then
    begin
      i := 0;
      p := FPerClientHashList.FirstPtr;
      while i < FPerClientHashList.Count do
        begin
          try
              OnProgress(TPeerClient(p^.Data));
          except
          end;
          Inc(i);
          p := p^.next;
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

function TCommunicationFramework.RegisterCompleteBuffer(Cmd: SystemString): TCommandCompleteBufferMode;
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

  Result := TCommandCompleteBufferMode.Create;
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

function TCommunicationFramework.ExecuteCompleteBuffer(Sender: TPeerClient; Cmd: SystemString; InData: PByte; DataSize: NativeInt): Boolean;
var
  b: TCoreClassObject;
begin
  Result := False;
  if not CanExecuteCommand(Sender, Cmd) then
      Exit;
  b := FCommandList[Cmd];
  if b = nil then
    begin
      Sender.PrintCommand('no exists complete buffer cmd:%s', Cmd);
      Exit;
    end;
  if not b.InheritsFrom(TCommandCompleteBufferMode) then
    begin
      Sender.PrintCommand('Illegal interface in cmd:%s', Cmd);
      Exit;
    end;
  Result := TCommandCompleteBufferMode(b).Execute(Sender, InData, DataSize);
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

  DoClientConnectAfter(Sender);
end;

procedure TCommunicationFrameworkServer.Command_Wait(Sender: TPeerClient; InData: SystemString; var OutData: SystemString);
begin
  OutData := IntToHex(GetTimeTick, SizeOf(TTimeTickValue) * 2);
end;

procedure TCommunicationFrameworkServer.InternalClientCreate(const Sender: TPeerClient);
begin
  DoClientConnectBefore(Sender);
  inherited InternalClientCreate(Sender);
end;

procedure TCommunicationFrameworkServer.InternalClientDestroy(const Sender: TPeerClient);
begin
  DoClientDisconnect(Sender);
  inherited InternalClientDestroy(Sender);
end;

constructor TCommunicationFrameworkServer.Create;
begin
  inherited Create(10 * 10000);
  {$IFDEF FPC}
  RegisterStream('ConnectedInit').OnExecute := @Command_ConnectedInit;
  RegisterConsole('Wait').OnExecute := @Command_Wait;
  {$ELSE}
  RegisterStream('ConnectedInit').OnExecute := Command_ConnectedInit;
  RegisterConsole('Wait').OnExecute := Command_Wait;
  {$ENDIF}
  PrintParams['Wait'] := False;

  FFrameworkIsServer := True;
  FFrameworkIsClient := False;
end;

constructor TCommunicationFrameworkServer.Create(HashPoolLen: Integer);
begin
  inherited Create(HashPoolLen);
  {$IFDEF FPC}
  RegisterStream('ConnectedInit').OnExecute := @Command_ConnectedInit;
  RegisterConsole('Wait').OnExecute := @Command_Wait;
  {$ELSE}
  RegisterStream('ConnectedInit').OnExecute := Command_ConnectedInit;
  RegisterConsole('Wait').OnExecute := Command_Wait;
  {$ENDIF}
  PrintParams['Wait'] := False;

  FFrameworkIsServer := True;
  FFrameworkIsClient := False;
end;

destructor TCommunicationFrameworkServer.Destroy;
begin
  DeleteRegistedCMD('ConnectedInit');
  DeleteRegistedCMD('Wait');
  inherited Destroy;
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

procedure TCommunicationFrameworkServer.DoClientConnectBefore(Sender: TPeerClient);
begin
end;

procedure TCommunicationFrameworkServer.DoClientConnectAfter(Sender: TPeerClient);
begin
end;

procedure TCommunicationFrameworkServer.DoClientDisconnect(Sender: TPeerClient);
begin
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

procedure TCommunicationFrameworkServer.SendStreamCmd(Client: TPeerClient; Cmd: SystemString; StreamData: TCoreClassStream; OnResult: TStreamMethod; DoneAutoFree: Boolean);
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
  p^.DoneAutoFree := DoneAutoFree;
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
  p^.DoneAutoFree := True;
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

procedure TCommunicationFrameworkServer.SendStreamCmd(Client: TPeerClient; Cmd: SystemString; StreamData: TCoreClassStream; OnResult: TStreamProc; DoneAutoFree: Boolean);
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
  p^.DoneAutoFree := DoneAutoFree;
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
  p^.DoneAutoFree := True;
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

procedure TCommunicationFrameworkServer.SendDirectStreamCmd(Client: TPeerClient; Cmd: SystemString; StreamData: TCoreClassStream; DoneAutoFree: Boolean);
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
  p^.DoneAutoFree := DoneAutoFree;
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
  p^.DoneAutoFree := True;
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

procedure TCommunicationFrameworkServer.SendBigStream(Client: TPeerClient; Cmd: SystemString; BigStream: TCoreClassStream; DoneAutoFree: Boolean);
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
  p^.DoneAutoFree := DoneAutoFree;
  TriggerQueueData(p);
  Client.PrintCommand('Send BigStream cmd: %s', Cmd);
end;

procedure TCommunicationFrameworkServer.SendCompleteBuffer(Client: TPeerClient; Cmd: SystemString; buff: PByte; buffSize: NativeInt; DoneAutoFree: Boolean);
var
  p: PQueueData;
begin
  // init queue data
  if (Client = nil) or (not Client.Connected) then
      Exit;
  if not CanSendCommand(Client, Cmd) then
      Exit;
  p := NewQueueData;
  p^.State := TQueueState.qsSendCompleteBuffer;
  p^.Client := Client;
  p^.Cmd := Cmd;
  p^.Cipher := Client.FSendDataCipherStyle;
  p^.Buffer := buff;
  p^.BufferSize := buffSize;
  p^.DoneAutoFree := DoneAutoFree;
  TriggerQueueData(p);
  Client.PrintCommand('Send complete buffer cmd: %s', Cmd);
end;

procedure TCommunicationFrameworkServer.SendConsoleCmd(ClientID: Cardinal; Cmd, ConsoleData: SystemString; OnResult: TConsoleMethod);
begin
  SendConsoleCmd(ClientFromID[ClientID], Cmd, ConsoleData, OnResult);
end;

procedure TCommunicationFrameworkServer.SendStreamCmd(ClientID: Cardinal; Cmd: SystemString; StreamData: TCoreClassStream; OnResult: TStreamMethod;
DoneAutoFree: Boolean);
begin
  SendStreamCmd(ClientFromID[ClientID], Cmd, StreamData, OnResult, DoneAutoFree);
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
DoneAutoFree: Boolean);
begin
  SendStreamCmd(ClientFromID[ClientID], Cmd, StreamData, OnResult, DoneAutoFree);
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

procedure TCommunicationFrameworkServer.SendDirectStreamCmd(ClientID: Cardinal; Cmd: SystemString; StreamData: TCoreClassStream; DoneAutoFree: Boolean);
begin
  SendDirectStreamCmd(ClientFromID[ClientID], Cmd, StreamData, DoneAutoFree);
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

procedure TCommunicationFrameworkServer.SendBigStream(ClientID: Cardinal; Cmd: SystemString; BigStream: TCoreClassStream; DoneAutoFree: Boolean);
begin
  SendBigStream(ClientFromID[ClientID], Cmd, BigStream, DoneAutoFree);
end;

procedure TCommunicationFrameworkServer.SendCompleteBuffer(ClientID: Cardinal; Cmd: SystemString; buff: PByte; buffSize: NativeInt; DoneAutoFree: Boolean);
begin
  SendCompleteBuffer(ClientFromID[ClientID], Cmd, buff, buffSize, DoneAutoFree);
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
  FConnectInitWaitingTimeout := GetTimeTick + FAsyncConnectTimeout;

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
  inherited Create(1);
  FNotyifyInterface := nil;
  FConnectInitWaiting := False;
  FConnectInitWaitingTimeout := 0;

  FWaiting := False;
  FWaitingTimeOut := 0;
  FAsyncConnectTimeout := 2000;
  FOnWaitResultCall := nil;
  FOnWaitResultMethod := nil;
  {$IFNDEF FPC}
  FOnWaitResultProc := nil;
  {$ENDIF}
  FFrameworkIsServer := False;
  FFrameworkIsClient := True;
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

procedure TCommunicationFrameworkClient.SendStreamCmd(Cmd: SystemString; StreamData: TCoreClassStream; OnResult: TStreamMethod; DoneAutoFree: Boolean);
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
  p^.DoneAutoFree := DoneAutoFree;
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
  p^.DoneAutoFree := True;
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

procedure TCommunicationFrameworkClient.SendStreamCmd(Cmd: SystemString; StreamData: TCoreClassStream; OnResult: TStreamProc; DoneAutoFree: Boolean);
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
  p^.DoneAutoFree := DoneAutoFree;
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
  p^.DoneAutoFree := True;
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

procedure TCommunicationFrameworkClient.SendDirectStreamCmd(Cmd: SystemString; StreamData: TCoreClassStream; DoneAutoFree: Boolean);
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
  p^.DoneAutoFree := DoneAutoFree;
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
  p^.DoneAutoFree := True;
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

procedure TCommunicationFrameworkClient.SendBigStream(Cmd: SystemString; BigStream: TCoreClassStream; DoneAutoFree: Boolean);
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
  p^.DoneAutoFree := DoneAutoFree;
  TriggerQueueData(p);
  ClientIO.PrintCommand('Send BigStream cmd: %s', Cmd);
end;

procedure TCommunicationFrameworkClient.SendCompleteBuffer(Cmd: SystemString; buff: PByte; buffSize: NativeInt; DoneAutoFree: Boolean);
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
  p^.State := TQueueState.qsSendCompleteBuffer;

  p^.Cmd := Cmd;
  p^.Cipher := ClientIO.FSendDataCipherStyle;
  p^.Buffer := buff;
  p^.BufferSize := buffSize;
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
  if stream.Size < 13 then
    begin
      Init;
      Exit;
    end;
  if stream.Size < PCardinal(stream.PositionAsPtr(0))^ + 13 then
    begin
      Init;
      Exit;
    end;
  buffSiz := PCardinal(stream.PositionAsPtr(0))^;
  frameworkID := PCardinal(stream.PositionAsPtr(4))^;
  p2pID := PCardinal(stream.PositionAsPtr(8))^;
  pkType := PByte(stream.PositionAsPtr(12))^;
  buff := stream.PositionAsPtr(13);
  Result := buffSiz + 13;
end;

procedure Tp2pVMFragmentPackage.BuildSendBuff(stream: TMemoryStream64);
begin
  stream.WritePtr(@buffSiz, 4);
  stream.WritePtr(@frameworkID, 4);
  stream.WritePtr(@p2pID, 4);
  stream.WritePtr(@pkType, 1);
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
      Exit;
  FRealSendBuff.Position := FRealSendBuff.Size;
  FRealSendBuff.WritePtr(buff, Size);
end;

procedure TPeerClientWithP2PVM.WriteBufferOpen;
begin
  FRealSendBuff.Clear;
end;

procedure TPeerClientWithP2PVM.WriteBufferFlush;
var
  p  : PByte;
  Siz: Integer;
begin
  if FRealSendBuff.Size <= 0 then
      Exit;

  if FLinkVM <> nil then
    begin
      p := FRealSendBuff.Memory;
      Siz := FRealSendBuff.Size;

      // fill fragment
      while Siz > FLinkVM.FMaxVMFragmentSize do
        begin
          FSendQueue.Add(BuildP2PVMPackage(FLinkVM.FMaxVMFragmentSize, FRemote_frameworkID, FRemote_p2pID, FLinkVM.c_p2pVM_LogicFragmentData, p));
          Inc(p, FLinkVM.FMaxVMFragmentSize);
          dec(Siz, FLinkVM.FMaxVMFragmentSize);
        end;

      if Siz > 0 then
          FSendQueue.Add(BuildP2PVMPackage(Siz, FRemote_frameworkID, FRemote_p2pID, FLinkVM.c_p2pVM_LogicFragmentData, p));
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
  Result := FSendQueue.Count > 0;
end;

procedure TCommunicationFrameworkWithP2PVM_Server.Connecting(SenderVM: TCommunicationFrameworkWithP2PVM;
const Remote_frameworkID, frameworkID: Cardinal; const ipv6: TIPV6; const Port: Word; var Allowed: Boolean);
var
  p       : Pp2pVMListen;
  LocalVMc: TPeerClientWithP2PVM;
begin
  if FLinkVMPool.Count = 0 then
    begin
      Allowed := False;
      Exit;
    end;

  p := SenderVM.FindListen(ipv6, Port);
  Allowed := (p <> nil) and (p^.frameworkID = frameworkID);

  if Allowed then
    begin
      // build client
      LocalVMc := TPeerClientWithP2PVM.Create(Self, nil);
      LocalVMc.FLinkVM := SenderVM;
      LocalVMc.FRemote_frameworkID := Remote_frameworkID;
      LocalVMc.FRemote_p2pID := 0;

      // connected reponse
      SenderVM.ConnectedReponse(LocalVMc.FRemote_frameworkID, LocalVMc.FRemote_p2pID, frameworkID, LocalVMc.ID);

      if not FQuietMode then
          DoStatus('connecting with %s port:%d', [IPv6ToStr(ipv6).Text, Port]);
    end;
end;

procedure TCommunicationFrameworkWithP2PVM_Server.ListenState(SenderVM: TCommunicationFrameworkWithP2PVM; const ipv6: TIPV6; const Port: Word; const State: Boolean);
begin
  if not FQuietMode then
    begin
      if State then
          DoStatus('Addr: "%s Port:%d" Listen is open', [IPv6ToStr(ipv6).Text, Port])
      else
          DoStatus('Addr: "%s Port:%d" Listen close!', [IPv6ToStr(ipv6).Text, Port]);
    end;
end;

procedure TCommunicationFrameworkWithP2PVM_Server.ProgressDisconnectClient(PeerClient: TPeerClient);
begin
  TPeerClientWithP2PVM(PeerClient).FLinkVM.DisconnectWithVM(PeerClient);
  DisposeObject(PeerClient);
end;

function TCommunicationFrameworkWithP2PVM_Server.ListenCount: Integer;
begin
  Result := FFrameworkListenPool.Count;
end;

function TCommunicationFrameworkWithP2PVM_Server.GetListen(const Index: Integer): Pp2pVMListen;
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
          Exit;
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
          Exit;
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
          Inc(i);
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
  inherited Create(10 * 10000);
  FFrameworkListenPool := TCoreClassList.Create;
  FLinkVMPool := TUInt32HashObjectList.Create;
  FFrameworkWithVM_ID := 0;
  StopService;
end;

constructor TCommunicationFrameworkWithP2PVM_Server.Create(HashPoolLen: Integer; frameworkID: Cardinal);
begin
  inherited Create(HashPoolLen);
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
          Inc(i);
          p := p^.next;
        end;
    end;

  DisposeObject(FLinkVMPool);
  DisposeObject(FFrameworkListenPool);
  inherited Destroy;
end;

procedure TCommunicationFrameworkWithP2PVM_Server.ProgressBackground;
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
              TPeerClient(p^.Data).ProcessAllSendCmd(nil, False, False);
          except
          end;
          Inc(i);
          p := p^.next;
        end;
    end;

  inherited ProgressBackground;
end;

procedure TCommunicationFrameworkWithP2PVM_Server.TriggerQueueData(v: PQueueData);
begin
  v^.Client.PostQueueData(v);
  v^.Client.ProcessAllSendCmd(nil, False, False);
end;

procedure TCommunicationFrameworkWithP2PVM_Server.CloseAllClient;
begin
  {$IFDEF FPC}
  ProgressPerClient(@ProgressDisconnectClient);
  {$ELSE}
  ProgressPerClient(ProgressDisconnectClient);
  {$ENDIF}
end;

procedure TCommunicationFrameworkWithP2PVM_Server.ProgressStopServiceWithPerVM(SenderVM: TCommunicationFrameworkWithP2PVM);
var
  i  : Integer;
  p  : Pp2pVMListen;
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
          Inc(i);
          p := p^.next;
        end;
    end;

  ClearListen;

  CloseAllClient;
end;

function TCommunicationFrameworkWithP2PVM_Server.StartService(Host: SystemString; Port: Word): Boolean;
var
  ipv6: TIPV6;
  si  : Cardinal;
  i   : Integer;
  p   : PUInt32HashListObjectStruct;
  lp  : Pp2pVMListen;
begin
  Result := False;

  ipv6 := StrToIPv6(Host, Result, si);

  if not Result then
      Exit;

  lp := FindListen(ipv6, Port);
  if lp = nil then
    begin
      New(lp);
      lp^.frameworkID := FFrameworkWithVM_ID;
      lp^.ListenHost := ipv6;
      lp^.ListenPort := Port;
      lp^.Listening := True;
      FFrameworkListenPool.Add(lp);
    end
  else
      lp^.Listening := True;

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
          Inc(i);
          p := p^.next;
        end;
    end
  else
    begin
      ListenState(nil, ipv6, Port, True);
    end;
  Result := True;
end;

function TCommunicationFrameworkWithP2PVM_Server.WaitSendConsoleCmd(Client: TPeerClient; Cmd, ConsoleData: SystemString; TimeOut: TTimeTickValue): SystemString;
begin
  Result := '';
  raiseInfo('WaitSend no Suppport VM server');
end;

procedure TCommunicationFrameworkWithP2PVM_Server.WaitSendStreamCmd(Client: TPeerClient; Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; TimeOut: TTimeTickValue);
begin
  raiseInfo('WaitSend no Suppport VM server');
end;

procedure TCommunicationFrameworkWithP2PVM_Client.InternalClientCreate(const Sender: TPeerClient);
begin
  inherited InternalClientCreate(Sender);
end;

procedure TCommunicationFrameworkWithP2PVM_Client.InternalClientDestroy(const Sender: TPeerClient);
begin
  FVMConnected := False;
  inherited InternalClientDestroy(Sender);
  FVMClient := nil;
  if FDestroying then
      Exit;

  if FLinkVM = nil then
      raiseInfo('no vm reference');

  FVMClient := TPeerClientWithP2PVM.Create(Self, nil);
  FVMClient.FLinkVM := FLinkVM;
end;

procedure TCommunicationFrameworkWithP2PVM_Client.VMConnectSuccessed(SenderVM: TCommunicationFrameworkWithP2PVM;
Remote_frameworkID, Remote_p2pID, frameworkID: Cardinal);
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
  inherited Create;
  FLinkVM := nil;
  FFrameworkWithVM_ID := 0;
  FVMClient := nil;
  FVMConnected := False;
  FDestroying := False;

  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := nil;
  {$IFNDEF FPC}
  FOnAsyncConnectNotifyProc := nil;
  {$ENDIF}
end;

constructor TCommunicationFrameworkWithP2PVM_Client.Create(frameworkID: Cardinal);
begin
  inherited Create;
  FLinkVM := nil;
  FFrameworkWithVM_ID := frameworkID;
  FVMClient := nil;
  FVMConnected := False;
  FDestroying := False;

  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := nil;
  {$IFNDEF FPC}
  FOnAsyncConnectNotifyProc := nil;
  {$ENDIF}
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
    {$ENDIF}
  except
  end;

  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := nil;
  {$IFNDEF FPC}
  FOnAsyncConnectNotifyProc := nil;
  {$ENDIF}
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
    {$ENDIF}
  except
  end;

  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := nil;
  {$IFNDEF FPC}
  FOnAsyncConnectNotifyProc := nil;
  {$ENDIF}
end;

function TCommunicationFrameworkWithP2PVM_Client.Connected: Boolean;
begin
  Result := (FVMConnected) and (FVMClient <> nil);
end;

function TCommunicationFrameworkWithP2PVM_Client.ClientIO: TPeerClient;
begin
  Result := FVMClient;
end;

procedure TCommunicationFrameworkWithP2PVM_Client.ProgressBackground;
begin
  if FVMClient <> nil then
    begin
      FVMClient.ProcessAllSendCmd(nil, False, False);
    end;

  inherited ProgressBackground;
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

procedure TCommunicationFrameworkWithP2PVM_Client.AsyncConnect(Addr: SystemString; Port: Word);
var
  r   : Boolean;
  ipv6: TIPV6;
  p   : Pp2pVMListen;
begin
  Disconnect;

  FVMConnected := False;

  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := nil;
  {$IFNDEF FPC}
  FOnAsyncConnectNotifyProc := nil;
  {$ENDIF}
  if (FLinkVM = nil) or (FLinkVM.FPhysicsTunnel = nil) then
    begin
      if not FQuietMode then
          DoStatus('no VM connect');
      TriggerDoConnectFailed;
      Exit;
    end;

  if not FLinkVM.WasAuthed then
    begin
      if not FQuietMode then
          DoStatus('VM no auth');
      TriggerDoConnectFailed;
      Exit;
    end;

  ipv6 := StrToIPv6(Addr, r);

  if not r then
    begin
      if not FQuietMode then
          DoStatus('ipv6 format error! %s', [Addr]);
      TriggerDoConnectFailed;
      Exit;
    end;

  p := FLinkVM.FindListen(ipv6, Port);
  if p = nil then
    begin
      if not FQuietMode then
          DoStatus('no remote listen %s port:%d', [IPv6ToStr(ipv6).Text, Port]);
      TriggerDoConnectFailed;
      Exit;
    end;

  FVMClient.FIP := ipv6;
  FVMClient.FPort := Port;

  FLinkVM.Connecting(p^.frameworkID, FFrameworkWithVM_ID, FVMClient.ID, ipv6, Port);
end;

procedure TCommunicationFrameworkWithP2PVM_Client.AsyncConnect(Addr: SystemString; Port: Word; OnResult: TStateCall);
var
  r   : Boolean;
  ipv6: TIPV6;
  p   : Pp2pVMListen;
begin
  Disconnect;

  FVMConnected := False;

  FOnAsyncConnectNotifyCall := OnResult;
  FOnAsyncConnectNotifyMethod := nil;
  {$IFNDEF FPC}
  FOnAsyncConnectNotifyProc := nil;
  {$ENDIF}
  if (FLinkVM = nil) or (FLinkVM.FPhysicsTunnel = nil) then
    begin
      if not FQuietMode then
          DoStatus('no VM connect');
      TriggerDoConnectFailed;
      Exit;
    end;

  if not FLinkVM.WasAuthed then
    begin
      if not FQuietMode then
          DoStatus('VM no auth');
      TriggerDoConnectFailed;
      Exit;
    end;

  ipv6 := StrToIPv6(Addr, r);

  if not r then
    begin
      if not FQuietMode then
          DoStatus('ipv6 format error! %s', [Addr]);
      TriggerDoConnectFailed;
      Exit;
    end;

  p := FLinkVM.FindListen(ipv6, Port);
  if p = nil then
    begin
      if not FQuietMode then
          DoStatus('no remote listen %s port:%d', [IPv6ToStr(ipv6).Text, Port]);
      TriggerDoConnectFailed;
      Exit;
    end;

  FVMClient.FIP := ipv6;
  FVMClient.FPort := Port;

  FLinkVM.Connecting(p^.frameworkID, FFrameworkWithVM_ID, FVMClient.ID, ipv6, Port);
end;

procedure TCommunicationFrameworkWithP2PVM_Client.AsyncConnect(Addr: SystemString; Port: Word; OnResult: TStateMethod);
var
  r   : Boolean;
  ipv6: TIPV6;
  p   : Pp2pVMListen;
begin
  Disconnect;

  FVMConnected := False;

  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := OnResult;
  {$IFNDEF FPC}
  FOnAsyncConnectNotifyProc := nil;
  {$ENDIF}
  if (FLinkVM = nil) or (FLinkVM.FPhysicsTunnel = nil) then
    begin
      if not FQuietMode then
          DoStatus('no VM connect');
      TriggerDoConnectFailed;
      Exit;
    end;

  if not FLinkVM.WasAuthed then
    begin
      if not FQuietMode then
          DoStatus('VM no auth');
      TriggerDoConnectFailed;
      Exit;
    end;

  ipv6 := StrToIPv6(Addr, r);

  if not r then
    begin
      if not FQuietMode then
          DoStatus('ipv6 format error! %s', [Addr]);
      TriggerDoConnectFailed;
      Exit;
    end;

  p := FLinkVM.FindListen(ipv6, Port);
  if p = nil then
    begin
      if not FQuietMode then
          DoStatus('no remote listen %s port:%d', [IPv6ToStr(ipv6).Text, Port]);
      TriggerDoConnectFailed;
      Exit;
    end;

  FVMClient.FIP := ipv6;
  FVMClient.FPort := Port;

  FLinkVM.Connecting(p^.frameworkID, FFrameworkWithVM_ID, FVMClient.ID, ipv6, Port);
end;

{$IFNDEF FPC}


procedure TCommunicationFrameworkWithP2PVM_Client.AsyncConnect(Addr: SystemString; Port: Word; OnResult: TStateProc);
var
  r   : Boolean;
  ipv6: TIPV6;
  p   : Pp2pVMListen;
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
      Exit;
    end;

  if not FLinkVM.WasAuthed then
    begin
      if not FQuietMode then
          DoStatus('VM no auth');
      TriggerDoConnectFailed;
      Exit;
    end;

  ipv6 := StrToIPv6(Addr, r);

  if not r then
    begin
      if not FQuietMode then
          DoStatus('ipv6 format error! %s', [Addr]);
      TriggerDoConnectFailed;
      Exit;
    end;

  p := FLinkVM.FindListen(ipv6, Port);
  if p = nil then
    begin
      if not FQuietMode then
          DoStatus('no remote listen %s port:%d', [IPv6ToStr(ipv6).Text, Port]);
      TriggerDoConnectFailed;
      Exit;
    end;

  FVMClient.FIP := ipv6;
  FVMClient.FPort := Port;

  FLinkVM.Connecting(p^.frameworkID, FFrameworkWithVM_ID, FVMClient.ID, ipv6, Port);
end;
{$ENDIF}


function TCommunicationFrameworkWithP2PVM_Client.Connect(Addr: string; Port: Word): Boolean;
var
  ipv6: TIPV6;
  p   : Pp2pVMListen;
  t   : TTimeTickValue;
begin
  Disconnect;

  Result := False;

  FVMConnected := False;
  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := nil;
  {$IFNDEF FPC}
  FOnAsyncConnectNotifyProc := nil;
  {$ENDIF}
  if (FLinkVM = nil) or (FLinkVM.FPhysicsTunnel = nil) then
    begin
      if not FQuietMode then
          DoStatus('no VM connect');
      Exit;
    end;

  if not FLinkVM.WasAuthed then
    begin
      if not FQuietMode then
          DoStatus('VM no auth');
      Exit;
    end;

  ipv6 := StrToIPv6(Addr, Result);

  if not Result then
    begin
      if not FQuietMode then
          DoStatus('ipv6 format error! %s', [Addr]);
      Exit;
    end;

  p := FLinkVM.FindListen(ipv6, Port);
  if p = nil then
    begin
      if not FQuietMode then
          DoStatus('no remote listen %s port:%d', [IPv6ToStr(ipv6).Text, Port]);
      Exit;
    end;

  FVMClient.FIP := ipv6;
  FVMClient.FPort := Port;
  FLinkVM.Connecting(p^.frameworkID, FFrameworkWithVM_ID, FVMClient.ID, ipv6, Port);

  t := GetTimeTick + 1000;
  while not FVMConnected do
    begin
      ProgressWaitSendOfClient(FVMClient);
      if GetTimeTick > t then
          break;
    end;

  t := GetTimeTick + 2000;
  while (FVMConnected) and (not RemoteInited) do
    begin
      ProgressWaitSendOfClient(FVMClient);
      if GetTimeTick > t then
          break;
    end;

  Result := (FVMConnected) and (RemoteInited);
end;

procedure TCommunicationFrameworkWithP2PVM_Client.Disconnect;
begin
  if Connected then
      FVMClient.Disconnect;
end;

procedure TCommunicationFrameworkWithP2PVM_Client.ProgressWaitSendOfClient(Client: TPeerClient);
begin
  if FLinkVM <> nil then
    begin
      if FLinkVM.FPhysicsTunnel <> nil then
          FLinkVM.FPhysicsTunnel.OwnerFramework.ProgressBackground;

      FLinkVM.Progress;
    end;

  inherited ProgressWaitSendOfClient(Client);
end;

procedure TCommunicationFrameworkWithP2PVM.Hook_SendByteBuffer(const Sender: TPeerClient; const buff: PByte; Siz: NativeInt);
var
  t: Tp2pVMFragmentPackage;
begin
  if Siz <= 0 then
      Exit;

  if FAuthed then
    begin
      t.Init;
      t.buffSiz := Siz;
      t.frameworkID := 0;
      t.p2pID := 0;
      t.pkType := c_p2pVM_PhysicsFragmentData;
      t.buff := buff;

      t.BuildSendBuff(FSendStream);
    end
  else
      FSendStream.WritePtr(buff, Siz);
end;

procedure TCommunicationFrameworkWithP2PVM.Hook_SaveReceiveBuffer(const Sender: TPeerClient; const buff: Pointer; Siz: Int64);
begin
  if Siz <= 0 then
      Exit;

  LockObject(Self);
  try
    FReceiveStream.Position := FReceiveStream.Size;
    FReceiveStream.WritePtr(buff, Siz);
  finally
      UnLockObject(Self);
  end;
end;

procedure TCommunicationFrameworkWithP2PVM.SyncProcessReceiveBuff;
var
  i         : Integer;
  lp        : Pp2pVMListen;
  p64       : Int64;
  sourStream: TMemoryStream64;
  fPk       : Tp2pVMFragmentPackage;
  rPos      : Integer;
begin
  // safe auth support
  if not FAuthed then
    begin
      if (FAuthWaiting) and (FReceiveStream.Size >= Length(P2PVMAuthToken)) and (CompareMemory(@P2PVMAuthToken[0], FReceiveStream.Memory, Length(P2PVMAuthToken))) then
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
              lp := FFrameworkListenPool[i];
              ListenState(lp^.frameworkID, lp^.ListenHost, lp^.ListenPort, lp^.Listening);
            end;

          // send auth successed token
          AuthSuccessed;

          // fill fragment buffer
          p64 := Length(P2PVMAuthToken);
          sourStream := TMemoryStream64.Create;
          FReceiveStream.Position := p64;
          if FReceiveStream.Size - FReceiveStream.Position > 0 then
              sourStream.CopyFrom(FReceiveStream, FReceiveStream.Size - FReceiveStream.Position);
          DisposeObject(FReceiveStream);
          FReceiveStream := sourStream;

          if not FQuietMode then
              DoStatus('VM connect Auth Success');
        end
      else if FAuthWaiting then
          Exit
      else
        begin
          // protocol safe support
          FPhysicsTunnel.FOwnerFramework.InternalSaveReceiveBuffer(FPhysicsTunnel, FReceiveStream.Memory, FReceiveStream.Size);
          FReceiveStream.Clear;
          FPhysicsTunnel.FOwnerFramework.InternalProcessReceiveBuffer(FPhysicsTunnel, nil, False, False);
          Exit;
        end;
    end;

  if FReceiveStream.Size < 13 then
      Exit;

  LockObject(Self);

  try
    sourStream := TMemoryStream64.Create;
    p64 := 0;
    sourStream.SetPointerWithProtectedMode(FReceiveStream.PositionAsPtr(p64), FReceiveStream.Size - p64);

    while sourStream.Size > 0 do
      begin
        fPk.Init;
        rPos := fPk.FillReceiveBuff(sourStream);
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
            Inc(p64, rPos);
            if FReceiveStream.Size - p64 >= 13 then
              begin
                sourStream.SetPointerWithProtectedMode(FReceiveStream.PositionAsPtr(p64), FReceiveStream.Size - p64);
              end
            else
                break;
          end
        else
            break;
      end;

    DisposeObject(sourStream);

    if p64 > 0 then
      begin
        sourStream := TMemoryStream64.Create;
        FReceiveStream.Position := p64;
        if FReceiveStream.Size - FReceiveStream.Position > 0 then
            sourStream.CopyFrom(FReceiveStream, FReceiveStream.Size - FReceiveStream.Position);
        DisposeObject(FReceiveStream);
        FReceiveStream := sourStream;
      end;
  finally
      UnLockObject(Self);
  end;
end;

procedure TCommunicationFrameworkWithP2PVM.Hook_ProcessReceiveBuffer(const Sender: TPeerClient; const ACurrentActiveThread: TCoreClassThread; const RecvSync, SendSync: Boolean);
begin
  {$IFDEF FPC}
  SyncMethod(ACurrentActiveThread, RecvSync, @SyncProcessReceiveBuff);
  {$ELSE}
  SyncMethod(ACurrentActiveThread, RecvSync, SyncProcessReceiveBuff);
  {$ENDIF}
end;

procedure TCommunicationFrameworkWithP2PVM.Hook_ClientDestroy(const Sender: TPeerClient);
begin
  CloseP2PVMTunnel;
  Sender.FOwnerFramework.InternalClientDestroy(Sender);
end;

procedure TCommunicationFrameworkWithP2PVM.SendVMBuffer(const buff: Pointer; const Siz: NativeInt);
begin
  FPhysicsTunnel.WriteBufferOpen;
  FPhysicsTunnel.OwnerFramework.InternalSendByteBuffer(FPhysicsTunnel, buff, Siz);
  FPhysicsTunnel.WriteBufferFlush;
  FPhysicsTunnel.WriteBufferClose;
end;

procedure TCommunicationFrameworkWithP2PVM.DisconnectWithVM(c: TPeerClient);
begin
end;

procedure TCommunicationFrameworkWithP2PVM.ReceivedEchoing(const frameworkID, p2pID: Cardinal; const buff: PByte; const Siz: Cardinal);
begin
  echoBuffer(buff, Siz);
end;

procedure TCommunicationFrameworkWithP2PVM.ReceivedEcho(const frameworkID, p2pID: Cardinal; const buff: PByte; const Siz: Cardinal);
type
  TBuf = array [0 .. 7] of Byte;
  PBuf = ^TBuf;
var
  p      : PBuf;
  u64ptr : UInt64;
  echoPtr: POnEcho;
  i      : Integer;
begin
  if Siz <> SizeOf(TBuf) then
    begin
      if not FQuietMode then
          DoStatus('echoing protocol with buffer error!');
      if buff <> nil then
        if not FQuietMode then
            DoStatus(buff, Siz, 40);
      Exit;
    end;
  p := @buff^;
  u64ptr := PUInt64(@p^[0])^;
  echoPtr := Pointer(u64ptr);
  if echoPtr = nil then
      Exit;

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
            {$ENDIF}
          except
          end;

          try
              Dispose(echoPtr);
          except
          end;
        end
      else
          Inc(i);
    end;
end;

procedure TCommunicationFrameworkWithP2PVM.ReceivedListen(const frameworkID, p2pID: Cardinal; const buff: PByte; const Siz: Cardinal);
type
  TBuf = array [0 .. 18] of Byte;
  PBuf = ^TBuf;
var
  p        : PBuf;
  ipv6     : TIPV6;
  Port     : Word;
  Listening: Boolean;
  lp       : Pp2pVMListen;
begin
  if Siz <> SizeOf(TBuf) then
    begin
      if not FQuietMode then
          DoStatus('listen protocol with buffer error!');
      if buff <> nil then
        if not FQuietMode then
            DoStatus(buff, Siz, 40);
      Exit;
    end;
  p := @buff^;
  ipv6 := PIPV6(@p^[0])^;
  Port := PWord(@p^[16])^;
  Listening := PBoolean(@p^[18])^;

  if p2pID <> 0 then
    begin
      if not FQuietMode then
          DoStatus('listen protocol error! PeerClient ID:%d', [p2pID]);
      Exit;
    end;

  lp := FindListen(ipv6, Port);
  if Listening then
    begin
      if lp = nil then
        begin
          New(lp);
          lp^.frameworkID := frameworkID;
          lp^.ListenHost := ipv6;
          lp^.ListenPort := Port;
          lp^.Listening := True;
          FFrameworkListenPool.Add(lp);
          ListenState(frameworkID, ipv6, Port, True);
        end
      else
        begin
          lp^.Listening := True;
          ListenState(frameworkID, ipv6, Port, True);
        end;
    end
  else
    begin
      DeleteListen(ipv6, Port);
      ListenState(frameworkID, ipv6, Port, False);
    end;
end;

procedure TCommunicationFrameworkWithP2PVM.ReceivedListenState(const frameworkID, p2pID: Cardinal; const buff: PByte; const Siz: Cardinal);
type
  TBuf = array [0 .. 18] of Byte;
  PBuf = ^TBuf;
var
  c        : TCommunicationFramework;
  p        : PBuf;
  ipv6     : TIPV6;
  Port     : Word;
  Listening: Boolean;
  lp       : Pp2pVMListen;
begin
  if Siz <> SizeOf(TBuf) then
    begin
      if not FQuietMode then
          DoStatus('listen state protocol with buffer error!');
      if buff <> nil then
        if not FQuietMode then
            DoStatus(buff, Siz, 40);
      Exit;
    end;
  p := @buff^;
  ipv6 := PIPV6(@p^[0])^;
  Port := PWord(@p^[16])^;
  Listening := PBoolean(@p^[18])^;

  if p2pID <> 0 then
    begin
      if not FQuietMode then
          DoStatus('listen state protocol error! PeerClient ID:%d', [p2pID]);
      Exit;
    end;

  lp := FindListen(ipv6, Port);
  if Listening then
    begin
      if lp = nil then
        begin
          New(lp);
          lp^.frameworkID := frameworkID;
          lp^.ListenHost := ipv6;
          lp^.ListenPort := Port;
          lp^.Listening := True;
          FFrameworkListenPool.Add(lp);
        end
      else
        begin
          lp^.Listening := True;
        end;
      if not FQuietMode then
          DoStatus('Remote Listen state Activted "%s port:%d"', [IPv6ToStr(ipv6).Text, Port]);
    end
  else
    begin
      DeleteListen(ipv6, Port);
      if not FQuietMode then
          DoStatus('Remote Listen state Close "%s port:%d"', [IPv6ToStr(ipv6).Text, Port]);
    end;

  c := TCommunicationFramework(FFrameworkPool[frameworkID]);
  if c is TCommunicationFrameworkWithP2PVM_Server then
    begin
      TCommunicationFrameworkWithP2PVM_Server(c).ListenState(Self, ipv6, Port, Listening);
      ListenState(TCommunicationFrameworkWithP2PVM_Server(c).FFrameworkWithVM_ID, ipv6, Port, Listening);
    end;
end;

procedure TCommunicationFrameworkWithP2PVM.ReceivedConnecting(const frameworkID, p2pID: Cardinal; const buff: PByte; const Siz: Cardinal);
type
  TBuf = array [0 .. 25] of Byte;
  PBuf = ^TBuf;
var
  c                 : TCommunicationFramework;
  p                 : PBuf;
  Remote_frameworkID: Cardinal;
  Remote_p2pID      : Cardinal;
  ipv6              : TIPV6;
  Port              : Word;
  LocalVMc          : TPeerClientWithP2PVM;
  Allowed           : Boolean;
begin
  if Siz <> SizeOf(TBuf) then
    begin
      if not FQuietMode then
        begin
          DoStatus('connect request with buffer error!');
          if buff <> nil then
              DoStatus(buff, Siz, 40);
        end;
      Exit;
    end;
  p := @buff^;
  Remote_frameworkID := PCardinal(@p^[0])^;
  Remote_p2pID := PCardinal(@p^[4])^;
  ipv6 := PIPV6(@p^[8])^;
  Port := PWord(@p^[24])^;

  if p2pID <> 0 then
    begin
      Disconnect(Remote_frameworkID, Remote_p2pID);
      if not FQuietMode then
          DoStatus('connect request with protocol error! PeerClient ID:%d', [p2pID]);
      Exit;
    end;

  c := TCommunicationFramework(FFrameworkPool[frameworkID]);
  if c is TCommunicationFrameworkWithP2PVM_Server then
    begin
      Allowed := True;
      TCommunicationFrameworkWithP2PVM_Server(c).Connecting(Self, Remote_frameworkID, frameworkID, ipv6, Port, Allowed);

      if not Allowed then
        begin
          Disconnect(Remote_frameworkID, 0);
          Exit;
        end;
    end
  else
    begin
      Disconnect(Remote_frameworkID, Remote_p2pID);
    end;
end;

procedure TCommunicationFrameworkWithP2PVM.ReceivedConnectedReponse(const frameworkID, p2pID: Cardinal; const buff: PByte; const Siz: Cardinal);
type
  TBuf = array [0 .. 7] of Byte;
  PBuf = ^TBuf;
var
  c                 : TCommunicationFramework;
  p                 : PBuf;
  LocalVMc          : TPeerClientWithP2PVM;
  Remote_frameworkID: Cardinal;
  Remote_p2pID      : Cardinal;
begin
  if Siz <> SizeOf(TBuf) then
    begin
      if not FQuietMode then
        begin
          DoStatus('connect request with buffer error!');
          if buff <> nil then
              DoStatus(buff, Siz, 40);
        end;
      Exit;
    end;

  c := TCommunicationFramework(FFrameworkPool[frameworkID]);
  if c is TCommunicationFrameworkWithP2PVM_Client then
    begin
      LocalVMc := TCommunicationFrameworkWithP2PVM_Client(c).FVMClient;

      p := @buff^;
      Remote_frameworkID := PCardinal(@p^[0])^;
      Remote_p2pID := PCardinal(@p^[4])^;

      // trigger connect reponse
      TCommunicationFrameworkWithP2PVM_Client(c).VMConnectSuccessed(Self, Remote_frameworkID, Remote_p2pID, frameworkID);

      if not FQuietMode then
          DoStatus('connect reponse from frameworkID[%d] p2pID[%d]', [Remote_frameworkID, Remote_p2pID]);
    end;
end;

procedure TCommunicationFrameworkWithP2PVM.ReceivedDisconnect(const frameworkID, p2pID: Cardinal; const buff: PByte; const Siz: Cardinal);
var
  c       : TCommunicationFramework;
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
      LocalVMc := TPeerClientWithP2PVM(c.FPerClientHashList[p2pID]);
      if LocalVMc = nil then
        begin
          if not FQuietMode then
              DoStatus('disconnect with protocol error! PeerClient ID:%d', [p2pID]);
          Exit;
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

procedure TCommunicationFrameworkWithP2PVM.ReceivedLogicFragmentData(const frameworkID, p2pID: Cardinal; const buff: PByte; const Siz: Cardinal);
var
  c       : TCommunicationFramework;
  LocalVMc: TPeerClient;
begin
  c := TCommunicationFramework(FFrameworkPool[frameworkID]);
  if c is TCommunicationFrameworkWithP2PVM_Server then
    begin
      LocalVMc := TPeerClient(c.FPerClientHashList[p2pID]);
      if LocalVMc <> nil then
        begin
          LocalVMc.SaveReceiveBuffer(buff, Siz);
          LocalVMc.FillRecvBuffer(nil, False, False);
        end
      else if not FQuietMode then
        begin
          DoStatus('fragment Data p2pID error: p2pID:%d buffer size:%d', [p2pID, Siz]);
          DoStatus(buff, umlMin(Siz, 164), 40);
        end;
    end
  else if c is TCommunicationFrameworkWithP2PVM_Client then
    begin
      LocalVMc := TCommunicationFrameworkWithP2PVM_Client(c).FVMClient;
      LocalVMc.SaveReceiveBuffer(buff, Siz);
      LocalVMc.FillRecvBuffer(nil, False, False);
    end
  else if not FQuietMode then
    begin
      DoStatus('fragment Data frameworkID error: frameworkID:%d buffer size:%d', [frameworkID, Siz]);
      DoStatus(buff, umlMin(Siz, 164), 40);
    end;
end;

procedure TCommunicationFrameworkWithP2PVM.ReceivedOriginFragmentData(const frameworkID, p2pID: Cardinal; const buff: PByte; const Siz: Cardinal);
begin
  if FPhysicsTunnel = nil then
      Exit;
  FPhysicsTunnel.FOwnerFramework.InternalSaveReceiveBuffer(FPhysicsTunnel, buff, Siz);
  FPhysicsTunnel.FOwnerFramework.InternalProcessReceiveBuffer(FPhysicsTunnel, nil, False, False);
end;

procedure TCommunicationFrameworkWithP2PVM.DoProcessPerClientFragmentSend(PeerClient: TPeerClient);
var
  p: Pp2pVMFragmentPackage;
begin
  if TPeerClientWithP2PVM(PeerClient).FLinkVM <> Self then
      Exit;

  if TPeerClientWithP2PVM(PeerClient).FSendQueue.Count > 0 then
    begin
      p := TPeerClientWithP2PVM(PeerClient).FSendQueue[0];
      TPeerClientWithP2PVM(PeerClient).FSendQueue.Delete(0);
      p^.BuildSendBuff(FSendStream);
      FreeP2PVMPackage(p);
    end;
end;

procedure TCommunicationFrameworkWithP2PVM.DoPerClientClose(PeerClient: TPeerClient);
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
  FMinVMProgressSize := 2048 * 1024; // 2M

  FQuietMode := False;

  FReceiveStream := TMemoryStream64.Create;
  FSendStream := TMemoryStream64.Create;

  FWaitEchoList := TCoreClassList.Create;

  FVMID := 0;
  OnAuthSuccessOnesNotify := nil;
end;

destructor TCommunicationFrameworkWithP2PVM.Destroy;
var
  i        : Integer;
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
  i        : Integer;
  p        : PUInt32HashListObjectStruct;
  lsiz     : Int64;
  OnEchoPtr: POnEcho;
begin
  if FPhysicsTunnel = nil then
      Exit;

  i := 0;
  while i < FWaitEchoList.Count do
    begin
      OnEchoPtr := FWaitEchoList[i];
      if OnEchoPtr^.TimeOut > GetTimeTick then
        begin
          FWaitEchoList.Delete(i);

          try
              Dispose(OnEchoPtr);
          except
          end;
        end
      else
          Inc(i);
    end;

  try
    if FSendStream.Size > 0 then
      begin
        SendVMBuffer(FSendStream.Memory, FSendStream.Size);
        FSendStream.Clear;
      end;
  except
  end;

  if not FAuthed then
      Exit;

  repeat
    lsiz := FSendStream.Size;
    if (FFrameworkPool.Count > 0) then
      begin
        i := 0;
        p := FFrameworkPool.FirstPtr;
        while i < FFrameworkPool.Count do
          begin
            {$IFDEF FPC}
            TCommunicationFramework(p^.Data).FastProgressPerClient(@DoProcessPerClientFragmentSend);
            {$ELSE}
            TCommunicationFramework(p^.Data).FastProgressPerClient(DoProcessPerClientFragmentSend);
            {$ENDIF}
            Inc(i);
            p := p^.next;
          end;
      end;

  until (FSendStream.Size = lsiz) or (FSendStream.Size > FMinVMProgressSize);

  if FSendStream.Size > 0 then
    begin
      SendVMBuffer(FSendStream.Memory, FSendStream.Size);
      FSendStream.Clear;
    end
end;

procedure TCommunicationFrameworkWithP2PVM.ProgressCommunicationFramework(OnProgress: TCommunicationFrameworkListCall);
var
  i: Integer;
  p: PUInt32HashListObjectStruct;
begin
  if (FFrameworkPool.Count > 0) and (Assigned(OnProgress)) then
    begin
      i := 0;
      p := FFrameworkPool.FirstPtr;
      while i < FFrameworkPool.Count do
        begin
          try
              OnProgress(TCommunicationFramework(p^.Data));
          except
          end;
          Inc(i);
          p := p^.next;
        end;
    end;
end;

procedure TCommunicationFrameworkWithP2PVM.ProgressCommunicationFramework(OnProgress: TCommunicationFrameworkListMethod);
var
  i: Integer;
  p: PUInt32HashListObjectStruct;
begin
  if (FFrameworkPool.Count > 0) and (Assigned(OnProgress)) then
    begin
      i := 0;
      p := FFrameworkPool.FirstPtr;
      while i < FFrameworkPool.Count do
        begin
          try
              OnProgress(TCommunicationFramework(p^.Data));
          except
          end;
          Inc(i);
          p := p^.next;
        end;
    end;
end;

{$IFNDEF FPC}


procedure TCommunicationFrameworkWithP2PVM.ProgressCommunicationFramework(OnProgress: TCommunicationFrameworkListProc);
var
  i: Integer;
  p: PUInt32HashListObjectStruct;
begin
  if (FFrameworkPool.Count > 0) and (Assigned(OnProgress)) then
    begin
      i := 0;
      p := FFrameworkPool.FirstPtr;
      while i < FFrameworkPool.Count do
        begin
          try
              OnProgress(TCommunicationFramework(p^.Data));
          except
          end;
          Inc(i);
          p := p^.next;
        end;
    end;
end;

{$ENDIF}


procedure TCommunicationFrameworkWithP2PVM.OpenP2PVMTunnel(c: TPeerClient);
begin
  FPhysicsTunnel := c;
  FAuthWaiting := False;
  FAuthed := False;
  FAuthSending := False;
  FReceiveStream.Clear;
  FSendStream.Clear;

  try
    {$IFDEF FPC}
    FPhysicsTunnel.OnInternalSendByteBuffer := @Hook_SendByteBuffer;
    FPhysicsTunnel.OnInternalSaveReceiveBuffer := @Hook_SaveReceiveBuffer;
    FPhysicsTunnel.OnInternalProcessReceiveBuffer := @Hook_ProcessReceiveBuffer;
    FPhysicsTunnel.OnDestroy := @Hook_ClientDestroy;
    {$ELSE}
    FPhysicsTunnel.OnInternalSendByteBuffer := Hook_SendByteBuffer;
    FPhysicsTunnel.OnInternalSaveReceiveBuffer := Hook_SaveReceiveBuffer;
    FPhysicsTunnel.OnInternalProcessReceiveBuffer := Hook_ProcessReceiveBuffer;
    FPhysicsTunnel.OnDestroy := Hook_ClientDestroy;
    {$ENDIF}
  except
  end;

  if not FQuietMode then
      DoStatus('Open VM P2P Tunnel ' + FPhysicsTunnel.PeerIP);
end;

procedure TCommunicationFrameworkWithP2PVM.CloseP2PVMTunnel;
var
  i        : Integer;
  OnEchoPtr: POnEcho;
  p        : PUInt32HashListObjectStruct;
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
              {$IFDEF FPC}
              TCommunicationFramework(p^.Data).ProgressPerClient(@DoPerClientClose);
              {$ELSE}
              TCommunicationFramework(p^.Data).ProgressPerClient(DoPerClientClose);
              {$ENDIF}
              TCommunicationFrameworkWithP2PVM_Server(p^.Data).FLinkVMPool.Delete(FVMID);
            end;
          Inc(i);
          p := p^.next;
        end;
    end;

  CloseAllClientIO;

  FAuthWaiting := False;
  FAuthed := False;
  FAuthSending := False;
  FReceiveStream.Clear;
  FSendStream.Clear;

  if FPhysicsTunnel = nil then
      Exit;

  try
    {$IFDEF FPC}
    FPhysicsTunnel.OnInternalSendByteBuffer := @FPhysicsTunnel.FOwnerFramework.InternalSendByteBuffer;
    FPhysicsTunnel.OnInternalSaveReceiveBuffer := @FPhysicsTunnel.FOwnerFramework.InternalSaveReceiveBuffer;
    FPhysicsTunnel.OnInternalProcessReceiveBuffer := @FPhysicsTunnel.FOwnerFramework.InternalProcessReceiveBuffer;
    FPhysicsTunnel.OnDestroy := @FPhysicsTunnel.FOwnerFramework.InternalClientDestroy;
    {$ELSE}
    FPhysicsTunnel.OnInternalSendByteBuffer := FPhysicsTunnel.FOwnerFramework.InternalSendByteBuffer;
    FPhysicsTunnel.OnInternalSaveReceiveBuffer := FPhysicsTunnel.FOwnerFramework.InternalSaveReceiveBuffer;
    FPhysicsTunnel.OnInternalProcessReceiveBuffer := FPhysicsTunnel.FOwnerFramework.InternalProcessReceiveBuffer;
    FPhysicsTunnel.OnDestroy := FPhysicsTunnel.FOwnerFramework.InternalClientDestroy;
    {$ENDIF}
  except
  end;

  if not FQuietMode then
      DoStatus('Close VM P2P Tunnel' + FPhysicsTunnel.PeerIP);

  FPhysicsTunnel := nil;
end;

procedure TCommunicationFrameworkWithP2PVM.InstallLogicFramework(c: TCommunicationFramework);
var
  i : Integer;
  lp: Pp2pVMListen;
begin
  if c is TCommunicationFrameworkWithP2PVM_Server then
    begin
      if TCommunicationFrameworkWithP2PVM_Server(c).FFrameworkWithVM_ID <> 0 then
        begin
          if FFrameworkPool.Exists(TCommunicationFrameworkWithP2PVM_Server(c).FFrameworkWithVM_ID) then
              raiseInfo('P2PVM server is installed');
        end
      else
        begin
          if FFrameworkPool.Count > 0 then
              TCommunicationFrameworkWithP2PVM_Server(c).FFrameworkWithVM_ID := FFrameworkPool.LastPtr^.u32
          else
              TCommunicationFrameworkWithP2PVM_Server(c).FFrameworkWithVM_ID := 1;
          while FFrameworkPool.Exists(TCommunicationFrameworkWithP2PVM_Server(c).FFrameworkWithVM_ID) do
              Inc(TCommunicationFrameworkWithP2PVM_Server(c).FFrameworkWithVM_ID);
        end;

      TCommunicationFrameworkWithP2PVM_Server(c).FLinkVMPool.Add(FVMID, Self);

      FFrameworkPool.Add(TCommunicationFrameworkWithP2PVM_Server(c).FFrameworkWithVM_ID, c, True);

      for i := 0 to TCommunicationFrameworkWithP2PVM_Server(c).ListenCount - 1 do
        begin
          lp := TCommunicationFrameworkWithP2PVM_Server(c).GetListen(i);
          Listen(TCommunicationFrameworkWithP2PVM_Server(c).FFrameworkWithVM_ID, lp^.ListenHost, lp^.ListenPort, lp^.Listening);
        end;
    end
  else if c is TCommunicationFrameworkWithP2PVM_Client then
    begin
      if TCommunicationFrameworkWithP2PVM_Client(c).FFrameworkWithVM_ID <> 0 then
        begin
          if FFrameworkPool.Exists(TCommunicationFrameworkWithP2PVM_Client(c).FFrameworkWithVM_ID) then
              raiseInfo('P2PVM client is installed');
        end
      else
        begin
          if FFrameworkPool.Count > 0 then
              TCommunicationFrameworkWithP2PVM_Client(c).FFrameworkWithVM_ID := FFrameworkPool.LastPtr^.u32
          else
              TCommunicationFrameworkWithP2PVM_Client(c).FFrameworkWithVM_ID := 1;
          while FFrameworkPool.Exists(TCommunicationFrameworkWithP2PVM_Client(c).FFrameworkWithVM_ID) do
              Inc(TCommunicationFrameworkWithP2PVM_Client(c).FFrameworkWithVM_ID);
        end;

      TCommunicationFrameworkWithP2PVM_Client(c).FLinkVM := Self;
      TCommunicationFrameworkWithP2PVM_Client(c).FVMClient := TPeerClientWithP2PVM.Create(TCommunicationFrameworkWithP2PVM_Client(c), nil);
      TCommunicationFrameworkWithP2PVM_Client(c).FVMClient.FLinkVM := Self;

      FFrameworkPool.Add(TCommunicationFrameworkWithP2PVM_Client(c).FFrameworkWithVM_ID, c, True);
    end;
end;

procedure TCommunicationFrameworkWithP2PVM.UninstallLogicFramework(c: TCommunicationFramework);
var
  i : Integer;
  lp: Pp2pVMListen;
begin
  if c is TCommunicationFrameworkWithP2PVM_Server then
    begin
      TCommunicationFrameworkWithP2PVM_Server(c).FLinkVMPool.Delete(FVMID);
      FFrameworkPool.Delete(TCommunicationFrameworkWithP2PVM_Server(c).FFrameworkWithVM_ID);

      i := 0;
      while i < FFrameworkListenPool.Count do
        begin
          lp := FFrameworkListenPool[i];
          if lp^.frameworkID = TCommunicationFrameworkWithP2PVM_Server(c).FFrameworkWithVM_ID then
            begin
              Dispose(lp);
              FFrameworkListenPool.Delete(i);
            end
          else
              Inc(i);
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
      Inc(frameworkID);
  Result := TCommunicationFrameworkWithP2PVM_Client.Create(frameworkID);
  InstallLogicFramework(Result);
end;

procedure TCommunicationFrameworkWithP2PVM.AuthWaiting;
begin
  if FPhysicsTunnel = nil then
      Exit;
  FAuthWaiting := True;
end;

procedure TCommunicationFrameworkWithP2PVM.AuthVM;
begin
  if FPhysicsTunnel = nil then
      Exit;
  if not FAuthed then
    if not FAuthSending then
      begin
        FSendStream.WritePtr(@P2PVMAuthToken[0], Length(P2PVMAuthToken));
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

procedure TCommunicationFrameworkWithP2PVM.echoing(const OnEchoPtr: POnEcho; TimeOut: TTimeTickValue);
var
  u64ptr: UInt64;
  p     : Pp2pVMFragmentPackage;
  i     : Integer;
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
                  Inc(i);
            end;

          if Assigned(OnEchoPtr^.OnEchoCall) then
              OnEchoPtr^.OnEchoCall(False);
          if Assigned(OnEchoPtr^.OnEchoMethod) then
              OnEchoPtr^.OnEchoMethod(False);
          {$IFNDEF FPC}
          if Assigned(OnEchoPtr^.OnEchoProc) then
              OnEchoPtr^.OnEchoProc(False);
          {$ENDIF}

          Dispose(OnEchoPtr);
        end;
      Exit;
    end;

  FWaitEchoList.Add(OnEchoPtr);

  u64ptr := UInt64(p);
  p := BuildP2PVMPackage(8, 0, 0, c_p2pVM_echoing, @u64ptr);

  FSendStream.Position := FSendStream.Size;
  p^.BuildSendBuff(FSendStream);
  FreeP2PVMPackage(p);
end;

procedure TCommunicationFrameworkWithP2PVM.echoing(OnResult: TStateCall; TimeOut: TTimeTickValue);
var
  p: POnEcho;
begin
  New(p);
  p^.OnEchoCall := OnResult;
  p^.OnEchoMethod := nil;
  {$IFNDEF FPC}
  p^.OnEchoProc := nil;
  {$ENDIF}
  p^.TimeOut := GetTimeTick + TimeOut;
  echoing(p, TimeOut);
end;

procedure TCommunicationFrameworkWithP2PVM.echoing(OnResult: TStateMethod; TimeOut: TTimeTickValue);
var
  p: POnEcho;
begin
  New(p);
  p^.OnEchoCall := nil;
  p^.OnEchoMethod := OnResult;
  {$IFNDEF FPC}
  p^.OnEchoProc := nil;
  {$ENDIF}
  p^.TimeOut := GetTimeTick + TimeOut;
  echoing(p, TimeOut);
end;

{$IFNDEF FPC}
procedure TCommunicationFrameworkWithP2PVM.echoing(OnResult: TStateProc; TimeOut: TTimeTickValue);
var
  p: POnEcho;
begin
  New(p);
  p^.OnEchoCall := nil;
  p^.OnEchoMethod := nil;
  p^.OnEchoProc := OnResult;
  p^.TimeOut := GetTimeTick + TimeOut;
  echoing(p, TimeOut);
end;
{$ENDIF}

procedure TCommunicationFrameworkWithP2PVM.echoBuffer(const buff: Pointer; const Siz: NativeInt);
var
  p: Pp2pVMFragmentPackage;
begin
  if (FPhysicsTunnel = nil) or (not WasAuthed) then
      Exit;
  p := BuildP2PVMPackage(Siz, 0, 0, c_p2pVM_echo, buff);

  FSendStream.Position := FSendStream.Size;
  p^.BuildSendBuff(FSendStream);
  FreeP2PVMPackage(p);
end;

procedure TCommunicationFrameworkWithP2PVM.Listen(const frameworkID: Cardinal; const ipv6: TIPV6; const Port: Word; const Listening: Boolean);
var
  lp  : Pp2pVMListen;
  c   : TCommunicationFramework;
  rBuf: array [0 .. 18] of Byte;
  p   : Pp2pVMFragmentPackage;
begin
  if (FPhysicsTunnel = nil) or (not WasAuthed) then
    begin
      lp := FindListen(ipv6, Port);
      if Listening then
        begin
          if lp = nil then
            begin
              New(lp);
              lp^.frameworkID := frameworkID;
              lp^.ListenHost := ipv6;
              lp^.ListenPort := Port;
              lp^.Listening := True;
              FFrameworkListenPool.Add(lp);
            end
          else
              lp^.Listening := True;
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
      PIPV6(@rBuf[0])^ := ipv6;
      PWord(@rBuf[16])^ := Port;
      PBoolean(@rBuf[18])^ := Listening;
      p := BuildP2PVMPackage(SizeOf(rBuf), frameworkID, 0, c_p2pVM_Listen, @rBuf[0]);

      FSendStream.Position := FSendStream.Size;
      p^.BuildSendBuff(FSendStream);
      FreeP2PVMPackage(p);
    end;
end;

procedure TCommunicationFrameworkWithP2PVM.ListenState(const frameworkID: Cardinal; const ipv6: TIPV6; const Port: Word; const Listening: Boolean);
var
  rBuf: array [0 .. 18] of Byte;
  p   : Pp2pVMFragmentPackage;
begin
  if (FPhysicsTunnel = nil) or (not WasAuthed) then
      Exit;
  PIPV6(@rBuf[0])^ := ipv6;
  PWord(@rBuf[16])^ := Port;
  PBoolean(@rBuf[18])^ := Listening;
  p := BuildP2PVMPackage(SizeOf(rBuf), frameworkID, 0, c_p2pVM_ListenState, @rBuf[0]);

  FSendStream.Position := FSendStream.Size;
  p^.BuildSendBuff(FSendStream);
  FreeP2PVMPackage(p);
end;

procedure TCommunicationFrameworkWithP2PVM.Connecting(const Remote_frameworkID, frameworkID, p2pID: Cardinal; const ipv6: TIPV6; const Port: Word);
var
  rBuf: array [0 .. 25] of Byte;
  p   : Pp2pVMFragmentPackage;
begin
  if (FPhysicsTunnel = nil) or (not WasAuthed) then
      Exit;
  PCardinal(@rBuf[0])^ := frameworkID;
  PCardinal(@rBuf[4])^ := p2pID;
  PIPV6(@rBuf[8])^ := ipv6;
  PWord(@rBuf[24])^ := Port;

  p := BuildP2PVMPackage(SizeOf(rBuf), Remote_frameworkID, 0, c_p2pVM_Connecting, @rBuf[0]);

  FSendStream.Position := FSendStream.Size;
  p^.BuildSendBuff(FSendStream);
  FreeP2PVMPackage(p);
end;

procedure TCommunicationFrameworkWithP2PVM.ConnectedReponse(const Remote_frameworkID, Remote_p2pID, frameworkID, p2pID: Cardinal);
var
  rBuf: array [0 .. 7] of Byte;
  p   : Pp2pVMFragmentPackage;
begin
  if (FPhysicsTunnel = nil) or (not WasAuthed) then
      Exit;
  PCardinal(@rBuf[0])^ := frameworkID;
  PCardinal(@rBuf[4])^ := p2pID;

  p := BuildP2PVMPackage(SizeOf(rBuf), Remote_frameworkID, Remote_p2pID, c_p2pVM_ConnectedReponse, @rBuf[0]);

  FSendStream.Position := FSendStream.Size;
  p^.BuildSendBuff(FSendStream);
  FreeP2PVMPackage(p);
end;

procedure TCommunicationFrameworkWithP2PVM.Disconnect(const Remote_frameworkID, Remote_p2pID: Cardinal);
var
  p: Pp2pVMFragmentPackage;
begin
  if (FPhysicsTunnel = nil) or (not WasAuthed) then
      Exit;
  p := BuildP2PVMPackage(0, Remote_frameworkID, Remote_p2pID, c_p2pVM_Disconnect, nil);

  FSendStream.Position := FSendStream.Size;
  p^.BuildSendBuff(FSendStream);
  FreeP2PVMPackage(p);
end;

function TCommunicationFrameworkWithP2PVM.ListenCount: Integer;
begin
  Result := FFrameworkListenPool.Count;
end;

function TCommunicationFrameworkWithP2PVM.GetListen(const Index: Integer): Pp2pVMListen;
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
          Exit;
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
          Exit;
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
          Inc(i);
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
            {$IFDEF FPC}
            TCommunicationFramework(p^.Data).ProgressPerClient(@DoPerClientClose);
            {$ELSE}
            TCommunicationFramework(p^.Data).ProgressPerClient(DoPerClientClose);
          {$ENDIF}
          Inc(i);
          p := p^.next;
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
            {$IFDEF FPC}
            TCommunicationFramework(p^.Data).ProgressPerClient(@DoPerClientClose);
            {$ELSE}
            TCommunicationFramework(p^.Data).ProgressPerClient(DoPerClientClose);
          {$ENDIF}
          Inc(i);
          p := p^.next;
        end;
    end;
end;

procedure Buildp2pVMAuthToken;
var
  i   : Integer;
  Seed: Integer;
begin
  SetLength(P2PVMAuthToken, 1024); // 1k auth buffer
  Seed := $FF00FF00FF;
  for i := 0 to (Length(P2PVMAuthToken) div 4) - 1 do
      PInteger(@P2PVMAuthToken[i * 4])^ := TMISC.Ran03(Seed);
end;

initialization

Buildp2pVMAuthToken;

{$IFDEF FPC}
ProgressBackgroundProc := @DefaultProgressBackgroundProc;
{$ELSE}
ProgressBackgroundProc := DefaultProgressBackgroundProc;
{$ENDIF}

finalization

SetLength(P2PVMAuthToken, 0);

end.
