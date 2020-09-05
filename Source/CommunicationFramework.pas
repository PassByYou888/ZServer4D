{ ****************************************************************************** }
{ * communication framework written by QQ 600585@qq.com                        * }
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

unit CommunicationFramework;

{$INCLUDE zDefine.inc}


interface

uses Classes, SysUtils, Variants,
  CoreClasses,
{$IFDEF FPC}
  FPCGenericStructlist,
{$ENDIF FPC}
  ListEngine, UnicodeMixedLib, DoStatusIO,
  DataFrameEngine, MemoryStream64, PascalStrings, CoreCipher, NotifyObjectBase, Cadencer;

{$REGION 'base Decl'}


type
  TPeerIO = class;
  TCommunicationFramework = class;

  TIPV4 = array [0 .. 3] of Byte;
  PIPV4 = ^TIPV4;

  TIPV6 = array [0 .. 7] of Word;
  PIPV6 = ^TIPV6;

  TConsoleMethod = procedure(Sender: TPeerIO; ResultData: SystemString) of object;
  TConsoleParamMethod = procedure(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, ResultData: SystemString) of object;
  TConsoleFailedMethod = procedure(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: SystemString) of object;
  TStreamMethod = procedure(Sender: TPeerIO; ResultData: TDataFrameEngine) of object;
  TStreamParamMethod = procedure(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, ResultData: TDataFrameEngine) of object;
  TStreamFailedMethod = procedure(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDataFrameEngine) of object;
  TStateCall = procedure(const State: Boolean);
  TStateMethod = procedure(const State: Boolean) of object;
  TIOStateCall = procedure(P_IO: TPeerIO; State: Boolean);
  TIOStateMethod = procedure(P_IO: TPeerIO; State: Boolean) of object;
  TParamStateCall = procedure(Param1: Pointer; Param2: TObject; const State: Boolean);
  TParamStateMethod = procedure(Param1: Pointer; Param2: TObject; const State: Boolean) of object;
  TNotifyCall = procedure();
  TNotifyMethod = procedure() of object;
  TDataNotifyCall = procedure(data: TCoreClassObject);
  TDataNotifyMethod = procedure(data: TCoreClassObject) of object;
  TIONotifyCall = procedure(P_IO: TPeerIO);
  TIONotifyMethod = procedure(P_IO: TPeerIO) of object;
  TProgressBackgroundProc = procedure();
  TProgressBackgroundMethod = procedure() of object;

{$IFDEF FPC}
  TConsoleProc = procedure(Sender: TPeerIO; ResultData: SystemString) is nested;
  TConsoleParamProc = procedure(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, ResultData: SystemString) is nested;
  TConsoleFailedProc = procedure(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: SystemString) is nested;
  TStreamProc = procedure(Sender: TPeerIO; ResultData: TDataFrameEngine) is nested;
  TStreamParamProc = procedure(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, ResultData: TDataFrameEngine) is nested;
  TStreamFailedProc = procedure(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDataFrameEngine) is nested;
  TStateProc = procedure(const State: Boolean) is nested;
  TIOStateProc = procedure(P_IO: TPeerIO; State: Boolean) is nested;
  TParamStateProc = procedure(Param1: Pointer; Param2: TObject; const State: Boolean) is nested;
  TNotifyProc = procedure() is nested;
  TDataNotifyProc = procedure(data: TCoreClassObject) is nested;
  TIONotifyProc = procedure(P_IO: TPeerIO) is nested;
{$ELSE FPC}
  TConsoleProc = reference to procedure(Sender: TPeerIO; ResultData: SystemString);
  TConsoleParamProc = reference to procedure(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, ResultData: SystemString);
  TConsoleFailedProc = reference to procedure(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: SystemString);
  TStreamProc = reference to procedure(Sender: TPeerIO; ResultData: TDataFrameEngine);
  TStreamParamProc = reference to procedure(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData, ResultData: TDataFrameEngine);
  TStreamFailedProc = reference to procedure(Sender: TPeerIO; Param1: Pointer; Param2: TObject; SendData: TDataFrameEngine);
  TStateProc = reference to procedure(const State: Boolean);
  TIOStateProc = reference to procedure(P_IO: TPeerIO; State: Boolean);
  TParamStateProc = reference to procedure(Param1: Pointer; Param2: TObject; const State: Boolean);
  TNotifyProc = reference to procedure();
  TDataNotifyProc = reference to procedure(data: TCoreClassObject);
  TIONotifyProc = reference to procedure(P_IO: TPeerIO);
{$ENDIF FPC}

  TOnStateStruct = record
    OnCall: TStateCall;
    OnMethod: TStateMethod;
    OnProc: TStateProc;
    procedure Init;
  end;

  POnStateStruct = ^TOnStateStruct;

  TStateParamBridge = class
  public
    OnNotifyC: TParamStateCall;
    OnNotifyM: TParamStateMethod;
    OnNotifyP: TParamStateProc;
    Param1: Pointer;
    Param2: TObject;
    constructor Create;
    procedure DoStateResult(const State: Boolean);
  end;

  TQueueState = (qsUnknow, qsSendConsoleCMD, qsSendStreamCMD, qsSendDirectConsoleCMD, qsSendDirectStreamCMD, qsSendBigStream, qsSendCompleteBuffer);

  TQueueData = record
    { queue state }
    State: TQueueState;
    { ID }
    IO_ID: Cardinal;
    { command }
    Cmd: SystemString;
    { cipher key }
    Cipher: TCipherSecurity;
    { console data }
    ConsoleData: SystemString;
    { console event }
    OnConsoleMethod: TConsoleMethod;
    OnConsoleParamMethod: TConsoleParamMethod;
    OnConsoleFailedMethod: TConsoleFailedMethod;
    OnConsoleProc: TConsoleProc;
    OnConsoleParamProc: TConsoleParamProc;
    OnConsoleFailedProc: TConsoleFailedProc;
    { stream data }
    StreamData: TMemoryStream64;
    { stream event }
    OnStreamMethod: TStreamMethod;
    OnStreamParamMethod: TStreamParamMethod;
    OnStreamFailedMethod: TStreamFailedMethod;
    OnStreamProc: TStreamProc;
    OnStreamParamProc: TStreamParamProc;
    OnStreamFailedProc: TStreamFailedProc;
    { bigStream }
    BigStreamStartPos: Int64;
    BigStream: TCoreClassStream;
    { complete buffer }
    buffer: PByte;
    BufferSize: NativeInt;
    { memory recycle }
    DoneAutoFree: Boolean;
    { console and stream param }
    Param1: Pointer;
    Param2: TObject;
  end;

  PQueueData = ^TQueueData;

{$ENDREGION 'base Decl'}
{$REGION 'IO Decl'}
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

{$IFDEF FPC}
  TCommandStreamProc = procedure(Sender: TPeerIO; InData, OutData: TDataFrameEngine) is nested;
  TCommandConsoleProc = procedure(Sender: TPeerIO; InData: SystemString; var OutData: SystemString) is nested;
  TCommandDirectStreamProc = procedure(Sender: TPeerIO; InData: TDataFrameEngine) is nested;
  TCommandDirectConsoleProc = procedure(Sender: TPeerIO; InData: SystemString) is nested;
  TCommandBigStreamProc = procedure(Sender: TPeerIO; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64) is nested;
  TCommandCompleteBufferProc = procedure(Sender: TPeerIO; InData: PByte; DataSize: NativeInt) is nested;
{$ELSE FPC}
  TCommandStreamProc = reference to procedure(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
  TCommandConsoleProc = reference to procedure(Sender: TPeerIO; InData: SystemString; var OutData: SystemString);
  TCommandDirectStreamProc = reference to procedure(Sender: TPeerIO; InData: TDataFrameEngine);
  TCommandDirectConsoleProc = reference to procedure(Sender: TPeerIO; InData: SystemString);
  TCommandBigStreamProc = reference to procedure(Sender: TPeerIO; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64);
  TCommandCompleteBufferProc = reference to procedure(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
{$ENDIF FPC}

  TCommandStream = class(TCoreClassObject)
  protected
    FOnExecuteCall: TCommandStreamCall;
    FOnExecuteMethod: TCommandStreamMethod;
    FOnExecuteProc: TCommandStreamProc;
  public
    constructor Create;
    destructor Destroy; override;

    function Execute(Sender: TPeerIO; InData, OutData: TDataFrameEngine): Boolean;
    property OnExecute: TCommandStreamMethod read FOnExecuteMethod write FOnExecuteMethod;
    property OnExecuteCall: TCommandStreamCall read FOnExecuteCall write FOnExecuteCall;
    property OnExecuteMethod: TCommandStreamMethod read FOnExecuteMethod write FOnExecuteMethod;
    property OnExecuteProc: TCommandStreamProc read FOnExecuteProc write FOnExecuteProc;
  end;

  TCommandConsole = class(TCoreClassObject)
  protected
    FOnExecuteCall: TCommandConsoleCall;
    FOnExecuteMethod: TCommandConsoleMethod;
    FOnExecuteProc: TCommandConsoleProc;
  public
    constructor Create;
    destructor Destroy; override;

    function Execute(Sender: TPeerIO; InData: SystemString; var OutData: SystemString): Boolean;
    property OnExecute: TCommandConsoleMethod read FOnExecuteMethod write FOnExecuteMethod;
    property OnExecuteCall: TCommandConsoleCall read FOnExecuteCall write FOnExecuteCall;
    property OnExecuteMethod: TCommandConsoleMethod read FOnExecuteMethod write FOnExecuteMethod;
    property OnExecuteProc: TCommandConsoleProc read FOnExecuteProc write FOnExecuteProc;
  end;

  TCommandDirectStream = class(TCoreClassObject)
  protected
    FOnExecuteCall: TCommandDirectStreamCall;
    FOnExecuteMethod: TCommandDirectStreamMethod;
    FOnExecuteProc: TCommandDirectStreamProc;
  public
    constructor Create;
    destructor Destroy; override;

    function Execute(Sender: TPeerIO; InData: TDataFrameEngine): Boolean;
    property OnExecute: TCommandDirectStreamMethod read FOnExecuteMethod write FOnExecuteMethod;
    property OnExecuteCall: TCommandDirectStreamCall read FOnExecuteCall write FOnExecuteCall;
    property OnExecuteMethod: TCommandDirectStreamMethod read FOnExecuteMethod write FOnExecuteMethod;
    property OnExecuteProc: TCommandDirectStreamProc read FOnExecuteProc write FOnExecuteProc;
  end;

  TCommandDirectConsole = class(TCoreClassObject)
  protected
    FOnExecuteCall: TCommandDirectConsoleCall;
    FOnExecuteMethod: TCommandDirectConsoleMethod;
    FOnExecuteProc: TCommandDirectConsoleProc;
  public
    constructor Create;
    destructor Destroy; override;

    function Execute(Sender: TPeerIO; InData: SystemString): Boolean;
    property OnExecute: TCommandDirectConsoleMethod read FOnExecuteMethod write FOnExecuteMethod;
    property OnExecuteCall: TCommandDirectConsoleCall read FOnExecuteCall write FOnExecuteCall;
    property OnExecuteMethod: TCommandDirectConsoleMethod read FOnExecuteMethod write FOnExecuteMethod;
    property OnExecuteProc: TCommandDirectConsoleProc read FOnExecuteProc write FOnExecuteProc;
  end;

  TCommandBigStream = class(TCoreClassObject)
  protected
    FOnExecuteCall: TCommandBigStreamCall;
    FOnExecuteMethod: TCommandBigStreamMethod;
    FOnExecuteProc: TCommandBigStreamProc;
  public
    constructor Create;
    destructor Destroy; override;

    function Execute(Sender: TPeerIO; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64): Boolean;
    property OnExecute: TCommandBigStreamMethod read FOnExecuteMethod write FOnExecuteMethod;
    property OnExecuteCall: TCommandBigStreamCall read FOnExecuteCall write FOnExecuteCall;
    property OnExecuteMethod: TCommandBigStreamMethod read FOnExecuteMethod write FOnExecuteMethod;
    property OnExecuteProc: TCommandBigStreamProc read FOnExecuteProc write FOnExecuteProc;
  end;

  TCommandCompleteBuffer = class(TCoreClassObject)
  protected
    FOnExecuteCall: TCommandCompleteBufferCall;
    FOnExecuteMethod: TCommandCompleteBufferMethod;
    FOnExecuteProc: TCommandCompleteBufferProc;
  public
    constructor Create;
    destructor Destroy; override;

    function Execute(Sender: TPeerIO; InData: PByte; DataSize: NativeInt): Boolean;
    property OnExecute: TCommandCompleteBufferMethod read FOnExecuteMethod write FOnExecuteMethod;
    property OnExecuteCall: TCommandCompleteBufferCall read FOnExecuteCall write FOnExecuteCall;
    property OnExecuteMethod: TCommandCompleteBufferMethod read FOnExecuteMethod write FOnExecuteMethod;
    property OnExecuteProc: TCommandCompleteBufferProc read FOnExecuteProc write FOnExecuteProc;
  end;

  PBigStreamBatchPostData = ^TBigStreamBatchPostData;

  TBigStreamBatchPostData = record
    Source: TMemoryStream64;
    CompletedBackcallPtr: UInt64;
    RemoteMD5: TMD5;
    SourceMD5: TMD5;
    index: Integer;
    DBStorePos: Int64;

    procedure Init;
    procedure Encode(d: TDataFrameEngine);
    procedure Decode(d: TDataFrameEngine);
  end;

  TBigStreamBatch = class(TCoreClassObject)
  protected
    FOwner: TPeerIO;
    FList: TCoreClassList;
    function GetItems(const index: Integer): PBigStreamBatchPostData;
  public
    constructor Create(Owner_: TPeerIO);
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
    FBigStreamBatch: TBigStreamBatch;
    FBusy: Boolean;
    procedure DelayFreeOnBusy;
  public
    constructor Create(Owner_: TPeerIO); virtual;
    destructor Destroy; override;
    procedure Progress; virtual;
    property Owner: TPeerIO read FOwner;
    property WorkPlatform: TExecutePlatform read FWorkPlatform write FWorkPlatform;
    // Stream Batch support
    property BigStreamBatchList: TBigStreamBatch read FBigStreamBatch;
    property BigStreamBatch: TBigStreamBatch read FBigStreamBatch;
    property Busy: Boolean read FBusy write FBusy; // default is false, if busy is true do delayed destruction.
  end;

  TPeerIOUserDefineClass = class of TPeerIOUserDefine;

  TPeerIOUserSpecial = class(TCoreClassInterfacedObject)
  protected
    FOwner: TPeerIO;
    FBusy: Boolean;
    procedure DelayFreeOnBusy;
  public
    constructor Create(Owner_: TPeerIO); virtual;
    destructor Destroy; override;
    procedure Progress; virtual;
    property Owner: TPeerIO read FOwner;
    property Busy: Boolean read FBusy write FBusy; // default is false, if busy is true do delayed destruction.
  end;

  TPeerIOUserSpecialClass = class of TPeerIOUserSpecial;

  TPeerClientUserDefine = TPeerIOUserDefine;
  TPeerClientUserSpecial = TPeerIOUserSpecial;

  PSequencePacket = ^TSequencePacket;

  TSequencePacket = record
    SequenceNumber: Cardinal;
    Size: Word;
    hash: TMD5;
    data: TMemoryStream64;
    tick: TTimeTick;
  end;

  PIDLE_Trace = ^TIDLE_Trace;

  TIDLE_Trace = record
    ID: Cardinal;
    data: TCoreClassObject;
    OnNotifyC: TDataNotifyCall;
    OnNotifyM: TDataNotifyMethod;
    OnNotifyP: TDataNotifyProc;
  end;

  POnEcho = ^TOnEcho;

  TOnEcho = record
    OnEchoCall: TStateCall;
    OnEchoMethod: TStateMethod;
    OnEchoProc: TStateProc;
    Timeout: TTimeTick;
  end;

  TBigStreamFragmentHead = packed record
    Size: Integer;
    Compressed: Boolean;
  end;

  PBigStreamFragmentHead = ^TBigStreamFragmentHead;

{$ENDREGION 'IODecl'}
{$REGION 'IO'}
  TInternalSendByteBuffer = procedure(const Sender: TPeerIO; const buff: PByte; siz: NativeInt) of object;
  TInternalSaveReceiveBuffer = procedure(const Sender: TPeerIO; const buff: Pointer; siz: Int64) of object;
  TInternalProcessReceiveBuffer = procedure(const Sender: TPeerIO; const CurrentActiveThread_: TCoreClassThread; const RecvSync, SendSync: Boolean) of object;
  TInternalProcessAllSendCmd = procedure(const Sender: TPeerIO; const CurrentActiveThread_: TCoreClassThread; const RecvSync, SendSync: Boolean) of object;
  TInternalIOCreate = procedure(const Sender: TPeerIO) of object;
  TInternalIODestory = procedure(const Sender: TPeerIO) of object;

  TCommunicationFrameworkWithP2PVM = class;

  TPeerIO = class(TCoreClassInterfacedObject)
  protected
    FCritical, FCustomProtocolCritical: TCritical;
    FOwnerFramework: TCommunicationFramework;
    FIOInterface: TCoreClassObject;
    FID: Cardinal;
    FHeadToken, FTailToken: Cardinal;
    FConsoleToken: Byte;
    FStreamToken: Byte;
    FDirectConsoleToken: Byte;
    FDirectStreamToken: Byte;
    FBigStreamToken: Byte;
    FBigStreamReceiveFragmentSignal: Byte;
    FBigStreamReceiveDoneSignal: Byte;
    FCompleteBufferToken: Byte;
    FReceivedAbort: Boolean;
    FReceivedBuffer: TMemoryStream64;
    FReceivedBuffer_Busy: TMemoryStream64;
    FBigStreamReceiveProcessing: Boolean;
    FBigStreamTotal: Int64;
    FBigStreamCompleted: Int64;
    FBigStreamCmd: SystemString;
    FSyncBigStreamReceive: TCoreClassStream;
    FBigStreamSending: TCoreClassStream;
    FBigStreamSendCurrentPos: Int64;
    FBigStreamSendDoneTimeFree: Boolean;
    FWaitBigStreamReceiveDoneSignal: Boolean;
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
    FLastCommunicationTick: TTimeTick;
    LastCommunicationTick_Received: TTimeTick;
    LastCommunicationTick_KeepAlive: TTimeTick;
    LastCommunicationTick_Sending: TTimeTick;
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
  public
    { external interface }
    function Connected: Boolean; virtual;
    procedure Disconnect; virtual;
    procedure SendByteBuffer(const buff: PByte; const Size: NativeInt); virtual;
    procedure WriteBufferOpen; virtual;
    procedure WriteBufferFlush; virtual;
    procedure WriteBufferClose; virtual;
    function GetPeerIP: SystemString; virtual;
    function WriteBufferEmpty: Boolean; virtual;
  protected
    { Sequence Packet Model }
    FSequencePacketActivted, FSequencePacketSignal: Boolean;
    SequenceNumberOnSendCounter, SequenceNumberOnReceivedCounter: Cardinal;
    SendingSequencePacketHistory: TUInt32HashPointerList;
    SequencePacketReceivedPool: TUInt32HashPointerList;
    SendingSequencePacketHistoryMemory, SequencePacketReceivedPoolMemory: Int64;
    IOSendBuffer, SequencePacketSendBuffer, SequencePacketReceivedBuffer: TMemoryStream64;

    { performance }
    FSequencePacketMTU: Word;

    { Security }
    FSequencePacketLimitPhysicsMemory: Int64;
    SequencePacketCloseDone: Boolean;

    SequencePacketVerifyTick: TTimeTick;

    procedure InitSequencePacketModel(const hashSize, MemoryDelta: Integer);
    procedure FreeSequencePacketModel;
    procedure ResetSequencePacketBuffer;
    procedure ProcessSequencePacketModel;
    function GetSequencePacketState: SystemString;
    function GetSequencePacketUsagePhysicsMemory: Int64;
    function ComputeSequencePacketHash(const p: PByte; const Count: nativeUInt): TMD5; inline;
    function IsSequencePacketModel: Boolean; inline;
    procedure FlushIOSendBuffer;
    procedure SendSequencePacketBegin;
    procedure SendSequencePacket(const buff: PByte; siz: NativeInt);
    procedure SendSequencePacketEnd;
    procedure SendSequencePacketKeepAlive(p: Pointer; siz: Word);
    procedure DoSequencePacketEchoKeepAlive(p: Pointer; siz: Word); virtual; { event: echo KeepAlive }
    procedure WriteSequencePacket(p: PSequencePacket);
    procedure ResendSequencePacket(SequenceNumber: Cardinal);
    function FillSequencePacketTo(const buff: Pointer; siz: Int64; ExtractDest: TMemoryStream64): Boolean;

    procedure Send_Free_OnPtr(p: Pointer);
    procedure Send_Add_OnPtr(p: Pointer);
    procedure Received_Free_OnPtr(p: Pointer);
    procedure Received_Add_OnPtr(p: Pointer);
  protected
    { private vm and protocol stack support }
    FP2PVMTunnel: TCommunicationFrameworkWithP2PVM;
    { vm auth token buffer }
    FP2PAuthToken: TBytes;
    { vm hook }
    OnInternalSendByteBuffer: TInternalSendByteBuffer;
    OnInternalSaveReceiveBuffer: TInternalSaveReceiveBuffer;
    OnInternalProcessReceiveBuffer: TInternalProcessReceiveBuffer;
    OnInternalProcessAllSendCmd: TInternalProcessAllSendCmd;
    OnCreate: TInternalIOCreate;
    OnDestroy: TInternalIODestory;
  protected
    { p2p vm: auth model result }
    OnVMBuildAuthModelResultCall: TNotifyCall;
    OnVMBuildAuthModelResultMethod: TNotifyMethod;
    OnVMBuildAuthModelResultProc: TNotifyProc;
    OnVMBuildAuthModelResultIOCall: TIONotifyCall;
    OnVMBuildAuthModelResultIOMethod: TIONotifyMethod;
    OnVMBuildAuthModelResultIOProc: TIONotifyProc;
    { p2p vm: auth result }
    OnVMAuthResultCall: TStateCall;
    OnVMAuthResultMethod: TStateMethod;
    OnVMAuthResultProc: TStateProc;
    OnVMAuthResultIOCall: TIOStateCall;
    OnVMAuthResultIOMethod: TIOStateMethod;
    OnVMAuthResultIOProc: TIOStateProc;
    procedure P2PVMAuthSuccess(Sender: TCommunicationFrameworkWithP2PVM);
  protected
    { automated P2PVM }
    FAutomatedP2PVMClient_Connection_Sequence: Integer;
    FAutomatedP2PVMClient_Connection_Sequence_Successed: Integer;
    FOnAutomatedP2PVMClientConnectionDoneCall: TIOStateCall;
    FOnAutomatedP2PVMClientConnectionDoneMethod: TIOStateMethod;
    FOnAutomatedP2PVMClientConnectionDoneProc: TIOStateProc;
  protected
    { user custom }
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
    BeginSendState: Boolean;

    procedure BeginSend;
    procedure Send(const buff: PByte; siz: NativeInt);
    procedure EndSend;

    procedure SendInteger(v: Integer);
    procedure SendCardinal(v: Cardinal);
    procedure SendInt64(v: Int64);
    procedure SendByte(v: Byte);
    procedure SendWord(v: Word);
    procedure SendVerifyCode(buff: Pointer; siz: NativeInt);
    procedure SendEncryptBuffer(buff: PByte; siz: NativeInt; cs: TCipherSecurity);
    procedure SendEncryptMemoryStream(Stream: TMemoryStream64; cs: TCipherSecurity);

    procedure InternalSendConsoleBuff(buff: TMemoryStream64; cs: TCipherSecurity);
    procedure InternalSendStreamBuff(buff: TMemoryStream64; cs: TCipherSecurity);
    procedure InternalSendDirectConsoleBuff(buff: TMemoryStream64; cs: TCipherSecurity);
    procedure InternalSendDirectStreamBuff(buff: TMemoryStream64; cs: TCipherSecurity);
    procedure InternalSendBigStreamHeader(Cmd: SystemString; streamSiz: Int64);
    procedure InternalSendBigStreamBuff(var Queue: TQueueData);
    procedure InternalSendCompleteBufferHeader(Cmd: SystemString; buffSiz, compSiz: Cardinal);
    procedure InternalSendCompleteBufferBuff(var Queue: TQueueData);
    procedure InternalSendBigStreamFragmentSignal;
    procedure InternalSendBigStreamDoneSignal;
    procedure SendBigStreamMiniPacket(buff: PByte; Size: NativeInt);

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
    procedure ExecuteDataFrame(CurrentActiveThread_: TCoreClassThread; const RecvSync, SendSync: Boolean; CommDataType: Byte; DataFrame: TDataFrameEngine);

    procedure Sync_ExecuteBigStream;
    function ReceivedBigStreamFragment(CurrentActiveThread_: TCoreClassThread; const Sync: Boolean): Boolean;

    procedure Sync_ExecuteCompleteBuffer;
    function FillCompleteBufferBuffer(CurrentActiveThread_: TCoreClassThread; const Sync: Boolean): Boolean;

    procedure Sync_ExecuteResult;
    function FillWaitOnResultBuffer(CurrentActiveThread_: TCoreClassThread; const RecvSync, SendSync: Boolean): Boolean;

    procedure InternalSaveReceiveBuffer(const buff: Pointer; siz: Int64);
    procedure InternalProcessReceiveBuffer(const CurrentActiveThread_: TCoreClassThread; const RecvSync, SendSync: Boolean);
    procedure InternalProcessAllSendCmd(const CurrentActiveThread_: TCoreClassThread; const RecvSync, SendSync: Boolean);

    procedure Sync_CheckAndTriggerFailedWaitResult;
    procedure CheckAndTriggerFailedWaitResult;

    procedure InternalCloseP2PVMTunnel;
  public
    constructor Create(OwnerFramework_: TCommunicationFramework; IOInterface_: TCoreClassObject);
    procedure CreateAfter; virtual;
    destructor Destroy; override;

    // check IO state
    function IOBusy: Boolean;

    procedure IO_IDLE_TraceC(data: TCoreClassObject; OnNotify: TDataNotifyCall);
    procedure IO_IDLE_TraceM(data: TCoreClassObject; OnNotify: TDataNotifyMethod);
    procedure IO_IDLE_TraceP(data: TCoreClassObject; OnNotify: TDataNotifyProc);

    { Sequence Packet model support }
    property SequencePacketSignal: Boolean read FSequencePacketSignal;
    { performance }
    property SequencePacketMTU: Word read FSequencePacketMTU write FSequencePacketMTU; { default set 1536 }
    { Security }
    property SequencePacketLimitPhysicsMemory: Int64 read FSequencePacketLimitPhysicsMemory write FSequencePacketLimitPhysicsMemory; { default set 0,no limit }
    { memory status }
    property SequencePacketUsagePhysicsMemory: Int64 read GetSequencePacketUsagePhysicsMemory;
    { information }
    property SequencePacketState: SystemString read GetSequencePacketState;

    { p2pVM Tunnel support }
    property p2pVM: TCommunicationFrameworkWithP2PVM read FP2PVMTunnel;
    property p2pVMTunnel: TCommunicationFrameworkWithP2PVM read FP2PVMTunnel;
    { p2pVM build safe Auth token }
    procedure BuildP2PAuthToken; overload;
    procedure BuildP2PAuthTokenC(const OnResult: TNotifyCall);
    procedure BuildP2PAuthTokenM(const OnResult: TNotifyMethod);
    procedure BuildP2PAuthTokenP(const OnResult: TNotifyProc);
    procedure BuildP2PAuthTokenIO_C(const OnResult: TIONotifyCall);
    procedure BuildP2PAuthTokenIO_M(const OnResult: TIONotifyMethod);
    procedure BuildP2PAuthTokenIO_P(const OnResult: TIONotifyProc);
    { p2pVM Open Tunnel }
    procedure OpenP2PVMTunnel(vmHashPoolSize: Integer; SendRemoteRequest: Boolean; const AuthToken: SystemString); overload;
    procedure OpenP2PVMTunnel(SendRemoteRequest: Boolean; const AuthToken: SystemString); overload;
    procedure OpenP2PVMTunnelC(SendRemoteRequest: Boolean; const AuthToken: SystemString; OnResult: TStateCall); overload;
    procedure OpenP2PVMTunnelM(SendRemoteRequest: Boolean; const AuthToken: SystemString; OnResult: TStateMethod); overload;
    procedure OpenP2PVMTunnelP(SendRemoteRequest: Boolean; const AuthToken: SystemString; OnResult: TStateProc); overload;
    procedure OpenP2PVMTunnelC(vmHashPoolSize: Integer; SendRemoteRequest: Boolean; const AuthToken: SystemString; OnResult: TStateCall); overload;
    procedure OpenP2PVMTunnelM(vmHashPoolSize: Integer; SendRemoteRequest: Boolean; const AuthToken: SystemString; OnResult: TStateMethod); overload;
    procedure OpenP2PVMTunnelP(vmHashPoolSize: Integer; SendRemoteRequest: Boolean; const AuthToken: SystemString; OnResult: TStateProc); overload;
    procedure OpenP2PVMTunnelIO_C(SendRemoteRequest: Boolean; const AuthToken: SystemString; OnResult: TIOStateCall); overload;
    procedure OpenP2PVMTunnelIO_M(SendRemoteRequest: Boolean; const AuthToken: SystemString; OnResult: TIOStateMethod); overload;
    procedure OpenP2PVMTunnelIO_P(SendRemoteRequest: Boolean; const AuthToken: SystemString; OnResult: TIOStateProc); overload;
    procedure OpenP2PVMTunnelIO_C(vmHashPoolSize: Integer; SendRemoteRequest: Boolean; const AuthToken: SystemString; OnResult: TIOStateCall); overload;
    procedure OpenP2PVMTunnelIO_M(vmHashPoolSize: Integer; SendRemoteRequest: Boolean; const AuthToken: SystemString; OnResult: TIOStateMethod); overload;
    procedure OpenP2PVMTunnelIO_P(vmHashPoolSize: Integer; SendRemoteRequest: Boolean; const AuthToken: SystemString; OnResult: TIOStateProc); overload;
    procedure OpenP2PVMTunnel; overload;
    { p2pVM Close Tunnel }
    procedure CloseP2PVMTunnel;
    { state }
    procedure PrintError(v: SystemString); overload;
    procedure PrintError(v: SystemString; const Args: array of const); overload;
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

    { delay close on now }
    procedure DelayClose; overload;
    { delay close on custom delay of double time }
    procedure DelayClose(const t: double); overload;
    { delay free on custom delay of double time }
    procedure DelayFree; overload;
    procedure DelayFree(const t: double); overload;

    procedure SaveReceiveBuffer(const p: Pointer; siz: Int64);
    procedure FillRecvBuffer(const CurrentActiveThread_: TCoreClassThread; const RecvSync, SendSync: Boolean);
    procedure ProcessAllSendCmd(const CurrentActiveThread_: TCoreClassThread; const RecvSync, SendSync: Boolean);
    procedure PostQueueData(p: PQueueData);

    { custom protocol }
    procedure WriteCustomBuffer(const buffer: PByte; const Size: NativeInt);

    { delay reponse }
    procedure PauseResultSend; virtual;
    procedure ContinueResultSend; virtual;
    { ContinueResultSend usage }
    property InText: SystemString read FInText;
    property OutText: SystemString read FOutText write FOutText;
    property InDataFrame: TDataFrameEngine read FInDataFrame;
    property OutDataFrame: TDataFrameEngine read FOutDataFrame;
    function ResultSendIsPaused: Boolean;

    { state }
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
    function GetBigStreamBatch: TBigStreamBatch;
    property BigStreamBatchList: TBigStreamBatch read GetBigStreamBatch;
    property BigStreamBatch: TBigStreamBatch read GetBigStreamBatch;

    { framework }
    property OwnerFramework: TCommunicationFramework read FOwnerFramework;
    property IOInterface: TCoreClassObject read FIOInterface write FIOInterface;
    procedure SetID(const Value: Cardinal);
    property ID: Cardinal read FID write SetID;
    property CipherKey: TCipherKeyBuffer read FCipherKey;
    function CipherKeyPtr: PCipherKeyBuffer;
    property SendCipherSecurity: TCipherSecurity read FSendDataCipherSecurity write FSendDataCipherSecurity;
    property RemoteExecutedForConnectInit: Boolean read FRemoteExecutedForConnectInit write FRemoteExecutedForConnectInit;

    { remote }
    property PeerIP: SystemString read GetPeerIP;

    { user define }
    property UserVariants: THashVariantList read GetUserVariants;
    property UserObjects: THashObjectList read GetUserObjects;
    property UserAutoFreeObjects: THashObjectList read GetUserAutoFreeObjects;
    property UserData: Pointer read FUserData write FUserData;
    property UserValue: Variant read FUserValue write FUserValue;

    property UserDefine: TPeerIOUserDefine read FUserDefine;
    property Define: TPeerIOUserDefine read FUserDefine;

    property UserSpecial: TPeerIOUserSpecial read FUserSpecial;
    property Special: TPeerIOUserSpecial read FUserSpecial;

    { hash code }
    procedure GenerateHashCode(const hs: THashSecurity; buff: Pointer; siz: Integer; var output: TBytes);
    function VerifyHashCode(const hs: THashSecurity; buff: Pointer; siz: Integer; var Code: TBytes): Boolean;

    { encrypt }
    procedure Encrypt(cs: TCipherSecurity; DataPtr: Pointer; Size: Cardinal; var k: TCipherKeyBuffer; enc: Boolean);

    { TimeOut Tick }
    function StopCommunicationTime: TTimeTick;
    procedure UpdateLastCommunicationTime;
    property LastCommunicationTime: TTimeTick read FLastCommunicationTick;
    property LastCommunicationTimeTick: TTimeTick read FLastCommunicationTick;

    { queue data }
    property CurrentQueueData: PQueueData read FCurrentQueueData;

    { send console cmd and result method }
    procedure SendConsoleCmdM(Cmd, ConsoleData: SystemString; OnResult: TConsoleMethod); overload;
    procedure SendConsoleCmdM(Cmd, ConsoleData: SystemString; Param1: Pointer; Param2: TObject; OnResult: TConsoleParamMethod); overload;
    procedure SendConsoleCmdM(Cmd, ConsoleData: SystemString; Param1: Pointer; Param2: TObject; OnResult: TConsoleParamMethod; OnFailed: TConsoleFailedMethod); overload;
    { send stream cmd and result method }
    procedure SendStreamCmdM(Cmd: SystemString; StreamData: TMemoryStream64; OnResult: TStreamMethod; DoneAutoFree: Boolean); overload;
    procedure SendStreamCmdM(Cmd: SystemString; StreamData: TDataFrameEngine; OnResult: TStreamMethod); overload;
    procedure SendStreamCmdM(Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamMethod); overload;
    procedure SendStreamCmdM(Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamMethod; OnFailed: TStreamFailedMethod); overload;

    { send console cmd and result proc }
    procedure SendConsoleCmdP(Cmd, ConsoleData: SystemString; OnResult: TConsoleProc); overload;
    procedure SendConsoleCmdP(Cmd, ConsoleData: SystemString; Param1: Pointer; Param2: TObject; OnResult: TConsoleParamProc); overload;
    procedure SendConsoleCmdP(Cmd, ConsoleData: SystemString; Param1: Pointer; Param2: TObject; OnResult: TConsoleParamProc; OnFailed: TConsoleFailedProc); overload;
    { send stream cmd and result proc }
    procedure SendStreamCmdP(Cmd: SystemString; StreamData: TMemoryStream64; OnResult: TStreamProc; DoneAutoFree: Boolean); overload;
    procedure SendStreamCmdP(Cmd: SystemString; StreamData: TDataFrameEngine; OnResult: TStreamProc); overload;
    procedure SendStreamCmdP(Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamProc); overload;
    procedure SendStreamCmdP(Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamProc; OnFailed: TStreamFailedProc); overload;

    { direct send cmd }
    procedure SendDirectConsoleCmd(Cmd: SystemString; ConsoleData: SystemString); overload;
    procedure SendDirectConsoleCmd(Cmd: SystemString); overload;
    procedure SendDirectStreamCmd(Cmd: SystemString; StreamData: TMemoryStream64; DoneAutoFree: Boolean); overload;
    procedure SendDirectStreamCmd(Cmd: SystemString; StreamData: TDataFrameEngine); overload;
    procedure SendDirectStreamCmd(Cmd: SystemString); overload;

    { wait send cmd }
    function WaitSendConsoleCmd(Cmd, ConsoleData: SystemString; Timeout: TTimeTick): SystemString;
    procedure WaitSendStreamCmd(Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; Timeout: TTimeTick);

    { send bigstream }
    procedure SendBigStream(Cmd: SystemString; BigStream: TCoreClassStream; StartPos: Int64; DoneAutoFree: Boolean); overload;
    procedure SendBigStream(Cmd: SystemString; BigStream: TCoreClassStream; DoneAutoFree: Boolean); overload;

    { send complete buffer }
    procedure SendCompleteBuffer(Cmd: SystemString; buff: PByte; BuffSize: NativeInt; DoneAutoFree: Boolean);
  end;

  TPeerIOClass = class of TPeerIO;

  TPeerClient = TPeerIO;
  TPeerClientClass = TPeerIOClass;
{$ENDREGION 'IO'}
{$REGION 'CommunicationFramework'}
  TPeerIOCMDNotify = procedure(Sender: TPeerIO; Cmd: SystemString; var Allow: Boolean) of object;

  TStatisticsType = (
    stReceiveSize, stSendSize,
    stRequest, stResponse,
    stConsole, stStream, stDirestConsole, stDirestStream, stReceiveBigStream, stSendBigStream, stReceiveCompleteBuffer, stSendCompleteBuffer,
    stExecConsole, stExecStream, stExecDirestConsole, stExecDirestStream, stExecBigStream, stExecCompleteBuffer,
    stTriggerConnect, stTriggerDisconnect,
    stTotalCommandExecute, stTotalCommandSend, stTotalCommandReg,
    stEncrypt, stCompress, stGenerateHash,
    stSequencePacketMemoryOnSending, stSequencePacketMemoryOnReceived,
    stSequencePacketReceived, stSequencePacketEcho, stSequencePacketRequestResend,
    stSequencePacketMatched, stSequencePacketPlan, stSequencePacketDiscard, stSequencePacketDiscardSize,
    stPause, stContinue,
    stCommunicationFrameworkLock, stCommunicationFrameworkUnLock, stIOLock, stIOUnLock,
    stTimeOutDisconnect,
    stPrint);

  TPeerIOListCall = procedure(P_IO: TPeerIO);
  TPeerIOListMethod = procedure(P_IO: TPeerIO) of object;
{$IFDEF FPC}
  TPeerIOListProc = procedure(P_IO: TPeerIO) is nested;
{$ELSE FPC}
  TPeerIOListProc = reference to procedure(P_IO: TPeerIO);
{$ENDIF FPC}

  IIOInterface = interface
    procedure PeerIO_Create(const Sender: TPeerIO);
    procedure PeerIO_Destroy(const Sender: TPeerIO);
  end;

  TIO_Array = array of Cardinal;

  ICommunicationFrameworkVMInterface = interface
    procedure p2pVMTunnelAuth(Sender: TPeerIO; const Token: SystemString; var Accept: Boolean);
    procedure p2pVMTunnelOpenBefore(Sender: TPeerIO; p2pVMTunnel: TCommunicationFrameworkWithP2PVM);
    procedure p2pVMTunnelOpen(Sender: TPeerIO; p2pVMTunnel: TCommunicationFrameworkWithP2PVM);
    procedure p2pVMTunnelOpenAfter(Sender: TPeerIO; p2pVMTunnel: TCommunicationFrameworkWithP2PVM);
    procedure p2pVMTunnelClose(Sender: TPeerIO; p2pVMTunnel: TCommunicationFrameworkWithP2PVM);
  end;

  IOnBigStreamInterface = interface
    procedure BeginStream(Sender: TPeerIO; Total: Int64);
    procedure Process(Sender: TPeerIO; Total, current: Int64);
    procedure EndStream(Sender: TPeerIO; Total: Int64);
  end;

  TCommunicationProtocol = (cpZServer, cpCustom);

  TProgressOnCommunicationFramework = procedure(Sender: TCommunicationFramework) of object;

  TCommunicationFrameworkWithP2PVM_Server = class;

  TAutomatedP2PVMServiceData = record
    Service: TCommunicationFrameworkWithP2PVM_Server;
  end;

  PAutomatedP2PVMServiceData = ^TAutomatedP2PVMServiceData;

  TAutomatedP2PVMServiceBind_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<PAutomatedP2PVMServiceData>;

  TAutomatedP2PVMServiceBind = class(TAutomatedP2PVMServiceBind_Decl)
  public
    procedure AddService(Service: TCommunicationFrameworkWithP2PVM_Server; IPV6: SystemString; Port: Word); overload;
    procedure AddService(Service: TCommunicationFrameworkWithP2PVM_Server); overload;
    procedure Clean;
  end;

  TCommunicationFrameworkWithP2PVM_Client = class;

  TAutomatedP2PVMClientData = record
    Client: TCommunicationFrameworkWithP2PVM_Client;
    IPV6: SystemString;
    Port: Word;
  end;

  PAutomatedP2PVMClientData = ^TAutomatedP2PVMClientData;

  TAutomatedP2PVMClientBind_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<PAutomatedP2PVMClientData>;

  TAutomatedP2PVMClientBind = class(TAutomatedP2PVMClientBind_Decl)
  public
    procedure AddClient(Client: TCommunicationFrameworkWithP2PVM_Client; IPV6: SystemString; Port: Word);
    procedure Clean;
  end;

  TOnAutomatedP2PVMClientConnectionDone_C = procedure(Sender: TCommunicationFramework; P_IO: TPeerIO);
  TOnAutomatedP2PVMClientConnectionDone_M = procedure(Sender: TCommunicationFramework; P_IO: TPeerIO) of object;
{$IFDEF FPC}
  TOnAutomatedP2PVMClientConnectionDone_P = procedure(Sender: TCommunicationFramework; P_IO: TPeerIO) is nested;
{$ELSE FPC}
  TOnAutomatedP2PVMClientConnectionDone_P = reference to procedure(Sender: TCommunicationFramework; P_IO: TPeerIO);
{$ENDIF FPC}

  TCommunicationFramework = class(TCoreClassInterfacedObject)
  protected
    FCritical: TCritical;
    FCommandList: THashObjectList;
    FPeerIO_HashPool: TUInt32HashObjectList;
    FIDSeed: Cardinal;
    FOnExecuteCommand: TPeerIOCMDNotify;
    FOnSendCommand: TPeerIOCMDNotify;
    FPeerIOUserDefineClass: TPeerIOUserDefineClass;
    FPeerIOUserSpecialClass: TPeerIOUserSpecialClass;
    FIdleTimeOut: TTimeTick;
    FSendDataCompressed: Boolean;
    FCompleteBufferCompressed: Boolean;
    FUsedParallelEncrypt: Boolean;
    FSyncOnResult: Boolean;
    FSyncOnCompleteBuffer: Boolean;
    FEnabledAtomicLockAndMultiThread: Boolean;
    FTimeOutKeepAlive: Boolean;
    FQuietMode: Boolean;
    FCipherSecurityArray: TCipherSecurityArray;
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
    FIOInterface: IIOInterface;
    FVMInterface: ICommunicationFrameworkVMInterface;
    FOnBigStreamInterface: IOnBigStreamInterface;
    FProtocol: TCommunicationProtocol;
    FSequencePacketActivted: Boolean;
    FPrefixName: SystemString;
    FName: SystemString;
    FInitedTimeMD5: TMD5;
    FCustomUserData: Pointer;
    FCustomUserObject: TCoreClassObject;
  protected
    procedure DoPrint(const v: SystemString); virtual;
    procedure DoError(const v: SystemString); virtual;

    function GetIdleTimeOut: TTimeTick; virtual;
    procedure SetIdleTimeOut(const Value: TTimeTick); virtual;

    function CanExecuteCommand(Sender: TPeerIO; Cmd: SystemString): Boolean; virtual;
    function CanSendCommand(Sender: TPeerIO; Cmd: SystemString): Boolean; virtual;
    function CanRegCommand(Sender: TCommunicationFramework; Cmd: SystemString): Boolean; virtual;

    procedure DelayClose(Sender: TNPostExecute);
    procedure DelayFree(Sender: TNPostExecute);
    procedure DelayExecuteOnResultState(Sender: TNPostExecute);
    procedure DelayExecuteOnCompleteBufferState(Sender: TNPostExecute);

    procedure IDLE_Trace_Execute(Sender: TNPostExecute);

    { make seed and return only ID }
    function MakeID: Cardinal;

    { user protocol support }
    procedure FillCustomBuffer(Sender: TPeerIO; const th: TCoreClassThread; const buffer: PByte; const Size: NativeInt; var FillDone: Boolean); virtual;
  protected
    { event }
    procedure Framework_InternalSendByteBuffer(const Sender: TPeerIO; const buff: PByte; siz: NativeInt);
    procedure Framework_InternalSaveReceiveBuffer(const Sender: TPeerIO; const buff: Pointer; siz: Int64);
    procedure Framework_InternalProcessReceiveBuffer(const Sender: TPeerIO; const CurrentActiveThread_: TCoreClassThread; const RecvSync, SendSync: Boolean);
    procedure Framework_InternalProcessAllSendCmd(const Sender: TPeerIO; const CurrentActiveThread_: TCoreClassThread; const RecvSync, SendSync: Boolean);
    procedure Framework_InternalIOCreate(const Sender: TPeerIO); virtual;
    procedure Framework_InternalIODestroy(const Sender: TPeerIO); virtual;

    { private vm and protocol stack support }
    procedure BuildP2PAuthTokenResult_OnIOIDLE(Sender: TCoreClassObject);
    procedure CommandResult_BuildP2PAuthToken(Sender: TPeerIO; ResultData: TDataFrameEngine);
    procedure Command_BuildP2PAuthToken(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
    procedure Command_InitP2PTunnel(Sender: TPeerIO; InData: SystemString);
    procedure Command_CloseP2PTunnel(Sender: TPeerIO; InData: SystemString);

    procedure VMAuthSuccessAfterDelayExecute(Sender: TNPostExecute);
    procedure VMAuthSuccessDelayExecute(Sender: TNPostExecute);
    procedure VMAuthFailedDelayExecute(Sender: TNPostExecute);
  protected
    { automated p2pVM }
    FAutomatedP2PVMService: Boolean;
    FAutomatedP2PVMServiceBind: TAutomatedP2PVMServiceBind;
    FAutomatedP2PVMClient: Boolean;
    FAutomatedP2PVMClientBind: TAutomatedP2PVMClientBind;
    FAutomatedP2PVMClientDelayBoot: double;
    FAutomatedP2PVMAuthToken: SystemString;
    FOnAutomatedP2PVMClientConnectionDone_C: TOnAutomatedP2PVMClientConnectionDone_C;
    FOnAutomatedP2PVMClientConnectionDone_M: TOnAutomatedP2PVMClientConnectionDone_M;
    FOnAutomatedP2PVMClientConnectionDone_P: TOnAutomatedP2PVMClientConnectionDone_P;

    procedure InitAutomatedP2PVM;
    procedure FreeAutomatedP2PVM;
    procedure DoAutomatedP2PVMClient_DelayRequest(Sender: TNPostExecute);
    procedure DoAutomatedP2PVMClient_Request(IO_ID: Cardinal);
    procedure AutomatedP2PVMClient_BuildP2PAuthTokenResult(P_IO: TPeerIO);
    procedure AutomatedP2PVMClient_OpenP2PVMTunnelResult(P_IO: TPeerIO; VMauthState: Boolean);
    procedure AutomatedP2PVMClient_ConnectionResult(Param1: Pointer; Param2: TObject; const ConnectionState: Boolean);
    procedure AutomatedP2PVMClient_Done(P_IO: TPeerIO);
  protected
    { large-scale IO support }
    FLargeScaleIOPool: TIO_Array;
    FProgressMaxDelay: TTimeTick;
    procedure InitLargeScaleIOPool;
    procedure FreeLargeScaleIOPool;
    procedure ProgressLargeScaleIOPool;
  public
    Statistics: array [TStatisticsType] of Int64;
    CmdRecvStatistics: THashVariantList;
    CmdSendStatistics: THashVariantList;
    CmdMaxExecuteConsumeStatistics: THashVariantList;
  public
    constructor Create(HashPoolSize: Integer);
    procedure CreateAfter; virtual;
    destructor Destroy; override;

    property SequencePacketActivted: Boolean read FSequencePacketActivted write FSequencePacketActivted; { default set True }
    { user protocol support }
    property Protocol: TCommunicationProtocol read FProtocol write FProtocol;
    procedure WriteCustomBuffer(P_IO: TPeerIO; const buffer: PByte; const Size: NativeInt);

    property PrefixName: SystemString read FPrefixName write FPrefixName;
    property Name: SystemString read FName write FName;

    { IO backcall interface }
    property IOInterface: IIOInterface read FIOInterface write FIOInterface;

    { p2pVM backcall interface }
    property VMInterface: ICommunicationFrameworkVMInterface read FVMInterface write FVMInterface;
    property OnVMInterface: ICommunicationFrameworkVMInterface read FVMInterface write FVMInterface;
    property OnVM: ICommunicationFrameworkVMInterface read FVMInterface write FVMInterface;
    { p2pVM trigger }
    procedure p2pVMTunnelAuth(Sender: TPeerIO; const Token: SystemString; var Accept: Boolean); virtual;
    procedure p2pVMTunnelOpenBefore(Sender: TPeerIO; p2pVMTunnel: TCommunicationFrameworkWithP2PVM); virtual;
    procedure p2pVMTunnelOpen(Sender: TPeerIO; p2pVMTunnel: TCommunicationFrameworkWithP2PVM); virtual;
    procedure p2pVMTunnelOpenAfter(Sender: TPeerIO; p2pVMTunnel: TCommunicationFrameworkWithP2PVM); virtual;
    procedure p2pVMTunnelClose(Sender: TPeerIO; p2pVMTunnel: TCommunicationFrameworkWithP2PVM); virtual;

    { automated P2PVM service support }
    property AutomatedP2PVMService: Boolean read FAutomatedP2PVMService write FAutomatedP2PVMService;
    property AutomatedP2PVMServiceBind: TAutomatedP2PVMServiceBind read FAutomatedP2PVMServiceBind;
    property AutomatedP2PVMBindService: TAutomatedP2PVMServiceBind read FAutomatedP2PVMServiceBind;

    { automated P2PVM client state }
    property AutomatedP2PVMClient: Boolean read FAutomatedP2PVMClient write FAutomatedP2PVMClient;
    property AutomatedP2PVMClientBind: TAutomatedP2PVMClientBind read FAutomatedP2PVMClientBind;
    property AutomatedP2PVMBindClient: TAutomatedP2PVMClientBind read FAutomatedP2PVMClientBind;
    property AutomatedP2PVMClientDelayBoot: double read FAutomatedP2PVMClientDelayBoot write FAutomatedP2PVMClientDelayBoot;
    property AutomatedP2PVMAuthToken: SystemString read FAutomatedP2PVMAuthToken write FAutomatedP2PVMAuthToken;
    property OnAutomatedP2PVMClientConnectionDone_C: TOnAutomatedP2PVMClientConnectionDone_C read FOnAutomatedP2PVMClientConnectionDone_C write FOnAutomatedP2PVMClientConnectionDone_C;
    property OnAutomatedP2PVMClientConnectionDone_M: TOnAutomatedP2PVMClientConnectionDone_M read FOnAutomatedP2PVMClientConnectionDone_M write FOnAutomatedP2PVMClientConnectionDone_M;
    property OnAutomatedP2PVMClientConnectionDone_P: TOnAutomatedP2PVMClientConnectionDone_P read FOnAutomatedP2PVMClientConnectionDone_P write FOnAutomatedP2PVMClientConnectionDone_P;
    { automated P2PVM client api }
    function AutomatedP2PVMClientConnectionDone(P_IO: TPeerIO): Boolean;
    procedure AutomatedP2PVM_Open(P_IO: TPeerIO);
    procedure AutomatedP2PVM_Open_C(P_IO: TPeerIO; OnResult: TIOStateCall);
    procedure AutomatedP2PVM_Open_M(P_IO: TPeerIO; OnResult: TIOStateMethod);
    procedure AutomatedP2PVM_Open_P(P_IO: TPeerIO; OnResult: TIOStateProc);
    procedure AutomatedP2PVM_Close(P_IO: TPeerIO);

    { IO Big Stream interface }
    property OnBigStreamInterface: IOnBigStreamInterface read FOnBigStreamInterface write FOnBigStreamInterface;
    property OnBigStream: IOnBigStreamInterface read FOnBigStreamInterface write FOnBigStreamInterface;

    { Security support }
    procedure SwitchMaxPerformance; virtual;
    procedure SwitchMaxSecurity; virtual;
    procedure SwitchDefaultPerformance; virtual;

    { atomic lock }
    procedure Lock_All_IO; virtual;
    procedure UnLock_All_IO; virtual;

    { delay run support }
    property ProgressEngine: TNProgressPostWithCadencer read FPostProgress;
    property ProgressPost: TNProgressPostWithCadencer read FPostProgress;
    property PostProgress: TNProgressPostWithCadencer read FPostProgress;
    property PostRun: TNProgressPostWithCadencer read FPostProgress;
    property PostExecute: TNProgressPostWithCadencer read FPostProgress;

    { framework token }
    property FrameworkIsServer: Boolean read FFrameworkIsServer;
    property FrameworkIsClient: Boolean read FFrameworkIsClient;
    property FrameworkInfo: SystemString read FFrameworkInfo;

    function IOBusy: Boolean;

    { mainLoop }
    procedure Progress; virtual;
    property OnProgress: TProgressOnCommunicationFramework read FOnProgress write FOnProgress;

    { seealso filler all IO,safe works }
    procedure ProgressPeerIOC(OnBackcall: TPeerIOListCall); overload;
    procedure ProgressPeerIOM(OnBackcall: TPeerIOListMethod); overload;
    procedure ProgressPeerIOP(OnBackcall: TPeerIOListProc); overload;

    { seealso filler all IO,fast }
    procedure FastProgressPeerIOC(OnBackcall: TPeerIOListCall); overload;
    procedure FastProgressPeerIOM(OnBackcall: TPeerIOListMethod); overload;
    procedure FastProgressPeerIOP(OnBackcall: TPeerIOListProc); overload;

    { PeerIO id array }
    procedure GetIO_Array(out IO_Array: TIO_Array);

    { block progress }
    procedure ProgressWaitSend(P_IO: TPeerIO); virtual;

    { print }
    procedure Print(const v: SystemString);
    procedure PrintParam(v: SystemString; Args: SystemString);
    procedure Error(const v: SystemString);
    procedure ErrorParam(v: SystemString; Args: SystemString);

    { register command for server/client }
    function DeleteRegistedCMD(Cmd: SystemString): Boolean;
    function UnRegisted(Cmd: SystemString): Boolean;
    function RegisterConsole(Cmd: SystemString): TCommandConsole;
    function RegisterStream(Cmd: SystemString): TCommandStream;
    function RegisterDirectStream(Cmd: SystemString): TCommandDirectStream;
    function RegisterDirectConsole(Cmd: SystemString): TCommandDirectConsole;
    function RegisterBigStream(Cmd: SystemString): TCommandBigStream;
    function RegisterCompleteBuffer(Cmd: SystemString): TCommandCompleteBuffer;
    function ExistsRegistedCmd(Cmd: SystemString): Boolean;
    procedure PrintRegistedCMD; overload;
    procedure PrintRegistedCMD(prefix: SystemString; incl_internalCMD: Boolean); overload;
    procedure PrintRegistedCMD(prefix: SystemString); overload;

    { execute command on local }
    function ExecuteConsole(Sender: TPeerIO; Cmd: SystemString; const InData: SystemString; var OutData: SystemString): Boolean; virtual;
    function ExecuteStream(Sender: TPeerIO; Cmd: SystemString; InData, OutData: TDataFrameEngine): Boolean; virtual;
    function ExecuteDirectStream(Sender: TPeerIO; Cmd: SystemString; InData: TDataFrameEngine): Boolean; virtual;
    function ExecuteDirectConsole(Sender: TPeerIO; Cmd: SystemString; const InData: SystemString): Boolean; virtual;
    function ExecuteBigStream(Sender: TPeerIO; Cmd: SystemString; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64): Boolean; virtual;
    function ExecuteCompleteBuffer(Sender: TPeerIO; Cmd: SystemString; InData: PByte; DataSize: NativeInt): Boolean; virtual;

    { misc }
    function FirstIO: TPeerIO;
    function LastIO: TPeerIO;
    property OnExecuteCommand: TPeerIOCMDNotify read FOnExecuteCommand write FOnExecuteCommand;
    property OnSendCommand: TPeerIOCMDNotify read FOnSendCommand write FOnSendCommand;
    function ExistsID(IO_ID: Cardinal): Boolean;

    { p2p options }
    property UsedParallelEncrypt: Boolean read FUsedParallelEncrypt write FUsedParallelEncrypt;
    property SyncOnResult: Boolean read FSyncOnResult write FSyncOnResult;
    property SyncOnCompleteBuffer: Boolean read FSyncOnCompleteBuffer write FSyncOnCompleteBuffer;
    property EnabledAtomicLockAndMultiThread: Boolean read FEnabledAtomicLockAndMultiThread write FEnabledAtomicLockAndMultiThread;
    property TimeOutKeepAlive: Boolean read FTimeOutKeepAlive write FTimeOutKeepAlive;
    property QuietMode: Boolean read FQuietMode write FQuietMode;
    property CipherSecurityArray: TCipherSecurityArray read FCipherSecurityArray;
    function GetRandomCipherSecurity: TCipherSecurity;
    property RandomCipherSecurity: TCipherSecurity read GetRandomCipherSecurity;
    property IdleTimeOut: TTimeTick read GetIdleTimeOut write SetIdleTimeOut;
    property TimeOutIDLE: TTimeTick read GetIdleTimeOut write SetIdleTimeOut;
    property SendDataCompressed: Boolean read FSendDataCompressed write FSendDataCompressed;
    property CompleteBufferCompressed: Boolean read FCompleteBufferCompressed write FCompleteBufferCompressed;
    property HashSecurity: THashSecurity read FHashSecurity;
    property MaxCompleteBufferSize: Cardinal read FMaxCompleteBufferSize write FMaxCompleteBufferSize;
    { large-scale IO support }
    property ProgressMaxDelay: TTimeTick read FProgressMaxDelay write FProgressMaxDelay;

    { state }
    property CMDWithThreadRuning: Integer read FCMDWithThreadRuning;
    property InitedTimeMD5: TMD5 read FInitedTimeMD5;

    { user custom }
    property CustomUserData: Pointer read FCustomUserData write FCustomUserData;
    property CustomUserObject: TCoreClassObject read FCustomUserObject write FCustomUserObject;

    { hash pool }
    property PeerIO_HashPool: TUInt32HashObjectList read FPeerIO_HashPool;
    property IOPool: TUInt32HashObjectList read FPeerIO_HashPool;

    { custom struct: user custom instance one }
    property PeerClientUserDefineClass: TPeerIOUserDefineClass read FPeerIOUserDefineClass write FPeerIOUserDefineClass;
    property PeerIOUserDefineClass: TPeerIOUserDefineClass read FPeerIOUserDefineClass write FPeerIOUserDefineClass;
    property IOUserDefineClass: TPeerIOUserDefineClass read FPeerIOUserDefineClass write FPeerIOUserDefineClass;
    property IODefineClass: TPeerIOUserDefineClass read FPeerIOUserDefineClass write FPeerIOUserDefineClass;
    property UserDefineClass: TPeerIOUserDefineClass read FPeerIOUserDefineClass write FPeerIOUserDefineClass;
    property ExternalDefineClass: TPeerIOUserDefineClass read FPeerIOUserDefineClass write FPeerIOUserDefineClass;

    { custom special struct: user custom instance two }
    property PeerClientUserSpecialClass: TPeerIOUserSpecialClass read FPeerIOUserSpecialClass write FPeerIOUserSpecialClass;
    property PeerIOUserSpecialClass: TPeerIOUserSpecialClass read FPeerIOUserSpecialClass write FPeerIOUserSpecialClass;
    property IOUserSpecialClass: TPeerIOUserSpecialClass read FPeerIOUserSpecialClass write FPeerIOUserSpecialClass;
    property IOSpecialClass: TPeerIOUserSpecialClass read FPeerIOUserSpecialClass write FPeerIOUserSpecialClass;
    property UserSpecialClass: TPeerIOUserSpecialClass read FPeerIOUserSpecialClass write FPeerIOUserSpecialClass;
    property ExternalSpecialClass: TPeerIOUserSpecialClass read FPeerIOUserSpecialClass write FPeerIOUserSpecialClass;

    { misc }
    property IDCounter: Cardinal read FIDSeed write FIDSeed;
    property IDSeed: Cardinal read FIDSeed write FIDSeed;
    property PrintParams: THashVariantList read FPrintParams;
  end;

{$ENDREGION 'CommunicationFramework'}
{$REGION 'CommunicationFrameworkServer'}

  TOnServerCustomProtocolReceiveBufferNotify = procedure(Sender: TPeerIO; const buffer: PByte; const Size: NativeInt; var FillDone: Boolean) of object;

  TCommunicationFramework_StableServer = class;

  TCommunicationFrameworkServer = class(TCommunicationFramework)
  protected
    function CanExecuteCommand(Sender: TPeerIO; Cmd: SystemString): Boolean; override;
    function CanSendCommand(Sender: TPeerIO; Cmd: SystemString): Boolean; override;
    function CanRegCommand(Sender: TCommunicationFramework; Cmd: SystemString): Boolean; override;

    procedure Command_CipherModel(Sender: TPeerIO; InData, OutData: TDataFrameEngine); virtual;
    procedure Command_Wait(Sender: TPeerIO; InData: SystemString; var OutData: SystemString); virtual;

    procedure Framework_InternalIOCreate(const Sender: TPeerIO); override;
    procedure Framework_InternalIODestroy(const Sender: TPeerIO); override;
  protected
    FOnServerCustomProtocolReceiveBufferNotify: TOnServerCustomProtocolReceiveBufferNotify;
    FillSync_Sender: TPeerIO;
    FillSync_Buffer: PByte;
    FillSync_BufferSize: NativeInt;
    FillSync_Done: Boolean;
    procedure SyncFillCustomBuffer;
    procedure FillCustomBuffer(Sender: TPeerIO; const th: TCoreClassThread; const buffer: PByte; const Size: NativeInt; var FillDone: Boolean); override;
  protected
    FStableIOProgressing: Boolean;
    FStableIO: TCommunicationFramework_StableServer;
  public
    constructor Create; virtual;
    constructor CreateCustomHashPool(HashPoolSize: Integer); virtual;
    destructor Destroy; override;

    { mainLoop }
    procedure Progress; override;

    { stable IO }
    function StableIO: TCommunicationFramework_StableServer;

    { disconnect client on ID }
    procedure Disconnect(ID: Cardinal); overload;
    procedure Disconnect(ID: Cardinal; delay: double); overload;

    { OnReceiveBuffer work on Protocol is cpCustom }
    procedure OnReceiveBuffer(Sender: TPeerIO; const buffer: PByte; const Size: NativeInt; var FillDone: Boolean); virtual;
    procedure WriteBuffer(P_IO: TPeerIO; const buffer: PByte; const Size: NativeInt);

    { external service method }
    procedure StopService; virtual;
    function StartService(Host: SystemString; Port: Word): Boolean; virtual;
    procedure TriggerQueueData(v: PQueueData); virtual;

    { service framework support }
    procedure DoIOConnectBefore(Sender: TPeerIO); virtual;
    procedure DoIOConnectAfter(Sender: TPeerIO); virtual;
    procedure DoIODisconnect(Sender: TPeerIO); virtual;

    { send console cmd method }
    procedure SendConsoleCmdM(P_IO: TPeerIO; const Cmd, ConsoleData: SystemString; OnResult: TConsoleMethod); overload;
    procedure SendConsoleCmdM(P_IO: TPeerIO; const Cmd, ConsoleData: SystemString; Param1: Pointer; Param2: TObject; OnResult: TConsoleParamMethod); overload;
    procedure SendConsoleCmdM(P_IO: TPeerIO; const Cmd, ConsoleData: SystemString; Param1: Pointer; Param2: TObject; OnResult: TConsoleParamMethod; OnFailed: TConsoleFailedMethod); overload;

    { send stream cmd method }
    procedure SendStreamCmdM(P_IO: TPeerIO; const Cmd: SystemString; StreamData: TMemoryStream64; OnResult: TStreamMethod; DoneAutoFree: Boolean); overload;
    procedure SendStreamCmdM(P_IO: TPeerIO; const Cmd: SystemString; StreamData: TDataFrameEngine; OnResult: TStreamMethod); overload;
    procedure SendStreamCmdM(P_IO: TPeerIO; const Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamMethod); overload;
    procedure SendStreamCmdM(P_IO: TPeerIO; const Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamMethod; OnFailed: TStreamFailedMethod); overload;

    { send console cmd proc }
    procedure SendConsoleCmdP(P_IO: TPeerIO; const Cmd, ConsoleData: SystemString; OnResult: TConsoleProc); overload;
    procedure SendConsoleCmdP(P_IO: TPeerIO; const Cmd, ConsoleData: SystemString; Param1: Pointer; Param2: TObject; OnResult: TConsoleParamProc); overload;
    procedure SendConsoleCmdP(P_IO: TPeerIO; const Cmd, ConsoleData: SystemString; Param1: Pointer; Param2: TObject; OnResult: TConsoleParamProc; OnFailed: TConsoleFailedProc); overload;

    { send stream cmd proc }
    procedure SendStreamCmdP(P_IO: TPeerIO; const Cmd: SystemString; StreamData: TMemoryStream64; OnResult: TStreamProc; DoneAutoFree: Boolean); overload;
    procedure SendStreamCmdP(P_IO: TPeerIO; const Cmd: SystemString; StreamData: TDataFrameEngine; OnResult: TStreamProc); overload;
    procedure SendStreamCmdP(P_IO: TPeerIO; const Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamProc); overload;
    procedure SendStreamCmdP(P_IO: TPeerIO; const Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamProc; OnFailed: TStreamFailedProc); overload;

    { send direct cmd }
    procedure SendDirectConsoleCmd(P_IO: TPeerIO; const Cmd, ConsoleData: SystemString); overload;
    procedure SendDirectConsoleCmd(P_IO: TPeerIO; const Cmd: SystemString); overload;
    procedure SendDirectStreamCmd(P_IO: TPeerIO; const Cmd: SystemString; StreamData: TMemoryStream64; DoneAutoFree: Boolean); overload;
    procedure SendDirectStreamCmd(P_IO: TPeerIO; const Cmd: SystemString; StreamData: TDataFrameEngine); overload;
    procedure SendDirectStreamCmd(P_IO: TPeerIO; const Cmd: SystemString); overload;

    { wait send }
    function WaitSendConsoleCmd(P_IO: TPeerIO; const Cmd, ConsoleData: SystemString; Timeout: TTimeTick): SystemString; overload; virtual;
    procedure WaitSendStreamCmd(P_IO: TPeerIO; const Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; Timeout: TTimeTick); overload; virtual;

    { send bigstream }
    procedure SendBigStream(P_IO: TPeerIO; const Cmd: SystemString; BigStream: TCoreClassStream; StartPos: Int64; DoneAutoFree: Boolean); overload;
    procedure SendBigStream(P_IO: TPeerIO; const Cmd: SystemString; BigStream: TCoreClassStream; DoneAutoFree: Boolean); overload;

    { send complete buffer }
    procedure SendCompleteBuffer(P_IO: TPeerIO; const Cmd: SystemString; buff: PByte; BuffSize: NativeInt; DoneAutoFree: Boolean); overload;

    { send used IO bind ID ,return method }
    procedure SendConsoleCmdM(IO_ID: Cardinal; const Cmd, ConsoleData: SystemString; OnResult: TConsoleMethod); overload;
    procedure SendConsoleCmdM(IO_ID: Cardinal; const Cmd, ConsoleData: SystemString; Param1: Pointer; Param2: TObject; OnResult: TConsoleParamMethod); overload;
    procedure SendConsoleCmdM(IO_ID: Cardinal; const Cmd, ConsoleData: SystemString; Param1: Pointer; Param2: TObject; OnResult: TConsoleParamMethod; OnFailed: TConsoleFailedMethod); overload;

    procedure SendStreamCmdM(IO_ID: Cardinal; const Cmd: SystemString; StreamData: TMemoryStream64; OnResult: TStreamMethod; DoneAutoFree: Boolean); overload;
    procedure SendStreamCmdM(IO_ID: Cardinal; const Cmd: SystemString; StreamData: TDataFrameEngine; OnResult: TStreamMethod); overload;
    procedure SendStreamCmdM(IO_ID: Cardinal; const Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamMethod); overload;
    procedure SendStreamCmdM(IO_ID: Cardinal; const Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamMethod; OnFailed: TStreamFailedMethod); overload;

    { send used IO bind ID,return proc }
    procedure SendConsoleCmdP(IO_ID: Cardinal; const Cmd, ConsoleData: SystemString; OnResult: TConsoleProc); overload;
    procedure SendConsoleCmdP(IO_ID: Cardinal; const Cmd, ConsoleData: SystemString; Param1: Pointer; Param2: TObject; OnResult: TConsoleParamProc); overload;
    procedure SendConsoleCmdP(IO_ID: Cardinal; const Cmd, ConsoleData: SystemString; Param1: Pointer; Param2: TObject; OnResult: TConsoleParamProc; OnFailed: TConsoleFailedProc); overload;

    procedure SendStreamCmdP(IO_ID: Cardinal; const Cmd: SystemString; StreamData: TMemoryStream64; OnResult: TStreamProc; DoneAutoFree: Boolean); overload;
    procedure SendStreamCmdP(IO_ID: Cardinal; const Cmd: SystemString; StreamData: TDataFrameEngine; OnResult: TStreamProc); overload;
    procedure SendStreamCmdP(IO_ID: Cardinal; const Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamProc); overload;
    procedure SendStreamCmdP(IO_ID: Cardinal; const Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamProc; OnFailed: TStreamFailedProc); overload;

    { direct send used IO BIND ID }
    procedure SendDirectConsoleCmd(IO_ID: Cardinal; const Cmd, ConsoleData: SystemString); overload;
    procedure SendDirectConsoleCmd(IO_ID: Cardinal; const Cmd: SystemString); overload;
    procedure SendDirectStreamCmd(IO_ID: Cardinal; const Cmd: SystemString; StreamData: TMemoryStream64; DoneAutoFree: Boolean); overload;
    procedure SendDirectStreamCmd(IO_ID: Cardinal; const Cmd: SystemString; StreamData: TDataFrameEngine); overload;
    procedure SendDirectStreamCmd(IO_ID: Cardinal; const Cmd: SystemString); overload;

    { wait send }
    function WaitSendConsoleCmd(IO_ID: Cardinal; const Cmd, ConsoleData: SystemString; Timeout: TTimeTick): SystemString; overload;
    procedure WaitSendStreamCmd(IO_ID: Cardinal; const Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; Timeout: TTimeTick); overload;

    { send bigstream }
    procedure SendBigStream(IO_ID: Cardinal; const Cmd: SystemString; BigStream: TCoreClassStream; StartPos: Int64; DoneAutoFree: Boolean); overload;
    procedure SendBigStream(IO_ID: Cardinal; const Cmd: SystemString; BigStream: TCoreClassStream; DoneAutoFree: Boolean); overload;

    { send complete buffer }
    procedure SendCompleteBuffer(IO_ID: Cardinal; const Cmd: SystemString; buff: PByte; BuffSize: NativeInt; DoneAutoFree: Boolean); overload;

    { Broadcast to all IO }
    procedure BroadcastDirectConsoleCmd(Cmd: SystemString; ConsoleData: SystemString);
    procedure BroadcastDirectStreamCmd(Cmd: SystemString; StreamData: TDataFrameEngine);

    function GetCount: Integer;
    property Count: Integer read GetCount;

    function Exists(P_IO: TCoreClassObject): Boolean; overload;
    function Exists(P_IO: TPeerIO): Boolean; overload;
    function Exists(P_IO: TPeerIOUserDefine): Boolean; overload;
    function Exists(P_IO: TPeerIOUserSpecial): Boolean; overload;
    function Exists(IO_ID: Cardinal): Boolean; overload;

    function GetPeerIO(ID: Cardinal): TPeerIO;
    property IO[ID: Cardinal]: TPeerIO read GetPeerIO; default;
    property PeerIO[ID: Cardinal]: TPeerIO read GetPeerIO;
  end;

  TCommunicationFrameworkServerClass = class of TCommunicationFrameworkServer;
{$ENDREGION 'CommunicationFrameworkServer'}
{$REGION 'CommunicationFrameworkClient'}
  TCommunicationFrameworkClient = class;

  ICommunicationFrameworkClientInterface = interface
    procedure ClientConnected(Sender: TCommunicationFrameworkClient);
    procedure ClientDisconnect(Sender: TCommunicationFrameworkClient);
  end;

  TOnClientCustomProtocolReceiveBufferNotify = procedure(Sender: TCommunicationFrameworkClient; const buffer: PByte; const Size: NativeInt; var FillDone: Boolean) of object;
  TOnCipherModelDone = procedure(Sender: TCommunicationFrameworkClient) of object;

  TCommunicationFramework_StableClient = class;

  TCommunicationFrameworkClient = class(TCommunicationFramework)
  protected
    FOnInterface: ICommunicationFrameworkClientInterface;

    FConnectInitWaiting: Boolean;
    FConnectInitWaitingTimeout: TTimeTick;
    FAsyncConnectTimeout: TTimeTick;
    FOnCipherModelDone: TOnCipherModelDone;

    FIgnoreProcessConnectedAndDisconnect: Boolean;
    FLastConnectIsSuccessed: Boolean;

    procedure StreamResult_CipherModel(Sender: TPeerIO; ResultData: TDataFrameEngine);

    procedure DoConnected(Sender: TPeerIO); virtual;
    procedure DoDisconnect(Sender: TPeerIO); virtual;

    function CanExecuteCommand(Sender: TPeerIO; Cmd: SystemString): Boolean; override;
    function CanSendCommand(Sender: TPeerIO; Cmd: SystemString): Boolean; override;
    function CanRegCommand(Sender: TCommunicationFramework; Cmd: SystemString): Boolean; override;
  protected
    FOnClientCustomProtocolReceiveBufferNotify: TOnClientCustomProtocolReceiveBufferNotify;
    FillSync_Buffer: PByte;
    FillSync_BufferSize: NativeInt;
    FillSync_Done: Boolean;
    procedure SyncFillCustomBuffer;
    procedure FillCustomBuffer(Sender: TPeerIO; const th: TCoreClassThread; const buffer: PByte; const Size: NativeInt; var FillDone: Boolean); override;
  protected
    FStableIOProgressing: Boolean;
    FStableIO: TCommunicationFramework_StableClient;
  protected
    { async wait support }
    FWaiting: Boolean;
    FWaitingTimeOut: TTimeTick;
    FOnWaitResultCall: TStateCall;
    FOnWaitResultMethod: TStateMethod;
    FOnWaitResultProc: TStateProc;
    procedure ConsoleResult_Wait(Sender: TPeerIO; ResultData: SystemString);
    function GetWaitTimeout(const t: TTimeTick): TTimeTick;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure IO_IDLE_TraceC(data: TCoreClassObject; OnNotify: TDataNotifyCall);
    procedure IO_IDLE_TraceM(data: TCoreClassObject; OnNotify: TDataNotifyMethod);
    procedure IO_IDLE_TraceP(data: TCoreClassObject; OnNotify: TDataNotifyProc);
    { OnReceiveBuffer work on Protocol is cpCustom }
    procedure OnReceiveBuffer(const buffer: PByte; const Size: NativeInt; var FillDone: Boolean); virtual;
    procedure WriteBuffer(const buffer: PByte; const Size: NativeInt);

    { mainLoop }
    procedure Progress; override;

    { stable IO }
    function StableIO: TCommunicationFramework_StableClient;

    { trigger io OnDisconnect event }
    procedure TriggerDoDisconnect;

    { external io,state }
    function Connected: Boolean; virtual;
    property LastConnectIsSuccessed: Boolean read FLastConnectIsSuccessed;
    { external io,intf }
    function ClientIO: TPeerIO; virtual;
    { external io,intf }
    procedure TriggerQueueData(v: PQueueData); virtual;

    { async connection }
    procedure TriggerDoConnectFailed; virtual;
    procedure TriggerDoConnectFinished; virtual;

    { cipher execute done }
    procedure CipherModelDone; virtual;
    property OnCipherModelDone: TOnCipherModelDone read FOnCipherModelDone write FOnCipherModelDone;

    property AsyncConnectTimeout: TTimeTick read FAsyncConnectTimeout write FAsyncConnectTimeout;

    { async connection }
    procedure AsyncConnectC(addr: SystemString; Port: Word; OnResult: TStateCall); overload; virtual;
    procedure AsyncConnectM(addr: SystemString; Port: Word; OnResult: TStateMethod); overload; virtual;
    procedure AsyncConnectP(addr: SystemString; Port: Word; OnResult: TStateProc); overload; virtual;
    procedure AsyncConnectC(addr: SystemString; Port: Word; Param1: Pointer; Param2: TObject; OnResult: TParamStateCall); overload;
    procedure AsyncConnectM(addr: SystemString; Port: Word; Param1: Pointer; Param2: TObject; OnResult: TParamStateMethod); overload;
    procedure AsyncConnectP(addr: SystemString; Port: Word; Param1: Pointer; Param2: TObject; OnResult: TParamStateProc); overload;

    { sync connecton }
    function Connect(addr: SystemString; Port: Word): Boolean; virtual;

    { disconnect }
    procedure Disconnect; virtual;
    { delay close on now }
    procedure DelayClose; overload;
    { delay close on custom delay of double time }
    procedure DelayClose(const t: double); overload;

    { sync wait reponse }
    function Wait(TimeOut_: TTimeTick): SystemString; overload;

    { async wait reponse }
    function WaitC(TimeOut_: TTimeTick; OnResult: TStateCall): Boolean; overload;
    function WaitM(TimeOut_: TTimeTick; OnResult: TStateMethod): Boolean; overload;
    function WaitP(TimeOut_: TTimeTick; OnResult: TStateProc): Boolean; overload;

    { command queue state }
    function WaitSendBusy: Boolean;
    function LastQueueData: PQueueData;
    function LastQueueCmd: SystemString;
    function QueueCmdCount: Integer;

    { send console cmd method }
    procedure SendConsoleCmdM(Cmd, ConsoleData: SystemString; OnResult: TConsoleMethod); overload;
    procedure SendConsoleCmdM(Cmd, ConsoleData: SystemString; Param1: Pointer; Param2: TObject; OnResult: TConsoleParamMethod); overload;
    procedure SendConsoleCmdM(Cmd, ConsoleData: SystemString; Param1: Pointer; Param2: TObject; OnResult: TConsoleParamMethod; OnFailed: TConsoleFailedMethod); overload;
    { send stream cmd method }
    procedure SendStreamCmdM(Cmd: SystemString; StreamData: TMemoryStream64; OnResult: TStreamMethod; DoneAutoFree: Boolean); overload;
    procedure SendStreamCmdM(Cmd: SystemString; StreamData: TDataFrameEngine; OnResult: TStreamMethod); overload;
    procedure SendStreamCmdM(Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamMethod); overload;
    procedure SendStreamCmdM(Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamMethod; OnFailed: TStreamFailedMethod); overload;

    { send console cmd proc }
    procedure SendConsoleCmdP(Cmd, ConsoleData: SystemString; OnResult: TConsoleProc); overload;
    procedure SendConsoleCmdP(Cmd, ConsoleData: SystemString; Param1: Pointer; Param2: TObject; OnResult: TConsoleParamProc); overload;
    procedure SendConsoleCmdP(Cmd, ConsoleData: SystemString; Param1: Pointer; Param2: TObject; OnResult: TConsoleParamProc; OnFailed: TConsoleFailedProc); overload;
    { send stream cmd proc }
    procedure SendStreamCmdP(Cmd: SystemString; StreamData: TMemoryStream64; OnResult: TStreamProc; DoneAutoFree: Boolean); overload;
    procedure SendStreamCmdP(Cmd: SystemString; StreamData: TDataFrameEngine; OnResult: TStreamProc); overload;
    procedure SendStreamCmdP(Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamProc); overload;
    procedure SendStreamCmdP(Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamProc; OnFailed: TStreamFailedProc); overload;

    { send direct cmd }
    procedure SendDirectConsoleCmd(Cmd: SystemString; ConsoleData: SystemString); overload;
    procedure SendDirectConsoleCmd(Cmd: SystemString); overload;
    procedure SendDirectStreamCmd(Cmd: SystemString; StreamData: TMemoryStream64; DoneAutoFree: Boolean); overload;
    procedure SendDirectStreamCmd(Cmd: SystemString; StreamData: TDataFrameEngine); overload;
    procedure SendDirectStreamCmd(Cmd: SystemString); overload;

    { wait send }
    function WaitSendConsoleCmd(Cmd, ConsoleData: SystemString; Timeout: TTimeTick): SystemString; virtual;
    procedure WaitSendStreamCmd(Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; Timeout: TTimeTick); virtual;

    { send bigstream }
    procedure SendBigStream(Cmd: SystemString; BigStream: TCoreClassStream; StartPos: Int64; DoneAutoFree: Boolean); overload;
    procedure SendBigStream(Cmd: SystemString; BigStream: TCoreClassStream; DoneAutoFree: Boolean); overload;

    { send complete buffer }
    procedure SendCompleteBuffer(Cmd: SystemString; buff: PByte; BuffSize: NativeInt; DoneAutoFree: Boolean); overload;

    property OnInterface: ICommunicationFrameworkClientInterface read FOnInterface write FOnInterface;
    property NotyifyInterface: ICommunicationFrameworkClientInterface read FOnInterface write FOnInterface;
    { remote service ID }
    { success ID > 0 }
    { failed! ID = 0 }
    function RemoteID: Cardinal;
    function RemoteKey: TCipherKeyBuffer;
    function RemoteInited: Boolean;
  end;

  TCommunicationFrameworkClientClass = class of TCommunicationFrameworkClient;
{$ENDREGION 'CommunicationFrameworkClient'}
{$REGION 'P2pVM'}
  Pp2pVMFragmentPacket = ^Tp2pVMFragmentPacket;

  Tp2pVMFragmentPacket = record
    buffSiz: Cardinal;
    FrameworkID: Cardinal;
    p2pID: Cardinal;
    pkType: Byte;
    buff: PByte;

    procedure Init;
    function FillReceiveBuff(Stream: TMemoryStream64): Integer;
    procedure BuildSendBuff(Stream: TMemoryStream64);
  end;

  TP2PVM_PeerIO = class(TPeerIO)
  protected
    FLinkVM: TCommunicationFrameworkWithP2PVM;
    FRealSendBuff: TMemoryStream64;
    FSendQueue: TCoreClassList;
    FRemote_frameworkID: Cardinal;
    FRemote_p2pID: Cardinal;
    FIP: TIPV6;
    FPort: Word;
    FDestroySyncRemote: Boolean;
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

  { p2p VM listen service }
  Pp2pVMListen = ^Tp2pVMListen;

  Tp2pVMListen = record
    FrameworkID: Cardinal;
    ListenHost: TIPV6;
    ListenPort: Word;
    Listening: Boolean;
  end;

  TCommunicationFrameworkWithP2PVM_Server = class(TCommunicationFrameworkServer)
  protected
    procedure Connecting(SenderVM: TCommunicationFrameworkWithP2PVM;
      const Remote_frameworkID, FrameworkID: Cardinal; const IPV6: TIPV6; const Port: Word; var Allowed: Boolean); virtual;
    procedure ListenState(SenderVM: TCommunicationFrameworkWithP2PVM; const IPV6: TIPV6; const Port: Word; const State: Boolean); virtual;
  protected
    FFrameworkListenPool: TCoreClassList;
    FLinkVMPool: TUInt32HashObjectList;
    FFrameworkWithVM_ID: Cardinal;

    procedure ProgressDisconnectClient(P_IO: TPeerIO);
    { internal Listen state }
    function ListenCount: Integer;
    function GetListen(const index: Integer): Pp2pVMListen;
    function FindListen(const IPV6: TIPV6; const Port: Word): Pp2pVMListen;
    function FindListening(const IPV6: TIPV6; const Port: Word): Pp2pVMListen;
    procedure DeleteListen(const IPV6: TIPV6; const Port: Word);
    procedure ClearListen;
  public
    constructor Create; override;
    constructor CustomCreate(HashPoolSize: Integer; FrameworkID: Cardinal);
    destructor Destroy; override;

    { mainLoop }
    procedure Progress; override;

    { intf }
    procedure TriggerQueueData(v: PQueueData); override;

    procedure CloseAllClient;

    { service method }
    procedure ProgressStopServiceWithPerVM(SenderVM: TCommunicationFrameworkWithP2PVM);
    procedure StopService; override;
    function StartService(Host: SystemString; Port: Word): Boolean; override;

    { sync }
    function WaitSendConsoleCmd(P_IO: TPeerIO; const Cmd, ConsoleData: SystemString; Timeout: TTimeTick): SystemString; override;
    procedure WaitSendStreamCmd(P_IO: TPeerIO; const Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; Timeout: TTimeTick); override;
  end;

  TCommunicationFrameworkWithP2PVM_Client = class(TCommunicationFrameworkClient)
  protected
    procedure Framework_InternalIOCreate(const Sender: TPeerIO); override;
    procedure Framework_InternalIODestroy(const Sender: TPeerIO); override;
    procedure VMConnectSuccessed(SenderVM: TCommunicationFrameworkWithP2PVM; Remote_frameworkID, Remote_p2pID, FrameworkID: Cardinal); virtual;
    procedure VMDisconnect(SenderVM: TCommunicationFrameworkWithP2PVM); virtual;
  protected
    FLinkVM: TCommunicationFrameworkWithP2PVM;
    FFrameworkWithVM_ID: Cardinal;
    FVMClientIO: TP2PVM_PeerIO;
    FVMConnected: Boolean;

    FOnAsyncConnectNotifyCall: TStateCall;
    FOnAsyncConnectNotifyMethod: TStateMethod;
    FOnAsyncConnectNotifyProc: TStateProc;
  public
    constructor Create; overload; override;
    constructor CustomCreate(FrameworkID: Cardinal); overload;
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
    procedure AsyncConnectP(addr: SystemString; Port: Word; OnResult: TStateProc); overload; override;
    function Connect(addr: SystemString; Port: Word): Boolean; override;
    procedure Disconnect; override;

    procedure ProgressWaitSend(P_IO: TPeerIO); override;

    property LinkVM: TCommunicationFrameworkWithP2PVM read FLinkVM;
    property FrameworkWithVM_ID: Cardinal read FFrameworkWithVM_ID;
    property VMClientIO: TP2PVM_PeerIO read FVMClientIO;
  end;

  TCommunicationFrameworkListCall = procedure(Sender: TCommunicationFramework);
  TCommunicationFrameworkListMethod = procedure(Sender: TCommunicationFramework) of object;
{$IFDEF FPC}
  TCommunicationFrameworkListProc = procedure(Sender: TCommunicationFramework) is nested;
{$ELSE FPC}
  TCommunicationFrameworkListProc = reference to procedure(Sender: TCommunicationFramework);
{$ENDIF FPC}
  TP2PVMAuthSuccessMethod = procedure(Sender: TCommunicationFrameworkWithP2PVM) of object;

  TCommunicationFrameworkWithP2PVM = class(TCoreClassObject)
  protected
    FCritical: TCritical;
    FPhysicsIO: TPeerIO;
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
  protected
    procedure Hook_SendByteBuffer(const Sender: TPeerIO; const buff: PByte; siz: NativeInt);
    procedure Hook_SaveReceiveBuffer(const Sender: TPeerIO; const buff: Pointer; siz: Int64);
    procedure SyncProcessReceiveBuff;
    procedure Hook_ProcessReceiveBuffer(const Sender: TPeerIO; const CurrentActiveThread_: TCoreClassThread; const RecvSync, SendSync: Boolean);
    procedure Hook_ClientDestroy(const Sender: TPeerIO);

    procedure SendVMBuffer(const buff: Pointer; const siz: NativeInt);

    procedure ReceivedEchoing(const FrameworkID, p2pID: Cardinal; const buff: PByte; const siz: Cardinal);
    procedure ReceivedEcho(const FrameworkID, p2pID: Cardinal; const buff: PByte; const siz: Cardinal);
    procedure ReceivedListen(const FrameworkID, p2pID: Cardinal; const buff: PByte; const siz: Cardinal);
    procedure ReceivedListenState(const FrameworkID, p2pID: Cardinal; const buff: PByte; const siz: Cardinal);
    procedure ReceivedConnecting(const FrameworkID, p2pID: Cardinal; const buff: PByte; const siz: Cardinal);
    procedure ReceivedConnectedReponse(const FrameworkID, p2pID: Cardinal; const buff: PByte; const siz: Cardinal);
    procedure ReceivedDisconnect(const FrameworkID, p2pID: Cardinal; const buff: PByte; const siz: Cardinal);
    procedure ReceivedLogicFragmentData(const FrameworkID, p2pID: Cardinal; const buff: PByte; const siz: Cardinal);
    procedure ReceivedPhysicsFragmentData(const FrameworkID, p2pID: Cardinal; const buff: PByte; const siz: Cardinal);

    procedure DoProcessPerClientFragmentSend(P_IO: TPeerIO);
    procedure DoPerClientClose(P_IO: TPeerIO);
  public
    constructor Create(HashPoolSize: Integer);
    destructor Destroy; override;

    { owner physical IO, maybe also virtual IO }
    property PhysicsIO: TPeerIO read FPhysicsIO;

    procedure Progress;

    procedure ProgressCommunicationFrameworkC(OnBackcall: TCommunicationFrameworkListCall); overload;
    procedure ProgressCommunicationFrameworkM(OnBackcall: TCommunicationFrameworkListMethod); overload;
    procedure ProgressCommunicationFrameworkP(OnBackcall: TCommunicationFrameworkListProc); overload;

    { p2p VM physics tunnel support }
    procedure OpenP2PVMTunnel(c: TPeerIO);
    procedure CloseP2PVMTunnel;

    { p2p VM logic CommunicationFramework support }
    procedure InstallLogicFramework(c: TCommunicationFramework);
    procedure UninstallLogicFramework(c: TCommunicationFramework);
    function CreateLogicClient: TCommunicationFrameworkWithP2PVM_Client;

    { p2p VM Peformance support }
    { MaxVMFragmentSize see also MTU }
    property MaxVMFragmentSize: Cardinal read FMaxVMFragmentSize write FMaxVMFragmentSize;
    property MaxRealBuffer: Cardinal read FMaxRealBuffer write FMaxRealBuffer;
    property QuietMode: Boolean read FQuietMode write FQuietMode;

    { p2p VM safe Support }
    procedure AuthWaiting;
    procedure AuthVM; overload;
    property WasAuthed: Boolean read FAuthed;
    procedure AuthSuccessed;

    { p2p VM echo support and keepalive }
    procedure echoing(const OnEchoPtr: POnEcho; Timeout: TTimeTick); overload;
    procedure echoingC(OnResult: TStateCall; Timeout: TTimeTick); overload;
    procedure echoingM(OnResult: TStateMethod; Timeout: TTimeTick); overload;
    procedure echoingP(OnResult: TStateProc; Timeout: TTimeTick); overload;
    procedure echoBuffer(const buff: Pointer; const siz: NativeInt);

    { p2p VM simulate with network listen }
    procedure SendListen(const FrameworkID: Cardinal; const IPV6: TIPV6; const Port: Word; const Listening: Boolean);
    procedure SendListenState(const FrameworkID: Cardinal; const IPV6: TIPV6; const Port: Word; const Listening: Boolean);

    { p2p VM simulate connecting }
    procedure SendConnecting(const Remote_frameworkID, FrameworkID, p2pID: Cardinal; const IPV6: TIPV6; const Port: Word);
    procedure SendConnectedReponse(const Remote_frameworkID, Remote_p2pID, FrameworkID, p2pID: Cardinal);
    procedure SendDisconnect(const Remote_frameworkID, Remote_p2pID: Cardinal);

    { p2p VM Listen Query }
    function ListenCount: Integer;
    function GetListen(const index: Integer): Pp2pVMListen;
    function FindListen(const IPV6: TIPV6; const Port: Word): Pp2pVMListen;
    function FindListening(const IPV6: TIPV6; const Port: Word): Pp2pVMListen;
    procedure DeleteListen(const IPV6: TIPV6; const Port: Word);
    procedure ClearListen;

    { p2p VM operaton }
    procedure CloseAllClientIO;
    procedure CloseAllServerIO;
  end;
{$ENDREGION 'P2pVM'}
{$REGION 'StableIO'}

  TStableServer_PeerIO = class;

  TStableServer_PhysicsIO_UserDefine = class(TPeerIOUserDefine)
  public
    BindStableIO: TStableServer_PeerIO;
    constructor Create(Owner_: TPeerIO); override;
    destructor Destroy; override;
  end;

  TStableServer_PeerIO = class(TPeerIO)
  public
    Activted: Boolean;
    DestroyRecyclePhysicsIO: Boolean;
    Connection_Token: Cardinal;
    InternalBindPhysicsIO: TPeerIO;
    OfflineTick: TTimeTick;
    property BindPhysicsIO: TPeerIO read InternalBindPhysicsIO write InternalBindPhysicsIO;

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
  end;

  TCommunicationFramework_CustomStableServer = class(TCommunicationFrameworkServer)
  protected
    Connection_Token_Counter: Cardinal;
    FPhysicsServer: TCommunicationFrameworkServer;
    FOfflineTimeout: TTimeTick;
    FLimitSequencePacketMemoryUsage: Int64;
    FAutoFreePhysicsServer: Boolean;
    FAutoProgressPhysicsServer: Boolean;
    CustomStableServerProgressing: Boolean;

    procedure ServerCustomProtocolReceiveBufferNotify(Sender: TPeerIO; const buffer: PByte; const Size: NativeInt; var FillDone: Boolean);
    procedure SetPhysicsServer(const Value: TCommunicationFrameworkServer);

    procedure cmd_BuildStableIO(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
    procedure cmd_OpenStableIO(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
    procedure cmd_CloseStableIO(Sender: TPeerIO; InData: SystemString);
  public
    constructor Create; override;
    destructor Destroy; override;

    property PhysicsServer: TCommunicationFrameworkServer read FPhysicsServer write SetPhysicsServer;
    property OfflineTimeout: TTimeTick read FOfflineTimeout write FOfflineTimeout;
    property LimitSequencePacketMemoryUsage: Int64 read FLimitSequencePacketMemoryUsage write FLimitSequencePacketMemoryUsage;
    property AutoFreePhysicsServer: Boolean read FAutoFreePhysicsServer write FAutoFreePhysicsServer;
    property AutoProgressPhysicsServer: Boolean read FAutoProgressPhysicsServer write FAutoProgressPhysicsServer;

    function StartService(Host: SystemString; Port: Word): Boolean; override;
    procedure StopService; override;

    procedure Progress; override;
    procedure TriggerQueueData(v: PQueueData); override;

    function WaitSendConsoleCmd(P_IO: TPeerIO; const Cmd, ConsoleData: SystemString; Timeout: TTimeTick): SystemString; override;
    procedure WaitSendStreamCmd(P_IO: TPeerIO; const Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; Timeout: TTimeTick); override;
  end;

  TCommunicationFramework_StableServer = class(TCommunicationFramework_CustomStableServer)
  end;

  TStableClient_PeerIO = class(TPeerIO)
  public
    Activted, WaitConnecting: Boolean;
    PhysicsIO_LastConnectTick: TTimeTick;
    Connection_Token: Cardinal;
    BindPhysicsIO: TPeerIO;

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
  end;

  TCommunicationFramework_CustomStableClient = class(TCommunicationFrameworkClient)
  protected
    FPhysicsClient: TCommunicationFrameworkClient;
    FStableClientIO: TStableClient_PeerIO;
    FConnection_Addr: SystemString;
    FConnection_Port: Word;
    FAutomatedConnection: Boolean;
    FLimitSequencePacketMemoryUsage: Int64;
    FAutoFreePhysicsClient: Boolean;
    FAutoProgressPhysicsClient: Boolean;
    CustomStableClientProgressing: Boolean;
    KeepAliveChecking: Boolean;
    SaveLastCommunicationTick_Received: TTimeTick;

    FOnAsyncConnectNotifyCall: TStateCall;
    FOnAsyncConnectNotifyMethod: TStateMethod;
    FOnAsyncConnectNotifyProc: TStateProc;
    procedure ClientCustomProtocolReceiveBufferNotify(Sender: TCommunicationFrameworkClient; const buffer: PByte; const Size: NativeInt; var FillDone: Boolean);
    procedure SetPhysicsClient(const Value: TCommunicationFrameworkClient);

    { connection }
    procedure BuildStableIO_Result(Sender: TPeerIO; ResultData: TDataFrameEngine);
    procedure AsyncConnectResult(const cState: Boolean);
    procedure PostConnection(Sender: TNPostExecute);

    { reconnection }
    procedure OpenStableIO_Result(Sender: TPeerIO; ResultData: TDataFrameEngine);
    procedure AsyncReconnectionResult(const cState: Boolean);
    procedure PostReconnection(Sender: TNPostExecute);
    procedure Reconnection;

    function GetStopCommunicationTimeTick: TTimeTick;
  public
    constructor Create; override;
    destructor Destroy; override;

    property PhysicsClient: TCommunicationFrameworkClient read FPhysicsClient write SetPhysicsClient;
    property LimitSequencePacketMemoryUsage: Int64 read FLimitSequencePacketMemoryUsage write FLimitSequencePacketMemoryUsage;
    property AutoFreePhysicsClient: Boolean read FAutoFreePhysicsClient write FAutoFreePhysicsClient;
    property AutoProgressPhysicsClient: Boolean read FAutoProgressPhysicsClient write FAutoProgressPhysicsClient;
    property AutomatedConnection: Boolean read FAutomatedConnection write FAutomatedConnection;
    property StopCommunicationTimeTick: TTimeTick read GetStopCommunicationTimeTick;
    property StableClientIO: TStableClient_PeerIO read FStableClientIO;

    procedure TriggerDoConnectFailed; override;
    procedure TriggerDoConnectFinished; override;

    procedure AsyncConnectC(addr: SystemString; Port: Word; OnResult: TStateCall); override;
    procedure AsyncConnectM(addr: SystemString; Port: Word; OnResult: TStateMethod); override;
    procedure AsyncConnectP(addr: SystemString; Port: Word; OnResult: TStateProc); override;
    function Connect(addr: SystemString; Port: Word): Boolean; override;

    function Connected: Boolean; override;
    procedure Disconnect; override;
    function ClientIO: TPeerIO; override;
    procedure TriggerQueueData(v: PQueueData); override;
    procedure Progress; override;
  end;

  TCommunicationFramework_StableClient = class(TCommunicationFramework_CustomStableClient)
  end;
{$ENDREGION 'StableIO'}
{$REGION 'HPC Stream Support'}


type
  THPC_Stream = class;

  TOnHPC_StreamCall = procedure(ThSender: THPC_Stream; ThInData, ThOutData: TDataFrameEngine);
  TOnHPC_StreamMethod = procedure(ThSender: THPC_Stream; ThInData, ThOutData: TDataFrameEngine) of object;
{$IFDEF FPC}
  TOnHPC_StreamProc = procedure(ThSender: THPC_Stream; ThInData, ThOutData: TDataFrameEngine) is nested;
{$ELSE FPC}
  TOnHPC_StreamProc = reference to procedure(ThSender: THPC_Stream; ThInData, ThOutData: TDataFrameEngine);
{$ENDIF FPC}

  THPC_Stream = class
  protected
    OnCall: TOnHPC_StreamCall;
    OnMethod: TOnHPC_StreamMethod;
    OnProc: TOnHPC_StreamProc;
    procedure Run(Sender: TCompute);
    procedure RunDone(Sender: TCompute);
  public
    Thread: TCompute;
    Framework: TCommunicationFramework;
    WorkID: Cardinal;
    UserData: Pointer;
    UserObject: TCoreClassObject;
    InData, OutData: TDataFrameEngine;
    constructor Create;
    destructor Destroy;
    function IsOnline: Boolean; { only work in mainthread }
  end;

procedure RunHPC_StreamC(Sender: TPeerIO;
  const UserData: Pointer; const UserObject: TCoreClassObject;
  const InData, OutData: TDataFrameEngine; const OnRun: TOnHPC_StreamCall);

procedure RunHPC_StreamM(Sender: TPeerIO;
  const UserData: Pointer; const UserObject: TCoreClassObject;
  const InData, OutData: TDataFrameEngine; const OnRun: TOnHPC_StreamMethod);

procedure RunHPC_StreamP(Sender: TPeerIO;
  const UserData: Pointer; const UserObject: TCoreClassObject;
  const InData, OutData: TDataFrameEngine; const OnRun: TOnHPC_StreamProc);
{$ENDREGION 'HPC Stream Support'}
{$REGION 'HPC DirectStream Support'}


type
  THPC_DirectStream = class;

  TOnHPC_DirectStreamCall = procedure(ThSender: THPC_DirectStream; ThInData: TDataFrameEngine);
  TOnHPC_DirectStreamMethod = procedure(ThSender: THPC_DirectStream; ThInData: TDataFrameEngine) of object;
{$IFDEF FPC}
  TOnHPC_DirectStreamProc = procedure(ThSender: THPC_DirectStream; ThInData: TDataFrameEngine) is nested;
{$ELSE FPC}
  TOnHPC_DirectStreamProc = reference to procedure(ThSender: THPC_DirectStream; ThInData: TDataFrameEngine);
{$ENDIF FPC}

  THPC_DirectStream = class
  protected
    OnCall: TOnHPC_DirectStreamCall;
    OnMethod: TOnHPC_DirectStreamMethod;
    OnProc: TOnHPC_DirectStreamProc;
    procedure Run(Sender: TCompute);
  public
    Thread: TCompute;
    Framework: TCommunicationFramework;
    WorkID: Cardinal;
    UserData: Pointer;
    UserObject: TCoreClassObject;
    InData: TDataFrameEngine;
    constructor Create;
    destructor Destroy;
    function IsOnline: Boolean; { only work in mainthread }
  end;

procedure RunHPC_DirectStreamC(Sender: TPeerIO;
  const UserData: Pointer; const UserObject: TCoreClassObject;
  const InData: TDataFrameEngine; const OnRun: TOnHPC_DirectStreamCall);

procedure RunHPC_DirectStreamM(Sender: TPeerIO;
  const UserData: Pointer; const UserObject: TCoreClassObject;
  const InData: TDataFrameEngine; const OnRun: TOnHPC_DirectStreamMethod);

procedure RunHPC_DirectStreamP(Sender: TPeerIO;
  const UserData: Pointer; const UserObject: TCoreClassObject;
  const InData: TDataFrameEngine; const OnRun: TOnHPC_DirectStreamProc);
{$ENDREGION 'HPC DirectStream Support'}
{$REGION 'HPC Console Support'}


type
  THPC_Console = class;

  TOnHPC_ConsoleCall = procedure(ThSender: THPC_Console; ThInData, ThOutData: SystemString);
  TOnHPC_ConsoleMethod = procedure(ThSender: THPC_Console; ThInData, ThOutData: SystemString) of object;
{$IFDEF FPC}
  TOnHPC_ConsoleProc = procedure(ThSender: THPC_Console; ThInData, ThOutData: SystemString) is nested;
{$ELSE FPC}
  TOnHPC_ConsoleProc = reference to procedure(ThSender: THPC_Console; ThInData, ThOutData: SystemString);
{$ENDIF FPC}

  THPC_Console = class
  protected
    OnCall: TOnHPC_ConsoleCall;
    OnMethod: TOnHPC_ConsoleMethod;
    OnProc: TOnHPC_ConsoleProc;
    procedure Run(Sender: TCompute);
    procedure RunDone(Sender: TCompute);
  public
    Thread: TCompute;
    Framework: TCommunicationFramework;
    WorkID: Cardinal;
    UserData: Pointer;
    UserObject: TCoreClassObject;
    InData, OutData: SystemString;
    constructor Create;
    destructor Destroy;
    function IsOnline: Boolean; { only work in mainthread }
  end;

procedure RunHPC_ConsoleC(Sender: TPeerIO;
  const UserData: Pointer; const UserObject: TCoreClassObject;
  const InData, OutData: SystemString; const OnRun: TOnHPC_ConsoleCall);

procedure RunHPC_ConsoleM(Sender: TPeerIO;
  const UserData: Pointer; const UserObject: TCoreClassObject;
  const InData, OutData: SystemString; const OnRun: TOnHPC_ConsoleMethod);

procedure RunHPC_ConsoleP(Sender: TPeerIO;
  const UserData: Pointer; const UserObject: TCoreClassObject;
  const InData, OutData: SystemString; const OnRun: TOnHPC_ConsoleProc);
{$ENDREGION 'HPC Console Support'}
{$REGION 'HPC DirectConsole Support'}


type
  THPC_DirectConsole = class;

  TOnHPC_DirectConsoleCall = procedure(ThSender: THPC_DirectConsole; ThInData: SystemString);
  TOnHPC_DirectConsoleMethod = procedure(ThSender: THPC_DirectConsole; ThInData: SystemString) of object;
{$IFDEF FPC}
  TOnHPC_DirectConsoleProc = procedure(ThSender: THPC_DirectConsole; ThInData: SystemString) is nested;
{$ELSE FPC}
  TOnHPC_DirectConsoleProc = reference to procedure(ThSender: THPC_DirectConsole; ThInData: SystemString);
{$ENDIF FPC}

  THPC_DirectConsole = class
  protected
    OnCall: TOnHPC_DirectConsoleCall;
    OnMethod: TOnHPC_DirectConsoleMethod;
    OnProc: TOnHPC_DirectConsoleProc;
    procedure Run(Sender: TCompute);
  public
    Thread: TCompute;
    Framework: TCommunicationFramework;
    WorkID: Cardinal;
    UserData: Pointer;
    UserObject: TCoreClassObject;
    InData: SystemString;
    constructor Create;
    destructor Destroy;
    function IsOnline: Boolean; { only work in mainthread }
  end;

procedure RunHPC_DirectConsoleC(Sender: TPeerIO;
  const UserData: Pointer; const UserObject: TCoreClassObject;
  const InData: SystemString; const OnRun: TOnHPC_DirectConsoleCall);

procedure RunHPC_DirectConsoleM(Sender: TPeerIO;
  const UserData: Pointer; const UserObject: TCoreClassObject;
  const InData: SystemString; const OnRun: TOnHPC_DirectConsoleMethod);

procedure RunHPC_DirectConsoleP(Sender: TPeerIO;
  const UserData: Pointer; const UserObject: TCoreClassObject;
  const InData: SystemString; const OnRun: TOnHPC_DirectConsoleProc);
{$ENDREGION 'HPC DirectConsole Support'}
{$REGION 'function'}

procedure DisposeQueueData(const v: PQueueData);
procedure InitQueueData(var v: TQueueData);
function NewQueueData: PQueueData;

function BuildP2PVMPacket(buffSiz, FrameworkID, p2pID: Cardinal; pkType: Byte; buff: PByte): Pp2pVMFragmentPacket;
procedure FreeP2PVMPacket(p: Pp2pVMFragmentPacket);

function IsSystemCMD(const Cmd: U_String): Boolean;

function StrToIPv4(const s: U_String; var Success: Boolean): TIPV4;
function IPv4ToStr(const IPv4Addr_: TIPV4): U_String;
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
{$ENDREGION 'function'}
{$REGION 'ConstAndVariant'}


var
  { sequence packet model }
  C_Sequence_Packet_HeadSize: Byte = $16;
  C_Sequence_QuietPacket: Byte = $01;
  C_Sequence_Packet: Byte = $02;
  C_Sequence_EchoPacket: Byte = $03;
  C_Sequence_KeepAlive: Byte = $04;
  C_Sequence_EchoKeepAlive: Byte = $05;
  C_Sequence_RequestResend: Byte = $06;

  { p2pVM }
  C_p2pVM_echoing: Byte = $01;
  C_p2pVM_echo: Byte = $02;
  C_p2pVM_AuthSuccessed: Byte = $09;
  C_p2pVM_Listen: Byte = $10;
  C_p2pVM_ListenState: Byte = $11;
  C_p2pVM_Connecting: Byte = $20;
  C_p2pVM_ConnectedReponse: Byte = $21;
  C_p2pVM_Disconnect: Byte = $40;
  C_p2pVM_LogicFragmentData: Byte = $54;
  C_p2pVM_PhysicsFragmentData: Byte = $64;

  { default system head }
  C_DefaultConsoleToken: Byte = $F1;
  C_DefaultStreamToken: Byte = $2F;
  C_DefaultDirectConsoleToken: Byte = $F3;
  C_DefaultDirectStreamToken: Byte = $4F;
  C_DefaultBigStreamToken: Byte = $F5;
  C_DefaultBigStreamReceiveFragmentSignal: Byte = $F6;
  C_DefaultBigStreamReceiveDoneSignal: Byte = $F7;
  C_DefaultCompleteBufferToken: Byte = $6F;

  { user custom header verify token }
  C_DataHeadToken: Cardinal = $F0F0F0F0;
  { user custom tail verify token }
  C_DataTailToken: Cardinal = $F1F1F1F1;

  { send flush buffer }
  C_SendFlushSize: NativeInt = 16 * 1024; { flush size = 16k byte }

  { max complete buffer }
  C_MaxCompleteBufferSize: NativeInt = 64 * 1024 * 1024; { 64M, 0 = infinity }

  { sequence packet model Packet MTU }
  C_SequencePacketMTU: Word = {$IFDEF Delphi}1536{$ELSE Delphi}20000{$ENDIF Delphi};

  { P2PVM Fragment size }
  C_P2PVM_MaxVMFragmentSize: Cardinal = {$IFDEF Delphi}8192{$ELSE Delphi}20000{$ENDIF Delphi};

  { P2PVM Max Real buffer }
  C_P2PVM_MaxRealBuffer: Cardinal = 2048 * 1024;

  { DoStatus ID }
  C_DoStatusID: Integer = $0FFFFFFF;

  { vm auth token size }
  C_VMAuthSize: Integer = 256;

  { BigStream fragment size }
  C_BigStream_ChunkSize: NativeInt = 1024 * 1024;

  { global progress backcall }
  ProgressBackgroundProc: TProgressBackgroundProc = nil;
  ProgressBackgroundMethod: TProgressBackgroundMethod = nil;

  { system }
  C_CipherModel: SystemString = '__@CipherModel';
  C_Wait: SystemString = '__@Wait';

  { P2PVM }
  C_BuildP2PAuthToken: SystemString = '__@BuildP2PAuthToken';
  C_InitP2PTunnel: SystemString = '__@InitP2PTunnel';
  C_CloseP2PTunnel: SystemString = '__@CloseP2PTunnel';

  { stable IO }
  C_BuildStableIO: SystemString = '__@BuildStableIO';
  C_OpenStableIO: SystemString = '__@OpenStableIO';
  C_CloseStableIO: SystemString = '__@CloseStableIO';

  { double tunnel }
  C_FileInfo: SystemString = '__@FileInfo';
  C_PostFile: SystemString = '__@PostFile';
  C_PostFileOver: SystemString = '__@PostFileOver';
  C_PostBatchStreamDone: SystemString = '__@PostBatchStreamDone';
  C_UserDB: SystemString = 'UserDB';
  C_UserLogin: SystemString = '__@UserLogin';
  C_RegisterUser: SystemString = '__@RegisterUser';
  C_TunnelLink: SystemString = '__@TunnelLink';
  C_ChangePasswd: SystemString = '__@ChangePasswd';
  C_CustomNewUser: SystemString = '__@CustomNewUser';
  C_ProcessStoreQueueCMD: SystemString = '__@ProcessStoreQueueCMD';
  C_GetPublicFileList: SystemString = '__@GetPublicFileList';
  C_GetPrivateFileList: SystemString = '__@GetPrivateFileList';
  C_GetPrivateDirectoryList: SystemString = '__@GetPrivateDirectoryList';
  C_CreatePrivateDirectory: SystemString = '__@CreatePrivateDirectory';
  C_GetPublicFileInfo: SystemString = '__@GetPublicFileInfo';
  C_GetPrivateFileInfo: SystemString = '__@GetPrivateFileInfo';
  C_GetPublicFileMD5: SystemString = '__@GetPublicFileMD5';
  C_GetPrivateFileMD5: SystemString = '__@GetPrivateFileMD5';
  C_GetPublicFile: SystemString = '__@GetPublicFile';
  C_GetPrivateFile: SystemString = '__@GetPrivateFile';
  C_GetUserPrivateFile: SystemString = '__@GetUserPrivateFile';
  C_PostPublicFileInfo: SystemString = '__@PostPublicFileInfo';
  C_PostPrivateFileInfo: SystemString = '__@PostPrivateFileInfo';
  C_GetCurrentCadencer: SystemString = '__@GetCurrentCadencer';
  C_NewBatchStream: SystemString = '__@NewBatchStream';
  C_PostBatchStream: SystemString = '__@PostBatchStream';
  C_ClearBatchStream: SystemString = '__@ClearBatchStream';
  C_GetBatchStreamState: SystemString = '__@GetBatchStreamState';
  C_GetUserPrivateFileList: SystemString = '__@GetUserPrivateFileList';
  C_GetUserPrivateDirectoryList: SystemString = '__@GetUserPrivateDirectoryList';
  C_GetFileTime: SystemString = '__@GetFileTime';
  C_GetFileInfo: SystemString = '__@GetFileInfo';
  C_GetFileMD5: SystemString = '__@GetFileMD5';
  C_GetFile: SystemString = '__@GetFile';
  C_GetFileAs: SystemString = '__@GetFileAs';
  C_PostFileInfo: SystemString = '__@PostFileInfo';
  C_GetPublicFileFragmentData: SystemString = '__@GetPublicFileFragmentData';
  C_GetPrivateFileFragmentData: SystemString = '__@GetPrivateFileFragmentData';
  C_GetFileFragmentData: SystemString = '__@GetFileFragmentData';
  C_PostFileFragmentData: SystemString = '__@PostFileFragmentData';

  { double tunnel datastore }
  C_DataStoreSecurity: SystemString = '__@DataStoreSecurity';
  C_CompletedFragmentBigStream: SystemString = '__@CompletedFragmentBigStream';
  C_CompletedQuery: SystemString = '__@CompletedQuery';
  C_CompletedDownloadAssemble: SystemString = '__@CompletedDownloadAssemble';
  C_CompletedFastDownloadAssemble: SystemString = '__@CompletedFastDownloadAssemble';
  C_CompletedStorePosTransform: SystemString = '__@CompletedStorePosTransform';
  C_InitDB: SystemString = '__@InitDB';
  C_CloseDB: SystemString = '__@CloseDB';
  C_CopyDB: SystemString = '__@CopyDB';
  C_CompressDB: SystemString = '__@CompressDB';
  C_ReplaceDB: SystemString = '__@ReplaceDB';
  C_ResetData: SystemString = '__@ResetData';
  C_QueryDB: SystemString = '__@QueryDB';
  C_DownloadDB: SystemString = '__@DownloadDB';
  C_DownloadDBWithID: SystemString = '__@DownloadDBWithID';
  C_RequestDownloadAssembleStream: SystemString = '__@RequestDownloadAssembleStream';
  C_RequestFastDownloadAssembleStrea: SystemString = '__@RequestFastDownloadAssembleStream';
  C_FastPostCompleteBuffer: SystemString = '__@FastPostCompleteBuffer';
  C_FastInsertCompleteBuffer: SystemString = '__@FastInsertCompleteBuffer';
  C_FastModifyCompleteBuffer: SystemString = '__@FastModifyCompleteBuffer';
  C_CompletedPostAssembleStream: SystemString = '__@CompletedPostAssembleStream';
  C_CompletedInsertAssembleStream: SystemString = '__@CompletedInsertAssembleStream';
  C_CompletedModifyAssembleStream: SystemString = '__@CompletedModifyAssembleStream';
  C_DeleteData: SystemString = '__@DeleteData';
  C_GetDBList: SystemString = '__@GetDBList';
  C_GetQueryList: SystemString = '__@GetQueryList';
  C_GetQueryState: SystemString = '__@GetQueryState';
  C_QueryStop: SystemString = '__@QueryStop';
  C_QueryPause: SystemString = '__@QueryPause';
  C_QueryPlay: SystemString = '__@QueryPlay';

{$ENDREGION 'ConstAndVariant'}

implementation

type
  TWaitSendConsoleCmdIntf = class(TCoreClassObject)
  public
    NewResult: SystemString;
    Done: Boolean;
    constructor Create;
    procedure WaitSendConsoleResultEvent(P_IO: TPeerIO; ResultData: SystemString);
  end;

  TWaitSendStreamCmdIntf = class(TCoreClassObject)
  public
    NewResult: TDataFrameEngine;
    Done: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure WaitSendStreamResultEvent(P_IO: TPeerIO; ResultData: TDataFrameEngine);
  end;

constructor TWaitSendConsoleCmdIntf.Create;
begin
  NewResult := '';
  Done := False;
end;

procedure TWaitSendConsoleCmdIntf.WaitSendConsoleResultEvent(P_IO: TPeerIO; ResultData: SystemString);
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

procedure TWaitSendStreamCmdIntf.WaitSendStreamResultEvent(P_IO: TPeerIO; ResultData: TDataFrameEngine);
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
            System.FreeMemory(v^.buffer);
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
  v.OnConsoleParamMethod := nil;
  v.OnConsoleFailedMethod := nil;
  v.OnConsoleProc := nil;
  v.OnConsoleParamProc := nil;
  v.OnConsoleFailedProc := nil;
  v.StreamData := nil;
  v.OnStreamMethod := nil;
  v.OnStreamParamMethod := nil;
  v.OnStreamFailedMethod := nil;
  v.OnStreamProc := nil;
  v.OnStreamParamProc := nil;
  v.OnStreamFailedProc := nil;
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

function BuildP2PVMPacket(buffSiz, FrameworkID, p2pID: Cardinal; pkType: Byte; buff: PByte): Pp2pVMFragmentPacket;
var
  p: Pp2pVMFragmentPacket;
begin
  new(p);
  p^.buffSiz := buffSiz;
  p^.FrameworkID := FrameworkID;
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

procedure FreeP2PVMPacket(p: Pp2pVMFragmentPacket);
begin
  if (p^.buff <> nil) and (p^.buffSiz > 0) then
      FreeMem(p^.buff, p^.buffSiz);
  Dispose(p);
end;

function IsSystemCMD(const Cmd: U_String): Boolean;
begin
  Result := Cmd.Same(C_CipherModel, C_BuildP2PAuthToken, C_InitP2PTunnel, C_CloseP2PTunnel, C_Wait) or
    Cmd.Same(C_BuildStableIO, C_OpenStableIO, C_CloseStableIO);
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

function IPv4ToStr(const IPv4Addr_: TIPV4): U_String;
begin
  Result.Text := IntToStr(IPv4Addr_[0]) + '.' + IntToStr(IPv4Addr_[1]) + '.' + IntToStr(IPv4Addr_[2]) + '.' + IntToStr(IPv4Addr_[3]);
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
      OmitCnt := 0; { Make the compiler happy }
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
        c.PrintCommand('execute console on result: %s', QueuePtr^.Cmd);
        try
            QueuePtr^.OnConsoleMethod(c, AResultText);
        except
        end;
      end;
    if Assigned(QueuePtr^.OnConsoleParamMethod) then
      begin
        c.PrintCommand('execute console on param result: %s', QueuePtr^.Cmd);
        try
            QueuePtr^.OnConsoleParamMethod(c, QueuePtr^.Param1, QueuePtr^.Param2, QueuePtr^.ConsoleData, AResultText);
        except
        end;
      end;
    if Assigned(QueuePtr^.OnConsoleProc) then
      begin
        c.PrintCommand('execute console on result(proc): %s', QueuePtr^.Cmd);
        try
            QueuePtr^.OnConsoleProc(c, AResultText);
        except
        end;
      end;
    if Assigned(QueuePtr^.OnConsoleParamProc) then
      begin
        c.PrintCommand('execute console on param result(proc): %s', QueuePtr^.Cmd);
        try
            QueuePtr^.OnConsoleParamProc(c, QueuePtr^.Param1, QueuePtr^.Param2, QueuePtr^.ConsoleData, AResultText);
        except
        end;
      end;
    if Assigned(QueuePtr^.OnStreamMethod) then
      begin
        c.PrintCommand('execute stream on result: %s', QueuePtr^.Cmd);
        try
          AResultDF.Reader.index := 0;
          QueuePtr^.OnStreamMethod(c, AResultDF);
        except
        end;
      end;
    if Assigned(QueuePtr^.OnStreamParamMethod) then
      begin
        c.PrintCommand('execute stream on param result: %s', QueuePtr^.Cmd);
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
    if Assigned(QueuePtr^.OnStreamProc) then
      begin
        c.PrintCommand('execute stream on result(proc): %s', QueuePtr^.Cmd);
        try
          AResultDF.Reader.index := 0;
          QueuePtr^.OnStreamProc(c, AResultDF);
        except
        end;
      end;
    if Assigned(QueuePtr^.OnStreamParamProc) then
      begin
        c.PrintCommand('execute stream on result(parameter + proc): %s', QueuePtr^.Cmd);
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
  except
  end;
  c.FReceiveResultRuning := False;
end;

procedure THPC_Stream.Run(Sender: TCompute);
begin
  Thread := Sender;
  try
    if Assigned(OnCall) then
        OnCall(Self, InData, OutData);
    if Assigned(OnMethod) then
        OnMethod(Self, InData, OutData);
    if Assigned(OnProc) then
        OnProc(Self, InData, OutData);
  except
  end;
end;

procedure THPC_Stream.RunDone(Sender: TCompute);
var
  P_IO: TPeerIO;
begin
  try
    if Framework <> nil then
      begin
        AtomDec(Framework.FCMDWithThreadRuning);

        P_IO := TPeerIO(Framework.FPeerIO_HashPool[WorkID]);

        if P_IO <> nil then
          begin
            P_IO.OutDataFrame.Assign(OutData);
            P_IO.ContinueResultSend;
          end;
      end;
  except
  end;
  DisposeObject(Self);
end;

constructor THPC_Stream.Create;
begin
  inherited Create;
  Thread := nil;
  OnCall := nil;
  OnMethod := nil;
  OnProc := nil;
  Framework := nil;
  WorkID := 0;
  UserData := nil;
  UserObject := nil;
  InData := TDataFrameEngine.Create;
  OutData := TDataFrameEngine.Create;
end;

destructor THPC_Stream.Destroy;
begin
  DisposeObject(InData);
  DisposeObject(OutData);
  inherited Destroy;
end;

function THPC_Stream.IsOnline: Boolean;
begin
  Result := (Framework <> nil) and (Framework.ExistsID(WorkID));
end;

procedure RunHPC_StreamC(Sender: TPeerIO;
  const UserData: Pointer; const UserObject: TCoreClassObject;
  const InData, OutData: TDataFrameEngine; const OnRun: TOnHPC_StreamCall);
var
  t: THPC_Stream;
begin
  Sender.PauseResultSend;
  t := THPC_Stream.Create;

  t.OnCall := OnRun;

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

  TCompute.RunM(UserData, UserObject, {$IFDEF FPC}@{$ENDIF FPC}t.Run, {$IFDEF FPC}@{$ENDIF FPC}t.RunDone);
end;

procedure RunHPC_StreamM(Sender: TPeerIO;
  const UserData: Pointer; const UserObject: TCoreClassObject;
  const InData, OutData: TDataFrameEngine; const OnRun: TOnHPC_StreamMethod);
var
  t: THPC_Stream;
begin
  Sender.PauseResultSend;
  t := THPC_Stream.Create;

  t.OnMethod := OnRun;

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

  TCompute.RunM(UserData, UserObject, {$IFDEF FPC}@{$ENDIF FPC}t.Run, {$IFDEF FPC}@{$ENDIF FPC}t.RunDone);
end;

procedure RunHPC_StreamP(Sender: TPeerIO;
  const UserData: Pointer; const UserObject: TCoreClassObject;
  const InData, OutData: TDataFrameEngine; const OnRun: TOnHPC_StreamProc);
var
  t: THPC_Stream;
begin
  Sender.PauseResultSend;
  t := THPC_Stream.Create;

  t.OnProc := OnRun;

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

  TCompute.RunM(UserData, UserObject, {$IFDEF FPC}@{$ENDIF FPC}t.Run, {$IFDEF FPC}@{$ENDIF FPC}t.RunDone);
end;

procedure THPC_DirectStream.Run(Sender: TCompute);
begin
  Thread := Sender;
  try
    if Assigned(OnCall) then
        OnCall(Self, InData);
    if Assigned(OnMethod) then
        OnMethod(Self, InData);
    if Assigned(OnProc) then
        OnProc(Self, InData);
  except
  end;
end;

constructor THPC_DirectStream.Create;
begin
  inherited Create;
  Thread := nil;
  OnCall := nil;
  OnMethod := nil;
  OnProc := nil;
  Framework := nil;
  WorkID := 0;
  UserData := nil;
  UserObject := nil;
  InData := TDataFrameEngine.Create;
end;

destructor THPC_DirectStream.Destroy;
begin
  DisposeObject(InData);
  inherited Destroy;
end;

function THPC_DirectStream.IsOnline: Boolean;
begin
  Result := (Framework <> nil) and (Framework.ExistsID(WorkID));
end;

procedure RunHPC_DirectStreamC(Sender: TPeerIO;
  const UserData: Pointer; const UserObject: TCoreClassObject;
  const InData: TDataFrameEngine; const OnRun: TOnHPC_DirectStreamCall);
var
  t: THPC_DirectStream;
begin
  t := THPC_DirectStream.Create;

  t.OnCall := OnRun;

  t.Framework := Sender.OwnerFramework;
  t.WorkID := Sender.ID;
  t.UserData := UserData;
  t.UserObject := UserObject;
  t.InData := TDataFrameEngine.Create;
  if InData <> nil then
      t.InData.Assign(InData);

  AtomInc(Sender.OwnerFramework.FCMDWithThreadRuning);

  TCompute.RunM(UserData, UserObject, {$IFDEF FPC}@{$ENDIF FPC}t.Run);
end;

procedure RunHPC_DirectStreamM(Sender: TPeerIO;
  const UserData: Pointer; const UserObject: TCoreClassObject;
  const InData: TDataFrameEngine; const OnRun: TOnHPC_DirectStreamMethod);
var
  t: THPC_DirectStream;
begin
  t := THPC_DirectStream.Create;

  t.OnMethod := OnRun;

  t.Framework := Sender.OwnerFramework;
  t.WorkID := Sender.ID;
  t.UserData := UserData;
  t.UserObject := UserObject;
  t.InData := TDataFrameEngine.Create;
  if InData <> nil then
      t.InData.Assign(InData);

  AtomInc(Sender.OwnerFramework.FCMDWithThreadRuning);

  TCompute.RunM(UserData, UserObject, {$IFDEF FPC}@{$ENDIF FPC}t.Run);
end;

procedure RunHPC_DirectStreamP(Sender: TPeerIO;
  const UserData: Pointer; const UserObject: TCoreClassObject;
  const InData: TDataFrameEngine; const OnRun: TOnHPC_DirectStreamProc);
var
  t: THPC_DirectStream;
begin
  t := THPC_DirectStream.Create;

  t.OnProc := OnRun;

  t.Framework := Sender.OwnerFramework;
  t.WorkID := Sender.ID;
  t.UserData := UserData;
  t.UserObject := UserObject;
  t.InData := TDataFrameEngine.Create;
  if InData <> nil then
      t.InData.Assign(InData);

  AtomInc(Sender.OwnerFramework.FCMDWithThreadRuning);

  TCompute.RunM(UserData, UserObject, {$IFDEF FPC}@{$ENDIF FPC}t.Run);
end;

procedure THPC_Console.Run(Sender: TCompute);
begin
  Thread := Sender;
  try
    if Assigned(OnCall) then
        OnCall(Self, InData, OutData);
    if Assigned(OnMethod) then
        OnMethod(Self, InData, OutData);
    if Assigned(OnProc) then
        OnProc(Self, InData, OutData);
  except
  end;
end;

procedure THPC_Console.RunDone(Sender: TCompute);
var
  P_IO: TPeerIO;
begin
  try
    if Framework <> nil then
      begin
        AtomDec(Framework.FCMDWithThreadRuning);

        P_IO := TPeerIO(Framework.FPeerIO_HashPool[WorkID]);

        if P_IO <> nil then
          begin
            P_IO.OutText := OutData;
            P_IO.ContinueResultSend;
          end;
      end;
  except
  end;
  DisposeObject(Self);
end;

constructor THPC_Console.Create;
begin
  inherited Create;
  Thread := nil;
  OnCall := nil;
  OnMethod := nil;
  OnProc := nil;
  Framework := nil;
  WorkID := 0;
  UserData := nil;
  UserObject := nil;
  InData := '';
  OutData := '';
end;

destructor THPC_Console.Destroy;
begin
  inherited Destroy;
end;

function THPC_Console.IsOnline: Boolean;
begin
  Result := (Framework <> nil) and (Framework.ExistsID(WorkID));
end;

procedure RunHPC_ConsoleC(Sender: TPeerIO;
  const UserData: Pointer; const UserObject: TCoreClassObject;
  const InData, OutData: SystemString; const OnRun: TOnHPC_ConsoleCall);
var
  t: THPC_Console;
begin
  Sender.PauseResultSend;
  t := THPC_Console.Create;

  t.OnCall := OnRun;

  t.Framework := Sender.OwnerFramework;
  t.WorkID := Sender.ID;
  t.UserData := UserData;
  t.UserObject := UserObject;
  t.InData := InData;
  t.OutData := OutData;

  AtomInc(Sender.OwnerFramework.FCMDWithThreadRuning);

  TCompute.RunM(UserData, UserObject, {$IFDEF FPC}@{$ENDIF FPC}t.Run, {$IFDEF FPC}@{$ENDIF FPC}t.RunDone);
end;

procedure RunHPC_ConsoleM(Sender: TPeerIO;
  const UserData: Pointer; const UserObject: TCoreClassObject;
  const InData, OutData: SystemString; const OnRun: TOnHPC_ConsoleMethod);
var
  t: THPC_Console;
begin
  Sender.PauseResultSend;
  t := THPC_Console.Create;

  t.OnMethod := OnRun;

  t.Framework := Sender.OwnerFramework;
  t.WorkID := Sender.ID;
  t.UserData := UserData;
  t.UserObject := UserObject;
  t.InData := InData;
  t.OutData := OutData;

  AtomInc(Sender.OwnerFramework.FCMDWithThreadRuning);

  TCompute.RunM(UserData, UserObject, {$IFDEF FPC}@{$ENDIF FPC}t.Run, {$IFDEF FPC}@{$ENDIF FPC}t.RunDone);
end;

procedure RunHPC_ConsoleP(Sender: TPeerIO;
  const UserData: Pointer; const UserObject: TCoreClassObject;
  const InData, OutData: SystemString; const OnRun: TOnHPC_ConsoleProc);
var
  t: THPC_Console;
begin
  Sender.PauseResultSend;
  t := THPC_Console.Create;

  t.OnProc := OnRun;

  t.Framework := Sender.OwnerFramework;
  t.WorkID := Sender.ID;
  t.UserData := UserData;
  t.UserObject := UserObject;
  t.InData := InData;
  t.OutData := OutData;

  AtomInc(Sender.OwnerFramework.FCMDWithThreadRuning);

  TCompute.RunM(UserData, UserObject, {$IFDEF FPC}@{$ENDIF FPC}t.Run, {$IFDEF FPC}@{$ENDIF FPC}t.RunDone);
end;

procedure THPC_DirectConsole.Run(Sender: TCompute);
begin
  Thread := Sender;
  try
    if Assigned(OnCall) then
        OnCall(Self, InData);
    if Assigned(OnMethod) then
        OnMethod(Self, InData);
    if Assigned(OnProc) then
        OnProc(Self, InData);
  except
  end;
end;

constructor THPC_DirectConsole.Create;
begin
  inherited Create;
  Thread := nil;
  OnCall := nil;
  OnMethod := nil;
  OnProc := nil;
  Framework := nil;
  WorkID := 0;
  UserData := nil;
  UserObject := nil;
  InData := '';
end;

destructor THPC_DirectConsole.Destroy;
begin
  inherited Destroy;
end;

function THPC_DirectConsole.IsOnline: Boolean;
begin
  Result := (Framework <> nil) and (Framework.ExistsID(WorkID));
end;

procedure RunHPC_DirectConsoleC(Sender: TPeerIO;
  const UserData: Pointer; const UserObject: TCoreClassObject;
  const InData: SystemString; const OnRun: TOnHPC_DirectConsoleCall);
var
  t: THPC_DirectConsole;
begin
  t := THPC_DirectConsole.Create;

  t.OnCall := OnRun;

  t.Framework := Sender.OwnerFramework;
  t.WorkID := Sender.ID;
  t.UserData := UserData;
  t.UserObject := UserObject;
  t.InData := InData;

  AtomInc(Sender.OwnerFramework.FCMDWithThreadRuning);

  TCompute.RunM(UserData, UserObject, {$IFDEF FPC}@{$ENDIF FPC}t.Run);
end;

procedure RunHPC_DirectConsoleM(Sender: TPeerIO;
  const UserData: Pointer; const UserObject: TCoreClassObject;
  const InData: SystemString; const OnRun: TOnHPC_DirectConsoleMethod);
var
  t: THPC_DirectConsole;
begin
  t := THPC_DirectConsole.Create;

  t.OnMethod := OnRun;

  t.Framework := Sender.OwnerFramework;
  t.WorkID := Sender.ID;
  t.UserData := UserData;
  t.UserObject := UserObject;
  t.InData := InData;

  AtomInc(Sender.OwnerFramework.FCMDWithThreadRuning);

  TCompute.RunM(UserData, UserObject, {$IFDEF FPC}@{$ENDIF FPC}t.Run);
end;

procedure RunHPC_DirectConsoleP(Sender: TPeerIO;
  const UserData: Pointer; const UserObject: TCoreClassObject;
  const InData: SystemString; const OnRun: TOnHPC_DirectConsoleProc);
var
  t: THPC_DirectConsole;
begin
  t := THPC_DirectConsole.Create;

  t.OnProc := OnRun;

  t.Framework := Sender.OwnerFramework;
  t.WorkID := Sender.ID;
  t.UserData := UserData;
  t.UserObject := UserObject;
  t.InData := InData;

  AtomInc(Sender.OwnerFramework.FCMDWithThreadRuning);

  TCompute.RunM(UserData, UserObject, {$IFDEF FPC}@{$ENDIF FPC}t.Run);
end;

procedure TOnStateStruct.Init;
begin
  OnCall := nil;
  OnMethod := nil;
  OnProc := nil;
end;

constructor TStateParamBridge.Create;
begin
  inherited Create;
  OnNotifyC := nil;
  OnNotifyM := nil;
  OnNotifyP := nil;
  Param1 := nil;
  Param2 := nil;
end;

procedure TStateParamBridge.DoStateResult(const State: Boolean);
begin
  if Assigned(OnNotifyC) then
      OnNotifyC(Param1, Param2, State);
  if Assigned(OnNotifyM) then
      OnNotifyM(Param1, Param2, State);
  if Assigned(OnNotifyP) then
      OnNotifyP(Param1, Param2, State);
  DisposeObject(Self);
end;

constructor TCommandStream.Create;
begin
  inherited Create;

  FOnExecuteCall := nil;
  FOnExecuteMethod := nil;
  FOnExecuteProc := nil;
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
    else if Assigned(FOnExecuteProc) then
        FOnExecuteProc(Sender, InData, OutData)
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
  FOnExecuteProc := nil;
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
    else if Assigned(FOnExecuteProc) then
        FOnExecuteProc(Sender, InData, OutData)
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
  FOnExecuteProc := nil;
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
    else if Assigned(FOnExecuteProc) then
        FOnExecuteProc(Sender, InData)
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
  FOnExecuteProc := nil;
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
    else if Assigned(FOnExecuteProc) then
        FOnExecuteProc(Sender, InData)
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
  FOnExecuteProc := nil;
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
    else if Assigned(FOnExecuteProc) then
        FOnExecuteProc(Sender, InData, BigStreamTotal, BigStreamCompleteSize)
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
  FOnExecuteProc := nil;
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
    else if Assigned(FOnExecuteProc) then
        FOnExecuteProc(Sender, InData, DataSize)
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

function TBigStreamBatch.GetItems(const index: Integer): PBigStreamBatchPostData;
begin
  Result := PBigStreamBatchPostData(FList[index]);
end;

constructor TBigStreamBatch.Create(Owner_: TPeerIO);
begin
  inherited Create;
  FOwner := Owner_;
  FList := TCoreClassList.Create;
end;

destructor TBigStreamBatch.Destroy;
begin
  Clear;
  DisposeObject(FList);
  inherited Destroy;
end;

procedure TBigStreamBatch.Clear;
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

function TBigStreamBatch.Count: Integer;
begin
  Result := FList.Count;
end;

function TBigStreamBatch.NewPostData: PBigStreamBatchPostData;
begin
  new(Result);
  Result^.Init;
  Result^.Source := TMemoryStream64.Create;
  Result^.index := FList.Add(Result);
end;

function TBigStreamBatch.First: PBigStreamBatchPostData;
begin
  Result := PBigStreamBatchPostData(FList[0]);
end;

function TBigStreamBatch.Last: PBigStreamBatchPostData;
begin
  Result := PBigStreamBatchPostData(FList[FList.Count - 1]);
end;

procedure TBigStreamBatch.DeleteLast;
begin
  if FList.Count > 0 then
      Delete(FList.Count - 1);
end;

procedure TBigStreamBatch.Delete(const index: Integer);
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

procedure TPeerIOUserDefine.DelayFreeOnBusy;
begin
  while FBusy do
      TCoreClassThread.Sleep(1);

  DisposeObject(Self);
end;

constructor TPeerIOUserDefine.Create(Owner_: TPeerIO);
begin
  inherited Create;
  FOwner := Owner_;
  FWorkPlatform := TExecutePlatform.epUnknow;
  FBigStreamBatch := TBigStreamBatch.Create(Owner);
  FBusy := False;
end;

destructor TPeerIOUserDefine.Destroy;
begin
  DisposeObject(FBigStreamBatch);
  inherited Destroy;
end;

procedure TPeerIOUserDefine.Progress;
begin
end;

procedure TPeerIOUserSpecial.DelayFreeOnBusy;
begin
  while FBusy do
      TCoreClassThread.Sleep(1);
  DisposeObject(Self);
end;

constructor TPeerIOUserSpecial.Create(Owner_: TPeerIO);
begin
  inherited Create;
  FOwner := Owner_;
  FBusy := False;
end;

destructor TPeerIOUserSpecial.Destroy;
begin
  inherited Destroy;
end;

procedure TPeerIOUserSpecial.Progress;
begin
end;

function TPeerIO.Connected: Boolean;
begin
  Result := False;
end;

procedure TPeerIO.Disconnect;
begin
  CheckAndTriggerFailedWaitResult();
end;

procedure TPeerIO.SendByteBuffer(const buff: PByte; const Size: NativeInt);
begin
end;

procedure TPeerIO.WriteBufferOpen;
begin
end;

procedure TPeerIO.WriteBufferFlush;
begin
end;

procedure TPeerIO.WriteBufferClose;
begin
end;

function TPeerIO.GetPeerIP: SystemString;
begin
  Result := 'offline';
end;

function TPeerIO.WriteBufferEmpty: Boolean;
begin
  Result := True;
end;

procedure TPeerIO.InitSequencePacketModel(const hashSize, MemoryDelta: Integer);
begin
  FSequencePacketActivted := FOwnerFramework.FSequencePacketActivted;
  FSequencePacketSignal := True;

  SequenceNumberOnSendCounter := 0;
  SequenceNumberOnReceivedCounter := 0;

  SendingSequencePacketHistory := TUInt32HashPointerList.CustomCreate(hashSize);
  SendingSequencePacketHistory.OnFreePtr := {$IFDEF FPC}@{$ENDIF FPC}Send_Free_OnPtr;
  SendingSequencePacketHistory.AutoFreeData := True;
  SendingSequencePacketHistory.OnAddPtr := {$IFDEF FPC}@{$ENDIF FPC}Send_Add_OnPtr;

  SequencePacketReceivedPool := TUInt32HashPointerList.CustomCreate(hashSize);
  SequencePacketReceivedPool.OnFreePtr := {$IFDEF FPC}@{$ENDIF FPC}Received_Free_OnPtr;
  SequencePacketReceivedPool.AutoFreeData := True;
  SequencePacketReceivedPool.OnAddPtr := {$IFDEF FPC}@{$ENDIF FPC}Received_Add_OnPtr;

  SendingSequencePacketHistoryMemory := 0;
  SequencePacketReceivedPoolMemory := 0;

  IOSendBuffer := TMemoryStream64.CustomCreate(MemoryDelta);
  SequencePacketSendBuffer := TMemoryStream64.CustomCreate(MemoryDelta);
  SequencePacketReceivedBuffer := TMemoryStream64.CustomCreate(MemoryDelta);

  FSequencePacketMTU := C_SequencePacketMTU;

  FSequencePacketLimitPhysicsMemory := 0;
  SequencePacketCloseDone := False;

  SequencePacketVerifyTick := GetTimeTick;
end;

procedure TPeerIO.FreeSequencePacketModel;
begin
  DisposeObject(SendingSequencePacketHistory);
  SendingSequencePacketHistory := nil;

  DisposeObject(SequencePacketReceivedPool);
  SequencePacketReceivedPool := nil;

  DisposeObject(IOSendBuffer);
  IOSendBuffer := nil;

  DisposeObject(SequencePacketSendBuffer);
  SequencePacketSendBuffer := nil;

  DisposeObject(SequencePacketReceivedBuffer);
  SequencePacketReceivedBuffer := nil;
end;

procedure TPeerIO.ResetSequencePacketBuffer;
begin
  IOSendBuffer.Clear;
  SequencePacketSendBuffer.Clear;
  SequencePacketReceivedBuffer.Clear;
end;

procedure TPeerIO.ProcessSequencePacketModel;
var
  i: NativeInt;
  pH: PUInt32HashListPointerStruct;
  p: PSequencePacket;
  siz: NativeInt;
begin
  if not IsSequencePacketModel then
      exit;

  if SequencePacketCloseDone then
      exit;

  if (FSequencePacketLimitPhysicsMemory <> 0) and
    (SendingSequencePacketHistoryMemory + SequencePacketReceivedPoolMemory > FSequencePacketLimitPhysicsMemory) then
    begin
      PrintError('memory exceeds security limit for Sequence Packet signal buffer.');
      SequencePacketCloseDone := True;
      DelayClose;
      exit;
    end;

  if (FSequencePacketSignal)
    and (SendingSequencePacketHistory.Count > 0)
    and (WriteBufferEmpty)
    and (GetTimeTick - SequencePacketVerifyTick > 1000) then
    begin
      IOSendBuffer.Position := IOSendBuffer.Size;

      siz := 0;
      i := 0;
      pH := SendingSequencePacketHistory.FirstPtr;
      while (i < SendingSequencePacketHistory.Count) and (siz < 100 * 1024) do
        begin
          p := pH^.data;
          if (GetTimeTick - p^.tick > 3000) then
            begin
              WriteSequencePacket(p);
              p^.tick := GetTimeTick;
              inc(siz, p^.Size);
            end;
          inc(i);
          pH := pH^.Next;
        end;

      SequencePacketVerifyTick := GetTimeTick;
    end;

  FlushIOSendBuffer;
end;

function TPeerIO.GetSequencePacketState: SystemString;
begin
  Result := PFormat('History: %s (block: %d) Received Pool: %s (block: %d) Total Memory: %s',
    [umlSizeToStr(SendingSequencePacketHistoryMemory).Text,
    SendingSequencePacketHistory.Count,
    umlSizeToStr(SequencePacketReceivedPoolMemory).Text,
    SequencePacketReceivedPool.Count,
    umlSizeToStr(SendingSequencePacketHistoryMemory + SequencePacketReceivedPoolMemory).Text
    ]);
end;

function TPeerIO.GetSequencePacketUsagePhysicsMemory: Int64;
begin
  Result := SendingSequencePacketHistoryMemory + SequencePacketReceivedPoolMemory;
end;

function TPeerIO.ComputeSequencePacketHash(const p: PByte; const Count: nativeUInt): TMD5;
begin
  Result := umlMD5(p, Count);
end;

function TPeerIO.IsSequencePacketModel: Boolean;
begin
  Result := (FSequencePacketActivted) and (FOwnerFramework.Protocol = TCommunicationProtocol.cpZServer);
end;

procedure TPeerIO.FlushIOSendBuffer;
begin
  if (IOSendBuffer.Size > 0) then
    begin
      WriteBufferOpen;
      OnInternalSendByteBuffer(Self, IOSendBuffer.Memory, IOSendBuffer.Size);
      IOSendBuffer.Clear;
      WriteBufferFlush;
      WriteBufferClose;
    end;
end;

procedure TPeerIO.SendSequencePacketBegin;
begin
  SequencePacketSendBuffer.Clear;
end;

procedure TPeerIO.SendSequencePacket(const buff: PByte; siz: NativeInt);
begin
  SequencePacketSendBuffer.WritePtr(buff, siz);
end;

procedure TPeerIO.SendSequencePacketEnd;
var
  pBuff: PByte;
  p: PSequencePacket;
  siz: NativeInt;
  FlushBuffSize: Word;
begin
  if SequencePacketSendBuffer.Size <= 0 then
      exit;

  if not IsSequencePacketModel then
    begin
      WriteBufferOpen;
      OnInternalSendByteBuffer(Self, SequencePacketSendBuffer.Memory, SequencePacketSendBuffer.Size);
      SequencePacketSendBuffer.Clear;
      WriteBufferFlush;
      WriteBufferClose;
      exit;
    end;

  FlushBuffSize := umlMax(FSequencePacketMTU, 1024) - (C_Sequence_Packet_HeadSize + 1);

  siz := SequencePacketSendBuffer.Size;
  pBuff := SequencePacketSendBuffer.Memory;

  IOSendBuffer.Position := IOSendBuffer.Size;

  { fragment build to sending }
  while siz > FlushBuffSize do
    begin
      new(p);
      p^.SequenceNumber := SequenceNumberOnSendCounter;
      p^.data := TMemoryStream64.Create;
      p^.data.Size := FlushBuffSize;
      p^.Size := p^.data.Size;
      CopyPtr(pBuff, p^.data.Memory, p^.data.Size);
      p^.hash := ComputeSequencePacketHash(p^.data.Memory, p^.data.Size);
      p^.tick := GetTimeTick;

      inc(pBuff, FlushBuffSize);
      dec(siz, FlushBuffSize);

      WriteSequencePacket(p);
      inc(SequenceNumberOnSendCounter);

      if FSequencePacketSignal then
          SendingSequencePacketHistory.Add(p^.SequenceNumber, p, False)
      else
        begin
          DisposeObject(p^.data);
          Dispose(p);
        end;
    end;

  if siz > 0 then
    begin
      new(p);
      p^.SequenceNumber := SequenceNumberOnSendCounter;
      p^.data := TMemoryStream64.Create;
      p^.data.Size := siz;
      p^.Size := p^.data.Size;
      CopyPtr(pBuff, p^.data.Memory, p^.data.Size);
      p^.hash := ComputeSequencePacketHash(p^.data.Memory, p^.data.Size);
      p^.tick := GetTimeTick;

      WriteSequencePacket(p);
      inc(SequenceNumberOnSendCounter);

      if FSequencePacketSignal then
          SendingSequencePacketHistory.Add(p^.SequenceNumber, p, False)
      else
        begin
          DisposeObject(p^.data);
          Dispose(p);
        end;
    end;
  SequencePacketSendBuffer.Clear;

  FlushIOSendBuffer;
end;

procedure TPeerIO.SendSequencePacketKeepAlive(p: Pointer; siz: Word);
begin
  if FSequencePacketSignal and IsSequencePacketModel then
    begin
      IOSendBuffer.Position := IOSendBuffer.Size;

      IOSendBuffer.WriteUInt8(C_Sequence_KeepAlive);
      IOSendBuffer.WriteUInt16(siz);
      if siz > 0 then
          IOSendBuffer.WritePtr(p, siz);
    end;
end;

procedure TPeerIO.DoSequencePacketEchoKeepAlive(p: Pointer; siz: Word);
begin
end;

procedure TPeerIO.WriteSequencePacket(p: PSequencePacket);
begin
  if FSequencePacketSignal then
      IOSendBuffer.WriteUInt8(C_Sequence_Packet)
  else
      IOSendBuffer.WriteUInt8(C_Sequence_QuietPacket);
  IOSendBuffer.WriteUInt16(p^.Size);
  IOSendBuffer.WriteUInt32(p^.SequenceNumber);
  IOSendBuffer.WriteMD5(p^.hash);
  IOSendBuffer.WritePtr(p^.data.Memory, p^.data.Size);
end;

procedure TPeerIO.ResendSequencePacket(SequenceNumber: Cardinal);
var
  p: PSequencePacket;
begin
  p := SendingSequencePacketHistory[SequenceNumber];
  if p <> nil then
    begin
      WriteSequencePacket(p);
      p^.tick := GetTimeTick();
    end
  else
      PrintError('resend error, invalid Sequence Packet ' + IntToHex(SequenceNumber, 8));
end;

function TPeerIO.FillSequencePacketTo(const buff: Pointer; siz: Int64; ExtractDest: TMemoryStream64): Boolean;
var
  ErrorState: Boolean;
  p: PSequencePacket;
  head: Byte;
  echoSiz: Word;
  echoBuff: TBytes;
  ResendNumber, DoneNumber: Cardinal;
  fastSwap, n: TMemoryStream64;
  hashMatched: Boolean;
begin
  Result := True;

  if not IsSequencePacketModel then
    begin
      ExtractDest.Position := ExtractDest.Size;
      if (buff <> nil) and (siz > 0) then
          ExtractDest.WritePtr(buff, siz);
      exit;
    end;

  SequencePacketReceivedBuffer.Position := SequencePacketReceivedBuffer.Size;
  if (buff <> nil) and (siz > 0) then
      SequencePacketReceivedBuffer.WritePtr(buff, siz);

  fastSwap := TMemoryStream64.Create;
  fastSwap.SetPointerWithProtectedMode(SequencePacketReceivedBuffer.Memory, SequencePacketReceivedBuffer.Size);

  IOSendBuffer.Position := IOSendBuffer.Size;
  ExtractDest.Position := ExtractDest.Size;

  ErrorState := False;
  new(p);

  while fastSwap.Size > 0 do
    begin
      if fastSwap.Position + 1 > fastSwap.Size then
          Break;

      head := fastSwap.ReadUInt8;

      if head = C_Sequence_KeepAlive then
        begin
          if fastSwap.Position + 2 > fastSwap.Size then
              Break;
          echoSiz := fastSwap.ReadUInt16;
          if fastSwap.Position + echoSiz > fastSwap.Size then
              Break;

          if FSequencePacketSignal then
            begin
              IOSendBuffer.WriteUInt8(C_Sequence_EchoKeepAlive);
              IOSendBuffer.WriteUInt16(echoSiz);
              if echoSiz > 0 then
                  IOSendBuffer.CopyFrom(fastSwap, echoSiz);
            end
          else
              fastSwap.Position := fastSwap.Position + echoSiz;
        end
      else if head = C_Sequence_EchoKeepAlive then
        begin
          if fastSwap.Position + 2 > fastSwap.Size then
              Break;
          echoSiz := fastSwap.ReadUInt16;
          if fastSwap.Position + echoSiz > fastSwap.Size then
              Break;

          SetLength(echoBuff, echoSiz);
          if echoSiz > 0 then
            begin
              fastSwap.ReadPtr(@echoBuff[0], echoSiz);
              DoSequencePacketEchoKeepAlive(@echoBuff[0], echoSiz);
              SetLength(echoBuff, 0);
            end
          else
              DoSequencePacketEchoKeepAlive(nil, 0);
        end
      else if head = C_Sequence_RequestResend then
        begin
          if fastSwap.Position + 4 > fastSwap.Size then
              Break;
          ResendNumber := fastSwap.ReadUInt32;
          { resend Packet }
          if FSequencePacketSignal then
              ResendSequencePacket(ResendNumber);
          AtomInc(FOwnerFramework.Statistics[TStatisticsType.stSequencePacketRequestResend]);
        end
      else if head = C_Sequence_EchoPacket then
        begin
          if fastSwap.Position + 4 > fastSwap.Size then
              Break;
          DoneNumber := fastSwap.ReadUInt32;
          { recycle Packet }
          SendingSequencePacketHistory.Delete(DoneNumber);
          AtomInc(FOwnerFramework.Statistics[TStatisticsType.stSequencePacketEcho]);
        end
      else if head in [C_Sequence_QuietPacket, C_Sequence_Packet] then
        begin
          if fastSwap.Position + C_Sequence_Packet_HeadSize > fastSwap.Size then
              Break;

          p^.Size := fastSwap.ReadUInt16;
          p^.SequenceNumber := fastSwap.ReadUInt32;
          p^.hash := fastSwap.ReadMD5;

          if fastSwap.Position + p^.Size > fastSwap.Size then
              Break;

          hashMatched := umlMD5Compare(p^.hash, ComputeSequencePacketHash(fastSwap.PositionAsPtr(), p^.Size));

          if hashMatched then
            begin
              if (FSequencePacketSignal) and (head = C_Sequence_Packet) then
                begin
                  IOSendBuffer.WriteUInt8(C_Sequence_EchoPacket);
                  IOSendBuffer.WriteUInt32(p^.SequenceNumber);
                end;

              AtomInc(FOwnerFramework.Statistics[TStatisticsType.stSequencePacketReceived]);

              if p^.SequenceNumber = SequenceNumberOnReceivedCounter then
                begin
                  ExtractDest.CopyFrom(fastSwap, p^.Size);
                  SequencePacketReceivedPool.Delete(p^.SequenceNumber);
                  inc(SequenceNumberOnReceivedCounter);
                  AtomInc(FOwnerFramework.Statistics[TStatisticsType.stSequencePacketMatched]);
                end
              else if (FSequencePacketSignal) and ((p^.SequenceNumber > SequenceNumberOnReceivedCounter) or
                (Cardinal(p^.SequenceNumber + Cardinal($7FFFFFFF)) > Cardinal(SequenceNumberOnReceivedCounter + Cardinal($7FFFFFFF)))) then
                begin
                  p^.data := TMemoryStream64.Create;
                  p^.data.CopyFrom(fastSwap, p^.Size);
                  p^.tick := GetTimeTick;
                  SequencePacketReceivedPool.Add(p^.SequenceNumber, p, True);

                  new(p);
                  AtomInc(FOwnerFramework.Statistics[TStatisticsType.stSequencePacketPlan]);
                end
              else
                begin
                  AtomInc(FOwnerFramework.Statistics[TStatisticsType.stSequencePacketDiscard]);
                  AtomInc(FOwnerFramework.Statistics[TStatisticsType.stSequencePacketDiscardSize], p^.Size);
                  fastSwap.Position := fastSwap.Position + p^.Size;
                end;
            end
          else
            begin
              fastSwap.Position := fastSwap.Position + p^.Size;
              if FSequencePacketSignal then
                begin
                  IOSendBuffer.WriteUInt8(C_Sequence_RequestResend);
                  IOSendBuffer.WriteUInt32(p^.SequenceNumber);
                end;
            end;
        end
      else
        begin
          PrintError('sequence packet: error head');
          DoStatus('error buffer: ', buff, umlMin(siz, 200), 60);
          ErrorState := True;
          Break;
        end;

      n := TMemoryStream64.Create;
      n.SetPointerWithProtectedMode(fastSwap.PositionAsPtr(), fastSwap.Size - fastSwap.Position);
      DisposeObject(fastSwap);
      fastSwap := n;
    end;
  Dispose(p);

  if ErrorState then
    begin
      DisposeObject(fastSwap);
      Result := False;
      exit;
    end;

  { strip buffer }
  n := TMemoryStream64.CustomCreate(SequencePacketReceivedBuffer.Delta);
  if fastSwap.Size > 0 then
    begin
      n.WritePtr(fastSwap.Memory, fastSwap.Size);
      n.Position := 0;
    end;
  DisposeObject(SequencePacketReceivedBuffer);
  SequencePacketReceivedBuffer := n;
  DisposeObject(fastSwap);

  { extract buffer }
  while SequencePacketReceivedPool.Count > 0 do
    begin
      p := SequencePacketReceivedPool[SequenceNumberOnReceivedCounter];
      if p = nil then
        begin
          if FSequencePacketSignal then
            begin
              IOSendBuffer.WriteUInt8(C_Sequence_RequestResend);
              IOSendBuffer.WriteUInt32(SequenceNumberOnReceivedCounter);
            end;
          Break;
        end;
      ExtractDest.WritePtr(p^.data.Memory, p^.Size);
      SequencePacketReceivedPool.Delete(SequenceNumberOnReceivedCounter);
      inc(SequenceNumberOnReceivedCounter);
    end;
end;

procedure TPeerIO.Send_Free_OnPtr(p: Pointer);
begin
  AtomDec(FOwnerFramework.Statistics[TStatisticsType.stSequencePacketMemoryOnSending], PSequencePacket(p)^.Size);
  dec(SendingSequencePacketHistoryMemory, PSequencePacket(p)^.Size);
  if SendingSequencePacketHistoryMemory < 0 then
      PrintError('SendingSequencePacketHistoryMemory overflow');
  DisposeObject(PSequencePacket(p)^.data);
  Dispose(PSequencePacket(p));
end;

procedure TPeerIO.Send_Add_OnPtr(p: Pointer);
begin
  AtomInc(FOwnerFramework.Statistics[TStatisticsType.stSequencePacketMemoryOnSending], PSequencePacket(p)^.Size);
  inc(SendingSequencePacketHistoryMemory, PSequencePacket(p)^.Size);
  if SendingSequencePacketHistoryMemory < 0 then
      PrintError('SendingSequencePacketHistoryMemory overflow');
end;

procedure TPeerIO.Received_Free_OnPtr(p: Pointer);
begin
  AtomDec(FOwnerFramework.Statistics[TStatisticsType.stSequencePacketMemoryOnReceived], PSequencePacket(p)^.Size);
  dec(SequencePacketReceivedPoolMemory, PSequencePacket(p)^.Size);
  if SequencePacketReceivedPoolMemory < 0 then
      PrintError('SequencePacketReceivedPoolMemory overflow');

  DisposeObject(PSequencePacket(p)^.data);
  Dispose(PSequencePacket(p));
end;

procedure TPeerIO.Received_Add_OnPtr(p: Pointer);
begin
  AtomInc(FOwnerFramework.Statistics[TStatisticsType.stSequencePacketMemoryOnReceived], PSequencePacket(p)^.Size);
  inc(SequencePacketReceivedPoolMemory, PSequencePacket(p)^.Size);
  if SequencePacketReceivedPoolMemory < 0 then
      PrintError('SequencePacketReceivedPoolMemory overflow');
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

procedure TPeerIO.BeginSend;
begin
  if BeginSendState then
      PrintError('illegal BeginSend!');
  BeginSendState := True;
  SendSequencePacketBegin;
end;

procedure TPeerIO.Send(const buff: PByte; siz: NativeInt);
begin
  SendSequencePacket(buff, siz);
end;

procedure TPeerIO.EndSend;
begin
  if not BeginSendState then
      PrintError('illegal EndSend!');
  BeginSendState := False;
  SendSequencePacketEnd;
end;

procedure TPeerIO.SendInteger(v: Integer);
begin
  Send(@v, C_Integer_Size);
end;

procedure TPeerIO.SendCardinal(v: Cardinal);
begin
  Send(@v, C_Cardinal_Size);
end;

procedure TPeerIO.SendInt64(v: Int64);
begin
  Send(@v, C_Int64_Size);
end;

procedure TPeerIO.SendByte(v: Byte);
begin
  Send(@v, C_Byte_Size);
end;

procedure TPeerIO.SendWord(v: Word);
begin
  Send(@v, C_Word_Size);
end;

procedure TPeerIO.SendVerifyCode(buff: Pointer; siz: NativeInt);
var
  headBuff: array [0 .. 2] of Byte;
  Code: TBytes;
begin
  GenerateHashCode(FOwnerFramework.FHashSecurity, buff, siz, Code);

  headBuff[0] := Byte(FOwnerFramework.FHashSecurity);
  PWORD(@headBuff[1])^ := Length(Code);
  Send(@headBuff[0], 3);
  if Length(Code) > 0 then
      Send(@Code[0], Length(Code));
end;

procedure TPeerIO.SendEncryptBuffer(buff: PByte; siz: NativeInt; cs: TCipherSecurity);
begin
  SendByte(Byte(cs));
  Encrypt(cs, buff, siz, FCipherKey, True);
  Send(buff, siz);
end;

procedure TPeerIO.SendEncryptMemoryStream(Stream: TMemoryStream64; cs: TCipherSecurity);
begin
  SendEncryptBuffer(Stream.Memory, Stream.Size, cs);
end;

procedure TPeerIO.InternalSendConsoleBuff(buff: TMemoryStream64; cs: TCipherSecurity);
begin
  BeginSend;
  SendCardinal(FHeadToken);
  SendByte(Byte(FConsoleToken));
  SendCardinal(Cardinal(buff.Size));

  SendVerifyCode(buff.Memory, buff.Size);
  SendEncryptMemoryStream(buff, cs);
  SendCardinal(FTailToken);
  EndSend;
end;

procedure TPeerIO.InternalSendStreamBuff(buff: TMemoryStream64; cs: TCipherSecurity);
begin
  BeginSend;
  SendCardinal(FHeadToken);
  SendByte(Byte(FStreamToken));
  SendCardinal(Cardinal(buff.Size));

  SendVerifyCode(buff.Memory, buff.Size);
  SendEncryptMemoryStream(buff, cs);
  SendCardinal(FTailToken);
  EndSend;
end;

procedure TPeerIO.InternalSendDirectConsoleBuff(buff: TMemoryStream64; cs: TCipherSecurity);
begin
  BeginSend;
  SendCardinal(FHeadToken);
  SendByte(Byte(FDirectConsoleToken));
  SendCardinal(Cardinal(buff.Size));

  SendVerifyCode(buff.Memory, buff.Size);
  SendEncryptMemoryStream(buff, cs);
  SendCardinal(FTailToken);
  EndSend;
end;

procedure TPeerIO.InternalSendDirectStreamBuff(buff: TMemoryStream64; cs: TCipherSecurity);
begin
  BeginSend;
  SendCardinal(FHeadToken);
  SendByte(Byte(FDirectStreamToken));
  SendCardinal(Cardinal(buff.Size));

  SendVerifyCode(buff.Memory, buff.Size);
  SendEncryptMemoryStream(buff, cs);
  SendCardinal(FTailToken);
  EndSend;
end;

procedure TPeerIO.InternalSendBigStreamHeader(Cmd: SystemString; streamSiz: Int64);
var
  buff: TBytes;
begin
  BeginSend;
  SendCardinal(FHeadToken);
  SendByte(FBigStreamToken);
  SendInt64(streamSiz);
  buff := TPascalString(Cmd).Bytes;
  SendCardinal(Cardinal(Length(buff)));
  Send(@buff[0], Length(buff));
  SetLength(buff, 0);
  SendCardinal(FTailToken);
  EndSend;
end;

procedure TPeerIO.InternalSendBigStreamBuff(var Queue: TQueueData);
var
  StartPos, EndPos: Int64;
  tmpPos: Int64;
  j: Int64;
  num: Int64;
  Rest: Int64;
  BigStream_Chunk: PByte;
begin
  InternalSendBigStreamHeader(Queue.Cmd, Queue.BigStream.Size - Queue.BigStreamStartPos);

  StartPos := Queue.BigStreamStartPos;
  EndPos := Queue.BigStream.Size;
  tmpPos := StartPos;
  { Calculate number of full chunks that will fit into the buffer }
  num := (EndPos - StartPos) div C_BigStream_ChunkSize;
  { Calculate remaining bytes }
  Rest := (EndPos - StartPos) mod C_BigStream_ChunkSize;
  { init buffer }
  BigStream_Chunk := GetMemory(C_BigStream_ChunkSize);
  { Process full chunks }
  j := 0;
  while j < num do
    begin
      if not Connected then
          exit;

      Queue.BigStream.Position := tmpPos;
      Queue.BigStream.read(BigStream_Chunk^, C_BigStream_ChunkSize);
      inc(tmpPos, C_BigStream_ChunkSize);

      SendBigStreamMiniPacket(BigStream_Chunk, C_BigStream_ChunkSize);

      { peer fragment > C_BigStream_ChunkSize }
      if Queue.BigStream.Size - tmpPos > C_BigStream_ChunkSize then
        begin
          FBigStreamSending := Queue.BigStream;
          FBigStreamSendCurrentPos := tmpPos;
          FBigStreamSendDoneTimeFree := Queue.DoneAutoFree;
          Queue.BigStream := nil;
          FreeMemory(BigStream_Chunk);

          if Assigned(FOwnerFramework.FOnBigStreamInterface) then
            begin
              FOwnerFramework.FOnBigStreamInterface.BeginStream(Self, FBigStreamSending.Size);
              FOwnerFramework.FOnBigStreamInterface.Process(Self, FBigStreamSending.Size, FBigStreamSendCurrentPos);
            end;

          exit;
        end;
      inc(j);
    end;

  { Process remaining bytes }
  if Rest > 0 then
    begin
      Queue.BigStream.Position := tmpPos;
      Queue.BigStream.read(BigStream_Chunk^, Rest);
      tmpPos := tmpPos + Rest;

      SendBigStreamMiniPacket(BigStream_Chunk, Rest);
    end;
  FreeMemory(BigStream_Chunk);
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
  SendCardinal(Cardinal(Length(buff)));
  Send(@buff[0], Length(buff));
  SetLength(buff, 0);
  SendCardinal(FTailToken);
end;

procedure TPeerIO.InternalSendCompleteBufferBuff(var Queue: TQueueData);
var
  sour, dest: TMemoryStream64;
begin
  BeginSend;
  if FOwnerFramework.FCompleteBufferCompressed then
    begin
      sour := TMemoryStream64.Create;
      sour.SetPointerWithProtectedMode(Queue.buffer, Queue.BufferSize);
      dest := TMemoryStream64.Create;
      ParallelCompressStream(scmZLIB_Fast, sour, dest);
      InternalSendCompleteBufferHeader(Queue.Cmd, Queue.BufferSize, dest.Size);
      Send(dest.Memory, dest.Size);
      DisposeObject(sour);
      DisposeObject(dest);
    end
  else
    begin
      InternalSendCompleteBufferHeader(Queue.Cmd, Queue.BufferSize, 0);
      Send(Queue.buffer, Queue.BufferSize);
    end;
  EndSend;
end;

procedure TPeerIO.InternalSendBigStreamFragmentSignal;
begin
  BeginSend;
  SendCardinal(FHeadToken);
  SendByte(FBigStreamReceiveFragmentSignal);
  SendCardinal(FTailToken);
  EndSend;
end;

procedure TPeerIO.InternalSendBigStreamDoneSignal;
begin
  BeginSend;
  SendCardinal(FHeadToken);
  SendByte(FBigStreamReceiveDoneSignal);
  SendCardinal(FTailToken);
  EndSend;
end;

procedure TPeerIO.SendBigStreamMiniPacket(buff: PByte; Size: NativeInt);
var
  head: TBigStreamFragmentHead;
  sourStream, destStream: TMemoryStream64;
begin
  BeginSend;

  if OwnerFramework.SendDataCompressed then
    begin
      sourStream := TMemoryStream64.Create;
      sourStream.SetPointerWithProtectedMode(buff, Size);
      destStream := TMemoryStream64.CustomCreate(8192);
      ParallelCompressStream(scmZLIB_Fast, sourStream, destStream);

      head.Size := destStream.Size;
      head.Compressed := True;

      Send(@head, SizeOf(head));
      Send(destStream.Memory, destStream.Size);

      DisposeObject(sourStream);
      DisposeObject(destStream);
    end
  else
    begin
      head.Size := Size;
      head.Compressed := False;

      Send(@head, SizeOf(head));
      Send(buff, Size);
    end;

  EndSend;
end;

procedure TPeerIO.Sync_InternalSendResultData;
begin
  if FResultDataBuffer.Size > 0 then
    begin
      BeginSend;
      Send(FResultDataBuffer.Memory, FResultDataBuffer.Size);
      FResultDataBuffer.Clear;
      EndSend;
    end;
end;

procedure TPeerIO.Sync_InternalSendConsoleCmd;
var
  df: TDataFrameEngine;
  Stream: TMemoryStream64;
begin
  df := TDataFrameEngine.Create;
  Stream := TMemoryStream64.Create;

  df.WriteString(FSyncPick^.Cmd);
  df.WriteString(FSyncPick^.ConsoleData);

  if FOwnerFramework.FSendDataCompressed then
      df.EncodeAsSelectCompressor(Stream, True)
  else
      df.EncodeTo(Stream, True);

  InternalSendConsoleBuff(Stream, FSyncPick^.Cipher);

  DisposeObject(df);
  DisposeObject(Stream);

  if FOwnerFramework.FSendDataCompressed then
      AtomInc(FOwnerFramework.Statistics[TStatisticsType.stCompress]);

  PrintCommand('internal send console: %s', FSyncPick^.Cmd);
end;

procedure TPeerIO.Sync_InternalSendStreamCmd;
var
  df: TDataFrameEngine;
  Stream: TMemoryStream64;
begin
  df := TDataFrameEngine.Create;
  Stream := TMemoryStream64.Create;

  df.WriteString(FSyncPick^.Cmd);
  df.WriteStream(FSyncPick^.StreamData);

  if FOwnerFramework.FSendDataCompressed then
      df.EncodeAsSelectCompressor(Stream, True)
  else
      df.EncodeTo(Stream, True);

  InternalSendStreamBuff(Stream, FSyncPick^.Cipher);

  DisposeObject(df);
  DisposeObject(Stream);

  if FOwnerFramework.FSendDataCompressed then
      AtomInc(FOwnerFramework.Statistics[TStatisticsType.stCompress]);

  PrintCommand('internal send stream: %s', FSyncPick^.Cmd);
end;

procedure TPeerIO.Sync_InternalSendDirectConsoleCmd;
var
  df: TDataFrameEngine;
  Stream: TMemoryStream64;
begin
  df := TDataFrameEngine.Create;
  Stream := TMemoryStream64.Create;

  df.WriteString(FSyncPick^.Cmd);
  df.WriteString(FSyncPick^.ConsoleData);

  if FOwnerFramework.FSendDataCompressed then
      df.EncodeAsSelectCompressor(Stream, True)
  else
      df.EncodeTo(Stream, True);

  InternalSendDirectConsoleBuff(Stream, FSyncPick^.Cipher);

  DisposeObject(df);
  DisposeObject(Stream);

  if FOwnerFramework.FSendDataCompressed then
      AtomInc(FOwnerFramework.Statistics[TStatisticsType.stCompress]);

  PrintCommand('internal send direct console: %s', FSyncPick^.Cmd);
end;

procedure TPeerIO.Sync_InternalSendDirectStreamCmd;
var
  df: TDataFrameEngine;
  Stream: TMemoryStream64;
begin
  df := TDataFrameEngine.Create;
  Stream := TMemoryStream64.Create;

  df.WriteString(FSyncPick^.Cmd);
  df.WriteStream(FSyncPick^.StreamData);

  if FOwnerFramework.FSendDataCompressed then
      df.EncodeAsSelectCompressor(Stream, True)
  else
      df.EncodeTo(Stream, True);

  InternalSendDirectStreamBuff(Stream, FSyncPick^.Cipher);

  DisposeObject(df);
  DisposeObject(Stream);

  if FOwnerFramework.FSendDataCompressed then
      AtomInc(FOwnerFramework.Statistics[TStatisticsType.stCompress]);

  PrintCommand('internal send direct stream: %s', FSyncPick^.Cmd);
end;

procedure TPeerIO.Sync_InternalSendBigStreamCmd;
begin
  InternalSendBigStreamBuff(FSyncPick^);
  AtomInc(FOwnerFramework.Statistics[TStatisticsType.stExecBigStream]);

  PrintCommand('internal send bigstream: %s', FSyncPick^.Cmd);
end;

procedure TPeerIO.Sync_InternalSendCompleteBufferCmd;
begin
  InternalSendCompleteBufferBuff(FSyncPick^);
  AtomInc(FOwnerFramework.Statistics[TStatisticsType.stExecCompleteBuffer]);

  PrintCommand('internal send complete buffer: %s', FSyncPick^.Cmd);
end;

procedure TPeerIO.Sync_ExecuteConsole;
var
  d: TTimeTick;
begin
  FReceiveCommandRuning := True;
  PrintCommand('execute console: %s', FInCmd);

  d := GetTimeTick;
  FOwnerFramework.ExecuteConsole(Self, FInCmd, FInText, FOutText);
  FReceiveCommandRuning := False;

  FOwnerFramework.CmdMaxExecuteConsumeStatistics.SetMax(FInCmd, GetTimeTick - d);

  AtomInc(FOwnerFramework.Statistics[TStatisticsType.stExecConsole]);
  FOwnerFramework.CmdRecvStatistics.IncValue(FInCmd, 1);
end;

procedure TPeerIO.Sync_ExecuteStream;
var
  d: TTimeTick;
begin
  FReceiveCommandRuning := True;
  PrintCommand('execute stream: %s', FInCmd);

  d := GetTimeTick;
  FOwnerFramework.ExecuteStream(Self, FInCmd, FInDataFrame, FOutDataFrame);
  FReceiveCommandRuning := False;

  FOwnerFramework.CmdMaxExecuteConsumeStatistics.SetMax(FInCmd, GetTimeTick - d);

  AtomInc(FOwnerFramework.Statistics[TStatisticsType.stExecStream]);
  FOwnerFramework.CmdRecvStatistics.IncValue(FInCmd, 1);
end;

procedure TPeerIO.Sync_ExecuteDirectConsole;
var
  d: TTimeTick;
begin
  FReceiveCommandRuning := True;
  PrintCommand('execute direct console: %s', FInCmd);

  d := GetTimeTick;
  FOwnerFramework.ExecuteDirectConsole(Self, FInCmd, FInText);
  FReceiveCommandRuning := False;

  FOwnerFramework.CmdMaxExecuteConsumeStatistics.SetMax(FInCmd, GetTimeTick - d);

  AtomInc(FOwnerFramework.Statistics[TStatisticsType.stExecDirestConsole]);
  FOwnerFramework.CmdRecvStatistics.IncValue(FInCmd, 1);
end;

procedure TPeerIO.Sync_ExecuteDirectStream;
var
  d: TTimeTick;
begin
  FReceiveCommandRuning := True;
  PrintCommand('execute direct stream: %s', FInCmd);

  d := GetTimeTick;
  FOwnerFramework.ExecuteDirectStream(Self, FInCmd, FInDataFrame);
  FReceiveCommandRuning := False;

  FOwnerFramework.CmdMaxExecuteConsumeStatistics.SetMax(FInCmd, GetTimeTick - d);

  AtomInc(FOwnerFramework.Statistics[TStatisticsType.stExecDirestStream]);
  FOwnerFramework.CmdRecvStatistics.IncValue(FInCmd, 1);
end;

procedure TPeerIO.Sync_SendConsoleResult;
var
  buff: TBytes;
begin
  BeginSend;
  buff := TPascalString(FOutText).Bytes;

  SendCardinal(FHeadToken);
  SendInteger(Length(buff));

  SendVerifyCode(@buff[0], Length(buff));

  SendEncryptBuffer(@buff[0], Length(buff), FReceiveDataCipherSecurity);
  SendCardinal(FTailToken);

  EndSend;

  AtomInc(FOwnerFramework.Statistics[TStatisticsType.stResponse]);
end;

procedure TPeerIO.Sync_SendStreamResult;
var
  m64: TMemoryStream64;
begin
  BeginSend;
  m64 := TMemoryStream64.Create;
  FOutDataFrame.EncodeTo(m64, True);

  SendCardinal(FHeadToken);
  SendInteger(m64.Size);

  SendVerifyCode(m64.Memory, m64.Size);

  SendEncryptBuffer(m64.Memory, m64.Size, FReceiveDataCipherSecurity);
  SendCardinal(FTailToken);
  DisposeObject(m64);
  EndSend;
  AtomInc(FOwnerFramework.Statistics[TStatisticsType.stResponse]);
end;

procedure TPeerIO.ExecuteDataFrame(CurrentActiveThread_: TCoreClassThread; const RecvSync, SendSync: Boolean; CommDataType: Byte; DataFrame: TDataFrameEngine);
begin
  FInCmd := DataFrame.Reader.ReadString;

  if CommDataType = FConsoleToken then
    begin
      FInText := DataFrame.Reader.ReadString;
      FOutText := '';

      FCanPauseResultSend := True;
      FReceiveTriggerRuning := True;
      IO_SyncMethod(CurrentActiveThread_, RecvSync, {$IFDEF FPC}@{$ENDIF FPC}Sync_ExecuteConsole);
      FReceiveTriggerRuning := False;
      FCanPauseResultSend := False;

      if FPauseResultSend then
        begin
          PrintCommand('pause console cmd %s Result', FInCmd);
          FCurrentPauseResultSend_CommDataType := CommDataType;
          exit;
        end;
      if not Connected then
          exit;

      PrintCommand('send console cmd %s Result data', FInCmd);
      IO_SyncMethod(CurrentActiveThread_, SendSync, {$IFDEF FPC}@{$ENDIF FPC}Sync_SendConsoleResult);
    end
  else if CommDataType = FStreamToken then
    begin
      FInDataFrame.Clear;
      FOutDataFrame.Clear;
      DataFrame.Reader.ReadDataFrame(FInDataFrame);

      FCanPauseResultSend := True;
      FReceiveTriggerRuning := True;
      IO_SyncMethod(CurrentActiveThread_, RecvSync, {$IFDEF FPC}@{$ENDIF FPC}Sync_ExecuteStream);
      FReceiveTriggerRuning := False;
      FCanPauseResultSend := False;

      if FPauseResultSend then
        begin
          PrintCommand('pause stream cmd %s Result', FInCmd);
          FCurrentPauseResultSend_CommDataType := CommDataType;
          exit;
        end;

      if not Connected then
          exit;

      PrintCommand('send stream cmd %s Result data', FInCmd);
      IO_SyncMethod(CurrentActiveThread_, SendSync, {$IFDEF FPC}@{$ENDIF FPC}Sync_SendStreamResult);
    end
  else if CommDataType = FDirectConsoleToken then
    begin
      FInText := DataFrame.Reader.ReadString;

      FReceiveTriggerRuning := True;
      IO_SyncMethod(CurrentActiveThread_, RecvSync, {$IFDEF FPC}@{$ENDIF FPC}Sync_ExecuteDirectConsole);
      FReceiveTriggerRuning := False;
    end
  else if CommDataType = FDirectStreamToken then
    begin
      FInDataFrame.Clear;
      FOutDataFrame.Clear;
      DataFrame.Reader.ReadDataFrame(FInDataFrame);

      FReceiveTriggerRuning := True;
      IO_SyncMethod(CurrentActiveThread_, RecvSync, {$IFDEF FPC}@{$ENDIF FPC}Sync_ExecuteDirectStream);
      FReceiveTriggerRuning := False;
    end;
end;

procedure TPeerIO.Sync_ExecuteBigStream;
var
  d: TTimeTick;
begin
  FReceiveCommandRuning := True;
  d := GetTimeTick;
  FOwnerFramework.ExecuteBigStream(Self, FBigStreamCmd, FSyncBigStreamReceive, FBigStreamTotal, FBigStreamCompleted);
  FReceiveCommandRuning := False;
  FOwnerFramework.CmdMaxExecuteConsumeStatistics.SetMax(FInCmd, GetTimeTick - d);

  if FBigStreamTotal = FBigStreamCompleted then
    begin
      InternalSendBigStreamDoneSignal();
      { do stream state }
      if Assigned(FOwnerFramework.FOnBigStreamInterface) then
          FOwnerFramework.FOnBigStreamInterface.EndStream(Self, FBigStreamTotal);

      FOwnerFramework.CmdRecvStatistics.IncValue(FBigStreamCmd, 1);
      PrintCommand('Big Stream complete: %s', FBigStreamCmd);
    end
  else
    begin
      { do stream state }
      if Assigned(FOwnerFramework.FOnBigStreamInterface) then
          FOwnerFramework.FOnBigStreamInterface.Process(Self, FBigStreamTotal, FBigStreamCompleted);
    end;
end;

function TPeerIO.ReceivedBigStreamFragment(CurrentActiveThread_: TCoreClassThread; const Sync: Boolean): Boolean;
var
  head: TBigStreamFragmentHead;
  np: Int64;
  buff, destBuff: TMemoryStream64;
  leftSize: Int64;
begin
  Result := False;
  if (FReceivedBuffer.Size - FReceivedBuffer.Position < SizeOf(head)) then
      exit;
  FReceivedBuffer.ReadPtr(@head, SizeOf(head));

  if (FReceivedBuffer.Size - FReceivedBuffer.Position < head.Size) then
      exit;

  np := FReceivedBuffer.Position + head.Size;

  buff := TMemoryStream64.CustomCreate(8192);
  buff.WritePtr(FReceivedBuffer.PositionAsPtr, head.Size);
  buff.Position := 0;

  if head.Compressed then
    begin
      destBuff := TMemoryStream64.CustomCreate(8192);
      ParallelDecompressStream(buff, destBuff);
      DisposeObject(buff);
      buff := destBuff;
      buff.Position := 0;
    end;

  leftSize := FBigStreamTotal - FBigStreamCompleted;
  if leftSize > buff.Size then
    begin
      InternalSendBigStreamFragmentSignal;

      FBigStreamCompleted := FBigStreamCompleted + buff.Size;
      FSyncBigStreamReceive := buff;

      { sync }
      IO_SyncMethod(CurrentActiveThread_, Sync, {$IFDEF FPC}@{$ENDIF FPC}Sync_ExecuteBigStream);
    end
  else
    begin
      FBigStreamCompleted := FBigStreamTotal;
      FSyncBigStreamReceive := buff;

      { sync }
      IO_SyncMethod(CurrentActiveThread_, Sync, {$IFDEF FPC}@{$ENDIF FPC}Sync_ExecuteBigStream);

      Result := True;

      FBigStreamTotal := 0;
      FBigStreamCompleted := 0;
      FBigStreamCmd := '';
      FBigStreamReceiveProcessing := False;
    end;

  FSyncBigStreamReceive := nil;
  DisposeObject(buff);

  { replace fragment buffer }
  if FReceivedBuffer.Size - np > 0 then
    begin
      buff := TMemoryStream64.CustomCreate(FReceivedBuffer.Delta);
      buff.WritePtr(FReceivedBuffer.PositionAsPtr(np), FReceivedBuffer.Size - np);
      DisposeObject(FReceivedBuffer);
      FReceivedBuffer := buff;
      FReceivedBuffer.Position := 0;
    end
  else
      FReceivedBuffer.Clear;
end;

procedure TPeerIO.Sync_ExecuteCompleteBuffer;
var
  d: TTimeTick;
begin
  if FOwnerFramework.FSyncOnCompleteBuffer then
    begin
      FReceiveCommandRuning := True;
      d := GetTimeTick;

      FOwnerFramework.ExecuteCompleteBuffer(Self, FCompleteBufferCmd, FCompleteBufferReceiveStream.Memory, FCompleteBufferReceiveStream.Size);

      FReceiveCommandRuning := False;
      FOwnerFramework.CmdMaxExecuteConsumeStatistics.SetMax(FInCmd, GetTimeTick - d);

      FOwnerFramework.CmdRecvStatistics.IncValue(FCompleteBufferCmd, 1);
      PrintCommand('execute complete buffer: %s', FCompleteBufferCmd);
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

function TPeerIO.FillCompleteBufferBuffer(CurrentActiveThread_: TCoreClassThread; const Sync: Boolean): Boolean;
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

      tmpStream := TMemoryStream64.CustomCreate(FReceivedBuffer.Delta);
      if FReceivedBuffer.Size - leftSize > 0 then
          tmpStream.WritePtr(FReceivedBuffer.PositionAsPtr(leftSize), FReceivedBuffer.Size - leftSize);
      DisposeObject(FReceivedBuffer);
      FReceivedBuffer := tmpStream;

      if FCompleteBufferCompressedSize > 0 then
        begin
          dest := TMemoryStream64.Create;
          ParallelDecompressStream(FCompleteBufferReceiveStream, dest);
          DisposeObject(FCompleteBufferReceiveStream);
          dest.Position := 0;
          FCompleteBufferReceiveStream := dest;
        end;

      IO_SyncMethod(CurrentActiveThread_, Sync, {$IFDEF FPC}@{$ENDIF FPC}Sync_ExecuteCompleteBuffer);
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

function TPeerIO.FillWaitOnResultBuffer(CurrentActiveThread_: TCoreClassThread; const RecvSync, SendSync: Boolean): Boolean;
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

  { 0: head token }
  if (FReceivedBuffer.Size - FReceivedBuffer.Position < C_Cardinal_Size) then
      exit;
  FReceivedBuffer.read(dHead, C_Cardinal_Size);
  if dHead <> FHeadToken then
    begin
      Print('Header Illegal');
      DelayClose();
      exit;
    end;

  { 1: data len }
  if (FReceivedBuffer.Size - FReceivedBuffer.Position < C_Integer_Size) then
      exit;
  FReceivedBuffer.read(dSize, C_Integer_Size);

  { 2:verify code header }
  if (FReceivedBuffer.Size - FReceivedBuffer.Position < 3) then
      exit;
  FReceivedBuffer.read(dHashSecurity, C_Byte_Size);
  FReceivedBuffer.read(dHashSiz, C_Word_Size);

  { 3:verify code body }
  if (FReceivedBuffer.Size - FReceivedBuffer.Position < dHashSiz) then
      exit;
  SetLength(dHash, dHashSiz);
  if Length(dHash) > 0 then
      FReceivedBuffer.read(dHash[0], dHashSiz);

  { 4: use Encrypt state }
  if (FReceivedBuffer.Size - FReceivedBuffer.Position < C_Byte_Size) then
      exit;
  FReceivedBuffer.read(dCipherSecurity, C_Byte_Size);

  { 5:process buff and tail token }
  if (FReceivedBuffer.Size - FReceivedBuffer.Position < dSize + C_Cardinal_Size) then
      exit;
  SetLength(buff, dSize);
  if Length(buff) > 0 then
      FReceivedBuffer.read(buff[0], dSize);

  { 6: tail token }
  FReceivedBuffer.read(dTail, C_Cardinal_Size);
  if dTail <> FTailToken then
    begin
      Print('tail token error!');
      DelayClose();
      exit;
    end;

  FReceiveDataCipherSecurity := TCipherSecurity(dCipherSecurity);

  try
    if Length(buff) > 0 then
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

  { stripped stream }
  tmpStream := TMemoryStream64.CustomCreate(FReceivedBuffer.Delta);
  if FReceivedBuffer.Size - FReceivedBuffer.Position > 0 then
      tmpStream.CopyFrom(FReceivedBuffer, FReceivedBuffer.Size - FReceivedBuffer.Position);
  DisposeObject(FReceivedBuffer);
  FReceivedBuffer := tmpStream;
  FReceivedBuffer.Position := 0;

  if Assigned(FCurrentQueueData^.OnConsoleMethod) or
    Assigned(FCurrentQueueData^.OnConsoleParamMethod) or
    Assigned(FCurrentQueueData^.OnConsoleProc) or
    Assigned(FCurrentQueueData^.OnConsoleParamProc) then
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

      IO_SyncMethod(CurrentActiveThread_, RecvSync, {$IFDEF FPC}@{$ENDIF FPC}Sync_ExecuteResult);

      AtomInc(FOwnerFramework.Statistics[TStatisticsType.stResponse]);
    end
  else
    if Assigned(FCurrentQueueData^.OnStreamMethod) or
    Assigned(FCurrentQueueData^.OnStreamParamMethod) or
    Assigned(FCurrentQueueData^.OnStreamProc) or
    Assigned(FCurrentQueueData^.OnStreamParamProc) then
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

      IO_SyncMethod(CurrentActiveThread_, RecvSync, {$IFDEF FPC}@{$ENDIF FPC}Sync_ExecuteResult);

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
  if not Connected then
      exit;
  AtomInc(FOwnerFramework.Statistics[TStatisticsType.stReceiveSize], siz);

  LockIO;
  try
    if FReceiveProcessing or FAllSendProcessing then
        FReceivedAbort := not FillSequencePacketTo(buff, siz, FReceivedBuffer_Busy)
    else
      begin
        FReceivedBuffer.Position := FReceivedBuffer.Size;
        if FReceivedBuffer_Busy.Size > 0 then
          begin
            FReceivedBuffer.WritePtr(FReceivedBuffer_Busy.Memory, FReceivedBuffer_Busy.Size);
            FReceivedBuffer_Busy.Clear;
          end;
        FReceivedAbort := not FillSequencePacketTo(buff, siz, FReceivedBuffer);
      end;
  finally
      UnLockIO;
  end;
end;

procedure TPeerIO.InternalProcessReceiveBuffer(const CurrentActiveThread_: TCoreClassThread; const RecvSync, SendSync: Boolean);
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

  { continue send }
  BigStream_Chunk: PByte;
  BigStream_RealChunkSize: Integer;
  BigStream_SendDone: Boolean;
begin
  if FReceivedAbort then
    begin
      DelayClose;
      exit;
    end;
  if FAllSendProcessing or
    FReceiveProcessing or
    FPauseResultSend or
    (FResultDataBuffer.Size > 0) or
    FReceiveTriggerRuning then
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
            rState := FillWaitOnResultBuffer(CurrentActiveThread_, RecvSync, SendSync);

            if rState then
                Continue
            else
                Break;
          end;

        if FBigStreamReceiveProcessing then
          begin
            rState := ReceivedBigStreamFragment(CurrentActiveThread_, RecvSync);

            if rState then
                Continue
            else
                Break;
          end;

        if FCompleteBufferReceiveProcessing then
          begin
            rState := FillCompleteBufferBuffer(CurrentActiveThread_, RecvSync);

            if rState then
                Continue
            else
                Break;
          end;

        { 0: head token }
        if (FReceivedBuffer.Size - FReceivedBuffer.Position < C_Cardinal_Size + C_Byte_Size) then
            Break;
        FReceivedBuffer.read(dHead, C_Cardinal_Size);
        if dHead <> FHeadToken then
          begin
            BreakAndDisconnect := True;
            Break;
          end;
        { 1: data type }
        FReceivedBuffer.read(dID, C_Byte_Size);

        { done signal }
        if dID = FBigStreamReceiveDoneSignal then
          begin
            { 2: process tail token }
            FReceivedBuffer.read(dTail, C_Cardinal_Size);
            if dTail <> FTailToken then
              begin
                PrintError('tail error!');
                BreakAndDisconnect := True;
                Break;
              end;

            { stripped stream }
            tmpStream := TMemoryStream64.CustomCreate(FReceivedBuffer.Delta);
            if FReceivedBuffer.Size - FReceivedBuffer.Position > 0 then
                tmpStream.CopyFrom(FReceivedBuffer, FReceivedBuffer.Size - FReceivedBuffer.Position);
            DisposeObject(FReceivedBuffer);
            FReceivedBuffer := tmpStream;

            { done }
            FWaitBigStreamReceiveDoneSignal := False;
          end
        else if dID = FBigStreamReceiveFragmentSignal then
          begin
            { 2: process tail token }
            FReceivedBuffer.read(dTail, C_Cardinal_Size);
            if dTail <> FTailToken then
              begin
                PrintError('tail error!');
                BreakAndDisconnect := True;
                Break;
              end;

            { stripped stream }
            tmpStream := TMemoryStream64.CustomCreate(FReceivedBuffer.Delta);
            if FReceivedBuffer.Size - FReceivedBuffer.Position > 0 then
                tmpStream.CopyFrom(FReceivedBuffer, FReceivedBuffer.Size - FReceivedBuffer.Position);
            DisposeObject(FReceivedBuffer);
            FReceivedBuffer := tmpStream;

            { save }
            if (FBigStreamSending <> nil) then
              begin
                BigStream_RealChunkSize := C_BigStream_ChunkSize;

                BigStream_SendDone := FBigStreamSending.Size - FBigStreamSendCurrentPos <= BigStream_RealChunkSize;

                if BigStream_SendDone then
                    BigStream_RealChunkSize := FBigStreamSending.Size - FBigStreamSendCurrentPos;

                BigStream_Chunk := GetMemory(BigStream_RealChunkSize);

                try
                  FBigStreamSending.Position := FBigStreamSendCurrentPos;
                  FBigStreamSending.read(BigStream_Chunk^, BigStream_RealChunkSize);
                except
                  PrintError('BigStream IO read error!');
                  BreakAndDisconnect := True;
                  Break;
                end;

                try
                  SendBigStreamMiniPacket(BigStream_Chunk, BigStream_RealChunkSize);
                  FreeMemory(BigStream_Chunk);
                  AtomInc(FBigStreamSendCurrentPos, BigStream_RealChunkSize);
                except
                  PrintError('BigStream send error!');
                  BreakAndDisconnect := True;
                  Break;
                end;

                if BigStream_SendDone then
                  begin
                    if Assigned(FOwnerFramework.FOnBigStreamInterface) then
                        FOwnerFramework.FOnBigStreamInterface.EndStream(Self, FBigStreamSending.Size);

                    if FBigStreamSendDoneTimeFree then
                        DisposeObject(FBigStreamSending);
                    FBigStreamSending := nil;
                    FBigStreamSendCurrentPos := -1;
                    FBigStreamSendDoneTimeFree := False;
                  end
                else
                  begin
                    if Assigned(FOwnerFramework.FOnBigStreamInterface) then
                        FOwnerFramework.FOnBigStreamInterface.Process(Self, FBigStreamSending.Size, FBigStreamSendCurrentPos);
                  end;
              end;
          end
        else if FWaitBigStreamReceiveDoneSignal then
          begin
            PrintError('BigStream error: FWaitBigStreamReceiveDoneSignal is True');
            BreakAndDisconnect := True;
            Break;
          end
        else if dID = FBigStreamToken then
          begin
            { 2:stream size }
            if (FReceivedBuffer.Size - FReceivedBuffer.Position < C_Int64_Size) then
                Break;
            FReceivedBuffer.read(Total, C_Int64_Size);

            { 3:command len }
            if (FReceivedBuffer.Size - FReceivedBuffer.Position < C_Cardinal_Size) then
                Break;
            FReceivedBuffer.read(dSize, C_Cardinal_Size);

            { 4:command and tial token }
            if (FReceivedBuffer.Size - FReceivedBuffer.Position < dSize + C_Cardinal_Size) then
                Break;
            SetLength(buff, dSize);
            if dSize > 0 then
                FReceivedBuffer.read(buff[0], dSize);

            { 5: process tail token }
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

            { stripped stream }
            tmpStream := TMemoryStream64.CustomCreate(FReceivedBuffer.Delta);
            if FReceivedBuffer.Size - FReceivedBuffer.Position > 0 then
                tmpStream.CopyFrom(FReceivedBuffer, FReceivedBuffer.Size - FReceivedBuffer.Position);
            DisposeObject(FReceivedBuffer);
            FReceivedBuffer := tmpStream;

            { do stream state }
            if Assigned(FOwnerFramework.FOnBigStreamInterface) then
                FOwnerFramework.FOnBigStreamInterface.BeginStream(Self, FBigStreamTotal);

            AtomInc(FOwnerFramework.Statistics[TStatisticsType.stReceiveBigStream]);
          end
        else if dID = FCompleteBufferToken then
          begin
            { 2:complete buff size }
            if (FReceivedBuffer.Size - FReceivedBuffer.Position < C_Cardinal_Size * 3) then
                Break;
            FReceivedBuffer.read(sourSiz, C_Cardinal_Size);
            FReceivedBuffer.read(compSiz, C_Cardinal_Size);
            FReceivedBuffer.read(dSize, C_Cardinal_Size);

            { 3:command and tial token }
            if (FReceivedBuffer.Size - FReceivedBuffer.Position < dSize + C_Cardinal_Size) then
                Break;
            SetLength(buff, dSize);
            if Length(buff) > 0 then
                FReceivedBuffer.read(buff[0], dSize);

            { 4: process tail token }
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
            FCompleteBufferReceiveStream.Delta := 1024 * 64;
            SetLength(buff, 0);

            { stripped stream }
            tmpStream := TMemoryStream64.CustomCreate(FReceivedBuffer.Delta);
            if FReceivedBuffer.Size - FReceivedBuffer.Position > 0 then
                tmpStream.CopyFrom(FReceivedBuffer, FReceivedBuffer.Size - FReceivedBuffer.Position);
            DisposeObject(FReceivedBuffer);
            FReceivedBuffer := tmpStream;

            AtomInc(FOwnerFramework.Statistics[TStatisticsType.stReceiveCompleteBuffer]);
          end
        else if dID in [FConsoleToken, FStreamToken, FDirectConsoleToken, FDirectStreamToken] then
          begin
            { 2: size }
            if (FReceivedBuffer.Size - FReceivedBuffer.Position < C_Cardinal_Size) then
                Break;
            FReceivedBuffer.read(dSize, C_Cardinal_Size);

            { 3:verify code header }
            if (FReceivedBuffer.Size - FReceivedBuffer.Position < 3) then
                Break;
            FReceivedBuffer.read(dHashSecurity, C_Byte_Size);
            FReceivedBuffer.read(dHashSiz, C_Word_Size);

            { 4:verify code body }
            if (FReceivedBuffer.Size - FReceivedBuffer.Position < dHashSiz) then
                Break;
            SetLength(dHash, dHashSiz);
            if Length(dHash) > 0 then
                FReceivedBuffer.read(dHash[0], dHashSiz);

            { 5: Encrypt style }
            if (FReceivedBuffer.Size - FReceivedBuffer.Position < C_Byte_Size) then
                Break;
            FReceivedBuffer.read(dCipherSecurity, C_Byte_Size);

            { 6: process stream }
            if (FReceivedBuffer.Size - FReceivedBuffer.Position < dSize + C_Cardinal_Size) then
                Break;
            tmpStream := TMemoryStream64.Create;
            tmpStream.SetPointerWithProtectedMode(FReceivedBuffer.PositionAsPtr, dSize);
            FReceivedBuffer.Position := FReceivedBuffer.Position + dSize;

            { 7: process tail token }
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
              DisposeObject(df);
              BreakAndDisconnect := True;
              Break;
            end;
            DisposeObject(tmpStream);

            { stripped stream }
            tmpStream := TMemoryStream64.CustomCreate(FReceivedBuffer.Delta);
            if FReceivedBuffer.Size - FReceivedBuffer.Position > 0 then
                tmpStream.CopyFrom(FReceivedBuffer, FReceivedBuffer.Size - FReceivedBuffer.Position);
            DisposeObject(FReceivedBuffer);
            FReceivedBuffer := tmpStream;

            try
                ExecuteDataFrame(CurrentActiveThread_, RecvSync, SendSync, dID, df);
            except
              PrintError('run ExecuteDataFrame error!');
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
        FillRecvBuffer(CurrentActiveThread_, RecvSync, SendSync)
    else
        ProcessAllSendCmd(CurrentActiveThread_, RecvSync, SendSync);
  end;
end;

procedure TPeerIO.InternalProcessAllSendCmd(const CurrentActiveThread_: TCoreClassThread; const RecvSync, SendSync: Boolean);
var
  p: PQueueData;
begin
  if FAllSendProcessing or
    FReceiveProcessing or
    FWaitOnResult or
    FBigStreamReceiveProcessing or
    (FBigStreamSending <> nil) or
    FReceiveTriggerRuning or
    FWaitBigStreamReceiveDoneSignal then
    begin
      exit;
    end;

  FAllSendProcessing := True;

  LockIO;

  if FResultDataBuffer.Size > 0 then
    begin
      IO_SyncMethod(CurrentActiveThread_, SendSync, {$IFDEF FPC}@{$ENDIF FPC}Sync_InternalSendResultData);
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
              IO_SyncMethod(CurrentActiveThread_, SendSync, {$IFDEF FPC}@{$ENDIF FPC}Sync_InternalSendConsoleCmd);

              FSyncPick := nil;

              FQueueList.Delete(0);
              Break;
            end;
          qsSendStreamCMD:
            begin
              AtomInc(FOwnerFramework.Statistics[TStatisticsType.stStream]);

              FSyncPick := p;
              FWaitOnResult := True;
              IO_SyncMethod(CurrentActiveThread_, SendSync, {$IFDEF FPC}@{$ENDIF FPC}Sync_InternalSendStreamCmd);
              FSyncPick := nil;

              FQueueList.Delete(0);
              Break;
            end;
          qsSendDirectConsoleCMD:
            begin
              AtomInc(FOwnerFramework.Statistics[TStatisticsType.stDirestConsole]);

              FSyncPick := p;
              IO_SyncMethod(CurrentActiveThread_, SendSync, {$IFDEF FPC}@{$ENDIF FPC}Sync_InternalSendDirectConsoleCmd);
              FSyncPick := nil;

              DisposeQueueData(p);
              FQueueList.Delete(0);
            end;
          qsSendDirectStreamCMD:
            begin
              AtomInc(FOwnerFramework.Statistics[TStatisticsType.stDirestStream]);

              FSyncPick := p;
              IO_SyncMethod(CurrentActiveThread_, SendSync, {$IFDEF FPC}@{$ENDIF FPC}Sync_InternalSendDirectStreamCmd);
              FSyncPick := nil;

              DisposeQueueData(p);
              FQueueList.Delete(0);
            end;
          qsSendBigStream:
            begin
              AtomInc(FOwnerFramework.Statistics[TStatisticsType.stSendBigStream]);

              FSyncPick := p;
              FWaitBigStreamReceiveDoneSignal := True;
              IO_SyncMethod(CurrentActiveThread_, SendSync, {$IFDEF FPC}@{$ENDIF FPC}Sync_InternalSendBigStreamCmd);
              FSyncPick := nil;

              DisposeQueueData(p);
              FQueueList.Delete(0);

              Break;
            end;
          qsSendCompleteBuffer:
            begin
              AtomInc(FOwnerFramework.Statistics[TStatisticsType.stSendCompleteBuffer]);

              FSyncPick := p;
              IO_SyncMethod(CurrentActiveThread_, SendSync, {$IFDEF FPC}@{$ENDIF FPC}Sync_InternalSendCompleteBufferCmd);

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
        FillRecvBuffer(CurrentActiveThread_, RecvSync, SendSync);
  end;
end;

procedure TPeerIO.Sync_CheckAndTriggerFailedWaitResult;
var
  tmp: TDataFrameEngine;
begin
  if (FCurrentQueueData <> nil) and (FWaitOnResult) then
    begin
      try
        if FCurrentQueueData^.State = qsSendConsoleCMD then
          begin
            if Assigned(FCurrentQueueData^.OnConsoleFailedMethod) then
                FCurrentQueueData^.OnConsoleFailedMethod(Self, FCurrentQueueData^.Param1, FCurrentQueueData^.Param2, FCurrentQueueData^.ConsoleData);
            if Assigned(FCurrentQueueData^.OnConsoleFailedProc) then
                FCurrentQueueData^.OnConsoleFailedProc(Self, FCurrentQueueData^.Param1, FCurrentQueueData^.Param2, FCurrentQueueData^.ConsoleData);
          end
        else if FCurrentQueueData^.State = qsSendStreamCMD then
          begin
            tmp := TDataFrameEngine.Create;
            FCurrentQueueData^.StreamData.Position := 0;
            tmp.DecodeFrom(FCurrentQueueData^.StreamData, True);
            if Assigned(FCurrentQueueData^.OnStreamFailedMethod) then
                FCurrentQueueData^.OnStreamFailedMethod(Self, FCurrentQueueData^.Param1, FCurrentQueueData^.Param2, tmp);
            if Assigned(FCurrentQueueData^.OnStreamFailedProc) then
                FCurrentQueueData^.OnStreamFailedProc(Self, FCurrentQueueData^.Param1, FCurrentQueueData^.Param2, tmp);
            DisposeObject(tmp);
          end;
        DisposeQueueData(FCurrentQueueData);
      except
      end;
      FCurrentQueueData := nil;
    end;
end;

procedure TPeerIO.CheckAndTriggerFailedWaitResult;
begin
  SyncMethod(TCoreClassThread.CurrentThread, True, {$IFDEF FPC}@{$ENDIF FPC}Sync_CheckAndTriggerFailedWaitResult);
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

constructor TPeerIO.Create(OwnerFramework_: TCommunicationFramework; IOInterface_: TCoreClassObject);
var
  kref: TInt64;
begin
  inherited Create;

  FCritical := TCritical.Create;
  FCustomProtocolCritical := TCritical.Create;
  FOwnerFramework := OwnerFramework_;
  FIOInterface := IOInterface_;

  FOwnerFramework.Lock_All_IO;

  FID := OwnerFramework_.MakeID;

  FHeadToken := C_DataHeadToken;
  FTailToken := C_DataTailToken;

  FConsoleToken := C_DefaultConsoleToken;
  FStreamToken := C_DefaultStreamToken;
  FDirectConsoleToken := C_DefaultDirectConsoleToken;
  FDirectStreamToken := C_DefaultDirectStreamToken;
  FBigStreamToken := C_DefaultBigStreamToken;
  FBigStreamReceiveFragmentSignal := C_DefaultBigStreamReceiveFragmentSignal;
  FBigStreamReceiveDoneSignal := C_DefaultBigStreamReceiveDoneSignal;
  FCompleteBufferToken := C_DefaultCompleteBufferToken;

  FReceivedAbort := False;
  FReceivedBuffer := TMemoryStream64.CustomCreate(8192);
  FReceivedBuffer_Busy := TMemoryStream64.CustomCreate(8192);

  FBigStreamReceiveProcessing := False;
  FBigStreamTotal := 0;
  FBigStreamCompleted := 0;
  FBigStreamCmd := '';
  FSyncBigStreamReceive := nil;
  FBigStreamSending := nil;
  FBigStreamSendCurrentPos := -1;
  FBigStreamSendDoneTimeFree := False;
  FWaitBigStreamReceiveDoneSignal := False;

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
  FSendDataCipherSecurity := FOwnerFramework.RandomCipherSecurity;
  FCanPauseResultSend := False;
  FQueueList := TCoreClassList.Create;

  UpdateLastCommunicationTime;
  LastCommunicationTick_Received := FLastCommunicationTick;
  LastCommunicationTick_KeepAlive := LastCommunicationTick_Received;
  LastCommunicationTick_Sending := FLastCommunicationTick;

  { generate random key }
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

  InitSequencePacketModel(512, 1024);

  FP2PVMTunnel := nil;
  SetLength(FP2PAuthToken, $FF);
  FillPtrByte(@FP2PAuthToken[0], Length(FP2PAuthToken), $0);

  OnInternalSendByteBuffer := {$IFDEF FPC}@{$ENDIF FPC}FOwnerFramework.Framework_InternalSendByteBuffer;
  OnInternalSaveReceiveBuffer := {$IFDEF FPC}@{$ENDIF FPC}FOwnerFramework.Framework_InternalSaveReceiveBuffer;
  OnInternalProcessReceiveBuffer := {$IFDEF FPC}@{$ENDIF FPC}FOwnerFramework.Framework_InternalProcessReceiveBuffer;
  OnInternalProcessAllSendCmd := {$IFDEF FPC}@{$ENDIF FPC}FOwnerFramework.Framework_InternalProcessAllSendCmd;
  OnCreate := {$IFDEF FPC}@{$ENDIF FPC}FOwnerFramework.Framework_InternalIOCreate;
  OnDestroy := {$IFDEF FPC}@{$ENDIF FPC}FOwnerFramework.Framework_InternalIODestroy;

  OnVMBuildAuthModelResultCall := nil;
  OnVMBuildAuthModelResultMethod := nil;
  OnVMBuildAuthModelResultProc := nil;
  OnVMBuildAuthModelResultIOCall := nil;
  OnVMBuildAuthModelResultIOMethod := nil;
  OnVMBuildAuthModelResultIOProc := nil;
  OnVMAuthResultCall := nil;
  OnVMAuthResultMethod := nil;
  OnVMAuthResultProc := nil;
  OnVMAuthResultIOCall := nil;
  OnVMAuthResultIOMethod := nil;
  OnVMAuthResultIOProc := nil;

  FAutomatedP2PVMClient_Connection_Sequence := 0;
  FAutomatedP2PVMClient_Connection_Sequence_Successed := 0;
  FOnAutomatedP2PVMClientConnectionDoneCall := nil;
  FOnAutomatedP2PVMClientConnectionDoneMethod := nil;
  FOnAutomatedP2PVMClientConnectionDoneProc := nil;

  FUserData := nil;
  FUserValue := Null;
  FUserVariants := nil;
  FUserObjects := nil;
  FUserAutoFreeObjects := nil;

  FUserDefine := FOwnerFramework.FPeerIOUserDefineClass.Create(Self);
  FUserSpecial := FOwnerFramework.FPeerIOUserSpecialClass.Create(Self);
  BeginSendState := False;

  OnCreate(Self);
  CreateAfter;

  FOwnerFramework.FPeerIO_HashPool.Add(FID, Self, False);
  FOwnerFramework.UnLock_All_IO;
end;

procedure TPeerIO.CreateAfter;
begin
end;

destructor TPeerIO.Destroy;
var
  i: Integer;
begin
  CheckAndTriggerFailedWaitResult();

  LockIO;
  try
      OnDestroy(Self);
  except
  end;
  UnLockIO;

  FreeSequencePacketModel();
  InternalCloseP2PVMTunnel;

  if (FBigStreamSending <> nil) and (FBigStreamSendDoneTimeFree) then
    begin
      DisposeObject(FBigStreamSending);
      FBigStreamSending := nil;
    end;

  AtomInc(FOwnerFramework.Statistics[TStatisticsType.stTriggerDisconnect]);

  FOwnerFramework.Lock_All_IO;
  FOwnerFramework.FPeerIO_HashPool.Delete(FID);
  FOwnerFramework.UnLock_All_IO;

  LockIO;
  try
    for i := 0 to FQueueList.Count - 1 do
        DisposeQueueData(FQueueList[i]);
    FQueueList.Clear;
  finally
      UnLockIO;
  end;

  if FUserDefine.FBusy then
      TCompute.RunM_NP({$IFDEF FPC}@{$ENDIF FPC}FUserDefine.DelayFreeOnBusy)
  else
      DisposeObject(FUserDefine);

  if FUserSpecial.FBusy then
      TCompute.RunM_NP({$IFDEF FPC}@{$ENDIF FPC}FUserSpecial.DelayFreeOnBusy)
  else
      DisposeObject(FUserSpecial);

  DisposeObject(FQueueList);
  DisposeObject(FReceivedBuffer);
  DisposeObject(FReceivedBuffer_Busy);
  DisposeObject(FCompleteBufferReceiveStream);
  DisposeObject(FResultDataBuffer);
  DisposeObject(FInDataFrame);
  DisposeObject(FOutDataFrame);
  DisposeObject(ResultDataFrame);

  DisposeObject(FCritical);
  DisposeObject(FCustomProtocolCritical);

  if FUserVariants <> nil then
      DisposeObject(FUserVariants);
  if FUserObjects <> nil then
      DisposeObject(FUserObjects);
  if FUserAutoFreeObjects <> nil then
      DisposeObject(FUserAutoFreeObjects);
  inherited Destroy;
end;

function TPeerIO.IOBusy: Boolean;
begin
  Result := (IOSendBuffer.Size > 0) or
    (SendingSequencePacketHistory.Count > 0) or
    (SequencePacketReceivedPool.Count > 0) or
    (FQueueList.Count > 0) or
    (FReceivedBuffer.Size > 0) or
    (FReceivedBuffer_Busy.Size > 0) or
    (FWaitOnResult) or
    (FBigStreamReceiveProcessing) or
    (FCompleteBufferReceiveProcessing) or
    (FPauseResultSend) or
    (FReceiveTriggerRuning);
end;

procedure TPeerIO.IO_IDLE_TraceC(data: TCoreClassObject; OnNotify: TDataNotifyCall);
var
  p: PIDLE_Trace;
begin
  if not IOBusy then
    begin
      OnNotify(data);
      exit;
    end;

  new(p);
  p^.ID := ID;
  p^.data := data;
  p^.OnNotifyC := OnNotify;
  p^.OnNotifyM := nil;
  p^.OnNotifyP := nil;
  FOwnerFramework.ProgressEngine.PostExecuteM(0.1, {$IFDEF FPC}@{$ENDIF FPC}FOwnerFramework.IDLE_Trace_Execute).Data5 := p;
end;

procedure TPeerIO.IO_IDLE_TraceM(data: TCoreClassObject; OnNotify: TDataNotifyMethod);
var
  p: PIDLE_Trace;
begin
  if not IOBusy then
    begin
      OnNotify(data);
      exit;
    end;

  new(p);
  p^.ID := ID;
  p^.data := data;
  p^.OnNotifyC := nil;
  p^.OnNotifyM := OnNotify;
  p^.OnNotifyP := nil;
  FOwnerFramework.ProgressEngine.PostExecuteM(0.1, {$IFDEF FPC}@{$ENDIF FPC}FOwnerFramework.IDLE_Trace_Execute).Data5 := p;
end;

procedure TPeerIO.IO_IDLE_TraceP(data: TCoreClassObject; OnNotify: TDataNotifyProc);
var
  p: PIDLE_Trace;
begin
  if not IOBusy then
    begin
      OnNotify(data);
      exit;
    end;

  new(p);
  p^.ID := ID;
  p^.data := data;
  p^.OnNotifyC := nil;
  p^.OnNotifyM := nil;
  p^.OnNotifyP := OnNotify;
  FOwnerFramework.ProgressEngine.PostExecuteM(0.1, {$IFDEF FPC}@{$ENDIF FPC}FOwnerFramework.IDLE_Trace_Execute).Data5 := p;
end;

procedure TPeerIO.BuildP2PAuthToken;
var
  de: TDataFrameEngine;
begin
  ResetSequencePacketBuffer;
  FSequencePacketSignal := False;

  de := TDataFrameEngine.Create;
  de.WriteInteger(umlRandomRange(-maxInt, maxInt));
  SendStreamCmdM(C_BuildP2PAuthToken, de, {$IFDEF FPC}@{$ENDIF FPC}FOwnerFramework.CommandResult_BuildP2PAuthToken);
  DisposeObject(de);
  InternalProcessAllSendCmd(nil, False, False);

  OnVMBuildAuthModelResultCall := nil;
  OnVMBuildAuthModelResultMethod := nil;
  OnVMBuildAuthModelResultProc := nil;
  OnVMBuildAuthModelResultIOCall := nil;
  OnVMBuildAuthModelResultIOMethod := nil;
  OnVMBuildAuthModelResultIOProc := nil;
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

procedure TPeerIO.BuildP2PAuthTokenP(const OnResult: TNotifyProc);
begin
  BuildP2PAuthToken;
  OnVMBuildAuthModelResultProc := OnResult;
end;

procedure TPeerIO.BuildP2PAuthTokenIO_C(const OnResult: TIONotifyCall);
begin
  BuildP2PAuthToken;
  OnVMBuildAuthModelResultIOCall := OnResult;
end;

procedure TPeerIO.BuildP2PAuthTokenIO_M(const OnResult: TIONotifyMethod);
begin
  BuildP2PAuthToken;
  OnVMBuildAuthModelResultIOMethod := OnResult;
end;

procedure TPeerIO.BuildP2PAuthTokenIO_P(const OnResult: TIONotifyProc);
begin
  BuildP2PAuthToken;
  OnVMBuildAuthModelResultIOProc := OnResult;
end;

procedure TPeerIO.OpenP2PVMTunnel(vmHashPoolSize: Integer; SendRemoteRequest: Boolean; const AuthToken: SystemString);
begin
  if FP2PVMTunnel = nil then
    begin
      ResetSequencePacketBuffer;
      FSequencePacketSignal := False;

      OnVMAuthResultCall := nil;
      OnVMAuthResultMethod := nil;
      OnVMAuthResultProc := nil;
      OnVMAuthResultIOCall := nil;
      OnVMAuthResultIOMethod := nil;
      OnVMAuthResultIOProc := nil;

      if SendRemoteRequest then
        begin
          if IOBusy then
            begin
              PrintError('OpenP2PVMTunnel failed: IO Busy.');
              exit;
            end;
          SendDirectConsoleCmd(C_InitP2PTunnel, AuthToken);
          ProcessAllSendCmd(nil, False, False);
        end;

      FP2PVMTunnel := TCommunicationFrameworkWithP2PVM.Create(vmHashPoolSize);
      FP2PVMTunnel.QuietMode := FOwnerFramework.QuietMode;
      FP2PVMTunnel.FVMID := FID;

      FP2PVMTunnel.OpenP2PVMTunnel(Self);
      FP2PVMTunnel.AuthWaiting;

      FP2PVMTunnel.OnAuthSuccessOnesNotify := {$IFDEF FPC}@{$ENDIF FPC}P2PVMAuthSuccess;
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
  FOwnerFramework.ProgressPost.PostExecuteM(10.0, {$IFDEF FPC}@{$ENDIF FPC}FOwnerFramework.VMAuthFailedDelayExecute).Data3 := FID;
end;

procedure TPeerIO.OpenP2PVMTunnelM(SendRemoteRequest: Boolean; const AuthToken: SystemString; OnResult: TStateMethod);
begin
  OpenP2PVMTunnel(SendRemoteRequest, AuthToken);
  OnVMAuthResultMethod := OnResult;
  FOwnerFramework.ProgressPost.PostExecuteM(10.0, {$IFDEF FPC}@{$ENDIF FPC}FOwnerFramework.VMAuthFailedDelayExecute).Data3 := FID;
end;

procedure TPeerIO.OpenP2PVMTunnelP(SendRemoteRequest: Boolean; const AuthToken: SystemString; OnResult: TStateProc);
begin
  OpenP2PVMTunnel(SendRemoteRequest, AuthToken);
  OnVMAuthResultProc := OnResult;
  FOwnerFramework.ProgressPost.PostExecuteM(10.0, {$IFDEF FPC}@{$ENDIF FPC}FOwnerFramework.VMAuthFailedDelayExecute).Data3 := FID;
end;

procedure TPeerIO.OpenP2PVMTunnelC(vmHashPoolSize: Integer; SendRemoteRequest: Boolean; const AuthToken: SystemString; OnResult: TStateCall);
begin
  OpenP2PVMTunnel(vmHashPoolSize, SendRemoteRequest, AuthToken);
  OnVMAuthResultCall := OnResult;
  FOwnerFramework.ProgressPost.PostExecuteM(10.0, {$IFDEF FPC}@{$ENDIF FPC}FOwnerFramework.VMAuthFailedDelayExecute).Data3 := FID;
end;

procedure TPeerIO.OpenP2PVMTunnelM(vmHashPoolSize: Integer; SendRemoteRequest: Boolean; const AuthToken: SystemString; OnResult: TStateMethod);
begin
  OpenP2PVMTunnel(vmHashPoolSize, SendRemoteRequest, AuthToken);
  OnVMAuthResultMethod := OnResult;
  FOwnerFramework.ProgressPost.PostExecuteM(10.0, {$IFDEF FPC}@{$ENDIF FPC}FOwnerFramework.VMAuthFailedDelayExecute).Data3 := FID;
end;

procedure TPeerIO.OpenP2PVMTunnelP(vmHashPoolSize: Integer; SendRemoteRequest: Boolean; const AuthToken: SystemString; OnResult: TStateProc);
begin
  OpenP2PVMTunnel(vmHashPoolSize, SendRemoteRequest, AuthToken);
  OnVMAuthResultProc := OnResult;
  FOwnerFramework.ProgressPost.PostExecuteM(10.0, {$IFDEF FPC}@{$ENDIF FPC}FOwnerFramework.VMAuthFailedDelayExecute).Data3 := FID;
end;

procedure TPeerIO.OpenP2PVMTunnelIO_C(SendRemoteRequest: Boolean; const AuthToken: SystemString; OnResult: TIOStateCall);
begin
  OpenP2PVMTunnel(SendRemoteRequest, AuthToken);
  OnVMAuthResultIOCall := OnResult;
  FOwnerFramework.ProgressPost.PostExecuteM(10.0, {$IFDEF FPC}@{$ENDIF FPC}FOwnerFramework.VMAuthFailedDelayExecute).Data3 := FID;
end;

procedure TPeerIO.OpenP2PVMTunnelIO_M(SendRemoteRequest: Boolean; const AuthToken: SystemString; OnResult: TIOStateMethod);
begin
  OpenP2PVMTunnel(SendRemoteRequest, AuthToken);
  OnVMAuthResultIOMethod := OnResult;
  FOwnerFramework.ProgressPost.PostExecuteM(10.0, {$IFDEF FPC}@{$ENDIF FPC}FOwnerFramework.VMAuthFailedDelayExecute).Data3 := FID;
end;

procedure TPeerIO.OpenP2PVMTunnelIO_P(SendRemoteRequest: Boolean; const AuthToken: SystemString; OnResult: TIOStateProc);
begin
  OpenP2PVMTunnel(SendRemoteRequest, AuthToken);
  OnVMAuthResultIOProc := OnResult;
  FOwnerFramework.ProgressPost.PostExecuteM(10.0, {$IFDEF FPC}@{$ENDIF FPC}FOwnerFramework.VMAuthFailedDelayExecute).Data3 := FID;
end;

procedure TPeerIO.OpenP2PVMTunnelIO_C(vmHashPoolSize: Integer; SendRemoteRequest: Boolean; const AuthToken: SystemString; OnResult: TIOStateCall);
begin
  OpenP2PVMTunnel(vmHashPoolSize, SendRemoteRequest, AuthToken);
  OnVMAuthResultIOCall := OnResult;
  FOwnerFramework.ProgressPost.PostExecuteM(10.0, {$IFDEF FPC}@{$ENDIF FPC}FOwnerFramework.VMAuthFailedDelayExecute).Data3 := FID;
end;

procedure TPeerIO.OpenP2PVMTunnelIO_M(vmHashPoolSize: Integer; SendRemoteRequest: Boolean; const AuthToken: SystemString; OnResult: TIOStateMethod);
begin
  OpenP2PVMTunnel(vmHashPoolSize, SendRemoteRequest, AuthToken);
  OnVMAuthResultIOMethod := OnResult;
  FOwnerFramework.ProgressPost.PostExecuteM(10.0, {$IFDEF FPC}@{$ENDIF FPC}FOwnerFramework.VMAuthFailedDelayExecute).Data3 := FID;
end;

procedure TPeerIO.OpenP2PVMTunnelIO_P(vmHashPoolSize: Integer; SendRemoteRequest: Boolean; const AuthToken: SystemString; OnResult: TIOStateProc);
begin
  OpenP2PVMTunnel(vmHashPoolSize, SendRemoteRequest, AuthToken);
  OnVMAuthResultIOProc := OnResult;
  FOwnerFramework.ProgressPost.PostExecuteM(10.0, {$IFDEF FPC}@{$ENDIF FPC}FOwnerFramework.VMAuthFailedDelayExecute).Data3 := FID;
end;

procedure TPeerIO.OpenP2PVMTunnel;
begin
  OpenP2PVMTunnel(False, '');
end;

procedure TPeerIO.CloseP2PVMTunnel;
begin
  if IOBusy then
    begin
      PrintError('CloseP2PVMTunnel failed: IO Busy.');
      exit;
    end;
  SendDirectConsoleCmd(C_CloseP2PTunnel, '');
  ProcessAllSendCmd(nil, False, False);
end;

procedure TPeerIO.PrintError(v: SystemString);
var
  n: SystemString;
begin
  n := GetPeerIP;
  if n <> '' then
      OwnerFramework.DoError(Format('error: %s %s', [n, v]))
  else
      OwnerFramework.DoError(Format('error: %s', [v]));
end;

procedure TPeerIO.PrintError(v: SystemString; const Args: array of const);
begin
  PrintError(Format(v, Args));
end;

procedure TPeerIO.Print(v: SystemString);
var
  n: SystemString;
begin
  n := GetPeerIP;
  if n <> '' then
      OwnerFramework.DoPrint(Format('%s %s', [n, v]))
  else
      OwnerFramework.DoPrint(Format('%s', [v]));
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
      FCritical.Acquire;
  AtomInc(FOwnerFramework.Statistics[TStatisticsType.stIOLock]);
end;

procedure TPeerIO.UnLockIO;
begin
  if FOwnerFramework.FEnabledAtomicLockAndMultiThread then
      FCritical.Release;
  AtomInc(FOwnerFramework.Statistics[TStatisticsType.stIOUnLock]);
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
begin
  { anti dead loop }
  if FProgressRunning then
      exit;

  FProgressRunning := True;

  ProcessSequencePacketModel();

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

  if (not FTimeOutProcessDone) and (FOwnerFramework.FIdleTimeOut > 0) and (StopCommunicationTime > FOwnerFramework.FIdleTimeOut) then
    begin
      AtomInc(FOwnerFramework.Statistics[TStatisticsType.stTimeOutDisconnect]);
      FTimeOutProcessDone := True;
      DelayClose(1.0);
    end;

  if (not FTimeOutProcessDone) and (OwnerFramework.TimeOutKeepAlive) and (IsSequencePacketModel) and (FSequencePacketSignal) and
    (GetTimeTick() - LastCommunicationTick_KeepAlive > 2000) and (WriteBufferEmpty) then
    begin
      SendSequencePacketKeepAlive(nil, 0);
      FlushIOSendBuffer;
      LastCommunicationTick_KeepAlive := GetTimeTick();
    end;

  { anti dead loop }
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
  UpdateLastCommunicationTime;
  LastCommunicationTick_Received := FLastCommunicationTick;
  LastCommunicationTick_KeepAlive := LastCommunicationTick_Received;
end;

procedure TPeerIO.FillRecvBuffer(const CurrentActiveThread_: TCoreClassThread; const RecvSync, SendSync: Boolean);
begin
  OnInternalProcessReceiveBuffer(Self, CurrentActiveThread_, RecvSync, SendSync);
end;

procedure TPeerIO.ProcessAllSendCmd(const CurrentActiveThread_: TCoreClassThread; const RecvSync, SendSync: Boolean);
begin
  OnInternalProcessAllSendCmd(Self, CurrentActiveThread_, RecvSync, SendSync);
end;

procedure TPeerIO.PostQueueData(p: PQueueData);
begin
  FOwnerFramework.CmdSendStatistics.IncValue(p^.Cmd, 1);
  FQueueList.Add(p);
end;

procedure TPeerIO.WriteCustomBuffer(const buffer: PByte; const Size: NativeInt);
begin
  WriteBufferOpen;
  OnInternalSendByteBuffer(Self, buffer, Size);
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
          buff.WritePtr(@b[0], Length(b));
        end
      else
          FOutDataFrame.EncodeTo(buff, True);

      dHead := FHeadToken;
      dTail := FTailToken;
      Len := buff.Size;

      { generate hash source }
      GenerateHashCode(FOwnerFramework.FHashSecurity, buff.Memory, buff.Size, Code);
      headBuff[0] := Byte(FOwnerFramework.FHashSecurity);
      PWORD(@headBuff[1])^ := Length(Code);

      { generate encrypt data body }
      bCipherSecurity := Byte(FReceiveDataCipherSecurity);
      Encrypt(FReceiveDataCipherSecurity, buff.Memory, buff.Size, FCipherKey, True);

      { result data header }
      FResultDataBuffer.WritePtr(@dHead, C_Cardinal_Size);
      FResultDataBuffer.WritePtr(@Len, C_Integer_Size);

      { verify code }
      FResultDataBuffer.WritePtr(@headBuff[0], 3);
      if Length(Code) > 0 then
          FResultDataBuffer.WritePtr(@Code[0], Length(Code));

      { data body }
      FResultDataBuffer.WritePtr(@bCipherSecurity, C_Byte_Size);
      FResultDataBuffer.WritePtr(buff.Memory, Len);

      { data tail }
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
  Complete := FBigStreamSendCurrentPos;
end;

function TPeerIO.GetBigStreamBatch: TBigStreamBatch;
begin
  Result := FUserDefine.FBigStreamBatch;
end;

procedure TPeerIO.SetID(const Value: Cardinal);
begin
  if Value = FID then
      exit;
  if not FOwnerFramework.FPeerIO_HashPool.Exists(FID) then
      PrintError('old ID illegal');
  if FOwnerFramework.FPeerIO_HashPool.Exists(Value) then
      PrintError('new ID illegal');

  FOwnerFramework.Lock_All_IO;
  try
    FOwnerFramework.FPeerIO_HashPool.Delete(FID);
    FID := Value;
    FOwnerFramework.FPeerIO_HashPool.Add(FID, Self, False);
  finally
      FOwnerFramework.UnLock_All_IO;
  end;
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

function TPeerIO.StopCommunicationTime: TTimeTick;
begin
  Result := GetTimeTick - LastCommunicationTick_Received;
end;

procedure TPeerIO.UpdateLastCommunicationTime;
begin
  FLastCommunicationTick := GetTimeTick;
end;

procedure TPeerIO.SendConsoleCmdM(Cmd, ConsoleData: SystemString; OnResult: TConsoleMethod);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendConsoleCmdM(Self, Cmd, ConsoleData, OnResult)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendConsoleCmdM(Cmd, ConsoleData, OnResult);
end;

procedure TPeerIO.SendConsoleCmdM(Cmd, ConsoleData: SystemString; Param1: Pointer; Param2: TObject; OnResult: TConsoleParamMethod);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendConsoleCmdM(Self, Cmd, ConsoleData, Param1, Param2, OnResult)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendConsoleCmdM(Cmd, ConsoleData, Param1, Param2, OnResult);
end;

procedure TPeerIO.SendConsoleCmdM(Cmd, ConsoleData: SystemString; Param1: Pointer; Param2: TObject; OnResult: TConsoleParamMethod; OnFailed: TConsoleFailedMethod);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendConsoleCmdM(Self, Cmd, ConsoleData, Param1, Param2, OnResult, OnFailed)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendConsoleCmdM(Cmd, ConsoleData, Param1, Param2, OnResult, OnFailed);
end;

procedure TPeerIO.SendStreamCmdM(Cmd: SystemString; StreamData: TMemoryStream64; OnResult: TStreamMethod; DoneAutoFree: Boolean);
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

procedure TPeerIO.SendStreamCmdM(Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamMethod; OnFailed: TStreamFailedMethod);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendStreamCmdM(Self, Cmd, StreamData, Param1, Param2, OnResult, OnFailed)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendStreamCmdM(Cmd, StreamData, Param1, Param2, OnResult, OnFailed);
end;

procedure TPeerIO.SendConsoleCmdP(Cmd, ConsoleData: SystemString; OnResult: TConsoleProc);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendConsoleCmdP(Self, Cmd, ConsoleData, OnResult)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendConsoleCmdP(Cmd, ConsoleData, OnResult);
end;

procedure TPeerIO.SendConsoleCmdP(Cmd, ConsoleData: SystemString; Param1: Pointer; Param2: TObject; OnResult: TConsoleParamProc);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendConsoleCmdP(Self, Cmd, ConsoleData, Param1, Param2, OnResult)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendConsoleCmdP(Cmd, ConsoleData, Param1, Param2, OnResult);
end;

procedure TPeerIO.SendConsoleCmdP(Cmd, ConsoleData: SystemString; Param1: Pointer; Param2: TObject; OnResult: TConsoleParamProc; OnFailed: TConsoleFailedProc);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendConsoleCmdP(Self, Cmd, ConsoleData, Param1, Param2, OnResult, OnFailed)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendConsoleCmdP(Cmd, ConsoleData, Param1, Param2, OnResult, OnFailed);
end;

procedure TPeerIO.SendStreamCmdP(Cmd: SystemString; StreamData: TMemoryStream64; OnResult: TStreamProc; DoneAutoFree: Boolean);
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

procedure TPeerIO.SendStreamCmdP(Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamProc; OnFailed: TStreamFailedProc);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendStreamCmdP(Self, Cmd, StreamData, Param1, Param2, OnResult, OnFailed)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendStreamCmdP(Cmd, StreamData, Param1, Param2, OnResult, OnFailed);
end;

procedure TPeerIO.SendDirectConsoleCmd(Cmd: SystemString; ConsoleData: SystemString);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendDirectConsoleCmd(Self, Cmd, ConsoleData)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendDirectConsoleCmd(Cmd, ConsoleData);
end;

procedure TPeerIO.SendDirectConsoleCmd(Cmd: SystemString);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendDirectConsoleCmd(Self, Cmd)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendDirectConsoleCmd(Cmd);
end;

procedure TPeerIO.SendDirectStreamCmd(Cmd: SystemString; StreamData: TMemoryStream64; DoneAutoFree: Boolean);
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

function TPeerIO.WaitSendConsoleCmd(Cmd, ConsoleData: SystemString; Timeout: TTimeTick): SystemString;
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      Result := TCommunicationFrameworkServer(FOwnerFramework).WaitSendConsoleCmd(Self, Cmd, ConsoleData, Timeout)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      Result := TCommunicationFrameworkClient(FOwnerFramework).WaitSendConsoleCmd(Cmd, ConsoleData, Timeout)
  else
      Result := '';
end;

procedure TPeerIO.WaitSendStreamCmd(Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; Timeout: TTimeTick);
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

procedure TAutomatedP2PVMServiceBind.AddService(Service: TCommunicationFrameworkWithP2PVM_Server; IPV6: SystemString; Port: Word);
var
  p: PAutomatedP2PVMServiceData;
begin
  new(p);
  p^.Service := Service;
  p^.Service.StartService(IPV6, Port);
  Add(p);
end;

procedure TAutomatedP2PVMServiceBind.AddService(Service: TCommunicationFrameworkWithP2PVM_Server);
var
  p: PAutomatedP2PVMServiceData;
begin
  new(p);
  p^.Service := Service;
  Add(p);
end;

procedure TAutomatedP2PVMServiceBind.Clean;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Dispose(Items[i]);
  Clear;
end;

procedure TAutomatedP2PVMClientBind.AddClient(Client: TCommunicationFrameworkWithP2PVM_Client; IPV6: SystemString; Port: Word);
var
  p: PAutomatedP2PVMClientData;
begin
  new(p);
  p^.Client := Client;
  p^.IPV6 := IPV6;
  p^.Port := Port;
  Add(p);
end;

procedure TAutomatedP2PVMClientBind.Clean;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Dispose(Items[i]);
  Clear;
end;

procedure TCommunicationFramework.DoPrint(const v: SystemString);
var
  n1, n2: SystemString;
begin
  if not FQuietMode then
    begin
      if FPrefixName <> '' then
          n1 := FPrefixName
      else
          n1 := '';
      if FName <> '' then
        begin
          if n1 <> '' then
              n1 := n1 + '.';
          n2 := FName + ' ';
        end
      else
          n2 := '';
      DoStatus(n1 + n2 + v, C_DoStatusID);
    end;

  AtomInc(Statistics[TStatisticsType.stPrint]);
end;

procedure TCommunicationFramework.DoError(const v: SystemString);
var
  n1, n2: SystemString;
begin
  if FPrefixName <> '' then
      n1 := FPrefixName
  else
      n1 := '';
  if FName <> '' then
    begin
      if n1 <> '' then
          n1 := n1 + '.';
      n2 := FName + ' ';
    end
  else
      n2 := '';
  DoStatus(n1 + n2 + v, C_DoStatusID);

  AtomInc(Statistics[TStatisticsType.stPrint]);
end;

function TCommunicationFramework.GetIdleTimeOut: TTimeTick;
begin
  Result := FIdleTimeOut;
end;

procedure TCommunicationFramework.SetIdleTimeOut(const Value: TTimeTick);
begin
  FIdleTimeOut := Value;
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
    begin
      c_IO.Disconnect;
    end;
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
  P_IO: TPeerIO;
  nQueue: PQueueData;
begin
  P_IO := TPeerIO(FPeerIO_HashPool[Sender.Data4]);
  nQueue := PQueueData(Sender.Data5);

  if P_IO <> nil then
    begin
      DoExecuteResult(P_IO, nQueue, Sender.Data3, Sender.DataEng);
    end;

  DisposeQueueData(nQueue);
end;

procedure TCommunicationFramework.DelayExecuteOnCompleteBufferState(Sender: TNPostExecute);
var
  P_IO: TPeerIO;
  Cmd: SystemString;
  CompleteBuff: TMemoryStream64;
begin
  P_IO := TPeerIO(FPeerIO_HashPool[Sender.Data3]);
  Cmd := Sender.Data4;

  CompleteBuff := TMemoryStream64(Sender.Data1);
  if P_IO <> nil then
      ExecuteCompleteBuffer(P_IO, Cmd, CompleteBuff.Memory, CompleteBuff.Size);
  DisposeObject(CompleteBuff);
end;

procedure TCommunicationFramework.IDLE_Trace_Execute(Sender: TNPostExecute);
var
  p: PIDLE_Trace;
  p_id: Cardinal;
  P_IO: TPeerIO;
begin
  p := Sender.Data5;
  p_id := p^.ID;

  P_IO := TPeerIO(FPeerIO_HashPool[p_id]);

  if P_IO <> nil then
    begin
      if P_IO.IOBusy then
        begin
          with ProgressEngine.PostExecuteM(0.1, {$IFDEF FPC}@{$ENDIF FPC}IDLE_Trace_Execute) do
            begin
              Data4 := p_id;
              Data5 := p;
            end;
        end
      else
        begin
          if Assigned(p^.OnNotifyC) then
              p^.OnNotifyC(p^.data);
          if Assigned(p^.OnNotifyM) then
              p^.OnNotifyM(p^.data);
          if Assigned(p^.OnNotifyP) then
              p^.OnNotifyP(p^.data);
          Dispose(p);
        end;
    end
  else
    begin
      Dispose(p);
    end;
end;

function TCommunicationFramework.MakeID: Cardinal;
begin
  repeat
    Result := FIDSeed;
    AtomInc(FIDSeed);
  until not FPeerIO_HashPool.Exists(Result);
end;

procedure TCommunicationFramework.FillCustomBuffer(Sender: TPeerIO; const th: TCoreClassThread; const buffer: PByte; const Size: NativeInt; var FillDone: Boolean);
begin
end;

procedure TCommunicationFramework.Framework_InternalSendByteBuffer(const Sender: TPeerIO; const buff: PByte; siz: NativeInt);
var
  p: PByte;
begin
  if siz <= 0 then
      exit;

  AtomInc(Statistics[TStatisticsType.stSendSize], siz);

  p := buff;

  { fill fragment }
  while siz > C_SendFlushSize do
    begin
      Sender.SendByteBuffer(p, C_SendFlushSize);
      inc(p, C_SendFlushSize);
      dec(siz, C_SendFlushSize);
    end;

  if siz > 0 then
    begin
      Sender.SendByteBuffer(p, siz);
    end;

  Sender.UpdateLastCommunicationTime;
  Sender.LastCommunicationTick_Sending := Sender.FLastCommunicationTick;
end;

procedure TCommunicationFramework.Framework_InternalSaveReceiveBuffer(const Sender: TPeerIO; const buff: Pointer; siz: Int64);
begin
  if siz > 0 then
      Sender.InternalSaveReceiveBuffer(buff, siz);
end;

procedure TCommunicationFramework.Framework_InternalProcessReceiveBuffer(const Sender: TPeerIO; const CurrentActiveThread_: TCoreClassThread; const RecvSync, SendSync: Boolean);
var
  FillDone: Boolean;
begin
  if FProtocol = cpCustom then
    begin
      if FEnabledAtomicLockAndMultiThread then
          Sender.FCustomProtocolCritical.Acquire;

      FillDone := True;

      if RecvSync and (CurrentActiveThread_ <> nil) then
          FillCustomBuffer(Sender, CurrentActiveThread_, Sender.FReceivedBuffer.Memory, Sender.FReceivedBuffer.Size, FillDone)
      else
          FillCustomBuffer(Sender, nil, Sender.FReceivedBuffer.Memory, Sender.FReceivedBuffer.Size, FillDone);

      if FillDone then
          Sender.FReceivedBuffer.Clear
      else
          Sender.InternalProcessReceiveBuffer(CurrentActiveThread_, RecvSync, SendSync);

      if FEnabledAtomicLockAndMultiThread then
          Sender.FCustomProtocolCritical.Release;
    end
  else
      Sender.InternalProcessReceiveBuffer(CurrentActiveThread_, RecvSync, SendSync);
end;

procedure TCommunicationFramework.Framework_InternalProcessAllSendCmd(const Sender: TPeerIO; const CurrentActiveThread_: TCoreClassThread; const RecvSync, SendSync: Boolean);
begin
  Sender.InternalProcessAllSendCmd(CurrentActiveThread_, RecvSync, SendSync);
end;

procedure TCommunicationFramework.Framework_InternalIOCreate(const Sender: TPeerIO);
begin
  if FIOInterface <> nil then
      FIOInterface.PeerIO_Create(Sender);
end;

procedure TCommunicationFramework.Framework_InternalIODestroy(const Sender: TPeerIO);
begin
  if FIOInterface <> nil then
      FIOInterface.PeerIO_Destroy(Sender);
end;

procedure TCommunicationFramework.BuildP2PAuthTokenResult_OnIOIDLE(Sender: TCoreClassObject);
var
  P_IO: TPeerIO;
begin
  P_IO := TPeerIO(Sender);

  try
    if Assigned(P_IO.OnVMBuildAuthModelResultCall) then
        P_IO.OnVMBuildAuthModelResultCall();
    if Assigned(P_IO.OnVMBuildAuthModelResultMethod) then
        P_IO.OnVMBuildAuthModelResultMethod();
    if Assigned(P_IO.OnVMBuildAuthModelResultProc) then
        P_IO.OnVMBuildAuthModelResultProc();
    if Assigned(P_IO.OnVMBuildAuthModelResultIOCall) then
        P_IO.OnVMBuildAuthModelResultIOCall(P_IO);
    if Assigned(P_IO.OnVMBuildAuthModelResultIOMethod) then
        P_IO.OnVMBuildAuthModelResultIOMethod(P_IO);
    if Assigned(P_IO.OnVMBuildAuthModelResultIOProc) then
        P_IO.OnVMBuildAuthModelResultIOProc(P_IO);
  except
  end;

  P_IO.OnVMBuildAuthModelResultCall := nil;
  P_IO.OnVMBuildAuthModelResultMethod := nil;
  P_IO.OnVMBuildAuthModelResultProc := nil;
  P_IO.OnVMBuildAuthModelResultIOCall := nil;
  P_IO.OnVMBuildAuthModelResultIOMethod := nil;
  P_IO.OnVMBuildAuthModelResultIOProc := nil;
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

  Sender.IO_IDLE_TraceM(Sender, {$IFDEF FPC}@{$ENDIF FPC}BuildP2PAuthTokenResult_OnIOIDLE);
end;

procedure TCommunicationFramework.Command_BuildP2PAuthToken(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
var
  i: Integer;
  seed: Integer;
  arr: TDataFrameArrayInteger;
begin
  Sender.ResetSequencePacketBuffer;
  Sender.FSequencePacketSignal := False;

  { build auth buffer }
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

  Sender.ResetSequencePacketBuffer;
  Sender.FSequencePacketSignal := False;

  Sender.OpenP2PVMTunnel(16, False, '');
  Sender.p2pVMTunnel.AuthVM;
  p2pVMTunnelOpenBefore(Sender, Sender.p2pVMTunnel);
end;

procedure TCommunicationFramework.Command_CloseP2PTunnel(Sender: TPeerIO; InData: SystemString);
begin
  Sender.InternalCloseP2PVMTunnel;
  Sender.ResetSequencePacketBuffer;
end;

procedure TCommunicationFramework.VMAuthSuccessAfterDelayExecute(Sender: TNPostExecute);
var
  P_IO: TPeerIO;
begin
  P_IO := TPeerIO(FPeerIO_HashPool[Sender.Data3]);
  if P_IO = nil then
      exit;

  try
    if Assigned(P_IO.OnVMAuthResultCall) then
        P_IO.OnVMAuthResultCall(True);
    if Assigned(P_IO.OnVMAuthResultMethod) then
        P_IO.OnVMAuthResultMethod(True);
    if Assigned(P_IO.OnVMAuthResultProc) then
        P_IO.OnVMAuthResultProc(True);
    if Assigned(P_IO.OnVMAuthResultIOCall) then
        P_IO.OnVMAuthResultIOCall(P_IO, True);
    if Assigned(P_IO.OnVMAuthResultIOMethod) then
        P_IO.OnVMAuthResultIOMethod(P_IO, True);
    if Assigned(P_IO.OnVMAuthResultIOProc) then
        P_IO.OnVMAuthResultIOProc(P_IO, True);
  except
  end;

  P_IO.OnVMAuthResultCall := nil;
  P_IO.OnVMAuthResultMethod := nil;
  P_IO.OnVMAuthResultProc := nil;
  P_IO.OnVMAuthResultIOCall := nil;
  P_IO.OnVMAuthResultIOMethod := nil;
  P_IO.OnVMAuthResultIOProc := nil;
  p2pVMTunnelOpenAfter(P_IO, P_IO.p2pVMTunnel);
end;

procedure TCommunicationFramework.VMAuthSuccessDelayExecute(Sender: TNPostExecute);
var
  P_IO: TPeerIO;
begin
  P_IO := TPeerIO(FPeerIO_HashPool[Sender.Data3]);
  if P_IO = nil then
      exit;

  ProgressPost.PostExecuteM(0.5, {$IFDEF FPC}@{$ENDIF FPC}VMAuthSuccessAfterDelayExecute).Data3 := P_IO.FID;
  p2pVMTunnelOpen(P_IO, P_IO.p2pVMTunnel);
end;

procedure TCommunicationFramework.VMAuthFailedDelayExecute(Sender: TNPostExecute);
var
  P_IO: TPeerIO;
begin
  P_IO := TPeerIO(FPeerIO_HashPool[Sender.Data3]);
  if P_IO = nil then
      exit;

  try
    if Assigned(P_IO.OnVMAuthResultCall) then
        P_IO.OnVMAuthResultCall(False);
    if Assigned(P_IO.OnVMAuthResultMethod) then
        P_IO.OnVMAuthResultMethod(False);
    if Assigned(P_IO.OnVMAuthResultProc) then
        P_IO.OnVMAuthResultProc(False);
    if Assigned(P_IO.OnVMAuthResultIOCall) then
        P_IO.OnVMAuthResultIOCall(P_IO, False);
    if Assigned(P_IO.OnVMAuthResultIOMethod) then
        P_IO.OnVMAuthResultIOMethod(P_IO, False);
    if Assigned(P_IO.OnVMAuthResultIOProc) then
        P_IO.OnVMAuthResultIOProc(P_IO, False);
  except
  end;

  P_IO.OnVMAuthResultCall := nil;
  P_IO.OnVMAuthResultMethod := nil;
  P_IO.OnVMAuthResultProc := nil;
  P_IO.OnVMAuthResultIOCall := nil;
  P_IO.OnVMAuthResultIOMethod := nil;
  P_IO.OnVMAuthResultIOProc := nil;
end;

procedure TCommunicationFramework.InitAutomatedP2PVM;
begin
  FAutomatedP2PVMService := False;
  FAutomatedP2PVMServiceBind := TAutomatedP2PVMServiceBind.Create;
  FAutomatedP2PVMClient := False;
  FAutomatedP2PVMClientBind := TAutomatedP2PVMClientBind.Create;
  FAutomatedP2PVMClientDelayBoot := 0.1;
  FAutomatedP2PVMAuthToken := '';
  FOnAutomatedP2PVMClientConnectionDone_C := nil;
  FOnAutomatedP2PVMClientConnectionDone_M := nil;
  FOnAutomatedP2PVMClientConnectionDone_P := nil;
end;

procedure TCommunicationFramework.FreeAutomatedP2PVM;
begin
  FAutomatedP2PVMServiceBind.Clean;
  DisposeObject(FAutomatedP2PVMServiceBind);
  FAutomatedP2PVMClientBind.Clean;
  DisposeObject(FAutomatedP2PVMClientBind);
end;

procedure TCommunicationFramework.DoAutomatedP2PVMClient_DelayRequest(Sender: TNPostExecute);
var
  IO_ID: Cardinal;
begin
  IO_ID := Sender.Data3;
  DoAutomatedP2PVMClient_Request(IO_ID);
end;

procedure TCommunicationFramework.DoAutomatedP2PVMClient_Request(IO_ID: Cardinal);
var
  P_IO: TPeerIO;
begin
  P_IO := TPeerIO(FPeerIO_HashPool[IO_ID]);
  if P_IO = nil then
    begin
      Print('AutomatedP2PVMClient_Request request fialed: loss IO');
      exit;
    end;
  if P_IO.OwnerFramework <> Self then
      RaiseInfo('illegal.');

  P_IO.FAutomatedP2PVMClient_Connection_Sequence := 0;
  P_IO.FAutomatedP2PVMClient_Connection_Sequence_Successed := 0;

  if FAutomatedP2PVMClient then
      P_IO.BuildP2PAuthTokenIO_M({$IFDEF FPC}@{$ENDIF FPC}AutomatedP2PVMClient_BuildP2PAuthTokenResult)
  else
      Print('AutomatedP2PVMClient is false, on do AutomatedP2PVMClient_Request dont work.');
end;

procedure TCommunicationFramework.AutomatedP2PVMClient_BuildP2PAuthTokenResult(P_IO: TPeerIO);
begin
  if P_IO <> nil then
      P_IO.OpenP2PVMTunnelIO_M(True, FAutomatedP2PVMAuthToken, {$IFDEF FPC}@{$ENDIF FPC}AutomatedP2PVMClient_OpenP2PVMTunnelResult);
end;

procedure TCommunicationFramework.AutomatedP2PVMClient_OpenP2PVMTunnelResult(P_IO: TPeerIO; VMauthState: Boolean);
var
  p: PAutomatedP2PVMClientData;
  i: Integer;
begin
  if not VMauthState then
    begin
      Print('Automated P2PVM Auth failed!');
      exit;
    end;

  for i := 0 to FAutomatedP2PVMClientBind.Count - 1 do
      P_IO.p2pVMTunnel.InstallLogicFramework(FAutomatedP2PVMClientBind[i]^.Client);

  if (FAutomatedP2PVMClientBind.Count > P_IO.FAutomatedP2PVMClient_Connection_Sequence) then
    begin
      p := FAutomatedP2PVMClientBind[P_IO.FAutomatedP2PVMClient_Connection_Sequence];
      p^.Client.AsyncConnectM(p^.IPV6, p^.Port, nil, P_IO, {$IFDEF FPC}@{$ENDIF FPC}AutomatedP2PVMClient_ConnectionResult);
    end
  else
    begin
      { all connection done. }
      AutomatedP2PVMClient_Done(P_IO);
    end;
end;

procedure TCommunicationFramework.AutomatedP2PVMClient_ConnectionResult(Param1: Pointer; Param2: TObject; const ConnectionState: Boolean);
var
  P_IO: TPeerIO;
  p: PAutomatedP2PVMClientData;
begin
  if not FPeerIO_HashPool.ExistsObject(Param2) then
    begin
      Print('Automated P2PVM IO failed.');
      exit;
    end;

  P_IO := TPeerIO(Param2);
  inc(P_IO.FAutomatedP2PVMClient_Connection_Sequence);
  if ConnectionState then
      inc(P_IO.FAutomatedP2PVMClient_Connection_Sequence_Successed);
  if (FAutomatedP2PVMClientBind.Count > P_IO.FAutomatedP2PVMClient_Connection_Sequence) then
    begin
      p := FAutomatedP2PVMClientBind[P_IO.FAutomatedP2PVMClient_Connection_Sequence];
      p^.Client.AsyncConnectM(p^.IPV6, p^.Port, nil, P_IO, {$IFDEF FPC}@{$ENDIF FPC}AutomatedP2PVMClient_ConnectionResult);
    end
  else
    begin
      { all connection done. }
      AutomatedP2PVMClient_Done(P_IO);
    end;
end;

procedure TCommunicationFramework.AutomatedP2PVMClient_Done(P_IO: TPeerIO);
begin
  Print('Automated P2PVM client connection done.');
  try
    if Assigned(FOnAutomatedP2PVMClientConnectionDone_C) then
        FOnAutomatedP2PVMClientConnectionDone_C(Self, P_IO);
    if Assigned(FOnAutomatedP2PVMClientConnectionDone_M) then
        FOnAutomatedP2PVMClientConnectionDone_M(Self, P_IO);
    if Assigned(FOnAutomatedP2PVMClientConnectionDone_P) then
        FOnAutomatedP2PVMClientConnectionDone_P(Self, P_IO);
  except
  end;

  try
    if Assigned(P_IO.FOnAutomatedP2PVMClientConnectionDoneCall) then
        P_IO.FOnAutomatedP2PVMClientConnectionDoneCall(P_IO, AutomatedP2PVMClientConnectionDone(P_IO));
    if Assigned(P_IO.FOnAutomatedP2PVMClientConnectionDoneMethod) then
        P_IO.FOnAutomatedP2PVMClientConnectionDoneMethod(P_IO, AutomatedP2PVMClientConnectionDone(P_IO));
    if Assigned(P_IO.FOnAutomatedP2PVMClientConnectionDoneProc) then
        P_IO.FOnAutomatedP2PVMClientConnectionDoneProc(P_IO, AutomatedP2PVMClientConnectionDone(P_IO));
  except
  end;

  P_IO.FOnAutomatedP2PVMClientConnectionDoneCall := nil;
  P_IO.FOnAutomatedP2PVMClientConnectionDoneMethod := nil;
  P_IO.FOnAutomatedP2PVMClientConnectionDoneProc := nil;
end;

procedure TCommunicationFramework.InitLargeScaleIOPool;
begin
  SetLength(FLargeScaleIOPool, 0);
  FProgressMaxDelay := 0;
end;

procedure TCommunicationFramework.FreeLargeScaleIOPool;
begin
  SetLength(FLargeScaleIOPool, 0);
end;

procedure TCommunicationFramework.ProgressLargeScaleIOPool;
var
  tk: TTimeTick;
  i: Integer;
  P_IO: TPeerIO;
begin
  if FPeerIO_HashPool.Count = 0 then
      exit;

  tk := GetTimeTick();
  if (Length(FLargeScaleIOPool) = 0) or (FProgressMaxDelay = 0) then
      GetIO_Array(FLargeScaleIOPool);

  i := High(FLargeScaleIOPool);

  while i >= 0 do
    begin
      P_IO := TPeerIO(FPeerIO_HashPool[FLargeScaleIOPool[i]]);

      if P_IO <> nil then
        begin
          try
              P_IO.Progress;
          except
          end;
        end;

      if (FProgressMaxDelay > 0) and (GetTimeTick() - tk > FProgressMaxDelay) then
          Break;
      dec(i);
    end;

  SetLength(FLargeScaleIOPool, umlMax(i, 0));
end;

constructor TCommunicationFramework.Create(HashPoolSize: Integer);
var
  st: TStatisticsType;
  d: double;
begin
  inherited Create;
  FCritical := TCritical.Create;
  FCommandList := THashObjectList.CustomCreate(True, 128);
  FIDSeed := 1;
  FPeerIO_HashPool := TUInt32HashObjectList.CustomCreate(HashPoolSize);
  FPeerIO_HashPool.AutoFreeData := False;
  FPeerIO_HashPool.AccessOptimization := False;
  FOnExecuteCommand := nil;
  FOnSendCommand := nil;
  FIdleTimeOut := 0;
  FUsedParallelEncrypt := True;
  FSyncOnResult := False;
  FSyncOnCompleteBuffer := True;
  FEnabledAtomicLockAndMultiThread := True;
  FTimeOutKeepAlive := True;
  FQuietMode := False;
  SetLength(FCipherSecurityArray, 0);
  FSendDataCompressed := True;
  FCompleteBufferCompressed := False;
  FHashSecurity := THashSecurity.hsNone;
  FMaxCompleteBufferSize := C_MaxCompleteBufferSize;
  FPeerIOUserDefineClass := TPeerIOUserDefine;
  FPeerIOUserSpecialClass := TPeerIOUserSpecial;

  FPrintParams := THashVariantList.CustomCreate(128);
  FPrintParams.AutoUpdateDefaultValue := True;

  FPostProgress := TNProgressPostWithCadencer.Create;

  FFrameworkIsServer := True;
  FFrameworkIsClient := True;
  FFrameworkInfo := ClassName;

  FOnProgressRuning := False;
  FOnProgress := nil;

  FCMDWithThreadRuning := 0;

  FIOInterface := nil;
  FVMInterface := nil;
  FOnBigStreamInterface := nil;

  FProtocol := cpZServer;
  FSequencePacketActivted := {$IFDEF UsedSequencePacket}True{$ELSE UsedSequencePacket}False{$ENDIF UsedSequencePacket};

  FPrefixName := '';
  FName := '';

  d := umlNow();
  FInitedTimeMD5 := umlMD5(@d, C_Double_Size);

  FCustomUserData := nil;
  FCustomUserObject := nil;

  InitAutomatedP2PVM();

  InitLargeScaleIOPool();

  for st := low(TStatisticsType) to high(TStatisticsType) do
      Statistics[st] := 0;
  CmdRecvStatistics := THashVariantList.CustomCreate(128);
  CmdSendStatistics := THashVariantList.CustomCreate(128);
  CmdMaxExecuteConsumeStatistics := THashVariantList.CustomCreate(128);

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
  FreeAutomatedP2PVM();
  SetLength(FCipherSecurityArray, 0);
  DeleteRegistedCMD(C_BuildP2PAuthToken);
  DeleteRegistedCMD(C_InitP2PTunnel);
  DeleteRegistedCMD(C_CloseP2PTunnel);
  DisposeObject(FCommandList);
  DisposeObject(FPeerIO_HashPool);
  DisposeObject(FPrintParams);
  DisposeObject(FPostProgress);
  DisposeObject([CmdRecvStatistics, CmdSendStatistics, CmdMaxExecuteConsumeStatistics]);
  DisposeObject(FCritical);
  FreeLargeScaleIOPool();
  inherited Destroy;
end;

procedure TCommunicationFramework.WriteCustomBuffer(P_IO: TPeerIO; const buffer: PByte; const Size: NativeInt);
begin
  P_IO.WriteCustomBuffer(buffer, Size);
end;

procedure TCommunicationFramework.p2pVMTunnelAuth(Sender: TPeerIO; const Token: SystemString; var Accept: Boolean);
begin
  if FVMInterface <> nil then
      FVMInterface.p2pVMTunnelAuth(Sender, Token, Accept);
  if (not Accept) and (FAutomatedP2PVMService) then
      Accept := SameText(FAutomatedP2PVMAuthToken, Token);
end;

procedure TCommunicationFramework.p2pVMTunnelOpenBefore(Sender: TPeerIO; p2pVMTunnel: TCommunicationFrameworkWithP2PVM);
var
  i: Integer;
begin
  if FVMInterface <> nil then
      FVMInterface.p2pVMTunnelOpenBefore(Sender, p2pVMTunnel);

  if FAutomatedP2PVMService then
    for i := 0 to FAutomatedP2PVMServiceBind.Count - 1 do
        p2pVMTunnel.InstallLogicFramework(FAutomatedP2PVMServiceBind[i]^.Service);
end;

procedure TCommunicationFramework.p2pVMTunnelOpen(Sender: TPeerIO; p2pVMTunnel: TCommunicationFrameworkWithP2PVM);
begin
  if FVMInterface <> nil then
      FVMInterface.p2pVMTunnelOpen(Sender, p2pVMTunnel);
end;

procedure TCommunicationFramework.p2pVMTunnelOpenAfter(Sender: TPeerIO; p2pVMTunnel: TCommunicationFrameworkWithP2PVM);
begin
  Sender.ResetSequencePacketBuffer;
  Sender.FSequencePacketSignal := True;
  Sender.SequencePacketVerifyTick := GetTimeTick;
  if FVMInterface <> nil then
      FVMInterface.p2pVMTunnelOpenAfter(Sender, p2pVMTunnel);
end;

procedure TCommunicationFramework.p2pVMTunnelClose(Sender: TPeerIO; p2pVMTunnel: TCommunicationFrameworkWithP2PVM);
begin
  if FVMInterface <> nil then
      FVMInterface.p2pVMTunnelClose(Sender, p2pVMTunnel);
end;

function TCommunicationFramework.AutomatedP2PVMClientConnectionDone(P_IO: TPeerIO): Boolean;
begin
  Result := (P_IO.FAutomatedP2PVMClient_Connection_Sequence_Successed = FAutomatedP2PVMClientBind.Count);
end;

procedure TCommunicationFramework.AutomatedP2PVM_Open(P_IO: TPeerIO);
begin
  if FAutomatedP2PVMClient then
      FPostProgress.PostExecuteM(FAutomatedP2PVMClientDelayBoot, {$IFDEF FPC}@{$ENDIF FPC}DoAutomatedP2PVMClient_DelayRequest).Data3 := P_IO.ID;
end;

procedure TCommunicationFramework.AutomatedP2PVM_Open_C(P_IO: TPeerIO; OnResult: TIOStateCall);
begin
  P_IO.FOnAutomatedP2PVMClientConnectionDoneCall := OnResult;
  P_IO.FOnAutomatedP2PVMClientConnectionDoneMethod := nil;
  P_IO.FOnAutomatedP2PVMClientConnectionDoneProc := nil;
  AutomatedP2PVM_Open(P_IO);
end;

procedure TCommunicationFramework.AutomatedP2PVM_Open_M(P_IO: TPeerIO; OnResult: TIOStateMethod);
begin
  P_IO.FOnAutomatedP2PVMClientConnectionDoneCall := nil;
  P_IO.FOnAutomatedP2PVMClientConnectionDoneMethod := OnResult;
  P_IO.FOnAutomatedP2PVMClientConnectionDoneProc := nil;
  AutomatedP2PVM_Open(P_IO);
end;

procedure TCommunicationFramework.AutomatedP2PVM_Open_P(P_IO: TPeerIO; OnResult: TIOStateProc);
begin
  P_IO.FOnAutomatedP2PVMClientConnectionDoneCall := nil;
  P_IO.FOnAutomatedP2PVMClientConnectionDoneMethod := nil;
  P_IO.FOnAutomatedP2PVMClientConnectionDoneProc := OnResult;
  AutomatedP2PVM_Open(P_IO);
end;

procedure TCommunicationFramework.AutomatedP2PVM_Close(P_IO: TPeerIO);
begin
  P_IO.CloseP2PVMTunnel;
end;

procedure TCommunicationFramework.SwitchMaxPerformance;
begin
  FUsedParallelEncrypt := False;
  FHashSecurity := THashSecurity.hsNone;
  FSendDataCompressed := False;
  SetLength(FCipherSecurityArray, 1);
  FCipherSecurityArray[0] := csNone;
end;

procedure TCommunicationFramework.SwitchMaxSecurity;
const
  C_CipherSecurity: array [0 .. 4] of TCipherSecurity = (csRC6, csSerpent, csMars, csRijndael, csTwoFish);
var
  i: Integer;
begin
  FUsedParallelEncrypt := True;
  FHashSecurity := THashSecurity.hsSHA512;
  FSendDataCompressed := True;
  SetLength(FCipherSecurityArray, Length(C_CipherSecurity));
  for i := Low(C_CipherSecurity) to high(C_CipherSecurity) do
      FCipherSecurityArray[i] := C_CipherSecurity[i];
end;

procedure TCommunicationFramework.SwitchDefaultPerformance;
const
  C_CipherSecurity: array [0 .. 9] of TCipherSecurity = (csDES64, csDES128, csDES192, csBlowfish, csLBC, csLQC, csRNG32, csRNG64, csLSC, csXXTea512);
var
  i: Integer;
begin
  FUsedParallelEncrypt := True;
  FHashSecurity := THashSecurity.hsFastMD5;
  FSendDataCompressed := True;
  SetLength(FCipherSecurityArray, Length(C_CipherSecurity));
  for i := Low(C_CipherSecurity) to high(C_CipherSecurity) do
      FCipherSecurityArray[i] := C_CipherSecurity[i];
end;

procedure TCommunicationFramework.Lock_All_IO;
begin
  if FEnabledAtomicLockAndMultiThread then
      FCritical.Acquire; { atomic lock }
  AtomInc(Statistics[TStatisticsType.stCommunicationFrameworkLock]);
end;

procedure TCommunicationFramework.UnLock_All_IO;
begin
  if FEnabledAtomicLockAndMultiThread then
      FCritical.Release; { atomic lock }
  AtomInc(Statistics[TStatisticsType.stCommunicationFrameworkUnLock]);
end;

function TCommunicationFramework.IOBusy: Boolean;
var
  IOArry: TIO_Array;
  i: Integer;
  P_IO: TPeerIO;
begin
  Result := False;
  if FPeerIO_HashPool.Count = 0 then
      exit;

  GetIO_Array(IOArry);
  for i := Low(IOArry) to High(IOArry) do
    begin
      P_IO := TPeerIO(FPeerIO_HashPool[IOArry[i]]);
      if (P_IO <> nil) and (P_IO.IOBusy) then
        begin
          Result := True;
          Break;
        end;
    end;
  SetLength(IOArry, 0);
end;

procedure TCommunicationFramework.Progress;
var
  i: Integer;
begin
  if FOnProgressRuning then
      exit;

  { anti Dead loop }
  FOnProgressRuning := True;

  try
    if Assigned(ProgressBackgroundProc) then
        ProgressBackgroundProc();
  except
  end;

  try
    if Assigned(ProgressBackgroundMethod) then
        ProgressBackgroundMethod();
  except
  end;

  { large-scale Progress }
  ProgressLargeScaleIOPool();

  try
    if FAutomatedP2PVMService then
      for i := 0 to FAutomatedP2PVMServiceBind.Count - 1 do
          FAutomatedP2PVMServiceBind[i]^.Service.Progress;
  except
  end;

  try
    if FAutomatedP2PVMClient then
      for i := 0 to FAutomatedP2PVMClientBind.Count - 1 do
          FAutomatedP2PVMClientBind[i]^.Client.Progress;
  except
  end;

  try
      ProgressPost.Progress;
  except
  end;

  try
    if Assigned(FOnProgress) then
        FOnProgress(Self);
  except
  end;

  { anti Dead loop }
  FOnProgressRuning := False;
end;

procedure TCommunicationFramework.ProgressPeerIOC(OnBackcall: TPeerIOListCall);
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

procedure TCommunicationFramework.ProgressPeerIOM(OnBackcall: TPeerIOListMethod);
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

procedure TCommunicationFramework.ProgressPeerIOP(OnBackcall: TPeerIOListProc);
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

procedure TCommunicationFramework.FastProgressPeerIOC(OnBackcall: TPeerIOListCall);
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
              OnBackcall(TPeerIO(p^.data));
          except
          end;
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure TCommunicationFramework.FastProgressPeerIOM(OnBackcall: TPeerIOListMethod);
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
              OnBackcall(TPeerIO(p^.data));
          except
          end;
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure TCommunicationFramework.FastProgressPeerIOP(OnBackcall: TPeerIOListProc);
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
              OnBackcall(TPeerIO(p^.data));
          except
          end;
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure TCommunicationFramework.GetIO_Array(out IO_Array: TIO_Array);
var
  i: Integer;
  p: PUInt32HashListObjectStruct;
begin
  Lock_All_IO;
  try
    SetLength(IO_Array, FPeerIO_HashPool.Count);
    if (FPeerIO_HashPool.Count > 0) then
      begin
        i := 0;
        p := FPeerIO_HashPool.FirstPtr;
        while i < FPeerIO_HashPool.Count do
          begin
            IO_Array[i] := TPeerIO(p^.data).FID;
            inc(i);
            p := p^.Next;
          end;
      end;
  finally
      UnLock_All_IO;
  end;
end;

procedure TCommunicationFramework.ProgressWaitSend(P_IO: TPeerIO);
begin
  Progress;
end;

procedure TCommunicationFramework.Print(const v: SystemString);
begin
  DoPrint(v);
end;

procedure TCommunicationFramework.PrintParam(v: SystemString; Args: SystemString);
begin
  try
    if (FPrintParams.GetDefaultValue(Args, True) = True) then
        Print(Format(v, [Args]));
  except
      Print(Format(v, [Args]));
  end;
end;

procedure TCommunicationFramework.Error(const v: SystemString);
begin
  DoError(v);
end;

procedure TCommunicationFramework.ErrorParam(v: SystemString; Args: SystemString);
begin
  DoError(Format(v, [Args]));
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
      RaiseInfo(Format('exists cmd: %s', [Cmd]));
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
      RaiseInfo(Format('exists cmd: %s', [Cmd]));
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
      RaiseInfo(Format('exists cmd: %s', [Cmd]));
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
      RaiseInfo(Format('exists cmd: %s', [Cmd]));
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
      RaiseInfo(Format('exists cmd: %s', [Cmd]));
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
      RaiseInfo(Format('exists cmd: %s', [Cmd]));
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
begin
  PrintRegistedCMD('', True);
end;

procedure TCommunicationFramework.PrintRegistedCMD(prefix: SystemString; incl_internalCMD: Boolean);
var
  l: TListPascalString;
  i: Integer;
begin
  l := TListPascalString.Create;
  FCommandList.GetNameList(l);
  for i := 0 to l.Count - 1 do
    if incl_internalCMD or (not umlMultipleMatch('__@*', l[i])) then
        Print(prefix + l.Objects[i].ClassName + ': ' + l[i]);
  DisposeObject(l);
end;

procedure TCommunicationFramework.PrintRegistedCMD(prefix: SystemString);
begin
  PrintRegistedCMD(prefix, True);
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
      Sender.PrintCommand('no exists console cmd: %s', Cmd);
      exit;
    end;
  if not b.InheritsFrom(TCommandConsole) then
    begin
      Sender.PrintCommand('Illegal interface in cmd: %s', Cmd);
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
      Sender.PrintCommand('no exists stream cmd: %s', Cmd);
      exit;
    end;
  if not b.InheritsFrom(TCommandStream) then
    begin
      Sender.PrintCommand('Illegal interface in cmd: %s', Cmd);
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
      Sender.PrintCommand('no exists direct console cmd: %s', Cmd);
      exit;
    end;
  if not b.InheritsFrom(TCommandDirectStream) then
    begin
      Sender.PrintCommand('Illegal interface in cmd: %s', Cmd);
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
      Sender.PrintCommand('no exists direct stream cmd: %s', Cmd);
      exit;
    end;
  if not b.InheritsFrom(TCommandDirectConsole) then
    begin
      Sender.PrintCommand('Illegal interface in cmd: %s', Cmd);
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
      Sender.PrintCommand('no exists Big Stream cmd: %s', Cmd);
      exit;
    end;
  if not b.InheritsFrom(TCommandBigStream) then
    begin
      Sender.PrintCommand('Illegal interface in cmd: %s', Cmd);
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
      Sender.PrintCommand('no exists complete buffer cmd: %s', Cmd);
      exit;
    end;
  if not b.InheritsFrom(TCommandCompleteBuffer) then
    begin
      Sender.PrintCommand('Illegal interface in cmd: %s', Cmd);
      exit;
    end;
  Result := TCommandCompleteBuffer(b).Execute(Sender, InData, DataSize);
end;

function TCommunicationFramework.FirstIO: TPeerIO;
begin
  Result := TPeerIO(FPeerIO_HashPool.First);
end;

function TCommunicationFramework.LastIO: TPeerIO;
begin
  Result := TPeerIO(FPeerIO_HashPool.Last);
end;

function TCommunicationFramework.ExistsID(IO_ID: Cardinal): Boolean;
begin
  Result := FPeerIO_HashPool.Exists(IO_ID);
end;

function TCommunicationFramework.GetRandomCipherSecurity: TCipherSecurity;
begin
  if Length(FCipherSecurityArray) > 0 then
      Result := FCipherSecurityArray[umlRandomRange(Low(FCipherSecurityArray), High(FCipherSecurityArray))]
  else
      Result := csNone;
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
  OutData.WriteByte(Byte(Sender.FSendDataCipherSecurity));
  OutData.WriteArrayByte.SetBuff(@Sender.FCipherKey[0], Length(Sender.FCipherKey));
  OutData.WriteMD5(FInitedTimeMD5);

  Sender.FRemoteExecutedForConnectInit := True;

  DoIOConnectAfter(Sender);

  if FAutomatedP2PVMClient then
      AutomatedP2PVM_Open(Sender);
end;

procedure TCommunicationFrameworkServer.Command_Wait(Sender: TPeerIO; InData: SystemString; var OutData: SystemString);
begin
  OutData := IntToHex(GetTimeTick, SizeOf(TTimeTick) * 2);
end;

procedure TCommunicationFrameworkServer.Framework_InternalIOCreate(const Sender: TPeerIO);
begin
  DoIOConnectBefore(Sender);
  inherited Framework_InternalIOCreate(Sender);
  if FProtocol = cpCustom then
      DoIOConnectAfter(Sender);
end;

procedure TCommunicationFrameworkServer.Framework_InternalIODestroy(const Sender: TPeerIO);
begin
  DoIODisconnect(Sender);
  inherited Framework_InternalIODestroy(Sender);
end;

procedure TCommunicationFrameworkServer.SyncFillCustomBuffer;
begin
  if Assigned(FOnServerCustomProtocolReceiveBufferNotify) then
    begin
      FOnServerCustomProtocolReceiveBufferNotify(FillSync_Sender, FillSync_Buffer, FillSync_BufferSize, FillSync_Done);

      if not FillSync_Done then
          OnReceiveBuffer(FillSync_Sender, FillSync_Buffer, FillSync_BufferSize, FillSync_Done);
    end
  else
      OnReceiveBuffer(FillSync_Sender, FillSync_Buffer, FillSync_BufferSize, FillSync_Done);
end;

procedure TCommunicationFrameworkServer.FillCustomBuffer(Sender: TPeerIO; const th: TCoreClassThread; const buffer: PByte; const Size: NativeInt; var FillDone: Boolean);
begin
  if Protocol = cpCustom then
    begin
      FillSync_Sender := Sender;
      FillSync_Buffer := buffer;
      FillSync_BufferSize := Size;
      FillSync_Done := FillDone;
      Sender.IO_SyncMethod(th, th <> nil, {$IFDEF FPC}@{$ENDIF FPC}SyncFillCustomBuffer);
      FillDone := FillSync_Done;
    end;
end;

constructor TCommunicationFrameworkServer.Create;
begin
  CreateCustomHashPool(10 * 10000);
end;

constructor TCommunicationFrameworkServer.CreateCustomHashPool(HashPoolSize: Integer);
begin
  inherited Create(HashPoolSize);
  FOnServerCustomProtocolReceiveBufferNotify := nil;
  FillSync_Sender := nil;
  FillSync_Buffer := nil;
  FillSync_BufferSize := 0;
  FillSync_Done := True;

  FStableIOProgressing := False;
  FStableIO := nil;

  FSyncOnResult := True;
  FSyncOnCompleteBuffer := True;

  RegisterStream(C_CipherModel).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_CipherModel;
  RegisterConsole(C_Wait).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_Wait;

  FFrameworkIsServer := True;
  FFrameworkIsClient := False;

  Name := 'Server';
end;

destructor TCommunicationFrameworkServer.Destroy;
begin
  if (FStableIO <> nil) and (not FStableIO.AutoFreePhysicsServer) then
    begin
      FStableIO.PhysicsServer := nil;
      DisposeObject(FStableIO);
      FStableIO := nil;
    end;

  while FCMDWithThreadRuning > 0 do
      CheckThreadSynchronize(1);

  DeleteRegistedCMD(C_CipherModel);
  DeleteRegistedCMD(C_Wait);
  inherited Destroy;
end;

procedure TCommunicationFrameworkServer.Progress;
begin
  inherited Progress;

  if (FStableIO <> nil) and (not FStableIOProgressing) then
    begin
      FStableIOProgressing := True;
      FStableIO.Progress;
      FStableIOProgressing := False;
    end;
end;

function TCommunicationFrameworkServer.StableIO: TCommunicationFramework_StableServer;
begin
  if FStableIO = nil then
    begin
      FStableIO := TCommunicationFramework_StableServer.Create;
      FStableIO.AutoFreePhysicsServer := False;
      FStableIO.AutoProgressPhysicsServer := True;
      FStableIO.PhysicsServer := Self;
    end;

  Result := FStableIO;
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

procedure TCommunicationFrameworkServer.OnReceiveBuffer(Sender: TPeerIO; const buffer: PByte; const Size: NativeInt; var FillDone: Boolean);
begin
end;

procedure TCommunicationFrameworkServer.WriteBuffer(P_IO: TPeerIO; const buffer: PByte; const Size: NativeInt);
begin
  WriteCustomBuffer(P_IO, buffer, Size);
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

procedure TCommunicationFrameworkServer.DoIOConnectBefore(Sender: TPeerIO);
begin
end;

procedure TCommunicationFrameworkServer.DoIOConnectAfter(Sender: TPeerIO);
begin
end;

procedure TCommunicationFrameworkServer.DoIODisconnect(Sender: TPeerIO);
begin
end;

procedure TCommunicationFrameworkServer.SendConsoleCmdM(P_IO: TPeerIO; const Cmd, ConsoleData: SystemString; OnResult: TConsoleMethod);
var
  p: PQueueData;
begin
  { init queue data }
  if (P_IO = nil) or (not P_IO.Connected) then
      exit;
  if not CanSendCommand(P_IO, Cmd) then
      exit;

  P_IO.PrintCommand('Send Console cmd: %s', Cmd);

  p := NewQueueData;
  p^.State := TQueueState.qsSendConsoleCMD;
  p^.IO_ID := P_IO.ID;
  p^.Cmd := Cmd;
  p^.Cipher := P_IO.FSendDataCipherSecurity;
  p^.ConsoleData := ConsoleData;
  p^.OnConsoleMethod := OnResult;
  TriggerQueueData(p);
end;

procedure TCommunicationFrameworkServer.SendConsoleCmdM(P_IO: TPeerIO; const Cmd, ConsoleData: SystemString; Param1: Pointer; Param2: TObject; OnResult: TConsoleParamMethod);
var
  p: PQueueData;
begin
  { init queue data }
  if (P_IO = nil) or (not P_IO.Connected) then
      exit;
  if not CanSendCommand(P_IO, Cmd) then
      exit;

  P_IO.PrintCommand('Send Console cmd: %s', Cmd);

  p := NewQueueData;
  p^.State := TQueueState.qsSendConsoleCMD;
  p^.IO_ID := P_IO.ID;
  p^.Cmd := Cmd;
  p^.Cipher := P_IO.FSendDataCipherSecurity;
  p^.ConsoleData := ConsoleData;
  p^.OnConsoleParamMethod := OnResult;
  p^.Param1 := Param1;
  p^.Param2 := Param2;
  TriggerQueueData(p);
end;

procedure TCommunicationFrameworkServer.SendConsoleCmdM(P_IO: TPeerIO; const Cmd, ConsoleData: SystemString; Param1: Pointer; Param2: TObject; OnResult: TConsoleParamMethod; OnFailed: TConsoleFailedMethod);
var
  p: PQueueData;
begin
  { init queue data }
  if (P_IO = nil) or (not P_IO.Connected) then
      exit;
  if not CanSendCommand(P_IO, Cmd) then
      exit;

  P_IO.PrintCommand('Send Console cmd: %s', Cmd);

  p := NewQueueData;
  p^.State := TQueueState.qsSendConsoleCMD;
  p^.IO_ID := P_IO.ID;
  p^.Cmd := Cmd;
  p^.Cipher := P_IO.FSendDataCipherSecurity;
  p^.ConsoleData := ConsoleData;
  p^.OnConsoleParamMethod := OnResult;
  p^.OnConsoleFailedMethod := OnFailed;
  p^.Param1 := Param1;
  p^.Param2 := Param2;
  TriggerQueueData(p);
end;

procedure TCommunicationFrameworkServer.SendStreamCmdM(P_IO: TPeerIO; const Cmd: SystemString; StreamData: TMemoryStream64; OnResult: TStreamMethod; DoneAutoFree: Boolean);
var
  p: PQueueData;
begin
  { init queue data }
  if (P_IO = nil) or (not P_IO.Connected) then
      exit;
  if not CanSendCommand(P_IO, Cmd) then
      exit;

  P_IO.PrintCommand('Send Stream cmd: %s', Cmd);

  p := NewQueueData;
  p^.State := TQueueState.qsSendStreamCMD;
  p^.IO_ID := P_IO.ID;
  p^.Cmd := Cmd;
  p^.Cipher := P_IO.FSendDataCipherSecurity;
  p^.DoneAutoFree := DoneAutoFree;
  p^.StreamData := StreamData;
  p^.OnStreamMethod := OnResult;
  TriggerQueueData(p);
end;

procedure TCommunicationFrameworkServer.SendStreamCmdM(P_IO: TPeerIO; const Cmd: SystemString; StreamData: TDataFrameEngine; OnResult: TStreamMethod);
var
  p: PQueueData;
begin
  { init queue data }
  if (P_IO = nil) or (not P_IO.Connected) then
      exit;
  if not CanSendCommand(P_IO, Cmd) then
      exit;

  P_IO.PrintCommand('Send Stream cmd: %s', Cmd);

  p := NewQueueData;
  p^.State := TQueueState.qsSendStreamCMD;
  p^.IO_ID := P_IO.ID;
  p^.Cmd := Cmd;
  p^.Cipher := P_IO.FSendDataCipherSecurity;
  p^.DoneAutoFree := True;
  p^.StreamData := TMemoryStream64.Create;
  if StreamData <> nil then
      StreamData.EncodeTo(p^.StreamData, True)
  else
      TDataFrameEngine.BuildEmptyStream(p^.StreamData);
  p^.OnStreamMethod := OnResult;
  TriggerQueueData(p);
end;

procedure TCommunicationFrameworkServer.SendStreamCmdM(P_IO: TPeerIO; const Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamMethod);
var
  p: PQueueData;
begin
  { init queue data }
  if (P_IO = nil) or (not P_IO.Connected) then
      exit;
  if not CanSendCommand(P_IO, Cmd) then
      exit;

  P_IO.PrintCommand('Send Stream cmd: %s', Cmd);

  p := NewQueueData;
  p^.State := TQueueState.qsSendStreamCMD;
  p^.IO_ID := P_IO.ID;
  p^.Cmd := Cmd;
  p^.Cipher := P_IO.FSendDataCipherSecurity;
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

procedure TCommunicationFrameworkServer.SendStreamCmdM(P_IO: TPeerIO; const Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamMethod; OnFailed: TStreamFailedMethod);
var
  p: PQueueData;
begin
  { init queue data }
  if (P_IO = nil) or (not P_IO.Connected) then
      exit;
  if not CanSendCommand(P_IO, Cmd) then
      exit;

  P_IO.PrintCommand('Send Stream cmd: %s', Cmd);

  p := NewQueueData;
  p^.State := TQueueState.qsSendStreamCMD;
  p^.IO_ID := P_IO.ID;
  p^.Cmd := Cmd;
  p^.Cipher := P_IO.FSendDataCipherSecurity;
  p^.DoneAutoFree := True;
  p^.StreamData := TMemoryStream64.Create;
  if StreamData <> nil then
      StreamData.EncodeTo(p^.StreamData, True)
  else
      TDataFrameEngine.BuildEmptyStream(p^.StreamData);
  p^.OnStreamParamMethod := OnResult;
  p^.OnStreamFailedMethod := OnFailed;
  p^.Param1 := Param1;
  p^.Param2 := Param2;
  TriggerQueueData(p);
end;

procedure TCommunicationFrameworkServer.SendConsoleCmdP(P_IO: TPeerIO; const Cmd, ConsoleData: SystemString; OnResult: TConsoleProc);
var
  p: PQueueData;
begin
  { init queue data }
  if (P_IO = nil) or (not P_IO.Connected) then
      exit;
  if not CanSendCommand(P_IO, Cmd) then
      exit;

  P_IO.PrintCommand('Send Console cmd: %s', Cmd);

  p := NewQueueData;
  p^.State := TQueueState.qsSendConsoleCMD;
  p^.IO_ID := P_IO.ID;
  p^.Cmd := Cmd;
  p^.Cipher := P_IO.FSendDataCipherSecurity;
  p^.ConsoleData := ConsoleData;
  p^.OnConsoleProc := OnResult;
  TriggerQueueData(p);
end;

procedure TCommunicationFrameworkServer.SendConsoleCmdP(P_IO: TPeerIO; const Cmd, ConsoleData: SystemString; Param1: Pointer; Param2: TObject; OnResult: TConsoleParamProc);
var
  p: PQueueData;
begin
  { init queue data }
  if (P_IO = nil) or (not P_IO.Connected) then
      exit;
  if not CanSendCommand(P_IO, Cmd) then
      exit;

  P_IO.PrintCommand('Send Console cmd: %s', Cmd);

  p := NewQueueData;
  p^.State := TQueueState.qsSendConsoleCMD;
  p^.IO_ID := P_IO.ID;
  p^.Cmd := Cmd;
  p^.Cipher := P_IO.FSendDataCipherSecurity;
  p^.ConsoleData := ConsoleData;
  p^.OnConsoleParamProc := OnResult;
  p^.Param1 := Param1;
  p^.Param2 := Param2;
  TriggerQueueData(p);
end;

procedure TCommunicationFrameworkServer.SendConsoleCmdP(P_IO: TPeerIO; const Cmd, ConsoleData: SystemString; Param1: Pointer; Param2: TObject; OnResult: TConsoleParamProc; OnFailed: TConsoleFailedProc);
var
  p: PQueueData;
begin
  { init queue data }
  if (P_IO = nil) or (not P_IO.Connected) then
      exit;
  if not CanSendCommand(P_IO, Cmd) then
      exit;

  P_IO.PrintCommand('Send Console cmd: %s', Cmd);

  p := NewQueueData;
  p^.State := TQueueState.qsSendConsoleCMD;
  p^.IO_ID := P_IO.ID;
  p^.Cmd := Cmd;
  p^.Cipher := P_IO.FSendDataCipherSecurity;
  p^.ConsoleData := ConsoleData;
  p^.OnConsoleParamProc := OnResult;
  p^.OnConsoleFailedProc := OnFailed;
  p^.Param1 := Param1;
  p^.Param2 := Param2;
  TriggerQueueData(p);
end;

procedure TCommunicationFrameworkServer.SendStreamCmdP(P_IO: TPeerIO; const Cmd: SystemString; StreamData: TMemoryStream64; OnResult: TStreamProc; DoneAutoFree: Boolean);
var
  p: PQueueData;
begin
  { init queue data }
  if (P_IO = nil) or (not P_IO.Connected) then
      exit;
  if not CanSendCommand(P_IO, Cmd) then
      exit;

  P_IO.PrintCommand('Send Stream cmd: %s', Cmd);

  p := NewQueueData;
  p^.State := TQueueState.qsSendStreamCMD;
  p^.IO_ID := P_IO.ID;
  p^.Cmd := Cmd;
  p^.Cipher := P_IO.FSendDataCipherSecurity;
  p^.DoneAutoFree := DoneAutoFree;
  p^.StreamData := StreamData;
  p^.OnStreamProc := OnResult;
  TriggerQueueData(p);
end;

procedure TCommunicationFrameworkServer.SendStreamCmdP(P_IO: TPeerIO; const Cmd: SystemString; StreamData: TDataFrameEngine; OnResult: TStreamProc);
var
  p: PQueueData;
begin
  { init queue data }
  if (P_IO = nil) or (not P_IO.Connected) then
      exit;
  if not CanSendCommand(P_IO, Cmd) then
      exit;

  P_IO.PrintCommand('Send Stream cmd: %s', Cmd);

  p := NewQueueData;
  p^.State := TQueueState.qsSendStreamCMD;
  p^.IO_ID := P_IO.ID;
  p^.Cmd := Cmd;
  p^.Cipher := P_IO.FSendDataCipherSecurity;
  p^.DoneAutoFree := True;
  p^.StreamData := TMemoryStream64.Create;
  if StreamData <> nil then
      StreamData.EncodeTo(p^.StreamData, True)
  else
      TDataFrameEngine.BuildEmptyStream(p^.StreamData);
  p^.OnStreamProc := OnResult;
  TriggerQueueData(p);
end;

procedure TCommunicationFrameworkServer.SendStreamCmdP(P_IO: TPeerIO; const Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamProc);
var
  p: PQueueData;
begin
  { init queue data }
  if (P_IO = nil) or (not P_IO.Connected) then
      exit;
  if not CanSendCommand(P_IO, Cmd) then
      exit;

  P_IO.PrintCommand('Send Stream cmd: %s', Cmd);

  p := NewQueueData;
  p^.State := TQueueState.qsSendStreamCMD;
  p^.IO_ID := P_IO.ID;
  p^.Cmd := Cmd;
  p^.Cipher := P_IO.FSendDataCipherSecurity;
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

procedure TCommunicationFrameworkServer.SendStreamCmdP(P_IO: TPeerIO; const Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamProc; OnFailed: TStreamFailedProc);
var
  p: PQueueData;
begin
  { init queue data }
  if (P_IO = nil) or (not P_IO.Connected) then
      exit;
  if not CanSendCommand(P_IO, Cmd) then
      exit;

  P_IO.PrintCommand('Send Stream cmd: %s', Cmd);

  p := NewQueueData;
  p^.State := TQueueState.qsSendStreamCMD;
  p^.IO_ID := P_IO.ID;
  p^.Cmd := Cmd;
  p^.Cipher := P_IO.FSendDataCipherSecurity;
  p^.DoneAutoFree := True;
  p^.StreamData := TMemoryStream64.Create;
  if StreamData <> nil then
      StreamData.EncodeTo(p^.StreamData, True)
  else
      TDataFrameEngine.BuildEmptyStream(p^.StreamData);
  p^.OnStreamParamProc := OnResult;
  p^.OnStreamFailedProc := OnFailed;
  p^.Param1 := Param1;
  p^.Param2 := Param2;
  TriggerQueueData(p);
end;

procedure TCommunicationFrameworkServer.SendDirectConsoleCmd(P_IO: TPeerIO; const Cmd, ConsoleData: SystemString);
var
  p: PQueueData;
begin
  { init queue data }
  if (P_IO = nil) or (not P_IO.Connected) then
      exit;
  if not CanSendCommand(P_IO, Cmd) then
      exit;
  P_IO.PrintCommand('Send DirectConsole cmd: %s', Cmd);

  p := NewQueueData;
  p^.State := TQueueState.qsSendDirectConsoleCMD;
  p^.IO_ID := P_IO.ID;
  p^.Cmd := Cmd;
  p^.Cipher := P_IO.FSendDataCipherSecurity;
  p^.ConsoleData := ConsoleData;
  TriggerQueueData(p);
end;

procedure TCommunicationFrameworkServer.SendDirectConsoleCmd(P_IO: TPeerIO; const Cmd: SystemString);
begin
  SendDirectConsoleCmd(P_IO, Cmd, '');
end;

procedure TCommunicationFrameworkServer.SendDirectStreamCmd(P_IO: TPeerIO; const Cmd: SystemString; StreamData: TMemoryStream64; DoneAutoFree: Boolean);
var
  p: PQueueData;
begin
  { init queue data }
  if (P_IO = nil) or (not P_IO.Connected) then
      exit;
  if not CanSendCommand(P_IO, Cmd) then
      exit;
  P_IO.PrintCommand('Send DirectStream cmd: %s', Cmd);

  p := NewQueueData;
  p^.State := TQueueState.qsSendDirectStreamCMD;
  p^.IO_ID := P_IO.ID;
  p^.Cmd := Cmd;
  p^.Cipher := P_IO.FSendDataCipherSecurity;
  p^.DoneAutoFree := DoneAutoFree;
  p^.StreamData := StreamData;
  TriggerQueueData(p);
end;

procedure TCommunicationFrameworkServer.SendDirectStreamCmd(P_IO: TPeerIO; const Cmd: SystemString; StreamData: TDataFrameEngine);
var
  p: PQueueData;
begin
  { init queue data }
  if (P_IO = nil) or (not P_IO.Connected) then
      exit;
  if not CanSendCommand(P_IO, Cmd) then
      exit;

  P_IO.PrintCommand('Send DirectStream cmd: %s', Cmd);

  p := NewQueueData;
  p^.State := TQueueState.qsSendDirectStreamCMD;
  p^.IO_ID := P_IO.ID;
  p^.Cmd := Cmd;
  p^.Cipher := P_IO.FSendDataCipherSecurity;
  p^.DoneAutoFree := True;
  p^.StreamData := TMemoryStream64.Create;
  if StreamData <> nil then
      StreamData.EncodeTo(p^.StreamData, True)
  else
      TDataFrameEngine.BuildEmptyStream(p^.StreamData);
  TriggerQueueData(p);
end;

procedure TCommunicationFrameworkServer.SendDirectStreamCmd(P_IO: TPeerIO; const Cmd: SystemString);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  SendDirectStreamCmd(P_IO, Cmd, de);
  DisposeObject(de);
end;

function TCommunicationFrameworkServer.WaitSendConsoleCmd(P_IO: TPeerIO; const Cmd, ConsoleData: SystemString; Timeout: TTimeTick): SystemString;
var
  waitIntf: TWaitSendConsoleCmdIntf;
  timetick: TTimeTick;
begin
  if (P_IO = nil) or (not P_IO.Connected) then
      exit('');
  if not CanSendCommand(P_IO, Cmd) then
      exit('');

  P_IO.PrintCommand('Begin Wait Console cmd: %s', Cmd);

  timetick := GetTimeTick + Timeout;

  while P_IO.WaitOnResult or P_IO.BigStreamReceiveing or P_IO.FWaitSendBusy do
    begin
      ProgressWaitSend(P_IO);
      if not Exists(P_IO) then
          exit;
      if (Timeout > 0) and (GetTimeTick > timetick) then
          exit('');
    end;

  if not Exists(P_IO) then
      exit('');

  P_IO.FWaitSendBusy := True;

  try
    waitIntf := TWaitSendConsoleCmdIntf.Create;
    waitIntf.Done := False;
    waitIntf.NewResult := '';
    SendConsoleCmdM(P_IO, Cmd, ConsoleData, {$IFDEF FPC}@{$ENDIF FPC}waitIntf.WaitSendConsoleResultEvent);
    while not waitIntf.Done do
      begin
        ProgressWaitSend(P_IO);
        if not Exists(P_IO) then
            Break;
        if (Timeout > 0) and (GetTimeTick > timetick) then
            Break;
      end;
    Result := waitIntf.NewResult;
    if waitIntf.Done then
        DisposeObject(waitIntf);
    P_IO.PrintCommand('End Wait Console cmd: %s', Cmd);
  except
      Result := '';
  end;

  if Exists(P_IO) then
      P_IO.FWaitSendBusy := False;
end;

procedure TCommunicationFrameworkServer.WaitSendStreamCmd(P_IO: TPeerIO; const Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; Timeout: TTimeTick);
var
  waitIntf: TWaitSendStreamCmdIntf;
  timetick: Cardinal;
begin
  if (P_IO = nil) or (not P_IO.Connected) then
      exit;
  if not CanSendCommand(P_IO, Cmd) then
      exit;

  P_IO.PrintCommand('Begin Wait Stream cmd: %s', Cmd);

  timetick := GetTimeTick + Timeout;

  while P_IO.WaitOnResult or P_IO.BigStreamReceiveing or P_IO.FWaitSendBusy do
    begin
      ProgressWaitSend(P_IO);
      if not Exists(P_IO) then
          exit;
      if (Timeout > 0) and (GetTimeTick > timetick) then
          exit;
    end;

  if not Exists(P_IO) then
      exit;

  P_IO.FWaitSendBusy := True;

  try
    waitIntf := TWaitSendStreamCmdIntf.Create;
    waitIntf.Done := False;
    SendStreamCmdM(P_IO, Cmd, StreamData, {$IFDEF FPC}@{$ENDIF FPC}waitIntf.WaitSendStreamResultEvent);
    while not waitIntf.Done do
      begin
        ProgressWaitSend(P_IO);
        if not Exists(P_IO) then
            Break;
        if (Timeout > 0) and (GetTimeTick > timetick) then
            Break;
      end;
    if waitIntf.Done then
      begin
        ResultData.Assign(waitIntf.NewResult);
        DisposeObject(waitIntf);
      end;
    P_IO.PrintCommand('End Wait Stream cmd: %s', Cmd);
  except
  end;

  if Exists(P_IO) then
      P_IO.FWaitSendBusy := False;
end;

procedure TCommunicationFrameworkServer.SendBigStream(P_IO: TPeerIO; const Cmd: SystemString; BigStream: TCoreClassStream; StartPos: Int64; DoneAutoFree: Boolean);
var
  p: PQueueData;
begin
  { init queue data }
  if (P_IO = nil) or (not P_IO.Connected) then
      exit;
  if not CanSendCommand(P_IO, Cmd) then
      exit;
  p := NewQueueData;
  p^.State := TQueueState.qsSendBigStream;
  p^.IO_ID := P_IO.ID;
  p^.Cmd := Cmd;
  p^.Cipher := P_IO.FSendDataCipherSecurity;
  p^.BigStreamStartPos := StartPos;
  p^.BigStream := BigStream;
  p^.DoneAutoFree := DoneAutoFree;
  TriggerQueueData(p);
  P_IO.PrintCommand('Send BigStream cmd: %s', Cmd);
end;

procedure TCommunicationFrameworkServer.SendBigStream(P_IO: TPeerIO; const Cmd: SystemString; BigStream: TCoreClassStream; DoneAutoFree: Boolean);
begin
  SendBigStream(P_IO, Cmd, BigStream, 0, DoneAutoFree);
end;

procedure TCommunicationFrameworkServer.SendCompleteBuffer(P_IO: TPeerIO; const Cmd: SystemString; buff: PByte; BuffSize: NativeInt; DoneAutoFree: Boolean);
var
  p: PQueueData;
begin
  { init queue data }
  if (P_IO = nil) or (not P_IO.Connected) then
      exit;
  if not CanSendCommand(P_IO, Cmd) then
      exit;
  P_IO.PrintCommand('Send complete buffer cmd: %s', Cmd);

  p := NewQueueData;
  p^.State := TQueueState.qsSendCompleteBuffer;
  p^.IO_ID := P_IO.ID;
  p^.Cmd := Cmd;
  p^.Cipher := P_IO.FSendDataCipherSecurity;
  p^.buffer := buff;
  p^.BufferSize := BuffSize;
  p^.DoneAutoFree := DoneAutoFree;
  TriggerQueueData(p);
end;

procedure TCommunicationFrameworkServer.SendConsoleCmdM(IO_ID: Cardinal; const Cmd, ConsoleData: SystemString; OnResult: TConsoleMethod);
begin
  SendConsoleCmdM(PeerIO[IO_ID], Cmd, ConsoleData, OnResult);
end;

procedure TCommunicationFrameworkServer.SendConsoleCmdM(IO_ID: Cardinal; const Cmd, ConsoleData: SystemString; Param1: Pointer; Param2: TObject; OnResult: TConsoleParamMethod);
begin
  SendConsoleCmdM(PeerIO[IO_ID], Cmd, ConsoleData, Param1, Param2, OnResult);
end;

procedure TCommunicationFrameworkServer.SendConsoleCmdM(IO_ID: Cardinal; const Cmd, ConsoleData: SystemString; Param1: Pointer; Param2: TObject; OnResult: TConsoleParamMethod; OnFailed: TConsoleFailedMethod);
begin
  SendConsoleCmdM(PeerIO[IO_ID], Cmd, ConsoleData, Param1, Param2, OnResult, OnFailed);
end;

procedure TCommunicationFrameworkServer.SendStreamCmdM(IO_ID: Cardinal; const Cmd: SystemString; StreamData: TMemoryStream64; OnResult: TStreamMethod;
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

procedure TCommunicationFrameworkServer.SendStreamCmdM(IO_ID: Cardinal; const Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamMethod; OnFailed: TStreamFailedMethod);
begin
  SendStreamCmdM(PeerIO[IO_ID], Cmd, StreamData, Param1, Param2, OnResult, OnFailed);
end;

procedure TCommunicationFrameworkServer.SendConsoleCmdP(IO_ID: Cardinal; const Cmd, ConsoleData: SystemString; OnResult: TConsoleProc);
begin
  SendConsoleCmdP(PeerIO[IO_ID], Cmd, ConsoleData, OnResult);
end;

procedure TCommunicationFrameworkServer.SendConsoleCmdP(IO_ID: Cardinal; const Cmd, ConsoleData: SystemString; Param1: Pointer; Param2: TObject; OnResult: TConsoleParamProc);
begin
  SendConsoleCmdP(PeerIO[IO_ID], Cmd, ConsoleData, Param1, Param2, OnResult);
end;

procedure TCommunicationFrameworkServer.SendConsoleCmdP(IO_ID: Cardinal; const Cmd, ConsoleData: SystemString; Param1: Pointer; Param2: TObject; OnResult: TConsoleParamProc; OnFailed: TConsoleFailedProc);
begin
  SendConsoleCmdP(PeerIO[IO_ID], Cmd, ConsoleData, Param1, Param2, OnResult, OnFailed);
end;

procedure TCommunicationFrameworkServer.SendStreamCmdP(IO_ID: Cardinal; const Cmd: SystemString; StreamData: TMemoryStream64; OnResult: TStreamProc; DoneAutoFree: Boolean);
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

procedure TCommunicationFrameworkServer.SendStreamCmdP(IO_ID: Cardinal; const Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamProc; OnFailed: TStreamFailedProc);
begin
  SendStreamCmdP(PeerIO[IO_ID], Cmd, StreamData, Param1, Param2, OnResult, OnFailed);
end;

procedure TCommunicationFrameworkServer.SendDirectConsoleCmd(IO_ID: Cardinal; const Cmd, ConsoleData: SystemString);
begin
  SendDirectConsoleCmd(PeerIO[IO_ID], Cmd, ConsoleData);
end;

procedure TCommunicationFrameworkServer.SendDirectConsoleCmd(IO_ID: Cardinal; const Cmd: SystemString);
begin
  SendDirectConsoleCmd(PeerIO[IO_ID], Cmd, '');
end;

procedure TCommunicationFrameworkServer.SendDirectStreamCmd(IO_ID: Cardinal; const Cmd: SystemString; StreamData: TMemoryStream64; DoneAutoFree: Boolean);
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

function TCommunicationFrameworkServer.WaitSendConsoleCmd(IO_ID: Cardinal; const Cmd, ConsoleData: SystemString; Timeout: TTimeTick): SystemString;
begin
  Result := WaitSendConsoleCmd(PeerIO[IO_ID], Cmd, ConsoleData, Timeout);
end;

procedure TCommunicationFrameworkServer.WaitSendStreamCmd(IO_ID: Cardinal; const Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; Timeout: TTimeTick);
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
  P_IO: TPeerIO;
begin
  GetIO_Array(IO_Array);
  for IO_ID in IO_Array do
    begin
      P_IO := PeerIO[IO_ID];
      if P_IO <> nil then
          SendDirectConsoleCmd(P_IO, Cmd, ConsoleData);
    end;
end;

procedure TCommunicationFrameworkServer.BroadcastDirectStreamCmd(Cmd: SystemString; StreamData: TDataFrameEngine);
var
  IO_ID: Cardinal;
  IO_Array: TIO_Array;
  P_IO: TPeerIO;
begin
  GetIO_Array(IO_Array);
  for IO_ID in IO_Array do
    begin
      P_IO := PeerIO[IO_ID];
      if P_IO <> nil then
          SendDirectStreamCmd(P_IO, Cmd, StreamData);
    end;
end;

function TCommunicationFrameworkServer.GetCount: Integer;
begin
  Result := FPeerIO_HashPool.Count;
end;

function TCommunicationFrameworkServer.Exists(P_IO: TCoreClassObject): Boolean;
begin
  if P_IO is TPeerIO then
      Result := Exists(P_IO as TPeerIO)
  else if P_IO is TPeerIOUserDefine then
      Result := Exists(P_IO as TPeerIOUserDefine)
  else if P_IO is TPeerIOUserSpecial then
      Result := Exists(P_IO as TPeerIOUserSpecial)
  else
      Result := False;
end;

function TCommunicationFrameworkServer.Exists(P_IO: TPeerIO): Boolean;
begin
  Result := FPeerIO_HashPool.ExistsObject(P_IO);
end;

function TCommunicationFrameworkServer.Exists(P_IO: TPeerIOUserDefine): Boolean;
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
          if TPeerIO(p^.data).FUserDefine = P_IO then
            begin
              Result := True;
              exit;
            end;
          inc(i);
          p := p^.Next;
        end;
    end;
end;

function TCommunicationFrameworkServer.Exists(P_IO: TPeerIOUserSpecial): Boolean;
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
          if TPeerIO(p^.data).FUserSpecial = P_IO then
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

procedure TCommunicationFrameworkClient.StreamResult_CipherModel(Sender: TPeerIO; ResultData: TDataFrameEngine);
var
  arr: TDataFrameArrayByte;
begin
  if ResultData.Count > 0 then
    begin
      { index 0: my remote id }
      Sender.ID := ResultData.Reader.ReadCardinal;

      { index 1: Encrypt }
      Sender.SendCipherSecurity := TCipherSecurity(ResultData.Reader.ReadByte);

      { index 2: Encrypt CipherKey }
      arr := ResultData.Reader.ReadArrayByte;
      SetLength(Sender.FCipherKey, arr.Count);
      arr.GetBuff(@Sender.FCipherKey[0]);

      { index 3: remote inited time md5 }
      if ResultData.Reader.NotEnd then
          FInitedTimeMD5 := ResultData.Reader.ReadMD5()
      else
          Sender.PrintError('protocol version mismatch. upgrade zserver from https://github.com/PassByYou888/ZServer4D');

      Sender.RemoteExecutedForConnectInit := True;

      if FConnectInitWaiting then
          TriggerDoConnectFinished;

      CipherModelDone;

      AutomatedP2PVM_Open(Sender);
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
  FLastConnectIsSuccessed := True;
  if FIgnoreProcessConnectedAndDisconnect then
    begin
      if FOnInterface <> nil then
        begin
          try
              FOnInterface.ClientConnected(Self);
          except
          end;
        end;

      Sender.RemoteExecutedForConnectInit := True;
      CipherModelDone;
      FConnectInitWaiting := False;
      exit;
    end;

  FConnectInitWaiting := True;
  if Protocol = cpZServer then
    begin
      FConnectInitWaitingTimeout := GetTimeTick + FAsyncConnectTimeout;

      ClientIO.SendCipherSecurity := TCipherSecurity.csNone;
      de := TDataFrameEngine.Create;
      de.WriteInteger(Integer(CurrentPlatform));
      SendStreamCmdM(C_CipherModel, de, {$IFDEF FPC}@{$ENDIF FPC}StreamResult_CipherModel);
      DisposeObject(de);

      if FOnInterface <> nil then
        begin
          try
              FOnInterface.ClientConnected(Self);
          except
          end;
        end;
    end
  else
    begin
      ClientIO.SendCipherSecurity := TCipherSecurity.csNone;
      if FOnInterface <> nil then
        begin
          try
              FOnInterface.ClientConnected(Self);
          except
          end;
        end;

      Sender.RemoteExecutedForConnectInit := True;
      TriggerDoConnectFinished;
      CipherModelDone;
      FConnectInitWaiting := False;
    end;
end;

procedure TCommunicationFrameworkClient.DoDisconnect(Sender: TPeerIO);
begin
  if not FIgnoreProcessConnectedAndDisconnect then
    begin
      FPeerIO_HashPool.Delete(Sender.FID);
      Sender.FID := 0;
      Sender.FRemoteExecutedForConnectInit := False;
    end;

  if (FLastConnectIsSuccessed) and (FOnInterface <> nil) then
      FOnInterface.ClientDisconnect(Self);
  FLastConnectIsSuccessed := False;
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
  if Assigned(FOnClientCustomProtocolReceiveBufferNotify) then
    begin
      FOnClientCustomProtocolReceiveBufferNotify(Self, FillSync_Buffer, FillSync_BufferSize, FillSync_Done);
      if not FillSync_Done then
          OnReceiveBuffer(FillSync_Buffer, FillSync_BufferSize, FillSync_Done);
    end
  else
      OnReceiveBuffer(FillSync_Buffer, FillSync_BufferSize, FillSync_Done);
end;

procedure TCommunicationFrameworkClient.FillCustomBuffer(Sender: TPeerIO; const th: TCoreClassThread; const buffer: PByte; const Size: NativeInt; var FillDone: Boolean);
begin
  if Protocol = cpCustom then
    begin
      FillSync_Buffer := buffer;
      FillSync_BufferSize := Size;
      FillSync_Done := FillDone;
      Sender.IO_SyncMethod(th, th <> nil, {$IFDEF FPC}@{$ENDIF FPC}SyncFillCustomBuffer);
      FillDone := FillSync_Done;
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
        if Assigned(FOnWaitResultProc) then
            FOnWaitResultProc(True);
      except
      end;

      FOnWaitResultCall := nil;
      FOnWaitResultMethod := nil;
      FOnWaitResultProc := nil;
    end;
end;

function TCommunicationFrameworkClient.GetWaitTimeout(const t: TTimeTick): TTimeTick;
begin
  if t = 0 then
      Result := 1000 * 60 * 5
  else
      Result := t;
end;

constructor TCommunicationFrameworkClient.Create;
begin
  inherited Create(1);
  FMaxCompleteBufferSize := 0; { 0 = infinity }
  FOnClientCustomProtocolReceiveBufferNotify := nil;
  FillSync_Buffer := nil;
  FillSync_BufferSize := 0;
  FillSync_Done := True;

  FStableIOProgressing := False;
  FStableIO := nil;

  FOnInterface := nil;
  FConnectInitWaiting := False;
  FConnectInitWaitingTimeout := 0;

  FWaiting := False;
  FWaitingTimeOut := 0;
  FAsyncConnectTimeout := 60 * 1000;
  FOnCipherModelDone := nil;

  FIgnoreProcessConnectedAndDisconnect := False;
  FLastConnectIsSuccessed := False;

  FOnWaitResultCall := nil;
  FOnWaitResultMethod := nil;
  FOnWaitResultProc := nil;
  FFrameworkIsServer := False;
  FFrameworkIsClient := True;

  Name := 'Client';
end;

destructor TCommunicationFrameworkClient.Destroy;
begin
  if (FStableIO <> nil) and (not FStableIO.AutoFreePhysicsClient) then
    begin
      DisposeObject(FStableIO);
      FStableIO := nil;
    end;
  inherited Destroy;
end;

procedure TCommunicationFrameworkClient.IO_IDLE_TraceC(data: TCoreClassObject; OnNotify: TDataNotifyCall);
begin
  if ClientIO = nil then
      OnNotify(data)
  else
      ClientIO.IO_IDLE_TraceC(data, OnNotify);
end;

procedure TCommunicationFrameworkClient.IO_IDLE_TraceM(data: TCoreClassObject; OnNotify: TDataNotifyMethod);
begin
  if ClientIO = nil then
      OnNotify(data)
  else
      ClientIO.IO_IDLE_TraceM(data, OnNotify);
end;

procedure TCommunicationFrameworkClient.IO_IDLE_TraceP(data: TCoreClassObject; OnNotify: TDataNotifyProc);
begin
  if ClientIO = nil then
      OnNotify(data)
  else
      ClientIO.IO_IDLE_TraceP(data, OnNotify);
end;

procedure TCommunicationFrameworkClient.OnReceiveBuffer(const buffer: PByte; const Size: NativeInt; var FillDone: Boolean);
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

  if (FWaiting) and ((GetTimeTick > FWaitingTimeOut) or (not Connected)) then
    begin
      FWaiting := False;
      FWaitingTimeOut := 0;
      try
        if Assigned(FOnWaitResultCall) then
            FOnWaitResultCall(False);
        if Assigned(FOnWaitResultMethod) then
            FOnWaitResultMethod(False);
        if Assigned(FOnWaitResultProc) then
            FOnWaitResultProc(False);
      except
      end;

      FOnWaitResultCall := nil;
      FOnWaitResultMethod := nil;
      FOnWaitResultProc := nil;
    end;

  if (FStableIO <> nil) and (not FStableIOProgressing) then
    begin
      FStableIOProgressing := True;
      FStableIO.Progress;
      FStableIOProgressing := False;
    end;
end;

function TCommunicationFrameworkClient.StableIO: TCommunicationFramework_StableClient;
begin
  if FStableIO = nil then
    begin
      FStableIO := TCommunicationFramework_StableClient.Create;
      FStableIO.AutoFreePhysicsClient := False;
      FStableIO.AutoProgressPhysicsClient := True;
      FStableIO.PhysicsClient := Self;
    end;

  Result := FStableIO;
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

procedure TCommunicationFrameworkClient.CipherModelDone;
begin
  try
    if Assigned(FOnCipherModelDone) then
        FOnCipherModelDone(Self);
  except
  end;
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

procedure TCommunicationFrameworkClient.AsyncConnectP(addr: SystemString; Port: Word; OnResult: TStateProc);
var
  r: Boolean;
begin
  r := Connect(addr, Port);
  if Assigned(OnResult) then
      OnResult(r);
end;

procedure TCommunicationFrameworkClient.AsyncConnectC(addr: SystemString; Port: Word; Param1: Pointer; Param2: TObject; OnResult: TParamStateCall);
var
  ParamBridge: TStateParamBridge;
begin
  ParamBridge := TStateParamBridge.Create;
  ParamBridge.Param1 := Param1;
  ParamBridge.Param2 := Param2;
  ParamBridge.OnNotifyC := OnResult;
  AsyncConnectM(addr, Port, {$IFDEF FPC}@{$ENDIF FPC}ParamBridge.DoStateResult);
end;

procedure TCommunicationFrameworkClient.AsyncConnectM(addr: SystemString; Port: Word; Param1: Pointer; Param2: TObject; OnResult: TParamStateMethod);
var
  ParamBridge: TStateParamBridge;
begin
  ParamBridge := TStateParamBridge.Create;
  ParamBridge.Param1 := Param1;
  ParamBridge.Param2 := Param2;
  ParamBridge.OnNotifyM := OnResult;
  AsyncConnectM(addr, Port, {$IFDEF FPC}@{$ENDIF FPC}ParamBridge.DoStateResult);
end;

procedure TCommunicationFrameworkClient.AsyncConnectP(addr: SystemString; Port: Word; Param1: Pointer; Param2: TObject; OnResult: TParamStateProc);
var
  ParamBridge: TStateParamBridge;
begin
  ParamBridge := TStateParamBridge.Create;
  ParamBridge.Param1 := Param1;
  ParamBridge.Param2 := Param2;
  ParamBridge.OnNotifyP := OnResult;
  AsyncConnectM(addr, Port, {$IFDEF FPC}@{$ENDIF FPC}ParamBridge.DoStateResult);
end;

function TCommunicationFrameworkClient.Connect(addr: SystemString; Port: Word): Boolean;
begin
  Result := False;
end;

procedure TCommunicationFrameworkClient.Disconnect;
begin
end;

procedure TCommunicationFrameworkClient.DelayClose;
begin
  try
    if ClientIO <> nil then
        ClientIO.DelayClose;
  except
  end;
end;

procedure TCommunicationFrameworkClient.DelayClose(const t: double);
begin
  try
    if ClientIO <> nil then
        ClientIO.DelayClose(t);
  except
  end;
end;

{ sync KeepAlive }
function TCommunicationFrameworkClient.Wait(TimeOut_: TTimeTick): SystemString;
begin
  Result := '';
  if (ClientIO = nil) then
      exit;
  if (not Connected) then
      exit;

  Result := WaitSendConsoleCmd(C_Wait, '', GetWaitTimeout(TimeOut_));
end;

function TCommunicationFrameworkClient.WaitC(TimeOut_: TTimeTick; OnResult: TStateCall): Boolean;
begin
  Result := False;
  if (ClientIO = nil) then
      exit;
  if (FWaiting) then
      exit;
  if (not Connected) then
    begin
      if Assigned(OnResult) then
          OnResult(False);
      exit;
    end;

  FWaiting := True;
  FWaitingTimeOut := GetTimeTick + GetWaitTimeout(TimeOut_);
  FOnWaitResultCall := OnResult;
  FOnWaitResultMethod := nil;
  FOnWaitResultProc := nil;
  SendConsoleCmdM(C_Wait, '', {$IFDEF FPC}@{$ENDIF FPC}ConsoleResult_Wait);
  Result := True;
end;

function TCommunicationFrameworkClient.WaitM(TimeOut_: TTimeTick; OnResult: TStateMethod): Boolean;
begin
  Result := False;
  if (ClientIO = nil) then
      exit;
  if (FWaiting) then
      exit;
  if (not Connected) then
    begin
      if Assigned(OnResult) then
          OnResult(False);
      exit;
    end;

  FWaiting := True;
  FWaitingTimeOut := GetTimeTick + GetWaitTimeout(TimeOut_);
  FOnWaitResultCall := nil;
  FOnWaitResultMethod := OnResult;
  FOnWaitResultProc := nil;
  SendConsoleCmdM(C_Wait, '', {$IFDEF FPC}@{$ENDIF FPC}ConsoleResult_Wait);

  Result := True;
end;

function TCommunicationFrameworkClient.WaitP(TimeOut_: TTimeTick; OnResult: TStateProc): Boolean;
begin
  Result := False;
  if (ClientIO = nil) then
      exit;
  if (FWaiting) then
      exit;
  if (not Connected) then
    begin
      if Assigned(OnResult) then
          OnResult(False);
      exit;
    end;

  FWaiting := True;
  FWaitingTimeOut := GetTimeTick + GetWaitTimeout(TimeOut_);
  FOnWaitResultCall := nil;
  FOnWaitResultMethod := nil;
  FOnWaitResultProc := OnResult;
  SendConsoleCmdM(C_Wait, '', {$IFDEF FPC}@{$ENDIF FPC}ConsoleResult_Wait);
  Result := True;
end;

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

  { init queue data }
  p := NewQueueData;
  p^.State := TQueueState.qsSendConsoleCMD;

  p^.Cmd := Cmd;
  p^.Cipher := ClientIO.FSendDataCipherSecurity;
  p^.ConsoleData := ConsoleData;
  p^.OnConsoleMethod := OnResult;
  TriggerQueueData(p);
end;

procedure TCommunicationFrameworkClient.SendConsoleCmdM(Cmd, ConsoleData: SystemString; Param1: Pointer; Param2: TObject; OnResult: TConsoleParamMethod);
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

  { init queue data }
  p := NewQueueData;
  p^.State := TQueueState.qsSendConsoleCMD;

  p^.Cmd := Cmd;
  p^.Cipher := ClientIO.FSendDataCipherSecurity;
  p^.ConsoleData := ConsoleData;
  p^.OnConsoleParamMethod := OnResult;
  p^.Param1 := Param1;
  p^.Param2 := Param2;
  TriggerQueueData(p);
end;

procedure TCommunicationFrameworkClient.SendConsoleCmdM(Cmd, ConsoleData: SystemString; Param1: Pointer; Param2: TObject; OnResult: TConsoleParamMethod; OnFailed: TConsoleFailedMethod);
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

  { init queue data }
  p := NewQueueData;
  p^.State := TQueueState.qsSendConsoleCMD;

  p^.Cmd := Cmd;
  p^.Cipher := ClientIO.FSendDataCipherSecurity;
  p^.ConsoleData := ConsoleData;
  p^.OnConsoleParamMethod := OnResult;
  p^.OnConsoleFailedMethod := OnFailed;
  p^.Param1 := Param1;
  p^.Param2 := Param2;
  TriggerQueueData(p);
end;

procedure TCommunicationFrameworkClient.SendStreamCmdM(Cmd: SystemString; StreamData: TMemoryStream64; OnResult: TStreamMethod; DoneAutoFree: Boolean);
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

  { init queue data }
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

  { init queue data }
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

  { init queue data }
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

procedure TCommunicationFrameworkClient.SendStreamCmdM(Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamMethod; OnFailed: TStreamFailedMethod);
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

  { init queue data }
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
  p^.OnStreamFailedMethod := OnFailed;
  p^.Param1 := Param1;
  p^.Param2 := Param2;
  TriggerQueueData(p);
end;

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

  { init queue data }
  p := NewQueueData;
  p^.State := TQueueState.qsSendConsoleCMD;

  p^.Cmd := Cmd;
  p^.Cipher := ClientIO.FSendDataCipherSecurity;
  p^.ConsoleData := ConsoleData;
  p^.OnConsoleProc := OnResult;
  TriggerQueueData(p);
end;

procedure TCommunicationFrameworkClient.SendConsoleCmdP(Cmd, ConsoleData: SystemString; Param1: Pointer; Param2: TObject; OnResult: TConsoleParamProc);
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

  { init queue data }
  p := NewQueueData;
  p^.State := TQueueState.qsSendConsoleCMD;

  p^.Cmd := Cmd;
  p^.Cipher := ClientIO.FSendDataCipherSecurity;
  p^.ConsoleData := ConsoleData;
  p^.OnConsoleParamProc := OnResult;
  p^.Param1 := Param1;
  p^.Param2 := Param2;
  TriggerQueueData(p);
end;

procedure TCommunicationFrameworkClient.SendConsoleCmdP(Cmd, ConsoleData: SystemString; Param1: Pointer; Param2: TObject; OnResult: TConsoleParamProc; OnFailed: TConsoleFailedProc);
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

  { init queue data }
  p := NewQueueData;
  p^.State := TQueueState.qsSendConsoleCMD;

  p^.Cmd := Cmd;
  p^.Cipher := ClientIO.FSendDataCipherSecurity;
  p^.ConsoleData := ConsoleData;
  p^.OnConsoleParamProc := OnResult;
  p^.OnConsoleFailedProc := OnFailed;
  p^.Param1 := Param1;
  p^.Param2 := Param2;
  TriggerQueueData(p);
end;

procedure TCommunicationFrameworkClient.SendStreamCmdP(Cmd: SystemString; StreamData: TMemoryStream64; OnResult: TStreamProc; DoneAutoFree: Boolean);
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

  { init queue data }
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

  { init queue data }
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

  { init queue data }
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

procedure TCommunicationFrameworkClient.SendStreamCmdP(Cmd: SystemString; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamProc; OnFailed: TStreamFailedProc);
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

  { init queue data }
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
  p^.OnStreamFailedProc := OnFailed;
  p^.Param1 := Param1;
  p^.Param2 := Param2;
  TriggerQueueData(p);
end;

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

  { init queue data }
  p := NewQueueData;
  p^.State := TQueueState.qsSendDirectConsoleCMD;

  p^.Cmd := Cmd;
  p^.Cipher := ClientIO.FSendDataCipherSecurity;
  p^.ConsoleData := ConsoleData;
  TriggerQueueData(p);
end;

procedure TCommunicationFrameworkClient.SendDirectConsoleCmd(Cmd: SystemString);
begin
  SendDirectConsoleCmd(Cmd, '');
end;

procedure TCommunicationFrameworkClient.SendDirectStreamCmd(Cmd: SystemString; StreamData: TMemoryStream64; DoneAutoFree: Boolean);
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

  { init queue data }
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

  { init queue data }
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

function TCommunicationFrameworkClient.WaitSendConsoleCmd(Cmd, ConsoleData: SystemString; Timeout: TTimeTick): SystemString;
var
  waitIntf: TWaitSendConsoleCmdIntf;
  timetick: TTimeTick;
begin
  Result := '';
  if ClientIO = nil then
      exit;
  if not Connected then
      exit;
  if not CanSendCommand(ClientIO, Cmd) then
      exit;
  ClientIO.PrintCommand('Begin Wait console cmd: %s', Cmd);

  timetick := GetTimeTick + Timeout;

  while ClientIO.WaitOnResult or ClientIO.BigStreamReceiveing or ClientIO.FWaitSendBusy do
    begin
      ProgressWaitSend(ClientIO);
      if not Connected then
          exit;
      if (Timeout > 0) and (GetTimeTick > timetick) then
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
        if (Timeout > 0) and (GetTimeTick > timetick) then
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

procedure TCommunicationFrameworkClient.WaitSendStreamCmd(Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; Timeout: TTimeTick);
var
  waitIntf: TWaitSendStreamCmdIntf;
  timetick: TTimeTick;
begin
  if ClientIO = nil then
      exit;
  if not Connected then
      exit;
  if not CanSendCommand(ClientIO, Cmd) then
      exit;

  ClientIO.PrintCommand('Begin Wait Stream cmd: %s', Cmd);

  timetick := GetTimeTick + Timeout;

  while ClientIO.WaitOnResult or ClientIO.BigStreamReceiveing or ClientIO.FWaitSendBusy do
    begin
      ProgressWaitSend(ClientIO);
      if not Connected then
          exit;
      if (Timeout > 0) and (GetTimeTick > timetick) then
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
        if (Timeout > 0) and (GetTimeTick > timetick) then
            Break;
      end;
    try
      if ClientIO <> nil then
          ClientIO.PrintCommand('End Wait Stream cmd: %s', Cmd);
    except
    end;

    if waitIntf.Done then
      begin
        if ResultData <> nil then
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
  { init queue data }
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
  { init queue data }
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

procedure Tp2pVMFragmentPacket.Init;
begin
  buffSiz := 0;
  FrameworkID := 0;
  p2pID := 0;
  pkType := 0;
  buff := nil;
end;

function Tp2pVMFragmentPacket.FillReceiveBuff(Stream: TMemoryStream64): Integer;
begin
  Result := 0;
  if Stream.Size < 13 then
    begin
      Init;
      exit;
    end;
  if Stream.Size < PCardinal(Stream.PositionAsPtr(0))^ + 13 then
    begin
      Init;
      exit;
    end;
  buffSiz := PCardinal(Stream.PositionAsPtr(0))^;
  FrameworkID := PCardinal(Stream.PositionAsPtr(4))^;
  p2pID := PCardinal(Stream.PositionAsPtr(8))^;
  pkType := PByte(Stream.PositionAsPtr(12))^;
  if buffSiz > 0 then
      buff := Stream.PositionAsPtr(13)
  else
      buff := nil;
  Result := buffSiz + 13;
end;

procedure Tp2pVMFragmentPacket.BuildSendBuff(Stream: TMemoryStream64);
begin
  Stream.WritePtr(@buffSiz, 4);
  Stream.WritePtr(@FrameworkID, 4);
  Stream.WritePtr(@p2pID, 4);
  Stream.WritePtr(@pkType, 1);
  if buffSiz > 0 then
      Stream.WritePtr(buff, buffSiz);
end;

procedure TP2PVM_PeerIO.CreateAfter;
begin
  inherited CreateAfter;
  FSequencePacketActivted := {$IFDEF UsedSequencePacketOnP2PVM}True{$ELSE UsedSequencePacketOnP2PVM}False{$ENDIF UsedSequencePacketOnP2PVM};

  FLinkVM := nil;
  FRealSendBuff := TMemoryStream64.Create;
  FSendQueue := TCoreClassList.Create;
  FRemote_frameworkID := 0;
  FRemote_p2pID := 0;
  FillPtrByte(@FIP, SizeOf(FIP), 0);
  FPort := 0;
  FDestroySyncRemote := True;

  if not FOwnerFramework.FQuietMode then
      Print('VM-IO Create %d', [ID]);
end;

destructor TP2PVM_PeerIO.Destroy;
var
  i: Integer;
begin
  if Connected then
    begin
      if (FDestroySyncRemote) and (FLinkVM <> nil) then
          FLinkVM.SendDisconnect(Remote_frameworkID, Remote_p2pID);
      if not FOwnerFramework.FQuietMode then
          DoStatus('VMClientIO %d disconnect', [ID]);
    end;

  for i := 0 to FSendQueue.Count - 1 do
      FreeP2PVMPacket(FSendQueue[i]);
  DisposeObject(FSendQueue);
  DisposeObject(FRealSendBuff);

  if not FOwnerFramework.FQuietMode then
      Print('VM-IO Destroy %d', [ID]);
  inherited Destroy;
end;

function TP2PVM_PeerIO.Connected: Boolean;
begin
  if FLinkVM = nil then
      Result := False
  else if FOwnerFramework is TCommunicationFrameworkWithP2PVM_Server then
      Result := (FLinkVM.FPhysicsIO <> nil)
  else if FOwnerFramework is TCommunicationFrameworkWithP2PVM_Client then
      Result := TCommunicationFrameworkWithP2PVM_Client(FOwnerFramework).Connected
  else
      Result := False;
end;

procedure TP2PVM_PeerIO.Disconnect;
begin
  DisposeObject(Self);
end;

procedure TP2PVM_PeerIO.SendByteBuffer(const buff: PByte; const Size: NativeInt);
begin
  if Size <= 0 then
      exit;
  FRealSendBuff.Position := FRealSendBuff.Size;
  FRealSendBuff.WritePtr(buff, Size);
end;

procedure TP2PVM_PeerIO.WriteBufferOpen;
begin
  FRealSendBuff.Clear;
end;

procedure TP2PVM_PeerIO.WriteBufferFlush;
var
  p: PByte;
  siz: Int64;
begin
  if FRealSendBuff.Size <= 0 then
      exit;

  if (FLinkVM <> nil) and (Connected) then
    begin
      p := FRealSendBuff.Memory;
      siz := FRealSendBuff.Size;

      { send fragment }
      while siz > FLinkVM.FMaxVMFragmentSize do
        begin
          FSendQueue.Add(BuildP2PVMPacket(FLinkVM.FMaxVMFragmentSize, FRemote_frameworkID, FRemote_p2pID, C_p2pVM_LogicFragmentData, p));
          inc(p, FLinkVM.FMaxVMFragmentSize);
          dec(siz, FLinkVM.FMaxVMFragmentSize);
        end;

      if siz > 0 then
          FSendQueue.Add(BuildP2PVMPacket(siz, FRemote_frameworkID, FRemote_p2pID, C_p2pVM_LogicFragmentData, p));
    end;

  FRealSendBuff.Clear;
end;

procedure TP2PVM_PeerIO.WriteBufferClose;
begin
  WriteBufferFlush;
end;

function TP2PVM_PeerIO.GetPeerIP: SystemString;
begin
  Result := IPv6ToStr(FIP).Text;
end;

function TP2PVM_PeerIO.WriteBufferEmpty: Boolean;
begin
  Result := (FRealSendBuff.Size = 0) and (FSendQueue.Count = 0);
end;

procedure TP2PVM_PeerIO.Progress;
begin
  inherited Progress;
  ProcessAllSendCmd(nil, False, False);
end;

procedure TCommunicationFrameworkWithP2PVM_Server.Connecting(SenderVM: TCommunicationFrameworkWithP2PVM;
  const Remote_frameworkID, FrameworkID: Cardinal; const IPV6: TIPV6; const Port: Word; var Allowed: Boolean);
var
  p: Pp2pVMListen;
  LocalVMc: TP2PVM_PeerIO;
begin
  if FLinkVMPool.Count = 0 then
    begin
      Allowed := False;
      exit;
    end;

  p := SenderVM.FindListen(IPV6, Port);
  Allowed := (p <> nil) and (p^.FrameworkID = FrameworkID);

  if Allowed then
    begin
      { build io }
      LocalVMc := TP2PVM_PeerIO.Create(Self, nil);
      LocalVMc.FLinkVM := SenderVM;
      LocalVMc.FRemote_frameworkID := Remote_frameworkID;
      LocalVMc.FRemote_p2pID := 0;
      LocalVMc.FIP := IPV6;
      LocalVMc.FPort := Port;

      { connected reponse }
      SenderVM.SendConnectedReponse(LocalVMc.FRemote_frameworkID, LocalVMc.FRemote_p2pID, FrameworkID, LocalVMc.ID);

      if not FQuietMode then
          DoStatus('Virtual connecting with "%s port:%d"', [IPv6ToStr(IPV6).Text, Port]);
    end;
end;

procedure TCommunicationFrameworkWithP2PVM_Server.ListenState(SenderVM: TCommunicationFrameworkWithP2PVM; const IPV6: TIPV6; const Port: Word; const State: Boolean);
begin
  if not FQuietMode then
    begin
      if State then
          DoStatus('Virtual Addr: "%s Port:%d" Listen is open', [IPv6ToStr(IPV6).Text, Port])
      else
          DoStatus('Virtual Addr: "%s Port:%d" Listen close!', [IPv6ToStr(IPV6).Text, Port]);
    end;
end;

procedure TCommunicationFrameworkWithP2PVM_Server.ProgressDisconnectClient(P_IO: TPeerIO);
begin
  DisposeObject(P_IO);
end;

function TCommunicationFrameworkWithP2PVM_Server.ListenCount: Integer;
begin
  Result := FFrameworkListenPool.Count;
end;

function TCommunicationFrameworkWithP2PVM_Server.GetListen(const index: Integer): Pp2pVMListen;
begin
  Result := FFrameworkListenPool[index];
end;

function TCommunicationFrameworkWithP2PVM_Server.FindListen(const IPV6: TIPV6; const Port: Word): Pp2pVMListen;
var
  i: Integer;
  p: Pp2pVMListen;
begin
  for i := 0 to FFrameworkListenPool.Count - 1 do
    begin
      p := FFrameworkListenPool[i];
      if (p^.ListenPort = Port) and (CompareIPV6(p^.ListenHost, IPV6)) then
        begin
          Result := p;
          exit;
        end;
    end;
  Result := nil;
end;

function TCommunicationFrameworkWithP2PVM_Server.FindListening(const IPV6: TIPV6; const Port: Word): Pp2pVMListen;
var
  i: Integer;
  p: Pp2pVMListen;
begin
  for i := 0 to FFrameworkListenPool.Count - 1 do
    begin
      p := FFrameworkListenPool[i];
      if (p^.Listening) and (p^.ListenPort = Port) and (CompareIPV6(p^.ListenHost, IPV6)) then
        begin
          Result := p;
          exit;
        end;
    end;
  Result := nil;
end;

procedure TCommunicationFrameworkWithP2PVM_Server.DeleteListen(const IPV6: TIPV6; const Port: Word);
var
  i: Integer;
  p: Pp2pVMListen;
begin
  i := 0;
  while i < FFrameworkListenPool.Count do
    begin
      p := FFrameworkListenPool[i];
      if (p^.ListenPort = Port) and (CompareIPV6(p^.ListenHost, IPV6)) then
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
  CustomCreate(10 * 10000, 0);
end;

constructor TCommunicationFrameworkWithP2PVM_Server.CustomCreate(HashPoolSize: Integer; FrameworkID: Cardinal);
begin
  inherited CreateCustomHashPool(HashPoolSize);
  FEnabledAtomicLockAndMultiThread := False;
  FFrameworkListenPool := TCoreClassList.Create;
  FLinkVMPool := TUInt32HashObjectList.Create;
  FFrameworkWithVM_ID := FrameworkID;
  StopService;
  Name := 'VMServer';
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
            (TCommunicationFrameworkWithP2PVM(p^.data)).UninstallLogicFramework(Self);
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
  ProgressPeerIOM({$IFDEF FPC}@{$ENDIF FPC}ProgressDisconnectClient);
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
      if SenderVM.FFrameworkPool[p^.FrameworkID] = Self then
          lst.Add(p);
    end;

  for i := 0 to lst.Count - 1 do
    begin
      p := lst[i];
      SenderVM.SendListen(p^.FrameworkID, p^.ListenHost, p^.ListenPort, False);
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
              ProgressStopServiceWithPerVM(TCommunicationFrameworkWithP2PVM(p^.data));
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
  IPV6: TIPV6;
  SI: Cardinal;
  i: Integer;
  p: PUInt32HashListObjectStruct;
  LP: Pp2pVMListen;
begin
  Result := False;

  IPV6 := StrToIPv6(Host, Result, SI);

  if not Result then
      exit;

  LP := FindListen(IPV6, Port);
  if LP = nil then
    begin
      new(LP);
      LP^.FrameworkID := FFrameworkWithVM_ID;
      LP^.ListenHost := IPV6;
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
              TCommunicationFrameworkWithP2PVM(p^.data).SendListen(FFrameworkWithVM_ID, IPV6, Port, True);
          except
          end;
          inc(i);
          p := p^.Next;
        end;
    end
  else
    begin
      ListenState(nil, IPV6, Port, True);
    end;
  Result := True;
end;

function TCommunicationFrameworkWithP2PVM_Server.WaitSendConsoleCmd(P_IO: TPeerIO; const Cmd, ConsoleData: SystemString; Timeout: TTimeTick): SystemString;
begin
  Result := '';
  RaiseInfo('WaitSend no Suppport VM server');
end;

procedure TCommunicationFrameworkWithP2PVM_Server.WaitSendStreamCmd(P_IO: TPeerIO; const Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; Timeout: TTimeTick);
begin
  RaiseInfo('WaitSend no Suppport VM server');
end;

procedure TCommunicationFrameworkWithP2PVM_Client.Framework_InternalIOCreate(const Sender: TPeerIO);
begin
  inherited Framework_InternalIOCreate(Sender);
end;

procedure TCommunicationFrameworkWithP2PVM_Client.Framework_InternalIODestroy(const Sender: TPeerIO);
begin
  FVMClientIO := nil;
  FVMConnected := False;
  inherited Framework_InternalIODestroy(Sender);
end;

procedure TCommunicationFrameworkWithP2PVM_Client.VMConnectSuccessed(SenderVM: TCommunicationFrameworkWithP2PVM; Remote_frameworkID, Remote_p2pID, FrameworkID: Cardinal);
begin
  FVMClientIO.FRemote_frameworkID := Remote_frameworkID;
  FVMClientIO.FRemote_p2pID := Remote_p2pID;

  FVMConnected := True;
  DoConnected(FVMClientIO);
end;

procedure TCommunicationFrameworkWithP2PVM_Client.VMDisconnect(SenderVM: TCommunicationFrameworkWithP2PVM);
begin
  FVMConnected := False;
  TriggerDoConnectFailed;
  if FVMClientIO <> nil then
      FVMClientIO.Disconnect;
end;

constructor TCommunicationFrameworkWithP2PVM_Client.Create;
begin
  CustomCreate(0);
end;

constructor TCommunicationFrameworkWithP2PVM_Client.CustomCreate(FrameworkID: Cardinal);
begin
  inherited Create;
  FEnabledAtomicLockAndMultiThread := False;
  FLinkVM := nil;
  FFrameworkWithVM_ID := FrameworkID;
  FVMClientIO := nil;
  FVMConnected := False;
  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := nil;
  FOnAsyncConnectNotifyProc := nil;
  Name := 'VMClientIO';
end;

destructor TCommunicationFrameworkWithP2PVM_Client.Destroy;
begin
  if FVMClientIO <> nil then
      DisposeObject(FVMClientIO);
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
    if Assigned(FOnAsyncConnectNotifyProc) then
        FOnAsyncConnectNotifyProc(False);
  except
  end;

  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := nil;
  FOnAsyncConnectNotifyProc := nil;
end;

procedure TCommunicationFrameworkWithP2PVM_Client.TriggerDoConnectFinished;
begin
  inherited TriggerDoConnectFinished;

  try
    if Assigned(FOnAsyncConnectNotifyCall) then
        FOnAsyncConnectNotifyCall(True);
    if Assigned(FOnAsyncConnectNotifyMethod) then
        FOnAsyncConnectNotifyMethod(True);
    if Assigned(FOnAsyncConnectNotifyProc) then
        FOnAsyncConnectNotifyProc(True);
  except
  end;

  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := nil;
  FOnAsyncConnectNotifyProc := nil;
end;

function TCommunicationFrameworkWithP2PVM_Client.Connected: Boolean;
begin
  Result := (FVMConnected) and (FVMClientIO <> nil);
end;

function TCommunicationFrameworkWithP2PVM_Client.ClientIO: TPeerIO;
begin
  Result := FVMClientIO;
end;

procedure TCommunicationFrameworkWithP2PVM_Client.Progress;
begin
  inherited Progress;
end;

procedure TCommunicationFrameworkWithP2PVM_Client.TriggerQueueData(v: PQueueData);
begin
  if Connected then
    begin
      FVMClientIO.PostQueueData(v);
      FVMClientIO.ProcessAllSendCmd(nil, False, False);
    end
  else
      DisposeQueueData(v);
end;

procedure TCommunicationFrameworkWithP2PVM_Client.AsyncConnect(addr: SystemString; Port: Word);
var
  r: Boolean;
  IPV6: TIPV6;
  p: Pp2pVMListen;
begin
  Disconnect;
  if FLinkVM = nil then
      RaiseInfo('no vm reference');
  FVMClientIO := TP2PVM_PeerIO.Create(Self, nil);
  FVMClientIO.FLinkVM := FLinkVM;

  FVMConnected := False;

  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := nil;
  FOnAsyncConnectNotifyProc := nil;
  if (FLinkVM = nil) or (FLinkVM.FPhysicsIO = nil) then
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

  IPV6 := StrToIPv6(addr, r);

  if not r then
    begin
      if not FQuietMode then
          DoStatus('ipv6 format error! %s', [addr]);
      TriggerDoConnectFailed;
      exit;
    end;

  p := FLinkVM.FindListen(IPV6, Port);
  if p = nil then
    begin
      if not FQuietMode then
          DoStatus('no remote listen %s port:%d', [IPv6ToStr(IPV6).Text, Port]);
      TriggerDoConnectFailed;
      exit;
    end;

  FVMClientIO.FIP := IPV6;
  FVMClientIO.FPort := Port;

  FLinkVM.SendConnecting(p^.FrameworkID, FFrameworkWithVM_ID, FVMClientIO.ID, IPV6, Port);
end;

procedure TCommunicationFrameworkWithP2PVM_Client.AsyncConnectC(addr: SystemString; Port: Word; OnResult: TStateCall);
var
  r: Boolean;
  IPV6: TIPV6;
  p: Pp2pVMListen;
begin
  Disconnect;
  if FLinkVM = nil then
      RaiseInfo('no vm reference');
  FVMClientIO := TP2PVM_PeerIO.Create(Self, nil);
  FVMClientIO.FLinkVM := FLinkVM;

  FVMConnected := False;

  FOnAsyncConnectNotifyCall := OnResult;
  FOnAsyncConnectNotifyMethod := nil;
  FOnAsyncConnectNotifyProc := nil;
  if (FLinkVM = nil) or (FLinkVM.FPhysicsIO = nil) then
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

  IPV6 := StrToIPv6(addr, r);

  if not r then
    begin
      if not FQuietMode then
          DoStatus('ipv6 format error! %s', [addr]);
      TriggerDoConnectFailed;
      exit;
    end;

  p := FLinkVM.FindListen(IPV6, Port);
  if p = nil then
    begin
      if not FQuietMode then
          DoStatus('no remote listen %s port:%d', [IPv6ToStr(IPV6).Text, Port]);
      TriggerDoConnectFailed;
      exit;
    end;

  FVMClientIO.FIP := IPV6;
  FVMClientIO.FPort := Port;

  FLinkVM.SendConnecting(p^.FrameworkID, FFrameworkWithVM_ID, FVMClientIO.ID, IPV6, Port);
end;

procedure TCommunicationFrameworkWithP2PVM_Client.AsyncConnectM(addr: SystemString; Port: Word; OnResult: TStateMethod);
var
  r: Boolean;
  IPV6: TIPV6;
  p: Pp2pVMListen;
begin
  Disconnect;
  if FLinkVM = nil then
      RaiseInfo('no vm reference');
  FVMClientIO := TP2PVM_PeerIO.Create(Self, nil);
  FVMClientIO.FLinkVM := FLinkVM;

  FVMConnected := False;

  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := OnResult;
  FOnAsyncConnectNotifyProc := nil;
  if (FLinkVM = nil) or (FLinkVM.FPhysicsIO = nil) then
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

  IPV6 := StrToIPv6(addr, r);

  if not r then
    begin
      if not FQuietMode then
          DoStatus('ipv6 format error! %s', [addr]);
      TriggerDoConnectFailed;
      exit;
    end;

  p := FLinkVM.FindListen(IPV6, Port);
  if p = nil then
    begin
      if not FQuietMode then
          DoStatus('no remote listen %s port:%d', [IPv6ToStr(IPV6).Text, Port]);
      TriggerDoConnectFailed;
      exit;
    end;

  FVMClientIO.FIP := IPV6;
  FVMClientIO.FPort := Port;

  FLinkVM.SendConnecting(p^.FrameworkID, FFrameworkWithVM_ID, FVMClientIO.ID, IPV6, Port);
end;

procedure TCommunicationFrameworkWithP2PVM_Client.AsyncConnectP(addr: SystemString; Port: Word; OnResult: TStateProc);
var
  r: Boolean;
  IPV6: TIPV6;
  p: Pp2pVMListen;
begin
  Disconnect;
  if FLinkVM = nil then
      RaiseInfo('no vm reference');
  FVMClientIO := TP2PVM_PeerIO.Create(Self, nil);
  FVMClientIO.FLinkVM := FLinkVM;

  FVMConnected := False;

  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := nil;
  FOnAsyncConnectNotifyProc := OnResult;

  if (FLinkVM = nil) or (FLinkVM.FPhysicsIO = nil) then
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

  IPV6 := StrToIPv6(addr, r);

  if not r then
    begin
      if not FQuietMode then
          DoStatus('ipv6 format error! %s', [addr]);
      TriggerDoConnectFailed;
      exit;
    end;

  p := FLinkVM.FindListen(IPV6, Port);
  if p = nil then
    begin
      if not FQuietMode then
          DoStatus('no remote listen %s port:%d', [IPv6ToStr(IPV6).Text, Port]);
      TriggerDoConnectFailed;
      exit;
    end;

  FVMClientIO.FIP := IPV6;
  FVMClientIO.FPort := Port;

  FLinkVM.SendConnecting(p^.FrameworkID, FFrameworkWithVM_ID, FVMClientIO.ID, IPV6, Port);
end;

function TCommunicationFrameworkWithP2PVM_Client.Connect(addr: SystemString; Port: Word): Boolean;
var
  IPV6: TIPV6;
  p: Pp2pVMListen;
  t: TTimeTick;
begin
  Disconnect;
  if FLinkVM = nil then
      RaiseInfo('no vm reference');
  FVMClientIO := TP2PVM_PeerIO.Create(Self, nil);
  FVMClientIO.FLinkVM := FLinkVM;

  Result := False;

  FVMConnected := False;
  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := nil;
  FOnAsyncConnectNotifyProc := nil;
  if (FLinkVM = nil) or (FLinkVM.FPhysicsIO = nil) then
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

  IPV6 := StrToIPv6(addr, Result);

  if not Result then
    begin
      if not FQuietMode then
          DoStatus('ipv6 format error! %s', [addr]);
      exit;
    end;

  p := FLinkVM.FindListen(IPV6, Port);
  if p = nil then
    begin
      if not FQuietMode then
          DoStatus('no remote listen %s port:%d', [IPv6ToStr(IPV6).Text, Port]);
      exit;
    end;

  FVMClientIO.FIP := IPV6;
  FVMClientIO.FPort := Port;
  FLinkVM.SendConnecting(p^.FrameworkID, FFrameworkWithVM_ID, FVMClientIO.ID, IPV6, Port);

  t := GetTimeTick + 1000;
  while not FVMConnected do
    begin
      ProgressWaitSend(FVMClientIO);
      if GetTimeTick > t then
          Break;
    end;

  t := GetTimeTick + 2000;
  while (FVMConnected) and (not RemoteInited) do
    begin
      ProgressWaitSend(FVMClientIO);
      if GetTimeTick > t then
          Break;
    end;

  Result := (FVMConnected) and (RemoteInited);
end;

procedure TCommunicationFrameworkWithP2PVM_Client.Disconnect;
begin
  if FVMClientIO <> nil then
    begin
      FVMClientIO.Disconnect;
    end;
end;

procedure TCommunicationFrameworkWithP2PVM_Client.ProgressWaitSend(P_IO: TPeerIO);
begin
  if FLinkVM <> nil then
    begin
      if FLinkVM.FPhysicsIO <> nil then
          FLinkVM.FPhysicsIO.OwnerFramework.Progress;

      FLinkVM.Progress;
    end;

  inherited ProgressWaitSend(P_IO);
end;

procedure TCommunicationFrameworkWithP2PVM.Hook_SendByteBuffer(const Sender: TPeerIO; const buff: PByte; siz: NativeInt);
var
  t: Tp2pVMFragmentPacket;
begin
  if siz <= 0 then
      exit;

  if FAuthed then
    begin
      t.Init;
      t.buffSiz := siz;
      t.FrameworkID := 0;
      t.p2pID := 0;
      t.pkType := C_p2pVM_PhysicsFragmentData;
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

  if FPhysicsIO.OwnerFramework.FEnabledAtomicLockAndMultiThread then
      FCritical.Acquire;

  FReceiveStream.Position := FReceiveStream.Size;
  FReceiveStream.WritePtr(buff, siz);
end;

procedure TCommunicationFrameworkWithP2PVM.SyncProcessReceiveBuff;
var
  i: Integer;
  LP: Pp2pVMListen;
  p64: Int64;
  sourStream: TMemoryStream64;
  fPk: Tp2pVMFragmentPacket;
  rPos: Integer;
begin
  if FReceiveStream.Size <= 0 then
      exit;
  { p2p auth }
  if not FAuthed then
    begin
      if (FAuthWaiting) and (FReceiveStream.Size >= Length(FPhysicsIO.FP2PAuthToken)) and
        (CompareMemory(@FPhysicsIO.FP2PAuthToken[0], FReceiveStream.Memory, Length(FPhysicsIO.FP2PAuthToken))) then
        begin
          FSendStream.Clear;

          if not FAuthSending then
              AuthVM;

          FAuthWaiting := False;
          FAuthed := True;
          FAuthSending := False;

          { sync listen state }
          for i := 0 to FFrameworkListenPool.Count - 1 do
            begin
              LP := FFrameworkListenPool[i];
              SendListenState(LP^.FrameworkID, LP^.ListenHost, LP^.ListenPort, LP^.Listening);
            end;

          { send auth successed token }
          AuthSuccessed;

          { fill fragment buffer }
          p64 := Length(FPhysicsIO.FP2PAuthToken);
          sourStream := TMemoryStream64.Create;
          FReceiveStream.Position := p64;
          if FReceiveStream.Size - FReceiveStream.Position > 0 then
              sourStream.CopyFrom(FReceiveStream, FReceiveStream.Size - FReceiveStream.Position);
          DisposeObject(FReceiveStream);
          FReceiveStream := sourStream;

          if not FQuietMode then
              DoStatus('VM Authentication Success');
        end
      else if FAuthWaiting then
          exit
      else
        begin
          { safe process fragment }
          if FReceiveStream.Size >= Length(FPhysicsIO.FP2PAuthToken) then
            begin
              FPhysicsIO.FOwnerFramework.Framework_InternalSaveReceiveBuffer(FPhysicsIO, FReceiveStream.Memory, FReceiveStream.Size);
              FReceiveStream.Clear;
              FPhysicsIO.FOwnerFramework.Framework_InternalProcessReceiveBuffer(FPhysicsIO, nil, False, False);
            end;
          exit;
        end;
    end;

  if FReceiveStream.Size < 13 then
      exit;

  sourStream := TMemoryStream64.Create;
  p64 := 0;
  sourStream.SetPointerWithProtectedMode(FReceiveStream.PositionAsPtr(p64), FReceiveStream.Size - p64);

  while sourStream.Size > 0 do
    begin
      fPk.Init;
      rPos := fPk.FillReceiveBuff(sourStream);
      if rPos > 0 then
        begin
          { protocol support }
          if fPk.pkType = C_p2pVM_echoing then
              ReceivedEchoing(fPk.FrameworkID, fPk.p2pID, fPk.buff, fPk.buffSiz)
          else if fPk.pkType = C_p2pVM_echo then
              ReceivedEcho(fPk.FrameworkID, fPk.p2pID, fPk.buff, fPk.buffSiz)
          else if fPk.pkType = C_p2pVM_AuthSuccessed then
            begin
              if Assigned(OnAuthSuccessOnesNotify) then
                begin
                  try
                      OnAuthSuccessOnesNotify(Self);
                  except
                  end;
                  OnAuthSuccessOnesNotify := nil;
                end;
            end
          else if fPk.pkType = C_p2pVM_Listen then
              ReceivedListen(fPk.FrameworkID, fPk.p2pID, fPk.buff, fPk.buffSiz)
          else if fPk.pkType = C_p2pVM_ListenState then
              ReceivedListenState(fPk.FrameworkID, fPk.p2pID, fPk.buff, fPk.buffSiz)
          else if fPk.pkType = C_p2pVM_Connecting then
              ReceivedConnecting(fPk.FrameworkID, fPk.p2pID, fPk.buff, fPk.buffSiz)
          else if fPk.pkType = C_p2pVM_ConnectedReponse then
              ReceivedConnectedReponse(fPk.FrameworkID, fPk.p2pID, fPk.buff, fPk.buffSiz)
          else if fPk.pkType = C_p2pVM_Disconnect then
              ReceivedDisconnect(fPk.FrameworkID, fPk.p2pID, fPk.buff, fPk.buffSiz)
          else if fPk.pkType = C_p2pVM_LogicFragmentData then
              ReceivedLogicFragmentData(fPk.FrameworkID, fPk.p2pID, fPk.buff, fPk.buffSiz)
          else if fPk.pkType = C_p2pVM_PhysicsFragmentData then
              ReceivedPhysicsFragmentData(fPk.FrameworkID, fPk.p2pID, fPk.buff, fPk.buffSiz)
          else if not FQuietMode then
            begin
              DoStatus('VM protocol header errror');
              DoStatus(@fPk, SizeOf(fPk), 40);
            end;

          { fill buffer }
          inc(p64, rPos);
          if FReceiveStream.Size - p64 >= 13 then
            begin
              sourStream.SetPointerWithProtectedMode(FReceiveStream.PositionAsPtr(p64), FReceiveStream.Size - p64);
            end
          else
              Break;
        end
      else
          Break;
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
end;

procedure TCommunicationFrameworkWithP2PVM.Hook_ProcessReceiveBuffer(const Sender: TPeerIO; const CurrentActiveThread_: TCoreClassThread; const RecvSync, SendSync: Boolean);
begin
  Sender.IO_SyncMethod(CurrentActiveThread_, RecvSync, {$IFDEF FPC}@{$ENDIF FPC}SyncProcessReceiveBuff);
  if FPhysicsIO.OwnerFramework.FEnabledAtomicLockAndMultiThread then
      FCritical.Release;
end;

procedure TCommunicationFrameworkWithP2PVM.Hook_ClientDestroy(const Sender: TPeerIO);
begin
  CloseP2PVMTunnel;
  Sender.FOwnerFramework.Framework_InternalIODestroy(Sender);
end;

procedure TCommunicationFrameworkWithP2PVM.SendVMBuffer(const buff: Pointer; const siz: NativeInt);
begin
  FPhysicsIO.WriteBufferOpen;
  FPhysicsIO.OwnerFramework.Framework_InternalSendByteBuffer(FPhysicsIO, buff, siz);
  FPhysicsIO.WriteBufferFlush;
  FPhysicsIO.WriteBufferClose;
end;

procedure TCommunicationFrameworkWithP2PVM.ReceivedEchoing(const FrameworkID, p2pID: Cardinal; const buff: PByte; const siz: Cardinal);
begin
  echoBuffer(buff, siz);
end;

procedure TCommunicationFrameworkWithP2PVM.ReceivedEcho(const FrameworkID, p2pID: Cardinal; const buff: PByte; const siz: Cardinal);
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
            if Assigned(echoPtr^.OnEchoProc) then
                echoPtr^.OnEchoProc(True);
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

procedure TCommunicationFrameworkWithP2PVM.ReceivedListen(const FrameworkID, p2pID: Cardinal; const buff: PByte; const siz: Cardinal);
type
  TBuf = array [0 .. 18] of Byte;
  PBuf = ^TBuf;
var
  p: PBuf;
  IPV6: TIPV6;
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
  IPV6 := PIPV6(@p^[0])^;
  Port := PWORD(@p^[16])^;
  Listening := PBoolean(@p^[18])^;

  if p2pID <> 0 then
    begin
      if not FQuietMode then
          DoStatus('listen protocol error! IO ID:%d', [p2pID]);
      exit;
    end;

  LP := FindListen(IPV6, Port);
  if Listening then
    begin
      if LP = nil then
        begin
          new(LP);
          LP^.FrameworkID := FrameworkID;
          LP^.ListenHost := IPV6;
          LP^.ListenPort := Port;
          LP^.Listening := True;
          FFrameworkListenPool.Add(LP);
          SendListenState(FrameworkID, IPV6, Port, True);
        end
      else
        begin
          LP^.Listening := True;
          SendListenState(FrameworkID, IPV6, Port, True);
        end;
    end
  else
    begin
      DeleteListen(IPV6, Port);
      SendListenState(FrameworkID, IPV6, Port, False);
    end;
end;

procedure TCommunicationFrameworkWithP2PVM.ReceivedListenState(const FrameworkID, p2pID: Cardinal; const buff: PByte; const siz: Cardinal);
type
  TBuf = array [0 .. 18] of Byte;
  PBuf = ^TBuf;
var
  c: TCommunicationFramework;
  p: PBuf;
  IPV6: TIPV6;
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
  IPV6 := PIPV6(@p^[0])^;
  Port := PWORD(@p^[16])^;
  Listening := PBoolean(@p^[18])^;

  if p2pID <> 0 then
    begin
      if not FQuietMode then
          DoStatus('Virtual listen state protocol error! IO ID:%d', [p2pID]);
      exit;
    end;

  LP := FindListen(IPV6, Port);
  if Listening then
    begin
      if LP = nil then
        begin
          new(LP);
          LP^.FrameworkID := FrameworkID;
          LP^.ListenHost := IPV6;
          LP^.ListenPort := Port;
          LP^.Listening := True;
          FFrameworkListenPool.Add(LP);
        end
      else
        begin
          LP^.Listening := True;
        end;
      if not FQuietMode then
          DoStatus('Virtual Remote Listen state Activted "%s port:%d"', [IPv6ToStr(IPV6).Text, Port]);
    end
  else
    begin
      DeleteListen(IPV6, Port);
      if not FQuietMode then
          DoStatus('Virtual Remote Listen state Close "%s port:%d"', [IPv6ToStr(IPV6).Text, Port]);
    end;

  c := TCommunicationFramework(FFrameworkPool[FrameworkID]);
  if c is TCommunicationFrameworkWithP2PVM_Server then
    begin
      TCommunicationFrameworkWithP2PVM_Server(c).ListenState(Self, IPV6, Port, Listening);
      SendListenState(TCommunicationFrameworkWithP2PVM_Server(c).FFrameworkWithVM_ID, IPV6, Port, Listening);
    end;
end;

procedure TCommunicationFrameworkWithP2PVM.ReceivedConnecting(const FrameworkID, p2pID: Cardinal; const buff: PByte; const siz: Cardinal);
type
  TBuf = array [0 .. 25] of Byte;
  PBuf = ^TBuf;
var
  c: TCommunicationFramework;
  p: PBuf;
  Remote_frameworkID: Cardinal;
  Remote_p2pID: Cardinal;
  IPV6: TIPV6;
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
  IPV6 := PIPV6(@p^[8])^;
  Port := PWORD(@p^[24])^;

  if p2pID <> 0 then
    begin
      SendDisconnect(Remote_frameworkID, Remote_p2pID);
      if not FQuietMode then
          DoStatus('connect request with protocol error! IO ID:%d', [p2pID]);
      exit;
    end;

  c := TCommunicationFramework(FFrameworkPool[FrameworkID]);
  if c is TCommunicationFrameworkWithP2PVM_Server then
    begin
      Allowed := True;
      TCommunicationFrameworkWithP2PVM_Server(c).Connecting(Self, Remote_frameworkID, FrameworkID, IPV6, Port, Allowed);

      if not Allowed then
        begin
          SendDisconnect(Remote_frameworkID, 0);
          exit;
        end;
    end
  else
    begin
      SendDisconnect(Remote_frameworkID, Remote_p2pID);
    end;
end;

procedure TCommunicationFrameworkWithP2PVM.ReceivedConnectedReponse(const FrameworkID, p2pID: Cardinal; const buff: PByte; const siz: Cardinal);
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

  c := TCommunicationFramework(FFrameworkPool[FrameworkID]);
  if c is TCommunicationFrameworkWithP2PVM_Client then
    begin
      p := @buff^;
      Remote_frameworkID := PCardinal(@p^[0])^;
      Remote_p2pID := PCardinal(@p^[4])^;

      { trigger connect reponse }
      TCommunicationFrameworkWithP2PVM_Client(c).VMConnectSuccessed(Self, Remote_frameworkID, Remote_p2pID, FrameworkID);

      if not FQuietMode then
          DoStatus('connecting reponse from frameworkID: %d p2pID: %d', [Remote_frameworkID, Remote_p2pID]);
    end;
end;

procedure TCommunicationFrameworkWithP2PVM.ReceivedDisconnect(const FrameworkID, p2pID: Cardinal; const buff: PByte; const siz: Cardinal);
var
  c: TCommunicationFramework;
  LocalVMc: TP2PVM_PeerIO;
begin
  c := TCommunicationFramework(FFrameworkPool[FrameworkID]);
  if c is TCommunicationFrameworkWithP2PVM_Client then
    begin
      if TCommunicationFrameworkWithP2PVM_Client(c).FVMClientIO <> nil then
          TCommunicationFrameworkWithP2PVM_Client(c).FVMClientIO.FDestroySyncRemote := False;
      TCommunicationFrameworkWithP2PVM_Client(c).VMDisconnect(Self);
    end
  else if c is TCommunicationFrameworkWithP2PVM_Server then
    begin
      LocalVMc := TP2PVM_PeerIO(c.FPeerIO_HashPool[p2pID]);
      if LocalVMc = nil then
        begin
          if not FQuietMode then
              DoStatus('disconnect protocol no p2pID:%d', [p2pID]);
          exit;
        end;
      LocalVMc.FDestroySyncRemote := False;
      LocalVMc.Disconnect;
    end
  else if not FQuietMode then
      DoStatus('disconnect protocol no frameworkID: %d', [FrameworkID]);
end;

procedure TCommunicationFrameworkWithP2PVM.ReceivedLogicFragmentData(const FrameworkID, p2pID: Cardinal; const buff: PByte; const siz: Cardinal);
var
  c: TCommunicationFramework;
  LocalVMc: TPeerIO;
begin
  AtomInc(FPhysicsIO.OwnerFramework.Statistics[TStatisticsType.stReceiveSize], siz);
  c := TCommunicationFramework(FFrameworkPool[FrameworkID]);
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
      LocalVMc := TCommunicationFrameworkWithP2PVM_Client(c).FVMClientIO;
      if LocalVMc <> nil then
        begin
          LocalVMc.SaveReceiveBuffer(buff, siz);
          LocalVMc.FillRecvBuffer(nil, False, False);
        end
      else if not FQuietMode then
        begin
          DoStatus('LocalVM [%d] error: no interface', [FrameworkID]);
        end;
    end
  else if not FQuietMode then
    begin
      DoStatus('fragment Data frameworkID error: frameworkID:%d buffer size:%d', [FrameworkID, siz]);
      DoStatus(buff, umlMin(siz, 164), 40);
    end;
end;

procedure TCommunicationFrameworkWithP2PVM.ReceivedPhysicsFragmentData(const FrameworkID, p2pID: Cardinal; const buff: PByte; const siz: Cardinal);
begin
  if FPhysicsIO = nil then
      exit;
  FPhysicsIO.FOwnerFramework.Framework_InternalSaveReceiveBuffer(FPhysicsIO, buff, siz);
  FPhysicsIO.FOwnerFramework.Framework_InternalProcessReceiveBuffer(FPhysicsIO, nil, False, False);
end;

procedure TCommunicationFrameworkWithP2PVM.DoProcessPerClientFragmentSend(P_IO: TPeerIO);
var
  p: Pp2pVMFragmentPacket;
begin
  if TP2PVM_PeerIO(P_IO).FLinkVM <> Self then
      exit;

  if TP2PVM_PeerIO(P_IO).FSendQueue.Count > 0 then
    begin
      p := TP2PVM_PeerIO(P_IO).FSendQueue[0];
      TP2PVM_PeerIO(P_IO).FSendQueue.Delete(0);
      p^.BuildSendBuff(FSendStream);
      FreeP2PVMPacket(p);
    end;
end;

procedure TCommunicationFrameworkWithP2PVM.DoPerClientClose(P_IO: TPeerIO);
begin
  if TP2PVM_PeerIO(P_IO).FLinkVM = Self then
      P_IO.Disconnect;
end;

constructor TCommunicationFrameworkWithP2PVM.Create(HashPoolSize: Integer);
begin
  inherited Create;
  FCritical := TCritical.Create;
  FPhysicsIO := nil;

  FAuthWaiting := False;
  FAuthed := False;
  FAuthSending := False;

  FFrameworkPool := TUInt32HashObjectList.CustomCreate(HashPoolSize);
  FFrameworkPool.AutoFreeData := False;
  FFrameworkPool.AccessOptimization := False;

  FFrameworkListenPool := TCoreClassList.Create;

  FMaxVMFragmentSize := C_P2PVM_MaxVMFragmentSize;
  FMaxRealBuffer := C_P2PVM_MaxRealBuffer;

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

  if FPhysicsIO <> nil then
      CloseP2PVMTunnel;

  ClearListen;

  DisposeObject(FWaitEchoList);
  DisposeObject(FReceiveStream);
  DisposeObject(FSendStream);
  DisposeObject(FFrameworkPool);
  DisposeObject(FFrameworkListenPool);
  DisposeObject(FCritical);
  inherited Destroy;
end;

procedure TCommunicationFrameworkWithP2PVM.Progress;
var
  i: Integer;
  p: PUInt32HashListObjectStruct;
  lsiz: Int64;
  OnEchoPtr: POnEcho;
begin
  if FPhysicsIO = nil then
      exit;

  { echo and keepalive simulate }
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
            if Assigned(OnEchoPtr^.OnEchoProc) then
                OnEchoPtr^.OnEchoProc(False);
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

  if not FPhysicsIO.WriteBufferEmpty then
      exit;

  { real send buffer }
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

  { fragment Packet }
  repeat
    lsiz := FSendStream.Size;
    if (FFrameworkPool.Count > 0) then
      begin
        i := 0;
        p := FFrameworkPool.FirstPtr;
        while i < FFrameworkPool.Count do
          begin
            TCommunicationFramework(p^.data).FastProgressPeerIOM({$IFDEF FPC}@{$ENDIF FPC}DoProcessPerClientFragmentSend);
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
              OnBackcall(TCommunicationFramework(p^.data));
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
              OnBackcall(TCommunicationFramework(p^.data));
          except
          end;
          inc(i);
          p := p^.Next;
        end;
    end;
end;

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
              OnBackcall(TCommunicationFramework(p^.data));
          except
          end;
          inc(i);
          p := p^.Next;
        end;
    end;
end;

procedure TCommunicationFrameworkWithP2PVM.OpenP2PVMTunnel(c: TPeerIO);
begin
  FPhysicsIO := c;
  FAuthWaiting := False;
  FAuthed := False;
  FAuthSending := False;
  FReceiveStream.Clear;
  FSendStream.Clear;

  { install tunnel driver }
  try
    FPhysicsIO.OnInternalSendByteBuffer := {$IFDEF FPC}@{$ENDIF FPC}Hook_SendByteBuffer;
    FPhysicsIO.OnInternalSaveReceiveBuffer := {$IFDEF FPC}@{$ENDIF FPC}Hook_SaveReceiveBuffer;
    FPhysicsIO.OnInternalProcessReceiveBuffer := {$IFDEF FPC}@{$ENDIF FPC}Hook_ProcessReceiveBuffer;
    FPhysicsIO.OnDestroy := {$IFDEF FPC}@{$ENDIF FPC}Hook_ClientDestroy;
  except
  end;

  if not FQuietMode then
      DoStatus('Open VM P2P Tunnel ' + FPhysicsIO.PeerIP);
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
          if p^.data is TCommunicationFrameworkWithP2PVM_Server then
            begin
              TCommunicationFramework(p^.data).ProgressPeerIOM({$IFDEF FPC}@{$ENDIF FPC}DoPerClientClose);
              TCommunicationFrameworkWithP2PVM_Server(p^.data).FLinkVMPool.Delete(FVMID);
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

  if FPhysicsIO = nil then
      exit;

  try
    FPhysicsIO.OnInternalSendByteBuffer := {$IFDEF FPC}@{$ENDIF FPC}FPhysicsIO.FOwnerFramework.Framework_InternalSendByteBuffer;
    FPhysicsIO.OnInternalSaveReceiveBuffer := {$IFDEF FPC}@{$ENDIF FPC}FPhysicsIO.FOwnerFramework.Framework_InternalSaveReceiveBuffer;
    FPhysicsIO.OnInternalProcessReceiveBuffer := {$IFDEF FPC}@{$ENDIF FPC}FPhysicsIO.FOwnerFramework.Framework_InternalProcessReceiveBuffer;
    FPhysicsIO.OnDestroy := {$IFDEF FPC}@{$ENDIF FPC}FPhysicsIO.FOwnerFramework.Framework_InternalIODestroy;
  except
  end;

  if not FQuietMode then
      DoStatus('Close VM P2P Tunnel ' + FPhysicsIO.PeerIP);

  FPhysicsIO := nil;
end;

procedure TCommunicationFrameworkWithP2PVM.InstallLogicFramework(c: TCommunicationFramework);
var
  i: Integer;
  LP: Pp2pVMListen;
begin
  if (c is TCommunicationFramework_CustomStableServer) then
    begin
      InstallLogicFramework(TCommunicationFramework_CustomStableServer(c).PhysicsServer);
      exit;
    end;
  if (c is TCommunicationFramework_CustomStableClient) then
    begin
      InstallLogicFramework(TCommunicationFramework_CustomStableClient(c).PhysicsClient);
      exit;
    end;

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
          SendListen(TCommunicationFrameworkWithP2PVM_Server(c).FFrameworkWithVM_ID, LP^.ListenHost, LP^.ListenPort, LP^.Listening);
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

      FFrameworkPool.Add(TCommunicationFrameworkWithP2PVM_Client(c).FFrameworkWithVM_ID, c, True);
    end
  else
      RaiseInfo('illegal p2pVM.');
end;

procedure TCommunicationFrameworkWithP2PVM.UninstallLogicFramework(c: TCommunicationFramework);
var
  i: Integer;
  LP: Pp2pVMListen;
begin
  if (c is TCommunicationFramework_CustomStableServer) then
    begin
      UninstallLogicFramework(TCommunicationFramework_CustomStableServer(c).PhysicsServer);
      exit;
    end;
  if (c is TCommunicationFramework_CustomStableClient) then
    begin
      UninstallLogicFramework(TCommunicationFramework_CustomStableClient(c).PhysicsClient);
      exit;
    end;

  if c is TCommunicationFrameworkWithP2PVM_Server then
    begin
      TCommunicationFrameworkWithP2PVM_Server(c).FLinkVMPool.Delete(FVMID);
      FFrameworkPool.Delete(TCommunicationFrameworkWithP2PVM_Server(c).FFrameworkWithVM_ID);

      i := 0;
      while i < FFrameworkListenPool.Count do
        begin
          LP := FFrameworkListenPool[i];
          if LP^.FrameworkID = TCommunicationFrameworkWithP2PVM_Server(c).FFrameworkWithVM_ID then
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
      TCommunicationFrameworkWithP2PVM_Client(c).Disconnect;
      TCommunicationFrameworkWithP2PVM_Client(c).FLinkVM := nil;
      FFrameworkPool.Delete(TCommunicationFrameworkWithP2PVM_Client(c).FFrameworkWithVM_ID);
    end
  else
      RaiseInfo('illegal p2pVM.');
end;

function TCommunicationFrameworkWithP2PVM.CreateLogicClient: TCommunicationFrameworkWithP2PVM_Client;
var
  FrameworkID: Cardinal;
begin
  if FFrameworkPool.Count > 0 then
      FrameworkID := FFrameworkPool.LastPtr^.u32 + 1
  else
      FrameworkID := 1;
  while FFrameworkPool.Exists(FrameworkID) do
      inc(FrameworkID);
  Result := TCommunicationFrameworkWithP2PVM_Client.CustomCreate(FrameworkID);
  InstallLogicFramework(Result);
end;

procedure TCommunicationFrameworkWithP2PVM.AuthWaiting;
begin
  if FPhysicsIO = nil then
      exit;
  FAuthWaiting := True;
end;

procedure TCommunicationFrameworkWithP2PVM.AuthVM;
begin
  if FPhysicsIO = nil then
      exit;
  if not FAuthed then
    if not FAuthSending then
      begin
        FSendStream.WritePtr(@FPhysicsIO.FP2PAuthToken[0], Length(FPhysicsIO.FP2PAuthToken));
        FAuthSending := True;
        FAuthWaiting := True;
      end;
end;

procedure TCommunicationFrameworkWithP2PVM.AuthSuccessed;
var
  p: Pp2pVMFragmentPacket;
begin
  p := BuildP2PVMPacket(0, 0, 0, C_p2pVM_AuthSuccessed, nil);

  FSendStream.Position := FSendStream.Size;
  p^.BuildSendBuff(FSendStream);
  FreeP2PVMPacket(p);
end;

procedure TCommunicationFrameworkWithP2PVM.echoing(const OnEchoPtr: POnEcho; Timeout: TTimeTick);
var
  u64ptr: UInt64;
  p: Pp2pVMFragmentPacket;
  i: Integer;
begin
  if (FPhysicsIO = nil) or (not WasAuthed) then
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
            if Assigned(OnEchoPtr^.OnEchoProc) then
                OnEchoPtr^.OnEchoProc(False);
          except
          end;

          Dispose(OnEchoPtr);
        end;
      exit;
    end;

  u64ptr := UInt64(OnEchoPtr);
  p := BuildP2PVMPacket(8, 0, 0, C_p2pVM_echoing, @u64ptr);

  FSendStream.Position := FSendStream.Size;
  p^.BuildSendBuff(FSendStream);
  FreeP2PVMPacket(p);

  FWaitEchoList.Add(OnEchoPtr);
end;

procedure TCommunicationFrameworkWithP2PVM.echoingC(OnResult: TStateCall; Timeout: TTimeTick);
var
  p: POnEcho;
begin
  new(p);
  p^.OnEchoCall := OnResult;
  p^.OnEchoMethod := nil;
  p^.OnEchoProc := nil;
  p^.Timeout := GetTimeTick + Timeout;
  echoing(p, Timeout);
end;

procedure TCommunicationFrameworkWithP2PVM.echoingM(OnResult: TStateMethod; Timeout: TTimeTick);
var
  p: POnEcho;
begin
  new(p);
  p^.OnEchoCall := nil;
  p^.OnEchoMethod := OnResult;
  p^.OnEchoProc := nil;
  p^.Timeout := GetTimeTick + Timeout;
  echoing(p, Timeout);
end;

procedure TCommunicationFrameworkWithP2PVM.echoingP(OnResult: TStateProc; Timeout: TTimeTick);
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

procedure TCommunicationFrameworkWithP2PVM.echoBuffer(const buff: Pointer; const siz: NativeInt);
var
  p: Pp2pVMFragmentPacket;
begin
  if (FPhysicsIO = nil) or (not WasAuthed) then
      exit;
  p := BuildP2PVMPacket(siz, 0, 0, C_p2pVM_echo, buff);

  FSendStream.Position := FSendStream.Size;
  p^.BuildSendBuff(FSendStream);
  FreeP2PVMPacket(p);
end;

procedure TCommunicationFrameworkWithP2PVM.SendListen(const FrameworkID: Cardinal; const IPV6: TIPV6; const Port: Word; const Listening: Boolean);
var
  LP: Pp2pVMListen;
  c: TCommunicationFramework;
  RBuf: array [0 .. 18] of Byte;
  p: Pp2pVMFragmentPacket;
begin
  if (FPhysicsIO = nil) or (not WasAuthed) then
    begin
      LP := FindListen(IPV6, Port);
      if Listening then
        begin
          if LP = nil then
            begin
              new(LP);
              LP^.FrameworkID := FrameworkID;
              LP^.ListenHost := IPV6;
              LP^.ListenPort := Port;
              LP^.Listening := True;
              FFrameworkListenPool.Add(LP);
            end
          else
              LP^.Listening := True;
        end
      else
          DeleteListen(IPV6, Port);

      c := TCommunicationFramework(FFrameworkPool[FrameworkID]);
      if c is TCommunicationFrameworkWithP2PVM_Server then
        begin
          TCommunicationFrameworkWithP2PVM_Server(c).ListenState(Self, IPV6, Port, Listening);
          SendListenState(TCommunicationFrameworkWithP2PVM_Server(c).FFrameworkWithVM_ID, IPV6, Port, Listening);
        end;
    end
  else
    begin
      PIPV6(@RBuf[0])^ := IPV6;
      PWORD(@RBuf[16])^ := Port;
      PBoolean(@RBuf[18])^ := Listening;
      p := BuildP2PVMPacket(SizeOf(RBuf), FrameworkID, 0, C_p2pVM_Listen, @RBuf[0]);

      FSendStream.Position := FSendStream.Size;
      p^.BuildSendBuff(FSendStream);
      FreeP2PVMPacket(p);
    end;
end;

procedure TCommunicationFrameworkWithP2PVM.SendListenState(const FrameworkID: Cardinal; const IPV6: TIPV6; const Port: Word; const Listening: Boolean);
var
  RBuf: array [0 .. 18] of Byte;
  p: Pp2pVMFragmentPacket;
begin
  if (FPhysicsIO = nil) or (not WasAuthed) then
      exit;
  PIPV6(@RBuf[0])^ := IPV6;
  PWORD(@RBuf[16])^ := Port;
  PBoolean(@RBuf[18])^ := Listening;
  p := BuildP2PVMPacket(SizeOf(RBuf), FrameworkID, 0, C_p2pVM_ListenState, @RBuf[0]);

  FSendStream.Position := FSendStream.Size;
  p^.BuildSendBuff(FSendStream);
  FreeP2PVMPacket(p);
end;

procedure TCommunicationFrameworkWithP2PVM.SendConnecting(const Remote_frameworkID, FrameworkID, p2pID: Cardinal; const IPV6: TIPV6; const Port: Word);
var
  RBuf: array [0 .. 25] of Byte;
  p: Pp2pVMFragmentPacket;
begin
  if (FPhysicsIO = nil) or (not WasAuthed) then
      exit;
  PCardinal(@RBuf[0])^ := FrameworkID;
  PCardinal(@RBuf[4])^ := p2pID;
  PIPV6(@RBuf[8])^ := IPV6;
  PWORD(@RBuf[24])^ := Port;

  p := BuildP2PVMPacket(SizeOf(RBuf), Remote_frameworkID, 0, C_p2pVM_Connecting, @RBuf[0]);

  FSendStream.Position := FSendStream.Size;
  p^.BuildSendBuff(FSendStream);
  FreeP2PVMPacket(p);
end;

procedure TCommunicationFrameworkWithP2PVM.SendConnectedReponse(const Remote_frameworkID, Remote_p2pID, FrameworkID, p2pID: Cardinal);
var
  RBuf: array [0 .. 7] of Byte;
  p: Pp2pVMFragmentPacket;
begin
  if (FPhysicsIO = nil) or (not WasAuthed) then
      exit;
  PCardinal(@RBuf[0])^ := FrameworkID;
  PCardinal(@RBuf[4])^ := p2pID;

  p := BuildP2PVMPacket(SizeOf(RBuf), Remote_frameworkID, Remote_p2pID, C_p2pVM_ConnectedReponse, @RBuf[0]);

  FSendStream.Position := FSendStream.Size;
  p^.BuildSendBuff(FSendStream);
  FreeP2PVMPacket(p);
end;

procedure TCommunicationFrameworkWithP2PVM.SendDisconnect(const Remote_frameworkID, Remote_p2pID: Cardinal);
var
  p: Pp2pVMFragmentPacket;
begin
  if (FPhysicsIO = nil) or (not WasAuthed) then
      exit;
  p := BuildP2PVMPacket(0, Remote_frameworkID, Remote_p2pID, C_p2pVM_Disconnect, nil);

  FSendStream.Position := FSendStream.Size;
  p^.BuildSendBuff(FSendStream);
  FreeP2PVMPacket(p);
end;

function TCommunicationFrameworkWithP2PVM.ListenCount: Integer;
begin
  Result := FFrameworkListenPool.Count;
end;

function TCommunicationFrameworkWithP2PVM.GetListen(const index: Integer): Pp2pVMListen;
begin
  Result := FFrameworkListenPool[index];
end;

function TCommunicationFrameworkWithP2PVM.FindListen(const IPV6: TIPV6; const Port: Word): Pp2pVMListen;
var
  i: Integer;
  p: Pp2pVMListen;
begin
  for i := 0 to FFrameworkListenPool.Count - 1 do
    begin
      p := FFrameworkListenPool[i];
      if (p^.ListenPort = Port) and (CompareIPV6(p^.ListenHost, IPV6)) then
        begin
          Result := p;
          exit;
        end;
    end;
  Result := nil;
end;

function TCommunicationFrameworkWithP2PVM.FindListening(const IPV6: TIPV6; const Port: Word): Pp2pVMListen;
var
  i: Integer;
  p: Pp2pVMListen;
begin
  for i := 0 to FFrameworkListenPool.Count - 1 do
    begin
      p := FFrameworkListenPool[i];
      if (p^.Listening) and (p^.ListenPort = Port) and (CompareIPV6(p^.ListenHost, IPV6)) then
        begin
          Result := p;
          exit;
        end;
    end;
  Result := nil;
end;

procedure TCommunicationFrameworkWithP2PVM.DeleteListen(const IPV6: TIPV6; const Port: Word);
var
  i: Integer;
  p: Pp2pVMListen;
begin
  i := 0;
  while i < FFrameworkListenPool.Count do
    begin
      p := FFrameworkListenPool[i];
      if (p^.ListenPort = Port) and (CompareIPV6(p^.ListenHost, IPV6)) then
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
          if p^.data is TCommunicationFrameworkWithP2PVM_Client then
              TCommunicationFramework(p^.data).ProgressPeerIOM({$IFDEF FPC}@{$ENDIF FPC}DoPerClientClose);
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
          if p^.data is TCommunicationFrameworkWithP2PVM_Server then
              TCommunicationFramework(p^.data).ProgressPeerIOM({$IFDEF FPC}@{$ENDIF FPC}DoPerClientClose);
          inc(i);
          p := p^.Next;
        end;
    end;
end;

constructor TStableServer_PhysicsIO_UserDefine.Create(Owner_: TPeerIO);
begin
  inherited Create(Owner_);
  BindStableIO := nil;
end;

destructor TStableServer_PhysicsIO_UserDefine.Destroy;
begin
  if BindStableIO <> nil then
    begin
      BindStableIO.BindPhysicsIO := nil;
      if not BindStableIO.Activted then
          BindStableIO.DelayClose(2.0);
      BindStableIO := nil;
    end;
  inherited Destroy;
end;

procedure TStableServer_PeerIO.CreateAfter;
begin
  inherited CreateAfter;
  Activted := False;
  DestroyRecyclePhysicsIO := True;
  Connection_Token := 0;
  InternalBindPhysicsIO := nil;
  OfflineTick := GetTimeTick;
end;

destructor TStableServer_PeerIO.Destroy;
begin
  if (DestroyRecyclePhysicsIO) and (BindPhysicsIO <> nil) then
    begin
      TStableServer_PhysicsIO_UserDefine(BindPhysicsIO.UserDefine).BindStableIO := nil;
      BindPhysicsIO.DelayClose;
      BindPhysicsIO := nil;
    end;

  inherited Destroy;
end;

function TStableServer_PeerIO.Connected: Boolean;
begin
  Result := True;
end;

procedure TStableServer_PeerIO.Disconnect;
begin
  DelayFree();
end;

procedure TStableServer_PeerIO.SendByteBuffer(const buff: PByte; const Size: NativeInt);
begin
  if BindPhysicsIO = nil then
      AtomDec(FOwnerFramework.Statistics[TStatisticsType.stSendSize], Size)
  else
      BindPhysicsIO.SendByteBuffer(buff, Size);
end;

procedure TStableServer_PeerIO.WriteBufferOpen;
begin
  if BindPhysicsIO <> nil then
      BindPhysicsIO.WriteBufferOpen;
end;

procedure TStableServer_PeerIO.WriteBufferFlush;
begin
  if BindPhysicsIO <> nil then
      BindPhysicsIO.WriteBufferFlush;
end;

procedure TStableServer_PeerIO.WriteBufferClose;
begin
  if BindPhysicsIO <> nil then
      BindPhysicsIO.WriteBufferClose;
end;

function TStableServer_PeerIO.GetPeerIP: SystemString;
begin
  if BindPhysicsIO <> nil then
      Result := BindPhysicsIO.GetPeerIP
  else
      Result := 'StableIO - offline';
end;

function TStableServer_PeerIO.WriteBufferEmpty: Boolean;
begin
  if BindPhysicsIO <> nil then
      Result := BindPhysicsIO.WriteBufferEmpty
  else
      Result := False;
end;

procedure TStableServer_PeerIO.Progress;
var
  t, offline_t: TTimeTick;
begin
  if (Activted) then
    begin
      t := GetTimeTick;

      if (BindPhysicsIO = nil) then
        begin
          offline_t := TCommunicationFramework_CustomStableServer(FOwnerFramework).OfflineTimeout;
          if (offline_t > 0) and (t - OfflineTick > offline_t) then
            begin
              DelayClose;
              exit;
            end;
        end
      else
          OfflineTick := t;
    end;

  inherited Progress;
  ProcessAllSendCmd(nil, False, False);
end;

procedure TCommunicationFramework_CustomStableServer.ServerCustomProtocolReceiveBufferNotify(Sender: TPeerIO; const buffer: PByte; const Size: NativeInt; var FillDone: Boolean);
var
  io_def: TStableServer_PhysicsIO_UserDefine;
begin
  io_def := Sender.UserDefine as TStableServer_PhysicsIO_UserDefine;
  FillDone := (io_def.BindStableIO <> nil);
  if FillDone then
    begin
      io_def.BindStableIO.SaveReceiveBuffer(buffer, Size);
      io_def.BindStableIO.FillRecvBuffer(nil, False, False);
    end;
end;

procedure TCommunicationFramework_CustomStableServer.SetPhysicsServer(const Value: TCommunicationFrameworkServer);
begin
  if FPhysicsServer <> nil then
    begin
      FPhysicsServer.FOnServerCustomProtocolReceiveBufferNotify := nil;
      FPhysicsServer.Protocol := TCommunicationProtocol.cpZServer;
      FPhysicsServer.UserDefineClass := TPeerIOUserDefine;
      FPhysicsServer.QuietMode := False;

      UnRegisted(C_BuildStableIO);
      UnRegisted(C_OpenStableIO);
    end;

  FPhysicsServer := Value;

  if FPhysicsServer <> nil then
    begin
      FPhysicsServer.FOnServerCustomProtocolReceiveBufferNotify := {$IFDEF FPC}@{$ENDIF FPC}ServerCustomProtocolReceiveBufferNotify;
      FPhysicsServer.Protocol := TCommunicationProtocol.cpCustom;
      FPhysicsServer.UserDefineClass := TStableServer_PhysicsIO_UserDefine;
      FPhysicsServer.SyncOnResult := True;
      FPhysicsServer.SyncOnCompleteBuffer := True;
      FPhysicsServer.QuietMode := False;
      FPhysicsServer.TimeOutIDLE := 60 * 1000;

      FPhysicsServer.RegisterStream(C_BuildStableIO).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_BuildStableIO;
      FPhysicsServer.RegisterStream(C_OpenStableIO).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_OpenStableIO;
    end;
end;

procedure TCommunicationFramework_CustomStableServer.cmd_BuildStableIO(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
var
  io_def: TStableServer_PhysicsIO_UserDefine;
  s_io: TStableServer_PeerIO;
begin
  io_def := Sender.UserDefine as TStableServer_PhysicsIO_UserDefine;
  s_io := TStableServer_PeerIO.Create(Self, nil);
  s_io.Activted := True;
  s_io.FSequencePacketActivted := True;
  s_io.FSequencePacketSignal := True;
  s_io.SequencePacketLimitPhysicsMemory := FLimitSequencePacketMemoryUsage;
  s_io.DestroyRecyclePhysicsIO := True;
  s_io.BindPhysicsIO := Sender;
  s_io.Connection_Token := Connection_Token_Counter;
  inc(Connection_Token_Counter);
  io_def.BindStableIO := s_io;

  OutData.WriteBool(True);
  OutData.WriteCardinal(s_io.Connection_Token);
  OutData.WriteCardinal(s_io.FID);
  OutData.WriteByte(Byte(s_io.FSendDataCipherSecurity));
  OutData.WriteArrayByte.SetBuff(@s_io.FCipherKey[0], Length(s_io.FCipherKey));
end;

procedure TCommunicationFramework_CustomStableServer.cmd_OpenStableIO(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
var
  io_def: TStableServer_PhysicsIO_UserDefine;
  connToken: Cardinal;
  arry: TDataFrameArrayByte;
  connKey: TBytes;
  IO_Array: TIO_Array;
  IO_ID: Cardinal;
  io_temp, io_picked: TStableServer_PeerIO;
begin
  io_def := Sender.UserDefine as TStableServer_PhysicsIO_UserDefine;

  io_picked := nil;
  connToken := InData.Reader.ReadCardinal;
  arry := InData.Reader.ReadArrayByte;
  SetLength(connKey, arry.Count);
  arry.GetBuff(@connKey[0]);

  GetIO_Array(IO_Array);
  for IO_ID in IO_Array do
    begin
      io_temp := TStableServer_PeerIO(PeerIO[IO_ID]);
      if (io_temp <> nil) and (io_temp.Activted) and
        (io_temp.Connection_Token = connToken) and (TCipher.CompareKey(connKey, io_temp.FCipherKey)) then
        begin
          io_picked := io_temp;
          Break;
        end;
    end;

  if io_picked = nil then
    begin
      OutData.WriteBool(False);
      OutData.WriteString(PFormat('illegal Request Token: maybe you cant use StableIO again after server is restarted.', []));
      exit;
    end;

  io_picked.BindPhysicsIO := Sender;
  io_picked.Activted := True;
  io_picked.DestroyRecyclePhysicsIO := True;
  io_picked.UserDefine.WorkPlatform := io_def.WorkPlatform;
  io_def.BindStableIO := io_picked;
  io_picked.ResetSequencePacketBuffer;
  io_picked.SequencePacketVerifyTick := GetTimeTick;
  io_picked.OfflineTick := GetTimeTick;

  OutData.WriteBool(True);
  OutData.WriteCardinal(io_picked.Connection_Token);
  OutData.WriteCardinal(io_picked.ID);
  OutData.WriteByte(Byte(io_picked.FSendDataCipherSecurity));
  OutData.WriteArrayByte.SetBuff(@io_picked.FCipherKey[0], Length(io_picked.FCipherKey));
end;

procedure TCommunicationFramework_CustomStableServer.cmd_CloseStableIO(Sender: TPeerIO; InData: SystemString);
var
  s_io: TStableServer_PeerIO;
begin
  s_io := Sender as TStableServer_PeerIO;
  s_io.Disconnect;
end;

constructor TCommunicationFramework_CustomStableServer.Create;
begin
  inherited Create;
  EnabledAtomicLockAndMultiThread := False;
  SwitchMaxSecurity;

  RegisterDirectConsole(C_CloseStableIO).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_CloseStableIO;

  Connection_Token_Counter := 1;
  FPhysicsServer := nil;
  FOfflineTimeout := 1000 * 60 * 5;
  FLimitSequencePacketMemoryUsage := 0;
  FAutoFreePhysicsServer := False;
  FAutoProgressPhysicsServer := True;
  CustomStableServerProgressing := False;

  Name := 'StableServer';
end;

destructor TCommunicationFramework_CustomStableServer.Destroy;
var
  phyServ: TCommunicationFrameworkServer;
begin
  UnRegisted(C_CloseStableIO);

  while Count > 0 do
      DisposeObject(FirstIO);

  StopService;
  phyServ := FPhysicsServer;
  SetPhysicsServer(nil);
  if FAutoFreePhysicsServer and (phyServ <> nil) then
      DisposeObject(phyServ);
  inherited Destroy;
end;

function TCommunicationFramework_CustomStableServer.StartService(Host: SystemString; Port: Word): Boolean;
begin
  Result := False;
  if FPhysicsServer <> nil then
      Result := FPhysicsServer.StartService(Host, Port);
end;

procedure TCommunicationFramework_CustomStableServer.StopService;
begin
  if FPhysicsServer <> nil then
      FPhysicsServer.StopService;
end;

procedure TCommunicationFramework_CustomStableServer.Progress;
begin
  if CustomStableServerProgressing then
      exit;

  CustomStableServerProgressing := True;
  if (FPhysicsServer <> nil) and (FAutoProgressPhysicsServer) then
      FPhysicsServer.Progress;
  inherited Progress;
  CustomStableServerProgressing := False;
end;

procedure TCommunicationFramework_CustomStableServer.TriggerQueueData(v: PQueueData);
var
  c: TPeerIO;
begin
  c := PeerIO[v^.IO_ID];
  if c <> nil then
    begin
      c.PostQueueData(v);
      c.ProcessAllSendCmd(nil, False, False);
    end
  else
      DisposeQueueData(v);
end;

function TCommunicationFramework_CustomStableServer.WaitSendConsoleCmd(P_IO: TPeerIO;
  const Cmd, ConsoleData: SystemString; Timeout: TTimeTick): SystemString;
begin
  Result := '';
  RaiseInfo('WaitSend no Suppport');
end;

procedure TCommunicationFramework_CustomStableServer.WaitSendStreamCmd(P_IO: TPeerIO;
  const Cmd: SystemString; StreamData, ResultData: TDataFrameEngine; Timeout: TTimeTick);
begin
  RaiseInfo('WaitSend no Suppport');
end;

procedure TStableClient_PeerIO.CreateAfter;
begin
  inherited CreateAfter;
  Activted := False;
  WaitConnecting := False;
  PhysicsIO_LastConnectTick := GetTimeTick;
  Connection_Token := 0;
  BindPhysicsIO := nil;
end;

destructor TStableClient_PeerIO.Destroy;
begin
  TCommunicationFramework_CustomStableClient(FOwnerFramework).DoDisconnect(Self);

  if (BindPhysicsIO <> nil) then
      BindPhysicsIO.DelayClose;

  inherited Destroy;
end;

function TStableClient_PeerIO.Connected: Boolean;
begin
  Result := True;
end;

procedure TStableClient_PeerIO.Disconnect;
begin
  if (BindPhysicsIO <> nil) then
      BindPhysicsIO.Disconnect;

  TCommunicationFramework_CustomStableClient(FOwnerFramework).Disconnect;
end;

procedure TStableClient_PeerIO.SendByteBuffer(const buff: PByte; const Size: NativeInt);
begin
  if (BindPhysicsIO = nil) or (not Activted) or (WaitConnecting) then
    begin
      AtomDec(FOwnerFramework.Statistics[TStatisticsType.stSendSize], Size);
      exit;
    end;

  BindPhysicsIO.SendByteBuffer(buff, Size);
end;

procedure TStableClient_PeerIO.WriteBufferOpen;
begin
  if BindPhysicsIO <> nil then
      BindPhysicsIO.WriteBufferOpen;
end;

procedure TStableClient_PeerIO.WriteBufferFlush;
begin
  if (BindPhysicsIO = nil) or (not Activted) or (WaitConnecting) then
      exit;
  BindPhysicsIO.WriteBufferFlush;
end;

procedure TStableClient_PeerIO.WriteBufferClose;
begin
  if (BindPhysicsIO = nil) or (not Activted) or (WaitConnecting) then
      exit;
  BindPhysicsIO.WriteBufferClose;
end;

function TStableClient_PeerIO.GetPeerIP: SystemString;
begin
  if (BindPhysicsIO = nil) or (not Activted) or (WaitConnecting) then
      Result := 'offline'
  else
      Result := BindPhysicsIO.GetPeerIP;
end;

function TStableClient_PeerIO.WriteBufferEmpty: Boolean;
begin
  if (BindPhysicsIO = nil) or (not Activted) or (WaitConnecting) then
      Result := False
  else
      Result := BindPhysicsIO.WriteBufferEmpty;
end;

procedure TStableClient_PeerIO.Progress;
begin
  ProcessAllSendCmd(nil, False, False);
  inherited Progress;
end;

procedure TCommunicationFramework_CustomStableClient.ClientCustomProtocolReceiveBufferNotify(Sender: TCommunicationFrameworkClient; const buffer: PByte; const Size: NativeInt; var FillDone: Boolean);
begin
  KeepAliveChecking := False;

  FillDone := FStableClientIO.Activted and (not FStableClientIO.WaitConnecting) and (FStableClientIO.BindPhysicsIO <> nil);
  if FillDone then
    begin
      FStableClientIO.SaveReceiveBuffer(buffer, Size);
      FStableClientIO.FillRecvBuffer(nil, False, False);
    end;
end;

procedure TCommunicationFramework_CustomStableClient.SetPhysicsClient(const Value: TCommunicationFrameworkClient);
begin
  if FPhysicsClient <> nil then
    begin
      Disconnect;
      FPhysicsClient.FOnClientCustomProtocolReceiveBufferNotify := nil;
      FPhysicsClient.Protocol := TCommunicationProtocol.cpZServer;
      FPhysicsClient.TimeOutIDLE := 0;
      FPhysicsClient.QuietMode := False;
    end;

  FPhysicsClient := Value;

  if FPhysicsClient <> nil then
    begin
      FPhysicsClient.FOnClientCustomProtocolReceiveBufferNotify := {$IFDEF FPC}@{$ENDIF FPC}ClientCustomProtocolReceiveBufferNotify;
      FPhysicsClient.Protocol := TCommunicationProtocol.cpCustom;
      FPhysicsClient.TimeOutIDLE := 0;
      FPhysicsClient.QuietMode := False;
    end;
end;

procedure TCommunicationFramework_CustomStableClient.BuildStableIO_Result(Sender: TPeerIO; ResultData: TDataFrameEngine);
var
  r_token, r_id: Cardinal;
  cSec: TCipherSecurity;
  arry: TDataFrameArrayByte;
  i: Integer;
  k: TCipherKeyBuffer;
begin
  if ResultData.Reader.ReadBool then
    begin
      r_token := ResultData.Reader.ReadCardinal;
      r_id := ResultData.Reader.ReadCardinal;
      cSec := TCipherSecurity(ResultData.Reader.ReadByte);
      arry := ResultData.Reader.ReadArrayByte;
      SetLength(k, arry.Count);
      for i := 0 to arry.Count - 1 do
          k[i] := arry[i];

      { connection token }
      FStableClientIO.Connection_Token := r_token;
      { bind physics IO }
      FStableClientIO.BindPhysicsIO := Sender;
      { remote id }
      FStableClientIO.ID := r_id;
      { Encrypt }
      FStableClientIO.FSendDataCipherSecurity := cSec;
      FStableClientIO.FCipherKey := TCipher.CopyKey(k);
      { switch state }
      FStableClientIO.Activted := True;
      FStableClientIO.WaitConnecting := False;
      { replace encrypt for physics IO }
      Sender.FSendDataCipherSecurity := cSec;
      Sender.FCipherKey := TCipher.CopyKey(k);
      { open sequence packet model }
      FStableClientIO.FSequencePacketActivted := True;
      FStableClientIO.FSequencePacketSignal := True;
      FStableClientIO.SequencePacketLimitPhysicsMemory := FLimitSequencePacketMemoryUsage;

      { triger }
      TriggerDoConnectFinished;
      DoConnected(FStableClientIO);
      FStableClientIO.LastCommunicationTick_Received := GetTimeTick;
      FStableClientIO.LastCommunicationTick_KeepAlive := FStableClientIO.LastCommunicationTick_Received;

      FPhysicsClient.ClientIO.Print('StableIO connection %s port:%d Success.', [FConnection_Addr, FConnection_Port]);
    end
  else
    begin
      Sender.PrintError(ResultData.Reader.ReadString);
      TriggerDoConnectFailed;
    end;
end;

procedure TCommunicationFramework_CustomStableClient.AsyncConnectResult(const cState: Boolean);
var
  de: TDataFrameEngine;
begin
  if cState then
    begin
      de := TDataFrameEngine.Create;
      FPhysicsClient.SendStreamCmdM(C_BuildStableIO, de, {$IFDEF FPC}@{$ENDIF FPC}BuildStableIO_Result);
      DisposeObject(de);
    end
  else
    begin
      FStableClientIO.WaitConnecting := False;

      if FAutomatedConnection then
          PostProgress.PostExecuteM(1.0, {$IFDEF FPC}@{$ENDIF FPC}PostConnection)
      else
          TriggerDoConnectFailed;
    end;
end;

procedure TCommunicationFramework_CustomStableClient.PostConnection(Sender: TNPostExecute);
begin
  if FStableClientIO.WaitConnecting then
      exit;

  FStableClientIO.WaitConnecting := True;
  FPhysicsClient.AsyncConnectM(FConnection_Addr, FConnection_Port, {$IFDEF FPC}@{$ENDIF FPC}AsyncConnectResult);
end;

procedure TCommunicationFramework_CustomStableClient.OpenStableIO_Result(Sender: TPeerIO; ResultData: TDataFrameEngine);
var
  r_token, r_id: Cardinal;
  cSec: TCipherSecurity;
  arry: TDataFrameArrayByte;
  k: TCipherKeyBuffer;
begin
  if ResultData.Reader.ReadBool then
    begin
      r_token := ResultData.Reader.ReadCardinal;
      r_id := ResultData.Reader.ReadCardinal;
      cSec := TCipherSecurity(ResultData.Reader.ReadByte);
      arry := ResultData.Reader.ReadArrayByte;
      SetLength(k, arry.Count);
      arry.GetBuff(@k[0]);

      { connection token }
      FStableClientIO.Connection_Token := r_token;
      { bind physics IO }
      FStableClientIO.BindPhysicsIO := Sender;
      { remote id }
      FStableClientIO.ID := r_id;
      { Encrypt }
      FStableClientIO.FSendDataCipherSecurity := cSec;
      FStableClientIO.FCipherKey := TCipher.CopyKey(k);
      { remote inited }
      FStableClientIO.RemoteExecutedForConnectInit := True;
      { switch state }
      FStableClientIO.Activted := True;
      FStableClientIO.WaitConnecting := False;
      { replace encrypt for physics IO }
      Sender.FSendDataCipherSecurity := cSec;
      Sender.FCipherKey := TCipher.CopyKey(k);
      { sequence packet model }
      FStableClientIO.FSequencePacketActivted := True;
      FStableClientIO.FSequencePacketSignal := True;
      FStableClientIO.SequencePacketLimitPhysicsMemory := FLimitSequencePacketMemoryUsage;
      FStableClientIO.ResetSequencePacketBuffer;
      FStableClientIO.SequencePacketVerifyTick := GetTimeTick;
      FStableClientIO.Print('StableIO calibrate session.', []);
    end
  else
    begin
      Sender.PrintError(ResultData.Reader.ReadString);

      FStableClientIO.WaitConnecting := False;

      FStableClientIO.Activted := False;
      FStableClientIO.BindPhysicsIO := nil;

      FOnAsyncConnectNotifyCall := nil;
      FOnAsyncConnectNotifyMethod := nil;
      FOnAsyncConnectNotifyProc := nil;
      FStableClientIO.DelayClose();

      if AutomatedConnection then
          PostProgress.PostExecuteM(1.0, {$IFDEF FPC}@{$ENDIF FPC}PostConnection);
    end;
end;

procedure TCommunicationFramework_CustomStableClient.AsyncReconnectionResult(const cState: Boolean);
var
  de: TDataFrameEngine;
begin
  if not FStableClientIO.WaitConnecting then
      exit;

  if cState then
    begin
      de := TDataFrameEngine.Create;
      de.WriteCardinal(FStableClientIO.Connection_Token);
      de.WriteArrayByte.SetBuff(@FStableClientIO.FCipherKey[0], Length(FStableClientIO.FCipherKey));
      FPhysicsClient.SendStreamCmdM(C_OpenStableIO, de, {$IFDEF FPC}@{$ENDIF FPC}OpenStableIO_Result);
      DisposeObject(de);
    end
  else
    begin
      FStableClientIO.WaitConnecting := False;
    end;
end;

procedure TCommunicationFramework_CustomStableClient.PostReconnection(Sender: TNPostExecute);
begin
  if not FStableClientIO.Activted then
      exit;
  if FPhysicsClient = nil then
      exit;
  if not FStableClientIO.WaitConnecting then
      exit;

  FPhysicsClient.AsyncConnectM(FConnection_Addr, FConnection_Port, {$IFDEF FPC}@{$ENDIF FPC}AsyncReconnectionResult);
end;

procedure TCommunicationFramework_CustomStableClient.Reconnection;
begin
  if not FStableClientIO.Activted then
      exit;
  if FPhysicsClient = nil then
      exit;
  if FStableClientIO.WaitConnecting then
      exit;

  FStableClientIO.WaitConnecting := True;
  FStableClientIO.PhysicsIO_LastConnectTick := GetTimeTick;
  FStableClientIO.BindPhysicsIO := nil;

  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := nil;
  FOnAsyncConnectNotifyProc := nil;
  PostProgress.PostExecuteM(0.5, {$IFDEF FPC}@{$ENDIF FPC}PostReconnection);
end;

function TCommunicationFramework_CustomStableClient.GetStopCommunicationTimeTick: TTimeTick;
begin
  if KeepAliveChecking then
      Result := GetTimeTick - SaveLastCommunicationTick_Received
  else
      Result := GetTimeTick - FStableClientIO.LastCommunicationTick_Received;
end;

constructor TCommunicationFramework_CustomStableClient.Create;
begin
  inherited Create;
  EnabledAtomicLockAndMultiThread := False;
  FIgnoreProcessConnectedAndDisconnect := True;

  FPhysicsClient := nil;
  FStableClientIO := TStableClient_PeerIO.Create(Self, nil);

  FConnection_Addr := '';
  FConnection_Port := 0;
  FAutomatedConnection := True;
  FLimitSequencePacketMemoryUsage := 0;
  FAutoFreePhysicsClient := False;
  FAutoProgressPhysicsClient := True;
  CustomStableClientProgressing := False;
  KeepAliveChecking := False;
  SaveLastCommunicationTick_Received := FStableClientIO.LastCommunicationTick_Received;

  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := nil;
  FOnAsyncConnectNotifyProc := nil;
  Name := 'StableClient';
end;

destructor TCommunicationFramework_CustomStableClient.Destroy;
var
  phyCli: TCommunicationFrameworkClient;
begin
  Disconnect;

  phyCli := FPhysicsClient;
  SetPhysicsClient(nil);
  if (phyCli <> nil) and (FAutoFreePhysicsClient) then
      DisposeObject(phyCli);
  inherited Destroy;
end;

procedure TCommunicationFramework_CustomStableClient.TriggerDoConnectFailed;
begin
  inherited TriggerDoConnectFailed;

  try
    if Assigned(FOnAsyncConnectNotifyCall) then
        FOnAsyncConnectNotifyCall(False);
    if Assigned(FOnAsyncConnectNotifyMethod) then
        FOnAsyncConnectNotifyMethod(False);
    if Assigned(FOnAsyncConnectNotifyProc) then
        FOnAsyncConnectNotifyProc(False);
  except
  end;

  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := nil;
  FOnAsyncConnectNotifyProc := nil;
end;

procedure TCommunicationFramework_CustomStableClient.TriggerDoConnectFinished;
begin
  inherited TriggerDoConnectFinished;

  try
    if Assigned(FOnAsyncConnectNotifyCall) then
        FOnAsyncConnectNotifyCall(True);
    if Assigned(FOnAsyncConnectNotifyMethod) then
        FOnAsyncConnectNotifyMethod(True);
    if Assigned(FOnAsyncConnectNotifyProc) then
        FOnAsyncConnectNotifyProc(True);
  except
  end;

  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := nil;
  FOnAsyncConnectNotifyProc := nil;
end;

procedure TCommunicationFramework_CustomStableClient.AsyncConnectC(addr: SystemString; Port: Word; OnResult: TStateCall);
begin
  Disconnect;

  FStableClientIO.Activted := False;
  FStableClientIO.BindPhysicsIO := nil;

  FConnection_Addr := addr;
  FConnection_Port := Port;

  if FPhysicsClient = nil then
    begin
      if Assigned(OnResult) then
          OnResult(False);
      exit;
    end;

  FOnAsyncConnectNotifyCall := OnResult;
  FOnAsyncConnectNotifyMethod := nil;
  FOnAsyncConnectNotifyProc := nil;
  PostProgress.PostExecuteM(0.0, {$IFDEF FPC}@{$ENDIF FPC}PostConnection);
end;

procedure TCommunicationFramework_CustomStableClient.AsyncConnectM(addr: SystemString; Port: Word; OnResult: TStateMethod);
begin
  Disconnect;

  FStableClientIO.Activted := False;
  FStableClientIO.BindPhysicsIO := nil;

  FConnection_Addr := addr;
  FConnection_Port := Port;

  if FPhysicsClient = nil then
    begin
      if Assigned(OnResult) then
          OnResult(False);
      exit;
    end;

  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := OnResult;
  FOnAsyncConnectNotifyProc := nil;
  PostProgress.PostExecuteM(0.0, {$IFDEF FPC}@{$ENDIF FPC}PostConnection);
end;

procedure TCommunicationFramework_CustomStableClient.AsyncConnectP(addr: SystemString; Port: Word; OnResult: TStateProc);
begin
  Disconnect;

  FStableClientIO.Activted := False;
  FStableClientIO.BindPhysicsIO := nil;

  FConnection_Addr := addr;
  FConnection_Port := Port;

  if FPhysicsClient = nil then
    begin
      if Assigned(OnResult) then
          OnResult(False);
      exit;
    end;

  FOnAsyncConnectNotifyCall := nil;
  FOnAsyncConnectNotifyMethod := nil;
  FOnAsyncConnectNotifyProc := OnResult;

  PostProgress.PostExecuteM(0.0, {$IFDEF FPC}@{$ENDIF FPC}PostConnection);
end;

function TCommunicationFramework_CustomStableClient.Connect(addr: SystemString; Port: Word): Boolean;
var
  t: TTimeTick;
begin
  Disconnect;

  FStableClientIO.Activted := False;
  FStableClientIO.BindPhysicsIO := nil;

  FConnection_Addr := addr;
  FConnection_Port := Port;

  Result := False;

  if FPhysicsClient = nil then
      exit;

  if FPhysicsClient.Connect(addr, Port) then
    begin
      AsyncConnectResult(True);

      t := GetTimeTick;
      while (not FStableClientIO.Activted) and (GetTimeTick - t < 5000) do
          Progress;
      Result := FStableClientIO.Activted;
    end;
end;

function TCommunicationFramework_CustomStableClient.Connected: Boolean;
begin
  Result := FStableClientIO.Activted;
end;

procedure TCommunicationFramework_CustomStableClient.Disconnect;
begin
  KeepAliveChecking := False;
  if (FPhysicsClient <> nil) and (FPhysicsClient.Connected) and (FStableClientIO.Activted) then
    begin
      FStableClientIO.FSequencePacketSignal := False;
      while FStableClientIO.IOBusy do
          Progress;
      FStableClientIO.SendDirectConsoleCmd(C_CloseStableIO);
      FStableClientIO.Progress;
      while FPhysicsClient.Connected and (not FPhysicsClient.ClientIO.WriteBufferEmpty) do
          FPhysicsClient.Progress;
      FStableClientIO.FSequencePacketSignal := True;
    end;

  DisposeObject(FStableClientIO);
  FStableClientIO := TStableClient_PeerIO.Create(Self, nil);
end;

function TCommunicationFramework_CustomStableClient.ClientIO: TPeerIO;
begin
  Result := FStableClientIO;
end;

procedure TCommunicationFramework_CustomStableClient.TriggerQueueData(v: PQueueData);
begin
  if not Connected then
    begin
      DisposeQueueData(v);
      exit;
    end;

  ClientIO.PostQueueData(v);
  ClientIO.FillRecvBuffer(nil, False, False);
end;

procedure TCommunicationFramework_CustomStableClient.Progress;
var
  t: TTimeTick;
begin
  if CustomStableClientProgressing then
      exit;

  CustomStableClientProgressing := True;
  if (FPhysicsClient <> nil) and (FAutoProgressPhysicsClient) then
    begin
      FPhysicsClient.Progress;
    end;
  inherited Progress;

  if FPhysicsClient <> nil then
    if (FStableClientIO.Activted) and (FStableClientIO.IsSequencePacketModel) then
      begin
        t := GetTimeTick;
        if FStableClientIO.WaitConnecting then
          begin
            if t - FStableClientIO.PhysicsIO_LastConnectTick > 5000 then
              begin
                KeepAliveChecking := False;
                FStableClientIO.WaitConnecting := False;
                FPhysicsClient.Disconnect;
                Reconnection;
              end;
          end
        else if not FPhysicsClient.Connected then
          begin
            KeepAliveChecking := False;
            Reconnection;
          end
        else if (FStableClientIO.FSequencePacketSignal) and (t - FStableClientIO.LastCommunicationTick_Received > 5000) then
          begin
            if (KeepAliveChecking) then
              begin
                FStableClientIO.LastCommunicationTick_Received := SaveLastCommunicationTick_Received;
                FStableClientIO.LastCommunicationTick_KeepAlive := FStableClientIO.LastCommunicationTick_Received;
                Reconnection;
                KeepAliveChecking := False;
              end
            else
              begin
                FStableClientIO.SendSequencePacketKeepAlive(nil, 0);
                SaveLastCommunicationTick_Received := FStableClientIO.LastCommunicationTick_Received;
                FStableClientIO.LastCommunicationTick_Received := GetTimeTick;
                FStableClientIO.LastCommunicationTick_KeepAlive := FStableClientIO.LastCommunicationTick_Received;
                KeepAliveChecking := True;
              end;
          end;
      end;

  CustomStableClientProgressing := False;
end;

initialization

ProgressBackgroundProc := nil;
ProgressBackgroundMethod := nil;

finalization

end.
