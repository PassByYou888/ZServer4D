{ ****************************************************************************** }
{ * communication framework written by QQ 600585@qq.com                        * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
(* https://github.com/PassByYou888/ZServer4D *)
{ ****************************************************************************** }
(*
  update history
  2017-11-28  support anonymous function byQQ600585
*)

unit CommunicationFramework;

interface

{$I zDefine.inc}


uses Classes, SysUtils, Variants, TypInfo,
  CoreClasses, ListEngine, UnicodeMixedLib, DoStatusIO,
  DataFrameEngine, MemoryStream64, PascalStrings, CoreCipher, NotifyObjectBase, Cadencer;

type
  TPeerClient = class;

  TConsoleMethod     = procedure(Sender: TPeerClient; ResultData: string) of object;
  TStreamMethod      = procedure(Sender: TPeerClient; ResultData: TDataFrameEngine) of object;
  TStreamParamMethod = procedure(Sender: TPeerClient; Param1: Pointer; Param2: TObject; InData, ResultData: TDataFrameEngine) of object;

  {$IFNDEF FPC}
  TConsoleProc     = reference to procedure(Sender: TPeerClient; ResultData: string);
  TStreamProc      = reference to procedure(Sender: TPeerClient; ResultData: TDataFrameEngine);
  TStreamParamProc = reference to procedure(Sender: TPeerClient; Param1: Pointer; Param2: TObject; InData, ResultData: TDataFrameEngine);

  TStateProc = reference to procedure(const State: Boolean);
  {$ENDIF}
  TQueueState = (qsUnknow, qsSendConsoleCMD, qsSendStreamCMD, qsSendDirectConsoleCMD, qsSendDirectStreamCMD, qsSendBigStream);

  TQueueData = record
    State: TQueueState;
    Client: TPeerClient;
    Cmd: string;
    Cipher: TCipherStyle;

    ConsoleData: string;
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
    FCommand: string;

    FOnCommandStreamProc: TCommandStreamProc;
  public
    constructor Create;
    destructor Destroy; override;

    function Execute(Sender: TPeerClient; InData, OutData: TDataFrameEngine): Boolean;

    property Command: string read FCommand write FCommand;
    property OnExecute: TCommandStreamProc read FOnCommandStreamProc write FOnCommandStreamProc;
  end;

  TCommandConsoleProc = procedure(Sender: TPeerClient; InData: string; var OutData: string) of object;

  TCommandConsoleMode = class(TCoreClassObject)
  private
  protected
    FCommand: string;

    FOnCommandConsoleProc: TCommandConsoleProc;
  public
    constructor Create;
    destructor Destroy; override;

    function Execute(Sender: TPeerClient; InData: string; var OutData: string): Boolean;

    property Command: string read FCommand write FCommand;
    property OnExecute: TCommandConsoleProc read FOnCommandConsoleProc write FOnCommandConsoleProc;
  end;

  TCommandDirectStreamProc = procedure(Sender: TPeerClient; InData: TDataFrameEngine) of object;

  TCommandDirectStreamMode = class(TCoreClassObject)
  private
  protected
    FCommand: string;

    FOnCommandDirectStreamProc: TCommandDirectStreamProc;
  public
    constructor Create;
    destructor Destroy; override;

    function Execute(Sender: TPeerClient; InData: TDataFrameEngine): Boolean;

    property Command: string read FCommand write FCommand;
    property OnExecute: TCommandDirectStreamProc read FOnCommandDirectStreamProc write FOnCommandDirectStreamProc;
  end;

  TCommandDirectConsoleProc = procedure(Sender: TPeerClient; InData: string) of object;

  TCommandDirectConsoleMode = class(TCoreClassObject)
  private
  protected
    FCommand: string;

    FOnCommandDirectConsoleProc: TCommandDirectConsoleProc;
  public
    constructor Create;
    destructor Destroy; override;

    function Execute(Sender: TPeerClient; InData: string): Boolean;

    property Command: string read FCommand write FCommand;
    property OnExecute: TCommandDirectConsoleProc read FOnCommandDirectConsoleProc write FOnCommandDirectConsoleProc;
  end;

  TCommandBigStreamProc = procedure(Sender: TPeerClient; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64) of object;

  TCommandBigStreamMode = class(TCoreClassObject)
  private
  protected
    FCommand: string;

    FOnCommandBigStreamProc: TCommandBigStreamProc;
  public
    constructor Create;
    destructor Destroy; override;

    function Execute(Sender: TPeerClient; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64): Boolean;

    property Command: string read FCommand write FCommand;
    property OnExecute: TCommandBigStreamProc read FOnCommandBigStreamProc write FOnCommandBigStreamProc;
  end;

  TCommunicationFramework = class;

  TPeerClientUserDefine = class(TCoreClassObject)
  protected
    FOwner       : TPeerClient;
    FWorkPlatform: TExecutePlatform;
  public
    constructor Create(AOwner: TPeerClient); virtual;

    property Owner: TPeerClient read FOwner;
    property WorkPlatform: TExecutePlatform read FWorkPlatform;
  end;

  TPeerClientUserDefineClass = class of TPeerClientUserDefine;

  TPeerClientUserSpecial = class(TCoreClassInterfacedObject)
  protected
    FOwner: TPeerClient;
  public
    constructor Create(AOwner: TPeerClient); virtual;

    property Owner: TPeerClient read FOwner;
  end;

  TPeerClientUserSpecialClass = class of TPeerClientUserSpecial;

  TPeerClient = class(TCoreClassInterfacedObject, IMemoryStream64WriteTrigger)
  private
    FOwnerFramework                     : TCommunicationFramework;
    FClientIntf                         : TCoreClassObject;
    FID                                 : Cardinal;
    FHeadFlag                           : Byte;
    FReceivedBuffer                     : TMemoryStream64OfWriteTrigger;
    FBigStreamReceiveProcessing         : Boolean;
    FBigStreamTotal, FBigStreamCompleted: Int64;
    FBigStreamCmd                       : string;
    FBigStreamReceive                   : TCoreClassStream;
    FBigStreamSending                   : TCoreClassStream;
    FBigStreamSendState                 : Int64; // stream current position
    FBigStreamSendDoneTimeFree          : Boolean;
    FCurrentQueueData                   : PQueueData;
    FWaitOnResult                       : Boolean;
    FCurrentPauseResultSend_CommDataType: Byte;
    FCanPauseResultSend                 : Boolean;
    FPauseResultSend                    : Boolean;
    FRunReceiveTrigger                  : Boolean;
    FReceiveDataCipherStyle             : TCipherStyle;
    FResultDataBuffer                   : TMemoryStream64;
    FSendDataCipherStyle                : TCipherStyle;
    FAllSendProcessing                  : Boolean;
    FQueueList                          : TCoreClassList;
    FLastCommunicationTimeTickCount     : TTimeTickValue;
    FCipherKey                          : TCipherKeyBuffer;
    FRemoteExecutedForConnectInit       : Boolean;
    FPrintStatus                        : Boolean;
    FInCmd                              : string;
    FInText, FOutText                   : string;
    FInDataFrame, FOutDataFrame         : TDataFrameEngine;
    ResultText                          : string;
    ResultDataFrame                     : TDataFrameEngine;
    FSyncPick                           : PQueueData;
    FWaitSendBusy                       : Boolean;
    FReceiveCommandRuning               : Boolean;
    FReceiveResultRuning                : Boolean;
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
    procedure InternalSendByteBuffer(buff: PByte; Size: Integer); inline;

    procedure SendInteger(v: Integer); inline;
    procedure SendCardinal(v: Cardinal); inline;
    procedure SendInt64(v: Int64); inline;
    procedure SendByte(v: Byte); inline;
    procedure SendWord(v: Word); inline;
    procedure SendVerifyCode(buff: Pointer; siz: Integer); inline;
    procedure SendEncryptBuffer(buff: PByte; Size: Integer; cs: TCipherStyle); inline;
    procedure SendEncryptMemoryStream(stream: TMemoryStream64; cs: TCipherStyle); inline;

    procedure InternalSendConsoleBuff(buff: TMemoryStream64; cs: TCipherStyle);
    procedure InternalSendStreamBuff(buff: TMemoryStream64; cs: TCipherStyle);
    procedure InternalSendDirectConsoleBuff(buff: TMemoryStream64; cs: TCipherStyle);
    procedure InternalSendDirectStreamBuff(buff: TMemoryStream64; cs: TCipherStyle);
    procedure InternalSendBigStreamHeader(Cmd: string; streamSiz: Int64);
    procedure InternalSendBigStreamBuff(var Queue: TQueueData);

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
    function GetPeerIP: string; virtual; abstract;
  public
    function WriteBufferEmpty: Boolean; virtual;
  public
    constructor Create(AOwnerFramework: TCommunicationFramework; AClientIntf: TCoreClassObject); virtual;
    destructor Destroy; override;

    procedure Print(v: string); overload;
    procedure Print(v: string; const Args: array of const); overload;
    procedure PrintParam(v: string; Args: string);

    procedure Progress; virtual;

    property ReceivedBuffer: TMemoryStream64OfWriteTrigger read FReceivedBuffer;
    procedure FillRecvBuffer(ACurrentActiveThread: TCoreClassThread; const RecvSync, SendSync: Boolean);
    procedure PostQueueData(p: PQueueData);
    function ProcessAllSendCmd(ACurrentActiveThread: TCoreClassThread; const RecvSync, SendSync: Boolean): Integer;

    procedure PauseResultSend; virtual;
    procedure ContinueResultSend; virtual;

    property CurrentBigStreamCommand: string read FBigStreamCmd;
    property CurrentCommand: string read FInCmd;

    // ContinueResultSend use it
    property InText: string read FInText;
    property OutText: string read FOutText write FOutText;
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

    // framework
    property OwnerFramework: TCommunicationFramework read FOwnerFramework;
    property ClientIntf: TCoreClassObject read FClientIntf write FClientIntf;
    property ID: Cardinal read FID;
    property CipherKey: TCipherKeyBuffer read FCipherKey;
    function CipherKeyPtr: PCipherKeyBuffer;
    property SendCipherStyle: TCipherStyle read FSendDataCipherStyle write FSendDataCipherStyle;
    property RemoteExecutedForConnectInit: Boolean read FRemoteExecutedForConnectInit;
    property PrintStatus: Boolean read FPrintStatus write FPrintStatus;

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
    procedure SendConsoleCmd(Cmd: string; ConsoleData: string; OnResult: TConsoleMethod); overload;
    procedure SendStreamCmd(Cmd: string; StreamData: TCoreClassStream; OnResult: TStreamMethod; DoneFreeStream: Boolean); overload;
    procedure SendStreamCmd(Cmd: string; StreamData: TDataFrameEngine; OnResult: TStreamMethod); overload;
    procedure SendStreamCmd(Cmd: string; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamMethod); overload;

    // send cmd and proc return
    {$IFNDEF FPC}
    procedure SendConsoleCmd(Cmd: string; ConsoleData: string; OnResult: TConsoleProc); overload;
    procedure SendStreamCmd(Cmd: string; StreamData: TCoreClassStream; OnResult: TStreamProc; DoneFreeStream: Boolean); overload;
    procedure SendStreamCmd(Cmd: string; StreamData: TDataFrameEngine; OnResult: TStreamProc); overload;
    procedure SendStreamCmd(Cmd: string; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamProc); overload;
    {$ENDIF}
    // direct send cmd
    procedure SendDirectConsoleCmd(Cmd: string; ConsoleData: string);
    procedure SendDirectStreamCmd(Cmd: string; StreamData: TCoreClassStream; DoneFreeStream: Boolean); overload;
    procedure SendDirectStreamCmd(Cmd: string; StreamData: TDataFrameEngine); overload;
    procedure SendDirectStreamCmd(Cmd: string); overload;

    // wait send cmd
    function WaitSendConsoleCmd(Cmd: string; ConsoleData: string; TimeOut: TTimeTickValue): string;
    procedure WaitSendStreamCmd(Cmd: string; StreamData, ResultData: TDataFrameEngine; TimeOut: TTimeTickValue);

    // send bigstream
    procedure SendBigStream(Cmd: string; BigStream: TCoreClassStream; DoneFreeStream: Boolean);
  end;

  TPeerClientNotify    = procedure(Sender: TPeerClient) of object;
  TPeerClientCMDNotify = procedure(Sender: TPeerClient; Cmd: string; var Allow: Boolean) of object;

  TStatisticsType = (
    stReceiveSize, stSendSize,
    stRequest, stResponse,
    stConsole, stStream, stDirestConsole, stDirestStream, stReceiveBigStream, stSendBigStream,
    stExecConsole, stExecStream, stExecDirestConsole, stExecDirestStream, stExecBigStream,
    stConnest, stDisconnest,
    stTotalCommandExecute, stTotalCommandSend, stTotalCommandReg,
    stEncrypt, stCompress, stGenerateHash,
    stPause, stContinue,
    stLock, stUnLock,
    stPrint, stIDCounter);

  TCommunicationFramework = class(TCoreClassInterfacedObject)
  private
    FCommandList               : THashObjectList;
    FRegistedClients           : TCoreClassListForObj;
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
    FCipherStyle               : TCipherStyle;
    FHashStyle                 : THashStyle;
    FPrintParams               : THashVariantList;
    FProgressPost              : TNProgressPostWithCadencer;

    FOnPeerClientCreateNotify : TBackcalls;
    FOnPeerClientDestroyNotify: TBackcalls;
  protected
    procedure DoPrint(const v: string); virtual;
  protected
    function GetIdleTimeout: TTimeTickValue; virtual;
    procedure SetIdleTimeout(const Value: TTimeTickValue); virtual;

    procedure DoConnected(Sender: TPeerClient); virtual;
    procedure DoDisconnect(Sender: TPeerClient); virtual;

    function CanExecuteCommand(Sender: TPeerClient; Cmd: string): Boolean; virtual;
    function CanSendCommand(Sender: TPeerClient; Cmd: string): Boolean; virtual;
    function CanRegCommand(Sender: TCommunicationFramework; Cmd: string): Boolean; virtual;

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

    procedure ProgressWaitSendOfClient(Client: TPeerClient); virtual;

    function DeleteRegistedCMD(Cmd: string): Boolean;
    function UnRegisted(Cmd: string): Boolean;

    function RegisterConsole(Cmd: string): TCommandConsoleMode;
    function RegisterStream(Cmd: string): TCommandStreamMode;
    function RegisterDirectStream(Cmd: string): TCommandDirectStreamMode;
    function RegisterDirectConsole(Cmd: string): TCommandDirectConsoleMode;
    function RegisterBigStream(Cmd: string): TCommandBigStreamMode;

    function ExecuteConsole(Sender: TPeerClient; Cmd: string; const InData: string; var OutData: string): Boolean; virtual;
    function ExecuteStream(Sender: TPeerClient; Cmd: string; InData, OutData: TDataFrameEngine): Boolean; virtual;
    function ExecuteDirectStream(Sender: TPeerClient; Cmd: string; InData: TDataFrameEngine): Boolean; virtual;
    function ExecuteDirectConsole(Sender: TPeerClient; Cmd: string; const InData: string): Boolean; virtual;
    function ExecuteBigStream(Sender: TPeerClient; Cmd: string; InData: TCoreClassStream; FBigStreamTotal, BigStreamCompleteSize: Int64): Boolean; virtual;

    property OnConnected: TPeerClientNotify read FOnConnected write FOnConnected;
    property OnDisconnect: TPeerClientNotify read FOnDisconnect write FOnDisconnect;
    property OnExecuteCommand: TPeerClientCMDNotify read FOnExecuteCommand write FOnExecuteCommand;
    property OnSendCommand: TPeerClientCMDNotify read FOnSendCommand write FOnSendCommand;

    property OnPeerClientCreateNotify: TBackcalls read FOnPeerClientCreateNotify;
    property OnPeerClientDestroyNotify: TBackcalls read FOnPeerClientDestroyNotify;

    property UsedParallelEncrypt: Boolean read FUsedParallelEncrypt write FUsedParallelEncrypt;
    property SyncOnResult: Boolean read FSyncOnResult write FSyncOnResult;
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
    procedure DoPrint(const v: string); override;
    function GetItems(index: Integer): TPeerClient;
    function CanExecuteCommand(Sender: TPeerClient; Cmd: string): Boolean; override;
    function CanSendCommand(Sender: TPeerClient; Cmd: string): Boolean; override;
    function CanRegCommand(Sender: TCommunicationFramework; Cmd: string): Boolean; override;

    procedure Command_ConnectedInit(Sender: TPeerClient; InData, OutData: TDataFrameEngine); virtual;
    procedure Command_Wait(Sender: TPeerClient; InData: string; var OutData: string); virtual;
  public
    constructor Create;
    destructor Destroy; override;

    procedure TriggerQueueData(v: PQueueData); virtual; abstract;

    // send cmd method
    procedure SendConsoleCmd(Client: TPeerClient; Cmd: string; ConsoleData: string; OnResult: TConsoleMethod); overload;
    procedure SendStreamCmd(Client: TPeerClient; Cmd: string; StreamData: TCoreClassStream; OnResult: TStreamMethod; DoneFreeStream: Boolean); overload;
    procedure SendStreamCmd(Client: TPeerClient; Cmd: string; StreamData: TDataFrameEngine; OnResult: TStreamMethod); overload;
    procedure SendStreamCmd(Client: TPeerClient; Cmd: string; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamMethod); overload;

    // send cmd proc
    {$IFNDEF FPC}
    procedure SendConsoleCmd(Client: TPeerClient; Cmd: string; ConsoleData: string; OnResult: TConsoleProc); overload;
    procedure SendStreamCmd(Client: TPeerClient; Cmd: string; StreamData: TCoreClassStream; OnResult: TStreamProc; DoneFreeStream: Boolean); overload;
    procedure SendStreamCmd(Client: TPeerClient; Cmd: string; StreamData: TDataFrameEngine; OnResult: TStreamProc); overload;
    procedure SendStreamCmd(Client: TPeerClient; Cmd: string; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamProc); overload;
    {$ENDIF}
    // send direct cmd
    procedure SendDirectConsoleCmd(Client: TPeerClient; Cmd: string; ConsoleData: string); overload;
    procedure SendDirectStreamCmd(Client: TPeerClient; Cmd: string; StreamData: TCoreClassStream; DoneFreeStream: Boolean); overload;
    procedure SendDirectStreamCmd(Client: TPeerClient; Cmd: string; StreamData: TDataFrameEngine); overload;
    procedure SendDirectStreamCmd(Client: TPeerClient; Cmd: string); overload;

    // wait send
    function WaitSendConsoleCmd(Client: TPeerClient; Cmd: string; ConsoleData: string; TimeOut: TTimeTickValue): string; overload; virtual;
    procedure WaitSendStreamCmd(Client: TPeerClient; Cmd: string; StreamData, ResultData: TDataFrameEngine; TimeOut: TTimeTickValue); overload; virtual;

    // send bitstream
    procedure SendBigStream(Client: TPeerClient; Cmd: string; BigStream: TCoreClassStream; DoneFreeStream: Boolean); overload;

    // send used client ID,return method
    procedure SendConsoleCmd(ClientID: Cardinal; Cmd: string; ConsoleData: string; OnResult: TConsoleMethod); overload;
    procedure SendStreamCmd(ClientID: Cardinal; Cmd: string; StreamData: TCoreClassStream; OnResult: TStreamMethod; DoneFreeStream: Boolean); overload;
    procedure SendStreamCmd(ClientID: Cardinal; Cmd: string; StreamData: TDataFrameEngine; OnResult: TStreamMethod); overload;
    procedure SendStreamCmd(ClientID: Cardinal; Cmd: string; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamMethod); overload;

    // send used client ID,return proc
    {$IFNDEF FPC}
    procedure SendConsoleCmd(ClientID: Cardinal; Cmd: string; ConsoleData: string; OnResult: TConsoleProc); overload;
    procedure SendStreamCmd(ClientID: Cardinal; Cmd: string; StreamData: TCoreClassStream; OnResult: TStreamProc; DoneFreeStream: Boolean); overload;
    procedure SendStreamCmd(ClientID: Cardinal; Cmd: string; StreamData: TDataFrameEngine; OnResult: TStreamProc); overload;
    procedure SendStreamCmd(ClientID: Cardinal; Cmd: string; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamProc); overload;
    {$ENDIF}
    // direct send used client ID
    procedure SendDirectConsoleCmd(ClientID: Cardinal; Cmd: string; ConsoleData: string); overload;
    procedure SendDirectStreamCmd(ClientID: Cardinal; Cmd: string; StreamData: TCoreClassStream; DoneFreeStream: Boolean); overload;
    procedure SendDirectStreamCmd(ClientID: Cardinal; Cmd: string; StreamData: TDataFrameEngine); overload;
    procedure SendDirectStreamCmd(ClientID: Cardinal; Cmd: string); overload;

    // wait send
    function WaitSendConsoleCmd(ClientID: Cardinal; Cmd: string; ConsoleData: string; TimeOut: TTimeTickValue): string; overload;
    procedure WaitSendStreamCmd(ClientID: Cardinal; Cmd: string; StreamData, ResultData: TDataFrameEngine; TimeOut: TTimeTickValue); overload;

    // send bitstream
    procedure SendBigStream(ClientID: Cardinal; Cmd: string; BigStream: TCoreClassStream; DoneFreeStream: Boolean); overload;

    // Broadcast to all client
    procedure BroadcastDirectConsoleCmd(Cmd: string; ConsoleData: string);
    procedure BroadcastSendDirectStreamCmd(Cmd: string; StreamData: TDataFrameEngine);

    function Count: Integer;
    function Exists(Client: TCoreClassObject): Boolean; overload;
    function Exists(Client: TPeerClient): Boolean; overload;
    property Items[index: Integer]: TPeerClient read GetItems; default;

    function Exists(ClientID: Cardinal): Boolean; overload;
    function GetClientFromID(ID: Cardinal): TPeerClient;
    property ClientFromID[ID: Cardinal]: TPeerClient read GetClientFromID;
  end;

  TCommunicationFrameworkClient = class;

  ICommunicationFrameworkClientInterface = interface
    procedure ClientConnected(Sender: TCommunicationFrameworkClient);
    procedure ClientDisconnect(Sender: TCommunicationFrameworkClient);
  end;

  TCommunicationFrameworkClient = class(TCommunicationFramework)
  protected
    FNotyifyInterface: ICommunicationFrameworkClientInterface;

    procedure DoPrint(const v: string); override;

    procedure StreamResult_ConnectedInit(Sender: TPeerClient; ResultData: TDataFrameEngine); virtual;
    procedure DoConnected(Sender: TPeerClient); override;

    procedure DoDisconnect(Sender: TPeerClient); override;
    function CanExecuteCommand(Sender: TPeerClient; Cmd: string): Boolean; override;
    function CanSendCommand(Sender: TPeerClient; Cmd: string): Boolean; override;
    function CanRegCommand(Sender: TCommunicationFramework; Cmd: string): Boolean; override;
  public
    constructor Create;

    procedure TriggerDoDisconnect;

    function Connected: Boolean; virtual; abstract;
    function ClientIO: TPeerClient; virtual; abstract;
    procedure TriggerQueueData(v: PQueueData); virtual; abstract;

    function Wait(ATimeOut: Cardinal): string; virtual;

    // send cmd method
    procedure SendConsoleCmd(Cmd: string; ConsoleData: string; OnResult: TConsoleMethod); overload;
    procedure SendStreamCmd(Cmd: string; StreamData: TCoreClassStream; OnResult: TStreamMethod; DoneFreeStream: Boolean); overload;
    procedure SendStreamCmd(Cmd: string; StreamData: TDataFrameEngine; OnResult: TStreamMethod); overload;
    procedure SendStreamCmd(Cmd: string; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamMethod); overload;

    // send cmd proc
    {$IFNDEF FPC}
    procedure SendConsoleCmd(Cmd: string; ConsoleData: string; OnResult: TConsoleProc); overload;
    procedure SendStreamCmd(Cmd: string; StreamData: TCoreClassStream; OnResult: TStreamProc; DoneFreeStream: Boolean); overload;
    procedure SendStreamCmd(Cmd: string; StreamData: TDataFrameEngine; OnResult: TStreamProc); overload;
    procedure SendStreamCmd(Cmd: string; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamProc); overload;
    {$ENDIF}
    // send direct cmd
    procedure SendDirectConsoleCmd(Cmd: string; ConsoleData: string);
    procedure SendDirectStreamCmd(Cmd: string; StreamData: TCoreClassStream; DoneFreeStream: Boolean); overload;
    procedure SendDirectStreamCmd(Cmd: string; StreamData: TDataFrameEngine); overload;
    procedure SendDirectStreamCmd(Cmd: string); overload;

    // wait send
    function WaitSendConsoleCmd(Cmd: string; ConsoleData: string; TimeOut: TTimeTickValue): string; virtual;
    procedure WaitSendStreamCmd(Cmd: string; StreamData, ResultData: TDataFrameEngine; TimeOut: TTimeTickValue); virtual;

    // send bitstream
    procedure SendBigStream(Cmd: string; BigStream: TCoreClassStream; DoneFreeStream: Boolean);

    property NotyifyInterface: ICommunicationFrameworkClientInterface read FNotyifyInterface write FNotyifyInterface;
    // remote service ID
    // success ID > 0
    // failed! ID = 0
    function RemoteID: Cardinal;
    function RemoteKey: TCipherKeyBuffer;
    function RemoteInited: Boolean;
  end;

  TProgressBackgroundProc = procedure();

const
  c_DataHeadFlag = $F0;

  // cdt=communication data type
  cdtConsole       = 11;
  cdtStream        = 22;
  cdtDirectConsole = 37;
  cdtDirectStream  = 46;
  cdtBigStream     = 75;

  c_DefaultPrintID = $FFFFFFFF;

var
  ProgressBackgroundProc: TProgressBackgroundProc = nil;

procedure DisposeQueueData(v: PQueueData); inline;
procedure InitQueueData(var v: TQueueData); inline;
function NewQueueData: PQueueData; inline;

function TranslateBindAddr(const Addr: string): string; inline;

procedure SyncMethod(t: TCoreClassThread; Sync: Boolean; proc: TThreadMethod); inline;
procedure DoExecuteResult(c: TPeerClient; QueuePtr: PQueueData; AResultText: string; AResultDF: TDataFrameEngine); inline;

{$IFNDEF FPC}
function WaitSendConsoleCmdInThread(th: TCoreClassThread; cf: TCommunicationFrameworkClient; Cmd: string; ConsoleData: string; TimeOut: TTimeTickValue): string;
procedure WaitSendStreamCmdInThread(th: TCoreClassThread; cf: TCommunicationFrameworkClient; Cmd: string; StreamData, ResultData: TDataFrameEngine; TimeOut: TTimeTickValue);
{$ENDIF}

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

function TranslateBindAddr(const Addr: string): string;
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

procedure DoExecuteResult(c: TPeerClient; QueuePtr: PQueueData; AResultText: string; AResultDF: TDataFrameEngine);
var
  AInData: TDataFrameEngine;
begin
  if QueuePtr = nil then
      exit;

  c.FReceiveResultRuning := True;

  try
    if Assigned(QueuePtr^.OnConsoleMethod) then
      begin
        c.PrintParam('execute console on result cmd: %s', QueuePtr^.Cmd);
        try
            QueuePtr^.OnConsoleMethod(c, AResultText);
        except
        end;
      end;
    {$IFNDEF FPC}
    if Assigned(QueuePtr^.OnConsoleProc) then
      begin
        c.PrintParam('execute console on proc cmd: %s', QueuePtr^.Cmd);
        try
            QueuePtr^.OnConsoleProc(c, AResultText);
        except
        end;
      end;
    {$ENDIF}
    if Assigned(QueuePtr^.OnStreamMethod) then
      begin
        c.PrintParam('execute stream on result cmd: %s', QueuePtr^.Cmd);
        try
          AResultDF.Reader.index := 0;
          QueuePtr^.OnStreamMethod(c, AResultDF);
        except
        end;
      end;
    if Assigned(QueuePtr^.OnStreamParamMethod) then
      begin
        c.PrintParam('execute stream on param result cmd: %s', QueuePtr^.Cmd);
        try
          AResultDF.Reader.index := 0;
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
        c.PrintParam('execute stream on proc cmd: %s', QueuePtr^.Cmd);
        try
          AResultDF.Reader.index := 0;
          QueuePtr^.OnStreamProc(c, AResultDF);
        except
        end;
      end;
    if Assigned(QueuePtr^.OnStreamParamProc) then
      begin
        c.PrintParam('execute stream on param proc cmd: %s', QueuePtr^.Cmd);
        try
          AResultDF.Reader.index := 0;
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
    NewResult: string;
    Done     : Boolean;
    constructor Create;
    procedure WaitSendConsoleResultEvent(Client: TPeerClient; ResultData: string);
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

procedure TWaitSendConsoleCmdIntf.WaitSendConsoleResultEvent(Client: TPeerClient; ResultData: string);
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


function WaitSendConsoleCmdInThread(th: TCoreClassThread; cf: TCommunicationFrameworkClient; Cmd: string; ConsoleData: string; TimeOut: TTimeTickValue): string;
var
  waitIntf: TWaitSendConsoleCmdIntf;
  timetick: TTimeTickValue;
  r       : Boolean;
begin
  Result := '';
  if cf.ClientIO = nil then
      exit;
  if not cf.Connected then
      exit;

  r := True;
  TCoreClassThread.Synchronize(th,
    procedure
    begin
      r := cf.CanSendCommand(cf.ClientIO, Cmd);
    end);
  if not r then
      exit;

  TCoreClassThread.Synchronize(th,
    procedure
    begin
      cf.ClientIO.PrintParam('Begin Wait console cmd: %s', Cmd);
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
          exit;
      if (TimeOut > 0) and (GetTimeTickCount > timetick) then
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
        cf.ClientIO.PrintParam('End Wait console cmd: %s', Cmd);
      end);
  except
      Result := '';
  end;
end;

procedure WaitSendStreamCmdInThread(th: TCoreClassThread; cf: TCommunicationFrameworkClient; Cmd: string; StreamData, ResultData: TDataFrameEngine; TimeOut: TTimeTickValue);
var
  waitIntf: TWaitSendStreamCmdIntf;
  timetick: TTimeTickValue;
  r       : Boolean;
begin
  if cf.ClientIO = nil then
      exit;
  if not cf.Connected then
      exit;

  r := True;
  TCoreClassThread.Synchronize(th,
    procedure
    begin
      r := cf.CanSendCommand(cf.ClientIO, Cmd);
    end);
  if not r then
      exit;

  TCoreClassThread.Synchronize(th,
    procedure
    begin
      cf.ClientIO.PrintParam('Begin Wait Stream cmd: %s', Cmd);
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
              exit;
          if (TimeOut > 0) and (GetTimeTickCount > timetick) then
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
        cf.ClientIO.PrintParam('End Wait Stream cmd: %s', Cmd);
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

function TCommandConsoleMode.Execute(Sender: TPeerClient; InData: string; var OutData: string): Boolean;
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

function TCommandDirectConsoleMode.Execute(Sender: TPeerClient; InData: string): Boolean;
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

constructor TPeerClientUserDefine.Create(AOwner: TPeerClient);
begin
  inherited Create;
  FOwner := AOwner;
  FWorkPlatform := TExecutePlatform.epUnknow;
end;

constructor TPeerClientUserSpecial.Create(AOwner: TPeerClient);
begin
  inherited Create;
  FOwner := AOwner;
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
  inc(FOwnerFramework.Statistics[TStatisticsType.stReceiveSize], Count);
end;

procedure TPeerClient.InternalSendByteBuffer(buff: PByte; Size: Integer);
const
  FlushBuffSize = 16 * 1024; // flush size = 16k byte
begin
  FLastCommunicationTimeTickCount := GetTimeTickCount;

  if Size < 1 then
      exit;

  // fill fragment
  while Size > FlushBuffSize do
    begin
      SendByteBuffer(buff, FlushBuffSize);
      inc(buff, FlushBuffSize);
      inc(FOwnerFramework.Statistics[TStatisticsType.stSendSize], FlushBuffSize);
      WriteBufferFlush;
      dec(Size, FlushBuffSize);
    end;

  if Size > 0 then
    begin
      SendByteBuffer(buff, Size);
      inc(FOwnerFramework.Statistics[TStatisticsType.stSendSize], Size);
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
  SendByte(Byte(FHeadFlag));
  SendByte(Byte(cdtConsole));
  SendCardinal(Cardinal(buff.Size));

  SendVerifyCode(buff.Memory, buff.Size);
  SendEncryptMemoryStream(buff, cs);

  WriteBufferFlush;
  WriteBufferClose;
end;

procedure TPeerClient.InternalSendStreamBuff(buff: TMemoryStream64; cs: TCipherStyle);
begin
  WriteBufferOpen;
  SendByte(Byte(FHeadFlag));
  SendByte(Byte(cdtStream));
  SendCardinal(Cardinal(buff.Size));

  SendVerifyCode(buff.Memory, buff.Size);
  SendEncryptMemoryStream(buff, cs);

  WriteBufferFlush;
  WriteBufferClose;
end;

procedure TPeerClient.InternalSendDirectConsoleBuff(buff: TMemoryStream64; cs: TCipherStyle);
begin
  WriteBufferOpen;
  SendByte(Byte(FHeadFlag));
  SendByte(Byte(cdtDirectConsole));
  SendCardinal(Cardinal(buff.Size));

  SendVerifyCode(buff.Memory, buff.Size);
  SendEncryptMemoryStream(buff, cs);

  WriteBufferFlush;
  WriteBufferClose;
end;

procedure TPeerClient.InternalSendDirectStreamBuff(buff: TMemoryStream64; cs: TCipherStyle);
begin
  WriteBufferOpen;
  SendByte(Byte(FHeadFlag));
  SendByte(Byte(cdtDirectStream));
  SendCardinal(Cardinal(buff.Size));

  SendVerifyCode(buff.Memory, buff.Size);
  SendEncryptMemoryStream(buff, cs);

  WriteBufferFlush;
  WriteBufferClose;
end;

procedure TPeerClient.InternalSendBigStreamHeader(Cmd: string; streamSiz: Int64);
var
  buff: TBytes;
begin
  WriteBufferOpen;

  SendByte(FHeadFlag);
  SendByte(cdtBigStream);
  SendInt64(streamSiz);
  buff := TPascalString(Cmd).Bytes;
  SendCardinal(Cardinal(Length(buff)));
  InternalSendByteBuffer(@buff[0], Length(buff));
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
          exit;

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
          exit;
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
      df.EncodeToCompressed(stream, True)
  else
      df.EncodeTo(stream, True);

  InternalSendConsoleBuff(stream, FSyncPick^.Cipher);

  DisposeObject(df);
  DisposeObject(stream);

  if FOwnerFramework.FSendDataCompressed then
      inc(FOwnerFramework.Statistics[TStatisticsType.stCompress]);
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
      df.EncodeToCompressed(stream, True)
  else
      df.EncodeTo(stream, True);

  InternalSendStreamBuff(stream, FSyncPick^.Cipher);

  DisposeObject(df);
  DisposeObject(stream);

  if FOwnerFramework.FSendDataCompressed then
      inc(FOwnerFramework.Statistics[TStatisticsType.stCompress]);
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
      df.EncodeToCompressed(stream, True)
  else
      df.EncodeTo(stream, True);

  InternalSendDirectConsoleBuff(stream, FSyncPick^.Cipher);

  DisposeObject(df);
  DisposeObject(stream);

  if FOwnerFramework.FSendDataCompressed then
      inc(FOwnerFramework.Statistics[TStatisticsType.stCompress]);
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
      df.EncodeToCompressed(stream, True)
  else
      df.EncodeTo(stream, True);

  InternalSendDirectStreamBuff(stream, FSyncPick^.Cipher);

  DisposeObject(df);
  DisposeObject(stream);

  if FOwnerFramework.FSendDataCompressed then
      inc(FOwnerFramework.Statistics[TStatisticsType.stCompress]);
end;

procedure TPeerClient.Sync_InternalSendBigStreamCmd;
begin
  FSyncPick^.BigStream.Position := 0;
  InternalSendBigStreamBuff(FSyncPick^);
  inc(FOwnerFramework.Statistics[TStatisticsType.stExecBigStream]);
end;

procedure TPeerClient.Sync_ExecuteConsole;
var
  d: TTimeTickValue;
begin
  FReceiveCommandRuning := True;
  PrintParam('execute console cmd:%s', FInCmd);

  d := GetTimeTickCount;
  FOwnerFramework.ExecuteConsole(Self, FInCmd, FInText, FOutText);
  FReceiveCommandRuning := False;

  FOwnerFramework.CmdMaxExecuteConsumeStatistics.SetMax(FInCmd, GetTimeTickCount - d);

  inc(FOwnerFramework.Statistics[TStatisticsType.stExecConsole]);
  FOwnerFramework.CmdRecvStatistics.IncValue(FInCmd, 1);
end;

procedure TPeerClient.Sync_ExecuteStream;
var
  d: TTimeTickValue;
begin
  FReceiveCommandRuning := True;
  PrintParam('execute stream cmd:%s', FInCmd);

  d := GetTimeTickCount;
  FOwnerFramework.ExecuteStream(Self, FInCmd, FInDataFrame, FOutDataFrame);
  FReceiveCommandRuning := False;

  FOwnerFramework.CmdMaxExecuteConsumeStatistics.SetMax(FInCmd, GetTimeTickCount - d);

  inc(FOwnerFramework.Statistics[TStatisticsType.stExecStream]);
  FOwnerFramework.CmdRecvStatistics.IncValue(FInCmd, 1);
end;

procedure TPeerClient.Sync_ExecuteDirectConsole;
var
  d: TTimeTickValue;
begin
  FReceiveCommandRuning := True;
  PrintParam('execute direct console cmd:%s', FInCmd);

  d := GetTimeTickCount;
  FOwnerFramework.ExecuteDirectConsole(Self, FInCmd, FInText);
  FReceiveCommandRuning := False;

  FOwnerFramework.CmdMaxExecuteConsumeStatistics.SetMax(FInCmd, GetTimeTickCount - d);

  inc(FOwnerFramework.Statistics[TStatisticsType.stExecDirestConsole]);
  FOwnerFramework.CmdRecvStatistics.IncValue(FInCmd, 1);
end;

procedure TPeerClient.Sync_ExecuteDirectStream;
var
  d: TTimeTickValue;
begin
  FReceiveCommandRuning := True;
  PrintParam('execute direct stream cmd:%s', FInCmd);

  d := GetTimeTickCount;
  FOwnerFramework.ExecuteDirectStream(Self, FInCmd, FInDataFrame);
  FReceiveCommandRuning := False;

  FOwnerFramework.CmdMaxExecuteConsumeStatistics.SetMax(FInCmd, GetTimeTickCount - d);

  inc(FOwnerFramework.Statistics[TStatisticsType.stExecDirestStream]);
  FOwnerFramework.CmdRecvStatistics.IncValue(FInCmd, 1);
end;

procedure TPeerClient.ExecuteDataFrame(ACurrentActiveThread: TCoreClassThread; const Sync: Boolean; CommDataType: Byte; dataFrame: TDataFrameEngine);
var
  m64 : TMemoryStream64;
  buff: TBytes;
begin
  FInCmd := dataFrame.Reader.ReadString;

  case CommDataType of
    cdtConsole:
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
            exit;
          end;
        if not Connected then
            exit;

        buff := TPascalString(FOutText).Bytes;
        WriteBufferOpen;

        SendByte(Byte(FHeadFlag));
        SendInteger(Length(buff));

        SendVerifyCode(@buff[0], Length(buff));

        SendEncryptBuffer(@buff[0], Length(buff), FReceiveDataCipherStyle);

        WriteBufferFlush;
        WriteBufferClose;

        inc(FOwnerFramework.Statistics[TStatisticsType.stResponse]);
      end;
    cdtStream:
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
            exit;
          end;

        if not Connected then
            exit;

        m64 := TMemoryStream64.Create;
        FOutDataFrame.EncodeTo(m64, True);

        WriteBufferOpen;
        SendByte(Byte(FHeadFlag));
        SendInteger(m64.Size);

        SendVerifyCode(m64.Memory, m64.Size);

        SendEncryptBuffer(m64.Memory, m64.Size, FReceiveDataCipherStyle);
        DisposeObject(m64);

        WriteBufferFlush;
        WriteBufferClose;
        inc(FOwnerFramework.Statistics[TStatisticsType.stResponse]);
      end;
    cdtDirectConsole:
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
            exit;
      end;
    cdtDirectStream:
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
            exit;
      end;
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
      PrintParam('execute Big Stream cmd:%s', FBigStreamCmd);

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
      exit;

  if FOwnerFramework.FSyncOnResult then
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
  dID         : Byte;
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
      exit;
  if FCurrentQueueData = nil then
      exit;

  FReceivedBuffer.Position := 0;

  if (FReceivedBuffer.Size - FReceivedBuffer.Position < umlByteLength) then
      exit;

  // 0: data type
  FReceivedBuffer.Read(dID, umlByteLength);

  if dID <> FHeadFlag then
    begin
      Disconnect;
      exit;
    end;

  // 1: data len
  if (FReceivedBuffer.Size - FReceivedBuffer.Position < umlIntegerLength) then
      exit;
  FReceivedBuffer.Read(dSize, umlIntegerLength);

  // 2:verify code header
  if (FReceivedBuffer.Size - FReceivedBuffer.Position < 3) then
      exit;
  FReceivedBuffer.Read(dHashStyle, umlByteLength);
  FReceivedBuffer.Read(dHashSiz, umlWordLength);

  // 3:verify code body
  if (FReceivedBuffer.Size - FReceivedBuffer.Position < dHashSiz) then
      exit;
  SetLength(dHash, dHashSiz);
  FReceivedBuffer.Read(dHash[0], dHashSiz);

  // 4: use Encrypt state
  if (FReceivedBuffer.Size - FReceivedBuffer.Position < umlByteLength) then
      exit;
  FReceivedBuffer.Read(dCipherStyle, umlByteLength);

  // 5:process bytes
  if (FReceivedBuffer.Size - FReceivedBuffer.Position < dSize) then
      exit;
  SetLength(buff, dSize);
  FReceivedBuffer.Read(buff[0], dSize);

  FReceiveDataCipherStyle := TCipherStyle(dCipherStyle);

  try
      Encrypt(FReceiveDataCipherStyle, @buff[0], dSize, FCipherKey, False);
  except
    Print('Encrypt error!');
    Disconnect;
    exit;
  end;

  if not VerifyHashCode(dHashStyle, @buff[0], dSize, dHash) then
    begin
      Print('verify data error!');
      Disconnect;
      exit;
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
        exit;
      end;

      {$IFDEF FPC}
      SyncMethod(ACurrentActiveThread, RecvSync, @Sync_ExecuteResult);
      {$ELSE}
      SyncMethod(ACurrentActiveThread, RecvSync, Sync_ExecuteResult);
      {$ENDIF}
      ResultText := '';

      inc(FOwnerFramework.Statistics[TStatisticsType.stResponse]);
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
          exit;
        end;

        {$IFDEF FPC}
        SyncMethod(ACurrentActiveThread, RecvSync, @Sync_ExecuteResult);
        {$ELSE}
        SyncMethod(ACurrentActiveThread, RecvSync, Sync_ExecuteResult);
        {$ENDIF}
        ResultDataFrame.Clear;

        inc(FOwnerFramework.Statistics[TStatisticsType.stResponse]);
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
  ProcessAllSendCmd(ACurrentActiveThread, RecvSync, SendSync);
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
  inc(AOwnerFramework.FIDCounter);
  if AOwnerFramework.FIDCounter = 0 then
      inc(AOwnerFramework.FIDCounter);

  FHeadFlag := c_DataHeadFlag;

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
  FPrintStatus := True;

  FAllSendProcessing := False;

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

  LockObject(FOwnerFramework.FRegistedClients);
  FOwnerFramework.FRegistedClients.Add(Self);
  UnLockObject(FOwnerFramework.FRegistedClients);

  inc(FOwnerFramework.Statistics[TStatisticsType.stConnest]);

  FOwnerFramework.FOnPeerClientCreateNotify.ExecuteBackcall(Self, NULL, NULL, NULL);
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

  FOwnerFramework.FOnPeerClientDestroyNotify.ExecuteBackcall(Self, NULL, NULL, NULL);

  inc(FOwnerFramework.Statistics[TStatisticsType.stDisconnest]);

  LockObject(FOwnerFramework.FRegistedClients);
  try
    i := 0;
    while i < FOwnerFramework.FRegistedClients.Count do
      begin
        if FOwnerFramework.FRegistedClients[i] = Self then
            FOwnerFramework.FRegistedClients.Delete(i)
        else
            inc(i);
      end;
  except
  end;
  UnLockObject(FOwnerFramework.FRegistedClients);

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

procedure TPeerClient.Print(v: string);
var
  n: string;
begin
  n := GetPeerIP;
  if n <> '' then
      OwnerFramework.DoPrint(Format('%s %s %s', [n, DateTimeToStr(now), v]))
  else
      OwnerFramework.DoPrint(Format('%s %s', [DateTimeToStr(now), v]));
end;

procedure TPeerClient.Print(v: string; const Args: array of const);
begin
  Print(Format(v, Args));
end;

procedure TPeerClient.PrintParam(v: string; Args: string);
begin
  try
    if OwnerFramework.FPrintParams.GetDefaultValue(Args, True) = True then
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

        inc(FBigStreamSendState, SendBufferSize);

      except
        UnLockObject(FBigStreamSending);
        Disconnect;
        exit;
      end;

      UnLockObject(FBigStreamSending);

      try
        WriteBufferOpen;
        InternalSendByteBuffer(@buff[0], SendBufferSize);
        WriteBufferFlush;
        WriteBufferClose;
      except
        Disconnect;
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
          FillRecvBuffer(nil, False, False);
        end;
    end;
end;

procedure TPeerClient.FillRecvBuffer(ACurrentActiveThread: TCoreClassThread; const RecvSync, SendSync: Boolean);
var
  dFlag, dID  : Byte;
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
      exit;
  if FPauseResultSend then
      exit;
  if FResultDataBuffer.Size > 0 then
      exit;
  if FRunReceiveTrigger then
      exit;
  if FBigStreamSending <> nil then
      exit;

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

      // 0: flag
      if (FReceivedBuffer.Size - FReceivedBuffer.Position < umlByteLength) then
          break;
      FReceivedBuffer.Read(dFlag, umlByteLength);
      if dFlag <> FHeadFlag then
        begin
          Disconnect;
          break;
        end;

      // 1: data type
      if (FReceivedBuffer.Size - FReceivedBuffer.Position < umlByteLength) then
          break;
      FReceivedBuffer.Read(dID, umlByteLength);

      case dID of
        cdtConsole, cdtStream, cdtDirectConsole, cdtDirectStream:
          begin
            // 2: size
            if (FReceivedBuffer.Size - FReceivedBuffer.Position < umlCardinalLength) then
                break;
            FReceivedBuffer.Read(dSize, umlCardinalLength);

            // 3:verify code header
            if (FReceivedBuffer.Size - FReceivedBuffer.Position < 3) then
                exit;
            FReceivedBuffer.Read(dHashStyle, umlByteLength);
            FReceivedBuffer.Read(dHashSiz, umlWordLength);

            // 4:verify code body
            if (FReceivedBuffer.Size - FReceivedBuffer.Position < dHashSiz) then
                exit;
            SetLength(dHash, dHashSiz);
            FReceivedBuffer.Read(dHash[0], dHashSiz);

            // 5: Encrypt style
            if (FReceivedBuffer.Size - FReceivedBuffer.Position < umlByteLength) then
                break;
            FReceivedBuffer.Read(dCipherStyle, umlByteLength);

            // 6: process stream
            if (FReceivedBuffer.Size - FReceivedBuffer.Position < dSize) then
                break;
            tmpStream := TMemoryStream64OfWriteTrigger.Create(nil);
            tmpStream.CopyFrom(FReceivedBuffer, dSize);

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

            inc(FOwnerFramework.Statistics[TStatisticsType.stRequest]);
          end;
        cdtBigStream:
          begin
            // 2:stream size
            if (FReceivedBuffer.Size - FReceivedBuffer.Position < umlInt64Length) then
                break;
            FReceivedBuffer.Read(Total, umlInt64Length);

            // 3:command len
            if (FReceivedBuffer.Size - FReceivedBuffer.Position < umlCardinalLength) then
                break;
            FReceivedBuffer.Read(dSize, umlCardinalLength);

            // 4:command text
            if (FReceivedBuffer.Size - FReceivedBuffer.Position < dSize) then
                break;
            SetLength(buff, dSize);
            FReceivedBuffer.Read(buff[0], dSize);
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

            inc(FOwnerFramework.Statistics[TStatisticsType.stReceiveBigStream]);
          end;
        else
          begin
            Disconnect;
            break;
          end;
      end;
    end;
  FReceivedBuffer.Position := FReceivedBuffer.Size;
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
      exit;

  if FAllSendProcessing then
      exit;
  if FWaitOnResult then
      exit;
  if FBigStreamReceiveProcessing then
      exit;
  if FBigStreamSending <> nil then
    begin
      Progress;
      exit;
    end;

  if FResultDataBuffer.Size > 0 then
    begin
      {$IFDEF FPC}
      SyncMethod(ACurrentActiveThread, SendSync, @Sync_InternalSendResultData);
      {$ELSE}
      SyncMethod(ACurrentActiveThread, SendSync, Sync_InternalSendResultData);
      {$ENDIF}
    end;

  if FRunReceiveTrigger then
      exit;

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
              inc(FOwnerFramework.Statistics[TStatisticsType.stConsole]);

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
              inc(Result);
              break;
            end;
          qsSendStreamCMD:
            begin
              inc(FOwnerFramework.Statistics[TStatisticsType.stStream]);

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
              inc(Result);
              break;
            end;
          qsSendDirectConsoleCMD:
            begin
              inc(FOwnerFramework.Statistics[TStatisticsType.stDirestConsole]);

              FSyncPick := p;
              {$IFDEF FPC}
              SyncMethod(ACurrentActiveThread, SendSync, @Sync_InternalSendDirectConsoleCmd);
              {$ELSE}
              SyncMethod(ACurrentActiveThread, SendSync, Sync_InternalSendDirectConsoleCmd);
              {$ENDIF}
              FSyncPick := nil;

              DisposeQueueData(p);
              FQueueList.Delete(0);
              inc(Result);
            end;
          qsSendDirectStreamCMD:
            begin
              inc(FOwnerFramework.Statistics[TStatisticsType.stDirestStream]);

              FSyncPick := p;
              {$IFDEF FPC}
              SyncMethod(ACurrentActiveThread, SendSync, @Sync_InternalSendDirectStreamCmd);
              {$ELSE}
              SyncMethod(ACurrentActiveThread, SendSync, Sync_InternalSendDirectStreamCmd);
              {$ENDIF}
              FSyncPick := nil;

              DisposeQueueData(p);
              FQueueList.Delete(0);
              inc(Result);
            end;
          qsSendBigStream:
            begin
              inc(FOwnerFramework.Statistics[TStatisticsType.stSendBigStream]);

              FSyncPick := p;
              {$IFDEF FPC}
              SyncMethod(ACurrentActiveThread, SendSync, @Sync_InternalSendBigStreamCmd);
              {$ELSE}
              SyncMethod(ACurrentActiveThread, SendSync, Sync_InternalSendBigStreamCmd);
              {$ENDIF}
              FSyncPick := nil;

              DisposeQueueData(p);
              FQueueList.Delete(0);
              inc(Result);

              if FBigStreamSending <> nil then
                  break;
            end;
        end;
      end;
  finally
    UnLockObject(FQueueList);
    FAllSendProcessing := False;
    FillRecvBuffer(ACurrentActiveThread, RecvSync, SendSync);
  end;
end;

procedure TPeerClient.PauseResultSend;
begin
  if FCanPauseResultSend then
    begin
      FPauseResultSend := True;
      inc(FOwnerFramework.Statistics[TStatisticsType.stPause]);
    end;
end;

procedure TPeerClient.ContinueResultSend;
var
  headBuff    : array [0 .. 2] of Byte;
  b           : TBytes;
  buff        : TMemoryStream64;
  flag        : Byte;
  len         : Integer;
  code        : TBytes;
  bCipherStyle: Byte;
begin
  if not FPauseResultSend then
      exit;
  if FResultDataBuffer.Size > 0 then
      exit;

  inc(FOwnerFramework.Statistics[TStatisticsType.stContinue]);

  case FCurrentPauseResultSend_CommDataType of
    cdtConsole, cdtStream:
      begin
        buff := TMemoryStream64.Create;

        if FCurrentPauseResultSend_CommDataType = cdtConsole then
          begin
            b := TPascalString(FOutText).Bytes;
            buff.WritePtr(@b[0], Length(b));
          end
        else
            FOutDataFrame.EncodeTo(buff, True);

        flag := FHeadFlag;
        len := buff.Size;

        // generate hash source
        GenerateHashCode(FOwnerFramework.FHashStyle, buff.Memory, buff.Size, code);
        headBuff[0] := Byte(FOwnerFramework.FHashStyle);
        PWord(@headBuff[1])^ := Length(code);

        // generate encrypt data body
        bCipherStyle := Byte(FReceiveDataCipherStyle);
        Encrypt(FReceiveDataCipherStyle, buff.Memory, buff.Size, FCipherKey, True);

        // result data header
        FResultDataBuffer.WritePtr(@flag, umlByteLength);
        FResultDataBuffer.WritePtr(@len, umlIntegerLength);

        // verify code
        FResultDataBuffer.WritePtr(@headBuff[0], 3);
        FResultDataBuffer.WritePtr(@code[0], Length(code));

        // data body
        FResultDataBuffer.WritePtr(@bCipherStyle, umlByteLength);
        FResultDataBuffer.WritePtr(buff.Memory, len);

        DisposeObject(buff);

        inc(FOwnerFramework.Statistics[TStatisticsType.stResponse]);
      end;
  end;
  FPauseResultSend := False;
end;

function TPeerClient.ResultSendIsPaused: Boolean;
begin
  Result := FPauseResultSend;
end;

function TPeerClient.CipherKeyPtr: PCipherKeyBuffer;
begin
  Result := @FCipherKey;
end;

procedure TPeerClient.GenerateHashCode(hs: THashStyle; buff: Pointer; siz: Integer; var output: TBytes);
begin
  TCipher.GenerateHashByte(hs, buff, siz, output);
  inc(FOwnerFramework.Statistics[TStatisticsType.stGenerateHash]);
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
      inc(FOwnerFramework.Statistics[TStatisticsType.stEncrypt]);
end;

function TPeerClient.StopCommunicationTime: TTimeTickValue;
begin
  Result := GetTimeTickCount - FLastCommunicationTimeTickCount;
end;

procedure TPeerClient.SetLastCommunicationTimeAsCurrent;
begin
  FLastCommunicationTimeTickCount := GetTimeTickCount;
end;

procedure TPeerClient.SendConsoleCmd(Cmd: string; ConsoleData: string; OnResult: TConsoleMethod);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendConsoleCmd(Self, Cmd, ConsoleData, OnResult)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendConsoleCmd(Cmd, ConsoleData, OnResult);
end;

procedure TPeerClient.SendStreamCmd(Cmd: string; StreamData: TCoreClassStream; OnResult: TStreamMethod; DoneFreeStream: Boolean);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendStreamCmd(Self, Cmd, StreamData, OnResult, DoneFreeStream)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendStreamCmd(Cmd, StreamData, OnResult, DoneFreeStream);
end;

procedure TPeerClient.SendStreamCmd(Cmd: string; StreamData: TDataFrameEngine; OnResult: TStreamMethod);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendStreamCmd(Self, Cmd, StreamData, OnResult)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendStreamCmd(Cmd, StreamData, OnResult);
end;

procedure TPeerClient.SendStreamCmd(Cmd: string; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamMethod);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendStreamCmd(Self, Cmd, StreamData, Param1, Param2, OnResult)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendStreamCmd(Cmd, StreamData, Param1, Param2, OnResult);
end;

{$IFNDEF FPC}


procedure TPeerClient.SendConsoleCmd(Cmd: string; ConsoleData: string; OnResult: TConsoleProc);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendConsoleCmd(Self, Cmd, ConsoleData, OnResult)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendConsoleCmd(Cmd, ConsoleData, OnResult);
end;

procedure TPeerClient.SendStreamCmd(Cmd: string; StreamData: TCoreClassStream; OnResult: TStreamProc; DoneFreeStream: Boolean);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendStreamCmd(Self, Cmd, StreamData, OnResult, DoneFreeStream)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendStreamCmd(Cmd, StreamData, OnResult, DoneFreeStream);
end;

procedure TPeerClient.SendStreamCmd(Cmd: string; StreamData: TDataFrameEngine; OnResult: TStreamProc);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendStreamCmd(Self, Cmd, StreamData, OnResult)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendStreamCmd(Cmd, StreamData, OnResult);
end;

procedure TPeerClient.SendStreamCmd(Cmd: string; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamProc);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendStreamCmd(Self, Cmd, StreamData, Param1, Param2, OnResult)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendStreamCmd(Cmd, StreamData, Param1, Param2, OnResult);
end;
{$ENDIF}


procedure TPeerClient.SendDirectConsoleCmd(Cmd: string; ConsoleData: string);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendDirectConsoleCmd(Self, Cmd, ConsoleData)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendDirectConsoleCmd(Cmd, ConsoleData);
end;

procedure TPeerClient.SendDirectStreamCmd(Cmd: string; StreamData: TCoreClassStream; DoneFreeStream: Boolean);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendDirectStreamCmd(Self, Cmd, StreamData, DoneFreeStream)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendDirectStreamCmd(Cmd, StreamData, DoneFreeStream);
end;

procedure TPeerClient.SendDirectStreamCmd(Cmd: string; StreamData: TDataFrameEngine);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendDirectStreamCmd(Self, Cmd, StreamData)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendDirectStreamCmd(Cmd, StreamData);
end;

procedure TPeerClient.SendDirectStreamCmd(Cmd: string);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendDirectStreamCmd(Self, Cmd)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendDirectStreamCmd(Cmd);
end;

function TPeerClient.WaitSendConsoleCmd(Cmd: string; ConsoleData: string; TimeOut: TTimeTickValue): string;
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      Result := TCommunicationFrameworkServer(FOwnerFramework).WaitSendConsoleCmd(Self, Cmd, ConsoleData, TimeOut)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      Result := TCommunicationFrameworkClient(FOwnerFramework).WaitSendConsoleCmd(Cmd, ConsoleData, TimeOut)
  else
      Result := '';
end;

procedure TPeerClient.WaitSendStreamCmd(Cmd: string; StreamData, ResultData: TDataFrameEngine; TimeOut: TTimeTickValue);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).WaitSendStreamCmd(Self, Cmd, StreamData, ResultData, TimeOut)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).WaitSendStreamCmd(Cmd, StreamData, ResultData, TimeOut);
end;

procedure TPeerClient.SendBigStream(Cmd: string; BigStream: TCoreClassStream; DoneFreeStream: Boolean);
begin
  if FOwnerFramework.InheritsFrom(TCommunicationFrameworkServer) then
      TCommunicationFrameworkServer(FOwnerFramework).SendBigStream(Self, Cmd, BigStream, DoneFreeStream)
  else if FOwnerFramework.InheritsFrom(TCommunicationFrameworkClient) then
      TCommunicationFrameworkClient(FOwnerFramework).SendBigStream(Cmd, BigStream, DoneFreeStream);
end;

procedure TCommunicationFramework.DoPrint(const v: string);
begin
  DoStatus(v, c_DefaultPrintID);
  inc(Statistics[TStatisticsType.stPrint]);
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

function TCommunicationFramework.CanExecuteCommand(Sender: TPeerClient; Cmd: string): Boolean;
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
      inc(Statistics[TStatisticsType.stTotalCommandExecute]);
end;

function TCommunicationFramework.CanSendCommand(Sender: TPeerClient; Cmd: string): Boolean;
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
      inc(Statistics[TStatisticsType.stTotalCommandSend]);
end;

function TCommunicationFramework.CanRegCommand(Sender: TCommunicationFramework; Cmd: string): Boolean;
begin
  Result := True;
  inc(Statistics[TStatisticsType.stTotalCommandReg]);
end;

procedure TCommunicationFramework.DelayExecuteOnResultState(Sender: TNPostExecute);
var
  cli   : TPeerClient;
  nQueue: PQueueData;
  i     : Integer;

  ExistsCli: Boolean;
begin
  cli := TPeerClient(Sender.Data1);
  nQueue := PQueueData(Sender.Data5);

  ExistsCli := False;
  for i := 0 to FRegistedClients.Count - 1 do
    if FRegistedClients[i] = cli then
        ExistsCli := True;

  if ExistsCli then
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
  FRegistedClients := TCoreClassListForObj.Create;
  FOnConnected := nil;
  FOnDisconnect := nil;
  FOnExecuteCommand := nil;
  FOnSendCommand := nil;
  FIdleTimeout := 0;
  FUsedParallelEncrypt := True;
  FSyncOnResult := False;
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
  DisposeObject(FRegistedClients);
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
  LockObject(FRegistedClients);
  inc(Statistics[TStatisticsType.stLock]);
end;

procedure TCommunicationFramework.UnLockClients;
begin
  UnLockObject(FRegistedClients);
  inc(Statistics[TStatisticsType.stUnLock]);
end;

procedure TCommunicationFramework.ProgressBackground;
var
  i: Integer;
begin
  try
    if Assigned(ProgressBackgroundProc) then
        ProgressBackgroundProc;
  except
  end;

  i := 0;
  try
    while i < FRegistedClients.Count do
      begin
        TPeerClient(FRegistedClients[i]).Progress;
        inc(i);
      end;
  except
  end;

  try
      ProgressPost.Progress;
  except
  end;

  Statistics[TStatisticsType.stIDCounter] := FIDCounter;
end;

procedure TCommunicationFramework.ProgressWaitSendOfClient(Client: TPeerClient);
begin
  ProgressBackground;
end;

function TCommunicationFramework.DeleteRegistedCMD(Cmd: string): Boolean;
begin
  Result := FCommandList.Exists(Cmd);
  FCommandList.Delete(Cmd);
end;

function TCommunicationFramework.UnRegisted(Cmd: string): Boolean;
begin
  Result := FCommandList.Exists(Cmd);
  FCommandList.Delete(Cmd);
end;

function TCommunicationFramework.RegisterConsole(Cmd: string): TCommandConsoleMode;
begin
  if not CanRegCommand(Self, Cmd) then
    begin
      raiseInfo(Format('Illegal Register', []));
      Result := nil;
      exit;
    end;

  if FCommandList.Exists(Cmd) then
    begin
      raiseInfo(Format('exists cmd:%s', [Cmd]));
      Result := nil;
      exit;
    end;

  Result := TCommandConsoleMode.Create;
  FCommandList[Cmd] := Result;

  CmdRecvStatistics.IncValue(Cmd, 0);
  CmdMaxExecuteConsumeStatistics[Cmd] := 0;
end;

function TCommunicationFramework.RegisterStream(Cmd: string): TCommandStreamMode;
begin
  if not CanRegCommand(Self, Cmd) then
    begin
      raiseInfo(Format('Illegal Register', []));
      Result := nil;
      exit;
    end;

  if FCommandList.Exists(Cmd) then
    begin
      raiseInfo(Format('exists cmd:%s', [Cmd]));
      Result := nil;
      exit;
    end;

  Result := TCommandStreamMode.Create;
  FCommandList[Cmd] := Result;

  CmdRecvStatistics.IncValue(Cmd, 0);
  CmdMaxExecuteConsumeStatistics[Cmd] := 0;
end;

function TCommunicationFramework.RegisterDirectStream(Cmd: string): TCommandDirectStreamMode;
begin
  if not CanRegCommand(Self, Cmd) then
    begin
      raiseInfo(Format('Illegal Register', []));
      Result := nil;
      exit;
    end;

  if FCommandList.Exists(Cmd) then
    begin
      raiseInfo(Format('exists cmd:%s', [Cmd]));
      Result := nil;
      exit;
    end;

  Result := TCommandDirectStreamMode.Create;
  FCommandList[Cmd] := Result;

  CmdRecvStatistics.IncValue(Cmd, 0);
  CmdMaxExecuteConsumeStatistics[Cmd] := 0;
end;

function TCommunicationFramework.RegisterDirectConsole(Cmd: string): TCommandDirectConsoleMode;
begin
  if not CanRegCommand(Self, Cmd) then
    begin
      raiseInfo(Format('Illegal Register', []));
      Result := nil;
      exit;
    end;

  if FCommandList.Exists(Cmd) then
    begin
      raiseInfo(Format('exists cmd:%s', [Cmd]));
      Result := nil;
      exit;
    end;

  Result := TCommandDirectConsoleMode.Create;
  FCommandList[Cmd] := Result;

  CmdRecvStatistics.IncValue(Cmd, 0);
  CmdMaxExecuteConsumeStatistics[Cmd] := 0;
end;

function TCommunicationFramework.RegisterBigStream(Cmd: string): TCommandBigStreamMode;
begin
  if not CanRegCommand(Self, Cmd) then
    begin
      raiseInfo(Format('Illegal Register', []));
      Result := nil;
      exit;
    end;

  if FCommandList.Exists(Cmd) then
    begin
      raiseInfo(Format('exists cmd:%s', [Cmd]));
      Result := nil;
      exit;
    end;

  Result := TCommandBigStreamMode.Create;
  FCommandList[Cmd] := Result;

  CmdRecvStatistics.IncValue(Cmd, 0);
  CmdMaxExecuteConsumeStatistics[Cmd] := 0;
end;

function TCommunicationFramework.ExecuteConsole(Sender: TPeerClient; Cmd: string; const InData: string; var OutData: string): Boolean;
var
  b: TCoreClassObject;
begin
  Result := False;
  if not CanExecuteCommand(Sender, Cmd) then
      exit;
  b := FCommandList[Cmd];
  if b = nil then
    begin
      Sender.PrintParam('no exists console cmd:%s', Cmd);
      exit;
    end;
  if not b.InheritsFrom(TCommandConsoleMode) then
    begin
      Sender.PrintParam('Illegal interface in cmd:%s', Cmd);
      exit;
    end;
  Result := TCommandConsoleMode(b).Execute(Sender, InData, OutData);
end;

function TCommunicationFramework.ExecuteStream(Sender: TPeerClient; Cmd: string; InData, OutData: TDataFrameEngine): Boolean;
var
  b: TCoreClassObject;
begin
  Result := False;
  if not CanExecuteCommand(Sender, Cmd) then
      exit;
  b := FCommandList[Cmd];
  if b = nil then
    begin
      Sender.PrintParam('no exists stream cmd:%s', Cmd);
      exit;
    end;
  if not b.InheritsFrom(TCommandStreamMode) then
    begin
      Sender.PrintParam('Illegal interface in cmd:%s', Cmd);
      exit;
    end;
  InData.Reader.index := 0;
  Result := TCommandStreamMode(b).Execute(Sender, InData, OutData);
end;

function TCommunicationFramework.ExecuteDirectStream(Sender: TPeerClient; Cmd: string; InData: TDataFrameEngine): Boolean;
var
  b: TCoreClassObject;
begin
  Result := False;
  if not CanExecuteCommand(Sender, Cmd) then
      exit;
  b := FCommandList[Cmd];
  if b = nil then
    begin
      Sender.PrintParam('no exists direct console cmd:%s', Cmd);
      exit;
    end;
  if not b.InheritsFrom(TCommandDirectStreamMode) then
    begin
      Sender.PrintParam('Illegal interface in cmd:%s', Cmd);
      exit;
    end;
  InData.Reader.index := 0;
  Result := TCommandDirectStreamMode(b).Execute(Sender, InData);
end;

function TCommunicationFramework.ExecuteDirectConsole(Sender: TPeerClient; Cmd: string; const InData: string): Boolean;
var
  b: TCoreClassObject;
begin
  Result := False;
  if not CanExecuteCommand(Sender, Cmd) then
      exit;
  b := FCommandList[Cmd];
  if b = nil then
    begin
      Sender.PrintParam('no exists direct stream cmd:%s', Cmd);
      exit;
    end;
  if not b.InheritsFrom(TCommandDirectConsoleMode) then
    begin
      Sender.PrintParam('Illegal interface in cmd:%s', Cmd);
      exit;
    end;
  Result := TCommandDirectConsoleMode(b).Execute(Sender, InData);
end;

function TCommunicationFramework.ExecuteBigStream(Sender: TPeerClient; Cmd: string; InData: TCoreClassStream; FBigStreamTotal, BigStreamCompleteSize: Int64): Boolean;
var
  b: TCoreClassObject;
begin
  Result := False;
  if not CanExecuteCommand(Sender, Cmd) then
      exit;
  b := FCommandList[Cmd];
  if b = nil then
    begin
      Sender.PrintParam('no exists Big Stream cmd:%s', Cmd);
      exit;
    end;
  if not b.InheritsFrom(TCommandBigStreamMode) then
    begin
      Sender.PrintParam('Illegal interface in cmd:%s', Cmd);
      exit;
    end;
  Result := TCommandBigStreamMode(b).Execute(Sender, InData, FBigStreamTotal, BigStreamCompleteSize);
end;

procedure TCommunicationFrameworkServer.DoPrint(const v: string);
begin
  inherited DoPrint('server ' + v);
end;

function TCommunicationFrameworkServer.GetItems(index: Integer): TPeerClient;
begin
  try
    if (index >= 0) and (index < Count) then
        Result := FRegistedClients[index] as TPeerClient
    else
        Result := nil;
  except
      Result := nil;
  end;
end;

function TCommunicationFrameworkServer.CanExecuteCommand(Sender: TPeerClient; Cmd: string): Boolean;
begin
  if umlMultipleMatch('ConnectedInit', Cmd) then
    begin
      Result := True;
    end
  else
      Result := inherited CanExecuteCommand(Sender, Cmd);
end;

function TCommunicationFrameworkServer.CanSendCommand(Sender: TPeerClient; Cmd: string): Boolean;
begin
  Result := inherited CanSendCommand(Sender, Cmd);
end;

function TCommunicationFrameworkServer.CanRegCommand(Sender: TCommunicationFramework; Cmd: string): Boolean;
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

procedure TCommunicationFrameworkServer.Command_Wait(Sender: TPeerClient; InData: string; var OutData: string);
begin
  OutData := IntToStr(GetTimeTickCount);
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

procedure TCommunicationFrameworkServer.SendConsoleCmd(Client: TPeerClient; Cmd, ConsoleData: string; OnResult: TConsoleMethod);
var
  p: PQueueData;
begin
  // init queue data
  if (Client = nil) or (not Client.Connected) then
      exit;
  if not CanSendCommand(Client, Cmd) then
      exit;

  p := NewQueueData;
  p^.State := TQueueState.qsSendConsoleCMD;
  p^.Client := Client;
  p^.Cmd := Cmd;
  p^.Cipher := Client.FSendDataCipherStyle;
  p^.ConsoleData := ConsoleData;
  p^.OnConsoleMethod := OnResult;
  TriggerQueueData(p);

  Client.PrintParam('Send Console cmd: %s', Cmd);
end;

procedure TCommunicationFrameworkServer.SendStreamCmd(Client: TPeerClient; Cmd: string; StreamData: TCoreClassStream; OnResult: TStreamMethod; DoneFreeStream: Boolean);
var
  p: PQueueData;
begin
  // init queue data
  if (Client = nil) or (not Client.Connected) then
      exit;
  if not CanSendCommand(Client, Cmd) then
      exit;
  p := NewQueueData;
  p^.State := TQueueState.qsSendStreamCMD;
  p^.Client := Client;
  p^.Cmd := Cmd;
  p^.Cipher := Client.FSendDataCipherStyle;
  p^.DoneFreeStream := DoneFreeStream;
  p^.StreamData := StreamData;
  p^.OnStreamMethod := OnResult;
  TriggerQueueData(p);
  Client.PrintParam('Send Stream cmd: %s', Cmd);
end;

procedure TCommunicationFrameworkServer.SendStreamCmd(Client: TPeerClient; Cmd: string; StreamData: TDataFrameEngine; OnResult: TStreamMethod);
var
  p: PQueueData;
begin
  // init queue data
  if (Client = nil) or (not Client.Connected) then
      exit;
  if not CanSendCommand(Client, Cmd) then
      exit;
  p := NewQueueData;
  p^.State := TQueueState.qsSendStreamCMD;
  p^.Client := Client;
  p^.Cmd := Cmd;
  p^.Cipher := Client.FSendDataCipherStyle;
  p^.DoneFreeStream := True;
  p^.StreamData := TMemoryStream64.Create;
  StreamData.EncodeTo(p^.StreamData, True);
  p^.OnStreamMethod := OnResult;
  TriggerQueueData(p);
  Client.PrintParam('Send Stream cmd: %s', Cmd);
end;

procedure TCommunicationFrameworkServer.SendStreamCmd(Client: TPeerClient; Cmd: string; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamMethod);
var
  p: PQueueData;
begin
  // init queue data
  if (Client = nil) or (not Client.Connected) then
      exit;
  if not CanSendCommand(Client, Cmd) then
      exit;
  p := NewQueueData;
  p^.State := TQueueState.qsSendStreamCMD;
  p^.Client := Client;
  p^.Cmd := Cmd;
  p^.Cipher := Client.FSendDataCipherStyle;
  p^.DoneFreeStream := True;
  p^.StreamData := TMemoryStream64.Create;
  StreamData.EncodeTo(p^.StreamData, True);
  p^.OnStreamParamMethod := OnResult;
  p^.Param1 := p^.Param1;
  p^.Param2 := p^.Param2;
  TriggerQueueData(p);
  Client.PrintParam('Send Stream cmd: %s', Cmd);
end;

{$IFNDEF FPC}


procedure TCommunicationFrameworkServer.SendConsoleCmd(Client: TPeerClient; Cmd, ConsoleData: string; OnResult: TConsoleProc);
var
  p: PQueueData;
begin
  // init queue data
  if (Client = nil) or (not Client.Connected) then
      exit;
  if not CanSendCommand(Client, Cmd) then
      exit;

  p := NewQueueData;
  p^.State := TQueueState.qsSendConsoleCMD;
  p^.Client := Client;
  p^.Cmd := Cmd;
  p^.Cipher := Client.FSendDataCipherStyle;
  p^.ConsoleData := ConsoleData;
  p^.OnConsoleProc := OnResult;
  TriggerQueueData(p);

  Client.PrintParam('Send Console cmd: %s', Cmd);
end;

procedure TCommunicationFrameworkServer.SendStreamCmd(Client: TPeerClient; Cmd: string; StreamData: TCoreClassStream; OnResult: TStreamProc; DoneFreeStream: Boolean);
var
  p: PQueueData;
begin
  // init queue data
  if (Client = nil) or (not Client.Connected) then
      exit;
  if not CanSendCommand(Client, Cmd) then
      exit;
  p := NewQueueData;
  p^.State := TQueueState.qsSendStreamCMD;
  p^.Client := Client;
  p^.Cmd := Cmd;
  p^.Cipher := Client.FSendDataCipherStyle;
  p^.DoneFreeStream := DoneFreeStream;
  p^.StreamData := StreamData;
  p^.OnStreamProc := OnResult;
  TriggerQueueData(p);
  Client.PrintParam('Send Stream cmd: %s', Cmd);
end;

procedure TCommunicationFrameworkServer.SendStreamCmd(Client: TPeerClient; Cmd: string; StreamData: TDataFrameEngine; OnResult: TStreamProc);
var
  p: PQueueData;
begin
  // init queue data
  if (Client = nil) or (not Client.Connected) then
      exit;
  if not CanSendCommand(Client, Cmd) then
      exit;
  p := NewQueueData;
  p^.State := TQueueState.qsSendStreamCMD;
  p^.Client := Client;
  p^.Cmd := Cmd;
  p^.Cipher := Client.FSendDataCipherStyle;
  p^.DoneFreeStream := True;
  p^.StreamData := TMemoryStream64.Create;
  StreamData.EncodeTo(p^.StreamData, True);
  p^.OnStreamProc := OnResult;
  TriggerQueueData(p);
  Client.PrintParam('Send Stream cmd: %s', Cmd);
end;

procedure TCommunicationFrameworkServer.SendStreamCmd(Client: TPeerClient; Cmd: string; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamProc);
var
  p: PQueueData;
begin
  // init queue data
  if (Client = nil) or (not Client.Connected) then
      exit;
  if not CanSendCommand(Client, Cmd) then
      exit;
  p := NewQueueData;
  p^.State := TQueueState.qsSendStreamCMD;
  p^.Client := Client;
  p^.Cmd := Cmd;
  p^.Cipher := Client.FSendDataCipherStyle;
  p^.DoneFreeStream := True;
  p^.StreamData := TMemoryStream64.Create;
  StreamData.EncodeTo(p^.StreamData, True);
  p^.OnStreamParamProc := OnResult;
  p^.Param1 := p^.Param1;
  p^.Param2 := p^.Param2;
  TriggerQueueData(p);
  Client.PrintParam('Send Stream cmd: %s', Cmd);
end;
{$ENDIF}


procedure TCommunicationFrameworkServer.SendDirectConsoleCmd(Client: TPeerClient; Cmd, ConsoleData: string);
var
  p: PQueueData;
begin
  // init queue data
  if (Client = nil) or (not Client.Connected) then
      exit;
  if not CanSendCommand(Client, Cmd) then
      exit;
  p := NewQueueData;
  p^.State := TQueueState.qsSendDirectConsoleCMD;
  p^.Client := Client;
  p^.Cmd := Cmd;
  p^.Cipher := Client.FSendDataCipherStyle;
  p^.ConsoleData := ConsoleData;
  TriggerQueueData(p);
  Client.PrintParam('Send DirectConsole cmd: %s', Cmd);
end;

procedure TCommunicationFrameworkServer.SendDirectStreamCmd(Client: TPeerClient; Cmd: string; StreamData: TCoreClassStream; DoneFreeStream: Boolean);
var
  p: PQueueData;
begin
  // init queue data
  if (Client = nil) or (not Client.Connected) then
      exit;
  if not CanSendCommand(Client, Cmd) then
      exit;
  p := NewQueueData;
  p^.State := TQueueState.qsSendDirectStreamCMD;
  p^.Client := Client;
  p^.Cmd := Cmd;
  p^.Cipher := Client.FSendDataCipherStyle;
  p^.DoneFreeStream := DoneFreeStream;
  p^.StreamData := StreamData;
  TriggerQueueData(p);
  Client.PrintParam('Send DirectStream cmd: %s', Cmd);
end;

procedure TCommunicationFrameworkServer.SendDirectStreamCmd(Client: TPeerClient; Cmd: string; StreamData: TDataFrameEngine);
var
  p: PQueueData;
begin
  // init queue data
  if (Client = nil) or (not Client.Connected) then
      exit;
  if not CanSendCommand(Client, Cmd) then
      exit;
  p := NewQueueData;
  p^.State := TQueueState.qsSendDirectStreamCMD;
  p^.Client := Client;
  p^.Cmd := Cmd;
  p^.Cipher := Client.FSendDataCipherStyle;
  p^.DoneFreeStream := True;
  p^.StreamData := TMemoryStream64.Create;
  StreamData.EncodeTo(p^.StreamData, True);
  TriggerQueueData(p);
  Client.PrintParam('Send DirectStream cmd: %s', Cmd);
end;

procedure TCommunicationFrameworkServer.SendDirectStreamCmd(Client: TPeerClient; Cmd: string);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  SendDirectStreamCmd(Client, Cmd, de);
  DisposeObject(de);
end;

function TCommunicationFrameworkServer.WaitSendConsoleCmd(Client: TPeerClient; Cmd: string; ConsoleData: string; TimeOut: TTimeTickValue): string;
var
  waitIntf: TWaitSendConsoleCmdIntf;
  timetick: TTimeTickValue;
begin
  if (Client = nil) or (not Client.Connected) then
      exit('');
  if not CanSendCommand(Client, Cmd) then
      exit('');

  Client.PrintParam('Begin Wait Console cmd: %s', Cmd);

  timetick := GetTimeTickCount + TimeOut;

  while Client.WaitOnResult or Client.BigStreamProcessing or Client.FWaitSendBusy do
    begin
      ProgressWaitSendOfClient(Client);
      if not Exists(Client) then
          exit;
      if (TimeOut > 0) and (GetTimeTickCount > timetick) then
          exit('');
    end;

  if not Exists(Client) then
      exit('');

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
    Client.PrintParam('End Wait Console cmd: %s', Cmd);
  except
      Result := '';
  end;

  if Exists(Client) then
      Client.FWaitSendBusy := False;
end;

procedure TCommunicationFrameworkServer.WaitSendStreamCmd(Client: TPeerClient; Cmd: string; StreamData, ResultData: TDataFrameEngine; TimeOut: TTimeTickValue);
var
  waitIntf: TWaitSendStreamCmdIntf;
  timetick: Cardinal;
begin
  if (Client = nil) or (not Client.Connected) then
      exit;
  if not CanSendCommand(Client, Cmd) then
      exit;

  Client.PrintParam('Begin Wait Stream cmd: %s', Cmd);

  timetick := GetTimeTickCount + TimeOut;

  while Client.WaitOnResult or Client.BigStreamProcessing or Client.FWaitSendBusy do
    begin
      ProgressWaitSendOfClient(Client);
      if not Exists(Client) then
          exit;
      if (TimeOut > 0) and (GetTimeTickCount > timetick) then
          exit;
    end;

  if not Exists(Client) then
      exit;

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
    Client.PrintParam('End Wait Stream cmd: %s', Cmd);
  except
  end;

  if Exists(Client) then
      Client.FWaitSendBusy := False;
end;

procedure TCommunicationFrameworkServer.SendBigStream(Client: TPeerClient; Cmd: string; BigStream: TCoreClassStream; DoneFreeStream: Boolean);
var
  p: PQueueData;
begin
  // init queue data
  if (Client = nil) or (not Client.Connected) then
      exit;
  if not CanSendCommand(Client, Cmd) then
      exit;
  p := NewQueueData;
  p^.State := TQueueState.qsSendBigStream;
  p^.Client := Client;
  p^.Cmd := Cmd;
  p^.Cipher := Client.FSendDataCipherStyle;
  p^.BigStream := BigStream;
  p^.DoneFreeStream := DoneFreeStream;
  TriggerQueueData(p);
  Client.PrintParam('Send BigStream cmd: %s', Cmd);
end;

procedure TCommunicationFrameworkServer.SendConsoleCmd(ClientID: Cardinal; Cmd, ConsoleData: string; OnResult: TConsoleMethod);
begin
  SendConsoleCmd(ClientFromID[ClientID], Cmd, ConsoleData, OnResult);
end;

procedure TCommunicationFrameworkServer.SendStreamCmd(ClientID: Cardinal; Cmd: string; StreamData: TCoreClassStream; OnResult: TStreamMethod;
DoneFreeStream: Boolean);
begin
  SendStreamCmd(ClientFromID[ClientID], Cmd, StreamData, OnResult, DoneFreeStream);
end;

procedure TCommunicationFrameworkServer.SendStreamCmd(ClientID: Cardinal; Cmd: string; StreamData: TDataFrameEngine; OnResult: TStreamMethod);
begin
  SendStreamCmd(ClientFromID[ClientID], Cmd, StreamData, OnResult);
end;

procedure TCommunicationFrameworkServer.SendStreamCmd(ClientID: Cardinal; Cmd: string; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamMethod);
begin
  SendStreamCmd(ClientFromID[ClientID], Cmd, StreamData, Param1, Param2, OnResult);
end;

{$IFNDEF FPC}


procedure TCommunicationFrameworkServer.SendConsoleCmd(ClientID: Cardinal; Cmd, ConsoleData: string; OnResult: TConsoleProc);
begin
  SendConsoleCmd(ClientFromID[ClientID], Cmd, ConsoleData, OnResult);
end;

procedure TCommunicationFrameworkServer.SendStreamCmd(ClientID: Cardinal; Cmd: string; StreamData: TCoreClassStream; OnResult: TStreamProc;
DoneFreeStream: Boolean);
begin
  SendStreamCmd(ClientFromID[ClientID], Cmd, StreamData, OnResult, DoneFreeStream);
end;

procedure TCommunicationFrameworkServer.SendStreamCmd(ClientID: Cardinal; Cmd: string; StreamData: TDataFrameEngine; OnResult: TStreamProc);
begin
  SendStreamCmd(ClientFromID[ClientID], Cmd, StreamData, OnResult);
end;

procedure TCommunicationFrameworkServer.SendStreamCmd(ClientID: Cardinal; Cmd: string; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamProc);
begin
  SendStreamCmd(ClientFromID[ClientID], Cmd, StreamData, Param1, Param2, OnResult);
end;
{$ENDIF}


procedure TCommunicationFrameworkServer.SendDirectConsoleCmd(ClientID: Cardinal; Cmd, ConsoleData: string);
begin
  SendDirectConsoleCmd(ClientFromID[ClientID], Cmd, ConsoleData);
end;

procedure TCommunicationFrameworkServer.SendDirectStreamCmd(ClientID: Cardinal; Cmd: string; StreamData: TCoreClassStream; DoneFreeStream: Boolean);
begin
  SendDirectStreamCmd(ClientFromID[ClientID], Cmd, StreamData, DoneFreeStream);
end;

procedure TCommunicationFrameworkServer.SendDirectStreamCmd(ClientID: Cardinal; Cmd: string; StreamData: TDataFrameEngine);
begin
  SendDirectStreamCmd(ClientFromID[ClientID], Cmd, StreamData);
end;

procedure TCommunicationFrameworkServer.SendDirectStreamCmd(ClientID: Cardinal; Cmd: string);
begin
  SendDirectStreamCmd(ClientFromID[ClientID], Cmd);
end;

function TCommunicationFrameworkServer.WaitSendConsoleCmd(ClientID: Cardinal; Cmd, ConsoleData: string; TimeOut: TTimeTickValue): string;
begin
  Result := WaitSendConsoleCmd(ClientFromID[ClientID], Cmd, ConsoleData, TimeOut);
end;

procedure TCommunicationFrameworkServer.WaitSendStreamCmd(ClientID: Cardinal; Cmd: string; StreamData, ResultData: TDataFrameEngine; TimeOut: TTimeTickValue);
begin
  WaitSendStreamCmd(ClientFromID[ClientID], Cmd, StreamData, ResultData, TimeOut);
end;

procedure TCommunicationFrameworkServer.SendBigStream(ClientID: Cardinal; Cmd: string; BigStream: TCoreClassStream; DoneFreeStream: Boolean);
begin
  SendBigStream(ClientFromID[ClientID], Cmd, BigStream, DoneFreeStream);
end;

procedure TCommunicationFrameworkServer.BroadcastDirectConsoleCmd(Cmd: string; ConsoleData: string);
var
  i: Integer;
begin
  LockClients;
  i := 0;
  while i < Count do
    begin
      if Items[i] <> nil then
          Items[i].SendDirectConsoleCmd(Cmd, ConsoleData);
      inc(i);
    end;
  UnLockClients;
end;

procedure TCommunicationFrameworkServer.BroadcastSendDirectStreamCmd(Cmd: string; StreamData: TDataFrameEngine);
var
  i: Integer;
begin
  LockClients;
  i := 0;
  while i < Count do
    begin
      if Items[i] <> nil then
          Items[i].SendDirectStreamCmd(Cmd, StreamData);
      inc(i);
    end;
  UnLockClients;
end;

function TCommunicationFrameworkServer.Count: Integer;
begin
  Result := FRegistedClients.Count;
end;

function TCommunicationFrameworkServer.Exists(Client: TCoreClassObject): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to Count - 1 do
    if Items[i] = Client then
        exit;
  Result := False;
end;

function TCommunicationFrameworkServer.Exists(Client: TPeerClient): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to Count - 1 do
    if Items[i] = Client then
        exit;
  Result := False;
end;

function TCommunicationFrameworkServer.Exists(ClientID: Cardinal): Boolean;
begin
  Result := GetClientFromID(ClientID) <> nil;
end;

function TCommunicationFrameworkServer.GetClientFromID(ID: Cardinal): TPeerClient;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].ID = ID then
        exit(Items[i]);
end;

procedure TCommunicationFrameworkClient.DoPrint(const v: string);
begin
  inherited DoPrint('client ' + v);
end;

procedure TCommunicationFrameworkClient.StreamResult_ConnectedInit(Sender: TPeerClient; ResultData: TDataFrameEngine);
var
  arr: TDataFrameArrayByte;
begin
  if ResultData.Count > 0 then
    begin
      // index 0: my remote id
      Sender.FID := ResultData.Reader.ReadCardinal;
      // index 1: used Encrypt
      Sender.FSendDataCipherStyle := TCipherStyle(ResultData.Reader.ReadByte);

      // index 2:Encrypt CipherKey
      arr := ResultData.Reader.ReadArrayByte;
      SetLength(Sender.FCipherKey, arr.Count);
      arr.GetBuff(@Sender.FCipherKey[0]);

      Sender.FRemoteExecutedForConnectInit := True;
    end;
end;

procedure TCommunicationFrameworkClient.DoConnected(Sender: TPeerClient);
var
  de: TDataFrameEngine;
begin
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

function TCommunicationFrameworkClient.CanExecuteCommand(Sender: TPeerClient; Cmd: string): Boolean;
begin
  Result := inherited CanExecuteCommand(Sender, Cmd);
end;

function TCommunicationFrameworkClient.CanSendCommand(Sender: TPeerClient; Cmd: string): Boolean;
begin
  if umlMultipleMatch('ConnectedInit', Cmd) then
    begin
      Result := True;
    end
  else
      Result := inherited CanSendCommand(Sender, Cmd);
end;

function TCommunicationFrameworkClient.CanRegCommand(Sender: TCommunicationFramework; Cmd: string): Boolean;
begin
  Result := inherited CanRegCommand(Sender, Cmd);
end;

constructor TCommunicationFrameworkClient.Create;
begin
  inherited Create;
  FNotyifyInterface := nil;
end;

procedure TCommunicationFrameworkClient.TriggerDoDisconnect;
begin
  DoDisconnect(ClientIO);
end;

function TCommunicationFrameworkClient.Wait(ATimeOut: Cardinal): string;
begin
  Result := '';
  if not Connected then
      exit;
  Result := WaitSendConsoleCmd('Wait', '', ATimeOut);
end;

procedure TCommunicationFrameworkClient.SendConsoleCmd(Cmd, ConsoleData: string; OnResult: TConsoleMethod);
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
  p^.State := TQueueState.qsSendConsoleCMD;

  p^.Cmd := Cmd;
  p^.Cipher := ClientIO.FSendDataCipherStyle;
  p^.ConsoleData := ConsoleData;
  p^.OnConsoleMethod := OnResult;
  TriggerQueueData(p);
  ClientIO.PrintParam('Send Console cmd: %s', Cmd);
end;

procedure TCommunicationFrameworkClient.SendStreamCmd(Cmd: string; StreamData: TCoreClassStream; OnResult: TStreamMethod; DoneFreeStream: Boolean);
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
  p^.State := TQueueState.qsSendStreamCMD;

  p^.Cmd := Cmd;
  p^.Cipher := ClientIO.FSendDataCipherStyle;
  p^.DoneFreeStream := DoneFreeStream;
  p^.StreamData := StreamData;
  p^.OnStreamMethod := OnResult;
  TriggerQueueData(p);
  ClientIO.PrintParam('Send Stream cmd: %s', Cmd);
end;

procedure TCommunicationFrameworkClient.SendStreamCmd(Cmd: string; StreamData: TDataFrameEngine; OnResult: TStreamMethod);
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
  p^.State := TQueueState.qsSendStreamCMD;

  p^.Cmd := Cmd;
  p^.Cipher := ClientIO.FSendDataCipherStyle;
  p^.DoneFreeStream := True;
  p^.StreamData := TMemoryStream64.Create;
  StreamData.EncodeTo(p^.StreamData, True);
  p^.OnStreamMethod := OnResult;
  TriggerQueueData(p);
  ClientIO.PrintParam('Send Stream cmd: %s', Cmd);
end;

procedure TCommunicationFrameworkClient.SendStreamCmd(Cmd: string; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamMethod);
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
  p^.State := TQueueState.qsSendStreamCMD;

  p^.Cmd := Cmd;
  p^.Cipher := ClientIO.FSendDataCipherStyle;
  p^.DoneFreeStream := True;
  p^.StreamData := TMemoryStream64.Create;
  StreamData.EncodeTo(p^.StreamData, True);
  p^.OnStreamParamMethod := OnResult;
  p^.Param1 := Param1;
  p^.Param2 := Param2;
  TriggerQueueData(p);
  ClientIO.PrintParam('Send Stream cmd: %s', Cmd);
end;

{$IFNDEF FPC}


procedure TCommunicationFrameworkClient.SendConsoleCmd(Cmd, ConsoleData: string; OnResult: TConsoleProc);
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
  p^.State := TQueueState.qsSendConsoleCMD;

  p^.Cmd := Cmd;
  p^.Cipher := ClientIO.FSendDataCipherStyle;
  p^.ConsoleData := ConsoleData;
  p^.OnConsoleProc := OnResult;
  TriggerQueueData(p);
  ClientIO.PrintParam('Send Console cmd: %s', Cmd);
end;

procedure TCommunicationFrameworkClient.SendStreamCmd(Cmd: string; StreamData: TCoreClassStream; OnResult: TStreamProc; DoneFreeStream: Boolean);
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
  p^.State := TQueueState.qsSendStreamCMD;

  p^.Cmd := Cmd;
  p^.Cipher := ClientIO.FSendDataCipherStyle;
  p^.DoneFreeStream := DoneFreeStream;
  p^.StreamData := StreamData;
  p^.OnStreamProc := OnResult;
  TriggerQueueData(p);
  ClientIO.PrintParam('Send Stream cmd: %s', Cmd);
end;

procedure TCommunicationFrameworkClient.SendStreamCmd(Cmd: string; StreamData: TDataFrameEngine; OnResult: TStreamProc);
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
  p^.State := TQueueState.qsSendStreamCMD;

  p^.Cmd := Cmd;
  p^.Cipher := ClientIO.FSendDataCipherStyle;
  p^.DoneFreeStream := True;
  p^.StreamData := TMemoryStream64.Create;
  StreamData.EncodeTo(p^.StreamData, True);
  p^.OnStreamProc := OnResult;
  TriggerQueueData(p);
  ClientIO.PrintParam('Send Stream cmd: %s', Cmd);
end;

procedure TCommunicationFrameworkClient.SendStreamCmd(Cmd: string; StreamData: TDataFrameEngine; Param1: Pointer; Param2: TObject; OnResult: TStreamParamProc);
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
  p^.State := TQueueState.qsSendStreamCMD;

  p^.Cmd := Cmd;
  p^.Cipher := ClientIO.FSendDataCipherStyle;
  p^.DoneFreeStream := True;
  p^.StreamData := TMemoryStream64.Create;
  StreamData.EncodeTo(p^.StreamData, True);
  p^.OnStreamParamProc := OnResult;
  p^.Param1 := Param1;
  p^.Param2 := Param2;
  TriggerQueueData(p);
  ClientIO.PrintParam('Send Stream cmd: %s', Cmd);
end;
{$ENDIF}


procedure TCommunicationFrameworkClient.SendDirectConsoleCmd(Cmd, ConsoleData: string);
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
  p^.State := TQueueState.qsSendDirectConsoleCMD;

  p^.Cmd := Cmd;
  p^.Cipher := ClientIO.FSendDataCipherStyle;
  p^.ConsoleData := ConsoleData;
  TriggerQueueData(p);
  ClientIO.PrintParam('Send DirectConsole cmd: %s', Cmd);
end;

procedure TCommunicationFrameworkClient.SendDirectStreamCmd(Cmd: string; StreamData: TCoreClassStream; DoneFreeStream: Boolean);
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
  p^.State := TQueueState.qsSendDirectStreamCMD;

  p^.Cmd := Cmd;
  p^.Cipher := ClientIO.FSendDataCipherStyle;
  p^.DoneFreeStream := DoneFreeStream;
  p^.StreamData := StreamData;
  TriggerQueueData(p);
  ClientIO.PrintParam('Send DirectStream cmd: %s', Cmd);
end;

procedure TCommunicationFrameworkClient.SendDirectStreamCmd(Cmd: string; StreamData: TDataFrameEngine);
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
  p^.State := TQueueState.qsSendDirectStreamCMD;

  p^.Cmd := Cmd;
  p^.Cipher := ClientIO.FSendDataCipherStyle;
  p^.DoneFreeStream := True;
  p^.StreamData := TMemoryStream64.Create;
  StreamData.EncodeTo(p^.StreamData, True);
  TriggerQueueData(p);
  ClientIO.PrintParam('Send DirectStream cmd: %s', Cmd);
end;

procedure TCommunicationFrameworkClient.SendDirectStreamCmd(Cmd: string);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  SendDirectStreamCmd(Cmd, de);
  DisposeObject(de);
end;

function TCommunicationFrameworkClient.WaitSendConsoleCmd(Cmd: string; ConsoleData: string; TimeOut: TTimeTickValue): string;
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
  ClientIO.PrintParam('Begin Wait console cmd: %s', Cmd);

  timetick := GetTimeTickCount + TimeOut;

  while ClientIO.WaitOnResult or ClientIO.BigStreamProcessing or ClientIO.FWaitSendBusy do
    begin
      ProgressWaitSendOfClient(ClientIO);
      if not Connected then
          exit;
      if (TimeOut > 0) and (GetTimeTickCount > timetick) then
          exit;
    end;

  if not Connected then
      exit('');

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
    ClientIO.PrintParam('End Wait console cmd: %s', Cmd);
  except
      Result := '';
  end;

  if Connected then
      ClientIO.FWaitSendBusy := False;
end;

procedure TCommunicationFrameworkClient.WaitSendStreamCmd(Cmd: string; StreamData, ResultData: TDataFrameEngine; TimeOut: TTimeTickValue);
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

  ClientIO.PrintParam('Begin Wait Stream cmd: %s', Cmd);

  timetick := GetTimeTickCount + TimeOut;

  while ClientIO.WaitOnResult or ClientIO.BigStreamProcessing or ClientIO.FWaitSendBusy do
    begin
      ProgressWaitSendOfClient(ClientIO);
      if not Connected then
          exit;
      if (TimeOut > 0) and (GetTimeTickCount > timetick) then
          exit;
    end;

  if not Connected then
      exit;

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
    ClientIO.PrintParam('End Wait Stream cmd: %s', Cmd);
  except
  end;

  if Connected then
      ClientIO.FWaitSendBusy := False;
end;

procedure TCommunicationFrameworkClient.SendBigStream(Cmd: string; BigStream: TCoreClassStream; DoneFreeStream: Boolean);
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
  p^.Cipher := ClientIO.FSendDataCipherStyle;
  p^.BigStream := BigStream;
  p^.DoneFreeStream := DoneFreeStream;
  TriggerQueueData(p);
  ClientIO.PrintParam('Send BigStream cmd: %s', Cmd);
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
