{ ****************************************************************************** }
{ * DataStore Service                                                          * }
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
(*
  update history
*)

unit CommunicationFrameworkDataStoreService;

{$INCLUDE zDefine.inc}

interface


uses CoreClasses, ListEngine, UnicodeMixedLib, DataFrameEngine, MemoryStream64, CommunicationFramework, TextDataEngine,
  DoStatusIO, Cadencer, NotifyObjectBase, PascalStrings, CoreCipher, ZDBEngine, ItemStream, CoreCompress,
{$IFNDEF FPC}
  SysUtils, ZS_JsonDataObjects,
{$ENDIF}
  CommunicationFrameworkDoubleTunnelIO, CommunicationFrameworkDataStoreServiceCommon, ZDBLocalManager;

type
  TDataStoreService = class;
  TDataStoreService_PeerClientSendTunnel = class;

  TDataStoreService_PeerClientRecvTunnel = class(TPeerClientUserDefineForRecvTunnel)
  private
    FPostPerformaceCounter: Integer;
    FLastPostPerformaceTime: TTimeTick;
    FPostCounterOfPerSec: Double;
  private
    { data security }
    FDataStoreCipherSecurity: TCipherSecurity;
    FDataStoreCipherKey: TCipherKeyBuffer;
    FCipherInstance: TCipher_Base;
  public
    constructor Create(Owner_: TPeerIO); override;
    destructor Destroy; override;

    procedure Progress; override;

    function SendTunnelDefine: TDataStoreService_PeerClientSendTunnel;
    property PostCounterOfPerSec: Double read FPostCounterOfPerSec;

    { data security }
    procedure EncryptBuffer(sour: Pointer; Size: NativeInt; Encrypt: Boolean);
  end;

  TDataStoreService_PeerClientSendTunnel = class(TPeerClientUserDefineForSendTunnel)
  public
    constructor Create(Owner_: TPeerIO); override;
    destructor Destroy; override;

    function RecvTunnelDefine: TDataStoreService_PeerClientRecvTunnel;
  end;

  TDataStoreService = class(TCommunicationFramework_DoubleTunnelService, IZDBLocalManagerNotify)
  private
    FZDBLocal: TZDBLocalManager;
    FQueryCallPool: THashObjectList;
    FPerQueryPipelineDelayFreeTime: Double;
  protected
    { interface from IZDBLocalManagerNotify }
    procedure CreateQuery(pipe: TZDBPipeline); virtual;
    procedure QueryFragmentData(pipe: TZDBPipeline; FragmentSource: TMemoryStream64); virtual;
    procedure QueryDone(pipe: TZDBPipeline); virtual;
    procedure StorePosTransform(const Data: Pointer; const TransformBuff: PZDBStorePosTransformArray);
    procedure OpenDB(ActiveDB: TZDBLMStore); virtual;
    procedure CreateDB(ActiveDB: TZDBLMStore); virtual;
    procedure CloseDB(ActiveDB: TZDBLMStore); virtual;
    procedure InsertData(Sender: TZDBLMStore; InsertPos: Int64; buff: TCoreClassStream; ID: Cardinal; CompletePos: Int64); virtual;
    procedure AddData(Sender: TZDBLMStore; buff: TCoreClassStream; ID: Cardinal; CompletePos: Int64); virtual;
    procedure ModifyData(Sender: TZDBLMStore; const StorePos: Int64; buff: TCoreClassStream); virtual;
    procedure DeleteData(Sender: TZDBLMStore; const StorePos: Int64); virtual;
  protected
    procedure DownloadQueryFilterMethod(dPipe: TZDBPipeline; var qState: TQueryState; var Allowed: Boolean);
    procedure DownloadQueryWithIDFilterMethod(dPipe: TZDBPipeline; var qState: TQueryState; var Allowed: Boolean);

    procedure UserOut(UserDefineIO: TPeerClientUserDefineForRecvTunnel); override;
    procedure UserLinkSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel); override;

    procedure Command_InitDB(Sender: TPeerIO; InData: TDataFrameEngine); virtual;
    procedure Command_CloseDB(Sender: TPeerIO; InData: TDataFrameEngine); virtual;

    procedure Command_CopyDB(Sender: TPeerIO; InData: TDataFrameEngine); virtual;
    procedure Command_CompressDB(Sender: TPeerIO; InData: TDataFrameEngine); virtual;
    procedure Command_ReplaceDB(Sender: TPeerIO; InData: TDataFrameEngine); virtual;
    procedure Command_ResetData(Sender: TPeerIO; InData: TDataFrameEngine); virtual;

    procedure Command_QueryDB(Sender: TPeerIO; InData: TDataFrameEngine); virtual;

    procedure Command_DownloadDB(Sender: TPeerIO; InData: TDataFrameEngine); virtual;
    procedure Command_DownloadDBWithID(Sender: TPeerIO; InData: TDataFrameEngine); virtual;

    procedure Command_RequestDownloadAssembleStream(Sender: TPeerIO; InData: TDataFrameEngine); virtual;
    procedure Command_RequestFastDownloadAssembleStream(Sender: TPeerIO; InData: TDataFrameEngine); virtual;

    procedure Command_FastPostCompleteBuffer(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
    procedure Command_FastInsertCompleteBuffer(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
    procedure Command_FastModifyCompleteBuffer(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);

    procedure Command_CompletedPostAssembleStream(Sender: TPeerIO; InData: TDataFrameEngine); virtual;
    procedure Command_CompletedInsertAssembleStream(Sender: TPeerIO; InData: TDataFrameEngine); virtual;
    procedure Command_CompletedModifyAssembleStream(Sender: TPeerIO; InData: TDataFrameEngine); virtual;

    procedure Command_DeleteData(Sender: TPeerIO; InData: TDataFrameEngine); virtual;

    procedure Command_GetDBList(Sender: TPeerIO; InData, OutData: TDataFrameEngine); virtual;
    procedure Command_GetQueryList(Sender: TPeerIO; InData, OutData: TDataFrameEngine); virtual;
    procedure Command_GetQueryState(Sender: TPeerIO; InData, OutData: TDataFrameEngine); virtual;
    procedure Command_QueryStop(Sender: TPeerIO; InData: TDataFrameEngine); virtual;
    procedure Command_QueryPause(Sender: TPeerIO; InData: TDataFrameEngine); virtual;
    procedure Command_QueryPlay(Sender: TPeerIO; InData: TDataFrameEngine); virtual;

    { send client command }
    procedure Send_CompletedFragmentBigStream(pipe: TTDataStoreService_DBPipeline);
    procedure Send_CompletedQuery(pipe: TTDataStoreService_DBPipeline);
    procedure Send_CompletedDownloadAssemble(SendCli_: TPeerIO; dataBaseName_: SystemString; dStorePos: Int64; BackcallPtr: UInt64);
    procedure Send_CompletedFastDownloadAssemble(SendCli_: TPeerIO; dataBaseName_: SystemString; dStorePos: Int64; BackcallPtr: UInt64);
    procedure Send_CompletedStorePosTransform(SendCli_: TPeerIO; const BackcallPtr: UInt64; const TransformBuff: PZDBStorePosTransformArray);
  public
    constructor Create(RecvTunnel_, SendTunnel_: TCommunicationFrameworkServer); override;
    destructor Destroy; override;

    procedure RegisterCommand; override;
    procedure UnRegisterCommand; override;

    procedure Progress; override;
    procedure CadencerProgress(Sender: TObject; const deltaTime, newTime: Double); override;

    function GetDataStoreUserDefine(RecvCli: TPeerIO): TDataStoreService_PeerClientRecvTunnel;

    function RegisterQueryCall(QuerierName_: SystemString): TTDataStoreService_QueryCall;
    procedure UnRegisterQueryCall(QuerierName_: SystemString);
    function GetRegistedQueryCall(QuerierName_: SystemString): TTDataStoreService_QueryCall;

    function PostCounterOfPerSec: Double;

    property ZDBLocal: TZDBLocalManager read FZDBLocal;
    property QueryCallPool: THashObjectList read FQueryCallPool;
    property PerQueryPipelineDelayFreeTime: Double read FPerQueryPipelineDelayFreeTime write FPerQueryPipelineDelayFreeTime;
  end;

  TDataStoreClient = class(TCommunicationFramework_DoubleTunnelClient)
  private
    FDataStoreCipherSecurity: TCipherSecurity;
    FDataStoreCipherKey: TCipherKeyBuffer;
    FCipherInstance: TCipher_Base;
    procedure EncryptBuffer(sour: Pointer; Size: NativeInt; Encrypt: Boolean);
    procedure Command_DataStoreSecurity(Sender: TPeerIO; InData: TDataFrameEngine);
  private
    procedure Command_CompletedFragmentBigStream(Sender: TPeerIO; InData: TDataFrameEngine); virtual;
    procedure Command_CompletedQuery(Sender: TPeerIO; InData: TDataFrameEngine); virtual;
    procedure Command_CompletedDownloadAssemble(Sender: TPeerIO; InData: TDataFrameEngine); virtual;
    procedure Command_CompletedFastDownloadAssemble(Sender: TPeerIO; InData: TDataFrameEngine); virtual;
    procedure Command_CompletedStorePosTransform(Sender: TPeerIO; InData: TDataFrameEngine); virtual;
  public
    constructor Create(RecvTunnel_, SendTunnel_: TCommunicationFrameworkClient); override;
    destructor Destroy; override;

    procedure RegisterCommand; override;
    procedure UnRegisterCommand; override;

    procedure Progress; override;

    function DataCipherKeyFinished: Boolean;

    procedure InitDB(InMem: Boolean; dataBaseName_: SystemString); virtual;
    procedure CloseDB(dataBaseName_: SystemString; CloseAndDeleted: Boolean); virtual;

    procedure CopyDB(dataBaseName_, CopyDestDatabaseName_: SystemString); overload;
    procedure CopyDB(dataBaseName_, CopyDestDatabaseName_: SystemString; const BackcallPtr: PStorePosTransformNotify); overload;
    procedure CopyDB_C(dataBaseName_, CopyDestDatabaseName_: SystemString; const OnDoneCall: TStorePosTransformNotifyCall); overload;
    procedure CopyDB_M(dataBaseName_, CopyDestDatabaseName_: SystemString; const OnDoneMethod: TStorePosTransformNotifyMethod); overload;
    procedure CopyDB_P(dataBaseName_, CopyDestDatabaseName_: SystemString; const OnDoneProc: TStorePosTransformNotifyProc); overload;

    procedure CompressDB(dataBaseName_: SystemString); overload;
    procedure CompressDB(dataBaseName_: SystemString; const BackcallPtr: PStorePosTransformNotify); overload;
    procedure CompressDB_C(dataBaseName_: SystemString; const OnDoneCall: TStorePosTransformNotifyCall); overload;
    procedure CompressDB_M(dataBaseName_: SystemString; const OnDoneMethod: TStorePosTransformNotifyMethod); overload;
    procedure CompressDB_P(dataBaseName_: SystemString; const OnDoneProc: TStorePosTransformNotifyProc); overload;

    procedure ReplaceDB(dataBaseName_, replaceN: SystemString); virtual;
    procedure ResetData(dataBaseName_: SystemString); virtual;

    procedure QuietQueryDB(RegistedQuerier_: SystemString; ReverseQuery: Boolean; dataBaseName_, OutputDatabaseName_: SystemString; MaxWait: Double; MaxQueryResult: Int64); virtual;

    procedure QueryDB(RegistedQuerier_: SystemString; SyncToClient, WriteResultToOutputDB, InMem, ReverseQuery: Boolean; dataBaseName_, OutputDatabaseName_: SystemString;
      fragmentReponseTime, MaxWait: Double; MaxQueryResult: Int64; BackcallPtr: PDataStoreClientQueryNotify; RemoteParams: THashVariantList); overload; virtual;

    procedure QueryDBC(RegistedQuerier_: SystemString; SyncToClient, WriteResultToOutputDB, InMem, ReverseQuery: Boolean; dataBaseName_, OutputDatabaseName_: SystemString;
      fragmentReponseTime, MaxWait: Double; MaxQueryResult: Int64;
      RemoteParams: THashVariantList; { service ref remote parameter }
      OnQueryCall: TFillQueryDataCall; OnDoneCall: TQueryDoneNotifyCall); overload;

    procedure QueryDBC(RegistedQuerier_: SystemString; SyncToClient, WriteResultToOutputDB, InMem, ReverseQuery: Boolean; dataBaseName_, OutputDatabaseName_: SystemString;
      fragmentReponseTime, MaxWait: Double; MaxQueryResult: Int64;
      RemoteParams: THashVariantList;                                           { service ref remote parameter }
      UserPointer: Pointer; UserObject: TCoreClassObject; UserVariant: Variant; { local event parameter }
      OnQueryCall: TUserFillQueryDataCall; OnDoneCall: TUserQueryDoneNotifyCall); overload;

    procedure QueryDBM(RegistedQuerier_: SystemString; SyncToClient, WriteResultToOutputDB, InMem, ReverseQuery: Boolean; dataBaseName_, OutputDatabaseName_: SystemString;
      fragmentReponseTime, MaxWait: Double; MaxQueryResult: Int64;
      RemoteParams: THashVariantList; { service ref remote parameter }
      OnQueryMethod: TFillQueryDataMethod; OnDoneMethod: TQueryDoneNotifyMethod); overload;

    procedure QueryDBM(RegistedQuerier_: SystemString; SyncToClient, WriteResultToOutputDB, InMem, ReverseQuery: Boolean; dataBaseName_, OutputDatabaseName_: SystemString;
      fragmentReponseTime, MaxWait: Double; MaxQueryResult: Int64;
      RemoteParams: THashVariantList;                                           { service ref remote parameter }
      UserPointer: Pointer; UserObject: TCoreClassObject; UserVariant: Variant; { local event parameter }
      OnQueryMethod: TUserFillQueryDataMethod; OnDoneMethod: TUserQueryDoneNotifyMethod); overload;

    procedure QueryDBP(RegistedQuerier_: SystemString; SyncToClient, WriteResultToOutputDB, InMem, ReverseQuery: Boolean; dataBaseName_, OutputDatabaseName_: SystemString;
      fragmentReponseTime, MaxWait: Double; MaxQueryResult: Int64;
      RemoteParams: THashVariantList; { service ref remote parameter }
      OnQueryProc: TFillQueryDataProc; OnDoneProc: TQueryDoneNotifyProc); overload;

    procedure QueryDBP(RegistedQuerier_: SystemString; SyncToClient, WriteResultToOutputDB, InMem, ReverseQuery: Boolean; dataBaseName_, OutputDatabaseName_: SystemString;
      fragmentReponseTime, MaxWait: Double; MaxQueryResult: Int64;
      RemoteParams: THashVariantList;                                           { service ref remote parameter }
      UserPointer: Pointer; UserObject: TCoreClassObject; UserVariant: Variant; { local event parameter }
      OnQueryProc: TUserFillQueryDataProc; OnDoneProc: TUserQueryDoneNotifyProc); overload;

    procedure QueryDBC(RegistedQuerier_: SystemString; dataBaseName_: SystemString; RemoteParams: THashVariantList; OnQueryCall: TFillQueryDataCall; OnDoneCall: TQueryDoneNotifyCall); overload;
    procedure QueryDBM(RegistedQuerier_: SystemString; dataBaseName_: SystemString; RemoteParams: THashVariantList; OnQueryMethod: TFillQueryDataMethod; OnDoneMethod: TQueryDoneNotifyMethod); overload;
    procedure QueryDBP(RegistedQuerier_: SystemString; dataBaseName_: SystemString; RemoteParams: THashVariantList; OnQueryProc: TFillQueryDataProc; OnDoneProc: TQueryDoneNotifyProc); overload;

    procedure DownloadDB(ReverseQuery: Boolean; dataBaseName_: SystemString; BackcallPtr: PDataStoreClientQueryNotify); overload; virtual;
    procedure DownloadDBC(ReverseQuery: Boolean; dataBaseName_: SystemString; OnQueryCall: TFillQueryDataCall; OnDoneCall: TQueryDoneNotifyCall); overload;
    procedure DownloadDBM(ReverseQuery: Boolean; dataBaseName_: SystemString; OnQueryMethod: TFillQueryDataMethod; OnDoneMethod: TQueryDoneNotifyMethod); overload;
    procedure DownloadDBP(ReverseQuery: Boolean; dataBaseName_: SystemString; OnQueryProc: TFillQueryDataProc; OnDoneProc: TQueryDoneNotifyProc); overload;

    procedure DownloadDBWithID(ReverseQuery: Boolean; dataBaseName_: SystemString; db_ID: Cardinal; BackcallPtr: PDataStoreClientQueryNotify); overload; virtual;
    procedure DownloadDBWithIDC(ReverseQuery: Boolean; dataBaseName_: SystemString; db_ID: Cardinal; OnQueryCall: TFillQueryDataCall; OnDoneCall: TQueryDoneNotifyCall); overload;
    procedure DownloadDBWithIDM(ReverseQuery: Boolean; dataBaseName_: SystemString; db_ID: Cardinal; OnQueryMethod: TFillQueryDataMethod; OnDoneMethod: TQueryDoneNotifyMethod); overload;
    procedure DownloadDBWithIDP(ReverseQuery: Boolean; dataBaseName_: SystemString; db_ID: Cardinal; OnQueryProc: TFillQueryDataProc; OnDoneProc: TQueryDoneNotifyProc); overload;

    procedure BeginAssembleStream; virtual;

    procedure RequestDownloadAssembleStream(dataBaseName_: SystemString; StorePos: Int64; BackcallPtr: PDataStoreClientDownloadNotify); virtual;
    procedure DownloadAssembleStreamC(dataBaseName_: SystemString; StorePos: Int64; OnDoneCall: TDownloadDoneNotifyCall); overload;
    procedure DownloadAssembleStreamM(dataBaseName_: SystemString; StorePos: Int64; OnDoneMethod: TDownloadDoneNotifyMethod); overload;
    procedure DownloadAssembleStreamP(dataBaseName_: SystemString; StorePos: Int64; OnDoneProc: TDownloadDoneNotifyProc); overload;
    procedure DownloadAssembleStreamC(dataBaseName_: SystemString; StorePos: Int64; AutoDecodeZDBStream: Boolean; OnDoneCall: TDownloadDoneNotifyCall); overload;
    procedure DownloadAssembleStreamM(dataBaseName_: SystemString; StorePos: Int64; AutoDecodeZDBStream: Boolean; OnDoneMethod: TDownloadDoneNotifyMethod); overload;
    procedure DownloadAssembleStreamP(dataBaseName_: SystemString; StorePos: Int64; AutoDecodeZDBStream: Boolean; OnDoneProc: TDownloadDoneNotifyProc); overload;

    procedure DownloadAssembleStreamC(dataBaseName_: SystemString; StorePos: Int64;
      UserPointer: Pointer; UserObject: TCoreClassObject; UserVariant: Variant; { local event parameter }
      OnDoneCall: TUserDownloadDoneNotifyCall); overload;
    procedure DownloadAssembleStreamM(dataBaseName_: SystemString; StorePos: Int64;
      UserPointer: Pointer; UserObject: TCoreClassObject; UserVariant: Variant; { local event parameter }
      OnDoneMethod: TUserDownloadDoneNotifyMethod); overload;
    procedure DownloadAssembleStreamP(dataBaseName_: SystemString; StorePos: Int64;
      UserPointer: Pointer; UserObject: TCoreClassObject; UserVariant: Variant; { local event parameter }
      OnDoneProc: TUserDownloadDoneNotifyProc); overload;

    procedure DownloadAssembleStreamC(dataBaseName_: SystemString; StorePos: Int64; AutoDecodeZDBStream: Boolean;
      UserPointer: Pointer; UserObject: TCoreClassObject; UserVariant: Variant; { local event parameter }
      OnDoneCall: TUserDownloadDoneNotifyCall); overload;
    procedure DownloadAssembleStreamM(dataBaseName_: SystemString; StorePos: Int64; AutoDecodeZDBStream: Boolean;
      UserPointer: Pointer; UserObject: TCoreClassObject; UserVariant: Variant; { local event parameter }
      OnDoneMethod: TUserDownloadDoneNotifyMethod); overload;
    procedure DownloadAssembleStreamP(dataBaseName_: SystemString; StorePos: Int64; AutoDecodeZDBStream: Boolean;
      UserPointer: Pointer; UserObject: TCoreClassObject; UserVariant: Variant; { local event parameter }
      OnDoneProc: TUserDownloadDoneNotifyProc); overload;

    procedure RequestFastDownloadAssembleStream(dataBaseName_: SystemString; StorePos: Int64; BackcallPtr: PDataStoreClientDownloadNotify); virtual;
    procedure FastDownloadAssembleStreamC(dataBaseName_: SystemString; StorePos: Int64; OnDoneCall: TDownloadDoneNotifyCall); overload;
    procedure FastDownloadAssembleStreamM(dataBaseName_: SystemString; StorePos: Int64; OnDoneMethod: TDownloadDoneNotifyMethod); overload;
    procedure FastDownloadAssembleStreamP(dataBaseName_: SystemString; StorePos: Int64; OnDoneProc: TDownloadDoneNotifyProc); overload;

    procedure FastDownloadAssembleStreamC(dataBaseName_: SystemString; StorePos: Int64; AutoDecodeZDBStream: Boolean; OnDoneCall: TDownloadDoneNotifyCall); overload;
    procedure FastDownloadAssembleStreamM(dataBaseName_: SystemString; StorePos: Int64; AutoDecodeZDBStream: Boolean; OnDoneMethod: TDownloadDoneNotifyMethod); overload;
    procedure FastDownloadAssembleStreamP(dataBaseName_: SystemString; StorePos: Int64; AutoDecodeZDBStream: Boolean; OnDoneProc: TDownloadDoneNotifyProc); overload;

    procedure FastDownloadAssembleStreamC(dataBaseName_: SystemString; StorePos: Int64;
      UserPointer: Pointer; UserObject: TCoreClassObject; UserVariant: Variant; { local event parameter }
      OnDoneCall: TUserDownloadDoneNotifyCall); overload;
    procedure FastDownloadAssembleStreamM(dataBaseName_: SystemString; StorePos: Int64;
      UserPointer: Pointer; UserObject: TCoreClassObject; UserVariant: Variant; { local event parameter }
      OnDoneMethod: TUserDownloadDoneNotifyMethod); overload;
    procedure FastDownloadAssembleStreamP(dataBaseName_: SystemString; StorePos: Int64;
      UserPointer: Pointer; UserObject: TCoreClassObject; UserVariant: Variant; { local event parameter }
      OnDoneProc: TUserDownloadDoneNotifyProc); overload;

    procedure FastDownloadAssembleStreamC(dataBaseName_: SystemString; StorePos: Int64; AutoDecodeZDBStream: Boolean;
      UserPointer: Pointer; UserObject: TCoreClassObject; UserVariant: Variant; { local event parameter }
      OnDoneCall: TUserDownloadDoneNotifyCall); overload;
    procedure FastDownloadAssembleStreamM(dataBaseName_: SystemString; StorePos: Int64; AutoDecodeZDBStream: Boolean;
      UserPointer: Pointer; UserObject: TCoreClassObject; UserVariant: Variant; { local event parameter }
      OnDoneMethod: TUserDownloadDoneNotifyMethod); overload;
    procedure FastDownloadAssembleStreamP(dataBaseName_: SystemString; StorePos: Int64; AutoDecodeZDBStream: Boolean;
      UserPointer: Pointer; UserObject: TCoreClassObject; UserVariant: Variant; { local event parameter }
      OnDoneProc: TUserDownloadDoneNotifyProc); overload;

    { Security post support }
    procedure PostAssembleStream(dataBaseName_: SystemString; stream: TMemoryStream64; dID: Cardinal; DoneTimeFree: Boolean); overload; virtual;
    procedure PostAssembleStreamCopy(dataBaseName_: SystemString; stream: TCoreClassStream; dID: Cardinal);
    procedure PostAssembleStream(dataBaseName_: SystemString; DataSource: TDataFrameEngine); overload;
    procedure PostAssembleStream(dataBaseName_: SystemString; DataSource: THashVariantList); overload;
    procedure PostAssembleStream(dataBaseName_: SystemString; DataSource: THashStringList); overload;
    procedure PostAssembleStream(dataBaseName_: SystemString; DataSource: TSectionTextData); overload;
{$IFNDEF FPC} procedure PostAssembleStream(dataBaseName_: SystemString; DataSource: TJsonObject); overload; virtual; {$ENDIF FPC}
    procedure PostAssembleStream(dataBaseName_: SystemString; DataSource: TPascalString); overload;

    { Security insert support }
    procedure InsertAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; stream: TMemoryStream64; dID: Cardinal; DoneTimeFree: Boolean); overload; virtual;
    procedure InsertAssembleStreamCopy(dataBaseName_: SystemString; dStorePos: Int64; stream: TCoreClassStream; dID: Cardinal);
    procedure InsertAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TDataFrameEngine); overload;
    procedure InsertAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; DataSource: THashVariantList); overload;
    procedure InsertAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; DataSource: THashStringList); overload;
    procedure InsertAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TSectionTextData); overload;
{$IFNDEF FPC} procedure InsertAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TJsonObject); overload; {$ENDIF FPC}
    procedure InsertAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TPascalString); overload;

    { Security modify support }
    procedure ModifyAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; stream: TMemoryStream64; DoneTimeFree: Boolean); overload; virtual;
    procedure ModifyAssembleStreamCopy(dataBaseName_: SystemString; dStorePos: Int64; stream: TCoreClassStream);
    procedure ModifyAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TDataFrameEngine); overload;
    procedure ModifyAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; DataSource: THashVariantList); overload;
    procedure ModifyAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; DataSource: THashStringList); overload;
    procedure ModifyAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TSectionTextData); overload;
{$IFNDEF FPC} procedure ModifyAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TJsonObject); overload; {$ENDIF FPC}
    procedure ModifyAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TPascalString); overload;

    procedure GetPostAssembleStreamStateM(OnResult: TStreamMethod); overload;
    procedure GetPostAssembleStreamStateM(Param1: Pointer; Param2: TObject; OnResult: TStreamParamMethod); overload;
    procedure GetPostAssembleStreamStateP(OnResult: TStreamProc); overload;
    procedure GetPostAssembleStreamStateP(Param1: Pointer; Param2: TObject; OnResult: TStreamParamProc); overload;

    procedure EndAssembleStream; virtual;

    procedure DeleteData(dataBaseName_: SystemString; dStorePos: Int64); virtual;

    { fast post support }
    procedure FastPostCompleteBuffer(dataBaseName_: SystemString; stream: TMemoryStream64; dID: Cardinal; DoneTimeFree: Boolean); overload; virtual;
    procedure FastPostCompleteBufferCopy(dataBaseName_: SystemString; stream: TCoreClassStream; dID: Cardinal);
    procedure FastPostCompleteBuffer(dataBaseName_: SystemString; DataSource: TDataFrameEngine); overload;
    procedure FastPostCompleteBuffer(dataBaseName_: SystemString; DataSource: THashVariantList); overload;
    procedure FastPostCompleteBuffer(dataBaseName_: SystemString; DataSource: THashStringList); overload;
    procedure FastPostCompleteBuffer(dataBaseName_: SystemString; DataSource: TSectionTextData); overload;
{$IFNDEF FPC} procedure FastPostCompleteBuffer(dataBaseName_: SystemString; DataSource: TJsonObject); overload; virtual; {$ENDIF FPC}
    procedure FastPostCompleteBuffer(dataBaseName_: SystemString; DataSource: TPascalString); overload;

    { fast insert support }
    procedure FastInsertCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; stream: TMemoryStream64; dID: Cardinal; DoneTimeFree: Boolean); overload; virtual;
    procedure FastInsertCompleteBufferCopy(dataBaseName_: SystemString; dStorePos: Int64; stream: TCoreClassStream; dID: Cardinal);
    procedure FastInsertCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TDataFrameEngine); overload;
    procedure FastInsertCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; DataSource: THashVariantList); overload;
    procedure FastInsertCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; DataSource: THashStringList); overload;
    procedure FastInsertCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TSectionTextData); overload;
{$IFNDEF FPC} procedure FastInsertCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TJsonObject); overload; {$ENDIF FPC}
    procedure FastInsertCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TPascalString); overload;

    { fast modify support }
    procedure FastModifyCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; stream: TMemoryStream64; dID: Cardinal; DoneTimeFree: Boolean); overload; virtual;
    procedure FastModifyCompleteBufferCopy(dataBaseName_: SystemString; dStorePos: Int64; stream: TCoreClassStream; dID: Cardinal);
    procedure FastModifyCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TDataFrameEngine); overload;
    procedure FastModifyCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; DataSource: THashVariantList); overload;
    procedure FastModifyCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; DataSource: THashStringList); overload;
    procedure FastModifyCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TSectionTextData); overload;
{$IFNDEF FPC} procedure FastModifyCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TJsonObject); overload; {$ENDIF FPC}
    procedure FastModifyCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TPascalString); overload;

    procedure QueryStop(PipeName_: SystemString); virtual;
    procedure QueryPause(PipeName_: SystemString); virtual;
    procedure QueryPlay(PipeName_: SystemString); virtual;

    procedure GetDBListM(OnResult: TStreamMethod); overload;
    procedure GetDBListM(Param1: Pointer; Param2: TObject; OnResult: TStreamParamMethod); overload;
    procedure GetQueryListM(OnResult: TStreamMethod); overload;
    procedure GetQueryListM(Param1: Pointer; Param2: TObject; OnResult: TStreamParamMethod); overload;
    procedure GetQueryStateM(PipeName_: SystemString; OnResult: TStreamMethod); overload;
    procedure GetQueryStateM(PipeName_: SystemString; Param1: Pointer; Param2: TObject; OnResult: TStreamParamMethod); overload;

    procedure GetDBListP(OnResult: TStreamProc); overload;
    procedure GetDBListP(Param1: Pointer; Param2: TObject; OnResult: TStreamParamProc); overload;
    procedure GetQueryListP(OnResult: TStreamProc); overload;
    procedure GetQueryListP(Param1: Pointer; Param2: TObject; OnResult: TStreamParamProc); overload;
    procedure GetQueryStateP(PipeName_: SystemString; OnResult: TStreamProc); overload;
    procedure GetQueryStateP(PipeName_: SystemString; Param1: Pointer; Param2: TObject; OnResult: TStreamParamProc); overload;
  end;

implementation

type
  POnStorePosTransformTrigger = ^TOnStorePosTransformTrigger;

  TOnStorePosTransformTrigger = record
    Client_SendTunnel_ID: Cardinal;
    BackcallPtr: UInt64;
  end;

constructor TDataStoreService_PeerClientRecvTunnel.Create(Owner_: TPeerIO);
type
  TCipherDef = array [0 .. 4] of TCipherSecurity;
const
  c: TCipherDef = (csRC6, csSerpent, csMars, csRijndael, csTwoFish);
var
  kref: TInt64;
begin
  inherited Create(Owner_);
  FPostPerformaceCounter := 0;
  FLastPostPerformaceTime := GetTimeTick;
  FPostCounterOfPerSec := 0;

  FDataStoreCipherSecurity := c[umlRandomRange(0, 4)];

  { generate random key }
  TMISC.GenerateRandomKey(kref, C_Int64_Size);
  TCipher.GenerateKey(FDataStoreCipherSecurity, @kref, C_Int64_Size, FDataStoreCipherKey);
  FCipherInstance := CreateCipherClass(FDataStoreCipherSecurity, FDataStoreCipherKey);
  FCipherInstance.Level := 1;
  FCipherInstance.CBC := True;
  FCipherInstance.ProcessTail := True;
end;

destructor TDataStoreService_PeerClientRecvTunnel.Destroy;
begin
  DisposeObjectAndNil(FCipherInstance);
  inherited Destroy;
end;

procedure TDataStoreService_PeerClientRecvTunnel.Progress;
var
  lastTime: TTimeTick;
begin
  lastTime := GetTimeTick;

  inherited Progress;

  if lastTime - FLastPostPerformaceTime > 1000 then
    begin
      try
        if FPostPerformaceCounter > 0 then
            FPostCounterOfPerSec := FPostPerformaceCounter / ((lastTime - FLastPostPerformaceTime) * 0.001)
        else
            FPostCounterOfPerSec := 0;
      except
          FPostCounterOfPerSec := 0;
      end;
      FLastPostPerformaceTime := lastTime;
      FPostPerformaceCounter := 0;
    end;
end;

function TDataStoreService_PeerClientRecvTunnel.SendTunnelDefine: TDataStoreService_PeerClientSendTunnel;
begin
  Result := SendTunnel as TDataStoreService_PeerClientSendTunnel;
end;

procedure TDataStoreService_PeerClientRecvTunnel.EncryptBuffer(sour: Pointer; Size: NativeInt; Encrypt: Boolean);
begin
  if FCipherInstance = nil then
      exit;
  if Encrypt then
      FCipherInstance.Encrypt(sour, Size)
  else
      FCipherInstance.Decrypt(sour, Size);
end;

constructor TDataStoreService_PeerClientSendTunnel.Create(Owner_: TPeerIO);
begin
  inherited Create(Owner_);
end;

destructor TDataStoreService_PeerClientSendTunnel.Destroy;
begin
  inherited Destroy;
end;

function TDataStoreService_PeerClientSendTunnel.RecvTunnelDefine: TDataStoreService_PeerClientRecvTunnel;
begin
  Result := RecvTunnel as TDataStoreService_PeerClientRecvTunnel;
end;

procedure TDataStoreService.CreateQuery(pipe: TZDBPipeline);
var
  pl: TTDataStoreService_DBPipeline;
begin
  pl := TTDataStoreService_DBPipeline(pipe);
end;

procedure TDataStoreService.QueryFragmentData(pipe: TZDBPipeline; FragmentSource: TMemoryStream64);
var
  pl: TTDataStoreService_DBPipeline;
  DestStream: TMemoryStream64;
begin
  pl := TTDataStoreService_DBPipeline(pipe);
  if not pl.SyncToClient then
      exit;

  if not SendTunnel.Exists(pl.SendTunnel) then
      exit;

  DestStream := TMemoryStream64.Create;
  DestStream.SwapInstance(FragmentSource);

  TDataStoreService_PeerClientRecvTunnel(pl.RecvTunnel).EncryptBuffer(DestStream.Memory, DestStream.Size, True);

  ClearBatchStream(pl.SendTunnel.Owner);
  PostBatchStream(pl.SendTunnel.Owner, DestStream, True);
  Send_CompletedFragmentBigStream(pl);
  ClearBatchStream(pl.SendTunnel.Owner);
end;

procedure TDataStoreService.QueryDone(pipe: TZDBPipeline);
var
  pl: TTDataStoreService_DBPipeline;
begin
  pl := TTDataStoreService_DBPipeline(pipe);

  if not FSendTunnel.Exists(pl.SendTunnel) then
      exit;

  Send_CompletedQuery(pl);
end;

procedure TDataStoreService.StorePosTransform(const Data: Pointer; const TransformBuff: PZDBStorePosTransformArray);
var
  p: POnStorePosTransformTrigger;
  de: TDataFrameEngine;
begin
  if Data = nil then
      exit;
  p := POnStorePosTransformTrigger(Data);
  if (p^.BackcallPtr <> 0) and (FSendTunnel.Exists(p^.Client_SendTunnel_ID)) then
      Send_CompletedStorePosTransform(SendTunnel.PeerIO[p^.Client_SendTunnel_ID], p^.BackcallPtr, TransformBuff);
  Dispose(p);
end;

procedure TDataStoreService.OpenDB(ActiveDB: TZDBLMStore);
begin
end;

procedure TDataStoreService.CreateDB(ActiveDB: TZDBLMStore);
begin
end;

procedure TDataStoreService.CloseDB(ActiveDB: TZDBLMStore);
begin
end;

procedure TDataStoreService.InsertData(Sender: TZDBLMStore; InsertPos: Int64; buff: TCoreClassStream; ID: Cardinal; CompletePos: Int64);
begin
end;

procedure TDataStoreService.AddData(Sender: TZDBLMStore; buff: TCoreClassStream; ID: Cardinal; CompletePos: Int64);
begin
end;

procedure TDataStoreService.ModifyData(Sender: TZDBLMStore; const StorePos: Int64; buff: TCoreClassStream);
begin
end;

procedure TDataStoreService.DeleteData(Sender: TZDBLMStore; const StorePos: Int64);
begin
end;

procedure TDataStoreService.DownloadQueryFilterMethod(dPipe: TZDBPipeline; var qState: TQueryState; var Allowed: Boolean);
begin
  Allowed := True;
end;

procedure TDataStoreService.DownloadQueryWithIDFilterMethod(dPipe: TZDBPipeline; var qState: TQueryState; var Allowed: Boolean);
begin
  try
      Allowed := qState.ID = dPipe.UserVariant;
  except
      Allowed := False;
  end;
end;

procedure TDataStoreService.UserOut(UserDefineIO: TPeerClientUserDefineForRecvTunnel);
var
  i: Integer;
  pl: TTDataStoreService_DBPipeline;
begin
  for i := 0 to FZDBLocal.QueryPipelineList.Count - 1 do
    begin
      pl := TTDataStoreService_DBPipeline(FZDBLocal.QueryPipelineList[i]);
      if pl.RecvTunnel = UserDefineIO.Owner.UserDefine then
          pl.stop;
    end;
  inherited UserOut(UserDefineIO);
end;

procedure TDataStoreService.UserLinkSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel);
var
  RT: TDataStoreService_PeerClientRecvTunnel;
  de: TDataFrameEngine;
  arr: TDataFrameArrayByte;
begin
  RT := UserDefineIO as TDataStoreService_PeerClientRecvTunnel;
  de := TDataFrameEngine.Create;
  de.WriteByte(Byte(RT.FDataStoreCipherSecurity));
  arr := de.WriteArrayByte;
  arr.AddPtrBuff(@RT.FDataStoreCipherKey[0], length(RT.FDataStoreCipherKey));
  RT.SendTunnel.Owner.SendDirectStreamCmd(C_DataStoreSecurity, de);
  DisposeObject(de);
  inherited UserLinkSuccess(UserDefineIO);
end;

procedure TDataStoreService.Command_InitDB(Sender: TPeerIO; InData: TDataFrameEngine);
var
  RT: TDataStoreService_PeerClientRecvTunnel;
  InMem: Boolean;
  dataBaseName_: SystemString;
begin
  RT := GetDataStoreUserDefine(Sender);
  if not RT.LinkOk then
      exit;

  InMem := InData.Reader.ReadBool;
  dataBaseName_ := InData.Reader.ReadString;
  if InMem then
      FZDBLocal.InitMemoryDB(dataBaseName_)
  else
      FZDBLocal.InitDB(dataBaseName_, False);
end;

procedure TDataStoreService.Command_CloseDB(Sender: TPeerIO; InData: TDataFrameEngine);
var
  RT: TDataStoreService_PeerClientRecvTunnel;
  dataBaseName_: SystemString;
  CloseAndDeleted: Boolean;
begin
  RT := GetDataStoreUserDefine(Sender);
  if not RT.LinkOk then
      exit;

  dataBaseName_ := InData.Reader.ReadString;
  CloseAndDeleted := InData.Reader.ReadBool;

  if CloseAndDeleted then
      FZDBLocal.CloseAndDeleteDB(dataBaseName_)
  else
      FZDBLocal.CloseDB(dataBaseName_);
end;

procedure TDataStoreService.Command_CopyDB(Sender: TPeerIO; InData: TDataFrameEngine);
var
  RT: TDataStoreService_PeerClientRecvTunnel;
  dataBaseName_, copy2N: SystemString;
  BackcallPtr: UInt64;
  p: POnStorePosTransformTrigger;
begin
  RT := GetDataStoreUserDefine(Sender);
  if not RT.LinkOk then
      exit;

  dataBaseName_ := InData.Reader.ReadString;
  copy2N := InData.Reader.ReadString;
  BackcallPtr := InData.Reader.ReadPointer;

  new(p);
  p^.Client_SendTunnel_ID := RT.SendTunnelID;
  p^.BackcallPtr := BackcallPtr;
  FZDBLocal.CopyDB(dataBaseName_, copy2N, p, {$IFDEF FPC}@{$ENDIF FPC}StorePosTransform);
end;

procedure TDataStoreService.Command_CompressDB(Sender: TPeerIO; InData: TDataFrameEngine);
var
  RT: TDataStoreService_PeerClientRecvTunnel;
  dataBaseName_: SystemString;
  BackcallPtr: UInt64;
  p: POnStorePosTransformTrigger;
begin
  RT := GetDataStoreUserDefine(Sender);
  if not RT.LinkOk then
      exit;

  dataBaseName_ := InData.Reader.ReadString;
  BackcallPtr := InData.Reader.ReadPointer;

  new(p);
  p^.Client_SendTunnel_ID := RT.SendTunnelID;
  p^.BackcallPtr := BackcallPtr;
  FZDBLocal.CompressDB(dataBaseName_, p, {$IFDEF FPC}@{$ENDIF FPC}StorePosTransform);
end;

procedure TDataStoreService.Command_ReplaceDB(Sender: TPeerIO; InData: TDataFrameEngine);
var
  RT: TDataStoreService_PeerClientRecvTunnel;
  dataBaseName_, replaceN: SystemString;
begin
  RT := GetDataStoreUserDefine(Sender);
  if not RT.LinkOk then
      exit;

  dataBaseName_ := InData.Reader.ReadString;
  replaceN := InData.Reader.ReadString;
  FZDBLocal.ReplaceDB(dataBaseName_, replaceN);
end;

procedure TDataStoreService.Command_ResetData(Sender: TPeerIO; InData: TDataFrameEngine);
var
  RT: TDataStoreService_PeerClientRecvTunnel;
  dataBaseName_: SystemString;
begin
  RT := GetDataStoreUserDefine(Sender);
  if not RT.LinkOk then
      exit;

  dataBaseName_ := InData.Reader.ReadString;
  FZDBLocal.ResetData(dataBaseName_);
end;

procedure TDataStoreService.Command_QueryDB(Sender: TPeerIO; InData: TDataFrameEngine);
var
  RT: TDataStoreService_PeerClientRecvTunnel;
  RegedQueryName: SystemString;
  SyncToClient, WriteResultToOutputDB, InMem, ReverseQuery: Boolean;
  dataBaseName_, OutputDatabaseName_: SystemString;
  fragmentReponseTime, MaxWait: Double;
  MaxQueryResult: Int64;

  AutoDestoryOutputDB: Boolean;
  DelayDestoryTime: Double;
  pl: TTDataStoreService_DBPipeline;
  qc: TTDataStoreService_QueryCall;
begin
  RT := GetDataStoreUserDefine(Sender);
  if not RT.LinkOk then
      exit;

  RegedQueryName := InData.Reader.ReadString;
  SyncToClient := InData.Reader.ReadBool;
  WriteResultToOutputDB := InData.Reader.ReadBool;
  InMem := InData.Reader.ReadBool;
  ReverseQuery := InData.Reader.ReadBool;
  dataBaseName_ := InData.Reader.ReadString;
  OutputDatabaseName_ := InData.Reader.ReadString;
  fragmentReponseTime := InData.Reader.ReadDouble;
  MaxWait := InData.Reader.ReadDouble;
  MaxQueryResult := InData.Reader.ReadInt64;

  if not FZDBLocal.ExistsDB(dataBaseName_) then
      exit;

  qc := TTDataStoreService_QueryCall(FQueryCallPool[RegedQueryName]);

  if InMem then
      AutoDestoryOutputDB := True
  else
      AutoDestoryOutputDB := False;

  pl := TTDataStoreService_DBPipeline(FZDBLocal.QueryDB(WriteResultToOutputDB, InMem, ReverseQuery, dataBaseName_, OutputDatabaseName_,
    AutoDestoryOutputDB, FPerQueryPipelineDelayFreeTime, fragmentReponseTime, MaxWait, 0, MaxQueryResult));
  pl.SendTunnel := RT.SendTunnelDefine;
  pl.RecvTunnel := RT;
  pl.BackcallPtr := InData.Reader.ReadPointer;
  pl.SyncToClient := SyncToClient;
  pl.RegistedQuery := RegedQueryName;
  pl.WriteFragmentBuffer := pl.SyncToClient;

  if InData.Reader.NotEnd then
      InData.Reader.ReadVariantList(pl.values);

  if qc <> nil then
    begin
      pl.OnDataFilterMethod := qc.OnPipelineQuery;
      pl.OnDataDoneMethod := qc.OnPipelineQueryDone;
    end
  else
    begin
      pl.OnDataFilterMethod := {$IFDEF FPC}@{$ENDIF FPC}DownloadQueryFilterMethod;
    end;
  ClearBatchStream(RT.SendTunnelDefine.Owner);
end;

procedure TDataStoreService.Command_DownloadDB(Sender: TPeerIO; InData: TDataFrameEngine);
var
  RT: TDataStoreService_PeerClientRecvTunnel;
  ReverseQuery: Boolean;
  dataBaseName_: SystemString;
  pl: TTDataStoreService_DBPipeline;
begin
  RT := GetDataStoreUserDefine(Sender);
  if not RT.LinkOk then
      exit;

  ReverseQuery := InData.Reader.ReadBool;
  dataBaseName_ := InData.Reader.ReadString;

  if not FZDBLocal.ExistsDB(dataBaseName_) then
      exit;

  pl := TTDataStoreService_DBPipeline(FZDBLocal.QueryDB(False, True, ReverseQuery, dataBaseName_, '', True, FPerQueryPipelineDelayFreeTime, 0.5, 0, 0, 0));
  pl.SendTunnel := RT.SendTunnelDefine;
  pl.RecvTunnel := RT;
  pl.BackcallPtr := InData.Reader.ReadPointer;
  pl.SyncToClient := True;
  pl.WriteFragmentBuffer := pl.SyncToClient;

  pl.OnDataFilterMethod := {$IFDEF FPC}@{$ENDIF FPC}DownloadQueryFilterMethod;
  ClearBatchStream(RT.SendTunnelDefine.Owner);
end;

procedure TDataStoreService.Command_DownloadDBWithID(Sender: TPeerIO; InData: TDataFrameEngine);
var
  RT: TDataStoreService_PeerClientRecvTunnel;
  ReverseQuery: Boolean;
  dataBaseName_: SystemString;
  downloadWithID: Cardinal;
  pl: TTDataStoreService_DBPipeline;
begin
  RT := GetDataStoreUserDefine(Sender);
  if not RT.LinkOk then
      exit;

  ReverseQuery := InData.Reader.ReadBool;
  dataBaseName_ := InData.Reader.ReadString;
  downloadWithID := InData.Reader.ReadCardinal;

  if not FZDBLocal.ExistsDB(dataBaseName_) then
      exit;

  pl := TTDataStoreService_DBPipeline(FZDBLocal.QueryDB(False, True, ReverseQuery, dataBaseName_, '', True, FPerQueryPipelineDelayFreeTime, 0.5, 0, 0, 0));
  pl.SendTunnel := RT.SendTunnelDefine;
  pl.RecvTunnel := RT;
  pl.BackcallPtr := InData.Reader.ReadPointer;
  pl.SyncToClient := True;
  pl.WriteFragmentBuffer := pl.SyncToClient;

  { user download with ID }
  pl.UserVariant := downloadWithID;

  pl.OnDataFilterMethod := {$IFDEF FPC}@{$ENDIF FPC}DownloadQueryWithIDFilterMethod;
  ClearBatchStream(RT.SendTunnelDefine.Owner);
end;

procedure TDataStoreService.Command_RequestDownloadAssembleStream(Sender: TPeerIO; InData: TDataFrameEngine);
var
  RT: TDataStoreService_PeerClientRecvTunnel;
  dataBaseName_: SystemString;
  StorePos: Int64;
  BackcallPtr: UInt64;
  M: TMemoryStream64;
begin
  RT := GetDataStoreUserDefine(Sender);
  if not RT.LinkOk then
      exit;

  dataBaseName_ := InData.Reader.ReadString;
  StorePos := InData.Reader.ReadInt64;
  BackcallPtr := InData.Reader.ReadPointer;

  M := TMemoryStream64.Create;
  if not FZDBLocal.ReadDBItemToZDBFragment(dataBaseName_, StorePos, M) then
    begin
      Sender.PrintParam('get Data Assemble Stream error: %s', dataBaseName_);
      DisposeObject(M);
      exit;
    end;

  RT.EncryptBuffer(M.Memory, M.Size, True);

  ClearBatchStream(RT.SendTunnelDefine.Owner);
  PostBatchStream(RT.SendTunnelDefine.Owner, M, True);
  Send_CompletedDownloadAssemble(RT.SendTunnelDefine.Owner, dataBaseName_, StorePos, BackcallPtr);
  ClearBatchStream(RT.SendTunnelDefine.Owner);
end;

procedure TDataStoreService.Command_RequestFastDownloadAssembleStream(Sender: TPeerIO; InData: TDataFrameEngine);
var
  RT: TDataStoreService_PeerClientRecvTunnel;
  dataBaseName_: SystemString;
  StorePos: Int64;
  BackcallPtr: UInt64;
  M: TMemoryStream64;
begin
  RT := GetDataStoreUserDefine(Sender);
  if not RT.LinkOk then
      exit;

  dataBaseName_ := InData.Reader.ReadString;
  StorePos := InData.Reader.ReadInt64;
  BackcallPtr := InData.Reader.ReadPointer;

  M := TMemoryStream64.Create;
  if not FZDBLocal.ReadDBItemToZDBFragment(dataBaseName_, StorePos, M) then
    begin
      Sender.PrintParam('get Data Assemble Stream error: %s', dataBaseName_);
      DisposeObject(M);
      exit;
    end;

  ClearBatchStream(RT.SendTunnelDefine.Owner);
  PostBatchStream(RT.SendTunnelDefine.Owner, M, True);
  Send_CompletedFastDownloadAssemble(RT.SendTunnelDefine.Owner, dataBaseName_, StorePos, BackcallPtr);
  ClearBatchStream(RT.SendTunnelDefine.Owner);
end;

procedure TDataStoreService.Command_FastPostCompleteBuffer(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
var
  RT: TDataStoreService_PeerClientRecvTunnel;
  dataBaseName_: TPascalString;
  itmID: Cardinal;
  StorePos: Int64;
  output: Pointer;
  outputSiz: nativeUInt;
  m64: TMemoryStream64;
begin
  RT := GetDataStoreUserDefine(Sender);
  if not RT.LinkOk then
      exit;
  inc(RT.FPostPerformaceCounter);

  DecodeZDBBuff(InData, DataSize, dataBaseName_, itmID, StorePos, output, outputSiz);
  m64 := TMemoryStream64.Create;
  m64.SetPointerWithProtectedMode(output, outputSiz);
  FZDBLocal.PostData(dataBaseName_, m64, itmID);
  DisposeObject(m64);
end;

procedure TDataStoreService.Command_FastInsertCompleteBuffer(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
var
  RT: TDataStoreService_PeerClientRecvTunnel;
  dataBaseName_: TPascalString;
  itmID: Cardinal;
  StorePos: Int64;
  output: Pointer;
  outputSiz: nativeUInt;
  m64: TMemoryStream64;
begin
  RT := GetDataStoreUserDefine(Sender);
  if not RT.LinkOk then
      exit;
  inc(RT.FPostPerformaceCounter);

  DecodeZDBBuff(InData, DataSize, dataBaseName_, itmID, StorePos, output, outputSiz);
  m64 := TMemoryStream64.Create;
  m64.SetPointerWithProtectedMode(output, outputSiz);
  FZDBLocal.InsertData(dataBaseName_, StorePos, m64, itmID);
  DisposeObject(m64);
end;

procedure TDataStoreService.Command_FastModifyCompleteBuffer(Sender: TPeerIO; InData: PByte; DataSize: NativeInt);
var
  RT: TDataStoreService_PeerClientRecvTunnel;
  dataBaseName_: TPascalString;
  itmID: Cardinal;
  StorePos: Int64;
  output: Pointer;
  outputSiz: nativeUInt;
  m64: TMemoryStream64;
begin
  RT := GetDataStoreUserDefine(Sender);
  if not RT.LinkOk then
      exit;
  inc(RT.FPostPerformaceCounter);

  DecodeZDBBuff(InData, DataSize, dataBaseName_, itmID, StorePos, output, outputSiz);
  m64 := TMemoryStream64.Create;
  m64.SetPointerWithProtectedMode(output, outputSiz);
  FZDBLocal.SetData(dataBaseName_, StorePos, m64);
  DisposeObject(m64);
end;

procedure TDataStoreService.Command_CompletedPostAssembleStream(Sender: TPeerIO; InData: TDataFrameEngine);
var
  RT: TDataStoreService_PeerClientRecvTunnel;
  dataBaseName_: SystemString;
  dID: Cardinal;
  p: PBigStreamBatchPostData;
begin
  RT := GetDataStoreUserDefine(Sender);
  if not RT.LinkOk then
      exit;

  if RT.BigStreamBatchList.Count <= 0 then
      exit;

  dataBaseName_ := InData.Reader.ReadString;
  dID := InData.Reader.ReadCardinal;

  p := RT.BigStreamBatchList.Last;
  RT.EncryptBuffer(p^.Source.Memory, p^.Source.Size, False);
  p^.DBStorePos := FZDBLocal.PostData(dataBaseName_, p^.Source, dID);
  inc(RT.FPostPerformaceCounter);
end;

procedure TDataStoreService.Command_CompletedInsertAssembleStream(Sender: TPeerIO; InData: TDataFrameEngine);
var
  RT: TDataStoreService_PeerClientRecvTunnel;
  dataBaseName_: SystemString;
  dStorePos: Int64;
  dID: Cardinal;
  p: PBigStreamBatchPostData;
begin
  RT := GetDataStoreUserDefine(Sender);
  if not RT.LinkOk then
      exit;

  if RT.BigStreamBatchList.Count <= 0 then
      exit;

  dataBaseName_ := InData.Reader.ReadString;
  dStorePos := InData.Reader.ReadInt64;
  dID := InData.Reader.ReadCardinal;

  p := RT.BigStreamBatchList.Last;
  RT.EncryptBuffer(p^.Source.Memory, p^.Source.Size, False);
  p^.DBStorePos := FZDBLocal.InsertData(dataBaseName_, dStorePos, p^.Source, dID);
  inc(RT.FPostPerformaceCounter);
end;

procedure TDataStoreService.Command_CompletedModifyAssembleStream(Sender: TPeerIO; InData: TDataFrameEngine);
var
  RT: TDataStoreService_PeerClientRecvTunnel;
  dataBaseName_: SystemString;
  dStorePos: Int64;
  p: PBigStreamBatchPostData;
begin
  RT := GetDataStoreUserDefine(Sender);
  if not RT.LinkOk then
      exit;

  if RT.BigStreamBatchList.Count <= 0 then
      exit;

  dataBaseName_ := InData.Reader.ReadString;
  dStorePos := InData.Reader.ReadInt64;

  p := RT.BigStreamBatchList.Last;
  RT.EncryptBuffer(p^.Source.Memory, p^.Source.Size, False);

  if FZDBLocal.SetData(dataBaseName_, dStorePos, p^.Source) then
    begin
      p^.DBStorePos := dStorePos;
    end
  else
    begin
    end;
  inc(RT.FPostPerformaceCounter);
end;

procedure TDataStoreService.Command_DeleteData(Sender: TPeerIO; InData: TDataFrameEngine);
var
  RT: TDataStoreService_PeerClientRecvTunnel;
  dataBaseName_: SystemString;
  dStorePos: Int64;
begin
  RT := GetDataStoreUserDefine(Sender);
  if not RT.LinkOk then
      exit;

  dataBaseName_ := InData.Reader.ReadString;
  dStorePos := InData.Reader.ReadInt64;
  FZDBLocal.DeleteData(dataBaseName_, dStorePos);
  inc(RT.FPostPerformaceCounter);
end;

procedure TDataStoreService.Command_GetDBList(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
var
  RT: TDataStoreService_PeerClientRecvTunnel;
  lst: TCoreClassListForObj;
  i: Integer;
  Database_: TZDBLMStore;
begin
  RT := GetDataStoreUserDefine(Sender);
  if not RT.LinkOk then
      exit;

  lst := TCoreClassListForObj.Create;
  FZDBLocal.GetDBList(lst);
  for i := 0 to lst.Count - 1 do
    begin
      Database_ := TZDBLMStore(lst[i]);
      OutData.WriteString(Database_.Name);
    end;
  DisposeObject(lst);
end;

procedure TDataStoreService.Command_GetQueryList(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
var
  RT: TDataStoreService_PeerClientRecvTunnel;
  i: Integer;
  pl: TTDataStoreService_DBPipeline;
begin
  RT := GetDataStoreUserDefine(Sender);
  if not RT.LinkOk then
      exit;
  for i := 0 to FZDBLocal.QueryPipelineList.Count - 1 do
    begin
      pl := TTDataStoreService_DBPipeline(FZDBLocal.QueryPipelineList[i]);
      if (pl.RecvTunnel <> nil) and (pl.RecvTunnel.Owner = Sender) and
        (pl.Activted) and (pl.SourceDB <> nil) and (pl.OutputDB <> nil) then
          OutData.WriteString(pl.PipelineName);
    end;
end;

procedure TDataStoreService.Command_GetQueryState(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
var
  RT: TDataStoreService_PeerClientRecvTunnel;
  PipeName_: SystemString;
  pl: TTDataStoreService_DBPipeline;
  ps: TPipeState;
begin
  RT := GetDataStoreUserDefine(Sender);
  if not RT.LinkOk then
      exit;

  PipeName_ := InData.Reader.ReadString;
  if not FZDBLocal.ExistsPipeline(PipeName_) then
      exit;

  pl := TTDataStoreService_DBPipeline(FZDBLocal.PipelineN[PipeName_]);
  if pl = nil then
      exit;

  if not pl.Activted then
      exit;
  if pl.SourceDB = nil then
      exit;
  if pl.OutputDB = nil then
      exit;

  ps.Init;
  ps.WriteOutputDB := (pl.WriteResultToOutputDB);
  ps.Activted := (pl.Activted);
  ps.SyncToClient := (pl.SyncToClient);
  ps.MemoryMode := (pl.OutputDB.IsMemoryMode);
  ps.Paused := (pl.Paused);
  ps.DBCounter := (pl.SourceDB.Count);
  ps.QueryCounter := (pl.QueryCounter);
  ps.QueryResultCounter := (pl.QueryResultCounter);
  ps.MaxQueryCompare := (pl.MaxQueryCompare);
  ps.MaxQueryResult := (pl.MaxQueryResult);
  ps.QueryPerformanceOfPerSec := (pl.QueryCounterOfPerSec);
  ps.ConsumTime := (pl.QueryConsumTime);
  ps.MaxWaitTime := (pl.MaxWaitTime);
  ps.SourceDB := (pl.SourceDBName);
  ps.OutputDB := (pl.OutputDBName);
  ps.PipelineName := (pl.PipelineName);
  ps.RegistedQuery := (pl.RegistedQuery);
  ps.Encode(OutData);
  ps.Init;
end;

procedure TDataStoreService.Command_QueryStop(Sender: TPeerIO; InData: TDataFrameEngine);
var
  RT: TDataStoreService_PeerClientRecvTunnel;
  PipeName_: SystemString;
  pl: TTDataStoreService_DBPipeline;
begin
  RT := GetDataStoreUserDefine(Sender);
  if not RT.LinkOk then
      exit;

  PipeName_ := InData.Reader.ReadString;
  if not FZDBLocal.ExistsPipeline(PipeName_) then
      exit;

  pl := TTDataStoreService_DBPipeline(FZDBLocal.PipelineN[PipeName_]);
  if pl <> nil then
      pl.stop;
end;

procedure TDataStoreService.Command_QueryPause(Sender: TPeerIO; InData: TDataFrameEngine);
var
  RT: TDataStoreService_PeerClientRecvTunnel;
  PipeName_: SystemString;
  pl: TTDataStoreService_DBPipeline;
begin
  RT := GetDataStoreUserDefine(Sender);
  if not RT.LinkOk then
      exit;

  PipeName_ := InData.Reader.ReadString;
  if not FZDBLocal.ExistsPipeline(PipeName_) then
      exit;

  pl := TTDataStoreService_DBPipeline(FZDBLocal.PipelineN[PipeName_]);
  if pl <> nil then
      pl.Pause;
end;

procedure TDataStoreService.Command_QueryPlay(Sender: TPeerIO; InData: TDataFrameEngine);
var
  RT: TDataStoreService_PeerClientRecvTunnel;
  PipeName_: SystemString;
  pl: TTDataStoreService_DBPipeline;
begin
  RT := GetDataStoreUserDefine(Sender);
  if not RT.LinkOk then
      exit;

  PipeName_ := InData.Reader.ReadString;
  if not FZDBLocal.ExistsPipeline(PipeName_) then
      exit;

  pl := TTDataStoreService_DBPipeline(FZDBLocal.PipelineN[PipeName_]);
  if pl <> nil then
      pl.Play;
end;

procedure TDataStoreService.Send_CompletedFragmentBigStream(pipe: TTDataStoreService_DBPipeline);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  de.WriteString(pipe.SourceDBName);
  de.WriteString(pipe.OutputDBName);
  de.WriteString(pipe.PipelineName);
  de.WritePointer(pipe.BackcallPtr);
  pipe.SendTunnel.Owner.SendDirectStreamCmd(C_CompletedFragmentBigStream, de);
  DisposeObject(de);
end;

procedure TDataStoreService.Send_CompletedQuery(pipe: TTDataStoreService_DBPipeline);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  de.WriteString(pipe.SourceDBName);
  de.WriteString(pipe.OutputDBName);
  de.WriteString(pipe.PipelineName);
  de.WritePointer(pipe.BackcallPtr);
  de.WriteInt64(pipe.QueryResultCounter);
  pipe.SendTunnel.Owner.SendDirectStreamCmd(C_CompletedQuery, de);
  DisposeObject(de);
  ClearBatchStream(pipe.SendTunnel.Owner);
end;

procedure TDataStoreService.Send_CompletedDownloadAssemble(SendCli_: TPeerIO; dataBaseName_: SystemString; dStorePos: Int64; BackcallPtr: UInt64);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  de.WriteString(dataBaseName_);
  de.WriteInt64(dStorePos);
  de.WritePointer(BackcallPtr);
  SendCli_.SendDirectStreamCmd(C_CompletedDownloadAssemble, de);
  DisposeObject(de);
  ClearBatchStream(SendCli_);
end;

procedure TDataStoreService.Send_CompletedFastDownloadAssemble(SendCli_: TPeerIO; dataBaseName_: SystemString; dStorePos: Int64; BackcallPtr: UInt64);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  de.WriteString(dataBaseName_);
  de.WriteInt64(dStorePos);
  de.WritePointer(BackcallPtr);
  SendCli_.SendDirectStreamCmd(C_CompletedFastDownloadAssemble, de);
  DisposeObject(de);
  ClearBatchStream(SendCli_);
end;

procedure TDataStoreService.Send_CompletedStorePosTransform(SendCli_: TPeerIO; const BackcallPtr: UInt64; const TransformBuff: PZDBStorePosTransformArray);
var
  de: TDataFrameEngine;
  i: Integer;
  arr: TDataFrameArrayInt64;
begin
  de := TDataFrameEngine.Create;
  de.WritePointer(BackcallPtr);

  arr := de.WriteArrayInt64;
  for i := 0 to length(TransformBuff^) - 1 do
      arr.Add(TransformBuff^[i].OriginPos);

  arr := de.WriteArrayInt64;
  for i := 0 to length(TransformBuff^) - 1 do
      arr.Add(TransformBuff^[i].NewPos);

  SendCli_.SendDirectStreamCmd(C_CompletedStorePosTransform, de);
  DisposeObject(de);
end;

constructor TDataStoreService.Create(RecvTunnel_, SendTunnel_: TCommunicationFrameworkServer);
begin
  inherited Create(RecvTunnel_, SendTunnel_);
  FRecvTunnel.PeerClientUserDefineClass := TDataStoreService_PeerClientRecvTunnel;
  FSendTunnel.PeerClientUserDefineClass := TDataStoreService_PeerClientSendTunnel;

  FZDBLocal := TZDBLocalManager.Create;
  FZDBLocal.PipelineClass := TTDataStoreService_DBPipeline;
  FZDBLocal.NotifyIntf := Self;

  FQueryCallPool := THashObjectList.Create(True);

  FPerQueryPipelineDelayFreeTime := 3.0;
end;

destructor TDataStoreService.Destroy;
begin
  DisposeObject([FZDBLocal, FQueryCallPool]);
  inherited Destroy;
end;

procedure TDataStoreService.RegisterCommand;
begin
  inherited RegisterCommand;

  FRecvTunnel.RegisterDirectStream(C_InitDB).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_InitDB;
  FRecvTunnel.RegisterDirectStream(C_CloseDB).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_CloseDB;

  FRecvTunnel.RegisterDirectStream(C_CopyDB).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_CopyDB;
  FRecvTunnel.RegisterDirectStream(C_CompressDB).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_CompressDB;
  FRecvTunnel.RegisterDirectStream(C_ReplaceDB).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_ReplaceDB;
  FRecvTunnel.RegisterDirectStream(C_ResetData).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_ResetData;

  FRecvTunnel.RegisterDirectStream(C_QueryDB).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_QueryDB;
  FRecvTunnel.RegisterDirectStream(C_DownloadDB).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_DownloadDB;
  FRecvTunnel.RegisterDirectStream(C_DownloadDBWithID).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_DownloadDBWithID;
  FRecvTunnel.RegisterDirectStream(C_RequestDownloadAssembleStream).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_RequestDownloadAssembleStream;
  FRecvTunnel.RegisterDirectStream(C_RequestFastDownloadAssembleStrea).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_RequestFastDownloadAssembleStream;

  FRecvTunnel.RegisterCompleteBuffer(C_FastPostCompleteBuffer).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_FastPostCompleteBuffer;
  FRecvTunnel.RegisterCompleteBuffer(C_FastInsertCompleteBuffer).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_FastInsertCompleteBuffer;
  FRecvTunnel.RegisterCompleteBuffer(C_FastModifyCompleteBuffer).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_FastModifyCompleteBuffer;

  FRecvTunnel.RegisterDirectStream(C_CompletedPostAssembleStream).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_CompletedPostAssembleStream;
  FRecvTunnel.RegisterDirectStream(C_CompletedInsertAssembleStream).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_CompletedInsertAssembleStream;
  FRecvTunnel.RegisterDirectStream(C_CompletedModifyAssembleStream).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_CompletedModifyAssembleStream;
  FRecvTunnel.RegisterDirectStream(C_DeleteData).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_DeleteData;

  FRecvTunnel.RegisterStream(C_GetDBList).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_GetDBList;
  FRecvTunnel.RegisterStream(C_GetQueryList).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_GetQueryList;
  FRecvTunnel.RegisterStream(C_GetQueryState).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_GetQueryState;
  FRecvTunnel.RegisterDirectStream(C_QueryStop).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_QueryStop;
  FRecvTunnel.RegisterDirectStream(C_QueryPause).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_QueryPause;
  FRecvTunnel.RegisterDirectStream(C_QueryPlay).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_QueryPlay;
end;

procedure TDataStoreService.UnRegisterCommand;
begin
  inherited UnRegisterCommand;
  FRecvTunnel.DeleteRegistedCMD(C_InitDB);
  FRecvTunnel.DeleteRegistedCMD(C_CloseDB);

  FRecvTunnel.DeleteRegistedCMD(C_CopyDB);
  FRecvTunnel.DeleteRegistedCMD(C_CompressDB);
  FRecvTunnel.DeleteRegistedCMD(C_ReplaceDB);
  FRecvTunnel.DeleteRegistedCMD(C_ResetData);

  FRecvTunnel.DeleteRegistedCMD(C_QueryDB);
  FRecvTunnel.DeleteRegistedCMD(C_DownloadDB);
  FRecvTunnel.DeleteRegistedCMD(C_RequestDownloadAssembleStream);

  FRecvTunnel.DeleteRegistedCMD(C_FastPostCompleteBuffer);
  FRecvTunnel.DeleteRegistedCMD(C_FastInsertCompleteBuffer);
  FRecvTunnel.DeleteRegistedCMD(C_FastModifyCompleteBuffer);

  FRecvTunnel.DeleteRegistedCMD(C_CompletedPostAssembleStream);
  FRecvTunnel.DeleteRegistedCMD(C_CompletedInsertAssembleStream);
  FRecvTunnel.DeleteRegistedCMD(C_CompletedModifyAssembleStream);
  FRecvTunnel.DeleteRegistedCMD(C_DeleteData);

  FRecvTunnel.DeleteRegistedCMD(C_GetDBList);
  FRecvTunnel.DeleteRegistedCMD(C_GetQueryList);
  FRecvTunnel.DeleteRegistedCMD(C_GetQueryState);
  FRecvTunnel.DeleteRegistedCMD(C_QueryStop);
  FRecvTunnel.DeleteRegistedCMD(C_QueryPause);
  FRecvTunnel.DeleteRegistedCMD(C_QueryPlay);
end;

procedure TDataStoreService.Progress;
begin
  inherited Progress;
  FZDBLocal.Progress;
end;

procedure TDataStoreService.CadencerProgress(Sender: TObject; const deltaTime, newTime: Double);
begin
  inherited CadencerProgress(Sender, deltaTime, newTime);
end;

function TDataStoreService.GetDataStoreUserDefine(RecvCli: TPeerIO): TDataStoreService_PeerClientRecvTunnel;
begin
  Result := RecvCli.UserDefine as TDataStoreService_PeerClientRecvTunnel;
end;

function TDataStoreService.RegisterQueryCall(QuerierName_: SystemString): TTDataStoreService_QueryCall;
begin
  if FQueryCallPool.Exists(QuerierName_) then
      RaiseInfo('Query call already registed:%s', [QuerierName_]);

  Result := TTDataStoreService_QueryCall.Create;
  FQueryCallPool[QuerierName_] := Result;
  DoStatus('Query Register info: "%s"', [QuerierName_]);
end;

procedure TDataStoreService.UnRegisterQueryCall(QuerierName_: SystemString);
begin
  if not FQueryCallPool.Exists(QuerierName_) then
      RaiseInfo('Query call not registed:%s', [QuerierName_]);

  FQueryCallPool.Delete(QuerierName_);
  DoStatus('Query UnRegister: "%s"', [QuerierName_]);
end;

function TDataStoreService.GetRegistedQueryCall(QuerierName_: SystemString): TTDataStoreService_QueryCall;
begin
  Result := TTDataStoreService_QueryCall(FQueryCallPool[QuerierName_]);
end;

function TDataStoreService.PostCounterOfPerSec: Double;
var
  IO_Array: TIO_Array;
  pcid: Cardinal;
  RT: TDataStoreService_PeerClientRecvTunnel;
begin
  Result := 0;
  FRecvTunnel.GetIO_Array(IO_Array);
  for pcid in IO_Array do
    begin
      RT := GetDataStoreUserDefine(FRecvTunnel.PeerIO[pcid]);
      Result := Result + RT.FPostCounterOfPerSec;
    end;
end;

procedure TDataStoreClient.EncryptBuffer(sour: Pointer; Size: NativeInt; Encrypt: Boolean);
begin
  if FCipherInstance = nil then
      exit;
  if Encrypt then
      FCipherInstance.Encrypt(sour, Size)
  else
      FCipherInstance.Decrypt(sour, Size);
end;

procedure TDataStoreClient.Command_DataStoreSecurity(Sender: TPeerIO; InData: TDataFrameEngine);
var
  arr: TDataFrameArrayByte;
begin
  FDataStoreCipherSecurity := TCipherSecurity(InData.Reader.ReadByte);
  arr := InData.Reader.ReadArrayByte;
  SetLength(FDataStoreCipherKey, arr.Count);
  arr.GetBuff(@FDataStoreCipherKey[0]);

  DisposeObjectAndNil(FCipherInstance);
  FCipherInstance := CreateCipherClass(FDataStoreCipherSecurity, FDataStoreCipherKey);
  FCipherInstance.Level := 1;
  FCipherInstance.CBC := True;
  FCipherInstance.ProcessTail := True;
end;

procedure TDataStoreClient.Command_CompletedFragmentBigStream(Sender: TPeerIO; InData: TDataFrameEngine);
var
  dataBaseName_, OutputDatabaseName_, PipeName_: SystemString;
  BackcallPtr: PDataStoreClientQueryNotify;
  M: TMemoryStream64;
begin
  dataBaseName_ := InData.Reader.ReadString;
  OutputDatabaseName_ := InData.Reader.ReadString;
  PipeName_ := InData.Reader.ReadString;
  BackcallPtr := PDataStoreClientQueryNotify(InData.Reader.ReadPointer);

  M := TMemoryStream64.Create;

  if Sender.UserDefine.BigStreamBatchList.Count > 0 then
    begin
      Sender.UserDefine.BigStreamBatchList.Last^.Source.Position := 0;
      EncryptBuffer(Sender.UserDefine.BigStreamBatchList.Last^.Source.Memory, Sender.UserDefine.BigStreamBatchList.Last^.Source.Size, False);
      Sender.UserDefine.BigStreamBatchList.Last^.Source.Position := 0;
      M.SwapInstance(Sender.UserDefine.BigStreamBatchList.Last^.Source);
      Sender.UserDefine.BigStreamBatchList.DeleteLast;
    end;

  if (BackcallPtr <> nil) and (M.Size > 0) then
    begin
      try
        M.Position := 0;
        if Assigned(BackcallPtr^.OnUserQueryCall) then
          begin
            FillFragmentSourceC(BackcallPtr^.UserPointer, BackcallPtr^.UserObject, BackcallPtr^.UserVariant, dataBaseName_, PipeName_, M, BackcallPtr^.OnUserQueryCall);
            M.Position := 0;
          end;
        if Assigned(BackcallPtr^.OnUserQueryMethod) then
          begin
            FillFragmentSourceM(BackcallPtr^.UserPointer, BackcallPtr^.UserObject, BackcallPtr^.UserVariant, dataBaseName_, PipeName_, M, BackcallPtr^.OnUserQueryMethod);
            M.Position := 0;
          end;
        if Assigned(BackcallPtr^.OnUserQueryProc) then
          begin
            FillFragmentSourceP(BackcallPtr^.UserPointer, BackcallPtr^.UserObject, BackcallPtr^.UserVariant, dataBaseName_, PipeName_, M, BackcallPtr^.OnUserQueryProc);
            M.Position := 0;
          end;
        if Assigned(BackcallPtr^.OnQueryCall) then
          begin
            FillFragmentSourceC(dataBaseName_, PipeName_, M, BackcallPtr^.OnQueryCall);
            M.Position := 0;
          end;
        if Assigned(BackcallPtr^.OnQueryMethod) then
          begin
            FillFragmentSourceM(dataBaseName_, PipeName_, M, BackcallPtr^.OnQueryMethod);
            M.Position := 0;
          end;
        if Assigned(BackcallPtr^.OnQueryProc) then
          begin
            FillFragmentSourceP(dataBaseName_, PipeName_, M, BackcallPtr^.OnQueryProc);
            M.Position := 0;
          end;
      except
      end;
    end;

  DisposeObject(M);
end;

procedure TDataStoreClient.Command_CompletedQuery(Sender: TPeerIO; InData: TDataFrameEngine);
var
  dataBaseName_, OutputDatabaseName_, PipeName_: SystemString;
  BackcallPtr: PDataStoreClientQueryNotify;
  TotalResultCount: Int64;
begin
  dataBaseName_ := InData.Reader.ReadString;
  OutputDatabaseName_ := InData.Reader.ReadString;
  PipeName_ := InData.Reader.ReadString;
  BackcallPtr := PDataStoreClientQueryNotify(InData.Reader.ReadPointer);
  TotalResultCount := InData.Reader.ReadInt64;

  if BackcallPtr <> nil then
    begin
      try
        if Assigned(BackcallPtr^.OnUserDoneCall) then
            BackcallPtr^.OnUserDoneCall(BackcallPtr^.UserPointer, BackcallPtr^.UserObject, BackcallPtr^.UserVariant, dataBaseName_, OutputDatabaseName_, PipeName_, TotalResultCount);
        if Assigned(BackcallPtr^.OnUserDoneMethod) then
            BackcallPtr^.OnUserDoneMethod(BackcallPtr^.UserPointer, BackcallPtr^.UserObject, BackcallPtr^.UserVariant, dataBaseName_, OutputDatabaseName_, PipeName_, TotalResultCount);
        if Assigned(BackcallPtr^.OnUserDoneProc) then
            BackcallPtr^.OnUserDoneProc(BackcallPtr^.UserPointer, BackcallPtr^.UserObject, BackcallPtr^.UserVariant, dataBaseName_, OutputDatabaseName_, PipeName_, TotalResultCount);

        if Assigned(BackcallPtr^.OnDoneCall) then
            BackcallPtr^.OnDoneCall(dataBaseName_, OutputDatabaseName_, PipeName_, TotalResultCount);
        if Assigned(BackcallPtr^.OnDoneMethod) then
            BackcallPtr^.OnDoneMethod(dataBaseName_, OutputDatabaseName_, PipeName_, TotalResultCount);
        if Assigned(BackcallPtr^.OnDoneProc) then
            BackcallPtr^.OnDoneProc(dataBaseName_, OutputDatabaseName_, PipeName_, TotalResultCount);
      except
      end;
      Dispose(BackcallPtr);
    end;
  Sender.UserDefine.BigStreamBatchList.Clear;
end;

procedure TDataStoreClient.Command_CompletedDownloadAssemble(Sender: TPeerIO; InData: TDataFrameEngine);
var
  dataBaseName_: SystemString;
  dStorePos: Int64;
  BackcallPtr: PDataStoreClientDownloadNotify;
  M, tmp: TMemoryStream64;
begin
  dataBaseName_ := InData.Reader.ReadString;
  dStorePos := InData.Reader.ReadInt64;
  BackcallPtr := PDataStoreClientDownloadNotify(InData.Reader.ReadPointer);

  if Sender.UserDefine.BigStreamBatchList.Count > 0 then
      M := Sender.UserDefine.BigStreamBatchList.Last^.Source
  else
      M := nil;

  if M <> nil then
    begin
      if BackcallPtr <> nil then
        begin
          EncryptBuffer(M.Memory, M.Size, False);

          if BackcallPtr^.AutoDecodeZDBStream then
              tmp := DecodeZDBFragment(M)
          else
              tmp := M;

          try
            tmp.Position := 0;
            if Assigned(BackcallPtr^.OnUserDoneCall) then
              begin
                BackcallPtr^.OnUserDoneCall(BackcallPtr^.UserPointer, BackcallPtr^.UserObject, BackcallPtr^.UserVariant, dataBaseName_, dStorePos, tmp);
                tmp.Position := 0;
              end;
            if Assigned(BackcallPtr^.OnUserDoneMethod) then
              begin
                BackcallPtr^.OnUserDoneMethod(BackcallPtr^.UserPointer, BackcallPtr^.UserObject, BackcallPtr^.UserVariant, dataBaseName_, dStorePos, tmp);
                tmp.Position := 0;
              end;
            if Assigned(BackcallPtr^.OnUserDoneProc) then
              begin
                BackcallPtr^.OnUserDoneProc(BackcallPtr^.UserPointer, BackcallPtr^.UserObject, BackcallPtr^.UserVariant, dataBaseName_, dStorePos, tmp);
                tmp.Position := 0;
              end;

            if Assigned(BackcallPtr^.OnDoneCall) then
              begin
                BackcallPtr^.OnDoneCall(dataBaseName_, dStorePos, tmp);
                tmp.Position := 0;
              end;
            if Assigned(BackcallPtr^.OnDoneMethod) then
              begin
                BackcallPtr^.OnDoneMethod(dataBaseName_, dStorePos, tmp);
                tmp.Position := 0;
              end;
            if Assigned(BackcallPtr^.OnDoneProc) then
              begin
                BackcallPtr^.OnDoneProc(dataBaseName_, dStorePos, tmp);
                tmp.Position := 0;
              end;
          except
          end;

          if BackcallPtr^.AutoDecodeZDBStream then
              DisposeObject(tmp);
          Dispose(BackcallPtr);
        end;
      Sender.UserDefine.BigStreamBatchList.DeleteLast;
    end;
end;

procedure TDataStoreClient.Command_CompletedFastDownloadAssemble(Sender: TPeerIO; InData: TDataFrameEngine);
var
  dataBaseName_: SystemString;
  dStorePos: Int64;
  BackcallPtr: PDataStoreClientDownloadNotify;
  M, tmp: TMemoryStream64;
begin
  dataBaseName_ := InData.Reader.ReadString;
  dStorePos := InData.Reader.ReadInt64;
  BackcallPtr := PDataStoreClientDownloadNotify(InData.Reader.ReadPointer);

  if Sender.UserDefine.BigStreamBatchList.Count > 0 then
      M := Sender.UserDefine.BigStreamBatchList.Last^.Source
  else
      M := nil;

  if M <> nil then
    begin
      if BackcallPtr <> nil then
        begin
          if BackcallPtr^.AutoDecodeZDBStream then
              tmp := DecodeZDBFragment(M)
          else
              tmp := M;

          try
            tmp.Position := 0;
            if Assigned(BackcallPtr^.OnUserDoneCall) then
              begin
                BackcallPtr^.OnUserDoneCall(BackcallPtr^.UserPointer, BackcallPtr^.UserObject, BackcallPtr^.UserVariant, dataBaseName_, dStorePos, tmp);
                tmp.Position := 0;
              end;
            if Assigned(BackcallPtr^.OnUserDoneMethod) then
              begin
                BackcallPtr^.OnUserDoneMethod(BackcallPtr^.UserPointer, BackcallPtr^.UserObject, BackcallPtr^.UserVariant, dataBaseName_, dStorePos, tmp);
                tmp.Position := 0;
              end;
            if Assigned(BackcallPtr^.OnUserDoneProc) then
              begin
                BackcallPtr^.OnUserDoneProc(BackcallPtr^.UserPointer, BackcallPtr^.UserObject, BackcallPtr^.UserVariant, dataBaseName_, dStorePos, tmp);
                tmp.Position := 0;
              end;

            if Assigned(BackcallPtr^.OnDoneCall) then
              begin
                BackcallPtr^.OnDoneCall(dataBaseName_, dStorePos, tmp);
                tmp.Position := 0;
              end;
            if Assigned(BackcallPtr^.OnDoneMethod) then
              begin
                BackcallPtr^.OnDoneMethod(dataBaseName_, dStorePos, tmp);
                tmp.Position := 0;
              end;
            if Assigned(BackcallPtr^.OnDoneProc) then
              begin
                BackcallPtr^.OnDoneProc(dataBaseName_, dStorePos, tmp);
                tmp.Position := 0;
              end;
          except
          end;

          if BackcallPtr^.AutoDecodeZDBStream then
              DisposeObject(tmp);
          Dispose(BackcallPtr);
        end;
      Sender.UserDefine.BigStreamBatchList.DeleteLast;
    end;
end;

procedure TDataStoreClient.Command_CompletedStorePosTransform(Sender: TPeerIO; InData: TDataFrameEngine);
var
  BackcallPtr: PStorePosTransformNotify;
  arr: TDataFrameArrayInt64;
  i: Integer;
  TransformBuff: TZDBStorePosTransformArray;
begin
  BackcallPtr := PStorePosTransformNotify(InData.Reader.ReadPointer);

  arr := InData.Reader.ReadArrayInt64;
  SetLength(TransformBuff, arr.Count);
  for i := 0 to arr.Count - 1 do
      TransformBuff[i].OriginPos := arr[i];

  arr := InData.Reader.ReadArrayInt64;
  for i := 0 to arr.Count - 1 do
      TransformBuff[i].NewPos := arr[i];

  if BackcallPtr <> nil then
    begin
      if Assigned(BackcallPtr^.OnDoneCall) then
          BackcallPtr^.OnDoneCall(@TransformBuff);
      if Assigned(BackcallPtr^.OnDoneMethod) then
          BackcallPtr^.OnDoneMethod(@TransformBuff);
      if Assigned(BackcallPtr^.OnDoneProc) then
          BackcallPtr^.OnDoneProc(@TransformBuff);
    end;

  SetLength(TransformBuff, 0);
  Dispose(BackcallPtr);
end;

constructor TDataStoreClient.Create(RecvTunnel_, SendTunnel_: TCommunicationFrameworkClient);
begin
  inherited Create(RecvTunnel_, SendTunnel_);
  FDataStoreCipherSecurity := TCipherSecurity.csNone;
  SetLength(FDataStoreCipherKey, 0);
  FCipherInstance := nil;
end;

destructor TDataStoreClient.Destroy;
begin
  DisposeObjectAndNil(FCipherInstance);
  inherited Destroy;
end;

procedure TDataStoreClient.RegisterCommand;
begin
  inherited RegisterCommand;
  FRecvTunnel.RegisterDirectStream(C_DataStoreSecurity).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_DataStoreSecurity;
  FRecvTunnel.RegisterDirectStream(C_CompletedFragmentBigStream).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_CompletedFragmentBigStream;
  FRecvTunnel.RegisterDirectStream(C_CompletedQuery).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_CompletedQuery;
  FRecvTunnel.RegisterDirectStream(C_CompletedDownloadAssemble).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_CompletedDownloadAssemble;
  FRecvTunnel.RegisterDirectStream(C_CompletedFastDownloadAssemble).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_CompletedFastDownloadAssemble;
  FRecvTunnel.RegisterDirectStream(C_CompletedStorePosTransform).OnExecute := {$IFDEF FPC}@{$ENDIF FPC}Command_CompletedStorePosTransform;
end;

procedure TDataStoreClient.UnRegisterCommand;
begin
  inherited UnRegisterCommand;
  FRecvTunnel.DeleteRegistedCMD(C_CompletedFragmentBigStream);
  FRecvTunnel.DeleteRegistedCMD(C_CompletedQuery);
  FRecvTunnel.DeleteRegistedCMD(C_CompletedDownloadAssemble);
  FRecvTunnel.DeleteRegistedCMD(C_CompletedFastDownloadAssemble);
  FRecvTunnel.DeleteRegistedCMD(C_CompletedStorePosTransform);
end;

procedure TDataStoreClient.Progress;
begin
  inherited Progress;
end;

function TDataStoreClient.DataCipherKeyFinished: Boolean;
begin
  Result := (length(FDataStoreCipherKey) > 0) or (Self.FDataStoreCipherSecurity <> csNone);
end;

procedure TDataStoreClient.InitDB(InMem: Boolean; dataBaseName_: SystemString);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;

  de.WriteBool(InMem);
  de.WriteString(dataBaseName_);

  SendTunnel.SendDirectStreamCmd(C_InitDB, de);
  DisposeObject(de);
end;

procedure TDataStoreClient.CloseDB(dataBaseName_: SystemString; CloseAndDeleted: Boolean);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  de.WriteString(dataBaseName_);
  de.WriteBool(CloseAndDeleted);
  SendTunnel.SendDirectStreamCmd(C_CloseDB, de);
  DisposeObject(de);
end;

procedure TDataStoreClient.CopyDB(dataBaseName_, CopyDestDatabaseName_: SystemString);
begin
  CopyDB(dataBaseName_, CopyDestDatabaseName_, nil);
end;

procedure TDataStoreClient.CopyDB(dataBaseName_, CopyDestDatabaseName_: SystemString; const BackcallPtr: PStorePosTransformNotify);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  de.WriteString(dataBaseName_);
  de.WriteString(CopyDestDatabaseName_);
  de.WritePointer(BackcallPtr);
  SendTunnel.SendDirectStreamCmd(C_CopyDB, de);
  DisposeObject(de);
end;

procedure TDataStoreClient.CopyDB_C(dataBaseName_, CopyDestDatabaseName_: SystemString; const OnDoneCall: TStorePosTransformNotifyCall);
var
  p: PStorePosTransformNotify;
begin
  new(p);
  p^.Init;
  p^.OnDoneCall := OnDoneCall;
  CopyDB(dataBaseName_, CopyDestDatabaseName_, p);
end;

procedure TDataStoreClient.CopyDB_M(dataBaseName_, CopyDestDatabaseName_: SystemString; const OnDoneMethod: TStorePosTransformNotifyMethod);
var
  p: PStorePosTransformNotify;
begin
  new(p);
  p^.Init;
  p^.OnDoneMethod := OnDoneMethod;
  CopyDB(dataBaseName_, CopyDestDatabaseName_, p);
end;

procedure TDataStoreClient.CopyDB_P(dataBaseName_, CopyDestDatabaseName_: SystemString; const OnDoneProc: TStorePosTransformNotifyProc);
var
  p: PStorePosTransformNotify;
begin
  new(p);
  p^.Init;
  p^.OnDoneProc := OnDoneProc;
  CopyDB(dataBaseName_, CopyDestDatabaseName_, p);
end;

procedure TDataStoreClient.CompressDB(dataBaseName_: SystemString);
begin
  CompressDB(dataBaseName_, nil);
end;

procedure TDataStoreClient.CompressDB(dataBaseName_: SystemString; const BackcallPtr: PStorePosTransformNotify);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  de.WriteString(dataBaseName_);
  de.WritePointer(BackcallPtr);
  SendTunnel.SendDirectStreamCmd(C_CompressDB, de);
  DisposeObject(de);
end;

procedure TDataStoreClient.CompressDB_C(dataBaseName_: SystemString; const OnDoneCall: TStorePosTransformNotifyCall);
var
  p: PStorePosTransformNotify;
begin
  new(p);
  p^.Init;
  p^.OnDoneCall := OnDoneCall;
  CompressDB(dataBaseName_, p);
end;

procedure TDataStoreClient.CompressDB_M(dataBaseName_: SystemString; const OnDoneMethod: TStorePosTransformNotifyMethod);
var
  p: PStorePosTransformNotify;
begin
  new(p);
  p^.Init;
  p^.OnDoneMethod := OnDoneMethod;
  CompressDB(dataBaseName_, p);
end;

procedure TDataStoreClient.CompressDB_P(dataBaseName_: SystemString; const OnDoneProc: TStorePosTransformNotifyProc);
var
  p: PStorePosTransformNotify;
begin
  new(p);
  p^.Init;
  p^.OnDoneProc := OnDoneProc;
  CompressDB(dataBaseName_, p);
end;

procedure TDataStoreClient.ReplaceDB(dataBaseName_, replaceN: SystemString);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  de.WriteString(dataBaseName_);
  de.WriteString(replaceN);
  SendTunnel.SendDirectStreamCmd(C_ReplaceDB, de);
  DisposeObject(de);
end;

procedure TDataStoreClient.ResetData(dataBaseName_: SystemString);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  de.WriteString(dataBaseName_);
  SendTunnel.SendDirectStreamCmd(C_ResetData, de);
  DisposeObject(de);
end;

procedure TDataStoreClient.QuietQueryDB(RegistedQuerier_: SystemString; ReverseQuery: Boolean; dataBaseName_, OutputDatabaseName_: SystemString; MaxWait: Double; MaxQueryResult: Int64);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;

  de.WriteString(RegistedQuerier_);
  de.WriteBool(False); { sync to client }
  de.WriteBool(True);  { write output Database_ }
  de.WriteBool(False); { in memory }
  de.WriteBool(ReverseQuery);
  de.WriteString(dataBaseName_);
  de.WriteString(OutputDatabaseName_);
  de.WriteDouble(0.1); { fragmentReponseTime }
  de.WriteDouble(MaxWait);
  de.WriteInt64(MaxQueryResult);
  de.WritePointer(0); { backcall address }

  SendTunnel.SendDirectStreamCmd(C_QueryDB, de);

  DisposeObject(de);
end;

procedure TDataStoreClient.QueryDB(RegistedQuerier_: SystemString; SyncToClient, WriteResultToOutputDB, InMem, ReverseQuery: Boolean; dataBaseName_, OutputDatabaseName_: SystemString;
  fragmentReponseTime, MaxWait: Double; MaxQueryResult: Int64; BackcallPtr: PDataStoreClientQueryNotify; RemoteParams: THashVariantList);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;

  de.WriteString(RegistedQuerier_);
  de.WriteBool(SyncToClient); { sync to client }
  de.WriteBool(WriteResultToOutputDB);
  de.WriteBool(InMem);
  de.WriteBool(ReverseQuery);
  de.WriteString(dataBaseName_);
  de.WriteString(OutputDatabaseName_);
  de.WriteDouble(fragmentReponseTime);
  de.WriteDouble(MaxWait);
  de.WriteInt64(MaxQueryResult);
  de.WritePointer(BackcallPtr);
  if RemoteParams <> nil then
      de.WriteVariantList(RemoteParams);

  SendTunnel.SendDirectStreamCmd(C_QueryDB, de);

  DisposeObject(de);
end;

procedure TDataStoreClient.QueryDBC(RegistedQuerier_: SystemString; SyncToClient, WriteResultToOutputDB, InMem, ReverseQuery: Boolean; dataBaseName_, OutputDatabaseName_: SystemString;
  fragmentReponseTime, MaxWait: Double; MaxQueryResult: Int64;
  RemoteParams: THashVariantList; OnQueryCall: TFillQueryDataCall; OnDoneCall: TQueryDoneNotifyCall);
var
  p: PDataStoreClientQueryNotify;
begin
  new(p);
  p^.Init;
  p^.OnQueryCall := OnQueryCall;
  p^.OnDoneCall := OnDoneCall;
  QueryDB(RegistedQuerier_, SyncToClient, WriteResultToOutputDB, InMem, ReverseQuery, dataBaseName_, OutputDatabaseName_, fragmentReponseTime, MaxWait, MaxQueryResult, p, RemoteParams);
end;

procedure TDataStoreClient.QueryDBC(RegistedQuerier_: SystemString; SyncToClient, WriteResultToOutputDB, InMem, ReverseQuery: Boolean; dataBaseName_, OutputDatabaseName_: SystemString;
  fragmentReponseTime, MaxWait: Double; MaxQueryResult: Int64;
  RemoteParams: THashVariantList;                                           { service ref remote parameter }
  UserPointer: Pointer; UserObject: TCoreClassObject; UserVariant: Variant; { local event parameter }
  OnQueryCall: TUserFillQueryDataCall; OnDoneCall: TUserQueryDoneNotifyCall);
var
  p: PDataStoreClientQueryNotify;
begin
  new(p);
  p^.Init;
  p^.UserPointer := UserPointer;
  p^.UserObject := UserObject;
  p^.UserVariant := UserVariant;
  p^.OnUserQueryCall := OnQueryCall;
  p^.OnUserDoneCall := OnDoneCall;
  QueryDB(RegistedQuerier_, SyncToClient, WriteResultToOutputDB, InMem, ReverseQuery, dataBaseName_, OutputDatabaseName_, fragmentReponseTime, MaxWait, MaxQueryResult, p, RemoteParams);
end;

procedure TDataStoreClient.QueryDBM(RegistedQuerier_: SystemString; SyncToClient, WriteResultToOutputDB, InMem, ReverseQuery: Boolean; dataBaseName_, OutputDatabaseName_: SystemString;
  fragmentReponseTime, MaxWait: Double; MaxQueryResult: Int64;
  RemoteParams: THashVariantList; OnQueryMethod: TFillQueryDataMethod; OnDoneMethod: TQueryDoneNotifyMethod);
var
  p: PDataStoreClientQueryNotify;
begin
  new(p);
  p^.Init;
  p^.OnQueryMethod := OnQueryMethod;
  p^.OnDoneMethod := OnDoneMethod;
  QueryDB(RegistedQuerier_, SyncToClient, WriteResultToOutputDB, InMem, ReverseQuery, dataBaseName_, OutputDatabaseName_, fragmentReponseTime, MaxWait, MaxQueryResult, p, RemoteParams);
end;

procedure TDataStoreClient.QueryDBM(RegistedQuerier_: SystemString; SyncToClient, WriteResultToOutputDB, InMem, ReverseQuery: Boolean; dataBaseName_, OutputDatabaseName_: SystemString;
  fragmentReponseTime, MaxWait: Double; MaxQueryResult: Int64;
  RemoteParams: THashVariantList;                                           { service ref remote parameter }
  UserPointer: Pointer; UserObject: TCoreClassObject; UserVariant: Variant; { local event parameter }
  OnQueryMethod: TUserFillQueryDataMethod; OnDoneMethod: TUserQueryDoneNotifyMethod);
var
  p: PDataStoreClientQueryNotify;
begin
  new(p);
  p^.Init;
  p^.UserPointer := UserPointer;
  p^.UserObject := UserObject;
  p^.UserVariant := UserVariant;
  p^.OnUserQueryMethod := OnQueryMethod;
  p^.OnUserDoneMethod := OnDoneMethod;
  QueryDB(RegistedQuerier_, SyncToClient, WriteResultToOutputDB, InMem, ReverseQuery, dataBaseName_, OutputDatabaseName_, fragmentReponseTime, MaxWait, MaxQueryResult, p, RemoteParams);
end;

procedure TDataStoreClient.QueryDBP(RegistedQuerier_: SystemString; SyncToClient, WriteResultToOutputDB, InMem, ReverseQuery: Boolean; dataBaseName_, OutputDatabaseName_: SystemString;
  fragmentReponseTime, MaxWait: Double; MaxQueryResult: Int64;
  RemoteParams: THashVariantList; OnQueryProc: TFillQueryDataProc; OnDoneProc: TQueryDoneNotifyProc);
var
  p: PDataStoreClientQueryNotify;
begin
  new(p);
  p^.Init;
  p^.OnQueryProc := OnQueryProc;
  p^.OnDoneProc := OnDoneProc;
  QueryDB(RegistedQuerier_, SyncToClient, WriteResultToOutputDB, InMem, ReverseQuery, dataBaseName_, OutputDatabaseName_, fragmentReponseTime, MaxWait, MaxQueryResult, p, RemoteParams);
end;

procedure TDataStoreClient.QueryDBP(RegistedQuerier_: SystemString; SyncToClient, WriteResultToOutputDB, InMem, ReverseQuery: Boolean; dataBaseName_, OutputDatabaseName_: SystemString;
  fragmentReponseTime, MaxWait: Double; MaxQueryResult: Int64;
  RemoteParams: THashVariantList;                                           { service ref remote parameter }
  UserPointer: Pointer; UserObject: TCoreClassObject; UserVariant: Variant; { local event parameter }
  OnQueryProc: TUserFillQueryDataProc; OnDoneProc: TUserQueryDoneNotifyProc);
var
  p: PDataStoreClientQueryNotify;
begin
  new(p);
  p^.Init;
  p^.UserPointer := UserPointer;
  p^.UserObject := UserObject;
  p^.UserVariant := UserVariant;
  p^.OnUserQueryProc := OnQueryProc;
  p^.OnUserDoneProc := OnDoneProc;
  QueryDB(RegistedQuerier_, SyncToClient, WriteResultToOutputDB, InMem, ReverseQuery, dataBaseName_, OutputDatabaseName_, fragmentReponseTime, MaxWait, MaxQueryResult, p, RemoteParams);
end;

procedure TDataStoreClient.QueryDBC(RegistedQuerier_: SystemString; dataBaseName_: SystemString; RemoteParams: THashVariantList; OnQueryCall: TFillQueryDataCall; OnDoneCall: TQueryDoneNotifyCall);
var
  p: PDataStoreClientQueryNotify;
begin
  new(p);
  p^.Init;
  p^.OnQueryCall := OnQueryCall;
  p^.OnDoneCall := OnDoneCall;
  QueryDB(RegistedQuerier_, True, False, True, False, dataBaseName_, 'Memory', 0.5, 0, 0, p, RemoteParams);
end;

procedure TDataStoreClient.QueryDBM(RegistedQuerier_: SystemString; dataBaseName_: SystemString; RemoteParams: THashVariantList; OnQueryMethod: TFillQueryDataMethod; OnDoneMethod: TQueryDoneNotifyMethod);
var
  p: PDataStoreClientQueryNotify;
begin
  new(p);
  p^.Init;
  p^.OnQueryMethod := OnQueryMethod;
  p^.OnDoneMethod := OnDoneMethod;
  QueryDB(RegistedQuerier_, True, False, True, False, dataBaseName_, 'Memory', 0.5, 0, 0, p, RemoteParams);
end;

procedure TDataStoreClient.QueryDBP(RegistedQuerier_: SystemString; dataBaseName_: SystemString; RemoteParams: THashVariantList; OnQueryProc: TFillQueryDataProc; OnDoneProc: TQueryDoneNotifyProc);
var
  p: PDataStoreClientQueryNotify;
begin
  new(p);
  p^.Init;
  p^.OnQueryProc := OnQueryProc;
  p^.OnDoneProc := OnDoneProc;
  QueryDB(RegistedQuerier_, True, False, True, False, dataBaseName_, 'Memory', 0.5, 0, 0, p, RemoteParams);
end;

procedure TDataStoreClient.DownloadDB(ReverseQuery: Boolean; dataBaseName_: SystemString; BackcallPtr: PDataStoreClientQueryNotify);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;

  de.WriteBool(ReverseQuery);
  de.WriteString(dataBaseName_);
  de.WritePointer(BackcallPtr);

  SendTunnel.SendDirectStreamCmd(C_DownloadDB, de);

  DisposeObject(de);
end;

procedure TDataStoreClient.DownloadDBC(ReverseQuery: Boolean; dataBaseName_: SystemString; OnQueryCall: TFillQueryDataCall; OnDoneCall: TQueryDoneNotifyCall);
var
  p: PDataStoreClientQueryNotify;
begin
  new(p);
  p^.Init;
  p^.OnQueryCall := OnQueryCall;
  p^.OnDoneCall := OnDoneCall;
  DownloadDB(ReverseQuery, dataBaseName_, p);
end;

procedure TDataStoreClient.DownloadDBM(ReverseQuery: Boolean; dataBaseName_: SystemString; OnQueryMethod: TFillQueryDataMethod; OnDoneMethod: TQueryDoneNotifyMethod);
var
  p: PDataStoreClientQueryNotify;
begin
  new(p);
  p^.Init;
  p^.OnQueryMethod := OnQueryMethod;
  p^.OnDoneMethod := OnDoneMethod;
  DownloadDB(ReverseQuery, dataBaseName_, p);
end;

procedure TDataStoreClient.DownloadDBP(ReverseQuery: Boolean; dataBaseName_: SystemString; OnQueryProc: TFillQueryDataProc; OnDoneProc: TQueryDoneNotifyProc);
var
  p: PDataStoreClientQueryNotify;
begin
  new(p);
  p^.Init;
  p^.OnQueryProc := OnQueryProc;
  p^.OnDoneProc := OnDoneProc;
  DownloadDB(ReverseQuery, dataBaseName_, p);
end;

procedure TDataStoreClient.DownloadDBWithID(ReverseQuery: Boolean; dataBaseName_: SystemString; db_ID: Cardinal; BackcallPtr: PDataStoreClientQueryNotify);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;

  de.WriteBool(ReverseQuery);
  de.WriteString(dataBaseName_);
  de.WriteCardinal(db_ID);
  de.WritePointer(BackcallPtr);

  SendTunnel.SendDirectStreamCmd(C_DownloadDBWithID, de);

  DisposeObject(de);
end;

procedure TDataStoreClient.DownloadDBWithIDC(ReverseQuery: Boolean; dataBaseName_: SystemString; db_ID: Cardinal; OnQueryCall: TFillQueryDataCall; OnDoneCall: TQueryDoneNotifyCall);
var
  p: PDataStoreClientQueryNotify;
begin
  new(p);
  p^.Init;
  p^.OnQueryCall := OnQueryCall;
  p^.OnDoneCall := OnDoneCall;
  DownloadDBWithID(ReverseQuery, dataBaseName_, db_ID, p);
end;

procedure TDataStoreClient.DownloadDBWithIDM(ReverseQuery: Boolean; dataBaseName_: SystemString; db_ID: Cardinal; OnQueryMethod: TFillQueryDataMethod; OnDoneMethod: TQueryDoneNotifyMethod);
var
  p: PDataStoreClientQueryNotify;
begin
  new(p);
  p^.Init;
  p^.OnQueryMethod := OnQueryMethod;
  p^.OnDoneMethod := OnDoneMethod;
  DownloadDBWithID(ReverseQuery, dataBaseName_, db_ID, p);
end;

procedure TDataStoreClient.DownloadDBWithIDP(ReverseQuery: Boolean; dataBaseName_: SystemString; db_ID: Cardinal; OnQueryProc: TFillQueryDataProc; OnDoneProc: TQueryDoneNotifyProc);
var
  p: PDataStoreClientQueryNotify;
begin
  new(p);
  p^.Init;
  p^.OnQueryProc := OnQueryProc;
  p^.OnDoneProc := OnDoneProc;
  DownloadDBWithID(ReverseQuery, dataBaseName_, db_ID, p);
end;

procedure TDataStoreClient.BeginAssembleStream;
begin
  ClearBatchStream;
end;

procedure TDataStoreClient.RequestDownloadAssembleStream(dataBaseName_: SystemString; StorePos: Int64; BackcallPtr: PDataStoreClientDownloadNotify);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;

  de.WriteString(dataBaseName_);
  de.WriteInt64(StorePos);
  de.WritePointer(BackcallPtr);

  SendTunnel.SendDirectStreamCmd(C_RequestDownloadAssembleStream, de);

  DisposeObject(de);
end;

procedure TDataStoreClient.DownloadAssembleStreamC(dataBaseName_: SystemString; StorePos: Int64; OnDoneCall: TDownloadDoneNotifyCall);
var
  p: PDataStoreClientDownloadNotify;
begin
  new(p);
  p^.Init;
  p^.OnDoneCall := OnDoneCall;

  RequestDownloadAssembleStream(dataBaseName_, StorePos, p);
end;

procedure TDataStoreClient.DownloadAssembleStreamM(dataBaseName_: SystemString; StorePos: Int64; OnDoneMethod: TDownloadDoneNotifyMethod);
var
  p: PDataStoreClientDownloadNotify;
begin
  new(p);
  p^.Init;
  p^.OnDoneMethod := OnDoneMethod;

  RequestDownloadAssembleStream(dataBaseName_, StorePos, p);
end;

procedure TDataStoreClient.DownloadAssembleStreamP(dataBaseName_: SystemString; StorePos: Int64; OnDoneProc: TDownloadDoneNotifyProc);
var
  p: PDataStoreClientDownloadNotify;
begin
  new(p);
  p^.Init;
  p^.OnDoneProc := OnDoneProc;

  RequestDownloadAssembleStream(dataBaseName_, StorePos, p);
end;

procedure TDataStoreClient.DownloadAssembleStreamC(dataBaseName_: SystemString; StorePos: Int64; AutoDecodeZDBStream: Boolean; OnDoneCall: TDownloadDoneNotifyCall);
var
  p: PDataStoreClientDownloadNotify;
begin
  new(p);
  p^.Init;
  p^.OnDoneCall := OnDoneCall;
  p^.AutoDecodeZDBStream := AutoDecodeZDBStream;

  RequestDownloadAssembleStream(dataBaseName_, StorePos, p);
end;

procedure TDataStoreClient.DownloadAssembleStreamM(dataBaseName_: SystemString; StorePos: Int64; AutoDecodeZDBStream: Boolean; OnDoneMethod: TDownloadDoneNotifyMethod);
var
  p: PDataStoreClientDownloadNotify;
begin
  new(p);
  p^.Init;
  p^.OnDoneMethod := OnDoneMethod;
  p^.AutoDecodeZDBStream := AutoDecodeZDBStream;

  RequestDownloadAssembleStream(dataBaseName_, StorePos, p);
end;

procedure TDataStoreClient.DownloadAssembleStreamP(dataBaseName_: SystemString; StorePos: Int64; AutoDecodeZDBStream: Boolean; OnDoneProc: TDownloadDoneNotifyProc);
var
  p: PDataStoreClientDownloadNotify;
begin
  new(p);
  p^.Init;
  p^.OnDoneProc := OnDoneProc;
  p^.AutoDecodeZDBStream := AutoDecodeZDBStream;

  RequestDownloadAssembleStream(dataBaseName_, StorePos, p);
end;

procedure TDataStoreClient.DownloadAssembleStreamC(dataBaseName_: SystemString; StorePos: Int64;
  UserPointer: Pointer; UserObject: TCoreClassObject; UserVariant: Variant; { local event parameter }
  OnDoneCall: TUserDownloadDoneNotifyCall);
var
  p: PDataStoreClientDownloadNotify;
begin
  new(p);
  p^.Init;
  p^.UserPointer := UserPointer;
  p^.UserObject := UserObject;
  p^.UserVariant := UserVariant;
  p^.OnUserDoneCall := OnDoneCall;

  RequestDownloadAssembleStream(dataBaseName_, StorePos, p);
end;

procedure TDataStoreClient.DownloadAssembleStreamM(dataBaseName_: SystemString; StorePos: Int64;
  UserPointer: Pointer; UserObject: TCoreClassObject; UserVariant: Variant; { local event parameter }
  OnDoneMethod: TUserDownloadDoneNotifyMethod);
var
  p: PDataStoreClientDownloadNotify;
begin
  new(p);
  p^.Init;
  p^.UserPointer := UserPointer;
  p^.UserObject := UserObject;
  p^.UserVariant := UserVariant;
  p^.OnUserDoneMethod := OnDoneMethod;

  RequestDownloadAssembleStream(dataBaseName_, StorePos, p);
end;

procedure TDataStoreClient.DownloadAssembleStreamP(dataBaseName_: SystemString; StorePos: Int64;
  UserPointer: Pointer; UserObject: TCoreClassObject; UserVariant: Variant; { local event parameter }
  OnDoneProc: TUserDownloadDoneNotifyProc);
var
  p: PDataStoreClientDownloadNotify;
begin
  new(p);
  p^.Init;
  p^.UserPointer := UserPointer;
  p^.UserObject := UserObject;
  p^.UserVariant := UserVariant;
  p^.OnUserDoneProc := OnDoneProc;

  RequestDownloadAssembleStream(dataBaseName_, StorePos, p);
end;

procedure TDataStoreClient.DownloadAssembleStreamC(dataBaseName_: SystemString; StorePos: Int64; AutoDecodeZDBStream: Boolean;
  UserPointer: Pointer; UserObject: TCoreClassObject; UserVariant: Variant; { local event parameter }
  OnDoneCall: TUserDownloadDoneNotifyCall);
var
  p: PDataStoreClientDownloadNotify;
begin
  new(p);
  p^.Init;
  p^.UserPointer := UserPointer;
  p^.UserObject := UserObject;
  p^.UserVariant := UserVariant;
  p^.OnUserDoneCall := OnDoneCall;
  p^.AutoDecodeZDBStream := AutoDecodeZDBStream;

  RequestDownloadAssembleStream(dataBaseName_, StorePos, p);
end;

procedure TDataStoreClient.DownloadAssembleStreamM(dataBaseName_: SystemString; StorePos: Int64; AutoDecodeZDBStream: Boolean;
  UserPointer: Pointer; UserObject: TCoreClassObject; UserVariant: Variant; { local event parameter }
  OnDoneMethod: TUserDownloadDoneNotifyMethod);
var
  p: PDataStoreClientDownloadNotify;
begin
  new(p);
  p^.Init;
  p^.UserPointer := UserPointer;
  p^.UserObject := UserObject;
  p^.UserVariant := UserVariant;
  p^.OnUserDoneMethod := OnDoneMethod;
  p^.AutoDecodeZDBStream := AutoDecodeZDBStream;

  RequestDownloadAssembleStream(dataBaseName_, StorePos, p);
end;

procedure TDataStoreClient.DownloadAssembleStreamP(dataBaseName_: SystemString; StorePos: Int64; AutoDecodeZDBStream: Boolean;
  UserPointer: Pointer; UserObject: TCoreClassObject; UserVariant: Variant; { local event parameter }
  OnDoneProc: TUserDownloadDoneNotifyProc);
var
  p: PDataStoreClientDownloadNotify;
begin
  new(p);
  p^.Init;
  p^.UserPointer := UserPointer;
  p^.UserObject := UserObject;
  p^.UserVariant := UserVariant;
  p^.OnUserDoneProc := OnDoneProc;
  p^.AutoDecodeZDBStream := AutoDecodeZDBStream;

  RequestDownloadAssembleStream(dataBaseName_, StorePos, p);
end;

procedure TDataStoreClient.RequestFastDownloadAssembleStream(dataBaseName_: SystemString; StorePos: Int64; BackcallPtr: PDataStoreClientDownloadNotify);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;

  de.WriteString(dataBaseName_);
  de.WriteInt64(StorePos);
  de.WritePointer(BackcallPtr);

  SendTunnel.SendDirectStreamCmd(C_RequestFastDownloadAssembleStrea, de);

  DisposeObject(de);
end;

procedure TDataStoreClient.FastDownloadAssembleStreamC(dataBaseName_: SystemString; StorePos: Int64; OnDoneCall: TDownloadDoneNotifyCall);
var
  p: PDataStoreClientDownloadNotify;
begin
  new(p);
  p^.Init;
  p^.OnDoneCall := OnDoneCall;

  RequestFastDownloadAssembleStream(dataBaseName_, StorePos, p);
end;

procedure TDataStoreClient.FastDownloadAssembleStreamM(dataBaseName_: SystemString; StorePos: Int64; OnDoneMethod: TDownloadDoneNotifyMethod);
var
  p: PDataStoreClientDownloadNotify;
begin
  new(p);
  p^.Init;
  p^.OnDoneMethod := OnDoneMethod;

  RequestFastDownloadAssembleStream(dataBaseName_, StorePos, p);
end;

procedure TDataStoreClient.FastDownloadAssembleStreamP(dataBaseName_: SystemString; StorePos: Int64; OnDoneProc: TDownloadDoneNotifyProc);
var
  p: PDataStoreClientDownloadNotify;
begin
  new(p);
  p^.Init;
  p^.OnDoneProc := OnDoneProc;

  RequestFastDownloadAssembleStream(dataBaseName_, StorePos, p);
end;

procedure TDataStoreClient.FastDownloadAssembleStreamC(dataBaseName_: SystemString; StorePos: Int64; AutoDecodeZDBStream: Boolean; OnDoneCall: TDownloadDoneNotifyCall);
var
  p: PDataStoreClientDownloadNotify;
begin
  new(p);
  p^.Init;
  p^.OnDoneCall := OnDoneCall;
  p^.AutoDecodeZDBStream := AutoDecodeZDBStream;

  RequestFastDownloadAssembleStream(dataBaseName_, StorePos, p);
end;

procedure TDataStoreClient.FastDownloadAssembleStreamM(dataBaseName_: SystemString; StorePos: Int64; AutoDecodeZDBStream: Boolean; OnDoneMethod: TDownloadDoneNotifyMethod);
var
  p: PDataStoreClientDownloadNotify;
begin
  new(p);
  p^.Init;
  p^.OnDoneMethod := OnDoneMethod;
  p^.AutoDecodeZDBStream := AutoDecodeZDBStream;

  RequestFastDownloadAssembleStream(dataBaseName_, StorePos, p);
end;

procedure TDataStoreClient.FastDownloadAssembleStreamP(dataBaseName_: SystemString; StorePos: Int64; AutoDecodeZDBStream: Boolean; OnDoneProc: TDownloadDoneNotifyProc);
var
  p: PDataStoreClientDownloadNotify;
begin
  new(p);
  p^.Init;
  p^.OnDoneProc := OnDoneProc;
  p^.AutoDecodeZDBStream := AutoDecodeZDBStream;

  RequestFastDownloadAssembleStream(dataBaseName_, StorePos, p);
end;

procedure TDataStoreClient.FastDownloadAssembleStreamC(dataBaseName_: SystemString; StorePos: Int64;
  UserPointer: Pointer; UserObject: TCoreClassObject; UserVariant: Variant; { local event parameter }
  OnDoneCall: TUserDownloadDoneNotifyCall);
var
  p: PDataStoreClientDownloadNotify;
begin
  new(p);
  p^.Init;
  p^.UserPointer := UserPointer;
  p^.UserObject := UserObject;
  p^.UserVariant := UserVariant;
  p^.OnUserDoneCall := OnDoneCall;

  RequestFastDownloadAssembleStream(dataBaseName_, StorePos, p);
end;

procedure TDataStoreClient.FastDownloadAssembleStreamM(dataBaseName_: SystemString; StorePos: Int64;
  UserPointer: Pointer; UserObject: TCoreClassObject; UserVariant: Variant; { local event parameter }
  OnDoneMethod: TUserDownloadDoneNotifyMethod);
var
  p: PDataStoreClientDownloadNotify;
begin
  new(p);
  p^.Init;
  p^.UserPointer := UserPointer;
  p^.UserObject := UserObject;
  p^.UserVariant := UserVariant;
  p^.OnUserDoneMethod := OnDoneMethod;

  RequestFastDownloadAssembleStream(dataBaseName_, StorePos, p);
end;

procedure TDataStoreClient.FastDownloadAssembleStreamP(dataBaseName_: SystemString; StorePos: Int64;
  UserPointer: Pointer; UserObject: TCoreClassObject; UserVariant: Variant; { local event parameter }
  OnDoneProc: TUserDownloadDoneNotifyProc);
var
  p: PDataStoreClientDownloadNotify;
begin
  new(p);
  p^.Init;
  p^.UserPointer := UserPointer;
  p^.UserObject := UserObject;
  p^.UserVariant := UserVariant;
  p^.OnUserDoneProc := OnDoneProc;

  RequestFastDownloadAssembleStream(dataBaseName_, StorePos, p);
end;

procedure TDataStoreClient.FastDownloadAssembleStreamC(dataBaseName_: SystemString; StorePos: Int64; AutoDecodeZDBStream: Boolean;
  UserPointer: Pointer; UserObject: TCoreClassObject; UserVariant: Variant; { local event parameter }
  OnDoneCall: TUserDownloadDoneNotifyCall);
var
  p: PDataStoreClientDownloadNotify;
begin
  new(p);
  p^.Init;
  p^.UserPointer := UserPointer;
  p^.UserObject := UserObject;
  p^.UserVariant := UserVariant;
  p^.OnUserDoneCall := OnDoneCall;
  p^.AutoDecodeZDBStream := AutoDecodeZDBStream;

  RequestFastDownloadAssembleStream(dataBaseName_, StorePos, p);
end;

procedure TDataStoreClient.FastDownloadAssembleStreamM(dataBaseName_: SystemString; StorePos: Int64; AutoDecodeZDBStream: Boolean;
  UserPointer: Pointer; UserObject: TCoreClassObject; UserVariant: Variant; { local event parameter }
  OnDoneMethod: TUserDownloadDoneNotifyMethod);
var
  p: PDataStoreClientDownloadNotify;
begin
  new(p);
  p^.Init;
  p^.UserPointer := UserPointer;
  p^.UserObject := UserObject;
  p^.UserVariant := UserVariant;
  p^.OnUserDoneMethod := OnDoneMethod;
  p^.AutoDecodeZDBStream := AutoDecodeZDBStream;

  RequestFastDownloadAssembleStream(dataBaseName_, StorePos, p);
end;

procedure TDataStoreClient.FastDownloadAssembleStreamP(dataBaseName_: SystemString; StorePos: Int64; AutoDecodeZDBStream: Boolean;
  UserPointer: Pointer; UserObject: TCoreClassObject; UserVariant: Variant; { local event parameter }
  OnDoneProc: TUserDownloadDoneNotifyProc);
var
  p: PDataStoreClientDownloadNotify;
begin
  new(p);
  p^.Init;
  p^.UserPointer := UserPointer;
  p^.UserObject := UserObject;
  p^.UserVariant := UserVariant;
  p^.OnUserDoneProc := OnDoneProc;
  p^.AutoDecodeZDBStream := AutoDecodeZDBStream;

  RequestFastDownloadAssembleStream(dataBaseName_, StorePos, p);
end;

procedure TDataStoreClient.PostAssembleStream(dataBaseName_: SystemString; stream: TMemoryStream64; dID: Cardinal; DoneTimeFree: Boolean);
var
  de: TDataFrameEngine;
begin
  if stream.Size = 0 then
    begin
      SendTunnel.Error('PostAssembleStream Error: stream size is 0');
      exit;
    end;
  EncryptBuffer(stream.Memory, stream.Size, True);
  PostBatchStream(stream, DoneTimeFree);

  de := TDataFrameEngine.Create;
  de.WriteString(dataBaseName_);
  de.WriteCardinal(dID);
  SendTunnel.SendDirectStreamCmd(C_CompletedPostAssembleStream, de);
  DisposeObject(de);
end;

procedure TDataStoreClient.PostAssembleStreamCopy(dataBaseName_: SystemString; stream: TCoreClassStream; dID: Cardinal);
var
  M: TMemoryStream64;
begin
  M := TMemoryStream64.Create;
  stream.Position := 0;
  M.CopyFrom(stream, stream.Size);
  M.Position := 0;
  PostAssembleStream(dataBaseName_, M, dID, True);
end;

procedure TDataStoreClient.PostAssembleStream(dataBaseName_: SystemString; DataSource: TDataFrameEngine);
var
  M: TMemoryStream64;
begin
  M := TMemoryStream64.Create;
  DataSource.EncodeTo(M, True);
  PostAssembleStream(dataBaseName_, M, c_DF, True);
end;

procedure TDataStoreClient.PostAssembleStream(dataBaseName_: SystemString; DataSource: THashVariantList);
var
  M: TMemoryStream64;
begin
  M := TMemoryStream64.Create;
  DataSource.SaveToStream(M);
  PostAssembleStream(dataBaseName_, M, c_VL, True);
end;

procedure TDataStoreClient.PostAssembleStream(dataBaseName_: SystemString; DataSource: THashStringList);
var
  M: TMemoryStream64;
begin
  M := TMemoryStream64.Create;
  DataSource.SaveToStream(M);
  PostAssembleStream(dataBaseName_, M, c_VT, True);
end;

procedure TDataStoreClient.PostAssembleStream(dataBaseName_: SystemString; DataSource: TSectionTextData);
var
  M: TMemoryStream64;
begin
  M := TMemoryStream64.Create;
  DataSource.SaveToStream(M);
  PostAssembleStream(dataBaseName_, M, c_TE, True);
end;

{$IFNDEF FPC}


procedure TDataStoreClient.PostAssembleStream(dataBaseName_: SystemString; DataSource: TJsonObject);
var
  M: TMemoryStream64;
begin
  M := TMemoryStream64.Create;
  DataSource.SaveToStream(M);
  PostAssembleStream(dataBaseName_, M, c_Json, True);
end;
{$ENDIF FPC}


procedure TDataStoreClient.PostAssembleStream(dataBaseName_: SystemString; DataSource: TPascalString);
var
  M: TMemoryStream64;
begin
  M := TMemoryStream64.Create;
  TDBEnginePascalString.SavePascalStringToStream(@DataSource, M);
  PostAssembleStream(dataBaseName_, M, c_PascalString, True);
end;

procedure TDataStoreClient.InsertAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; stream: TMemoryStream64; dID: Cardinal; DoneTimeFree: Boolean);
var
  de: TDataFrameEngine;
begin
  if stream.Size = 0 then
    begin
      SendTunnel.Error('InsertAssembleStream Error: stream size is 0');
      exit;
    end;
  EncryptBuffer(stream.Memory, stream.Size, True);
  PostBatchStream(stream, DoneTimeFree);

  de := TDataFrameEngine.Create;
  de.WriteString(dataBaseName_);
  de.WriteInt64(dStorePos);
  de.WriteCardinal(dID);
  SendTunnel.SendDirectStreamCmd(C_CompletedInsertAssembleStream, de);
  DisposeObject(de);
end;

procedure TDataStoreClient.InsertAssembleStreamCopy(dataBaseName_: SystemString; dStorePos: Int64; stream: TCoreClassStream; dID: Cardinal);
var
  M: TMemoryStream64;
begin
  M := TMemoryStream64.Create;
  stream.Position := 0;
  M.CopyFrom(stream, stream.Size);
  M.Position := 0;
  InsertAssembleStream(dataBaseName_, dStorePos, M, dID, True);
end;

procedure TDataStoreClient.InsertAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TDataFrameEngine);
var
  M: TMemoryStream64;
begin
  M := TMemoryStream64.Create;
  DataSource.EncodeTo(M, True);
  InsertAssembleStream(dataBaseName_, dStorePos, M, c_DF, True);
end;

procedure TDataStoreClient.InsertAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; DataSource: THashVariantList);
var
  M: TMemoryStream64;
begin
  M := TMemoryStream64.Create;
  DataSource.SaveToStream(M);
  InsertAssembleStream(dataBaseName_, dStorePos, M, c_VL, True);
end;

procedure TDataStoreClient.InsertAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; DataSource: THashStringList);
var
  M: TMemoryStream64;
begin
  M := TMemoryStream64.Create;
  DataSource.SaveToStream(M);
  InsertAssembleStream(dataBaseName_, dStorePos, M, c_VT, True);
end;

procedure TDataStoreClient.InsertAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TSectionTextData);
var
  M: TMemoryStream64;
begin
  M := TMemoryStream64.Create;
  DataSource.SaveToStream(M);
  InsertAssembleStream(dataBaseName_, dStorePos, M, c_TE, True);
end;

{$IFNDEF FPC}


procedure TDataStoreClient.InsertAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TJsonObject);
var
  M: TMemoryStream64;
begin
  M := TMemoryStream64.Create;
  DataSource.SaveToStream(M, False, TEncoding.UTF8, True);
  InsertAssembleStream(dataBaseName_, dStorePos, M, c_Json, True);
end;
{$ENDIF FPC}


procedure TDataStoreClient.InsertAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TPascalString);
var
  M: TMemoryStream64;
begin
  M := TMemoryStream64.Create;
  TDBEnginePascalString.SavePascalStringToStream(@DataSource, M);
  InsertAssembleStream(dataBaseName_, dStorePos, M, c_PascalString, True);
end;

procedure TDataStoreClient.ModifyAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; stream: TMemoryStream64; DoneTimeFree: Boolean);
var
  de: TDataFrameEngine;
begin
  if stream.Size = 0 then
    begin
      SendTunnel.Error('ModifyAssembleStream Error: stream size is 0');
      exit;
    end;
  EncryptBuffer(stream.Memory, stream.Size, True);
  PostBatchStream(stream, DoneTimeFree);

  de := TDataFrameEngine.Create;
  de.WriteString(dataBaseName_);
  de.WriteInt64(dStorePos);
  SendTunnel.SendDirectStreamCmd(C_CompletedModifyAssembleStream, de);
  DisposeObject(de);
end;

procedure TDataStoreClient.ModifyAssembleStreamCopy(dataBaseName_: SystemString; dStorePos: Int64; stream: TCoreClassStream);
var
  M: TMemoryStream64;
begin
  M := TMemoryStream64.Create;
  stream.Position := 0;
  M.CopyFrom(stream, stream.Size);
  M.Position := 0;
  ModifyAssembleStream(dataBaseName_, dStorePos, M, True);
end;

procedure TDataStoreClient.ModifyAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TDataFrameEngine);
var
  M: TMemoryStream64;
begin
  M := TMemoryStream64.Create;
  DataSource.EncodeTo(M, True);
  ModifyAssembleStream(dataBaseName_, dStorePos, M, True);
end;

procedure TDataStoreClient.ModifyAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; DataSource: THashVariantList);
var
  M: TMemoryStream64;
begin
  M := TMemoryStream64.Create;
  DataSource.SaveToStream(M);
  ModifyAssembleStream(dataBaseName_, dStorePos, M, True);
end;

procedure TDataStoreClient.ModifyAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; DataSource: THashStringList);
var
  M: TMemoryStream64;
begin
  M := TMemoryStream64.Create;
  DataSource.SaveToStream(M);
  ModifyAssembleStream(dataBaseName_, dStorePos, M, True);
end;

procedure TDataStoreClient.ModifyAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TSectionTextData);
var
  M: TMemoryStream64;
begin
  M := TMemoryStream64.Create;
  DataSource.SaveToStream(M);
  ModifyAssembleStream(dataBaseName_, dStorePos, M, True);
end;

{$IFNDEF FPC}


procedure TDataStoreClient.ModifyAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TJsonObject);
var
  M: TMemoryStream64;
begin
  M := TMemoryStream64.Create;
  DataSource.SaveToStream(M, False, TEncoding.UTF8, True);
  ModifyAssembleStream(dataBaseName_, dStorePos, M, True);
end;
{$ENDIF FPC}


procedure TDataStoreClient.ModifyAssembleStream(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TPascalString);
var
  M: TMemoryStream64;
begin
  M := TMemoryStream64.Create;
  TDBEnginePascalString.SavePascalStringToStream(@DataSource, M);
  ModifyAssembleStream(dataBaseName_, dStorePos, M, True);
end;

procedure TDataStoreClient.GetPostAssembleStreamStateM(OnResult: TStreamMethod);
begin
  GetBatchStreamStateM(OnResult);
end;

procedure TDataStoreClient.GetPostAssembleStreamStateM(Param1: Pointer; Param2: TObject; OnResult: TStreamParamMethod);
begin
  GetBatchStreamStateM(Param1, Param2, OnResult);
end;

procedure TDataStoreClient.GetPostAssembleStreamStateP(OnResult: TStreamProc);
begin
  GetBatchStreamStateP(OnResult);
end;

procedure TDataStoreClient.GetPostAssembleStreamStateP(Param1: Pointer; Param2: TObject; OnResult: TStreamParamProc);
begin
  GetBatchStreamStateP(Param1, Param2, OnResult);
end;

procedure TDataStoreClient.EndAssembleStream;
begin
  ClearBatchStream;
end;

procedure TDataStoreClient.DeleteData(dataBaseName_: SystemString; dStorePos: Int64);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  de.WriteString(dataBaseName_);
  de.WriteInt64(dStorePos);
  SendTunnel.SendDirectStreamCmd(C_DeleteData, de);
  DisposeObject(de);
end;

procedure TDataStoreClient.FastPostCompleteBuffer(dataBaseName_: SystemString; stream: TMemoryStream64; dID: Cardinal; DoneTimeFree: Boolean);
var
  p: Pointer;
  siz: nativeUInt;
begin
  if stream.Size = 0 then
    begin
      SendTunnel.Error('FastPostCompleteBuffer Error: stream size is 0');
      exit;
    end;
  p := EncodeZDBBuff(dataBaseName_, dID, 0, stream.Memory, stream.Size, siz);
  SendTunnel.SendCompleteBuffer(C_FastPostCompleteBuffer, p, siz, True);

  if DoneTimeFree then
      DisposeObject(stream);
end;

procedure TDataStoreClient.FastPostCompleteBufferCopy(dataBaseName_: SystemString; stream: TCoreClassStream; dID: Cardinal);
var
  M: TMemoryStream64;
begin
  M := TMemoryStream64.Create;
  stream.Position := 0;
  M.CopyFrom(stream, stream.Size);
  M.Position := 0;
  FastPostCompleteBuffer(dataBaseName_, M, dID, True);
end;

procedure TDataStoreClient.FastPostCompleteBuffer(dataBaseName_: SystemString; DataSource: TDataFrameEngine);
var
  M: TMemoryStream64;
begin
  M := TMemoryStream64.Create;
  DataSource.EncodeTo(M, True);
  FastPostCompleteBuffer(dataBaseName_, M, c_DF, True);
end;

procedure TDataStoreClient.FastPostCompleteBuffer(dataBaseName_: SystemString; DataSource: THashVariantList);
var
  M: TMemoryStream64;
begin
  M := TMemoryStream64.Create;
  DataSource.SaveToStream(M);
  FastPostCompleteBuffer(dataBaseName_, M, c_VL, True);
end;

procedure TDataStoreClient.FastPostCompleteBuffer(dataBaseName_: SystemString; DataSource: THashStringList);
var
  M: TMemoryStream64;
begin
  M := TMemoryStream64.Create;
  DataSource.SaveToStream(M);
  FastPostCompleteBuffer(dataBaseName_, M, c_VT, True);
end;

procedure TDataStoreClient.FastPostCompleteBuffer(dataBaseName_: SystemString; DataSource: TSectionTextData);
var
  M: TMemoryStream64;
begin
  M := TMemoryStream64.Create;
  DataSource.SaveToStream(M);
  FastPostCompleteBuffer(dataBaseName_, M, c_TE, True);
end;

{$IFNDEF FPC}


procedure TDataStoreClient.FastPostCompleteBuffer(dataBaseName_: SystemString; DataSource: TJsonObject);
var
  M: TMemoryStream64;
begin
  M := TMemoryStream64.Create;
  DataSource.SaveToStream(M);
  FastPostCompleteBuffer(dataBaseName_, M, c_Json, True);
end;
{$ENDIF FPC}


procedure TDataStoreClient.FastPostCompleteBuffer(dataBaseName_: SystemString; DataSource: TPascalString);
var
  M: TMemoryStream64;
begin
  M := TMemoryStream64.Create;
  TDBEnginePascalString.SavePascalStringToStream(@DataSource, M);
  FastPostCompleteBuffer(dataBaseName_, M, c_PascalString, True);
end;

procedure TDataStoreClient.FastInsertCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; stream: TMemoryStream64; dID: Cardinal; DoneTimeFree: Boolean);
var
  p: Pointer;
  siz: nativeUInt;
begin
  if stream.Size = 0 then
    begin
      SendTunnel.Error('FastInsertCompleteBuffer Error: stream size is 0');
      exit;
    end;
  p := EncodeZDBBuff(dataBaseName_, dID, dStorePos, stream.Memory, stream.Size, siz);
  SendTunnel.SendCompleteBuffer(C_FastInsertCompleteBuffer, p, siz, True);

  if DoneTimeFree then
      DisposeObject(stream);
end;

procedure TDataStoreClient.FastInsertCompleteBufferCopy(dataBaseName_: SystemString; dStorePos: Int64; stream: TCoreClassStream; dID: Cardinal);
var
  M: TMemoryStream64;
begin
  M := TMemoryStream64.Create;
  stream.Position := 0;
  M.CopyFrom(stream, stream.Size);
  M.Position := 0;
  FastInsertCompleteBuffer(dataBaseName_, dStorePos, M, dID, True);
end;

procedure TDataStoreClient.FastInsertCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TDataFrameEngine);
var
  M: TMemoryStream64;
begin
  M := TMemoryStream64.Create;
  DataSource.EncodeTo(M, True);
  FastInsertCompleteBuffer(dataBaseName_, dStorePos, M, c_DF, True);
end;

procedure TDataStoreClient.FastInsertCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; DataSource: THashVariantList);
var
  M: TMemoryStream64;
begin
  M := TMemoryStream64.Create;
  DataSource.SaveToStream(M);
  FastInsertCompleteBuffer(dataBaseName_, dStorePos, M, c_VL, True);
end;

procedure TDataStoreClient.FastInsertCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; DataSource: THashStringList);
var
  M: TMemoryStream64;
begin
  M := TMemoryStream64.Create;
  DataSource.SaveToStream(M);
  FastInsertCompleteBuffer(dataBaseName_, dStorePos, M, c_VT, True);
end;

procedure TDataStoreClient.FastInsertCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TSectionTextData);
var
  M: TMemoryStream64;
begin
  M := TMemoryStream64.Create;
  DataSource.SaveToStream(M);
  FastInsertCompleteBuffer(dataBaseName_, dStorePos, M, c_TE, True);
end;

{$IFNDEF FPC}


procedure TDataStoreClient.FastInsertCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TJsonObject);
var
  M: TMemoryStream64;
begin
  M := TMemoryStream64.Create;
  DataSource.SaveToStream(M, False, TEncoding.UTF8, True);
  FastInsertCompleteBuffer(dataBaseName_, dStorePos, M, c_Json, True);
end;
{$ENDIF FPC}


procedure TDataStoreClient.FastInsertCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TPascalString);
var
  M: TMemoryStream64;
begin
  M := TMemoryStream64.Create;
  TDBEnginePascalString.SavePascalStringToStream(@DataSource, M);
  FastInsertCompleteBuffer(dataBaseName_, dStorePos, M, c_PascalString, True);
end;

procedure TDataStoreClient.FastModifyCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; stream: TMemoryStream64; dID: Cardinal; DoneTimeFree: Boolean);
var
  p: Pointer;
  siz: nativeUInt;
begin
  if stream.Size = 0 then
    begin
      SendTunnel.Error('FastModifyCompleteBuffer Error: stream size is 0');
      exit;
    end;
  p := EncodeZDBBuff(dataBaseName_, dID, dStorePos, stream.Memory, stream.Size, siz);
  SendTunnel.SendCompleteBuffer(C_FastModifyCompleteBuffer, p, siz, True);

  if DoneTimeFree then
      DisposeObject(stream);
end;

procedure TDataStoreClient.FastModifyCompleteBufferCopy(dataBaseName_: SystemString; dStorePos: Int64; stream: TCoreClassStream; dID: Cardinal);
var
  M: TMemoryStream64;
begin
  M := TMemoryStream64.Create;
  stream.Position := 0;
  M.CopyFrom(stream, stream.Size);
  M.Position := 0;
  FastModifyCompleteBuffer(dataBaseName_, dStorePos, M, dID, True);
end;

procedure TDataStoreClient.FastModifyCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TDataFrameEngine);
var
  M: TMemoryStream64;
begin
  M := TMemoryStream64.Create;
  DataSource.EncodeTo(M, True);
  FastModifyCompleteBuffer(dataBaseName_, dStorePos, M, c_DF, True);
end;

procedure TDataStoreClient.FastModifyCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; DataSource: THashVariantList);
var
  M: TMemoryStream64;
begin
  M := TMemoryStream64.Create;
  DataSource.SaveToStream(M);
  FastModifyCompleteBuffer(dataBaseName_, dStorePos, M, c_VL, True);
end;

procedure TDataStoreClient.FastModifyCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; DataSource: THashStringList);
var
  M: TMemoryStream64;
begin
  M := TMemoryStream64.Create;
  DataSource.SaveToStream(M);
  FastModifyCompleteBuffer(dataBaseName_, dStorePos, M, c_VT, True);
end;

procedure TDataStoreClient.FastModifyCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TSectionTextData);
var
  M: TMemoryStream64;
begin
  M := TMemoryStream64.Create;
  DataSource.SaveToStream(M);
  FastModifyCompleteBuffer(dataBaseName_, dStorePos, M, c_TE, True);
end;

{$IFNDEF FPC}


procedure TDataStoreClient.FastModifyCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TJsonObject);
var
  M: TMemoryStream64;
begin
  M := TMemoryStream64.Create;
  DataSource.SaveToStream(M, False, TEncoding.UTF8, True);
  FastModifyCompleteBuffer(dataBaseName_, dStorePos, M, c_Json, True);
end;
{$ENDIF FPC}


procedure TDataStoreClient.FastModifyCompleteBuffer(dataBaseName_: SystemString; dStorePos: Int64; DataSource: TPascalString);
var
  M: TMemoryStream64;
begin
  M := TMemoryStream64.Create;
  TDBEnginePascalString.SavePascalStringToStream(@DataSource, M);
  FastModifyCompleteBuffer(dataBaseName_, dStorePos, M, c_PascalString, True);
end;

procedure TDataStoreClient.QueryStop(PipeName_: SystemString);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  de.WriteString(PipeName_);
  SendTunnel.SendDirectStreamCmd(C_QueryStop, de);
  DisposeObject(de);
end;

procedure TDataStoreClient.QueryPause(PipeName_: SystemString);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  de.WriteString(PipeName_);
  SendTunnel.SendDirectStreamCmd(C_QueryPause, de);
  DisposeObject(de);
end;

procedure TDataStoreClient.QueryPlay(PipeName_: SystemString);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  de.WriteString(PipeName_);
  SendTunnel.SendDirectStreamCmd(C_QueryPlay, de);
  DisposeObject(de);
end;

procedure TDataStoreClient.GetDBListM(OnResult: TStreamMethod);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  SendTunnel.SendStreamCmdM(C_GetDBList, de, OnResult);
  DisposeObject(de);
end;

procedure TDataStoreClient.GetDBListM(Param1: Pointer; Param2: TObject; OnResult: TStreamParamMethod);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  SendTunnel.SendStreamCmdM(C_GetDBList, de, Param1, Param2, OnResult);
  DisposeObject(de);
end;

procedure TDataStoreClient.GetQueryListM(OnResult: TStreamMethod);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  SendTunnel.SendStreamCmdM(C_GetQueryList, de, OnResult);
  DisposeObject(de);
end;

procedure TDataStoreClient.GetQueryListM(Param1: Pointer; Param2: TObject; OnResult: TStreamParamMethod);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  SendTunnel.SendStreamCmdM(C_GetQueryList, de, Param1, Param2, OnResult);
  DisposeObject(de);
end;

procedure TDataStoreClient.GetQueryStateM(PipeName_: SystemString; OnResult: TStreamMethod);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  de.WriteString(PipeName_);
  SendTunnel.SendStreamCmdM(C_GetQueryState, de, OnResult);
  DisposeObject(de);
end;

procedure TDataStoreClient.GetQueryStateM(PipeName_: SystemString; Param1: Pointer; Param2: TObject; OnResult: TStreamParamMethod);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  de.WriteString(PipeName_);
  SendTunnel.SendStreamCmdM(C_GetQueryState, de, Param1, Param2, OnResult);
  DisposeObject(de);
end;

procedure TDataStoreClient.GetDBListP(OnResult: TStreamProc);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  SendTunnel.SendStreamCmdP(C_GetDBList, de, OnResult);
  DisposeObject(de);
end;

procedure TDataStoreClient.GetDBListP(Param1: Pointer; Param2: TObject; OnResult: TStreamParamProc);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  SendTunnel.SendStreamCmdP(C_GetDBList, de, Param1, Param2, OnResult);
  DisposeObject(de);
end;

procedure TDataStoreClient.GetQueryListP(OnResult: TStreamProc);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  SendTunnel.SendStreamCmdP(C_GetQueryList, de, OnResult);
  DisposeObject(de);
end;

procedure TDataStoreClient.GetQueryListP(Param1: Pointer; Param2: TObject; OnResult: TStreamParamProc);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  SendTunnel.SendStreamCmdP(C_GetQueryList, de, Param1, Param2, OnResult);
  DisposeObject(de);
end;

procedure TDataStoreClient.GetQueryStateP(PipeName_: SystemString; OnResult: TStreamProc);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  de.WriteString(PipeName_);
  SendTunnel.SendStreamCmdP(C_GetQueryState, de, OnResult);
  DisposeObject(de);
end;

procedure TDataStoreClient.GetQueryStateP(PipeName_: SystemString; Param1: Pointer; Param2: TObject; OnResult: TStreamParamProc);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  de.WriteString(PipeName_);
  SendTunnel.SendStreamCmdP(C_GetQueryState, de, Param1, Param2, OnResult);
  DisposeObject(de);
end;

end.
