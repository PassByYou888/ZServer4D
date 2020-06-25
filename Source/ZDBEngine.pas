{ ****************************************************************************** }
{ * ZDBEngine, createby qq600585                                               * }
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
  2017-12-6 added cache system
  2017-12-8 optimization cache system
*)

unit ZDBEngine;

{$INCLUDE zDefine.inc}

interface

uses SysUtils, Classes,
  ListEngine, PascalStrings, UnicodeMixedLib, TextDataEngine,
{$IFNDEF FPC} ZS_JsonDataObjects, {$ENDIF}
  CoreClasses, MemoryStream64, ObjectData, ObjectDataManager,
  DataFrameEngine, ItemStream;

type
  TDBStore = class;

  TStoreArray = array of Int64;
  PStoreArray = ^TStoreArray;

  // Base Data Struct
  TDBEngineDF = class(TDataFrameEngine)
  protected
    FDBStorePos: Int64;
    dbEng: TDBStore;
    CreateTime, ModificationTime: TDateTime;
    MemoryUsed: nativeUInt;
  public
    constructor Create;
    procedure Save;

    property StorePos: Int64 read FDBStorePos;
    property Eng: TDBStore read dbEng;
  end;

  // Base Data Struct
  TDBEngineVL = class(THashVariantList)
  protected
    FDBStorePos: Int64;
    dbEng: TDBStore;
    CreateTime, ModificationTime: TDateTime;
    MemoryUsed: nativeUInt;
  public
    constructor Create;
    procedure Save;

    property StorePos: Int64 read FDBStorePos;
    property Eng: TDBStore read dbEng;
  end;

  // Base Data Struct
  TDBEngineVT = class(THashStringList)
  protected
    FDBStorePos: Int64;
    dbEng: TDBStore;
    CreateTime, ModificationTime: TDateTime;
    MemoryUsed: nativeUInt;
  public
    constructor Create;
    procedure Save;

    property StorePos: Int64 read FDBStorePos;
    property Eng: TDBStore read dbEng;
  end;

  // Base Data Struct
  TDBEngineTE = class(TSectionTextData)
  protected
    FDBStorePos: Int64;
    dbEng: TDBStore;
    CreateTime, ModificationTime: TDateTime;
    MemoryUsed: nativeUInt;
  public
    constructor Create;
    procedure Save;

    property StorePos: Int64 read FDBStorePos;
    property Eng: TDBStore read dbEng;
  end;

{$IFNDEF FPC}

  // Base Data Struct
  TDBEngineJson = class(TJsonObject)
  protected
    FDBStorePos: Int64;
    dbEng: TDBStore;
    CreateTime, ModificationTime: TDateTime;
    MemoryUsed: nativeUInt;
  public
    constructor Create;
    procedure Save;

    property StorePos: Int64 read FDBStorePos;
    property Eng: TDBStore read dbEng;
  end;
{$ENDIF}

  // Base Data Struct
  TDBEnginePascalString = class(TCoreClassObject)
  protected
    FDBStorePos: Int64;
    dbEng: TDBStore;
    CreateTime, ModificationTime: TDateTime;
    MemoryUsed: nativeUInt;
  public
    hash: THash;
    Buff: TPascalString;

    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Save;

    procedure LoadFromStream(stream: TCoreClassStream);
    procedure SaveToStream(stream: TCoreClassStream);

    property StorePos: Int64 read FDBStorePos;
    property Eng: TDBStore read dbEng;

    class procedure LoadPascalStringFromStream(p: PPascalString; stream: TCoreClassStream);
    class procedure SavePascalStringToStream(p: PPascalString; stream: TCoreClassStream);
  end;

  // Base DataBase Struct
  TDBListDF = class(TCoreClassObject)
  protected
    FHashListBuff: TCoreClassListForObj;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    function Count: Integer;
    function GetItems(const index: Integer): TDBEngineDF;
    property Items[const index: Integer]: TDBEngineDF read GetItems; default;
    function Add: TDBEngineDF; overload;
    procedure Add(Value: TDBEngineDF); overload;
    procedure Delete(index: Integer);

    procedure LoadFromStoreEngine(dbEng: TDBStore);
    procedure ExportToStoreEngine(dbEng: TDBStore);

    property HashListBuff: TCoreClassListForObj read FHashListBuff;
  end;

  // Base DataBase Struct
  TDBListVL = class(TCoreClassObject)
  protected
    FHashListBuff: TCoreClassListForObj;
    procedure do_ImportCSV(const sour: TPascalString; const king, Data: TArrayPascalString);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    function Count: Integer;
    function GetItems(const index: Integer): TDBEngineVL;
    property Items[const index: Integer]: TDBEngineVL read GetItems; default;
    function Add: TDBEngineVL; overload;
    procedure Add(Value: TDBEngineVL); overload;

    procedure ImportCSVStream(stream: TCoreClassStream);
    procedure ImportCSVFile(fn: SystemString);

    procedure ImportTextStream(stream: TCoreClassStream);
    procedure ImportTextFile(fn: SystemString);
    procedure ExportTextStream(stream: TCoreClassStream);
    procedure ExportTextFile(fn: SystemString);

    procedure LoadFromStoreEngine(dbEng: TDBStore);
    procedure ExportToStoreEngine(dbEng: TDBStore);

    property HashListBuff: TCoreClassListForObj read FHashListBuff;
  end;

  // Base DataBase Struct
  TDBListVT = class(TCoreClassObject)
  protected
    FHashListBuff: TCoreClassListForObj;
    procedure do_ImportCSV(const sour: TPascalString; const king, Data: TArrayPascalString);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    function Count: Integer;
    function GetItems(const index: Integer): TDBEngineVT;
    property Items[const index: Integer]: TDBEngineVT read GetItems; default;
    function Add: TDBEngineVT; overload;
    procedure Add(Value: TDBEngineVT); overload;

    procedure ImportCSVStream(stream: TCoreClassStream);
    procedure ImportCSVFile(fn: SystemString);

    procedure ImportTextStream(stream: TCoreClassStream);
    procedure ImportTextFile(fn: SystemString);
    procedure ExportTextStream(stream: TCoreClassStream);
    procedure ExportTextFile(fn: SystemString);

    procedure LoadFromStoreEngine(dbEng: TDBStore);
    procedure ExportToStoreEngine(dbEng: TDBStore);

    property HashListBuff: TCoreClassListForObj read FHashListBuff;
  end;

  // Base DataBase Struct
  TDBListTE = class(TCoreClassObject)
  protected
    FHashListBuff: TCoreClassListForObj;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    function Count: Integer;
    function GetItems(const index: Integer): TDBEngineTE;
    property Items[const index: Integer]: TDBEngineTE read GetItems; default;
    function Add: TDBEngineTE; overload;
    procedure Add(Value: TDBEngineTE); overload;

    procedure LoadFromStoreEngine(dbEng: TDBStore);
    procedure ExportToStoreEngine(dbEng: TDBStore);

    property HashListBuff: TCoreClassListForObj read FHashListBuff;
  end;

{$IFNDEF FPC}

  // Base DataBase Struct
  TDBListJson = class(TCoreClassObject)
  protected
    FHashListBuff: TCoreClassListForObj;
    procedure do_ImportCSV(const sour: TPascalString; const king, Data: TArrayPascalString);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    function Count: Integer;
    function GetItems(const index: Integer): TDBEngineJson;
    property Items[const index: Integer]: TDBEngineJson read GetItems; default;
    function Add: TDBEngineJson; overload;
    procedure Add(Value: TDBEngineJson); overload;

    procedure ImportCSVStream(stream: TCoreClassStream);
    procedure ImportCSVFile(fn: SystemString);

    procedure LoadFromStoreEngine(dbEng: TDBStore);
    procedure ExportToStoreEngine(dbEng: TDBStore);

    property HashListBuff: TCoreClassListForObj read FHashListBuff;
  end;
{$ENDIF}

  // Base DataBase Struct
  TDBListPascalString = class(TCoreClassObject)
  protected
    FHashListBuff: TCoreClassListForObj;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    function Count: Integer;
    function GetItems(const index: Integer): TDBEnginePascalString;
    property Items[const index: Integer]: TDBEnginePascalString read GetItems; default;
    function Add: TDBEnginePascalString; overload;
    procedure Add(Value: TDBEnginePascalString); overload;
    procedure Add(const Value: TPascalString); overload;

    procedure ImportTextStream(stream: TCoreClassStream);

    procedure LoadFromStoreEngine(dbEng: TDBStore);
    procedure ExportToStoreEngine(dbEng: TDBStore);

    property HashListBuff: TCoreClassListForObj read FHashListBuff;
  end;

  TDBCacheStream64 = class;

  PQueryState = ^TQueryState;

  TQueryState = record
    Eng: TDBStore;
    StorePos: Int64;
    QueryHnd: PHeader;
    index: NativeInt;
    TaskTag: SystemString;
    deltaTime, NewTime: TTimeTick;
    Aborted: Boolean;
    property dbEng: TDBStore read Eng;

    function ID: Cardinal;
    function IsDF: Boolean;
    function IsVL: Boolean;
    function IsVT: Boolean;
    function IsTE: Boolean;
    function IsJson: Boolean;
    function IsString: Boolean;
    function IsOther: Boolean;
    function IsFirst: Boolean;
    function IsLast: Boolean;
    function Cache: TDBCacheStream64;
    function NextCache: TDBCacheStream64;
    function PrevCache: TDBCacheStream64;
    function NextPos: Int64;
    function PrevPos: Int64;
  end;

  TQueryCall = procedure(var qState: TQueryState);
  TQueryMethod = procedure(var qState: TQueryState) of object;

  TQueryDoneCall = procedure();
  TQueryDoneMethod = procedure() of object;

  TRemoveCall = procedure(StorePos: Int64; RemoveSuccesed: Boolean);
  TRemoveMethod = procedure(StorePos: Int64; RemoveSuccesed: Boolean) of object;

{$IFDEF FPC}
  TQueryProc = procedure(var qState: TQueryState) is nested;
  TQueryDoneProc = procedure() is nested;
  TRemoveProc = procedure(StorePos: Int64; RemoveSuccesed: Boolean) is nested;
{$ELSE FPC}
  TQueryProc = reference to procedure(var qState: TQueryState);
  TQueryDoneProc = reference to procedure();
  TRemoveProc = reference to procedure(StorePos: Int64; RemoveSuccesed: Boolean);
{$ENDIF FPC}

  TQueryTask = class(TCoreClassObject)
  protected
    FDBEng: TDBStore;
    FInited: Boolean;
    FReverse: Boolean;
    FItmSrHnd: THeader;
    FState: TQueryState;
    FTriggerTime: TTimeTick;
    FTaskTag: SystemString;
    FLastTime: TTimeTick;
    FStoped, FPaused: Boolean;
    FProcessQueryDone: Boolean;
    FSyncTrigger: Boolean;
    FOnQueryCall: TQueryCall;
    FOnQueryMethod: TQueryMethod;
    FOnQueryProc: TQueryProc;
    FOnQueryDoneCall: TQueryDoneCall;
    FOnQueryDoneMethod: TQueryDoneMethod;
    FOnQueryDoneProc: TQueryDoneProc;
    procedure DoTriggerQuery;
    procedure DoQueryDone;
  public
    constructor Create;

    procedure stop;
    procedure Pause;
    procedure Play;

    function ProcessQuery(): Boolean;
    property Paused: Boolean read FPaused;
    function ConsumTime: Double;
  end;

  PRemoveQueueData = ^TRemoveQueueData;

  TRemoveQueueData = record
    OnRemoveCall: TRemoveCall;
    OnRemoveMethod: TRemoveMethod;
    OnRemoveProc: TRemoveProc;
  end;

  TQueryThread = class(TCoreClassThread)
  public
    StoreEngine: TDBStore;
    Paused: Boolean;
    PausedIdleTime: Double;
    RemoveQueue, RemoveCompletedQueue: TInt64HashPointerList;
    PickedQueryQueue: TCoreClassListForObj;

    procedure PickQueryQueue;
    procedure AsyncQuery;

    procedure SyncQueryDone;
    procedure SyncRemove;
    procedure SyncCheckCache;
    procedure SyncUpdateCacheState;

    procedure Execute; override;

    constructor Create(CreateSuspended: Boolean);
    destructor Destroy; override;

    procedure RemoveDeleteProc(p: Pointer);

    procedure PostRemoveQueue(StorePos: Int64);
    procedure PostRemoveQueueC(StorePos: Int64; OnRemove: TRemoveCall);
    procedure PostRemoveQueueM(StorePos: Int64; OnRemove: TRemoveMethod);
    procedure PostRemoveQueueP(StorePos: Int64; OnRemove: TRemoveProc);
  end;

  IDBStoreBaseNotify = interface
    procedure DoInsertData(Sender: TDBStore; InsertPos: Int64; Buff: TCoreClassStream; ID: Cardinal; CompletePos: Int64);
    procedure DoAddData(Sender: TDBStore; Buff: TCoreClassStream; ID: Cardinal; CompletePos: Int64);
    procedure DoModifyData(Sender: TDBStore; const StorePos: Int64; Buff: TCoreClassStream);
    procedure DoDeleteData(Sender: TDBStore; const StorePos: Int64);
  end;

  // store engine
  TCacheStyle = (csAutomation, csNever, csAlways);

  TDBCacheStream64 = class(TMemoryStream64)
  private
    OwnerEng: TDBStore;
    OwnerCache: TInt64HashObjectList;
    ID: Cardinal;
    CreateTime, ModificationTime: TDateTime;
    StorePos: Int64;
    UsedMemorySize: NativeInt;
  public
    constructor Create;
    destructor Destroy; override;

    property CacheID: Cardinal read ID;
  end;

  TDBStore = class(TCoreClassInterfacedObject)
  protected
    FDBEngine: TObjectDataManagerOfCache;
    FStoreFieldPos: Int64;
    FCount: Int64;
    FQueryQueue: TCoreClassListForObj;
    FQueryThread: TQueryThread;
    FQueryThreadTerminate: Boolean;
    FQueryThreadLastActivtedTime: TDateTime;
    FNotifyIntf: IDBStoreBaseNotify;
    FCache: TInt64HashObjectList;
    FStreamCache: TInt64HashObjectList;
    FUsedInstanceCacheMemory: Int64;
    FCacheStyle: TCacheStyle;
    FCacheAnnealingTime: Double;
    FMinimizeCacheMemorySize: Int64;
    FMaximumCacheMemorySize: Int64;
    FMinimizeStreamCacheMemorySize: Int64;
    FMaximumStreamCacheMemorySize: Int64;
    FUsedStreamCacheMemory: Int64;
    FMinimizeCacheOfFileSize: Int64;
    FCacheAnnealingState: SystemString;
    FResultDF: TDBEngineDF;
    FResultVL: TDBEngineVL;
    FResultVT: TDBEngineVT;
    FResultTE: TDBEngineTE;
{$IFNDEF FPC} FResultJson: TDBEngineJson; {$ENDIF}
    FResultPascalString: TDBEnginePascalString;
    // user define
    FUserPointer: Pointer;
    FUserObject: TCoreClassObject;
    FUserString: SystemString;
  protected
    procedure ReadHeaderInfo;
    procedure ThreadFreeEvent(Sender: TObject);
    procedure DoCreateInit; virtual;
    procedure InstanceCacheObjectFreeProc(Obj: TCoreClassObject);
    procedure ProcessNewInstanceCache(StorePos: Int64; Obj: TCoreClassObject; siz: NativeInt);
    procedure StreamCacheObjectFreeProc(Obj: TCoreClassObject);
    procedure ProcessNewStreamCache(M: TDBCacheStream64);
    function Internal_DeleteData(const StorePos: Int64): Boolean;
  public
    constructor Create(dbFile: SystemString; OnlyRead: Boolean);
    constructor CreateMemory(DBMemory: TMemoryStream64; OnlyRead: Boolean);
    constructor CreateNew(dbFile: SystemString);
    constructor CreateNewMemory;
    destructor Destroy; override;

    // compress support
    procedure CompressTo(DestDB: TObjectDataManager);
    procedure Compress;

    // realtime disk
    procedure Update;

    // file and stream
    procedure SaveToStream(stream: TCoreClassStream);
    procedure SaveToFile(fn: SystemString);
    procedure LoadFromStream(stream: TCoreClassStream);
    procedure LoadFromFile(fn: SystemString);

    function IsMemoryMode: Boolean;
    function IsReadOnly: Boolean;
    procedure ResetDB;
    function RenameDB(NewName: SystemString): Boolean;

    property DBEngine: TObjectDataManagerOfCache read FDBEngine;
    property Count: Int64 read FCount;

    // cache states
    property Cache: TInt64HashObjectList read FCache;
    procedure Recache;
    function AllowedCache: Boolean; virtual;

    {
      csAutomation:
      automatically manages the cache according from the parameters
      ---- CacheAnnealingTime, MaximumCacheMemorySize, MinimizeCacheMemorySize, MaximumStreamCacheMemorySize, MinimizeCacheOfFileSize
      ---- if the database is too large memory usage is automatically scheduled.

      csNever:
      disable cache

      csAlways:
      using memory to speed up all entries, memory crashes maybe when the database is large.
    }
    property CacheStyle: TCacheStyle read FCacheStyle write FCacheStyle;

    // only work in CacheStyle = csAutomation
    property CacheAnnealingTime: Double read FCacheAnnealingTime write FCacheAnnealingTime;
    property MaximumCacheMemorySize: Int64 read FMaximumCacheMemorySize write FMaximumCacheMemorySize;
    property MinimizeCacheMemorySize: Int64 read FMinimizeCacheMemorySize write FMinimizeCacheMemorySize;
    property MaximumStreamCacheMemorySize: Int64 read FMaximumStreamCacheMemorySize write FMaximumStreamCacheMemorySize;
    property MinimizeCacheOfFileSize: Int64 read FMinimizeCacheOfFileSize write FMinimizeCacheOfFileSize;

    // cache information
    property CacheAnnealingState: SystemString read FCacheAnnealingState;

    // user define
    property UserPointer: Pointer read FUserPointer write FUserPointer;
    property UserObject: TCoreClassObject read FUserObject write FUserObject;
    property UserString: SystemString read FUserString write FUserString;

    // lowlevel operation
    // Security delete operation
    procedure DeleteData(const StorePos: Int64);
    // insert
    function InsertData(const InsertPos: Int64; Buff: TCoreClassStream; ID: Cardinal; var itmHnd: TItemHandle): Int64; overload;
    function InsertData(const InsertPos: Int64; Buff: TCoreClassStream; ID: Cardinal): Int64; overload;
    // append
    function AddData(Buff: TCoreClassStream; ID: Cardinal; var itmHnd: TItemHandle): Int64; overload;
    function AddData(Buff: TCoreClassStream; ID: Cardinal): Int64; overload;
    // modify
    function SetData(const StorePos: Int64; Buff: TCoreClassStream): Boolean;
    // get cache
    function GetCacheStream(const StorePos: Int64; ID: Cardinal): TDBCacheStream64; overload;
    function GetCacheStream(const StorePos: Int64): TDBCacheStream64; overload;
    // backcall
    property NotifyIntf: IDBStoreBaseNotify read FNotifyIntf write FNotifyIntf;

    // baseapi
    function QueryFirst(var qState: TQueryState): Boolean;
    function QueryNext(var qState: TQueryState): Boolean;
    function QueryLast(var qState: TQueryState): Boolean;
    function QueryPrev(var qState: TQueryState): Boolean;

    // data array
    procedure BuildStorePosArray(ReverseBuild: Boolean; const OutputPtr: PStoreArray);
    procedure BuildStoreArray(ReverseBuild: Boolean; const OutputPtr: PStoreArray);

    // wait query
    procedure WaitQuery(ReverseQuery: Boolean;
      const OnQueryCall: TQueryCall;
      const OnQueryProc: TQueryProc;
      const OnQueryMethod: TQueryMethod); overload;

    procedure WaitQueryC(ReverseQuery: Boolean; const OnQueryCall: TQueryCall); overload;
    procedure WaitQueryM(ReverseQuery: Boolean; const OnQueryMethod: TQueryMethod); overload;
    procedure WaitQueryP(ReverseQuery: Boolean; const OnQueryProc: TQueryProc); overload;

    procedure WaitQueryC(const OnQueryCall: TQueryCall); overload;
    procedure WaitQueryP(const OnQueryProc: TQueryProc); overload;
    procedure WaitQueryM(const OnQueryMethod: TQueryMethod); overload;

    // background query
    function Query(const TaskTag: SystemString; const ReverseQuery: Boolean;
      const OnQueryCall: TQueryCall; const OnQueryDoneCall: TQueryDoneCall;
      const OnQueryProc: TQueryProc; const OnQueryDoneProc: TQueryDoneProc;
      const OnQueryMethod: TQueryMethod; const OnQueryDoneMethod: TQueryDoneMethod): TQueryTask; overload;

    function QueryC(const TaskTag: SystemString; const ReverseQuery: Boolean; const OnQueryCall: TQueryCall; const OnQueryDoneCall: TQueryDoneCall): TQueryTask; overload;
    function QueryM(const TaskTag: SystemString; const ReverseQuery: Boolean; const OnQueryMethod: TQueryMethod; const OnQueryDoneMethod: TQueryDoneMethod): TQueryTask; overload;
    function QueryP(const TaskTag: SystemString; const ReverseQuery: Boolean; const OnQueryProc: TQueryProc; const OnQueryDoneProc: TQueryDoneProc): TQueryTask; overload;

    function QueryC(const TaskTag: SystemString; const OnQueryCall: TQueryCall; const OnQueryDoneCall: TQueryDoneCall): TQueryTask; overload;
    function QueryM(const TaskTag: SystemString; const OnQueryMethod: TQueryMethod; const OnQueryDoneMethod: TQueryDoneMethod): TQueryTask; overload;
    function QueryP(const TaskTag: SystemString; const OnQueryProc: TQueryProc; const OnQueryDoneProc: TQueryDoneProc): TQueryTask; overload;

    function QueryC(const OnQueryCall: TQueryCall; const OnQueryDoneCall: TQueryDoneCall): TQueryTask; overload;
    function QueryM(const OnQueryMethod: TQueryMethod; const OnQueryDoneMethod: TQueryDoneMethod): TQueryTask; overload;
    function QueryP(const OnQueryProc: TQueryProc; const OnQueryDoneProc: TQueryDoneProc): TQueryTask; overload;

    function QueryC(const ReverseQuery: Boolean; const OnQueryCall: TQueryCall; const OnQueryDoneCall: TQueryDoneCall): TQueryTask; overload;
    function QueryM(const ReverseQuery: Boolean; const OnQueryMethod: TQueryMethod; const OnQueryDoneMethod: TQueryDoneMethod): TQueryTask; overload;
    function QueryP(const ReverseQuery: Boolean; const OnQueryProc: TQueryProc; const OnQueryDoneProc: TQueryDoneProc): TQueryTask; overload;

    procedure WaitQueryThread; overload;
    procedure WaitQueryThread(waitTime: TTimeTick); overload;

    // query state
    function QueryProcessing: Boolean;
    property QueryThreadLastActivtedTime: TDateTime read FQueryThreadLastActivtedTime;

    // query task operation
    procedure StopQuery(const TaskTag: SystemString);
    procedure StopAllQuery;
    function QueryThreadCount: Integer;

    // dataframe operation
    function InsertData(const InsertPos: Int64; Buff: TDataFrameEngine): Int64; overload;
    function AddData(Buff: TDataFrameEngine): Int64; overload;
    function GetDF(const StorePos: Int64): TDBEngineDF; overload;
    function GetDF(var qState: TQueryState): TDBEngineDF; overload;
    function BuildDF(const StorePos: Int64): TDBEngineDF; overload;
    function BuildDF(var qState: TQueryState): TDBEngineDF; overload;
    property DF[const StorePos: Int64]: TDBEngineDF read GetDF;

    // key-value operation
    function InsertData(const InsertPos: Int64; Buff: THashVariantList): Int64; overload;
    function AddData(Buff: THashVariantList): Int64; overload;
    function GetVL(const StorePos: Int64): TDBEngineVL; overload;
    function GetVL(var qState: TQueryState): TDBEngineVL; overload;
    function BuildVL(const StorePos: Int64): TDBEngineVL; overload;
    function BuildVL(var qState: TQueryState): TDBEngineVL; overload;
    property VL[const StorePos: Int64]: TDBEngineVL read GetVL;

    // key-string operation
    function InsertData(const InsertPos: Int64; Buff: THashStringList): Int64; overload;
    function AddData(Buff: THashStringList): Int64; overload;
    function GetVT(const StorePos: Int64): TDBEngineVT; overload;
    function GetVT(var qState: TQueryState): TDBEngineVT; overload;
    function BuildVT(const StorePos: Int64): TDBEngineVT; overload;
    function BuildVT(var qState: TQueryState): TDBEngineVT; overload;
    property VT[const StorePos: Int64]: TDBEngineVT read GetVT;

    // text section operation
    function InsertData(const InsertPos: Int64; Buff: TSectionTextData): Int64; overload;
    function AddData(Buff: TSectionTextData): Int64; overload;
    function GetTE(const StorePos: Int64): TDBEngineTE; overload;
    function GetTE(var qState: TQueryState): TDBEngineTE; overload;
    function BuildTE(const StorePos: Int64): TDBEngineTE; overload;
    function BuildTE(var qState: TQueryState): TDBEngineTE; overload;
    property TE[const StorePos: Int64]: TDBEngineTE read GetTE;

    // json operation
{$IFNDEF FPC}
    function InsertData(const InsertPos: Int64; Buff: TJsonObject): Int64; overload;
    function AddData(Buff: TJsonObject): Int64; overload;
    function GetJson(const StorePos: Int64): TDBEngineJson; overload;
    function GetJson(var qState: TQueryState): TDBEngineJson; overload;
    function BuildJson(const StorePos: Int64): TDBEngineJson; overload;
    function BuildJson(var qState: TQueryState): TDBEngineJson; overload;
    property Json[const StorePos: Int64]: TDBEngineJson read GetJson;
{$ENDIF}
    //
    // string operation
    function InsertData(const InsertPos: Int64; Buff: TDBEnginePascalString): Int64; overload;
    function InsertData(const InsertPos: Int64; Buff: TPascalString): Int64; overload;
    function InsertString(const InsertPos: Int64; Buff: TPascalString): Int64; overload;
    function AddData(Buff: TDBEnginePascalString): Int64; overload;
    function AddData(Buff: TPascalString): Int64; overload;
    function AddString(Buff: TPascalString): Int64; overload;
    function GetPascalString(const StorePos: Int64): TDBEnginePascalString; overload;
    function GetPascalString(var qState: TQueryState): TDBEnginePascalString; overload;
    function GetString(const StorePos: Int64): TPascalString; overload;
    function GetString(var qState: TQueryState): TPascalString; overload;
    procedure SetString(const StorePos: Int64; const Value: TPascalString); overload;
    function BuildPascalString(const StorePos: Int64): TDBEnginePascalString; overload;
    function BuildPascalString(var qState: TQueryState): TDBEnginePascalString; overload;
    property PascalString[const StorePos: Int64]: TPascalString read GetString write SetString;
  end;

procedure zDBthSync(t: TCoreClassThread; Sync: Boolean; proc: TThreadMethod);

const
  c_DF: Cardinal = $FFFFFFF0;
  c_VL: Cardinal = $FFFFFFF1;
  c_TE: Cardinal = $FFFFFFF2;
  c_Json: Cardinal = $FFFFFFF3;
  c_PascalString: Cardinal = $FFFFFFF4;
  c_VT: Cardinal = $FFFFFFF5;

var
  DefaultCacheAnnealingTime: Double;
  DefaultCacheBufferLength: Integer;
  DefaultIndexCacheBufferLength: Integer;
  DefaultMinimizeInstanceCacheSize: Int64;
  DefaultMaximumInstanceCacheSize: Int64;
  DefaultMinimizeStreamCacheSize: Int64;
  DefaultMaximumStreamCacheSize: Int64;
  DefaultMinimizeCacheOfFileSize: Int64;

implementation

uses MH_ZDB, CoreCipher, DoStatusIO;

procedure zDBthSync(t: TCoreClassThread; Sync: Boolean; proc: TThreadMethod);
begin
  try
    if Sync then
        TCoreClassThread.Synchronize(t, proc)
    else
        proc();
  except
  end;
end;

constructor TDBEngineDF.Create;
begin
  inherited Create;
  FDBStorePos := -1;
  dbEng := nil;
  CreateTime := umlDefaultTime;
  ModificationTime := CreateTime;
  MemoryUsed := 0;
end;

procedure TDBEngineDF.Save;
var
  M: TMemoryStream64;
begin
  if (FDBStorePos < 0) or (dbEng = nil) then
      Exit;

  M := TMemoryStream64.Create;
  EncodeTo(M, True);
  dbEng.SetData(FDBStorePos, M);
  DisposeObject(M);
end;

constructor TDBEngineVL.Create;
begin
  inherited CustomCreate(2);
  FDBStorePos := -1;
  dbEng := nil;
  CreateTime := umlDefaultTime;
  ModificationTime := CreateTime;
  MemoryUsed := 0;
end;

procedure TDBEngineVL.Save;
var
  M: TMemoryStream64;
begin
  if (FDBStorePos < 0) or (dbEng = nil) then
      Exit;

  M := TMemoryStream64.Create;
  SaveToStream(M);
  dbEng.SetData(FDBStorePos, M);
  DisposeObject(M);
end;

constructor TDBEngineVT.Create;
begin
  inherited CustomCreate(2);
  FDBStorePos := -1;
  dbEng := nil;
  CreateTime := umlDefaultTime;
  ModificationTime := CreateTime;
  MemoryUsed := 0;
end;

procedure TDBEngineVT.Save;
var
  M: TMemoryStream64;
begin
  if (FDBStorePos < 0) or (dbEng = nil) then
      Exit;

  M := TMemoryStream64.Create;
  SaveToStream(M);
  dbEng.SetData(FDBStorePos, M);
  DisposeObject(M);
end;

constructor TDBEngineTE.Create;
begin
  inherited Create;
  FDBStorePos := -1;
  dbEng := nil;
  CreateTime := umlDefaultTime;
  ModificationTime := CreateTime;
  MemoryUsed := 0;
end;

procedure TDBEngineTE.Save;
var
  M: TMemoryStream64;
begin
  if (FDBStorePos < 0) or (dbEng = nil) then
      Exit;

  M := TMemoryStream64.Create;
  SaveToStream(M);
  dbEng.SetData(FDBStorePos, M);
  DisposeObject(M);
end;

{$IFNDEF FPC}


constructor TDBEngineJson.Create;
begin
  inherited Create;
  FDBStorePos := -1;
  dbEng := nil;
  CreateTime := umlDefaultTime;
  ModificationTime := CreateTime;
  MemoryUsed := 0;
end;

procedure TDBEngineJson.Save;
var
  M: TMemoryStream64;
begin
  if (FDBStorePos < 0) or (dbEng = nil) then
      Exit;

  M := TMemoryStream64.Create;
  SaveToStream(M, True, TEncoding.UTF8, True);
  dbEng.SetData(FDBStorePos, M);
  DisposeObject(M);
end;
{$ENDIF}


constructor TDBEnginePascalString.Create;
begin
  inherited Create;
  FDBStorePos := -1;
  dbEng := nil;
  CreateTime := umlDefaultTime;
  ModificationTime := CreateTime;
  Buff.Len := 0;
  hash := 0;
  MemoryUsed := 0;
end;

destructor TDBEnginePascalString.Destroy;
begin
  Buff := '';
  inherited Destroy;
end;

procedure TDBEnginePascalString.Clear;
begin
  Buff.Len := 0;
  hash := 0;
end;

procedure TDBEnginePascalString.Save;
var
  M: TMemoryStream64;
begin
  if (FDBStorePos < 0) or (dbEng = nil) then
      Exit;

  M := TMemoryStream64.Create;
  SaveToStream(M);
  dbEng.SetData(FDBStorePos, M);
  DisposeObject(M);
end;

procedure TDBEnginePascalString.LoadFromStream(stream: TCoreClassStream);
begin
  TDBEnginePascalString.LoadPascalStringFromStream(@Buff, stream);
  hash := Buff.hash;
end;

procedure TDBEnginePascalString.SaveToStream(stream: TCoreClassStream);
begin
  TDBEnginePascalString.SavePascalStringToStream(@Buff, stream);
end;

class procedure TDBEnginePascalString.LoadPascalStringFromStream(p: PPascalString; stream: TCoreClassStream);
var
  L: Integer;
  b: TBytes;
begin
  stream.read(L, C_Integer_Size);
  SetLength(b, L);
  stream.read(b[0], L);
  p^.Bytes := b;
  SetLength(b, 0);
end;

class procedure TDBEnginePascalString.SavePascalStringToStream(p: PPascalString; stream: TCoreClassStream);
var
  L: Integer;
  b: TBytes;
begin
  p^.FastGetBytes(b);
  L := length(b);
  stream.write(L, C_Integer_Size);
  stream.write(b[0], L);
  SetLength(b, 0);
end;

constructor TDBListDF.Create;
begin
  inherited Create;
  FHashListBuff := TCoreClassListForObj.Create;
end;

destructor TDBListDF.Destroy;
begin
  Clear;
  DisposeObject([FHashListBuff]);
  inherited Destroy;
end;

procedure TDBListDF.Clear;
var
  i: Integer;
begin
  for i := 0 to FHashListBuff.Count - 1 do
      DisposeObject(FHashListBuff[i]);
  FHashListBuff.Clear;
end;

function TDBListDF.Count: Integer;
begin
  Result := FHashListBuff.Count;
end;

function TDBListDF.GetItems(const index: Integer): TDBEngineDF;
begin
  Result := FHashListBuff[index] as TDBEngineDF;
end;

function TDBListDF.Add: TDBEngineDF;
begin
  Result := TDBEngineDF.Create;
  Result.FDBStorePos := -1;
  Result.dbEng := nil;
  FHashListBuff.Add(Result);
end;

procedure TDBListDF.Add(Value: TDBEngineDF);
begin
  FHashListBuff.Add(Value);
end;

procedure TDBListDF.Delete(index: Integer);
begin
  DisposeObject(FHashListBuff[index]);
  FHashListBuff.Delete(index);
end;

procedure TDBListDF.LoadFromStoreEngine(dbEng: TDBStore);
var
  itmSearHnd: THeader;
  qState: TQueryState;
begin
  Clear;
  qState.QueryHnd := @itmSearHnd;
  if dbEng.QueryFirst(qState) then
    begin
      repeat
        if qState.ID = c_DF then
            FHashListBuff.Add(dbEng.BuildDF(qState.StorePos));
      until not dbEng.QueryNext(qState);
    end;
end;

procedure TDBListDF.ExportToStoreEngine(dbEng: TDBStore);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      dbEng.AddData(GetItems(i));
end;

procedure TDBListVL.do_ImportCSV(const sour: TPascalString; const king, Data: TArrayPascalString);
var
  VL: TDBEngineVL;
  i: Integer;
begin
  VL := Add;
  for i := low(king) to high(king) do
      VL[king[i].Text] := Data[i].Text;
end;

constructor TDBListVL.Create;
begin
  inherited Create;
  FHashListBuff := TCoreClassListForObj.Create;
end;

destructor TDBListVL.Destroy;
begin
  Clear;
  DisposeObject([FHashListBuff]);
  inherited Destroy;
end;

procedure TDBListVL.Clear;
var
  i: Integer;
begin
  for i := 0 to FHashListBuff.Count - 1 do
      DisposeObject(FHashListBuff[i]);
  FHashListBuff.Clear;
end;

function TDBListVL.Count: Integer;
begin
  Result := FHashListBuff.Count;
end;

function TDBListVL.GetItems(const index: Integer): TDBEngineVL;
begin
  Result := FHashListBuff[index] as TDBEngineVL;
end;

function TDBListVL.Add: TDBEngineVL;
begin
  Result := TDBEngineVL.Create;
  Result.FDBStorePos := -1;
  Result.dbEng := nil;
  FHashListBuff.Add(Result);
end;

procedure TDBListVL.Add(Value: TDBEngineVL);
begin
  FHashListBuff.Add(Value);
end;

procedure TDBListVL.ImportCSVStream(stream: TCoreClassStream);
var
  lst: TListPascalString;
  Buff: TArrayPascalString;
begin
  lst := TListPascalString.Create;
  lst.LoadFromStream(stream);
  lst.FillTo(Buff);
  ImportCSV_M(Buff, {$IFDEF FPC}@{$ENDIF FPC}do_ImportCSV);
  DisposeObject(lst);
  SetLength(Buff, 0);
end;

procedure TDBListVL.ImportCSVFile(fn: SystemString);
var
  fs: TCoreClassFileStream;
begin
  fs := TCoreClassFileStream.Create(fn, fmOpenRead or fmShareDenyNone);
  try
      ImportCSVStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TDBListVL.ImportTextStream(stream: TCoreClassStream);
var
  sour: TListPascalString;
  i: Integer;
  n: TPascalString;
  VL: THashVariantList;
  TextName, TextValue: TPascalString;
begin
  sour := TListPascalString.Create;
  try
      sour.LoadFromStream(stream);
  except
    DisposeObject(sour);
    Exit;
  end;

  VL := THashVariantList.Create;

  i := 0;
  while i < sour.Count do
    begin
      n := sour[i].TrimChar(#32);
      inc(i);
      if n.Len = 0 then
        begin
          if VL.Count > 0 then
            begin
              FHashListBuff.Add(VL);
              VL := THashVariantList.Create;
            end;
        end
      else if ((n.Exists(':')) or (n.Exists('='))) and (not CharIn(n.First, [':', '='])) then
        begin
          TextName := umlGetFirstStr_Discontinuity(n, ':=');
          if TextName.Len > 0 then
            begin
              TextValue := umlDeleteFirstStr_Discontinuity(n, ':=');
              VL[TextName.Text] := THashVariantTextStream.StrToV(TextValue.Text);
            end
          else
              VL[n.Text] := '';
        end
      else
        begin
          VL[n.Text] := '';
        end;
    end;

  if VL.Count > 0 then
    begin
      FHashListBuff.Add(VL);
    end
  else
      DisposeObject(VL);

  DisposeObject([sour]);
end;

procedure TDBListVL.ImportTextFile(fn: SystemString);
var
  fs: TCoreClassFileStream;
begin
  fs := TCoreClassFileStream.Create(fn, fmOpenRead or fmShareDenyNone);
  try
      ImportTextStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TDBListVL.ExportTextStream(stream: TCoreClassStream);
const
  LineBreak = #13#10;
var
  i, j: Integer;
  ls: TCoreClassList;
  s, n: TPascalString;
  b: TPascalString;
  Buff: TBytes;
begin
  ls := TCoreClassList.Create;

  for i := 0 to FHashListBuff.Count - 1 do
    begin
      ls.Clear;
      THashVariantList(FHashListBuff[i]).HashList.GetListData(ls);
      b := '';
      if ls.Count > 0 then
        begin
          for j := 0 to ls.Count - 1 do
            begin
              s.Text := THashVariantTextStream.VToStr(PHashVariantListData(PHashListData(ls[j])^.Data)^.v);

              if s.Len > 0 then
                  n.Text := PHashListData(ls[j])^.OriginName + '=' + s.Text
              else
                  n.Text := PHashListData(ls[j])^.OriginName;

              b := b + n + LineBreak;
            end;

          b := b + LineBreak;
          Buff := b.Bytes;
          stream.write(Buff, length(Buff));
          b := '';
        end;
    end;

  DisposeObject([ls]);
end;

procedure TDBListVL.ExportTextFile(fn: SystemString);
var
  fs: TCoreClassFileStream;
begin
  fs := TCoreClassFileStream.Create(fn, fmCreate);
  try
      ExportTextStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TDBListVL.LoadFromStoreEngine(dbEng: TDBStore);
var
  itmSearHnd: THeader;
  qState: TQueryState;
begin
  Clear;
  qState.QueryHnd := @itmSearHnd;
  if dbEng.QueryFirst(qState) then
    begin
      repeat
        if qState.ID = c_VL then
            FHashListBuff.Add(dbEng.BuildVL(qState.StorePos));
      until not dbEng.QueryNext(qState);
    end;
end;

procedure TDBListVL.ExportToStoreEngine(dbEng: TDBStore);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      dbEng.AddData(GetItems(i));
end;

procedure TDBListVT.do_ImportCSV(const sour: TPascalString; const king, Data: TArrayPascalString);
var
  VL: TDBEngineVT;
  i: Integer;
begin
  VL := Add;
  for i := low(king) to high(king) do
      VL[king[i].Text] := Data[i].Text;
end;

constructor TDBListVT.Create;
begin
  inherited Create;
  FHashListBuff := TCoreClassListForObj.Create;
end;

destructor TDBListVT.Destroy;
begin
  Clear;
  DisposeObject([FHashListBuff]);
  inherited Destroy;
end;

procedure TDBListVT.Clear;
var
  i: Integer;
begin
  for i := 0 to FHashListBuff.Count - 1 do
      DisposeObject(FHashListBuff[i]);
  FHashListBuff.Clear;
end;

function TDBListVT.Count: Integer;
begin
  Result := FHashListBuff.Count;
end;

function TDBListVT.GetItems(const index: Integer): TDBEngineVT;
begin
  Result := FHashListBuff[index] as TDBEngineVT;
end;

function TDBListVT.Add: TDBEngineVT;
begin
  Result := TDBEngineVT.Create;
  Result.FDBStorePos := -1;
  Result.dbEng := nil;
  FHashListBuff.Add(Result);
end;

procedure TDBListVT.Add(Value: TDBEngineVT);
begin
  FHashListBuff.Add(Value);
end;

procedure TDBListVT.ImportCSVStream(stream: TCoreClassStream);
var
  lst: TListPascalString;
  Buff: TArrayPascalString;
begin
  lst := TListPascalString.Create;
  lst.LoadFromStream(stream);
  lst.FillTo(Buff);
  ImportCSV_M(Buff, {$IFDEF FPC}@{$ENDIF FPC}do_ImportCSV);
  DisposeObject(lst);
  SetLength(Buff, 0);
end;

procedure TDBListVT.ImportCSVFile(fn: SystemString);
var
  fs: TCoreClassFileStream;
begin
  fs := TCoreClassFileStream.Create(fn, fmOpenRead or fmShareDenyNone);
  try
      ImportCSVStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TDBListVT.ImportTextStream(stream: TCoreClassStream);
var
  sour: TListPascalString;
  i: Integer;
  n: TPascalString;
  VL: THashVariantList;
  TextName, TextValue: TPascalString;
begin
  sour := TListPascalString.Create;
  try
      sour.LoadFromStream(stream);
  except
    DisposeObject(sour);
    Exit;
  end;

  VL := THashVariantList.Create;

  i := 0;
  while i < sour.Count do
    begin
      n := sour[i].TrimChar(#32);
      inc(i);
      if n.Len = 0 then
        begin
          if VL.Count > 0 then
            begin
              FHashListBuff.Add(VL);
              VL := THashVariantList.Create;
            end;
        end
      else if ((n.Exists(':')) or (n.Exists('='))) and (not CharIn(n.First, [':', '='])) then
        begin
          TextName := umlGetFirstStr_Discontinuity(n, ':=');
          if TextName.Len > 0 then
            begin
              TextValue := umlDeleteFirstStr_Discontinuity(n, ':=');
              VL[TextName.Text] := THashVariantTextStream.StrToV(TextValue.Text);
            end
          else
              VL[n.Text] := '';
        end
      else
        begin
          VL[n.Text] := '';
        end;
    end;

  if VL.Count > 0 then
    begin
      FHashListBuff.Add(VL);
    end
  else
      DisposeObject(VL);

  DisposeObject([sour]);
end;

procedure TDBListVT.ImportTextFile(fn: SystemString);
var
  fs: TCoreClassFileStream;
begin
  fs := TCoreClassFileStream.Create(fn, fmOpenRead or fmShareDenyNone);
  try
      ImportTextStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TDBListVT.ExportTextStream(stream: TCoreClassStream);
const
  LineBreak = #13#10;
var
  i, j: Integer;
  ls: TCoreClassList;
  s, n: TPascalString;
  b: TPascalString;
  Buff: TBytes;
begin
  ls := TCoreClassList.Create;

  for i := 0 to FHashListBuff.Count - 1 do
    begin
      ls.Clear;
      THashVariantList(FHashListBuff[i]).HashList.GetListData(ls);
      b := '';
      if ls.Count > 0 then
        begin
          for j := 0 to ls.Count - 1 do
            begin
              s.Text := THashVariantTextStream.VToStr(PHashVariantListData(PHashListData(ls[j])^.Data)^.v);

              if s.Len > 0 then
                  n.Text := PHashListData(ls[j])^.OriginName + '=' + s.Text
              else
                  n.Text := PHashListData(ls[j])^.OriginName;

              b := b + n + LineBreak;
            end;

          b := b + LineBreak;
          Buff := b.Bytes;
          stream.write(Buff, length(Buff));
          b := '';
        end;
    end;

  DisposeObject([ls]);
end;

procedure TDBListVT.ExportTextFile(fn: SystemString);
var
  fs: TCoreClassFileStream;
begin
  fs := TCoreClassFileStream.Create(fn, fmCreate);
  try
      ExportTextStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TDBListVT.LoadFromStoreEngine(dbEng: TDBStore);
var
  itmSearHnd: THeader;
  qState: TQueryState;
begin
  Clear;
  qState.QueryHnd := @itmSearHnd;
  if dbEng.QueryFirst(qState) then
    begin
      repeat
        if qState.ID = c_VT then
            FHashListBuff.Add(dbEng.BuildVT(qState.StorePos));
      until not dbEng.QueryNext(qState);
    end;
end;

procedure TDBListVT.ExportToStoreEngine(dbEng: TDBStore);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      dbEng.AddData(GetItems(i));
end;

constructor TDBListTE.Create;
begin
  inherited Create;
  FHashListBuff := TCoreClassListForObj.Create;
end;

destructor TDBListTE.Destroy;
begin
  Clear;
  DisposeObject([FHashListBuff]);
  inherited Destroy;
end;

procedure TDBListTE.Clear;
var
  i: Integer;
begin
  for i := 0 to FHashListBuff.Count - 1 do
      DisposeObject(FHashListBuff[i]);
  FHashListBuff.Clear;
end;

function TDBListTE.Count: Integer;
begin
  Result := FHashListBuff.Count;
end;

function TDBListTE.GetItems(const index: Integer): TDBEngineTE;
begin
  Result := FHashListBuff[index] as TDBEngineTE;
end;

function TDBListTE.Add: TDBEngineTE;
begin
  Result := TDBEngineTE.Create;
  Result.FDBStorePos := -1;
  Result.dbEng := nil;
  FHashListBuff.Add(Result);
end;

procedure TDBListTE.Add(Value: TDBEngineTE);
begin
  FHashListBuff.Add(Value);
end;

procedure TDBListTE.LoadFromStoreEngine(dbEng: TDBStore);
var
  itmSearHnd: THeader;
  qState: TQueryState;
begin
  Clear;
  qState.QueryHnd := @itmSearHnd;
  if dbEng.QueryFirst(qState) then
    begin
      repeat
        if qState.ID = c_TE then
            FHashListBuff.Add(dbEng.BuildTE(qState.StorePos));
      until not dbEng.QueryNext(qState);
    end;
end;

procedure TDBListTE.ExportToStoreEngine(dbEng: TDBStore);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      dbEng.AddData(GetItems(i));
end;

{$IFNDEF FPC}


procedure TDBListJson.do_ImportCSV(const sour: TPascalString; const king, Data: TArrayPascalString);
var
  js: TDBEngineJson;
  i: Integer;
begin
  js := Add;
  for i := low(king) to high(king) do
      js.s[king[i].Text] := Data[i].Text;
end;

constructor TDBListJson.Create;
begin
  inherited Create;
  FHashListBuff := TCoreClassListForObj.Create;
end;

destructor TDBListJson.Destroy;
begin
  Clear;
  DisposeObject([FHashListBuff]);
  inherited Destroy;
end;

procedure TDBListJson.Clear;
var
  i: Integer;
begin
  for i := 0 to FHashListBuff.Count - 1 do
      DisposeObject(FHashListBuff[i]);
  FHashListBuff.Clear;
end;

function TDBListJson.Count: Integer;
begin
  Result := FHashListBuff.Count;
end;

function TDBListJson.GetItems(const index: Integer): TDBEngineJson;
begin
  Result := FHashListBuff[index] as TDBEngineJson;
end;

function TDBListJson.Add: TDBEngineJson;
begin
  Result := TDBEngineJson.Create;
  Result.FDBStorePos := -1;
  Result.dbEng := nil;
  FHashListBuff.Add(Result);
end;

procedure TDBListJson.Add(Value: TDBEngineJson);
begin
  FHashListBuff.Add(Value);
end;

procedure TDBListJson.ImportCSVStream(stream: TCoreClassStream);
var
  lst: TListPascalString;
  Buff: TArrayPascalString;
begin
  lst := TListPascalString.Create;
  lst.LoadFromStream(stream);
  lst.FillTo(Buff);
  ImportCSV_M(Buff, {$IFDEF FPC}@{$ENDIF FPC}do_ImportCSV);
  DisposeObject(lst);
  SetLength(Buff, 0);
end;

procedure TDBListJson.ImportCSVFile(fn: SystemString);
var
  fs: TCoreClassFileStream;
begin
  fs := TCoreClassFileStream.Create(fn, fmOpenRead or fmShareDenyNone);
  try
      ImportCSVStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

procedure TDBListJson.LoadFromStoreEngine(dbEng: TDBStore);
var
  itmSearHnd: THeader;
  qState: TQueryState;
begin
  Clear;
  qState.QueryHnd := @itmSearHnd;
  if dbEng.QueryFirst(qState) then
    begin
      repeat
        if qState.ID = c_Json then
            FHashListBuff.Add(dbEng.BuildJson(qState.StorePos));
      until not dbEng.QueryNext(qState);
    end;
end;

procedure TDBListJson.ExportToStoreEngine(dbEng: TDBStore);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      dbEng.AddData(GetItems(i));
end;
{$ENDIF}


constructor TDBListPascalString.Create;
begin
  inherited Create;
  FHashListBuff := TCoreClassListForObj.Create;
end;

destructor TDBListPascalString.Destroy;
begin
  Clear;
  DisposeObject([FHashListBuff]);
  inherited Destroy;
end;

procedure TDBListPascalString.Clear;
var
  i: Integer;
begin
  for i := 0 to FHashListBuff.Count - 1 do
      DisposeObject(FHashListBuff[i]);
  FHashListBuff.Clear;
end;

function TDBListPascalString.Count: Integer;
begin
  Result := FHashListBuff.Count;
end;

function TDBListPascalString.GetItems(const index: Integer): TDBEnginePascalString;
begin
  Result := FHashListBuff[index] as TDBEnginePascalString;
end;

function TDBListPascalString.Add: TDBEnginePascalString;
begin
  Result := TDBEnginePascalString.Create;
  Result.FDBStorePos := -1;
  Result.dbEng := nil;
  FHashListBuff.Add(Result);
end;

procedure TDBListPascalString.Add(Value: TDBEnginePascalString);
begin
  FHashListBuff.Add(Value);
end;

procedure TDBListPascalString.Add(const Value: TPascalString);
var
  t: TDBEnginePascalString;
begin
  t := Add;
  t.Buff := Value;
end;

procedure TDBListPascalString.ImportTextStream(stream: TCoreClassStream);
var
  lst: TListPascalString;
  i: Integer;
begin
  lst := TListPascalString.Create;
  lst.LoadFromStream(stream);
  for i := 0 to lst.Count - 1 do
      Add(lst[i]);
  DisposeObject(lst);
end;

procedure TDBListPascalString.LoadFromStoreEngine(dbEng: TDBStore);
var
  itmSearHnd: THeader;
  qState: TQueryState;
begin
  Clear;
  qState.QueryHnd := @itmSearHnd;
  if dbEng.QueryFirst(qState) then
    begin
      repeat
        if qState.ID = c_PascalString then
            FHashListBuff.Add(dbEng.BuildPascalString(qState.StorePos));
      until not dbEng.QueryNext(qState);
    end;
end;

procedure TDBListPascalString.ExportToStoreEngine(dbEng: TDBStore);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      dbEng.AddData(GetItems(i));
end;

function TQueryState.ID: Cardinal;
begin
  if QueryHnd <> nil then
      Result := QueryHnd^.UserProperty
  else
      Result := 0;
end;

function TQueryState.IsDF: Boolean;
begin
  Result := ID = c_DF;
end;

function TQueryState.IsVL: Boolean;
begin
  Result := ID = c_VL;
end;

function TQueryState.IsVT: Boolean;
begin
  Result := ID = c_VT;
end;

function TQueryState.IsTE: Boolean;
begin
  Result := ID = c_TE;
end;

function TQueryState.IsJson: Boolean;
begin
  Result := ID = c_Json;
end;

function TQueryState.IsString: Boolean;
begin
  Result := ID = c_PascalString;
end;

function TQueryState.IsOther: Boolean;
begin
  Result := not(ID in [c_DF, c_VL, c_TE, c_Json, c_PascalString]);
end;

function TQueryState.IsFirst: Boolean;
begin
  Result := (QueryHnd <> nil) and (QueryHnd^.PositionID in [DB_Header_First, DB_Header_1]);
end;

function TQueryState.IsLast: Boolean;
begin
  Result := (QueryHnd <> nil) and (QueryHnd^.PositionID in [DB_Header_Last, DB_Header_1]);
end;

function TQueryState.Cache: TDBCacheStream64;
begin
  Result := Eng.GetCacheStream(StorePos);
end;

function TQueryState.NextCache: TDBCacheStream64;
begin
  if (QueryHnd <> nil) and (QueryHnd^.PositionID in [DB_Header_First, DB_Header_Medium]) then
      Result := Eng.GetCacheStream(QueryHnd^.NextHeader)
  else
      Result := nil;
end;

function TQueryState.PrevCache: TDBCacheStream64;
begin
  if (QueryHnd <> nil) and (QueryHnd^.PositionID in [DB_Header_Last, DB_Header_Medium]) then
      Result := Eng.GetCacheStream(QueryHnd^.PrevHeader)
  else
      Result := nil;
end;

function TQueryState.NextPos: Int64;
begin
  if (QueryHnd <> nil) and (QueryHnd^.PositionID in [DB_Header_First, DB_Header_Medium]) then
      Result := QueryHnd^.NextHeader
  else
      Result := 0;
end;

function TQueryState.PrevPos: Int64;
begin
  if (QueryHnd <> nil) and (QueryHnd^.PositionID in [DB_Header_Last, DB_Header_Medium]) then
      Result := QueryHnd^.PrevHeader
  else
      Result := 0;
end;

procedure TQueryTask.DoTriggerQuery;
begin
  try
{$IFDEF FPC}
    if Assigned(FOnQueryCall) then
        FOnQueryCall(FState);
    if Assigned(FOnQueryMethod) then
        FOnQueryMethod(FState);
{$ELSE}
    if Assigned(FOnQueryCall) then
        FOnQueryCall(FState);
    if Assigned(FOnQueryMethod) then
        FOnQueryMethod(FState);
    if Assigned(FOnQueryProc) then
        FOnQueryProc(FState);
{$ENDIF}
  except
  end;
end;

procedure TQueryTask.DoQueryDone;
begin
  try
{$IFDEF FPC}
    if Assigned(FOnQueryDoneCall) then
        FOnQueryDoneCall();
    if Assigned(FOnQueryDoneMethod) then
        FOnQueryDoneMethod();
{$ELSE}
    if Assigned(FOnQueryDoneCall) then
        FOnQueryDoneCall();
    if Assigned(FOnQueryDoneMethod) then
        FOnQueryDoneMethod();
    if Assigned(FOnQueryDoneProc) then
        FOnQueryDoneProc();
{$ENDIF}
  except
  end;
end;

constructor TQueryTask.Create;
begin
  inherited Create;
  FDBEng := nil;
  FInited := False;
  FReverse := False;
  Init_THeader(FItmSrHnd);
  FState.StorePos := 0;
  FState.QueryHnd := @FItmSrHnd;
  FState.Aborted := False;
  FState.index := -1;

  FTriggerTime := 0;
  FTaskTag := '';

  FLastTime := 0;

  FStoped := False;
  FPaused := False;
  FProcessQueryDone := False;
  FSyncTrigger := True;

{$IFDEF FPC}
  FOnQueryCall := nil;
  FOnQueryMethod := nil;
  FOnQueryDoneCall := nil;
  FOnQueryDoneMethod := nil;
{$ELSE}
  FOnQueryCall := nil;
  FOnQueryMethod := nil;
  FOnQueryProc := nil;
  FOnQueryDoneCall := nil;
  FOnQueryDoneMethod := nil;
  FOnQueryDoneProc := nil;
{$ENDIF}
end;

procedure TQueryTask.stop;
begin
  FStoped := True;
end;

procedure TQueryTask.Pause;
begin
  FPaused := True;
end;

procedure TQueryTask.Play;
begin
  FPaused := False;
end;

function TQueryTask.ProcessQuery(): Boolean;
var
  TT: TTimeTick;
begin
  Result := False;
  if FStoped or FProcessQueryDone then
    begin
      zDBthSync(FDBEng.FQueryThread, FSyncTrigger, {$IFDEF FPC}@{$ENDIF FPC}DoQueryDone);
      Exit;
    end;

  if FPaused then
    begin
      Result := True;
      Exit;
    end;
  TT := GetTimeTick;

  if FInited then
    begin
      FState.NewTime := TT - FTriggerTime;
      FState.deltaTime := TT - FLastTime;

      if FReverse then
        begin
          if not FDBEng.QueryPrev(FState) then
            begin
              zDBthSync(FDBEng.FQueryThread, FSyncTrigger, {$IFDEF FPC}@{$ENDIF FPC}DoQueryDone);
              Exit;
            end;
          dec(FState.index);
          zDBthSync(FDBEng.FQueryThread, FSyncTrigger, {$IFDEF FPC}@{$ENDIF FPC}DoTriggerQuery);
          if FState.Aborted then
            begin
              zDBthSync(FDBEng.FQueryThread, FSyncTrigger, {$IFDEF FPC}@{$ENDIF FPC}DoQueryDone);
              Exit;
            end;
          Result := True;
        end
      else
        begin
          if not FDBEng.QueryNext(FState) then
            begin
              zDBthSync(FDBEng.FQueryThread, FSyncTrigger, {$IFDEF FPC}@{$ENDIF FPC}DoQueryDone);
              Exit;
            end;
          inc(FState.index);
          zDBthSync(FDBEng.FQueryThread, FSyncTrigger, {$IFDEF FPC}@{$ENDIF FPC}DoTriggerQuery);
          if FState.Aborted then
            begin
              zDBthSync(FDBEng.FQueryThread, FSyncTrigger, {$IFDEF FPC}@{$ENDIF FPC}DoQueryDone);
              Exit;
            end;
          Result := True;
        end;

      FLastTime := GetTimeTick;
    end
  else
    begin
      FTriggerTime := TT;
      FLastTime := FTriggerTime;

      if FReverse then
        begin
          if not FDBEng.QueryLast(FState) then
            begin
              zDBthSync(FDBEng.FQueryThread, FSyncTrigger, {$IFDEF FPC}@{$ENDIF FPC}DoQueryDone);
              Exit;
            end;
          FState.index := FDBEng.Count - 1;
          zDBthSync(FDBEng.FQueryThread, FSyncTrigger, {$IFDEF FPC}@{$ENDIF FPC}DoTriggerQuery);
          if FState.Aborted then
            begin
              zDBthSync(FDBEng.FQueryThread, FSyncTrigger, {$IFDEF FPC}@{$ENDIF FPC}DoQueryDone);
              Exit;
            end;
          Result := True;
        end
      else
        begin
          if not FDBEng.QueryFirst(FState) then
            begin
              zDBthSync(FDBEng.FQueryThread, FSyncTrigger, {$IFDEF FPC}@{$ENDIF FPC}DoQueryDone);
              Exit;
            end;
          FState.index := 0;
          zDBthSync(FDBEng.FQueryThread, FSyncTrigger, {$IFDEF FPC}@{$ENDIF FPC}DoTriggerQuery);
          if FState.Aborted then
            begin
              zDBthSync(FDBEng.FQueryThread, FSyncTrigger, {$IFDEF FPC}@{$ENDIF FPC}DoQueryDone);
              Exit;
            end;
          Result := True;
        end;

      if Result then
        begin
          FInited := True;
          FState.TaskTag := FTaskTag;
          FState.NewTime := GetTimeTick - FTriggerTime;
          FState.deltaTime := FState.NewTime;
        end;
    end;
end;

function TQueryTask.ConsumTime: Double;
begin
  Result := FState.NewTime * 0.001;
end;

procedure TQueryThread.PickQueryQueue;
var
  i: Integer;
begin
  if (PickedQueryQueue.Count <> StoreEngine.FQueryQueue.Count) then
      PickedQueryQueue.Count := StoreEngine.FQueryQueue.Count;
  for i := 0 to StoreEngine.FQueryQueue.Count - 1 do
      PickedQueryQueue[i] := StoreEngine.FQueryQueue[i];
end;

procedure TQueryThread.AsyncQuery;
var
  i: Integer;
  QT: TQueryTask;
begin
  if StoreEngine = nil then
      Exit;

  zDBthSync(Self, True, {$IFDEF FPC}@{$ENDIF FPC}PickQueryQueue);

  i := 0;
  for i := 0 to PickedQueryQueue.Count - 1 do
    begin
      QT := TQueryTask(PickedQueryQueue[i]);
      QT.FProcessQueryDone := not QT.ProcessQuery;
    end;

  zDBthSync(Self, True, {$IFDEF FPC}@{$ENDIF FPC}SyncQueryDone);

  Paused := (StoreEngine.FQueryQueue.Count = 0) and (RemoveQueue.Count = 0);

  if Paused then
    begin
      StoreEngine.FQueryThreadLastActivtedTime := Now;
      zDBthSync(Self, True, {$IFDEF FPC}@{$ENDIF FPC}SyncUpdateCacheState);
    end;
end;

procedure TQueryThread.SyncQueryDone;
var
  i: Integer;
  QT: TQueryTask;
begin
  i := 0;
  while i < StoreEngine.FQueryQueue.Count do
    begin
      QT := TQueryTask(StoreEngine.FQueryQueue[i]);

      if QT.FProcessQueryDone then
        begin
          DisposeObject(QT);
          StoreEngine.FQueryQueue.Delete(i);
        end
      else
          inc(i);
    end;
  if StoreEngine.FQueryQueue.Count = 0 then
      SyncRemove;
end;

procedure TQueryThread.SyncRemove;
var
  i: Integer;
  p: PInt64HashListPointerStruct;
  triggerPtr: PRemoveQueueData;
  removed: Boolean;
begin
  RemoveCompletedQueue.Clear;

  if RemoveQueue.Count > 0 then
    begin
      i := 0;
      p := RemoveQueue.FirstPtr;
      while i < RemoveQueue.Count do
        begin
          triggerPtr := p^.Data;

          if RemoveCompletedQueue.Exists(p^.i64) then
              removed := True
          else
            begin
              removed := StoreEngine.Internal_DeleteData(p^.i64);
              RemoveCompletedQueue.Add(p^.i64, triggerPtr, True);
            end;

          if triggerPtr <> nil then
            begin
              try
                if Assigned(triggerPtr^.OnRemoveCall) then
                    triggerPtr^.OnRemoveCall(p^.i64, removed);
                if Assigned(triggerPtr^.OnRemoveMethod) then
                    triggerPtr^.OnRemoveMethod(p^.i64, removed);
{$IFNDEF FPC}
                if Assigned(triggerPtr^.OnRemoveProc) then
                    triggerPtr^.OnRemoveProc(p^.i64, removed);
{$ENDIF FPC}
              except
              end;
            end;

          inc(i);
          p := p^.Next;
        end;
    end;
  RemoveQueue.Clear;
  RemoveCompletedQueue.Clear;
end;

procedure TQueryThread.SyncCheckCache;
var
  Allowed: Boolean;
begin
  if StoreEngine = nil then
      Exit;

  Allowed := (StoreEngine.FUsedInstanceCacheMemory > StoreEngine.FMinimizeCacheMemorySize);

  if PausedIdleTime > StoreEngine.CacheAnnealingTime then
    begin
      PausedIdleTime := 0;
      if Allowed then
        begin
          StoreEngine.FCacheAnnealingState := Format('cleanup instance:%d(%s) stream:%d(%s)',
            [
            StoreEngine.FCache.Count,
            umlSizeToStr(StoreEngine.FUsedInstanceCacheMemory).Text,
            StoreEngine.FStreamCache.Count,
            umlSizeToStr(StoreEngine.FUsedStreamCacheMemory).Text
            ]);
          StoreEngine.Recache;
        end;
    end
  else if Allowed then
      StoreEngine.FCacheAnnealingState := Format('Annealing Cooldown %d instance:%s stream:%s',
      [
      Round(StoreEngine.CacheAnnealingTime - PausedIdleTime),
      umlSizeToStr(StoreEngine.FUsedInstanceCacheMemory).Text,
      umlSizeToStr(StoreEngine.FUsedStreamCacheMemory).Text
      ]);
end;

procedure TQueryThread.SyncUpdateCacheState;
begin
  if StoreEngine <> nil then
      StoreEngine.FCacheAnnealingState := Format('instance:%d(%s) stream:%d(%s)',
      [
      StoreEngine.FCache.Count,
      umlSizeToStr(StoreEngine.FUsedInstanceCacheMemory).Text,
      StoreEngine.FStreamCache.Count,
      umlSizeToStr(StoreEngine.FUsedStreamCacheMemory).Text
      ]);
end;

procedure TQueryThread.Execute;
var
  cloop: NativeInt;
begin
  cloop := 0;
  while StoreEngine <> nil do
    begin
      PausedIdleTime := 0;
      while Paused do
        begin
          Sleep(10);
          PausedIdleTime := PausedIdleTime + 0.01;

          zDBthSync(Self, True, {$IFDEF FPC}@{$ENDIF FPC}SyncCheckCache);
        end;

      AsyncQuery();
      if (cloop = 0) or (cloop > 1000) then
        begin
          cloop := 0;
          zDBthSync(Self, True, {$IFDEF FPC}@{$ENDIF FPC}SyncUpdateCacheState);
        end;

      inc(cloop);
    end;
end;

constructor TQueryThread.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
  Paused := True;

  RemoveQueue := TInt64HashPointerList.CustomCreate(1024);
  RemoveQueue.AutoFreeData := True;
  RemoveQueue.OnFreePtr := {$IFDEF FPC}@{$ENDIF FPC}RemoveDeleteProc;
  RemoveCompletedQueue := TInt64HashPointerList.CustomCreate(1024);
  RemoveCompletedQueue.AutoFreeData := False;
  PickedQueryQueue := TCoreClassListForObj.Create;
end;

destructor TQueryThread.Destroy;
begin
  DisposeObject([RemoveQueue, RemoveCompletedQueue]);
  DisposeObject(PickedQueryQueue);
  inherited Destroy;
end;

procedure TQueryThread.RemoveDeleteProc(p: Pointer);
begin
  if p <> nil then
      Dispose(PRemoveQueueData(p));
end;

procedure TQueryThread.PostRemoveQueue(StorePos: Int64);
begin
  RemoveQueue.Add(StorePos, nil, False);
end;

procedure TQueryThread.PostRemoveQueueC(StorePos: Int64; OnRemove: TRemoveCall);
var
  p: PRemoveQueueData;
begin
  new(p);
  p^.OnRemoveCall := OnRemove;
  p^.OnRemoveMethod := nil;
  p^.OnRemoveProc := nil;
  RemoveQueue.Add(StorePos, p, False);
end;

procedure TQueryThread.PostRemoveQueueM(StorePos: Int64; OnRemove: TRemoveMethod);
var
  p: PRemoveQueueData;
begin
  new(p);
  p^.OnRemoveCall := nil;
  p^.OnRemoveMethod := OnRemove;
  p^.OnRemoveProc := nil;
  RemoveQueue.Add(StorePos, p, False);
end;

procedure TQueryThread.PostRemoveQueueP(StorePos: Int64; OnRemove: TRemoveProc);
var
  p: PRemoveQueueData;
begin
  new(p);
  p^.OnRemoveCall := nil;
  p^.OnRemoveMethod := nil;
  p^.OnRemoveProc := OnRemove;
  RemoveQueue.Add(StorePos, p, False);
end;

constructor TDBCacheStream64.Create;
begin
  inherited Create;
  OwnerEng := nil;
  OwnerCache := nil;
  ID := 0;
  CreateTime := 0;
  ModificationTime := 0;
  StorePos := -1;
  UsedMemorySize := 0;
end;

destructor TDBCacheStream64.Destroy;
begin
  if OwnerCache <> nil then
    begin
      OwnerCache.AutoFreeData := False;
      OwnerCache.Delete(StorePos);
      OwnerCache.AutoFreeData := True;
    end;
  if OwnerEng <> nil then
      dec(OwnerEng.FUsedStreamCacheMemory, UsedMemorySize);
  inherited Destroy;
end;

procedure TDBStore.ReadHeaderInfo;
var
  f: TFieldHandle;
begin
  if not FDBEngine.GetPathField('/Store', FStoreFieldPos) then
    begin
      if FDBEngine.IsOnlyRead then
          RaiseInfo('/Store field error!');
      if not(FDBEngine.CreateField('/Store', '') and
        FDBEngine.GetPathField('/Store', FStoreFieldPos)) then
          RaiseInfo('reinit /Store field error!');
    end;

  if not FDBEngine.GetFieldData(FStoreFieldPos, f) then
      RaiseInfo('store field data failed!');

  FCount := f.HeaderCount;
end;

procedure TDBStore.ThreadFreeEvent(Sender: TObject);
begin
  FQueryThreadTerminate := True;
end;

procedure TDBStore.DoCreateInit;
begin
  FQueryQueue := TCoreClassListForObj.Create;

  FQueryThread := TQueryThread.Create(True);
  FQueryThread.StoreEngine := Self;

  FQueryThreadTerminate := False;
  FQueryThreadLastActivtedTime := Now;

  FNotifyIntf := nil;

  FCache := TInt64HashObjectList.CustomCreate(DefaultCacheBufferLength);
  FCache.AutoFreeData := True;
  FStreamCache := TInt64HashObjectList.CustomCreate(DefaultCacheBufferLength);
  FStreamCache.AutoFreeData := True;
  FStreamCache.AccessOptimization := True;

  FUsedInstanceCacheMemory := 0;
  FCacheStyle := TCacheStyle.csAutomation;
  FCacheAnnealingTime := DefaultCacheAnnealingTime;
  FMaximumCacheMemorySize := DefaultMaximumInstanceCacheSize;
  FMinimizeCacheMemorySize := DefaultMinimizeInstanceCacheSize;
  FMinimizeStreamCacheMemorySize := DefaultMinimizeStreamCacheSize;
  FMaximumStreamCacheMemorySize := DefaultMaximumStreamCacheSize;
  FUsedStreamCacheMemory := 0;
  FMinimizeCacheOfFileSize := DefaultMinimizeCacheOfFileSize;
  FCacheAnnealingState := '';

  FCache.OnObjectFreeProc := {$IFDEF FPC}@{$ENDIF FPC}InstanceCacheObjectFreeProc;
  FStreamCache.OnObjectFreeProc := {$IFDEF FPC}@{$ENDIF FPC}StreamCacheObjectFreeProc;

  FResultDF := TDBEngineDF.Create;
  FResultVL := TDBEngineVL.Create;
  FResultVT := TDBEngineVT.Create;
  FResultTE := TDBEngineTE.Create;
{$IFNDEF FPC}
  FResultJson := TDBEngineJson.Create;
{$ENDIF}
  FResultPascalString := TDBEnginePascalString.Create;

  FQueryThread.OnTerminate := {$IFDEF FPC}@{$ENDIF FPC}ThreadFreeEvent;
  FQueryThread.Suspended := False;

  FUserPointer := nil;
  FUserObject := nil;
  FUserString := '';
end;

procedure TDBStore.InstanceCacheObjectFreeProc(Obj: TCoreClassObject);
begin
  if Obj is TDBEngineDF then
      dec(FUsedInstanceCacheMemory, TDBEngineDF(Obj).MemoryUsed)
  else if Obj is TDBEngineVL then
      dec(FUsedInstanceCacheMemory, TDBEngineVL(Obj).MemoryUsed)
  else if Obj is TDBEngineVT then
      dec(FUsedInstanceCacheMemory, TDBEngineVT(Obj).MemoryUsed)
  else if Obj is TDBEngineTE then
      dec(FUsedInstanceCacheMemory, TDBEngineTE(Obj).MemoryUsed)
{$IFNDEF FPC}
  else if Obj is TDBEngineJson then
      dec(FUsedInstanceCacheMemory, TDBEngineJson(Obj).MemoryUsed)
{$ENDIF}
  else if Obj is TDBEnginePascalString then
      dec(FUsedInstanceCacheMemory, TDBEnginePascalString(Obj).MemoryUsed)
  else
      RaiseInfo('unknow class info.');

  DisposeObject(Obj);
end;

procedure TDBStore.ProcessNewInstanceCache(StorePos: Int64; Obj: TCoreClassObject; siz: NativeInt);
begin
  FCache.Add(StorePos, Obj, False);
  inc(FUsedInstanceCacheMemory, siz);

  if FCacheStyle = TCacheStyle.csAlways then
      Exit;

  if (FUsedInstanceCacheMemory > FMaximumCacheMemorySize) then
    while (FUsedInstanceCacheMemory > FMinimizeCacheMemorySize) do
        FCache.DeleteFirst;
end;

procedure TDBStore.StreamCacheObjectFreeProc(Obj: TCoreClassObject);
begin
  try
    TDBCacheStream64(Obj).OwnerCache := nil;
    DisposeObject(Obj);
  except
  end;
end;

procedure TDBStore.ProcessNewStreamCache(M: TDBCacheStream64);
begin
  FStreamCache.Add(M.StorePos, M, False);
  M.UsedMemorySize := M.Size;
  inc(FUsedStreamCacheMemory, M.UsedMemorySize);

  if (FUsedStreamCacheMemory > FMaximumStreamCacheMemorySize) then
    while (FUsedStreamCacheMemory > FMinimizeStreamCacheMemorySize) do
        FStreamCache.DeleteFirst;
end;

function TDBStore.Internal_DeleteData(const StorePos: Int64): Boolean;
var
  itmHnd: TItemHandle;
begin
  Result := False;
  if IsReadOnly then
      Exit;

  FCache.Delete(StorePos);
  FStreamCache.Delete(StorePos);

  Result := FDBEngine.FastDelete(FStoreFieldPos, StorePos);
  if Result then
    begin
      dec(FCount);

      try
        if Assigned(FNotifyIntf) then
            FNotifyIntf.DoDeleteData(Self, StorePos);
      except
      end;
    end;
end;

constructor TDBStore.Create(dbFile: SystemString; OnlyRead: Boolean);
begin
  inherited Create;
  FDBEngine := TObjectDataManagerOfCache.Open(dbFile, ObjectDataMarshal.ID, OnlyRead);
  ReadHeaderInfo;
  DoCreateInit;
end;

constructor TDBStore.CreateMemory(DBMemory: TMemoryStream64; OnlyRead: Boolean);
begin
  inherited Create;
  FDBEngine := TObjectDataManagerOfCache.CreateAsStream(DBMemory, '', ObjectDataMarshal.ID, OnlyRead, False, True);
  ReadHeaderInfo;
  DoCreateInit;
end;

constructor TDBStore.CreateNew(dbFile: SystemString);
begin
  inherited Create;
  FDBEngine := TObjectDataManagerOfCache.CreateNew(dbFile, ObjectDataMarshal.ID);
  FDBEngine.CreateField('/Store', '');
  ReadHeaderInfo;
  DoCreateInit;
  Update;
end;

constructor TDBStore.CreateNewMemory;
begin
  inherited Create;
  FDBEngine := TObjectDataManagerOfCache.CreateAsStream(TMemoryStream64.Create, '', ObjectDataMarshal.ID, False, True, True);
  FDBEngine.CreateField('/Store', '');
  ReadHeaderInfo;
  DoCreateInit;
  Update;
end;

destructor TDBStore.Destroy;
var
  i: Integer;
begin
  FQueryThread.StoreEngine := nil;
  FQueryThread.Paused := False;

  // wait thread
  while not FQueryThreadTerminate do
      CheckThreadSynchronize;

  for i := 0 to FQueryQueue.Count - 1 do
      DisposeObject(FQueryQueue[i]);
  DisposeObject([FDBEngine, FQueryQueue, FCache, FStreamCache]);
  DisposeObject([FResultDF, FResultVL, FResultVT, FResultTE, FResultPascalString]);
{$IFNDEF FPC}
  DisposeObject(FResultJson);
{$ENDIF}
  inherited Destroy;
end;

procedure TDBStore.CompressTo(DestDB: TObjectDataManager);
begin
  Update;

  // DoStatus('build struct...');
  DestDB.CreateField('/Store', '');

  // DoStatus('compress data...');
  FDBEngine.CopyFieldToPath(FStoreFieldPos, DestDB, '/Store');

  DestDB.UpdateIO;
end;

procedure TDBStore.Compress;
var
  DestDB: TObjectDataManagerOfCache;
  fn, oldFN: SystemString;
  i: Integer;
begin
  StopAllQuery;
  Recache;

  if FDBEngine.StreamEngine <> nil then
    begin
      DestDB := TObjectDataManagerOfCache.CreateAsStream(FDBEngine.Handle^.IOHnd.FixedStringL, TMemoryStream64.Create, '', ObjectDataMarshal.ID, False, True, True);
      DestDB.OverWriteItem := False;
      CompressTo(DestDB);
      DisposeObject([FDBEngine]);
      FDBEngine := DestDB;
      ReadHeaderInfo;
    end
  else
    begin
      oldFN := FDBEngine.ObjectName;
      i := 0;
      repeat
        inc(i);
        fn := umlChangeFileExt(FDBEngine.ObjectName, '.~' + IntToStr(i)).Text;
      until not umlFileExists(fn);
      DestDB := TObjectDataManagerOfCache.CreateNew(FDBEngine.Handle^.IOHnd.FixedStringL, fn, ObjectDataMarshal.ID);
      DestDB.OverWriteItem := False;
      CompressTo(DestDB);
      DisposeObject([FDBEngine, DestDB]);

      umlDeleteFile(oldFN);
      umlRenameFile(fn, oldFN);

      FDBEngine := TObjectDataManagerOfCache.Open(oldFN, ObjectDataMarshal.ID, False);
      ReadHeaderInfo;
    end;
end;

procedure TDBStore.Update;
begin
  FDBEngine.UpdateIO;
end;

procedure TDBStore.SaveToStream(stream: TCoreClassStream);
var
  DestDB: TObjectDataManager;
begin
  DestDB := TObjectDataManager.CreateAsStream(stream, '', ObjectDataMarshal.ID, False, True, False);
  CompressTo(DestDB);
  DisposeObject(DestDB);
end;

procedure TDBStore.SaveToFile(fn: SystemString);
var
  DestDB: TObjectDataManager;
begin
  DestDB := TObjectDataManager.CreateNew(fn, ObjectDataMarshal.ID);
  CompressTo(DestDB);
  DisposeObject(DestDB);
end;

procedure TDBStore.LoadFromStream(stream: TCoreClassStream);
var
  DestDB: TObjectDataManager;
begin
  StopAllQuery;
  Recache;

  FDBEngine.FieldDelete('/', 'Store');
  FDBEngine.CreateField('/Store', '');
  ReadHeaderInfo;
  Compress;

  DestDB := TObjectDataManager.CreateAsStream(stream, '', ObjectDataMarshal.ID, True, False, False);
  DestDB.CopyFieldToPath(DestDB.GetPathFieldPos('/Store'), FDBEngine, '/Store');
  DisposeObject(DestDB);
end;

procedure TDBStore.LoadFromFile(fn: SystemString);
var
  fs: TCoreClassFileStream;
begin
  if not umlFileExists(fn) then
      Exit;
  fs := TCoreClassFileStream.Create(fn, fmOpenRead or fmShareDenyNone);
  try
      LoadFromStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

function TDBStore.IsMemoryMode: Boolean;
begin
  Result := FDBEngine.StreamEngine is TMemoryStream64;
end;

function TDBStore.IsReadOnly: Boolean;
begin
  Result := FDBEngine.IsOnlyRead;
end;

procedure TDBStore.ResetDB;
begin
  StopAllQuery;
  Recache;

  FDBEngine.FieldDelete('/', 'Store');
  FDBEngine.CreateField('/Store', '');
  ReadHeaderInfo;
  Compress;
end;

function TDBStore.RenameDB(NewName: SystemString): Boolean;
var
  oldFN, newfn: SystemString;
begin
  StopAllQuery;
  Recache;

  Result := False;
  if FDBEngine.IsOnlyRead then
      Exit;

  if IsMemoryMode then
    begin
      FDBEngine.ObjectName := NewName;
      Result := True;
      Exit;
    end;
  oldFN := FDBEngine.ObjectName;
  if not umlFileExists(oldFN) then
      Exit;

  FDBEngine.Flush;
  DisposeObject(FDBEngine);

  newfn := umlCombineFileName(umlGetFilePath(oldFN), NewName).Text;

  if umlRenameFile(oldFN, newfn) then
    begin
      oldFN := newfn;
      Result := True;
    end;

  FDBEngine := TObjectDataManagerOfCache.Open(oldFN, ObjectDataMarshal.ID, False);
  ReadHeaderInfo;
end;

procedure TDBStore.Recache;
begin
  FCache.Clear;
  FDBEngine.CleaupCache;
  FStreamCache.Clear;

  FResultDF.Clear;
  FResultVL.Clear;
  FResultVT.Clear;
  FResultTE.Clear;
{$IFNDEF FPC}
  FResultJson.Clear;
{$ENDIF}
  FResultPascalString.Clear;

  FQueryThread.SyncUpdateCacheState;
end;

function TDBStore.AllowedCache: Boolean;
begin
  case FCacheStyle of
    TCacheStyle.csAutomation:
      begin
        if (FUsedStreamCacheMemory > FMaximumStreamCacheMemorySize) then
            Result := False
        else if (FUsedInstanceCacheMemory < FMinimizeCacheMemorySize) then
            Result := True
        else if (FQueryQueue.Count >= 2) and (FUsedInstanceCacheMemory < FMaximumCacheMemorySize) then
            Result := True
        else if FDBEngine.Size < FMinimizeCacheOfFileSize then
            Result := True
        else
            Result := False;
      end;
    TCacheStyle.csNever: Result := False;
    else Result := True;
  end;
end;

procedure TDBStore.DeleteData(const StorePos: Int64);
begin
  FQueryThread.PostRemoveQueue(StorePos);
  FQueryThread.Paused := False;
  FQueryThreadLastActivtedTime := Now;
end;

function TDBStore.InsertData(const InsertPos: Int64; Buff: TCoreClassStream; ID: Cardinal; var itmHnd: TItemHandle): Int64;
var
  itmStream: TItemStream;
begin
  Result := -1;

  if FDBEngine.ItemFastInsertNew(FStoreFieldPos, InsertPos, '', '', itmHnd) then
    begin
      itmHnd.Item.RHeader.UserProperty := ID;
      itmHnd.Name := '0x' + TCipher.BuffToString(@itmHnd.Item.RHeader.CurrentHeader, C_Int64_Size);
      itmStream := TItemStream.Create(FDBEngine, itmHnd);
      Buff.Position := 0;
      itmStream.CopyFrom(Buff, Buff.Size);
      itmStream.UpdateHandle;
      DisposeObject(itmStream);
      Result := itmHnd.Item.RHeader.CurrentHeader;
      inc(FCount);

      try
        if Assigned(FNotifyIntf) then
            FNotifyIntf.DoInsertData(Self, InsertPos, Buff, ID, Result);
      except
      end;
      FQueryThread.Paused := False;
      FQueryThreadLastActivtedTime := Now;
    end;
end;

function TDBStore.InsertData(const InsertPos: Int64; Buff: TCoreClassStream; ID: Cardinal): Int64;
var
  itmHnd: TItemHandle;
begin
  Result := InsertData(InsertPos, Buff, ID, itmHnd);
end;

function TDBStore.AddData(Buff: TCoreClassStream; ID: Cardinal; var itmHnd: TItemHandle): Int64;
var
  itmStream: TItemStream;
begin
  Result := -1;

  if IsReadOnly then
      Exit;

  if FDBEngine.ItemFastCreate(FStoreFieldPos, '', '', itmHnd) then
    begin
      itmHnd.Item.RHeader.UserProperty := ID;
      itmHnd.Name := '0x' + TCipher.BuffToString(@itmHnd.Item.RHeader.CurrentHeader, C_Int64_Size);
      itmStream := TItemStream.Create(FDBEngine, itmHnd);
      Buff.Position := 0;
      itmStream.CopyFrom(Buff, Buff.Size);
      itmStream.UpdateHandle;
      DisposeObject(itmStream);
      Result := itmHnd.Item.RHeader.CurrentHeader;
      inc(FCount);

      try
        if Assigned(FNotifyIntf) then
            FNotifyIntf.DoAddData(Self, Buff, ID, Result);
      except
      end;
      FQueryThread.Paused := False;
      FQueryThreadLastActivtedTime := Now;
    end;
end;

function TDBStore.AddData(Buff: TCoreClassStream; ID: Cardinal): Int64;
var
  itmHnd: TItemHandle;
begin
  Result := AddData(Buff, ID, itmHnd);
end;

function TDBStore.SetData(const StorePos: Int64; Buff: TCoreClassStream): Boolean;
var
  itmHnd: TItemHandle;
  itmStream: TItemStream;
begin
  Result := False;

  if IsReadOnly then
      Exit;

  if FDBEngine.ItemFastResetBody(StorePos) then
    if FDBEngine.ItemFastOpen(StorePos, itmHnd) then
      begin
        itmStream := TItemStream.Create(FDBEngine, itmHnd);
        Buff.Position := 0;
        itmStream.CopyFrom(Buff, Buff.Size);
        itmStream.UpdateHandle;
        DisposeObject(itmStream);
        Result := True;

        FCache.Delete(StorePos);
        FStreamCache.Delete(StorePos);

        try
          if Assigned(FNotifyIntf) then
              FNotifyIntf.DoModifyData(Self, StorePos, Buff);
        except
        end;
        FQueryThread.Paused := False;
        FQueryThreadLastActivtedTime := Now;
      end;
end;

function TDBStore.GetCacheStream(const StorePos: Int64; ID: Cardinal): TDBCacheStream64;
var
  itmHnd: TItemHandle;
  itmStream: TItemStream;
begin
  Result := TDBCacheStream64(FStreamCache[StorePos]);
  if Result = nil then
    begin
      itmStream := nil;

      if FDBEngine.ItemFastOpen(StorePos, itmHnd) then
        begin
          if ID = itmHnd.Item.RHeader.UserProperty then
              itmStream := TItemStream.Create(FDBEngine, itmHnd);

          try
            Result := TDBCacheStream64.Create;
            Result.CopyFrom(itmStream, itmStream.Size);
            Result.Position := 0;

            Result.OwnerEng := Self;
            Result.OwnerCache := FStreamCache;
            Result.ID := itmStream.Hnd^.Item.RHeader.UserProperty;
            Result.CreateTime := itmStream.Hnd^.CreateTime;
            Result.ModificationTime := itmStream.Hnd^.ModificationTime;
            Result.StorePos := StorePos;
          finally
              ProcessNewStreamCache(Result);
          end;

          DisposeObject(itmStream);
        end;
    end
  else if Result.ID <> ID then
      Result := nil
  else
      Result.Position := 0;
end;

function TDBStore.GetCacheStream(const StorePos: Int64): TDBCacheStream64;
var
  itmHnd: TItemHandle;
  itmStream: TItemStream;
begin
  Result := TDBCacheStream64(FStreamCache[StorePos]);
  if Result = nil then
    begin
      if FDBEngine.ItemFastOpen(StorePos, itmHnd) then
        begin
          itmStream := TItemStream.Create(FDBEngine, itmHnd);

          try
            Result := TDBCacheStream64.Create;
            Result.CopyFrom(itmStream, itmStream.Size);
            Result.Position := 0;

            Result.OwnerEng := Self;
            Result.OwnerCache := FStreamCache;
            Result.ID := itmStream.Hnd^.Item.RHeader.UserProperty;
            Result.CreateTime := itmStream.Hnd^.CreateTime;
            Result.ModificationTime := itmStream.Hnd^.ModificationTime;
            Result.StorePos := StorePos;
          finally
              ProcessNewStreamCache(Result);
          end;

          DisposeObject(itmStream);
        end;
    end
  else
      Result.Position := 0;
end;

function TDBStore.QueryFirst(var qState: TQueryState): Boolean;
begin
  Result := False;
  qState.Eng := Self;
  qState.StorePos := -1;
  qState.Aborted := False;
  qState.TaskTag := '';
  qState.deltaTime := 0;
  qState.NewTime := 0;
  if qState.QueryHnd = nil then
      Exit;

  try
    Result := FDBEngine.GetFirstHeaderFromField(FStoreFieldPos, qState.QueryHnd^);
    if Result then
        qState.StorePos := qState.QueryHnd^.CurrentHeader;
  except
      Result := False;
  end;
end;

function TDBStore.QueryNext(var qState: TQueryState): Boolean;
begin
  Result := False;

  if qState.QueryHnd = nil then
      Exit;
  if qState.QueryHnd^.PositionID in [DB_Header_Last, DB_Header_1] then
      Exit;

  try
    Result := FDBEngine.GetHeader(qState.QueryHnd^.NextHeader, qState.QueryHnd^);
    if Result then
        qState.StorePos := qState.QueryHnd^.CurrentHeader;
  except
      Result := False;
  end;
end;

function TDBStore.QueryLast(var qState: TQueryState): Boolean;
begin
  Result := False;
  qState.Eng := Self;
  qState.StorePos := -1;
  qState.Aborted := False;
  qState.TaskTag := '';
  qState.deltaTime := 0;
  qState.NewTime := 0;
  if qState.QueryHnd = nil then
      Exit;

  try
    Result := FDBEngine.GetLastHeaderFromField(FStoreFieldPos, qState.QueryHnd^);
    if Result then
        qState.StorePos := qState.QueryHnd^.CurrentHeader;
  except
      Result := False;
  end;
end;

function TDBStore.QueryPrev(var qState: TQueryState): Boolean;
begin
  Result := False;

  if qState.QueryHnd = nil then
      Exit;
  if qState.QueryHnd^.PositionID in [DB_Header_First, DB_Header_1] then
      Exit;

  try
    Result := FDBEngine.GetHeader(qState.QueryHnd^.PrevHeader, qState.QueryHnd^);
    if Result then
        qState.StorePos := qState.QueryHnd^.CurrentHeader;
  except
      Result := False;
  end;
end;

procedure TDBStore.BuildStorePosArray(ReverseBuild: Boolean; const OutputPtr: PStoreArray);
type
  TDynamicQueryMethod = function(var qState: TQueryState): Boolean of object;
var
  itmSearHnd: THeader;
  qState: TQueryState;
  f, n: TDynamicQueryMethod;
begin
  SetLength(OutputPtr^, FCount);

  qState.StorePos := -1;
  qState.Aborted := False;
  qState.QueryHnd := @itmSearHnd;

  if ReverseBuild then
    begin
      f := {$IFDEF FPC}@{$ENDIF FPC}QueryLast;
      n := {$IFDEF FPC}@{$ENDIF FPC}QueryPrev;
      qState.index := Count - 1;
    end
  else
    begin
      f := {$IFDEF FPC}@{$ENDIF FPC}QueryFirst;
      n := {$IFDEF FPC}@{$ENDIF FPC}QueryNext;
      qState.index := 0;
    end;

  if f(qState) then
    begin
      repeat
        OutputPtr^[qState.index] := qState.StorePos;
        if ReverseBuild then
            dec(qState.index)
        else
            inc(qState.index);
      until (not n(qState));
    end;
end;

procedure TDBStore.BuildStoreArray(ReverseBuild: Boolean; const OutputPtr: PStoreArray);
begin
  BuildStorePosArray(ReverseBuild, OutputPtr);
end;

procedure TDBStore.WaitQuery(ReverseQuery: Boolean; const OnQueryCall: TQueryCall; const OnQueryProc: TQueryProc; const OnQueryMethod: TQueryMethod);
type
  TDynamicQueryMethod = function(var qState: TQueryState): Boolean of object;
var
  itmSearHnd: THeader;
  qState: TQueryState;
  f, n: TDynamicQueryMethod;
begin
  qState.StorePos := -1;
  qState.Aborted := False;
  qState.QueryHnd := @itmSearHnd;

  if ReverseQuery then
    begin
      f := {$IFDEF FPC}@{$ENDIF FPC}QueryLast;
      n := {$IFDEF FPC}@{$ENDIF FPC}QueryPrev;
      qState.index := Count - 1;
    end
  else
    begin
      f := {$IFDEF FPC}@{$ENDIF FPC}QueryFirst;
      n := {$IFDEF FPC}@{$ENDIF FPC}QueryNext;
      qState.index := 0;
    end;

  if f(qState) then
    begin
      repeat
        try
          if Assigned(OnQueryCall) then
              OnQueryCall(qState);
          if Assigned(OnQueryProc) then
              OnQueryProc(qState);
          if Assigned(OnQueryMethod) then
              OnQueryMethod(qState);
        except
        end;

        if qState.Aborted then
            Break;

        if ReverseQuery then
            dec(qState.index)
        else
            inc(qState.index);
      until (not n(qState));
    end;
end;

procedure TDBStore.WaitQueryC(ReverseQuery: Boolean; const OnQueryCall: TQueryCall);
begin
  WaitQuery(ReverseQuery, OnQueryCall, nil, nil);
end;

procedure TDBStore.WaitQueryM(ReverseQuery: Boolean; const OnQueryMethod: TQueryMethod);
begin
  WaitQuery(ReverseQuery, nil, nil, OnQueryMethod);
end;

procedure TDBStore.WaitQueryP(ReverseQuery: Boolean; const OnQueryProc: TQueryProc);
begin
  WaitQuery(ReverseQuery, nil, OnQueryProc, nil);
end;

procedure TDBStore.WaitQueryC(const OnQueryCall: TQueryCall);
begin
  WaitQueryC(False, OnQueryCall);
end;

procedure TDBStore.WaitQueryM(const OnQueryMethod: TQueryMethod);
begin
  WaitQueryM(False, OnQueryMethod);
end;

procedure TDBStore.WaitQueryP(const OnQueryProc: TQueryProc);
begin
  WaitQueryP(False, OnQueryProc);
end;

function TDBStore.Query(const TaskTag: SystemString; const ReverseQuery: Boolean;
  const OnQueryCall: TQueryCall; const OnQueryDoneCall: TQueryDoneCall;
  const OnQueryProc: TQueryProc; const OnQueryDoneProc: TQueryDoneProc;
  const OnQueryMethod: TQueryMethod; const OnQueryDoneMethod: TQueryDoneMethod): TQueryTask;
begin
  Result := TQueryTask.Create;
  Result.FDBEng := Self;
  Result.FReverse := ReverseQuery;
  Result.FTaskTag := TaskTag;
  Result.FOnQueryCall := OnQueryCall;
  Result.FOnQueryDoneCall := OnQueryDoneCall;
  Result.FOnQueryProc := OnQueryProc;
  Result.FOnQueryDoneProc := OnQueryDoneProc;
  Result.FOnQueryMethod := OnQueryMethod;
  Result.FOnQueryDoneMethod := OnQueryDoneMethod;

  FQueryQueue.Add(Result);
  FQueryThread.Paused := False;
  FQueryThreadLastActivtedTime := Now;
end;

function TDBStore.QueryC(const TaskTag: SystemString; const ReverseQuery: Boolean; const OnQueryCall: TQueryCall; const OnQueryDoneCall: TQueryDoneCall): TQueryTask;
begin
  Result := Query(TaskTag, ReverseQuery, OnQueryCall, OnQueryDoneCall, nil, nil, nil, nil);
end;

function TDBStore.QueryP(const TaskTag: SystemString; const ReverseQuery: Boolean; const OnQueryProc: TQueryProc; const OnQueryDoneProc: TQueryDoneProc): TQueryTask;
begin
  Result := Query(TaskTag, ReverseQuery, nil, nil, OnQueryProc, OnQueryDoneProc, nil, nil);
end;

function TDBStore.QueryM(const TaskTag: SystemString; const ReverseQuery: Boolean; const OnQueryMethod: TQueryMethod; const OnQueryDoneMethod: TQueryDoneMethod): TQueryTask;
begin
  Result := Query(TaskTag, ReverseQuery, nil, nil, nil, nil, OnQueryMethod, OnQueryDoneMethod);
end;

function TDBStore.QueryC(const TaskTag: SystemString; const OnQueryCall: TQueryCall; const OnQueryDoneCall: TQueryDoneCall): TQueryTask;
begin
  Result := QueryC(TaskTag, False, OnQueryCall, OnQueryDoneCall);
end;

function TDBStore.QueryP(const TaskTag: SystemString; const OnQueryProc: TQueryProc; const OnQueryDoneProc: TQueryDoneProc): TQueryTask;
begin
  Result := QueryP(TaskTag, False, OnQueryProc, OnQueryDoneProc);
end;

function TDBStore.QueryM(const TaskTag: SystemString; const OnQueryMethod: TQueryMethod; const OnQueryDoneMethod: TQueryDoneMethod): TQueryTask;
begin
  Result := QueryM(TaskTag, False, OnQueryMethod, OnQueryDoneMethod);
end;

function TDBStore.QueryC(const OnQueryCall: TQueryCall; const OnQueryDoneCall: TQueryDoneCall): TQueryTask;
begin
  Result := QueryC('', OnQueryCall, OnQueryDoneCall);
end;

function TDBStore.QueryP(const OnQueryProc: TQueryProc; const OnQueryDoneProc: TQueryDoneProc): TQueryTask;
begin
  Result := QueryP('', OnQueryProc, OnQueryDoneProc);
end;

function TDBStore.QueryC(const ReverseQuery: Boolean; const OnQueryCall: TQueryCall; const OnQueryDoneCall: TQueryDoneCall): TQueryTask;
begin
  Result := QueryC('', ReverseQuery, OnQueryCall, OnQueryDoneCall);
end;

function TDBStore.QueryM(const ReverseQuery: Boolean; const OnQueryMethod: TQueryMethod; const OnQueryDoneMethod: TQueryDoneMethod): TQueryTask;
begin
  Result := QueryM('', ReverseQuery, OnQueryMethod, OnQueryDoneMethod);
end;

function TDBStore.QueryP(const ReverseQuery: Boolean; const OnQueryProc: TQueryProc; const OnQueryDoneProc: TQueryDoneProc): TQueryTask;
begin
  Result := QueryP('', ReverseQuery, OnQueryProc, OnQueryDoneProc);
end;

function TDBStore.QueryM(const OnQueryMethod: TQueryMethod; const OnQueryDoneMethod: TQueryDoneMethod): TQueryTask;
begin
  Result := QueryM('', OnQueryMethod, OnQueryDoneMethod);
end;

procedure TDBStore.WaitQueryThread;
begin
  while not FQueryThread.Paused do
      CheckThreadSynchronize(1);
end;

procedure TDBStore.WaitQueryThread(waitTime: TTimeTick);
var
  st: TTimeTick;
begin
  st := GetTimeTick + waitTime;
  while (not FQueryThread.Paused) and (waitTime > 0) and (GetTimeTick < st) do
      CheckThreadSynchronize;
end;

function TDBStore.QueryProcessing: Boolean;
begin
  Result := not FQueryThread.Paused;
end;

procedure TDBStore.StopQuery(const TaskTag: SystemString);
var
  i: Integer;
  t: TQueryTask;
begin
  i := 0;
  while i < FQueryQueue.Count do
    begin
      t := TQueryTask(FQueryQueue[i]);
      if umlMultipleMatch(TaskTag, t.FTaskTag) then
          t.stop;
      inc(i);
    end;
end;

procedure TDBStore.StopAllQuery;
var
  i: Integer;
begin
  for i := 0 to FQueryQueue.Count - 1 do
      TQueryTask(FQueryQueue[i]).stop;
  WaitQueryThread;
end;

function TDBStore.QueryThreadCount: Integer;
begin
  Result := FQueryQueue.Count;
end;

function TDBStore.InsertData(const InsertPos: Int64; Buff: TDataFrameEngine): Int64;
var
  M: TMemoryStream64;
begin
  M := TMemoryStream64.Create;

  Buff.EncodeTo(M, False);

  Result := InsertData(InsertPos, M, c_DF);
  DisposeObject(M);
end;

function TDBStore.AddData(Buff: TDataFrameEngine): Int64;
var
  M: TMemoryStream64;
begin
  M := TMemoryStream64.Create;

  Buff.EncodeTo(M, False);

  Result := AddData(M, c_DF);
  DisposeObject(M);
end;

function TDBStore.GetDF(const StorePos: Int64): TDBEngineDF;
var
  lastAcc: TCoreClassObject;
  M: TDBCacheStream64;
begin
  lastAcc := FCache[StorePos];
  if lastAcc is TDBEngineDF then
    begin
      Result := lastAcc as TDBEngineDF;
      Result.Reader.index := 0;
      Exit;
    end;
  if lastAcc <> nil then
      FCache.Delete(StorePos);

  Result := nil;

  M := GetCacheStream(StorePos, c_DF);
  if M <> nil then
    begin
      M.Position := 0;

      MH_ZDB.BeginMemoryHook;
      try
        if AllowedCache then
            Result := TDBEngineDF.Create
        else
            Result := FResultDF;

        try
            Result.DecodeFrom(M, True);
        except
        end;
        Result.FDBStorePos := StorePos;
        Result.dbEng := Self;
        Result.CreateTime := M.CreateTime;
        Result.ModificationTime := M.ModificationTime;
        Result.MemoryUsed := MH_ZDB.GetHookMemorySize;
        if AllowedCache then
            ProcessNewInstanceCache(StorePos, Result, Result.MemoryUsed);
      finally
          MH_ZDB.EndMemoryHook;
      end;
    end;
end;

function TDBStore.GetDF(var qState: TQueryState): TDBEngineDF;
begin
  Result := GetDF(qState.StorePos);
end;

function TDBStore.BuildDF(const StorePos: Int64): TDBEngineDF;
var
  M: TDBCacheStream64;
begin
  Result := nil;
  M := GetCacheStream(StorePos, c_DF);
  if M <> nil then
    begin
      M.Position := 0;

      MH_ZDB.BeginMemoryHook;
      try
        Result := TDBEngineDF.Create;
        try
            Result.DecodeFrom(M, True);
        except
        end;
        Result.FDBStorePos := StorePos;
        Result.dbEng := Self;
        Result.CreateTime := M.CreateTime;
        Result.ModificationTime := M.ModificationTime;
        Result.MemoryUsed := MH_ZDB.GetHookMemorySize;
      finally
          MH_ZDB.EndMemoryHook;
      end;
    end;
end;

function TDBStore.BuildDF(var qState: TQueryState): TDBEngineDF;
begin
  Result := BuildDF(qState.StorePos);
end;

function TDBStore.InsertData(const InsertPos: Int64; Buff: THashVariantList): Int64;
var
  M: TMemoryStream64;
begin
  M := TMemoryStream64.Create;
  Buff.SaveToStream(M);
  Result := InsertData(InsertPos, M, c_VL);
  DisposeObject(M);
end;

function TDBStore.AddData(Buff: THashVariantList): Int64;
var
  M: TMemoryStream64;
begin
  M := TMemoryStream64.Create;
  Buff.SaveToStream(M);
  Result := AddData(M, c_VL);
  DisposeObject(M);
end;

function TDBStore.GetVL(const StorePos: Int64): TDBEngineVL;
var
  lastAcc: TCoreClassObject;
  M: TDBCacheStream64;
begin
  lastAcc := FCache[StorePos];
  if lastAcc is TDBEngineVL then
    begin
      Result := lastAcc as TDBEngineVL;
      Exit;
    end;

  if lastAcc <> nil then
      FCache.Delete(StorePos);

  Result := nil;

  M := GetCacheStream(StorePos, c_VL);
  if M <> nil then
    begin
      M.Position := 0;

      MH_ZDB.BeginMemoryHook;
      try
        if AllowedCache then
            Result := TDBEngineVL.Create
        else
            Result := FResultVL;

        try
            Result.LoadFromStream(M);
        except
        end;
        Result.FDBStorePos := StorePos;
        Result.dbEng := Self;
        Result.CreateTime := M.CreateTime;
        Result.ModificationTime := M.ModificationTime;
        Result.MemoryUsed := MH_ZDB.GetHookMemorySize;
        if AllowedCache then
            ProcessNewInstanceCache(StorePos, Result, Result.MemoryUsed);
      finally
          MH_ZDB.EndMemoryHook;
      end;
    end;
end;

function TDBStore.GetVL(var qState: TQueryState): TDBEngineVL;
begin
  Result := GetVL(qState.StorePos);
end;

function TDBStore.BuildVL(const StorePos: Int64): TDBEngineVL;
var
  M: TDBCacheStream64;
begin
  Result := nil;
  M := GetCacheStream(StorePos, c_VL);
  if M <> nil then
    begin
      M.Position := 0;
      MH_ZDB.BeginMemoryHook;
      try
        Result := TDBEngineVL.Create;
        try
            Result.LoadFromStream(M);
        except
        end;
        Result.FDBStorePos := StorePos;
        Result.dbEng := Self;
        Result.CreateTime := M.CreateTime;
        Result.ModificationTime := M.ModificationTime;
        Result.MemoryUsed := MH_ZDB.GetHookMemorySize;
      finally
          MH_ZDB.EndMemoryHook;
      end;
    end;
end;

function TDBStore.BuildVL(var qState: TQueryState): TDBEngineVL;
begin
  Result := BuildVL(qState.StorePos);
end;

function TDBStore.InsertData(const InsertPos: Int64; Buff: THashStringList): Int64;
var
  M: TMemoryStream64;
begin
  M := TMemoryStream64.Create;
  Buff.SaveToStream(M);
  Result := InsertData(InsertPos, M, c_VT);
  DisposeObject(M);
end;

function TDBStore.AddData(Buff: THashStringList): Int64;
var
  M: TMemoryStream64;
begin
  M := TMemoryStream64.Create;
  Buff.SaveToStream(M);
  Result := AddData(M, c_VT);
  DisposeObject(M);
end;

function TDBStore.GetVT(const StorePos: Int64): TDBEngineVT;
var
  lastAcc: TCoreClassObject;
  M: TDBCacheStream64;
begin
  lastAcc := FCache[StorePos];
  if lastAcc is TDBEngineVT then
    begin
      Result := lastAcc as TDBEngineVT;
      Exit;
    end;

  if lastAcc <> nil then
      FCache.Delete(StorePos);

  Result := nil;

  M := GetCacheStream(StorePos, c_VT);
  if M <> nil then
    begin
      M.Position := 0;

      MH_ZDB.BeginMemoryHook;
      try
        if AllowedCache then
            Result := TDBEngineVT.Create
        else
            Result := FResultVT;

        try
            Result.LoadFromStream(M);
        except
        end;
        Result.FDBStorePos := StorePos;
        Result.dbEng := Self;
        Result.CreateTime := M.CreateTime;
        Result.ModificationTime := M.ModificationTime;
        Result.MemoryUsed := MH_ZDB.GetHookMemorySize;
        if AllowedCache then
            ProcessNewInstanceCache(StorePos, Result, Result.MemoryUsed);
      finally
          MH_ZDB.EndMemoryHook;
      end;
    end;
end;

function TDBStore.GetVT(var qState: TQueryState): TDBEngineVT;
begin
  Result := GetVT(qState.StorePos);
end;

function TDBStore.BuildVT(const StorePos: Int64): TDBEngineVT;
var
  M: TDBCacheStream64;
begin
  Result := nil;
  M := GetCacheStream(StorePos, c_VT);
  if M <> nil then
    begin
      M.Position := 0;
      MH_ZDB.BeginMemoryHook;
      try
        Result := TDBEngineVT.Create;
        try
            Result.LoadFromStream(M);
        except
        end;
        Result.FDBStorePos := StorePos;
        Result.dbEng := Self;
        Result.CreateTime := M.CreateTime;
        Result.ModificationTime := M.ModificationTime;
        Result.MemoryUsed := MH_ZDB.GetHookMemorySize;
      finally
          MH_ZDB.EndMemoryHook;
      end;
    end;
end;

function TDBStore.BuildVT(var qState: TQueryState): TDBEngineVT;
begin
  Result := BuildVT(qState.StorePos);
end;

function TDBStore.InsertData(const InsertPos: Int64; Buff: TSectionTextData): Int64;
var
  M: TMemoryStream64;
begin
  M := TMemoryStream64.Create;
  Buff.SaveToStream(M);
  Result := InsertData(InsertPos, M, c_TE);
  DisposeObject(M);
end;

function TDBStore.AddData(Buff: TSectionTextData): Int64;
var
  M: TMemoryStream64;
begin
  M := TMemoryStream64.Create;
  Buff.SaveToStream(M);
  Result := AddData(M, c_TE);
  DisposeObject(M);
end;

function TDBStore.GetTE(const StorePos: Int64): TDBEngineTE;
var
  lastAcc: TCoreClassObject;
  M: TDBCacheStream64;
begin
  lastAcc := FCache[StorePos];
  if lastAcc is TDBEngineTE then
    begin
      Result := lastAcc as TDBEngineTE;
      Exit;
    end;

  if lastAcc <> nil then
      FCache.Delete(StorePos);

  Result := nil;

  M := GetCacheStream(StorePos, c_TE);
  if M <> nil then
    begin
      M.Position := 0;

      MH_ZDB.BeginMemoryHook;
      try
        if AllowedCache then
            Result := TDBEngineTE.Create
        else
            Result := FResultTE;

        try
            Result.LoadFromStream(M);
        except
        end;
        Result.FDBStorePos := StorePos;
        Result.dbEng := Self;
        Result.CreateTime := M.CreateTime;
        Result.ModificationTime := M.ModificationTime;
        Result.MemoryUsed := MH_ZDB.GetHookMemorySize;
        if AllowedCache then
            ProcessNewInstanceCache(StorePos, Result, Result.MemoryUsed);
      finally
          MH_ZDB.EndMemoryHook;
      end;
    end;
end;

function TDBStore.GetTE(var qState: TQueryState): TDBEngineTE;
begin
  Result := GetTE(qState.StorePos);
end;

function TDBStore.BuildTE(const StorePos: Int64): TDBEngineTE;
var
  M: TDBCacheStream64;
begin
  Result := nil;
  M := GetCacheStream(StorePos, c_TE);
  if M <> nil then
    begin
      M.Position := 0;
      MH_ZDB.BeginMemoryHook;
      try
        Result := TDBEngineTE.Create;
        try
            Result.LoadFromStream(M);
        except
        end;
        Result.FDBStorePos := StorePos;
        Result.dbEng := Self;
        Result.CreateTime := M.CreateTime;
        Result.ModificationTime := M.ModificationTime;
        Result.MemoryUsed := MH_ZDB.GetHookMemorySize;
      finally
          MH_ZDB.EndMemoryHook;
      end;
    end;
end;

function TDBStore.BuildTE(var qState: TQueryState): TDBEngineTE;
begin
  Result := BuildTE(qState.StorePos);
end;

{$IFNDEF FPC}


function TDBStore.InsertData(const InsertPos: Int64; Buff: TJsonObject): Int64;
var
  M: TMemoryStream64;
begin
  M := TMemoryStream64.Create;
  Buff.SaveToStream(M, True, TEncoding.UTF8, True);
  Result := InsertData(InsertPos, M, c_Json);
  DisposeObject(M);
end;

function TDBStore.AddData(Buff: TJsonObject): Int64;
var
  M: TMemoryStream64;
begin
  M := TMemoryStream64.Create;
  Buff.SaveToStream(M, True, TEncoding.UTF8, True);
  Result := AddData(M, c_Json);
  DisposeObject(M);
end;

function TDBStore.GetJson(const StorePos: Int64): TDBEngineJson;
var
  lastAcc: TCoreClassObject;
  M: TDBCacheStream64;
begin
  lastAcc := FCache[StorePos];
  if lastAcc is TDBEngineJson then
    begin
      Result := lastAcc as TDBEngineJson;
      Exit;
    end;

  if lastAcc <> nil then
      FCache.Delete(StorePos);

  Result := nil;

  M := GetCacheStream(StorePos, c_Json);
  if M <> nil then
    begin
      M.Position := 0;

      MH_ZDB.BeginMemoryHook;
      try
        if AllowedCache then
            Result := TDBEngineJson.Create
        else
            Result := FResultJson;

        try
            Result.LoadFromStream(M, TEncoding.UTF8, False);
        except
        end;
        Result.FDBStorePos := StorePos;
        Result.dbEng := Self;
        Result.CreateTime := M.CreateTime;
        Result.ModificationTime := M.ModificationTime;
        Result.MemoryUsed := MH_ZDB.GetHookMemorySize;
        if AllowedCache then
            ProcessNewInstanceCache(StorePos, Result, Result.MemoryUsed);
      finally
          MH_ZDB.EndMemoryHook;
      end;
    end;
end;

function TDBStore.GetJson(var qState: TQueryState): TDBEngineJson;
begin
  Result := GetJson(qState.StorePos);
end;

function TDBStore.BuildJson(const StorePos: Int64): TDBEngineJson;
var
  M: TDBCacheStream64;
begin
  Result := nil;
  M := GetCacheStream(StorePos, c_Json);
  if M <> nil then
    begin
      M.Position := 0;
      MH_ZDB.BeginMemoryHook;
      try
        Result := TDBEngineJson.Create;
        try
            Result.LoadFromStream(M, TEncoding.UTF8, False);
        except
        end;
        Result.FDBStorePos := StorePos;
        Result.dbEng := Self;
        Result.CreateTime := M.CreateTime;
        Result.ModificationTime := M.ModificationTime;
        Result.MemoryUsed := MH_ZDB.GetHookMemorySize;
      finally
          MH_ZDB.EndMemoryHook;
      end;
    end;
end;

function TDBStore.BuildJson(var qState: TQueryState): TDBEngineJson;
begin
  Result := BuildJson(qState.StorePos);
end;
{$ENDIF}


function TDBStore.InsertData(const InsertPos: Int64; Buff: TDBEnginePascalString): Int64;
var
  M: TMemoryStream64;
begin
  M := TMemoryStream64.Create;
  Buff.SaveToStream(M);
  Result := InsertData(InsertPos, M, c_PascalString);
  DisposeObject(M);
end;

function TDBStore.InsertData(const InsertPos: Int64; Buff: TPascalString): Int64;
begin
  Result := InsertString(InsertPos, Buff);
end;

function TDBStore.InsertString(const InsertPos: Int64; Buff: TPascalString): Int64;
var
  t: TDBEnginePascalString;
  M: TMemoryStream64;
  itmHnd: TItemHandle;
begin
  if AllowedCache then
    begin
      MH_ZDB.BeginMemoryHook;
      t := TDBEnginePascalString.Create;
      t.Buff := Buff;
      t.MemoryUsed := MH_ZDB.GetHookMemorySize;
      MH_ZDB.EndMemoryHook;

      M := TMemoryStream64.Create;
      t.SaveToStream(M);
      M.Position := 0;
      Result := InsertData(InsertPos, M, c_PascalString, itmHnd);
      DisposeObject(M);

      t.FDBStorePos := Result;
      t.dbEng := Self;
      t.CreateTime := itmHnd.CreateTime;
      t.ModificationTime := itmHnd.ModificationTime;

      ProcessNewInstanceCache(t.FDBStorePos, t, t.MemoryUsed);
    end
  else
    begin
      t := TDBEnginePascalString.Create;
      t.Buff := Buff;

      M := TMemoryStream64.Create;
      t.SaveToStream(M);
      M.Position := 0;
      Result := InsertData(InsertPos, M, c_PascalString, itmHnd);
      DisposeObject(M);

      DisposeObject(t);
    end;
end;

function TDBStore.AddData(Buff: TDBEnginePascalString): Int64;
var
  M: TMemoryStream64;
begin
  M := TMemoryStream64.Create;
  Buff.SaveToStream(M);
  M.Position := 0;
  Result := AddData(M, c_PascalString);
  DisposeObject(M);
end;

function TDBStore.AddData(Buff: TPascalString): Int64;
begin
  Result := AddString(Buff);
end;

function TDBStore.AddString(Buff: TPascalString): Int64;
var
  t: TDBEnginePascalString;
  M: TMemoryStream64;
  itmHnd: TItemHandle;
begin
  if AllowedCache then
    begin
      MH_ZDB.BeginMemoryHook;
      t := TDBEnginePascalString.Create;
      t.Buff := Buff;
      t.MemoryUsed := MH_ZDB.GetHookMemorySize;
      MH_ZDB.EndMemoryHook;

      M := TMemoryStream64.Create;
      t.SaveToStream(M);
      M.Position := 0;
      Result := AddData(M, c_PascalString, itmHnd);
      DisposeObject(M);

      t.FDBStorePos := Result;
      t.dbEng := Self;
      t.CreateTime := itmHnd.CreateTime;
      t.ModificationTime := itmHnd.ModificationTime;

      ProcessNewInstanceCache(t.FDBStorePos, t, t.MemoryUsed);
    end
  else
    begin
      t := TDBEnginePascalString.Create;
      t.Buff := Buff;

      M := TMemoryStream64.Create;
      t.SaveToStream(M);
      M.Position := 0;
      Result := AddData(M, c_PascalString, itmHnd);
      DisposeObject(M);

      DisposeObject(t);
    end;
end;

function TDBStore.GetPascalString(const StorePos: Int64): TDBEnginePascalString;
var
  lastAcc: TCoreClassObject;
  M: TDBCacheStream64;
begin
  lastAcc := FCache[StorePos];
  if lastAcc is TDBEnginePascalString then
    begin
      Result := lastAcc as TDBEnginePascalString;
      Exit;
    end;

  if lastAcc <> nil then
      FCache.Delete(StorePos);

  Result := nil;

  M := GetCacheStream(StorePos, c_PascalString);
  if M <> nil then
    begin
      M.Position := 0;

      MH_ZDB.BeginMemoryHook;
      try
        if AllowedCache then
            Result := TDBEnginePascalString.Create
        else
            Result := FResultPascalString;

        try
            Result.LoadFromStream(M);
        except
        end;
        Result.FDBStorePos := StorePos;
        Result.dbEng := Self;
        Result.CreateTime := M.CreateTime;
        Result.ModificationTime := M.ModificationTime;
        Result.MemoryUsed := MH_ZDB.GetHookMemorySize;
        if AllowedCache then
            ProcessNewInstanceCache(StorePos, Result, Result.MemoryUsed);
      finally
          MH_ZDB.EndMemoryHook;
      end;
    end;
end;

function TDBStore.GetPascalString(var qState: TQueryState): TDBEnginePascalString;
begin
  Result := GetPascalString(qState.StorePos);
end;

function TDBStore.GetString(const StorePos: Int64): TPascalString;
var
  t: TDBEnginePascalString;
begin
  t := GetPascalString(StorePos);
  if t <> nil then
      Result := t.Buff
  else
      Result := '';
end;

function TDBStore.GetString(var qState: TQueryState): TPascalString;
begin
  Result := GetString(qState.StorePos);
end;

procedure TDBStore.SetString(const StorePos: Int64; const Value: TPascalString);
var
  t: TDBEnginePascalString;
begin
  t := GetPascalString(StorePos);
  if t <> nil then
    begin
      t.Buff := Value;
      t.Save;
    end;
end;

function TDBStore.BuildPascalString(const StorePos: Int64): TDBEnginePascalString;
var
  M: TDBCacheStream64;
begin
  Result := nil;
  M := GetCacheStream(StorePos, c_PascalString);
  if M <> nil then
    begin
      M.Position := 0;
      MH_ZDB.BeginMemoryHook;
      try
        Result := TDBEnginePascalString.Create;
        try
            Result.LoadFromStream(M);
        except
        end;
        Result.FDBStorePos := StorePos;
        Result.dbEng := Self;
        Result.CreateTime := M.CreateTime;
        Result.ModificationTime := M.ModificationTime;
        Result.MemoryUsed := MH_ZDB.GetHookMemorySize;
      finally
          MH_ZDB.EndMemoryHook;
      end;
    end;
end;

function TDBStore.BuildPascalString(var qState: TQueryState): TDBEnginePascalString;
begin
  Result := BuildPascalString(qState.StorePos);
end;

initialization

DefaultCacheAnnealingTime := 15.0;
DefaultMinimizeCacheOfFileSize := 16 * 1024 * 1024; // 16M

{$IFDEF CPU64}
DefaultCacheBufferLength := 10000 * 40;
DefaultIndexCacheBufferLength := 10000 * 40;
DefaultMinimizeInstanceCacheSize := 512 * 1024 * 1024; // 512M
DefaultMaximumInstanceCacheSize := 1024 * 1024 * 1024; // 1GB
DefaultMinimizeStreamCacheSize := 96 * 1024 * 1024;    // 96M
DefaultMaximumStreamCacheSize := 128 * 1024 * 1024;    // 128M
{$ELSE}
DefaultCacheBufferLength := 10000 * 10;
DefaultIndexCacheBufferLength := 10000 * 10;
DefaultMinimizeInstanceCacheSize := 32 * 1024 * 1024; // 32M
DefaultMaximumInstanceCacheSize := 128 * 1024 * 1024; // 128M
DefaultMinimizeStreamCacheSize := 24 * 1024 * 1024;   // 24M
DefaultMaximumStreamCacheSize := 32 * 1024 * 1024;    // 32M
{$ENDIF}

finalization

end.
