{ ****************************************************************************** }
{ * ZDBEngine, createby qq600585                                               * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
(* https://github.com/PassByYou888/ZServer4D *)
{ ****************************************************************************** }
(*
  update history
  2017-12-6 added cache system
  2017-12-8 optimization cache system
*)

unit ZDBEngine;

{$I zDefine.inc}

interface

uses Classes, SysUtils,
  ListEngine, PascalStrings, UnicodeMixedLib,
  TextDataEngine,

  {$IFNDEF FPC}
  JsonDataObjects,
  {$ENDIF}
  CoreClasses, MemoryStream64, ObjectData, ObjectDataManager,
  DataFrameEngine, ItemStream, DoStatusIO, CoreCipher;

var
  DefaultCacheAnnealingTime     : Double  = 15.0;
  DefaultCacheBufferLength      : Integer = 10000;
  DefaultMinimizeCache          : Integer = 10000;
  DefaultMaximumCache           : Integer = 10000 * 500;
  DefaultIndexCacheSize         : Integer = 10000 * 10;
  DefaultMinimizeCacheOfFileSize: Int64   = 128 * 1024 * 1024;

type
  TDBStoreBase = class;

  // Base Data Struct
  TDBEngineDF = class(TDataFrameEngine)
  protected
    DBStorePos                : Int64;
    DBEng                     : TDBStoreBase;
    CreateTime, LastModifyTime: TDateTime;
  public
    constructor Create;
    procedure Save;
  end;

  // Base Data Struct
  TDBEngineVL = class(THashVariantList)
  protected
    DBStorePos                : Int64;
    DBEng                     : TDBStoreBase;
    CreateTime, LastModifyTime: TDateTime;
  public
    constructor Create;
    procedure Save;
  end;

  // Base Data Struct
  TDBEngineTE = class(TSectionTextData)
  protected
    DBStorePos                : Int64;
    DBEng                     : TDBStoreBase;
    CreateTime, LastModifyTime: TDateTime;
  public
    constructor Create;
    procedure Save;
  end;

  {$IFNDEF FPC}

  // Base Data Struct
  TDBEngineJson = class(TJsonObject)
  protected
    DBStorePos                : Int64;
    DBEng                     : TDBStoreBase;
    CreateTime, LastModifyTime: TDateTime;
  public
    constructor Create;
    procedure Save;
  end;
  {$ENDIF}

  // Base Data Struct
  TDBEnginePascalString = class(TCoreClassObject)
  protected
    DBStorePos                : Int64;
    DBEng                     : TDBStoreBase;
    CreateTime, LastModifyTime: TDateTime;
  public
    buff: TPascalString;

    constructor Create;

    procedure Clear;

    procedure Save;

    procedure LoadFromStream(Stream: TCoreClassStream);
    procedure SaveToStream(Stream: TCoreClassStream);
    class procedure LoadPascalStringFromStream(p: PPascalString; Stream: TCoreClassStream);
    class procedure SavePascalStringToStream(p: PPascalString; Stream: TCoreClassStream);
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
    procedure Add(value: TDBEngineDF); overload;
    procedure Delete(index: Integer);

    procedure LoadFromStoreEngine(DBEng: TDBStoreBase);
    procedure ExportToStoreEngine(DBEng: TDBStoreBase);
  end;

  // Base DataBase Struct
  TDBListVL = class(TCoreClassObject)
  protected
    FHashListBuff: TCoreClassListForObj;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    function Count: Integer;
    function GetItems(const index: Integer): TDBEngineVL;
    property Items[const index: Integer]: TDBEngineVL read GetItems; default;
    function Add: TDBEngineVL; overload;
    procedure Add(value: TDBEngineVL); overload;

    procedure ImportTextStream(Stream: TCoreClassStream);
    procedure ExportTextStream(Stream: TCoreClassStream);

    procedure LoadFromStoreEngine(DBEng: TDBStoreBase);
    procedure ExportToStoreEngine(DBEng: TDBStoreBase);
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
    procedure Add(value: TDBEngineTE); overload;

    procedure LoadFromStoreEngine(DBEng: TDBStoreBase);
    procedure ExportToStoreEngine(DBEng: TDBStoreBase);
  end;

  {$IFNDEF FPC}

  // Base DataBase Struct
  TDBListJson = class(TCoreClassObject)
  protected
    FHashListBuff: TCoreClassListForObj;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    function Count: Integer;
    function GetItems(const index: Integer): TDBEngineJson;
    property Items[const index: Integer]: TDBEngineJson read GetItems; default;
    function Add: TDBEngineJson; overload;
    procedure Add(value: TDBEngineJson); overload;

    procedure LoadFromStoreEngine(DBEng: TDBStoreBase);
    procedure ExportToStoreEngine(DBEng: TDBStoreBase);
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
    procedure Add(value: TDBEnginePascalString); overload;

    procedure LoadFromStoreEngine(DBEng: TDBStoreBase);
    procedure ExportToStoreEngine(DBEng: TDBStoreBase);
  end;

  PQueryState = ^TQueryState;

  TQueryState = record
    DBEng: TDBStoreBase;
    StorePos: Int64;
    QueryHnd: PHeader;
    TaskTag: SystemString;
    DeltaTime, NewTime: TTimeTickValue;
    Aborted: Boolean;

    function ID: Cardinal; inline;
    function IsDF: Boolean; inline;
    function IsVL: Boolean; inline;
    function IsTE: Boolean; inline;
    function IsJson: Boolean; inline;
    function IsString: Boolean; inline;
    function IsOther: Boolean; inline;
  end;

  TQueryCall       = procedure(var qState: TQueryState);
  TQueryMethod     = procedure(var qState: TQueryState) of object;
  TQueryDoneCall   = procedure();
  TQueryDoneMethod = procedure() of object;

  {$IFNDEF FPC}
  TQueryProc     = reference to procedure(var qState: TQueryState);
  TQueryDoneProc = reference to procedure();
  {$ENDIF}

  TQueryTask = class(TCoreClassObject)
  protected
    FDBEng   : TDBStoreBase;
    FInited  : Boolean;
    FReverse : Boolean;
    FItmSrHnd: THeader;
    FState   : TQueryState;

    FTriggerTime: TTimeTickValue;
    FTaskTag    : SystemString;

    FLastTime: TTimeTickValue;

    FStoped, FPaused: Boolean;

    {$IFDEF FPC}
    FOnQueryCall      : TQueryCall;
    FOnQueryMethod    : TQueryMethod;
    FOnQueryDoneCall  : TQueryDoneCall;
    FOnQueryDoneMethod: TQueryDoneMethod;
    {$ELSE}
    FOnQueryCall      : TQueryCall;
    FOnQueryMethod    : TQueryMethod;
    FOnQueryProc      : TQueryProc;
    FOnQueryDoneCall  : TQueryDoneCall;
    FOnQueryDoneMethod: TQueryDoneMethod;
    FOnQueryDoneProc  : TQueryDoneProc;
    {$ENDIF}
    procedure DoTriggerQuery;
    procedure DoQueryDone;
  public
    constructor Create;

    procedure Stop;
    procedure Pause;
    procedure Play;

    function ProcessQuery: Boolean;
    property Paused: Boolean read FPaused;
    function ConsumTime: Double; inline;
  end;

  TQueryThread = class(TCoreClassThread)
  public
    StoreEngine   : TDBStoreBase;
    Paused        : Boolean;
    PausedIdleTime: Double;
    procedure SyncQuery;
    procedure SyncCheckCache;
    procedure SyncUpdateCacheState;
    procedure Execute; override;
    destructor Destroy; override;
  end;

  IDBStoreBaseNotify = interface
    procedure DoInsertData(Sender: TDBStoreBase; InsertPos: Int64; buff: TCoreClassStream; ID: Cardinal; CompletePos: Int64);
    procedure DoAddData(Sender: TDBStoreBase; buff: TCoreClassStream; ID: Cardinal; CompletePos: Int64);
    procedure DoModifyData(Sender: TDBStoreBase; const StorePos: Int64; buff: TCoreClassStream);
    procedure DoDeleteData(Sender: TDBStoreBase; const StorePos: Int64);
  end;

  PIndexCacheHeader    = PHeader;
  PIndexCacheItemBlock = PItemBlock;

  PIndexCacheItem = ^TIndexCacheItem;

  TIndexCacheItem = record
    Description: umlString;
    ExtID: Byte;
    FirstBlockPOS: Int64;
    LastBlockPOS: Int64;
    Size: Int64;
    BlockCount: Int64;
    CurrentBlockSeekPOS: Int64;
    CurrentFileSeekPOS: Int64;
    DataWrited: Boolean;
    Return: Integer;
    procedure Write(var wVal: TItem); inline;
    procedure Read(var rVal: TItem); inline;
  end;

  PIndexCacheField = ^TIndexCacheField;

  TIndexCacheField = record
    UpLevelFieldPOS: Int64;
    Description: umlString;
    HeaderCount: Int64;
    FirstHeaderPOS: Int64;
    LastHeaderPOS: Int64;
    Return: Integer;
    procedure Write(var wVal: TField); inline;
    procedure Read(var rVal: TField); inline;
  end;

  // store engine
  TCacheStyle = (csAutomation, csDisabled, csEnabled);

  TDBStoreBase = class(TCoreClassObject)
  protected
    FDBEngine                                             : TObjectDataManager;
    FStoreFieldPos                                        : Int64;
    FCount                                                : Int64;
    FQueryQueue                                           : TCoreClassListForObj;
    FQueryThread                                          : TQueryThread;
    FQueryThreadTerminate                                 : Boolean;
    FQueryThreadLastActivtedTime                          : TDateTime;
    FNotifyIntf                                           : IDBStoreBaseNotify;
    FCache                                                : TInt64HashObjectList;
    FCacheStyle                                           : TCacheStyle;
    FCacheAnnealingTime                                   : Double;
    FMaximumCache                                         : Integer;
    FMinimizeCache                                        : Integer;
    FMinimizeCacheOfFileSize                              : Int64;
    FCacheAnnealingState                                  : SystemString;
    FHeaderCache, FItemBlockCache, FItemCache, FFieldCache: TInt64HashPointerList;
    FResultDF                                             : TDBEngineDF;
    FResultVL                                             : TDBEngineVL;
    FResultTE                                             : TDBEngineTE;
    {$IFNDEF FPC}
    FResultJson: TDBEngineJson;
    {$ENDIF}
    FResultPascalString: TDBEnginePascalString;

    procedure ReadHeaderInfo;

    procedure ThreadFreeEvent(Sender: TObject);
    procedure DoCreateInit; virtual;

    procedure HeaderCache_DataFreeProc(p: Pointer);
    procedure ItemBlockCache_DataFreeProc(p: Pointer);
    procedure ItemCache_DataFreeProc(p: Pointer);
    procedure FieldCache_DataFreeProc(p: Pointer);

    procedure HeaderWriteProc(fPos: Int64; var wVal: THeader);
    procedure HeaderReadProc(fPos: Int64; var rVal: THeader; var Done: Boolean);
    procedure ItemBlockWriteProc(fPos: Int64; var wVal: TItemBlock);
    procedure ItemBlockReadProc(fPos: Int64; var rVal: TItemBlock; var Done: Boolean);
    procedure ItemWriteProc(fPos: Int64; var wVal: TItem);
    procedure ItemReadProc(fPos: Int64; var rVal: TItem; var Done: Boolean);
    procedure OnlyItemRecWriteProc(fPos: Int64; var wVal: TItem);
    procedure OnlyItemRecReadProc(fPos: Int64; var rVal: TItem; var Done: Boolean);
    procedure FieldWriteProc(fPos: Int64; var wVal: TField);
    procedure FieldReadProc(fPos: Int64; var rVal: TField; var Done: Boolean);
    procedure OnlyFieldRecWriteProc(fPos: Int64; var wVal: TField);
    procedure OnlyFieldRecReadProc(fPos: Int64; var rVal: TField; var Done: Boolean);
  public
    constructor Create(DBFile: SystemString; OnlyRead: Boolean);
    constructor CreateMemory(DBMemory: TCoreClassStream; OnlyRead: Boolean);
    constructor CreateNew(DBFile: SystemString);
    constructor CreateNewMemory;
    destructor Destroy; override;

    procedure CompressTo(DestDB: TObjectDataManager);
    procedure Compress;

    procedure Update;

    procedure SaveToStream(Stream: TCoreClassStream);
    procedure SaveToFile(fn: SystemString);

    procedure LoadFromStream(Stream: TCoreClassStream);
    procedure LoadFromFile(fn: SystemString);

    function IsMemoryMode: Boolean;
    function IsReadOnly: Boolean;
    procedure ResetDB;
    function RenameDB(newName: SystemString): Boolean;

    property DBEngine: TObjectDataManager read FDBEngine;
    property Count: Int64 read FCount;

    // cache states
    property Cache: TInt64HashObjectList read FCache;
    procedure Recache;
    function AllowedCache: Boolean; inline;

    property CacheStyle: TCacheStyle read FCacheStyle write FCacheStyle;
    property CacheAnnealingTime: Double read FCacheAnnealingTime write FCacheAnnealingTime;
    property MaximumCache: Integer read FMaximumCache write FMaximumCache;
    property MinimizeCache: Integer read FMinimizeCache write FMinimizeCache;
    property MinimizeCacheOfFileSize: Int64 read FMinimizeCacheOfFileSize write FMinimizeCacheOfFileSize;
    property CacheAnnealingState: SystemString read FCacheAnnealingState;

    // lowlevel data
    function InsertData(const InsertPos: Int64; buff: TCoreClassStream; ID: Cardinal; var itmHnd: TItemHandle): Int64; overload; inline;
    function InsertData(const InsertPos: Int64; buff: TCoreClassStream; ID: Cardinal): Int64; overload; inline;
    function AddData(buff: TCoreClassStream; ID: Cardinal; var itmHnd: TItemHandle): Int64; overload; inline;
    function AddData(buff: TCoreClassStream; ID: Cardinal): Int64; overload; inline;
    function SetData(const StorePos: Int64; buff: TCoreClassStream): Boolean; inline;
    function GetData(const StorePos: Int64; ID: Cardinal): TItemStream; overload; inline;
    function GetData(const StorePos: Int64): TItemStream; overload; inline;
    function DeleteData(const StorePos: Int64): Boolean; inline;
    function GetSize(const StorePos: Int64): Int64; inline;

    // backcall
    property NotifyIntf: IDBStoreBaseNotify read FNotifyIntf write FNotifyIntf;

    // lowlevel
    function QueryFirst(var qState: TQueryState): Boolean;
    function QueryNext(var qState: TQueryState): Boolean;
    function QueryLast(var qState: TQueryState): Boolean;
    function QueryPrev(var qState: TQueryState): Boolean;

    // wait query
    {$IFDEF FPC}
    procedure WaitQuery(ReverseQuery: Boolean;
      const OnQueryCall: TQueryCall;
      const OnQueryMethod: TQueryMethod); overload;

    procedure WaitQuery(ReverseQuery: Boolean; const OnQueryCall: TQueryCall); overload;
    procedure WaitQuery(ReverseQuery: Boolean; const OnQueryMethod: TQueryMethod); overload;
    procedure WaitQuery(const OnQueryCall: TQueryCall); overload;
    procedure WaitQuery(const OnQueryMethod: TQueryMethod); overload;
    {$ELSE}
    procedure WaitQuery(ReverseQuery: Boolean;
      const OnQueryCall: TQueryCall;
      const OnQueryProc: TQueryProc;
      const OnQueryMethod: TQueryMethod); overload;

    procedure WaitQuery(ReverseQuery: Boolean; const OnQueryCall: TQueryCall); overload;
    procedure WaitQuery(ReverseQuery: Boolean; const OnQueryMethod: TQueryMethod); overload;
    procedure WaitQuery(ReverseQuery: Boolean; const OnQueryProc: TQueryProc); overload;
    procedure WaitQuery(const OnQueryCall: TQueryCall); overload;
    procedure WaitQuery(const OnQueryProc: TQueryProc); overload;
    procedure WaitQuery(const OnQueryMethod: TQueryMethod); overload;
    {$ENDIF}
    //
    // background query
    {$IFDEF FPC}
    function Query(const TaskTag: SystemString; const ReverseQuery: Boolean;
      const OnQueryCall: TQueryCall; const OnQueryDoneCall: TQueryDoneCall;
      const OnQueryMethod: TQueryMethod; const OnQueryDoneMethod: TQueryDoneMethod): TQueryTask; overload;

    function Query(const TaskTag: SystemString; const ReverseQuery: Boolean; const OnQueryMethod: TQueryMethod; const OnQueryDoneMethod: TQueryDoneMethod): TQueryTask; overload;
    function Query(const TaskTag: SystemString; const ReverseQuery: Boolean; const OnQueryCall: TQueryCall; const OnQueryDoneCall: TQueryDoneCall): TQueryTask; overload;
    function Query(const TaskTag: SystemString; const OnQueryCall: TQueryCall; const OnQueryDoneCall: TQueryDoneCall): TQueryTask; overload;
    function Query(const TaskTag: SystemString; const OnQueryMethod: TQueryMethod; const OnQueryDoneMethod: TQueryDoneMethod): TQueryTask; overload;
    function Query(const OnQueryCall: TQueryCall; const OnQueryDoneCall: TQueryDoneCall): TQueryTask; overload;
    function Query(const OnQueryMethod: TQueryMethod; const OnQueryDoneMethod: TQueryDoneMethod): TQueryTask; overload;
    function Query(const ReverseQuery: Boolean; const OnQueryCall: TQueryCall; const OnQueryDoneCall: TQueryDoneCall): TQueryTask; overload;
    function Query(const ReverseQuery: Boolean; const OnQueryMethod: TQueryMethod; const OnQueryDoneMethod: TQueryDoneMethod): TQueryTask; overload;
    {$ELSE}
    function Query(const TaskTag: SystemString; const ReverseQuery: Boolean;
      const OnQueryCall: TQueryCall; const OnQueryDoneCall: TQueryDoneCall;
      const OnQueryProc: TQueryProc; const OnQueryDoneProc: TQueryDoneProc;
      const OnQueryMethod: TQueryMethod; const OnQueryDoneMethod: TQueryDoneMethod): TQueryTask; overload;

    function Query(const TaskTag: SystemString; const ReverseQuery: Boolean; const OnQueryMethod: TQueryMethod; const OnQueryDoneMethod: TQueryDoneMethod): TQueryTask; overload;
    function Query(const TaskTag: SystemString; const ReverseQuery: Boolean; const OnQueryCall: TQueryCall; const OnQueryDoneCall: TQueryDoneCall): TQueryTask; overload;
    function Query(const TaskTag: SystemString; const ReverseQuery: Boolean; const OnQueryProc: TQueryProc; const OnQueryDoneProc: TQueryDoneProc): TQueryTask; overload;
    function Query(const TaskTag: SystemString; const OnQueryCall: TQueryCall; const OnQueryDoneCall: TQueryDoneCall): TQueryTask; overload;
    function Query(const TaskTag: SystemString; const OnQueryMethod: TQueryMethod; const OnQueryDoneMethod: TQueryDoneMethod): TQueryTask; overload;
    function Query(const TaskTag: SystemString; const OnQueryProc: TQueryProc; const OnQueryDoneProc: TQueryDoneProc): TQueryTask; overload;
    function Query(const OnQueryCall: TQueryCall; const OnQueryDoneCall: TQueryDoneCall): TQueryTask; overload;
    function Query(const OnQueryMethod: TQueryMethod; const OnQueryDoneMethod: TQueryDoneMethod): TQueryTask; overload;
    function Query(const OnQueryProc: TQueryProc; const OnQueryDoneProc: TQueryDoneProc): TQueryTask; overload;
    function Query(const ReverseQuery: Boolean; const OnQueryCall: TQueryCall; const OnQueryDoneCall: TQueryDoneCall): TQueryTask; overload;
    function Query(const ReverseQuery: Boolean; const OnQueryMethod: TQueryMethod; const OnQueryDoneMethod: TQueryDoneMethod): TQueryTask; overload;
    function Query(const ReverseQuery: Boolean; const OnQueryProc: TQueryProc; const OnQueryDoneProc: TQueryDoneProc): TQueryTask; overload;
    {$ENDIF}
    procedure WaitQueryThread; overload;
    procedure WaitQueryThread(waitTime: TTimeTickValue); overload;

    // query state
    function QueryProcessing: Boolean; inline;
    property QueryThreadLastActivtedTime: TDateTime read FQueryThreadLastActivtedTime;

    // query task operation
    procedure StopQuery(const TaskTag: SystemString);
    procedure StopAllQuery;

    // data operation
    function InsertData(const InsertPos: Int64; buff: TDataFrameEngine): Int64; overload;
    function AddData(buff: TDataFrameEngine): Int64; overload;
    function GetDF(const StorePos: Int64): TDBEngineDF; overload;
    function GetDF(var qState: TQueryState): TDBEngineDF; overload;
    function BuildDF(const StorePos: Int64): TDBEngineDF; overload;
    function BuildDF(var qState: TQueryState): TDBEngineDF; overload;
    property DF[const StorePos: Int64]: TDBEngineDF read GetDF;

    // data operation
    function InsertData(const InsertPos: Int64; buff: THashVariantList): Int64; overload;
    function AddData(buff: THashVariantList): Int64; overload;
    function GetVL(const StorePos: Int64): TDBEngineVL; overload;
    function GetVL(var qState: TQueryState): TDBEngineVL; overload;
    function BuildVL(const StorePos: Int64): TDBEngineVL; overload;
    function BuildVL(var qState: TQueryState): TDBEngineVL; overload;
    property VL[const StorePos: Int64]: TDBEngineVL read GetVL;

    // data operation
    function InsertData(const InsertPos: Int64; buff: TSectionTextData): Int64; overload;
    function AddData(buff: TSectionTextData): Int64; overload;
    function GetTE(const StorePos: Int64): TDBEngineTE; overload;
    function GetTE(var qState: TQueryState): TDBEngineTE; overload;
    function BuildTE(const StorePos: Int64): TDBEngineTE; overload;
    function BuildTE(var qState: TQueryState): TDBEngineTE; overload;
    property TE[const StorePos: Int64]: TDBEngineTE read GetTE;

    // data operation
    {$IFNDEF FPC}
    function InsertData(const InsertPos: Int64; buff: TJsonObject): Int64; overload;
    function AddData(buff: TJsonObject): Int64; overload;
    function GetJson(const StorePos: Int64): TDBEngineJson; overload;
    function GetJson(var qState: TQueryState): TDBEngineJson; overload;
    function BuildJson(const StorePos: Int64): TDBEngineJson; overload;
    function BuildJson(var qState: TQueryState): TDBEngineJson; overload;
    property Json[const StorePos: Int64]: TDBEngineJson read GetJson;
    {$ENDIF}
    //
    // data operation
    function InsertData(const InsertPos: Int64; buff: TDBEnginePascalString): Int64; overload;
    function InsertData(const InsertPos: Int64; buff: TPascalString): Int64; overload;
    function InsertString(const InsertPos: Int64; buff: TPascalString): Int64; overload;
    function AddData(buff: TDBEnginePascalString): Int64; overload;
    function AddData(buff: TPascalString): Int64; overload;
    function AddString(buff: TPascalString): Int64; overload;
    function GetPascalString(const StorePos: Int64): TDBEnginePascalString; overload;
    function GetPascalString(var qState: TQueryState): TDBEnginePascalString; overload;
    function GetString(const StorePos: Int64): TPascalString; overload;
    function GetString(var qState: TQueryState): TPascalString; overload;
    procedure SetString(const StorePos: Int64; const value: TPascalString); overload;
    function BuildPascalString(const StorePos: Int64): TDBEnginePascalString; overload;
    function BuildPascalString(var qState: TQueryState): TDBEnginePascalString; overload;
    property PascalString[const StorePos: Int64]: TPascalString read GetString write SetString;
  end;

var
  c_DF          : Cardinal = $FFFFFFF0;
  c_VL          : Cardinal = $FFFFFFF1;
  c_TE          : Cardinal = $FFFFFFF2;
  c_Json        : Cardinal = $FFFFFFF3;
  c_PascalString: Cardinal = $FFFFFFF4;

implementation

constructor TDBEngineDF.Create;
begin
  inherited Create;
  DBStorePos := -1;
  DBEng := nil;
  CreateTime := umlDefaultTime;
  LastModifyTime := CreateTime;
end;

procedure TDBEngineDF.Save;
var
  m: TMemoryStream64;
begin
  if (DBStorePos < 0) or (DBEng = nil) then
      exit;

  m := TMemoryStream64.Create;
  EncodeTo(m, True);
  DBEng.SetData(DBStorePos, m);
  DisposeObject(m);
end;

constructor TDBEngineVL.Create;
begin
  inherited Create;
  DBStorePos := -1;
  DBEng := nil;
  CreateTime := umlDefaultTime;
  LastModifyTime := CreateTime;
end;

procedure TDBEngineVL.Save;
var
  m: TMemoryStream64;
begin
  if (DBStorePos < 0) or (DBEng = nil) then
      exit;

  m := TMemoryStream64.Create;
  SaveToStream(m);
  DBEng.SetData(DBStorePos, m);
  DisposeObject(m);
end;

constructor TDBEngineTE.Create;
begin
  inherited Create;
  DBStorePos := -1;
  DBEng := nil;
  CreateTime := umlDefaultTime;
  LastModifyTime := CreateTime;
end;

procedure TDBEngineTE.Save;
var
  m: TMemoryStream64;
begin
  if (DBStorePos < 0) or (DBEng = nil) then
      exit;

  m := TMemoryStream64.Create;
  SaveToStream(m);
  DBEng.SetData(DBStorePos, m);
  DisposeObject(m);
end;

{$IFNDEF FPC}


constructor TDBEngineJson.Create;
begin
  inherited Create;
  DBStorePos := -1;
  DBEng := nil;
  CreateTime := umlDefaultTime;
  LastModifyTime := CreateTime;
end;

procedure TDBEngineJson.Save;
var
  m: TMemoryStream64;
begin
  if (DBStorePos < 0) or (DBEng = nil) then
      exit;

  m := TMemoryStream64.Create;
  SaveToStream(m, True, TEncoding.UTF8, True);
  DBEng.SetData(DBStorePos, m);
  DisposeObject(m);
end;
{$ENDIF}


constructor TDBEnginePascalString.Create;
begin
  inherited Create;
  DBStorePos := -1;
  DBEng := nil;
  CreateTime := umlDefaultTime;
  LastModifyTime := CreateTime;
  buff.len := 0;
end;

procedure TDBEnginePascalString.Clear;
begin
  buff.len := 0;
end;

procedure TDBEnginePascalString.Save;
var
  m: TMemoryStream64;
begin
  if (DBStorePos < 0) or (DBEng = nil) then
      exit;

  m := TMemoryStream64.Create;
  SaveToStream(m);
  DBEng.SetData(DBStorePos, m);
  DisposeObject(m);
end;

procedure TDBEnginePascalString.LoadFromStream(Stream: TCoreClassStream);
begin
  LoadPascalStringFromStream(@buff, Stream);
end;

procedure TDBEnginePascalString.SaveToStream(Stream: TCoreClassStream);
begin
  SavePascalStringToStream(@buff, Stream);
end;

class procedure TDBEnginePascalString.LoadPascalStringFromStream(p: PPascalString; Stream: TCoreClassStream);
var
  l: Integer;
  b: TBytes;
begin
  Stream.Read(l, umlIntegerLength);
  SetLength(b, l);
  Stream.Read(b[0], l);
  p^.Bytes := b;
  SetLength(b, 0);
end;

class procedure TDBEnginePascalString.SavePascalStringToStream(p: PPascalString; Stream: TCoreClassStream);
var
  l: Integer;
  b: TBytes;
begin
  p^.FastGetBytes(b);
  l := length(b);
  Stream.Write(l, umlIntegerLength);
  Stream.Write(b[0], l);
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
  Result.DBStorePos := -1;
  Result.DBEng := nil;
  FHashListBuff.Add(Result);
end;

procedure TDBListDF.Add(value: TDBEngineDF);
begin
  FHashListBuff.Add(value);
end;

procedure TDBListDF.Delete(index: Integer);
begin
  DisposeObject(FHashListBuff[index]);
  FHashListBuff.Delete(index);
end;

procedure TDBListDF.LoadFromStoreEngine(DBEng: TDBStoreBase);
var
  itmSearHnd: THeader;
  qState    : TQueryState;
begin
  Clear;
  qState.QueryHnd := @itmSearHnd;
  if DBEng.QueryFirst(qState) then
    begin
      repeat
        if qState.ID = c_DF then
            FHashListBuff.Add(DBEng.BuildDF(qState.StorePos));
      until not DBEng.QueryNext(qState);
    end;
end;

procedure TDBListDF.ExportToStoreEngine(DBEng: TDBStoreBase);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      DBEng.AddData(GetItems(i));
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
  Result.DBStorePos := -1;
  Result.DBEng := nil;
  FHashListBuff.Add(Result);
end;

procedure TDBListVL.Add(value: TDBEngineVL);
begin
  FHashListBuff.Add(value);
end;

procedure TDBListVL.ImportTextStream(Stream: TCoreClassStream);
var
  sour               : TCoreClassStringList;
  i                  : Integer;
  n                  : TPascalString;
  VL                 : THashVariantList;
  TextName, TextValue: TPascalString;
begin
  sour := TCoreClassStringList.Create;
  try
    {$IFDEF FPC}
    sour.LoadFromStream(Stream);
    {$ELSE}
    sour.LoadFromStream(Stream, TEncoding.UTF8);
    {$ENDIF}
  except
    DisposeObject(sour);
    exit;
  end;

  VL := THashVariantList.Create;

  i := 0;
  while i < sour.Count do
    begin
      n := umlTrimSpace(sour[i]);
      inc(i);
      if n.len = 0 then
        begin
          if VL.Count > 0 then
            begin
              FHashListBuff.Add(VL);
              VL := THashVariantList.Create;
            end;
        end
      else if ((n.Exists(':')) or (n.Exists('='))) and (not CharIn(n.First, [':', '='])) then
        begin
          TextName := umlGetFirstStr_M(n, ':=');
          if TextName.len > 0 then
            begin
              TextValue := umlDeleteFirstStr_M(n, ':=');
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

procedure TDBListVL.ExportTextStream(Stream: TCoreClassStream);
const
  lineBreak = #13#10;
var
  i, j: Integer;
  ls  : TCoreClassList;
  s, n: TPascalString;
  b   : TPascalString;
  buff: TBytes;
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

              if s.len > 0 then
                  n.Text := PHashListData(ls[j])^.OriginName + '=' + s.Text
              else
                  n.Text := PHashListData(ls[j])^.OriginName;

              b := b + n + lineBreak;
            end;

          b := b + lineBreak;
          buff := b.Bytes;
          Stream.Write(buff, length(buff));
          b := '';
        end;
    end;

  DisposeObject([ls]);
end;

procedure TDBListVL.LoadFromStoreEngine(DBEng: TDBStoreBase);
var
  itmSearHnd: THeader;
  qState    : TQueryState;
begin
  Clear;
  qState.QueryHnd := @itmSearHnd;
  if DBEng.QueryFirst(qState) then
    begin
      repeat
        if qState.ID = c_VL then
            FHashListBuff.Add(DBEng.BuildVL(qState.StorePos));
      until not DBEng.QueryNext(qState);
    end;
end;

procedure TDBListVL.ExportToStoreEngine(DBEng: TDBStoreBase);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      DBEng.AddData(GetItems(i));
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
  Result.DBStorePos := -1;
  Result.DBEng := nil;
  FHashListBuff.Add(Result);
end;

procedure TDBListTE.Add(value: TDBEngineTE);
begin
  FHashListBuff.Add(value);
end;

procedure TDBListTE.LoadFromStoreEngine(DBEng: TDBStoreBase);
var
  itmSearHnd: THeader;
  qState    : TQueryState;
begin
  Clear;
  qState.QueryHnd := @itmSearHnd;
  if DBEng.QueryFirst(qState) then
    begin
      repeat
        if qState.ID = c_TE then
            FHashListBuff.Add(DBEng.BuildTE(qState.StorePos));
      until not DBEng.QueryNext(qState);
    end;
end;

procedure TDBListTE.ExportToStoreEngine(DBEng: TDBStoreBase);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      DBEng.AddData(GetItems(i));
end;

{$IFNDEF FPC}


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
  Result.DBStorePos := -1;
  Result.DBEng := nil;
  FHashListBuff.Add(Result);
end;

procedure TDBListJson.Add(value: TDBEngineJson);
begin
  FHashListBuff.Add(value);
end;

procedure TDBListJson.LoadFromStoreEngine(DBEng: TDBStoreBase);
var
  itmSearHnd: THeader;
  qState    : TQueryState;
begin
  Clear;
  qState.QueryHnd := @itmSearHnd;
  if DBEng.QueryFirst(qState) then
    begin
      repeat
        if qState.ID = c_Json then
            FHashListBuff.Add(DBEng.BuildJson(qState.StorePos));
      until not DBEng.QueryNext(qState);
    end;
end;

procedure TDBListJson.ExportToStoreEngine(DBEng: TDBStoreBase);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      DBEng.AddData(GetItems(i));
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
  Result.DBStorePos := -1;
  Result.DBEng := nil;
  FHashListBuff.Add(Result);
end;

procedure TDBListPascalString.Add(value: TDBEnginePascalString);
begin
  FHashListBuff.Add(value);
end;

procedure TDBListPascalString.LoadFromStoreEngine(DBEng: TDBStoreBase);
var
  itmSearHnd: THeader;
  qState    : TQueryState;
begin
  Clear;
  qState.QueryHnd := @itmSearHnd;
  if DBEng.QueryFirst(qState) then
    begin
      repeat
        if qState.ID = c_PascalString then
            FHashListBuff.Add(DBEng.BuildPascalString(qState.StorePos));
      until not DBEng.QueryNext(qState);
    end;
end;

procedure TDBListPascalString.ExportToStoreEngine(DBEng: TDBStoreBase);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      DBEng.AddData(GetItems(i));
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

  FTriggerTime := 0;
  FTaskTag := '';

  FLastTime := 0;

  FStoped := False;
  FPaused := False;

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

procedure TQueryTask.Stop;
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

function TQueryTask.ProcessQuery: Boolean;
var
  tt: TTimeTickValue;
begin
  Result := False;
  if FStoped then
    begin
      DoQueryDone;
      exit;
    end;

  if FPaused then
    begin
      Result := True;
      exit;
    end;
  tt := GetTimeTick;

  if FInited then
    begin
      FState.NewTime := tt - FTriggerTime;
      FState.DeltaTime := tt - FLastTime;

      if FReverse then
        begin
          if not FDBEng.QueryPrev(FState) then
            begin
              DoQueryDone;
              exit;
            end;
          DoTriggerQuery;
          if FState.Aborted then
            begin
              DoQueryDone;
              exit;
            end;
          Result := True;
        end
      else
        begin
          if not FDBEng.QueryNext(FState) then
            begin
              DoQueryDone;
              exit;
            end;
          DoTriggerQuery;
          if FState.Aborted then
            begin
              DoQueryDone;
              exit;
            end;
          Result := True;
        end;

      FLastTime := GetTimeTick;
    end
  else
    begin
      FTriggerTime := tt;
      FLastTime := FTriggerTime;

      if FReverse then
        begin
          if not FDBEng.QueryLast(FState) then
            begin
              DoQueryDone;
              exit;
            end;
          DoTriggerQuery;
          if FState.Aborted then
            begin
              DoQueryDone;
              exit;
            end;
          Result := True;
        end
      else
        begin
          if not FDBEng.QueryFirst(FState) then
            begin
              DoQueryDone;
              exit;
            end;
          DoTriggerQuery;
          if FState.Aborted then
            begin
              DoQueryDone;
              exit;
            end;
          Result := True;
        end;

      if Result then
        begin
          FInited := True;
          FState.TaskTag := FTaskTag;
          FState.NewTime := GetTimeTick - FTriggerTime;
          FState.DeltaTime := FState.NewTime;
        end;
    end;
end;

function TQueryTask.ConsumTime: Double;
begin
  Result := FState.NewTime * 0.001;
end;

procedure TQueryThread.SyncQuery;
var
  i : Integer;
  qt: TQueryTask;
begin
  if StoreEngine = nil then
      exit;

  i := 0;
  while i < StoreEngine.FQueryQueue.Count do
    begin
      qt := StoreEngine.FQueryQueue[i] as TQueryTask;
      if not qt.ProcessQuery then
        begin
          DoStatus('complete query task,consum time: %dms', [qt.FState.NewTime]);
          DisposeObject(qt);
          StoreEngine.FQueryQueue.Delete(i);
        end
      else
          inc(i);
    end;

  Paused := StoreEngine.FQueryQueue.Count = 0;
  if Paused then
    begin
      StoreEngine.FQueryThreadLastActivtedTime := Now;
      SyncUpdateCacheState;
    end;
end;

procedure TQueryThread.SyncCheckCache;
var
  Allowed: Boolean;
begin
  if StoreEngine = nil then
      exit;

  Allowed := (not StoreEngine.AllowedCache) and (StoreEngine.FCache.Count > 0);

  if PausedIdleTime > StoreEngine.CacheAnnealingTime then
    begin
      PausedIdleTime := 0;
      if Allowed then
        begin
          StoreEngine.FCacheAnnealingState := Format('cleanup cache(stream:%d header:%d block:%d Item:%d field:%d)',
            [StoreEngine.FCache.Count,
            StoreEngine.FHeaderCache.Count,
            StoreEngine.FItemBlockCache.Count,
            StoreEngine.FItemCache.Count,
            StoreEngine.FFieldCache.Count]);
          StoreEngine.Recache;
        end;
    end
  else if Allowed then
      StoreEngine.FCacheAnnealingState := Format('Annealing Cooldown %d', [Round(StoreEngine.CacheAnnealingTime - PausedIdleTime)]);
end;

procedure TQueryThread.SyncUpdateCacheState;
begin
  if StoreEngine <> nil then
      StoreEngine.FCacheAnnealingState := Format('cache state(stream:%d header:%d)',
      [StoreEngine.FCache.Count,
      StoreEngine.FItemCache.Count]);
end;

procedure TQueryThread.Execute;
var
  cloop: SmallInt;
begin
  cloop := 0;
  while StoreEngine <> nil do
    begin
      PausedIdleTime := 0;
      while Paused do
        begin
          Sleep(100);
          PausedIdleTime := PausedIdleTime + 0.1;

          try
            {$IFDEF FPC}
            Synchronize(Self, @SyncCheckCache);
            {$ELSE}
            Synchronize(Self, SyncCheckCache);
            {$ENDIF}
          except
          end;
        end;

      try
        {$IFDEF FPC}
        Synchronize(Self, @SyncQuery);
        {$ELSE}
        Synchronize(Self, SyncQuery);
        {$ENDIF}
      except
      end;

      //
      if (cloop = 0) or (cloop > 10000) then
        begin
          cloop := 0;
          try
            {$IFDEF FPC}
            Synchronize(Self, @SyncUpdateCacheState);
            {$ELSE}
            Synchronize(Self, SyncUpdateCacheState);
            {$ENDIF}
          except
          end;
        end;
      inc(cloop);
    end;
end;

destructor TQueryThread.Destroy;
begin
  inherited Destroy;
end;

procedure TIndexCacheItem.Write(var wVal: TItem);
begin
  Description := wVal.Description;
  ExtID := wVal.ExtID;
  FirstBlockPOS := wVal.FirstBlockPOS;
  LastBlockPOS := wVal.LastBlockPOS;
  Size := wVal.Size;
  BlockCount := wVal.BlockCount;
  CurrentBlockSeekPOS := wVal.CurrentBlockSeekPOS;
  CurrentFileSeekPOS := wVal.CurrentFileSeekPOS;
  DataWrited := wVal.DataWrited;
  Return := wVal.Return;
end;

procedure TIndexCacheItem.Read(var rVal: TItem);
begin
  rVal.Description := Description;
  rVal.ExtID := ExtID;
  rVal.FirstBlockPOS := FirstBlockPOS;
  rVal.LastBlockPOS := LastBlockPOS;
  rVal.Size := Size;
  rVal.BlockCount := BlockCount;
  rVal.CurrentBlockSeekPOS := CurrentBlockSeekPOS;
  rVal.CurrentFileSeekPOS := CurrentFileSeekPOS;
  rVal.DataWrited := DataWrited;
  rVal.Return := Return;
end;

procedure TIndexCacheField.Write(var wVal: TField);
begin
  UpLevelFieldPOS := wVal.UpLevelFieldPOS;
  Description := wVal.Description;
  HeaderCount := wVal.HeaderCount;
  FirstHeaderPOS := wVal.FirstHeaderPOS;
  LastHeaderPOS := wVal.LastHeaderPOS;
  Return := wVal.Return;
end;

procedure TIndexCacheField.Read(var rVal: TField);
begin
  rVal.UpLevelFieldPOS := UpLevelFieldPOS;
  rVal.Description := Description;
  rVal.HeaderCount := HeaderCount;
  rVal.FirstHeaderPOS := FirstHeaderPOS;
  rVal.LastHeaderPOS := LastHeaderPOS;
  rVal.Return := Return;
end;

procedure TDBStoreBase.ReadHeaderInfo;
var
  f: TFieldHandle;
begin
  if not FDBEngine.GetPathField('/Store', FStoreFieldPos) then
      RaiseInfo('no exists store field');

  if not FDBEngine.GetFieldData(FStoreFieldPos, f) then
      RaiseInfo('store field data failed!');

  FCount := f.HeaderCount;
end;

procedure TDBStoreBase.ThreadFreeEvent(Sender: TObject);
begin
  FQueryThreadTerminate := True;
end;

procedure TDBStoreBase.DoCreateInit;
begin
  FQueryQueue := TCoreClassListForObj.Create;

  FQueryThread := TQueryThread.Create(True);
  FQueryThread.StoreEngine := Self;
  FQueryThread.Paused := True;
  FQueryThread.FreeOnTerminate := True;
  FQueryThreadTerminate := False;
  FQueryThreadLastActivtedTime := Now;

  FNotifyIntf := nil;

  FCache := TInt64HashObjectList.Create(DefaultCacheBufferLength);
  FCache.AutoFreeData := True;

  FCacheStyle := TCacheStyle.csAutomation;
  FCacheAnnealingTime := DefaultCacheAnnealingTime;
  FMaximumCache := DefaultMaximumCache;
  FMinimizeCache := DefaultMinimizeCache;
  FMinimizeCacheOfFileSize := DefaultMinimizeCacheOfFileSize;
  FCacheAnnealingState := '';

  FHeaderCache := TInt64HashPointerList.Create(DefaultIndexCacheSize);
  FHeaderCache.AutoFreeData := True;
  FHeaderCache.AccessOptimization := True;

  FItemBlockCache := TInt64HashPointerList.Create(DefaultIndexCacheSize);
  FItemBlockCache.AutoFreeData := True;
  FItemBlockCache.AccessOptimization := True;

  FItemCache := TInt64HashPointerList.Create(DefaultIndexCacheSize);
  FItemCache.AutoFreeData := True;
  FItemCache.AccessOptimization := True;

  FFieldCache := TInt64HashPointerList.Create(DefaultIndexCacheSize);
  FFieldCache.AutoFreeData := True;
  FFieldCache.AccessOptimization := True;

  {$IFDEF FPC}
  FHeaderCache.OnDataFreeProc := @HeaderCache_DataFreeProc;
  FItemBlockCache.OnDataFreeProc := @ItemBlockCache_DataFreeProc;
  FItemCache.OnDataFreeProc := @ItemCache_DataFreeProc;
  FFieldCache.OnDataFreeProc := @FieldCache_DataFreeProc;
  {$ELSE}
  FHeaderCache.OnDataFreeProc := HeaderCache_DataFreeProc;
  FItemBlockCache.OnDataFreeProc := ItemBlockCache_DataFreeProc;
  FItemCache.OnDataFreeProc := ItemCache_DataFreeProc;
  FFieldCache.OnDataFreeProc := FieldCache_DataFreeProc;
  {$ENDIF}
  FResultDF := TDBEngineDF.Create;
  FResultVL := TDBEngineVL.Create;
  FResultTE := TDBEngineTE.Create;
  {$IFNDEF FPC}
  FResultJson := TDBEngineJson.Create;
  {$ENDIF}
  FResultPascalString := TDBEnginePascalString.Create;

  {$IFDEF FPC}
  FQueryThread.OnTerminate := @ThreadFreeEvent;
  {$ELSE}
  FQueryThread.OnTerminate := ThreadFreeEvent;
  {$ENDIF}
  FQueryThread.Suspended := False;

  if not IsMemoryMode then
    begin
      {$IFDEF FPC}
      FDBEngine.ObjectDataHandlePtr^.OnWriteHeader := @HeaderWriteProc;
      FDBEngine.ObjectDataHandlePtr^.OnReadHeader := @HeaderReadProc;
      FDBEngine.ObjectDataHandlePtr^.OnWriteItemBlock := @ItemBlockWriteProc;
      FDBEngine.ObjectDataHandlePtr^.OnReadItemBlock := @ItemBlockReadProc;
      FDBEngine.ObjectDataHandlePtr^.OnWriteItem := @ItemWriteProc;
      FDBEngine.ObjectDataHandlePtr^.OnReadItem := @ItemReadProc;
      FDBEngine.ObjectDataHandlePtr^.OnOnlyWriteItemRec := @OnlyItemRecWriteProc;
      FDBEngine.ObjectDataHandlePtr^.OnOnlyReadItemRec := @OnlyItemRecReadProc;
      FDBEngine.ObjectDataHandlePtr^.OnWriteField := @FieldWriteProc;
      FDBEngine.ObjectDataHandlePtr^.OnReadField := @FieldReadProc;
      FDBEngine.ObjectDataHandlePtr^.OnOnlyWriteFieldRec := @OnlyFieldRecWriteProc;
      FDBEngine.ObjectDataHandlePtr^.OnOnlyReadFieldRec := @OnlyFieldRecReadProc;
      {$ELSE}
      FDBEngine.ObjectDataHandlePtr^.OnWriteHeader := HeaderWriteProc;
      FDBEngine.ObjectDataHandlePtr^.OnReadHeader := HeaderReadProc;
      FDBEngine.ObjectDataHandlePtr^.OnWriteItemBlock := ItemBlockWriteProc;
      FDBEngine.ObjectDataHandlePtr^.OnReadItemBlock := ItemBlockReadProc;
      FDBEngine.ObjectDataHandlePtr^.OnWriteItem := ItemWriteProc;
      FDBEngine.ObjectDataHandlePtr^.OnReadItem := ItemReadProc;
      FDBEngine.ObjectDataHandlePtr^.OnOnlyWriteItemRec := OnlyItemRecWriteProc;
      FDBEngine.ObjectDataHandlePtr^.OnOnlyReadItemRec := OnlyItemRecReadProc;
      FDBEngine.ObjectDataHandlePtr^.OnWriteField := FieldWriteProc;
      FDBEngine.ObjectDataHandlePtr^.OnReadField := FieldReadProc;
      FDBEngine.ObjectDataHandlePtr^.OnOnlyWriteFieldRec := OnlyFieldRecWriteProc;
      FDBEngine.ObjectDataHandlePtr^.OnOnlyReadFieldRec := OnlyFieldRecReadProc;
      {$ENDIF}
    end;
end;

procedure TDBStoreBase.HeaderCache_DataFreeProc(p: Pointer);
begin
  Dispose(PIndexCacheHeader(p));
end;

procedure TDBStoreBase.ItemBlockCache_DataFreeProc(p: Pointer);
begin
  Dispose(PIndexCacheItemBlock(p));
end;

procedure TDBStoreBase.ItemCache_DataFreeProc(p: Pointer);
begin
  Dispose(PIndexCacheItem(p));
end;

procedure TDBStoreBase.FieldCache_DataFreeProc(p: Pointer);
begin
  Dispose(PIndexCacheField(p));
end;

procedure TDBStoreBase.HeaderWriteProc(fPos: Int64; var wVal: THeader);
var
  p: PIndexCacheHeader;
begin
  p := PIndexCacheHeader(FHeaderCache[wVal.CurrentHeader]);
  if p = nil then
    begin
      new(p);
      p^ := wVal;
      FHeaderCache.Add(wVal.CurrentHeader, p, False);
    end
  else
      p^ := wVal;

  p^.Return := db_Header_ok;
end;

procedure TDBStoreBase.HeaderReadProc(fPos: Int64; var rVal: THeader; var Done: Boolean);
var
  p: PIndexCacheHeader;
begin
  p := PIndexCacheHeader(FHeaderCache[fPos]);
  Done := p <> nil;
  if not Done then
      exit;
  rVal := p^;
end;

procedure TDBStoreBase.ItemBlockWriteProc(fPos: Int64; var wVal: TItemBlock);
var
  p: PIndexCacheItemBlock;
begin
  p := PIndexCacheItemBlock(FItemBlockCache[wVal.CurrentBlockPOS]);
  if p = nil then
    begin
      new(p);
      p^ := wVal;
      FItemBlockCache.Add(wVal.CurrentBlockPOS, p, False);
    end
  else
      p^ := wVal;

  p^.Return := db_Item_ok;
end;

procedure TDBStoreBase.ItemBlockReadProc(fPos: Int64; var rVal: TItemBlock; var Done: Boolean);
var
  p: PIndexCacheItemBlock;
begin
  p := PIndexCacheItemBlock(FItemBlockCache[fPos]);
  Done := p <> nil;
  if not Done then
      exit;
  rVal := p^;
end;

procedure TDBStoreBase.ItemWriteProc(fPos: Int64; var wVal: TItem);
var
  p: PIndexCacheItem;
begin
  HeaderWriteProc(fPos, wVal.RHeader);

  p := PIndexCacheItem(FItemCache[wVal.RHeader.DataMainPOS]);
  if p = nil then
    begin
      new(p);
      p^.Write(wVal);
      FItemCache.Add(wVal.RHeader.DataMainPOS, p, False);
    end
  else
      p^.Write(wVal);

  p^.Return := db_Item_ok;
end;

procedure TDBStoreBase.ItemReadProc(fPos: Int64; var rVal: TItem; var Done: Boolean);
var
  p: PIndexCacheItem;
begin
  HeaderReadProc(fPos, rVal.RHeader, Done);

  if not Done then
      exit;

  p := PIndexCacheItem(FItemCache[rVal.RHeader.DataMainPOS]);
  Done := p <> nil;
  if not Done then
      exit;

  p^.Read(rVal);
end;

procedure TDBStoreBase.OnlyItemRecWriteProc(fPos: Int64; var wVal: TItem);
var
  p: PIndexCacheItem;
begin
  p := PIndexCacheItem(FItemCache[fPos]);
  if p = nil then
    begin
      new(p);
      p^.Write(wVal);
      FItemCache.Add(fPos, p, False);
    end
  else
      p^.Write(wVal);

  p^.Return := db_Item_ok;
end;

procedure TDBStoreBase.OnlyItemRecReadProc(fPos: Int64; var rVal: TItem; var Done: Boolean);
var
  p: PIndexCacheItem;
begin
  p := PIndexCacheItem(FItemCache[fPos]);
  Done := p <> nil;
  if not Done then
      exit;

  p^.Read(rVal);
end;

procedure TDBStoreBase.FieldWriteProc(fPos: Int64; var wVal: TField);
var
  p: PIndexCacheField;
begin
  HeaderWriteProc(fPos, wVal.RHeader);

  p := PIndexCacheField(FFieldCache[wVal.RHeader.DataMainPOS]);
  if p = nil then
    begin
      new(p);
      p^.Write(wVal);
      FFieldCache.Add(wVal.RHeader.DataMainPOS, p, False);
    end
  else
      p^.Write(wVal);

  p^.Return := db_Field_ok;
end;

procedure TDBStoreBase.FieldReadProc(fPos: Int64; var rVal: TField; var Done: Boolean);
var
  p: PIndexCacheField;
begin
  HeaderReadProc(fPos, rVal.RHeader, Done);

  if not Done then
      exit;

  p := PIndexCacheField(FFieldCache[rVal.RHeader.DataMainPOS]);
  Done := p <> nil;
  if not Done then
      exit;

  p^.Read(rVal);
end;

procedure TDBStoreBase.OnlyFieldRecWriteProc(fPos: Int64; var wVal: TField);
var
  p: PIndexCacheField;
begin
  p := PIndexCacheField(FFieldCache[fPos]);
  if p = nil then
    begin
      new(p);
      p^.Write(wVal);
      FFieldCache.Add(fPos, p, False);
    end
  else
      p^.Write(wVal);

  p^.Return := db_Field_ok;
end;

procedure TDBStoreBase.OnlyFieldRecReadProc(fPos: Int64; var rVal: TField; var Done: Boolean);
var
  p: PIndexCacheField;
begin
  p := PIndexCacheField(FFieldCache[fPos]);
  Done := p <> nil;
  if not Done then
      exit;

  p^.Read(rVal);
end;

constructor TDBStoreBase.Create(DBFile: SystemString; OnlyRead: Boolean);
begin
  inherited Create;
  FDBEngine := TObjectDataManager.Create(DBFile, ObjectDataMarshal.ID, OnlyRead);
  ReadHeaderInfo;

  DoCreateInit;
end;

constructor TDBStoreBase.CreateMemory(DBMemory: TCoreClassStream; OnlyRead: Boolean);
begin
  inherited Create;
  FDBEngine := TObjectDataManager.CreateAsStream(DBMemory, '', ObjectDataMarshal.ID, OnlyRead, False, True);
  ReadHeaderInfo;

  DoCreateInit;
end;

constructor TDBStoreBase.CreateNew(DBFile: SystemString);
begin
  inherited Create;
  FDBEngine := TObjectDataManager.CreateNew(DBFile, ObjectDataMarshal.ID);
  FDBEngine.CreateField('/Store', '');
  ReadHeaderInfo;

  DoCreateInit;
end;

constructor TDBStoreBase.CreateNewMemory;
begin
  inherited Create;
  FDBEngine := TObjectDataManager.CreateAsStream(TMemoryStream64.Create, '', ObjectDataMarshal.ID, False, True, True);
  FDBEngine.CreateField('/Store', '');
  ReadHeaderInfo;

  DoCreateInit;
end;

destructor TDBStoreBase.Destroy;
var
  i: Integer;
begin
  FQueryThread.StoreEngine := nil;
  FQueryThread.Paused := False;

  // wait thread
  while not FQueryThreadTerminate do
      Classes.CheckSynchronize;

  for i := 0 to FQueryQueue.Count - 1 do
      DisposeObject(FQueryQueue[i]);
  DisposeObject([FDBEngine, FQueryQueue, FCache, FHeaderCache, FItemBlockCache, FItemCache, FFieldCache]);
  DisposeObject([FResultDF, FResultVL, FResultTE, FResultPascalString]);
  {$IFNDEF FPC}
  DisposeObject(FResultJson);
  {$ENDIF}
  inherited Destroy;
end;

procedure TDBStoreBase.CompressTo(DestDB: TObjectDataManager);
begin
  DoStatus('build struct...');
  DestDB.CreateField('/Store', '');

  DoStatus('compress data...');
  FDBEngine.CopyFieldToPath(FStoreFieldPos, DestDB, '/Store');

  DestDB.Update;
  DoStatus('build finish...', []);
end;

procedure TDBStoreBase.Compress;
var
  DestDB   : TObjectDataManager;
  fn, oldFN: SystemString;
  i        : Integer;
begin
  StopAllQuery;
  Recache;

  if FDBEngine.StreamEngine <> nil then
    begin
      DestDB := TObjectDataManager.CreateAsStream(TMemoryStream64.Create, '', ObjectDataMarshal.ID, False, True, True);
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
      DestDB := TObjectDataManager.CreateNew(fn, ObjectDataMarshal.ID);
      CompressTo(DestDB);
      DisposeObject([FDBEngine, DestDB]);

      umlDeleteFile(oldFN);
      umlRenameFile(fn, oldFN);

      FDBEngine := TObjectDataManager.Create(oldFN, ObjectDataMarshal.ID, False);
      ReadHeaderInfo;
    end;
end;

procedure TDBStoreBase.Update;
begin
  FDBEngine.Update;
end;

procedure TDBStoreBase.SaveToStream(Stream: TCoreClassStream);
var
  DestDB: TObjectDataManager;
begin
  DestDB := TObjectDataManager.CreateAsStream(TMemoryStream64.Create, '', ObjectDataMarshal.ID, False, True, False);
  CompressTo(DestDB);
  DisposeObject(DestDB);
end;

procedure TDBStoreBase.SaveToFile(fn: SystemString);
var
  DestDB: TObjectDataManager;
begin
  DestDB := TObjectDataManager.CreateNew(fn, ObjectDataMarshal.ID);
  CompressTo(DestDB);
  DisposeObject(DestDB);
end;

procedure TDBStoreBase.LoadFromStream(Stream: TCoreClassStream);
var
  DestDB: TObjectDataManager;
begin
  StopAllQuery;
  Recache;

  FDBEngine.FieldDelete('/', 'Store');
  FDBEngine.CreateField('/Store', '');
  ReadHeaderInfo;
  Compress;

  DestDB := TObjectDataManager.CreateAsStream(Stream, '', ObjectDataMarshal.ID, True, False, False);
  DestDB.CopyFieldToPath(DestDB.GetPathFieldPos('/Store'), FDBEngine, '/Store');
  DisposeObject(DestDB);
end;

procedure TDBStoreBase.LoadFromFile(fn: SystemString);
var
  fs: TCoreClassFileStream;
begin
  if not umlFileExists(fn) then
      exit;
  fs := TCoreClassFileStream.Create(fn, fmOpenRead or fmShareDenyWrite);
  try
      LoadFromStream(fs);
  finally
      DisposeObject(fs);
  end;
end;

function TDBStoreBase.IsMemoryMode: Boolean;
begin
  Result := FDBEngine.StreamEngine is TMemoryStream64;
end;

function TDBStoreBase.IsReadOnly: Boolean;
begin
  Result := FDBEngine.IsOnlyRead;
end;

procedure TDBStoreBase.ResetDB;
begin
  StopAllQuery;
  Recache;

  FDBEngine.FieldDelete('/', 'Store');
  FDBEngine.CreateField('/Store', '');
  ReadHeaderInfo;
  Compress;
end;

function TDBStoreBase.RenameDB(newName: SystemString): Boolean;
var
  oldFN, newfn: SystemString;
begin
  StopAllQuery;
  Recache;

  Result := False;
  if FDBEngine.IsOnlyRead then
      exit;

  if IsMemoryMode then
    begin
      FDBEngine.ObjectName := newName;
      Result := True;
      exit;
    end;
  oldFN := FDBEngine.ObjectName;
  if not umlFileExists(oldFN) then
      exit;

  FDBEngine.Update;
  DisposeObject(FDBEngine);

  newfn := umlCombineFileName(umlGetFilePath(oldFN), newName).Text;

  if umlRenameFile(oldFN, newfn) then
    begin
      oldFN := newfn;
      Result := True;
    end;

  FDBEngine := TObjectDataManager.Create(oldFN, ObjectDataMarshal.ID, False);
  ReadHeaderInfo;
end;

procedure TDBStoreBase.Recache;
begin
  FCache.Clear;
  FHeaderCache.Clear;
  FItemBlockCache.Clear;
  FItemCache.Clear;
  FFieldCache.Clear;

  FResultDF.Clear;
  FResultVL.Clear;
  FResultTE.Clear;
  {$IFNDEF FPC}
  FResultJson.Clear;
  {$ENDIF}
  FResultPascalString.Clear;
end;

function TDBStoreBase.AllowedCache: Boolean;
begin
  case FCacheStyle of
    TCacheStyle.csAutomation:
      begin
        if FDBEngine.StreamEngine is TMemoryStream64 then
            Result := False
        else if (FCache.Count < FMinimizeCache) then
            Result := True
        else if (FQueryQueue.Count >= 2) and (FCache.Count < FMaximumCache) then
            Result := True
        else if FDBEngine.Size < FMinimizeCacheOfFileSize then
            Result := True
        else
            Result := False;
      end;
    TCacheStyle.csDisabled: Result := False;
    else Result := True;
  end;
end;

function TDBStoreBase.InsertData(const InsertPos: Int64; buff: TCoreClassStream; ID: Cardinal; var itmHnd: TItemHandle): Int64;
var
  itmStream: TItemStream;
begin
  Result := -1;

  if FDBEngine.ItemFastInsertNew(FStoreFieldPos, InsertPos, '', '', itmHnd) then
    begin
      itmHnd.Item.RHeader.UserProperty := ID;
      itmHnd.Name := '0x' + TCipher.BuffToString(@itmHnd.Item.RHeader.CurrentHeader, umlInt64Length);
      itmStream := TItemStream.Create(FDBEngine, itmHnd);
      buff.Position := 0;
      itmStream.CopyFrom(buff, buff.Size);
      itmStream.UpdateHandle;
      DisposeObject(itmStream);
      Result := itmHnd.Item.RHeader.CurrentHeader;
      inc(FCount);

      try
        if Assigned(FNotifyIntf) then
            FNotifyIntf.DoInsertData(Self, InsertPos, buff, ID, Result);
      except
      end;
    end;
end;

function TDBStoreBase.InsertData(const InsertPos: Int64; buff: TCoreClassStream; ID: Cardinal): Int64;
var
  itmHnd: TItemHandle;
begin
  Result := InsertData(InsertPos, buff, ID, itmHnd);
end;

function TDBStoreBase.AddData(buff: TCoreClassStream; ID: Cardinal; var itmHnd: TItemHandle): Int64;
var
  itmStream: TItemStream;
begin
  Result := -1;

  if IsReadOnly then
      exit;

  if FDBEngine.ItemFastCreate(FStoreFieldPos, '', '', itmHnd) then
    begin
      itmHnd.Item.RHeader.UserProperty := ID;
      itmHnd.Name := '0x' + TCipher.BuffToString(@itmHnd.Item.RHeader.CurrentHeader, umlInt64Length);
      itmStream := TItemStream.Create(FDBEngine, itmHnd);
      buff.Position := 0;
      itmStream.CopyFrom(buff, buff.Size);
      itmStream.UpdateHandle;
      DisposeObject(itmStream);
      Result := itmHnd.Item.RHeader.CurrentHeader;
      inc(FCount);

      try
        if Assigned(FNotifyIntf) then
            FNotifyIntf.DoAddData(Self, buff, ID, Result);
      except
      end;
    end;
end;

function TDBStoreBase.AddData(buff: TCoreClassStream; ID: Cardinal): Int64;
var
  itmHnd: TItemHandle;
begin
  Result := AddData(buff, ID, itmHnd);
end;

function TDBStoreBase.SetData(const StorePos: Int64; buff: TCoreClassStream): Boolean;
var
  itmHnd   : TItemHandle;
  itmStream: TItemStream;
begin
  Result := False;

  if IsReadOnly then
      exit;

  if FDBEngine.ItemFastResetBody(StorePos) then
    if FDBEngine.ItemFastOpen(StorePos, itmHnd) then
      begin
        itmStream := TItemStream.Create(FDBEngine, itmHnd);
        buff.Position := 0;
        itmStream.CopyFrom(buff, buff.Size);
        itmStream.UpdateHandle;
        DisposeObject(itmStream);
        Result := True;

        FCache.Delete(StorePos);

        try
          if Assigned(FNotifyIntf) then
              FNotifyIntf.DoModifyData(Self, StorePos, buff);
        except
        end;
      end;
end;

function TDBStoreBase.GetData(const StorePos: Int64; ID: Cardinal): TItemStream;
var
  itmHnd: TItemHandle;
begin
  Result := nil;

  if FDBEngine.ItemFastOpen(StorePos, itmHnd) then
    begin
      if ID = itmHnd.Item.RHeader.UserProperty then
          Result := TItemStream.Create(FDBEngine, itmHnd);
    end;
end;

function TDBStoreBase.GetData(const StorePos: Int64): TItemStream;
var
  itmHnd: TItemHandle;
begin
  Result := nil;

  if FDBEngine.ItemFastOpen(StorePos, itmHnd) then
      Result := TItemStream.Create(FDBEngine, itmHnd);
end;

function TDBStoreBase.DeleteData(const StorePos: Int64): Boolean;
var
  itmHnd: TItemHandle;
begin
  Result := False;
  if IsReadOnly then
      exit;

  FCache.Delete(StorePos);

  if FDBEngine.ItemFastOpen(StorePos, itmHnd) then
    begin
      FHeaderCache.Delete(itmHnd.Item.RHeader.CurrentHeader);
      FItemCache.Delete(itmHnd.Item.RHeader.DataMainPOS);
      FItemBlockCache.Delete(itmHnd.Item.FirstBlockPOS);
    end;

  Result := FDBEngine.FastDelete(FStoreFieldPos, StorePos);
  if Result then
    begin
      Dec(FCount);

      try
        if Assigned(FNotifyIntf) then
            FNotifyIntf.DoDeleteData(Self, StorePos);
      except
      end;
    end;
end;

function TDBStoreBase.GetSize(const StorePos: Int64): Int64;
var
  itmHnd: TItemHandle;
begin
  Result := -1;
  if DBEngine.ItemFastOpen(StorePos, itmHnd) then
      Result := itmHnd.Item.Size;
end;

function TDBStoreBase.QueryFirst(var qState: TQueryState): Boolean;
begin
  Result := False;
  qState.DBEng := Self;
  qState.StorePos := -1;
  qState.Aborted := False;
  qState.TaskTag := '';
  qState.DeltaTime := 0;
  qState.NewTime := 0;
  if qState.QueryHnd = nil then
      exit;

  try
    Result := FDBEngine.GetFirstHeaderFromField(FStoreFieldPos, qState.QueryHnd^);
    if Result then
        qState.StorePos := qState.QueryHnd^.CurrentHeader;
  except
      Result := False;
  end;
end;

function TDBStoreBase.QueryNext(var qState: TQueryState): Boolean;
begin
  Result := False;

  if qState.QueryHnd = nil then
      exit;
  if qState.QueryHnd^.PositionID in [db_Header_LastPositionFlags, db_Header_OnlyPositionFlags] then
      exit;

  try
    Result := FDBEngine.GetHeader(qState.QueryHnd^.NextHeader, qState.QueryHnd^);
    if Result then
        qState.StorePos := qState.QueryHnd^.CurrentHeader;
  except
      Result := False;
  end;
end;

function TDBStoreBase.QueryLast(var qState: TQueryState): Boolean;
begin
  Result := False;
  qState.DBEng := Self;
  qState.StorePos := -1;
  qState.Aborted := False;
  qState.TaskTag := '';
  qState.DeltaTime := 0;
  qState.NewTime := 0;
  if qState.QueryHnd = nil then
      exit;

  try
    Result := FDBEngine.GetLastHeaderFromField(FStoreFieldPos, qState.QueryHnd^);
    if Result then
        qState.StorePos := qState.QueryHnd^.CurrentHeader;
  except
      Result := False;
  end;
end;

function TDBStoreBase.QueryPrev(var qState: TQueryState): Boolean;
begin
  Result := False;

  if qState.QueryHnd = nil then
      exit;
  if qState.QueryHnd^.PositionID in [db_Header_FirstPositionFlags, db_Header_OnlyPositionFlags] then
      exit;

  try
    Result := FDBEngine.GetHeader(qState.QueryHnd^.PrevHeader, qState.QueryHnd^);
    if Result then
        qState.StorePos := qState.QueryHnd^.CurrentHeader;
  except
      Result := False;
  end;
end;

{$IFDEF FPC}


procedure TDBStoreBase.WaitQuery(ReverseQuery: Boolean; const OnQueryCall: TQueryCall; const OnQueryMethod: TQueryMethod);
type
  TDynamicQueryMethod = function(var qState: TQueryState): Boolean of object;
var
  itmSearHnd: THeader;
  qState    : TQueryState;
  f, n      : TDynamicQueryMethod;
begin
  qState.StorePos := -1;
  qState.Aborted := False;
  qState.QueryHnd := @itmSearHnd;

  if ReverseQuery then
    begin
      f := @QueryLast;
      n := @QueryPrev;
    end
  else
    begin
      f := @QueryFirst;
      n := @QueryNext;
    end;

  if f(qState) then
    begin
      repeat
        try
          if Assigned(OnQueryCall) then
              OnQueryCall(qState);
          if Assigned(OnQueryMethod) then
              OnQueryMethod(qState);
        except
        end;

        if qState.Aborted then
            break;

      until (not n(qState));
    end;
end;
{$ELSE}


procedure TDBStoreBase.WaitQuery(ReverseQuery: Boolean; const OnQueryCall: TQueryCall; const OnQueryProc: TQueryProc; const OnQueryMethod: TQueryMethod);
type
  TDynamicQueryMethod = function(var qState: TQueryState): Boolean of object;
var
  itmSearHnd: THeader;
  qState    : TQueryState;
  f, n      : TDynamicQueryMethod;
begin
  qState.StorePos := -1;
  qState.Aborted := False;
  qState.QueryHnd := @itmSearHnd;

  if ReverseQuery then
    begin
      f := QueryLast;
      n := QueryPrev;
    end
  else
    begin
      f := QueryFirst;
      n := QueryNext;
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
            break;

      until (not n(qState));
    end;
end;
{$ENDIF}


procedure TDBStoreBase.WaitQuery(ReverseQuery: Boolean; const OnQueryCall: TQueryCall);
begin
  {$IFDEF FPC}
  WaitQuery(ReverseQuery, OnQueryCall, nil);
  {$ELSE}
  WaitQuery(ReverseQuery, OnQueryCall, nil, nil);
  {$ENDIF}
end;

procedure TDBStoreBase.WaitQuery(ReverseQuery: Boolean; const OnQueryMethod: TQueryMethod);
begin
  {$IFDEF FPC}
  WaitQuery(ReverseQuery, nil, OnQueryMethod);
  {$ELSE}
  WaitQuery(ReverseQuery, nil, nil, OnQueryMethod);
  {$ENDIF}
end;

{$IFNDEF FPC}


procedure TDBStoreBase.WaitQuery(ReverseQuery: Boolean; const OnQueryProc: TQueryProc);
begin
  WaitQuery(ReverseQuery, nil, OnQueryProc, nil);
end;
{$ENDIF}


procedure TDBStoreBase.WaitQuery(const OnQueryCall: TQueryCall);
begin
  WaitQuery(False, OnQueryCall);
end;

procedure TDBStoreBase.WaitQuery(const OnQueryMethod: TQueryMethod);
begin
  WaitQuery(False, OnQueryMethod);
end;

{$IFNDEF FPC}


procedure TDBStoreBase.WaitQuery(const OnQueryProc: TQueryProc);
begin
  WaitQuery(False, OnQueryProc);
end;

{$ENDIF}

{$IFDEF FPC}


function TDBStoreBase.Query(const TaskTag: SystemString; const ReverseQuery: Boolean;
  const OnQueryCall: TQueryCall; const OnQueryDoneCall: TQueryDoneCall;
  const OnQueryMethod: TQueryMethod; const OnQueryDoneMethod: TQueryDoneMethod): TQueryTask;
begin
  Result := TQueryTask.Create;
  Result.FDBEng := Self;
  Result.FReverse := ReverseQuery;
  Result.FTaskTag := TaskTag;
  Result.FOnQueryCall := OnQueryCall;
  Result.FOnQueryDoneCall := OnQueryDoneCall;
  Result.FOnQueryMethod := OnQueryMethod;
  Result.FOnQueryDoneMethod := OnQueryDoneMethod;
  FQueryQueue.Add(Result);
  FQueryThread.Paused := False;
  FQueryThreadLastActivtedTime := Now;
end;

function TDBStoreBase.Query(const TaskTag: SystemString; const ReverseQuery: Boolean; const OnQueryCall: TQueryCall; const OnQueryDoneCall: TQueryDoneCall): TQueryTask;
begin
  Result := Query(TaskTag, ReverseQuery, OnQueryCall, OnQueryDoneCall, nil, nil);
end;

function TDBStoreBase.Query(const TaskTag: SystemString; const ReverseQuery: Boolean; const OnQueryMethod: TQueryMethod; const OnQueryDoneMethod: TQueryDoneMethod): TQueryTask;
begin
  Result := Query(TaskTag, ReverseQuery, nil, nil, OnQueryMethod, OnQueryDoneMethod);
end;

function TDBStoreBase.Query(const TaskTag: SystemString; const OnQueryCall: TQueryCall; const OnQueryDoneCall: TQueryDoneCall): TQueryTask;
begin
  Result := Query(TaskTag, False, OnQueryCall, OnQueryDoneCall);
end;

function TDBStoreBase.Query(const TaskTag: SystemString; const OnQueryMethod: TQueryMethod; const OnQueryDoneMethod: TQueryDoneMethod): TQueryTask;
begin
  Result := Query(TaskTag, False, OnQueryMethod, OnQueryDoneMethod);
end;

function TDBStoreBase.Query(const OnQueryCall: TQueryCall; const OnQueryDoneCall: TQueryDoneCall): TQueryTask;
begin
  Result := Query('', OnQueryCall, OnQueryDoneCall);
end;

function TDBStoreBase.Query(const OnQueryMethod: TQueryMethod; const OnQueryDoneMethod: TQueryDoneMethod): TQueryTask;
begin
  Result := Query('', OnQueryMethod, OnQueryDoneMethod);
end;

function TDBStoreBase.Query(const ReverseQuery: Boolean; const OnQueryCall: TQueryCall; const OnQueryDoneCall: TQueryDoneCall): TQueryTask;
begin
  Result := Query('', ReverseQuery, OnQueryCall, OnQueryDoneCall);
end;

function TDBStoreBase.Query(const ReverseQuery: Boolean; const OnQueryMethod: TQueryMethod; const OnQueryDoneMethod: TQueryDoneMethod): TQueryTask;
begin
  Result := Query('', ReverseQuery, OnQueryMethod, OnQueryDoneMethod);
end;

{$ELSE}


function TDBStoreBase.Query(const TaskTag: SystemString; const ReverseQuery: Boolean;
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

function TDBStoreBase.Query(const TaskTag: SystemString; const ReverseQuery: Boolean; const OnQueryCall: TQueryCall; const OnQueryDoneCall: TQueryDoneCall): TQueryTask;
begin
  Result := Query(TaskTag, ReverseQuery, OnQueryCall, OnQueryDoneCall, nil, nil, nil, nil);
end;

function TDBStoreBase.Query(const TaskTag: SystemString; const ReverseQuery: Boolean; const OnQueryProc: TQueryProc; const OnQueryDoneProc: TQueryDoneProc): TQueryTask;
begin
  Result := Query(TaskTag, ReverseQuery, nil, nil, OnQueryProc, OnQueryDoneProc, nil, nil);
end;

function TDBStoreBase.Query(const TaskTag: SystemString; const ReverseQuery: Boolean; const OnQueryMethod: TQueryMethod; const OnQueryDoneMethod: TQueryDoneMethod): TQueryTask;
begin
  Result := Query(TaskTag, ReverseQuery, nil, nil, nil, nil, OnQueryMethod, OnQueryDoneMethod);
end;

function TDBStoreBase.Query(const TaskTag: SystemString; const OnQueryCall: TQueryCall; const OnQueryDoneCall: TQueryDoneCall): TQueryTask;
begin
  Result := Query(TaskTag, False, OnQueryCall, OnQueryDoneCall);
end;

function TDBStoreBase.Query(const TaskTag: SystemString; const OnQueryProc: TQueryProc; const OnQueryDoneProc: TQueryDoneProc): TQueryTask;
begin
  Result := Query(TaskTag, False, OnQueryProc, OnQueryDoneProc);
end;

function TDBStoreBase.Query(const TaskTag: SystemString; const OnQueryMethod: TQueryMethod; const OnQueryDoneMethod: TQueryDoneMethod): TQueryTask;
begin
  Result := Query(TaskTag, False, OnQueryMethod, OnQueryDoneMethod);
end;

function TDBStoreBase.Query(const OnQueryCall: TQueryCall; const OnQueryDoneCall: TQueryDoneCall): TQueryTask;
begin
  Result := Query('', OnQueryCall, OnQueryDoneCall);
end;

function TDBStoreBase.Query(const OnQueryProc: TQueryProc; const OnQueryDoneProc: TQueryDoneProc): TQueryTask;
begin
  Result := Query('', OnQueryProc, OnQueryDoneProc);
end;

function TDBStoreBase.Query(const ReverseQuery: Boolean; const OnQueryCall: TQueryCall; const OnQueryDoneCall: TQueryDoneCall): TQueryTask;
begin
  Result := Query('', ReverseQuery, OnQueryCall, OnQueryDoneCall);
end;

function TDBStoreBase.Query(const ReverseQuery: Boolean; const OnQueryMethod: TQueryMethod; const OnQueryDoneMethod: TQueryDoneMethod): TQueryTask;
begin
  Result := Query('', ReverseQuery, OnQueryMethod, OnQueryDoneMethod);
end;

function TDBStoreBase.Query(const ReverseQuery: Boolean; const OnQueryProc: TQueryProc; const OnQueryDoneProc: TQueryDoneProc): TQueryTask;
begin
  Result := Query('', ReverseQuery, OnQueryProc, OnQueryDoneProc);
end;

function TDBStoreBase.Query(const OnQueryMethod: TQueryMethod; const OnQueryDoneMethod: TQueryDoneMethod): TQueryTask;
begin
  Result := Query('', OnQueryMethod, OnQueryDoneMethod);
end;

{$ENDIF}


procedure TDBStoreBase.WaitQueryThread;
begin
  while not FQueryThread.Paused do
      Classes.CheckSynchronize;
end;

procedure TDBStoreBase.WaitQueryThread(waitTime: TTimeTickValue);
var
  st: TTimeTickValue;
begin
  st := GetTimeTick + waitTime;
  while (not FQueryThread.Paused) and (waitTime > 0) and (GetTimeTick < st) do
      Classes.CheckSynchronize;
end;

function TDBStoreBase.QueryProcessing: Boolean;
begin
  Result := not FQueryThread.Paused;
end;

procedure TDBStoreBase.StopQuery(const TaskTag: SystemString);
var
  i: Integer;
  t: TQueryTask;
begin
  i := 0;
  while i < FQueryQueue.Count do
    begin
      t := TQueryTask(FQueryQueue[i]);
      if umlMultipleMatch(TaskTag, t.FTaskTag) then
          t.Stop;
      inc(i);
    end;
end;

procedure TDBStoreBase.StopAllQuery;
var
  i: Integer;
begin
  for i := 0 to FQueryQueue.Count - 1 do
      TQueryTask(FQueryQueue[i]).Stop;
  FQueryThread.SyncQuery;
end;

function TDBStoreBase.InsertData(const InsertPos: Int64; buff: TDataFrameEngine): Int64;
var
  m: TMemoryStream64;
begin
  m := TMemoryStream64.Create;

  buff.EncodeTo(m, False);

  Result := InsertData(InsertPos, m, c_DF);
  DisposeObject(m);
end;

function TDBStoreBase.AddData(buff: TDataFrameEngine): Int64;
var
  m: TMemoryStream64;
begin
  m := TMemoryStream64.Create;

  buff.EncodeTo(m, False);

  Result := AddData(m, c_DF);
  DisposeObject(m);
end;

function TDBStoreBase.GetDF(const StorePos: Int64): TDBEngineDF;
var
  lastAcc: TCoreClassObject;
  m      : TItemStream;
begin
  lastAcc := FCache[StorePos];
  if lastAcc is TDBEngineDF then
    begin
      Result := lastAcc as TDBEngineDF;
      Result.Reader.index := 0;
      exit;
    end;
  if lastAcc <> nil then
      FCache.Delete(StorePos);

  Result := nil;

  m := GetData(StorePos, c_DF);
  if m <> nil then
    begin
      m.Position := 0;

      if AllowedCache then
          Result := TDBEngineDF.Create
      else
          Result := FResultDF;

      try
          Result.DecodeFrom(m, True);
      except
      end;
      Result.DBStorePos := StorePos;
      Result.DBEng := Self;
      with m.Hnd^ do
        begin
          Result.CreateTime := CreateTime;
          Result.LastModifyTime := LastModifyTime;
        end;
      if AllowedCache then
          FCache.Add(StorePos, Result, False);
      DisposeObject(m);
    end;
end;

function TDBStoreBase.GetDF(var qState: TQueryState): TDBEngineDF;
begin
  Result := GetDF(qState.StorePos);
end;

function TDBStoreBase.BuildDF(const StorePos: Int64): TDBEngineDF;
var
  m: TItemStream;
begin
  Result := nil;
  m := GetData(StorePos, c_DF);
  if m <> nil then
    begin
      m.Position := 0;

      Result := TDBEngineDF.Create;
      try
          Result.DecodeFrom(m, True);
      except
      end;
      Result.DBStorePos := StorePos;
      Result.DBEng := Self;
      with m.Hnd^ do
        begin
          Result.CreateTime := CreateTime;
          Result.LastModifyTime := LastModifyTime;
        end;
    end;
  DisposeObject(m);
end;

function TDBStoreBase.BuildDF(var qState: TQueryState): TDBEngineDF;
begin
  Result := BuildDF(qState.StorePos);
end;

function TDBStoreBase.InsertData(const InsertPos: Int64; buff: THashVariantList): Int64;
var
  m: TMemoryStream64;
begin
  m := TMemoryStream64.Create;
  buff.SaveToStream(m);
  Result := InsertData(InsertPos, m, c_VL);
  DisposeObject(m);
end;

function TDBStoreBase.AddData(buff: THashVariantList): Int64;
var
  m: TMemoryStream64;
begin
  m := TMemoryStream64.Create;
  buff.SaveToStream(m);
  Result := AddData(m, c_VL);
  DisposeObject(m);
end;

function TDBStoreBase.GetVL(const StorePos: Int64): TDBEngineVL;
var
  lastAcc: TCoreClassObject;
  m      : TItemStream;
begin
  lastAcc := FCache[StorePos];
  if lastAcc is TDBEngineVL then
    begin
      Result := lastAcc as TDBEngineVL;
      exit;
    end;

  if lastAcc <> nil then
      FCache.Delete(StorePos);

  Result := nil;

  m := GetData(StorePos, c_VL);
  if m <> nil then
    begin
      m.Position := 0;

      if AllowedCache then
          Result := TDBEngineVL.Create
      else
          Result := FResultVL;

      try
          Result.LoadFromStream(m);
      except
      end;
      Result.DBStorePos := StorePos;
      Result.DBEng := Self;
      with m.Hnd^ do
        begin
          Result.CreateTime := CreateTime;
          Result.LastModifyTime := LastModifyTime;
        end;

      if AllowedCache then
          FCache.Add(StorePos, Result, False);

      DisposeObject(m);
    end;
end;

function TDBStoreBase.GetVL(var qState: TQueryState): TDBEngineVL;
begin
  Result := GetVL(qState.StorePos);
end;

function TDBStoreBase.BuildVL(const StorePos: Int64): TDBEngineVL;
var
  m: TItemStream;
begin
  Result := nil;
  m := GetData(StorePos, c_VL);
  if m <> nil then
    begin
      m.Position := 0;
      Result := TDBEngineVL.Create;
      try
          Result.LoadFromStream(m);
      except
      end;
      Result.DBStorePos := StorePos;
      Result.DBEng := Self;
      with m.Hnd^ do
        begin
          Result.CreateTime := CreateTime;
          Result.LastModifyTime := LastModifyTime;
        end;
    end;
  DisposeObject(m);
end;

function TDBStoreBase.BuildVL(var qState: TQueryState): TDBEngineVL;
begin
  Result := BuildVL(qState.StorePos);
end;

function TDBStoreBase.InsertData(const InsertPos: Int64; buff: TSectionTextData): Int64;
var
  m: TMemoryStream64;
begin
  m := TMemoryStream64.Create;
  buff.SaveToStream(m);
  Result := InsertData(InsertPos, m, c_TE);
  DisposeObject(m);
end;

function TDBStoreBase.AddData(buff: TSectionTextData): Int64;
var
  m: TMemoryStream64;
begin
  m := TMemoryStream64.Create;
  buff.SaveToStream(m);
  Result := AddData(m, c_TE);
  DisposeObject(m);
end;

function TDBStoreBase.GetTE(const StorePos: Int64): TDBEngineTE;
var
  lastAcc: TCoreClassObject;
  m      : TItemStream;
begin
  lastAcc := FCache[StorePos];
  if lastAcc is TDBEngineTE then
    begin
      Result := lastAcc as TDBEngineTE;
      exit;
    end;

  if lastAcc <> nil then
      FCache.Delete(StorePos);

  Result := nil;

  m := GetData(StorePos, c_TE);
  if m <> nil then
    begin
      m.Position := 0;

      if AllowedCache then
          Result := TDBEngineTE.Create
      else
          Result := FResultTE;

      try
          Result.LoadFromStream(m);
      except
      end;
      Result.DBStorePos := StorePos;
      Result.DBEng := Self;
      with m.Hnd^ do
        begin
          Result.CreateTime := CreateTime;
          Result.LastModifyTime := LastModifyTime;
        end;

      if AllowedCache then
          FCache.Add(StorePos, Result, False);

      DisposeObject(m);
    end;
end;

function TDBStoreBase.GetTE(var qState: TQueryState): TDBEngineTE;
begin
  Result := GetTE(qState.StorePos);
end;

function TDBStoreBase.BuildTE(const StorePos: Int64): TDBEngineTE;
var
  m: TItemStream;
begin
  Result := nil;
  m := GetData(StorePos, c_TE);
  if m <> nil then
    begin
      m.Position := 0;
      Result := TDBEngineTE.Create;
      try
          Result.LoadFromStream(m);
      except
      end;
      Result.DBStorePos := StorePos;
      Result.DBEng := Self;
      with m.Hnd^ do
        begin
          Result.CreateTime := CreateTime;
          Result.LastModifyTime := LastModifyTime;
        end;
    end;
  DisposeObject(m);
end;

function TDBStoreBase.BuildTE(var qState: TQueryState): TDBEngineTE;
begin
  Result := BuildTE(qState.StorePos);
end;

{$IFNDEF FPC}


function TDBStoreBase.InsertData(const InsertPos: Int64; buff: TJsonObject): Int64;
var
  m: TMemoryStream64;
begin
  m := TMemoryStream64.Create;
  buff.SaveToStream(m, True, TEncoding.UTF8, True);
  Result := InsertData(InsertPos, m, c_Json);
  DisposeObject(m);
end;

function TDBStoreBase.AddData(buff: TJsonObject): Int64;
var
  m: TMemoryStream64;
begin
  m := TMemoryStream64.Create;
  buff.SaveToStream(m, True, TEncoding.UTF8, True);
  Result := AddData(m, c_Json);
  DisposeObject(m);
end;

function TDBStoreBase.GetJson(const StorePos: Int64): TDBEngineJson;
var
  lastAcc: TCoreClassObject;
  m      : TItemStream;
begin
  lastAcc := FCache[StorePos];
  if lastAcc is TDBEngineJson then
    begin
      Result := lastAcc as TDBEngineJson;
      exit;
    end;

  if lastAcc <> nil then
      FCache.Delete(StorePos);

  Result := nil;

  m := GetData(StorePos, c_Json);
  if m <> nil then
    begin
      m.Position := 0;

      if AllowedCache then
          Result := TDBEngineJson.Create
      else
          Result := FResultJson;

      try
          Result.LoadFromStream(m, TEncoding.UTF8, False);
      except
      end;
      Result.DBStorePos := StorePos;
      Result.DBEng := Self;
      with m.Hnd^ do
        begin
          Result.CreateTime := CreateTime;
          Result.LastModifyTime := LastModifyTime;
        end;
      if AllowedCache then
          FCache.Add(StorePos, Result, False);
      DisposeObject(m);
    end;
end;

function TDBStoreBase.GetJson(var qState: TQueryState): TDBEngineJson;
begin
  Result := GetJson(qState.StorePos);
end;

function TDBStoreBase.BuildJson(const StorePos: Int64): TDBEngineJson;
var
  m: TItemStream;
begin
  Result := nil;
  m := GetData(StorePos, c_Json);
  if m <> nil then
    begin
      m.Position := 0;
      Result := TDBEngineJson.Create;
      try
          Result.LoadFromStream(m, TEncoding.UTF8, False);
      except
      end;
      Result.DBStorePos := StorePos;
      Result.DBEng := Self;
      with m.Hnd^ do
        begin
          Result.CreateTime := CreateTime;
          Result.LastModifyTime := LastModifyTime;
        end;
    end;
  DisposeObject(m);
end;

function TDBStoreBase.BuildJson(var qState: TQueryState): TDBEngineJson;
begin
  Result := BuildJson(qState.StorePos);
end;
{$ENDIF}


function TDBStoreBase.InsertData(const InsertPos: Int64; buff: TDBEnginePascalString): Int64;
var
  m: TMemoryStream64;
begin
  m := TMemoryStream64.Create;
  buff.SaveToStream(m);
  Result := InsertData(InsertPos, m, c_PascalString);
  DisposeObject(m);
end;

function TDBStoreBase.InsertData(const InsertPos: Int64; buff: TPascalString): Int64;
begin
  Result := InsertString(InsertPos, buff);
end;

function TDBStoreBase.InsertString(const InsertPos: Int64; buff: TPascalString): Int64;
var
  t     : TDBEnginePascalString;
  m     : TMemoryStream64;
  itmHnd: TItemHandle;
begin
  t := TDBEnginePascalString.Create;
  t.buff := buff;

  m := TMemoryStream64.Create;
  t.SaveToStream(m);
  m.Position := 0;
  Result := InsertData(InsertPos, m, c_PascalString, itmHnd);
  DisposeObject(m);

  if AllowedCache then
    begin
      t.DBStorePos := Result;
      t.DBEng := Self;
      t.CreateTime := itmHnd.CreateTime;
      t.LastModifyTime := itmHnd.LastModifyTime;
      FCache.Add(Result, t, True);
    end
  else
    begin
      DisposeObject(t);
    end;
end;

function TDBStoreBase.AddData(buff: TDBEnginePascalString): Int64;
var
  m: TMemoryStream64;
begin
  m := TMemoryStream64.Create;
  buff.SaveToStream(m);
  m.Position := 0;
  Result := AddData(m, c_PascalString);
  DisposeObject(m);
end;

function TDBStoreBase.AddData(buff: TPascalString): Int64;
begin
  Result := AddString(buff);
end;

function TDBStoreBase.AddString(buff: TPascalString): Int64;
var
  t     : TDBEnginePascalString;
  m     : TMemoryStream64;
  itmHnd: TItemHandle;
begin
  t := TDBEnginePascalString.Create;
  t.buff := buff;

  m := TMemoryStream64.Create;
  t.SaveToStream(m);
  m.Position := 0;
  Result := AddData(m, c_PascalString, itmHnd);
  DisposeObject(m);

  if AllowedCache then
    begin
      t.DBStorePos := Result;
      t.DBEng := Self;
      t.CreateTime := itmHnd.CreateTime;
      t.LastModifyTime := itmHnd.LastModifyTime;
      FCache.Add(Result, t, True);
    end
  else
    begin
      DisposeObject(t);
    end;
end;

function TDBStoreBase.GetPascalString(const StorePos: Int64): TDBEnginePascalString;
var
  lastAcc: TCoreClassObject;
  m      : TItemStream;
begin
  lastAcc := FCache[StorePos];
  if lastAcc is TDBEnginePascalString then
    begin
      Result := lastAcc as TDBEnginePascalString;
      exit;
    end;

  if lastAcc <> nil then
      FCache.Delete(StorePos);

  Result := nil;

  m := GetData(StorePos, c_PascalString);
  if m <> nil then
    begin
      m.Position := 0;

      if AllowedCache then
          Result := TDBEnginePascalString.Create
      else
          Result := FResultPascalString;

      try
          Result.LoadFromStream(m);
      except
      end;
      Result.DBStorePos := StorePos;
      Result.DBEng := Self;
      with m.Hnd^ do
        begin
          Result.CreateTime := CreateTime;
          Result.LastModifyTime := LastModifyTime;
        end;
      if AllowedCache then
          FCache.Add(StorePos, Result, False);
      DisposeObject(m);
    end;
end;

function TDBStoreBase.GetPascalString(var qState: TQueryState): TDBEnginePascalString;
begin
  Result := GetPascalString(qState.StorePos);
end;

function TDBStoreBase.GetString(const StorePos: Int64): TPascalString;
var
  t: TDBEnginePascalString;
begin
  t := GetPascalString(StorePos);
  if t <> nil then
      Result := t.buff
  else
      Result := '';
end;

function TDBStoreBase.GetString(var qState: TQueryState): TPascalString;
begin
  Result := GetString(qState.StorePos);
end;

procedure TDBStoreBase.SetString(const StorePos: Int64; const value: TPascalString);
var
  t: TDBEnginePascalString;
begin
  t := GetPascalString(StorePos);
  if t <> nil then
    begin
      t.buff := value;
      t.Save;
    end;
end;

function TDBStoreBase.BuildPascalString(const StorePos: Int64): TDBEnginePascalString;
var
  m: TItemStream;
begin
  Result := nil;
  m := GetData(StorePos, c_PascalString);
  if m <> nil then
    begin
      m.Position := 0;
      Result := TDBEnginePascalString.Create;
      try
          Result.LoadFromStream(m);
      except
      end;
      Result.DBStorePos := StorePos;
      Result.DBEng := Self;
      with m.Hnd^ do
        begin
          Result.CreateTime := CreateTime;
          Result.LastModifyTime := LastModifyTime;
        end;
    end;
  DisposeObject(m);
end;

function TDBStoreBase.BuildPascalString(var qState: TQueryState): TDBEnginePascalString;
begin
  Result := BuildPascalString(qState.StorePos);
end;

end.
