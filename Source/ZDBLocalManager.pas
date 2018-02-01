{ ****************************************************************************** }
{ * ZDBLocal , createby qq600585                                               * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
(* https://github.com/PassByYou888/ZServer4D *)
{ ****************************************************************************** }
(*
  update history
*)
unit ZDBLocalManager;

{$I zDefine.inc}

interface

uses SysUtils, Variants,
  CoreClasses, ListEngine, UnicodeMixedLib, DataFrameEngine, MemoryStream64, TextDataEngine,
  {$IFNDEF FPC}
  JsonDataObjects,
  {$ENDIF}
  DoStatusIO, Cadencer, NotifyObjectBase, PascalStrings, CoreCipher, ZDBEngine, ItemStream;

type
  TZDBStoreEngine = class(TDBStoreBase)
  protected
    FName          : SystemString;
    FLastModifyTime: TTimeTickValue;
    procedure DoCreateInit; override;
  public
    property name: SystemString read FName;
  end;

  TZDBLocalManager = class;
  TZDBPipeline     = class;

  TZDBPipelineFilterCall   = procedure(dPipe: TZDBPipeline; var qState: TQueryState; var Allowed: Boolean);
  TZDBPipelineFilterMethod = procedure(dPipe: TZDBPipeline; var qState: TQueryState; var Allowed: Boolean) of object;

  TZDBPipelineDoneCall   = procedure(dPipe: TZDBPipeline);
  TZDBPipelineDoneMethod = procedure(dPipe: TZDBPipeline) of object;

  {$IFNDEF FPC}
  TZDBPipelineFilterProc = reference to procedure(dPipe: TZDBPipeline; var qState: TQueryState; var Allowed: Boolean);
  TZDBPipelineDoneProc   = reference to procedure(dPipe: TZDBPipeline);
  {$ENDIF}

  TZDBPipeline = class(TCoreClassInterfacedObject)
  private
    FQueryCounter            : Int64;
    FCurrentFragmentTime     : TTimeTickValue;
    FFragmentBuffer          : TMemoryStream64;
    FActivted                : Boolean;
    FQueryTask               : TQueryTask;
    FPerformaceCounter       : NativeInt;
    FLastPerformaceTime      : TTimeTickValue;
    FQueryCounterOfPerSec    : Double;
    FRealTimePostFragmentData: Boolean;
    FQueryResultCounter      : Int64;

    procedure Query(var qState: TQueryState);
    procedure QueryDone();

    procedure WriteToOutput(DBEng: TDBStoreBase; StorePos: Int64; ID: Cardinal);
    procedure PostFragmentData(forcePost: Boolean); {$IFDEF INLINE_ASM} inline; {$ENDIF}
  public
    Owner                                   : TZDBLocalManager;
    SourceDB                                : TZDBStoreEngine;
    OutputDB                                : TZDBStoreEngine;
    SourceDBName, OutputDBName, PipelineName: SystemString;

    // query options
    WriteResultToOutputDB : Boolean; // result write to output
    AutoDestroyDB         : Boolean; // complete time destroy DB
    FragmentWaitTime      : Double;  // fragment time,realtime send to client
    MaxWaitTime           : Double;  // max wait complete time,query to abort from out time
    MaxQueryCompare       : Int64;   // max query compare
    MaxQueryResult        : Int64;   // max query result
    QueryDoneFreeDelayTime: Double;  // delay free query pipeline
    WriteFragmentBuffer   : Boolean; // write fragment buffer

    OnDataFilterCall  : TZDBPipelineFilterCall;
    OnDataFilterMethod: TZDBPipelineFilterMethod;

    OnDataDoneCall  : TZDBPipelineDoneCall;
    OnDataDoneMethod: TZDBPipelineDoneMethod;
    {$IFNDEF FPC}
    OnDataFilterProc: TZDBPipelineFilterProc;
    OnDataDoneProc  : TZDBPipelineDoneProc;
    {$ENDIF}
    //
    //
    Values     : THashVariantList;
    DataEng    : TDataFrameEngine;
    UserPointer: Pointer;
    UserObject : TCoreClassObject;
    UserVariant: Variant;
  public
    procedure InitOptions;

    constructor Create(InMem: Boolean; AOwner: TZDBLocalManager; sourDBName, APipelineN, OutDBName: SystemString); virtual;

    destructor Destroy; override;

    procedure Progress(deltaTime: Double); virtual;

    procedure Stop; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure Pause; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure Play; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Paused: Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function QueryConsumTime: Double; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    property Activted: Boolean read FActivted;
    property QueryCounterOfPerSec: Double read FQueryCounterOfPerSec;
    property RealTimePostFragmentData: Boolean read FRealTimePostFragmentData write FRealTimePostFragmentData;
    property QueryCounter: Int64 read FQueryCounter;
    property QueryResultCounter: Int64 read FQueryResultCounter;
  end;

  TZDBPipelineClass = class of TZDBPipeline;

  IZDBLocalManagerNotify = interface
    procedure CreateQuery(pipe: TZDBPipeline);
    procedure QueryFragmentData(pipe: TZDBPipeline; FragmentSource: TMemoryStream64);
    procedure QueryDone(pipe: TZDBPipeline);
    procedure CreateDB(ActiveDB: TZDBStoreEngine);
    procedure CloseDB(ActiveDB: TZDBStoreEngine);
    procedure InsertData(Sender: TZDBStoreEngine; InsertPos: Int64; buff: TCoreClassStream; ID: Cardinal; CompletePos: Int64);
    procedure AddData(Sender: TZDBStoreEngine; buff: TCoreClassStream; ID: Cardinal; CompletePos: Int64);
    procedure ModifyData(Sender: TZDBStoreEngine; const StorePos: Int64; buff: TCoreClassStream);
    procedure DeleteData(Sender: TZDBStoreEngine; const StorePos: Int64);
  end;

  TZDBLocalManager = class(TCoreClassInterfacedObject, IDBStoreBaseNotify, ICadencerProgressInterface)
  protected
    FRootPath         : SystemString;
    FDBPool           : THashObjectList;
    FQueryPipelinePool: THashObjectList;
    FQueryPipelineList: TCoreClassListForObj;
    FTaskCounter      : Cardinal;
    FCadencerEng      : TCadencer;
    FProgressPost     : TNProgressPost;
    FPipelineClass    : TZDBPipelineClass;
    FNotifyIntf       : IZDBLocalManagerNotify;
  protected
    // zdbEngine trigger
    procedure DoInsertData(Sender: TDBStoreBase; InsertPos: Int64; buff: TCoreClassStream; ID: Cardinal; CompletePos: Int64); virtual;
    procedure DoAddData(Sender: TDBStoreBase; buff: TCoreClassStream; ID: Cardinal; CompletePos: Int64); virtual;
    procedure DoModifyData(Sender: TDBStoreBase; const StorePos: Int64; buff: TCoreClassStream); virtual;
    procedure DoDeleteData(Sender: TDBStoreBase; const StorePos: Int64); virtual;
  protected
    // canencer trigger
    procedure ZDBEngProgress(Name: PSystemString; obj: TCoreClassObject);
    procedure CadencerProgress(const deltaTime, newTime: Double);
  protected
    procedure DoQueryFragmentData(pipe: TZDBPipeline; FragmentSour: TMemoryStream64); virtual;
    procedure DoQueryDone(pipe: TZDBPipeline); virtual;
    procedure DelayFreePipe(Sender: TNPostExecute); virtual;

    procedure DoQueryCopy(dPipe: TZDBPipeline; var qState: TQueryState; var Allowed: Boolean);
    procedure DoCompressDone(dPipe: TZDBPipeline);
    procedure DelayReplaceDB(Sender: TNPostExecute);
  public
    constructor Create;
    destructor Destroy; override;

    property PipelineClass: TZDBPipelineClass read FPipelineClass write FPipelineClass;
    property NotifyIntf: IZDBLocalManagerNotify read FNotifyIntf write FNotifyIntf;

    procedure Clear;
    procedure LoadDB(ReadOnly: Boolean);
    procedure SetRootPath(const Value: SystemString);
    property RootPath: SystemString read FRootPath write SetRootPath;

    procedure Progress; virtual;
    property ProgressPost: TNProgressPost read FProgressPost;

    // local operation
    function InitDB(dbN: SystemString): TZDBStoreEngine; overload;
    function InitDB(dbN: SystemString; ReadOnly: Boolean): TZDBStoreEngine; overload;
    function InitNewDB(dbN: SystemString): TZDBStoreEngine;
    function InitMemoryDB(dbN: SystemString): TZDBStoreEngine;
    procedure CloseDB(dbN: SystemString);
    procedure CloseAndDeleteDB(dbN: SystemString);

    // async operation
    procedure CopyDB(SourN, DestN: SystemString);
    procedure CompressDB(dbN: SystemString);
    procedure CompressAllDB;
    procedure ReplaceDB(dbN, replaceN: SystemString);
    procedure ResetData(dbN: SystemString);

    // cleaup all cache
    procedure Recache;

    // container operation
    function GenerateTaskName: SystemString; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function GenerateNewTaskName: SystemString; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function GetPipeline(pipeName: SystemString): TZDBPipeline;
    function GetDB(dN: SystemString): TZDBStoreEngine;
    function GetDBName(dN: SystemString): TZDBStoreEngine;
    property DBName[dN: SystemString]: TZDBStoreEngine read GetDBName; default;
    property PipelineN[pipeName: SystemString]: TZDBPipeline read GetPipeline;
    property QueryPipelineList: TCoreClassListForObj read FQueryPipelineList;
    function ExistsDB(dN: SystemString): Boolean;
    function ExistsPipeline(pipeName: SystemString): Boolean;
    procedure StopPipeline(pipeName: SystemString);
    procedure GetPipeList(OutputList: TCoreClassListForObj);
    procedure GetDBList(OutputList: TCoreClassListForObj);
    function Busy(db: TZDBStoreEngine): Boolean;
    function AllowDestroy(db: TZDBStoreEngine): Boolean;

    // query
    function QueryDB(WriteResultToOutputDB, InMemory, ReverseQuery: Boolean; dbN, OutputDB: SystemString;
      AutoDestroyDB: Boolean; QueryDoneFreeDelayTime, FragmentWaitTime, MaxWaitTime: Double;
      MaxQueryCompare, MaxQueryResult: Int64): TZDBPipeline; overload;

    function QueryDB(WriteResultToOutputDB, InMemory, ReverseQuery: Boolean; dbN, OutputDB: SystemString;
      AutoDestroyDB: Boolean; QueryDoneFreeDelayTime, FragmentWaitTime, MaxWaitTime: Double;
      MaxQueryCompare, MaxQueryResult: Int64;
      OnDataFilterCall: TZDBPipelineFilterCall; OnDataDoneCall: TZDBPipelineDoneCall): TZDBPipeline; overload;

    function QueryDB(WriteResultToOutputDB, InMemory, ReverseQuery: Boolean; dbN, OutputDB: SystemString;
      AutoDestroyDB: Boolean; QueryDoneFreeDelayTime, FragmentWaitTime, MaxWaitTime: Double;
      MaxQueryCompare, MaxQueryResult: Int64;
      OnDataFilterMethod: TZDBPipelineFilterMethod; OnDataDoneMethod: TZDBPipelineDoneMethod): TZDBPipeline; overload;

    {$IFNDEF FPC}
    function QueryDB(WriteResultToOutputDB, InMemory, ReverseQuery: Boolean; dbN, OutputDB: SystemString;
      AutoDestroyDB: Boolean; QueryDoneFreeDelayTime, FragmentWaitTime, MaxWaitTime: Double;
      MaxQueryCompare, MaxQueryResult: Int64;
      OnDataFilterProc: TZDBPipelineFilterProc; OnDataDoneProc: TZDBPipelineDoneProc): TZDBPipeline; overload;
    function QueryDB(DataEng: TDataFrameEngine; UserObj: TCoreClassObject;
      WriteResultToOutputDB, InMemory, ReverseQuery: Boolean; dbN, OutputDB: SystemString;
      AutoDestroyDB: Boolean; QueryDoneFreeDelayTime, FragmentWaitTime, MaxWaitTime: Double;
      MaxQueryCompare, MaxQueryResult: Int64;
      OnDataFilterProc: TZDBPipelineFilterProc; OnDataDoneProc: TZDBPipelineDoneProc): TZDBPipeline; overload;
    {$ENDIF}
    function QueryDBToMemory(WriteResultToOutputDB, ReverseQuery: Boolean; dbN: SystemString;
      QueryDoneFreeDelayTime, FragmentWaitTime, MaxWaitTime: Double;
      MaxQueryCompare, MaxQueryResult: Int64): TZDBPipeline; overload;

    function QueryDBToMemory(WriteResultToOutputDB, ReverseQuery: Boolean; dbN: SystemString;
      FragmentWaitTime, MaxWaitTime: Double; MaxQueryResult: Int64): TZDBPipeline; overload;

    {$IFNDEF FPC}
    function QueryDBToMemory(WriteResultToOutputDB, ReverseQuery: Boolean; dbN: SystemString;
      QueryDoneFreeDelayTime, FragmentWaitTime, MaxWaitTime: Double; MaxQueryCompare, MaxQueryResult: Int64;
      OnDataFilterProc: TZDBPipelineFilterProc; OnDataDoneProc: TZDBPipelineDoneProc): TZDBPipeline; overload;
    {$ENDIF}
    function QueryDBToFile(WriteResultToOutputDB, ReverseQuery: Boolean; dbN, OutputDB: SystemString;
      FragmentWaitTime, MaxWaitTime: Double; MaxQueryCompare, MaxQueryResult: Int64): TZDBPipeline;

    // build single data fragment
    function WriteDBItemToOneFragment(dbN: SystemString; StorePos: Int64; DestStream: TMemoryStream64): Boolean;

    // post operation
    function PostData(dN: SystemString; sourDBEng: TZDBStoreEngine; SourStorePos: Int64): Int64; overload;
    function PostData(dN: SystemString; var qState: TQueryState): Int64; overload;
    function PostData(dN: SystemString; dSour: TCoreClassStream; ID: Cardinal): Int64; overload;
    function PostData(dN: SystemString; dSour: TDataFrameEngine): Int64; overload;
    function PostData(dN: SystemString; dSour: THashVariantList): Int64; overload;
    function PostData(dN: SystemString; dSour: TSectionTextData): Int64; overload;
    function PostData(dN: SystemString; dSour: TPascalString): Int64; overload;
    {$IFNDEF FPC}
    function PostData(dN: SystemString; dSour: TJsonObject): Int64; overload;
    {$ENDIF}
    //
    // insert operation
    function InsertData(dN: SystemString; InsertPos: Int64; dSour: TCoreClassStream; ID: Cardinal): Int64; overload;
    function InsertData(dN: SystemString; InsertPos: Int64; dSour: TDataFrameEngine): Int64; overload;
    function InsertData(dN: SystemString; InsertPos: Int64; dSour: THashVariantList): Int64; overload;
    function InsertData(dN: SystemString; InsertPos: Int64; dSour: TSectionTextData): Int64; overload;
    function InsertData(dN: SystemString; InsertPos: Int64; dSour: TPascalString): Int64; overload;
    {$IFNDEF FPC}
    function InsertData(dN: SystemString; InsertPos: Int64; dSour: TJsonObject): Int64; overload;
    {$ENDIF}
    //
    // delete operation
    procedure DeleteData(dN: SystemString; StorePos: Int64); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    //
    // manual getData
    function GetData(dN: SystemString; StorePos: Int64; ID: Cardinal): TMemoryStream64InCache; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    //
    // modify operation
    function SetData(dN: SystemString; StorePos: Int64; dSour: TMemoryStream64): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
  end;

  TFillQueryDataCall   = procedure(dbN, pipeN: SystemString; StorePos: Int64; ID: Cardinal; DataSour: TMemoryStream64);
  TFillQueryDataMethod = procedure(dbN, pipeN: SystemString; StorePos: Int64; ID: Cardinal; DataSour: TMemoryStream64) of object;
  //
  {$IFNDEF FPC}
  TFillQueryDataProc = reference to procedure(dbN, pipeN: SystemString; StorePos: Int64; ID: Cardinal; DataSour: TMemoryStream64);
  {$ENDIF}
  TUserFillQueryDataCall = procedure(UserPointer: Pointer; UserObject: TCoreClassObject; UserVariant: Variant;
    dbN, pipeN: SystemString; StorePos: Int64; ID: Cardinal; DataSour: TMemoryStream64);
  TUserFillQueryDataMethod = procedure(UserPointer: Pointer; UserObject: TCoreClassObject; UserVariant: Variant;
    dbN, pipeN: SystemString; StorePos: Int64; ID: Cardinal; DataSour: TMemoryStream64) of object;
  {$IFNDEF FPC}
  TUserFillQueryDataProc = reference to procedure(UserPointer: Pointer; UserObject: TCoreClassObject; UserVariant: Variant;
    dbN, pipeN: SystemString; StorePos: Int64; ID: Cardinal; DataSour: TMemoryStream64);
  {$ENDIF}

function GeneratePipeName(const sourDBName, taskName: SystemString): SystemString; {$IFDEF INLINE_ASM} inline; {$ENDIF}

// fill and store
procedure FillFragmentToDB(DataSour: TMemoryStream64; db: TDBStoreBase); {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure FillFragmentSource(dbN, pipeN: SystemString; DataSour: TMemoryStream64; OnResult: TFillQueryDataCall); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure FillFragmentSource(dbN, pipeN: SystemString; DataSour: TMemoryStream64; OnResult: TFillQueryDataMethod); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
{$IFNDEF FPC} procedure FillFragmentSource(dbN, pipeN: SystemString; DataSour: TMemoryStream64; OnResult: TFillQueryDataProc); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}{$ENDIF}

// fill and trigger
procedure FillFragmentSource(UserPointer: Pointer; UserObject: TCoreClassObject; UserVariant: Variant;
  dbN, pipeN: SystemString; DataSour: TMemoryStream64; OnResult: TUserFillQueryDataCall); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure FillFragmentSource(UserPointer: Pointer; UserObject: TCoreClassObject; UserVariant: Variant;
  dbN, pipeN: SystemString; DataSour: TMemoryStream64; OnResult: TUserFillQueryDataMethod); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
{$IFNDEF FPC} procedure FillFragmentSource(UserPointer: Pointer; UserObject: TCoreClassObject; UserVariant: Variant;
  dbN, pipeN: SystemString; DataSour: TMemoryStream64; OnResult: TUserFillQueryDataProc); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}{$ENDIF}
// one fragment operation
function EncodeOneFragment(db: TDBStoreBase; StorePos: Int64; DestStream: TMemoryStream64): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function DecodeOneFragment(DataSour: TMemoryStream64; var dStorePos: Int64; var ID: Cardinal): TMemoryStream64; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function DecodeOneFragment(DataSour: TMemoryStream64): TMemoryStream64; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function DecodeOneNewFragment(DataSour: TMemoryStream64; var dStorePos: Int64; var ID: Cardinal): TMemoryStream64; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function DecodeOneNewFragment(DataSour: TMemoryStream64): TMemoryStream64; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

// encrypt as completeBuffer
function EncodeOneBuff(const dbN: TPascalString; const ID: Cardinal; const StorePos: Int64;
  buff: Pointer; buffSiz: NativeUInt; var outputSiz: NativeUInt): Pointer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure DecodeOneBuff(buff: Pointer; buffSiz: NativeUInt;
  var dbN: TPascalString; var ID: Cardinal; var StorePos: Int64; var output: Pointer; var outputSiz: NativeUInt); {$IFDEF INLINE_ASM} inline; {$ENDIF}


implementation

function GeneratePipeName(const sourDBName, taskName: SystemString): SystemString;
begin
  Result := sourDBName + '.QueryPipe.' + taskName;
end;

procedure FillFragmentToDB(DataSour: TMemoryStream64; db: TDBStoreBase);
var
  StorePos, siz: Int64;
  ID           : Cardinal;
  m64          : TMemoryStream64;
begin
  DataSour.Position := 0;

  m64 := TMemoryStream64.Create;
  while DataSour.Position < DataSour.Size do
    begin
      if DataSour.ReadPtr(@StorePos, umlInt64Length) <> umlCardinalLength then
          break;
      if DataSour.ReadPtr(@siz, umlInt64Length) <> umlCardinalLength then
          break;
      if DataSour.ReadPtr(@ID, umlCardinalLength) <> umlCardinalLength then
          break;

      if DataSour.Position + siz > DataSour.Size then
          break;

      try
        m64.SetPointerWithProtectedMode(DataSour.PositionAsPtr(DataSour.Position), siz);
        db.AddData(m64, ID);
      except
      end;

      DataSour.Position := DataSour.Position + siz;
    end;
  DisposeObject(m64);
end;

procedure FillFragmentSource(dbN, pipeN: SystemString; DataSour: TMemoryStream64; OnResult: TFillQueryDataCall);
var
  StorePos, siz: Int64;
  ID           : Cardinal;
  m64          : TMemoryStream64;
begin
  if not Assigned(OnResult) then
      exit;
  if DataSour.Size <= 0 then
      exit;

  DataSour.Position := 0;

  m64 := TMemoryStream64.Create;
  while DataSour.Position < DataSour.Size do
    begin
      if DataSour.ReadPtr(@StorePos, umlInt64Length) <> umlInt64Length then
          break;
      if DataSour.ReadPtr(@siz, umlInt64Length) <> umlInt64Length then
          break;
      if DataSour.ReadPtr(@ID, umlCardinalLength) <> umlCardinalLength then
          break;

      if DataSour.Position + siz > DataSour.Size then
          break;

      try
        m64.SetPointerWithProtectedMode(DataSour.PositionAsPtr(DataSour.Position), siz);
        OnResult(dbN, pipeN, StorePos, ID, m64);
      except
      end;

      DataSour.Position := DataSour.Position + siz;
    end;
  DisposeObject(m64);
end;

procedure FillFragmentSource(dbN, pipeN: SystemString; DataSour: TMemoryStream64; OnResult: TFillQueryDataMethod);
var
  StorePos, siz: Int64;
  ID           : Cardinal;
  m64          : TMemoryStream64;
begin
  if not Assigned(OnResult) then
      exit;
  if DataSour.Size <= 0 then
      exit;

  DataSour.Position := 0;

  m64 := TMemoryStream64.Create;
  while DataSour.Position < DataSour.Size do
    begin
      if DataSour.ReadPtr(@StorePos, umlInt64Length) <> umlInt64Length then
          break;
      if DataSour.ReadPtr(@siz, umlInt64Length) <> umlInt64Length then
          break;
      if DataSour.ReadPtr(@ID, umlCardinalLength) <> umlCardinalLength then
          break;

      if DataSour.Position + siz > DataSour.Size then
          break;

      try
        m64.SetPointerWithProtectedMode(DataSour.PositionAsPtr(DataSour.Position), siz);
        OnResult(dbN, pipeN, StorePos, ID, m64);
      except
      end;

      DataSour.Position := DataSour.Position + siz;
    end;
  DisposeObject(m64);
end;

{$IFNDEF FPC}


procedure FillFragmentSource(dbN, pipeN: SystemString; DataSour: TMemoryStream64; OnResult: TFillQueryDataProc);
var
  StorePos, siz: Int64;
  ID           : Cardinal;
  m64          : TMemoryStream64;
begin
  if not Assigned(OnResult) then
      exit;
  if DataSour.Size <= 0 then
      exit;

  DataSour.Position := 0;

  m64 := TMemoryStream64.Create;
  while DataSour.Position < DataSour.Size do
    begin
      if DataSour.ReadPtr(@StorePos, umlInt64Length) <> umlInt64Length then
          break;
      if DataSour.ReadPtr(@siz, umlInt64Length) <> umlInt64Length then
          break;
      if DataSour.ReadPtr(@ID, umlCardinalLength) <> umlCardinalLength then
          break;

      if DataSour.Position + siz > DataSour.Size then
          break;

      try
        m64.SetPointerWithProtectedMode(DataSour.PositionAsPtr(DataSour.Position), siz);
        OnResult(dbN, pipeN, StorePos, ID, m64);
      except
      end;

      DataSour.Position := DataSour.Position + siz;
    end;
  DisposeObject(m64);
end;
{$ENDIF}


procedure FillFragmentSource(UserPointer: Pointer; UserObject: TCoreClassObject; UserVariant: Variant;
  dbN, pipeN: SystemString; DataSour: TMemoryStream64; OnResult: TUserFillQueryDataCall);
var
  StorePos, siz: Int64;
  ID           : Cardinal;
  m64          : TMemoryStream64;
begin
  if not Assigned(OnResult) then
      exit;
  if DataSour.Size <= 0 then
      exit;

  DataSour.Position := 0;

  m64 := TMemoryStream64.Create;
  while DataSour.Position < DataSour.Size do
    begin
      if DataSour.ReadPtr(@StorePos, umlInt64Length) <> umlInt64Length then
          break;
      if DataSour.ReadPtr(@siz, umlInt64Length) <> umlInt64Length then
          break;
      if DataSour.ReadPtr(@ID, umlCardinalLength) <> umlCardinalLength then
          break;

      if DataSour.Position + siz > DataSour.Size then
          break;

      try
        m64.SetPointerWithProtectedMode(DataSour.PositionAsPtr(DataSour.Position), siz);
        OnResult(UserPointer, UserObject, UserVariant, dbN, pipeN, StorePos, ID, m64);
      except
      end;

      DataSour.Position := DataSour.Position + siz;
    end;
  DisposeObject(m64);
end;

procedure FillFragmentSource(UserPointer: Pointer; UserObject: TCoreClassObject; UserVariant: Variant;
  dbN, pipeN: SystemString; DataSour: TMemoryStream64; OnResult: TUserFillQueryDataMethod);
var
  StorePos, siz: Int64;
  ID           : Cardinal;
  m64          : TMemoryStream64;
begin
  if not Assigned(OnResult) then
      exit;
  if DataSour.Size <= 0 then
      exit;

  DataSour.Position := 0;

  m64 := TMemoryStream64.Create;
  while DataSour.Position < DataSour.Size do
    begin
      if DataSour.ReadPtr(@StorePos, umlInt64Length) <> umlInt64Length then
          break;
      if DataSour.ReadPtr(@siz, umlInt64Length) <> umlInt64Length then
          break;
      if DataSour.ReadPtr(@ID, umlCardinalLength) <> umlCardinalLength then
          break;

      if DataSour.Position + siz > DataSour.Size then
          break;

      try
        m64.SetPointerWithProtectedMode(DataSour.PositionAsPtr(DataSour.Position), siz);
        OnResult(UserPointer, UserObject, UserVariant, dbN, pipeN, StorePos, ID, m64);
      except
      end;

      DataSour.Position := DataSour.Position + siz;
    end;
  DisposeObject(m64);
end;

{$IFNDEF FPC}


procedure FillFragmentSource(UserPointer: Pointer; UserObject: TCoreClassObject; UserVariant: Variant;
  dbN, pipeN: SystemString; DataSour: TMemoryStream64; OnResult: TUserFillQueryDataProc);
var
  StorePos, siz: Int64;
  ID           : Cardinal;
  m64          : TMemoryStream64;
begin
  if not Assigned(OnResult) then
      exit;
  if DataSour.Size <= 0 then
      exit;

  DataSour.Position := 0;

  m64 := TMemoryStream64.Create;
  while DataSour.Position < DataSour.Size do
    begin
      if DataSour.ReadPtr(@StorePos, umlInt64Length) <> umlInt64Length then
          break;
      if DataSour.ReadPtr(@siz, umlInt64Length) <> umlInt64Length then
          break;
      if DataSour.ReadPtr(@ID, umlCardinalLength) <> umlCardinalLength then
          break;

      if DataSour.Position + siz > DataSour.Size then
          break;

      try
        m64.SetPointerWithProtectedMode(DataSour.PositionAsPtr(DataSour.Position), siz);
        OnResult(UserPointer, UserObject, UserVariant, dbN, pipeN, StorePos, ID, m64);
      except
      end;

      DataSour.Position := DataSour.Position + siz;
    end;
  DisposeObject(m64);
end;
{$ENDIF}


function EncodeOneFragment(db: TDBStoreBase; StorePos: Int64; DestStream: TMemoryStream64): Boolean;
var
  itmStream: TMemoryStream64InCache;
  siz      : Int64;
  ID       : Cardinal;
begin
  Result := False;
  itmStream := db.GetCacheStream(StorePos);
  if itmStream <> nil then
    begin
      siz := itmStream.Size;
      ID := itmStream.CacheID;
      DestStream.Position := DestStream.Size;
      DestStream.WritePtr(@StorePos, umlInt64Length);
      DestStream.WritePtr(@siz, umlInt64Length);
      DestStream.WritePtr(@ID, umlCardinalLength);
      DestStream.CopyFrom(itmStream, siz);

      DisposeObject(itmStream);
      Result := True;
    end;
end;

function DecodeOneFragment(DataSour: TMemoryStream64; var dStorePos: Int64; var ID: Cardinal): TMemoryStream64;
var
  siz: Int64;
begin
  Result := nil;
  if DataSour.ReadPtr(@dStorePos, umlInt64Length) <> umlInt64Length then
      exit;
  if DataSour.ReadPtr(@siz, umlInt64Length) <> umlInt64Length then
      exit;
  if DataSour.ReadPtr(@ID, umlCardinalLength) <> umlCardinalLength then
      exit;

  if DataSour.Position + siz > DataSour.Size then
      exit;

  Result := TMemoryStream64.Create;
  Result.SetPointerWithProtectedMode(DataSour.PositionAsPtr(DataSour.Position), siz);
end;

function DecodeOneFragment(DataSour: TMemoryStream64): TMemoryStream64;
var
  dStorePos: Int64;
  ID       : Cardinal;
begin
  Result := DecodeOneFragment(DataSour, dStorePos, ID);
end;

function DecodeOneNewFragment(DataSour: TMemoryStream64; var dStorePos: Int64; var ID: Cardinal): TMemoryStream64;
var
  siz: Int64;
begin
  Result := nil;
  if DataSour.ReadPtr(@dStorePos, umlInt64Length) <> umlInt64Length then
      exit;
  if DataSour.ReadPtr(@siz, umlInt64Length) <> umlInt64Length then
      exit;
  if DataSour.ReadPtr(@ID, umlCardinalLength) <> umlCardinalLength then
      exit;

  if DataSour.Position + siz > DataSour.Size then
      exit;

  Result := TMemoryStream64.Create;
  Result.CopyFrom(DataSour, siz);
  Result.Position := 0;
end;

function DecodeOneNewFragment(DataSour: TMemoryStream64): TMemoryStream64;
var
  dStorePos: Int64;
  ID       : Cardinal;
begin
  Result := DecodeOneNewFragment(DataSour, dStorePos, ID);
end;

function EncodeOneBuff(const dbN: TPascalString; const ID: Cardinal; const StorePos: Int64;
  buff: Pointer; buffSiz: NativeUInt; var outputSiz: NativeUInt): Pointer;
var
  nb: TBytes;
  l : word;
  p : PByteArray;
begin
  dbN.FastGetBytes(nb);
  l := length(nb);
  outputSiz := 2 + l + 4 + 8 + buffSiz;
  p := GetMemory(outputSiz);
  Result := p;
  PWord(@p^[0])^ := l;
  copyPtr(@nb[0], @p^[2], l);
  PCardinal(@(p^[2 + l]))^ := ID;
  PInt64(@(p^[2 + l + 4]))^ := StorePos;
  copyPtr(buff, @p^[2 + l + 4 + 8], buffSiz);
end;

procedure DecodeOneBuff(buff: Pointer; buffSiz: NativeUInt;
  var dbN: TPascalString; var ID: Cardinal; var StorePos: Int64; var output: Pointer; var outputSiz: NativeUInt);
var
  nb: TBytes;
  p : PByteArray;
begin
  p := buff;
  setLength(nb, PWord(@p^[0])^);
  copyPtr(@p^[2], @nb[0], PWord(@p^[0])^);
  dbN.Bytes := nb;
  ID := PCardinal(@(p^[2 + PWord(@p^[0])^]))^;
  StorePos := PInt64(@(p^[2 + PWord(@p^[0])^ + 4]))^;
  outputSiz := buffSiz - (2 + PWord(@p^[0])^ + 4 + 8);
  output := @p^[2 + PWord(@p^[0])^ + 4 + 8];
end;

procedure TZDBStoreEngine.DoCreateInit;
begin
  inherited DoCreateInit;
  FName := '';
  FLastModifyTime := GetTimeTick;
end;

procedure TZDBPipeline.Query(var qState: TQueryState);
var
  lastTime   : TTimeTickValue;
  AlreadWrite: Boolean;
  Allowed    : Boolean;

  procedure DoWrite;
  begin
    if AlreadWrite then
        exit;

    WriteToOutput(qState.DBEng, qState.StorePos, qState.ID);
    AlreadWrite := True;
    inc(FQueryResultCounter);
  end;

begin
  lastTime := GetTimeTick;
  inc(FPerformaceCounter);

  FActivted := True;

  AlreadWrite := False;

  Allowed := False;

  if OutputDB = nil then
      exit;
  if SourceDB = nil then
      exit;

  try
    if Assigned(OnDataFilterCall) then
        OnDataFilterCall(Self, qState, Allowed);

    if Allowed then
        DoWrite;
  except
  end;

  Allowed := False;
  try
    if Assigned(OnDataFilterMethod) then
        OnDataFilterMethod(Self, qState, Allowed);

    if Allowed then
        DoWrite;
  except
  end;

  {$IFNDEF FPC}
  Allowed := False;
  try
    if Assigned(OnDataFilterProc) then
        OnDataFilterProc(Self, qState, Allowed);

    if Allowed then
        DoWrite;
  except
  end;
  {$ENDIF}
  inc(FQueryCounter);

  FCurrentFragmentTime := FCurrentFragmentTime + qState.deltaTime;
  if (AlreadWrite) and (FCurrentFragmentTime >= Trunc(FragmentWaitTime * 1000)) then
    begin
      PostFragmentData(False);
      FCurrentFragmentTime := 0;
    end;

  if (MaxQueryResult > 0) and (FQueryResultCounter >= MaxQueryResult) then
    begin
      qState.Aborted := True;
      exit;
    end;
  if (MaxQueryCompare > 0) and (FQueryCounter >= MaxQueryCompare) then
    begin
      qState.Aborted := True;
      exit;
    end;
  if (MaxWaitTime > 0) and (qState.newTime >= Trunc(MaxWaitTime * 1000)) then
    begin
      qState.Aborted := True;
      exit;
    end;

  if lastTime - FLastPerformaceTime > 1000 then
    begin
      try
        if FPerformaceCounter > 0 then
            FQueryCounterOfPerSec := FPerformaceCounter / ((lastTime - FLastPerformaceTime) * 0.001)
        else
            FQueryCounterOfPerSec := 0;
      except
          FQueryCounterOfPerSec := 0;
      end;
      FLastPerformaceTime := lastTime;
      FPerformaceCounter := 0;
    end;
end;

procedure TZDBPipeline.QueryDone();
begin
  PostFragmentData(True);

  try
    if Assigned(OnDataDoneCall) then
        OnDataDoneCall(Self);
  except
  end;

  try
    if Assigned(OnDataDoneMethod) then
        OnDataDoneMethod(Self);
  except
  end;

  {$IFNDEF FPC}
  try
    if Assigned(OnDataDoneProc) then
        OnDataDoneProc(Self);
  except
  end;
  {$ENDIF}
  //
  try
      Owner.DoQueryDone(Self);
  except
  end;

  FActivted := False;
  FQueryTask := nil;

  FPerformaceCounter := 0;
  FLastPerformaceTime := GetTimeTick;
end;

procedure TZDBPipeline.WriteToOutput(DBEng: TDBStoreBase; StorePos: Int64; ID: Cardinal);
var
  itmStream: TMemoryStream64;
  siz      : Int64;
begin
  if (not WriteResultToOutputDB) and (not WriteFragmentBuffer) then
      exit;

  itmStream := DBEng.GetCacheStream(StorePos, ID);

  if WriteResultToOutputDB then
    begin
      OutputDB.AddData(itmStream, ID);
    end;

  if WriteFragmentBuffer then
    begin
      itmStream.Position := 0;
      siz := itmStream.Size;
      FFragmentBuffer.Position := FFragmentBuffer.Size;
      FFragmentBuffer.WritePtr(@StorePos, umlInt64Length);
      FFragmentBuffer.WritePtr(@siz, umlInt64Length);
      FFragmentBuffer.WritePtr(@ID, umlCardinalLength);
      FFragmentBuffer.CopyFrom(itmStream, siz);
    end;
end;

procedure TZDBPipeline.PostFragmentData(forcePost: Boolean);
begin
  if (not forcePost) and (not FRealTimePostFragmentData) then
      exit;
  if FFragmentBuffer.Size <= 0 then
      exit;

  FFragmentBuffer.Position := 0;
  Owner.DoQueryFragmentData(Self, FFragmentBuffer);
  FFragmentBuffer.Clear;
end;

procedure TZDBPipeline.InitOptions;
begin
  FQueryCounter := 0;
  FCurrentFragmentTime := 0;
  FFragmentBuffer := TMemoryStream64.Create;

  FActivted := True;
  FQueryTask := nil;
  FPerformaceCounter := 0;
  FLastPerformaceTime := GetTimeTick;
  FQueryCounterOfPerSec := 0;
  FRealTimePostFragmentData := True;
  FQueryResultCounter := 0;

  // data query options
  WriteResultToOutputDB := True; // query result write to output
  AutoDestroyDB := True;         // complete time destroy DB
  FragmentWaitTime := 0.5;       // fragment time,realtime send to client
  MaxWaitTime := 0;              // max wait complete time,query to abort from out time
  MaxQueryCompare := 0;          // max query compare
  MaxQueryResult := 0;           // max query result
  QueryDoneFreeDelayTime := 60;  // query done free delay time
  WriteFragmentBuffer := True;   // write fragment

  OnDataFilterCall := nil;
  OnDataFilterMethod := nil;
  {$IFNDEF FPC}
  OnDataFilterProc := nil;
  {$ENDIF}
  OnDataDoneCall := nil;
  OnDataDoneMethod := nil;
  {$IFNDEF FPC}
  OnDataDoneProc := nil;
  {$ENDIF}
  Values := THashVariantList.Create;
  DataEng := TDataFrameEngine.Create;
  UserPointer := nil;
  UserObject := nil;
  UserVariant := NULL;

  Owner.FQueryPipelinePool[PipelineName] := Self;
  Owner.FQueryPipelineList.Add(Self);
end;

constructor TZDBPipeline.Create(InMem: Boolean; AOwner: TZDBLocalManager; sourDBName, APipelineN, OutDBName: SystemString);
begin
  inherited Create;
  Owner := AOwner;

  SourceDB := Owner.FDBPool[sourDBName] as TZDBStoreEngine;

  PipelineName := APipelineN;
  SourceDBName := sourDBName;
  OutputDBName := OutDBName;

  if InMem then
      OutputDB := Owner.InitMemoryDB(OutDBName)
  else
      OutputDB := Owner.InitDB(OutDBName, False);

  InitOptions;
end;

destructor TZDBPipeline.Destroy;
var
  fn: SystemString;
  i : Integer;
  pl: TZDBPipeline;
begin
  i := 0;
  while i < Owner.FQueryPipelineList.Count do
    begin
      if Owner.FQueryPipelineList[i] = Self then
          Owner.FQueryPipelineList.Delete(i)
      else
          inc(i);
    end;

  Owner.FQueryPipelinePool.Delete(PipelineName);

  try
    if (OutputDB <> nil) and (AutoDestroyDB) then
      begin
        for i := 0 to Owner.FQueryPipelineList.Count - 1 do
          begin
            pl := TZDBPipeline(Owner.FQueryPipelineList[i]);
            if pl.OutputDB = OutputDB then
                pl.OutputDB := nil;
            if pl.SourceDB = OutputDB then
                pl.SourceDB := nil;
          end;

        if OutputDB.IsMemoryMode then
          begin
            Owner.CloseDB(OutputDB.Name);
          end
        else
          begin
            Owner.CloseAndDeleteDB(OutputDB.Name);
          end;
      end;
  except
  end;

  DisposeObject([FFragmentBuffer, Values, DataEng]);

  inherited Destroy;
end;

procedure TZDBPipeline.Progress(deltaTime: Double);
begin
end;

procedure TZDBPipeline.Stop;
begin
  if FQueryTask <> nil then
      FQueryTask.Stop;
end;

procedure TZDBPipeline.Pause;
begin
  if (FragmentWaitTime > 0) then
      PostFragmentData(True);
  if FQueryTask <> nil then
      FQueryTask.Pause;
end;

procedure TZDBPipeline.Play;
begin
  if FQueryTask <> nil then
      FQueryTask.Play;
end;

function TZDBPipeline.Paused: Boolean;
begin
  if FQueryTask <> nil then
      Result := FQueryTask.Paused
  else
      Result := False;
end;

function TZDBPipeline.QueryConsumTime: Double;
begin
  if FQueryTask <> nil then
      Result := FQueryTask.ConsumTime
  else
      Result := 0;
end;

procedure TZDBLocalManager.DoInsertData(Sender: TDBStoreBase; InsertPos: Int64; buff: TCoreClassStream; ID: Cardinal; CompletePos: Int64);
begin
  TZDBStoreEngine(Sender).FLastModifyTime := GetTimeTick;
  try
    if Assigned(FNotifyIntf) then
        FNotifyIntf.InsertData(TZDBStoreEngine(Sender), InsertPos, buff, ID, CompletePos);
  except
  end;
end;

procedure TZDBLocalManager.DoAddData(Sender: TDBStoreBase; buff: TCoreClassStream; ID: Cardinal; CompletePos: Int64);
begin
  TZDBStoreEngine(Sender).FLastModifyTime := GetTimeTick;
  try
    if Assigned(FNotifyIntf) then
        FNotifyIntf.AddData(TZDBStoreEngine(Sender), buff, ID, CompletePos);
  except
  end;
end;

procedure TZDBLocalManager.DoModifyData(Sender: TDBStoreBase; const StorePos: Int64; buff: TCoreClassStream);
begin
  TZDBStoreEngine(Sender).FLastModifyTime := GetTimeTick;
  try
    if Assigned(FNotifyIntf) then
        FNotifyIntf.ModifyData(TZDBStoreEngine(Sender), StorePos, buff);
  except
  end;
end;

procedure TZDBLocalManager.DoDeleteData(Sender: TDBStoreBase; const StorePos: Int64);
begin
  TZDBStoreEngine(Sender).FLastModifyTime := GetTimeTick;
  try
    if Assigned(FNotifyIntf) then
        FNotifyIntf.DeleteData(TZDBStoreEngine(Sender), StorePos);
  except
  end;
end;

procedure TZDBLocalManager.DoQueryFragmentData(pipe: TZDBPipeline; FragmentSour: TMemoryStream64);
begin
  if not Assigned(FNotifyIntf) then
      exit;

  FragmentSour.Position := 0;

  try
      FNotifyIntf.QueryFragmentData(pipe, FragmentSour);
  except
  end;
end;

procedure TZDBLocalManager.DoQueryDone(pipe: TZDBPipeline);
begin
  try
    if Assigned(FNotifyIntf) then
        FNotifyIntf.QueryDone(pipe);
  except
  end;

  {$IFDEF FPC}
  with ProgressPost.PostExecute(pipe.QueryDoneFreeDelayTime, @DelayFreePipe) do
  {$ELSE}
  with ProgressPost.PostExecute(pipe.QueryDoneFreeDelayTime, DelayFreePipe) do
    {$ENDIF}
      Data1 := pipe;
end;

procedure TZDBLocalManager.DelayFreePipe(Sender: TNPostExecute);
var
  i       : Integer;
  sour, pl: TZDBPipeline;
begin
  sour := TZDBPipeline(Sender.Data1);

  if sour.AutoDestroyDB then
    for i := 0 to FQueryPipelineList.Count - 1 do
      begin
        pl := TZDBPipeline(FQueryPipelineList[i]);
        if (pl.SourceDB = sour.OutputDB) and (pl.Activted) then
          begin
            {$IFDEF FPC}
            with ProgressPost.PostExecute(1.0, @DelayFreePipe) do
            {$ELSE}
            with ProgressPost.PostExecute(1.0, DelayFreePipe) do
              {$ENDIF}
                Data1 := sour;
            exit;
          end;
      end;

  DisposeObject(sour);
end;

procedure TZDBLocalManager.DoQueryCopy(dPipe: TZDBPipeline; var qState: TQueryState; var Allowed: Boolean);
begin
  PostData(dPipe.UserVariant, qState);
  Allowed := False;
end;

procedure TZDBLocalManager.DoCompressDone(dPipe: TZDBPipeline);
var
  SourN   : SystemString;
  replaceN: SystemString;
begin
  SourN := dPipe.SourceDB.Name;
  replaceN := dPipe.UserVariant;
  {$IFDEF FPC}
  with ProgressPost.PostExecute(2.0, @DelayReplaceDB) do
  {$ELSE}
  with ProgressPost.PostExecute(2.0, DelayReplaceDB) do
    {$ENDIF}
    begin
      Data3 := SourN;
      Data4 := replaceN;
    end;
end;

procedure TZDBLocalManager.DelayReplaceDB(Sender: TNPostExecute);
var
  SourN   : SystemString;
  replaceN: SystemString;
  sourDB  : TZDBStoreEngine;
  pl      : TZDBPipeline;
  i       : Integer;
  dbBusy  : Boolean;
begin
  SourN := Sender.Data3;
  replaceN := Sender.Data4;

  if not ExistsDB(SourN) then
      exit;
  if not ExistsDB(replaceN) then
      exit;

  sourDB := DBName[SourN];

  dbBusy := sourDB.QueryProcessing;

  if not dbBusy then
    for i := 0 to FQueryPipelineList.Count - 1 do
      if TZDBPipeline(FQueryPipelineList[i]).SourceDB = sourDB then
        begin
          dbBusy := True;
          break;
        end;

  if dbBusy then
    begin
      {$IFDEF FPC}
      with ProgressPost.PostExecute(1.0, @DelayReplaceDB) do
      {$ELSE}
      with ProgressPost.PostExecute(1.0, DelayReplaceDB) do
        {$ENDIF}
        begin
          Data3 := SourN;
          Data4 := replaceN;
        end;
      exit;
    end;
  CloseAndDeleteDB(SourN);

  if DBName[replaceN].RenameDB(SourN + '.OX') then
    begin
      CloseDB(replaceN);
      InitDB(SourN, False);
    end;
end;

constructor TZDBLocalManager.Create;
begin
  inherited Create;
  FRootPath := umlCurrentPath;
  FDBPool := THashObjectList.Create(True, 1024);
  FDBPool.AccessOptimization := False;

  FQueryPipelinePool := THashObjectList.Create(False, 1024);
  FQueryPipelinePool.AccessOptimization := False;

  FQueryPipelineList := TCoreClassListForObj.Create;

  FTaskCounter := 1;
  FCadencerEng := TCadencer.Create;
  FCadencerEng.ProgressIntf := Self;
  FProgressPost := TNProgressPost.Create;
  FPipelineClass := TZDBPipeline;
  FNotifyIntf := nil;
end;

destructor TZDBLocalManager.Destroy;
var
  lst: TCoreClassListForObj;
  i  : Integer;
begin
  FProgressPost.ResetPost;

  lst := TCoreClassListForObj.Create;
  FDBPool.GetAsList(lst);
  for i := 0 to lst.Count - 1 do
      TZDBStoreEngine(lst[i]).StopAllQuery;
  DisposeObject(lst);

  lst := TCoreClassListForObj.Create;
  FQueryPipelinePool.GetAsList(lst);
  for i := 0 to lst.Count - 1 do
      DisposeObject(lst[i]);
  DisposeObject(lst);

  DisposeObject([FDBPool, FQueryPipelinePool, FQueryPipelineList, FCadencerEng, FProgressPost]);
  inherited Destroy;
end;

function TZDBLocalManager.InitDB(dbN: SystemString): TZDBStoreEngine;
begin
  Result := InitDB(dbN, False);
end;

function TZDBLocalManager.InitDB(dbN: SystemString; ReadOnly: Boolean): TZDBStoreEngine;
var
  fn     : umlString;
  isNewDB: Boolean;
begin
  Result := GetDB(dbN);
  if Result <> nil then
      exit;

  if not umlString(dbN).Exists(['/', '\']) then
      fn := umlCombineFileName(FRootPath, dbN + '.OX')
  else
    begin
      fn.Text := dbN;
      dbN := umlChangeFileExt(umlGetFileName(dbN), '');
    end;

  isNewDB := not umlFileExists(fn);

  if isNewDB then
      Result := TZDBStoreEngine.CreateNew(fn)
  else
      Result := TZDBStoreEngine.Create(fn, readonly);

  Result.NotifyIntf := Self;
  Result.FName := dbN;

  FDBPool[dbN] := Result;

  try
    if (isNewDB) and (Assigned(FNotifyIntf)) then
        FNotifyIntf.CreateDB(Result);
  except
  end;
end;

function TZDBLocalManager.InitNewDB(dbN: SystemString): TZDBStoreEngine;
var
  fn: umlString;
begin
  if not umlString(dbN).Exists(['/', '\']) then
      fn := umlCombineFileName(FRootPath, dbN + '.OX')
  else
    begin
      fn := dbN;
      dbN := umlChangeFileExt(umlGetFileName(dbN), '');
    end;

  FDBPool.Delete(dbN);

  Result := TZDBStoreEngine.CreateNew(fn);

  Result.NotifyIntf := Self;
  Result.FName := dbN;

  FDBPool[dbN] := Result;

  try
    if Assigned(FNotifyIntf) then
        FNotifyIntf.CreateDB(Result);
  except
  end;
end;

function TZDBLocalManager.InitMemoryDB(dbN: SystemString): TZDBStoreEngine;
begin
  Result := GetDB(dbN);
  if Result <> nil then
      exit;
  Result := TZDBStoreEngine.CreateNewMemory;

  Result.NotifyIntf := Self;
  Result.FName := dbN;

  FDBPool[dbN] := Result;

  try
    if Assigned(FNotifyIntf) then
        FNotifyIntf.CreateDB(Result);
  except
  end;
end;

procedure TZDBLocalManager.ZDBEngProgress(Name: PSystemString; obj: TCoreClassObject);
var
  db: TZDBStoreEngine;
begin
  if obj = nil then
      exit;

  db := TZDBStoreEngine(obj);
  if (db.DBEngine.Modify) and (GetTimeTick - db.FLastModifyTime > 1000) then
    begin
      db.DBEngine.Update;
      db.FLastModifyTime := GetTimeTick;
    end;
end;

procedure TZDBLocalManager.CadencerProgress(const deltaTime, newTime: Double);
var
  i: Integer;
begin
  FProgressPost.Progress(deltaTime);

  for i := 0 to FQueryPipelineList.Count - 1 do
    begin
      try
          TZDBPipeline(FQueryPipelineList[i]).Progress(deltaTime);
      except
      end;
    end;

  {$IFDEF FPC}
  FDBPool.Progress(@ZDBEngProgress);
  {$ELSE}
  FDBPool.Progress(ZDBEngProgress);
  {$ENDIF}
end;

procedure TZDBLocalManager.CloseDB(dbN: SystemString);
var
  db: TZDBStoreEngine;
  i : Integer;
  pl: TZDBPipeline;
begin
  db := GetDB(dbN);
  if db = nil then
      exit;

  for i := 0 to FQueryPipelineList.Count - 1 do
    begin
      pl := TZDBPipeline(FQueryPipelineList[i]);
      if pl.OutputDB = db then
          pl.OutputDB := nil;
      if pl.SourceDB = db then
          pl.SourceDB := nil;
    end;

  try
    if Assigned(FNotifyIntf) then
        FNotifyIntf.CloseDB(db);
  except
  end;

  FDBPool.Delete(dbN);
end;

procedure TZDBLocalManager.CloseAndDeleteDB(dbN: SystemString);
var
  db: TZDBStoreEngine;
  fn: SystemString;
begin
  db := GetDB(dbN);
  if db = nil then
      exit;

  if db.DBEngine.StreamEngine is TMemoryStream64 then
    begin
      CloseDB(db.Name);
    end
  else
    begin
      fn := db.DBEngine.ObjectName;
      CloseDB(db.Name);
      if umlFileExists(fn) then
          umlDeleteFile(fn);
    end;
end;

procedure TZDBLocalManager.CopyDB(SourN, DestN: SystemString);
var
  n : SystemString;
  pl: TZDBPipeline;
  db: TZDBStoreEngine;
  nd: TZDBStoreEngine;
begin
  db := GetDB(SourN);
  if db = nil then
      exit;

  if db.IsReadOnly then
      exit;

  n := DestN;

  if db.IsMemoryMode then
      nd := InitMemoryDB(n)
  else
      nd := InitDB(n, False);

  pl := QueryDB(False, True, False, db.Name, 'Copying', True, 0.0, 0, 0, 0, 0);
  {$IFDEF FPC}
  pl.OnDataFilterMethod := @DoQueryCopy;
  {$ELSE}
  pl.OnDataFilterMethod := DoQueryCopy;
  {$ENDIF}
  pl.UserVariant := nd.Name;
end;

procedure TZDBLocalManager.CompressDB(dbN: SystemString);
var
  n : SystemString;
  pl: TZDBPipeline;
  db: TZDBStoreEngine;
  nd: TZDBStoreEngine;
begin
  db := GetDB(dbN);
  if db = nil then
      exit;

  if db.IsReadOnly then
      exit;

  if ExistsPipeline(db.Name + '.*.Compressing') then
      exit;

  n := db.Name + '.CompressSwap';

  if ExistsDB(n) then
      exit;

  if db.IsMemoryMode then
      nd := InitMemoryDB(n)
  else
      nd := InitNewDB(n);

  pl := QueryDB(False, True, False, db.Name, n, False, 0.0, 0, 0, 0, 0);
  {$IFDEF FPC}
  pl.OnDataFilterMethod := @DoQueryCopy;
  pl.OnDataDoneMethod := @DoCompressDone;
  {$ELSE}
  pl.OnDataFilterMethod := DoQueryCopy;
  pl.OnDataDoneMethod := DoCompressDone;
  {$ENDIF}
  pl.UserVariant := nd.Name;
end;

procedure TZDBLocalManager.CompressAllDB;
var
  lst: TCoreClassListForObj;
  i  : Integer;
begin
  lst := TCoreClassListForObj.Create;
  FDBPool.GetAsList(lst);
  for i := 0 to lst.Count - 1 do
      CompressDB(TZDBStoreEngine(lst[i]).Name);
  DisposeObject(lst);
end;

procedure TZDBLocalManager.ReplaceDB(dbN, replaceN: SystemString);
begin
  {$IFDEF FPC}
  with ProgressPost.PostExecute(0, @DelayReplaceDB) do
  {$ELSE}
  with ProgressPost.PostExecute(0, DelayReplaceDB) do
    {$ENDIF}
    begin
      Data3 := dbN;
      Data4 := replaceN;
    end;
end;

procedure TZDBLocalManager.ResetData(dbN: SystemString);
var
  db: TZDBStoreEngine;
begin
  db := GetDB(dbN);
  if db = nil then
      exit;

  if db.IsReadOnly then
      exit;

  db.ResetDB;
end;

procedure TZDBLocalManager.Recache;
var
  lst: TCoreClassListForObj;
  i  : Integer;
  db : TZDBStoreEngine;
begin
  lst := TCoreClassListForObj.Create;
  FDBPool.GetAsList(lst);
  for i := 0 to lst.Count - 1 do
    begin
      db := TZDBStoreEngine(lst[i]);
      db.Recache;
    end;
  DisposeObject(lst);
end;

function TZDBLocalManager.GenerateTaskName: SystemString;
begin
  Result := 'Task' + umlIntToStr(FTaskCounter);
end;

function TZDBLocalManager.GenerateNewTaskName: SystemString;
begin
  Result := GenerateTaskName;
  inc(FTaskCounter);
end;

function TZDBLocalManager.GetPipeline(pipeName: SystemString): TZDBPipeline;
begin
  Result := TZDBPipeline(FQueryPipelinePool[pipeName]);
end;

function TZDBLocalManager.GetDB(dN: SystemString): TZDBStoreEngine;
begin
  Result := TZDBStoreEngine(FDBPool[dN]);
end;

function TZDBLocalManager.GetDBName(dN: SystemString): TZDBStoreEngine;
begin
  Result := InitMemoryDB(dN);
end;

function TZDBLocalManager.ExistsDB(dN: SystemString): Boolean;
begin
  Result := FDBPool.Exists(dN);
end;

function TZDBLocalManager.ExistsPipeline(pipeName: SystemString): Boolean;
var
  i: Integer;
begin
  Result := FQueryPipelinePool.Exists(pipeName);
  if Result then
      exit;
  for i := 0 to FQueryPipelineList.Count - 1 do
    if umlMultipleMatch(True, pipeName, TZDBPipeline(FQueryPipelineList[i]).PipelineName) then
      begin
        Result := True;
        exit;
      end;
end;

procedure TZDBLocalManager.StopPipeline(pipeName: SystemString);
var
  pl: TZDBPipeline;
begin
  pl := GetPipeline(pipeName);
  if pl <> nil then
      pl.Stop;
end;

procedure TZDBLocalManager.GetPipeList(OutputList: TCoreClassListForObj);
begin
  FQueryPipelinePool.GetAsList(OutputList);
end;

procedure TZDBLocalManager.GetDBList(OutputList: TCoreClassListForObj);
begin
  FDBPool.GetAsList(OutputList);
end;

function TZDBLocalManager.Busy(db: TZDBStoreEngine): Boolean;
var
  i : Integer;
  pl: TZDBPipeline;
begin
  Result := False;
  for i := 0 to FQueryPipelineList.Count - 1 do
    begin
      pl := TZDBPipeline(FQueryPipelineList[i]);
      if (pl.Activted) and ((pl.SourceDB = db) or (pl.OutputDB = db)) then
        begin
          Result := True;
          exit;
        end;
    end;
end;

function TZDBLocalManager.AllowDestroy(db: TZDBStoreEngine): Boolean;
var
  i : Integer;
  pl: TZDBPipeline;
begin
  Result := False;
  if db = nil then
      exit;

  for i := 0 to FQueryPipelineList.Count - 1 do
    begin
      pl := TZDBPipeline(FQueryPipelineList[i]);
      if (pl.Activted) and (pl.AutoDestroyDB) and ((pl.SourceDB = db) or (pl.OutputDB = db)) then
          exit;
    end;

  Result := True;
end;

function TZDBLocalManager.QueryDB(WriteResultToOutputDB, InMemory, ReverseQuery: Boolean; dbN, OutputDB: SystemString;
  AutoDestroyDB: Boolean; QueryDoneFreeDelayTime, FragmentWaitTime, MaxWaitTime: Double;
  MaxQueryCompare, MaxQueryResult: Int64): TZDBPipeline;
var
  tn : SystemString;
  plN: SystemString;
begin
  Result := nil;

  if not ExistsDB(dbN) then
      exit;

  tn := GenerateNewTaskName;
  plN := GeneratePipeName(dbN, tn);
  if OutputDB = '' then
      OutputDB := plN;
  Result := FPipelineClass.Create(InMemory, Self, dbN, plN, OutputDB);

  Result.WriteResultToOutputDB := WriteResultToOutputDB;
  Result.AutoDestroyDB := AutoDestroyDB;
  Result.FragmentWaitTime := FragmentWaitTime;
  Result.MaxWaitTime := MaxWaitTime;
  Result.MaxQueryCompare := MaxQueryCompare;
  Result.MaxQueryResult := MaxQueryResult;
  Result.QueryDoneFreeDelayTime := QueryDoneFreeDelayTime;
  Result.WriteFragmentBuffer := True;

  {$IFDEF FPC}
  Result.FQueryTask := Result.SourceDB.Query(Result.PipelineName, @Result.Query, @Result.QueryDone);
  {$ELSE}
  Result.FQueryTask := Result.SourceDB.Query(Result.PipelineName, Result.Query, Result.QueryDone);
  {$ENDIF}
  try
    if Assigned(NotifyIntf) then
        NotifyIntf.CreateQuery(Result);
  except
  end;
end;

function TZDBLocalManager.QueryDB(WriteResultToOutputDB, InMemory, ReverseQuery: Boolean; dbN, OutputDB: SystemString;
  AutoDestroyDB: Boolean; QueryDoneFreeDelayTime, FragmentWaitTime, MaxWaitTime: Double;
  MaxQueryCompare, MaxQueryResult: Int64;
  OnDataFilterCall: TZDBPipelineFilterCall; OnDataDoneCall: TZDBPipelineDoneCall): TZDBPipeline;
begin
  Result := QueryDB(WriteResultToOutputDB, InMemory, ReverseQuery, dbN, OutputDB, AutoDestroyDB, QueryDoneFreeDelayTime, FragmentWaitTime, MaxWaitTime, MaxQueryCompare, MaxQueryResult);
  Result.OnDataFilterCall := OnDataFilterCall;
  Result.OnDataDoneCall := OnDataDoneCall;
end;

function TZDBLocalManager.QueryDB(WriteResultToOutputDB, InMemory, ReverseQuery: Boolean; dbN, OutputDB: SystemString;
  AutoDestroyDB: Boolean; QueryDoneFreeDelayTime, FragmentWaitTime, MaxWaitTime: Double;
  MaxQueryCompare, MaxQueryResult: Int64;
  OnDataFilterMethod: TZDBPipelineFilterMethod; OnDataDoneMethod: TZDBPipelineDoneMethod): TZDBPipeline;
begin
  Result := QueryDB(WriteResultToOutputDB, InMemory, ReverseQuery, dbN, OutputDB, AutoDestroyDB, QueryDoneFreeDelayTime, FragmentWaitTime, MaxWaitTime, MaxQueryCompare, MaxQueryResult);
  Result.OnDataFilterMethod := OnDataFilterMethod;
  Result.OnDataDoneMethod := OnDataDoneMethod;
end;

{$IFNDEF FPC}


function TZDBLocalManager.QueryDB(WriteResultToOutputDB, InMemory, ReverseQuery: Boolean; dbN, OutputDB: SystemString;
  AutoDestroyDB: Boolean; QueryDoneFreeDelayTime, FragmentWaitTime, MaxWaitTime: Double;
  MaxQueryCompare, MaxQueryResult: Int64;
  OnDataFilterProc: TZDBPipelineFilterProc; OnDataDoneProc: TZDBPipelineDoneProc): TZDBPipeline;
begin
  Result := QueryDB(WriteResultToOutputDB, InMemory, ReverseQuery, dbN, OutputDB, AutoDestroyDB, QueryDoneFreeDelayTime, FragmentWaitTime, MaxWaitTime, MaxQueryCompare, MaxQueryResult);
  Result.OnDataFilterProc := OnDataFilterProc;
  Result.OnDataDoneProc := OnDataDoneProc;
end;

function TZDBLocalManager.QueryDB(DataEng: TDataFrameEngine; UserObj: TCoreClassObject;
  WriteResultToOutputDB, InMemory, ReverseQuery: Boolean; dbN, OutputDB: SystemString;
  AutoDestroyDB: Boolean; QueryDoneFreeDelayTime, FragmentWaitTime, MaxWaitTime: Double;
  MaxQueryCompare, MaxQueryResult: Int64;
  OnDataFilterProc: TZDBPipelineFilterProc; OnDataDoneProc: TZDBPipelineDoneProc): TZDBPipeline;
begin
  Result := QueryDB(WriteResultToOutputDB, InMemory, ReverseQuery, dbN, OutputDB, AutoDestroyDB, QueryDoneFreeDelayTime, FragmentWaitTime, MaxWaitTime, MaxQueryCompare, MaxQueryResult);
  Result.OnDataFilterProc := OnDataFilterProc;
  Result.OnDataDoneProc := OnDataDoneProc;
  if DataEng <> nil then
      Result.DataEng.Assign(DataEng);
  Result.UserObject := UserObj;
end;

{$ENDIF}


function TZDBLocalManager.QueryDBToMemory(WriteResultToOutputDB, ReverseQuery: Boolean; dbN: SystemString;
  QueryDoneFreeDelayTime, FragmentWaitTime, MaxWaitTime: Double;
  MaxQueryCompare, MaxQueryResult: Int64): TZDBPipeline;
begin
  Result := QueryDB(WriteResultToOutputDB, True, ReverseQuery, dbN, 'Temp', True, QueryDoneFreeDelayTime, FragmentWaitTime, MaxWaitTime, MaxQueryCompare, MaxQueryResult);
end;

function TZDBLocalManager.QueryDBToMemory(WriteResultToOutputDB, ReverseQuery: Boolean; dbN: SystemString;
  FragmentWaitTime, MaxWaitTime: Double; MaxQueryResult: Int64): TZDBPipeline;
begin
  Result := QueryDB(WriteResultToOutputDB, True, ReverseQuery, dbN, 'Temp', True, 60 * 5, FragmentWaitTime, MaxWaitTime, 0, MaxQueryResult);
end;

{$IFNDEF FPC}


function TZDBLocalManager.QueryDBToMemory(WriteResultToOutputDB, ReverseQuery: Boolean; dbN: SystemString;
  QueryDoneFreeDelayTime, FragmentWaitTime, MaxWaitTime: Double; MaxQueryCompare, MaxQueryResult: Int64;
  OnDataFilterProc: TZDBPipelineFilterProc; OnDataDoneProc: TZDBPipelineDoneProc): TZDBPipeline;
begin
  Result := QueryDB(WriteResultToOutputDB, True, ReverseQuery, dbN, 'Temp', True, QueryDoneFreeDelayTime, FragmentWaitTime, MaxWaitTime, MaxQueryCompare, MaxQueryResult);
  Result.OnDataFilterProc := OnDataFilterProc;
  Result.OnDataDoneProc := OnDataDoneProc;
end;
{$ENDIF}


function TZDBLocalManager.QueryDBToFile(WriteResultToOutputDB, ReverseQuery: Boolean; dbN, OutputDB: SystemString;
  FragmentWaitTime, MaxWaitTime: Double; MaxQueryCompare, MaxQueryResult: Int64): TZDBPipeline;
begin
  Result := QueryDB(WriteResultToOutputDB, False, ReverseQuery, dbN, OutputDB, False, 0, FragmentWaitTime, MaxWaitTime, MaxQueryCompare, MaxQueryResult);
end;

function TZDBLocalManager.WriteDBItemToOneFragment(dbN: SystemString; StorePos: Int64; DestStream: TMemoryStream64): Boolean;
begin
  Result := False;
  if not ExistsDB(dbN) then
      exit;

  Result := EncodeOneFragment(DBName[dbN], StorePos, DestStream);
end;

function TZDBLocalManager.PostData(dN: SystemString; sourDBEng: TZDBStoreEngine; SourStorePos: Int64): Int64;
var
  d: TZDBStoreEngine;
  m: TMemoryStream64InCache;
begin
  Result := -1;
  d := GetDB(dN);
  if d = nil then
      d := InitMemoryDB(dN);
  m := sourDBEng.GetCacheStream(SourStorePos);
  if m <> nil then
    begin
      Result := d.AddData(m, m.CacheID);
      DisposeObject(m);
    end;
end;

function TZDBLocalManager.PostData(dN: SystemString; var qState: TQueryState): Int64;
var
  d: TZDBStoreEngine;
  m: TMemoryStream64InCache;
begin
  Result := -1;
  d := GetDB(dN);
  if d = nil then
      d := InitMemoryDB(dN);
  m := qState.DBEng.GetCacheStream(qState.StorePos, qState.ID);
  if m <> nil then
    begin
      Result := d.AddData(m, m.CacheID);
      DisposeObject(m);
    end;
end;

function TZDBLocalManager.PostData(dN: SystemString; dSour: TCoreClassStream; ID: Cardinal): Int64;
var
  d: TZDBStoreEngine;
begin
  Result := -1;
  d := GetDB(dN);
  if d = nil then
      d := InitMemoryDB(dN);
  Result := d.AddData(dSour, ID);
end;

function TZDBLocalManager.PostData(dN: SystemString; dSour: TDataFrameEngine): Int64;
var
  d: TZDBStoreEngine;
begin
  Result := -1;
  d := GetDB(dN);
  if d = nil then
      d := InitMemoryDB(dN);
  Result := d.AddData(dSour);
end;

function TZDBLocalManager.PostData(dN: SystemString; dSour: THashVariantList): Int64;
var
  d: TZDBStoreEngine;
begin
  Result := -1;
  d := GetDB(dN);
  if d = nil then
      d := InitMemoryDB(dN);
  Result := d.AddData(dSour);
end;

function TZDBLocalManager.PostData(dN: SystemString; dSour: TSectionTextData): Int64;
var
  d: TZDBStoreEngine;
begin
  Result := -1;
  d := GetDB(dN);
  if d = nil then
      d := InitMemoryDB(dN);
  Result := d.AddData(dSour);
end;

function TZDBLocalManager.PostData(dN: SystemString; dSour: TPascalString): Int64;
var
  d: TZDBStoreEngine;
begin
  Result := -1;
  d := GetDB(dN);
  if d = nil then
      d := InitMemoryDB(dN);
  Result := d.AddData(dSour);
end;

{$IFNDEF FPC}


function TZDBLocalManager.PostData(dN: SystemString; dSour: TJsonObject): Int64;
var
  d: TZDBStoreEngine;
begin
  Result := -1;
  d := GetDB(dN);
  if d = nil then
      d := InitMemoryDB(dN);
  Result := d.AddData(dSour);
end;
{$ENDIF}


procedure TZDBLocalManager.Clear;
var
  lst: TCoreClassListForObj;
  i  : Integer;
begin
  FProgressPost.ResetPost;

  lst := TCoreClassListForObj.Create;
  FDBPool.GetAsList(lst);
  for i := 0 to lst.Count - 1 do
      TZDBStoreEngine(lst[i]).StopAllQuery;
  DisposeObject(lst);

  lst := TCoreClassListForObj.Create;
  FQueryPipelinePool.GetAsList(lst);
  for i := 0 to lst.Count - 1 do
      DisposeObject(lst[i]);
  DisposeObject(lst);

  FDBPool.Clear;
end;

procedure TZDBLocalManager.LoadDB(ReadOnly: Boolean);
var
  arr  : umlStringDynArray;
  fn, n: SystemString;

begin
  Clear;

  arr := umlGetFileListWithFullPath(RootPath);

  for fn in arr do
    begin
      n := umlGetFileName(fn);
      if not umlMultipleMatch(True, '*.CompressSwap.*', n) then
        if umlMultipleMatch(True, '*.OX', n) then
            InitDB(umlChangeFileExt(n, '').Text, readonly);
    end;
  setLength(arr, 0);
end;

procedure TZDBLocalManager.SetRootPath(const Value: SystemString);
begin
  FRootPath := Value;
  LoadDB(False);
end;

procedure TZDBLocalManager.Progress;
begin
  FCadencerEng.Progress;
end;

function TZDBLocalManager.InsertData(dN: SystemString; InsertPos: Int64; dSour: TCoreClassStream; ID: Cardinal): Int64;
var
  d: TZDBStoreEngine;
begin
  Result := -1;
  d := GetDB(dN);
  if d = nil then
    begin
      d := InitMemoryDB(dN);
      Result := d.AddData(dSour, ID);
    end
  else
      Result := d.InsertData(InsertPos, dSour, ID);
end;

function TZDBLocalManager.InsertData(dN: SystemString; InsertPos: Int64; dSour: TDataFrameEngine): Int64;
var
  d: TZDBStoreEngine;
begin
  Result := -1;
  d := GetDB(dN);
  if d = nil then
    begin
      d := InitMemoryDB(dN);
      Result := d.AddData(dSour);
    end
  else
      Result := d.InsertData(InsertPos, dSour);
end;

function TZDBLocalManager.InsertData(dN: SystemString; InsertPos: Int64; dSour: THashVariantList): Int64;
var
  d: TZDBStoreEngine;
begin
  Result := -1;
  d := GetDB(dN);
  if d = nil then
    begin
      d := InitMemoryDB(dN);
      Result := d.AddData(dSour);
    end
  else
      Result := d.InsertData(InsertPos, dSour);
end;

function TZDBLocalManager.InsertData(dN: SystemString; InsertPos: Int64; dSour: TSectionTextData): Int64;
var
  d: TZDBStoreEngine;
begin
  Result := -1;
  d := GetDB(dN);
  if d = nil then
    begin
      d := InitMemoryDB(dN);
      Result := d.AddData(dSour);
    end
  else
      Result := d.InsertData(InsertPos, dSour);
end;

function TZDBLocalManager.InsertData(dN: SystemString; InsertPos: Int64; dSour: TPascalString): Int64;
var
  d: TZDBStoreEngine;
begin
  Result := -1;
  d := GetDB(dN);
  if d = nil then
    begin
      d := InitMemoryDB(dN);
      Result := d.AddData(dSour);
    end
  else
      Result := d.InsertData(InsertPos, dSour);
end;

{$IFNDEF FPC}


function TZDBLocalManager.InsertData(dN: SystemString; InsertPos: Int64; dSour: TJsonObject): Int64;
var
  d: TZDBStoreEngine;
begin
  Result := -1;
  d := GetDB(dN);
  if d = nil then
    begin
      d := InitMemoryDB(dN);
      Result := d.AddData(dSour);
    end
  else
      Result := d.InsertData(InsertPos, dSour);
end;
{$ENDIF}


procedure TZDBLocalManager.DeleteData(dN: SystemString; StorePos: Int64);
var
  d: TZDBStoreEngine;
begin
  d := GetDB(dN);
  if d = nil then
      exit;
  d.PostDeleteData(StorePos);
end;

function TZDBLocalManager.GetData(dN: SystemString; StorePos: Int64; ID: Cardinal): TMemoryStream64InCache;
var
  d: TZDBStoreEngine;
begin
  Result := nil;
  d := GetDB(dN);
  if d = nil then
      exit;
  Result := d.GetCacheStream(StorePos, ID);
end;

function TZDBLocalManager.SetData(dN: SystemString; StorePos: Int64; dSour: TMemoryStream64): Boolean;
var
  d: TZDBStoreEngine;
begin
  Result := False;
  d := GetDB(dN);
  if d = nil then
      exit;
  Result := d.SetData(StorePos, dSour);
end;

initialization

end.
