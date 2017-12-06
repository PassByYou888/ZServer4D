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

uses CoreClasses, ListEngine, UnicodeMixedLib, DataFrameEngine, MemoryStream64, TextDataEngine,
  {$IFNDEF FPC}
  JsonDataObjects,
  {$ENDIF}
  DoStatusIO, Cadencer, NotifyObjectBase, PascalStrings, CoreCipher, ZDBEngine, ItemStream;

type
  TZDBStoreEngine = class(TDBStoreBase)
  protected
    FName: SystemString;
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

  TZDBPipeline = class(TCoreClassObject)
  private
    FQueryCounter            : Int64;
    FCurrentFragmentTime     : TTimeTickValue;
    FFragmentBuffer          : TMemoryStream64;
    FActivted                : Boolean;
    FQueryTask               : TQueryTask;
    FPerformaceCounter       : Integer;
    FLastPerformaceTime      : TTimeTickValue;
    FQueryCounterOfPerSec    : Double;
    FRealTimePostFragmentData: Boolean;

    procedure Query(var UserData: Pointer; var qState: TQueryState);
    procedure QueryDone(var UserData: Pointer);

    function WriteToOutput(DBEng: TDBStoreBase; StorePos: Int64; id: Cardinal): Int64; inline;
    procedure PostFragmentData(forcePost: Boolean); inline;
  public
    Owner       : TZDBLocalManager;
    SourceDB    : TZDBStoreEngine;
    OutputDB    : TZDBStoreEngine;
    PipelineName: SystemString;

    // query options
    AutoDestroyDB         : Boolean; // complete time destroy DB
    FragmentWaitTime      : Double;  // fragment time,realtime send to client
    MaxWaitTime           : Double;  // max wait complete time,query to abort from out time
    MaxQueryCompare       : Int64;   // max query compare
    MaxQueryResult        : Int64;   // max query result
    QueryDoneFreeDelayTime: Double;  // delay free query pipeline
    NoOutput              : Boolean; // no outputDB

    OnDataFilterCall  : TZDBPipelineFilterCall;
    OnDataFilterMethod: TZDBPipelineFilterMethod;

    OnDataDoneCall  : TZDBPipelineDoneCall;
    OnDataDoneMethod: TZDBPipelineDoneMethod;
    {$IFNDEF FPC}
    OnDataFilterProc: TZDBPipelineFilterProc;
    OnDataDoneProc  : TZDBPipelineDoneProc;
    {$ENDIF}
    Values: THashVariantList;
  public
    function GeneratePipeName(const sourDBName, taskName, OutDBName: SystemString): SystemString; inline;

    constructor CreateQueryToNewMemory(AOwner: TZDBLocalManager; sourDBName, taskName, OutDBName: SystemString); virtual;
    constructor CreateQueryToFile(AOwner: TZDBLocalManager; sourDBName, taskName, OutDBName: SystemString); virtual;
    destructor Destroy; override;

    procedure Progress(deltaTime: Double); virtual;

    procedure Stop; virtual;
    procedure Pause; virtual;
    procedure Play; virtual;

    function WriteOutput(var qState: TQueryState): Int64;

    property Activted: Boolean read FActivted;
    property QueryCounterOfPerSec: Double read FQueryCounterOfPerSec;
    property RealTimePostFragmentData: Boolean read FRealTimePostFragmentData write FRealTimePostFragmentData;
    property QueryCounter: Int64 read FQueryCounter;
  end;

  TZDBPipelineClass = class of TZDBPipeline;

  IZDBLocalManagerNotify = interface
    procedure CreateQuery(pipe: TZDBPipeline);
    procedure QueryFragmentData(pipe: TZDBPipeline; FragmentSource: TMemoryStream64);
    procedure QueryDone(pipe: TZDBPipeline);
    procedure CreateDB(ActiveDB: TZDBStoreEngine);
    procedure CloseDB(ActiveDB: TZDBStoreEngine);
    procedure InsertData(Sender: TZDBStoreEngine; InsertPos: Int64; buff: TCoreClassStream; id: Cardinal; CompletePos: Int64);
    procedure AddData(Sender: TZDBStoreEngine; buff: TCoreClassStream; id: Cardinal; CompletePos: Int64);
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
    procedure DoInsertData(Sender: TDBStoreBase; InsertPos: Int64; buff: TCoreClassStream; id: Cardinal; CompletePos: Int64); virtual;
    procedure DoAddData(Sender: TDBStoreBase; buff: TCoreClassStream; id: Cardinal; CompletePos: Int64); virtual;
    procedure DoModifyData(Sender: TDBStoreBase; const StorePos: Int64; buff: TCoreClassStream); virtual;
    procedure DoDeleteData(Sender: TDBStoreBase; const StorePos: Int64); virtual;
  protected
    // canencer trigger
    procedure CadencerProgress(const deltaTime, newTime: Double);
  protected
    procedure DoQueryFragmentData(pipe: TZDBPipeline; FragmentSour: TMemoryStream64); virtual;
    procedure DoQueryDone(pipe: TZDBPipeline); virtual;
    procedure DelayFreePipe(Sender: TNPostExecute); virtual;
  public
    constructor Create;
    destructor Destroy; override;

    property PipelineClass: TZDBPipelineClass read FPipelineClass write FPipelineClass;
    property NotifyIntf: IZDBLocalManagerNotify read FNotifyIntf write FNotifyIntf;

    procedure Reset;
    procedure LoadDB(ReadOnly: Boolean);
    procedure SetRootPath(const Value: SystemString);
    property RootPath: SystemString read FRootPath write SetRootPath;

    procedure Progress; virtual;
    property ProgressPost: TNProgressPost read FProgressPost;

    // local operation
    function InitDB(dbN: SystemString; ReadOnly: Boolean): TZDBStoreEngine;
    function InitNewDB(dbN: SystemString): TZDBStoreEngine;
    function InitMemoryDB(dbN: SystemString): TZDBStoreEngine;
    procedure CloseDB(dbN: SystemString);

    procedure Recache;

    function GenerateNewTaskName: SystemString; inline;
    function GetPipeline(pipeName: SystemString): TZDBPipeline;
    function GetDB(dN: SystemString): TZDBStoreEngine;

    property DBName[dN: SystemString]: TZDBStoreEngine read GetDB; default;
    property PipelineN[pipeName: SystemString]: TZDBPipeline read GetPipeline;
    property QueryPipelineList: TCoreClassListForObj read FQueryPipelineList;

    function ExistsDB(dN: SystemString): Boolean;
    function ExistsPipeline(pipeName: SystemString): Boolean;

    procedure StopPipeline(pipeName: SystemString);

    procedure GetPipeList(OutputList: TCoreClassListForObj);
    procedure GetDBList(OutputList: TCoreClassListForObj);

    function QueryDB(InMemory, ReverseQuery: Boolean; dbN, OutputDB: SystemString;
      AutoDestroyDB: Boolean; DelayDestroyDBTime, FragmentWaitTime, MaxWaitTime: Double;
      MaxQueryCompare, MaxQueryResult: Int64): TZDBPipeline; overload;

    function QueryDBToMemory(ReverseQuery: Boolean; dbN: SystemString;
      DelayDestroyDBTime, FragmentWaitTime, MaxWaitTime: Double;
      MaxQueryCompare, MaxQueryResult: Int64): TZDBPipeline; overload;

    function QueryDBToMemory(ReverseQuery: Boolean; dbN: SystemString;
      FragmentWaitTime, MaxWaitTime: Double; MaxQueryResult: Int64): TZDBPipeline; overload;

    {$IFNDEF FPC}
    function QueryDBToMemory(ReverseQuery: Boolean; dbN: SystemString;
      DelayDestroyDBTime, FragmentWaitTime, MaxWaitTime: Double; MaxQueryCompare, MaxQueryResult: Int64;
      OnDataFilterProc: TZDBPipelineFilterProc; OnDataDoneProc: TZDBPipelineDoneProc): TZDBPipeline; overload;
    {$ENDIF}
    function QueryDBToFile(ReverseQuery: Boolean; dbN, OutputDB: SystemString;
      FragmentWaitTime, MaxWaitTime: Double; MaxQueryCompare, MaxQueryResult: Int64): TZDBPipeline;

    function WriteDBItemToOneFragment(dbN: SystemString; StorePos: Int64; DestStream: TMemoryStream64): Boolean;

    function PostData(dN: SystemString; var qState: TQueryState): Int64; overload;
    function PostData(dN: SystemString; dSour: TCoreClassStream; id: Cardinal): Int64; overload;
    function PostData(dN: SystemString; dSour: TDataFrameEngine): Int64; overload;
    function PostData(dN: SystemString; dSour: THashVariantList): Int64; overload;
    function PostData(dN: SystemString; dSour: TSectionTextData): Int64; overload;
    function PostData(dN: SystemString; dSour: TPascalString): Int64; overload;
    {$IFNDEF FPC}
    function PostData(dN: SystemString; dSour: TJsonObject): Int64; overload;
    {$ENDIF}
    function InsertData(dN: SystemString; InsertPos: Int64; dSour: TCoreClassStream; id: Cardinal): Int64; overload;
    function InsertData(dN: SystemString; InsertPos: Int64; dSour: TDataFrameEngine): Int64; overload;
    function InsertData(dN: SystemString; InsertPos: Int64; dSour: THashVariantList): Int64; overload;
    function InsertData(dN: SystemString; InsertPos: Int64; dSour: TSectionTextData): Int64; overload;
    function InsertData(dN: SystemString; InsertPos: Int64; dSour: TPascalString): Int64; overload;
    {$IFNDEF FPC}
    function InsertData(dN: SystemString; InsertPos: Int64; dSour: TJsonObject): Int64; overload;
    {$ENDIF}
    function DeleteData(dN: SystemString; StorePos: Int64): Boolean; inline;
    function GetData(dN: SystemString; StorePos: Int64; id: Cardinal): TItemStream; inline;
    function SetData(dN: SystemString; StorePos: Int64; dSour: TMemoryStream64): Boolean; inline;
  end;

  TFillQueryDataCall   = procedure(dbN, pipeN: SystemString; StorePos: Int64; id: Cardinal; DataSour: TMemoryStream64);
  TFillQueryDataMethod = procedure(dbN, pipeN: SystemString; StorePos: Int64; id: Cardinal; DataSour: TMemoryStream64) of object;
  {$IFNDEF FPC}
  TFillQueryDataProc = reference to procedure(dbN, pipeN: SystemString; StorePos: Int64; id: Cardinal; DataSour: TMemoryStream64);
  {$ENDIF}

procedure FillFragmentToDB(DataSour: TMemoryStream64; db: TDBStoreBase); inline;
procedure FillFragmentSource(dbN, pipeN: SystemString; DataSour: TMemoryStream64; OnResult: TFillQueryDataCall); overload; inline;
procedure FillFragmentSource(dbN, pipeN: SystemString; DataSour: TMemoryStream64; OnResult: TFillQueryDataMethod); overload; inline;
{$IFNDEF FPC}
procedure FillFragmentSource(dbN, pipeN: SystemString; DataSour: TMemoryStream64; OnResult: TFillQueryDataProc); overload; inline;
{$ENDIF}

function DecodeOneFragment(DataSour: TMemoryStream64; var dStorePos: Int64; var id: Cardinal): TMemoryStream64;

implementation

procedure FillFragmentToDB(DataSour: TMemoryStream64; db: TDBStoreBase);
var
  StorePos, siz: Int64;
  id           : Cardinal;
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
      if DataSour.ReadPtr(@id, umlCardinalLength) <> umlCardinalLength then
          break;

      if DataSour.Position + siz > DataSour.Size then
          break;

      try
        m64.SetPointerWithProtectedMode(DataSour.PositionAsPtr(DataSour.Position), siz);
        db.AddData(m64, id);
      except
      end;

      DataSour.Position := DataSour.Position + siz;
    end;
  DisposeObject(m64);
end;

procedure FillFragmentSource(dbN, pipeN: SystemString; DataSour: TMemoryStream64; OnResult: TFillQueryDataCall);
var
  StorePos, siz: Int64;
  id           : Cardinal;
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
      if DataSour.ReadPtr(@id, umlCardinalLength) <> umlCardinalLength then
          break;

      if DataSour.Position + siz > DataSour.Size then
          break;

      try
        m64.SetPointerWithProtectedMode(DataSour.PositionAsPtr(DataSour.Position), siz);
        OnResult(dbN, pipeN, StorePos, id, m64);
      except
      end;

      DataSour.Position := DataSour.Position + siz;
    end;
  DisposeObject(m64);
end;

procedure FillFragmentSource(dbN, pipeN: SystemString; DataSour: TMemoryStream64; OnResult: TFillQueryDataMethod);
var
  StorePos, siz: Int64;
  id           : Cardinal;
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
      if DataSour.ReadPtr(@id, umlCardinalLength) <> umlCardinalLength then
          break;

      if DataSour.Position + siz > DataSour.Size then
          break;

      try
        m64.SetPointerWithProtectedMode(DataSour.PositionAsPtr(DataSour.Position), siz);
        OnResult(dbN, pipeN, StorePos, id, m64);
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
  id           : Cardinal;
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
      if DataSour.ReadPtr(@id, umlCardinalLength) <> umlCardinalLength then
          break;

      if DataSour.Position + siz > DataSour.Size then
          break;

      try
        m64.SetPointerWithProtectedMode(DataSour.PositionAsPtr(DataSour.Position), siz);
        OnResult(dbN, pipeN, StorePos, id, m64);
      except
      end;

      DataSour.Position := DataSour.Position + siz;
    end;
  DisposeObject(m64);
end;
{$ENDIF}


function DecodeOneFragment(DataSour: TMemoryStream64; var dStorePos: Int64; var id: Cardinal): TMemoryStream64;
var
  siz: Int64;
begin
  Result := nil;
  if DataSour.ReadPtr(@dStorePos, umlInt64Length) <> umlInt64Length then
      exit;
  if DataSour.ReadPtr(@siz, umlInt64Length) <> umlInt64Length then
      exit;
  if DataSour.ReadPtr(@id, umlCardinalLength) <> umlCardinalLength then
      exit;

  if DataSour.Position + siz > DataSour.Size then
      exit;

  Result := TMemoryStream64.Create;
  Result.SetPointerWithProtectedMode(DataSour.PositionAsPtr(DataSour.Position), siz);
end;

procedure TZDBStoreEngine.DoCreateInit;
begin
  inherited DoCreateInit;
  FName := '';
end;

procedure TZDBPipeline.Query(var UserData: Pointer; var qState: TQueryState);

var
  lastTime   : TTimeTickValue;
  AlreadWrite: Boolean;
  Allowed    : Boolean;

  procedure DoWrite;
  begin
    if AlreadWrite then
        exit;

    if not NoOutput then
        WriteToOutput(qState.DBEng, qState.StorePos, qState.QueryHnd^.FieldSearch.RHeader.UserProperty);
    AlreadWrite := True;
  end;

begin
  lastTime := GetTimeTick;
  inc(FPerformaceCounter);

  FActivted := True;

  AlreadWrite := False;

  Allowed := False;
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

  if (MaxQueryResult > 0) and (OutputDB.Count >= MaxQueryResult) then
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

  if (FragmentWaitTime > 0) and (AlreadWrite) then
    begin
      FCurrentFragmentTime := FCurrentFragmentTime + qState.deltaTime;
      while FCurrentFragmentTime >= Trunc(FragmentWaitTime * 1000) do
        begin
          PostFragmentData(False);
          FCurrentFragmentTime := FCurrentFragmentTime - Trunc(FragmentWaitTime * 1000);
        end;
    end;

  if lastTime - FLastPerformaceTime > 1000 then
    begin
      try
          FQueryCounterOfPerSec := FPerformaceCounter / ((lastTime - FLastPerformaceTime) * 0.001);
      except
          FQueryCounterOfPerSec := 0;
      end;
      FLastPerformaceTime := lastTime;
      FPerformaceCounter := 0;
    end;
end;

procedure TZDBPipeline.QueryDone(var UserData: Pointer);
begin
  if (FragmentWaitTime > 0) then
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
  Owner.DoQueryDone(Self);

  FActivted := False;

  FPerformaceCounter := 0;
  FLastPerformaceTime := GetTimeTick;
end;

function TZDBPipeline.WriteToOutput(DBEng: TDBStoreBase; StorePos: Int64; id: Cardinal): Int64;
var
  itmStream: TItemStream;
  siz      : Int64;
begin
  itmStream := DBEng.GetData(StorePos, id);
  Result := OutputDB.AddData(itmStream, id);
  if FragmentWaitTime > 0 then
    begin
      itmStream.Position := 0;
      siz := itmStream.Size;
      FFragmentBuffer.Position := FFragmentBuffer.Size;
      FFragmentBuffer.WritePtr(@StorePos, umlInt64Length);
      FFragmentBuffer.WritePtr(@siz, umlInt64Length);
      FFragmentBuffer.WritePtr(@id, umlCardinalLength);
      FFragmentBuffer.CopyFrom(itmStream, siz);
    end;
  DisposeObject(itmStream);
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

function TZDBPipeline.GeneratePipeName(const sourDBName, taskName, OutDBName: SystemString): SystemString;
begin
  Result := sourDBName + '.QueryPipe.' + taskName + '.' + OutDBName;
end;

constructor TZDBPipeline.CreateQueryToNewMemory(AOwner: TZDBLocalManager; sourDBName, taskName, OutDBName: SystemString);
begin
  inherited Create;
  FQueryCounter := 0;
  FCurrentFragmentTime := 0;
  FFragmentBuffer := TMemoryStream64.Create;

  Owner := AOwner;

  SourceDB := Owner.FDBPool[sourDBName] as TZDBStoreEngine;

  PipelineName := GeneratePipeName(sourDBName, taskName, OutDBName);
  OutputDB := Owner.InitMemoryDB(PipelineName);

  FActivted := True;

  // data query options
  AutoDestroyDB := True;        // complete time destroy DB
  FragmentWaitTime := 0.5;      // fragment time,realtime send to client
  MaxWaitTime := 0;             // max wait complete time,query to abort from out time
  MaxQueryCompare := 0;         // max query compare
  MaxQueryResult := 0;          // max query result
  QueryDoneFreeDelayTime := 60; // query done free delay time
  NoOutput := False;            // no outputDB

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

  FQueryTask := nil;

  FPerformaceCounter := 0;
  FLastPerformaceTime := GetTimeTick;
  FQueryCounterOfPerSec := 0;
  FRealTimePostFragmentData := True;

  Owner.FQueryPipelinePool[PipelineName] := Self;
  Owner.FQueryPipelineList.Add(Self);
end;

constructor TZDBPipeline.CreateQueryToFile(AOwner: TZDBLocalManager; sourDBName, taskName, OutDBName: SystemString);
begin
  inherited Create;
  FQueryCounter := 0;
  FCurrentFragmentTime := 0;
  FFragmentBuffer := TMemoryStream64.Create;

  Owner := AOwner;

  SourceDB := Owner.FDBPool[sourDBName] as TZDBStoreEngine;

  PipelineName := GeneratePipeName(sourDBName, taskName, OutDBName);
  OutputDB := Owner.InitNewDB(PipelineName);

  FActivted := True;

  // data query options
  AutoDestroyDB := True;        // complete time destroy DB
  FragmentWaitTime := 0.5;      // fragment time,realtime send to client
  MaxWaitTime := 0;             // max wait complete time,query to abort from out time
  MaxQueryCompare := 0;         // max query compare
  MaxQueryResult := 0;          // max query result
  QueryDoneFreeDelayTime := 60; // query done free delay time
  NoOutput := False;            // no outputDB

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

  FQueryTask := nil;
  FPerformaceCounter := 0;
  FLastPerformaceTime := GetTimeTick;
  FQueryCounterOfPerSec := 0;
  FRealTimePostFragmentData := True;

  Owner.FQueryPipelinePool[PipelineName] := Self;
  Owner.FQueryPipelineList.Add(Self);
end;

destructor TZDBPipeline.Destroy;
var
  fn: SystemString;
  i : Integer;
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
    if AutoDestroyDB then
      begin
        if OutputDB.DBEngine.StreamEngine is TMemoryStream64 then
          begin
            Owner.CloseDB(PipelineName);
          end
        else
          begin
            fn := OutputDB.DBEngine.ObjectName;
            Owner.CloseDB(PipelineName);
            if umlFileExists(fn) then
                umlDeleteFile(fn);
          end;
      end;
  except
  end;

  DisposeObject([FFragmentBuffer, Values]);

  inherited Destroy;
end;

procedure TZDBPipeline.Progress(deltaTime: Double);
begin
end;

procedure TZDBPipeline.Stop;
begin
  FQueryTask.Stop;
end;

procedure TZDBPipeline.Pause;
begin
  if (FragmentWaitTime > 0) then
      PostFragmentData(True);
  FQueryTask.Pause;
end;

procedure TZDBPipeline.Play;
begin
  FQueryTask.Play;
end;

function TZDBPipeline.WriteOutput(var qState: TQueryState): Int64;
var
  itmStream: TItemStream;
begin
  itmStream := qState.DBEng.GetData(qState.StorePos, qState.QueryHnd^.FieldSearch.RHeader.UserProperty);
  Result := OutputDB.AddData(itmStream, qState.QueryHnd^.FieldSearch.RHeader.UserProperty);
  DisposeObject(itmStream);
end;

procedure TZDBLocalManager.DoInsertData(Sender: TDBStoreBase; InsertPos: Int64; buff: TCoreClassStream; id: Cardinal; CompletePos: Int64);
begin
  try
    if Assigned(FNotifyIntf) then
        FNotifyIntf.InsertData(TZDBStoreEngine(Sender), InsertPos, buff, id, CompletePos);
  except
  end;
end;

procedure TZDBLocalManager.DoAddData(Sender: TDBStoreBase; buff: TCoreClassStream; id: Cardinal; CompletePos: Int64);
begin
  try
    if Assigned(FNotifyIntf) then
        FNotifyIntf.AddData(TZDBStoreEngine(Sender), buff, id, CompletePos);
  except
  end;
end;

procedure TZDBLocalManager.DoModifyData(Sender: TDBStoreBase; const StorePos: Int64; buff: TCoreClassStream);
begin
  try
    if Assigned(FNotifyIntf) then
        FNotifyIntf.ModifyData(TZDBStoreEngine(Sender), StorePos, buff);
  except
  end;
end;

procedure TZDBLocalManager.DoDeleteData(Sender: TDBStoreBase; const StorePos: Int64);
begin
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
        pl := TZDBPipeline(FQueryPipelineList);
        if pl.SourceDB = sour.OutputDB then
          begin
            {$IFDEF FPC}
            with ProgressPost.PostExecute(15.0, @DelayFreePipe) do
            {$ELSE}
            with ProgressPost.PostExecute(15.0, DelayFreePipe) do
              {$ENDIF}
                Data1 := sour;
            exit;
          end;
      end;

  DisposeObject(sour);
end;

constructor TZDBLocalManager.Create;
begin
  inherited Create;
  FRootPath := umlCurrentPath;
  FDBPool := THashObjectList.Create(True, 1024);
  FQueryPipelinePool := THashObjectList.Create(False, 1024);
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
end;

procedure TZDBLocalManager.CloseDB(dbN: SystemString);
begin
  if not FDBPool.Exists(dbN) then
      exit;

  try
    if Assigned(FNotifyIntf) then
        FNotifyIntf.CloseDB(GetDB(dbN));
  except
  end;

  FDBPool.Delete(dbN);
end;

procedure TZDBLocalManager.Recache;
var
  lst: TCoreClassListForObj;
  i  : Integer;
  db : TZDBStoreEngine;
begin
  lst := TCoreClassListForObj.Create;
  FDBPool.FastGetAsList(lst);
  for i := 0 to lst.Count - 1 do
    begin
      db := TZDBStoreEngine(lst[i]);
      db.Recache;
    end;
  DisposeObject(lst);
end;

function TZDBLocalManager.GenerateNewTaskName: SystemString;
begin
  Result := 'Task' + umlIntToStr(FTaskCounter);
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

function TZDBLocalManager.ExistsDB(dN: SystemString): Boolean;
begin
  Result := FDBPool.Exists(dN);
end;

function TZDBLocalManager.ExistsPipeline(pipeName: SystemString): Boolean;
begin
  Result := FQueryPipelinePool.Exists(pipeName);
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

function TZDBLocalManager.QueryDB(InMemory, ReverseQuery: Boolean; dbN, OutputDB: SystemString;
  AutoDestroyDB: Boolean; DelayDestroyDBTime, FragmentWaitTime, MaxWaitTime: Double;
  MaxQueryCompare, MaxQueryResult: Int64): TZDBPipeline;
var
  tn: SystemString;
begin
  tn := GenerateNewTaskName;
  if InMemory then
      Result := FPipelineClass.CreateQueryToNewMemory(Self, dbN, tn, OutputDB)
  else
      Result := FPipelineClass.CreateQueryToFile(Self, dbN, tn, OutputDB);

  Result.AutoDestroyDB := AutoDestroyDB;
  Result.FragmentWaitTime := FragmentWaitTime;
  Result.MaxWaitTime := MaxWaitTime;
  Result.MaxQueryCompare := MaxQueryCompare;
  Result.MaxQueryResult := MaxQueryResult;
  Result.QueryDoneFreeDelayTime := DelayDestroyDBTime;
  Result.NoOutput := False;

  {$IFDEF FPC}
  Result.FQueryTask := Result.SourceDB.Query(Result.PipelineName, nil, @Result.Query, @Result.QueryDone);
  {$ELSE}
  Result.FQueryTask := Result.SourceDB.Query(Result.PipelineName, nil, Result.Query, Result.QueryDone);
  {$ENDIF}
  try
    if Assigned(NotifyIntf) then
        NotifyIntf.CreateQuery(Result);
  except
  end;
end;

function TZDBLocalManager.QueryDBToMemory(ReverseQuery: Boolean; dbN: SystemString;
  DelayDestroyDBTime, FragmentWaitTime, MaxWaitTime: Double;
  MaxQueryCompare, MaxQueryResult: Int64): TZDBPipeline;
begin
  Result := QueryDB(True, ReverseQuery, dbN, 'Temp', True, DelayDestroyDBTime, FragmentWaitTime, MaxWaitTime, MaxQueryCompare, MaxQueryResult);
end;

function TZDBLocalManager.QueryDBToMemory(ReverseQuery: Boolean; dbN: SystemString;
  FragmentWaitTime, MaxWaitTime: Double; MaxQueryResult: Int64): TZDBPipeline;
begin
  Result := QueryDB(True, ReverseQuery, dbN, 'Temp', True, 60 * 5, FragmentWaitTime, MaxWaitTime, 0, MaxQueryResult);
end;

{$IFNDEF FPC}


function TZDBLocalManager.QueryDBToMemory(ReverseQuery: Boolean; dbN: SystemString;
  DelayDestroyDBTime, FragmentWaitTime, MaxWaitTime: Double; MaxQueryCompare, MaxQueryResult: Int64;
  OnDataFilterProc: TZDBPipelineFilterProc; OnDataDoneProc: TZDBPipelineDoneProc): TZDBPipeline;
begin
  Result := QueryDB(True, ReverseQuery, dbN, 'Temp', True, DelayDestroyDBTime, FragmentWaitTime, MaxWaitTime, MaxQueryCompare, MaxQueryResult);
  Result.OnDataFilterProc := OnDataFilterProc;
  Result.OnDataDoneProc := OnDataDoneProc;
end;
{$ENDIF}


function TZDBLocalManager.QueryDBToFile(ReverseQuery: Boolean; dbN, OutputDB: SystemString;
  FragmentWaitTime, MaxWaitTime: Double; MaxQueryCompare, MaxQueryResult: Int64): TZDBPipeline;
begin
  Result := QueryDB(False, ReverseQuery, dbN, OutputDB, False, 0, FragmentWaitTime, MaxWaitTime, MaxQueryCompare, MaxQueryResult);
end;

function TZDBLocalManager.WriteDBItemToOneFragment(dbN: SystemString; StorePos: Int64; DestStream: TMemoryStream64): Boolean;
var
  itmStream: TItemStream;
  siz      : Int64;
  id       : Cardinal;
begin
  Result := False;
  if not ExistsDB(dbN) then
      exit;
  itmStream := DBName[dbN].GetData(StorePos);
  if itmStream <> nil then
    begin
      siz := itmStream.Size;
      id := itmStream.Hnd^.Item.RHeader.UserProperty;
      DestStream.Position := DestStream.Size;
      DestStream.WritePtr(@StorePos, umlInt64Length);
      DestStream.WritePtr(@siz, umlInt64Length);
      DestStream.WritePtr(@id, umlCardinalLength);
      DestStream.CopyFrom(itmStream, siz);
      Result := True;
    end;
end;

function TZDBLocalManager.PostData(dN: SystemString; var qState: TQueryState): Int64;
var
  d: TZDBStoreEngine;
  m: TItemStream;
begin
  Result := -1;
  d := GetDB(dN);
  if d = nil then
      d := InitMemoryDB(dN);

  m := qState.DBEng.GetData(qState.StorePos, qState.QueryHnd^.FieldSearch.RHeader.PositionID);
  Result := d.AddData(m, qState.QueryHnd^.FieldSearch.RHeader.PositionID);
  DisposeObject(m);
end;

function TZDBLocalManager.PostData(dN: SystemString; dSour: TCoreClassStream; id: Cardinal): Int64;
var
  d: TZDBStoreEngine;
begin
  Result := -1;
  d := GetDB(dN);
  if d = nil then
      d := InitMemoryDB(dN);
  Result := d.AddData(dSour, id);
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


procedure TZDBLocalManager.Reset;
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
  fn, n: string;

begin
  Reset;

  arr := umlGetFileListWithFullPath(RootPath);

  for fn in arr do
    begin
      n := umlGetFileName(fn);
      if umlMultipleMatch(True, '*.OX', n) then
          InitDB(umlChangeFileExt(n, '').Text, readonly);
    end;
  SetLength(arr, 0);
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

function TZDBLocalManager.InsertData(dN: SystemString; InsertPos: Int64; dSour: TCoreClassStream; id: Cardinal): Int64;
var
  d: TZDBStoreEngine;
begin
  Result := -1;
  d := GetDB(dN);
  if d = nil then
    begin
      d := InitMemoryDB(dN);
      Result := d.AddData(dSour, id);
    end
  else
      Result := d.InsertData(InsertPos, dSour, id);
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


function TZDBLocalManager.DeleteData(dN: SystemString; StorePos: Int64): Boolean;
var
  d: TZDBStoreEngine;
begin
  Result := False;
  d := GetDB(dN);
  if d = nil then
      exit;
  Result := d.DeleteData(StorePos);
end;

function TZDBLocalManager.GetData(dN: SystemString; StorePos: Int64; id: Cardinal): TItemStream;
var
  d: TZDBStoreEngine;
begin
  Result := nil;
  d := GetDB(dN);
  if d = nil then
      exit;
  Result := d.GetData(StorePos, id);
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

end.
