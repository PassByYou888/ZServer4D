{******************************************************************************}
{* DBEngine, support,                                                         *}
{* https://github.com/PassByYou888/CoreCipher                                 *}
(* https://github.com/PassByYou888/ZServer4D                                  *)
{******************************************************************************}


unit ListDBEngine;

{$I zDefine.inc}

interface

uses SysUtils, Classes,
  ListEngine, PascalStrings, UnicodeMixedLib,
  TextDataEngine,

  {$IFNDEF FPC}
  JsonDataObjects,
  {$ENDIF}
  CoreClasses, MemoryStream64, ObjectData, ObjectDataManager,
  DataFrameEngine, ItemStream, DoStatusIO;

type
  TDBStoreEngine = class;

  // Base Data Struct
  TDBEngineDF = class(TDataFrameEngine)
  protected
    DBStorePos                : Int64;
    DBEng                     : TDBStoreEngine;
    CreateTime, LastModifyTime: TDateTime;
  public
    constructor Create;
    procedure Save;
  end;

  // Base Data Struct
  TDBEngineVL = class(THashVariantList)
  protected
    DBStorePos                : Int64;
    DBEng                     : TDBStoreEngine;
    CreateTime, LastModifyTime: TDateTime;
  public
    constructor Create;
    procedure Save;
  end;

  // Base Data Struct
  TDBEngineTE = class(TSectionTextData)
  protected
    DBStorePos                : Int64;
    DBEng                     : TDBStoreEngine;
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
    DBEng                     : TDBStoreEngine;
    CreateTime, LastModifyTime: TDateTime;
  public
    constructor Create;
    procedure Save;
  end;
  {$ENDIF}

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

    procedure LoadFromStoreEngine(DBEng: TDBStoreEngine);
    procedure ExportToStoreEngine(DBEng: TDBStoreEngine);
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

    procedure ImportTextStream(stream: TCoreClassStream);
    procedure ExportTextStream(stream: TCoreClassStream);

    procedure LoadFromStoreEngine(DBEng: TDBStoreEngine);
    procedure ExportToStoreEngine(DBEng: TDBStoreEngine);
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

    procedure LoadFromStoreEngine(DBEng: TDBStoreEngine);
    procedure ExportToStoreEngine(DBEng: TDBStoreEngine);
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

    procedure LoadFromStoreEngine(DBEng: TDBStoreEngine);
    procedure ExportToStoreEngine(DBEng: TDBStoreEngine);
  end;
  {$ENDIF}

  TQueryState = record
    DBEng: TDBStoreEngine;
    StorePos: Int64;
    QueryHnd: TItemSearch;
    Breaked: Boolean;
  end;

  TWaitQueryCall   = procedure(var qState: TQueryState);
  TWaitQueryMethod = procedure(var qState: TQueryState) of object;
  {$IFDEF FPC}
  {$ELSE}
  TWaitQueryProcedure = reference to procedure(var qState: TQueryState);
  {$ENDIF}
  TQueryCall   = procedure(var UserData: Pointer; var qState: TQueryState);
  TQueryMethod = procedure(var UserData: Pointer; var qState: TQueryState) of object;

  {$IFDEF FPC}
  {$ELSE}
  TQueryProcedure = reference to procedure(var UserData: Pointer; var qState: TQueryState);
  {$ENDIF}
  TQueryDoneCall   = procedure(var UserData: Pointer);
  TQueryDoneMethod = procedure(var UserData: Pointer) of object;

  {$IFDEF FPC}
  {$ELSE}
  TQueryDoneProcedure = reference to procedure(var UserData: Pointer);
  {$ENDIF}

  TQueryTask = class(TCoreClassObject)
  private
    FDBEng  : TDBStoreEngine;
    FInited : Boolean;
    FReverse: Boolean;
    FState  : TQueryState;

    FUserData: Pointer;

    {$IFDEF FPC}
    FOnQueryCall      : TQueryCall;
    FOnQueryMethod    : TQueryMethod;
    FOnQueryDoneCall  : TQueryDoneCall;
    FOnQueryDoneMethod: TQueryDoneMethod;
    {$ELSE}
    FOnQueryCall  : TQueryCall;
    FOnQueryMethod: TQueryMethod;
    FOnQueryProc  : TQueryProcedure;

    FOnQueryDoneCall  : TQueryDoneCall;
    FOnQueryDoneMethod: TQueryDoneMethod;
    FOnQueryDoneProc  : TQueryDoneProcedure;
    {$ENDIF}
    procedure DoTriggerQuery;
    procedure DoQueryDone;
  public
    constructor Create;

    function ProcessQuery: Boolean;
  end;

  TQueryThread = class(TCoreClassThread)
  public
    StoreEngine: TDBStoreEngine;
    Paused     : Boolean;
    procedure SyncQuery;
    procedure Execute; override;
    destructor Destroy; override;
  end;

  // store engine
  TDBStoreEngine = class(TCoreClassObject)
  protected
    FDBEngine     : TObjectDataManager;
    FStoreFieldPos: Int64;
    FCount        : Int64;

    FQueryQueue          : TCoreClassListForObj;
    FQueryThread         : TQueryThread;
    FQueryThreadTerminate: Boolean;

    FResultDF: TDBEngineDF;
    FResultVL: TDBEngineVL;
    FResultTE: TDBEngineTE;
    {$IFNDEF FPC}
    FResultJson: TDBEngineJson;
    {$ENDIF}
    procedure ReadHeaderInfo;

    procedure ThreadFreeEvent(Sender: TObject);
    procedure DoCreateInit; virtual;
  public
    constructor Create(DBFile: string; OnlyRead: Boolean);
    constructor CreateMemory(DBMemory: TCoreClassStream; OnlyRead: Boolean);
    constructor CreateNew(DBFile: string);
    constructor CreateNewMemory;
    destructor Destroy; override;

    procedure CompressTo(DestDB: TObjectDataManager);
    procedure Compress;
    procedure Update;

    property DBEngine: TObjectDataManager read FDBEngine;
    property Count: Int64 read FCount;

    function AddData(buff: TCoreClassStream; UserProperty: Cardinal): Int64; overload;
    function SetDataUsedStorePos(const StorePos: Int64; buff: TCoreClassStream): Boolean;
    function GetData(const StorePos: Int64; UserProperty: Cardinal): TItemStream;
    function DeleteData(const StorePos: Int64): Boolean;
    function GetSize(const StorePos: Int64): Int64;

    function QueryFirst(var qs: TQueryState): Boolean;
    function QueryNext(var qs: TQueryState): Boolean;
    function QueryLast(var qs: TQueryState): Boolean;
    function QueryPrev(var qs: TQueryState): Boolean;

    {$IFDEF FPC}
    procedure WaitQuery(ReverseQuery: Boolean;
      const OnQueryCall: TWaitQueryCall;
      const OnQueryMethod: TWaitQueryMethod); overload;
    procedure WaitQuery(ReverseQuery: Boolean; const OnQueryCall: TWaitQueryCall); overload;
    procedure WaitQuery(ReverseQuery: Boolean; const OnQueryMethod: TWaitQueryMethod); overload;
    procedure WaitQuery(const OnQueryCall: TWaitQueryCall); overload;
    procedure WaitQuery(const OnQueryMethod: TWaitQueryMethod); overload;
    {$ELSE}
    procedure WaitQuery(ReverseQuery: Boolean;
      const OnQueryCall: TWaitQueryCall;
      const OnQueryProc: TWaitQueryProcedure;
      const OnQueryMethod: TWaitQueryMethod); overload;
    procedure WaitQuery(ReverseQuery: Boolean; const OnQueryCall: TWaitQueryCall); overload;
    procedure WaitQuery(ReverseQuery: Boolean; const OnQueryMethod: TWaitQueryMethod); overload;
    procedure WaitQuery(ReverseQuery: Boolean; const OnQueryProc: TWaitQueryProcedure); overload;
    procedure WaitQuery(const OnQueryCall: TWaitQueryCall); overload;
    procedure WaitQuery(const OnQueryProc: TWaitQueryProcedure); overload;
    procedure WaitQuery(const OnQueryMethod: TWaitQueryMethod); overload;
    {$ENDIF}
    {$IFDEF FPC}
    procedure Query(const UserData: Pointer; const ReverseQuery: Boolean;
      const OnQueryCall: TQueryCall; const OnQueryDoneCall: TQueryDoneCall;
      const OnQueryMethod: TQueryMethod; const OnQueryDoneMethod: TQueryDoneMethod); overload;

    procedure Query(const UserData: Pointer; const ReverseQuery: Boolean; const OnQueryMethod: TQueryMethod; const OnQueryDoneMethod: TQueryDoneMethod); overload;
    procedure Query(const UserData: Pointer; const ReverseQuery: Boolean; const OnQueryCall: TQueryCall; const OnQueryDoneCall: TQueryDoneCall); overload;
    procedure Query(const UserData: Pointer; const OnQueryCall: TQueryCall; const OnQueryDoneCall: TQueryDoneCall); overload;
    procedure Query(const UserData: Pointer; const OnQueryMethod: TQueryMethod; const OnQueryDoneMethod: TQueryDoneMethod); overload;
    procedure Query(const OnQueryCall: TQueryCall; const OnQueryDoneCall: TQueryDoneCall); overload;
    procedure Query(const OnQueryMethod: TQueryMethod; const OnQueryDoneMethod: TQueryDoneMethod); overload;

    procedure Query(const ReverseQuery: Boolean; const OnQueryCall: TQueryCall; const OnQueryDoneCall: TQueryDoneCall); overload;
    procedure Query(const ReverseQuery: Boolean; const OnQueryMethod: TQueryMethod; const OnQueryDoneMethod: TQueryDoneMethod); overload;
    {$ELSE}
    procedure Query(const UserData: Pointer; const ReverseQuery: Boolean;
      const OnQueryCall: TQueryCall; const OnQueryDoneCall: TQueryDoneCall;
      const OnQueryProc: TQueryProcedure; const OnQueryDoneProc: TQueryDoneProcedure;
      const OnQueryMethod: TQueryMethod; const OnQueryDoneMethod: TQueryDoneMethod); overload;

    procedure Query(const UserData: Pointer; const ReverseQuery: Boolean; const OnQueryMethod: TQueryMethod; const OnQueryDoneMethod: TQueryDoneMethod); overload;
    procedure Query(const UserData: Pointer; const ReverseQuery: Boolean; const OnQueryCall: TQueryCall; const OnQueryDoneCall: TQueryDoneCall); overload;
    procedure Query(const UserData: Pointer; const ReverseQuery: Boolean; const OnQueryProc: TQueryProcedure; const OnQueryDoneProc: TQueryDoneProcedure); overload;
    procedure Query(const UserData: Pointer; const OnQueryCall: TQueryCall; const OnQueryDoneCall: TQueryDoneCall); overload;
    procedure Query(const UserData: Pointer; const OnQueryMethod: TQueryMethod; const OnQueryDoneMethod: TQueryDoneMethod); overload;
    procedure Query(const UserData: Pointer; const OnQueryProc: TQueryProcedure; const OnQueryDoneProc: TQueryDoneProcedure); overload;
    procedure Query(const OnQueryCall: TQueryCall; const OnQueryDoneCall: TQueryDoneCall); overload;
    procedure Query(const OnQueryMethod: TQueryMethod; const OnQueryDoneMethod: TQueryDoneMethod); overload;
    procedure Query(const OnQueryProc: TQueryProcedure; const OnQueryDoneProc: TQueryDoneProcedure); overload;

    procedure Query(const ReverseQuery: Boolean; const OnQueryCall: TQueryCall; const OnQueryDoneCall: TQueryDoneCall); overload;
    procedure Query(const ReverseQuery: Boolean; const OnQueryMethod: TQueryMethod; const OnQueryDoneMethod: TQueryDoneMethod); overload;
    procedure Query(const ReverseQuery: Boolean; const OnQueryProc: TQueryProcedure; const OnQueryDoneProc: TQueryDoneProcedure); overload;
    {$ENDIF}
    procedure WaitQueryThread;

    procedure AddData(buff: TDataFrameEngine); overload;
    function GetDF(const StorePos: Int64): TDBEngineDF; overload;
    function GetDF(var qs: TQueryState): TDBEngineDF; overload;
    function BuildDF(const StorePos: Int64): TDBEngineDF; overload;
    function BuildDF(var qs: TQueryState): TDBEngineDF; overload;

    procedure AddData(buff: THashVariantList); overload;
    function GetVL(const StorePos: Int64): TDBEngineVL; overload;
    function GetVL(var qs: TQueryState): TDBEngineVL; overload;
    function BuildVL(const StorePos: Int64): TDBEngineVL; overload;
    function BuildVL(var qs: TQueryState): TDBEngineVL; overload;

    procedure AddData(buff: TSectionTextData); overload;
    function GetTE(const StorePos: Int64): TDBEngineTE; overload;
    function GetTE(var qs: TQueryState): TDBEngineTE; overload;
    function BuildTE(const StorePos: Int64): TDBEngineTE; overload;
    function BuildTE(var qs: TQueryState): TDBEngineTE; overload;

    {$IFNDEF FPC}
    procedure AddData(buff: TJsonObject); overload;
    function GetJson(const StorePos: Int64): TDBEngineJson; overload;
    function GetJson(var qs: TQueryState): TDBEngineJson; overload;
    function BuildJson(const StorePos: Int64): TDBEngineJson; overload;
    function BuildJson(var qs: TQueryState): TDBEngineJson; overload;
    {$ENDIF}
  end;

const
  c_DF   = 100;
  c_VL   = 200;
  c_TE   = 300;
  c_Json = 400;

implementation

constructor TDBEngineDF.Create;
begin
  inherited Create;
  DBStorePos := -1;
  DBEng := nil;
  CreateTime := Now;
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
  DBEng.SetDataUsedStorePos(DBStorePos, m);
  DisposeObject(m);
end;

constructor TDBEngineVL.Create;
begin
  inherited Create;
  DBStorePos := -1;
  DBEng := nil;
  CreateTime := Now;
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
  DBEng.SetDataUsedStorePos(DBStorePos, m);
  DisposeObject(m);
end;

constructor TDBEngineTE.Create;
begin
  inherited Create;
  DBStorePos := -1;
  DBEng := nil;
  CreateTime := Now;
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
  DBEng.SetDataUsedStorePos(DBStorePos, m);
  DisposeObject(m);
end;

{$IFNDEF FPC}


constructor TDBEngineJson.Create;
begin
  inherited Create;
  DBStorePos := -1;
  DBEng := nil;
  CreateTime := Now;
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
  DBEng.SetDataUsedStorePos(DBStorePos, m);
  DisposeObject(m);
end;
{$ENDIF}


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

procedure TDBListDF.LoadFromStoreEngine(DBEng: TDBStoreEngine);
var
  qs: TQueryState;
begin
  Clear;
  if DBEng.QueryFirst(qs) then
    begin
      repeat
          FHashListBuff.Add(DBEng.BuildDF(qs.StorePos));
      until not DBEng.QueryNext(qs);
    end;
end;

procedure TDBListDF.ExportToStoreEngine(DBEng: TDBStoreEngine);
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

procedure TDBListVL.ImportTextStream(stream: TCoreClassStream);
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
    sour.LoadFromStream(stream);
    {$ELSE}
    sour.LoadFromStream(stream, TEncoding.UTF8);
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
          TextName := umlGetFirstStr_M(n, ':=');
          if TextName.Len > 0 then
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

procedure TDBListVL.ExportTextStream(stream: TCoreClassStream);
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

              if s.Len > 0 then
                  n.Text := PHashListData(ls[j])^.OriginName + '=' + s.Text
              else
                  n.Text := PHashListData(ls[j])^.OriginName;

              b := b + n + lineBreak;
            end;

          b := b + lineBreak;
          buff := b.Bytes;
          stream.Write(buff, Length(buff));
          b := '';
        end;
    end;

  DisposeObject([ls]);
end;

procedure TDBListVL.LoadFromStoreEngine(DBEng: TDBStoreEngine);
var
  qs: TQueryState;
begin
  Clear;
  if DBEng.QueryFirst(qs) then
    begin
      repeat
          FHashListBuff.Add(DBEng.BuildVL(qs.StorePos));
      until not DBEng.QueryNext(qs);
    end;
end;

procedure TDBListVL.ExportToStoreEngine(DBEng: TDBStoreEngine);
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

procedure TDBListTE.LoadFromStoreEngine(DBEng: TDBStoreEngine);
var
  qs: TQueryState;
begin
  Clear;
  if DBEng.QueryFirst(qs) then
    begin
      repeat
          FHashListBuff.Add(DBEng.BuildTE(qs.StorePos));
      until not DBEng.QueryNext(qs);
    end;
end;

procedure TDBListTE.ExportToStoreEngine(DBEng: TDBStoreEngine);
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

procedure TDBListJson.LoadFromStoreEngine(DBEng: TDBStoreEngine);
begin
  Clear;
  DBEng.WaitQuery(False,
    procedure(var qs: TQueryState)
    begin
      FHashListBuff.Add(DBEng.BuildJson(qs.StorePos));
    end);
end;

procedure TDBListJson.ExportToStoreEngine(DBEng: TDBStoreEngine);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      DBEng.AddData(GetItems(i));
end;
{$ENDIF}


procedure TQueryTask.DoTriggerQuery;
begin
  try
    {$IFDEF FPC}
    if Assigned(FOnQueryCall) then
        FOnQueryCall(FUserData, FState);
    if Assigned(FOnQueryMethod) then
        FOnQueryMethod(FUserData, FState);
    {$ELSE}
    if Assigned(FOnQueryCall) then
        FOnQueryCall(FUserData, FState);
    if Assigned(FOnQueryMethod) then
        FOnQueryMethod(FUserData, FState);
    if Assigned(FOnQueryProc) then
        FOnQueryProc(FUserData, FState);
    {$ENDIF}
  except
  end;
end;

procedure TQueryTask.DoQueryDone;
begin
  try
    {$IFDEF FPC}
    if Assigned(FOnQueryDoneCall) then
        FOnQueryDoneCall(FUserData);
    if Assigned(FOnQueryDoneMethod) then
        FOnQueryDoneMethod(FUserData);
    {$ELSE}
    if Assigned(FOnQueryDoneCall) then
        FOnQueryDoneCall(FUserData);
    if Assigned(FOnQueryDoneMethod) then
        FOnQueryDoneMethod(FUserData);
    if Assigned(FOnQueryDoneProc) then
        FOnQueryDoneProc(FUserData);
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
  FState.StorePos := 0;
  InitTTMDBSearchItem(FState.QueryHnd);
  FState.Breaked := False;

  FUserData := nil;

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

function TQueryTask.ProcessQuery: Boolean;
begin
  Result := False;
  if FInited then
    begin
      if FReverse then
        begin
          if not FDBEng.QueryPrev(FState) then
            begin
              DoQueryDone;
              exit;
            end;
          DoTriggerQuery;
          Result := not FState.Breaked;
        end
      else
        begin
          if not FDBEng.QueryNext(FState) then
            begin
              DoQueryDone;
              exit;
            end;
          DoTriggerQuery;
          Result := not FState.Breaked;
        end;
    end
  else
    begin
      if FReverse then
        begin
          if not FDBEng.QueryLast(FState) then
            begin
              DoQueryDone;
              exit;
            end;
          DoTriggerQuery;
          Result := not FState.Breaked;
        end
      else
        begin
          if not FDBEng.QueryFirst(FState) then
            begin
              DoQueryDone;
              exit;
            end;
          DoTriggerQuery;
          Result := not FState.Breaked;
        end;

      if Result then
          FInited := True;
    end;
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
          DisposeObject(qt);
          StoreEngine.FQueryQueue.Delete(i);
        end
      else
          inc(i);
    end;

  Paused := StoreEngine.FQueryQueue.Count = 0;
end;

procedure TQueryThread.Execute;
begin
  while StoreEngine <> nil do
    begin
      while Paused do
          Sleep(10);

      {$IFDEF FPC}
      Synchronize(Self, @SyncQuery);
      {$ELSE}
      Synchronize(Self, SyncQuery);
      {$ENDIF}
    end;
end;

destructor TQueryThread.Destroy;
begin
  inherited Destroy;
end;

procedure TDBStoreEngine.ReadHeaderInfo;
var
  f: TFieldHandle;
begin
  if not FDBEngine.GetPathField('/Store', FStoreFieldPos) then
      RaiseInfo('no exists store field');

  if not FDBEngine.GetFieldData(FStoreFieldPos, f) then
      RaiseInfo('store field data failed!');

  FCount := f.HeaderCount;
end;

procedure TDBStoreEngine.ThreadFreeEvent(Sender: TObject);
begin
  FQueryThreadTerminate := True;
end;

procedure TDBStoreEngine.DoCreateInit;
begin
  FQueryQueue := TCoreClassListForObj.Create;

  FQueryThread := TQueryThread.Create(True);
  FQueryThread.StoreEngine := Self;
  FQueryThread.Paused := True;
  FQueryThread.FreeOnTerminate := True;
  FQueryThreadTerminate := False;

  {$IFDEF FPC}
  FQueryThread.OnTerminate := @ThreadFreeEvent;
  {$ELSE}
  FQueryThread.OnTerminate := ThreadFreeEvent;
  {$ENDIF}
  FResultDF := TDBEngineDF.Create;
  FResultVL := TDBEngineVL.Create;
  FResultTE := TDBEngineTE.Create;
  {$IFNDEF FPC}
  FResultJson := TDBEngineJson.Create;
  {$ENDIF}
  FQueryThread.Suspended := False;
end;

constructor TDBStoreEngine.Create(DBFile: string; OnlyRead: Boolean);
begin
  inherited Create;
  FDBEngine := TObjectDataManager.Create(DBFile, ObjectDataMarshal.id, OnlyRead);
  ReadHeaderInfo;

  DoCreateInit;
end;

constructor TDBStoreEngine.CreateMemory(DBMemory: TCoreClassStream; OnlyRead: Boolean);
begin
  inherited Create;
  FDBEngine := TObjectDataManager.CreateAsStream(DBMemory, '', ObjectDataMarshal.id, OnlyRead, False, True);
  ReadHeaderInfo;

  DoCreateInit;
end;

constructor TDBStoreEngine.CreateNew(DBFile: string);
begin
  inherited Create;
  FDBEngine := TObjectDataManager.CreateNew(DBFile, ObjectDataMarshal.id);
  FDBEngine.CreateDir('/Store', '');
  ReadHeaderInfo;

  DoCreateInit;
end;

constructor TDBStoreEngine.CreateNewMemory;
begin
  inherited Create;
  FDBEngine := TObjectDataManager.CreateAsStream(TMemoryStream64.Create, '', ObjectDataMarshal.id, False, True, True);
  FDBEngine.CreateDir('/Store', '');
  ReadHeaderInfo;

  DoCreateInit;
end;

destructor TDBStoreEngine.Destroy;
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
  DisposeObject([FDBEngine, FQueryQueue, FResultDF, FResultVL, FResultTE]);
  {$IFNDEF FPC}
  DisposeObject(FResultJson);
  {$ENDIF}
  inherited Destroy;
end;

procedure TDBStoreEngine.CompressTo(DestDB: TObjectDataManager);
begin
  DoStatus('build struct...');
  DestDB.CreateDir('/Store', '');

  DoStatus('compress data...');
  FDBEngine.CopyFieldToPath(FStoreFieldPos, DestDB, '/Store');

  DestDB.Update;
  DoStatus('build finish...', []);
end;

procedure TDBStoreEngine.Compress;
var
  DestDB   : TObjectDataManager;
  fn, oldFN: string;
  i        : Integer;
begin
  if FDBEngine.StreamEngine <> nil then
    begin
      DestDB := TObjectDataManager.CreateAsStream(TMemoryStream64.Create, '', ObjectDataMarshal.id, False, True, True);
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
      DestDB := TObjectDataManager.CreateNew(fn, ObjectDataMarshal.id);
      CompressTo(DestDB);
      DisposeObject([FDBEngine, DestDB]);

      umlDeleteFile(oldFN);
      umlRenameFile(fn, oldFN);

      FDBEngine := TObjectDataManager.Create(oldFN, ObjectDataMarshal.id, False);
      ReadHeaderInfo;
    end;

  FResultDF.DBEng := nil;
  FResultDF.DBStorePos := -1;
  FResultVL.DBEng := nil;
  FResultVL.DBStorePos := -1;
  FResultTE.DBEng := nil;
  FResultTE.DBStorePos := -1;
end;

procedure TDBStoreEngine.Update;
begin
  FDBEngine.Update;
end;

function TDBStoreEngine.AddData(buff: TCoreClassStream; UserProperty: Cardinal): Int64;
var
  itmHnd   : TItemHandle;
  itmStream: TItemStream;
begin
  Result := -1;

  if FDBEngine.ItemFastCreate(FStoreFieldPos, '', '', itmHnd) then
    begin
      itmHnd.Item.RHeader.UserProperty := UserProperty;
      itmHnd.Name := '0x' + IntToHex(itmHnd.Item.RHeader.CurrentHeader, 16);
      itmStream := TItemStream.Create(FDBEngine, itmHnd);
      buff.Position := 0;
      itmStream.CopyFrom(buff, buff.Size);
      itmStream.UpdateHandle;
      DisposeObject(itmStream);
      Result := itmHnd.Item.RHeader.CurrentHeader;
      inc(FCount);
    end;
end;

function TDBStoreEngine.SetDataUsedStorePos(const StorePos: Int64; buff: TCoreClassStream): Boolean;
var
  itmHnd   : TItemHandle;
  itmStream: TItemStream;
begin
  Result := False;

  if FDBEngine.ItemFastResetBody(StorePos) then
    if FDBEngine.ItemFastOpen(StorePos, itmHnd) then
      begin
        itmStream := TItemStream.Create(FDBEngine, itmHnd);
        buff.Position := 0;
        itmStream.CopyFrom(buff, buff.Size);
        itmStream.UpdateHandle;
        DisposeObject(itmStream);

        Result := True;
      end;
end;

function TDBStoreEngine.GetData(const StorePos: Int64; UserProperty: Cardinal): TItemStream;
var
  itmHnd: TItemHandle;
begin
  Result := nil;

  if FDBEngine.ItemFastOpen(StorePos, itmHnd) then
    begin
      if UserProperty = itmHnd.Item.RHeader.UserProperty then
          Result := TItemStream.Create(FDBEngine, itmHnd);
    end;
end;

function TDBStoreEngine.DeleteData(const StorePos: Int64): Boolean;
begin
  Result := FDBEngine.FastDelete(FStoreFieldPos, StorePos);
  if Result then
      Dec(FCount);
end;

function TDBStoreEngine.GetSize(const StorePos: Int64): Int64;
var
  itmHnd: TItemHandle;
begin
  Result := -1;
  if DBEngine.ItemFastOpen(StorePos, itmHnd) then
      Result := itmHnd.Item.Size;
end;

function TDBStoreEngine.QueryFirst(var qs: TQueryState): Boolean;
begin
  qs.DBEng := Self;
  qs.StorePos := -1;
  qs.Breaked := False;
  Result := FDBEngine.ItemFastFindFirst(FStoreFieldPos, '*', qs.QueryHnd);
  if Result then
      qs.StorePos := qs.QueryHnd.HeaderPOS;
end;

function TDBStoreEngine.QueryNext(var qs: TQueryState): Boolean;
begin
  Result := FDBEngine.ItemFastFindNext(qs.QueryHnd);
  if Result then
      qs.StorePos := qs.QueryHnd.HeaderPOS;
end;

function TDBStoreEngine.QueryLast(var qs: TQueryState): Boolean;
begin
  qs.DBEng := Self;
  qs.StorePos := -1;
  qs.Breaked := False;
  Result := FDBEngine.ItemFastFindLast(FStoreFieldPos, '*', qs.QueryHnd);
  if Result then
      qs.StorePos := qs.QueryHnd.HeaderPOS;
end;

function TDBStoreEngine.QueryPrev(var qs: TQueryState): Boolean;
begin
  Result := FDBEngine.ItemFastFindPrev(qs.QueryHnd);
  if Result then
      qs.StorePos := qs.QueryHnd.HeaderPOS;
end;

{$IFDEF FPC}


procedure TDBStoreEngine.WaitQuery(ReverseQuery: Boolean; const OnQueryCall: TWaitQueryCall; const OnQueryMethod: TWaitQueryMethod);
type
  TDynamicQueryMethod = function(var qs: TQueryState): Boolean of object;
var
  qs  : TQueryState;
  f, n: TDynamicQueryMethod;
begin
  qs.StorePos := -1;
  qs.Breaked := False;

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

  if f(qs) then
    begin
      repeat
        try
          if Assigned(OnQueryCall) then
              OnQueryCall(qs);
          if Assigned(OnQueryMethod) then
              OnQueryMethod(qs);
        except
        end;

        if qs.Breaked then
            break;

      until (not n(qs));
    end;
end;
{$ELSE}


procedure TDBStoreEngine.WaitQuery(ReverseQuery: Boolean; const OnQueryCall: TWaitQueryCall; const OnQueryProc: TWaitQueryProcedure; const OnQueryMethod: TWaitQueryMethod);
type
  TDynamicQueryMethod = function(var qs: TQueryState): Boolean of object;
var
  qs  : TQueryState;
  f, n: TDynamicQueryMethod;
begin
  qs.StorePos := -1;
  qs.Breaked := False;

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

  if f(qs) then
    begin
      repeat
        try
          if Assigned(OnQueryCall) then
              OnQueryCall(qs);
          if Assigned(OnQueryProc) then
              OnQueryProc(qs);
          if Assigned(OnQueryMethod) then
              OnQueryMethod(qs);
        except
        end;

        if qs.Breaked then
            break;

      until (not n(qs));
    end;
end;
{$ENDIF}


procedure TDBStoreEngine.WaitQuery(ReverseQuery: Boolean; const OnQueryCall: TWaitQueryCall);
begin
  {$IFDEF FPC}
  WaitQuery(ReverseQuery, OnQueryCall, nil);
  {$ELSE}
  WaitQuery(ReverseQuery, OnQueryCall, nil, nil);
  {$ENDIF}
end;

procedure TDBStoreEngine.WaitQuery(ReverseQuery: Boolean; const OnQueryMethod: TWaitQueryMethod);
begin
  {$IFDEF FPC}
  WaitQuery(ReverseQuery, nil, OnQueryMethod);
  {$ELSE}
  WaitQuery(ReverseQuery, nil, nil, OnQueryMethod);
  {$ENDIF}
end;

{$IFNDEF FPC}


procedure TDBStoreEngine.WaitQuery(ReverseQuery: Boolean; const OnQueryProc: TWaitQueryProcedure);
begin
  WaitQuery(ReverseQuery, nil, OnQueryProc, nil);
end;
{$ENDIF}


procedure TDBStoreEngine.WaitQuery(const OnQueryCall: TWaitQueryCall);
begin
  WaitQuery(False, OnQueryCall);
end;

procedure TDBStoreEngine.WaitQuery(const OnQueryMethod: TWaitQueryMethod);
begin
  WaitQuery(False, OnQueryMethod);
end;

{$IFNDEF FPC}


procedure TDBStoreEngine.WaitQuery(const OnQueryProc: TWaitQueryProcedure);
begin
  WaitQuery(False, OnQueryProc);
end;

{$ENDIF}

{$IFDEF FPC}


procedure TDBStoreEngine.Query(const UserData: Pointer; const ReverseQuery: Boolean;
const OnQueryCall: TQueryCall; const OnQueryDoneCall: TQueryDoneCall;
const OnQueryMethod: TQueryMethod; const OnQueryDoneMethod: TQueryDoneMethod);
var
  qt: TQueryTask;
begin
  qt := TQueryTask.Create;
  qt.FDBEng := Self;
  qt.FReverse := ReverseQuery;
  qt.FUserData := UserData;
  qt.FOnQueryCall := OnQueryCall;
  qt.FOnQueryDoneCall := OnQueryDoneCall;
  qt.FOnQueryMethod := OnQueryMethod;
  qt.FOnQueryDoneMethod := OnQueryDoneMethod;
  FQueryQueue.Add(qt);
  FQueryThread.Paused := False;
end;

procedure TDBStoreEngine.Query(const UserData: Pointer; const ReverseQuery: Boolean; const OnQueryCall: TQueryCall; const OnQueryDoneCall: TQueryDoneCall);
begin
  Query(UserData, ReverseQuery, OnQueryCall, OnQueryDoneCall, nil, nil);
end;

procedure TDBStoreEngine.Query(const UserData: Pointer; const ReverseQuery: Boolean; const OnQueryMethod: TQueryMethod; const OnQueryDoneMethod: TQueryDoneMethod);
begin
  Query(UserData, ReverseQuery, nil, nil, OnQueryMethod, OnQueryDoneMethod);
end;

procedure TDBStoreEngine.Query(const UserData: Pointer; const OnQueryCall: TQueryCall; const OnQueryDoneCall: TQueryDoneCall);
begin
  Query(UserData, False, OnQueryCall, OnQueryDoneCall);
end;

procedure TDBStoreEngine.Query(const UserData: Pointer; const OnQueryMethod: TQueryMethod; const OnQueryDoneMethod: TQueryDoneMethod);
begin
  Query(UserData, False, OnQueryMethod, OnQueryDoneMethod);
end;

procedure TDBStoreEngine.Query(const OnQueryCall: TQueryCall; const OnQueryDoneCall: TQueryDoneCall);
begin
  Query(nil, OnQueryCall, OnQueryDoneCall);
end;

procedure TDBStoreEngine.Query(const OnQueryMethod: TQueryMethod; const OnQueryDoneMethod: TQueryDoneMethod);
begin
  Query(nil, OnQueryMethod, OnQueryDoneMethod);
end;

procedure TDBStoreEngine.Query(const ReverseQuery: Boolean; const OnQueryCall: TQueryCall; const OnQueryDoneCall: TQueryDoneCall);
begin
  Query(nil, ReverseQuery, OnQueryCall, OnQueryDoneCall);
end;

procedure TDBStoreEngine.Query(const ReverseQuery: Boolean; const OnQueryMethod: TQueryMethod; const OnQueryDoneMethod: TQueryDoneMethod);
begin
  Query(nil, ReverseQuery, OnQueryMethod, OnQueryDoneMethod);
end;

{$ELSE}


procedure TDBStoreEngine.Query(const UserData: Pointer; const ReverseQuery: Boolean;
const OnQueryCall: TQueryCall; const OnQueryDoneCall: TQueryDoneCall;
const OnQueryProc: TQueryProcedure; const OnQueryDoneProc: TQueryDoneProcedure;
const OnQueryMethod: TQueryMethod; const OnQueryDoneMethod: TQueryDoneMethod);
var
  qt: TQueryTask;
begin
  qt := TQueryTask.Create;
  qt.FDBEng := Self;
  qt.FReverse := ReverseQuery;
  qt.FUserData := UserData;
  qt.FOnQueryCall := OnQueryCall;
  qt.FOnQueryDoneCall := OnQueryDoneCall;
  qt.FOnQueryProc := OnQueryProc;
  qt.FOnQueryDoneProc := OnQueryDoneProc;
  qt.FOnQueryMethod := OnQueryMethod;
  qt.FOnQueryDoneMethod := OnQueryDoneMethod;
  FQueryQueue.Add(qt);
  FQueryThread.Paused := False;
end;

procedure TDBStoreEngine.Query(const UserData: Pointer; const ReverseQuery: Boolean; const OnQueryCall: TQueryCall; const OnQueryDoneCall: TQueryDoneCall);
begin
  Query(UserData, ReverseQuery, OnQueryCall, OnQueryDoneCall, nil, nil, nil, nil);
end;

procedure TDBStoreEngine.Query(const UserData: Pointer; const ReverseQuery: Boolean; const OnQueryProc: TQueryProcedure; const OnQueryDoneProc: TQueryDoneProcedure);
begin
  Query(UserData, ReverseQuery, nil, nil, OnQueryProc, OnQueryDoneProc, nil, nil);
end;

procedure TDBStoreEngine.Query(const UserData: Pointer; const ReverseQuery: Boolean; const OnQueryMethod: TQueryMethod; const OnQueryDoneMethod: TQueryDoneMethod);
begin
  Query(UserData, ReverseQuery, nil, nil, nil, nil, OnQueryMethod, OnQueryDoneMethod);
end;

procedure TDBStoreEngine.Query(const UserData: Pointer; const OnQueryCall: TQueryCall; const OnQueryDoneCall: TQueryDoneCall);
begin
  Query(UserData, False, OnQueryCall, OnQueryDoneCall);
end;

procedure TDBStoreEngine.Query(const UserData: Pointer; const OnQueryProc: TQueryProcedure; const OnQueryDoneProc: TQueryDoneProcedure);
begin
  Query(UserData, False, OnQueryProc, OnQueryDoneProc);
end;

procedure TDBStoreEngine.Query(const UserData: Pointer; const OnQueryMethod: TQueryMethod; const OnQueryDoneMethod: TQueryDoneMethod);
begin
  Query(UserData, False, OnQueryMethod, OnQueryDoneMethod);
end;

procedure TDBStoreEngine.Query(const OnQueryCall: TQueryCall; const OnQueryDoneCall: TQueryDoneCall);
begin
  Query(nil, OnQueryCall, OnQueryDoneCall);
end;

procedure TDBStoreEngine.Query(const OnQueryProc: TQueryProcedure; const OnQueryDoneProc: TQueryDoneProcedure);
begin
  Query(nil, OnQueryProc, OnQueryDoneProc);
end;

procedure TDBStoreEngine.Query(const ReverseQuery: Boolean; const OnQueryCall: TQueryCall; const OnQueryDoneCall: TQueryDoneCall);
begin
  Query(nil, ReverseQuery, OnQueryCall, OnQueryDoneCall);
end;

procedure TDBStoreEngine.Query(const ReverseQuery: Boolean; const OnQueryMethod: TQueryMethod; const OnQueryDoneMethod: TQueryDoneMethod);
begin
  Query(nil, ReverseQuery, OnQueryMethod, OnQueryDoneMethod);
end;

procedure TDBStoreEngine.Query(const ReverseQuery: Boolean; const OnQueryProc: TQueryProcedure; const OnQueryDoneProc: TQueryDoneProcedure);
begin
  Query(nil, ReverseQuery, OnQueryProc, OnQueryDoneProc);
end;

procedure TDBStoreEngine.Query(const OnQueryMethod: TQueryMethod; const OnQueryDoneMethod: TQueryDoneMethod);
begin
  Query(nil, OnQueryMethod, OnQueryDoneMethod);
end;

{$ENDIF}


procedure TDBStoreEngine.WaitQueryThread;
begin
  while not FQueryThread.Paused do
      Classes.CheckSynchronize;
end;

procedure TDBStoreEngine.AddData(buff: TDataFrameEngine);
var
  m: TMemoryStream64;
begin
  m := TMemoryStream64.Create;

  buff.EncodeTo(m, False);

  AddData(m, c_DF);
  DisposeObject(m);
end;

function TDBStoreEngine.GetDF(const StorePos: Int64): TDBEngineDF;
var
  m: TItemStream;
begin
  Result := FResultDF;
  if (Result.DBStorePos > 0) and (Result.DBEng = Self) then
      exit;

  Result.DBStorePos := -1;
  Result.DBEng := nil;
  Result.CreateTime := Now;
  Result.LastModifyTime := Result.CreateTime;

  FResultDF.Clear;
  m := GetData(StorePos, c_DF);
  if m <> nil then
    begin
      m.Position := 0;
      try
        Result.DecodeFrom(m, True);
        Result.DBStorePos := StorePos;
        Result.DBEng := Self;
        with m.Hnd^ do
          begin
            Result.CreateTime := CreateTime;
            Result.CreateTime := LastModifyTime;
          end;
      except
      end;
    end;
  DisposeObject(m);
end;

function TDBStoreEngine.GetDF(var qs: TQueryState): TDBEngineDF;
begin
  Result := GetDF(qs.StorePos);
end;

function TDBStoreEngine.BuildDF(const StorePos: Int64): TDBEngineDF;
var
  m: TItemStream;
begin
  if FResultDF.DBStorePos = StorePos then
    begin
      Result := FResultDF;
      Result.Reader.index := 0;

      FResultDF := TDBEngineDF.Create;
      exit;
    end;

  Result := nil;
  m := GetData(StorePos, c_DF);
  if m <> nil then
    begin
      m.Position := 0;
      Result := TDBEngineDF.Create;
      try
        Result.DecodeFrom(m, True);
        Result.DBStorePos := StorePos;
        Result.DBEng := Self;
        with m.Hnd^ do
          begin
            Result.CreateTime := CreateTime;
            Result.CreateTime := LastModifyTime;
          end;
      except
      end;
    end;
  DisposeObject(m);
end;

function TDBStoreEngine.BuildDF(var qs: TQueryState): TDBEngineDF;
begin
  Result := BuildDF(qs.StorePos);
end;

procedure TDBStoreEngine.AddData(buff: THashVariantList);
var
  m: TMemoryStream64;
begin
  m := TMemoryStream64.Create;
  buff.SaveToStream(m);
  AddData(m, c_VL);
  DisposeObject(m);
end;

function TDBStoreEngine.GetVL(const StorePos: Int64): TDBEngineVL;
var
  m: TItemStream;
begin
  Result := FResultVL;
  if (Result.DBStorePos > 0) and (Result.DBEng = Self) then
      exit;

  Result.DBStorePos := -1;
  Result.DBEng := nil;
  Result.CreateTime := Now;
  Result.LastModifyTime := Result.CreateTime;

  FResultDF.Clear;
  m := GetData(StorePos, c_VL);
  if m <> nil then
    begin
      m.Position := 0;
      try
        Result.LoadFromStream(m);
        Result.DBStorePos := StorePos;
        Result.DBEng := Self;
        with m.Hnd^ do
          begin
            Result.CreateTime := CreateTime;
            Result.CreateTime := LastModifyTime;
          end;
      except
      end;
    end;
  DisposeObject(m);
end;

function TDBStoreEngine.GetVL(var qs: TQueryState): TDBEngineVL;
begin
  Result := GetVL(qs.StorePos);
end;

function TDBStoreEngine.BuildVL(const StorePos: Int64): TDBEngineVL;
var
  m: TItemStream;
begin
  if FResultVL.DBStorePos = StorePos then
    begin
      Result := FResultVL;

      FResultVL := TDBEngineVL.Create;
      exit;
    end;

  Result := nil;
  m := GetData(StorePos, c_VL);
  if m <> nil then
    begin
      m.Position := 0;
      Result := TDBEngineVL.Create;
      try
        Result.LoadFromStream(m);
        Result.DBStorePos := StorePos;
        Result.DBEng := Self;
        with m.Hnd^ do
          begin
            Result.CreateTime := CreateTime;
            Result.CreateTime := LastModifyTime;
          end;
      except
      end;
    end;
  DisposeObject(m);
end;

function TDBStoreEngine.BuildVL(var qs: TQueryState): TDBEngineVL;
begin
  Result := BuildVL(qs.StorePos);
end;

procedure TDBStoreEngine.AddData(buff: TSectionTextData);
var
  m: TMemoryStream64;
begin
  m := TMemoryStream64.Create;
  buff.SaveToStream(m);
  AddData(m, c_TE);
  DisposeObject(m);
end;

function TDBStoreEngine.GetTE(const StorePos: Int64): TDBEngineTE;
var
  m: TItemStream;
begin
  Result := FResultTE;
  if (Result.DBStorePos > 0) and (Result.DBEng = Self) then
      exit;

  Result.DBStorePos := -1;
  Result.DBEng := nil;
  Result.CreateTime := Now;
  Result.LastModifyTime := Result.CreateTime;

  FResultDF.Clear;
  m := GetData(StorePos, c_TE);
  if m <> nil then
    begin
      m.Position := 0;
      try
        Result.LoadFromStream(m);
        Result.DBStorePos := StorePos;
        Result.DBEng := Self;
        with m.Hnd^ do
          begin
            Result.CreateTime := CreateTime;
            Result.CreateTime := LastModifyTime;
          end;
      except
      end;
    end;
  DisposeObject(m);
end;

function TDBStoreEngine.GetTE(var qs: TQueryState): TDBEngineTE;
begin
  Result := GetTE(qs.StorePos);
end;

function TDBStoreEngine.BuildTE(const StorePos: Int64): TDBEngineTE;
var
  m: TItemStream;
begin
  if FResultTE.DBStorePos = StorePos then
    begin
      Result := FResultTE;

      FResultTE := TDBEngineTE.Create;
      exit;
    end;
  Result := nil;
  m := GetData(StorePos, c_TE);
  if m <> nil then
    begin
      m.Position := 0;
      Result := TDBEngineTE.Create;
      try
        Result.LoadFromStream(m);
        Result.DBStorePos := StorePos;
        Result.DBEng := Self;
        with m.Hnd^ do
          begin
            Result.CreateTime := CreateTime;
            Result.CreateTime := LastModifyTime;
          end;
      except
      end;
    end;
  DisposeObject(m);
end;

function TDBStoreEngine.BuildTE(var qs: TQueryState): TDBEngineTE;
begin
  Result := BuildTE(qs.StorePos);
end;

{$IFNDEF FPC}


procedure TDBStoreEngine.AddData(buff: TJsonObject);
var
  m: TMemoryStream64;
begin
  m := TMemoryStream64.Create;
  buff.SaveToStream(m, True, TEncoding.UTF8, True);
  AddData(m, c_Json);
  DisposeObject(m);
end;

function TDBStoreEngine.GetJson(const StorePos: Int64): TDBEngineJson;
var
  m: TItemStream;
begin
  Result := FResultJson;
  if (Result.DBStorePos > 0) and (Result.DBEng = Self) then
      exit;

  Result.DBStorePos := -1;
  Result.DBEng := nil;
  Result.CreateTime := Now;
  Result.LastModifyTime := Result.CreateTime;

  FResultDF.Clear;
  m := GetData(StorePos, c_Json);
  if m <> nil then
    begin
      m.Position := 0;
      try
        Result.LoadFromStream(m, TEncoding.UTF8, True, nil);
        Result.DBStorePos := StorePos;
        Result.DBEng := Self;
        with m.Hnd^ do
          begin
            Result.CreateTime := CreateTime;
            Result.CreateTime := LastModifyTime;
          end;
      except
      end;
    end;
  DisposeObject(m);
end;

function TDBStoreEngine.GetJson(var qs: TQueryState): TDBEngineJson;
begin
  Result := GetJson(qs.StorePos);
end;

function TDBStoreEngine.BuildJson(const StorePos: Int64): TDBEngineJson;
var
  m: TItemStream;
begin
  if FResultJson.DBStorePos = StorePos then
    begin
      Result := FResultJson;

      FResultJson := TDBEngineJson.Create;
      exit;
    end;
  Result := nil;
  m := GetData(StorePos, c_Json);
  if m <> nil then
    begin
      m.Position := 0;
      Result := TDBEngineJson.Create;
      try
        Result.LoadFromStream(m, TEncoding.UTF8, True, nil);
        Result.DBStorePos := StorePos;
        Result.DBEng := Self;
        with m.Hnd^ do
          begin
            Result.CreateTime := CreateTime;
            Result.CreateTime := LastModifyTime;
          end;
      except
      end;
    end;
  DisposeObject(m);
end;

function TDBStoreEngine.BuildJson(var qs: TQueryState): TDBEngineJson;
begin
  Result := BuildJson(qs.StorePos);
end;
{$ENDIF}

end.
