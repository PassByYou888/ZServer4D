{ ****************************************************************************** }
{ * DataStore Service common unit                                              * }
{ * written by QQ 600585@qq.com                                                * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ ****************************************************************************** }
(*
  update history
*)
unit CommunicationFrameworkDataStoreServiceCommon;

{$I zDefine.inc}

interface

uses Variants, CoreClasses, CommunicationFramework, PascalStrings, ZDBEngine, ZDBLocalManager, MemoryStream64,
  DataFrameEngine;

type
  TTDataStoreService_DBPipeline = class(TZDBPipeline)
  public
    SendTunnel   : TPeerClientUserDefine;
    RecvTunnel   : TPeerClientUserDefine;
    BackcallPtr  : UInt64;
    SyncToClient : Boolean;
    RegistedQuery: SystemString;

    constructor Create(InMem: Boolean; AOwner: TZDBLocalManager; sourDBName, APipelineN, OutDBName: SystemString); override;
    destructor Destroy; override;

    procedure Progress(deltaTime: Double); override;
  end;

  TTDataStoreService_QueryCall = class(TCoreClassObject)
  private
  public
    OnPipelineQuery    : TZDBPipelineFilterMethod;
    OnPipelineQueryDone: TZDBPipelineDoneMethod;
    constructor Create;
  end;

  TUserQueryDoneNotifyCall = procedure(UserPointer: Pointer; UserObject: TCoreClassObject; UserVariant: Variant;
    dbN, outN, pipeN: SystemString; TotalResult: Int64);
  TUserQueryDoneNotifyMethod = procedure(UserPointer: Pointer; UserObject: TCoreClassObject; UserVariant: Variant;
    dbN, outN, pipeN: SystemString; TotalResult: Int64) of object;
  {$IFNDEF FPC}
  TUserQueryDoneNotifyProc = reference to procedure(UserPointer: Pointer; UserObject: TCoreClassObject; UserVariant: Variant;
    dbN, outN, pipeN: SystemString; TotalResult: Int64);
  {$ENDIF}
  //
  TQueryDoneNotifyCall   = procedure(dbN, outN, pipeN: SystemString; TotalResult: Int64);
  TQueryDoneNotifyMethod = procedure(dbN, outN, pipeN: SystemString; TotalResult: Int64) of object;
  {$IFNDEF FPC}
  TQueryDoneNotifyProc = reference to procedure(dbN, outN, pipeN: SystemString; TotalResult: Int64);
  {$ENDIF}
  //
  PDataStoreClientQueryNotify = ^TDataStoreClientQueryNotify;

  TDataStoreClientQueryNotify = packed record
    UserPointer: Pointer;
    UserObject: TCoreClassObject;
    UserVariant: Variant;

    OnQueryCall: TFillQueryDataCall;
    OnQueryMethod: TFillQueryDataMethod;
    {$IFNDEF FPC}
    OnQueryProc: TFillQueryDataProc;
    {$ENDIF}
    //
    OnUserQueryCall: TUserFillQueryDataCall;
    OnUserQueryMethod: TUserFillQueryDataMethod;
    {$IFNDEF FPC}
    OnUserQueryProc: TUserFillQueryDataProc;
    {$ENDIF}
    //
    OnDoneCall: TQueryDoneNotifyCall;
    OnDoneMethod: TQueryDoneNotifyMethod;
    {$IFNDEF FPC}
    OnDoneProc: TQueryDoneNotifyProc;
    {$ENDIF}
    //
    OnUserDoneCall: TUserQueryDoneNotifyCall;
    OnUserDoneMethod: TUserQueryDoneNotifyMethod;
    {$IFNDEF FPC}
    OnUserDoneProc: TUserQueryDoneNotifyProc;
    {$ENDIF}
    //
    procedure Init; {$IFDEF INLINE_ASM} inline; {$ENDIF}
  end;

  TUserDownloadDoneNotifyCall = procedure(UserPointer: Pointer; UserObject: TCoreClassObject; UserVariant: Variant;
    dbN: SystemString; dStorePos: Int64; stream: TMemoryStream64);
  TUserDownloadDoneNotifyMethod = procedure(UserPointer: Pointer; UserObject: TCoreClassObject; UserVariant: Variant;
    dbN: SystemString; dStorePos: Int64; stream: TMemoryStream64) of object;
  {$IFNDEF FPC}
  TUserDownloadDoneNotifyProc = reference to procedure(UserPointer: Pointer; UserObject: TCoreClassObject; UserVariant: Variant;
    dbN: SystemString; dStorePos: Int64; stream: TMemoryStream64);
  {$ENDIF}
  //
  TDownloadDoneNotifyCall   = procedure(dbN: SystemString; dStorePos: Int64; stream: TMemoryStream64);
  TDownloadDoneNotifyMethod = procedure(dbN: SystemString; dStorePos: Int64; stream: TMemoryStream64) of object;
  {$IFNDEF FPC}
  TDownloadDoneNotifyProc = reference to procedure(dbN: SystemString; dStorePos: Int64; stream: TMemoryStream64);
  {$ENDIF}
  //
  PDataStoreClientDownloadNotify = ^TDataStoreClientDownloadNotify;

  TDataStoreClientDownloadNotify = packed record
    UserPointer: Pointer;
    UserObject: TCoreClassObject;
    UserVariant: Variant;

    OnUserDoneCall: TUserDownloadDoneNotifyCall;
    OnUserDoneMethod: TUserDownloadDoneNotifyMethod;
    {$IFNDEF FPC}
    OnUserDoneProc: TUserDownloadDoneNotifyProc;
    {$ENDIF}
    //
    OnDoneCall: TDownloadDoneNotifyCall;
    OnDoneMethod: TDownloadDoneNotifyMethod;
    {$IFNDEF FPC}
    OnDoneProc: TDownloadDoneNotifyProc;
    {$ENDIF}
    procedure Init; {$IFDEF INLINE_ASM} inline; {$ENDIF}
  end;

  TPipeState = packed record
    WriteOutputDB, Activted, SyncToClient, MemoryMode, Paused: Boolean;
    DBCounter, QueryCounter, QueryResultCounter, MaxQueryCompare, MaxQueryResult: Int64;
    QueryPerformanceOfPerSec, ConsumTime, MaxWaitTime: Double;
    SourceDB, OutputDB, PipelineName, RegistedQuery: SystemString;
    //
    procedure Init; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure Encode(d: TDataFrameEngine); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure Decode(d: TDataFrameEngine); {$IFDEF INLINE_ASM} inline; {$ENDIF}
  end;

  TPipeInfo = TPipeState;
  PPipeInfo = ^TPipeInfo;

implementation

constructor TTDataStoreService_DBPipeline.Create(InMem: Boolean; AOwner: TZDBLocalManager; sourDBName, APipelineN, OutDBName: SystemString);
begin
  inherited Create(InMem, AOwner, sourDBName, APipelineN, OutDBName);
  SendTunnel := nil;
  RecvTunnel := nil;
  BackcallPtr := 0;
  SyncToClient := False;
  RegistedQuery := '';
end;

destructor TTDataStoreService_DBPipeline.Destroy;
begin
  inherited Destroy;
end;

procedure TTDataStoreService_DBPipeline.Progress(deltaTime: Double);
begin
  inherited Progress(deltaTime);
end;

constructor TTDataStoreService_QueryCall.Create;
begin
  inherited Create;
  OnPipelineQuery := nil;
  OnPipelineQueryDone := nil;
end;

procedure TDataStoreClientQueryNotify.Init;
begin
  UserPointer := nil;
  UserObject := nil;
  UserVariant := Null;

  OnQueryCall := nil;
  OnQueryMethod := nil;
  {$IFNDEF FPC}
  OnQueryProc := nil;
  {$ENDIF}
  //
  OnUserQueryCall := nil;
  OnUserQueryMethod := nil;
  {$IFNDEF FPC}
  OnUserQueryProc := nil;
  {$ENDIF}
  //
  OnDoneCall := nil;
  OnDoneMethod := nil;
  {$IFNDEF FPC}
  OnDoneProc := nil;
  {$ENDIF}
  //
  OnUserDoneCall := nil;
  OnUserDoneMethod := nil;
  {$IFNDEF FPC}
  OnUserDoneProc := nil;
  {$ENDIF}
end;

procedure TDataStoreClientDownloadNotify.Init;
begin
  UserPointer := nil;
  UserObject := nil;
  UserVariant := Null;

  OnDoneCall := nil;
  OnDoneMethod := nil;
  {$IFNDEF FPC}
  OnDoneProc := nil;
  {$ENDIF}
  //
  OnUserDoneCall := nil;
  OnUserDoneMethod := nil;
  {$IFNDEF FPC}
  OnUserDoneProc := nil;
  {$ENDIF}
end;

procedure TPipeState.Init;
begin
  WriteOutputDB := False;
  Activted := False;
  SyncToClient := False;
  MemoryMode := False;
  Paused := False;
  DBCounter := 0;
  QueryCounter := 0;
  QueryResultCounter := 0;
  MaxQueryCompare := 0;
  MaxQueryResult := 0;
  QueryPerformanceOfPerSec := 0;
  ConsumTime := 0;
  MaxWaitTime := 0;
  SourceDB := '';
  OutputDB := '';
  PipelineName := '';
  RegistedQuery := '';
end;

procedure TPipeState.Encode(d: TDataFrameEngine);
begin
  d.WriteBool(WriteOutputDB);
  d.WriteBool(Activted);
  d.WriteBool(SyncToClient);
  d.WriteBool(MemoryMode);
  d.WriteBool(Paused);
  d.WriteInt64(DBCounter);
  d.WriteInt64(QueryCounter);
  d.WriteInt64(QueryResultCounter);
  d.WriteInt64(MaxQueryCompare);
  d.WriteInt64(MaxQueryResult);
  d.WriteDouble(QueryPerformanceOfPerSec);
  d.WriteDouble(ConsumTime);
  d.WriteDouble(MaxWaitTime);
  d.WriteString(SourceDB);
  d.WriteString(OutputDB);
  d.WriteString(PipelineName);
  d.WriteString(RegistedQuery);
end;

procedure TPipeState.Decode(d: TDataFrameEngine);
begin
  Init;
  WriteOutputDB := d.Reader.ReadBool;
  Activted := d.Reader.ReadBool;
  SyncToClient := d.Reader.ReadBool;
  MemoryMode := d.Reader.ReadBool;
  Paused := d.Reader.ReadBool;
  DBCounter := d.Reader.ReadInt64;
  QueryCounter := d.Reader.ReadInt64;
  QueryResultCounter := d.Reader.ReadInt64;
  MaxQueryCompare := d.Reader.ReadInt64;
  MaxQueryResult := d.Reader.ReadInt64;
  QueryPerformanceOfPerSec := d.Reader.ReadDouble;
  ConsumTime := d.Reader.ReadDouble;
  MaxWaitTime := d.Reader.ReadDouble;
  SourceDB := d.Reader.ReadString;
  OutputDB := d.Reader.ReadString;
  PipelineName := d.Reader.ReadString;
  RegistedQuery := d.Reader.ReadString;
end;

initialization

end.
