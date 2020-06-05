{ ****************************************************************************** }
{ * DataStore Service common unit                                              * }
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
unit CommunicationFrameworkDataStoreServiceCommon;

{$INCLUDE zDefine.inc}

interface

uses Variants, CoreClasses, CommunicationFramework, PascalStrings, ZDBEngine, ZDBLocalManager, MemoryStream64,
  DataFrameEngine;

type
  TTDataStoreService_DBPipeline = class(TZDBPipeline)
  public
    SendTunnel: TPeerClientUserDefine;
    RecvTunnel: TPeerClientUserDefine;
    BackcallPtr: UInt64;
    SyncToClient: Boolean;
    RegistedQuery: SystemString;

    constructor Create(InMem: Boolean; Owner_: TZDBLocalManager; sourDBName, APipelineN, OutDBName: SystemString); override;
    destructor Destroy; override;

    procedure Progress(deltaTime: Double); override;
  end;

  TTDataStoreService_QueryCall = class(TCoreClassObject)
  private
  public
    OnPipelineQuery: TZDBPipelineFilterMethod;
    OnPipelineQueryDone: TZDBPipelineDoneMethod;
    constructor Create;
  end;

  TUserQueryDoneNotifyCall = procedure(UserPointer: Pointer; UserObject: TCoreClassObject; UserVariant: Variant;
    dbN, outN, pipeN: SystemString; TotalResult: Int64);
  TUserQueryDoneNotifyMethod = procedure(UserPointer: Pointer; UserObject: TCoreClassObject; UserVariant: Variant;
    dbN, outN, pipeN: SystemString; TotalResult: Int64) of object;
  TQueryDoneNotifyCall = procedure(dbN, outN, pipeN: SystemString; TotalResult: Int64);
  TQueryDoneNotifyMethod = procedure(dbN, outN, pipeN: SystemString; TotalResult: Int64) of object;

{$IFDEF FPC}
  TUserQueryDoneNotifyProc = procedure(UserPointer: Pointer; UserObject: TCoreClassObject; UserVariant: Variant;
    dbN, outN, pipeN: SystemString; TotalResult: Int64) is nested;
  TQueryDoneNotifyProc = procedure(dbN, outN, pipeN: SystemString; TotalResult: Int64) is nested;
{$ELSE FPC}
  TUserQueryDoneNotifyProc = reference to procedure(UserPointer: Pointer; UserObject: TCoreClassObject; UserVariant: Variant;
    dbN, outN, pipeN: SystemString; TotalResult: Int64);
  TQueryDoneNotifyProc = reference to procedure(dbN, outN, pipeN: SystemString; TotalResult: Int64);
{$ENDIF FPC}
  PDataStoreClientQueryNotify = ^TDataStoreClientQueryNotify;

  TDataStoreClientQueryNotify = record
    UserPointer: Pointer;
    UserObject: TCoreClassObject;
    UserVariant: Variant;
    OnQueryCall: TFillQueryDataCall;
    OnQueryMethod: TFillQueryDataMethod;
    OnQueryProc: TFillQueryDataProc;
    OnUserQueryCall: TUserFillQueryDataCall;
    OnUserQueryMethod: TUserFillQueryDataMethod;
    OnUserQueryProc: TUserFillQueryDataProc;
    OnDoneCall: TQueryDoneNotifyCall;
    OnDoneMethod: TQueryDoneNotifyMethod;
    OnDoneProc: TQueryDoneNotifyProc;
    OnUserDoneCall: TUserQueryDoneNotifyCall;
    OnUserDoneMethod: TUserQueryDoneNotifyMethod;
    OnUserDoneProc: TUserQueryDoneNotifyProc;
    procedure Init;
  end;

  TUserDownloadDoneNotifyCall = procedure(UserPointer: Pointer; UserObject: TCoreClassObject; UserVariant: Variant;
    dbN: SystemString; dStorePos: Int64; stream: TMemoryStream64);
  TUserDownloadDoneNotifyMethod = procedure(UserPointer: Pointer; UserObject: TCoreClassObject; UserVariant: Variant;
    dbN: SystemString; dStorePos: Int64; stream: TMemoryStream64) of object;
  TDownloadDoneNotifyCall = procedure(dbN: SystemString; dStorePos: Int64; stream: TMemoryStream64);
  TDownloadDoneNotifyMethod = procedure(dbN: SystemString; dStorePos: Int64; stream: TMemoryStream64) of object;

{$IFDEF FPC}
  TUserDownloadDoneNotifyProc = procedure(UserPointer: Pointer; UserObject: TCoreClassObject; UserVariant: Variant;
    dbN: SystemString; dStorePos: Int64; stream: TMemoryStream64) is nested;
  TDownloadDoneNotifyProc = procedure(dbN: SystemString; dStorePos: Int64; stream: TMemoryStream64) is nested;
{$ELSE FPC}
  TUserDownloadDoneNotifyProc = reference to procedure(UserPointer: Pointer; UserObject: TCoreClassObject; UserVariant: Variant;
    dbN: SystemString; dStorePos: Int64; stream: TMemoryStream64);
  TDownloadDoneNotifyProc = reference to procedure(dbN: SystemString; dStorePos: Int64; stream: TMemoryStream64);
{$ENDIF FPC}
  PDataStoreClientDownloadNotify = ^TDataStoreClientDownloadNotify;

  TDataStoreClientDownloadNotify = record
    UserPointer: Pointer;
    UserObject: TCoreClassObject;
    UserVariant: Variant;
    OnUserDoneCall: TUserDownloadDoneNotifyCall;
    OnUserDoneMethod: TUserDownloadDoneNotifyMethod;
    OnUserDoneProc: TUserDownloadDoneNotifyProc;
    OnDoneCall: TDownloadDoneNotifyCall;
    OnDoneMethod: TDownloadDoneNotifyMethod;
    OnDoneProc: TDownloadDoneNotifyProc;
    procedure Init;
  end;

  { client storePos transform }
  TStorePosTransformNotifyCall = procedure(const TransformBuff: PZDBStorePosTransformArray);
  TStorePosTransformNotifyMethod = procedure(const TransformBuff: PZDBStorePosTransformArray) of object;
{$IFDEF FPC}
  TStorePosTransformNotifyProc = procedure(const TransformBuff: PZDBStorePosTransformArray) is nested;
{$ELSE FPC}
  TStorePosTransformNotifyProc = reference to procedure(const TransformBuff: PZDBStorePosTransformArray);
{$ENDIF FPC}
  PStorePosTransformNotify = ^TStorePosTransformNotify;

  TStorePosTransformNotify = record
    OnDoneCall: TStorePosTransformNotifyCall;
    OnDoneMethod: TStorePosTransformNotifyMethod;
    OnDoneProc: TStorePosTransformNotifyProc;
    procedure Init;
  end;

  TPipeState = record
    WriteOutputDB, Activted, SyncToClient, MemoryMode, Paused: Boolean;
    DBCounter, QueryCounter, QueryResultCounter, MaxQueryCompare, MaxQueryResult: Int64;
    QueryPerformanceOfPerSec, ConsumTime, MaxWaitTime: Double;
    SourceDB, OutputDB, PipelineName, RegistedQuery: SystemString;
    { }
    procedure Init;
    procedure Encode(d: TDataFrameEngine);
    procedure Decode(d: TDataFrameEngine);
  end;

  TPipeInfo = TPipeState;
  PPipeInfo = ^TPipeInfo;

implementation

constructor TTDataStoreService_DBPipeline.Create(InMem: Boolean; Owner_: TZDBLocalManager; sourDBName, APipelineN, OutDBName: SystemString);
begin
  inherited Create(InMem, Owner_, sourDBName, APipelineN, OutDBName);
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
  OnQueryProc := nil;

  OnUserQueryCall := nil;
  OnUserQueryMethod := nil;
  OnUserQueryProc := nil;

  OnDoneCall := nil;
  OnDoneMethod := nil;
  OnDoneProc := nil;

  OnUserDoneCall := nil;
  OnUserDoneMethod := nil;
  OnUserDoneProc := nil;
end;

procedure TStorePosTransformNotify.Init;
begin
  OnDoneCall := nil;
  OnDoneMethod := nil;
  OnDoneProc := nil;
end;

procedure TDataStoreClientDownloadNotify.Init;
begin
  UserPointer := nil;
  UserObject := nil;
  UserVariant := Null;

  OnDoneCall := nil;
  OnDoneMethod := nil;
  OnDoneProc := nil;

  OnUserDoneCall := nil;
  OnUserDoneMethod := nil;
  OnUserDoneProc := nil;
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
