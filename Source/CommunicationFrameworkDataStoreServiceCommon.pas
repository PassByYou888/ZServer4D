{ ****************************************************************************** }
{ * DataStore Service common unit                                              * }
{ * written by QQ 600585@qq.com                                                * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ ****************************************************************************** }
(*
  update history
*)
unit CommunicationFrameworkDataStoreServiceCommon;

{$I  zDefine.inc}

interface

uses CoreClasses, CommunicationFramework, PascalStrings, ZDBEngine, ZDBLocalManager, MemoryStream64,
  DataFrameEngine;

const
  PerQueryPipelineDoneDelayFreeTime = 5.0;

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

  TQueryDoneNotifyCall   = procedure(dbN, outN, pipeN: SystemString; TotalResult: Int64);
  TQueryDoneNotifyMethod = procedure(dbN, outN, pipeN: SystemString; TotalResult: Int64) of object;

  {$IFNDEF FPC}
  TQueryDoneNotifyProc = reference to procedure(dbN, outN, pipeN: SystemString; TotalResult: Int64);
  {$ENDIF}
  PDataStoreClientQueryNotify = ^TDataStoreClientQueryNotify;

  TDataStoreClientQueryNotify = record
    OnQueryCall: TFillQueryDataCall;
    OnQueryMethod: TFillQueryDataMethod;
    {$IFNDEF FPC}
    OnQueryProc: TFillQueryDataProc;
    {$ENDIF}
    OnDoneCall: TQueryDoneNotifyCall;
    OnDoneMethod: TQueryDoneNotifyMethod;
    {$IFNDEF FPC}
    OnDoneProc: TQueryDoneNotifyProc;
    {$ENDIF}
    procedure Init;
  end;

  TDownloadDoneNotifyCall   = procedure(dbN: SystemString; dStorePos: Int64; stream: TMemoryStream64);
  TDownloadDoneNotifyMethod = procedure(dbN: SystemString; dStorePos: Int64; stream: TMemoryStream64) of object;

  {$IFNDEF FPC}
  TDownloadDoneNotifyProc = reference to procedure(dbN: SystemString; dStorePos: Int64; stream: TMemoryStream64);
  {$ENDIF}
  PDataStoreClientDownloadNotify = ^TDataStoreClientDownloadNotify;

  TDataStoreClientDownloadNotify = record
    OnDoneCall: TDownloadDoneNotifyCall;
    OnDoneMethod: TDownloadDoneNotifyMethod;
    {$IFNDEF FPC}
    OnDoneProc: TDownloadDoneNotifyProc;
    {$ENDIF}
    procedure Init;
  end;

  TPipeState = record
    WriteOutputDB, Activted, SyncToClient, MemoryMode, Paused: Boolean;
    DBCounter, QueryCounter, QueryResultCounter, MaxQueryCompare, MaxQueryResult: Int64;
    QueryPerformanceOfPerSec, ConsumTime, MaxWaitTime: Double;
    SourceDB, OutputDB, PipelineName, RegistedQuery: SystemString;
    procedure Init; inline;
    procedure Encode(d: TDataFrameEngine); inline;
    procedure Decode(d: TDataFrameEngine); inline;
  end;

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
  OnQueryCall := nil;
  OnQueryMethod := nil;
  {$IFNDEF FPC}
  OnQueryProc := nil;
  {$ENDIF}
  OnDoneCall := nil;
  OnDoneMethod := nil;
  {$IFNDEF FPC}
  OnDoneProc := nil;
  {$ENDIF}
end;

procedure TDataStoreClientDownloadNotify.Init;
begin
  OnDoneCall := nil;
  OnDoneMethod := nil;
  {$IFNDEF FPC}
  OnDoneProc := nil;
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
