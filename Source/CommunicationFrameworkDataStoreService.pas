{ ****************************************************************************** }
{ * DataStore Service framework(incl File service)                             * }
{ * written by QQ 600585@qq.com                                                * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ ****************************************************************************** }
(*
  update history
*)

unit CommunicationFrameworkDataStoreService;

interface

{$I ..\zDefine.inc}


uses CoreClasses, ListEngine, UnicodeMixedLib, DataFrameEngine, MemoryStream64, CommunicationFramework, TextDataEngine,
  DoStatusIO, Cadencer, NotifyObjectBase, PascalStrings, CoreCipher, ZDBEngine, ItemStream,
  {$IFNDEF FPC}
  JsonDataObjects,
  {$ENDIF}
  CommunicationFrameworkDoubleTunnelIO, CommunicationFrameworkDoubleTunnelIO_NoAuth, ZDBLocalManager;

type
  TDataStoreService                      = class;
  TDataStoreService_PeerClientSendTunnel = class;
  TTDataStoreService_DBPipeline          = class;

  TDataStoreService_PeerClientRecvTunnel = class(TPeerClientUserDefineForRecvTunnel_NoAuth)
  public
    constructor Create(AOwner: TPeerClient); override;
    destructor Destroy; override;

    function SendTunnelDefine: TDataStoreService_PeerClientSendTunnel;
  end;

  TDataStoreService_PeerClientSendTunnel = class(TPeerClientUserDefineForSendTunnel_NoAuth)
  public
    constructor Create(AOwner: TPeerClient); override;
    destructor Destroy; override;

    function RecvTunnelDefine: TDataStoreService_PeerClientRecvTunnel;
  end;

  TTDataStoreService_DBPipeline = class(TZDBPipeline)
  private
    FOwnerDataStoreService: TDataStoreService;
    FSendTunnel           : TDataStoreService_PeerClientSendTunnel;
    FRecvTunnel           : TDataStoreService_PeerClientRecvTunnel;
    FBackcallPtr          : UInt64;
  public
    constructor CreateQueryToNewMemory(AOwner: TZDBLocalManager; sourDBName, taskName, OutDBName: SystemString); override;
    constructor CreateQueryToFile(AOwner: TZDBLocalManager; sourDBName, taskName, OutDBName: SystemString); override;
    destructor Destroy; override;

    procedure Progress(deltaTime: Double); override;
  end;

  TTDataStoreService_QueryCall = class(TCoreClassObject)
  private
    FName: string;
  public
    OnPipelineQuery    : TZDBPipelineFilterMethod;
    OnPipelineQueryDone: TZDBPipelineDoneMethod;
    property name      : string read FName;
    constructor Create;
  end;

  TDataStoreService = class(TCommunicationFramework_DoubleTunnelService_NoAuth, IZDBLocalManagerNotify)
  private
    FZDBLocal     : TZDBLocalManager;
    FQueryCallPool: THashObjectList;
  private
    procedure CreateQuery(pipe: TZDBPipeline);
    procedure QueryFragmentData(pipe: TZDBPipeline; FragmentSource: TMemoryStream64);
    procedure QueryDone(pipe: TZDBPipeline);
    procedure CreateDB(ActiveDB: TZDBStoreEngine);
    procedure CloseDB(ActiveDB: TZDBStoreEngine);
    procedure InsertData(Sender: TZDBStoreEngine; InsertPos: Int64; buff: TCoreClassStream; id: Cardinal; CompletePos: Int64);
    procedure AddData(Sender: TZDBStoreEngine; buff: TCoreClassStream; id: Cardinal; CompletePos: Int64);
    procedure ModifyData(Sender: TZDBStoreEngine; const StorePos: Int64; buff: TCoreClassStream);
    procedure DeleteData(Sender: TZDBStoreEngine; const StorePos: Int64);
  protected
    procedure DownloadQueryFilterMethod(dPipe: TZDBPipeline; var qState: TQueryState; var Allowed: Boolean);
  protected
    procedure UserOut(UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth); override;
  protected
    procedure Command_InitDB(Sender: TPeerClient; InData: TDataFrameEngine); virtual;
    procedure Command_CloseDB(Sender: TPeerClient; InData: TDataFrameEngine); virtual;

    procedure Command_QueryDB(Sender: TPeerClient; InData: TDataFrameEngine); virtual;
    procedure Command_DownloadDB(Sender: TPeerClient; InData: TDataFrameEngine); virtual;
    procedure Command_RequestDownloadAssembleStream(Sender: TPeerClient; InData: TDataFrameEngine); virtual;

    procedure Command_CompletedPostAssembleStream(Sender: TPeerClient; InData: TDataFrameEngine); virtual;
    procedure Command_CompletedInsertAssembleStream(Sender: TPeerClient; InData: TDataFrameEngine); virtual;
    procedure Command_CompletedModifyAssembleStream(Sender: TPeerClient; InData, OutData: TDataFrameEngine); virtual;

    procedure Command_DeleteData(Sender: TPeerClient; InData, OutData: TDataFrameEngine); virtual;
  public
    constructor Create(ARecvTunnel, ASendTunnel: TCommunicationFrameworkServer);
    destructor Destroy; override;

    procedure RegisterCommand; override;
    procedure UnRegisterCommand; override;

    procedure Progress; override;

    function GetDataStoreUserDefine(RecvCli: TPeerClient): TDataStoreService_PeerClientRecvTunnel;

    function RegisterQueryCall(cName: string): TTDataStoreService_QueryCall;
    procedure UnRegisterQueryCall(cName: string);
    function GetRegistedQueryCall(cName: string): TTDataStoreService_QueryCall;

    // send client command
    procedure Send_CompletedFragmentBigStream(pipe: TTDataStoreService_DBPipeline);
    procedure Send_CompletedQuery(pipe: TTDataStoreService_DBPipeline);
    procedure Send_CompletedDownloadAssemble(ASendCli: TPeerClient; dbN: string; dStorePos: Int64; BackcallPtr: UInt64);

    property ZDBLocal: TZDBLocalManager read FZDBLocal;
    property QueryCallPool: THashObjectList read FQueryCallPool;
  end;

  TQueryDoneNotifyCall   = procedure(dbN, outN, pipeN: string; TotalResult: Int64);
  TQueryDoneNotifyMethod = procedure(dbN, outN, pipeN: string; TotalResult: Int64) of object;

  {$IFNDEF FPC}
  TQueryDoneNotifyProc = reference to procedure(dbN, outN, pipeN: string; TotalResult: Int64);
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

  TDownloadDoneNotifyCall   = procedure(dbN: string; dStorePos: Int64; Stream: TMemoryStream64);
  TDownloadDoneNotifyMethod = procedure(dbN: string; dStorePos: Int64; Stream: TMemoryStream64) of object;

  {$IFNDEF FPC}
  TDownloadDoneNotifyProc = reference to procedure(dbN: string; dStorePos: Int64; Stream: TMemoryStream64);
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

  TDataStoreClient = class(TCommunicationFramework_DoubleTunnelClient_NoAuth)
  private
    procedure Command_CompletedFragmentBigStream(Sender: TPeerClient; InData: TDataFrameEngine); virtual;
    procedure Command_CompletedQuery(Sender: TPeerClient; InData: TDataFrameEngine); virtual;
    procedure Command_CompletedDownloadAssemble(Sender: TPeerClient; InData: TDataFrameEngine); virtual;
  public
    constructor Create(ARecvTunnel, ASendTunnel: TCommunicationFrameworkClient);
    destructor Destroy; override;

    procedure RegisterCommand; override;
    procedure UnRegisterCommand; override;

    procedure Progress; override;

    procedure InitDB(inMem: Boolean; dbN: string); virtual;
    procedure CloseDB(dbN: string); virtual;

    procedure QueryDB(RegistedQueryName: string; inMem, ReverseQuery: Boolean; dbN, outDBN: string;
      fragmentWait, MaxWait: Double; MaxQueryResult: Int64; BackcallPtr: PDataStoreClientQueryNotify; Values: THashVariantList); overload; virtual;

    procedure QueryDB(RegistedQueryName: string; inMem, ReverseQuery: Boolean; dbN, outDBN: string;
      fragmentWait, MaxWait: Double; MaxQueryResult: Int64;
      Values: THashVariantList; OnQueryCall: TFillQueryDataCall; OnDoneCall: TQueryDoneNotifyCall); overload;

    procedure QueryDB(RegistedQueryName: string; inMem, ReverseQuery: Boolean; dbN, outDBN: string;
      fragmentWait, MaxWait: Double; MaxQueryResult: Int64;
      Values: THashVariantList; OnQueryMethod: TFillQueryDataMethod; OnDoneMethod: TQueryDoneNotifyMethod); overload;

    {$IFNDEF FPC}
    procedure QueryDB(RegistedQueryName: string; inMem, ReverseQuery: Boolean; dbN, outDBN: string;
      fragmentWait, MaxWait: Double; MaxQueryResult: Int64;
      Values: THashVariantList; OnQueryProc: TFillQueryDataProc; OnDoneProc: TQueryDoneNotifyProc); overload;
    {$ENDIF}
    procedure QueryDB(RegistedQueryName: string; dbN: string; Values: THashVariantList; OnQueryCall: TFillQueryDataCall; OnDoneCall: TQueryDoneNotifyCall); overload;
    procedure QueryDB(RegistedQueryName: string; dbN: string; Values: THashVariantList; OnQueryMethod: TFillQueryDataMethod; OnDoneMethod: TQueryDoneNotifyMethod); overload;
    {$IFNDEF FPC}
    procedure QueryDB(RegistedQueryName: string; dbN: string; Values: THashVariantList; OnQueryProc: TFillQueryDataProc; OnDoneProc: TQueryDoneNotifyProc); overload;
    {$ENDIF}
    procedure DownloadDB(ReverseQuery: Boolean; dbN: string; BackcallPtr: PDataStoreClientQueryNotify); overload; virtual;
    procedure DownloadDB(ReverseQuery: Boolean; dbN: string; OnQueryCall: TFillQueryDataCall; OnDoneCall: TQueryDoneNotifyCall); overload;
    procedure DownloadDB(ReverseQuery: Boolean; dbN: string; OnQueryMethod: TFillQueryDataMethod; OnDoneMethod: TQueryDoneNotifyMethod); overload;
    {$IFNDEF FPC}
    procedure DownloadDB(ReverseQuery: Boolean; dbN: string; OnQueryProc: TFillQueryDataProc; OnDoneProc: TQueryDoneNotifyProc); overload;
    {$ENDIF}
    procedure BeginAssembleStream; virtual;

    procedure RequestDownloadAssembleStream(dbN: string; StorePos: Int64; BackcallPtr: PDataStoreClientDownloadNotify); virtual;
    procedure DownloadAssembleStream(dbN: string; StorePos: Int64; OnDoneCall: TDownloadDoneNotifyCall); overload;
    procedure DownloadAssembleStream(dbN: string; StorePos: Int64; OnDoneMethod: TDownloadDoneNotifyMethod); overload;
    {$IFNDEF FPC}
    procedure DownloadAssembleStream(dbN: string; StorePos: Int64; OnDoneProc: TDownloadDoneNotifyProc); overload;
    {$ENDIF}
    procedure PostAssembleStream(dbN: string; Stream: TCoreClassStream; dID: Cardinal; DoneTimeFree: Boolean); overload; virtual;
    procedure PostAssembleStream(dbN: string; DataSource: TDataFrameEngine); overload; virtual;
    procedure PostAssembleStream(dbN: string; DataSource: THashVariantList); overload; virtual;
    procedure PostAssembleStream(dbN: string; DataSource: TSectionTextData); overload; virtual;
    {$IFNDEF FPC}
    procedure PostAssembleStream(dbN: string; DataSource: TJsonObject); overload; virtual;
    {$ENDIF}
    procedure InsertAssembleStream(dbN: string; dStorePos: Int64; Stream: TCoreClassStream; dID: Cardinal; DoneTimeFree: Boolean); overload; virtual;
    procedure ModifyAssembleStream(dbN: string; dStorePos: Int64; Stream: TCoreClassStream; DoneTimeFree: Boolean; OnResult: TStreamMethod); overload; virtual;
    {$IFNDEF FPC}
    procedure ModifyAssembleStream(dbN: string; dStorePos: Int64; Stream: TCoreClassStream; DoneTimeFree: Boolean; OnResult: TStreamProc); overload; virtual;
    {$ENDIF}
    procedure GetPostAssembleStreamState(OnResult: TStreamMethod); overload; virtual;
    {$IFNDEF FPC}
    procedure GetPostAssembleStreamState(OnResult: TStreamProc); overload; virtual;
    {$ENDIF}
    procedure EndAssembleStream; virtual;

    procedure DeleteData(dbN: string; dStorePos: Int64; OnResult: TStreamMethod); overload; virtual;
    {$IFNDEF FPC}
    procedure DeleteData(dbN: string; dStorePos: Int64; OnResult: TStreamProc); overload; virtual;
    {$ENDIF}
  end;

implementation


constructor TDataStoreService_PeerClientRecvTunnel.Create(AOwner: TPeerClient);
begin
  inherited Create(AOwner);
end;

destructor TDataStoreService_PeerClientRecvTunnel.Destroy;
begin
  inherited Destroy;
end;

function TDataStoreService_PeerClientRecvTunnel.SendTunnelDefine: TDataStoreService_PeerClientSendTunnel;
begin
  Result := SendTunnel as TDataStoreService_PeerClientSendTunnel;
end;

constructor TDataStoreService_PeerClientSendTunnel.Create(AOwner: TPeerClient);
begin
  inherited Create(AOwner);
end;

destructor TDataStoreService_PeerClientSendTunnel.Destroy;
begin
  inherited Destroy;
end;

function TDataStoreService_PeerClientSendTunnel.RecvTunnelDefine: TDataStoreService_PeerClientRecvTunnel;
begin
  Result := RecvTunnel as TDataStoreService_PeerClientRecvTunnel;
end;

constructor TTDataStoreService_DBPipeline.CreateQueryToNewMemory(AOwner: TZDBLocalManager; sourDBName, taskName, OutDBName: SystemString);
begin
  inherited CreateQueryToNewMemory(AOwner, sourDBName, taskName, OutDBName);
  FSendTunnel := nil;
  FRecvTunnel := nil;
  FBackcallPtr := 0;
end;

constructor TTDataStoreService_DBPipeline.CreateQueryToFile(AOwner: TZDBLocalManager; sourDBName, taskName, OutDBName: SystemString);
begin
  inherited CreateQueryToFile(AOwner, sourDBName, taskName, OutDBName);
  FSendTunnel := nil;
  FRecvTunnel := nil;
  FBackcallPtr := 0;
end;

destructor TTDataStoreService_DBPipeline.Destroy;
begin
  inherited Destroy;
end;

procedure TTDataStoreService_DBPipeline.Progress(deltaTime: Double);
begin
  inherited Progress(deltaTime);
  RealTimePostFragmentData := not FSendTunnel.Owner.BigStreamIsSending;
end;

constructor TTDataStoreService_QueryCall.Create;
begin
  inherited Create;
  FName := '';
  OnPipelineQuery := nil;
  OnPipelineQueryDone := nil;
end;

procedure TDataStoreService.CreateQuery(pipe: TZDBPipeline);
var
  pl: TTDataStoreService_DBPipeline;
begin
  doStatus('create query: %s', [pipe.PipelineName]);
  pl := TTDataStoreService_DBPipeline(pipe);
end;

procedure TDataStoreService.QueryFragmentData(pipe: TZDBPipeline; FragmentSource: TMemoryStream64);
var
  pl        : TTDataStoreService_DBPipeline;
  destStream: TMemoryStream64;
begin
  pl := TTDataStoreService_DBPipeline(pipe);
  if not SendTunnel.Exists(pl.FSendTunnel.Owner) then
      exit;

  destStream := TMemoryStream64.Create;
  FragmentSource.Position := 0;
  MaxCompressStream(FragmentSource, destStream);

  ClearBatchStream(pl.FSendTunnel.Owner);
  PostBatchStream(pl.FSendTunnel.Owner, destStream, True);
  Send_CompletedFragmentBigStream(pl);
  ClearBatchStream(pl.FSendTunnel.Owner);
end;

procedure TDataStoreService.QueryDone(pipe: TZDBPipeline);
var
  pl: TTDataStoreService_DBPipeline;
begin
  doStatus('%s query done', [pipe.PipelineName]);
  pl := TTDataStoreService_DBPipeline(pipe);

  if not FSendTunnel.Exists(pl.FSendTunnel) then
      exit;

  Send_CompletedQuery(pl);
end;

procedure TDataStoreService.CreateDB(ActiveDB: TZDBStoreEngine);
begin
  doStatus('create new DB: %s', [ActiveDB.name]);
end;

procedure TDataStoreService.CloseDB(ActiveDB: TZDBStoreEngine);
begin
  doStatus('close DB: %s', [ActiveDB.name]);
end;

procedure TDataStoreService.InsertData(Sender: TZDBStoreEngine; InsertPos: Int64; buff: TCoreClassStream; id: Cardinal; CompletePos: Int64);
begin
end;

procedure TDataStoreService.AddData(Sender: TZDBStoreEngine; buff: TCoreClassStream; id: Cardinal; CompletePos: Int64);
begin
end;

procedure TDataStoreService.ModifyData(Sender: TZDBStoreEngine; const StorePos: Int64; buff: TCoreClassStream);
begin
end;

procedure TDataStoreService.DeleteData(Sender: TZDBStoreEngine; const StorePos: Int64);
begin
end;

procedure TDataStoreService.DownloadQueryFilterMethod(dPipe: TZDBPipeline; var qState: TQueryState; var Allowed: Boolean);
begin
  Allowed := True;
end;

procedure TDataStoreService.UserOut(UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth);
begin
  inherited UserOut(UserDefineIO);
end;

procedure TDataStoreService.Command_InitDB(Sender: TPeerClient; InData: TDataFrameEngine);
var
  rt   : TDataStoreService_PeerClientRecvTunnel;
  inMem: Boolean;
  dbN  : string;
begin
  rt := GetDataStoreUserDefine(Sender);
  if not rt.LinkOk then
      exit;

  inMem := InData.Reader.ReadBool;
  dbN := InData.Reader.ReadString;
  if inMem then
      FZDBLocal.InitMemoryDB(dbN)
  else
      FZDBLocal.InitDB(dbN, False);
end;

procedure TDataStoreService.Command_CloseDB(Sender: TPeerClient; InData: TDataFrameEngine);
var
  rt : TDataStoreService_PeerClientRecvTunnel;
  dbN: string;
begin
  rt := GetDataStoreUserDefine(Sender);
  if not rt.LinkOk then
      exit;

  dbN := InData.Reader.ReadString;
  FZDBLocal.CloseDB(dbN);
end;

procedure TDataStoreService.Command_QueryDB(Sender: TPeerClient; InData: TDataFrameEngine);
var
  rt                   : TDataStoreService_PeerClientRecvTunnel;
  RegedQueryName       : string;
  inMem, ReverseQuery  : Boolean;
  dbN, outDBN          : string;
  fragmentWait, MaxWait: Double;
  MaxQueryResult       : Int64;
  pl                   : TTDataStoreService_DBPipeline;
  qc                   : TTDataStoreService_QueryCall;
begin
  rt := GetDataStoreUserDefine(Sender);
  if not rt.LinkOk then
      exit;

  RegedQueryName := InData.Reader.ReadString;
  inMem := InData.Reader.ReadBool;
  ReverseQuery := InData.Reader.ReadBool;
  dbN := InData.Reader.ReadString;
  outDBN := InData.Reader.ReadString;
  fragmentWait := InData.Reader.ReadDouble;
  MaxWait := InData.Reader.ReadDouble;
  MaxQueryResult := InData.Reader.ReadInt64;

  if not FZDBLocal.ExistsDB(dbN) then
      exit;

  qc := TTDataStoreService_QueryCall(FQueryCallPool[RegedQueryName]);

  pl := TTDataStoreService_DBPipeline(FZDBLocal.QueryDB(inMem, ReverseQuery, dbN, outDBN, True, 5.0, fragmentWait, MaxWait, 0, MaxQueryResult));
  pl.FOwnerDataStoreService := Self;
  pl.FSendTunnel := rt.SendTunnelDefine;
  pl.FRecvTunnel := rt;
  pl.FBackcallPtr := InData.Reader.ReadPointer;
  if InData.Reader.NotEnd then
      InData.Reader.ReadVariantList(pl.Values);

  if qc <> nil then
    begin
      pl.OnDataFilterMethod := qc.OnPipelineQuery;
      pl.OnDataDoneMethod := qc.OnPipelineQueryDone;
    end
  else
    {$IFDEF FPC}
      pl.OnDataFilterMethod := @DownloadQueryFilterMethod;
    {$ELSE}
      pl.OnDataFilterMethod := DownloadQueryFilterMethod;
  {$ENDIF}
  ClearBatchStream(rt.SendTunnelDefine.Owner);
end;

procedure TDataStoreService.Command_DownloadDB(Sender: TPeerClient; InData: TDataFrameEngine);
var
  rt          : TDataStoreService_PeerClientRecvTunnel;
  ReverseQuery: Boolean;
  dbN         : string;
  pl          : TTDataStoreService_DBPipeline;
begin
  rt := GetDataStoreUserDefine(Sender);
  if not rt.LinkOk then
      exit;

  ReverseQuery := InData.Reader.ReadBool;
  dbN := InData.Reader.ReadString;

  if not FZDBLocal.ExistsDB(dbN) then
      exit;

  pl := TTDataStoreService_DBPipeline(FZDBLocal.QueryDB(False, ReverseQuery, dbN, 'Download', True, 5.0, 0.5, 0, 0, 0));
  pl.FOwnerDataStoreService := Self;
  pl.FSendTunnel := rt.SendTunnelDefine;
  pl.FRecvTunnel := rt;
  pl.FBackcallPtr := InData.Reader.ReadPointer;
  {$IFDEF FPC}
  pl.OnDataFilterMethod := @DownloadQueryFilterMethod;
  {$ELSE}
  pl.OnDataFilterMethod := DownloadQueryFilterMethod;
  {$ENDIF}
  ClearBatchStream(rt.SendTunnelDefine.Owner);
end;

procedure TDataStoreService.Command_RequestDownloadAssembleStream(Sender: TPeerClient; InData: TDataFrameEngine);
var
  rt         : TDataStoreService_PeerClientRecvTunnel;
  dbN        : string;
  StorePos   : Int64;
  BackcallPtr: UInt64;
  m          : TMemoryStream64;
begin
  rt := GetDataStoreUserDefine(Sender);
  if not rt.LinkOk then
      exit;

  dbN := InData.Reader.ReadString;
  StorePos := InData.Reader.ReadInt64;
  BackcallPtr := InData.Reader.ReadPointer;

  m := TMemoryStream64.Create;
  if not FZDBLocal.WriteDBItemToOneFragment(dbN, StorePos, m) then
    begin
      Sender.PrintParam('get Data Assemble Stream error: %s', dbN);
      DisposeObject(m);
      exit;
    end;

  ClearBatchStream(rt.SendTunnelDefine.Owner);
  PostBatchStream(rt.SendTunnelDefine.Owner, m, True);
  Send_CompletedDownloadAssemble(rt.SendTunnelDefine.Owner, dbN, StorePos, BackcallPtr);
  ClearBatchStream(rt.SendTunnelDefine.Owner);
end;

procedure TDataStoreService.Command_CompletedPostAssembleStream(Sender: TPeerClient; InData: TDataFrameEngine);
var
  rt : TDataStoreService_PeerClientRecvTunnel;
  dbN: string;
  dID: Cardinal;
  p  : PBigStreamBatchPostData;
begin
  rt := GetDataStoreUserDefine(Sender);
  if not rt.LinkOk then
      exit;

  if rt.BigStreamBatchList.Count <= 0 then
      exit;

  dbN := InData.Reader.ReadString;
  dID := InData.Reader.ReadCardinal;

  p := rt.BigStreamBatchList.Last;
  p^.DBStorePos := FZDBLocal.PostData(dbN, p^.Source, dID);
end;

procedure TDataStoreService.Command_CompletedInsertAssembleStream(Sender: TPeerClient; InData: TDataFrameEngine);
var
  rt       : TDataStoreService_PeerClientRecvTunnel;
  dbN      : string;
  dStorePos: Int64;
  dID      : Cardinal;
  p        : PBigStreamBatchPostData;
begin
  rt := GetDataStoreUserDefine(Sender);
  if not rt.LinkOk then
      exit;

  if rt.BigStreamBatchList.Count <= 0 then
      exit;

  dbN := InData.Reader.ReadString;
  dStorePos := InData.Reader.ReadInt64;
  dID := InData.Reader.ReadCardinal;

  p := rt.BigStreamBatchList.Last;
  p^.DBStorePos := FZDBLocal.InsertData(dbN, dStorePos, p^.Source, dID);
end;

procedure TDataStoreService.Command_CompletedModifyAssembleStream(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
var
  rt       : TDataStoreService_PeerClientRecvTunnel;
  dbN      : string;
  dStorePos: Int64;
  p        : PBigStreamBatchPostData;
begin
  rt := GetDataStoreUserDefine(Sender);
  if not rt.LinkOk then
    begin
      OutData.WriteBool(False);
      exit;
    end;

  if rt.BigStreamBatchList.Count <= 0 then
    begin
      OutData.WriteBool(False);
      exit;
    end;

  dbN := InData.Reader.ReadString;
  dStorePos := InData.Reader.ReadInt64;

  p := rt.BigStreamBatchList.Last;

  if FZDBLocal.SetData(dbN, dStorePos, p^.Source) then
    begin
      p^.DBStorePos := dStorePos;
      OutData.WriteBool(True);
    end
  else
    begin
      OutData.WriteBool(False);
    end;
end;

procedure TDataStoreService.Command_DeleteData(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
var
  rt       : TDataStoreService_PeerClientRecvTunnel;
  dbN      : string;
  dStorePos: Int64;
begin
  rt := GetDataStoreUserDefine(Sender);
  if not rt.LinkOk then
    begin
      OutData.WriteBool(False);
      exit;
    end;
  dbN := InData.Reader.ReadString;
  dStorePos := InData.Reader.ReadInt64;
  if FZDBLocal.DeleteData(dbN, dStorePos) then
    begin
      OutData.WriteBool(True);
    end
  else
    begin
      OutData.WriteBool(False);
    end;
end;

constructor TDataStoreService.Create(ARecvTunnel, ASendTunnel: TCommunicationFrameworkServer);
begin
  inherited Create(ARecvTunnel, ASendTunnel);
  FRecvTunnel.PeerClientUserDefineClass := TDataStoreService_PeerClientRecvTunnel;
  FSendTunnel.PeerClientUserDefineClass := TDataStoreService_PeerClientSendTunnel;

  FZDBLocal := TZDBLocalManager.Create;
  FZDBLocal.PipelineClass := TTDataStoreService_DBPipeline;
  FZDBLocal.NotifyIntf := Self;
  FZDBLocal.LoadDB(False);

  FQueryCallPool := THashObjectList.Create(True);
  RegisterCommand;
end;

destructor TDataStoreService.Destroy;
begin
  UnRegisterCommand;
  DisposeObject([FZDBLocal, FQueryCallPool]);
  inherited Destroy;
end;

procedure TDataStoreService.RegisterCommand;
begin
  inherited RegisterCommand;
  {$IFDEF FPC}
  FRecvTunnel.RegisterDirectStream('InitDB').OnExecute := @Command_InitDB;
  FRecvTunnel.RegisterDirectStream('CloseDB').OnExecute := @Command_CloseDB;
  FRecvTunnel.RegisterDirectStream('QueryDB').OnExecute := @Command_QueryDB;
  FRecvTunnel.RegisterDirectStream('DownloadDB').OnExecute := @Command_DownloadDB;
  FRecvTunnel.RegisterDirectStream('RequestDownloadAssembleStream').OnExecute := @Command_RequestDownloadAssembleStream;
  FRecvTunnel.RegisterDirectStream('CompletedPostAssembleStream').OnExecute := @Command_CompletedPostAssembleStream;
  FRecvTunnel.RegisterDirectStream('CompletedInsertAssembleStream').OnExecute := @Command_CompletedInsertAssembleStream;
  FRecvTunnel.RegisterStream('CompletedModifyAssembleStream').OnExecute := @Command_CompletedModifyAssembleStream;
  FRecvTunnel.RegisterStream('DeleteData').OnExecute := @Command_DeleteData;
  {$ELSE}
  FRecvTunnel.RegisterDirectStream('InitDB').OnExecute := Command_InitDB;
  FRecvTunnel.RegisterDirectStream('CloseDB').OnExecute := Command_CloseDB;
  FRecvTunnel.RegisterDirectStream('QueryDB').OnExecute := Command_QueryDB;
  FRecvTunnel.RegisterDirectStream('DownloadDB').OnExecute := Command_DownloadDB;
  FRecvTunnel.RegisterDirectStream('RequestDownloadAssembleStream').OnExecute := Command_RequestDownloadAssembleStream;
  FRecvTunnel.RegisterDirectStream('CompletedPostAssembleStream').OnExecute := Command_CompletedPostAssembleStream;
  FRecvTunnel.RegisterDirectStream('CompletedInsertAssembleStream').OnExecute := Command_CompletedInsertAssembleStream;
  FRecvTunnel.RegisterStream('CompletedModifyAssembleStream').OnExecute := Command_CompletedModifyAssembleStream;
  FRecvTunnel.RegisterStream('DeleteData').OnExecute := Command_DeleteData;
  {$ENDIF}
end;

procedure TDataStoreService.UnRegisterCommand;
begin
  inherited UnRegisterCommand;
  FRecvTunnel.DeleteRegistedCMD('InitDB');
  FRecvTunnel.DeleteRegistedCMD('CloseDB');
  FRecvTunnel.DeleteRegistedCMD('QueryDB');
  FRecvTunnel.DeleteRegistedCMD('DownloadDB');
  FRecvTunnel.DeleteRegistedCMD('RequestDownloadAssembleStream');
  FRecvTunnel.DeleteRegistedCMD('CompletedPostAssembleStream');
  FRecvTunnel.DeleteRegistedCMD('CompletedInsertAssembleStream');
  FRecvTunnel.DeleteRegistedCMD('CompletedModifyAssembleStream');
  FRecvTunnel.DeleteRegistedCMD('DeleteData');
end;

procedure TDataStoreService.Progress;
begin
  inherited Progress;
  FZDBLocal.Progress;
end;

function TDataStoreService.GetDataStoreUserDefine(RecvCli: TPeerClient): TDataStoreService_PeerClientRecvTunnel;
begin
  Result := RecvCli.UserDefine as TDataStoreService_PeerClientRecvTunnel;
end;

function TDataStoreService.RegisterQueryCall(cName: string): TTDataStoreService_QueryCall;
begin
  if FQueryCallPool.Exists(cName) then
      RaiseInfo('Query call already registed:%s', [cName]);

  Result := TTDataStoreService_QueryCall.Create;
  Result.FName := cName;
  FQueryCallPool[cName] := Result;
end;

procedure TDataStoreService.UnRegisterQueryCall(cName: string);
begin
  if not FQueryCallPool.Exists(cName) then
      RaiseInfo('Query call not registed:%s', [cName]);

  FQueryCallPool.Delete(cName);
end;

function TDataStoreService.GetRegistedQueryCall(cName: string): TTDataStoreService_QueryCall;
begin
  Result := TTDataStoreService_QueryCall(FQueryCallPool[cName]);
end;

procedure TDataStoreService.Send_CompletedFragmentBigStream(pipe: TTDataStoreService_DBPipeline);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  de.WriteString(pipe.SourceDB.name);
  de.WriteString(pipe.OutputDB.name);
  de.WriteString(pipe.PipelineName);
  de.WritePointer(pipe.FBackcallPtr);
  pipe.FSendTunnel.Owner.SendDirectStreamCmd('CompletedFragmentBigStream', de);
  DisposeObject(de);
end;

procedure TDataStoreService.Send_CompletedQuery(pipe: TTDataStoreService_DBPipeline);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  de.WriteString(pipe.SourceDB.name);
  de.WriteString(pipe.OutputDB.name);
  de.WriteString(pipe.PipelineName);
  de.WritePointer(pipe.FBackcallPtr);
  de.WriteInt64(pipe.OutputDB.Count);
  pipe.FSendTunnel.Owner.SendDirectStreamCmd('CompletedQuery', de);
  DisposeObject(de);
  ClearBatchStream(pipe.FSendTunnel.Owner);
end;

procedure TDataStoreService.Send_CompletedDownloadAssemble(ASendCli: TPeerClient; dbN: string; dStorePos: Int64; BackcallPtr: UInt64);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  de.WriteString(dbN);
  de.WriteInt64(dStorePos);
  de.WritePointer(BackcallPtr);
  ASendCli.SendDirectStreamCmd('CompletedDownloadAssemble', de);
  DisposeObject(de);
  ClearBatchStream(ASendCli);
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

procedure TDataStoreClient.Command_CompletedFragmentBigStream(Sender: TPeerClient; InData: TDataFrameEngine);
var
  dbN, outN, pipeN: string;
  BackcallPtr     : PDataStoreClientQueryNotify;
  m               : TMemoryStream64;
begin
  dbN := InData.Reader.ReadString;
  outN := InData.Reader.ReadString;
  pipeN := InData.Reader.ReadString;
  BackcallPtr := PDataStoreClientQueryNotify(InData.Reader.ReadPointer);

  m := TMemoryStream64.Create;

  Sender.UserDefine.BigStreamBatchList.Last^.Source.Position := 0;
  DecompressStream(Sender.UserDefine.BigStreamBatchList.Last^.Source, m);
  Sender.UserDefine.BigStreamBatchList.DeleteLast;

  if BackcallPtr <> nil then
    begin
      try
        if Assigned(BackcallPtr^.OnQueryCall) then
            FillFragmentSource(dbN, pipeN, m, BackcallPtr^.OnQueryCall);
        if Assigned(BackcallPtr^.OnQueryMethod) then
            FillFragmentSource(dbN, pipeN, m, BackcallPtr^.OnQueryMethod);
        {$IFNDEF FPC}
        if Assigned(BackcallPtr^.OnQueryProc) then
            FillFragmentSource(dbN, pipeN, m, BackcallPtr^.OnQueryProc);
        {$ENDIF}
      except
      end;
    end;

  DisposeObject(m);
end;

procedure TDataStoreClient.Command_CompletedQuery(Sender: TPeerClient; InData: TDataFrameEngine);
var
  dbN, outN, pipeN: string;
  BackcallPtr     : PDataStoreClientQueryNotify;
  TotalResultCount: Int64;
begin
  dbN := InData.Reader.ReadString;
  outN := InData.Reader.ReadString;
  pipeN := InData.Reader.ReadString;
  BackcallPtr := PDataStoreClientQueryNotify(InData.Reader.ReadPointer);
  TotalResultCount := InData.Reader.ReadInt64;

  if BackcallPtr <> nil then
    begin
      try
        if Assigned(BackcallPtr^.OnDoneCall) then
            BackcallPtr^.OnDoneCall(dbN, outN, pipeN, TotalResultCount);
        if Assigned(BackcallPtr^.OnDoneMethod) then
            BackcallPtr^.OnDoneMethod(dbN, outN, pipeN, TotalResultCount);
        {$IFNDEF FPC}
        if Assigned(BackcallPtr^.OnDoneProc) then
            BackcallPtr^.OnDoneProc(dbN, outN, pipeN, TotalResultCount);
        {$ENDIF}
      except
      end;
      Dispose(BackcallPtr);
    end;
  Sender.UserDefine.BigStreamBatchList.Clear;
end;

procedure TDataStoreClient.Command_CompletedDownloadAssemble(Sender: TPeerClient; InData: TDataFrameEngine);
var
  dbN        : string;
  dStorePos  : Int64;
  BackcallPtr: PDataStoreClientDownloadNotify;
  m          : TMemoryStream64;
begin
  dbN := InData.Reader.ReadString;
  dStorePos := InData.Reader.ReadInt64;
  BackcallPtr := PDataStoreClientDownloadNotify(InData.Reader.ReadPointer);

  if Sender.UserDefine.BigStreamBatchList.Count > 0 then
      m := Sender.UserDefine.BigStreamBatchList.Last^.Source
  else
      m := nil;

  if BackcallPtr <> nil then
    begin
      if m <> nil then
        begin
          try
            m.Position := 0;
            if Assigned(BackcallPtr^.OnDoneCall) then
                BackcallPtr^.OnDoneCall(dbN, dStorePos, m);
            m.Position := 0;
            if Assigned(BackcallPtr^.OnDoneMethod) then
                BackcallPtr^.OnDoneMethod(dbN, dStorePos, m);
            {$IFNDEF FPC}
            m.Position := 0;
            if Assigned(BackcallPtr^.OnDoneProc) then
                BackcallPtr^.OnDoneProc(dbN, dStorePos, m);
            {$ENDIF}
          except
          end;
          Sender.UserDefine.BigStreamBatchList.DeleteLast;
        end;
      Dispose(BackcallPtr);
    end;
end;

constructor TDataStoreClient.Create(ARecvTunnel, ASendTunnel: TCommunicationFrameworkClient);
begin
  inherited Create(ARecvTunnel, ASendTunnel);
  RegisterCommand;
end;

destructor TDataStoreClient.Destroy;
begin
  UnRegisterCommand;
  inherited Destroy;
end;

procedure TDataStoreClient.RegisterCommand;
begin
  inherited RegisterCommand;
  {$IFDEF FPC}
  FRecvTunnel.RegisterDirectStream('CompletedFragmentBigStream').OnExecute := @Command_CompletedFragmentBigStream;
  FRecvTunnel.RegisterDirectStream('CompletedQuery').OnExecute := @Command_CompletedQuery;
  FRecvTunnel.RegisterDirectStream('CompletedDownloadAssemble').OnExecute := @Command_CompletedDownloadAssemble;
  {$ELSE}
  FRecvTunnel.RegisterDirectStream('CompletedFragmentBigStream').OnExecute := Command_CompletedFragmentBigStream;
  FRecvTunnel.RegisterDirectStream('CompletedQuery').OnExecute := Command_CompletedQuery;
  FRecvTunnel.RegisterDirectStream('CompletedDownloadAssemble').OnExecute := Command_CompletedDownloadAssemble;
  {$ENDIF}
end;

procedure TDataStoreClient.UnRegisterCommand;
begin
  inherited UnRegisterCommand;
  FRecvTunnel.DeleteRegistedCMD('CompletedFragmentBigStream');
  FRecvTunnel.DeleteRegistedCMD('CompletedQuery');
  FRecvTunnel.DeleteRegistedCMD('CompletedDownloadAssemble');
end;

procedure TDataStoreClient.Progress;
begin
  inherited Progress;
end;

procedure TDataStoreClient.InitDB(inMem: Boolean; dbN: string);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;

  de.WriteBool(inMem);
  de.WriteString(dbN);

  SendTunnel.SendDirectStreamCmd('InitDB', de);
  DisposeObject(de);
end;

procedure TDataStoreClient.CloseDB(dbN: string);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;

  de.WriteString(dbN);

  SendTunnel.SendDirectStreamCmd('CloseDB', de);
  DisposeObject(de);
end;

procedure TDataStoreClient.QueryDB(RegistedQueryName: string; inMem, ReverseQuery: Boolean; dbN, outDBN: string;
  fragmentWait, MaxWait: Double; MaxQueryResult: Int64; BackcallPtr: PDataStoreClientQueryNotify; Values: THashVariantList);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;

  de.WriteString(RegistedQueryName);
  de.WriteBool(inMem);
  de.WriteBool(ReverseQuery);
  de.WriteString(dbN);
  de.WriteString(outDBN);
  de.WriteDouble(fragmentWait);
  de.WriteDouble(MaxWait);
  de.WriteInt64(MaxQueryResult);
  de.WritePointer(BackcallPtr);
  if Values <> nil then
      de.WriteVariantList(Values);

  SendTunnel.SendDirectStreamCmd('QueryDB', de);

  DisposeObject(de);
end;

procedure TDataStoreClient.QueryDB(RegistedQueryName: string; inMem, ReverseQuery: Boolean; dbN, outDBN: string;
  fragmentWait, MaxWait: Double; MaxQueryResult: Int64;
  Values: THashVariantList; OnQueryCall: TFillQueryDataCall; OnDoneCall: TQueryDoneNotifyCall);
var
  p: PDataStoreClientQueryNotify;
begin
  new(p);
  p^.Init;
  p^.OnQueryCall := OnQueryCall;
  p^.OnDoneCall := OnDoneCall;
  QueryDB(RegistedQueryName, inMem, ReverseQuery, dbN, outDBN, fragmentWait, MaxWait, MaxQueryResult, p, Values);
end;

procedure TDataStoreClient.QueryDB(RegistedQueryName: string; inMem, ReverseQuery: Boolean; dbN, outDBN: string;
  fragmentWait, MaxWait: Double; MaxQueryResult: Int64;
  Values: THashVariantList; OnQueryMethod: TFillQueryDataMethod; OnDoneMethod: TQueryDoneNotifyMethod);
var
  p: PDataStoreClientQueryNotify;
begin
  new(p);
  p^.Init;
  p^.OnQueryMethod := OnQueryMethod;
  p^.OnDoneMethod := OnDoneMethod;
  QueryDB(RegistedQueryName, inMem, ReverseQuery, dbN, outDBN, fragmentWait, MaxWait, MaxQueryResult, p, Values);
end;

{$IFNDEF FPC}


procedure TDataStoreClient.QueryDB(RegistedQueryName: string; inMem, ReverseQuery: Boolean; dbN, outDBN: string;
  fragmentWait, MaxWait: Double; MaxQueryResult: Int64;
  Values: THashVariantList; OnQueryProc: TFillQueryDataProc; OnDoneProc: TQueryDoneNotifyProc);
var
  p: PDataStoreClientQueryNotify;
begin
  new(p);
  p^.Init;
  p^.OnQueryProc := OnQueryProc;
  p^.OnDoneProc := OnDoneProc;
  QueryDB(RegistedQueryName, inMem, ReverseQuery, dbN, outDBN, fragmentWait, MaxWait, MaxQueryResult, p, Values);
end;
{$ENDIF}


procedure TDataStoreClient.QueryDB(RegistedQueryName: string; dbN: string; Values: THashVariantList; OnQueryCall: TFillQueryDataCall; OnDoneCall: TQueryDoneNotifyCall);
var
  p: PDataStoreClientQueryNotify;
begin
  new(p);
  p^.Init;
  p^.OnQueryCall := OnQueryCall;
  p^.OnDoneCall := OnDoneCall;
  QueryDB(RegistedQueryName, True, False, dbN, 'temp', 0.5, 0, 0, p, Values);
end;

procedure TDataStoreClient.QueryDB(RegistedQueryName: string; dbN: string; Values: THashVariantList; OnQueryMethod: TFillQueryDataMethod; OnDoneMethod: TQueryDoneNotifyMethod);
var
  p: PDataStoreClientQueryNotify;
begin
  new(p);
  p^.Init;
  p^.OnQueryMethod := OnQueryMethod;
  p^.OnDoneMethod := OnDoneMethod;
  QueryDB(RegistedQueryName, True, False, dbN, 'temp', 0.5, 0, 0, p, Values);
end;

{$IFNDEF FPC}


procedure TDataStoreClient.QueryDB(RegistedQueryName: string; dbN: string; Values: THashVariantList; OnQueryProc: TFillQueryDataProc; OnDoneProc: TQueryDoneNotifyProc);
var
  p: PDataStoreClientQueryNotify;
begin
  new(p);
  p^.Init;
  p^.OnQueryProc := OnQueryProc;
  p^.OnDoneProc := OnDoneProc;
  QueryDB(RegistedQueryName, True, False, dbN, 'temp', 0.5, 0, 0, p, Values);
end;
{$ENDIF}


procedure TDataStoreClient.DownloadDB(ReverseQuery: Boolean; dbN: string; BackcallPtr: PDataStoreClientQueryNotify);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;

  de.WriteBool(ReverseQuery);
  de.WriteString(dbN);
  de.WritePointer(BackcallPtr);

  SendTunnel.SendDirectStreamCmd('DownloadDB', de);

  DisposeObject(de);
end;

procedure TDataStoreClient.DownloadDB(ReverseQuery: Boolean; dbN: string; OnQueryCall: TFillQueryDataCall; OnDoneCall: TQueryDoneNotifyCall);
var
  p: PDataStoreClientQueryNotify;
begin
  new(p);
  p^.Init;
  p^.OnQueryCall := OnQueryCall;
  p^.OnDoneCall := OnDoneCall;
  DownloadDB(ReverseQuery, dbN, p);
end;

procedure TDataStoreClient.DownloadDB(ReverseQuery: Boolean; dbN: string; OnQueryMethod: TFillQueryDataMethod; OnDoneMethod: TQueryDoneNotifyMethod);
var
  p: PDataStoreClientQueryNotify;
begin
  new(p);
  p^.Init;
  p^.OnQueryMethod := OnQueryMethod;
  p^.OnDoneMethod := OnDoneMethod;
  DownloadDB(ReverseQuery, dbN, p);
end;

{$IFNDEF FPC}


procedure TDataStoreClient.DownloadDB(ReverseQuery: Boolean; dbN: string; OnQueryProc: TFillQueryDataProc; OnDoneProc: TQueryDoneNotifyProc);
var
  p: PDataStoreClientQueryNotify;
begin
  new(p);
  p^.Init;
  p^.OnQueryProc := OnQueryProc;
  p^.OnDoneProc := OnDoneProc;
  DownloadDB(ReverseQuery, dbN, p);
end;
{$ENDIF}


procedure TDataStoreClient.BeginAssembleStream;
begin
  ClearBatchStream;
end;

procedure TDataStoreClient.RequestDownloadAssembleStream(dbN: string; StorePos: Int64; BackcallPtr: PDataStoreClientDownloadNotify);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;

  de.WriteString(dbN);
  de.WriteInt64(StorePos);
  de.WritePointer(BackcallPtr);

  SendTunnel.SendDirectStreamCmd('RequestDownloadAssembleStream', de);

  DisposeObject(de);
end;

procedure TDataStoreClient.DownloadAssembleStream(dbN: string; StorePos: Int64; OnDoneCall: TDownloadDoneNotifyCall);
var
  p: PDataStoreClientDownloadNotify;
begin
  new(p);
  p^.Init;
  p^.OnDoneCall := OnDoneCall;

  RequestDownloadAssembleStream(dbN, StorePos, p);
end;

procedure TDataStoreClient.DownloadAssembleStream(dbN: string; StorePos: Int64; OnDoneMethod: TDownloadDoneNotifyMethod);
var
  p: PDataStoreClientDownloadNotify;
begin
  new(p);
  p^.Init;
  p^.OnDoneMethod := OnDoneMethod;

  RequestDownloadAssembleStream(dbN, StorePos, p);
end;

{$IFNDEF FPC}


procedure TDataStoreClient.DownloadAssembleStream(dbN: string; StorePos: Int64; OnDoneProc: TDownloadDoneNotifyProc);
var
  p: PDataStoreClientDownloadNotify;
begin
  new(p);
  p^.Init;
  p^.OnDoneProc := OnDoneProc;

  RequestDownloadAssembleStream(dbN, StorePos, p);
end;
{$ENDIF}


procedure TDataStoreClient.PostAssembleStream(dbN: string; Stream: TCoreClassStream; dID: Cardinal; DoneTimeFree: Boolean);
var
  de: TDataFrameEngine;
begin
  PostBatchStream(Stream, DoneTimeFree);

  de := TDataFrameEngine.Create;
  de.WriteString(dbN);
  de.WriteCardinal(dID);
  SendTunnel.SendDirectStreamCmd('CompletedPostAssembleStream', de);
  DisposeObject(de);
end;

procedure TDataStoreClient.PostAssembleStream(dbN: string; DataSource: TDataFrameEngine);
var
  m: TMemoryStream64;
begin
  m := TMemoryStream64.Create;
  DataSource.EncodeTo(m, True);
  PostAssembleStream(dbN, m, c_DF, True);
end;

procedure TDataStoreClient.PostAssembleStream(dbN: string; DataSource: THashVariantList);
var
  m: TMemoryStream64;
begin
  m := TMemoryStream64.Create;
  DataSource.SaveToStream(m);
  PostAssembleStream(dbN, m, c_VL, True);
end;

procedure TDataStoreClient.PostAssembleStream(dbN: string; DataSource: TSectionTextData);
var
  m: TMemoryStream64;
begin
  m := TMemoryStream64.Create;
  DataSource.SaveToStream(m);
  PostAssembleStream(dbN, m, c_TE, True);
end;

{$IFNDEF FPC}


procedure TDataStoreClient.PostAssembleStream(dbN: string; DataSource: TJsonObject);
var
  m: TMemoryStream64;
begin
  m := TMemoryStream64.Create;
  DataSource.SaveToStream(m);
  PostAssembleStream(dbN, m, c_Json, True);
end;
{$ENDIF}


procedure TDataStoreClient.InsertAssembleStream(dbN: string; dStorePos: Int64; Stream: TCoreClassStream; dID: Cardinal; DoneTimeFree: Boolean);
var
  de: TDataFrameEngine;
begin
  PostBatchStream(Stream, DoneTimeFree);

  de := TDataFrameEngine.Create;
  de.WriteString(dbN);
  de.WriteInt64(dStorePos);
  de.WriteCardinal(dID);
  SendTunnel.SendDirectStreamCmd('CompletedInsertAssembleStream', de);
  DisposeObject(de);
end;

procedure TDataStoreClient.ModifyAssembleStream(dbN: string; dStorePos: Int64; Stream: TCoreClassStream; DoneTimeFree: Boolean; OnResult: TStreamMethod);
var
  de: TDataFrameEngine;
begin
  PostBatchStream(Stream, DoneTimeFree);

  de := TDataFrameEngine.Create;
  de.WriteString(dbN);
  de.WriteInt64(dStorePos);
  SendTunnel.SendStreamCmd('CompletedModifyAssembleStream', de, OnResult);
  DisposeObject(de);
end;

{$IFNDEF FPC}


procedure TDataStoreClient.ModifyAssembleStream(dbN: string; dStorePos: Int64; Stream: TCoreClassStream; DoneTimeFree: Boolean; OnResult: TStreamProc);
var
  de: TDataFrameEngine;
begin
  PostBatchStream(Stream, DoneTimeFree);

  de := TDataFrameEngine.Create;
  de.WriteString(dbN);
  de.WriteInt64(dStorePos);
  SendTunnel.SendStreamCmd('CompletedModifyAssembleStream', de, OnResult);
  DisposeObject(de);
end;
{$ENDIF}


procedure TDataStoreClient.GetPostAssembleStreamState(OnResult: TStreamMethod);
begin
  GetBatchStreamState(OnResult);
end;

{$IFNDEF FPC}


procedure TDataStoreClient.GetPostAssembleStreamState(OnResult: TStreamProc);
begin
  GetBatchStreamState(OnResult);
end;
{$ENDIF}


procedure TDataStoreClient.EndAssembleStream;
begin
  ClearBatchStream;
end;

procedure TDataStoreClient.DeleteData(dbN: string; dStorePos: Int64; OnResult: TStreamMethod);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  de.WriteString(dbN);
  de.WriteInt64(dStorePos);
  SendTunnel.SendStreamCmd('DeleteData', de, OnResult);
  DisposeObject(de);
end;

{$IFNDEF FPC}


procedure TDataStoreClient.DeleteData(dbN: string; dStorePos: Int64; OnResult: TStreamProc);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  de.WriteString(dbN);
  de.WriteInt64(dStorePos);
  SendTunnel.SendStreamCmd('DeleteData', de, OnResult);
  DisposeObject(de);
end;
{$ENDIF}

end.
