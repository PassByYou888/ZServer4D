{ ****************************************************************************** }
{ * DataStore Service framework(incl File service)                             * }
{ * written by QQ 600585@qq.com                                                * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ ****************************************************************************** }

unit CommunicationFrameworkDataStoreService;

interface

{$I ..\zDefine.inc}


uses Classes,
  CoreClasses, ListEngine, UnicodeMixedLib, DataFrameEngine, MemoryStream64, CommunicationFramework, TextDataEngine,
  DoStatusIO, Cadencer, NotifyObjectBase, PascalStrings, CoreCipher, ZDBEngine, ItemStream,
  CommunicationFrameworkDoubleTunnelIO;

type
  TDataStoreService = class;
  TDataPipeline     = class;

  TDataFilterCall   = function(dPipe: TDataPipeline; var qState: TQueryState; var Allowed: Boolean)  : Boolean;
  TDataFilterMethod = function(dPipe: TDataPipeline; var qState: TQueryState; var Allowed: Boolean): Boolean of object;

  {$IFNDEF FPC}
  TDataFilterProc = reference to function(dPipe: TDataPipeline; var qState: TQueryState; var Allowed: Boolean): Boolean;
  {$ENDIF}

  TDataPipeline = class
  private
    QueryCounter       : Int64;
    CurrentFragmentTime: TTimeTickValue;
    FragmentBuffer     : TMemoryStream64;
    UserBreaked        : Boolean;

    procedure Query(var UserData: Pointer; var qState: TQueryState);
    procedure QueryDone(var UserData: Pointer);

    function WriteToOutput(DBEng: TDBStoreEngine; StorePos: Int64; UserProperty: Cardinal): Int64;
    procedure SendFragmentData;
  public
    Owner         : TDataStoreService;
    RecvID, SendID: Cardinal;
    SourceDB      : TDBStoreEngine;
    OutputDB      : TDBStoreEngine;
    Name          : string;

    Processed: Boolean;

    // data query options
    AutoDestroyDB   : Boolean; // complete time destroy DB
    FragmentWaitTime: Double;  // fragment time,realtime send to client
    MaxWaitTime     : Double;  // max wait complete time,query to abort from out time
    MaxQueryCompare : Int64;   // max query compare
    MaxQueryResult  : Int64;   // max query result

    OnDataFilterCall  : TDataFilterCall;
    OnDataFilterMethod: TDataFilterMethod;
    {$IFNDEF FPC}
    OnDataFilterProc: TDataFilterProc;
    {$ENDIF}
    constructor CreateAsNewMemory(AOwner: TDataStoreService; sourDBName, taskName, OutDBName: string); overload;
    constructor CreateAsFile(AOwner: TDataStoreService; sourDBName, taskName, OutDBName: string); overload;
    destructor Destroy; override;
  end;

  TDataStoreService_PeerClientRecvTunnel = class(TPeerClientUserDefineForRecvTunnel)
  public
    constructor Create(AOwner: TPeerClient); override;
    destructor Destroy; override;
  end;

  TDataStoreService_PeerClientSendTunnel = class(TPeerClientUserDefineForSendTunnel)
  public
    constructor Create(AOwner: TPeerClient); override;
    destructor Destroy; override;
  end;

  TDataStoreService = class(TCommunicationFramework_DoubleTunnelService)
  protected
    FDBPool           : THashObjectList;
    FQueryPipelinePool: THashObjectList;
    FTaskCounter      : Int64;
  protected
    procedure Command_InitDB(Sender: TPeerClient; InData, OutData: TDataFrameEngine); virtual;
    procedure Command_CloseDB(Sender: TPeerClient; InData, OutData: TDataFrameEngine); virtual;

    procedure Command_QueryDB(Sender: TPeerClient; InData, OutData: TDataFrameEngine); virtual;

    procedure Command_RequestDownloadAssembleStream(Sender: TPeerClient; InData, OutData: TDataFrameEngine); virtual;

    // Append data assemble in service
    procedure Command_AppendAssemble(Sender: TPeerClient; InData: TDataFrameEngine); virtual;
    procedure Command_RequestAppendBigStream(Sender: TPeerClient; InData: TDataFrameEngine); virtual;
    procedure Command_AppendAssembleStream(Sender: TPeerClient; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64); virtual;

    // Insert data assemble in service
    procedure Command_InsertAssemble(Sender: TPeerClient; InData: TDataFrameEngine); virtual;
    procedure Command_RequestInsertBigStream(Sender: TPeerClient; InData: TDataFrameEngine); virtual;
    procedure Command_InsertAssembleStream(Sender: TPeerClient; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64); virtual;

    // Modify data assemble in service
    procedure Command_ModifyAssemble(Sender: TPeerClient; InData: TDataFrameEngine); virtual;
    procedure Command_RequestModifyBigStream(Sender: TPeerClient; InData: TDataFrameEngine); virtual;
    procedure Command_ModifyAssembleStream(Sender: TPeerClient; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64); virtual;

    // delete
    procedure Command_DeleteData(Sender: TPeerClient; InData, OutData: TDataFrameEngine); virtual;

    // base information
    procedure Command_GetDBInfo(Sender: TPeerClient; InData, OutData: TDataFrameEngine); virtual;
    procedure Command_GetDBStoreInfo(Sender: TPeerClient; InData, OutData: TDataFrameEngine); virtual;
  public
    constructor Create(ARecvTunnel, ASendTunnel: TCommunicationFrameworkServer);
    destructor Destroy; override;

    procedure RegisterCommand; override;
    procedure UnRegisterCommand; override;

    procedure Progress; override;

    // local operation
    function InitDB(dbN: string; ReadOnly: Boolean): TDBStoreEngine;
    function InitNewDB(dbN: string): TDBStoreEngine;
    function InitMemoryDB(dbN: string): TDBStoreEngine;
    procedure CloseDB(dbN: string);

    function GenerateNewTaskName: string;
    function GetPipeline(pipeName: string): TDataPipeline;
    function GetDB(dN: string): TDBStoreEngine;

    function QueryDB(InMemory, ReverseQuery: Boolean; dbN, OutputDB: string; PeerRecv, PeerSend: TPeerClient;
      AutoDestroyDB: Boolean; FragmentWaitTime, MaxWaitTime: Double;
      MaxQueryCompare, MaxQueryResult: Int64): TDataPipeline; overload;

    // send client command
    procedure Send_BeginQuery(SendID: Cardinal; pipeName: string);
    procedure Send_EndQuery(SendID: Cardinal; pipeName: string);
  end;

  TDataStoreClient = class(TCommunicationFramework_DoubleTunnelClient)
  protected
    procedure Command_BeginQuery(Sender: TPeerClient; InData: TDataFrameEngine); virtual;
    procedure Command_FragmentBigStream(Sender: TPeerClient; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64);
    procedure Command_EndQuery(Sender: TPeerClient; InData: TDataFrameEngine); virtual;
  public
    constructor Create(ARecvTunnel, ASendTunnel: TCommunicationFrameworkClient);
    destructor Destroy; override;

    procedure RegisterCommand; override;
    procedure UnRegisterCommand; override;

    procedure Progress; override;

    procedure PostStreamToDB(dbN: string; stream: TCoreClassStream; UserProperty: Cardinal);
  end;

function GeneratePipeName(const sourDBName, taskName, OutDBName: string): string;

implementation

function GeneratePipeName(const sourDBName, taskName, OutDBName: string): string;
begin
  Result := sourDBName + '.pipe.' + taskName + '.' + OutDBName;
end;

procedure TDataPipeline.Query(var UserData: Pointer; var qState: TQueryState);

var
  AlreadWrite: Boolean;
  Allowed    : Boolean;

  procedure DoWrite;
  begin
    if AlreadWrite then
        exit;

    WriteToOutput(qState.DBEng, qState.StorePos, qState.QueryHnd^.FieldSearch.RHeader.UserProperty);
    AlreadWrite := True;
  end;

begin
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
  inc(QueryCounter);

  if (MaxQueryResult > 0) and (OutputDB.Count >= MaxQueryResult) then
    begin
      qState.Breaked := True;
      exit;
    end;
  if (MaxQueryCompare > 0) and (QueryCounter >= MaxQueryCompare) then
    begin
      qState.Breaked := True;
      exit;
    end;
  if (MaxWaitTime > 0) and (qState.newTime >= Trunc(MaxWaitTime * 1000)) then
    begin
      qState.Breaked := True;
      exit;
    end;

  if (FragmentWaitTime > 0) and (AlreadWrite) then
    begin
      CurrentFragmentTime := CurrentFragmentTime + qState.deltaTime;
      while CurrentFragmentTime >= Trunc(FragmentWaitTime * 1000) do
        begin
          SendFragmentData;
          CurrentFragmentTime := CurrentFragmentTime - Trunc(FragmentWaitTime * 1000);
        end;
    end;

  if UserBreaked then
      qState.Breaked := True;
end;

procedure TDataPipeline.QueryDone(var UserData: Pointer);
begin
  if (FragmentWaitTime > 0) then
      SendFragmentData;

  Processed := False;

  Owner.Send_EndQuery(SendID, name);
end;

function TDataPipeline.WriteToOutput(DBEng: TDBStoreEngine; StorePos: Int64; UserProperty: Cardinal): Int64;
var
  itmStream: TItemStream;
  siz      : Int64;
begin
  itmStream := DBEng.GetData(StorePos, UserProperty);
  Result := OutputDB.AddData(itmStream, UserProperty);
  if FragmentWaitTime > 0 then
    begin
      itmStream.Position := 0;
      siz := itmStream.Size;
      FragmentBuffer.Position := FragmentBuffer.Size;
      FragmentBuffer.WritePtr(@StorePos, umlInt64Length);
      FragmentBuffer.WritePtr(@siz, umlInt64Length);
      FragmentBuffer.CopyFrom(itmStream, siz);
    end;
  DisposeObject(itmStream);
end;

procedure TDataPipeline.SendFragmentData;
var
  c  : TPeerClient;
  m64: TMemoryStream64;
begin
  if FragmentBuffer.Size <= 0 then
      exit;
  c := Owner.FSendTunnel.GetClientFromID(SendID);
  if c <> nil then
    begin
      // send hash code

      // encrypt fragment
      c.Encrypt(c.SendCipherStyle, FragmentBuffer.Memory, FragmentBuffer.Size, c.CipherKeyPtr^, True);

      // compress fragment
      m64 := TMemoryStream64.Create;
      FragmentBuffer.Position := 0;
      MaxCompressStream(FragmentBuffer, m64);
      c.SendBigStream(name, m64, True);
    end;
  FragmentBuffer.Clear;
end;

constructor TDataPipeline.CreateAsNewMemory(AOwner: TDataStoreService; sourDBName, taskName, OutDBName: string);
begin
  inherited Create;
  QueryCounter := 0;
  CurrentFragmentTime := 0;
  FragmentBuffer := TMemoryStream64.Create;
  UserBreaked := False;

  Owner := AOwner;
  RecvID := 0;
  SendID := 0;

  SourceDB := Owner.FDBPool[sourDBName] as TDBStoreEngine;

  name := GeneratePipeName(sourDBName, taskName, OutDBName);
  OutputDB := Owner.InitMemoryDB(name);

  Processed := True;

  // data query options
  AutoDestroyDB := True;   // complete time destroy DB
  FragmentWaitTime := 0.5; // fragment time,realtime send to client
  MaxWaitTime := 0;        // max wait complete time,query to abort from out time
  MaxQueryCompare := 0;    // max query compare
  MaxQueryResult := 0;     // max query result

  OnDataFilterCall := nil;
  OnDataFilterMethod := nil;
  OnDataFilterProc := nil;

  Owner.FQueryPipelinePool[name] := Self;
end;

constructor TDataPipeline.CreateAsFile(AOwner: TDataStoreService; sourDBName, taskName, OutDBName: string);
begin
  inherited Create;
  QueryCounter := 0;
  CurrentFragmentTime := 0;
  FragmentBuffer := TMemoryStream64.Create;
  UserBreaked := False;

  Owner := AOwner;
  RecvID := 0;
  SendID := 0;

  SourceDB := Owner.FDBPool[sourDBName] as TDBStoreEngine;

  name := GeneratePipeName(sourDBName, taskName, OutDBName);
  OutputDB := Owner.InitNewDB(name);

  Processed := True;

  // data query options
  AutoDestroyDB := True;   // complete time destroy DB
  FragmentWaitTime := 0.5; // fragment time,realtime send to client
  MaxWaitTime := 0;        // max wait complete time,query to abort from out time
  MaxQueryCompare := 0;    // max query compare
  MaxQueryResult := 0;     // max query result

  OnDataFilterCall := nil;
  OnDataFilterMethod := nil;
  OnDataFilterProc := nil;

  Owner.FQueryPipelinePool[name] := Self;
end;

destructor TDataPipeline.Destroy;
var
  fn: string;
begin
  Owner.FQueryPipelinePool.Delete(name);

  if AutoDestroyDB then
    begin
      if OutputDB.DBEngine.StreamEngine is TMemoryStream64 then
        begin
          Owner.CloseDB(name);
        end
      else
        begin
          fn := OutputDB.DBEngine.ObjectName;
          Owner.CloseDB(name);
          if umlFileExists(fn) then
              umlDeleteFile(fn);
        end;
    end;

  DisposeObject([FragmentBuffer]);

  inherited Destroy;
end;

constructor TDataStoreService_PeerClientRecvTunnel.Create(AOwner: TPeerClient);
begin
  inherited Create(AOwner);
end;

destructor TDataStoreService_PeerClientRecvTunnel.Destroy;
begin
  inherited Destroy;
end;

constructor TDataStoreService_PeerClientSendTunnel.Create(AOwner: TPeerClient);
begin
  inherited Create(AOwner);
end;

destructor TDataStoreService_PeerClientSendTunnel.Destroy;
begin
  inherited Destroy;
end;

procedure TDataStoreService.Command_InitDB(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
var
  UsedMemDB: Boolean;
  dbN      : string;
begin
  UsedMemDB := InData.Reader.ReadBool;
  dbN := InData.Reader.ReadString;
  if UsedMemDB then
      InitMemoryDB(dbN)
  else
      InitDB(dbN, False);
end;

procedure TDataStoreService.Command_InsertAssemble(Sender: TPeerClient;
  InData: TDataFrameEngine);
begin

end;

procedure TDataStoreService.Command_InsertAssembleStream(Sender: TPeerClient;
  InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64);
begin

end;

procedure TDataStoreService.Command_ModifyAssemble(Sender: TPeerClient;
  InData: TDataFrameEngine);
begin

end;

procedure TDataStoreService.Command_ModifyAssembleStream(Sender: TPeerClient;
  InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64);
begin

end;

procedure TDataStoreService.Command_AppendAssemble(Sender: TPeerClient; InData: TDataFrameEngine);
begin

end;

procedure TDataStoreService.Command_AppendAssembleStream(Sender: TPeerClient; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64);
begin

end;

procedure TDataStoreService.Command_CloseDB(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
var
  dbN: string;
begin
  dbN := InData.Reader.ReadString;
  CloseDB(dbN);
end;

procedure TDataStoreService.Command_DeleteData(Sender: TPeerClient; InData,
  OutData: TDataFrameEngine);
begin

end;

procedure TDataStoreService.Command_GetDBInfo(Sender: TPeerClient; InData,
  OutData: TDataFrameEngine);
begin

end;

procedure TDataStoreService.Command_GetDBStoreInfo(Sender: TPeerClient;
  InData, OutData: TDataFrameEngine);
begin

end;

procedure TDataStoreService.Command_QueryDB(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
begin
end;

procedure TDataStoreService.Command_RequestAppendBigStream(Sender: TPeerClient; InData: TDataFrameEngine);
begin

end;

procedure TDataStoreService.Command_RequestDownloadAssembleStream(
  Sender: TPeerClient; InData, OutData: TDataFrameEngine);
begin

end;

procedure TDataStoreService.Command_RequestInsertBigStream(
  Sender: TPeerClient; InData: TDataFrameEngine);
begin

end;

procedure TDataStoreService.Command_RequestModifyBigStream(
  Sender: TPeerClient; InData: TDataFrameEngine);
begin

end;

constructor TDataStoreService.Create(ARecvTunnel, ASendTunnel: TCommunicationFrameworkServer);
begin
  inherited Create(ARecvTunnel, ASendTunnel);
  FRecvTunnel.PeerClientUserDefineClass := TDataStoreService_PeerClientRecvTunnel;
  FSendTunnel.PeerClientUserDefineClass := TDataStoreService_PeerClientSendTunnel;

  FDBPool := THashObjectList.Create(True, 1024);
  FQueryPipelinePool := THashObjectList.Create(False, 2048);
  FTaskCounter := 1;
end;

destructor TDataStoreService.Destroy;
begin
  DisposeObject([FDBPool, FQueryPipelinePool]);
  inherited Destroy;
end;

procedure TDataStoreService.RegisterCommand;
begin
  inherited RegisterCommand;

  FRecvTunnel.RegisterStream('InitDB').OnExecute := Command_InitDB;
  FRecvTunnel.RegisterStream('CloseDB').OnExecute := Command_CloseDB;
  FRecvTunnel.RegisterStream('QueryDB').OnExecute := Command_QueryDB;
end;

procedure TDataStoreService.UnRegisterCommand;
begin
  inherited UnRegisterCommand;
  FRecvTunnel.DeleteRegistedCMD('InitDB');
  FRecvTunnel.DeleteRegistedCMD('CloseDB');
  FRecvTunnel.DeleteRegistedCMD('QueryDB');
end;

procedure TDataStoreService.Progress;
begin
  inherited Progress;
  Classes.CheckSynchronize;
end;

function TDataStoreService.InitDB(dbN: string; ReadOnly: Boolean): TDBStoreEngine;
var
  fn: umlString;
begin
  fn := umlCombineFileName(RootPath, dbN + '.OX');
  if umlFileExists(fn) then
      Result := TDBStoreEngine.Create(fn, readonly)
  else
      Result := TDBStoreEngine.CreateNew(fn);

  FDBPool[dbN] := Result;
end;

function TDataStoreService.InitNewDB(dbN: string): TDBStoreEngine;
var
  fn: umlString;
begin
  FDBPool.Delete(dbN);
  fn := umlCombineFileName(RootPath, dbN + '.OX');
  Result := TDBStoreEngine.CreateNew(fn);

  FDBPool[dbN] := Result;
end;

function TDataStoreService.InitMemoryDB(dbN: string): TDBStoreEngine;
begin
  Result := TDBStoreEngine.CreateNewMemory;
  FDBPool[dbN] := Result;
end;

procedure TDataStoreService.CloseDB(dbN: string);
begin
  FDBPool.Delete(dbN);
end;

function TDataStoreService.GenerateNewTaskName: string;
begin
  Result := 'Task' + umlIntToStr(FTaskCounter);
  inc(FTaskCounter);
end;

function TDataStoreService.GetPipeline(pipeName: string): TDataPipeline;
begin
  Result := TDataPipeline(FQueryPipelinePool[pipeName]);
end;

function TDataStoreService.GetDB(dN: string): TDBStoreEngine;
begin
  Result := TDBStoreEngine(FDBPool[dN]);
end;

function TDataStoreService.QueryDB(InMemory, ReverseQuery: Boolean; dbN, OutputDB: string; PeerRecv, PeerSend: TPeerClient;
  AutoDestroyDB: Boolean; FragmentWaitTime, MaxWaitTime: Double;
  MaxQueryCompare, MaxQueryResult: Int64): TDataPipeline;
var
  tn: string;
begin
  tn := GenerateNewTaskName;
  if InMemory then
      Result := TDataPipeline.CreateAsNewMemory(Self, dbN, tn, OutputDB)
  else
      Result := TDataPipeline.CreateAsFile(Self, dbN, tn, OutputDB);

  Result.RecvID := PeerRecv.ID;
  Result.SendID := PeerSend.ID;

  Result.AutoDestroyDB := AutoDestroyDB;
  Result.FragmentWaitTime := FragmentWaitTime;
  Result.MaxWaitTime := MaxWaitTime;
  Result.MaxQueryCompare := MaxQueryCompare;
  Result.MaxQueryResult := MaxQueryResult;

  Result.SourceDB.Query(Result.Name, nil, Result.Query, Result.QueryDone);
  Send_BeginQuery(Result.SendID, Result.Name);
end;

procedure TDataStoreService.Send_BeginQuery(SendID: Cardinal; pipeName: string);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  de.WriteString(pipeName);
  FSendTunnel.SendDirectStreamCmd(SendID, 'BeginQuery', de);
  DisposeObject(de);
end;

procedure TDataStoreService.Send_EndQuery(SendID: Cardinal; pipeName: string);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  de.WriteString(pipeName);
  FSendTunnel.SendDirectStreamCmd(SendID, 'EndQuery', de);
  DisposeObject(de);
end;

procedure TDataStoreClient.Command_BeginQuery(Sender: TPeerClient; InData: TDataFrameEngine);
var
  pipeName: string;
begin
  pipeName := InData.Reader.ReadString;
  FRecvTunnel.RegisterDirectStream(pipeName).OnExecute := Command_BeginQuery;
end;

procedure TDataStoreClient.Command_FragmentBigStream(Sender: TPeerClient; InData: TCoreClassStream; BigStreamTotal, BigStreamCompleteSize: Int64);
var
  pipeName: string;
begin
  pipeName := Sender.CurrentBigStreamCommand;
end;

procedure TDataStoreClient.Command_EndQuery(Sender: TPeerClient; InData: TDataFrameEngine);
var
  pipeName: string;
begin
  pipeName := InData.Reader.ReadString;
  FRecvTunnel.DeleteRegistedCMD(pipeName);
end;

constructor TDataStoreClient.Create(ARecvTunnel, ASendTunnel: TCommunicationFrameworkClient);
begin
  inherited Create(ARecvTunnel, ASendTunnel);
end;

destructor TDataStoreClient.Destroy;
begin
  inherited Destroy;
end;

procedure TDataStoreClient.RegisterCommand;
begin
  inherited RegisterCommand;
  FRecvTunnel.RegisterDirectStream('BeginQuery').OnExecute := Command_BeginQuery;
  FRecvTunnel.RegisterDirectStream('EndQuery').OnExecute := Command_EndQuery;
end;

procedure TDataStoreClient.UnRegisterCommand;
begin
  inherited UnRegisterCommand;
  FRecvTunnel.DeleteRegistedCMD('BeginQuery');
  FRecvTunnel.DeleteRegistedCMD('EndQuery');
end;

procedure TDataStoreClient.Progress;
begin
  inherited Progress;
end;

procedure TDataStoreClient.PostStreamToDB(dbN: string; stream: TCoreClassStream; UserProperty: Cardinal);
begin
end;

end.
