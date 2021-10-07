program DatasetSimOnLinuxServer;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  SysUtils,
  Classes,
  Variants,
  CoreClasses,
  ZDBEngine,
  ZDBLocalManager,
  DoStatusIO,
  DataFrameEngine,
  PascalStrings,
  ListEngine,
  UnicodeMixedLib,
  MemoryStream64,
  TextParsing,
  zExpression,
  OpCode,
  CommunicationFramework,
  CommunicationFrameworkDoubleTunnelIO_NoAuth,
  CommunicationFramework_Server_CrossSocket;

{  The demo uses ZDB as the data entity to simulate the transmission operation of dataset
Working idea of remote SQL for dataset:
On the client side, we first create a record. This container is used to store our query results
Then, we send a query command "querymydatabase" to the server and take record as a parameter
Next, the server uses query or its own defined data processing technology to process your query criteria
After the server completes the query and processing of data, the server will feed back according to the combined command of batchstream
Batchstream combined command mode, which is about these four steps
1. Clearbatchstream / / reset the remote batchstream container
2. Postbatchstream / / start transmitting our query results
3. Sendqueryresult / / tells the client that the query result has been transmitted
4. Clearbatchstream / / reset the remote batchstream container
These four steps of sending work are done on the server
The reception and processing of these four steps are done at the client
In this demo, the query and advanced database engine of Delphi are not directly used
In this demo, ZDB is used to simulate query and advanced database query engine. Its working mechanism and implementation idea are the same as that of database engine  }

type
  TMyDoubleServer = class(TCommunicationFramework_DoubleTunnelService_NoAuth)
  public
    zdb: TZDBLocalManager;
    procedure cmd_QueryMyDatabase(Sender: TPeerIO; InData: TDataFrameEngine);
    procedure RegisterCommand; override;
    procedure UnRegisterCommand; override;
  end;

type
  POpCodeRun_Record = ^TOpCodeRun_Record;

  TOpCodeRun_Record = record
    op: TOpCode;
    opR: TOpCustomRunTime;
    sql: TPascalString;
    Remote_backCall: UInt64;
    dPipe: TZDBPipeline;
    qState: PQueryState;
    function zExp_DB(var Param: TOpParam): Variant;
  end;

function TOpCodeRun_Record.zExp_DB(var Param: TOpParam): Variant;
begin
  Result := qState.Eng.VT[qState^.StorePos]['Key'];
end;

procedure TMyDoubleServer.cmd_QueryMyDatabase(Sender: TPeerIO; InData: TDataFrameEngine);
var
  queryRec: POpCodeRun_Record;
begin
  new(queryRec);
  {  Remote callback address  }
  queryRec^.Remote_backCall := InData.Reader.ReadPointer;
  {  Query a local database through SQL. Of course, it can also be a remote database  }
  queryRec^.sql := InData.Reader.ReadString;
  {  Simulate SQL statements using zexpression  }
  queryRec^.dPipe := nil;
  queryRec^.qState := nil;
  queryRec^.opR := TOpCustomRunTime.Create;
  queryRec^.opR.RegOpM('val', queryRec^.zExp_DB);
  queryRec^.opR.RegOpM('value', queryRec^.zExp_DB);
  queryRec^.op := BuildAsOpCode(True, tsPascal, queryRec^.sql, queryRec^.opR);
  if queryRec^.op = nil then
    begin
      Sender.PrintError('build opCode error:' + queryRec^.sql);
      DisposeObject(queryRec^.opR);
      Dispose(queryRec);
      exit;
    end;

  {  SQL databases use similar methods directly  }
  {  Such methods generally have a syntax check before execution  }
  // query.executeSql(sql)
  {  When the query is complete  }
  {  Both Delphi and Lazarus databases have a foundation class called dataset. Query will output query results to a dataset defined by you after executing the query  }
  {  All datasets can be saved. If you don't know how to save them, search the savetostream method and you'll always find it  }
  // dataset.savetoStream(mystream);
  {  Because it is too troublesome to build a module to use the database, I directly use ZDB to simulate the query method of the database. My method is very violent, that is, to package the data  }
  with zdb.QueryDBP(
    True,   {  The query results are written to the return table  }
    True,   {  The returned table of the query is a memory table. If false, it is an entity file table  }
    False,  {  Query from last  }
    'mydb', {  The name of the target database for the query  }
    '',     {  Returns the name of the table. If it is left blank, a temporary table name will be randomly generated  }
    True,   {  Release the return table when the query is complete  }
    30,     {  The delay time, in seconds, to release the returned table when the query is completed  }
    0.1,    {  Fragment accumulation time: when there is a lot of feedback in the query, the feedback event will be triggered every time it is accumulated to facilitate batch operation. During the accumulation time, the data exists in memory  }
    0,      {  Query execution time, 0 is infinite  }
    0,      {  The maximum number of matching query entries, 0 is infinite  }
    1000,   {  Maximum query result feedback, 0 means infinite  }
      procedure(dPipe: TZDBPipeline; var qState: TQueryState; var Allowed: Boolean)
    var
        queryRec_Ptr: POpCodeRun_Record;
    begin
        queryRec_Ptr := dPipe.UserPointer;

        queryRec_Ptr^.dPipe := dPipe;
        queryRec_Ptr^.qState := @qState;
        try
          Allowed := Boolean(queryRec_Ptr^.op.Execute(queryRec_Ptr^.opR));
          if Allowed then
            nop;
        except
          {  If an exception occurs in the query, end the query task immediately  }
          qState.Aborted := True;
          {  You can also end the query task by the following method  }
          // dPipe.stop;
      end;
    end,
    procedure(dPipe: TZDBPipeline)
    var
      queryRec_Ptr: POpCodeRun_Record;
      r_io_def: TPeerClientUserDefineForRecvTunnel_NoAuth;
      de: TDataFrameEngine;
      mystream: TMemoryStream64;
    begin

      queryRec_Ptr := dPipe.UserPointer;
      {  This is all done after the query is completed  }

      {  Release the opruntime class that is no longer used  }
      DisposeObject(queryRec_Ptr^.op);
      DisposeObject(queryRec_Ptr^.opR);

      {  When the query is completed, if the client has dropped the line, if the line drops, the memory will be released and no feedback will be given  }
      if not RecvTunnel.Exists(Cardinal(dPipe.UserVariant)) then
        begin
          Dispose(queryRec_Ptr);
          exit;
        end;

      {  When the query is completed, if the client is still online  }

      r_io_def := GetUserDefineRecvTunnel(RecvTunnel.PeerIO[dPipe.UserVariant]);

      {  Put the query results into mystream. Dpipe.outputdb.savetostream is equivalent to dataset.savetostream  }
      dPipe.OutputDB.Update;
      mystream := TMemoryStream64.Create;
      dPipe.OutputDB.SaveToStream(mystream);

      {  Now, we will send mystream to the client and feed back the statistical results of the client query and the callback trigger event  }

      {  Reset batchstream  }
      ClearBatchStream(r_io_def.SendTunnel.Owner);
      PostBatchStream(r_io_def.SendTunnel.Owner, mystream, True);

      {  Memory address of feedback callback pointer  }
      de := TDataFrameEngine.Create;
      de.WritePointer(queryRec_Ptr^.Remote_backCall);
      de.WriteInteger(dPipe.OutputDB.Count);
      de.WriteString(dPipe.PipelineName);
      r_io_def.SendTunnel.Owner.SendDirectStreamCmd('QueryDone', de);
      DisposeObject(de);

      {  Reset batchstream  }
      ClearBatchStream(r_io_def.SendTunnel.Owner);

      {  Finally, release the queryrec we use_ PTR memory pointer  }
      Dispose(queryRec_Ptr);
    end) do
    begin
      UserVariant := Sender.id;
      UserPointer := queryRec;
    end;
end;

procedure TMyDoubleServer.RegisterCommand;
begin
  inherited;
  FRecvTunnel.RegisterDirectStream('QueryMyDatabase').OnExecute := {$IFDEF FPC}@{$ENDIF FPC}cmd_QueryMyDatabase;
end;

procedure TMyDoubleServer.UnRegisterCommand;
begin
  inherited;
  FRecvTunnel.DeleteRegistedCMD('QueryMyDatabase');
end;

{  Main cycle  }
procedure MainLoop;
var
  serv: TMyDoubleServer;
  rt, st: TCommunicationFrameworkServer;
  {  Simulate entity database usage  }
  i: integer;
  VT: TDBEngineVT;
begin
  doStatus('The demo uses ZDB as the data entity to simulate the transmission operation of dataset' +
    #13#10 +
    'Working idea of remote SQL for dataset:' +
    #13#10 +
    'On the client side, we first create a record. This container is used to store our query results' +
    'Then, we send a query command "querymydatabase" to the server and take record as a parameter' +
    'Next, the server uses query or its own defined data processing technology to process your query criteria' +
    'After the server completes the query and processing of data, the server will feed back according to the combined command of batchstream' +
    #13#10 +
    'Batchstream combined command mode, which is about these four steps' +
    #13#10 +
    '1. Clearbatchstream / / reset the remote batchstream container' +
    '2. Postbatchstream / / start transmitting our query results' +
    '3. Sendqueryresult / / tells the client that the query result has been transmitted' +
    '4. Clearbatchstream / / reset the remote batchstream container' +
    #13#10 +
    'These four steps of sending work are done on the server' +
    'The reception and processing of these four steps are done at the client' +
    #13#10 +
    'In this demo, the query and advanced database engine of Delphi are not directly used' +
    'In this demo, ZDB is used to simulate query and advanced database query engine. Its working mechanism and implementation idea are the same as that of database engine');

  rt := TCommunicationFramework_Server_CrossSocket.Create.StableIO;
  if rt.StartService('', 10991) then
      doStatus('listen %s on port:10991 success', [TranslateBindAddr('')])
  else
      doStatus('listen %s on port:10991 failed', [TranslateBindAddr('')]);

  st := TCommunicationFramework_Server_CrossSocket.Create.StableIO;
  if st.StartService('', 10992) then
      doStatus('listen %s on port:10992 success', [TranslateBindAddr('')])
  else
      doStatus('listen %s on port:10992 failed', [TranslateBindAddr('')]);

  {  We use stable IO technology to build dual channel interactive services  }
  {  Stable IO can automatically disconnect and reconnect without destroying the instance  }
  {  Of course, the client also needs to be stable io  }
  {  This demo is an endless loop service and will not exit, so the server will not be released. There is no memory leakage in stableio  }
  serv := TMyDoubleServer.Create(rt, st);
  serv.RegisterCommand;

  {  Simulated entity database  }
  serv.zdb := TZDBLocalManager.Create;
  {  Simulate database in memory  }
  serv.zdb.InitMemoryDB('mydb');
  {  Create 100000 data records  }
  doStatus('database building...');
  for i := 1 to 1 * 10000 do
    begin
      VT := TDBEngineVT.Create;
      VT['key'] := inttostr(umlRandomRange(-10000, 10000));
      serv.zdb.PostData('mydb', VT);
      DisposeObject(VT);
    end;
  doStatus('database build done,total: %d ,size: %s', [serv.zdb['mydb'].Count, umlSizeToStr(serv.zdb['mydb'].DBEngine.Size).Text]);

  doStatus('server prepare ok.');
  while True do
    begin
      serv.Progress;
      serv.zdb.Progress;
      if serv.RecvTunnel.Count > 0 then
          CoreClasses.CheckThreadSynchronize()
      else
          CoreClasses.CheckThreadSynchronize(10);
    end;
end;

begin
  try
      MainLoop;
  except
    on E: Exception do
        Writeln(E.ClassName, ': ', E.Message);
  end;

end.
