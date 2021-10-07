program LargeScale_ZDB_DataBuild;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  SysUtils,
  Math,
  Classes,
  CoreClasses,
  PascalStrings,
  UnicodeMixedLib,
  TextParsing,
  DoStatusIO,
  MemoryStream64,
  ListEngine,
  ZDBEngine,
  ZDBLocalManager;

function DestDBPath: SystemString;
begin
  Result := umlGetCurrentPath;
end;

{  Simulation build.CSV format file  }
procedure BuildRandCSVData;
const
  c_MaxFileSize = Int64(8) * Int64(1024 * 1024); {  CSV file size to build  }
var
  ioHnd: TIOHnd;
  i: Integer;
  n, tmp: U_String;
  c: Int64;
  buff: TBytes;
begin
  DoStatus('Start building large-scale CSV');
  InitIOHnd(ioHnd);
  umlFileCreate(umlCombineFileName(DestDBPath, 'big.csv'), ioHnd);

  // prepare csv header
  n := '';
  for i := 0 to 5 do
      n.Append('%d,', [i]);
  n.DeleteLast;
  n.Append(#13#10);
  buff := n.PlatformBytes;
  umlBlockWrite(ioHnd, buff[0], length(buff));
  SetLength(buff, 0);
  n := '';

  // build csv body
  c := 0;
  while (ioHnd.Size < c_MaxFileSize) do
    begin
      n := '';
      for i := 0 to 5 do
        begin
          tmp := TPascalString.RandomString(umlRandomRange(10, 20));
          tmp.DeleteChar('()[]"' + #39);
          n.Append(tmp + ',');
        end;
      n.DeleteLast;
      n.Append(#13#10);
      buff := n.PlatformBytes;
      umlBlockWrite(ioHnd, buff[0], length(buff));
      SetLength(buff, 0);
      n := '';
      inc(c);

      if c mod 100000 = 0 then
          DoStatus('.CSV building.. completed %s target %s', [umlSizeToStr(ioHnd.Size).Text, umlSizeToStr(c_MaxFileSize).Text]);
    end;

  umlFileClose(ioHnd);
end;

procedure BuildZDB;
var
  LM: TZDBLocalManager;
  db: TZDBLMStore;
  r: TStreamReader;
begin
  DoStatus('Start building large-scale ZDB');
  LM := TZDBLocalManager.Create;
  LM.RootPath := DestDBPath;

  r := TStreamReader.Create(umlCombineFileName(DestDBPath, 'big.csv').Text, TEncoding.UTF8);
  db := LM.InitNewDB('big');

  CustomImportCSV_P(
    procedure(var L: TPascalString; var IsEnd: Boolean)
    begin
      IsEnd := r.EndOfStream;
      if not IsEnd then
          L := r.ReadLine;
    end,
    procedure(const sour: TPascalString; const king, Data: TArrayPascalString)
    var
      i: Integer;
      VT: THashStringList;
    begin
      VT := THashStringList.CustomCreate(16);
      for i := Low(king) to High(king) do
          VT[king[i]] := Data[i];
      db.AddData(VT);
      DisposeObject(VT);

      {  Save data to physical hard disk every 10000 records imported  }
      if db.Count mod 10000 = 0 then
        begin
          DoStatus(' %d builds completed, database size %s, kernel status %s %s',
            [db.Count, umlSizeToStr(db.DBEngine.Size).Text,
            db.CacheAnnealingState, db.DBEngine.CacheStatus]);
        end;

      {  The tzdblocalmanager.progress method can save the database once per second  }
      LM.Progress;

      {  If we do not use tzdblocalmanager.progress, we can also release the cache manually: empty the cache every 200000 records imported  }
      if db.Count mod 200000 = 0 then
        begin
          db.DBEngine.CleaupCache;
        end;
    end);

  DisposeObject(r);
  DisposeObject(LM);
end;

procedure QueryZDB1;
var
  LM: TZDBLocalManager;
  db: TZDBLMStore;
  LVT: TDBListVT;
  tk: TTimeTick;
  i, j: Integer;
begin
  DoStatus('Fast query simulation');
  LM := TZDBLocalManager.Create;
  LM.RootPath := DestDBPath;
  db := LM.InitDB('big');

  {  To traverse big data, we need to increase the hash pool of the kernel to speed up  }
  db.DBEngine.ResetCachePool(100 * 10000);

  {  It takes a long time to load memory query. Please wait patiently  }
  DoStatus('Loading memory', []);
  LVT := TDBListVT.Create;

  {  Synchronous loading. The characteristics of this method are: during data reading, the program is unresponsive, and the loadfromstoreengine method can only work in the main thread  }
  // LVT.LoadFromStoreEngine(db);

  {  Loading data asynchronously is characterized by using the query mechanism. Its performance is a little worse than that of synchronous mode, but it can be asynchronous. We can let the program do other things during loading  }
  {  We create a loading query task and load it in step mode, which is a pseudo asynchronous mode  }
  db.QueryP('loading', False,
    procedure(var qState: TQueryState)
    begin
      if qState.IsVT then
          LVT.Add(qState.Eng.BuildVT(qState));
    end,
    procedure()
    begin
    end);

  {  Wait for the background query task loading to complete  }
  tk := GetTimeTick;
  while db.QueryProcessing do
    begin
      CheckThreadSynchronize(100);
      if GetTimeTick - tk > 1000 then
        begin
          DoStatus('Loading status %D / %d kernel status %s %s', [LVT.Count, db.Count, db.CacheAnnealingState, db.DBEngine.CacheStatus]);
          tk := GetTimeTick;
        end;
    end;

  {  After loading, the query will be very fast. Hundreds of millions of data are queried in seconds  }
  tk := GetTimeTick;
  for j := 0 to 99 do
    for i := 0 to LVT.Count - 1 do
      begin
        CompareText(LVT[i]['1'], 'abc');
      end;
  DoStatus('Average query time in memory mode %DMS', [(GetTimeTick - tk) div 100]);
  DisposeObject(LVT);

  DoStatus('Quick query simulation completed');
  DisposeObject(LM);
end;

procedure QueryZDB2;
var
  LM: TZDBLocalManager;
  db: TZDBLMStore;
  tk: TTimeTick;
  i: Integer;
begin
  DoStatus('Background query simulation');
  LM := TZDBLocalManager.Create;
  LM.RootPath := DestDBPath;
  db := LM.InitDB('big');

  {  To traverse big data, we need to increase the hash pool of the kernel to speed up  }
  db.DBEngine.ResetCachePool(100 * 10000);

  {  Traversal query  }
  {  With the help of annealing engine, multi task query will be faster than single task  }
  {  Simulate 200 simultaneous query tasks  }
  for i := 0 to 200 - 1 do
    begin
      LM.QueryDBP(
        False,                                   {  Write query results to a temporary database  }
      True,                                      {  The temporary database is in memory mode  }
      Odd(MT19937Rand32(MaxInt)),                {  Random positive and negative direction query  }
      db.Name,                                   {  Target database  }
      '',                                        {  The name of the temporary database. If this name is empty, it is a random name  }
      True,                                      {  The new database of query output will be released automatically  }
      0.0,                                       {  Delay time to release temporary database  }
      0,                                         {  Fragment data feedback time, which is the parameter provided to the online mechanism, the ZDB of CS architecture  }
      ifThen(i = 0, 70, umlRandomRangeD(1, 70)), {  Limit query time, random XX XX seconds  }
      0,                                         {  Limit traversal records for maximum queries  }
      0,                                         {  Limit the return record of the maximum query  }
        procedure(dPipe: TZDBPipeline; var qState: TQueryState; var Allowed: Boolean)
        begin
          Allowed := True;
        end,
        procedure(dPipe: TZDBPipeline)
        begin
          DoStatus('%S completed %d record queries within %s time limit',
            [dPipe.PipelineName, umlTimeTickToStr(round(dPipe.QueryConsumTime * 1000)).Text, dPipe.QueryCounter]);
        end
        );
    end;

  {  Wait for the background query task to complete  }
  tk := GetTimeTick;
  while db.QueryProcessing do
    begin
      LM.Progress;
      CheckThreadSynchronize(100);
      if GetTimeTick - tk > 1000 then
        begin
          DoStatus('Kernel status %s %s', [db.CacheAnnealingState, db.DBEngine.CacheStatus]);
          tk := GetTimeTick;
        end;
    end;

  DoStatus('Background query simulation completed');
  DisposeObject(LM);
end;

procedure QueryZDB3;
var
  LM: TZDBLocalManager;
  db: TZDBLMStore;
  tk: TTimeTick;
  i: Integer;
begin
  DoStatus('Simulation of large-scale buffer query using kernel cache');
  LM := TZDBLocalManager.Create;
  LM.RootPath := DestDBPath;
  db := LM.InitDB('big');

  {  To traverse big data, we need to increase the hash pool of the kernel to speed up  }
  db.DBEngine.ResetCachePool(100 * 10000);

  {  This method is to directly shut down the annealing engine and buffer all instances into the ZDB kernel  }
  {  After the buffer is completed, the query speed is very fast  }
  db.CacheStyle := csAlways;
  {  Start buffering task  }
  LM.QueryDBP(False, {  Write query results to a temporary database  }
  True,              {  The temporary database is in memory mode  }
  True,              {  Positive direction query  }
  db.Name,           {  Target database  }
  '',                {  The name of the temporary database. If this name is empty, it is a random name  }
  True,              {  The new database of query output will be released automatically  }
  0.0,               {  Delay time to release temporary database  }
  0,                 {  Fragment data feedback time, which is the parameter provided to the online mechanism, the ZDB of CS architecture  }
  0,                 {  Limit query time, 0 is wireless  }
  0,                 {  Limit traversal records for maximum queries  }
  0,                 {  Limit the return record of the maximum query  }
    procedure(dPipe: TZDBPipeline; var qState: TQueryState; var Allowed: Boolean)
    begin
      Allowed := False;
      {  The getvt method is its own buffered instance  }
      qState.Eng.GetVT(qState);
    end,
    procedure(dPipe: TZDBPipeline)
    begin
    end
    );

  {  Wait for the background query task to complete  }
  tk := GetTimeTick;
  while db.QueryProcessing do
    begin
      LM.Progress;
      CheckThreadSynchronize(100);
      if GetTimeTick - tk > 1000 then
        begin
          DoStatus('Kernel buffer status %s %s', [db.CacheAnnealingState, db.DBEngine.CacheStatus]);
          tk := GetTimeTick;
        end;
    end;
  DoStatus('All data instances have been buffered');

  DoStatus('Start simulating 2 query tasks');
  {  Simulate two simultaneous query tasks  }
  for i := 0 to 2 - 1 do
    begin
      LM.QueryDBP(False,          {  Write query results to a temporary database  }
      True,                       {  The temporary database is in memory mode  }
      Odd(MT19937Rand32(MaxInt)), {  Random positive and negative direction query  }
      db.Name,                    {  Target database  }
      '',                         {  The name of the temporary database. If this name is empty, it is a random name  }
      True,                       {  The new database of query output will be released automatically  }
      0.0,                        {  Delay time to release temporary database  }
      0,                          {  Fragment data feedback time, which is the parameter provided to the online mechanism, the ZDB of CS architecture  }
      0,                          {  Limit query time, 0 is infinite  }
      0,                          {  Limit traversal records for maximum queries  }
      0,                          {  Limit the return record of the maximum query  }
        procedure(dPipe: TZDBPipeline; var qState: TQueryState; var Allowed: Boolean)
        begin
          if qState.IsVT then
              Allowed := CompareText(qState.Eng.VT[qState.StorePos]['0'], 'abc') > 0;
        end,
        procedure(dPipe: TZDBPipeline)
        begin
          DoStatus('%s in %s finished querying %d records in %s time',
            [dPipe.PipelineName, umlTimeTickToStr(round(dPipe.QueryConsumTime * 1000)).Text, dPipe.QueryCounter]);
        end
        );
    end;

  {  Wait for the background query task to complete  }
  while db.QueryProcessing do
    begin
      LM.Progress;
      CheckThreadSynchronize(10);
    end;

  DoStatus('The large - scale buffer query using kernel cache has been completed');
  DisposeObject(LM);
end;

procedure QueryZDB4;
var
  LM: TZDBLocalManager;
  db: TZDBLMStore;
  tk: TTimeTick;
  arry: TStoreArray;
  i, j: Integer;
begin
  DoStatus('Using storage address query simulation');
  LM := TZDBLocalManager.Create;
  LM.RootPath := DestDBPath;
  db := LM.InitDB('big');

  {  To traverse big data, we need to increase the hash pool of the kernel to speed up  }
  db.DBEngine.ResetCachePool(100 * 10000);

  {  This method is to directly shut down the annealing engine and buffer all instances into the ZDB kernel  }
  {  After the buffer is completed, the query speed is very fast  }
  db.CacheStyle := csAlways;
  {  Start buffering task  }
  LM.QueryDBP(False, {  Write query results to a temporary database  }
  True,              {  The temporary database is in memory mode  }
  True,              {  Positive direction query  }
  db.Name,           {  Target database  }
  '',                {  The name of the temporary database. If this name is empty, it is a random name  }
  True,              {  The new database of query output will be released automatically  }
  0.0,               {  Delay time to release temporary database  }
  0,                 {  Fragment data feedback time, which is the parameter provided to the online mechanism, the ZDB of CS architecture  }
  0,                 {  Limit query time, 0 is wireless  }
  0,                 {  Limit traversal records for maximum queries  }
  0,                 {  Limit the return record of the maximum query  }
    procedure(dPipe: TZDBPipeline; var qState: TQueryState; var Allowed: Boolean)
    begin
      Allowed := False;
      {  The getvt method is its own buffered instance  }
      qState.Eng.GetVT(qState);
    end,
    procedure(dPipe: TZDBPipeline)
    begin
    end
    );

  {  Wait for the background query task to complete  }
  tk := GetTimeTick;
  while db.QueryProcessing do
    begin
      LM.Progress;
      CheckThreadSynchronize(100);
      if GetTimeTick - tk > 1000 then
        begin
          DoStatus('Kernel buffer status %s %s', [db.CacheAnnealingState, db.DBEngine.CacheStatus]);
          tk := GetTimeTick;
        end;
    end;
  DoStatus('All data instances have been buffered');

  DoStatus('Build storage address array');
  db.BuildStoreArray(False, @arry);

  DoStatus('The storage address is being used for 100 traversal queries');
  tk := GetTimeTick;
  for j := 0 to 100 - 1 do
    for i := Low(arry) to high(arry) do
        CompareText(db.VT[arry[i]]['0'], 'abc');
  DoStatus('The query has been completed, and the average time of complete traversal is %DMS', [(GetTimeTick - tk) div 100]);

  DoStatus('Query with storage address completed');
  DisposeObject(LM);
end;

begin
  BuildRandCSVData;
  BuildZDB;
  QueryZDB1;
  QueryZDB2;
  QueryZDB3;
  QueryZDB4;
  DoStatus('Press enter to exit');
  readln;

end.
