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
  DoStatusIO,
  MemoryStream64,
  ListEngine,
  ZDBEngine,
  ZDBLocalManager;

function DestDBPath: SystemString;
begin
  Result := umlGetCurrentPath;
end;

// 模拟构建.CSV格式文件
procedure BuildRandCSVData;
const
  c_MaxFileSize = Int64(16) * Int64(1024 * 1024); // 需要构建的csv文件尺寸
var
  ioHnd: TIOHnd;
  i: Integer;
  n: U_String;
  c: Int64;
  buff: TBytes;
begin
  DoStatus('开始构建大规模csv');
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
          n.Append(TPascalString.RandomString(umlRandomRange(5, 20)) + ',');
      n.DeleteLast;
      n.Append(#13#10);
      buff := n.PlatformBytes;
      umlBlockWrite(ioHnd, buff[0], length(buff));
      SetLength(buff, 0);
      n := '';
      inc(c);

      if c mod 100000 = 0 then
          DoStatus('.CSV 构建中.. 已经完成 %s 目标 %s', [umlSizeToStr(ioHnd.Size).Text, umlSizeToStr(c_MaxFileSize).Text]);
    end;

  umlFileClose(ioHnd);
end;

procedure BuildZDB;
var
  LM: TZDBLocalManager;
  db: TZDBLMStore;
  r: TStreamReader;
begin
  DoStatus('开始构建大规模ZDB');
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

      // 每导入10000条记录，保存数据到物理硬盘
      if db.Count mod 10000 = 0 then
        begin
          DoStatus('已完成 %d 条构建, 数据库尺寸 %s 内核状态 %s %s',
            [db.Count, umlSizeToStr(db.DBEngine.Size).Text,
            db.CacheAnnealingState, db.DBEngine.CacheStatus]);
        end;

      // TZDBLocalManager.Progress方法可以每秒保存一次数据库
      LM.Progress;

      // 如果当我们不使用TZDBLocalManager.Progress时，也可以手动释放cache：每导入20万条记录时清空一次cache
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
  DoStatus('快速查询模拟');
  LM := TZDBLocalManager.Create;
  LM.RootPath := DestDBPath;
  db := LM.InitDB('big');

  // 遍历大数据我们需要加大内核的hash池来提速
  db.DBEngine.SetPoolCache(100 * 10000);

  // 载入内存方式查询，需要的时间时间很长，耐心等待
  DoStatus('正在载入内存', []);
  LVT := TDBListVT.Create;

  // 同步方式载入，这种方式的特点：在读取数据期间，程序是无响应的，LoadFromStoreEngine方法只能工作在主线程中
  // LVT.LoadFromStoreEngine(db);

  // 异步方式载入数据，这种方式的特点就是用查询机制，它性能比同步方式方式要差一点，但是可以是的异步，我们在载入期间可以让程序做别的事情
  // 我们创建一个loading的查询任务，在该任务中以step方式载入，这是假异步方式
  db.QueryP('loading', False,
    procedure(var qState: TQueryState)
    begin
      if qState.IsVT then
          LVT.Add(qState.Eng.BuildVT(qState));
    end,
    procedure()
    begin
    end);

  // 等待后台查询任务loading完成
  tk := GetTimeTick;
  while db.QueryProcessing do
    begin
      CheckThreadSynchronize(100);
      if GetTimeTick - tk > 1000 then
        begin
          DoStatus('loading 载入状态 %d/%d 内核状态 %s %s', [LVT.Count, db.Count, db.CacheAnnealingState, db.DBEngine.CacheStatus]);
          tk := GetTimeTick;
        end;
    end;

  // 载入完成后，这时查询就会非常快了，上亿条数据都是秒查
  tk := GetTimeTick;
  for j := 0 to 99 do
    for i := 0 to LVT.Count - 1 do
      begin
        CompareText(LVT[i]['1'], 'abc');
      end;
  DoStatus('内存方式平均查询耗时%dms', [(GetTimeTick - tk) div 100]);
  DisposeObject(LVT);

  DoStatus('快速查询模拟已完成.');
  DisposeObject(LM);
end;

procedure QueryZDB2;
var
  LM: TZDBLocalManager;
  db: TZDBLMStore;
  tk: TTimeTick;
  i: Integer;
begin
  DoStatus('后台查询模拟');
  LM := TZDBLocalManager.Create;
  LM.RootPath := DestDBPath;
  db := LM.InitDB('big');

  // 遍历大数据我们需要加大内核的hash池来提速
  db.DBEngine.SetPoolCache(100 * 10000);

  // 遍历方式查询
  // 在退火引擎的帮助下，多任务查询会比单任务更快
  // 模拟200个同时查询的任务
  for i := 0 to 200 - 1 do
    begin
      LM.QueryDBP(
        False,                                   // 将查询结果写入到一个临时数据库
      True,                                      // 临时数据库是内存模式
      Odd(MT19937Rand32(MaxInt)),                // 随机正反方向查询
      db.Name,                                   // 目标数据库
      '',                                        // 临时数据库名字，这个名字给空就是随机名字
      True,                                      // 查询输出的新数据库会自动被释放
      0.0,                                       // 释放临时数据库的延迟时间
      0,                                         // 碎片数据反馈时间,这是提供给online机制使用的参数,cs架构的ZDB
      ifThen(i = 0, 70, umlRandomRangeD(1, 70)), // 限制查询时间，随机xx-xx秒
      0,                                         // 限制最大查询的遍历记录
      0,                                         // 限制最大查询的返回记录
        procedure(dPipe: TZDBPipeline; var qState: TQueryState; var Allowed: Boolean)
        begin
          Allowed := True;
        end,
        procedure(dPipe: TZDBPipeline)
        begin
          DoStatus('%s 在%s时限中 完成 %d 条记录查询',
            [dPipe.PipelineName, umlTimeTickToStr(round(dPipe.QueryConsumTime * 1000)).Text, dPipe.QueryCounter]);
        end
        );
    end;

  // 等待后台查询任务完成
  tk := GetTimeTick;
  while db.QueryProcessing do
    begin
      LM.Progress;
      CheckThreadSynchronize(100);
      if GetTimeTick - tk > 1000 then
        begin
          DoStatus('内核状态 %s %s', [db.CacheAnnealingState, db.DBEngine.CacheStatus]);
          tk := GetTimeTick;
        end;
    end;

  DoStatus('后台查询模拟已完成.');
  DisposeObject(LM);
end;

procedure QueryZDB3;
var
  LM: TZDBLocalManager;
  db: TZDBLMStore;
  tk: TTimeTick;
  i: Integer;
begin
  DoStatus('利用内核cache大规模缓冲查询模拟');
  LM := TZDBLocalManager.Create;
  LM.RootPath := DestDBPath;
  db := LM.InitDB('big');

  // 遍历大数据我们需要加大内核的hash池来提速
  db.DBEngine.SetPoolCache(100 * 10000);

  // 这种方式是直接关闭退火引擎，把实例全部缓冲到ZDB的内核中
  // 在缓冲完成后，查询速度非常快
  db.CacheStyle := csAlways;
  // 开始缓冲任务
  LM.QueryDBP(False, // 将查询结果写入到一个临时数据库
  True,              // 临时数据库是内存模式
  True,              // 正方向查询
  db.Name,           // 目标数据库
  '',                // 临时数据库名字，这个名字给空就是随机名字
  True,              // 查询输出的新数据库会自动被释放
  0.0,               // 释放临时数据库的延迟时间
  0,                 // 碎片数据反馈时间,这是提供给online机制使用的参数,cs架构的ZDB
  0,                 // 限制查询时间，0是无线
  0,                 // 限制最大查询的遍历记录
  0,                 // 限制最大查询的返回记录
    procedure(dPipe: TZDBPipeline; var qState: TQueryState; var Allowed: Boolean)
    begin
      Allowed := False;
      // GetVT方法是自己缓冲实例
      qState.Eng.GetVT(qState);
    end,
    procedure(dPipe: TZDBPipeline)
    begin
    end
    );

  // 等待后台查询任务完成
  tk := GetTimeTick;
  while db.QueryProcessing do
    begin
      LM.Progress;
      CheckThreadSynchronize(100);
      if GetTimeTick - tk > 1000 then
        begin
          DoStatus('内核缓冲状态 %s %s', [db.CacheAnnealingState, db.DBEngine.CacheStatus]);
          tk := GetTimeTick;
        end;
    end;
  DoStatus('全数据实例已经缓冲完成.');

  DoStatus('开始模拟2个查询任务.');
  // 模拟2个同时查询的任务
  for i := 0 to 2 - 1 do
    begin
      LM.QueryDBP(False,          // 将查询结果写入到一个临时数据库
      True,                       // 临时数据库是内存模式
      Odd(MT19937Rand32(MaxInt)), // 随机正反方向查询
      db.Name,                    // 目标数据库
      '',                         // 临时数据库名字，这个名字给空就是随机名字
      True,                       // 查询输出的新数据库会自动被释放
      0.0,                        // 释放临时数据库的延迟时间
      0,                          // 碎片数据反馈时间,这是提供给online机制使用的参数,cs架构的ZDB
      0,                          // 限制查询时间，0是无限
      0,                          // 限制最大查询的遍历记录
      0,                          // 限制最大查询的返回记录
        procedure(dPipe: TZDBPipeline; var qState: TQueryState; var Allowed: Boolean)
        begin
          if qState.IsVT then
              Allowed := CompareText(qState.Eng.VT[qState.StorePos]['0'], 'abc') > 0;
        end,
        procedure(dPipe: TZDBPipeline)
        begin
          DoStatus('%s 在%s时间 完成 %d 条记录查询',
            [dPipe.PipelineName, umlTimeTickToStr(round(dPipe.QueryConsumTime * 1000)).Text, dPipe.QueryCounter]);
        end
        );
    end;

  // 等待后台查询任务完成
  while db.QueryProcessing do
    begin
      LM.Progress;
      CheckThreadSynchronize(10);
    end;

  DoStatus('利用内核cache大规模缓冲查询已完成.');
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
  DoStatus('利用存储地址查询模拟');
  LM := TZDBLocalManager.Create;
  LM.RootPath := DestDBPath;
  db := LM.InitDB('big');

  // 遍历大数据我们需要加大内核的hash池来提速
  db.DBEngine.SetPoolCache(100 * 10000);

  // 这种方式是直接关闭退火引擎，把实例全部缓冲到ZDB的内核中
  // 在缓冲完成后，查询速度非常快
  db.CacheStyle := csAlways;
  // 开始缓冲任务
  LM.QueryDBP(False, // 将查询结果写入到一个临时数据库
  True,              // 临时数据库是内存模式
  True,              // 正方向查询
  db.Name,           // 目标数据库
  '',                // 临时数据库名字，这个名字给空就是随机名字
  True,              // 查询输出的新数据库会自动被释放
  0.0,               // 释放临时数据库的延迟时间
  0,                 // 碎片数据反馈时间,这是提供给online机制使用的参数,cs架构的ZDB
  0,                 // 限制查询时间，0是无线
  0,                 // 限制最大查询的遍历记录
  0,                 // 限制最大查询的返回记录
    procedure(dPipe: TZDBPipeline; var qState: TQueryState; var Allowed: Boolean)
    begin
      Allowed := False;
      // GetVT方法是自己缓冲实例
      qState.Eng.GetVT(qState);
    end,
    procedure(dPipe: TZDBPipeline)
    begin
    end
    );

  // 等待后台查询任务完成
  tk := GetTimeTick;
  while db.QueryProcessing do
    begin
      LM.Progress;
      CheckThreadSynchronize(100);
      if GetTimeTick - tk > 1000 then
        begin
          DoStatus('内核缓冲状态 %s %s', [db.CacheAnnealingState, db.DBEngine.CacheStatus]);
          tk := GetTimeTick;
        end;
    end;
  DoStatus('全数据实例已经缓冲完成.');

  DoStatus('构建存储地址数组');
  db.BuildStoreArray(False, @arry);

  DoStatus('正在使用存储地址做遍历方式查询100次');
  tk := GetTimeTick;
  for j := 0 to 100 - 1 do
    for i := Low(arry) to high(arry) do
        CompareText(db.VT[arry[i]]['0'], 'abc');
  DoStatus('查询已经完成，完整遍历平均耗时 %dms', [(GetTimeTick - tk) div 100]);

  DoStatus('利用存储地址查询已完成.');
  DisposeObject(LM);
end;

begin
  BuildRandCSVData;
  BuildZDB;
  QueryZDB1;
  QueryZDB2;
  QueryZDB3;
  QueryZDB4;
  DoStatus('回车键退出.');
  readln;
end.
