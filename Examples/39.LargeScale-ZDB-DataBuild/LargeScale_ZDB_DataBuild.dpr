program LargeScale_ZDB_DataBuild;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  SysUtils,
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
  Result := 'x:\';
end;

// 模拟构建.CSV格式文件
procedure BuildRandCSVData;
const
  c_MaxFileSize = Int64(128) * Int64(1024 * 1024); // 需要构建的csv文件尺寸
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
  db: TZDBStoreEngine;
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

      // 大数据导入需要避免cache太多
      if db.Count mod 100000 = 0 then
        begin
          db.DBEngine.CleaupCache;
        end;
    end);

  DisposeObject(r);
  DisposeObject(LM);
end;

procedure QueryZDB;
var
  LM: TZDBLocalManager;
  db: TZDBStoreEngine;
  LVT: TDBListVT;
  i, j: Integer;
  tk: TTimeTick;
begin
  DoStatus('查询模拟');
  LM := TZDBLocalManager.Create;
  LM.RootPath := DestDBPath;
  db := LM.InitDB('big');

  // 遍历大数据我们需要加大内核的hash池来提速
  db.DBEngine.SetPoolCache(100 * 10000);

  // 载入内存方式查询
  if False then
    begin
      DoStatus('正在载入内存', []);
      LVT := TDBListVT.Create;
      // 全部载入内存的时间会很长
      LVT.LoadFromStoreEngine(db);
      // 载入完成后，查询会非常快
      tk := GetTimeTick;
      for j := 0 to 99 do
        for i := 0 to LVT.Count - 1 do
          begin
            CompareText(LVT[i]['1'], 'abc');
          end;
      DoStatus('内存方式平均查询耗时%dms', [(GetTimeTick - tk) div 100]);
      DisposeObject(LVT);
    end;

  // 遍历方式查询
  // 在退火引擎的帮助下，多任务查询会比单任务更快

  // 模拟3个同时查询的任务
  for i := 0 to 3 - 1 do
    begin
      LM.QueryDBP(False,          // 将查询结果写入到一个临时数据库
      True,                       // 临时数据库是内存模式
      Odd(MT19937Rand32(MaxInt)), // 随机正反方向查询
      db.Name,                    // 目标数据库
      '',                         // 临时数据库名字，这个名字给空就是随机名字
      True,                       // 查询输出的新数据库会自动被释放
      0.0,                        // 释放临时数据库的延迟时间
      0,                          // 碎片数据反馈时间
      0,                          // umlRandomRangeD(10, 60),    // 限制查询时间，随机xx-xx秒
      0,                          // 限制最大查询的遍历记录
      0,                          // 限制最大查询的返回记录
        procedure(dPipe: TZDBPipeline; var qState: TQueryState; var Allowed: Boolean)
        begin
          Allowed := True;
          if qState.IsVT then
              qState.dbEng.VT[qState.StorePos];
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

  DoStatus('所有查询任务已完成.');
  DisposeObject(LM);
end;

begin
  BuildRandCSVData;
  BuildZDB;
  QueryZDB;
end.
