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

{
  该Demo使用ZDB作为数据实体，模拟了Dataset的传输操作

  Dataset的远程sql工作思路:

  我们在客户端，先创建一个 Record，这个容器用于存放我们的查询结果
  然后，我们发送一个查询命令 "QueryMyDatabase" 到服务器，同时，将Record作为参数
  接下来，服务器使用query或则自己定义的数据处理工艺，对你的查询条件进行处理
  当服务器对数据的查询处理完成后，服务器会按照 BatchStream 的组合命令反馈

  BatchStream组合命令模式，大概就是这4步

  1,ClearBatchStream  //复位远程BatchStream容器
  2,PostBatchStream   //开始传输我们的查询结果
  3,SendQueryResult   //告诉客户端，查询结果已经传输完成
  4,ClearBatchStream  //复位远程BatchStream容器

  这4步的发送工作，都在服务器干
  这4步的接收和处理，都在客户端干

  在该Demo中，没有直接使用delphi自带的Query和高级的数据库引擎
  在该Demo中，使用zdb来模拟query和高级数据库查询引擎，其工作机制和实现思路与数据库引擎相同
}

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
  // 远程回调地址
  queryRec^.Remote_backCall := InData.Reader.ReadPointer;
  // 通过sql查询一个本地数据库，当然，也可以是远程的数据库
  queryRec^.sql := InData.Reader.ReadString;
  // 使用zExpression模拟sql语句
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

  // sql数据库直接使用类似方法
  // 这类方法一般在执行前，都会有个语法检查
  // query.executeSql(sql)
  // 当查询完成后
  // delphi和lazarus的数据库都有一个地基类，叫Dataset，Query在执行查询后，都会往一个你定义的Dataset里面输出查询结果
  // dataset都能保存，如果不知道怎么保存，就去搜索SaveToStream的方法，总会找到
  // dataset.savetoStream(mystream);
  // 由于使用数据库需要建Module，过于麻烦，我直接使用zdb来模拟数据库的查询方式，我的方法很暴力，就是把数据打包
  with zdb.QueryDBP(
    True,   // 查询结果写入到返回表
    True,   // 查询的返回表是内存表，如果是False就是一个实体的文件表
    False,  // 从最后开始查询
    'mydb', // 查询的目标数据库名称
    '',     // 返回表的名称，这里给空会随机生成一个临时的表名字
    True,   // 查询完成时，释放返回表
    30,     // 查询完成时，释放返回表的延迟时间，单位是秒
    0.1,    // 碎片积累时间，当查询有很多反馈时，每积累到这个时间，就触发反馈事件，便于批量操作，在积累时间中，数据都存在于内存
    0,      // 查询执行时间,0是无限
    0,      // 最大的查询条目匹配数量，0是无限
    1000,   // 最大的查询结果反馈，0表示无限
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
          // 如果查询中发生异常，立即结束查询任务
          qState.Aborted := True;
          // 也可以通过下面的方法结束查询任务
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
      // 这里都是当查询完成后干的事

      // 释放不再使用的OpRunTime类
      DisposeObject(queryRec_Ptr^.op);
      DisposeObject(queryRec_Ptr^.opR);

      // 当查询完成后，如果可客户端已经掉线，如果掉线，释放内存，不反馈了
      if not RecvTunnel.Exists(Cardinal(dPipe.UserVariant)) then
        begin
          Dispose(queryRec_Ptr);
          exit;
        end;

      // 当查询完成后，如果可客户端还在线

      r_io_def := GetUserDefineRecvTunnel(RecvTunnel.PeerIO[dPipe.UserVariant]);

      // 将查询的结果放到mystream中，dPipe.OutputDB.SaveToStream等同于Dataset.SaveToStream
      dPipe.OutputDB.Update;
      mystream := TMemoryStream64.Create;
      dPipe.OutputDB.SaveToStream(mystream);

      // 现在，我们要将myStream发送给客户端同时，反馈给客户端查询的统计结果和回调触发事件

      // 复位BatchStream
      ClearBatchStream(r_io_def.SendTunnel.Owner);
      PostBatchStream(r_io_def.SendTunnel.Owner, mystream, True);

      // 反馈回调指针的内存地址
      de := TDataFrameEngine.Create;
      de.WritePointer(queryRec_Ptr^.Remote_backCall);
      de.WriteInteger(dPipe.OutputDB.Count);
      de.WriteString(dPipe.PipelineName);
      r_io_def.SendTunnel.Owner.SendDirectStreamCmd('QueryDone', de);
      DisposeObject(de);

      // 复位BatchStream
      ClearBatchStream(r_io_def.SendTunnel.Owner);

      // 最后，释放我们使用的 queryRec_Ptr 内存指针
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

{ 主循环 }
procedure MainLoop;
var
  serv: TMyDoubleServer;
  rt, st: TCommunicationFrameworkServer;
  // 模拟实体数据库使用
  i: integer;
  VT: TDBEngineVT;
begin
  doStatus('  该Demo使用ZDB作为数据实体，模拟了Dataset的传输操作'#13#10 +
    #13#10 +
    '  Dataset的远程sql工作思路:'#13#10 +
    #13#10 +
    '  我们在客户端，先创建一个 Record，这个容器用于存放我们的查询结果'#13#10 +
    '  然后，我们发送一个查询命令 "QueryMyDatabase" 到服务器，同时，将Record作为参数'#13#10 +
    '  接下来，服务器使用query或则自己定义的数据处理工艺，对你的查询条件进行处理'#13#10 +
    '  当服务器对数据的查询处理完成后，服务器会按照 BatchStream 的组合命令反馈'#13#10 +
    #13#10 +
    '  BatchStream组合命令模式，大概就是这4步'#13#10 +
    #13#10 +
    '  1,ClearBatchStream  //复位远程BatchStream容器'#13#10 +
    '  2,PostBatchStream   //开始传输我们的查询结果'#13#10 +
    '  3,SendQueryResult   //告诉客户端，查询结果已经传输完成'#13#10 +
    '  4,ClearBatchStream  //复位远程BatchStream容器'#13#10 +
    #13#10 +
    '  这4步的发送工作，都在服务器干'#13#10 +
    '  这4步的接收和处理，都在客户端干'#13#10 +
    #13#10 +
    '  在该Demo中，没有直接使用delphi自带的Query和高级的数据库引擎'#13#10 +
    '  在该Demo中，使用zdb来模拟query和高级数据库查询引擎，其工作机制和实现思路与数据库引擎相同'#13#10);

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

  // 我们使用StableIO技术来构建双通道的交互服务
  // StableIO可以在不破坏实例的前提下，自动化断线重连
  // 当然，客户端也需要是StableIO的方式
  // 这个Demo是死循环服务，不会退出，所以不释放服务器，StableIO是没有内存泄漏的
  serv := TMyDoubleServer.Create(rt, st);
  serv.RegisterCommand;

  // 模拟实体数据库
  serv.zdb := TZDBLocalManager.Create;
  // 模拟数据库在内存中
  serv.zdb.InitMemoryDB('mydb');
  // 创建10万条数据记录
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
