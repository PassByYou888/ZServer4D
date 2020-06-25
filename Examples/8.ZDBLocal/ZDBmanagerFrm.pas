unit ZDBmanagerFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.Math,
  ZDBLocalManager, CoreClasses, DoStatusIO, PascalStrings, ZDBEngine,
  MemoryStream64, UnicodeMixedLib, DataFrameEngine, Vcl.ExtCtrls,
  ListEngine;

type
  TZDBmanagerForm = class(TForm, IZDBLocalManagerNotify)
    buildTempDataButton: TButton;
    QueryButton: TButton;
    Timer1: TTimer;
    InsertButton: TButton;
    DeleteButton: TButton;
    ModifyButton: TButton;
    CompressButton: TButton;
    StopButton: TButton;
    Timer2: TTimer;
    Panel1: TPanel;
    Memo1: TMemo;
    ListBox1: TListBox;
    Splitter1: TSplitter;
    PrintButton: TButton;
    ReverseQueryButton: TButton;
    QueryAndDeleteButton: TButton;
    QueryAndModifyButton: TButton;
    QueryAndAnalysisButton: TButton;
    procedure buildTempDataButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure QueryButtonClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure InsertButtonClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure ModifyButtonClick(Sender: TObject);
    procedure CompressButtonClick(Sender: TObject);
    procedure StopButtonClick(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure PrintButtonClick(Sender: TObject);
    procedure ReverseQueryButtonClick(Sender: TObject);
    procedure QueryAndDeleteButtonClick(Sender: TObject);
    procedure QueryAndModifyButtonClick(Sender: TObject);
    procedure QueryAndAnalysisButtonClick(Sender: TObject);
  private
    { Private declarations }
    procedure CreateQuery(pipe: TZDBPipeline);
    procedure QueryFragmentData(pipe: TZDBPipeline; FragmentSource: TMemoryStream64);
    procedure QueryDone(pipe: TZDBPipeline);
    procedure CreateDB(ActiveDB: TZDBLMStore);
    procedure CloseDB(ActiveDB: TZDBLMStore);
    procedure InsertData(Sender: TZDBLMStore; InsertPos: Int64; buff: TCoreClassStream; ID: Cardinal; CompletePos: Int64);
    procedure AddData(Sender: TZDBLMStore; buff: TCoreClassStream; ID: Cardinal; CompletePos: Int64);
    procedure ModifyData(Sender: TZDBLMStore; const StorePos: Int64; buff: TCoreClassStream);
    procedure DeleteData(Sender: TZDBLMStore; const StorePos: Int64);
  public
    { Public declarations }
    zdb: TZDBLocalManager;
    procedure DoStatusNear(AText: SystemString; const ID: Integer);
  end;

var
  ZDBmanagerForm: TZDBmanagerForm;

implementation

{$R *.dfm}


procedure TZDBmanagerForm.buildTempDataButtonClick(Sender: TObject);
var
  i, j, k: Integer;
  n: TPascalString;
  df: TDBEngineDF;
  ms: TMemoryStream64;
  d: TTimeTick;
  tc: Int64;
begin
  doStatus('build struct...');
  d := GetTimeTickCount;

  Randomize;

  tc := 0;

  enabled := False;

  for i := 1 to 50000 do
    begin
      df := TDBEngineDF.Create;
      for j := 1 to RandomRange(10, 25) do
          df.WriteDouble(umlRandomRangeD(-2000.0, 2000.0));
      for j := 1 to RandomRange(10, 25) do
        begin
          n.Len := umlRandomRange(5, 40);
          for k := 1 to n.Len do
              n[k] := Char(umlRandomRange(Ord('a'), Ord('z')));
          df.WriteString(n.Text);
        end;
      zdb.PostData('Test', df);
      inc(tc, df.Count);
      DisposeObject(df);
      Caption := Format('total:%s complete:%s data:%s', [umlSizeToStr(50000).Text, umlSizeToStr(i).Text, umlSizeToStr(zdb['Test'].DBEngine.Size).Text]);
      application.ProcessMessages;
    end;

  doStatus('build struct (%s of data) time:%dms', [umlSizeToStr(tc).Text, GetTimeTickCount - d]);
  enabled := True;
  Caption := 'ZDB Local...';
end;

procedure TZDBmanagerForm.FormCreate(Sender: TObject);
begin
  zdb := TZDBLocalManager.Create;
  zdb.NotifyIntf := Self;
  zdb.InitDB('Test', False);
  zdb.LoadDB(False);
  AddDoStatusHook(Self, DoStatusNear);
end;

procedure TZDBmanagerForm.FormDestroy(Sender: TObject);
begin
  DisposeObject(zdb);
end;

procedure TZDBmanagerForm.QueryButtonClick(Sender: TObject);
var
  p: TZDBPipeline;
begin
  p := zdb.QueryDB(True, True, False, 'Test', 'output_' + TimeToStr(Now), True, 1, 0.1, 0, 0, 0);
  p.OnDataFilterProc := procedure(dPipe: TZDBPipeline; var qState: TQueryState; var Allowed: Boolean)
    begin
      if qState.IsDF then
        with qState.Eng.GetDF(qState) do
            Allowed := InRange(ReadDouble(0), -100, 100);
    end;
end;

procedure TZDBmanagerForm.Timer1Timer(Sender: TObject);
begin
  zdb.Progress;
end;

procedure TZDBmanagerForm.Timer2Timer(Sender: TObject);
var
  i: Integer;
  lst: TCoreClassListForObj;
  db: TZDBLMStore;
  pl: TZDBPipeline;
begin
  lst := TCoreClassListForObj.Create;
  zdb.GetDBList(lst);

  ListBox1.Clear;

  ListBox1.Items.Add('database...');
  for i := 0 to lst.Count - 1 do
    begin
      db := TZDBLMStore(lst[i]);
      ListBox1.Items.Add(Format('db: %s total items:%d size:%s %s', [db.name, db.Count, umlSizeToStr(db.DBEngine.Size).Text, db.CacheAnnealingState]));
    end;

  lst.Clear;
  ListBox1.Items.Add('query pipeline...');
  zdb.GetPipeList(lst);
  for i := 0 to lst.Count - 1 do
    begin
      pl := TZDBPipeline(lst[i]);
      ListBox1.Items.Add(Format('name: %s query performance:%f', [pl.PipelineName, pl.QueryCounterOfPerSec]));
    end;

  DisposeObject(lst);
end;

procedure TZDBmanagerForm.CreateQuery(pipe: TZDBPipeline);
begin
end;

procedure TZDBmanagerForm.QueryFragmentData(pipe: TZDBPipeline; FragmentSource: TMemoryStream64);
begin
  // performance test
  FillFragmentSourceP(pipe.SourceDB.name, pipe.PipelineName, FragmentSource,
    procedure(dbN, pipeN: SystemString; StorePos: Int64; ID: Cardinal; DataSour: TMemoryStream64)
    var
      df: TDataFrameEngine;
    begin
      df := pipe.SourceDB.GetDF(StorePos);
    end);
end;

procedure TZDBmanagerForm.ReverseQueryButtonClick(Sender: TObject);
var
  p: TZDBPipeline;
begin
  p := zdb.QueryDB(False, True, True, 'Test', 'output', True, 1, 0.1, 0, 0, 0);
  p.OnDataFilterProc := procedure(dPipe: TZDBPipeline; var qState: TQueryState; var Allowed: Boolean)
    begin
      if qState.IsDF then
        with qState.Eng.GetDF(qState) do
            Allowed := InRange(ReadDouble(0), -100, 100);
    end;
end;

procedure TZDBmanagerForm.StopButtonClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to zdb.QueryPipelineList.Count - 1 do
    begin
      TZDBPipeline(zdb.QueryPipelineList[i]).Stop;
    end;
end;

procedure TZDBmanagerForm.QueryDone(pipe: TZDBPipeline);
begin
  doStatus('query done!');
end;

procedure TZDBmanagerForm.CreateDB(ActiveDB: TZDBLMStore);
begin
  doStatus('create db:%s', [ActiveDB.name]);
end;

procedure TZDBmanagerForm.CloseDB(ActiveDB: TZDBLMStore);
begin
  doStatus('close db:%s', [ActiveDB.name]);
end;

procedure TZDBmanagerForm.CompressButtonClick(Sender: TObject);
var
  DBEng: TDBStore;
begin
  if zdb.ExistsDB('testdb') then
    begin
      DBEng := zdb.DBName['testdb'];
      doStatus('size:%s', [umlSizeToStr(DBEng.DBEngine.StreamEngine.Size).Text]);
      DBEng.Compress;
      doStatus('size:%s', [umlSizeToStr(DBEng.DBEngine.StreamEngine.Size).Text]);
    end;

  zdb.CompressDB('Test');
  // zdb.CopyDB('Test', 'NewTest');
  // zdb.ReplaceDB('Test', 'NewTest');
end;

procedure TZDBmanagerForm.InsertButtonClick(Sender: TObject);
var
  DBEng: TDBStore;
  lst: TListInt64;
begin
  DBEng := zdb.InitMemoryDB('testdb');

  lst := TListInt64.Create;

  DBEng.AddData('1');
  DBEng.AddData('2');
  DBEng.AddData('3');
  DBEng.AddData('4');
  DBEng.AddData('5');

  doStatus('db count:', [DBEng.Count]);

  DBEng.WaitQueryP(False,
    procedure(var qs: TQueryState)
    begin
      if qs.IsString then
          lst.Add(qs.StorePos);
    end);

  DBEng.InsertData(lst[lst.Count - 1], 'insert data1');
  DBEng.InsertData(lst[lst.Count - 1], 'insert data2');
  DBEng.InsertData(lst[lst.Count - 1], 'insert data3');
  DBEng.InsertData(lst[lst.Count - 1], 'insert data4');

  doStatus('');
  doStatus('');
  doStatus('');

  DBEng.WaitQueryP(False,
    procedure(var qs: TQueryState)
    begin
      if qs.IsString then
          doStatus('item:%s', [DBEng.GetString(qs.StorePos).Text]);
    end);

  DisposeObject([lst]);
end;

procedure TZDBmanagerForm.InsertData(Sender: TZDBLMStore; InsertPos: Int64; buff: TCoreClassStream; ID: Cardinal; CompletePos: Int64);
begin

end;

procedure TZDBmanagerForm.AddData(Sender: TZDBLMStore; buff: TCoreClassStream; ID: Cardinal; CompletePos: Int64);
begin

end;

procedure TZDBmanagerForm.ModifyButtonClick(Sender: TObject);
var
  p: TZDBPipeline;
begin
  p := zdb.QueryDB(False, True, False, 'testDB', 'testoutput', True, 1.0, 1.0, 0, 0, 0);
  if p = nil then
      exit;

  p.OnDataFilterProc := procedure(dPipe: TZDBPipeline; var qState: TQueryState; var Allowed: Boolean)
    begin
      if qState.IsString then
          qState.Eng.PascalString[qState.StorePos] := IntToStr(dPipe.QueryCounter);
    end;
  p.OnDataDoneProc := procedure(dPipe: TZDBPipeline)
    begin
      doStatus('');
      doStatus('');
      doStatus('');
      dPipe.SourceDB.WaitQueryP(False,
        procedure(var qState: TQueryState)
        begin
          if qState.IsString then
              doStatus('item:%s', [qState.Eng.GetString(qState.StorePos).Text]);
        end);
    end;
end;

procedure TZDBmanagerForm.ModifyData(Sender: TZDBLMStore; const StorePos: Int64; buff: TCoreClassStream);
begin

end;

procedure TZDBmanagerForm.PrintButtonClick(Sender: TObject);
var
  p: TZDBPipeline;
begin
  p := zdb.QueryDB(False, True, False, 'testDB', 'testoutput', True, 1.0, 1.0, 0, 0, 0);
  if p = nil then
      exit;

  p.OnDataFilterProc := procedure(dPipe: TZDBPipeline; var qState: TQueryState; var Allowed: Boolean)
    begin
      doStatus('%d:%s', [dPipe.QueryCounter, qState.Eng.GetString(qState.StorePos).Text]);
    end;
  p.OnDataDoneProc := procedure(dPipe: TZDBPipeline)
    begin
    end;

  doStatus('');
  doStatus('');
  doStatus('');
end;

procedure TZDBmanagerForm.QueryAndAnalysisButtonClick(Sender: TObject);
var
  p: TZDBPipeline;
begin
  if zdb.ExistsDB('Analysis') then
      exit;

  p := zdb.QueryDB(True, False, True, 'Test', 'Analysis', True, 1.0, 0.1, 0, 0, 0);
  p.OnDataFilterProc := procedure(dPipe: TZDBPipeline; var qState: TQueryState; var Allowed: Boolean)
    var
      df: TDataFrameEngine;
    begin
      if not qState.IsDF then
          exit;
      df := qState.Eng.GetDF(qState);
      if InRange(df.ReadDouble(0), 1, 10) then
        begin
          Allowed := True;
        end;
    end;
  p.OnDataDoneProc := procedure(dPipe: TZDBPipeline)
    var
      AnalysisPipe: TZDBPipeline;
      sum: Double;
    begin
      doStatus('Analysis finished!');
      sum := 0;
      AnalysisPipe := zdb.QueryDB(False, True, True, 'Analysis', 'temp', True, 1, 0.1, 0, 0, 0);
      AnalysisPipe.OnDataFilterProc := procedure(dPipe: TZDBPipeline; var qState: TQueryState; var Allowed: Boolean)
        begin
          with qState.Eng.GetDF(qState) do
              sum := sum + ReadDouble(0);
        end;
      AnalysisPipe.OnDataDoneProc := procedure(dPipe: TZDBPipeline)
        begin
          doStatus('符合1.0到10.0之间条件的条目总和:%f', [sum]);
          doStatus('符合1.0到10.0之间条件的条目总数:%d', [zdb['Analysis'].Count]);
        end;
    end;
end;

procedure TZDBmanagerForm.QueryAndDeleteButtonClick(Sender: TObject);
var
  p: TZDBPipeline;
begin
  p := zdb.QueryDB(False, True, True, 'Test', 'output', True, 1, 0.1, 0, 0, 0);
  p.OnDataFilterProc := procedure(dPipe: TZDBPipeline; var qState: TQueryState; var Allowed: Boolean)
    begin
      if qState.IsDF then
        with qState.Eng.GetDF(qState) do
          if InRange(ReadDouble(0), -100, 100) then
              qState.Eng.DeleteData(qState.StorePos);
    end;
end;

procedure TZDBmanagerForm.QueryAndModifyButtonClick(Sender: TObject);
var
  p: TZDBPipeline;
begin
  p := zdb.QueryDB(True, True, False, 'Test', 'output', True, 1, 0.1, 0, 0, 0);
  p.OnDataFilterProc := procedure(dPipe: TZDBPipeline; var qState: TQueryState; var Allowed: Boolean)
    begin
      if qState.IsDF then
        with qState.Eng.GetDF(qState) do
          if InRange(ReadDouble(0), -200, 200) then
            begin
              TDataFrameDouble(Data[0]).Buffer := umlRandomRangeD(-100.0, 100.0);
              Save;
            end;
    end;
end;

procedure TZDBmanagerForm.DeleteButtonClick(Sender: TObject);
var
  DBEng: TDBStore;
begin
  DBEng := zdb.InitMemoryDB('testdb');
  doStatus('db count:', [DBEng.Count]);

  DBEng.WaitQueryP(False,
    procedure(var qs: TQueryState)
    begin
      if qs.IsString then
        if umlMultipleMatch(True, 'insert*', DBEng.PascalString[qs.StorePos]) then
            DBEng.DeleteData(qs.StorePos);
    end);

  DBEng.WaitQueryThread;

  doStatus('');
  doStatus('');
  doStatus('');

  DBEng.WaitQueryP(False,
    procedure(var qs: TQueryState)
    begin
      if qs.IsString then
          doStatus('item:%s', [DBEng.GetString(qs.StorePos).Text]);
    end);
end;

procedure TZDBmanagerForm.DeleteData(Sender: TZDBLMStore; const StorePos: Int64);
begin

end;

procedure TZDBmanagerForm.DoStatusNear(AText: SystemString; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
end;

end.
