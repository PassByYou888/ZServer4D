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
    Memo1: TMemo;
    QueryButton: TButton;
    Timer1: TTimer;
    InsertButton: TButton;
    DeleteButton: TButton;
    ModifyButton: TButton;
    CompressButton: TButton;
    RecacheButton: TButton;
    ListBox1: TListBox;
    Timer2: TTimer;
    procedure buildTempDataButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure QueryButtonClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure InsertButtonClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure ModifyButtonClick(Sender: TObject);
    procedure CompressButtonClick(Sender: TObject);
    procedure RecacheButtonClick(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
  private
    { Private declarations }
    procedure CreateQuery(pipe: TZDBPipeline);
    procedure QueryFragmentData(pipe: TZDBPipeline; FragmentSource: TMemoryStream64);
    procedure QueryDone(pipe: TZDBPipeline);
    procedure CreateDB(ActiveDB: TZDBStoreEngine);
    procedure CloseDB(ActiveDB: TZDBStoreEngine);
    procedure InsertData(Sender: TZDBStoreEngine; InsertPos: Int64; buff: TCoreClassStream; ID: Cardinal; CompletePos: Int64);
    procedure AddData(Sender: TZDBStoreEngine; buff: TCoreClassStream; ID: Cardinal; CompletePos: Int64);
    procedure ModifyData(Sender: TZDBStoreEngine; const StorePos: Int64; buff: TCoreClassStream);
    procedure DeleteData(Sender: TZDBStoreEngine; const StorePos: Int64);
  public
    { Public declarations }
    zdb: TZDBLocalManager;
    procedure DoStatusNear(AText: string; const ID: Integer);
  end;

var
  ZDBmanagerForm: TZDBmanagerForm;

implementation

{$R *.dfm}


procedure TZDBmanagerForm.buildTempDataButtonClick(Sender: TObject);
var
  i, j, k: Integer;
  n      : TPascalString;
  df     : TDBEngineDF;
  ms     : TMemoryStream64;
  d      : TTimeTickValue;
  tc     : Int64;
begin
  doStatus('build struct...');
  d := GetTimeTickCount;

  Randomize;

  tc := 0;

  enabled := False;

  for i := 1 to 500000 do
    begin
      df := TDBEngineDF.Create;
      for j := 1 to RandomRange(10, 25) do
          df.WriteDouble(umlRandomRangeD(-2000.0, 2000));
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
      Caption := Format('total:%s complete:%s', [umlSizeToStr(500000).Text, umlSizeToStr(i).Text]);
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
  p := zdb.QueryDB(True, False, 'Test', 'output', True, 1, 0.1, 0, 0, 0);
  p.OnDataFilterProc := procedure(dPipe: TZDBPipeline; var qState: TQueryState; var Allowed: Boolean)
    var
      c: Integer;
      d: Double;
    begin
      c := qState.DBEng.GetDF(qState).Count;
      d := qState.DBEng.GetDF(qState).ReadDouble(0);
      Allowed := InRange(d, 0.0, 100);
    end;
end;

procedure TZDBmanagerForm.Timer1Timer(Sender: TObject);
begin
  zdb.Progress;
end;

procedure TZDBmanagerForm.Timer2Timer(Sender: TObject);
var
  i  : Integer;
  lst: TCoreClassListForObj;
  db : TZDBStoreEngine;
  pl : TZDBPipeline;
begin
  lst := TCoreClassListForObj.Create;
  zdb.GetDBList(lst);

  ListBox1.Clear;

  ListBox1.Items.Add('database...');
  for i := 0 to lst.Count - 1 do
    begin
      db := TZDBStoreEngine(lst[i]);
      ListBox1.Items.Add(Format('db: %s total items:%d', [db.name, db.Count]));
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
  FillFragmentSource(pipe.SourceDB.name, pipe.PipelineName, FragmentSource,
    procedure(dbN, pipeN: SystemString; StorePos: Int64; ID: Cardinal; DataSour: TMemoryStream64)
    var
      df: TDataFrameEngine;
    begin
      df := pipe.SourceDB.GetDF(StorePos);
    end);
end;

procedure TZDBmanagerForm.RecacheButtonClick(Sender: TObject);
begin
  zdb.Recache;
end;

procedure TZDBmanagerForm.QueryDone(pipe: TZDBPipeline);
begin
  doStatus('query done!');
end;

procedure TZDBmanagerForm.CreateDB(ActiveDB: TZDBStoreEngine);
begin
  doStatus('create db:%s', [ActiveDB.name]);
end;

procedure TZDBmanagerForm.CloseDB(ActiveDB: TZDBStoreEngine);
begin
  doStatus('close db:%s', [ActiveDB.name]);
end;

procedure TZDBmanagerForm.CompressButtonClick(Sender: TObject);
var
  DBEng: TDBStoreBase;
begin
  DBEng := zdb.DBName['testdb'];
  doStatus('size:%s', [umlSizeToStr(DBEng.DBEngine.StreamEngine.Size).Text]);
  DBEng.Compress;
  doStatus('size:%s', [umlSizeToStr(DBEng.DBEngine.StreamEngine.Size).Text]);
end;

procedure TZDBmanagerForm.InsertButtonClick(Sender: TObject);
var
  DBEng: TDBStoreBase;
  lst  : TListInt64;
begin
  DBEng := zdb.InitMemoryDB('testdb');

  lst := TListInt64.Create;

  DBEng.AddData('1');
  DBEng.AddData('2');
  DBEng.AddData('3');
  DBEng.AddData('4');
  DBEng.AddData('5');

  doStatus('db count:', [DBEng.Count]);

  DBEng.WaitQuery(False,
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

  DBEng.WaitQuery(False,
    procedure(var qs: TQueryState)
    begin
      if qs.IsString then
          doStatus('item:%s', [DBEng.GetString(qs.StorePos).Text]);
    end);

  DisposeObject([lst]);
end;

procedure TZDBmanagerForm.InsertData(Sender: TZDBStoreEngine; InsertPos: Int64; buff: TCoreClassStream; ID: Cardinal; CompletePos: Int64);
begin

end;

procedure TZDBmanagerForm.AddData(Sender: TZDBStoreEngine; buff: TCoreClassStream; ID: Cardinal; CompletePos: Int64);
begin

end;

procedure TZDBmanagerForm.ModifyButtonClick(Sender: TObject);
var
  p: TZDBPipeline;
begin
  p := zdb.QueryDB(True, False, 'testDB', 'testoutput', True, 1.0, 1.0, 0, 0, 0);

  p.OnDataFilterProc := procedure(dPipe: TZDBPipeline; var qState: TQueryState; var Allowed: Boolean)
    begin
      if qState.IsString then
          qState.DBEng.PascalString[qState.StorePos] := IntToStr(dPipe.QueryCounter);
    end;
  p.OnDataDoneProc := procedure(dPipe: TZDBPipeline)
    begin
      doStatus('');
      doStatus('');
      doStatus('');
      dPipe.SourceDB.WaitQuery(False,
        procedure(var qState: TQueryState)
        begin
          if qState.IsString then
              doStatus('item:%s', [qState.DBEng.GetString(qState.StorePos).Text]);
        end);
    end;
end;

procedure TZDBmanagerForm.ModifyData(Sender: TZDBStoreEngine; const StorePos: Int64; buff: TCoreClassStream);
begin

end;

procedure TZDBmanagerForm.DeleteButtonClick(Sender: TObject);
var
  DBEng: TDBStoreBase;
begin
  DBEng := zdb.InitMemoryDB('testdb');
  doStatus('db count:', [DBEng.Count]);

  DBEng.WaitQuery(False,
    procedure(var qs: TQueryState)
    begin
      if qs.IsString then
        if umlMultipleMatch(True, 'insert*', DBEng.PascalString[qs.StorePos]) then
            DBEng.DeleteData(qs.StorePos);
    end);

  doStatus('');
  doStatus('');
  doStatus('');

  DBEng.WaitQuery(False,
    procedure(var qs: TQueryState)
    begin
      if qs.IsString then
          doStatus('item:%s', [DBEng.GetString(qs.StorePos).Text]);
    end);
end;

procedure TZDBmanagerForm.DeleteData(Sender: TZDBStoreEngine; const StorePos: Int64);
begin

end;

procedure TZDBmanagerForm.DoStatusNear(AText: string; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
end;

end.
