{ ****************************************************************************** }
{ * Video Data store formula (platform compatible)                             * }
{ * by QQ 600585@qq.com                                                        * }
{ ****************************************************************************** }
{ * https://zpascal.net                                                        * }
{ * https://github.com/PassByYou888/zAI                                        * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/PascalString                               * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zChinese                                   * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/FFMPEG-Header                              * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/InfiniteIoT                                * }
{ * https://github.com/PassByYou888/FastMD5                                    * }
{ ****************************************************************************** }
unit ZDBVideoStoreFormula;

{$INCLUDE zDefine.inc}

interface

uses SysUtils, Types, TypInfo,
{$IFDEF FPC}
  FPCGenericStructlist,
{$ENDIF FPC}
  CoreClasses, PascalStrings, UnicodeMixedLib, DoStatusIO, MemoryStream64, ListEngine,
  ObjectData, ObjectDataManager, ZDBEngine, ZDBLocalManager;

type
  TVideoType = (vtH264, vtMJPEG, vtMotion, vtAny);

  TVideoZDBStoreData = record
    VT: TDBEngineVT;
    VideoStreamPos: Int64;
    function Classifier: U_String;
    function token: U_String;
    function VideoType: TVideoType;
    function Date_: TDate;
    function Time_: TTime;
  end;

  PVideoZDBStoreData = ^TVideoZDBStoreData;

  TVideoZDBStoreDataList_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<PVideoZDBStoreData>;

  TVideoZDBStoreDataList = class(TVideoZDBStoreDataList_Decl)
  public
    procedure Clean;
  end;

  TVideoZDBStoreFormula = class
  protected
    FDBEngine: TDBStore;
    FList: TVideoZDBStoreDataList;
    procedure QueryCall_(var qState: TQueryState);
  public
    constructor Create;
    destructor Destroy; override;
    procedure OpenDB(DBFile_: SystemString; OnlyRead_: Boolean); overload;
    procedure OpenDB(DBFile_: SystemString); overload;
    procedure CreateDB(DBFile_: SystemString);
    procedure CloseDB;
    procedure Compression;
    procedure Refresh;
    procedure SaveVideoStream(Classifier, token: SystemString; VideoType: TVideoType; stream: TCoreClassStream);
    procedure SaveVideoFile(Classifier, token: SystemString; VideoType: TVideoType; fileName: U_String);
    function Count: Integer;
    function GetItems(index: Integer): PVideoZDBStoreData;
    property Items[index: Integer]: PVideoZDBStoreData read GetItems; default;
    function Delete(index: Integer; removeDB: Boolean): Boolean;
  end;

const
  C_Video_ID: Cardinal = $00000080;

implementation

function TVideoZDBStoreData.Classifier: U_String;
begin
  Result := VT.GetDefaultValue('classifier', '');
end;

function TVideoZDBStoreData.token: U_String;
begin
  Result := VT.GetDefaultValue('token', '');
end;

function TVideoZDBStoreData.VideoType: TVideoType;
var
  n: U_String;
  V: TVideoType;
begin
  n := VT.GetDefaultValue('videoType', '');
  if n = '' then
      exit(vtAny);
  for V := Low(TVideoType) to High(TVideoType) do
    if n.Same(GetEnumName(TypeInfo(TVideoType), Ord(V))) then
        exit(V);
end;

function TVideoZDBStoreData.Date_: TDate;
var
  Format_: TFormatSettings;
begin
  Format_ := {$IFDEF FPC} DefaultFormatSettings {$ELSE FPC} TFormatSettings.Create {$ENDIF FPC};
  Format_.ShortDateFormat := 'ee/mm/dd';
  Format_.DateSeparator := '/';
  Format_.ShortTimeFormat := 'hh-nn-ss';
  Format_.TimeSeparator := '-';
  Result := StrToDate(VT.GetDefaultValue('Date', ''), Format_);
end;

function TVideoZDBStoreData.Time_: TTime;
var
  Format_: TFormatSettings;
begin
  Format_ := {$IFDEF FPC} DefaultFormatSettings {$ELSE FPC} TFormatSettings.Create {$ENDIF FPC};
  Format_.ShortDateFormat := 'ee/mm/dd';
  Format_.DateSeparator := '/';
  Format_.ShortTimeFormat := 'hh-nn-ss';
  Format_.TimeSeparator := '-';
  Result := StrToTime(VT.GetDefaultValue('Time', ''), Format_);
end;

procedure TVideoZDBStoreDataList.Clean;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      dispose(Items[i]);
  Clear;
end;

procedure TVideoZDBStoreFormula.QueryCall_(var qState: TQueryState);
var
  p: PVideoZDBStoreData;
begin
  if qState.IsVT then
    begin
      new(p);
      p^.VT := qState.Eng.GetVT(qState.StorePos);
      p^.VideoStreamPos := qState.PrevPos;
      FList.Add(p);
    end;
end;

constructor TVideoZDBStoreFormula.Create;
begin
  inherited Create;
  FDBEngine := nil;
  FList := TVideoZDBStoreDataList.Create;
end;

destructor TVideoZDBStoreFormula.Destroy;
begin
  CloseDB;
  DisposeObject(FList);
  inherited Destroy;
end;

procedure TVideoZDBStoreFormula.OpenDB(DBFile_: SystemString; OnlyRead_: Boolean);
begin
  CloseDB();
  if umlFileExists(DBFile_) then
      FDBEngine := TDBStore.Create(DBFile_, OnlyRead_)
  else
      FDBEngine := TDBStore.CreateNew(DBFile_);
  FDBEngine.CacheStyle := csAlways;
  Refresh;
end;

procedure TVideoZDBStoreFormula.OpenDB(DBFile_: SystemString);
begin
  OpenDB(DBFile_, False);
end;

procedure TVideoZDBStoreFormula.CreateDB(DBFile_: SystemString);
begin
  CloseDB();
  FDBEngine := TDBStore.CreateNew(DBFile_);
  FDBEngine.CacheStyle := csAlways;
  Refresh;
end;

procedure TVideoZDBStoreFormula.CloseDB;
begin
  FList.Clean;
  if FDBEngine = nil then
      exit;
  DisposeObjectAndNil(FDBEngine);
end;

procedure TVideoZDBStoreFormula.Compression;
begin
  FDBEngine.Compress;
  Refresh;
end;

procedure TVideoZDBStoreFormula.Refresh;
begin
  FList.Clean;
  FDBEngine.WaitQueryM(False, {$IFDEF FPC}@{$ENDIF FPC}QueryCall_);
end;

procedure TVideoZDBStoreFormula.SaveVideoStream(Classifier, token: SystemString; VideoType: TVideoType; stream: TCoreClassStream);
var
  VT: THashStringList;
  Format_: TFormatSettings;
  Now_: TDateTime;
begin
  if FDBEngine = nil then
    begin
      DoStatus('Database Engine error.');
      exit;
    end;
  FDBEngine.AddData(stream, C_Video_ID);
  VT := THashStringList.Create;
  VT.SetDefaultValue('classifier', Classifier);
  VT.SetDefaultValue('token', token);
  VT.SetDefaultValue('videoType', GetEnumName(TypeInfo(TVideoType), Ord(VideoType)));
  Format_ := {$IFDEF FPC} DefaultFormatSettings {$ELSE FPC} TFormatSettings.Create {$ENDIF FPC};
  Format_.ShortDateFormat := 'ee/mm/dd';
  Format_.DateSeparator := '/';
  Format_.ShortTimeFormat := 'hh-nn-ss';
  Format_.TimeSeparator := '-';
  Now_ := Now();
  VT.SetDefaultValue('Date', DateToStr(Now_, Format_));
  VT.SetDefaultValue('Time', TimeToStr(Now_, Format_));
  FDBEngine.AddData(VT);
  DisposeObject(VT);
end;

procedure TVideoZDBStoreFormula.SaveVideoFile(Classifier, token: SystemString; VideoType: TVideoType; fileName: U_String);
var
  fs: TCoreClassFileStream;
begin
  fs := TCoreClassFileStream.Create(fileName, fmOpenRead);
  SaveVideoStream(Classifier, token, VideoType, fs);
  DisposeObject(fs);
end;

function TVideoZDBStoreFormula.Count: Integer;
begin
  Result := FList.Count;
end;

function TVideoZDBStoreFormula.GetItems(index: Integer): PVideoZDBStoreData;
begin
  Result := FList[index];
end;

function TVideoZDBStoreFormula.Delete(index: Integer; removeDB: Boolean): Boolean;
begin
  Result := False;
  if (index >= 0) and (index < FList.Count) then
    begin
      FDBEngine.DeleteData(FList[index]^.VT.StorePos);
      FDBEngine.DeleteData(FList[index]^.VideoStreamPos);
      dispose(FList[index]);
      FList.Delete(index);
      Result := True;
    end;
end;

initialization

finalization

end.
