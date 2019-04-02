{ ****************************************************************************** }
{ * ObjectData Manager, base on ObjectData, write by qq600585                  * }
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
(*
  update history
*)

unit ObjectDataManager;

{$INCLUDE zDefine.inc}

interface

uses CoreClasses, ObjectData, UnicodeMixedLib, PascalStrings, ListEngine;

type
  TItemHandle = ObjectData.TTMDBItemHandle;
  PItemHandle = ^TItemHandle;
  TFieldHandle = ObjectData.TField;
  PFieldHandle = ^TFieldHandle;
  TItemSearch = ObjectData.TTMDBSearchItem;
  PItemSearch = ^TItemSearch;
  TFieldSearch = ObjectData.TTMDBSearchField;
  PFieldSearch = ^TFieldSearch;
  TItemRecursionSearch = ObjectData.TTMDBRecursionSearch;
  PItemRecursionSearch = ^TItemRecursionSearch;

  TObjectDataManager = class(TCoreClassObject)
  protected
    FStreamEngine: TCoreClassStream;
    FDBHandle: TTMDB;
    FNeedCreateNew, FOnlyRead: Boolean;
    FObjectName: SystemString;
    FDefaultItemID: Byte;
    FIsOpened: Boolean;
    FData: Pointer;

    function GetAutoFreeHandle: Boolean;
    procedure SetAutoFreeHandle(const Value: Boolean);
  protected
    procedure DoCreateFinish; virtual;

    function GetOverWriteItem: Boolean;
    function GetAllowSameHeaderName: Boolean;
    function GetDBTime: TDateTime;
    procedure SetOverWriteItem(Value: Boolean);
    procedure SetAllowSameHeaderName(Value: Boolean);

    procedure DBErrorProc(error: U_String);
  public
    constructor Create(const dbName: SystemString; const dbItemID: Byte; dbOnlyRead: Boolean);
    constructor CreateNew(const dbName: SystemString; const dbItemID: Byte);
    constructor CreateAsStream(AStream: TCoreClassStream; const dbName: SystemString; const dbItemID: Byte; dbOnlyRead, isNewDB, DestroyTimeFreeStream: Boolean);
    destructor Destroy; override;
    function Open(): Boolean;
    function NewHandle(AStream: TCoreClassStream; const dbName: SystemString; const dbItemID: Byte; dbOnlyRead, aIsNew: Boolean): Boolean;
    function CopyTo(DestDB: TObjectDataManager): Boolean;
    function CopyToPath(DestDB: TObjectDataManager; destPath: SystemString): Boolean;
    function CopyFieldToPath(FieldPos: Int64; DestDB: TObjectDataManager; destPath: SystemString): Boolean;
    procedure SaveToStream(stream: TCoreClassStream);
    procedure ImpFromPath(ImpPath, DBPath: SystemString; IncludeSub: Boolean);
    procedure ImpFromFiles(ImpFiles: TCoreClassStrings; DBPath: SystemString);
    function isAbort: Boolean;
    function Close: Boolean;
    function ErrorNo: Int64;
    function Modification: Boolean;
    function Size: Int64;
    function IOReadSize: Int64;
    function IOWriteSize: Int64;

    // defaultItem ID
    procedure SetID(const ID: Byte);

    // realtime IO update
    procedure UpdateIO; virtual;

    // field api
    function CreateField(const DirName, DirDescription: SystemString): Boolean;
    function CreateRootField(const RootName: SystemString): Boolean;
    function DirectoryExists(const DirName: SystemString): Boolean;
    function FastDelete(const FieldPos: Int64; const fPos: Int64): Boolean;
    function FastFieldExists(const FieldPos: Int64; const FieldName: SystemString): Boolean;
    function FastFieldCreate(const FieldPos: Int64; const FieldName, FieldDescription: SystemString; var NewFieldPos: Int64): Boolean;
    function RootField: Int64;
    function SetRootField(const RootName: SystemString): Boolean;
    function FieldRename(const FieldPos: Int64; const NewFieldName, NewFieldDescription: SystemString): Boolean;
    function FieldDelete(const DBPath: SystemString; const FieldName: SystemString): Boolean;
    function FieldExists(const DBPath: SystemString; const FieldName: SystemString): Boolean;
    function FieldFastFindFirst(const FieldPos: Int64; const Filter: SystemString; var FieldSearchHandle: TFieldSearch): Boolean;
    function FieldFastFindLast(const FieldPos: Int64; const Filter: SystemString; var FieldSearchHandle: TFieldSearch): Boolean;
    function FieldFastFindNext(var FieldSearchHandle: TFieldSearch): Boolean;
    function FieldFastFindPrev(var FieldSearchHandle: TFieldSearch): Boolean;
    function FieldFindFirst(const DBPath, Filter: SystemString; var FieldSearchHandle: TFieldSearch): Boolean;
    function FieldFindLast(const DBPath, Filter: SystemString; var FieldSearchHandle: TFieldSearch): Boolean;
    function FieldFindNext(var FieldSearchHandle: TFieldSearch): Boolean;
    function FieldFindPrev(var FieldSearchHandle: TFieldSearch): Boolean;
    function FieldMove(const DBPath, FieldName, destPath: SystemString): Boolean;
    function GetFieldData(const FieldPos: Int64; var dest: TFieldHandle): Boolean;
    function GetFieldPath(const FieldPos: Int64): SystemString;
    function GetPathField(const DBPath: SystemString; var dest: Int64): Boolean;
    function GetPathFieldPos(const DBPath: SystemString): Int64;
    function GetPathFieldHeaderCount(const DBPath: SystemString): Int64;
    function GetPathFieldHeaderNames(const DBPath: SystemString; var output: U_StringArray): Boolean;

    // header api
    function GetHeaderModificationTime(const hPos: Int64): TDateTime;
    function GetFirstHeaderFromField(FieldPos: Int64; var h: THeader): Boolean;
    function GetLastHeaderFromField(FieldPos: Int64; var h: THeader): Boolean;
    function GetHeader(hPos: Int64; var h: THeader): Boolean;

    // item api
    function GetItemSize(const DBPath, DBItem: SystemString): Int64;
    function ItemCreate(const DBPath, DBItem, DBItemDescription: SystemString; var ItemHnd: TItemHandle): Boolean;
    function ItemDelete(const DBPath, DBItem: SystemString): Boolean;
    function ItemExists(const DBPath, DBItem: SystemString): Boolean;
    function ItemOpen(const DBPath, DBItem: SystemString; var ItemHnd: TItemHandle): Boolean;
    function ItemAutoOpenOrCreate(const DBPath, DBItem, DBItemDescription: SystemString; var ItemHnd: TItemHandle): Boolean;
    function ItemUpdate(var ItemHnd: TItemHandle): Boolean;
    function ItemClose(var ItemHnd: TItemHandle): Boolean;
    function ItemCopyTo(var ItemHnd: TItemHandle; DestDB: TObjectDataManager; var DestItemHandle: TItemHandle; const CopySize: Int64): Boolean;
    function ItemMove(const DBPath, ItemName, destPath: SystemString): Boolean;
    function ItemRename(const FieldPos: Int64; var ItemHnd: TItemHandle; const NewName, NewDescription: SystemString): Boolean;
    function ItemFastInsertNew(const FieldPos, InsertHeaderPos: Int64; const DBItem, DBItemDescription: SystemString; var ItemHnd: TItemHandle): Boolean;
    function ItemFastCreate(const fPos: Int64; const DBItem, DBItemDescription: SystemString; var ItemHnd: TItemHandle): Boolean;
    function ItemFastOpen(const fPos: Int64; var ItemHnd: TItemHandle): Boolean;
    function ItemFastResetBody(const fPos: Int64): Boolean;
    function ItemFastExists(const FieldPos: Int64; const DBItem: SystemString): Boolean;
    function ItemFastFindFirst(const FieldPos: Int64; const DBItem: SystemString; var ItemSearchHandle: TItemSearch): Boolean;
    function ItemFastFindLast(const FieldPos: Int64; const DBItem: SystemString; var ItemSearchHandle: TItemSearch): Boolean;
    function ItemFastFindNext(var ItemSearchHandle: TItemSearch): Boolean;
    function ItemFastFindPrev(var ItemSearchHandle: TItemSearch): Boolean;
    function ItemFindFirst(const DBPath, DBItem: SystemString; var ItemSearchHandle: TItemSearch): Boolean;
    function ItemFindLast(const DBPath, DBItem: SystemString; var ItemSearchHandle: TItemSearch): Boolean;
    function ItemFindNext(var ItemSearchHandle: TItemSearch): Boolean;
    function ItemFindPrev(var ItemSearchHandle: TItemSearch): Boolean;

    // item block operation
    function ItemRead(var ItemHnd: TItemHandle; const siz: Int64; var Buffers): Boolean; overload;
    function ItemSeekStart(var ItemHnd: TItemHandle): Boolean;
    function ItemSeekLast(var ItemHnd: TItemHandle): Boolean;
    function ItemSeek(var ItemHnd: TItemHandle; const ItemOffset: Int64): Boolean;
    function ItemGetPos(var ItemHnd: TItemHandle): Int64;
    function ItemGetSize(var ItemHnd: TItemHandle): Int64;
    function ItemWrite(var ItemHnd: TItemHandle; const siz: Int64; var Buffers): Boolean;

    // item stream
    function ItemReadToStream(var ItemHnd: TItemHandle; stream: TCoreClassStream): Boolean; overload;
    function ItemWriteFromStream(var ItemHnd: TItemHandle; stream: TCoreClassStream): Boolean; overload;
    function ItemReadToStream(const DBPath, DBItem: SystemString; stream: TCoreClassStream): Boolean; overload;
    function ItemWriteFromStream(const DBPath, DBItem: SystemString; stream: TCoreClassStream): Boolean; overload;

    // recursion support
    function RecursionSearchFirst(const InitPath, Filter: SystemString; var RecursionSearchHnd: TItemRecursionSearch): Boolean;
    function RecursionSearchNext(var RecursionSearchHnd: TItemRecursionSearch): Boolean;

    // options
    function DBHandlePtr: PTMDB;
    property AutoFreeHandle: Boolean read GetAutoFreeHandle write SetAutoFreeHandle;
    property IsOnlyRead: Boolean read FOnlyRead;
    property NeedCreateNew: Boolean read FNeedCreateNew;
    property ObjectName: SystemString read FObjectName write FObjectName;
    property DefaultItemID: Byte read FDefaultItemID;
    property StreamEngine: TCoreClassStream read FStreamEngine;
    property DBTime: TDateTime read GetDBTime;
    property OverWriteItem: Boolean read GetOverWriteItem write SetOverWriteItem;
    property SameHeaderName: Boolean read GetAllowSameHeaderName write SetAllowSameHeaderName;

    // user custom data
    property Data: Pointer read FData write FData;
  end;

  TObjectDataManagerClass = class of TObjectDataManager;

  TObjectDataManagerOfCache = class(TObjectDataManager)
  protected type
    PObjectDataCacheHeader = PHeader;
    PObjectDataCacheItemBlock = PItemBlock;

    TObjectDataCacheItem = record
      Description: U_String;
      ExtID: Byte;
      FirstBlockPOS: Int64;
      LastBlockPOS: Int64;
      Size: Int64;
      BlockCount: Int64;
      CurrentBlockSeekPOS: Int64;
      CurrentFileSeekPOS: Int64;
      Return: Integer;
      MemorySiz: nativeUInt;
      procedure write(var wVal: TItem);
      procedure read(var rVal: TItem);
    end;

    PObjectDataCacheItem = ^TObjectDataCacheItem;

    TObjectDataCacheField = record
      UpLevelFieldPOS: Int64;
      Description: U_String;
      HeaderCount: Int64;
      FirstHeaderPOS: Int64;
      LastHeaderPOS: Int64;
      Return: Integer;
      MemorySiz: nativeUInt;
      procedure write(var wVal: TField);
      procedure read(var rVal: TField);
    end;

    PObjectDataCacheField = ^TObjectDataCacheField;

    TSwapHead = packed record
      Size: Integer;
      MD5: TMD5;
      Position: Int64;
    end;

  protected
    FHeaderCache, FItemBlockCache, FItemCache, FFieldCache: TInt64HashPointerList;
    FPrepareWritePool: TInt64HashObjectList;

    procedure HeaderCache_DataFreeProc(p: Pointer);
    procedure ItemBlockCache_DataFreeProc(p: Pointer);
    procedure ItemCache_DataFreeProc(p: Pointer);
    procedure FieldCache_DataFreeProc(p: Pointer);
    procedure PrepareWritePool_DataFreeProc(obj: TCoreClassObject);

    function CheckPreapreWrite(fPos: Int64): Boolean;

    procedure DeleteHeaderProc(fPos: Int64);

    procedure PrepareHeaderWriteProc(fPos: Int64; var wVal: THeader; var Done: Boolean);
    procedure HeaderWriteProc(fPos: Int64; var wVal: THeader);
    procedure HeaderReadProc(fPos: Int64; var rVal: THeader; var Done: Boolean);

    procedure PrepareItemBlockWriteProc(fPos: Int64; var wVal: TItemBlock; var Done: Boolean);
    procedure ItemBlockWriteProc(fPos: Int64; var wVal: TItemBlock);
    procedure ItemBlockReadProc(fPos: Int64; var rVal: TItemBlock; var Done: Boolean);

    procedure PrepareItemWriteProc(fPos: Int64; var wVal: TItem; var Done: Boolean);
    procedure ItemWriteProc(fPos: Int64; var wVal: TItem);
    procedure ItemReadProc(fPos: Int64; var rVal: TItem; var Done: Boolean);

    procedure PrepareOnlyItemRecWriteProc(fPos: Int64; var wVal: TItem; var Done: Boolean);
    procedure OnlyItemRecWriteProc(fPos: Int64; var wVal: TItem);
    procedure OnlyItemRecReadProc(fPos: Int64; var rVal: TItem; var Done: Boolean);

    procedure PrepareFieldWriteProc(fPos: Int64; var wVal: TField; var Done: Boolean);
    procedure FieldWriteProc(fPos: Int64; var wVal: TField);
    procedure FieldReadProc(fPos: Int64; var rVal: TField; var Done: Boolean);

    procedure PrepareOnlyFieldRecWriteProc(fPos: Int64; var wVal: TField; var Done: Boolean);
    procedure OnlyFieldRecWriteProc(fPos: Int64; var wVal: TField);
    procedure OnlyFieldRecReadProc(fPos: Int64; var rVal: TField; var Done: Boolean);

    procedure PrepareTMDBWriteProc(fPos: Int64; const wVal: PTMDB; var Done: Boolean);
    procedure TMDBWriteProc(fPos: Int64; const wVal: PTMDB);
    procedure TMDBReadProc(fPos: Int64; const rVal: PTMDB; var Done: Boolean);

    procedure CheckAndRestoreFlush;
    procedure DoCreateFinish; override;
  public
    destructor Destroy; override;
    procedure BuildDBCacheIntf;
    procedure FreeDBCacheIntf;
    procedure CleaupCache;
    procedure SetPoolCache(const Value: Integer);
    procedure UpdateIO; override;
    procedure Flush();
    function CacheStatus: SystemString;
  end;

  TObjectDataMarshal = class(TCoreClassObject)
  protected
    FID: Byte;
    FLibList: TCoreClassStrings;
    FUseWildcard: Boolean;
    function GetItems(aIndex: Integer): TObjectDataManager;
    function GetNames(AName: SystemString): TObjectDataManager;
    procedure SetItems(aIndex: Integer; const Value: TObjectDataManager);
  public
    constructor Create(dbItemID: Byte);
    destructor Destroy; override;
    function GetAbsoluteFileName(fileName: SystemString): SystemString;
    function NewDB(dbFile: SystemString; dbOnlyRead: Boolean): TObjectDataManager;
    function Open(dbFile: SystemString; dbOnlyRead: Boolean): TObjectDataManager;
    procedure CloseDB(db: TObjectDataManager);
    procedure Clear;
    function Count: Integer;
    procedure Delete(aIndex: Integer);
    procedure DeleteFromName(AName: SystemString);
    procedure UpdateAll;
    procedure Disable;
    procedure Enabled;

    property LibList: TCoreClassStrings read FLibList;
    property Items[aIndex: Integer]: TObjectDataManager read GetItems write SetItems;
    property Names[AName: SystemString]: TObjectDataManager read GetNames; default;
    property UseWildcard: Boolean read FUseWildcard write FUseWildcard;
    property ID: Byte read FID write FID;
  end;

function ObjectDataMarshal: TObjectDataMarshal;
function DBMarshal: TObjectDataMarshal;

procedure TestObjectData();

implementation

uses ItemStream, Types, MemoryStream64, DoStatusIO;

const
  MaxBuffSize = 65535;
  UserRootName = 'User';

var
  Internal_ObjectDataMarshal: TObjectDataMarshal = nil;

function ObjectDataMarshal: TObjectDataMarshal;
begin
  if Internal_ObjectDataMarshal = nil then
      Internal_ObjectDataMarshal := TObjectDataMarshal.Create(0);
  Result := Internal_ObjectDataMarshal;
end;

function DBMarshal: TObjectDataMarshal;
begin
  Result := ObjectDataMarshal();
end;

function TObjectDataManager.GetAutoFreeHandle: Boolean;
begin
  if not isAbort then
      Result := FDBHandle.IOHnd.AutoFree
  else
      Result := False;
end;

procedure TObjectDataManager.SetAutoFreeHandle(const Value: Boolean);
begin
  if not isAbort then
      FDBHandle.IOHnd.AutoFree := Value;
end;

procedure TObjectDataManager.DoCreateFinish;
begin
end;

function TObjectDataManager.GetOverWriteItem: Boolean;
begin
  Result := FDBHandle.OverWriteItem;
end;

function TObjectDataManager.GetAllowSameHeaderName: Boolean;
begin
  Result := FDBHandle.AllowSameHeaderName;
end;

function TObjectDataManager.GetDBTime: TDateTime;
begin
  Result := FDBHandle.CreateTime;
end;

procedure TObjectDataManager.SetOverWriteItem(Value: Boolean);
begin
  FDBHandle.OverWriteItem := Value;
end;

procedure TObjectDataManager.SetAllowSameHeaderName(Value: Boolean);
begin
  FDBHandle.AllowSameHeaderName := Value;
end;

procedure TObjectDataManager.DBErrorProc(error: U_String);
begin
  DoStatus('error: %s - %s!', [ObjectName, error.Text]);
end;

constructor TObjectDataManager.Create(const dbName: SystemString; const dbItemID: Byte; dbOnlyRead: Boolean);
begin
  inherited Create;
  NewHandle(nil, dbName, dbItemID, dbOnlyRead, False);
  DoCreateFinish;
end;

constructor TObjectDataManager.CreateNew(const dbName: SystemString; const dbItemID: Byte);
begin
  inherited Create;
  NewHandle(nil, dbName, dbItemID, False, True);
  DoCreateFinish;
end;

constructor TObjectDataManager.CreateAsStream(AStream: TCoreClassStream; const dbName: SystemString; const dbItemID: Byte; dbOnlyRead, isNewDB, DestroyTimeFreeStream: Boolean);
begin
  inherited Create;
  NewHandle(AStream, dbName, dbItemID, dbOnlyRead, isNewDB);
  AutoFreeHandle := DestroyTimeFreeStream;
  DoCreateFinish;
end;

destructor TObjectDataManager.Destroy;
begin
  Close;
  inherited Destroy;
end;

function TObjectDataManager.Open(): Boolean;
begin
  Result := False;
  try
    if StreamEngine <> nil then
      begin
        if FNeedCreateNew then
          begin
            if not db_CreateAsStream(StreamEngine, ObjectName, '', FDBHandle) then
              begin
                Exit;
              end;
            if not(db_CreateRootField(UserRootName, '', FDBHandle)) then
                Exit;
            if not(db_SetCurrentRootField(UserRootName, FDBHandle)) then
                Exit;
          end
        else
          begin
            if not db_OpenAsStream(StreamEngine, ObjectName, FDBHandle, IsOnlyRead) then
              begin
                Exit;
              end;
          end;
      end
    else if (FNeedCreateNew) or (not umlFileExists(ObjectName)) then
      begin
        if not db_CreateNew(ObjectName, '', FDBHandle) then
          begin
            db_ClosePack(FDBHandle);
            Init_TTMDB(FDBHandle);
            if not db_CreateNew(ObjectName, '', FDBHandle) then
              begin
                Exit;
              end;
          end;
        if not(db_CreateRootField(UserRootName, '', FDBHandle)) then
            Exit;
        if not(db_SetCurrentRootField(UserRootName, FDBHandle)) then
            Exit;
      end
    else if not db_Open(ObjectName, FDBHandle, IsOnlyRead) then
      begin
        db_ClosePack(FDBHandle);
        Init_TTMDB(FDBHandle);
        if not db_Open(ObjectName, FDBHandle, IsOnlyRead) then
          begin
            Exit;
          end;
      end;
    Result := True;
  except
  end;
end;

function TObjectDataManager.NewHandle(AStream: TCoreClassStream; const dbName: SystemString; const dbItemID: Byte; dbOnlyRead, aIsNew: Boolean): Boolean;
begin
  Close;
  Init_TTMDB(FDBHandle);
  FDBHandle.OnError := {$IFDEF FPC}@{$ENDIF FPC}DBErrorProc;

  FStreamEngine := AStream;
  FObjectName := dbName;
  FNeedCreateNew := aIsNew;
  FOnlyRead := dbOnlyRead;
  FDefaultItemID := dbItemID;
  FIsOpened := Open();
  Result := FIsOpened;

  OverWriteItem := True;
  SameHeaderName := False;
  AutoFreeHandle := True;
  FData := nil;
end;

function TObjectDataManager.CopyTo(DestDB: TObjectDataManager): Boolean;
begin
  Result := db_CopyAllTo(FDBHandle, DestDB.FDBHandle);
end;

function TObjectDataManager.CopyToPath(DestDB: TObjectDataManager; destPath: SystemString): Boolean;
begin
  Result := db_CopyAllToDestPath(FDBHandle, DestDB.FDBHandle, destPath);
end;

function TObjectDataManager.CopyFieldToPath(FieldPos: Int64; DestDB: TObjectDataManager; destPath: SystemString): Boolean;
var
  DestFieldPos: Int64;
begin
  Result := False;
  CreateField(destPath, '');
  if GetPathField(destPath, DestFieldPos) then
      Result := db_CopyFieldTo('*', FDBHandle, FieldPos, DestDB.FDBHandle, DestFieldPos);
end;

procedure TObjectDataManager.SaveToStream(stream: TCoreClassStream);
var
  E: TObjectDataManager;
begin
  E := TObjectDataManager.CreateAsStream(stream, ObjectName, DefaultItemID, False, True, False);
  CopyTo(E);
  DisposeObject(E);
end;

procedure TObjectDataManager.ImpFromPath(ImpPath, DBPath: SystemString; IncludeSub: Boolean);
var
  fAry: U_StringArray;
  n: SystemString;
  fPos: Int64;
  fs: TCoreClassFileStream;
  itmHnd: TItemHandle;
  itmStream: TItemStream;
begin
  DBPath := umlCharReplace(DBPath, '\', '/').Text;
  if not DirectoryExists(DBPath) then
      CreateField(DBPath, '');
  fPos := GetPathFieldPos(DBPath);

  fAry := umlGetFileListWithFullPath(ImpPath);
  for n in fAry do
    begin
      fs := TCoreClassFileStream.Create(n, fmOpenRead or fmShareDenyWrite);
      ItemFastCreate(fPos, umlGetFileName(n).Text, '', itmHnd);
      itmStream := TItemStream.Create(Self, itmHnd);
      try
          itmStream.CopyFrom(fs, fs.Size)
      except
      end;
      itmStream.CloseHandle;
      DisposeObject(fs);
      DisposeObject(itmStream);
    end;

  if IncludeSub then
    begin
      fAry := umlGetDirListWithFullPath(ImpPath);
      for n in fAry do
          ImpFromPath(n, umlCombineFileName(DBPath, umlGetFileName(n)).Text, IncludeSub);
    end;
end;

procedure TObjectDataManager.ImpFromFiles(ImpFiles: TCoreClassStrings; DBPath: SystemString);
var
  i: Integer;
  n: SystemString;
  fPos: Int64;
  fs: TCoreClassFileStream;
  itmHnd: TItemHandle;
  itmStream: TItemStream;
begin
  DBPath := umlCharReplace(DBPath, '\', '/').Text;
  if not DirectoryExists(DBPath) then
      CreateField(DBPath, '');
  fPos := GetPathFieldPos(DBPath);

  for i := 0 to ImpFiles.Count - 1 do
    begin
      n := ImpFiles[i];
      fs := TCoreClassFileStream.Create(n, fmOpenRead or fmShareDenyWrite);
      ItemFastCreate(fPos, umlGetFileName(n).Text, '', itmHnd);
      itmStream := TItemStream.Create(Self, itmHnd);
      try
          itmStream.CopyFrom(fs, fs.Size)
      except
      end;
      itmStream.CloseHandle;
      DisposeObject(fs);
      DisposeObject(itmStream);
    end;
end;

function TObjectDataManager.isAbort: Boolean;
begin
  Result := not FIsOpened;
end;

function TObjectDataManager.Close: Boolean;
begin
  Result := db_ClosePack(FDBHandle);
end;

function TObjectDataManager.ErrorNo: Int64;
begin
  Result := FDBHandle.Return;
end;

function TObjectDataManager.Modification: Boolean;
begin
  Result := FDBHandle.IOHnd.WriteStated;
end;

function TObjectDataManager.Size: Int64;
begin
  Result := FDBHandle.IOHnd.Size;
end;

function TObjectDataManager.IOReadSize: Int64;
begin
  Result := FDBHandle.IOHnd.IORead;
end;

function TObjectDataManager.IOWriteSize: Int64;
begin
  Result := FDBHandle.IOHnd.IOWrite;
end;

procedure TObjectDataManager.SetID(const ID: Byte);
begin
  FDefaultItemID := ID;
end;

procedure TObjectDataManager.UpdateIO;
begin
  db_Update(FDBHandle);
end;

function TObjectDataManager.CreateField(const DirName, DirDescription: SystemString): Boolean;
begin
  Result := db_CreateField(DirName, DirDescription, FDBHandle);
end;

function TObjectDataManager.CreateRootField(const RootName: SystemString): Boolean;
begin
  Result := db_CreateRootField(RootName, RootName, FDBHandle);
end;

function TObjectDataManager.DirectoryExists(const DirName: SystemString): Boolean;
var
  Field: TFieldHandle;
begin
  Result := db_GetField(DirName, Field, FDBHandle);
end;

function TObjectDataManager.FastDelete(const FieldPos: Int64; const fPos: Int64): Boolean;
var
  FieldHnd: TFieldHandle;
begin
  Init_TField(FieldHnd);
  Result := False;
  if dbField_ReadRec(FieldPos, FDBHandle.IOHnd, FieldHnd) then
      Result := dbField_DeleteHeader(fPos, FieldPos, FDBHandle.IOHnd, FieldHnd);
end;

function TObjectDataManager.FastFieldExists(const FieldPos: Int64; const FieldName: SystemString): Boolean;
var
  FieldSearch: TFieldSearch;
begin
  Result := FieldFastFindFirst(FieldPos, FieldName, FieldSearch);
end;

function TObjectDataManager.FastFieldCreate(const FieldPos: Int64; const FieldName, FieldDescription: SystemString; var NewFieldPos: Int64): Boolean;
var
  NewField: TField;
begin
  Init_TField(NewField);
  NewField.Description := FieldDescription;
  Result := dbField_CreateField(FieldName, FieldPos, FDBHandle.IOHnd, NewField);
  NewFieldPos := NewField.RHeader.CurrentHeader;
end;

function TObjectDataManager.RootField: Int64;
begin
  Result := FDBHandle.DefaultFieldPOS;
end;

function TObjectDataManager.SetRootField(const RootName: SystemString): Boolean;
begin
  Result := db_SetCurrentRootField(RootName, FDBHandle);
end;

function TObjectDataManager.FieldRename(const FieldPos: Int64; const NewFieldName, NewFieldDescription: SystemString): Boolean;
var
  FieldHnd: TFieldHandle;
begin
  Result := False;
  if not umlExistsChar(NewFieldName, '\/') then
    begin
      Init_TField(FieldHnd);
      if dbField_ReadRec(FieldPos, FDBHandle.IOHnd, FieldHnd) then
        begin
          if (not FastFieldExists(FieldHnd.UpLevelFieldPOS, NewFieldName)) and (FieldHnd.RHeader.CurrentHeader <> FDBHandle.DefaultFieldPOS) then
            begin
              FieldHnd.RHeader.Name := NewFieldName;
              FieldHnd.Description := NewFieldDescription;
              Result := dbField_WriteRec(FieldPos, FDBHandle.IOHnd, FieldHnd);
            end;
        end;
    end;
end;

function TObjectDataManager.FieldDelete(const DBPath: SystemString; const FieldName: SystemString): Boolean;
begin
  Result := db_DeleteField(DBPath, FieldName, FDBHandle);
end;

function TObjectDataManager.FieldExists(const DBPath: SystemString; const FieldName: SystemString): Boolean;
var
  FieldSearch: TFieldSearch;
begin
  Result := FieldFindFirst(DBPath, FieldName, FieldSearch);
end;

function TObjectDataManager.FieldFastFindFirst(const FieldPos: Int64; const Filter: SystemString; var FieldSearchHandle: TFieldSearch): Boolean;
begin
  Init_TTMDBSearchField(FieldSearchHandle);
  Result := db_FastFindFirstField(FieldPos, Filter, FieldSearchHandle, FDBHandle);
end;

function TObjectDataManager.FieldFastFindLast(const FieldPos: Int64; const Filter: SystemString; var FieldSearchHandle: TFieldSearch): Boolean;
begin
  Init_TTMDBSearchField(FieldSearchHandle);
  Result := db_FastFindLastField(FieldPos, Filter, FieldSearchHandle, FDBHandle);
end;

function TObjectDataManager.FieldFastFindNext(var FieldSearchHandle: TFieldSearch): Boolean;
begin
  Result := db_FastFindNextField(FieldSearchHandle, FDBHandle);
end;

function TObjectDataManager.FieldFastFindPrev(var FieldSearchHandle: TFieldSearch): Boolean;
begin
  Result := db_FastFindPrevField(FieldSearchHandle, FDBHandle);
end;

function TObjectDataManager.FieldFindFirst(const DBPath, Filter: SystemString; var FieldSearchHandle: TFieldSearch): Boolean;
begin
  Init_TTMDBSearchField(FieldSearchHandle);
  Result := db_FindFirstField(DBPath, Filter, FieldSearchHandle, FDBHandle);
end;

function TObjectDataManager.FieldFindLast(const DBPath, Filter: SystemString; var FieldSearchHandle: TFieldSearch): Boolean;
begin
  Init_TTMDBSearchField(FieldSearchHandle);
  Result := db_FindLastField(DBPath, Filter, FieldSearchHandle, FDBHandle);
end;

function TObjectDataManager.FieldFindNext(var FieldSearchHandle: TFieldSearch): Boolean;
begin
  Result := db_FindNextField(FieldSearchHandle, FDBHandle);
end;

function TObjectDataManager.FieldFindPrev(var FieldSearchHandle: TFieldSearch): Boolean;
begin
  Result := db_FindPrevField(FieldSearchHandle, FDBHandle);
end;

function TObjectDataManager.FieldMove(const DBPath, FieldName, destPath: SystemString): Boolean;
begin
  Result := db_MoveField(DBPath, FieldName, destPath, FDBHandle);
end;

function TObjectDataManager.GetFieldData(const FieldPos: Int64; var dest: TFieldHandle): Boolean;
begin
  Init_TField(dest);
  Result := dbField_ReadRec(FieldPos, FDBHandle.IOHnd, dest);
end;

function TObjectDataManager.GetFieldPath(const FieldPos: Int64): SystemString;
var
  ReturnPath: U_String;
begin
  if db_GetPath(FieldPos, FDBHandle.DefaultFieldPOS, FDBHandle, ReturnPath) then
      Result := ReturnPath
  else
      Result := '';
end;

function TObjectDataManager.GetPathField(const DBPath: SystemString; var dest: Int64): Boolean;
var
  FieldHnd: TFieldHandle;
begin
  Result := db_GetField(DBPath, FieldHnd, FDBHandle);
  if Result then
      dest := FieldHnd.RHeader.CurrentHeader;
end;

function TObjectDataManager.GetPathFieldPos(const DBPath: SystemString): Int64;
begin
  if not GetPathField(DBPath, Result) then
      Result := 0;
end;

function TObjectDataManager.GetPathFieldHeaderCount(const DBPath: SystemString): Int64;
var
  FieldHnd: TFieldHandle;
begin
  Result := 0;
  if db_GetField(DBPath, FieldHnd, FDBHandle) then
      Result := FieldHnd.HeaderCount;
end;

function TObjectDataManager.GetPathFieldHeaderNames(const DBPath: SystemString; var output: U_StringArray): Boolean;
var
  FieldHnd: TFieldHandle;
  i: Integer;
  h: ObjectData.THeader;
begin
  Result := False;
  SetLength(output, 0);
  if not db_GetField(DBPath, FieldHnd, FDBHandle) then
      Exit;
  SetLength(output, FieldHnd.HeaderCount);
  i := 0;

  if FieldHnd.HeaderCount > 0 then
    if dbHeader_ReadRec(FieldHnd.FirstHeaderPOS, FDBHandle.IOHnd, h) then
      begin
        repeat
          output[i] := h.Name;
          inc(i);
          if h.PositionID in [DB_Header_OnlyPositionFlags, DB_Header_LastPositionFlags] then
              break;
          dbHeader_ReadRec(h.NextHeader, FDBHandle.IOHnd, h);
        until False;
      end;
  Result := True;
end;

function TObjectDataManager.GetHeaderModificationTime(const hPos: Int64): TDateTime;
var
  h: THeader;
begin
  if dbHeader_ReadRec(hPos, FDBHandle.IOHnd, h) then
      Result := h.ModificationTime
  else
      Result := umlDefaultTime;
end;

function TObjectDataManager.GetFirstHeaderFromField(FieldPos: Int64; var h: THeader): Boolean;
var
  f: TField;
begin
  Result := (dbField_ReadRec(FieldPos, FDBHandle.IOHnd, f)) and (f.HeaderCount > 0);
  if Result then
    begin
      Result := GetHeader(f.FirstHeaderPOS, h);
    end;
end;

function TObjectDataManager.GetLastHeaderFromField(FieldPos: Int64; var h: THeader): Boolean;
var
  f: TField;
begin
  Result := (dbField_ReadRec(FieldPos, FDBHandle.IOHnd, f)) and (f.HeaderCount > 0);
  if Result then
      Result := GetHeader(f.LastHeaderPOS, h);
end;

function TObjectDataManager.GetHeader(hPos: Int64; var h: THeader): Boolean;
begin
  Result := dbHeader_ReadRec(hPos, FDBHandle.IOHnd, h);
end;

function TObjectDataManager.GetItemSize(const DBPath, DBItem: SystemString): Int64;
var
  DBItemHandle: TItemHandle;
begin
  Init_TTMDBItemHandle(DBItemHandle);
  if db_GetItem(DBPath, DBItem, FDefaultItemID, DBItemHandle.Item, FDBHandle) then
      Result := DBItemHandle.Item.Size
  else
      Result := 0;
end;

function TObjectDataManager.ItemCreate(const DBPath, DBItem, DBItemDescription: SystemString; var ItemHnd: TItemHandle): Boolean;
{
  It can automatically create a path
}
begin
  Init_TTMDBItemHandle(ItemHnd);
  Result := db_ItemCreate(DBPath, DBItem, DBItemDescription, FDefaultItemID, ItemHnd, FDBHandle);
end;

function TObjectDataManager.ItemDelete(const DBPath, DBItem: SystemString): Boolean;
begin
  Result := db_DeleteItem(DBPath, DBItem, FDefaultItemID, FDBHandle);
end;

function TObjectDataManager.ItemExists(const DBPath, DBItem: SystemString): Boolean;
var
  ItemSearchHnd: TItemSearch;
begin
  Init_TTMDBSearchItem(ItemSearchHnd);
  Result := db_FindFirstItem(DBPath, DBItem, FDefaultItemID, ItemSearchHnd, FDBHandle);
end;

function TObjectDataManager.ItemOpen(const DBPath, DBItem: SystemString; var ItemHnd: TItemHandle): Boolean;
begin
  Init_TTMDBItemHandle(ItemHnd);
  Result := db_ItemOpen(DBPath, DBItem, FDefaultItemID, ItemHnd, FDBHandle);
end;

function TObjectDataManager.ItemAutoOpenOrCreate(const DBPath, DBItem, DBItemDescription: SystemString; var ItemHnd: TItemHandle): Boolean;
begin
  if ItemExists(DBPath, DBItem) then
      Result := ItemOpen(DBPath, DBItem, ItemHnd)
  else
      Result := ItemCreate(DBPath, DBItem, DBItemDescription, ItemHnd);
end;

function TObjectDataManager.ItemUpdate(var ItemHnd: TItemHandle): Boolean;
begin
  Result := db_ItemUpdate(ItemHnd, FDBHandle);
end;

function TObjectDataManager.ItemClose(var ItemHnd: TItemHandle): Boolean;
begin
  Result := db_ItemClose(ItemHnd, FDBHandle);
end;

function TObjectDataManager.ItemCopyTo(var ItemHnd: TItemHandle; DestDB: TObjectDataManager; var DestItemHandle: TItemHandle; const CopySize: Int64): Boolean;
var
  i: Integer;
  buff: array [0 .. MaxBuffSize] of Byte;
begin
  Result := False;
  if CopySize > MaxBuffSize then
    begin
      for i := 1 to (CopySize div MaxBuffSize) do
        begin
          if not ItemRead(ItemHnd, MaxBuffSize, buff) then
              Exit;
          if not DestDB.ItemWrite(DestItemHandle, MaxBuffSize, buff) then
              Exit;
        end;
      if (CopySize mod MaxBuffSize) > 0 then
        begin
          if not ItemRead(ItemHnd, CopySize mod MaxBuffSize, buff) then
              Exit;
          if not DestDB.ItemWrite(DestItemHandle, CopySize mod MaxBuffSize, buff) then
              Exit;
        end;
    end
  else
    begin
      if CopySize > 0 then
        begin
          if not ItemRead(ItemHnd, CopySize, buff) then
              Exit;
          if not DestDB.ItemWrite(DestItemHandle, CopySize, buff) then
              Exit;
        end;
    end;
  Result := True;
end;

function TObjectDataManager.ItemMove(const DBPath, ItemName, destPath: SystemString): Boolean;
begin
  Result := db_MoveItem(DBPath, ItemName, destPath, FDefaultItemID, FDBHandle);
end;

function TObjectDataManager.ItemRename(const FieldPos: Int64; var ItemHnd: TItemHandle; const NewName, NewDescription: SystemString): Boolean;
begin
  Result := db_ItemReName(FieldPos, NewName, NewDescription, ItemHnd, FDBHandle);
end;

function TObjectDataManager.ItemFastInsertNew(const FieldPos, InsertHeaderPos: Int64; const DBItem, DBItemDescription: SystemString; var ItemHnd: TItemHandle): Boolean;
begin
  Init_TTMDBItemHandle(ItemHnd);
  Result := db_ItemFastInsertNew(DBItem, DBItemDescription, FieldPos, InsertHeaderPos, FDefaultItemID, ItemHnd, FDBHandle);
end;

function TObjectDataManager.ItemFastCreate(const fPos: Int64; const DBItem, DBItemDescription: SystemString; var ItemHnd: TItemHandle): Boolean;
begin
  Init_TTMDBItemHandle(ItemHnd);
  Result := db_ItemFastCreate(DBItem, DBItemDescription, fPos, FDefaultItemID, ItemHnd, FDBHandle);
end;

function TObjectDataManager.ItemFastOpen(const fPos: Int64; var ItemHnd: TItemHandle): Boolean;
begin
  Init_TTMDBItemHandle(ItemHnd);
  Result := db_ItemFastOpen(fPos, FDefaultItemID, ItemHnd, FDBHandle);
end;

function TObjectDataManager.ItemFastResetBody(const fPos: Int64): Boolean;
var
  ItemHnd: TItemHandle;
begin
  Result := ItemFastOpen(fPos, ItemHnd)
    and db_ItemBodyReset(ItemHnd, FDBHandle);
end;

function TObjectDataManager.ItemFastExists(const FieldPos: Int64; const DBItem: SystemString): Boolean;
var
  ItemSearchHnd: TItemSearch;
begin
  Init_TTMDBSearchItem(ItemSearchHnd);
  Result := db_FastFindFirstItem(FieldPos, DBItem, FDefaultItemID, ItemSearchHnd, FDBHandle);
end;

function TObjectDataManager.ItemFastFindFirst(const FieldPos: Int64; const DBItem: SystemString; var ItemSearchHandle: TItemSearch): Boolean;
begin
  Init_TTMDBSearchItem(ItemSearchHandle);
  Result := db_FastFindFirstItem(FieldPos, DBItem, FDefaultItemID, ItemSearchHandle, FDBHandle);
end;

function TObjectDataManager.ItemFastFindLast(const FieldPos: Int64; const DBItem: SystemString; var ItemSearchHandle: TItemSearch): Boolean;
begin
  Init_TTMDBSearchItem(ItemSearchHandle);
  Result := db_FastFindLastItem(FieldPos, DBItem, FDefaultItemID, ItemSearchHandle, FDBHandle);
end;

function TObjectDataManager.ItemFastFindNext(var ItemSearchHandle: TItemSearch): Boolean;
begin
  Result := db_FastFindNextItem(ItemSearchHandle, FDefaultItemID, FDBHandle);
end;

function TObjectDataManager.ItemFastFindPrev(var ItemSearchHandle: TItemSearch): Boolean;
begin
  Result := db_FastFindPrevItem(ItemSearchHandle, FDefaultItemID, FDBHandle);
end;

function TObjectDataManager.ItemFindFirst(const DBPath, DBItem: SystemString; var ItemSearchHandle: TItemSearch): Boolean;
begin
  Init_TTMDBSearchItem(ItemSearchHandle);
  Result := db_FindFirstItem(DBPath, DBItem, FDefaultItemID, ItemSearchHandle, FDBHandle);
end;

function TObjectDataManager.ItemFindLast(const DBPath, DBItem: SystemString; var ItemSearchHandle: TItemSearch): Boolean;
begin
  Init_TTMDBSearchItem(ItemSearchHandle);
  Result := db_FindLastItem(DBPath, DBItem, FDefaultItemID, ItemSearchHandle, FDBHandle);
end;

function TObjectDataManager.ItemFindNext(var ItemSearchHandle: TItemSearch): Boolean;
begin
  Result := db_FindNextItem(ItemSearchHandle, FDefaultItemID, FDBHandle);
end;

function TObjectDataManager.ItemFindPrev(var ItemSearchHandle: TItemSearch): Boolean;
begin
  Result := db_FindPrevItem(ItemSearchHandle, FDefaultItemID, FDBHandle);
end;

function TObjectDataManager.ItemRead(var ItemHnd: TItemHandle; const siz: Int64; var Buffers): Boolean;
begin
  Result := db_ItemRead(siz, Buffers, ItemHnd, FDBHandle);
end;

function TObjectDataManager.ItemSeekStart(var ItemHnd: TItemHandle): Boolean;
begin
  Result := db_ItemSeekStartPos(ItemHnd, FDBHandle);
end;

function TObjectDataManager.ItemSeekLast(var ItemHnd: TItemHandle): Boolean;
begin
  Result := db_ItemSeekLastPos(ItemHnd, FDBHandle);
end;

function TObjectDataManager.ItemSeek(var ItemHnd: TItemHandle; const ItemOffset: Int64): Boolean;
var
  siz: Integer;
begin
  siz := db_ItemGetSize(ItemHnd, FDBHandle);
  if ItemOffset > siz then
      Result := db_AppendItemSize(ItemHnd, ItemOffset - siz, FDBHandle)
  else if ItemOffset = siz then
      Result := db_ItemSeekLastPos(ItemHnd, FDBHandle)
  else if ItemOffset = 0 then
      Result := db_ItemSeekStartPos(ItemHnd, FDBHandle)
  else
      Result := db_ItemSeekPos(ItemOffset, ItemHnd, FDBHandle);
end;

function TObjectDataManager.ItemGetPos(var ItemHnd: TItemHandle): Int64;
begin
  Result := db_ItemGetPos(ItemHnd, FDBHandle);
end;

function TObjectDataManager.ItemGetSize(var ItemHnd: TItemHandle): Int64;
begin
  Result := db_ItemGetSize(ItemHnd, FDBHandle);
end;

function TObjectDataManager.ItemWrite(var ItemHnd: TItemHandle; const siz: Int64; var Buffers): Boolean;
begin
  Result := db_ItemWrite(siz, Buffers, ItemHnd, FDBHandle);
end;

function TObjectDataManager.ItemReadToStream(var ItemHnd: TItemHandle; stream: TCoreClassStream): Boolean;
var
  sour: TItemStream;
begin
  sour := TItemStream.Create(Self, ItemHnd);
  sour.SeekStart();
  Result := stream.CopyFrom(sour, sour.Size) = sour.Size;
  ItemHnd := sour.Hnd^;
  DisposeObject(sour);
end;

function TObjectDataManager.ItemWriteFromStream(var ItemHnd: TItemHandle; stream: TCoreClassStream): Boolean;
var
  sour: TItemStream;
begin
  sour := TItemStream.Create(Self, ItemHnd);
  sour.SeekStart();
  stream.Position := 0;
  Result := sour.CopyFrom(stream, stream.Size) = stream.Size;
  sour.CloseHandle;
  ItemHnd := sour.Hnd^;
  DisposeObject(sour);
end;

function TObjectDataManager.ItemReadToStream(const DBPath, DBItem: SystemString; stream: TCoreClassStream): Boolean;
var
  itmHnd: TItemHandle;
begin
  Result := False;
  if not ItemOpen(DBPath, DBItem, itmHnd) then
      Exit;
  Result := ItemReadToStream(itmHnd, stream);
end;

function TObjectDataManager.ItemWriteFromStream(const DBPath, DBItem: SystemString; stream: TCoreClassStream): Boolean;
var
  itmHnd: TItemHandle;
begin
  Result := False;
  ItemDelete(DBPath, DBItem);
  if ItemCreate(DBPath, DBItem, DBItem, itmHnd) then
      Result := ItemWriteFromStream(itmHnd, stream);
end;

function TObjectDataManager.RecursionSearchFirst(const InitPath, Filter: SystemString; var RecursionSearchHnd: TItemRecursionSearch): Boolean;
begin
  Init_TTMDBRecursionSearch(RecursionSearchHnd);
  Result := db_RecursionSearchFirst(InitPath, Filter, RecursionSearchHnd, FDBHandle);
end;

function TObjectDataManager.RecursionSearchNext(var RecursionSearchHnd: TItemRecursionSearch): Boolean;
begin
  Result := db_RecursionSearchNext(RecursionSearchHnd, FDBHandle);
end;

function TObjectDataManager.DBHandlePtr: PTMDB;
begin
  Result := @FDBHandle;
end;

procedure TObjectDataManagerOfCache.TObjectDataCacheItem.write(var wVal: TItem);
begin
  Description := wVal.Description;
  ExtID := wVal.ExtID;
  FirstBlockPOS := wVal.FirstBlockPOS;
  LastBlockPOS := wVal.LastBlockPOS;
  Size := wVal.Size;
  BlockCount := wVal.BlockCount;
  CurrentBlockSeekPOS := wVal.CurrentBlockSeekPOS;
  CurrentFileSeekPOS := wVal.CurrentFileSeekPOS;
  Return := wVal.Return;
  MemorySiz := 0;
end;

procedure TObjectDataManagerOfCache.TObjectDataCacheItem.read(var rVal: TItem);
begin
  rVal.Description := Description;
  rVal.ExtID := ExtID;
  rVal.FirstBlockPOS := FirstBlockPOS;
  rVal.LastBlockPOS := LastBlockPOS;
  rVal.Size := Size;
  rVal.BlockCount := BlockCount;
  rVal.CurrentBlockSeekPOS := CurrentBlockSeekPOS;
  rVal.CurrentFileSeekPOS := CurrentFileSeekPOS;
  rVal.Return := Return;
end;

procedure TObjectDataManagerOfCache.TObjectDataCacheField.write(var wVal: TField);
begin
  UpLevelFieldPOS := wVal.UpLevelFieldPOS;
  Description := wVal.Description;
  HeaderCount := wVal.HeaderCount;
  FirstHeaderPOS := wVal.FirstHeaderPOS;
  LastHeaderPOS := wVal.LastHeaderPOS;
  Return := wVal.Return;
  MemorySiz := 0;
end;

procedure TObjectDataManagerOfCache.TObjectDataCacheField.read(var rVal: TField);
begin
  rVal.UpLevelFieldPOS := UpLevelFieldPOS;
  rVal.Description := Description;
  rVal.HeaderCount := HeaderCount;
  rVal.FirstHeaderPOS := FirstHeaderPOS;
  rVal.LastHeaderPOS := LastHeaderPOS;
  rVal.Return := Return;
end;

procedure TObjectDataManagerOfCache.HeaderCache_DataFreeProc(p: Pointer);
begin
  Dispose(PObjectDataCacheHeader(p));
end;

procedure TObjectDataManagerOfCache.ItemBlockCache_DataFreeProc(p: Pointer);
begin
  Dispose(PObjectDataCacheItemBlock(p));
end;

procedure TObjectDataManagerOfCache.ItemCache_DataFreeProc(p: Pointer);
begin
  Dispose(PObjectDataCacheItem(p));
end;

procedure TObjectDataManagerOfCache.FieldCache_DataFreeProc(p: Pointer);
begin
  Dispose(PObjectDataCacheField(p));
end;

procedure TObjectDataManagerOfCache.PrepareWritePool_DataFreeProc(obj: TCoreClassObject);
begin
  DisposeObject(obj);
end;

function TObjectDataManagerOfCache.CheckPreapreWrite(fPos: Int64): Boolean;
begin
  Result := (FDBHandle.IOHnd.Handle is TReliableFileStream) and
    (not FDBHandle.IOHnd.IsOnlyRead) and (FDBHandle.IOHnd.IsOpen) and (fPos < FDBHandle.IOHnd.Size);
end;

procedure TObjectDataManagerOfCache.DeleteHeaderProc(fPos: Int64);
var
  h: THeader;
  itm: TItem;
  bPos: Int64;
  block: TItemBlock;
begin
  if not dbHeader_ReadRec(fPos, FDBHandle.IOHnd, h) then
    begin
      FPrepareWritePool.Delete(fPos);
      FHeaderCache.Delete(fPos);
      Exit;
    end;

  if h.ID = DB_Header_Field_ID then
      FFieldCache.Delete(h.DataPosition)
  else if h.ID = DB_Header_Item_ID then
    begin
      itm.RHeader := h;
      if dbItem_OnlyReadItemRec(h.DataPosition, FDBHandle.IOHnd, itm) then
        begin
          bPos := itm.FirstBlockPOS;
          while dbItem_OnlyReadItemBlockRec(bPos, FDBHandle.IOHnd, block) do
            begin
              FPrepareWritePool.Delete(bPos);
              FItemBlockCache.Delete(bPos);
              bPos := block.NextBlockPOS;
              if bPos = itm.LastBlockPOS then
                  break;
            end;
        end;
      FItemCache.Delete(h.DataPosition);
    end;

  FPrepareWritePool.Delete(fPos);
  FPrepareWritePool.Delete(h.DataPosition);
  FHeaderCache.Delete(fPos);
end;

procedure TObjectDataManagerOfCache.PrepareHeaderWriteProc(fPos: Int64; var wVal: THeader; var Done: Boolean);
var
  m64: TMemoryStream64;
  Hnd: TIOHnd;
begin
  Done := False;
  if not CheckPreapreWrite(fPos) then
      Exit;
  m64 := TMemoryStream64(FPrepareWritePool[fPos]);
  if m64 = nil then
    begin
      m64 := TMemoryStream64.CustomCreate(DB_Header_Size);
      FPrepareWritePool.Add(fPos, m64, False);
    end;
  InitIOHnd(Hnd);
  umlFileOpenAsStream('', m64, Hnd, False);
  dbHeader_WriteRec(0, Hnd, wVal);
  umlFileClose(Hnd);
  if m64.Position <> m64.Size then
      RaiseInfo('preapre write error!');
  m64.Position := 0;

  Done := True;
end;

procedure TObjectDataManagerOfCache.HeaderWriteProc(fPos: Int64; var wVal: THeader);
var
  p: PObjectDataCacheHeader;
begin
  p := PObjectDataCacheHeader(FHeaderCache[wVal.CurrentHeader]);
  if p = nil then
    begin
      new(p);
      p^ := wVal;
      FHeaderCache.Add(wVal.CurrentHeader, p, False);
    end
  else
      p^ := wVal;

  p^.Return := DB_Header_ok;
end;

procedure TObjectDataManagerOfCache.HeaderReadProc(fPos: Int64; var rVal: THeader; var Done: Boolean);
var
  p: PObjectDataCacheHeader;
  m64: TMemoryStream64;
  Hnd: TIOHnd;
begin
  p := PObjectDataCacheHeader(FHeaderCache[fPos]);
  Done := p <> nil;
  if not Done then
    begin
      m64 := TMemoryStream64(FPrepareWritePool[fPos]);
      if m64 <> nil then
        begin
          InitIOHnd(Hnd);
          umlFileOpenAsStream('', m64, Hnd, False);
          Done := dbHeader_ReadRec(0, Hnd, rVal);
          umlFileClose(Hnd);
          m64.Position := 0;
          if Done then
            begin
              new(p);
              p^ := rVal;
              FHeaderCache.Add(rVal.CurrentHeader, p, False);
              p^.Return := DB_Header_ok;
            end;
        end;
    end
  else
      rVal := p^;
end;

procedure TObjectDataManagerOfCache.PrepareItemBlockWriteProc(fPos: Int64; var wVal: TItemBlock; var Done: Boolean);
var
  m64: TMemoryStream64;
  Hnd: TIOHnd;
begin
  Done := False;
  if not CheckPreapreWrite(fPos) then
      Exit;
  m64 := TMemoryStream64(FPrepareWritePool[fPos]);
  if m64 = nil then
    begin
      m64 := TMemoryStream64.CustomCreate(DB_Item_BlockSize);
      FPrepareWritePool.Add(fPos, m64, False);
    end;
  InitIOHnd(Hnd);
  umlFileOpenAsStream('', m64, Hnd, False);
  dbItem_OnlyWriteItemBlockRec(0, Hnd, wVal);
  umlFileClose(Hnd);
  if m64.Position <> m64.Size then
      RaiseInfo('preapre write error!');
  m64.Position := 0;

  Done := True;
end;

procedure TObjectDataManagerOfCache.ItemBlockWriteProc(fPos: Int64; var wVal: TItemBlock);
var
  p: PObjectDataCacheItemBlock;
begin
  p := PObjectDataCacheItemBlock(FItemBlockCache[wVal.CurrentBlockPOS]);
  if p = nil then
    begin
      new(p);
      p^ := wVal;
      FItemBlockCache.Add(wVal.CurrentBlockPOS, p, False);
    end
  else
      p^ := wVal;

  p^.Return := DB_Item_ok;
end;

procedure TObjectDataManagerOfCache.ItemBlockReadProc(fPos: Int64; var rVal: TItemBlock; var Done: Boolean);
var
  p: PObjectDataCacheItemBlock;
  m64: TMemoryStream64;
  Hnd: TIOHnd;
begin
  p := PObjectDataCacheItemBlock(FItemBlockCache[fPos]);
  Done := p <> nil;
  if not Done then
    begin
      m64 := TMemoryStream64(FPrepareWritePool[fPos]);
      if m64 <> nil then
        begin
          InitIOHnd(Hnd);
          umlFileOpenAsStream('', m64, Hnd, False);
          Done := dbItem_OnlyReadItemBlockRec(0, Hnd, rVal);
          umlFileClose(Hnd);
          m64.Position := 0;
          if Done then
            begin
              new(p);
              p^ := rVal;
              FItemBlockCache.Add(rVal.CurrentBlockPOS, p, False);
              p^.Return := DB_Item_ok;
            end;
        end;
    end
  else
      rVal := p^;
end;

procedure TObjectDataManagerOfCache.PrepareItemWriteProc(fPos: Int64; var wVal: TItem; var Done: Boolean);
begin
  Done := False;
  if not CheckPreapreWrite(fPos) then
      Exit;

  PrepareOnlyItemRecWriteProc(wVal.RHeader.DataPosition, wVal, Done);
  PrepareHeaderWriteProc(fPos, wVal.RHeader, Done);
end;

procedure TObjectDataManagerOfCache.ItemWriteProc(fPos: Int64; var wVal: TItem);
begin
  HeaderWriteProc(fPos, wVal.RHeader);
  OnlyItemRecWriteProc(wVal.RHeader.DataPosition, wVal);
end;

procedure TObjectDataManagerOfCache.ItemReadProc(fPos: Int64; var rVal: TItem; var Done: Boolean);
var
  p: PObjectDataCacheItem;
begin
  HeaderReadProc(fPos, rVal.RHeader, Done);

  if not Done then
    begin
      FDBHandle.IOHnd.Data := nil;
      Done := dbHeader_ReadRec(fPos, FDBHandle.IOHnd, rVal.RHeader);
      FDBHandle.IOHnd.Data := @FDBHandle;

      if Done then
          HeaderWriteProc(fPos, rVal.RHeader)
      else
          Exit;
    end;

  OnlyItemRecReadProc(rVal.RHeader.DataPosition, rVal, Done);
end;

procedure TObjectDataManagerOfCache.PrepareOnlyItemRecWriteProc(fPos: Int64; var wVal: TItem; var Done: Boolean);
var
  m64: TMemoryStream64;
  Hnd: TIOHnd;
begin
  Done := False;
  if not CheckPreapreWrite(fPos) then
      Exit;
  m64 := TMemoryStream64(FPrepareWritePool[fPos]);
  if m64 = nil then
    begin
      m64 := TMemoryStream64.CustomCreate(DB_Item_Size);
      FPrepareWritePool.Add(fPos, m64, False);
    end;
  InitIOHnd(Hnd);
  umlFileOpenAsStream('', m64, Hnd, False);
  dbItem_OnlyWriteItemRec(0, Hnd, wVal);
  umlFileClose(Hnd);
  if m64.Position <> m64.Size then
      RaiseInfo('preapre write error!');
  m64.Position := 0;

  Done := True;
end;

procedure TObjectDataManagerOfCache.OnlyItemRecWriteProc(fPos: Int64; var wVal: TItem);
var
  p: PObjectDataCacheItem;
begin
  p := PObjectDataCacheItem(FItemCache[fPos]);
  if p = nil then
    begin
      new(p);
      p^.write(wVal);
      FItemCache.Add(fPos, p, False);
    end
  else
      p^.write(wVal);

  p^.Return := DB_Item_ok;
end;

procedure TObjectDataManagerOfCache.OnlyItemRecReadProc(fPos: Int64; var rVal: TItem; var Done: Boolean);
var
  p: PObjectDataCacheItem;
  m64: TMemoryStream64;
  Hnd: TIOHnd;
begin
  p := PObjectDataCacheItem(FItemCache[fPos]);
  Done := p <> nil;
  if not Done then
    begin
      m64 := TMemoryStream64(FPrepareWritePool[fPos]);
      if m64 <> nil then
        begin
          InitIOHnd(Hnd);
          umlFileOpenAsStream('', m64, Hnd, False);
          Done := dbItem_OnlyReadItemRec(0, Hnd, rVal);
          umlFileClose(Hnd);
          m64.Position := 0;
          if Done then
            begin
              new(p);
              p^.read(rVal);
              FItemCache.Add(fPos, p, False);
              p^.Return := DB_Item_ok;
            end;
        end;
    end
  else
      p^.read(rVal);
end;

procedure TObjectDataManagerOfCache.PrepareFieldWriteProc(fPos: Int64; var wVal: TField; var Done: Boolean);
begin
  Done := False;
  if not CheckPreapreWrite(fPos) then
      Exit;

  PrepareOnlyFieldRecWriteProc(wVal.RHeader.DataPosition, wVal, Done);
  PrepareHeaderWriteProc(fPos, wVal.RHeader, Done);
end;

procedure TObjectDataManagerOfCache.FieldWriteProc(fPos: Int64; var wVal: TField);
begin
  HeaderWriteProc(fPos, wVal.RHeader);
  OnlyFieldRecWriteProc(wVal.RHeader.DataPosition, wVal);
end;

procedure TObjectDataManagerOfCache.FieldReadProc(fPos: Int64; var rVal: TField; var Done: Boolean);
begin
  HeaderReadProc(fPos, rVal.RHeader, Done);

  if not Done then
    begin
      FDBHandle.IOHnd.Data := nil;
      Done := dbHeader_ReadRec(fPos, FDBHandle.IOHnd, rVal.RHeader);
      FDBHandle.IOHnd.Data := @FDBHandle;

      if Done then
          HeaderWriteProc(fPos, rVal.RHeader)
      else
          Exit;
    end;

  OnlyFieldRecReadProc(rVal.RHeader.DataPosition, rVal, Done);
end;

procedure TObjectDataManagerOfCache.PrepareOnlyFieldRecWriteProc(fPos: Int64; var wVal: TField; var Done: Boolean);
var
  m64: TMemoryStream64;
  Hnd: TIOHnd;
begin
  Done := False;
  if not CheckPreapreWrite(fPos) then
      Exit;
  m64 := TMemoryStream64(FPrepareWritePool[fPos]);
  if m64 = nil then
    begin
      m64 := TMemoryStream64.CustomCreate(DB_Field_Size);
      FPrepareWritePool.Add(fPos, m64, False);
    end;
  InitIOHnd(Hnd);
  umlFileOpenAsStream('', m64, Hnd, False);
  dbField_OnlyWriteFieldRec(0, Hnd, wVal);
  umlFileClose(Hnd);
  if m64.Position <> m64.Size then
      RaiseInfo('preapre write error!');
  m64.Position := 0;

  Done := True;
end;

procedure TObjectDataManagerOfCache.OnlyFieldRecWriteProc(fPos: Int64; var wVal: TField);
var
  p: PObjectDataCacheField;
begin
  p := PObjectDataCacheField(FFieldCache[fPos]);
  if p = nil then
    begin
      new(p);
      p^.write(wVal);
      FFieldCache.Add(fPos, p, False);
    end
  else
      p^.write(wVal);

  p^.Return := DB_Field_ok;
end;

procedure TObjectDataManagerOfCache.OnlyFieldRecReadProc(fPos: Int64; var rVal: TField; var Done: Boolean);
var
  p: PObjectDataCacheField;
  m64: TMemoryStream64;
  Hnd: TIOHnd;
begin
  p := PObjectDataCacheField(FFieldCache[fPos]);
  Done := p <> nil;
  if not Done then
    begin
      m64 := TMemoryStream64(FPrepareWritePool[fPos]);
      if m64 <> nil then
        begin
          InitIOHnd(Hnd);
          umlFileOpenAsStream('', m64, Hnd, False);
          Done := dbField_OnlyReadFieldRec(0, Hnd, rVal);
          umlFileClose(Hnd);
          m64.Position := 0;
          if Done then
            begin
              new(p);
              p^.read(rVal);
              FFieldCache.Add(fPos, p, False);
              p^.Return := DB_Item_ok;
            end;
        end;
    end
  else
      p^.read(rVal);
end;

procedure TObjectDataManagerOfCache.PrepareTMDBWriteProc(fPos: Int64; const wVal: PTMDB; var Done: Boolean);
var
  m64: TMemoryStream64;
  Hnd: TIOHnd;
begin
  Done := False;
  if not CheckPreapreWrite(fPos) then
      Exit;
  m64 := TMemoryStream64(FPrepareWritePool[fPos]);
  if m64 = nil then
    begin
      m64 := TMemoryStream64.CustomCreate(DB_Size);
      FPrepareWritePool.Add(fPos, m64, False);
    end;
  InitIOHnd(Hnd);
  umlFileOpenAsStream('', m64, Hnd, False);

  FDBHandle.IOHnd.Data := nil;

  db_WriteRec(0, Hnd, wVal^);

  FDBHandle.IOHnd.Data := @FDBHandle;

  umlFileClose(Hnd);
  if m64.Position <> m64.Size then
      RaiseInfo('preapre write error!');
  m64.Position := 0;

  Done := True;
end;

procedure TObjectDataManagerOfCache.TMDBWriteProc(fPos: Int64; const wVal: PTMDB);
begin
end;

procedure TObjectDataManagerOfCache.TMDBReadProc(fPos: Int64; const rVal: PTMDB; var Done: Boolean);
begin
  Done := False;
end;

procedure TObjectDataManagerOfCache.CheckAndRestoreFlush;
var
  swapFileName: TPascalString;
  swapHnd: TCoreClassFileStream;
  swaphead: TSwapHead;
  CheckSuccessed: Boolean;
  m64: TMemoryStream64;
  m5: TMD5;
begin
  if not(FDBHandle.IOHnd.Handle is TReliableFileStream) then
      Exit;
  if (FDBHandle.IOHnd.IsOnlyRead) or (not FDBHandle.IOHnd.IsOpen) then
      Exit;

  swapFileName := TReliableFileStream(FDBHandle.IOHnd.Handle).fileName + '.~flush';

  if not umlFileExists(swapFileName) then
      Exit;

  swapHnd := nil;
  try
      swapHnd := TCoreClassFileStream.Create(swapFileName, fmOpenReadWrite);
  except
    DisposeObject(swapHnd);
    umlDeleteFile(swapFileName);
    Exit;
  end;
  CheckSuccessed := True;
  m64 := TMemoryStream64.CustomCreate(8192);
  while swapHnd.Position < swapHnd.Size do
    begin
      if swapHnd.read(swaphead, SizeOf(swaphead)) <> SizeOf(swaphead) then
        begin
          DoStatus('%s CRC header errors, the database will be restored to previous state', [umlGetFileName(swapFileName).Text]);
          CheckSuccessed := False;
          break;
        end;
      m64.Clear;
      if m64.CopyFrom(swapHnd, swaphead.Size) <> swaphead.Size then
        begin
          DoStatus('%s CRC data loss, the database will be restored to previous state', [umlGetFileName(swapFileName).Text]);
          CheckSuccessed := False;
          break;
        end;
      m5 := umlMD5(m64.Memory, m64.Size);
      if not umlCompareMD5(m5, swaphead.MD5) then
        begin
          DoStatus('%s CRC validation errors, the database will be restored to previous state', [umlGetFileName(swapFileName).Text]);
          CheckSuccessed := False;
          break;
        end;
    end;

  if CheckSuccessed then
    begin
      DoStatus('%s CRC done!, the database to restored to previous state', [umlGetFileName(swapFileName).Text]);
      swapHnd.Position := 0;
      swapHnd.read(swaphead, SizeOf(swaphead));
      m64.Clear;
      m64.CopyFrom(swapHnd, swaphead.Size);

      FDBHandle.IOHnd.Handle.Position := swaphead.Position;
      FDBHandle.IOHnd.Handle.write(m64.Memory^, m64.Size);
    end;

  DisposeObject(swapHnd);
  DisposeObject(m64);
  umlDeleteFile(swapFileName);
end;

procedure TObjectDataManagerOfCache.DoCreateFinish;
begin
  inherited DoCreateFinish;

  CheckAndRestoreFlush;

  FHeaderCache := TInt64HashPointerList.CustomCreate(10 * 10000);
  FHeaderCache.AutoFreeData := True;
  FHeaderCache.AccessOptimization := True;

  FItemBlockCache := TInt64HashPointerList.CustomCreate(10 * 10000);
  FItemBlockCache.AutoFreeData := True;
  FItemBlockCache.AccessOptimization := True;

  FItemCache := TInt64HashPointerList.CustomCreate(10 * 10000);
  FItemCache.AutoFreeData := True;
  FItemCache.AccessOptimization := True;

  FFieldCache := TInt64HashPointerList.CustomCreate(10 * 10000);
  FFieldCache.AutoFreeData := True;
  FFieldCache.AccessOptimization := True;

  FPrepareWritePool := TInt64HashObjectList.CustomCreate(40 * 10000);
  FPrepareWritePool.AutoFreeData := True;
  FPrepareWritePool.AccessOptimization := True;

  FHeaderCache.OnFreePtr := {$IFDEF FPC}@{$ENDIF FPC}HeaderCache_DataFreeProc;
  FItemBlockCache.OnFreePtr := {$IFDEF FPC}@{$ENDIF FPC}ItemBlockCache_DataFreeProc;
  FItemCache.OnFreePtr := {$IFDEF FPC}@{$ENDIF FPC}ItemCache_DataFreeProc;
  FFieldCache.OnFreePtr := {$IFDEF FPC}@{$ENDIF FPC}FieldCache_DataFreeProc;
  FPrepareWritePool.OnObjectFreeProc := {$IFDEF FPC}@{$ENDIF FPC}PrepareWritePool_DataFreeProc;

  BuildDBCacheIntf;
end;

destructor TObjectDataManagerOfCache.Destroy;
begin
  FreeDBCacheIntf;
  DisposeObject([FHeaderCache, FItemBlockCache, FItemCache, FFieldCache, FPrepareWritePool]);
  inherited Destroy;
end;

procedure TObjectDataManagerOfCache.BuildDBCacheIntf;
begin
  FDBHandle.OnDeleteHeader := {$IFDEF FPC}@{$ENDIF FPC}DeleteHeaderProc;

  FDBHandle.OnPrepareWriteHeader := {$IFDEF FPC}@{$ENDIF FPC}PrepareHeaderWriteProc;
  FDBHandle.OnWriteHeader := {$IFDEF FPC}@{$ENDIF FPC}HeaderWriteProc;
  FDBHandle.OnReadHeader := {$IFDEF FPC}@{$ENDIF FPC}HeaderReadProc;

  FDBHandle.OnPrepareWriteItemBlock := {$IFDEF FPC}@{$ENDIF FPC}PrepareItemBlockWriteProc;
  FDBHandle.OnWriteItemBlock := {$IFDEF FPC}@{$ENDIF FPC}ItemBlockWriteProc;
  FDBHandle.OnReadItemBlock := {$IFDEF FPC}@{$ENDIF FPC}ItemBlockReadProc;

  FDBHandle.OnPrepareWriteItem := {$IFDEF FPC}@{$ENDIF FPC}PrepareItemWriteProc;
  FDBHandle.OnWriteItem := {$IFDEF FPC}@{$ENDIF FPC}ItemWriteProc;
  FDBHandle.OnReadItem := {$IFDEF FPC}@{$ENDIF FPC}ItemReadProc;

  FDBHandle.OnPrepareOnlyWriteItemRec := {$IFDEF FPC}@{$ENDIF FPC}PrepareOnlyItemRecWriteProc;
  FDBHandle.OnOnlyWriteItemRec := {$IFDEF FPC}@{$ENDIF FPC}OnlyItemRecWriteProc;
  FDBHandle.OnOnlyReadItemRec := {$IFDEF FPC}@{$ENDIF FPC}OnlyItemRecReadProc;

  FDBHandle.OnPrepareWriteField := {$IFDEF FPC}@{$ENDIF FPC}PrepareFieldWriteProc;
  FDBHandle.OnWriteField := {$IFDEF FPC}@{$ENDIF FPC}FieldWriteProc;
  FDBHandle.OnReadField := {$IFDEF FPC}@{$ENDIF FPC}FieldReadProc;

  FDBHandle.OnPrepareOnlyWriteFieldRec := {$IFDEF FPC}@{$ENDIF FPC}PrepareOnlyFieldRecWriteProc;
  FDBHandle.OnOnlyWriteFieldRec := {$IFDEF FPC}@{$ENDIF FPC}OnlyFieldRecWriteProc;
  FDBHandle.OnOnlyReadFieldRec := {$IFDEF FPC}@{$ENDIF FPC}OnlyFieldRecReadProc;

  FDBHandle.OnPrepareWriteTMDB := {$IFDEF FPC}@{$ENDIF FPC}PrepareTMDBWriteProc;
  FDBHandle.OnWriteTMDB := {$IFDEF FPC}@{$ENDIF FPC}TMDBWriteProc;
  FDBHandle.OnReadTMDB := {$IFDEF FPC}@{$ENDIF FPC}TMDBReadProc;
end;

procedure TObjectDataManagerOfCache.FreeDBCacheIntf;
begin
  CleaupCache;

  FDBHandle.OnDeleteHeader := nil;

  FDBHandle.OnPrepareWriteHeader := nil;
  FDBHandle.OnWriteHeader := nil;
  FDBHandle.OnReadHeader := nil;

  FDBHandle.OnPrepareWriteItemBlock := nil;
  FDBHandle.OnWriteItemBlock := nil;
  FDBHandle.OnReadItemBlock := nil;

  FDBHandle.OnPrepareWriteItem := nil;
  FDBHandle.OnWriteItem := nil;
  FDBHandle.OnReadItem := nil;

  FDBHandle.OnPrepareOnlyWriteItemRec := nil;
  FDBHandle.OnOnlyWriteItemRec := nil;
  FDBHandle.OnOnlyReadItemRec := nil;

  FDBHandle.OnPrepareWriteField := nil;
  FDBHandle.OnWriteField := nil;
  FDBHandle.OnReadField := nil;

  FDBHandle.OnPrepareOnlyWriteFieldRec := nil;
  FDBHandle.OnOnlyWriteFieldRec := nil;
  FDBHandle.OnOnlyReadFieldRec := nil;

  FDBHandle.OnPrepareWriteTMDB := nil;
  FDBHandle.OnWriteTMDB := nil;
  FDBHandle.OnReadTMDB := nil;
end;

procedure TObjectDataManagerOfCache.CleaupCache;
begin
  Flush;
  FHeaderCache.Clear;
  FItemBlockCache.Clear;
  FItemCache.Clear;
  FFieldCache.Clear;
  FPrepareWritePool.Clear;
end;

procedure TObjectDataManagerOfCache.SetPoolCache(const Value: Integer);
begin
  CleaupCache();
  FHeaderCache.SetHashBlockCount(Value);
  FItemBlockCache.SetHashBlockCount(Value);
  FItemCache.SetHashBlockCount(Value);
  FFieldCache.SetHashBlockCount(Value);
  FPrepareWritePool.SetHashBlockCount(Value * 4);
end;

procedure TObjectDataManagerOfCache.UpdateIO;
begin
  Flush();
end;

procedure TObjectDataManagerOfCache.Flush();
var
  i: NativeInt;
  p: PInt64HashListObjectStruct;
  m64: TMemoryStream64;
  swapFileName: TPascalString;
  swapHnd: TCoreClassFileStream;
  swaphead: TSwapHead;
begin
  if (not FDBHandle.IOHnd.IsOnlyRead)
    and (FDBHandle.IOHnd.IsOpen)
    and (FPrepareWritePool.Count > 0) then
    begin
      // step 1: flush to swap file
      if (FDBHandle.IOHnd.Handle is TReliableFileStream) then
        begin
          swapFileName := TReliableFileStream(FDBHandle.IOHnd.Handle).fileName + '.~flush';
          swapHnd := nil;
          try
            swapHnd := TCoreClassFileStream.Create(swapFileName, fmCreate);

            i := 0;
            p := FPrepareWritePool.FirstPtr;
            while i < FPrepareWritePool.Count do
              begin
                m64 := TMemoryStream64(p^.Data);
                if p^.i64 >= FDBHandle.IOHnd.Size then
                    RaiseInfo('flush: prepare write buffer error!');

                swaphead.Size := m64.Size;
                swaphead.MD5 := umlMD5(m64.Memory, m64.Size);
                swaphead.Position := p^.i64;
                swapHnd.write(swaphead, SizeOf(swaphead));
                swapHnd.write(m64.Memory^, m64.Size);
                inc(i);
                p := p^.Next;
              end;
          except
          end;
          DisposeObject(swapHnd);
        end;

      // step 2: flash fragment
      i := 0;
      p := FPrepareWritePool.FirstPtr;
      while i < FPrepareWritePool.Count do
        begin
          m64 := TMemoryStream64(p^.Data);
          FDBHandle.IOHnd.Handle.Position := p^.i64;
          FDBHandle.IOHnd.Handle.write(m64.Memory^, m64.Size);
          inc(i);
          p := p^.Next;
        end;

      // step 3: delete swap file
      if (FDBHandle.IOHnd.Handle is TReliableFileStream) then
          umlDeleteFile(swapFileName);
    end;
  FPrepareWritePool.Clear;

  inherited UpdateIO;
end;

function TObjectDataManagerOfCache.CacheStatus: SystemString;
begin
  Result := PFormat('h:%d b:%d i:%d f:%d w:%d', [FHeaderCache.Count, FItemBlockCache.Count, FItemCache.Count, FFieldCache.Count, FPrepareWritePool.Count]);
end;

function TObjectDataMarshal.GetItems(aIndex: Integer): TObjectDataManager;
begin
  Result := TObjectDataManager(FLibList.Objects[aIndex]);
end;

function TObjectDataMarshal.GetNames(AName: SystemString): TObjectDataManager;
var
  i: Integer;
  aUName: SystemString;
begin
  Result := nil;
  aUName := GetAbsoluteFileName(AName);
  if FLibList.Count > 0 then
    begin
      if FUseWildcard then
        begin
          if not umlMatchChar('\', aUName) then
              aUName := '*\' + aUName;
          for i := 0 to FLibList.Count - 1 do
            begin
              if umlMultipleMatch(False, aUName, FLibList[i]) then
                begin
                  Result := TObjectDataManager(FLibList.Objects[i]);
                  Exit;
                end;
            end;
        end
      else
        begin
          if umlMatchChar('\', aUName) then
            begin
              for i := 0 to FLibList.Count - 1 do
                if umlSameText(aUName, FLibList[i]) then
                  begin
                    Result := TObjectDataManager(FLibList.Objects[i]);
                    Exit;
                  end;
            end
          else
            begin
              for i := 0 to FLibList.Count - 1 do
                if umlSameText(aUName, umlGetLastStr(FLibList[i], '\')) then
                  begin
                    Result := TObjectDataManager(FLibList.Objects[i]);
                    Exit;
                  end;
            end;
        end;
    end;
end;

procedure TObjectDataMarshal.SetItems(aIndex: Integer; const Value: TObjectDataManager);
begin
  if Value <> nil then
    begin
      FLibList.Objects[aIndex] := Value;
      FLibList[aIndex] := GetAbsoluteFileName(Value.ObjectName);
    end;
end;

constructor TObjectDataMarshal.Create(dbItemID: Byte);
begin
  inherited Create;
  FID := dbItemID;
  FLibList := TCoreClassStringList.Create;
  FUseWildcard := True;
end;

destructor TObjectDataMarshal.Destroy;
begin
  Clear;
  DisposeObject(FLibList);
  inherited Destroy;
end;

function TObjectDataMarshal.GetAbsoluteFileName(fileName: SystemString): SystemString;
begin
  Result := umlUpperCase(umlCharReplace(umlTrimSpace(fileName), '/', '\')).Text;
end;

function TObjectDataMarshal.NewDB(dbFile: SystemString; dbOnlyRead: Boolean): TObjectDataManager;
var
  i: Integer;
  aUName: SystemString;
begin
  Result := nil;
  aUName := GetAbsoluteFileName(dbFile);
  if FLibList.Count > 0 then
    for i := 0 to FLibList.Count - 1 do
      if umlSameText(aUName, FLibList[i]) then
          Result := TObjectDataManager(FLibList.Objects[i]);
  if Result = nil then
    begin
      Result := TObjectDataManager.CreateNew(dbFile, FID);
      if Result.isAbort then
        begin
          DisposeObject(Result);
          Result := nil;
        end
      else
        begin
          FLibList.AddObject(GetAbsoluteFileName(Result.ObjectName), Result);
        end;
    end;
end;

function TObjectDataMarshal.Open(dbFile: SystemString; dbOnlyRead: Boolean): TObjectDataManager;
var
  i: Integer;
  aUName: SystemString;
begin
  Result := nil;
  aUName := GetAbsoluteFileName(dbFile);
  if FLibList.Count > 0 then
    for i := 0 to FLibList.Count - 1 do
      if umlSameText(aUName, FLibList[i]) then
          Result := TObjectDataManager(FLibList.Objects[i]);
  if Result = nil then
    begin
      Result := TObjectDataManager.Create(dbFile, FID, dbOnlyRead);
      if Result.isAbort then
        begin
          DisposeObject(Result);
          Result := nil;
        end
      else
        begin
          FLibList.AddObject(GetAbsoluteFileName(Result.ObjectName), Result);
        end;
    end;
end;

procedure TObjectDataMarshal.CloseDB(db: TObjectDataManager);
var
  i: Integer;
begin
  i := 0;
  while i < Count do
    if Items[i] = db then
        Delete(i)
    else
        inc(i);
end;

procedure TObjectDataMarshal.Clear;
begin
  while Count > 0 do
      Delete(0);
end;

function TObjectDataMarshal.Count: Integer;
begin
  Result := FLibList.Count;
end;

procedure TObjectDataMarshal.Delete(aIndex: Integer);
begin
  try
      DisposeObject(FLibList.Objects[aIndex]);
  except
  end;
  FLibList.Delete(aIndex);
end;

procedure TObjectDataMarshal.DeleteFromName(AName: SystemString);
var
  i: Integer;
  aUName: SystemString;
begin
  aUName := GetAbsoluteFileName(AName);
  if FLibList.Count > 0 then
    begin
      if FUseWildcard then
        begin
          if not umlMatchChar('\', aUName) then
              aUName := '*\' + aUName;
          i := 0;
          while i < FLibList.Count do
            begin
              if umlMultipleMatch(False, aUName, FLibList[i]) then
                begin
                  DisposeObject(FLibList.Objects[i]);
                  FLibList.Delete(i);
                end
              else
                  inc(i);
            end;
        end
      else
        begin
          if umlMatchChar('\', aUName) then
            begin
              i := 0;
              while i < FLibList.Count do
                begin
                  if umlSameText(aUName, FLibList[i]) then
                    begin
                      DisposeObject(FLibList.Objects[i]);
                      FLibList.Delete(i);
                    end
                  else
                      inc(i);
                end;
            end
          else
            begin
              i := 0;
              while i < FLibList.Count do
                begin
                  if umlSameText(aUName, umlGetLastStr(FLibList[i], '\')) then
                    begin
                      DisposeObject(FLibList.Objects[i]);
                      FLibList.Delete(i);
                    end
                  else
                      inc(i);
                end;
            end;
        end;
    end;
end;

procedure TObjectDataMarshal.UpdateAll;
var
  i: Integer;
begin
  if Count > 0 then
    for i := 0 to Count - 1 do
        Items[i].UpdateIO;
end;

procedure TObjectDataMarshal.Disable;
var
  i: Integer;
begin
  if Count > 0 then
    for i := 0 to Count - 1 do
        Items[i].Close;
end;

procedure TObjectDataMarshal.Enabled;
var
  i: Integer;
begin
  if Count > 0 then
    for i := 0 to Count - 1 do
        Items[i].Open;
end;

procedure TestObjectData_(c: TObjectDataManagerClass);
var
  db: TObjectDataManager;
  itmHnd1, itmHnd2, itmHnd3, itmHnd4: TItemHandle;
  buff: TBytes;
  nameL: U_StringArray;
  n: U_String;
begin
  db := c.CreateAsStream(TMemoryStream64.CustomCreate($FFFF), '', 0, False, True, True);

  if db.CreateRootField('_RootField') then
      DoStatus('CreateRootField ok')
  else
      DoStatus('CreateRootField error');
  if db.SetRootField('_RootField') then
      DoStatus('SetRootField ok')
  else
      DoStatus('SetRootField error');

  if db is TObjectDataManagerOfCache then
      DoStatus(TObjectDataManagerOfCache(db).CacheStatus);

  DoStatus('db size: %d', [db.Size]);

  DoStatus('DB field test.');
  db.CreateField('/a/b/c', '');

  if db is TObjectDataManagerOfCache then
      DoStatus(TObjectDataManagerOfCache(db).CacheStatus);

  if db.GetPathFieldHeaderCount('/a') <> 1 then
      DoStatus('create field error');
  if db.GetPathFieldHeaderCount('/a/b') <> 1 then
      DoStatus('create field error');
  if db.GetPathFieldHeaderCount('/a/b/c') <> 0 then
      DoStatus('create field error');

  if db is TObjectDataManagerOfCache then
      DoStatus(TObjectDataManagerOfCache(db).CacheStatus);

  DoStatus('DB item body test');
  if not db.ItemCreate('/a/b/c', '1', '', itmHnd1) then
      DoStatus('create item error');
  buff := umlBytesOf('1111');
  db.ItemWrite(itmHnd1, length(buff), buff[0]);
  DoStatus('item1 size:%d', [itmHnd1.Item.Size]);

  if db is TObjectDataManagerOfCache then
      DoStatus(TObjectDataManagerOfCache(db).CacheStatus);

  if not db.ItemCreate('/a/b/c', '2', '', itmHnd2) then
      DoStatus('create item error');
  buff := umlBytesOf('22222');
  db.ItemWrite(itmHnd2, length(buff), buff[0]);
  DoStatus('item2 size:%d', [itmHnd2.Item.Size]);

  if db is TObjectDataManagerOfCache then
      DoStatus(TObjectDataManagerOfCache(db).CacheStatus);

  if not db.ItemCreate('/a/b/c', '3', '', itmHnd3) then
      DoStatus('create item error');
  buff := umlBytesOf('3333');
  db.ItemWrite(itmHnd3, length(buff), buff[0]);
  DoStatus('item3 size:%d', [itmHnd3.Item.Size]);

  if db is TObjectDataManagerOfCache then
      DoStatus(TObjectDataManagerOfCache(db).CacheStatus);

  if not db.ItemCreate('/a/b/c', '4', '', itmHnd4) then
      DoStatus('create item error');
  buff := umlBytesOf('44444444');
  db.ItemWrite(itmHnd4, length(buff), buff[0]);
  DoStatus('item4 size:%d', [itmHnd4.Item.Size]);

  if db is TObjectDataManagerOfCache then
      DoStatus(TObjectDataManagerOfCache(db).CacheStatus);

  buff := umlBytesOf('t12345');
  db.ItemWrite(itmHnd1, length(buff), buff[0]);
  db.ItemSeekStart(itmHnd1);
  SetLength(buff, itmHnd1.Item.Size);
  db.ItemRead(itmHnd1, length(buff), buff[0]);
  if umlStringOf(buff).Same('1111t12345') then
      DoStatus('test fragment buffer ok!');
  DoStatus('item1 size:%d', [itmHnd1.Item.Size]);

  db.ItemClose(itmHnd1);
  db.ItemClose(itmHnd2);
  db.ItemClose(itmHnd3);
  db.ItemClose(itmHnd4);

  if db is TObjectDataManagerOfCache then
      DoStatus(TObjectDataManagerOfCache(db).CacheStatus);

  if not db.GetPathFieldHeaderNames('/a/b/c', nameL) then
      DoStatus('get field list error');

  if db is TObjectDataManagerOfCache then
      DoStatus(TObjectDataManagerOfCache(db).CacheStatus);

  if db.GetItemSize('/a/b/c', nameL[0]) <> 10 then
      DoStatus('item body error');
  if db.GetItemSize('/a/b/c', nameL[1]) <> 5 then
      DoStatus('item body error');
  if db.GetItemSize('/a/b/c', nameL[2]) <> 4 then
      DoStatus('item body error');
  if db.GetItemSize('/a/b/c', nameL[3]) <> 8 then
      DoStatus('item body error');

  if db is TObjectDataManagerOfCache then
      DoStatus(TObjectDataManagerOfCache(db).CacheStatus);

  DoStatus('DB item delete test');

  db.ItemDelete('/a/b/c', '3');
  if not db.GetPathFieldHeaderNames('/a/b/c', nameL) then
      DoStatus('get field list error');

  if db.GetItemSize('/a/b/c', nameL[0]) <> 10 then
      DoStatus('item body error');
  if db.GetItemSize('/a/b/c', nameL[1]) <> 5 then
      DoStatus('item body error');
  if db.GetItemSize('/a/b/c', nameL[2]) <> 8 then
      DoStatus('item body error');

  db.ItemDelete('/a/b/c', '1');
  if not db.GetPathFieldHeaderNames('/a/b/c', nameL) then
      DoStatus('get field list error');

  if db.GetItemSize('/a/b/c', nameL[0]) <> 5 then
      DoStatus('item body error');
  if db.GetItemSize('/a/b/c', nameL[1]) <> 8 then
      DoStatus('item body error');

  db.ItemDelete('/a/b/c', '2');
  if not db.GetPathFieldHeaderNames('/a/b/c', nameL) then
      DoStatus('get field list error');

  if db.GetItemSize('/a/b/c', nameL[0]) <> 8 then
      DoStatus('item body error');

  db.ItemDelete('/a/b/c', '4');
  if not db.GetPathFieldHeaderNames('/a/b/c', nameL) then
      DoStatus('get field list error');

  if length(nameL) = 0 then
      DoStatus('delete test done!');

  if db is TObjectDataManagerOfCache then
      DoStatus(TObjectDataManagerOfCache(db).CacheStatus);

  DoStatus('DB field delete test');

  if not db.GetPathFieldHeaderNames('/a/b', nameL) then
      DoStatus('get field list error');
  if (length(nameL) <> 1) then
      DoStatus('get field list error')
  else if nameL[0] = 'c' then
      DoStatus('test field delete ok');

  db.FieldDelete('/a/b', 'c');
  if not db.GetPathFieldHeaderNames('/a/b', nameL) then
      DoStatus('get field list error');
  if db.GetPathFieldHeaderCount('/a/b') <> 0 then
      DoStatus('delete field error');

  db.FieldDelete('/a', 'b');
  if db.GetPathFieldHeaderCount('/a/b') <> 0 then
      DoStatus('delete field error');
  db.FieldDelete('/', 'a');
  if db.GetPathFieldHeaderCount('/') <> 0 then
      DoStatus('delete field error');

  if length(nameL) = 0 then
      DoStatus('field delete test done!');

  if db is TObjectDataManagerOfCache then
      DoStatus(TObjectDataManagerOfCache(db).CacheStatus);

  DisposeObject(db);
end;

procedure TestObjectData();
begin
  TestObjectData_(TObjectDataManager);
  TestObjectData_(TObjectDataManagerOfCache);
end;

initialization

ObjectDataMarshal();

finalization

if Internal_ObjectDataMarshal <> nil then
  begin
    DisposeObject(Internal_ObjectDataMarshal);
    Internal_ObjectDataMarshal := nil;
  end;

end.
