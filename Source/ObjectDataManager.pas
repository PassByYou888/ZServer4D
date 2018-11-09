{ ****************************************************************************** }
{ * Object Data Manager, base on ObjectData, write by qq600585                 * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }
(*
  update history
*)

unit ObjectDataManager;

{$INCLUDE zDefine.inc}

interface

uses CoreClasses, ObjectData, UnicodeMixedLib, PascalStrings, ListEngine;

type
  TItemHandle = TTMDBItemHandle;
  PItemHandle = ^TItemHandle;
  PFieldHandle = ^TFieldHandle;
  TFieldHandle = TField;
  PItemSearch = ^TItemSearch;
  TItemSearch = TTMDBSearchItem;
  PFieldSearch = ^TFieldSearch;
  TFieldSearch = TTMDBSearchField;
  PItemRecursionSearch = ^TItemRecursionSearch;
  TItemRecursionSearch = TTMDBRecursionSearch;

  TObjectDataManager = class(TCoreClassObject)
  private
    FStreamEngine: TCoreClassStream;
    FObjectDataHandle: TTMDB;
    FNeedCreateNew, FOverWriteItem, FAllowSameHeaderName, FOnlyRead: Boolean;
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
  public
    constructor Create(const aObjectName: SystemString; const AID: Byte; aOnlyRead: Boolean);
    constructor CreateNew(const aObjectName: SystemString; const AID: Byte);
    constructor CreateAsStream(AStream: TCoreClassStream; const aObjectName: SystemString; const AID: Byte; aOnlyRead, aIsNewData, aAutoFreeHandle: Boolean);
    destructor Destroy; override;
    function Open: Boolean;
    function NewHandle(AStream: TCoreClassStream; const aObjectName: SystemString; const AID: Byte; aOnlyRead, aIsNew: Boolean): Boolean;
    function copyto(DestDB: TObjectDataManager): Boolean;
    function CopyToPath(DestDB: TObjectDataManager; destPath: SystemString): Boolean;
    function CopyFieldToPath(FieldPos: Int64; DestDB: TObjectDataManager; destPath: SystemString): Boolean;
    procedure SaveToStream(stream: TCoreClassStream);
    procedure ImpFromPath(ImpPath, DBPath: SystemString; IncludeSub: Boolean);
    procedure ImpFromFiles(ImpFiles: TCoreClassStrings; DBPath: SystemString);
    function isAbort: Boolean;
    function Close: Boolean;
    function ErrorNo: Int64;
    function Modify: Boolean;
    function Size: Int64;
    function IOReadSize: Int64;
    function IOWriteSize: Int64;
    procedure SetID(const ID: Byte);
    procedure Update;

    // field
    function CreateField(const DirName, DirDescription: SystemString): Boolean;
    function CreateRootField(const RootName: SystemString): Boolean;
    function DirectoryExists(const DirName: SystemString): Boolean;
    function FastDelete(const FieldPos: Int64; const APos: Int64): Boolean;
    function FastFieldExists(const FieldPos: Int64; const FieldName: SystemString): Boolean;
    function FastFieldCreate(const FieldPos: Int64; const FieldName, FieldDescription: SystemString; var NewFieldPos: Int64): Boolean;
    function SetRootField(const RootName: SystemString): Boolean;
    function FieldReName(const FieldPos: Int64; const NewFieldName, NewFieldDescription: SystemString): Boolean;
    function RootField: Int64;
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
    function GetHeaderLastModifyTime(const hPos: Int64): TDateTime;
    function GetItemSize(const DBPath, DBItem: SystemString): Int64;

    // fast api support
    function GetFirstHeaderFromField(FieldPos: Int64; var h: THeader): Boolean;
    function GetLastHeaderFromField(FieldPos: Int64; var h: THeader): Boolean;
    function GetHeader(hPos: Int64; var h: THeader): Boolean;
    function ItemAutoOpenOrCreate(const DBPath, DBItem, DBItemDescription: SystemString; var ItemHnd: TItemHandle): Boolean;
    function ItemUpdate(var ItemHnd: TItemHandle): Boolean;
    function ItemClose(var ItemHnd: TItemHandle): Boolean;
    function ItemReName(const FieldPos: Int64; var ItemHnd: TItemHandle; const aNewName, aNewDescription: SystemString): Boolean;
    function ItemCopyTo(var ItemHnd: TItemHandle; DestDB: TObjectDataManager; var DestItemHandle: TItemHandle; const CopySize: Int64): Boolean;
    function ItemCreate(const DBPath, DBItem, DBItemDescription: SystemString; var ItemHnd: TItemHandle): Boolean;
    function ItemDelete(const DBPath, DBItem: SystemString): Boolean;
    function ItemExists(const DBPath, DBItem: SystemString): Boolean;
    function ItemFastInsertNew(const FieldPos, InsertHeaderPos: Int64; const DBItem, DBItemDescription: SystemString; var ItemHnd: TItemHandle): Boolean;
    function ItemFastCreate(const APos: Int64; const DBItem, DBItemDescription: SystemString; var ItemHnd: TItemHandle): Boolean;
    function ItemFastExists(const FieldPos: Int64; const DBItem: SystemString): Boolean;
    function ItemFastFindFirst(const FieldPos: Int64; const DBItem: SystemString; var ItemSearchHandle: TItemSearch): Boolean;
    function ItemFastFindLast(const FieldPos: Int64; const DBItem: SystemString; var ItemSearchHandle: TItemSearch): Boolean;
    function ItemFastFindNext(var ItemSearchHandle: TItemSearch): Boolean;
    function ItemFastFindPrev(var ItemSearchHandle: TItemSearch): Boolean;
    function ItemFastOpen(const APos: Int64; var ItemHnd: TItemHandle): Boolean;
    function ItemFastResetBody(const APos: Int64): Boolean;
    function ItemFindFirst(const DBPath, DBItem: SystemString; var ItemSearchHandle: TItemSearch): Boolean;
    function ItemFindLast(const DBPath, DBItem: SystemString; var ItemSearchHandle: TItemSearch): Boolean;
    function ItemFindNext(var ItemSearchHandle: TItemSearch): Boolean;
    function ItemFindPrev(var ItemSearchHandle: TItemSearch): Boolean;
    function ItemMove(const DBPath, ItemName, destPath: SystemString): Boolean;
    function ItemOpen(const DBPath, DBItem: SystemString; var ItemHnd: TItemHandle): Boolean;

    // block operation
    function ItemRead(var ItemHnd: TItemHandle; const ASize: Int64; var Buffers): Boolean;
    function ItemSeekStart(var ItemHnd: TItemHandle): Boolean;
    function ItemSeekLast(var ItemHnd: TItemHandle): Boolean;
    function ItemSeek(var ItemHnd: TItemHandle; const ItemOffset: Int64): Boolean;
    function ItemGetPos(var ItemHnd: TItemHandle): Int64;
    function ItemGetSize(var ItemHnd: TItemHandle): Int64;
    function ItemWrite(var ItemHnd: TItemHandle; const ASize: Int64; var Buffers): Boolean;

    // recursion support
    function RecursionSearchFirst(const InitPath, MaskName: SystemString; var RecursionSearchHnd: TItemRecursionSearch): Boolean;
    function RecursionSearchNext(var RecursionSearchHnd: TItemRecursionSearch): Boolean;

    function ObjectDataHandlePtr: PTMDB;
    property AutoFreeHandle: Boolean read GetAutoFreeHandle write SetAutoFreeHandle;
    property IsOnlyRead: Boolean read FOnlyRead;
    property NeedCreateNew: Boolean read FNeedCreateNew;
    property ObjectName: SystemString read FObjectName write FObjectName;
    property DefaultItemID: Byte read FDefaultItemID;
    property StreamEngine: TCoreClassStream read FStreamEngine;
    property DBTime: TDateTime read GetDBTime;
    property OverWriteItem: Boolean read GetOverWriteItem write SetOverWriteItem;
    property AllowSameHeaderName: Boolean read GetAllowSameHeaderName write SetAllowSameHeaderName;
    // user custom data
    property Data: Pointer read FData write FData;
  end;

  TObjectDataManagerOfCache = class(TObjectDataManager)
  private type
    PObjectDataCacheHeader = PHeader;
    PObjectDataCacheItemBlock = PItemBlock;

    PObjectDataCacheItem = ^TObjectDataCacheItem;

    TObjectDataCacheItem = record
      Description: U_String;
      ExtID: Byte;
      FirstBlockPOS: Int64;
      LastBlockPOS: Int64;
      Size: Int64;
      BlockCount: Int64;
      CurrentBlockSeekPOS: Int64;
      CurrentFileSeekPOS: Int64;
      DataWrited: Boolean;
      Return: Integer;
      MemorySiz: nativeUInt;
      procedure write(var wVal: TItem);
      procedure read(var rVal: TItem);
    end;

    PObjectDataCacheField = ^TObjectDataCacheField;

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
  private
    FHeaderCache, FItemBlockCache, FItemCache, FFieldCache: TInt64HashPointerList;

    procedure HeaderCache_AddDataProc(p: Pointer);
    procedure ItemBlockCache_AddDataProc(p: Pointer);
    procedure ItemCache_AddDataProc(p: Pointer);
    procedure FieldCache_AddDataProc(p: Pointer);

    procedure HeaderCache_DataFreeProc(p: Pointer);
    procedure ItemBlockCache_DataFreeProc(p: Pointer);
    procedure ItemCache_DataFreeProc(p: Pointer);
    procedure FieldCache_DataFreeProc(p: Pointer);

    procedure HeaderWriteProc(fPos: Int64; var wVal: THeader);
    procedure HeaderReadProc(fPos: Int64; var rVal: THeader; var Done: Boolean);
    procedure ItemBlockWriteProc(fPos: Int64; var wVal: TItemBlock);
    procedure ItemBlockReadProc(fPos: Int64; var rVal: TItemBlock; var Done: Boolean);
    procedure ItemWriteProc(fPos: Int64; var wVal: TItem);
    procedure ItemReadProc(fPos: Int64; var rVal: TItem; var Done: Boolean);
    procedure OnlyItemRecWriteProc(fPos: Int64; var wVal: TItem);
    procedure OnlyItemRecReadProc(fPos: Int64; var rVal: TItem; var Done: Boolean);
    procedure FieldWriteProc(fPos: Int64; var wVal: TField);
    procedure FieldReadProc(fPos: Int64; var rVal: TField; var Done: Boolean);
    procedure OnlyFieldRecWriteProc(fPos: Int64; var wVal: TField);
    procedure OnlyFieldRecReadProc(fPos: Int64; var rVal: TField; var Done: Boolean);

    procedure DoCreateFinish; override;
  public
    destructor Destroy; override;
    procedure BuildDBCacheIntf;
    procedure FreeDBCacheIntf;
    procedure CleaupCache;

    property HeaderCache: TInt64HashPointerList read FHeaderCache;
    property ItemBlockCache: TInt64HashPointerList read FItemBlockCache;
    property ItemCache: TInt64HashPointerList read FItemCache;
    property FieldCache: TInt64HashPointerList read FFieldCache;
  end;

  TObjectDataMarshal = class(TCoreClassObject)
  private
    FID: Byte;
    FLibList: TCoreClassStrings;
    FUseWildcard: Boolean;
    function GetItems(aIndex: Integer): TObjectDataManager;
    function GetNames(AName: SystemString): TObjectDataManager;
    procedure SetItems(aIndex: Integer; const Value: TObjectDataManager);
  public
    constructor Create(AID: Byte);
    destructor Destroy; override;
    function GetAbsoluteFileName(aFileName: SystemString): SystemString;
    function NewDB(aFile: SystemString; aOnlyRead: Boolean): TObjectDataManager;
    function Open(aFile: SystemString; aOnlyRead: Boolean): TObjectDataManager;
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

implementation

uses ItemStream, Types;

const
  MaxBuffSize = 65535;
  UserRootName = 'User';

var
  I_ObjectDataMarshal: TObjectDataMarshal = nil;

function ObjectDataMarshal: TObjectDataMarshal;
begin
  if I_ObjectDataMarshal = nil then
      I_ObjectDataMarshal := TObjectDataMarshal.Create(0);
  Result := I_ObjectDataMarshal;
end;

function TObjectDataManager.GetAutoFreeHandle: Boolean;
begin
  if not isAbort then
      Result := FObjectDataHandle.IOHnd.AutoFree
  else
      Result := False;
end;

procedure TObjectDataManager.SetAutoFreeHandle(const Value: Boolean);
begin
  if not isAbort then
      FObjectDataHandle.IOHnd.AutoFree := Value;
end;

procedure TObjectDataManager.DoCreateFinish;
begin
end;

function TObjectDataManager.GetOverWriteItem: Boolean;
begin
  Result := FObjectDataHandle.OverWriteItem;
end;

function TObjectDataManager.GetAllowSameHeaderName: Boolean;
begin
  Result := FObjectDataHandle.AllowSameHeaderName;
end;

function TObjectDataManager.GetDBTime: TDateTime;
begin
  Result := FObjectDataHandle.CreateTime;
end;

procedure TObjectDataManager.SetOverWriteItem(Value: Boolean);
begin
  FObjectDataHandle.OverWriteItem := Value;
  FOverWriteItem := Value;
end;

procedure TObjectDataManager.SetAllowSameHeaderName(Value: Boolean);
begin
  FObjectDataHandle.AllowSameHeaderName := Value;
  FAllowSameHeaderName := Value;
end;

constructor TObjectDataManager.Create(const aObjectName: SystemString; const AID: Byte; aOnlyRead: Boolean);
begin
  inherited Create;
  NewHandle(nil, aObjectName, AID, aOnlyRead, False);
  DoCreateFinish;
end;

constructor TObjectDataManager.CreateNew(const aObjectName: SystemString; const AID: Byte);
begin
  inherited Create;
  NewHandle(nil, aObjectName, AID, False, True);
  DoCreateFinish;
end;

constructor TObjectDataManager.CreateAsStream(AStream: TCoreClassStream; const aObjectName: SystemString; const AID: Byte; aOnlyRead, aIsNewData, aAutoFreeHandle: Boolean);
begin
  inherited Create;
  NewHandle(AStream, aObjectName, AID, aOnlyRead, aIsNewData);
  AutoFreeHandle := aAutoFreeHandle;
  DoCreateFinish;
end;

destructor TObjectDataManager.Destroy;
begin
  Close;
  inherited Destroy;
end;

function TObjectDataManager.Open: Boolean;
begin
  Result := False;
  if StreamEngine <> nil then
    begin
      if FNeedCreateNew then
        begin
          if not dbPack_CreateAsStream(StreamEngine, ObjectName, '', FObjectDataHandle) then
            begin
              Exit;
            end;
          if not(dbPack_CreateRootField(UserRootName, '', FObjectDataHandle)) then
              Exit;
          if not(dbPack_SetCurrentRootField(UserRootName, FObjectDataHandle)) then
              Exit;
        end
      else
        begin
          if not dbPack_OpenAsStream(StreamEngine, ObjectName, FObjectDataHandle, IsOnlyRead) then
            begin
              Exit;
            end;
        end;
    end
  else if (FNeedCreateNew) or (not umlFileExists(ObjectName)) then
    begin
      if not dbPack_CreatePack(ObjectName, '', FObjectDataHandle) then
        begin
          dbPack_ClosePack(FObjectDataHandle);
          Init_TTMDB(FObjectDataHandle);
          if not dbPack_CreatePack(ObjectName, '', FObjectDataHandle) then
            begin
              Exit;
            end;
        end;
      if not(dbPack_CreateRootField(UserRootName, '', FObjectDataHandle)) then
          Exit;
      if not(dbPack_SetCurrentRootField(UserRootName, FObjectDataHandle)) then
          Exit;
    end
  else if not dbPack_OpenPack(ObjectName, FObjectDataHandle, IsOnlyRead) then
    begin
      dbPack_ClosePack(FObjectDataHandle);
      Init_TTMDB(FObjectDataHandle);
      if not dbPack_OpenPack(ObjectName, FObjectDataHandle, IsOnlyRead) then
        begin
          Exit;
        end;
    end;
  FObjectDataHandle.OverWriteItem := FOverWriteItem;
  FObjectDataHandle.AllowSameHeaderName := FAllowSameHeaderName;
  Result := True;
end;

function TObjectDataManager.NewHandle(AStream: TCoreClassStream; const aObjectName: SystemString; const AID: Byte; aOnlyRead, aIsNew: Boolean): Boolean;
begin
  Close;
  Init_TTMDB(FObjectDataHandle);
  FStreamEngine := AStream;
  FObjectName := aObjectName;
  FNeedCreateNew := aIsNew;
  FOnlyRead := aOnlyRead;
  FDefaultItemID := AID;
  FIsOpened := Open();
  Result := FIsOpened;

  OverWriteItem := True;
  AllowSameHeaderName := False;
  AutoFreeHandle := True;
  FData := nil;
end;

function TObjectDataManager.copyto(DestDB: TObjectDataManager): Boolean;
begin
  Result := dbPack_CopyAllTo(FObjectDataHandle, DestDB.FObjectDataHandle);
end;

function TObjectDataManager.CopyToPath(DestDB: TObjectDataManager; destPath: SystemString): Boolean;
begin
  Result := dbPack_CopyAllToDestPath(FObjectDataHandle, DestDB.FObjectDataHandle, destPath);
end;

function TObjectDataManager.CopyFieldToPath(FieldPos: Int64; DestDB: TObjectDataManager; destPath: SystemString): Boolean;
var
  DestFieldPos: Int64;
begin
  Result := False;
  CreateField(destPath, '');
  if GetPathField(destPath, DestFieldPos) then
      Result := dbPack_CopyFieldTo('*', FObjectDataHandle, FieldPos, DestDB.FObjectDataHandle, DestFieldPos);
end;

procedure TObjectDataManager.SaveToStream(stream: TCoreClassStream);
var
  E: TObjectDataManager;
begin
  E := TObjectDataManager.CreateAsStream(stream, ObjectName, DefaultItemID, False, True, False);
  copyto(E);
  DisposeObject(E);
end;

procedure TObjectDataManager.ImpFromPath(ImpPath, DBPath: SystemString; IncludeSub: Boolean);
var
  fAry: U_StringArray;
  n: SystemString;
  fPos: Int64;
  fs: TCoreClassFileStream;
  itmHnd: TItemHandle;
  itmStream: TItemStreamEngine;
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
      itmStream := TItemStreamEngine.Create(Self, itmHnd);
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
  itmStream: TItemStreamEngine;
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
      itmStream := TItemStreamEngine.Create(Self, itmHnd);
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
  Result := dbPack_ClosePack(FObjectDataHandle);
end;

function TObjectDataManager.ErrorNo: Int64;
begin
  Result := FObjectDataHandle.Return;
end;

function TObjectDataManager.Modify: Boolean;
begin
  Result := FObjectDataHandle.IOHnd.WriteFlag;
end;

function TObjectDataManager.Size: Int64;
begin
  Result := FObjectDataHandle.IOHnd.Size;
end;

function TObjectDataManager.IOReadSize: Int64;
begin
  Result := FObjectDataHandle.IOHnd.IORead;
end;

function TObjectDataManager.IOWriteSize: Int64;
begin
  Result := FObjectDataHandle.IOHnd.IOWrite;
end;

procedure TObjectDataManager.SetID(const ID: Byte);
begin
  FDefaultItemID := ID;
end;

procedure TObjectDataManager.Update;
begin
  dbPack_Update(FObjectDataHandle);
end;

function TObjectDataManager.CreateField(const DirName, DirDescription: SystemString): Boolean;
begin
  Result := dbPack_CreateField(DirName, DirDescription, FObjectDataHandle);
end;

function TObjectDataManager.CreateRootField(const RootName: SystemString): Boolean;
begin
  Result := dbPack_CreateRootField(RootName, RootName, FObjectDataHandle);
end;

function TObjectDataManager.DirectoryExists(const DirName: SystemString): Boolean;
var
  Field: TFieldHandle;
begin
  Result := dbPack_GetField(DirName, Field, FObjectDataHandle);
end;

function TObjectDataManager.FastDelete(const FieldPos: Int64; const APos: Int64): Boolean;
var
  FieldHnd: TFieldHandle;
begin
  Init_TField(FieldHnd);
  Result := False;
  if dbField_ReadRec(FieldPos, FObjectDataHandle.IOHnd, FieldHnd) then
      Result := dbField_DeleteHeader(APos, FieldPos, FObjectDataHandle.IOHnd, FieldHnd);
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
  Result := dbField_CreateField(FieldName, FieldPos, FObjectDataHandle.IOHnd, NewField);
  NewFieldPos := NewField.RHeader.CurrentHeader;
end;

function TObjectDataManager.SetRootField(const RootName: SystemString): Boolean;
begin
  Result := dbPack_SetCurrentRootField(RootName, FObjectDataHandle);
end;

function TObjectDataManager.FieldReName(const FieldPos: Int64; const NewFieldName, NewFieldDescription: SystemString): Boolean;
var
  FieldHnd: TFieldHandle;
begin
  Result := False;
  if not umlExistsLimitChar(NewFieldName, '\/') then
    begin
      Init_TField(FieldHnd);
      if dbField_ReadRec(FieldPos, FObjectDataHandle.IOHnd, FieldHnd) then
        begin
          if (not FastFieldExists(FieldHnd.UpLevelFieldPOS, NewFieldName)) and (FieldHnd.RHeader.CurrentHeader <> FObjectDataHandle.DefaultFieldPOS) then
            begin
              FieldHnd.RHeader.Name := NewFieldName;
              FieldHnd.Description := NewFieldDescription;
              Result := dbField_WriteRec(FieldPos, FObjectDataHandle.IOHnd, FieldHnd);
            end;
        end;
    end;
end;

function TObjectDataManager.RootField: Int64;
begin
  Result := FObjectDataHandle.DefaultFieldPOS;
end;

function TObjectDataManager.FieldDelete(const DBPath: SystemString; const FieldName: SystemString): Boolean;
begin
  Result := dbPack_DeleteField(DBPath, FieldName, FObjectDataHandle);
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
  Result := dbPack_FastFindFirstField(FieldPos, Filter, FieldSearchHandle, FObjectDataHandle);
end;

function TObjectDataManager.FieldFastFindLast(const FieldPos: Int64; const Filter: SystemString; var FieldSearchHandle: TFieldSearch): Boolean;
begin
  Init_TTMDBSearchField(FieldSearchHandle);
  Result := dbPack_FastFindLastField(FieldPos, Filter, FieldSearchHandle, FObjectDataHandle);
end;

function TObjectDataManager.FieldFastFindNext(var FieldSearchHandle: TFieldSearch): Boolean;
begin
  Result := dbPack_FastFindNextField(FieldSearchHandle, FObjectDataHandle);
end;

function TObjectDataManager.FieldFastFindPrev(var FieldSearchHandle: TFieldSearch): Boolean;
begin
  Result := dbPack_FastFindPrevField(FieldSearchHandle, FObjectDataHandle);
end;

function TObjectDataManager.FieldFindFirst(const DBPath, Filter: SystemString; var FieldSearchHandle: TFieldSearch): Boolean;
begin
  Init_TTMDBSearchField(FieldSearchHandle);
  Result := dbPack_FindFirstField(DBPath, Filter, FieldSearchHandle, FObjectDataHandle);
end;

function TObjectDataManager.FieldFindLast(const DBPath, Filter: SystemString; var FieldSearchHandle: TFieldSearch): Boolean;
begin
  Init_TTMDBSearchField(FieldSearchHandle);
  Result := dbPack_FindLastField(DBPath, Filter, FieldSearchHandle, FObjectDataHandle);
end;

function TObjectDataManager.FieldFindNext(var FieldSearchHandle: TFieldSearch): Boolean;
begin
  Result := dbPack_FindNextField(FieldSearchHandle, FObjectDataHandle);
end;

function TObjectDataManager.FieldFindPrev(var FieldSearchHandle: TFieldSearch): Boolean;
begin
  Result := dbPack_FindPrevField(FieldSearchHandle, FObjectDataHandle);
end;

function TObjectDataManager.FieldMove(const DBPath, FieldName, destPath: SystemString): Boolean;
begin
  Result := dbPack_MoveField(DBPath, FieldName, destPath, FObjectDataHandle);
end;

function TObjectDataManager.GetFieldData(const FieldPos: Int64; var dest: TFieldHandle): Boolean;
begin
  Init_TField(dest);
  Result := dbField_ReadRec(FieldPos, FObjectDataHandle.IOHnd, dest);
end;

function TObjectDataManager.GetFieldPath(const FieldPos: Int64): SystemString;
var
  ReturnPath: U_String;
begin
  if dbPack_GetPath(FieldPos, FObjectDataHandle.DefaultFieldPOS, FObjectDataHandle, ReturnPath) then
      Result := ReturnPath
  else
      Result := '';
end;

function TObjectDataManager.GetPathField(const DBPath: SystemString; var dest: Int64): Boolean;
var
  FieldHnd: TFieldHandle;
begin
  Result := dbPack_GetField(DBPath, FieldHnd, FObjectDataHandle);
  if Result then
      dest := FieldHnd.RHeader.CurrentHeader;
end;

function TObjectDataManager.GetPathFieldPos(const DBPath: SystemString): Int64;
begin
  if not GetPathField(DBPath, Result) then
      Result := 0;
end;

function TObjectDataManager.GetHeaderLastModifyTime(const hPos: Int64): TDateTime;
var
  h: THeader;
begin
  if dbHeader_ReadRec(hPos, FObjectDataHandle.IOHnd, h) then
      Result := h.LastModifyTime
  else
      Result := umlDefaultTime;
end;

function TObjectDataManager.GetItemSize(const DBPath, DBItem: SystemString): Int64;
var
  DBItemHandle: TItemHandle;
begin
  Init_TTMDBItemHandle(DBItemHandle);
  if dbPack_GetItem(DBPath, DBItem, FDefaultItemID, DBItemHandle.Item, FObjectDataHandle) then
      Result := DBItemHandle.Item.Size
  else
      Result := 0;
end;

function TObjectDataManager.GetFirstHeaderFromField(FieldPos: Int64; var h: THeader): Boolean;
var
  f: TField;
begin
  Result := (dbField_ReadRec(FieldPos, FObjectDataHandle.IOHnd, f)) and (f.HeaderCount > 0);
  if Result then
    begin
      Result := GetHeader(f.FirstHeaderPOS, h);
    end;
end;

function TObjectDataManager.GetLastHeaderFromField(FieldPos: Int64; var h: THeader): Boolean;
var
  f: TField;
begin
  Result := (dbField_ReadRec(FieldPos, FObjectDataHandle.IOHnd, f)) and (f.HeaderCount > 0);
  if Result then
      Result := GetHeader(f.LastHeaderPOS, h);
end;

function TObjectDataManager.GetHeader(hPos: Int64; var h: THeader): Boolean;
begin
  Result := dbHeader_ReadRec(hPos, FObjectDataHandle.IOHnd, h);
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
  Result := dbPack_ItemUpdate(ItemHnd, FObjectDataHandle);
end;

function TObjectDataManager.ItemClose(var ItemHnd: TItemHandle): Boolean;
begin
  Result := dbPack_ItemClose(ItemHnd, FObjectDataHandle);
end;

function TObjectDataManager.ItemReName(const FieldPos: Int64; var ItemHnd: TItemHandle; const aNewName, aNewDescription: SystemString): Boolean;
begin
  Result := dbPack_ItemReName(FieldPos, aNewName, aNewDescription, ItemHnd, FObjectDataHandle);
end;

function TObjectDataManager.ItemCopyTo(var ItemHnd: TItemHandle; DestDB: TObjectDataManager; var DestItemHandle: TItemHandle; const CopySize: Int64): Boolean;
var
  RepaInt: Integer;
  buff: array [0 .. MaxBuffSize] of Byte;
begin
  Result := False;
  if CopySize > MaxBuffSize then
    begin
      for RepaInt := 1 to (CopySize div MaxBuffSize) do
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

function TObjectDataManager.ItemCreate(const DBPath, DBItem, DBItemDescription: SystemString; var ItemHnd: TItemHandle): Boolean;
{
  It can automatically create a path
}
begin
  Init_TTMDBItemHandle(ItemHnd);
  Result := dbPack_ItemCreate(DBPath, DBItem, DBItemDescription, FDefaultItemID, ItemHnd, FObjectDataHandle);
end;

function TObjectDataManager.ItemDelete(const DBPath, DBItem: SystemString): Boolean;
begin
  Result := dbPack_DeleteItem(DBPath, DBItem, FDefaultItemID, FObjectDataHandle);
end;

function TObjectDataManager.ItemExists(const DBPath, DBItem: SystemString): Boolean;
var
  ItemSearchHnd: TItemSearch;
begin
  Init_TTMDBSearchItem(ItemSearchHnd);
  Result := dbPack_FindFirstItem(DBPath, DBItem, FDefaultItemID, ItemSearchHnd, FObjectDataHandle);
end;

function TObjectDataManager.ItemFastInsertNew(const FieldPos, InsertHeaderPos: Int64; const DBItem, DBItemDescription: SystemString; var ItemHnd: TItemHandle): Boolean;
begin
  Init_TTMDBItemHandle(ItemHnd);
  Result := dbPack_ItemFastInsertNew(DBItem, DBItemDescription, FieldPos, InsertHeaderPos, FDefaultItemID, ItemHnd, FObjectDataHandle);
end;

function TObjectDataManager.ItemFastCreate(const APos: Int64; const DBItem, DBItemDescription: SystemString; var ItemHnd: TItemHandle): Boolean;
begin
  Init_TTMDBItemHandle(ItemHnd);
  Result := dbPack_ItemFastCreate(DBItem, DBItemDescription, APos, FDefaultItemID, ItemHnd, FObjectDataHandle);
end;

function TObjectDataManager.ItemFastExists(const FieldPos: Int64; const DBItem: SystemString): Boolean;
var
  ItemSearchHnd: TItemSearch;
begin
  Init_TTMDBSearchItem(ItemSearchHnd);
  Result := dbPack_FastFindFirstItem(FieldPos, DBItem, FDefaultItemID, ItemSearchHnd, FObjectDataHandle);
end;

function TObjectDataManager.ItemFastFindFirst(const FieldPos: Int64; const DBItem: SystemString; var ItemSearchHandle: TItemSearch): Boolean;
begin
  Init_TTMDBSearchItem(ItemSearchHandle);
  Result := dbPack_FastFindFirstItem(FieldPos, DBItem, FDefaultItemID, ItemSearchHandle, FObjectDataHandle);
end;

function TObjectDataManager.ItemFastFindLast(const FieldPos: Int64; const DBItem: SystemString; var ItemSearchHandle: TItemSearch): Boolean;
begin
  Init_TTMDBSearchItem(ItemSearchHandle);
  Result := dbPack_FastFindLastItem(FieldPos, DBItem, FDefaultItemID, ItemSearchHandle, FObjectDataHandle);
end;

function TObjectDataManager.ItemFastFindNext(var ItemSearchHandle: TItemSearch): Boolean;
begin
  Result := dbPack_FastFindNextItem(ItemSearchHandle, FDefaultItemID, FObjectDataHandle);
end;

function TObjectDataManager.ItemFastFindPrev(var ItemSearchHandle: TItemSearch): Boolean;
begin
  Result := dbPack_FastFindPrevItem(ItemSearchHandle, FDefaultItemID, FObjectDataHandle);
end;

function TObjectDataManager.ItemFastOpen(const APos: Int64; var ItemHnd: TItemHandle): Boolean;
begin
  Init_TTMDBItemHandle(ItemHnd);
  Result := dbPack_ItemFastOpen(APos, FDefaultItemID, ItemHnd, FObjectDataHandle);
end;

function TObjectDataManager.ItemFastResetBody(const APos: Int64): Boolean;
var
  ItemHnd: TItemHandle;
begin
  Result := ItemFastOpen(APos, ItemHnd)
    and dbPack_ItemBodyReset(ItemHnd, FObjectDataHandle);
end;

function TObjectDataManager.ItemFindFirst(const DBPath, DBItem: SystemString; var ItemSearchHandle: TItemSearch): Boolean;
begin
  Init_TTMDBSearchItem(ItemSearchHandle);
  Result := dbPack_FindFirstItem(DBPath, DBItem, FDefaultItemID, ItemSearchHandle, FObjectDataHandle);
end;

function TObjectDataManager.ItemFindLast(const DBPath, DBItem: SystemString; var ItemSearchHandle: TItemSearch): Boolean;
begin
  Init_TTMDBSearchItem(ItemSearchHandle);
  Result := dbPack_FindLastItem(DBPath, DBItem, FDefaultItemID, ItemSearchHandle, FObjectDataHandle);
end;

function TObjectDataManager.ItemFindNext(var ItemSearchHandle: TItemSearch): Boolean;
begin
  Result := dbPack_FindNextItem(ItemSearchHandle, FDefaultItemID, FObjectDataHandle);
end;

function TObjectDataManager.ItemFindPrev(var ItemSearchHandle: TItemSearch): Boolean;
begin
  Result := dbPack_FindPrevItem(ItemSearchHandle, FDefaultItemID, FObjectDataHandle);
end;

function TObjectDataManager.ItemMove(const DBPath, ItemName, destPath: SystemString): Boolean;
begin
  Result := dbPack_MoveItem(DBPath, ItemName, destPath, FDefaultItemID, FObjectDataHandle);
end;

function TObjectDataManager.ItemOpen(const DBPath, DBItem: SystemString; var ItemHnd: TItemHandle): Boolean;
begin
  Init_TTMDBItemHandle(ItemHnd);
  Result := dbPack_ItemOpen(DBPath, DBItem, FDefaultItemID, ItemHnd, FObjectDataHandle);
end;

function TObjectDataManager.ItemRead(var ItemHnd: TItemHandle; const ASize: Int64; var Buffers): Boolean;
begin
  Result := dbPack_ItemRead(ASize, Buffers, ItemHnd, FObjectDataHandle);
end;

function TObjectDataManager.ItemSeekStart(var ItemHnd: TItemHandle): Boolean;
begin
  Result := dbPack_ItemSeekStartPos(ItemHnd, FObjectDataHandle);
end;

function TObjectDataManager.ItemSeekLast(var ItemHnd: TItemHandle): Boolean;
begin
  Result := dbPack_ItemSeekLastPos(ItemHnd, FObjectDataHandle);
end;

function TObjectDataManager.ItemSeek(var ItemHnd: TItemHandle; const ItemOffset: Int64): Boolean;
var
  ASize: Integer;
begin
  ASize := dbPack_ItemGetSize(ItemHnd, FObjectDataHandle);
  if ItemOffset > ASize then
      Result := dbPack_AppendItemSize(ItemHnd, ItemOffset - ASize, FObjectDataHandle)
  else if ItemOffset = ASize then
      Result := dbPack_ItemSeekLastPos(ItemHnd, FObjectDataHandle)
  else if ItemOffset = 0 then
      Result := dbPack_ItemSeekStartPos(ItemHnd, FObjectDataHandle)
  else
      Result := dbPack_ItemSeekPos(ItemOffset, ItemHnd, FObjectDataHandle);
end;

function TObjectDataManager.ItemGetPos(var ItemHnd: TItemHandle): Int64;
begin
  Result := dbPack_ItemGetPos(ItemHnd, FObjectDataHandle);
end;

function TObjectDataManager.ItemGetSize(var ItemHnd: TItemHandle): Int64;
begin
  Result := dbPack_ItemGetSize(ItemHnd, FObjectDataHandle);
end;

function TObjectDataManager.ItemWrite(var ItemHnd: TItemHandle; const ASize: Int64; var Buffers): Boolean;
begin
  Result := dbPack_ItemWrite(ASize, Buffers, ItemHnd, FObjectDataHandle);
end;

function TObjectDataManager.RecursionSearchFirst(const InitPath, MaskName: SystemString; var RecursionSearchHnd: TItemRecursionSearch): Boolean;
begin
  Init_TTMDBRecursionSearch(RecursionSearchHnd);
  Result := dbPack_RecursionSearchFirst(InitPath, MaskName, RecursionSearchHnd, FObjectDataHandle);
end;

function TObjectDataManager.RecursionSearchNext(var RecursionSearchHnd: TItemRecursionSearch): Boolean;
begin
  Result := dbPack_RecursionSearchNext(RecursionSearchHnd, FObjectDataHandle);
end;

function TObjectDataManager.ObjectDataHandlePtr: PTMDB;
begin
  Result := @FObjectDataHandle;
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
  DataWrited := wVal.DataWrited;
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
  rVal.DataWrited := DataWrited;
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

procedure TObjectDataManagerOfCache.HeaderCache_AddDataProc(p: Pointer);
begin
end;

procedure TObjectDataManagerOfCache.ItemBlockCache_AddDataProc(p: Pointer);
begin
end;

procedure TObjectDataManagerOfCache.ItemCache_AddDataProc(p: Pointer);
begin
end;

procedure TObjectDataManagerOfCache.FieldCache_AddDataProc(p: Pointer);
begin
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

procedure TObjectDataManagerOfCache.HeaderWriteProc(fPos: Int64; var wVal: THeader);
var
  p: PObjectDataCacheHeader;
begin
  p := PObjectDataCacheHeader(FHeaderCache[wVal.CurrentHeader]);
  if p = nil then
    begin
      try
        new(p);
        p^ := wVal;

        FHeaderCache.Add(wVal.CurrentHeader, p, False);
      finally
      end;
    end
  else
      p^ := wVal;

  p^.Return := db_Header_ok;
end;

procedure TObjectDataManagerOfCache.HeaderReadProc(fPos: Int64; var rVal: THeader; var Done: Boolean);
var
  p: PObjectDataCacheHeader;
begin
  p := PObjectDataCacheHeader(FHeaderCache[fPos]);
  Done := p <> nil;
  if not Done then
      Exit;
  rVal := p^;
end;

procedure TObjectDataManagerOfCache.ItemBlockWriteProc(fPos: Int64; var wVal: TItemBlock);
var
  p: PObjectDataCacheItemBlock;
begin
  p := PObjectDataCacheItemBlock(FItemBlockCache[wVal.CurrentBlockPOS]);
  if p = nil then
    begin
      try
        new(p);
        p^ := wVal;

        FItemBlockCache.Add(wVal.CurrentBlockPOS, p, False);
      finally
      end;
    end
  else
      p^ := wVal;

  p^.Return := db_Item_ok;
end;

procedure TObjectDataManagerOfCache.ItemBlockReadProc(fPos: Int64; var rVal: TItemBlock; var Done: Boolean);
var
  p: PObjectDataCacheItemBlock;
begin
  p := PObjectDataCacheItemBlock(FItemBlockCache[fPos]);
  Done := p <> nil;
  if not Done then
      Exit;
  rVal := p^;
end;

procedure TObjectDataManagerOfCache.ItemWriteProc(fPos: Int64; var wVal: TItem);
var
  p: PObjectDataCacheItem;
begin
  HeaderWriteProc(fPos, wVal.RHeader);

  p := PObjectDataCacheItem(FItemCache[wVal.RHeader.DataMainPOS]);
  if p = nil then
    begin
      try
        new(p);
        p^.write(wVal);

        FItemCache.Add(wVal.RHeader.DataMainPOS, p, False);
      finally
      end;
    end
  else
      p^.write(wVal);

  p^.Return := db_Item_ok;
end;

procedure TObjectDataManagerOfCache.ItemReadProc(fPos: Int64; var rVal: TItem; var Done: Boolean);
var
  p: PObjectDataCacheItem;
begin
  HeaderReadProc(fPos, rVal.RHeader, Done);

  if not Done then
      Exit;

  p := PObjectDataCacheItem(FItemCache[rVal.RHeader.DataMainPOS]);
  Done := p <> nil;
  if not Done then
      Exit;

  p^.read(rVal);
end;

procedure TObjectDataManagerOfCache.OnlyItemRecWriteProc(fPos: Int64; var wVal: TItem);
var
  p: PObjectDataCacheItem;
begin
  p := PObjectDataCacheItem(FItemCache[fPos]);
  if p = nil then
    begin
      try
        new(p);
        p^.write(wVal);

        FItemCache.Add(fPos, p, False);
      finally
      end;
    end
  else
      p^.write(wVal);

  p^.Return := db_Item_ok;
end;

procedure TObjectDataManagerOfCache.OnlyItemRecReadProc(fPos: Int64; var rVal: TItem; var Done: Boolean);
var
  p: PObjectDataCacheItem;
begin
  p := PObjectDataCacheItem(FItemCache[fPos]);
  Done := p <> nil;
  if not Done then
      Exit;

  p^.read(rVal);
end;

procedure TObjectDataManagerOfCache.FieldWriteProc(fPos: Int64; var wVal: TField);
var
  p: PObjectDataCacheField;
begin
  HeaderWriteProc(fPos, wVal.RHeader);

  p := PObjectDataCacheField(FFieldCache[wVal.RHeader.DataMainPOS]);
  if p = nil then
    begin
      try
        new(p);
        p^.write(wVal);

        FFieldCache.Add(wVal.RHeader.DataMainPOS, p, False);
      finally
      end;
    end
  else
      p^.write(wVal);

  p^.Return := db_Field_ok;
end;

procedure TObjectDataManagerOfCache.FieldReadProc(fPos: Int64; var rVal: TField; var Done: Boolean);
var
  p: PObjectDataCacheField;
begin
  HeaderReadProc(fPos, rVal.RHeader, Done);

  if not Done then
      Exit;

  p := PObjectDataCacheField(FFieldCache[rVal.RHeader.DataMainPOS]);
  Done := p <> nil;
  if not Done then
      Exit;

  p^.read(rVal);
end;

procedure TObjectDataManagerOfCache.OnlyFieldRecWriteProc(fPos: Int64; var wVal: TField);
var
  p: PObjectDataCacheField;
begin
  p := PObjectDataCacheField(FFieldCache[fPos]);
  if p = nil then
    begin
      try
        new(p);
        p^.write(wVal);

        FFieldCache.Add(fPos, p, False);
      finally
      end;
    end
  else
      p^.write(wVal);

  p^.Return := db_Field_ok;
end;

procedure TObjectDataManagerOfCache.OnlyFieldRecReadProc(fPos: Int64; var rVal: TField; var Done: Boolean);
var
  p: PObjectDataCacheField;
begin
  p := PObjectDataCacheField(FFieldCache[fPos]);
  Done := p <> nil;
  if not Done then
      Exit;

  p^.read(rVal);
end;

procedure TObjectDataManagerOfCache.DoCreateFinish;
begin
  inherited DoCreateFinish;

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

  FHeaderCache.OnAddPtr := {$IFDEF FPC}@{$ENDIF FPC}HeaderCache_AddDataProc;
  FItemBlockCache.OnAddPtr := {$IFDEF FPC}@{$ENDIF FPC}ItemBlockCache_AddDataProc;
  FItemCache.OnAddPtr := {$IFDEF FPC}@{$ENDIF FPC}ItemCache_AddDataProc;
  FFieldCache.OnAddPtr := {$IFDEF FPC}@{$ENDIF FPC}FieldCache_AddDataProc;

  FHeaderCache.OnFreePtr := {$IFDEF FPC}@{$ENDIF FPC}HeaderCache_DataFreeProc;
  FItemBlockCache.OnFreePtr := {$IFDEF FPC}@{$ENDIF FPC}ItemBlockCache_DataFreeProc;
  FItemCache.OnFreePtr := {$IFDEF FPC}@{$ENDIF FPC}ItemCache_DataFreeProc;
  FFieldCache.OnFreePtr := {$IFDEF FPC}@{$ENDIF FPC}FieldCache_DataFreeProc;

  BuildDBCacheIntf;
end;

destructor TObjectDataManagerOfCache.Destroy;
begin
  FreeDBCacheIntf;
  DisposeObject([FHeaderCache, FItemBlockCache, FItemCache, FFieldCache]);
  inherited Destroy;
end;

procedure TObjectDataManagerOfCache.BuildDBCacheIntf;
begin
  ObjectDataHandlePtr^.OnWriteHeader := {$IFDEF FPC}@{$ENDIF FPC}HeaderWriteProc;
  ObjectDataHandlePtr^.OnReadHeader := {$IFDEF FPC}@{$ENDIF FPC}HeaderReadProc;
  ObjectDataHandlePtr^.OnWriteItemBlock := {$IFDEF FPC}@{$ENDIF FPC}ItemBlockWriteProc;
  ObjectDataHandlePtr^.OnReadItemBlock := {$IFDEF FPC}@{$ENDIF FPC}ItemBlockReadProc;
  ObjectDataHandlePtr^.OnWriteItem := {$IFDEF FPC}@{$ENDIF FPC}ItemWriteProc;
  ObjectDataHandlePtr^.OnReadItem := {$IFDEF FPC}@{$ENDIF FPC}ItemReadProc;
  ObjectDataHandlePtr^.OnOnlyWriteItemRec := {$IFDEF FPC}@{$ENDIF FPC}OnlyItemRecWriteProc;
  ObjectDataHandlePtr^.OnOnlyReadItemRec := {$IFDEF FPC}@{$ENDIF FPC}OnlyItemRecReadProc;
  ObjectDataHandlePtr^.OnWriteField := {$IFDEF FPC}@{$ENDIF FPC}FieldWriteProc;
  ObjectDataHandlePtr^.OnReadField := {$IFDEF FPC}@{$ENDIF FPC}FieldReadProc;
  ObjectDataHandlePtr^.OnOnlyWriteFieldRec := {$IFDEF FPC}@{$ENDIF FPC}OnlyFieldRecWriteProc;
  ObjectDataHandlePtr^.OnOnlyReadFieldRec := {$IFDEF FPC}@{$ENDIF FPC}OnlyFieldRecReadProc;
end;

procedure TObjectDataManagerOfCache.FreeDBCacheIntf;
begin
  ObjectDataHandlePtr^.OnWriteHeader := nil;
  ObjectDataHandlePtr^.OnReadHeader := nil;
  ObjectDataHandlePtr^.OnWriteItemBlock := nil;
  ObjectDataHandlePtr^.OnReadItemBlock := nil;
  ObjectDataHandlePtr^.OnWriteItem := nil;
  ObjectDataHandlePtr^.OnReadItem := nil;
  ObjectDataHandlePtr^.OnOnlyWriteItemRec := nil;
  ObjectDataHandlePtr^.OnOnlyReadItemRec := nil;
  ObjectDataHandlePtr^.OnWriteField := nil;
  ObjectDataHandlePtr^.OnReadField := nil;
  ObjectDataHandlePtr^.OnOnlyWriteFieldRec := nil;
  ObjectDataHandlePtr^.OnOnlyReadFieldRec := nil;

  CleaupCache;
end;

procedure TObjectDataManagerOfCache.CleaupCache;
begin
  FHeaderCache.Clear;
  FItemBlockCache.Clear;
  FItemCache.Clear;
  FFieldCache.Clear;
end;

function TObjectDataMarshal.GetItems(aIndex: Integer): TObjectDataManager;
begin
  Result := TObjectDataManager(FLibList.Objects[aIndex]);
end;

function TObjectDataMarshal.GetNames(AName: SystemString): TObjectDataManager;
var
  RepaInt: Integer;
  aUName: SystemString;
begin
  Result := nil;
  aUName := GetAbsoluteFileName(AName);
  if FLibList.Count > 0 then
    begin
      if FUseWildcard then
        begin
          if not umlMatchLimitChar('\', aUName) then
              aUName := '*\' + aUName;
          for RepaInt := 0 to FLibList.Count - 1 do
            begin
              if umlMultipleMatch(False, aUName, FLibList[RepaInt]) then
                begin
                  Result := TObjectDataManager(FLibList.Objects[RepaInt]);
                  Exit;
                end;
            end;
        end
      else
        begin
          if umlMatchLimitChar('\', aUName) then
            begin
              for RepaInt := 0 to FLibList.Count - 1 do
                if umlSameText(aUName, FLibList[RepaInt]) then
                  begin
                    Result := TObjectDataManager(FLibList.Objects[RepaInt]);
                    Exit;
                  end;
            end
          else
            begin
              for RepaInt := 0 to FLibList.Count - 1 do
                if umlSameText(aUName, umlGetLastStr(FLibList[RepaInt], '\')) then
                  begin
                    Result := TObjectDataManager(FLibList.Objects[RepaInt]);
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

constructor TObjectDataMarshal.Create(AID: Byte);
begin
  inherited Create;
  FID := AID;
  FLibList := TCoreClassStringList.Create;
  FUseWildcard := True;
end;

destructor TObjectDataMarshal.Destroy;
begin
  Clear;
  DisposeObject(FLibList);
  inherited Destroy;
end;

function TObjectDataMarshal.GetAbsoluteFileName(aFileName: SystemString): SystemString;
begin
  Result := umlUpperCase(umlCharReplace(umlTrimSpace(aFileName), '/', '\')).Text;
end;

function TObjectDataMarshal.NewDB(aFile: SystemString; aOnlyRead: Boolean): TObjectDataManager;
var
  RepaInt: Integer;
  aUName: SystemString;
begin
  Result := nil;
  aUName := GetAbsoluteFileName(aFile);
  if FLibList.Count > 0 then
    for RepaInt := 0 to FLibList.Count - 1 do
      if umlSameText(aUName, FLibList[RepaInt]) then
          Result := TObjectDataManager(FLibList.Objects[RepaInt]);
  if Result = nil then
    begin
      Result := TObjectDataManager.CreateNew(aFile, FID);
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

function TObjectDataMarshal.Open(aFile: SystemString; aOnlyRead: Boolean): TObjectDataManager;
var
  RepaInt: Integer;
  aUName: SystemString;
begin
  Result := nil;
  aUName := GetAbsoluteFileName(aFile);
  if FLibList.Count > 0 then
    for RepaInt := 0 to FLibList.Count - 1 do
      if umlSameText(aUName, FLibList[RepaInt]) then
          Result := TObjectDataManager(FLibList.Objects[RepaInt]);
  if Result = nil then
    begin
      Result := TObjectDataManager.Create(aFile, FID, aOnlyRead);
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
  RepaInt: Integer;
  aUName: SystemString;
begin
  aUName := GetAbsoluteFileName(AName);
  if FLibList.Count > 0 then
    begin
      if FUseWildcard then
        begin
          if not umlMatchLimitChar('\', aUName) then
              aUName := '*\' + aUName;
          RepaInt := 0;
          while RepaInt < FLibList.Count do
            begin
              if umlMultipleMatch(False, aUName, FLibList[RepaInt]) then
                begin
                  DisposeObject(FLibList.Objects[RepaInt]);
                  FLibList.Delete(RepaInt);
                end
              else
                  inc(RepaInt);
            end;
        end
      else
        begin
          if umlMatchLimitChar('\', aUName) then
            begin
              RepaInt := 0;
              while RepaInt < FLibList.Count do
                begin
                  if umlSameText(aUName, FLibList[RepaInt]) then
                    begin
                      DisposeObject(FLibList.Objects[RepaInt]);
                      FLibList.Delete(RepaInt);
                    end
                  else
                      inc(RepaInt);
                end;
            end
          else
            begin
              RepaInt := 0;
              while RepaInt < FLibList.Count do
                begin
                  if umlSameText(aUName, umlGetLastStr(FLibList[RepaInt], '\')) then
                    begin
                      DisposeObject(FLibList.Objects[RepaInt]);
                      FLibList.Delete(RepaInt);
                    end
                  else
                      inc(RepaInt);
                end;
            end;
        end;
    end;
end;

procedure TObjectDataMarshal.UpdateAll;
var
  RepaInt: Integer;
begin
  if Count > 0 then
    for RepaInt := 0 to Count - 1 do
        Items[RepaInt].Update;
end;

procedure TObjectDataMarshal.Disable;
var
  RepaInt: Integer;
begin
  if Count > 0 then
    for RepaInt := 0 to Count - 1 do
        Items[RepaInt].Close;
end;

procedure TObjectDataMarshal.Enabled;
var
  RepaInt: Integer;
begin
  if Count > 0 then
    for RepaInt := 0 to Count - 1 do
        Items[RepaInt].Open;
end;

initialization

ObjectDataMarshal();

finalization

if I_ObjectDataMarshal <> nil then
  begin
    DisposeObject(I_ObjectDataMarshal);
    I_ObjectDataMarshal := nil;
  end;

end.
