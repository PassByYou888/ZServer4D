{ ****************************************************************************** }
{ * ObjectDBManager                                                            * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
(* https://github.com/PassByYou888/ZServer4D *)
{ ****************************************************************************** }

unit ObjectDataManager;

{$I zDefine.inc}

interface

uses CoreClasses, ObjectData, UnicodeMixedLib, PascalStrings;

type
  TItemHandle          = TTMDBItemHandle;
  PItemHandle          = ^TItemHandle;
  PFieldHandle         = ^TFieldHandle;
  TFieldHandle         = TField;
  PItemSearch          = ^TItemSearch;
  TItemSearch          = TTMDBSearchItem;
  PFieldSearch         = ^TFieldSearch;
  TFieldSearch         = TTMDBSearchField;
  PItemRecursionSearch = ^TItemRecursionSearch;
  TItemRecursionSearch = TTMDBRecursionSearch;

  TObjectDataManager = class(TCoreClassObject)
  private
    FStreamEngine                                           : TCoreClassStream;
    ObjectDataHandle                                        : TTMDB;
    FNeedCreateNew, FOverWriteItem, FSameItemName, FOnlyRead: Boolean;
    FObjectName                                             : string;
    FDefaultItemID                                          : Byte;
    FIsOpened                                               : Boolean;
    FData                                                   : Pointer;

    function GetAutoFreeHandle: Boolean;
    procedure SetAutoFreeHandle(const Value: Boolean);
  protected
    function GetOverWriteItem: Boolean;
    function GetSameItemName: Boolean;
    function GetDBTime: TDateTime;
    procedure SetOverWriteItem(Value: Boolean);
    procedure SetSameItemName(Value: Boolean);
  public
    constructor Create(const aObjectName: string; const aID: Byte; aOnlyRead: Boolean);
    constructor CreateNew(const aObjectName: string; const aID: Byte);
    constructor CreateAsStream(aStream: TCoreClassStream; const aObjectName: string; const aID: Byte; aOnlyRead, aIsNewData, aAutoFreeHandle: Boolean);
    destructor Destroy; override;
    function Open: Boolean;
    function NewHandle(aStream: TCoreClassStream; const aObjectName: string; const aID: Byte; aOnlyRead, aIsNew: Boolean): Boolean;
    function CopyTo(DestDB: TObjectDataManager): Boolean;
    function CopyToPath(DestDB: TObjectDataManager; DestPath: string): Boolean;
    function CopyFieldToPath(FieldPos: Int64; DestDB: TObjectDataManager; DestPath: string): Boolean;
    procedure SaveToStream(Stream: TCoreClassStream);
    procedure ImpFromPath(ImpPath, dbPath: string; IncludeSub: Boolean);
    procedure ImpFromFiles(ImpFiles: TCoreClassStrings; dbPath: string);
    function isAbort: Boolean;
    function Close: Boolean;
    function ErrorNo: Int64;
    function Modify: Boolean;
    function Size: Int64;
    procedure SetID(const ID: Byte);
    procedure Update;

    function CreateDir(const DirName, DirDescription: string): Boolean;
    function CreateRootField(const RootName: string): Boolean;
    function DirectoryExists(const DirName: string): Boolean;
    function FastDelete(const FieldPos: Int64; const aPos: Int64): Boolean;
    function FastFieldExists(const FieldPos: Int64; const FieldName: string): Boolean;
    function FastFieldCreate(const FieldPos: Int64; const FieldName, FieldDescription: string; var NewFieldPos: Int64): Boolean;
    function SetRootField(const RootName: string): Boolean;
    function FieldReName(const FieldPos: Int64; const NewFieldName, NewFieldDescription: string): Boolean;
    function RootField: Int64;
    function FieldDelete(const dbPath: string; const FieldName: string): Boolean;
    function FieldExists(const dbPath: string; const FieldName: string): Boolean;
    function FieldFastFindFirst(const FieldPos: Int64; const Filter: string; var FieldSearchHandle: TFieldSearch): Boolean;
    function FieldFastFindLast(const FieldPos: Int64; const Filter: string; var FieldSearchHandle: TFieldSearch): Boolean;
    function FieldFastFindNext(var FieldSearchHandle: TFieldSearch): Boolean;
    function FieldFastFindPrev(var FieldSearchHandle: TFieldSearch): Boolean;
    function FieldFindFirst(const dbPath, Filter: string; var FieldSearchHandle: TFieldSearch): Boolean;
    function FieldFindLast(const dbPath, Filter: string; var FieldSearchHandle: TFieldSearch): Boolean;
    function FieldFindNext(var FieldSearchHandle: TFieldSearch): Boolean;
    function FieldFindPrev(var FieldSearchHandle: TFieldSearch): Boolean;
    function FieldMove(const dbPath, FieldName, DestPath: string): Boolean;
    function GetFieldData(const FieldPos: Int64; var Dest: TFieldHandle): Boolean;
    function GetFieldPath(const FieldPos: Int64): string;
    function GetPathField(const dbPath: string; var Dest: Int64): Boolean;
    function GetPathFieldPos(const dbPath: string): Int64;

    function AddFile(const dbPath, DBItem, DBItemDescription, FileName: string): Boolean;
    function PutToFile(const dbPath, DBItem, FileName: string): Boolean;
    function GetItemSize(const dbPath, DBItem: string): Int64;
    function ItemAddFile(var ItemHnd: TItemHandle; const FileName: string): Boolean;
    function ItemAutoConnect(const dbPath, DBItem, DBItemDescription: string; var ItemHnd: TItemHandle): Boolean;
    function ItemFastAutoConnect_F(const FieldPos: Int64; const DBItem, DBItemDescription: string; var ItemHnd: TItemHandle): Boolean;
    function ItemFastAutoConnect_L(const FieldPos: Int64; const DBItem, DBItemDescription: string; var ItemHnd: TItemHandle): Boolean;
    function ItemUpdate(var ItemHnd: TItemHandle): Boolean;
    function ItemClose(var ItemHnd: TItemHandle): Boolean;
    function ItemReName(const FieldPos: Int64; var ItemHnd: TItemHandle; const aNewName, aNewDescription: string): Boolean;
    function ItemCopyTo(var ItemHnd: TItemHandle; DestDB: TObjectDataManager; var DestItemHandle: TItemHandle; const CopySize: Int64): Boolean;
    function ItemCreate(const dbPath, DBItem, DBItemDescription: string; var ItemHnd: TItemHandle): Boolean;
    function ItemDelete(const dbPath, DBItem: string): Boolean;
    function ItemExists(const dbPath, DBItem: string): Boolean;
    function ItemFastInsertNew(const FieldPos, InsertHeaderPos: Int64; const DBItem, DBItemDescription: string; var ItemHnd: TItemHandle): Boolean;
    function ItemFastCreate(const aPos: Int64; const DBItem, DBItemDescription: string; var ItemHnd: TItemHandle): Boolean;
    function ItemFastExists(const FieldPos: Int64; const DBItem: string): Boolean;
    function ItemFastFindFirst(const FieldPos: Int64; const DBItem: string; var ItemSearchHandle: TItemSearch): Boolean;
    function ItemFastFindLast(const FieldPos: Int64; const DBItem: string; var ItemSearchHandle: TItemSearch): Boolean;
    function ItemFastFindNext(var ItemSearchHandle: TItemSearch): Boolean;
    function ItemFastFindPrev(var ItemSearchHandle: TItemSearch): Boolean;
    function ItemFastOpen(const aPos: Int64; var ItemHnd: TItemHandle): Boolean;
    function ItemFastResetBody(const aPos: Int64): Boolean;
    function ItemFindFirst(const dbPath, DBItem: string; var ItemSearchHandle: TItemSearch): Boolean;
    function ItemFindLast(const dbPath, DBItem: string; var ItemSearchHandle: TItemSearch): Boolean;
    function ItemFindNext(var ItemSearchHandle: TItemSearch): Boolean;
    function ItemFindPrev(var ItemSearchHandle: TItemSearch): Boolean;
    function ItemGetFile(var ItemHnd: TItemHandle; const FileName: string): Boolean;
    function ItemMove(const dbPath, ItemName, DestPath: string): Boolean;
    function ItemOpen(const dbPath, DBItem: string; var ItemHnd: TItemHandle): Boolean;
    function ItemRead(var ItemHnd: TItemHandle; const aSize: Int64; var Buffers): Boolean;
    function ItemReadBool(var ItemHnd: TItemHandle; var Value: Boolean): Boolean;
    function ItemReadInt(var ItemHnd: TItemHandle; var Value: Integer): Boolean;
    function ItemReadReturnBool(var ItemHnd: TItemHandle; const DefaultValue: Boolean): Boolean;
    function ItemReadReturnInt(var ItemHnd: TItemHandle; const DefaultValue: Integer): Integer;
    function ItemReadReturnStr(var ItemHnd: TItemHandle; const DefaultValue: string): string;
    function ItemReadStr(var ItemHnd: TItemHandle; var Value: string): Boolean;
    function ItemReadTime(var ItemHnd: TItemHandle; var Value: TDateTime): Boolean;
    function ItemSeekStart(var ItemHnd: TItemHandle): Boolean;
    function ItemSeekLast(var ItemHnd: TItemHandle): Boolean;
    function ItemSeek(var ItemHnd: TItemHandle; const ItemOffset: Int64): Boolean;
    function ItemGetPos(var ItemHnd: TItemHandle): Int64;
    function ItemGetSize(var ItemHnd: TItemHandle): Int64;
    function ItemWrite(var ItemHnd: TItemHandle; const aSize: Int64; var Buffers): Boolean;
    function ItemWriteBool(var ItemHnd: TItemHandle; const Value: Boolean): Boolean;
    function ItemWriteInt(var ItemHnd: TItemHandle; const Value: Integer): Boolean;
    function ItemWriteStr(var ItemHnd: TItemHandle; const Value: string): Boolean;
    function ItemWriteTime(var ItemHnd: TItemHandle; const Value: TDateTime): Boolean;

    function ReadBool(const Section, Item: string; const Value: Boolean): Boolean;
    function ReadString(const Section, Item, Value: string): string;
    function WriteBool(const Section, Item: string; const Value: Boolean): Boolean;
    function WriteString(const Section, Item, Value: string): Boolean;

    function RecursionSearchFirst(const InitPath, MaskName: string; var RecursionSearchHnd: TItemRecursionSearch): Boolean;
    function RecursionSearchNext(var RecursionSearchHnd: TItemRecursionSearch): Boolean;

    property AutoFreeHandle: Boolean read GetAutoFreeHandle write SetAutoFreeHandle;
    property IsOnlyRead: Boolean read FOnlyRead;
    property NeedCreateNew: Boolean read FNeedCreateNew;
    property ObjectName: string read FObjectName;
    property DefaultItemID: Byte read FDefaultItemID;
    property StreamEngine: TCoreClassStream read FStreamEngine;
    property Time: TDateTime read GetDBTime;

    property OverWriteItem: Boolean read GetOverWriteItem write SetOverWriteItem;
    property SameItemName: Boolean read GetSameItemName write SetSameItemName;
    // user custom data
    property Data: Pointer read FData write FData;
  end;

  TObjectDataMarshal = class(TCoreClassObject)
  private
    FID         : Byte;
    FLibList    : TCoreClassStrings;
    FUseWildcard: Boolean;
    function GetItems(aIndex: Integer): TObjectDataManager;
    function GetNames(AName: string): TObjectDataManager;
    procedure SetItems(aIndex: Integer; const Value: TObjectDataManager);
  public
    constructor Create(aID: Byte);
    destructor Destroy; override;
    function GetAbsoluteFileName(aFileName: string): string;
    function NewDB(aFile: string; aOnlyRead: Boolean): TObjectDataManager;
    function Open(aFile: string; aOnlyRead: Boolean): TObjectDataManager;
    procedure CloseDB(db: TObjectDataManager);
    procedure Clear;
    function Count: Integer;
    procedure Delete(aIndex: Integer);
    procedure DeleteFromName(AName: string);
    procedure UpdateAll;
    procedure Disable;
    procedure Enabled;

    property LibList: TCoreClassStrings read FLibList;
    property Items[aIndex: Integer]: TObjectDataManager read GetItems write SetItems;
    property Names[AName: string]: TObjectDataManager read GetNames; default;
    property UseWildcard: Boolean read FUseWildcard write FUseWildcard;
    property ID: Byte read FID write FID;
  end;

function ObjectDataMarshal: TObjectDataMarshal;

implementation

uses ItemStream, Types;

const
  MaxBuffSize  = 65535;
  UserRootName = 'User';

var
  _ObjectDataMarshal: TObjectDataMarshal = nil;

function ObjectDataMarshal: TObjectDataMarshal;
begin
  if _ObjectDataMarshal = nil then
      _ObjectDataMarshal := TObjectDataMarshal.Create(0);
  Result := _ObjectDataMarshal;
end;

function TObjectDataManager.GetAutoFreeHandle: Boolean;
begin
  if not isAbort then
      Result := ObjectDataHandle.RecFile.AutoFree
  else
      Result := False;
end;

procedure TObjectDataManager.SetAutoFreeHandle(const Value: Boolean);
begin
  if not isAbort then
      ObjectDataHandle.RecFile.AutoFree := Value;
end;

function TObjectDataManager.GetOverWriteItem: Boolean;
begin
  Result := ObjectDataHandle.OverWriteItem;
end;

function TObjectDataManager.GetSameItemName: Boolean;
begin
  Result := ObjectDataHandle.SameItemName;
end;

function TObjectDataManager.GetDBTime: TDateTime;
begin
  Result := ObjectDataHandle.CreateTime;
end;

procedure TObjectDataManager.SetOverWriteItem(Value: Boolean);
begin
  ObjectDataHandle.OverWriteItem := Value;
  FOverWriteItem := Value;
end;

procedure TObjectDataManager.SetSameItemName(Value: Boolean);
begin
  ObjectDataHandle.SameItemName := Value;
  FSameItemName := Value;
end;

constructor TObjectDataManager.Create(const aObjectName: string; const aID: Byte; aOnlyRead: Boolean);
begin
  inherited Create;
  NewHandle(nil, aObjectName, aID, aOnlyRead, False);
end;

constructor TObjectDataManager.CreateNew(const aObjectName: string; const aID: Byte);
begin
  inherited Create;
  NewHandle(nil, aObjectName, aID, False, True);
end;

constructor TObjectDataManager.CreateAsStream(aStream: TCoreClassStream; const aObjectName: string; const aID: Byte; aOnlyRead, aIsNewData, aAutoFreeHandle: Boolean);
begin
  inherited Create;
  NewHandle(aStream, aObjectName, aID, aOnlyRead, aIsNewData);
  AutoFreeHandle := aAutoFreeHandle;
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
          if not dbPack_CreateAsStream(StreamEngine, ObjectName, '', ObjectDataHandle) then
            begin
              Exit;
            end;
          if not(dbPack_CreateRootField(UserRootName, '', ObjectDataHandle)) then
              Exit;
          if not(dbPack_SetCurrentRootField(UserRootName, ObjectDataHandle)) then
              Exit;
        end
      else
        begin
          if not dbPack_OpenAsStream(StreamEngine, ObjectName, ObjectDataHandle, IsOnlyRead) then
            begin
              Exit;
            end;
        end;
    end
  else if (FNeedCreateNew) or (not umlFileExists(ObjectName)) then
    begin
      if not dbPack_CreatePack(ObjectName, '', ObjectDataHandle) then
        begin
          dbPack_ClosePack(ObjectDataHandle);
          InitTTMDB(ObjectDataHandle);
          if not dbPack_CreatePack(ObjectName, '', ObjectDataHandle) then
            begin
              Exit;
            end;
        end;
      if not(dbPack_CreateRootField(UserRootName, '', ObjectDataHandle)) then
          Exit;
      if not(dbPack_SetCurrentRootField(UserRootName, ObjectDataHandle)) then
          Exit;
    end
  else if not dbPack_OpenPack(ObjectName, ObjectDataHandle, IsOnlyRead) then
    begin
      dbPack_ClosePack(ObjectDataHandle);
      InitTTMDB(ObjectDataHandle);
      if not dbPack_OpenPack(ObjectName, ObjectDataHandle, IsOnlyRead) then
        begin
          Exit;
        end;
    end;
  ObjectDataHandle.OverWriteItem := FOverWriteItem;
  ObjectDataHandle.SameItemName := FSameItemName;
  Result := True;
end;

function TObjectDataManager.NewHandle(aStream: TCoreClassStream; const aObjectName: string; const aID: Byte; aOnlyRead, aIsNew: Boolean): Boolean;
begin
  Close;
  InitTTMDB(ObjectDataHandle);
  FStreamEngine := aStream;
  FObjectName := aObjectName;
  FNeedCreateNew := aIsNew;
  FOnlyRead := aOnlyRead;
  FDefaultItemID := aID;
  FIsOpened := Open();
  Result := FIsOpened;

  OverWriteItem := True;
  SameItemName := False;
  AutoFreeHandle := True;
  FData := nil;
end;

function TObjectDataManager.CopyTo(DestDB: TObjectDataManager): Boolean;
begin
  Result := dbPack_CopyAllTo(ObjectDataHandle, DestDB.ObjectDataHandle);
end;

function TObjectDataManager.CopyToPath(DestDB: TObjectDataManager; DestPath: string): Boolean;
begin
  Result := dbPack_CopyAllToDestPath(ObjectDataHandle, DestDB.ObjectDataHandle, DestPath);
end;

function TObjectDataManager.CopyFieldToPath(FieldPos: Int64; DestDB: TObjectDataManager; DestPath: string): Boolean;
var
  DestFieldPos: Int64;
begin
  Result := False;
  CreateDir(DestPath, '');
  if GetPathField(DestPath, DestFieldPos) then
      Result := dbPack_CopyFieldTo('*', ObjectDataHandle, FieldPos, DestDB.ObjectDataHandle, DestFieldPos);
end;

procedure TObjectDataManager.SaveToStream(Stream: TCoreClassStream);
var
  e: TObjectDataManager;
begin
  e := TObjectDataManager.CreateAsStream(Stream, ObjectName, DefaultItemID, False, True, False);
  CopyTo(e);
  DisposeObject(e);
end;

procedure TObjectDataManager.ImpFromPath(ImpPath, dbPath: string; IncludeSub: Boolean);
var
  fAry     : umlStringDynArray;
  n        : string;
  fPos     : Int64;
  fs       : TCoreClassFileStream;
  itmHnd   : TItemHandle;
  itmStream: TItemStreamEngine;
begin
  dbPath := umlCharReplace(dbPath, '\', '/').Text;
  if not DirectoryExists(dbPath) then
      CreateDir(dbPath, '');
  fPos := GetPathFieldPos(dbPath);

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
          ImpFromPath(n, umlCombineFileName(dbPath, umlGetFileName(n)).Text, IncludeSub);
    end;
end;

procedure TObjectDataManager.ImpFromFiles(ImpFiles: TCoreClassStrings; dbPath: string);
var
  i        : Integer;
  n        : string;
  fPos     : Int64;
  fs       : TCoreClassFileStream;
  itmHnd   : TItemHandle;
  itmStream: TItemStreamEngine;
begin
  dbPath := umlCharReplace(dbPath, '\', '/').Text;
  if not DirectoryExists(dbPath) then
      CreateDir(dbPath, '');
  fPos := GetPathFieldPos(dbPath);

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
  Result := dbPack_ClosePack(ObjectDataHandle);
end;

function TObjectDataManager.ErrorNo: Int64;
begin
  Result := ObjectDataHandle.Return;
end;

function TObjectDataManager.Modify: Boolean;
begin
  Result := ObjectDataHandle.WriteFlags;
end;

function TObjectDataManager.Size: Int64;
begin
  Result := ObjectDataHandle.RecFile.Size;
end;

procedure TObjectDataManager.SetID(const ID: Byte);
begin
  FDefaultItemID := ID;
end;

procedure TObjectDataManager.Update;
begin
  dbPack_Update(ObjectDataHandle);
end;

function TObjectDataManager.CreateDir(const DirName, DirDescription: string): Boolean;
begin
  Result := dbPack_CreateField(DirName, DirDescription, ObjectDataHandle);
end;

function TObjectDataManager.CreateRootField(const RootName: string): Boolean;
begin
  Result := dbPack_CreateRootField(RootName, RootName, ObjectDataHandle);
end;

function TObjectDataManager.DirectoryExists(const DirName: string): Boolean;
var
  Field: TFieldHandle;
begin
  Result := dbPack_GetField(DirName, Field, ObjectDataHandle);
end;

function TObjectDataManager.FastDelete(const FieldPos: Int64; const aPos: Int64): Boolean;
var
  FieldHnd: TFieldHandle;
begin
  InitTField(FieldHnd);
  Result := False;
  if dbField_ReadRec(FieldPos, ObjectDataHandle.RecFile, FieldHnd) then
      Result := dbField_DeleteHeader(aPos, FieldPos, ObjectDataHandle.RecFile, FieldHnd);
end;

function TObjectDataManager.FastFieldExists(const FieldPos: Int64; const FieldName: string): Boolean;
var
  FieldSearch: TFieldSearch;
begin
  Result := FieldFastFindFirst(FieldPos, FieldName, FieldSearch);
end;

function TObjectDataManager.FastFieldCreate(const FieldPos: Int64; const FieldName, FieldDescription: string; var NewFieldPos: Int64): Boolean;
var
  NewField: TField;
begin
  InitTField(NewField);
  NewField.Description := FieldDescription;
  Result := dbField_CreateField(FieldName, FieldPos, ObjectDataHandle.RecFile, NewField);
  NewFieldPos := NewField.RHeader.CurrentHeader;
end;

function TObjectDataManager.SetRootField(const RootName: string): Boolean;
begin
  Result := dbPack_SetCurrentRootField(RootName, ObjectDataHandle);
end;

function TObjectDataManager.FieldReName(const FieldPos: Int64; const NewFieldName, NewFieldDescription: string): Boolean;
var
  FieldHnd: TFieldHandle;
begin
  Result := False;
  if not umlExistsLimitChar(NewFieldName, '\/') then
    begin
      InitTField(FieldHnd);
      if dbField_ReadRec(FieldPos, ObjectDataHandle.RecFile, FieldHnd) then
        begin
          if (not FastFieldExists(FieldHnd.UpLevelFieldPOS, NewFieldName)) and (FieldHnd.RHeader.CurrentHeader <> ObjectDataHandle.DefaultFieldPOS) then
            begin
              FieldHnd.RHeader.Name := NewFieldName;
              FieldHnd.Description := NewFieldDescription;
              Result := dbField_WriteRec(FieldPos, ObjectDataHandle.RecFile, FieldHnd);
            end;
        end;
    end;
end;

function TObjectDataManager.RootField: Int64;
begin
  Result := ObjectDataHandle.DefaultFieldPOS;
end;

function TObjectDataManager.FieldDelete(const dbPath: string; const FieldName: string): Boolean;
begin
  Result := dbPack_DeleteField(dbPath, FieldName, ObjectDataHandle);
end;

function TObjectDataManager.FieldExists(const dbPath: string; const FieldName: string): Boolean;
var
  FieldSearch: TFieldSearch;
begin
  Result := FieldFindFirst(dbPath, FieldName, FieldSearch);
end;

function TObjectDataManager.FieldFastFindFirst(const FieldPos: Int64; const Filter: string; var FieldSearchHandle: TFieldSearch): Boolean;
begin
  InitTTMDBSearchField(FieldSearchHandle);
  Result := dbPack_FastFindFirstField(FieldPos, Filter, FieldSearchHandle, ObjectDataHandle);
end;

function TObjectDataManager.FieldFastFindLast(const FieldPos: Int64; const Filter: string; var FieldSearchHandle: TFieldSearch): Boolean;
begin
  InitTTMDBSearchField(FieldSearchHandle);
  Result := dbPack_FastFindLastField(FieldPos, Filter, FieldSearchHandle, ObjectDataHandle);
end;

function TObjectDataManager.FieldFastFindNext(var FieldSearchHandle: TFieldSearch): Boolean;
begin
  Result := dbPack_FastFindNextField(FieldSearchHandle, ObjectDataHandle);
end;

function TObjectDataManager.FieldFastFindPrev(var FieldSearchHandle: TFieldSearch): Boolean;
begin
  Result := dbPack_FastFindPrevField(FieldSearchHandle, ObjectDataHandle);
end;

function TObjectDataManager.FieldFindFirst(const dbPath, Filter: string; var FieldSearchHandle: TFieldSearch): Boolean;
begin
  InitTTMDBSearchField(FieldSearchHandle);
  Result := dbPack_FindFirstField(dbPath, Filter, FieldSearchHandle, ObjectDataHandle);
end;

function TObjectDataManager.FieldFindLast(const dbPath, Filter: string; var FieldSearchHandle: TFieldSearch): Boolean;
begin
  InitTTMDBSearchField(FieldSearchHandle);
  Result := dbPack_FindLastField(dbPath, Filter, FieldSearchHandle, ObjectDataHandle);
end;

function TObjectDataManager.FieldFindNext(var FieldSearchHandle: TFieldSearch): Boolean;
begin
  Result := dbPack_FindNextField(FieldSearchHandle, ObjectDataHandle);
end;

function TObjectDataManager.FieldFindPrev(var FieldSearchHandle: TFieldSearch): Boolean;
begin
  Result := dbPack_FindPrevField(FieldSearchHandle, ObjectDataHandle);
end;

function TObjectDataManager.FieldMove(const dbPath, FieldName, DestPath: string): Boolean;
begin
  Result := dbPack_MoveField(dbPath, FieldName, DestPath, ObjectDataHandle);
end;

function TObjectDataManager.GetFieldData(const FieldPos: Int64; var Dest: TFieldHandle): Boolean;
begin
  InitTField(Dest);
  Result := dbField_ReadRec(FieldPos, ObjectDataHandle.RecFile, Dest);
end;

function TObjectDataManager.GetFieldPath(const FieldPos: Int64): string;
var
  ReturnPath: umlString;
begin
  if dbPack_GetPath(FieldPos, ObjectDataHandle.DefaultFieldPOS, ObjectDataHandle, ReturnPath) then
      Result := ReturnPath
  else
      Result := '';
end;

function TObjectDataManager.GetPathField(const dbPath: string; var Dest: Int64): Boolean;
var
  FieldHnd: TFieldHandle;
begin
  Result := dbPack_GetField(dbPath, FieldHnd, ObjectDataHandle);
  if Result then
      Dest := FieldHnd.RHeader.CurrentHeader;
end;

function TObjectDataManager.GetPathFieldPos(const dbPath: string): Int64;
begin
  if not GetPathField(dbPath, Result) then
      Result := 0;
end;

function TObjectDataManager.AddFile(const dbPath, DBItem, DBItemDescription, FileName: string): Boolean;
var
  DBItemHandle: TItemHandle;
begin
  Result := False;

  if ItemExists(dbPath, DBItem) then
    begin
      if not ItemOpen(dbPath, DBItem, DBItemHandle) then
        begin
          ItemClose(DBItemHandle);
          Exit;
        end;
    end
  else
    begin
      CreateDir(dbPath, DBItemDescription);
      if not ItemCreate(dbPath, DBItem, DBItemDescription, DBItemHandle) then
        begin
          ItemClose(DBItemHandle);
          Exit;
        end;
    end;

  if not ItemAddFile(DBItemHandle, FileName) then
    begin
      ItemClose(DBItemHandle);
      Exit;
    end;
  Result := True;
end;

function TObjectDataManager.PutToFile(const dbPath, DBItem, FileName: string): Boolean;
var
  DBItemHandle: TItemHandle;
begin
  Result := False;
  if not ItemOpen(dbPath, DBItem, DBItemHandle) then
    begin
      ItemClose(DBItemHandle);
      Exit;
    end;
  if not ItemGetFile(DBItemHandle, FileName) then
    begin
      ItemClose(DBItemHandle);
      Exit;
    end;
  Result := True;
end;

function TObjectDataManager.GetItemSize(const dbPath, DBItem: string): Int64;
var
  DBItemHandle: TItemHandle;
begin
  InitTTMDBItemHandle(DBItemHandle);
  if dbPack_GetItem(dbPath, DBItem, FDefaultItemID, DBItemHandle.Item, ObjectDataHandle) then
      Result := DBItemHandle.Item.Size
  else
      Result := 0;
end;

function TObjectDataManager.ItemAddFile(var ItemHnd: TItemHandle; const FileName: string): Boolean;
var
  RepaInt   : Integer;
  FileHandle: TRecFile;
  FileSize  : Int64;
  Buff      : array [0 .. MaxBuffSize] of umlChar;
begin
  Result := False;
  if not umlFileExists(FileName) then
      Exit;
  InitTRecFile(FileHandle);
  if not umlFileOpen(FileName, FileHandle, True) then
    begin
      umlFileClose(FileHandle);
      Exit;
    end;
  FileSize := FileHandle.Size;

  if not dbPack_ItemWrite(umlSizeLength, FileSize, ItemHnd, ObjectDataHandle) then
    begin
      umlFileClose(FileHandle);
      Exit;
    end;

  if FileSize > MaxBuffSize then
    begin
      for RepaInt := 1 to FileSize div MaxBuffSize do
        begin
          if not umlFileRead(FileHandle, MaxBuffSize, Buff) then
            begin
              umlFileClose(FileHandle);
              Exit;
            end;
          if not dbPack_ItemWrite(MaxBuffSize, Buff, ItemHnd, ObjectDataHandle) then
            begin
              umlFileClose(FileHandle);
              Exit;
            end;
        end;
      if (FileSize mod MaxBuffSize) > 0 then
        begin
          if not umlFileRead(FileHandle, FileSize mod MaxBuffSize, Buff) then
            begin
              umlFileClose(FileHandle);
              Exit;
            end;
          if not dbPack_ItemWrite(FileSize mod MaxBuffSize, Buff, ItemHnd, ObjectDataHandle) then
            begin
              umlFileClose(FileHandle);
              Exit;
            end;
        end;
    end
  else
    begin
      if FileSize > 0 then
        begin
          if not umlFileRead(FileHandle, FileSize, Buff) then
            begin
              umlFileClose(FileHandle);
              Exit;
            end;
          if not dbPack_ItemWrite(FileSize, Buff, ItemHnd, ObjectDataHandle) then
            begin
              umlFileClose(FileHandle);
              Exit;
            end;
        end;
    end;
  umlFileClose(FileHandle);
  Result := True;
end;

function TObjectDataManager.ItemAutoConnect(const dbPath, DBItem, DBItemDescription: string; var ItemHnd: TItemHandle): Boolean;
begin
  if ItemExists(dbPath, DBItem) then
      Result := ItemOpen(dbPath, DBItem, ItemHnd)
  else
      Result := ItemCreate(dbPath, DBItem, DBItemDescription, ItemHnd);
end;

function TObjectDataManager.ItemFastAutoConnect_F(const FieldPos: Int64; const DBItem, DBItemDescription: string; var ItemHnd: TItemHandle): Boolean;
var
  srHnd: TItemSearch;
begin
  if ItemFastFindFirst(FieldPos, DBItem, srHnd) then
    begin
      Result := ItemFastOpen(srHnd.HeaderPOS, ItemHnd);
    end
  else
    begin
      Result := ItemFastCreate(FieldPos, DBItem, DBItemDescription, ItemHnd);
    end;
end;

function TObjectDataManager.ItemFastAutoConnect_L(const FieldPos: Int64; const DBItem, DBItemDescription: string; var ItemHnd: TItemHandle): Boolean;
var
  srHnd: TItemSearch;
begin
  if ItemFastFindLast(FieldPos, DBItem, srHnd) then
    begin
      Result := ItemFastOpen(srHnd.HeaderPOS, ItemHnd);
    end
  else
    begin
      Result := ItemFastCreate(FieldPos, DBItem, DBItemDescription, ItemHnd);
    end;
end;

function TObjectDataManager.ItemUpdate(var ItemHnd: TItemHandle): Boolean;
begin
  Result := dbPack_ItemUpdate(ItemHnd, ObjectDataHandle);
end;

function TObjectDataManager.ItemClose(var ItemHnd: TItemHandle): Boolean;
begin
  Result := dbPack_ItemClose(ItemHnd, ObjectDataHandle);
end;

function TObjectDataManager.ItemReName(const FieldPos: Int64; var ItemHnd: TItemHandle; const aNewName, aNewDescription: string): Boolean;
begin
  Result := dbPack_ItemReName(FieldPos, aNewName, aNewDescription, ItemHnd, ObjectDataHandle);
end;

function TObjectDataManager.ItemCopyTo(var ItemHnd: TItemHandle; DestDB: TObjectDataManager; var DestItemHandle: TItemHandle; const CopySize: Int64): Boolean;
var
  RepaInt: Integer;
  Buff   : array [0 .. MaxBuffSize] of umlChar;
begin
  Result := False;
  if CopySize > MaxBuffSize then
    begin
      for RepaInt := 1 to (CopySize div MaxBuffSize) do
        begin
          if not ItemRead(ItemHnd, MaxBuffSize, Buff) then
              Exit;
          if not DestDB.ItemWrite(DestItemHandle, MaxBuffSize, Buff) then
              Exit;
        end;
      if (CopySize mod MaxBuffSize) > 0 then
        begin
          if not ItemRead(ItemHnd, CopySize mod MaxBuffSize, Buff) then
              Exit;
          if not DestDB.ItemWrite(DestItemHandle, CopySize mod MaxBuffSize, Buff) then
              Exit;
        end;
    end
  else
    begin
      if CopySize > 0 then
        begin
          if not ItemRead(ItemHnd, CopySize, Buff) then
              Exit;
          if not DestDB.ItemWrite(DestItemHandle, CopySize, Buff) then
              Exit;
        end;
    end;
  Result := True;
end;

function TObjectDataManager.ItemCreate(const dbPath, DBItem, DBItemDescription: string; var ItemHnd: TItemHandle): Boolean;
{
  It can automatically create a path
}
begin
  InitTTMDBItemHandle(ItemHnd);
  Result := dbPack_ItemCreate(dbPath, DBItem, DBItemDescription, FDefaultItemID, ItemHnd, ObjectDataHandle);
end;

function TObjectDataManager.ItemDelete(const dbPath, DBItem: string): Boolean;
begin
  Result := dbPack_DeleteItem(dbPath, DBItem, FDefaultItemID, ObjectDataHandle);
end;

function TObjectDataManager.ItemExists(const dbPath, DBItem: string): Boolean;
var
  ItemSearchHnd: TItemSearch;
begin
  InitTTMDBSearchItem(ItemSearchHnd);
  Result := dbPack_FindFirstItem(dbPath, DBItem, FDefaultItemID, ItemSearchHnd, ObjectDataHandle);
end;

function TObjectDataManager.ItemFastInsertNew(const FieldPos, InsertHeaderPos: Int64; const DBItem, DBItemDescription: string; var ItemHnd: TItemHandle): Boolean;
begin
  InitTTMDBItemHandle(ItemHnd);
  Result := dbPack_ItemFastInsertNew(DBItem, DBItemDescription, FieldPos, InsertHeaderPos, FDefaultItemID, ItemHnd, ObjectDataHandle);
end;

function TObjectDataManager.ItemFastCreate(const aPos: Int64; const DBItem, DBItemDescription: string; var ItemHnd: TItemHandle): Boolean;
begin
  InitTTMDBItemHandle(ItemHnd);
  Result := dbPack_ItemFastCreate(DBItem, DBItemDescription, aPos, FDefaultItemID, ItemHnd, ObjectDataHandle);
end;

function TObjectDataManager.ItemFastExists(const FieldPos: Int64; const DBItem: string): Boolean;
var
  ItemSearchHnd: TItemSearch;
begin
  InitTTMDBSearchItem(ItemSearchHnd);
  Result := dbPack_FastFindFirstItem(FieldPos, DBItem, FDefaultItemID, ItemSearchHnd, ObjectDataHandle);
end;

function TObjectDataManager.ItemFastFindFirst(const FieldPos: Int64; const DBItem: string; var ItemSearchHandle: TItemSearch): Boolean;
begin
  InitTTMDBSearchItem(ItemSearchHandle);
  Result := dbPack_FastFindFirstItem(FieldPos, DBItem, FDefaultItemID, ItemSearchHandle, ObjectDataHandle);
end;

function TObjectDataManager.ItemFastFindLast(const FieldPos: Int64; const DBItem: string; var ItemSearchHandle: TItemSearch): Boolean;
begin
  InitTTMDBSearchItem(ItemSearchHandle);
  Result := dbPack_FastFindLastItem(FieldPos, DBItem, FDefaultItemID, ItemSearchHandle, ObjectDataHandle);
end;

function TObjectDataManager.ItemFastFindNext(var ItemSearchHandle: TItemSearch): Boolean;
begin
  Result := dbPack_FastFindNextItem(ItemSearchHandle, FDefaultItemID, ObjectDataHandle);
end;

function TObjectDataManager.ItemFastFindPrev(var ItemSearchHandle: TItemSearch): Boolean;
begin
  Result := dbPack_FastFindPrevItem(ItemSearchHandle, FDefaultItemID, ObjectDataHandle);
end;

function TObjectDataManager.ItemFastOpen(const aPos: Int64; var ItemHnd: TItemHandle): Boolean;
begin
  InitTTMDBItemHandle(ItemHnd);
  Result := dbPack_ItemFastOpen(aPos, FDefaultItemID, ItemHnd, ObjectDataHandle);
end;

function TObjectDataManager.ItemFastResetBody(const aPos: Int64): Boolean;
var
  ItemHnd: TItemHandle;
begin
  Result := ItemFastOpen(aPos, ItemHnd)
    and dbPack_ItemBodyReset(ItemHnd, ObjectDataHandle);
end;

function TObjectDataManager.ItemFindFirst(const dbPath, DBItem: string; var ItemSearchHandle: TItemSearch): Boolean;
begin
  InitTTMDBSearchItem(ItemSearchHandle);
  Result := dbPack_FindFirstItem(dbPath, DBItem, FDefaultItemID, ItemSearchHandle, ObjectDataHandle);
end;

function TObjectDataManager.ItemFindLast(const dbPath, DBItem: string; var ItemSearchHandle: TItemSearch): Boolean;
begin
  InitTTMDBSearchItem(ItemSearchHandle);
  Result := dbPack_FindLastItem(dbPath, DBItem, FDefaultItemID, ItemSearchHandle, ObjectDataHandle);
end;

function TObjectDataManager.ItemFindNext(var ItemSearchHandle: TItemSearch): Boolean;
begin
  Result := dbPack_FindNextItem(ItemSearchHandle, FDefaultItemID, ObjectDataHandle);
end;

function TObjectDataManager.ItemFindPrev(var ItemSearchHandle: TItemSearch): Boolean;
begin
  Result := dbPack_FindPrevItem(ItemSearchHandle, FDefaultItemID, ObjectDataHandle);
end;

function TObjectDataManager.ItemGetFile(var ItemHnd: TItemHandle; const FileName: string): Boolean;
var
  RepaInt   : Integer;
  FileHandle: TRecFile;
  FileSize  : Int64;
  Buff      : array [0 .. MaxBuffSize] of umlChar;
begin
  Result := False;
  InitTRecFile(FileHandle);
  if not umlFileCreate(FileName, FileHandle) then
    begin
      umlFileClose(FileHandle);
      Exit;
    end;

  if not dbPack_ItemRead(umlSizeLength, FileSize, ItemHnd, ObjectDataHandle) then
    begin
      umlFileClose(FileHandle);
      Exit;
    end;

  if FileSize > MaxBuffSize then
    begin
      for RepaInt := 1 to FileSize div MaxBuffSize do
        begin
          if not dbPack_ItemRead(MaxBuffSize, Buff, ItemHnd, ObjectDataHandle) then
            begin
              umlFileClose(FileHandle);
              Exit;
            end;
          if not umlFileWrite(FileHandle, MaxBuffSize, Buff) then
            begin
              umlFileClose(FileHandle);
              Exit;
            end;
        end;
      if (FileSize mod MaxBuffSize) > 0 then
        begin
          if not dbPack_ItemRead(FileSize mod MaxBuffSize, Buff, ItemHnd, ObjectDataHandle) then
            begin
              umlFileClose(FileHandle);
              Exit;
            end;
          if not umlFileWrite(FileHandle, FileSize mod MaxBuffSize, Buff) then
            begin
              umlFileClose(FileHandle);
              Exit;
            end;
        end;
    end
  else
    begin
      if FileSize > 0 then
        begin
          if not dbPack_ItemRead(FileSize, Buff, ItemHnd, ObjectDataHandle) then
            begin
              umlFileClose(FileHandle);
              Exit;
            end;
          if not umlFileWrite(FileHandle, FileSize, Buff) then
            begin
              umlFileClose(FileHandle);
              Exit;
            end;
        end;
    end;
  umlFileClose(FileHandle);
  Result := True;
end;

function TObjectDataManager.ItemMove(const dbPath, ItemName, DestPath: string): Boolean;
begin
  Result := dbPack_MoveItem(dbPath, ItemName, DestPath, FDefaultItemID, ObjectDataHandle);
end;

function TObjectDataManager.ItemOpen(const dbPath, DBItem: string; var ItemHnd: TItemHandle): Boolean;
begin
  InitTTMDBItemHandle(ItemHnd);
  Result := dbPack_ItemOpen(dbPath, DBItem, FDefaultItemID, ItemHnd, ObjectDataHandle);
end;

function TObjectDataManager.ItemRead(var ItemHnd: TItemHandle; const aSize: Int64; var Buffers): Boolean;
begin
  Result := dbPack_ItemRead(aSize, Buffers, ItemHnd, ObjectDataHandle);
end;

function TObjectDataManager.ItemReadBool(var ItemHnd: TItemHandle; var Value: Boolean): Boolean;
var
  TempValue: Byte;
begin
  TempValue := 0;
  Result := dbPack_ItemRead(1, TempValue, ItemHnd, ObjectDataHandle);
  Value := (TempValue = 1);
end;

function TObjectDataManager.ItemReadInt(var ItemHnd: TItemHandle; var Value: Integer): Boolean;
begin
  Result := dbPack_ItemRead(umlIntegerLength, Value, ItemHnd, ObjectDataHandle);
end;

function TObjectDataManager.ItemReadReturnBool(var ItemHnd: TItemHandle; const DefaultValue: Boolean): Boolean;
var
  TempValue: Byte;
begin
  TempValue := 0;
  if dbPack_ItemRead(1, TempValue, ItemHnd, ObjectDataHandle) then
      Result := (TempValue = 1)
  else
      Result := DefaultValue;
end;

function TObjectDataManager.ItemReadReturnInt(var ItemHnd: TItemHandle; const DefaultValue: Integer): Integer;
var
  TempInt: Integer;
begin
  TempInt := DefaultValue;
  if dbPack_ItemRead(umlIntegerLength, TempInt, ItemHnd, ObjectDataHandle) then
      Result := TempInt
  else
      Result := DefaultValue;
end;

function TObjectDataManager.ItemReadReturnStr(var ItemHnd: TItemHandle; const DefaultValue: string): string;
var
  TempStr: umlString;
begin
  TempStr := DefaultValue;
  if dbPack_ItemReadStr(TempStr, ItemHnd, ObjectDataHandle) then
      Result := TempStr
  else
      Result := DefaultValue;
end;

function TObjectDataManager.ItemReadStr(var ItemHnd: TItemHandle; var Value: string): Boolean;
var
  TempStr: umlString;
begin
  Result := dbPack_ItemReadStr(TempStr, ItemHnd, ObjectDataHandle);
  if Result then
      Value := TempStr;
end;

function TObjectDataManager.ItemReadTime(var ItemHnd: TItemHandle; var Value: TDateTime): Boolean;
var
  TempValue: Double;
begin
  if dbPack_ItemRead(umlDoubleLength, TempValue, ItemHnd, ObjectDataHandle) then
    begin
      try
          Value := TempValue;
      except
      end;
      Result := True;
    end
  else
    begin
      Result := False;
    end;
end;

function TObjectDataManager.ItemSeekStart(var ItemHnd: TItemHandle): Boolean;
begin
  Result := dbPack_ItemSeekStartPos(ItemHnd, ObjectDataHandle);
end;

function TObjectDataManager.ItemSeekLast(var ItemHnd: TItemHandle): Boolean;
begin
  Result := dbPack_ItemSeekLastPos(ItemHnd, ObjectDataHandle);
end;

function TObjectDataManager.ItemSeek(var ItemHnd: TItemHandle; const ItemOffset: Int64): Boolean;
var
  aSize: Integer;
begin
  aSize := dbPack_ItemGetSize(ItemHnd, ObjectDataHandle);
  if ItemOffset > aSize then
      Result := dbPack_AppendItemSize(ItemHnd, ItemOffset - aSize, ObjectDataHandle)
  else if ItemOffset = aSize then
      Result := dbPack_ItemSeekLastPos(ItemHnd, ObjectDataHandle)
  else if ItemOffset = 0 then
      Result := dbPack_ItemSeekStartPos(ItemHnd, ObjectDataHandle)
  else
      Result := dbPack_ItemSeekPos(ItemOffset, ItemHnd, ObjectDataHandle);
end;

function TObjectDataManager.ItemGetPos(var ItemHnd: TItemHandle): Int64;
begin
  Result := dbPack_ItemGetPos(ItemHnd, ObjectDataHandle);
end;

function TObjectDataManager.ItemGetSize(var ItemHnd: TItemHandle): Int64;
begin
  Result := dbPack_ItemGetSize(ItemHnd, ObjectDataHandle);
end;

function TObjectDataManager.ItemWrite(var ItemHnd: TItemHandle; const aSize: Int64; var Buffers): Boolean;
begin
  Result := dbPack_ItemWrite(aSize, Buffers, ItemHnd, ObjectDataHandle);
end;

function TObjectDataManager.ItemWriteBool(var ItemHnd: TItemHandle; const Value: Boolean): Boolean;
var
  TempValue: Byte;
begin
  if Value then
      TempValue := 1
  else
      TempValue := 0;
  Result := dbPack_ItemWrite(1, TempValue, ItemHnd, ObjectDataHandle);
end;

function TObjectDataManager.ItemWriteInt(var ItemHnd: TItemHandle; const Value: Integer): Boolean;
var
  TempValue: Integer;
begin
  TempValue := Value;
  Result := dbPack_ItemWrite(umlIntegerLength, TempValue, ItemHnd, ObjectDataHandle);
end;

function TObjectDataManager.ItemWriteStr(var ItemHnd: TItemHandle; const Value: string): Boolean;
begin
  Result := dbPack_ItemWriteStr(Value, ItemHnd, ObjectDataHandle);
end;

function TObjectDataManager.ItemWriteTime(var ItemHnd: TItemHandle; const Value: TDateTime): Boolean;
var
  TempValue: Double;
begin
  TempValue := Value;
  Result := dbPack_ItemWrite(umlDoubleLength, TempValue, ItemHnd, ObjectDataHandle);
end;

function TObjectDataManager.ReadBool(const Section, Item: string; const Value: Boolean): Boolean;
begin
  if Value then
      Result := ReadString(Section, Item, '1') = '1'
  else
      Result := ReadString(Section, Item, '0') = '1';
end;

function TObjectDataManager.ReadString(const Section, Item, Value: string): string;
var
  TempStr     : string;
  DBItemHandle: TItemHandle;
begin
  Result := Value;
  TempStr := Value;
  if not ItemOpen(Section, Item, DBItemHandle) then
    begin
      ItemClose(DBItemHandle);
      Exit;
    end;
  if not ItemReadStr(DBItemHandle, TempStr) then
    begin
      ItemClose(DBItemHandle);
      Exit;
    end;
  ItemClose(DBItemHandle);
  Result := TempStr;
end;

function TObjectDataManager.WriteBool(const Section, Item: string; const Value: Boolean): Boolean;
begin
  if Value then
      Result := WriteString(Section, Item, '1')
  else
      Result := WriteString(Section, Item, '0');
end;

function TObjectDataManager.WriteString(const Section, Item, Value: string): Boolean;
var
  DBItemHandle: TItemHandle;
begin
  Result := False;

  if not ItemExists(Section, Item) then
    begin
      if not ItemCreate(Section, Item, 'Section.Database', DBItemHandle) then
        begin
          ItemClose(DBItemHandle);
          Exit;
        end;
    end
  else
    begin
      if not ItemOpen(Section, Item, DBItemHandle) then
        begin
          ItemClose(DBItemHandle);
          Exit;
        end;
    end;
  if not ItemWriteStr(DBItemHandle, Value) then
    begin
      ItemClose(DBItemHandle);
      Exit;
    end;
  ItemClose(DBItemHandle);
  Result := True;
end;

function TObjectDataManager.RecursionSearchFirst(const InitPath, MaskName: string; var RecursionSearchHnd: TItemRecursionSearch): Boolean;
begin
  InitTTMDBRecursionSearch(RecursionSearchHnd);
  Result := dbPack_RecursionSearchFirst(InitPath, MaskName, RecursionSearchHnd, ObjectDataHandle);
end;

function TObjectDataManager.RecursionSearchNext(var RecursionSearchHnd: TItemRecursionSearch): Boolean;
begin
  Result := dbPack_RecursionSearchNext(RecursionSearchHnd, ObjectDataHandle);
end;

function TObjectDataMarshal.GetItems(aIndex: Integer): TObjectDataManager;
begin
  Result := TObjectDataManager(FLibList.Objects[aIndex]);
end;

function TObjectDataMarshal.GetNames(AName: string): TObjectDataManager;
var
  RepaInt: Integer;
  aUName : string;
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
            if umlMultipleMatch(False, aUName, FLibList[RepaInt]) then
              begin
                Result := TObjectDataManager(FLibList.Objects[RepaInt]);
                Exit;
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

constructor TObjectDataMarshal.Create(aID: Byte);
begin
  inherited Create;
  FID := aID;
  FLibList := TCoreClassStringList.Create;
  FUseWildcard := True;
end;

destructor TObjectDataMarshal.Destroy;
begin
  Clear;
  DisposeObject(FLibList);
  inherited Destroy;
end;

function TObjectDataMarshal.GetAbsoluteFileName(aFileName: string): string;
begin
  Result := umlUpperCase(aFileName).Text;
end;

function TObjectDataMarshal.NewDB(aFile: string; aOnlyRead: Boolean): TObjectDataManager;
var
  RepaInt: Integer;
  aUName : string;
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

function TObjectDataMarshal.Open(aFile: string; aOnlyRead: Boolean): TObjectDataManager;
var
  RepaInt: Integer;
  aUName : string;
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
        Inc(i);
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

procedure TObjectDataMarshal.DeleteFromName(AName: string);
var
  RepaInt: Integer;
  aUName : string;
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
                  Inc(RepaInt);
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
                      Inc(RepaInt);
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
                      Inc(RepaInt);
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

if _ObjectDataMarshal <> nil then
  begin
    DisposeObject(_ObjectDataMarshal);
    _ObjectDataMarshal := nil;
  end;

end.
