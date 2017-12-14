{ ****************************************************************************** }
{ * ObjectDBManager                                                            * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
(* https://github.com/PassByYou888/ZServer4D *)
{ ****************************************************************************** }
(*
  update history
*)

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
    FStreamEngine                                                  : TCoreClassStream;
    FObjectDataHandle                                              : TTMDB;
    FNeedCreateNew, FOverWriteItem, FAllowSameHeaderName, FOnlyRead: Boolean;
    FObjectName                                                    : SystemString;
    FDefaultItemID                                                 : Byte;
    FIsOpened                                                      : Boolean;
    FData                                                          : Pointer;

    function GetAutoFreeHandle: Boolean;
    procedure SetAutoFreeHandle(const Value: Boolean);
  protected
    function GetOverWriteItem: Boolean;
    function GetAllowSameHeaderName: Boolean;
    function GetDBTime: TDateTime;
    procedure SetOverWriteItem(Value: Boolean);
    procedure SetAllowSameHeaderName(Value: Boolean);
  public
    constructor Create(const aObjectName: SystemString; const aID: Byte; aOnlyRead: Boolean);
    constructor CreateNew(const aObjectName: SystemString; const aID: Byte);
    constructor CreateAsStream(aStream: TCoreClassStream; const aObjectName: SystemString; const aID: Byte; aOnlyRead, aIsNewData, aAutoFreeHandle: Boolean);
    destructor Destroy; override;
    function Open: Boolean;
    function NewHandle(aStream: TCoreClassStream; const aObjectName: SystemString; const aID: Byte; aOnlyRead, aIsNew: Boolean): Boolean;
    function CopyTo(DestDB: TObjectDataManager): Boolean;
    function CopyToPath(DestDB: TObjectDataManager; DestPath: SystemString): Boolean;
    function CopyFieldToPath(FieldPos: Int64; DestDB: TObjectDataManager; DestPath: SystemString): Boolean;
    procedure SaveToStream(Stream: TCoreClassStream);
    procedure ImpFromPath(ImpPath, dbPath: SystemString; IncludeSub: Boolean);
    procedure ImpFromFiles(ImpFiles: TCoreClassStrings; dbPath: SystemString);
    function isAbort: Boolean;
    function Close: Boolean;
    function ErrorNo: Int64;
    function Modify: Boolean;
    function Size: Int64;
    procedure SetID(const ID: Byte);
    procedure Update;

    function CreateField(const DirName, DirDescription: SystemString): Boolean;
    function CreateRootField(const RootName: SystemString): Boolean;
    function DirectoryExists(const DirName: SystemString): Boolean;
    function FastDelete(const FieldPos: Int64; const aPos: Int64): Boolean;
    function FastFieldExists(const FieldPos: Int64; const FieldName: SystemString): Boolean;
    function FastFieldCreate(const FieldPos: Int64; const FieldName, FieldDescription: SystemString; var NewFieldPos: Int64): Boolean;
    function SetRootField(const RootName: SystemString): Boolean;
    function FieldReName(const FieldPos: Int64; const NewFieldName, NewFieldDescription: SystemString): Boolean;
    function RootField: Int64;
    function FieldDelete(const dbPath: SystemString; const FieldName: SystemString): Boolean;
    function FieldExists(const dbPath: SystemString; const FieldName: SystemString): Boolean;
    function FieldFastFindFirst(const FieldPos: Int64; const Filter: SystemString; var FieldSearchHandle: TFieldSearch): Boolean;
    function FieldFastFindLast(const FieldPos: Int64; const Filter: SystemString; var FieldSearchHandle: TFieldSearch): Boolean;
    function FieldFastFindNext(var FieldSearchHandle: TFieldSearch): Boolean;
    function FieldFastFindPrev(var FieldSearchHandle: TFieldSearch): Boolean;
    function FieldFindFirst(const dbPath, Filter: SystemString; var FieldSearchHandle: TFieldSearch): Boolean;
    function FieldFindLast(const dbPath, Filter: SystemString; var FieldSearchHandle: TFieldSearch): Boolean;
    function FieldFindNext(var FieldSearchHandle: TFieldSearch): Boolean;
    function FieldFindPrev(var FieldSearchHandle: TFieldSearch): Boolean;
    function FieldMove(const dbPath, FieldName, DestPath: SystemString): Boolean;
    function GetFieldData(const FieldPos: Int64; var Dest: TFieldHandle): Boolean;
    function GetFieldPath(const FieldPos: Int64): SystemString;
    function GetPathField(const dbPath: SystemString; var Dest: Int64): Boolean;
    function GetPathFieldPos(const dbPath: SystemString): Int64;
    function GetHeaderLastModifyTime(const hPos: Int64): TDateTime;

    function AddFile(const dbPath, DBItem, DBItemDescription, FileName: SystemString): Boolean;
    function PutToFile(const dbPath, DBItem, FileName: SystemString): Boolean;
    function GetItemSize(const dbPath, DBItem: SystemString): Int64;

    // fast lowlevel
    function GetFirstHeaderFromField(FieldPos: Int64; var h: THeader): Boolean; inline;
    function GetLastHeaderFromField(FieldPos: Int64; var h: THeader): Boolean; inline;
    function GetHeader(hPos: Int64; var h: THeader): Boolean; inline;

    function ItemAddFile(var ItemHnd: TItemHandle; const FileName: SystemString): Boolean;
    function ItemAutoConnect(const dbPath, DBItem, DBItemDescription: SystemString; var ItemHnd: TItemHandle): Boolean;
    function ItemFastAutoConnect_F(const FieldPos: Int64; const DBItem, DBItemDescription: SystemString; var ItemHnd: TItemHandle): Boolean;
    function ItemFastAutoConnect_L(const FieldPos: Int64; const DBItem, DBItemDescription: SystemString; var ItemHnd: TItemHandle): Boolean;
    function ItemUpdate(var ItemHnd: TItemHandle): Boolean;
    function ItemClose(var ItemHnd: TItemHandle): Boolean;
    function ItemReName(const FieldPos: Int64; var ItemHnd: TItemHandle; const aNewName, aNewDescription: SystemString): Boolean;
    function ItemCopyTo(var ItemHnd: TItemHandle; DestDB: TObjectDataManager; var DestItemHandle: TItemHandle; const CopySize: Int64): Boolean;
    function ItemCreate(const dbPath, DBItem, DBItemDescription: SystemString; var ItemHnd: TItemHandle): Boolean;
    function ItemDelete(const dbPath, DBItem: SystemString): Boolean;
    function ItemExists(const dbPath, DBItem: SystemString): Boolean;
    function ItemFastInsertNew(const FieldPos, InsertHeaderPos: Int64; const DBItem, DBItemDescription: SystemString; var ItemHnd: TItemHandle): Boolean;
    function ItemFastCreate(const aPos: Int64; const DBItem, DBItemDescription: SystemString; var ItemHnd: TItemHandle): Boolean;
    function ItemFastExists(const FieldPos: Int64; const DBItem: SystemString): Boolean;
    function ItemFastFindFirst(const FieldPos: Int64; const DBItem: SystemString; var ItemSearchHandle: TItemSearch): Boolean;
    function ItemFastFindLast(const FieldPos: Int64; const DBItem: SystemString; var ItemSearchHandle: TItemSearch): Boolean;
    function ItemFastFindNext(var ItemSearchHandle: TItemSearch): Boolean;
    function ItemFastFindPrev(var ItemSearchHandle: TItemSearch): Boolean;
    function ItemFastOpen(const aPos: Int64; var ItemHnd: TItemHandle): Boolean; inline;
    function ItemFastResetBody(const aPos: Int64): Boolean;
    function ItemFindFirst(const dbPath, DBItem: SystemString; var ItemSearchHandle: TItemSearch): Boolean; inline;
    function ItemFindLast(const dbPath, DBItem: SystemString; var ItemSearchHandle: TItemSearch): Boolean; inline;
    function ItemFindNext(var ItemSearchHandle: TItemSearch): Boolean; inline;
    function ItemFindPrev(var ItemSearchHandle: TItemSearch): Boolean; inline;
    function ItemGetFile(var ItemHnd: TItemHandle; const FileName: SystemString): Boolean;
    function ItemMove(const dbPath, ItemName, DestPath: SystemString): Boolean;
    function ItemOpen(const dbPath, DBItem: SystemString; var ItemHnd: TItemHandle): Boolean;

    function ItemRead(var ItemHnd: TItemHandle; const aSize: Int64; var Buffers): Boolean; inline;
    function ItemReadBool(var ItemHnd: TItemHandle; var Value: Boolean): Boolean; inline;
    function ItemReadInt(var ItemHnd: TItemHandle; var Value: Integer): Boolean; inline;
    function ItemReadReturnBool(var ItemHnd: TItemHandle; const DefaultValue: Boolean): Boolean; inline;
    function ItemReadReturnInt(var ItemHnd: TItemHandle; const DefaultValue: Integer): Integer; inline;
    function ItemReadReturnStr(var ItemHnd: TItemHandle; const DefaultValue: SystemString): SystemString; inline;
    function ItemReadStr(var ItemHnd: TItemHandle; var Value: SystemString): Boolean; inline;
    function ItemReadTime(var ItemHnd: TItemHandle; var Value: TDateTime): Boolean; inline;

    function ItemSeekStart(var ItemHnd: TItemHandle): Boolean; inline;
    function ItemSeekLast(var ItemHnd: TItemHandle): Boolean; inline;
    function ItemSeek(var ItemHnd: TItemHandle; const ItemOffset: Int64): Boolean; inline;
    function ItemGetPos(var ItemHnd: TItemHandle): Int64; inline;
    function ItemGetSize(var ItemHnd: TItemHandle): Int64; inline;

    function ItemWrite(var ItemHnd: TItemHandle; const aSize: Int64; var Buffers): Boolean; inline;
    function ItemWriteBool(var ItemHnd: TItemHandle; const Value: Boolean): Boolean; inline;
    function ItemWriteInt(var ItemHnd: TItemHandle; const Value: Integer): Boolean; inline;
    function ItemWriteStr(var ItemHnd: TItemHandle; const Value: SystemString): Boolean; inline;
    function ItemWriteTime(var ItemHnd: TItemHandle; const Value: TDateTime): Boolean; inline;
    function ReadBool(const Section, Item: SystemString; const Value: Boolean): Boolean; inline;
    function ReadString(const Section, Item, Value: SystemString): SystemString; inline;
    function WriteBool(const Section, Item: SystemString; const Value: Boolean): Boolean; inline;
    function WriteString(const Section, Item, Value: SystemString): Boolean; inline;

    function RecursionSearchFirst(const InitPath, MaskName: SystemString; var RecursionSearchHnd: TItemRecursionSearch): Boolean;
    function RecursionSearchNext(var RecursionSearchHnd: TItemRecursionSearch): Boolean;

    function ObjectDataHandlePtr: PTMDB; inline;

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

  TObjectDataMarshal = class(TCoreClassObject)
  private
    FID         : Byte;
    FLibList    : TCoreClassStrings;
    FUseWildcard: Boolean;
    function GetItems(aIndex: Integer): TObjectDataManager;
    function GetNames(AName: SystemString): TObjectDataManager;
    procedure SetItems(aIndex: Integer; const Value: TObjectDataManager);
  public
    constructor Create(aID: Byte);
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
      Result := FObjectDataHandle.IOHnd.AutoFree
  else
      Result := False;
end;

procedure TObjectDataManager.SetAutoFreeHandle(const Value: Boolean);
begin
  if not isAbort then
      FObjectDataHandle.IOHnd.AutoFree := Value;
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

constructor TObjectDataManager.Create(const aObjectName: SystemString; const aID: Byte; aOnlyRead: Boolean);
begin
  inherited Create;
  NewHandle(nil, aObjectName, aID, aOnlyRead, False);
end;

constructor TObjectDataManager.CreateNew(const aObjectName: SystemString; const aID: Byte);
begin
  inherited Create;
  NewHandle(nil, aObjectName, aID, False, True);
end;

constructor TObjectDataManager.CreateAsStream(aStream: TCoreClassStream; const aObjectName: SystemString; const aID: Byte; aOnlyRead, aIsNewData, aAutoFreeHandle: Boolean);
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

function TObjectDataManager.NewHandle(aStream: TCoreClassStream; const aObjectName: SystemString; const aID: Byte; aOnlyRead, aIsNew: Boolean): Boolean;
begin
  Close;
  Init_TTMDB(FObjectDataHandle);
  FStreamEngine := aStream;
  FObjectName := aObjectName;
  FNeedCreateNew := aIsNew;
  FOnlyRead := aOnlyRead;
  FDefaultItemID := aID;
  FIsOpened := Open();
  Result := FIsOpened;

  OverWriteItem := True;
  AllowSameHeaderName := False;
  AutoFreeHandle := True;
  FData := nil;
end;

function TObjectDataManager.CopyTo(DestDB: TObjectDataManager): Boolean;
begin
  Result := dbPack_CopyAllTo(FObjectDataHandle, DestDB.FObjectDataHandle);
end;

function TObjectDataManager.CopyToPath(DestDB: TObjectDataManager; DestPath: SystemString): Boolean;
begin
  Result := dbPack_CopyAllToDestPath(FObjectDataHandle, DestDB.FObjectDataHandle, DestPath);
end;

function TObjectDataManager.CopyFieldToPath(FieldPos: Int64; DestDB: TObjectDataManager; DestPath: SystemString): Boolean;
var
  DestFieldPos: Int64;
begin
  Result := False;
  CreateField(DestPath, '');
  if GetPathField(DestPath, DestFieldPos) then
      Result := dbPack_CopyFieldTo('*', FObjectDataHandle, FieldPos, DestDB.FObjectDataHandle, DestFieldPos);
end;

procedure TObjectDataManager.SaveToStream(Stream: TCoreClassStream);
var
  e: TObjectDataManager;
begin
  e := TObjectDataManager.CreateAsStream(Stream, ObjectName, DefaultItemID, False, True, False);
  CopyTo(e);
  DisposeObject(e);
end;

procedure TObjectDataManager.ImpFromPath(ImpPath, dbPath: SystemString; IncludeSub: Boolean);
var
  fAry     : umlStringDynArray;
  n        : SystemString;
  fPos     : Int64;
  fs       : TCoreClassFileStream;
  itmHnd   : TItemHandle;
  itmStream: TItemStreamEngine;
begin
  dbPath := umlCharReplace(dbPath, '\', '/').Text;
  if not DirectoryExists(dbPath) then
      CreateField(dbPath, '');
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

procedure TObjectDataManager.ImpFromFiles(ImpFiles: TCoreClassStrings; dbPath: SystemString);
var
  i        : Integer;
  n        : SystemString;
  fPos     : Int64;
  fs       : TCoreClassFileStream;
  itmHnd   : TItemHandle;
  itmStream: TItemStreamEngine;
begin
  dbPath := umlCharReplace(dbPath, '\', '/').Text;
  if not DirectoryExists(dbPath) then
      CreateField(dbPath, '');
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

function TObjectDataManager.FastDelete(const FieldPos: Int64; const aPos: Int64): Boolean;
var
  FieldHnd: TFieldHandle;
begin
  Init_TField(FieldHnd);
  Result := False;
  if dbField_ReadRec(FieldPos, FObjectDataHandle.IOHnd, FieldHnd) then
      Result := dbField_DeleteHeader(aPos, FieldPos, FObjectDataHandle.IOHnd, FieldHnd);
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

function TObjectDataManager.FieldDelete(const dbPath: SystemString; const FieldName: SystemString): Boolean;
begin
  Result := dbPack_DeleteField(dbPath, FieldName, FObjectDataHandle);
end;

function TObjectDataManager.FieldExists(const dbPath: SystemString; const FieldName: SystemString): Boolean;
var
  FieldSearch: TFieldSearch;
begin
  Result := FieldFindFirst(dbPath, FieldName, FieldSearch);
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

function TObjectDataManager.FieldFindFirst(const dbPath, Filter: SystemString; var FieldSearchHandle: TFieldSearch): Boolean;
begin
  Init_TTMDBSearchField(FieldSearchHandle);
  Result := dbPack_FindFirstField(dbPath, Filter, FieldSearchHandle, FObjectDataHandle);
end;

function TObjectDataManager.FieldFindLast(const dbPath, Filter: SystemString; var FieldSearchHandle: TFieldSearch): Boolean;
begin
  Init_TTMDBSearchField(FieldSearchHandle);
  Result := dbPack_FindLastField(dbPath, Filter, FieldSearchHandle, FObjectDataHandle);
end;

function TObjectDataManager.FieldFindNext(var FieldSearchHandle: TFieldSearch): Boolean;
begin
  Result := dbPack_FindNextField(FieldSearchHandle, FObjectDataHandle);
end;

function TObjectDataManager.FieldFindPrev(var FieldSearchHandle: TFieldSearch): Boolean;
begin
  Result := dbPack_FindPrevField(FieldSearchHandle, FObjectDataHandle);
end;

function TObjectDataManager.FieldMove(const dbPath, FieldName, DestPath: SystemString): Boolean;
begin
  Result := dbPack_MoveField(dbPath, FieldName, DestPath, FObjectDataHandle);
end;

function TObjectDataManager.GetFieldData(const FieldPos: Int64; var Dest: TFieldHandle): Boolean;
begin
  Init_TField(Dest);
  Result := dbField_ReadRec(FieldPos, FObjectDataHandle.IOHnd, Dest);
end;

function TObjectDataManager.GetFieldPath(const FieldPos: Int64): SystemString;
var
  ReturnPath: umlString;
begin
  if dbPack_GetPath(FieldPos, FObjectDataHandle.DefaultFieldPOS, FObjectDataHandle, ReturnPath) then
      Result := ReturnPath
  else
      Result := '';
end;

function TObjectDataManager.GetPathField(const dbPath: SystemString; var Dest: Int64): Boolean;
var
  FieldHnd: TFieldHandle;
begin
  Result := dbPack_GetField(dbPath, FieldHnd, FObjectDataHandle);
  if Result then
      Dest := FieldHnd.RHeader.CurrentHeader;
end;

function TObjectDataManager.GetPathFieldPos(const dbPath: SystemString): Int64;
begin
  if not GetPathField(dbPath, Result) then
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

function TObjectDataManager.AddFile(const dbPath, DBItem, DBItemDescription, FileName: SystemString): Boolean;
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
      CreateField(dbPath, DBItemDescription);
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

function TObjectDataManager.PutToFile(const dbPath, DBItem, FileName: SystemString): Boolean;
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

function TObjectDataManager.GetItemSize(const dbPath, DBItem: SystemString): Int64;
var
  DBItemHandle: TItemHandle;
begin
  Init_TTMDBItemHandle(DBItemHandle);
  if dbPack_GetItem(dbPath, DBItem, FDefaultItemID, DBItemHandle.Item, FObjectDataHandle) then
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

function TObjectDataManager.ItemAddFile(var ItemHnd: TItemHandle; const FileName: SystemString): Boolean;
var
  RepaInt   : Integer;
  FileHandle: TIOHnd;
  FileSize  : Int64;
  Buff      : array [0 .. MaxBuffSize] of umlChar;
begin
  Result := False;
  if not umlFileExists(FileName) then
      Exit;
  InitIOHnd(FileHandle);
  if not umlFileOpen(FileName, FileHandle, True) then
    begin
      umlFileClose(FileHandle);
      Exit;
    end;
  FileSize := FileHandle.Size;

  if not dbPack_ItemWrite(umlSizeLength, FileSize, ItemHnd, FObjectDataHandle) then
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
          if not dbPack_ItemWrite(MaxBuffSize, Buff, ItemHnd, FObjectDataHandle) then
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
          if not dbPack_ItemWrite(FileSize mod MaxBuffSize, Buff, ItemHnd, FObjectDataHandle) then
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
          if not dbPack_ItemWrite(FileSize, Buff, ItemHnd, FObjectDataHandle) then
            begin
              umlFileClose(FileHandle);
              Exit;
            end;
        end;
    end;
  umlFileClose(FileHandle);
  Result := True;
end;

function TObjectDataManager.ItemAutoConnect(const dbPath, DBItem, DBItemDescription: SystemString; var ItemHnd: TItemHandle): Boolean;
begin
  if ItemExists(dbPath, DBItem) then
      Result := ItemOpen(dbPath, DBItem, ItemHnd)
  else
      Result := ItemCreate(dbPath, DBItem, DBItemDescription, ItemHnd);
end;

function TObjectDataManager.ItemFastAutoConnect_F(const FieldPos: Int64; const DBItem, DBItemDescription: SystemString; var ItemHnd: TItemHandle): Boolean;
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

function TObjectDataManager.ItemFastAutoConnect_L(const FieldPos: Int64; const DBItem, DBItemDescription: SystemString; var ItemHnd: TItemHandle): Boolean;
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

function TObjectDataManager.ItemCreate(const dbPath, DBItem, DBItemDescription: SystemString; var ItemHnd: TItemHandle): Boolean;
{
  It can automatically create a path
}
begin
  Init_TTMDBItemHandle(ItemHnd);
  Result := dbPack_ItemCreate(dbPath, DBItem, DBItemDescription, FDefaultItemID, ItemHnd, FObjectDataHandle);
end;

function TObjectDataManager.ItemDelete(const dbPath, DBItem: SystemString): Boolean;
begin
  Result := dbPack_DeleteItem(dbPath, DBItem, FDefaultItemID, FObjectDataHandle);
end;

function TObjectDataManager.ItemExists(const dbPath, DBItem: SystemString): Boolean;
var
  ItemSearchHnd: TItemSearch;
begin
  Init_TTMDBSearchItem(ItemSearchHnd);
  Result := dbPack_FindFirstItem(dbPath, DBItem, FDefaultItemID, ItemSearchHnd, FObjectDataHandle);
end;

function TObjectDataManager.ItemFastInsertNew(const FieldPos, InsertHeaderPos: Int64; const DBItem, DBItemDescription: SystemString; var ItemHnd: TItemHandle): Boolean;
begin
  Init_TTMDBItemHandle(ItemHnd);
  Result := dbPack_ItemFastInsertNew(DBItem, DBItemDescription, FieldPos, InsertHeaderPos, FDefaultItemID, ItemHnd, FObjectDataHandle);
end;

function TObjectDataManager.ItemFastCreate(const aPos: Int64; const DBItem, DBItemDescription: SystemString; var ItemHnd: TItemHandle): Boolean;
begin
  Init_TTMDBItemHandle(ItemHnd);
  Result := dbPack_ItemFastCreate(DBItem, DBItemDescription, aPos, FDefaultItemID, ItemHnd, FObjectDataHandle);
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

function TObjectDataManager.ItemFastOpen(const aPos: Int64; var ItemHnd: TItemHandle): Boolean;
begin
  Init_TTMDBItemHandle(ItemHnd);
  Result := dbPack_ItemFastOpen(aPos, FDefaultItemID, ItemHnd, FObjectDataHandle);
end;

function TObjectDataManager.ItemFastResetBody(const aPos: Int64): Boolean;
var
  ItemHnd: TItemHandle;
begin
  Result := ItemFastOpen(aPos, ItemHnd)
    and dbPack_ItemBodyReset(ItemHnd, FObjectDataHandle);
end;

function TObjectDataManager.ItemFindFirst(const dbPath, DBItem: SystemString; var ItemSearchHandle: TItemSearch): Boolean;
begin
  Init_TTMDBSearchItem(ItemSearchHandle);
  Result := dbPack_FindFirstItem(dbPath, DBItem, FDefaultItemID, ItemSearchHandle, FObjectDataHandle);
end;

function TObjectDataManager.ItemFindLast(const dbPath, DBItem: SystemString; var ItemSearchHandle: TItemSearch): Boolean;
begin
  Init_TTMDBSearchItem(ItemSearchHandle);
  Result := dbPack_FindLastItem(dbPath, DBItem, FDefaultItemID, ItemSearchHandle, FObjectDataHandle);
end;

function TObjectDataManager.ItemFindNext(var ItemSearchHandle: TItemSearch): Boolean;
begin
  Result := dbPack_FindNextItem(ItemSearchHandle, FDefaultItemID, FObjectDataHandle);
end;

function TObjectDataManager.ItemFindPrev(var ItemSearchHandle: TItemSearch): Boolean;
begin
  Result := dbPack_FindPrevItem(ItemSearchHandle, FDefaultItemID, FObjectDataHandle);
end;

function TObjectDataManager.ItemGetFile(var ItemHnd: TItemHandle; const FileName: SystemString): Boolean;
var
  RepaInt   : Integer;
  FileHandle: TIOHnd;
  FileSize  : Int64;
  Buff      : array [0 .. MaxBuffSize] of umlChar;
begin
  Result := False;
  InitIOHnd(FileHandle);
  if not umlFileCreate(FileName, FileHandle) then
    begin
      umlFileClose(FileHandle);
      Exit;
    end;

  if not dbPack_ItemRead(umlSizeLength, FileSize, ItemHnd, FObjectDataHandle) then
    begin
      umlFileClose(FileHandle);
      Exit;
    end;

  if FileSize > MaxBuffSize then
    begin
      for RepaInt := 1 to FileSize div MaxBuffSize do
        begin
          if not dbPack_ItemRead(MaxBuffSize, Buff, ItemHnd, FObjectDataHandle) then
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
          if not dbPack_ItemRead(FileSize mod MaxBuffSize, Buff, ItemHnd, FObjectDataHandle) then
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
          if not dbPack_ItemRead(FileSize, Buff, ItemHnd, FObjectDataHandle) then
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

function TObjectDataManager.ItemMove(const dbPath, ItemName, DestPath: SystemString): Boolean;
begin
  Result := dbPack_MoveItem(dbPath, ItemName, DestPath, FDefaultItemID, FObjectDataHandle);
end;

function TObjectDataManager.ItemOpen(const dbPath, DBItem: SystemString; var ItemHnd: TItemHandle): Boolean;
begin
  Init_TTMDBItemHandle(ItemHnd);
  Result := dbPack_ItemOpen(dbPath, DBItem, FDefaultItemID, ItemHnd, FObjectDataHandle);
end;

function TObjectDataManager.ItemRead(var ItemHnd: TItemHandle; const aSize: Int64; var Buffers): Boolean;
begin
  Result := dbPack_ItemRead(aSize, Buffers, ItemHnd, FObjectDataHandle);
end;

function TObjectDataManager.ItemReadBool(var ItemHnd: TItemHandle; var Value: Boolean): Boolean;
var
  TempValue: Byte;
begin
  TempValue := 0;
  Result := dbPack_ItemRead(1, TempValue, ItemHnd, FObjectDataHandle);
  Value := (TempValue = 1);
end;

function TObjectDataManager.ItemReadInt(var ItemHnd: TItemHandle; var Value: Integer): Boolean;
begin
  Result := dbPack_ItemRead(umlIntegerLength, Value, ItemHnd, FObjectDataHandle);
end;

function TObjectDataManager.ItemReadReturnBool(var ItemHnd: TItemHandle; const DefaultValue: Boolean): Boolean;
var
  TempValue: Byte;
begin
  TempValue := 0;
  if dbPack_ItemRead(1, TempValue, ItemHnd, FObjectDataHandle) then
      Result := (TempValue = 1)
  else
      Result := DefaultValue;
end;

function TObjectDataManager.ItemReadReturnInt(var ItemHnd: TItemHandle; const DefaultValue: Integer): Integer;
var
  TempInt: Integer;
begin
  TempInt := DefaultValue;
  if dbPack_ItemRead(umlIntegerLength, TempInt, ItemHnd, FObjectDataHandle) then
      Result := TempInt
  else
      Result := DefaultValue;
end;

function TObjectDataManager.ItemReadReturnStr(var ItemHnd: TItemHandle; const DefaultValue: SystemString): SystemString;
var
  TempStr: umlString;
begin
  TempStr := DefaultValue;
  if dbPack_ItemReadStr(TempStr, ItemHnd, FObjectDataHandle) then
      Result := TempStr
  else
      Result := DefaultValue;
end;

function TObjectDataManager.ItemReadStr(var ItemHnd: TItemHandle; var Value: SystemString): Boolean;
var
  TempStr: umlString;
begin
  Result := dbPack_ItemReadStr(TempStr, ItemHnd, FObjectDataHandle);
  if Result then
      Value := TempStr;
end;

function TObjectDataManager.ItemReadTime(var ItemHnd: TItemHandle; var Value: TDateTime): Boolean;
var
  TempValue: Double;
begin
  if dbPack_ItemRead(umlDoubleLength, TempValue, ItemHnd, FObjectDataHandle) then
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
  Result := dbPack_ItemSeekStartPos(ItemHnd, FObjectDataHandle);
end;

function TObjectDataManager.ItemSeekLast(var ItemHnd: TItemHandle): Boolean;
begin
  Result := dbPack_ItemSeekLastPos(ItemHnd, FObjectDataHandle);
end;

function TObjectDataManager.ItemSeek(var ItemHnd: TItemHandle; const ItemOffset: Int64): Boolean;
var
  aSize: Integer;
begin
  aSize := dbPack_ItemGetSize(ItemHnd, FObjectDataHandle);
  if ItemOffset > aSize then
      Result := dbPack_AppendItemSize(ItemHnd, ItemOffset - aSize, FObjectDataHandle)
  else if ItemOffset = aSize then
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

function TObjectDataManager.ItemWrite(var ItemHnd: TItemHandle; const aSize: Int64; var Buffers): Boolean;
begin
  Result := dbPack_ItemWrite(aSize, Buffers, ItemHnd, FObjectDataHandle);
end;

function TObjectDataManager.ItemWriteBool(var ItemHnd: TItemHandle; const Value: Boolean): Boolean;
var
  TempValue: Byte;
begin
  if Value then
      TempValue := 1
  else
      TempValue := 0;
  Result := dbPack_ItemWrite(1, TempValue, ItemHnd, FObjectDataHandle);
end;

function TObjectDataManager.ItemWriteInt(var ItemHnd: TItemHandle; const Value: Integer): Boolean;
var
  TempValue: Integer;
begin
  TempValue := Value;
  Result := dbPack_ItemWrite(umlIntegerLength, TempValue, ItemHnd, FObjectDataHandle);
end;

function TObjectDataManager.ItemWriteStr(var ItemHnd: TItemHandle; const Value: SystemString): Boolean;
begin
  Result := dbPack_ItemWriteStr(Value, ItemHnd, FObjectDataHandle);
end;

function TObjectDataManager.ItemWriteTime(var ItemHnd: TItemHandle; const Value: TDateTime): Boolean;
var
  TempValue: Double;
begin
  TempValue := Value;
  Result := dbPack_ItemWrite(umlDoubleLength, TempValue, ItemHnd, FObjectDataHandle);
end;

function TObjectDataManager.ReadBool(const Section, Item: SystemString; const Value: Boolean): Boolean;
begin
  if Value then
      Result := ReadString(Section, Item, '1') = '1'
  else
      Result := ReadString(Section, Item, '0') = '1';
end;

function TObjectDataManager.ReadString(const Section, Item, Value: SystemString): SystemString;
var
  TempStr     : SystemString;
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

function TObjectDataManager.WriteBool(const Section, Item: SystemString; const Value: Boolean): Boolean;
begin
  if Value then
      Result := WriteString(Section, Item, '1')
  else
      Result := WriteString(Section, Item, '0');
end;

function TObjectDataManager.WriteString(const Section, Item, Value: SystemString): Boolean;
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

function TObjectDataMarshal.GetItems(aIndex: Integer): TObjectDataManager;
begin
  Result := TObjectDataManager(FLibList.Objects[aIndex]);
end;

function TObjectDataMarshal.GetNames(AName: SystemString): TObjectDataManager;
var
  RepaInt: Integer;
  aUName : SystemString;
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

function TObjectDataMarshal.GetAbsoluteFileName(aFileName: SystemString): SystemString;
begin
  Result := umlUpperCase(aFileName).Text;
end;

function TObjectDataMarshal.NewDB(aFile: SystemString; aOnlyRead: Boolean): TObjectDataManager;
var
  RepaInt: Integer;
  aUName : SystemString;
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
  aUName : SystemString;
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

procedure TObjectDataMarshal.DeleteFromName(AName: SystemString);
var
  RepaInt: Integer;
  aUName : SystemString;
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
