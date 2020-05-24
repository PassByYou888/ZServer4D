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
  TItemHandle = ObjectData.TItemHandle_;
  PItemHandle = ^TItemHandle;
  TFieldHandle = ObjectData.TField;
  PFieldHandle = ^TFieldHandle;
  TItemSearch = ObjectData.TSearchItem_;
  PItemSearch = ^TItemSearch;
  TFieldSearch = ObjectData.TSearchField_;
  PFieldSearch = ^TFieldSearch;
  TItemRecursionSearch = ObjectData.TRecursionSearch_;
  PItemRecursionSearch = ^TItemRecursionSearch;

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
    State: Integer;
    MemorySiz: nativeUInt;
    procedure write(var wVal: TItem);
    procedure read(var rVal: TItem);
  end;

  PObjectDataCacheItem = ^TObjectDataCacheItem;

  TObjectDataCacheField = record
    UpFieldPOS: Int64;
    Description: U_String;
    HeaderCount: Int64;
    FirstHeaderPOS: Int64;
    LastHeaderPOS: Int64;
    State: Integer;
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

  TObjectDataManager = class;
{$IFDEF FPC}
  TDBImpNotifyProc = procedure(Sender: TObjectDataManager; sourFile: SystemString; fieldPos, ItemPos: Int64) is nested;
  TDBExpNotifyProc = procedure(Sender: TObjectDataManager; fieldPos, ItemPos: Int64; destFile: SystemString) is nested;
{$ELSE FPC}
  TDBImpNotifyProc = reference to procedure(Sender: TObjectDataManager; sourFile: SystemString; fieldPos, ItemPos: Int64);
  TDBExpNotifyProc = reference to procedure(Sender: TObjectDataManager; fieldPos, ItemPos: Int64; destFile: SystemString);
{$ENDIF FPC}

  TObjectDataManager = class(TCoreClassObject)
  protected
    FStreamEngine: TCoreClassStream;
    FDBHandle: TObjectDataHandle;
    FNeedCreateNew, FOnlyRead: Boolean;
    FObjectName: SystemString;
    FDefaultItemID: Byte;
    FIsOpened: Boolean;
    FData: Pointer;

    function GetAutoFreeHandle: Boolean;
    procedure SetAutoFreeHandle(const Value: Boolean);
  protected
    procedure DoOpenBefore; virtual;
    procedure DoOpenAfter; virtual;

    function GetOverWriteItem: Boolean;
    function GetAllowSameHeaderName: Boolean;
    function GetDBTime: TDateTime;
    procedure SetOverWriteItem(Value: Boolean);
    procedure SetAllowSameHeaderName(Value: Boolean);

    procedure DBErrorProc(error: U_String);
    function DoOpen(): Boolean;
    function NewHandle(Stream_: TCoreClassStream; const dbFile: SystemString; const dbItemID: Byte; dbOnlyRead, IsNewDB_: Boolean): Boolean; overload;
    function NewHandle(FixedStringL: Byte; Stream_: TCoreClassStream; const dbFile: SystemString; const dbItemID: Byte; dbOnlyRead, IsNewDB_: Boolean): Boolean; overload;
  public
    // Open Database
    constructor Open(const dbFile: SystemString; const dbItemID: Byte; dbOnlyRead: Boolean); overload;
    // create new Database
    constructor CreateNew(const dbFile: SystemString; const dbItemID: Byte); overload;
    constructor CreateNew(FixedStringL: Byte; const dbFile: SystemString; const dbItemID: Byte); overload;
    // create or Open form Stream IO
    constructor CreateAsStream(Stream_: TCoreClassStream;
      const dbFile: SystemString; const dbItemID: Byte; dbOnlyRead, isNewDB, DestroyTimeFreeStream: Boolean); overload;
    constructor CreateAsStream(FixedStringL: Byte; Stream_: TCoreClassStream;
      const dbFile: SystemString; const dbItemID: Byte; dbOnlyRead, isNewDB, DestroyTimeFreeStream: Boolean); overload;

    // destroy
    destructor Destroy; override;

    function CopyTo(DestDB: TObjectDataManager): Boolean;
    function CopyToPath(DestDB: TObjectDataManager; destPath: SystemString): Boolean;
    function CopyFieldToPath(fieldPos: Int64; DestDB: TObjectDataManager; destPath: SystemString): Boolean;
    function CopyItemToPath(const DBPath, DBItemName: SystemString; DestDB: TObjectDataManager; destPath: SystemString): Integer;

    // export to stream
    procedure SaveToStream(stream: TCoreClassStream);

    // export to ZLib Compressor for stream
    procedure SaveToZLibStream(stream: TCoreClassStream);

    // export to parallel Compressor for stream
    procedure SaveToParallelCompressionStream(stream: TCoreClassStream);

    // Import recursively
    procedure ImpFromPathP(ImpPath, DBPath: SystemString; IncludeSub: Boolean; Notify: TDBImpNotifyProc); overload;
    procedure ImpFromPath(ImpPath, DBPath: SystemString; IncludeSub: Boolean); overload;

    // Import batch
    procedure ImpFromFilesP(ImpFiles: TCoreClassStrings; DBPath: SystemString; Notify: TDBImpNotifyProc); overload;
    procedure ImpFromFiles(ImpFiles: TCoreClassStrings; DBPath: SystemString); overload;

    // split direct
    procedure SplitTo(RootPh, destFile: SystemString; SplitSiz: Int64);

    // split to ZLib compressor for DB
    procedure SplitToZLib(RootPh, destFile: SystemString; SplitSiz: Int64);

    // split to Parallel compressor for DB
    procedure SplitToParallelCompression(RootPh, destFile: SystemString; SplitSiz: Int64);

    // export to disk
    procedure ExpPathToDisk(DBPath, ExpPath_: SystemString; IncludeSub: Boolean); overload;
    procedure ExpPathToDiskP(DBPath, ExpPath_: SystemString; IncludeSub: Boolean; Notify: TDBExpNotifyProc); overload;
    procedure ExpItemToDisk(DBPath, DBItem, ExpFilename_: SystemString);

    // state
    function Is_BACKUP_Mode: Boolean;
    function Is_Flush_Mode: Boolean;
    function isAbort: Boolean;
    function Close: Boolean;
    function ErrorNo: Int64;
    function Modification: Boolean;
    function Size: Int64;
    function IOReadSize: Int64;
    function IOWriteSize: Int64;
    procedure SetID(const ID_: Byte);
    procedure UpdateIO; virtual;

    // field api
    function CreateField(const DirName, DirDescription: SystemString): Boolean;
    function CreateRootField(const RootName: SystemString): Boolean;
    function DirectoryExists(const DirName: SystemString): Boolean;
    function FastDelete(const fieldPos: Int64; const fPos: Int64): Boolean;
    function FastFieldExists(const fieldPos: Int64; const FieldName: SystemString): Boolean;
    function FastFieldCreate(const fieldPos: Int64; const FieldName, FieldDescription: SystemString; var NewFieldPos: Int64): Boolean;
    function RootField: Int64;
    function SetRootField(const RootName: SystemString): Boolean;
    function GetRootFieldPos(const RootName: SystemString): Int64;
    function FieldRename(const fieldPos: Int64; const NewFieldName, NewFieldDescription: SystemString): Boolean;
    function FieldDelete(const DBPath: SystemString; const FieldName: SystemString): Boolean;
    function FieldExists(const DBPath: SystemString; const FieldName: SystemString): Boolean; overload;
    function FieldExists(const DBPath: SystemString): Boolean; overload;
    function FieldFastFindFirst(const fieldPos: Int64; const Filter: SystemString; var FieldSearchHandle: TFieldSearch): Boolean;
    function FieldFastFindLast(const fieldPos: Int64; const Filter: SystemString; var FieldSearchHandle: TFieldSearch): Boolean;
    function FieldFastFindNext(var FieldSearchHandle: TFieldSearch): Boolean;
    function FieldFastFindPrev(var FieldSearchHandle: TFieldSearch): Boolean;
    function FieldFindFirst(const DBPath, Filter: SystemString; var FieldSearchHandle: TFieldSearch): Boolean;
    function FieldFindLast(const DBPath, Filter: SystemString; var FieldSearchHandle: TFieldSearch): Boolean;
    function FieldFindNext(var FieldSearchHandle: TFieldSearch): Boolean;
    function FieldFindPrev(var FieldSearchHandle: TFieldSearch): Boolean;
    function FieldMove(const DBPath, FieldName, destPath: SystemString): Boolean;
    function GetFieldData(const fieldPos: Int64; var dest: TFieldHandle): Boolean;
    function GetFieldPath(const fieldPos: Int64): SystemString; overload;
    function GetFieldPath(const fieldPos, RootFieldPos: Int64): SystemString; overload;
    function GetPathField(const DBPath: SystemString; var dest: Int64): Boolean;
    function GetPathFieldPos(const DBPath: SystemString): Int64;
    function GetPathFieldHeaderCount(const DBPath: SystemString): Int64;
    function GetPathFieldHeaderNames(const DBPath: SystemString; var output: U_StringArray): Boolean;

    // header api
    function GetHeaderModificationTime(const hPos: Int64): TDateTime;
    function GetFirstHeaderFromField(fieldPos: Int64; var h: THeader): Boolean;
    function GetLastHeaderFromField(fieldPos: Int64; var h: THeader): Boolean;
    function GetHeader(hPos: Int64; var h: THeader): Boolean;

    // item api
    function GetItemSize(const DBPath, DBItemName: SystemString): Int64;
    function ItemCreate(const DBPath, DBItemName, DBItemDescription: SystemString; var ItemHnd: TItemHandle): Boolean;
    function ItemDelete(const DBPath, DBItemName: SystemString): Boolean;
    function ItemExists(const DBPath, DBItemName: SystemString): Boolean;
    function ItemOpen(const DBPath, DBItemName: SystemString; var ItemHnd: TItemHandle): Boolean;
    function ItemAutoOpenOrCreate(const DBPath, DBItemName, DBItemDescription: SystemString; var ItemHnd: TItemHandle): Boolean;
    function ItemUpdate(var ItemHnd: TItemHandle): Boolean;
    function ItemClose(var ItemHnd: TItemHandle): Boolean;
    function ItemCopyTo(var ItemHnd: TItemHandle; DestDB: TObjectDataManager; var DestItemHandle: TItemHandle; const CopySize: Int64): Boolean;
    function ItemMove(const DBPath, ItemName, destPath: SystemString): Boolean;
    function ItemRename(const fieldPos: Int64; var ItemHnd: TItemHandle; const NewName, NewDescription: SystemString): Boolean;
    function ItemFastInsertNew(const fieldPos, InsertHeaderPos: Int64; const DBItemName, DBItemDescription: SystemString; var ItemHnd: TItemHandle): Boolean;
    function ItemFastCreate(const fPos: Int64; const DBItemName, DBItemDescription: SystemString; var ItemHnd: TItemHandle): Boolean;
    function ItemFastOpen(const fPos: Int64; var ItemHnd: TItemHandle): Boolean;
    function ItemFastResetBody(const fPos: Int64): Boolean;
    function ItemFastExists(const fieldPos: Int64; const DBItemName: SystemString): Boolean;
    function ItemFastFindFirst(const fieldPos: Int64; const DBItemName: SystemString; var ItemSearchHandle: TItemSearch): Boolean;
    function ItemFastFindLast(const fieldPos: Int64; const DBItemName: SystemString; var ItemSearchHandle: TItemSearch): Boolean;
    function ItemFastFindNext(var ItemSearchHandle: TItemSearch): Boolean;
    function ItemFastFindPrev(var ItemSearchHandle: TItemSearch): Boolean;
    function ItemFindFirst(const DBPath, DBItemName: SystemString; var ItemSearchHandle: TItemSearch): Boolean;
    function ItemFindLast(const DBPath, DBItemName: SystemString; var ItemSearchHandle: TItemSearch): Boolean;
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
    function ItemReadToStream(const DBPath, DBItemName: SystemString; stream: TCoreClassStream): Boolean; overload;
    function ItemWriteFromStream(const DBPath, DBItemName: SystemString; stream: TCoreClassStream): Boolean; overload;

    // recursion support
    function RecursionSearchFirst(const InitPath, Filter: SystemString; var RecursionSearchHnd: TItemRecursionSearch): Boolean;
    function RecursionSearchNext(var RecursionSearchHnd: TItemRecursionSearch): Boolean;

    // options
    function HandlePtr: PObjectDataHandle;
    property Handle: PObjectDataHandle read HandlePtr;
    property AutoFreeHandle: Boolean read GetAutoFreeHandle write SetAutoFreeHandle;
    property IsOnlyRead: Boolean read FOnlyRead;
    property NeedCreateNew: Boolean read FNeedCreateNew;
    property ObjectName: SystemString read FObjectName write FObjectName;
    property DefaultItemID: Byte read FDefaultItemID;
    property ID: Byte read FDefaultItemID;
    property StreamEngine: TCoreClassStream read FStreamEngine;
    property DBTime: TDateTime read GetDBTime;
    property OverWriteItem: Boolean read GetOverWriteItem write SetOverWriteItem;
    property SameHeaderName: Boolean read GetAllowSameHeaderName write SetAllowSameHeaderName;

    // user custom data
    property Data: Pointer read FData write FData;
  end;

  TObjectDataManagerClass = class of TObjectDataManager;

  TObjectDataManagerOfCache = class(TObjectDataManager)
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

    procedure PrepareTMDBWriteProc(fPos: Int64; const wVal: PObjectDataHandle; var Done: Boolean);
    procedure TMDBWriteProc(fPos: Int64; const wVal: PObjectDataHandle);
    procedure TMDBReadProc(fPos: Int64; const rVal: PObjectDataHandle; var Done: Boolean);

    procedure DoOpenBefore; override;
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
    function GetNames(Name_: SystemString): TObjectDataManager;
    procedure SetItems(aIndex: Integer; const Value: TObjectDataManager);
  public
    constructor Create(dbItemID: Byte);
    destructor Destroy; override;
    function GetAbsoluteFileName(fileName: SystemString): SystemString;
    function NewDB(dbFile: SystemString; dbOnlyRead: Boolean): TObjectDataManager; overload;
    function NewDB(FixedStringL: Byte; dbFile: SystemString; dbOnlyRead: Boolean): TObjectDataManager; overload;
    function Open(dbFile: SystemString; dbOnlyRead: Boolean): TObjectDataManager;
    procedure CloseDB(db: TObjectDataManager);
    procedure Clear;
    function Count: Integer;
    procedure Delete(aIndex: Integer);
    procedure DeleteFromName(Name_: SystemString);
    procedure UpdateAll;
    procedure Disable;
    procedure Enabled;

    property LibList: TCoreClassStrings read FLibList;
    property Items[aIndex: Integer]: TObjectDataManager read GetItems write SetItems;
    property Names[Name_: SystemString]: TObjectDataManager read GetNames; default;
    property UseWildcard: Boolean read FUseWildcard write FUseWildcard;
    property ID: Byte read FID write FID;
  end;

function ObjectDataMarshal: TObjectDataMarshal;
function DBMarshal: TObjectDataMarshal;
procedure CheckAndRemoveFlush(PrepareOpenFile: U_String);
procedure CheckAndRestoreFlush(PrepareOpenFile: U_String);

procedure TestObjectData();

implementation

uses ItemStream, Types, MemoryStream64, DoStatusIO;

const
  SFlush = '.~flush';
  STmp = '.tmp';
  SOld = '.old';
  C_BufferChunkSize = $FFFF;

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

procedure CheckAndRemoveFlush(PrepareOpenFile: U_String);
var
  swapFileName: TPascalString;
begin
  if not umlFileExists(PrepareOpenFile) then
      Exit;
  swapFileName := PrepareOpenFile + SFlush;

  if not umlFileExists(swapFileName) then
      Exit;
  umlDeleteFile(swapFileName);
end;

procedure CheckAndRestoreFlush(PrepareOpenFile: U_String);
var
  IOHnd: TIOHnd;
  swapFileName: TPascalString;
  swapHnd: TCoreClassFileStream;
  swapCompleted, swapTotal: Integer;
  swapHead: TSwapHead;
  CheckSuccessed: Boolean;
  m64: TMemoryStream64;
  m5: TMD5;
  oldDBHnd, newDBHnd: TObjectDataHandle;
begin
  if not umlFileExists(PrepareOpenFile) then
      Exit;
  swapFileName := PrepareOpenFile + SFlush;

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

  // check crc
  swapCompleted := 0;
  if swapHnd.read(swapTotal, C_Integer_Size) = C_Integer_Size then
    while swapHnd.Position < swapHnd.Size do
      begin
        if swapHnd.read(swapHead, SizeOf(swapHead)) <> SizeOf(swapHead) then
          begin
            DoStatus('%s CRC header errors, the database will be restored to previous state', [umlGetFileName(swapFileName).Text]);
            CheckSuccessed := False;
            break;
          end;
        m64.Clear;
        if m64.CopyFrom(swapHnd, swapHead.Size) <> swapHead.Size then
          begin
            DoStatus('%s CRC data loss, the database will be restored to previous state', [umlGetFileName(swapFileName).Text]);
            CheckSuccessed := False;
            break;
          end;
        m5 := umlMD5(m64.Memory, m64.Size);
        if not umlCompareMD5(m5, swapHead.MD5) then
          begin
            DoStatus('%s CRC validation errors, the database will be restored to previous state', [umlGetFileName(swapFileName).Text]);
            CheckSuccessed := False;
            break;
          end;
        inc(swapCompleted);
      end;

  CheckSuccessed := CheckSuccessed and (swapCompleted = swapTotal);

  // restore
  if CheckSuccessed then
    begin
      DoStatus('Start backup of old database.');
      Init_TTMDB(oldDBHnd);
      db_Open(PrepareOpenFile, oldDBHnd, True);
      Init_TTMDB(newDBHnd, oldDBHnd.FixedStringL);
      db_CreateNew(PrepareOpenFile + SOld, newDBHnd);
      db_CopyAllTo(oldDBHnd, newDBHnd);
      db_ClosePack(oldDBHnd);
      db_ClosePack(newDBHnd);
      DoStatus('old database "%s" rename as -> "%s"', [umlGetFileName(PrepareOpenFile).Text, umlGetFileName(PrepareOpenFile).Text + SOld]);

      DoStatus('database to restored to previous state', []);
      InitIOHnd(IOHnd);
      if umlFileOpen(PrepareOpenFile, IOHnd, False) then
        begin
          swapCompleted := 0;
          swapHnd.Position := 0;
          if swapHnd.read(swapTotal, C_Integer_Size) = C_Integer_Size then
            while swapHnd.Position < swapHnd.Size do
              begin
                swapHnd.read(swapHead, SizeOf(swapHead));
                m64.Clear;
                m64.CopyFrom(swapHnd, swapHead.Size);

                umlFileSeek(IOHnd, swapHead.Position);
                umlBlockWrite(IOHnd, m64.Memory^, m64.Size);
                inc(swapCompleted);
                DoStatus('CRC %s restored %d / %d', [umlMD5ToStr(swapHead.MD5).Text, swapCompleted, swapTotal]);
              end;
        end;
      umlFileClose(IOHnd);
    end
  else
    begin
      DoStatus('%s CRC error!, Start repairing this database, please wait', [umlGetFileName(swapFileName).Text]);
      Init_TTMDB(oldDBHnd);
      db_Open(PrepareOpenFile, oldDBHnd, True);

      Init_TTMDB(newDBHnd, oldDBHnd.FixedStringL);
      db_CreateNew(PrepareOpenFile + STmp, newDBHnd);

      DoStatus('Start backup of old database.');
      db_CopyAllTo(oldDBHnd, newDBHnd);

      db_ClosePack(oldDBHnd);
      db_ClosePack(newDBHnd);

      umlDeleteFile(PrepareOpenFile + SOld);
      umlRenameFile(PrepareOpenFile, PrepareOpenFile + SOld);
      umlRenameFile(PrepareOpenFile + STmp, PrepareOpenFile);

      DoStatus('old database "%s" rename as -> "%s"', [umlGetFileName(PrepareOpenFile).Text, umlGetFileName(PrepareOpenFile).Text + SOld]);
    end;

  DisposeObject(swapHnd);
  DisposeObject(m64);
  umlDeleteFile(swapFileName);
end;

procedure TObjectDataCacheItem.write(var wVal: TItem);
begin
  Description := wVal.Description;
  ExtID := wVal.ExtID;
  FirstBlockPOS := wVal.FirstBlockPOS;
  LastBlockPOS := wVal.LastBlockPOS;
  Size := wVal.Size;
  BlockCount := wVal.BlockCount;
  CurrentBlockSeekPOS := wVal.CurrentBlockSeekPOS;
  CurrentFileSeekPOS := wVal.CurrentFileSeekPOS;
  State := wVal.State;
  MemorySiz := 0;
end;

procedure TObjectDataCacheItem.read(var rVal: TItem);
begin
  rVal.Description := Description;
  rVal.ExtID := ExtID;
  rVal.FirstBlockPOS := FirstBlockPOS;
  rVal.LastBlockPOS := LastBlockPOS;
  rVal.Size := Size;
  rVal.BlockCount := BlockCount;
  rVal.CurrentBlockSeekPOS := CurrentBlockSeekPOS;
  rVal.CurrentFileSeekPOS := CurrentFileSeekPOS;
  rVal.State := State;
end;

procedure TObjectDataCacheField.write(var wVal: TField);
begin
  UpFieldPOS := wVal.UpFieldPOS;
  Description := wVal.Description;
  HeaderCount := wVal.HeaderCount;
  FirstHeaderPOS := wVal.FirstHeaderPOS;
  LastHeaderPOS := wVal.LastHeaderPOS;
  State := wVal.State;
  MemorySiz := 0;
end;

procedure TObjectDataCacheField.read(var rVal: TField);
begin
  rVal.UpFieldPOS := UpFieldPOS;
  rVal.Description := Description;
  rVal.HeaderCount := HeaderCount;
  rVal.FirstHeaderPOS := FirstHeaderPOS;
  rVal.LastHeaderPOS := LastHeaderPOS;
  rVal.State := State;
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

procedure TObjectDataManager.DoOpenBefore;
begin
end;

procedure TObjectDataManager.DoOpenAfter;
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

function TObjectDataManager.DoOpen(): Boolean;
begin
  Result := False;
  try
    if StreamEngine <> nil then
      begin
        if FNeedCreateNew then
          begin
            if not db_CreateAsStream(StreamEngine, ObjectName, '', FDBHandle) then
                Exit;
          end
        else
          begin
            if not db_OpenAsStream(StreamEngine, ObjectName, FDBHandle, IsOnlyRead) then
                Exit;
          end;
      end
    else if (FNeedCreateNew) or (not umlFileExists(ObjectName)) then
      begin
        if not db_CreateNew(ObjectName, FDBHandle) then
            Exit;
      end
    else if not db_Open(ObjectName, FDBHandle, IsOnlyRead) then
        Exit;
    Result := True;
  except
  end;
end;

function TObjectDataManager.NewHandle(Stream_: TCoreClassStream; const dbFile: SystemString; const dbItemID: Byte; dbOnlyRead, IsNewDB_: Boolean): Boolean;
begin
  Result := NewHandle(65, Stream_, dbFile, dbItemID, dbOnlyRead, IsNewDB_);
end;

function TObjectDataManager.NewHandle(FixedStringL: Byte; Stream_: TCoreClassStream; const dbFile: SystemString; const dbItemID: Byte; dbOnlyRead, IsNewDB_: Boolean): Boolean;
begin
  Close;
  Init_TTMDB(FDBHandle, FixedStringL);
  FDBHandle.OnError := {$IFDEF FPC}@{$ENDIF FPC}DBErrorProc;

  FStreamEngine := Stream_;
  FObjectName := dbFile;
  FNeedCreateNew := IsNewDB_;
  FOnlyRead := dbOnlyRead;
  FDefaultItemID := dbItemID;

  DoOpenBefore;
  try
      FIsOpened := DoOpen();
  except
    FIsOpened := False;
    Result := False;
    Close;
  end;
  DoOpenAfter;

  Result := FIsOpened;

  OverWriteItem := True;
  SameHeaderName := False;
  AutoFreeHandle := True;
  FData := nil;
end;

constructor TObjectDataManager.Open(const dbFile: SystemString; const dbItemID: Byte; dbOnlyRead: Boolean);
begin
  inherited Create;
  if dbOnlyRead then
      CheckAndRemoveFlush(dbFile)
  else
      CheckAndRestoreFlush(dbFile);
  NewHandle(nil, dbFile, dbItemID, dbOnlyRead, False);
end;

constructor TObjectDataManager.CreateNew(const dbFile: SystemString; const dbItemID: Byte);
begin
  inherited Create;
  CheckAndRemoveFlush(dbFile);
  NewHandle(nil, dbFile, dbItemID, False, True);
end;

constructor TObjectDataManager.CreateNew(FixedStringL: Byte; const dbFile: SystemString; const dbItemID: Byte);
begin
  inherited Create;
  CheckAndRemoveFlush(dbFile);
  NewHandle(FixedStringL, nil, dbFile, dbItemID, False, True);
end;

constructor TObjectDataManager.CreateAsStream(Stream_: TCoreClassStream;
  const dbFile: SystemString; const dbItemID: Byte; dbOnlyRead, isNewDB, DestroyTimeFreeStream: Boolean);
begin
  inherited Create;
  NewHandle(Stream_, dbFile, dbItemID, dbOnlyRead, isNewDB);
  AutoFreeHandle := DestroyTimeFreeStream;
end;

constructor TObjectDataManager.CreateAsStream(FixedStringL: Byte; Stream_: TCoreClassStream;
  const dbFile: SystemString; const dbItemID: Byte; dbOnlyRead, isNewDB, DestroyTimeFreeStream: Boolean);
begin
  inherited Create;
  NewHandle(FixedStringL, Stream_, dbFile, dbItemID, dbOnlyRead, isNewDB);
  AutoFreeHandle := DestroyTimeFreeStream;
end;

destructor TObjectDataManager.Destroy;
begin
  Close;
  inherited Destroy;
end;

function TObjectDataManager.CopyTo(DestDB: TObjectDataManager): Boolean;
begin
  Result := db_CopyAllTo(FDBHandle, DestDB.FDBHandle);
end;

function TObjectDataManager.CopyToPath(DestDB: TObjectDataManager; destPath: SystemString): Boolean;
begin
  Result := db_CopyAllToDestPath(FDBHandle, DestDB.FDBHandle, destPath);
end;

function TObjectDataManager.CopyFieldToPath(fieldPos: Int64; DestDB: TObjectDataManager; destPath: SystemString): Boolean;
var
  DestFieldPos: Int64;
begin
  Result := False;
  DestDB.CreateField(destPath, '');
  if DestDB.GetPathField(destPath, DestFieldPos) then
      Result := db_CopyFieldTo('*', FDBHandle, fieldPos, DestDB.FDBHandle, DestFieldPos);
end;

function TObjectDataManager.CopyItemToPath(const DBPath, DBItemName: SystemString; DestDB: TObjectDataManager; destPath: SystemString): Integer;
var
  srHnd: TItemSearch;
  sourItmHnd, destItmHnd: TItemHandle;
begin
  Result := 0;
  DestDB.CreateField(destPath, '');
  if ItemFindFirst(DBPath, DBItemName, srHnd) then
    begin
      repeat
        if ItemFastOpen(srHnd.HeaderPOS, sourItmHnd) then
          begin
            if DestDB.ItemCreate(destPath, sourItmHnd.Name, sourItmHnd.Description, destItmHnd) then
              begin
                ItemCopyTo(sourItmHnd, DestDB, destItmHnd, sourItmHnd.Item.Size);
                DestDB.ItemClose(destItmHnd);
                inc(Result);
              end;
            ItemClose(sourItmHnd);
          end;
      until not ItemFindNext(srHnd);
    end;
end;

procedure TObjectDataManager.SaveToStream(stream: TCoreClassStream);
var
  E: TObjectDataManager;
begin
  E := TObjectDataManager.CreateAsStream(Handle^.IOHnd.FixedStringL, stream, ObjectName, DefaultItemID, False, True, False);
  E.OverWriteItem := False;
  CopyTo(E);
  DisposeObject(E);
end;

procedure TObjectDataManager.SaveToZLibStream(stream: TCoreClassStream);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  SaveToStream(m64);
  m64.Position := 0;
  MaxCompressStream(m64, stream);
  DisposeObject(m64);
end;

procedure TObjectDataManager.SaveToParallelCompressionStream(stream: TCoreClassStream);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.CustomCreate(1024 * 1024);
  SaveToStream(m64);
  m64.Position := 0;
  ParallelCompressStream(TSelectCompressionMethod.scmZLIB_Max, m64, stream);
  DisposeObject(m64);
end;

procedure TObjectDataManager.ImpFromPathP(ImpPath, DBPath: SystemString; IncludeSub: Boolean; Notify: TDBImpNotifyProc);
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
      fs := TCoreClassFileStream.Create(n, fmOpenRead or fmShareDenyNone);
      ItemFastCreate(fPos, umlGetFileName(n).Text, '', itmHnd);
      itmStream := TItemStream.Create(Self, itmHnd);
      try
          itmStream.CopyFrom64(fs, fs.Size)
      except
      end;
      itmStream.CloseHandle;
      DisposeObject(fs);
      DisposeObject(itmStream);
      if Assigned(Notify) then
          Notify(Self, n, fPos, itmHnd.Item.RHeader.CurrentHeader);
    end;

  if IncludeSub then
    begin
      fAry := umlGetDirListWithFullPath(ImpPath);
      for n in fAry do
          ImpFromPathP(n, umlCombineUnixPath(DBPath, umlGetLastStr(n, '\/')).Text, IncludeSub, Notify);
    end;
end;

procedure TObjectDataManager.ImpFromPath(ImpPath, DBPath: SystemString; IncludeSub: Boolean);
begin
  ImpFromPathP(ImpPath, DBPath, IncludeSub, nil);
end;

procedure TObjectDataManager.ImpFromFilesP(ImpFiles: TCoreClassStrings; DBPath: SystemString; Notify: TDBImpNotifyProc);
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
      fs := TCoreClassFileStream.Create(n, fmOpenRead or fmShareDenyNone);
      ItemFastCreate(fPos, umlGetFileName(n).Text, '', itmHnd);
      itmStream := TItemStream.Create(Self, itmHnd);
      try
          itmStream.CopyFrom64(fs, fs.Size)
      except
      end;
      itmStream.CloseHandle;
      DisposeObject(fs);
      DisposeObject(itmStream);
      if Assigned(Notify) then
          Notify(Self, n, fPos, itmHnd.Item.RHeader.CurrentHeader);
    end;
end;

procedure TObjectDataManager.ImpFromFiles(ImpFiles: TCoreClassStrings; DBPath: SystemString);
begin
  ImpFromFilesP(ImpFiles, DBPath, nil);
end;

procedure TObjectDataManager.SplitTo(RootPh, destFile: SystemString; SplitSiz: Int64);
var
  fn: U_String;
  sr: TItemRecursionSearch;
  Ph: U_String;
  itmHnd, destItmHnd: TItemHandle;
  DestDB: TObjectDataManager;
  spID: Integer;
begin
  fn := destFile;
  DestDB := TObjectDataManagerOfCache.CreateNew(FDBHandle.FixedStringL, fn, DefaultItemID);
  DestDB.OverWriteItem := False;
  spID := 1;
  if RecursionSearchFirst(RootPh, '*', sr) then
    begin
      repeat
        if sr.ReturnHeader.ID = DB_Header_Item_ID then
          begin
            Ph := GetFieldPath(sr.CurrentField.RHeader.CurrentHeader);
            ItemFastOpen(sr.ReturnHeader.CurrentHeader, itmHnd);
            DestDB.CreateField(Ph, '');
            DestDB.ItemCreate(Ph, itmHnd.Name, itmHnd.Description, destItmHnd);
            ItemCopyTo(itmHnd, DestDB, destItmHnd, itmHnd.Item.Size);
            destItmHnd.Item.RHeader.UserProperty := itmHnd.Item.RHeader.UserProperty;
            destItmHnd.Item.RHeader.CreateTime := itmHnd.Item.RHeader.CreateTime;
            destItmHnd.Item.RHeader.ModificationTime := itmHnd.Item.RHeader.ModificationTime;
            DestDB.ItemClose(destItmHnd);
            ItemClose(itmHnd);

            if DestDB.Size > SplitSiz then
              begin
                DestDB.UpdateIO;
                DisposeObject(DestDB);

                fn := umlChangeFileExt(destFile, '') + umlIntToStr(spID) + umlGetFileExt(destFile);
                inc(spID);
                DestDB := TObjectDataManagerOfCache.CreateNew(FDBHandle.FixedStringL, fn, DefaultItemID);
                DestDB.OverWriteItem := False;
              end;
          end;

      until not RecursionSearchNext(sr);
    end;
  DestDB.UpdateIO;
  DisposeObject(DestDB);
end;

procedure TObjectDataManager.SplitToZLib(RootPh, destFile: SystemString; SplitSiz: Int64);
var
  fn: U_String;
  sr: TItemRecursionSearch;
  Ph: U_String;
  itmHnd, destItmHnd: TItemHandle;
  DestDB: TObjectDataManager;
  spID: Integer;
  fs: TCoreClassFileStream;
begin
  fn := destFile;
  DestDB := TObjectDataManagerOfCache.CreateAsStream(FDBHandle.FixedStringL,
    TMemoryStream64.CustomCreate(1024 * 1024), '', DefaultItemID, False, True, True);
  DestDB.OverWriteItem := False;
  spID := 1;
  if RecursionSearchFirst(RootPh, '*', sr) then
    begin
      repeat
        if sr.ReturnHeader.ID = DB_Header_Item_ID then
          begin
            Ph := GetFieldPath(sr.CurrentField.RHeader.CurrentHeader);
            ItemFastOpen(sr.ReturnHeader.CurrentHeader, itmHnd);
            DestDB.CreateField(Ph, '');
            DestDB.ItemCreate(Ph, itmHnd.Name, itmHnd.Description, destItmHnd);
            ItemCopyTo(itmHnd, DestDB, destItmHnd, itmHnd.Item.Size);
            destItmHnd.Item.RHeader.UserProperty := itmHnd.Item.RHeader.UserProperty;
            destItmHnd.Item.RHeader.CreateTime := itmHnd.Item.RHeader.CreateTime;
            destItmHnd.Item.RHeader.ModificationTime := itmHnd.Item.RHeader.ModificationTime;
            DestDB.ItemClose(destItmHnd);
            ItemClose(itmHnd);

            if DestDB.Size > SplitSiz then
              begin
                DestDB.UpdateIO;
                fs := TCoreClassFileStream.Create(fn, fmCreate);
                DestDB.StreamEngine.Position := 0;
                MaxCompressStream(DestDB.StreamEngine, fs);
                DisposeObject(fs);
                DisposeObject(DestDB);

                fn := umlChangeFileExt(destFile, '') + umlIntToStr(spID) + umlGetFileExt(destFile);
                inc(spID);
                DestDB := TObjectDataManagerOfCache.CreateAsStream(FDBHandle.FixedStringL,
                  TMemoryStream64.CustomCreate(1024 * 1024), '', DefaultItemID, False, True, True);
                DestDB.OverWriteItem := False;
              end;
          end;

      until not RecursionSearchNext(sr);
    end;
  DestDB.UpdateIO;
  fs := TCoreClassFileStream.Create(fn, fmCreate);
  DestDB.StreamEngine.Position := 0;
  MaxCompressStream(DestDB.StreamEngine, fs);
  DisposeObject(fs);
  DisposeObject(DestDB);
end;

procedure TObjectDataManager.SplitToParallelCompression(RootPh, destFile: SystemString; SplitSiz: Int64);
var
  fn: U_String;
  sr: TItemRecursionSearch;
  Ph: U_String;
  itmHnd, destItmHnd: TItemHandle;
  DestDB: TObjectDataManager;
  spID: Integer;
  fs: TCoreClassFileStream;
begin
  fn := destFile;
  DestDB := TObjectDataManagerOfCache.CreateAsStream(FDBHandle.FixedStringL,
    TMemoryStream64.CustomCreate(1024 * 1024), '', DefaultItemID, False, True, True);
  DestDB.OverWriteItem := False;
  spID := 1;
  if RecursionSearchFirst(RootPh, '*', sr) then
    begin
      repeat
        if sr.ReturnHeader.ID = DB_Header_Item_ID then
          begin
            Ph := GetFieldPath(sr.CurrentField.RHeader.CurrentHeader);
            ItemFastOpen(sr.ReturnHeader.CurrentHeader, itmHnd);
            DestDB.CreateField(Ph, '');
            DestDB.ItemCreate(Ph, itmHnd.Name, itmHnd.Description, destItmHnd);
            ItemCopyTo(itmHnd, DestDB, destItmHnd, itmHnd.Item.Size);
            destItmHnd.Item.RHeader.UserProperty := itmHnd.Item.RHeader.UserProperty;
            destItmHnd.Item.RHeader.CreateTime := itmHnd.Item.RHeader.CreateTime;
            destItmHnd.Item.RHeader.ModificationTime := itmHnd.Item.RHeader.ModificationTime;
            DestDB.ItemClose(destItmHnd);
            ItemClose(itmHnd);

            if DestDB.Size > SplitSiz then
              begin
                DestDB.UpdateIO;
                fs := TCoreClassFileStream.Create(fn, fmCreate);
                DestDB.StreamEngine.Position := 0;
                ParallelCompressStream(TSelectCompressionMethod.scmZLIB_Max, TMemoryStream64(DestDB.StreamEngine), fs);
                DisposeObject(fs);
                DisposeObject(DestDB);

                fn := umlChangeFileExt(destFile, '') + umlIntToStr(spID) + umlGetFileExt(destFile);
                inc(spID);
                DestDB := TObjectDataManagerOfCache.CreateAsStream(FDBHandle.FixedStringL,
                  TMemoryStream64.CustomCreate(1024 * 1024), '', DefaultItemID, False, True, True);
                DestDB.OverWriteItem := False;
              end;
          end;

      until not RecursionSearchNext(sr);
    end;
  DestDB.UpdateIO;
  fs := TCoreClassFileStream.Create(fn, fmCreate);
  DestDB.StreamEngine.Position := 0;
  ParallelCompressStream(TSelectCompressionMethod.scmZLIB_Max, TMemoryStream64(DestDB.StreamEngine), fs);
  DisposeObject(fs);
  DisposeObject(DestDB);
end;

procedure TObjectDataManager.ExpPathToDisk(DBPath, ExpPath_: SystemString; IncludeSub: Boolean);
begin
  ExpPathToDiskP(DBPath, ExpPath_, IncludeSub, nil);
end;

procedure TObjectDataManager.ExpPathToDiskP(DBPath, ExpPath_: SystemString; IncludeSub: Boolean; Notify: TDBExpNotifyProc);
var
  rFieldPos: Int64;
  rs: TItemRecursionSearch;
  destPath: U_String;
  itmHnd: TItemHandle;
  fs: TCoreClassFileStream;
  sr: TItemSearch;
begin
  destPath := ExpPath_;
  umlCreateDirectory(destPath);
  if IncludeSub then
    begin
      if GetPathField(DBPath, rFieldPos) then
        begin
          if RecursionSearchFirst(DBPath, '*', rs) then
            begin
              repeat
                if rs.ReturnHeader.ID = DB_Header_Field_ID then
                  begin
                    destPath := umlCombinePath(ExpPath_, GetFieldPath(rs.ReturnHeader.CurrentHeader, rFieldPos));
                    umlCreateDirectory(destPath);
                  end
                else if ItemFastOpen(rs.ReturnHeader.CurrentHeader, itmHnd) then
                  begin
                    try
                      fs := TCoreClassFileStream.Create(umlCombineFileName(destPath, itmHnd.Name), fmCreate);
                      ItemReadToStream(itmHnd, fs);
                      DisposeObject(fs);
                    except
                        DoStatus('failed file stream %s', [umlCombineFileName(destPath, itmHnd.Name).Text]);
                    end;
                    ItemClose(itmHnd);
                    if Assigned(Notify) then
                        Notify(Self, rs.CurrentField.RHeader.CurrentHeader, itmHnd.Item.RHeader.CurrentHeader, umlCombineFileName(destPath, itmHnd.Name));
                  end;
              until not RecursionSearchNext(rs);
            end;
        end;
    end
  else if GetPathField(DBPath, rFieldPos) then
    begin
      if ItemFastFindFirst(rFieldPos, '*', sr) then
        begin
          repeat
            if ItemFastOpen(sr.HeaderPOS, itmHnd) then
              begin
                try
                  fs := TCoreClassFileStream.Create(umlCombineFileName(destPath, itmHnd.Name), fmCreate);
                  ItemReadToStream(itmHnd, fs);
                  DisposeObject(fs);
                except
                    DoStatus('failed file stream %s', [umlCombineFileName(destPath, itmHnd.Name).Text]);
                end;
                ItemClose(itmHnd);
                if Assigned(Notify) then
                    Notify(Self, rFieldPos, itmHnd.Item.RHeader.CurrentHeader, umlCombineFileName(destPath, itmHnd.Name));
              end;
          until not ItemFastFindNext(sr);
        end;
    end;
end;

procedure TObjectDataManager.ExpItemToDisk(DBPath, DBItem, ExpFilename_: SystemString);
var
  itmHnd: TItemHandle;
  fs: TCoreClassFileStream;
begin
  if ItemOpen(DBPath, DBItem, itmHnd) then
    begin
      try
        fs := TCoreClassFileStream.Create(ExpFilename_, fmCreate);
        ItemReadToStream(itmHnd, fs);
        DisposeObject(fs);
      except
          DoStatus('failed file stream %s', [ExpFilename_]);
      end;
      ItemClose(itmHnd);
    end;
end;

function TObjectDataManager.Is_BACKUP_Mode: Boolean;
begin
{$IFDEF ZDB_BACKUP}
  Result := FDBHandle.IOHnd.Handle is TReliableFileStream;
{$ELSE ZDB_BACKUP}
  Result := False;
{$ENDIF ZDB_BACKUP}
end;

function TObjectDataManager.Is_Flush_Mode: Boolean;
begin
{$IFDEF ZDB_PHYSICAL_FLUSH}
  Result := (not FDBHandle.IOHnd.IsOnlyRead)
    and (FDBHandle.IOHnd.IsOpen)
    and (FDBHandle.IOHnd.Handle is TReliableFileStream);
{$ELSE ZDB_PHYSICAL_FLUSH}
  Result := False;
{$ENDIF ZDB_PHYSICAL_FLUSH}
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
  Result := FDBHandle.State;
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

procedure TObjectDataManager.SetID(const ID_: Byte);
begin
  FDefaultItemID := ID_;
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

function TObjectDataManager.FastDelete(const fieldPos: Int64; const fPos: Int64): Boolean;
var
  FieldHnd: TFieldHandle;
begin
  Init_TField(FieldHnd);
  Result := False;
  if dbField_ReadRec(fieldPos, FDBHandle.IOHnd, FieldHnd) then
      Result := dbField_DeleteHeader(fPos, fieldPos, FDBHandle.IOHnd, FieldHnd);
end;

function TObjectDataManager.FastFieldExists(const fieldPos: Int64; const FieldName: SystemString): Boolean;
var
  FieldSearch: TFieldSearch;
begin
  Result := FieldFastFindFirst(fieldPos, FieldName, FieldSearch);
end;

function TObjectDataManager.FastFieldCreate(const fieldPos: Int64; const FieldName, FieldDescription: SystemString; var NewFieldPos: Int64): Boolean;
var
  NewField: TField;
begin
  Init_TField(NewField);
  NewField.Description := FieldDescription;
  Result := dbField_CreateField(FieldName, fieldPos, FDBHandle.IOHnd, NewField);
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

function TObjectDataManager.GetRootFieldPos(const RootName: SystemString): Int64;
var
  f: TFieldHandle;
begin
  Init_TField(f);
  if db_GetRootField(RootName, f, FDBHandle) then
      Result := f.RHeader.CurrentHeader
  else
      Result := -1;
end;

function TObjectDataManager.FieldRename(const fieldPos: Int64; const NewFieldName, NewFieldDescription: SystemString): Boolean;
var
  FieldHnd: TFieldHandle;
begin
  Result := False;
  if not umlExistsChar(NewFieldName, '\/') then
    begin
      Init_TField(FieldHnd);
      if dbField_ReadRec(fieldPos, FDBHandle.IOHnd, FieldHnd) then
        begin
          if (not FastFieldExists(FieldHnd.UpFieldPOS, NewFieldName)) and (FieldHnd.RHeader.CurrentHeader <> FDBHandle.DefaultFieldPOS) then
            begin
              FieldHnd.RHeader.Name := NewFieldName;
              FieldHnd.Description := NewFieldDescription;
              Result := dbField_WriteRec(fieldPos, FDBHandle.IOHnd, FieldHnd);
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

function TObjectDataManager.FieldExists(const DBPath: SystemString): Boolean;
var
  fieldPos: Int64;
begin
  Result := GetPathField(DBPath, fieldPos);
end;

function TObjectDataManager.FieldFastFindFirst(const fieldPos: Int64; const Filter: SystemString; var FieldSearchHandle: TFieldSearch): Boolean;
begin
  Init_TTMDBSearchField(FieldSearchHandle);
  Result := db_FastFindFirstField(fieldPos, Filter, FieldSearchHandle, FDBHandle);
end;

function TObjectDataManager.FieldFastFindLast(const fieldPos: Int64; const Filter: SystemString; var FieldSearchHandle: TFieldSearch): Boolean;
begin
  Init_TTMDBSearchField(FieldSearchHandle);
  Result := db_FastFindLastField(fieldPos, Filter, FieldSearchHandle, FDBHandle);
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

function TObjectDataManager.GetFieldData(const fieldPos: Int64; var dest: TFieldHandle): Boolean;
begin
  Init_TField(dest);
  Result := dbField_ReadRec(fieldPos, FDBHandle.IOHnd, dest);
end;

function TObjectDataManager.GetFieldPath(const fieldPos: Int64): SystemString;
var
  ReturnPath: U_String;
begin
  if db_GetPath(fieldPos, FDBHandle.DefaultFieldPOS, FDBHandle, ReturnPath) then
      Result := ReturnPath
  else
      Result := '';
end;

function TObjectDataManager.GetFieldPath(const fieldPos, RootFieldPos: Int64): SystemString;
var
  ReturnPath: U_String;
begin
  if db_GetPath(fieldPos, RootFieldPos, FDBHandle, ReturnPath) then
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
          if h.PositionID in [DB_Header_1, DB_Header_Last] then
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

function TObjectDataManager.GetFirstHeaderFromField(fieldPos: Int64; var h: THeader): Boolean;
var
  f: TField;
begin
  Result := (dbField_ReadRec(fieldPos, FDBHandle.IOHnd, f)) and (f.HeaderCount > 0);
  if Result then
    begin
      Result := GetHeader(f.FirstHeaderPOS, h);
    end;
end;

function TObjectDataManager.GetLastHeaderFromField(fieldPos: Int64; var h: THeader): Boolean;
var
  f: TField;
begin
  Result := (dbField_ReadRec(fieldPos, FDBHandle.IOHnd, f)) and (f.HeaderCount > 0);
  if Result then
      Result := GetHeader(f.LastHeaderPOS, h);
end;

function TObjectDataManager.GetHeader(hPos: Int64; var h: THeader): Boolean;
begin
  Result := dbHeader_ReadRec(hPos, FDBHandle.IOHnd, h);
end;

function TObjectDataManager.GetItemSize(const DBPath, DBItemName: SystemString): Int64;
var
  DBItemHandle: TItemHandle;
begin
  Init_TTMDBItemHandle(DBItemHandle);
  if db_GetItem(DBPath, DBItemName, FDefaultItemID, DBItemHandle.Item, FDBHandle) then
      Result := DBItemHandle.Item.Size
  else
      Result := 0;
end;

function TObjectDataManager.ItemCreate(const DBPath, DBItemName, DBItemDescription: SystemString; var ItemHnd: TItemHandle): Boolean;
{
  It can automatically create a path
}
begin
  Init_TTMDBItemHandle(ItemHnd);
  Result := db_ItemCreate(DBPath, DBItemName, DBItemDescription, FDefaultItemID, ItemHnd, FDBHandle);
end;

function TObjectDataManager.ItemDelete(const DBPath, DBItemName: SystemString): Boolean;
begin
  Result := db_DeleteItem(DBPath, DBItemName, FDefaultItemID, FDBHandle);
end;

function TObjectDataManager.ItemExists(const DBPath, DBItemName: SystemString): Boolean;
var
  ItemSearchHnd: TItemSearch;
begin
  Init_TTMDBSearchItem(ItemSearchHnd);
  Result := db_FindFirstItem(DBPath, DBItemName, FDefaultItemID, ItemSearchHnd, FDBHandle);
end;

function TObjectDataManager.ItemOpen(const DBPath, DBItemName: SystemString; var ItemHnd: TItemHandle): Boolean;
begin
  Init_TTMDBItemHandle(ItemHnd);
  Result := db_ItemOpen(DBPath, DBItemName, FDefaultItemID, ItemHnd, FDBHandle);
end;

function TObjectDataManager.ItemAutoOpenOrCreate(const DBPath, DBItemName, DBItemDescription: SystemString; var ItemHnd: TItemHandle): Boolean;
begin
  if ItemExists(DBPath, DBItemName) then
      Result := ItemOpen(DBPath, DBItemName, ItemHnd)
  else
      Result := ItemCreate(DBPath, DBItemName, DBItemDescription, ItemHnd);
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
  buff: array [0 .. C_BufferChunkSize] of Byte;
begin
  Result := False;
  if CopySize > C_BufferChunkSize then
    begin
      for i := 1 to (CopySize div C_BufferChunkSize) do
        begin
          if not ItemRead(ItemHnd, C_BufferChunkSize, buff) then
              Exit;
          if not DestDB.ItemWrite(DestItemHandle, C_BufferChunkSize, buff) then
              Exit;
        end;
      if (CopySize mod C_BufferChunkSize) > 0 then
        begin
          if not ItemRead(ItemHnd, CopySize mod C_BufferChunkSize, buff) then
              Exit;
          if not DestDB.ItemWrite(DestItemHandle, CopySize mod C_BufferChunkSize, buff) then
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

function TObjectDataManager.ItemRename(const fieldPos: Int64; var ItemHnd: TItemHandle; const NewName, NewDescription: SystemString): Boolean;
begin
  Result := db_ItemReName(fieldPos, NewName, NewDescription, ItemHnd, FDBHandle);
end;

function TObjectDataManager.ItemFastInsertNew(const fieldPos, InsertHeaderPos: Int64; const DBItemName, DBItemDescription: SystemString; var ItemHnd: TItemHandle): Boolean;
begin
  Init_TTMDBItemHandle(ItemHnd);
  Result := db_ItemFastInsertNew(DBItemName, DBItemDescription, fieldPos, InsertHeaderPos, FDefaultItemID, ItemHnd, FDBHandle);
end;

function TObjectDataManager.ItemFastCreate(const fPos: Int64; const DBItemName, DBItemDescription: SystemString; var ItemHnd: TItemHandle): Boolean;
begin
  Init_TTMDBItemHandle(ItemHnd);
  Result := db_ItemFastCreate(DBItemName, DBItemDescription, fPos, FDefaultItemID, ItemHnd, FDBHandle);
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

function TObjectDataManager.ItemFastExists(const fieldPos: Int64; const DBItemName: SystemString): Boolean;
var
  ItemSearchHnd: TItemSearch;
begin
  Init_TTMDBSearchItem(ItemSearchHnd);
  Result := db_FastFindFirstItem(fieldPos, DBItemName, FDefaultItemID, ItemSearchHnd, FDBHandle);
end;

function TObjectDataManager.ItemFastFindFirst(const fieldPos: Int64; const DBItemName: SystemString; var ItemSearchHandle: TItemSearch): Boolean;
begin
  Init_TTMDBSearchItem(ItemSearchHandle);
  Result := db_FastFindFirstItem(fieldPos, DBItemName, FDefaultItemID, ItemSearchHandle, FDBHandle);
end;

function TObjectDataManager.ItemFastFindLast(const fieldPos: Int64; const DBItemName: SystemString; var ItemSearchHandle: TItemSearch): Boolean;
begin
  Init_TTMDBSearchItem(ItemSearchHandle);
  Result := db_FastFindLastItem(fieldPos, DBItemName, FDefaultItemID, ItemSearchHandle, FDBHandle);
end;

function TObjectDataManager.ItemFastFindNext(var ItemSearchHandle: TItemSearch): Boolean;
begin
  Result := db_FastFindNextItem(ItemSearchHandle, FDefaultItemID, FDBHandle);
end;

function TObjectDataManager.ItemFastFindPrev(var ItemSearchHandle: TItemSearch): Boolean;
begin
  Result := db_FastFindPrevItem(ItemSearchHandle, FDefaultItemID, FDBHandle);
end;

function TObjectDataManager.ItemFindFirst(const DBPath, DBItemName: SystemString; var ItemSearchHandle: TItemSearch): Boolean;
begin
  Init_TTMDBSearchItem(ItemSearchHandle);
  Result := db_FindFirstItem(DBPath, DBItemName, FDefaultItemID, ItemSearchHandle, FDBHandle);
end;

function TObjectDataManager.ItemFindLast(const DBPath, DBItemName: SystemString; var ItemSearchHandle: TItemSearch): Boolean;
begin
  Init_TTMDBSearchItem(ItemSearchHandle);
  Result := db_FindLastItem(DBPath, DBItemName, FDefaultItemID, ItemSearchHandle, FDBHandle);
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
  sour.UpdateHandle;
  DisposeObject(sour);
end;

function TObjectDataManager.ItemReadToStream(const DBPath, DBItemName: SystemString; stream: TCoreClassStream): Boolean;
var
  itmHnd: TItemHandle;
begin
  Result := False;
  if not ItemOpen(DBPath, DBItemName, itmHnd) then
      Exit;
  Result := ItemReadToStream(itmHnd, stream);
end;

function TObjectDataManager.ItemWriteFromStream(const DBPath, DBItemName: SystemString; stream: TCoreClassStream): Boolean;
var
  itmHnd: TItemHandle;
begin
  Result := False;
  if ItemCreate(DBPath, DBItemName, DBItemName, itmHnd) then
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

function TObjectDataManager.HandlePtr: PObjectDataHandle;
begin
  Result := @FDBHandle;
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
      m64 := TMemoryStream64.CustomCreate(Get_DB_HeaderL(FDBHandle.IOHnd));
      FPrepareWritePool.Add(fPos, m64, False);
    end;
  InitIOHnd(Hnd);
  Hnd.FixedStringL := FDBHandle.FixedStringL;
  umlFileOpenAsStream('', m64, Hnd, False);
  dbHeader_WriteRec(0, Hnd, wVal);
  umlFileClose(Hnd);
  if m64.Position <> Get_DB_HeaderL(FDBHandle.IOHnd) then
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

  p^.State := DB_Header_ok;
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
          Hnd.FixedStringL := FDBHandle.FixedStringL;
          umlFileOpenAsStream('', m64, Hnd, False);
          Done := dbHeader_ReadRec(0, Hnd, rVal);
          umlFileClose(Hnd);
          m64.Position := 0;
          if Done then
            begin
              new(p);
              p^ := rVal;
              FHeaderCache.Add(rVal.CurrentHeader, p, False);
              p^.State := DB_Header_ok;
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
      m64 := TMemoryStream64.CustomCreate(Get_DB_BlockL(FDBHandle.IOHnd));
      FPrepareWritePool.Add(fPos, m64, False);
    end;
  InitIOHnd(Hnd);
  Hnd.FixedStringL := FDBHandle.FixedStringL;
  umlFileOpenAsStream('', m64, Hnd, False);
  dbItem_OnlyWriteItemBlockRec(0, Hnd, wVal);
  umlFileClose(Hnd);
  if m64.Position <> Get_DB_BlockL(FDBHandle.IOHnd) then
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

  p^.State := DB_Item_ok;
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
          Hnd.FixedStringL := FDBHandle.FixedStringL;
          umlFileOpenAsStream('', m64, Hnd, False);
          Done := dbItem_OnlyReadItemBlockRec(0, Hnd, rVal);
          umlFileClose(Hnd);
          m64.Position := 0;
          if Done then
            begin
              new(p);
              p^ := rVal;
              FItemBlockCache.Add(rVal.CurrentBlockPOS, p, False);
              p^.State := DB_Item_ok;
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
      m64 := TMemoryStream64.CustomCreate(Get_DB_ItemL(FDBHandle.IOHnd));
      FPrepareWritePool.Add(fPos, m64, False);
    end;
  InitIOHnd(Hnd);
  Hnd.FixedStringL := FDBHandle.FixedStringL;
  umlFileOpenAsStream('', m64, Hnd, False);
  dbItem_OnlyWriteItemRec(0, Hnd, wVal);
  umlFileClose(Hnd);
  if m64.Position <> Get_DB_ItemL(FDBHandle.IOHnd) then
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

  p^.State := DB_Item_ok;
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
          Hnd.FixedStringL := FDBHandle.FixedStringL;
          umlFileOpenAsStream('', m64, Hnd, False);
          Done := dbItem_OnlyReadItemRec(0, Hnd, rVal);
          umlFileClose(Hnd);
          m64.Position := 0;
          if Done then
            begin
              new(p);
              p^.read(rVal);
              FItemCache.Add(fPos, p, False);
              p^.State := DB_Item_ok;
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
      m64 := TMemoryStream64.CustomCreate(Get_DB_FieldL(FDBHandle.IOHnd));
      FPrepareWritePool.Add(fPos, m64, False);
    end;
  InitIOHnd(Hnd);
  Hnd.FixedStringL := FDBHandle.FixedStringL;
  umlFileOpenAsStream('', m64, Hnd, False);
  dbField_OnlyWriteFieldRec(0, Hnd, wVal);
  umlFileClose(Hnd);
  if m64.Position <> Get_DB_FieldL(FDBHandle.IOHnd) then
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

  p^.State := DB_Field_ok;
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
          Hnd.FixedStringL := FDBHandle.FixedStringL;
          umlFileOpenAsStream('', m64, Hnd, False);
          Done := dbField_OnlyReadFieldRec(0, Hnd, rVal);
          umlFileClose(Hnd);
          m64.Position := 0;
          if Done then
            begin
              new(p);
              p^.read(rVal);
              FFieldCache.Add(fPos, p, False);
              p^.State := DB_Item_ok;
            end;
        end;
    end
  else
      p^.read(rVal);
end;

procedure TObjectDataManagerOfCache.PrepareTMDBWriteProc(fPos: Int64; const wVal: PObjectDataHandle; var Done: Boolean);
begin
  Done := CheckPreapreWrite(fPos);
end;

procedure TObjectDataManagerOfCache.TMDBWriteProc(fPos: Int64; const wVal: PObjectDataHandle);
var
  m64: TMemoryStream64;
  Hnd: TIOHnd;
begin
  m64 := TMemoryStream64(FPrepareWritePool[fPos]);
  if m64 = nil then
    begin
      m64 := TMemoryStream64.CustomCreate(Get_DB_L(FDBHandle.IOHnd));
      FPrepareWritePool.Add(fPos, m64, False);
    end;
  InitIOHnd(Hnd);
  Hnd.FixedStringL := FDBHandle.FixedStringL;
  umlFileOpenAsStream('', m64, Hnd, False);

  FDBHandle.IOHnd.Data := nil;
  db_WriteRec(0, Hnd, wVal^);
  FDBHandle.IOHnd.Data := @FDBHandle;

  umlFileClose(Hnd);
  if m64.Position <> Get_DB_L(FDBHandle.IOHnd) then
      RaiseInfo('write error!');
  m64.Position := 0;
end;

procedure TObjectDataManagerOfCache.TMDBReadProc(fPos: Int64; const rVal: PObjectDataHandle; var Done: Boolean);
var
  m64: TMemoryStream64;
  Hnd: TIOHnd;
begin
  m64 := TMemoryStream64(FPrepareWritePool[fPos]);
  if m64 <> nil then
    begin
      InitIOHnd(Hnd);
      Hnd.FixedStringL := FDBHandle.FixedStringL;
      umlFileOpenAsStream('', m64, Hnd, False);
      FDBHandle.IOHnd.Data := nil;
      Done := db_ReadRec(0, Hnd, rVal^);
      FDBHandle.IOHnd.Data := @FDBHandle;
      umlFileClose(Hnd);
      m64.Position := 0;
    end;
end;

procedure TObjectDataManagerOfCache.DoOpenBefore;
begin
  inherited DoOpenBefore;

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
  swapTotal: Integer;
  p: PInt64HashListObjectStruct;
  m64: TMemoryStream64;
  swapFileName: TPascalString;
  swapHnd: TCoreClassFileStream;
  swapHead: TSwapHead;
begin
  // update db header
  inherited UpdateIO;

  if (not FDBHandle.IOHnd.IsOnlyRead)
    and (FDBHandle.IOHnd.IsOpen)
    and (FPrepareWritePool.Count > 0) then
    begin
      // step 1: flush to swap file
{$IFDEF ZDB_PHYSICAL_FLUSH}
      if (FDBHandle.IOHnd.Handle is TReliableFileStream) then
        begin
          swapFileName := TReliableFileStream(FDBHandle.IOHnd.Handle).fileName + SFlush;
          swapHnd := nil;
          try
            swapHnd := TCoreClassFileStream.Create(swapFileName, fmCreate);

            swapTotal := FPrepareWritePool.Count;
            swapHnd.write(swapTotal, C_Integer_Size);

            i := 0;
            p := FPrepareWritePool.FirstPtr;
            while i < FPrepareWritePool.Count do
              begin
                m64 := TMemoryStream64(p^.Data);
                if p^.i64 >= FDBHandle.IOHnd.Size then
                    RaiseInfo('flush: prepare write buffer error!');

                swapHead.Size := m64.Size;
                swapHead.MD5 := umlMD5(m64.Memory, m64.Size);
                swapHead.Position := p^.i64;
                swapHnd.write(swapHead, SizeOf(swapHead));
                swapHnd.write(m64.Memory^, m64.Size);
                inc(i);
                p := p^.Next;
              end;
          except
          end;
          DisposeObject(swapHnd);
        end;
{$ENDIF ZDB_PHYSICAL_FLUSH}
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

{$IFDEF ZDB_PHYSICAL_FLUSH}
      // step 3: delete swap file
      if (FDBHandle.IOHnd.Handle is TReliableFileStream) then
          umlDeleteFile(swapFileName);
{$ENDIF ZDB_PHYSICAL_FLUSH}
    end;
  FPrepareWritePool.Clear;
end;

function TObjectDataManagerOfCache.CacheStatus: SystemString;
begin
  Result := PFormat('header %d block %d item %d field %d prepare %d', [FHeaderCache.Count, FItemBlockCache.Count, FItemCache.Count, FFieldCache.Count, FPrepareWritePool.Count]);
end;

function TObjectDataMarshal.GetItems(aIndex: Integer): TObjectDataManager;
begin
  Result := TObjectDataManager(FLibList.Objects[aIndex]);
end;

function TObjectDataMarshal.GetNames(Name_: SystemString): TObjectDataManager;
var
  i: Integer;
  aUName: SystemString;
begin
  Result := nil;
  aUName := GetAbsoluteFileName(Name_);
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

function TObjectDataMarshal.NewDB(FixedStringL: Byte; dbFile: SystemString; dbOnlyRead: Boolean): TObjectDataManager;
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
      Result := TObjectDataManager.CreateNew(FixedStringL, dbFile, FID);
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
      Result := TObjectDataManager.Open(dbFile, FID, dbOnlyRead);
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

procedure TObjectDataMarshal.DeleteFromName(Name_: SystemString);
var
  i: Integer;
  aUName: SystemString;
begin
  aUName := GetAbsoluteFileName(Name_);
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
        Items[i].DoOpen;
end;

procedure TestObjectData_(c: TObjectDataManagerClass);
var
  db: TObjectDataManager;
  itmHnd1, itmHnd2, itmHnd3, itmHnd4: TItemHandle;
  buff: TBytes;
  nameL: U_StringArray;
  n: U_String;
begin
  db := c.CreateAsStream($FF, TMemoryStream64.CustomCreate($FFFF), '', 0, False, True, True);

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
