{******************************************************************************}
{* Low Object DB Imp library                                                  *}
{* https://github.com/PassByYou888/CoreCipher                                 *}
(* https://github.com/PassByYou888/ZServer4D                                  *)
{******************************************************************************}


unit ObjectData;

{$I zDefine.inc}

interface

uses UnicodeMixedLib;

{ .$DEFINE INLINE_ASM }

const
  umlVersionLength  = 8;
  umlTimeLength     = 8;
  umlCountLength    = 8;
  umlSizeLength     = 8;
  umlPositionLength = 8;
  umlIDLength       = 1;
  umlPropertyLength = 4;
  umlLevelLength    = 2;

  db_Pack_MajorVersion = 2;
  db_Pack_MinorVersion = 1;
  MaxSecursionLevel    = 256;

  db_Pack_FileDescription    = 'OD';
  db_Pack_DefaultDescription = 'Field';

  db_PathChar = '/';

  db_Header_RecLength                      = (umlPositionLength * 4) + (umlTimeLength * 2) + (umlIDLength * 2) + (umlPropertyLength * 1) + ((umlDefaultStrSize + umlDefaultStrLengthSize) * 1);
  db_Item_RecLength                        = ((umlDefaultStrSize + umlDefaultStrLengthSize) * 1) + (umlIDLength * 1) + (umlPositionLength * 2) + (umlSizeLength * 1) + (umlCountLength * 1);
  db_Item_BlockRecLength                   = (umlIDLength * 1) + (umlPositionLength * 4) + (umlSizeLength * 1);
  db_Field_RecLength                       = ((umlDefaultStrSize + umlDefaultStrLengthSize) * 1) + (umlCountLength * 1) + (umlPositionLength * 3);
  db_Pack_RecLength                        = ((umlDefaultStrSize + umlDefaultStrLengthSize) * 3) + (umlVersionLength * 2) + (umlTimeLength * 2) + (umlCountLength * 1) + (umlPositionLength * 4) + (umlLevelLength * 1);
  db_ItemStructDescriptionHandle_RecLength = ((umlDefaultStrSize + umlDefaultStrLengthSize) * 1) + (umlPositionLength * 3) + (umlIDLength * 2) + (umlSizeLength * 1);

  db_Header_FieldID             = 21;
  db_Header_ItemID              = 22;
  db_Header_FirstPositionFlags  = 11;
  db_Header_MediumPositionFlags = 12;
  db_Header_LastPositionFlags   = 13;
  db_Header_OnlyPositionFlags   = 14;
  db_item_OnlyPositionFlags     = 33;
  db_item_FirstPositionFlags    = 34;
  db_item_MediumPositionFlags   = 35;
  db_item_LastPositionFlags     = 36;

type
  THeader = record
    CurrentHeader, NextHeader, PrevHeader, DataMainPOS: Int64;
    CreateTime, LastModifyTime: Double;
    ID: Byte;
    PositionID: Byte;
    UserProperty: Cardinal; // external define
    Name: string;
    Return: Int64;
  end;

  TItemBlock = record
    IDFlags: Byte;
    CurrentBlockPOS, NextBlockPOS, PrevBlockPOS, DataBlockPOS: Int64;
    Size: Int64;
    Return: Int64;
  end;

  TItem = record
    RHeader: THeader;
    Description: string;
    ExtID: Byte;
    FirstBlockPOS, LastBlockPOS: Int64;
    Size: Int64;
    BlockCount: Int64;
    CurrentBlockSeekPOS: Int64;
    CurrentFileSeekPOS: Int64;
    CurrentItemBlock: TItemBlock;
    DataModify: Boolean;
    Return: Int64;
  end;

  TField = record
    HigherFieldPOS: Int64;
    Description: string;
    HeaderCount: Int64;
    FirstHeaderPOS, LastHeaderPOS: Int64;
    RHeader: THeader;
    Return: Int64;
  end;

  TFieldSearch = record
    InitFlags: Boolean;
    Name: string;
    StartPOS, OverPOS: Int64;
    ID: Byte;
    PositionID: Byte;
    RHeader: THeader;
    Return: Int64;
  end;

  TTMDB = record
    FileDescription: string;
    Name: string;
    Description: string;
    MajorVer: SmallInt;
    MinorVer: SmallInt;
    CreateTime, LastModifyTime: Double;
    RootHeaderCount: Int64;
    DefaultFieldPOS, FirstHeaderPOS, LastHeaderPOS, CurrentFieldPOS: Int64;
    CurrentFieldLevel: Word;
    RecFile: TRecFile;
    OverWriteItem: Boolean;
    WriteFlags: Boolean;
    SameItemName: Boolean;
    Return: Int64;
  end;

  TTMDBItemHandle = record
    Item: TItem;
    Path: string;
    Name: string;
    Description: string;
    CreateTime, LastModifyTime: Double;
    ItemExtID: Byte;
    OpenFlags: Boolean;
  end;

  TTMDBSearchHeader = record
    Name: string;
    ID: Byte;
    CreateTime, LastModifyTime: Double;
    HeaderPOS: Int64;
    CompleteCount: Int64;
    FieldSearch: TFieldSearch;
  end;

  TTMDBSearchItem = record
    Name: string;
    Description: string;
    ExtID: Byte;
    Size: Int64;
    HeaderPOS: Int64;
    CompleteCount: Int64;
    FieldSearch: TFieldSearch;
  end;

  TTMDBSearchField = record
    Name: string;
    Description: string;
    HeaderCount: Int64;
    HeaderPOS: Int64;
    CompleteCount: Int64;
    FieldSearch: TFieldSearch;
  end;

  TTMDBDescriptionHandle = record
    StructVarID: Byte;
    StructDescription: string;
    StructNextPos, StructCurrentPos, StructPublicPos: Int64;
    StructSize: Int64;
    StructPositionID: Byte;
  end;

  TTMDBItemStruct = record
    Description: string;
    StructCount: Int64;
    StructFirstPos, StructLastPos, ItemStructCurrentPos: Int64;
    DescriptionHandle: TTMDBDescriptionHandle;
  end;

  TTMDBRecursionSearch = record
    ReturnHeader: THeader;
    CurrentField: TField;
    InitPath: string;
    FilterName: string;
    SearchBuffGo: Integer;
    SearchBuff: array [0 .. MaxSecursionLevel] of TFieldSearch;
  end;

procedure InitTHeader(var SenderHeader: THeader); {$IFDEF INLINE_ASM}inline; {$ENDIF}
procedure InitTItemBlock(var SenderItemBlock: TItemBlock); {$IFDEF INLINE_ASM}inline; {$ENDIF}
procedure InitTItem(var SenderItem: TItem); {$IFDEF INLINE_ASM}inline; {$ENDIF}
procedure InitTField(var SenderField: TField); {$IFDEF INLINE_ASM}inline; {$ENDIF}
procedure InitTFieldSearch(var SenderFieldSearch: TFieldSearch); {$IFDEF INLINE_ASM}inline; {$ENDIF}
procedure InitTTMDB(var SenderTMDB: TTMDB); {$IFDEF INLINE_ASM}inline; {$ENDIF}
procedure InitTTMDBItemHandle(var SenderTMDBItemHandle: TTMDBItemHandle); {$IFDEF INLINE_ASM}inline; {$ENDIF}
procedure InitTTMDBSearchHeader(var SenderTMDBSearchHeader: TTMDBSearchHeader); {$IFDEF INLINE_ASM}inline; {$ENDIF}
procedure InitTTMDBSearchItem(var SenderTMDBSearchItem: TTMDBSearchItem); {$IFDEF INLINE_ASM}inline; {$ENDIF}
procedure InitTTMDBSearchField(var SenderTMDBSearchField: TTMDBSearchField); {$IFDEF INLINE_ASM}inline; {$ENDIF}
procedure InitTTMDBDescriptionHandle(var SenderTMDBDescriptionHandle: TTMDBDescriptionHandle); {$IFDEF INLINE_ASM}inline; {$ENDIF}
procedure InitTTMDBItemStruct(var SenderTMDBItemStruct: TTMDBItemStruct); {$IFDEF INLINE_ASM}inline; {$ENDIF}
procedure InitTTMDBRecursionSearch(var SenderTMDBRecursionSearch: TTMDBRecursionSearch); {$IFDEF INLINE_ASM}inline; {$ENDIF}

function dbHeader_WriteRec(const POS: Int64; var RecFile: TRecFile; var SenderHeader: THeader): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbHeader_ReadRec(const POS: Int64; var RecFile: TRecFile; var SenderHeader: THeader): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}

function dbHeader_GetNextRec(const POS: Int64; var RecFile: TRecFile): THeader; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbHeader_GetPrevRec(const POS: Int64; var RecFile: TRecFile): THeader; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbHeader_GetPOSRec(const POS: Int64; var RecFile: TRecFile): THeader; {$IFDEF INLINE_ASM}inline; {$ENDIF}

function dbHeader_MultipleMatch(const SourStr, DestStr: string): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}

function dbHeader_FindNext(const Name: string; const FirstHeaderPOS, LastHeaderPOS: Int64; var RecFile: TRecFile; var SenderHeader: THeader): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbHeader_FindPrev(const Name: string; const LastHeaderPOS, FirstHeaderPOS: Int64; var RecFile: TRecFile; var SenderHeader: THeader): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}

function dbItem_WriteRec(const POS: Int64; var RecFile: TRecFile; var SenderItem: TItem): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbItem_ReadRec(const POS: Int64; var RecFile: TRecFile; var SenderItem: TItem): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}

function dbItem_OnlyWriteItemRec(const POS: Int64; var RecFile: TRecFile; var SenderItem: TItem): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbItem_OnlyReadItemRec(const POS: Int64; var RecFile: TRecFile; var SenderItem: TItem): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}

function dbItem_OnlyWriteItemBlockRec(const POS: Int64; var RecFile: TRecFile; var SenderItemBlock: TItemBlock): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbItem_OnlyReadItemBlockRec(const POS: Int64; var RecFile: TRecFile; var SenderItemBlock: TItemBlock): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}

function dbItem_BlockCreate(var RecFile: TRecFile; var SenderItem: TItem): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbItem_BlockInit(var RecFile: TRecFile; var SenderItem: TItem): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbItem_BlockReadData(var RecFile: TRecFile; var SenderItem: TItem; var Buffers; const _Size: Int64): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbItem_BlockAppendWriteData(var RecFile: TRecFile; var SenderItem: TItem; var Buffers; const Size: Int64): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbItem_BlockWriteData(var RecFile: TRecFile; var SenderItem: TItem; var Buffers; const Size: Int64): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbItem_BlockSeekPOS(var RecFile: TRecFile; var SenderItem: TItem; const Position: Int64): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbItem_BlockGetPOS(var RecFile: TRecFile; var SenderItem: TItem): Int64; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbItem_BlockSeekStartPOS(var RecFile: TRecFile; var SenderItem: TItem): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbItem_BlockSeekLastPOS(var RecFile: TRecFile; var SenderItem: TItem): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}

function dbField_WriteRec(const POS: Int64; var RecFile: TRecFile; var SenderField: TField): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbField_ReadRec(const POS: Int64; var RecFile: TRecFile; var SenderField: TField): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbField_OnlyWriteFieldRec(const POS: Int64; var RecFile: TRecFile; var SenderField: TField): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbField_OnlyReadFieldRec(const POS: Int64; var RecFile: TRecFile; var SenderField: TField): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbField_GetPOSField(const POS: Int64; var RecFile: TRecFile): TField; {$IFDEF INLINE_ASM}inline; {$ENDIF}

function dbField_GetFirstHeader(const POS: Int64; var RecFile: TRecFile): THeader; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbField_GetLastHeader(const POS: Int64; var RecFile: TRecFile): THeader; {$IFDEF INLINE_ASM}inline; {$ENDIF}

function dbField_OnlyFindFirstName(const Name: string; const POS: Int64; var RecFile: TRecFile; var SenderFieldSearch: TFieldSearch): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbField_OnlyFindNextName(var RecFile: TRecFile; var SenderFieldSearch: TFieldSearch): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbField_OnlyFindLastName(const Name: string; const POS: Int64; var RecFile: TRecFile; var SenderFieldSearch: TFieldSearch): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbField_OnlyFindPrevName(var RecFile: TRecFile; var SenderFieldSearch: TFieldSearch): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}

function dbField_FindFirst(const Name: string; const ID: Int64; const POS: Int64; var RecFile: TRecFile; var SenderFieldSearch: TFieldSearch): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbField_FindNext(var RecFile: TRecFile; var SenderFieldSearch: TFieldSearch): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbField_FindLast(const Name: string; const ID: Int64; const POS: Int64; var RecFile: TRecFile; var SenderFieldSearch: TFieldSearch): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbField_FindPrev(var RecFile: TRecFile; var SenderFieldSearch: TFieldSearch): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}

function dbField_FindFirstItem(const Name: string; const ItemExtID: Int64; const POS: Int64; var RecFile: TRecFile; var SenderFieldSearch: TFieldSearch): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbField_FindNextItem(const ItemExtID: Int64; var RecFile: TRecFile; var SenderFieldSearch: TFieldSearch): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbField_FindLastItem(const Name: string; const ItemExtID: Int64; const POS: Int64; var RecFile: TRecFile; var SenderFieldSearch: TFieldSearch): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbField_FindPrevItem(const ItemExtID: Int64; var RecFile: TRecFile; var SenderFieldSearch: TFieldSearch): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbField_ExistItem(const Name: string; const ItemExtID: Int64; const POS: Int64; var RecFile: TRecFile): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}

function dbField_ExistHeader(const Name: string; const ID: Int64; const POS: Int64; var RecFile: TRecFile): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}

function dbField_CreateHeader(const Name: string; const ID: Int64; const POS: Int64; var RecFile: TRecFile; var SenderHeader: THeader): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbField_DeleteHeader(const HeaderPOS, FieldPos: Int64; var RecFile: TRecFile; var SenderField: TField): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbField_MoveHeader(const HeaderPOS: Int64; const SourcerFieldPOS, TargetFieldPos: Int64; var RecFile: TRecFile; var SenderField: TField): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}

function dbField_CreateField(const Name: string; const POS: Int64; var RecFile: TRecFile; var SenderField: TField): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}

function dbField_CreateItem(const Name: string; const ExterID: Int64; const POS: Int64; var RecFile: TRecFile; var SenderItem: TItem): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}

function dbField_CopyItem(var SenderItem: TItem; var RecFile: TRecFile; const DestFieldPos: Int64; var DestRecFile: TRecFile): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbField_CopyItemBuffer(var SenderItem: TItem; var RecFile: TRecFile; var DestItemHnd: TItem; var DestRecFile: TRecFile): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbField_CopyAllTo(const FilterName: string; const FieldPos: Int64; var RecFile: TRecFile; const DestFieldPos: Int64; var DestRecFile: TRecFile): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}

function dbPack_WriteRec(const POS: Int64; var RecFile: TRecFile; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbPack_ReadRec(const POS: Int64; var RecFile: TRecFile; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}

function dbPack_CreatePack(const Name, Description: string; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbPack_OpenPack(const Name: string; var SenderTMDB: TTMDB; _OnlyRead: Boolean): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}

function dbPack_CreateAsStream(Stream: TMixedStream; const Name, Description: string; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbPack_OpenAsStream(Stream: TMixedStream; const Name: string; var SenderTMDB: TTMDB; _OnlyRead: Boolean): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}

function dbPack_ClosePack(var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}

function dbPack_CopyFieldTo(const FilterName: string; var SenderTMDB: TTMDB; const SourceFieldPos: Int64; var DestTMDB: TTMDB; const DestFieldPos: Int64): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbPack_CopyAllTo(var SenderTMDB: TTMDB; var DestTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbPack_CopyAllToDestPath(var SenderTMDB: TTMDB; var DestTMDB: TTMDB; DestPath: string): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}

function dbPack_Update(var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}

function dbPack_TestNameStr(const Name: string): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}

function dbPack_AutoCheckRootField(const Name: string; var SenderField: TField; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbPack_CreateRootHeader(const Name: string; const ID: Int64; var SenderTMDB: TTMDB; var SenderHeader: THeader): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbPack_CreateRootField(const Name, Description: string; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbPack_CreateAndSetRootField(const Name, Description: string; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbPack_CreateField(const PathName, Description: string; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}

function dbPack_SetFieldName(const PathName, OriginFieldName, NewFieldName, FieldDescription: string; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbPack_SetItemName(const PathName, OriginItemName, NewItemName, ItemDescription: string; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbPack_DeleteField(const PathName, FilterName: string; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}

function dbPack_DeleteHeader(const PathName, FilterName: string; const ID: Int64; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}

function dbPack_MoveItem(const SourcerPathName, FilterName: string; const TargetPathName: string; const ItemExtID: Int64; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}

function dbPack_MoveField(const SourcerPathName, FilterName: string; const TargetPathName: string; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}

function dbPack_MoveHeader(const SourcerPathName, FilterName: string; const TargetPathName: string; const HeaderID: Int64; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}

function dbPack_SetCurrentRootField(const Name: string; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbPack_SetCurrentField(const PathName: string; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}

function dbPack_GetRootField(const Name: string; var SenderField: TField; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbPack_GetField(const PathName: string; var SenderField: TField; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbPack_GetPath(const FieldPos, RootFieldPos: Int64; var SenderTMDB: TTMDB; var RetPath: string): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}

function dbPack_NewItem(const PathName, ItemName, ItemDescription: string; const ItemExtID: Int64; var SenderItem: TItem; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}

function dbPack_DeleteItem(const PathName, FilterName: string; const ItemExtID: Int64; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}

function dbPack_GetItem(const PathName, ItemName: string; const ItemExtID: Int64; var SenderItem: TItem; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}

function dbPack_ItemCreate(const PathName, ItemName, ItemDescription: string; const ItemExtID: Int64; var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbPack_ItemFastCreate(const ItemName, ItemDescription: string; const POS: Int64; const ItemExtID: Int64; var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline;
{$ENDIF}
function dbPack_ItemOpen(const PathName, ItemName: string; const ItemExtID: Int64; var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbPack_ItemFastOpen(const POS: Int64; const ItemExtID: Int64; var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbPack_ItemClose(var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbPack_ItemUpdate(var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbPack_ItemBodyReset(var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbPack_ItemReName(const FieldPos: Int64; const NewItemName, NewItemDescription: string; var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}

function dbPack_ItemRead(const Size: Int64; var Buffers; var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbPack_ItemWrite(const Size: Int64; var Buffers; var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbPack_ItemReadStr(var Name: string; var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbPack_ItemWriteStr(const Name: string; var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}

function dbPack_ItemSeekPos(const POS: Int64; var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbPack_ItemSeekStartPos(var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbPack_ItemSeekLastPos(var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbPack_ItemGetPos(var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Int64; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbPack_ItemGetSize(var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Int64; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbPack_AppendItemSize(var SenderTMDBItemHandle: TTMDBItemHandle; const Size: Int64; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}

function dbPack_ExistsRootField(const Name: string; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbPack_FindFirstHeader(const PathName, FilterName: string; const ID: Int64; var SenderSearch: TTMDBSearchHeader; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbPack_FindNextHeader(var SenderSearch: TTMDBSearchHeader; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbPack_FindLastHeader(const PathName, FilterName: string; const ID: Int64; var SenderSearch: TTMDBSearchHeader; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbPack_FindPrevHeader(var SenderSearch: TTMDBSearchHeader; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}

function dbPack_FindFirstItem(const PathName, FilterName: string; const ItemExtID: Int64; var SenderSearch: TTMDBSearchItem; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbPack_FindNextItem(var SenderSearch: TTMDBSearchItem; const ItemExtID: Int64; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbPack_FindLastItem(const PathName, FilterName: string; const ItemExtID: Int64; var SenderSearch: TTMDBSearchItem; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbPack_FindPrevItem(var SenderSearch: TTMDBSearchItem; const ItemExtID: Int64; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}

function dbPack_FastFindFirstItem(const FieldPos: Int64; const FilterName: string; const ItemExtID: Int64; var SenderSearch: TTMDBSearchItem; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbPack_FastFindNextItem(var SenderSearch: TTMDBSearchItem; const ItemExtID: Int64; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbPack_FastFindLastItem(const FieldPos: Int64; const FilterName: string; const ItemExtID: Int64; var SenderSearch: TTMDBSearchItem; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbPack_FastFindPrevItem(var SenderSearch: TTMDBSearchItem; const ItemExtID: Int64; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}

function dbPack_FindFirstField(const PathName, FilterName: string; var SenderSearch: TTMDBSearchField; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbPack_FindNextField(var SenderSearch: TTMDBSearchField; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbPack_FindLastField(const PathName, FilterName: string; var SenderSearch: TTMDBSearchField; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbPack_FindPrevField(var SenderSearch: TTMDBSearchField; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}

function dbPack_FastFindFirstField(const FieldPos: Int64; const FilterName: string; var SenderSearch: TTMDBSearchField; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbPack_FastFindNextField(var SenderSearch: TTMDBSearchField; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbPack_FastFindLastField(const FieldPos: Int64; const FilterName: string; var SenderSearch: TTMDBSearchField; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbPack_FastFindPrevField(var SenderSearch: TTMDBSearchField; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}

function dbPack_RecursionSearchFirst(const InitPath, FilterName: string; var SenderRecursionSearch: TTMDBRecursionSearch; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
function dbPack_RecursionSearchNext(var SenderRecursionSearch: TTMDBRecursionSearch; var SenderTMDB: TTMDB): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}


var
  TreeMDBHeaderNameMultipleCharacter: umlSystemString = '?';
  TreeMDBHeaderNameMultipleString   : umlSystemString = '*';
  db_FieldPathLimitChar             : umlSystemString = '/\';

implementation

uses PascalStrings;

const
  { return code }
  db_Header_ok                       = -300;
  db_Header_SetPosError              = -301;
  db_Header_WritePosError            = -303;
  db_Header_WriteNextPosError        = -304;
  db_Header_WritePrevPosError        = -305;
  db_Header_WritePubMainPosError     = -306;
  db_Header_WriteIDError             = -307;
  db_Header_WritePositionIDError     = -311;
  db_Header_WriteNameError           = -308;
  db_Header_WriteCreateTimeError     = -309;
  db_Header_WriteLastEditTimeError   = -310;
  db_Header_WriteUserPropertyIDError = -332;
  db_Header_ReadPosError             = -321;
  db_Header_ReadNextPosError         = -322;
  db_Header_ReadPrevPosError         = -323;
  db_Header_ReadPubMainPosError      = -324;
  db_Header_ReadIDError              = -325;
  db_Header_ReadPositionIDError      = -312;
  db_Header_ReadNameError            = -326;
  db_Header_ReadCreateTimeError      = -327;
  db_Header_ReadLastEditTimeError    = -328;
  db_Header_ReadUserPropertyIDError  = -331;
  db_Header_NotFindHeader            = -320;

  db_Item_ok                         = -200;
  db_Item_SetPosError                = -201;
  db_Item_WriteRecDescriptionError   = -204;
  db_Item_WriteRecExterIDError       = -205;
  db_Item_WriteFirstBlockPOSError    = -206;
  db_Item_WriteLastBlockPOSError     = -207;
  db_Item_WriteRecBuffSizeError      = -208;
  db_Item_WriteBlockCountError       = -209;
  db_Item_ReadRecDescriptionError    = -214;
  db_Item_ReadRecExterIDError        = -215;
  db_Item_ReadFirstBlockPOSError     = -216;
  db_Item_ReadLastBlockPOSError      = -217;
  db_Item_ReadRecBuffSizeError       = -218;
  db_Item_ReadBlockCountError        = -219;
  db_Item_WriteItemBlockIDFlagsError = -220;
  db_Item_WriteCurrentBlockPOSError  = -221;
  db_Item_WriteNextBlockPOSError     = -222;
  db_Item_WritePrevBlockPOSError     = -223;
  db_Item_WriteDataBlockPOSError     = -224;
  db_Item_WriteDataBuffSizeError     = -225;
  db_Item_ReadItemBlockIDFlagsError  = -230;
  db_Item_ReadCurrentBlockPOSError   = -231;
  db_Item_ReadNextBlockPOSError      = -232;
  db_Item_ReadPrevBlockPOSError      = -233;
  db_Item_ReadDataBlockPOSError      = -234;
  db_Item_ReadDataBuffSizeError      = -235;
  db_Item_BlockPositionError         = -240;
  db_Item_BlockOverrate              = -241;
  db_Item_BlockReadError             = -242;
  db_Item_BlockWriteError            = -243;

  db_Field_ok                       = -100;
  db_Field_SetPosError              = -101;
  db_Field_WriteHeaderFieldPosError = -103;
  db_Field_WriteDescriptionError    = -104;
  db_Field_WriteCountError          = -106;
  db_Field_WriteFirstPosError       = -107;
  db_Field_WriteLastPosError        = -108;
  db_Field_ReadHeaderFieldPosError  = -110;
  db_Field_ReadDescriptionError     = -111;
  db_Field_ReadCountError           = -112;
  db_Field_ReadFirstPosError        = -113;
  db_Field_ReadLastPosError         = -114;
  db_Field_NotInitSearch            = -121;
  db_Field_DeleteHeaderError        = -124;

  db_Pack_ok                            = -400;
  db_Pack_RepOpenPackError              = -401;
  db_Pack_CreatePackError               = -402;
  db_Pack_WriteFileDescriptionNameError = -460;
  db_Pack_WriteNameError                = -403;
  db_Pack_WriteDescriptionError         = -404;
  db_Pack_PositionSeekError             = -405;
  db_Pack_WriteMajorVersionError        = -406;
  db_Pack_WriteMinorVersionError        = -407;
  db_Pack_WriteCreateTimeError          = -408;
  db_Pack_WriteLastEditTimeError        = -409;
  db_Pack_WriteHeaderCountError         = -410;
  db_Pack_WriteDefaultPositionError     = -411;
  db_Pack_WriteFirstPositionError       = -412;
  db_Pack_WriteLastPositionError        = -413;
  db_Pack_ReadFileDescriptionNameError  = -461;
  db_Pack_ReadNameError                 = -414;
  db_Pack_ReadDescriptionError          = -415;
  db_Pack_ReadMajorVersionError         = -416;
  db_Pack_ReadMinorVersionError         = -417;
  db_Pack_ReadCreateTimeError           = -418;
  db_Pack_ReadLastEditTimeError         = -419;
  db_Pack_ReadHeaderCountError          = -420;
  db_Pack_ReadDefaultPositionError      = -421;
  db_Pack_ReadFirstPositionError        = -422;
  db_Pack_ReadLastPositionError         = -423;
  db_Pack_RepCreatePackError            = -424;
  db_Pack_OpenPackError                 = -425;
  db_Pack_ClosePackError                = -426;
  db_Pack_WriteCurrentPositionError     = -427;
  db_Pack_WriteCurrentLevelError        = -428;
  db_Pack_ReadCurrentPositionError      = -429;
  db_Pack_ReadCurrentLevelError         = -430;
  db_Pack_PathNameError                 = -440;
  db_Pack_RepeatCreateItemError         = -450;
  db_Pack_OpenItemError                 = -451;
  db_Pack_ItemNameError                 = -452;
  db_Pack_RepeatOpenItemError           = -453;
  db_Pack_CloseItemError                = -454;
  db_Pack_ItemStructNotFindDescription  = -455;
  db_Pack_RecursionSearchOver           = -456;

function dbPack_GetIndexStrCount(const StrName: string): Integer; {$IFDEF INLINE_ASM}inline; {$ENDIF}
begin
  Result := umlGetIndexStrCount(StrName, db_FieldPathLimitChar);
end;

function dbPack_MaskFirstPath(const PathName: string): string; {$IFDEF INLINE_ASM}inline; {$ENDIF}
begin
  Result := umlDeleteFirstStr(PathName, db_FieldPathLimitChar).Text;
end;

function dbPack_MaskLastPath(const PathName: string): string; {$IFDEF INLINE_ASM}inline; {$ENDIF}
begin
  Result := umlDeleteLastStr(PathName, db_FieldPathLimitChar).Text;
end;

function dbPack_GetFirstPath(const PathName: string): string; {$IFDEF INLINE_ASM}inline; {$ENDIF}
begin
  Result := umlGetFirstStr(PathName, db_FieldPathLimitChar).Text;
end;

function dbPack_GetLastPath(const PathName: string): string; {$IFDEF INLINE_ASM}inline; {$ENDIF}
begin
  Result := umlGetLastStr(PathName, db_FieldPathLimitChar).Text;
end;

function dbPack_MatchStr(const SourStr, DestStr: string): Boolean; {$IFDEF INLINE_ASM}inline; {$ENDIF}
begin
  Result := umlMultipleMatch(True, SourStr, DestStr, TreeMDBHeaderNameMultipleString, TreeMDBHeaderNameMultipleCharacter);
end;

procedure InitTHeader(var SenderHeader: THeader);
begin
  SenderHeader.CurrentHeader := 0;
  SenderHeader.NextHeader := 0;
  SenderHeader.PrevHeader := 0;
  SenderHeader.DataMainPOS := 0;
  SenderHeader.CreateTime := 0;
  SenderHeader.LastModifyTime := 0;
  SenderHeader.ID := 0;
  SenderHeader.PositionID := 0;
  SenderHeader.UserProperty := 0;
  SenderHeader.Name := '';
  SenderHeader.Return := db_Header_ok;
end;

procedure InitTItemBlock(var SenderItemBlock: TItemBlock);
begin
  SenderItemBlock.IDFlags := 0;
  SenderItemBlock.CurrentBlockPOS := 0;
  SenderItemBlock.NextBlockPOS := 0;
  SenderItemBlock.PrevBlockPOS := 0;
  SenderItemBlock.DataBlockPOS := 0;
  SenderItemBlock.Size := 0;
  SenderItemBlock.Return := db_Item_ok;
end;

procedure InitTItem(var SenderItem: TItem);
begin
  InitTHeader(SenderItem.RHeader);
  SenderItem.Description := '';
  SenderItem.ExtID := 0;
  SenderItem.FirstBlockPOS := 0;
  SenderItem.LastBlockPOS := 0;
  SenderItem.Size := 0;
  SenderItem.BlockCount := 0;
  SenderItem.CurrentBlockSeekPOS := 0;
  SenderItem.CurrentFileSeekPOS := 0;
  InitTItemBlock(SenderItem.CurrentItemBlock);
  SenderItem.DataModify := False;
  SenderItem.Return := db_Item_ok;
end;

procedure InitTField(var SenderField: TField);
begin
  SenderField.HigherFieldPOS := 0;
  SenderField.Description := '';
  SenderField.HeaderCount := 0;
  SenderField.FirstHeaderPOS := 0;
  SenderField.LastHeaderPOS := 0;
  InitTHeader(SenderField.RHeader);
  SenderField.Return := db_Field_ok;
end;

procedure InitTFieldSearch(var SenderFieldSearch: TFieldSearch);
begin
  SenderFieldSearch.InitFlags := False;
  SenderFieldSearch.StartPOS := 0;
  SenderFieldSearch.OverPOS := 0;
  SenderFieldSearch.Name := '';
  SenderFieldSearch.ID := 0;
  SenderFieldSearch.PositionID := 0;
  InitTHeader(SenderFieldSearch.RHeader);
  SenderFieldSearch.Return := db_Field_ok;
end;

procedure InitTTMDB(var SenderTMDB: TTMDB);
begin
  SenderTMDB.FileDescription := '';
  SenderTMDB.Name := '';
  SenderTMDB.Description := '';
  SenderTMDB.MajorVer := 0;
  SenderTMDB.MinorVer := 0;
  SenderTMDB.CreateTime := 0;
  SenderTMDB.LastModifyTime := 0;
  SenderTMDB.RootHeaderCount := 0;
  SenderTMDB.DefaultFieldPOS := 0;
  SenderTMDB.FirstHeaderPOS := 0;
  SenderTMDB.LastHeaderPOS := 0;
  SenderTMDB.CurrentFieldPOS := 0;
  SenderTMDB.CurrentFieldLevel := 0;
  InitTRecFile(SenderTMDB.RecFile);
  SenderTMDB.OverWriteItem := True;
  SenderTMDB.WriteFlags := False;
  SenderTMDB.SameItemName := False;
  SenderTMDB.Return := db_Pack_ok;
end;

procedure InitTTMDBItemHandle(var SenderTMDBItemHandle: TTMDBItemHandle);
begin
  InitTItem(SenderTMDBItemHandle.Item);
  SenderTMDBItemHandle.Path := '';
  SenderTMDBItemHandle.Name := '';
  SenderTMDBItemHandle.Description := '';
  SenderTMDBItemHandle.CreateTime := 0;
  SenderTMDBItemHandle.LastModifyTime := 0;
  SenderTMDBItemHandle.ItemExtID := 0;
  SenderTMDBItemHandle.OpenFlags := False;
end;

procedure InitTTMDBSearchHeader(var SenderTMDBSearchHeader: TTMDBSearchHeader);
begin
  SenderTMDBSearchHeader.Name := '';
  SenderTMDBSearchHeader.ID := 0;
  SenderTMDBSearchHeader.CreateTime := 0;
  SenderTMDBSearchHeader.LastModifyTime := 0;
  SenderTMDBSearchHeader.HeaderPOS := 0;
  SenderTMDBSearchHeader.CompleteCount := 0;
  InitTFieldSearch(SenderTMDBSearchHeader.FieldSearch);
end;

procedure InitTTMDBSearchItem(var SenderTMDBSearchItem: TTMDBSearchItem);
begin
  SenderTMDBSearchItem.Name := '';
  SenderTMDBSearchItem.Description := '';
  SenderTMDBSearchItem.ExtID := 0;
  SenderTMDBSearchItem.Size := 0;
  SenderTMDBSearchItem.HeaderPOS := 0;
  SenderTMDBSearchItem.CompleteCount := 0;
  InitTFieldSearch(SenderTMDBSearchItem.FieldSearch);
end;

procedure InitTTMDBSearchField(var SenderTMDBSearchField: TTMDBSearchField);
begin
  SenderTMDBSearchField.Name := '';
  SenderTMDBSearchField.Description := '';
  SenderTMDBSearchField.HeaderCount := 0;
  SenderTMDBSearchField.HeaderPOS := 0;
  SenderTMDBSearchField.CompleteCount := 0;
  InitTFieldSearch(SenderTMDBSearchField.FieldSearch);
end;

procedure InitTTMDBDescriptionHandle(var SenderTMDBDescriptionHandle: TTMDBDescriptionHandle);
begin
  SenderTMDBDescriptionHandle.StructVarID := 0;
  SenderTMDBDescriptionHandle.StructDescription := '';
  SenderTMDBDescriptionHandle.StructNextPos := 0;
  SenderTMDBDescriptionHandle.StructCurrentPos := 0;
  SenderTMDBDescriptionHandle.StructPublicPos := 0;
  SenderTMDBDescriptionHandle.StructSize := 0;
  SenderTMDBDescriptionHandle.StructPositionID := 0;
end;

procedure InitTTMDBItemStruct(var SenderTMDBItemStruct: TTMDBItemStruct);
begin
  SenderTMDBItemStruct.Description := '';
  SenderTMDBItemStruct.StructCount := 0;
  SenderTMDBItemStruct.StructFirstPos := 0;
  SenderTMDBItemStruct.StructLastPos := 0;
  SenderTMDBItemStruct.ItemStructCurrentPos := 0;
  InitTTMDBDescriptionHandle(SenderTMDBItemStruct.DescriptionHandle);
end;

procedure InitTTMDBRecursionSearch(var SenderTMDBRecursionSearch: TTMDBRecursionSearch);
var
  Rep_Int: Integer;
begin
  InitTHeader(SenderTMDBRecursionSearch.ReturnHeader);
  InitTField(SenderTMDBRecursionSearch.CurrentField);
  SenderTMDBRecursionSearch.InitPath := '';
  SenderTMDBRecursionSearch.FilterName := '';
  SenderTMDBRecursionSearch.SearchBuffGo := 0;
  for Rep_Int := 0 to MaxSecursionLevel do
      InitTFieldSearch(SenderTMDBRecursionSearch.SearchBuff[Rep_Int]);
end;

function dbHeader_WriteRec(const POS: Int64; var RecFile: TRecFile; var SenderHeader: THeader): Boolean;
begin
  if umlFileSeek(RecFile, POS) = False then
    begin
      SenderHeader.Return := db_Header_SetPosError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(RecFile, umlPositionLength, SenderHeader.CurrentHeader) = False then
    begin
      SenderHeader.Return := db_Header_WritePosError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(RecFile, umlPositionLength, SenderHeader.NextHeader) = False then
    begin
      SenderHeader.Return := db_Header_WriteNextPosError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(RecFile, umlPositionLength, SenderHeader.PrevHeader) = False then
    begin
      SenderHeader.Return := db_Header_WritePrevPosError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(RecFile, umlPositionLength, SenderHeader.DataMainPOS) = False then
    begin
      SenderHeader.Return := db_Header_WritePubMainPosError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(RecFile, umlTimeLength, SenderHeader.CreateTime) = False then
    begin
      SenderHeader.Return := db_Header_WriteCreateTimeError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(RecFile, umlTimeLength, SenderHeader.LastModifyTime) = False then
    begin
      SenderHeader.Return := db_Header_WriteLastEditTimeError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(RecFile, umlIDLength, SenderHeader.ID) = False then
    begin
      SenderHeader.Return := db_Header_WriteIDError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(RecFile, umlIDLength, SenderHeader.PositionID) = False then
    begin
      SenderHeader.Return := db_Header_WritePositionIDError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(RecFile, umlPropertyLength, SenderHeader.UserProperty) = False then
    begin
      SenderHeader.Return := db_Header_WriteUserPropertyIDError;
      Result := False;
      Exit;
    end;
  if umlFileWriteStr(RecFile, SenderHeader.Name) = False then
    begin
      SenderHeader.Return := db_Header_WriteNameError;
      Result := False;
      Exit;
    end;
  SenderHeader.Return := db_Header_ok;
  Result := True;
end;

function dbHeader_ReadRec(const POS: Int64; var RecFile: TRecFile; var SenderHeader: THeader): Boolean;
begin
  if umlFileSeek(RecFile, POS) = False then
    begin
      SenderHeader.Return := db_Header_SetPosError;
      Result := False;
      Exit;
    end;
  if umlFileRead(RecFile, umlPositionLength, SenderHeader.CurrentHeader) = False then
    begin
      SenderHeader.Return := db_Header_ReadPosError;
      Result := False;
      Exit;
    end;
  if umlFileRead(RecFile, umlPositionLength, SenderHeader.NextHeader) = False then
    begin
      SenderHeader.Return := db_Header_ReadNextPosError;
      Result := False;
      Exit;
    end;
  if umlFileRead(RecFile, umlPositionLength, SenderHeader.PrevHeader) = False then
    begin
      SenderHeader.Return := db_Header_ReadPrevPosError;
      Result := False;
      Exit;
    end;
  if umlFileRead(RecFile, umlPositionLength, SenderHeader.DataMainPOS) = False then
    begin
      SenderHeader.Return := db_Header_ReadPubMainPosError;
      Result := False;
      Exit;
    end;
  if umlFileRead(RecFile, umlTimeLength, SenderHeader.CreateTime) = False then
    begin
      SenderHeader.Return := db_Header_ReadCreateTimeError;
      Result := False;
      Exit;
    end;
  if umlFileRead(RecFile, umlTimeLength, SenderHeader.LastModifyTime) = False then
    begin
      SenderHeader.Return := db_Header_ReadLastEditTimeError;
      Result := False;
      Exit;
    end;
  if umlFileRead(RecFile, umlIDLength, SenderHeader.ID) = False then
    begin
      SenderHeader.Return := db_Header_ReadIDError;
      Result := False;
      Exit;
    end;
  if umlFileRead(RecFile, umlIDLength, SenderHeader.PositionID) = False then
    begin
      SenderHeader.Return := db_Header_ReadPositionIDError;
      Result := False;
      Exit;
    end;
  if umlFileRead(RecFile, umlPropertyLength, SenderHeader.UserProperty) = False then
    begin
      SenderHeader.Return := db_Header_ReadUserPropertyIDError;
      Result := False;
      Exit;
    end;
  if umlFileReadStr(RecFile, SenderHeader.Name) = False then
    begin
      SenderHeader.Return := db_Header_ReadNameError;
      Result := False;
      Exit;
    end;
  SenderHeader.Return := db_Header_ok;
  Result := True;
end;

function dbHeader_GetNextRec(const POS: Int64; var RecFile: TRecFile): THeader;
begin
  if dbHeader_ReadRec(POS, RecFile, Result) = False then
      Exit;
  dbHeader_ReadRec(Result.NextHeader, RecFile, Result);
end;

function dbHeader_GetPrevRec(const POS: Int64; var RecFile: TRecFile): THeader;
begin
  if dbHeader_ReadRec(POS, RecFile, Result) = False then
      Exit;
  dbHeader_ReadRec(Result.PrevHeader, RecFile, Result);
end;

function dbHeader_GetPOSRec(const POS: Int64; var RecFile: TRecFile): THeader;
begin
  dbHeader_ReadRec(POS, RecFile, Result);
end;

function dbHeader_MultipleMatch(const SourStr, DestStr: string): Boolean;
begin
  Result := dbPack_MatchStr(SourStr, DestStr);
end;

function dbHeader_FindNext(const Name: string; const FirstHeaderPOS, LastHeaderPOS: Int64; var RecFile: TRecFile; var SenderHeader: THeader): Boolean;
begin
  if dbHeader_ReadRec(FirstHeaderPOS, RecFile, SenderHeader) = False then
    begin
      Result := False;
      Exit;
    end;
  if dbHeader_MultipleMatch(name, SenderHeader.Name) = True then
    begin
      Result := True;
      Exit;
    end;
  if (SenderHeader.PositionID = db_Header_LastPositionFlags) or (SenderHeader.PositionID = db_Header_OnlyPositionFlags) then
    begin
      SenderHeader.Return := db_Header_NotFindHeader;
      Result := False;
      Exit;
    end;
  while dbHeader_ReadRec(SenderHeader.NextHeader, RecFile, SenderHeader) = True do
    begin
      if dbHeader_MultipleMatch(name, SenderHeader.Name) = True then
        begin
          Result := True;
          Exit;
        end;
      if SenderHeader.PositionID = db_Header_LastPositionFlags then
        begin
          SenderHeader.Return := db_Header_NotFindHeader;
          Result := False;
          Exit;
        end;
    end;
  SenderHeader.Return := db_Header_ok;
  Result := False;
end;

function dbHeader_FindPrev(const Name: string; const LastHeaderPOS, FirstHeaderPOS: Int64; var RecFile: TRecFile; var SenderHeader: THeader): Boolean;
begin
  if dbHeader_ReadRec(LastHeaderPOS, RecFile, SenderHeader) = False then
    begin
      Result := False;
      Exit;
    end;
  if dbHeader_MultipleMatch(name, SenderHeader.Name) = True then
    begin
      Result := True;
      Exit;
    end;
  if (SenderHeader.PositionID = db_Header_FirstPositionFlags) or (SenderHeader.PositionID = db_Header_OnlyPositionFlags) then
    begin
      SenderHeader.Return := db_Header_NotFindHeader;
      Result := False;
      Exit;
    end;
  while dbHeader_ReadRec(SenderHeader.PrevHeader, RecFile, SenderHeader) = True do
    begin
      if dbHeader_MultipleMatch(name, SenderHeader.Name) = True then
        begin
          Result := True;
          Exit;
        end;
      if SenderHeader.PositionID = db_Header_FirstPositionFlags then
        begin
          SenderHeader.Return := db_Header_NotFindHeader;
          Result := False;
          Exit;
        end;
    end;
  SenderHeader.Return := db_Header_ok;
  Result := False;
end;

function dbItem_WriteRec(const POS: Int64; var RecFile: TRecFile; var SenderItem: TItem): Boolean;
begin
  if dbHeader_WriteRec(POS, RecFile, SenderItem.RHeader) = False then
    begin
      SenderItem.Return := SenderItem.RHeader.Return;
      Result := False;
      Exit;
    end;
  if umlFileSeek(RecFile, SenderItem.RHeader.DataMainPOS) = False then
    begin
      SenderItem.Return := db_Item_SetPosError;
      Result := False;
      Exit;
    end;
  if umlFileWriteStr(RecFile, SenderItem.Description) = False then
    begin
      SenderItem.Return := db_Item_WriteRecDescriptionError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(RecFile, umlIDLength, SenderItem.ExtID) = False then
    begin
      SenderItem.Return := db_Item_WriteRecExterIDError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(RecFile, umlPositionLength, SenderItem.FirstBlockPOS) = False then
    begin
      SenderItem.Return := db_Item_WriteFirstBlockPOSError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(RecFile, umlPositionLength, SenderItem.LastBlockPOS) = False then
    begin
      SenderItem.Return := db_Item_WriteLastBlockPOSError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(RecFile, umlSizeLength, SenderItem.Size) = False then
    begin
      SenderItem.Return := db_Item_WriteRecBuffSizeError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(RecFile, umlCountLength, SenderItem.BlockCount) = False then
    begin
      SenderItem.Return := db_Item_WriteBlockCountError;
      Result := False;
      Exit;
    end;
  SenderItem.Return := db_Item_ok;
  Result := True;
end;

function dbItem_ReadRec(const POS: Int64; var RecFile: TRecFile; var SenderItem: TItem): Boolean;
begin
  if dbHeader_ReadRec(POS, RecFile, SenderItem.RHeader) = False then
    begin
      SenderItem.Return := SenderItem.RHeader.Return;
      Result := False;
      Exit;
    end;
  if umlFileSeek(RecFile, SenderItem.RHeader.DataMainPOS) = False then
    begin
      SenderItem.Return := db_Item_SetPosError;
      Result := False;
      Exit;
    end;
  if umlFileReadStr(RecFile, SenderItem.Description) = False then
    begin
      SenderItem.Return := db_Item_ReadRecDescriptionError;
      Result := False;
      Exit;
    end;
  if umlFileRead(RecFile, umlIDLength, SenderItem.ExtID) = False then
    begin
      SenderItem.Return := db_Item_ReadRecExterIDError;
      Result := False;
      Exit;
    end;
  if umlFileRead(RecFile, umlPositionLength, SenderItem.FirstBlockPOS) = False then
    begin
      SenderItem.Return := db_Item_ReadFirstBlockPOSError;
      Result := False;
      Exit;
    end;
  if umlFileRead(RecFile, umlPositionLength, SenderItem.LastBlockPOS) = False then
    begin
      SenderItem.Return := db_Item_ReadLastBlockPOSError;
      Result := False;
      Exit;
    end;
  if umlFileRead(RecFile, umlSizeLength, SenderItem.Size) = False then
    begin
      SenderItem.Return := db_Item_ReadRecBuffSizeError;
      Result := False;
      Exit;
    end;
  if umlFileRead(RecFile, umlCountLength, SenderItem.BlockCount) = False then
    begin
      SenderItem.Return := db_Item_ReadBlockCountError;
      Result := False;
      Exit;
    end;
  SenderItem.Return := db_Item_ok;
  Result := True;
end;

function dbItem_OnlyWriteItemRec(const POS: Int64; var RecFile: TRecFile; var SenderItem: TItem): Boolean;
begin
  if umlFileSeek(RecFile, POS) = False then
    begin
      SenderItem.Return := db_Item_SetPosError;
      Result := False;
      Exit;
    end;
  if umlFileWriteStr(RecFile, SenderItem.Description) = False then
    begin
      SenderItem.Return := db_Item_WriteRecDescriptionError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(RecFile, umlIDLength, SenderItem.ExtID) = False then
    begin
      SenderItem.Return := db_Item_WriteRecExterIDError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(RecFile, umlPositionLength, SenderItem.FirstBlockPOS) = False then
    begin
      SenderItem.Return := db_Item_WriteFirstBlockPOSError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(RecFile, umlPositionLength, SenderItem.LastBlockPOS) = False then
    begin
      SenderItem.Return := db_Item_WriteLastBlockPOSError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(RecFile, umlSizeLength, SenderItem.Size) = False then
    begin
      SenderItem.Return := db_Item_WriteRecBuffSizeError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(RecFile, umlCountLength, SenderItem.BlockCount) = False then
    begin
      SenderItem.Return := db_Item_WriteBlockCountError;
      Result := False;
      Exit;
    end;
  SenderItem.Return := db_Item_ok;
  Result := True;
end;

function dbItem_OnlyReadItemRec(const POS: Int64; var RecFile: TRecFile; var SenderItem: TItem): Boolean;
begin
  if umlFileSeek(RecFile, POS) = False then
    begin
      SenderItem.Return := db_Item_SetPosError;
      Result := False;
      Exit;
    end;
  if umlFileReadStr(RecFile, SenderItem.Description) = False then
    begin
      SenderItem.Return := db_Item_ReadRecDescriptionError;
      Result := False;
      Exit;
    end;
  if umlFileRead(RecFile, umlIDLength, SenderItem.ExtID) = False then
    begin
      SenderItem.Return := db_Item_ReadRecExterIDError;
      Result := False;
      Exit;
    end;
  if umlFileRead(RecFile, umlPositionLength, SenderItem.FirstBlockPOS) = False then
    begin
      SenderItem.Return := db_Item_ReadFirstBlockPOSError;
      Result := False;
      Exit;
    end;
  if umlFileRead(RecFile, umlPositionLength, SenderItem.LastBlockPOS) = False then
    begin
      SenderItem.Return := db_Item_ReadLastBlockPOSError;
      Result := False;
      Exit;
    end;
  if umlFileRead(RecFile, umlSizeLength, SenderItem.Size) = False then
    begin
      SenderItem.Return := db_Item_ReadRecBuffSizeError;
      Result := False;
      Exit;
    end;
  if umlFileRead(RecFile, umlCountLength, SenderItem.BlockCount) = False then
    begin
      SenderItem.Return := db_Item_ReadBlockCountError;
      Result := False;
      Exit;
    end;
  SenderItem.Return := db_Item_ok;
  Result := True;
end;

function dbItem_OnlyWriteItemBlockRec(const POS: Int64; var RecFile: TRecFile; var SenderItemBlock: TItemBlock): Boolean;
begin
  if umlFileSeek(RecFile, POS) = False then
    begin
      SenderItemBlock.Return := db_Item_SetPosError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(RecFile, umlIDLength, SenderItemBlock.IDFlags) = False then
    begin
      SenderItemBlock.Return := db_Item_WriteItemBlockIDFlagsError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(RecFile, umlPositionLength, SenderItemBlock.CurrentBlockPOS) = False then
    begin
      SenderItemBlock.Return := db_Item_WriteCurrentBlockPOSError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(RecFile, umlPositionLength, SenderItemBlock.NextBlockPOS) = False then
    begin
      SenderItemBlock.Return := db_Item_WriteNextBlockPOSError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(RecFile, umlPositionLength, SenderItemBlock.PrevBlockPOS) = False then
    begin
      SenderItemBlock.Return := db_Item_WritePrevBlockPOSError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(RecFile, umlPositionLength, SenderItemBlock.DataBlockPOS) = False then
    begin
      SenderItemBlock.Return := db_Item_WriteDataBlockPOSError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(RecFile, umlSizeLength, SenderItemBlock.Size) = False then
    begin
      SenderItemBlock.Return := db_Item_WriteDataBuffSizeError;
      Result := False;
      Exit;
    end;
  SenderItemBlock.Return := db_Item_ok;
  Result := True;
end;

function dbItem_OnlyReadItemBlockRec(const POS: Int64; var RecFile: TRecFile; var SenderItemBlock: TItemBlock): Boolean;
begin
  if umlFileSeek(RecFile, POS) = False then
    begin
      SenderItemBlock.Return := db_Item_SetPosError;
      Result := False;
      Exit;
    end;
  if umlFileRead(RecFile, umlIDLength, SenderItemBlock.IDFlags) = False then
    begin
      SenderItemBlock.Return := db_Item_ReadItemBlockIDFlagsError;
      Result := False;
      Exit;
    end;
  if umlFileRead(RecFile, umlPositionLength, SenderItemBlock.CurrentBlockPOS) = False then
    begin
      SenderItemBlock.Return := db_Item_ReadCurrentBlockPOSError;
      Result := False;
      Exit;
    end;
  if umlFileRead(RecFile, umlPositionLength, SenderItemBlock.NextBlockPOS) = False then
    begin
      SenderItemBlock.Return := db_Item_ReadNextBlockPOSError;
      Result := False;
      Exit;
    end;
  if umlFileRead(RecFile, umlPositionLength, SenderItemBlock.PrevBlockPOS) = False then
    begin
      SenderItemBlock.Return := db_Item_ReadPrevBlockPOSError;
      Result := False;
      Exit;
    end;
  if umlFileRead(RecFile, umlPositionLength, SenderItemBlock.DataBlockPOS) = False then
    begin
      SenderItemBlock.Return := db_Item_ReadDataBlockPOSError;
      Result := False;
      Exit;
    end;
  if umlFileRead(RecFile, umlSizeLength, SenderItemBlock.Size) = False then
    begin
      SenderItemBlock.Return := db_Item_ReadDataBuffSizeError;
      Result := False;
      Exit;
    end;
  SenderItemBlock.Return := db_Item_ok;
  Result := True;
end;

function dbItem_BlockCreate(var RecFile: TRecFile; var SenderItem: TItem): Boolean;
var
  FirstItemBlock, LastItemBlock: TItemBlock;
begin
  case SenderItem.BlockCount of
    0:
      begin
        LastItemBlock.IDFlags := db_item_OnlyPositionFlags;
        LastItemBlock.CurrentBlockPOS := umlFileGetSize(RecFile);
        LastItemBlock.NextBlockPOS := LastItemBlock.CurrentBlockPOS;
        LastItemBlock.PrevBlockPOS := LastItemBlock.CurrentBlockPOS;
        LastItemBlock.DataBlockPOS := LastItemBlock.CurrentBlockPOS + db_Item_BlockRecLength;
        LastItemBlock.Size := 0;
        if dbItem_OnlyWriteItemBlockRec(LastItemBlock.CurrentBlockPOS, RecFile, LastItemBlock) = False then
          begin
            SenderItem.Return := LastItemBlock.Return;
            Result := False;
            Exit;
          end;
        SenderItem.BlockCount := 1;
        SenderItem.FirstBlockPOS := LastItemBlock.CurrentBlockPOS;
        SenderItem.LastBlockPOS := LastItemBlock.CurrentBlockPOS;
        if dbItem_OnlyWriteItemRec(SenderItem.RHeader.DataMainPOS, RecFile, SenderItem) = False then
          begin
            Result := False;
            Exit;
          end;
      end;
    1:
      begin
        if dbItem_OnlyReadItemBlockRec(SenderItem.FirstBlockPOS, RecFile, FirstItemBlock) = False then
          begin
            SenderItem.Return := FirstItemBlock.Return;
            Result := False;
            Exit;
          end;
        LastItemBlock.IDFlags := db_item_LastPositionFlags;
        LastItemBlock.CurrentBlockPOS := umlFileGetSize(RecFile);
        LastItemBlock.NextBlockPOS := FirstItemBlock.CurrentBlockPOS;
        LastItemBlock.PrevBlockPOS := FirstItemBlock.CurrentBlockPOS;
        LastItemBlock.DataBlockPOS := LastItemBlock.CurrentBlockPOS + db_Item_BlockRecLength;
        LastItemBlock.Size := 0;
        if dbItem_OnlyWriteItemBlockRec(LastItemBlock.CurrentBlockPOS, RecFile, LastItemBlock) = False then
          begin
            SenderItem.Return := LastItemBlock.Return;
            Result := False;
            Exit;
          end;
        FirstItemBlock.IDFlags := db_item_FirstPositionFlags;
        FirstItemBlock.NextBlockPOS := LastItemBlock.CurrentBlockPOS;
        FirstItemBlock.PrevBlockPOS := LastItemBlock.CurrentBlockPOS;
        if dbItem_OnlyWriteItemBlockRec(SenderItem.FirstBlockPOS, RecFile, FirstItemBlock) = False then
          begin
            SenderItem.Return := FirstItemBlock.Return;
            Result := False;
            Exit;
          end;
        SenderItem.BlockCount := SenderItem.BlockCount + 1;
        SenderItem.LastBlockPOS := LastItemBlock.CurrentBlockPOS;
        if dbItem_OnlyWriteItemRec(SenderItem.RHeader.DataMainPOS, RecFile, SenderItem) = False then
          begin
            Result := False;
            Exit;
          end;
      end;
    else
      begin
        if dbItem_OnlyReadItemBlockRec(SenderItem.FirstBlockPOS, RecFile, FirstItemBlock) = False then
          begin
            SenderItem.Return := FirstItemBlock.Return;
            Result := False;
            Exit;
          end;
        FirstItemBlock.PrevBlockPOS := umlFileGetSize(RecFile);
        if dbItem_OnlyWriteItemBlockRec(SenderItem.FirstBlockPOS, RecFile, FirstItemBlock) = False then
          begin
            SenderItem.Return := FirstItemBlock.Return;
            Result := False;
            Exit;
          end;
        if dbItem_OnlyReadItemBlockRec(SenderItem.LastBlockPOS, RecFile, LastItemBlock) = False then
          begin
            SenderItem.Return := LastItemBlock.Return;
            Result := False;
            Exit;
          end;
        LastItemBlock.IDFlags := db_item_MediumPositionFlags;
        LastItemBlock.NextBlockPOS := FirstItemBlock.PrevBlockPOS;
        if dbItem_OnlyWriteItemBlockRec(SenderItem.LastBlockPOS, RecFile, LastItemBlock) = False then
          begin
            SenderItem.Return := LastItemBlock.Return;
            Result := False;
            Exit;
          end;
        LastItemBlock.IDFlags := db_item_LastPositionFlags;
        LastItemBlock.CurrentBlockPOS := FirstItemBlock.PrevBlockPOS;
        LastItemBlock.NextBlockPOS := SenderItem.FirstBlockPOS;
        LastItemBlock.PrevBlockPOS := SenderItem.LastBlockPOS;
        LastItemBlock.DataBlockPOS := LastItemBlock.CurrentBlockPOS + db_Item_BlockRecLength;
        LastItemBlock.Size := 0;
        if dbItem_OnlyWriteItemBlockRec(LastItemBlock.CurrentBlockPOS, RecFile, LastItemBlock) = False then
          begin
            SenderItem.Return := LastItemBlock.Return;
            Result := False;
            Exit;
          end;
        SenderItem.BlockCount := SenderItem.BlockCount + 1;
        SenderItem.LastBlockPOS := LastItemBlock.CurrentBlockPOS;
        if dbItem_OnlyWriteItemRec(SenderItem.RHeader.DataMainPOS, RecFile, SenderItem) = False then
          begin
            Result := False;
            Exit;
          end;
      end;
  end;
  SenderItem.CurrentItemBlock := LastItemBlock;
  SenderItem.CurrentBlockSeekPOS := 0;
  SenderItem.CurrentFileSeekPOS := SenderItem.CurrentItemBlock.DataBlockPOS;
  SenderItem.DataModify := True;
  SenderItem.Return := db_Item_ok;
  Result := True;
end;

function dbItem_BlockInit(var RecFile: TRecFile; var SenderItem: TItem): Boolean;
begin
  if SenderItem.BlockCount = 0 then
    begin
      SenderItem.Return := db_Item_ok;
      Result := True;
      Exit;
    end;
  if dbItem_OnlyReadItemBlockRec(SenderItem.FirstBlockPOS, RecFile, SenderItem.CurrentItemBlock) = False then
    begin
      SenderItem.Return := SenderItem.CurrentItemBlock.Return;
      Result := False;
      Exit;
    end;
  SenderItem.CurrentBlockSeekPOS := 0;
  SenderItem.CurrentFileSeekPOS := SenderItem.CurrentItemBlock.DataBlockPOS;
  SenderItem.Return := db_Item_ok;
  Result := True;
end;

function dbItem_BlockReadData(var RecFile: TRecFile; var SenderItem: TItem; var Buffers; const _Size: Int64): Boolean;
label
  Rep_Label;
var
  BuffPointer            : Pointer;
  BuffInt                : NativeUInt;
  DeformitySize, BlockPOS: Int64;
  ItemBlock              : TItemBlock;
  Size                   : Int64;
begin
  if (_Size <= SenderItem.Size) then
      Size := _Size
  else
      Size := SenderItem.Size;

  if Size = 0 then
    begin
      SenderItem.Return := db_Item_ok;
      Result := True;
      Exit;
    end;

  if (SenderItem.BlockCount = 0) then
    begin
      SenderItem.Return := db_Item_BlockOverrate;
      Result := False;
      Exit;
    end;

  if SenderItem.CurrentBlockSeekPOS > SenderItem.CurrentItemBlock.Size then
    begin
      SenderItem.Return := db_Item_BlockPositionError;
      Result := False;
      Exit;
    end;
  ItemBlock := SenderItem.CurrentItemBlock;
  BlockPOS := SenderItem.CurrentBlockSeekPOS;
  BuffInt := NativeUInt(@Buffers);
  BuffPointer := Pointer(BuffInt);
  DeformitySize := Size;
Rep_Label:
  if ItemBlock.Size - BlockPOS = 0 then
    begin
      case ItemBlock.IDFlags of
        db_item_LastPositionFlags, db_item_OnlyPositionFlags:
          begin
            SenderItem.Return := db_Item_BlockOverrate;
            Result := False;
            Exit;
          end;
      end;
      if dbItem_OnlyReadItemBlockRec(ItemBlock.NextBlockPOS, RecFile, ItemBlock) = False then
        begin
          SenderItem.Return := ItemBlock.Return;
          Result := False;
          Exit;
        end;
      if BlockPOS > 0 then
          BlockPOS := 0;
      while (ItemBlock.Size - BlockPOS) = 0 do
        begin
          case ItemBlock.IDFlags of
            db_item_LastPositionFlags:
              begin
                SenderItem.Return := db_Item_BlockOverrate;
                Result := False;
                Exit;
              end;
          end;
          if dbItem_OnlyReadItemBlockRec(ItemBlock.NextBlockPOS, RecFile, ItemBlock) = False then
            begin
              SenderItem.Return := ItemBlock.Return;
              Result := False;
              Exit;
            end;
        end;
    end;

  if umlFileSeek(RecFile, ItemBlock.DataBlockPOS + BlockPOS) = False then
    begin
      SenderItem.Return := db_Item_SetPosError;
      Result := False;
      Exit;
    end;

  if DeformitySize <= ItemBlock.Size - BlockPOS then
    begin
      if umlFileRead(RecFile, DeformitySize, BuffPointer^) = False then
        begin
          SenderItem.Return := db_Item_BlockReadError;
          Result := False;
          Exit;
        end;
      SenderItem.CurrentBlockSeekPOS := BlockPOS + DeformitySize;
      SenderItem.CurrentFileSeekPOS := ItemBlock.DataBlockPOS + (BlockPOS + DeformitySize);
      SenderItem.CurrentItemBlock := ItemBlock;
      SenderItem.Return := db_Item_ok;
      Result := True;
      Exit;
    end;

  if umlFileRead(RecFile, ItemBlock.Size - BlockPOS, BuffPointer^) = False then
    begin
      SenderItem.Return := db_Item_BlockReadError;
      Result := False;
      Exit;
    end;
  case ItemBlock.IDFlags of
    db_item_LastPositionFlags, db_item_OnlyPositionFlags:
      begin
        SenderItem.Return := db_Item_BlockOverrate;
        Result := False;
        Exit;
      end;
  end;
  BuffInt := BuffInt + (ItemBlock.Size - BlockPOS);
  BuffPointer := Pointer(BuffInt);
  DeformitySize := DeformitySize - (ItemBlock.Size - BlockPOS);
  if dbItem_OnlyReadItemBlockRec(ItemBlock.NextBlockPOS, RecFile, ItemBlock) = False then
    begin
      SenderItem.Return := ItemBlock.Return;
      Result := False;
      Exit;
    end;

  if BlockPOS = 0 then
      goto Rep_Label;
  BlockPOS := 0;
  goto Rep_Label;
end;

function dbItem_BlockAppendWriteData(var RecFile: TRecFile; var SenderItem: TItem; var Buffers; const Size: Int64): Boolean;
begin
  if (SenderItem.BlockCount > 0) and ((SenderItem.CurrentItemBlock.DataBlockPOS + SenderItem.CurrentItemBlock.Size) = umlFileGetSize(RecFile)) then
    begin
      if umlFileSeek(RecFile, umlFileGetSize(RecFile)) = False then
        begin
          SenderItem.Return := db_Item_SetPosError;
          Result := False;
          Exit;
        end;
      if umlFileWrite(RecFile, Size, Buffers) = False then
        begin
          SenderItem.Return := db_Item_BlockWriteError;
          Result := False;
          Exit;
        end;
      SenderItem.CurrentItemBlock.Size := SenderItem.CurrentItemBlock.Size + Size;
      if dbItem_OnlyWriteItemBlockRec(SenderItem.CurrentItemBlock.CurrentBlockPOS, RecFile, SenderItem.CurrentItemBlock) = False then
        begin
          SenderItem.Return := SenderItem.CurrentItemBlock.Return;
          Result := False;
          Exit;
        end;
      SenderItem.Size := SenderItem.Size + Size;
      if dbItem_OnlyWriteItemRec(SenderItem.RHeader.DataMainPOS, RecFile, SenderItem) = False then
        begin
          Result := False;
          Exit;
        end;
      SenderItem.CurrentBlockSeekPOS := SenderItem.CurrentItemBlock.Size;
      SenderItem.CurrentFileSeekPOS := SenderItem.CurrentItemBlock.DataBlockPOS + SenderItem.CurrentItemBlock.Size;
      SenderItem.DataModify := True;
      SenderItem.Return := db_Item_ok;
      Result := True;
      Exit;
    end;

  if dbItem_BlockCreate(RecFile, SenderItem) = False then
    begin
      Result := False;
      Exit;
    end;

  if umlFileSeek(RecFile, SenderItem.CurrentItemBlock.DataBlockPOS) = False then
    begin
      SenderItem.Return := db_Item_SetPosError;
      Result := False;
      Exit;
    end;

  if umlFileWrite(RecFile, Size, Buffers) = False then
    begin
      SenderItem.Return := db_Item_BlockWriteError;
      Result := False;
      Exit;
    end;
  SenderItem.CurrentItemBlock.Size := Size;
  if dbItem_OnlyWriteItemBlockRec(SenderItem.CurrentItemBlock.CurrentBlockPOS, RecFile, SenderItem.CurrentItemBlock) = False then
    begin
      SenderItem.Return := SenderItem.CurrentItemBlock.Return;
      Result := False;
      Exit;
    end;
  SenderItem.Size := SenderItem.Size + Size;
  if dbItem_OnlyWriteItemRec(SenderItem.RHeader.DataMainPOS, RecFile, SenderItem) = False then
    begin
      Result := False;
      Exit;
    end;
  SenderItem.CurrentBlockSeekPOS := SenderItem.CurrentItemBlock.Size;
  SenderItem.CurrentFileSeekPOS := SenderItem.CurrentItemBlock.DataBlockPOS + SenderItem.CurrentItemBlock.Size;
  SenderItem.DataModify := True;
  SenderItem.Return := db_Item_ok;
  Result := True;
end;

function dbItem_BlockWriteData(var RecFile: TRecFile; var SenderItem: TItem; var Buffers; const Size: Int64): Boolean;
label
  Rep_Label;
var
  BuffPointer            : Pointer;
  BuffInt                : NativeUInt;
  DeformitySize, BlockPOS: Int64;
  ItemBlock              : TItemBlock;
begin
  if (SenderItem.Size = 0) or (SenderItem.BlockCount = 0) then
    begin
      Result := dbItem_BlockAppendWriteData(RecFile, SenderItem, Buffers, Size);
      Exit;
    end;
  case SenderItem.CurrentItemBlock.IDFlags of
    db_item_LastPositionFlags, db_item_OnlyPositionFlags:
      begin
        if SenderItem.CurrentBlockSeekPOS = SenderItem.CurrentItemBlock.Size then
          begin
            Result := dbItem_BlockAppendWriteData(RecFile, SenderItem, Buffers, Size);
            Exit;
          end;
      end;
  end;

  if SenderItem.CurrentBlockSeekPOS > SenderItem.CurrentItemBlock.Size then
    begin
      SenderItem.Return := db_Item_BlockPositionError;
      Result := False;
      Exit;
    end;
  ItemBlock := SenderItem.CurrentItemBlock;
  BlockPOS := SenderItem.CurrentBlockSeekPOS;
  BuffInt := NativeUInt(@Buffers);
  BuffPointer := Pointer(BuffInt);
  DeformitySize := Size;
Rep_Label:
  if ItemBlock.Size - BlockPOS = 0 then
    begin
      case ItemBlock.IDFlags of
        db_item_LastPositionFlags, db_item_OnlyPositionFlags:
          begin
            Result := dbItem_BlockAppendWriteData(RecFile, SenderItem, BuffPointer^, DeformitySize);
            Exit;
          end;
      end;
      if dbItem_OnlyReadItemBlockRec(ItemBlock.NextBlockPOS, RecFile, ItemBlock) = False then
        begin
          SenderItem.Return := ItemBlock.Return;
          Result := False;
          Exit;
        end;
      if BlockPOS > 0 then
          BlockPOS := 0;
      while (ItemBlock.Size - BlockPOS) = 0 do
        begin
          case ItemBlock.IDFlags of
            db_item_LastPositionFlags:
              begin
                Result := dbItem_BlockAppendWriteData(RecFile, SenderItem, BuffPointer^, DeformitySize);
                Exit;
              end;
          end;
          if dbItem_OnlyReadItemBlockRec(ItemBlock.NextBlockPOS, RecFile, ItemBlock) = False then
            begin
              SenderItem.Return := ItemBlock.Return;
              Result := False;
              Exit;
            end;
        end;
    end;

  if umlFileSeek(RecFile, ItemBlock.DataBlockPOS + BlockPOS) = False then
    begin
      SenderItem.Return := db_Item_SetPosError;
      Result := False;
      Exit;
    end;

  if DeformitySize <= ItemBlock.Size - BlockPOS then
    begin
      if umlFileWrite(RecFile, DeformitySize, BuffPointer^) = False then
        begin
          SenderItem.Return := db_Item_BlockWriteError;
          Result := False;
          Exit;
        end;
      SenderItem.CurrentBlockSeekPOS := BlockPOS + DeformitySize;
      SenderItem.CurrentFileSeekPOS := ItemBlock.DataBlockPOS + (BlockPOS + DeformitySize);
      SenderItem.CurrentItemBlock := ItemBlock;
      SenderItem.DataModify := True;
      SenderItem.Return := db_Item_ok;
      Result := True;
      Exit;
    end;

  if umlFileWrite(RecFile, ItemBlock.Size - BlockPOS, BuffPointer^) = False then
    begin
      SenderItem.Return := db_Item_BlockWriteError;
      Result := False;
      Exit;
    end;
  BuffInt := BuffInt + (ItemBlock.Size - BlockPOS);
  BuffPointer := Pointer(BuffInt);
  DeformitySize := DeformitySize - (ItemBlock.Size - BlockPOS);
  case ItemBlock.IDFlags of
    db_item_LastPositionFlags, db_item_OnlyPositionFlags:
      begin
        Result := dbItem_BlockAppendWriteData(RecFile, SenderItem, BuffPointer^, DeformitySize);
        Exit;
      end;
  end;
  if dbItem_OnlyReadItemBlockRec(ItemBlock.NextBlockPOS, RecFile, ItemBlock) = False then
    begin
      SenderItem.Return := ItemBlock.Return;
      Result := False;
      Exit;
    end;

  if BlockPOS = 0 then
      goto Rep_Label;
  BlockPOS := 0;
  goto Rep_Label;
end;

function dbItem_BlockSeekPOS(var RecFile: TRecFile; var SenderItem: TItem; const Position: Int64): Boolean;
var
  ItemBlock   : TItemBlock;
  DeformityInt: Int64;
begin
  if (Position = 0) and (SenderItem.Size = 0) then
    begin
      SenderItem.Return := db_Item_ok;
      Result := True;
      Exit;
    end;

  if (Position > SenderItem.Size) or (SenderItem.BlockCount = 0) then
    begin
      SenderItem.Return := db_Item_BlockOverrate;
      Result := False;
      Exit;
    end;
  DeformityInt := Position;
  if dbItem_OnlyReadItemBlockRec(SenderItem.FirstBlockPOS, RecFile, ItemBlock) = False then
    begin
      SenderItem.Return := ItemBlock.Return;
      Result := False;
      Exit;
    end;

  if DeformityInt <= ItemBlock.Size then
    begin
      SenderItem.CurrentBlockSeekPOS := ItemBlock.Size - (ItemBlock.Size - DeformityInt);
      SenderItem.CurrentFileSeekPOS := ItemBlock.DataBlockPOS + SenderItem.CurrentBlockSeekPOS;
      SenderItem.CurrentItemBlock := ItemBlock;
      SenderItem.Return := db_Item_ok;
      Result := True;
      Exit;
    end;
  case ItemBlock.IDFlags of
    db_item_LastPositionFlags, db_item_OnlyPositionFlags:
      begin
        SenderItem.Return := db_Item_BlockOverrate;
        Result := False;
        Exit;
      end;
  end;
  DeformityInt := DeformityInt - ItemBlock.Size;
  while dbItem_OnlyReadItemBlockRec(ItemBlock.NextBlockPOS, RecFile, ItemBlock) = True do
    begin
      if DeformityInt <= ItemBlock.Size then
        begin
          SenderItem.CurrentBlockSeekPOS := ItemBlock.Size - (ItemBlock.Size - DeformityInt);
          SenderItem.CurrentFileSeekPOS := ItemBlock.DataBlockPOS + SenderItem.CurrentBlockSeekPOS;
          SenderItem.CurrentItemBlock := ItemBlock;
          SenderItem.Return := db_Item_ok;
          Result := True;
          Exit;
        end;
      case ItemBlock.IDFlags of
        db_item_LastPositionFlags:
          begin
            SenderItem.Return := db_Item_BlockOverrate;
            Result := False;
            Exit;
          end;
      end;
      DeformityInt := DeformityInt - ItemBlock.Size;
    end;
  SenderItem.Return := ItemBlock.Return;
  Result := False;
end;

function dbItem_BlockGetPOS(var RecFile: TRecFile; var SenderItem: TItem): Int64;
var
  ItemBlock: TItemBlock;
begin
  if (SenderItem.Size = 0) or (SenderItem.BlockCount = 0) then
    begin
      SenderItem.Return := db_Item_BlockOverrate;
      Result := 0;
      Exit;
    end;

  if SenderItem.CurrentBlockSeekPOS > SenderItem.CurrentItemBlock.Size then
    begin
      SenderItem.Return := db_Item_BlockPositionError;
      Result := 0;
      Exit;
    end;
  Result := SenderItem.CurrentBlockSeekPOS;
  case SenderItem.CurrentItemBlock.IDFlags of
    db_item_FirstPositionFlags, db_item_OnlyPositionFlags:
      begin
        SenderItem.Return := db_Item_ok;
        Exit;
      end;
  end;
  if dbItem_OnlyReadItemBlockRec(SenderItem.CurrentItemBlock.PrevBlockPOS, RecFile, ItemBlock) = False then
    begin
      SenderItem.Return := ItemBlock.Return;
      Result := 0;
      Exit;
    end;
  Result := Result + ItemBlock.Size;
  case ItemBlock.IDFlags of
    db_item_FirstPositionFlags, db_item_OnlyPositionFlags:
      begin
        SenderItem.Return := db_Item_ok;
        Exit;
      end;
  end;
  while dbItem_OnlyReadItemBlockRec(ItemBlock.PrevBlockPOS, RecFile, ItemBlock) = True do
    begin
      Result := Result + ItemBlock.Size;
      if ItemBlock.IDFlags = db_item_FirstPositionFlags then
        begin
          SenderItem.Return := db_Item_ok;
          Exit;
        end;
    end;
  SenderItem.Return := ItemBlock.Return;
  Result := 0;
end;

function dbItem_BlockSeekStartPOS(var RecFile: TRecFile; var SenderItem: TItem): Boolean;
begin
  if SenderItem.BlockCount = 0 then
    begin
      SenderItem.Return := db_Item_BlockOverrate;
      Result := False;
      Exit;
    end;
  if dbItem_OnlyReadItemBlockRec(SenderItem.FirstBlockPOS, RecFile, SenderItem.CurrentItemBlock) = False then
    begin
      SenderItem.Return := SenderItem.CurrentItemBlock.Return;
      Result := False;
      Exit;
    end;
  SenderItem.CurrentBlockSeekPOS := 0;
  SenderItem.CurrentFileSeekPOS := SenderItem.CurrentItemBlock.DataBlockPOS;
  SenderItem.Return := db_Item_ok;
  Result := True;
end;

function dbItem_BlockSeekLastPOS(var RecFile: TRecFile; var SenderItem: TItem): Boolean;
begin
  if SenderItem.BlockCount = 0 then
    begin
      SenderItem.Return := db_Item_BlockOverrate;
      Result := False;
      Exit;
    end;
  if dbItem_OnlyReadItemBlockRec(SenderItem.LastBlockPOS, RecFile, SenderItem.CurrentItemBlock) = False then
    begin
      SenderItem.Return := SenderItem.CurrentItemBlock.Return;
      Result := False;
      Exit;
    end;
  SenderItem.CurrentBlockSeekPOS := 0;
  SenderItem.CurrentFileSeekPOS := SenderItem.CurrentItemBlock.DataBlockPOS;
  SenderItem.Return := db_Item_ok;
  Result := True;
end;

function dbField_WriteRec(const POS: Int64; var RecFile: TRecFile; var SenderField: TField): Boolean;
begin
  if dbHeader_WriteRec(POS, RecFile, SenderField.RHeader) = False then
    begin
      SenderField.Return := SenderField.RHeader.Return;
      Result := False;
      Exit;
    end;
  if umlFileSeek(RecFile, SenderField.RHeader.DataMainPOS) = False then
    begin
      SenderField.Return := db_Field_SetPosError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(RecFile, umlPositionLength, SenderField.HigherFieldPOS) = False then
    begin
      SenderField.Return := db_Field_WriteHeaderFieldPosError;
      Result := False;
      Exit;
    end;
  if umlFileWriteStr(RecFile, SenderField.Description) = False then
    begin
      SenderField.Return := db_Field_WriteDescriptionError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(RecFile, umlCountLength, SenderField.HeaderCount) = False then
    begin
      SenderField.Return := db_Field_WriteCountError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(RecFile, umlPositionLength, SenderField.FirstHeaderPOS) = False then
    begin
      SenderField.Return := db_Field_WriteFirstPosError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(RecFile, umlPositionLength, SenderField.LastHeaderPOS) = False then
    begin
      SenderField.Return := db_Field_WriteLastPosError;
      Result := False;
      Exit;
    end;
  SenderField.Return := db_Field_ok;
  Result := True;
end;

function dbField_ReadRec(const POS: Int64; var RecFile: TRecFile; var SenderField: TField): Boolean;
begin
  if dbHeader_ReadRec(POS, RecFile, SenderField.RHeader) = False then
    begin
      SenderField.Return := SenderField.RHeader.Return;
      Result := False;
      Exit;
    end;
  if umlFileSeek(RecFile, SenderField.RHeader.DataMainPOS) = False then
    begin
      SenderField.Return := db_Field_SetPosError;
      Result := False;
      Exit;
    end;
  if umlFileRead(RecFile, umlPositionLength, SenderField.HigherFieldPOS) = False then
    begin
      SenderField.Return := db_Field_ReadHeaderFieldPosError;
      Result := False;
      Exit;
    end;
  if umlFileReadStr(RecFile, SenderField.Description) = False then
    begin
      SenderField.Return := db_Field_ReadDescriptionError;
      Result := False;
      Exit;
    end;
  if umlFileRead(RecFile, umlCountLength, SenderField.HeaderCount) = False then
    begin
      SenderField.Return := db_Field_ReadCountError;
      Result := False;
      Exit;
    end;
  if umlFileRead(RecFile, umlPositionLength, SenderField.FirstHeaderPOS) = False then
    begin
      SenderField.Return := db_Field_ReadFirstPosError;
      Result := False;
      Exit;
    end;
  if umlFileRead(RecFile, umlPositionLength, SenderField.LastHeaderPOS) = False then
    begin
      SenderField.Return := db_Field_ReadLastPosError;
      Result := False;
      Exit;
    end;
  SenderField.Return := db_Field_ok;
  Result := True;
end;

function dbField_OnlyWriteFieldRec(const POS: Int64; var RecFile: TRecFile; var SenderField: TField): Boolean;
begin
  if umlFileSeek(RecFile, POS) = False then
    begin
      SenderField.Return := db_Field_SetPosError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(RecFile, umlPositionLength, SenderField.HigherFieldPOS) = False then
    begin
      SenderField.Return := db_Field_WriteHeaderFieldPosError;
      Result := False;
      Exit;
    end;
  if umlFileWriteStr(RecFile, SenderField.Description) = False then
    begin
      SenderField.Return := db_Field_WriteDescriptionError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(RecFile, umlCountLength, SenderField.HeaderCount) = False then
    begin
      SenderField.Return := db_Field_WriteCountError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(RecFile, umlPositionLength, SenderField.FirstHeaderPOS) = False then
    begin
      SenderField.Return := db_Field_WriteFirstPosError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(RecFile, umlPositionLength, SenderField.LastHeaderPOS) = False then
    begin
      SenderField.Return := db_Field_WriteLastPosError;
      Result := False;
      Exit;
    end;
  SenderField.Return := db_Field_ok;
  Result := True;
end;

function dbField_OnlyReadFieldRec(const POS: Int64; var RecFile: TRecFile; var SenderField: TField): Boolean;
begin
  if umlFileSeek(RecFile, POS) = False then
    begin
      SenderField.Return := db_Field_SetPosError;
      Result := False;
      Exit;
    end;
  if umlFileRead(RecFile, umlPositionLength, SenderField.HigherFieldPOS) = False then
    begin
      SenderField.Return := db_Field_ReadHeaderFieldPosError;
      Result := False;
      Exit;
    end;
  if umlFileReadStr(RecFile, SenderField.Description) = False then
    begin
      SenderField.Return := db_Field_ReadDescriptionError;
      Result := False;
      Exit;
    end;
  if umlFileRead(RecFile, umlCountLength, SenderField.HeaderCount) = False then
    begin
      SenderField.Return := db_Field_ReadCountError;
      Result := False;
      Exit;
    end;
  if umlFileRead(RecFile, umlPositionLength, SenderField.FirstHeaderPOS) = False then
    begin
      SenderField.Return := db_Field_ReadFirstPosError;
      Result := False;
      Exit;
    end;
  if umlFileRead(RecFile, umlPositionLength, SenderField.LastHeaderPOS) = False then
    begin
      SenderField.Return := db_Field_ReadLastPosError;
      Result := False;
      Exit;
    end;
  SenderField.Return := db_Field_ok;
  Result := True;
end;

function dbField_GetPOSField(const POS: Int64; var RecFile: TRecFile): TField;
begin
  dbField_ReadRec(POS, RecFile, Result);
end;

function dbField_GetFirstHeader(const POS: Int64; var RecFile: TRecFile): THeader;
var
  _Field: TField;
begin
  if dbField_ReadRec(POS, RecFile, _Field) = False then
      Exit;
  dbHeader_ReadRec(_Field.FirstHeaderPOS, RecFile, Result);
end;

function dbField_GetLastHeader(const POS: Int64; var RecFile: TRecFile): THeader;
var
  _Field: TField;
begin
  if dbField_ReadRec(POS, RecFile, _Field) = False then
      Exit;
  dbHeader_ReadRec(_Field.LastHeaderPOS, RecFile, Result);
end;

function dbField_OnlyFindFirstName(const Name: string; const POS: Int64; var RecFile: TRecFile; var SenderFieldSearch: TFieldSearch): Boolean;
var
  _Field: TField;
begin
  SenderFieldSearch.InitFlags := False;
  if dbField_ReadRec(POS, RecFile, _Field) = False then
    begin
      SenderFieldSearch.Return := _Field.Return;
      Result := False;
      Exit;
    end;
  if _Field.HeaderCount = 0 then
    begin
      SenderFieldSearch.Return := db_Header_NotFindHeader;
      Result := False;
      Exit;
    end;
  if dbHeader_FindNext(name, _Field.FirstHeaderPOS, _Field.LastHeaderPOS, RecFile, SenderFieldSearch.RHeader) = False then
    begin
      SenderFieldSearch.Return := SenderFieldSearch.RHeader.Return;
      Result := False;
      Exit;
    end;
  SenderFieldSearch.InitFlags := True;
  SenderFieldSearch.PositionID := SenderFieldSearch.RHeader.PositionID;
  SenderFieldSearch.OverPOS := _Field.LastHeaderPOS;
  SenderFieldSearch.StartPOS := SenderFieldSearch.RHeader.NextHeader;
  SenderFieldSearch.Name := name;
  SenderFieldSearch.ID := SenderFieldSearch.RHeader.ID;
  SenderFieldSearch.Return := SenderFieldSearch.RHeader.Return;
  Result := True;
end;

function dbField_OnlyFindNextName(var RecFile: TRecFile; var SenderFieldSearch: TFieldSearch): Boolean;
begin
  if SenderFieldSearch.InitFlags = False then
    begin
      SenderFieldSearch.Return := db_Field_NotInitSearch;
      Result := False;
      Exit;
    end;
  case SenderFieldSearch.PositionID of
    db_Header_OnlyPositionFlags, db_Header_LastPositionFlags:
      begin
        SenderFieldSearch.InitFlags := False;
        SenderFieldSearch.Return := db_Header_NotFindHeader;
        Result := False;
        Exit;
      end;
  end;
  if dbHeader_FindNext(SenderFieldSearch.Name, SenderFieldSearch.StartPOS, SenderFieldSearch.OverPOS, RecFile, SenderFieldSearch.RHeader) = False then
    begin
      SenderFieldSearch.InitFlags := False;
      SenderFieldSearch.Return := SenderFieldSearch.RHeader.Return;
      Result := False;
      Exit;
    end;
  SenderFieldSearch.PositionID := SenderFieldSearch.RHeader.PositionID;
  SenderFieldSearch.StartPOS := SenderFieldSearch.RHeader.NextHeader;
  SenderFieldSearch.ID := SenderFieldSearch.RHeader.ID;
  SenderFieldSearch.Return := SenderFieldSearch.RHeader.Return;
  Result := True;
end;

function dbField_OnlyFindLastName(const Name: string; const POS: Int64; var RecFile: TRecFile; var SenderFieldSearch: TFieldSearch): Boolean;
var
  _Field: TField;
begin
  SenderFieldSearch.InitFlags := False;
  if dbField_ReadRec(POS, RecFile, _Field) = False then
    begin
      SenderFieldSearch.Return := _Field.Return;
      Result := False;
      Exit;
    end;
  if _Field.HeaderCount = 0 then
    begin
      SenderFieldSearch.Return := db_Header_NotFindHeader;
      Result := False;
      Exit;
    end;
  if dbHeader_FindPrev(name, _Field.LastHeaderPOS, _Field.FirstHeaderPOS, RecFile, SenderFieldSearch.RHeader) = False then
    begin
      SenderFieldSearch.Return := SenderFieldSearch.RHeader.Return;
      Result := False;
      Exit;
    end;
  SenderFieldSearch.InitFlags := True;
  SenderFieldSearch.PositionID := SenderFieldSearch.RHeader.PositionID;
  SenderFieldSearch.OverPOS := _Field.FirstHeaderPOS;
  SenderFieldSearch.StartPOS := SenderFieldSearch.RHeader.PrevHeader;
  SenderFieldSearch.Name := name;
  SenderFieldSearch.ID := SenderFieldSearch.RHeader.ID;
  SenderFieldSearch.Return := SenderFieldSearch.RHeader.Return;
  Result := True;
end;

function dbField_OnlyFindPrevName(var RecFile: TRecFile; var SenderFieldSearch: TFieldSearch): Boolean;
begin
  if SenderFieldSearch.InitFlags = False then
    begin
      SenderFieldSearch.Return := db_Field_NotInitSearch;
      Result := False;
      Exit;
    end;
  case SenderFieldSearch.PositionID of
    db_Header_OnlyPositionFlags, db_Header_FirstPositionFlags:
      begin
        SenderFieldSearch.InitFlags := False;
        SenderFieldSearch.Return := db_Header_NotFindHeader;
        Result := False;
        Exit;
      end;
  end;
  if dbHeader_FindPrev(SenderFieldSearch.Name, SenderFieldSearch.StartPOS, SenderFieldSearch.OverPOS, RecFile, SenderFieldSearch.RHeader) = False then
    begin
      SenderFieldSearch.InitFlags := False;
      SenderFieldSearch.Return := SenderFieldSearch.RHeader.Return;
      Result := False;
      Exit;
    end;
  SenderFieldSearch.PositionID := SenderFieldSearch.RHeader.PositionID;
  SenderFieldSearch.StartPOS := SenderFieldSearch.RHeader.PrevHeader;
  SenderFieldSearch.ID := SenderFieldSearch.RHeader.ID;
  SenderFieldSearch.Return := SenderFieldSearch.RHeader.Return;
  Result := True;
end;

function dbField_FindFirst(const Name: string; const ID: Int64; const POS: Int64; var RecFile: TRecFile; var SenderFieldSearch: TFieldSearch): Boolean;
var
  _Field: TField;
begin
  SenderFieldSearch.InitFlags := False;
  if dbField_ReadRec(POS, RecFile, _Field) = False then
    begin
      SenderFieldSearch.Return := _Field.Return;
      Result := False;
      Exit;
    end;
  if _Field.HeaderCount = 0 then
    begin
      SenderFieldSearch.Return := db_Header_NotFindHeader;
      Result := False;
      Exit;
    end;
  SenderFieldSearch.OverPOS := _Field.LastHeaderPOS;
  SenderFieldSearch.StartPOS := _Field.FirstHeaderPOS;
  while dbHeader_FindNext(name, SenderFieldSearch.StartPOS, SenderFieldSearch.OverPOS, RecFile, SenderFieldSearch.RHeader) = True do
    begin
      SenderFieldSearch.StartPOS := SenderFieldSearch.RHeader.NextHeader;
      if SenderFieldSearch.RHeader.ID = ID then
        begin
          SenderFieldSearch.InitFlags := True;
          SenderFieldSearch.PositionID := SenderFieldSearch.RHeader.PositionID;
          SenderFieldSearch.Name := name;
          SenderFieldSearch.ID := ID;
          SenderFieldSearch.Return := SenderFieldSearch.RHeader.Return;
          Result := True;
          Exit;
        end;
      if (SenderFieldSearch.RHeader.PositionID = db_Header_OnlyPositionFlags) or (SenderFieldSearch.RHeader.PositionID = db_Header_LastPositionFlags) then
        begin
          SenderFieldSearch.Return := db_Header_NotFindHeader;
          Result := False;
          Exit;
        end;
    end;
  SenderFieldSearch.Return := SenderFieldSearch.RHeader.Return;
  Result := False;
end;

function dbField_FindNext(var RecFile: TRecFile; var SenderFieldSearch: TFieldSearch): Boolean;
begin
  if SenderFieldSearch.InitFlags = False then
    begin
      SenderFieldSearch.Return := db_Field_NotInitSearch;
      Result := False;
      Exit;
    end;
  case SenderFieldSearch.PositionID of
    db_Header_OnlyPositionFlags, db_Header_LastPositionFlags:
      begin
        SenderFieldSearch.InitFlags := False;
        SenderFieldSearch.Return := db_Header_NotFindHeader;
        Result := False;
        Exit;
      end;
  end;
  while dbHeader_FindNext(SenderFieldSearch.Name, SenderFieldSearch.StartPOS, SenderFieldSearch.OverPOS, RecFile, SenderFieldSearch.RHeader) = True do
    begin
      SenderFieldSearch.StartPOS := SenderFieldSearch.RHeader.NextHeader;

      if SenderFieldSearch.RHeader.ID = SenderFieldSearch.ID then
        begin
          SenderFieldSearch.PositionID := SenderFieldSearch.RHeader.PositionID;
          SenderFieldSearch.Return := SenderFieldSearch.RHeader.Return;
          Result := True;
          Exit;
        end;

      if SenderFieldSearch.RHeader.PositionID = db_Header_LastPositionFlags then
        begin
          SenderFieldSearch.InitFlags := False;
          SenderFieldSearch.Return := db_Header_NotFindHeader;
          Result := False;
          Exit;
        end;
    end;
  SenderFieldSearch.InitFlags := False;
  SenderFieldSearch.Return := SenderFieldSearch.RHeader.Return;
  Result := False;
end;

function dbField_FindLast(const Name: string; const ID: Int64; const POS: Int64; var RecFile: TRecFile; var SenderFieldSearch: TFieldSearch): Boolean;
var
  _Field: TField;
begin
  SenderFieldSearch.InitFlags := False;
  if dbField_ReadRec(POS, RecFile, _Field) = False then
    begin
      SenderFieldSearch.Return := _Field.Return;
      Result := False;
      Exit;
    end;
  if _Field.HeaderCount = 0 then
    begin
      SenderFieldSearch.Return := db_Header_NotFindHeader;
      Result := False;
      Exit;
    end;
  SenderFieldSearch.OverPOS := _Field.FirstHeaderPOS;
  SenderFieldSearch.StartPOS := _Field.LastHeaderPOS;
  while dbHeader_FindPrev(name, SenderFieldSearch.StartPOS, SenderFieldSearch.OverPOS, RecFile, SenderFieldSearch.RHeader) = True do
    begin
      SenderFieldSearch.StartPOS := SenderFieldSearch.RHeader.PrevHeader;
      if SenderFieldSearch.RHeader.ID = ID then
        begin
          SenderFieldSearch.InitFlags := True;
          SenderFieldSearch.PositionID := SenderFieldSearch.RHeader.PositionID;
          SenderFieldSearch.Name := name;
          SenderFieldSearch.ID := ID;
          SenderFieldSearch.Return := SenderFieldSearch.RHeader.Return;
          Result := True;
          Exit;
        end;
      if (SenderFieldSearch.RHeader.PositionID = db_Header_OnlyPositionFlags) or (SenderFieldSearch.RHeader.PositionID = db_Header_FirstPositionFlags) then
        begin
          SenderFieldSearch.Return := db_Header_NotFindHeader;
          Result := False;
          Exit;
        end;
    end;
  SenderFieldSearch.Return := SenderFieldSearch.RHeader.Return;
  Result := False;
end;

function dbField_FindPrev(var RecFile: TRecFile; var SenderFieldSearch: TFieldSearch): Boolean;
begin
  if SenderFieldSearch.InitFlags = False then
    begin
      SenderFieldSearch.Return := db_Field_NotInitSearch;
      Result := False;
      Exit;
    end;
  case SenderFieldSearch.PositionID of
    db_Header_OnlyPositionFlags, db_Header_FirstPositionFlags:
      begin
        SenderFieldSearch.InitFlags := False;
        SenderFieldSearch.Return := db_Header_NotFindHeader;
        Result := False;
        Exit;
      end;
  end;
  while dbHeader_FindPrev(SenderFieldSearch.Name, SenderFieldSearch.StartPOS, SenderFieldSearch.OverPOS, RecFile, SenderFieldSearch.RHeader) = True do
    begin
      SenderFieldSearch.StartPOS := SenderFieldSearch.RHeader.PrevHeader;

      if SenderFieldSearch.RHeader.ID = SenderFieldSearch.ID then
        begin
          SenderFieldSearch.PositionID := SenderFieldSearch.RHeader.PositionID;
          SenderFieldSearch.Return := SenderFieldSearch.RHeader.Return;
          Result := True;
          Exit;
        end;

      if SenderFieldSearch.RHeader.PositionID = db_Header_FirstPositionFlags then
        begin
          SenderFieldSearch.InitFlags := False;
          SenderFieldSearch.Return := db_Header_NotFindHeader;
          Result := False;
          Exit;
        end;
    end;
  SenderFieldSearch.InitFlags := False;
  SenderFieldSearch.Return := SenderFieldSearch.RHeader.Return;
  Result := False;
end;

function dbField_FindFirstItem(const Name: string; const ItemExtID: Int64; const POS: Int64; var RecFile: TRecFile; var SenderFieldSearch: TFieldSearch): Boolean;
var
  _Item: TItem;
begin
  if dbField_FindFirst(name, db_Header_ItemID, POS, RecFile, SenderFieldSearch) = False then
    begin
      Result := False;
      Exit;
    end;
  if dbItem_ReadRec(SenderFieldSearch.RHeader.CurrentHeader, RecFile, _Item) = False then
    begin
      SenderFieldSearch.Return := _Item.Return;
      Result := False;
      Exit;
    end;

  if _Item.ExtID = ItemExtID then
    begin
      Result := True;
      Exit;
    end;

  while dbField_FindNext(RecFile, SenderFieldSearch) = True do
    begin
      if dbItem_ReadRec(SenderFieldSearch.RHeader.CurrentHeader, RecFile, _Item) = False then
        begin
          SenderFieldSearch.Return := _Item.Return;
          Result := False;
          Exit;
        end;
      if _Item.ExtID = ItemExtID then
        begin
          Result := True;
          Exit;
        end;
    end;
  Result := False;
end;

function dbField_FindNextItem(const ItemExtID: Int64; var RecFile: TRecFile; var SenderFieldSearch: TFieldSearch): Boolean;
var
  _Item: TItem;
begin
  while dbField_FindNext(RecFile, SenderFieldSearch) = True do
    begin
      if dbItem_ReadRec(SenderFieldSearch.RHeader.CurrentHeader, RecFile, _Item) = False then
        begin
          SenderFieldSearch.Return := _Item.Return;
          Result := False;
          Exit;
        end;
      if _Item.ExtID = ItemExtID then
        begin
          Result := True;
          Exit;
        end;
    end;
  Result := False;
end;

function dbField_FindLastItem(const Name: string; const ItemExtID: Int64; const POS: Int64; var RecFile: TRecFile; var SenderFieldSearch: TFieldSearch): Boolean;
var
  _Item: TItem;
begin
  if dbField_FindLast(name, db_Header_ItemID, POS, RecFile, SenderFieldSearch) = False then
    begin
      Result := False;
      Exit;
    end;
  if dbItem_ReadRec(SenderFieldSearch.RHeader.CurrentHeader, RecFile, _Item) = False then
    begin
      SenderFieldSearch.Return := _Item.Return;
      Result := False;
      Exit;
    end;

  if _Item.ExtID = ItemExtID then
    begin
      Result := True;
      Exit;
    end;

  while dbField_FindPrev(RecFile, SenderFieldSearch) = True do
    begin
      if dbItem_ReadRec(SenderFieldSearch.RHeader.CurrentHeader, RecFile, _Item) = False then
        begin
          SenderFieldSearch.Return := _Item.Return;
          Result := False;
          Exit;
        end;
      if _Item.ExtID = ItemExtID then
        begin
          Result := True;
          Exit;
        end;
    end;
  Result := False;
end;

function dbField_FindPrevItem(const ItemExtID: Int64; var RecFile: TRecFile; var SenderFieldSearch: TFieldSearch): Boolean;
var
  _Item: TItem;
begin
  while dbField_FindPrev(RecFile, SenderFieldSearch) = True do
    begin
      if dbItem_ReadRec(SenderFieldSearch.RHeader.CurrentHeader, RecFile, _Item) = False then
        begin
          SenderFieldSearch.Return := _Item.Return;
          Result := False;
          Exit;
        end;
      if _Item.ExtID = ItemExtID then
        begin
          Result := True;
          Exit;
        end;
    end;
  Result := False;
end;

function dbField_ExistItem(const Name: string; const ItemExtID: Int64; const POS: Int64; var RecFile: TRecFile): Boolean;
var
  _FieldSearch: TFieldSearch;
begin
  Result := dbField_FindFirstItem(name, ItemExtID, POS, RecFile, _FieldSearch);
end;

function dbField_ExistHeader(const Name: string; const ID: Int64; const POS: Int64; var RecFile: TRecFile): Boolean;
var
  _FieldSearch: TFieldSearch;
begin
  Result := dbField_FindFirst(name, ID, POS, RecFile, _FieldSearch);
end;

function dbField_CreateHeader(const Name: string; const ID: Int64; const POS: Int64; var RecFile: TRecFile; var SenderHeader: THeader): Boolean;
var
  _Field : TField;
  _Header: THeader;
begin
  if dbField_ReadRec(POS, RecFile, _Field) = False then
    begin
      SenderHeader.Return := _Field.Return;
      Result := False;
      Exit;
    end;
  SenderHeader.ID := ID;
  SenderHeader.Name := name;
  case _Field.HeaderCount of
    0:
      begin
        _Field.HeaderCount := 1;
        _Field.FirstHeaderPOS := umlFileGetSize(RecFile);
        _Field.LastHeaderPOS := _Field.FirstHeaderPOS;
        _Field.RHeader.LastModifyTime := umlDefaultTime;
        SenderHeader.PositionID := db_Header_OnlyPositionFlags;
        SenderHeader.NextHeader := _Field.LastHeaderPOS;
        SenderHeader.PrevHeader := _Field.FirstHeaderPOS;
        SenderHeader.CurrentHeader := _Field.FirstHeaderPOS;
        SenderHeader.CreateTime := umlDefaultTime;
        SenderHeader.LastModifyTime := umlDefaultTime;
        SenderHeader.DataMainPOS := SenderHeader.CurrentHeader + db_Header_RecLength;
        if dbField_WriteRec(_Field.RHeader.CurrentHeader, RecFile, _Field) = False then
          begin
            SenderHeader.Return := _Field.Return;
            Result := False;
            Exit;
          end;
        if dbHeader_WriteRec(SenderHeader.CurrentHeader, RecFile, SenderHeader) = False then
          begin
            Result := False;
            Exit;
          end;
      end;
    1:
      begin
        SenderHeader.CurrentHeader := umlFileGetSize(RecFile);
        SenderHeader.NextHeader := _Field.FirstHeaderPOS;
        SenderHeader.PrevHeader := _Field.FirstHeaderPOS;

        if dbHeader_ReadRec(_Field.FirstHeaderPOS, RecFile, _Header) = False then
          begin
            SenderHeader.Return := _Header.Return;
            Result := False;
            Exit;
          end;
        _Header.PrevHeader := SenderHeader.CurrentHeader;
        _Header.NextHeader := SenderHeader.CurrentHeader;
        _Header.PositionID := db_Header_FirstPositionFlags;
        if dbHeader_WriteRec(_Field.FirstHeaderPOS, RecFile, _Header) = False then
          begin
            SenderHeader.Return := _Header.Return;
            Result := False;
            Exit;
          end;
        _Field.HeaderCount := _Field.HeaderCount + 1;
        _Field.LastHeaderPOS := SenderHeader.CurrentHeader;
        _Field.RHeader.LastModifyTime := umlDefaultTime;
        SenderHeader.CreateTime := umlDefaultTime;
        SenderHeader.LastModifyTime := umlDefaultTime;
        SenderHeader.DataMainPOS := SenderHeader.CurrentHeader + db_Header_RecLength;
        SenderHeader.PositionID := db_Header_LastPositionFlags;
        if dbField_WriteRec(_Field.RHeader.CurrentHeader, RecFile, _Field) = False then
          begin
            SenderHeader.Return := _Field.Return;
            Result := False;
            Exit;
          end;
        if dbHeader_WriteRec(SenderHeader.CurrentHeader, RecFile, SenderHeader) = False then
          begin
            Result := False;
            Exit;
          end;
      end;
    else
      begin
        SenderHeader.CurrentHeader := umlFileGetSize(RecFile);

        if dbHeader_ReadRec(_Field.FirstHeaderPOS, RecFile, _Header) = False then
          begin
            SenderHeader.Return := _Header.Return;
            Result := False;
            Exit;
          end;
        _Header.PrevHeader := SenderHeader.CurrentHeader;
        SenderHeader.NextHeader := _Header.CurrentHeader;
        if dbHeader_WriteRec(_Field.FirstHeaderPOS, RecFile, _Header) = False then
          begin
            SenderHeader.Return := _Header.Return;
            Result := False;
            Exit;
          end;

        if dbHeader_ReadRec(_Field.LastHeaderPOS, RecFile, _Header) = False then
          begin
            SenderHeader.Return := _Header.Return;
            Result := False;
            Exit;
          end;
        _Header.NextHeader := SenderHeader.CurrentHeader;
        SenderHeader.PrevHeader := _Field.LastHeaderPOS;
        _Header.PositionID := db_Header_MediumPositionFlags;
        if dbHeader_WriteRec(_Field.LastHeaderPOS, RecFile, _Header) = False then
          begin
            SenderHeader.Return := _Header.Return;
            Result := False;
            Exit;
          end;
        _Field.HeaderCount := _Field.HeaderCount + 1;
        _Field.LastHeaderPOS := SenderHeader.CurrentHeader;
        _Field.RHeader.LastModifyTime := umlDefaultTime;
        SenderHeader.CreateTime := umlDefaultTime;
        SenderHeader.LastModifyTime := umlDefaultTime;
        SenderHeader.DataMainPOS := SenderHeader.CurrentHeader + db_Header_RecLength;
        SenderHeader.PositionID := db_Header_LastPositionFlags;
        if dbField_WriteRec(_Field.RHeader.CurrentHeader, RecFile, _Field) = False then
          begin
            SenderHeader.Return := _Field.Return;
            Result := False;
            Exit;
          end;
        if dbHeader_WriteRec(SenderHeader.CurrentHeader, RecFile, SenderHeader) = False then
          begin
            Result := False;
            Exit;
          end;
      end;
  end;
  SenderHeader.Return := db_Header_ok;
  Result := True;
end;

function dbField_DeleteHeader(const HeaderPOS, FieldPos: Int64; var RecFile: TRecFile; var SenderField: TField): Boolean;
var
  DeleteHeader, SwapHeader: THeader;
begin
  if dbField_ReadRec(FieldPos, RecFile, SenderField) = False then
    begin
      Result := False;
      Exit;
    end;
  case SenderField.HeaderCount of
    0:
      begin
        SenderField.Return := db_Field_DeleteHeaderError;
        Result := False;
        Exit;
      end;
    1:
      begin
        if HeaderPOS = SenderField.FirstHeaderPOS then
          begin
            SenderField.HeaderCount := 0;
            SenderField.FirstHeaderPOS := 0;
            SenderField.LastHeaderPOS := 0;
            SenderField.RHeader.LastModifyTime := umlDefaultTime;
            if dbField_WriteRec(SenderField.RHeader.CurrentHeader, RecFile, SenderField) = False then
              begin
                Result := False;
                Exit;
              end;
            Result := True;
            SenderField.Return := db_Field_ok;
            Exit;
          end;
        Result := False;
        SenderField.Return := db_Field_DeleteHeaderError;
        Exit;
      end;
    2:
      begin
        if dbHeader_ReadRec(HeaderPOS, RecFile, DeleteHeader) = False then
          begin
            SenderField.Return := DeleteHeader.Return;
            Result := False;
            Exit;
          end;
        case DeleteHeader.PositionID of
          db_Header_FirstPositionFlags:
            begin
              if dbHeader_ReadRec(SenderField.LastHeaderPOS, RecFile, DeleteHeader) = False then
                begin
                  SenderField.Return := DeleteHeader.Return;
                  Result := False;
                  Exit;
                end;
              DeleteHeader.NextHeader := DeleteHeader.CurrentHeader;
              DeleteHeader.PrevHeader := DeleteHeader.CurrentHeader;
              DeleteHeader.PositionID := db_Header_OnlyPositionFlags;
              if dbHeader_WriteRec(DeleteHeader.CurrentHeader, RecFile, DeleteHeader) = False then
                begin
                  SenderField.Return := DeleteHeader.Return;
                  Result := False;
                  Exit;
                end;
              SenderField.FirstHeaderPOS := DeleteHeader.CurrentHeader;
              SenderField.LastHeaderPOS := DeleteHeader.CurrentHeader;
              SenderField.HeaderCount := SenderField.HeaderCount - 1;
              SenderField.RHeader.LastModifyTime := umlDefaultTime;
              if dbField_WriteRec(SenderField.RHeader.CurrentHeader, RecFile, SenderField) = False then
                begin
                  Result := False;
                  Exit;
                end;
              SenderField.Return := db_Field_ok;
              Result := True;
            end;
          db_Header_LastPositionFlags:
            begin
              if dbHeader_ReadRec(SenderField.FirstHeaderPOS, RecFile, DeleteHeader) = False then
                begin
                  SenderField.Return := DeleteHeader.Return;
                  Result := False;
                  Exit;
                end;
              DeleteHeader.NextHeader := DeleteHeader.CurrentHeader;
              DeleteHeader.PrevHeader := DeleteHeader.CurrentHeader;
              DeleteHeader.PositionID := db_Header_OnlyPositionFlags;
              if dbHeader_WriteRec(DeleteHeader.CurrentHeader, RecFile, DeleteHeader) = False then
                begin
                  SenderField.Return := DeleteHeader.Return;
                  Result := False;
                  Exit;
                end;
              SenderField.FirstHeaderPOS := DeleteHeader.CurrentHeader;
              SenderField.LastHeaderPOS := DeleteHeader.CurrentHeader;
              SenderField.HeaderCount := SenderField.HeaderCount - 1;
              SenderField.RHeader.LastModifyTime := umlDefaultTime;
              if dbField_WriteRec(SenderField.RHeader.CurrentHeader, RecFile, SenderField) = False then
                begin
                  Result := False;
                  Exit;
                end;
              SenderField.Return := db_Field_ok;
              Result := True;
            end;
          else
            begin
              SenderField.Return := db_Field_DeleteHeaderError;
              Result := False;
            end;
        end;
        Exit;
      end;
    3:
      begin
        if dbHeader_ReadRec(HeaderPOS, RecFile, DeleteHeader) = False then
          begin
            SenderField.Return := DeleteHeader.Return;
            Result := False;
            Exit;
          end;
        case DeleteHeader.PositionID of
          db_Header_FirstPositionFlags:
            begin
              if dbHeader_ReadRec(DeleteHeader.NextHeader, RecFile, SwapHeader) = False then
                begin
                  SenderField.Return := SwapHeader.Return;
                  Result := False;
                  Exit;
                end;
              SwapHeader.PrevHeader := DeleteHeader.PrevHeader;
              SwapHeader.PositionID := db_Header_FirstPositionFlags;
              if dbHeader_WriteRec(SwapHeader.CurrentHeader, RecFile, SwapHeader) = False then
                begin
                  SenderField.Return := SwapHeader.Return;
                  Result := False;
                  Exit;
                end;
              SenderField.FirstHeaderPOS := SwapHeader.CurrentHeader;
              if dbHeader_ReadRec(DeleteHeader.PrevHeader, RecFile, SwapHeader) = False then
                begin
                  SenderField.Return := SwapHeader.Return;
                  Result := False;
                  Exit;
                end;
              SwapHeader.NextHeader := DeleteHeader.NextHeader;
              if dbHeader_WriteRec(SwapHeader.CurrentHeader, RecFile, SwapHeader) = False then
                begin
                  SenderField.Return := SwapHeader.Return;
                  Result := False;
                  Exit;
                end;
              SenderField.HeaderCount := SenderField.HeaderCount - 1;
              SenderField.RHeader.LastModifyTime := umlDefaultTime;
              if dbField_WriteRec(SenderField.RHeader.CurrentHeader, RecFile, SenderField) = False then
                begin
                  Result := False;
                  Exit;
                end;
              SenderField.Return := db_Field_ok;
              Result := True;
            end;
          db_Header_MediumPositionFlags:
            begin
              if dbHeader_ReadRec(DeleteHeader.PrevHeader, RecFile, SwapHeader) = False then
                begin
                  SenderField.Return := SwapHeader.Return;
                  Result := False;
                  Exit;
                end;
              SwapHeader.NextHeader := DeleteHeader.NextHeader;
              if dbHeader_WriteRec(SwapHeader.CurrentHeader, RecFile, SwapHeader) = False then
                begin
                  SenderField.Return := SwapHeader.Return;
                  Result := False;
                  Exit;
                end;
              if dbHeader_ReadRec(DeleteHeader.NextHeader, RecFile, SwapHeader) = False then
                begin
                  SenderField.Return := SwapHeader.Return;
                  Result := False;
                  Exit;
                end;
              SwapHeader.PrevHeader := DeleteHeader.PrevHeader;
              if dbHeader_WriteRec(SwapHeader.CurrentHeader, RecFile, SwapHeader) = False then
                begin
                  SenderField.Return := SwapHeader.Return;
                  Result := False;
                  Exit;
                end;
              SenderField.HeaderCount := SenderField.HeaderCount - 1;
              SenderField.RHeader.LastModifyTime := umlDefaultTime;
              if dbField_WriteRec(SenderField.RHeader.CurrentHeader, RecFile, SenderField) = False then
                begin
                  Result := False;
                  Exit;
                end;
              SenderField.Return := db_Field_ok;
              Result := True;
            end;
          db_Header_LastPositionFlags:
            begin
              if dbHeader_ReadRec(DeleteHeader.PrevHeader, RecFile, SwapHeader) = False then
                begin
                  SenderField.Return := SwapHeader.Return;
                  Result := False;
                  Exit;
                end;
              SwapHeader.NextHeader := DeleteHeader.NextHeader;
              SwapHeader.PositionID := db_Header_LastPositionFlags;
              if dbHeader_WriteRec(SwapHeader.CurrentHeader, RecFile, SwapHeader) = False then
                begin
                  SenderField.Return := SwapHeader.Return;
                  Result := False;
                  Exit;
                end;
              SenderField.LastHeaderPOS := SwapHeader.CurrentHeader;
              if dbHeader_ReadRec(DeleteHeader.NextHeader, RecFile, SwapHeader) = False then
                begin
                  SenderField.Return := SwapHeader.Return;
                  Result := False;
                  Exit;
                end;
              SwapHeader.PrevHeader := DeleteHeader.PrevHeader;
              if dbHeader_WriteRec(SwapHeader.CurrentHeader, RecFile, SwapHeader) = False then
                begin
                  SenderField.Return := SwapHeader.Return;
                  Result := False;
                  Exit;
                end;
              SenderField.HeaderCount := SenderField.HeaderCount - 1;
              SenderField.RHeader.LastModifyTime := umlDefaultTime;
              if dbField_WriteRec(SenderField.RHeader.CurrentHeader, RecFile, SenderField) = False then
                begin
                  Result := False;
                  Exit;
                end;
              SenderField.Return := db_Field_ok;
              Result := True;
            end;
          else
            begin
              SenderField.Return := db_Field_DeleteHeaderError;
              Result := False;
            end;
        end;
        Exit;
      end;
    else
      begin
        if dbHeader_ReadRec(HeaderPOS, RecFile, DeleteHeader) = False then
          begin
            SenderField.Return := DeleteHeader.Return;
            Result := False;
            Exit;
          end;
        case DeleteHeader.PositionID of
          db_Header_FirstPositionFlags:
            begin
              if dbHeader_ReadRec(DeleteHeader.NextHeader, RecFile, SwapHeader) = False then
                begin
                  SenderField.Return := SwapHeader.Return;
                  Result := False;
                  Exit;
                end;
              SwapHeader.PrevHeader := DeleteHeader.PrevHeader;
              SwapHeader.PositionID := db_Header_FirstPositionFlags;
              if dbHeader_WriteRec(SwapHeader.CurrentHeader, RecFile, SwapHeader) = False then
                begin
                  SenderField.Return := SwapHeader.Return;
                  Result := False;
                  Exit;
                end;
              SenderField.FirstHeaderPOS := SwapHeader.CurrentHeader;
              if dbHeader_ReadRec(DeleteHeader.PrevHeader, RecFile, SwapHeader) = False then
                begin
                  SenderField.Return := SwapHeader.Return;
                  Result := False;
                  Exit;
                end;
              SwapHeader.NextHeader := DeleteHeader.NextHeader;
              if dbHeader_WriteRec(SwapHeader.CurrentHeader, RecFile, SwapHeader) = False then
                begin
                  SenderField.Return := SwapHeader.Return;
                  Result := False;
                  Exit;
                end;
              SenderField.HeaderCount := SenderField.HeaderCount - 1;
              SenderField.RHeader.LastModifyTime := umlDefaultTime;
              if dbField_WriteRec(SenderField.RHeader.CurrentHeader, RecFile, SenderField) = False then
                begin
                  Result := False;
                  Exit;
                end;
              SenderField.Return := db_Field_ok;
              Result := True;
            end;
          db_Header_MediumPositionFlags:
            begin
              if dbHeader_ReadRec(DeleteHeader.PrevHeader, RecFile, SwapHeader) = False then
                begin
                  SenderField.Return := SwapHeader.Return;
                  Result := False;
                  Exit;
                end;
              SwapHeader.NextHeader := DeleteHeader.NextHeader;
              if dbHeader_WriteRec(SwapHeader.CurrentHeader, RecFile, SwapHeader) = False then
                begin
                  SenderField.Return := SwapHeader.Return;
                  Result := False;
                  Exit;
                end;
              if dbHeader_ReadRec(DeleteHeader.NextHeader, RecFile, SwapHeader) = False then
                begin
                  SenderField.Return := SwapHeader.Return;
                  Result := False;
                  Exit;
                end;
              SwapHeader.PrevHeader := DeleteHeader.PrevHeader;
              if dbHeader_WriteRec(SwapHeader.CurrentHeader, RecFile, SwapHeader) = False then
                begin
                  SenderField.Return := SwapHeader.Return;
                  Result := False;
                  Exit;
                end;
              SenderField.HeaderCount := SenderField.HeaderCount - 1;
              SenderField.RHeader.LastModifyTime := umlDefaultTime;
              if dbField_WriteRec(SenderField.RHeader.CurrentHeader, RecFile, SenderField) = False then
                begin
                  Result := False;
                  Exit;
                end;
              SenderField.Return := db_Field_ok;
              Result := True;
            end;
          db_Header_LastPositionFlags:
            begin
              if dbHeader_ReadRec(DeleteHeader.PrevHeader, RecFile, SwapHeader) = False then
                begin
                  SenderField.Return := SwapHeader.Return;
                  Result := False;
                  Exit;
                end;
              SwapHeader.NextHeader := DeleteHeader.NextHeader;
              SwapHeader.PositionID := db_Header_LastPositionFlags;
              if dbHeader_WriteRec(SwapHeader.CurrentHeader, RecFile, SwapHeader) = False then
                begin
                  SenderField.Return := SwapHeader.Return;
                  Result := False;
                  Exit;
                end;
              SenderField.LastHeaderPOS := SwapHeader.CurrentHeader;
              if dbHeader_ReadRec(DeleteHeader.NextHeader, RecFile, SwapHeader) = False then
                begin
                  SenderField.Return := SwapHeader.Return;
                  Result := False;
                  Exit;
                end;
              SwapHeader.PrevHeader := DeleteHeader.PrevHeader;
              if dbHeader_WriteRec(SwapHeader.CurrentHeader, RecFile, SwapHeader) = False then
                begin
                  SenderField.Return := SwapHeader.Return;
                  Result := False;
                  Exit;
                end;
              SenderField.HeaderCount := SenderField.HeaderCount - 1;
              SenderField.RHeader.LastModifyTime := umlDefaultTime;
              if dbField_WriteRec(SenderField.RHeader.CurrentHeader, RecFile, SenderField) = False then
                begin
                  Result := False;
                  Exit;
                end;
              SenderField.Return := db_Field_ok;
              Result := True;
            end;
          else
            begin
              SenderField.Return := db_Field_DeleteHeaderError;
              Result := False;
            end;
        end;
        Exit;
      end;
  end;
  Result := True;
end;

function dbField_MoveHeader(const HeaderPOS: Int64; const SourcerFieldPOS, TargetFieldPos: Int64; var RecFile: TRecFile; var SenderField: TField): Boolean;
var
  ActiveHeader, SwapHeader: THeader;
begin
  if dbHeader_ReadRec(HeaderPOS, RecFile, ActiveHeader) = False then
    begin
      SenderField.Return := ActiveHeader.Return;
      Result := False;
      Exit;
    end;
  if dbField_DeleteHeader(ActiveHeader.CurrentHeader, SourcerFieldPOS, RecFile, SenderField) = False then
    begin
      Result := False;
      Exit;
    end;

  if dbField_ReadRec(TargetFieldPos, RecFile, SenderField) = False then
    begin
      Result := False;
      Exit;
    end;
  case SenderField.HeaderCount of
    0:
      begin
        SenderField.HeaderCount := 1;
        SenderField.FirstHeaderPOS := ActiveHeader.CurrentHeader;
        SenderField.LastHeaderPOS := SenderField.FirstHeaderPOS;
        ActiveHeader.PositionID := db_Header_OnlyPositionFlags;
        ActiveHeader.NextHeader := SenderField.FirstHeaderPOS;
        ActiveHeader.PrevHeader := SenderField.FirstHeaderPOS;
        if dbField_WriteRec(SenderField.RHeader.CurrentHeader, RecFile, SenderField) = False then
          begin
            Result := False;
            Exit;
          end;
        if dbHeader_WriteRec(ActiveHeader.CurrentHeader, RecFile, ActiveHeader) = False then
          begin
            SenderField.Return := ActiveHeader.Return;
            Result := False;
            Exit;
          end;
      end;
    1:
      begin

        if dbHeader_ReadRec(SenderField.FirstHeaderPOS, RecFile, SwapHeader) = False then
          begin
            SenderField.Return := SwapHeader.Return;
            Result := False;
            Exit;
          end;
        SwapHeader.PrevHeader := ActiveHeader.CurrentHeader;
        SwapHeader.NextHeader := ActiveHeader.CurrentHeader;
        SwapHeader.PositionID := db_Header_FirstPositionFlags;
        ActiveHeader.NextHeader := SwapHeader.CurrentHeader;
        ActiveHeader.PrevHeader := SwapHeader.CurrentHeader;
        ActiveHeader.PositionID := db_Header_LastPositionFlags;

        if dbHeader_WriteRec(SwapHeader.CurrentHeader, RecFile, SwapHeader) = False then
          begin
            SenderField.Return := SwapHeader.Return;
            Result := False;
            Exit;
          end;
        SenderField.HeaderCount := SenderField.HeaderCount + 1;
        SenderField.LastHeaderPOS := ActiveHeader.CurrentHeader;
        SenderField.RHeader.LastModifyTime := umlDefaultTime;
        if dbField_WriteRec(SenderField.RHeader.CurrentHeader, RecFile, SenderField) = False then
          begin
            Result := False;
            Exit;
          end;
        if dbHeader_WriteRec(ActiveHeader.CurrentHeader, RecFile, ActiveHeader) = False then
          begin
            SenderField.Return := ActiveHeader.Return;
            Result := False;
            Exit;
          end;
      end;
    else
      begin

        if dbHeader_ReadRec(SenderField.FirstHeaderPOS, RecFile, SwapHeader) = False then
          begin
            SenderField.Return := SwapHeader.Return;
            Result := False;
            Exit;
          end;
        SwapHeader.PrevHeader := ActiveHeader.CurrentHeader;
        SwapHeader.PositionID := db_Header_FirstPositionFlags;
        ActiveHeader.NextHeader := SwapHeader.CurrentHeader;
        if dbHeader_WriteRec(SwapHeader.CurrentHeader, RecFile, SwapHeader) = False then
          begin
            SenderField.Return := SwapHeader.Return;
            Result := False;
            Exit;
          end;

        if dbHeader_ReadRec(SenderField.LastHeaderPOS, RecFile, SwapHeader) = False then
          begin
            SenderField.Return := SwapHeader.Return;
            Result := False;
            Exit;
          end;
        SwapHeader.NextHeader := ActiveHeader.CurrentHeader;
        ActiveHeader.PrevHeader := SwapHeader.CurrentHeader;
        SwapHeader.PositionID := db_Header_MediumPositionFlags;
        if dbHeader_WriteRec(SwapHeader.CurrentHeader, RecFile, SwapHeader) = False then
          begin
            SenderField.Return := SwapHeader.Return;
            Result := False;
            Exit;
          end;
        SenderField.HeaderCount := SenderField.HeaderCount + 1;
        SenderField.LastHeaderPOS := ActiveHeader.CurrentHeader;
        SenderField.RHeader.LastModifyTime := umlDefaultTime;
        ActiveHeader.PositionID := db_Header_LastPositionFlags;
        if dbField_WriteRec(SenderField.RHeader.CurrentHeader, RecFile, SenderField) = False then
          begin
            Result := False;
            Exit;
          end;
        if dbHeader_WriteRec(ActiveHeader.CurrentHeader, RecFile, ActiveHeader) = False then
          begin
            SenderField.Return := ActiveHeader.Return;
            Result := False;
            Exit;
          end;
      end;
  end;
  SenderField.Return := db_Field_ok;
  Result := True;
end;

function dbField_CreateField(const Name: string; const POS: Int64; var RecFile: TRecFile; var SenderField: TField): Boolean;
var
  _Field: TField;
begin
  if dbField_CreateHeader(name, db_Header_FieldID, POS, RecFile, SenderField.RHeader) = False then
    begin
      SenderField.Return := SenderField.RHeader.Return;
      Result := False;
      Exit;
    end;
  if dbField_ReadRec(POS, RecFile, _Field) = False then
    begin
      SenderField.Return := _Field.Return;
      Result := False;
      Exit;
    end;
  _Field.RHeader.LastModifyTime := umlDefaultTime;
  if dbField_WriteRec(POS, RecFile, _Field) = False then
    begin
      SenderField.Return := _Field.Return;
      Result := False;
      Exit;
    end;

  SenderField.HeaderCount := 0;
  SenderField.HigherFieldPOS := _Field.RHeader.CurrentHeader;
  if dbField_OnlyWriteFieldRec(SenderField.RHeader.DataMainPOS, RecFile, SenderField) = False then
    begin
      Result := False;
      Exit;
    end;
  SenderField.Return := db_Field_ok;
  Result := True;
end;

function dbField_CreateItem(const Name: string; const ExterID: Int64; const POS: Int64; var RecFile: TRecFile; var SenderItem: TItem): Boolean;
begin
  if dbField_CreateHeader(name, db_Header_ItemID, POS, RecFile, SenderItem.RHeader) = False then
    begin
      SenderItem.Return := SenderItem.RHeader.Return;
      Result := False;
      Exit;
    end;

  SenderItem.ExtID := ExterID;
  SenderItem.FirstBlockPOS := 0;
  SenderItem.LastBlockPOS := 0;
  SenderItem.Size := 0;
  SenderItem.BlockCount := 0;
  SenderItem.RHeader.LastModifyTime := umlDefaultTime;
  if dbItem_OnlyWriteItemRec(SenderItem.RHeader.DataMainPOS, RecFile, SenderItem) = False then
    begin
      Result := False;
      Exit;
    end;
  SenderItem.DataModify := True;
  SenderItem.Return := db_Item_ok;
  Result := True;
end;

function dbField_CopyItem(var SenderItem: TItem; var RecFile: TRecFile; const DestFieldPos: Int64; var DestRecFile: TRecFile): Boolean;
var
  Rep_Int   : Integer;
  NewItemHnd: TItem;
  Buff      : array [0 .. umlMaxFileRecSize] of umlChar;
begin
  InitTItem(NewItemHnd);
  NewItemHnd := SenderItem;
  if dbField_CreateItem(SenderItem.RHeader.Name, SenderItem.ExtID, DestFieldPos, DestRecFile, NewItemHnd) = False then
    begin
      SenderItem.Return := NewItemHnd.Return;
      Result := False;
      Exit;
    end;
  if dbItem_BlockSeekStartPOS(RecFile, SenderItem) = False then
    begin
      Result := False;
      Exit;
    end;
  if SenderItem.Size > umlMaxFileRecSize then
    begin
      for Rep_Int := 1 to (SenderItem.Size div umlMaxFileRecSize) do
        begin
          if dbItem_BlockReadData(RecFile, SenderItem, Buff, umlMaxFileRecSize) = False then
            begin
              Result := False;
              Exit;
            end;
          if dbItem_BlockAppendWriteData(DestRecFile, NewItemHnd, Buff, umlMaxFileRecSize) = False then
            begin
              SenderItem.Return := NewItemHnd.Return;
              Result := False;
              Exit;
            end;
        end;
      if (SenderItem.Size mod umlMaxFileRecSize) > 0 then
        begin
          if dbItem_BlockReadData(RecFile, SenderItem, Buff, SenderItem.Size mod umlMaxFileRecSize) = False then
            begin
              Result := False;
              Exit;
            end;
          if dbItem_BlockAppendWriteData(DestRecFile, NewItemHnd, Buff, SenderItem.Size mod umlMaxFileRecSize) = False then
            begin
              SenderItem.Return := NewItemHnd.Return;
              Result := False;
              Exit;
            end;
        end;
    end
  else
    begin
      if dbItem_BlockReadData(RecFile, SenderItem, Buff, SenderItem.Size) = False then
        begin
          Result := False;
          Exit;
        end;
      if dbItem_BlockAppendWriteData(DestRecFile, NewItemHnd, Buff, SenderItem.Size) = False then
        begin
          SenderItem.Return := NewItemHnd.Return;
          Result := False;
          Exit;
        end;
    end;
  if dbItem_WriteRec(NewItemHnd.RHeader.CurrentHeader, DestRecFile, NewItemHnd) = False then
    begin
      SenderItem.Return := NewItemHnd.Return;
      Result := False;
      Exit;
    end;
  SenderItem.Return := db_Item_ok;
  Result := True;
end;

function dbField_CopyItemBuffer(var SenderItem: TItem; var RecFile: TRecFile; var DestItemHnd: TItem; var DestRecFile: TRecFile): Boolean;
var
  Rep_Int: Integer;
  Buff   : array [0 .. umlMaxFileRecSize] of umlChar;
begin
  if dbItem_BlockSeekStartPOS(RecFile, SenderItem) = False then
    begin
      Result := False;
      Exit;
    end;
  if SenderItem.Size > umlMaxFileRecSize then
    begin
      for Rep_Int := 1 to (SenderItem.Size div umlMaxFileRecSize) do
        begin
          if dbItem_BlockReadData(RecFile, SenderItem, Buff, umlMaxFileRecSize) = False then
            begin
              Result := False;
              Exit;
            end;
          if dbItem_BlockAppendWriteData(DestRecFile, DestItemHnd, Buff, umlMaxFileRecSize) = False then
            begin
              SenderItem.Return := DestItemHnd.Return;
              Result := False;
              Exit;
            end;
        end;
      if (SenderItem.Size mod umlMaxFileRecSize) > 0 then
        begin
          if dbItem_BlockReadData(RecFile, SenderItem, Buff, SenderItem.Size mod umlMaxFileRecSize) = False then
            begin
              Result := False;
              Exit;
            end;
          if dbItem_BlockAppendWriteData(DestRecFile, DestItemHnd, Buff, SenderItem.Size mod umlMaxFileRecSize) = False then
            begin
              SenderItem.Return := DestItemHnd.Return;
              Result := False;
              Exit;
            end;
        end;
    end
  else
    begin
      if dbItem_BlockReadData(RecFile, SenderItem, Buff, SenderItem.Size) = False then
        begin
          Result := False;
          Exit;
        end;
      if dbItem_BlockAppendWriteData(DestRecFile, DestItemHnd, Buff, SenderItem.Size) = False then
        begin
          SenderItem.Return := DestItemHnd.Return;
          Result := False;
          Exit;
        end;
    end;
  if dbItem_WriteRec(DestItemHnd.RHeader.CurrentHeader, DestRecFile, DestItemHnd) = False then
    begin
      SenderItem.Return := DestItemHnd.Return;
      Result := False;
      Exit;
    end;
  SenderItem.Return := db_Item_ok;
  Result := True;
end;

function dbField_CopyAllTo(const FilterName: string; const FieldPos: Int64; var RecFile: TRecFile; const DestFieldPos: Int64; var DestRecFile: TRecFile): Boolean;
var
  _FieldSearch: TFieldSearch;
  NewField    : TField;
  NewItem     : TItem;
begin
  InitTFieldSearch(_FieldSearch);
  if dbField_OnlyFindFirstName(FilterName, FieldPos, RecFile, _FieldSearch) then
    begin
      repeat
        case _FieldSearch.ID of
          db_Header_FieldID:
            begin
              InitTField(NewField);
              if dbField_ReadRec(_FieldSearch.RHeader.CurrentHeader, RecFile, NewField) then
                if dbField_CreateField(_FieldSearch.RHeader.Name, DestFieldPos, DestRecFile, NewField) then
                    dbField_CopyAllTo(FilterName, _FieldSearch.RHeader.CurrentHeader, RecFile, NewField.RHeader.CurrentHeader, DestRecFile);
            end;
          db_Header_ItemID:
            begin
              if dbItem_ReadRec(_FieldSearch.RHeader.CurrentHeader, RecFile, NewItem) then
                begin
                  dbField_CopyItem(NewItem, RecFile, DestFieldPos, DestRecFile);
                end;
            end;
        end;
      until not dbField_OnlyFindNextName(RecFile, _FieldSearch);
    end;
  Result := True;
end;

function dbPack_WriteRec(const POS: Int64; var RecFile: TRecFile; var SenderTMDB: TTMDB): Boolean;
begin
  if umlFileSeek(RecFile, POS) = False then
    begin
      SenderTMDB.Return := db_Pack_PositionSeekError;
      Result := False;
      Exit;
    end;
  SenderTMDB.FileDescription := db_Pack_FileDescription;
  if umlFileWriteStr(RecFile, SenderTMDB.FileDescription) = False then
    begin
      SenderTMDB.Return := db_Pack_WriteFileDescriptionNameError;
      Result := False;
      Exit;
    end;
  if umlFileWriteStr(RecFile, SenderTMDB.Name) = False then
    begin
      SenderTMDB.Return := db_Pack_WriteNameError;
      Result := False;
      Exit;
    end;
  if umlFileWriteStr(RecFile, SenderTMDB.Description) = False then
    begin
      SenderTMDB.Return := db_Pack_WriteDescriptionError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(RecFile, umlVersionLength, SenderTMDB.MajorVer) = False then
    begin
      SenderTMDB.Return := db_Pack_WriteMajorVersionError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(RecFile, umlVersionLength, SenderTMDB.MinorVer) = False then
    begin
      SenderTMDB.Return := db_Pack_WriteMinorVersionError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(RecFile, umlTimeLength, SenderTMDB.CreateTime) = False then
    begin
      SenderTMDB.Return := db_Pack_WriteCreateTimeError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(RecFile, umlTimeLength, SenderTMDB.LastModifyTime) = False then
    begin
      SenderTMDB.Return := db_Pack_WriteLastEditTimeError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(RecFile, umlCountLength, SenderTMDB.RootHeaderCount) = False then
    begin
      SenderTMDB.Return := db_Pack_WriteHeaderCountError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(RecFile, umlPositionLength, SenderTMDB.DefaultFieldPOS) = False then
    begin
      SenderTMDB.Return := db_Pack_WriteDefaultPositionError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(RecFile, umlPositionLength, SenderTMDB.FirstHeaderPOS) = False then
    begin
      SenderTMDB.Return := db_Pack_WriteFirstPositionError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(RecFile, umlPositionLength, SenderTMDB.LastHeaderPOS) = False then
    begin
      SenderTMDB.Return := db_Pack_WriteLastPositionError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(RecFile, umlPositionLength, SenderTMDB.CurrentFieldPOS) = False then
    begin
      SenderTMDB.Return := db_Pack_WriteCurrentPositionError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(RecFile, umlLevelLength, SenderTMDB.CurrentFieldLevel) = False then
    begin
      SenderTMDB.Return := db_Pack_WriteCurrentLevelError;
      Result := False;
      Exit;
    end;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_ReadRec(const POS: Int64; var RecFile: TRecFile; var SenderTMDB: TTMDB): Boolean;
begin
  if umlFileSeek(RecFile, POS) = False then
    begin
      SenderTMDB.Return := db_Pack_PositionSeekError;
      Result := False;
      Exit;
    end;
  if umlFileReadStr(RecFile, SenderTMDB.FileDescription) = False then
    begin
      SenderTMDB.Return := db_Pack_ReadFileDescriptionNameError;
      Result := False;
      Exit;
    end;
  if SenderTMDB.FileDescription <> db_Pack_FileDescription then
    begin
      SenderTMDB.Return := db_Pack_ReadFileDescriptionNameError;
      Result := False;
      Exit;
    end;
  if umlFileReadStr(RecFile, SenderTMDB.Name) = False then
    begin
      SenderTMDB.Return := db_Pack_ReadNameError;
      Result := False;
      Exit;
    end;
  if umlFileReadStr(RecFile, SenderTMDB.Description) = False then
    begin
      SenderTMDB.Return := db_Pack_ReadDescriptionError;
      Result := False;
      Exit;
    end;
  if umlFileRead(RecFile, umlVersionLength, SenderTMDB.MajorVer) = False then
    begin
      SenderTMDB.Return := db_Pack_ReadMajorVersionError;
      Result := False;
      Exit;
    end;
  if umlFileRead(RecFile, umlVersionLength, SenderTMDB.MinorVer) = False then
    begin
      SenderTMDB.Return := db_Pack_ReadMinorVersionError;
      Result := False;
      Exit;
    end;
  if umlFileRead(RecFile, umlTimeLength, SenderTMDB.CreateTime) = False then
    begin
      SenderTMDB.Return := db_Pack_ReadCreateTimeError;
      Result := False;
      Exit;
    end;
  if umlFileRead(RecFile, umlTimeLength, SenderTMDB.LastModifyTime) = False then
    begin
      SenderTMDB.Return := db_Pack_ReadLastEditTimeError;
      Result := False;
      Exit;
    end;
  if umlFileRead(RecFile, umlCountLength, SenderTMDB.RootHeaderCount) = False then
    begin
      SenderTMDB.Return := db_Pack_ReadHeaderCountError;
      Result := False;
      Exit;
    end;
  if umlFileRead(RecFile, umlPositionLength, SenderTMDB.DefaultFieldPOS) = False then
    begin
      SenderTMDB.Return := db_Pack_ReadDefaultPositionError;
      Result := False;
      Exit;
    end;
  if umlFileRead(RecFile, umlPositionLength, SenderTMDB.FirstHeaderPOS) = False then
    begin
      SenderTMDB.Return := db_Pack_ReadFirstPositionError;
      Result := False;
      Exit;
    end;
  if umlFileRead(RecFile, umlPositionLength, SenderTMDB.LastHeaderPOS) = False then
    begin
      SenderTMDB.Return := db_Pack_ReadLastPositionError;
      Result := False;
      Exit;
    end;
  if umlFileRead(RecFile, umlPositionLength, SenderTMDB.CurrentFieldPOS) = False then
    begin
      SenderTMDB.Return := db_Pack_ReadCurrentPositionError;
      Result := False;
      Exit;
    end;
  if umlFileRead(RecFile, umlLevelLength, SenderTMDB.CurrentFieldLevel) = False then
    begin
      SenderTMDB.Return := db_Pack_ReadCurrentLevelError;
      Result := False;
      Exit;
    end;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_CreatePack(const Name, Description: string; var SenderTMDB: TTMDB): Boolean;
begin
  if umlFileTest(SenderTMDB.RecFile) = True then
    begin
      SenderTMDB.Return := db_Pack_RepCreatePackError;
      Result := False;
      Exit;
    end;
  if umlFileCreate(name, SenderTMDB.RecFile) = False then
    begin
      SenderTMDB.Return := db_Pack_CreatePackError;
      Result := False;
      Exit;
    end;
  SenderTMDB.Name := umlGetLastStr(name, db_FieldPathLimitChar).Text;
  SenderTMDB.Description := Description;
  SenderTMDB.MajorVer := db_Pack_MajorVersion;
  SenderTMDB.MinorVer := db_Pack_MinorVersion;
  SenderTMDB.CreateTime := umlDefaultTime;
  SenderTMDB.LastModifyTime := SenderTMDB.CreateTime;
  SenderTMDB.RootHeaderCount := 0;
  SenderTMDB.DefaultFieldPOS := db_Pack_RecLength;
  SenderTMDB.LastHeaderPOS := SenderTMDB.DefaultFieldPOS;
  SenderTMDB.FirstHeaderPOS := SenderTMDB.DefaultFieldPOS;
  SenderTMDB.CurrentFieldPOS := SenderTMDB.DefaultFieldPOS;
  SenderTMDB.WriteFlags := True;
  if dbPack_WriteRec(0, SenderTMDB.RecFile, SenderTMDB) = False then
    begin
      Result := False;
      Exit;
    end;
  if dbPack_CreateAndSetRootField(db_Pack_DefaultDescription, db_Pack_DefaultDescription, SenderTMDB) = False then
    begin
      Result := False;
      Exit;
    end;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_OpenPack(const Name: string; var SenderTMDB: TTMDB; _OnlyRead: Boolean): Boolean;
begin
  if umlFileTest(SenderTMDB.RecFile) = True then
    begin
      SenderTMDB.Return := db_Pack_RepOpenPackError;
      Result := False;
      Exit;
    end;

  if umlFileOpen(name, SenderTMDB.RecFile, _OnlyRead) = False then
    begin
      SenderTMDB.Return := db_Pack_OpenPackError;
      Result := False;
      Exit;
    end;

  if dbPack_ReadRec(0, SenderTMDB.RecFile, SenderTMDB) = False then
    begin
      Result := False;
      Exit;
    end;

  if SenderTMDB.Name <> name then
    begin
      SenderTMDB.Name := name;
      SenderTMDB.WriteFlags := not _OnlyRead;
    end
  else
      SenderTMDB.WriteFlags := False;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_CreateAsStream(Stream: TMixedStream; const Name, Description: string; var SenderTMDB: TTMDB): Boolean;
begin
  if umlFileTest(SenderTMDB.RecFile) = True then
    begin
      SenderTMDB.Return := db_Pack_RepCreatePackError;
      Result := False;
      Exit;
    end;
  if umlFileCreateAsStream(name, Stream, SenderTMDB.RecFile) = False then
    begin
      SenderTMDB.Return := db_Pack_CreatePackError;
      Result := False;
      Exit;
    end;
  SenderTMDB.Name := name;
  SenderTMDB.Description := Description;
  SenderTMDB.MajorVer := db_Pack_MajorVersion;
  SenderTMDB.MinorVer := db_Pack_MinorVersion;
  SenderTMDB.CreateTime := umlDefaultTime;
  SenderTMDB.LastModifyTime := SenderTMDB.CreateTime;
  SenderTMDB.RootHeaderCount := 0;
  SenderTMDB.DefaultFieldPOS := db_Pack_RecLength;
  SenderTMDB.LastHeaderPOS := SenderTMDB.DefaultFieldPOS;
  SenderTMDB.FirstHeaderPOS := SenderTMDB.DefaultFieldPOS;
  SenderTMDB.CurrentFieldPOS := SenderTMDB.DefaultFieldPOS;
  SenderTMDB.WriteFlags := True;
  if dbPack_WriteRec(0, SenderTMDB.RecFile, SenderTMDB) = False then
    begin
      Result := False;
      Exit;
    end;
  if dbPack_CreateAndSetRootField(db_Pack_DefaultDescription, db_Pack_DefaultDescription, SenderTMDB) = False then
    begin
      Result := False;
      Exit;
    end;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_OpenAsStream(Stream: TMixedStream; const Name: string; var SenderTMDB: TTMDB; _OnlyRead: Boolean): Boolean;
begin
  if umlFileTest(SenderTMDB.RecFile) = True then
    begin
      SenderTMDB.Return := db_Pack_RepOpenPackError;
      Result := False;
      Exit;
    end;

  if umlFileOpenAsStream(name, Stream, SenderTMDB.RecFile, _OnlyRead) = False then
    begin
      SenderTMDB.Return := db_Pack_OpenPackError;
      Result := False;
      Exit;
    end;

  if dbPack_ReadRec(0, SenderTMDB.RecFile, SenderTMDB) = False then
    begin
      Result := False;
      Exit;
    end;

  if SenderTMDB.Name <> name then
    begin
      SenderTMDB.Name := name;
      SenderTMDB.WriteFlags := not _OnlyRead;
    end
  else
      SenderTMDB.WriteFlags := False;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_ClosePack(var SenderTMDB: TTMDB): Boolean;
begin
  if umlFileTest(SenderTMDB.RecFile) = False then
    begin
      SenderTMDB.Return := db_Pack_ClosePackError;
      Result := False;
      Exit;
    end;
  if SenderTMDB.WriteFlags = True then
    begin
      SenderTMDB.LastModifyTime := umlDefaultTime;
      if dbPack_WriteRec(0, SenderTMDB.RecFile, SenderTMDB) = False then
        begin
          Result := False;
          Exit;
        end;
      SenderTMDB.WriteFlags := False;
    end;
  if umlFileClose(SenderTMDB.RecFile) = False then
    begin
      SenderTMDB.Return := db_Pack_ClosePackError;
      Result := False;
      Exit;
    end;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_Update(var SenderTMDB: TTMDB): Boolean;
begin
  if umlFileTest(SenderTMDB.RecFile) = False then
    begin
      SenderTMDB.Return := db_Pack_ClosePackError;
      Result := False;
      Exit;
    end;
  if SenderTMDB.WriteFlags = True then
    begin
      SenderTMDB.LastModifyTime := umlDefaultTime;
      if dbPack_WriteRec(0, SenderTMDB.RecFile, SenderTMDB) = False then
        begin
          Result := False;
          Exit;
        end;
      SenderTMDB.WriteFlags := False;
    end;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_CopyFieldTo(const FilterName: string; var SenderTMDB: TTMDB; const SourceFieldPos: Int64; var DestTMDB: TTMDB; const DestFieldPos: Int64): Boolean;
begin
  if dbField_CopyAllTo(FilterName, SourceFieldPos, SenderTMDB.RecFile, DestFieldPos, DestTMDB.RecFile) then
    begin
      SenderTMDB.Return := db_Pack_ok;
      DestTMDB.Return := db_Pack_ok;
      Result := True;
    end
  else
    begin
      SenderTMDB.Return := db_Pack_CreatePackError;
      DestTMDB.Return := db_Pack_CreatePackError;
      Result := False;
    end;
end;

function dbPack_CopyAllTo(var SenderTMDB: TTMDB; var DestTMDB: TTMDB): Boolean;
begin
  Result := dbPack_CopyFieldTo('*', SenderTMDB, SenderTMDB.DefaultFieldPOS, DestTMDB, DestTMDB.DefaultFieldPOS);
end;

function dbPack_CopyAllToDestPath(var SenderTMDB: TTMDB; var DestTMDB: TTMDB; DestPath: string): Boolean;
var
  _Field: TField;
begin
  Result := False;
  dbPack_CreateField(DestPath, '', DestTMDB);
  if dbPack_GetField(DestPath, _Field, DestTMDB) then
    begin
      Result := dbPack_CopyFieldTo('*', SenderTMDB, SenderTMDB.DefaultFieldPOS, DestTMDB, _Field.RHeader.CurrentHeader);
    end;
end;

function dbPack_TestNameStr(const Name: string): Boolean;
begin
  Result := umlGetLength(umlDelLimitChar(name, db_FieldPathLimitChar + #9#32#13#10)) > 0;
end;

function dbPack_AutoCheckRootField(const Name: string; var SenderField: TField; var SenderTMDB: TTMDB): Boolean;
var
  _Field: TField;
begin
  if dbPack_TestNameStr(name) = False then
    begin
      SenderTMDB.Return := db_Pack_PathNameError;
      SenderField.Return := SenderTMDB.Return;
      Result := False;
      Exit;
    end;

  if dbPack_GetRootField(name, _Field, SenderTMDB) = False then
    begin
      if dbPack_CreateRootHeader(name, db_Header_FieldID, SenderTMDB, SenderField.RHeader) = False then
        begin
          SenderTMDB.Return := SenderField.RHeader.Return;
          SenderField.Return := SenderTMDB.Return;
          Result := False;
          Exit;
        end;
      SenderField.HeaderCount := 0;
      SenderField.HigherFieldPOS := -1;
      if dbField_OnlyWriteFieldRec(SenderField.RHeader.DataMainPOS, SenderTMDB.RecFile, SenderField) = False then
        begin
          SenderTMDB.Return := SenderField.Return;
          SenderField.Return := SenderTMDB.Return;
          Result := False;
          Exit;
        end;
    end
  else
    begin
      SenderField := _Field;
    end;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_CreateRootHeader(const Name: string; const ID: Int64; var SenderTMDB: TTMDB; var SenderHeader: THeader): Boolean;
var
  _Header: THeader;
begin
  SenderHeader.ID := ID;
  SenderHeader.Name := name;
  case SenderTMDB.RootHeaderCount of
    0:
      begin
        SenderTMDB.RootHeaderCount := 1;
        SenderTMDB.FirstHeaderPOS := umlFileGetSize(SenderTMDB.RecFile);
        SenderTMDB.LastHeaderPOS := SenderTMDB.FirstHeaderPOS;
        SenderTMDB.LastModifyTime := umlDefaultTime;
        SenderHeader.PositionID := db_Header_OnlyPositionFlags;
        SenderHeader.NextHeader := SenderTMDB.LastHeaderPOS;
        SenderHeader.PrevHeader := SenderTMDB.FirstHeaderPOS;
        SenderHeader.CurrentHeader := SenderTMDB.FirstHeaderPOS;
        SenderHeader.CreateTime := umlDefaultTime;
        SenderHeader.LastModifyTime := umlDefaultTime;
        SenderHeader.DataMainPOS := SenderHeader.CurrentHeader + db_Header_RecLength;
        if dbHeader_WriteRec(SenderHeader.CurrentHeader, SenderTMDB.RecFile, SenderHeader) = False then
          begin
            Result := False;
            Exit;
          end;
      end;
    1:
      begin
        SenderHeader.CurrentHeader := umlFileGetSize(SenderTMDB.RecFile);
        SenderHeader.NextHeader := SenderTMDB.FirstHeaderPOS;
        SenderHeader.PrevHeader := SenderTMDB.FirstHeaderPOS;
        if dbHeader_ReadRec(SenderTMDB.FirstHeaderPOS, SenderTMDB.RecFile, _Header) = False then
          begin
            SenderHeader.Return := _Header.Return;
            Result := False;
            Exit;
          end;
        _Header.PrevHeader := SenderHeader.CurrentHeader;
        _Header.NextHeader := SenderHeader.CurrentHeader;
        _Header.PositionID := db_Header_FirstPositionFlags;
        if dbHeader_WriteRec(SenderTMDB.FirstHeaderPOS, SenderTMDB.RecFile, _Header) = False then
          begin
            SenderHeader.Return := _Header.Return;
            Result := False;
            Exit;
          end;
        SenderTMDB.RootHeaderCount := SenderTMDB.RootHeaderCount + 1;
        SenderTMDB.LastHeaderPOS := SenderHeader.CurrentHeader;
        SenderTMDB.LastModifyTime := umlDefaultTime;
        SenderHeader.CreateTime := umlDefaultTime;
        SenderHeader.LastModifyTime := umlDefaultTime;
        SenderHeader.DataMainPOS := SenderHeader.CurrentHeader + db_Header_RecLength;
        SenderHeader.PositionID := db_Header_LastPositionFlags;
        if dbHeader_WriteRec(SenderHeader.CurrentHeader, SenderTMDB.RecFile, SenderHeader) = False then
          begin
            Result := False;
            Exit;
          end;
      end;
    else
      begin
        SenderHeader.CurrentHeader := umlFileGetSize(SenderTMDB.RecFile);
        if dbHeader_ReadRec(SenderTMDB.FirstHeaderPOS, SenderTMDB.RecFile, _Header) = False then
          begin
            SenderHeader.Return := _Header.Return;
            Result := False;
            Exit;
          end;
        _Header.PrevHeader := SenderHeader.CurrentHeader;
        SenderHeader.NextHeader := _Header.CurrentHeader;
        if dbHeader_WriteRec(SenderTMDB.FirstHeaderPOS, SenderTMDB.RecFile, _Header) = False then
          begin
            SenderHeader.Return := _Header.Return;
            Result := False;
            Exit;
          end;
        if dbHeader_ReadRec(SenderTMDB.LastHeaderPOS, SenderTMDB.RecFile, _Header) = False then
          begin
            SenderHeader.Return := _Header.Return;
            Result := False;
            Exit;
          end;
        _Header.NextHeader := SenderHeader.CurrentHeader;
        SenderHeader.PrevHeader := SenderTMDB.LastHeaderPOS;
        _Header.PositionID := db_Header_MediumPositionFlags;
        if dbHeader_WriteRec(SenderTMDB.LastHeaderPOS, SenderTMDB.RecFile, _Header) = False then
          begin
            SenderHeader.Return := _Header.Return;
            Result := False;
            Exit;
          end;
        SenderTMDB.RootHeaderCount := SenderTMDB.RootHeaderCount + 1;
        SenderTMDB.LastHeaderPOS := SenderHeader.CurrentHeader;
        SenderTMDB.LastModifyTime := umlDefaultTime;
        SenderHeader.CreateTime := umlDefaultTime;
        SenderHeader.LastModifyTime := umlDefaultTime;
        SenderHeader.DataMainPOS := SenderHeader.CurrentHeader + db_Header_RecLength;
        SenderHeader.PositionID := db_Header_LastPositionFlags;
        if dbHeader_WriteRec(SenderHeader.CurrentHeader, SenderTMDB.RecFile, SenderHeader) = False then
          begin
            Result := False;
            Exit;
          end;
      end;
  end;

  if SenderTMDB.WriteFlags = False then
      SenderTMDB.WriteFlags := True;
  SenderHeader.Return := db_Header_ok;
  Result := True;
end;

function dbPack_CreateRootField(const Name, Description: string; var SenderTMDB: TTMDB): Boolean;
var
  _Field: TField;
begin
  if dbPack_TestNameStr(name) = False then
    begin
      SenderTMDB.Return := db_Pack_PathNameError;
      Result := False;
      Exit;
    end;

  if dbPack_ExistsRootField(name, SenderTMDB) = True then
    begin
      SenderTMDB.Return := db_Pack_PathNameError;
      Result := False;
      Exit;
    end;
  if dbPack_CreateRootHeader(name, db_Header_FieldID, SenderTMDB, _Field.RHeader) = False then
    begin
      SenderTMDB.Return := _Field.RHeader.Return;
      Result := False;
      Exit;
    end;
  _Field.Description := Description;
  _Field.HeaderCount := 0;
  _Field.HigherFieldPOS := -1;
  if dbField_OnlyWriteFieldRec(_Field.RHeader.DataMainPOS, SenderTMDB.RecFile, _Field) = False then
    begin
      SenderTMDB.Return := _Field.Return;
      Result := False;
      Exit;
    end;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_CreateAndSetRootField(const Name, Description: string; var SenderTMDB: TTMDB): Boolean;
var
  _Field: TField;
begin
  if dbPack_TestNameStr(name) = False then
    begin
      SenderTMDB.Return := db_Pack_PathNameError;
      Result := False;
      Exit;
    end;

  if dbPack_ExistsRootField(name, SenderTMDB) = True then
    begin
      SenderTMDB.Return := db_Pack_PathNameError;
      Result := False;
      Exit;
    end;
  if dbPack_CreateRootHeader(name, db_Header_FieldID, SenderTMDB, _Field.RHeader) = False then
    begin
      SenderTMDB.Return := _Field.RHeader.Return;
      Result := False;
      Exit;
    end;
  _Field.Description := Description;
  _Field.HeaderCount := 0;
  _Field.HigherFieldPOS := -1;
  if dbField_OnlyWriteFieldRec(_Field.RHeader.DataMainPOS, SenderTMDB.RecFile, _Field) = False then
    begin
      SenderTMDB.Return := _Field.Return;
      Result := False;
      Exit;
    end;
  SenderTMDB.DefaultFieldPOS := _Field.RHeader.CurrentHeader;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_CreateField(const PathName, Description: string; var SenderTMDB: TTMDB): Boolean;
var
  _Field                   : TField;
  _FieldSearch             : TFieldSearch;
  Rep_Int, _PathCount      : Integer;
  TempPathStr, TempPathName: string;
begin
  if umlFileTest(SenderTMDB.RecFile) = False then
    begin
      SenderTMDB.Return := db_Pack_ClosePackError;
      Result := False;
      Exit;
    end;
  if dbField_ReadRec(SenderTMDB.DefaultFieldPOS, SenderTMDB.RecFile, _Field) = False then
    begin
      SenderTMDB.Return := _Field.Return;
      Result := False;
      Exit;
    end;

  if umlGetLength(PathName) = 0 then
    begin
      SenderTMDB.Return := db_Pack_ok;
      Result := True;
      Exit;
    end;
  TempPathName := PathName;
  _PathCount := dbPack_GetIndexStrCount(TempPathName);
  if _PathCount > 0 then
    begin
      for Rep_Int := 1 to _PathCount do
        begin
          TempPathStr := dbPack_GetFirstPath(TempPathName);
          TempPathName := dbPack_MaskFirstPath(TempPathName);

          if dbPack_TestNameStr(TempPathStr) = False then
            begin
              SenderTMDB.Return := db_Pack_PathNameError;
              Result := False;
              Exit;
            end;
          case dbField_FindFirst(TempPathStr, db_Header_FieldID, _Field.RHeader.CurrentHeader, SenderTMDB.RecFile, _FieldSearch) of
            False:
              begin
                _Field.Description := Description;
                if dbField_CreateField(TempPathStr, _Field.RHeader.CurrentHeader, SenderTMDB.RecFile, _Field) = False then
                  begin
                    SenderTMDB.Return := _Field.Return;
                    Result := False;
                    Exit;
                  end;
              end;
            True:
              begin
                if dbField_ReadRec(_FieldSearch.RHeader.CurrentHeader, SenderTMDB.RecFile, _Field) = False then
                  begin
                    SenderTMDB.Return := _Field.Return;
                    Result := False;
                    Exit;
                  end;
              end;
          end;
        end;
    end;
  SenderTMDB.WriteFlags := True;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_SetFieldName(const PathName, OriginFieldName, NewFieldName, FieldDescription: string; var SenderTMDB: TTMDB): Boolean;
var
  TempSR     : TFieldSearch;
  _Field     : TField;
  OriginField: TField;
begin
  if dbPack_GetField(PathName, _Field, SenderTMDB) = False then
    begin
      Result := False;
      Exit;
    end;
  if dbField_FindFirst(OriginFieldName, db_Header_FieldID, _Field.RHeader.CurrentHeader, SenderTMDB.RecFile, TempSR) = False then
    begin
      SenderTMDB.Return := TempSR.Return;
      Result := False;
      Exit;
    end;
  if dbField_ReadRec(TempSR.RHeader.CurrentHeader, SenderTMDB.RecFile, OriginField) = False then
    begin
      SenderTMDB.Return := OriginField.RHeader.Return;
      Result := False;
      Exit;
    end;
  OriginField.RHeader.Name := NewFieldName;
  OriginField.Description := FieldDescription;
  if dbField_WriteRec(OriginField.RHeader.CurrentHeader, SenderTMDB.RecFile, OriginField) = False then
    begin
      SenderTMDB.Return := OriginField.RHeader.Return;
      Result := False;
      Exit;
    end;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_SetItemName(const PathName, OriginItemName, NewItemName, ItemDescription: string; var SenderTMDB: TTMDB): Boolean;
var
  TempSR    : TFieldSearch;
  _Field    : TField;
  OriginItem: TItem;
begin
  if dbPack_GetField(PathName, _Field, SenderTMDB) = False then
    begin
      Result := False;
      Exit;
    end;
  if dbField_FindFirst(OriginItemName, db_Header_ItemID, _Field.RHeader.CurrentHeader, SenderTMDB.RecFile, TempSR) = False then
    begin
      SenderTMDB.Return := TempSR.Return;
      Result := False;
      Exit;
    end;
  if dbItem_ReadRec(TempSR.RHeader.CurrentHeader, SenderTMDB.RecFile, OriginItem) = False then
    begin
      SenderTMDB.Return := OriginItem.RHeader.Return;
      Result := False;
      Exit;
    end;
  OriginItem.RHeader.Name := NewItemName;
  OriginItem.Description := ItemDescription;
  if dbItem_WriteRec(OriginItem.RHeader.CurrentHeader, SenderTMDB.RecFile, OriginItem) = False then
    begin
      SenderTMDB.Return := OriginItem.RHeader.Return;
      Result := False;
      Exit;
    end;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_DeleteField(const PathName, FilterName: string; var SenderTMDB: TTMDB): Boolean;
var
  TempSR: TFieldSearch;
  _Field: TField;
begin
  if dbPack_GetField(PathName, _Field, SenderTMDB) = False then
    begin
      Result := False;
      Exit;
    end;
  if dbField_FindFirst(FilterName, db_Header_FieldID, _Field.RHeader.CurrentHeader, SenderTMDB.RecFile, TempSR) = False then
    begin
      SenderTMDB.Return := TempSR.Return;
      Result := False;
      Exit;
    end;
  if dbField_DeleteHeader(TempSR.RHeader.CurrentHeader, _Field.RHeader.CurrentHeader, SenderTMDB.RecFile, _Field) = False then
    begin
      SenderTMDB.Return := _Field.Return;
      Result := False;
      Exit;
    end;
  while dbField_FindFirst(FilterName, db_Header_FieldID, _Field.RHeader.CurrentHeader, SenderTMDB.RecFile, TempSR) = True do
    begin
      if dbField_DeleteHeader(TempSR.RHeader.CurrentHeader, _Field.RHeader.CurrentHeader, SenderTMDB.RecFile, _Field) = False then
        begin
          SenderTMDB.Return := _Field.Return;
          Result := False;
          Exit;
        end;
    end;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_DeleteHeader(const PathName, FilterName: string; const ID: Int64; var SenderTMDB: TTMDB): Boolean;
var
  TempSR: TFieldSearch;
  _Field: TField;
begin
  if dbPack_GetField(PathName, _Field, SenderTMDB) = False then
    begin
      Result := False;
      Exit;
    end;
  if dbField_FindFirst(FilterName, ID, _Field.RHeader.CurrentHeader, SenderTMDB.RecFile, TempSR) = False then
    begin
      SenderTMDB.Return := TempSR.Return;
      Result := False;
      Exit;
    end;
  if dbField_DeleteHeader(TempSR.RHeader.CurrentHeader, _Field.RHeader.CurrentHeader, SenderTMDB.RecFile, _Field) = False then
    begin
      SenderTMDB.Return := _Field.Return;
      Result := False;
      Exit;
    end;
  while dbField_FindFirst(FilterName, ID, _Field.RHeader.CurrentHeader, SenderTMDB.RecFile, TempSR) = True do
    begin
      if dbField_DeleteHeader(TempSR.RHeader.CurrentHeader, _Field.RHeader.CurrentHeader, SenderTMDB.RecFile, _Field) = False then
        begin
          SenderTMDB.Return := _Field.Return;
          Result := False;
          Exit;
        end;
    end;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_MoveItem(const SourcerPathName, FilterName: string; const TargetPathName: string; const ItemExtID: Int64; var SenderTMDB: TTMDB): Boolean;
var
  TempSR                   : TFieldSearch;
  SourcerField, TargetField: TField;
begin
  if dbPack_GetField(SourcerPathName, SourcerField, SenderTMDB) = False then
    begin
      Result := False;
      Exit;
    end;
  if dbPack_GetField(TargetPathName, TargetField, SenderTMDB) = False then
    begin
      Result := False;
      Exit;
    end;

  if SourcerField.RHeader.CurrentHeader = TargetField.RHeader.CurrentHeader then
    begin
      SenderTMDB.Return := db_Pack_PathNameError;
      Result := False;
      Exit;
    end;
  if dbField_FindFirstItem(FilterName, ItemExtID, SourcerField.RHeader.CurrentHeader, SenderTMDB.RecFile, TempSR) = False then
    begin
      SenderTMDB.Return := TempSR.Return;
      Result := False;
      Exit;
    end;

  if TempSR.RHeader.CurrentHeader = TargetField.RHeader.CurrentHeader then
    begin
      SenderTMDB.Return := db_Pack_PathNameError;
      Result := False;
      Exit;
    end;
  if dbField_MoveHeader(TempSR.RHeader.CurrentHeader, SourcerField.RHeader.CurrentHeader, TargetField.RHeader.CurrentHeader, SenderTMDB.RecFile, TargetField) = False then
    begin
      SenderTMDB.Return := TargetField.Return;
      Result := False;
      Exit;
    end;
  while dbField_FindFirstItem(FilterName, ItemExtID, SourcerField.RHeader.CurrentHeader, SenderTMDB.RecFile, TempSR) = True do
    begin
      if TempSR.RHeader.CurrentHeader = TargetField.RHeader.CurrentHeader then
        begin
          SenderTMDB.Return := db_Pack_PathNameError;
          Result := False;
          Exit;
        end;
      if dbField_MoveHeader(TempSR.RHeader.CurrentHeader, SourcerField.RHeader.CurrentHeader, TargetField.RHeader.CurrentHeader, SenderTMDB.RecFile, TargetField) = False
      then
        begin
          SenderTMDB.Return := TargetField.Return;
          Result := False;
          Exit;
        end;
    end;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_MoveField(const SourcerPathName, FilterName: string; const TargetPathName: string; var SenderTMDB: TTMDB): Boolean;
var
  TempSR                   : TFieldSearch;
  SourcerField, TargetField: TField;
begin
  if dbPack_GetField(SourcerPathName, SourcerField, SenderTMDB) = False then
    begin
      Result := False;
      Exit;
    end;
  if dbPack_GetField(TargetPathName, TargetField, SenderTMDB) = False then
    begin
      Result := False;
      Exit;
    end;

  if SourcerField.RHeader.CurrentHeader = TargetField.RHeader.CurrentHeader then
    begin
      SenderTMDB.Return := db_Pack_PathNameError;
      Result := False;
      Exit;
    end;
  if dbField_FindFirst(FilterName, db_Header_FieldID, SourcerField.RHeader.CurrentHeader, SenderTMDB.RecFile, TempSR) = False then
    begin
      SenderTMDB.Return := TempSR.Return;
      Result := False;
      Exit;
    end;

  if TempSR.RHeader.CurrentHeader = TargetField.RHeader.CurrentHeader then
    begin
      SenderTMDB.Return := db_Pack_PathNameError;
      Result := False;
      Exit;
    end;
  if dbField_MoveHeader(TempSR.RHeader.CurrentHeader, SourcerField.RHeader.CurrentHeader, TargetField.RHeader.CurrentHeader, SenderTMDB.RecFile, TargetField) = False then
    begin
      SenderTMDB.Return := TargetField.Return;
      Result := False;
      Exit;
    end;
  while dbField_FindFirst(FilterName, db_Header_FieldID, SourcerField.RHeader.CurrentHeader, SenderTMDB.RecFile, TempSR) = True do
    begin
      if TempSR.RHeader.CurrentHeader = TargetField.RHeader.CurrentHeader then
        begin
          SenderTMDB.Return := db_Pack_PathNameError;
          Result := False;
          Exit;
        end;
      if dbField_MoveHeader(TempSR.RHeader.CurrentHeader, SourcerField.RHeader.CurrentHeader, TargetField.RHeader.CurrentHeader, SenderTMDB.RecFile, TargetField) = False
      then
        begin
          SenderTMDB.Return := TargetField.Return;
          Result := False;
          Exit;
        end;
    end;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_MoveHeader(const SourcerPathName, FilterName: string; const TargetPathName: string; const HeaderID: Int64; var SenderTMDB: TTMDB): Boolean;
var
  TempSR                   : TFieldSearch;
  SourcerField, TargetField: TField;
begin
  if dbPack_GetField(SourcerPathName, SourcerField, SenderTMDB) = False then
    begin
      Result := False;
      Exit;
    end;
  if dbPack_GetField(TargetPathName, TargetField, SenderTMDB) = False then
    begin
      Result := False;
      Exit;
    end;

  if SourcerField.RHeader.CurrentHeader = TargetField.RHeader.CurrentHeader then
    begin
      SenderTMDB.Return := db_Pack_PathNameError;
      Result := False;
      Exit;
    end;
  if dbField_FindFirst(FilterName, HeaderID, SourcerField.RHeader.CurrentHeader, SenderTMDB.RecFile, TempSR) = False then
    begin
      SenderTMDB.Return := TempSR.Return;
      Result := False;
      Exit;
    end;

  if TempSR.RHeader.CurrentHeader = TargetField.RHeader.CurrentHeader then
    begin
      SenderTMDB.Return := db_Pack_PathNameError;
      Result := False;
      Exit;
    end;
  if dbField_MoveHeader(TempSR.RHeader.CurrentHeader, SourcerField.RHeader.CurrentHeader, TargetField.RHeader.CurrentHeader, SenderTMDB.RecFile, TargetField) = False then
    begin
      SenderTMDB.Return := TargetField.Return;
      Result := False;
      Exit;
    end;
  while dbField_FindFirst(FilterName, HeaderID, SourcerField.RHeader.CurrentHeader, SenderTMDB.RecFile, TempSR) = True do
    begin
      if TempSR.RHeader.CurrentHeader = TargetField.RHeader.CurrentHeader then
        begin
          SenderTMDB.Return := db_Pack_PathNameError;
          Result := False;
          Exit;
        end;
      if dbField_MoveHeader(TempSR.RHeader.CurrentHeader, SourcerField.RHeader.CurrentHeader, TargetField.RHeader.CurrentHeader, SenderTMDB.RecFile, TargetField) = False
      then
        begin
          SenderTMDB.Return := TargetField.Return;
          Result := False;
          Exit;
        end;
    end;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_SetCurrentRootField(const Name: string; var SenderTMDB: TTMDB): Boolean;
var
  _Field: TField;
begin
  if SenderTMDB.RootHeaderCount = 0 then
    begin
      SenderTMDB.Return := db_Pack_PathNameError;
      Result := False;
      Exit;
    end;
  if dbHeader_ReadRec(SenderTMDB.DefaultFieldPOS, SenderTMDB.RecFile, _Field.RHeader) = False then
    begin
      SenderTMDB.Return := _Field.RHeader.Return;
      Result := False;
      Exit;
    end;

  if (dbHeader_MultipleMatch(name, _Field.RHeader.Name) = True) and (_Field.RHeader.ID = db_Header_FieldID) then
    begin
      SenderTMDB.DefaultFieldPOS := _Field.RHeader.CurrentHeader;
      SenderTMDB.Return := db_Pack_ok;
      Result := True;
      Exit;
    end;

  if SenderTMDB.RootHeaderCount = 1 then
    begin
      SenderTMDB.Return := db_Pack_PathNameError;
      Result := False;
      Exit;
    end;
  if dbHeader_ReadRec(_Field.RHeader.NextHeader, SenderTMDB.RecFile, _Field.RHeader) = False then
    begin
      SenderTMDB.Return := _Field.RHeader.Return;
      Result := False;
      Exit;
    end;

  if (dbHeader_MultipleMatch(name, _Field.RHeader.Name) = True) and (_Field.RHeader.ID = db_Header_FieldID) then
    begin
      SenderTMDB.DefaultFieldPOS := _Field.RHeader.CurrentHeader;
      SenderTMDB.Return := db_Pack_ok;
      Result := True;
      Exit;
    end;

  while _Field.RHeader.CurrentHeader <> SenderTMDB.DefaultFieldPOS do
    begin
      if dbHeader_ReadRec(_Field.RHeader.NextHeader, SenderTMDB.RecFile, _Field.RHeader) = False then
        begin
          SenderTMDB.Return := _Field.RHeader.Return;
          Result := False;
          Exit;
        end;
      if (dbHeader_MultipleMatch(name, _Field.RHeader.Name) = True) and (_Field.RHeader.ID = db_Header_FieldID) then
        begin
          SenderTMDB.DefaultFieldPOS := _Field.RHeader.CurrentHeader;
          SenderTMDB.Return := db_Pack_ok;
          Result := True;
          Exit;
        end;
    end;
  SenderTMDB.Return := db_Pack_PathNameError;
  Result := False;
end;

function dbPack_SetCurrentField(const PathName: string; var SenderTMDB: TTMDB): Boolean;
var
  _Field                   : TField;
  _FieldSearch             : TFieldSearch;
  Rep_Int, _PathCount      : Integer;
  TempPathStr, TempPathName: string;
begin
  if umlFileTest(SenderTMDB.RecFile) = False then
    begin
      SenderTMDB.Return := db_Pack_ClosePackError;
      Result := False;
      Exit;
    end;
  if dbField_ReadRec(SenderTMDB.DefaultFieldPOS, SenderTMDB.RecFile, _Field) = False then
    begin
      SenderTMDB.Return := _Field.Return;
      Result := False;
      Exit;
    end;

  if umlGetLength(PathName) = 0 then
    begin
      SenderTMDB.CurrentFieldPOS := _Field.RHeader.CurrentHeader;
      SenderTMDB.CurrentFieldLevel := 1;
      SenderTMDB.Return := db_Pack_ok;
      Result := True;
      Exit;
    end;
  TempPathName := PathName;
  _PathCount := dbPack_GetIndexStrCount(TempPathName);
  if _PathCount > 0 then
    begin
      for Rep_Int := 1 to _PathCount do
        begin
          TempPathStr := dbPack_GetFirstPath(TempPathName);
          TempPathName := dbPack_MaskFirstPath(TempPathName);

          if dbPack_TestNameStr(TempPathStr) = False then
            begin
              SenderTMDB.Return := db_Pack_PathNameError;
              Result := False;
              Exit;
            end;
          if dbField_FindFirst(TempPathStr, db_Header_FieldID, _Field.RHeader.CurrentHeader, SenderTMDB.RecFile, _FieldSearch) = False then
            begin
              SenderTMDB.Return := _FieldSearch.Return;
              Result := False;
              Exit;
            end;
          if dbField_ReadRec(_FieldSearch.RHeader.CurrentHeader, SenderTMDB.RecFile, _Field) = False then
            begin
              SenderTMDB.Return := _Field.Return;
              Result := False;
              Exit;
            end;
        end;
    end;
  SenderTMDB.CurrentFieldPOS := _Field.RHeader.CurrentHeader;
  SenderTMDB.CurrentFieldLevel := _PathCount;
  SenderTMDB.WriteFlags := True;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_GetRootField(const Name: string; var SenderField: TField; var SenderTMDB: TTMDB): Boolean;
var
  _Field: TField;
begin
  InitTField(SenderField);
  InitTField(_Field);

  if SenderTMDB.RootHeaderCount = 0 then
    begin
      SenderTMDB.Return := db_Pack_PathNameError;
      SenderField.Return := SenderTMDB.Return;
      Result := False;
      Exit;
    end;
  if dbHeader_ReadRec(SenderTMDB.DefaultFieldPOS, SenderTMDB.RecFile, _Field.RHeader) = False then
    begin
      SenderTMDB.Return := _Field.RHeader.Return;
      SenderField.Return := SenderTMDB.Return;
      Result := False;
      Exit;
    end;

  if (dbHeader_MultipleMatch(name, _Field.RHeader.Name) = True) and (_Field.RHeader.ID = db_Header_FieldID) then
    begin
      SenderTMDB.Return := db_Pack_ok;
      SenderField := _Field;
      Result := True;
      Exit;
    end;

  if SenderTMDB.RootHeaderCount = 1 then
    begin
      SenderTMDB.Return := db_Pack_PathNameError;
      SenderField.Return := SenderTMDB.Return;
      Result := False;
      Exit;
    end;
  if dbHeader_ReadRec(_Field.RHeader.NextHeader, SenderTMDB.RecFile, _Field.RHeader) = False then
    begin
      SenderTMDB.Return := _Field.RHeader.Return;
      SenderField.Return := SenderTMDB.Return;
      Result := False;
      Exit;
    end;

  if (dbHeader_MultipleMatch(name, _Field.RHeader.Name) = True) and (_Field.RHeader.ID = db_Header_FieldID) then
    begin
      SenderTMDB.Return := db_Pack_ok;
      SenderField := _Field;
      Result := True;
      Exit;
    end;

  while _Field.RHeader.CurrentHeader <> SenderTMDB.DefaultFieldPOS do
    begin
      if dbHeader_ReadRec(_Field.RHeader.NextHeader, SenderTMDB.RecFile, _Field.RHeader) = False then
        begin
          SenderTMDB.Return := _Field.RHeader.Return;
          SenderField.Return := SenderTMDB.Return;
          Result := False;
          Exit;
        end;
      if (dbHeader_MultipleMatch(name, _Field.RHeader.Name) = True) and (_Field.RHeader.ID = db_Header_FieldID) then
        begin
          SenderTMDB.Return := db_Pack_ok;
          SenderField := _Field;
          Result := True;
          Exit;
        end;
    end;
  SenderTMDB.Return := db_Pack_PathNameError;
  SenderField.Return := SenderTMDB.Return;
  Result := False;
end;

function dbPack_GetField(const PathName: string; var SenderField: TField; var SenderTMDB: TTMDB): Boolean;
var
  _Field                   : TField;
  _FieldSearch             : TFieldSearch;
  Rep_Int, _PathCount      : Integer;
  TempPathStr, TempPathName: string;
begin
  if umlFileTest(SenderTMDB.RecFile) = False then
    begin
      SenderTMDB.Return := db_Pack_ClosePackError;
      Result := False;
      Exit;
    end;
  if dbField_ReadRec(SenderTMDB.DefaultFieldPOS, SenderTMDB.RecFile, _Field) = False then
    begin
      SenderTMDB.Return := _Field.Return;
      Result := False;
      Exit;
    end;

  if umlGetLength(PathName) = 0 then
    begin
      SenderField := _Field;
      SenderTMDB.Return := db_Pack_ok;
      Result := True;
      Exit;
    end;
  TempPathName := PathName;
  _PathCount := dbPack_GetIndexStrCount(TempPathName);

  if _PathCount > 0 then
    begin
      for Rep_Int := 1 to _PathCount do
        begin
          TempPathStr := dbPack_GetFirstPath(TempPathName);
          TempPathName := dbPack_MaskFirstPath(TempPathName);

          if dbPack_TestNameStr(TempPathStr) = False then
            begin
              SenderTMDB.Return := db_Pack_PathNameError;
              Result := False;
              Exit;
            end;
          if dbField_FindFirst(TempPathStr, db_Header_FieldID, _Field.RHeader.CurrentHeader, SenderTMDB.RecFile, _FieldSearch) = False then
            begin
              SenderTMDB.Return := _FieldSearch.Return;
              Result := False;
              Exit;
            end;
          if dbField_ReadRec(_FieldSearch.RHeader.CurrentHeader, SenderTMDB.RecFile, _Field) = False then
            begin
              SenderTMDB.Return := _Field.Return;
              Result := False;
              Exit;
            end;
        end;
    end;
  SenderField := _Field;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_GetPath(const FieldPos, RootFieldPos: Int64; var SenderTMDB: TTMDB; var RetPath: string): Boolean;
var
  _Field: TField;
begin

  if dbHeader_ReadRec(FieldPos, SenderTMDB.RecFile, _Field.RHeader) = False then
    begin
      SenderTMDB.Return := _Field.RHeader.Return;
      Result := False;
      Exit;
    end;

  if _Field.RHeader.ID <> db_Header_FieldID then
    begin
      SenderTMDB.Return := db_Field_SetPosError;
      Result := False;
      Exit;
    end;
  if dbField_OnlyReadFieldRec(_Field.RHeader.DataMainPOS, SenderTMDB.RecFile, _Field) = False then
    begin
      SenderTMDB.Return := _Field.Return;
      Result := False;
      Exit;
    end;

  if _Field.RHeader.CurrentHeader = RootFieldPos then
    begin
      RetPath := db_PathChar;
      SenderTMDB.Return := db_Pack_ok;
      Result := True;
      Exit;
    end;
  RetPath := _Field.RHeader.Name + db_PathChar;

  while dbField_ReadRec(_Field.HigherFieldPOS, SenderTMDB.RecFile, _Field) = True do
    begin
      if _Field.RHeader.CurrentHeader = RootFieldPos then
        begin
          RetPath := db_PathChar + RetPath;
          SenderTMDB.Return := db_Pack_ok;
          Result := True;
          Exit;
        end;
      RetPath := _Field.RHeader.Name + db_PathChar + RetPath;
    end;
  SenderTMDB.Return := _Field.Return;
  Result := False;
end;

function dbPack_NewItem(const PathName, ItemName, ItemDescription: string; const ItemExtID: Int64; var SenderItem: TItem; var SenderTMDB: TTMDB): Boolean;
var
  _Field                   : TField;
  _FieldSearch             : TFieldSearch;
  Rep_Int, _PathCount      : Integer;
  TempPathStr, TempPathName: string;
begin
  if umlFileTest(SenderTMDB.RecFile) = False then
    begin
      SenderTMDB.Return := db_Pack_ClosePackError;
      Result := False;
      Exit;
    end;
  if dbField_ReadRec(SenderTMDB.DefaultFieldPOS, SenderTMDB.RecFile, _Field) = False then
    begin
      SenderTMDB.Return := _Field.Return;
      Result := False;
      Exit;
    end;

  if umlGetLength(PathName) > 0 then
    begin
      TempPathName := PathName;
      _PathCount := dbPack_GetIndexStrCount(TempPathName);
      if _PathCount > 0 then
        begin
          for Rep_Int := 1 to _PathCount do
            begin
              TempPathStr := dbPack_GetFirstPath(TempPathName);
              TempPathName := dbPack_MaskFirstPath(TempPathName);

              if dbPack_TestNameStr(TempPathStr) = False then
                begin
                  SenderTMDB.Return := db_Pack_PathNameError;
                  Result := False;
                  Exit;
                end;
              case dbField_FindFirst(TempPathStr, db_Header_FieldID, _Field.RHeader.CurrentHeader, SenderTMDB.RecFile, _FieldSearch) of
                False:
                  begin
                    _Field.Description := db_Pack_DefaultDescription;
                    if dbField_CreateField(TempPathStr, _Field.RHeader.CurrentHeader, SenderTMDB.RecFile, _Field) = False then
                      begin
                        SenderTMDB.Return := _Field.Return;
                        Result := False;
                        Exit;
                      end;
                  end;
                True:
                  begin
                    if dbField_ReadRec(_FieldSearch.RHeader.CurrentHeader, SenderTMDB.RecFile, _Field) = False then
                      begin
                        SenderTMDB.Return := _Field.Return;
                        Result := False;
                        Exit;
                      end;
                  end;
              end;
            end;
        end;
    end;

  if dbPack_TestNameStr(ItemName) = False then
    begin
      SenderTMDB.Return := db_Pack_ItemNameError;
      Result := False;
      Exit;
    end;
  if dbField_FindFirstItem(ItemName, ItemExtID, _Field.RHeader.CurrentHeader, SenderTMDB.RecFile, _FieldSearch) = True then
    begin
      if SenderTMDB.OverWriteItem = False then
        begin
          if SenderTMDB.SameItemName = False then
            begin
              SenderTMDB.Return := db_Pack_RepeatCreateItemError;
              Result := False;
              Exit;
            end;
        end
      else
        begin
          if dbField_DeleteHeader(_FieldSearch.RHeader.CurrentHeader, _Field.RHeader.CurrentHeader, SenderTMDB.RecFile, _Field) = False then
            begin
              SenderTMDB.Return := _Field.Return;
              Result := False;
              Exit;
            end;
        end;
    end;
  SenderItem.Description := ItemDescription;
  if dbField_CreateItem(ItemName, ItemExtID, _Field.RHeader.CurrentHeader, SenderTMDB.RecFile, SenderItem) = False then
    begin
      SenderTMDB.Return := SenderItem.Return;
      Result := False;
      Exit;
    end;
  SenderTMDB.WriteFlags := True;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_DeleteItem(const PathName, FilterName: string; const ItemExtID: Int64; var SenderTMDB: TTMDB): Boolean;
var
  TempSR: TFieldSearch;
  _Field: TField;
begin
  if dbPack_GetField(PathName, _Field, SenderTMDB) = False then
    begin
      Result := False;
      Exit;
    end;
  if dbField_FindFirstItem(FilterName, ItemExtID, _Field.RHeader.CurrentHeader, SenderTMDB.RecFile, TempSR) = False then
    begin
      SenderTMDB.Return := TempSR.Return;
      Result := False;
      Exit;
    end;
  if dbField_DeleteHeader(TempSR.RHeader.CurrentHeader, _Field.RHeader.CurrentHeader, SenderTMDB.RecFile, _Field) = False then
    begin
      SenderTMDB.Return := _Field.Return;
      Result := False;
      Exit;
    end;
  while dbField_FindFirstItem(FilterName, ItemExtID, _Field.RHeader.CurrentHeader, SenderTMDB.RecFile, TempSR) = True do
    begin
      if dbField_DeleteHeader(TempSR.RHeader.CurrentHeader, _Field.RHeader.CurrentHeader, SenderTMDB.RecFile, _Field) = False then
        begin
          SenderTMDB.Return := _Field.Return;
          Result := False;
          Exit;
        end;
    end;
  SenderTMDB.WriteFlags := True;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_GetItem(const PathName, ItemName: string; const ItemExtID: Int64; var SenderItem: TItem; var SenderTMDB: TTMDB): Boolean;
var
  _Field  : TField;
  _FieldSR: TFieldSearch;
begin
  if dbPack_GetField(PathName, _Field, SenderTMDB) = False then
    begin
      Result := False;
      Exit;
    end;
  if dbField_FindFirstItem(ItemName, ItemExtID, _Field.RHeader.CurrentHeader, SenderTMDB.RecFile, _FieldSR) = False then
    begin
      SenderTMDB.Return := db_Pack_OpenItemError;
      Result := False;
      Exit;
    end;
  if dbItem_ReadRec(_FieldSR.RHeader.CurrentHeader, SenderTMDB.RecFile, SenderItem) = False then
    begin
      SenderTMDB.Return := SenderItem.Return;
      Result := False;
      Exit;
    end;
  SenderTMDB.WriteFlags := True;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_ItemCreate(const PathName, ItemName, ItemDescription: string; const ItemExtID: Int64; var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Boolean;
begin
  if SenderTMDBItemHandle.OpenFlags = True then
    begin
      SenderTMDB.Return := db_Pack_RepeatCreateItemError;
      Result := False;
      Exit;
    end;
  if dbPack_NewItem(PathName, ItemName, ItemDescription, ItemExtID, SenderTMDBItemHandle.Item, SenderTMDB) = False then
    begin
      Result := False;
      Exit;
    end;
  if dbItem_BlockInit(SenderTMDB.RecFile, SenderTMDBItemHandle.Item) = False then
    begin
      SenderTMDB.Return := SenderTMDBItemHandle.Item.Return;
      Result := False;
      Exit;
    end;
  SenderTMDBItemHandle.Path := PathName;
  SenderTMDBItemHandle.Name := ItemName;
  SenderTMDBItemHandle.Description := ItemDescription;
  SenderTMDBItemHandle.CreateTime := SenderTMDBItemHandle.Item.RHeader.CreateTime;
  SenderTMDBItemHandle.LastModifyTime := SenderTMDBItemHandle.Item.RHeader.LastModifyTime;
  SenderTMDBItemHandle.ItemExtID := ItemExtID;
  SenderTMDBItemHandle.OpenFlags := True;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_ItemFastCreate(const ItemName, ItemDescription: string; const POS: Int64; const ItemExtID: Int64; var SenderTMDBItemHandle: TTMDBItemHandle;
  var SenderTMDB: TTMDB): Boolean;
begin
  if SenderTMDBItemHandle.OpenFlags = True then
    begin
      SenderTMDB.Return := db_Pack_RepeatCreateItemError;
      Result := False;
      Exit;
    end;
  if dbField_CreateItem(ItemName, ItemExtID, POS, SenderTMDB.RecFile, SenderTMDBItemHandle.Item) = False then
    begin
      Result := False;
      Exit;
    end;
  if dbItem_BlockInit(SenderTMDB.RecFile, SenderTMDBItemHandle.Item) = False then
    begin
      SenderTMDB.Return := SenderTMDBItemHandle.Item.Return;
      Result := False;
      Exit;
    end;
  SenderTMDBItemHandle.Path := '';
  SenderTMDBItemHandle.Name := ItemName;
  SenderTMDBItemHandle.Description := ItemDescription;
  SenderTMDBItemHandle.CreateTime := SenderTMDBItemHandle.Item.RHeader.CreateTime;
  SenderTMDBItemHandle.LastModifyTime := SenderTMDBItemHandle.Item.RHeader.LastModifyTime;
  SenderTMDBItemHandle.ItemExtID := ItemExtID;
  SenderTMDBItemHandle.OpenFlags := True;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_ItemOpen(const PathName, ItemName: string; const ItemExtID: Int64; var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Boolean;
begin
  if SenderTMDBItemHandle.OpenFlags = True then
    begin
      SenderTMDB.Return := db_Pack_RepeatOpenItemError;
      Result := False;
      Exit;
    end;
  if dbPack_GetItem(PathName, ItemName, ItemExtID, SenderTMDBItemHandle.Item, SenderTMDB) = False then
    begin
      Result := False;
      Exit;
    end;
  if dbItem_BlockInit(SenderTMDB.RecFile, SenderTMDBItemHandle.Item) = False then
    begin
      SenderTMDB.Return := SenderTMDBItemHandle.Item.Return;
      Result := False;
      Exit;
    end;
  SenderTMDBItemHandle.Path := PathName;
  SenderTMDBItemHandle.Name := ItemName;
  SenderTMDBItemHandle.Description := SenderTMDBItemHandle.Item.Description;
  SenderTMDBItemHandle.CreateTime := SenderTMDBItemHandle.Item.RHeader.CreateTime;
  SenderTMDBItemHandle.LastModifyTime := SenderTMDBItemHandle.Item.RHeader.LastModifyTime;
  SenderTMDBItemHandle.ItemExtID := ItemExtID;
  SenderTMDBItemHandle.OpenFlags := True;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_ItemFastOpen(const POS: Int64; const ItemExtID: Int64; var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Boolean;
var
  _Item: TItem;
begin
  if SenderTMDBItemHandle.OpenFlags = True then
    begin
      SenderTMDB.Return := db_Pack_RepeatOpenItemError;
      Result := False;
      Exit;
    end;
  if dbHeader_ReadRec(POS, SenderTMDB.RecFile, _Item.RHeader) = False then
    begin
      SenderTMDB.Return := _Item.RHeader.Return;
      Result := False;
      Exit;
    end;
  if _Item.RHeader.ID <> db_Header_ItemID then
    begin
      SenderTMDB.Return := db_Pack_OpenItemError;
      Result := False;
      Exit;
    end;
  if dbItem_OnlyReadItemRec(_Item.RHeader.DataMainPOS, SenderTMDB.RecFile, _Item) = False then
    begin
      SenderTMDB.Return := _Item.Return;
      Result := False;
      Exit;
    end;
  if _Item.ExtID <> ItemExtID then
    begin
      SenderTMDB.Return := db_Pack_OpenItemError;
      Result := False;
      Exit;
    end;
  SenderTMDBItemHandle.Item := _Item;
  if dbItem_BlockInit(SenderTMDB.RecFile, SenderTMDBItemHandle.Item) = False then
    begin
      SenderTMDB.Return := SenderTMDBItemHandle.Item.Return;
      Result := False;
      Exit;
    end;
  SenderTMDBItemHandle.Path := '';
  SenderTMDBItemHandle.Name := _Item.RHeader.Name;
  SenderTMDBItemHandle.Description := SenderTMDBItemHandle.Item.Description;
  SenderTMDBItemHandle.CreateTime := SenderTMDBItemHandle.Item.RHeader.CreateTime;
  SenderTMDBItemHandle.LastModifyTime := SenderTMDBItemHandle.Item.RHeader.LastModifyTime;
  SenderTMDBItemHandle.ItemExtID := ItemExtID;
  SenderTMDBItemHandle.OpenFlags := True;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_ItemUpdate(var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Boolean;
begin
  if SenderTMDBItemHandle.OpenFlags = False then
    begin
      SenderTMDB.Return := db_Pack_CloseItemError;
      Result := False;
      Exit;
    end;
  if SenderTMDBItemHandle.Item.DataModify then
    begin
      SenderTMDBItemHandle.Item.RHeader.Name := SenderTMDBItemHandle.Name;
      SenderTMDBItemHandle.Item.RHeader.CreateTime := SenderTMDBItemHandle.CreateTime;
      SenderTMDBItemHandle.Item.RHeader.LastModifyTime := SenderTMDBItemHandle.LastModifyTime;
      SenderTMDBItemHandle.Item.Description := SenderTMDBItemHandle.Description;
      SenderTMDBItemHandle.Item.ExtID := SenderTMDBItemHandle.ItemExtID;
      if dbItem_WriteRec(SenderTMDBItemHandle.Item.RHeader.CurrentHeader, SenderTMDB.RecFile, SenderTMDBItemHandle.Item) = False then
        begin
          SenderTMDB.Return := SenderTMDBItemHandle.Item.Return;
          Result := False;
          Exit;
        end;
      SenderTMDBItemHandle.Item.DataModify := False;
    end;
  Result := True;
end;

function dbPack_ItemBodyReset(var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Boolean;
begin
  if SenderTMDBItemHandle.OpenFlags = False then
    begin
      SenderTMDB.Return := db_Pack_CloseItemError;
      Result := False;
      Exit;
    end;

  SenderTMDBItemHandle.Item.RHeader.Name := SenderTMDBItemHandle.Name;
  SenderTMDBItemHandle.Item.RHeader.CreateTime := SenderTMDBItemHandle.CreateTime;
  SenderTMDBItemHandle.Item.RHeader.LastModifyTime := SenderTMDBItemHandle.LastModifyTime;
  SenderTMDBItemHandle.Item.Description := SenderTMDBItemHandle.Description;
  SenderTMDBItemHandle.Item.ExtID := SenderTMDBItemHandle.ItemExtID;

  SenderTMDBItemHandle.Item.FirstBlockPOS := 0;
  SenderTMDBItemHandle.Item.LastBlockPOS := 0;
  SenderTMDBItemHandle.Item.Size := 0;
  SenderTMDBItemHandle.Item.BlockCount := 0;

  if dbItem_WriteRec(SenderTMDBItemHandle.Item.RHeader.CurrentHeader, SenderTMDB.RecFile, SenderTMDBItemHandle.Item) = False then
    begin
      SenderTMDB.Return := SenderTMDBItemHandle.Item.Return;
      Result := False;
      Exit;
    end;
  SenderTMDBItemHandle.Item.DataModify := False;

  Result := True;
end;

function dbPack_ItemClose(var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Boolean;
begin
  Result := dbPack_ItemUpdate(SenderTMDBItemHandle, SenderTMDB);
  if Result then
      SenderTMDBItemHandle.OpenFlags := False;
end;

function dbPack_ItemReName(const FieldPos: Int64; const NewItemName, NewItemDescription: string; var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Boolean;
var
  SenderSearchHnd: TTMDBSearchItem;
begin
  if SenderTMDBItemHandle.OpenFlags = False then
    begin
      SenderTMDB.Return := db_Pack_CloseItemError;
      Result := False;
      Exit;
    end;
  if dbPack_FastFindFirstItem(FieldPos, NewItemName, SenderTMDBItemHandle.ItemExtID, SenderSearchHnd, SenderTMDB) then
    if (SenderTMDBItemHandle.Name = NewItemName) then
      begin
        SenderTMDB.Return := db_Pack_ItemNameError;
        Result := False;
        Exit;
      end;
  SenderTMDBItemHandle.Name := NewItemName;
  SenderTMDBItemHandle.Description := NewItemDescription;
  SenderTMDBItemHandle.Item.RHeader.Name := SenderTMDBItemHandle.Name;
  SenderTMDBItemHandle.Item.Description := SenderTMDBItemHandle.Description;
  if dbItem_WriteRec(SenderTMDBItemHandle.Item.RHeader.CurrentHeader, SenderTMDB.RecFile, SenderTMDBItemHandle.Item) = False then
    begin
      SenderTMDB.Return := SenderTMDBItemHandle.Item.Return;
      Result := False;
      Exit;
    end;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_ItemRead(const Size: Int64; var Buffers; var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Boolean;
begin
  if SenderTMDBItemHandle.OpenFlags = False then
    begin
      SenderTMDB.Return := db_Pack_OpenItemError;
      Result := False;
      Exit;
    end;
  if dbItem_BlockReadData(SenderTMDB.RecFile, SenderTMDBItemHandle.Item, Buffers, Size) = False then
    begin
      SenderTMDB.Return := SenderTMDBItemHandle.Item.Return;
      Result := False;
      Exit;
    end;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_ItemWrite(const Size: Int64; var Buffers; var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Boolean;
begin
  if SenderTMDBItemHandle.OpenFlags = False then
    begin
      SenderTMDB.Return := db_Pack_OpenItemError;
      Result := False;
      Exit;
    end;
  if dbItem_BlockWriteData(SenderTMDB.RecFile, SenderTMDBItemHandle.Item, Buffers, Size) = False then
    begin
      SenderTMDB.Return := SenderTMDBItemHandle.Item.Return;
      Result := False;
      Exit;
    end;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_ItemReadStr(var Name: string; var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Boolean;
var
  StrSize : Integer;
  SwapName: umlBytes;
begin
  if SenderTMDBItemHandle.OpenFlags = False then
    begin
      SenderTMDB.Return := db_Pack_OpenItemError;
      Result := False;
      Exit;
    end;
  if dbItem_BlockReadData(SenderTMDB.RecFile, SenderTMDBItemHandle.Item, StrSize, umlDefaultStrLengthSize) = False then
    begin
      SenderTMDB.Return := SenderTMDBItemHandle.Item.Return;
      Result := False;
      Exit;
    end;
  umlSetLength(SwapName, StrSize);
  if StrSize <= 0 then
    begin
      name := '';
      SenderTMDB.Return := db_Pack_ok;
      Result := True;
      Exit;
    end;
  if dbItem_BlockReadData(SenderTMDB.RecFile, SenderTMDBItemHandle.Item, SwapName[0], StrSize) = False then
    begin
      SenderTMDB.Return := SenderTMDBItemHandle.Item.Return;
      Result := False;
      Exit;
    end;
  name := umlStringOf(SwapName).Text;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_ItemWriteStr(const Name: string; var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Boolean;
var
  StrSize : Integer;
  SwapName: umlBytes;
begin
  if SenderTMDBItemHandle.OpenFlags = False then
    begin
      SenderTMDB.Return := db_Pack_OpenItemError;
      Result := False;
      Exit;
    end;
  SwapName := umlBytesOf(name);
  StrSize := umlGetLength(SwapName);
  if dbItem_BlockWriteData(SenderTMDB.RecFile, SenderTMDBItemHandle.Item, StrSize, umlDefaultStrLengthSize) = False then
    begin
      SenderTMDB.Return := SenderTMDBItemHandle.Item.Return;
      Result := False;
      Exit;
    end;
  if StrSize <= 0 then
    begin
      SenderTMDB.Return := db_Pack_ok;
      Result := True;
      Exit;
    end;
  if dbItem_BlockWriteData(SenderTMDB.RecFile, SenderTMDBItemHandle.Item, SwapName[0], StrSize) = False then
    begin
      SenderTMDB.Return := SenderTMDBItemHandle.Item.Return;
      Result := False;
      Exit;
    end;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_ItemSeekPos(const POS: Int64; var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Boolean;
begin
  if SenderTMDBItemHandle.OpenFlags = False then
    begin
      SenderTMDB.Return := db_Pack_OpenItemError;
      Result := False;
      Exit;
    end;
  if dbItem_BlockSeekPOS(SenderTMDB.RecFile, SenderTMDBItemHandle.Item, POS) = False then
    begin
      SenderTMDB.Return := SenderTMDBItemHandle.Item.Return;
      Result := False;
      Exit;
    end;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_ItemSeekStartPos(var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Boolean;
begin
  if SenderTMDBItemHandle.OpenFlags = False then
    begin
      SenderTMDB.Return := db_Pack_OpenItemError;
      Result := False;
      Exit;
    end;
  if dbItem_BlockSeekStartPOS(SenderTMDB.RecFile, SenderTMDBItemHandle.Item) = False then
    begin
      SenderTMDB.Return := SenderTMDBItemHandle.Item.Return;
      Result := False;
      Exit;
    end;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_ItemSeekLastPos(var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Boolean;
begin
  if SenderTMDBItemHandle.OpenFlags = False then
    begin
      SenderTMDB.Return := db_Pack_OpenItemError;
      Result := False;
      Exit;
    end;
  if dbItem_BlockSeekLastPOS(SenderTMDB.RecFile, SenderTMDBItemHandle.Item) = False then
    begin
      SenderTMDB.Return := SenderTMDBItemHandle.Item.Return;
      Result := False;
      Exit;
    end;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_ItemGetPos(var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Int64;
begin
  if SenderTMDBItemHandle.OpenFlags = False then
    begin
      SenderTMDB.Return := db_Pack_OpenItemError;
      Result := 0;
      Exit;
    end;
  Result := dbItem_BlockGetPOS(SenderTMDB.RecFile, SenderTMDBItemHandle.Item);
end;

function dbPack_ItemGetSize(var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Int64;
begin
  if SenderTMDBItemHandle.OpenFlags = False then
    begin
      SenderTMDB.Return := db_Pack_OpenItemError;
      Result := 0;
      Exit;
    end;
  Result := SenderTMDBItemHandle.Item.Size;
end;

function dbPack_AppendItemSize(var SenderTMDBItemHandle: TTMDBItemHandle; const Size: Int64; var SenderTMDB: TTMDB): Boolean;
var
  SwapBuffers: array [0 .. umlMaxFileRecSize] of Byte;
  Rep_Int    : Integer;
begin
  if SenderTMDBItemHandle.OpenFlags = False then
    begin
      SenderTMDB.Return := db_Pack_OpenItemError;
      Result := False;
      Exit;
    end;
  if dbItem_BlockSeekLastPOS(SenderTMDB.RecFile, SenderTMDBItemHandle.Item) = False then
    begin
      SenderTMDB.Return := SenderTMDBItemHandle.Item.Return;
      Result := False;
      Exit;
    end;
  if Size > umlMaxFileRecSize then
    begin
      for Rep_Int := 1 to (Size div umlMaxFileRecSize) do
        begin
          if dbItem_BlockWriteData(SenderTMDB.RecFile, SenderTMDBItemHandle.Item, SwapBuffers, umlMaxFileRecSize) = False then
            begin
              SenderTMDB.Return := SenderTMDBItemHandle.Item.Return;
              Result := False;
              Exit;
            end;
        end;
      if dbItem_BlockWriteData(SenderTMDB.RecFile, SenderTMDBItemHandle.Item, SwapBuffers, (Size mod umlMaxFileRecSize)) = False then
        begin
          SenderTMDB.Return := SenderTMDBItemHandle.Item.Return;
          Result := False;
          Exit;
        end;
      SenderTMDB.Return := db_Pack_ok;
      Result := True;
      Exit;
    end;
  if dbItem_BlockWriteData(SenderTMDB.RecFile, SenderTMDBItemHandle.Item, SwapBuffers, Size) = False then
    begin
      SenderTMDB.Return := SenderTMDBItemHandle.Item.Return;
      Result := False;
      Exit;
    end;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_ExistsRootField(const Name: string; var SenderTMDB: TTMDB): Boolean;
var
  _Field: TField;
begin
  if SenderTMDB.RootHeaderCount = 0 then
    begin
      SenderTMDB.Return := db_Pack_PathNameError;
      Result := False;
      Exit;
    end;
  if dbHeader_ReadRec(SenderTMDB.DefaultFieldPOS, SenderTMDB.RecFile, _Field.RHeader) = False then
    begin
      SenderTMDB.Return := _Field.RHeader.Return;
      Result := False;
      Exit;
    end;

  if (dbHeader_MultipleMatch(name, _Field.RHeader.Name) = True) and (_Field.RHeader.ID = db_Header_FieldID) then
    begin
      SenderTMDB.Return := db_Pack_ok;
      Result := True;
      Exit;
    end;

  if SenderTMDB.RootHeaderCount = 1 then
    begin
      SenderTMDB.Return := db_Pack_PathNameError;
      Result := False;
      Exit;
    end;
  if dbHeader_ReadRec(_Field.RHeader.NextHeader, SenderTMDB.RecFile, _Field.RHeader) = False then
    begin
      SenderTMDB.Return := _Field.RHeader.Return;
      Result := False;
      Exit;
    end;

  if (dbHeader_MultipleMatch(name, _Field.RHeader.Name) = True) and (_Field.RHeader.ID = db_Header_FieldID) then
    begin
      SenderTMDB.Return := db_Pack_ok;
      Result := True;
      Exit;
    end;

  while _Field.RHeader.CurrentHeader <> SenderTMDB.DefaultFieldPOS do
    begin
      if dbHeader_ReadRec(_Field.RHeader.NextHeader, SenderTMDB.RecFile, _Field.RHeader) = False then
        begin
          SenderTMDB.Return := _Field.RHeader.Return;
          Result := False;
          Exit;
        end;
      if (dbHeader_MultipleMatch(name, _Field.RHeader.Name) = True) and (_Field.RHeader.ID = db_Header_FieldID) then
        begin
          SenderTMDB.Return := db_Pack_ok;
          Result := True;
          Exit;
        end;
    end;
  SenderTMDB.Return := db_Pack_PathNameError;
  Result := False;
end;

function dbPack_FindFirstHeader(const PathName, FilterName: string; const ID: Int64; var SenderSearch: TTMDBSearchHeader; var SenderTMDB: TTMDB): Boolean;
var
  _Field: TField;
begin
  if dbPack_GetField(PathName, _Field, SenderTMDB) = False then
    begin
      Result := False;
      Exit;
    end;
  if dbField_FindFirst(FilterName, ID, _Field.RHeader.CurrentHeader, SenderTMDB.RecFile, SenderSearch.FieldSearch) = False then
    begin
      SenderTMDB.Return := SenderSearch.FieldSearch.Return;
      Result := False;
      Exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.ID := SenderSearch.FieldSearch.RHeader.ID;
  SenderSearch.CreateTime := SenderSearch.FieldSearch.RHeader.CreateTime;
  SenderSearch.LastModifyTime := SenderSearch.FieldSearch.RHeader.LastModifyTime;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := 1;
  SenderTMDB.Return := SenderSearch.FieldSearch.Return;
  Result := True;
end;

function dbPack_FindNextHeader(var SenderSearch: TTMDBSearchHeader; var SenderTMDB: TTMDB): Boolean;
begin
  if dbField_FindNext(SenderTMDB.RecFile, SenderSearch.FieldSearch) = False then
    begin
      SenderTMDB.Return := SenderSearch.FieldSearch.Return;
      Result := False;
      Exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.ID := SenderSearch.FieldSearch.RHeader.ID;
  SenderSearch.CreateTime := SenderSearch.FieldSearch.RHeader.CreateTime;
  SenderSearch.LastModifyTime := SenderSearch.FieldSearch.RHeader.LastModifyTime;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := SenderSearch.CompleteCount + 1;
  SenderTMDB.Return := SenderSearch.FieldSearch.Return;
  Result := True;
end;

function dbPack_FindLastHeader(const PathName, FilterName: string; const ID: Int64; var SenderSearch: TTMDBSearchHeader; var SenderTMDB: TTMDB): Boolean;
var
  _Field: TField;
begin
  if dbPack_GetField(PathName, _Field, SenderTMDB) = False then
    begin
      Result := False;
      Exit;
    end;
  if dbField_FindLast(FilterName, ID, _Field.RHeader.CurrentHeader, SenderTMDB.RecFile, SenderSearch.FieldSearch) = False then
    begin
      SenderTMDB.Return := SenderSearch.FieldSearch.Return;
      Result := False;
      Exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.ID := SenderSearch.FieldSearch.RHeader.ID;
  SenderSearch.CreateTime := SenderSearch.FieldSearch.RHeader.CreateTime;
  SenderSearch.LastModifyTime := SenderSearch.FieldSearch.RHeader.LastModifyTime;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := 1;
  SenderTMDB.Return := SenderSearch.FieldSearch.Return;
  Result := True;
end;

function dbPack_FindPrevHeader(var SenderSearch: TTMDBSearchHeader; var SenderTMDB: TTMDB): Boolean;
begin
  if dbField_FindPrev(SenderTMDB.RecFile, SenderSearch.FieldSearch) = False then
    begin
      SenderTMDB.Return := SenderSearch.FieldSearch.Return;
      Result := False;
      Exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.ID := SenderSearch.FieldSearch.RHeader.ID;
  SenderSearch.CreateTime := SenderSearch.FieldSearch.RHeader.CreateTime;
  SenderSearch.LastModifyTime := SenderSearch.FieldSearch.RHeader.LastModifyTime;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := SenderSearch.CompleteCount + 1;
  SenderTMDB.Return := SenderSearch.FieldSearch.Return;
  Result := True;
end;

function dbPack_FindFirstItem(const PathName, FilterName: string; const ItemExtID: Int64; var SenderSearch: TTMDBSearchItem; var SenderTMDB: TTMDB): Boolean;
var
  _Field: TField;
  _Item : TItem;
begin
  if dbPack_GetField(PathName, _Field, SenderTMDB) = False then
    begin
      Result := False;
      Exit;
    end;
  if dbField_FindFirstItem(FilterName, ItemExtID, _Field.RHeader.CurrentHeader, SenderTMDB.RecFile, SenderSearch.FieldSearch) = False then
    begin
      SenderTMDB.Return := SenderSearch.FieldSearch.Return;
      Result := False;
      Exit;
    end;
  if dbItem_OnlyReadItemRec(SenderSearch.FieldSearch.RHeader.DataMainPOS, SenderTMDB.RecFile, _Item) = False then
    begin
      SenderTMDB.Return := _Item.Return;
      Result := False;
      Exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.Description := _Item.Description;
  SenderSearch.ExtID := _Item.ExtID;
  SenderSearch.Size := _Item.Size;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := 1;
  SenderTMDB.Return := SenderSearch.FieldSearch.Return;
  Result := True;
end;

function dbPack_FindNextItem(var SenderSearch: TTMDBSearchItem; const ItemExtID: Int64; var SenderTMDB: TTMDB): Boolean;
var
  _Item: TItem;
begin
  if dbField_FindNextItem(ItemExtID, SenderTMDB.RecFile, SenderSearch.FieldSearch) = False then
    begin
      SenderTMDB.Return := SenderSearch.FieldSearch.Return;
      Result := False;
      Exit;
    end;
  if dbItem_OnlyReadItemRec(SenderSearch.FieldSearch.RHeader.DataMainPOS, SenderTMDB.RecFile, _Item) = False then
    begin
      SenderTMDB.Return := _Item.Return;
      Result := False;
      Exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.Description := _Item.Description;
  SenderSearch.ExtID := _Item.ExtID;
  SenderSearch.Size := _Item.Size;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := SenderSearch.CompleteCount + 1;
  SenderTMDB.Return := SenderSearch.FieldSearch.Return;
  Result := True;
end;

function dbPack_FindLastItem(const PathName, FilterName: string; const ItemExtID: Int64; var SenderSearch: TTMDBSearchItem; var SenderTMDB: TTMDB): Boolean;
var
  _Field: TField;
  _Item : TItem;
begin
  if dbPack_GetField(PathName, _Field, SenderTMDB) = False then
    begin
      Result := False;
      Exit;
    end;
  if dbField_FindLastItem(FilterName, ItemExtID, _Field.RHeader.CurrentHeader, SenderTMDB.RecFile, SenderSearch.FieldSearch) = False then
    begin
      SenderTMDB.Return := SenderSearch.FieldSearch.Return;
      Result := False;
      Exit;
    end;
  if dbItem_OnlyReadItemRec(SenderSearch.FieldSearch.RHeader.DataMainPOS, SenderTMDB.RecFile, _Item) = False then
    begin
      SenderTMDB.Return := _Item.Return;
      Result := False;
      Exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.Description := _Item.Description;
  SenderSearch.ExtID := _Item.ExtID;
  SenderSearch.Size := _Item.Size;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := 1;
  SenderTMDB.Return := SenderSearch.FieldSearch.Return;
  Result := True;
end;

function dbPack_FindPrevItem(var SenderSearch: TTMDBSearchItem; const ItemExtID: Int64; var SenderTMDB: TTMDB): Boolean;
var
  _Item: TItem;
begin
  if dbField_FindPrevItem(ItemExtID, SenderTMDB.RecFile, SenderSearch.FieldSearch) = False then
    begin
      SenderTMDB.Return := SenderSearch.FieldSearch.Return;
      Result := False;
      Exit;
    end;
  if dbItem_OnlyReadItemRec(SenderSearch.FieldSearch.RHeader.DataMainPOS, SenderTMDB.RecFile, _Item) = False then
    begin
      SenderTMDB.Return := _Item.Return;
      Result := False;
      Exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.Description := _Item.Description;
  SenderSearch.ExtID := _Item.ExtID;
  SenderSearch.Size := _Item.Size;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := SenderSearch.CompleteCount + 1;
  SenderTMDB.Return := SenderSearch.FieldSearch.Return;
  Result := True;
end;

function dbPack_FastFindFirstItem(const FieldPos: Int64; const FilterName: string; const ItemExtID: Int64; var SenderSearch: TTMDBSearchItem; var SenderTMDB: TTMDB): Boolean;
var
  _Item: TItem;
begin
  if dbField_FindFirstItem(FilterName, ItemExtID, FieldPos, SenderTMDB.RecFile, SenderSearch.FieldSearch) = False then
    begin
      SenderTMDB.Return := SenderSearch.FieldSearch.Return;
      Result := False;
      Exit;
    end;
  if dbItem_OnlyReadItemRec(SenderSearch.FieldSearch.RHeader.DataMainPOS, SenderTMDB.RecFile, _Item) = False then
    begin
      SenderTMDB.Return := _Item.Return;
      Result := False;
      Exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.Description := _Item.Description;
  SenderSearch.ExtID := _Item.ExtID;
  SenderSearch.Size := _Item.Size;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := 1;
  SenderTMDB.Return := SenderSearch.FieldSearch.Return;
  Result := True;
end;

function dbPack_FastFindNextItem(var SenderSearch: TTMDBSearchItem; const ItemExtID: Int64; var SenderTMDB: TTMDB): Boolean;
var
  _Item: TItem;
begin
  if dbField_FindNextItem(ItemExtID, SenderTMDB.RecFile, SenderSearch.FieldSearch) = False then
    begin
      SenderTMDB.Return := SenderSearch.FieldSearch.Return;
      Result := False;
      Exit;
    end;
  if dbItem_OnlyReadItemRec(SenderSearch.FieldSearch.RHeader.DataMainPOS, SenderTMDB.RecFile, _Item) = False then
    begin
      SenderTMDB.Return := _Item.Return;
      Result := False;
      Exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.Description := _Item.Description;
  SenderSearch.ExtID := _Item.ExtID;
  SenderSearch.Size := _Item.Size;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := SenderSearch.CompleteCount + 1;
  SenderTMDB.Return := SenderSearch.FieldSearch.Return;
  Result := True;
end;

function dbPack_FastFindLastItem(const FieldPos: Int64; const FilterName: string; const ItemExtID: Int64; var SenderSearch: TTMDBSearchItem; var SenderTMDB: TTMDB): Boolean;
var
  _Item: TItem;
begin
  if dbField_FindLastItem(FilterName, ItemExtID, FieldPos, SenderTMDB.RecFile, SenderSearch.FieldSearch) = False then
    begin
      SenderTMDB.Return := SenderSearch.FieldSearch.Return;
      Result := False;
      Exit;
    end;
  if dbItem_OnlyReadItemRec(SenderSearch.FieldSearch.RHeader.DataMainPOS, SenderTMDB.RecFile, _Item) = False then
    begin
      SenderTMDB.Return := _Item.Return;
      Result := False;
      Exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.Description := _Item.Description;
  SenderSearch.ExtID := _Item.ExtID;
  SenderSearch.Size := _Item.Size;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := 1;
  SenderTMDB.Return := SenderSearch.FieldSearch.Return;
  Result := True;
end;

function dbPack_FastFindPrevItem(var SenderSearch: TTMDBSearchItem; const ItemExtID: Int64; var SenderTMDB: TTMDB): Boolean;
var
  _Item: TItem;
begin
  if dbField_FindPrevItem(ItemExtID, SenderTMDB.RecFile, SenderSearch.FieldSearch) = False then
    begin
      SenderTMDB.Return := SenderSearch.FieldSearch.Return;
      Result := False;
      Exit;
    end;
  if dbItem_OnlyReadItemRec(SenderSearch.FieldSearch.RHeader.DataMainPOS, SenderTMDB.RecFile, _Item) = False then
    begin
      SenderTMDB.Return := _Item.Return;
      Result := False;
      Exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.Description := _Item.Description;
  SenderSearch.ExtID := _Item.ExtID;
  SenderSearch.Size := _Item.Size;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := SenderSearch.CompleteCount + 1;
  SenderTMDB.Return := SenderSearch.FieldSearch.Return;
  Result := True;
end;

function dbPack_FindFirstField(const PathName, FilterName: string; var SenderSearch: TTMDBSearchField; var SenderTMDB: TTMDB): Boolean;
var
  _Field: TField;
begin
  if dbPack_GetField(PathName, _Field, SenderTMDB) = False then
    begin
      Result := False;
      Exit;
    end;
  if dbField_FindFirst(FilterName, db_Header_FieldID, _Field.RHeader.CurrentHeader, SenderTMDB.RecFile, SenderSearch.FieldSearch) = False then
    begin
      SenderTMDB.Return := SenderSearch.FieldSearch.Return;
      Result := False;
      Exit;
    end;
  if dbField_OnlyReadFieldRec(SenderSearch.FieldSearch.RHeader.DataMainPOS, SenderTMDB.RecFile, _Field) = False then
    begin
      SenderTMDB.Return := _Field.Return;
      Result := False;
      Exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.Description := _Field.Description;
  SenderSearch.HeaderCount := _Field.HeaderCount;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := 1;
  SenderTMDB.Return := SenderSearch.FieldSearch.Return;
  Result := True;
end;

function dbPack_FindNextField(var SenderSearch: TTMDBSearchField; var SenderTMDB: TTMDB): Boolean;
var
  _Field: TField;
begin
  if dbField_FindNext(SenderTMDB.RecFile, SenderSearch.FieldSearch) = False then
    begin
      SenderTMDB.Return := SenderSearch.FieldSearch.Return;
      Result := False;
      Exit;
    end;
  if dbField_OnlyReadFieldRec(SenderSearch.FieldSearch.RHeader.DataMainPOS, SenderTMDB.RecFile, _Field) = False then
    begin
      SenderTMDB.Return := _Field.Return;
      Result := False;
      Exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.Description := _Field.Description;
  SenderSearch.HeaderCount := _Field.HeaderCount;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := SenderSearch.CompleteCount + 1;
  SenderTMDB.Return := SenderSearch.FieldSearch.Return;
  Result := True;
end;

function dbPack_FindLastField(const PathName, FilterName: string; var SenderSearch: TTMDBSearchField; var SenderTMDB: TTMDB): Boolean;
var
  _Field: TField;
begin
  if dbPack_GetField(PathName, _Field, SenderTMDB) = False then
    begin
      Result := False;
      Exit;
    end;
  if dbField_FindLast(FilterName, db_Header_FieldID, _Field.RHeader.CurrentHeader, SenderTMDB.RecFile, SenderSearch.FieldSearch) = False then
    begin
      SenderTMDB.Return := SenderSearch.FieldSearch.Return;
      Result := False;
      Exit;
    end;
  if dbField_OnlyReadFieldRec(SenderSearch.FieldSearch.RHeader.DataMainPOS, SenderTMDB.RecFile, _Field) = False then
    begin
      SenderTMDB.Return := _Field.Return;
      Result := False;
      Exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.Description := _Field.Description;
  SenderSearch.HeaderCount := _Field.HeaderCount;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := 1;
  SenderTMDB.Return := SenderSearch.FieldSearch.Return;
  Result := True;
end;

function dbPack_FindPrevField(var SenderSearch: TTMDBSearchField; var SenderTMDB: TTMDB): Boolean;
var
  _Field: TField;
begin
  if dbField_FindPrev(SenderTMDB.RecFile, SenderSearch.FieldSearch) = False then
    begin
      SenderTMDB.Return := SenderSearch.FieldSearch.Return;
      Result := False;
      Exit;
    end;
  if dbField_OnlyReadFieldRec(SenderSearch.FieldSearch.RHeader.DataMainPOS, SenderTMDB.RecFile, _Field) = False then
    begin
      SenderTMDB.Return := _Field.Return;
      Result := False;
      Exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.Description := _Field.Description;
  SenderSearch.HeaderCount := _Field.HeaderCount;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := SenderSearch.CompleteCount + 1;
  SenderTMDB.Return := SenderSearch.FieldSearch.Return;
  Result := True;
end;

function dbPack_FastFindFirstField(const FieldPos: Int64; const FilterName: string; var SenderSearch: TTMDBSearchField; var SenderTMDB: TTMDB): Boolean;
var
  _Field: TField;
begin
  if dbField_FindFirst(FilterName, db_Header_FieldID, FieldPos, SenderTMDB.RecFile, SenderSearch.FieldSearch) = False then
    begin
      SenderTMDB.Return := SenderSearch.FieldSearch.Return;
      Result := False;
      Exit;
    end;
  if dbField_OnlyReadFieldRec(SenderSearch.FieldSearch.RHeader.DataMainPOS, SenderTMDB.RecFile, _Field) = False then
    begin
      SenderTMDB.Return := _Field.Return;
      Result := False;
      Exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.Description := _Field.Description;
  SenderSearch.HeaderCount := _Field.HeaderCount;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := 1;
  SenderTMDB.Return := SenderSearch.FieldSearch.Return;
  Result := True;
end;

function dbPack_FastFindNextField(var SenderSearch: TTMDBSearchField; var SenderTMDB: TTMDB): Boolean;
var
  _Field: TField;
begin
  if dbField_FindNext(SenderTMDB.RecFile, SenderSearch.FieldSearch) = False then
    begin
      SenderTMDB.Return := SenderSearch.FieldSearch.Return;
      Result := False;
      Exit;
    end;
  if dbField_OnlyReadFieldRec(SenderSearch.FieldSearch.RHeader.DataMainPOS, SenderTMDB.RecFile, _Field) = False then
    begin
      SenderTMDB.Return := _Field.Return;
      Result := False;
      Exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.Description := _Field.Description;
  SenderSearch.HeaderCount := _Field.HeaderCount;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := SenderSearch.CompleteCount + 1;
  SenderTMDB.Return := SenderSearch.FieldSearch.Return;
  Result := True;
end;

function dbPack_FastFindLastField(const FieldPos: Int64; const FilterName: string; var SenderSearch: TTMDBSearchField; var SenderTMDB: TTMDB): Boolean;
var
  _Field: TField;
begin
  if dbField_FindLast(FilterName, db_Header_FieldID, FieldPos, SenderTMDB.RecFile, SenderSearch.FieldSearch) = False then
    begin
      SenderTMDB.Return := SenderSearch.FieldSearch.Return;
      Result := False;
      Exit;
    end;
  if dbField_OnlyReadFieldRec(SenderSearch.FieldSearch.RHeader.DataMainPOS, SenderTMDB.RecFile, _Field) = False then
    begin
      SenderTMDB.Return := _Field.Return;
      Result := False;
      Exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.Description := _Field.Description;
  SenderSearch.HeaderCount := _Field.HeaderCount;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := 1;
  SenderTMDB.Return := SenderSearch.FieldSearch.Return;
  Result := True;
end;

function dbPack_FastFindPrevField(var SenderSearch: TTMDBSearchField; var SenderTMDB: TTMDB): Boolean;
var
  _Field: TField;
begin
  if dbField_FindPrev(SenderTMDB.RecFile, SenderSearch.FieldSearch) = False then
    begin
      SenderTMDB.Return := SenderSearch.FieldSearch.Return;
      Result := False;
      Exit;
    end;
  if dbField_OnlyReadFieldRec(SenderSearch.FieldSearch.RHeader.DataMainPOS, SenderTMDB.RecFile, _Field) = False then
    begin
      SenderTMDB.Return := _Field.Return;
      Result := False;
      Exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.Description := _Field.Description;
  SenderSearch.HeaderCount := _Field.HeaderCount;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := SenderSearch.CompleteCount + 1;
  SenderTMDB.Return := SenderSearch.FieldSearch.Return;
  Result := True;
end;

function dbPack_RecursionSearchFirst(const InitPath, FilterName: string; var SenderRecursionSearch: TTMDBRecursionSearch; var SenderTMDB: TTMDB): Boolean;
begin
  if dbPack_GetField(InitPath, SenderRecursionSearch.CurrentField, SenderTMDB) = False then
    begin
      Result := False;
      Exit;
    end;
  SenderRecursionSearch.SearchBuffGo := 0;
  if dbField_FindFirst(FilterName, db_Header_ItemID, SenderRecursionSearch.CurrentField.RHeader.CurrentHeader, SenderTMDB.RecFile,
    SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo]) = False then
    begin
      if dbField_FindFirst('*', db_Header_FieldID, SenderRecursionSearch.CurrentField.RHeader.CurrentHeader, SenderTMDB.RecFile,
        SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo]) = False then
        begin
          SenderTMDB.Return := SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo].Return;
          Result := False;
          Exit;
        end;
      if dbField_OnlyReadFieldRec(SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo].RHeader.DataMainPOS, SenderTMDB.RecFile, SenderRecursionSearch.CurrentField) = False
      then
        begin
          SenderTMDB.Return := SenderRecursionSearch.CurrentField.Return;
          Result := False;
          Exit;
        end;
      SenderRecursionSearch.CurrentField.RHeader := SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo].RHeader;
      SenderRecursionSearch.ReturnHeader := SenderRecursionSearch.CurrentField.RHeader;
      SenderRecursionSearch.SearchBuffGo := SenderRecursionSearch.SearchBuffGo + 1;
    end
  else
      SenderRecursionSearch.ReturnHeader := SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo].RHeader;
  SenderRecursionSearch.InitPath := InitPath;
  SenderRecursionSearch.FilterName := FilterName;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_RecursionSearchNext(var SenderRecursionSearch: TTMDBRecursionSearch; var SenderTMDB: TTMDB): Boolean;
begin
  case SenderRecursionSearch.ReturnHeader.ID of
    db_Header_FieldID:
      begin
        if dbField_FindFirst(SenderRecursionSearch.FilterName, db_Header_ItemID, SenderRecursionSearch.CurrentField.RHeader.CurrentHeader, SenderTMDB.RecFile,
          SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo]) = True then
          begin
            SenderRecursionSearch.ReturnHeader := SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo].RHeader;
            SenderTMDB.Return := db_Pack_ok;
            Result := True;
            Exit;
          end;
        if dbField_FindFirst('*', db_Header_FieldID, SenderRecursionSearch.CurrentField.RHeader.CurrentHeader, SenderTMDB.RecFile,
          SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo]) = True then
          begin
            if dbField_OnlyReadFieldRec(SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo].RHeader.DataMainPOS, SenderTMDB.RecFile,
              SenderRecursionSearch.CurrentField) = False then
              begin
                SenderTMDB.Return := SenderRecursionSearch.CurrentField.Return;
                Result := False;
                Exit;
              end;
            SenderRecursionSearch.CurrentField.RHeader := SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo].RHeader;
            SenderRecursionSearch.ReturnHeader := SenderRecursionSearch.CurrentField.RHeader;
            SenderRecursionSearch.SearchBuffGo := SenderRecursionSearch.SearchBuffGo + 1;
            SenderTMDB.Return := db_Pack_ok;
            Result := True;
            Exit;
          end;

        if SenderRecursionSearch.SearchBuffGo = 0 then
          begin
            SenderTMDB.Return := db_Pack_RecursionSearchOver;
            Result := False;
            Exit;
          end;
        SenderRecursionSearch.SearchBuffGo := SenderRecursionSearch.SearchBuffGo - 1;
        while dbField_FindNext(SenderTMDB.RecFile, SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo]) = False do
          begin
            if SenderRecursionSearch.SearchBuffGo = 0 then
              begin
                SenderTMDB.Return := db_Pack_RecursionSearchOver;
                Result := False;
                Exit;
              end;
            SenderRecursionSearch.SearchBuffGo := SenderRecursionSearch.SearchBuffGo - 1;
          end;

        if dbField_OnlyReadFieldRec(SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo].RHeader.DataMainPOS, SenderTMDB.RecFile, SenderRecursionSearch.CurrentField)
          = False then
          begin
            SenderTMDB.Return := SenderRecursionSearch.CurrentField.Return;
            Result := False;
            Exit;
          end;
        SenderRecursionSearch.CurrentField.RHeader := SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo].RHeader;
        SenderRecursionSearch.ReturnHeader := SenderRecursionSearch.CurrentField.RHeader;
        SenderRecursionSearch.SearchBuffGo := SenderRecursionSearch.SearchBuffGo + 1;
        SenderTMDB.Return := db_Pack_ok;
        Result := True;
        Exit;
      end;
    db_Header_ItemID:
      begin
        if dbField_FindNext(SenderTMDB.RecFile, SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo]) = True then
          begin
            SenderRecursionSearch.ReturnHeader := SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo].RHeader;
            SenderTMDB.Return := db_Pack_ok;
            Result := True;
            Exit;
          end;
        if dbField_FindFirst('*', db_Header_FieldID, SenderRecursionSearch.CurrentField.RHeader.CurrentHeader, SenderTMDB.RecFile,
          SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo]) = True then
          begin
            if dbField_OnlyReadFieldRec(SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo].RHeader.DataMainPOS, SenderTMDB.RecFile,
              SenderRecursionSearch.CurrentField) = False then
              begin
                SenderTMDB.Return := SenderRecursionSearch.CurrentField.Return;
                Result := False;
                Exit;
              end;
            SenderRecursionSearch.CurrentField.RHeader := SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo].RHeader;
            SenderRecursionSearch.ReturnHeader := SenderRecursionSearch.CurrentField.RHeader;
            SenderRecursionSearch.SearchBuffGo := SenderRecursionSearch.SearchBuffGo + 1;
            SenderTMDB.Return := db_Pack_ok;
            Result := True;
            Exit;
          end;

        if SenderRecursionSearch.SearchBuffGo = 0 then
          begin
            SenderTMDB.Return := db_Pack_RecursionSearchOver;
            Result := False;
            Exit;
          end;
        SenderRecursionSearch.SearchBuffGo := SenderRecursionSearch.SearchBuffGo - 1;
        while dbField_FindNext(SenderTMDB.RecFile, SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo]) = False do
          begin
            if SenderRecursionSearch.SearchBuffGo = 0 then
              begin
                SenderTMDB.Return := db_Pack_RecursionSearchOver;
                Result := False;
                Exit;
              end;
            SenderRecursionSearch.SearchBuffGo := SenderRecursionSearch.SearchBuffGo - 1;
          end;

        if dbField_OnlyReadFieldRec(SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo].RHeader.DataMainPOS, SenderTMDB.RecFile, SenderRecursionSearch.CurrentField)
          = False then
          begin
            SenderTMDB.Return := SenderRecursionSearch.CurrentField.Return;
            Result := False;
            Exit;
          end;
        SenderRecursionSearch.CurrentField.RHeader := SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo].RHeader;
        SenderRecursionSearch.ReturnHeader := SenderRecursionSearch.CurrentField.RHeader;
        SenderRecursionSearch.SearchBuffGo := SenderRecursionSearch.SearchBuffGo + 1;
        SenderTMDB.Return := db_Pack_ok;
        Result := True;
        Exit;
      end;
    else
      begin
        Result := False;
      end;
  end;
end;

end.
