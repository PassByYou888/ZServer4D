{ ****************************************************************************** }
{ * Low Object DB Imp library                                                  * }
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
  2017-12-6 performance optimization
*)

unit ObjectData;

{$INCLUDE zDefine.inc}

interface

uses UnicodeMixedLib;

const
  DB_Version_Size = C_Word_Size;
  DB_Time_Size = C_Double_Size;
  DB_Counter_Size = C_Int64_Size;
  DB_DataSize_Size = C_Int64_Size;
  DB_Position_Size = C_Int64_Size;
  DB_ID_Size = C_Byte_Size;
  DB_Property_Size = C_Cardinal_Size;
  DB_Level_Size = C_Word_Size;

  db_Pack_MajorVersion = 2;
  db_Pack_MinorVersion = 1;
  db_Max_Secursion_Level = 128;

  db_Pack_FileDescription = 'ObjectDataV2.0';
  db_Pack_DefaultDescription = 'Field';

  db_Path_Delimiter = '/';

  db_String_Length = C_FixedLengthStringSize + C_FixedLengthStringSize;
  db_Header_Size = (db_String_Length * 1) + (DB_Position_Size * 4) + (DB_Time_Size * 2) + (DB_ID_Size * 2) + (DB_Property_Size * 1);
  db_Item_Size = (db_String_Length * 1) + (DB_ID_Size * 1) + (DB_Position_Size * 2) + (DB_DataSize_Size * 1) + (DB_Counter_Size * 1);
  db_Item_BlockSize = (DB_ID_Size * 1) + (DB_Position_Size * 4) + (DB_DataSize_Size * 1);
  db_Field_Size = (db_String_Length * 1) + (DB_Counter_Size * 1) + (DB_Position_Size * 3);
  db_Pack_Size = (db_String_Length * 1) + (DB_Version_Size * 2) + (DB_Time_Size * 2) + (DB_Counter_Size * 1) + (DB_Position_Size * 4) + (DB_Level_Size * 1);

  db_Header_Field_ID = 21;
  db_Header_Item_ID = 22;

  db_Header_FirstPositionFlags = 11;
  db_Header_MediumPositionFlags = 12;
  db_Header_LastPositionFlags = 13;
  db_Header_OnlyPositionFlags = 14;

  db_item_OnlyPositionFlags = 33;
  db_item_FirstPositionFlags = 34;
  db_item_MediumPositionFlags = 35;
  db_item_LastPositionFlags = 36;

type
  PHeader = ^THeader;

  THeader = record
    CurrentHeader: Int64; // nowrite
    NextHeader, PrevHeader, DataMainPOS: Int64;
    CreateTime, LastModifyTime: Double;
    ID, PositionID: Byte;
    UserProperty: Cardinal; // external define
    Name: U_String;
    Return: Integer; // nowrite
  end;

  TObjectDataHeaderWriteProc = procedure(fPos: Int64; var wVal: THeader) of object;
  TObjectDataHeaderReadProc = procedure(fPos: Int64; var rVal: THeader; var Done: Boolean) of object;

  PItemBlock = ^TItemBlock;

  TItemBlock = record
    IDFlags: Byte;
    CurrentBlockPOS, NextBlockPOS, PrevBlockPOS, DataBlockPOS: Int64;
    Size: Int64;
    Return: Integer; // nowrite
  end;

  TObjectDataItemBlockWriteProc = procedure(fPos: Int64; var wVal: TItemBlock) of object;
  TObjectDataItemBlockReadProc = procedure(fPos: Int64; var rVal: TItemBlock; var Done: Boolean) of object;

  PItem = ^TItem;

  TItem = record
    RHeader: THeader;
    Description: U_String;
    ExtID: Byte;
    FirstBlockPOS, LastBlockPOS: Int64;
    Size: Int64;
    BlockCount: Int64;
    CurrentBlockSeekPOS: Int64;
    CurrentFileSeekPOS: Int64;
    CurrentItemBlock: TItemBlock; // nowrite
    DataWrited: Boolean;          // nowrite
    Return: Integer;              // nowrite
  end;

  TObjectDataItemWriteProc = procedure(fPos: Int64; var wVal: TItem) of object;
  TObjectDataItemReadProc = procedure(fPos: Int64; var rVal: TItem; var Done: Boolean) of object;

  PField = ^TField;

  TField = record
    RHeader: THeader;
    UpLevelFieldPOS: Int64;
    Description: U_String;
    HeaderCount: Int64;
    FirstHeaderPOS, LastHeaderPOS: Int64;
    Return: Integer; // nowrite
  end;

  TObjectDataFieldWriteProc = procedure(fPos: Int64; var wVal: TField) of object;
  TObjectDataFieldReadProc = procedure(fPos: Int64; var rVal: TField; var Done: Boolean) of object;

  TFieldSearch = record
    RHeader: THeader;
    InitFlags: Boolean;
    Name: U_String;
    StartPos, OverPOS: Int64;
    ID: Byte;
    PositionID: Byte;
    Return: Integer;
  end;

  PTMDB = ^TTMDB;

  TTMDB = record
    FileDescription: U_String;
    MajorVer, MinorVer: SmallInt;
    CreateTime, LastModifyTime: Double;
    RootHeaderCount: Int64;
    DefaultFieldPOS, FirstHeaderPOS, LastHeaderPOS, CurrentFieldPOS: Int64;
    CurrentFieldLevel: Word;
    IOHnd: TIOHnd;
    OverWriteItem: Boolean;
    AllowSameHeaderName: Boolean;
    OnWriteHeader: TObjectDataHeaderWriteProc;
    OnReadHeader: TObjectDataHeaderReadProc;
    OnWriteItemBlock: TObjectDataItemBlockWriteProc;
    OnReadItemBlock: TObjectDataItemBlockReadProc;
    OnWriteItem: TObjectDataItemWriteProc;
    OnReadItem: TObjectDataItemReadProc;
    OnOnlyWriteItemRec: TObjectDataItemWriteProc;
    OnOnlyReadItemRec: TObjectDataItemReadProc;
    OnWriteField: TObjectDataFieldWriteProc;
    OnReadField: TObjectDataFieldReadProc;
    OnOnlyWriteFieldRec: TObjectDataFieldWriteProc;
    OnOnlyReadFieldRec: TObjectDataFieldReadProc;
    Return: Integer;
  end;

  TTMDBItemHandle = record
    Item: TItem;
    Path: U_String;
    Name: U_String;
    Description: U_String;
    CreateTime, LastModifyTime: Double;
    ItemExtID: Byte;
    OpenFlags: Boolean;
  end;

  TTMDBSearchHeader = record
    Name: U_String;
    ID: Byte;
    CreateTime, LastModifyTime: Double;
    HeaderPOS: Int64;
    CompleteCount: Int64;
    FieldSearch: TFieldSearch;
  end;

  TTMDBSearchItem = record
    Name: U_String;
    Description: U_String;
    ExtID: Byte;
    Size: Int64;
    HeaderPOS: Int64;
    CompleteCount: Int64;
    FieldSearch: TFieldSearch;
  end;

  TTMDBSearchField = record
    Name: U_String;
    Description: U_String;
    HeaderCount: Int64;
    HeaderPOS: Int64;
    CompleteCount: Int64;
    FieldSearch: TFieldSearch;
  end;

  TTMDBDescriptionHandle = record
    StructVarID: Byte;
    StructDescription: U_String;
    StructNextPos, StructCurrentPos, StructPublicPos: Int64;
    StructSize: Int64;
    StructPositionID: Byte;
  end;

  TTMDBItemStruct = record
    Description: U_String;
    StructCount: Int64;
    StructFirstPos, StructLastPos, ItemStructCurrentPos: Int64;
    DescriptionHandle: TTMDBDescriptionHandle;
  end;

  TTMDBRecursionSearch = record
    ReturnHeader: THeader;
    CurrentField: TField;
    InitPath: U_String;
    FilterName: U_String;
    SearchBuffGo: Integer;
    SearchBuff: array [0 .. db_Max_Secursion_Level] of TFieldSearch;
  end;

procedure Init_THeader(var SenderHeader: THeader); inline;
procedure Init_TItemBlock(var SenderItemBlock: TItemBlock); inline;
procedure Init_TItem(var SenderItem: TItem); inline;
procedure Init_TField(var SenderField: TField); inline;
procedure Init_TFieldSearch(var SenderFieldSearch: TFieldSearch); inline;
procedure Init_TTMDB(var SenderTMDB: TTMDB); inline;
procedure Init_TTMDBItemHandle(var SenderTMDBItemHandle: TTMDBItemHandle); inline;
procedure Init_TTMDBSearchHeader(var SenderTMDBSearchHeader: TTMDBSearchHeader); inline;
procedure Init_TTMDBSearchItem(var SenderTMDBSearchItem: TTMDBSearchItem); inline;
procedure Init_TTMDBSearchField(var SenderTMDBSearchField: TTMDBSearchField); inline;
procedure Init_TTMDBDescriptionHandle(var SenderTMDBDescriptionHandle: TTMDBDescriptionHandle); inline;
procedure Init_TTMDBItemStruct(var SenderTMDBItemStruct: TTMDBItemStruct); inline;
procedure Init_TTMDBRecursionSearch(var SenderTMDBRecursionSearch: TTMDBRecursionSearch); inline;

function dbHeader_WriteRec(const fPos: Int64; var IOHnd: TIOHnd; var SenderHeader: THeader): Boolean; inline;
function dbHeader_ReadRec(const fPos: Int64; var IOHnd: TIOHnd; var SenderHeader: THeader): Boolean; inline;

function dbItem_WriteRec(const fPos: Int64; var IOHnd: TIOHnd; var SenderItem: TItem): Boolean; inline;
function dbItem_ReadRec(const fPos: Int64; var IOHnd: TIOHnd; var SenderItem: TItem): Boolean; inline;

function dbField_WriteRec(const fPos: Int64; var IOHnd: TIOHnd; var SenderField: TField): Boolean; inline;
function dbField_ReadRec(const fPos: Int64; var IOHnd: TIOHnd; var SenderField: TField): Boolean; inline;

function dbItem_OnlyWriteItemBlockRec(const fPos: Int64; var IOHnd: TIOHnd; var SenderItemBlock: TItemBlock): Boolean; inline;
function dbItem_OnlyReadItemBlockRec(const fPos: Int64; var IOHnd: TIOHnd; var SenderItemBlock: TItemBlock): Boolean; inline;

function dbPack_WriteRec(const fPos: Int64; var IOHnd: TIOHnd; var SenderTMDB: TTMDB): Boolean; inline;
function dbPack_ReadRec(const fPos: Int64; var IOHnd: TIOHnd; var SenderTMDB: TTMDB): Boolean; inline;

function dbItem_OnlyWriteItemRec(const fPos: Int64; var IOHnd: TIOHnd; var SenderItem: TItem): Boolean; inline;
function dbItem_OnlyReadItemRec(const fPos: Int64; var IOHnd: TIOHnd; var SenderItem: TItem): Boolean; inline;

function dbField_OnlyWriteFieldRec(const fPos: Int64; var IOHnd: TIOHnd; var SenderField: TField): Boolean; inline;
function dbField_OnlyReadFieldRec(const fPos: Int64; var IOHnd: TIOHnd; var SenderField: TField): Boolean; inline;

function dbMultipleMatch(const SourStr, DestStr: U_String): Boolean; inline;

function dbHeader_FindNext(const Name: U_String; const FirstHeaderPOS, LastHeaderPOS: Int64; var IOHnd: TIOHnd; var SenderHeader: THeader): Boolean; { inline token }
function dbHeader_FindPrev(const Name: U_String; const LastHeaderPOS, FirstHeaderPOS: Int64; var IOHnd: TIOHnd; var SenderHeader: THeader): Boolean; { inline token }

function dbItem_BlockCreate(var IOHnd: TIOHnd; var SenderItem: TItem): Boolean;                                          { inline token }
function dbItem_BlockInit(var IOHnd: TIOHnd; var SenderItem: TItem): Boolean;                                            { inline token }
function dbItem_BlockReadData(var IOHnd: TIOHnd; var SenderItem: TItem; var Buffers; const _Size: Int64): Boolean;       { inline token }
function dbItem_BlockAppendWriteData(var IOHnd: TIOHnd; var SenderItem: TItem; var Buffers; const Size: Int64): Boolean; { inline token }
function dbItem_BlockWriteData(var IOHnd: TIOHnd; var SenderItem: TItem; var Buffers; const Size: Int64): Boolean;       { inline token }
function dbItem_BlockSeekPOS(var IOHnd: TIOHnd; var SenderItem: TItem; const Position: Int64): Boolean;                  { inline token }
function dbItem_BlockGetPOS(var IOHnd: TIOHnd; var SenderItem: TItem): Int64;                                            { inline token }
function dbItem_BlockSeekStartPOS(var IOHnd: TIOHnd; var SenderItem: TItem): Boolean;                                    { inline token }
function dbItem_BlockSeekLastPOS(var IOHnd: TIOHnd; var SenderItem: TItem): Boolean;                                     { inline token }

function dbField_GetPOSField(const fPos: Int64; var IOHnd: TIOHnd): TField; { inline token }

function dbField_GetFirstHeader(const fPos: Int64; var IOHnd: TIOHnd): THeader; { inline token }
function dbField_GetLastHeader(const fPos: Int64; var IOHnd: TIOHnd): THeader;  { inline token }

function dbField_OnlyFindFirstName(const Name: U_String; const fPos: Int64; var IOHnd: TIOHnd; var SenderFieldSearch: TFieldSearch): Boolean; { inline token }
function dbField_OnlyFindNextName(var IOHnd: TIOHnd; var SenderFieldSearch: TFieldSearch): Boolean;                                           { inline token }
function dbField_OnlyFindLastName(const Name: U_String; const fPos: Int64; var IOHnd: TIOHnd; var SenderFieldSearch: TFieldSearch): Boolean;  { inline token }
function dbField_OnlyFindPrevName(var IOHnd: TIOHnd; var SenderFieldSearch: TFieldSearch): Boolean;                                           { inline token }

function dbField_FindFirst(const Name: U_String; const ID: Byte; const fPos: Int64; var IOHnd: TIOHnd; var SenderFieldSearch: TFieldSearch): Boolean; { inline token }
function dbField_FindNext(var IOHnd: TIOHnd; var SenderFieldSearch: TFieldSearch): Boolean;                                                           { inline token }
function dbField_FindLast(const Name: U_String; const ID: Byte; const fPos: Int64; var IOHnd: TIOHnd; var SenderFieldSearch: TFieldSearch): Boolean;  { inline token }
function dbField_FindPrev(var IOHnd: TIOHnd; var SenderFieldSearch: TFieldSearch): Boolean;                                                           { inline token }

function dbField_FindFirstItem(const Name: U_String; const ItemExtID: Byte; const fPos: Int64; var IOHnd: TIOHnd; var SenderFieldSearch: TFieldSearch; var SenderItem: TItem): Boolean; overload; { inline token }
function dbField_FindNextItem(const ItemExtID: Byte; var IOHnd: TIOHnd; var SenderFieldSearch: TFieldSearch; var SenderItem: TItem): Boolean; overload;                                           { inline token }
function dbField_FindLastItem(const Name: U_String; const ItemExtID: Byte; const fPos: Int64; var IOHnd: TIOHnd; var SenderFieldSearch: TFieldSearch; var SenderItem: TItem): Boolean; overload;  { inline token }
function dbField_FindPrevItem(const ItemExtID: Byte; var IOHnd: TIOHnd; var SenderFieldSearch: TFieldSearch; var SenderItem: TItem): Boolean; overload;                                           { inline token }

function dbField_FindFirstItem(const Name: U_String; const ItemExtID: Byte; const fPos: Int64; var IOHnd: TIOHnd; var SenderFieldSearch: TFieldSearch): Boolean; overload; { inline token }
function dbField_FindNextItem(const ItemExtID: Byte; var IOHnd: TIOHnd; var SenderFieldSearch: TFieldSearch): Boolean; overload;                                           { inline token }
function dbField_FindLastItem(const Name: U_String; const ItemExtID: Byte; const fPos: Int64; var IOHnd: TIOHnd; var SenderFieldSearch: TFieldSearch): Boolean; overload;  { inline token }
function dbField_FindPrevItem(const ItemExtID: Byte; var IOHnd: TIOHnd; var SenderFieldSearch: TFieldSearch): Boolean; overload;                                           { inline token }

function dbField_ExistItem(const Name: U_String; const ItemExtID: Byte; const fPos: Int64; var IOHnd: TIOHnd): Boolean; { inline token }

function dbField_ExistHeader(const Name: U_String; const ID: Byte; const fPos: Int64; var IOHnd: TIOHnd): Boolean; { inline token }

function dbField_CreateHeader(const Name: U_String; const ID: Byte; const fPos: Int64; var IOHnd: TIOHnd; var SenderHeader: THeader): Boolean;                      { inline token }
function dbField_InsertNewHeader(const Name: U_String; const ID: Byte; const FieldPos, InsertHeaderPos: Int64; var IOHnd: TIOHnd; var NewHeader: THeader): Boolean; { inline token }

function dbField_DeleteHeader(const HeaderPOS, FieldPos: Int64; var IOHnd: TIOHnd; var SenderField: TField): Boolean;                                   { inline token }
function dbField_MoveHeader(const HeaderPOS: Int64; const SourcerFieldPOS, TargetFieldPos: Int64; var IOHnd: TIOHnd; var SenderField: TField): Boolean; { inline token }

function dbField_CreateField(const Name: U_String; const fPos: Int64; var IOHnd: TIOHnd; var SenderField: TField): Boolean;                                            { inline token }
function dbField_InsertNewField(const Name: U_String; const FieldPos, CurrentInsertPos: Int64; var IOHnd: TIOHnd; var SenderField: TField): Boolean;                   { inline token }
function dbField_CreateItem(const Name: U_String; const ExterID: Byte; const fPos: Int64; var IOHnd: TIOHnd; var SenderItem: TItem): Boolean;                          { inline token }
function dbField_InsertNewItem(const Name: U_String; const ExterID: Byte; const FieldPos, CurrentInsertPos: Int64; var IOHnd: TIOHnd; var SenderItem: TItem): Boolean; { inline token }

function dbField_CopyItem(var SenderItem: TItem; var IOHnd: TIOHnd; const DestFieldPos: Int64; var DestRecFile: TIOHnd): Boolean;                              { inline token }
function dbField_CopyItemBuffer(var SenderItem: TItem; var IOHnd: TIOHnd; var DestItemHnd: TItem; var DestRecFile: TIOHnd): Boolean;                           { inline token }
function dbField_CopyAllTo(const FilterName: U_String; const FieldPos: Int64; var IOHnd: TIOHnd; const DestFieldPos: Int64; var DestRecFile: TIOHnd): Boolean; { inline token }

function dbPack_CreatePack(const Name, Description: U_String; var SenderTMDB: TTMDB): Boolean;      { inline token }
function dbPack_OpenPack(const Name: U_String; var SenderTMDB: TTMDB; _OnlyRead: Boolean): Boolean; { inline token }

function dbPack_CreateAsStream(stream: U_Stream; const Name, Description: U_String; var SenderTMDB: TTMDB): Boolean;      { inline token }
function dbPack_OpenAsStream(stream: U_Stream; const Name: U_String; var SenderTMDB: TTMDB; _OnlyRead: Boolean): Boolean; { inline token }

function dbPack_ClosePack(var SenderTMDB: TTMDB): Boolean; { inline token }

function dbPack_CopyFieldTo(const FilterName: U_String; var SenderTMDB: TTMDB; const SourceFieldPos: Int64; var DestTMDB: TTMDB; const DestFieldPos: Int64): Boolean; { inline token }
function dbPack_CopyAllTo(var SenderTMDB: TTMDB; var DestTMDB: TTMDB): Boolean;                                                                                       { inline token }
function dbPack_CopyAllToDestPath(var SenderTMDB: TTMDB; var DestTMDB: TTMDB; destPath: U_String): Boolean;                                                           { inline token }

function dbPack_Update(var SenderTMDB: TTMDB): Boolean; { inline token }

function dbPack_TestNameStr(const Name: U_String): Boolean; { inline token }

function dbPack_AutoCheckRootField(const Name: U_String; var SenderField: TField; var SenderTMDB: TTMDB): Boolean;                 { inline token }
function dbPack_CreateRootHeader(const Name: U_String; const ID: Byte; var SenderTMDB: TTMDB; var SenderHeader: THeader): Boolean; { inline token }
function dbPack_CreateRootField(const Name, Description: U_String; var SenderTMDB: TTMDB): Boolean;                                { inline token }
function dbPack_CreateAndSetRootField(const Name, Description: U_String; var SenderTMDB: TTMDB): Boolean;                          { inline token }
function dbPack_CreateField(const pathName, Description: U_String; var SenderTMDB: TTMDB): Boolean;                                { inline token }

function dbPack_SetFieldName(const pathName, OriginFieldName, NewFieldName, FieldDescription: U_String; var SenderTMDB: TTMDB): Boolean; { inline token }
function dbPack_SetItemName(const pathName, OriginItemName, NewItemName, ItemDescription: U_String; var SenderTMDB: TTMDB): Boolean;     { inline token }
function dbPack_DeleteField(const pathName, FilterName: U_String; var SenderTMDB: TTMDB): Boolean;                                       { inline token }

function dbPack_DeleteHeader(const pathName, FilterName: U_String; const ID: Byte; var SenderTMDB: TTMDB): Boolean; { inline token }

function dbPack_MoveItem(const SourcerPathName, FilterName: U_String; const TargetPathName: U_String; const ItemExtID: Byte; var SenderTMDB: TTMDB): Boolean; { inline token }

function dbPack_MoveField(const SourcerPathName, FilterName: U_String; const TargetPathName: U_String; var SenderTMDB: TTMDB): Boolean; { inline token }

function dbPack_MoveHeader(const SourcerPathName, FilterName: U_String; const TargetPathName: U_String; const HeaderID: Byte; var SenderTMDB: TTMDB): Boolean; { inline token }

function dbPack_SetCurrentRootField(const Name: U_String; var SenderTMDB: TTMDB): Boolean; { inline token }
function dbPack_SetCurrentField(const pathName: U_String; var SenderTMDB: TTMDB): Boolean; { inline token }

function dbPack_GetRootField(const Name: U_String; var SenderField: TField; var SenderTMDB: TTMDB): Boolean;         { inline token }
function dbPack_GetField(const pathName: U_String; var SenderField: TField; var SenderTMDB: TTMDB): Boolean;         { inline token }
function dbPack_GetPath(const FieldPos, RootFieldPos: Int64; var SenderTMDB: TTMDB; var RetPath: U_String): Boolean; { inline token }

function dbPack_NewItem(const pathName, ItemName, ItemDescription: U_String; const ItemExtID: Byte; var SenderItem: TItem; var SenderTMDB: TTMDB): Boolean; { inline token }

function dbPack_DeleteItem(const pathName, FilterName: U_String; const ItemExtID: Byte; var SenderTMDB: TTMDB): Boolean; { inline token }

function dbPack_GetItem(const pathName, ItemName: U_String; const ItemExtID: Byte; var SenderItem: TItem; var SenderTMDB: TTMDB): Boolean; { inline token }

function dbPack_ItemCreate(const pathName, ItemName, ItemDescription: U_String; const ItemExtID: Byte; var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Boolean;                                      { inline token }
function dbPack_ItemFastCreate(const ItemName, ItemDescription: U_String; const fPos: Int64; const ItemExtID: Byte; var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Boolean;                         { inline token }
function dbPack_ItemFastInsertNew(const ItemName, ItemDescription: U_String; const FieldPos, InsertHeaderPos: Int64; const ItemExtID: Byte; var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Boolean; { inline token }
function dbPack_ItemOpen(const pathName, ItemName: U_String; const ItemExtID: Byte; var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Boolean;                                                         { inline token }
function dbPack_ItemFastOpen(const fPos: Int64; const ItemExtID: Byte; var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Boolean;                                                                      { inline token }
function dbPack_ItemClose(var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Boolean;                                                                                                                   { inline token }
function dbPack_ItemUpdate(var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Boolean;                                                                                                                  { inline token }
function dbPack_ItemBodyReset(var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Boolean;                                                                                                               { inline token }
function dbPack_ItemReName(const FieldPos: Int64; const NewItemName, NewItemDescription: U_String; var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Boolean;                                          { inline token }

function dbPack_ItemRead(const Size: Int64; var Buffers; var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Boolean;  { inline token }
function dbPack_ItemWrite(const Size: Int64; var Buffers; var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Boolean; { inline token }
function dbPack_ItemReadStr(var Name: U_String; var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Boolean;           { inline token }
function dbPack_ItemWriteStr(const Name: U_String; var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Boolean;        { inline token }

function dbPack_ItemSeekPos(const fPos: Int64; var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Boolean;    { inline token }
function dbPack_ItemSeekStartPos(var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Boolean;                  { inline token }
function dbPack_ItemSeekLastPos(var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Boolean;                   { inline token }
function dbPack_ItemGetPos(var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Int64;                          { inline token }
function dbPack_ItemGetSize(var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Int64;                         { inline token }
function dbPack_AppendItemSize(var SenderTMDBItemHandle: TTMDBItemHandle; const Size: Int64; var SenderTMDB: TTMDB): Boolean; { inline token }

function dbPack_ExistsRootField(const Name: U_String; var SenderTMDB: TTMDB): Boolean;                                                                      { inline token }
function dbPack_FindFirstHeader(const pathName, FilterName: U_String; const ID: Byte; var SenderSearch: TTMDBSearchHeader; var SenderTMDB: TTMDB): Boolean; { inline token }
function dbPack_FindNextHeader(var SenderSearch: TTMDBSearchHeader; var SenderTMDB: TTMDB): Boolean;                                                        { inline token }
function dbPack_FindLastHeader(const pathName, FilterName: U_String; const ID: Byte; var SenderSearch: TTMDBSearchHeader; var SenderTMDB: TTMDB): Boolean;  { inline token }
function dbPack_FindPrevHeader(var SenderSearch: TTMDBSearchHeader; var SenderTMDB: TTMDB): Boolean;                                                        { inline token }

function dbPack_FindFirstItem(const pathName, FilterName: U_String; const ItemExtID: Byte; var SenderSearch: TTMDBSearchItem; var SenderTMDB: TTMDB): Boolean; { inline token }
function dbPack_FindNextItem(var SenderSearch: TTMDBSearchItem; const ItemExtID: Byte; var SenderTMDB: TTMDB): Boolean;                                        { inline token }
function dbPack_FindLastItem(const pathName, FilterName: U_String; const ItemExtID: Byte; var SenderSearch: TTMDBSearchItem; var SenderTMDB: TTMDB): Boolean;  { inline token }
function dbPack_FindPrevItem(var SenderSearch: TTMDBSearchItem; const ItemExtID: Byte; var SenderTMDB: TTMDB): Boolean;                                        { inline token }

function dbPack_FastFindFirstItem(const FieldPos: Int64; const FilterName: U_String; const ItemExtID: Byte; var SenderSearch: TTMDBSearchItem; var SenderTMDB: TTMDB): Boolean; { inline token }
function dbPack_FastFindNextItem(var SenderSearch: TTMDBSearchItem; const ItemExtID: Byte; var SenderTMDB: TTMDB): Boolean;                                                     { inline token }
function dbPack_FastFindLastItem(const FieldPos: Int64; const FilterName: U_String; const ItemExtID: Byte; var SenderSearch: TTMDBSearchItem; var SenderTMDB: TTMDB): Boolean;  { inline token }
function dbPack_FastFindPrevItem(var SenderSearch: TTMDBSearchItem; const ItemExtID: Byte; var SenderTMDB: TTMDB): Boolean;                                                     { inline token }

function dbPack_FindFirstField(const pathName, FilterName: U_String; var SenderSearch: TTMDBSearchField; var SenderTMDB: TTMDB): Boolean; { inline token }
function dbPack_FindNextField(var SenderSearch: TTMDBSearchField; var SenderTMDB: TTMDB): Boolean;                                        { inline token }
function dbPack_FindLastField(const pathName, FilterName: U_String; var SenderSearch: TTMDBSearchField; var SenderTMDB: TTMDB): Boolean;  { inline token }
function dbPack_FindPrevField(var SenderSearch: TTMDBSearchField; var SenderTMDB: TTMDB): Boolean;                                        { inline token }

function dbPack_FastFindFirstField(const FieldPos: Int64; const FilterName: U_String; var SenderSearch: TTMDBSearchField; var SenderTMDB: TTMDB): Boolean; { inline token }
function dbPack_FastFindNextField(var SenderSearch: TTMDBSearchField; var SenderTMDB: TTMDB): Boolean;                                                     { inline token }
function dbPack_FastFindLastField(const FieldPos: Int64; const FilterName: U_String; var SenderSearch: TTMDBSearchField; var SenderTMDB: TTMDB): Boolean;  { inline token }
function dbPack_FastFindPrevField(var SenderSearch: TTMDBSearchField; var SenderTMDB: TTMDB): Boolean;                                                     { inline token }

function dbPack_RecursionSearchFirst(const InitPath, FilterName: U_String; var SenderRecursionSearch: TTMDBRecursionSearch; var SenderTMDB: TTMDB): Boolean; { inline token }
function dbPack_RecursionSearchNext(var SenderRecursionSearch: TTMDBRecursionSearch; var SenderTMDB: TTMDB): Boolean;                                        { inline token }

const
  { return code }
  db_Header_ok = 300;
  db_Header_SetPosError = -301;
  db_Header_WritePosError = -303;
  db_Header_WriteNextPosError = -304;
  db_Header_WritePrevPosError = -305;
  db_Header_WritePubMainPosError = -306;
  db_Header_WriteIDError = -307;
  db_Header_WritePositionIDError = -311;
  db_Header_WriteNameError = -308;
  db_Header_WriteCreateTimeError = -309;
  db_Header_WriteLastEditTimeError = -310;
  db_Header_WriteUserPropertyIDError = -332;
  db_Header_ReadPosError = -321;
  db_Header_ReadNextPosError = -322;
  db_Header_ReadPrevPosError = -323;
  db_Header_ReadPubMainPosError = -324;
  db_Header_ReadIDError = -325;
  db_Header_ReadPositionIDError = -312;
  db_Header_ReadNameError = -326;
  db_Header_ReadCreateTimeError = -327;
  db_Header_ReadLastEditTimeError = -328;
  db_Header_ReadUserPropertyIDError = -331;
  db_Header_NotFindHeader = -320;
  db_Item_ok = 200;
  db_Item_SetPosError = -201;
  db_Item_WriteRecDescriptionError = -204;
  db_Item_WriteRecExterIDError = -205;
  db_Item_WriteFirstBlockPOSError = -206;
  db_Item_WriteLastBlockPOSError = -207;
  db_Item_WriteRecBuffSizeError = -208;
  db_Item_WriteBlockCountError = -209;
  db_Item_ReadRecDescriptionError = -214;
  db_Item_ReadRecExterIDError = -215;
  db_Item_ReadFirstBlockPOSError = -216;
  db_Item_ReadLastBlockPOSError = -217;
  db_Item_ReadRecBuffSizeError = -218;
  db_Item_ReadBlockCountError = -219;
  db_Item_WriteItemBlockIDFlagsError = -220;
  db_Item_WriteCurrentBlockPOSError = -221;
  db_Item_WriteNextBlockPOSError = -222;
  db_Item_WritePrevBlockPOSError = -223;
  db_Item_WriteDataBlockPOSError = -224;
  db_Item_WriteDataBuffSizeError = -225;
  db_Item_ReadItemBlockIDFlagsError = -230;
  db_Item_ReadCurrentBlockPOSError = -231;
  db_Item_ReadNextBlockPOSError = -232;
  db_Item_ReadPrevBlockPOSError = -233;
  db_Item_ReadDataBlockPOSError = -234;
  db_Item_ReadDataBuffSizeError = -235;
  db_Item_BlockPositionError = -240;
  db_Item_BlockOverrate = -241;
  db_Item_BlockReadError = -242;
  db_Item_BlockWriteError = -243;
  db_Field_ok = 100;
  db_Field_SetPosError = -101;
  db_Field_WriteHeaderFieldPosError = -103;
  db_Field_WriteDescriptionError = -104;
  db_Field_WriteCountError = -106;
  db_Field_WriteFirstPosError = -107;
  db_Field_WriteLastPosError = -108;
  db_Field_ReadHeaderFieldPosError = -110;
  db_Field_ReadDescriptionError = -111;
  db_Field_ReadCountError = -112;
  db_Field_ReadFirstPosError = -113;
  db_Field_ReadLastPosError = -114;
  db_Field_NotInitSearch = -121;
  db_Field_DeleteHeaderError = -124;
  db_Pack_ok = 400;
  db_Pack_RepOpenPackError = -401;
  db_Pack_CreatePackError = -402;
  db_Pack_WriteFileDescriptionNameError = -460;
  db_Pack_WriteNameError = -403;
  db_Pack_WriteDescriptionError = -404;
  db_Pack_PositionSeekError = -405;
  db_Pack_WriteMajorVersionError = -406;
  db_Pack_WriteMinorVersionError = -407;
  db_Pack_WriteCreateTimeError = -408;
  db_Pack_WriteLastEditTimeError = -409;
  db_Pack_WriteHeaderCountError = -410;
  db_Pack_WriteDefaultPositionError = -411;
  db_Pack_WriteFirstPositionError = -412;
  db_Pack_WriteLastPositionError = -413;
  db_Pack_ReadFileDescriptionNameError = -461;
  db_Pack_ReadNameError = -414;
  db_Pack_ReadDescriptionError = -415;
  db_Pack_ReadMajorVersionError = -416;
  db_Pack_ReadMinorVersionError = -417;
  db_Pack_ReadCreateTimeError = -418;
  db_Pack_ReadLastEditTimeError = -419;
  db_Pack_ReadHeaderCountError = -420;
  db_Pack_ReadDefaultPositionError = -421;
  db_Pack_ReadFirstPositionError = -422;
  db_Pack_ReadLastPositionError = -423;
  db_Pack_RepCreatePackError = -424;
  db_Pack_OpenPackError = -425;
  db_Pack_ClosePackError = -426;
  db_Pack_WriteCurrentPositionError = -427;
  db_Pack_WriteCurrentLevelError = -428;
  db_Pack_ReadCurrentPositionError = -429;
  db_Pack_ReadCurrentLevelError = -430;
  db_Pack_PathNameError = -440;
  db_Pack_RepeatCreateItemError = -450;
  db_Pack_OpenItemError = -451;
  db_Pack_ItemNameError = -452;
  db_Pack_RepeatOpenItemError = -453;
  db_Pack_CloseItemError = -454;
  db_Pack_ItemStructNotFindDescription = -455;
  db_Pack_RecursionSearchOver = -456;
  db_FileBufferError = -500;

var
  TreeMDBHeaderNameMultipleCharacter: U_SystemString = '?';
  TreeMDBHeaderNameMultipleString: U_SystemString = '*';
  db_FieldPathLimitChar: U_SystemString = '/\';

implementation

uses PascalStrings;

function dbPack_GetPathCount(const StrName: U_String): Integer; inline;
begin
  Result := umlGetIndexStrCount(StrName, db_FieldPathLimitChar);
end;

function dbPack_DeleteFirstPath(const pathName: U_String): U_String; inline;
begin
  Result := umlDeleteFirstStr(pathName, db_FieldPathLimitChar).Text;
end;

function dbPack_DeleteLastPath(const pathName: U_String): U_String; inline;
begin
  Result := umlDeleteLastStr(pathName, db_FieldPathLimitChar).Text;
end;

function dbPack_GetFirstPath(const pathName: U_String): U_String; inline;
begin
  Result := umlGetFirstStr(pathName, db_FieldPathLimitChar).Text;
end;

function dbPack_GetLastPath(const pathName: U_String): U_String; inline;
begin
  Result := umlGetLastStr(pathName, db_FieldPathLimitChar).Text;
end;

procedure Init_THeader(var SenderHeader: THeader);
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

procedure Init_TItemBlock(var SenderItemBlock: TItemBlock);
begin
  SenderItemBlock.IDFlags := 0;
  SenderItemBlock.CurrentBlockPOS := 0;
  SenderItemBlock.NextBlockPOS := 0;
  SenderItemBlock.PrevBlockPOS := 0;
  SenderItemBlock.DataBlockPOS := 0;
  SenderItemBlock.Size := 0;
  SenderItemBlock.Return := db_Item_ok;
end;

procedure Init_TItem(var SenderItem: TItem);
begin
  Init_THeader(SenderItem.RHeader);
  SenderItem.Description := '';
  SenderItem.ExtID := 0;
  SenderItem.FirstBlockPOS := 0;
  SenderItem.LastBlockPOS := 0;
  SenderItem.Size := 0;
  SenderItem.BlockCount := 0;
  SenderItem.CurrentBlockSeekPOS := 0;
  SenderItem.CurrentFileSeekPOS := 0;
  Init_TItemBlock(SenderItem.CurrentItemBlock);
  SenderItem.DataWrited := False;
  SenderItem.Return := db_Item_ok;
end;

procedure Init_TField(var SenderField: TField);
begin
  SenderField.UpLevelFieldPOS := 0;
  SenderField.Description := '';
  SenderField.HeaderCount := 0;
  SenderField.FirstHeaderPOS := 0;
  SenderField.LastHeaderPOS := 0;
  Init_THeader(SenderField.RHeader);
  SenderField.Return := db_Field_ok;
end;

procedure Init_TFieldSearch(var SenderFieldSearch: TFieldSearch);
begin
  SenderFieldSearch.InitFlags := False;
  SenderFieldSearch.StartPos := 0;
  SenderFieldSearch.OverPOS := 0;
  SenderFieldSearch.Name := '';
  SenderFieldSearch.ID := 0;
  SenderFieldSearch.PositionID := 0;
  Init_THeader(SenderFieldSearch.RHeader);
  SenderFieldSearch.Return := db_Field_ok;
end;

procedure Init_TTMDB(var SenderTMDB: TTMDB);
begin
  SenderTMDB.FileDescription := '';
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
  InitIOHnd(SenderTMDB.IOHnd);
  SenderTMDB.IOHnd.Data := @SenderTMDB;
  SenderTMDB.OverWriteItem := True;
  SenderTMDB.AllowSameHeaderName := False;

  SenderTMDB.OnWriteHeader := nil;
  SenderTMDB.OnReadHeader := nil;
  SenderTMDB.OnWriteItemBlock := nil;
  SenderTMDB.OnReadItemBlock := nil;
  SenderTMDB.OnWriteItem := nil;
  SenderTMDB.OnReadItem := nil;
  SenderTMDB.OnOnlyWriteItemRec := nil;
  SenderTMDB.OnOnlyReadItemRec := nil;
  SenderTMDB.OnWriteField := nil;
  SenderTMDB.OnReadField := nil;
  SenderTMDB.OnOnlyWriteFieldRec := nil;
  SenderTMDB.OnOnlyReadFieldRec := nil;

  SenderTMDB.Return := db_Pack_ok;
end;

procedure Init_TTMDBItemHandle(var SenderTMDBItemHandle: TTMDBItemHandle);
begin
  Init_TItem(SenderTMDBItemHandle.Item);
  SenderTMDBItemHandle.Path := '';
  SenderTMDBItemHandle.Name := '';
  SenderTMDBItemHandle.Description := '';
  SenderTMDBItemHandle.CreateTime := 0;
  SenderTMDBItemHandle.LastModifyTime := 0;
  SenderTMDBItemHandle.ItemExtID := 0;
  SenderTMDBItemHandle.OpenFlags := False;
end;

procedure Init_TTMDBSearchHeader(var SenderTMDBSearchHeader: TTMDBSearchHeader);
begin
  SenderTMDBSearchHeader.Name := '';
  SenderTMDBSearchHeader.ID := 0;
  SenderTMDBSearchHeader.CreateTime := 0;
  SenderTMDBSearchHeader.LastModifyTime := 0;
  SenderTMDBSearchHeader.HeaderPOS := 0;
  SenderTMDBSearchHeader.CompleteCount := 0;
  Init_TFieldSearch(SenderTMDBSearchHeader.FieldSearch);
end;

procedure Init_TTMDBSearchItem(var SenderTMDBSearchItem: TTMDBSearchItem);
begin
  SenderTMDBSearchItem.Name := '';
  SenderTMDBSearchItem.Description := '';
  SenderTMDBSearchItem.ExtID := 0;
  SenderTMDBSearchItem.Size := 0;
  SenderTMDBSearchItem.HeaderPOS := 0;
  SenderTMDBSearchItem.CompleteCount := 0;
  Init_TFieldSearch(SenderTMDBSearchItem.FieldSearch);
end;

procedure Init_TTMDBSearchField(var SenderTMDBSearchField: TTMDBSearchField);
begin
  SenderTMDBSearchField.Name := '';
  SenderTMDBSearchField.Description := '';
  SenderTMDBSearchField.HeaderCount := 0;
  SenderTMDBSearchField.HeaderPOS := 0;
  SenderTMDBSearchField.CompleteCount := 0;
  Init_TFieldSearch(SenderTMDBSearchField.FieldSearch);
end;

procedure Init_TTMDBDescriptionHandle(var SenderTMDBDescriptionHandle: TTMDBDescriptionHandle);
begin
  SenderTMDBDescriptionHandle.StructVarID := 0;
  SenderTMDBDescriptionHandle.StructDescription := '';
  SenderTMDBDescriptionHandle.StructNextPos := 0;
  SenderTMDBDescriptionHandle.StructCurrentPos := 0;
  SenderTMDBDescriptionHandle.StructPublicPos := 0;
  SenderTMDBDescriptionHandle.StructSize := 0;
  SenderTMDBDescriptionHandle.StructPositionID := 0;
end;

procedure Init_TTMDBItemStruct(var SenderTMDBItemStruct: TTMDBItemStruct);
begin
  SenderTMDBItemStruct.Description := '';
  SenderTMDBItemStruct.StructCount := 0;
  SenderTMDBItemStruct.StructFirstPos := 0;
  SenderTMDBItemStruct.StructLastPos := 0;
  SenderTMDBItemStruct.ItemStructCurrentPos := 0;
  Init_TTMDBDescriptionHandle(SenderTMDBItemStruct.DescriptionHandle);
end;

procedure Init_TTMDBRecursionSearch(var SenderTMDBRecursionSearch: TTMDBRecursionSearch);
var
  i: Integer;
begin
  Init_THeader(SenderTMDBRecursionSearch.ReturnHeader);
  Init_TField(SenderTMDBRecursionSearch.CurrentField);
  SenderTMDBRecursionSearch.InitPath := '';
  SenderTMDBRecursionSearch.FilterName := '';
  SenderTMDBRecursionSearch.SearchBuffGo := 0;
  for i := 0 to db_Max_Secursion_Level do
      Init_TFieldSearch(SenderTMDBRecursionSearch.SearchBuff[i]);
end;

function dbHeader_WriteRec(const fPos: Int64; var IOHnd: TIOHnd; var SenderHeader: THeader): Boolean;
begin
  if umlFileSeek(IOHnd, fPos) = False then
    begin
      SenderHeader.Return := db_Header_SetPosError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(IOHnd, DB_Position_Size, SenderHeader.NextHeader) = False then
    begin
      SenderHeader.Return := db_Header_WriteNextPosError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(IOHnd, DB_Position_Size, SenderHeader.PrevHeader) = False then
    begin
      SenderHeader.Return := db_Header_WritePrevPosError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(IOHnd, DB_Position_Size, SenderHeader.DataMainPOS) = False then
    begin
      SenderHeader.Return := db_Header_WritePubMainPosError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(IOHnd, DB_Time_Size, SenderHeader.CreateTime) = False then
    begin
      SenderHeader.Return := db_Header_WriteCreateTimeError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(IOHnd, DB_Time_Size, SenderHeader.LastModifyTime) = False then
    begin
      SenderHeader.Return := db_Header_WriteLastEditTimeError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(IOHnd, DB_ID_Size, SenderHeader.ID) = False then
    begin
      SenderHeader.Return := db_Header_WriteIDError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(IOHnd, DB_ID_Size, SenderHeader.PositionID) = False then
    begin
      SenderHeader.Return := db_Header_WritePositionIDError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(IOHnd, DB_Property_Size, SenderHeader.UserProperty) = False then
    begin
      SenderHeader.Return := db_Header_WriteUserPropertyIDError;
      Result := False;
      Exit;
    end;
  if umlFileWriteStr(IOHnd, SenderHeader.Name) = False then
    begin
      SenderHeader.Return := db_Header_WriteNameError;
      Result := False;
      Exit;
    end;

  SenderHeader.Return := db_Header_ok;
  Result := True;

  if IOHnd.Data <> nil then
    if Assigned(PTMDB(IOHnd.Data)^.OnWriteHeader) then
        PTMDB(IOHnd.Data)^.OnWriteHeader(fPos, SenderHeader);
end;

function dbHeader_ReadRec(const fPos: Int64; var IOHnd: TIOHnd; var SenderHeader: THeader): Boolean;
begin
  if IOHnd.Data <> nil then
    if Assigned(PTMDB(IOHnd.Data)^.OnReadHeader) then
      begin
        Result := False;
        PTMDB(IOHnd.Data)^.OnReadHeader(fPos, SenderHeader, Result);
        if Result then
            Exit;
      end;

  if umlFileSeek(IOHnd, fPos) = False then
    begin
      SenderHeader.Return := db_Header_SetPosError;
      Result := False;
      Exit;
    end;

  SenderHeader.CurrentHeader := fPos;

  if umlFileRead(IOHnd, DB_Position_Size, SenderHeader.NextHeader) = False then
    begin
      SenderHeader.Return := db_Header_ReadNextPosError;
      Result := False;
      Exit;
    end;
  if umlFileRead(IOHnd, DB_Position_Size, SenderHeader.PrevHeader) = False then
    begin
      SenderHeader.Return := db_Header_ReadPrevPosError;
      Result := False;
      Exit;
    end;
  if umlFileRead(IOHnd, DB_Position_Size, SenderHeader.DataMainPOS) = False then
    begin
      SenderHeader.Return := db_Header_ReadPubMainPosError;
      Result := False;
      Exit;
    end;
  if umlFileRead(IOHnd, DB_Time_Size, SenderHeader.CreateTime) = False then
    begin
      SenderHeader.Return := db_Header_ReadCreateTimeError;
      Result := False;
      Exit;
    end;
  if umlFileRead(IOHnd, DB_Time_Size, SenderHeader.LastModifyTime) = False then
    begin
      SenderHeader.Return := db_Header_ReadLastEditTimeError;
      Result := False;
      Exit;
    end;
  if umlFileRead(IOHnd, DB_ID_Size, SenderHeader.ID) = False then
    begin
      SenderHeader.Return := db_Header_ReadIDError;
      Result := False;
      Exit;
    end;
  if umlFileRead(IOHnd, DB_ID_Size, SenderHeader.PositionID) = False then
    begin
      SenderHeader.Return := db_Header_ReadPositionIDError;
      Result := False;
      Exit;
    end;
  if umlFileRead(IOHnd, DB_Property_Size, SenderHeader.UserProperty) = False then
    begin
      SenderHeader.Return := db_Header_ReadUserPropertyIDError;
      Result := False;
      Exit;
    end;
  if umlFileReadStr(IOHnd, SenderHeader.Name) = False then
    begin
      SenderHeader.Return := db_Header_ReadNameError;
      Result := False;
      Exit;
    end;

  SenderHeader.Return := db_Header_ok;
  Result := True;

  if IOHnd.Data <> nil then
    if Assigned(PTMDB(IOHnd.Data)^.OnWriteHeader) then
        PTMDB(IOHnd.Data)^.OnWriteHeader(fPos, SenderHeader);
end;

function dbItem_WriteRec(const fPos: Int64; var IOHnd: TIOHnd; var SenderItem: TItem): Boolean;
begin
  if dbHeader_WriteRec(fPos, IOHnd, SenderItem.RHeader) = False then
    begin
      SenderItem.Return := SenderItem.RHeader.Return;
      Result := False;
      Exit;
    end;
  if umlFileSeek(IOHnd, SenderItem.RHeader.DataMainPOS) = False then
    begin
      SenderItem.Return := db_Item_SetPosError;
      Result := False;
      Exit;
    end;
  if umlFileWriteStr(IOHnd, SenderItem.Description) = False then
    begin
      SenderItem.Return := db_Item_WriteRecDescriptionError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(IOHnd, DB_ID_Size, SenderItem.ExtID) = False then
    begin
      SenderItem.Return := db_Item_WriteRecExterIDError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(IOHnd, DB_Position_Size, SenderItem.FirstBlockPOS) = False then
    begin
      SenderItem.Return := db_Item_WriteFirstBlockPOSError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(IOHnd, DB_Position_Size, SenderItem.LastBlockPOS) = False then
    begin
      SenderItem.Return := db_Item_WriteLastBlockPOSError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(IOHnd, DB_DataSize_Size, SenderItem.Size) = False then
    begin
      SenderItem.Return := db_Item_WriteRecBuffSizeError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(IOHnd, DB_Counter_Size, SenderItem.BlockCount) = False then
    begin
      SenderItem.Return := db_Item_WriteBlockCountError;
      Result := False;
      Exit;
    end;
  SenderItem.Return := db_Item_ok;
  Result := True;

  if IOHnd.Data <> nil then
    if Assigned(PTMDB(IOHnd.Data)^.OnWriteItem) then
        PTMDB(IOHnd.Data)^.OnWriteItem(fPos, SenderItem);
end;

function dbItem_ReadRec(const fPos: Int64; var IOHnd: TIOHnd; var SenderItem: TItem): Boolean;
begin
  if IOHnd.Data <> nil then
    if Assigned(PTMDB(IOHnd.Data)^.OnReadItem) then
      begin
        Result := False;
        PTMDB(IOHnd.Data)^.OnReadItem(fPos, SenderItem, Result);
        if Result then
            Exit;
      end;

  if dbHeader_ReadRec(fPos, IOHnd, SenderItem.RHeader) = False then
    begin
      SenderItem.Return := SenderItem.RHeader.Return;
      Result := False;
      Exit;
    end;
  if umlFileSeek(IOHnd, SenderItem.RHeader.DataMainPOS) = False then
    begin
      SenderItem.Return := db_Item_SetPosError;
      Result := False;
      Exit;
    end;
  if umlFileReadStr(IOHnd, SenderItem.Description) = False then
    begin
      SenderItem.Return := db_Item_ReadRecDescriptionError;
      Result := False;
      Exit;
    end;
  if umlFileRead(IOHnd, DB_ID_Size, SenderItem.ExtID) = False then
    begin
      SenderItem.Return := db_Item_ReadRecExterIDError;
      Result := False;
      Exit;
    end;
  if umlFileRead(IOHnd, DB_Position_Size, SenderItem.FirstBlockPOS) = False then
    begin
      SenderItem.Return := db_Item_ReadFirstBlockPOSError;
      Result := False;
      Exit;
    end;
  if umlFileRead(IOHnd, DB_Position_Size, SenderItem.LastBlockPOS) = False then
    begin
      SenderItem.Return := db_Item_ReadLastBlockPOSError;
      Result := False;
      Exit;
    end;
  if umlFileRead(IOHnd, DB_DataSize_Size, SenderItem.Size) = False then
    begin
      SenderItem.Return := db_Item_ReadRecBuffSizeError;
      Result := False;
      Exit;
    end;
  if umlFileRead(IOHnd, DB_Counter_Size, SenderItem.BlockCount) = False then
    begin
      SenderItem.Return := db_Item_ReadBlockCountError;
      Result := False;
      Exit;
    end;
  SenderItem.Return := db_Item_ok;
  Result := True;

  if IOHnd.Data <> nil then
    if Assigned(PTMDB(IOHnd.Data)^.OnWriteItem) then
        PTMDB(IOHnd.Data)^.OnWriteItem(fPos, SenderItem);
end;

function dbField_WriteRec(const fPos: Int64; var IOHnd: TIOHnd; var SenderField: TField): Boolean;
begin
  if dbHeader_WriteRec(fPos, IOHnd, SenderField.RHeader) = False then
    begin
      SenderField.Return := SenderField.RHeader.Return;
      Result := False;
      Exit;
    end;
  if umlFileSeek(IOHnd, SenderField.RHeader.DataMainPOS) = False then
    begin
      SenderField.Return := db_Field_SetPosError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(IOHnd, DB_Position_Size, SenderField.UpLevelFieldPOS) = False then
    begin
      SenderField.Return := db_Field_WriteHeaderFieldPosError;
      Result := False;
      Exit;
    end;
  if umlFileWriteStr(IOHnd, SenderField.Description) = False then
    begin
      SenderField.Return := db_Field_WriteDescriptionError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(IOHnd, DB_Counter_Size, SenderField.HeaderCount) = False then
    begin
      SenderField.Return := db_Field_WriteCountError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(IOHnd, DB_Position_Size, SenderField.FirstHeaderPOS) = False then
    begin
      SenderField.Return := db_Field_WriteFirstPosError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(IOHnd, DB_Position_Size, SenderField.LastHeaderPOS) = False then
    begin
      SenderField.Return := db_Field_WriteLastPosError;
      Result := False;
      Exit;
    end;
  SenderField.Return := db_Field_ok;
  Result := True;

  if IOHnd.Data <> nil then
    if Assigned(PTMDB(IOHnd.Data)^.OnWriteField) then
        PTMDB(IOHnd.Data)^.OnWriteField(fPos, SenderField);
end;

function dbField_ReadRec(const fPos: Int64; var IOHnd: TIOHnd; var SenderField: TField): Boolean;
begin
  if IOHnd.Data <> nil then
    if Assigned(PTMDB(IOHnd.Data)^.OnReadField) then
      begin
        Result := False;
        PTMDB(IOHnd.Data)^.OnReadField(fPos, SenderField, Result);
        if Result then
            Exit;
      end;

  if dbHeader_ReadRec(fPos, IOHnd, SenderField.RHeader) = False then
    begin
      SenderField.Return := SenderField.RHeader.Return;
      Result := False;
      Exit;
    end;
  if umlFileSeek(IOHnd, SenderField.RHeader.DataMainPOS) = False then
    begin
      SenderField.Return := db_Field_SetPosError;
      Result := False;
      Exit;
    end;
  if umlFileRead(IOHnd, DB_Position_Size, SenderField.UpLevelFieldPOS) = False then
    begin
      SenderField.Return := db_Field_ReadHeaderFieldPosError;
      Result := False;
      Exit;
    end;
  if umlFileReadStr(IOHnd, SenderField.Description) = False then
    begin
      SenderField.Return := db_Field_ReadDescriptionError;
      Result := False;
      Exit;
    end;
  if umlFileRead(IOHnd, DB_Counter_Size, SenderField.HeaderCount) = False then
    begin
      SenderField.Return := db_Field_ReadCountError;
      Result := False;
      Exit;
    end;
  if umlFileRead(IOHnd, DB_Position_Size, SenderField.FirstHeaderPOS) = False then
    begin
      SenderField.Return := db_Field_ReadFirstPosError;
      Result := False;
      Exit;
    end;
  if umlFileRead(IOHnd, DB_Position_Size, SenderField.LastHeaderPOS) = False then
    begin
      SenderField.Return := db_Field_ReadLastPosError;
      Result := False;
      Exit;
    end;
  SenderField.Return := db_Field_ok;
  Result := True;

  if IOHnd.Data <> nil then
    if Assigned(PTMDB(IOHnd.Data)^.OnWriteField) then
        PTMDB(IOHnd.Data)^.OnWriteField(fPos, SenderField);
end;

function dbItem_OnlyWriteItemBlockRec(const fPos: Int64; var IOHnd: TIOHnd; var SenderItemBlock: TItemBlock): Boolean;
begin
  if umlFileSeek(IOHnd, fPos) = False then
    begin
      SenderItemBlock.Return := db_Item_SetPosError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(IOHnd, DB_ID_Size, SenderItemBlock.IDFlags) = False then
    begin
      SenderItemBlock.Return := db_Item_WriteItemBlockIDFlagsError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(IOHnd, DB_Position_Size, SenderItemBlock.CurrentBlockPOS) = False then
    begin
      SenderItemBlock.Return := db_Item_WriteCurrentBlockPOSError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(IOHnd, DB_Position_Size, SenderItemBlock.NextBlockPOS) = False then
    begin
      SenderItemBlock.Return := db_Item_WriteNextBlockPOSError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(IOHnd, DB_Position_Size, SenderItemBlock.PrevBlockPOS) = False then
    begin
      SenderItemBlock.Return := db_Item_WritePrevBlockPOSError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(IOHnd, DB_Position_Size, SenderItemBlock.DataBlockPOS) = False then
    begin
      SenderItemBlock.Return := db_Item_WriteDataBlockPOSError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(IOHnd, DB_DataSize_Size, SenderItemBlock.Size) = False then
    begin
      SenderItemBlock.Return := db_Item_WriteDataBuffSizeError;
      Result := False;
      Exit;
    end;
  SenderItemBlock.Return := db_Item_ok;
  Result := True;

  if IOHnd.Data <> nil then
    if Assigned(PTMDB(IOHnd.Data)^.OnWriteItemBlock) then
        PTMDB(IOHnd.Data)^.OnWriteItemBlock(fPos, SenderItemBlock);
end;

function dbItem_OnlyReadItemBlockRec(const fPos: Int64; var IOHnd: TIOHnd; var SenderItemBlock: TItemBlock): Boolean;
begin
  if IOHnd.Data <> nil then
    if Assigned(PTMDB(IOHnd.Data)^.OnReadItemBlock) then
      begin
        Result := False;
        PTMDB(IOHnd.Data)^.OnReadItemBlock(fPos, SenderItemBlock, Result);
        if Result then
            Exit;
      end;

  if umlFileSeek(IOHnd, fPos) = False then
    begin
      SenderItemBlock.Return := db_Item_SetPosError;
      Result := False;
      Exit;
    end;
  if umlFileRead(IOHnd, DB_ID_Size, SenderItemBlock.IDFlags) = False then
    begin
      SenderItemBlock.Return := db_Item_ReadItemBlockIDFlagsError;
      Result := False;
      Exit;
    end;
  if umlFileRead(IOHnd, DB_Position_Size, SenderItemBlock.CurrentBlockPOS) = False then
    begin
      SenderItemBlock.Return := db_Item_ReadCurrentBlockPOSError;
      Result := False;
      Exit;
    end;
  if umlFileRead(IOHnd, DB_Position_Size, SenderItemBlock.NextBlockPOS) = False then
    begin
      SenderItemBlock.Return := db_Item_ReadNextBlockPOSError;
      Result := False;
      Exit;
    end;
  if umlFileRead(IOHnd, DB_Position_Size, SenderItemBlock.PrevBlockPOS) = False then
    begin
      SenderItemBlock.Return := db_Item_ReadPrevBlockPOSError;
      Result := False;
      Exit;
    end;
  if umlFileRead(IOHnd, DB_Position_Size, SenderItemBlock.DataBlockPOS) = False then
    begin
      SenderItemBlock.Return := db_Item_ReadDataBlockPOSError;
      Result := False;
      Exit;
    end;
  if umlFileRead(IOHnd, DB_DataSize_Size, SenderItemBlock.Size) = False then
    begin
      SenderItemBlock.Return := db_Item_ReadDataBuffSizeError;
      Result := False;
      Exit;
    end;
  SenderItemBlock.Return := db_Item_ok;
  Result := True;

  if IOHnd.Data <> nil then
    if Assigned(PTMDB(IOHnd.Data)^.OnWriteItemBlock) then
        PTMDB(IOHnd.Data)^.OnWriteItemBlock(fPos, SenderItemBlock);
end;

function dbPack_WriteRec(const fPos: Int64; var IOHnd: TIOHnd; var SenderTMDB: TTMDB): Boolean;
begin
  if umlFileSeek(IOHnd, fPos) = False then
    begin
      SenderTMDB.Return := db_Pack_PositionSeekError;
      Result := False;
      Exit;
    end;
  SenderTMDB.FileDescription := db_Pack_FileDescription;
  if umlFileWriteStr(IOHnd, SenderTMDB.FileDescription) = False then
    begin
      SenderTMDB.Return := db_Pack_WriteFileDescriptionNameError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(IOHnd, DB_Version_Size, SenderTMDB.MajorVer) = False then
    begin
      SenderTMDB.Return := db_Pack_WriteMajorVersionError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(IOHnd, DB_Version_Size, SenderTMDB.MinorVer) = False then
    begin
      SenderTMDB.Return := db_Pack_WriteMinorVersionError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(IOHnd, DB_Time_Size, SenderTMDB.CreateTime) = False then
    begin
      SenderTMDB.Return := db_Pack_WriteCreateTimeError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(IOHnd, DB_Time_Size, SenderTMDB.LastModifyTime) = False then
    begin
      SenderTMDB.Return := db_Pack_WriteLastEditTimeError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(IOHnd, DB_Counter_Size, SenderTMDB.RootHeaderCount) = False then
    begin
      SenderTMDB.Return := db_Pack_WriteHeaderCountError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(IOHnd, DB_Position_Size, SenderTMDB.DefaultFieldPOS) = False then
    begin
      SenderTMDB.Return := db_Pack_WriteDefaultPositionError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(IOHnd, DB_Position_Size, SenderTMDB.FirstHeaderPOS) = False then
    begin
      SenderTMDB.Return := db_Pack_WriteFirstPositionError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(IOHnd, DB_Position_Size, SenderTMDB.LastHeaderPOS) = False then
    begin
      SenderTMDB.Return := db_Pack_WriteLastPositionError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(IOHnd, DB_Position_Size, SenderTMDB.CurrentFieldPOS) = False then
    begin
      SenderTMDB.Return := db_Pack_WriteCurrentPositionError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(IOHnd, DB_Level_Size, SenderTMDB.CurrentFieldLevel) = False then
    begin
      SenderTMDB.Return := db_Pack_WriteCurrentLevelError;
      Result := False;
      Exit;
    end;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_ReadRec(const fPos: Int64; var IOHnd: TIOHnd; var SenderTMDB: TTMDB): Boolean;
begin
  if umlFileSeek(IOHnd, fPos) = False then
    begin
      SenderTMDB.Return := db_Pack_PositionSeekError;
      Result := False;
      Exit;
    end;
  if umlFileReadStr(IOHnd, SenderTMDB.FileDescription) = False then
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
  if umlFileRead(IOHnd, DB_Version_Size, SenderTMDB.MajorVer) = False then
    begin
      SenderTMDB.Return := db_Pack_ReadMajorVersionError;
      Result := False;
      Exit;
    end;
  if umlFileRead(IOHnd, DB_Version_Size, SenderTMDB.MinorVer) = False then
    begin
      SenderTMDB.Return := db_Pack_ReadMinorVersionError;
      Result := False;
      Exit;
    end;
  if umlFileRead(IOHnd, DB_Time_Size, SenderTMDB.CreateTime) = False then
    begin
      SenderTMDB.Return := db_Pack_ReadCreateTimeError;
      Result := False;
      Exit;
    end;
  if umlFileRead(IOHnd, DB_Time_Size, SenderTMDB.LastModifyTime) = False then
    begin
      SenderTMDB.Return := db_Pack_ReadLastEditTimeError;
      Result := False;
      Exit;
    end;
  if umlFileRead(IOHnd, DB_Counter_Size, SenderTMDB.RootHeaderCount) = False then
    begin
      SenderTMDB.Return := db_Pack_ReadHeaderCountError;
      Result := False;
      Exit;
    end;
  if umlFileRead(IOHnd, DB_Position_Size, SenderTMDB.DefaultFieldPOS) = False then
    begin
      SenderTMDB.Return := db_Pack_ReadDefaultPositionError;
      Result := False;
      Exit;
    end;
  if umlFileRead(IOHnd, DB_Position_Size, SenderTMDB.FirstHeaderPOS) = False then
    begin
      SenderTMDB.Return := db_Pack_ReadFirstPositionError;
      Result := False;
      Exit;
    end;
  if umlFileRead(IOHnd, DB_Position_Size, SenderTMDB.LastHeaderPOS) = False then
    begin
      SenderTMDB.Return := db_Pack_ReadLastPositionError;
      Result := False;
      Exit;
    end;
  if umlFileRead(IOHnd, DB_Position_Size, SenderTMDB.CurrentFieldPOS) = False then
    begin
      SenderTMDB.Return := db_Pack_ReadCurrentPositionError;
      Result := False;
      Exit;
    end;
  if umlFileRead(IOHnd, DB_Level_Size, SenderTMDB.CurrentFieldLevel) = False then
    begin
      SenderTMDB.Return := db_Pack_ReadCurrentLevelError;
      Result := False;
      Exit;
    end;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbItem_OnlyWriteItemRec(const fPos: Int64; var IOHnd: TIOHnd; var SenderItem: TItem): Boolean;
begin
  if umlFileSeek(IOHnd, fPos) = False then
    begin
      SenderItem.Return := db_Item_SetPosError;
      Result := False;
      Exit;
    end;
  if umlFileWriteStr(IOHnd, SenderItem.Description) = False then
    begin
      SenderItem.Return := db_Item_WriteRecDescriptionError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(IOHnd, DB_ID_Size, SenderItem.ExtID) = False then
    begin
      SenderItem.Return := db_Item_WriteRecExterIDError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(IOHnd, DB_Position_Size, SenderItem.FirstBlockPOS) = False then
    begin
      SenderItem.Return := db_Item_WriteFirstBlockPOSError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(IOHnd, DB_Position_Size, SenderItem.LastBlockPOS) = False then
    begin
      SenderItem.Return := db_Item_WriteLastBlockPOSError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(IOHnd, DB_DataSize_Size, SenderItem.Size) = False then
    begin
      SenderItem.Return := db_Item_WriteRecBuffSizeError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(IOHnd, DB_Counter_Size, SenderItem.BlockCount) = False then
    begin
      SenderItem.Return := db_Item_WriteBlockCountError;
      Result := False;
      Exit;
    end;
  SenderItem.Return := db_Item_ok;
  Result := True;

  if IOHnd.Data <> nil then
    if Assigned(PTMDB(IOHnd.Data)^.OnOnlyWriteItemRec) then
        PTMDB(IOHnd.Data)^.OnOnlyWriteItemRec(fPos, SenderItem);
end;

function dbItem_OnlyReadItemRec(const fPos: Int64; var IOHnd: TIOHnd; var SenderItem: TItem): Boolean;
begin
  if IOHnd.Data <> nil then
    if Assigned(PTMDB(IOHnd.Data)^.OnOnlyReadItemRec) then
      begin
        Result := False;
        PTMDB(IOHnd.Data)^.OnOnlyReadItemRec(fPos, SenderItem, Result);
        if Result then
            Exit;
      end;

  if umlFileSeek(IOHnd, fPos) = False then
    begin
      SenderItem.Return := db_Item_SetPosError;
      Result := False;
      Exit;
    end;
  if umlFileReadStr(IOHnd, SenderItem.Description) = False then
    begin
      SenderItem.Return := db_Item_ReadRecDescriptionError;
      Result := False;
      Exit;
    end;
  if umlFileRead(IOHnd, DB_ID_Size, SenderItem.ExtID) = False then
    begin
      SenderItem.Return := db_Item_ReadRecExterIDError;
      Result := False;
      Exit;
    end;
  if umlFileRead(IOHnd, DB_Position_Size, SenderItem.FirstBlockPOS) = False then
    begin
      SenderItem.Return := db_Item_ReadFirstBlockPOSError;
      Result := False;
      Exit;
    end;
  if umlFileRead(IOHnd, DB_Position_Size, SenderItem.LastBlockPOS) = False then
    begin
      SenderItem.Return := db_Item_ReadLastBlockPOSError;
      Result := False;
      Exit;
    end;
  if umlFileRead(IOHnd, DB_DataSize_Size, SenderItem.Size) = False then
    begin
      SenderItem.Return := db_Item_ReadRecBuffSizeError;
      Result := False;
      Exit;
    end;
  if umlFileRead(IOHnd, DB_Counter_Size, SenderItem.BlockCount) = False then
    begin
      SenderItem.Return := db_Item_ReadBlockCountError;
      Result := False;
      Exit;
    end;
  SenderItem.Return := db_Item_ok;
  Result := True;

  if IOHnd.Data <> nil then
    if Assigned(PTMDB(IOHnd.Data)^.OnOnlyWriteItemRec) then
        PTMDB(IOHnd.Data)^.OnOnlyWriteItemRec(fPos, SenderItem);
end;

function dbField_OnlyWriteFieldRec(const fPos: Int64; var IOHnd: TIOHnd; var SenderField: TField): Boolean;
begin
  if umlFileSeek(IOHnd, fPos) = False then
    begin
      SenderField.Return := db_Field_SetPosError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(IOHnd, DB_Position_Size, SenderField.UpLevelFieldPOS) = False then
    begin
      SenderField.Return := db_Field_WriteHeaderFieldPosError;
      Result := False;
      Exit;
    end;
  if umlFileWriteStr(IOHnd, SenderField.Description) = False then
    begin
      SenderField.Return := db_Field_WriteDescriptionError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(IOHnd, DB_Counter_Size, SenderField.HeaderCount) = False then
    begin
      SenderField.Return := db_Field_WriteCountError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(IOHnd, DB_Position_Size, SenderField.FirstHeaderPOS) = False then
    begin
      SenderField.Return := db_Field_WriteFirstPosError;
      Result := False;
      Exit;
    end;
  if umlFileWrite(IOHnd, DB_Position_Size, SenderField.LastHeaderPOS) = False then
    begin
      SenderField.Return := db_Field_WriteLastPosError;
      Result := False;
      Exit;
    end;
  SenderField.Return := db_Field_ok;
  Result := True;

  if IOHnd.Data <> nil then
    if Assigned(PTMDB(IOHnd.Data)^.OnOnlyWriteFieldRec) then
        PTMDB(IOHnd.Data)^.OnOnlyWriteFieldRec(fPos, SenderField);
end;

function dbField_OnlyReadFieldRec(const fPos: Int64; var IOHnd: TIOHnd; var SenderField: TField): Boolean;
begin
  if IOHnd.Data <> nil then
    if Assigned(PTMDB(IOHnd.Data)^.OnOnlyReadFieldRec) then
      begin
        Result := False;
        PTMDB(IOHnd.Data)^.OnOnlyReadFieldRec(fPos, SenderField, Result);
        if Result then
            Exit;
      end;

  if umlFileSeek(IOHnd, fPos) = False then
    begin
      SenderField.Return := db_Field_SetPosError;
      Result := False;
      Exit;
    end;
  if umlFileRead(IOHnd, DB_Position_Size, SenderField.UpLevelFieldPOS) = False then
    begin
      SenderField.Return := db_Field_ReadHeaderFieldPosError;
      Result := False;
      Exit;
    end;
  if umlFileReadStr(IOHnd, SenderField.Description) = False then
    begin
      SenderField.Return := db_Field_ReadDescriptionError;
      Result := False;
      Exit;
    end;
  if umlFileRead(IOHnd, DB_Counter_Size, SenderField.HeaderCount) = False then
    begin
      SenderField.Return := db_Field_ReadCountError;
      Result := False;
      Exit;
    end;
  if umlFileRead(IOHnd, DB_Position_Size, SenderField.FirstHeaderPOS) = False then
    begin
      SenderField.Return := db_Field_ReadFirstPosError;
      Result := False;
      Exit;
    end;
  if umlFileRead(IOHnd, DB_Position_Size, SenderField.LastHeaderPOS) = False then
    begin
      SenderField.Return := db_Field_ReadLastPosError;
      Result := False;
      Exit;
    end;
  SenderField.Return := db_Field_ok;
  Result := True;

  if IOHnd.Data <> nil then
    if Assigned(PTMDB(IOHnd.Data)^.OnOnlyWriteFieldRec) then
        PTMDB(IOHnd.Data)^.OnOnlyWriteFieldRec(fPos, SenderField);
end;

function dbMultipleMatch(const SourStr, DestStr: U_String): Boolean;
begin
  if SourStr.Len = 0 then
      Result := True
  else if DestStr.Len = 0 then
      Result := False
  else
      Result := umlMultipleMatch(True, SourStr, DestStr, TreeMDBHeaderNameMultipleString, TreeMDBHeaderNameMultipleCharacter);
end;

function dbHeader_FindNext(const Name: U_String; const FirstHeaderPOS, LastHeaderPOS: Int64; var IOHnd: TIOHnd; var SenderHeader: THeader): Boolean;
begin
  if dbHeader_ReadRec(FirstHeaderPOS, IOHnd, SenderHeader) = False then
    begin
      Result := False;
      Exit;
    end;
  if dbMultipleMatch(Name, SenderHeader.Name) then
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
  while dbHeader_ReadRec(SenderHeader.NextHeader, IOHnd, SenderHeader) do
    begin
      if dbMultipleMatch(Name, SenderHeader.Name) then
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

function dbHeader_FindPrev(const Name: U_String; const LastHeaderPOS, FirstHeaderPOS: Int64; var IOHnd: TIOHnd; var SenderHeader: THeader): Boolean;
begin
  if dbHeader_ReadRec(LastHeaderPOS, IOHnd, SenderHeader) = False then
    begin
      Result := False;
      Exit;
    end;
  if dbMultipleMatch(Name, SenderHeader.Name) then
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
  while dbHeader_ReadRec(SenderHeader.PrevHeader, IOHnd, SenderHeader) do
    begin
      if dbMultipleMatch(Name, SenderHeader.Name) then
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

function dbItem_BlockCreate(var IOHnd: TIOHnd; var SenderItem: TItem): Boolean;
var
  FirstItemBlock, LastItemBlock: TItemBlock;
begin
  case SenderItem.BlockCount of
    0:
      begin
        LastItemBlock.IDFlags := db_item_OnlyPositionFlags;
        LastItemBlock.CurrentBlockPOS := umlFileGetSize(IOHnd);
        LastItemBlock.NextBlockPOS := LastItemBlock.CurrentBlockPOS;
        LastItemBlock.PrevBlockPOS := LastItemBlock.CurrentBlockPOS;
        LastItemBlock.DataBlockPOS := LastItemBlock.CurrentBlockPOS + db_Item_BlockSize;
        LastItemBlock.Size := 0;
        if dbItem_OnlyWriteItemBlockRec(LastItemBlock.CurrentBlockPOS, IOHnd, LastItemBlock) = False then
          begin
            SenderItem.Return := LastItemBlock.Return;
            Result := False;
            Exit;
          end;
        SenderItem.BlockCount := 1;
        SenderItem.FirstBlockPOS := LastItemBlock.CurrentBlockPOS;
        SenderItem.LastBlockPOS := LastItemBlock.CurrentBlockPOS;
        if dbItem_OnlyWriteItemRec(SenderItem.RHeader.DataMainPOS, IOHnd, SenderItem) = False then
          begin
            Result := False;
            Exit;
          end;
      end;
    1:
      begin
        if dbItem_OnlyReadItemBlockRec(SenderItem.FirstBlockPOS, IOHnd, FirstItemBlock) = False then
          begin
            SenderItem.Return := FirstItemBlock.Return;
            Result := False;
            Exit;
          end;
        LastItemBlock.IDFlags := db_item_LastPositionFlags;
        LastItemBlock.CurrentBlockPOS := umlFileGetSize(IOHnd);
        LastItemBlock.NextBlockPOS := FirstItemBlock.CurrentBlockPOS;
        LastItemBlock.PrevBlockPOS := FirstItemBlock.CurrentBlockPOS;
        LastItemBlock.DataBlockPOS := LastItemBlock.CurrentBlockPOS + db_Item_BlockSize;
        LastItemBlock.Size := 0;
        if dbItem_OnlyWriteItemBlockRec(LastItemBlock.CurrentBlockPOS, IOHnd, LastItemBlock) = False then
          begin
            SenderItem.Return := LastItemBlock.Return;
            Result := False;
            Exit;
          end;
        FirstItemBlock.IDFlags := db_item_FirstPositionFlags;
        FirstItemBlock.NextBlockPOS := LastItemBlock.CurrentBlockPOS;
        FirstItemBlock.PrevBlockPOS := LastItemBlock.CurrentBlockPOS;
        if dbItem_OnlyWriteItemBlockRec(SenderItem.FirstBlockPOS, IOHnd, FirstItemBlock) = False then
          begin
            SenderItem.Return := FirstItemBlock.Return;
            Result := False;
            Exit;
          end;
        SenderItem.BlockCount := SenderItem.BlockCount + 1;
        SenderItem.LastBlockPOS := LastItemBlock.CurrentBlockPOS;
        if dbItem_OnlyWriteItemRec(SenderItem.RHeader.DataMainPOS, IOHnd, SenderItem) = False then
          begin
            Result := False;
            Exit;
          end;
      end;
    else
      begin
        if dbItem_OnlyReadItemBlockRec(SenderItem.FirstBlockPOS, IOHnd, FirstItemBlock) = False then
          begin
            SenderItem.Return := FirstItemBlock.Return;
            Result := False;
            Exit;
          end;
        FirstItemBlock.PrevBlockPOS := umlFileGetSize(IOHnd);
        if dbItem_OnlyWriteItemBlockRec(SenderItem.FirstBlockPOS, IOHnd, FirstItemBlock) = False then
          begin
            SenderItem.Return := FirstItemBlock.Return;
            Result := False;
            Exit;
          end;
        if dbItem_OnlyReadItemBlockRec(SenderItem.LastBlockPOS, IOHnd, LastItemBlock) = False then
          begin
            SenderItem.Return := LastItemBlock.Return;
            Result := False;
            Exit;
          end;
        LastItemBlock.IDFlags := db_item_MediumPositionFlags;
        LastItemBlock.NextBlockPOS := FirstItemBlock.PrevBlockPOS;
        if dbItem_OnlyWriteItemBlockRec(SenderItem.LastBlockPOS, IOHnd, LastItemBlock) = False then
          begin
            SenderItem.Return := LastItemBlock.Return;
            Result := False;
            Exit;
          end;
        LastItemBlock.IDFlags := db_item_LastPositionFlags;
        LastItemBlock.CurrentBlockPOS := FirstItemBlock.PrevBlockPOS;
        LastItemBlock.NextBlockPOS := SenderItem.FirstBlockPOS;
        LastItemBlock.PrevBlockPOS := SenderItem.LastBlockPOS;
        LastItemBlock.DataBlockPOS := LastItemBlock.CurrentBlockPOS + db_Item_BlockSize;
        LastItemBlock.Size := 0;
        if dbItem_OnlyWriteItemBlockRec(LastItemBlock.CurrentBlockPOS, IOHnd, LastItemBlock) = False then
          begin
            SenderItem.Return := LastItemBlock.Return;
            Result := False;
            Exit;
          end;
        SenderItem.BlockCount := SenderItem.BlockCount + 1;
        SenderItem.LastBlockPOS := LastItemBlock.CurrentBlockPOS;
        if dbItem_OnlyWriteItemRec(SenderItem.RHeader.DataMainPOS, IOHnd, SenderItem) = False then
          begin
            Result := False;
            Exit;
          end;
      end;
  end;
  SenderItem.CurrentItemBlock := LastItemBlock;
  SenderItem.CurrentBlockSeekPOS := 0;
  SenderItem.CurrentFileSeekPOS := SenderItem.CurrentItemBlock.DataBlockPOS;
  SenderItem.DataWrited := True;
  SenderItem.Return := db_Item_ok;
  Result := True;
end;

function dbItem_BlockInit(var IOHnd: TIOHnd; var SenderItem: TItem): Boolean;
begin
  if SenderItem.BlockCount = 0 then
    begin
      SenderItem.Return := db_Item_ok;
      Result := True;
      Exit;
    end;
  if dbItem_OnlyReadItemBlockRec(SenderItem.FirstBlockPOS, IOHnd, SenderItem.CurrentItemBlock) = False then
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

function dbItem_BlockReadData(var IOHnd: TIOHnd; var SenderItem: TItem; var Buffers; const _Size: Int64): Boolean;
label
  Rep_Label;
var
  BuffPointer: Pointer;
  BuffInt: nativeUInt;
  DeformitySize, BlockPOS: Int64;
  ItemBlock: TItemBlock;
  Size: Int64;
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
  BuffInt := nativeUInt(@Buffers);
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
      if dbItem_OnlyReadItemBlockRec(ItemBlock.NextBlockPOS, IOHnd, ItemBlock) = False then
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
          if dbItem_OnlyReadItemBlockRec(ItemBlock.NextBlockPOS, IOHnd, ItemBlock) = False then
            begin
              SenderItem.Return := ItemBlock.Return;
              Result := False;
              Exit;
            end;
        end;
    end;

  if umlFileSeek(IOHnd, ItemBlock.DataBlockPOS + BlockPOS) = False then
    begin
      SenderItem.Return := db_Item_SetPosError;
      Result := False;
      Exit;
    end;

  if DeformitySize <= ItemBlock.Size - BlockPOS then
    begin
      if umlFileRead(IOHnd, DeformitySize, BuffPointer^) = False then
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

  if umlFileRead(IOHnd, ItemBlock.Size - BlockPOS, BuffPointer^) = False then
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
  if dbItem_OnlyReadItemBlockRec(ItemBlock.NextBlockPOS, IOHnd, ItemBlock) = False then
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

function dbItem_BlockAppendWriteData(var IOHnd: TIOHnd; var SenderItem: TItem; var Buffers; const Size: Int64): Boolean;
begin
  if (SenderItem.BlockCount > 0) and ((SenderItem.CurrentItemBlock.DataBlockPOS + SenderItem.CurrentItemBlock.Size) = umlFileGetSize(IOHnd)) then
    begin
      if umlFileSeek(IOHnd, umlFileGetSize(IOHnd)) = False then
        begin
          SenderItem.Return := db_Item_SetPosError;
          Result := False;
          Exit;
        end;
      if umlFileWrite(IOHnd, Size, Buffers) = False then
        begin
          SenderItem.Return := db_Item_BlockWriteError;
          Result := False;
          Exit;
        end;
      SenderItem.CurrentItemBlock.Size := SenderItem.CurrentItemBlock.Size + Size;
      if dbItem_OnlyWriteItemBlockRec(SenderItem.CurrentItemBlock.CurrentBlockPOS, IOHnd, SenderItem.CurrentItemBlock) = False then
        begin
          SenderItem.Return := SenderItem.CurrentItemBlock.Return;
          Result := False;
          Exit;
        end;
      SenderItem.Size := SenderItem.Size + Size;
      if dbItem_OnlyWriteItemRec(SenderItem.RHeader.DataMainPOS, IOHnd, SenderItem) = False then
        begin
          Result := False;
          Exit;
        end;
      SenderItem.CurrentBlockSeekPOS := SenderItem.CurrentItemBlock.Size;
      SenderItem.CurrentFileSeekPOS := SenderItem.CurrentItemBlock.DataBlockPOS + SenderItem.CurrentItemBlock.Size;
      SenderItem.DataWrited := True;
      SenderItem.Return := db_Item_ok;
      Result := True;
      Exit;
    end;

  if dbItem_BlockCreate(IOHnd, SenderItem) = False then
    begin
      Result := False;
      Exit;
    end;

  if umlFileSeek(IOHnd, SenderItem.CurrentItemBlock.DataBlockPOS) = False then
    begin
      SenderItem.Return := db_Item_SetPosError;
      Result := False;
      Exit;
    end;

  if umlFileWrite(IOHnd, Size, Buffers) = False then
    begin
      SenderItem.Return := db_Item_BlockWriteError;
      Result := False;
      Exit;
    end;
  SenderItem.CurrentItemBlock.Size := Size;
  if dbItem_OnlyWriteItemBlockRec(SenderItem.CurrentItemBlock.CurrentBlockPOS, IOHnd, SenderItem.CurrentItemBlock) = False then
    begin
      SenderItem.Return := SenderItem.CurrentItemBlock.Return;
      Result := False;
      Exit;
    end;
  SenderItem.Size := SenderItem.Size + Size;
  if dbItem_OnlyWriteItemRec(SenderItem.RHeader.DataMainPOS, IOHnd, SenderItem) = False then
    begin
      Result := False;
      Exit;
    end;
  SenderItem.CurrentBlockSeekPOS := SenderItem.CurrentItemBlock.Size;
  SenderItem.CurrentFileSeekPOS := SenderItem.CurrentItemBlock.DataBlockPOS + SenderItem.CurrentItemBlock.Size;
  SenderItem.DataWrited := True;
  SenderItem.Return := db_Item_ok;
  Result := True;
end;

function dbItem_BlockWriteData(var IOHnd: TIOHnd; var SenderItem: TItem; var Buffers; const Size: Int64): Boolean;
label
  Rep_Label;
var
  BuffPointer: Pointer;
  BuffInt: nativeUInt;
  DeformitySize, BlockPOS: Int64;
  ItemBlock: TItemBlock;
begin
  if (SenderItem.Size = 0) or (SenderItem.BlockCount = 0) then
    begin
      Result := dbItem_BlockAppendWriteData(IOHnd, SenderItem, Buffers, Size);
      Exit;
    end;
  case SenderItem.CurrentItemBlock.IDFlags of
    db_item_LastPositionFlags, db_item_OnlyPositionFlags:
      begin
        if SenderItem.CurrentBlockSeekPOS = SenderItem.CurrentItemBlock.Size then
          begin
            Result := dbItem_BlockAppendWriteData(IOHnd, SenderItem, Buffers, Size);
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
  BuffInt := nativeUInt(@Buffers);
  BuffPointer := Pointer(BuffInt);
  DeformitySize := Size;
Rep_Label:
  if ItemBlock.Size - BlockPOS = 0 then
    begin
      case ItemBlock.IDFlags of
        db_item_LastPositionFlags, db_item_OnlyPositionFlags:
          begin
            Result := dbItem_BlockAppendWriteData(IOHnd, SenderItem, BuffPointer^, DeformitySize);
            Exit;
          end;
      end;
      if dbItem_OnlyReadItemBlockRec(ItemBlock.NextBlockPOS, IOHnd, ItemBlock) = False then
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
                Result := dbItem_BlockAppendWriteData(IOHnd, SenderItem, BuffPointer^, DeformitySize);
                Exit;
              end;
          end;
          if dbItem_OnlyReadItemBlockRec(ItemBlock.NextBlockPOS, IOHnd, ItemBlock) = False then
            begin
              SenderItem.Return := ItemBlock.Return;
              Result := False;
              Exit;
            end;
        end;
    end;

  if umlFileSeek(IOHnd, ItemBlock.DataBlockPOS + BlockPOS) = False then
    begin
      SenderItem.Return := db_Item_SetPosError;
      Result := False;
      Exit;
    end;

  if DeformitySize <= ItemBlock.Size - BlockPOS then
    begin
      if umlFileWrite(IOHnd, DeformitySize, BuffPointer^) = False then
        begin
          SenderItem.Return := db_Item_BlockWriteError;
          Result := False;
          Exit;
        end;
      SenderItem.CurrentBlockSeekPOS := BlockPOS + DeformitySize;
      SenderItem.CurrentFileSeekPOS := ItemBlock.DataBlockPOS + (BlockPOS + DeformitySize);
      SenderItem.CurrentItemBlock := ItemBlock;
      SenderItem.DataWrited := True;
      SenderItem.Return := db_Item_ok;
      Result := True;
      Exit;
    end;

  if umlFileWrite(IOHnd, ItemBlock.Size - BlockPOS, BuffPointer^) = False then
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
        Result := dbItem_BlockAppendWriteData(IOHnd, SenderItem, BuffPointer^, DeformitySize);
        Exit;
      end;
  end;
  if dbItem_OnlyReadItemBlockRec(ItemBlock.NextBlockPOS, IOHnd, ItemBlock) = False then
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

function dbItem_BlockSeekPOS(var IOHnd: TIOHnd; var SenderItem: TItem; const Position: Int64): Boolean;
var
  ItemBlock: TItemBlock;
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
  if dbItem_OnlyReadItemBlockRec(SenderItem.FirstBlockPOS, IOHnd, ItemBlock) = False then
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
  while dbItem_OnlyReadItemBlockRec(ItemBlock.NextBlockPOS, IOHnd, ItemBlock) do
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

function dbItem_BlockGetPOS(var IOHnd: TIOHnd; var SenderItem: TItem): Int64;
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
  if dbItem_OnlyReadItemBlockRec(SenderItem.CurrentItemBlock.PrevBlockPOS, IOHnd, ItemBlock) = False then
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
  while dbItem_OnlyReadItemBlockRec(ItemBlock.PrevBlockPOS, IOHnd, ItemBlock) do
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

function dbItem_BlockSeekStartPOS(var IOHnd: TIOHnd; var SenderItem: TItem): Boolean;
begin
  if SenderItem.BlockCount = 0 then
    begin
      SenderItem.Return := db_Item_BlockOverrate;
      Result := False;
      Exit;
    end;
  if dbItem_OnlyReadItemBlockRec(SenderItem.FirstBlockPOS, IOHnd, SenderItem.CurrentItemBlock) = False then
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

function dbItem_BlockSeekLastPOS(var IOHnd: TIOHnd; var SenderItem: TItem): Boolean;
begin
  if SenderItem.BlockCount = 0 then
    begin
      SenderItem.Return := db_Item_BlockOverrate;
      Result := False;
      Exit;
    end;
  if dbItem_OnlyReadItemBlockRec(SenderItem.LastBlockPOS, IOHnd, SenderItem.CurrentItemBlock) = False then
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

function dbField_GetPOSField(const fPos: Int64; var IOHnd: TIOHnd): TField;
begin
  dbField_ReadRec(fPos, IOHnd, Result);
end;

function dbField_GetFirstHeader(const fPos: Int64; var IOHnd: TIOHnd): THeader;
var
  f: TField;
begin
  if dbField_ReadRec(fPos, IOHnd, f) = False then
      Exit;
  dbHeader_ReadRec(f.FirstHeaderPOS, IOHnd, Result);
end;

function dbField_GetLastHeader(const fPos: Int64; var IOHnd: TIOHnd): THeader;
var
  f: TField;
begin
  if dbField_ReadRec(fPos, IOHnd, f) = False then
      Exit;
  dbHeader_ReadRec(f.LastHeaderPOS, IOHnd, Result);
end;

function dbField_OnlyFindFirstName(const Name: U_String; const fPos: Int64; var IOHnd: TIOHnd; var SenderFieldSearch: TFieldSearch): Boolean;
var
  f: TField;
begin
  SenderFieldSearch.InitFlags := False;
  if dbField_ReadRec(fPos, IOHnd, f) = False then
    begin
      SenderFieldSearch.Return := f.Return;
      Result := False;
      Exit;
    end;
  if f.HeaderCount = 0 then
    begin
      SenderFieldSearch.Return := db_Header_NotFindHeader;
      Result := False;
      Exit;
    end;
  if dbHeader_FindNext(Name, f.FirstHeaderPOS, f.LastHeaderPOS, IOHnd, SenderFieldSearch.RHeader) = False then
    begin
      SenderFieldSearch.Return := SenderFieldSearch.RHeader.Return;
      Result := False;
      Exit;
    end;
  SenderFieldSearch.InitFlags := True;
  SenderFieldSearch.PositionID := SenderFieldSearch.RHeader.PositionID;
  SenderFieldSearch.OverPOS := f.LastHeaderPOS;
  SenderFieldSearch.StartPos := SenderFieldSearch.RHeader.NextHeader;
  SenderFieldSearch.Name := Name;
  SenderFieldSearch.ID := SenderFieldSearch.RHeader.ID;
  SenderFieldSearch.Return := SenderFieldSearch.RHeader.Return;
  Result := True;
end;

function dbField_OnlyFindNextName(var IOHnd: TIOHnd; var SenderFieldSearch: TFieldSearch): Boolean;
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
  if dbHeader_FindNext(SenderFieldSearch.Name, SenderFieldSearch.StartPos, SenderFieldSearch.OverPOS, IOHnd, SenderFieldSearch.RHeader) = False then
    begin
      SenderFieldSearch.InitFlags := False;
      SenderFieldSearch.Return := SenderFieldSearch.RHeader.Return;
      Result := False;
      Exit;
    end;
  SenderFieldSearch.PositionID := SenderFieldSearch.RHeader.PositionID;
  SenderFieldSearch.StartPos := SenderFieldSearch.RHeader.NextHeader;
  SenderFieldSearch.ID := SenderFieldSearch.RHeader.ID;
  SenderFieldSearch.Return := SenderFieldSearch.RHeader.Return;
  Result := True;
end;

function dbField_OnlyFindLastName(const Name: U_String; const fPos: Int64; var IOHnd: TIOHnd; var SenderFieldSearch: TFieldSearch): Boolean;
var
  f: TField;
begin
  SenderFieldSearch.InitFlags := False;
  if dbField_ReadRec(fPos, IOHnd, f) = False then
    begin
      SenderFieldSearch.Return := f.Return;
      Result := False;
      Exit;
    end;
  if f.HeaderCount = 0 then
    begin
      SenderFieldSearch.Return := db_Header_NotFindHeader;
      Result := False;
      Exit;
    end;
  if dbHeader_FindPrev(Name, f.LastHeaderPOS, f.FirstHeaderPOS, IOHnd, SenderFieldSearch.RHeader) = False then
    begin
      SenderFieldSearch.Return := SenderFieldSearch.RHeader.Return;
      Result := False;
      Exit;
    end;
  SenderFieldSearch.InitFlags := True;
  SenderFieldSearch.PositionID := SenderFieldSearch.RHeader.PositionID;
  SenderFieldSearch.OverPOS := f.FirstHeaderPOS;
  SenderFieldSearch.StartPos := SenderFieldSearch.RHeader.PrevHeader;
  SenderFieldSearch.Name := Name;
  SenderFieldSearch.ID := SenderFieldSearch.RHeader.ID;
  SenderFieldSearch.Return := SenderFieldSearch.RHeader.Return;
  Result := True;
end;

function dbField_OnlyFindPrevName(var IOHnd: TIOHnd; var SenderFieldSearch: TFieldSearch): Boolean;
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
  if dbHeader_FindPrev(SenderFieldSearch.Name, SenderFieldSearch.StartPos, SenderFieldSearch.OverPOS, IOHnd, SenderFieldSearch.RHeader) = False then
    begin
      SenderFieldSearch.InitFlags := False;
      SenderFieldSearch.Return := SenderFieldSearch.RHeader.Return;
      Result := False;
      Exit;
    end;
  SenderFieldSearch.PositionID := SenderFieldSearch.RHeader.PositionID;
  SenderFieldSearch.StartPos := SenderFieldSearch.RHeader.PrevHeader;
  SenderFieldSearch.ID := SenderFieldSearch.RHeader.ID;
  SenderFieldSearch.Return := SenderFieldSearch.RHeader.Return;
  Result := True;
end;

function dbField_FindFirst(const Name: U_String; const ID: Byte; const fPos: Int64; var IOHnd: TIOHnd; var SenderFieldSearch: TFieldSearch): Boolean;
var
  f: TField;
begin
  SenderFieldSearch.InitFlags := False;
  if dbField_ReadRec(fPos, IOHnd, f) = False then
    begin
      SenderFieldSearch.Return := f.Return;
      Result := False;
      Exit;
    end;
  if f.HeaderCount = 0 then
    begin
      SenderFieldSearch.Return := db_Header_NotFindHeader;
      Result := False;
      Exit;
    end;
  SenderFieldSearch.OverPOS := f.LastHeaderPOS;
  SenderFieldSearch.StartPos := f.FirstHeaderPOS;
  while dbHeader_FindNext(Name, SenderFieldSearch.StartPos, SenderFieldSearch.OverPOS, IOHnd, SenderFieldSearch.RHeader) do
    begin
      SenderFieldSearch.StartPos := SenderFieldSearch.RHeader.NextHeader;
      if SenderFieldSearch.RHeader.ID = ID then
        begin
          SenderFieldSearch.InitFlags := True;
          SenderFieldSearch.PositionID := SenderFieldSearch.RHeader.PositionID;
          SenderFieldSearch.Name := Name;
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

function dbField_FindNext(var IOHnd: TIOHnd; var SenderFieldSearch: TFieldSearch): Boolean;
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
  while dbHeader_FindNext(SenderFieldSearch.Name, SenderFieldSearch.StartPos, SenderFieldSearch.OverPOS, IOHnd, SenderFieldSearch.RHeader) do
    begin
      SenderFieldSearch.StartPos := SenderFieldSearch.RHeader.NextHeader;

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

function dbField_FindLast(const Name: U_String; const ID: Byte; const fPos: Int64; var IOHnd: TIOHnd; var SenderFieldSearch: TFieldSearch): Boolean;
var
  f: TField;
begin
  SenderFieldSearch.InitFlags := False;
  if dbField_ReadRec(fPos, IOHnd, f) = False then
    begin
      SenderFieldSearch.Return := f.Return;
      Result := False;
      Exit;
    end;
  if f.HeaderCount = 0 then
    begin
      SenderFieldSearch.Return := db_Header_NotFindHeader;
      Result := False;
      Exit;
    end;
  SenderFieldSearch.OverPOS := f.FirstHeaderPOS;
  SenderFieldSearch.StartPos := f.LastHeaderPOS;
  while dbHeader_FindPrev(Name, SenderFieldSearch.StartPos, SenderFieldSearch.OverPOS, IOHnd, SenderFieldSearch.RHeader) do
    begin
      SenderFieldSearch.StartPos := SenderFieldSearch.RHeader.PrevHeader;
      if SenderFieldSearch.RHeader.ID = ID then
        begin
          SenderFieldSearch.InitFlags := True;
          SenderFieldSearch.PositionID := SenderFieldSearch.RHeader.PositionID;
          SenderFieldSearch.Name := Name;
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

function dbField_FindPrev(var IOHnd: TIOHnd; var SenderFieldSearch: TFieldSearch): Boolean;
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
  while dbHeader_FindPrev(SenderFieldSearch.Name, SenderFieldSearch.StartPos, SenderFieldSearch.OverPOS, IOHnd, SenderFieldSearch.RHeader) do
    begin
      SenderFieldSearch.StartPos := SenderFieldSearch.RHeader.PrevHeader;

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

function dbField_FindFirstItem(const Name: U_String; const ItemExtID: Byte; const fPos: Int64; var IOHnd: TIOHnd; var SenderFieldSearch: TFieldSearch; var SenderItem: TItem): Boolean;
begin
  if dbField_FindFirst(Name, db_Header_Item_ID, fPos, IOHnd, SenderFieldSearch) = False then
    begin
      Result := False;
      Exit;
    end;
  SenderItem.RHeader := SenderFieldSearch.RHeader;
  if dbItem_OnlyReadItemRec(SenderFieldSearch.RHeader.DataMainPOS, IOHnd, SenderItem) = False then
    begin
      SenderFieldSearch.Return := SenderItem.Return;
      Result := False;
      Exit;
    end;

  if SenderItem.ExtID = ItemExtID then
    begin
      Result := True;
      Exit;
    end;

  while dbField_FindNext(IOHnd, SenderFieldSearch) do
    begin
      SenderItem.RHeader := SenderFieldSearch.RHeader;
      if dbItem_OnlyReadItemRec(SenderFieldSearch.RHeader.DataMainPOS, IOHnd, SenderItem) = False then
        begin
          SenderFieldSearch.Return := SenderItem.Return;
          Result := False;
          Exit;
        end;

      if SenderItem.ExtID = ItemExtID then
        begin
          Result := True;
          Exit;
        end;
    end;
  Result := False;
end;

function dbField_FindNextItem(const ItemExtID: Byte; var IOHnd: TIOHnd; var SenderFieldSearch: TFieldSearch; var SenderItem: TItem): Boolean;
begin
  while dbField_FindNext(IOHnd, SenderFieldSearch) do
    begin
      SenderItem.RHeader := SenderFieldSearch.RHeader;
      if dbItem_OnlyReadItemRec(SenderFieldSearch.RHeader.DataMainPOS, IOHnd, SenderItem) = False then
        begin
          SenderFieldSearch.Return := SenderItem.Return;
          Result := False;
          Exit;
        end;

      if SenderItem.ExtID = ItemExtID then
        begin
          Result := True;
          Exit;
        end;
    end;
  Result := False;
end;

function dbField_FindLastItem(const Name: U_String; const ItemExtID: Byte; const fPos: Int64; var IOHnd: TIOHnd; var SenderFieldSearch: TFieldSearch; var SenderItem: TItem): Boolean;
begin
  if dbField_FindLast(Name, db_Header_Item_ID, fPos, IOHnd, SenderFieldSearch) = False then
    begin
      Result := False;
      Exit;
    end;
  SenderItem.RHeader := SenderFieldSearch.RHeader;
  if dbItem_OnlyReadItemRec(SenderFieldSearch.RHeader.DataMainPOS, IOHnd, SenderItem) = False then
    begin
      SenderFieldSearch.Return := SenderItem.Return;
      Result := False;
      Exit;
    end;

  if SenderItem.ExtID = ItemExtID then
    begin
      Result := True;
      Exit;
    end;

  while dbField_FindPrev(IOHnd, SenderFieldSearch) do
    begin
      SenderItem.RHeader := SenderFieldSearch.RHeader;
      if dbItem_OnlyReadItemRec(SenderFieldSearch.RHeader.DataMainPOS, IOHnd, SenderItem) = False then
        begin
          SenderFieldSearch.Return := SenderItem.Return;
          Result := False;
          Exit;
        end;

      if SenderItem.ExtID = ItemExtID then
        begin
          Result := True;
          Exit;
        end;
    end;
  Result := False;
end;

function dbField_FindPrevItem(const ItemExtID: Byte; var IOHnd: TIOHnd; var SenderFieldSearch: TFieldSearch; var SenderItem: TItem): Boolean;
begin
  while dbField_FindPrev(IOHnd, SenderFieldSearch) do
    begin
      SenderItem.RHeader := SenderFieldSearch.RHeader;
      if dbItem_OnlyReadItemRec(SenderFieldSearch.RHeader.DataMainPOS, IOHnd, SenderItem) = False then
        begin
          SenderFieldSearch.Return := SenderItem.Return;
          Result := False;
          Exit;
        end;

      if SenderItem.ExtID = ItemExtID then
        begin
          Result := True;
          Exit;
        end;
    end;
  Result := False;
end;

function dbField_FindFirstItem(const Name: U_String; const ItemExtID: Byte; const fPos: Int64; var IOHnd: TIOHnd; var SenderFieldSearch: TFieldSearch): Boolean;
var
  itm: TItem;
begin
  if dbField_FindFirst(Name, db_Header_Item_ID, fPos, IOHnd, SenderFieldSearch) = False then
    begin
      Result := False;
      Exit;
    end;
  if dbItem_ReadRec(SenderFieldSearch.RHeader.CurrentHeader, IOHnd, itm) = False then
    begin
      SenderFieldSearch.Return := itm.Return;
      Result := False;
      Exit;
    end;

  if itm.ExtID = ItemExtID then
    begin
      Result := True;
      Exit;
    end;

  while dbField_FindNext(IOHnd, SenderFieldSearch) do
    begin
      if dbItem_ReadRec(SenderFieldSearch.RHeader.CurrentHeader, IOHnd, itm) = False then
        begin
          SenderFieldSearch.Return := itm.Return;
          Result := False;
          Exit;
        end;
      if itm.ExtID = ItemExtID then
        begin
          Result := True;
          Exit;
        end;
    end;
  Result := False;
end;

function dbField_FindNextItem(const ItemExtID: Byte; var IOHnd: TIOHnd; var SenderFieldSearch: TFieldSearch): Boolean;
var
  itm: TItem;
begin
  while dbField_FindNext(IOHnd, SenderFieldSearch) do
    begin
      if dbItem_ReadRec(SenderFieldSearch.RHeader.CurrentHeader, IOHnd, itm) = False then
        begin
          SenderFieldSearch.Return := itm.Return;
          Result := False;
          Exit;
        end;
      if itm.ExtID = ItemExtID then
        begin
          Result := True;
          Exit;
        end;
    end;
  Result := False;
end;

function dbField_FindLastItem(const Name: U_String; const ItemExtID: Byte; const fPos: Int64; var IOHnd: TIOHnd; var SenderFieldSearch: TFieldSearch): Boolean;
var
  itm: TItem;
begin
  if dbField_FindLast(Name, db_Header_Item_ID, fPos, IOHnd, SenderFieldSearch) = False then
    begin
      Result := False;
      Exit;
    end;
  if dbItem_ReadRec(SenderFieldSearch.RHeader.CurrentHeader, IOHnd, itm) = False then
    begin
      SenderFieldSearch.Return := itm.Return;
      Result := False;
      Exit;
    end;

  if itm.ExtID = ItemExtID then
    begin
      Result := True;
      Exit;
    end;

  while dbField_FindPrev(IOHnd, SenderFieldSearch) do
    begin
      if dbItem_ReadRec(SenderFieldSearch.RHeader.CurrentHeader, IOHnd, itm) = False then
        begin
          SenderFieldSearch.Return := itm.Return;
          Result := False;
          Exit;
        end;
      if itm.ExtID = ItemExtID then
        begin
          Result := True;
          Exit;
        end;
    end;
  Result := False;
end;

function dbField_FindPrevItem(const ItemExtID: Byte; var IOHnd: TIOHnd; var SenderFieldSearch: TFieldSearch): Boolean;
var
  itm: TItem;
begin
  while dbField_FindPrev(IOHnd, SenderFieldSearch) do
    begin
      if dbItem_ReadRec(SenderFieldSearch.RHeader.CurrentHeader, IOHnd, itm) = False then
        begin
          SenderFieldSearch.Return := itm.Return;
          Result := False;
          Exit;
        end;
      if itm.ExtID = ItemExtID then
        begin
          Result := True;
          Exit;
        end;
    end;
  Result := False;
end;

function dbField_ExistItem(const Name: U_String; const ItemExtID: Byte; const fPos: Int64; var IOHnd: TIOHnd): Boolean;
var
  fs: TFieldSearch;
begin
  Result := dbField_FindFirstItem(Name, ItemExtID, fPos, IOHnd, fs);
end;

function dbField_ExistHeader(const Name: U_String; const ID: Byte; const fPos: Int64; var IOHnd: TIOHnd): Boolean;
var
  fs: TFieldSearch;
begin
  Result := dbField_FindFirst(Name, ID, fPos, IOHnd, fs);
end;

function dbField_CreateHeader(const Name: U_String; const ID: Byte; const fPos: Int64; var IOHnd: TIOHnd; var SenderHeader: THeader): Boolean;
var
  f: TField;
  _Header: THeader;
begin
  if dbField_ReadRec(fPos, IOHnd, f) = False then
    begin
      SenderHeader.Return := f.Return;
      Result := False;
      Exit;
    end;
  SenderHeader.ID := ID;
  SenderHeader.Name := Name;
  case f.HeaderCount of
    0:
      begin
        f.HeaderCount := 1;
        f.FirstHeaderPOS := umlFileGetSize(IOHnd);
        f.LastHeaderPOS := f.FirstHeaderPOS;
        f.RHeader.LastModifyTime := umlDefaultTime;
        SenderHeader.PositionID := db_Header_OnlyPositionFlags;
        SenderHeader.NextHeader := f.LastHeaderPOS;
        SenderHeader.PrevHeader := f.FirstHeaderPOS;
        SenderHeader.CurrentHeader := f.FirstHeaderPOS;
        SenderHeader.CreateTime := umlDefaultTime;
        SenderHeader.LastModifyTime := umlDefaultTime;
        SenderHeader.DataMainPOS := SenderHeader.CurrentHeader + db_Header_Size;
        if dbField_WriteRec(f.RHeader.CurrentHeader, IOHnd, f) = False then
          begin
            SenderHeader.Return := f.Return;
            Result := False;
            Exit;
          end;
        if dbHeader_WriteRec(SenderHeader.CurrentHeader, IOHnd, SenderHeader) = False then
          begin
            Result := False;
            Exit;
          end;
      end;
    1:
      begin
        SenderHeader.CurrentHeader := umlFileGetSize(IOHnd);
        SenderHeader.NextHeader := f.FirstHeaderPOS;
        SenderHeader.PrevHeader := f.FirstHeaderPOS;

        if dbHeader_ReadRec(f.FirstHeaderPOS, IOHnd, _Header) = False then
          begin
            SenderHeader.Return := _Header.Return;
            Result := False;
            Exit;
          end;
        _Header.PrevHeader := SenderHeader.CurrentHeader;
        _Header.NextHeader := SenderHeader.CurrentHeader;
        _Header.PositionID := db_Header_FirstPositionFlags;
        if dbHeader_WriteRec(f.FirstHeaderPOS, IOHnd, _Header) = False then
          begin
            SenderHeader.Return := _Header.Return;
            Result := False;
            Exit;
          end;
        f.HeaderCount := f.HeaderCount + 1;
        f.LastHeaderPOS := SenderHeader.CurrentHeader;
        f.RHeader.LastModifyTime := umlDefaultTime;
        SenderHeader.CreateTime := umlDefaultTime;
        SenderHeader.LastModifyTime := umlDefaultTime;
        SenderHeader.DataMainPOS := SenderHeader.CurrentHeader + db_Header_Size;
        SenderHeader.PositionID := db_Header_LastPositionFlags;
        if dbField_WriteRec(f.RHeader.CurrentHeader, IOHnd, f) = False then
          begin
            SenderHeader.Return := f.Return;
            Result := False;
            Exit;
          end;
        if dbHeader_WriteRec(SenderHeader.CurrentHeader, IOHnd, SenderHeader) = False then
          begin
            Result := False;
            Exit;
          end;
      end;
    else
      begin
        SenderHeader.CurrentHeader := umlFileGetSize(IOHnd);

        // modify first header
        if dbHeader_ReadRec(f.FirstHeaderPOS, IOHnd, _Header) = False then
          begin
            SenderHeader.Return := _Header.Return;
            Result := False;
            Exit;
          end;
        _Header.PrevHeader := SenderHeader.CurrentHeader;
        SenderHeader.NextHeader := _Header.CurrentHeader;
        if dbHeader_WriteRec(f.FirstHeaderPOS, IOHnd, _Header) = False then
          begin
            SenderHeader.Return := _Header.Return;
            Result := False;
            Exit;
          end;

        // moidfy last header
        if dbHeader_ReadRec(f.LastHeaderPOS, IOHnd, _Header) = False then
          begin
            SenderHeader.Return := _Header.Return;
            Result := False;
            Exit;
          end;
        _Header.NextHeader := SenderHeader.CurrentHeader;
        SenderHeader.PrevHeader := f.LastHeaderPOS;
        _Header.PositionID := db_Header_MediumPositionFlags;
        if dbHeader_WriteRec(f.LastHeaderPOS, IOHnd, _Header) = False then
          begin
            SenderHeader.Return := _Header.Return;
            Result := False;
            Exit;
          end;

        f.HeaderCount := f.HeaderCount + 1;
        f.LastHeaderPOS := SenderHeader.CurrentHeader;
        f.RHeader.LastModifyTime := umlDefaultTime;
        SenderHeader.CreateTime := umlDefaultTime;
        SenderHeader.LastModifyTime := umlDefaultTime;
        SenderHeader.DataMainPOS := SenderHeader.CurrentHeader + db_Header_Size;
        SenderHeader.PositionID := db_Header_LastPositionFlags;
        if dbField_WriteRec(f.RHeader.CurrentHeader, IOHnd, f) = False then
          begin
            SenderHeader.Return := f.Return;
            Result := False;
            Exit;
          end;
        if dbHeader_WriteRec(SenderHeader.CurrentHeader, IOHnd, SenderHeader) = False then
          begin
            Result := False;
            Exit;
          end;
      end;
  end;
  SenderHeader.Return := db_Header_ok;
  Result := True;
end;

function dbField_InsertNewHeader(const Name: U_String; const ID: Byte; const FieldPos, InsertHeaderPos: Int64; var IOHnd: TIOHnd; var NewHeader: THeader): Boolean;
var
  f: TField;

  Curr, Prev: THeader;
begin
  if dbField_ReadRec(FieldPos, IOHnd, f) = False then
    begin
      NewHeader.Return := f.Return;
      Result := False;
      Exit;
    end;

  if dbHeader_ReadRec(InsertHeaderPos, IOHnd, Curr) = False then
    begin
      NewHeader.Return := Curr.Return;
      Result := False;
      Exit;
    end;

  f.RHeader.LastModifyTime := umlDefaultTime;

  NewHeader.CurrentHeader := umlFileGetSize(IOHnd);
  NewHeader.DataMainPOS := NewHeader.CurrentHeader + db_Header_Size;
  NewHeader.CreateTime := umlDefaultTime;
  NewHeader.LastModifyTime := umlDefaultTime;
  NewHeader.ID := ID;
  NewHeader.UserProperty := 0;
  NewHeader.Name := Name;
  NewHeader.Return := db_Header_ok;

  case Curr.PositionID of
    db_Header_FirstPositionFlags:
      begin
        if f.HeaderCount > 1 then
          begin
            // moidfy field
            f.HeaderCount := f.HeaderCount + 1;
            f.FirstHeaderPOS := NewHeader.CurrentHeader;
            if dbField_WriteRec(f.RHeader.CurrentHeader, IOHnd, f) = False then
              begin
                NewHeader.Return := f.Return;
                Result := False;
                Exit;
              end;

            // write newheader
            NewHeader.PrevHeader := f.LastHeaderPOS;
            NewHeader.NextHeader := Curr.CurrentHeader;
            NewHeader.PositionID := db_Header_FirstPositionFlags;
            if dbHeader_WriteRec(NewHeader.CurrentHeader, IOHnd, NewHeader) = False then
              begin
                Result := False;
                Exit;
              end;

            // moidfy current
            Curr.PrevHeader := NewHeader.CurrentHeader;
            Curr.PositionID := db_Header_MediumPositionFlags;
            if dbHeader_WriteRec(Curr.CurrentHeader, IOHnd, Curr) = False then
              begin
                NewHeader.Return := Curr.Return;
                Result := False;
                Exit;
              end;
          end
        else if f.HeaderCount = 1 then
          begin
            // modify field
            f.HeaderCount := f.HeaderCount + 1;
            f.FirstHeaderPOS := NewHeader.CurrentHeader;
            f.LastHeaderPOS := Curr.CurrentHeader;
            if dbField_WriteRec(f.RHeader.CurrentHeader, IOHnd, f) = False then
              begin
                NewHeader.Return := f.Return;
                Result := False;
                Exit;
              end;

            // write newheader
            NewHeader.PrevHeader := f.LastHeaderPOS;
            NewHeader.NextHeader := Curr.CurrentHeader;
            NewHeader.PositionID := db_Header_FirstPositionFlags;
            if dbHeader_WriteRec(NewHeader.CurrentHeader, IOHnd, NewHeader) = False then
              begin
                Result := False;
                Exit;
              end;

            // modify current header
            Curr.PrevHeader := NewHeader.CurrentHeader;
            Curr.PositionID := db_Header_LastPositionFlags;
            if dbHeader_WriteRec(Curr.CurrentHeader, IOHnd, Curr) = False then
              begin
                NewHeader.Return := Curr.Return;
                Result := False;
                Exit;
              end;
          end
        else
          begin
            // error
            NewHeader.Return := db_Header_NotFindHeader;
            Result := False;
            Exit;
          end
      end;
    db_Header_MediumPositionFlags:
      begin
        // read prev header
        if dbHeader_ReadRec(Curr.PrevHeader, IOHnd, Prev) = False then
          begin
            NewHeader.Return := Prev.Return;
            Result := False;
            Exit;
          end;

        // modify field
        f.HeaderCount := f.HeaderCount + 1;
        if dbField_WriteRec(f.RHeader.CurrentHeader, IOHnd, f) = False then
          begin
            NewHeader.Return := f.Return;
            Result := False;
            Exit;
          end;

        // write newheader
        NewHeader.PrevHeader := Prev.CurrentHeader;
        NewHeader.NextHeader := Curr.CurrentHeader;
        NewHeader.PositionID := db_Header_MediumPositionFlags;
        if dbHeader_WriteRec(NewHeader.CurrentHeader, IOHnd, NewHeader) = False then
          begin
            Result := False;
            Exit;
          end;

        // modify prev header
        Prev.NextHeader := NewHeader.CurrentHeader;
        if dbHeader_WriteRec(Prev.CurrentHeader, IOHnd, Prev) = False then
          begin
            NewHeader.Return := Prev.Return;
            Result := False;
            Exit;
          end;

        // write current
        Curr.PrevHeader := NewHeader.CurrentHeader;
        Curr.PositionID := db_Header_MediumPositionFlags;
        if dbHeader_WriteRec(Curr.CurrentHeader, IOHnd, Curr) = False then
          begin
            NewHeader.Return := Curr.Return;
            Result := False;
            Exit;
          end;
      end;
    db_Header_LastPositionFlags:
      begin
        if f.HeaderCount > 1 then
          begin
            // read prev header
            if dbHeader_ReadRec(Curr.PrevHeader, IOHnd, Prev) = False then
              begin
                NewHeader.Return := Prev.Return;
                Result := False;
                Exit;
              end;

            // modify field
            f.HeaderCount := f.HeaderCount + 1;
            if dbField_WriteRec(f.RHeader.CurrentHeader, IOHnd, f) = False then
              begin
                NewHeader.Return := f.Return;
                Result := False;
                Exit;
              end;

            // write newheader
            NewHeader.PrevHeader := Prev.CurrentHeader;
            NewHeader.NextHeader := Curr.CurrentHeader;
            NewHeader.PositionID := db_Header_MediumPositionFlags;
            if dbHeader_WriteRec(NewHeader.CurrentHeader, IOHnd, NewHeader) = False then
              begin
                Result := False;
                Exit;
              end;

            // modify prev header
            Prev.NextHeader := NewHeader.CurrentHeader;
            if dbHeader_WriteRec(Prev.CurrentHeader, IOHnd, Prev) = False then
              begin
                NewHeader.Return := Prev.Return;
                Result := False;
                Exit;
              end;

            // write current
            Curr.PrevHeader := NewHeader.CurrentHeader;
            Curr.PositionID := db_Header_LastPositionFlags;
            if dbHeader_WriteRec(Curr.CurrentHeader, IOHnd, Curr) = False then
              begin
                NewHeader.Return := Curr.Return;
                Result := False;
                Exit;
              end;
          end
        else if f.HeaderCount = 1 then
          begin
            // modify field
            f.HeaderCount := f.HeaderCount + 1;
            f.FirstHeaderPOS := NewHeader.CurrentHeader;
            f.LastHeaderPOS := Curr.CurrentHeader;
            if dbField_WriteRec(f.RHeader.CurrentHeader, IOHnd, f) = False then
              begin
                NewHeader.Return := f.Return;
                Result := False;
                Exit;
              end;

            // write newheader
            NewHeader.PrevHeader := f.LastHeaderPOS;
            NewHeader.NextHeader := Curr.CurrentHeader;
            NewHeader.PositionID := db_Header_FirstPositionFlags;
            if dbHeader_WriteRec(NewHeader.CurrentHeader, IOHnd, NewHeader) = False then
              begin
                Result := False;
                Exit;
              end;

            // modify current header
            Curr.PrevHeader := NewHeader.CurrentHeader;
            Curr.PositionID := db_Header_LastPositionFlags;
            if dbHeader_WriteRec(Curr.CurrentHeader, IOHnd, Curr) = False then
              begin
                NewHeader.Return := Curr.Return;
                Result := False;
                Exit;
              end;
          end
        else
          begin
            // error
            NewHeader.Return := db_Header_NotFindHeader;
            Result := False;
            Exit;
          end;
      end;
    db_Header_OnlyPositionFlags:
      begin
        // modify field
        f.HeaderCount := f.HeaderCount + 1;
        f.FirstHeaderPOS := NewHeader.CurrentHeader;
        f.LastHeaderPOS := Curr.CurrentHeader;
        if dbField_WriteRec(f.RHeader.CurrentHeader, IOHnd, f) = False then
          begin
            NewHeader.Return := f.Return;
            Result := False;
            Exit;
          end;

        // write newheader
        NewHeader.PrevHeader := f.LastHeaderPOS;
        NewHeader.NextHeader := Curr.CurrentHeader;
        NewHeader.PositionID := db_Header_FirstPositionFlags;
        if dbHeader_WriteRec(NewHeader.CurrentHeader, IOHnd, NewHeader) = False then
          begin
            Result := False;
            Exit;
          end;

        // modify current header
        Curr.PrevHeader := NewHeader.CurrentHeader;
        Curr.PositionID := db_Header_LastPositionFlags;
        if dbHeader_WriteRec(Curr.CurrentHeader, IOHnd, Curr) = False then
          begin
            NewHeader.Return := Curr.Return;
            Result := False;
            Exit;
          end;
      end;
  end;

  NewHeader.Return := db_Header_ok;
  Result := True;
end;

function dbField_DeleteHeader(const HeaderPOS, FieldPos: Int64; var IOHnd: TIOHnd; var SenderField: TField): Boolean;
var
  DeleteHeader, SwapHeader: THeader;
begin
  if dbField_ReadRec(FieldPos, IOHnd, SenderField) = False then
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
            if dbField_WriteRec(SenderField.RHeader.CurrentHeader, IOHnd, SenderField) = False then
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
        if dbHeader_ReadRec(HeaderPOS, IOHnd, DeleteHeader) = False then
          begin
            SenderField.Return := DeleteHeader.Return;
            Result := False;
            Exit;
          end;
        case DeleteHeader.PositionID of
          db_Header_FirstPositionFlags:
            begin
              if dbHeader_ReadRec(SenderField.LastHeaderPOS, IOHnd, DeleteHeader) = False then
                begin
                  SenderField.Return := DeleteHeader.Return;
                  Result := False;
                  Exit;
                end;
              DeleteHeader.NextHeader := DeleteHeader.CurrentHeader;
              DeleteHeader.PrevHeader := DeleteHeader.CurrentHeader;
              DeleteHeader.PositionID := db_Header_OnlyPositionFlags;
              if dbHeader_WriteRec(DeleteHeader.CurrentHeader, IOHnd, DeleteHeader) = False then
                begin
                  SenderField.Return := DeleteHeader.Return;
                  Result := False;
                  Exit;
                end;
              SenderField.FirstHeaderPOS := DeleteHeader.CurrentHeader;
              SenderField.LastHeaderPOS := DeleteHeader.CurrentHeader;
              SenderField.HeaderCount := SenderField.HeaderCount - 1;
              SenderField.RHeader.LastModifyTime := umlDefaultTime;
              if dbField_WriteRec(SenderField.RHeader.CurrentHeader, IOHnd, SenderField) = False then
                begin
                  Result := False;
                  Exit;
                end;
              SenderField.Return := db_Field_ok;
              Result := True;
            end;
          db_Header_LastPositionFlags:
            begin
              if dbHeader_ReadRec(SenderField.FirstHeaderPOS, IOHnd, DeleteHeader) = False then
                begin
                  SenderField.Return := DeleteHeader.Return;
                  Result := False;
                  Exit;
                end;
              DeleteHeader.NextHeader := DeleteHeader.CurrentHeader;
              DeleteHeader.PrevHeader := DeleteHeader.CurrentHeader;
              DeleteHeader.PositionID := db_Header_OnlyPositionFlags;
              if dbHeader_WriteRec(DeleteHeader.CurrentHeader, IOHnd, DeleteHeader) = False then
                begin
                  SenderField.Return := DeleteHeader.Return;
                  Result := False;
                  Exit;
                end;
              SenderField.FirstHeaderPOS := DeleteHeader.CurrentHeader;
              SenderField.LastHeaderPOS := DeleteHeader.CurrentHeader;
              SenderField.HeaderCount := SenderField.HeaderCount - 1;
              SenderField.RHeader.LastModifyTime := umlDefaultTime;
              if dbField_WriteRec(SenderField.RHeader.CurrentHeader, IOHnd, SenderField) = False then
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
        if dbHeader_ReadRec(HeaderPOS, IOHnd, DeleteHeader) = False then
          begin
            SenderField.Return := DeleteHeader.Return;
            Result := False;
            Exit;
          end;
        case DeleteHeader.PositionID of
          db_Header_FirstPositionFlags:
            begin
              if dbHeader_ReadRec(DeleteHeader.NextHeader, IOHnd, SwapHeader) = False then
                begin
                  SenderField.Return := SwapHeader.Return;
                  Result := False;
                  Exit;
                end;
              SwapHeader.PrevHeader := DeleteHeader.PrevHeader;
              SwapHeader.PositionID := db_Header_FirstPositionFlags;
              if dbHeader_WriteRec(SwapHeader.CurrentHeader, IOHnd, SwapHeader) = False then
                begin
                  SenderField.Return := SwapHeader.Return;
                  Result := False;
                  Exit;
                end;
              SenderField.FirstHeaderPOS := SwapHeader.CurrentHeader;
              if dbHeader_ReadRec(DeleteHeader.PrevHeader, IOHnd, SwapHeader) = False then
                begin
                  SenderField.Return := SwapHeader.Return;
                  Result := False;
                  Exit;
                end;
              SwapHeader.NextHeader := DeleteHeader.NextHeader;
              if dbHeader_WriteRec(SwapHeader.CurrentHeader, IOHnd, SwapHeader) = False then
                begin
                  SenderField.Return := SwapHeader.Return;
                  Result := False;
                  Exit;
                end;
              SenderField.HeaderCount := SenderField.HeaderCount - 1;
              SenderField.RHeader.LastModifyTime := umlDefaultTime;
              if dbField_WriteRec(SenderField.RHeader.CurrentHeader, IOHnd, SenderField) = False then
                begin
                  Result := False;
                  Exit;
                end;
              SenderField.Return := db_Field_ok;
              Result := True;
            end;
          db_Header_MediumPositionFlags:
            begin
              if dbHeader_ReadRec(DeleteHeader.PrevHeader, IOHnd, SwapHeader) = False then
                begin
                  SenderField.Return := SwapHeader.Return;
                  Result := False;
                  Exit;
                end;
              SwapHeader.NextHeader := DeleteHeader.NextHeader;
              if dbHeader_WriteRec(SwapHeader.CurrentHeader, IOHnd, SwapHeader) = False then
                begin
                  SenderField.Return := SwapHeader.Return;
                  Result := False;
                  Exit;
                end;
              if dbHeader_ReadRec(DeleteHeader.NextHeader, IOHnd, SwapHeader) = False then
                begin
                  SenderField.Return := SwapHeader.Return;
                  Result := False;
                  Exit;
                end;
              SwapHeader.PrevHeader := DeleteHeader.PrevHeader;
              if dbHeader_WriteRec(SwapHeader.CurrentHeader, IOHnd, SwapHeader) = False then
                begin
                  SenderField.Return := SwapHeader.Return;
                  Result := False;
                  Exit;
                end;
              SenderField.HeaderCount := SenderField.HeaderCount - 1;
              SenderField.RHeader.LastModifyTime := umlDefaultTime;
              if dbField_WriteRec(SenderField.RHeader.CurrentHeader, IOHnd, SenderField) = False then
                begin
                  Result := False;
                  Exit;
                end;
              SenderField.Return := db_Field_ok;
              Result := True;
            end;
          db_Header_LastPositionFlags:
            begin
              if dbHeader_ReadRec(DeleteHeader.PrevHeader, IOHnd, SwapHeader) = False then
                begin
                  SenderField.Return := SwapHeader.Return;
                  Result := False;
                  Exit;
                end;
              SwapHeader.NextHeader := DeleteHeader.NextHeader;
              SwapHeader.PositionID := db_Header_LastPositionFlags;
              if dbHeader_WriteRec(SwapHeader.CurrentHeader, IOHnd, SwapHeader) = False then
                begin
                  SenderField.Return := SwapHeader.Return;
                  Result := False;
                  Exit;
                end;
              SenderField.LastHeaderPOS := SwapHeader.CurrentHeader;
              if dbHeader_ReadRec(DeleteHeader.NextHeader, IOHnd, SwapHeader) = False then
                begin
                  SenderField.Return := SwapHeader.Return;
                  Result := False;
                  Exit;
                end;
              SwapHeader.PrevHeader := DeleteHeader.PrevHeader;
              if dbHeader_WriteRec(SwapHeader.CurrentHeader, IOHnd, SwapHeader) = False then
                begin
                  SenderField.Return := SwapHeader.Return;
                  Result := False;
                  Exit;
                end;
              SenderField.HeaderCount := SenderField.HeaderCount - 1;
              SenderField.RHeader.LastModifyTime := umlDefaultTime;
              if dbField_WriteRec(SenderField.RHeader.CurrentHeader, IOHnd, SenderField) = False then
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
        if dbHeader_ReadRec(HeaderPOS, IOHnd, DeleteHeader) = False then
          begin
            SenderField.Return := DeleteHeader.Return;
            Result := False;
            Exit;
          end;
        case DeleteHeader.PositionID of
          db_Header_FirstPositionFlags:
            begin
              if dbHeader_ReadRec(DeleteHeader.NextHeader, IOHnd, SwapHeader) = False then
                begin
                  SenderField.Return := SwapHeader.Return;
                  Result := False;
                  Exit;
                end;
              SwapHeader.PrevHeader := DeleteHeader.PrevHeader;
              SwapHeader.PositionID := db_Header_FirstPositionFlags;
              if dbHeader_WriteRec(SwapHeader.CurrentHeader, IOHnd, SwapHeader) = False then
                begin
                  SenderField.Return := SwapHeader.Return;
                  Result := False;
                  Exit;
                end;
              SenderField.FirstHeaderPOS := SwapHeader.CurrentHeader;
              if dbHeader_ReadRec(DeleteHeader.PrevHeader, IOHnd, SwapHeader) = False then
                begin
                  SenderField.Return := SwapHeader.Return;
                  Result := False;
                  Exit;
                end;
              SwapHeader.NextHeader := DeleteHeader.NextHeader;
              if dbHeader_WriteRec(SwapHeader.CurrentHeader, IOHnd, SwapHeader) = False then
                begin
                  SenderField.Return := SwapHeader.Return;
                  Result := False;
                  Exit;
                end;
              SenderField.HeaderCount := SenderField.HeaderCount - 1;
              SenderField.RHeader.LastModifyTime := umlDefaultTime;
              if dbField_WriteRec(SenderField.RHeader.CurrentHeader, IOHnd, SenderField) = False then
                begin
                  Result := False;
                  Exit;
                end;
              SenderField.Return := db_Field_ok;
              Result := True;
            end;
          db_Header_MediumPositionFlags:
            begin
              if dbHeader_ReadRec(DeleteHeader.PrevHeader, IOHnd, SwapHeader) = False then
                begin
                  SenderField.Return := SwapHeader.Return;
                  Result := False;
                  Exit;
                end;
              SwapHeader.NextHeader := DeleteHeader.NextHeader;
              if dbHeader_WriteRec(SwapHeader.CurrentHeader, IOHnd, SwapHeader) = False then
                begin
                  SenderField.Return := SwapHeader.Return;
                  Result := False;
                  Exit;
                end;
              if dbHeader_ReadRec(DeleteHeader.NextHeader, IOHnd, SwapHeader) = False then
                begin
                  SenderField.Return := SwapHeader.Return;
                  Result := False;
                  Exit;
                end;
              SwapHeader.PrevHeader := DeleteHeader.PrevHeader;
              if dbHeader_WriteRec(SwapHeader.CurrentHeader, IOHnd, SwapHeader) = False then
                begin
                  SenderField.Return := SwapHeader.Return;
                  Result := False;
                  Exit;
                end;
              SenderField.HeaderCount := SenderField.HeaderCount - 1;
              SenderField.RHeader.LastModifyTime := umlDefaultTime;
              if dbField_WriteRec(SenderField.RHeader.CurrentHeader, IOHnd, SenderField) = False then
                begin
                  Result := False;
                  Exit;
                end;
              SenderField.Return := db_Field_ok;
              Result := True;
            end;
          db_Header_LastPositionFlags:
            begin
              if dbHeader_ReadRec(DeleteHeader.PrevHeader, IOHnd, SwapHeader) = False then
                begin
                  SenderField.Return := SwapHeader.Return;
                  Result := False;
                  Exit;
                end;
              SwapHeader.NextHeader := DeleteHeader.NextHeader;
              SwapHeader.PositionID := db_Header_LastPositionFlags;
              if dbHeader_WriteRec(SwapHeader.CurrentHeader, IOHnd, SwapHeader) = False then
                begin
                  SenderField.Return := SwapHeader.Return;
                  Result := False;
                  Exit;
                end;
              SenderField.LastHeaderPOS := SwapHeader.CurrentHeader;
              if dbHeader_ReadRec(DeleteHeader.NextHeader, IOHnd, SwapHeader) = False then
                begin
                  SenderField.Return := SwapHeader.Return;
                  Result := False;
                  Exit;
                end;
              SwapHeader.PrevHeader := DeleteHeader.PrevHeader;
              if dbHeader_WriteRec(SwapHeader.CurrentHeader, IOHnd, SwapHeader) = False then
                begin
                  SenderField.Return := SwapHeader.Return;
                  Result := False;
                  Exit;
                end;
              SenderField.HeaderCount := SenderField.HeaderCount - 1;
              SenderField.RHeader.LastModifyTime := umlDefaultTime;
              if dbField_WriteRec(SenderField.RHeader.CurrentHeader, IOHnd, SenderField) = False then
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

function dbField_MoveHeader(const HeaderPOS: Int64; const SourcerFieldPOS, TargetFieldPos: Int64; var IOHnd: TIOHnd; var SenderField: TField): Boolean;
var
  ActiveHeader, SwapHeader: THeader;
begin
  if dbHeader_ReadRec(HeaderPOS, IOHnd, ActiveHeader) = False then
    begin
      SenderField.Return := ActiveHeader.Return;
      Result := False;
      Exit;
    end;
  if dbField_DeleteHeader(ActiveHeader.CurrentHeader, SourcerFieldPOS, IOHnd, SenderField) = False then
    begin
      Result := False;
      Exit;
    end;

  if dbField_ReadRec(TargetFieldPos, IOHnd, SenderField) = False then
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
        if dbField_WriteRec(SenderField.RHeader.CurrentHeader, IOHnd, SenderField) = False then
          begin
            Result := False;
            Exit;
          end;
        if dbHeader_WriteRec(ActiveHeader.CurrentHeader, IOHnd, ActiveHeader) = False then
          begin
            SenderField.Return := ActiveHeader.Return;
            Result := False;
            Exit;
          end;
      end;
    1:
      begin

        if dbHeader_ReadRec(SenderField.FirstHeaderPOS, IOHnd, SwapHeader) = False then
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

        if dbHeader_WriteRec(SwapHeader.CurrentHeader, IOHnd, SwapHeader) = False then
          begin
            SenderField.Return := SwapHeader.Return;
            Result := False;
            Exit;
          end;
        SenderField.HeaderCount := SenderField.HeaderCount + 1;
        SenderField.LastHeaderPOS := ActiveHeader.CurrentHeader;
        SenderField.RHeader.LastModifyTime := umlDefaultTime;
        if dbField_WriteRec(SenderField.RHeader.CurrentHeader, IOHnd, SenderField) = False then
          begin
            Result := False;
            Exit;
          end;
        if dbHeader_WriteRec(ActiveHeader.CurrentHeader, IOHnd, ActiveHeader) = False then
          begin
            SenderField.Return := ActiveHeader.Return;
            Result := False;
            Exit;
          end;
      end;
    else
      begin

        if dbHeader_ReadRec(SenderField.FirstHeaderPOS, IOHnd, SwapHeader) = False then
          begin
            SenderField.Return := SwapHeader.Return;
            Result := False;
            Exit;
          end;
        SwapHeader.PrevHeader := ActiveHeader.CurrentHeader;
        SwapHeader.PositionID := db_Header_FirstPositionFlags;
        ActiveHeader.NextHeader := SwapHeader.CurrentHeader;
        if dbHeader_WriteRec(SwapHeader.CurrentHeader, IOHnd, SwapHeader) = False then
          begin
            SenderField.Return := SwapHeader.Return;
            Result := False;
            Exit;
          end;

        if dbHeader_ReadRec(SenderField.LastHeaderPOS, IOHnd, SwapHeader) = False then
          begin
            SenderField.Return := SwapHeader.Return;
            Result := False;
            Exit;
          end;
        SwapHeader.NextHeader := ActiveHeader.CurrentHeader;
        ActiveHeader.PrevHeader := SwapHeader.CurrentHeader;
        SwapHeader.PositionID := db_Header_MediumPositionFlags;
        if dbHeader_WriteRec(SwapHeader.CurrentHeader, IOHnd, SwapHeader) = False then
          begin
            SenderField.Return := SwapHeader.Return;
            Result := False;
            Exit;
          end;
        SenderField.HeaderCount := SenderField.HeaderCount + 1;
        SenderField.LastHeaderPOS := ActiveHeader.CurrentHeader;
        SenderField.RHeader.LastModifyTime := umlDefaultTime;
        ActiveHeader.PositionID := db_Header_LastPositionFlags;
        if dbField_WriteRec(SenderField.RHeader.CurrentHeader, IOHnd, SenderField) = False then
          begin
            Result := False;
            Exit;
          end;
        if dbHeader_WriteRec(ActiveHeader.CurrentHeader, IOHnd, ActiveHeader) = False then
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

function dbField_CreateField(const Name: U_String; const fPos: Int64; var IOHnd: TIOHnd; var SenderField: TField): Boolean;
begin
  if dbField_CreateHeader(Name, db_Header_Field_ID, fPos, IOHnd, SenderField.RHeader) = False then
    begin
      SenderField.Return := SenderField.RHeader.Return;
      Result := False;
      Exit;
    end;

  SenderField.HeaderCount := 0;
  SenderField.UpLevelFieldPOS := fPos;
  if dbField_OnlyWriteFieldRec(SenderField.RHeader.DataMainPOS, IOHnd, SenderField) = False then
    begin
      Result := False;
      Exit;
    end;
  SenderField.Return := db_Field_ok;
  Result := True;
end;

function dbField_InsertNewField(const Name: U_String; const FieldPos, CurrentInsertPos: Int64; var IOHnd: TIOHnd; var SenderField: TField): Boolean;
begin
  if dbField_InsertNewHeader(Name, db_Header_Field_ID, FieldPos, CurrentInsertPos, IOHnd, SenderField.RHeader) = False then
    begin
      SenderField.Return := SenderField.RHeader.Return;
      Result := False;
      Exit;
    end;

  SenderField.HeaderCount := 0;
  SenderField.UpLevelFieldPOS := FieldPos;
  if dbField_OnlyWriteFieldRec(SenderField.RHeader.DataMainPOS, IOHnd, SenderField) = False then
    begin
      Result := False;
      Exit;
    end;
  SenderField.Return := db_Field_ok;
  Result := True;
end;

function dbField_CreateItem(const Name: U_String; const ExterID: Byte; const fPos: Int64; var IOHnd: TIOHnd; var SenderItem: TItem): Boolean;
begin
  if dbField_CreateHeader(Name, db_Header_Item_ID, fPos, IOHnd, SenderItem.RHeader) = False then
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
  if dbItem_OnlyWriteItemRec(SenderItem.RHeader.DataMainPOS, IOHnd, SenderItem) = False then
    begin
      Result := False;
      Exit;
    end;
  SenderItem.DataWrited := True;
  SenderItem.Return := db_Item_ok;
  Result := True;
end;

function dbField_InsertNewItem(const Name: U_String; const ExterID: Byte; const FieldPos, CurrentInsertPos: Int64; var IOHnd: TIOHnd; var SenderItem: TItem): Boolean;
begin
  if dbField_InsertNewHeader(Name, db_Header_Item_ID, FieldPos, CurrentInsertPos, IOHnd, SenderItem.RHeader) = False then
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
  if dbItem_OnlyWriteItemRec(SenderItem.RHeader.DataMainPOS, IOHnd, SenderItem) = False then
    begin
      Result := False;
      Exit;
    end;
  SenderItem.DataWrited := True;
  SenderItem.Return := db_Item_ok;
  Result := True;
end;

function dbField_CopyItem(var SenderItem: TItem; var IOHnd: TIOHnd; const DestFieldPos: Int64; var DestRecFile: TIOHnd): Boolean;
var
  i: Integer;
  NewItemHnd: TItem;
  buff: array [0 .. C_MaxBufferFragmentSize] of Byte;
begin
  Init_TItem(NewItemHnd);
  NewItemHnd := SenderItem;
  if dbField_CreateItem(SenderItem.RHeader.Name, SenderItem.ExtID, DestFieldPos, DestRecFile, NewItemHnd) = False then
    begin
      SenderItem.Return := NewItemHnd.Return;
      Result := False;
      Exit;
    end;
  if dbItem_BlockSeekStartPOS(IOHnd, SenderItem) = False then
    begin
      Result := False;
      Exit;
    end;
  if SenderItem.Size > C_MaxBufferFragmentSize then
    begin
      for i := 1 to (SenderItem.Size div C_MaxBufferFragmentSize) do
        begin
          if dbItem_BlockReadData(IOHnd, SenderItem, buff, C_MaxBufferFragmentSize) = False then
            begin
              Result := False;
              Exit;
            end;
          if dbItem_BlockAppendWriteData(DestRecFile, NewItemHnd, buff, C_MaxBufferFragmentSize) = False then
            begin
              SenderItem.Return := NewItemHnd.Return;
              Result := False;
              Exit;
            end;
        end;
      if (SenderItem.Size mod C_MaxBufferFragmentSize) > 0 then
        begin
          if dbItem_BlockReadData(IOHnd, SenderItem, buff, SenderItem.Size mod C_MaxBufferFragmentSize) = False then
            begin
              Result := False;
              Exit;
            end;
          if dbItem_BlockAppendWriteData(DestRecFile, NewItemHnd, buff, SenderItem.Size mod C_MaxBufferFragmentSize) = False then
            begin
              SenderItem.Return := NewItemHnd.Return;
              Result := False;
              Exit;
            end;
        end;
    end
  else
    begin
      if dbItem_BlockReadData(IOHnd, SenderItem, buff, SenderItem.Size) = False then
        begin
          Result := False;
          Exit;
        end;
      if dbItem_BlockAppendWriteData(DestRecFile, NewItemHnd, buff, SenderItem.Size) = False then
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

function dbField_CopyItemBuffer(var SenderItem: TItem; var IOHnd: TIOHnd; var DestItemHnd: TItem; var DestRecFile: TIOHnd): Boolean;
var
  i: Integer;
  buff: array [0 .. C_MaxBufferFragmentSize] of Byte;
begin
  if dbItem_BlockSeekStartPOS(IOHnd, SenderItem) = False then
    begin
      Result := False;
      Exit;
    end;
  if SenderItem.Size > C_MaxBufferFragmentSize then
    begin
      for i := 1 to (SenderItem.Size div C_MaxBufferFragmentSize) do
        begin
          if dbItem_BlockReadData(IOHnd, SenderItem, buff, C_MaxBufferFragmentSize) = False then
            begin
              Result := False;
              Exit;
            end;
          if dbItem_BlockAppendWriteData(DestRecFile, DestItemHnd, buff, C_MaxBufferFragmentSize) = False then
            begin
              SenderItem.Return := DestItemHnd.Return;
              Result := False;
              Exit;
            end;
        end;
      if (SenderItem.Size mod C_MaxBufferFragmentSize) > 0 then
        begin
          if dbItem_BlockReadData(IOHnd, SenderItem, buff, SenderItem.Size mod C_MaxBufferFragmentSize) = False then
            begin
              Result := False;
              Exit;
            end;
          if dbItem_BlockAppendWriteData(DestRecFile, DestItemHnd, buff, SenderItem.Size mod C_MaxBufferFragmentSize) = False then
            begin
              SenderItem.Return := DestItemHnd.Return;
              Result := False;
              Exit;
            end;
        end;
    end
  else
    begin
      if dbItem_BlockReadData(IOHnd, SenderItem, buff, SenderItem.Size) = False then
        begin
          Result := False;
          Exit;
        end;
      if dbItem_BlockAppendWriteData(DestRecFile, DestItemHnd, buff, SenderItem.Size) = False then
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

function dbField_CopyAllTo(const FilterName: U_String; const FieldPos: Int64; var IOHnd: TIOHnd; const DestFieldPos: Int64; var DestRecFile: TIOHnd): Boolean;
var
  fs: TFieldSearch;
  NewField: TField;
  NewItem: TItem;
begin
  Init_TFieldSearch(fs);
  if dbField_OnlyFindFirstName(FilterName, FieldPos, IOHnd, fs) then
    begin
      repeat
        case fs.ID of
          db_Header_Field_ID:
            begin
              Init_TField(NewField);
              if dbField_ReadRec(fs.RHeader.CurrentHeader, IOHnd, NewField) then
                if dbField_CreateField(fs.RHeader.Name, DestFieldPos, DestRecFile, NewField) then
                    dbField_CopyAllTo(FilterName, fs.RHeader.CurrentHeader, IOHnd, NewField.RHeader.CurrentHeader, DestRecFile);
            end;
          db_Header_Item_ID:
            begin
              if dbItem_ReadRec(fs.RHeader.CurrentHeader, IOHnd, NewItem) then
                begin
                  dbField_CopyItem(NewItem, IOHnd, DestFieldPos, DestRecFile);
                end;
            end;
        end;
      until not dbField_OnlyFindNextName(IOHnd, fs);
    end;
  Result := True;
end;

function dbPack_CreatePack(const Name, Description: U_String; var SenderTMDB: TTMDB): Boolean;
begin
  if umlFileTest(SenderTMDB.IOHnd) then
    begin
      SenderTMDB.Return := db_Pack_RepCreatePackError;
      Result := False;
      Exit;
    end;
  if umlFileCreate(Name, SenderTMDB.IOHnd) = False then
    begin
      SenderTMDB.Return := db_Pack_CreatePackError;
      Result := False;
      Exit;
    end;
  SenderTMDB.MajorVer := db_Pack_MajorVersion;
  SenderTMDB.MinorVer := db_Pack_MinorVersion;
  SenderTMDB.CreateTime := umlDefaultTime;
  SenderTMDB.LastModifyTime := SenderTMDB.CreateTime;
  SenderTMDB.RootHeaderCount := 0;
  SenderTMDB.DefaultFieldPOS := db_Pack_Size;
  SenderTMDB.LastHeaderPOS := SenderTMDB.DefaultFieldPOS;
  SenderTMDB.FirstHeaderPOS := SenderTMDB.DefaultFieldPOS;
  SenderTMDB.CurrentFieldPOS := SenderTMDB.DefaultFieldPOS;
  if dbPack_WriteRec(0, SenderTMDB.IOHnd, SenderTMDB) = False then
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

function dbPack_OpenPack(const Name: U_String; var SenderTMDB: TTMDB; _OnlyRead: Boolean): Boolean;
begin
  if umlFileTest(SenderTMDB.IOHnd) then
    begin
      SenderTMDB.Return := db_Pack_RepOpenPackError;
      Result := False;
      Exit;
    end;

  if umlFileOpen(Name, SenderTMDB.IOHnd, _OnlyRead) = False then
    begin
      SenderTMDB.Return := db_Pack_OpenPackError;
      Result := False;
      Exit;
    end;

  if dbPack_ReadRec(0, SenderTMDB.IOHnd, SenderTMDB) = False then
    begin
      Result := False;
      Exit;
    end;

  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_CreateAsStream(stream: U_Stream; const Name, Description: U_String; var SenderTMDB: TTMDB): Boolean;
begin
  if umlFileTest(SenderTMDB.IOHnd) then
    begin
      SenderTMDB.Return := db_Pack_RepCreatePackError;
      Result := False;
      Exit;
    end;
  if umlFileCreateAsStream(Name, stream, SenderTMDB.IOHnd) = False then
    begin
      SenderTMDB.Return := db_Pack_CreatePackError;
      Result := False;
      Exit;
    end;
  SenderTMDB.MajorVer := db_Pack_MajorVersion;
  SenderTMDB.MinorVer := db_Pack_MinorVersion;
  SenderTMDB.CreateTime := umlDefaultTime;
  SenderTMDB.LastModifyTime := SenderTMDB.CreateTime;
  SenderTMDB.RootHeaderCount := 0;
  SenderTMDB.DefaultFieldPOS := db_Pack_Size;
  SenderTMDB.LastHeaderPOS := SenderTMDB.DefaultFieldPOS;
  SenderTMDB.FirstHeaderPOS := SenderTMDB.DefaultFieldPOS;
  SenderTMDB.CurrentFieldPOS := SenderTMDB.DefaultFieldPOS;
  if dbPack_WriteRec(0, SenderTMDB.IOHnd, SenderTMDB) = False then
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

function dbPack_OpenAsStream(stream: U_Stream; const Name: U_String; var SenderTMDB: TTMDB; _OnlyRead: Boolean): Boolean;
begin
  if umlFileTest(SenderTMDB.IOHnd) then
    begin
      SenderTMDB.Return := db_Pack_RepOpenPackError;
      Result := False;
      Exit;
    end;

  if umlFileOpenAsStream(Name, stream, SenderTMDB.IOHnd, _OnlyRead) = False then
    begin
      SenderTMDB.Return := db_Pack_OpenPackError;
      Result := False;
      Exit;
    end;

  if dbPack_ReadRec(0, SenderTMDB.IOHnd, SenderTMDB) = False then
    begin
      Result := False;
      Exit;
    end;

  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_ClosePack(var SenderTMDB: TTMDB): Boolean;
begin
  if umlFileTest(SenderTMDB.IOHnd) = False then
    begin
      SenderTMDB.Return := db_Pack_ClosePackError;
      Result := False;
      Exit;
    end;
  if SenderTMDB.IOHnd.WriteFlag then
    begin
      SenderTMDB.LastModifyTime := umlDefaultTime;
      if dbPack_WriteRec(0, SenderTMDB.IOHnd, SenderTMDB) = False then
        begin
          Result := False;
          Exit;
        end;
    end;
  if umlFileClose(SenderTMDB.IOHnd) = False then
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
  if umlFileTest(SenderTMDB.IOHnd) = False then
    begin
      SenderTMDB.Return := db_Pack_ClosePackError;
      Result := False;
      Exit;
    end;
  if SenderTMDB.IOHnd.WriteFlag then
    begin
      SenderTMDB.LastModifyTime := umlDefaultTime;
      if dbPack_WriteRec(0, SenderTMDB.IOHnd, SenderTMDB) = False then
        begin
          Result := False;
          Exit;
        end;
    end;
  SenderTMDB.Return := db_Pack_ok;
  Result := umlFileUpdate(SenderTMDB.IOHnd);
end;

function dbPack_CopyFieldTo(const FilterName: U_String; var SenderTMDB: TTMDB; const SourceFieldPos: Int64; var DestTMDB: TTMDB; const DestFieldPos: Int64): Boolean;
begin
  if dbField_CopyAllTo(FilterName, SourceFieldPos, SenderTMDB.IOHnd, DestFieldPos, DestTMDB.IOHnd) then
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

function dbPack_CopyAllToDestPath(var SenderTMDB: TTMDB; var DestTMDB: TTMDB; destPath: U_String): Boolean;
var
  f: TField;
begin
  Result := False;
  dbPack_CreateField(destPath, '', DestTMDB);
  if dbPack_GetField(destPath, f, DestTMDB) then
    begin
      Result := dbPack_CopyFieldTo('*', SenderTMDB, SenderTMDB.DefaultFieldPOS, DestTMDB, f.RHeader.CurrentHeader);
    end;
end;

function dbPack_TestNameStr(const Name: U_String): Boolean;
begin
  Result := umlDeleteChar(Name, TPascalString(db_FieldPathLimitChar + #9#32#13#10)).Len > 0;
end;

function dbPack_AutoCheckRootField(const Name: U_String; var SenderField: TField; var SenderTMDB: TTMDB): Boolean;
var
  f: TField;
begin
  if dbPack_TestNameStr(Name) = False then
    begin
      SenderTMDB.Return := db_Pack_PathNameError;
      SenderField.Return := SenderTMDB.Return;
      Result := False;
      Exit;
    end;

  if dbPack_GetRootField(Name, f, SenderTMDB) = False then
    begin
      if dbPack_CreateRootHeader(Name, db_Header_Field_ID, SenderTMDB, SenderField.RHeader) = False then
        begin
          SenderTMDB.Return := SenderField.RHeader.Return;
          SenderField.Return := SenderTMDB.Return;
          Result := False;
          Exit;
        end;
      SenderField.HeaderCount := 0;
      SenderField.UpLevelFieldPOS := -1;
      if dbField_OnlyWriteFieldRec(SenderField.RHeader.DataMainPOS, SenderTMDB.IOHnd, SenderField) = False then
        begin
          SenderTMDB.Return := SenderField.Return;
          SenderField.Return := SenderTMDB.Return;
          Result := False;
          Exit;
        end;
    end
  else
    begin
      SenderField := f;
    end;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_CreateRootHeader(const Name: U_String; const ID: Byte; var SenderTMDB: TTMDB; var SenderHeader: THeader): Boolean;
var
  _Header: THeader;
begin
  SenderHeader.ID := ID;
  SenderHeader.Name := Name;
  case SenderTMDB.RootHeaderCount of
    0:
      begin
        SenderTMDB.RootHeaderCount := 1;
        SenderTMDB.FirstHeaderPOS := umlFileGetSize(SenderTMDB.IOHnd);
        SenderTMDB.LastHeaderPOS := SenderTMDB.FirstHeaderPOS;
        SenderTMDB.LastModifyTime := umlDefaultTime;
        SenderHeader.PositionID := db_Header_OnlyPositionFlags;
        SenderHeader.NextHeader := SenderTMDB.LastHeaderPOS;
        SenderHeader.PrevHeader := SenderTMDB.FirstHeaderPOS;
        SenderHeader.CurrentHeader := SenderTMDB.FirstHeaderPOS;
        SenderHeader.CreateTime := umlDefaultTime;
        SenderHeader.LastModifyTime := umlDefaultTime;
        SenderHeader.DataMainPOS := SenderHeader.CurrentHeader + db_Header_Size;
        if dbHeader_WriteRec(SenderHeader.CurrentHeader, SenderTMDB.IOHnd, SenderHeader) = False then
          begin
            Result := False;
            Exit;
          end;
      end;
    1:
      begin
        SenderHeader.CurrentHeader := umlFileGetSize(SenderTMDB.IOHnd);
        SenderHeader.NextHeader := SenderTMDB.FirstHeaderPOS;
        SenderHeader.PrevHeader := SenderTMDB.FirstHeaderPOS;
        if dbHeader_ReadRec(SenderTMDB.FirstHeaderPOS, SenderTMDB.IOHnd, _Header) = False then
          begin
            SenderHeader.Return := _Header.Return;
            Result := False;
            Exit;
          end;
        _Header.PrevHeader := SenderHeader.CurrentHeader;
        _Header.NextHeader := SenderHeader.CurrentHeader;
        _Header.PositionID := db_Header_FirstPositionFlags;
        if dbHeader_WriteRec(SenderTMDB.FirstHeaderPOS, SenderTMDB.IOHnd, _Header) = False then
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
        SenderHeader.DataMainPOS := SenderHeader.CurrentHeader + db_Header_Size;
        SenderHeader.PositionID := db_Header_LastPositionFlags;
        if dbHeader_WriteRec(SenderHeader.CurrentHeader, SenderTMDB.IOHnd, SenderHeader) = False then
          begin
            Result := False;
            Exit;
          end;
      end;
    else
      begin
        SenderHeader.CurrentHeader := umlFileGetSize(SenderTMDB.IOHnd);
        if dbHeader_ReadRec(SenderTMDB.FirstHeaderPOS, SenderTMDB.IOHnd, _Header) = False then
          begin
            SenderHeader.Return := _Header.Return;
            Result := False;
            Exit;
          end;
        _Header.PrevHeader := SenderHeader.CurrentHeader;
        SenderHeader.NextHeader := _Header.CurrentHeader;
        if dbHeader_WriteRec(SenderTMDB.FirstHeaderPOS, SenderTMDB.IOHnd, _Header) = False then
          begin
            SenderHeader.Return := _Header.Return;
            Result := False;
            Exit;
          end;
        if dbHeader_ReadRec(SenderTMDB.LastHeaderPOS, SenderTMDB.IOHnd, _Header) = False then
          begin
            SenderHeader.Return := _Header.Return;
            Result := False;
            Exit;
          end;
        _Header.NextHeader := SenderHeader.CurrentHeader;
        SenderHeader.PrevHeader := SenderTMDB.LastHeaderPOS;
        _Header.PositionID := db_Header_MediumPositionFlags;
        if dbHeader_WriteRec(SenderTMDB.LastHeaderPOS, SenderTMDB.IOHnd, _Header) = False then
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
        SenderHeader.DataMainPOS := SenderHeader.CurrentHeader + db_Header_Size;
        SenderHeader.PositionID := db_Header_LastPositionFlags;
        if dbHeader_WriteRec(SenderHeader.CurrentHeader, SenderTMDB.IOHnd, SenderHeader) = False then
          begin
            Result := False;
            Exit;
          end;
      end;
  end;

  SenderHeader.Return := db_Header_ok;
  Result := True;
end;

function dbPack_CreateRootField(const Name, Description: U_String; var SenderTMDB: TTMDB): Boolean;
var
  f: TField;
begin
  if dbPack_TestNameStr(Name) = False then
    begin
      SenderTMDB.Return := db_Pack_PathNameError;
      Result := False;
      Exit;
    end;

  if dbPack_ExistsRootField(Name, SenderTMDB) then
    begin
      SenderTMDB.Return := db_Pack_PathNameError;
      Result := False;
      Exit;
    end;
  if dbPack_CreateRootHeader(Name, db_Header_Field_ID, SenderTMDB, f.RHeader) = False then
    begin
      SenderTMDB.Return := f.RHeader.Return;
      Result := False;
      Exit;
    end;
  f.Description := Description;
  f.HeaderCount := 0;
  f.UpLevelFieldPOS := -1;
  if dbField_OnlyWriteFieldRec(f.RHeader.DataMainPOS, SenderTMDB.IOHnd, f) = False then
    begin
      SenderTMDB.Return := f.Return;
      Result := False;
      Exit;
    end;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_CreateAndSetRootField(const Name, Description: U_String; var SenderTMDB: TTMDB): Boolean;
var
  f: TField;
begin
  if dbPack_TestNameStr(Name) = False then
    begin
      SenderTMDB.Return := db_Pack_PathNameError;
      Result := False;
      Exit;
    end;

  if dbPack_ExistsRootField(Name, SenderTMDB) then
    begin
      SenderTMDB.Return := db_Pack_PathNameError;
      Result := False;
      Exit;
    end;
  if dbPack_CreateRootHeader(Name, db_Header_Field_ID, SenderTMDB, f.RHeader) = False then
    begin
      SenderTMDB.Return := f.RHeader.Return;
      Result := False;
      Exit;
    end;
  f.Description := Description;
  f.HeaderCount := 0;
  f.UpLevelFieldPOS := -1;
  if dbField_OnlyWriteFieldRec(f.RHeader.DataMainPOS, SenderTMDB.IOHnd, f) = False then
    begin
      SenderTMDB.Return := f.Return;
      Result := False;
      Exit;
    end;
  SenderTMDB.DefaultFieldPOS := f.RHeader.CurrentHeader;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_CreateField(const pathName, Description: U_String; var SenderTMDB: TTMDB): Boolean;
var
  f: TField;
  fs: TFieldSearch;
  i, PC: Integer;
  TempPathStr, TempPathName: U_String;
begin
  if umlFileTest(SenderTMDB.IOHnd) = False then
    begin
      SenderTMDB.Return := db_Pack_ClosePackError;
      Result := False;
      Exit;
    end;
  if dbField_ReadRec(SenderTMDB.DefaultFieldPOS, SenderTMDB.IOHnd, f) = False then
    begin
      SenderTMDB.Return := f.Return;
      Result := False;
      Exit;
    end;

  if umlGetLength(pathName) = 0 then
    begin
      SenderTMDB.Return := db_Pack_ok;
      Result := True;
      Exit;
    end;
  TempPathName := pathName;
  PC := dbPack_GetPathCount(TempPathName);
  if PC > 0 then
    begin
      for i := 1 to PC do
        begin
          TempPathStr := dbPack_GetFirstPath(TempPathName);
          TempPathName := dbPack_DeleteFirstPath(TempPathName);

          if dbPack_TestNameStr(TempPathStr) = False then
            begin
              SenderTMDB.Return := db_Pack_PathNameError;
              Result := False;
              Exit;
            end;
          case dbField_FindFirst(TempPathStr, db_Header_Field_ID, f.RHeader.CurrentHeader, SenderTMDB.IOHnd, fs) of
            False:
              begin
                f.Description := Description;
                if dbField_CreateField(TempPathStr, f.RHeader.CurrentHeader, SenderTMDB.IOHnd, f) = False then
                  begin
                    SenderTMDB.Return := f.Return;
                    Result := False;
                    Exit;
                  end;
              end;
            True:
              begin
                if dbField_ReadRec(fs.RHeader.CurrentHeader, SenderTMDB.IOHnd, f) = False then
                  begin
                    SenderTMDB.Return := f.Return;
                    Result := False;
                    Exit;
                  end;
              end;
          end;
        end;
    end;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_SetFieldName(const pathName, OriginFieldName, NewFieldName, FieldDescription: U_String; var SenderTMDB: TTMDB): Boolean;
var
  TempSR: TFieldSearch;
  f: TField;
  OriginField: TField;
begin
  if dbPack_GetField(pathName, f, SenderTMDB) = False then
    begin
      Result := False;
      Exit;
    end;
  if dbField_FindFirst(OriginFieldName, db_Header_Field_ID, f.RHeader.CurrentHeader, SenderTMDB.IOHnd, TempSR) = False then
    begin
      SenderTMDB.Return := TempSR.Return;
      Result := False;
      Exit;
    end;
  if dbField_ReadRec(TempSR.RHeader.CurrentHeader, SenderTMDB.IOHnd, OriginField) = False then
    begin
      SenderTMDB.Return := OriginField.RHeader.Return;
      Result := False;
      Exit;
    end;
  OriginField.RHeader.Name := NewFieldName;
  OriginField.Description := FieldDescription;
  if dbField_WriteRec(OriginField.RHeader.CurrentHeader, SenderTMDB.IOHnd, OriginField) = False then
    begin
      SenderTMDB.Return := OriginField.RHeader.Return;
      Result := False;
      Exit;
    end;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_SetItemName(const pathName, OriginItemName, NewItemName, ItemDescription: U_String; var SenderTMDB: TTMDB): Boolean;
var
  TempSR: TFieldSearch;
  f: TField;
  OriginItem: TItem;
begin
  if dbPack_GetField(pathName, f, SenderTMDB) = False then
    begin
      Result := False;
      Exit;
    end;
  if dbField_FindFirst(OriginItemName, db_Header_Item_ID, f.RHeader.CurrentHeader, SenderTMDB.IOHnd, TempSR) = False then
    begin
      SenderTMDB.Return := TempSR.Return;
      Result := False;
      Exit;
    end;
  if dbItem_ReadRec(TempSR.RHeader.CurrentHeader, SenderTMDB.IOHnd, OriginItem) = False then
    begin
      SenderTMDB.Return := OriginItem.RHeader.Return;
      Result := False;
      Exit;
    end;
  OriginItem.RHeader.Name := NewItemName;
  OriginItem.Description := ItemDescription;
  if dbItem_WriteRec(OriginItem.RHeader.CurrentHeader, SenderTMDB.IOHnd, OriginItem) = False then
    begin
      SenderTMDB.Return := OriginItem.RHeader.Return;
      Result := False;
      Exit;
    end;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_DeleteField(const pathName, FilterName: U_String; var SenderTMDB: TTMDB): Boolean;
var
  TempSR: TFieldSearch;
  f: TField;
begin
  if dbPack_GetField(pathName, f, SenderTMDB) = False then
    begin
      Result := False;
      Exit;
    end;
  if dbField_FindFirst(FilterName, db_Header_Field_ID, f.RHeader.CurrentHeader, SenderTMDB.IOHnd, TempSR) = False then
    begin
      SenderTMDB.Return := TempSR.Return;
      Result := False;
      Exit;
    end;
  if dbField_DeleteHeader(TempSR.RHeader.CurrentHeader, f.RHeader.CurrentHeader, SenderTMDB.IOHnd, f) = False then
    begin
      SenderTMDB.Return := f.Return;
      Result := False;
      Exit;
    end;
  while dbField_FindFirst(FilterName, db_Header_Field_ID, f.RHeader.CurrentHeader, SenderTMDB.IOHnd, TempSR) do
    begin
      if dbField_DeleteHeader(TempSR.RHeader.CurrentHeader, f.RHeader.CurrentHeader, SenderTMDB.IOHnd, f) = False then
        begin
          SenderTMDB.Return := f.Return;
          Result := False;
          Exit;
        end;
    end;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_DeleteHeader(const pathName, FilterName: U_String; const ID: Byte; var SenderTMDB: TTMDB): Boolean;
var
  TempSR: TFieldSearch;
  f: TField;
begin
  if dbPack_GetField(pathName, f, SenderTMDB) = False then
    begin
      Result := False;
      Exit;
    end;
  if dbField_FindFirst(FilterName, ID, f.RHeader.CurrentHeader, SenderTMDB.IOHnd, TempSR) = False then
    begin
      SenderTMDB.Return := TempSR.Return;
      Result := False;
      Exit;
    end;
  if dbField_DeleteHeader(TempSR.RHeader.CurrentHeader, f.RHeader.CurrentHeader, SenderTMDB.IOHnd, f) = False then
    begin
      SenderTMDB.Return := f.Return;
      Result := False;
      Exit;
    end;
  while dbField_FindFirst(FilterName, ID, f.RHeader.CurrentHeader, SenderTMDB.IOHnd, TempSR) do
    begin
      if dbField_DeleteHeader(TempSR.RHeader.CurrentHeader, f.RHeader.CurrentHeader, SenderTMDB.IOHnd, f) = False then
        begin
          SenderTMDB.Return := f.Return;
          Result := False;
          Exit;
        end;
    end;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_MoveItem(const SourcerPathName, FilterName: U_String; const TargetPathName: U_String; const ItemExtID: Byte; var SenderTMDB: TTMDB): Boolean;
var
  TempSR: TFieldSearch;
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
  if dbField_FindFirstItem(FilterName, ItemExtID, SourcerField.RHeader.CurrentHeader, SenderTMDB.IOHnd, TempSR) = False then
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
  if dbField_MoveHeader(TempSR.RHeader.CurrentHeader, SourcerField.RHeader.CurrentHeader, TargetField.RHeader.CurrentHeader, SenderTMDB.IOHnd, TargetField) = False then
    begin
      SenderTMDB.Return := TargetField.Return;
      Result := False;
      Exit;
    end;
  while dbField_FindFirstItem(FilterName, ItemExtID, SourcerField.RHeader.CurrentHeader, SenderTMDB.IOHnd, TempSR) do
    begin
      if TempSR.RHeader.CurrentHeader = TargetField.RHeader.CurrentHeader then
        begin
          SenderTMDB.Return := db_Pack_PathNameError;
          Result := False;
          Exit;
        end;
      if dbField_MoveHeader(TempSR.RHeader.CurrentHeader, SourcerField.RHeader.CurrentHeader, TargetField.RHeader.CurrentHeader, SenderTMDB.IOHnd, TargetField) = False
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

function dbPack_MoveField(const SourcerPathName, FilterName: U_String; const TargetPathName: U_String; var SenderTMDB: TTMDB): Boolean;
var
  TempSR: TFieldSearch;
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
  if dbField_FindFirst(FilterName, db_Header_Field_ID, SourcerField.RHeader.CurrentHeader, SenderTMDB.IOHnd, TempSR) = False then
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
  if dbField_MoveHeader(TempSR.RHeader.CurrentHeader, SourcerField.RHeader.CurrentHeader, TargetField.RHeader.CurrentHeader, SenderTMDB.IOHnd, TargetField) = False then
    begin
      SenderTMDB.Return := TargetField.Return;
      Result := False;
      Exit;
    end;
  while dbField_FindFirst(FilterName, db_Header_Field_ID, SourcerField.RHeader.CurrentHeader, SenderTMDB.IOHnd, TempSR) do
    begin
      if TempSR.RHeader.CurrentHeader = TargetField.RHeader.CurrentHeader then
        begin
          SenderTMDB.Return := db_Pack_PathNameError;
          Result := False;
          Exit;
        end;
      if dbField_MoveHeader(TempSR.RHeader.CurrentHeader, SourcerField.RHeader.CurrentHeader, TargetField.RHeader.CurrentHeader, SenderTMDB.IOHnd, TargetField) = False
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

function dbPack_MoveHeader(const SourcerPathName, FilterName: U_String; const TargetPathName: U_String; const HeaderID: Byte; var SenderTMDB: TTMDB): Boolean;
var
  TempSR: TFieldSearch;
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
  if dbField_FindFirst(FilterName, HeaderID, SourcerField.RHeader.CurrentHeader, SenderTMDB.IOHnd, TempSR) = False then
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
  if dbField_MoveHeader(TempSR.RHeader.CurrentHeader, SourcerField.RHeader.CurrentHeader, TargetField.RHeader.CurrentHeader, SenderTMDB.IOHnd, TargetField) = False then
    begin
      SenderTMDB.Return := TargetField.Return;
      Result := False;
      Exit;
    end;
  while dbField_FindFirst(FilterName, HeaderID, SourcerField.RHeader.CurrentHeader, SenderTMDB.IOHnd, TempSR) do
    begin
      if TempSR.RHeader.CurrentHeader = TargetField.RHeader.CurrentHeader then
        begin
          SenderTMDB.Return := db_Pack_PathNameError;
          Result := False;
          Exit;
        end;
      if dbField_MoveHeader(TempSR.RHeader.CurrentHeader, SourcerField.RHeader.CurrentHeader, TargetField.RHeader.CurrentHeader, SenderTMDB.IOHnd, TargetField) = False
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

function dbPack_SetCurrentRootField(const Name: U_String; var SenderTMDB: TTMDB): Boolean;
var
  f: TField;
begin
  if SenderTMDB.RootHeaderCount = 0 then
    begin
      SenderTMDB.Return := db_Pack_PathNameError;
      Result := False;
      Exit;
    end;
  if dbHeader_ReadRec(SenderTMDB.DefaultFieldPOS, SenderTMDB.IOHnd, f.RHeader) = False then
    begin
      SenderTMDB.Return := f.RHeader.Return;
      Result := False;
      Exit;
    end;

  if (dbMultipleMatch(Name, f.RHeader.Name) = True) and (f.RHeader.ID = db_Header_Field_ID) then
    begin
      SenderTMDB.DefaultFieldPOS := f.RHeader.CurrentHeader;
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
  if dbHeader_ReadRec(f.RHeader.NextHeader, SenderTMDB.IOHnd, f.RHeader) = False then
    begin
      SenderTMDB.Return := f.RHeader.Return;
      Result := False;
      Exit;
    end;

  if (dbMultipleMatch(Name, f.RHeader.Name) = True) and (f.RHeader.ID = db_Header_Field_ID) then
    begin
      SenderTMDB.DefaultFieldPOS := f.RHeader.CurrentHeader;
      SenderTMDB.Return := db_Pack_ok;
      Result := True;
      Exit;
    end;

  while f.RHeader.CurrentHeader <> SenderTMDB.DefaultFieldPOS do
    begin
      if dbHeader_ReadRec(f.RHeader.NextHeader, SenderTMDB.IOHnd, f.RHeader) = False then
        begin
          SenderTMDB.Return := f.RHeader.Return;
          Result := False;
          Exit;
        end;
      if (dbMultipleMatch(Name, f.RHeader.Name) = True) and (f.RHeader.ID = db_Header_Field_ID) then
        begin
          SenderTMDB.DefaultFieldPOS := f.RHeader.CurrentHeader;
          SenderTMDB.Return := db_Pack_ok;
          Result := True;
          Exit;
        end;
    end;
  SenderTMDB.Return := db_Pack_PathNameError;
  Result := False;
end;

function dbPack_SetCurrentField(const pathName: U_String; var SenderTMDB: TTMDB): Boolean;
var
  f: TField;
  fs: TFieldSearch;
  i, PC: Integer;
  TempPathStr, TempPathName: U_String;
begin
  if umlFileTest(SenderTMDB.IOHnd) = False then
    begin
      SenderTMDB.Return := db_Pack_ClosePackError;
      Result := False;
      Exit;
    end;
  if dbField_ReadRec(SenderTMDB.DefaultFieldPOS, SenderTMDB.IOHnd, f) = False then
    begin
      SenderTMDB.Return := f.Return;
      Result := False;
      Exit;
    end;

  if umlGetLength(pathName) = 0 then
    begin
      SenderTMDB.CurrentFieldPOS := f.RHeader.CurrentHeader;
      SenderTMDB.CurrentFieldLevel := 1;
      SenderTMDB.Return := db_Pack_ok;
      Result := True;
      Exit;
    end;
  TempPathName := pathName;
  PC := dbPack_GetPathCount(TempPathName);
  if PC > 0 then
    begin
      for i := 1 to PC do
        begin
          TempPathStr := dbPack_GetFirstPath(TempPathName);
          TempPathName := dbPack_DeleteFirstPath(TempPathName);

          if dbPack_TestNameStr(TempPathStr) = False then
            begin
              SenderTMDB.Return := db_Pack_PathNameError;
              Result := False;
              Exit;
            end;
          if dbField_FindFirst(TempPathStr, db_Header_Field_ID, f.RHeader.CurrentHeader, SenderTMDB.IOHnd, fs) = False then
            begin
              SenderTMDB.Return := fs.Return;
              Result := False;
              Exit;
            end;
          if dbField_ReadRec(fs.RHeader.CurrentHeader, SenderTMDB.IOHnd, f) = False then
            begin
              SenderTMDB.Return := f.Return;
              Result := False;
              Exit;
            end;
        end;
    end;
  SenderTMDB.CurrentFieldPOS := f.RHeader.CurrentHeader;
  SenderTMDB.CurrentFieldLevel := PC;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_GetRootField(const Name: U_String; var SenderField: TField; var SenderTMDB: TTMDB): Boolean;
var
  f: TField;
begin
  Init_TField(SenderField);
  Init_TField(f);

  if SenderTMDB.RootHeaderCount = 0 then
    begin
      SenderTMDB.Return := db_Pack_PathNameError;
      SenderField.Return := SenderTMDB.Return;
      Result := False;
      Exit;
    end;
  if dbHeader_ReadRec(SenderTMDB.DefaultFieldPOS, SenderTMDB.IOHnd, f.RHeader) = False then
    begin
      SenderTMDB.Return := f.RHeader.Return;
      SenderField.Return := SenderTMDB.Return;
      Result := False;
      Exit;
    end;

  if (dbMultipleMatch(Name, f.RHeader.Name) = True) and (f.RHeader.ID = db_Header_Field_ID) then
    begin
      SenderTMDB.Return := db_Pack_ok;
      SenderField := f;
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
  if dbHeader_ReadRec(f.RHeader.NextHeader, SenderTMDB.IOHnd, f.RHeader) = False then
    begin
      SenderTMDB.Return := f.RHeader.Return;
      SenderField.Return := SenderTMDB.Return;
      Result := False;
      Exit;
    end;

  if (dbMultipleMatch(Name, f.RHeader.Name) = True) and (f.RHeader.ID = db_Header_Field_ID) then
    begin
      SenderTMDB.Return := db_Pack_ok;
      SenderField := f;
      Result := True;
      Exit;
    end;

  while f.RHeader.CurrentHeader <> SenderTMDB.DefaultFieldPOS do
    begin
      if dbHeader_ReadRec(f.RHeader.NextHeader, SenderTMDB.IOHnd, f.RHeader) = False then
        begin
          SenderTMDB.Return := f.RHeader.Return;
          SenderField.Return := SenderTMDB.Return;
          Result := False;
          Exit;
        end;
      if (dbMultipleMatch(Name, f.RHeader.Name) = True) and (f.RHeader.ID = db_Header_Field_ID) then
        begin
          SenderTMDB.Return := db_Pack_ok;
          SenderField := f;
          Result := True;
          Exit;
        end;
    end;
  SenderTMDB.Return := db_Pack_PathNameError;
  SenderField.Return := SenderTMDB.Return;
  Result := False;
end;

function dbPack_GetField(const pathName: U_String; var SenderField: TField; var SenderTMDB: TTMDB): Boolean;
var
  f: TField;
  fs: TFieldSearch;
  i, PC: Integer;
  TempPathStr, TempPathName: U_String;
begin
  if umlFileTest(SenderTMDB.IOHnd) = False then
    begin
      SenderTMDB.Return := db_Pack_ClosePackError;
      Result := False;
      Exit;
    end;
  if dbField_ReadRec(SenderTMDB.DefaultFieldPOS, SenderTMDB.IOHnd, f) = False then
    begin
      SenderTMDB.Return := f.Return;
      Result := False;
      Exit;
    end;

  if umlGetLength(pathName) = 0 then
    begin
      SenderField := f;
      SenderTMDB.Return := db_Pack_ok;
      Result := True;
      Exit;
    end;
  TempPathName := pathName;
  PC := dbPack_GetPathCount(TempPathName);

  if PC > 0 then
    begin
      for i := 1 to PC do
        begin
          TempPathStr := dbPack_GetFirstPath(TempPathName);
          TempPathName := dbPack_DeleteFirstPath(TempPathName);

          if dbPack_TestNameStr(TempPathStr) = False then
            begin
              SenderTMDB.Return := db_Pack_PathNameError;
              Result := False;
              Exit;
            end;
          if dbField_FindFirst(TempPathStr, db_Header_Field_ID, f.RHeader.CurrentHeader, SenderTMDB.IOHnd, fs) = False then
            begin
              SenderTMDB.Return := fs.Return;
              Result := False;
              Exit;
            end;
          if dbField_ReadRec(fs.RHeader.CurrentHeader, SenderTMDB.IOHnd, f) = False then
            begin
              SenderTMDB.Return := f.Return;
              Result := False;
              Exit;
            end;
        end;
    end;
  SenderField := f;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_GetPath(const FieldPos, RootFieldPos: Int64; var SenderTMDB: TTMDB; var RetPath: U_String): Boolean;
var
  f: TField;
begin

  if dbHeader_ReadRec(FieldPos, SenderTMDB.IOHnd, f.RHeader) = False then
    begin
      SenderTMDB.Return := f.RHeader.Return;
      Result := False;
      Exit;
    end;

  if f.RHeader.ID <> db_Header_Field_ID then
    begin
      SenderTMDB.Return := db_Field_SetPosError;
      Result := False;
      Exit;
    end;
  if dbField_OnlyReadFieldRec(f.RHeader.DataMainPOS, SenderTMDB.IOHnd, f) = False then
    begin
      SenderTMDB.Return := f.Return;
      Result := False;
      Exit;
    end;

  if f.RHeader.CurrentHeader = RootFieldPos then
    begin
      RetPath := db_Path_Delimiter;
      SenderTMDB.Return := db_Pack_ok;
      Result := True;
      Exit;
    end;
  RetPath := f.RHeader.Name + db_Path_Delimiter;

  while dbField_ReadRec(f.UpLevelFieldPOS, SenderTMDB.IOHnd, f) do
    begin
      if f.RHeader.CurrentHeader = RootFieldPos then
        begin
          RetPath := db_Path_Delimiter + RetPath;
          SenderTMDB.Return := db_Pack_ok;
          Result := True;
          Exit;
        end;
      RetPath := f.RHeader.Name + db_Path_Delimiter + RetPath;
    end;
  SenderTMDB.Return := f.Return;
  Result := False;
end;

function dbPack_NewItem(const pathName, ItemName, ItemDescription: U_String; const ItemExtID: Byte; var SenderItem: TItem; var SenderTMDB: TTMDB): Boolean;
var
  f: TField;
  fs: TFieldSearch;
  i, PC: Integer;
  TempPathStr, TempPathName: U_String;
begin
  if umlFileTest(SenderTMDB.IOHnd) = False then
    begin
      SenderTMDB.Return := db_Pack_ClosePackError;
      Result := False;
      Exit;
    end;
  if dbField_ReadRec(SenderTMDB.DefaultFieldPOS, SenderTMDB.IOHnd, f) = False then
    begin
      SenderTMDB.Return := f.Return;
      Result := False;
      Exit;
    end;

  if umlGetLength(pathName) > 0 then
    begin
      TempPathName := pathName;
      PC := dbPack_GetPathCount(TempPathName);
      if PC > 0 then
        begin
          for i := 1 to PC do
            begin
              TempPathStr := dbPack_GetFirstPath(TempPathName);
              TempPathName := dbPack_DeleteFirstPath(TempPathName);

              if dbPack_TestNameStr(TempPathStr) = False then
                begin
                  SenderTMDB.Return := db_Pack_PathNameError;
                  Result := False;
                  Exit;
                end;
              case dbField_FindFirst(TempPathStr, db_Header_Field_ID, f.RHeader.CurrentHeader, SenderTMDB.IOHnd, fs) of
                False:
                  begin
                    f.Description := db_Pack_DefaultDescription;
                    if dbField_CreateField(TempPathStr, f.RHeader.CurrentHeader, SenderTMDB.IOHnd, f) = False then
                      begin
                        SenderTMDB.Return := f.Return;
                        Result := False;
                        Exit;
                      end;
                  end;
                True:
                  begin
                    if dbField_ReadRec(fs.RHeader.CurrentHeader, SenderTMDB.IOHnd, f) = False then
                      begin
                        SenderTMDB.Return := f.Return;
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
  if dbField_FindFirstItem(ItemName, ItemExtID, f.RHeader.CurrentHeader, SenderTMDB.IOHnd, fs) then
    begin
      if SenderTMDB.OverWriteItem = False then
        begin
          if SenderTMDB.AllowSameHeaderName = False then
            begin
              SenderTMDB.Return := db_Pack_RepeatCreateItemError;
              Result := False;
              Exit;
            end;
        end
      else
        begin
          if dbField_DeleteHeader(fs.RHeader.CurrentHeader, f.RHeader.CurrentHeader, SenderTMDB.IOHnd, f) = False then
            begin
              SenderTMDB.Return := f.Return;
              Result := False;
              Exit;
            end;
        end;
    end;
  SenderItem.Description := ItemDescription;
  if dbField_CreateItem(ItemName, ItemExtID, f.RHeader.CurrentHeader, SenderTMDB.IOHnd, SenderItem) = False then
    begin
      SenderTMDB.Return := SenderItem.Return;
      Result := False;
      Exit;
    end;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_DeleteItem(const pathName, FilterName: U_String; const ItemExtID: Byte; var SenderTMDB: TTMDB): Boolean;
var
  TempSR: TFieldSearch;
  f: TField;
begin
  if dbPack_GetField(pathName, f, SenderTMDB) = False then
    begin
      Result := False;
      Exit;
    end;
  if dbField_FindFirstItem(FilterName, ItemExtID, f.RHeader.CurrentHeader, SenderTMDB.IOHnd, TempSR) = False then
    begin
      SenderTMDB.Return := TempSR.Return;
      Result := False;
      Exit;
    end;
  if dbField_DeleteHeader(TempSR.RHeader.CurrentHeader, f.RHeader.CurrentHeader, SenderTMDB.IOHnd, f) = False then
    begin
      SenderTMDB.Return := f.Return;
      Result := False;
      Exit;
    end;
  while dbField_FindFirstItem(FilterName, ItemExtID, f.RHeader.CurrentHeader, SenderTMDB.IOHnd, TempSR) do
    begin
      if dbField_DeleteHeader(TempSR.RHeader.CurrentHeader, f.RHeader.CurrentHeader, SenderTMDB.IOHnd, f) = False then
        begin
          SenderTMDB.Return := f.Return;
          Result := False;
          Exit;
        end;
    end;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_GetItem(const pathName, ItemName: U_String; const ItemExtID: Byte; var SenderItem: TItem; var SenderTMDB: TTMDB): Boolean;
var
  f: TField;
  _FieldSR: TFieldSearch;
begin
  if dbPack_GetField(pathName, f, SenderTMDB) = False then
    begin
      Result := False;
      Exit;
    end;
  if dbField_FindFirstItem(ItemName, ItemExtID, f.RHeader.CurrentHeader, SenderTMDB.IOHnd, _FieldSR) = False then
    begin
      SenderTMDB.Return := db_Pack_OpenItemError;
      Result := False;
      Exit;
    end;
  if dbItem_ReadRec(_FieldSR.RHeader.CurrentHeader, SenderTMDB.IOHnd, SenderItem) = False then
    begin
      SenderTMDB.Return := SenderItem.Return;
      Result := False;
      Exit;
    end;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_ItemCreate(const pathName, ItemName, ItemDescription: U_String; const ItemExtID: Byte; var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Boolean;
begin
  if SenderTMDBItemHandle.OpenFlags then
    begin
      SenderTMDB.Return := db_Pack_RepeatCreateItemError;
      Result := False;
      Exit;
    end;
  if dbPack_NewItem(pathName, ItemName, ItemDescription, ItemExtID, SenderTMDBItemHandle.Item, SenderTMDB) = False then
    begin
      Result := False;
      Exit;
    end;
  if dbItem_BlockInit(SenderTMDB.IOHnd, SenderTMDBItemHandle.Item) = False then
    begin
      SenderTMDB.Return := SenderTMDBItemHandle.Item.Return;
      Result := False;
      Exit;
    end;
  SenderTMDBItemHandle.Path := pathName;
  SenderTMDBItemHandle.Name := ItemName;
  SenderTMDBItemHandle.Description := ItemDescription;
  SenderTMDBItemHandle.CreateTime := SenderTMDBItemHandle.Item.RHeader.CreateTime;
  SenderTMDBItemHandle.LastModifyTime := SenderTMDBItemHandle.Item.RHeader.LastModifyTime;
  SenderTMDBItemHandle.ItemExtID := ItemExtID;
  SenderTMDBItemHandle.OpenFlags := True;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_ItemFastCreate(const ItemName, ItemDescription: U_String; const fPos: Int64; const ItemExtID: Byte; var SenderTMDBItemHandle: TTMDBItemHandle;
  var SenderTMDB: TTMDB): Boolean;
begin
  if SenderTMDBItemHandle.OpenFlags then
    begin
      SenderTMDB.Return := db_Pack_RepeatCreateItemError;
      Result := False;
      Exit;
    end;
  if dbField_CreateItem(ItemName, ItemExtID, fPos, SenderTMDB.IOHnd, SenderTMDBItemHandle.Item) = False then
    begin
      Result := False;
      Exit;
    end;
  if dbItem_BlockInit(SenderTMDB.IOHnd, SenderTMDBItemHandle.Item) = False then
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

function dbPack_ItemFastInsertNew(const ItemName, ItemDescription: U_String; const FieldPos, InsertHeaderPos: Int64; const ItemExtID: Byte; var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Boolean;
begin
  if SenderTMDBItemHandle.OpenFlags then
    begin
      SenderTMDB.Return := db_Pack_RepeatCreateItemError;
      Result := False;
      Exit;
    end;
  if dbField_InsertNewItem(ItemName, ItemExtID, FieldPos, InsertHeaderPos, SenderTMDB.IOHnd, SenderTMDBItemHandle.Item) = False then
    begin
      Result := False;
      Exit;
    end;
  if dbItem_BlockInit(SenderTMDB.IOHnd, SenderTMDBItemHandle.Item) = False then
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

function dbPack_ItemOpen(const pathName, ItemName: U_String; const ItemExtID: Byte; var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Boolean;
begin
  if SenderTMDBItemHandle.OpenFlags then
    begin
      SenderTMDB.Return := db_Pack_RepeatOpenItemError;
      Result := False;
      Exit;
    end;
  if dbPack_GetItem(pathName, ItemName, ItemExtID, SenderTMDBItemHandle.Item, SenderTMDB) = False then
    begin
      Result := False;
      Exit;
    end;
  if dbItem_BlockInit(SenderTMDB.IOHnd, SenderTMDBItemHandle.Item) = False then
    begin
      SenderTMDB.Return := SenderTMDBItemHandle.Item.Return;
      Result := False;
      Exit;
    end;
  SenderTMDBItemHandle.Path := pathName;
  SenderTMDBItemHandle.Name := ItemName;
  SenderTMDBItemHandle.Description := SenderTMDBItemHandle.Item.Description;
  SenderTMDBItemHandle.CreateTime := SenderTMDBItemHandle.Item.RHeader.CreateTime;
  SenderTMDBItemHandle.LastModifyTime := SenderTMDBItemHandle.Item.RHeader.LastModifyTime;
  SenderTMDBItemHandle.ItemExtID := ItemExtID;
  SenderTMDBItemHandle.OpenFlags := True;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_ItemFastOpen(const fPos: Int64; const ItemExtID: Byte; var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Boolean;
var
  itm: TItem;
begin
  if SenderTMDBItemHandle.OpenFlags then
    begin
      SenderTMDB.Return := db_Pack_RepeatOpenItemError;
      Result := False;
      Exit;
    end;
  if dbHeader_ReadRec(fPos, SenderTMDB.IOHnd, itm.RHeader) = False then
    begin
      SenderTMDB.Return := itm.RHeader.Return;
      Result := False;
      Exit;
    end;
  if itm.RHeader.ID <> db_Header_Item_ID then
    begin
      SenderTMDB.Return := db_Pack_OpenItemError;
      Result := False;
      Exit;
    end;
  if dbItem_OnlyReadItemRec(itm.RHeader.DataMainPOS, SenderTMDB.IOHnd, itm) = False then
    begin
      SenderTMDB.Return := itm.Return;
      Result := False;
      Exit;
    end;
  if itm.ExtID <> ItemExtID then
    begin
      SenderTMDB.Return := db_Pack_OpenItemError;
      Result := False;
      Exit;
    end;
  SenderTMDBItemHandle.Item := itm;
  if dbItem_BlockInit(SenderTMDB.IOHnd, SenderTMDBItemHandle.Item) = False then
    begin
      SenderTMDB.Return := SenderTMDBItemHandle.Item.Return;
      Result := False;
      Exit;
    end;
  SenderTMDBItemHandle.Path := '';
  SenderTMDBItemHandle.Name := itm.RHeader.Name;
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
  if SenderTMDBItemHandle.Item.DataWrited then
    begin
      SenderTMDBItemHandle.Item.RHeader.Name := SenderTMDBItemHandle.Name;
      SenderTMDBItemHandle.Item.RHeader.CreateTime := SenderTMDBItemHandle.CreateTime;
      SenderTMDBItemHandle.Item.RHeader.LastModifyTime := SenderTMDBItemHandle.LastModifyTime;
      SenderTMDBItemHandle.Item.Description := SenderTMDBItemHandle.Description;
      SenderTMDBItemHandle.Item.ExtID := SenderTMDBItemHandle.ItemExtID;
      if dbItem_WriteRec(SenderTMDBItemHandle.Item.RHeader.CurrentHeader, SenderTMDB.IOHnd, SenderTMDBItemHandle.Item) = False then
        begin
          SenderTMDB.Return := SenderTMDBItemHandle.Item.Return;
          Result := False;
          Exit;
        end;
      SenderTMDBItemHandle.Item.DataWrited := False;
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

  if dbItem_WriteRec(SenderTMDBItemHandle.Item.RHeader.CurrentHeader, SenderTMDB.IOHnd, SenderTMDBItemHandle.Item) = False then
    begin
      SenderTMDB.Return := SenderTMDBItemHandle.Item.Return;
      Result := False;
      Exit;
    end;
  SenderTMDBItemHandle.Item.DataWrited := False;

  Result := True;
end;

function dbPack_ItemClose(var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Boolean;
begin
  Result := dbPack_ItemUpdate(SenderTMDBItemHandle, SenderTMDB);
  if Result then
      SenderTMDBItemHandle.OpenFlags := False;
end;

function dbPack_ItemReName(const FieldPos: Int64; const NewItemName, NewItemDescription: U_String; var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Boolean;
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
  if dbItem_WriteRec(SenderTMDBItemHandle.Item.RHeader.CurrentHeader, SenderTMDB.IOHnd, SenderTMDBItemHandle.Item) = False then
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
  if dbItem_BlockReadData(SenderTMDB.IOHnd, SenderTMDBItemHandle.Item, Buffers, Size) = False then
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
  if dbItem_BlockWriteData(SenderTMDB.IOHnd, SenderTMDBItemHandle.Item, Buffers, Size) = False then
    begin
      SenderTMDB.Return := SenderTMDBItemHandle.Item.Return;
      Result := False;
      Exit;
    end;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_ItemReadStr(var Name: U_String; var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Boolean;
var
  StrSize: Integer;
  SwapName: U_Bytes;
begin
  if SenderTMDBItemHandle.OpenFlags = False then
    begin
      SenderTMDB.Return := db_Pack_OpenItemError;
      Result := False;
      Exit;
    end;
  if dbItem_BlockReadData(SenderTMDB.IOHnd, SenderTMDBItemHandle.Item, StrSize, C_FixedLengthStringHeaderSize) = False then
    begin
      SenderTMDB.Return := SenderTMDBItemHandle.Item.Return;
      Result := False;
      Exit;
    end;
  umlSetLength(SwapName, StrSize);
  if StrSize <= 0 then
    begin
      Name := '';
      SenderTMDB.Return := db_Pack_ok;
      Result := True;
      Exit;
    end;
  if dbItem_BlockReadData(SenderTMDB.IOHnd, SenderTMDBItemHandle.Item, SwapName[0], StrSize) = False then
    begin
      SenderTMDB.Return := SenderTMDBItemHandle.Item.Return;
      Result := False;
      Exit;
    end;
  Name := umlStringOf(SwapName).Text;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_ItemWriteStr(const Name: U_String; var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Boolean;
var
  StrSize: Integer;
  SwapName: U_Bytes;
begin
  if SenderTMDBItemHandle.OpenFlags = False then
    begin
      SenderTMDB.Return := db_Pack_OpenItemError;
      Result := False;
      Exit;
    end;
  SwapName := umlBytesOf(Name);
  StrSize := umlGetLength(SwapName);
  if dbItem_BlockWriteData(SenderTMDB.IOHnd, SenderTMDBItemHandle.Item, StrSize, C_FixedLengthStringHeaderSize) = False then
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
  if dbItem_BlockWriteData(SenderTMDB.IOHnd, SenderTMDBItemHandle.Item, SwapName[0], StrSize) = False then
    begin
      SenderTMDB.Return := SenderTMDBItemHandle.Item.Return;
      Result := False;
      Exit;
    end;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_ItemSeekPos(const fPos: Int64; var SenderTMDBItemHandle: TTMDBItemHandle; var SenderTMDB: TTMDB): Boolean;
begin
  if SenderTMDBItemHandle.OpenFlags = False then
    begin
      SenderTMDB.Return := db_Pack_OpenItemError;
      Result := False;
      Exit;
    end;
  if dbItem_BlockSeekPOS(SenderTMDB.IOHnd, SenderTMDBItemHandle.Item, fPos) = False then
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
  if dbItem_BlockSeekStartPOS(SenderTMDB.IOHnd, SenderTMDBItemHandle.Item) = False then
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
  if dbItem_BlockSeekLastPOS(SenderTMDB.IOHnd, SenderTMDBItemHandle.Item) = False then
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
  Result := dbItem_BlockGetPOS(SenderTMDB.IOHnd, SenderTMDBItemHandle.Item);
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
  SwapBuffers: array [0 .. C_MaxBufferFragmentSize] of Byte;
  i: Integer;
begin
  if SenderTMDBItemHandle.OpenFlags = False then
    begin
      SenderTMDB.Return := db_Pack_OpenItemError;
      Result := False;
      Exit;
    end;
  if dbItem_BlockSeekLastPOS(SenderTMDB.IOHnd, SenderTMDBItemHandle.Item) = False then
    begin
      SenderTMDB.Return := SenderTMDBItemHandle.Item.Return;
      Result := False;
      Exit;
    end;
  if Size > C_MaxBufferFragmentSize then
    begin
      for i := 1 to (Size div C_MaxBufferFragmentSize) do
        begin
          if dbItem_BlockWriteData(SenderTMDB.IOHnd, SenderTMDBItemHandle.Item, SwapBuffers, C_MaxBufferFragmentSize) = False then
            begin
              SenderTMDB.Return := SenderTMDBItemHandle.Item.Return;
              Result := False;
              Exit;
            end;
        end;
      if dbItem_BlockWriteData(SenderTMDB.IOHnd, SenderTMDBItemHandle.Item, SwapBuffers, (Size mod C_MaxBufferFragmentSize)) = False then
        begin
          SenderTMDB.Return := SenderTMDBItemHandle.Item.Return;
          Result := False;
          Exit;
        end;
      SenderTMDB.Return := db_Pack_ok;
      Result := True;
      Exit;
    end;
  if dbItem_BlockWriteData(SenderTMDB.IOHnd, SenderTMDBItemHandle.Item, SwapBuffers, Size) = False then
    begin
      SenderTMDB.Return := SenderTMDBItemHandle.Item.Return;
      Result := False;
      Exit;
    end;
  SenderTMDB.Return := db_Pack_ok;
  Result := True;
end;

function dbPack_ExistsRootField(const Name: U_String; var SenderTMDB: TTMDB): Boolean;
var
  f: TField;
begin
  if SenderTMDB.RootHeaderCount = 0 then
    begin
      SenderTMDB.Return := db_Pack_PathNameError;
      Result := False;
      Exit;
    end;
  if dbHeader_ReadRec(SenderTMDB.DefaultFieldPOS, SenderTMDB.IOHnd, f.RHeader) = False then
    begin
      SenderTMDB.Return := f.RHeader.Return;
      Result := False;
      Exit;
    end;

  if (dbMultipleMatch(Name, f.RHeader.Name) = True) and (f.RHeader.ID = db_Header_Field_ID) then
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
  if dbHeader_ReadRec(f.RHeader.NextHeader, SenderTMDB.IOHnd, f.RHeader) = False then
    begin
      SenderTMDB.Return := f.RHeader.Return;
      Result := False;
      Exit;
    end;

  if (dbMultipleMatch(Name, f.RHeader.Name) = True) and (f.RHeader.ID = db_Header_Field_ID) then
    begin
      SenderTMDB.Return := db_Pack_ok;
      Result := True;
      Exit;
    end;

  while f.RHeader.CurrentHeader <> SenderTMDB.DefaultFieldPOS do
    begin
      if dbHeader_ReadRec(f.RHeader.NextHeader, SenderTMDB.IOHnd, f.RHeader) = False then
        begin
          SenderTMDB.Return := f.RHeader.Return;
          Result := False;
          Exit;
        end;
      if (dbMultipleMatch(Name, f.RHeader.Name) = True) and (f.RHeader.ID = db_Header_Field_ID) then
        begin
          SenderTMDB.Return := db_Pack_ok;
          Result := True;
          Exit;
        end;
    end;
  SenderTMDB.Return := db_Pack_PathNameError;
  Result := False;
end;

function dbPack_FindFirstHeader(const pathName, FilterName: U_String; const ID: Byte; var SenderSearch: TTMDBSearchHeader; var SenderTMDB: TTMDB): Boolean;
var
  f: TField;
begin
  if dbPack_GetField(pathName, f, SenderTMDB) = False then
    begin
      Result := False;
      Exit;
    end;
  if dbField_FindFirst(FilterName, ID, f.RHeader.CurrentHeader, SenderTMDB.IOHnd, SenderSearch.FieldSearch) = False then
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
  if dbField_FindNext(SenderTMDB.IOHnd, SenderSearch.FieldSearch) = False then
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

function dbPack_FindLastHeader(const pathName, FilterName: U_String; const ID: Byte; var SenderSearch: TTMDBSearchHeader; var SenderTMDB: TTMDB): Boolean;
var
  f: TField;
begin
  if dbPack_GetField(pathName, f, SenderTMDB) = False then
    begin
      Result := False;
      Exit;
    end;
  if dbField_FindLast(FilterName, ID, f.RHeader.CurrentHeader, SenderTMDB.IOHnd, SenderSearch.FieldSearch) = False then
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
  if dbField_FindPrev(SenderTMDB.IOHnd, SenderSearch.FieldSearch) = False then
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

function dbPack_FindFirstItem(const pathName, FilterName: U_String; const ItemExtID: Byte; var SenderSearch: TTMDBSearchItem; var SenderTMDB: TTMDB): Boolean;
var
  f: TField;
  itm: TItem;
begin
  if dbPack_GetField(pathName, f, SenderTMDB) = False then
    begin
      Result := False;
      Exit;
    end;
  if dbField_FindFirstItem(FilterName, ItemExtID, f.RHeader.CurrentHeader, SenderTMDB.IOHnd, SenderSearch.FieldSearch, itm) = False then
    begin
      SenderTMDB.Return := SenderSearch.FieldSearch.Return;
      Result := False;
      Exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.Description := itm.Description;
  SenderSearch.ExtID := itm.ExtID;
  SenderSearch.Size := itm.Size;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := 1;
  SenderTMDB.Return := SenderSearch.FieldSearch.Return;
  Result := True;
end;

function dbPack_FindNextItem(var SenderSearch: TTMDBSearchItem; const ItemExtID: Byte; var SenderTMDB: TTMDB): Boolean;
var
  itm: TItem;
begin
  if dbField_FindNextItem(ItemExtID, SenderTMDB.IOHnd, SenderSearch.FieldSearch, itm) = False then
    begin
      SenderTMDB.Return := SenderSearch.FieldSearch.Return;
      Result := False;
      Exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.Description := itm.Description;
  SenderSearch.ExtID := itm.ExtID;
  SenderSearch.Size := itm.Size;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := SenderSearch.CompleteCount + 1;
  SenderTMDB.Return := SenderSearch.FieldSearch.Return;
  Result := True;
end;

function dbPack_FindLastItem(const pathName, FilterName: U_String; const ItemExtID: Byte; var SenderSearch: TTMDBSearchItem; var SenderTMDB: TTMDB): Boolean;
var
  f: TField;
  itm: TItem;
begin
  if dbPack_GetField(pathName, f, SenderTMDB) = False then
    begin
      Result := False;
      Exit;
    end;
  if dbField_FindLastItem(FilterName, ItemExtID, f.RHeader.CurrentHeader, SenderTMDB.IOHnd, SenderSearch.FieldSearch, itm) = False then
    begin
      SenderTMDB.Return := SenderSearch.FieldSearch.Return;
      Result := False;
      Exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.Description := itm.Description;
  SenderSearch.ExtID := itm.ExtID;
  SenderSearch.Size := itm.Size;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := 1;
  SenderTMDB.Return := SenderSearch.FieldSearch.Return;
  Result := True;
end;

function dbPack_FindPrevItem(var SenderSearch: TTMDBSearchItem; const ItemExtID: Byte; var SenderTMDB: TTMDB): Boolean;
var
  itm: TItem;
begin
  if dbField_FindPrevItem(ItemExtID, SenderTMDB.IOHnd, SenderSearch.FieldSearch, itm) = False then
    begin
      SenderTMDB.Return := SenderSearch.FieldSearch.Return;
      Result := False;
      Exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.Description := itm.Description;
  SenderSearch.ExtID := itm.ExtID;
  SenderSearch.Size := itm.Size;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := SenderSearch.CompleteCount + 1;
  SenderTMDB.Return := SenderSearch.FieldSearch.Return;
  Result := True;
end;

function dbPack_FastFindFirstItem(const FieldPos: Int64; const FilterName: U_String; const ItemExtID: Byte; var SenderSearch: TTMDBSearchItem; var SenderTMDB: TTMDB): Boolean;
var
  itm: TItem;
begin
  if dbField_FindFirstItem(FilterName, ItemExtID, FieldPos, SenderTMDB.IOHnd, SenderSearch.FieldSearch, itm) = False then
    begin
      SenderTMDB.Return := SenderSearch.FieldSearch.Return;
      Result := False;
      Exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.Description := itm.Description;
  SenderSearch.ExtID := itm.ExtID;
  SenderSearch.Size := itm.Size;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := 1;
  SenderTMDB.Return := SenderSearch.FieldSearch.Return;
  Result := True;
end;

function dbPack_FastFindNextItem(var SenderSearch: TTMDBSearchItem; const ItemExtID: Byte; var SenderTMDB: TTMDB): Boolean;
var
  itm: TItem;
begin
  if dbField_FindNextItem(ItemExtID, SenderTMDB.IOHnd, SenderSearch.FieldSearch, itm) = False then
    begin
      SenderTMDB.Return := SenderSearch.FieldSearch.Return;
      Result := False;
      Exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.Description := itm.Description;
  SenderSearch.ExtID := itm.ExtID;
  SenderSearch.Size := itm.Size;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := SenderSearch.CompleteCount + 1;
  SenderTMDB.Return := SenderSearch.FieldSearch.Return;
  Result := True;
end;

function dbPack_FastFindLastItem(const FieldPos: Int64; const FilterName: U_String; const ItemExtID: Byte; var SenderSearch: TTMDBSearchItem; var SenderTMDB: TTMDB): Boolean;
var
  itm: TItem;
begin
  if dbField_FindLastItem(FilterName, ItemExtID, FieldPos, SenderTMDB.IOHnd, SenderSearch.FieldSearch, itm) = False then
    begin
      SenderTMDB.Return := SenderSearch.FieldSearch.Return;
      Result := False;
      Exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.Description := itm.Description;
  SenderSearch.ExtID := itm.ExtID;
  SenderSearch.Size := itm.Size;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := 1;
  SenderTMDB.Return := SenderSearch.FieldSearch.Return;
  Result := True;
end;

function dbPack_FastFindPrevItem(var SenderSearch: TTMDBSearchItem; const ItemExtID: Byte; var SenderTMDB: TTMDB): Boolean;
var
  itm: TItem;
begin
  if dbField_FindPrevItem(ItemExtID, SenderTMDB.IOHnd, SenderSearch.FieldSearch, itm) = False then
    begin
      SenderTMDB.Return := SenderSearch.FieldSearch.Return;
      Result := False;
      Exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.Description := itm.Description;
  SenderSearch.ExtID := itm.ExtID;
  SenderSearch.Size := itm.Size;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := SenderSearch.CompleteCount + 1;
  SenderTMDB.Return := SenderSearch.FieldSearch.Return;
  Result := True;
end;

function dbPack_FindFirstField(const pathName, FilterName: U_String; var SenderSearch: TTMDBSearchField; var SenderTMDB: TTMDB): Boolean;
var
  f: TField;
begin
  if dbPack_GetField(pathName, f, SenderTMDB) = False then
    begin
      Result := False;
      Exit;
    end;
  if dbField_FindFirst(FilterName, db_Header_Field_ID, f.RHeader.CurrentHeader, SenderTMDB.IOHnd, SenderSearch.FieldSearch) = False then
    begin
      SenderTMDB.Return := SenderSearch.FieldSearch.Return;
      Result := False;
      Exit;
    end;
  if dbField_OnlyReadFieldRec(SenderSearch.FieldSearch.RHeader.DataMainPOS, SenderTMDB.IOHnd, f) = False then
    begin
      SenderTMDB.Return := f.Return;
      Result := False;
      Exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.Description := f.Description;
  SenderSearch.HeaderCount := f.HeaderCount;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := 1;
  SenderTMDB.Return := SenderSearch.FieldSearch.Return;
  Result := True;
end;

function dbPack_FindNextField(var SenderSearch: TTMDBSearchField; var SenderTMDB: TTMDB): Boolean;
var
  f: TField;
begin
  if dbField_FindNext(SenderTMDB.IOHnd, SenderSearch.FieldSearch) = False then
    begin
      SenderTMDB.Return := SenderSearch.FieldSearch.Return;
      Result := False;
      Exit;
    end;
  if dbField_OnlyReadFieldRec(SenderSearch.FieldSearch.RHeader.DataMainPOS, SenderTMDB.IOHnd, f) = False then
    begin
      SenderTMDB.Return := f.Return;
      Result := False;
      Exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.Description := f.Description;
  SenderSearch.HeaderCount := f.HeaderCount;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := SenderSearch.CompleteCount + 1;
  SenderTMDB.Return := SenderSearch.FieldSearch.Return;
  Result := True;
end;

function dbPack_FindLastField(const pathName, FilterName: U_String; var SenderSearch: TTMDBSearchField; var SenderTMDB: TTMDB): Boolean;
var
  f: TField;
begin
  if dbPack_GetField(pathName, f, SenderTMDB) = False then
    begin
      Result := False;
      Exit;
    end;
  if dbField_FindLast(FilterName, db_Header_Field_ID, f.RHeader.CurrentHeader, SenderTMDB.IOHnd, SenderSearch.FieldSearch) = False then
    begin
      SenderTMDB.Return := SenderSearch.FieldSearch.Return;
      Result := False;
      Exit;
    end;
  if dbField_OnlyReadFieldRec(SenderSearch.FieldSearch.RHeader.DataMainPOS, SenderTMDB.IOHnd, f) = False then
    begin
      SenderTMDB.Return := f.Return;
      Result := False;
      Exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.Description := f.Description;
  SenderSearch.HeaderCount := f.HeaderCount;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := 1;
  SenderTMDB.Return := SenderSearch.FieldSearch.Return;
  Result := True;
end;

function dbPack_FindPrevField(var SenderSearch: TTMDBSearchField; var SenderTMDB: TTMDB): Boolean;
var
  f: TField;
begin
  if dbField_FindPrev(SenderTMDB.IOHnd, SenderSearch.FieldSearch) = False then
    begin
      SenderTMDB.Return := SenderSearch.FieldSearch.Return;
      Result := False;
      Exit;
    end;
  if dbField_OnlyReadFieldRec(SenderSearch.FieldSearch.RHeader.DataMainPOS, SenderTMDB.IOHnd, f) = False then
    begin
      SenderTMDB.Return := f.Return;
      Result := False;
      Exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.Description := f.Description;
  SenderSearch.HeaderCount := f.HeaderCount;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := SenderSearch.CompleteCount + 1;
  SenderTMDB.Return := SenderSearch.FieldSearch.Return;
  Result := True;
end;

function dbPack_FastFindFirstField(const FieldPos: Int64; const FilterName: U_String; var SenderSearch: TTMDBSearchField; var SenderTMDB: TTMDB): Boolean;
var
  f: TField;
begin
  if dbField_FindFirst(FilterName, db_Header_Field_ID, FieldPos, SenderTMDB.IOHnd, SenderSearch.FieldSearch) = False then
    begin
      SenderTMDB.Return := SenderSearch.FieldSearch.Return;
      Result := False;
      Exit;
    end;
  if dbField_OnlyReadFieldRec(SenderSearch.FieldSearch.RHeader.DataMainPOS, SenderTMDB.IOHnd, f) = False then
    begin
      SenderTMDB.Return := f.Return;
      Result := False;
      Exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.Description := f.Description;
  SenderSearch.HeaderCount := f.HeaderCount;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := 1;
  SenderTMDB.Return := SenderSearch.FieldSearch.Return;
  Result := True;
end;

function dbPack_FastFindNextField(var SenderSearch: TTMDBSearchField; var SenderTMDB: TTMDB): Boolean;
var
  f: TField;
begin
  if dbField_FindNext(SenderTMDB.IOHnd, SenderSearch.FieldSearch) = False then
    begin
      SenderTMDB.Return := SenderSearch.FieldSearch.Return;
      Result := False;
      Exit;
    end;
  if dbField_OnlyReadFieldRec(SenderSearch.FieldSearch.RHeader.DataMainPOS, SenderTMDB.IOHnd, f) = False then
    begin
      SenderTMDB.Return := f.Return;
      Result := False;
      Exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.Description := f.Description;
  SenderSearch.HeaderCount := f.HeaderCount;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := SenderSearch.CompleteCount + 1;
  SenderTMDB.Return := SenderSearch.FieldSearch.Return;
  Result := True;
end;

function dbPack_FastFindLastField(const FieldPos: Int64; const FilterName: U_String; var SenderSearch: TTMDBSearchField; var SenderTMDB: TTMDB): Boolean;
var
  f: TField;
begin
  if dbField_FindLast(FilterName, db_Header_Field_ID, FieldPos, SenderTMDB.IOHnd, SenderSearch.FieldSearch) = False then
    begin
      SenderTMDB.Return := SenderSearch.FieldSearch.Return;
      Result := False;
      Exit;
    end;
  if dbField_OnlyReadFieldRec(SenderSearch.FieldSearch.RHeader.DataMainPOS, SenderTMDB.IOHnd, f) = False then
    begin
      SenderTMDB.Return := f.Return;
      Result := False;
      Exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.Description := f.Description;
  SenderSearch.HeaderCount := f.HeaderCount;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := 1;
  SenderTMDB.Return := SenderSearch.FieldSearch.Return;
  Result := True;
end;

function dbPack_FastFindPrevField(var SenderSearch: TTMDBSearchField; var SenderTMDB: TTMDB): Boolean;
var
  f: TField;
begin
  if dbField_FindPrev(SenderTMDB.IOHnd, SenderSearch.FieldSearch) = False then
    begin
      SenderTMDB.Return := SenderSearch.FieldSearch.Return;
      Result := False;
      Exit;
    end;
  if dbField_OnlyReadFieldRec(SenderSearch.FieldSearch.RHeader.DataMainPOS, SenderTMDB.IOHnd, f) = False then
    begin
      SenderTMDB.Return := f.Return;
      Result := False;
      Exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.Description := f.Description;
  SenderSearch.HeaderCount := f.HeaderCount;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := SenderSearch.CompleteCount + 1;
  SenderTMDB.Return := SenderSearch.FieldSearch.Return;
  Result := True;
end;

function dbPack_RecursionSearchFirst(const InitPath, FilterName: U_String; var SenderRecursionSearch: TTMDBRecursionSearch; var SenderTMDB: TTMDB): Boolean;
begin
  if dbPack_GetField(InitPath, SenderRecursionSearch.CurrentField, SenderTMDB) = False then
    begin
      Result := False;
      Exit;
    end;
  SenderRecursionSearch.SearchBuffGo := 0;
  if dbField_FindFirst(FilterName, db_Header_Item_ID, SenderRecursionSearch.CurrentField.RHeader.CurrentHeader, SenderTMDB.IOHnd,
    SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo]) = False then
    begin
      if dbField_FindFirst('*', db_Header_Field_ID, SenderRecursionSearch.CurrentField.RHeader.CurrentHeader, SenderTMDB.IOHnd,
        SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo]) = False then
        begin
          SenderTMDB.Return := SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo].Return;
          Result := False;
          Exit;
        end;
      if dbField_OnlyReadFieldRec(SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo].RHeader.DataMainPOS, SenderTMDB.IOHnd, SenderRecursionSearch.CurrentField) = False
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
    db_Header_Field_ID:
      begin
        if dbField_FindFirst(SenderRecursionSearch.FilterName, db_Header_Item_ID, SenderRecursionSearch.CurrentField.RHeader.CurrentHeader, SenderTMDB.IOHnd,
          SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo]) then
          begin
            SenderRecursionSearch.ReturnHeader := SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo].RHeader;
            SenderTMDB.Return := db_Pack_ok;
            Result := True;
            Exit;
          end;
        if dbField_FindFirst('*', db_Header_Field_ID, SenderRecursionSearch.CurrentField.RHeader.CurrentHeader, SenderTMDB.IOHnd,
          SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo]) then
          begin
            if dbField_OnlyReadFieldRec(SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo].RHeader.DataMainPOS, SenderTMDB.IOHnd,
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
        while dbField_FindNext(SenderTMDB.IOHnd, SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo]) = False do
          begin
            if SenderRecursionSearch.SearchBuffGo = 0 then
              begin
                SenderTMDB.Return := db_Pack_RecursionSearchOver;
                Result := False;
                Exit;
              end;
            SenderRecursionSearch.SearchBuffGo := SenderRecursionSearch.SearchBuffGo - 1;
          end;

        if dbField_OnlyReadFieldRec(SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo].RHeader.DataMainPOS, SenderTMDB.IOHnd, SenderRecursionSearch.CurrentField) = False then
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
    db_Header_Item_ID:
      begin
        if dbField_FindNext(SenderTMDB.IOHnd, SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo]) then
          begin
            SenderRecursionSearch.ReturnHeader := SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo].RHeader;
            SenderTMDB.Return := db_Pack_ok;
            Result := True;
            Exit;
          end;
        if dbField_FindFirst('*', db_Header_Field_ID, SenderRecursionSearch.CurrentField.RHeader.CurrentHeader, SenderTMDB.IOHnd,
          SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo]) then
          begin
            if dbField_OnlyReadFieldRec(SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo].RHeader.DataMainPOS, SenderTMDB.IOHnd,
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
        while dbField_FindNext(SenderTMDB.IOHnd, SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo]) = False do
          begin
            if SenderRecursionSearch.SearchBuffGo = 0 then
              begin
                SenderTMDB.Return := db_Pack_RecursionSearchOver;
                Result := False;
                Exit;
              end;
            SenderRecursionSearch.SearchBuffGo := SenderRecursionSearch.SearchBuffGo - 1;
          end;

        if dbField_OnlyReadFieldRec(SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo].RHeader.DataMainPOS, SenderTMDB.IOHnd, SenderRecursionSearch.CurrentField) = False then
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

