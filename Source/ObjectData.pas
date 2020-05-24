{ ****************************************************************************** }
{ * Low Object DB Imp library                                                  * }
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
  2017-12-6 performance optimization
  2018-12-7, reliability optimization, by qq600585
*)

unit ObjectData;

{$INCLUDE zDefine.inc}

interface

uses CoreClasses, UnicodeMixedLib;

const
  DB_Version_Size = C_Word_Size;
  DB_Time_Size = C_Double_Size;
  DB_Counter_Size = C_Int64_Size;
  DB_DataSize_Size = C_Int64_Size;
  DB_Position_Size = C_Int64_Size;
  DB_ID_Size = C_Byte_Size;
  DB_Property_Size = C_Cardinal_Size;
  DB_Level_Size = C_Word_Size;
  DB_ReservedData_Size = 64;
  DB_FixedStringL_Size = 1;

  DB_MajorVersion = 2;
  DB_MinorVersion = 3;
  DB_Max_Secursion_Level = 128;

  DB_FileDescription = 'ObjectDataV2.3';
  DB_DefaultField = 'Default-Root';

  DB_Path_Delimiter = '/';

  DB_Header_Field_ID = 21;
  DB_Header_Item_ID = 22;

  DB_Header_First = 11;
  DB_Header_Medium = 12;
  DB_Header_Last = 13;
  DB_Header_1 = 14;

  DB_Item_1 = 33;
  DB_Item_First = 34;
  DB_Item_Medium = 35;
  DB_Item_Last = 36;

{$REGION 'State Code'}
  { State code }
  DB_Header_ok = 300;
  DB_Header_SetPosError = -301;
  DB_Header_WritePosError = -303;
  DB_Header_WriteNextPosError = -304;
  DB_Header_WritePrevPosError = -305;
  DB_Header_WritePubMainPosError = -306;
  DB_Header_WriteIDError = -307;
  DB_Header_WritePositionIDError = -311;
  DB_Header_WriteNameError = -308;
  DB_Header_WriteCreateTimeError = -309;
  DB_Header_WriteLastEditTimeError = -310;
  DB_Header_WriteUserPropertyIDError = -332;
  DB_Header_ReadPosError = -321;
  DB_Header_ReadNextPosError = -322;
  DB_Header_ReadPrevPosError = -323;
  DB_Header_ReadPubMainPosError = -324;
  DB_Header_ReadIDError = -325;
  DB_Header_ReadPositionIDError = -312;
  DB_Header_ReadNameError = -326;
  DB_Header_ReadCreateTimeError = -327;
  DB_Header_ReadLastEditTimeError = -328;
  DB_Header_ReadUserPropertyIDError = -331;
  DB_Header_NotFindHeader = -320;
  DB_Item_ok = 200;
  DB_Item_SetPosError = -201;
  DB_Item_WriteRecDescriptionError = -204;
  DB_Item_WriteRecExterIDError = -205;
  DB_Item_WriteFirstBlockPOSError = -206;
  DB_Item_WriteLastBlockPOSError = -207;
  DB_Item_WriteRecBuffSizeError = -208;
  DB_Item_WriteBlockCountError = -209;
  DB_Item_ReadRecDescriptionError = -214;
  DB_Item_ReadRecExterIDError = -215;
  DB_Item_ReadFirstBlockPOSError = -216;
  DB_Item_ReadLastBlockPOSError = -217;
  DB_Item_ReadRecBuffSizeError = -218;
  DB_Item_ReadBlockCountError = -219;
  DB_Item_WriteItemBlockIDFlagsError = -220;
  DB_Item_WriteCurrentBlockPOSError = -221;
  DB_Item_WriteNextBlockPOSError = -222;
  DB_Item_WritePrevBlockPOSError = -223;
  DB_Item_WriteDataBlockPOSError = -224;
  DB_Item_WriteDataBuffSizeError = -225;
  DB_Item_ReadItemBlockIDFlagsError = -230;
  DB_Item_ReadCurrentBlockPOSError = -231;
  DB_Item_ReadNextBlockPOSError = -232;
  DB_Item_ReadPrevBlockPOSError = -233;
  DB_Item_ReadDataBlockPOSError = -234;
  DB_Item_ReadDataBuffSizeError = -235;
  DB_Item_BlockPositionError = -240;
  DB_Item_BlockOverrate = -241;
  DB_Item_BlockReadError = -242;
  DB_Item_BlockWriteError = -243;
  DB_Field_ok = 100;
  DB_Field_SetPosError = -101;
  DB_Field_WriteHeaderFieldPosError = -103;
  DB_Field_WriteDescriptionError = -104;
  DB_Field_WriteCountError = -106;
  DB_Field_WriteFirstPosError = -107;
  DB_Field_WriteLastPosError = -108;
  DB_Field_ReadHeaderFieldPosError = -110;
  DB_Field_ReadDescriptionError = -111;
  DB_Field_ReadCountError = -112;
  DB_Field_ReadFirstPosError = -113;
  DB_Field_ReadLastPosError = -114;
  DB_Field_NotInitSearch = -121;
  DB_Field_DeleteHeaderError = -124;
  DB_ok = 400;
  DB_RepOpenPackError = -401;
  DB_CreatePackError = -402;
  DB_WriteReservedDataError = -460;
  DB_WriteNameError = -403;
  DB_WriteDescriptionError = -404;
  DB_PositionSeekError = -405;
  DB_WriteMajorVersionError = -406;
  DB_WriteMinorVersionError = -407;
  DB_WriteCreateTimeError = -408;
  DB_WriteLastEditTimeError = -409;
  DB_WriteHeaderCountError = -410;
  DB_WriteDefaultPositionError = -411;
  DB_WriteFirstPositionError = -412;
  DB_WriteLastPositionError = -413;
  DB_WriteFixedStringLError = -462;
  DB_ReadReservedDataError = -461;
  DB_ReadNameError = -414;
  DB_ReadDescriptionError = -415;
  DB_ReadMajorVersionError = -416;
  DB_ReadMinorVersionError = -417;
  DB_ReadCreateTimeError = -418;
  DB_ReadLastEditTimeError = -419;
  DB_ReadHeaderCountError = -420;
  DB_ReadDefaultPositionError = -421;
  DB_ReadFirstPositionError = -422;
  DB_ReadLastPositionError = -423;
  DB_RepCreatePackError = -424;
  DB_OpenPackError = -425;
  DB_ClosePackError = -426;
  DB_ReadFixedStringLError = -431;
  DB_WriteCurrentPositionError = -427;
  DB_WriteCurrentLevelError = -428;
  DB_ReadCurrentPositionError = -429;
  DB_ReadCurrentLevelError = -430;
  DB_PathNameError = -440;
  DB_RepeatCreateItemError = -450;
  DB_OpenItemError = -451;
  DB_ItemNameError = -452;
  DB_RepeatOpenItemError = -453;
  DB_CloseItemError = -454;
  DB_ItemStructNotFindDescription = -455;
  DB_RecursionSearchOver = -456;
  DB_FileBufferError = -500;
  DB_CheckIOError = -501;
  DB_ExceptionError = -502;
{$ENDREGION 'State Code'}


type
  THeader = record
    CurrentHeader: Int64;                        // nowrite
    NextHeader, PrevHeader, DataPosition: Int64; // store position
    ID: Byte;                                    // DB_Header_Field_ID, DB_Header_Item_ID
    PositionID: Byte;                            // DB_Header_First, DB_Header_Medium, DB_Header_Last, DB_Header_1
    CreateTime, ModificationTime: Double;        // time
    UserProperty: Cardinal;                      // external define
    Name: U_String;                              // header name
    State: Integer;                              // nowrite
  end;

  PHeader = ^THeader;

  TItemBlock = record
    ID: Byte;                                                         // block ID
    CurrentBlockPOS, NextBlockPOS, PrevBlockPOS, DataPosition: Int64; // data pos
    Size: Int64;                                                      // block size
    State: Integer;                                                   // nowrite
  end;

  PItemBlock = ^TItemBlock;

  TItem = record
    RHeader: THeader;                   // header
    Description: U_String;              // descript
    ExtID: Byte;                        // item external ID
    FirstBlockPOS, LastBlockPOS: Int64; // item block pos
    Size: Int64;                        // size
    BlockCount: Int64;                  // block count
    CurrentBlockSeekPOS: Int64;         // nowrite
    CurrentFileSeekPOS: Int64;          // nowrite
    CurrentItemBlock: TItemBlock;       // nowrite
    DataModification: Boolean;          // nowrite
    State: Integer;                     // nowrite
  end;

  PItem = ^TItem;

  TField = record
    RHeader: THeader;                     // header
    UpFieldPOS: Int64;                    // up level field
    Description: U_String;                // description
    HeaderCount: Int64;                   // children
    FirstHeaderPOS, LastHeaderPOS: Int64; // header pos
    State: Integer;                       // nowrite
  end;

  PField = ^TField;

  // internal used
  TFieldSearch = record
    RHeader: THeader;
    InitFlags: Boolean;
    Name: U_String;
    StartPos, OverPOS: Int64;
    ID: Byte;
    PositionID: Byte;
    State: Integer;
  end;

  PObjectDataHandle = ^TObjectDataHandle;

  // backcall declaration
  TObjectDataErrorProc = procedure(error: U_String) of object;
  TObjectDataHeaderDeleteProc = procedure(fPos: Int64) of object;
  TObjectDataHeaderWriteBeforeProc = procedure(fPos: Int64; var wVal: THeader; var Done: Boolean) of object;
  TObjectDataHeaderWriteAfterProc = procedure(fPos: Int64; var wVal: THeader) of object;
  TObjectDataHeaderReadProc = procedure(fPos: Int64; var rVal: THeader; var Done: Boolean) of object;
  TObjectDataItemBlockWriteBeforeProc = procedure(fPos: Int64; var wVal: TItemBlock; var Done: Boolean) of object;
  TObjectDataItemBlockWriteAfterProc = procedure(fPos: Int64; var wVal: TItemBlock) of object;
  TObjectDataItemBlockReadProc = procedure(fPos: Int64; var rVal: TItemBlock; var Done: Boolean) of object;
  TObjectDataItemWriteBeforeProc = procedure(fPos: Int64; var wVal: TItem; var Done: Boolean) of object;
  TObjectDataItemWriteAfterProc = procedure(fPos: Int64; var wVal: TItem) of object;
  TObjectDataItemReadProc = procedure(fPos: Int64; var rVal: TItem; var Done: Boolean) of object;
  TObjectDataFieldWriteBeforeProc = procedure(fPos: Int64; var wVal: TField; var Done: Boolean) of object;
  TObjectDataFieldWriteAfterProc = procedure(fPos: Int64; var wVal: TField) of object;
  TObjectDataFieldReadProc = procedure(fPos: Int64; var rVal: TField; var Done: Boolean) of object;
  TObjectDataTMDBWriteBeforeProc = procedure(fPos: Int64; const wVal: PObjectDataHandle; var Done: Boolean) of object;
  TObjectDataTMDBWriteAfterProc = procedure(fPos: Int64; const wVal: PObjectDataHandle) of object;
  TObjectDataTMDBReadProc = procedure(fPos: Int64; const rVal: PObjectDataHandle; var Done: Boolean) of object;

  TObjectDataHandle = record

    IOHnd: TIOHnd; // IO handle

    ReservedData: array [0 .. DB_ReservedData_Size - 1] of Byte; // file: reserved struct
    FixedStringL: Byte;                                          // file: fixed string length
    MajorVer, MinorVer: SmallInt;                                // file: version info
    CreateTime, ModificationTime: Double;                        // file: time
    RootHeaderCount: Int64;                                      // file: header counter
    DefaultFieldPOS, FirstHeaderPOS, LastHeaderPOS: Int64;       // file: field struct pos
    CurrentFieldPOS: Int64;                                      // file: current field pos
    CurrentFieldLevel: Word;                                     // file: current field level

    OverWriteItem: Boolean;       // nowrite
    AllowSameHeaderName: Boolean; // nowrite
    State: Integer;               // nowrite

    OnError: TObjectDataErrorProc;                                // backcall
    OnDeleteHeader: TObjectDataHeaderDeleteProc;                  // backcall
    OnPrepareWriteHeader: TObjectDataHeaderWriteBeforeProc;       // backcall
    OnWriteHeader: TObjectDataHeaderWriteAfterProc;               // backcall
    OnReadHeader: TObjectDataHeaderReadProc;                      // backcall
    OnPrepareWriteItemBlock: TObjectDataItemBlockWriteBeforeProc; // backcall
    OnWriteItemBlock: TObjectDataItemBlockWriteAfterProc;         // backcall
    OnReadItemBlock: TObjectDataItemBlockReadProc;                // backcall
    OnPrepareWriteItem: TObjectDataItemWriteBeforeProc;           // backcall
    OnWriteItem: TObjectDataItemWriteAfterProc;                   // backcall
    OnReadItem: TObjectDataItemReadProc;                          // backcall
    OnPrepareOnlyWriteItemRec: TObjectDataItemWriteBeforeProc;    // backcall
    OnOnlyWriteItemRec: TObjectDataItemWriteAfterProc;            // backcall
    OnOnlyReadItemRec: TObjectDataItemReadProc;                   // backcall
    OnPrepareWriteField: TObjectDataFieldWriteBeforeProc;         // backcall
    OnWriteField: TObjectDataFieldWriteAfterProc;                 // backcall
    OnReadField: TObjectDataFieldReadProc;                        // backcall
    OnPrepareOnlyWriteFieldRec: TObjectDataFieldWriteBeforeProc;  // backcall
    OnOnlyWriteFieldRec: TObjectDataFieldWriteAfterProc;          // backcall
    OnOnlyReadFieldRec: TObjectDataFieldReadProc;                 // backcall
    OnPrepareWriteTMDB: TObjectDataTMDBWriteBeforeProc;           // backcall
    OnWriteTMDB: TObjectDataTMDBWriteAfterProc;                   // backcall
    OnReadTMDB: TObjectDataTMDBReadProc;                          // backcall
  end;

  // runtime struct
  TItemHandle_ = record
    Item: TItem;
    Name: U_String;
    Description: U_String;
    CreateTime, ModificationTime: Double;
    ItemExtID: Byte;
    OpenFlags: Boolean;
  end;

  // runtime struct
  TSearchHeader_ = record
    Name: U_String;
    ID: Byte;
    CreateTime, ModificationTime: Double;
    HeaderPOS: Int64;
    CompleteCount: Int64;
    FieldSearch: TFieldSearch;
  end;

  // runtime struct
  TSearchItem_ = record
    Name: U_String;
    Description: U_String;
    ExtID: Byte;
    Size: Int64;
    HeaderPOS: Int64;
    CompleteCount: Int64;
    FieldSearch: TFieldSearch;
  end;

  // runtime struct
  TSearchField_ = record
    Name: U_String;
    Description: U_String;
    HeaderCount: Int64;
    HeaderPOS: Int64;
    CompleteCount: Int64;
    FieldSearch: TFieldSearch;
  end;

  // runtime struct
  TRecursionSearch_ = record
    ReturnHeader: THeader;
    CurrentField: TField;
    InitPath: U_String;
    FilterName: U_String;
    SearchBuffGo: Integer;
    SearchBuff: array [0 .. DB_Max_Secursion_Level] of TFieldSearch;
  end;

  // internal used
function Get_DB_StringL(var IOHnd: TIOHnd): Integer;
function Get_DB_HeaderL(var IOHnd: TIOHnd): Integer;
function Get_DB_ItemL(var IOHnd: TIOHnd): Integer;
function Get_DB_BlockL(var IOHnd: TIOHnd): Integer;
function Get_DB_FieldL(var IOHnd: TIOHnd): Integer;
function Get_DB_L(var IOHnd: TIOHnd): Integer;

function TranslateReturnCode(const ReturnCode: Integer): U_String;

// internal: init store struct
procedure Init_THeader(var Header_: THeader);
procedure Init_TItemBlock(var Block_: TItemBlock);
procedure Init_TItem(var Item_: TItem);
procedure Init_TField(var Field_: TField);
procedure Init_TTMDB(var DB_: TObjectDataHandle); overload;
procedure Init_TTMDB(var DB_: TObjectDataHandle; const FixedStringL: Byte); overload;

// internal: init runtime struct
procedure Init_TFieldSearch(var FieldS_: TFieldSearch);
procedure Init_TTMDBItemHandle(var ItemHnd_: TItemHandle_);
procedure Init_TTMDBSearchHeader(var SearchHeader_: TSearchHeader_);
procedure Init_TTMDBSearchItem(var SearchItem_: TSearchItem_);
procedure Init_TTMDBSearchField(var SearchField_: TSearchField_);
procedure Init_TTMDBRecursionSearch(var RecursionSearch_: TRecursionSearch_);

// internal: header data struct
function dbHeader_WriteRec(const fPos: Int64; var IOHnd: TIOHnd; var Header_: THeader): Boolean;
function dbHeader_ReadRec(const fPos: Int64; var IOHnd: TIOHnd; var Header_: THeader): Boolean;
function dbHeader_ReadReservedRec(const fPos: Int64; var IOHnd: TIOHnd; var Header_: THeader): Boolean;

// internal: item data struct, include header
function dbItem_WriteRec(const fPos: Int64; var IOHnd: TIOHnd; var Item_: TItem): Boolean;
function dbItem_ReadRec(const fPos: Int64; var IOHnd: TIOHnd; var Item_: TItem): Boolean;

// internal: field data struct, include header
function dbField_WriteRec(const fPos: Int64; var IOHnd: TIOHnd; var Field_: TField): Boolean;
function dbField_ReadRec(const fPos: Int64; var IOHnd: TIOHnd; var Field_: TField): Boolean;

// internal: item block struct
function dbItem_OnlyWriteItemBlockRec(const fPos: Int64; var IOHnd: TIOHnd; var Block_: TItemBlock): Boolean;
function dbItem_OnlyReadItemBlockRec(const fPos: Int64; var IOHnd: TIOHnd; var Block_: TItemBlock): Boolean;

// internal: DB struct
function db_WriteRec(const fPos: Int64; var IOHnd: TIOHnd; var DB_: TObjectDataHandle): Boolean;
function db_ReadRec(const fPos: Int64; var IOHnd: TIOHnd; var DB_: TObjectDataHandle): Boolean;

// internal: item data struct, no header
function dbItem_OnlyWriteItemRec(const fPos: Int64; var IOHnd: TIOHnd; var Item_: TItem): Boolean;
function dbItem_OnlyReadItemRec(const fPos: Int64; var IOHnd: TIOHnd; var Item_: TItem): Boolean;

// internal: field data struct, no header
function dbField_OnlyWriteFieldRec(const fPos: Int64; var IOHnd: TIOHnd; var Field_: TField): Boolean;
function dbField_OnlyReadFieldRec(const fPos: Int64; var IOHnd: TIOHnd; var Field_: TField): Boolean;

// internal: string match
function dbMultipleMatch(const SourStr, DestStr: U_String): Boolean;

// internal: find header
function dbHeader_FindNext(const Name: U_String; const FirstHeaderPOS, LastHeaderPOS: Int64; var IOHnd: TIOHnd; var Header_: THeader): Boolean;
function dbHeader_FindPrev(const Name: U_String; const LastHeaderPOS, FirstHeaderPOS: Int64; var IOHnd: TIOHnd; var Header_: THeader): Boolean;

// internal: item block
function dbItem_BlockCreate(var IOHnd: TIOHnd; var Item_: TItem): Boolean;
function dbItem_BlockInit(var IOHnd: TIOHnd; var Item_: TItem): Boolean;
function dbItem_BlockReadData(var IOHnd: TIOHnd; var Item_: TItem; var Buffers; const _Size: Int64): Boolean;
function dbItem_BlockAppendWriteData(var IOHnd: TIOHnd; var Item_: TItem; var Buffers; const Size: Int64): Boolean;
function dbItem_BlockWriteData(var IOHnd: TIOHnd; var Item_: TItem; var Buffers; const Size: Int64): Boolean;
function dbItem_BlockSeekPOS(var IOHnd: TIOHnd; var Item_: TItem; const Position: Int64): Boolean;
function dbItem_BlockGetPOS(var IOHnd: TIOHnd; var Item_: TItem): Int64;
function dbItem_BlockSeekStartPOS(var IOHnd: TIOHnd; var Item_: TItem): Boolean;
function dbItem_BlockSeekLastPOS(var IOHnd: TIOHnd; var Item_: TItem): Boolean;

// internal: field and item
function dbField_GetPOSField(const fPos: Int64; var IOHnd: TIOHnd): TField;
function dbField_GetFirstHeader(const fPos: Int64; var IOHnd: TIOHnd): THeader;
function dbField_GetLastHeader(const fPos: Int64; var IOHnd: TIOHnd): THeader;
function dbField_OnlyFindFirstName(const Name: U_String; const fPos: Int64; var IOHnd: TIOHnd; var FieldS_: TFieldSearch): Boolean;
function dbField_OnlyFindNextName(var IOHnd: TIOHnd; var FieldS_: TFieldSearch): Boolean;
function dbField_OnlyFindLastName(const Name: U_String; const fPos: Int64; var IOHnd: TIOHnd; var FieldS_: TFieldSearch): Boolean;
function dbField_OnlyFindPrevName(var IOHnd: TIOHnd; var FieldS_: TFieldSearch): Boolean;
function dbField_FindFirst(const Name: U_String; const ID: Byte; const fPos: Int64; var IOHnd: TIOHnd; var FieldS_: TFieldSearch): Boolean;
function dbField_FindNext(var IOHnd: TIOHnd; var FieldS_: TFieldSearch): Boolean;
function dbField_FindLast(const Name: U_String; const ID: Byte; const fPos: Int64; var IOHnd: TIOHnd; var FieldS_: TFieldSearch): Boolean;
function dbField_FindPrev(var IOHnd: TIOHnd; var FieldS_: TFieldSearch): Boolean;
function dbField_FindFirstItem(const Name: U_String; const ItemExtID: Byte; const fPos: Int64; var IOHnd: TIOHnd; var FieldS_: TFieldSearch; var Item_: TItem): Boolean; overload;
function dbField_FindNextItem(const ItemExtID: Byte; var IOHnd: TIOHnd; var FieldS_: TFieldSearch; var Item_: TItem): Boolean; overload;
function dbField_FindLastItem(const Name: U_String; const ItemExtID: Byte; const fPos: Int64; var IOHnd: TIOHnd; var FieldS_: TFieldSearch; var Item_: TItem): Boolean; overload;
function dbField_FindPrevItem(const ItemExtID: Byte; var IOHnd: TIOHnd; var FieldS_: TFieldSearch; var Item_: TItem): Boolean; overload;
function dbField_FindFirstItem(const Name: U_String; const ItemExtID: Byte; const fPos: Int64; var IOHnd: TIOHnd; var FieldS_: TFieldSearch): Boolean; overload;
function dbField_FindNextItem(const ItemExtID: Byte; var IOHnd: TIOHnd; var FieldS_: TFieldSearch): Boolean; overload;
function dbField_FindLastItem(const Name: U_String; const ItemExtID: Byte; const fPos: Int64; var IOHnd: TIOHnd; var FieldS_: TFieldSearch): Boolean; overload;
function dbField_FindPrevItem(const ItemExtID: Byte; var IOHnd: TIOHnd; var FieldS_: TFieldSearch): Boolean; overload;
function dbField_ExistItem(const Name: U_String; const ItemExtID: Byte; const fPos: Int64; var IOHnd: TIOHnd): Boolean;
function dbField_ExistHeader(const Name: U_String; const ID: Byte; const fPos: Int64; var IOHnd: TIOHnd): Boolean;
function dbField_CreateHeader(const Name: U_String; const ID: Byte; const fPos: Int64; var IOHnd: TIOHnd; var Header_: THeader): Boolean;
function dbField_InsertNewHeader(const Name: U_String; const ID: Byte; const FieldPos, InsertHeaderPos: Int64; var IOHnd: TIOHnd; var NewHeader: THeader): Boolean;
function dbField_DeleteHeader(const HeaderPOS, FieldPos: Int64; var IOHnd: TIOHnd; var Field_: TField): Boolean;
function dbField_MoveHeader(const HeaderPOS: Int64; const SourcerFieldPOS, TargetFieldPos: Int64; var IOHnd: TIOHnd; var Field_: TField): Boolean;
function dbField_CreateField(const Name: U_String; const fPos: Int64; var IOHnd: TIOHnd; var Field_: TField): Boolean;
function dbField_InsertNewField(const Name: U_String; const FieldPos, CurrentInsertPos: Int64; var IOHnd: TIOHnd; var Field_: TField): Boolean;
function dbField_CreateItem(const Name: U_String; const ExterID: Byte; const fPos: Int64; var IOHnd: TIOHnd; var Item_: TItem): Boolean;
function dbField_InsertNewItem(const Name: U_String; const ExterID: Byte; const FieldPos, CurrentInsertPos: Int64; var IOHnd: TIOHnd; var Item_: TItem): Boolean;
function dbField_CopyItem(var Item_: TItem; var IOHnd: TIOHnd; const DestFieldPos: Int64; var DestIOHnd: TIOHnd): Boolean;
function dbField_CopyItemBuffer(var Item_: TItem; var IOHnd: TIOHnd; var DestItem_: TItem; var DestIOHnd: TIOHnd): Boolean;
function dbField_CopyAllTo(const FilterName: U_String; const FieldPos: Int64; var IOHnd: TIOHnd; const DestFieldPos: Int64; var DestIOHnd: TIOHnd): Boolean;

// API
function db_CreateNew(const FileName: U_String; var DB_: TObjectDataHandle): Boolean;
function db_Open(const FileName: U_String; var DB_: TObjectDataHandle; _OnlyRead: Boolean): Boolean;
function db_CreateAsStream(stream: U_Stream; const Name, Description: U_String; var DB_: TObjectDataHandle): Boolean;
function db_OpenAsStream(stream: U_Stream; const Name: U_String; var DB_: TObjectDataHandle; _OnlyRead: Boolean): Boolean;
function db_ClosePack(var DB_: TObjectDataHandle): Boolean;

// API
function db_CopyFieldTo(const FilterName: U_String; var DB_: TObjectDataHandle; const SourceFieldPos: Int64; var DestTMDB: TObjectDataHandle; const DestFieldPos: Int64): Boolean;
function db_CopyAllTo(var DB_: TObjectDataHandle; var DestTMDB: TObjectDataHandle): Boolean;
function db_CopyAllToDestPath(var DB_: TObjectDataHandle; var DestTMDB: TObjectDataHandle; destPath: U_String): Boolean;

// API
function db_Update(var DB_: TObjectDataHandle): Boolean;

// API
function db_TestName(const Name: U_String): Boolean;

// API
function db_CheckRootField(const Name: U_String; var Field_: TField; var DB_: TObjectDataHandle): Boolean;
function db_CreateRootHeader(const Name: U_String; const ID: Byte; var DB_: TObjectDataHandle; var Header_: THeader): Boolean;
function db_CreateRootField(const Name, Description: U_String; var DB_: TObjectDataHandle): Boolean;
function db_CreateAndSetRootField(const Name, Description: U_String; var DB_: TObjectDataHandle): Boolean;
function db_CreateField(const pathName, Description: U_String; var DB_: TObjectDataHandle): Boolean;

// API
function db_SetFieldName(const pathName, OriginFieldName, NewFieldName, FieldDescription: U_String; var DB_: TObjectDataHandle): Boolean;
function db_SetItemName(const pathName, OriginItemName, NewItemName, ItemDescription: U_String; var DB_: TObjectDataHandle): Boolean;
function db_DeleteField(const pathName, FilterName: U_String; var DB_: TObjectDataHandle): Boolean;

// API
function db_DeleteHeader(const pathName, FilterName: U_String; const ID: Byte; var DB_: TObjectDataHandle): Boolean;
function db_MoveItem(const SourcerPathName, FilterName: U_String; const TargetPathName: U_String; const ItemExtID: Byte; var DB_: TObjectDataHandle): Boolean;
function db_MoveField(const SourcerPathName, FilterName: U_String; const TargetPathName: U_String; var DB_: TObjectDataHandle): Boolean;
function db_MoveHeader(const SourcerPathName, FilterName: U_String; const TargetPathName: U_String; const HeaderID: Byte; var DB_: TObjectDataHandle): Boolean;

// API
function db_SetCurrentRootField(const Name: U_String; var DB_: TObjectDataHandle): Boolean;
function db_SetCurrentField(const pathName: U_String; var DB_: TObjectDataHandle): Boolean;

// API
function db_GetRootField(const Name: U_String; var Field_: TField; var DB_: TObjectDataHandle): Boolean;
function db_GetField(const pathName: U_String; var Field_: TField; var DB_: TObjectDataHandle): Boolean;
function db_GetPath(const FieldPos, RootFieldPos: Int64; var DB_: TObjectDataHandle; var RetPath: U_String): Boolean;

// API
function db_NewItem(const pathName, ItemName, ItemDescription: U_String; const ItemExtID: Byte; var Item_: TItem; var DB_: TObjectDataHandle): Boolean;
function db_DeleteItem(const pathName, FilterName: U_String; const ItemExtID: Byte; var DB_: TObjectDataHandle): Boolean;
function db_GetItem(const pathName, ItemName: U_String; const ItemExtID: Byte; var Item_: TItem; var DB_: TObjectDataHandle): Boolean;

// API
function db_ItemCreate(const pathName, ItemName, ItemDescription: U_String; const ItemExtID: Byte; var ItemHnd_: TItemHandle_; var DB_: TObjectDataHandle): Boolean;
function db_ItemFastCreate(const ItemName, ItemDescription: U_String; const fPos: Int64; const ItemExtID: Byte; var ItemHnd_: TItemHandle_; var DB_: TObjectDataHandle): Boolean;
function db_ItemFastInsertNew(const ItemName, ItemDescription: U_String; const FieldPos, InsertHeaderPos: Int64; const ItemExtID: Byte; var ItemHnd_: TItemHandle_; var DB_: TObjectDataHandle): Boolean;
function db_ItemOpen(const pathName, ItemName: U_String; const ItemExtID: Byte; var ItemHnd_: TItemHandle_; var DB_: TObjectDataHandle): Boolean;
function db_ItemFastOpen(const fPos: Int64; const ItemExtID: Byte; var ItemHnd_: TItemHandle_; var DB_: TObjectDataHandle): Boolean;
function db_ItemClose(var ItemHnd_: TItemHandle_; var DB_: TObjectDataHandle): Boolean;
function db_ItemUpdate(var ItemHnd_: TItemHandle_; var DB_: TObjectDataHandle): Boolean;
function db_ItemBodyReset(var ItemHnd_: TItemHandle_; var DB_: TObjectDataHandle): Boolean;
function db_ItemReName(const FieldPos: Int64; const NewItemName, NewItemDescription: U_String; var ItemHnd_: TItemHandle_; var DB_: TObjectDataHandle): Boolean;

// API
function db_ItemRead(const Size: Int64; var Buffers; var ItemHnd_: TItemHandle_; var DB_: TObjectDataHandle): Boolean;
function db_ItemWrite(const Size: Int64; var Buffers; var ItemHnd_: TItemHandle_; var DB_: TObjectDataHandle): Boolean;

// API
function db_ItemSeekPos(const fPos: Int64; var ItemHnd_: TItemHandle_; var DB_: TObjectDataHandle): Boolean;
function db_ItemSeekStartPos(var ItemHnd_: TItemHandle_; var DB_: TObjectDataHandle): Boolean;
function db_ItemSeekLastPos(var ItemHnd_: TItemHandle_; var DB_: TObjectDataHandle): Boolean;
function db_ItemGetPos(var ItemHnd_: TItemHandle_; var DB_: TObjectDataHandle): Int64;
function db_ItemGetSize(var ItemHnd_: TItemHandle_; var DB_: TObjectDataHandle): Int64;
function db_AppendItemSize(var ItemHnd_: TItemHandle_; const Size: Int64; var DB_: TObjectDataHandle): Boolean;

// API
function db_ExistsRootField(const Name: U_String; var DB_: TObjectDataHandle): Boolean;
function db_FindFirstHeader(const pathName, FilterName: U_String; const ID: Byte; var SenderSearch: TSearchHeader_; var DB_: TObjectDataHandle): Boolean;
function db_FindNextHeader(var SenderSearch: TSearchHeader_; var DB_: TObjectDataHandle): Boolean;
function db_FindLastHeader(const pathName, FilterName: U_String; const ID: Byte; var SenderSearch: TSearchHeader_; var DB_: TObjectDataHandle): Boolean;
function db_FindPrevHeader(var SenderSearch: TSearchHeader_; var DB_: TObjectDataHandle): Boolean;

// API
function db_FindFirstItem(const pathName, FilterName: U_String; const ItemExtID: Byte; var SenderSearch: TSearchItem_; var DB_: TObjectDataHandle): Boolean;
function db_FindNextItem(var SenderSearch: TSearchItem_; const ItemExtID: Byte; var DB_: TObjectDataHandle): Boolean;
function db_FindLastItem(const pathName, FilterName: U_String; const ItemExtID: Byte; var SenderSearch: TSearchItem_; var DB_: TObjectDataHandle): Boolean;
function db_FindPrevItem(var SenderSearch: TSearchItem_; const ItemExtID: Byte; var DB_: TObjectDataHandle): Boolean;

// API
function db_FastFindFirstItem(const FieldPos: Int64; const FilterName: U_String; const ItemExtID: Byte; var SenderSearch: TSearchItem_; var DB_: TObjectDataHandle): Boolean;
function db_FastFindNextItem(var SenderSearch: TSearchItem_; const ItemExtID: Byte; var DB_: TObjectDataHandle): Boolean;
function db_FastFindLastItem(const FieldPos: Int64; const FilterName: U_String; const ItemExtID: Byte; var SenderSearch: TSearchItem_; var DB_: TObjectDataHandle): Boolean;
function db_FastFindPrevItem(var SenderSearch: TSearchItem_; const ItemExtID: Byte; var DB_: TObjectDataHandle): Boolean;

// API
function db_FindFirstField(const pathName, FilterName: U_String; var SenderSearch: TSearchField_; var DB_: TObjectDataHandle): Boolean;
function db_FindNextField(var SenderSearch: TSearchField_; var DB_: TObjectDataHandle): Boolean;
function db_FindLastField(const pathName, FilterName: U_String; var SenderSearch: TSearchField_; var DB_: TObjectDataHandle): Boolean;
function db_FindPrevField(var SenderSearch: TSearchField_; var DB_: TObjectDataHandle): Boolean;

// API
function db_FastFindFirstField(const FieldPos: Int64; const FilterName: U_String; var SenderSearch: TSearchField_; var DB_: TObjectDataHandle): Boolean;
function db_FastFindNextField(var SenderSearch: TSearchField_; var DB_: TObjectDataHandle): Boolean;
function db_FastFindLastField(const FieldPos: Int64; const FilterName: U_String; var SenderSearch: TSearchField_; var DB_: TObjectDataHandle): Boolean;
function db_FastFindPrevField(var SenderSearch: TSearchField_; var DB_: TObjectDataHandle): Boolean;

// API
function db_RecursionSearchFirst(const InitPath, FilterName: U_String; var SenderRecursionSearch: TRecursionSearch_; var DB_: TObjectDataHandle): Boolean;
function db_RecursionSearchNext(var SenderRecursionSearch: TRecursionSearch_; var DB_: TObjectDataHandle): Boolean;

var
  TreeMDBHeaderNameMultipleCharacter: U_SystemString = '?';
  TreeMDBHeaderNameMultipleString: U_SystemString = '*';
  db_FieldPathLimitChar: U_SystemString = '/\';

implementation

uses PascalStrings;

function Get_DB_StringL(var IOHnd: TIOHnd): Integer;
begin
  Result := IOHnd.FixedStringL;
end;

function Get_DB_HeaderL(var IOHnd: TIOHnd): Integer;
begin
  Result := (Get_DB_StringL(IOHnd) * 1) +
    (DB_Position_Size * 3) +
    (DB_Time_Size * 2) +
    (DB_ID_Size * 2) +
    (DB_Property_Size * 1);
end;

function Get_DB_ItemL(var IOHnd: TIOHnd): Integer;
begin
  Result := (Get_DB_StringL(IOHnd) * 1) +
    (DB_ID_Size * 1) +
    (DB_Position_Size * 2) +
    (DB_DataSize_Size * 1) +
    (DB_Counter_Size * 1);
end;

function Get_DB_BlockL(var IOHnd: TIOHnd): Integer;
begin
  Result := (DB_ID_Size * 1) +
    (DB_Position_Size * 4) +
    (DB_DataSize_Size * 1);
end;

function Get_DB_FieldL(var IOHnd: TIOHnd): Integer;
begin
  Result := (Get_DB_StringL(IOHnd) * 1) +
    (DB_Counter_Size * 1) +
    (DB_Position_Size * 3);
end;

function Get_DB_L(var IOHnd: TIOHnd): Integer;
begin
  Result := (DB_ReservedData_Size * 1) +
    (DB_FixedStringL_Size * 1) +
    (DB_Version_Size * 2) +
    (DB_Time_Size * 2) +
    (DB_Counter_Size * 1) +
    (DB_Position_Size * 4) +
    (DB_Level_Size * 1);
end;

function TranslateReturnCode(const ReturnCode: Integer): U_String;
begin
  case ReturnCode of
    DB_Header_ok: Result := 'DB_Header_ok';
    DB_Header_SetPosError: Result := 'DB_Header_SetPosError';
    DB_Header_WritePosError: Result := 'DB_Header_WritePosError';
    DB_Header_WriteNextPosError: Result := 'DB_Header_WriteNextPosError';
    DB_Header_WritePrevPosError: Result := 'DB_Header_WritePrevPosError';
    DB_Header_WritePubMainPosError: Result := 'DB_Header_WritePubMainPosError';
    DB_Header_WriteIDError: Result := 'DB_Header_WriteIDError';
    DB_Header_WritePositionIDError: Result := 'DB_Header_WritePositionIDError';
    DB_Header_WriteNameError: Result := 'DB_Header_WriteNameError';
    DB_Header_WriteCreateTimeError: Result := 'DB_Header_WriteCreateTimeError';
    DB_Header_WriteLastEditTimeError: Result := 'DB_Header_WriteLastEditTimeError';
    DB_Header_WriteUserPropertyIDError: Result := 'DB_Header_WriteUserPropertyIDError';
    DB_Header_ReadPosError: Result := 'DB_Header_ReadPosError';
    DB_Header_ReadNextPosError: Result := 'DB_Header_ReadNextPosError';
    DB_Header_ReadPrevPosError: Result := 'DB_Header_ReadPrevPosError';
    DB_Header_ReadPubMainPosError: Result := 'DB_Header_ReadPubMainPosError';
    DB_Header_ReadIDError: Result := 'DB_Header_ReadIDError';
    DB_Header_ReadPositionIDError: Result := 'DB_Header_ReadPositionIDError';
    DB_Header_ReadNameError: Result := 'DB_Header_ReadNameError';
    DB_Header_ReadCreateTimeError: Result := 'DB_Header_ReadCreateTimeError';
    DB_Header_ReadLastEditTimeError: Result := 'DB_Header_ReadLastEditTimeError';
    DB_Header_ReadUserPropertyIDError: Result := 'DB_Header_ReadUserPropertyIDError';
    DB_Header_NotFindHeader: Result := 'DB_Header_NotFindHeader';
    DB_Item_ok: Result := 'DB_Item_ok';
    DB_Item_SetPosError: Result := 'DB_Item_SetPosError';
    DB_Item_WriteRecDescriptionError: Result := 'DB_Item_WriteRecDescriptionError';
    DB_Item_WriteRecExterIDError: Result := 'DB_Item_WriteRecExterIDError';
    DB_Item_WriteFirstBlockPOSError: Result := 'DB_Item_WriteFirstBlockPOSError';
    DB_Item_WriteLastBlockPOSError: Result := 'DB_Item_WriteLastBlockPOSError';
    DB_Item_WriteRecBuffSizeError: Result := 'DB_Item_WriteRecBuffSizeError';
    DB_Item_WriteBlockCountError: Result := 'DB_Item_WriteBlockCountError';
    DB_Item_ReadRecDescriptionError: Result := 'DB_Item_ReadRecDescriptionError';
    DB_Item_ReadRecExterIDError: Result := 'DB_Item_ReadRecExterIDError';
    DB_Item_ReadFirstBlockPOSError: Result := 'DB_Item_ReadFirstBlockPOSError';
    DB_Item_ReadLastBlockPOSError: Result := 'DB_Item_ReadLastBlockPOSError';
    DB_Item_ReadRecBuffSizeError: Result := 'DB_Item_ReadRecBuffSizeError';
    DB_Item_ReadBlockCountError: Result := 'DB_Item_ReadBlockCountError';
    DB_Item_WriteItemBlockIDFlagsError: Result := 'DB_Item_WriteItemBlockIDFlagsError';
    DB_Item_WriteCurrentBlockPOSError: Result := 'DB_Item_WriteCurrentBlockPOSError';
    DB_Item_WriteNextBlockPOSError: Result := 'DB_Item_WriteNextBlockPOSError';
    DB_Item_WritePrevBlockPOSError: Result := 'DB_Item_WritePrevBlockPOSError';
    DB_Item_WriteDataBlockPOSError: Result := 'DB_Item_WriteDataBlockPOSError';
    DB_Item_WriteDataBuffSizeError: Result := 'DB_Item_WriteDataBuffSizeError';
    DB_Item_ReadItemBlockIDFlagsError: Result := 'DB_Item_ReadItemBlockIDFlagsError';
    DB_Item_ReadCurrentBlockPOSError: Result := 'DB_Item_ReadCurrentBlockPOSError';
    DB_Item_ReadNextBlockPOSError: Result := 'DB_Item_ReadNextBlockPOSError';
    DB_Item_ReadPrevBlockPOSError: Result := 'DB_Item_ReadPrevBlockPOSError';
    DB_Item_ReadDataBlockPOSError: Result := 'DB_Item_ReadDataBlockPOSError';
    DB_Item_ReadDataBuffSizeError: Result := 'DB_Item_ReadDataBuffSizeError';
    DB_Item_BlockPositionError: Result := 'DB_Item_BlockPositionError';
    DB_Item_BlockOverrate: Result := 'DB_Item_BlockOverrate';
    DB_Item_BlockReadError: Result := 'DB_Item_BlockReadError';
    DB_Item_BlockWriteError: Result := 'DB_Item_BlockWriteError';
    DB_Field_ok: Result := 'DB_Field_ok';
    DB_Field_SetPosError: Result := 'DB_Field_SetPosError';
    DB_Field_WriteHeaderFieldPosError: Result := 'DB_Field_WriteHeaderFieldPosError';
    DB_Field_WriteDescriptionError: Result := 'DB_Field_WriteDescriptionError';
    DB_Field_WriteCountError: Result := 'DB_Field_WriteCountError';
    DB_Field_WriteFirstPosError: Result := 'DB_Field_WriteFirstPosError';
    DB_Field_WriteLastPosError: Result := 'DB_Field_WriteLastPosError';
    DB_Field_ReadHeaderFieldPosError: Result := 'DB_Field_ReadHeaderFieldPosError';
    DB_Field_ReadDescriptionError: Result := 'DB_Field_ReadDescriptionError';
    DB_Field_ReadCountError: Result := 'DB_Field_ReadCountError';
    DB_Field_ReadFirstPosError: Result := 'DB_Field_ReadFirstPosError';
    DB_Field_ReadLastPosError: Result := 'DB_Field_ReadLastPosError';
    DB_Field_NotInitSearch: Result := 'DB_Field_NotInitSearch';
    DB_Field_DeleteHeaderError: Result := 'DB_Field_DeleteHeaderError';
    DB_ok: Result := 'DB_ok';
    DB_RepOpenPackError: Result := 'DB_RepOpenPackError';
    DB_CreatePackError: Result := 'DB_CreatePackError';
    DB_WriteReservedDataError: Result := 'DB_WriteReservedDataError';
    DB_WriteNameError: Result := 'DB_WriteNameError';
    DB_WriteDescriptionError: Result := 'DB_WriteDescriptionError';
    DB_PositionSeekError: Result := 'DB_PositionSeekError';
    DB_WriteMajorVersionError: Result := 'DB_WriteMajorVersionError';
    DB_WriteMinorVersionError: Result := 'DB_WriteMinorVersionError';
    DB_WriteCreateTimeError: Result := 'DB_WriteCreateTimeError';
    DB_WriteLastEditTimeError: Result := 'DB_WriteLastEditTimeError';
    DB_WriteHeaderCountError: Result := 'DB_WriteHeaderCountError';
    DB_WriteDefaultPositionError: Result := 'DB_WriteDefaultPositionError';
    DB_WriteFirstPositionError: Result := 'DB_WriteFirstPositionError';
    DB_WriteLastPositionError: Result := 'DB_WriteLastPositionError';
    DB_WriteFixedStringLError: Result := 'DB_WriteFixedStringLError';
    DB_ReadReservedDataError: Result := 'DB_ReadReservedDataError';
    DB_ReadNameError: Result := 'DB_ReadNameError';
    DB_ReadDescriptionError: Result := 'DB_ReadDescriptionError';
    DB_ReadMajorVersionError: Result := 'DB_ReadMajorVersionError';
    DB_ReadMinorVersionError: Result := 'DB_ReadMinorVersionError';
    DB_ReadCreateTimeError: Result := 'DB_ReadCreateTimeError';
    DB_ReadLastEditTimeError: Result := 'DB_ReadLastEditTimeError';
    DB_ReadHeaderCountError: Result := 'DB_ReadHeaderCountError';
    DB_ReadDefaultPositionError: Result := 'DB_ReadDefaultPositionError';
    DB_ReadFirstPositionError: Result := 'DB_ReadFirstPositionError';
    DB_ReadLastPositionError: Result := 'DB_ReadLastPositionError';
    DB_ReadFixedStringLError: Result := 'DB_ReadFixedStringLError';
    DB_RepCreatePackError: Result := 'DB_RepCreatePackError';
    DB_OpenPackError: Result := 'DB_OpenPackError';
    DB_ClosePackError: Result := 'DB_ClosePackError';
    DB_WriteCurrentPositionError: Result := 'DB_WriteCurrentPositionError';
    DB_WriteCurrentLevelError: Result := 'DB_WriteCurrentLevelError';
    DB_ReadCurrentPositionError: Result := 'DB_ReadCurrentPositionError';
    DB_ReadCurrentLevelError: Result := 'DB_ReadCurrentLevelError';
    DB_PathNameError: Result := 'DB_PathNameError';
    DB_RepeatCreateItemError: Result := 'DB_RepeatCreateItemError';
    DB_OpenItemError: Result := 'DB_OpenItemError';
    DB_ItemNameError: Result := 'DB_ItemNameError';
    DB_RepeatOpenItemError: Result := 'DB_RepeatOpenItemError';
    DB_CloseItemError: Result := 'DB_CloseItemError';
    DB_ItemStructNotFindDescription: Result := 'DB_ItemStructNotFindDescription';
    DB_RecursionSearchOver: Result := 'DB_RecursionSearchOver';
    DB_FileBufferError: Result := 'DB_FileBufferError';
    DB_CheckIOError: Result := 'DB_CheckIOError';
    DB_ExceptionError: Result := 'DB_ExceptionError';
    else Result := 'unknow error';
  end;
end;

function db_GetPathCount(const StrName: U_String): Integer;
begin
  Result := umlGetIndexStrCount(StrName, db_FieldPathLimitChar);
end;

function db_DeleteFirstPath(const pathName: U_String): U_String;
begin
  Result := umlDeleteFirstStr(pathName, db_FieldPathLimitChar).Text;
end;

function db_DeleteLastPath(const pathName: U_String): U_String;
begin
  Result := umlDeleteLastStr(pathName, db_FieldPathLimitChar).Text;
end;

function db_GetFirstPath(const pathName: U_String): U_String;
begin
  Result := umlGetFirstStr(pathName, db_FieldPathLimitChar).Text;
end;

function db_GetLastPath(const pathName: U_String): U_String;
begin
  Result := umlGetLastStr(pathName, db_FieldPathLimitChar).Text;
end;

procedure Init_THeader(var Header_: THeader);
begin
  Header_.CurrentHeader := 0;
  Header_.NextHeader := 0;
  Header_.PrevHeader := 0;
  Header_.DataPosition := 0;
  Header_.CreateTime := 0;
  Header_.ModificationTime := 0;
  Header_.ID := 0;
  Header_.PositionID := 0;
  Header_.UserProperty := 0;
  Header_.Name := '';
  Header_.State := DB_Header_ok;
end;

procedure Init_TItemBlock(var Block_: TItemBlock);
begin
  Block_.ID := 0;
  Block_.CurrentBlockPOS := 0;
  Block_.NextBlockPOS := 0;
  Block_.PrevBlockPOS := 0;
  Block_.DataPosition := 0;
  Block_.Size := 0;
  Block_.State := DB_Item_ok;
end;

procedure Init_TItem(var Item_: TItem);
begin
  Init_THeader(Item_.RHeader);
  Item_.Description := '';
  Item_.ExtID := 0;
  Item_.FirstBlockPOS := 0;
  Item_.LastBlockPOS := 0;
  Item_.Size := 0;
  Item_.BlockCount := 0;
  Item_.CurrentBlockSeekPOS := 0;
  Item_.CurrentFileSeekPOS := 0;
  Init_TItemBlock(Item_.CurrentItemBlock);
  Item_.DataModification := False;
  Item_.State := DB_Item_ok;
end;

procedure Init_TField(var Field_: TField);
begin
  Field_.UpFieldPOS := 0;
  Field_.Description := '';
  Field_.HeaderCount := 0;
  Field_.FirstHeaderPOS := 0;
  Field_.LastHeaderPOS := 0;
  Init_THeader(Field_.RHeader);
  Field_.State := DB_Field_ok;
end;

procedure Init_TTMDB(var DB_: TObjectDataHandle);
begin
  Init_TTMDB(DB_, 64 + 1);
end;

procedure Init_TTMDB(var DB_: TObjectDataHandle; const FixedStringL: Byte);
begin
  InitIOHnd(DB_.IOHnd);
  DB_.IOHnd.FixedStringL := FixedStringL;
  FillPtrByte(@DB_.ReservedData[0], DB_ReservedData_Size, 0);
  DB_.FixedStringL := DB_.IOHnd.FixedStringL;
  DB_.MajorVer := 0;
  DB_.MinorVer := 0;
  DB_.CreateTime := 0;
  DB_.ModificationTime := 0;
  DB_.RootHeaderCount := 0;
  DB_.DefaultFieldPOS := 0;
  DB_.FirstHeaderPOS := 0;
  DB_.LastHeaderPOS := 0;
  DB_.CurrentFieldPOS := 0;
  DB_.CurrentFieldLevel := 0;
  DB_.IOHnd.Data := @DB_;
  DB_.OverWriteItem := True;
  DB_.AllowSameHeaderName := False;

  DB_.OnError := nil;

  DB_.OnDeleteHeader := nil;

  DB_.OnPrepareWriteHeader := nil;
  DB_.OnWriteHeader := nil;
  DB_.OnReadHeader := nil;

  DB_.OnPrepareWriteItemBlock := nil;
  DB_.OnWriteItemBlock := nil;
  DB_.OnReadItemBlock := nil;

  DB_.OnPrepareWriteItem := nil;
  DB_.OnWriteItem := nil;
  DB_.OnReadItem := nil;

  DB_.OnPrepareOnlyWriteItemRec := nil;
  DB_.OnOnlyWriteItemRec := nil;
  DB_.OnOnlyReadItemRec := nil;

  DB_.OnPrepareWriteField := nil;
  DB_.OnWriteField := nil;
  DB_.OnReadField := nil;

  DB_.OnPrepareOnlyWriteFieldRec := nil;
  DB_.OnOnlyWriteFieldRec := nil;
  DB_.OnOnlyReadFieldRec := nil;

  DB_.OnPrepareWriteTMDB := nil;
  DB_.OnWriteTMDB := nil;
  DB_.OnReadTMDB := nil;

  DB_.State := DB_ok;
end;

procedure Init_TFieldSearch(var FieldS_: TFieldSearch);
begin
  FieldS_.InitFlags := False;
  FieldS_.StartPos := 0;
  FieldS_.OverPOS := 0;
  FieldS_.Name := '';
  FieldS_.ID := 0;
  FieldS_.PositionID := 0;
  Init_THeader(FieldS_.RHeader);
  FieldS_.State := DB_Field_ok;
end;

procedure Init_TTMDBItemHandle(var ItemHnd_: TItemHandle_);
begin
  Init_TItem(ItemHnd_.Item);
  ItemHnd_.Name := '';
  ItemHnd_.Description := '';
  ItemHnd_.CreateTime := 0;
  ItemHnd_.ModificationTime := 0;
  ItemHnd_.ItemExtID := 0;
  ItemHnd_.OpenFlags := False;
end;

procedure Init_TTMDBSearchHeader(var SearchHeader_: TSearchHeader_);
begin
  SearchHeader_.Name := '';
  SearchHeader_.ID := 0;
  SearchHeader_.CreateTime := 0;
  SearchHeader_.ModificationTime := 0;
  SearchHeader_.HeaderPOS := 0;
  SearchHeader_.CompleteCount := 0;
  Init_TFieldSearch(SearchHeader_.FieldSearch);
end;

procedure Init_TTMDBSearchItem(var SearchItem_: TSearchItem_);
begin
  SearchItem_.Name := '';
  SearchItem_.Description := '';
  SearchItem_.ExtID := 0;
  SearchItem_.Size := 0;
  SearchItem_.HeaderPOS := 0;
  SearchItem_.CompleteCount := 0;
  Init_TFieldSearch(SearchItem_.FieldSearch);
end;

procedure Init_TTMDBSearchField(var SearchField_: TSearchField_);
begin
  SearchField_.Name := '';
  SearchField_.Description := '';
  SearchField_.HeaderCount := 0;
  SearchField_.HeaderPOS := 0;
  SearchField_.CompleteCount := 0;
  Init_TFieldSearch(SearchField_.FieldSearch);
end;

procedure Init_TTMDBRecursionSearch(var RecursionSearch_: TRecursionSearch_);
var
  i: Integer;
begin
  Init_THeader(RecursionSearch_.ReturnHeader);
  Init_TField(RecursionSearch_.CurrentField);
  RecursionSearch_.InitPath := '';
  RecursionSearch_.FilterName := '';
  RecursionSearch_.SearchBuffGo := 0;
  for i := 0 to DB_Max_Secursion_Level do
      Init_TFieldSearch(RecursionSearch_.SearchBuff[i]);
end;

function dbHeader_WriteRec(const fPos: Int64; var IOHnd: TIOHnd; var Header_: THeader): Boolean;
begin
  Result := False;
  Header_.State := DB_ExceptionError;
  try
    if (IOHnd.IsOnlyRead) or (not IOHnd.IsOpen) then
      begin
        Header_.State := DB_CheckIOError;
        Result := False;
        exit;
      end;

    if IOHnd.Data <> nil then
      if Assigned(PObjectDataHandle(IOHnd.Data)^.OnPrepareWriteHeader) then
        begin
          Result := False;
          PObjectDataHandle(IOHnd.Data)^.OnPrepareWriteHeader(fPos, Header_, Result);
          if Result then
            begin
              Header_.State := DB_Header_ok;
              if Assigned(PObjectDataHandle(IOHnd.Data)^.OnWriteHeader) then
                  PObjectDataHandle(IOHnd.Data)^.OnWriteHeader(fPos, Header_);
              exit;
            end;
        end;

    if umlFileSeek(IOHnd, fPos) = False then
      begin
        Header_.State := DB_Header_SetPosError;
        Result := False;
        exit;
      end;
    if umlFileWrite(IOHnd, DB_Position_Size, Header_.NextHeader) = False then
      begin
        Header_.State := DB_Header_WriteNextPosError;
        Result := False;
        exit;
      end;
    if umlFileWrite(IOHnd, DB_Position_Size, Header_.PrevHeader) = False then
      begin
        Header_.State := DB_Header_WritePrevPosError;
        Result := False;
        exit;
      end;
    if umlFileWrite(IOHnd, DB_Position_Size, Header_.DataPosition) = False then
      begin
        Header_.State := DB_Header_WritePubMainPosError;
        Result := False;
        exit;
      end;
    if umlFileWrite(IOHnd, DB_Time_Size, Header_.CreateTime) = False then
      begin
        Header_.State := DB_Header_WriteCreateTimeError;
        Result := False;
        exit;
      end;
    if umlFileWrite(IOHnd, DB_Time_Size, Header_.ModificationTime) = False then
      begin
        Header_.State := DB_Header_WriteLastEditTimeError;
        Result := False;
        exit;
      end;
    if umlFileWrite(IOHnd, DB_ID_Size, Header_.ID) = False then
      begin
        Header_.State := DB_Header_WriteIDError;
        Result := False;
        exit;
      end;
    if umlFileWrite(IOHnd, DB_ID_Size, Header_.PositionID) = False then
      begin
        Header_.State := DB_Header_WritePositionIDError;
        Result := False;
        exit;
      end;
    if umlFileWrite(IOHnd, DB_Property_Size, Header_.UserProperty) = False then
      begin
        Header_.State := DB_Header_WriteUserPropertyIDError;
        Result := False;
        exit;
      end;
    if umlFileWriteFixedString(IOHnd, Header_.Name) = False then
      begin
        Header_.State := DB_Header_WriteNameError;
        Result := False;
        exit;
      end;

    Header_.State := DB_Header_ok;
    Result := True;

    if IOHnd.Data <> nil then
      if Assigned(PObjectDataHandle(IOHnd.Data)^.OnWriteHeader) then
          PObjectDataHandle(IOHnd.Data)^.OnWriteHeader(fPos, Header_);
  finally
    if not Result then
      if IOHnd.Data <> nil then
        if Assigned(PObjectDataHandle(IOHnd.Data)^.OnError) then
            PObjectDataHandle(IOHnd.Data)^.OnError(TranslateReturnCode(Header_.State));
  end;
end;

function dbHeader_ReadRec(const fPos: Int64; var IOHnd: TIOHnd; var Header_: THeader): Boolean;
begin
  Result := False;
  Header_.State := DB_ExceptionError;
  try
    if IOHnd.Data <> nil then
      if Assigned(PObjectDataHandle(IOHnd.Data)^.OnReadHeader) then
        begin
          Result := False;
          PObjectDataHandle(IOHnd.Data)^.OnReadHeader(fPos, Header_, Result);
          if Result then
              exit;
        end;

    if umlFileSeek(IOHnd, fPos) = False then
      begin
        Header_.State := DB_Header_SetPosError;
        Result := False;
        exit;
      end;

    Header_.CurrentHeader := fPos;

    if umlFileRead(IOHnd, DB_Position_Size, Header_.NextHeader) = False then
      begin
        Header_.State := DB_Header_ReadNextPosError;
        Result := False;
        exit;
      end;
    if umlFileRead(IOHnd, DB_Position_Size, Header_.PrevHeader) = False then
      begin
        Header_.State := DB_Header_ReadPrevPosError;
        Result := False;
        exit;
      end;
    if umlFileRead(IOHnd, DB_Position_Size, Header_.DataPosition) = False then
      begin
        Header_.State := DB_Header_ReadPubMainPosError;
        Result := False;
        exit;
      end;
    if umlFileRead(IOHnd, DB_Time_Size, Header_.CreateTime) = False then
      begin
        Header_.State := DB_Header_ReadCreateTimeError;
        Result := False;
        exit;
      end;
    if umlFileRead(IOHnd, DB_Time_Size, Header_.ModificationTime) = False then
      begin
        Header_.State := DB_Header_ReadLastEditTimeError;
        Result := False;
        exit;
      end;
    if umlFileRead(IOHnd, DB_ID_Size, Header_.ID) = False then
      begin
        Header_.State := DB_Header_ReadIDError;
        Result := False;
        exit;
      end;
    if umlFileRead(IOHnd, DB_ID_Size, Header_.PositionID) = False then
      begin
        Header_.State := DB_Header_ReadPositionIDError;
        Result := False;
        exit;
      end;
    if umlFileRead(IOHnd, DB_Property_Size, Header_.UserProperty) = False then
      begin
        Header_.State := DB_Header_ReadUserPropertyIDError;
        Result := False;
        exit;
      end;
    if umlFileReadFixedString(IOHnd, Header_.Name) = False then
      begin
        Header_.State := DB_Header_ReadNameError;
        Result := False;
        exit;
      end;

    Header_.State := DB_Header_ok;
    Result := True;

    if IOHnd.Data <> nil then
      if Assigned(PObjectDataHandle(IOHnd.Data)^.OnWriteHeader) then
          PObjectDataHandle(IOHnd.Data)^.OnWriteHeader(fPos, Header_);
  finally
    if not Result then
      if IOHnd.Data <> nil then
        if Assigned(PObjectDataHandle(IOHnd.Data)^.OnError) then
            PObjectDataHandle(IOHnd.Data)^.OnError(TranslateReturnCode(Header_.State));
  end;
end;

function dbHeader_ReadReservedRec(const fPos: Int64; var IOHnd: TIOHnd; var Header_: THeader): Boolean;
var
  h: THeader;
begin
  Result := dbHeader_ReadRec(fPos, IOHnd, h);
  Header_.CurrentHeader := h.CurrentHeader;
  Header_.NextHeader := h.NextHeader;
  Header_.PrevHeader := h.PrevHeader;
  Header_.DataPosition := h.DataPosition;
  Header_.ID := h.ID;
  Header_.PositionID := h.PositionID;
end;

function dbItem_WriteRec(const fPos: Int64; var IOHnd: TIOHnd; var Item_: TItem): Boolean;
begin
  Result := False;
  Item_.State := DB_ExceptionError;
  try
    if (IOHnd.IsOnlyRead) or (not IOHnd.IsOpen) then
      begin
        Item_.State := DB_CheckIOError;
        Item_.RHeader.State := DB_CheckIOError;
        Result := False;
        exit;
      end;
    if IOHnd.Data <> nil then
      if Assigned(PObjectDataHandle(IOHnd.Data)^.OnPrepareWriteItem) then
        begin
          Result := False;
          PObjectDataHandle(IOHnd.Data)^.OnPrepareWriteItem(fPos, Item_, Result);
          if Result then
            begin
              Item_.State := DB_Item_ok;
              if Assigned(PObjectDataHandle(IOHnd.Data)^.OnWriteItem) then
                  PObjectDataHandle(IOHnd.Data)^.OnWriteItem(fPos, Item_);
              exit;
            end;
        end;

    if dbHeader_WriteRec(fPos, IOHnd, Item_.RHeader) = False then
      begin
        Item_.State := Item_.RHeader.State;
        Result := False;
        exit;
      end;
    if umlFileSeek(IOHnd, Item_.RHeader.DataPosition) = False then
      begin
        Item_.State := DB_Item_SetPosError;
        Result := False;
        exit;
      end;
    if umlFileWriteFixedString(IOHnd, Item_.Description) = False then
      begin
        Item_.State := DB_Item_WriteRecDescriptionError;
        Result := False;
        exit;
      end;
    if umlFileWrite(IOHnd, DB_ID_Size, Item_.ExtID) = False then
      begin
        Item_.State := DB_Item_WriteRecExterIDError;
        Result := False;
        exit;
      end;
    if umlFileWrite(IOHnd, DB_Position_Size, Item_.FirstBlockPOS) = False then
      begin
        Item_.State := DB_Item_WriteFirstBlockPOSError;
        Result := False;
        exit;
      end;
    if umlFileWrite(IOHnd, DB_Position_Size, Item_.LastBlockPOS) = False then
      begin
        Item_.State := DB_Item_WriteLastBlockPOSError;
        Result := False;
        exit;
      end;
    if umlFileWrite(IOHnd, DB_DataSize_Size, Item_.Size) = False then
      begin
        Item_.State := DB_Item_WriteRecBuffSizeError;
        Result := False;
        exit;
      end;
    if umlFileWrite(IOHnd, DB_Counter_Size, Item_.BlockCount) = False then
      begin
        Item_.State := DB_Item_WriteBlockCountError;
        Result := False;
        exit;
      end;
    Item_.State := DB_Item_ok;
    Result := True;

    if IOHnd.Data <> nil then
      if Assigned(PObjectDataHandle(IOHnd.Data)^.OnWriteItem) then
          PObjectDataHandle(IOHnd.Data)^.OnWriteItem(fPos, Item_);
  finally
    if not Result then
      if IOHnd.Data <> nil then
        if Assigned(PObjectDataHandle(IOHnd.Data)^.OnError) then
            PObjectDataHandle(IOHnd.Data)^.OnError(TranslateReturnCode(Item_.State));
  end;
end;

function dbItem_ReadRec(const fPos: Int64; var IOHnd: TIOHnd; var Item_: TItem): Boolean;
begin
  Result := False;
  Item_.State := DB_ExceptionError;
  try
    if IOHnd.Data <> nil then
      if Assigned(PObjectDataHandle(IOHnd.Data)^.OnReadItem) then
        begin
          Result := False;
          PObjectDataHandle(IOHnd.Data)^.OnReadItem(fPos, Item_, Result);
          if Result then
              exit;
        end;

    if dbHeader_ReadRec(fPos, IOHnd, Item_.RHeader) = False then
      begin
        Item_.State := Item_.RHeader.State;
        Result := False;
        exit;
      end;
    if umlFileSeek(IOHnd, Item_.RHeader.DataPosition) = False then
      begin
        Item_.State := DB_Item_SetPosError;
        Result := False;
        exit;
      end;
    if umlFileReadFixedString(IOHnd, Item_.Description) = False then
      begin
        Item_.State := DB_Item_ReadRecDescriptionError;
        Result := False;
        exit;
      end;
    if umlFileRead(IOHnd, DB_ID_Size, Item_.ExtID) = False then
      begin
        Item_.State := DB_Item_ReadRecExterIDError;
        Result := False;
        exit;
      end;
    if umlFileRead(IOHnd, DB_Position_Size, Item_.FirstBlockPOS) = False then
      begin
        Item_.State := DB_Item_ReadFirstBlockPOSError;
        Result := False;
        exit;
      end;
    if umlFileRead(IOHnd, DB_Position_Size, Item_.LastBlockPOS) = False then
      begin
        Item_.State := DB_Item_ReadLastBlockPOSError;
        Result := False;
        exit;
      end;
    if umlFileRead(IOHnd, DB_DataSize_Size, Item_.Size) = False then
      begin
        Item_.State := DB_Item_ReadRecBuffSizeError;
        Result := False;
        exit;
      end;
    if umlFileRead(IOHnd, DB_Counter_Size, Item_.BlockCount) = False then
      begin
        Item_.State := DB_Item_ReadBlockCountError;
        Result := False;
        exit;
      end;
    Item_.State := DB_Item_ok;
    Result := True;

    if IOHnd.Data <> nil then
      if Assigned(PObjectDataHandle(IOHnd.Data)^.OnWriteItem) then
          PObjectDataHandle(IOHnd.Data)^.OnWriteItem(fPos, Item_);
  finally
    if not Result then
      if IOHnd.Data <> nil then
        if Assigned(PObjectDataHandle(IOHnd.Data)^.OnError) then
            PObjectDataHandle(IOHnd.Data)^.OnError(TranslateReturnCode(Item_.State));
  end;
end;

function dbField_WriteRec(const fPos: Int64; var IOHnd: TIOHnd; var Field_: TField): Boolean;
begin
  Result := False;
  Field_.State := DB_ExceptionError;
  try
    if (IOHnd.IsOnlyRead) or (not IOHnd.IsOpen) then
      begin
        Field_.State := DB_CheckIOError;
        Field_.RHeader.State := DB_CheckIOError;
        Result := False;
        exit;
      end;
    if IOHnd.Data <> nil then
      if Assigned(PObjectDataHandle(IOHnd.Data)^.OnPrepareWriteField) then
        begin
          Result := False;
          PObjectDataHandle(IOHnd.Data)^.OnPrepareWriteField(fPos, Field_, Result);
          if Result then
            begin
              Field_.State := DB_Field_ok;
              if Assigned(PObjectDataHandle(IOHnd.Data)^.OnWriteField) then
                  PObjectDataHandle(IOHnd.Data)^.OnWriteField(fPos, Field_);
              exit;
            end;
        end;

    if dbHeader_WriteRec(fPos, IOHnd, Field_.RHeader) = False then
      begin
        Field_.State := Field_.RHeader.State;
        Result := False;
        exit;
      end;
    if umlFileSeek(IOHnd, Field_.RHeader.DataPosition) = False then
      begin
        Field_.State := DB_Field_SetPosError;
        Result := False;
        exit;
      end;
    if umlFileWrite(IOHnd, DB_Position_Size, Field_.UpFieldPOS) = False then
      begin
        Field_.State := DB_Field_WriteHeaderFieldPosError;
        Result := False;
        exit;
      end;
    if umlFileWriteFixedString(IOHnd, Field_.Description) = False then
      begin
        Field_.State := DB_Field_WriteDescriptionError;
        Result := False;
        exit;
      end;
    if umlFileWrite(IOHnd, DB_Counter_Size, Field_.HeaderCount) = False then
      begin
        Field_.State := DB_Field_WriteCountError;
        Result := False;
        exit;
      end;
    if umlFileWrite(IOHnd, DB_Position_Size, Field_.FirstHeaderPOS) = False then
      begin
        Field_.State := DB_Field_WriteFirstPosError;
        Result := False;
        exit;
      end;
    if umlFileWrite(IOHnd, DB_Position_Size, Field_.LastHeaderPOS) = False then
      begin
        Field_.State := DB_Field_WriteLastPosError;
        Result := False;
        exit;
      end;
    Field_.State := DB_Field_ok;
    Result := True;

    if IOHnd.Data <> nil then
      if Assigned(PObjectDataHandle(IOHnd.Data)^.OnWriteField) then
          PObjectDataHandle(IOHnd.Data)^.OnWriteField(fPos, Field_);
  finally
    if not Result then
      if IOHnd.Data <> nil then
        if Assigned(PObjectDataHandle(IOHnd.Data)^.OnError) then
            PObjectDataHandle(IOHnd.Data)^.OnError(TranslateReturnCode(Field_.State));
  end;
end;

function dbField_ReadRec(const fPos: Int64; var IOHnd: TIOHnd; var Field_: TField): Boolean;
begin
  Result := False;
  Field_.State := DB_ExceptionError;
  try
    if IOHnd.Data <> nil then
      if Assigned(PObjectDataHandle(IOHnd.Data)^.OnReadField) then
        begin
          Result := False;
          PObjectDataHandle(IOHnd.Data)^.OnReadField(fPos, Field_, Result);
          if Result then
              exit;
        end;

    if dbHeader_ReadRec(fPos, IOHnd, Field_.RHeader) = False then
      begin
        Field_.State := Field_.RHeader.State;
        Result := False;
        exit;
      end;
    if umlFileSeek(IOHnd, Field_.RHeader.DataPosition) = False then
      begin
        Field_.State := DB_Field_SetPosError;
        Result := False;
        exit;
      end;
    if umlFileRead(IOHnd, DB_Position_Size, Field_.UpFieldPOS) = False then
      begin
        Field_.State := DB_Field_ReadHeaderFieldPosError;
        Result := False;
        exit;
      end;
    if umlFileReadFixedString(IOHnd, Field_.Description) = False then
      begin
        Field_.State := DB_Field_ReadDescriptionError;
        Result := False;
        exit;
      end;
    if umlFileRead(IOHnd, DB_Counter_Size, Field_.HeaderCount) = False then
      begin
        Field_.State := DB_Field_ReadCountError;
        Result := False;
        exit;
      end;
    if umlFileRead(IOHnd, DB_Position_Size, Field_.FirstHeaderPOS) = False then
      begin
        Field_.State := DB_Field_ReadFirstPosError;
        Result := False;
        exit;
      end;
    if umlFileRead(IOHnd, DB_Position_Size, Field_.LastHeaderPOS) = False then
      begin
        Field_.State := DB_Field_ReadLastPosError;
        Result := False;
        exit;
      end;
    Field_.State := DB_Field_ok;
    Result := True;

    if IOHnd.Data <> nil then
      if Assigned(PObjectDataHandle(IOHnd.Data)^.OnWriteField) then
          PObjectDataHandle(IOHnd.Data)^.OnWriteField(fPos, Field_);
  finally
    if not Result then
      if IOHnd.Data <> nil then
        if Assigned(PObjectDataHandle(IOHnd.Data)^.OnError) then
            PObjectDataHandle(IOHnd.Data)^.OnError(TranslateReturnCode(Field_.State));
  end;
end;

function dbItem_OnlyWriteItemBlockRec(const fPos: Int64; var IOHnd: TIOHnd; var Block_: TItemBlock): Boolean;
begin
  Result := False;
  Block_.State := DB_ExceptionError;
  try
    if (IOHnd.IsOnlyRead) or (not IOHnd.IsOpen) then
      begin
        Block_.State := DB_CheckIOError;
        Result := False;
        exit;
      end;
    if IOHnd.Data <> nil then
      if Assigned(PObjectDataHandle(IOHnd.Data)^.OnPrepareWriteItemBlock) then
        begin
          Result := False;
          PObjectDataHandle(IOHnd.Data)^.OnPrepareWriteItemBlock(fPos, Block_, Result);
          if Result then
            begin
              Block_.State := DB_Item_ok;
              if Assigned(PObjectDataHandle(IOHnd.Data)^.OnWriteItemBlock) then
                  PObjectDataHandle(IOHnd.Data)^.OnWriteItemBlock(fPos, Block_);
              exit;
            end;
        end;

    if umlFileSeek(IOHnd, fPos) = False then
      begin
        Block_.State := DB_Item_SetPosError;
        Result := False;
        exit;
      end;
    if umlFileWrite(IOHnd, DB_ID_Size, Block_.ID) = False then
      begin
        Block_.State := DB_Item_WriteItemBlockIDFlagsError;
        Result := False;
        exit;
      end;
    if umlFileWrite(IOHnd, DB_Position_Size, Block_.CurrentBlockPOS) = False then
      begin
        Block_.State := DB_Item_WriteCurrentBlockPOSError;
        Result := False;
        exit;
      end;
    if umlFileWrite(IOHnd, DB_Position_Size, Block_.NextBlockPOS) = False then
      begin
        Block_.State := DB_Item_WriteNextBlockPOSError;
        Result := False;
        exit;
      end;
    if umlFileWrite(IOHnd, DB_Position_Size, Block_.PrevBlockPOS) = False then
      begin
        Block_.State := DB_Item_WritePrevBlockPOSError;
        Result := False;
        exit;
      end;
    if umlFileWrite(IOHnd, DB_Position_Size, Block_.DataPosition) = False then
      begin
        Block_.State := DB_Item_WriteDataBlockPOSError;
        Result := False;
        exit;
      end;
    if umlFileWrite(IOHnd, DB_DataSize_Size, Block_.Size) = False then
      begin
        Block_.State := DB_Item_WriteDataBuffSizeError;
        Result := False;
        exit;
      end;
    Block_.State := DB_Item_ok;
    Result := True;

    if IOHnd.Data <> nil then
      if Assigned(PObjectDataHandle(IOHnd.Data)^.OnWriteItemBlock) then
          PObjectDataHandle(IOHnd.Data)^.OnWriteItemBlock(fPos, Block_);
  finally
    if not Result then
      if IOHnd.Data <> nil then
        if Assigned(PObjectDataHandle(IOHnd.Data)^.OnError) then
            PObjectDataHandle(IOHnd.Data)^.OnError(TranslateReturnCode(Block_.State));
  end;
end;

function dbItem_OnlyReadItemBlockRec(const fPos: Int64; var IOHnd: TIOHnd; var Block_: TItemBlock): Boolean;
begin
  Result := False;
  Block_.State := DB_ExceptionError;
  try
    if IOHnd.Data <> nil then
      if Assigned(PObjectDataHandle(IOHnd.Data)^.OnReadItemBlock) then
        begin
          Result := False;
          PObjectDataHandle(IOHnd.Data)^.OnReadItemBlock(fPos, Block_, Result);
          if Result then
              exit;
        end;

    if umlFileSeek(IOHnd, fPos) = False then
      begin
        Block_.State := DB_Item_SetPosError;
        Result := False;
        exit;
      end;
    if umlFileRead(IOHnd, DB_ID_Size, Block_.ID) = False then
      begin
        Block_.State := DB_Item_ReadItemBlockIDFlagsError;
        Result := False;
        exit;
      end;
    if umlFileRead(IOHnd, DB_Position_Size, Block_.CurrentBlockPOS) = False then
      begin
        Block_.State := DB_Item_ReadCurrentBlockPOSError;
        Result := False;
        exit;
      end;
    if umlFileRead(IOHnd, DB_Position_Size, Block_.NextBlockPOS) = False then
      begin
        Block_.State := DB_Item_ReadNextBlockPOSError;
        Result := False;
        exit;
      end;
    if umlFileRead(IOHnd, DB_Position_Size, Block_.PrevBlockPOS) = False then
      begin
        Block_.State := DB_Item_ReadPrevBlockPOSError;
        Result := False;
        exit;
      end;
    if umlFileRead(IOHnd, DB_Position_Size, Block_.DataPosition) = False then
      begin
        Block_.State := DB_Item_ReadDataBlockPOSError;
        Result := False;
        exit;
      end;
    if umlFileRead(IOHnd, DB_DataSize_Size, Block_.Size) = False then
      begin
        Block_.State := DB_Item_ReadDataBuffSizeError;
        Result := False;
        exit;
      end;
    Block_.State := DB_Item_ok;
    Result := True;

    if IOHnd.Data <> nil then
      if Assigned(PObjectDataHandle(IOHnd.Data)^.OnWriteItemBlock) then
          PObjectDataHandle(IOHnd.Data)^.OnWriteItemBlock(fPos, Block_);
  finally
    if not Result then
      if IOHnd.Data <> nil then
        if Assigned(PObjectDataHandle(IOHnd.Data)^.OnError) then
            PObjectDataHandle(IOHnd.Data)^.OnError(TranslateReturnCode(Block_.State));
  end;
end;

function db_WriteRec(const fPos: Int64; var IOHnd: TIOHnd; var DB_: TObjectDataHandle): Boolean;
begin
  Result := False;
  DB_.State := DB_ExceptionError;
  try
    if (IOHnd.IsOnlyRead) or (not IOHnd.IsOpen) then
      begin
        DB_.State := DB_CheckIOError;
        Result := False;
        exit;
      end;
    if IOHnd.Data <> nil then
      if Assigned(PObjectDataHandle(IOHnd.Data)^.OnPrepareWriteTMDB) then
        begin
          Result := False;
          PObjectDataHandle(IOHnd.Data)^.OnPrepareWriteTMDB(fPos, @DB_, Result);
          if Result then
            begin
              DB_.State := DB_ok;
              if Assigned(PObjectDataHandle(IOHnd.Data)^.OnWriteTMDB) then
                  PObjectDataHandle(IOHnd.Data)^.OnWriteTMDB(fPos, @DB_);
              exit;
            end;
        end;

    if umlFileSeek(IOHnd, fPos) = False then
      begin
        DB_.State := DB_PositionSeekError;
        Result := False;
        exit;
      end;
    if umlFileWrite(IOHnd, DB_ReservedData_Size, DB_.ReservedData[0]) = False then
      begin
        DB_.State := DB_WriteReservedDataError;
        Result := False;
        exit;
      end;
    if umlFileWrite(IOHnd, DB_FixedStringL_Size, DB_.FixedStringL) = False then
      begin
        DB_.State := DB_WriteFixedStringLError;
        Result := False;
        exit;
      end;
    if umlFileWrite(IOHnd, DB_Version_Size, DB_.MajorVer) = False then
      begin
        DB_.State := DB_WriteMajorVersionError;
        Result := False;
        exit;
      end;
    if umlFileWrite(IOHnd, DB_Version_Size, DB_.MinorVer) = False then
      begin
        DB_.State := DB_WriteMinorVersionError;
        Result := False;
        exit;
      end;
    if umlFileWrite(IOHnd, DB_Time_Size, DB_.CreateTime) = False then
      begin
        DB_.State := DB_WriteCreateTimeError;
        Result := False;
        exit;
      end;
    if umlFileWrite(IOHnd, DB_Time_Size, DB_.ModificationTime) = False then
      begin
        DB_.State := DB_WriteLastEditTimeError;
        Result := False;
        exit;
      end;
    if umlFileWrite(IOHnd, DB_Counter_Size, DB_.RootHeaderCount) = False then
      begin
        DB_.State := DB_WriteHeaderCountError;
        Result := False;
        exit;
      end;
    if umlFileWrite(IOHnd, DB_Position_Size, DB_.DefaultFieldPOS) = False then
      begin
        DB_.State := DB_WriteDefaultPositionError;
        Result := False;
        exit;
      end;
    if umlFileWrite(IOHnd, DB_Position_Size, DB_.FirstHeaderPOS) = False then
      begin
        DB_.State := DB_WriteFirstPositionError;
        Result := False;
        exit;
      end;
    if umlFileWrite(IOHnd, DB_Position_Size, DB_.LastHeaderPOS) = False then
      begin
        DB_.State := DB_WriteLastPositionError;
        Result := False;
        exit;
      end;
    if umlFileWrite(IOHnd, DB_Position_Size, DB_.CurrentFieldPOS) = False then
      begin
        DB_.State := DB_WriteCurrentPositionError;
        Result := False;
        exit;
      end;
    if umlFileWrite(IOHnd, DB_Level_Size, DB_.CurrentFieldLevel) = False then
      begin
        DB_.State := DB_WriteCurrentLevelError;
        Result := False;
        exit;
      end;
    DB_.State := DB_ok;
    Result := True;

    if IOHnd.Data <> nil then
      if Assigned(PObjectDataHandle(IOHnd.Data)^.OnWriteTMDB) then
          PObjectDataHandle(IOHnd.Data)^.OnWriteTMDB(fPos, @DB_);
  finally
    if not Result then
      if IOHnd.Data <> nil then
        if Assigned(PObjectDataHandle(IOHnd.Data)^.OnError) then
            PObjectDataHandle(IOHnd.Data)^.OnError(TranslateReturnCode(DB_.State));
  end;
end;

function db_ReadRec(const fPos: Int64; var IOHnd: TIOHnd; var DB_: TObjectDataHandle): Boolean;
begin
  Result := False;
  DB_.State := DB_ExceptionError;
  try
    if IOHnd.Data <> nil then
      if Assigned(PObjectDataHandle(IOHnd.Data)^.OnReadTMDB) then
        begin
          Result := False;
          PObjectDataHandle(IOHnd.Data)^.OnReadTMDB(fPos, @DB_, Result);
          if Result then
            begin
              exit;
            end;
        end;

    if umlFileSeek(IOHnd, fPos) = False then
      begin
        DB_.State := DB_PositionSeekError;
        Result := False;
        exit;
      end;
    if umlFileRead(IOHnd, DB_ReservedData_Size, DB_.ReservedData[0]) = False then
      begin
        DB_.State := DB_ReadReservedDataError;
        Result := False;
        exit;
      end;
    if umlFileRead(IOHnd, DB_FixedStringL_Size, DB_.FixedStringL) = False then
      begin
        DB_.State := DB_ReadFixedStringLError;
        Result := False;
        exit;
      end;
    if umlFileRead(IOHnd, DB_Version_Size, DB_.MajorVer) = False then
      begin
        DB_.State := DB_ReadMajorVersionError;
        Result := False;
        exit;
      end;
    if umlFileRead(IOHnd, DB_Version_Size, DB_.MinorVer) = False then
      begin
        DB_.State := DB_ReadMinorVersionError;
        Result := False;
        exit;
      end;
    if umlFileRead(IOHnd, DB_Time_Size, DB_.CreateTime) = False then
      begin
        DB_.State := DB_ReadCreateTimeError;
        Result := False;
        exit;
      end;
    if umlFileRead(IOHnd, DB_Time_Size, DB_.ModificationTime) = False then
      begin
        DB_.State := DB_ReadLastEditTimeError;
        Result := False;
        exit;
      end;
    if umlFileRead(IOHnd, DB_Counter_Size, DB_.RootHeaderCount) = False then
      begin
        DB_.State := DB_ReadHeaderCountError;
        Result := False;
        exit;
      end;
    if umlFileRead(IOHnd, DB_Position_Size, DB_.DefaultFieldPOS) = False then
      begin
        DB_.State := DB_ReadDefaultPositionError;
        Result := False;
        exit;
      end;
    if umlFileRead(IOHnd, DB_Position_Size, DB_.FirstHeaderPOS) = False then
      begin
        DB_.State := DB_ReadFirstPositionError;
        Result := False;
        exit;
      end;
    if umlFileRead(IOHnd, DB_Position_Size, DB_.LastHeaderPOS) = False then
      begin
        DB_.State := DB_ReadLastPositionError;
        Result := False;
        exit;
      end;
    if umlFileRead(IOHnd, DB_Position_Size, DB_.CurrentFieldPOS) = False then
      begin
        DB_.State := DB_ReadCurrentPositionError;
        Result := False;
        exit;
      end;
    if umlFileRead(IOHnd, DB_Level_Size, DB_.CurrentFieldLevel) = False then
      begin
        DB_.State := DB_ReadCurrentLevelError;
        Result := False;
        exit;
      end;
    DB_.State := DB_ok;
    Result := True;
  finally
    if not Result then
      if IOHnd.Data <> nil then
        if Assigned(PObjectDataHandle(IOHnd.Data)^.OnError) then
            PObjectDataHandle(IOHnd.Data)^.OnError(TranslateReturnCode(DB_.State));
  end;
end;

function dbItem_OnlyWriteItemRec(const fPos: Int64; var IOHnd: TIOHnd; var Item_: TItem): Boolean;
begin
  Result := False;
  Item_.State := DB_ExceptionError;
  try
    if (IOHnd.IsOnlyRead) or (not IOHnd.IsOpen) then
      begin
        Item_.State := DB_CheckIOError;
        Result := False;
        exit;
      end;
    if IOHnd.Data <> nil then
      if Assigned(PObjectDataHandle(IOHnd.Data)^.OnPrepareOnlyWriteItemRec) then
        begin
          Result := False;
          PObjectDataHandle(IOHnd.Data)^.OnPrepareOnlyWriteItemRec(fPos, Item_, Result);
          if Result then
            begin
              Item_.State := DB_Item_ok;
              if Assigned(PObjectDataHandle(IOHnd.Data)^.OnOnlyWriteItemRec) then
                  PObjectDataHandle(IOHnd.Data)^.OnOnlyWriteItemRec(fPos, Item_);
              exit;
            end;
        end;

    if umlFileSeek(IOHnd, fPos) = False then
      begin
        Item_.State := DB_Item_SetPosError;
        Result := False;
        exit;
      end;
    if umlFileWriteFixedString(IOHnd, Item_.Description) = False then
      begin
        Item_.State := DB_Item_WriteRecDescriptionError;
        Result := False;
        exit;
      end;
    if umlFileWrite(IOHnd, DB_ID_Size, Item_.ExtID) = False then
      begin
        Item_.State := DB_Item_WriteRecExterIDError;
        Result := False;
        exit;
      end;
    if umlFileWrite(IOHnd, DB_Position_Size, Item_.FirstBlockPOS) = False then
      begin
        Item_.State := DB_Item_WriteFirstBlockPOSError;
        Result := False;
        exit;
      end;
    if umlFileWrite(IOHnd, DB_Position_Size, Item_.LastBlockPOS) = False then
      begin
        Item_.State := DB_Item_WriteLastBlockPOSError;
        Result := False;
        exit;
      end;
    if umlFileWrite(IOHnd, DB_DataSize_Size, Item_.Size) = False then
      begin
        Item_.State := DB_Item_WriteRecBuffSizeError;
        Result := False;
        exit;
      end;
    if umlFileWrite(IOHnd, DB_Counter_Size, Item_.BlockCount) = False then
      begin
        Item_.State := DB_Item_WriteBlockCountError;
        Result := False;
        exit;
      end;
    Item_.State := DB_Item_ok;
    Result := True;

    if IOHnd.Data <> nil then
      if Assigned(PObjectDataHandle(IOHnd.Data)^.OnOnlyWriteItemRec) then
          PObjectDataHandle(IOHnd.Data)^.OnOnlyWriteItemRec(fPos, Item_);
  finally
    if not Result then
      if IOHnd.Data <> nil then
        if Assigned(PObjectDataHandle(IOHnd.Data)^.OnError) then
            PObjectDataHandle(IOHnd.Data)^.OnError(TranslateReturnCode(Item_.State));
  end;
end;

function dbItem_OnlyReadItemRec(const fPos: Int64; var IOHnd: TIOHnd; var Item_: TItem): Boolean;
begin
  Result := False;
  Item_.State := DB_ExceptionError;
  try
    if IOHnd.Data <> nil then
      if Assigned(PObjectDataHandle(IOHnd.Data)^.OnOnlyReadItemRec) then
        begin
          Result := False;
          PObjectDataHandle(IOHnd.Data)^.OnOnlyReadItemRec(fPos, Item_, Result);
          if Result then
              exit;
        end;

    if umlFileSeek(IOHnd, fPos) = False then
      begin
        Item_.State := DB_Item_SetPosError;
        Result := False;
        exit;
      end;
    if umlFileReadFixedString(IOHnd, Item_.Description) = False then
      begin
        Item_.State := DB_Item_ReadRecDescriptionError;
        Result := False;
        exit;
      end;
    if umlFileRead(IOHnd, DB_ID_Size, Item_.ExtID) = False then
      begin
        Item_.State := DB_Item_ReadRecExterIDError;
        Result := False;
        exit;
      end;
    if umlFileRead(IOHnd, DB_Position_Size, Item_.FirstBlockPOS) = False then
      begin
        Item_.State := DB_Item_ReadFirstBlockPOSError;
        Result := False;
        exit;
      end;
    if umlFileRead(IOHnd, DB_Position_Size, Item_.LastBlockPOS) = False then
      begin
        Item_.State := DB_Item_ReadLastBlockPOSError;
        Result := False;
        exit;
      end;
    if umlFileRead(IOHnd, DB_DataSize_Size, Item_.Size) = False then
      begin
        Item_.State := DB_Item_ReadRecBuffSizeError;
        Result := False;
        exit;
      end;
    if umlFileRead(IOHnd, DB_Counter_Size, Item_.BlockCount) = False then
      begin
        Item_.State := DB_Item_ReadBlockCountError;
        Result := False;
        exit;
      end;
    Item_.State := DB_Item_ok;
    Result := True;

    if IOHnd.Data <> nil then
      if Assigned(PObjectDataHandle(IOHnd.Data)^.OnOnlyWriteItemRec) then
          PObjectDataHandle(IOHnd.Data)^.OnOnlyWriteItemRec(fPos, Item_);
  finally
    if not Result then
      if IOHnd.Data <> nil then
        if Assigned(PObjectDataHandle(IOHnd.Data)^.OnError) then
            PObjectDataHandle(IOHnd.Data)^.OnError(TranslateReturnCode(Item_.State));
  end;
end;

function dbField_OnlyWriteFieldRec(const fPos: Int64; var IOHnd: TIOHnd; var Field_: TField): Boolean;
begin
  Result := False;
  Field_.State := DB_ExceptionError;
  try
    if (IOHnd.IsOnlyRead) or (not IOHnd.IsOpen) then
      begin
        Field_.State := DB_CheckIOError;
        Result := False;
        exit;
      end;
    if IOHnd.Data <> nil then
      if Assigned(PObjectDataHandle(IOHnd.Data)^.OnPrepareOnlyWriteFieldRec) then
        begin
          Result := False;
          PObjectDataHandle(IOHnd.Data)^.OnPrepareOnlyWriteFieldRec(fPos, Field_, Result);
          if Result then
            begin
              Field_.State := DB_Field_ok;
              if Assigned(PObjectDataHandle(IOHnd.Data)^.OnOnlyWriteFieldRec) then
                  PObjectDataHandle(IOHnd.Data)^.OnOnlyWriteFieldRec(fPos, Field_);
              exit;
            end;
        end;

    if umlFileSeek(IOHnd, fPos) = False then
      begin
        Field_.State := DB_Field_SetPosError;
        Result := False;
        exit;
      end;
    if umlFileWrite(IOHnd, DB_Position_Size, Field_.UpFieldPOS) = False then
      begin
        Field_.State := DB_Field_WriteHeaderFieldPosError;
        Result := False;
        exit;
      end;
    if umlFileWriteFixedString(IOHnd, Field_.Description) = False then
      begin
        Field_.State := DB_Field_WriteDescriptionError;
        Result := False;
        exit;
      end;
    if umlFileWrite(IOHnd, DB_Counter_Size, Field_.HeaderCount) = False then
      begin
        Field_.State := DB_Field_WriteCountError;
        Result := False;
        exit;
      end;
    if umlFileWrite(IOHnd, DB_Position_Size, Field_.FirstHeaderPOS) = False then
      begin
        Field_.State := DB_Field_WriteFirstPosError;
        Result := False;
        exit;
      end;
    if umlFileWrite(IOHnd, DB_Position_Size, Field_.LastHeaderPOS) = False then
      begin
        Field_.State := DB_Field_WriteLastPosError;
        Result := False;
        exit;
      end;
    Field_.State := DB_Field_ok;
    Result := True;

    if IOHnd.Data <> nil then
      if Assigned(PObjectDataHandle(IOHnd.Data)^.OnOnlyWriteFieldRec) then
          PObjectDataHandle(IOHnd.Data)^.OnOnlyWriteFieldRec(fPos, Field_);
  finally
    if not Result then
      if IOHnd.Data <> nil then
        if Assigned(PObjectDataHandle(IOHnd.Data)^.OnError) then
            PObjectDataHandle(IOHnd.Data)^.OnError(TranslateReturnCode(Field_.State));
  end;
end;

function dbField_OnlyReadFieldRec(const fPos: Int64; var IOHnd: TIOHnd; var Field_: TField): Boolean;
begin
  Result := False;
  Field_.State := DB_ExceptionError;
  try
    if IOHnd.Data <> nil then
      if Assigned(PObjectDataHandle(IOHnd.Data)^.OnOnlyReadFieldRec) then
        begin
          Result := False;
          PObjectDataHandle(IOHnd.Data)^.OnOnlyReadFieldRec(fPos, Field_, Result);
          if Result then
              exit;
        end;

    if umlFileSeek(IOHnd, fPos) = False then
      begin
        Field_.State := DB_Field_SetPosError;
        Result := False;
        exit;
      end;
    if umlFileRead(IOHnd, DB_Position_Size, Field_.UpFieldPOS) = False then
      begin
        Field_.State := DB_Field_ReadHeaderFieldPosError;
        Result := False;
        exit;
      end;
    if umlFileReadFixedString(IOHnd, Field_.Description) = False then
      begin
        Field_.State := DB_Field_ReadDescriptionError;
        Result := False;
        exit;
      end;
    if umlFileRead(IOHnd, DB_Counter_Size, Field_.HeaderCount) = False then
      begin
        Field_.State := DB_Field_ReadCountError;
        Result := False;
        exit;
      end;
    if umlFileRead(IOHnd, DB_Position_Size, Field_.FirstHeaderPOS) = False then
      begin
        Field_.State := DB_Field_ReadFirstPosError;
        Result := False;
        exit;
      end;
    if umlFileRead(IOHnd, DB_Position_Size, Field_.LastHeaderPOS) = False then
      begin
        Field_.State := DB_Field_ReadLastPosError;
        Result := False;
        exit;
      end;
    Field_.State := DB_Field_ok;
    Result := True;

    if IOHnd.Data <> nil then
      if Assigned(PObjectDataHandle(IOHnd.Data)^.OnOnlyWriteFieldRec) then
          PObjectDataHandle(IOHnd.Data)^.OnOnlyWriteFieldRec(fPos, Field_);
  finally
    if not Result then
      if IOHnd.Data <> nil then
        if Assigned(PObjectDataHandle(IOHnd.Data)^.OnError) then
            PObjectDataHandle(IOHnd.Data)^.OnError(TranslateReturnCode(Field_.State));
  end;
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

function dbHeader_FindNext(const Name: U_String; const FirstHeaderPOS, LastHeaderPOS: Int64; var IOHnd: TIOHnd; var Header_: THeader): Boolean;
begin
  if dbHeader_ReadRec(FirstHeaderPOS, IOHnd, Header_) = False then
    begin
      Result := False;
      exit;
    end;
  if dbMultipleMatch(Name, Header_.Name) then
    begin
      Result := True;
      exit;
    end;
  if (Header_.PositionID = DB_Header_Last) or (Header_.PositionID = DB_Header_1) then
    begin
      Header_.State := DB_Header_NotFindHeader;
      Result := False;
      exit;
    end;
  while dbHeader_ReadRec(Header_.NextHeader, IOHnd, Header_) do
    begin
      if dbMultipleMatch(Name, Header_.Name) then
        begin
          Result := True;
          exit;
        end;
      if Header_.PositionID = DB_Header_Last then
        begin
          Header_.State := DB_Header_NotFindHeader;
          Result := False;
          exit;
        end;
    end;
  Header_.State := DB_Header_ok;
  Result := False;
end;

function dbHeader_FindPrev(const Name: U_String; const LastHeaderPOS, FirstHeaderPOS: Int64; var IOHnd: TIOHnd; var Header_: THeader): Boolean;
begin
  if dbHeader_ReadRec(LastHeaderPOS, IOHnd, Header_) = False then
    begin
      Result := False;
      exit;
    end;
  if dbMultipleMatch(Name, Header_.Name) then
    begin
      Result := True;
      exit;
    end;
  if (Header_.PositionID = DB_Header_First) or (Header_.PositionID = DB_Header_1) then
    begin
      Header_.State := DB_Header_NotFindHeader;
      Result := False;
      exit;
    end;
  while dbHeader_ReadRec(Header_.PrevHeader, IOHnd, Header_) do
    begin
      if dbMultipleMatch(Name, Header_.Name) then
        begin
          Result := True;
          exit;
        end;
      if Header_.PositionID = DB_Header_First then
        begin
          Header_.State := DB_Header_NotFindHeader;
          Result := False;
          exit;
        end;
    end;
  Header_.State := DB_Header_ok;
  Result := False;
end;

function dbItem_BlockCreate(var IOHnd: TIOHnd; var Item_: TItem): Boolean;
var
  FirstItemBlock, LastItemBlock: TItemBlock;
begin
  case Item_.BlockCount of
    0:
      begin
        LastItemBlock.ID := DB_Item_1;
        LastItemBlock.CurrentBlockPOS := umlFileGetSize(IOHnd);
        LastItemBlock.NextBlockPOS := LastItemBlock.CurrentBlockPOS;
        LastItemBlock.PrevBlockPOS := LastItemBlock.CurrentBlockPOS;
        LastItemBlock.DataPosition := LastItemBlock.CurrentBlockPOS + Get_DB_BlockL(IOHnd);
        LastItemBlock.Size := 0;
        if dbItem_OnlyWriteItemBlockRec(LastItemBlock.CurrentBlockPOS, IOHnd, LastItemBlock) = False then
          begin
            Item_.State := LastItemBlock.State;
            Result := False;
            exit;
          end;
        Item_.BlockCount := 1;
        Item_.FirstBlockPOS := LastItemBlock.CurrentBlockPOS;
        Item_.LastBlockPOS := LastItemBlock.CurrentBlockPOS;
        if dbItem_OnlyWriteItemRec(Item_.RHeader.DataPosition, IOHnd, Item_) = False then
          begin
            Result := False;
            exit;
          end;
      end;
    1:
      begin
        if dbItem_OnlyReadItemBlockRec(Item_.FirstBlockPOS, IOHnd, FirstItemBlock) = False then
          begin
            Item_.State := FirstItemBlock.State;
            Result := False;
            exit;
          end;
        LastItemBlock.ID := DB_Item_Last;
        LastItemBlock.CurrentBlockPOS := umlFileGetSize(IOHnd);
        LastItemBlock.NextBlockPOS := FirstItemBlock.CurrentBlockPOS;
        LastItemBlock.PrevBlockPOS := FirstItemBlock.CurrentBlockPOS;
        LastItemBlock.DataPosition := LastItemBlock.CurrentBlockPOS + Get_DB_BlockL(IOHnd);
        LastItemBlock.Size := 0;
        if dbItem_OnlyWriteItemBlockRec(LastItemBlock.CurrentBlockPOS, IOHnd, LastItemBlock) = False then
          begin
            Item_.State := LastItemBlock.State;
            Result := False;
            exit;
          end;
        FirstItemBlock.ID := DB_Item_First;
        FirstItemBlock.NextBlockPOS := LastItemBlock.CurrentBlockPOS;
        FirstItemBlock.PrevBlockPOS := LastItemBlock.CurrentBlockPOS;
        if dbItem_OnlyWriteItemBlockRec(Item_.FirstBlockPOS, IOHnd, FirstItemBlock) = False then
          begin
            Item_.State := FirstItemBlock.State;
            Result := False;
            exit;
          end;
        Item_.BlockCount := Item_.BlockCount + 1;
        Item_.LastBlockPOS := LastItemBlock.CurrentBlockPOS;
        if dbItem_OnlyWriteItemRec(Item_.RHeader.DataPosition, IOHnd, Item_) = False then
          begin
            Result := False;
            exit;
          end;
      end;
    else
      begin
        if dbItem_OnlyReadItemBlockRec(Item_.FirstBlockPOS, IOHnd, FirstItemBlock) = False then
          begin
            Item_.State := FirstItemBlock.State;
            Result := False;
            exit;
          end;
        FirstItemBlock.PrevBlockPOS := umlFileGetSize(IOHnd);
        if dbItem_OnlyWriteItemBlockRec(Item_.FirstBlockPOS, IOHnd, FirstItemBlock) = False then
          begin
            Item_.State := FirstItemBlock.State;
            Result := False;
            exit;
          end;
        if dbItem_OnlyReadItemBlockRec(Item_.LastBlockPOS, IOHnd, LastItemBlock) = False then
          begin
            Item_.State := LastItemBlock.State;
            Result := False;
            exit;
          end;
        LastItemBlock.ID := DB_Item_Medium;
        LastItemBlock.NextBlockPOS := FirstItemBlock.PrevBlockPOS;
        if dbItem_OnlyWriteItemBlockRec(Item_.LastBlockPOS, IOHnd, LastItemBlock) = False then
          begin
            Item_.State := LastItemBlock.State;
            Result := False;
            exit;
          end;
        LastItemBlock.ID := DB_Item_Last;
        LastItemBlock.CurrentBlockPOS := FirstItemBlock.PrevBlockPOS;
        LastItemBlock.NextBlockPOS := Item_.FirstBlockPOS;
        LastItemBlock.PrevBlockPOS := Item_.LastBlockPOS;
        LastItemBlock.DataPosition := LastItemBlock.CurrentBlockPOS + Get_DB_BlockL(IOHnd);
        LastItemBlock.Size := 0;
        if dbItem_OnlyWriteItemBlockRec(LastItemBlock.CurrentBlockPOS, IOHnd, LastItemBlock) = False then
          begin
            Item_.State := LastItemBlock.State;
            Result := False;
            exit;
          end;
        Item_.BlockCount := Item_.BlockCount + 1;
        Item_.LastBlockPOS := LastItemBlock.CurrentBlockPOS;
        if dbItem_OnlyWriteItemRec(Item_.RHeader.DataPosition, IOHnd, Item_) = False then
          begin
            Result := False;
            exit;
          end;
      end;
  end;
  Item_.CurrentItemBlock := LastItemBlock;
  Item_.CurrentBlockSeekPOS := 0;
  Item_.CurrentFileSeekPOS := Item_.CurrentItemBlock.DataPosition;
  Item_.DataModification := True;
  Item_.State := DB_Item_ok;
  Result := True;
end;

function dbItem_BlockInit(var IOHnd: TIOHnd; var Item_: TItem): Boolean;
begin
  if Item_.BlockCount = 0 then
    begin
      Item_.State := DB_Item_ok;
      Result := True;
      exit;
    end;
  if dbItem_OnlyReadItemBlockRec(Item_.FirstBlockPOS, IOHnd, Item_.CurrentItemBlock) = False then
    begin
      Item_.State := Item_.CurrentItemBlock.State;
      Result := False;
      exit;
    end;
  Item_.CurrentBlockSeekPOS := 0;
  Item_.CurrentFileSeekPOS := Item_.CurrentItemBlock.DataPosition;
  Item_.State := DB_Item_ok;
  Result := True;
end;

function dbItem_BlockReadData(var IOHnd: TIOHnd; var Item_: TItem; var Buffers; const _Size: Int64): Boolean;
label
  Rep_Label;
var
  BuffPointer: Pointer;
  BuffInt: nativeUInt;
  DeformitySize, BlockPOS: Int64;
  ItemBlock: TItemBlock;
  Size: Int64;
begin
  if (_Size <= Item_.Size) then
      Size := _Size
  else
      Size := Item_.Size;

  if Size = 0 then
    begin
      Item_.State := DB_Item_ok;
      Result := True;
      exit;
    end;

  if (Item_.BlockCount = 0) then
    begin
      Item_.State := DB_Item_BlockOverrate;
      Result := False;
      exit;
    end;

  if Item_.CurrentBlockSeekPOS > Item_.CurrentItemBlock.Size then
    begin
      Item_.State := DB_Item_BlockPositionError;
      Result := False;
      exit;
    end;
  ItemBlock := Item_.CurrentItemBlock;
  BlockPOS := Item_.CurrentBlockSeekPOS;
  BuffInt := nativeUInt(@Buffers);
  BuffPointer := Pointer(BuffInt);
  DeformitySize := Size;
Rep_Label:
  if ItemBlock.Size - BlockPOS = 0 then
    begin
      case ItemBlock.ID of
        DB_Item_Last, DB_Item_1:
          begin
            Item_.State := DB_Item_BlockOverrate;
            Result := False;
            exit;
          end;
      end;
      if dbItem_OnlyReadItemBlockRec(ItemBlock.NextBlockPOS, IOHnd, ItemBlock) = False then
        begin
          Item_.State := ItemBlock.State;
          Result := False;
          exit;
        end;
      if BlockPOS > 0 then
          BlockPOS := 0;
      while (ItemBlock.Size - BlockPOS) = 0 do
        begin
          case ItemBlock.ID of
            DB_Item_Last:
              begin
                Item_.State := DB_Item_BlockOverrate;
                Result := False;
                exit;
              end;
          end;
          if dbItem_OnlyReadItemBlockRec(ItemBlock.NextBlockPOS, IOHnd, ItemBlock) = False then
            begin
              Item_.State := ItemBlock.State;
              Result := False;
              exit;
            end;
        end;
    end;

  if umlFileSeek(IOHnd, ItemBlock.DataPosition + BlockPOS) = False then
    begin
      Item_.State := DB_Item_SetPosError;
      Result := False;
      exit;
    end;

  if DeformitySize <= ItemBlock.Size - BlockPOS then
    begin
      if umlFileRead(IOHnd, DeformitySize, BuffPointer^) = False then
        begin
          Item_.State := DB_Item_BlockReadError;
          Result := False;
          exit;
        end;
      Item_.CurrentBlockSeekPOS := BlockPOS + DeformitySize;
      Item_.CurrentFileSeekPOS := ItemBlock.DataPosition + (BlockPOS + DeformitySize);
      Item_.CurrentItemBlock := ItemBlock;
      Item_.State := DB_Item_ok;
      Result := True;
      exit;
    end;

  if umlFileRead(IOHnd, ItemBlock.Size - BlockPOS, BuffPointer^) = False then
    begin
      Item_.State := DB_Item_BlockReadError;
      Result := False;
      exit;
    end;
  case ItemBlock.ID of
    DB_Item_Last, DB_Item_1:
      begin
        Item_.State := DB_Item_BlockOverrate;
        Result := False;
        exit;
      end;
  end;
  BuffInt := BuffInt + (ItemBlock.Size - BlockPOS);
  BuffPointer := Pointer(BuffInt);
  DeformitySize := DeformitySize - (ItemBlock.Size - BlockPOS);
  if dbItem_OnlyReadItemBlockRec(ItemBlock.NextBlockPOS, IOHnd, ItemBlock) = False then
    begin
      Item_.State := ItemBlock.State;
      Result := False;
      exit;
    end;

  if BlockPOS = 0 then
      goto Rep_Label;
  BlockPOS := 0;
  goto Rep_Label;
end;

function dbItem_BlockAppendWriteData(var IOHnd: TIOHnd; var Item_: TItem; var Buffers; const Size: Int64): Boolean;
begin
  if (Item_.BlockCount > 0) and ((Item_.CurrentItemBlock.DataPosition + Item_.CurrentItemBlock.Size) = umlFileGetSize(IOHnd)) then
    begin
      if umlFileSeek(IOHnd, umlFileGetSize(IOHnd)) = False then
        begin
          Item_.State := DB_Item_SetPosError;
          Result := False;
          exit;
        end;
      if umlFileWrite(IOHnd, Size, Buffers) = False then
        begin
          Item_.State := DB_Item_BlockWriteError;
          Result := False;
          exit;
        end;
      Item_.CurrentItemBlock.Size := Item_.CurrentItemBlock.Size + Size;
      if dbItem_OnlyWriteItemBlockRec(Item_.CurrentItemBlock.CurrentBlockPOS, IOHnd, Item_.CurrentItemBlock) = False then
        begin
          Item_.State := Item_.CurrentItemBlock.State;
          Result := False;
          exit;
        end;
      Item_.Size := Item_.Size + Size;
      if dbItem_OnlyWriteItemRec(Item_.RHeader.DataPosition, IOHnd, Item_) = False then
        begin
          Result := False;
          exit;
        end;
      Item_.CurrentBlockSeekPOS := Item_.CurrentItemBlock.Size;
      Item_.CurrentFileSeekPOS := Item_.CurrentItemBlock.DataPosition + Item_.CurrentItemBlock.Size;
      Item_.DataModification := True;
      Item_.State := DB_Item_ok;
      Result := True;
      exit;
    end;

  if dbItem_BlockCreate(IOHnd, Item_) = False then
    begin
      Result := False;
      exit;
    end;

  if umlFileSeek(IOHnd, Item_.CurrentItemBlock.DataPosition) = False then
    begin
      Item_.State := DB_Item_SetPosError;
      Result := False;
      exit;
    end;

  if umlFileWrite(IOHnd, Size, Buffers) = False then
    begin
      Item_.State := DB_Item_BlockWriteError;
      Result := False;
      exit;
    end;
  Item_.CurrentItemBlock.Size := Size;
  if dbItem_OnlyWriteItemBlockRec(Item_.CurrentItemBlock.CurrentBlockPOS, IOHnd, Item_.CurrentItemBlock) = False then
    begin
      Item_.State := Item_.CurrentItemBlock.State;
      Result := False;
      exit;
    end;
  Item_.Size := Item_.Size + Size;
  if dbItem_OnlyWriteItemRec(Item_.RHeader.DataPosition, IOHnd, Item_) = False then
    begin
      Result := False;
      exit;
    end;
  Item_.CurrentBlockSeekPOS := Item_.CurrentItemBlock.Size;
  Item_.CurrentFileSeekPOS := Item_.CurrentItemBlock.DataPosition + Item_.CurrentItemBlock.Size;
  Item_.DataModification := True;
  Item_.State := DB_Item_ok;
  Result := True;
end;

function dbItem_BlockWriteData(var IOHnd: TIOHnd; var Item_: TItem; var Buffers; const Size: Int64): Boolean;
label
  Rep_Label;
var
  BuffPointer: Pointer;
  BuffInt: nativeUInt;
  DeformitySize, BlockPOS: Int64;
  ItemBlock: TItemBlock;
begin
  if (Item_.Size = 0) or (Item_.BlockCount = 0) then
    begin
      Result := dbItem_BlockAppendWriteData(IOHnd, Item_, Buffers, Size);
      exit;
    end;
  case Item_.CurrentItemBlock.ID of
    DB_Item_Last, DB_Item_1:
      begin
        if Item_.CurrentBlockSeekPOS = Item_.CurrentItemBlock.Size then
          begin
            Result := dbItem_BlockAppendWriteData(IOHnd, Item_, Buffers, Size);
            exit;
          end;
      end;
  end;

  if Item_.CurrentBlockSeekPOS > Item_.CurrentItemBlock.Size then
    begin
      Item_.State := DB_Item_BlockPositionError;
      Result := False;
      exit;
    end;
  ItemBlock := Item_.CurrentItemBlock;
  BlockPOS := Item_.CurrentBlockSeekPOS;
  BuffInt := nativeUInt(@Buffers);
  BuffPointer := Pointer(BuffInt);
  DeformitySize := Size;
Rep_Label:
  if ItemBlock.Size - BlockPOS = 0 then
    begin
      case ItemBlock.ID of
        DB_Item_Last, DB_Item_1:
          begin
            Result := dbItem_BlockAppendWriteData(IOHnd, Item_, BuffPointer^, DeformitySize);
            exit;
          end;
      end;
      if dbItem_OnlyReadItemBlockRec(ItemBlock.NextBlockPOS, IOHnd, ItemBlock) = False then
        begin
          Item_.State := ItemBlock.State;
          Result := False;
          exit;
        end;
      if BlockPOS > 0 then
          BlockPOS := 0;
      while (ItemBlock.Size - BlockPOS) = 0 do
        begin
          case ItemBlock.ID of
            DB_Item_Last:
              begin
                Result := dbItem_BlockAppendWriteData(IOHnd, Item_, BuffPointer^, DeformitySize);
                exit;
              end;
          end;
          if dbItem_OnlyReadItemBlockRec(ItemBlock.NextBlockPOS, IOHnd, ItemBlock) = False then
            begin
              Item_.State := ItemBlock.State;
              Result := False;
              exit;
            end;
        end;
    end;

  if umlFileSeek(IOHnd, ItemBlock.DataPosition + BlockPOS) = False then
    begin
      Item_.State := DB_Item_SetPosError;
      Result := False;
      exit;
    end;

  if DeformitySize <= ItemBlock.Size - BlockPOS then
    begin
      if umlFileWrite(IOHnd, DeformitySize, BuffPointer^) = False then
        begin
          Item_.State := DB_Item_BlockWriteError;
          Result := False;
          exit;
        end;
      Item_.CurrentBlockSeekPOS := BlockPOS + DeformitySize;
      Item_.CurrentFileSeekPOS := ItemBlock.DataPosition + (BlockPOS + DeformitySize);
      Item_.CurrentItemBlock := ItemBlock;
      Item_.DataModification := True;
      Item_.State := DB_Item_ok;
      Result := True;
      exit;
    end;

  if umlFileWrite(IOHnd, ItemBlock.Size - BlockPOS, BuffPointer^) = False then
    begin
      Item_.State := DB_Item_BlockWriteError;
      Result := False;
      exit;
    end;
  BuffInt := BuffInt + (ItemBlock.Size - BlockPOS);
  BuffPointer := Pointer(BuffInt);
  DeformitySize := DeformitySize - (ItemBlock.Size - BlockPOS);
  case ItemBlock.ID of
    DB_Item_Last, DB_Item_1:
      begin
        Result := dbItem_BlockAppendWriteData(IOHnd, Item_, BuffPointer^, DeformitySize);
        exit;
      end;
  end;
  if dbItem_OnlyReadItemBlockRec(ItemBlock.NextBlockPOS, IOHnd, ItemBlock) = False then
    begin
      Item_.State := ItemBlock.State;
      Result := False;
      exit;
    end;

  if BlockPOS = 0 then
      goto Rep_Label;
  BlockPOS := 0;
  goto Rep_Label;
end;

function dbItem_BlockSeekPOS(var IOHnd: TIOHnd; var Item_: TItem; const Position: Int64): Boolean;
var
  ItemBlock: TItemBlock;
  DeformityInt: Int64;
begin
  if (Position = 0) and (Item_.Size = 0) then
    begin
      Item_.State := DB_Item_ok;
      Result := True;
      exit;
    end;

  if (Position > Item_.Size) or (Item_.BlockCount = 0) then
    begin
      Item_.State := DB_Item_BlockOverrate;
      Result := False;
      exit;
    end;
  DeformityInt := Position;
  if dbItem_OnlyReadItemBlockRec(Item_.FirstBlockPOS, IOHnd, ItemBlock) = False then
    begin
      Item_.State := ItemBlock.State;
      Result := False;
      exit;
    end;

  if DeformityInt <= ItemBlock.Size then
    begin
      Item_.CurrentBlockSeekPOS := ItemBlock.Size - (ItemBlock.Size - DeformityInt);
      Item_.CurrentFileSeekPOS := ItemBlock.DataPosition + Item_.CurrentBlockSeekPOS;
      Item_.CurrentItemBlock := ItemBlock;
      Item_.State := DB_Item_ok;
      Result := True;
      exit;
    end;
  case ItemBlock.ID of
    DB_Item_Last, DB_Item_1:
      begin
        Item_.State := DB_Item_BlockOverrate;
        Result := False;
        exit;
      end;
  end;
  DeformityInt := DeformityInt - ItemBlock.Size;
  while dbItem_OnlyReadItemBlockRec(ItemBlock.NextBlockPOS, IOHnd, ItemBlock) do
    begin
      if DeformityInt <= ItemBlock.Size then
        begin
          Item_.CurrentBlockSeekPOS := ItemBlock.Size - (ItemBlock.Size - DeformityInt);
          Item_.CurrentFileSeekPOS := ItemBlock.DataPosition + Item_.CurrentBlockSeekPOS;
          Item_.CurrentItemBlock := ItemBlock;
          Item_.State := DB_Item_ok;
          Result := True;
          exit;
        end;
      case ItemBlock.ID of
        DB_Item_Last:
          begin
            Item_.State := DB_Item_BlockOverrate;
            Result := False;
            exit;
          end;
      end;
      DeformityInt := DeformityInt - ItemBlock.Size;
    end;
  Item_.State := ItemBlock.State;
  Result := False;
end;

function dbItem_BlockGetPOS(var IOHnd: TIOHnd; var Item_: TItem): Int64;
var
  ItemBlock: TItemBlock;
begin
  if (Item_.Size = 0) or (Item_.BlockCount = 0) then
    begin
      Item_.State := DB_Item_BlockOverrate;
      Result := 0;
      exit;
    end;

  if Item_.CurrentBlockSeekPOS > Item_.CurrentItemBlock.Size then
    begin
      Item_.State := DB_Item_BlockPositionError;
      Result := 0;
      exit;
    end;
  Result := Item_.CurrentBlockSeekPOS;
  case Item_.CurrentItemBlock.ID of
    DB_Item_First, DB_Item_1:
      begin
        Item_.State := DB_Item_ok;
        exit;
      end;
  end;
  if dbItem_OnlyReadItemBlockRec(Item_.CurrentItemBlock.PrevBlockPOS, IOHnd, ItemBlock) = False then
    begin
      Item_.State := ItemBlock.State;
      Result := 0;
      exit;
    end;
  Result := Result + ItemBlock.Size;
  case ItemBlock.ID of
    DB_Item_First, DB_Item_1:
      begin
        Item_.State := DB_Item_ok;
        exit;
      end;
  end;
  while dbItem_OnlyReadItemBlockRec(ItemBlock.PrevBlockPOS, IOHnd, ItemBlock) do
    begin
      Result := Result + ItemBlock.Size;
      if ItemBlock.ID = DB_Item_First then
        begin
          Item_.State := DB_Item_ok;
          exit;
        end;
    end;
  Item_.State := ItemBlock.State;
  Result := 0;
end;

function dbItem_BlockSeekStartPOS(var IOHnd: TIOHnd; var Item_: TItem): Boolean;
begin
  if Item_.BlockCount = 0 then
    begin
      Item_.State := DB_Item_BlockOverrate;
      Result := False;
      exit;
    end;
  if dbItem_OnlyReadItemBlockRec(Item_.FirstBlockPOS, IOHnd, Item_.CurrentItemBlock) = False then
    begin
      Item_.State := Item_.CurrentItemBlock.State;
      Result := False;
      exit;
    end;
  Item_.CurrentBlockSeekPOS := 0;
  Item_.CurrentFileSeekPOS := Item_.CurrentItemBlock.DataPosition;
  Item_.State := DB_Item_ok;
  Result := True;
end;

function dbItem_BlockSeekLastPOS(var IOHnd: TIOHnd; var Item_: TItem): Boolean;
begin
  if Item_.BlockCount = 0 then
    begin
      Item_.State := DB_Item_BlockOverrate;
      Result := False;
      exit;
    end;
  if dbItem_OnlyReadItemBlockRec(Item_.LastBlockPOS, IOHnd, Item_.CurrentItemBlock) = False then
    begin
      Item_.State := Item_.CurrentItemBlock.State;
      Result := False;
      exit;
    end;
  Item_.CurrentBlockSeekPOS := Item_.CurrentItemBlock.Size;
  Item_.CurrentFileSeekPOS := Item_.CurrentItemBlock.DataPosition + Item_.CurrentItemBlock.Size;
  Item_.State := DB_Item_ok;
  Result := True;
end;

function dbField_GetPOSField(const fPos: Int64; var IOHnd: TIOHnd): TField;
begin
  if not dbField_ReadRec(fPos, IOHnd, Result) then
      Init_TField(Result);
end;

function dbField_GetFirstHeader(const fPos: Int64; var IOHnd: TIOHnd): THeader;
var
  f: TField;
begin
  Init_THeader(Result);
  if dbField_ReadRec(fPos, IOHnd, f) then
      dbHeader_ReadRec(f.FirstHeaderPOS, IOHnd, Result);
end;

function dbField_GetLastHeader(const fPos: Int64; var IOHnd: TIOHnd): THeader;
var
  f: TField;
begin
  Init_THeader(Result);
  if dbField_ReadRec(fPos, IOHnd, f) then
      dbHeader_ReadRec(f.LastHeaderPOS, IOHnd, Result);
end;

function dbField_OnlyFindFirstName(const Name: U_String; const fPos: Int64; var IOHnd: TIOHnd; var FieldS_: TFieldSearch): Boolean;
var
  f: TField;
begin
  FieldS_.InitFlags := False;
  if dbField_ReadRec(fPos, IOHnd, f) = False then
    begin
      FieldS_.State := f.State;
      Result := False;
      exit;
    end;
  if f.HeaderCount = 0 then
    begin
      FieldS_.State := DB_Header_NotFindHeader;
      Result := False;
      exit;
    end;
  if dbHeader_FindNext(Name, f.FirstHeaderPOS, f.LastHeaderPOS, IOHnd, FieldS_.RHeader) = False then
    begin
      FieldS_.State := FieldS_.RHeader.State;
      Result := False;
      exit;
    end;
  FieldS_.InitFlags := True;
  FieldS_.PositionID := FieldS_.RHeader.PositionID;
  FieldS_.OverPOS := f.LastHeaderPOS;
  FieldS_.StartPos := FieldS_.RHeader.NextHeader;
  FieldS_.Name := Name;
  FieldS_.ID := FieldS_.RHeader.ID;
  FieldS_.State := FieldS_.RHeader.State;
  Result := True;
end;

function dbField_OnlyFindNextName(var IOHnd: TIOHnd; var FieldS_: TFieldSearch): Boolean;
begin
  if FieldS_.InitFlags = False then
    begin
      FieldS_.State := DB_Field_NotInitSearch;
      Result := False;
      exit;
    end;
  case FieldS_.PositionID of
    DB_Header_1, DB_Header_Last:
      begin
        FieldS_.InitFlags := False;
        FieldS_.State := DB_Header_NotFindHeader;
        Result := False;
        exit;
      end;
  end;
  if dbHeader_FindNext(FieldS_.Name, FieldS_.StartPos, FieldS_.OverPOS, IOHnd, FieldS_.RHeader) = False then
    begin
      FieldS_.InitFlags := False;
      FieldS_.State := FieldS_.RHeader.State;
      Result := False;
      exit;
    end;
  FieldS_.PositionID := FieldS_.RHeader.PositionID;
  FieldS_.StartPos := FieldS_.RHeader.NextHeader;
  FieldS_.ID := FieldS_.RHeader.ID;
  FieldS_.State := FieldS_.RHeader.State;
  Result := True;
end;

function dbField_OnlyFindLastName(const Name: U_String; const fPos: Int64; var IOHnd: TIOHnd; var FieldS_: TFieldSearch): Boolean;
var
  f: TField;
begin
  FieldS_.InitFlags := False;
  if dbField_ReadRec(fPos, IOHnd, f) = False then
    begin
      FieldS_.State := f.State;
      Result := False;
      exit;
    end;
  if f.HeaderCount = 0 then
    begin
      FieldS_.State := DB_Header_NotFindHeader;
      Result := False;
      exit;
    end;
  if dbHeader_FindPrev(Name, f.LastHeaderPOS, f.FirstHeaderPOS, IOHnd, FieldS_.RHeader) = False then
    begin
      FieldS_.State := FieldS_.RHeader.State;
      Result := False;
      exit;
    end;
  FieldS_.InitFlags := True;
  FieldS_.PositionID := FieldS_.RHeader.PositionID;
  FieldS_.OverPOS := f.FirstHeaderPOS;
  FieldS_.StartPos := FieldS_.RHeader.PrevHeader;
  FieldS_.Name := Name;
  FieldS_.ID := FieldS_.RHeader.ID;
  FieldS_.State := FieldS_.RHeader.State;
  Result := True;
end;

function dbField_OnlyFindPrevName(var IOHnd: TIOHnd; var FieldS_: TFieldSearch): Boolean;
begin
  if FieldS_.InitFlags = False then
    begin
      FieldS_.State := DB_Field_NotInitSearch;
      Result := False;
      exit;
    end;
  case FieldS_.PositionID of
    DB_Header_1, DB_Header_First:
      begin
        FieldS_.InitFlags := False;
        FieldS_.State := DB_Header_NotFindHeader;
        Result := False;
        exit;
      end;
  end;
  if dbHeader_FindPrev(FieldS_.Name, FieldS_.StartPos, FieldS_.OverPOS, IOHnd, FieldS_.RHeader) = False then
    begin
      FieldS_.InitFlags := False;
      FieldS_.State := FieldS_.RHeader.State;
      Result := False;
      exit;
    end;
  FieldS_.PositionID := FieldS_.RHeader.PositionID;
  FieldS_.StartPos := FieldS_.RHeader.PrevHeader;
  FieldS_.ID := FieldS_.RHeader.ID;
  FieldS_.State := FieldS_.RHeader.State;
  Result := True;
end;

function dbField_FindFirst(const Name: U_String; const ID: Byte; const fPos: Int64; var IOHnd: TIOHnd; var FieldS_: TFieldSearch): Boolean;
var
  f: TField;
begin
  FieldS_.InitFlags := False;
  if dbField_ReadRec(fPos, IOHnd, f) = False then
    begin
      FieldS_.State := f.State;
      Result := False;
      exit;
    end;
  if f.HeaderCount = 0 then
    begin
      FieldS_.State := DB_Header_NotFindHeader;
      Result := False;
      exit;
    end;
  FieldS_.OverPOS := f.LastHeaderPOS;
  FieldS_.StartPos := f.FirstHeaderPOS;
  while dbHeader_FindNext(Name, FieldS_.StartPos, FieldS_.OverPOS, IOHnd, FieldS_.RHeader) do
    begin
      FieldS_.StartPos := FieldS_.RHeader.NextHeader;
      if FieldS_.RHeader.ID = ID then
        begin
          FieldS_.InitFlags := True;
          FieldS_.PositionID := FieldS_.RHeader.PositionID;
          FieldS_.Name := Name;
          FieldS_.ID := ID;
          FieldS_.State := FieldS_.RHeader.State;
          Result := True;
          exit;
        end;
      if (FieldS_.RHeader.PositionID = DB_Header_1) or (FieldS_.RHeader.PositionID = DB_Header_Last) then
        begin
          FieldS_.State := DB_Header_NotFindHeader;
          Result := False;
          exit;
        end;
    end;
  FieldS_.State := FieldS_.RHeader.State;
  Result := False;
end;

function dbField_FindNext(var IOHnd: TIOHnd; var FieldS_: TFieldSearch): Boolean;
begin
  if FieldS_.InitFlags = False then
    begin
      FieldS_.State := DB_Field_NotInitSearch;
      Result := False;
      exit;
    end;
  case FieldS_.PositionID of
    DB_Header_1, DB_Header_Last:
      begin
        FieldS_.InitFlags := False;
        FieldS_.State := DB_Header_NotFindHeader;
        Result := False;
        exit;
      end;
  end;
  while dbHeader_FindNext(FieldS_.Name, FieldS_.StartPos, FieldS_.OverPOS, IOHnd, FieldS_.RHeader) do
    begin
      FieldS_.StartPos := FieldS_.RHeader.NextHeader;

      if FieldS_.RHeader.ID = FieldS_.ID then
        begin
          FieldS_.PositionID := FieldS_.RHeader.PositionID;
          FieldS_.State := FieldS_.RHeader.State;
          Result := True;
          exit;
        end;

      if FieldS_.RHeader.PositionID = DB_Header_Last then
        begin
          FieldS_.InitFlags := False;
          FieldS_.State := DB_Header_NotFindHeader;
          Result := False;
          exit;
        end;
    end;
  FieldS_.InitFlags := False;
  FieldS_.State := FieldS_.RHeader.State;
  Result := False;
end;

function dbField_FindLast(const Name: U_String; const ID: Byte; const fPos: Int64; var IOHnd: TIOHnd; var FieldS_: TFieldSearch): Boolean;
var
  f: TField;
begin
  FieldS_.InitFlags := False;
  if dbField_ReadRec(fPos, IOHnd, f) = False then
    begin
      FieldS_.State := f.State;
      Result := False;
      exit;
    end;
  if f.HeaderCount = 0 then
    begin
      FieldS_.State := DB_Header_NotFindHeader;
      Result := False;
      exit;
    end;
  FieldS_.OverPOS := f.FirstHeaderPOS;
  FieldS_.StartPos := f.LastHeaderPOS;
  while dbHeader_FindPrev(Name, FieldS_.StartPos, FieldS_.OverPOS, IOHnd, FieldS_.RHeader) do
    begin
      FieldS_.StartPos := FieldS_.RHeader.PrevHeader;
      if FieldS_.RHeader.ID = ID then
        begin
          FieldS_.InitFlags := True;
          FieldS_.PositionID := FieldS_.RHeader.PositionID;
          FieldS_.Name := Name;
          FieldS_.ID := ID;
          FieldS_.State := FieldS_.RHeader.State;
          Result := True;
          exit;
        end;
      if (FieldS_.RHeader.PositionID = DB_Header_1) or (FieldS_.RHeader.PositionID = DB_Header_First) then
        begin
          FieldS_.State := DB_Header_NotFindHeader;
          Result := False;
          exit;
        end;
    end;
  FieldS_.State := FieldS_.RHeader.State;
  Result := False;
end;

function dbField_FindPrev(var IOHnd: TIOHnd; var FieldS_: TFieldSearch): Boolean;
begin
  if FieldS_.InitFlags = False then
    begin
      FieldS_.State := DB_Field_NotInitSearch;
      Result := False;
      exit;
    end;
  case FieldS_.PositionID of
    DB_Header_1, DB_Header_First:
      begin
        FieldS_.InitFlags := False;
        FieldS_.State := DB_Header_NotFindHeader;
        Result := False;
        exit;
      end;
  end;
  while dbHeader_FindPrev(FieldS_.Name, FieldS_.StartPos, FieldS_.OverPOS, IOHnd, FieldS_.RHeader) do
    begin
      FieldS_.StartPos := FieldS_.RHeader.PrevHeader;

      if FieldS_.RHeader.ID = FieldS_.ID then
        begin
          FieldS_.PositionID := FieldS_.RHeader.PositionID;
          FieldS_.State := FieldS_.RHeader.State;
          Result := True;
          exit;
        end;

      if FieldS_.RHeader.PositionID = DB_Header_First then
        begin
          FieldS_.InitFlags := False;
          FieldS_.State := DB_Header_NotFindHeader;
          Result := False;
          exit;
        end;
    end;
  FieldS_.InitFlags := False;
  FieldS_.State := FieldS_.RHeader.State;
  Result := False;
end;

function dbField_FindFirstItem(const Name: U_String; const ItemExtID: Byte; const fPos: Int64; var IOHnd: TIOHnd; var FieldS_: TFieldSearch; var Item_: TItem): Boolean;
begin
  if dbField_FindFirst(Name, DB_Header_Item_ID, fPos, IOHnd, FieldS_) = False then
    begin
      Result := False;
      exit;
    end;
  Item_.RHeader := FieldS_.RHeader;
  if dbItem_OnlyReadItemRec(FieldS_.RHeader.DataPosition, IOHnd, Item_) = False then
    begin
      FieldS_.State := Item_.State;
      Result := False;
      exit;
    end;

  if Item_.ExtID = ItemExtID then
    begin
      Result := True;
      exit;
    end;

  while dbField_FindNext(IOHnd, FieldS_) do
    begin
      Item_.RHeader := FieldS_.RHeader;
      if dbItem_OnlyReadItemRec(FieldS_.RHeader.DataPosition, IOHnd, Item_) = False then
        begin
          FieldS_.State := Item_.State;
          Result := False;
          exit;
        end;

      if Item_.ExtID = ItemExtID then
        begin
          Result := True;
          exit;
        end;
    end;
  Result := False;
end;

function dbField_FindNextItem(const ItemExtID: Byte; var IOHnd: TIOHnd; var FieldS_: TFieldSearch; var Item_: TItem): Boolean;
begin
  while dbField_FindNext(IOHnd, FieldS_) do
    begin
      Item_.RHeader := FieldS_.RHeader;
      if dbItem_OnlyReadItemRec(FieldS_.RHeader.DataPosition, IOHnd, Item_) = False then
        begin
          FieldS_.State := Item_.State;
          Result := False;
          exit;
        end;

      if Item_.ExtID = ItemExtID then
        begin
          Result := True;
          exit;
        end;
    end;
  Result := False;
end;

function dbField_FindLastItem(const Name: U_String; const ItemExtID: Byte; const fPos: Int64; var IOHnd: TIOHnd; var FieldS_: TFieldSearch; var Item_: TItem): Boolean;
begin
  if dbField_FindLast(Name, DB_Header_Item_ID, fPos, IOHnd, FieldS_) = False then
    begin
      Result := False;
      exit;
    end;
  Item_.RHeader := FieldS_.RHeader;
  if dbItem_OnlyReadItemRec(FieldS_.RHeader.DataPosition, IOHnd, Item_) = False then
    begin
      FieldS_.State := Item_.State;
      Result := False;
      exit;
    end;

  if Item_.ExtID = ItemExtID then
    begin
      Result := True;
      exit;
    end;

  while dbField_FindPrev(IOHnd, FieldS_) do
    begin
      Item_.RHeader := FieldS_.RHeader;
      if dbItem_OnlyReadItemRec(FieldS_.RHeader.DataPosition, IOHnd, Item_) = False then
        begin
          FieldS_.State := Item_.State;
          Result := False;
          exit;
        end;

      if Item_.ExtID = ItemExtID then
        begin
          Result := True;
          exit;
        end;
    end;
  Result := False;
end;

function dbField_FindPrevItem(const ItemExtID: Byte; var IOHnd: TIOHnd; var FieldS_: TFieldSearch; var Item_: TItem): Boolean;
begin
  while dbField_FindPrev(IOHnd, FieldS_) do
    begin
      Item_.RHeader := FieldS_.RHeader;
      if dbItem_OnlyReadItemRec(FieldS_.RHeader.DataPosition, IOHnd, Item_) = False then
        begin
          FieldS_.State := Item_.State;
          Result := False;
          exit;
        end;

      if Item_.ExtID = ItemExtID then
        begin
          Result := True;
          exit;
        end;
    end;
  Result := False;
end;

function dbField_FindFirstItem(const Name: U_String; const ItemExtID: Byte; const fPos: Int64; var IOHnd: TIOHnd; var FieldS_: TFieldSearch): Boolean;
var
  itm: TItem;
begin
  if dbField_FindFirst(Name, DB_Header_Item_ID, fPos, IOHnd, FieldS_) = False then
    begin
      Result := False;
      exit;
    end;
  if dbItem_ReadRec(FieldS_.RHeader.CurrentHeader, IOHnd, itm) = False then
    begin
      FieldS_.State := itm.State;
      Result := False;
      exit;
    end;

  if itm.ExtID = ItemExtID then
    begin
      Result := True;
      exit;
    end;

  while dbField_FindNext(IOHnd, FieldS_) do
    begin
      if dbItem_ReadRec(FieldS_.RHeader.CurrentHeader, IOHnd, itm) = False then
        begin
          FieldS_.State := itm.State;
          Result := False;
          exit;
        end;
      if itm.ExtID = ItemExtID then
        begin
          Result := True;
          exit;
        end;
    end;
  Result := False;
end;

function dbField_FindNextItem(const ItemExtID: Byte; var IOHnd: TIOHnd; var FieldS_: TFieldSearch): Boolean;
var
  itm: TItem;
begin
  while dbField_FindNext(IOHnd, FieldS_) do
    begin
      if dbItem_ReadRec(FieldS_.RHeader.CurrentHeader, IOHnd, itm) = False then
        begin
          FieldS_.State := itm.State;
          Result := False;
          exit;
        end;
      if itm.ExtID = ItemExtID then
        begin
          Result := True;
          exit;
        end;
    end;
  Result := False;
end;

function dbField_FindLastItem(const Name: U_String; const ItemExtID: Byte; const fPos: Int64; var IOHnd: TIOHnd; var FieldS_: TFieldSearch): Boolean;
var
  itm: TItem;
begin
  if dbField_FindLast(Name, DB_Header_Item_ID, fPos, IOHnd, FieldS_) = False then
    begin
      Result := False;
      exit;
    end;
  if dbItem_ReadRec(FieldS_.RHeader.CurrentHeader, IOHnd, itm) = False then
    begin
      FieldS_.State := itm.State;
      Result := False;
      exit;
    end;

  if itm.ExtID = ItemExtID then
    begin
      Result := True;
      exit;
    end;

  while dbField_FindPrev(IOHnd, FieldS_) do
    begin
      if dbItem_ReadRec(FieldS_.RHeader.CurrentHeader, IOHnd, itm) = False then
        begin
          FieldS_.State := itm.State;
          Result := False;
          exit;
        end;
      if itm.ExtID = ItemExtID then
        begin
          Result := True;
          exit;
        end;
    end;
  Result := False;
end;

function dbField_FindPrevItem(const ItemExtID: Byte; var IOHnd: TIOHnd; var FieldS_: TFieldSearch): Boolean;
var
  itm: TItem;
begin
  while dbField_FindPrev(IOHnd, FieldS_) do
    begin
      if dbItem_ReadRec(FieldS_.RHeader.CurrentHeader, IOHnd, itm) = False then
        begin
          FieldS_.State := itm.State;
          Result := False;
          exit;
        end;
      if itm.ExtID = ItemExtID then
        begin
          Result := True;
          exit;
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

function dbField_CreateHeader(const Name: U_String; const ID: Byte; const fPos: Int64; var IOHnd: TIOHnd; var Header_: THeader): Boolean;
var
  f: TField;
  Header: THeader;
begin
  if dbField_ReadRec(fPos, IOHnd, f) = False then
    begin
      Header_.State := f.State;
      Result := False;
      exit;
    end;
  Header_.ID := ID;
  Header_.Name := Name;
  case f.HeaderCount of
    0:
      begin
        f.HeaderCount := 1;
        f.FirstHeaderPOS := umlFileGetSize(IOHnd);
        f.LastHeaderPOS := f.FirstHeaderPOS;
        f.RHeader.ModificationTime := umlDefaultTime;
        Header_.PositionID := DB_Header_1;
        Header_.NextHeader := f.LastHeaderPOS;
        Header_.PrevHeader := f.FirstHeaderPOS;
        Header_.CurrentHeader := f.FirstHeaderPOS;
        Header_.CreateTime := umlDefaultTime;
        Header_.ModificationTime := umlDefaultTime;
        Header_.DataPosition := Header_.CurrentHeader + Get_DB_HeaderL(IOHnd);
        if dbField_WriteRec(f.RHeader.CurrentHeader, IOHnd, f) = False then
          begin
            Header_.State := f.State;
            Result := False;
            exit;
          end;
        if dbHeader_WriteRec(Header_.CurrentHeader, IOHnd, Header_) = False then
          begin
            Result := False;
            exit;
          end;
      end;
    1:
      begin
        Header_.CurrentHeader := umlFileGetSize(IOHnd);
        Header_.NextHeader := f.FirstHeaderPOS;
        Header_.PrevHeader := f.FirstHeaderPOS;

        if dbHeader_ReadRec(f.FirstHeaderPOS, IOHnd, Header) = False then
          begin
            Header_.State := Header.State;
            Result := False;
            exit;
          end;
        Header.PrevHeader := Header_.CurrentHeader;
        Header.NextHeader := Header_.CurrentHeader;
        Header.PositionID := DB_Header_First;
        if dbHeader_WriteRec(f.FirstHeaderPOS, IOHnd, Header) = False then
          begin
            Header_.State := Header.State;
            Result := False;
            exit;
          end;
        f.HeaderCount := f.HeaderCount + 1;
        f.LastHeaderPOS := Header_.CurrentHeader;
        f.RHeader.ModificationTime := umlDefaultTime;
        Header_.CreateTime := umlDefaultTime;
        Header_.ModificationTime := umlDefaultTime;
        Header_.DataPosition := Header_.CurrentHeader + Get_DB_HeaderL(IOHnd);
        Header_.PositionID := DB_Header_Last;
        if dbField_WriteRec(f.RHeader.CurrentHeader, IOHnd, f) = False then
          begin
            Header_.State := f.State;
            Result := False;
            exit;
          end;
        if dbHeader_WriteRec(Header_.CurrentHeader, IOHnd, Header_) = False then
          begin
            Result := False;
            exit;
          end;
      end;
    else
      begin
        Header_.CurrentHeader := umlFileGetSize(IOHnd);

        // modify first header
        if dbHeader_ReadRec(f.FirstHeaderPOS, IOHnd, Header) = False then
          begin
            Header_.State := Header.State;
            Result := False;
            exit;
          end;
        Header.PrevHeader := Header_.CurrentHeader;
        Header_.NextHeader := Header.CurrentHeader;
        if dbHeader_WriteRec(f.FirstHeaderPOS, IOHnd, Header) = False then
          begin
            Header_.State := Header.State;
            Result := False;
            exit;
          end;

        // moidfy last header
        if dbHeader_ReadRec(f.LastHeaderPOS, IOHnd, Header) = False then
          begin
            Header_.State := Header.State;
            Result := False;
            exit;
          end;
        Header.NextHeader := Header_.CurrentHeader;
        Header_.PrevHeader := f.LastHeaderPOS;
        Header.PositionID := DB_Header_Medium;
        if dbHeader_WriteRec(f.LastHeaderPOS, IOHnd, Header) = False then
          begin
            Header_.State := Header.State;
            Result := False;
            exit;
          end;

        f.HeaderCount := f.HeaderCount + 1;
        f.LastHeaderPOS := Header_.CurrentHeader;
        f.RHeader.ModificationTime := umlDefaultTime;
        Header_.CreateTime := umlDefaultTime;
        Header_.ModificationTime := umlDefaultTime;
        Header_.DataPosition := Header_.CurrentHeader + Get_DB_HeaderL(IOHnd);
        Header_.PositionID := DB_Header_Last;
        if dbField_WriteRec(f.RHeader.CurrentHeader, IOHnd, f) = False then
          begin
            Header_.State := f.State;
            Result := False;
            exit;
          end;
        if dbHeader_WriteRec(Header_.CurrentHeader, IOHnd, Header_) = False then
          begin
            Result := False;
            exit;
          end;
      end;
  end;
  Header_.State := DB_Header_ok;
  Result := True;
end;

function dbField_InsertNewHeader(const Name: U_String; const ID: Byte; const FieldPos, InsertHeaderPos: Int64; var IOHnd: TIOHnd; var NewHeader: THeader): Boolean;
var
  f: TField;

  Curr, Prev: THeader;
begin
  if dbField_ReadRec(FieldPos, IOHnd, f) = False then
    begin
      NewHeader.State := f.State;
      Result := False;
      exit;
    end;

  if dbHeader_ReadRec(InsertHeaderPos, IOHnd, Curr) = False then
    begin
      NewHeader.State := Curr.State;
      Result := False;
      exit;
    end;

  f.RHeader.ModificationTime := umlDefaultTime;

  NewHeader.CurrentHeader := umlFileGetSize(IOHnd);
  NewHeader.DataPosition := NewHeader.CurrentHeader + Get_DB_HeaderL(IOHnd);
  NewHeader.CreateTime := umlDefaultTime;
  NewHeader.ModificationTime := umlDefaultTime;
  NewHeader.ID := ID;
  NewHeader.UserProperty := 0;
  NewHeader.Name := Name;
  NewHeader.State := DB_Header_ok;

  case Curr.PositionID of
    DB_Header_First:
      begin
        if f.HeaderCount > 1 then
          begin
            // moidfy field
            f.HeaderCount := f.HeaderCount + 1;
            f.FirstHeaderPOS := NewHeader.CurrentHeader;
            if dbField_WriteRec(f.RHeader.CurrentHeader, IOHnd, f) = False then
              begin
                NewHeader.State := f.State;
                Result := False;
                exit;
              end;

            // write newheader
            NewHeader.PrevHeader := f.LastHeaderPOS;
            NewHeader.NextHeader := Curr.CurrentHeader;
            NewHeader.PositionID := DB_Header_First;
            if dbHeader_WriteRec(NewHeader.CurrentHeader, IOHnd, NewHeader) = False then
              begin
                Result := False;
                exit;
              end;

            // moidfy current
            Curr.PrevHeader := NewHeader.CurrentHeader;
            Curr.PositionID := DB_Header_Medium;
            if dbHeader_WriteRec(Curr.CurrentHeader, IOHnd, Curr) = False then
              begin
                NewHeader.State := Curr.State;
                Result := False;
                exit;
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
                NewHeader.State := f.State;
                Result := False;
                exit;
              end;

            // write newheader
            NewHeader.PrevHeader := f.LastHeaderPOS;
            NewHeader.NextHeader := Curr.CurrentHeader;
            NewHeader.PositionID := DB_Header_First;
            if dbHeader_WriteRec(NewHeader.CurrentHeader, IOHnd, NewHeader) = False then
              begin
                Result := False;
                exit;
              end;

            // modify current header
            Curr.PrevHeader := NewHeader.CurrentHeader;
            Curr.PositionID := DB_Header_Last;
            if dbHeader_WriteRec(Curr.CurrentHeader, IOHnd, Curr) = False then
              begin
                NewHeader.State := Curr.State;
                Result := False;
                exit;
              end;
          end
        else
          begin
            // error
            NewHeader.State := DB_Header_NotFindHeader;
            Result := False;
            exit;
          end
      end;
    DB_Header_Medium:
      begin
        // read prev header
        if dbHeader_ReadRec(Curr.PrevHeader, IOHnd, Prev) = False then
          begin
            NewHeader.State := Prev.State;
            Result := False;
            exit;
          end;

        // modify field
        f.HeaderCount := f.HeaderCount + 1;
        if dbField_WriteRec(f.RHeader.CurrentHeader, IOHnd, f) = False then
          begin
            NewHeader.State := f.State;
            Result := False;
            exit;
          end;

        // write newheader
        NewHeader.PrevHeader := Prev.CurrentHeader;
        NewHeader.NextHeader := Curr.CurrentHeader;
        NewHeader.PositionID := DB_Header_Medium;
        if dbHeader_WriteRec(NewHeader.CurrentHeader, IOHnd, NewHeader) = False then
          begin
            Result := False;
            exit;
          end;

        // modify prev header
        Prev.NextHeader := NewHeader.CurrentHeader;
        if dbHeader_WriteRec(Prev.CurrentHeader, IOHnd, Prev) = False then
          begin
            NewHeader.State := Prev.State;
            Result := False;
            exit;
          end;

        // write current
        Curr.PrevHeader := NewHeader.CurrentHeader;
        Curr.PositionID := DB_Header_Medium;
        if dbHeader_WriteRec(Curr.CurrentHeader, IOHnd, Curr) = False then
          begin
            NewHeader.State := Curr.State;
            Result := False;
            exit;
          end;
      end;
    DB_Header_Last:
      begin
        if f.HeaderCount > 1 then
          begin
            // read prev header
            if dbHeader_ReadRec(Curr.PrevHeader, IOHnd, Prev) = False then
              begin
                NewHeader.State := Prev.State;
                Result := False;
                exit;
              end;

            // modify field
            f.HeaderCount := f.HeaderCount + 1;
            if dbField_WriteRec(f.RHeader.CurrentHeader, IOHnd, f) = False then
              begin
                NewHeader.State := f.State;
                Result := False;
                exit;
              end;

            // write newheader
            NewHeader.PrevHeader := Prev.CurrentHeader;
            NewHeader.NextHeader := Curr.CurrentHeader;
            NewHeader.PositionID := DB_Header_Medium;
            if dbHeader_WriteRec(NewHeader.CurrentHeader, IOHnd, NewHeader) = False then
              begin
                Result := False;
                exit;
              end;

            // modify prev header
            Prev.NextHeader := NewHeader.CurrentHeader;
            if dbHeader_WriteRec(Prev.CurrentHeader, IOHnd, Prev) = False then
              begin
                NewHeader.State := Prev.State;
                Result := False;
                exit;
              end;

            // write current
            Curr.PrevHeader := NewHeader.CurrentHeader;
            Curr.PositionID := DB_Header_Last;
            if dbHeader_WriteRec(Curr.CurrentHeader, IOHnd, Curr) = False then
              begin
                NewHeader.State := Curr.State;
                Result := False;
                exit;
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
                NewHeader.State := f.State;
                Result := False;
                exit;
              end;

            // write newheader
            NewHeader.PrevHeader := f.LastHeaderPOS;
            NewHeader.NextHeader := Curr.CurrentHeader;
            NewHeader.PositionID := DB_Header_First;
            if dbHeader_WriteRec(NewHeader.CurrentHeader, IOHnd, NewHeader) = False then
              begin
                Result := False;
                exit;
              end;

            // modify current header
            Curr.PrevHeader := NewHeader.CurrentHeader;
            Curr.PositionID := DB_Header_Last;
            if dbHeader_WriteRec(Curr.CurrentHeader, IOHnd, Curr) = False then
              begin
                NewHeader.State := Curr.State;
                Result := False;
                exit;
              end;
          end
        else
          begin
            // error
            NewHeader.State := DB_Header_NotFindHeader;
            Result := False;
            exit;
          end;
      end;
    DB_Header_1:
      begin
        // modify field
        f.HeaderCount := f.HeaderCount + 1;
        f.FirstHeaderPOS := NewHeader.CurrentHeader;
        f.LastHeaderPOS := Curr.CurrentHeader;
        if dbField_WriteRec(f.RHeader.CurrentHeader, IOHnd, f) = False then
          begin
            NewHeader.State := f.State;
            Result := False;
            exit;
          end;

        // write newheader
        NewHeader.PrevHeader := f.LastHeaderPOS;
        NewHeader.NextHeader := Curr.CurrentHeader;
        NewHeader.PositionID := DB_Header_First;
        if dbHeader_WriteRec(NewHeader.CurrentHeader, IOHnd, NewHeader) = False then
          begin
            Result := False;
            exit;
          end;

        // modify current header
        Curr.PrevHeader := NewHeader.CurrentHeader;
        Curr.PositionID := DB_Header_Last;
        if dbHeader_WriteRec(Curr.CurrentHeader, IOHnd, Curr) = False then
          begin
            NewHeader.State := Curr.State;
            Result := False;
            exit;
          end;
      end;
  end;

  NewHeader.State := DB_Header_ok;
  Result := True;
end;

function dbField_DeleteHeader_(const HeaderPOS, FieldPos: Int64; var IOHnd: TIOHnd; var Field_: TField): Boolean;
var
  DeleteHeader, SwapHeader: THeader;
begin
  if dbField_ReadRec(FieldPos, IOHnd, Field_) = False then
    begin
      Result := False;
      exit;
    end;
  case Field_.HeaderCount of
    0:
      begin
        Field_.State := DB_Field_DeleteHeaderError;
        Result := False;
        exit;
      end;
    1:
      begin
        if HeaderPOS = Field_.FirstHeaderPOS then
          begin
            Field_.HeaderCount := 0;
            Field_.FirstHeaderPOS := 0;
            Field_.LastHeaderPOS := 0;
            Field_.RHeader.ModificationTime := umlDefaultTime;
            if dbField_WriteRec(Field_.RHeader.CurrentHeader, IOHnd, Field_) = False then
              begin
                Result := False;
                exit;
              end;
            Result := True;
            Field_.State := DB_Field_ok;
            exit;
          end;
        Result := False;
        Field_.State := DB_Field_DeleteHeaderError;
        exit;
      end;
    2:
      begin
        if dbHeader_ReadRec(HeaderPOS, IOHnd, DeleteHeader) = False then
          begin
            Field_.State := DeleteHeader.State;
            Result := False;
            exit;
          end;
        case DeleteHeader.PositionID of
          DB_Header_First:
            begin
              if dbHeader_ReadRec(Field_.LastHeaderPOS, IOHnd, SwapHeader) = False then
                begin
                  Field_.State := SwapHeader.State;
                  Result := False;
                  exit;
                end;
              SwapHeader.NextHeader := SwapHeader.CurrentHeader;
              SwapHeader.PrevHeader := SwapHeader.CurrentHeader;
              SwapHeader.PositionID := DB_Header_1;
              if dbHeader_WriteRec(SwapHeader.CurrentHeader, IOHnd, SwapHeader) = False then
                begin
                  Field_.State := SwapHeader.State;
                  Result := False;
                  exit;
                end;
              Field_.FirstHeaderPOS := SwapHeader.CurrentHeader;
              Field_.LastHeaderPOS := SwapHeader.CurrentHeader;
              Field_.HeaderCount := Field_.HeaderCount - 1;
              Field_.RHeader.ModificationTime := umlDefaultTime;
              if dbField_WriteRec(Field_.RHeader.CurrentHeader, IOHnd, Field_) = False then
                begin
                  Result := False;
                  exit;
                end;
              Field_.State := DB_Field_ok;
              Result := True;
            end;
          DB_Header_Last:
            begin
              if dbHeader_ReadRec(Field_.FirstHeaderPOS, IOHnd, SwapHeader) = False then
                begin
                  Field_.State := SwapHeader.State;
                  Result := False;
                  exit;
                end;
              SwapHeader.NextHeader := SwapHeader.CurrentHeader;
              SwapHeader.PrevHeader := SwapHeader.CurrentHeader;
              SwapHeader.PositionID := DB_Header_1;
              if dbHeader_WriteRec(SwapHeader.CurrentHeader, IOHnd, SwapHeader) = False then
                begin
                  Field_.State := SwapHeader.State;
                  Result := False;
                  exit;
                end;
              Field_.FirstHeaderPOS := SwapHeader.CurrentHeader;
              Field_.LastHeaderPOS := SwapHeader.CurrentHeader;
              Field_.HeaderCount := Field_.HeaderCount - 1;
              Field_.RHeader.ModificationTime := umlDefaultTime;
              if dbField_WriteRec(Field_.RHeader.CurrentHeader, IOHnd, Field_) = False then
                begin
                  Result := False;
                  exit;
                end;
              Field_.State := DB_Field_ok;
              Result := True;
            end;
          else
            begin
              Field_.State := DB_Field_DeleteHeaderError;
              Result := False;
            end;
        end;
        exit;
      end;
    3:
      begin
        if dbHeader_ReadRec(HeaderPOS, IOHnd, DeleteHeader) = False then
          begin
            Field_.State := DeleteHeader.State;
            Result := False;
            exit;
          end;
        case DeleteHeader.PositionID of
          DB_Header_First:
            begin
              if dbHeader_ReadRec(DeleteHeader.NextHeader, IOHnd, SwapHeader) = False then
                begin
                  Field_.State := SwapHeader.State;
                  Result := False;
                  exit;
                end;
              SwapHeader.PrevHeader := DeleteHeader.PrevHeader;
              SwapHeader.PositionID := DB_Header_First;
              if dbHeader_WriteRec(SwapHeader.CurrentHeader, IOHnd, SwapHeader) = False then
                begin
                  Field_.State := SwapHeader.State;
                  Result := False;
                  exit;
                end;
              Field_.FirstHeaderPOS := SwapHeader.CurrentHeader;
              if dbHeader_ReadRec(DeleteHeader.PrevHeader, IOHnd, SwapHeader) = False then
                begin
                  Field_.State := SwapHeader.State;
                  Result := False;
                  exit;
                end;
              SwapHeader.NextHeader := DeleteHeader.NextHeader;
              if dbHeader_WriteRec(SwapHeader.CurrentHeader, IOHnd, SwapHeader) = False then
                begin
                  Field_.State := SwapHeader.State;
                  Result := False;
                  exit;
                end;
              Field_.HeaderCount := Field_.HeaderCount - 1;
              Field_.RHeader.ModificationTime := umlDefaultTime;
              if dbField_WriteRec(Field_.RHeader.CurrentHeader, IOHnd, Field_) = False then
                begin
                  Result := False;
                  exit;
                end;
              Field_.State := DB_Field_ok;
              Result := True;
            end;
          DB_Header_Medium:
            begin
              if dbHeader_ReadRec(DeleteHeader.PrevHeader, IOHnd, SwapHeader) = False then
                begin
                  Field_.State := SwapHeader.State;
                  Result := False;
                  exit;
                end;
              SwapHeader.NextHeader := DeleteHeader.NextHeader;
              if dbHeader_WriteRec(SwapHeader.CurrentHeader, IOHnd, SwapHeader) = False then
                begin
                  Field_.State := SwapHeader.State;
                  Result := False;
                  exit;
                end;
              if dbHeader_ReadRec(DeleteHeader.NextHeader, IOHnd, SwapHeader) = False then
                begin
                  Field_.State := SwapHeader.State;
                  Result := False;
                  exit;
                end;
              SwapHeader.PrevHeader := DeleteHeader.PrevHeader;
              if dbHeader_WriteRec(SwapHeader.CurrentHeader, IOHnd, SwapHeader) = False then
                begin
                  Field_.State := SwapHeader.State;
                  Result := False;
                  exit;
                end;
              Field_.HeaderCount := Field_.HeaderCount - 1;
              Field_.RHeader.ModificationTime := umlDefaultTime;
              if dbField_WriteRec(Field_.RHeader.CurrentHeader, IOHnd, Field_) = False then
                begin
                  Result := False;
                  exit;
                end;
              Field_.State := DB_Field_ok;
              Result := True;
            end;
          DB_Header_Last:
            begin
              if dbHeader_ReadRec(DeleteHeader.PrevHeader, IOHnd, SwapHeader) = False then
                begin
                  Field_.State := SwapHeader.State;
                  Result := False;
                  exit;
                end;
              SwapHeader.NextHeader := DeleteHeader.NextHeader;
              SwapHeader.PositionID := DB_Header_Last;
              if dbHeader_WriteRec(SwapHeader.CurrentHeader, IOHnd, SwapHeader) = False then
                begin
                  Field_.State := SwapHeader.State;
                  Result := False;
                  exit;
                end;
              Field_.LastHeaderPOS := SwapHeader.CurrentHeader;
              if dbHeader_ReadRec(DeleteHeader.NextHeader, IOHnd, SwapHeader) = False then
                begin
                  Field_.State := SwapHeader.State;
                  Result := False;
                  exit;
                end;
              SwapHeader.PrevHeader := DeleteHeader.PrevHeader;
              if dbHeader_WriteRec(SwapHeader.CurrentHeader, IOHnd, SwapHeader) = False then
                begin
                  Field_.State := SwapHeader.State;
                  Result := False;
                  exit;
                end;
              Field_.HeaderCount := Field_.HeaderCount - 1;
              Field_.RHeader.ModificationTime := umlDefaultTime;
              if dbField_WriteRec(Field_.RHeader.CurrentHeader, IOHnd, Field_) = False then
                begin
                  Result := False;
                  exit;
                end;
              Field_.State := DB_Field_ok;
              Result := True;
            end;
          else
            begin
              Field_.State := DB_Field_DeleteHeaderError;
              Result := False;
            end;
        end;
        exit;
      end;
    else
      begin
        if dbHeader_ReadRec(HeaderPOS, IOHnd, DeleteHeader) = False then
          begin
            Field_.State := DeleteHeader.State;
            Result := False;
            exit;
          end;
        case DeleteHeader.PositionID of
          DB_Header_First:
            begin
              if dbHeader_ReadRec(DeleteHeader.NextHeader, IOHnd, SwapHeader) = False then
                begin
                  Field_.State := SwapHeader.State;
                  Result := False;
                  exit;
                end;
              SwapHeader.PrevHeader := DeleteHeader.PrevHeader;
              SwapHeader.PositionID := DB_Header_First;
              if dbHeader_WriteRec(SwapHeader.CurrentHeader, IOHnd, SwapHeader) = False then
                begin
                  Field_.State := SwapHeader.State;
                  Result := False;
                  exit;
                end;
              Field_.FirstHeaderPOS := SwapHeader.CurrentHeader;
              if dbHeader_ReadRec(DeleteHeader.PrevHeader, IOHnd, SwapHeader) = False then
                begin
                  Field_.State := SwapHeader.State;
                  Result := False;
                  exit;
                end;
              SwapHeader.NextHeader := DeleteHeader.NextHeader;
              if dbHeader_WriteRec(SwapHeader.CurrentHeader, IOHnd, SwapHeader) = False then
                begin
                  Field_.State := SwapHeader.State;
                  Result := False;
                  exit;
                end;
              Field_.HeaderCount := Field_.HeaderCount - 1;
              Field_.RHeader.ModificationTime := umlDefaultTime;
              if dbField_WriteRec(Field_.RHeader.CurrentHeader, IOHnd, Field_) = False then
                begin
                  Result := False;
                  exit;
                end;
              Field_.State := DB_Field_ok;
              Result := True;
            end;
          DB_Header_Medium:
            begin
              if dbHeader_ReadRec(DeleteHeader.PrevHeader, IOHnd, SwapHeader) = False then
                begin
                  Field_.State := SwapHeader.State;
                  Result := False;
                  exit;
                end;
              SwapHeader.NextHeader := DeleteHeader.NextHeader;
              if dbHeader_WriteRec(SwapHeader.CurrentHeader, IOHnd, SwapHeader) = False then
                begin
                  Field_.State := SwapHeader.State;
                  Result := False;
                  exit;
                end;
              if dbHeader_ReadRec(DeleteHeader.NextHeader, IOHnd, SwapHeader) = False then
                begin
                  Field_.State := SwapHeader.State;
                  Result := False;
                  exit;
                end;
              SwapHeader.PrevHeader := DeleteHeader.PrevHeader;
              if dbHeader_WriteRec(SwapHeader.CurrentHeader, IOHnd, SwapHeader) = False then
                begin
                  Field_.State := SwapHeader.State;
                  Result := False;
                  exit;
                end;
              Field_.HeaderCount := Field_.HeaderCount - 1;
              Field_.RHeader.ModificationTime := umlDefaultTime;
              if dbField_WriteRec(Field_.RHeader.CurrentHeader, IOHnd, Field_) = False then
                begin
                  Result := False;
                  exit;
                end;
              Field_.State := DB_Field_ok;
              Result := True;
            end;
          DB_Header_Last:
            begin
              if dbHeader_ReadRec(DeleteHeader.PrevHeader, IOHnd, SwapHeader) = False then
                begin
                  Field_.State := SwapHeader.State;
                  Result := False;
                  exit;
                end;
              SwapHeader.NextHeader := DeleteHeader.NextHeader;
              SwapHeader.PositionID := DB_Header_Last;
              if dbHeader_WriteRec(SwapHeader.CurrentHeader, IOHnd, SwapHeader) = False then
                begin
                  Field_.State := SwapHeader.State;
                  Result := False;
                  exit;
                end;
              Field_.LastHeaderPOS := SwapHeader.CurrentHeader;
              if dbHeader_ReadRec(DeleteHeader.NextHeader, IOHnd, SwapHeader) = False then
                begin
                  Field_.State := SwapHeader.State;
                  Result := False;
                  exit;
                end;
              SwapHeader.PrevHeader := DeleteHeader.PrevHeader;
              if dbHeader_WriteRec(SwapHeader.CurrentHeader, IOHnd, SwapHeader) = False then
                begin
                  Field_.State := SwapHeader.State;
                  Result := False;
                  exit;
                end;
              Field_.HeaderCount := Field_.HeaderCount - 1;
              Field_.RHeader.ModificationTime := umlDefaultTime;
              if dbField_WriteRec(Field_.RHeader.CurrentHeader, IOHnd, Field_) = False then
                begin
                  Result := False;
                  exit;
                end;
              Field_.State := DB_Field_ok;
              Result := True;
            end;
          else
            begin
              Field_.State := DB_Field_DeleteHeaderError;
              Result := False;
            end;
        end;
        exit;
      end;
  end;
  Result := True;
end;

function dbField_DeleteHeader(const HeaderPOS, FieldPos: Int64; var IOHnd: TIOHnd; var Field_: TField): Boolean;
begin
  Result := dbField_DeleteHeader_(HeaderPOS, FieldPos, IOHnd, Field_);
  if Result then
    if IOHnd.Data <> nil then
      if Assigned(PObjectDataHandle(IOHnd.Data)^.OnDeleteHeader) then
          PObjectDataHandle(IOHnd.Data)^.OnDeleteHeader(HeaderPOS);
end;

function dbField_MoveHeader(const HeaderPOS: Int64; const SourcerFieldPOS, TargetFieldPos: Int64; var IOHnd: TIOHnd; var Field_: TField): Boolean;
var
  ActiveHeader, SwapHeader: THeader;
begin
  if dbHeader_ReadRec(HeaderPOS, IOHnd, ActiveHeader) = False then
    begin
      Field_.State := ActiveHeader.State;
      Result := False;
      exit;
    end;
  if dbField_DeleteHeader_(ActiveHeader.CurrentHeader, SourcerFieldPOS, IOHnd, Field_) = False then
    begin
      Result := False;
      exit;
    end;

  if dbField_ReadRec(TargetFieldPos, IOHnd, Field_) = False then
    begin
      Result := False;
      exit;
    end;
  case Field_.HeaderCount of
    0:
      begin
        Field_.HeaderCount := 1;
        Field_.FirstHeaderPOS := ActiveHeader.CurrentHeader;
        Field_.LastHeaderPOS := Field_.FirstHeaderPOS;
        ActiveHeader.PositionID := DB_Header_1;
        ActiveHeader.NextHeader := Field_.FirstHeaderPOS;
        ActiveHeader.PrevHeader := Field_.FirstHeaderPOS;
        if dbField_WriteRec(Field_.RHeader.CurrentHeader, IOHnd, Field_) = False then
          begin
            Result := False;
            exit;
          end;
        if dbHeader_WriteRec(ActiveHeader.CurrentHeader, IOHnd, ActiveHeader) = False then
          begin
            Field_.State := ActiveHeader.State;
            Result := False;
            exit;
          end;
      end;
    1:
      begin

        if dbHeader_ReadRec(Field_.FirstHeaderPOS, IOHnd, SwapHeader) = False then
          begin
            Field_.State := SwapHeader.State;
            Result := False;
            exit;
          end;
        SwapHeader.PrevHeader := ActiveHeader.CurrentHeader;
        SwapHeader.NextHeader := ActiveHeader.CurrentHeader;
        SwapHeader.PositionID := DB_Header_First;
        ActiveHeader.NextHeader := SwapHeader.CurrentHeader;
        ActiveHeader.PrevHeader := SwapHeader.CurrentHeader;
        ActiveHeader.PositionID := DB_Header_Last;

        if dbHeader_WriteRec(SwapHeader.CurrentHeader, IOHnd, SwapHeader) = False then
          begin
            Field_.State := SwapHeader.State;
            Result := False;
            exit;
          end;
        Field_.HeaderCount := Field_.HeaderCount + 1;
        Field_.LastHeaderPOS := ActiveHeader.CurrentHeader;
        Field_.RHeader.ModificationTime := umlDefaultTime;
        if dbField_WriteRec(Field_.RHeader.CurrentHeader, IOHnd, Field_) = False then
          begin
            Result := False;
            exit;
          end;
        if dbHeader_WriteRec(ActiveHeader.CurrentHeader, IOHnd, ActiveHeader) = False then
          begin
            Field_.State := ActiveHeader.State;
            Result := False;
            exit;
          end;
      end;
    else
      begin
        if dbHeader_ReadRec(Field_.FirstHeaderPOS, IOHnd, SwapHeader) = False then
          begin
            Field_.State := SwapHeader.State;
            Result := False;
            exit;
          end;
        SwapHeader.PrevHeader := ActiveHeader.CurrentHeader;
        SwapHeader.PositionID := DB_Header_First;
        ActiveHeader.NextHeader := SwapHeader.CurrentHeader;
        if dbHeader_WriteRec(SwapHeader.CurrentHeader, IOHnd, SwapHeader) = False then
          begin
            Field_.State := SwapHeader.State;
            Result := False;
            exit;
          end;

        if dbHeader_ReadRec(Field_.LastHeaderPOS, IOHnd, SwapHeader) = False then
          begin
            Field_.State := SwapHeader.State;
            Result := False;
            exit;
          end;
        SwapHeader.NextHeader := ActiveHeader.CurrentHeader;
        ActiveHeader.PrevHeader := SwapHeader.CurrentHeader;
        SwapHeader.PositionID := DB_Header_Medium;
        if dbHeader_WriteRec(SwapHeader.CurrentHeader, IOHnd, SwapHeader) = False then
          begin
            Field_.State := SwapHeader.State;
            Result := False;
            exit;
          end;
        Field_.HeaderCount := Field_.HeaderCount + 1;
        Field_.LastHeaderPOS := ActiveHeader.CurrentHeader;
        Field_.RHeader.ModificationTime := umlDefaultTime;
        ActiveHeader.PositionID := DB_Header_Last;
        if dbField_WriteRec(Field_.RHeader.CurrentHeader, IOHnd, Field_) = False then
          begin
            Result := False;
            exit;
          end;
        if dbHeader_WriteRec(ActiveHeader.CurrentHeader, IOHnd, ActiveHeader) = False then
          begin
            Field_.State := ActiveHeader.State;
            Result := False;
            exit;
          end;
      end;
  end;
  Field_.State := DB_Field_ok;
  Result := True;
end;

function dbField_CreateField(const Name: U_String; const fPos: Int64; var IOHnd: TIOHnd; var Field_: TField): Boolean;
begin
  if dbField_CreateHeader(Name, DB_Header_Field_ID, fPos, IOHnd, Field_.RHeader) = False then
    begin
      Field_.State := Field_.RHeader.State;
      Result := False;
      exit;
    end;

  Field_.HeaderCount := 0;
  Field_.UpFieldPOS := fPos;
  if dbField_OnlyWriteFieldRec(Field_.RHeader.DataPosition, IOHnd, Field_) = False then
    begin
      Result := False;
      exit;
    end;
  Field_.State := DB_Field_ok;
  Result := True;
end;

function dbField_InsertNewField(const Name: U_String; const FieldPos, CurrentInsertPos: Int64; var IOHnd: TIOHnd; var Field_: TField): Boolean;
begin
  if dbField_InsertNewHeader(Name, DB_Header_Field_ID, FieldPos, CurrentInsertPos, IOHnd, Field_.RHeader) = False then
    begin
      Field_.State := Field_.RHeader.State;
      Result := False;
      exit;
    end;

  Field_.HeaderCount := 0;
  Field_.UpFieldPOS := FieldPos;
  if dbField_OnlyWriteFieldRec(Field_.RHeader.DataPosition, IOHnd, Field_) = False then
    begin
      Result := False;
      exit;
    end;
  Field_.State := DB_Field_ok;
  Result := True;
end;

function dbField_CreateItem(const Name: U_String; const ExterID: Byte; const fPos: Int64; var IOHnd: TIOHnd; var Item_: TItem): Boolean;
begin
  if dbField_CreateHeader(Name, DB_Header_Item_ID, fPos, IOHnd, Item_.RHeader) = False then
    begin
      Item_.State := Item_.RHeader.State;
      Result := False;
      exit;
    end;

  Item_.ExtID := ExterID;
  Item_.FirstBlockPOS := 0;
  Item_.LastBlockPOS := 0;
  Item_.Size := 0;
  Item_.BlockCount := 0;
  Item_.RHeader.ModificationTime := umlDefaultTime;
  if dbItem_OnlyWriteItemRec(Item_.RHeader.DataPosition, IOHnd, Item_) = False then
    begin
      Result := False;
      exit;
    end;
  Item_.DataModification := True;
  Item_.State := DB_Item_ok;
  Result := True;
end;

function dbField_InsertNewItem(const Name: U_String; const ExterID: Byte; const FieldPos, CurrentInsertPos: Int64; var IOHnd: TIOHnd; var Item_: TItem): Boolean;
begin
  if dbField_InsertNewHeader(Name, DB_Header_Item_ID, FieldPos, CurrentInsertPos, IOHnd, Item_.RHeader) = False then
    begin
      Item_.State := Item_.RHeader.State;
      Result := False;
      exit;
    end;

  Item_.ExtID := ExterID;
  Item_.FirstBlockPOS := 0;
  Item_.LastBlockPOS := 0;
  Item_.Size := 0;
  Item_.BlockCount := 0;
  Item_.RHeader.ModificationTime := umlDefaultTime;
  if dbItem_OnlyWriteItemRec(Item_.RHeader.DataPosition, IOHnd, Item_) = False then
    begin
      Result := False;
      exit;
    end;
  Item_.DataModification := True;
  Item_.State := DB_Item_ok;
  Result := True;
end;

function dbField_CopyItem(var Item_: TItem; var IOHnd: TIOHnd; const DestFieldPos: Int64; var DestIOHnd: TIOHnd): Boolean;
var
  i: Integer;
  NewItemHnd: TItem;
  buff: array [0 .. C_MaxBufferFragmentSize] of Byte;
begin
  Init_TItem(NewItemHnd);
  NewItemHnd := Item_;
  if dbField_CreateItem(Item_.RHeader.Name, Item_.ExtID, DestFieldPos, DestIOHnd, NewItemHnd) = False then
    begin
      Item_.State := NewItemHnd.State;
      Result := False;
      exit;
    end;
  if dbItem_BlockSeekStartPOS(IOHnd, Item_) = False then
    begin
      Result := False;
      exit;
    end;
  if Item_.Size > C_MaxBufferFragmentSize then
    begin
      for i := 1 to (Item_.Size div C_MaxBufferFragmentSize) do
        begin
          if dbItem_BlockReadData(IOHnd, Item_, buff, C_MaxBufferFragmentSize) = False then
            begin
              Result := False;
              exit;
            end;
          if dbItem_BlockAppendWriteData(DestIOHnd, NewItemHnd, buff, C_MaxBufferFragmentSize) = False then
            begin
              Item_.State := NewItemHnd.State;
              Result := False;
              exit;
            end;
        end;
      if (Item_.Size mod C_MaxBufferFragmentSize) > 0 then
        begin
          if dbItem_BlockReadData(IOHnd, Item_, buff, Item_.Size mod C_MaxBufferFragmentSize) = False then
            begin
              Result := False;
              exit;
            end;
          if dbItem_BlockAppendWriteData(DestIOHnd, NewItemHnd, buff, Item_.Size mod C_MaxBufferFragmentSize) = False then
            begin
              Item_.State := NewItemHnd.State;
              Result := False;
              exit;
            end;
        end;
    end
  else
    begin
      if dbItem_BlockReadData(IOHnd, Item_, buff, Item_.Size) = False then
        begin
          Result := False;
          exit;
        end;
      if dbItem_BlockAppendWriteData(DestIOHnd, NewItemHnd, buff, Item_.Size) = False then
        begin
          Item_.State := NewItemHnd.State;
          Result := False;
          exit;
        end;
    end;
  if dbItem_WriteRec(NewItemHnd.RHeader.CurrentHeader, DestIOHnd, NewItemHnd) = False then
    begin
      Item_.State := NewItemHnd.State;
      Result := False;
      exit;
    end;
  Item_.State := DB_Item_ok;
  Result := True;
end;

function dbField_CopyItemBuffer(var Item_: TItem; var IOHnd: TIOHnd; var DestItem_: TItem; var DestIOHnd: TIOHnd): Boolean;
var
  i: Integer;
  buff: array [0 .. C_MaxBufferFragmentSize] of Byte;
begin
  if dbItem_BlockSeekStartPOS(IOHnd, Item_) = False then
    begin
      Result := False;
      exit;
    end;
  if Item_.Size > C_MaxBufferFragmentSize then
    begin
      for i := 1 to (Item_.Size div C_MaxBufferFragmentSize) do
        begin
          if dbItem_BlockReadData(IOHnd, Item_, buff, C_MaxBufferFragmentSize) = False then
            begin
              Result := False;
              exit;
            end;
          if dbItem_BlockAppendWriteData(DestIOHnd, DestItem_, buff, C_MaxBufferFragmentSize) = False then
            begin
              Item_.State := DestItem_.State;
              Result := False;
              exit;
            end;
        end;
      if (Item_.Size mod C_MaxBufferFragmentSize) > 0 then
        begin
          if dbItem_BlockReadData(IOHnd, Item_, buff, Item_.Size mod C_MaxBufferFragmentSize) = False then
            begin
              Result := False;
              exit;
            end;
          if dbItem_BlockAppendWriteData(DestIOHnd, DestItem_, buff, Item_.Size mod C_MaxBufferFragmentSize) = False then
            begin
              Item_.State := DestItem_.State;
              Result := False;
              exit;
            end;
        end;
    end
  else
    begin
      if dbItem_BlockReadData(IOHnd, Item_, buff, Item_.Size) = False then
        begin
          Result := False;
          exit;
        end;
      if dbItem_BlockAppendWriteData(DestIOHnd, DestItem_, buff, Item_.Size) = False then
        begin
          Item_.State := DestItem_.State;
          Result := False;
          exit;
        end;
    end;

  // fixed by qq600585,2018-12
  // Header has a certain chance of being changed by other item operation during the opening of item
  if dbHeader_ReadReservedRec(DestItem_.RHeader.CurrentHeader, DestIOHnd, DestItem_.RHeader) = False then
    begin
      Item_.State := DestItem_.State;
      Result := False;
      exit;
    end;
  if dbItem_WriteRec(DestItem_.RHeader.CurrentHeader, DestIOHnd, DestItem_) = False then
    begin
      Item_.State := DestItem_.State;
      Result := False;
      exit;
    end;
  Item_.State := DB_Item_ok;
  Result := True;
end;

function dbField_CopyAllTo(const FilterName: U_String; const FieldPos: Int64; var IOHnd: TIOHnd; const DestFieldPos: Int64; var DestIOHnd: TIOHnd): Boolean;
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
          DB_Header_Field_ID:
            begin
              Init_TField(NewField);
              if dbField_ReadRec(fs.RHeader.CurrentHeader, IOHnd, NewField) then
                if dbField_CreateField(fs.RHeader.Name, DestFieldPos, DestIOHnd, NewField) then
                    dbField_CopyAllTo(FilterName, fs.RHeader.CurrentHeader, IOHnd, NewField.RHeader.CurrentHeader, DestIOHnd);
            end;
          DB_Header_Item_ID:
            begin
              if dbItem_ReadRec(fs.RHeader.CurrentHeader, IOHnd, NewItem) then
                begin
                  dbField_CopyItem(NewItem, IOHnd, DestFieldPos, DestIOHnd);
                end;
            end;
        end;
      until not dbField_OnlyFindNextName(IOHnd, fs);
    end;
  Result := True;
end;

function db_CreateNew(const FileName: U_String; var DB_: TObjectDataHandle): Boolean;
begin
  if umlFileTest(DB_.IOHnd) then
    begin
      DB_.State := DB_RepCreatePackError;
      Result := False;
      exit;
    end;
  if umlFileCreate(FileName, DB_.IOHnd) = False then
    begin
      DB_.State := DB_CreatePackError;
      Result := False;
      exit;
    end;
  FillPtrByte(@DB_.ReservedData[0], DB_ReservedData_Size, 0);
  DB_.FixedStringL := DB_.IOHnd.FixedStringL;
  DB_.MajorVer := DB_MajorVersion;
  DB_.MinorVer := DB_MinorVersion;
  DB_.CreateTime := umlDefaultTime;
  DB_.ModificationTime := DB_.CreateTime;
  DB_.RootHeaderCount := 0;
  DB_.DefaultFieldPOS := Get_DB_L(DB_.IOHnd);
  DB_.LastHeaderPOS := DB_.DefaultFieldPOS;
  DB_.FirstHeaderPOS := DB_.DefaultFieldPOS;
  DB_.CurrentFieldPOS := DB_.DefaultFieldPOS;
  if db_WriteRec(0, DB_.IOHnd, DB_) = False then
    begin
      Result := False;
      exit;
    end;
  if db_CreateAndSetRootField(DB_DefaultField, DB_FileDescription, DB_) = False then
    begin
      Result := False;
      exit;
    end;
  DB_.State := DB_ok;
  Result := True;
end;

function db_Open(const FileName: U_String; var DB_: TObjectDataHandle; _OnlyRead: Boolean): Boolean;
begin
  if umlFileTest(DB_.IOHnd) then
    begin
      DB_.State := DB_RepOpenPackError;
      Result := False;
      exit;
    end;

  if umlFileOpen(FileName, DB_.IOHnd, _OnlyRead) = False then
    begin
      DB_.State := DB_OpenPackError;
      Result := False;
      exit;
    end;

  if DB_.IOHnd.Size = 0 then
    begin
      FillPtrByte(@DB_.ReservedData[0], DB_ReservedData_Size, 0);
      DB_.FixedStringL := DB_.IOHnd.FixedStringL;
      DB_.MajorVer := DB_MajorVersion;
      DB_.MinorVer := DB_MinorVersion;
      DB_.CreateTime := umlDefaultTime;
      DB_.ModificationTime := DB_.CreateTime;
      DB_.RootHeaderCount := 0;
      DB_.DefaultFieldPOS := Get_DB_L(DB_.IOHnd);
      DB_.LastHeaderPOS := DB_.DefaultFieldPOS;
      DB_.FirstHeaderPOS := DB_.DefaultFieldPOS;
      DB_.CurrentFieldPOS := DB_.DefaultFieldPOS;
      if db_WriteRec(0, DB_.IOHnd, DB_) = False then
        begin
          Result := False;
          exit;
        end;
      if db_CreateAndSetRootField(DB_DefaultField, DB_FileDescription, DB_) = False then
        begin
          Result := False;
          exit;
        end;
      DB_.State := DB_ok;
      Result := True;
      exit;
    end;

  if db_ReadRec(0, DB_.IOHnd, DB_) = False then
    begin
      Result := False;
      exit;
    end;
  if DB_.FixedStringL = 0 then
      DB_.FixedStringL := DB_.IOHnd.FixedStringL;
  DB_.IOHnd.FixedStringL := DB_.FixedStringL;

  DB_.State := DB_ok;
  Result := True;
end;

function db_CreateAsStream(stream: U_Stream; const Name, Description: U_String; var DB_: TObjectDataHandle): Boolean;
begin
  if umlFileTest(DB_.IOHnd) then
    begin
      DB_.State := DB_RepCreatePackError;
      Result := False;
      exit;
    end;
  if umlFileCreateAsStream(Name, stream, DB_.IOHnd) = False then
    begin
      DB_.State := DB_CreatePackError;
      Result := False;
      exit;
    end;
  FillPtrByte(@DB_.ReservedData[0], DB_ReservedData_Size, 0);
  DB_.FixedStringL := DB_.IOHnd.FixedStringL;
  DB_.MajorVer := DB_MajorVersion;
  DB_.MinorVer := DB_MinorVersion;
  DB_.CreateTime := umlDefaultTime;
  DB_.ModificationTime := DB_.CreateTime;
  DB_.RootHeaderCount := 0;
  DB_.DefaultFieldPOS := Get_DB_L(DB_.IOHnd);
  DB_.LastHeaderPOS := DB_.DefaultFieldPOS;
  DB_.FirstHeaderPOS := DB_.DefaultFieldPOS;
  DB_.CurrentFieldPOS := DB_.DefaultFieldPOS;
  if db_WriteRec(0, DB_.IOHnd, DB_) = False then
    begin
      Result := False;
      exit;
    end;
  if db_CreateAndSetRootField(DB_DefaultField, DB_FileDescription, DB_) = False then
    begin
      Result := False;
      exit;
    end;
  DB_.State := DB_ok;
  Result := True;
end;

function db_OpenAsStream(stream: U_Stream; const Name: U_String; var DB_: TObjectDataHandle; _OnlyRead: Boolean): Boolean;
begin
  if umlFileTest(DB_.IOHnd) then
    begin
      DB_.State := DB_RepOpenPackError;
      Result := False;
      exit;
    end;

  if umlFileOpenAsStream(Name, stream, DB_.IOHnd, _OnlyRead) = False then
    begin
      DB_.State := DB_OpenPackError;
      Result := False;
      exit;
    end;

  if db_ReadRec(0, DB_.IOHnd, DB_) = False then
    begin
      Result := False;
      exit;
    end;
  if DB_.FixedStringL = 0 then
      DB_.FixedStringL := DB_.IOHnd.FixedStringL;
  DB_.IOHnd.FixedStringL := DB_.FixedStringL;

  DB_.State := DB_ok;
  Result := True;
end;

function db_ClosePack(var DB_: TObjectDataHandle): Boolean;
begin
  if umlFileTest(DB_.IOHnd) = False then
    begin
      DB_.State := DB_ClosePackError;
      Result := False;
      exit;
    end;
  if DB_.IOHnd.WriteStated then
    begin
      DB_.ModificationTime := umlDefaultTime;
      if db_WriteRec(0, DB_.IOHnd, DB_) = False then
        begin
          Result := False;
          exit;
        end;
    end;
  if umlFileClose(DB_.IOHnd) = False then
    begin
      DB_.State := DB_ClosePackError;
      Result := False;
      exit;
    end;
  DB_.State := DB_ok;
  Result := True;
end;

function db_Update(var DB_: TObjectDataHandle): Boolean;
begin
  if umlFileTest(DB_.IOHnd) = False then
    begin
      DB_.State := DB_ClosePackError;
      Result := False;
      exit;
    end;
  if DB_.IOHnd.WriteStated then
    begin
      DB_.ModificationTime := umlDefaultTime;
      if db_WriteRec(0, DB_.IOHnd, DB_) = False then
        begin
          Result := False;
          exit;
        end;
    end;
  DB_.State := DB_ok;
  Result := umlFileUpdate(DB_.IOHnd);
end;

function db_CopyFieldTo(const FilterName: U_String; var DB_: TObjectDataHandle; const SourceFieldPos: Int64; var DestTMDB: TObjectDataHandle; const DestFieldPos: Int64): Boolean;
begin
  if dbField_CopyAllTo(FilterName, SourceFieldPos, DB_.IOHnd, DestFieldPos, DestTMDB.IOHnd) then
    begin
      DB_.State := DB_ok;
      DestTMDB.State := DB_ok;
      Result := True;
    end
  else
    begin
      DB_.State := DB_CreatePackError;
      DestTMDB.State := DB_CreatePackError;
      Result := False;
    end;
end;

function db_CopyAllTo(var DB_: TObjectDataHandle; var DestTMDB: TObjectDataHandle): Boolean;
begin
  Result := db_CopyFieldTo('*', DB_, DB_.DefaultFieldPOS, DestTMDB, DestTMDB.DefaultFieldPOS);
end;

function db_CopyAllToDestPath(var DB_: TObjectDataHandle; var DestTMDB: TObjectDataHandle; destPath: U_String): Boolean;
var
  f: TField;
begin
  Result := False;
  db_CreateField(destPath, '', DestTMDB);
  if db_GetField(destPath, f, DestTMDB) then
    begin
      Result := db_CopyFieldTo('*', DB_, DB_.DefaultFieldPOS, DestTMDB, f.RHeader.CurrentHeader);
    end;
end;

function db_TestName(const Name: U_String): Boolean;
begin
  Result := umlDeleteChar(Name, db_FieldPathLimitChar + #9#32#13#10).Len > 0;
end;

function db_CheckRootField(const Name: U_String; var Field_: TField; var DB_: TObjectDataHandle): Boolean;
var
  f: TField;
begin
  if db_TestName(Name) = False then
    begin
      DB_.State := DB_PathNameError;
      Field_.State := DB_.State;
      Result := False;
      exit;
    end;

  if db_GetRootField(Name, f, DB_) = False then
    begin
      if db_CreateRootHeader(Name, DB_Header_Field_ID, DB_, Field_.RHeader) = False then
        begin
          DB_.State := Field_.RHeader.State;
          Field_.State := DB_.State;
          Result := False;
          exit;
        end;
      Field_.HeaderCount := 0;
      Field_.UpFieldPOS := -1;
      if dbField_OnlyWriteFieldRec(Field_.RHeader.DataPosition, DB_.IOHnd, Field_) = False then
        begin
          DB_.State := Field_.State;
          Field_.State := DB_.State;
          Result := False;
          exit;
        end;
    end
  else
    begin
      Field_ := f;
    end;
  DB_.State := DB_ok;
  Result := True;
end;

function db_CreateRootHeader(const Name: U_String; const ID: Byte; var DB_: TObjectDataHandle; var Header_: THeader): Boolean;
var
  Header: THeader;
begin
  Header_.ID := ID;
  Header_.Name := Name;
  case DB_.RootHeaderCount of
    0:
      begin
        DB_.RootHeaderCount := 1;
        DB_.FirstHeaderPOS := umlFileGetSize(DB_.IOHnd);
        DB_.LastHeaderPOS := DB_.FirstHeaderPOS;
        DB_.ModificationTime := umlDefaultTime;
        Header_.PositionID := DB_Header_1;
        Header_.NextHeader := DB_.LastHeaderPOS;
        Header_.PrevHeader := DB_.FirstHeaderPOS;
        Header_.CurrentHeader := DB_.FirstHeaderPOS;
        Header_.CreateTime := umlDefaultTime;
        Header_.ModificationTime := umlDefaultTime;
        Header_.DataPosition := Header_.CurrentHeader + Get_DB_HeaderL(DB_.IOHnd);
        if dbHeader_WriteRec(Header_.CurrentHeader, DB_.IOHnd, Header_) = False then
          begin
            Result := False;
            exit;
          end;
      end;
    1:
      begin
        Header_.CurrentHeader := umlFileGetSize(DB_.IOHnd);
        Header_.NextHeader := DB_.FirstHeaderPOS;
        Header_.PrevHeader := DB_.FirstHeaderPOS;
        if dbHeader_ReadRec(DB_.FirstHeaderPOS, DB_.IOHnd, Header) = False then
          begin
            Header_.State := Header.State;
            Result := False;
            exit;
          end;
        Header.PrevHeader := Header_.CurrentHeader;
        Header.NextHeader := Header_.CurrentHeader;
        Header.PositionID := DB_Header_First;
        if dbHeader_WriteRec(DB_.FirstHeaderPOS, DB_.IOHnd, Header) = False then
          begin
            Header_.State := Header.State;
            Result := False;
            exit;
          end;
        DB_.RootHeaderCount := DB_.RootHeaderCount + 1;
        DB_.LastHeaderPOS := Header_.CurrentHeader;
        DB_.ModificationTime := umlDefaultTime;
        Header_.CreateTime := umlDefaultTime;
        Header_.ModificationTime := umlDefaultTime;
        Header_.DataPosition := Header_.CurrentHeader + Get_DB_HeaderL(DB_.IOHnd);
        Header_.PositionID := DB_Header_Last;
        if dbHeader_WriteRec(Header_.CurrentHeader, DB_.IOHnd, Header_) = False then
          begin
            Result := False;
            exit;
          end;
      end;
    else
      begin
        Header_.CurrentHeader := umlFileGetSize(DB_.IOHnd);
        if dbHeader_ReadRec(DB_.FirstHeaderPOS, DB_.IOHnd, Header) = False then
          begin
            Header_.State := Header.State;
            Result := False;
            exit;
          end;
        Header.PrevHeader := Header_.CurrentHeader;
        Header_.NextHeader := Header.CurrentHeader;
        if dbHeader_WriteRec(DB_.FirstHeaderPOS, DB_.IOHnd, Header) = False then
          begin
            Header_.State := Header.State;
            Result := False;
            exit;
          end;
        if dbHeader_ReadRec(DB_.LastHeaderPOS, DB_.IOHnd, Header) = False then
          begin
            Header_.State := Header.State;
            Result := False;
            exit;
          end;
        Header.NextHeader := Header_.CurrentHeader;
        Header_.PrevHeader := DB_.LastHeaderPOS;
        Header.PositionID := DB_Header_Medium;
        if dbHeader_WriteRec(DB_.LastHeaderPOS, DB_.IOHnd, Header) = False then
          begin
            Header_.State := Header.State;
            Result := False;
            exit;
          end;
        DB_.RootHeaderCount := DB_.RootHeaderCount + 1;
        DB_.LastHeaderPOS := Header_.CurrentHeader;
        DB_.ModificationTime := umlDefaultTime;
        Header_.CreateTime := umlDefaultTime;
        Header_.ModificationTime := umlDefaultTime;
        Header_.DataPosition := Header_.CurrentHeader + Get_DB_HeaderL(DB_.IOHnd);
        Header_.PositionID := DB_Header_Last;
        if dbHeader_WriteRec(Header_.CurrentHeader, DB_.IOHnd, Header_) = False then
          begin
            Result := False;
            exit;
          end;
      end;
  end;

  Header_.State := DB_Header_ok;
  Result := True;
end;

function db_CreateRootField(const Name, Description: U_String; var DB_: TObjectDataHandle): Boolean;
var
  f: TField;
begin
  if db_TestName(Name) = False then
    begin
      DB_.State := DB_PathNameError;
      Result := False;
      exit;
    end;

  if db_ExistsRootField(Name, DB_) then
    begin
      DB_.State := DB_PathNameError;
      Result := False;
      exit;
    end;
  if db_CreateRootHeader(Name, DB_Header_Field_ID, DB_, f.RHeader) = False then
    begin
      DB_.State := f.RHeader.State;
      Result := False;
      exit;
    end;
  f.Description := Description;
  f.HeaderCount := 0;
  f.UpFieldPOS := -1;
  if dbField_OnlyWriteFieldRec(f.RHeader.DataPosition, DB_.IOHnd, f) = False then
    begin
      DB_.State := f.State;
      Result := False;
      exit;
    end;
  DB_.State := DB_ok;
  Result := True;
end;

function db_CreateAndSetRootField(const Name, Description: U_String; var DB_: TObjectDataHandle): Boolean;
var
  f: TField;
begin
  if db_TestName(Name) = False then
    begin
      DB_.State := DB_PathNameError;
      Result := False;
      exit;
    end;

  if db_ExistsRootField(Name, DB_) then
    begin
      DB_.State := DB_PathNameError;
      Result := False;
      exit;
    end;
  if db_CreateRootHeader(Name, DB_Header_Field_ID, DB_, f.RHeader) = False then
    begin
      DB_.State := f.RHeader.State;
      Result := False;
      exit;
    end;
  f.Description := Description;
  f.HeaderCount := 0;
  f.UpFieldPOS := -1;
  if dbField_OnlyWriteFieldRec(f.RHeader.DataPosition, DB_.IOHnd, f) = False then
    begin
      DB_.State := f.State;
      Result := False;
      exit;
    end;
  DB_.DefaultFieldPOS := f.RHeader.CurrentHeader;
  DB_.State := DB_ok;
  Result := True;
end;

function db_CreateField(const pathName, Description: U_String; var DB_: TObjectDataHandle): Boolean;
var
  f: TField;
  fs: TFieldSearch;
  i, PC: Integer;
  TempPathStr, TempPathName: U_String;
begin
  if umlFileTest(DB_.IOHnd) = False then
    begin
      DB_.State := DB_ClosePackError;
      Result := False;
      exit;
    end;
  if dbField_ReadRec(DB_.DefaultFieldPOS, DB_.IOHnd, f) = False then
    begin
      DB_.State := f.State;
      Result := False;
      exit;
    end;

  if umlGetLength(pathName) = 0 then
    begin
      DB_.State := DB_ok;
      Result := True;
      exit;
    end;
  TempPathName := pathName;
  PC := db_GetPathCount(TempPathName);
  if PC > 0 then
    begin
      for i := 1 to PC do
        begin
          TempPathStr := db_GetFirstPath(TempPathName);
          TempPathName := db_DeleteFirstPath(TempPathName);

          if db_TestName(TempPathStr) = False then
            begin
              DB_.State := DB_PathNameError;
              Result := False;
              exit;
            end;
          case dbField_FindFirst(TempPathStr, DB_Header_Field_ID, f.RHeader.CurrentHeader, DB_.IOHnd, fs) of
            False:
              begin
                f.Description := Description;
                if dbField_CreateField(TempPathStr, f.RHeader.CurrentHeader, DB_.IOHnd, f) = False then
                  begin
                    DB_.State := f.State;
                    Result := False;
                    exit;
                  end;
              end;
            True:
              begin
                if dbField_ReadRec(fs.RHeader.CurrentHeader, DB_.IOHnd, f) = False then
                  begin
                    DB_.State := f.State;
                    Result := False;
                    exit;
                  end;
              end;
          end;
        end;
    end;
  DB_.State := DB_ok;
  Result := True;
end;

function db_SetFieldName(const pathName, OriginFieldName, NewFieldName, FieldDescription: U_String; var DB_: TObjectDataHandle): Boolean;
var
  TempSR: TFieldSearch;
  f: TField;
  OriginField: TField;
begin
  if db_GetField(pathName, f, DB_) = False then
    begin
      Result := False;
      exit;
    end;
  if dbField_FindFirst(OriginFieldName, DB_Header_Field_ID, f.RHeader.CurrentHeader, DB_.IOHnd, TempSR) = False then
    begin
      DB_.State := TempSR.State;
      Result := False;
      exit;
    end;
  if dbField_ReadRec(TempSR.RHeader.CurrentHeader, DB_.IOHnd, OriginField) = False then
    begin
      DB_.State := OriginField.RHeader.State;
      Result := False;
      exit;
    end;
  OriginField.RHeader.Name := NewFieldName;
  OriginField.Description := FieldDescription;
  if dbField_WriteRec(OriginField.RHeader.CurrentHeader, DB_.IOHnd, OriginField) = False then
    begin
      DB_.State := OriginField.RHeader.State;
      Result := False;
      exit;
    end;
  DB_.State := DB_ok;
  Result := True;
end;

function db_SetItemName(const pathName, OriginItemName, NewItemName, ItemDescription: U_String; var DB_: TObjectDataHandle): Boolean;
var
  TempSR: TFieldSearch;
  f: TField;
  OriginItem: TItem;
begin
  if db_GetField(pathName, f, DB_) = False then
    begin
      Result := False;
      exit;
    end;
  if dbField_FindFirst(OriginItemName, DB_Header_Item_ID, f.RHeader.CurrentHeader, DB_.IOHnd, TempSR) = False then
    begin
      DB_.State := TempSR.State;
      Result := False;
      exit;
    end;
  if dbItem_ReadRec(TempSR.RHeader.CurrentHeader, DB_.IOHnd, OriginItem) = False then
    begin
      DB_.State := OriginItem.RHeader.State;
      Result := False;
      exit;
    end;
  OriginItem.RHeader.Name := NewItemName;
  OriginItem.Description := ItemDescription;
  if dbItem_WriteRec(OriginItem.RHeader.CurrentHeader, DB_.IOHnd, OriginItem) = False then
    begin
      DB_.State := OriginItem.RHeader.State;
      Result := False;
      exit;
    end;
  DB_.State := DB_ok;
  Result := True;
end;

function db_DeleteField(const pathName, FilterName: U_String; var DB_: TObjectDataHandle): Boolean;
var
  TempSR: TFieldSearch;
  f: TField;
begin
  if db_GetField(pathName, f, DB_) = False then
    begin
      Result := False;
      exit;
    end;
  if dbField_FindFirst(FilterName, DB_Header_Field_ID, f.RHeader.CurrentHeader, DB_.IOHnd, TempSR) = False then
    begin
      DB_.State := TempSR.State;
      Result := False;
      exit;
    end;
  if dbField_DeleteHeader(TempSR.RHeader.CurrentHeader, f.RHeader.CurrentHeader, DB_.IOHnd, f) = False then
    begin
      DB_.State := f.State;
      Result := False;
      exit;
    end;
  while dbField_FindFirst(FilterName, DB_Header_Field_ID, f.RHeader.CurrentHeader, DB_.IOHnd, TempSR) do
    begin
      if dbField_DeleteHeader(TempSR.RHeader.CurrentHeader, f.RHeader.CurrentHeader, DB_.IOHnd, f) = False then
        begin
          DB_.State := f.State;
          Result := False;
          exit;
        end;
    end;
  DB_.State := DB_ok;
  Result := True;
end;

function db_DeleteHeader(const pathName, FilterName: U_String; const ID: Byte; var DB_: TObjectDataHandle): Boolean;
var
  TempSR: TFieldSearch;
  f: TField;
begin
  if db_GetField(pathName, f, DB_) = False then
    begin
      Result := False;
      exit;
    end;
  if dbField_FindFirst(FilterName, ID, f.RHeader.CurrentHeader, DB_.IOHnd, TempSR) = False then
    begin
      DB_.State := TempSR.State;
      Result := False;
      exit;
    end;
  if dbField_DeleteHeader(TempSR.RHeader.CurrentHeader, f.RHeader.CurrentHeader, DB_.IOHnd, f) = False then
    begin
      DB_.State := f.State;
      Result := False;
      exit;
    end;
  while dbField_FindFirst(FilterName, ID, f.RHeader.CurrentHeader, DB_.IOHnd, TempSR) do
    begin
      if dbField_DeleteHeader(TempSR.RHeader.CurrentHeader, f.RHeader.CurrentHeader, DB_.IOHnd, f) = False then
        begin
          DB_.State := f.State;
          Result := False;
          exit;
        end;
    end;
  DB_.State := DB_ok;
  Result := True;
end;

function db_MoveItem(const SourcerPathName, FilterName: U_String; const TargetPathName: U_String; const ItemExtID: Byte; var DB_: TObjectDataHandle): Boolean;
var
  TempSR: TFieldSearch;
  SourcerField, TargetField: TField;
begin
  if db_GetField(SourcerPathName, SourcerField, DB_) = False then
    begin
      Result := False;
      exit;
    end;
  if db_GetField(TargetPathName, TargetField, DB_) = False then
    begin
      Result := False;
      exit;
    end;

  if SourcerField.RHeader.CurrentHeader = TargetField.RHeader.CurrentHeader then
    begin
      DB_.State := DB_PathNameError;
      Result := False;
      exit;
    end;
  if dbField_FindFirstItem(FilterName, ItemExtID, SourcerField.RHeader.CurrentHeader, DB_.IOHnd, TempSR) = False then
    begin
      DB_.State := TempSR.State;
      Result := False;
      exit;
    end;

  if TempSR.RHeader.CurrentHeader = TargetField.RHeader.CurrentHeader then
    begin
      DB_.State := DB_PathNameError;
      Result := False;
      exit;
    end;
  if dbField_MoveHeader(TempSR.RHeader.CurrentHeader, SourcerField.RHeader.CurrentHeader, TargetField.RHeader.CurrentHeader, DB_.IOHnd, TargetField) = False then
    begin
      DB_.State := TargetField.State;
      Result := False;
      exit;
    end;
  while dbField_FindFirstItem(FilterName, ItemExtID, SourcerField.RHeader.CurrentHeader, DB_.IOHnd, TempSR) do
    begin
      if TempSR.RHeader.CurrentHeader = TargetField.RHeader.CurrentHeader then
        begin
          DB_.State := DB_PathNameError;
          Result := False;
          exit;
        end;
      if dbField_MoveHeader(TempSR.RHeader.CurrentHeader, SourcerField.RHeader.CurrentHeader, TargetField.RHeader.CurrentHeader, DB_.IOHnd, TargetField) = False
      then
        begin
          DB_.State := TargetField.State;
          Result := False;
          exit;
        end;
    end;
  DB_.State := DB_ok;
  Result := True;
end;

function db_MoveField(const SourcerPathName, FilterName: U_String; const TargetPathName: U_String; var DB_: TObjectDataHandle): Boolean;
var
  TempSR: TFieldSearch;
  SourcerField, TargetField: TField;
begin
  if db_GetField(SourcerPathName, SourcerField, DB_) = False then
    begin
      Result := False;
      exit;
    end;
  if db_GetField(TargetPathName, TargetField, DB_) = False then
    begin
      Result := False;
      exit;
    end;

  if SourcerField.RHeader.CurrentHeader = TargetField.RHeader.CurrentHeader then
    begin
      DB_.State := DB_PathNameError;
      Result := False;
      exit;
    end;
  if dbField_FindFirst(FilterName, DB_Header_Field_ID, SourcerField.RHeader.CurrentHeader, DB_.IOHnd, TempSR) = False then
    begin
      DB_.State := TempSR.State;
      Result := False;
      exit;
    end;

  if TempSR.RHeader.CurrentHeader = TargetField.RHeader.CurrentHeader then
    begin
      DB_.State := DB_PathNameError;
      Result := False;
      exit;
    end;
  if dbField_MoveHeader(TempSR.RHeader.CurrentHeader, SourcerField.RHeader.CurrentHeader, TargetField.RHeader.CurrentHeader, DB_.IOHnd, TargetField) = False then
    begin
      DB_.State := TargetField.State;
      Result := False;
      exit;
    end;
  while dbField_FindFirst(FilterName, DB_Header_Field_ID, SourcerField.RHeader.CurrentHeader, DB_.IOHnd, TempSR) do
    begin
      if TempSR.RHeader.CurrentHeader = TargetField.RHeader.CurrentHeader then
        begin
          DB_.State := DB_PathNameError;
          Result := False;
          exit;
        end;
      if dbField_MoveHeader(TempSR.RHeader.CurrentHeader, SourcerField.RHeader.CurrentHeader, TargetField.RHeader.CurrentHeader, DB_.IOHnd, TargetField) = False
      then
        begin
          DB_.State := TargetField.State;
          Result := False;
          exit;
        end;
    end;
  DB_.State := DB_ok;
  Result := True;
end;

function db_MoveHeader(const SourcerPathName, FilterName: U_String; const TargetPathName: U_String; const HeaderID: Byte; var DB_: TObjectDataHandle): Boolean;
var
  TempSR: TFieldSearch;
  SourcerField, TargetField: TField;
begin
  if db_GetField(SourcerPathName, SourcerField, DB_) = False then
    begin
      Result := False;
      exit;
    end;
  if db_GetField(TargetPathName, TargetField, DB_) = False then
    begin
      Result := False;
      exit;
    end;

  if SourcerField.RHeader.CurrentHeader = TargetField.RHeader.CurrentHeader then
    begin
      DB_.State := DB_PathNameError;
      Result := False;
      exit;
    end;
  if dbField_FindFirst(FilterName, HeaderID, SourcerField.RHeader.CurrentHeader, DB_.IOHnd, TempSR) = False then
    begin
      DB_.State := TempSR.State;
      Result := False;
      exit;
    end;

  if TempSR.RHeader.CurrentHeader = TargetField.RHeader.CurrentHeader then
    begin
      DB_.State := DB_PathNameError;
      Result := False;
      exit;
    end;
  if dbField_MoveHeader(TempSR.RHeader.CurrentHeader, SourcerField.RHeader.CurrentHeader, TargetField.RHeader.CurrentHeader, DB_.IOHnd, TargetField) = False then
    begin
      DB_.State := TargetField.State;
      Result := False;
      exit;
    end;
  while dbField_FindFirst(FilterName, HeaderID, SourcerField.RHeader.CurrentHeader, DB_.IOHnd, TempSR) do
    begin
      if TempSR.RHeader.CurrentHeader = TargetField.RHeader.CurrentHeader then
        begin
          DB_.State := DB_PathNameError;
          Result := False;
          exit;
        end;
      if dbField_MoveHeader(TempSR.RHeader.CurrentHeader, SourcerField.RHeader.CurrentHeader, TargetField.RHeader.CurrentHeader, DB_.IOHnd, TargetField) = False
      then
        begin
          DB_.State := TargetField.State;
          Result := False;
          exit;
        end;
    end;
  DB_.State := DB_ok;
  Result := True;
end;

function db_SetCurrentRootField(const Name: U_String; var DB_: TObjectDataHandle): Boolean;
var
  f: TField;
begin
  if DB_.RootHeaderCount = 0 then
    begin
      DB_.State := DB_PathNameError;
      Result := False;
      exit;
    end;
  if dbHeader_ReadRec(DB_.DefaultFieldPOS, DB_.IOHnd, f.RHeader) = False then
    begin
      DB_.State := f.RHeader.State;
      Result := False;
      exit;
    end;

  if (dbMultipleMatch(Name, f.RHeader.Name) = True) and (f.RHeader.ID = DB_Header_Field_ID) then
    begin
      DB_.DefaultFieldPOS := f.RHeader.CurrentHeader;
      DB_.State := DB_ok;
      Result := True;
      exit;
    end;

  if DB_.RootHeaderCount = 1 then
    begin
      DB_.State := DB_PathNameError;
      Result := False;
      exit;
    end;
  if dbHeader_ReadRec(f.RHeader.NextHeader, DB_.IOHnd, f.RHeader) = False then
    begin
      DB_.State := f.RHeader.State;
      Result := False;
      exit;
    end;

  if (dbMultipleMatch(Name, f.RHeader.Name) = True) and (f.RHeader.ID = DB_Header_Field_ID) then
    begin
      DB_.DefaultFieldPOS := f.RHeader.CurrentHeader;
      DB_.State := DB_ok;
      Result := True;
      exit;
    end;

  while f.RHeader.CurrentHeader <> DB_.DefaultFieldPOS do
    begin
      if dbHeader_ReadRec(f.RHeader.NextHeader, DB_.IOHnd, f.RHeader) = False then
        begin
          DB_.State := f.RHeader.State;
          Result := False;
          exit;
        end;
      if (dbMultipleMatch(Name, f.RHeader.Name) = True) and (f.RHeader.ID = DB_Header_Field_ID) then
        begin
          DB_.DefaultFieldPOS := f.RHeader.CurrentHeader;
          DB_.State := DB_ok;
          Result := True;
          exit;
        end;
    end;
  DB_.State := DB_PathNameError;
  Result := False;
end;

function db_SetCurrentField(const pathName: U_String; var DB_: TObjectDataHandle): Boolean;
var
  f: TField;
  fs: TFieldSearch;
  i, PC: Integer;
  TempPathStr, TempPathName: U_String;
begin
  if umlFileTest(DB_.IOHnd) = False then
    begin
      DB_.State := DB_ClosePackError;
      Result := False;
      exit;
    end;
  if dbField_ReadRec(DB_.DefaultFieldPOS, DB_.IOHnd, f) = False then
    begin
      DB_.State := f.State;
      Result := False;
      exit;
    end;

  if umlGetLength(pathName) = 0 then
    begin
      DB_.CurrentFieldPOS := f.RHeader.CurrentHeader;
      DB_.CurrentFieldLevel := 1;
      DB_.State := DB_ok;
      Result := True;
      exit;
    end;
  TempPathName := pathName;
  PC := db_GetPathCount(TempPathName);
  if PC > 0 then
    begin
      for i := 1 to PC do
        begin
          TempPathStr := db_GetFirstPath(TempPathName);
          TempPathName := db_DeleteFirstPath(TempPathName);

          if db_TestName(TempPathStr) = False then
            begin
              DB_.State := DB_PathNameError;
              Result := False;
              exit;
            end;
          if dbField_FindFirst(TempPathStr, DB_Header_Field_ID, f.RHeader.CurrentHeader, DB_.IOHnd, fs) = False then
            begin
              DB_.State := fs.State;
              Result := False;
              exit;
            end;
          if dbField_ReadRec(fs.RHeader.CurrentHeader, DB_.IOHnd, f) = False then
            begin
              DB_.State := f.State;
              Result := False;
              exit;
            end;
        end;
    end;
  DB_.CurrentFieldPOS := f.RHeader.CurrentHeader;
  DB_.CurrentFieldLevel := PC;
  DB_.State := DB_ok;
  Result := True;
end;

function db_GetRootField(const Name: U_String; var Field_: TField; var DB_: TObjectDataHandle): Boolean;
var
  f: TField;
begin
  Init_TField(Field_);
  Init_TField(f);

  if DB_.RootHeaderCount = 0 then
    begin
      DB_.State := DB_PathNameError;
      Field_.State := DB_.State;
      Result := False;
      exit;
    end;
  if dbHeader_ReadRec(DB_.DefaultFieldPOS, DB_.IOHnd, f.RHeader) = False then
    begin
      DB_.State := f.RHeader.State;
      Field_.State := DB_.State;
      Result := False;
      exit;
    end;

  if (dbMultipleMatch(Name, f.RHeader.Name) = True) and (f.RHeader.ID = DB_Header_Field_ID) then
    begin
      DB_.State := DB_ok;
      Field_ := f;
      Result := True;
      exit;
    end;

  if DB_.RootHeaderCount = 1 then
    begin
      DB_.State := DB_PathNameError;
      Field_.State := DB_.State;
      Result := False;
      exit;
    end;
  if dbHeader_ReadRec(f.RHeader.NextHeader, DB_.IOHnd, f.RHeader) = False then
    begin
      DB_.State := f.RHeader.State;
      Field_.State := DB_.State;
      Result := False;
      exit;
    end;

  if (dbMultipleMatch(Name, f.RHeader.Name) = True) and (f.RHeader.ID = DB_Header_Field_ID) then
    begin
      DB_.State := DB_ok;
      Field_ := f;
      Result := True;
      exit;
    end;

  while f.RHeader.CurrentHeader <> DB_.DefaultFieldPOS do
    begin
      if dbHeader_ReadRec(f.RHeader.NextHeader, DB_.IOHnd, f.RHeader) = False then
        begin
          DB_.State := f.RHeader.State;
          Field_.State := DB_.State;
          Result := False;
          exit;
        end;
      if (dbMultipleMatch(Name, f.RHeader.Name) = True) and (f.RHeader.ID = DB_Header_Field_ID) then
        begin
          DB_.State := DB_ok;
          Field_ := f;
          Result := True;
          exit;
        end;
    end;
  DB_.State := DB_PathNameError;
  Field_.State := DB_.State;
  Result := False;
end;

function db_GetField(const pathName: U_String; var Field_: TField; var DB_: TObjectDataHandle): Boolean;
var
  f: TField;
  fs: TFieldSearch;
  i, PC: Integer;
  TempPathStr, TempPathName: U_String;
begin
  if umlFileTest(DB_.IOHnd) = False then
    begin
      DB_.State := DB_ClosePackError;
      Result := False;
      exit;
    end;
  if dbField_ReadRec(DB_.DefaultFieldPOS, DB_.IOHnd, f) = False then
    begin
      DB_.State := f.State;
      Result := False;
      exit;
    end;

  if umlGetLength(pathName) = 0 then
    begin
      Field_ := f;
      DB_.State := DB_ok;
      Result := True;
      exit;
    end;
  TempPathName := pathName;
  PC := db_GetPathCount(TempPathName);

  if PC > 0 then
    begin
      for i := 1 to PC do
        begin
          TempPathStr := db_GetFirstPath(TempPathName);
          TempPathName := db_DeleteFirstPath(TempPathName);

          if db_TestName(TempPathStr) = False then
            begin
              DB_.State := DB_PathNameError;
              Result := False;
              exit;
            end;
          if dbField_FindFirst(TempPathStr, DB_Header_Field_ID, f.RHeader.CurrentHeader, DB_.IOHnd, fs) = False then
            begin
              DB_.State := fs.State;
              Result := False;
              exit;
            end;
          if dbField_ReadRec(fs.RHeader.CurrentHeader, DB_.IOHnd, f) = False then
            begin
              DB_.State := f.State;
              Result := False;
              exit;
            end;
        end;
    end;
  Field_ := f;
  DB_.State := DB_ok;
  Result := True;
end;

function db_GetPath(const FieldPos, RootFieldPos: Int64; var DB_: TObjectDataHandle; var RetPath: U_String): Boolean;
var
  f: TField;
begin
  if dbHeader_ReadRec(FieldPos, DB_.IOHnd, f.RHeader) = False then
    begin
      DB_.State := f.RHeader.State;
      Result := False;
      exit;
    end;

  if f.RHeader.ID <> DB_Header_Field_ID then
    begin
      DB_.State := DB_Field_SetPosError;
      Result := False;
      exit;
    end;
  if dbField_OnlyReadFieldRec(f.RHeader.DataPosition, DB_.IOHnd, f) = False then
    begin
      DB_.State := f.State;
      Result := False;
      exit;
    end;

  if f.RHeader.CurrentHeader = RootFieldPos then
    begin
      RetPath := DB_Path_Delimiter;
      DB_.State := DB_ok;
      Result := True;
      exit;
    end;
  RetPath := f.RHeader.Name + DB_Path_Delimiter;

  while dbField_ReadRec(f.UpFieldPOS, DB_.IOHnd, f) do
    begin
      if f.RHeader.CurrentHeader = RootFieldPos then
        begin
          RetPath := DB_Path_Delimiter + RetPath;
          DB_.State := DB_ok;
          Result := True;
          exit;
        end;
      RetPath := f.RHeader.Name + DB_Path_Delimiter + RetPath;
    end;
  DB_.State := f.State;
  Result := False;
end;

function db_NewItem(const pathName, ItemName, ItemDescription: U_String; const ItemExtID: Byte; var Item_: TItem; var DB_: TObjectDataHandle): Boolean;
var
  f: TField;
  fs: TFieldSearch;
  i, PC: Integer;
  TempPathStr, TempPathName: U_String;
begin
  if umlFileTest(DB_.IOHnd) = False then
    begin
      DB_.State := DB_ClosePackError;
      Result := False;
      exit;
    end;
  if dbField_ReadRec(DB_.DefaultFieldPOS, DB_.IOHnd, f) = False then
    begin
      DB_.State := f.State;
      Result := False;
      exit;
    end;

  if umlGetLength(pathName) > 0 then
    begin
      TempPathName := pathName;
      PC := db_GetPathCount(TempPathName);
      if PC > 0 then
        begin
          for i := 1 to PC do
            begin
              TempPathStr := db_GetFirstPath(TempPathName);
              TempPathName := db_DeleteFirstPath(TempPathName);

              if db_TestName(TempPathStr) = False then
                begin
                  DB_.State := DB_PathNameError;
                  Result := False;
                  exit;
                end;
              case dbField_FindFirst(TempPathStr, DB_Header_Field_ID, f.RHeader.CurrentHeader, DB_.IOHnd, fs) of
                False:
                  begin
                    f.Description := DB_FileDescription;
                    if dbField_CreateField(TempPathStr, f.RHeader.CurrentHeader, DB_.IOHnd, f) = False then
                      begin
                        DB_.State := f.State;
                        Result := False;
                        exit;
                      end;
                  end;
                True:
                  begin
                    if dbField_ReadRec(fs.RHeader.CurrentHeader, DB_.IOHnd, f) = False then
                      begin
                        DB_.State := f.State;
                        Result := False;
                        exit;
                      end;
                  end;
              end;
            end;
        end;
    end;

  if db_TestName(ItemName) = False then
    begin
      DB_.State := DB_ItemNameError;
      Result := False;
      exit;
    end;
  if dbField_FindFirstItem(ItemName, ItemExtID, f.RHeader.CurrentHeader, DB_.IOHnd, fs) then
    begin
      if DB_.OverWriteItem = False then
        begin
          if DB_.AllowSameHeaderName = False then
            begin
              DB_.State := DB_RepeatCreateItemError;
              Result := False;
              exit;
            end;
        end
      else
        begin
          if dbField_DeleteHeader(fs.RHeader.CurrentHeader, f.RHeader.CurrentHeader, DB_.IOHnd, f) = False then
            begin
              DB_.State := f.State;
              Result := False;
              exit;
            end;
        end;
    end;
  Item_.Description := ItemDescription;
  if dbField_CreateItem(ItemName, ItemExtID, f.RHeader.CurrentHeader, DB_.IOHnd, Item_) = False then
    begin
      DB_.State := Item_.State;
      Result := False;
      exit;
    end;
  DB_.State := DB_ok;
  Result := True;
end;

function db_DeleteItem(const pathName, FilterName: U_String; const ItemExtID: Byte; var DB_: TObjectDataHandle): Boolean;
var
  TempSR: TFieldSearch;
  f: TField;
begin
  if db_GetField(pathName, f, DB_) = False then
    begin
      Result := False;
      exit;
    end;
  if dbField_FindFirstItem(FilterName, ItemExtID, f.RHeader.CurrentHeader, DB_.IOHnd, TempSR) = False then
    begin
      DB_.State := TempSR.State;
      Result := False;
      exit;
    end;
  if dbField_DeleteHeader(TempSR.RHeader.CurrentHeader, f.RHeader.CurrentHeader, DB_.IOHnd, f) = False then
    begin
      DB_.State := f.State;
      Result := False;
      exit;
    end;
  while dbField_FindFirstItem(FilterName, ItemExtID, f.RHeader.CurrentHeader, DB_.IOHnd, TempSR) do
    begin
      if dbField_DeleteHeader(TempSR.RHeader.CurrentHeader, f.RHeader.CurrentHeader, DB_.IOHnd, f) = False then
        begin
          DB_.State := f.State;
          Result := False;
          exit;
        end;
    end;
  DB_.State := DB_ok;
  Result := True;
end;

function db_GetItem(const pathName, ItemName: U_String; const ItemExtID: Byte; var Item_: TItem; var DB_: TObjectDataHandle): Boolean;
var
  f: TField;
  _FieldSR: TFieldSearch;
begin
  if db_GetField(pathName, f, DB_) = False then
    begin
      Result := False;
      exit;
    end;
  if dbField_FindFirstItem(ItemName, ItemExtID, f.RHeader.CurrentHeader, DB_.IOHnd, _FieldSR) = False then
    begin
      DB_.State := DB_OpenItemError;
      Result := False;
      exit;
    end;
  if dbItem_ReadRec(_FieldSR.RHeader.CurrentHeader, DB_.IOHnd, Item_) = False then
    begin
      DB_.State := Item_.State;
      Result := False;
      exit;
    end;
  DB_.State := DB_ok;
  Result := True;
end;

function db_ItemCreate(const pathName, ItemName, ItemDescription: U_String; const ItemExtID: Byte; var ItemHnd_: TItemHandle_; var DB_: TObjectDataHandle): Boolean;
begin
  if ItemHnd_.OpenFlags then
    begin
      DB_.State := DB_RepeatCreateItemError;
      Result := False;
      exit;
    end;
  if db_NewItem(pathName, ItemName, ItemDescription, ItemExtID, ItemHnd_.Item, DB_) = False then
    begin
      Result := False;
      exit;
    end;
  if dbItem_BlockInit(DB_.IOHnd, ItemHnd_.Item) = False then
    begin
      DB_.State := ItemHnd_.Item.State;
      Result := False;
      exit;
    end;
  ItemHnd_.Name := ItemName;
  ItemHnd_.Description := ItemDescription;
  ItemHnd_.CreateTime := ItemHnd_.Item.RHeader.CreateTime;
  ItemHnd_.ModificationTime := ItemHnd_.Item.RHeader.ModificationTime;
  ItemHnd_.ItemExtID := ItemExtID;
  ItemHnd_.OpenFlags := True;
  DB_.State := DB_ok;
  Result := True;
end;

function db_ItemFastCreate(const ItemName, ItemDescription: U_String; const fPos: Int64; const ItemExtID: Byte; var ItemHnd_: TItemHandle_;
  var DB_: TObjectDataHandle): Boolean;
begin
  if ItemHnd_.OpenFlags then
    begin
      DB_.State := DB_RepeatCreateItemError;
      Result := False;
      exit;
    end;
  if dbField_CreateItem(ItemName, ItemExtID, fPos, DB_.IOHnd, ItemHnd_.Item) = False then
    begin
      Result := False;
      exit;
    end;
  if dbItem_BlockInit(DB_.IOHnd, ItemHnd_.Item) = False then
    begin
      DB_.State := ItemHnd_.Item.State;
      Result := False;
      exit;
    end;
  ItemHnd_.Name := ItemName;
  ItemHnd_.Description := ItemDescription;
  ItemHnd_.CreateTime := ItemHnd_.Item.RHeader.CreateTime;
  ItemHnd_.ModificationTime := ItemHnd_.Item.RHeader.ModificationTime;
  ItemHnd_.ItemExtID := ItemExtID;
  ItemHnd_.OpenFlags := True;
  DB_.State := DB_ok;
  Result := True;
end;

function db_ItemFastInsertNew(const ItemName, ItemDescription: U_String; const FieldPos, InsertHeaderPos: Int64; const ItemExtID: Byte; var ItemHnd_: TItemHandle_; var DB_: TObjectDataHandle): Boolean;
begin
  if ItemHnd_.OpenFlags then
    begin
      DB_.State := DB_RepeatCreateItemError;
      Result := False;
      exit;
    end;
  if dbField_InsertNewItem(ItemName, ItemExtID, FieldPos, InsertHeaderPos, DB_.IOHnd, ItemHnd_.Item) = False then
    begin
      Result := False;
      exit;
    end;
  if dbItem_BlockInit(DB_.IOHnd, ItemHnd_.Item) = False then
    begin
      DB_.State := ItemHnd_.Item.State;
      Result := False;
      exit;
    end;
  ItemHnd_.Name := ItemName;
  ItemHnd_.Description := ItemDescription;
  ItemHnd_.CreateTime := ItemHnd_.Item.RHeader.CreateTime;
  ItemHnd_.ModificationTime := ItemHnd_.Item.RHeader.ModificationTime;
  ItemHnd_.ItemExtID := ItemExtID;
  ItemHnd_.OpenFlags := True;
  DB_.State := DB_ok;
  Result := True;
end;

function db_ItemOpen(const pathName, ItemName: U_String; const ItemExtID: Byte; var ItemHnd_: TItemHandle_; var DB_: TObjectDataHandle): Boolean;
begin
  if ItemHnd_.OpenFlags then
    begin
      DB_.State := DB_RepeatOpenItemError;
      Result := False;
      exit;
    end;
  if db_GetItem(pathName, ItemName, ItemExtID, ItemHnd_.Item, DB_) = False then
    begin
      Result := False;
      exit;
    end;
  if dbItem_BlockInit(DB_.IOHnd, ItemHnd_.Item) = False then
    begin
      DB_.State := ItemHnd_.Item.State;
      Result := False;
      exit;
    end;
  ItemHnd_.Name := ItemName;
  ItemHnd_.Description := ItemHnd_.Item.Description;
  ItemHnd_.CreateTime := ItemHnd_.Item.RHeader.CreateTime;
  ItemHnd_.ModificationTime := ItemHnd_.Item.RHeader.ModificationTime;
  ItemHnd_.ItemExtID := ItemExtID;
  ItemHnd_.OpenFlags := True;
  DB_.State := DB_ok;
  Result := True;
end;

function db_ItemFastOpen(const fPos: Int64; const ItemExtID: Byte; var ItemHnd_: TItemHandle_; var DB_: TObjectDataHandle): Boolean;
begin
  if ItemHnd_.OpenFlags then
    begin
      DB_.State := DB_RepeatOpenItemError;
      Result := False;
      exit;
    end;
  if dbHeader_ReadRec(fPos, DB_.IOHnd, ItemHnd_.Item.RHeader) = False then
    begin
      DB_.State := ItemHnd_.Item.RHeader.State;
      Result := False;
      exit;
    end;
  if ItemHnd_.Item.RHeader.ID <> DB_Header_Item_ID then
    begin
      DB_.State := DB_OpenItemError;
      Result := False;
      exit;
    end;
  if dbItem_OnlyReadItemRec(ItemHnd_.Item.RHeader.DataPosition, DB_.IOHnd, ItemHnd_.Item) = False then
    begin
      DB_.State := ItemHnd_.Item.State;
      Result := False;
      exit;
    end;
  if ItemHnd_.Item.ExtID <> ItemExtID then
    begin
      DB_.State := DB_OpenItemError;
      Result := False;
      exit;
    end;
  if dbItem_BlockInit(DB_.IOHnd, ItemHnd_.Item) = False then
    begin
      DB_.State := ItemHnd_.Item.State;
      Result := False;
      exit;
    end;
  ItemHnd_.Name := ItemHnd_.Item.RHeader.Name;
  ItemHnd_.Description := ItemHnd_.Item.Description;
  ItemHnd_.CreateTime := ItemHnd_.Item.RHeader.CreateTime;
  ItemHnd_.ModificationTime := ItemHnd_.Item.RHeader.ModificationTime;
  ItemHnd_.ItemExtID := ItemExtID;
  ItemHnd_.OpenFlags := True;
  DB_.State := DB_ok;
  Result := True;
end;

function db_ItemUpdate(var ItemHnd_: TItemHandle_; var DB_: TObjectDataHandle): Boolean;
begin
  if ItemHnd_.OpenFlags = False then
    begin
      DB_.State := DB_CloseItemError;
      Result := False;
      exit;
    end;
  if ItemHnd_.Item.DataModification then
    begin
      // fixed by qq600585,2018-12
      // Header has a certain chance of being changed by other item operation during the opening of item
      if dbHeader_ReadReservedRec(ItemHnd_.Item.RHeader.CurrentHeader, DB_.IOHnd, ItemHnd_.Item.RHeader) = False then
        begin
          DB_.State := ItemHnd_.Item.State;
          Result := False;
          exit;
        end;
      ItemHnd_.Item.RHeader.Name := ItemHnd_.Name;
      ItemHnd_.Item.RHeader.CreateTime := ItemHnd_.CreateTime;
      ItemHnd_.Item.RHeader.ModificationTime := ItemHnd_.ModificationTime;
      ItemHnd_.Item.Description := ItemHnd_.Description;
      ItemHnd_.Item.ExtID := ItemHnd_.ItemExtID;
      if dbItem_WriteRec(ItemHnd_.Item.RHeader.CurrentHeader, DB_.IOHnd, ItemHnd_.Item) = False then
        begin
          DB_.State := ItemHnd_.Item.State;
          Result := False;
          exit;
        end;
      ItemHnd_.Item.DataModification := False;
    end;
  Result := True;
end;

function db_ItemBodyReset(var ItemHnd_: TItemHandle_; var DB_: TObjectDataHandle): Boolean;
begin
  if ItemHnd_.OpenFlags = False then
    begin
      DB_.State := DB_CloseItemError;
      Result := False;
      exit;
    end;

  // fixed by qq600585,2018-12
  // Header has a certain chance of being changed by other item operation during the opening of item
  if dbHeader_ReadReservedRec(ItemHnd_.Item.RHeader.CurrentHeader, DB_.IOHnd, ItemHnd_.Item.RHeader) = False then
    begin
      DB_.State := ItemHnd_.Item.State;
      Result := False;
      exit;
    end;

  ItemHnd_.Item.RHeader.Name := ItemHnd_.Name;
  ItemHnd_.Item.RHeader.CreateTime := ItemHnd_.CreateTime;
  ItemHnd_.Item.RHeader.ModificationTime := ItemHnd_.ModificationTime;
  ItemHnd_.Item.Description := ItemHnd_.Description;
  ItemHnd_.Item.ExtID := ItemHnd_.ItemExtID;

  ItemHnd_.Item.FirstBlockPOS := 0;
  ItemHnd_.Item.LastBlockPOS := 0;
  ItemHnd_.Item.Size := 0;
  ItemHnd_.Item.BlockCount := 0;

  if dbItem_WriteRec(ItemHnd_.Item.RHeader.CurrentHeader, DB_.IOHnd, ItemHnd_.Item) = False then
    begin
      DB_.State := ItemHnd_.Item.State;
      Result := False;
      exit;
    end;
  ItemHnd_.Item.DataModification := False;

  Result := True;
end;

function db_ItemClose(var ItemHnd_: TItemHandle_; var DB_: TObjectDataHandle): Boolean;
begin
  Result := db_ItemUpdate(ItemHnd_, DB_);
  if Result then
      ItemHnd_.OpenFlags := False;
end;

function db_ItemReName(const FieldPos: Int64; const NewItemName, NewItemDescription: U_String; var ItemHnd_: TItemHandle_; var DB_: TObjectDataHandle): Boolean;
var
  SenderSearchHnd: TSearchItem_;
begin
  if ItemHnd_.OpenFlags = False then
    begin
      DB_.State := DB_CloseItemError;
      Result := False;
      exit;
    end;
  if db_FastFindFirstItem(FieldPos, NewItemName, ItemHnd_.ItemExtID, SenderSearchHnd, DB_) then
    if (ItemHnd_.Name = NewItemName) then
      begin
        DB_.State := DB_ItemNameError;
        Result := False;
        exit;
      end;

  // fixed by qq600585,2018-12
  // Header has a certain chance of being changed by other item operation during the opening of item
  if dbHeader_ReadReservedRec(ItemHnd_.Item.RHeader.CurrentHeader, DB_.IOHnd, ItemHnd_.Item.RHeader) = False then
    begin
      DB_.State := ItemHnd_.Item.State;
      Result := False;
      exit;
    end;

  ItemHnd_.Name := NewItemName;
  ItemHnd_.Description := NewItemDescription;
  ItemHnd_.Item.RHeader.Name := ItemHnd_.Name;
  ItemHnd_.Item.Description := ItemHnd_.Description;
  if dbItem_WriteRec(ItemHnd_.Item.RHeader.CurrentHeader, DB_.IOHnd, ItemHnd_.Item) = False then
    begin
      DB_.State := ItemHnd_.Item.State;
      Result := False;
      exit;
    end;
  DB_.State := DB_ok;
  Result := True;
end;

function db_ItemRead(const Size: Int64; var Buffers; var ItemHnd_: TItemHandle_; var DB_: TObjectDataHandle): Boolean;
begin
  if ItemHnd_.OpenFlags = False then
    begin
      DB_.State := DB_OpenItemError;
      Result := False;
      exit;
    end;
  if dbItem_BlockReadData(DB_.IOHnd, ItemHnd_.Item, Buffers, Size) = False then
    begin
      DB_.State := ItemHnd_.Item.State;
      Result := False;
      exit;
    end;
  DB_.State := DB_ok;
  Result := True;
end;

function db_ItemWrite(const Size: Int64; var Buffers; var ItemHnd_: TItemHandle_; var DB_: TObjectDataHandle): Boolean;
begin
  if ItemHnd_.OpenFlags = False then
    begin
      DB_.State := DB_OpenItemError;
      Result := False;
      exit;
    end;
  if dbItem_BlockWriteData(DB_.IOHnd, ItemHnd_.Item, Buffers, Size) = False then
    begin
      DB_.State := ItemHnd_.Item.State;
      Result := False;
      exit;
    end;
  DB_.State := DB_ok;
  Result := True;
end;

function db_ItemSeekPos(const fPos: Int64; var ItemHnd_: TItemHandle_; var DB_: TObjectDataHandle): Boolean;
begin
  if ItemHnd_.OpenFlags = False then
    begin
      DB_.State := DB_OpenItemError;
      Result := False;
      exit;
    end;
  if dbItem_BlockSeekPOS(DB_.IOHnd, ItemHnd_.Item, fPos) = False then
    begin
      DB_.State := ItemHnd_.Item.State;
      Result := False;
      exit;
    end;
  DB_.State := DB_ok;
  Result := True;
end;

function db_ItemSeekStartPos(var ItemHnd_: TItemHandle_; var DB_: TObjectDataHandle): Boolean;
begin
  if ItemHnd_.OpenFlags = False then
    begin
      DB_.State := DB_OpenItemError;
      Result := False;
      exit;
    end;
  if dbItem_BlockSeekStartPOS(DB_.IOHnd, ItemHnd_.Item) = False then
    begin
      DB_.State := ItemHnd_.Item.State;
      Result := False;
      exit;
    end;
  DB_.State := DB_ok;
  Result := True;
end;

function db_ItemSeekLastPos(var ItemHnd_: TItemHandle_; var DB_: TObjectDataHandle): Boolean;
begin
  if ItemHnd_.OpenFlags = False then
    begin
      DB_.State := DB_OpenItemError;
      Result := False;
      exit;
    end;
  if dbItem_BlockSeekLastPOS(DB_.IOHnd, ItemHnd_.Item) = False then
    begin
      DB_.State := ItemHnd_.Item.State;
      Result := False;
      exit;
    end;
  DB_.State := DB_ok;
  Result := True;
end;

function db_ItemGetPos(var ItemHnd_: TItemHandle_; var DB_: TObjectDataHandle): Int64;
begin
  if ItemHnd_.OpenFlags = False then
    begin
      DB_.State := DB_OpenItemError;
      Result := 0;
      exit;
    end;
  Result := dbItem_BlockGetPOS(DB_.IOHnd, ItemHnd_.Item);
end;

function db_ItemGetSize(var ItemHnd_: TItemHandle_; var DB_: TObjectDataHandle): Int64;
begin
  if ItemHnd_.OpenFlags = False then
    begin
      DB_.State := DB_OpenItemError;
      Result := 0;
      exit;
    end;
  Result := ItemHnd_.Item.Size;
end;

function db_AppendItemSize(var ItemHnd_: TItemHandle_; const Size: Int64; var DB_: TObjectDataHandle): Boolean;
var
  SwapBuffers: array [0 .. C_MaxBufferFragmentSize] of Byte;
  i: Integer;
begin
  if ItemHnd_.OpenFlags = False then
    begin
      DB_.State := DB_OpenItemError;
      Result := False;
      exit;
    end;
  if dbItem_BlockSeekLastPOS(DB_.IOHnd, ItemHnd_.Item) = False then
    begin
      DB_.State := ItemHnd_.Item.State;
      Result := False;
      exit;
    end;
  if Size > C_MaxBufferFragmentSize then
    begin
      for i := 1 to (Size div C_MaxBufferFragmentSize) do
        begin
          if dbItem_BlockWriteData(DB_.IOHnd, ItemHnd_.Item, SwapBuffers, C_MaxBufferFragmentSize) = False then
            begin
              DB_.State := ItemHnd_.Item.State;
              Result := False;
              exit;
            end;
        end;
      if dbItem_BlockWriteData(DB_.IOHnd, ItemHnd_.Item, SwapBuffers, (Size mod C_MaxBufferFragmentSize)) = False then
        begin
          DB_.State := ItemHnd_.Item.State;
          Result := False;
          exit;
        end;
      DB_.State := DB_ok;
      Result := True;
      exit;
    end;
  if dbItem_BlockWriteData(DB_.IOHnd, ItemHnd_.Item, SwapBuffers, Size) = False then
    begin
      DB_.State := ItemHnd_.Item.State;
      Result := False;
      exit;
    end;
  DB_.State := DB_ok;
  Result := True;
end;

function db_ExistsRootField(const Name: U_String; var DB_: TObjectDataHandle): Boolean;
var
  f: TField;
begin
  if DB_.RootHeaderCount = 0 then
    begin
      DB_.State := DB_PathNameError;
      Result := False;
      exit;
    end;
  if dbHeader_ReadRec(DB_.DefaultFieldPOS, DB_.IOHnd, f.RHeader) = False then
    begin
      DB_.State := f.RHeader.State;
      Result := False;
      exit;
    end;

  if (dbMultipleMatch(Name, f.RHeader.Name) = True) and (f.RHeader.ID = DB_Header_Field_ID) then
    begin
      DB_.State := DB_ok;
      Result := True;
      exit;
    end;

  if DB_.RootHeaderCount = 1 then
    begin
      DB_.State := DB_PathNameError;
      Result := False;
      exit;
    end;
  if dbHeader_ReadRec(f.RHeader.NextHeader, DB_.IOHnd, f.RHeader) = False then
    begin
      DB_.State := f.RHeader.State;
      Result := False;
      exit;
    end;

  if (dbMultipleMatch(Name, f.RHeader.Name) = True) and (f.RHeader.ID = DB_Header_Field_ID) then
    begin
      DB_.State := DB_ok;
      Result := True;
      exit;
    end;

  while f.RHeader.CurrentHeader <> DB_.DefaultFieldPOS do
    begin
      if dbHeader_ReadRec(f.RHeader.NextHeader, DB_.IOHnd, f.RHeader) = False then
        begin
          DB_.State := f.RHeader.State;
          Result := False;
          exit;
        end;
      if (dbMultipleMatch(Name, f.RHeader.Name) = True) and (f.RHeader.ID = DB_Header_Field_ID) then
        begin
          DB_.State := DB_ok;
          Result := True;
          exit;
        end;
    end;
  DB_.State := DB_PathNameError;
  Result := False;
end;

function db_FindFirstHeader(const pathName, FilterName: U_String; const ID: Byte; var SenderSearch: TSearchHeader_; var DB_: TObjectDataHandle): Boolean;
var
  f: TField;
begin
  if db_GetField(pathName, f, DB_) = False then
    begin
      Result := False;
      exit;
    end;
  if dbField_FindFirst(FilterName, ID, f.RHeader.CurrentHeader, DB_.IOHnd, SenderSearch.FieldSearch) = False then
    begin
      DB_.State := SenderSearch.FieldSearch.State;
      Result := False;
      exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.ID := SenderSearch.FieldSearch.RHeader.ID;
  SenderSearch.CreateTime := SenderSearch.FieldSearch.RHeader.CreateTime;
  SenderSearch.ModificationTime := SenderSearch.FieldSearch.RHeader.ModificationTime;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := 1;
  DB_.State := SenderSearch.FieldSearch.State;
  Result := True;
end;

function db_FindNextHeader(var SenderSearch: TSearchHeader_; var DB_: TObjectDataHandle): Boolean;
begin
  if dbField_FindNext(DB_.IOHnd, SenderSearch.FieldSearch) = False then
    begin
      DB_.State := SenderSearch.FieldSearch.State;
      Result := False;
      exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.ID := SenderSearch.FieldSearch.RHeader.ID;
  SenderSearch.CreateTime := SenderSearch.FieldSearch.RHeader.CreateTime;
  SenderSearch.ModificationTime := SenderSearch.FieldSearch.RHeader.ModificationTime;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := SenderSearch.CompleteCount + 1;
  DB_.State := SenderSearch.FieldSearch.State;
  Result := True;
end;

function db_FindLastHeader(const pathName, FilterName: U_String; const ID: Byte; var SenderSearch: TSearchHeader_; var DB_: TObjectDataHandle): Boolean;
var
  f: TField;
begin
  if db_GetField(pathName, f, DB_) = False then
    begin
      Result := False;
      exit;
    end;
  if dbField_FindLast(FilterName, ID, f.RHeader.CurrentHeader, DB_.IOHnd, SenderSearch.FieldSearch) = False then
    begin
      DB_.State := SenderSearch.FieldSearch.State;
      Result := False;
      exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.ID := SenderSearch.FieldSearch.RHeader.ID;
  SenderSearch.CreateTime := SenderSearch.FieldSearch.RHeader.CreateTime;
  SenderSearch.ModificationTime := SenderSearch.FieldSearch.RHeader.ModificationTime;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := 1;
  DB_.State := SenderSearch.FieldSearch.State;
  Result := True;
end;

function db_FindPrevHeader(var SenderSearch: TSearchHeader_; var DB_: TObjectDataHandle): Boolean;
begin
  if dbField_FindPrev(DB_.IOHnd, SenderSearch.FieldSearch) = False then
    begin
      DB_.State := SenderSearch.FieldSearch.State;
      Result := False;
      exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.ID := SenderSearch.FieldSearch.RHeader.ID;
  SenderSearch.CreateTime := SenderSearch.FieldSearch.RHeader.CreateTime;
  SenderSearch.ModificationTime := SenderSearch.FieldSearch.RHeader.ModificationTime;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := SenderSearch.CompleteCount + 1;
  DB_.State := SenderSearch.FieldSearch.State;
  Result := True;
end;

function db_FindFirstItem(const pathName, FilterName: U_String; const ItemExtID: Byte; var SenderSearch: TSearchItem_; var DB_: TObjectDataHandle): Boolean;
var
  f: TField;
  itm: TItem;
begin
  if db_GetField(pathName, f, DB_) = False then
    begin
      Result := False;
      exit;
    end;
  if dbField_FindFirstItem(FilterName, ItemExtID, f.RHeader.CurrentHeader, DB_.IOHnd, SenderSearch.FieldSearch, itm) = False then
    begin
      DB_.State := SenderSearch.FieldSearch.State;
      Result := False;
      exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.Description := itm.Description;
  SenderSearch.ExtID := itm.ExtID;
  SenderSearch.Size := itm.Size;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := 1;
  DB_.State := SenderSearch.FieldSearch.State;
  Result := True;
end;

function db_FindNextItem(var SenderSearch: TSearchItem_; const ItemExtID: Byte; var DB_: TObjectDataHandle): Boolean;
var
  itm: TItem;
begin
  if dbField_FindNextItem(ItemExtID, DB_.IOHnd, SenderSearch.FieldSearch, itm) = False then
    begin
      DB_.State := SenderSearch.FieldSearch.State;
      Result := False;
      exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.Description := itm.Description;
  SenderSearch.ExtID := itm.ExtID;
  SenderSearch.Size := itm.Size;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := SenderSearch.CompleteCount + 1;
  DB_.State := SenderSearch.FieldSearch.State;
  Result := True;
end;

function db_FindLastItem(const pathName, FilterName: U_String; const ItemExtID: Byte; var SenderSearch: TSearchItem_; var DB_: TObjectDataHandle): Boolean;
var
  f: TField;
  itm: TItem;
begin
  if db_GetField(pathName, f, DB_) = False then
    begin
      Result := False;
      exit;
    end;
  if dbField_FindLastItem(FilterName, ItemExtID, f.RHeader.CurrentHeader, DB_.IOHnd, SenderSearch.FieldSearch, itm) = False then
    begin
      DB_.State := SenderSearch.FieldSearch.State;
      Result := False;
      exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.Description := itm.Description;
  SenderSearch.ExtID := itm.ExtID;
  SenderSearch.Size := itm.Size;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := 1;
  DB_.State := SenderSearch.FieldSearch.State;
  Result := True;
end;

function db_FindPrevItem(var SenderSearch: TSearchItem_; const ItemExtID: Byte; var DB_: TObjectDataHandle): Boolean;
var
  itm: TItem;
begin
  if dbField_FindPrevItem(ItemExtID, DB_.IOHnd, SenderSearch.FieldSearch, itm) = False then
    begin
      DB_.State := SenderSearch.FieldSearch.State;
      Result := False;
      exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.Description := itm.Description;
  SenderSearch.ExtID := itm.ExtID;
  SenderSearch.Size := itm.Size;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := SenderSearch.CompleteCount + 1;
  DB_.State := SenderSearch.FieldSearch.State;
  Result := True;
end;

function db_FastFindFirstItem(const FieldPos: Int64; const FilterName: U_String; const ItemExtID: Byte; var SenderSearch: TSearchItem_; var DB_: TObjectDataHandle): Boolean;
var
  itm: TItem;
begin
  if dbField_FindFirstItem(FilterName, ItemExtID, FieldPos, DB_.IOHnd, SenderSearch.FieldSearch, itm) = False then
    begin
      DB_.State := SenderSearch.FieldSearch.State;
      Result := False;
      exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.Description := itm.Description;
  SenderSearch.ExtID := itm.ExtID;
  SenderSearch.Size := itm.Size;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := 1;
  DB_.State := SenderSearch.FieldSearch.State;
  Result := True;
end;

function db_FastFindNextItem(var SenderSearch: TSearchItem_; const ItemExtID: Byte; var DB_: TObjectDataHandle): Boolean;
var
  itm: TItem;
begin
  if dbField_FindNextItem(ItemExtID, DB_.IOHnd, SenderSearch.FieldSearch, itm) = False then
    begin
      DB_.State := SenderSearch.FieldSearch.State;
      Result := False;
      exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.Description := itm.Description;
  SenderSearch.ExtID := itm.ExtID;
  SenderSearch.Size := itm.Size;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := SenderSearch.CompleteCount + 1;
  DB_.State := SenderSearch.FieldSearch.State;
  Result := True;
end;

function db_FastFindLastItem(const FieldPos: Int64; const FilterName: U_String; const ItemExtID: Byte; var SenderSearch: TSearchItem_; var DB_: TObjectDataHandle): Boolean;
var
  itm: TItem;
begin
  if dbField_FindLastItem(FilterName, ItemExtID, FieldPos, DB_.IOHnd, SenderSearch.FieldSearch, itm) = False then
    begin
      DB_.State := SenderSearch.FieldSearch.State;
      Result := False;
      exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.Description := itm.Description;
  SenderSearch.ExtID := itm.ExtID;
  SenderSearch.Size := itm.Size;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := 1;
  DB_.State := SenderSearch.FieldSearch.State;
  Result := True;
end;

function db_FastFindPrevItem(var SenderSearch: TSearchItem_; const ItemExtID: Byte; var DB_: TObjectDataHandle): Boolean;
var
  itm: TItem;
begin
  if dbField_FindPrevItem(ItemExtID, DB_.IOHnd, SenderSearch.FieldSearch, itm) = False then
    begin
      DB_.State := SenderSearch.FieldSearch.State;
      Result := False;
      exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.Description := itm.Description;
  SenderSearch.ExtID := itm.ExtID;
  SenderSearch.Size := itm.Size;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := SenderSearch.CompleteCount + 1;
  DB_.State := SenderSearch.FieldSearch.State;
  Result := True;
end;

function db_FindFirstField(const pathName, FilterName: U_String; var SenderSearch: TSearchField_; var DB_: TObjectDataHandle): Boolean;
var
  f: TField;
begin
  if db_GetField(pathName, f, DB_) = False then
    begin
      Result := False;
      exit;
    end;
  if dbField_FindFirst(FilterName, DB_Header_Field_ID, f.RHeader.CurrentHeader, DB_.IOHnd, SenderSearch.FieldSearch) = False then
    begin
      DB_.State := SenderSearch.FieldSearch.State;
      Result := False;
      exit;
    end;
  if dbField_OnlyReadFieldRec(SenderSearch.FieldSearch.RHeader.DataPosition, DB_.IOHnd, f) = False then
    begin
      DB_.State := f.State;
      Result := False;
      exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.Description := f.Description;
  SenderSearch.HeaderCount := f.HeaderCount;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := 1;
  DB_.State := SenderSearch.FieldSearch.State;
  Result := True;
end;

function db_FindNextField(var SenderSearch: TSearchField_; var DB_: TObjectDataHandle): Boolean;
var
  f: TField;
begin
  if dbField_FindNext(DB_.IOHnd, SenderSearch.FieldSearch) = False then
    begin
      DB_.State := SenderSearch.FieldSearch.State;
      Result := False;
      exit;
    end;
  if dbField_OnlyReadFieldRec(SenderSearch.FieldSearch.RHeader.DataPosition, DB_.IOHnd, f) = False then
    begin
      DB_.State := f.State;
      Result := False;
      exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.Description := f.Description;
  SenderSearch.HeaderCount := f.HeaderCount;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := SenderSearch.CompleteCount + 1;
  DB_.State := SenderSearch.FieldSearch.State;
  Result := True;
end;

function db_FindLastField(const pathName, FilterName: U_String; var SenderSearch: TSearchField_; var DB_: TObjectDataHandle): Boolean;
var
  f: TField;
begin
  if db_GetField(pathName, f, DB_) = False then
    begin
      Result := False;
      exit;
    end;
  if dbField_FindLast(FilterName, DB_Header_Field_ID, f.RHeader.CurrentHeader, DB_.IOHnd, SenderSearch.FieldSearch) = False then
    begin
      DB_.State := SenderSearch.FieldSearch.State;
      Result := False;
      exit;
    end;
  if dbField_OnlyReadFieldRec(SenderSearch.FieldSearch.RHeader.DataPosition, DB_.IOHnd, f) = False then
    begin
      DB_.State := f.State;
      Result := False;
      exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.Description := f.Description;
  SenderSearch.HeaderCount := f.HeaderCount;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := 1;
  DB_.State := SenderSearch.FieldSearch.State;
  Result := True;
end;

function db_FindPrevField(var SenderSearch: TSearchField_; var DB_: TObjectDataHandle): Boolean;
var
  f: TField;
begin
  if dbField_FindPrev(DB_.IOHnd, SenderSearch.FieldSearch) = False then
    begin
      DB_.State := SenderSearch.FieldSearch.State;
      Result := False;
      exit;
    end;
  if dbField_OnlyReadFieldRec(SenderSearch.FieldSearch.RHeader.DataPosition, DB_.IOHnd, f) = False then
    begin
      DB_.State := f.State;
      Result := False;
      exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.Description := f.Description;
  SenderSearch.HeaderCount := f.HeaderCount;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := SenderSearch.CompleteCount + 1;
  DB_.State := SenderSearch.FieldSearch.State;
  Result := True;
end;

function db_FastFindFirstField(const FieldPos: Int64; const FilterName: U_String; var SenderSearch: TSearchField_; var DB_: TObjectDataHandle): Boolean;
var
  f: TField;
begin
  if dbField_FindFirst(FilterName, DB_Header_Field_ID, FieldPos, DB_.IOHnd, SenderSearch.FieldSearch) = False then
    begin
      DB_.State := SenderSearch.FieldSearch.State;
      Result := False;
      exit;
    end;
  if dbField_OnlyReadFieldRec(SenderSearch.FieldSearch.RHeader.DataPosition, DB_.IOHnd, f) = False then
    begin
      DB_.State := f.State;
      Result := False;
      exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.Description := f.Description;
  SenderSearch.HeaderCount := f.HeaderCount;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := 1;
  DB_.State := SenderSearch.FieldSearch.State;
  Result := True;
end;

function db_FastFindNextField(var SenderSearch: TSearchField_; var DB_: TObjectDataHandle): Boolean;
var
  f: TField;
begin
  if dbField_FindNext(DB_.IOHnd, SenderSearch.FieldSearch) = False then
    begin
      DB_.State := SenderSearch.FieldSearch.State;
      Result := False;
      exit;
    end;
  if dbField_OnlyReadFieldRec(SenderSearch.FieldSearch.RHeader.DataPosition, DB_.IOHnd, f) = False then
    begin
      DB_.State := f.State;
      Result := False;
      exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.Description := f.Description;
  SenderSearch.HeaderCount := f.HeaderCount;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := SenderSearch.CompleteCount + 1;
  DB_.State := SenderSearch.FieldSearch.State;
  Result := True;
end;

function db_FastFindLastField(const FieldPos: Int64; const FilterName: U_String; var SenderSearch: TSearchField_; var DB_: TObjectDataHandle): Boolean;
var
  f: TField;
begin
  if dbField_FindLast(FilterName, DB_Header_Field_ID, FieldPos, DB_.IOHnd, SenderSearch.FieldSearch) = False then
    begin
      DB_.State := SenderSearch.FieldSearch.State;
      Result := False;
      exit;
    end;
  if dbField_OnlyReadFieldRec(SenderSearch.FieldSearch.RHeader.DataPosition, DB_.IOHnd, f) = False then
    begin
      DB_.State := f.State;
      Result := False;
      exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.Description := f.Description;
  SenderSearch.HeaderCount := f.HeaderCount;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := 1;
  DB_.State := SenderSearch.FieldSearch.State;
  Result := True;
end;

function db_FastFindPrevField(var SenderSearch: TSearchField_; var DB_: TObjectDataHandle): Boolean;
var
  f: TField;
begin
  if dbField_FindPrev(DB_.IOHnd, SenderSearch.FieldSearch) = False then
    begin
      DB_.State := SenderSearch.FieldSearch.State;
      Result := False;
      exit;
    end;
  if dbField_OnlyReadFieldRec(SenderSearch.FieldSearch.RHeader.DataPosition, DB_.IOHnd, f) = False then
    begin
      DB_.State := f.State;
      Result := False;
      exit;
    end;
  SenderSearch.Name := SenderSearch.FieldSearch.RHeader.Name;
  SenderSearch.Description := f.Description;
  SenderSearch.HeaderCount := f.HeaderCount;
  SenderSearch.HeaderPOS := SenderSearch.FieldSearch.RHeader.CurrentHeader;
  SenderSearch.CompleteCount := SenderSearch.CompleteCount + 1;
  DB_.State := SenderSearch.FieldSearch.State;
  Result := True;
end;

function db_RecursionSearchFirst(const InitPath, FilterName: U_String; var SenderRecursionSearch: TRecursionSearch_; var DB_: TObjectDataHandle): Boolean;
begin
  if db_GetField(InitPath, SenderRecursionSearch.CurrentField, DB_) = False then
    begin
      Result := False;
      exit;
    end;
  SenderRecursionSearch.SearchBuffGo := 0;
  if dbField_FindFirst(FilterName, DB_Header_Item_ID, SenderRecursionSearch.CurrentField.RHeader.CurrentHeader, DB_.IOHnd,
    SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo]) = False then
    begin
      if dbField_FindFirst('*', DB_Header_Field_ID, SenderRecursionSearch.CurrentField.RHeader.CurrentHeader, DB_.IOHnd,
        SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo]) = False then
        begin
          DB_.State := SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo].State;
          Result := False;
          exit;
        end;
      if dbField_OnlyReadFieldRec(SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo].RHeader.DataPosition, DB_.IOHnd, SenderRecursionSearch.CurrentField) = False
      then
        begin
          DB_.State := SenderRecursionSearch.CurrentField.State;
          Result := False;
          exit;
        end;
      SenderRecursionSearch.CurrentField.RHeader := SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo].RHeader;
      SenderRecursionSearch.ReturnHeader := SenderRecursionSearch.CurrentField.RHeader;
      SenderRecursionSearch.SearchBuffGo := SenderRecursionSearch.SearchBuffGo + 1;
    end
  else
      SenderRecursionSearch.ReturnHeader := SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo].RHeader;
  SenderRecursionSearch.InitPath := InitPath;
  SenderRecursionSearch.FilterName := FilterName;
  DB_.State := DB_ok;
  Result := True;
end;

function db_RecursionSearchNext(var SenderRecursionSearch: TRecursionSearch_; var DB_: TObjectDataHandle): Boolean;
begin
  case SenderRecursionSearch.ReturnHeader.ID of
    DB_Header_Field_ID:
      begin
        if dbField_FindFirst(SenderRecursionSearch.FilterName, DB_Header_Item_ID, SenderRecursionSearch.CurrentField.RHeader.CurrentHeader, DB_.IOHnd,
          SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo]) then
          begin
            SenderRecursionSearch.ReturnHeader := SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo].RHeader;
            DB_.State := DB_ok;
            Result := True;
            exit;
          end;
        if dbField_FindFirst('*', DB_Header_Field_ID, SenderRecursionSearch.CurrentField.RHeader.CurrentHeader, DB_.IOHnd,
          SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo]) then
          begin
            if dbField_OnlyReadFieldRec(SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo].RHeader.DataPosition, DB_.IOHnd,
              SenderRecursionSearch.CurrentField) = False then
              begin
                DB_.State := SenderRecursionSearch.CurrentField.State;
                Result := False;
                exit;
              end;
            SenderRecursionSearch.CurrentField.RHeader := SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo].RHeader;
            SenderRecursionSearch.ReturnHeader := SenderRecursionSearch.CurrentField.RHeader;
            SenderRecursionSearch.SearchBuffGo := SenderRecursionSearch.SearchBuffGo + 1;
            DB_.State := DB_ok;
            Result := True;
            exit;
          end;

        if SenderRecursionSearch.SearchBuffGo = 0 then
          begin
            DB_.State := DB_RecursionSearchOver;
            Result := False;
            exit;
          end;
        SenderRecursionSearch.SearchBuffGo := SenderRecursionSearch.SearchBuffGo - 1;
        while dbField_FindNext(DB_.IOHnd, SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo]) = False do
          begin
            if SenderRecursionSearch.SearchBuffGo = 0 then
              begin
                DB_.State := DB_RecursionSearchOver;
                Result := False;
                exit;
              end;
            SenderRecursionSearch.SearchBuffGo := SenderRecursionSearch.SearchBuffGo - 1;
          end;

        if dbField_OnlyReadFieldRec(SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo].RHeader.DataPosition, DB_.IOHnd, SenderRecursionSearch.CurrentField) = False then
          begin
            DB_.State := SenderRecursionSearch.CurrentField.State;
            Result := False;
            exit;
          end;
        SenderRecursionSearch.CurrentField.RHeader := SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo].RHeader;
        SenderRecursionSearch.ReturnHeader := SenderRecursionSearch.CurrentField.RHeader;
        SenderRecursionSearch.SearchBuffGo := SenderRecursionSearch.SearchBuffGo + 1;
        DB_.State := DB_ok;
        Result := True;
        exit;
      end;
    DB_Header_Item_ID:
      begin
        if dbField_FindNext(DB_.IOHnd, SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo]) then
          begin
            SenderRecursionSearch.ReturnHeader := SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo].RHeader;
            DB_.State := DB_ok;
            Result := True;
            exit;
          end;
        if dbField_FindFirst('*', DB_Header_Field_ID, SenderRecursionSearch.CurrentField.RHeader.CurrentHeader, DB_.IOHnd,
          SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo]) then
          begin
            if dbField_OnlyReadFieldRec(SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo].RHeader.DataPosition, DB_.IOHnd,
              SenderRecursionSearch.CurrentField) = False then
              begin
                DB_.State := SenderRecursionSearch.CurrentField.State;
                Result := False;
                exit;
              end;
            SenderRecursionSearch.CurrentField.RHeader := SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo].RHeader;
            SenderRecursionSearch.ReturnHeader := SenderRecursionSearch.CurrentField.RHeader;
            SenderRecursionSearch.SearchBuffGo := SenderRecursionSearch.SearchBuffGo + 1;
            DB_.State := DB_ok;
            Result := True;
            exit;
          end;

        if SenderRecursionSearch.SearchBuffGo = 0 then
          begin
            DB_.State := DB_RecursionSearchOver;
            Result := False;
            exit;
          end;
        SenderRecursionSearch.SearchBuffGo := SenderRecursionSearch.SearchBuffGo - 1;
        while dbField_FindNext(DB_.IOHnd, SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo]) = False do
          begin
            if SenderRecursionSearch.SearchBuffGo = 0 then
              begin
                DB_.State := DB_RecursionSearchOver;
                Result := False;
                exit;
              end;
            SenderRecursionSearch.SearchBuffGo := SenderRecursionSearch.SearchBuffGo - 1;
          end;

        if dbField_OnlyReadFieldRec(SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo].RHeader.DataPosition, DB_.IOHnd, SenderRecursionSearch.CurrentField) = False then
          begin
            DB_.State := SenderRecursionSearch.CurrentField.State;
            Result := False;
            exit;
          end;
        SenderRecursionSearch.CurrentField.RHeader := SenderRecursionSearch.SearchBuff[SenderRecursionSearch.SearchBuffGo].RHeader;
        SenderRecursionSearch.ReturnHeader := SenderRecursionSearch.CurrentField.RHeader;
        SenderRecursionSearch.SearchBuffGo := SenderRecursionSearch.SearchBuffGo + 1;
        DB_.State := DB_ok;
        Result := True;
        exit;
      end;
    else
      begin
        Result := False;
      end;
  end;
end;

end.
