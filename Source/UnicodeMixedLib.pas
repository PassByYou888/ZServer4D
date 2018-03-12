{ ****************************************************************************** }
{ * MixedLibrary,writen by QQ 600585@qq.com                                    * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ ****************************************************************************** }

{
  *
  * Unit Name: MixedLibrary
  * Purpose  : mixed Low Level Function Library
  *
}

(*
  update history
  2017-11-26 fixed UmlMD5Stream and umlMD5 calculate x64 and x86,ARM platform more than 4G memory Support QQ600585
*)

unit UnicodeMixedLib;

{$I zDefine.inc}

interface

uses SysUtils, Classes, Types, Variants,
  PascalStrings,
  CoreClasses;

const
  umlAddressLength  = SizeOf(Pointer);
  umlPointerLength  = umlAddressLength;
  umlIntegerLength  = 4;
  umlInt64Length    = 8;
  umlUInt64Length   = 8;
  umlSingleLength   = 4;
  umlDoubleLength   = 8;
  umlExtendedLength = 10;
  umlSmallIntLength = 2;
  umlByteLength     = 1;
  umlShortIntLength = 1;
  umlWordLength     = 2;
  umlDWORDLength    = 4;
  umlCardinalLength = 4;
  umlBooleanLength  = 1;
  umlBoolLength     = 1;
  umlMD5Length      = 16;
  umlDESLength      = 8;

  umlMaxSearchRec = 1024;

  umlMaxFileRecSize = $F000;

  umlSeekError       = -910;
  umlFileWriteError  = -909;
  umlFileReadError   = -908;
  umlFileHandleError = -907;
  umlCloseFileError  = -906;
  umlOpenFileError   = -905;
  umlNotOpenFile     = -904;
  umlCreateFileError = -903;
  umlFileIsActive    = -902;
  umlNotFindFile     = -901;
  umlNotError        = -900;

  FixedLengthStringSize       = 64;
  FixedLengthStringHeaderSize = 1;

  PrepareReadCacheSize = 512;

type
  umlSystemString   = SystemString;
  umlString         = TPascalString;
  umlPString        = PPascalString;
  umlChar           = SystemChar;
  umlStringDynArray = array of SystemString;

  umlBytes = TBytes;

  TSR = TSearchRec;

  TMixedStream = TCoreClassStream;

  TIOHnd = packed record
    IsOnlyRead: Boolean;
    OpenFlags: Boolean;
    AutoFree: Boolean;
    Handle: TMixedStream;
    Time: TDateTime;
    Attrib: Integer;
    Size: Int64;
    Position: Int64;
    Name: umlString;
    FlushBuff: TMixedStream;
    PrepareReadPosition: Int64;
    PrepareReadBuff: TMixedStream;
    IORead, IOWrite: Int64;
    WriteFlag: Boolean;
    Data: Pointer;
    Return: Integer;
  end;

  umlArraySystemString = umlStringDynArray;
  umlArrayString       = TArrayPascalString;
  pumlArrayString      = ^umlArrayString;

  FixedLengthString = packed record
    len: Byte;
    Data: packed array [0 .. FixedLengthStringSize] of Byte;
  end;

function umlBytesOf(const S: TPascalString): TBytes; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlStringOf(const S: TBytes): TPascalString; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function FixedLengthString2Pascal(var S: FixedLengthString): TPascalString; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Pascal2FixedLengthString(var S: TPascalString): FixedLengthString; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function umlComparePosStr(const S: TPascalString; Offset: Integer; const t: TPascalString): Boolean;
function umlPos(const SubStr, Str: TPascalString; const Offset: Integer = 1): Integer;

function umlVarToStr(const V: Variant): TPascalString; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlStrToVar(const S: TPascalString): Variant; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function umlMax(const v1, v2: UInt64): UInt64; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlMax(const v1, v2: Cardinal): Cardinal; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlMax(const v1, v2: Word): Word; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlMax(const v1, v2: Byte): Byte; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlMax(const v1, v2: Int64): Int64; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlMax(const v1, v2: Integer): Integer; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlMax(const v1, v2: SmallInt): SmallInt; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlMax(const v1, v2: ShortInt): ShortInt; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlMax(const v1, v2: Double): Double; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlMax(const v1, v2: Single): Single; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function umlMin(const v1, v2: UInt64): UInt64; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlMin(const v1, v2: Cardinal): Cardinal; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlMin(const v1, v2: Word): Word; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlMin(const v1, v2: Byte): Byte; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlMin(const v1, v2: Int64): Int64; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlMin(const v1, v2: Integer): Integer; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlMin(const v1, v2: SmallInt): SmallInt; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlMin(const v1, v2: ShortInt): ShortInt; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlMin(const v1, v2: Double): Double; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlMin(const v1, v2: Single): Single; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function umlDeltaNumber(const V, delta: NativeInt): NativeInt; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlGetResourceStream(const FileName: TPascalString): TStream;

function umlSameVarValue(const v1, v2: Variant): Boolean;

function umlRandomRange(const aMin, aMax: Integer): Integer;
function umlRandomRangeF(const aMin, aMax: Single): Single;
function umlRandomRangeD(const aMin, aMax: Double): Double;
function umlDefaultTime: Double; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlDefaultAttrib: Integer;
function umlBoolToStr(const Value: Boolean): TPascalString;
function umlStrToBool(const Value: TPascalString): Boolean;

function umlFileExists(const FileName: TPascalString): Boolean;
function umlDirectoryExists(const DirectoryName: TPascalString): Boolean;
function umlCreateDirectory(const DirectoryName: TPascalString): Boolean;
function umlCurrentDirectory: TPascalString;
function umlCurrentPath: TPascalString;

function umlFindFirstFile(const FileName: TPascalString; var SR: TSR): Boolean;
function umlFindNextFile(var SR: TSR): Boolean;
function umlFindFirstDir(const DirName: TPascalString; var SR: TSR): Boolean;
function umlFindNextDir(var SR: TSR): Boolean;
procedure umlFindClose(var SR: TSR);

function umlGetFileList(const FullPath: TPascalString; AsLst: TCoreClassStrings): Integer;
function umlGetDirList(const FullPath: TPascalString; AsLst: TCoreClassStrings): Integer;

function umlGetFileListWithFullPath(const FullPath: TPascalString): umlStringDynArray;
function umlGetDirListWithFullPath(const FullPath: TPascalString): umlStringDynArray;

function umlCombinePath(const s1, s2: TPascalString): TPascalString;
function umlCombineFileName(const pathName, FileName: TPascalString): TPascalString;
function umlGetFileName(const S: TPascalString): TPascalString;
function umlGetFilePath(const S: TPascalString): TPascalString;
function umlChangeFileExt(const S, ext: TPascalString): TPascalString;
function umlGetFileExt(const S: TPascalString): TPascalString;

procedure InitIOHnd(var IOHnd: TIOHnd); {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlFileCreateAsStream(const Name: TPascalString; Stream: TMixedStream; var IOHnd: TIOHnd): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlFileOpenAsStream(const Name: TPascalString; Stream: TMixedStream; var IOHnd: TIOHnd; _OnlyRead: Boolean): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlFileCreate(const Name: TPascalString; var IOHnd: TIOHnd): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlFileOpen(const Name: TPascalString; var IOHnd: TIOHnd; _OnlyRead: Boolean): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlFileClose(var IOHnd: TIOHnd): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlFileUpdate(var IOHnd: TIOHnd): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlFileTest(var IOHnd: TIOHnd): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure umlResetPrepareRead(var IOHnd: TIOHnd); {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlFilePrepareRead(var IOHnd: TIOHnd; Size: Int64; var Buffers): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlFileRead(var IOHnd: TIOHnd; Size: Int64; var Buffers): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function umlFileBeginWrite(var IOHnd: TIOHnd): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlFileEndWrite(var IOHnd: TIOHnd): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlFileWrite(var IOHnd: TIOHnd; const Size: Int64; var Buffers): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function umlFileWriteStr(var IOHnd: TIOHnd; var Value: TPascalString): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlFileReadStr(var IOHnd: TIOHnd; var Value: TPascalString): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function umlFileSeek(var IOHnd: TIOHnd; APos: Int64): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlFileGetPOS(var IOHnd: TIOHnd): Int64; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlFileGetSize(var IOHnd: TIOHnd): Int64; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function umlGetFileTime(const FileName: TPascalString): TDateTime;
procedure umlSetFileTime(const FileName: TPascalString; newTime: TDateTime);

function umlGetFileSize(const FileName: TPascalString): Int64;

function umlGetFileCount(const FileName: TPascalString): Integer;
function umlGetFileDateTime(const FileName: TPascalString): TDateTime;
function umlDeleteFile(const FileName: TPascalString; const _VerifyCheck: Boolean = False): Boolean;
function umlCopyFile(const SourFile, DestFile: TPascalString): Boolean;
function umlRenameFile(const OldName, NewName: TPascalString): Boolean;

{ TPascalString }
procedure umlSetLength(var aStr: TPascalString; len: Integer); overload;
procedure umlSetLength(var aStr: umlBytes; len: Integer); overload;
procedure umlSetLength(var aStr: TArrayPascalString; len: Integer); overload;

function umlGetLength(const aStr: TPascalString): Integer; overload;
function umlGetLength(var aStr: umlBytes): Integer; overload;
function umlGetLength(const aStr: TArrayPascalString): Integer; overload;

function umlUpperCase(const Str: TPascalString): TPascalString; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlLowerCase(const Str: TPascalString): TPascalString; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlCopyStr(const aStr: TPascalString; MainPosition, LastPosition: Integer): TPascalString; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlSameText(const s1, s2: TPascalString): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function umlDeleteChar(const SText, ch: TPascalString): TPascalString; overload;
function umlDeleteChar(const SText: TPascalString; const SomeChars: array of SystemChar): TPascalString; overload;
function umlDeleteChar(const SText: TPascalString; const SomeCharsets: TOrdChars): TPascalString; overload;
function umlGetNumberCharInText(const n: TPascalString): TPascalString;

function umlMatchLimitChar(CharValue: umlChar; LimitValue: umlPString): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlMatchLimitChar(CharValue: umlChar; LimitValue: TPascalString): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlExistsLimitChar(StrValue: TPascalString; LimitValue: TPascalString): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlExistsChar(StrValue: TPascalString; LimitValue: TPascalString): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function umlTrimChar(const S, limitS: TPascalString): TPascalString; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function umlGetFirstStr(const aStr, limitS: TPascalString): TPascalString; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlGetLastStr(const aStr, limitS: TPascalString): TPascalString; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlDeleteFirstStr(const aStr, limitS: TPascalString): TPascalString; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlDeleteLastStr(const aStr, limitS: TPascalString): TPascalString; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlGetIndexStrCount(const aStr, limitS: TPascalString): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlGetIndexStr(const aStr: TPascalString; limitS: TPascalString; Index: Integer): TPascalString; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure umlGetSplitArray(const _SourText: TPascalString; var _DestArray: TArrayPascalString; const _SplitChar: TPascalString); {$IFDEF INLINE_ASM} inline; {$ENDIF}
function TArrayPascalStringToText(var _Ary: TArrayPascalString; const _SplitChar: TPascalString): TPascalString; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlStringsToText(_List: TCoreClassStrings; const _SplitChar: TPascalString): TPascalString; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function umlGetFirstStr_M(const aStr, limitS: TPascalString): TPascalString;
function umlDeleteFirstStr_M(const aStr, limitS: TPascalString): TPascalString;
function umlGetLastStr_M(const aStr, limitS: TPascalString): TPascalString;
function umlDeleteLastStr_M(const aStr, limitS: TPascalString): TPascalString;
function umlGetIndexStrCount_M(const aStr, limitS: TPascalString): Integer;
function umlGetIndexStr_M(const aStr: TPascalString; limitS: TPascalString; Index: Integer): TPascalString;

function umlGetFirstTextPos(const S: TPascalString; const TextArry: TArrayPascalString; var OutText: TPascalString): Integer;
function umlDeleteText(const Sour: TPascalString; const bToken, eToken: TArrayPascalString; ANeedBegin, ANeedEnd: Boolean): TPascalString;
function umlGetTextContent(const Sour: TPascalString; const bToken, eToken: TArrayPascalString): TPascalString;

type
  TTextType  = (ntBool, ntInt, ntInt64, ntUInt64, ntWord, ntByte, ntSmallInt, ntShortInt, ntUInt, ntSingle, ntDouble, ntCurrency, ntUnknow);
  TTextTypes = set of TTextType;
function umlGetNumTextType(const S: TPascalString): TTextType;

function umlIsHex(const aStr: TPascalString): Boolean;
function umlIsNumber(const aStr: TPascalString): Boolean;
function umlIsIntNumber(const aStr: TPascalString): Boolean;
function umlIsFloatNumber(const aStr: TPascalString): Boolean;
function umlIsBool(const aStr: TPascalString): Boolean;
function umlNumberCount(const aStr: TPascalString): Integer;

function umlPercentageToFloat(OriginMax, OriginMin, ProcressParameter: Double): Double;
function umlPercentageToInt(OriginParameter, ProcressParameter: Integer): Integer;
function umlPercentageToStr(OriginParameter, ProcressParameter: Integer): TPascalString;
function umlSmartSizeToStr(Size: Int64): TPascalString;
function umlIntToStr(Parameter: Double): TPascalString; overload;
function umlIntToStr(Parameter: Int64): TPascalString; overload;
function umlSizeToStr(Parameter: Int64): TPascalString;
function umlTimeToStr(TimeInteger: Integer): TPascalString;
function umlDateToStr(TimeInteger: Integer): TPascalString;
function umlFloatToStr(f: Double): TPascalString;

function umlStrToInt(const _V: TPascalString): Integer; overload;
function umlStrToInt(const _V: TPascalString; _Def: Integer): Integer; overload;
function umlStrToInt(const _V: TPascalString; _Def: Double): Integer; overload;
function umlStrToFloat(const _V: TPascalString; _Def: Double): Double; overload;

function umlMultipleMatch(IgnoreCase: Boolean; const SourceStr, TargetStr, umlMultipleString, umlMultipleCharacter: TPascalString): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlMultipleMatch(IgnoreCase: Boolean; const SourceStr, TargetStr: TPascalString): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlMultipleMatch(const SourceStr, TargetStr: TPascalString): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlMultipleMatch(const ValueCheck: TArrayPascalString; Value: TPascalString): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlSearchMatch(const SourceStr, TargetStr: TPascalString): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlSearchMatch(const ValueCheck: TArrayPascalString; Value: TPascalString): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

{ De Time Double Code }
function umlDeTimeCodeToStr(NowDateTime: TDateTime): TPascalString;

{ StringReplace replaces occurances of <oldpattern> with <newpattern> in a
  given TPascalString.  Assumes the TPascalString may contain Multibyte characters }
function umlStringReplace(const S, OldPattern, NewPattern: TPascalString; IgnoreCase: Boolean): TPascalString;
function umlCharReplace(const S: TPascalString; OldPattern, NewPattern: umlChar): TPascalString;

function umlEncodeText2HTML(const psSrc: TPascalString): TPascalString; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function umlURLEncode(const AValue: TPascalString): TPascalString;

procedure umlBase64EncodeBytes(var Sour, Dest: TBytes); overload;
procedure umlBase64EncodeBytes(var Sour: TBytes; var Dest: TPascalString); overload;

procedure umlBase64DecodeBytes(var Sour, Dest: TBytes); overload;
procedure umlBase64DecodeBytes(const Sour: TPascalString; var Dest: TBytes); overload;

procedure umlDecodeLineBASE64(const Buffer: TPascalString; var Output: TPascalString);
procedure umlEncodeLineBASE64(const Buffer: TPascalString; var Output: TPascalString);
procedure umlDecodeStreamBASE64(const Buffer: TPascalString; Output: TCoreClassStream);
procedure umlEncodeStreamBASE64(Buffer: TCoreClassStream; var Output: TPascalString);
procedure umlDivisionBase64Text(const Buffer: TPascalString; width: Integer; DivisionAsPascalString: Boolean; var Output: TPascalString);

type
  PMD5 = ^TMD5;
  TMD5 = array [0 .. 15] of Byte;

const
  NullMD5: TMD5 = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);

function umlMD5(const BuffPtr: PBYTE; BufSiz: NativeUInt): TMD5;
function umlMD5Char(const BuffPtr: PBYTE; const BuffSize: NativeUInt): TPascalString;
function umlMD5String(const BuffPtr: PBYTE; const BuffSize: NativeUInt): TPascalString;
function umlStreamMD5(Stream: TCoreClassStream; StartPos, EndPos: Int64): TMD5; overload;
function umlStreamMD5(Stream: TCoreClassStream): TMD5; overload;
function umlStreamMD5Char(Stream: TCoreClassStream): TPascalString; overload;
function umlStreamMD5String(Stream: TCoreClassStream): TPascalString; overload;
function umlStringMD5(const Value: TPascalString): TMD5; overload;
function umlStringMD5Char(const Value: TPascalString): TPascalString; overload;
function umlMD52Str(md5: TMD5): TPascalString;
function umlMD52String(md5: TMD5): TPascalString;
function umlMD5Compare(const m1, m2: TMD5): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlCompareMD5(const m1, m2: TMD5): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlIsNullMD5(m: TMD5): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlWasNullMD5(m: TMD5): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}


const
  CRC16Table: array [0 .. 255] of Word = (
    $0000, $C0C1, $C181, $0140, $C301, $03C0, $0280, $C241, $C601, $06C0, $0780,
    $C741, $0500, $C5C1, $C481, $0440, $CC01, $0CC0, $0D80, $CD41, $0F00, $CFC1,
    $CE81, $0E40, $0A00, $CAC1, $CB81, $0B40, $C901, $09C0, $0880, $C841, $D801,
    $18C0, $1980, $D941, $1B00, $DBC1, $DA81, $1A40, $1E00, $DEC1, $DF81, $1F40,
    $DD01, $1DC0, $1C80, $DC41, $1400, $D4C1, $D581, $1540, $D701, $17C0, $1680,
    $D641, $D201, $12C0, $1380, $D341, $1100, $D1C1, $D081, $1040, $F001, $30C0,
    $3180, $F141, $3300, $F3C1, $F281, $3240, $3600, $F6C1, $F781, $3740, $F501,
    $35C0, $3480, $F441, $3C00, $FCC1, $FD81, $3D40, $FF01, $3FC0, $3E80, $FE41,
    $FA01, $3AC0, $3B80, $FB41, $3900, $F9C1, $F881, $3840, $2800, $E8C1, $E981,
    $2940, $EB01, $2BC0, $2A80, $EA41, $EE01, $2EC0, $2F80, $EF41, $2D00, $EDC1,
    $EC81, $2C40, $E401, $24C0, $2580, $E541, $2700, $E7C1, $E681, $2640, $2200,
    $E2C1, $E381, $2340, $E101, $21C0, $2080, $E041, $A001, $60C0, $6180, $A141,
    $6300, $A3C1, $A281, $6240, $6600, $A6C1, $A781, $6740, $A501, $65C0, $6480,
    $A441, $6C00, $ACC1, $AD81, $6D40, $AF01, $6FC0, $6E80, $AE41, $AA01, $6AC0,
    $6B80, $AB41, $6900, $A9C1, $A881, $6840, $7800, $B8C1, $B981, $7940, $BB01,
    $7BC0, $7A80, $BA41, $BE01, $7EC0, $7F80, $BF41, $7D00, $BDC1, $BC81, $7C40,
    $B401, $74C0, $7580, $B541, $7700, $B7C1, $B681, $7640, $7200, $B2C1, $B381,
    $7340, $B101, $71C0, $7080, $B041, $5000, $90C1, $9181, $5140, $9301, $53C0,
    $5280, $9241, $9601, $56C0, $5780, $9741, $5500, $95C1, $9481, $5440, $9C01,
    $5CC0, $5D80, $9D41, $5F00, $9FC1, $9E81, $5E40, $5A00, $9AC1, $9B81, $5B40,
    $9901, $59C0, $5880, $9841, $8801, $48C0, $4980, $8941, $4B00, $8BC1, $8A81,
    $4A40, $4E00, $8EC1, $8F81, $4F40, $8D01, $4DC0, $4C80, $8C41, $4400, $84C1,
    $8581, $4540, $8701, $47C0, $4680, $8641, $8201, $42C0, $4380, $8341, $4100,
    $81C1, $8081, $4040
    );

function umlCRC16(const Value: PBYTE; const Count: NativeUInt): Word; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlStringCRC16(const Value: TPascalString): Word; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlStreamCRC16(Stream: TMixedStream; StartPos, EndPos: Int64): Word; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlStreamCRC16(Stream: TMixedStream): Word; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}


const
  CRC32Table: array [0 .. 255] of Cardinal = (
    $00000000, $77073096, $EE0E612C, $990951BA, $076DC419, $706AF48F, $E963A535,
    $9E6495A3, $0EDB8832, $79DCB8A4, $E0D5E91E, $97D2D988, $09B64C2B, $7EB17CBD,
    $E7B82D07, $90BF1D91, $1DB71064, $6AB020F2, $F3B97148, $84BE41DE, $1ADAD47D,
    $6DDDE4EB, $F4D4B551, $83D385C7, $136C9856, $646BA8C0, $FD62F97A, $8A65C9EC,
    $14015C4F, $63066CD9, $FA0F3D63, $8D080DF5, $3B6E20C8, $4C69105E, $D56041E4,
    $A2677172, $3C03E4D1, $4B04D447, $D20D85FD, $A50AB56B, $35B5A8FA, $42B2986C,
    $DBBBC9D6, $ACBCF940, $32D86CE3, $45DF5C75, $DCD60DCF, $ABD13D59, $26D930AC,
    $51DE003A, $C8D75180, $BFD06116, $21B4F4B5, $56B3C423, $CFBA9599, $B8BDA50F,
    $2802B89E, $5F058808, $C60CD9B2, $B10BE924, $2F6F7C87, $58684C11, $C1611DAB,
    $B6662D3D, $76DC4190, $01DB7106, $98D220BC, $EFD5102A, $71B18589, $06B6B51F,
    $9FBFE4A5, $E8B8D433, $7807C9A2, $0F00F934, $9609A88E, $E10E9818, $7F6A0DBB,
    $086D3D2D, $91646C97, $E6635C01, $6B6B51F4, $1C6C6162, $856530D8, $F262004E,
    $6C0695ED, $1B01A57B, $8208F4C1, $F50FC457, $65B0D9C6, $12B7E950, $8BBEB8EA,
    $FCB9887C, $62DD1DDF, $15DA2D49, $8CD37CF3, $FBD44C65, $4DB26158, $3AB551CE,
    $A3BC0074, $D4BB30E2, $4ADFA541, $3DD895D7, $A4D1C46D, $D3D6F4FB, $4369E96A,
    $346ED9FC, $AD678846, $DA60B8D0, $44042D73, $33031DE5, $AA0A4C5F, $DD0D7CC9,
    $5005713C, $270241AA, $BE0B1010, $C90C2086, $5768B525, $206F85B3, $B966D409,
    $CE61E49F, $5EDEF90E, $29D9C998, $B0D09822, $C7D7A8B4, $59B33D17, $2EB40D81,
    $B7BD5C3B, $C0BA6CAD, $EDB88320, $9ABFB3B6, $03B6E20C, $74B1D29A, $EAD54739,
    $9DD277AF, $04DB2615, $73DC1683, $E3630B12, $94643B84, $0D6D6A3E, $7A6A5AA8,
    $E40ECF0B, $9309FF9D, $0A00AE27, $7D079EB1, $F00F9344, $8708A3D2, $1E01F268,
    $6906C2FE, $F762575D, $806567CB, $196C3671, $6E6B06E7, $FED41B76, $89D32BE0,
    $10DA7A5A, $67DD4ACC, $F9B9DF6F, $8EBEEFF9, $17B7BE43, $60B08ED5, $D6D6A3E8,
    $A1D1937E, $38D8C2C4, $4FDFF252, $D1BB67F1, $A6BC5767, $3FB506DD, $48B2364B,
    $D80D2BDA, $AF0A1B4C, $36034AF6, $41047A60, $DF60EFC3, $A867DF55, $316E8EEF,
    $4669BE79, $CB61B38C, $BC66831A, $256FD2A0, $5268E236, $CC0C7795, $BB0B4703,
    $220216B9, $5505262F, $C5BA3BBE, $B2BD0B28, $2BB45A92, $5CB36A04, $C2D7FFA7,
    $B5D0CF31, $2CD99E8B, $5BDEAE1D, $9B64C2B0, $EC63F226, $756AA39C, $026D930A,
    $9C0906A9, $EB0E363F, $72076785, $05005713, $95BF4A82, $E2B87A14, $7BB12BAE,
    $0CB61B38, $92D28E9B, $E5D5BE0D, $7CDCEFB7, $0BDBDF21, $86D3D2D4, $F1D4E242,
    $68DDB3F8, $1FDA836E, $81BE16CD, $F6B9265B, $6FB077E1, $18B74777, $88085AE6,
    $FF0F6A70, $66063BCA, $11010B5C, $8F659EFF, $F862AE69, $616BFFD3, $166CCF45,
    $A00AE278, $D70DD2EE, $4E048354, $3903B3C2, $A7672661, $D06016F7, $4969474D,
    $3E6E77DB, $AED16A4A, $D9D65ADC, $40DF0B66, $37D83BF0, $A9BCAE53, $DEBB9EC5,
    $47B2CF7F, $30B5FFE9, $BDBDF21C, $CABAC28A, $53B39330, $24B4A3A6, $BAD03605,
    $CDD70693, $54DE5729, $23D967BF, $B3667A2E, $C4614AB8, $5D681B02, $2A6F2B94,
    $B40BBE37, $C30C8EA1, $5A05DF1B, $2D02EF8D
    );

function umlCRC32(const Value: PBYTE; const Count: NativeUInt): Cardinal; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlString2CRC32(const Value: TPascalString): Cardinal; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlStreamCRC32(Stream: TMixedStream; StartPos, EndPos: Int64): Cardinal; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function umlStreamCRC32(Stream: TMixedStream): Cardinal; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}


type
  TDESKey = array [0 .. 7] of Byte;
  PDESKey = ^TDESKey;

const
  NullDES: TDESKey = (0, 0, 0, 0, 0, 0, 0, 0);

  { TRUE to encrypt, FALSE to decrypt }
procedure umlDES(const Input: TDESKey; var Output: TDESKey; const Key: TDESKey; Encrypt: Boolean); overload;
procedure umlDES(DataPtr: Pointer; Size: Cardinal; const Key: TDESKey; Encrypt: Boolean); overload;
procedure umlDES(DataPtr: Pointer; Size: Cardinal; const Key: TPascalString; Encrypt: Boolean); overload;
procedure umlDES(Input, Output: TMixedStream; const Key: TDESKey; Encrypt: Boolean); overload;
procedure umlDES(Input, Output: TMixedStream; const Key: TPascalString; Encrypt: Boolean); overload;
function umlDESCompare(const d1, d2: TDESKey): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure umlFastSymbol(DataPtr: Pointer; Size: Cardinal; const Key: TDESKey; Encrypt: Boolean); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure umlFastSymbol(DataPtr: Pointer; Size: Cardinal; const Key: TPascalString; Encrypt: Boolean); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function umlTrimSpace(const S: TPascalString): TPascalString; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function umlSeparatorText(AText: TPascalString; Dest: TCoreClassStrings; SeparatorChar: TPascalString): Integer;
function umlStringsMatchText(OriginValue: TCoreClassStrings; DestValue: TPascalString; IgnoreCase: Boolean = True): Boolean;

function umlStringsInExists(Dest: TCoreClassStrings; SText: TPascalString; IgnoreCase: Boolean = True): Boolean;
function umlTextInStrings(const SText: TPascalString; Dest: TCoreClassStrings; IgnoreCase: Boolean = True): Boolean;

function umlAddNewStrTo(SourceStr: TPascalString; Dest: TCoreClassStrings; IgnoreCase: Boolean = True): Boolean;
function umlDeleteStrings(const SText: TPascalString; Dest: TCoreClassStrings; IgnoreCase: Boolean = True): Integer;
function umlDeleteStringsNot(const SText: TPascalString; Dest: TCoreClassStrings; IgnoreCase: Boolean = True): Integer;
function umlMergeStrings(Source, Dest: TCoreClassStrings; IgnoreCase: Boolean = True): Integer;

function umlConverStrToFileName(const Value: TPascalString): TPascalString;

function umlSplitTextMatch(const SText, limit, MatchText: TPascalString; IgnoreCase: Boolean): Boolean;
function umlSplitTextTrimSpaceMatch(const SText, limit, MatchText: TPascalString; IgnoreCase: Boolean): Boolean;
function umlSplitDeleteText(const SText, limit, MatchText: TPascalString; IgnoreCase: Boolean): TPascalString;
function umlSplitTextAsList(const SText, limit: TPascalString; AsLst: TCoreClassStrings): Boolean;
function umlSplitTextAsListAndTrimSpace(const SText, limit: TPascalString; AsLst: TCoreClassStrings): Boolean;
function umlListAsSplitText(const List: TCoreClassStrings; limit: TPascalString): TPascalString;

function umlUpdateComponentName(const Name: TPascalString): TPascalString;
function umlMakeComponentName(OWner: TCoreClassComponent; RefrenceName: TPascalString): TPascalString;

procedure umlReadComponent(Stream: TCoreClassStream; comp: TCoreClassComponent);
procedure umlWriteComponent(Stream: TCoreClassStream; comp: TCoreClassComponent);
procedure umlCopyComponentDataTo(comp, copyto: TCoreClassComponent);

function umlProcessCycleValue(CurrentVal, DeltaVal, StartVal, OverVal: Single; var EndFlag: Boolean): Single; {$IFDEF INLINE_ASM} inline; {$ENDIF}


implementation

uses
  {$IF Defined(WIN32) or Defined(WIN64)}
  Fast_MD5,
  {$ENDIF}
  MemoryStream64;

function umlBytesOf(const S: TPascalString): TBytes;
begin
  Result := BytesOfPascalString(S);
end;

function umlStringOf(const S: TBytes): TPascalString;
begin
  Result := PascalStringOfBytes(S);
end;

function FixedLengthString2Pascal(var S: FixedLengthString): TPascalString;
var
  b: TBytes;
begin
  SetLength(b, FixedLengthStringSize);
  CopyPtr(@S.Data[0], @b[0], FixedLengthStringSize);
  SetLength(b, S.len);
  Result.Bytes := b;
  SetLength(b, 0);
end;

function Pascal2FixedLengthString(var S: TPascalString): FixedLengthString;
var
  bb: TBytes;
begin
  bb := S.Bytes;
  Result.len := Length(bb);
  if Result.len > FixedLengthStringSize then
      Result.len := FixedLengthStringSize
  else
      FillPtrByte(@Result.Data[0], FixedLengthStringSize, 0);
  CopyPtr(@bb[0], @Result.Data[0], Result.len);
end;

function umlComparePosStr(const S: TPascalString; Offset: Integer; const t: TPascalString): Boolean;
begin
  Result := S.ComparePos(Offset, @t);
end;

function umlPos(const SubStr, Str: TPascalString; const Offset: Integer = 1): Integer;
begin
  Result := Str.GetPos(SubStr, Offset);
end;

function umlVarToStr(const V: Variant): TPascalString;
var
  n, b64: TPascalString;
begin
  try
    case VarType(V) of
      varSmallint, varInteger, varShortInt, varByte, varWord, varLongWord:
        begin
          Result := IntToStr(V);
        end;
      varInt64:
        begin
          Result := IntToStr(Int64(V));
        end;
      varUInt64:
        begin
          {$IFDEF FPC}
          Result := IntToStr(UInt64(V));
          {$ELSE}
          Result := UIntToStr(UInt64(V));
          {$ENDIF}
        end;
      varSingle, varDouble, varCurrency, varDate:
        begin
          Result := FloatToStr(V);
        end;
      varOleStr, varString, varUString:
        begin
          n.Text := VarToStr(V);

          if umlExistsLimitChar(n, #10#13#9#8#0) then
            begin
              umlEncodeLineBASE64(n, b64);
              Result := '___base64:' + b64.Text;
            end
          else
              Result := n.Text;
        end;
      varBoolean:
        begin
          Result := BoolToStr(V, True);
        end;
      else
        Result := VarToStr(V);
    end;
  except
    try
        Result := VarToStr(V);
    except
        Result := '';
    end;
  end;
end;

function umlStrToVar(const S: TPascalString): Variant;
var
  n, b64: TPascalString;
begin
  n := umlTrimSpace(S);
  try
    if n.ComparePos(1, '___base64:') then
      begin
        n := umlDeleteFirstStr(n, ':').Text;
        umlDecodeLineBASE64(n, b64);
        Result := b64.Text;
      end
    else
      begin
        case umlGetNumTextType(n) of
          ntBool: Result := StrToBool(n.Text);
          ntInt: Result := StrToInt(n.Text);
          ntInt64: Result := StrToInt64(n.Text);
          {$IFDEF FPC}
          ntUInt64: Result := StrToQWord(n.Text);
          {$ELSE}
          ntUInt64: Result := StrToUInt64(n.Text);
          {$ENDIF}
          ntWord: Result := StrToInt(n.Text);
          ntByte: Result := StrToInt(n.Text);
          ntSmallInt: Result := StrToInt(n.Text);
          ntShortInt: Result := StrToInt(n.Text);
          ntUInt: Result := StrToInt(n.Text);
          ntSingle: Result := StrToFloat(n.Text);
          ntDouble: Result := StrToFloat(n.Text);
          ntCurrency: Result := StrToFloat(n.Text);
          else Result := n.Text;
        end;
      end;
  except
      Result := n.Text;
  end;
end;

function umlMax(const v1, v2: UInt64): UInt64;
begin
  if v1 > v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMax(const v1, v2: Cardinal): Cardinal;
begin
  if v1 > v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMax(const v1, v2: Word): Word;
begin
  if v1 > v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMax(const v1, v2: Byte): Byte;
begin
  if v1 > v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMax(const v1, v2: Int64): Int64;
begin
  if v1 > v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMax(const v1, v2: Integer): Integer;
begin
  if v1 > v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMax(const v1, v2: SmallInt): SmallInt;
begin
  if v1 > v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMax(const v1, v2: ShortInt): ShortInt;
begin
  if v1 > v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMax(const v1, v2: Double): Double;
begin
  if v1 > v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMax(const v1, v2: Single): Single;
begin
  if v1 > v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMin(const v1, v2: UInt64): UInt64;
begin
  if v1 < v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMin(const v1, v2: Cardinal): Cardinal;
begin
  if v1 < v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMin(const v1, v2: Word): Word;
begin
  if v1 < v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMin(const v1, v2: Byte): Byte;
begin
  if v1 < v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMin(const v1, v2: Int64): Int64;
begin
  if v1 < v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMin(const v1, v2: Integer): Integer;
begin
  if v1 < v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMin(const v1, v2: SmallInt): SmallInt;
begin
  if v1 < v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMin(const v1, v2: ShortInt): ShortInt;
begin
  if v1 < v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMin(const v1, v2: Double): Double;
begin
  if v1 < v2 then
      Result := v1
  else
      Result := v2;
end;

function umlMin(const v1, v2: Single): Single;
begin
  if v1 < v2 then
      Result := v1
  else
      Result := v2;
end;

function umlDeltaNumber(const V, delta: NativeInt): NativeInt;
begin
  Result := (V + (delta - 1)) and (not(delta - 1));
end;

function umlGetResourceStream(const FileName: TPascalString): TStream;
var
  n: TPascalString;
begin
  if FileName.Exists('.') then
      n := umlDeleteLastStr(FileName, '.')
  else
      n := FileName;

  Result := TResourceStream.Create(hInstance, n.Text, RT_RCDATA);
end;

function umlSameVarValue(const v1, v2: Variant): Boolean;
begin
  try
      Result := VarSameValue(v1, v2);
  except
      Result := False;
  end;
end;

function umlRandomRange(const aMin, aMax: Integer): Integer;
var
  mn, mx: Integer;
begin
  mn := aMin;
  mx := aMax;

  if mn > mx then
      Inc(mn)
  else
      Inc(mx);

  if mn > mx then
      Result := Random(mn - mx) + mx
  else
      Result := Random(mx - mn) + mn;
end;

function umlRandomRangeF(const aMin, aMax: Single): Single;
begin
  Result := (umlRandomRange(Trunc(aMin * 1000), Trunc(aMax * 1000))) * 0.001;
end;

function umlRandomRangeD(const aMin, aMax: Double): Double;
begin
  Result := (umlRandomRange(Trunc(aMin * 10000), Trunc(aMax * 10000))) * 0.0001;
end;

function umlDefaultTime: Double;
begin
  Result := Now;
end;

function umlDefaultAttrib: Integer;
begin
  Result := 0;
end;

function umlBoolToStr(const Value: Boolean): TPascalString;
begin
  if Value then
      Result := 'YES'
  else
      Result := 'NO';
end;

function umlStrToBool(const Value: TPascalString): Boolean;
var
  NewValue: TPascalString;
begin
  NewValue := umlUpperCase(Value);
  if NewValue = 'YES' then
      Result := True
  else if NewValue = 'NO' then
      Result := False
  else if NewValue = 'TRUE' then
      Result := True
  else if NewValue = 'FALSE' then
      Result := False
  else if NewValue = '1' then
      Result := True
  else if NewValue = '0' then
      Result := False
  else
      Result := False;
end;

function umlFileExists(const FileName: TPascalString): Boolean;
begin
  if FileName.len > 0 then
      Result := FileExists(FileName.Text)
  else
      Result := False;
end;

function umlDirectoryExists(const DirectoryName: TPascalString): Boolean;
begin
  Result := DirectoryExists(DirectoryName.Text);
end;

function umlCreateDirectory(const DirectoryName: TPascalString): Boolean;
begin
  Result := umlDirectoryExists(DirectoryName);
  if not Result then
      Result := ForceDirectories(DirectoryName.Text);
end;

function umlCurrentDirectory: TPascalString;
begin
  Result.Text := GetCurrentDir;
end;

function umlCurrentPath: TPascalString;
begin
  Result.Text := GetCurrentDir;
  case CurrentPlatform of
    epWin32, epWin64: if (Result.len = 0) or (Result.Last <> '\') then
          Result := Result.Text + '\';
    else
      if (Result.len = 0) or (Result.Last <> '/') then
          Result := Result.Text + '/';
  end;
end;

function umlFindFirstFile(const FileName: TPascalString; var SR: TSR): Boolean;
label SearchPoint;
begin
  if FindFirst(FileName.Text, faAnyFile, SR) <> 0 then
    begin
      Result := False;
      Exit;
    end;
  if ((SR.Attr and faDirectory) <> faDirectory) then
    begin
      Result := True;
      Exit;
    end;
SearchPoint:
  if FindNext(SR) <> 0 then
    begin
      Result := False;
      Exit;
    end;
  if ((SR.Attr and faDirectory) <> faDirectory) then
    begin
      Result := True;
      Exit;
    end;
  goto SearchPoint;
end;

function umlFindNextFile(var SR: TSR): Boolean;
label SearchPoint;
begin
SearchPoint:
  if FindNext(SR) <> 0 then
    begin
      Result := False;
      Exit;
    end;
  if ((SR.Attr and faDirectory) <> faDirectory) then
    begin
      Result := True;
      Exit;
    end;
  goto SearchPoint;
end;

function umlFindFirstDir(const DirName: TPascalString; var SR: TSR): Boolean;
label SearchPoint;
begin
  if FindFirst(DirName.Text, faAnyFile, SR) <> 0 then
    begin
      Result := False;
      Exit;
    end;
  if ((SR.Attr and faDirectory) = faDirectory) and (SR.Name <> '.') and (SR.Name <> '..') then
    begin
      Result := True;
      Exit;
    end;
SearchPoint:
  if FindNext(SR) <> 0 then
    begin
      Result := False;
      Exit;
    end;
  if ((SR.Attr and faDirectory) = faDirectory) and (SR.Name <> '.') and (SR.Name <> '..') then
    begin
      Result := True;
      Exit;
    end;
  goto SearchPoint;
end;

function umlFindNextDir(var SR: TSR): Boolean;
label SearchPoint;
begin
SearchPoint:
  if FindNext(SR) <> 0 then
    begin
      Result := False;
      Exit;
    end;
  if ((SR.Attr and faDirectory) = faDirectory) and (SR.Name <> '.') and (SR.Name <> '..') then
    begin
      Result := True;
      Exit;
    end;
  goto SearchPoint;
end;

procedure umlFindClose(var SR: TSR);
begin
  FindClose(SR);
end;

function umlGetFileList(const FullPath: TPascalString; AsLst: TCoreClassStrings): Integer;
var
  _SR: TSR;
begin
  Result := 0;
  if umlFindFirstFile(umlCombineFileName(FullPath, '*'), _SR) then
    begin
      repeat
        AsLst.Add(_SR.Name);
        Inc(Result);
      until not umlFindNextFile(_SR);
    end;
  umlFindClose(_SR);
end;

function umlGetDirList(const FullPath: TPascalString; AsLst: TCoreClassStrings): Integer;
var
  _SR: TSR;
begin
  Result := 0;
  if umlFindFirstDir(umlCombineFileName(FullPath, '*'), _SR) then
    begin
      repeat
        AsLst.Add(_SR.Name);
        Inc(Result);
      until not umlFindNextDir(_SR);
    end;
  umlFindClose(_SR);
end;

function umlGetFileListWithFullPath(const FullPath: TPascalString): umlStringDynArray;
var
  ph: TPascalString;
  ns: TCoreClassStrings;
  i : Integer;
begin
  ph := FullPath;
  ns := TCoreClassStringList.Create;
  umlGetFileList(FullPath, ns);
  SetLength(Result, ns.Count);
  for i := 0 to ns.Count - 1 do
      Result[i] := umlCombineFileName(ph, ns[i]).Text;
  DisposeObject(ns);
end;

function umlGetDirListWithFullPath(const FullPath: TPascalString): umlStringDynArray;
var
  ph: TPascalString;
  ns: TCoreClassStrings;
  i : Integer;
begin
  ph := FullPath;
  ns := TCoreClassStringList.Create;
  umlGetDirList(FullPath, ns);
  SetLength(Result, ns.Count);
  for i := 0 to ns.Count - 1 do
      Result[i] := umlCombinePath(ph, ns[i]).Text;
  DisposeObject(ns);
end;

function umlCombinePath(const s1, s2: TPascalString): TPascalString;
var
  n1, n2, n: TPascalString;
begin
  n1 := umlTrimSpace(s1);
  n2 := umlTrimSpace(s2);
  case CurrentPlatform of
    epWin32, epWin64:
      begin
        n1 := umlCharReplace(n1, '/', '\');
        n2 := umlCharReplace(n2, '/', '\');

        if (n2.len > 0) and (n2.First = '\') then
            n2.DeleteFirst;

        if n1.len > 0 then
          begin
            if n1.Last = '\' then
                Result := n1.Text + n2.Text
            else
                Result := n1.Text + '\' + n2.Text;
          end
        else
            Result := n2;

        repeat
          n := Result;
          Result := umlStringReplace(Result, '\\', '\', True);
        until Result.Same(n);
        if (Result.len > 0) and (Result.Last <> '\') then
            Result.Append('\');
      end;
    else
      begin
        n1 := umlCharReplace(n1, '\', '/');
        n2 := umlCharReplace(n2, '\', '/');

        if (n2.len > 0) and (n2.First = '/') then
            n2.DeleteFirst;

        if n1.len > 0 then
          begin
            if n1.Last = '/' then
                Result := n1.Text + n2.Text
            else
                Result := n1.Text + '/' + n2.Text;
          end
        else
            Result := n2;

        repeat
          n := Result;
          Result := umlStringReplace(Result, '//', '/', True);
        until Result.Same(n);
        if (Result.len > 0) and (Result.Last <> '/') then
            Result.Append('/');
      end;
  end;
end;

function umlCombineFileName(const pathName, FileName: TPascalString): TPascalString;
var
  pn, fn, n: TPascalString;
begin
  pn := umlTrimSpace(pathName);
  fn := umlTrimSpace(FileName);
  case CurrentPlatform of
    epWin32, epWin64:
      begin
        pn := umlCharReplace(pn, '/', '\');
        fn := umlCharReplace(fn, '/', '\');

        if (fn.len > 0) and (fn.First = '\') then
            fn.DeleteFirst;
        if (fn.len > 0) and (fn.Last = '\') then
            fn.DeleteLast;

        if pn.len > 0 then
          begin
            if pn.Last = '\' then
                Result := pn.Text + fn.Text
            else
                Result := pn.Text + '\' + fn.Text;
          end
        else
            Result := fn;

        repeat
          n := Result;
          Result := umlStringReplace(Result, '\\', '\', True);
        until Result.Same(n);

        if Result.Last = '\' then
            Result.DeleteLast;
      end;
    else
      begin
        pn := umlCharReplace(pn, '\', '/');
        fn := umlCharReplace(fn, '\', '/');

        if (fn.len > 0) and (fn.First = '/') then
            fn.DeleteFirst;
        if (fn.len > 0) and (fn.Last = '/') then
            fn.DeleteLast;

        if pn.len > 0 then
          begin
            if pn.Last = '/' then
                Result := pn.Text + fn.Text
            else
                Result := pn.Text + '/' + fn.Text;
          end
        else
            Result := fn;

        repeat
          n := Result;
          Result := umlStringReplace(Result, '//', '/', True);
        until Result.Same(n);
      end;
  end;
end;

function umlGetFileName(const S: TPascalString): TPascalString;
var
  n: TPascalString;
begin
  n := umlTrimSpace(S);
  case CurrentPlatform of
    epWin32, epWin64:
      begin
        n := umlCharReplace(umlTrimSpace(S), '/', '\');
        if n.Exists('\') then
            Result := umlGetLastStr(n, '\')
        else
            Result := n;
      end;
    else
      begin
        n := umlCharReplace(umlTrimSpace(S), '\', '/');
        if n.Exists('/') then
            Result := umlGetLastStr(n, '/')
        else
            Result := n;
      end;
  end;
end;

function umlGetFilePath(const S: TPascalString): TPascalString;
var
  n: TPascalString;
begin
  n := umlTrimSpace(n);
  case CurrentPlatform of
    epWin32, epWin64:
      begin
        n := umlCharReplace(umlTrimSpace(S), '/', '\');
        if (n.Last <> '\') and (n.Exists('\')) then
            Result := umlDeleteLastStr(n, '\')
        else
            Result := n;
      end;
    else
      begin
        n := umlCharReplace(umlTrimSpace(S), '\', '/');
        if (n.Last <> '/') and (n.Exists('/')) then
            Result := umlDeleteLastStr(n, '/')
        else
            Result := n;
      end;
  end;
end;

function umlChangeFileExt(const S, ext: TPascalString): TPascalString;
begin
  if (ext.len > 0) and (ext.First <> '.') then
      ext.Text := '.' + ext.Text;
  if umlExistsLimitChar(S, '.') then
      Result := umlDeleteLastStr(S, '.') + ext
  else
      Result := S + ext;
end;

function umlGetFileExt(const S: TPascalString): TPascalString;
begin
  if umlExistsLimitChar(S, '.') then
      Result := '.' + umlGetLastStr(S, '.')
  else
      Result := '';
end;

procedure InitIOHnd(var IOHnd: TIOHnd);
begin
  IOHnd.IsOnlyRead := True;
  IOHnd.OpenFlags := False;
  IOHnd.AutoFree := False;
  IOHnd.Handle := nil;
  IOHnd.Time := 0;
  IOHnd.Attrib := 0;
  IOHnd.Size := 0;
  IOHnd.Position := 0;
  IOHnd.Name := '';
  IOHnd.FlushBuff := nil;
  IOHnd.PrepareReadPosition := -1;
  IOHnd.PrepareReadBuff := nil;
  IOHnd.IORead := 0;
  IOHnd.IOWrite := 0;
  IOHnd.WriteFlag := False;
  IOHnd.Data := nil;
  IOHnd.Return := umlNotError;
end;

function umlFileCreateAsStream(const Name: TPascalString; Stream: TMixedStream; var IOHnd: TIOHnd): Boolean;
begin
  if IOHnd.OpenFlags = True then
    begin
      IOHnd.Return := umlFileIsActive;
      Result := False;
      Exit;
    end;
  Stream.Position := 0;
  IOHnd.Handle := Stream;
  IOHnd.Return := umlNotError;
  IOHnd.Size := 0;
  IOHnd.Position := 0;
  IOHnd.Time := umlDefaultTime;
  IOHnd.Attrib := umlDefaultAttrib;
  IOHnd.Name := name;
  IOHnd.OpenFlags := True;
  IOHnd.IsOnlyRead := False;
  IOHnd.AutoFree := False;
  Result := True;
end;

function umlFileOpenAsStream(const Name: TPascalString; Stream: TMixedStream; var IOHnd: TIOHnd; _OnlyRead: Boolean): Boolean;
begin
  if IOHnd.OpenFlags = True then
    begin
      IOHnd.Return := umlFileIsActive;
      Result := False;
      Exit;
    end;
  Stream.Position := 0;
  IOHnd.Handle := Stream;
  IOHnd.IsOnlyRead := _OnlyRead;
  IOHnd.Return := umlNotError;
  IOHnd.Size := Stream.Size;
  IOHnd.Position := 0;
  IOHnd.Time := umlDefaultTime;
  IOHnd.Attrib := 0;
  IOHnd.Name := name;
  IOHnd.OpenFlags := True;
  IOHnd.AutoFree := False;
  Result := True;
end;

function umlFileCreate(const Name: TPascalString; var IOHnd: TIOHnd): Boolean;
begin
  if IOHnd.OpenFlags = True then
    begin
      IOHnd.Return := umlFileIsActive;
      Result := False;
      Exit;
    end;
  try
      IOHnd.Handle := TCoreClassFileStream.Create(name.Text, fmCreate);
  except
    IOHnd.Handle := nil;
    IOHnd.Return := umlCreateFileError;
    Result := False;
    Exit;
  end;
  IOHnd.Return := umlNotError;
  IOHnd.Size := 0;
  IOHnd.Position := 0;
  IOHnd.Time := Now;
  IOHnd.Attrib := umlDefaultAttrib;
  IOHnd.Name := name;
  IOHnd.OpenFlags := True;
  IOHnd.IsOnlyRead := False;
  IOHnd.AutoFree := True;
  Result := True;
end;

function umlFileOpen(const Name: TPascalString; var IOHnd: TIOHnd; _OnlyRead: Boolean): Boolean;
var
  SR: TSR;
begin
  if IOHnd.OpenFlags = True then
    begin
      IOHnd.Return := umlFileIsActive;
      Result := False;
      Exit;
    end;
  if umlFindFirstFile(name, SR) = False then
    begin
      IOHnd.Return := umlNotFindFile;
      Result := False;
      umlFindClose(SR);
      Exit;
    end;
  try
    if _OnlyRead then
        IOHnd.Handle := TCoreClassFileStream.Create(name.Text, fmOpenRead or fmShareDenyWrite)
    else
        IOHnd.Handle := TCoreClassFileStream.Create(name.Text, fmOpenReadWrite);
  except
    IOHnd.Handle := nil;
    IOHnd.Return := umlOpenFileError;
    Result := False;
    umlFindClose(SR);
    Exit;
  end;
  IOHnd.IsOnlyRead := _OnlyRead;
  IOHnd.Return := umlNotError;
  IOHnd.Size := SR.Size;
  IOHnd.Position := 0;
  if not FileAge(name.Text, IOHnd.Time) then
      IOHnd.Time := Now;
  IOHnd.Attrib := SR.Attr;
  IOHnd.Name := name;
  IOHnd.OpenFlags := True;
  IOHnd.AutoFree := True;
  Result := True;
  umlFindClose(SR);
end;

function umlFileClose(var IOHnd: TIOHnd): Boolean;
begin
  if IOHnd.OpenFlags = False then
    begin
      IOHnd.Return := umlNotOpenFile;
      Result := False;
      Exit;
    end;
  if IOHnd.Handle = nil then
    begin
      IOHnd.Return := umlFileHandleError;
      Result := False;
      Exit;
    end;

  umlFileEndWrite(IOHnd);

  if IOHnd.PrepareReadBuff <> nil then
      DisposeObject(IOHnd.PrepareReadBuff);
  IOHnd.PrepareReadBuff := nil;
  IOHnd.PrepareReadPosition := -1;

  try
    if IOHnd.AutoFree then
        DisposeObject(IOHnd.Handle)
    else
        IOHnd.Handle := nil;
  except
  end;
  IOHnd.Handle := nil;
  IOHnd.Return := umlNotError;
  IOHnd.Time := umlDefaultTime;
  IOHnd.Attrib := umlDefaultAttrib;
  IOHnd.Name := '';
  IOHnd.OpenFlags := False;
  IOHnd.WriteFlag := False;
  Result := True;
end;

function umlFileUpdate(var IOHnd: TIOHnd): Boolean;
begin
  if (IOHnd.OpenFlags = False) or (IOHnd.Handle = nil) then
    begin
      IOHnd.Return := umlFileHandleError;
      Result := False;
      Exit;
    end;

  umlFileEndWrite(IOHnd);
  umlResetPrepareRead(IOHnd);
  IOHnd.WriteFlag := False;

  Result := True;
end;

function umlFileTest(var IOHnd: TIOHnd): Boolean;
begin
  if (IOHnd.OpenFlags = False) or (IOHnd.Handle = nil) then
    begin
      IOHnd.Return := umlFileHandleError;
      Result := False;
      Exit;
    end;
  IOHnd.Return := umlNotError;
  Result := True;
end;

procedure umlResetPrepareRead(var IOHnd: TIOHnd);
begin
  if IOHnd.PrepareReadBuff <> nil then
      DisposeObject(IOHnd.PrepareReadBuff);
  IOHnd.PrepareReadBuff := nil;
  IOHnd.PrepareReadPosition := -1;
end;

function umlFilePrepareRead(var IOHnd: TIOHnd; Size: Int64; var Buffers): Boolean;
var
  m64      : TMemoryStream64;
  preRedSiz: Int64;
begin
  Result := False;

  if not IOHnd.Handle.InheritsFrom(TCoreClassFileStream) then
      Exit;

  if Size > PrepareReadCacheSize then
    begin
      umlResetPrepareRead(IOHnd);
      IOHnd.Handle.Position := IOHnd.Position;
      Exit;
    end;

  if IOHnd.PrepareReadBuff = nil then
      IOHnd.PrepareReadBuff := TMemoryStream64.Create;

  m64 := TMemoryStream64(IOHnd.PrepareReadBuff);

  if (IOHnd.Position < IOHnd.PrepareReadPosition) or (IOHnd.PrepareReadPosition + m64.Size < IOHnd.Position + Size) then
    begin
      // prepare read buffer
      IOHnd.Handle.Position := IOHnd.Position;
      IOHnd.PrepareReadPosition := IOHnd.Position;

      m64.Clear;
      IOHnd.PrepareReadPosition := IOHnd.Handle.Position;
      if IOHnd.Handle.Size - IOHnd.Handle.Position >= PrepareReadCacheSize then
        begin
          Result := m64.CopyFrom(IOHnd.Handle, PrepareReadCacheSize) = PrepareReadCacheSize;
          Inc(IOHnd.IORead, PrepareReadCacheSize);
        end
      else
        begin
          preRedSiz := IOHnd.Handle.Size - IOHnd.Handle.Position;
          Result := m64.CopyFrom(IOHnd.Handle, preRedSiz) = preRedSiz;
          Inc(IOHnd.IORead, preRedSiz);
        end;
    end;

  if (IOHnd.Position >= IOHnd.PrepareReadPosition) and (IOHnd.PrepareReadPosition + m64.Size >= IOHnd.Position + Size) then
    begin
      CopyPtr(Pointer(NativeUInt(m64.Memory) + (IOHnd.Position - IOHnd.PrepareReadPosition)), @Buffers, Size);
      Inc(IOHnd.Position, Size);
      Result := True;
    end
  else
    begin
      // safe process
      umlResetPrepareRead(IOHnd);
      IOHnd.Handle.Position := IOHnd.Position;
      Exit;
    end;
end;

function umlFileRead(var IOHnd: TIOHnd; Size: Int64; var Buffers): Boolean;
var
  BuffPointer: Pointer;
  i          : NativeInt;
  BuffInt    : NativeUInt;
begin
  if not umlFileEndWrite(IOHnd) then
    begin
      Result := False;
      Exit;
    end;

  if Size = 0 then
    begin
      IOHnd.Return := umlNotError;
      Result := True;
      Exit;
    end;

  if umlFilePrepareRead(IOHnd, Size, Buffers) then
    begin
      IOHnd.Return := umlNotError;
      Result := True;
      Exit;
    end;

  try
    if Size > umlMaxFileRecSize then
      begin
        // process Chunk buffer
        BuffInt := NativeUInt(@Buffers);
        BuffPointer := Pointer(BuffInt);
        for i := 1 to (Size div umlMaxFileRecSize) do
          begin
            if IOHnd.Handle.Read(BuffPointer^, umlMaxFileRecSize) <> umlMaxFileRecSize then
              begin
                IOHnd.Return := umlFileReadError;
                Result := False;
                Exit;
              end;
            BuffInt := BuffInt + umlMaxFileRecSize;
            BuffPointer := Pointer(BuffInt);
          end;
        // process buffer rest
        i := Size mod umlMaxFileRecSize;
        if IOHnd.Handle.Read(BuffPointer^, i) <> i then
          begin
            IOHnd.Return := umlFileReadError;
            Result := False;
            Exit;
          end;
        Inc(IOHnd.Position, Size);
        IOHnd.Return := umlNotError;
        Result := True;
        Inc(IOHnd.IORead, Size);
        Exit;
      end;
    if IOHnd.Handle.Read(Buffers, Size) <> Size then
      begin
        IOHnd.Return := umlFileReadError;
        Result := False;
        Exit;
      end;
    Inc(IOHnd.Position, Size);
    IOHnd.Return := umlNotError;
    Result := True;
    Inc(IOHnd.IORead, Size);
  except
    IOHnd.Return := umlFileReadError;
    Result := False;
  end;
end;

function umlFileBeginWrite(var IOHnd: TIOHnd): Boolean;
begin
  Result := True;

  if not umlFileTest(IOHnd) then
      Exit;

  if IOHnd.FlushBuff <> nil then
      Exit;

  if IOHnd.Handle is TCoreClassFileStream then
      IOHnd.FlushBuff := TMemoryStream64.Create;
end;

function umlFileEndWrite(var IOHnd: TIOHnd): Boolean;
var
  m64: TMemoryStream64;
begin
  if IOHnd.FlushBuff <> nil then
    begin
      m64 := TMemoryStream64(IOHnd.FlushBuff);
      IOHnd.FlushBuff := nil;

      if IOHnd.Handle.Write(m64.Memory^, m64.Size) <> m64.Size then
        begin
          IOHnd.Return := umlFileWriteError;
          Result := False;
          Exit;
        end;
      Inc(IOHnd.IOWrite, m64.Size);
      DisposeObject(m64);
    end;
  Result := True;
end;

function umlFileWrite(var IOHnd: TIOHnd; const Size: Int64; var Buffers): Boolean;
var
  BuffPointer: Pointer;
  i          : NativeInt;
  BuffInt    : NativeUInt;
begin
  if (IOHnd.IsOnlyRead) or (not IOHnd.OpenFlags) then
    begin
      IOHnd.Return := umlFileWriteError;
      Result := False;
      Exit;
    end;
  if Size = 0 then
    begin
      IOHnd.Return := umlNotError;
      Result := True;
      Exit;
    end;

  IOHnd.WriteFlag := True;

  umlResetPrepareRead(IOHnd);

  if Size <= $F000 then
      umlFileBeginWrite(IOHnd);

  if IOHnd.FlushBuff <> nil then
    begin
      if TMemoryStream64(IOHnd.FlushBuff).Write64(Buffers, Size) <> Size then
        begin
          IOHnd.Return := umlFileWriteError;
          Result := False;
          Exit;
        end;

      Inc(IOHnd.Position, Size);
      if IOHnd.Position > IOHnd.Size then
          IOHnd.Size := IOHnd.Position;
      IOHnd.Return := umlNotError;
      Result := True;
      Exit;
    end;

  try
    if Size > umlMaxFileRecSize then
      begin
        // process buffer chunk
        BuffInt := NativeUInt(@Buffers);
        BuffPointer := Pointer(BuffInt);
        for i := 1 to (Size div umlMaxFileRecSize) do
          begin
            if IOHnd.Handle.Write(BuffPointer^, umlMaxFileRecSize) <> umlMaxFileRecSize then
              begin
                IOHnd.Return := umlFileWriteError;
                Result := False;
                Exit;
              end;
            BuffInt := BuffInt + umlMaxFileRecSize;
            BuffPointer := Pointer(BuffInt);
          end;
        // process buffer rest
        i := Size mod umlMaxFileRecSize;
        if IOHnd.Handle.Write(BuffPointer^, i) <> i then
          begin
            IOHnd.Return := umlFileWriteError;
            Result := False;
            Exit;
          end;

        Inc(IOHnd.Position, Size);
        if IOHnd.Position > IOHnd.Size then
            IOHnd.Size := IOHnd.Position;
        IOHnd.Return := umlNotError;
        Result := True;
        Inc(IOHnd.IOWrite, Size);
        Exit;
      end;
    if IOHnd.Handle.Write(Buffers, Size) <> Size then
      begin
        IOHnd.Return := umlFileWriteError;
        Result := False;
        Exit;
      end;

    Inc(IOHnd.Position, Size);
    if IOHnd.Position > IOHnd.Size then
        IOHnd.Size := IOHnd.Position;
    IOHnd.Return := umlNotError;
    Result := True;
    Inc(IOHnd.IOWrite, Size);
  except
    IOHnd.Return := umlFileWriteError;
    Result := False;
  end;
end;

function umlFileWriteStr(var IOHnd: TIOHnd; var Value: TPascalString): Boolean;
var
  buff: FixedLengthString;
begin
  buff := Pascal2FixedLengthString(Value);
  if umlFileWrite(IOHnd, FixedLengthStringSize + FixedLengthStringHeaderSize, buff) = False then
    begin
      IOHnd.Return := umlFileWriteError;
      Result := False;
      Exit;
    end;

  IOHnd.Return := umlNotError;
  Result := True;
end;

function umlFileReadStr(var IOHnd: TIOHnd; var Value: TPascalString): Boolean;
var
  buff: FixedLengthString;
begin
  try
    if umlFileRead(IOHnd, FixedLengthStringSize + FixedLengthStringHeaderSize, buff) = False then
      begin
        IOHnd.Return := umlFileReadError;
        Result := False;
        Exit;
      end;
    Value := FixedLengthString2Pascal(buff);
  except
      Value.Text := '';
  end;

  IOHnd.Return := umlNotError;
  Result := True;
end;

function umlFileSeek(var IOHnd: TIOHnd; APos: Int64): Boolean;
begin
  if (APos <> IOHnd.Position) or (APos <> IOHnd.Handle.Position) then
    if not umlFileEndWrite(IOHnd) then
      begin
        Result := False;
        Exit;
      end;

  IOHnd.Return := umlSeekError;
  Result := False;
  try
    IOHnd.Position := IOHnd.Handle.Seek(APos, TSeekOrigin.soBeginning);
    Result := IOHnd.Position <> -1;
    if Result then
        IOHnd.Return := umlNotError;
  except
  end;
end;

function umlFileGetPOS(var IOHnd: TIOHnd): Int64;
begin
  if (IOHnd.OpenFlags = False) or (IOHnd.Handle = nil) then
    begin
      IOHnd.Return := umlFileHandleError;
      Result := umlFileHandleError;
      Exit;
    end;
  try
      Result := IOHnd.Position;
  except
    IOHnd.Return := umlFileHandleError;
    Result := umlFileHandleError;
  end;
end;

function umlFileGetSize(var IOHnd: TIOHnd): Int64;
begin
  if (IOHnd.OpenFlags = False) or (IOHnd.Handle = nil) then
    begin
      IOHnd.Return := umlFileHandleError;
      Result := 0;
      Exit;
    end;
  Result := IOHnd.Size;
end;

function umlGetFileTime(const FileName: TPascalString): TDateTime;
var
  f: THandle;
begin
  f := FileOpen(FileName.Text, fmOpenRead or fmShareDenyWrite);
  if f = THandle(-1) then
      Result := Now
  else
    begin
      Result := FileDateToDateTime(FileGetDate(f));
      FileClose(f);
    end;
end;

procedure umlSetFileTime(const FileName: TPascalString; newTime: TDateTime);
begin
  FileSetDate(FileName.Text, DateTimeToFileDate(newTime));
end;

function umlGetFileSize(const FileName: TPascalString): Int64;
var
  SR: TSR;
begin
  Result := 0;
  if umlFindFirstFile(FileName, SR) = True then
    begin
      Result := SR.Size;
      while umlFindNextFile(SR) = True do
          Result := Result + SR.Size;
    end;
  umlFindClose(SR);
end;

function umlGetFileCount(const FileName: TPascalString): Integer;
var
  SR: TSR;
begin
  Result := 0;
  if umlFindFirstFile(FileName, SR) = True then
    begin
      Result := Result + 1;
      while umlFindNextFile(SR) = True do
          Result := Result + 1;
    end;
  umlFindClose(SR);
end;

function umlGetFileDateTime(const FileName: TPascalString): TDateTime;
begin
  if not FileAge(FileName.Text, Result, False) then
      Result := Now;
end;

function umlDeleteFile(const FileName: TPascalString; const _VerifyCheck: Boolean = False): Boolean;
var
  _SR: TSR;
begin
  if umlExistsLimitChar(FileName, '*?') then
    begin
      if umlFindFirstFile(FileName, _SR) then
        begin
          repeat
            try
                DeleteFile(umlCombineFileName(FileName, _SR.Name).Text);
            except
            end;
          until not umlFindNextFile(_SR);
        end;
      umlFindClose(_SR);
      Result := True;
    end
  else
    begin
      try
          Result := DeleteFile(FileName.Text);
      except
          Result := False;
      end;
      if Result and _VerifyCheck then
          Result := not umlFileExists(FileName)
      else
          Result := True;
    end;
end;

function umlCopyFile(const SourFile, DestFile: TPascalString): Boolean;
var
  _SH, _DH: TCoreClassFileStream;
begin
  Result := False;
  _SH := nil;
  _DH := nil;
  try
    if not umlFileExists(SourFile) then
        Exit;
    if umlMultipleMatch(True, ExpandFileName(SourFile.Text), ExpandFileName(DestFile.Text)) then
        Exit;
    _SH := TCoreClassFileStream.Create(SourFile.Text, fmOpenRead or fmShareDenyWrite);
    _DH := TCoreClassFileStream.Create(DestFile.Text, fmCreate);
    Result := _DH.CopyFrom(_SH, _SH.Size) = _SH.Size;
    DisposeObject(_SH);
    DisposeObject(_DH);
    umlSetFileTime(DestFile, umlGetFileTime(SourFile));
  except
    if _SH <> nil then
        DisposeObject(_SH);
    if _DH <> nil then
        DisposeObject(_DH);
  end;
end;

function umlRenameFile(const OldName, NewName: TPascalString): Boolean;
begin
  Result := RenameFile(OldName.Text, NewName.Text);
end;

procedure umlSetLength(var aStr: TPascalString; len: Integer);
begin
  aStr.len := len;
end;

procedure umlSetLength(var aStr: umlBytes; len: Integer);
begin
  SetLength(aStr, len);
end;

procedure umlSetLength(var aStr: TArrayPascalString; len: Integer);
begin
  SetLength(aStr, len);
end;

function umlGetLength(const aStr: TPascalString): Integer;
begin
  Result := aStr.len;
end;

function umlGetLength(var aStr: umlBytes): Integer;
begin
  Result := Length(aStr);
end;

function umlGetLength(const aStr: TArrayPascalString): Integer;
begin
  Result := Length(aStr);
end;

function umlUpperCase(const Str: TPascalString): TPascalString;
begin
  Result := UpperCase(Str.Text);
end;

function umlLowerCase(const Str: TPascalString): TPascalString;
begin
  Result := LowerCase(Str.Text);
end;

function umlCopyStr(const aStr: TPascalString; MainPosition, LastPosition: Integer): TPascalString;
begin
  Result := aStr.GetString(MainPosition, LastPosition);
end;

function umlSameText(const s1, s2: TPascalString): Boolean;
begin
  Result := s1.Same(s2);
end;

function umlDeleteChar(const SText, ch: TPascalString): TPascalString;
var
  i: Integer;
begin
  Result := '';
  if SText.len > 0 then
    for i := 1 to SText.len do
      if not CharIn(SText[i], ch) then
          Result.Append(SText[i]);
end;

function umlDeleteChar(const SText: TPascalString; const SomeChars: array of SystemChar): TPascalString;
var
  i: Integer;
begin
  Result := '';
  if SText.len > 0 then
    for i := 1 to SText.len do
      if not CharIn(SText[i], SomeChars) then
          Result.Append(SText[i]);
end;

function umlDeleteChar(const SText: TPascalString; const SomeCharsets: TOrdChars): TPascalString; overload;
var
  i: Integer;
begin
  Result := '';
  if SText.len > 0 then
    for i := 1 to SText.len do
      if not CharIn(SText[i], SomeCharsets) then
          Result.Append(SText[i]);
end;

function umlGetNumberCharInText(const n: TPascalString): TPascalString;
var
  i: Integer;
begin
  Result := '';
  i := 0;
  if n.len = 0 then
      Exit;

  while i <= n.len do
    begin
      if (not CharIn(n[i], c0to9)) then
        begin
          if (Result.len = 0) then
              Inc(i)
          else
              Exit;
        end
      else
        begin
          Result.Append(n[i]);
          Inc(i);
        end;
    end;
end;

function umlMatchLimitChar(CharValue: umlChar; LimitValue: umlPString): Boolean;
begin
  Result := CharIn(CharValue, LimitValue);
end;

function umlMatchLimitChar(CharValue: umlChar; LimitValue: TPascalString): Boolean;
begin
  Result := CharIn(CharValue, @LimitValue);
end;

function umlExistsLimitChar(StrValue: TPascalString; LimitValue: TPascalString): Boolean;
var
  c: SystemChar;
begin
  Result := True;
  for c in StrValue.buff do
    if CharIn(c, @LimitValue) then
        Exit;
  Result := False;
end;

function umlExistsChar(StrValue: TPascalString; LimitValue: TPascalString): Boolean;
var
  c: SystemChar;
begin
  Result := True;
  for c in StrValue.buff do
    if CharIn(c, @LimitValue) then
        Exit;
  Result := False;
end;

function umlTrimChar(const S, limitS: TPascalString): TPascalString;
var
  l, bp, ep: Integer;
begin
  Result := '';
  l := S.len;
  if l > 0 then
    begin
      bp := 1;
      while CharIn(S[bp], @limitS) do
        begin
          Inc(bp);
          if (bp > l) then
            begin
              Result := '';
              Exit;
            end;
        end;
      if bp > l then
          Result := ''
      else
        begin
          ep := l;

          while CharIn(S[ep], @limitS) do
            begin
              Dec(ep);
              if (ep < 1) then
                begin
                  Result := '';
                  Exit;
                end;
            end;
          Result := S.GetString(bp, ep + 1);
        end;
    end;
end;

function umlGetFirstStr(const aStr, limitS: TPascalString): TPascalString;
var
  umlGetFirstName_PrevPos, umlGetFirstName_Pos: Integer;
begin
  Result := aStr;
  if Result.len <= 1 then
    begin
      Exit;
    end;
  umlGetFirstName_Pos := 1;
  while umlMatchLimitChar(Result[umlGetFirstName_Pos], @limitS) do
    begin
      if umlGetFirstName_Pos = Result.len then
          Exit;
      Inc(umlGetFirstName_Pos);
    end;
  umlGetFirstName_PrevPos := umlGetFirstName_Pos;
  while not umlMatchLimitChar(Result[umlGetFirstName_Pos], @limitS) do
    begin
      if umlGetFirstName_Pos = Result.len then
        begin
          Result := umlCopyStr(Result, umlGetFirstName_PrevPos, umlGetFirstName_Pos + 1);
          Exit;
        end;
      Inc(umlGetFirstName_Pos);
    end;
  Result := umlCopyStr(Result, umlGetFirstName_PrevPos, umlGetFirstName_Pos);
end;

function umlGetLastStr(const aStr, limitS: TPascalString): TPascalString;
var
  umlGetLastName_PrevPos, umlGetLastName_Pos: Integer;
begin
  Result := aStr;
  umlGetLastName_Pos := Result.len;
  if umlGetLastName_Pos <= 1 then
    begin
      Exit;
    end;
  while umlMatchLimitChar(Result[umlGetLastName_Pos], @limitS) do
    begin
      if umlGetLastName_Pos = 1 then
          Exit;
      Dec(umlGetLastName_Pos);
    end;
  umlGetLastName_PrevPos := umlGetLastName_Pos;
  while not umlMatchLimitChar(Result[umlGetLastName_Pos], @limitS) do
    begin
      if umlGetLastName_Pos = 1 then
        begin
          Result := umlCopyStr(Result, umlGetLastName_Pos, umlGetLastName_PrevPos + 1);
          Exit;
        end;
      Dec(umlGetLastName_Pos);
    end;
  Result := umlCopyStr(Result, umlGetLastName_Pos + 1, umlGetLastName_PrevPos + 1);
end;

function umlDeleteFirstStr(const aStr, limitS: TPascalString): TPascalString;
var
  umlMaskFirstName_Pos: Integer;
begin
  Result := aStr;
  if Result.len <= 1 then
    begin
      Result := '';
      Exit;
    end;
  umlMaskFirstName_Pos := 1;
  while umlMatchLimitChar(Result[umlMaskFirstName_Pos], @limitS) do
    begin
      if umlMaskFirstName_Pos = Result.len then
        begin
          Result := '';
          Exit;
        end;
      Inc(umlMaskFirstName_Pos);
    end;
  while not umlMatchLimitChar(Result[umlMaskFirstName_Pos], @limitS) do
    begin
      if umlMaskFirstName_Pos = Result.len then
        begin
          Result := '';
          Exit;
        end;
      Inc(umlMaskFirstName_Pos);
    end;
  while umlMatchLimitChar(Result[umlMaskFirstName_Pos], @limitS) do
    begin
      if umlMaskFirstName_Pos = Result.len then
        begin
          Result := '';
          Exit;
        end;
      Inc(umlMaskFirstName_Pos);
    end;
  Result := umlCopyStr(Result, umlMaskFirstName_Pos, Result.len + 1);
end;

function umlDeleteLastStr(const aStr, limitS: TPascalString): TPascalString;
var
  umlMaskLastName_Pos: Integer;
begin
  Result := aStr;
  umlMaskLastName_Pos := Result.len;
  if umlMaskLastName_Pos <= 1 then
    begin
      Result := '';
      Exit;
    end;
  while umlMatchLimitChar(Result[umlMaskLastName_Pos], @limitS) do
    begin
      if umlMaskLastName_Pos = 1 then
        begin
          Result := '';
          Exit;
        end;
      Dec(umlMaskLastName_Pos);
    end;
  while not umlMatchLimitChar(Result[umlMaskLastName_Pos], @limitS) do
    begin
      if umlMaskLastName_Pos = 1 then
        begin
          Result := '';
          Exit;
        end;
      Dec(umlMaskLastName_Pos);
    end;
  while umlMatchLimitChar(Result[umlMaskLastName_Pos], @limitS) do
    begin
      if umlMaskLastName_Pos = 1 then
        begin
          Result := '';
          Exit;
        end;
      Dec(umlMaskLastName_Pos);
    end;
  umlSetLength(Result, umlMaskLastName_Pos);
end;

function umlGetIndexStrCount(const aStr, limitS: TPascalString): Integer;
var
  Str : TPascalString;
  APos: Integer;
begin
  Str := aStr;
  Result := 0;
  if Str.len = 0 then
      Exit;
  APos := 1;
  while True do
    begin
      while umlMatchLimitChar(Str[APos], @limitS) do
        begin
          if APos >= Str.len then
              Exit;
          Inc(APos);
        end;
      Inc(Result);
      while not umlMatchLimitChar(Str[APos], @limitS) do
        begin
          if APos >= Str.len then
              Exit;
          Inc(APos);
        end;
    end;
end;

function umlGetIndexStr(const aStr: TPascalString; limitS: TPascalString; Index: Integer): TPascalString;
var
  umlGetIndexName_Repeat: Integer;
begin
  case index of
    - 1:
      begin
        Result := '';
        Exit;
      end;
    0, 1:
      begin
        Result := umlGetFirstStr(aStr, limitS);
        Exit;
      end;
  end;
  if index >= umlGetIndexStrCount(aStr, limitS) then
    begin
      Result := umlGetLastStr(aStr, limitS);
      Exit;
    end;
  Result := aStr;
  for umlGetIndexName_Repeat := 2 to index do
    begin
      Result := umlDeleteFirstStr(Result, limitS);
    end;
  Result := umlGetFirstStr(Result, limitS);
end;

procedure umlGetSplitArray(const _SourText: TPascalString; var _DestArray: TArrayPascalString; const _SplitChar: TPascalString);
var
  i, _IndexCount: Integer;
  SText         : TPascalString;
begin
  SText := _SourText;
  _IndexCount := umlGetIndexStrCount(SText, _SplitChar);
  if (_IndexCount = 0) and (_SourText.len > 0) then
    begin
      SetLength(_DestArray, 1);
      _DestArray[0] := _SourText;
    end
  else
    begin
      SetLength(_DestArray, _IndexCount);
      i := low(_DestArray);
      while i < _IndexCount do
        begin
          _DestArray[i] := umlGetFirstStr(SText, _SplitChar);
          SText := umlDeleteFirstStr(SText, _SplitChar);
          Inc(i);
        end;
    end;
end;

function TArrayPascalStringToText(var _Ary: TArrayPascalString; const _SplitChar: TPascalString): TPascalString;
var
  i: Integer;
begin
  Result := '';
  for i := low(_Ary) to high(_Ary) do
    if i < high(_Ary) then
        Result := Result + _Ary[i] + _SplitChar
    else
        Result := Result + _Ary[i];
end;

function umlStringsToText(_List: TCoreClassStrings; const _SplitChar: TPascalString): TPascalString;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to _List.Count - 1 do
    if i > 0 then
        Result := Result + _SplitChar + _List[i]
    else
        Result := _List[i];
end;

function umlGetFirstStr_M(const aStr, limitS: TPascalString): TPascalString;
var
  umlGetFirstName_PrevPos, umlGetFirstName_Pos: Integer;
begin
  Result := aStr;
  if Result.len <= 1 then
      Exit;
  umlGetFirstName_Pos := 1;
  if umlMatchLimitChar(Result[umlGetFirstName_Pos], @limitS) then
    begin
      Inc(umlGetFirstName_Pos);
      umlGetFirstName_PrevPos := umlGetFirstName_Pos;
    end
  else
    begin
      umlGetFirstName_PrevPos := umlGetFirstName_Pos;
      while not umlMatchLimitChar(Result[umlGetFirstName_Pos], @limitS) do
        begin
          if umlGetFirstName_Pos = Result.len then
            begin
              Result := umlCopyStr(Result, umlGetFirstName_PrevPos, umlGetFirstName_Pos + 1);
              Exit;
            end;
          Inc(umlGetFirstName_Pos);
        end;
    end;
  Result := umlCopyStr(Result, umlGetFirstName_PrevPos, umlGetFirstName_Pos);
end;

function umlDeleteFirstStr_M(const aStr, limitS: TPascalString): TPascalString;
var
  umlMaskFirstName_Pos: Integer;
begin
  Result := aStr;
  if Result.len <= 1 then
    begin
      Result := '';
      Exit;
    end;
  umlMaskFirstName_Pos := 1;
  while not umlMatchLimitChar(Result[umlMaskFirstName_Pos], @limitS) do
    begin
      if umlMaskFirstName_Pos = Result.len then
        begin
          Result := '';
          Exit;
        end;
      Inc(umlMaskFirstName_Pos);
    end;
  if umlMatchLimitChar(Result[umlMaskFirstName_Pos], @limitS) then
      Inc(umlMaskFirstName_Pos);
  Result := umlCopyStr(Result, umlMaskFirstName_Pos, Result.len + 1);
end;

function umlGetLastStr_M(const aStr, limitS: TPascalString): TPascalString;
var
  umlGetLastName_PrevPos, umlGetLastName_Pos: Integer;
begin
  Result := aStr;
  umlGetLastName_Pos := Result.len;
  if umlGetLastName_Pos <= 1 then
      Exit;
  if Result[umlGetLastName_Pos] = limitS then
      Dec(umlGetLastName_Pos);
  umlGetLastName_PrevPos := umlGetLastName_Pos;
  while not umlMatchLimitChar(Result[umlGetLastName_Pos], @limitS) do
    begin
      if umlGetLastName_Pos = 1 then
        begin
          Result := umlCopyStr(Result, umlGetLastName_Pos, umlGetLastName_PrevPos + 1);
          Exit;
        end;
      Dec(umlGetLastName_Pos);
    end;
  Result := umlCopyStr(Result, umlGetLastName_Pos + 1, umlGetLastName_PrevPos + 1);
end;

function umlDeleteLastStr_M(const aStr, limitS: TPascalString): TPascalString;
var
  umlMaskLastName_Pos: Integer;
begin
  Result := aStr;
  umlMaskLastName_Pos := Result.len;
  if umlMaskLastName_Pos <= 1 then
    begin
      Result := '';
      Exit;
    end;
  if umlMatchLimitChar(Result[umlMaskLastName_Pos], @limitS) then
      Dec(umlMaskLastName_Pos);
  while not umlMatchLimitChar(Result[umlMaskLastName_Pos], @limitS) do
    begin
      if umlMaskLastName_Pos = 1 then
        begin
          Result := '';
          Exit;
        end;
      Dec(umlMaskLastName_Pos);
    end;
  umlSetLength(Result, umlMaskLastName_Pos);
end;

function umlGetIndexStrCount_M(const aStr, limitS: TPascalString): Integer;
var
  Str : TPascalString;
  APos: Integer;
begin
  Str := aStr;
  Result := 0;
  if Str.len = 0 then
      Exit;
  APos := 1;
  Result := 1;
  while True do
    begin
      while not umlMatchLimitChar(Str[APos], @limitS) do
        begin
          if APos = Str.len then
              Exit;
          Inc(APos);
        end;
      Inc(Result);
      if APos = Str.len then
          Exit;
      Inc(APos);
    end;
end;

function umlGetIndexStr_M(const aStr: TPascalString; limitS: TPascalString; Index: Integer): TPascalString;
var
  umlGetIndexName_Repeat: Integer;
begin
  case index of
    - 1:
      begin
        Result := '';
        Exit;
      end;
    0, 1:
      begin
        Result := umlGetFirstStr_M(aStr, limitS);
        Exit;
      end;
  end;
  if index >= umlGetIndexStrCount_M(aStr, limitS) then
    begin
      Result := umlGetLastStr_M(aStr, limitS);
      Exit;
    end;
  Result := aStr;
  for umlGetIndexName_Repeat := 2 to index do
      Result := umlDeleteFirstStr_M(Result, limitS);
  Result := umlGetFirstStr_M(Result, limitS);
end;

function umlGetFirstTextPos(const S: TPascalString; const TextArry: TArrayPascalString; var OutText: TPascalString): Integer;
var
  i, j: Integer;
begin
  Result := -1;
  for i := 1 to S.len do
    begin
      for j := low(TextArry) to high(TextArry) do
        begin
          if S.ComparePos(i, @TextArry[j]) then
            begin
              OutText := TextArry[j];
              Result := i;
              Exit;
            end;
        end;
    end;
end;

function umlDeleteText(const Sour: TPascalString; const bToken, eToken: TArrayPascalString; ANeedBegin, ANeedEnd: Boolean): TPascalString;
var
  ABeginPos, AEndPos           : Integer;
  ABeginText, AEndText, ANewStr: TPascalString;
begin
  Result := Sour;
  if Sour.len > 0 then
    begin
      ABeginPos := umlGetFirstTextPos(Sour, bToken, ABeginText);
      if ABeginPos > 0 then
          ANewStr := umlCopyStr(Sour, ABeginPos + ABeginText.len, Sour.len + 1)
      else if ANeedBegin then
          Exit
      else
          ANewStr := Sour;

      AEndPos := umlGetFirstTextPos(ANewStr, eToken, AEndText);
      if AEndPos > 0 then
          ANewStr := umlCopyStr(ANewStr, (AEndPos + AEndText.len), ANewStr.len + 1)
      else if ANeedEnd then
          Exit
      else
          ANewStr := '';

      if ABeginPos > 0 then
        begin
          if AEndPos > 0 then
              Result := umlCopyStr(Sour, 0, ABeginPos - 1) + umlDeleteText(ANewStr, bToken, eToken, ANeedBegin, ANeedEnd)
          else
              Result := umlCopyStr(Sour, 0, ABeginPos - 1) + ANewStr;
        end
      else if AEndPos > 0 then
          Result := ANewStr;
    end;
end;

function umlGetTextContent(const Sour: TPascalString; const bToken, eToken: TArrayPascalString): TPascalString;
var
  ABeginPos, AEndPos           : Integer;
  ABeginText, AEndText, ANewStr: TPascalString;
begin
  Result := '';
  if Sour.len > 0 then
    begin
      ABeginPos := umlGetFirstTextPos(Sour, bToken, ABeginText);
      if ABeginPos > 0 then
          ANewStr := umlCopyStr(Sour, ABeginPos + ABeginText.len, Sour.len + 1)
      else
          ANewStr := Sour;

      AEndPos := umlGetFirstTextPos(ANewStr, eToken, AEndText);
      if AEndPos > 0 then
          Result := umlCopyStr(ANewStr, 0, AEndPos - 1)
      else
          Result := ANewStr;
    end;
end;

function umlGetNumTextType(const S: TPascalString): TTextType;
type
  TValSym = (vsSymSub, vsSymAdd, vsSymAddSub, vsSymDollar, vsDot, vsDotBeforNum, vsDotAfterNum, vsNum, vsAtoF, vsE, vsUnknow);
var
  cnt: array [TValSym] of Integer;
  n  : TPascalString;
  V  : TValSym;
  c  : SystemChar;
  i  : Integer;
begin
  n := umlTrimSpace(S);
  if n.Same('true') or n.Same('false') then
      Exit(ntBool);

  for V := low(TValSym) to high(TValSym) do
      cnt[V] := 0;

  for i := 1 to n.len do
    begin
      c := n[i];
      if CharIn(c, [c0to9]) then
        begin
          Inc(cnt[vsNum]);
          if cnt[vsDot] > 0 then
              Inc(cnt[vsDotAfterNum]);
        end
      else if CharIn(c, [cLoAtoF, cHiAtoF]) then
        begin
          Inc(cnt[vsAtoF]);
          if CharIn(c, 'eE') then
              Inc(cnt[vsE]);
        end
      else if c = '.' then
        begin
          Inc(cnt[vsDot]);
          cnt[vsDotBeforNum] := cnt[vsNum];
        end
      else if CharIn(c, '-') then
        begin
          Inc(cnt[vsSymSub]);
          Inc(cnt[vsSymAddSub]);
        end
      else if CharIn(c, '+') then
        begin
          Inc(cnt[vsSymAdd]);
          Inc(cnt[vsSymAddSub]);
        end
      else if CharIn(c, '$') and (i = 1) then
          Inc(cnt[vsSymDollar])
      else
          Exit(ntUnknow);
    end;

  if cnt[vsDot] > 1 then
      Exit(ntUnknow);
  if cnt[vsSymDollar] > 1 then
      Exit(ntUnknow);
  if (cnt[vsSymDollar] = 0) and (cnt[vsNum] = 0) then
      Exit(ntUnknow);
  if (cnt[vsSymAdd] > 1) and (cnt[vsE] = 0) and (cnt[vsSymDollar] = 0) then
      Exit(ntUnknow);

  if (cnt[vsSymDollar] = 0) and
    ((cnt[vsDot] = 1) or ((cnt[vsE] = 1) and ((cnt[vsSymAddSub] >= 1) and (cnt[vsSymDollar] = 0)))) then
    begin
      if cnt[vsSymDollar] > 0 then
          Exit(ntUnknow);
      if (cnt[vsAtoF] <> cnt[vsE]) then
          Exit(ntUnknow);

      if cnt[vsE] = 1 then
        begin
          Result := ntDouble
        end
      else if ((cnt[vsDotBeforNum] > 0)) and (cnt[vsDotAfterNum] > 0) then
        begin
          if cnt[vsDotAfterNum] < 5 then
              Result := ntCurrency
          else if cnt[vsNum] > 7 then
              Result := ntDouble
          else
              Result := ntSingle;
        end
      else
          Exit(ntUnknow);
    end
  else
    begin
      if cnt[vsSymDollar] = 1 then
        begin
          if cnt[vsSymSub] > 0 then
            begin
              if cnt[vsNum] + cnt[vsAtoF] < 2 then
                  Result := ntShortInt
              else if cnt[vsNum] + cnt[vsAtoF] < 4 then
                  Result := ntSmallInt
              else if cnt[vsNum] + cnt[vsAtoF] < 7 then
                  Result := ntInt
              else if cnt[vsNum] + cnt[vsAtoF] < 13 then
                  Result := ntInt64
              else
                  Result := ntUnknow;
            end
          else
            begin
              if cnt[vsNum] + cnt[vsAtoF] < 3 then
                  Result := ntByte
              else if cnt[vsNum] + cnt[vsAtoF] < 5 then
                  Result := ntWord
              else if cnt[vsNum] + cnt[vsAtoF] < 8 then
                  Result := ntUInt
              else if cnt[vsNum] + cnt[vsAtoF] < 14 then
                  Result := ntUInt64
              else
                  Result := ntUnknow;
            end;
        end
      else if cnt[vsAtoF] > 0 then
          Exit(ntUnknow)
      else if cnt[vsSymSub] > 0 then
        begin
          if cnt[vsNum] < 3 then
              Result := ntShortInt
          else if cnt[vsNum] < 5 then
              Result := ntSmallInt
          else if cnt[vsNum] < 8 then
              Result := ntInt
          else if cnt[vsNum] < 15 then
              Result := ntInt64
          else
              Result := ntUnknow;
        end
      else
        begin
          if cnt[vsNum] < 3 then
              Result := ntByte
          else if cnt[vsNum] < 5 then
              Result := ntWord
          else if cnt[vsNum] < 8 then
              Result := ntUInt
          else if cnt[vsNum] < 16 then
              Result := ntUInt64
          else
              Result := ntUnknow;
        end;
    end;
end;

function umlIsHex(const aStr: TPascalString): Boolean;
begin
  Result := umlGetNumTextType(aStr) in
    [ntInt, ntInt64, ntUInt64, ntWord, ntByte, ntSmallInt, ntShortInt, ntUInt];
end;

function umlIsNumber(const aStr: TPascalString): Boolean;
begin
  Result := umlGetNumTextType(aStr) <> ntUnknow;
end;

function umlIsIntNumber(const aStr: TPascalString): Boolean;
begin
  Result := umlGetNumTextType(aStr) in
    [ntInt, ntInt64, ntUInt64, ntWord, ntByte, ntSmallInt, ntShortInt, ntUInt];
end;

function umlIsFloatNumber(const aStr: TPascalString): Boolean;
begin
  Result := umlGetNumTextType(aStr) in [ntSingle, ntDouble, ntCurrency];
end;

function umlIsBool(const aStr: TPascalString): Boolean;
begin
  Result := umlGetNumTextType(aStr) = ntBool;
end;

function umlNumberCount(const aStr: TPascalString): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to aStr.len do
    if CharIn(aStr[i], [c0to9]) then
        Inc(Result);
end;

function umlPercentageToFloat(OriginMax, OriginMin, ProcressParameter: Double): Double;
begin
  Result := (ProcressParameter - OriginMin) * 100.0 / (OriginMax - OriginMin);
end;

function umlPercentageToInt(OriginParameter, ProcressParameter: Integer): Integer;
begin
  if OriginParameter = 0 then
      Result := 0
  else
      Result := (Integer(Round((ProcressParameter * 100.0) / OriginParameter)));
end;

function umlPercentageToStr(OriginParameter, ProcressParameter: Integer): TPascalString;
begin
  Result := IntToStr(umlPercentageToInt(OriginParameter, ProcressParameter)) + '%';
end;

function umlSmartSizeToStr(Size: Int64): TPascalString;
begin
  if Size < 1 shl 10 then
      Result := format('%d', [Size])
  else if Size < 1 shl 20 then
      Result := format('%fKb', [Size / (1 shl 10)])
  else if Size < 1 shl 30 then
      Result := format('%fM', [Size / (1 shl 20)])
  else
      Result := format('%fG', [Size / (1 shl 30)])
end;

function umlIntToStr(Parameter: Double): TPascalString;
begin
  Result := IntToStr(Round(Parameter));
end;

function umlIntToStr(Parameter: Int64): TPascalString;
begin
  Result := IntToStr(Parameter);
end;

function umlSizeToStr(Parameter: Int64): TPascalString;
begin
  try
      Result := umlSmartSizeToStr(Parameter);
  except
      Result := IntToStr(Parameter) + ' B';
  end;
end;

function umlTimeToStr(TimeInteger: Integer): TPascalString;
begin
  Result := DateTimeToStr(FileDateToDateTime(TimeInteger));
end;

function umlDateToStr(TimeInteger: Integer): TPascalString;
begin
  Result := DateToStr(FileDateToDateTime(TimeInteger));
end;

function umlFloatToStr(f: Double): TPascalString;
begin
  Result := FloatToStr(f);
end;

function umlStrToInt(const _V: TPascalString): Integer;
begin
  Result := umlStrToInt(_V, 0);
end;

function umlStrToInt(const _V: TPascalString; _Def: Integer): Integer;
begin
  if umlIsNumber(_V) then
      Result := StrToInt(_V.Text)
  else
      Result := _Def;
end;

function umlStrToInt(const _V: TPascalString; _Def: Double): Integer;
begin
  if umlIsNumber(_V) then
      Result := StrToInt(_V.Text)
  else
      Result := Round(_Def);
end;

function umlStrToFloat(const _V: TPascalString; _Def: Double): Double;
begin
  if umlIsNumber(_V) then
      Result := StrToFloat(_V.Text)
  else
      Result := _Def;
end;

function umlMultipleMatch(IgnoreCase: Boolean; const SourceStr, TargetStr, umlMultipleString, umlMultipleCharacter: TPascalString): Boolean;
label CharacterRep_Label, MultipleCharacterRep_Label, MultipleStringRep_Label;
var
  UpperCaseSourceStr, UpperCaseTargetStr, SwapStr                            : TPascalString;
  SourceChar, TargetChar, SwapChar                                           : umlChar;
  SourceIndex, TargetIndex, SwapIndex, SourceLength, TargetLength, SwapLength: Integer;
begin
  SourceLength := SourceStr.len;
  if SourceLength = 0 then
    begin
      Result := True;
      Exit;
    end;

  TargetLength := TargetStr.len;
  if TargetLength = 0 then
    begin
      Result := False;
      Exit;
    end;

  if IgnoreCase then
    begin
      UpperCaseSourceStr := umlUpperCase(SourceStr);
      UpperCaseTargetStr := umlUpperCase(TargetStr);
    end
  else
    begin
      UpperCaseSourceStr := SourceStr;
      UpperCaseTargetStr := TargetStr;
    end;

  if (not umlExistsLimitChar(SourceStr, umlMultipleCharacter)) and (not umlExistsLimitChar(SourceStr, umlMultipleString)) then
    begin
      Result := (SourceLength = TargetLength) and (UpperCaseSourceStr = UpperCaseTargetStr);
      Exit;
    end;
  if SourceLength = 1 then
    begin
      if umlMatchLimitChar(UpperCaseSourceStr[1], @umlMultipleString) then
          Result := True
      else
          Result := False;
      Exit;
    end;
  SourceIndex := 1;
  TargetIndex := 1;
  SourceChar := UpperCaseSourceStr[SourceIndex];
  TargetChar := UpperCaseTargetStr[TargetIndex];
CharacterRep_Label:
  while (SourceChar = TargetChar) and (not umlMatchLimitChar(SourceChar, @umlMultipleCharacter)) and (not umlMatchLimitChar(SourceChar, @umlMultipleString)) do
    begin
      if SourceIndex = SourceLength then
        begin
          if TargetIndex = TargetLength then
            begin
              Result := True;
              Exit;
            end;
          Result := False;
          Exit;
        end;
      if TargetIndex = TargetLength then
        begin
          SourceIndex := SourceIndex + 1;
          if SourceIndex = SourceLength then
            begin
              SourceChar := UpperCaseSourceStr[SourceIndex];
              Result := umlMatchLimitChar(SourceChar, @umlMultipleString) or umlMatchLimitChar(SourceChar, @umlMultipleCharacter);
              Exit;
            end;
          Result := False;
          Exit;
        end;
      SourceIndex := SourceIndex + 1;
      TargetIndex := TargetIndex + 1;
      SourceChar := UpperCaseSourceStr[SourceIndex];
      TargetChar := UpperCaseTargetStr[TargetIndex];
    end;
MultipleCharacterRep_Label:
  while umlMatchLimitChar(SourceChar, @umlMultipleCharacter) do
    begin
      if SourceIndex = SourceLength then
        begin
          if TargetIndex = TargetLength then
            begin
              Result := True;
              Exit;
            end;
          Result := False;
          Exit;
        end;
      if TargetIndex = TargetLength then
        begin
          SourceIndex := SourceIndex + 1;
          SourceChar := UpperCaseSourceStr[SourceIndex];
          if (SourceIndex = SourceLength) and ((umlMatchLimitChar(SourceChar, @umlMultipleString)) or (umlMatchLimitChar(SourceChar, @umlMultipleCharacter))) then
            begin
              Result := True;
              Exit;
            end;
          Result := False;
          Exit;
        end;
      SourceIndex := SourceIndex + 1;
      TargetIndex := TargetIndex + 1;
      SourceChar := UpperCaseSourceStr[SourceIndex];
      TargetChar := UpperCaseTargetStr[TargetIndex];
    end;
MultipleStringRep_Label:
  if umlMatchLimitChar(SourceChar, @umlMultipleString) then
    begin
      if SourceIndex = SourceLength then
        begin
          Result := True;
          Exit;
        end;
      SourceIndex := SourceIndex + 1;
      SourceChar := UpperCaseSourceStr[SourceIndex];

      while (umlMatchLimitChar(SourceChar, @umlMultipleString)) or (umlMatchLimitChar(SourceChar, @umlMultipleCharacter)) do
        begin
          if SourceIndex = SourceLength then
            begin
              Result := True;
              Exit;
            end;
          SourceIndex := SourceIndex + 1;
          SourceChar := UpperCaseSourceStr[SourceIndex];
          while umlMatchLimitChar(SourceChar, @umlMultipleCharacter) do
            begin
              if SourceIndex = SourceLength then
                begin
                  Result := True;
                  Exit;
                end;
              SourceIndex := SourceIndex + 1;
              SourceChar := UpperCaseSourceStr[SourceIndex];
            end;
        end;
      SwapStr := umlCopyStr(UpperCaseSourceStr, SourceIndex, SourceLength + 1);
      SwapLength := SwapStr.len;
      if SwapLength = 0 then
        begin
          Result := (UpperCaseSourceStr[SourceIndex] = umlMultipleString);
          Exit;
        end;
      SwapIndex := 1;
      SwapChar := SwapStr[SwapIndex];
      while (not umlMatchLimitChar(SwapChar, @umlMultipleCharacter)) and (not umlMatchLimitChar(SwapChar, @umlMultipleString)) and (SwapIndex < SwapLength) do
        begin
          SwapIndex := SwapIndex + 1;
          SwapChar := SwapStr[SwapIndex];
        end;
      if (umlMatchLimitChar(SwapChar, @umlMultipleCharacter)) or (umlMatchLimitChar(SwapChar, @umlMultipleString)) then
          SwapStr := umlCopyStr(SwapStr, 1, SwapIndex)
      else
        begin
          SwapStr := umlCopyStr(SwapStr, 1, SwapIndex + 1);
          if SwapStr = '' then
            begin
              Result := False;
              Exit;
            end;
          SwapLength := SwapStr.len;
          SwapIndex := 1;
          SwapChar := SwapStr[SwapLength];
          TargetChar := UpperCaseTargetStr[TargetLength];
          while SwapChar = TargetChar do
            begin
              if SwapIndex = SwapLength then
                begin
                  Result := True;
                  Exit;
                end;
              if SwapIndex = TargetLength then
                begin
                  Result := False;
                  Exit;
                end;
              SwapChar := SwapStr[(SwapLength) - SwapIndex];
              TargetChar := UpperCaseTargetStr[(TargetLength) - SwapIndex];
              SwapIndex := SwapIndex + 1;
            end;
          Result := False;
          Exit;
        end;
      SwapChar := SwapStr[1];
      SwapIndex := 1;
      SwapLength := SwapStr.len;
      while SwapIndex <= SwapLength do
        begin
          if (TargetIndex - 1) + SwapIndex > TargetLength then
            begin
              Result := False;
              Exit;
            end;
          SwapChar := SwapStr[SwapIndex];
          TargetChar := UpperCaseTargetStr[(TargetIndex - 1) + SwapIndex];
          while SwapChar <> TargetChar do
            begin
              if (TargetIndex + SwapLength) > TargetLength then
                begin
                  Result := False;
                  Exit;
                end;
              TargetIndex := TargetIndex + 1;
              SwapIndex := 1;
              SwapChar := SwapStr[SwapIndex];
              TargetChar := UpperCaseTargetStr[(TargetIndex - 1) + SwapIndex];
            end;
          SwapIndex := SwapIndex + 1;
        end;
      TargetIndex := (TargetIndex - 1) + SwapLength;
      SourceIndex := (SourceIndex - 1) + SwapLength;
      TargetChar := SwapChar;
      SourceChar := SwapChar;
    end;
  if SourceChar = TargetChar then
      goto CharacterRep_Label
  else if umlMatchLimitChar(SourceChar, @umlMultipleCharacter) then
      goto MultipleCharacterRep_Label
  else if umlMatchLimitChar(SourceChar, @umlMultipleString) then
      goto MultipleStringRep_Label
  else
      Result := False;
end;

function umlMultipleMatch(IgnoreCase: Boolean; const SourceStr, TargetStr: TPascalString): Boolean;
begin
  if (SourceStr.len > 0) and (SourceStr.Text <> '*') then
      Result := umlMultipleMatch(IgnoreCase, SourceStr, TargetStr, '*', '?')
  else
      Result := True;
end;

function umlMultipleMatch(const SourceStr, TargetStr: TPascalString): Boolean;
var
  fi: TArrayPascalString;
begin
  if (SourceStr.len > 0) and (SourceStr.Text <> '*') then
    begin
      umlGetSplitArray(SourceStr, fi, ';');
      Result := umlMultipleMatch(fi, TargetStr);
    end
  else
      Result := True;
end;

function umlMultipleMatch(const ValueCheck: TArrayPascalString; Value: TPascalString): Boolean;
var
  i: Integer;
begin
  Result := True;
  if umlGetLength(Value) > 0 then
    begin
      if high(ValueCheck) >= 0 then
        begin
          Result := False;
          for i := low(ValueCheck) to high(ValueCheck) do
            begin
              Result := umlMultipleMatch(True, ValueCheck[i], Value);
              if Result then
                  Exit;
            end;
        end;
    end;
end;

function umlSearchMatch(const SourceStr, TargetStr: TPascalString): Boolean;
var
  fi: TArrayPascalString;
begin
  if (SourceStr.len > 0) and (SourceStr.Text <> '*') then
    begin
      umlGetSplitArray(SourceStr, fi, ';');
      Result := umlSearchMatch(fi, TargetStr);
    end
  else
      Result := True;
end;

function umlSearchMatch(const ValueCheck: TArrayPascalString; Value: TPascalString): Boolean;
var
  i: Integer;
begin
  Result := True;
  if umlGetLength(Value) > 0 then
    begin
      if high(ValueCheck) >= 0 then
        begin
          Result := False;
          for i := low(ValueCheck) to high(ValueCheck) do
            begin
              Result := (Value.GetPos(ValueCheck[i]) > 0) or (umlMultipleMatch(True, ValueCheck[i], Value));
              if Result then
                  Exit;
            end;
        end;
    end;
end;

function umlDeTimeCodeToStr(NowDateTime: TDateTime): TPascalString;
var
  Year, Month, Day    : Word;
  Hour, Min, Sec, MSec: Word;
begin
  DecodeDate(NowDateTime, Year, Month, Day);
  DecodeTime(NowDateTime, Hour, Min, Sec, MSec);
  Result := IntToHex(Year, 4) + IntToHex(Month, 2) +
    IntToHex(Day, 2) + IntToHex(Hour, 1) + IntToHex(Min, 2) +
    IntToHex(Sec, 2) + IntToHex(MSec, 3);
end;

function umlStringReplace(const S, OldPattern, NewPattern: TPascalString; IgnoreCase: Boolean): TPascalString;
var
  f: TReplaceFlags;
begin
  f := [rfReplaceAll];
  if IgnoreCase then
      f := f + [rfIgnoreCase];
  Result.Text := StringReplace(S.Text, OldPattern.Text, NewPattern.Text, f);
end;

function umlCharReplace(const S: TPascalString; OldPattern, NewPattern: umlChar): TPascalString;
var
  i: Integer;
begin
  Result := S;
  if Result.len > 0 then
    begin
      for i := 1 to umlGetLength(Result) do
        begin
          if Result[i] = OldPattern then
              Result[i] := NewPattern;
        end;
    end;
end;

function umlEncodeText2HTML(const psSrc: TPascalString): TPascalString;
var
  i: Integer;
begin
  Result := '';
  if psSrc.len > 0 then
    begin
      i := 1;
      while i <= psSrc.len do
        begin
          case psSrc[i] of
            ' ': Result.Append('&nbsp;');
            '<': Result.Append('&lt;');
            '>': Result.Append('&gt;');
            '&': Result.Append('&amp;');
            '"': Result.Append('&quot;');
            #9: Result.Append('&nbsp;&nbsp;&nbsp;&nbsp;');
            #13:
              begin
                if i + 1 <= psSrc.len then
                  begin
                    if psSrc[i + 1] = #10 then
                        Inc(i);
                    Result.Append('<br>');
                  end
                else
                  begin
                    Result.Append('<br>');
                  end;
              end;
            #10:
              begin
                if i + 1 <= psSrc.len then
                  begin
                    if psSrc[i + 1] = #13 then
                        Inc(i);
                    Result.Append('<br>');
                  end
                else
                  begin
                    Result.Append('<br>');
                  end;
              end;
            else
              Result.Append(psSrc[i]);
          end;
          Inc(i);
        end;
    end;
end;

function umlURLEncode(const AValue: TPascalString): TPascalString;
const
  XD: array [0 .. 15] of Char = ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
var
  buff: TBytes;
  i   : Integer;
begin
  buff := AValue.Bytes;
  Result := '';
  i := 0;
  while i < Length(buff) do
    begin
      if (i + 2 < Length(buff)) and (buff[i] = Ord('%')) then
        if CharIn(SystemChar(buff[i + 1]), cHex) and CharIn(SystemChar(buff[i + 2]), cHex) then
          begin
            Result.Append('%' + Char(buff[i + 1]) + Char(buff[i + 2]));
            Inc(i, 3);
            Continue;
          end;

      if (buff[i] > 33) and (buff[i] < 127) then
        begin
          if CharIn(SystemChar(buff[i]), [c0to9, cAtoZ], '_') then
              Result.Append(Char(buff[i]))
          else
              Result.Append('%' + XD[(buff[i] shr 4) and $0F] + XD[buff[i] and $0F]);
        end
      else
          Result.Append('%' + XD[(buff[i] shr 4) and $0F] + XD[buff[i] and $0F]);
      Inc(i);
    end;
end;

type
  TByte4 = packed record
    b1: Byte;
    b2: Byte;
    b3: Byte;
    b4: Byte;
  end;

  PByte4 = ^TByte4;

  TByte3 = packed record
    b1: Byte;
    b2: Byte;
    b3: Byte;
  end;

  PByte3 = ^TByte3;

function umlBase64DecodePartial(const InputBuffer: Pointer; const InputBytesCount: Cardinal; const OutputBuffer: Pointer; var ByteBuffer, ByteBufferSpace: NativeInt): Cardinal;
const
  umlBase64_DECODE_TABLE: array [Byte] of Cardinal = (255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 062, 255, 255, 255, 063, 052, 053, 054, 055, 056, 057, 058, 059, 060, 061, 255, 255,
    255, 255, 255, 255, 255, 000, 001, 002, 003, 004, 005, 006, 007, 008, 009, 010, 011, 012, 013, 014, 015, 016, 017, 018, 019, 020, 021, 022, 023, 024, 025, 255, 255, 255, 255,
    255, 255, 026, 027, 028, 029, 030, 031, 032, 033, 034, 035, 036, 037, 038, 039, 040, 041, 042, 043, 044, 045, 046, 047, 048, 049, 050, 051, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255);
var
  lByteBuffer, lByteBufferSpace, c: Cardinal;
  InPtr, InLimitPtr               : PBYTE;
  OutPtr                          : PByte3;
begin
  if InputBytesCount > 0 then
    begin
      InPtr := InputBuffer;
      NativeUInt(InLimitPtr) := NativeUInt(InPtr) + InputBytesCount;
      OutPtr := OutputBuffer;
      lByteBuffer := ByteBuffer;
      lByteBufferSpace := ByteBufferSpace;
      while InPtr <> InLimitPtr do
        begin
          c := umlBase64_DECODE_TABLE[InPtr^];
          Inc(InPtr);
          if c = $FF then
              Continue;
          lByteBuffer := lByteBuffer shl 6;
          lByteBuffer := lByteBuffer or c;
          Dec(lByteBufferSpace);
          if lByteBufferSpace <> 0 then
              Continue;
          OutPtr^.b3 := Byte(lByteBuffer);
          lByteBuffer := lByteBuffer shr 8;
          OutPtr^.b2 := Byte(lByteBuffer);
          lByteBuffer := lByteBuffer shr 8;
          OutPtr^.b1 := Byte(lByteBuffer);
          lByteBuffer := 0;
          Inc(OutPtr);
          lByteBufferSpace := 4;
        end;
      ByteBuffer := lByteBuffer;
      ByteBufferSpace := lByteBufferSpace;
      Result := Cardinal(OutPtr) - Cardinal(OutputBuffer);
    end
  else
      Result := 0;
end;

function umlBase64DecodePartialEnd(const OutputBuffer: Pointer; const ByteBuffer: NativeInt; const ByteBufferSpace: NativeInt): Cardinal;
var
  lByteBuffer: Cardinal;
begin
  case ByteBufferSpace of
    1:
      begin
        lByteBuffer := ByteBuffer shr 2;
        PByte3(OutputBuffer)^.b2 := Byte(lByteBuffer);
        lByteBuffer := lByteBuffer shr 8;
        PByte3(OutputBuffer)^.b1 := Byte(lByteBuffer);
        Result := 2;
      end;
    2:
      begin
        lByteBuffer := ByteBuffer shr 4;
        PByte3(OutputBuffer)^.b1 := Byte(lByteBuffer);
        Result := 1;
      end;
    else
      Result := 0;
  end;
end;

procedure umlBase64Encode(const InputBuffer: Pointer; const InputByteCount: NativeInt; const OutputBuffer: Pointer);
const
  EQUAL_SIGN  = Byte('=');
  BUFFER_SIZE = $3000;

  umlBase64_ENCODE_TABLE: array [0 .. 63] of Byte = (065, 066, 067, 068, 069, 070, 071, 072, 073, 074, 075, 076, 077, 078, 079, 080, 081, 082, 083, 084, 085, 086, 087, 088, 089,
    090, 097, 098, 099, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 048, 049, 050, 051, 052, 053, 054, 055,
    056, 057, 043, 047);
var
  b, InMax3        : NativeUInt;
  InPtr, InLimitPtr: ^Byte;
  OutPtr           : PByte4;
begin
  if InputByteCount <= 0 then
      Exit;
  InPtr := InputBuffer;
  InMax3 := InputByteCount div 3 * 3;
  OutPtr := OutputBuffer;
  NativeUInt(InLimitPtr) := NativeUInt(InPtr) + InMax3;
  while InPtr <> InLimitPtr do
    begin
      b := InPtr^;
      b := b shl 8;
      Inc(InPtr);
      b := b or InPtr^;
      b := b shl 8;
      Inc(InPtr);
      b := b or InPtr^;
      Inc(InPtr);
      OutPtr^.b4 := umlBase64_ENCODE_TABLE[b and $3F];
      b := b shr 6;
      OutPtr^.b3 := umlBase64_ENCODE_TABLE[b and $3F];
      b := b shr 6;
      OutPtr^.b2 := umlBase64_ENCODE_TABLE[b and $3F];
      b := b shr 6;
      OutPtr^.b1 := umlBase64_ENCODE_TABLE[b];
      Inc(OutPtr);
    end;

  case InputByteCount - InMax3 of
    1:
      begin
        b := InPtr^;
        b := b shl 4;
        OutPtr^.b2 := umlBase64_ENCODE_TABLE[b and $3F];
        b := b shr 6;
        OutPtr^.b1 := umlBase64_ENCODE_TABLE[b];
        OutPtr^.b3 := EQUAL_SIGN;
        OutPtr^.b4 := EQUAL_SIGN;
      end;
    2:
      begin
        b := InPtr^;
        Inc(InPtr);
        b := b shl 8;
        b := b or InPtr^;
        b := b shl 2;
        OutPtr^.b3 := umlBase64_ENCODE_TABLE[b and $3F];
        b := b shr 6;
        OutPtr^.b2 := umlBase64_ENCODE_TABLE[b and $3F];
        b := b shr 6;
        OutPtr^.b1 := umlBase64_ENCODE_TABLE[b];
        OutPtr^.b4 := EQUAL_SIGN;
      end;
  end;
end;

procedure umlBase64EncodeBytes(var Sour, Dest: TBytes);
var
  l: NativeInt;
begin
  l := Length(Sour);
  if l > 0 then
    begin
      SetLength(Dest, (l + 2) div 3 * 4);
      umlBase64Encode(@Sour[0], l, @Dest[0]);
    end;
end;

procedure umlBase64EncodeBytes(var Sour: TBytes; var Dest: TPascalString);
var
  buff: TBytes;
begin
  umlBase64EncodeBytes(Sour, buff);
  Dest.Bytes := buff;
end;

procedure umlBase64DecodeBytes(var Sour, Dest: TBytes);
var
  ByteBuffer, ByteBufferSpace: NativeInt;
  l                          : NativeInt;
begin
  l := Length(Sour);
  if l > 0 then
    begin
      SetLength(Dest, (l + 3) div 4 * 3);
      ByteBuffer := 0;
      ByteBufferSpace := 4;
      l := umlBase64DecodePartial(@Sour[0], l, @Dest[0], ByteBuffer, ByteBufferSpace);
      Inc(l, umlBase64DecodePartialEnd(Pointer(NativeUInt(@Dest[0]) + l), ByteBuffer, ByteBufferSpace));
      SetLength(Dest, l);
    end;
end;

procedure umlBase64DecodeBytes(const Sour: TPascalString; var Dest: TBytes);
var
  buff: TBytes;
begin
  buff := Sour.Bytes;
  umlBase64DecodeBytes(buff, Dest);
end;

procedure umlDecodeLineBASE64(const Buffer: TPascalString; var Output: TPascalString);
var
  b, nb: TBytes;
begin
  b := umlBytesOf(Buffer);
  umlBase64DecodeBytes(b, nb);
  Output := umlStringOf(nb);
end;

procedure umlEncodeLineBASE64(const Buffer: TPascalString; var Output: TPascalString);
var
  b, nb: TBytes;
begin
  b := umlBytesOf(Buffer);
  umlBase64EncodeBytes(b, nb);
  Output := umlStringOf(nb);
end;

procedure umlDecodeStreamBASE64(const Buffer: TPascalString; Output: TCoreClassStream);
var
  b, nb: TBytes;
  bak  : Int64;
begin
  b := umlBytesOf(Buffer);
  umlBase64DecodeBytes(b, nb);
  bak := Output.Position;
  Output.WriteBuffer(nb[0], Length(nb));
  Output.Position := bak;
end;

procedure umlEncodeStreamBASE64(Buffer: TCoreClassStream; var Output: TPascalString);
var
  b, nb: TBytes;
  bak  : Int64;
begin
  bak := Buffer.Position;

  Buffer.Position := 0;
  SetLength(b, Buffer.Size);
  Buffer.ReadBuffer(b[0], Buffer.Size);
  umlBase64EncodeBytes(b, nb);
  Output := umlStringOf(nb);

  Buffer.Position := bak;
end;

procedure umlDivisionBase64Text(const Buffer: TPascalString; width: Integer; DivisionAsPascalString: Boolean; var Output: TPascalString);
var
  i, n: Integer;
begin
  Output := '';
  n := 0;
  for i := 1 to Buffer.len do
    begin
      if (DivisionAsPascalString) and (n = 0) then
          Output.Append('''');

      Output.Append(Buffer[i]);
      Inc(n);
      if n = width then
        begin
          if DivisionAsPascalString then
              Output.Append('''' + '+' + #13#10)
          else
              Output.Append(#13#10);
          n := 0;
        end;
    end;
  if DivisionAsPascalString then
      Output.Append('''' + ';');
end;

procedure umlTransformMD5(var Accu; var buf); inline;
  function ROL(const x: Cardinal; const n: Byte): Cardinal; inline;
  begin
    Result := (x shl n) or (x shr (32 - n))
  end;

  function FF(const a, b, c, d, x: Cardinal; const S: Byte; const ac: Cardinal): Cardinal; inline;
  begin
    Result := ROL(a + x + ac + (b and c or not b and d), S) + b
  end;

  function GG(const a, b, c, d, x: Cardinal; const S: Byte; const ac: Cardinal): Cardinal; inline;
  begin
    Result := ROL(a + x + ac + (b and d or c and not d), S) + b
  end;

  function HH(const a, b, c, d, x: Cardinal; const S: Byte; const ac: Cardinal): Cardinal; inline;
  begin
    Result := ROL(a + x + ac + (b xor c xor d), S) + b
  end;

  function II(const a, b, c, d, x: Cardinal; const S: Byte; const ac: Cardinal): Cardinal; inline;
  begin
    Result := ROL(a + x + ac + (c xor (b or not d)), S) + b
  end;

type
  TDigestCardinal = array [0 .. 3] of Cardinal;
  TCardinalBuf    = array [0 .. 15] of Cardinal;
var
  a, b, c, d: Cardinal;
begin
  a := TDigestCardinal(Accu)[0];
  b := TDigestCardinal(Accu)[1];
  c := TDigestCardinal(Accu)[2];
  d := TDigestCardinal(Accu)[3];

  a := FF(a, b, c, d, TCardinalBuf(buf)[0], 7, $D76AA478);   { 1 }
  d := FF(d, a, b, c, TCardinalBuf(buf)[1], 12, $E8C7B756);  { 2 }
  c := FF(c, d, a, b, TCardinalBuf(buf)[2], 17, $242070DB);  { 3 }
  b := FF(b, c, d, a, TCardinalBuf(buf)[3], 22, $C1BDCEEE);  { 4 }
  a := FF(a, b, c, d, TCardinalBuf(buf)[4], 7, $F57C0FAF);   { 5 }
  d := FF(d, a, b, c, TCardinalBuf(buf)[5], 12, $4787C62A);  { 6 }
  c := FF(c, d, a, b, TCardinalBuf(buf)[6], 17, $A8304613);  { 7 }
  b := FF(b, c, d, a, TCardinalBuf(buf)[7], 22, $FD469501);  { 8 }
  a := FF(a, b, c, d, TCardinalBuf(buf)[8], 7, $698098D8);   { 9 }
  d := FF(d, a, b, c, TCardinalBuf(buf)[9], 12, $8B44F7AF);  { 10 }
  c := FF(c, d, a, b, TCardinalBuf(buf)[10], 17, $FFFF5BB1); { 11 }
  b := FF(b, c, d, a, TCardinalBuf(buf)[11], 22, $895CD7BE); { 12 }
  a := FF(a, b, c, d, TCardinalBuf(buf)[12], 7, $6B901122);  { 13 }
  d := FF(d, a, b, c, TCardinalBuf(buf)[13], 12, $FD987193); { 14 }
  c := FF(c, d, a, b, TCardinalBuf(buf)[14], 17, $A679438E); { 15 }
  b := FF(b, c, d, a, TCardinalBuf(buf)[15], 22, $49B40821); { 16 }

  a := GG(a, b, c, d, TCardinalBuf(buf)[1], 5, $F61E2562);   { 17 }
  d := GG(d, a, b, c, TCardinalBuf(buf)[6], 9, $C040B340);   { 18 }
  c := GG(c, d, a, b, TCardinalBuf(buf)[11], 14, $265E5A51); { 19 }
  b := GG(b, c, d, a, TCardinalBuf(buf)[0], 20, $E9B6C7AA);  { 20 }
  a := GG(a, b, c, d, TCardinalBuf(buf)[5], 5, $D62F105D);   { 21 }
  d := GG(d, a, b, c, TCardinalBuf(buf)[10], 9, $02441453);  { 22 }
  c := GG(c, d, a, b, TCardinalBuf(buf)[15], 14, $D8A1E681); { 23 }
  b := GG(b, c, d, a, TCardinalBuf(buf)[4], 20, $E7D3FBC8);  { 24 }
  a := GG(a, b, c, d, TCardinalBuf(buf)[9], 5, $21E1CDE6);   { 25 }
  d := GG(d, a, b, c, TCardinalBuf(buf)[14], 9, $C33707D6);  { 26 }
  c := GG(c, d, a, b, TCardinalBuf(buf)[3], 14, $F4D50D87);  { 27 }
  b := GG(b, c, d, a, TCardinalBuf(buf)[8], 20, $455A14ED);  { 28 }
  a := GG(a, b, c, d, TCardinalBuf(buf)[13], 5, $A9E3E905);  { 29 }
  d := GG(d, a, b, c, TCardinalBuf(buf)[2], 9, $FCEFA3F8);   { 30 }
  c := GG(c, d, a, b, TCardinalBuf(buf)[7], 14, $676F02D9);  { 31 }
  b := GG(b, c, d, a, TCardinalBuf(buf)[12], 20, $8D2A4C8A); { 32 }

  a := HH(a, b, c, d, TCardinalBuf(buf)[5], 4, $FFFA3942);   { 33 }
  d := HH(d, a, b, c, TCardinalBuf(buf)[8], 11, $8771F681);  { 34 }
  c := HH(c, d, a, b, TCardinalBuf(buf)[11], 16, $6D9D6122); { 35 }
  b := HH(b, c, d, a, TCardinalBuf(buf)[14], 23, $FDE5380C); { 36 }
  a := HH(a, b, c, d, TCardinalBuf(buf)[1], 4, $A4BEEA44);   { 37 }
  d := HH(d, a, b, c, TCardinalBuf(buf)[4], 11, $4BDECFA9);  { 38 }
  c := HH(c, d, a, b, TCardinalBuf(buf)[7], 16, $F6BB4B60);  { 39 }
  b := HH(b, c, d, a, TCardinalBuf(buf)[10], 23, $BEBFBC70); { 40 }
  a := HH(a, b, c, d, TCardinalBuf(buf)[13], 4, $289B7EC6);  { 41 }
  d := HH(d, a, b, c, TCardinalBuf(buf)[0], 11, $EAA127FA);  { 42 }
  c := HH(c, d, a, b, TCardinalBuf(buf)[3], 16, $D4EF3085);  { 43 }
  b := HH(b, c, d, a, TCardinalBuf(buf)[6], 23, $04881D05);  { 44 }
  a := HH(a, b, c, d, TCardinalBuf(buf)[9], 4, $D9D4D039);   { 45 }
  d := HH(d, a, b, c, TCardinalBuf(buf)[12], 11, $E6DB99E5); { 46 }
  c := HH(c, d, a, b, TCardinalBuf(buf)[15], 16, $1FA27CF8); { 47 }
  b := HH(b, c, d, a, TCardinalBuf(buf)[2], 23, $C4AC5665);  { 48 }

  a := II(a, b, c, d, TCardinalBuf(buf)[0], 6, $F4292244);   { 49 }
  d := II(d, a, b, c, TCardinalBuf(buf)[7], 10, $432AFF97);  { 50 }
  c := II(c, d, a, b, TCardinalBuf(buf)[14], 15, $AB9423A7); { 51 }
  b := II(b, c, d, a, TCardinalBuf(buf)[5], 21, $FC93A039);  { 52 }
  a := II(a, b, c, d, TCardinalBuf(buf)[12], 6, $655B59C3);  { 53 }
  d := II(d, a, b, c, TCardinalBuf(buf)[3], 10, $8F0CCC92);  { 54 }
  c := II(c, d, a, b, TCardinalBuf(buf)[10], 15, $FFEFF47D); { 55 }
  b := II(b, c, d, a, TCardinalBuf(buf)[1], 21, $85845DD1);  { 56 }
  a := II(a, b, c, d, TCardinalBuf(buf)[8], 6, $6FA87E4F);   { 57 }
  d := II(d, a, b, c, TCardinalBuf(buf)[15], 10, $FE2CE6E0); { 58 }
  c := II(c, d, a, b, TCardinalBuf(buf)[6], 15, $A3014314);  { 59 }
  b := II(b, c, d, a, TCardinalBuf(buf)[13], 21, $4E0811A1); { 60 }
  a := II(a, b, c, d, TCardinalBuf(buf)[4], 6, $F7537E82);   { 61 }
  d := II(d, a, b, c, TCardinalBuf(buf)[11], 10, $BD3AF235); { 62 }
  c := II(c, d, a, b, TCardinalBuf(buf)[2], 15, $2AD7D2BB);  { 63 }
  b := II(b, c, d, a, TCardinalBuf(buf)[9], 21, $EB86D391);  { 64 }

  Inc(TDigestCardinal(Accu)[0], a);
  Inc(TDigestCardinal(Accu)[1], b);
  Inc(TDigestCardinal(Accu)[2], c);
  Inc(TDigestCardinal(Accu)[3], d)
end;

function umlMD5(const BuffPtr: PBYTE; BufSiz: NativeUInt): TMD5;
{$IF Defined(FastMD5) and Defined(Delphi) and (Defined(WIN32) or Defined(WIN64))}
begin
  Result := FastMD5(BuffPtr, BufSiz);
end;
{$ELSE}


var
  digest : TMD5;
  Lo, Hi : Cardinal;
  p      : PBYTE;
  WorkLen: Byte;
  WorkBuf: array [0 .. 63] of Byte;
begin
  Lo := 0;
  Hi := 0;
  PCardinal(@digest[0])^ := $67452301;
  PCardinal(@digest[4])^ := $EFCDAB89;
  PCardinal(@digest[8])^ := $98BADCFE;
  PCardinal(@digest[12])^ := $10325476;

  if BufSiz shl 3 < 0 then
      Inc(Hi);

  Inc(Lo, BufSiz shl 3);
  Inc(Hi, BufSiz shr 29);

  p := BuffPtr;

  while BufSiz >= $40 do
    begin
      umlTransformMD5(digest, p^);
      Inc(p, $40);
      Dec(BufSiz, $40);
    end;
  if BufSiz > 0 then
      CopyPtr(p, @WorkBuf[0], BufSiz);

  Result := PMD5(@digest[0])^;
  WorkBuf[BufSiz] := $80;
  WorkLen := BufSiz + 1;
  if WorkLen > $38 then
    begin
      if WorkLen < $40 then
          FillPtrByte(@WorkBuf[WorkLen], $40 - WorkLen, 0);
      umlTransformMD5(Result, WorkBuf);
      WorkLen := 0
    end;
  FillPtrByte(@WorkBuf[WorkLen], $38 - WorkLen, 0);
  PCardinal(@WorkBuf[$38])^ := Lo;
  PCardinal(@WorkBuf[$3C])^ := Hi;
  umlTransformMD5(Result, WorkBuf);
end;
{$IFEND}


function umlMD5Char(const BuffPtr: PBYTE; const BuffSize: NativeUInt): TPascalString;
begin
  Result := umlMD52Str(umlMD5(BuffPtr, BuffSize));
end;

function umlMD5String(const BuffPtr: PBYTE; const BuffSize: NativeUInt): TPascalString;
begin
  Result := umlMD52Str(umlMD5(BuffPtr, BuffSize));
end;

function umlStreamMD5(Stream: TCoreClassStream; StartPos, EndPos: Int64): TMD5;
{$IF Defined(FastMD5) and Defined(Delphi) and (Defined(WIN32) or Defined(WIN64))}
begin
  Result := FastMD5(Stream, StartPos, EndPos);
end;
{$ELSE}


const
  deltaSize = $40 * $FFFF;

var
  digest  : TMD5;
  Lo, Hi  : Cardinal;
  DeltaBuf: Pointer;
  BufSiz  : Int64;
  rest    : Cardinal;
  p       : PBYTE;
  WorkLen : Byte;
  WorkBuf : array [0 .. 63] of Byte;
begin
  {$IFDEF OptimizationMemoryStreamMD5}
  if Stream is TCoreClassMemoryStream then
    begin
      Result := umlMD5(Pointer(NativeUInt(TCoreClassMemoryStream(Stream).Memory) + StartPos), EndPos - StartPos);
      Exit;
    end;
  if Stream is TMemoryStream64 then
    begin
      Result := umlMD5(TMemoryStream64(Stream).PositionAsPtr(StartPos), EndPos - StartPos);
      Exit;
    end;
  {$IFEND}
  //

  Lo := 0;
  Hi := 0;
  PCardinal(@digest[0])^ := $67452301;
  PCardinal(@digest[4])^ := $EFCDAB89;
  PCardinal(@digest[8])^ := $98BADCFE;
  PCardinal(@digest[12])^ := $10325476;

  BufSiz := EndPos - StartPos;
  rest := 0;

  if BufSiz shl 3 < 0 then
      Inc(Hi);

  Inc(Lo, BufSiz shl 3);
  Inc(Hi, BufSiz shr 29);

  DeltaBuf := GetMemory(deltaSize);
  Stream.Position := StartPos;

  if BufSiz < $40 then
    begin
      Stream.Read(DeltaBuf^, BufSiz);
      p := DeltaBuf;
    end
  else
    while BufSiz >= $40 do
      begin
        if rest = 0 then
          begin
            if BufSiz >= deltaSize then
                rest := Stream.Read(DeltaBuf^, deltaSize)
            else
                rest := Stream.Read(DeltaBuf^, BufSiz);

            p := DeltaBuf;
          end;
        umlTransformMD5(digest, p^);
        Inc(p, $40);
        Dec(BufSiz, $40);
        Dec(rest, $40);
      end;

  if BufSiz > 0 then
      CopyPtr(p, @WorkBuf[0], BufSiz);

  FreeMemory(DeltaBuf);

  Result := PMD5(@digest[0])^;
  WorkBuf[BufSiz] := $80;
  WorkLen := BufSiz + 1;
  if WorkLen > $38 then
    begin
      if WorkLen < $40 then
          FillPtrByte(@WorkBuf[WorkLen], $40 - WorkLen, 0);
      umlTransformMD5(Result, WorkBuf);
      WorkLen := 0
    end;
  FillPtrByte(@WorkBuf[WorkLen], $38 - WorkLen, 0);
  PCardinal(@WorkBuf[$38])^ := Lo;
  PCardinal(@WorkBuf[$3C])^ := Hi;
  umlTransformMD5(Result, WorkBuf);
end;
{$IFEND}


function umlStreamMD5(Stream: TCoreClassStream): TMD5;
begin
  if Stream.Size <= 0 then
    begin
      Result := NullMD5;
      Exit;
    end;
  Stream.Position := 0;
  Result := umlStreamMD5(Stream, 0, Stream.Size);
  Stream.Position := 0;
end;

function umlStreamMD5Char(Stream: TCoreClassStream): TPascalString;
begin
  Result := umlMD52Str(umlStreamMD5(Stream));
end;

function umlStreamMD5String(Stream: TCoreClassStream): TPascalString;
begin
  Result := umlMD52Str(umlStreamMD5(Stream));
end;

function umlStringMD5(const Value: TPascalString): TMD5;
var
  b: TBytes;
begin
  b := umlBytesOf(Value);
  Result := umlMD5(@b[0], Length(b));
end;

function umlStringMD5Char(const Value: TPascalString): TPascalString;
var
  b: TBytes;
begin
  b := umlBytesOf(Value);
  Result := umlMD52Str(umlMD5(@b[0], Length(b)));
end;

function umlMD52Str(md5: TMD5): TPascalString;
const
  HexArr: array [0 .. 15] of umlChar = ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f');
var
  i: Integer;
begin
  Result.len := 32;
  for i := 0 to 15 do
    begin
      Result.buff[i * 2] := HexArr[(md5[i] shr 4) and $0F];
      Result.buff[i * 2 + 1] := HexArr[md5[i] and $0F];
    end;
end;

function umlMD52String(md5: TMD5): TPascalString;
const
  HexArr: array [0 .. 15] of umlChar = ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f');
var
  i: Integer;
begin
  Result.len := 32;
  for i := 0 to 15 do
    begin
      Result.buff[i * 2] := HexArr[(md5[i] shr 4) and $0F];
      Result.buff[i * 2 + 1] := HexArr[md5[i] and $0F];
    end;
end;

function umlMD5Compare(const m1, m2: TMD5): Boolean;
begin
  Result := (PUInt64(@m1[0])^ = PUInt64(@m2[0])^) and (PUInt64(@m1[8])^ = PUInt64(@m2[8])^);
end;

function umlCompareMD5(const m1, m2: TMD5): Boolean;
begin
  Result := (PUInt64(@m1[0])^ = PUInt64(@m2[0])^) and (PUInt64(@m1[8])^ = PUInt64(@m2[8])^);
end;

function umlIsNullMD5(m: TMD5): Boolean;
begin
  Result := umlCompareMD5(m, NullMD5);
end;

function umlWasNullMD5(m: TMD5): Boolean;
begin
  Result := umlCompareMD5(m, NullMD5);
end;

function umlCRC16(const Value: PBYTE; const Count: NativeUInt): Word;
var
  i : NativeUInt;
  pb: PByteArray absolute Value;
begin
  Result := 0;
  for i := 0 to Count - 1 do
      Result := (Result shr 8) xor CRC16Table[pb^[i] xor (Result and $FF)];
end;

function umlStringCRC16(const Value: TPascalString): Word;
var
  b: TBytes;
begin
  b := umlBytesOf(Value);
  Result := umlCRC16(@b[0], Length(b));
end;

function umlStreamCRC16(Stream: TMixedStream; StartPos, EndPos: Int64): Word;
const
  ChunkSize = 1024 * 1024;
  procedure CRC16BUpdate(var crc: Word; const buf: Pointer; len: NativeUInt); {$IFDEF INLINE_ASM} inline; {$ENDIF}
  var
    p: PBYTE;
    i: Integer;
  begin
    p := buf;
    for i := 0 to len - 1 do
      begin
        crc := (crc shr 8) xor CRC16Table[p^ xor (crc and $FF)];
        Inc(p);
      end;
  end;

var
  j    : NativeUInt;
  Num  : NativeUInt;
  rest : NativeUInt;
  buf  : Pointer;
  FSize: Int64;
begin
  { Allocate buffer to read file }
  buf := GetMemory(ChunkSize);
  { Initialize CRC }
  Result := 0;

  { V1.03 calculate how much of the file we are processing }
  FSize := Stream.Size;
  if (StartPos >= FSize) then
      StartPos := 0;
  if (EndPos > FSize) or (EndPos = 0) then
      EndPos := FSize;

  { Calculate number of full chunks that will fit into the buffer }
  Num := EndPos div ChunkSize;
  { Calculate remaining bytes }
  rest := EndPos mod ChunkSize;

  { Set the stream to the beginning of the file }
  Stream.Position := StartPos;

  { Process full chunks }
  for j := 0 to Num - 1 do begin
      Stream.Read(buf^, ChunkSize);
      CRC16BUpdate(Result, buf, ChunkSize);
    end;

  { Process remaining bytes }
  if rest > 0 then begin
      Stream.Read(buf^, rest);
      CRC16BUpdate(Result, buf, rest);
    end;

  FreeMem(buf, ChunkSize);
end;

function umlStreamCRC16(Stream: TMixedStream): Word;
begin
  Stream.Position := 0;
  Result := umlStreamCRC16(Stream, 0, Stream.Size);
  Stream.Position := 0;
end;

function umlCRC32(const Value: PBYTE; const Count: NativeUInt): Cardinal;
var
  i : NativeUInt;
  pb: PByteArray absolute Value;
begin
  Result := $FFFFFFFF;
  for i := 0 to Count - 1 do
      Result := ((Result shr 8) and $00FFFFFF) xor CRC32Table[(Result xor pb^[i]) and $FF];
  Result := Result xor $FFFFFFFF;
end;

function umlString2CRC32(const Value: TPascalString): Cardinal;
var
  b: TBytes;
begin
  b := umlBytesOf(Value);
  Result := umlCRC32(@b[0], Length(b));
end;

function umlStreamCRC32(Stream: TMixedStream; StartPos, EndPos: Int64): Cardinal;
const
  ChunkSize = 1024 * 1024;

  procedure CRC32BUpdate(var crc: Cardinal; const buf: Pointer; len: NativeUInt); {$IFDEF INLINE_ASM} inline; {$ENDIF}
  var
    p: PBYTE;
    i: Integer;
  begin
    p := buf;
    for i := 0 to len - 1 do
      begin
        crc := ((crc shr 8) and $00FFFFFF) xor CRC32Table[(crc xor p^) and $FF];
        Inc(p);
      end;
  end;

var
  j    : NativeUInt;
  Num  : NativeUInt;
  rest : NativeUInt;
  buf  : Pointer;
  FSize: Int64;
begin
  { Allocate buffer to read file }
  buf := GetMemory(ChunkSize);

  { Initialize CRC }
  Result := $FFFFFFFF;

  { V1.03 calculate how much of the file we are processing }
  FSize := Stream.Size;
  if (StartPos >= FSize) then
      StartPos := 0;
  if (EndPos > FSize) or (EndPos = 0) then
      EndPos := FSize;

  { Calculate number of full chunks that will fit into the buffer }
  Num := EndPos div ChunkSize;
  { Calculate remaining bytes }
  rest := EndPos mod ChunkSize;

  { Set the stream to the beginning of the file }
  Stream.Position := StartPos;

  { Process full chunks }
  for j := 0 to Num - 1 do begin
      Stream.Read(buf^, ChunkSize);
      CRC32BUpdate(Result, buf, ChunkSize);
    end;

  { Process remaining bytes }
  if rest > 0 then begin
      Stream.Read(buf^, rest);
      CRC32BUpdate(Result, buf, rest);
    end;

  FreeMem(buf, ChunkSize);

  Result := Result xor $FFFFFFFF;
end;

function umlStreamCRC32(Stream: TMixedStream): Cardinal;
begin
  Stream.Position := 0;
  Result := umlStreamCRC32(Stream, 0, Stream.Size);
  Stream.Position := 0;
end;

procedure umlDES(const Input: TDESKey; var Output: TDESKey; const Key: TDESKey; Encrypt: Boolean);
type
  TArrayOf16Bytes = array [1 .. 16] of Byte;
  TArrayOf28Bytes = array [1 .. 28] of Byte;
  TArrayOf32Bytes = array [1 .. 32] of Byte;
  TArrayOf48Bytes = array [1 .. 48] of Byte;
  TArrayOf56Bytes = array [1 .. 56] of Byte;
  TArrayOf64Bytes = array [1 .. 64] of Byte;

  TDesData = packed record
    InputValue: TArrayOf64Bytes;
    OutputValue: TArrayOf64Bytes;
    RoundKeys: array [1 .. 16] of TArrayOf48Bytes;
    l, r: TArrayOf32Bytes;
    FunctionResult: TArrayOf32Bytes;
    c, d: TArrayOf28Bytes;
  end;

const
  { Initial Permutation }
  IP: TArrayOf64Bytes = (
    58, 50, 42, 34, 26, 18, 10, 2,
    60, 52, 44, 36, 28, 20, 12, 4,
    62, 54, 46, 38, 30, 22, 14, 6,
    64, 56, 48, 40, 32, 24, 16, 8,
    57, 49, 41, 33, 25, 17, 9, 1,
    59, 51, 43, 35, 27, 19, 11, 3,
    61, 53, 45, 37, 29, 21, 13, 5,
    63, 55, 47, 39, 31, 23, 15, 7);
  { Final Permutation }
  InvIP: TArrayOf64Bytes = (
    40, 8, 48, 16, 56, 24, 64, 32,
    39, 7, 47, 15, 55, 23, 63, 31,
    38, 6, 46, 14, 54, 22, 62, 30,
    37, 5, 45, 13, 53, 21, 61, 29,
    36, 4, 44, 12, 52, 20, 60, 28,
    35, 3, 43, 11, 51, 19, 59, 27,
    34, 2, 42, 10, 50, 18, 58, 26,
    33, 1, 41, 9, 49, 17, 57, 25);
  { Expansion Permutation }
  e: TArrayOf48Bytes = (
    32, 1, 2, 3, 4, 5,
    4, 5, 6, 7, 8, 9,
    8, 9, 10, 11, 12, 13,
    12, 13, 14, 15, 16, 17,
    16, 17, 18, 19, 20, 21,
    20, 21, 22, 23, 24, 25,
    24, 25, 26, 27, 28, 29,
    28, 29, 30, 31, 32, 1);
  { P-Box permutation }
  p: TArrayOf32Bytes = (
    16, 7, 20, 21, 29, 12, 28, 17,
    1, 15, 23, 26, 5, 18, 31, 10,
    2, 8, 24, 14, 32, 27, 3, 9,
    19, 13, 30, 6, 22, 11, 4, 25);
  { Key Permutation }
  PC_1: TArrayOf56Bytes = (
    57, 49, 41, 33, 25, 17, 9,
    1, 58, 50, 42, 34, 26, 18,
    10, 2, 59, 51, 43, 35, 27,
    19, 11, 3, 60, 52, 44, 36,
    63, 55, 47, 39, 31, 23, 15,
    7, 62, 54, 46, 38, 30, 22,
    14, 6, 61, 53, 45, 37, 29,
    21, 13, 5, 28, 20, 12, 4);
  { Compression Permutation }
  PC_2: TArrayOf48Bytes = (
    14, 17, 11, 24, 1, 5,
    3, 28, 15, 6, 21, 10,
    23, 19, 12, 4, 26, 8,
    16, 7, 27, 20, 13, 2,
    41, 52, 31, 37, 47, 55,
    30, 40, 51, 45, 33, 48,
    44, 49, 39, 56, 34, 53,
    46, 42, 50, 36, 29, 32);
  { Number of key bits shifted per round }
  ST: TArrayOf16Bytes = (
    1, 1, 2, 2, 2, 2, 2, 2,
    1, 2, 2, 2, 2, 2, 2, 1);
  { S-Boxes }
  SBoxes: array [1 .. 8, 0 .. 3, 0 .. 15] of Byte =
    (((14, 4, 13, 1, 2, 15, 11, 8, 3, 10, 6, 12, 5, 9, 0, 7),
    (0, 15, 7, 4, 14, 2, 13, 1, 10, 6, 12, 11, 9, 5, 3, 8),
    (4, 1, 14, 8, 13, 6, 2, 11, 15, 12, 9, 7, 3, 10, 5, 0),
    (15, 12, 8, 2, 4, 9, 1, 7, 5, 11, 3, 14, 10, 0, 6, 13)),

    ((15, 1, 8, 14, 6, 11, 3, 4, 9, 7, 2, 13, 12, 0, 5, 10),
    (3, 13, 4, 7, 15, 2, 8, 14, 12, 0, 1, 10, 6, 9, 11, 5),
    (0, 14, 7, 11, 10, 4, 13, 1, 5, 8, 12, 6, 9, 3, 2, 15),
    (13, 8, 10, 1, 3, 15, 4, 2, 11, 6, 7, 12, 0, 5, 14, 9)),

    ((10, 0, 9, 14, 6, 3, 15, 5, 1, 13, 12, 7, 11, 4, 2, 8),
    (13, 7, 0, 9, 3, 4, 6, 10, 2, 8, 5, 14, 12, 11, 15, 1),
    (13, 6, 4, 9, 8, 15, 3, 0, 11, 1, 2, 12, 5, 10, 14, 7),
    (1, 10, 13, 0, 6, 9, 8, 7, 4, 15, 14, 3, 11, 5, 2, 12)),

    ((7, 13, 14, 3, 0, 6, 9, 10, 1, 2, 8, 5, 11, 12, 4, 15),
    (13, 8, 11, 5, 6, 15, 0, 3, 4, 7, 2, 12, 1, 10, 14, 9),
    (10, 6, 9, 0, 12, 11, 7, 13, 15, 1, 3, 14, 5, 2, 8, 4),
    (3, 15, 0, 6, 10, 1, 13, 8, 9, 4, 5, 11, 12, 7, 2, 14)),

    ((2, 12, 4, 1, 7, 10, 11, 6, 8, 5, 3, 15, 13, 0, 14, 9),
    (14, 11, 2, 12, 4, 7, 13, 1, 5, 0, 15, 10, 3, 9, 8, 6),
    (4, 2, 1, 11, 10, 13, 7, 8, 15, 9, 12, 5, 6, 3, 0, 14),
    (11, 8, 12, 7, 1, 14, 2, 13, 6, 15, 0, 9, 10, 4, 5, 3)),

    ((12, 1, 10, 15, 9, 2, 6, 8, 0, 13, 3, 4, 14, 7, 5, 11),
    (10, 15, 4, 2, 7, 12, 9, 5, 6, 1, 13, 14, 0, 11, 3, 8),
    (9, 14, 15, 5, 2, 8, 12, 3, 7, 0, 4, 10, 1, 13, 11, 6),
    (4, 3, 2, 12, 9, 5, 15, 10, 11, 14, 1, 7, 6, 0, 8, 13)),

    ((4, 11, 2, 14, 15, 0, 8, 13, 3, 12, 9, 7, 5, 10, 6, 1),
    (13, 0, 11, 7, 4, 9, 1, 10, 14, 3, 5, 12, 2, 15, 8, 6),
    (1, 4, 11, 13, 12, 3, 7, 14, 10, 15, 6, 8, 0, 5, 9, 2),
    (6, 11, 13, 8, 1, 4, 10, 7, 9, 5, 0, 15, 14, 2, 3, 12)),

    ((13, 2, 8, 4, 6, 15, 11, 1, 10, 9, 3, 14, 5, 0, 12, 7),
    (1, 15, 13, 8, 10, 3, 7, 4, 12, 5, 6, 11, 0, 14, 9, 2),
    (7, 11, 4, 1, 9, 12, 14, 2, 0, 6, 10, 13, 15, 3, 5, 8),
    (2, 1, 14, 7, 4, 10, 8, 13, 15, 12, 9, 0, 3, 5, 6, 11)));

  function GetBit(const Bits: TDESKey; const Index: Byte): Byte; {$IFDEF INLINE_ASM} inline; {$ENDIF}
  var
    idx: Byte;
  begin
    idx := index - 1;
    if Bits[idx div 8] and (128 shr (idx mod 8)) > 0 then
        Result := 1
    else
        Result := 0;
  end;

  procedure SetBit(var Bits: TDESKey; Index, Value: Byte); {$IFDEF INLINE_ASM} inline; {$ENDIF}
  var
    Bit: Byte;
  begin
    Dec(index);
    Bit := 128 shr (index mod 8);
    case Value of
      0: Bits[index div 8] := Bits[index div 8] and (not Bit);
      1: Bits[index div 8] := Bits[index div 8] or Bit;
    end;
  end;

  procedure f(var FR: TArrayOf32Bytes; var FK: TArrayOf48Bytes; var TotalOut: TArrayOf32Bytes); {$IFDEF INLINE_ASM} inline; {$ENDIF}
  var
    Temp1                  : TArrayOf48Bytes;
    Temp2                  : TArrayOf32Bytes;
    n, H, i, j, Row, Column: Cardinal;
  begin
    for n := 1 to 48 do
        Temp1[n] := FR[e[n]] xor FK[n];
    for n := 1 to 8 do
      begin
        i := (n - 1) * 6;
        j := (n - 1) * 4;
        Row := Temp1[i + 1] * 2 + Temp1[i + 6];
        Column := Temp1[i + 2] * 8 + Temp1[i + 3] * 4 +
          Temp1[i + 4] * 2 + Temp1[i + 5];
        for H := 1 to 4 do
          begin
            case H of
              1: Temp2[j + H] := (SBoxes[n, Row, Column] and 8) div 8;
              2: Temp2[j + H] := (SBoxes[n, Row, Column] and 4) div 4;
              3: Temp2[j + H] := (SBoxes[n, Row, Column] and 2) div 2;
              4: Temp2[j + H] := (SBoxes[n, Row, Column] and 1);
            end;
          end;
      end;
    for n := 1 to 32 do
        TotalOut[n] := Temp2[p[n]];
  end;

  procedure Shift(var SubKeyPart: TArrayOf28Bytes); {$IFDEF INLINE_ASM} inline; {$ENDIF}
  var
    n, b: Byte;
  begin
    b := SubKeyPart[1];
    for n := 1 to 27 do
        SubKeyPart[n] := SubKeyPart[n + 1];
    SubKeyPart[28] := b;
  end;

  procedure SubKey(var DesData: TDesData; Round: Byte; var SubKey: TArrayOf48Bytes); {$IFDEF INLINE_ASM} inline; {$ENDIF}
  var
    n, b: Byte;
  begin
    for n := 1 to ST[Round] do
      begin
        Shift(DesData.c);
        Shift(DesData.d);
      end;
    for n := 1 to 48 do
      begin
        b := PC_2[n];
        if b <= 28 then
            SubKey[n] := DesData.c[b]
        else
            SubKey[n] := DesData.d[b - 28];
      end;
  end;

var
  n, b, Round: Byte;
  DesData    : TDesData;
begin
  for n := 1 to 64 do
      DesData.InputValue[n] := GetBit(Input, n);
  for n := 1 to 28 do
    begin
      DesData.c[n] := GetBit(Key, PC_1[n]);
      DesData.d[n] := GetBit(Key, PC_1[n + 28]);
    end;
  for n := 1 to 16 do
      SubKey(DesData, n, DesData.RoundKeys[n]);
  for n := 1 to 64 do
    begin
      if n <= 32 then
          DesData.l[n] := DesData.InputValue[IP[n]]
      else
          DesData.r[n - 32] := DesData.InputValue[IP[n]];
    end;
  for Round := 1 to 16 do
    begin
      if Encrypt then
          f(DesData.r, DesData.RoundKeys[Round], DesData.FunctionResult)
      else
          f(DesData.r, DesData.RoundKeys[17 - Round], DesData.FunctionResult);
      for n := 1 to 32 do
          DesData.FunctionResult[n] := DesData.FunctionResult[n] xor DesData.l[n];
      DesData.l := DesData.r;
      DesData.r := DesData.FunctionResult;
    end;
  for n := 1 to 64 do
    begin
      b := InvIP[n];
      if b <= 32 then
          DesData.OutputValue[n] := DesData.r[b]
      else
          DesData.OutputValue[n] := DesData.l[b - 32];
    end;
  for n := 1 to 64 do
      SetBit(Output, n, DesData.OutputValue[n]);
end;

procedure umlDES(DataPtr: Pointer; Size: Cardinal; const Key: TDESKey; Encrypt: Boolean);
var
  p: NativeUInt;
begin
  p := 0;
  repeat
    umlDES(PDESKey(Pointer(NativeUInt(DataPtr) + p))^, PDESKey(Pointer(NativeUInt(DataPtr) + p))^, Key, Encrypt);
    p := p + 8;
  until p + 8 > Size;
end;

procedure umlDES(DataPtr: Pointer; Size: Cardinal; const Key: TPascalString; Encrypt: Boolean);
var
  h64: THash64;
begin
  h64 := FastHash64PascalString(@Key);
  umlDES(DataPtr, Size, PDESKey(@h64)^, Encrypt);
end;

procedure umlDES(Input, Output: TMixedStream; const Key: TDESKey; Encrypt: Boolean);
const
  bufflen = 1024 * 1024;
var
  buff: array of Byte;

  procedure FillBuff(Size: Cardinal);
  var
    p: Cardinal;
  begin
    p := 0;

    repeat
      umlDES(PDESKey(@buff[p])^, PDESKey(@buff[p])^, Key, Encrypt);
      p := p + 8;
    until p + 8 > Size;
  end;

var
  l      : Cardinal;
  p, Size: Int64;
begin
  SetLength(buff, bufflen);
  Input.Position := 0;
  p := 0;
  l := bufflen;
  Size := Input.Size;

  if Encrypt then
    begin
      Output.Size := umlInt64Length + Size;
      Output.Position := 0;
      Output.Write(Size, umlInt64Length);

      while p + bufflen < Size do
        begin
          Input.Read(buff[0], l);
          FillBuff(l);
          Output.Write(buff[0], l);
          p := p + l;
        end;

      l := Size - p;
      Input.Read(buff[0], l);
      FillBuff(l);
      Output.Write(buff[0], l);
    end
  else
    begin
      Input.Read(Size, umlInt64Length);
      Output.Size := Size;
      Output.Position := 0;

      while p + bufflen < Size do
        begin
          Input.Read(buff[0], l);
          FillBuff(l);
          Output.Write(buff[0], l);
          p := p + l;
        end;

      l := Size - p;
      Input.Read(buff[0], l);
      FillBuff(l);
      Output.Write(buff[0], l);
    end;
end;

procedure umlDES(Input, Output: TMixedStream; const Key: TPascalString; Encrypt: Boolean);
var
  h64: THash64;
begin
  h64 := FastHash64PascalString(@Key);
  umlDES(Input, Output, PDESKey(@h64)^, Encrypt);
end;

function umlDESCompare(const d1, d2: TDESKey): Boolean;
begin
  Result := PUInt64(@d1[0])^ = PUInt64(@d2[0])^;
end;

procedure umlFastSymbol(DataPtr: Pointer; Size: Cardinal; const Key: TDESKey; Encrypt: Boolean);
var
  p: NativeUInt;
  i: Integer;
  b: PBYTE;
begin
  i := 0;
  for p := 0 to Size - 1 do
    begin
      b := Pointer(NativeUInt(DataPtr) + p);
      if Encrypt then
          b^ := b^ + Key[i]
      else
          b^ := b^ - Key[i];

      Inc(i);
      if i >= umlDESLength then
          i := 0;
    end;
end;

procedure umlFastSymbol(DataPtr: Pointer; Size: Cardinal; const Key: TPascalString; Encrypt: Boolean);
var
  h64: THash64;
begin
  h64 := FastHash64PascalString(@Key);
  umlFastSymbol(DataPtr, Size, PDESKey(@h64)^, Encrypt);
end;

function umlTrimSpace(const S: TPascalString): TPascalString;
var
  l, bp, ep: Integer;
begin
  Result := '';
  l := S.len;
  if l > 0 then
    begin
      bp := 1;
      while CharIn(S[bp], [#32, #0]) do
        begin
          Inc(bp);
          if (bp > l) then
            begin
              Result := '';
              Exit;
            end;
        end;
      if bp > l then
          Result := ''
      else
        begin
          ep := l;

          while CharIn(S[ep], [#32, #0]) do
            begin
              Dec(ep);
              if (ep < 1) then
                begin
                  Result := '';
                  Exit;
                end;
            end;
          Result := S.GetString(bp, ep + 1);
        end;
    end;
end;

function umlSeparatorText(AText: TPascalString; Dest: TCoreClassStrings; SeparatorChar: TPascalString): Integer;
var
  ANewText, ASeparatorText: TPascalString;
begin
  Result := 0;
  if Assigned(Dest) then
    begin
      ANewText := AText;
      ASeparatorText := umlGetFirstStr(ANewText, SeparatorChar);
      while (ASeparatorText.len > 0) and (ANewText.len > 0) do
        begin
          Dest.Add(ASeparatorText.Text);
          Inc(Result);
          ANewText := umlDeleteFirstStr(ANewText, SeparatorChar);
          ASeparatorText := umlGetFirstStr(ANewText, SeparatorChar);
        end;
    end;
end;

function umlStringsMatchText(OriginValue: TCoreClassStrings; DestValue: TPascalString; IgnoreCase: Boolean = True): Boolean;
var
  i: Integer;
begin
  Result := False;
  if not Assigned(OriginValue) then
      Exit;
  if OriginValue.Count > 0 then
    begin
      for i := 0 to OriginValue.Count - 1 do
        begin
          if umlMultipleMatch(IgnoreCase, OriginValue[i], DestValue) then
            begin
              Result := True;
              Exit;
            end;
        end;
    end;
end;

function umlStringsInExists(Dest: TCoreClassStrings; SText: TPascalString; IgnoreCase: Boolean = True): Boolean;
var
  i  : Integer;
  _NS: TPascalString;
begin
  Result := False;
  if IgnoreCase then
      _NS := umlUpperCase(SText)
  else
      _NS := SText;
  if Assigned(Dest) then
    begin
      if Dest.Count > 0 then
        begin
          for i := 0 to Dest.Count - 1 do
            begin
              if ((not IgnoreCase) and (SText = Dest[i])) or ((IgnoreCase) and (umlSameText(SText, Dest[i]))) then
                begin
                  Result := True;
                  Exit;
                end;
            end;
        end;
    end;
end;

function umlTextInStrings(const SText: TPascalString; Dest: TCoreClassStrings; IgnoreCase: Boolean = True): Boolean;
begin
  Result := umlStringsInExists(Dest, SText, IgnoreCase);
end;

function umlAddNewStrTo(SourceStr: TPascalString; Dest: TCoreClassStrings; IgnoreCase: Boolean = True): Boolean;
begin
  Result := not umlStringsInExists(Dest, SourceStr, IgnoreCase);
  if Result then
      Dest.Append(SourceStr.Text);
end;

function umlDeleteStrings(const SText: TPascalString; Dest: TCoreClassStrings; IgnoreCase: Boolean = True): Integer;
var
  i: Integer;
begin
  Result := 0;
  if Assigned(Dest) then
    begin
      if Dest.Count > 0 then
        begin
          i := 0;
          while i < Dest.Count do
            begin
              if ((not IgnoreCase) and (SText = Dest[i])) or ((IgnoreCase) and (umlMultipleMatch(IgnoreCase, SText, Dest[i]))) then
                begin
                  Dest.Delete(i);
                  Inc(Result);
                end
              else
                  Inc(i);
            end;
        end;
    end;
end;

function umlDeleteStringsNot(const SText: TPascalString; Dest: TCoreClassStrings; IgnoreCase: Boolean = True): Integer;
var
  i: Integer;
begin
  Result := 0;
  if Assigned(Dest) then
    begin
      if Dest.Count > 0 then
        begin
          i := 0;
          while i < Dest.Count do
            begin
              if ((not IgnoreCase) and (SText <> Dest[i])) or ((IgnoreCase) and (not umlMultipleMatch(IgnoreCase, SText, Dest[i]))) then
                begin
                  Dest.Delete(i);
                  Inc(Result);
                end
              else
                  Inc(i);
            end;
        end;
    end;
end;

function umlMergeStrings(Source, Dest: TCoreClassStrings; IgnoreCase: Boolean = True): Integer;
var
  i: Integer;
begin
  Result := 0;
  if (not Assigned(Source)) or (not Assigned(Dest)) then
      Exit;
  if Source.Count > 0 then
    begin
      for i := 0 to Source.Count - 1 do
        begin
          umlAddNewStrTo(Source[i], Dest, IgnoreCase);
          Inc(Result);
        end;
    end;
end;

function umlConverStrToFileName(const Value: TPascalString): TPascalString;
var
  i: Integer;
begin
  Result := Value;
  for i := 1 to umlGetLength(Result) do
    begin
      if CharIn(Result[i], '":;/\|<>?*%') then
          Result[i] := ' ';
    end;
end;

function umlSplitTextMatch(const SText, limit, MatchText: TPascalString; IgnoreCase: Boolean): Boolean;
var
  n, t: TPascalString;
begin
  Result := True;
  if MatchText = '' then
      Exit;
  n := SText;
  //
  if umlExistsLimitChar(n, limit) then
    begin
      repeat
        t := umlGetFirstStr(n, limit);
        if umlMultipleMatch(IgnoreCase, MatchText, t) then
            Exit;
        n := umlDeleteFirstStr(n, limit);
      until n = '';
    end
  else
    begin
      t := n;
      if umlMultipleMatch(IgnoreCase, MatchText, t) then
          Exit;
    end;
  //
  Result := False;
end;

function umlSplitTextTrimSpaceMatch(const SText, limit, MatchText: TPascalString; IgnoreCase: Boolean): Boolean;
var
  n, t: TPascalString;
begin
  Result := True;
  if MatchText = '' then
      Exit;
  n := SText;

  if umlExistsLimitChar(n, limit) then
    begin
      repeat
        t := umlTrimSpace(umlGetFirstStr(n, limit));
        if umlMultipleMatch(IgnoreCase, MatchText, t) then
            Exit;
        n := umlDeleteFirstStr(n, limit);
      until n = '';
    end
  else
    begin
      t := umlTrimSpace(n);
      if umlMultipleMatch(IgnoreCase, MatchText, t) then
          Exit;
    end;

  Result := False;
end;

function umlSplitDeleteText(const SText, limit, MatchText: TPascalString; IgnoreCase: Boolean): TPascalString;
var
  n, t: TPascalString;
begin
  if (MatchText = '') or (limit = '') then
    begin
      Result := SText;
      Exit;
    end;
  Result := '';
  n := SText;
  //
  if umlExistsLimitChar(n, limit) then
    begin
      repeat
        t := umlGetFirstStr(n, limit);
        if not umlMultipleMatch(IgnoreCase, MatchText, t) then
          begin
            if Result <> '' then
                Result := Result + limit[1] + t
            else
                Result := t;
          end;
        n := umlDeleteFirstStr(n, limit);
      until n = '';
    end
  else
    begin
      t := n;
      if not umlMultipleMatch(IgnoreCase, MatchText, t) then
          Result := SText;
    end;
end;

function umlSplitTextAsList(const SText, limit: TPascalString; AsLst: TCoreClassStrings): Boolean;
var
  n, t: TPascalString;
begin
  AsLst.Clear;
  n := SText;
  //
  if umlExistsLimitChar(n, limit) then
    begin
      repeat
        t := umlGetFirstStr(n, limit);
        AsLst.Append(t.Text);
        n := umlDeleteFirstStr(n, limit);
      until n = '';
    end
  else
    begin
      t := n;
      if umlGetLength(t) > 0 then
          AsLst.Append(t.Text);
    end;
  //
  Result := AsLst.Count > 0;
end;

function umlSplitTextAsListAndTrimSpace(const SText, limit: TPascalString; AsLst: TCoreClassStrings): Boolean;
var
  n, t: TPascalString;
begin
  AsLst.Clear;
  n := SText;
  //
  if umlExistsLimitChar(n, limit) then
    begin
      repeat
        t := umlGetFirstStr(n, limit);
        AsLst.Append(umlTrimSpace(t).Text);
        n := umlDeleteFirstStr(n, limit);
      until n = '';
    end
  else
    begin
      t := n;
      if umlGetLength(t) > 0 then
          AsLst.Append(umlTrimSpace(t).Text);
    end;
  //
  Result := AsLst.Count > 0;
end;

function umlListAsSplitText(const List: TCoreClassStrings; limit: TPascalString): TPascalString;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to List.Count - 1 do
    if Result = '' then
        Result := List[i]
    else
        Result := Result + limit + List[i];
end;

function umlUpdateComponentName(const Name: TPascalString): TPascalString;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to umlGetLength(name) do
    if umlGetLength(Result) > 0 then
      begin
        if CharIn(name[i], [c0to9, cLoAtoZ, cHiAtoZ], '-') then
            Result := Result + name[i];
      end
    else if CharIn(name[i], [cLoAtoZ, cHiAtoZ]) then
        Result := Result + name[i];
end;

function umlMakeComponentName(OWner: TCoreClassComponent; RefrenceName: TPascalString): TPascalString;
var
  c: Cardinal;
begin
  c := 1;
  RefrenceName := umlUpdateComponentName(RefrenceName);
  Result := RefrenceName;
  while OWner.FindComponent(Result.Text) <> nil do
    begin
      Result := RefrenceName + IntToStr(c);
      Inc(c);
    end;
end;

procedure umlReadComponent(Stream: TCoreClassStream; comp: TCoreClassComponent);
var
  r            : TCoreClassReader;
  needClearName: Boolean;
begin
  r := TCoreClassReader.Create(Stream, 4096);
  r.IgnoreChildren := True;
  try
    needClearName := (comp.Name = '');
    r.ReadRootComponent(comp);
    if needClearName then
        comp.Name := '';
  except
  end;
  DisposeObject(r);
end;

procedure umlWriteComponent(Stream: TCoreClassStream; comp: TCoreClassComponent);
var
  W: TCoreClassWriter;
begin
  W := TCoreClassWriter.Create(Stream, 4096);
  W.IgnoreChildren := True;
  W.WriteDescendent(comp, nil);
  DisposeObject(W);
end;

procedure umlCopyComponentDataTo(comp, copyto: TCoreClassComponent);
var
  ms: TCoreClassMemoryStream;
begin
  if comp.ClassType <> copyto.ClassType then
      Exit;
  ms := TCoreClassMemoryStream.Create;
  try
    umlWriteComponent(ms, comp);
    ms.Position := 0;
    umlReadComponent(ms, copyto);
  except
  end;
  DisposeObject(ms);
end;

function umlProcessCycleValue(CurrentVal, DeltaVal, StartVal, OverVal: Single; var EndFlag: Boolean): Single;
  function IfOut(Cur, delta, Dest: Single): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
  begin
    if Cur > Dest then
        Result := Cur - delta < Dest
    else
        Result := Cur + delta > Dest;
  end;

  function GetOutValue(Cur, delta, Dest: Single): Single; {$IFDEF INLINE_ASM} inline; {$ENDIF}
  begin
    if IfOut(Cur, delta, Dest) then
      begin
        if Cur > Dest then
            Result := Dest - (Cur - delta)
        else
            Result := Cur + delta - Dest;
      end
    else
        Result := 0;
  end;

  function GetDeltaValue(Cur, delta, Dest: Single): Single; {$IFDEF INLINE_ASM} inline; {$ENDIF}
  begin
    if Cur > Dest then
        Result := Cur - delta
    else
        Result := Cur + delta;
  end;

begin
  if (DeltaVal > 0) and (StartVal <> OverVal) then
    begin
      if EndFlag then
        begin
          if IfOut(CurrentVal, DeltaVal, OverVal) then
            begin
              EndFlag := False;
              Result := umlProcessCycleValue(OverVal, GetOutValue(CurrentVal, DeltaVal, OverVal), StartVal, OverVal, EndFlag);
            end
          else
              Result := GetDeltaValue(CurrentVal, DeltaVal, OverVal);
        end
      else
        begin
          if IfOut(CurrentVal, DeltaVal, StartVal) then
            begin
              EndFlag := True;
              Result := umlProcessCycleValue(StartVal, GetOutValue(CurrentVal, DeltaVal, StartVal), StartVal, OverVal, EndFlag);
            end
          else
              Result := GetDeltaValue(CurrentVal, DeltaVal, StartVal);
        end
    end
  else
      Result := CurrentVal;
end;

initialization

finalization

end.
