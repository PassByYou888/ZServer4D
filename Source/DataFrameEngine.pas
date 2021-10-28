{ ****************************************************************************** }
{ * Data Struct Engine                                                         * }
{ * written by QQ 600585@qq.com                                                * }
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

unit DataFrameEngine;

{$INCLUDE zDefine.inc}

interface

uses SysUtils, CoreClasses, Types, Variants,
  ListEngine, MemoryStream64, CoreCipher,
  DoStatusIO, GeometryLib, TextDataEngine, Geometry2DUnit, Geometry3DUnit,
  ZJson, NumberBase,
{$IFDEF DELPHI}
  ZS_JsonDataObjects,
{$ENDIF DELPHI}
  CoreCompress, UnicodeMixedLib, PascalStrings;

type
  TDFE = class;
  TDataFrameEngine = TDFE;
  TDFEngine = TDFE;
  TDF = TDFE;
  TDataFrame = TDFE;

  TDFBase = class(TCoreClassObject)
  protected
    FID: Byte; // data frame id
  public
    constructor Create(ID: Byte);
    destructor Destroy; override;

    procedure LoadFromStream(stream: TCoreClassStream); virtual; abstract;
    procedure SaveToStream(stream: TCoreClassStream); virtual; abstract;
    procedure LoadFromJson(jarry: TZ_JsonArray; index_: Integer); virtual; abstract;
    procedure SaveToJson(jarry: TZ_JsonArray; index_: Integer); virtual; abstract;
    function ComputeEncodeSize: Int64; virtual; abstract;
  end;

  TDFString = class sealed(TDFBase)
  public
    Buffer: TBytes;

    constructor Create(ID: Byte);
    destructor Destroy; override;

    procedure LoadFromStream(stream: TCoreClassStream); override;
    procedure SaveToStream(stream: TCoreClassStream); override;
    procedure LoadFromJson(jarry: TZ_JsonArray; index_: Integer); override;
    procedure SaveToJson(jarry: TZ_JsonArray; index_: Integer); override;
    function ComputeEncodeSize: Int64; override;
  end;

  TDFInteger = class sealed(TDFBase)
  private
  protected
    FBuffer: Integer;
  public
    constructor Create(ID: Byte);
    destructor Destroy; override;

    procedure LoadFromStream(stream: TCoreClassStream); override;
    procedure SaveToStream(stream: TCoreClassStream); override;
    procedure LoadFromJson(jarry: TZ_JsonArray; index_: Integer); override;
    procedure SaveToJson(jarry: TZ_JsonArray; index_: Integer); override;
    function ComputeEncodeSize: Int64; override;

    property Buffer: Integer read FBuffer write FBuffer;
  end;

  TDFCardinal = class sealed(TDFBase)
  private
  protected
    FBuffer: Cardinal;
  public
    constructor Create(ID: Byte);
    destructor Destroy; override;

    procedure LoadFromStream(stream: TCoreClassStream); override;
    procedure SaveToStream(stream: TCoreClassStream); override;
    procedure LoadFromJson(jarry: TZ_JsonArray; index_: Integer); override;
    procedure SaveToJson(jarry: TZ_JsonArray; index_: Integer); override;
    function ComputeEncodeSize: Int64; override;

    property Buffer: Cardinal read FBuffer write FBuffer;
  end;

  TDFWord = class sealed(TDFBase)
  private
  protected
    FBuffer: Word;
  public
    constructor Create(ID: Byte);
    destructor Destroy; override;

    procedure LoadFromStream(stream: TCoreClassStream); override;
    procedure SaveToStream(stream: TCoreClassStream); override;
    procedure LoadFromJson(jarry: TZ_JsonArray; index_: Integer); override;
    procedure SaveToJson(jarry: TZ_JsonArray; index_: Integer); override;
    function ComputeEncodeSize: Int64; override;

    property Buffer: Word read FBuffer write FBuffer;
  end;

  TDFByte = class sealed(TDFBase)
  private
  protected
    FBuffer: Byte;
  public
    constructor Create(ID: Byte);
    destructor Destroy; override;

    procedure LoadFromStream(stream: TCoreClassStream); override;
    procedure SaveToStream(stream: TCoreClassStream); override;
    procedure LoadFromJson(jarry: TZ_JsonArray; index_: Integer); override;
    procedure SaveToJson(jarry: TZ_JsonArray; index_: Integer); override;
    function ComputeEncodeSize: Int64; override;

    property Buffer: Byte read FBuffer write FBuffer;
  end;

  TDFSingle = class sealed(TDFBase)
  private
  protected
    FBuffer: Single;
  public
    constructor Create(ID: Byte);
    destructor Destroy; override;

    procedure LoadFromStream(stream: TCoreClassStream); override;
    procedure SaveToStream(stream: TCoreClassStream); override;
    procedure LoadFromJson(jarry: TZ_JsonArray; index_: Integer); override;
    procedure SaveToJson(jarry: TZ_JsonArray; index_: Integer); override;
    function ComputeEncodeSize: Int64; override;

    property Buffer: Single read FBuffer write FBuffer;
  end;

  TDFDouble = class sealed(TDFBase)
  private
  protected
    FBuffer: Double;
  public
    constructor Create(ID: Byte);
    destructor Destroy; override;

    procedure LoadFromStream(stream: TCoreClassStream); override;
    procedure SaveToStream(stream: TCoreClassStream); override;
    procedure LoadFromJson(jarry: TZ_JsonArray; index_: Integer); override;
    procedure SaveToJson(jarry: TZ_JsonArray; index_: Integer); override;
    function ComputeEncodeSize: Int64; override;

    property Buffer: Double read FBuffer write FBuffer;
  end;

  TDFArrayInteger = class sealed(TDFBase)
  protected
    FBuffer: TMS64;
  public
    constructor Create(ID: Byte);
    destructor Destroy; override;

    procedure Clear;
    procedure Add(v: Integer);
    function Count: Integer;
    procedure WriteArray(const a: array of Integer);

    procedure LoadFromStream(stream: TCoreClassStream); override;
    procedure SaveToStream(stream: TCoreClassStream); override;
    procedure LoadFromJson(jarry: TZ_JsonArray; index_: Integer); override;
    procedure SaveToJson(jarry: TZ_JsonArray; index_: Integer); override;
    function ComputeEncodeSize: Int64; override;

    function GetBuffer(index_: Integer): Integer;
    procedure SetBuffer(index_: Integer; Value: Integer);
    property Buffer[index_: Integer]: Integer read GetBuffer write SetBuffer; default;
  end;

  TDFArrayShortInt = class sealed(TDFBase)
  protected
    FBuffer: TMS64;
  public
    constructor Create(ID: ShortInt);
    destructor Destroy; override;

    procedure Clear;
    procedure Add(v: ShortInt);
    function Count: Integer;
    procedure WriteArray(const a: array of ShortInt);

    procedure LoadFromStream(stream: TCoreClassStream); override;
    procedure SaveToStream(stream: TCoreClassStream); override;
    procedure LoadFromJson(jarry: TZ_JsonArray; index_: Integer); override;
    procedure SaveToJson(jarry: TZ_JsonArray; index_: Integer); override;
    function ComputeEncodeSize: Int64; override;

    function GetBuffer(index_: Integer): ShortInt;
    procedure SetBuffer(index_: Integer; Value: ShortInt);
    property Buffer[index_: Integer]: ShortInt read GetBuffer write SetBuffer; default;
  end;

  TDFArrayByte = class sealed(TDFBase)
  protected
    FBuffer: TMS64;
  public
    constructor Create(ID: Byte);
    destructor Destroy; override;

    procedure Clear;
    procedure Add(v: Byte);
    procedure AddPtrBuff(p: PByte; Size_: Integer);
    procedure AddI64(v: Int64);
    procedure AddU64(v: UInt64);
    procedure Addi(v: Integer);
    procedure AddWord(v: Word);
    function Count: Int64;
    property Size: Int64 read Count;
    procedure WriteArray(const a: array of Byte);
    procedure SetArray(const a: array of Byte);
    procedure SetBuff(p: PByte; Size_: Integer);
    procedure GetBuff(p: PByte);

    procedure LoadFromStream(stream: TCoreClassStream); override;
    procedure SaveToStream(stream: TCoreClassStream); override;
    procedure LoadFromJson(jarry: TZ_JsonArray; index_: Integer); override;
    procedure SaveToJson(jarry: TZ_JsonArray; index_: Integer); override;
    function ComputeEncodeSize: Int64; override;

    function GetBuffer(index_: Integer): Byte;
    procedure SetBuffer(index_: Integer; Value: Byte);
    property Buffer[index_: Integer]: Byte read GetBuffer write SetBuffer; default;
  end;

  TDFArraySingle = class sealed(TDFBase)
  protected
    FBuffer: TMS64;
  public
    constructor Create(ID: Byte);
    destructor Destroy; override;

    procedure Clear;
    procedure Add(v: Single);
    function Count: Integer;
    procedure WriteArray(const a: array of Single);

    procedure LoadFromStream(stream: TCoreClassStream); override;
    procedure SaveToStream(stream: TCoreClassStream); override;
    procedure LoadFromJson(jarry: TZ_JsonArray; index_: Integer); override;
    procedure SaveToJson(jarry: TZ_JsonArray; index_: Integer); override;
    function ComputeEncodeSize: Int64; override;

    function GetBuffer(index_: Integer): Single;
    procedure SetBuffer(index_: Integer; Value: Single);
    property Buffer[index_: Integer]: Single read GetBuffer write SetBuffer; default;
  end;

  TDFArrayDouble = class sealed(TDFBase)
  private
  protected
    FBuffer: TMS64;
  public
    constructor Create(ID: Byte);
    destructor Destroy; override;

    procedure Clear;
    procedure Add(v: Double);
    function Count: Integer;
    procedure WriteArray(const a: array of Double);

    procedure LoadFromStream(stream: TCoreClassStream); override;
    procedure SaveToStream(stream: TCoreClassStream); override;
    procedure LoadFromJson(jarry: TZ_JsonArray; index_: Integer); override;
    procedure SaveToJson(jarry: TZ_JsonArray; index_: Integer); override;
    function ComputeEncodeSize: Int64; override;

    function GetBuffer(index_: Integer): Double;
    procedure SetBuffer(index_: Integer; Value: Double);
    property Buffer[index_: Integer]: Double read GetBuffer write SetBuffer; default;
  end;

  TDFArrayInt64 = class sealed(TDFBase)
  private
  protected
    FBuffer: TMS64;
  public
    constructor Create(ID: Byte);
    destructor Destroy; override;

    procedure Clear;
    procedure Add(v: Int64);
    function Count: Integer;
    procedure WriteArray(const a: array of Int64);

    procedure LoadFromStream(stream: TCoreClassStream); override;
    procedure SaveToStream(stream: TCoreClassStream); override;
    procedure LoadFromJson(jarry: TZ_JsonArray; index_: Integer); override;
    procedure SaveToJson(jarry: TZ_JsonArray; index_: Integer); override;
    function ComputeEncodeSize: Int64; override;

    function GetBuffer(index_: Integer): Int64;
    procedure SetBuffer(index_: Integer; Value: Int64);
    property Buffer[index_: Integer]: Int64 read GetBuffer write SetBuffer; default;
  end;

  TDFStream = class sealed(TDFBase)
  private
  protected
    FBuffer: TMS64;
  public
    constructor Create(ID: Byte);
    destructor Destroy; override;

    procedure Clear;

    procedure LoadFromStream(stream: TCoreClassStream); override;
    procedure SaveToStream(stream: TCoreClassStream); override;
    procedure LoadFromJson(jarry: TZ_JsonArray; index_: Integer); override;
    procedure SaveToJson(jarry: TZ_JsonArray; index_: Integer); override;
    function ComputeEncodeSize: Int64; override;

    function GetBuffer: TCoreClassStream;
    procedure SetBuffer(_Buffer: TCoreClassStream);
    property Buffer: TCoreClassStream read GetBuffer write SetBuffer;
    property InstanceBuffer: TMS64 read FBuffer write FBuffer;
  end;

  TDFVariant = class sealed(TDFBase)
  private
  protected
    FBuffer: Variant;
  public
    constructor Create(ID: Byte);
    destructor Destroy; override;

    procedure LoadFromStream(stream: TCoreClassStream); override;
    procedure SaveToStream(stream: TCoreClassStream); override;
    procedure LoadFromJson(jarry: TZ_JsonArray; index_: Integer); override;
    procedure SaveToJson(jarry: TZ_JsonArray; index_: Integer); override;
    function ComputeEncodeSize: Int64; override;

    property Buffer: Variant read FBuffer write FBuffer;
  end;

  TDFInt64 = class sealed(TDFBase)
  private
  protected
    FBuffer: Int64;
  public
    constructor Create(ID: Byte);
    destructor Destroy; override;

    procedure LoadFromStream(stream: TCoreClassStream); override;
    procedure SaveToStream(stream: TCoreClassStream); override;
    procedure LoadFromJson(jarry: TZ_JsonArray; index_: Integer); override;
    procedure SaveToJson(jarry: TZ_JsonArray; index_: Integer); override;
    function ComputeEncodeSize: Int64; override;

    property Buffer: Int64 read FBuffer write FBuffer;
  end;

  TDFUInt64 = class sealed(TDFBase)
  private
  protected
    FBuffer: UInt64;
  public
    constructor Create(ID: Byte);
    destructor Destroy; override;

    procedure LoadFromStream(stream: TCoreClassStream); override;
    procedure SaveToStream(stream: TCoreClassStream); override;
    procedure LoadFromJson(jarry: TZ_JsonArray; index_: Integer); override;
    procedure SaveToJson(jarry: TZ_JsonArray; index_: Integer); override;
    function ComputeEncodeSize: Int64; override;

    property Buffer: UInt64 read FBuffer write FBuffer;
  end;

  TDFEngineReader = class(TCoreClassObject)
  private
    FOwner: TDFE;
    FIndex: Integer;
  public
    constructor Create(Owner_: TDFE);
    destructor Destroy; override;
    property Index: Integer read FIndex write FIndex;
    property Owner: TDFE read FOwner;
    function IsEnd: Boolean;
    function NotEnd: Boolean;
    procedure GoNext;

    function ReadString: SystemString;
    function ReadInteger: Integer;
    function ReadCardinal: Cardinal;
    function ReadWord: Word;
    function ReadBool: Boolean;
    function ReadBoolean: Boolean;
    function ReadByte: Byte;
    function ReadSingle: Single;
    function ReadDouble: Double;
    function ReadArrayInteger: TDFArrayInteger;
    function ReadArrayShortInt: TDFArrayShortInt;
    function ReadArrayByte: TDFArrayByte;
    function ReadMD5: TMD5;
    function ReadArraySingle: TDFArraySingle;
    function ReadArrayDouble: TDFArrayDouble;
    function ReadArrayInt64: TDFArrayInt64;
    procedure ReadStream(output: TCoreClassStream);
    function ReadVariant: Variant;
    function ReadInt64: Int64;
    function ReadUInt64: UInt64;
    procedure ReadStrings(output: TCoreClassStrings);
    procedure ReadListStrings(output: TListString);
    procedure ReadPascalStrings(output: TListPascalString);
    procedure ReadDataFrame(output: TDFE);
    procedure ReadHashStringList(output: THashStringList);
    procedure ReadVariantList(output: THashVariantList);
    procedure ReadSectionText(output: TSectionTextData);
    procedure ReadTextSection(output: TSectionTextData);
    procedure ReadJson(output: TZ_JsonObject); overload;
{$IFDEF DELPHI}
    procedure ReadJson(output: TJsonObject); overload;
{$ENDIF DELPHI}
    function ReadRect: TRect;
    function ReadRectf: TRectf;
    function ReadPoint: TPoint;
    function ReadPointf: TPointf;
    function ReadVector: TVector;
    function ReadAffineVector: TAffineVector;
    function ReadVec3: TVec3;
    function ReadVec4: TVec4;
    function ReadVector3: TVector3;
    function ReadVector4: TVector4;
    function ReadMat4: TMat4;
    function ReadMatrix4: TMatrix4;
    function Read2DPoint: T2DPoint;
    function ReadVec2: TVec2;
    function ReadRectV2: TRectV2;
    function ReadPointer: UInt64;
    procedure ReadNM(output: TNumberModule);
    procedure ReadNMPool(output: TNumberModulePool);
    // auto read from stream data
    procedure Read(var Buf_; Count_: Int64); overload;
    // read as TDFBase
    function Read: TDFBase; overload;
    // return current TDFBase
    function Current: TDFBase;
  end;

  TRunTimeDataType = (rdtString, rdtInteger, rdtLongWord, rdtWORD, rdtByte, rdtSingle, rdtDouble,
    rdtArrayInteger, rdtArraySingle, rdtArrayDouble, rdtStream, rdtVariant, rdtInt64, rdtArrayShortInt, rdtCardinal, rdtUInt64, rdtArrayByte,
    rdtArrayInt64);

  TDFE = class(TCoreClassObject)
  private
    FDataList: TCoreClassListForObj;
    FReader: TDFEngineReader;
    FCompressorDeflate: TCompressorDeflate;
    FCompressorBRRC: TCompressorBRRC;
    FIsChanged: Boolean;
  protected
    function DataTypeToByte(v: TRunTimeDataType): Byte;
    function ByteToDataType(v: Byte): TRunTimeDataType;
  public
    constructor Create;
    destructor Destroy; override;

    property Reader: TDFEngineReader read FReader;
    property R: TDFEngineReader read FReader;
    property IsChanged: Boolean read FIsChanged write FIsChanged;

    procedure SwapInstance(source: TDFE);

    procedure Clear;
    function AddData(v: TRunTimeDataType): TDFBase;
    function GetData(index_: Integer): TDFBase;
    function GetDataInfo(Obj_: TDFBase): SystemString;
    function Count: Integer;
    function Delete(index_: Integer): Boolean;
    function DeleteFirst: Boolean;
    function DeleteLast: Boolean; overload;
    function DeleteLastCount(num_: Integer): Boolean; overload;
    function DeleteCount(index_, Count_: Integer): Boolean;
    procedure Assign(source: TDFE);
    function Clone: TDFE;

    procedure WriteString(v: SystemString); overload;
    procedure WriteString(v: TPascalString); overload;
    procedure WriteString(const Fmt: SystemString; const Args: array of const); overload;
    procedure WriteInteger(v: Integer);
    procedure WriteCardinal(v: Cardinal);
    procedure WriteWORD(v: Word);
    procedure WriteBool(v: Boolean);
    procedure WriteBoolean(v: Boolean);
    procedure WriteByte(v: Byte);
    procedure WriteSingle(v: Single);
    procedure WriteDouble(v: Double);
    function WriteArrayInteger: TDFArrayInteger;
    function WriteArrayShortInt: TDFArrayShortInt;
    function WriteArrayByte: TDFArrayByte;
    procedure WriteMD5(md5: TMD5);
    function WriteArraySingle: TDFArraySingle;
    function WriteArrayDouble: TDFArrayDouble;
    function WriteArrayInt64: TDFArrayInt64;
    procedure WriteStream(v: TCoreClassStream);
    procedure WriteInstanceStream(v: TMS64);
    procedure WriteVariant(v: Variant);
    procedure WriteInt64(v: Int64);
    procedure WriteUInt64(v: UInt64);
    procedure WriteStrings(v: TCoreClassStrings);
    procedure WriteListStrings(v: TListString);
    procedure WritePascalStrings(v: TListPascalString);
    procedure WriteDataFrame(v: TDFE);
    // select compresssion
    procedure WriteDataFrameCompressed(v: TDFE);
    // zlib compression
    procedure WriteDataFrameZLib(v: TDFE);
    procedure WriteHashStringList(v: THashStringList);
    procedure WriteVariantList(v: THashVariantList);
    procedure WriteSectionText(v: TSectionTextData);
    procedure WriteTextSection(v: TSectionTextData);
    procedure WriteJson(v: TZ_JsonObject); overload;
    procedure WriteJson(v: TZ_JsonObject; Formated_: Boolean); overload;
{$IFDEF DELPHI}
    procedure WriteJson(v: TJsonObject); overload;
{$ENDIF DELPHI}
    procedure WriteFile(fn: SystemString);
    procedure WriteRect(v: TRect);
    procedure WriteRectf(v: TRectf);
    procedure WritePoint(v: TPoint);
    procedure WritePointf(v: TPointf);
    procedure WriteVector(v: TVector);
    procedure WriteAffineVector(v: TAffineVector);
    procedure WriteVec4(v: TVec4);
    procedure WriteVec3(v: TVec3);
    procedure WriteVector4(v: TVector4);
    procedure WriteVector3(v: TVector3);
    procedure WriteMat4(v: TMat4);
    procedure WriteMatrix4(v: TMatrix4);
    procedure Write2DPoint(v: T2DPoint);
    procedure WriteVec2(v: TVec2);
    procedure WriteRectV2(v: TRectV2);
    procedure WritePointer(v: Pointer); overload;
    procedure WritePointer(v: UInt64); overload;
    procedure WriteNM(NM: TNumberModule);
    procedure WriteNMPool(NMPool: TNumberModulePool);
    // auto append new stream and write
    procedure write(const Buf_; Count_: Int64);

    function ReadString(index_: Integer): SystemString;
    function ReadInteger(index_: Integer): Integer;
    function ReadCardinal(index_: Integer): Cardinal;
    function ReadWord(index_: Integer): Word;
    function ReadBool(index_: Integer): Boolean;
    function ReadBoolean(index_: Integer): Boolean;
    function ReadByte(index_: Integer): Byte;
    function ReadSingle(index_: Integer): Single;
    function ReadDouble(index_: Integer): Double;
    function ReadArrayInteger(index_: Integer): TDFArrayInteger;
    function ReadArrayShortInt(index_: Integer): TDFArrayShortInt;
    function ReadArrayByte(index_: Integer): TDFArrayByte;
    function ReadMD5(index_: Integer): TMD5;
    function ReadArraySingle(index_: Integer): TDFArraySingle;
    function ReadArrayDouble(index_: Integer): TDFArrayDouble;
    function ReadArrayInt64(index_: Integer): TDFArrayInt64;
    procedure ReadStream(index_: Integer; output: TCoreClassStream);
    function ReadVariant(index_: Integer): Variant;
    function ReadInt64(index_: Integer): Int64;
    function ReadUInt64(index_: Integer): UInt64;
    procedure ReadStrings(index_: Integer; output: TCoreClassStrings);
    procedure ReadListStrings(index_: Integer; output: TListString);
    procedure ReadPascalStrings(index_: Integer; output: TListPascalString);
    procedure ReadDataFrame(index_: Integer; output: TDFE);
    procedure ReadHashStringList(index_: Integer; output: THashStringList);
    procedure ReadVariantList(index_: Integer; output: THashVariantList);
    procedure ReadSectionText(index_: Integer; output: TSectionTextData);
    procedure ReadTextSection(index_: Integer; output: TSectionTextData);
    procedure ReadJson(index_: Integer; output: TZ_JsonObject); overload;
{$IFDEF DELPHI}
    procedure ReadJson(index_: Integer; output: TJsonObject); overload;
{$ENDIF DELPHI}
    function ReadRect(index_: Integer): TRect;
    function ReadRectf(index_: Integer): TRectf;
    function ReadPoint(index_: Integer): TPoint;
    function ReadPointf(index_: Integer): TPointf;
    function ReadVector(index_: Integer): TVector;
    function ReadAffineVector(index_: Integer): TAffineVector;
    function ReadVec3(index_: Integer): TVec3;
    function ReadVec4(index_: Integer): TVec4;
    function ReadVector3(index_: Integer): TVector3;
    function ReadVector4(index_: Integer): TVector4;
    function ReadMat4(index_: Integer): TMat4;
    function ReadMatrix4(index_: Integer): TMatrix4;
    function Read2DPoint(index_: Integer): T2DPoint;
    function ReadVec2(index_: Integer): TVec2;
    function ReadRectV2(index_: Integer): TRectV2;
    function ReadPointer(index_: Integer): UInt64;
    procedure ReadNM(index_: Integer; output: TNumberModule);
    procedure ReadNMPool(index_: Integer; output: TNumberModulePool);
    // read from stream data
    procedure Read(index_: Integer; var Buf_; Count_: Int64); overload;
    // read as TDFBase
    function Read(index_: Integer): TDFBase; overload;

    function ComputeEncodeSize: Int64;
    class procedure BuildEmptyStream(output: TCoreClassStream);
    function FastEncode32To(output: TCoreClassStream; sizeInfo32: Cardinal): Integer;
    function FastEncode64To(output: TCoreClassStream; sizeInfo64: Int64): Integer;
    function FastEncodeTo(output: TCoreClassStream): Integer;
    function EncodeTo(output: TCoreClassStream; const FastMode, AutoCompressed: Boolean): Integer; overload;
    function EncodeTo(output: TCoreClassStream; const FastMode: Boolean): Integer; overload;
    function EncodeTo(output: TCoreClassStream): Integer; overload;
    // data security support
    // QuantumCryptographyPassword: used sha-3-512 cryptography as 512 bits password
    procedure Encrypt(output: TCoreClassStream; Compressed_: Boolean; SecurityLevel: Integer; Key: TCipherKeyBuffer);
    function Decrypt(input: TCoreClassStream; Key: TCipherKeyBuffer): Boolean;
    // json support
    procedure EncodeAsPublicJson(var output: TPascalString); overload;
    procedure EncodeAsPublicJson(output: TCoreClassStream); overload;
    procedure EncodeAsJson(output: TCoreClassStream); overload;
    procedure EncodeAsJson(json: TZ_JsonObject); overload;
    procedure DecodeFromJson(stream: TCoreClassStream); overload;
    procedure DecodeFromJson(const s: TPascalString); overload;
    procedure DecodeFromJson(json: TZ_JsonObject); overload;
    // Parallel compressor
    function EncodeAsSelectCompressor(scm: TSelectCompressionMethod; output: TCoreClassStream; const FastMode: Boolean): Integer; overload;
    function EncodeAsSelectCompressor(output: TCoreClassStream; const FastMode: Boolean): Integer; overload;
    function EncodeAsSelectCompressor(output: TCoreClassStream): Integer; overload;
    // ZLib compressor
    function EncodeAsZLib(output: TCoreClassStream; const FastMode: Boolean): Integer; overload;
    function EncodeAsZLib(output: TCoreClassStream): Integer; overload;
    // Deflate compressor
    function EncodeAsDeflate(output: TCoreClassStream; const FastMode: Boolean): Integer; overload;
    function EncodeAsDeflate(output: TCoreClassStream): Integer; overload;
    // BRRC compressor
    function EncodeAsBRRC(output: TCoreClassStream; const FastMode: Boolean): Integer; overload;
    function EncodeAsBRRC(output: TCoreClassStream): Integer; overload;
    function IsCompressed(source: TCoreClassStream): Boolean;

    function DecodeFrom(source: TCoreClassStream; const FastMode: Boolean): Integer; overload;
    function DecodeFrom(source: TCoreClassStream): Integer; overload;
    procedure EncodeToBytes(const Compressed, FastMode: Boolean; var output: TBytes);
    procedure DecodeFromBytes(var buff: TBytes); overload;
    procedure DecodeFromBytes(var buff: TBytes; const FastMode: Boolean); overload;
    function GetMD5(const FastMode: Boolean): TMD5;
    function Compare(source: TDFE): Boolean;
    procedure LoadFromStream(stream: TCoreClassStream);
    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromFile(fileName_: U_String);
    procedure SaveToFile(fileName_: U_String);
    property Data[index_: Integer]: TDFBase read GetData; default;
    property List: TCoreClassListForObj read FDataList;
  end;

  TDataWriter = class(TCoreClassObject)
  protected
    FEngine: TDFE;
    FStream: TCoreClassStream;
  public
    constructor Create(Stream_: TCoreClassStream);
    destructor Destroy; override;

    procedure Clear;

    procedure WriteString(v: SystemString);
    procedure WriteInteger(v: Integer);
    procedure WriteCardinal(v: Cardinal);
    procedure WriteWORD(v: Word);
    procedure WriteBool(v: Boolean);
    procedure WriteBoolean(v: Boolean);
    procedure WriteByte(v: Byte);
    procedure WriteSingle(v: Single);
    procedure WriteDouble(v: Double);
    procedure WriteArrayInteger(v: array of Integer);
    procedure WriteArrayShortInt(v: array of ShortInt);
    procedure WriteArrayByte(v: array of Byte);
    procedure WriteArraySingle(v: array of Single);
    procedure WriteArrayDouble(v: array of Double);
    procedure WriteArrayInt64(v: array of Int64);
    procedure WriteStream(v: TCoreClassStream);
    procedure WriteVariant(v: Variant);
    procedure WriteInt64(v: Int64);
    procedure WriteUInt64(v: UInt64);
    procedure WriteStrings(v: TCoreClassStrings);
    procedure WriteListStrings(v: TListString);
    procedure WritePascalStrings(v: TListPascalString);
    procedure WriteDataFrame(v: TDFE);
    procedure WriteDataFrameCompressed(v: TDFE);
    procedure WriteHashStringList(v: THashStringList);
    procedure WriteVariantList(v: THashVariantList);
    procedure WriteSectionText(v: TSectionTextData);
    procedure WriteJson(v: TZ_JsonObject); overload;
{$IFDEF DELPHI}
    procedure WriteJson(v: TJsonObject); overload;
{$ENDIF DELPHI}
    procedure WriteRect(v: TRect);
    procedure WriteRectf(v: TRectf);
    procedure WritePoint(v: TPoint);
    procedure WritePointf(v: TPointf);
    procedure WriteVector(v: TVector);
    procedure WriteAffineVector(v: TAffineVector);
    procedure WriteVec4(v: TVec4);
    procedure WriteVec3(v: TVec3);
    procedure WriteVector4(v: TVector4);
    procedure WriteVector3(v: TVector3);
    procedure WriteMat4(v: TMat4);
    procedure WriteMatrix4(v: TMatrix4);
    procedure Write2DPoint(v: T2DPoint);
    procedure WriteVec2(v: TVec2);
    procedure WriteRectV2(v: TRectV2);
    procedure WritePointer(v: Pointer);
    procedure WriteNM(NM: TNumberModule);
    procedure WriteNMPool(NMPool: TNumberModulePool);
    procedure write(const Buf_; Count_: Int64);
  end;

  TDataReader = class(TCoreClassObject)
  protected
    FEngine: TDFE;
  public
    constructor Create(Stream_: TCoreClassStream);
    destructor Destroy; override;

    function ReadString: SystemString;
    function ReadInteger: Integer;
    function ReadCardinal: Cardinal;
    function ReadWord: Word;
    function ReadBool: Boolean;
    function ReadBoolean: Boolean;
    function ReadByte: Byte;
    function ReadSingle: Single;
    function ReadDouble: Double;
    procedure ReadArrayInteger(var Data: array of Integer);
    procedure ReadArrayShortInt(var Data: array of ShortInt);
    procedure ReadArrayByte(var Data: array of Byte);
    procedure ReadArraySingle(var Data: array of Single);
    procedure ReadArrayDouble(var Data: array of Double);
    procedure ReadArrayInt64(var Data: array of Int64);
    procedure ReadStream(output: TCoreClassStream);
    function ReadVariant: Variant;
    function ReadInt64: Int64;
    function ReadUInt64: UInt64;
    procedure ReadStrings(output: TCoreClassStrings);
    procedure ReadListStrings(output: TListString);
    procedure ReadPascalStrings(output: TListPascalString);
    procedure ReadDataFrame(output: TDFE);
    procedure ReadHashStringList(output: THashStringList);
    procedure ReadVariantList(output: THashVariantList);
    procedure ReadSectionText(output: TSectionTextData);
    procedure ReadJson(output: TZ_JsonObject); overload;
{$IFDEF DELPHI}
    procedure ReadJson(output: TJsonObject); overload;
{$ENDIF DELPHI}
    function ReadRect: TRect;
    function ReadRectf: TRectf;
    function ReadPoint: TPoint;
    function ReadPointf: TPointf;
    function ReadVector: TVector;
    function ReadAffineVector: TAffineVector;
    function ReadVec3: TVec3;
    function ReadVec4: TVec4;
    function ReadVector3: TVector3;
    function ReadVector4: TVector4;
    function ReadMat4: TMat4;
    function ReadMatrix4: TMatrix4;
    function Read2DPoint: T2DPoint;
    function ReadVec2: TVec2;
    function ReadRectV2: TRectV2;
    function ReadPointer: UInt64;
    procedure ReadNM(output: TNumberModule);
    procedure ReadNMPool(output: TNumberModulePool);
    procedure Read(var Buf_; Count_: Int64);
  end;

implementation

constructor TDFBase.Create(ID: Byte);
begin
  inherited Create;
  FID := ID;
end;

destructor TDFBase.Destroy;
begin
  inherited Destroy;
end;

constructor TDFString.Create(ID: Byte);
begin
  inherited Create(ID);
  SetLength(Buffer, 0);
end;

destructor TDFString.Destroy;
begin
  SetLength(Buffer, 0);
  inherited Destroy;
end;

procedure TDFString.LoadFromStream(stream: TCoreClassStream);
var
  _Len: Integer;
begin
  stream.Read(_Len, C_Integer_Size);
  SetLength(Buffer, _Len);
  if (_Len > 0) then
      stream.Read(Buffer[0], _Len);
end;

procedure TDFString.SaveToStream(stream: TCoreClassStream);
var
  _Len: Integer;
begin
  _Len := length(Buffer);
  stream.write(_Len, C_Integer_Size);
  if _Len > 0 then
      stream.write(Buffer[0], _Len);
end;

procedure TDFString.LoadFromJson(jarry: TZ_JsonArray; index_: Integer);
begin
  Buffer := umlBytesOf(jarry.s[index_]);
end;

procedure TDFString.SaveToJson(jarry: TZ_JsonArray; index_: Integer);
begin
  jarry.Add(umlStringOf(Buffer));
end;

function TDFString.ComputeEncodeSize: Int64;
begin
  Result := C_Integer_Size + length(Buffer);
end;

constructor TDFInteger.Create(ID: Byte);
begin
  inherited Create(ID);
  Buffer := 0;
end;

destructor TDFInteger.Destroy;
begin
  inherited Destroy;
end;

procedure TDFInteger.LoadFromStream(stream: TCoreClassStream);
begin
  stream.Read(FBuffer, C_Integer_Size);
end;

procedure TDFInteger.SaveToStream(stream: TCoreClassStream);
begin
  stream.write(FBuffer, C_Integer_Size);
end;

procedure TDFInteger.LoadFromJson(jarry: TZ_JsonArray; index_: Integer);
begin
  FBuffer := jarry.i[index_];
end;

procedure TDFInteger.SaveToJson(jarry: TZ_JsonArray; index_: Integer);
begin
  jarry.Add(FBuffer);
end;

function TDFInteger.ComputeEncodeSize: Int64;
begin
  Result := C_Integer_Size;
end;

constructor TDFCardinal.Create(ID: Byte);
begin
  inherited Create(ID);
  Buffer := 0;
end;

destructor TDFCardinal.Destroy;
begin
  inherited Destroy;
end;

procedure TDFCardinal.LoadFromStream(stream: TCoreClassStream);
begin
  stream.Read(FBuffer, C_Cardinal_Size);
end;

procedure TDFCardinal.SaveToStream(stream: TCoreClassStream);
begin
  stream.write(FBuffer, C_Cardinal_Size);
end;

procedure TDFCardinal.LoadFromJson(jarry: TZ_JsonArray; index_: Integer);
begin
  FBuffer := jarry.i[index_];
end;

procedure TDFCardinal.SaveToJson(jarry: TZ_JsonArray; index_: Integer);
begin
  jarry.Add(FBuffer);
end;

function TDFCardinal.ComputeEncodeSize: Int64;
begin
  Result := C_Cardinal_Size;
end;

constructor TDFWord.Create(ID: Byte);
begin
  inherited Create(ID);
  Buffer := 0;
end;

destructor TDFWord.Destroy;
begin
  inherited Destroy;
end;

procedure TDFWord.LoadFromStream(stream: TCoreClassStream);
begin
  stream.Read(FBuffer, C_Word_Size);
end;

procedure TDFWord.SaveToStream(stream: TCoreClassStream);
begin
  stream.write(FBuffer, C_Word_Size);
end;

procedure TDFWord.LoadFromJson(jarry: TZ_JsonArray; index_: Integer);
begin
  FBuffer := jarry.i[index_];
end;

procedure TDFWord.SaveToJson(jarry: TZ_JsonArray; index_: Integer);
begin
  jarry.Add(FBuffer);
end;

function TDFWord.ComputeEncodeSize: Int64;
begin
  Result := C_Word_Size;
end;

constructor TDFByte.Create(ID: Byte);
begin
  inherited Create(ID);
  Buffer := 0;
end;

destructor TDFByte.Destroy;
begin
  inherited Destroy;
end;

procedure TDFByte.LoadFromStream(stream: TCoreClassStream);
begin
  stream.Read(FBuffer, C_Byte_Size);
end;

procedure TDFByte.SaveToStream(stream: TCoreClassStream);
begin
  stream.write(FBuffer, C_Byte_Size);
end;

procedure TDFByte.LoadFromJson(jarry: TZ_JsonArray; index_: Integer);
begin
  FBuffer := jarry.i[index_];
end;

procedure TDFByte.SaveToJson(jarry: TZ_JsonArray; index_: Integer);
begin
  jarry.Add(FBuffer);
end;

function TDFByte.ComputeEncodeSize: Int64;
begin
  Result := C_Byte_Size;
end;

constructor TDFSingle.Create(ID: Byte);
begin
  inherited Create(ID);
  Buffer := 0;
end;

destructor TDFSingle.Destroy;
begin
  inherited Destroy;
end;

procedure TDFSingle.LoadFromStream(stream: TCoreClassStream);
begin
  stream.Read(FBuffer, C_Single_Size);
end;

procedure TDFSingle.SaveToStream(stream: TCoreClassStream);
begin
  stream.write(FBuffer, C_Single_Size);
end;

procedure TDFSingle.LoadFromJson(jarry: TZ_JsonArray; index_: Integer);
begin
  FBuffer := jarry.f[index_];
end;

procedure TDFSingle.SaveToJson(jarry: TZ_JsonArray; index_: Integer);
begin
  jarry.AddF(FBuffer);
end;

function TDFSingle.ComputeEncodeSize: Int64;
begin
  Result := C_Single_Size;
end;

constructor TDFDouble.Create(ID: Byte);
begin
  inherited Create(ID);
  Buffer := 0;
end;

destructor TDFDouble.Destroy;
begin
  inherited Destroy;
end;

procedure TDFDouble.LoadFromStream(stream: TCoreClassStream);
begin
  stream.Read(FBuffer, C_Double_Size);
end;

procedure TDFDouble.SaveToStream(stream: TCoreClassStream);
begin
  stream.write(FBuffer, C_Double_Size);
end;

procedure TDFDouble.LoadFromJson(jarry: TZ_JsonArray; index_: Integer);
begin
  FBuffer := jarry.f[index_];
end;

procedure TDFDouble.SaveToJson(jarry: TZ_JsonArray; index_: Integer);
begin
  jarry.AddF(FBuffer);
end;

function TDFDouble.ComputeEncodeSize: Int64;
begin
  Result := C_Double_Size;
end;

constructor TDFArrayInteger.Create(ID: Byte);
begin
  inherited Create(ID);
  FBuffer := TMS64.CustomCreate(128);
end;

destructor TDFArrayInteger.Destroy;
begin
  Clear;
  DisposeObject(FBuffer);
  inherited Destroy;
end;

procedure TDFArrayInteger.Clear;
begin
  FBuffer.Clear;
end;

procedure TDFArrayInteger.Add(v: Integer);
begin
  FBuffer.Position := FBuffer.Size;
  FBuffer.WriteInt32(v);
end;

function TDFArrayInteger.Count: Integer;
begin
  Result := FBuffer.Size div C_Integer_Size;
end;

procedure TDFArrayInteger.WriteArray(const a: array of Integer);
begin
  if length(a) > 0 then
    begin
      FBuffer.Position := FBuffer.Size;
      FBuffer.WritePtr(@a[0], length(a) * C_Integer_Size);
    end;
end;

procedure TDFArrayInteger.LoadFromStream(stream: TCoreClassStream);
var
  L: Integer;
begin
  Clear;
  stream.Read(L, C_Integer_Size);
  FBuffer.CopyFrom(stream, L * C_Integer_Size);
end;

procedure TDFArrayInteger.SaveToStream(stream: TCoreClassStream);
var
  L: Integer;
begin
  L := Count;
  stream.write(L, C_Integer_Size);
  stream.write(FBuffer.Memory^, L * C_Integer_Size);
end;

procedure TDFArrayInteger.LoadFromJson(jarry: TZ_JsonArray; index_: Integer);
var
  ja: TZ_JsonArray;
  i: Integer;
begin
  ja := jarry.a[index_];
  for i := 0 to ja.Count - 1 do
      Add(ja.i[i]);
end;

procedure TDFArrayInteger.SaveToJson(jarry: TZ_JsonArray; index_: Integer);
var
  ja: TZ_JsonArray;
  i: Integer;
begin
  ja := jarry.AddArray;
  for i := 0 to Count - 1 do
      ja.Add(Buffer[i]);
end;

function TDFArrayInteger.ComputeEncodeSize: Int64;
begin
  Result := C_Integer_Size + C_Integer_Size * Count;
end;

function TDFArrayInteger.GetBuffer(index_: Integer): Integer;
begin
  Result := PInteger(FBuffer.PositionAsPtr(index_ * C_Integer_Size))^;
end;

procedure TDFArrayInteger.SetBuffer(index_: Integer; Value: Integer);
begin
  PInteger(FBuffer.PositionAsPtr(index_ * C_Integer_Size))^ := Value;
end;

constructor TDFArrayShortInt.Create(ID: ShortInt);
begin
  inherited Create(ID);
  FBuffer := TMS64.CustomCreate(128);
end;

destructor TDFArrayShortInt.Destroy;
begin
  Clear;
  DisposeObject(FBuffer);
  inherited Destroy;
end;

procedure TDFArrayShortInt.Clear;
begin
  FBuffer.Clear;
end;

procedure TDFArrayShortInt.Add(v: ShortInt);
begin
  FBuffer.Position := FBuffer.Size;
  FBuffer.WriteInt8(v);
end;

function TDFArrayShortInt.Count: Integer;
begin
  Result := FBuffer.Size;
end;

procedure TDFArrayShortInt.WriteArray(const a: array of ShortInt);
begin
  if length(a) > 0 then
    begin
      FBuffer.Position := FBuffer.Size;
      FBuffer.WritePtr(@a[0], length(a));
    end;
end;

procedure TDFArrayShortInt.LoadFromStream(stream: TCoreClassStream);
var
  L: Integer;
begin
  Clear;
  stream.Read(L, C_Integer_Size);
  FBuffer.CopyFrom(stream, L);
end;

procedure TDFArrayShortInt.SaveToStream(stream: TCoreClassStream);
var
  L: Integer;
begin
  L := Count;
  stream.write(L, C_Integer_Size);
  stream.write(FBuffer.Memory^, L);
end;

procedure TDFArrayShortInt.LoadFromJson(jarry: TZ_JsonArray; index_: Integer);
var
  ja: TZ_JsonArray;
  i: Integer;
begin
  ja := jarry.a[index_];
  for i := 0 to ja.Count - 1 do
      Add(ja.i[i]);
end;

procedure TDFArrayShortInt.SaveToJson(jarry: TZ_JsonArray; index_: Integer);
var
  ja: TZ_JsonArray;
  i: Integer;
begin
  ja := jarry.AddArray;
  for i := 0 to Count - 1 do
      ja.Add(Buffer[i]);
end;

function TDFArrayShortInt.ComputeEncodeSize: Int64;
begin
  Result := C_Integer_Size + C_Short_Int_Size * Count;
end;

function TDFArrayShortInt.GetBuffer(index_: Integer): ShortInt;
begin
  Result := PShortInt(FBuffer.PositionAsPtr(index_))^;
end;

procedure TDFArrayShortInt.SetBuffer(index_: Integer; Value: ShortInt);
begin
  PShortInt(FBuffer.PositionAsPtr(index_))^ := Value;
end;

constructor TDFArrayByte.Create(ID: Byte);
begin
  inherited Create(ID);
  FBuffer := TMS64.CustomCreate(128);
end;

destructor TDFArrayByte.Destroy;
begin
  Clear;
  DisposeObject(FBuffer);
  inherited Destroy;
end;

procedure TDFArrayByte.Clear;
begin
  FBuffer.Clear;
end;

procedure TDFArrayByte.Add(v: Byte);
begin
  FBuffer.Position := FBuffer.Size;
  FBuffer.WriteUInt8(v);
end;

procedure TDFArrayByte.AddPtrBuff(p: PByte; Size_: Integer);
begin
  FBuffer.Position := FBuffer.Size;
  FBuffer.WritePtr(p, Size_);
end;

procedure TDFArrayByte.AddI64(v: Int64);
begin
  AddPtrBuff(@v, C_Int64_Size);
end;

procedure TDFArrayByte.AddU64(v: UInt64);
begin
  AddPtrBuff(@v, C_UInt64_Size);
end;

procedure TDFArrayByte.Addi(v: Integer);
begin
  AddPtrBuff(@v, C_Integer_Size);
end;

procedure TDFArrayByte.AddWord(v: Word);
begin
  AddPtrBuff(@v, C_Word_Size);
end;

function TDFArrayByte.Count: Int64;
begin
  Result := FBuffer.Size;
end;

procedure TDFArrayByte.WriteArray(const a: array of Byte);
begin
  if length(a) > 0 then
      AddPtrBuff(@a[0], length(a));
end;

procedure TDFArrayByte.SetArray(const a: array of Byte);
begin
  Clear;
  if length(a) > 0 then
      AddPtrBuff(@a[0], length(a));
end;

procedure TDFArrayByte.SetBuff(p: PByte; Size_: Integer);
begin
  Clear;
  AddPtrBuff(p, Size_);
end;

procedure TDFArrayByte.GetBuff(p: PByte);
begin
  CopyPtr(FBuffer.Memory, p, FBuffer.Size);
end;

procedure TDFArrayByte.LoadFromStream(stream: TCoreClassStream);
var
  L: Integer;
begin
  Clear;
  stream.Read(L, C_Integer_Size);
  FBuffer.CopyFrom(stream, L);
end;

procedure TDFArrayByte.SaveToStream(stream: TCoreClassStream);
var
  L: Integer;
begin
  L := Count;
  stream.write(L, C_Integer_Size);
  stream.write(FBuffer.Memory^, L);
end;

procedure TDFArrayByte.LoadFromJson(jarry: TZ_JsonArray; index_: Integer);
var
  ja: TZ_JsonArray;
  i: Integer;
begin
  ja := jarry.a[index_];
  for i := 0 to ja.Count - 1 do
      Add(ja.i[i]);
end;

procedure TDFArrayByte.SaveToJson(jarry: TZ_JsonArray; index_: Integer);
var
  ja: TZ_JsonArray;
  i: Integer;
begin
  ja := jarry.AddArray;
  for i := 0 to Count - 1 do
      ja.Add(Buffer[i]);
end;

function TDFArrayByte.ComputeEncodeSize: Int64;
begin
  Result := C_Integer_Size + C_Byte_Size * Count;
end;

function TDFArrayByte.GetBuffer(index_: Integer): Byte;
begin
  Result := PByte(FBuffer.PositionAsPtr(index_))^;
end;

procedure TDFArrayByte.SetBuffer(index_: Integer; Value: Byte);
begin
  PByte(FBuffer.PositionAsPtr(index_))^ := Value;
end;

constructor TDFArraySingle.Create(ID: Byte);
begin
  inherited Create(ID);
  FBuffer := TMS64.CustomCreate(128);
end;

destructor TDFArraySingle.Destroy;
begin
  Clear;
  DisposeObject(FBuffer);
  inherited Destroy;
end;

procedure TDFArraySingle.Clear;
begin
  FBuffer.Clear;
end;

procedure TDFArraySingle.Add(v: Single);
begin
  FBuffer.Position := FBuffer.Size;
  FBuffer.WriteSingle(v);
end;

function TDFArraySingle.Count: Integer;
begin
  Result := FBuffer.Size div C_Single_Size;
end;

procedure TDFArraySingle.WriteArray(const a: array of Single);
begin
  if length(a) > 0 then
    begin
      FBuffer.Position := FBuffer.Size;
      FBuffer.WritePtr(@a[0], length(a) * C_Single_Size);
    end;
end;

procedure TDFArraySingle.LoadFromStream(stream: TCoreClassStream);
var
  L: Integer;
begin
  Clear;
  stream.Read(L, C_Integer_Size);
  FBuffer.CopyFrom(stream, L * C_Single_Size);
end;

procedure TDFArraySingle.SaveToStream(stream: TCoreClassStream);
var
  L: Integer;
begin
  L := Count;
  stream.write(L, C_Integer_Size);
  stream.write(FBuffer.Memory^, L * C_Single_Size);
end;

procedure TDFArraySingle.LoadFromJson(jarry: TZ_JsonArray; index_: Integer);
var
  ja: TZ_JsonArray;
  i: Integer;
begin
  ja := jarry.a[index_];
  for i := 0 to ja.Count - 1 do
      Add(ja.f[i]);
end;

procedure TDFArraySingle.SaveToJson(jarry: TZ_JsonArray; index_: Integer);
var
  ja: TZ_JsonArray;
  i: Integer;
begin
  ja := jarry.AddArray;
  for i := 0 to Count - 1 do
      ja.AddF(Buffer[i]);
end;

function TDFArraySingle.ComputeEncodeSize: Int64;
begin
  Result := C_Integer_Size + C_Single_Size * Count;
end;

function TDFArraySingle.GetBuffer(index_: Integer): Single;
begin
  Result := PSingle(FBuffer.PositionAsPtr(index_ * C_Single_Size))^;
end;

procedure TDFArraySingle.SetBuffer(index_: Integer; Value: Single);
begin
  PSingle(FBuffer.PositionAsPtr(index_ * C_Single_Size))^ := Value;
end;

constructor TDFArrayDouble.Create(ID: Byte);
begin
  inherited Create(ID);
  FBuffer := TMS64.CustomCreate(128);
end;

destructor TDFArrayDouble.Destroy;
begin
  Clear;
  DisposeObject(FBuffer);
  inherited Destroy;
end;

procedure TDFArrayDouble.Clear;
begin
  FBuffer.Clear;
end;

procedure TDFArrayDouble.Add(v: Double);
begin
  FBuffer.Position := FBuffer.Size;
  FBuffer.WriteDouble(v);
end;

function TDFArrayDouble.Count: Integer;
begin
  Result := FBuffer.Size div C_Double_Size;
end;

procedure TDFArrayDouble.WriteArray(const a: array of Double);
begin
  if length(a) > 0 then
    begin
      FBuffer.Position := FBuffer.Size;
      FBuffer.WritePtr(@a[0], length(a) * C_Double_Size);
    end;
end;

procedure TDFArrayDouble.LoadFromStream(stream: TCoreClassStream);
var
  L: Integer;
begin
  Clear;
  stream.Read(L, C_Integer_Size);
  FBuffer.CopyFrom(stream, L * C_Double_Size);
end;

procedure TDFArrayDouble.SaveToStream(stream: TCoreClassStream);
var
  L: Integer;
begin
  L := Count;
  stream.write(L, C_Integer_Size);
  stream.write(FBuffer.Memory^, L * C_Double_Size);
end;

procedure TDFArrayDouble.LoadFromJson(jarry: TZ_JsonArray; index_: Integer);
var
  ja: TZ_JsonArray;
  i: Integer;
begin
  ja := jarry.a[index_];
  for i := 0 to ja.Count - 1 do
      Add(ja.f[i]);
end;

procedure TDFArrayDouble.SaveToJson(jarry: TZ_JsonArray; index_: Integer);
var
  ja: TZ_JsonArray;
  i: Integer;
begin
  ja := jarry.AddArray;
  for i := 0 to Count - 1 do
      ja.AddF(Buffer[i]);
end;

function TDFArrayDouble.ComputeEncodeSize: Int64;
begin
  Result := C_Integer_Size + C_Double_Size * Count;
end;

function TDFArrayDouble.GetBuffer(index_: Integer): Double;
begin
  Result := PDouble(FBuffer.PositionAsPtr(index_ * C_Double_Size))^;
end;

procedure TDFArrayDouble.SetBuffer(index_: Integer; Value: Double);
begin
  PDouble(FBuffer.PositionAsPtr(index_ * C_Double_Size))^ := Value;
end;

constructor TDFArrayInt64.Create(ID: Byte);
begin
  inherited Create(ID);
  FBuffer := TMS64.CustomCreate(128);
end;

destructor TDFArrayInt64.Destroy;
begin
  Clear;
  DisposeObject(FBuffer);
  inherited Destroy;
end;

procedure TDFArrayInt64.Clear;
begin
  FBuffer.Clear;
end;

procedure TDFArrayInt64.Add(v: Int64);
begin
  FBuffer.Position := FBuffer.Size;
  FBuffer.WriteInt64(v);
end;

function TDFArrayInt64.Count: Integer;
begin
  Result := FBuffer.Size div C_Int64_Size;
end;

procedure TDFArrayInt64.WriteArray(const a: array of Int64);
begin
  if length(a) > 0 then
    begin
      FBuffer.Position := FBuffer.Size;
      FBuffer.WritePtr(@a[0], length(a) * C_Int64_Size);
    end;
end;

procedure TDFArrayInt64.LoadFromStream(stream: TCoreClassStream);
var
  L: Integer;
begin
  Clear;
  stream.Read(L, C_Integer_Size);
  FBuffer.CopyFrom(stream, L * C_Int64_Size);
end;

procedure TDFArrayInt64.SaveToStream(stream: TCoreClassStream);
var
  L: Integer;
begin
  L := Count;
  stream.write(L, C_Integer_Size);
  stream.write(FBuffer.Memory^, L * C_Int64_Size);
end;

procedure TDFArrayInt64.LoadFromJson(jarry: TZ_JsonArray; index_: Integer);
var
  ja: TZ_JsonArray;
  i: Integer;
begin
  ja := jarry.a[index_];
  for i := 0 to ja.Count - 1 do
      Add(ja.L[i]);
end;

procedure TDFArrayInt64.SaveToJson(jarry: TZ_JsonArray; index_: Integer);
var
  ja: TZ_JsonArray;
  i: Integer;
begin
  ja := jarry.AddArray;
  for i := 0 to Count - 1 do
      ja.Add(Buffer[i]);
end;

function TDFArrayInt64.ComputeEncodeSize: Int64;
begin
  Result := C_Integer_Size + C_Int64_Size * Count;
end;

function TDFArrayInt64.GetBuffer(index_: Integer): Int64;
begin
  Result := PInt64(FBuffer.PositionAsPtr(index_ * C_Int64_Size))^;
end;

procedure TDFArrayInt64.SetBuffer(index_: Integer; Value: Int64);
begin
  PInt64(FBuffer.PositionAsPtr(index_ * C_Int64_Size))^ := Value;
end;

constructor TDFStream.Create(ID: Byte);
begin
  inherited Create(ID);
  FBuffer := TMS64.Create;
end;

destructor TDFStream.Destroy;
begin
  DisposeObject(FBuffer);
  inherited Destroy;
end;

procedure TDFStream.Clear;
begin
  FBuffer.Clear;
end;

procedure TDFStream.LoadFromStream(stream: TCoreClassStream);
var
  _Len: Integer;
begin
  FBuffer.Clear;
  stream.Read(_Len, C_Integer_Size);
  if (_Len > 0) then
      FBuffer.CopyFrom(stream, _Len);
end;

procedure TDFStream.SaveToStream(stream: TCoreClassStream);
var
  _Len: Integer;
begin
  _Len := FBuffer.Size;
  stream.write(_Len, C_Integer_Size);
  if _Len > 0 then
    begin
      FBuffer.Position := 0;
      stream.CopyFrom(FBuffer, _Len);
    end;
end;

procedure TDFStream.LoadFromJson(jarry: TZ_JsonArray; index_: Integer);
var
  b64: TPascalString;
begin
  FBuffer.Clear;
  b64.Text := jarry.s[index_];
  umlDecodeStreamBASE64(b64, FBuffer);
end;

procedure TDFStream.SaveToJson(jarry: TZ_JsonArray; index_: Integer);
var
  b64: TPascalString;
begin
  umlEncodeStreamBASE64(FBuffer, b64);
  jarry.Add(b64.Text);
end;

function TDFStream.ComputeEncodeSize: Int64;
begin
  Result := C_Integer_Size + FBuffer.Size;
end;

function TDFStream.GetBuffer: TCoreClassStream;
begin
  Result := FBuffer;
end;

procedure TDFStream.SetBuffer(_Buffer: TCoreClassStream);
var
  p_: Int64;
begin
  FBuffer.Clear;
  p_ := _Buffer.Position;
  _Buffer.Position := 0;
  if _Buffer.Size > 0 then
      FBuffer.CopyFrom(_Buffer, _Buffer.Size);
  _Buffer.Position := p_;
end;

constructor TDFVariant.Create(ID: Byte);
begin
  inherited Create(ID);
end;

destructor TDFVariant.Destroy;
begin
  inherited Destroy;
end;

procedure TDFVariant.LoadFromStream(stream: TCoreClassStream);
var
  vt: TVarType;
begin
  vt := TVarType(StreamReadUInt16(stream));
  case vt of
    varEmpty, varNull: FBuffer := NULL;
    varSmallInt: FBuffer := StreamReadInt16(stream);
    varInteger: FBuffer := StreamReadInt32(stream);
    varSingle: FBuffer := StreamReadSingle(stream);
    varDouble: FBuffer := StreamReadDouble(stream);
    varCurrency: FBuffer := StreamReadCurrency(stream);
    varBoolean: FBuffer := StreamReadBool(stream);
    varShortInt: FBuffer := StreamReadInt8(stream);
    varByte: FBuffer := StreamReadUInt8(stream);
    varWord: FBuffer := StreamReadUInt16(stream);
    varLongWord: FBuffer := StreamReadUInt32(stream);
    varInt64: FBuffer := StreamReadInt64(stream);
    varUInt64: FBuffer := StreamReadUInt64(stream);
    varOleStr, varString, varUString: FBuffer := StreamReadString(stream).Text;
    else RaiseInfo('error variant type');
  end;
end;

procedure TDFVariant.SaveToStream(stream: TCoreClassStream);
var
  vt: TVarType;
begin
  vt := TVarData(FBuffer).VType;
  StreamWriteUInt16(stream, Word(vt));
  case vt of
    varEmpty, varNull:;
    varSmallInt: StreamWriteInt16(stream, FBuffer);
    varInteger: StreamWriteInt32(stream, FBuffer);
    varSingle: StreamWriteSingle(stream, FBuffer);
    varDouble: StreamWriteDouble(stream, FBuffer);
    varCurrency: StreamWriteCurrency(stream, FBuffer);
    varBoolean: StreamWriteBool(stream, FBuffer);
    varShortInt: StreamWriteInt8(stream, FBuffer);
    varByte: StreamWriteUInt8(stream, FBuffer);
    varWord: StreamWriteUInt16(stream, FBuffer);
    varLongWord: StreamWriteUInt32(stream, FBuffer);
    varInt64: StreamWriteInt64(stream, FBuffer);
    varUInt64: StreamWriteUInt64(stream, FBuffer);
    varOleStr, varString, varUString: StreamWriteString(stream, SystemString(FBuffer));
    else
      RaiseInfo('error variant type');
  end;
end;

procedure TDFVariant.LoadFromJson(jarry: TZ_JsonArray; index_: Integer);
begin
  FBuffer := umlStrToVar(jarry.s[index_]);
end;

procedure TDFVariant.SaveToJson(jarry: TZ_JsonArray; index_: Integer);
begin
  jarry.Add(umlVarToStr(FBuffer, True).Text);
end;

function TDFVariant.ComputeEncodeSize: Int64;
begin
  case TVarData(FBuffer).VType of
    varEmpty, varNull: Result := 2 + 0;
    varSmallInt: Result := 2 + 2;
    varInteger: Result := 2 + 4;
    varSingle: Result := 2 + 4;
    varDouble: Result := 2 + 8;
    varCurrency: Result := 2 + 8;
    varBoolean: Result := 2 + 1;
    varShortInt: Result := 2 + 1;
    varByte: Result := 2 + 1;
    varWord: Result := 2 + 2;
    varLongWord: Result := 2 + 4;
    varInt64: Result := 2 + 8;
    varUInt64: Result := 2 + 8;
    varOleStr, varString, varUString: Result := 2 + ComputeStreamWriteStringSize(SystemString(FBuffer));
    else
      RaiseInfo('error variant type');
  end;
end;

constructor TDFInt64.Create(ID: Byte);
begin
  inherited Create(ID);
  Buffer := 0;
end;

destructor TDFInt64.Destroy;
begin
  inherited Destroy;
end;

procedure TDFInt64.LoadFromStream(stream: TCoreClassStream);
begin
  stream.Read(FBuffer, C_Int64_Size);
end;

procedure TDFInt64.SaveToStream(stream: TCoreClassStream);
begin
  stream.write(FBuffer, C_Int64_Size);
end;

procedure TDFInt64.LoadFromJson(jarry: TZ_JsonArray; index_: Integer);
begin
  FBuffer := jarry.L[index_];
end;

procedure TDFInt64.SaveToJson(jarry: TZ_JsonArray; index_: Integer);
begin
  jarry.Add(FBuffer);
end;

function TDFInt64.ComputeEncodeSize: Int64;
begin
  Result := C_Int64_Size;
end;

constructor TDFUInt64.Create(ID: Byte);
begin
  inherited Create(ID);
  Buffer := 0;
end;

destructor TDFUInt64.Destroy;
begin
  inherited Destroy;
end;

procedure TDFUInt64.LoadFromStream(stream: TCoreClassStream);
begin
  stream.Read(FBuffer, C_UInt64_Size);
end;

procedure TDFUInt64.SaveToStream(stream: TCoreClassStream);
begin
  stream.write(FBuffer, C_UInt64_Size);
end;

procedure TDFUInt64.LoadFromJson(jarry: TZ_JsonArray; index_: Integer);
begin
  FBuffer := jarry.u[index_];
end;

procedure TDFUInt64.SaveToJson(jarry: TZ_JsonArray; index_: Integer);
begin
  jarry.Add(FBuffer);
end;

function TDFUInt64.ComputeEncodeSize: Int64;
begin
  Result := C_UInt64_Size;
end;

constructor TDFEngineReader.Create(Owner_: TDFE);
begin
  inherited Create;
  FOwner := Owner_;
  FIndex := 0;
end;

destructor TDFEngineReader.Destroy;
begin
  inherited Destroy;
end;

function TDFEngineReader.IsEnd: Boolean;
begin
  Result := FIndex >= FOwner.Count;
end;

function TDFEngineReader.NotEnd: Boolean;
begin
  Result := FIndex < FOwner.Count;
end;

procedure TDFEngineReader.GoNext;
begin
  inc(FIndex);
end;

function TDFEngineReader.ReadString: SystemString;
begin
  Result := FOwner.ReadString(FIndex);
  inc(FIndex);
end;

function TDFEngineReader.ReadInteger: Integer;
begin
  Result := FOwner.ReadInteger(FIndex);
  inc(FIndex);
end;

function TDFEngineReader.ReadCardinal: Cardinal;
begin
  Result := FOwner.ReadCardinal(FIndex);
  inc(FIndex);
end;

function TDFEngineReader.ReadWord: Word;
begin
  Result := FOwner.ReadWord(FIndex);
  inc(FIndex);
end;

function TDFEngineReader.ReadBool: Boolean;
begin
  Result := FOwner.ReadBool(FIndex);
  inc(FIndex);
end;

function TDFEngineReader.ReadBoolean: Boolean;
begin
  Result := ReadBool;
end;

function TDFEngineReader.ReadByte: Byte;
begin
  Result := FOwner.ReadByte(FIndex);
  inc(FIndex);
end;

function TDFEngineReader.ReadSingle: Single;
begin
  Result := FOwner.ReadSingle(FIndex);
  inc(FIndex);
end;

function TDFEngineReader.ReadDouble: Double;
begin
  Result := FOwner.ReadDouble(FIndex);
  inc(FIndex);
end;

function TDFEngineReader.ReadArrayInteger: TDFArrayInteger;
begin
  Result := FOwner.ReadArrayInteger(FIndex);
  inc(FIndex);
end;

function TDFEngineReader.ReadArrayShortInt: TDFArrayShortInt;
begin
  Result := FOwner.ReadArrayShortInt(FIndex);
  inc(FIndex);
end;

function TDFEngineReader.ReadArrayByte: TDFArrayByte;
begin
  Result := FOwner.ReadArrayByte(FIndex);
  inc(FIndex);
end;

function TDFEngineReader.ReadMD5: TMD5;
begin
  Result := FOwner.ReadMD5(FIndex);
  inc(FIndex);
end;

function TDFEngineReader.ReadArraySingle: TDFArraySingle;
begin
  Result := FOwner.ReadArraySingle(FIndex);
  inc(FIndex);
end;

function TDFEngineReader.ReadArrayDouble: TDFArrayDouble;
begin
  Result := FOwner.ReadArrayDouble(FIndex);
  inc(FIndex);
end;

function TDFEngineReader.ReadArrayInt64: TDFArrayInt64;
begin
  Result := FOwner.ReadArrayInt64(FIndex);
  inc(FIndex);
end;

procedure TDFEngineReader.ReadStream(output: TCoreClassStream);
begin
  FOwner.ReadStream(FIndex, output);
  inc(FIndex);
end;

function TDFEngineReader.ReadVariant: Variant;
begin
  Result := FOwner.ReadVariant(FIndex);
  inc(FIndex);
end;

function TDFEngineReader.ReadInt64: Int64;
begin
  Result := FOwner.ReadInt64(FIndex);
  inc(FIndex);
end;

function TDFEngineReader.ReadUInt64: UInt64;
begin
  Result := FOwner.ReadUInt64(FIndex);
  inc(FIndex);
end;

procedure TDFEngineReader.ReadStrings(output: TCoreClassStrings);
begin
  FOwner.ReadStrings(FIndex, output);
  inc(FIndex);
end;

procedure TDFEngineReader.ReadListStrings(output: TListString);
begin
  FOwner.ReadListStrings(FIndex, output);
  inc(FIndex);
end;

procedure TDFEngineReader.ReadPascalStrings(output: TListPascalString);
begin
  FOwner.ReadPascalStrings(FIndex, output);
  inc(FIndex);
end;

procedure TDFEngineReader.ReadDataFrame(output: TDFE);
begin
  FOwner.ReadDataFrame(FIndex, output);
  inc(FIndex);
end;

procedure TDFEngineReader.ReadHashStringList(output: THashStringList);
begin
  FOwner.ReadHashStringList(FIndex, output);
  inc(FIndex);
end;

procedure TDFEngineReader.ReadVariantList(output: THashVariantList);
begin
  FOwner.ReadVariantList(FIndex, output);
  inc(FIndex);
end;

procedure TDFEngineReader.ReadSectionText(output: TSectionTextData);
begin
  FOwner.ReadSectionText(FIndex, output);
  inc(FIndex);
end;

procedure TDFEngineReader.ReadTextSection(output: TSectionTextData);
begin
  ReadSectionText(output);
end;

procedure TDFEngineReader.ReadJson(output: TZ_JsonObject);
begin
  FOwner.ReadJson(FIndex, output);
  inc(FIndex);
end;

{$IFDEF DELPHI}


procedure TDFEngineReader.ReadJson(output: TJsonObject);
begin
  FOwner.ReadJson(FIndex, output);
  inc(FIndex);
end;
{$ENDIF DELPHI}


function TDFEngineReader.ReadRect: TRect;
begin
  Result := FOwner.ReadRect(FIndex);
  inc(FIndex);
end;

function TDFEngineReader.ReadRectf: TRectf;
begin
  Result := FOwner.ReadRectf(FIndex);
  inc(FIndex);
end;

function TDFEngineReader.ReadPoint: TPoint;
begin
  Result := FOwner.ReadPoint(FIndex);
  inc(FIndex);
end;

function TDFEngineReader.ReadPointf: TPointf;
begin
  Result := FOwner.ReadPointf(FIndex);
  inc(FIndex);
end;

function TDFEngineReader.ReadVector: TVector;
begin
  Result := FOwner.ReadVector(FIndex);
  inc(FIndex);
end;

function TDFEngineReader.ReadAffineVector: TAffineVector;
begin
  Result := FOwner.ReadAffineVector(FIndex);
  inc(FIndex);
end;

function TDFEngineReader.ReadVec3: TVec3;
begin
  Result := FOwner.ReadVec3(FIndex);
  inc(FIndex);
end;

function TDFEngineReader.ReadVec4: TVec4;
begin
  Result := FOwner.ReadVec4(FIndex);
  inc(FIndex);
end;

function TDFEngineReader.ReadVector3: TVector3;
begin
  Result := FOwner.ReadVector3(FIndex);
  inc(FIndex);
end;

function TDFEngineReader.ReadVector4: TVector4;
begin
  Result := FOwner.ReadVector4(FIndex);
  inc(FIndex);
end;

function TDFEngineReader.ReadMat4: TMat4;
begin
  Result := FOwner.ReadMat4(FIndex);
  inc(FIndex);
end;

function TDFEngineReader.ReadMatrix4: TMatrix4;
begin
  Result := FOwner.ReadMatrix4(FIndex);
  inc(FIndex);
end;

function TDFEngineReader.Read2DPoint: T2DPoint;
begin
  Result := FOwner.Read2DPoint(FIndex);
  inc(FIndex);
end;

function TDFEngineReader.ReadVec2: TVec2;
begin
  Result := FOwner.ReadVec2(FIndex);
  inc(FIndex);
end;

function TDFEngineReader.ReadRectV2: TRectV2;
begin
  Result := FOwner.ReadRectV2(FIndex);
  inc(FIndex);
end;

function TDFEngineReader.ReadPointer: UInt64;
begin
  Result := FOwner.ReadPointer(FIndex);
  inc(FIndex);
end;

procedure TDFEngineReader.ReadNM(output: TNumberModule);
begin
  FOwner.ReadNM(FIndex, output);
  inc(FIndex);
end;

procedure TDFEngineReader.ReadNMPool(output: TNumberModulePool);
begin
  FOwner.ReadNMPool(FIndex, output);
  inc(FIndex);
end;

procedure TDFEngineReader.Read(var Buf_; Count_: Int64);
begin
  FOwner.Read(FIndex, Buf_, Count_);
  inc(FIndex);
end;

function TDFEngineReader.Read: TDFBase;
begin
  Result := FOwner.Read(FIndex);
  inc(FIndex);
end;

function TDFEngineReader.Current: TDFBase;
begin
  Result := FOwner.Read(FIndex);
end;

function TDFE.DataTypeToByte(v: TRunTimeDataType): Byte;
begin
  Result := Byte(v);
end;

function TDFE.ByteToDataType(v: Byte): TRunTimeDataType;
begin
  Result := TRunTimeDataType(v);
end;

constructor TDFE.Create;
begin
  inherited Create;
  FDataList := TCoreClassListForObj.Create;
  FReader := TDFEngineReader.Create(Self);
  FCompressorDeflate := nil;
  FCompressorBRRC := nil;
  FIsChanged := False;
end;

destructor TDFE.Destroy;
begin
  Clear;
  DisposeObject(FDataList);
  DisposeObject(FReader);
  if FCompressorDeflate <> nil then
      DisposeObject(FCompressorDeflate);
  if FCompressorBRRC <> nil then
      DisposeObject(FCompressorBRRC);
  inherited Destroy;
end;

procedure TDFE.SwapInstance(source: TDFE);
var
  tmp_DataList: TCoreClassListForObj;
  tmp_Reader: TDFEngineReader;
begin
  if Self = source then
      exit;
  tmp_DataList := FDataList;
  tmp_Reader := FReader;

  FDataList := source.FDataList;
  FReader := source.FReader;

  source.FDataList := tmp_DataList;
  source.FReader := tmp_Reader;
  FIsChanged := True;
  source.FIsChanged := True;
end;

procedure TDFE.Clear;
var
  i: Integer;
  Obj: TDFBase;
begin
  for i := 0 to FDataList.Count - 1 do
    begin
      Obj := TDFBase(FDataList[i]);
      try
          DisposeObject(Obj);
      except
      end;
    end;

  try
      FDataList.Clear;
  except
  end;

  FReader.index := 0;
end;

function TDFE.AddData(v: TRunTimeDataType): TDFBase;
begin
  case v of
    rdtString: Result := TDFString.Create(DataTypeToByte(v));
    rdtInteger: Result := TDFInteger.Create(DataTypeToByte(v));
    rdtCardinal: Result := TDFCardinal.Create(DataTypeToByte(v));
    rdtWORD: Result := TDFWord.Create(DataTypeToByte(v));
    rdtByte: Result := TDFByte.Create(DataTypeToByte(v));
    rdtSingle: Result := TDFSingle.Create(DataTypeToByte(v));
    rdtDouble: Result := TDFDouble.Create(DataTypeToByte(v));
    rdtArrayInteger: Result := TDFArrayInteger.Create(DataTypeToByte(v));
    rdtArrayShortInt: Result := TDFArrayShortInt.Create(DataTypeToByte(v));
    rdtArrayByte: Result := TDFArrayByte.Create(DataTypeToByte(v));
    rdtArraySingle: Result := TDFArraySingle.Create(DataTypeToByte(v));
    rdtArrayDouble: Result := TDFArrayDouble.Create(DataTypeToByte(v));
    rdtArrayInt64: Result := TDFArrayInt64.Create(DataTypeToByte(v));
    rdtStream: Result := TDFStream.Create(DataTypeToByte(v));
    rdtVariant: Result := TDFVariant.Create(DataTypeToByte(v));
    rdtInt64: Result := TDFInt64.Create(DataTypeToByte(v));
    rdtUInt64: Result := TDFUInt64.Create(DataTypeToByte(v));
    else
      Result := nil;
  end;
  if Result <> nil then
      FDataList.Add(Result);
  FIsChanged := True;
end;

function TDFE.GetData(index_: Integer): TDFBase;
begin
  if (index_ >= 0) and (index_ < FDataList.Count) then
      Result := TDFBase(FDataList[index_])
  else
      Result := nil;
end;

function TDFE.GetDataInfo(Obj_: TDFBase): SystemString;
begin
  case ByteToDataType(Obj_.FID) of
    rdtString: Result := 'SystemString';
    rdtInteger: Result := 'Integer';
    rdtCardinal: Result := 'Cardinal';
    rdtWORD: Result := 'WORD';
    rdtByte: Result := 'Byte';
    rdtSingle: Result := 'Single';
    rdtDouble: Result := 'Double';
    rdtArrayInteger: Result := 'ArrayInteger';
    rdtArrayShortInt: Result := 'ShortInt';
    rdtArrayByte: Result := 'Byte';
    rdtArraySingle: Result := 'ArraySingle';
    rdtArrayDouble: Result := 'ArrayDouble';
    rdtArrayInt64: Result := 'ArrayInt64';
    rdtStream: Result := 'Stream';
    rdtVariant: Result := 'Variant';
    rdtInt64: Result := 'Int64';
    rdtUInt64: Result := 'UInt64';
    else
      Result := '';
  end;
end;

function TDFE.Count: Integer;
begin
  Result := FDataList.Count;
end;

function TDFE.Delete(index_: Integer): Boolean;
begin
  try
    DisposeObject(TDFBase(FDataList[index_]));
    FDataList.Delete(index_);
    Result := True;
  except
      Result := False;
  end;
end;

function TDFE.DeleteFirst: Boolean;
begin
  Result := Delete(0);
end;

function TDFE.DeleteLast: Boolean;
begin
  Result := Delete(Count - 1);
end;

function TDFE.DeleteLastCount(num_: Integer): Boolean;
begin
  Result := True;
  while num_ > 0 do
    begin
      Result := Result and DeleteLast;
      dec(num_);
    end;
end;

function TDFE.DeleteCount(index_, Count_: Integer): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to Count_ - 1 do
      Result := Result and Delete(index_);
end;

procedure TDFE.Assign(source: TDFE);
var
  m64: TMS64;
  i: Integer;
  DataFrame_: TDFBase;
begin
  if Self = source then
      exit;
  Clear;
  m64 := TMS64.CustomCreate(8192);
  for i := 0 to source.Count - 1 do
    begin
      DataFrame_ := AddData(ByteToDataType(source[i].FID));
      source[i].SaveToStream(m64);
      m64.Position := 0;
      DataFrame_.LoadFromStream(m64);
      m64.Clear;
    end;
  DisposeObject(m64);
end;

function TDFE.Clone: TDFE;
begin
  Result := TDFE.Create;
  Result.Assign(Self);
end;

procedure TDFE.WriteString(v: SystemString);
var
  Obj_: TDFString;
begin
  Obj_ := TDFString.Create(DataTypeToByte(rdtString));
  Obj_.Buffer := umlBytesOf(v);
  FDataList.Add(Obj_);
end;

procedure TDFE.WriteString(v: TPascalString);
var
  Obj_: TDFString;
begin
  Obj_ := TDFString.Create(DataTypeToByte(rdtString));
  Obj_.Buffer := v.Bytes;
  FDataList.Add(Obj_);
end;

procedure TDFE.WriteString(const Fmt: SystemString; const Args: array of const);
begin
  WriteString(PFormat(Fmt, Args));
end;

procedure TDFE.WriteInteger(v: Integer);
var
  Obj_: TDFInteger;
begin
  Obj_ := TDFInteger.Create(DataTypeToByte(rdtInteger));
  Obj_.Buffer := v;
  FDataList.Add(Obj_);
end;

procedure TDFE.WriteCardinal(v: Cardinal);
var
  Obj_: TDFCardinal;
begin
  Obj_ := TDFCardinal.Create(DataTypeToByte(rdtCardinal));
  Obj_.Buffer := v;
  FDataList.Add(Obj_);
end;

procedure TDFE.WriteWORD(v: Word);
var
  Obj_: TDFWord;
begin
  Obj_ := TDFWord.Create(DataTypeToByte(rdtWORD));
  Obj_.Buffer := v;
  FDataList.Add(Obj_);
end;

procedure TDFE.WriteBool(v: Boolean);
begin
  if v then
      WriteByte(1)
  else
      WriteByte(0);
end;

procedure TDFE.WriteBoolean(v: Boolean);
begin
  WriteBool(v);
end;

procedure TDFE.WriteByte(v: Byte);
var
  Obj_: TDFByte;
begin
  Obj_ := TDFByte.Create(DataTypeToByte(rdtByte));
  Obj_.Buffer := v;
  FDataList.Add(Obj_);
end;

procedure TDFE.WriteSingle(v: Single);
var
  Obj_: TDFSingle;
begin
  Obj_ := TDFSingle.Create(DataTypeToByte(rdtSingle));
  Obj_.Buffer := v;
  FDataList.Add(Obj_);
end;

procedure TDFE.WriteDouble(v: Double);
var
  Obj_: TDFDouble;
begin
  Obj_ := TDFDouble.Create(DataTypeToByte(rdtDouble));
  Obj_.Buffer := v;
  FDataList.Add(Obj_);
end;

function TDFE.WriteArrayInteger: TDFArrayInteger;
begin
  Result := TDFArrayInteger.Create(DataTypeToByte(rdtArrayInteger));
  FDataList.Add(Result);
end;

function TDFE.WriteArrayShortInt: TDFArrayShortInt;
begin
  Result := TDFArrayShortInt.Create(DataTypeToByte(rdtArrayShortInt));
  FDataList.Add(Result);
end;

function TDFE.WriteArrayByte: TDFArrayByte;
begin
  Result := TDFArrayByte.Create(DataTypeToByte(rdtArrayByte));
  FDataList.Add(Result);
end;

procedure TDFE.WriteMD5(md5: TMD5);
begin
  WriteArrayByte.SetBuff(@md5[0], SizeOf(TMD5));
end;

function TDFE.WriteArraySingle: TDFArraySingle;
begin
  Result := TDFArraySingle.Create(DataTypeToByte(rdtArraySingle));
  FDataList.Add(Result);
end;

function TDFE.WriteArrayDouble: TDFArrayDouble;
begin
  Result := TDFArrayDouble.Create(DataTypeToByte(rdtArrayDouble));
  FDataList.Add(Result);
end;

function TDFE.WriteArrayInt64: TDFArrayInt64;
begin
  Result := TDFArrayInt64.Create(DataTypeToByte(rdtArrayInt64));
  FDataList.Add(Result);
end;

procedure TDFE.WriteStream(v: TCoreClassStream);
var
  Obj_: TDFStream;
begin
  Obj_ := TDFStream.Create(DataTypeToByte(rdtStream));
  Obj_.Buffer := v;
  FDataList.Add(Obj_);
end;

procedure TDFE.WriteInstanceStream(v: TMS64);
var
  Obj_: TDFStream;
begin
  Obj_ := TDFStream.Create(DataTypeToByte(rdtStream));
  Obj_.InstanceBuffer := v;
  FDataList.Add(Obj_);
end;

procedure TDFE.WriteVariant(v: Variant);
var
  Obj_: TDFVariant;
begin
  Obj_ := TDFVariant.Create(DataTypeToByte(rdtVariant));
  Obj_.Buffer := v;
  FDataList.Add(Obj_);
end;

procedure TDFE.WriteInt64(v: Int64);
var
  Obj_: TDFInt64;
begin
  Obj_ := TDFInt64.Create(DataTypeToByte(rdtInt64));
  Obj_.Buffer := v;
  FDataList.Add(Obj_);
end;

procedure TDFE.WriteUInt64(v: UInt64);
var
  Obj_: TDFUInt64;
begin
  Obj_ := TDFUInt64.Create(DataTypeToByte(rdtUInt64));
  Obj_.Buffer := v;
  FDataList.Add(Obj_);
end;

procedure TDFE.WriteStrings(v: TCoreClassStrings);
var
  d: TMS64;
begin
  d := TMS64.Create;
{$IFDEF FPC}
  v.SaveToStream(d);
{$ELSE}
  v.SaveToStream(d, TEncoding.UTF8);
{$ENDIF}
  d.Position := 0;
  WriteStream(d);
  DisposeObject(d);
end;

procedure TDFE.WriteListStrings(v: TListString);
var
  d: TMS64;
begin
  d := TMS64.Create;
  v.SaveToStream(d);
  d.Position := 0;
  WriteStream(d);
  DisposeObject(d);
end;

procedure TDFE.WritePascalStrings(v: TListPascalString);
var
  d: TMS64;
begin
  d := TMS64.Create;
  v.SaveToStream(d);
  d.Position := 0;
  WriteStream(d);
  DisposeObject(d);
end;

procedure TDFE.WriteDataFrame(v: TDFE);
var
  Obj_: TDFStream;
begin
  Obj_ := TDFStream.Create(DataTypeToByte(rdtStream));
  v.FastEncodeTo(Obj_.Buffer);
  FDataList.Add(Obj_);
end;

procedure TDFE.WriteDataFrameCompressed(v: TDFE);
var
  Obj_: TDFStream;
begin
  Obj_ := TDFStream.Create(DataTypeToByte(rdtStream));
  v.EncodeAsSelectCompressor(Obj_.Buffer, True);
  FDataList.Add(Obj_);
end;

procedure TDFE.WriteDataFrameZLib(v: TDFE);
var
  Obj_: TDFStream;
begin
  Obj_ := TDFStream.Create(DataTypeToByte(rdtStream));
  v.EncodeAsZLib(Obj_.Buffer, True);
  FDataList.Add(Obj_);
end;

procedure TDFE.WriteHashStringList(v: THashStringList);
var
  ms: TMS64;
  h: THashStringTextStream;
begin
  ms := TMS64.Create;
  h := THashStringTextStream.Create(v);
  h.SaveToStream(ms);
  DisposeObject(h);
  ms.Position := 0;
  WriteStream(ms);
  DisposeObject(ms);
end;

procedure TDFE.WriteVariantList(v: THashVariantList);
var
  ms: TMS64;
  h: THashVariantTextStream;
begin
  ms := TMS64.Create;
  h := THashVariantTextStream.Create(v);
  h.SaveToStream(ms);
  DisposeObject(h);
  ms.Position := 0;
  WriteStream(ms);
  DisposeObject(ms);
end;

procedure TDFE.WriteSectionText(v: TSectionTextData);
var
  ms: TMS64;
begin
  ms := TMS64.Create;
  v.SaveToStream(ms);
  ms.Position := 0;
  WriteStream(ms);
  DisposeObject(ms);
end;

procedure TDFE.WriteTextSection(v: TSectionTextData);
begin
  WriteSectionText(v);
end;

procedure TDFE.WriteJson(v: TZ_JsonObject);
begin
  WriteJson(v, False);
end;

procedure TDFE.WriteJson(v: TZ_JsonObject; Formated_: Boolean);
var
  ms: TMS64;
begin
  ms := TMS64.Create;
  v.SaveToStream(ms, Formated_);
  ms.Position := 0;
  WriteStream(ms);
  DisposeObject(ms);
end;

{$IFDEF DELPHI}


procedure TDFE.WriteJson(v: TJsonObject);
var
  ms: TMS64;
begin
  ms := TMS64.Create;
  v.SaveToStream(ms, True, TEncoding.UTF8, True);
  ms.Position := 0;
  WriteStream(ms);
  DisposeObject(ms);
end;
{$ENDIF DELPHI}


procedure TDFE.WriteFile(fn: SystemString);
var
  fs: TCoreClassFileStream;
begin
  if umlFileExists(fn) then
    begin
      fs := TCoreClassFileStream.Create(fn, fmOpenRead or fmShareDenyNone);
      fs.Position := 0;
      WriteStream(fs);
      DisposeObject(fs);
    end;
end;

procedure TDFE.WriteRect(v: TRect);
begin
  with WriteArrayInteger do
    begin
      Add(v.Left);
      Add(v.Top);
      Add(v.Right);
      Add(v.Bottom);
    end;
end;

procedure TDFE.WriteRectf(v: TRectf);
begin
  with WriteArraySingle do
    begin
      Add(v.Left);
      Add(v.Top);
      Add(v.Right);
      Add(v.Bottom);
    end;
end;

procedure TDFE.WritePoint(v: TPoint);
begin
  with WriteArrayInteger do
    begin
      Add(v.x);
      Add(v.y);
    end;
end;

procedure TDFE.WritePointf(v: TPointf);
begin
  with WriteArraySingle do
    begin
      Add(v.x);
      Add(v.y);
    end;
end;

procedure TDFE.WriteVector(v: TVector);
begin
  WriteArraySingle.WriteArray(v);
end;

procedure TDFE.WriteAffineVector(v: TAffineVector);
begin
  WriteArraySingle.WriteArray(v);
end;

procedure TDFE.WriteVec4(v: TVec4);
begin
  WriteArraySingle.WriteArray(v);
end;

procedure TDFE.WriteVec3(v: TVec3);
begin
  WriteArraySingle.WriteArray(v);
end;

procedure TDFE.WriteVector4(v: TVector4);
begin
  WriteArraySingle.WriteArray(v.buff);
end;

procedure TDFE.WriteVector3(v: TVector3);
begin
  WriteArraySingle.WriteArray(v.buff);
end;

procedure TDFE.WriteMat4(v: TMat4);
begin
  with WriteArraySingle do
    begin
      WriteArray(v[0]);
      WriteArray(v[1]);
      WriteArray(v[2]);
      WriteArray(v[3]);
    end;
end;

procedure TDFE.WriteMatrix4(v: TMatrix4);
begin
  WriteMat4(v.buff);
end;

procedure TDFE.Write2DPoint(v: T2DPoint);
begin
  with WriteArraySingle do
      WriteArray(v);
end;

procedure TDFE.WriteVec2(v: TVec2);
begin
  Write2DPoint(v);
end;

procedure TDFE.WriteRectV2(v: TRectV2);
begin
  with WriteArraySingle do
    begin
      WriteArray(v[0]);
      WriteArray(v[1]);
    end;
end;

procedure TDFE.WritePointer(v: Pointer);
begin
  WriteUInt64(UInt64(v));
end;

procedure TDFE.WritePointer(v: UInt64);
begin
  WriteUInt64(v);
end;

procedure TDFE.WriteNM(NM: TNumberModule);
var
  D_: TDFE;
begin
  D_ := TDFE.Create;
  D_.WriteString(NM.Name);
  D_.WriteVariant(NM.OriginValue);
  D_.WriteVariant(NM.CurrentValue);
  WriteDataFrame(D_);
  DisposeObject(D_);
end;

procedure TDFE.WriteNMPool(NMPool: TNumberModulePool);

var
  D_: TDFE;
{$IFDEF FPC}
  procedure fpc_progress_(const Name: PSystemString; NM: TNumberModule);
  var
    Tmp_: TDFE;
  begin
    Tmp_ := TDFE.Create;
    Tmp_.WriteString(NM.Name);
    Tmp_.WriteVariant(NM.OriginValue);
    Tmp_.WriteVariant(NM.CurrentValue);
    D_.WriteDataFrame(Tmp_);
    DisposeObject(Tmp_);
  end;
{$ENDIF FPC}


begin
  D_ := TDFE.Create;

{$IFDEF FPC}
  NMPool.List.ProgressP(@fpc_progress_);
{$ELSE FPC}
  NMPool.List.ProgressP(procedure(const Name: PSystemString; NM: TNumberModule)
    var
      Tmp_: TDFE;
    begin
      Tmp_ := TDFE.Create;
      Tmp_.WriteString(NM.Name);
      Tmp_.WriteVariant(NM.OriginValue);
      Tmp_.WriteVariant(NM.CurrentValue);
      D_.WriteDataFrame(Tmp_);
      DisposeObject(Tmp_);
    end);
{$ENDIF FPC}
  WriteDataFrame(D_);
  DisposeObject(D_);
end;

// append new stream and write
procedure TDFE.write(const Buf_; Count_: Int64);
var
  s: TMS64;
begin
  s := TMS64.Create;
  s.Write64(Buf_, Count_);
  WriteStream(s);
  DisposeObject(s);
end;

function TDFE.ReadString(index_: Integer): SystemString;
var
  Obj_: TDFBase;
  i: Integer;
begin
  Obj_ := Data[index_];
  if Obj_ is TDFString then
      Result := umlStringOf(TDFString(Obj_).Buffer).Text
  else if Obj_ is TDFInteger then
      Result := IntToStr(TDFInteger(Obj_).Buffer)
  else if Obj_ is TDFCardinal then
      Result := IntToStr(TDFCardinal(Obj_).Buffer)
  else if Obj_ is TDFWord then
      Result := IntToStr(TDFWord(Obj_).Buffer)
  else if Obj_ is TDFByte then
      Result := IntToStr(TDFByte(Obj_).Buffer)
  else if Obj_ is TDFSingle then
      Result := FloatToStr(TDFSingle(Obj_).Buffer)
  else if Obj_ is TDFDouble then
      Result := FloatToStr(TDFDouble(Obj_).Buffer)
  else if Obj_ is TDFArrayInteger then
    begin
      Result := '(';
      with TDFArrayInteger(Obj_) do
        begin
          for i := 0 to Count - 1 do
            if Result <> '(' then
                Result := Result + ',' + IntToStr(Buffer[i])
            else
                Result := Result + IntToStr(Buffer[i]);
        end;
      Result := Result + ')';
    end
  else if Obj_ is TDFArrayShortInt then
    begin
      Result := '(';
      with TDFArrayShortInt(Obj_) do
        begin
          for i := 0 to Count - 1 do
            if Result <> '(' then
                Result := Result + ',' + IntToStr(Buffer[i])
            else
                Result := Result + IntToStr(Buffer[i]);
        end;
      Result := Result + ')';
    end
  else if Obj_ is TDFArrayByte then
    begin
      Result := '(';
      with TDFArrayByte(Obj_) do
        begin
          for i := 0 to Count - 1 do
            if Result <> '(' then
                Result := Result + ',' + IntToStr(Buffer[i])
            else
                Result := Result + IntToStr(Buffer[i]);
        end;
      Result := Result + ')';
    end
  else if Obj_ is TDFArraySingle then
    begin
      Result := '(';
      with TDFArraySingle(Obj_) do
        begin
          for i := 0 to Count - 1 do
            if Result <> '(' then
                Result := Result + ',' + FloatToStr(Buffer[i])
            else
                Result := Result + FloatToStr(Buffer[i]);
        end;
      Result := Result + ')';
    end
  else if Obj_ is TDFArrayDouble then
    begin
      Result := '(';
      with TDFArrayDouble(Obj_) do
        begin
          for i := 0 to Count - 1 do
            if Result <> '(' then
                Result := Result + ',' + FloatToStr(Buffer[i])
            else
                Result := Result + FloatToStr(Buffer[i]);
        end;
      Result := Result + ')';
    end
  else if Obj_ is TDFArrayInt64 then
    begin
      Result := '(';
      with TDFArrayInt64(Obj_) do
        begin
          for i := 0 to Count - 1 do
            if Result <> '(' then
                Result := Result + ',' + IntToStr(Buffer[i])
            else
                Result := Result + IntToStr(Buffer[i]);
        end;
      Result := Result + ')';
    end
  else if Obj_ is TDFVariant then
      Result := umlVarToStr(TDFVariant(Obj_).Buffer)
  else if Obj_ is TDFInt64 then
      Result := IntToStr(TDFInt64(Obj_).Buffer)
  else if Obj_ is TDFUInt64 then
{$IFDEF FPC}
    Result := IntToStr(TDFUInt64(Obj_).Buffer)
{$ELSE}
    Result := UIntToStr(TDFUInt64(Obj_).Buffer)
{$ENDIF}
  else
      Result := '';
end;

function TDFE.ReadInteger(index_: Integer): Integer;
var
  Obj_: TDFBase;
begin
  Obj_ := Data[index_];
  if Obj_ is TDFInteger then
      Result := TDFInteger(Obj_).Buffer
  else if Obj_ is TDFString then
      Result := umlStrToInt(umlStringOf(TDFString(Obj_).Buffer), 0)
  else if Obj_ is TDFCardinal then
      Result := TDFCardinal(Obj_).Buffer
  else if Obj_ is TDFWord then
      Result := TDFWord(Obj_).Buffer
  else if Obj_ is TDFByte then
      Result := TDFByte(Obj_).Buffer
  else if Obj_ is TDFSingle then
      Result := Trunc(TDFSingle(Obj_).Buffer)
  else if Obj_ is TDFDouble then
      Result := Trunc(TDFDouble(Obj_).Buffer)
  else if Obj_ is TDFVariant then
      Result := (TDFVariant(Obj_).Buffer)
  else if Obj_ is TDFInt64 then
      Result := (TDFInt64(Obj_).Buffer)
  else if Obj_ is TDFUInt64 then
      Result := (TDFUInt64(Obj_).Buffer)
  else
      Result := 0;
end;

function TDFE.ReadCardinal(index_: Integer): Cardinal;
var
  Obj_: TDFBase;
begin
  Obj_ := Data[index_];
  if Obj_ is TDFCardinal then
      Result := TDFCardinal(Obj_).Buffer
  else if Obj_ is TDFString then
      Result := umlStrToInt(umlStringOf(TDFString(Obj_).Buffer), 0)
  else if Obj_ is TDFInteger then
      Result := TDFInteger(Obj_).Buffer
  else if Obj_ is TDFWord then
      Result := TDFWord(Obj_).Buffer
  else if Obj_ is TDFByte then
      Result := TDFByte(Obj_).Buffer
  else if Obj_ is TDFSingle then
      Result := Trunc(TDFSingle(Obj_).Buffer)
  else if Obj_ is TDFDouble then
      Result := Trunc(TDFDouble(Obj_).Buffer)
  else if Obj_ is TDFVariant then
      Result := (TDFVariant(Obj_).Buffer)
  else if Obj_ is TDFInt64 then
      Result := (TDFInt64(Obj_).Buffer)
  else if Obj_ is TDFUInt64 then
      Result := (TDFUInt64(Obj_).Buffer)
  else
      Result := 0;
end;

function TDFE.ReadWord(index_: Integer): Word;
var
  Obj_: TDFBase;
begin
  Obj_ := Data[index_];
  if Obj_ is TDFWord then
      Result := TDFWord(Obj_).Buffer
  else if Obj_ is TDFString then
      Result := umlStrToInt(umlStringOf(TDFString(Obj_).Buffer), 0)
  else if Obj_ is TDFInteger then
      Result := TDFInteger(Obj_).Buffer
  else if Obj_ is TDFCardinal then
      Result := TDFCardinal(Obj_).Buffer
  else if Obj_ is TDFByte then
      Result := TDFByte(Obj_).Buffer
  else if Obj_ is TDFSingle then
      Result := Trunc(TDFSingle(Obj_).Buffer)
  else if Obj_ is TDFDouble then
      Result := Trunc(TDFDouble(Obj_).Buffer)
  else if Obj_ is TDFVariant then
      Result := (TDFVariant(Obj_).Buffer)
  else if Obj_ is TDFInt64 then
      Result := (TDFInt64(Obj_).Buffer)
  else if Obj_ is TDFUInt64 then
      Result := (TDFUInt64(Obj_).Buffer)
  else
      Result := 0;
end;

function TDFE.ReadBool(index_: Integer): Boolean;
begin
  Result := ReadByte(index_) = 1;
end;

function TDFE.ReadBoolean(index_: Integer): Boolean;
begin
  Result := ReadBool(index_);
end;

function TDFE.ReadByte(index_: Integer): Byte;
var
  Obj_: TDFBase;
begin
  Obj_ := Data[index_];
  if Obj_ is TDFByte then
      Result := TDFByte(Obj_).Buffer
  else if Obj_ is TDFString then
      Result := umlStrToInt(umlStringOf(TDFString(Obj_).Buffer), 0)
  else if Obj_ is TDFInteger then
      Result := TDFInteger(Obj_).Buffer
  else if Obj_ is TDFCardinal then
      Result := TDFCardinal(Obj_).Buffer
  else if Obj_ is TDFWord then
      Result := TDFWord(Obj_).Buffer
  else if Obj_ is TDFSingle then
      Result := Trunc(TDFSingle(Obj_).Buffer)
  else if Obj_ is TDFDouble then
      Result := Trunc(TDFDouble(Obj_).Buffer)
  else if Obj_ is TDFVariant then
      Result := (TDFVariant(Obj_).Buffer)
  else if Obj_ is TDFInt64 then
      Result := (TDFInt64(Obj_).Buffer)
  else if Obj_ is TDFUInt64 then
      Result := (TDFUInt64(Obj_).Buffer)
  else
      Result := 0;
end;

function TDFE.ReadSingle(index_: Integer): Single;
var
  Obj_: TDFBase;
begin
  Obj_ := Data[index_];
  if Obj_ is TDFSingle then
      Result := TDFSingle(Obj_).Buffer
  else if Obj_ is TDFString then
      Result := umlStrToFloat(umlStringOf(TDFString(Obj_).Buffer), 0)
  else if Obj_ is TDFInteger then
      Result := TDFInteger(Obj_).Buffer
  else if Obj_ is TDFCardinal then
      Result := TDFCardinal(Obj_).Buffer
  else if Obj_ is TDFWord then
      Result := TDFWord(Obj_).Buffer
  else if Obj_ is TDFByte then
      Result := TDFByte(Obj_).Buffer
  else if Obj_ is TDFDouble then
      Result := TDFDouble(Obj_).Buffer
  else if Obj_ is TDFVariant then
      Result := (TDFVariant(Obj_).Buffer)
  else if Obj_ is TDFInt64 then
      Result := (TDFInt64(Obj_).Buffer)
  else if Obj_ is TDFUInt64 then
      Result := (TDFUInt64(Obj_).Buffer)
  else
      Result := 0;
end;

function TDFE.ReadDouble(index_: Integer): Double;
var
  Obj_: TDFBase;
begin
  Obj_ := Data[index_];
  if Obj_ is TDFDouble then
      Result := TDFDouble(Obj_).Buffer
  else if Obj_ is TDFString then
      Result := umlStrToFloat(umlStringOf(TDFString(Obj_).Buffer), 0)
  else if Obj_ is TDFInteger then
      Result := TDFInteger(Obj_).Buffer
  else if Obj_ is TDFCardinal then
      Result := TDFCardinal(Obj_).Buffer
  else if Obj_ is TDFWord then
      Result := TDFWord(Obj_).Buffer
  else if Obj_ is TDFByte then
      Result := TDFByte(Obj_).Buffer
  else if Obj_ is TDFSingle then
      Result := TDFSingle(Obj_).Buffer
  else if Obj_ is TDFVariant then
      Result := (TDFVariant(Obj_).Buffer)
  else if Obj_ is TDFInt64 then
      Result := (TDFInt64(Obj_).Buffer)
  else if Obj_ is TDFUInt64 then
      Result := (TDFUInt64(Obj_).Buffer)
  else
      Result := 0;
end;

function TDFE.ReadArrayInteger(index_: Integer): TDFArrayInteger;
var
  Obj_: TDFBase;
begin
  Obj_ := Data[index_];
  if Obj_ is TDFArrayInteger then
      Result := TDFArrayInteger(Obj_)
  else
      Result := nil;
end;

function TDFE.ReadArrayShortInt(index_: Integer): TDFArrayShortInt;
var
  Obj_: TDFBase;
begin
  Obj_ := Data[index_];
  if Obj_ is TDFArrayShortInt then
      Result := TDFArrayShortInt(Obj_)
  else
      Result := nil;
end;

function TDFE.ReadArrayByte(index_: Integer): TDFArrayByte;
var
  Obj_: TDFBase;
begin
  Obj_ := Data[index_];
  if Obj_ is TDFArrayByte then
      Result := TDFArrayByte(Obj_)
  else
      Result := nil;
end;

function TDFE.ReadMD5(index_: Integer): TMD5;
begin
  with ReadArrayByte(index_) do
      GetBuff(@Result[0]);
end;

function TDFE.ReadArraySingle(index_: Integer): TDFArraySingle;
var
  Obj_: TDFBase;
begin
  Obj_ := Data[index_];
  if Obj_ is TDFArraySingle then
      Result := TDFArraySingle(Obj_)
  else
      Result := nil;
end;

function TDFE.ReadArrayDouble(index_: Integer): TDFArrayDouble;
var
  Obj_: TDFBase;
begin
  Obj_ := Data[index_];
  if Obj_ is TDFArrayDouble then
      Result := TDFArrayDouble(Obj_)
  else
      Result := nil;
end;

function TDFE.ReadArrayInt64(index_: Integer): TDFArrayInt64;
var
  Obj_: TDFBase;
begin
  Obj_ := Data[index_];
  if Obj_ is TDFArrayInt64 then
      Result := TDFArrayInt64(Obj_)
  else
      Result := nil;
end;

procedure TDFE.ReadStream(index_: Integer; output: TCoreClassStream);
var
  Obj_: TDFBase;
  LNeedResetPos: Boolean;
begin
  Obj_ := Data[index_];
  LNeedResetPos := output.Size = 0;
  if Obj_ is TDFStream then
    begin
      with TDFStream(Obj_) do
        begin
          Buffer.Position := 0;
          output.CopyFrom(Buffer, Buffer.Size);
          Buffer.Position := 0;
        end;
    end
  else if output is TMS64 then
      Obj_.SaveToStream(TMS64(output))
  else
      RaiseInfo('no support');
  if LNeedResetPos then
      output.Position := 0;
end;

function TDFE.ReadVariant(index_: Integer): Variant;
var
  Obj_: TDFBase;
begin
  Obj_ := Data[index_];
  if Obj_ is TDFVariant then
      Result := TDFVariant(Obj_).Buffer
  else if Obj_ is TDFString then
      Result := TDFString(Obj_).Buffer
  else if Obj_ is TDFInteger then
      Result := TDFInteger(Obj_).Buffer
  else if Obj_ is TDFCardinal then
      Result := TDFCardinal(Obj_).Buffer
  else if Obj_ is TDFWord then
      Result := TDFWord(Obj_).Buffer
  else if Obj_ is TDFByte then
      Result := TDFByte(Obj_).Buffer
  else if Obj_ is TDFSingle then
      Result := TDFSingle(Obj_).Buffer
  else if Obj_ is TDFDouble then
      Result := TDFDouble(Obj_).Buffer
  else if Obj_ is TDFInt64 then
      Result := (TDFInt64(Obj_).Buffer)
  else
      Result := 0;
end;

function TDFE.ReadInt64(index_: Integer): Int64;
var
  Obj_: TDFBase;
begin
  Obj_ := Data[index_];
  if Obj_ is TDFInt64 then
      Result := TDFInt64(Obj_).Buffer
  else if Obj_ is TDFUInt64 then
      Result := TDFUInt64(Obj_).Buffer
  else if Obj_ is TDFInteger then
      Result := TDFInteger(Obj_).Buffer
  else if Obj_ is TDFCardinal then
      Result := TDFCardinal(Obj_).Buffer
  else if Obj_ is TDFWord then
      Result := TDFWord(Obj_).Buffer
  else if Obj_ is TDFByte then
      Result := TDFByte(Obj_).Buffer
  else if Obj_ is TDFSingle then
      Result := Trunc(TDFSingle(Obj_).Buffer)
  else if Obj_ is TDFDouble then
      Result := Trunc(TDFDouble(Obj_).Buffer)
  else if Obj_ is TDFVariant then
      Result := TDFVariant(Obj_).Buffer
  else
      Result := 0;
end;

function TDFE.ReadUInt64(index_: Integer): UInt64;
var
  Obj_: TDFBase;
begin
  Obj_ := Data[index_];
  if Obj_ is TDFUInt64 then
      Result := TDFUInt64(Obj_).Buffer
  else if Obj_ is TDFInt64 then
      Result := TDFInt64(Obj_).Buffer
  else if Obj_ is TDFInteger then
      Result := TDFInteger(Obj_).Buffer
  else if Obj_ is TDFCardinal then
      Result := TDFCardinal(Obj_).Buffer
  else if Obj_ is TDFWord then
      Result := TDFWord(Obj_).Buffer
  else if Obj_ is TDFByte then
      Result := TDFByte(Obj_).Buffer
  else if Obj_ is TDFSingle then
      Result := Trunc(TDFSingle(Obj_).Buffer)
  else if Obj_ is TDFDouble then
      Result := Trunc(TDFDouble(Obj_).Buffer)
  else if Obj_ is TDFVariant then
      Result := TDFVariant(Obj_).Buffer
  else
      Result := 0;
end;

procedure TDFE.ReadStrings(index_: Integer; output: TCoreClassStrings);
var
  d: TMS64;
begin
  d := TMS64.Create;
  ReadStream(index_, d);
  d.Position := 0;

{$IFDEF FPC}
  output.LoadFromStream(d);
{$ELSE}
  output.LoadFromStream(d, TEncoding.UTF8);
{$ENDIF}
  DisposeObject(d);
end;

procedure TDFE.ReadListStrings(index_: Integer; output: TListString);
var
  d: TMS64;
begin
  d := TMS64.Create;
  ReadStream(index_, d);
  d.Position := 0;

  output.LoadFromStream(d);
  DisposeObject(d);
end;

procedure TDFE.ReadPascalStrings(index_: Integer; output: TListPascalString);
var
  d: TMS64;
begin
  d := TMS64.Create;
  ReadStream(index_, d);
  d.Position := 0;

  output.LoadFromStream(d);
  DisposeObject(d);
end;

procedure TDFE.ReadDataFrame(index_: Integer; output: TDFE);
var
  Obj_: TDFBase;
  d: TMS64;
begin
  Obj_ := Data[index_];
  if Obj_ is TDFStream then
    begin
      TDFStream(Obj_).Buffer.Position := 0;
      output.DecodeFrom(TDFStream(Obj_).Buffer, True);
      TDFStream(Obj_).Buffer.Position := 0;
    end
  else
    begin
      d := TMS64.Create;
      ReadStream(index_, d);
      d.Position := 0;
      output.DecodeFrom(d, True);
      DisposeObject(d);
    end;
end;

procedure TDFE.ReadHashStringList(index_: Integer; output: THashStringList);
var
  d: TMS64;
  h: THashStringTextStream;
begin
  d := TMS64.Create;
  ReadStream(index_, d);
  d.Position := 0;
  h := THashStringTextStream.Create(output);
  h.LoadFromStream(d);
  DisposeObject(h);
  DisposeObject(d);
end;

procedure TDFE.ReadVariantList(index_: Integer; output: THashVariantList);
var
  d: TMS64;
  h: THashVariantTextStream;
begin
  d := TMS64.Create;
  ReadStream(index_, d);
  d.Position := 0;
  h := THashVariantTextStream.Create(output);
  h.LoadFromStream(d);
  DisposeObject(h);
  DisposeObject(d);
end;

procedure TDFE.ReadSectionText(index_: Integer; output: TSectionTextData);
var
  d: TMS64;
begin
  d := TMS64.Create;
  ReadStream(index_, d);
  d.Position := 0;
  output.LoadFromStream(d);
  DisposeObject(d);
end;

procedure TDFE.ReadTextSection(index_: Integer; output: TSectionTextData);
begin
  ReadSectionText(index_, output);
end;

procedure TDFE.ReadJson(index_: Integer; output: TZ_JsonObject);
var
  d: TMS64;
begin
  d := TMS64.Create;
  ReadStream(index_, d);
  d.Position := 0;
  output.Clear;
  output.LoadFromStream(d);
  DisposeObject(d);
end;

{$IFDEF DELPHI}


procedure TDFE.ReadJson(index_: Integer; output: TJsonObject);
var
  d: TMS64;
begin
  d := TMS64.Create;
  ReadStream(index_, d);
  d.Position := 0;
  output.Clear;
  output.LoadFromStream(d, TEncoding.UTF8, True);
  DisposeObject(d);
end;
{$ENDIF DELPHI}


function TDFE.ReadRect(index_: Integer): TRect;
begin
  with ReadArrayInteger(index_) do
    begin
      Result := Rect(Buffer[0], Buffer[1], Buffer[2], Buffer[3]);
    end;
end;

function TDFE.ReadRectf(index_: Integer): TRectf;
begin
  with ReadArraySingle(index_) do
    begin
      Result := Rectf(Buffer[0], Buffer[1], Buffer[2], Buffer[3]);
    end;
end;

function TDFE.ReadPoint(index_: Integer): TPoint;
begin
  with ReadArrayInteger(index_) do
    begin
      Result := Point(Buffer[0], Buffer[1]);
    end;
end;

function TDFE.ReadPointf(index_: Integer): TPointf;
begin
  with ReadArraySingle(index_) do
    begin
      Result := Pointf(Buffer[0], Buffer[1]);
    end;
end;

function TDFE.ReadVector(index_: Integer): TVector;
begin
  with ReadArraySingle(index_) do
    begin
      Result[0] := Buffer[0];
      Result[1] := Buffer[1];
      Result[2] := Buffer[2];
      Result[3] := Buffer[3];
    end;
end;

function TDFE.ReadAffineVector(index_: Integer): TAffineVector;
begin
  with ReadArraySingle(index_) do
    begin
      Result[0] := Buffer[0];
      Result[1] := Buffer[1];
      Result[2] := Buffer[2];
    end;
end;

function TDFE.ReadVec3(index_: Integer): TVec3;
begin
  with ReadArraySingle(index_) do
    begin
      Result[0] := Buffer[0];
      Result[1] := Buffer[1];
      Result[2] := Buffer[2];
    end;
end;

function TDFE.ReadVec4(index_: Integer): TVec4;
begin
  with ReadArraySingle(index_) do
    begin
      Result[0] := Buffer[0];
      Result[1] := Buffer[1];
      Result[2] := Buffer[2];
      Result[3] := Buffer[3];
    end;
end;

function TDFE.ReadVector3(index_: Integer): TVector3;
begin
  with ReadArraySingle(index_) do
    begin
      Result := Vector3(Buffer[0], Buffer[1], Buffer[2]);
    end;
end;

function TDFE.ReadVector4(index_: Integer): TVector4;
begin
  with ReadArraySingle(index_) do
    begin
      Result := Vector4(Buffer[0], Buffer[1], Buffer[2], Buffer[3]);
    end;
end;

function TDFE.ReadMat4(index_: Integer): TMat4;
var
  i, j: Integer;
begin
  with ReadArraySingle(index_) do
    begin
      for i := 0 to 3 do
        for j := 0 to 3 do
            Result[i][j] := Buffer[i * 4 + j];
    end;
end;

function TDFE.ReadMatrix4(index_: Integer): TMatrix4;
begin
  Result.buff := ReadMat4(index_);
end;

function TDFE.Read2DPoint(index_: Integer): T2DPoint;
begin
  with ReadArraySingle(index_) do
    begin
      Result[0] := Buffer[0];
      Result[1] := Buffer[1];
    end;
end;

function TDFE.ReadVec2(index_: Integer): TVec2;
begin
  Result := Read2DPoint(index_);
end;

function TDFE.ReadRectV2(index_: Integer): TRectV2;
begin
  with ReadArraySingle(index_) do
    begin
      Result[0][0] := Buffer[0];
      Result[0][1] := Buffer[1];
      Result[1][0] := Buffer[2];
      Result[1][1] := Buffer[3];
    end;
end;

function TDFE.ReadPointer(index_: Integer): UInt64;
begin
  Result := ReadUInt64(index_);
end;

procedure TDFE.ReadNM(index_: Integer; output: TNumberModule);
var
  D_: TDFE;
begin
  D_ := TDFE.Create;
  ReadDataFrame(index_, D_);
  output.Name := D_.Reader.ReadString;
  output.DirectOriginValue := D_.Reader.ReadVariant;
  output.DirectCurrentValue := D_.Reader.ReadVariant;
  DisposeObject(D_);
end;

procedure TDFE.ReadNMPool(index_: Integer; output: TNumberModulePool);
var
  D_, Tmp_: TDFE;
  DM: TNumberModule;
  N_: SystemString;
  L_: TCoreClassListForObj;
  i: Integer;
begin
  L_ := TCoreClassListForObj.Create;
  D_ := TDFE.Create;
  ReadDataFrame(index_, D_);
  while D_.Reader.NotEnd do
    begin
      Tmp_ := TDFE.Create;
      D_.Reader.ReadDataFrame(Tmp_);
      N_ := Tmp_.Reader.ReadString;
      DM := output[N_];
      DM.Name := N_;
      DM.DirectOriginValue := Tmp_.Reader.ReadVariant;
      DM.DirectCurrentValue := Tmp_.Reader.ReadVariant;
      L_.Add(DM);
      DisposeObject(Tmp_);
    end;
  DisposeObject(D_);
  for i := 0 to L_.Count - 1 do
    begin
      DM := TNumberModule(L_[i]);
      DM.DoChange;
    end;
  DisposeObject(L_);
end;

procedure TDFE.Read(index_: Integer; var Buf_; Count_: Int64);
var
  s: TMS64;
begin
  s := TMS64.Create;
  ReadStream(index_, s);
  s.Read64(Buf_, Count_);
  DisposeObject(s);
end;

function TDFE.Read(index_: Integer): TDFBase;
begin
  Result := Data[index_];
end;

function TDFE.ComputeEncodeSize: Int64;
var
  i: Integer;
begin
  Result := C_Integer_Size;
  for i := 0 to Count - 1 do
      Result := Result + C_Byte_Size + GetData(i).ComputeEncodeSize;
end;

class procedure TDFE.BuildEmptyStream(output: TCoreClassStream);
type
  THead32_ = packed record
    editionToken: Byte;
    sizeInfo: Cardinal;
    compToken: Byte;
    md5: TMD5;
    num: Integer;
  end;
var
  head_: THead32_;
begin
  // make header
  head_.editionToken := $FF;
  head_.sizeInfo := C_Integer_Size;
  head_.compToken := 0;
  head_.md5 := NullMD5;
  head_.num := 0;
  output.write(head_, SizeOf(THead32_));
end;

function TDFE.FastEncode32To(output: TCoreClassStream; sizeInfo32: Cardinal): Integer;
type
  THead32_ = packed record
    editionToken: Byte;
    sizeInfo32: Cardinal;
    compToken: Byte;
    md5: TMD5;
  end;
var
  head_: THead32_;
  i: Integer;
  DataFrame_: TDFBase;
  ID: Byte;
begin
  Result := Count;

  if Result = 0 then
    begin
      BuildEmptyStream(output);
      exit;
    end;

  // make header
  head_.editionToken := $FF;
  head_.sizeInfo32 := sizeInfo32;
  head_.compToken := 0;
  head_.md5 := NullMD5;

  // write header
  output.write(head_, SizeOf(THead32_));

  // write body
  output.write(Result, C_Integer_Size);
  for i := 0 to Count - 1 do
    begin
      DataFrame_ := GetData(i);
      ID := DataFrame_.FID;
      output.write(DataFrame_.FID, C_Byte_Size);
      DataFrame_.SaveToStream(output);
    end;
end;

function TDFE.FastEncode64To(output: TCoreClassStream; sizeInfo64: Int64): Integer;
type
  THead64_ = packed record
    editionToken: Byte;
    sizeInfo64: Int64;
    compToken: Byte;
    md5: TMD5;
  end;
var
  head_: THead64_;
  i: Integer;
  DataFrame_: TDFBase;
  ID: Byte;
begin
  Result := Count;

  if Result = 0 then
    begin
      BuildEmptyStream(output);
      exit;
    end;

  // make header
  head_.editionToken := $FA;
  head_.sizeInfo64 := sizeInfo64;
  head_.compToken := 0;
  head_.md5 := NullMD5;

  // write header
  output.write(head_, SizeOf(THead64_));

  // write body
  output.write(Result, C_Integer_Size);
  for i := 0 to Count - 1 do
    begin
      DataFrame_ := GetData(i);
      ID := DataFrame_.FID;
      output.write(DataFrame_.FID, C_Byte_Size);
      DataFrame_.SaveToStream(output);
    end;
end;

function TDFE.FastEncodeTo(output: TCoreClassStream): Integer;
var
  sizeInfo64: Int64;
begin
  sizeInfo64 := ComputeEncodeSize;
  if sizeInfo64 > C_Max_UInt32 then
      Result := FastEncode64To(output, sizeInfo64)
  else
      Result := FastEncode32To(output, sizeInfo64);
end;

function TDFE.EncodeTo(output: TCoreClassStream; const FastMode, AutoCompressed: Boolean): Integer;
var
  i: Integer;
  DataFrame_: TDFBase;
  StoreStream, nStream: TMS64;
  ID: Byte;

  editionToken: Byte;
  sizeInfo32: Cardinal;
  sizeInfo64: Int64;
  compToken: Byte;
  md5: TMD5;
begin
  Result := Count;

  if Result = 0 then
    begin
      BuildEmptyStream(output);
      exit;
    end;

  // if encode size too large(>1M), we use EncodeAsSelectCompressor
  if (AutoCompressed) and (ComputeEncodeSize > 1024 * 1024) then
    begin
      Result := EncodeAsSelectCompressor(TSelectCompressionMethod.scmZLIB_Fast, output, FastMode);
      exit;
    end;

  if FastMode and (not AutoCompressed) then
    begin
      Result := FastEncodeTo(output);
      exit;
    end;

  StoreStream := TMS64.CustomCreate(8192);

  // make body
  StoreStream.Write64(Result, C_Integer_Size);

  nStream := TMS64.Create;
  for i := 0 to Count - 1 do
    begin
      DataFrame_ := GetData(i);
      ID := DataFrame_.FID;
      DataFrame_.SaveToStream(nStream);

      StoreStream.Write64(ID, C_Byte_Size);
      nStream.Position := 0;
      StoreStream.CopyFrom(nStream, nStream.Size);
      nStream.Clear;
    end;

  // make header
  sizeInfo32 := Cardinal(StoreStream.Size);
  sizeInfo64 := StoreStream.Size;
  if sizeInfo64 > C_Max_UInt32 then
      editionToken := $FA
  else
      editionToken := $FF;
  compToken := 0;
  StoreStream.Position := 0;
  if FastMode then
      md5 := NullMD5
  else
      md5 := umlMD5(StoreStream.Memory, StoreStream.Size);

  // prepare write header
  nStream.Clear;
  nStream.write(editionToken, C_Byte_Size);
  if sizeInfo64 > C_Max_UInt32 then
      nStream.write(sizeInfo64, C_Int64_Size)
  else
      nStream.write(sizeInfo32, C_Cardinal_Size);
  nStream.write(compToken, C_Byte_Size);
  nStream.write(md5[0], C_MD5_Size);

  // write header
  nStream.Position := 0;
  output.CopyFrom(nStream, nStream.Size);
  DisposeObject(nStream);

  // write body
  StoreStream.Position := 0;
  output.CopyFrom(StoreStream, StoreStream.Size);
  DisposeObject(StoreStream);
end;

function TDFE.EncodeTo(output: TCoreClassStream; const FastMode: Boolean): Integer;
begin
  Result := EncodeTo(output, FastMode, True);
end;

function TDFE.EncodeTo(output: TCoreClassStream): Integer;
begin
  Result := EncodeTo(output, False);
end;

procedure TDFE.Encrypt(output: TCoreClassStream; Compressed_: Boolean; SecurityLevel: Integer; Key: TCipherKeyBuffer);
var
  m64: TMS64;
begin
  m64 := TMS64.Create;
  if Compressed_ then
      EncodeAsSelectCompressor(m64, True)
  else
      EncodeTo(m64, True);

  QuantumEncrypt(m64, output, SecurityLevel, Key);
  DisposeObject(m64);
end;

function TDFE.Decrypt(input: TCoreClassStream; Key: TCipherKeyBuffer): Boolean;
var
  m64: TMS64;
begin
  m64 := TMS64.Create;
  Result := QuantumDecrypt(input, m64, Key);
  if Result then
    begin
      m64.Position := 0;
      DecodeFrom(m64, True);
    end;

  DisposeObject(m64);
end;

procedure TDFE.EncodeAsPublicJson(var output: TPascalString);
var
  m64: TMS64;
  buff: TBytes;
begin
  m64 := TMS64.Create;
  EncodeAsPublicJson(m64);
  SetLength(buff, m64.Size);
  CopyPtr(m64.Memory, @buff[0], m64.Size);
  DisposeObject(m64);
  output.Bytes := buff;
  SetLength(buff, 0);
end;

procedure TDFE.EncodeAsPublicJson(output: TCoreClassStream);
var
  j: TZ_JsonObject;
  i: Integer;
begin
  j := TZ_JsonObject.Create;
  j.s['help'] := 'This JSON with TDFE encode';

  for i := 0 to Count - 1 do
    begin
      j.a['Ref'].Add(TDFBase(FDataList[i]).FID);
      TDFBase(FDataList[i]).SaveToJson(j.a['Data'], i);
    end;

  j.SaveToStream(output, True);

  DisposeObject(j);
end;

procedure TDFE.EncodeAsJson(output: TCoreClassStream);
var
  j: TZ_JsonObject;
  i: Integer;
  DataFrame_: TDFBase;
begin
  j := TZ_JsonObject.Create;

  for i := 0 to Count - 1 do
    begin
      DataFrame_ := TDFBase(FDataList[i]);
      DataFrame_.SaveToJson(j.a['Data'], i);
      j.a['Ref'].Add(DataFrame_.FID);
    end;

  j.SaveToStream(output, False);

  DisposeObject(j);
end;

procedure TDFE.EncodeAsJson(json: TZ_JsonObject);
var
  i: Integer;
  DataFrame_: TDFBase;
begin
  json.Clear;
  for i := 0 to Count - 1 do
    begin
      DataFrame_ := TDFBase(FDataList[i]);
      DataFrame_.SaveToJson(json.a['Data'], i);
      json.a['Ref'].Add(DataFrame_.FID);
    end;
end;

procedure TDFE.DecodeFromJson(stream: TCoreClassStream);
var
  j: TZ_JsonObject;
  t: Byte;
  i: Integer;
  DataFrame_: TDFBase;
begin
  Clear;
  j := TZ_JsonObject.Create;
  try
      j.LoadFromStream(stream);
  except
    DisposeObject(j);
    exit;
  end;

  try
    for i := 0 to j.a['Ref'].Count - 1 do
      begin
        t := j.a['Ref'].i[i];
        DataFrame_ := AddData(ByteToDataType(t));
        DataFrame_.LoadFromJson(j.a['Data'], i);
      end;
  except
    DisposeObject(j);
    exit;
  end;

  DisposeObject(j);
end;

procedure TDFE.DecodeFromJson(const s: TPascalString);
var
  buff: TBytes;
  m64: TMS64;
begin
  buff := s.Bytes;
  m64 := TMS64.Create;
  m64.SetPointerWithProtectedMode(@buff[0], length(buff));
  m64.Position := 0;
  DecodeFromJson(m64);
  DisposeObject(m64);
  SetLength(buff, 0);
end;

procedure TDFE.DecodeFromJson(json: TZ_JsonObject);
var
  t: Byte;
  i: Integer;
  DataFrame_: TDFBase;
begin
  Clear;

  for i := 0 to json.a['Ref'].Count - 1 do
    begin
      t := json.a['Ref'].i[i];
      DataFrame_ := AddData(ByteToDataType(t));
      DataFrame_.LoadFromJson(json.a['Data'], i);
    end;
end;

function TDFE.EncodeAsSelectCompressor(scm: TSelectCompressionMethod; output: TCoreClassStream; const FastMode: Boolean): Integer;
var
  i: Integer;
  DataFrame_: TDFBase;
  StoreStream, nStream, compStream: TMS64;
  ID: Byte;

  editionToken: Byte;
  sizeInfo32: Cardinal;
  sizeInfo64: Int64;
  compToken: Byte;
  compsizeInfo32: Cardinal;
  compsizeInfo64: Int64;
  md5: TMD5;
begin
  Result := Count;

  if Result = 0 then
    begin
      BuildEmptyStream(output);
      exit;
    end;

  StoreStream := TMS64.CustomCreate(8192);

  // make body
  StoreStream.Write64(Result, C_Integer_Size);

  nStream := TMS64.Create;
  for i := 0 to Count - 1 do
    begin
      DataFrame_ := GetData(i);
      ID := DataFrame_.FID;
      DataFrame_.SaveToStream(nStream);

      StoreStream.Write64(ID, C_Byte_Size);
      nStream.Position := 0;
      StoreStream.CopyFrom(nStream, nStream.Size);
      nStream.Clear;
    end;

  // compress body and make header
  compsizeInfo32 := Cardinal(StoreStream.Size);
  compsizeInfo64 := StoreStream.Size;
  StoreStream.Position := 0;
  if FastMode then
      md5 := NullMD5
  else
      md5 := umlMD5(StoreStream.Memory, StoreStream.Size);

  compStream := TMS64.CustomCreate($FFFF);
  ParallelCompressMemory(scm, StoreStream, compStream);
  DisposeObject(StoreStream);

  // make header
  sizeInfo32 := Cardinal(compStream.Size);
  sizeInfo64 := compStream.Size;
  if sizeInfo64 > C_Max_UInt32 then
      editionToken := $FA
  else
      editionToken := $FF;
  if compsizeInfo64 > C_Max_UInt32 then
      compToken := 44
  else
      compToken := 4;

  // prepare write header
  nStream.Clear;
  nStream.write(editionToken, C_Byte_Size);
  if sizeInfo64 > C_Max_UInt32 then
      nStream.write(sizeInfo64, C_Int64_Size)
  else
      nStream.write(sizeInfo32, C_Cardinal_Size);
  nStream.write(compToken, C_Byte_Size);
  if compsizeInfo64 > C_Max_UInt32 then
      nStream.write(compsizeInfo64, C_Int64_Size)
  else
      nStream.write(compsizeInfo32, C_Cardinal_Size);
  nStream.write(md5[0], C_MD5_Size);

  // write header
  nStream.Position := 0;
  output.CopyFrom(nStream, nStream.Size);
  DisposeObject(nStream);

  // write body
  compStream.Position := 0;
  output.CopyFrom(compStream, compStream.Size);
  DisposeObject(compStream);
end;

function TDFE.EncodeAsSelectCompressor(output: TCoreClassStream; const FastMode: Boolean): Integer;
var
  scm: TSelectCompressionMethod;
begin
  if ComputeEncodeSize > 64 * 1024 then
    begin
      if FastMode then
          scm := TSelectCompressionMethod.scmZLIB_Fast
      else
          scm := TSelectCompressionMethod.scmZLIB_Max;
      Result := EncodeAsSelectCompressor(scm, output, FastMode);
    end
  else
      Result := EncodeAsZLib(output, FastMode);
end;

function TDFE.EncodeAsSelectCompressor(output: TCoreClassStream): Integer;
begin
  Result := EncodeAsSelectCompressor(output, False);
end;

function TDFE.EncodeAsZLib(output: TCoreClassStream; const FastMode: Boolean): Integer;
var
  i: Integer;
  DataFrame_: TDFBase;
  StoreStream, nStream, compStream: TMS64;
  ZCompStream: TCompressionStream;
  ID: Byte;

  editionToken: Byte;
  sizeInfo32: Cardinal;
  sizeInfo64: Int64;
  compToken: Byte;
  compsizeInfo32: Cardinal;
  compsizeInfo64: Int64;
  md5: TMD5;
begin
  Result := Count;

  if Result = 0 then
    begin
      BuildEmptyStream(output);
      exit;
    end;

  // if encode size too large(>1M), we use EncodeAsSelectCompressor
  if ComputeEncodeSize > 1024 * 1024 then
    begin
      Result := EncodeAsSelectCompressor(TSelectCompressionMethod.scmZLIB, output, FastMode);
      exit;
    end;

  StoreStream := TMS64.CustomCreate(8192);

  // make body
  StoreStream.Write64(Result, C_Integer_Size);

  nStream := TMS64.Create;
  for i := 0 to Count - 1 do
    begin
      DataFrame_ := GetData(i);
      ID := DataFrame_.FID;
      DataFrame_.SaveToStream(nStream);

      StoreStream.Write64(ID, C_Byte_Size);
      nStream.Position := 0;
      StoreStream.CopyFrom(nStream, nStream.Size);
      nStream.Clear;
    end;

  // compress body and make header
  compsizeInfo32 := Cardinal(StoreStream.Size);
  compsizeInfo64 := StoreStream.Size;
  StoreStream.Position := 0;
  if FastMode then
      md5 := NullMD5
  else
      md5 := umlMD5(StoreStream.Memory, StoreStream.Size);

  compStream := TMS64.CustomCreate($FFFF);
  ZCompStream := TCompressionStream.Create(compStream);
  StoreStream.Position := 0;
  ZCompStream.CopyFrom(StoreStream, StoreStream.Size);
  DisposeObject(ZCompStream);
  DisposeObject(StoreStream);

  // make header
  sizeInfo32 := compStream.Size;
  sizeInfo64 := compStream.Size;
  if sizeInfo64 > C_Max_UInt32 then
      editionToken := $FA
  else
      editionToken := $FF;
  if compsizeInfo64 > C_Max_UInt32 then
      compToken := 11
  else
      compToken := 1;

  // prepare write header
  nStream.Clear;
  nStream.write(editionToken, C_Byte_Size);
  if sizeInfo64 > C_Max_UInt32 then
      nStream.write(sizeInfo64, C_Int64_Size)
  else
      nStream.write(sizeInfo32, C_Cardinal_Size);
  nStream.write(compToken, C_Byte_Size);
  if compsizeInfo64 > C_Max_UInt32 then
      nStream.write(compsizeInfo64, C_Int64_Size)
  else
      nStream.write(compsizeInfo32, C_Cardinal_Size);
  nStream.write(md5[0], C_MD5_Size);

  // write header
  nStream.Position := 0;
  output.CopyFrom(nStream, nStream.Size);
  DisposeObject(nStream);

  // write body
  compStream.Position := 0;
  output.CopyFrom(compStream, compStream.Size);
  DisposeObject(compStream);
end;

function TDFE.EncodeAsZLib(output: TCoreClassStream): Integer;
begin
  Result := EncodeAsZLib(output, False);
end;

function TDFE.EncodeAsDeflate(output: TCoreClassStream; const FastMode: Boolean): Integer;
var
  i: Integer;
  DataFrame_: TDFBase;
  StoreStream, nStream, compStream: TMS64;
  ID: Byte;

  editionToken: Byte;
  sizeInfo32: Cardinal;
  sizeInfo64: Int64;
  compToken: Byte;
  compsizeInfo32: Cardinal;
  compsizeInfo64: Int64;
  md5: TMD5;
begin
  Result := Count;

  if Result = 0 then
    begin
      BuildEmptyStream(output);
      exit;
    end;

  // if encode size too large(>1M), we use EncodeAsSelectCompressor
  if ComputeEncodeSize > 1024 * 1024 then
    begin
      Result := EncodeAsSelectCompressor(TSelectCompressionMethod.scmZLIB, output, FastMode);
      exit;
    end;

  StoreStream := TMS64.CustomCreate(8192);

  // make body
  StoreStream.Write64(Result, C_Integer_Size);

  nStream := TMS64.Create;
  for i := 0 to Count - 1 do
    begin
      DataFrame_ := GetData(i);
      ID := DataFrame_.FID;
      DataFrame_.SaveToStream(nStream);

      StoreStream.Write64(ID, C_Byte_Size);
      nStream.Position := 0;
      StoreStream.CopyFrom(nStream, nStream.Size);
      nStream.Clear;
    end;

  // compress body and make header
  compsizeInfo32 := Cardinal(StoreStream.Size);
  compsizeInfo64 := StoreStream.Size;
  StoreStream.Position := 0;
  if FastMode then
      md5 := NullMD5
  else
      md5 := umlMD5(StoreStream.Memory, StoreStream.Size);

  compStream := TMS64.Create;
  StoreStream.Position := 0;

  if FCompressorDeflate = nil then
      FCompressorDeflate := TCompressorDeflate.Create;

  CoreCompressStream(FCompressorDeflate, StoreStream, compStream);
  DisposeObject(StoreStream);

  // make header
  sizeInfo32 := Cardinal(compStream.Size);
  sizeInfo64 := compStream.Size;
  if sizeInfo64 > C_Max_UInt32 then
      editionToken := $FA
  else
      editionToken := $FF;
  if compsizeInfo64 > C_Max_UInt32 then
      compToken := 22
  else
      compToken := 2;

  // prepare write header
  nStream.Clear;
  nStream.write(editionToken, C_Byte_Size);
  if sizeInfo64 > C_Max_UInt32 then
      nStream.write(sizeInfo64, C_Int64_Size)
  else
      nStream.write(sizeInfo32, C_Cardinal_Size);
  nStream.write(compToken, C_Byte_Size);
  if compsizeInfo64 > C_Max_UInt32 then
      nStream.write(compsizeInfo64, C_Int64_Size)
  else
      nStream.write(compsizeInfo32, C_Cardinal_Size);
  nStream.write(md5[0], C_MD5_Size);

  // write header
  nStream.Position := 0;
  output.CopyFrom(nStream, nStream.Size);
  DisposeObject(nStream);

  // write body
  compStream.Position := 0;
  output.CopyFrom(compStream, compStream.Size);
  DisposeObject(compStream);
end;

function TDFE.EncodeAsDeflate(output: TCoreClassStream): Integer;
begin
  Result := EncodeAsDeflate(output, False);
end;

function TDFE.EncodeAsBRRC(output: TCoreClassStream; const FastMode: Boolean): Integer;
var
  i: Integer;
  DataFrame_: TDFBase;
  StoreStream, nStream, compStream: TMS64;
  ID: Byte;

  editionToken: Byte;
  sizeInfo32: Cardinal;
  sizeInfo64: Int64;
  compToken: Byte;
  compsizeInfo32: Cardinal;
  compsizeInfo64: Int64;
  md5: TMD5;
begin
  Result := Count;

  if Result = 0 then
    begin
      BuildEmptyStream(output);
      exit;
    end;

  // if encode size too large(>1M), we use EncodeAsSelectCompressor
  if ComputeEncodeSize > 1024 * 1024 then
    begin
      Result := EncodeAsSelectCompressor(TSelectCompressionMethod.scmZLIB, output, FastMode);
      exit;
    end;

  StoreStream := TMS64.CustomCreate(8192);

  // make body
  StoreStream.Write64(Result, C_Integer_Size);

  nStream := TMS64.Create;
  for i := 0 to Count - 1 do
    begin
      DataFrame_ := GetData(i);
      ID := DataFrame_.FID;
      DataFrame_.SaveToStream(nStream);

      StoreStream.Write64(ID, C_Byte_Size);
      nStream.Position := 0;
      StoreStream.CopyFrom(nStream, nStream.Size);
      nStream.Clear;
    end;

  // compress body and make header
  compsizeInfo32 := Cardinal(StoreStream.Size);
  compsizeInfo64 := StoreStream.Size;
  StoreStream.Position := 0;
  if FastMode then
      md5 := NullMD5
  else
      md5 := umlMD5(StoreStream.Memory, StoreStream.Size);

  compStream := TMS64.Create;
  StoreStream.Position := 0;

  if FCompressorBRRC = nil then
      FCompressorBRRC := TCompressorBRRC.Create;

  CoreCompressStream(FCompressorBRRC, StoreStream, compStream);
  DisposeObject(StoreStream);

  // make header
  sizeInfo32 := Cardinal(compStream.Size);
  sizeInfo64 := compStream.Size;
  if sizeInfo64 > C_Max_UInt32 then
      editionToken := $FA
  else
      editionToken := $FF;
  if compsizeInfo64 > C_Max_UInt32 then
      compToken := 33
  else
      compToken := 3;

  // prepare write header
  nStream.Clear;
  nStream.write(editionToken, C_Byte_Size);
  if sizeInfo64 > C_Max_UInt32 then
      nStream.write(sizeInfo64, C_Int64_Size)
  else
      nStream.write(sizeInfo32, C_Cardinal_Size);
  nStream.write(compToken, C_Byte_Size);
  if compsizeInfo64 > C_Max_UInt32 then
      nStream.write(compsizeInfo64, C_Int64_Size)
  else
      nStream.write(compsizeInfo32, C_Cardinal_Size);
  nStream.write(md5[0], C_MD5_Size);

  // write header
  nStream.Position := 0;
  output.CopyFrom(nStream, nStream.Size);
  DisposeObject(nStream);

  // write body
  compStream.Position := 0;
  output.CopyFrom(compStream, compStream.Size);
  DisposeObject(compStream);
end;

function TDFE.EncodeAsBRRC(output: TCoreClassStream): Integer;
begin
  Result := EncodeAsBRRC(output, False);
end;

function TDFE.IsCompressed(source: TCoreClassStream): Boolean;
var
  bakPos: Int64;

  editionToken: Byte;
  sizeInfo32: Cardinal;
  sizeInfo64, sizeInfo: Int64;
  compToken: Byte;
begin
  bakPos := source.Position;
  Result := False;

  source.Read(editionToken, C_Byte_Size);
  if (editionToken in [$FF, $FA]) then
    begin
      if editionToken = $FF then
        begin
          source.Read(sizeInfo32, C_Cardinal_Size);
          sizeInfo := sizeInfo32;
        end
      else
        begin
          source.Read(sizeInfo64, C_Int64_Size);
          sizeInfo := sizeInfo64;
        end;

      source.Read(compToken, C_Byte_Size);

      Result := compToken in [1, 11, 2, 22, 3, 33, 4, 44];
    end;

  source.Position := bakPos;
end;

function TDFE.DecodeFrom(source: TCoreClassStream; const FastMode: Boolean): Integer;
var
  i, num_: Integer;
  ID: Byte;
  StoreStream: TMS64;
  ZDecompStream: TDecompressionStream;
  DataFrame_: TDFBase;

  editionToken: Byte;
  sizeInfo32: Cardinal;
  sizeInfo64, sizeInfo: Int64;
  compToken: Byte;
  compsizeInfo32: Cardinal;
  compsizeInfo64, compsizeInfo: Int64;
  md5: TMD5;
begin
  Clear;

  Result := -1;

  StoreStream := TMS64.Create;

  source.Read(editionToken, C_Byte_Size);
  if (editionToken in [$FF, $FA]) then
    begin
      if editionToken = $FF then
        begin
          source.Read(sizeInfo32, C_Cardinal_Size);
          sizeInfo := sizeInfo32;
        end
      else
        begin
          source.Read(sizeInfo64, C_Int64_Size);
          sizeInfo := sizeInfo64;
        end;

      source.Read(compToken, C_Byte_Size);

      if compToken = 0 then
        begin
          source.Read(md5[0], 16);

          if source is TMS64 then
              StoreStream.SetPointerWithProtectedMode(TMS64(source).PositionAsPtr, sizeInfo)
          else
            begin
              if sizeInfo > 0 then
                  StoreStream.CopyFrom(source, sizeInfo);
            end;

          StoreStream.Position := 0;
          if (not FastMode) and (not umlIsNullMD5(md5)) then
            if not umlMD5Compare(umlMD5(StoreStream.Memory, StoreStream.Size), md5) then
              begin
                DoStatus('md5 error!');
                DisposeObject(StoreStream);
                exit;
              end;
        end
      else if compToken in [1, 11] then
        begin
          if compToken = 1 then
            begin
              source.Read(compsizeInfo32, C_Cardinal_Size);
              compsizeInfo := compsizeInfo32;
            end
          else
            begin
              source.Read(compsizeInfo64, C_Int64_Size);
              compsizeInfo := compsizeInfo64;
            end;

          source.Read(md5[0], 16);

          ZDecompStream := TDecompressionStream.Create(source);
          StoreStream.CopyFrom(ZDecompStream, compsizeInfo);
          DisposeObject(ZDecompStream);

          StoreStream.Position := 0;
          if (not FastMode) and (not umlIsNullMD5(md5)) then
            if not umlMD5Compare(umlMD5(StoreStream.Memory, StoreStream.Size), md5) then
              begin
                DoStatus('ZLIB md5 error!');
                DisposeObject(StoreStream);
                exit;
              end;
        end
      else if compToken in [2, 22] then
        begin
          if compToken = 2 then
            begin
              source.Read(compsizeInfo32, C_Cardinal_Size);
              compsizeInfo := compsizeInfo32;
            end
          else
            begin
              source.Read(compsizeInfo64, C_Int64_Size);
              compsizeInfo := compsizeInfo64;
            end;
          source.Read(md5[0], 16);

          if FCompressorDeflate = nil then
              FCompressorDeflate := TCompressorDeflate.Create;
          CoreDecompressStream(FCompressorDeflate, source, StoreStream);

          StoreStream.Position := 0;
          if (not FastMode) and (not umlIsNullMD5(md5)) then
            if not umlMD5Compare(umlMD5(StoreStream.Memory, StoreStream.Size), md5) then
              begin
                DoStatus('Deflate md5 error!');
                DisposeObject(StoreStream);
                exit;
              end;
        end
      else if compToken in [3, 33] then
        begin
          if compToken = 3 then
            begin
              source.Read(compsizeInfo32, C_Cardinal_Size);
              compsizeInfo := compsizeInfo32;
            end
          else
            begin
              source.Read(compsizeInfo64, C_Int64_Size);
              compsizeInfo := compsizeInfo64;
            end;
          source.Read(md5[0], 16);

          if FCompressorBRRC = nil then
              FCompressorBRRC := TCompressorBRRC.Create;
          CoreDecompressStream(FCompressorBRRC, source, StoreStream);

          StoreStream.Position := 0;
          if (not FastMode) and (not umlIsNullMD5(md5)) then
            if not umlMD5Compare(umlMD5(StoreStream.Memory, StoreStream.Size), md5) then
              begin
                DoStatus('BRRC md5 error!');
                DisposeObject(StoreStream);
                exit;
              end;
        end
      else if compToken in [4, 44] then
        begin
          if compToken = 4 then
            begin
              source.Read(compsizeInfo32, C_Cardinal_Size);
              compsizeInfo := compsizeInfo32;
            end
          else
            begin
              source.Read(compsizeInfo64, C_Int64_Size);
              compsizeInfo := compsizeInfo64;
            end;
          source.Read(md5[0], 16);

          ParallelDecompressStream(source, StoreStream);

          StoreStream.Position := 0;
          if (not FastMode) and (not umlIsNullMD5(md5)) then
            if not umlMD5Compare(umlMD5(StoreStream.Memory, StoreStream.Size), md5) then
              begin
                DoStatus('select compression md5 error!');
                DisposeObject(StoreStream);
                exit;
              end;
        end;

      StoreStream.Position := 0;

      StoreStream.Read64(num_, C_Integer_Size);
      for i := 0 to num_ - 1 do
        begin
          StoreStream.Read64(ID, C_Byte_Size);
          DataFrame_ := AddData(ByteToDataType(ID));
          DataFrame_.LoadFromStream(StoreStream);
        end;
      DisposeObject(StoreStream);
      Result := num_;
    end
  else
    begin
      DoStatus('dataframe decode error!');
      DisposeObject(StoreStream);
      exit;
    end;
end;

function TDFE.DecodeFrom(source: TCoreClassStream): Integer;
begin
  Result := DecodeFrom(source, False);
end;

procedure TDFE.EncodeToBytes(const Compressed, FastMode: Boolean; var output: TBytes);
var
  enStream: TMS64;
begin
  enStream := TMS64.Create;
  if Compressed then
      EncodeAsSelectCompressor(enStream, FastMode)
  else
      EncodeTo(enStream, FastMode);

  SetLength(output, enStream.Size);
  CopyPtr(enStream.Memory, @output[0], enStream.Size);
  DisposeObject(enStream);
end;

procedure TDFE.DecodeFromBytes(var buff: TBytes);
begin
  DecodeFromBytes(buff, False);
end;

procedure TDFE.DecodeFromBytes(var buff: TBytes; const FastMode: Boolean);
var
  enStream: TMS64;
begin
  enStream := TMS64.Create;
  enStream.SetPointerWithProtectedMode(@buff[0], length(buff));
  DecodeFrom(enStream, FastMode);
  DisposeObject(enStream);
end;

function TDFE.GetMD5(const FastMode: Boolean): TMD5;
var
  enStream: TMS64;
begin
  enStream := TMS64.Create;
  EncodeTo(enStream, FastMode);

  Result := umlMD5(enStream.Memory, enStream.Size);
  DisposeObject(enStream);
end;

function TDFE.Compare(source: TDFE): Boolean;
var
  i: Integer;
  s1, s2: TMS64;
begin
  Result := False;

  if Count <> source.Count then
      exit;

  s1 := TMS64.CustomCreate(8192);
  s2 := TMS64.CustomCreate(8192);
  try
    for i := 0 to Count - 1 do
      begin
        if FDataList[i].ClassType <> source[i].ClassType then
            exit;
        if TDFBase(FDataList[i]).FID <> TDFBase(source[i]).FID then
            exit;
        if TDFBase(FDataList[i]).ComputeEncodeSize <> TDFBase(source[i]).ComputeEncodeSize then
            exit;

        s1.Clear;
        s2.Clear;
        TDFBase(FDataList[i]).SaveToStream(s1);
        TDFBase(source[i]).SaveToStream(s2);
        if s1.Size <> s2.Size then
            exit;
        if not CompareMemory(s1.Memory, s2.Memory, s1.Size) then
            exit;
        s1.Clear;
        s2.Clear;
      end;
    Result := True;
  finally
    DisposeObject(s1);
    DisposeObject(s2);
  end;
end;

procedure TDFE.LoadFromStream(stream: TCoreClassStream);
begin
  try
      DecodeFrom(stream);
  except
  end;
end;

procedure TDFE.SaveToStream(stream: TCoreClassStream);
var
  siz: Integer;
begin
  try
    siz := ComputeEncodeSize;
    if siz > 1024 then
        EncodeAsSelectCompressor(stream)
    else
        EncodeTo(stream);
  except
  end;
end;

procedure TDFE.LoadFromFile(fileName_: U_String);
var
  fs: TCoreClassFileStream;
begin
  fs := TCoreClassFileStream.Create(fileName_, fmOpenRead or fmShareDenyNone);
  LoadFromStream(fs);
  DisposeObject(fs);
end;

procedure TDFE.SaveToFile(fileName_: U_String);
var
  fs: TCoreClassFileStream;
begin
  fs := TCoreClassFileStream.Create(fileName_, fmCreate);
  SaveToStream(fs);
  DisposeObject(fs);
end;

constructor TDataWriter.Create(Stream_: TCoreClassStream);
begin
  inherited Create;
  FEngine := TDFE.Create;
  FStream := Stream_;
end;

destructor TDataWriter.Destroy;
var
  FlagCompressed: Boolean;
  verflag: TBytes;
  siz: Int64;
  M: TMS64;
begin
  if FStream <> nil then
    begin
      M := TMS64.Create;
      FEngine.FastEncodeTo(M);
      siz := M.Size;

      // write version flag
      verflag := umlBytesOf('0001');
      FStream.write(verflag, 4);

      // write compressed flag
      FlagCompressed := False;
      FStream.write(FlagCompressed, C_Boolean_Size);

      // write siz info
      FStream.write(siz, C_Int64_Size);

      // write buffer
      M.Position := 0;
      FStream.CopyFrom(M, siz);
      DisposeObject(M);
    end;

  DisposeObject(FEngine);
  inherited Destroy;
end;

procedure TDataWriter.Clear;
begin
  FEngine.Clear;
end;

procedure TDataWriter.WriteString(v: SystemString);
begin
  FEngine.WriteString(v);
end;

procedure TDataWriter.WriteInteger(v: Integer);
begin
  FEngine.WriteInteger(v);
end;

procedure TDataWriter.WriteCardinal(v: Cardinal);
begin
  FEngine.WriteCardinal(v);
end;

procedure TDataWriter.WriteWORD(v: Word);
begin
  FEngine.WriteWORD(v);
end;

procedure TDataWriter.WriteBool(v: Boolean);
begin
  FEngine.WriteBool(v);
end;

procedure TDataWriter.WriteBoolean(v: Boolean);
begin
  FEngine.WriteBoolean(v);
end;

procedure TDataWriter.WriteByte(v: Byte);
begin
  FEngine.WriteByte(v);
end;

procedure TDataWriter.WriteSingle(v: Single);
begin
  FEngine.WriteSingle(v);
end;

procedure TDataWriter.WriteDouble(v: Double);
begin
  FEngine.WriteDouble(v);
end;

procedure TDataWriter.WriteArrayInteger(v: array of Integer);
begin
  FEngine.WriteArrayInteger.WriteArray(v);
end;

procedure TDataWriter.WriteArrayShortInt(v: array of ShortInt);
begin
  FEngine.WriteArrayShortInt.WriteArray(v);
end;

procedure TDataWriter.WriteArrayByte(v: array of Byte);
begin
  FEngine.WriteArrayByte.WriteArray(v);
end;

procedure TDataWriter.WriteArraySingle(v: array of Single);
begin
  FEngine.WriteArraySingle.WriteArray(v);
end;

procedure TDataWriter.WriteArrayDouble(v: array of Double);
begin
  FEngine.WriteArrayDouble.WriteArray(v);
end;

procedure TDataWriter.WriteArrayInt64(v: array of Int64);
begin
  FEngine.WriteArrayInt64.WriteArray(v);
end;

procedure TDataWriter.WriteStream(v: TCoreClassStream);
begin
  FEngine.WriteStream(v);
end;

procedure TDataWriter.WriteVariant(v: Variant);
begin
  FEngine.WriteVariant(v);
end;

procedure TDataWriter.WriteInt64(v: Int64);
begin
  FEngine.WriteInt64(v);
end;

procedure TDataWriter.WriteUInt64(v: UInt64);
begin
  FEngine.WriteUInt64(v);
end;

procedure TDataWriter.WriteStrings(v: TCoreClassStrings);
begin
  FEngine.WriteStrings(v);
end;

procedure TDataWriter.WriteListStrings(v: TListString);
begin
  FEngine.WriteListStrings(v);
end;

procedure TDataWriter.WritePascalStrings(v: TListPascalString);
begin
  FEngine.WritePascalStrings(v);
end;

procedure TDataWriter.WriteDataFrame(v: TDFE);
begin
  FEngine.WriteDataFrame(v);
end;

procedure TDataWriter.WriteDataFrameCompressed(v: TDFE);
begin
  FEngine.WriteDataFrameCompressed(v);
end;

procedure TDataWriter.WriteHashStringList(v: THashStringList);
begin
  FEngine.WriteHashStringList(v);
end;

procedure TDataWriter.WriteVariantList(v: THashVariantList);
begin
  FEngine.WriteVariantList(v);
end;

procedure TDataWriter.WriteSectionText(v: TSectionTextData);
begin
  FEngine.WriteSectionText(v);
end;

procedure TDataWriter.WriteJson(v: TZ_JsonObject);
begin
  FEngine.WriteJson(v);
end;

{$IFDEF DELPHI}


procedure TDataWriter.WriteJson(v: TJsonObject);
begin
  FEngine.WriteJson(v);
end;
{$ENDIF DELPHI}


procedure TDataWriter.WriteRect(v: TRect);
begin
  FEngine.WriteRect(v);
end;

procedure TDataWriter.WriteRectf(v: TRectf);
begin
  FEngine.WriteRectf(v);
end;

procedure TDataWriter.WritePoint(v: TPoint);
begin
  FEngine.WritePoint(v);
end;

procedure TDataWriter.WritePointf(v: TPointf);
begin
  FEngine.WritePointf(v);
end;

procedure TDataWriter.WriteVector(v: TVector);
begin
  FEngine.WriteVector(v);
end;

procedure TDataWriter.WriteAffineVector(v: TAffineVector);
begin
  FEngine.WriteAffineVector(v);
end;

procedure TDataWriter.WriteVec4(v: TVec4);
begin
  FEngine.WriteVec4(v);
end;

procedure TDataWriter.WriteVec3(v: TVec3);
begin
  FEngine.WriteVec3(v);
end;

procedure TDataWriter.WriteVector4(v: TVector4);
begin
  FEngine.WriteVector4(v);
end;

procedure TDataWriter.WriteVector3(v: TVector3);
begin
  FEngine.WriteVector3(v);
end;

procedure TDataWriter.WriteMat4(v: TMat4);
begin
  FEngine.WriteMat4(v);
end;

procedure TDataWriter.WriteMatrix4(v: TMatrix4);
begin
  FEngine.WriteMatrix4(v);
end;

procedure TDataWriter.Write2DPoint(v: T2DPoint);
begin
  FEngine.Write2DPoint(v);
end;

procedure TDataWriter.WriteVec2(v: TVec2);
begin
  FEngine.WriteVec2(v);
end;

procedure TDataWriter.WriteRectV2(v: TRectV2);
begin
  FEngine.WriteRectV2(v);
end;

procedure TDataWriter.WritePointer(v: Pointer);
begin
  FEngine.WritePointer(v);
end;

procedure TDataWriter.WriteNM(NM: TNumberModule);
begin
  FEngine.WriteNM(NM);
end;

procedure TDataWriter.WriteNMPool(NMPool: TNumberModulePool);
begin
  FEngine.WriteNMPool(NMPool);
end;

procedure TDataWriter.write(const Buf_; Count_: Int64);
begin
  FEngine.write(Buf_, Count_);
end;

constructor TDataReader.Create(Stream_: TCoreClassStream);
var
  verflag: TBytes;
  FlagCompressed: Boolean;
  siz: Int64;
  M: TMS64;
begin
  inherited Create;
  FEngine := TDFE.Create;
  if Stream_ <> nil then
    begin
      // read version flag
      SetLength(verflag, 4);
      Stream_.Read(verflag, 4);
      if umlStringOf(verflag) <> '0001' then
          raise Exception.Create('Version flag Does not match!');

      // read compressed flag
      Stream_.Read(FlagCompressed, C_Boolean_Size);

      // read size info
      Stream_.Read(siz, C_Int64_Size);

      // read buffer
      M := TMS64.Create;
      M.CopyFrom(Stream_, siz);
      M.Position := 0;
      FEngine.DecodeFrom(M);
      DisposeObject(M);
    end;
end;

destructor TDataReader.Destroy;
begin
  DisposeObject(FEngine);
  inherited Destroy;
end;

function TDataReader.ReadString: SystemString;
begin
  Result := FEngine.Reader.ReadString;
end;

function TDataReader.ReadInteger: Integer;
begin
  Result := FEngine.Reader.ReadInteger;
end;

function TDataReader.ReadCardinal: Cardinal;
begin
  Result := FEngine.Reader.ReadCardinal;
end;

function TDataReader.ReadWord: Word;
begin
  Result := FEngine.Reader.ReadWord;
end;

function TDataReader.ReadBool: Boolean;
begin
  Result := FEngine.Reader.ReadBool;
end;

function TDataReader.ReadBoolean: Boolean;
begin
  Result := FEngine.Reader.ReadBoolean;
end;

function TDataReader.ReadByte: Byte;
begin
  Result := FEngine.Reader.ReadByte;
end;

function TDataReader.ReadSingle: Single;
begin
  Result := FEngine.Reader.ReadSingle;
end;

function TDataReader.ReadDouble: Double;
begin
  Result := FEngine.Reader.ReadDouble;
end;

procedure TDataReader.ReadArrayInteger(var Data: array of Integer);
var
  i: Integer;
  rb: TDFArrayInteger;
begin
  rb := FEngine.Reader.ReadArrayInteger;
  for i := low(Data) to high(Data) do
      Data[i] := rb[i];
end;

procedure TDataReader.ReadArrayShortInt(var Data: array of ShortInt);
var
  i: Integer;
  rb: TDFArrayShortInt;
begin
  rb := FEngine.Reader.ReadArrayShortInt;
  for i := low(Data) to high(Data) do
      Data[i] := rb[i];
end;

procedure TDataReader.ReadArrayByte(var Data: array of Byte);
var
  i: Integer;
  rb: TDFArrayByte;
begin
  rb := FEngine.Reader.ReadArrayByte;
  for i := low(Data) to high(Data) do
      Data[i] := rb[i];
end;

procedure TDataReader.ReadArraySingle(var Data: array of Single);
var
  i: Integer;
  rb: TDFArraySingle;
begin
  rb := FEngine.Reader.ReadArraySingle;
  for i := low(Data) to high(Data) do
      Data[i] := rb[i];
end;

procedure TDataReader.ReadArrayDouble(var Data: array of Double);
var
  i: Integer;
  rb: TDFArrayDouble;
begin
  rb := FEngine.Reader.ReadArrayDouble;
  for i := low(Data) to high(Data) do
      Data[i] := rb[i];
end;

procedure TDataReader.ReadArrayInt64(var Data: array of Int64);
var
  i: Integer;
  rb: TDFArrayInt64;
begin
  rb := FEngine.Reader.ReadArrayInt64;
  for i := low(Data) to high(Data) do
      Data[i] := rb[i];
end;

procedure TDataReader.ReadStream(output: TCoreClassStream);
begin
  FEngine.Reader.ReadStream(output);
end;

function TDataReader.ReadVariant: Variant;
begin
  Result := FEngine.Reader.ReadVariant;
end;

function TDataReader.ReadInt64: Int64;
begin
  Result := FEngine.Reader.ReadInt64;
end;

function TDataReader.ReadUInt64: UInt64;
begin
  Result := FEngine.Reader.ReadUInt64;
end;

procedure TDataReader.ReadStrings(output: TCoreClassStrings);
begin
  FEngine.Reader.ReadStrings(output);
end;

procedure TDataReader.ReadListStrings(output: TListString);
begin
  FEngine.Reader.ReadListStrings(output);
end;

procedure TDataReader.ReadPascalStrings(output: TListPascalString);
begin
  FEngine.Reader.ReadPascalStrings(output);
end;

procedure TDataReader.ReadDataFrame(output: TDFE);
begin
  FEngine.Reader.ReadDataFrame(output);
end;

procedure TDataReader.ReadHashStringList(output: THashStringList);
begin
  FEngine.Reader.ReadHashStringList(output);
end;

procedure TDataReader.ReadVariantList(output: THashVariantList);
begin
  FEngine.Reader.ReadVariantList(output);
end;

procedure TDataReader.ReadSectionText(output: TSectionTextData);
begin
  FEngine.Reader.ReadSectionText(output);
end;

procedure TDataReader.ReadJson(output: TZ_JsonObject);
begin
  FEngine.Reader.ReadJson(output);
end;

{$IFDEF DELPHI}


procedure TDataReader.ReadJson(output: TJsonObject);
begin
  FEngine.Reader.ReadJson(output);
end;
{$ENDIF DELPHI}


function TDataReader.ReadRect: TRect;
begin
  Result := FEngine.Reader.ReadRect;
end;

function TDataReader.ReadRectf: TRectf;
begin
  Result := FEngine.Reader.ReadRectf;
end;

function TDataReader.ReadPoint: TPoint;
begin
  Result := FEngine.Reader.ReadPoint;
end;

function TDataReader.ReadPointf: TPointf;
begin
  Result := FEngine.Reader.ReadPointf;
end;

function TDataReader.ReadVector: TVector;
begin
  Result := FEngine.Reader.ReadVector;
end;

function TDataReader.ReadAffineVector: TAffineVector;
begin
  Result := FEngine.Reader.ReadAffineVector;
end;

function TDataReader.ReadVec3: TVec3;
begin
  Result := FEngine.Reader.ReadVec3;
end;

function TDataReader.ReadVec4: TVec4;
begin
  Result := FEngine.Reader.ReadVec4;
end;

function TDataReader.ReadVector3: TVector3;
begin
  Result := FEngine.Reader.ReadVector3;
end;

function TDataReader.ReadVector4: TVector4;
begin
  Result := FEngine.Reader.ReadVector4;
end;

function TDataReader.ReadMat4: TMat4;
begin
  Result := FEngine.Reader.ReadMat4;
end;

function TDataReader.ReadMatrix4: TMatrix4;
begin
  Result := FEngine.Reader.ReadMatrix4;
end;

function TDataReader.Read2DPoint: T2DPoint;
begin
  Result := FEngine.Reader.Read2DPoint;
end;

function TDataReader.ReadVec2: TVec2;
begin
  Result := FEngine.Reader.ReadVec2;
end;

function TDataReader.ReadRectV2: TRectV2;
begin
  Result := FEngine.Reader.ReadRectV2;
end;

function TDataReader.ReadPointer: UInt64;
begin
  Result := FEngine.Reader.ReadPointer;
end;

procedure TDataReader.ReadNM(output: TNumberModule);
begin
  FEngine.Reader.ReadNM(output);
end;

procedure TDataReader.ReadNMPool(output: TNumberModulePool);
begin
  FEngine.Reader.ReadNMPool(output);
end;

procedure TDataReader.Read(var Buf_; Count_: Int64);
begin
  FEngine.Reader.Read(Buf_, Count_);
end;

end.
