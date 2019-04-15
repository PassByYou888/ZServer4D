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
(*
  update history
  2017-12-6
  added supported pointer
*)

unit DataFrameEngine;

{$INCLUDE zDefine.inc}

interface

uses SysUtils, CoreClasses, Types,
  ListEngine, MemoryStream64, CoreCipher,
  DoStatusIO, GeometryLib, TextDataEngine, Geometry2DUnit, Geometry3DUnit,
{$IFNDEF FPC} ZS_JsonDataObjects, {$ENDIF}
  CoreCompress, UnicodeMixedLib, PascalStrings;

type
  TDataFrameBase = class(TCoreClassObject)
  protected
    FID: Byte; // data frame id
  public
    constructor Create(ID: Byte);
    destructor Destroy; override;

    procedure LoadFromStream(stream: TMemoryStream64); virtual; abstract;
    procedure SaveToStream(stream: TMemoryStream64); virtual; abstract;
{$IFNDEF FPC}
    procedure LoadFromJson(jarry: TJsonArray; idx: Integer); virtual; abstract;
    procedure SaveToJson(jarry: TJsonArray; idx: Integer); virtual; abstract;
{$ENDIF}
    function ComputeEncodeSize: Integer; virtual; abstract;
  end;

  TDataFrameString = class sealed(TDataFrameBase)
  public
    Buffer: TBytes;

    constructor Create(ID: Byte);
    destructor Destroy; override;

    procedure LoadFromStream(stream: TMemoryStream64); override;
    procedure SaveToStream(stream: TMemoryStream64); override;
{$IFNDEF FPC}
    procedure LoadFromJson(jarry: TJsonArray; idx: Integer); override;
    procedure SaveToJson(jarry: TJsonArray; idx: Integer); override;
{$ENDIF}
    function ComputeEncodeSize: Integer; override;
  end;

  TDataFrameInteger = class sealed(TDataFrameBase)
  private
  protected
    FBuffer: Integer;
  public
    constructor Create(ID: Byte);
    destructor Destroy; override;

    procedure LoadFromStream(stream: TMemoryStream64); override;
    procedure SaveToStream(stream: TMemoryStream64); override;
{$IFNDEF FPC}
    procedure LoadFromJson(jarry: TJsonArray; idx: Integer); override;
    procedure SaveToJson(jarry: TJsonArray; idx: Integer); override;
{$ENDIF}
    function ComputeEncodeSize: Integer; override;

    property Buffer: Integer read FBuffer write FBuffer;
  end;

  TDataFrameCardinal = class sealed(TDataFrameBase)
  private
  protected
    FBuffer: Cardinal;
  public
    constructor Create(ID: Byte);
    destructor Destroy; override;

    procedure LoadFromStream(stream: TMemoryStream64); override;
    procedure SaveToStream(stream: TMemoryStream64); override;
{$IFNDEF FPC}
    procedure LoadFromJson(jarry: TJsonArray; idx: Integer); override;
    procedure SaveToJson(jarry: TJsonArray; idx: Integer); override;
{$ENDIF}
    function ComputeEncodeSize: Integer; override;

    property Buffer: Cardinal read FBuffer write FBuffer;
  end;

  TDataFrameWord = class sealed(TDataFrameBase)
  private
  protected
    FBuffer: Word;
  public
    constructor Create(ID: Byte);
    destructor Destroy; override;

    procedure LoadFromStream(stream: TMemoryStream64); override;
    procedure SaveToStream(stream: TMemoryStream64); override;
{$IFNDEF FPC}
    procedure LoadFromJson(jarry: TJsonArray; idx: Integer); override;
    procedure SaveToJson(jarry: TJsonArray; idx: Integer); override;
{$ENDIF}
    function ComputeEncodeSize: Integer; override;

    property Buffer: Word read FBuffer write FBuffer;
  end;

  TDataFrameByte = class sealed(TDataFrameBase)
  private
  protected
    FBuffer: Byte;
  public
    constructor Create(ID: Byte);
    destructor Destroy; override;

    procedure LoadFromStream(stream: TMemoryStream64); override;
    procedure SaveToStream(stream: TMemoryStream64); override;
{$IFNDEF FPC}
    procedure LoadFromJson(jarry: TJsonArray; idx: Integer); override;
    procedure SaveToJson(jarry: TJsonArray; idx: Integer); override;
{$ENDIF}
    function ComputeEncodeSize: Integer; override;

    property Buffer: Byte read FBuffer write FBuffer;
  end;

  TDataFrameSingle = class sealed(TDataFrameBase)
  private
  protected
    FBuffer: Single;
  public
    constructor Create(ID: Byte);
    destructor Destroy; override;

    procedure LoadFromStream(stream: TMemoryStream64); override;
    procedure SaveToStream(stream: TMemoryStream64); override;
{$IFNDEF FPC}
    procedure LoadFromJson(jarry: TJsonArray; idx: Integer); override;
    procedure SaveToJson(jarry: TJsonArray; idx: Integer); override;
{$ENDIF}
    function ComputeEncodeSize: Integer; override;

    property Buffer: Single read FBuffer write FBuffer;
  end;

  TDataFrameDouble = class sealed(TDataFrameBase)
  private
  protected
    FBuffer: Double;
  public
    constructor Create(ID: Byte);
    destructor Destroy; override;

    procedure LoadFromStream(stream: TMemoryStream64); override;
    procedure SaveToStream(stream: TMemoryStream64); override;
{$IFNDEF FPC}
    procedure LoadFromJson(jarry: TJsonArray; idx: Integer); override;
    procedure SaveToJson(jarry: TJsonArray; idx: Integer); override;
{$ENDIF}
    function ComputeEncodeSize: Integer; override;

    property Buffer: Double read FBuffer write FBuffer;
  end;

  TDataFrameArrayInteger = class sealed(TDataFrameBase)
  protected
    FBuffer: TCoreClassList;
  public
    constructor Create(ID: Byte);
    destructor Destroy; override;

    procedure Clear;
    procedure Add(v: Integer);
    procedure Delete(idx: Integer);
    function Count: Integer;
    procedure WriteArray(const a: array of Integer);

    procedure LoadFromStream(stream: TMemoryStream64); override;
    procedure SaveToStream(stream: TMemoryStream64); override;
{$IFNDEF FPC}
    procedure LoadFromJson(jarry: TJsonArray; idx: Integer); override;
    procedure SaveToJson(jarry: TJsonArray; idx: Integer); override;
{$ENDIF}
    function ComputeEncodeSize: Integer; override;

    function GetBuffer(idx: Integer): Integer;
    procedure SetBuffer(idx: Integer; Value: Integer);
    property Buffer[idx: Integer]: Integer read GetBuffer write SetBuffer; default;
  end;

  TDataFrameArrayShortInt = class sealed(TDataFrameBase)
  protected
    FBuffer: TCoreClassList;
  public
    constructor Create(ID: ShortInt);
    destructor Destroy; override;

    procedure Clear;
    procedure Add(v: ShortInt);
    procedure Delete(idx: Integer);
    function Count: Integer;
    procedure WriteArray(const a: array of ShortInt);

    procedure LoadFromStream(stream: TMemoryStream64); override;
    procedure SaveToStream(stream: TMemoryStream64); override;
{$IFNDEF FPC}
    procedure LoadFromJson(jarry: TJsonArray; idx: Integer); override;
    procedure SaveToJson(jarry: TJsonArray; idx: Integer); override;
{$ENDIF}
    function ComputeEncodeSize: Integer; override;

    function GetBuffer(idx: Integer): ShortInt;
    procedure SetBuffer(idx: Integer; Value: ShortInt);
    property Buffer[idx: Integer]: ShortInt read GetBuffer write SetBuffer; default;
  end;

  TDataFrameArrayByte = class sealed(TDataFrameBase)
  protected
    FBuffer: TCoreClassList;
  public
    constructor Create(ID: Byte);
    destructor Destroy; override;

    procedure Clear;
    procedure Add(v: Byte);
    procedure AddPtrBuff(p: PByte; Size: Integer);
    procedure AddI64(v: Int64);
    procedure AddU64(v: UInt64);
    procedure Addi(v: Integer);
    procedure AddWord(v: Word);
    procedure Delete(idx: Integer);
    function Count: Integer;
    procedure WriteArray(const a: array of Byte);
    procedure SetArray(const a: array of Byte);
    procedure SetBuff(p: PByte; Size: Integer);
    procedure GetBuff(p: PByte);

    procedure LoadFromStream(stream: TMemoryStream64); override;
    procedure SaveToStream(stream: TMemoryStream64); override;
{$IFNDEF FPC}
    procedure LoadFromJson(jarry: TJsonArray; idx: Integer); override;
    procedure SaveToJson(jarry: TJsonArray; idx: Integer); override;
{$ENDIF}
    function ComputeEncodeSize: Integer; override;

    function GetBuffer(idx: Integer): Byte;
    procedure SetBuffer(idx: Integer; Value: Byte);
    property Buffer[idx: Integer]: Byte read GetBuffer write SetBuffer; default;
  end;

  TDataFrameArraySingle = class sealed(TDataFrameBase)
  protected
    FBuffer: TCoreClassList;
  public
    constructor Create(ID: Byte);
    destructor Destroy; override;

    procedure Clear;
    procedure Add(v: Single);
    procedure Delete(idx: Integer);
    function Count: Integer;
    procedure WriteArray(const a: array of Single);

    procedure LoadFromStream(stream: TMemoryStream64); override;
    procedure SaveToStream(stream: TMemoryStream64); override;
{$IFNDEF FPC}
    procedure LoadFromJson(jarry: TJsonArray; idx: Integer); override;
    procedure SaveToJson(jarry: TJsonArray; idx: Integer); override;
{$ENDIF}
    function ComputeEncodeSize: Integer; override;

    function GetBuffer(idx: Integer): Single;
    procedure SetBuffer(idx: Integer; Value: Single);
    property Buffer[idx: Integer]: Single read GetBuffer write SetBuffer; default;
  end;

  TDataFrameArrayDouble = class sealed(TDataFrameBase)
  private
  protected
    FBuffer: TCoreClassList;
  public
    constructor Create(ID: Byte);
    destructor Destroy; override;

    procedure Clear;
    procedure Add(v: Double);
    procedure Delete(idx: Integer);
    function Count: Integer;
    procedure WriteArray(const a: array of Double);

    procedure LoadFromStream(stream: TMemoryStream64); override;
    procedure SaveToStream(stream: TMemoryStream64); override;
{$IFNDEF FPC}
    procedure LoadFromJson(jarry: TJsonArray; idx: Integer); override;
    procedure SaveToJson(jarry: TJsonArray; idx: Integer); override;
{$ENDIF}
    function ComputeEncodeSize: Integer; override;

    function GetBuffer(idx: Integer): Double;
    procedure SetBuffer(idx: Integer; Value: Double);
    property Buffer[idx: Integer]: Double read GetBuffer write SetBuffer; default;
  end;

  TDataFrameArrayInt64 = class sealed(TDataFrameBase)
  private
  protected
    FBuffer: TCoreClassList;
  public
    constructor Create(ID: Byte);
    destructor Destroy; override;

    procedure Clear;
    procedure Add(v: Int64);
    procedure Delete(idx: Integer);
    function Count: Integer;
    procedure WriteArray(const a: array of Int64);

    procedure LoadFromStream(stream: TMemoryStream64); override;
    procedure SaveToStream(stream: TMemoryStream64); override;
{$IFNDEF FPC}
    procedure LoadFromJson(jarry: TJsonArray; idx: Integer); override;
    procedure SaveToJson(jarry: TJsonArray; idx: Integer); override;
{$ENDIF}
    function ComputeEncodeSize: Integer; override;

    function GetBuffer(idx: Integer): Int64;
    procedure SetBuffer(idx: Integer; Value: Int64);
    property Buffer[idx: Integer]: Int64 read GetBuffer write SetBuffer; default;
  end;

  TDataFrameStream = class sealed(TDataFrameBase)
  private
  protected
    FBuffer: TMemoryStream64;
  public
    constructor Create(ID: Byte);
    destructor Destroy; override;

    procedure Clear;

    procedure LoadFromStream(stream: TMemoryStream64); override;
    procedure SaveToStream(stream: TMemoryStream64); override;
{$IFNDEF FPC}
    procedure LoadFromJson(jarry: TJsonArray; idx: Integer); override;
    procedure SaveToJson(jarry: TJsonArray; idx: Integer); override;
{$ENDIF}
    function ComputeEncodeSize: Integer; override;

    function GetBuffer: TCoreClassStream;
    procedure SetBuffer(_Buffer: TCoreClassStream);
    property Buffer: TCoreClassStream read GetBuffer write SetBuffer;
  end;

  TDataFrameVariant = class sealed(TDataFrameBase)
  private
  protected
    FBuffer: Variant;
  public
    constructor Create(ID: Byte);
    destructor Destroy; override;

    procedure LoadFromStream(stream: TMemoryStream64); override;
    procedure SaveToStream(stream: TMemoryStream64); override;
{$IFNDEF FPC}
    procedure LoadFromJson(jarry: TJsonArray; idx: Integer); override;
    procedure SaveToJson(jarry: TJsonArray; idx: Integer); override;
{$ENDIF}
    function ComputeEncodeSize: Integer; override;

    property Buffer: Variant read FBuffer write FBuffer;
  end;

  TDataFrameInt64 = class sealed(TDataFrameBase)
  private
  protected
    FBuffer: Int64;
  public
    constructor Create(ID: Byte);
    destructor Destroy; override;

    procedure LoadFromStream(stream: TMemoryStream64); override;
    procedure SaveToStream(stream: TMemoryStream64); override;
{$IFNDEF FPC}
    procedure LoadFromJson(jarry: TJsonArray; idx: Integer); override;
    procedure SaveToJson(jarry: TJsonArray; idx: Integer); override;
{$ENDIF}
    function ComputeEncodeSize: Integer; override;

    property Buffer: Int64 read FBuffer write FBuffer;
  end;

  TDataFrameUInt64 = class sealed(TDataFrameBase)
  private
  protected
    FBuffer: UInt64;
  public
    constructor Create(ID: Byte);
    destructor Destroy; override;

    procedure LoadFromStream(stream: TMemoryStream64); override;
    procedure SaveToStream(stream: TMemoryStream64); override;
{$IFNDEF FPC}
    procedure LoadFromJson(jarry: TJsonArray; idx: Integer); override;
    procedure SaveToJson(jarry: TJsonArray; idx: Integer); override;
{$ENDIF}
    function ComputeEncodeSize: Integer; override;

    property Buffer: UInt64 read FBuffer write FBuffer;
  end;

  TDataFrameEngine = class;

  TDataFrameEngineReader = class sealed(TCoreClassObject)
  private
    FOwner: TDataFrameEngine;
    FIndex: Integer;
  public
    constructor Create(AOwner: TDataFrameEngine);
    destructor Destroy; override;
    property index: Integer read FIndex write FIndex;
    //
    function IsEnd: Boolean;
    function NotEnd: Boolean;
    procedure GoNext;
    //
    function ReadString: SystemString;
    function ReadBytes: TBytes;
    function ReadInteger: Integer;
    function ReadCardinal: Cardinal;
    function ReadWord: Word;
    function ReadBool: Boolean;
    function ReadBoolean: Boolean;
    function ReadByte: Byte;
    function ReadSingle: Single;
    function ReadDouble: Double;
    function ReadArrayInteger: TDataFrameArrayInteger;
    function ReadArrayShortInt: TDataFrameArrayShortInt;
    function ReadArrayByte: TDataFrameArrayByte;
    function ReadMD5: UnicodeMixedLib.TMD5;
    function ReadArraySingle: TDataFrameArraySingle;
    function ReadArrayDouble: TDataFrameArrayDouble;
    function ReadArrayInt64: TDataFrameArrayInt64;
    procedure ReadStream(output: TCoreClassStream);
    function ReadVariant: Variant;
    function ReadInt64: Int64;
    function ReadUInt64: UInt64;
    procedure ReadStrings(output: TCoreClassStrings);
    procedure ReadListStrings(output: TListString);
    procedure ReadPascalStrings(output: TListPascalString);
    procedure ReadDataFrame(output: TDataFrameEngine);
    procedure ReadHashStringList(output: THashStringList);
    procedure ReadVariantList(output: THashVariantList);
    procedure ReadSectionText(output: TSectionTextData);
    procedure ReadTextSection(output: TSectionTextData);
{$IFNDEF FPC} procedure ReadJson(output: TJsonObject); {$ENDIF}
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
    // auto read from stream data
    procedure read(var aBuf; aCount: Int64); overload;
    // read as TDataFrameBase
    function read: TDataFrameBase; overload;
  end;

  TDataFrameEngine = class(TCoreClassObject)
  private type
    TRunTimeDataType = (rdtString, rdtInteger, rdtLongWord, rdtWORD, rdtByte, rdtSingle, rdtDouble,
      rdtArrayInteger, rdtArraySingle, rdtArrayDouble, rdtStream, rdtVariant, rdtInt64, rdtArrayShortInt, rdtCardinal, rdtUInt64, rdtArrayByte,
      rdtArrayInt64);
  private
    FDataList: TCoreClassListForObj;
    FReader: TDataFrameEngineReader;
    FCompressorDeflate: TCompressorDeflate;
    FCompressorBRRC: TCompressorBRRC;
  protected
    function DataTypeToByte(v: TRunTimeDataType): Byte;
    function ByteToDataType(v: Byte): TRunTimeDataType;
  public
    constructor Create;
    destructor Destroy; override;

    property Reader: TDataFrameEngineReader read FReader;

    procedure Clear;
    function AddData(v: TRunTimeDataType): TDataFrameBase;
    function GetData(idx: Integer): TDataFrameBase;
    function GetDataInfo(Obj_: TDataFrameBase): SystemString;
    function Count: Integer;
    function Delete(idx: Integer): Boolean;
    function DeleteFirst: Boolean;
    function DeleteLast: Boolean; overload;
    function DeleteLastCount(cnt: Integer): Boolean; overload;
    function DeleteCount(idx, _Count: Integer): Boolean;
    //
    procedure Assign(SameObj: TDataFrameEngine);
    function Clone: TDataFrameEngine;
    //
    procedure WriteString(v: SystemString); overload;
    procedure WriteString(v: TPascalString); overload;
    procedure WriteBytes(v: TBytes);
    procedure WriteInteger(v: Integer);
    procedure WriteCardinal(v: Cardinal);
    procedure WriteWORD(v: Word);
    procedure WriteBool(v: Boolean);
    procedure WriteBoolean(v: Boolean);
    procedure WriteByte(v: Byte);
    procedure WriteSingle(v: Single);
    procedure WriteDouble(v: Double);
    function WriteArrayInteger: TDataFrameArrayInteger;
    function WriteArrayShortInt: TDataFrameArrayShortInt;
    function WriteArrayByte: TDataFrameArrayByte;
    procedure WriteMD5(md5: UnicodeMixedLib.TMD5);
    function WriteArraySingle: TDataFrameArraySingle;
    function WriteArrayDouble: TDataFrameArrayDouble;
    function WriteArrayInt64: TDataFrameArrayInt64;
    procedure WriteStream(v: TCoreClassStream);
    procedure WriteVariant(v: Variant);
    procedure WriteInt64(v: Int64);
    procedure WriteUInt64(v: UInt64);
    procedure WriteStrings(v: TCoreClassStrings);
    procedure WriteListStrings(v: TListString);
    procedure WritePascalStrings(v: TListPascalString);
    procedure WriteDataFrame(v: TDataFrameEngine);
    procedure WriteDataFrameCompressed(v: TDataFrameEngine);
    procedure WriteHashStringList(v: THashStringList);
    procedure WriteVariantList(v: THashVariantList);
    procedure WriteSectionText(v: TSectionTextData);
    procedure WriteTextSection(v: TSectionTextData);
{$IFNDEF FPC} procedure WriteJson(v: TJsonObject); {$ENDIF}
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
    // auto append new stream and write
    procedure write(const aBuf; aCount: Int64);
    //
    function ReadString(idx: Integer): SystemString;
    function ReadBytes(idx: Integer): TBytes;
    function ReadInteger(idx: Integer): Integer;
    function ReadCardinal(idx: Integer): Cardinal;
    function ReadWord(idx: Integer): Word;
    function ReadBool(idx: Integer): Boolean;
    function ReadBoolean(idx: Integer): Boolean;
    function ReadByte(idx: Integer): Byte;
    function ReadSingle(idx: Integer): Single;
    function ReadDouble(idx: Integer): Double;
    function ReadArrayInteger(idx: Integer): TDataFrameArrayInteger;
    function ReadArrayShortInt(idx: Integer): TDataFrameArrayShortInt;
    function ReadArrayByte(idx: Integer): TDataFrameArrayByte;
    function ReadMD5(idx: Integer): UnicodeMixedLib.TMD5;
    function ReadArraySingle(idx: Integer): TDataFrameArraySingle;
    function ReadArrayDouble(idx: Integer): TDataFrameArrayDouble;
    function ReadArrayInt64(idx: Integer): TDataFrameArrayInt64;
    procedure ReadStream(idx: Integer; output: TCoreClassStream);
    function ReadVariant(idx: Integer): Variant;
    function ReadInt64(idx: Integer): Int64;
    function ReadUInt64(idx: Integer): UInt64;
    procedure ReadStrings(idx: Integer; output: TCoreClassStrings);
    procedure ReadListStrings(idx: Integer; output: TListString);
    procedure ReadPascalStrings(idx: Integer; output: TListPascalString);
    procedure ReadDataFrame(idx: Integer; output: TDataFrameEngine);
    procedure ReadHashStringList(idx: Integer; output: THashStringList);
    procedure ReadVariantList(idx: Integer; output: THashVariantList);
    procedure ReadSectionText(idx: Integer; output: TSectionTextData);
    procedure ReadTextSection(idx: Integer; output: TSectionTextData);
{$IFNDEF FPC} procedure ReadJson(idx: Integer; output: TJsonObject); {$ENDIF}
    function ReadRect(idx: Integer): TRect;
    function ReadRectf(idx: Integer): TRectf;
    function ReadPoint(idx: Integer): TPoint;
    function ReadPointf(idx: Integer): TPointf;
    function ReadVector(idx: Integer): TVector;
    function ReadAffineVector(idx: Integer): TAffineVector;
    function ReadVec3(idx: Integer): TVec3;
    function ReadVec4(idx: Integer): TVec4;
    function ReadVector3(idx: Integer): TVector3;
    function ReadVector4(idx: Integer): TVector4;
    function ReadMat4(idx: Integer): TMat4;
    function ReadMatrix4(idx: Integer): TMatrix4;
    function Read2DPoint(idx: Integer): T2DPoint;
    function ReadVec2(idx: Integer): TVec2;
    function ReadRectV2(idx: Integer): TRectV2;
    function ReadPointer(idx: Integer): UInt64;
    // read from stream data
    procedure read(idx: Integer; var aBuf; aCount: Int64);
    //
    function ComputeEncodeSize: Integer;

    class procedure BuildEmptyStream(output: TCoreClassStream);

    function EncodeTo(output: TCoreClassStream; const FastMode: Boolean): Integer; overload;
    function EncodeTo(output: TCoreClassStream): Integer; overload;

    // data security support
    // QuantumCryptographyPassword: used sha-3-512 cryptography as 512 bits password
    procedure Encrypt(output: TCoreClassStream; Compressed_: Boolean; SecurityLevel: Integer; Key: TCipherKeyBuffer);
    function Decrypt(input: TCoreClassStream; Key: TCipherKeyBuffer): Boolean;

    // json support
{$IFNDEF FPC}
    procedure EncodeAsPublicJson(output: TCoreClassStream);
    procedure EncodeAsJson(output: TCoreClassStream);
    procedure DecodeFromJson(stream: TCoreClassStream);
{$ENDIF}
    //
    // parallel compressor
    function EncodeAsSelectCompress(scm: TSelectCompressionMethod; output: TCoreClassStream; const FastMode: Boolean): Integer; overload;
    function EncodeAsSelectCompress(output: TCoreClassStream; const FastMode: Boolean): Integer; overload;
    function EncodeAsSelectCompress(output: TCoreClassStream): Integer; overload;

    // ZLib compressor
    function EncodeAsZLib(output: TCoreClassStream; const FastMode: Boolean): Integer; overload;
    function EncodeAsZLib(output: TCoreClassStream): Integer; overload;

    // Deflate compressor
    function EncodeAsDeflate(output: TCoreClassStream; const FastMode: Boolean): Integer; overload;
    function EncodeAsDeflate(output: TCoreClassStream): Integer; overload;

    // BRRC compressor
    function EncodeAsBRRC(output: TCoreClassStream; const FastMode: Boolean): Integer; overload;
    function EncodeAsBRRC(output: TCoreClassStream): Integer; overload;

    function IsCompressed(Source: TCoreClassStream): Boolean;
    function DecodeFrom(Source: TCoreClassStream; const FastMode: Boolean): Integer; overload;
    function DecodeFrom(Source: TCoreClassStream): Integer; overload;

    procedure EncodeToBytes(const Compressed, FastMode: Boolean; var output: TBytes);
    procedure DecodeFromBytes(var b: TBytes); overload;
    procedure DecodeFromBytes(var b: TBytes; const FastMode: Boolean); overload;

    function GetMD5(const FastMode: Boolean): UnicodeMixedLib.TMD5;
    function Compare(dest: TDataFrameEngine): Boolean;

    procedure LoadFromStream(stream: TCoreClassStream);
    procedure SaveToStream(stream: TCoreClassStream);

    property Data[idx: Integer]: TDataFrameBase read GetData; default;
    property List: TCoreClassListForObj read FDataList;
  end;

  TDataWriter = class sealed(TCoreClassPersistent)
  protected
    FEngine: TDataFrameEngine;
    FStream: TCoreClassStream;
  public
    constructor Create(AStream: TCoreClassStream);
    destructor Destroy; override;

    procedure Clear; { virtual; }

    procedure WriteString(v: SystemString);                  { virtual; }
    procedure WriteBytes(v: TBytes);                         { virtual; }
    procedure WriteInteger(v: Integer);                      { virtual; }
    procedure WriteCardinal(v: Cardinal);                    { virtual; }
    procedure WriteWORD(v: Word);                            { virtual; }
    procedure WriteBool(v: Boolean);                         { virtual; }
    procedure WriteBoolean(v: Boolean);                      { virtual; }
    procedure WriteByte(v: Byte);                            { virtual; }
    procedure WriteSingle(v: Single);                        { virtual; }
    procedure WriteDouble(v: Double);                        { virtual; }
    procedure WriteArrayInteger(v: array of Integer);        { virtual; }
    procedure WriteArrayShortInt(v: array of ShortInt);      { virtual; }
    procedure WriteArrayByte(v: array of Byte);              { virtual; }
    procedure WriteArraySingle(v: array of Single);          { virtual; }
    procedure WriteArrayDouble(v: array of Double);          { virtual; }
    procedure WriteArrayInt64(v: array of Int64);            { virtual; }
    procedure WriteStream(v: TCoreClassStream);              { virtual; }
    procedure WriteVariant(v: Variant);                      { virtual; }
    procedure WriteInt64(v: Int64);                          { virtual; }
    procedure WriteUInt64(v: UInt64);                        { virtual; }
    procedure WriteStrings(v: TCoreClassStrings);            { virtual; }
    procedure WriteListStrings(v: TListString);              { virtual; }
    procedure WritePascalStrings(v: TListPascalString);      { virtual; }
    procedure WriteDataFrame(v: TDataFrameEngine);           { virtual; }
    procedure WriteDataFrameCompressed(v: TDataFrameEngine); { virtual; }
    procedure WriteHashStringList(v: THashStringList);       { virtual; }
    procedure WriteVariantList(v: THashVariantList);         { virtual; }
    procedure WriteSectionText(v: TSectionTextData);         { virtual; }
{$IFNDEF FPC} procedure WriteJson(v: TJsonObject); { virtual; }    {$ENDIF}
    procedure WriteRect(v: TRect);                 { virtual; }
    procedure WriteRectf(v: TRectf);               { virtual; }
    procedure WritePoint(v: TPoint);               { virtual; }
    procedure WritePointf(v: TPointf);             { virtual; }
    procedure WriteVector(v: TVector);             { virtual; }
    procedure WriteAffineVector(v: TAffineVector); { virtual; }
    procedure WriteVec4(v: TVec4);                 { virtual; }
    procedure WriteVec3(v: TVec3);                 { virtual; }
    procedure WriteVector4(v: TVector4);           { virtual; }
    procedure WriteVector3(v: TVector3);           { virtual; }
    procedure WriteMat4(v: TMat4);                 { virtual; }
    procedure WriteMatrix4(v: TMatrix4);           { virtual; }
    procedure Write2DPoint(v: T2DPoint);           { virtual; }
    procedure WriteVec2(v: TVec2);                 { virtual; }
    procedure WriteRectV2(v: TRectV2);             { virtual; }
    procedure WritePointer(v: Pointer);            { virtual; }
    procedure write(const aBuf; aCount: Int64);    { virtual; }
  end;

  TDataReader = class sealed(TCoreClassPersistent)
  protected
    FEngine: TDataFrameEngine;
  public
    constructor Create(AStream: TCoreClassStream);
    destructor Destroy; override;

    function ReadString: SystemString;                        { virtual; }
    function ReadBytes: TBytes;                               { virtual; }
    function ReadInteger: Integer;                            { virtual; }
    function ReadCardinal: Cardinal;                          { virtual; }
    function ReadWord: Word;                                  { virtual; }
    function ReadBool: Boolean;                               { virtual; }
    function ReadBoolean: Boolean;                            { virtual; }
    function ReadByte: Byte;                                  { virtual; }
    function ReadSingle: Single;                              { virtual; }
    function ReadDouble: Double;                              { virtual; }
    procedure ReadArrayInteger(var Data: array of Integer);   { virtual; }
    procedure ReadArrayShortInt(var Data: array of ShortInt); { virtual; }
    procedure ReadArrayByte(var Data: array of Byte);         { virtual; }
    procedure ReadArraySingle(var Data: array of Single);     { virtual; }
    procedure ReadArrayDouble(var Data: array of Double);     { virtual; }
    procedure ReadArrayInt64(var Data: array of Int64);       { virtual; }
    procedure ReadStream(output: TCoreClassStream);           { virtual; }
    function ReadVariant: Variant;                            { virtual; }
    function ReadInt64: Int64;                                { virtual; }
    function ReadUInt64: UInt64;                              { virtual; }
    procedure ReadStrings(output: TCoreClassStrings);         { virtual; }
    procedure ReadListStrings(output: TListString);           { virtual; }
    procedure ReadPascalStrings(output: TListPascalString);   { virtual; }
    procedure ReadDataFrame(output: TDataFrameEngine);        { virtual; }
    procedure ReadHashStringList(output: THashStringList);    { virtual; }
    procedure ReadVariantList(output: THashVariantList);      { virtual; }
    procedure ReadSectionText(output: TSectionTextData);      { virtual; }
{$IFNDEF FPC} procedure ReadJson(output: TJsonObject); { virtual; }    {$ENDIF}
    function ReadRect: TRect;                 { virtual; }
    function ReadRectf: TRectf;               { virtual; }
    function ReadPoint: TPoint;               { virtual; }
    function ReadPointf: TPointf;             { virtual; }
    function ReadVector: TVector;             { virtual; }
    function ReadAffineVector: TAffineVector; { virtual; }
    function ReadVec3: TVec3;                 { virtual; }
    function ReadVec4: TVec4;                 { virtual; }
    function ReadVector3: TVector3;           { virtual; }
    function ReadVector4: TVector4;           { virtual; }
    function ReadMat4: TMat4;                 { virtual; }
    function ReadMatrix4: TMatrix4;           { virtual; }
    function Read2DPoint: T2DPoint;           { virtual; }
    function ReadVec2: TVec2;                 { virtual; }
    function ReadRectV2: TRectV2;             { virtual; }
    function ReadPointer: UInt64;             { virtual; }
    procedure read(var aBuf; aCount: Int64);  { virtual; }
  end;

implementation

constructor TDataFrameBase.Create(ID: Byte);
begin
  inherited Create;
  FID := ID;
end;

destructor TDataFrameBase.Destroy;
begin
  inherited Destroy;
end;

constructor TDataFrameString.Create(ID: Byte);
begin
  inherited Create(ID);
  SetLength(Buffer, 0);
end;

destructor TDataFrameString.Destroy;
begin
  SetLength(Buffer, 0);
  inherited Destroy;
end;

procedure TDataFrameString.LoadFromStream(stream: TMemoryStream64);
var
  _Len: Integer;
begin
  stream.Read64(_Len, C_Integer_Size);
  SetLength(Buffer, _Len);
  if (_Len > 0) then
      stream.Read64(Buffer[0], _Len);
end;

procedure TDataFrameString.SaveToStream(stream: TMemoryStream64);
var
  _Len: Integer;
begin
  _Len := length(Buffer);
  stream.Write64(_Len, C_Integer_Size);
  if _Len > 0 then
      stream.Write64(Buffer[0], _Len);
end;

{$IFNDEF FPC}


procedure TDataFrameString.LoadFromJson(jarry: TJsonArray; idx: Integer);
begin
  Buffer := umlBytesOf(jarry.s[idx]);
end;

procedure TDataFrameString.SaveToJson(jarry: TJsonArray; idx: Integer);
begin
  jarry.Add(umlStringOf(Buffer));
end;
{$ENDIF}


function TDataFrameString.ComputeEncodeSize: Integer;
begin
  Result := C_Integer_Size + length(Buffer);
end;

constructor TDataFrameInteger.Create(ID: Byte);
begin
  inherited Create(ID);
  Buffer := 0;
end;

destructor TDataFrameInteger.Destroy;
begin
  inherited Destroy;
end;

procedure TDataFrameInteger.LoadFromStream(stream: TMemoryStream64);
begin
  stream.Read64(FBuffer, C_Integer_Size);
end;

procedure TDataFrameInteger.SaveToStream(stream: TMemoryStream64);
begin
  stream.Write64(FBuffer, C_Integer_Size);
end;

{$IFNDEF FPC}


procedure TDataFrameInteger.LoadFromJson(jarry: TJsonArray; idx: Integer);
begin
  FBuffer := jarry.i[idx];
end;

procedure TDataFrameInteger.SaveToJson(jarry: TJsonArray; idx: Integer);
begin
  jarry.Add(FBuffer);
end;
{$ENDIF}


function TDataFrameInteger.ComputeEncodeSize: Integer;
begin
  Result := C_Integer_Size;
end;

constructor TDataFrameCardinal.Create(ID: Byte);
begin
  inherited Create(ID);
  Buffer := 0;
end;

destructor TDataFrameCardinal.Destroy;
begin
  inherited Destroy;
end;

procedure TDataFrameCardinal.LoadFromStream(stream: TMemoryStream64);
begin
  stream.Read64(FBuffer, C_Cardinal_Size);
end;

procedure TDataFrameCardinal.SaveToStream(stream: TMemoryStream64);
begin
  stream.Write64(FBuffer, C_Cardinal_Size);
end;

{$IFNDEF FPC}


procedure TDataFrameCardinal.LoadFromJson(jarry: TJsonArray; idx: Integer);
begin
  FBuffer := jarry.i[idx];
end;

procedure TDataFrameCardinal.SaveToJson(jarry: TJsonArray; idx: Integer);
begin
  jarry.Add(FBuffer);
end;
{$ENDIF}


function TDataFrameCardinal.ComputeEncodeSize: Integer;
begin
  Result := C_Cardinal_Size;
end;

constructor TDataFrameWord.Create(ID: Byte);
begin
  inherited Create(ID);
  Buffer := 0;
end;

destructor TDataFrameWord.Destroy;
begin
  inherited Destroy;
end;

procedure TDataFrameWord.LoadFromStream(stream: TMemoryStream64);
begin
  stream.Read64(FBuffer, C_Word_Size);
end;

procedure TDataFrameWord.SaveToStream(stream: TMemoryStream64);
begin
  stream.Write64(FBuffer, C_Word_Size);
end;

{$IFNDEF FPC}


procedure TDataFrameWord.LoadFromJson(jarry: TJsonArray; idx: Integer);
begin
  FBuffer := jarry.i[idx];
end;

procedure TDataFrameWord.SaveToJson(jarry: TJsonArray; idx: Integer);
begin
  jarry.Add(FBuffer);
end;
{$ENDIF}


function TDataFrameWord.ComputeEncodeSize: Integer;
begin
  Result := C_Word_Size;
end;

constructor TDataFrameByte.Create(ID: Byte);
begin
  inherited Create(ID);
  Buffer := 0;
end;

destructor TDataFrameByte.Destroy;
begin
  inherited Destroy;
end;

procedure TDataFrameByte.LoadFromStream(stream: TMemoryStream64);
begin
  stream.Read64(FBuffer, C_Byte_Size);
end;

procedure TDataFrameByte.SaveToStream(stream: TMemoryStream64);
begin
  stream.Write64(FBuffer, C_Byte_Size);
end;

{$IFNDEF FPC}


procedure TDataFrameByte.LoadFromJson(jarry: TJsonArray; idx: Integer);
begin
  FBuffer := jarry.i[idx];
end;

procedure TDataFrameByte.SaveToJson(jarry: TJsonArray; idx: Integer);
begin
  jarry.Add(FBuffer);
end;
{$ENDIF}


function TDataFrameByte.ComputeEncodeSize: Integer;
begin
  Result := C_Byte_Size;
end;

constructor TDataFrameSingle.Create(ID: Byte);
begin
  inherited Create(ID);
  Buffer := 0;
end;

destructor TDataFrameSingle.Destroy;
begin
  inherited Destroy;
end;

procedure TDataFrameSingle.LoadFromStream(stream: TMemoryStream64);
begin
  stream.Read64(FBuffer, C_Single_Size);
end;

procedure TDataFrameSingle.SaveToStream(stream: TMemoryStream64);
begin
  stream.Write64(FBuffer, C_Single_Size);
end;

{$IFNDEF FPC}


procedure TDataFrameSingle.LoadFromJson(jarry: TJsonArray; idx: Integer);
begin
  FBuffer := jarry.f[idx];
end;

procedure TDataFrameSingle.SaveToJson(jarry: TJsonArray; idx: Integer);
begin
  jarry.AddF(FBuffer);
end;
{$ENDIF}


function TDataFrameSingle.ComputeEncodeSize: Integer;
begin
  Result := C_Single_Size;
end;

constructor TDataFrameDouble.Create(ID: Byte);
begin
  inherited Create(ID);
  Buffer := 0;
end;

destructor TDataFrameDouble.Destroy;
begin
  inherited Destroy;
end;

procedure TDataFrameDouble.LoadFromStream(stream: TMemoryStream64);
begin
  stream.Read64(FBuffer, C_Double_Size);
end;

procedure TDataFrameDouble.SaveToStream(stream: TMemoryStream64);
begin
  stream.Write64(FBuffer, C_Double_Size);
end;

{$IFNDEF FPC}


procedure TDataFrameDouble.LoadFromJson(jarry: TJsonArray; idx: Integer);
begin
  FBuffer := jarry.f[idx];
end;

procedure TDataFrameDouble.SaveToJson(jarry: TJsonArray; idx: Integer);
begin
  jarry.AddF(FBuffer);
end;
{$ENDIF}


function TDataFrameDouble.ComputeEncodeSize: Integer;
begin
  Result := C_Double_Size;
end;

constructor TDataFrameArrayInteger.Create(ID: Byte);
begin
  inherited Create(ID);
  FBuffer := TCoreClassList.Create;
end;

destructor TDataFrameArrayInteger.Destroy;
begin
  Clear;
  DisposeObject(FBuffer);
  inherited Destroy;
end;

procedure TDataFrameArrayInteger.Clear;
var
  i: Integer;
  p: PInteger;
begin
  for i := 0 to FBuffer.Count - 1 do
    begin
      p := FBuffer[i];
      Dispose(p);
    end;
  FBuffer.Clear;
end;

procedure TDataFrameArrayInteger.Add(v: Integer);
var
  _PV: PInteger;
begin
  new(_PV);
  _PV^ := v;
  FBuffer.Add(_PV);
end;

procedure TDataFrameArrayInteger.Delete(idx: Integer);
begin
  Dispose(PInteger(FBuffer[idx]));
  FBuffer.Delete(idx);
end;

function TDataFrameArrayInteger.Count: Integer;
begin
  Result := FBuffer.Count;
end;

procedure TDataFrameArrayInteger.WriteArray(const a: array of Integer);
var
  i: Integer;
begin
  for i := low(a) to high(a) do
      Add(a[i]);
end;

procedure TDataFrameArrayInteger.LoadFromStream(stream: TMemoryStream64);
var
  i, L: Integer;
  d: Integer;
begin
  Clear;
  stream.Read64(L, C_Integer_Size);
  for i := 0 to L - 1 do
    begin
      stream.Read64(d, C_Integer_Size);
      Add(d);
    end;
end;

procedure TDataFrameArrayInteger.SaveToStream(stream: TMemoryStream64);
var
  i, L: Integer;
  d: Integer;
begin
  L := Count;
  stream.Write64(L, C_Integer_Size);
  for i := 0 to L - 1 do
    begin
      d := Buffer[i];
      stream.Write64(d, C_Integer_Size);
    end;
end;

{$IFNDEF FPC}


procedure TDataFrameArrayInteger.LoadFromJson(jarry: TJsonArray; idx: Integer);
var
  ja: TJsonArray;
  i: Integer;
begin
  ja := jarry.a[idx];
  for i := 0 to ja.Count - 1 do
      Add(ja.i[i]);
end;

procedure TDataFrameArrayInteger.SaveToJson(jarry: TJsonArray; idx: Integer);
var
  ja: TJsonArray;
  i: Integer;
begin
  ja := jarry.AddArray;
  for i := 0 to Count - 1 do
      ja.Add(Buffer[i]);
end;
{$ENDIF}


function TDataFrameArrayInteger.ComputeEncodeSize: Integer;
begin
  Result := C_Integer_Size + C_Integer_Size * Count;
end;

function TDataFrameArrayInteger.GetBuffer(idx: Integer): Integer;
begin
  Result := PInteger(FBuffer[idx])^;
end;

procedure TDataFrameArrayInteger.SetBuffer(idx: Integer; Value: Integer);
begin
  PInteger(FBuffer[idx])^ := Value;
end;

constructor TDataFrameArrayShortInt.Create(ID: ShortInt);
begin
  inherited Create(ID);
  FBuffer := TCoreClassList.Create;
end;

destructor TDataFrameArrayShortInt.Destroy;
begin
  Clear;
  DisposeObject(FBuffer);
  inherited Destroy;
end;

procedure TDataFrameArrayShortInt.Clear;
var
  i: Integer;
  p: PShortInt;
begin
  for i := 0 to FBuffer.Count - 1 do
    begin
      p := FBuffer[i];
      Dispose(p);
    end;
  FBuffer.Clear;
end;

procedure TDataFrameArrayShortInt.Add(v: ShortInt);
var
  _PV: PShortInt;
begin
  new(_PV);
  _PV^ := v;
  FBuffer.Add(_PV);
end;

procedure TDataFrameArrayShortInt.Delete(idx: Integer);
begin
  Dispose(PShortInt(FBuffer[idx]));
  FBuffer.Delete(idx);
end;

function TDataFrameArrayShortInt.Count: Integer;
begin
  Result := FBuffer.Count;
end;

procedure TDataFrameArrayShortInt.WriteArray(const a: array of ShortInt);
var
  i: Integer;
begin
  for i := low(a) to high(a) do
      Add(a[i]);
end;

procedure TDataFrameArrayShortInt.LoadFromStream(stream: TMemoryStream64);
var
  i, L: Integer;
  d: ShortInt;
begin
  Clear;
  stream.Read64(L, C_Integer_Size);
  for i := 0 to L - 1 do
    begin
      stream.Read64(d, C_Short_Int_Size);
      Add(d);
    end;
end;

procedure TDataFrameArrayShortInt.SaveToStream(stream: TMemoryStream64);
var
  i, L: Integer;
  d: ShortInt;
begin
  L := Count;
  stream.Write64(L, C_Integer_Size);
  for i := 0 to L - 1 do
    begin
      d := Buffer[i];
      stream.Write64(d, C_Short_Int_Size);
    end;
end;

{$IFNDEF FPC}


procedure TDataFrameArrayShortInt.LoadFromJson(jarry: TJsonArray; idx: Integer);
var
  ja: TJsonArray;
  i: Integer;
begin
  ja := jarry.a[idx];
  for i := 0 to ja.Count - 1 do
      Add(ja.i[i]);
end;

procedure TDataFrameArrayShortInt.SaveToJson(jarry: TJsonArray; idx: Integer);
var
  ja: TJsonArray;
  i: Integer;
begin
  ja := jarry.AddArray;
  for i := 0 to Count - 1 do
      ja.Add(Buffer[i]);
end;
{$ENDIF}


function TDataFrameArrayShortInt.ComputeEncodeSize: Integer;
begin
  Result := C_Integer_Size + C_Short_Int_Size * Count;
end;

function TDataFrameArrayShortInt.GetBuffer(idx: Integer): ShortInt;
begin
  Result := PShortInt(FBuffer[idx])^;
end;

procedure TDataFrameArrayShortInt.SetBuffer(idx: Integer; Value: ShortInt);
begin
  PShortInt(FBuffer[idx])^ := Value;
end;

constructor TDataFrameArrayByte.Create(ID: Byte);
begin
  inherited Create(ID);
  FBuffer := TCoreClassList.Create;
end;

destructor TDataFrameArrayByte.Destroy;
begin
  Clear;
  DisposeObject(FBuffer);
  inherited Destroy;
end;

procedure TDataFrameArrayByte.Clear;
var
  i: Integer;
  p: PByte;
begin
  for i := 0 to FBuffer.Count - 1 do
    begin
      p := FBuffer[i];
      Dispose(p);
    end;
  FBuffer.Clear;
end;

procedure TDataFrameArrayByte.Add(v: Byte);
var
  _PV: PByte;
begin
  new(_PV);
  _PV^ := v;
  FBuffer.Add(_PV);
end;

procedure TDataFrameArrayByte.AddPtrBuff(p: PByte; Size: Integer);
var
  i: Integer;
begin
  for i := 0 to Size - 1 do
    begin
      Add(p^);
      inc(p);
    end;
end;

procedure TDataFrameArrayByte.AddI64(v: Int64);
begin
  AddPtrBuff(@v, C_Int64_Size);
end;

procedure TDataFrameArrayByte.AddU64(v: UInt64);
begin
  AddPtrBuff(@v, C_UInt64_Size);
end;

procedure TDataFrameArrayByte.Addi(v: Integer);
begin
  AddPtrBuff(@v, C_Integer_Size);
end;

procedure TDataFrameArrayByte.AddWord(v: Word);
begin
  AddPtrBuff(@v, C_Word_Size);
end;

procedure TDataFrameArrayByte.Delete(idx: Integer);
begin
  Dispose(PByte(FBuffer[idx]));
  FBuffer.Delete(idx);
end;

function TDataFrameArrayByte.Count: Integer;
begin
  Result := FBuffer.Count;
end;

procedure TDataFrameArrayByte.WriteArray(const a: array of Byte);
var
  i: Integer;
begin
  for i := low(a) to high(a) do
      Add(a[i]);
end;

procedure TDataFrameArrayByte.SetArray(const a: array of Byte);
var
  i: Integer;
begin
  Clear;
  for i := low(a) to high(a) do
      Add(a[i]);
end;

procedure TDataFrameArrayByte.SetBuff(p: PByte; Size: Integer);
begin
  Clear;
  AddPtrBuff(p, Size);
end;

procedure TDataFrameArrayByte.GetBuff(p: PByte);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    begin
      p^ := PByte(FBuffer[i])^;
      inc(p);
    end;
end;

procedure TDataFrameArrayByte.LoadFromStream(stream: TMemoryStream64);
var
  i, L: Integer;
  d: Byte;
begin
  Clear;
  stream.Read64(L, C_Integer_Size);
  for i := 0 to L - 1 do
    begin
      stream.Read64(d, C_Byte_Size);
      Add(d);
    end;
end;

procedure TDataFrameArrayByte.SaveToStream(stream: TMemoryStream64);
var
  i, L: Integer;
  d: Byte;
begin
  L := Count;
  stream.Write64(L, C_Integer_Size);
  for i := 0 to L - 1 do
    begin
      d := Buffer[i];
      stream.Write64(d, C_Byte_Size);
    end;
end;

{$IFNDEF FPC}


procedure TDataFrameArrayByte.LoadFromJson(jarry: TJsonArray; idx: Integer);
var
  ja: TJsonArray;
  i: Integer;
begin
  ja := jarry.a[idx];
  for i := 0 to ja.Count - 1 do
      Add(ja.i[i]);
end;

procedure TDataFrameArrayByte.SaveToJson(jarry: TJsonArray; idx: Integer);
var
  ja: TJsonArray;
  i: Integer;
begin
  ja := jarry.AddArray;
  for i := 0 to Count - 1 do
      ja.Add(Buffer[i]);
end;
{$ENDIF}


function TDataFrameArrayByte.ComputeEncodeSize: Integer;
begin
  Result := C_Integer_Size + C_Byte_Size * Count;
end;

function TDataFrameArraySingle.GetBuffer(idx: Integer): Single;
begin
  Result := PSingle(FBuffer[idx])^;
end;

procedure TDataFrameArraySingle.SetBuffer(idx: Integer; Value: Single);
begin
  PSingle(FBuffer[idx])^ := Value;
end;

function TDataFrameArrayByte.GetBuffer(idx: Integer): Byte;
begin
  Result := PByte(FBuffer[idx])^;
end;

procedure TDataFrameArrayByte.SetBuffer(idx: Integer; Value: Byte);
begin
  PByte(FBuffer[idx])^ := Value;
end;

constructor TDataFrameArraySingle.Create(ID: Byte);
begin
  inherited Create(ID);
  FBuffer := TCoreClassList.Create;
end;

destructor TDataFrameArraySingle.Destroy;
begin
  Clear;
  DisposeObject(FBuffer);
  inherited Destroy;
end;

procedure TDataFrameArraySingle.Clear;
var
  i: Integer;
  p: PSingle;
begin
  for i := 0 to FBuffer.Count - 1 do
    begin
      p := FBuffer[i];
      Dispose(p);
    end;
  FBuffer.Clear;
end;

procedure TDataFrameArraySingle.Add(v: Single);
var
  _PV: PSingle;
begin
  new(_PV);
  _PV^ := v;
  FBuffer.Add(_PV);
end;

procedure TDataFrameArraySingle.Delete(idx: Integer);
begin
  Dispose(PSingle(FBuffer[idx]));
  FBuffer.Delete(idx);
end;

function TDataFrameArraySingle.Count: Integer;
begin
  Result := FBuffer.Count;
end;

procedure TDataFrameArraySingle.WriteArray(const a: array of Single);
var
  i: Integer;
begin
  for i := low(a) to high(a) do
      Add(a[i]);
end;

procedure TDataFrameArraySingle.LoadFromStream(stream: TMemoryStream64);
var
  i, L: Integer;
  d: Single;
begin
  Clear;
  stream.Read64(L, C_Integer_Size);
  for i := 0 to L - 1 do
    begin
      stream.Read64(d, C_Single_Size);
      Add(d);
    end;
end;

procedure TDataFrameArraySingle.SaveToStream(stream: TMemoryStream64);
var
  i, L: Integer;
  d: Single;
begin
  L := Count;
  stream.Write64(L, C_Integer_Size);
  for i := 0 to L - 1 do
    begin
      d := Buffer[i];
      stream.Write64(d, C_Single_Size);
    end;
end;

{$IFNDEF FPC}


procedure TDataFrameArraySingle.LoadFromJson(jarry: TJsonArray; idx: Integer);
var
  ja: TJsonArray;
  i: Integer;
begin
  ja := jarry.a[idx];
  for i := 0 to ja.Count - 1 do
      Add(ja.f[i]);
end;

procedure TDataFrameArraySingle.SaveToJson(jarry: TJsonArray; idx: Integer);
var
  ja: TJsonArray;
  i: Integer;
begin
  ja := jarry.AddArray;
  for i := 0 to Count - 1 do
      ja.AddF(Buffer[i]);
end;
{$ENDIF}


function TDataFrameArraySingle.ComputeEncodeSize: Integer;
begin
  Result := C_Integer_Size + C_Single_Size * Count;
end;

constructor TDataFrameArrayDouble.Create(ID: Byte);
begin
  inherited Create(ID);
  FBuffer := TCoreClassList.Create;
end;

destructor TDataFrameArrayDouble.Destroy;
begin
  Clear;
  DisposeObject(FBuffer);
  inherited Destroy;
end;

procedure TDataFrameArrayDouble.Clear;
var
  i: Integer;
  p: PDouble;
begin
  for i := 0 to FBuffer.Count - 1 do
    begin
      p := FBuffer[i];
      Dispose(p);
    end;
  FBuffer.Clear;
end;

procedure TDataFrameArrayDouble.Add(v: Double);
var
  _PV: PDouble;
begin
  new(_PV);
  _PV^ := v;
  FBuffer.Add(_PV);
end;

procedure TDataFrameArrayDouble.Delete(idx: Integer);
begin
  Dispose(PDouble(FBuffer[idx]));
  FBuffer.Delete(idx);
end;

function TDataFrameArrayDouble.Count: Integer;
begin
  Result := FBuffer.Count;
end;

procedure TDataFrameArrayDouble.WriteArray(const a: array of Double);
var
  i: Integer;
begin
  for i := low(a) to high(a) do
      Add(a[i]);
end;

procedure TDataFrameArrayDouble.LoadFromStream(stream: TMemoryStream64);
var
  i, L: Integer;
  d: Double;
begin
  Clear;
  stream.Read64(L, C_Integer_Size);
  for i := 0 to L - 1 do
    begin
      stream.Read64(d, C_Double_Size);
      Add(d);
    end;
end;

procedure TDataFrameArrayDouble.SaveToStream(stream: TMemoryStream64);
var
  i, L: Integer;
  d: Double;
begin
  L := Count;
  stream.Write64(L, C_Integer_Size);
  for i := 0 to L - 1 do
    begin
      d := Buffer[i];
      stream.Write64(d, C_Double_Size);
    end;
end;

{$IFNDEF FPC}


procedure TDataFrameArrayDouble.LoadFromJson(jarry: TJsonArray; idx: Integer);
var
  ja: TJsonArray;
  i: Integer;
begin
  ja := jarry.a[idx];
  for i := 0 to ja.Count - 1 do
      Add(ja.f[i]);
end;

procedure TDataFrameArrayDouble.SaveToJson(jarry: TJsonArray; idx: Integer);
var
  ja: TJsonArray;
  i: Integer;
begin
  ja := jarry.AddArray;
  for i := 0 to Count - 1 do
      ja.AddF(Buffer[i]);
end;
{$ENDIF}


function TDataFrameArrayDouble.ComputeEncodeSize: Integer;
begin
  Result := C_Integer_Size + C_Double_Size * Count;
end;

function TDataFrameArrayDouble.GetBuffer(idx: Integer): Double;
begin
  Result := PDouble(FBuffer[idx])^;
end;

procedure TDataFrameArrayDouble.SetBuffer(idx: Integer; Value: Double);
begin
  PDouble(FBuffer[idx])^ := Value;
end;

constructor TDataFrameArrayInt64.Create(ID: Byte);
begin
  inherited Create(ID);
  FBuffer := TCoreClassList.Create;
end;

destructor TDataFrameArrayInt64.Destroy;
begin
  Clear;
  DisposeObject(FBuffer);
  inherited Destroy;
end;

procedure TDataFrameArrayInt64.Clear;
var
  i: Integer;
  p: PInt64;
begin
  for i := 0 to FBuffer.Count - 1 do
    begin
      p := FBuffer[i];
      Dispose(p);
    end;
  FBuffer.Clear;
end;

procedure TDataFrameArrayInt64.Add(v: Int64);
var
  _PV: PInt64;
begin
  new(_PV);
  _PV^ := v;
  FBuffer.Add(_PV);
end;

procedure TDataFrameArrayInt64.Delete(idx: Integer);
begin
  Dispose(PInt64(FBuffer[idx]));
  FBuffer.Delete(idx);
end;

function TDataFrameArrayInt64.Count: Integer;
begin
  Result := FBuffer.Count;
end;

procedure TDataFrameArrayInt64.WriteArray(const a: array of Int64);
var
  i: Integer;
begin
  for i := low(a) to high(a) do
      Add(a[i]);
end;

procedure TDataFrameArrayInt64.LoadFromStream(stream: TMemoryStream64);
var
  i, L: Integer;
  d: Int64;
begin
  Clear;
  stream.Read64(L, C_Integer_Size);
  for i := 0 to L - 1 do
    begin
      stream.Read64(d, C_Int64_Size);
      Add(d);
    end;
end;

procedure TDataFrameArrayInt64.SaveToStream(stream: TMemoryStream64);
var
  i, L: Integer;
  d: Int64;
begin
  L := Count;
  stream.Write64(L, C_Integer_Size);
  for i := 0 to L - 1 do
    begin
      d := Buffer[i];
      stream.Write64(d, C_Int64_Size);
    end;
end;

{$IFNDEF FPC}


procedure TDataFrameArrayInt64.LoadFromJson(jarry: TJsonArray; idx: Integer);
var
  ja: TJsonArray;
  i: Integer;
begin
  ja := jarry.a[idx];
  for i := 0 to ja.Count - 1 do
      Add(ja.L[i]);
end;

procedure TDataFrameArrayInt64.SaveToJson(jarry: TJsonArray; idx: Integer);
var
  ja: TJsonArray;
  i: Integer;
begin
  ja := jarry.AddArray;
  for i := 0 to Count - 1 do
      ja.Add(Buffer[i]);
end;
{$ENDIF}


function TDataFrameArrayInt64.ComputeEncodeSize: Integer;
begin
  Result := C_Integer_Size + C_Int64_Size * Count;
end;

function TDataFrameArrayInt64.GetBuffer(idx: Integer): Int64;
begin
  Result := PInt64(FBuffer[idx])^;
end;

procedure TDataFrameArrayInt64.SetBuffer(idx: Integer; Value: Int64);
begin
  PInt64(FBuffer[idx])^ := Value;
end;

constructor TDataFrameStream.Create(ID: Byte);
begin
  inherited Create(ID);
  FBuffer := TMemoryStream64.Create;
end;

destructor TDataFrameStream.Destroy;
begin
  DisposeObject(FBuffer);
  inherited Destroy;
end;

procedure TDataFrameStream.Clear;
begin
  FBuffer.Clear;
end;

procedure TDataFrameStream.LoadFromStream(stream: TMemoryStream64);
var
  _Len: Integer;
begin
  FBuffer.Clear;
  stream.Read64(_Len, C_Integer_Size);
  if (_Len > 0) then
      FBuffer.CopyFrom(stream, _Len);
end;

procedure TDataFrameStream.SaveToStream(stream: TMemoryStream64);
var
  _Len: Integer;
begin
  _Len := FBuffer.Size;
  stream.Write64(_Len, C_Integer_Size);
  if _Len > 0 then
    begin
      FBuffer.Position := 0;
      stream.CopyFrom(FBuffer, _Len);
    end;
end;

{$IFNDEF FPC}


procedure TDataFrameStream.LoadFromJson(jarry: TJsonArray; idx: Integer);
var
  b64: TPascalString;
begin
  FBuffer.Clear;
  b64.Text := jarry.s[idx];
  umlDecodeStreamBASE64(b64, FBuffer);
end;

procedure TDataFrameStream.SaveToJson(jarry: TJsonArray; idx: Integer);
var
  b64: TPascalString;
begin
  umlEncodeStreamBASE64(FBuffer, b64);
  jarry.Add(b64.Text);
end;
{$ENDIF}


function TDataFrameStream.ComputeEncodeSize: Integer;
begin
  Result := C_Integer_Size + FBuffer.Size;
end;

function TDataFrameStream.GetBuffer: TCoreClassStream;
begin
  Result := FBuffer;
end;

procedure TDataFrameStream.SetBuffer(_Buffer: TCoreClassStream);
var
  _P: Int64;
begin
  FBuffer.Clear;
  _P := _Buffer.Position;
  _Buffer.Position := 0;
  if _Buffer.Size > 0 then
      FBuffer.CopyFrom(_Buffer, _Buffer.Size);
  _Buffer.Position := _P;
end;

constructor TDataFrameVariant.Create(ID: Byte);
begin
  inherited Create(ID);
end;

destructor TDataFrameVariant.Destroy;
begin
  inherited Destroy;
end;

procedure TDataFrameVariant.LoadFromStream(stream: TMemoryStream64);
var
  vt: TVarType;
begin
  vt := TVarType(stream.ReadUInt16);
  case vt of
    varSmallInt: FBuffer := stream.ReadInt16;
    varInteger: FBuffer := stream.ReadInt32;
    varSingle: FBuffer := stream.ReadSingle;
    varDouble: FBuffer := stream.ReadDouble;
    varCurrency: FBuffer := stream.ReadCurrency;
    varBoolean: FBuffer := stream.ReadBool;
    varShortInt: FBuffer := stream.ReadInt8;
    varByte: FBuffer := stream.ReadUInt8;
    varWord: FBuffer := stream.ReadUInt16;
    varLongWord: FBuffer := stream.ReadUInt32;
    varInt64: FBuffer := stream.ReadInt64;
    varUInt64: FBuffer := stream.ReadUInt64;
    varOleStr, varString, varUString: FBuffer := stream.ReadString.Text;
    else RaiseInfo('error variant type');
  end;
end;

procedure TDataFrameVariant.SaveToStream(stream: TMemoryStream64);
var
  vt: TVarType;
begin
  vt := TVarData(FBuffer).VType;
  stream.WriteUInt16(Word(vt));
  case vt of
    varSmallInt: stream.WriteInt16(FBuffer);
    varInteger: stream.WriteInt32(FBuffer);
    varSingle: stream.WriteSingle(FBuffer);
    varDouble: stream.WriteDouble(FBuffer);
    varCurrency: stream.WriteCurrency(FBuffer);
    varBoolean: stream.WriteBool(FBuffer);
    varShortInt: stream.WriteInt8(FBuffer);
    varByte: stream.WriteUInt8(FBuffer);
    varWord: stream.WriteUInt16(FBuffer);
    varLongWord: stream.WriteUInt32(FBuffer);
    varInt64: stream.WriteInt64(FBuffer);
    varUInt64: stream.WriteUInt64(FBuffer);
    varOleStr, varString, varUString: stream.WriteString(FBuffer);
    else
      RaiseInfo('error variant type');
  end;
end;

{$IFNDEF FPC}


procedure TDataFrameVariant.LoadFromJson(jarry: TJsonArray; idx: Integer);
begin
  FBuffer := umlStrToVar(jarry.s[idx]);
end;

procedure TDataFrameVariant.SaveToJson(jarry: TJsonArray; idx: Integer);
begin
  jarry.Add(umlVarToStr(FBuffer, True).Text);
end;
{$ENDIF}


function TDataFrameVariant.ComputeEncodeSize: Integer;
var
  tmp: TMemoryStream64;
begin
  tmp := TMemoryStream64.CustomCreate(1024);
  SaveToStream(tmp);
  Result := tmp.Size;
  DisposeObject(tmp);
end;

constructor TDataFrameInt64.Create(ID: Byte);
begin
  inherited Create(ID);
  Buffer := 0;
end;

destructor TDataFrameInt64.Destroy;
begin
  inherited Destroy;
end;

procedure TDataFrameInt64.LoadFromStream(stream: TMemoryStream64);
begin
  stream.Read64(FBuffer, C_Int64_Size);
end;

procedure TDataFrameInt64.SaveToStream(stream: TMemoryStream64);
begin
  stream.Write64(FBuffer, C_Int64_Size);
end;

{$IFNDEF FPC}


procedure TDataFrameInt64.LoadFromJson(jarry: TJsonArray; idx: Integer);
begin
  FBuffer := jarry.L[idx];
end;

procedure TDataFrameInt64.SaveToJson(jarry: TJsonArray; idx: Integer);
begin
  jarry.Add(FBuffer);
end;
{$ENDIF}


function TDataFrameInt64.ComputeEncodeSize: Integer;
begin
  Result := C_Int64_Size;
end;

constructor TDataFrameUInt64.Create(ID: Byte);
begin
  inherited Create(ID);
  Buffer := 0;
end;

destructor TDataFrameUInt64.Destroy;
begin
  inherited Destroy;
end;

procedure TDataFrameUInt64.LoadFromStream(stream: TMemoryStream64);
begin
  stream.Read64(FBuffer, C_UInt64_Size);
end;

procedure TDataFrameUInt64.SaveToStream(stream: TMemoryStream64);
begin
  stream.Write64(FBuffer, C_UInt64_Size);
end;

{$IFNDEF FPC}


procedure TDataFrameUInt64.LoadFromJson(jarry: TJsonArray; idx: Integer);
begin
  FBuffer := jarry.u[idx];
end;

procedure TDataFrameUInt64.SaveToJson(jarry: TJsonArray; idx: Integer);
begin
  jarry.Add(FBuffer);
end;
{$ENDIF}


function TDataFrameUInt64.ComputeEncodeSize: Integer;
begin
  Result := C_UInt64_Size;
end;

constructor TDataFrameEngineReader.Create(AOwner: TDataFrameEngine);
begin
  inherited Create;
  FOwner := AOwner;
  FIndex := 0;
end;

destructor TDataFrameEngineReader.Destroy;
begin
  inherited Destroy;
end;

function TDataFrameEngineReader.IsEnd: Boolean;
begin
  Result := FIndex >= FOwner.Count;
end;

function TDataFrameEngineReader.NotEnd: Boolean;
begin
  Result := FIndex < FOwner.Count;
end;

procedure TDataFrameEngineReader.GoNext;
begin
  inc(FIndex);
end;

function TDataFrameEngineReader.ReadString: SystemString;
begin
  Result := FOwner.ReadString(FIndex);
  inc(FIndex);
end;

function TDataFrameEngineReader.ReadBytes: TBytes;
begin
  Result := FOwner.ReadBytes(FIndex);
  inc(FIndex);
end;

function TDataFrameEngineReader.ReadInteger: Integer;
begin
  Result := FOwner.ReadInteger(FIndex);
  inc(FIndex);
end;

function TDataFrameEngineReader.ReadCardinal: Cardinal;
begin
  Result := FOwner.ReadCardinal(FIndex);
  inc(FIndex);
end;

function TDataFrameEngineReader.ReadWord: Word;
begin
  Result := FOwner.ReadWord(FIndex);
  inc(FIndex);
end;

function TDataFrameEngineReader.ReadBool: Boolean;
begin
  Result := FOwner.ReadBool(FIndex);
  inc(FIndex);
end;

function TDataFrameEngineReader.ReadBoolean: Boolean;
begin
  Result := ReadBool;
end;

function TDataFrameEngineReader.ReadByte: Byte;
begin
  Result := FOwner.ReadByte(FIndex);
  inc(FIndex);
end;

function TDataFrameEngineReader.ReadSingle: Single;
begin
  Result := FOwner.ReadSingle(FIndex);
  inc(FIndex);
end;

function TDataFrameEngineReader.ReadDouble: Double;
begin
  Result := FOwner.ReadDouble(FIndex);
  inc(FIndex);
end;

function TDataFrameEngineReader.ReadArrayInteger: TDataFrameArrayInteger;
begin
  Result := FOwner.ReadArrayInteger(FIndex);
  inc(FIndex);
end;

function TDataFrameEngineReader.ReadArrayShortInt: TDataFrameArrayShortInt;
begin
  Result := FOwner.ReadArrayShortInt(FIndex);
  inc(FIndex);
end;

function TDataFrameEngineReader.ReadArrayByte: TDataFrameArrayByte;
begin
  Result := FOwner.ReadArrayByte(FIndex);
  inc(FIndex);
end;

function TDataFrameEngineReader.ReadMD5: UnicodeMixedLib.TMD5;
begin
  Result := FOwner.ReadMD5(FIndex);
  inc(FIndex);
end;

function TDataFrameEngineReader.ReadArraySingle: TDataFrameArraySingle;
begin
  Result := FOwner.ReadArraySingle(FIndex);
  inc(FIndex);
end;

function TDataFrameEngineReader.ReadArrayDouble: TDataFrameArrayDouble;
begin
  Result := FOwner.ReadArrayDouble(FIndex);
  inc(FIndex);
end;

function TDataFrameEngineReader.ReadArrayInt64: TDataFrameArrayInt64;
begin
  Result := FOwner.ReadArrayInt64(FIndex);
  inc(FIndex);
end;

procedure TDataFrameEngineReader.ReadStream(output: TCoreClassStream);
begin
  FOwner.ReadStream(FIndex, output);
  inc(FIndex);
end;

function TDataFrameEngineReader.ReadVariant: Variant;
begin
  Result := FOwner.ReadVariant(FIndex);
  inc(FIndex);
end;

function TDataFrameEngineReader.ReadInt64: Int64;
begin
  Result := FOwner.ReadInt64(FIndex);
  inc(FIndex);
end;

function TDataFrameEngineReader.ReadUInt64: UInt64;
begin
  Result := FOwner.ReadUInt64(FIndex);
  inc(FIndex);
end;

procedure TDataFrameEngineReader.ReadStrings(output: TCoreClassStrings);
begin
  FOwner.ReadStrings(FIndex, output);
  inc(FIndex);
end;

procedure TDataFrameEngineReader.ReadListStrings(output: TListString);
begin
  FOwner.ReadListStrings(FIndex, output);
  inc(FIndex);
end;

procedure TDataFrameEngineReader.ReadPascalStrings(output: TListPascalString);
begin
  FOwner.ReadPascalStrings(FIndex, output);
  inc(FIndex);
end;

procedure TDataFrameEngineReader.ReadDataFrame(output: TDataFrameEngine);
begin
  FOwner.ReadDataFrame(FIndex, output);
  inc(FIndex);
end;

procedure TDataFrameEngineReader.ReadHashStringList(output: THashStringList);
begin
  FOwner.ReadHashStringList(FIndex, output);
  inc(FIndex);
end;

procedure TDataFrameEngineReader.ReadVariantList(output: THashVariantList);
begin
  FOwner.ReadVariantList(FIndex, output);
  inc(FIndex);
end;

procedure TDataFrameEngineReader.ReadSectionText(output: TSectionTextData);
begin
  FOwner.ReadSectionText(FIndex, output);
  inc(FIndex);
end;

procedure TDataFrameEngineReader.ReadTextSection(output: TSectionTextData);
begin
  ReadSectionText(output);
end;

{$IFNDEF FPC}


procedure TDataFrameEngineReader.ReadJson(output: TJsonObject);
begin
  FOwner.ReadJson(FIndex, output);
  inc(FIndex);
end;
{$ENDIF}


function TDataFrameEngineReader.ReadRect: TRect;
begin
  Result := FOwner.ReadRect(FIndex);
  inc(FIndex);
end;

function TDataFrameEngineReader.ReadRectf: TRectf;
begin
  Result := FOwner.ReadRectf(FIndex);
  inc(FIndex);
end;

function TDataFrameEngineReader.ReadPoint: TPoint;
begin
  Result := FOwner.ReadPoint(FIndex);
  inc(FIndex);
end;

function TDataFrameEngineReader.ReadPointf: TPointf;
begin
  Result := FOwner.ReadPointf(FIndex);
  inc(FIndex);
end;

function TDataFrameEngineReader.ReadVector: TVector;
begin
  Result := FOwner.ReadVector(FIndex);
  inc(FIndex);
end;

function TDataFrameEngineReader.ReadAffineVector: TAffineVector;
begin
  Result := FOwner.ReadAffineVector(FIndex);
  inc(FIndex);
end;

function TDataFrameEngineReader.ReadVec3: TVec3;
begin
  Result := FOwner.ReadVec3(FIndex);
  inc(FIndex);
end;

function TDataFrameEngineReader.ReadVec4: TVec4;
begin
  Result := FOwner.ReadVec4(FIndex);
  inc(FIndex);
end;

function TDataFrameEngineReader.ReadVector3: TVector3;
begin
  Result := FOwner.ReadVector3(FIndex);
  inc(FIndex);
end;

function TDataFrameEngineReader.ReadVector4: TVector4;
begin
  Result := FOwner.ReadVector4(FIndex);
  inc(FIndex);
end;

function TDataFrameEngineReader.ReadMat4: TMat4;
begin
  Result := FOwner.ReadMat4(FIndex);
  inc(FIndex);
end;

function TDataFrameEngineReader.ReadMatrix4: TMatrix4;
begin
  Result := FOwner.ReadMatrix4(FIndex);
  inc(FIndex);
end;

function TDataFrameEngineReader.Read2DPoint: T2DPoint;
begin
  Result := FOwner.Read2DPoint(FIndex);
  inc(FIndex);
end;

function TDataFrameEngineReader.ReadVec2: TVec2;
begin
  Result := FOwner.ReadVec2(FIndex);
  inc(FIndex);
end;

function TDataFrameEngineReader.ReadRectV2: TRectV2;
begin
  Result := FOwner.ReadRectV2(FIndex);
  inc(FIndex);
end;

function TDataFrameEngineReader.ReadPointer: UInt64;
begin
  Result := FOwner.ReadPointer(FIndex);
  inc(FIndex);
end;

procedure TDataFrameEngineReader.read(var aBuf; aCount: Int64);
begin
  FOwner.read(FIndex, aBuf, aCount);
  inc(FIndex);
end;

function TDataFrameEngineReader.read: TDataFrameBase;
begin
  Result := FOwner.Data[FIndex];
  inc(FIndex);
end;

function TDataFrameEngine.DataTypeToByte(v: TRunTimeDataType): Byte;
begin
  Result := Byte(v);
end;

function TDataFrameEngine.ByteToDataType(v: Byte): TRunTimeDataType;
begin
  Result := TRunTimeDataType(v);
end;

constructor TDataFrameEngine.Create;
begin
  inherited Create;
  FDataList := TCoreClassListForObj.Create;
  FReader := TDataFrameEngineReader.Create(Self);
  FCompressorDeflate := nil;
  FCompressorBRRC := nil;
end;

destructor TDataFrameEngine.Destroy;
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

procedure TDataFrameEngine.Clear;
var
  i: Integer;
  Obj: TDataFrameBase;
begin
  for i := 0 to FDataList.Count - 1 do
    begin
      Obj := TDataFrameBase(FDataList[i]);
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

function TDataFrameEngine.AddData(v: TRunTimeDataType): TDataFrameBase;
begin
  case v of
    rdtString:
      Result := TDataFrameString.Create(DataTypeToByte(v));
    rdtInteger:
      Result := TDataFrameInteger.Create(DataTypeToByte(v));
    rdtCardinal:
      Result := TDataFrameCardinal.Create(DataTypeToByte(v));
    rdtWORD:
      Result := TDataFrameWord.Create(DataTypeToByte(v));
    rdtByte:
      Result := TDataFrameByte.Create(DataTypeToByte(v));
    rdtSingle:
      Result := TDataFrameSingle.Create(DataTypeToByte(v));
    rdtDouble:
      Result := TDataFrameDouble.Create(DataTypeToByte(v));
    rdtArrayInteger:
      Result := TDataFrameArrayInteger.Create(DataTypeToByte(v));
    rdtArrayShortInt:
      Result := TDataFrameArrayShortInt.Create(DataTypeToByte(v));
    rdtArrayByte:
      Result := TDataFrameArrayByte.Create(DataTypeToByte(v));
    rdtArraySingle:
      Result := TDataFrameArraySingle.Create(DataTypeToByte(v));
    rdtArrayDouble:
      Result := TDataFrameArrayDouble.Create(DataTypeToByte(v));
    rdtArrayInt64:
      Result := TDataFrameArrayInt64.Create(DataTypeToByte(v));
    rdtStream:
      Result := TDataFrameStream.Create(DataTypeToByte(v));
    rdtVariant:
      Result := TDataFrameVariant.Create(DataTypeToByte(v));
    rdtInt64:
      Result := TDataFrameInt64.Create(DataTypeToByte(v));
    rdtUInt64:
      Result := TDataFrameUInt64.Create(DataTypeToByte(v));
    else
      Result := nil;
  end;
  if Result <> nil then
      FDataList.Add(Result);
end;

function TDataFrameEngine.GetData(idx: Integer): TDataFrameBase;
begin
  if (idx >= 0) and (idx < FDataList.Count) then
      Result := TDataFrameBase(FDataList[idx])
  else
      Result := nil;
end;

function TDataFrameEngine.GetDataInfo(Obj_: TDataFrameBase): SystemString;
begin
  case ByteToDataType(Obj_.FID) of
    rdtString:
      Result := 'SystemString';
    rdtInteger:
      Result := 'Integer';
    rdtCardinal:
      Result := 'Cardinal';
    rdtWORD:
      Result := 'WORD';
    rdtByte:
      Result := 'Byte';
    rdtSingle:
      Result := 'Single';
    rdtDouble:
      Result := 'Double';
    rdtArrayInteger:
      Result := 'ArrayInteger';
    rdtArrayShortInt:
      Result := 'ShortInt';
    rdtArrayByte:
      Result := 'Byte';
    rdtArraySingle:
      Result := 'ArraySingle';
    rdtArrayDouble:
      Result := 'ArrayDouble';
    rdtArrayInt64:
      Result := 'ArrayInt64';
    rdtStream:
      Result := 'Stream';
    rdtVariant:
      Result := 'Variant';
    rdtInt64:
      Result := 'Int64';
    rdtUInt64:
      Result := 'UInt64';
    else
      Result := '';
  end;
end;

function TDataFrameEngine.Count: Integer;
begin
  Result := FDataList.Count;
end;

function TDataFrameEngine.Delete(idx: Integer): Boolean;
begin
  try
    DisposeObject(TDataFrameBase(FDataList[idx]));
    FDataList.Delete(idx);
    Result := True;
  except
      Result := False;
  end;
end;

function TDataFrameEngine.DeleteFirst: Boolean;
begin
  Result := Delete(0);
end;

function TDataFrameEngine.DeleteLast: Boolean;
begin
  Result := Delete(Count - 1);
end;

function TDataFrameEngine.DeleteLastCount(cnt: Integer): Boolean;
begin
  Result := True;
  while cnt > 0 do
    begin
      Result := Result and DeleteLast;
      dec(cnt);
    end;
end;

function TDataFrameEngine.DeleteCount(idx, _Count: Integer): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to _Count - 1 do
      Result := Result and Delete(idx);
end;

procedure TDataFrameEngine.Assign(SameObj: TDataFrameEngine);
var
  s: TMemoryStream64;
begin
  Clear;
  s := TMemoryStream64.Create;
  SameObj.EncodeTo(s, True);
  s.Position := 0;
  DecodeFrom(s, True);
  DisposeObject(s);
end;

function TDataFrameEngine.Clone: TDataFrameEngine;
begin
  Result := TDataFrameEngine.Create;
  Result.Assign(Self);
end;

procedure TDataFrameEngine.WriteString(v: SystemString);
var
  Obj_: TDataFrameString;
begin
  Obj_ := TDataFrameString.Create(DataTypeToByte(rdtString));
  Obj_.Buffer := umlBytesOf(v);
  FDataList.Add(Obj_);
end;

procedure TDataFrameEngine.WriteString(v: TPascalString);
var
  Obj_: TDataFrameString;
begin
  Obj_ := TDataFrameString.Create(DataTypeToByte(rdtString));
  Obj_.Buffer := v.Bytes;
  FDataList.Add(Obj_);
end;

procedure TDataFrameEngine.WriteBytes(v: TBytes);
var
  Obj_: TDataFrameString;
begin
  Obj_ := TDataFrameString.Create(DataTypeToByte(rdtString));
  SetLength(Obj_.Buffer, length(v));
  if length(v) > 0 then
      CopyPtr(@v[0], @Obj_.Buffer[0], length(v));
  FDataList.Add(Obj_);
end;

procedure TDataFrameEngine.WriteInteger(v: Integer);
var
  Obj_: TDataFrameInteger;
begin
  Obj_ := TDataFrameInteger.Create(DataTypeToByte(rdtInteger));
  Obj_.Buffer := v;
  FDataList.Add(Obj_);
end;

procedure TDataFrameEngine.WriteCardinal(v: Cardinal);
var
  Obj_: TDataFrameCardinal;
begin
  Obj_ := TDataFrameCardinal.Create(DataTypeToByte(rdtCardinal));
  Obj_.Buffer := v;
  FDataList.Add(Obj_);
end;

procedure TDataFrameEngine.WriteWORD(v: Word);
var
  Obj_: TDataFrameWord;
begin
  Obj_ := TDataFrameWord.Create(DataTypeToByte(rdtWORD));
  Obj_.Buffer := v;
  FDataList.Add(Obj_);
end;

procedure TDataFrameEngine.WriteBool(v: Boolean);
begin
  if v then
      WriteByte(1)
  else
      WriteByte(0);
end;

procedure TDataFrameEngine.WriteBoolean(v: Boolean);
begin
  WriteBool(v);
end;

procedure TDataFrameEngine.WriteByte(v: Byte);
var
  Obj_: TDataFrameByte;
begin
  Obj_ := TDataFrameByte.Create(DataTypeToByte(rdtByte));
  Obj_.Buffer := v;
  FDataList.Add(Obj_);
end;

procedure TDataFrameEngine.WriteSingle(v: Single);
var
  Obj_: TDataFrameSingle;
begin
  Obj_ := TDataFrameSingle.Create(DataTypeToByte(rdtSingle));
  Obj_.Buffer := v;
  FDataList.Add(Obj_);
end;

procedure TDataFrameEngine.WriteDouble(v: Double);
var
  Obj_: TDataFrameDouble;
begin
  Obj_ := TDataFrameDouble.Create(DataTypeToByte(rdtDouble));
  Obj_.Buffer := v;
  FDataList.Add(Obj_);
end;

function TDataFrameEngine.WriteArrayInteger: TDataFrameArrayInteger;
begin
  Result := TDataFrameArrayInteger.Create(DataTypeToByte(rdtArrayInteger));
  FDataList.Add(Result);
end;

function TDataFrameEngine.WriteArrayShortInt: TDataFrameArrayShortInt;
begin
  Result := TDataFrameArrayShortInt.Create(DataTypeToByte(rdtArrayShortInt));
  FDataList.Add(Result);
end;

function TDataFrameEngine.WriteArrayByte: TDataFrameArrayByte;
begin
  Result := TDataFrameArrayByte.Create(DataTypeToByte(rdtArrayByte));
  FDataList.Add(Result);
end;

procedure TDataFrameEngine.WriteMD5(md5: UnicodeMixedLib.TMD5);
begin
  WriteArrayByte.WriteArray(md5);
end;

function TDataFrameEngine.WriteArraySingle: TDataFrameArraySingle;
begin
  Result := TDataFrameArraySingle.Create(DataTypeToByte(rdtArraySingle));
  FDataList.Add(Result);
end;

function TDataFrameEngine.WriteArrayDouble: TDataFrameArrayDouble;
begin
  Result := TDataFrameArrayDouble.Create(DataTypeToByte(rdtArrayDouble));
  FDataList.Add(Result);
end;

function TDataFrameEngine.WriteArrayInt64: TDataFrameArrayInt64;
begin
  Result := TDataFrameArrayInt64.Create(DataTypeToByte(rdtArrayInt64));
  FDataList.Add(Result);
end;

procedure TDataFrameEngine.WriteStream(v: TCoreClassStream);
var
  Obj_: TDataFrameStream;
begin
  Obj_ := TDataFrameStream.Create(DataTypeToByte(rdtStream));
  Obj_.Buffer := v;
  FDataList.Add(Obj_);
end;

procedure TDataFrameEngine.WriteVariant(v: Variant);
var
  Obj_: TDataFrameVariant;
begin
  Obj_ := TDataFrameVariant.Create(DataTypeToByte(rdtVariant));
  Obj_.Buffer := v;
  FDataList.Add(Obj_);
end;

procedure TDataFrameEngine.WriteInt64(v: Int64);
var
  Obj_: TDataFrameInt64;
begin
  Obj_ := TDataFrameInt64.Create(DataTypeToByte(rdtInt64));
  Obj_.Buffer := v;
  FDataList.Add(Obj_);
end;

procedure TDataFrameEngine.WriteUInt64(v: UInt64);
var
  Obj_: TDataFrameUInt64;
begin
  Obj_ := TDataFrameUInt64.Create(DataTypeToByte(rdtUInt64));
  Obj_.Buffer := v;
  FDataList.Add(Obj_);
end;

procedure TDataFrameEngine.WriteStrings(v: TCoreClassStrings);
var
  d: TMemoryStream64;
begin
  d := TMemoryStream64.Create;
{$IFDEF FPC}
  v.SaveToStream(d);
{$ELSE}
  v.SaveToStream(d, TEncoding.UTF8);
{$ENDIF}
  d.Position := 0;
  WriteStream(d);
  DisposeObject(d);
end;

procedure TDataFrameEngine.WriteListStrings(v: TListString);
var
  d: TMemoryStream64;
begin
  d := TMemoryStream64.Create;
  v.SaveToStream(d);
  d.Position := 0;
  WriteStream(d);
  DisposeObject(d);
end;

procedure TDataFrameEngine.WritePascalStrings(v: TListPascalString);
var
  d: TMemoryStream64;
begin
  d := TMemoryStream64.Create;
  v.SaveToStream(d);
  d.Position := 0;
  WriteStream(d);
  DisposeObject(d);
end;

procedure TDataFrameEngine.WriteDataFrame(v: TDataFrameEngine);
var
  Obj_: TDataFrameStream;
begin
  Obj_ := TDataFrameStream.Create(DataTypeToByte(rdtStream));
  v.EncodeTo(Obj_.Buffer, True);
  FDataList.Add(Obj_);
end;

procedure TDataFrameEngine.WriteDataFrameCompressed(v: TDataFrameEngine);
var
  Obj_: TDataFrameStream;
begin
  Obj_ := TDataFrameStream.Create(DataTypeToByte(rdtStream));
  v.EncodeAsZLib(Obj_.Buffer, True);
  FDataList.Add(Obj_);
end;

procedure TDataFrameEngine.WriteHashStringList(v: THashStringList);
var
  ms: TMemoryStream64;
  h: THashStringTextStream;
begin
  ms := TMemoryStream64.Create;
  h := THashStringTextStream.Create(v);
  h.SaveToStream(ms);
  DisposeObject(h);
  ms.Position := 0;
  WriteStream(ms);
  DisposeObject(ms);
end;

procedure TDataFrameEngine.WriteVariantList(v: THashVariantList);
var
  ms: TMemoryStream64;
  h: THashVariantTextStream;
begin
  ms := TMemoryStream64.Create;
  h := THashVariantTextStream.Create(v);
  h.SaveToStream(ms);
  DisposeObject(h);
  ms.Position := 0;
  WriteStream(ms);
  DisposeObject(ms);
end;

procedure TDataFrameEngine.WriteSectionText(v: TSectionTextData);
var
  ms: TMemoryStream64;
begin
  ms := TMemoryStream64.Create;
  v.SaveToStream(ms);
  ms.Position := 0;
  WriteStream(ms);
  DisposeObject(ms);
end;

procedure TDataFrameEngine.WriteTextSection(v: TSectionTextData);
begin
  WriteSectionText(v);
end;

{$IFNDEF FPC}


procedure TDataFrameEngine.WriteJson(v: TJsonObject);
var
  ms: TMemoryStream64;
begin
  ms := TMemoryStream64.Create;
  v.SaveToStream(ms, False, TEncoding.UTF8, True);
  ms.Position := 0;
  WriteStream(ms);
  DisposeObject(ms);
end;
{$ENDIF}


procedure TDataFrameEngine.WriteFile(fn: SystemString);
var
  fs: TCoreClassFileStream;
begin
  if umlFileExists(fn) then
    begin
      fs := TCoreClassFileStream.Create(fn, fmOpenRead or fmShareDenyWrite);
      fs.Position := 0;
      WriteStream(fs);
      DisposeObject(fs);
    end;
end;

procedure TDataFrameEngine.WriteRect(v: TRect);
begin
  with WriteArrayInteger do
    begin
      Add(v.Left);
      Add(v.Top);
      Add(v.Right);
      Add(v.Bottom);
    end;
end;

procedure TDataFrameEngine.WriteRectf(v: TRectf);
begin
  with WriteArraySingle do
    begin
      Add(v.Left);
      Add(v.Top);
      Add(v.Right);
      Add(v.Bottom);
    end;
end;

procedure TDataFrameEngine.WritePoint(v: TPoint);
begin
  with WriteArrayInteger do
    begin
      Add(v.x);
      Add(v.y);
    end;
end;

procedure TDataFrameEngine.WritePointf(v: TPointf);
begin
  with WriteArraySingle do
    begin
      Add(v.x);
      Add(v.y);
    end;
end;

procedure TDataFrameEngine.WriteVector(v: TVector);
begin
  WriteArraySingle.WriteArray(v);
end;

procedure TDataFrameEngine.WriteAffineVector(v: TAffineVector);
begin
  WriteArraySingle.WriteArray(v);
end;

procedure TDataFrameEngine.WriteVec4(v: TVec4);
begin
  WriteArraySingle.WriteArray(v);
end;

procedure TDataFrameEngine.WriteVec3(v: TVec3);
begin
  WriteArraySingle.WriteArray(v);
end;

procedure TDataFrameEngine.WriteVector4(v: TVector4);
begin
  WriteArraySingle.WriteArray(v.Link);
end;

procedure TDataFrameEngine.WriteVector3(v: TVector3);
begin
  WriteArraySingle.WriteArray(v.Link);
end;

procedure TDataFrameEngine.WriteMat4(v: TMat4);
begin
  with WriteArraySingle do
    begin
      WriteArray(v[0]);
      WriteArray(v[1]);
      WriteArray(v[2]);
      WriteArray(v[3]);
    end;
end;

procedure TDataFrameEngine.WriteMatrix4(v: TMatrix4);
begin
  WriteMat4(v.Link);
end;

procedure TDataFrameEngine.Write2DPoint(v: T2DPoint);
begin
  with WriteArraySingle do
      WriteArray(v);
end;

procedure TDataFrameEngine.WriteVec2(v: TVec2);
begin
  Write2DPoint(v);
end;

procedure TDataFrameEngine.WriteRectV2(v: TRectV2);
begin
  with WriteArraySingle do
    begin
      WriteArray(v[0]);
      WriteArray(v[1]);
    end;
end;

procedure TDataFrameEngine.WritePointer(v: Pointer);
begin
  WriteUInt64(UInt64(v));
end;

procedure TDataFrameEngine.WritePointer(v: UInt64);
begin
  WriteUInt64(v);
end;

// append new stream and write
procedure TDataFrameEngine.write(const aBuf; aCount: Int64);
var
  s: TMemoryStream64;
begin
  s := TMemoryStream64.Create;
  s.Write64(aBuf, aCount);
  WriteStream(s);
  DisposeObject(s);
end;

function TDataFrameEngine.ReadString(idx: Integer): SystemString;
var
  Obj_: TDataFrameBase;
  i: Integer;
begin
  Obj_ := Data[idx];
  if Obj_ is TDataFrameString then
      Result := umlStringOf(TDataFrameString(Obj_).Buffer).Text
  else if Obj_ is TDataFrameInteger then
      Result := IntToStr(TDataFrameInteger(Obj_).Buffer)
  else if Obj_ is TDataFrameCardinal then
      Result := IntToStr(TDataFrameCardinal(Obj_).Buffer)
  else if Obj_ is TDataFrameWord then
      Result := IntToStr(TDataFrameWord(Obj_).Buffer)
  else if Obj_ is TDataFrameByte then
      Result := IntToStr(TDataFrameByte(Obj_).Buffer)
  else if Obj_ is TDataFrameSingle then
      Result := FloatToStr(TDataFrameSingle(Obj_).Buffer)
  else if Obj_ is TDataFrameDouble then
      Result := FloatToStr(TDataFrameDouble(Obj_).Buffer)
  else if Obj_ is TDataFrameArrayInteger then
    begin
      Result := '(';
      with TDataFrameArrayInteger(Obj_) do
        begin
          for i := 0 to Count - 1 do
            if Result <> '(' then
                Result := Result + ',' + IntToStr(Buffer[i])
            else
                Result := Result + IntToStr(Buffer[i]);
        end;
      Result := Result + ')';
    end
  else if Obj_ is TDataFrameArrayShortInt then
    begin
      Result := '(';
      with TDataFrameArrayShortInt(Obj_) do
        begin
          for i := 0 to Count - 1 do
            if Result <> '(' then
                Result := Result + ',' + IntToStr(Buffer[i])
            else
                Result := Result + IntToStr(Buffer[i]);
        end;
      Result := Result + ')';
    end
  else if Obj_ is TDataFrameArrayByte then
    begin
      Result := '(';
      with TDataFrameArrayByte(Obj_) do
        begin
          for i := 0 to Count - 1 do
            if Result <> '(' then
                Result := Result + ',' + IntToStr(Buffer[i])
            else
                Result := Result + IntToStr(Buffer[i]);
        end;
      Result := Result + ')';
    end
  else if Obj_ is TDataFrameArraySingle then
    begin
      Result := '(';
      with TDataFrameArraySingle(Obj_) do
        begin
          for i := 0 to Count - 1 do
            if Result <> '(' then
                Result := Result + ',' + FloatToStr(Buffer[i])
            else
                Result := Result + FloatToStr(Buffer[i]);
        end;
      Result := Result + ')';
    end
  else if Obj_ is TDataFrameArrayDouble then
    begin
      Result := '(';
      with TDataFrameArrayDouble(Obj_) do
        begin
          for i := 0 to Count - 1 do
            if Result <> '(' then
                Result := Result + ',' + FloatToStr(Buffer[i])
            else
                Result := Result + FloatToStr(Buffer[i]);
        end;
      Result := Result + ')';
    end
  else if Obj_ is TDataFrameArrayInt64 then
    begin
      Result := '(';
      with TDataFrameArrayInt64(Obj_) do
        begin
          for i := 0 to Count - 1 do
            if Result <> '(' then
                Result := Result + ',' + IntToStr(Buffer[i])
            else
                Result := Result + IntToStr(Buffer[i]);
        end;
      Result := Result + ')';
    end
  else if Obj_ is TDataFrameVariant then
      Result := umlVarToStr(TDataFrameVariant(Obj_).Buffer)
  else if Obj_ is TDataFrameInt64 then
      Result := IntToStr(TDataFrameInt64(Obj_).Buffer)
  else if Obj_ is TDataFrameUInt64 then
{$IFDEF FPC}
    Result := IntToStr(TDataFrameUInt64(Obj_).Buffer)
{$ELSE}
    Result := UIntToStr(TDataFrameUInt64(Obj_).Buffer)
{$ENDIF}
  else
      Result := '';
end;

function TDataFrameEngine.ReadBytes(idx: Integer): TBytes;
var
  Obj_: TDataFrameBase;
  i: Integer;
begin
  Obj_ := Data[idx];
  if Obj_ is TDataFrameString then
    begin
      SetLength(Result, length(TDataFrameString(Obj_).Buffer));
      if length(Result) > 0 then
          CopyPtr(@TDataFrameString(Obj_).Buffer[0], @Result[0], length(Result));
    end
  else if Obj_ is TDataFrameArrayByte then
    begin
      SetLength(Result, TDataFrameArrayByte(Obj_).Count);
      for i := 0 to TDataFrameArrayByte(Obj_).Count - 1 do
          Result[i] := TDataFrameArrayByte(Obj_).Buffer[i];
    end
  else
      SetLength(Result, 0);
end;

function TDataFrameEngine.ReadInteger(idx: Integer): Integer;
var
  Obj_: TDataFrameBase;
begin
  Obj_ := Data[idx];
  if Obj_ is TDataFrameInteger then
      Result := TDataFrameInteger(Obj_).Buffer
  else if Obj_ is TDataFrameString then
      Result := umlStrToInt(umlStringOf(TDataFrameString(Obj_).Buffer), 0)
  else if Obj_ is TDataFrameCardinal then
      Result := TDataFrameCardinal(Obj_).Buffer
  else if Obj_ is TDataFrameWord then
      Result := TDataFrameWord(Obj_).Buffer
  else if Obj_ is TDataFrameByte then
      Result := TDataFrameByte(Obj_).Buffer
  else if Obj_ is TDataFrameSingle then
      Result := Trunc(TDataFrameSingle(Obj_).Buffer)
  else if Obj_ is TDataFrameDouble then
      Result := Trunc(TDataFrameDouble(Obj_).Buffer)
  else if Obj_ is TDataFrameVariant then
      Result := (TDataFrameVariant(Obj_).Buffer)
  else if Obj_ is TDataFrameInt64 then
      Result := (TDataFrameInt64(Obj_).Buffer)
  else if Obj_ is TDataFrameUInt64 then
      Result := (TDataFrameUInt64(Obj_).Buffer)
  else
      Result := 0;
end;

function TDataFrameEngine.ReadCardinal(idx: Integer): Cardinal;
var
  Obj_: TDataFrameBase;
begin
  Obj_ := Data[idx];
  if Obj_ is TDataFrameCardinal then
      Result := TDataFrameCardinal(Obj_).Buffer
  else if Obj_ is TDataFrameString then
      Result := umlStrToInt(umlStringOf(TDataFrameString(Obj_).Buffer), 0)
  else if Obj_ is TDataFrameInteger then
      Result := TDataFrameInteger(Obj_).Buffer
  else if Obj_ is TDataFrameWord then
      Result := TDataFrameWord(Obj_).Buffer
  else if Obj_ is TDataFrameByte then
      Result := TDataFrameByte(Obj_).Buffer
  else if Obj_ is TDataFrameSingle then
      Result := Trunc(TDataFrameSingle(Obj_).Buffer)
  else if Obj_ is TDataFrameDouble then
      Result := Trunc(TDataFrameDouble(Obj_).Buffer)
  else if Obj_ is TDataFrameVariant then
      Result := (TDataFrameVariant(Obj_).Buffer)
  else if Obj_ is TDataFrameInt64 then
      Result := (TDataFrameInt64(Obj_).Buffer)
  else if Obj_ is TDataFrameUInt64 then
      Result := (TDataFrameUInt64(Obj_).Buffer)
  else
      Result := 0;
end;

function TDataFrameEngine.ReadWord(idx: Integer): Word;
var
  Obj_: TDataFrameBase;
begin
  Obj_ := Data[idx];
  if Obj_ is TDataFrameWord then
      Result := TDataFrameWord(Obj_).Buffer
  else if Obj_ is TDataFrameString then
      Result := umlStrToInt(umlStringOf(TDataFrameString(Obj_).Buffer), 0)
  else if Obj_ is TDataFrameInteger then
      Result := TDataFrameInteger(Obj_).Buffer
  else if Obj_ is TDataFrameCardinal then
      Result := TDataFrameCardinal(Obj_).Buffer
  else if Obj_ is TDataFrameByte then
      Result := TDataFrameByte(Obj_).Buffer
  else if Obj_ is TDataFrameSingle then
      Result := Trunc(TDataFrameSingle(Obj_).Buffer)
  else if Obj_ is TDataFrameDouble then
      Result := Trunc(TDataFrameDouble(Obj_).Buffer)
  else if Obj_ is TDataFrameVariant then
      Result := (TDataFrameVariant(Obj_).Buffer)
  else if Obj_ is TDataFrameInt64 then
      Result := (TDataFrameInt64(Obj_).Buffer)
  else if Obj_ is TDataFrameUInt64 then
      Result := (TDataFrameUInt64(Obj_).Buffer)
  else
      Result := 0;
end;

function TDataFrameEngine.ReadBool(idx: Integer): Boolean;
begin
  Result := ReadByte(idx) = 1;
end;

function TDataFrameEngine.ReadBoolean(idx: Integer): Boolean;
begin
  Result := ReadBool(idx);
end;

function TDataFrameEngine.ReadByte(idx: Integer): Byte;
var
  Obj_: TDataFrameBase;
begin
  Obj_ := Data[idx];
  if Obj_ is TDataFrameByte then
      Result := TDataFrameByte(Obj_).Buffer
  else if Obj_ is TDataFrameString then
      Result := umlStrToInt(umlStringOf(TDataFrameString(Obj_).Buffer), 0)
  else if Obj_ is TDataFrameInteger then
      Result := TDataFrameInteger(Obj_).Buffer
  else if Obj_ is TDataFrameCardinal then
      Result := TDataFrameCardinal(Obj_).Buffer
  else if Obj_ is TDataFrameWord then
      Result := TDataFrameWord(Obj_).Buffer
  else if Obj_ is TDataFrameSingle then
      Result := Trunc(TDataFrameSingle(Obj_).Buffer)
  else if Obj_ is TDataFrameDouble then
      Result := Trunc(TDataFrameDouble(Obj_).Buffer)
  else if Obj_ is TDataFrameVariant then
      Result := (TDataFrameVariant(Obj_).Buffer)
  else if Obj_ is TDataFrameInt64 then
      Result := (TDataFrameInt64(Obj_).Buffer)
  else if Obj_ is TDataFrameUInt64 then
      Result := (TDataFrameUInt64(Obj_).Buffer)
  else
      Result := 0;
end;

function TDataFrameEngine.ReadSingle(idx: Integer): Single;
var
  Obj_: TDataFrameBase;
begin
  Obj_ := Data[idx];
  if Obj_ is TDataFrameSingle then
      Result := TDataFrameSingle(Obj_).Buffer
  else if Obj_ is TDataFrameString then
      Result := umlStrToFloat(umlStringOf(TDataFrameString(Obj_).Buffer), 0)
  else if Obj_ is TDataFrameInteger then
      Result := TDataFrameInteger(Obj_).Buffer
  else if Obj_ is TDataFrameCardinal then
      Result := TDataFrameCardinal(Obj_).Buffer
  else if Obj_ is TDataFrameWord then
      Result := TDataFrameWord(Obj_).Buffer
  else if Obj_ is TDataFrameByte then
      Result := TDataFrameByte(Obj_).Buffer
  else if Obj_ is TDataFrameDouble then
      Result := TDataFrameDouble(Obj_).Buffer
  else if Obj_ is TDataFrameVariant then
      Result := (TDataFrameVariant(Obj_).Buffer)
  else if Obj_ is TDataFrameInt64 then
      Result := (TDataFrameInt64(Obj_).Buffer)
  else if Obj_ is TDataFrameUInt64 then
      Result := (TDataFrameUInt64(Obj_).Buffer)
  else
      Result := 0;
end;

function TDataFrameEngine.ReadDouble(idx: Integer): Double;
var
  Obj_: TDataFrameBase;
begin
  Obj_ := Data[idx];
  if Obj_ is TDataFrameDouble then
      Result := TDataFrameDouble(Obj_).Buffer
  else if Obj_ is TDataFrameString then
      Result := umlStrToFloat(umlStringOf(TDataFrameString(Obj_).Buffer), 0)
  else if Obj_ is TDataFrameInteger then
      Result := TDataFrameInteger(Obj_).Buffer
  else if Obj_ is TDataFrameCardinal then
      Result := TDataFrameCardinal(Obj_).Buffer
  else if Obj_ is TDataFrameWord then
      Result := TDataFrameWord(Obj_).Buffer
  else if Obj_ is TDataFrameByte then
      Result := TDataFrameByte(Obj_).Buffer
  else if Obj_ is TDataFrameSingle then
      Result := TDataFrameSingle(Obj_).Buffer
  else if Obj_ is TDataFrameVariant then
      Result := (TDataFrameVariant(Obj_).Buffer)
  else if Obj_ is TDataFrameInt64 then
      Result := (TDataFrameInt64(Obj_).Buffer)
  else if Obj_ is TDataFrameUInt64 then
      Result := (TDataFrameUInt64(Obj_).Buffer)
  else
      Result := 0;
end;

function TDataFrameEngine.ReadArrayInteger(idx: Integer): TDataFrameArrayInteger;
var
  Obj_: TDataFrameBase;
begin
  Obj_ := Data[idx];
  if Obj_ is TDataFrameArrayInteger then
      Result := TDataFrameArrayInteger(Obj_)
  else
      Result := nil;
end;

function TDataFrameEngine.ReadArrayShortInt(idx: Integer): TDataFrameArrayShortInt;
var
  Obj_: TDataFrameBase;
begin
  Obj_ := Data[idx];
  if Obj_ is TDataFrameArrayShortInt then
      Result := TDataFrameArrayShortInt(Obj_)
  else
      Result := nil;
end;

function TDataFrameEngine.ReadArrayByte(idx: Integer): TDataFrameArrayByte;
var
  Obj_: TDataFrameBase;
begin
  Obj_ := Data[idx];
  if Obj_ is TDataFrameArrayByte then
      Result := TDataFrameArrayByte(Obj_)
  else
      Result := nil;
end;

function TDataFrameEngine.ReadMD5(idx: Integer): UnicodeMixedLib.TMD5;
var
  i: Integer;
begin
  with ReadArrayByte(idx) do
    for i := low(UnicodeMixedLib.TMD5) to high(UnicodeMixedLib.TMD5) do
        Result[i] := Buffer[i];
end;

function TDataFrameEngine.ReadArraySingle(idx: Integer): TDataFrameArraySingle;
var
  Obj_: TDataFrameBase;
begin
  Obj_ := Data[idx];
  if Obj_ is TDataFrameArraySingle then
      Result := TDataFrameArraySingle(Obj_)
  else
      Result := nil;
end;

function TDataFrameEngine.ReadArrayDouble(idx: Integer): TDataFrameArrayDouble;
var
  Obj_: TDataFrameBase;
begin
  Obj_ := Data[idx];
  if Obj_ is TDataFrameArrayDouble then
      Result := TDataFrameArrayDouble(Obj_)
  else
      Result := nil;
end;

function TDataFrameEngine.ReadArrayInt64(idx: Integer): TDataFrameArrayInt64;
var
  Obj_: TDataFrameBase;
begin
  Obj_ := Data[idx];
  if Obj_ is TDataFrameArrayInt64 then
      Result := TDataFrameArrayInt64(Obj_)
  else
      Result := nil;
end;

procedure TDataFrameEngine.ReadStream(idx: Integer; output: TCoreClassStream);
var
  Obj_: TDataFrameBase;
  AneedResetPos: Boolean;
begin
  Obj_ := Data[idx];
  AneedResetPos := output.Size = 0;
  if Obj_ is TDataFrameStream then
    begin
      with TDataFrameStream(Obj_) do
        begin
          Buffer.Position := 0;
          output.CopyFrom(Buffer, Buffer.Size);
          Buffer.Position := 0;
        end;
    end
  else if output is TMemoryStream64 then
      Obj_.SaveToStream(TMemoryStream64(output))
  else
      RaiseInfo('no support');
  if AneedResetPos then
      output.Position := 0;
end;

function TDataFrameEngine.ReadVariant(idx: Integer): Variant;
var
  Obj_: TDataFrameBase;
begin
  Obj_ := Data[idx];
  if Obj_ is TDataFrameVariant then
      Result := TDataFrameVariant(Obj_).Buffer
  else if Obj_ is TDataFrameString then
      Result := TDataFrameString(Obj_).Buffer
  else if Obj_ is TDataFrameInteger then
      Result := TDataFrameInteger(Obj_).Buffer
  else if Obj_ is TDataFrameCardinal then
      Result := TDataFrameCardinal(Obj_).Buffer
  else if Obj_ is TDataFrameWord then
      Result := TDataFrameWord(Obj_).Buffer
  else if Obj_ is TDataFrameByte then
      Result := TDataFrameByte(Obj_).Buffer
  else if Obj_ is TDataFrameSingle then
      Result := TDataFrameSingle(Obj_).Buffer
  else if Obj_ is TDataFrameDouble then
      Result := TDataFrameDouble(Obj_).Buffer
  else if Obj_ is TDataFrameInt64 then
      Result := (TDataFrameInt64(Obj_).Buffer)
  else
      Result := 0;
end;

function TDataFrameEngine.ReadInt64(idx: Integer): Int64;
var
  Obj_: TDataFrameBase;
begin
  Obj_ := Data[idx];
  if Obj_ is TDataFrameInt64 then
      Result := TDataFrameInt64(Obj_).Buffer
  else if Obj_ is TDataFrameUInt64 then
      Result := TDataFrameUInt64(Obj_).Buffer
  else if Obj_ is TDataFrameInteger then
      Result := TDataFrameInteger(Obj_).Buffer
  else if Obj_ is TDataFrameCardinal then
      Result := TDataFrameCardinal(Obj_).Buffer
  else if Obj_ is TDataFrameWord then
      Result := TDataFrameWord(Obj_).Buffer
  else if Obj_ is TDataFrameByte then
      Result := TDataFrameByte(Obj_).Buffer
  else if Obj_ is TDataFrameSingle then
      Result := Trunc(TDataFrameSingle(Obj_).Buffer)
  else if Obj_ is TDataFrameDouble then
      Result := Trunc(TDataFrameDouble(Obj_).Buffer)
  else if Obj_ is TDataFrameVariant then
      Result := TDataFrameVariant(Obj_).Buffer
  else
      Result := 0;
end;

function TDataFrameEngine.ReadUInt64(idx: Integer): UInt64;
var
  Obj_: TDataFrameBase;
begin
  Obj_ := Data[idx];
  if Obj_ is TDataFrameUInt64 then
      Result := TDataFrameUInt64(Obj_).Buffer
  else if Obj_ is TDataFrameInt64 then
      Result := TDataFrameInt64(Obj_).Buffer
  else if Obj_ is TDataFrameInteger then
      Result := TDataFrameInteger(Obj_).Buffer
  else if Obj_ is TDataFrameCardinal then
      Result := TDataFrameCardinal(Obj_).Buffer
  else if Obj_ is TDataFrameWord then
      Result := TDataFrameWord(Obj_).Buffer
  else if Obj_ is TDataFrameByte then
      Result := TDataFrameByte(Obj_).Buffer
  else if Obj_ is TDataFrameSingle then
      Result := Trunc(TDataFrameSingle(Obj_).Buffer)
  else if Obj_ is TDataFrameDouble then
      Result := Trunc(TDataFrameDouble(Obj_).Buffer)
  else if Obj_ is TDataFrameVariant then
      Result := TDataFrameVariant(Obj_).Buffer
  else
      Result := 0;
end;

procedure TDataFrameEngine.ReadStrings(idx: Integer; output: TCoreClassStrings);
var
  d: TMemoryStream64;
begin
  d := TMemoryStream64.Create;
  ReadStream(idx, d);
  d.Position := 0;

{$IFDEF FPC}
  output.LoadFromStream(d);
{$ELSE}
  output.LoadFromStream(d, TEncoding.UTF8);
{$ENDIF}
  DisposeObject(d);
end;

procedure TDataFrameEngine.ReadListStrings(idx: Integer; output: TListString);
var
  d: TMemoryStream64;
begin
  d := TMemoryStream64.Create;
  ReadStream(idx, d);
  d.Position := 0;

  output.LoadFromStream(d);
  DisposeObject(d);
end;

procedure TDataFrameEngine.ReadPascalStrings(idx: Integer; output: TListPascalString);
var
  d: TMemoryStream64;
begin
  d := TMemoryStream64.Create;
  ReadStream(idx, d);
  d.Position := 0;

  output.LoadFromStream(d);
  DisposeObject(d);
end;

procedure TDataFrameEngine.ReadDataFrame(idx: Integer; output: TDataFrameEngine);
var
  Obj_: TDataFrameBase;
  d: TMemoryStream64;
begin
  Obj_ := Data[idx];
  if Obj_ is TDataFrameStream then
    begin
      TDataFrameStream(Obj_).Buffer.Position := 0;
      output.DecodeFrom(TDataFrameStream(Obj_).Buffer, True);
      TDataFrameStream(Obj_).Buffer.Position := 0;
    end
  else
    begin
      d := TMemoryStream64.Create;
      ReadStream(idx, d);
      d.Position := 0;
      output.DecodeFrom(d, True);
      DisposeObject(d);
    end;
end;

procedure TDataFrameEngine.ReadHashStringList(idx: Integer; output: THashStringList);
var
  d: TMemoryStream64;
  h: THashStringTextStream;
begin
  d := TMemoryStream64.Create;
  ReadStream(idx, d);
  d.Position := 0;
  h := THashStringTextStream.Create(output);
  h.LoadFromStream(d);
  DisposeObject(h);
  DisposeObject(d);
end;

procedure TDataFrameEngine.ReadVariantList(idx: Integer; output: THashVariantList);
var
  d: TMemoryStream64;
  h: THashVariantTextStream;
begin
  d := TMemoryStream64.Create;
  ReadStream(idx, d);
  d.Position := 0;
  h := THashVariantTextStream.Create(output);
  h.LoadFromStream(d);
  DisposeObject(h);
  DisposeObject(d);
end;

procedure TDataFrameEngine.ReadSectionText(idx: Integer; output: TSectionTextData);
var
  d: TMemoryStream64;
begin
  d := TMemoryStream64.Create;
  ReadStream(idx, d);
  d.Position := 0;
  output.LoadFromStream(d);
  DisposeObject(d);
end;

procedure TDataFrameEngine.ReadTextSection(idx: Integer; output: TSectionTextData);
begin
  ReadSectionText(idx, output);
end;

{$IFNDEF FPC}


procedure TDataFrameEngine.ReadJson(idx: Integer; output: TJsonObject);
var
  d: TMemoryStream64;
begin
  d := TMemoryStream64.Create;
  ReadStream(idx, d);
  d.Position := 0;
  output.LoadFromStream(d, TEncoding.UTF8, False);
  DisposeObject(d);
end;
{$ENDIF}


function TDataFrameEngine.ReadRect(idx: Integer): TRect;
begin
  with ReadArrayInteger(idx) do
    begin
      Result := Rect(Buffer[0], Buffer[1], Buffer[2], Buffer[3]);
    end;
end;

function TDataFrameEngine.ReadRectf(idx: Integer): TRectf;
begin
  with ReadArraySingle(idx) do
    begin
      Result := Rectf(Buffer[0], Buffer[1], Buffer[2], Buffer[3]);
    end;
end;

function TDataFrameEngine.ReadPoint(idx: Integer): TPoint;
begin
  with ReadArrayInteger(idx) do
    begin
      Result := Point(Buffer[0], Buffer[1]);
    end;
end;

function TDataFrameEngine.ReadPointf(idx: Integer): TPointf;
begin
  with ReadArraySingle(idx) do
    begin
      Result := Pointf(Buffer[0], Buffer[1]);
    end;
end;

function TDataFrameEngine.ReadVector(idx: Integer): TVector;
begin
  with ReadArraySingle(idx) do
    begin
      Result[0] := Buffer[0];
      Result[1] := Buffer[1];
      Result[2] := Buffer[2];
      Result[3] := Buffer[3];
    end;
end;

function TDataFrameEngine.ReadAffineVector(idx: Integer): TAffineVector;
begin
  with ReadArraySingle(idx) do
    begin
      Result[0] := Buffer[0];
      Result[1] := Buffer[1];
      Result[2] := Buffer[2];
    end;
end;

function TDataFrameEngine.ReadVec3(idx: Integer): TVec3;
begin
  with ReadArraySingle(idx) do
    begin
      Result[0] := Buffer[0];
      Result[1] := Buffer[1];
      Result[2] := Buffer[2];
    end;
end;

function TDataFrameEngine.ReadVec4(idx: Integer): TVec4;
begin
  with ReadArraySingle(idx) do
    begin
      Result[0] := Buffer[0];
      Result[1] := Buffer[1];
      Result[2] := Buffer[2];
      Result[3] := Buffer[3];
    end;
end;

function TDataFrameEngine.ReadVector3(idx: Integer): TVector3;
begin
  with ReadArraySingle(idx) do
    begin
      Result := Vector3(Buffer[0], Buffer[1], Buffer[2]);
    end;
end;

function TDataFrameEngine.ReadVector4(idx: Integer): TVector4;
begin
  with ReadArraySingle(idx) do
    begin
      Result := Vector4(Buffer[0], Buffer[1], Buffer[2], Buffer[3]);
    end;
end;

function TDataFrameEngine.ReadMat4(idx: Integer): TMat4;
var
  i, j: Integer;
begin
  with ReadArraySingle(idx) do
    begin
      for i := 0 to 3 do
        for j := 0 to 3 do
            Result[i][j] := Buffer[i * 4 + j];
    end;
end;

function TDataFrameEngine.ReadMatrix4(idx: Integer): TMatrix4;
begin
  Result.Link := ReadMat4(idx);
end;

function TDataFrameEngine.Read2DPoint(idx: Integer): T2DPoint;
begin
  with ReadArraySingle(idx) do
    begin
      Result[0] := Buffer[0];
      Result[1] := Buffer[1];
    end;
end;

function TDataFrameEngine.ReadVec2(idx: Integer): TVec2;
begin
  Result := Read2DPoint(idx);
end;

function TDataFrameEngine.ReadRectV2(idx: Integer): TRectV2;
begin
  with ReadArraySingle(idx) do
    begin
      Result[0][0] := Buffer[0];
      Result[0][1] := Buffer[1];
      Result[1][0] := Buffer[2];
      Result[1][1] := Buffer[3];
    end;
end;

function TDataFrameEngine.ReadPointer(idx: Integer): UInt64;
begin
  Result := ReadUInt64(idx);
end;

procedure TDataFrameEngine.read(idx: Integer; var aBuf; aCount: Int64);
var
  s: TMemoryStream64;
begin
  s := TMemoryStream64.Create;
  ReadStream(idx, s);
  s.Read64(aBuf, aCount);
  DisposeObject(s);
end;

function TDataFrameEngine.ComputeEncodeSize: Integer;
var
  i: Integer;
begin
  Result := C_Integer_Size;
  for i := 0 to Count - 1 do
      Result := Result + C_Byte_Size + GetData(i).ComputeEncodeSize;
end;

class procedure TDataFrameEngine.BuildEmptyStream(output: TCoreClassStream);
var
  editionToken: Byte;
  sizeInfo: Integer;
  compToken: Byte;
  md5: UnicodeMixedLib.TMD5;
  cnt: Integer;
begin
  // make header
  editionToken := $FF;
  sizeInfo := C_Integer_Size;
  compToken := 0;
  md5 := NullMD5;
  cnt := 0;

  output.write(editionToken, C_Byte_Size);
  output.write(sizeInfo, C_Integer_Size);
  output.write(compToken, C_Byte_Size);
  output.write(md5[0], C_MD5_Size);
  output.write(cnt, C_Integer_Size);
end;

function TDataFrameEngine.EncodeTo(output: TCoreClassStream; const FastMode: Boolean): Integer;
var
  i: Integer;
  b: TDataFrameBase;
  StoreStream, nStream: TMemoryStream64;
  ID: Byte;

  editionToken: Byte;
  sizeInfo: Integer;
  compToken: Byte;
  md5: UnicodeMixedLib.TMD5;
begin
  Result := Count;

  if Result = 0 then
    begin
      BuildEmptyStream(output);
      Exit;
    end;

  StoreStream := TMemoryStream64.Create;

  // make body
  StoreStream.Write64(Result, C_Integer_Size);

  nStream := TMemoryStream64.Create;
  for i := 0 to Count - 1 do
    begin
      b := GetData(i);
      ID := b.FID;
      b.SaveToStream(nStream);

      StoreStream.Write64(ID, C_Byte_Size);
      nStream.Position := 0;
      StoreStream.CopyFrom(nStream, nStream.Size);
      nStream.Clear;
    end;

  // make header
  editionToken := $FF;
  sizeInfo := StoreStream.Size;
  compToken := 0;
  StoreStream.Position := 0;
  if FastMode then
      md5 := NullMD5
  else
      md5 := umlMD5(StoreStream.Memory, StoreStream.Size);

  nStream.Clear;
  nStream.write(editionToken, C_Byte_Size);
  nStream.write(sizeInfo, C_Integer_Size);
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

function TDataFrameEngine.EncodeTo(output: TCoreClassStream): Integer;
begin
  Result := EncodeTo(output, False);
end;

procedure TDataFrameEngine.Encrypt(output: TCoreClassStream; Compressed_: Boolean; SecurityLevel: Integer; Key: TCipherKeyBuffer);
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  if Compressed_ then
      EncodeAsSelectCompress(m64, True)
  else
      EncodeTo(m64, True);

  QuantumEncrypt(m64, output, SecurityLevel, Key);
  DisposeObject(m64);
end;

function TDataFrameEngine.Decrypt(input: TCoreClassStream; Key: TCipherKeyBuffer): Boolean;
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  Result := QuantumDecrypt(input, m64, Key);
  if Result then
    begin
      m64.Position := 0;
      DecodeFrom(m64, True);
    end;

  DisposeObject(m64);
end;

{$IFNDEF FPC}


procedure TDataFrameEngine.EncodeAsPublicJson(output: TCoreClassStream);
var
  j: TJsonObject;
  i: Integer;
begin
  j := TJsonObject.Create;
  j.s['help'] := 'This JSON with TDataFrameEngine encode';

  for i := 0 to Count - 1 do
    begin
      j.a['Ref'].Add(TDataFrameBase(FDataList[i]).FID);
      TDataFrameBase(FDataList[i]).SaveToJson(j.a['Data'], i);
    end;

  j.SaveToStream(output, False, TEncoding.UTF8, True);

  DisposeObject(j);
end;

procedure TDataFrameEngine.EncodeAsJson(output: TCoreClassStream);
var
  j: TJsonObject;
  i: Integer;
  b: TDataFrameBase;
begin
  j := TJsonObject.Create;

  for i := 0 to Count - 1 do
    begin
      b := TDataFrameBase(FDataList[i]);
      b.SaveToJson(j.a['Data'], i);
      j.a['Ref'].Add(b.FID);
    end;

  j.SaveToStream(output, True, TEncoding.UTF8, True);

  DisposeObject(j);
end;

procedure TDataFrameEngine.DecodeFromJson(stream: TCoreClassStream);
var
  j: TJsonObject;
  t: Byte;
  i: Integer;
  b: TDataFrameBase;
begin
  Clear;
  j := TJsonObject.Create;
  try
      j.LoadFromStream(stream, TEncoding.UTF8, True);
  except
    DisposeObject(j);
    Exit;
  end;

  try
    for i := 0 to j.a['Ref'].Count - 1 do
      begin
        t := j.a['Ref'].i[i];
        b := AddData(ByteToDataType(t));
        b.LoadFromJson(j.a['Data'], i);
      end;
  except
    DisposeObject(j);
    Exit;
  end;

  DisposeObject(j);
end;
{$ENDIF}


function TDataFrameEngine.EncodeAsSelectCompress(scm: TSelectCompressionMethod; output: TCoreClassStream; const FastMode: Boolean): Integer;
var
  i: Integer;
  b: TDataFrameBase;
  StoreStream, nStream, compStream: TMemoryStream64;
  ID: Byte;

  editionToken: Byte;
  sizeInfo: Integer;
  compToken: Byte;
  compSizeInfo: Integer;
  md5: UnicodeMixedLib.TMD5;
begin
  Result := Count;

  if Result = 0 then
    begin
      BuildEmptyStream(output);
      Exit;
    end;

  StoreStream := TMemoryStream64.Create;

  // make body
  StoreStream.Write64(Result, C_Integer_Size);

  nStream := TMemoryStream64.Create;
  for i := 0 to Count - 1 do
    begin
      b := GetData(i);
      ID := b.FID;
      b.SaveToStream(nStream);

      StoreStream.Write64(ID, C_Byte_Size);
      nStream.Position := 0;
      StoreStream.CopyFrom(nStream, nStream.Size);
      nStream.Clear;
    end;

  // compress body and make header
  compSizeInfo := StoreStream.Size;
  StoreStream.Position := 0;
  if FastMode then
      md5 := NullMD5
  else
      md5 := umlMD5(StoreStream.Memory, StoreStream.Size);

  compStream := TMemoryStream64.CustomCreate($FFFF);
  ParallelCompressStream(scm, StoreStream, compStream);
  DisposeObject(StoreStream);

  editionToken := $FF;
  sizeInfo := compStream.Size;
  compToken := 4;

  nStream.Clear;
  nStream.write(editionToken, C_Byte_Size);
  nStream.write(sizeInfo, C_Integer_Size);
  nStream.write(compToken, C_Byte_Size);
  nStream.write(compSizeInfo, C_Integer_Size);
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

function TDataFrameEngine.EncodeAsSelectCompress(output: TCoreClassStream; const FastMode: Boolean): Integer;
var
  scm: TSelectCompressionMethod;
begin
  if ComputeEncodeSize > 64 * 1024 then
    begin
      if FastMode then
          scm := TSelectCompressionMethod.scmZLIB
      else
          scm := TSelectCompressionMethod.scmZLIB_Max;
      Result := EncodeAsSelectCompress(scm, output, FastMode);
    end
  else
      Result := EncodeAsZLib(output, FastMode);
end;

function TDataFrameEngine.EncodeAsSelectCompress(output: TCoreClassStream): Integer;
begin
  Result := EncodeAsSelectCompress(output, False);
end;

function TDataFrameEngine.EncodeAsZLib(output: TCoreClassStream; const FastMode: Boolean): Integer;
var
  i: Integer;
  b: TDataFrameBase;
  StoreStream, nStream, compStream: TMemoryStream64;
  ZCompStream: TCompressionStream;
  ID: Byte;

  editionToken: Byte;
  sizeInfo: Integer;
  compToken: Byte;
  compSizeInfo: Integer;
  md5: UnicodeMixedLib.TMD5;
begin
  Result := Count;

  if Result = 0 then
    begin
      BuildEmptyStream(output);
      Exit;
    end;

  StoreStream := TMemoryStream64.Create;

  // make body
  StoreStream.Write64(Result, C_Integer_Size);

  nStream := TMemoryStream64.Create;
  for i := 0 to Count - 1 do
    begin
      b := GetData(i);
      ID := b.FID;
      b.SaveToStream(nStream);

      StoreStream.Write64(ID, C_Byte_Size);
      nStream.Position := 0;
      StoreStream.CopyFrom(nStream, nStream.Size);
      nStream.Clear;
    end;

  // compress body and make header
  compSizeInfo := StoreStream.Size;
  StoreStream.Position := 0;
  if FastMode then
      md5 := NullMD5
  else
      md5 := umlMD5(StoreStream.Memory, StoreStream.Size);

  compStream := TMemoryStream64.CustomCreate($FFFF);
  ZCompStream := TCompressionStream.Create(compStream);
  StoreStream.Position := 0;
  ZCompStream.CopyFrom(StoreStream, StoreStream.Size);
  DisposeObject(ZCompStream);
  DisposeObject(StoreStream);

  editionToken := $FF;
  sizeInfo := compStream.Size;
  compToken := 1;

  nStream.Clear;
  nStream.write(editionToken, C_Byte_Size);
  nStream.write(sizeInfo, C_Integer_Size);
  nStream.write(compToken, C_Byte_Size);
  nStream.write(compSizeInfo, C_Integer_Size);
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

function TDataFrameEngine.EncodeAsZLib(output: TCoreClassStream): Integer;
begin
  Result := EncodeAsZLib(output, False);
end;

function TDataFrameEngine.EncodeAsDeflate(output: TCoreClassStream; const FastMode: Boolean): Integer;
var
  i: Integer;
  b: TDataFrameBase;
  StoreStream, nStream, compStream: TMemoryStream64;
  ID: Byte;

  editionToken: Byte;
  sizeInfo: Integer;
  compToken: Byte;
  compSizeInfo: Integer;
  md5: UnicodeMixedLib.TMD5;
begin
  Result := Count;

  if Result = 0 then
    begin
      BuildEmptyStream(output);
      Exit;
    end;

  StoreStream := TMemoryStream64.Create;

  // make body
  StoreStream.Write64(Result, C_Integer_Size);

  nStream := TMemoryStream64.Create;
  for i := 0 to Count - 1 do
    begin
      b := GetData(i);
      ID := b.FID;
      b.SaveToStream(nStream);

      StoreStream.Write64(ID, C_Byte_Size);
      nStream.Position := 0;
      StoreStream.CopyFrom(nStream, nStream.Size);
      nStream.Clear;
    end;

  // compress body and make header
  compSizeInfo := StoreStream.Size;
  StoreStream.Position := 0;
  if FastMode then
      md5 := NullMD5
  else
      md5 := umlMD5(StoreStream.Memory, StoreStream.Size);

  compStream := TMemoryStream64.Create;
  StoreStream.Position := 0;

  if FCompressorDeflate = nil then
      FCompressorDeflate := TCompressorDeflate.Create;

  CoreCompressStream(FCompressorDeflate, StoreStream, compStream);
  DisposeObject(StoreStream);

  editionToken := $FF;
  sizeInfo := compStream.Size;
  compToken := 2;

  nStream.Clear;
  nStream.write(editionToken, C_Byte_Size);
  nStream.write(sizeInfo, C_Integer_Size);
  nStream.write(compToken, C_Byte_Size);
  nStream.write(compSizeInfo, C_Integer_Size);
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

function TDataFrameEngine.EncodeAsDeflate(output: TCoreClassStream): Integer;
begin
  Result := EncodeAsDeflate(output, False);
end;

function TDataFrameEngine.EncodeAsBRRC(output: TCoreClassStream; const FastMode: Boolean): Integer;
var
  i: Integer;
  b: TDataFrameBase;
  StoreStream, nStream, compStream: TMemoryStream64;
  ID: Byte;

  editionToken: Byte;
  sizeInfo: Integer;
  compToken: Byte;
  compSizeInfo: Integer;
  md5: UnicodeMixedLib.TMD5;
begin
  Result := Count;

  if Result = 0 then
    begin
      BuildEmptyStream(output);
      Exit;
    end;

  StoreStream := TMemoryStream64.Create;

  // make body
  StoreStream.Write64(Result, C_Integer_Size);

  nStream := TMemoryStream64.Create;
  for i := 0 to Count - 1 do
    begin
      b := GetData(i);
      ID := b.FID;
      b.SaveToStream(nStream);

      StoreStream.Write64(ID, C_Byte_Size);
      nStream.Position := 0;
      StoreStream.CopyFrom(nStream, nStream.Size);
      nStream.Clear;
    end;

  // compress body and make header
  compSizeInfo := StoreStream.Size;
  StoreStream.Position := 0;
  if FastMode then
      md5 := NullMD5
  else
      md5 := umlMD5(StoreStream.Memory, StoreStream.Size);

  compStream := TMemoryStream64.Create;
  StoreStream.Position := 0;

  if FCompressorBRRC = nil then
      FCompressorBRRC := TCompressorBRRC.Create;

  CoreCompressStream(FCompressorBRRC, StoreStream, compStream);
  DisposeObject(StoreStream);

  editionToken := $FF;
  sizeInfo := compStream.Size;
  compToken := 3;

  nStream.Clear;
  nStream.write(editionToken, C_Byte_Size);
  nStream.write(sizeInfo, C_Integer_Size);
  nStream.write(compToken, C_Byte_Size);
  nStream.write(compSizeInfo, C_Integer_Size);
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

function TDataFrameEngine.EncodeAsBRRC(output: TCoreClassStream): Integer;
begin
  Result := EncodeAsBRRC(output, False);
end;

function TDataFrameEngine.IsCompressed(Source: TCoreClassStream): Boolean;
var
  bakPos: Int64;

  editionToken: Byte;
  sizeInfo: Integer;
  compToken: Byte;
begin
  bakPos := Source.Position;
  Result := False;

  Source.read(editionToken, C_Byte_Size);
  if editionToken = $FF then
    begin
      Source.read(sizeInfo, C_Integer_Size);
      Source.read(compToken, C_Byte_Size);

      Result := compToken in [1, 2, 3, 4];
    end;

  Source.Position := bakPos;
end;

function TDataFrameEngine.DecodeFrom(Source: TCoreClassStream; const FastMode: Boolean): Integer;
var
  i, cnt: Integer;
  ID: Byte;
  StoreStream: TMemoryStream64;
  ZDecompStream: TDecompressionStream;
  b: TDataFrameBase;

  editionToken: Byte;
  sizeInfo: Integer;
  compToken: Byte;
  compSizeInfo: Integer;
  md5: UnicodeMixedLib.TMD5;
begin
  Clear;

  Result := -1;

  StoreStream := TMemoryStream64.Create;

  Source.read(editionToken, C_Byte_Size);
  if editionToken = $FF then
    begin
      Source.read(sizeInfo, C_Integer_Size);
      Source.read(compToken, C_Byte_Size);
      if compToken = 0 then
        begin
          Source.read(md5[0], 16);

          if Source is TMemoryStream64 then
              StoreStream.SetPointerWithProtectedMode(TMemoryStream64(Source).PositionAsPtr, sizeInfo)
          else
              StoreStream.CopyFrom(Source, sizeInfo);

          StoreStream.Position := 0;
          if (not FastMode) and (not umlIsNullMD5(md5)) then
            if not umlMD5Compare(umlMD5(StoreStream.Memory, StoreStream.Size), md5) then
              begin
                DoStatus('dataframe md5 error!');
                DisposeObject(StoreStream);
                Exit;
              end;
        end
      else if compToken = 1 then
        begin
          Source.read(compSizeInfo, C_Integer_Size);
          Source.read(md5[0], 16);

          ZDecompStream := TDecompressionStream.Create(Source);
          StoreStream.CopyFrom(ZDecompStream, compSizeInfo);
          DisposeObject(ZDecompStream);

          StoreStream.Position := 0;
          if (not FastMode) and (not umlIsNullMD5(md5)) then
            if not umlMD5Compare(umlMD5(StoreStream.Memory, StoreStream.Size), md5) then
              begin
                DoStatus('ZLib md5 error!');
                DisposeObject(StoreStream);
                Exit;
              end;
        end
      else if compToken = 2 then
        begin
          Source.read(compSizeInfo, C_Integer_Size);
          Source.read(md5[0], 16);

          if FCompressorDeflate = nil then
              FCompressorDeflate := TCompressorDeflate.Create;
          CoreDecompressStream(FCompressorDeflate, Source, StoreStream);

          StoreStream.Position := 0;
          if (not FastMode) and (not umlIsNullMD5(md5)) then
            if not umlMD5Compare(umlMD5(StoreStream.Memory, StoreStream.Size), md5) then
              begin
                DoStatus('Deflate md5 error!');
                DisposeObject(StoreStream);
                Exit;
              end;
        end
      else if compToken = 3 then
        begin
          Source.read(compSizeInfo, C_Integer_Size);
          Source.read(md5[0], 16);

          if FCompressorBRRC = nil then
              FCompressorBRRC := TCompressorBRRC.Create;
          CoreDecompressStream(FCompressorBRRC, Source, StoreStream);

          StoreStream.Position := 0;
          if (not FastMode) and (not umlIsNullMD5(md5)) then
            if not umlMD5Compare(umlMD5(StoreStream.Memory, StoreStream.Size), md5) then
              begin
                DoStatus('BRRC md5 error!');
                DisposeObject(StoreStream);
                Exit;
              end;
        end
      else if compToken = 4 then
        begin
          Source.read(compSizeInfo, C_Integer_Size);
          Source.read(md5[0], 16);

          ParallelDecompressStream(Source, StoreStream);

          StoreStream.Position := 0;
          if (not FastMode) and (not umlIsNullMD5(md5)) then
            if not umlMD5Compare(umlMD5(StoreStream.Memory, StoreStream.Size), md5) then
              begin
                DoStatus('select compression md5 error!');
                DisposeObject(StoreStream);
                Exit;
              end;
        end;

      StoreStream.Position := 0;

      StoreStream.Read64(cnt, C_Integer_Size);
      for i := 0 to cnt - 1 do
        begin
          StoreStream.Read64(ID, C_Byte_Size);
          b := AddData(ByteToDataType(ID));
          b.LoadFromStream(StoreStream);
        end;
      DisposeObject(StoreStream);
      Result := cnt;
    end
  else
    begin
      DoStatus('dataframe decode error!');
      DisposeObject(StoreStream);
      Exit;
    end;
end;

function TDataFrameEngine.DecodeFrom(Source: TCoreClassStream): Integer;
begin
  Result := DecodeFrom(Source, False);
end;

procedure TDataFrameEngine.EncodeToBytes(const Compressed, FastMode: Boolean; var output: TBytes);
var
  enStream: TMemoryStream64;
begin
  enStream := TMemoryStream64.Create;
  if Compressed then
      EncodeAsSelectCompress(enStream, FastMode)
  else
      EncodeTo(enStream, FastMode);

  SetLength(output, enStream.Size);
  CopyPtr(enStream.Memory, @output[0], enStream.Size);
  DisposeObject(enStream);
end;

procedure TDataFrameEngine.DecodeFromBytes(var b: TBytes);
begin
  DecodeFromBytes(b, False);
end;

procedure TDataFrameEngine.DecodeFromBytes(var b: TBytes; const FastMode: Boolean);
var
  enStream: TMemoryStream64;
begin
  enStream := TMemoryStream64.Create;
  enStream.SetPointerWithProtectedMode(@b[0], length(b));
  DecodeFrom(enStream, FastMode);
  DisposeObject(enStream);
end;

function TDataFrameEngine.GetMD5(const FastMode: Boolean): UnicodeMixedLib.TMD5;
var
  enStream: TMemoryStream64;
begin
  enStream := TMemoryStream64.Create;
  EncodeTo(enStream, FastMode);

  Result := umlMD5(enStream.Memory, enStream.Size);
  DisposeObject(enStream);
end;

function TDataFrameEngine.Compare(dest: TDataFrameEngine): Boolean;
var
  m1, m2: UnicodeMixedLib.TMD5;
  i: Integer;
begin
  Result := False;

  // fast prepare compare
  if Count <> dest.Count then
      Exit;
  for i := 0 to Count - 1 do
    if FDataList[i].ClassType <> dest[i].ClassType then
        Exit;
  if ComputeEncodeSize <> dest.ComputeEncodeSize then
      Exit;

  // data compare
  m1 := GetMD5(False);
  m2 := dest.GetMD5(False);
  Result := umlMD5Compare(m1, m2);
end;

procedure TDataFrameEngine.LoadFromStream(stream: TCoreClassStream);
begin
  try
      DecodeFrom(stream);
  except
  end;
end;

procedure TDataFrameEngine.SaveToStream(stream: TCoreClassStream);
var
  siz: Integer;
begin
  try
    siz := ComputeEncodeSize;
    if siz > 1024 then
        EncodeAsSelectCompress(stream)
    else
        EncodeTo(stream);
  except
  end;
end;

constructor TDataWriter.Create(AStream: TCoreClassStream);
begin
  inherited Create;
  FEngine := TDataFrameEngine.Create;
  FStream := AStream;
end;

destructor TDataWriter.Destroy;
var
  FlagCompressed: Boolean;
  verflag: TBytes;
  Len: Int64;
  M: TMemoryStream64;
begin
  if FStream <> nil then
    begin
      M := TMemoryStream64.Create;
      FEngine.EncodeTo(M);
      Len := M.Size;

      // write version flag
      verflag := umlBytesOf('0001');
      FStream.write(verflag, 4);

      // write compressed flag
      FlagCompressed := False;
      FStream.write(FlagCompressed, C_Boolean_Size);

      // write length info
      FStream.write(Len, C_Int64_Size);

      // write buffer
      M.Position := 0;
      FStream.CopyFrom(M, Len);
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

procedure TDataWriter.WriteBytes(v: TBytes);
begin
  FEngine.WriteBytes(v);
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

procedure TDataWriter.WriteDataFrame(v: TDataFrameEngine);
begin
  FEngine.WriteDataFrame(v);
end;

procedure TDataWriter.WriteDataFrameCompressed(v: TDataFrameEngine);
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

{$IFNDEF FPC}


procedure TDataWriter.WriteJson(v: TJsonObject);
begin
  FEngine.WriteJson(v);
end;
{$ENDIF}


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

procedure TDataWriter.write(const aBuf; aCount: Int64);
begin
  FEngine.write(aBuf, aCount);
end;

constructor TDataReader.Create(AStream: TCoreClassStream);
var
  verflag: TBytes;
  FlagCompressed: Boolean;
  Len: Int64;
  M: TMemoryStream64;
begin
  inherited Create;
  FEngine := TDataFrameEngine.Create;
  if AStream <> nil then
    begin
      // read version flag
      SetLength(verflag, 4);
      AStream.read(verflag, 4);
      if umlStringOf(verflag) <> '0001' then
          raise Exception.Create('Version flag Does not match!');

      // read compressed flag
      AStream.read(FlagCompressed, C_Boolean_Size);

      // read length info
      AStream.read(Len, C_Int64_Size);

      // write buffer
      M := TMemoryStream64.Create;
      M.CopyFrom(AStream, Len);
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

function TDataReader.ReadBytes: TBytes;
begin
  Result := FEngine.Reader.ReadBytes;
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
  rb: TDataFrameArrayInteger;
begin
  rb := FEngine.Reader.ReadArrayInteger;
  for i := low(Data) to high(Data) do
      Data[i] := rb[i];
end;

procedure TDataReader.ReadArrayShortInt(var Data: array of ShortInt);
var
  i: Integer;
  rb: TDataFrameArrayShortInt;
begin
  rb := FEngine.Reader.ReadArrayShortInt;
  for i := low(Data) to high(Data) do
      Data[i] := rb[i];
end;

procedure TDataReader.ReadArrayByte(var Data: array of Byte);
var
  i: Integer;
  rb: TDataFrameArrayByte;
begin
  rb := FEngine.Reader.ReadArrayByte;
  for i := low(Data) to high(Data) do
      Data[i] := rb[i];
end;

procedure TDataReader.ReadArraySingle(var Data: array of Single);
var
  i: Integer;
  rb: TDataFrameArraySingle;
begin
  rb := FEngine.Reader.ReadArraySingle;
  for i := low(Data) to high(Data) do
      Data[i] := rb[i];
end;

procedure TDataReader.ReadArrayDouble(var Data: array of Double);
var
  i: Integer;
  rb: TDataFrameArrayDouble;
begin
  rb := FEngine.Reader.ReadArrayDouble;
  for i := low(Data) to high(Data) do
      Data[i] := rb[i];
end;

procedure TDataReader.ReadArrayInt64(var Data: array of Int64);
var
  i: Integer;
  rb: TDataFrameArrayInt64;
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

procedure TDataReader.ReadDataFrame(output: TDataFrameEngine);
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

{$IFNDEF FPC}


procedure TDataReader.ReadJson(output: TJsonObject);
begin
  FEngine.Reader.ReadJson(output);
end;
{$ENDIF}


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

procedure TDataReader.read(var aBuf; aCount: Int64);
begin
  FEngine.Reader.read(aBuf, aCount);
end;

end.
