{ ****************************************************************************** }
{ * Data Struct Engine                                                         * }
{ * written by QQ 600585@qq.com                                                * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
(* https://github.com/PassByYou888/ZServer4D *)
{ ****************************************************************************** }
(*
  update history
  2017-12-6
  added supported pointer
*)

unit DataFrameEngine;

{$I zDefine.inc}

interface

uses SysUtils, CoreClasses, Types, Variants,
  ListEngine, MemoryStream64,
  GeometryLib, TextDataEngine, Geometry2DUnit, Geometry3DUnit,

  {$IFNDEF FPC}
  JsonDataObjects,
  {$ENDIF}
  CoreCompress,
  UnicodeMixedLib, PascalStrings;

type
  TDataFrameBase = class(TCoreClassObject)
  protected
    FID: Byte; // data frame only id
  public
    constructor Create(id: Byte);
    destructor Destroy; override;

    procedure LoadFromStream(stream: TMemoryStream64); virtual; abstract;
    procedure SaveToStream(stream: TMemoryStream64); virtual; abstract;
    {$IFNDEF FPC}
    procedure LoadFromJson(jarry: TJsonArray; idx: Integer); virtual; abstract;
    procedure SaveToJson(jarry: TJsonArray; idx: Integer); virtual; abstract;
    {$ENDIF}
    function ComputeEncodeSize: Integer; virtual; abstract;
  end;

  TDataFrameString = class(TDataFrameBase)
  private
  protected
    FBuffer: SystemString;
  public
    constructor Create(id: Byte);
    destructor Destroy; override;

    procedure LoadFromStream(stream: TMemoryStream64); override;
    procedure SaveToStream(stream: TMemoryStream64); override;
    {$IFNDEF FPC}
    procedure LoadFromJson(jarry: TJsonArray; idx: Integer); override;
    procedure SaveToJson(jarry: TJsonArray; idx: Integer); override;
    {$ENDIF}
    function ComputeEncodeSize: Integer; override;

    property Buffer: SystemString read FBuffer write FBuffer;
  end;

  TDataFrameInteger = class(TDataFrameBase)
  private
  protected
    FBuffer: Integer;
  public
    constructor Create(id: Byte);
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

  TDataFrameCardinal = class(TDataFrameBase)
  private
  protected
    FBuffer: Cardinal;
  public
    constructor Create(id: Byte);
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

  TDataFrameWord = class(TDataFrameBase)
  private
  protected
    FBuffer: Word;
  public
    constructor Create(id: Byte);
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

  TDataFrameByte = class(TDataFrameBase)
  private
  protected
    FBuffer: Byte;
  public
    constructor Create(id: Byte);
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

  TDataFrameSingle = class(TDataFrameBase)
  private
  protected
    FBuffer: Single;
  public
    constructor Create(id: Byte);
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

  TDataFrameDouble = class(TDataFrameBase)
  private
  protected
    FBuffer: Double;
  public
    constructor Create(id: Byte);
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

  TDataFrameArrayInteger = class(TDataFrameBase)
  private
  protected
    FBuffer: TCoreClassList;

    function GetBuffer(idx: Integer): Integer;
    procedure SetBuffer(idx: Integer; Value: Integer);

  public
    constructor Create(id: Byte);
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

    property Buffer[idx: Integer]: Integer read GetBuffer write SetBuffer; default;
  end;

  TDataFrameArrayShortInt = class(TDataFrameBase)
  private
  protected
    FBuffer: TCoreClassList;

    function GetBuffer(idx: Integer): ShortInt;
    procedure SetBuffer(idx: Integer; Value: ShortInt);

  public
    constructor Create(id: ShortInt);
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

    property Buffer[idx: Integer]: ShortInt read GetBuffer write SetBuffer; default;
  end;

  TDataFrameArrayByte = class(TDataFrameBase)
  private
  protected
    FBuffer: TCoreClassList;

    function GetBuffer(idx: Integer): Byte;
    procedure SetBuffer(idx: Integer; Value: Byte);

  public
    constructor Create(id: Byte);
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

    property Buffer[idx: Integer]: Byte read GetBuffer write SetBuffer; default;
  end;

  TDataFrameArraySingle = class(TDataFrameBase)
  private
  protected
    FBuffer: TCoreClassList;

    function GetBuffer(idx: Integer): Single;
    procedure SetBuffer(idx: Integer; Value: Single);

  public
    constructor Create(id: Byte);
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

    property Buffer[idx: Integer]: Single read GetBuffer write SetBuffer; default;
  end;

  TDataFrameArrayDouble = class(TDataFrameBase)
  private
  protected
    FBuffer: TCoreClassList;

    function GetBuffer(idx: Integer): Double;
    procedure SetBuffer(idx: Integer; Value: Double);

  public
    constructor Create(id: Byte);
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

    property Buffer[idx: Integer]: Double read GetBuffer write SetBuffer; default;
  end;

  TDataFrameArrayInt64 = class(TDataFrameBase)
  private
  protected
    FBuffer: TCoreClassList;

    function GetBuffer(idx: Integer): Int64;
    procedure SetBuffer(idx: Integer; Value: Int64);

  public
    constructor Create(id: Byte);
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

    property Buffer[idx: Integer]: Int64 read GetBuffer write SetBuffer; default;
  end;

  TDataFrameStream = class(TDataFrameBase)
  private
  protected
    FBuffer: TMemoryStream64;
    function GetBuffer: TCoreClassStream;
    procedure SetBuffer(_Buffer: TCoreClassStream);
  public
    constructor Create(id: Byte);
    destructor Destroy; override;

    procedure Clear;

    procedure LoadFromStream(stream: TMemoryStream64); override;
    procedure SaveToStream(stream: TMemoryStream64); override;
    {$IFNDEF FPC}
    procedure LoadFromJson(jarry: TJsonArray; idx: Integer); override;
    procedure SaveToJson(jarry: TJsonArray; idx: Integer); override;
    {$ENDIF}
    function ComputeEncodeSize: Integer; override;

    property Buffer: TCoreClassStream read GetBuffer write SetBuffer;
  end;

  TDataFrameVariant = class(TDataFrameBase)
  private
  protected
    FBuffer: Variant;
  public
    constructor Create(id: Byte);
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

  TDataFrameInt64 = class(TDataFrameBase)
  private
  protected
    FBuffer: Int64;
  public
    constructor Create(id: Byte);
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

  TDataFrameUInt64 = class(TDataFrameBase)
  private
  protected
    FBuffer: UInt64;
  public
    constructor Create(id: Byte);
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

  TRunTimeDataType = (rdtString, rdtInteger, rdtLongWord, rdtWORD, rdtByte, rdtSingle, rdtDouble,
    rdtArrayInteger, rdtArraySingle, rdtArrayDouble, rdtStream, rdtVariant, rdtInt64, rdtArrayShortInt, rdtCardinal, rdtUInt64, rdtArrayByte,
    rdtArrayInt64);

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
    function IsEnd: Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function NotEnd: Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure GoNext; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    //
    function ReadString: SystemString; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadInteger: Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadCardinal: Cardinal; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadWord: Word; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadBool: Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadBoolean: Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadByte: Byte; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadSingle: Single; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadDouble: Double; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadArrayInteger: TDataFrameArrayInteger; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadArrayShortInt: TDataFrameArrayShortInt; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadArrayByte: TDataFrameArrayByte; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadMD5: UnicodeMixedLib.TMD5; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadArraySingle: TDataFrameArraySingle; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadArrayDouble: TDataFrameArrayDouble; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadArrayInt64: TDataFrameArrayInt64; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure ReadStream(output: TCoreClassStream); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadVariant: Variant; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadInt64: Int64; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadUInt64: UInt64; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure ReadStrings(output: TCoreClassStrings); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure ReadDataFrame(output: TDataFrameEngine); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure ReadVariantList(output: THashVariantList); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure ReadSectionText(output: TSectionTextData); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure ReadTextSection(output: TSectionTextData); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    {$IFNDEF FPC} procedure ReadJson(output: TJsonObject); {$IFDEF INLINE_ASM} inline; {$ENDIF}    {$ENDIF}
    function ReadRect: TRect; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadRectf: TRectf; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadPoint: TPoint; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadPointf: TPointf; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadVector: TVector; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadAffineVector: TAffineVector; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadVec3: TVec3; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadVec4: TVec4; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadVector3: TVector3; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadVector4: TVector4; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadMat4: TMat4; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadMatrix4: TMatrix4; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Read2DPoint: T2DPoint; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadVec2: TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Read2DRect: T2DRect; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadPointer: UInt64; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    // auto read from stream data
    procedure Read(var aBuf; aCount: Int64); {$IFDEF INLINE_ASM} inline; {$ENDIF}
  end;

  TDataFrameEngine = class(TCoreClassObject)
  private
    FDataList         : TCoreClassListForObj;
    FReader           : TDataFrameEngineReader;
    FCompressorDeflate: TCompressorDeflate;
    FCompressorBRRC   : TCompressorBRRC;
  protected
    function DataTypeToByte(v: TRunTimeDataType): Byte;
    function ByteToDataType(v: Byte): TRunTimeDataType;
  public
    constructor Create;
    destructor Destroy; override;

    property Reader: TDataFrameEngineReader read FReader;

    procedure Clear; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function AddData(v: TRunTimeDataType): TDataFrameBase; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function GetData(idx: Integer): TDataFrameBase; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function GetDataInfo(_Obj: TDataFrameBase): SystemString; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Count: Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Delete(idx: Integer): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function DeleteFirst: Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function DeleteLast: Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function DeleteLastCount(cnt: Integer): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function DeleteCount(idx, _Count: Integer): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    //
    procedure Assign(SameObj: TDataFrameEngine); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    //
    procedure WriteString(v: SystemString); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure WriteString(v: umlString); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure WriteInteger(v: Integer); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure WriteCardinal(v: Cardinal); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure WriteWORD(v: Word); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure WriteBool(v: Boolean); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure WriteBoolean(v: Boolean); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure WriteByte(v: Byte); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure WriteSingle(v: Single); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure WriteDouble(v: Double); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function WriteArrayInteger: TDataFrameArrayInteger; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function WriteArrayShortInt: TDataFrameArrayShortInt; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function WriteArrayByte: TDataFrameArrayByte; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure WriteMD5(md5: UnicodeMixedLib.TMD5); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function WriteArraySingle: TDataFrameArraySingle; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function WriteArrayDouble: TDataFrameArrayDouble; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function WriteArrayInt64: TDataFrameArrayInt64; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure WriteStream(v: TCoreClassStream); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure WriteVariant(v: Variant); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure WriteInt64(v: Int64); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure WriteUInt64(v: UInt64); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure WriteStrings(v: TCoreClassStrings); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure WriteDataFrame(v: TDataFrameEngine); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure WriteDataFrameCompressed(v: TDataFrameEngine); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure WriteVariantList(v: THashVariantList); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure WriteSectionText(v: TSectionTextData); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure WriteTextSection(v: TSectionTextData); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    {$IFNDEF FPC} procedure WriteJson(v: TJsonObject); {$IFDEF INLINE_ASM} inline; {$ENDIF}    {$ENDIF}
    procedure WriteFile(fn: SystemString); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure WriteRect(v: TRect); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure WriteRectf(v: TRectf); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure WritePoint(v: TPoint); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure WritePointf(v: TPointf); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure WriteVector(v: TVector); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure WriteAffineVector(v: TAffineVector); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure WriteVec4(v: TVec4); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure WriteVec3(v: TVec3); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure WriteVector4(v: TVector4); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure WriteVector3(v: TVector3); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure WriteMat4(v: TMat4); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure WriteMatrix4(v: TMatrix4); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure Write2DPoint(v: T2DPoint); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure WriteVec2(v: TVec2); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure Write2DRect(v: T2DRect); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure WritePointer(v: Pointer); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure WritePointer(v: UInt64); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    // auto append new stream and write
    procedure Write(const aBuf; aCount: Int64); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    //
    function ReadString(idx: Integer): SystemString; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadInteger(idx: Integer): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadCardinal(idx: Integer): Cardinal; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadWord(idx: Integer): Word; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadBool(idx: Integer): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadBoolean(idx: Integer): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadByte(idx: Integer): Byte; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadSingle(idx: Integer): Single; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadDouble(idx: Integer): Double; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadArrayInteger(idx: Integer): TDataFrameArrayInteger; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadArrayShortInt(idx: Integer): TDataFrameArrayShortInt; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadArrayByte(idx: Integer): TDataFrameArrayByte; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadMD5(idx: Integer): UnicodeMixedLib.TMD5; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadArraySingle(idx: Integer): TDataFrameArraySingle; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadArrayDouble(idx: Integer): TDataFrameArrayDouble; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadArrayInt64(idx: Integer): TDataFrameArrayInt64; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure ReadStream(idx: Integer; output: TCoreClassStream); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadVariant(idx: Integer): Variant; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadInt64(idx: Integer): Int64; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadUInt64(idx: Integer): UInt64; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure ReadStrings(idx: Integer; output: TCoreClassStrings); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure ReadDataFrame(idx: Integer; output: TDataFrameEngine); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure ReadVariantList(idx: Integer; output: THashVariantList); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure ReadSectionText(idx: Integer; output: TSectionTextData); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure ReadTextSection(idx: Integer; output: TSectionTextData); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    {$IFNDEF FPC} procedure ReadJson(idx: Integer; output: TJsonObject); {$IFDEF INLINE_ASM} inline; {$ENDIF}    {$ENDIF}
    function ReadRect(idx: Integer): TRect; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadRectf(idx: Integer): TRectf; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadPoint(idx: Integer): TPoint; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadPointf(idx: Integer): TPointf; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadVector(idx: Integer): TVector; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadAffineVector(idx: Integer): TAffineVector; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadVec3(idx: Integer): TVec3; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadVec4(idx: Integer): TVec4; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadVector3(idx: Integer): TVector3; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadVector4(idx: Integer): TVector4; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadMat4(idx: Integer): TMat4; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadMatrix4(idx: Integer): TMatrix4; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Read2DPoint(idx: Integer): T2DPoint; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadVec2(idx: Integer): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Read2DRect(idx: Integer): T2DRect; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ReadPointer(idx: Integer): UInt64; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    // read from stream data
    procedure Read(idx: Integer; var aBuf; aCount: Int64); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    //
    function ComputeEncodeSize: Integer;

    class procedure BuildEmptyStream(output: TCoreClassStream);

    function EncodeTo(output: TCoreClassStream; const FastMode: Boolean): Integer; overload;
    function EncodeTo(output: TCoreClassStream): Integer; overload;

    // json support
    {$IFNDEF FPC}
    procedure EncodeAsPublicJson(output: TCoreClassStream);
    procedure EncodeAsJson(output: TCoreClassStream);
    procedure DecodeFromJson(stream: TMemoryStream64);
    {$ENDIF}
    //
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

    procedure DecodeFromBytes(var b: TBytes); overload;
    procedure DecodeFromBytes(var b: TBytes; const FastMode: Boolean); overload;

    function GetMD5(const FastMode: Boolean): UnicodeMixedLib.TMD5;
    function Compare(dest: TDataFrameEngine): Boolean;

    procedure LoadFromStream(stream: TCoreClassStream);
    procedure SaveToStream(stream: TCoreClassStream);

    property Data[idx: Integer]: TDataFrameBase read GetData; default;
    property List: TCoreClassListForObj read FDataList;
  end;

  TDataWriter = class(TCoreClassPersistent)
  protected
    FEngine: TDataFrameEngine;
    FStream: TCoreClassStream;
  public
    constructor Create(AStream: TCoreClassStream);
    destructor Destroy; override;

    procedure Clear; virtual;

    procedure WriteString(v: SystemString); virtual;
    procedure WriteInteger(v: Integer); virtual;
    procedure WriteCardinal(v: Cardinal); virtual;
    procedure WriteWORD(v: Word); virtual;
    procedure WriteBool(v: Boolean); virtual;
    procedure WriteBoolean(v: Boolean); virtual;
    procedure WriteByte(v: Byte); virtual;
    procedure WriteSingle(v: Single); virtual;
    procedure WriteDouble(v: Double); virtual;
    procedure WriteArrayInteger(v: array of Integer); virtual;
    procedure WriteArrayShortInt(v: array of ShortInt); virtual;
    procedure WriteArrayByte(v: array of Byte); virtual;
    procedure WriteArraySingle(v: array of Single); virtual;
    procedure WriteArrayDouble(v: array of Double); virtual;
    procedure WriteArrayInt64(v: array of Int64); virtual;
    procedure WriteStream(v: TCoreClassStream); virtual;
    procedure WriteVariant(v: Variant); virtual;
    procedure WriteInt64(v: Int64); virtual;
    procedure WriteUInt64(v: UInt64); virtual;
    procedure WriteStrings(v: TCoreClassStrings); virtual;
    procedure WriteDataFrame(v: TDataFrameEngine); virtual;
    procedure WriteDataFrameCompressed(v: TDataFrameEngine); virtual;
    procedure WriteVariantList(v: THashVariantList); virtual;
    procedure WriteSectionText(v: TSectionTextData); virtual;
    {$IFNDEF FPC}
    procedure WriteJson(v: TJsonObject); virtual;
    {$ENDIF}
    procedure WriteRect(v: TRect); virtual;
    procedure WriteRectf(v: TRectf); virtual;
    procedure WritePoint(v: TPoint); virtual;
    procedure WritePointf(v: TPointf); virtual;
    procedure WriteVector(v: TVector); virtual;
    procedure WriteAffineVector(v: TAffineVector); virtual;
    procedure WriteVec4(v: TVec4); virtual;
    procedure WriteVec3(v: TVec3); virtual;
    procedure WriteVector4(v: TVector4); virtual;
    procedure WriteVector3(v: TVector3); virtual;
    procedure WriteMat4(v: TMat4); virtual;
    procedure WriteMatrix4(v: TMatrix4); virtual;
    procedure Write2DPoint(v: T2DPoint); virtual;
    procedure WriteVec2(v: TVec2); virtual;
    procedure Write2DRect(v: T2DRect); virtual;
    procedure WritePointer(v: Pointer); virtual;
    procedure Write(const aBuf; aCount: Int64); virtual;
  end;

  TDataReader = class(TCoreClassPersistent)
  protected
    FEngine: TDataFrameEngine;
  public
    constructor Create(AStream: TCoreClassStream);
    destructor Destroy; override;

    function ReadString: SystemString; virtual;
    function ReadInteger: Integer; virtual;
    function ReadCardinal: Cardinal; virtual;
    function ReadWord: Word; virtual;
    function ReadBool: Boolean; virtual;
    function ReadBoolean: Boolean; virtual;
    function ReadByte: Byte; virtual;
    function ReadSingle: Single; virtual;
    function ReadDouble: Double; virtual;
    procedure ReadArrayInteger(var Data: array of Integer); virtual;
    procedure ReadArrayShortInt(var Data: array of ShortInt); virtual;
    procedure ReadArrayByte(var Data: array of Byte); virtual;
    procedure ReadArraySingle(var Data: array of Single); virtual;
    procedure ReadArrayDouble(var Data: array of Double); virtual;
    procedure ReadArrayInt64(var Data: array of Int64); virtual;
    procedure ReadStream(output: TCoreClassStream); virtual;
    function ReadVariant: Variant; virtual;
    function ReadInt64: Int64; virtual;
    function ReadUInt64: UInt64; virtual;
    procedure ReadStrings(output: TCoreClassStrings); virtual;
    procedure ReadDataFrame(output: TDataFrameEngine); virtual;
    procedure ReadVariantList(output: THashVariantList); virtual;
    procedure ReadSectionText(output: TSectionTextData); virtual;
    {$IFNDEF FPC}
    procedure ReadJson(output: TJsonObject); virtual;
    {$ENDIF}
    function ReadRect: TRect; virtual;
    function ReadRectf: TRectf; virtual;
    function ReadPoint: TPoint; virtual;
    function ReadPointf: TPointf; virtual;
    function ReadVector: TVector; virtual;
    function ReadAffineVector: TAffineVector; virtual;
    function ReadVec3: TVec3; virtual;
    function ReadVec4: TVec4; virtual;
    function ReadVector3: TVector3; virtual;
    function ReadVector4: TVector4; virtual;
    function ReadMat4: TMat4; virtual;
    function ReadMatrix4: TMatrix4; virtual;
    function Read2DPoint: T2DPoint; virtual;
    function ReadVec2: TVec2; virtual;
    function Read2DRect: T2DRect; virtual;
    function ReadPointer: UInt64; virtual;
    procedure Read(var aBuf; aCount: Int64); virtual;
  end;

implementation

uses DoStatusIO;

constructor TDataFrameBase.Create(id: Byte);
begin
  inherited Create;
  FID := id;
end;

destructor TDataFrameBase.Destroy;
begin
  inherited Destroy;
end;

constructor TDataFrameString.Create(id: Byte);
begin
  inherited Create(id);
  Buffer := '';
end;

destructor TDataFrameString.Destroy;
begin
  inherited Destroy;
end;

procedure TDataFrameString.LoadFromStream(stream: TMemoryStream64);
var
  _Len: Integer;
  b   : TBytes;
begin
  stream.Read64(_Len, umlIntegerLength);
  SetLength(b, _Len);
  if (_Len > 0) then
      stream.Read64(b[0], _Len);

  FBuffer := umlStringOf(b).Text;
end;

procedure TDataFrameString.SaveToStream(stream: TMemoryStream64);
var
  b   : TBytes;
  _Len: Integer;
begin
  b := umlBytesOf(FBuffer);
  _Len := Length(b);
  stream.Write64(_Len, umlIntegerLength);
  if _Len > 0 then
      stream.Write64(b[0], _Len);
end;

{$IFNDEF FPC}


procedure TDataFrameString.LoadFromJson(jarry: TJsonArray; idx: Integer);
begin
  FBuffer := jarry.S[idx];
end;

procedure TDataFrameString.SaveToJson(jarry: TJsonArray; idx: Integer);
begin
  jarry.Add(FBuffer);
end;
{$ENDIF}


function TDataFrameString.ComputeEncodeSize: Integer;
var
  b: TBytes;
begin
  b := umlBytesOf(FBuffer);
  Result := umlIntegerLength + Length(b);
end;

constructor TDataFrameInteger.Create(id: Byte);
begin
  inherited Create(id);
  Buffer := 0;
end;

destructor TDataFrameInteger.Destroy;
begin
  inherited Destroy;
end;

procedure TDataFrameInteger.LoadFromStream(stream: TMemoryStream64);
begin
  stream.Read64(FBuffer, umlIntegerLength);
end;

procedure TDataFrameInteger.SaveToStream(stream: TMemoryStream64);
begin
  stream.Write64(FBuffer, umlIntegerLength);
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
  Result := umlIntegerLength;
end;

constructor TDataFrameCardinal.Create(id: Byte);
begin
  inherited Create(id);
  Buffer := 0;
end;

destructor TDataFrameCardinal.Destroy;
begin
  inherited Destroy;
end;

procedure TDataFrameCardinal.LoadFromStream(stream: TMemoryStream64);
begin
  stream.Read64(FBuffer, umlCardinalLength);
end;

procedure TDataFrameCardinal.SaveToStream(stream: TMemoryStream64);
begin
  stream.Write64(FBuffer, umlCardinalLength);
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
  Result := umlCardinalLength;
end;

constructor TDataFrameWord.Create(id: Byte);
begin
  inherited Create(id);
  Buffer := 0;
end;

destructor TDataFrameWord.Destroy;
begin
  inherited Destroy;
end;

procedure TDataFrameWord.LoadFromStream(stream: TMemoryStream64);
begin
  stream.Read64(FBuffer, umlWordLength);
end;

procedure TDataFrameWord.SaveToStream(stream: TMemoryStream64);
begin
  stream.Write64(FBuffer, umlWordLength);
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
  Result := umlWordLength;
end;

constructor TDataFrameByte.Create(id: Byte);
begin
  inherited Create(id);
  Buffer := 0;
end;

destructor TDataFrameByte.Destroy;
begin
  inherited Destroy;
end;

procedure TDataFrameByte.LoadFromStream(stream: TMemoryStream64);
begin
  stream.Read64(FBuffer, umlByteLength);
end;

procedure TDataFrameByte.SaveToStream(stream: TMemoryStream64);
begin
  stream.Write64(FBuffer, umlByteLength);
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
  Result := umlByteLength;
end;

constructor TDataFrameSingle.Create(id: Byte);
begin
  inherited Create(id);
  Buffer := 0;
end;

destructor TDataFrameSingle.Destroy;
begin
  inherited Destroy;
end;

procedure TDataFrameSingle.LoadFromStream(stream: TMemoryStream64);
begin
  stream.Read64(FBuffer, umlSingleLength);
end;

procedure TDataFrameSingle.SaveToStream(stream: TMemoryStream64);
begin
  stream.Write64(FBuffer, umlSingleLength);
end;

{$IFNDEF FPC}


procedure TDataFrameSingle.LoadFromJson(jarry: TJsonArray; idx: Integer);
begin
  FBuffer := jarry.F[idx];
end;

procedure TDataFrameSingle.SaveToJson(jarry: TJsonArray; idx: Integer);
begin
  jarry.AddF(FBuffer);
end;
{$ENDIF}


function TDataFrameSingle.ComputeEncodeSize: Integer;
begin
  Result := umlSingleLength;
end;

constructor TDataFrameDouble.Create(id: Byte);
begin
  inherited Create(id);
  Buffer := 0;
end;

destructor TDataFrameDouble.Destroy;
begin
  inherited Destroy;
end;

procedure TDataFrameDouble.LoadFromStream(stream: TMemoryStream64);
begin
  stream.Read64(FBuffer, umlDoubleLength);
end;

procedure TDataFrameDouble.SaveToStream(stream: TMemoryStream64);
begin
  stream.Write64(FBuffer, umlDoubleLength);
end;

{$IFNDEF FPC}


procedure TDataFrameDouble.LoadFromJson(jarry: TJsonArray; idx: Integer);
begin
  FBuffer := jarry.F[idx];
end;

procedure TDataFrameDouble.SaveToJson(jarry: TJsonArray; idx: Integer);
begin
  jarry.AddF(FBuffer);
end;
{$ENDIF}


function TDataFrameDouble.ComputeEncodeSize: Integer;
begin
  Result := umlDoubleLength;
end;

function TDataFrameArrayInteger.GetBuffer(idx: Integer): Integer;
begin
  Result := PInteger(FBuffer[idx])^;
end;

procedure TDataFrameArrayInteger.SetBuffer(idx: Integer; Value: Integer);
begin
  PInteger(FBuffer[idx])^ := Value;
end;

constructor TDataFrameArrayInteger.Create(id: Byte);
begin
  inherited Create(id);
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
  New(_PV);
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
  i, l: Integer;
  d   : Integer;
begin
  Clear;
  stream.Read64(l, umlIntegerLength);
  for i := 0 to l - 1 do
    begin
      stream.Read64(d, umlIntegerLength);
      Add(d);
    end;
end;

procedure TDataFrameArrayInteger.SaveToStream(stream: TMemoryStream64);
var
  i, l: Integer;
  d   : Integer;
begin
  l := Count;
  stream.Write64(l, umlIntegerLength);
  for i := 0 to l - 1 do
    begin
      d := Buffer[i];
      stream.Write64(d, umlIntegerLength);
    end;
end;

{$IFNDEF FPC}


procedure TDataFrameArrayInteger.LoadFromJson(jarry: TJsonArray; idx: Integer);
var
  ja: TJsonArray;
  i : Integer;
begin
  ja := jarry.a[idx];
  for i := 0 to ja.Count - 1 do
      Add(ja.i[i]);
end;

procedure TDataFrameArrayInteger.SaveToJson(jarry: TJsonArray; idx: Integer);
var
  ja: TJsonArray;
  i : Integer;
begin
  ja := jarry.AddArray;
  for i := 0 to Count - 1 do
      ja.Add(Buffer[i]);
end;
{$ENDIF}


function TDataFrameArrayInteger.ComputeEncodeSize: Integer;
begin
  Result := umlIntegerLength + umlIntegerLength * Count;
end;

function TDataFrameArrayShortInt.GetBuffer(idx: Integer): ShortInt;
begin
  Result := PShortInt(FBuffer[idx])^;
end;

procedure TDataFrameArrayShortInt.SetBuffer(idx: Integer; Value: ShortInt);
begin
  PShortInt(FBuffer[idx])^ := Value;
end;

constructor TDataFrameArrayShortInt.Create(id: ShortInt);
begin
  inherited Create(id);
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
  New(_PV);
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
  i, l: Integer;
  d   : ShortInt;
begin
  Clear;
  stream.Read64(l, umlIntegerLength);
  for i := 0 to l - 1 do
    begin
      stream.Read64(d, umlShortIntLength);
      Add(d);
    end;
end;

procedure TDataFrameArrayShortInt.SaveToStream(stream: TMemoryStream64);
var
  i, l: Integer;
  d   : ShortInt;
begin
  l := Count;
  stream.Write64(l, umlIntegerLength);
  for i := 0 to l - 1 do
    begin
      d := Buffer[i];
      stream.Write64(d, umlShortIntLength);
    end;
end;

{$IFNDEF FPC}


procedure TDataFrameArrayShortInt.LoadFromJson(jarry: TJsonArray; idx: Integer);
var
  ja: TJsonArray;
  i : Integer;
begin
  ja := jarry.a[idx];
  for i := 0 to ja.Count - 1 do
      Add(ja.i[i]);
end;

procedure TDataFrameArrayShortInt.SaveToJson(jarry: TJsonArray; idx: Integer);
var
  ja: TJsonArray;
  i : Integer;
begin
  ja := jarry.AddArray;
  for i := 0 to Count - 1 do
      ja.Add(Buffer[i]);
end;
{$ENDIF}


function TDataFrameArrayShortInt.ComputeEncodeSize: Integer;
begin
  Result := umlIntegerLength + umlShortIntLength * Count;
end;

function TDataFrameArrayByte.GetBuffer(idx: Integer): Byte;
begin
  Result := PByte(FBuffer[idx])^;
end;

procedure TDataFrameArrayByte.SetBuffer(idx: Integer; Value: Byte);
begin
  PByte(FBuffer[idx])^ := Value;
end;

constructor TDataFrameArrayByte.Create(id: Byte);
begin
  inherited Create(id);
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
  New(_PV);
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
  AddPtrBuff(@v, umlInt64Length);
end;

procedure TDataFrameArrayByte.AddU64(v: UInt64);
begin
  AddPtrBuff(@v, umlUInt64Length);
end;

procedure TDataFrameArrayByte.Addi(v: Integer);
begin
  AddPtrBuff(@v, umlIntegerLength);
end;

procedure TDataFrameArrayByte.AddWord(v: Word);
begin
  AddPtrBuff(@v, umlWordLength);
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
  i, l: Integer;
  d   : Byte;
begin
  Clear;
  stream.Read64(l, umlIntegerLength);
  for i := 0 to l - 1 do
    begin
      stream.Read64(d, umlByteLength);
      Add(d);
    end;
end;

procedure TDataFrameArrayByte.SaveToStream(stream: TMemoryStream64);
var
  i, l: Integer;
  d   : Byte;
begin
  l := Count;
  stream.Write64(l, umlIntegerLength);
  for i := 0 to l - 1 do
    begin
      d := Buffer[i];
      stream.Write64(d, umlByteLength);
    end;
end;

{$IFNDEF FPC}


procedure TDataFrameArrayByte.LoadFromJson(jarry: TJsonArray; idx: Integer);
var
  ja: TJsonArray;
  i : Integer;
begin
  ja := jarry.a[idx];
  for i := 0 to ja.Count - 1 do
      Add(ja.i[i]);
end;

procedure TDataFrameArrayByte.SaveToJson(jarry: TJsonArray; idx: Integer);
var
  ja: TJsonArray;
  i : Integer;
begin
  ja := jarry.AddArray;
  for i := 0 to Count - 1 do
      ja.Add(Buffer[i]);
end;
{$ENDIF}


function TDataFrameArrayByte.ComputeEncodeSize: Integer;
begin
  Result := umlIntegerLength + umlByteLength * Count;
end;

function TDataFrameArraySingle.GetBuffer(idx: Integer): Single;
begin
  Result := PSingle(FBuffer[idx])^;
end;

procedure TDataFrameArraySingle.SetBuffer(idx: Integer; Value: Single);
begin
  PSingle(FBuffer[idx])^ := Value;
end;

constructor TDataFrameArraySingle.Create(id: Byte);
begin
  inherited Create(id);
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
  New(_PV);
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
  i, l: Integer;
  d   : Single;
begin
  Clear;
  stream.Read64(l, umlIntegerLength);
  for i := 0 to l - 1 do
    begin
      stream.Read64(d, umlSingleLength);
      Add(d);
    end;
end;

procedure TDataFrameArraySingle.SaveToStream(stream: TMemoryStream64);
var
  i, l: Integer;
  d   : Single;
begin
  l := Count;
  stream.Write64(l, umlIntegerLength);
  for i := 0 to l - 1 do
    begin
      d := Buffer[i];
      stream.Write64(d, umlSingleLength);
    end;
end;

{$IFNDEF FPC}


procedure TDataFrameArraySingle.LoadFromJson(jarry: TJsonArray; idx: Integer);
var
  ja: TJsonArray;
  i : Integer;
begin
  ja := jarry.a[idx];
  for i := 0 to ja.Count - 1 do
      Add(ja.F[i]);
end;

procedure TDataFrameArraySingle.SaveToJson(jarry: TJsonArray; idx: Integer);
var
  ja: TJsonArray;
  i : Integer;
begin
  ja := jarry.AddArray;
  for i := 0 to Count - 1 do
      ja.AddF(Buffer[i]);
end;
{$ENDIF}


function TDataFrameArraySingle.ComputeEncodeSize: Integer;
begin
  Result := umlIntegerLength + umlSingleLength * Count;
end;

function TDataFrameArrayDouble.GetBuffer(idx: Integer): Double;
begin
  Result := PDouble(FBuffer[idx])^;
end;

procedure TDataFrameArrayDouble.SetBuffer(idx: Integer; Value: Double);
begin
  PDouble(FBuffer[idx])^ := Value;
end;

constructor TDataFrameArrayDouble.Create(id: Byte);
begin
  inherited Create(id);
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
  New(_PV);
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
  i, l: Integer;
  d   : Double;
begin
  Clear;
  stream.Read64(l, umlIntegerLength);
  for i := 0 to l - 1 do
    begin
      stream.Read64(d, umlDoubleLength);
      Add(d);
    end;
end;

procedure TDataFrameArrayDouble.SaveToStream(stream: TMemoryStream64);
var
  i, l: Integer;
  d   : Double;
begin
  l := Count;
  stream.Write64(l, umlIntegerLength);
  for i := 0 to l - 1 do
    begin
      d := Buffer[i];
      stream.Write64(d, umlDoubleLength);
    end;
end;

{$IFNDEF FPC}


procedure TDataFrameArrayDouble.LoadFromJson(jarry: TJsonArray; idx: Integer);
var
  ja: TJsonArray;
  i : Integer;
begin
  ja := jarry.a[idx];
  for i := 0 to ja.Count - 1 do
      Add(ja.F[i]);
end;

procedure TDataFrameArrayDouble.SaveToJson(jarry: TJsonArray; idx: Integer);
var
  ja: TJsonArray;
  i : Integer;
begin
  ja := jarry.AddArray;
  for i := 0 to Count - 1 do
      ja.AddF(Buffer[i]);
end;
{$ENDIF}


function TDataFrameArrayDouble.ComputeEncodeSize: Integer;
begin
  Result := umlIntegerLength + umlDoubleLength * Count;
end;

function TDataFrameArrayInt64.GetBuffer(idx: Integer): Int64;
begin
  Result := PInt64(FBuffer[idx])^;
end;

procedure TDataFrameArrayInt64.SetBuffer(idx: Integer; Value: Int64);
begin
  PInt64(FBuffer[idx])^ := Value;
end;

constructor TDataFrameArrayInt64.Create(id: Byte);
begin
  inherited Create(id);
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
  New(_PV);
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
  i, l: Integer;
  d   : Int64;
begin
  Clear;
  stream.Read64(l, umlIntegerLength);
  for i := 0 to l - 1 do
    begin
      stream.Read64(d, umlInt64Length);
      Add(d);
    end;
end;

procedure TDataFrameArrayInt64.SaveToStream(stream: TMemoryStream64);
var
  i, l: Integer;
  d   : Int64;
begin
  l := Count;
  stream.Write64(l, umlIntegerLength);
  for i := 0 to l - 1 do
    begin
      d := Buffer[i];
      stream.Write64(d, umlInt64Length);
    end;
end;

{$IFNDEF FPC}


procedure TDataFrameArrayInt64.LoadFromJson(jarry: TJsonArray; idx: Integer);
var
  ja: TJsonArray;
  i : Integer;
begin
  ja := jarry.a[idx];
  for i := 0 to ja.Count - 1 do
      Add(ja.l[i]);
end;

procedure TDataFrameArrayInt64.SaveToJson(jarry: TJsonArray; idx: Integer);
var
  ja: TJsonArray;
  i : Integer;
begin
  ja := jarry.AddArray;
  for i := 0 to Count - 1 do
      ja.Add(Buffer[i]);
end;
{$ENDIF}


function TDataFrameArrayInt64.ComputeEncodeSize: Integer;
begin
  Result := umlIntegerLength + umlInt64Length * Count;
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

constructor TDataFrameStream.Create(id: Byte);
begin
  inherited Create(id);
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
  stream.Read64(_Len, umlIntegerLength);
  if (_Len > 0) then
      FBuffer.CopyFrom(stream, _Len);
end;

procedure TDataFrameStream.SaveToStream(stream: TMemoryStream64);
var
  _Len: Integer;
begin
  _Len := FBuffer.Size;
  stream.Write64(_Len, umlIntegerLength);
  if _Len > 0 then
    begin
      FBuffer.Position := 0;
      stream.CopyFrom(FBuffer, _Len);
    end;
end;

{$IFNDEF FPC}


procedure TDataFrameStream.LoadFromJson(jarry: TJsonArray; idx: Integer);
var
  b64: umlString;
begin
  FBuffer.Clear;
  b64.Text := jarry.S[idx];
  umlDecodeStreamBASE64(b64, FBuffer);
end;

procedure TDataFrameStream.SaveToJson(jarry: TJsonArray; idx: Integer);
var
  b64: umlString;
begin
  umlEncodeStreamBASE64(FBuffer, b64);
  jarry.Add(b64.Text);
end;
{$ENDIF}


function TDataFrameStream.ComputeEncodeSize: Integer;
begin
  Result := umlIntegerLength + FBuffer.Size;
end;

constructor TDataFrameVariant.Create(id: Byte);
begin
  inherited Create(id);
end;

destructor TDataFrameVariant.Destroy;
begin
  inherited Destroy;
end;

procedure TDataFrameVariant.LoadFromStream(stream: TMemoryStream64);
var
  r: TCoreClassReader;
begin
  r := TCoreClassReader.Create(stream, 1024 * 4);
  r.IgnoreChildren := True;
  FBuffer := r.ReadVariant;
  DisposeObject(r);
end;

procedure TDataFrameVariant.SaveToStream(stream: TMemoryStream64);
var
  w: TCoreClassWriter;
begin
  w := TCoreClassWriter.Create(stream, 1024 * 4);
  w.IgnoreChildren := True;
  w.WriteVariant(FBuffer);
  DisposeObject(w);
end;

{$IFNDEF FPC}


procedure TDataFrameVariant.LoadFromJson(jarry: TJsonArray; idx: Integer);
begin
  FBuffer := umlStrToVar(jarry.S[idx]);
end;

procedure TDataFrameVariant.SaveToJson(jarry: TJsonArray; idx: Integer);
begin
  jarry.Add(umlVarToStr(FBuffer).Text);
end;
{$ENDIF}


function TDataFrameVariant.ComputeEncodeSize: Integer;
var
  tmp: TCoreClassMemoryStream;
  w  : TCoreClassWriter;
begin
  tmp := TCoreClassMemoryStream.Create;
  w := TCoreClassWriter.Create(tmp, 1024 * 4);
  w.IgnoreChildren := True;
  w.WriteVariant(FBuffer);
  DisposeObject(w);
  Result := tmp.Size;
  DisposeObject(tmp);
end;

constructor TDataFrameInt64.Create(id: Byte);
begin
  inherited Create(id);
  Buffer := 0;
end;

destructor TDataFrameInt64.Destroy;
begin
  inherited Destroy;
end;

procedure TDataFrameInt64.LoadFromStream(stream: TMemoryStream64);
begin
  stream.Read64(FBuffer, umlInt64Length);
end;

procedure TDataFrameInt64.SaveToStream(stream: TMemoryStream64);
begin
  stream.Write64(FBuffer, umlInt64Length);
end;

{$IFNDEF FPC}


procedure TDataFrameInt64.LoadFromJson(jarry: TJsonArray; idx: Integer);
begin
  FBuffer := jarry.l[idx];
end;

procedure TDataFrameInt64.SaveToJson(jarry: TJsonArray; idx: Integer);
begin
  jarry.Add(FBuffer);
end;
{$ENDIF}


function TDataFrameInt64.ComputeEncodeSize: Integer;
begin
  Result := umlInt64Length;
end;

constructor TDataFrameUInt64.Create(id: Byte);
begin
  inherited Create(id);
  Buffer := 0;
end;

destructor TDataFrameUInt64.Destroy;
begin
  inherited Destroy;
end;

procedure TDataFrameUInt64.LoadFromStream(stream: TMemoryStream64);
begin
  stream.Read64(FBuffer, umlUInt64Length);
end;

procedure TDataFrameUInt64.SaveToStream(stream: TMemoryStream64);
begin
  stream.Write64(FBuffer, umlUInt64Length);
end;

{$IFNDEF FPC}


procedure TDataFrameUInt64.LoadFromJson(jarry: TJsonArray; idx: Integer);
begin
  FBuffer := jarry.U[idx];
end;

procedure TDataFrameUInt64.SaveToJson(jarry: TJsonArray; idx: Integer);
begin
  jarry.Add(FBuffer);
end;
{$ENDIF}


function TDataFrameUInt64.ComputeEncodeSize: Integer;
begin
  Result := umlUInt64Length;
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

procedure TDataFrameEngineReader.ReadDataFrame(output: TDataFrameEngine);
begin
  FOwner.ReadDataFrame(FIndex, output);
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

function TDataFrameEngineReader.Read2DRect: T2DRect;
begin
  Result := FOwner.Read2DRect(FIndex);
  inc(FIndex);
end;

function TDataFrameEngineReader.ReadPointer: UInt64;
begin
  Result := FOwner.ReadPointer(FIndex);
  inc(FIndex);
end;

procedure TDataFrameEngineReader.Read(var aBuf; aCount: Int64);
begin
  FOwner.Read(FIndex, aBuf, aCount);
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
  i  : Integer;
  obj: TDataFrameBase;
begin
  for i := 0 to FDataList.Count - 1 do
    begin
      obj := TDataFrameBase(FDataList[i]);
      try
          DisposeObject(obj);
      except
      end;
    end;

  try
      FDataList.Clear;
  except
  end;

  FReader.Index := 0;
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

function TDataFrameEngine.GetDataInfo(_Obj: TDataFrameBase): SystemString;
begin
  case ByteToDataType(_Obj.FID) of
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
      Dec(cnt);
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
  S: TMemoryStream64;
begin
  Clear;
  S := TMemoryStream64.Create;
  SameObj.EncodeTo(S, True);
  S.Position := 0;
  DecodeFrom(S, True);
  DisposeObject(S);
end;

procedure TDataFrameEngine.WriteString(v: SystemString);
var
  _Obj: TDataFrameString;
begin
  _Obj := TDataFrameString.Create(DataTypeToByte(rdtString));
  _Obj.Buffer := v;
  FDataList.Add(_Obj);
end;

procedure TDataFrameEngine.WriteString(v: umlString);
begin
  WriteString(v.Text);
end;

procedure TDataFrameEngine.WriteInteger(v: Integer);
var
  _Obj: TDataFrameInteger;
begin
  _Obj := TDataFrameInteger.Create(DataTypeToByte(rdtInteger));
  _Obj.Buffer := v;
  FDataList.Add(_Obj);
end;

procedure TDataFrameEngine.WriteCardinal(v: Cardinal);
var
  _Obj: TDataFrameCardinal;
begin
  _Obj := TDataFrameCardinal.Create(DataTypeToByte(rdtCardinal));
  _Obj.Buffer := v;
  FDataList.Add(_Obj);
end;

procedure TDataFrameEngine.WriteWORD(v: Word);
var
  _Obj: TDataFrameWord;
begin
  _Obj := TDataFrameWord.Create(DataTypeToByte(rdtWORD));
  _Obj.Buffer := v;
  FDataList.Add(_Obj);
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
  _Obj: TDataFrameByte;
begin
  _Obj := TDataFrameByte.Create(DataTypeToByte(rdtByte));
  _Obj.Buffer := v;
  FDataList.Add(_Obj);
end;

procedure TDataFrameEngine.WriteSingle(v: Single);
var
  _Obj: TDataFrameSingle;
begin
  _Obj := TDataFrameSingle.Create(DataTypeToByte(rdtSingle));
  _Obj.Buffer := v;
  FDataList.Add(_Obj);
end;

procedure TDataFrameEngine.WriteDouble(v: Double);
var
  _Obj: TDataFrameDouble;
begin
  _Obj := TDataFrameDouble.Create(DataTypeToByte(rdtDouble));
  _Obj.Buffer := v;
  FDataList.Add(_Obj);
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
  _Obj: TDataFrameStream;
begin
  _Obj := TDataFrameStream.Create(DataTypeToByte(rdtStream));
  _Obj.Buffer := v;
  FDataList.Add(_Obj);
end;

procedure TDataFrameEngine.WriteVariant(v: Variant);
var
  _Obj: TDataFrameVariant;
begin
  _Obj := TDataFrameVariant.Create(DataTypeToByte(rdtVariant));
  _Obj.Buffer := v;
  FDataList.Add(_Obj);
end;

procedure TDataFrameEngine.WriteInt64(v: Int64);
var
  _Obj: TDataFrameInt64;
begin
  _Obj := TDataFrameInt64.Create(DataTypeToByte(rdtInt64));
  _Obj.Buffer := v;
  FDataList.Add(_Obj);
end;

procedure TDataFrameEngine.WriteUInt64(v: UInt64);
var
  _Obj: TDataFrameUInt64;
begin
  _Obj := TDataFrameUInt64.Create(DataTypeToByte(rdtUInt64));
  _Obj.Buffer := v;
  FDataList.Add(_Obj);
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

procedure TDataFrameEngine.WriteDataFrame(v: TDataFrameEngine);
var
  d: TMemoryStream64;
begin
  d := TMemoryStream64.Create;
  v.EncodeTo(d, True);
  d.Position := 0;
  WriteStream(d);
  DisposeObject(d);
end;

procedure TDataFrameEngine.WriteDataFrameCompressed(v: TDataFrameEngine);
var
  d: TMemoryStream64;
begin
  d := TMemoryStream64.Create;
  v.EncodeAsZLib(d, True);
  d.Position := 0;
  WriteStream(d);
  DisposeObject(d);
end;

procedure TDataFrameEngine.WriteVariantList(v: THashVariantList);
var
  ms: TMemoryStream64;
  h : THashVariantTextStream;
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
  v.SaveToStream(ms, False, TEncoding.UTF8, False);
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
      Add(v.X);
      Add(v.Y);
    end;
end;

procedure TDataFrameEngine.WritePointf(v: TPointf);
begin
  with WriteArraySingle do
    begin
      Add(v.X);
      Add(v.Y);
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

procedure TDataFrameEngine.Write2DRect(v: T2DRect);
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
procedure TDataFrameEngine.Write(const aBuf; aCount: Int64);
var
  S: TMemoryStream64;
begin
  S := TMemoryStream64.Create;
  S.Write64(aBuf, aCount);
  WriteStream(S);
  DisposeObject(S);
end;

function TDataFrameEngine.ReadString(idx: Integer): SystemString;
var
  _Obj: TDataFrameBase;
  i   : Integer;
begin
  _Obj := Data[idx];
  if _Obj is TDataFrameString then
      Result := TDataFrameString(_Obj).Buffer
  else if _Obj is TDataFrameInteger then
      Result := IntToStr(TDataFrameInteger(_Obj).Buffer)
  else if _Obj is TDataFrameCardinal then
      Result := IntToStr(TDataFrameCardinal(_Obj).Buffer)
  else if _Obj is TDataFrameWord then
      Result := IntToStr(TDataFrameWord(_Obj).Buffer)
  else if _Obj is TDataFrameByte then
      Result := IntToStr(TDataFrameByte(_Obj).Buffer)
  else if _Obj is TDataFrameSingle then
      Result := FloatToStr(TDataFrameSingle(_Obj).Buffer)
  else if _Obj is TDataFrameDouble then
      Result := FloatToStr(TDataFrameDouble(_Obj).Buffer)
  else if _Obj is TDataFrameArrayInteger then
    begin
      Result := '(';
      with TDataFrameArrayInteger(_Obj) do
        begin
          for i := 0 to Count - 1 do
            if Result <> '(' then
                Result := Result + ',' + IntToStr(Buffer[i])
            else
                Result := Result + IntToStr(Buffer[i]);
        end;
      Result := Result + ')';
    end
  else if _Obj is TDataFrameArrayShortInt then
    begin
      Result := '(';
      with TDataFrameArrayShortInt(_Obj) do
        begin
          for i := 0 to Count - 1 do
            if Result <> '(' then
                Result := Result + ',' + IntToStr(Buffer[i])
            else
                Result := Result + IntToStr(Buffer[i]);
        end;
      Result := Result + ')';
    end
  else if _Obj is TDataFrameArrayByte then
    begin
      Result := '(';
      with TDataFrameArrayByte(_Obj) do
        begin
          for i := 0 to Count - 1 do
            if Result <> '(' then
                Result := Result + ',' + IntToStr(Buffer[i])
            else
                Result := Result + IntToStr(Buffer[i]);
        end;
      Result := Result + ')';
    end
  else if _Obj is TDataFrameArraySingle then
    begin
      Result := '(';
      with TDataFrameArraySingle(_Obj) do
        begin
          for i := 0 to Count - 1 do
            if Result <> '(' then
                Result := Result + ',' + FloatToStr(Buffer[i])
            else
                Result := Result + FloatToStr(Buffer[i]);
        end;
      Result := Result + ')';
    end
  else if _Obj is TDataFrameArrayDouble then
    begin
      Result := '(';
      with TDataFrameArrayDouble(_Obj) do
        begin
          for i := 0 to Count - 1 do
            if Result <> '(' then
                Result := Result + ',' + FloatToStr(Buffer[i])
            else
                Result := Result + FloatToStr(Buffer[i]);
        end;
      Result := Result + ')';
    end
  else if _Obj is TDataFrameArrayInt64 then
    begin
      Result := '(';
      with TDataFrameArrayInt64(_Obj) do
        begin
          for i := 0 to Count - 1 do
            if Result <> '(' then
                Result := Result + ',' + IntToStr(Buffer[i])
            else
                Result := Result + IntToStr(Buffer[i]);
        end;
      Result := Result + ')';
    end
  else if _Obj is TDataFrameVariant then
      Result := VarToStr(TDataFrameVariant(_Obj).Buffer)
  else if _Obj is TDataFrameInt64 then
      Result := IntToStr(TDataFrameInt64(_Obj).Buffer)
  else if _Obj is TDataFrameUInt64 then
    {$IFDEF FPC}
    Result := IntToStr(TDataFrameUInt64(_Obj).Buffer)
    {$ELSE}
    Result := UIntToStr(TDataFrameUInt64(_Obj).Buffer)
    {$ENDIF}
  else
      Result := '';
end;

function TDataFrameEngine.ReadInteger(idx: Integer): Integer;
var
  _Obj: TDataFrameBase;
begin
  _Obj := Data[idx];
  if _Obj is TDataFrameInteger then
      Result := TDataFrameInteger(_Obj).Buffer
  else if _Obj is TDataFrameString then
      Result := umlStrToInt(TDataFrameString(_Obj).Buffer, 0)
  else if _Obj is TDataFrameCardinal then
      Result := TDataFrameCardinal(_Obj).Buffer
  else if _Obj is TDataFrameWord then
      Result := TDataFrameWord(_Obj).Buffer
  else if _Obj is TDataFrameByte then
      Result := TDataFrameByte(_Obj).Buffer
  else if _Obj is TDataFrameSingle then
      Result := Trunc(TDataFrameSingle(_Obj).Buffer)
  else if _Obj is TDataFrameDouble then
      Result := Trunc(TDataFrameDouble(_Obj).Buffer)
  else if _Obj is TDataFrameVariant then
      Result := (TDataFrameVariant(_Obj).Buffer)
  else if _Obj is TDataFrameInt64 then
      Result := (TDataFrameInt64(_Obj).Buffer)
  else if _Obj is TDataFrameUInt64 then
      Result := (TDataFrameUInt64(_Obj).Buffer)
  else
      Result := 0;
end;

function TDataFrameEngine.ReadCardinal(idx: Integer): Cardinal;
var
  _Obj: TDataFrameBase;
begin
  _Obj := Data[idx];
  if _Obj is TDataFrameCardinal then
      Result := TDataFrameCardinal(_Obj).Buffer
  else if _Obj is TDataFrameString then
      Result := umlStrToInt(TDataFrameString(_Obj).Buffer, 0)
  else if _Obj is TDataFrameInteger then
      Result := TDataFrameInteger(_Obj).Buffer
  else if _Obj is TDataFrameWord then
      Result := TDataFrameWord(_Obj).Buffer
  else if _Obj is TDataFrameByte then
      Result := TDataFrameByte(_Obj).Buffer
  else if _Obj is TDataFrameSingle then
      Result := Trunc(TDataFrameSingle(_Obj).Buffer)
  else if _Obj is TDataFrameDouble then
      Result := Trunc(TDataFrameDouble(_Obj).Buffer)
  else if _Obj is TDataFrameVariant then
      Result := (TDataFrameVariant(_Obj).Buffer)
  else if _Obj is TDataFrameInt64 then
      Result := (TDataFrameInt64(_Obj).Buffer)
  else if _Obj is TDataFrameUInt64 then
      Result := (TDataFrameUInt64(_Obj).Buffer)
  else
      Result := 0;
end;

function TDataFrameEngine.ReadWord(idx: Integer): Word;
var
  _Obj: TDataFrameBase;
begin
  _Obj := Data[idx];
  if _Obj is TDataFrameWord then
      Result := TDataFrameWord(_Obj).Buffer
  else if _Obj is TDataFrameString then
      Result := umlStrToInt(TDataFrameString(_Obj).Buffer, 0)
  else if _Obj is TDataFrameInteger then
      Result := TDataFrameInteger(_Obj).Buffer
  else if _Obj is TDataFrameCardinal then
      Result := TDataFrameCardinal(_Obj).Buffer
  else if _Obj is TDataFrameByte then
      Result := TDataFrameByte(_Obj).Buffer
  else if _Obj is TDataFrameSingle then
      Result := Trunc(TDataFrameSingle(_Obj).Buffer)
  else if _Obj is TDataFrameDouble then
      Result := Trunc(TDataFrameDouble(_Obj).Buffer)
  else if _Obj is TDataFrameVariant then
      Result := (TDataFrameVariant(_Obj).Buffer)
  else if _Obj is TDataFrameInt64 then
      Result := (TDataFrameInt64(_Obj).Buffer)
  else if _Obj is TDataFrameUInt64 then
      Result := (TDataFrameUInt64(_Obj).Buffer)
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
  _Obj: TDataFrameBase;
begin
  _Obj := Data[idx];
  if _Obj is TDataFrameByte then
      Result := TDataFrameByte(_Obj).Buffer
  else if _Obj is TDataFrameString then
      Result := umlStrToInt(TDataFrameString(_Obj).Buffer, 0)
  else if _Obj is TDataFrameInteger then
      Result := TDataFrameInteger(_Obj).Buffer
  else if _Obj is TDataFrameCardinal then
      Result := TDataFrameCardinal(_Obj).Buffer
  else if _Obj is TDataFrameWord then
      Result := TDataFrameWord(_Obj).Buffer
  else if _Obj is TDataFrameSingle then
      Result := Trunc(TDataFrameSingle(_Obj).Buffer)
  else if _Obj is TDataFrameDouble then
      Result := Trunc(TDataFrameDouble(_Obj).Buffer)
  else if _Obj is TDataFrameVariant then
      Result := (TDataFrameVariant(_Obj).Buffer)
  else if _Obj is TDataFrameInt64 then
      Result := (TDataFrameInt64(_Obj).Buffer)
  else if _Obj is TDataFrameUInt64 then
      Result := (TDataFrameUInt64(_Obj).Buffer)
  else
      Result := 0;
end;

function TDataFrameEngine.ReadSingle(idx: Integer): Single;
var
  _Obj: TDataFrameBase;
begin
  _Obj := Data[idx];
  if _Obj is TDataFrameSingle then
      Result := TDataFrameSingle(_Obj).Buffer
  else if _Obj is TDataFrameString then
      Result := umlStrToFloat(TDataFrameString(_Obj).Buffer, 0)
  else if _Obj is TDataFrameInteger then
      Result := TDataFrameInteger(_Obj).Buffer
  else if _Obj is TDataFrameCardinal then
      Result := TDataFrameCardinal(_Obj).Buffer
  else if _Obj is TDataFrameWord then
      Result := TDataFrameWord(_Obj).Buffer
  else if _Obj is TDataFrameByte then
      Result := TDataFrameByte(_Obj).Buffer
  else if _Obj is TDataFrameDouble then
      Result := TDataFrameDouble(_Obj).Buffer
  else if _Obj is TDataFrameVariant then
      Result := (TDataFrameVariant(_Obj).Buffer)
  else if _Obj is TDataFrameInt64 then
      Result := (TDataFrameInt64(_Obj).Buffer)
  else if _Obj is TDataFrameUInt64 then
      Result := (TDataFrameUInt64(_Obj).Buffer)
  else
      Result := 0;
end;

function TDataFrameEngine.ReadDouble(idx: Integer): Double;
var
  _Obj: TDataFrameBase;
begin
  _Obj := Data[idx];
  if _Obj is TDataFrameDouble then
      Result := TDataFrameDouble(_Obj).Buffer
  else if _Obj is TDataFrameString then
      Result := umlStrToFloat(TDataFrameString(_Obj).Buffer, 0)
  else if _Obj is TDataFrameInteger then
      Result := TDataFrameInteger(_Obj).Buffer
  else if _Obj is TDataFrameCardinal then
      Result := TDataFrameCardinal(_Obj).Buffer
  else if _Obj is TDataFrameWord then
      Result := TDataFrameWord(_Obj).Buffer
  else if _Obj is TDataFrameByte then
      Result := TDataFrameByte(_Obj).Buffer
  else if _Obj is TDataFrameSingle then
      Result := TDataFrameSingle(_Obj).Buffer
  else if _Obj is TDataFrameVariant then
      Result := (TDataFrameVariant(_Obj).Buffer)
  else if _Obj is TDataFrameInt64 then
      Result := (TDataFrameInt64(_Obj).Buffer)
  else if _Obj is TDataFrameUInt64 then
      Result := (TDataFrameUInt64(_Obj).Buffer)
  else
      Result := 0;
end;

function TDataFrameEngine.ReadArrayInteger(idx: Integer): TDataFrameArrayInteger;
var
  _Obj: TDataFrameBase;
begin
  _Obj := Data[idx];
  if _Obj is TDataFrameArrayInteger then
      Result := TDataFrameArrayInteger(_Obj)
  else
      Result := nil;
end;

function TDataFrameEngine.ReadArrayShortInt(idx: Integer): TDataFrameArrayShortInt;
var
  _Obj: TDataFrameBase;
begin
  _Obj := Data[idx];
  if _Obj is TDataFrameArrayShortInt then
      Result := TDataFrameArrayShortInt(_Obj)
  else
      Result := nil;
end;

function TDataFrameEngine.ReadArrayByte(idx: Integer): TDataFrameArrayByte;
var
  _Obj: TDataFrameBase;
begin
  _Obj := Data[idx];
  if _Obj is TDataFrameArrayByte then
      Result := TDataFrameArrayByte(_Obj)
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
  _Obj: TDataFrameBase;
begin
  _Obj := Data[idx];
  if _Obj is TDataFrameArraySingle then
      Result := TDataFrameArraySingle(_Obj)
  else
      Result := nil;
end;

function TDataFrameEngine.ReadArrayDouble(idx: Integer): TDataFrameArrayDouble;
var
  _Obj: TDataFrameBase;
begin
  _Obj := Data[idx];
  if _Obj is TDataFrameArrayDouble then
      Result := TDataFrameArrayDouble(_Obj)
  else
      Result := nil;
end;

function TDataFrameEngine.ReadArrayInt64(idx: Integer): TDataFrameArrayInt64;
var
  _Obj: TDataFrameBase;
begin
  _Obj := Data[idx];
  if _Obj is TDataFrameArrayInt64 then
      Result := TDataFrameArrayInt64(_Obj)
  else
      Result := nil;
end;

procedure TDataFrameEngine.ReadStream(idx: Integer; output: TCoreClassStream);
var
  _Obj         : TDataFrameBase;
  AneedResetPos: Boolean;
begin
  _Obj := Data[idx];
  AneedResetPos := output.Size = 0;
  if _Obj is TDataFrameStream then
    begin
      with TDataFrameStream(_Obj) do
        begin
          Buffer.Position := 0;
          output.CopyFrom(Buffer, Buffer.Size);
          Buffer.Position := 0;
        end;
    end
  else if output is TMemoryStream64 then
      _Obj.SaveToStream(TMemoryStream64(output))
  else
      raiseInfo('no support');
  if AneedResetPos then
      output.Position := 0;
end;

function TDataFrameEngine.ReadVariant(idx: Integer): Variant;
var
  _Obj: TDataFrameBase;
begin
  _Obj := Data[idx];
  if _Obj is TDataFrameVariant then
      Result := TDataFrameVariant(_Obj).Buffer
  else if _Obj is TDataFrameString then
      Result := TDataFrameString(_Obj).Buffer
  else if _Obj is TDataFrameInteger then
      Result := TDataFrameInteger(_Obj).Buffer
  else if _Obj is TDataFrameCardinal then
      Result := TDataFrameCardinal(_Obj).Buffer
  else if _Obj is TDataFrameWord then
      Result := TDataFrameWord(_Obj).Buffer
  else if _Obj is TDataFrameByte then
      Result := TDataFrameByte(_Obj).Buffer
  else if _Obj is TDataFrameSingle then
      Result := TDataFrameSingle(_Obj).Buffer
  else if _Obj is TDataFrameDouble then
      Result := TDataFrameDouble(_Obj).Buffer
  else if _Obj is TDataFrameInt64 then
      Result := (TDataFrameInt64(_Obj).Buffer)
  else
      Result := 0;
end;

function TDataFrameEngine.ReadInt64(idx: Integer): Int64;
var
  _Obj: TDataFrameBase;
begin
  _Obj := Data[idx];
  if _Obj is TDataFrameInt64 then
      Result := TDataFrameInt64(_Obj).Buffer
  else if _Obj is TDataFrameUInt64 then
      Result := TDataFrameUInt64(_Obj).Buffer
  else if _Obj is TDataFrameInteger then
      Result := TDataFrameInteger(_Obj).Buffer
  else if _Obj is TDataFrameCardinal then
      Result := TDataFrameCardinal(_Obj).Buffer
  else if _Obj is TDataFrameWord then
      Result := TDataFrameWord(_Obj).Buffer
  else if _Obj is TDataFrameByte then
      Result := TDataFrameByte(_Obj).Buffer
  else if _Obj is TDataFrameSingle then
      Result := Trunc(TDataFrameSingle(_Obj).Buffer)
  else if _Obj is TDataFrameDouble then
      Result := Trunc(TDataFrameDouble(_Obj).Buffer)
  else if _Obj is TDataFrameVariant then
      Result := TDataFrameVariant(_Obj).Buffer
  else
      Result := 0;
end;

function TDataFrameEngine.ReadUInt64(idx: Integer): UInt64;
var
  _Obj: TDataFrameBase;
begin
  _Obj := Data[idx];
  if _Obj is TDataFrameUInt64 then
      Result := TDataFrameUInt64(_Obj).Buffer
  else if _Obj is TDataFrameInt64 then
      Result := TDataFrameInt64(_Obj).Buffer
  else if _Obj is TDataFrameInteger then
      Result := TDataFrameInteger(_Obj).Buffer
  else if _Obj is TDataFrameCardinal then
      Result := TDataFrameCardinal(_Obj).Buffer
  else if _Obj is TDataFrameWord then
      Result := TDataFrameWord(_Obj).Buffer
  else if _Obj is TDataFrameByte then
      Result := TDataFrameByte(_Obj).Buffer
  else if _Obj is TDataFrameSingle then
      Result := Trunc(TDataFrameSingle(_Obj).Buffer)
  else if _Obj is TDataFrameDouble then
      Result := Trunc(TDataFrameDouble(_Obj).Buffer)
  else if _Obj is TDataFrameVariant then
      Result := TDataFrameVariant(_Obj).Buffer
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

procedure TDataFrameEngine.ReadDataFrame(idx: Integer; output: TDataFrameEngine);
var
  d: TMemoryStream64;
begin
  d := TMemoryStream64.Create;
  ReadStream(idx, d);
  d.Position := 0;
  output.DecodeFrom(d, True);
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
  output.LoadFromStream(d, TEncoding.UTF8, False, nil);
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

function TDataFrameEngine.Read2DRect(idx: Integer): T2DRect;
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

procedure TDataFrameEngine.Read(idx: Integer; var aBuf; aCount: Int64);
var
  S: TMemoryStream64;
begin
  S := TMemoryStream64.Create;
  ReadStream(idx, S);
  S.Read64(aBuf, aCount);
  DisposeObject(S);
end;

function TDataFrameEngine.ComputeEncodeSize: Integer;
var
  i: Integer;
begin
  Result := umlIntegerLength;
  for i := 0 to Count - 1 do
      Result := Result + umlByteLength + GetData(i).ComputeEncodeSize;
end;

class procedure TDataFrameEngine.BuildEmptyStream(output: TCoreClassStream);
var
  editionFlag: Byte;
  sizeInfo   : Integer;
  compFlag   : Byte;
  md5        : UnicodeMixedLib.TMD5;
  cnt        : Integer;
begin
  // make header
  editionFlag := $FF;
  sizeInfo := umlIntegerLength;
  compFlag := 0;
  md5 := NullMD5;
  cnt := 0;

  output.Write(editionFlag, umlByteLength);
  output.Write(sizeInfo, umlIntegerLength);
  output.Write(compFlag, umlByteLength);
  output.Write(md5[0], umlMD5Length);
  output.Write(cnt, umlIntegerLength);
end;

function TDataFrameEngine.EncodeTo(output: TCoreClassStream; const FastMode: Boolean): Integer;
var
  i                   : Integer;
  b                   : TDataFrameBase;
  StoreStream, nStream: TMemoryStream64;
  id                  : Byte;

  editionFlag: Byte;
  sizeInfo   : Integer;
  compFlag   : Byte;
  md5        : UnicodeMixedLib.TMD5;
begin
  Result := Count;

  if Result = 0 then
    begin
      BuildEmptyStream(output);
      exit;
    end;

  StoreStream := TMemoryStream64.Create;

  // make body
  StoreStream.Write64(Result, umlIntegerLength);

  nStream := TMemoryStream64.Create;
  for i := 0 to Count - 1 do
    begin
      b := GetData(i);
      id := b.FID;
      b.SaveToStream(nStream);

      StoreStream.Write64(id, umlByteLength);
      nStream.Position := 0;
      StoreStream.CopyFrom(nStream, nStream.Size);
      nStream.Clear;
    end;

  // make header
  editionFlag := $FF;
  sizeInfo := StoreStream.Size;
  compFlag := 0;
  StoreStream.Position := 0;
  if FastMode then
      md5 := NullMD5
  else
      md5 := umlMD5(StoreStream.Memory, StoreStream.Size);

  nStream.Clear;
  nStream.Write(editionFlag, umlByteLength);
  nStream.Write(sizeInfo, umlIntegerLength);
  nStream.Write(compFlag, umlByteLength);
  nStream.Write(md5[0], umlMD5Length);

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

{$IFNDEF FPC}


procedure TDataFrameEngine.EncodeAsPublicJson(output: TCoreClassStream);
var
  j: TJsonObject;
  i: Integer;
begin
  j := TJsonObject.Create;
  j.S['help'] := 'This JSON with TDataFrameEngine encode';

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

procedure TDataFrameEngine.DecodeFromJson(stream: TMemoryStream64);
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
    exit;
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
    exit;
  end;

  DisposeObject(j);
end;
{$ENDIF}


function TDataFrameEngine.EncodeAsZLib(output: TCoreClassStream; const FastMode: Boolean): Integer;
var
  i                               : Integer;
  b                               : TDataFrameBase;
  StoreStream, nStream, compStream: TMemoryStream64;
  ZCompStream                     : TCompressionStream;
  id                              : Byte;

  editionFlag : Byte;
  sizeInfo    : Integer;
  compFlag    : Byte;
  compSizeInfo: Integer;
  md5         : UnicodeMixedLib.TMD5;
begin
  Result := Count;

  if Result = 0 then
    begin
      BuildEmptyStream(output);
      exit;
    end;

  StoreStream := TMemoryStream64.Create;

  // make body
  StoreStream.Write64(Result, umlIntegerLength);

  nStream := TMemoryStream64.Create;
  for i := 0 to Count - 1 do
    begin
      b := GetData(i);
      id := b.FID;
      b.SaveToStream(nStream);

      StoreStream.Write64(id, umlByteLength);
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
  ZCompStream := TCompressionStream.Create(compStream);
  StoreStream.Position := 0;
  ZCompStream.CopyFrom(StoreStream, StoreStream.Size);
  DisposeObject(ZCompStream);
  DisposeObject(StoreStream);

  editionFlag := $FF;
  sizeInfo := compStream.Size;
  compFlag := 1;

  nStream.Clear;
  nStream.Write(editionFlag, umlByteLength);
  nStream.Write(sizeInfo, umlIntegerLength);
  nStream.Write(compFlag, umlByteLength);
  nStream.Write(compSizeInfo, umlIntegerLength);
  nStream.Write(md5[0], umlMD5Length);

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
  i                               : Integer;
  b                               : TDataFrameBase;
  StoreStream, nStream, compStream: TMemoryStream64;
  id                              : Byte;

  editionFlag : Byte;
  sizeInfo    : Integer;
  compFlag    : Byte;
  compSizeInfo: Integer;
  md5         : UnicodeMixedLib.TMD5;
begin
  Result := Count;

  if Result = 0 then
    begin
      BuildEmptyStream(output);
      exit;
    end;

  StoreStream := TMemoryStream64.Create;

  // make body
  StoreStream.Write64(Result, umlIntegerLength);

  nStream := TMemoryStream64.Create;
  for i := 0 to Count - 1 do
    begin
      b := GetData(i);
      id := b.FID;
      b.SaveToStream(nStream);

      StoreStream.Write64(id, umlByteLength);
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

  editionFlag := $FF;
  sizeInfo := compStream.Size;
  compFlag := 2;

  nStream.Clear;
  nStream.Write(editionFlag, umlByteLength);
  nStream.Write(sizeInfo, umlIntegerLength);
  nStream.Write(compFlag, umlByteLength);
  nStream.Write(compSizeInfo, umlIntegerLength);
  nStream.Write(md5[0], umlMD5Length);

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
  i                               : Integer;
  b                               : TDataFrameBase;
  StoreStream, nStream, compStream: TMemoryStream64;
  id                              : Byte;

  editionFlag : Byte;
  sizeInfo    : Integer;
  compFlag    : Byte;
  compSizeInfo: Integer;
  md5         : UnicodeMixedLib.TMD5;
begin
  Result := Count;

  if Result = 0 then
    begin
      BuildEmptyStream(output);
      exit;
    end;

  StoreStream := TMemoryStream64.Create;

  // make body
  StoreStream.Write64(Result, umlIntegerLength);

  nStream := TMemoryStream64.Create;
  for i := 0 to Count - 1 do
    begin
      b := GetData(i);
      id := b.FID;
      b.SaveToStream(nStream);

      StoreStream.Write64(id, umlByteLength);
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

  editionFlag := $FF;
  sizeInfo := compStream.Size;
  compFlag := 3;

  nStream.Clear;
  nStream.Write(editionFlag, umlByteLength);
  nStream.Write(sizeInfo, umlIntegerLength);
  nStream.Write(compFlag, umlByteLength);
  nStream.Write(compSizeInfo, umlIntegerLength);
  nStream.Write(md5[0], umlMD5Length);

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

function TDataFrameEngine.IsCompressed(source: TCoreClassStream): Boolean;
var
  bakPos: Int64;

  editionFlag: Byte;
  sizeInfo   : Integer;
  compFlag   : Byte;
begin
  bakPos := source.Position;
  Result := False;

  source.Read(editionFlag, umlByteLength);
  if editionFlag = $FF then
    begin
      source.Read(sizeInfo, umlIntegerLength);
      source.Read(compFlag, umlByteLength);

      Result := compFlag in [1, 2, 3];
    end;

  source.Position := bakPos;
end;

function TDataFrameEngine.DecodeFrom(source: TCoreClassStream; const FastMode: Boolean): Integer;
var
  i, cnt       : Integer;
  id           : Byte;
  StoreStream  : TMemoryStream64;
  ZDecompStream: TDecompressionStream;
  b            : TDataFrameBase;

  editionFlag : Byte;
  sizeInfo    : Integer;
  compFlag    : Byte;
  compSizeInfo: Integer;
  md5         : UnicodeMixedLib.TMD5;
begin
  Clear;

  Result := 0;

  StoreStream := TMemoryStream64.Create;

  source.Read(editionFlag, umlByteLength);
  if editionFlag = $FF then
    begin
      source.Read(sizeInfo, umlIntegerLength);
      source.Read(compFlag, umlByteLength);
      if compFlag = 0 then
        begin
          source.Read(md5[0], 16);

          if source is TMemoryStream64 then
              StoreStream.SetPointerWithProtectedMode(TMemoryStream64(source).PositionAsPtr, sizeInfo)
          else
              StoreStream.CopyFrom(source, sizeInfo);

          StoreStream.Position := 0;
          if (not FastMode) and (not umlIsNullMD5(md5)) then
            if not umlMD5Compare(umlMD5(StoreStream.Memory, StoreStream.Size), md5) then
              begin
                DoStatus('dataframe md5 error!');
                DisposeObject(StoreStream);
                exit;
              end;
        end
      else if compFlag = 1 then
        begin
          source.Read(compSizeInfo, umlIntegerLength);
          source.Read(md5[0], 16);

          ZDecompStream := TDecompressionStream.Create(source);
          StoreStream.CopyFrom(ZDecompStream, compSizeInfo);
          DisposeObject(ZDecompStream);

          StoreStream.Position := 0;
          if (not FastMode) and (not umlIsNullMD5(md5)) then
            if not umlMD5Compare(umlMD5(StoreStream.Memory, StoreStream.Size), md5) then
              begin
                DoStatus('ZLib md5 error!');
                DisposeObject(StoreStream);
                exit;
              end;
        end
      else if compFlag = 2 then
        begin
          source.Read(compSizeInfo, umlIntegerLength);
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
      else if compFlag = 3 then
        begin
          source.Read(compSizeInfo, umlIntegerLength);
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
        end;
    end
  else
    begin
      DoStatus('dataframe decode error!');
      DisposeObject(StoreStream);
      exit;
    end;

  StoreStream.Position := 0;

  StoreStream.Read64(cnt, umlIntegerLength);
  for i := 0 to cnt - 1 do
    begin
      StoreStream.Read64(id, umlByteLength);
      b := AddData(ByteToDataType(id));
      b.LoadFromStream(StoreStream);
    end;
  DisposeObject(StoreStream);
  Result := cnt;
end;

function TDataFrameEngine.DecodeFrom(source: TCoreClassStream): Integer;
begin
  Result := DecodeFrom(source, False);
end;

procedure TDataFrameEngine.EncodeToBytes(const Compressed, FastMode: Boolean; var output: TBytes);
var
  enStream: TMemoryStream64;
begin
  enStream := TMemoryStream64.Create;
  if Compressed then
      EncodeAsZLib(enStream, FastMode)
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
  enStream.SetPointerWithProtectedMode(@b[0], Length(b));
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
  i     : Integer;
begin
  Result := False;

  // fast prepare compare
  if Count <> dest.Count then
      exit;
  for i := 0 to Count - 1 do
    if FDataList[i].ClassType <> dest[i].ClassType then
        exit;

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
begin
  try
    if ComputeEncodeSize > 8 * 1024 then
        EncodeAsZLib(stream)
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
  verflag       : TBytes;
  len           : Int64;
  m             : TMemoryStream64;
begin
  if FStream <> nil then
    begin
      m := TMemoryStream64.Create;
      FEngine.EncodeTo(m);
      len := m.Size;

      // write version flag
      verflag := umlBytesOf('0001');
      FStream.Write(verflag, 4);

      // write compressed flag
      FlagCompressed := False;
      FStream.Write(FlagCompressed, umlBooleanLength);

      // write length info
      FStream.Write(len, umlInt64Length);

      // write buffer
      m.Position := 0;
      FStream.CopyFrom(m, len);
      DisposeObject(m);
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

procedure TDataWriter.WriteDataFrame(v: TDataFrameEngine);
begin
  FEngine.WriteDataFrame(v);
end;

procedure TDataWriter.WriteDataFrameCompressed(v: TDataFrameEngine);
begin
  FEngine.WriteDataFrameCompressed(v);
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

procedure TDataWriter.Write2DRect(v: T2DRect);
begin
  FEngine.Write2DRect(v);
end;

procedure TDataWriter.WritePointer(v: Pointer);
begin
  FEngine.WritePointer(v);
end;

procedure TDataWriter.Write(const aBuf; aCount: Int64);
begin
  FEngine.Write(aBuf, aCount);
end;

constructor TDataReader.Create(AStream: TCoreClassStream);
var
  verflag       : TBytes;
  FlagCompressed: Boolean;
  len           : Int64;
  m             : TMemoryStream64;
begin
  inherited Create;
  FEngine := TDataFrameEngine.Create;
  if AStream <> nil then
    begin
      // read version flag
      SetLength(verflag, 4);
      AStream.Read(verflag, 4);
      if umlStringOf(verflag) <> '0001' then
          raise Exception.Create('Version flag Does not match!');

      // read compressed flag
      AStream.Read(FlagCompressed, umlBooleanLength);

      // read length info
      AStream.Read(len, umlInt64Length);

      // write buffer
      m := TMemoryStream64.Create;
      m.CopyFrom(AStream, len);
      m.Position := 0;
      FEngine.DecodeFrom(m);
      DisposeObject(m);
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
  i : Integer;
  rb: TDataFrameArrayInteger;
begin
  rb := FEngine.Reader.ReadArrayInteger;
  for i := low(Data) to high(Data) do
      Data[i] := rb[i];
end;

procedure TDataReader.ReadArrayShortInt(var Data: array of ShortInt);
var
  i : Integer;
  rb: TDataFrameArrayShortInt;
begin
  rb := FEngine.Reader.ReadArrayShortInt;
  for i := low(Data) to high(Data) do
      Data[i] := rb[i];
end;

procedure TDataReader.ReadArrayByte(var Data: array of Byte);
var
  i : Integer;
  rb: TDataFrameArrayByte;
begin
  rb := FEngine.Reader.ReadArrayByte;
  for i := low(Data) to high(Data) do
      Data[i] := rb[i];
end;

procedure TDataReader.ReadArraySingle(var Data: array of Single);
var
  i : Integer;
  rb: TDataFrameArraySingle;
begin
  rb := FEngine.Reader.ReadArraySingle;
  for i := low(Data) to high(Data) do
      Data[i] := rb[i];
end;

procedure TDataReader.ReadArrayDouble(var Data: array of Double);
var
  i : Integer;
  rb: TDataFrameArrayDouble;
begin
  rb := FEngine.Reader.ReadArrayDouble;
  for i := low(Data) to high(Data) do
      Data[i] := rb[i];
end;

procedure TDataReader.ReadArrayInt64(var Data: array of Int64);
var
  i : Integer;
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

procedure TDataReader.ReadDataFrame(output: TDataFrameEngine);
begin
  FEngine.Reader.ReadDataFrame(output);
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

function TDataReader.Read2DRect: T2DRect;
begin
  Result := FEngine.Reader.Read2DRect;
end;

function TDataReader.ReadPointer: UInt64;
begin
  Result := FEngine.Reader.ReadPointer;
end;

procedure TDataReader.Read(var aBuf; aCount: Int64);
begin
  FEngine.Reader.Read(aBuf, aCount);
end;

end.
