unit CoreCompress;

{$I zDefine.inc}

interface

uses Math, Types, CoreClasses;

type
  PPCCInt8 = ^PCCInt8;
  PCCInt8  = ^TCCInt8;
  TCCInt8  = ShortInt;

  PPCCUInt8 = ^PCCUInt8;
  PCCUInt8  = ^TCCUInt8;
  TCCUInt8  = byte;

  PPCCInt16 = ^PCCInt16;
  PCCInt16  = ^TCCInt16;
  TCCInt16  = SmallInt;

  PPCCUInt16 = ^PCCUInt16;
  PCCUInt16  = ^TCCUInt16;
  TCCUInt16  = Word;

  PPCCInt32 = ^PCCInt32;
  PCCInt32  = ^TCCInt32;
  TCCInt32  = Integer;

  PPCCUInt32 = ^PCCUInt32;
  PCCUInt32  = ^TCCUInt32;
  TCCUInt32  = Cardinal;

  PPCCInt64 = ^PCCInt64;
  PCCInt64  = ^TCCInt64;
  TCCInt64  = Int64;

  PPCCUInt64 = ^PCCUInt64;
  PCCUInt64  = ^TCCUInt64;
  TCCUInt64  = UInt64;

  PPCCPointer = ^PCCPointer;
  PCCPointer  = ^TCCPointer;
  TCCPointer  = Pointer;

  TCCPtrUInt = NativeUInt;
  TCCPtrInt  = NativeInt;

  PPCCPtrUInt = ^PCCPtrUInt;
  PPCCPtrInt  = ^PCCPtrInt;
  PCCPtrUInt  = ^TCCPtrUInt;
  PCCPtrInt   = ^TCCPtrInt;

  PPCCSizeUInt = ^PCCSizeUInt;
  PCCSizeUInt  = ^TCCSizeUInt;
  TCCSizeUInt  = TCCPtrUInt;

  PPCCSizeInt = ^PCCSizeInt;
  PCCSizeInt  = ^TCCSizeInt;
  TCCSizeInt  = TCCPtrInt;

  PPCCNativeUInt = ^PCCNativeUInt;
  PCCNativeUInt  = ^TCCNativeUInt;
  TCCNativeUInt  = TCCPtrUInt;

  PPCCNativeInt = ^PCCNativeInt;
  PCCNativeInt  = ^TCCNativeInt;
  TCCNativeInt  = TCCPtrInt;

  PPCCSize = ^PCCSizeUInt;
  PCCSize  = ^TCCSizeUInt;
  TCCSize  = TCCPtrUInt;

  PCCUInt8Array = ^TCCUInt8Array;
  TCCUInt8Array = array [0 .. 65535] of TCCUInt8;

  PPCCUInt64Record = ^PCCUInt64Record;
  PCCUInt64Record  = ^TCCUInt64Record;

  TCCUInt64Record = packed record
    case boolean of
      false: ( {$IFDEF BIG_ENDIAN}Hi, Lo{$ELSE}Lo, Hi{$ENDIF}: TCCUInt32;);
      true: (Value: TCCUInt64;);
  end;

  TCompressor = class(TCoreClassObject)
  public
    constructor Create; reintroduce; virtual;
    destructor Destroy; override;
    function Compress(const aInData: TCCPointer; const aInSize: TCCSizeUInt; const aOutData: TCCPointer; const aOutLimit: TCCSizeUInt): TCCSizeUInt; virtual;
    function Decompress(const aInData: TCCPointer; const aInSize: TCCSizeUInt; const aOutData: TCCPointer; const aOutLimit: TCCSizeUInt): TCCSizeUInt; virtual;

    procedure CompressStream(Sour: TCoreClassStream; StartPos, EndPos: NativeInt; CompressTo: TCoreClassStream);
    procedure DecompressStream(Sour, DecompressTo: TCoreClassStream);
  end;

  TCompressorClass = class of TCompressor;

  TCompressorDeflate = class(TCompressor)
  protected
    const
    HashBits           = 16;
    HashSize           = 1 shl HashBits;
    HashMask           = HashSize - 1;
    HashShift          = 32 - HashBits;
    WindowSize         = 32768;
    WindowMask         = WindowSize - 1;
    MinMatch           = 3;
    MaxMatch           = 258;
    MaxOffset          = 32768;
    HashRef_ENDIAN_B30 = {$IF defined(FPC_BIG_ENDIAN)}$FFFFFF00{$ELSE}$00FFFFFF{$IFEND};

  const
    {$IFNDEF BIG_ENDIAN}
    MultiplyDeBruijnBytePosition: array [0 .. 31] of TCCUInt8 = (0, 0, 3, 0, 3, 1, 3, 0, 3, 2, 2, 1, 3, 2, 0, 1, 3, 3, 1, 2, 2, 2, 2, 0, 3, 1, 2, 0, 1, 0, 1, 1);
    {$ENDIF}
    //
    LengthCodes: array [0 .. 28, 0 .. 3] of TCCUInt32 =
      ( // Code, ExtraBits, Min, Max
      (257, 0, 3, 3),
      (258, 0, 4, 4),
      (259, 0, 5, 5),
      (260, 0, 6, 6),
      (261, 0, 7, 7),
      (262, 0, 8, 8),
      (263, 0, 9, 9),
      (264, 0, 10, 10),
      (265, 1, 11, 12),
      (266, 1, 13, 14),
      (267, 1, 15, 16),
      (268, 1, 17, 18),
      (269, 2, 19, 22),
      (270, 2, 23, 26),
      (271, 2, 27, 30),
      (272, 2, 31, 34),
      (273, 3, 35, 42),
      (274, 3, 43, 50),
      (275, 3, 51, 58),
      (276, 3, 59, 66),
      (277, 4, 67, 82),
      (278, 4, 83, 98),
      (279, 4, 99, 114),
      (280, 4, 115, 130),
      (281, 5, 131, 162),
      (282, 5, 163, 194),
      (283, 5, 195, 226),
      (284, 5, 227, 257),
      (285, 0, 258, 258)
      );
    DistanceCodes: array [0 .. 29, 0 .. 3] of TCCUInt32 =
      ( // Code, ExtraBits, Min, Max
      (0, 0, 1, 1),
      (1, 0, 2, 2),
      (2, 0, 3, 3),
      (3, 0, 4, 4),
      (4, 1, 5, 6),
      (5, 1, 7, 8),
      (6, 2, 9, 12),
      (7, 2, 13, 16),
      (8, 3, 17, 24),
      (9, 3, 25, 32),
      (10, 4, 33, 48),
      (11, 4, 49, 64),
      (12, 5, 65, 96),
      (13, 5, 97, 128),
      (14, 6, 129, 192),
      (15, 6, 193, 256),
      (16, 7, 257, 384),
      (17, 7, 385, 512),
      (18, 8, 513, 768),
      (19, 8, 769, 1024),
      (20, 9, 1025, 1536),
      (21, 9, 1537, 2048),
      (22, 10, 2049, 3072),
      (23, 10, 3073, 4096),
      (24, 11, 4097, 6144),
      (25, 11, 6145, 8192),
      (26, 12, 8193, 12288),
      (27, 12, 12289, 16384),
      (28, 13, 16385, 24576),
      (29, 13, 24577, 32768)
      );
    MirrorBytes: array [TCCUInt8] of TCCUInt8 =
      (
      $00, $80, $40, $C0, $20, $A0, $60, $E0,
      $10, $90, $50, $D0, $30, $B0, $70, $F0,
      $08, $88, $48, $C8, $28, $A8, $68, $E8,
      $18, $98, $58, $D8, $38, $B8, $78, $F8,
      $04, $84, $44, $C4, $24, $A4, $64, $E4,
      $14, $94, $54, $D4, $34, $B4, $74, $F4,
      $0C, $8C, $4C, $CC, $2C, $AC, $6C, $EC,
      $1C, $9C, $5C, $DC, $3C, $BC, $7C, $FC,
      $02, $82, $42, $C2, $22, $A2, $62, $E2,
      $12, $92, $52, $D2, $32, $B2, $72, $F2,
      $0A, $8A, $4A, $CA, $2A, $AA, $6A, $EA,
      $1A, $9A, $5A, $DA, $3A, $BA, $7A, $FA,
      $06, $86, $46, $C6, $26, $A6, $66, $E6,
      $16, $96, $56, $D6, $36, $B6, $76, $F6,
      $0E, $8E, $4E, $CE, $2E, $AE, $6E, $EE,
      $1E, $9E, $5E, $DE, $3E, $BE, $7E, $FE,
      $01, $81, $41, $C1, $21, $A1, $61, $E1,
      $11, $91, $51, $D1, $31, $B1, $71, $F1,
      $09, $89, $49, $C9, $29, $A9, $69, $E9,
      $19, $99, $59, $D9, $39, $B9, $79, $F9,
      $05, $85, $45, $C5, $25, $A5, $65, $E5,
      $15, $95, $55, $D5, $35, $B5, $75, $F5,
      $0D, $8D, $4D, $CD, $2D, $AD, $6D, $ED,
      $1D, $9D, $5D, $DD, $3D, $BD, $7D, $FD,
      $03, $83, $43, $C3, $23, $A3, $63, $E3,
      $13, $93, $53, $D3, $33, $B3, $73, $F3,
      $0B, $8B, $4B, $CB, $2B, $AB, $6B, $EB,
      $1B, $9B, $5B, $DB, $3B, $BB, $7B, $FB,
      $07, $87, $47, $C7, $27, $A7, $67, $E7,
      $17, $97, $57, $D7, $37, $B7, $77, $F7,
      $0F, $8F, $4F, $CF, $2F, $AF, $6F, $EF,
      $1F, $9F, $5F, $DF, $3F, $BF, $7F, $FF
      );
    CLCIndex: array [0 .. 18] of TCCUInt8 = (16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15);

  type
    PHashTable  = ^THashTable;
    THashTable  = array [0 .. HashSize - 1] of PCCUInt8;
    PChainTable = ^TChainTable;
    TChainTable = array [0 .. WindowSize - 1] of TCCPointer;
    PTree       = ^TTree;

    TTree = packed record
      Table: array [0 .. 15] of TCCUInt16;
      Translation: array [0 .. 287] of TCCUInt16;
    end;

    PBuffer  = ^TBuffer;
    TBuffer  = array [0 .. 65535] of TCCUInt8;
    PLengths = ^TLengths;
    TLengths = array [0 .. 288 + 32 - 1] of TCCUInt8;
    POffsets = ^TOffsets;
    TOffsets = array [0 .. 15] of TCCUInt16;
    TBits    = array [0 .. 29] of TCCUInt8;
    PBits    = ^TBits;
    TBase    = array [0 .. 29] of TCCUInt16;
    PBase    = ^TBase;
  private
    fHashTable               : THashTable;
    fChainTable              : TChainTable;
    fLengthCodesLookUpTable  : array [0 .. 258] of TCCInt32;
    fDistanceCodesLookUpTable: array [0 .. 32768] of TCCInt32;
    fSymbolLengthTree        : TTree;
    fDistanceTree            : TTree;
    fFixedSymbolLengthTree   : TTree;
    fFixedDistanceTree       : TTree;
    fLengthBits              : TBits;
    fDistanceBits            : TBits;
    fLengthBase              : TBase;
    fDistanceBase            : TBase;
    fCodeTree                : TTree;
    fLengths                 : TLengths;
    fWithHeader              : boolean;
    fGreedy                  : boolean;
    fSkipStrength            : TCCUInt32;
    fMaxSteps                : TCCUInt32;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Compress(const aInData: TCCPointer; const aInSize: TCCSizeUInt; const aOutData: TCCPointer; const aOutLimit: TCCSizeUInt): TCCSizeUInt; override;
    function Decompress(const aInData: TCCPointer; const aInSize: TCCSizeUInt; const aOutData: TCCPointer; const aOutLimit: TCCSizeUInt): TCCSizeUInt; override;
  published
    property WithHeader  : boolean read fWithHeader write fWithHeader;
    property Greedy      : boolean read fGreedy write fGreedy;
    property SkipStrength: TCCUInt32 read fSkipStrength write fSkipStrength;
    property MaxSteps    : TCCUInt32 read fMaxSteps write fMaxSteps;
  end;

  TCompressorBRRC = class(TCompressor)
  private
    const
    FlagModel    = 0;
    LiteralModel = 2;
    SizeModels   = 258;
  private
  public
    constructor Create; override;
    destructor Destroy; override;
    function Compress(const aInData: TCCPointer; const aInSize: TCCSizeUInt; const aOutData: TCCPointer; const aOutLimit: TCCSizeUInt): TCCSizeUInt; override;
    function Decompress(const aInData: TCCPointer; const aInSize: TCCSizeUInt; const aOutData: TCCPointer; const aOutLimit: TCCSizeUInt): TCCSizeUInt; override;
  published
  end;

function CoreCompressStream(Compressor: TCompressor; Sour: TCoreClassStream; ComTo: TCoreClassStream): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function CoreDecompressStream(Compressor: TCompressor; Sour: TCoreClassStream; DeTo: TCoreClassStream): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}

implementation

uses MemoryStream64;

function CoreCompressStream(Compressor: TCompressor; Sour: TCoreClassStream; ComTo: TCoreClassStream): Boolean;
begin
  try
    Compressor.CompressStream(Sour, 0, Sour.Size, ComTo);
    Result := True;
  except
      Result := False;
  end;
end;

function CoreDecompressStream(Compressor: TCompressor; Sour: TCoreClassStream; DeTo: TCoreClassStream): Boolean;
begin
  try
    Compressor.DecompressStream(Sour, DeTo);
    Result := True;
  except
      Result := False;
  end;
end;

procedure BytewiseMemoryMove(const aSource; var aDestination; const aLength: TCCSizeUInt); {$IF defined(CPU386)} register; assembler; {$IFDEF fpc}nostackframe; {$ENDIF}
asm
  push esi
  push edi
  mov esi,eax
  mov edi,edx
  cld
  rep movsb
  pop edi
  pop esi
end;
{$ELSEIF defined(CPUX64)}assembler; {$IFDEF fpc}nostackframe; {$ENDIF}
asm
  {$IFNDEF fpc}
  .noframe
  {$ENDIF}
  {$IFDEF Win64}
  // Win64 ABI: rcx, rdx, r8, r9, rest on stack (scratch registers: rax, rcx, rdx, r8, r9, r10, r11)
  push rdi
  push rsi
  mov rsi,rcx
  mov rdi,rdx
  mov rcx,r8
  {$ELSE}
  // SystemV ABI: rdi, rsi, rdx, rcx, r8, r9, rest on stack (scratch registers: rax, rdi, rsi, rdx, rcx, r8, r9, r10, r11)
  xchg rsi,rdi
  mov rcx,rdx
  {$ENDIF}
  cld
  rep movsb
  {$IFDEF win64}
  pop rsi
  pop rdi
  {$ENDIF}
end;
{$ELSE}


var
  Index              : TCCSizeUInt;
  Source, Destination: PCCUInt8Array;
begin
  if aLength > 0 then
    begin
      Source := TCCPointer(@aSource);
      Destination := TCCPointer(@aDestination);
      for index := 0 to aLength - 1 do
        begin
          Destination^[index] := Source^[index];
        end;
    end;
end;
{$IFEND}


procedure RLELikeSideEffectAwareMemoryMove(const aSource; var aDestination; const aLength: TCCSizeUInt);
begin
  if aLength > 0 then
    begin
      if (TCCSizeUInt(TCCPointer(@aSource)) + aLength) <= TCCSizeUInt(TCCPointer(@aDestination)) then
        // Non-overlapping, so we an use an optimized memory move function
          Move(aSource, aDestination, aLength)
      else
        // Overlapping, so we must do copy byte-wise for to get the free RLE-like side-effect included
          BytewiseMemoryMove(aSource, aDestination, aLength);
    end;
end;

{$IFNDEF fpc}


function BSRDWord(Value: TCCUInt32): TCCUInt32; {$IF defined(CPU386)}assembler; register;
asm
  bsr eax,eax
  jnz @Done
  mov eax,255
@Done:
end;
{$ELSEIF defined(CPUX64)}assembler;
register; {$IFDEF fpc}nostackframe; {$ENDIF}
asm
  {$IFNDEF fpc}
  .NOFRAME
  {$ENDIF}
  {$IFDEF Windows}
  bsr eax,ecx
  {$ELSE}
  bsr eax,edi
  {$ENDIF}
  jnz @Done
  mov eax,255
@Done:
end;
{$ELSE}


const
  BSRDebruijn32Multiplicator                      = TCCUInt32($07C4ACDD);
  BSRDebruijn32Shift                              = 27;
  BSRDebruijn32Mask                               = 31;
  BSRDebruijn32Table: array [0 .. 31] of TCCInt32 = (0, 9, 1, 10, 13, 21, 2, 29, 11, 14, 16, 18, 22, 25, 3, 30, 8, 12, 20, 28, 15, 17, 24, 7, 19, 27, 23, 6, 26, 5, 4, 31);
begin
  if Value = 0 then
    begin
      result := 255;
    end
  else
    begin
      Value := Value or (Value shr 1);
      Value := Value or (Value shr 2);
      Value := Value or (Value shr 4);
      Value := Value or (Value shr 8);
      Value := Value or (Value shr 16);
      result := BSRDebruijn32Table[((Value * BSRDebruijn32Multiplicator) shr BSRDebruijn32Shift) and BSRDebruijn32Mask];
    end;
end;
{$IFEND}


function SARLongint(Value, Shift: TCCInt32): TCCInt32; {$IF defined(CPU386)}assembler; register;
asm
  mov ecx,edx
  sar eax,cl
end;
{$ELSEIF defined(CPUX64)}assembler;
register; {$IFDEF fpc}nostackframe; {$ENDIF}
asm
  {$IFNDEF fpc}
  .noframe
  {$ENDIF}
  {$IFDEF Win64}
  // Win64 ABI: rcx, rdx, r8, r9, rest on stack (scratch registers: rax, rcx, rdx, r8, r9, r10, r11)
  mov eax,ecx
  mov ecx,edx
  {$ELSE}
  // SystemV ABI: rdi, rsi, rdx, rcx, r8, r9, rest on stack (scratch registers: rax, rdi, rsi, rdx, rcx, r8, r9, r10, r11)
  mov eax,edi
  mov ecx,esi
  {$ENDIF}
  sar eax,cl
end;
{$ELSEIF defined(CPUARM) and defined(fpc)}assembler;
register;
asm
  mov r0,r0,asr R1
end {$IFDEF fpc}['r0', 'R1']
{$ENDIF};
{$ELSE}
begin
  Shift := Shift and 31;
  result := (TCCUInt32(Value) shr Shift) or (TCCUInt32(TCCInt32(TCCUInt32(-TCCUInt32(TCCUInt32(Value) shr 31)) and TCCUInt32(-TCCUInt32(ord(Shift <> 0) and 1)))) shl (32 - Shift));
end;
{$IFEND}


function SARInt64(Value: TCCInt64; Shift: TCCInt32): TCCInt64; {$IF defined(CPU386)}assembler; register;
asm
  mov ecx,eax
  and cl,63
  cmp cl,32
  jc @Full
  mov eax,dword ptr [Value+4]
  sar eax,cl
  bt eax,31
  sbb edx,eax
  jmp @Done
@Full:
  mov eax,dword ptr [Value+0]
  mov edx,dword ptr [Value+4]
  shrd eax,edx,cl
  sar edx,cl
@Done:
  mov dword ptr [result+0],eax
  mov dword ptr [result+4],edx
end;
{$ELSEIF defined(CPUX64)}assembler;
register; {$IFDEF fpc}nostackframe; {$ENDIF}
asm
  {$IFNDEF fpc}
  .noframe
  {$ENDIF}
  {$IFDEF Win64}
  // Win64 ABI: rcx, rdx, r8, r9, rest on stack (scratch registers: rax, rcx, rdx, r8, r9, r10, r11)
  mov rax,rcx
  mov rcx,rdx
  {$ELSE}
  // SystemV ABI: rdi, rsi, rdx, rcx, r8, r9, rest on stack (scratch registers: rax, rdi, rsi, rdx, rcx, r8, r9, r10, r11)
  mov rax,rdi
  mov rcx,rsi
  {$ENDIF}
  sar rax,cl
end;
{$ELSE}
begin
  Shift := Shift and 63;
  result := (TCCInt64(Value) shr Shift) or (TCCInt64(TCCInt64(TCCInt64(-TCCInt64(TCCInt64(Value) shr 63)) and TCCInt64(-TCCInt64(ord(Shift <> 0) and 1)))) shl (63 - Shift));
end;
{$IFEND}

{$ENDIF}


constructor TCompressor.Create;
begin
  inherited Create;
end;

destructor TCompressor.Destroy;
begin
  inherited Destroy;
end;

function TCompressor.Compress(const aInData: TCCPointer; const aInSize: TCCSizeUInt; const aOutData: TCCPointer; const aOutLimit: TCCSizeUInt): TCCSizeUInt;
begin
  result := 0;
end;

function TCompressor.Decompress(const aInData: TCCPointer; const aInSize: TCCSizeUInt; const aOutData: TCCPointer; const aOutLimit: TCCSizeUInt): TCCSizeUInt;
begin
  result := 0;
end;

procedure TCompressor.CompressStream(Sour: TCoreClassStream; StartPos, EndPos: NativeInt; CompressTo: TCoreClassStream);
const
  ChunkSize       = $FFFF - $2000;
  PrepareBuffSize = $FFFF;
type
  TPrepareBuff = array [0 .. PrepareBuffSize + 2] of byte;
  PPrepareBuff = ^TPrepareBuff;

var
  buff          : array [0 .. ChunkSize] of byte;
  PrepareBuffPtr: PPrepareBuff;
  siz           : Int64;
  j             : NativeInt;
  Num           : NativeInt;
  Rest          : NativeInt;

  CompressOriginPos, CompressToSiz: Int64;
begin
  LockObject(Self);
  siz := EndPos - StartPos;
  try
    if siz > 0 then
      begin
        if CompressTo is TCoreClassMemoryStream then
          begin
            CompressOriginPos := CompressTo.Position;
            TCoreClassMemoryStream(CompressTo).Size := CompressOriginPos + Sour.Size + 8 + ((siz div ChunkSize) * $2000);

            CompressTo.Write(siz, 8);
            CompressToSiz := 8;

            Sour.Position := StartPos;

            if siz > ChunkSize then
              begin
                { Calculate number of full chunks that will fit into the buffer }
                Num := siz div ChunkSize;
                { Calculate remaining bytes }
                Rest := siz mod ChunkSize;

                { Process full chunks }
                for j := 0 to Num - 1 do
                  begin
                    Sour.Read(buff[0], ChunkSize);
                    PrepareBuffPtr := Pointer(NativeUInt(TCoreClassMemoryStream(CompressTo).Memory) + (CompressOriginPos + CompressToSiz));
                    PWord(@(PrepareBuffPtr^[0]))^ := Compress(@buff[0], ChunkSize, @PrepareBuffPtr^[2], PrepareBuffSize);
                    inc(CompressToSiz, PWord(@(PrepareBuffPtr^[0]))^ + 2);
                  end;

                { Process remaining bytes }
                if Rest > 0 then
                  begin
                    Sour.Read(buff[0], Rest);
                    PrepareBuffPtr := Pointer(NativeUInt(TCoreClassMemoryStream(CompressTo).Memory) + (CompressOriginPos + CompressToSiz));
                    PWord(@(PrepareBuffPtr^[0]))^ := Compress(@buff[0], Rest, @PrepareBuffPtr^[2], PrepareBuffSize);
                    inc(CompressToSiz, PWord(@(PrepareBuffPtr^[0]))^ + 2);
                  end;
              end
            else
              begin
                Sour.Read(buff[0], siz);
                PrepareBuffPtr := Pointer(NativeUInt(TCoreClassMemoryStream(CompressTo).Memory) + (CompressOriginPos + CompressToSiz));
                PWord(@(PrepareBuffPtr^[0]))^ := Compress(@buff[0], siz, @PrepareBuffPtr^[2], PrepareBuffSize);
                inc(CompressToSiz, PWord(@(PrepareBuffPtr^[0]))^ + 2);
              end;

            if CompressOriginPos + CompressToSiz < TCoreClassMemoryStream(CompressTo).Size then
                TCoreClassMemoryStream(CompressTo).Size := CompressOriginPos + CompressToSiz;
          end
        else if CompressTo is TMemoryStream64 then
          begin
            CompressOriginPos := CompressTo.Position;
            TMemoryStream64(CompressTo).Size := CompressOriginPos + Sour.Size + 8 + ((siz div ChunkSize) * $2000);

            CompressTo.Write(siz, 8);
            CompressToSiz := 8;

            Sour.Position := StartPos;

            if siz > ChunkSize then
              begin
                { Calculate number of full chunks that will fit into the buffer }
                Num := siz div ChunkSize;
                { Calculate remaining bytes }
                Rest := siz mod ChunkSize;

                { Process full chunks }
                for j := 0 to Num - 1 do
                  begin
                    Sour.Read(buff[0], ChunkSize);
                    PrepareBuffPtr := TMemoryStream64(CompressTo).PositionAsPtr(CompressOriginPos + CompressToSiz);
                    PWord(@(PrepareBuffPtr^[0]))^ := Compress(@buff[0], ChunkSize, @PrepareBuffPtr^[2], PrepareBuffSize);
                    inc(CompressToSiz, PWord(@(PrepareBuffPtr^[0]))^ + 2);
                  end;

                { Process remaining bytes }
                if Rest > 0 then
                  begin
                    Sour.Read(buff[0], Rest);
                    PrepareBuffPtr := TMemoryStream64(CompressTo).PositionAsPtr(CompressOriginPos + CompressToSiz);
                    PWord(@(PrepareBuffPtr^[0]))^ := Compress(@buff[0], Rest, @PrepareBuffPtr^[2], PrepareBuffSize);
                    inc(CompressToSiz, PWord(@(PrepareBuffPtr^[0]))^ + 2);
                  end;
              end
            else
              begin
                Sour.Read(buff[0], siz);
                PrepareBuffPtr := TMemoryStream64(CompressTo).PositionAsPtr(CompressOriginPos + CompressToSiz);
                PWord(@(PrepareBuffPtr^[0]))^ := Compress(@buff[0], siz, @PrepareBuffPtr^[2], PrepareBuffSize);
                inc(CompressToSiz, PWord(@(PrepareBuffPtr^[0]))^ + 2);
              end;

            if CompressOriginPos + CompressToSiz < TMemoryStream64(CompressTo).Size then
                TMemoryStream64(CompressTo).Size := CompressOriginPos + CompressToSiz;
          end
        else
          begin
            CompressTo.Write(siz, 8);
            Sour.Position := StartPos;

            New(PrepareBuffPtr);
            if siz > ChunkSize then
              begin
                { Calculate number of full chunks that will fit into the buffer }
                Num := siz div ChunkSize;
                { Calculate remaining bytes }
                Rest := siz mod ChunkSize;

                { Process full chunks }
                for j := 0 to Num - 1 do
                  begin
                    Sour.Read(buff[0], ChunkSize);
                    PWord(@(PrepareBuffPtr^[0]))^ := Compress(@buff[0], ChunkSize, @PrepareBuffPtr^[2], PrepareBuffSize);
                    CompressTo.Write(PrepareBuffPtr^[0], PWord(@(PrepareBuffPtr^[0]))^ + 2);
                  end;

                { Process remaining bytes }
                if Rest > 0 then
                  begin
                    Sour.Read(buff[0], Rest);
                    PWord(@(PrepareBuffPtr^[0]))^ := Compress(@buff[0], Rest, @PrepareBuffPtr^[2], PrepareBuffSize);
                    CompressTo.Write(PrepareBuffPtr^[0], PWord(@(PrepareBuffPtr^[0]))^ + 2);
                  end;
              end
            else
              begin
                Sour.Read(buff[0], siz);
                PWord(@(PrepareBuffPtr^[0]))^ := Compress(@buff[0], siz, @PrepareBuffPtr^[2], PrepareBuffSize);
                CompressTo.Write(PrepareBuffPtr^[0], PWord(@(PrepareBuffPtr^[0]))^ + 2);
              end;
            Dispose(PrepareBuffPtr);
          end;
      end;
  finally
      UnLockObject(Self);
  end;
end;

procedure TCompressor.DecompressStream(Sour, DecompressTo: TCoreClassStream);
var
  siz, cSiz, DecompressOriginPos: Int64;
  bufSiz, deBufSiz              : Word;
  buff, debuff                  : array of byte;
begin
  LockObject(Self);
  try
    if Sour.Position + 10 < Sour.Size then
      begin
        Sour.Read(siz, 8);
        cSiz := 0;
        DecompressOriginPos := DecompressTo.Position;

        if DecompressTo is TCoreClassMemoryStream then
          begin
            TCoreClassMemoryStream(DecompressTo).Size := DecompressOriginPos + siz;
            DecompressTo.Position := DecompressOriginPos;
            SetLength(buff, $FFFF);
            while cSiz < siz do
              begin
                if Sour.Read(bufSiz, 2) <> 2 then
                    break;
                if Sour.Read(buff[0], bufSiz) <> bufSiz then
                    break;

                deBufSiz := Decompress(@buff[0], bufSiz, Pointer(NativeUInt(TCoreClassMemoryStream(DecompressTo).Memory) + (DecompressOriginPos + cSiz)), $FFFF);
                inc(cSiz, deBufSiz);
              end;
            SetLength(buff, 0);
          end
        else if DecompressTo is TMemoryStream64 then
          begin
            TMemoryStream64(DecompressTo).Size := DecompressOriginPos + siz;
            DecompressTo.Position := DecompressOriginPos;
            SetLength(buff, $FFFF);
            while cSiz < siz do
              begin
                if Sour.Read(bufSiz, 2) <> 2 then
                    break;
                if Sour.Read(buff[0], bufSiz) <> bufSiz then
                    break;

                deBufSiz := Decompress(@buff[0], bufSiz, TMemoryStream64(DecompressTo).PositionAsPtr(DecompressOriginPos + cSiz), $FFFF);
                inc(cSiz, deBufSiz);
              end;
            SetLength(buff, 0);
          end
        else
          begin
            SetLength(buff, $FFFF);
            SetLength(debuff, $FFFF);
            while cSiz < siz do
              begin
                if Sour.Read(bufSiz, 2) <> 2 then
                    break;
                if Sour.Read(buff[0], bufSiz) <> bufSiz then
                    break;

                deBufSiz := Decompress(@buff[0], bufSiz, @debuff[0], $FFFF);
                DecompressTo.Write(debuff[0], deBufSiz);
                inc(cSiz, deBufSiz);
              end;
            SetLength(buff, 0);
            SetLength(debuff, 0);
          end;
      end;
  finally
      UnLockObject(Self);
  end;
end;

constructor TCompressorDeflate.Create;
  procedure BuildFixedTrees(var aLT, aDT: TTree); {$IFDEF INLINE_ASM} inline; {$ENDIF}
  var
    i: TCCInt32;
  begin
    for i := 0 to 6 do
      begin
        aLT.Table[i] := 0;
      end;
    aLT.Table[7] := 24;
    aLT.Table[8] := 152;
    aLT.Table[9] := 112;
    for i := 0 to 23 do
        aLT.Translation[i] := 256 + i;
    for i := 0 to 143 do
        aLT.Translation[24 + i] := i;
    for i := 0 to 7 do
        aLT.Translation[168 + i] := 280 + i;
    for i := 0 to 111 do
        aLT.Translation[176 + i] := 144 + i;
    for i := 0 to 4 do
        aDT.Table[i] := 0;
    aDT.Table[5] := 32;
    for i := 0 to 31 do
        aDT.Translation[i] := i;
  end;
  procedure BuildBitsBase(aBits: PCCUInt8Array; aBase: PCCUInt16; aDelta, aFirst: TCCInt32); {$IFDEF INLINE_ASM} inline; {$ENDIF}
  var
    i, Sum: TCCInt32;
  begin
    for i := 0 to aDelta - 1 do
        aBits^[i] := 0;
    for i := 0 to (30 - aDelta) - 1 do
        aBits^[i + aDelta] := i div aDelta;
    Sum := aFirst;
    for i := 0 to 29 do
      begin
        aBase^ := Sum;
        inc(aBase);
        inc(Sum, 1 shl aBits^[i]);
      end;
  end;

var
  Index, ValueIndex: TCCInt32;
begin
  inherited Create;
  for index := 0 to length(LengthCodes) - 1 do
    for ValueIndex := IfThen(index = 0, 0, LengthCodes[index, 2]) to LengthCodes[index, 3] do
        fLengthCodesLookUpTable[ValueIndex] := index;
  for index := 0 to length(DistanceCodes) - 1 do
    for ValueIndex := IfThen(index = 0, 0, DistanceCodes[index, 2]) to DistanceCodes[index, 3] do
        fDistanceCodesLookUpTable[ValueIndex] := index;

  FillPtrByte(@fLengthBits, sizeof(TBits), 0);
  FillPtrByte(@fDistanceBits, sizeof(TBits), 0);
  FillPtrByte(@fLengthBase, sizeof(TBase), 0);
  FillPtrByte(@fDistanceBase, sizeof(TBase), 0);
  FillPtrByte(@fFixedSymbolLengthTree, sizeof(TTree), 0);
  FillPtrByte(@fFixedDistanceTree, sizeof(TTree), 0);
  BuildFixedTrees(fFixedSymbolLengthTree, fFixedDistanceTree);
  BuildBitsBase(TCCPointer(@fLengthBits[0]), PCCUInt16(TCCPointer(@fLengthBase[0])), 4, 3);
  BuildBitsBase(TCCPointer(@fDistanceBits[0]), PCCUInt16(TCCPointer(@fDistanceBase[0])), 2, 1);
  fLengthBits[28] := 0;
  fLengthBase[28] := 258;
  fWithHeader := false;
  fGreedy := false;
  fSkipStrength := 32;
  fMaxSteps := 128;
end;

destructor TCompressorDeflate.Destroy;
begin
  inherited Destroy;
end;

function TCompressorDeflate.Compress(const aInData: TCCPointer; const aInSize: TCCSizeUInt; const aOutData: TCCPointer; const aOutLimit: TCCSizeUInt): TCCSizeUInt;
var
  OutputBits, CountOutputBits: TCCUInt32;
  DestLen                    : TCCSizeUInt;
  OK                         : boolean;
  procedure DoOutputBits(const aBits, aCountBits: TCCUInt32);
  begin
    Assert((CountOutputBits + aCountBits) <= 32);
    OutputBits := OutputBits or (aBits shl CountOutputBits);
    inc(CountOutputBits, aCountBits);
    while CountOutputBits >= 8 do
      begin
        if DestLen < aOutLimit then
          begin
            PCCUInt8Array(aOutData)^[DestLen] := OutputBits and $FF;
            inc(DestLen);
          end
        else
          begin
            OK := false;
          end;
        OutputBits := OutputBits shr 8;
        dec(CountOutputBits, 8);
      end;
  end;
  procedure DoOutputLiteral(const aValue: TCCUInt8); {$IFDEF INLINE_ASM} inline; {$ENDIF}
  begin
    case aValue of
      0 .. 143: DoOutputBits(MirrorBytes[$30 + aValue], 8);
      else DoOutputBits((MirrorBytes[$90 + (aValue - 144)] shl 1) or 1, 9);
    end;
  end;
  procedure DoOutputCopy(const aDistance, aLength: TCCUInt32);
  var
    Remain, ToDo, Index: TCCUInt32;
  begin
    Remain := aLength;
    while Remain > 0 do
      begin
        case Remain of
          0 .. 258: ToDo := Remain;
          259 .. 260: ToDo := Remain - 3;
          else ToDo := 258;
        end;
        dec(Remain, ToDo);
        index := fLengthCodesLookUpTable[Min(Max(ToDo, 0), 258)];
        if LengthCodes[index, 0] <= 279 then
            DoOutputBits(MirrorBytes[(LengthCodes[index, 0] - 256) shl 1], 7)
        else
            DoOutputBits(MirrorBytes[$C0 + (LengthCodes[index, 0] - 280)], 8);
        if LengthCodes[index, 1] <> 0 then
            DoOutputBits(ToDo - LengthCodes[index, 2], LengthCodes[index, 1]);
        index := fDistanceCodesLookUpTable[Min(Max(aDistance, 0), 32768)];
        DoOutputBits(MirrorBytes[DistanceCodes[index, 0] shl 3], 5);
        if DistanceCodes[index, 1] <> 0 then
            DoOutputBits(aDistance - DistanceCodes[index, 2], DistanceCodes[index, 1]);
      end;
  end;
  procedure OutputStartBlock;
  begin
    DoOutputBits(1, 1); // Final block
    DoOutputBits(1, 2); // Static huffman block
  end;
  procedure OutputEndBlock;
  begin
    DoOutputBits(0, 7); // Close block
    DoOutputBits(0, 7); // Make sure all bits are flushed
  end;
  function Adler32(const aData: TCCPointer; const aLength: TCCUInt32): TCCUInt32; {$IFDEF INLINE_ASM} inline; {$ENDIF}
  const
    Base               = 65521;
    MaximumCountAtOnce = 5552;
  var
    Buf                        : PCCUInt8;
    Remain, s1, s2, ToDo, Index: TCCUInt32;
  begin
    s1 := 1;
    s2 := 0;
    Buf := aData;
    Remain := aLength;
    while Remain > 0 do
      begin
        if Remain < MaximumCountAtOnce then
            ToDo := Remain
        else
            ToDo := MaximumCountAtOnce;
        dec(Remain, ToDo);
        for index := 1 to ToDo do
          begin
            inc(s1, TCCUInt8(Buf^));
            inc(s2, s1);
            inc(Buf);
          end;
        s1 := s1 mod Base;
        s2 := s2 mod Base;
      end;
    result := (s2 shl 16) or s1;
  end;

var
  CurrentPointer, EndPointer, EndSearchPointer, Head, CurrentPossibleMatch: PCCUInt8;
  BestMatchDistance, BestMatchLength, MatchLength, CheckSum, Step, Difference, Offset,
    UnsuccessfulFindMatchAttempts: TCCUInt32;
  HashTableItem                  : PPCCUInt8;
begin
  OK := true;
  DestLen := 0;
  OutputBits := 0;
  CountOutputBits := 0;
  if fWithHeader then
    begin
      DoOutputBits($78, 8); // CMF
      DoOutputBits($9C, 8); // FLG Default Compression
    end;
  OutputStartBlock;
  FillPtrByte(@fHashTable, sizeof(THashTable), 0);
  FillPtrByte(@fChainTable, sizeof(TChainTable), 0);
  CurrentPointer := aInData;
  EndPointer := TCCPointer(TCCPtrUInt(TCCPtrUInt(CurrentPointer) + TCCPtrUInt(aInSize)));
  EndSearchPointer := TCCPointer(TCCPtrUInt((TCCPtrUInt(CurrentPointer) + TCCPtrUInt(aInSize)) - TCCPtrUInt(TCCInt64(Max(TCCInt64(MinMatch), TCCInt64(sizeof(TCCUInt32)))))));
  UnsuccessfulFindMatchAttempts := TCCUInt32(1) shl fSkipStrength;
  while TCCPtrUInt(CurrentPointer) < TCCPtrUInt(EndSearchPointer) do
    begin
      HashTableItem := @fHashTable[((((PCCUInt32(TCCPointer(CurrentPointer))^ and TCCUInt32(HashRef_ENDIAN_B30){$IF defined(FPC_BIG_ENDIAN)} shr 8{$IFEND})) * TCCUInt32($1E35A7BD)) shr HashShift) and HashMask];
      Head := HashTableItem^;
      CurrentPossibleMatch := Head;
      BestMatchDistance := 0;
      BestMatchLength := 1;
      Step := 0;
      while assigned(CurrentPossibleMatch) and
        (TCCPtrUInt(CurrentPointer) > TCCPtrUInt(CurrentPossibleMatch)) and
        (TCCPtrInt(TCCPtrUInt(TCCPtrUInt(CurrentPointer) - TCCPtrUInt(CurrentPossibleMatch))) < TCCPtrInt(MaxOffset)) do
        begin
          Difference := PCCUInt32(TCCPointer(@PCCUInt8Array(CurrentPointer)^[0]))^ xor PCCUInt32(TCCPointer(@PCCUInt8Array(CurrentPossibleMatch)^[0]))^;
          if (Difference and TCCUInt32(HashRef_ENDIAN_B30)) = 0 then
            begin
              if (BestMatchLength <= (TCCPtrUInt(EndPointer) - TCCPtrUInt(CurrentPointer))) and
                (PCCUInt8Array(CurrentPointer)^[BestMatchLength - 1] = PCCUInt8Array(CurrentPossibleMatch)^[BestMatchLength - 1]) then
                begin
                  MatchLength := MinMatch;

                  while ((TCCPtrUInt(@PCCUInt8Array(CurrentPointer)^[MatchLength]) and (sizeof(TCCUInt32) - 1)) <> 0) and
                    ((TCCPtrUInt(@PCCUInt8Array(CurrentPointer)^[MatchLength]) < TCCPtrUInt(EndPointer))) and
                    (PCCUInt8Array(CurrentPointer)^[MatchLength] = PCCUInt8Array(CurrentPossibleMatch)^[MatchLength]) do
                    begin
                      inc(MatchLength);
                    end;

                  while (TCCPtrUInt(@PCCUInt8Array(CurrentPointer)^[MatchLength + (sizeof(TCCUInt32) - 1)]) < TCCPtrUInt(EndPointer)) do
                    begin
                      Difference := PCCUInt32(TCCPointer(@PCCUInt8Array(CurrentPointer)^[MatchLength]))^ xor PCCUInt32(TCCPointer(@PCCUInt8Array(CurrentPossibleMatch)^[MatchLength]))^;
                      if Difference = 0 then
                        begin
                          inc(MatchLength, sizeof(TCCUInt32));
                        end
                      else
                        begin
                          {$IF defined(FPC_BIG_ENDIAN)}
                          if (Difference shr 16) <> 0 then
                            begin
                              inc(MatchLength, not(Difference shr 24));
                            end
                          else
                            begin
                              inc(MatchLength, 2 + (not(Difference shr 8)));
                            end;
                          {$ELSE}
                          inc(MatchLength, MultiplyDeBruijnBytePosition[TCCUInt32(TCCUInt32(Difference and (-Difference)) * TCCUInt32($077CB531)) shr 27]);
                          {$IFEND}
                          break;
                        end;
                    end;
                  if BestMatchLength < MatchLength then
                    begin
                      BestMatchDistance := TCCPtrUInt(TCCPtrUInt(CurrentPointer) - TCCPtrUInt(CurrentPossibleMatch));
                      BestMatchLength := MatchLength;
                    end;
                end;
            end;
          inc(Step);
          if Step < fMaxSteps then
              CurrentPossibleMatch := fChainTable[(TCCPtrUInt(CurrentPossibleMatch) - TCCPtrUInt(aInData)) and WindowMask]
          else
              break;
        end;
      if (BestMatchDistance > 0) and (BestMatchLength > 1) then
        begin
          DoOutputCopy(BestMatchDistance, BestMatchLength);
          UnsuccessfulFindMatchAttempts := TCCUInt32(1) shl fSkipStrength;
        end
      else
        begin
          if fSkipStrength > 31 then
            begin
              DoOutputLiteral(CurrentPointer^);
            end
          else
            begin
              Step := UnsuccessfulFindMatchAttempts shr fSkipStrength;
              Offset := 0;
              while (Offset < Step) and ((TCCPtrUInt(CurrentPointer) + Offset) < TCCPtrUInt(EndSearchPointer)) do
                begin
                  DoOutputLiteral(PCCUInt8Array(CurrentPointer)^[Offset]);
                  inc(Offset);
                end;
              BestMatchLength := Offset;
              inc(UnsuccessfulFindMatchAttempts, ord(UnsuccessfulFindMatchAttempts < TCCUInt32($FFFFFFFF)) and 1);
            end;
        end;

      if not OK then
          break;

      HashTableItem^ := CurrentPointer;
      fChainTable[(TCCPtrUInt(CurrentPointer) - TCCPtrUInt(aInData)) and WindowMask] := Head;
      if fGreedy then
        begin
          inc(CurrentPointer);
          dec(BestMatchLength);
          while (BestMatchLength > 0) and (TCCPtrUInt(CurrentPointer) < TCCPtrUInt(EndSearchPointer)) do
            begin
              HashTableItem := @fHashTable[((((PCCUInt32(TCCPointer(CurrentPointer))^ and TCCUInt32(HashRef_ENDIAN_B30){$IF defined(FPC_BIG_ENDIAN)} shr 8{$IFEND})) * TCCUInt32($1E35A7BD)) shr HashShift) and HashMask];
              Head := HashTableItem^;
              HashTableItem^ := CurrentPointer;
              fChainTable[(TCCPtrUInt(CurrentPointer) - TCCPtrUInt(aInData)) and WindowMask] := Head;
              inc(CurrentPointer);
              dec(BestMatchLength);
            end;
        end;
      inc(CurrentPointer, BestMatchLength);
    end;
  while TCCPtrUInt(CurrentPointer) < TCCPtrUInt(EndPointer) do
    begin
      DoOutputLiteral(CurrentPointer^);

      if not OK then
          break;

      inc(CurrentPointer);
    end;
  OutputEndBlock;
  if fWithHeader then
    begin
      CheckSum := Adler32(aInData, aInSize);
      if (DestLen + 4) < aOutLimit then
        begin
          PCCUInt8Array(aOutData)^[DestLen + 0] := (CheckSum shr 24) and $FF;
          PCCUInt8Array(aOutData)^[DestLen + 1] := (CheckSum shr 16) and $FF;
          PCCUInt8Array(aOutData)^[DestLen + 2] := (CheckSum shr 8) and $FF;
          PCCUInt8Array(aOutData)^[DestLen + 3] := (CheckSum shr 0) and $FF;
          inc(DestLen, 4);
        end;
    end;

  if OK then
      result := DestLen
  else
      result := 0;
end;

function TCompressorDeflate.Decompress(const aInData: TCCPointer; const aInSize: TCCSizeUInt; const aOutData: TCCPointer; const aOutLimit: TCCSizeUInt): TCCSizeUInt;
var
  Tag, BitCount    : TCCUInt32;
  Source, SourceEnd: PCCUInt8;
  Dest             : PCCUInt8;
  DestLen          : TCCSizeUInt;
  function Adler32(aData: TCCPointer; aLength: TCCUInt32): TCCUInt32; {$IFDEF INLINE_ASM} inline; {$ENDIF}
  const
    Base = 65521;
    NMAX = 5552;
  var
    Buf         : PCCUInt8;
    s1, s2, k, i: TCCUInt32;
  begin
    s1 := 1;
    s2 := 0;
    Buf := aData;
    while aLength > 0 do
      begin
        if aLength < NMAX then
            k := aLength
        else
            k := NMAX;
        dec(aLength, k);
        for i := 1 to k do
          begin
            inc(s1, TCCUInt8(Buf^));
            inc(s2, s1);
            inc(Buf);
          end;
        s1 := s1 mod Base;
        s2 := s2 mod Base;
      end;
    result := (s2 shl 16) or s1;
  end;
  procedure BuildTree(var aTree: TTree; aLengths: PCCUInt8Array; aNum: TCCInt32); {$IFDEF INLINE_ASM} inline; {$ENDIF}
  var
    Offsets: TOffsets;
    i      : TCCInt32;
    Sum    : TCCUInt32;
  begin
    for i := 0 to 15 do
        aTree.Table[i] := 0;
    for i := 0 to aNum - 1 do
        inc(aTree.Table[TCCUInt8(aLengths^[i])]);
    aTree.Table[0] := 0;
    Sum := 0;
    for i := 0 to 15 do
      begin
        Offsets[i] := Sum;
        inc(Sum, aTree.Table[i]);
      end;
    for i := 0 to aNum - 1 do
      if aLengths^[i] <> 0 then
        begin
          aTree.Translation[Offsets[TCCUInt8(aLengths^[i])]] := i;
          inc(Offsets[TCCUInt8(aLengths^[i])]);
        end;
  end;
  function GetBit: TCCUInt32;
  begin
    if BitCount = 0 then
      begin
        Tag := TCCUInt8(Source^);
        inc(Source);
        BitCount := 7;
      end
    else
        dec(BitCount);
    result := Tag and 1;
    Tag := Tag shr 1;
  end;
  function ReadBits(aNum, aBase: TCCUInt32): TCCUInt32; {$IFDEF INLINE_ASM} inline; {$ENDIF}
  var
    Limit, Mask: TCCUInt32;
  begin
    result := 0;
    if aNum <> 0 then
      begin
        Limit := 1 shl aNum;
        Mask := 1;
        while Mask < Limit do
          begin
            if GetBit <> 0 then
                inc(result, Mask);
            Mask := Mask shl 1;
          end;
      end;
    inc(result, aBase);
  end;
  function DecodeSymbol(const aTree: TTree): TCCUInt32; {$IFDEF INLINE_ASM} inline; {$ENDIF}
  var
    Sum, c, l: TCCInt32;
  begin
    Sum := 0;
    c := 0;
    l := 0;
    repeat
      c := (c * 2) + TCCInt32(GetBit);
      inc(l);
      inc(Sum, aTree.Table[l]);
      dec(c, aTree.Table[l]);
    until not(c >= 0);
    result := aTree.Translation[Sum + c];
  end;
  procedure DecodeTrees(var aLT, aDT: TTree);
  var
    hlit, hdist, hclen, i, Num, Len, clen, Symbol, Prev: TCCUInt32;
  begin
    FillPtrByte(@fCodeTree, sizeof(TTree), 0);
    FillPtrByte(@fLengths, sizeof(TLengths), 0);
    hlit := ReadBits(5, 257);
    hdist := ReadBits(5, 1);
    hclen := ReadBits(4, 4);
    for i := 0 to 18 do
        fLengths[i] := 0;
    for i := 1 to hclen do
      begin
        clen := ReadBits(3, 0);
        fLengths[CLCIndex[i - 1]] := clen;
      end;
    BuildTree(fCodeTree, TCCPointer(@fLengths[0]), 19);
    Num := 0;
    while Num < (hlit + hdist) do
      begin
        Symbol := DecodeSymbol(fCodeTree);
        case Symbol of
          16:
            begin
              Prev := fLengths[Num - 1];
              Len := ReadBits(2, 3);
              while Len > 0 do
                begin
                  fLengths[Num] := Prev;
                  inc(Num);
                  dec(Len);
                end;
            end;
          17:
            begin
              Len := ReadBits(3, 3);
              while Len > 0 do
                begin
                  fLengths[Num] := 0;
                  inc(Num);
                  dec(Len);
                end;
            end;
          18:
            begin
              Len := ReadBits(7, 11);
              while Len > 0 do
                begin
                  fLengths[Num] := 0;
                  inc(Num);
                  dec(Len);
                end;
            end;
          else
            begin
              fLengths[Num] := Symbol;
              inc(Num);
            end;
        end;
      end;
    BuildTree(aLT, TCCPointer(@fLengths[0]), hlit);
    BuildTree(aDT, TCCPointer(@fLengths[hlit]), hdist);
  end;
  function InflateBlockData(const aLT, aDT: TTree): boolean;
  var
    Symbol               : TCCUInt32;
    Len, Distance, Offset: TCCInt32;
    t                    : PCCUInt8;
  begin
    result := false;
    while (TCCPtrUInt(TCCPointer(Source)) < TCCPtrUInt(TCCPointer(SourceEnd))) or (BitCount > 0) do
      begin
        Symbol := DecodeSymbol(aLT);
        if Symbol = 256 then
          begin
            result := true;
            break;
          end;
        if Symbol < 256 then
          begin
            if (DestLen + 1) <= aOutLimit then
              begin
                Dest^ := TCCUInt8(Symbol);
                inc(Dest);
                inc(DestLen);
              end
            else
                exit;
          end
        else
          begin
            dec(Symbol, 257);
            Len := ReadBits(fLengthBits[Symbol], fLengthBase[Symbol]);
            Distance := DecodeSymbol(aDT);
            Offset := ReadBits(fDistanceBits[Distance], fDistanceBase[Distance]);
            if (DestLen + TCCSizeUInt(Len)) <= aOutLimit then
              begin
                t := TCCPointer(Dest);
                dec(t, Offset);
                RLELikeSideEffectAwareMemoryMove(t^, Dest^, Len);
                inc(Dest, Len);
                inc(DestLen, Len);
              end
            else
                exit;
          end;
      end;
  end;
  function InflateUncompressedBlock: boolean;
  var
    Len, InvLen: TCCUInt32;
  begin
    result := false;
    Len := (TCCUInt8(PCCUInt8Array(Source)^[1]) shl 8) or TCCUInt8(PCCUInt8Array(Source)^[0]);
    InvLen := (TCCUInt8(PCCUInt8Array(Source)^[3]) shl 8) or TCCUInt8(PCCUInt8Array(Source)^[2]);
    if Len <> ((not InvLen) and $FFFF) then
        exit;
    inc(Source, 4);
    if Len > 0 then
      begin
        if (DestLen + Len) < aOutLimit then
          begin
            Move(Source^, Dest^, Len);
            inc(Source, Len);
            inc(Dest, Len);
          end
        else
            exit;
      end;
    BitCount := 0;
    inc(DestLen, Len);
    result := true;
  end;
  function InflateFixedBlock: boolean;
  begin
    result := InflateBlockData(fFixedSymbolLengthTree, fFixedDistanceTree);
  end;
  function InflateDynamicBlock: boolean;
  begin
    FillPtrByte(@fSymbolLengthTree, sizeof(TTree), 0);
    FillPtrByte(@fDistanceTree, sizeof(TTree), 0);
    DecodeTrees(fSymbolLengthTree, fDistanceTree);
    result := InflateBlockData(fSymbolLengthTree, fDistanceTree);
  end;
  function Uncompress: boolean;
  var
    FinalBlock: boolean;
    BlockType : TCCUInt32;
  begin
    BitCount := 0;
    repeat
      FinalBlock := GetBit <> 0;
      BlockType := ReadBits(2, 0);
      case BlockType of
        0:
          begin
            result := InflateUncompressedBlock;
          end;
        1:
          begin
            result := InflateFixedBlock;
          end;
        2:
          begin
            result := InflateDynamicBlock;
          end;
        else
          begin
            result := false;
          end;
      end;
    until FinalBlock or not result;
  end;
  function UncompressZLIB: boolean;
  var
    cmf, flg: TCCUInt8;
    a32     : TCCUInt32;
  begin
    result := false;
    Source := aInData;
    cmf := TCCUInt8(PCCUInt8Array(Source)^[0]);
    flg := TCCUInt8(PCCUInt8Array(Source)^[1]);
    if ((((cmf shl 8) + flg) mod 31) <> 0) or ((cmf and $F) <> 8) or ((cmf shr 4) > 7) or ((flg and $20) <> 0) then
      begin
        exit;
      end;
    a32 := (TCCUInt8(PCCUInt8Array(Source)^[aInSize - 4]) shl 24) or
      (TCCUInt8(PCCUInt8Array(Source)^[aInSize - 3]) shl 16) or
      (TCCUInt8(PCCUInt8Array(Source)^[aInSize - 2]) shl 8) or
      (TCCUInt8(PCCUInt8Array(Source)^[aInSize - 1]) shl 0);
    inc(Source, 2);
    SourceEnd := @PCCUInt8Array(Source)^[aInSize - 6];
    result := Uncompress;
    if not result then
      begin
        exit;
      end;
    result := Adler32(aOutData, DestLen) = a32;
  end;
  function UncompressDirect: boolean;
  begin
    Source := aInData;
    SourceEnd := @PCCUInt8Array(Source)^[aInSize];
    result := Uncompress;
  end;

begin
  Dest := aOutData;
  DestLen := 0;
  result := 0;
  if fWithHeader then
    begin
      if UncompressZLIB then
        begin
          result := DestLen;
        end;
    end
  else
    begin
      if UncompressDirect then
        begin
          result := DestLen;
        end;
    end;
end;

constructor TCompressorBRRC.Create;
begin
  inherited Create;
end;

destructor TCompressorBRRC.Destroy;
begin
  inherited Destroy;
end;

function TCompressorBRRC.Compress(const aInData: TCCPointer; const aInSize: TCCSizeUInt; const aOutData: TCCPointer; const aOutLimit: TCCSizeUInt): TCCSizeUInt;
var
  {$IFNDEF CPU64}Code, {$ENDIF}Range, Cache, CountFFBytes: TCCUInt32;
  {$IFDEF CPU64}Code                                     : TCCUInt64; {$ENDIF}
  Model                                                  : array [0 .. SizeModels - 1] of TCCUInt32;
  OK, FirstByte{$IFNDEF CPU64}, Carry{$ENDIF}            : boolean;
  DestLen                                                : TCCInt32;
  procedure EncoderShift;
  {$IFDEF CPU64}
  var
    Carry: boolean;
    {$ENDIF}
  begin
    {$IFDEF CPU64}
    Carry := PCCUInt64Record(TCCPointer(@Code))^.Hi <> 0; // or (Code shr 32)<>0; or also (Code and TCCUInt64($ffffffff00000000))<>0;
    {$ENDIF}
    if (Code < $FF000000) or Carry then
      begin
        if FirstByte then
          begin
            FirstByte := false;
          end
        else
          begin
            if TCCSizeUInt(DestLen) < TCCSizeUInt(aOutLimit) then
              begin
                PCCUInt8Array(aOutData)^[DestLen] := Cache + TCCUInt8(ord(Carry) and 1);
                inc(DestLen);
              end
            else
              begin
                OK := false;
                exit;
              end;
          end;
        while CountFFBytes <> 0 do
          begin
            dec(CountFFBytes);
            if TCCSizeUInt(DestLen) < TCCSizeUInt(aOutLimit) then
              begin
                PCCUInt8Array(aOutData)^[DestLen] := $FF + TCCUInt8(ord(Carry) and 1);
                inc(DestLen);
              end
            else
              begin
                OK := false;
                exit;
              end;
          end;
        Cache := (Code shr 24) and $FF;
      end
    else
      begin
        inc(CountFFBytes);
      end;
    Code := (Code shl 8){$IFDEF CPU64} and TCCUInt32($FFFFFFFF){$ENDIF};
    Carry := false;
  end;
  function EncodeBit(ModelIndex, Move, Bit: TCCInt32): TCCInt32;
  var
    Bound{$IFNDEF CPU64}, OldCode{$ENDIF}: TCCUInt32;
  begin
    Bound := (Range shr 12) * Model[ModelIndex];
    if Bit = 0 then
      begin
        Range := Bound;
        inc(Model[ModelIndex], (4096 - Model[ModelIndex]) shr Move);
      end
    else
      begin
        {$IFNDEF CPU64}
        OldCode := Code;
        {$ENDIF}
        inc(Code, Bound);
        {$IFNDEF CPU64}
        Carry := Carry or (Code < OldCode);
        {$ENDIF}
        dec(Range, Bound);
        dec(Model[ModelIndex], Model[ModelIndex] shr Move);
      end;
    while Range < $1000000 do
      begin
        Range := Range shl 8;
        EncoderShift;
      end;
    result := Bit;
  end;
  procedure EncoderFlush;
  var
    Counter: TCCInt32;
  begin
    for Counter := 1 to 5 do
        EncoderShift;
  end;
  procedure EncodeTree(ModelIndex, Bits, Move, Value: TCCInt32);
  var
    Context: TCCInt32;
  begin
    Context := 1;
    while Bits > 0 do
      begin
        dec(Bits);
        Context := (Context shl 1) or EncodeBit(ModelIndex + Context, Move, (Value shr Bits) and 1);
      end;
  end;

var
  CurrentPointer, EndPointer: PCCUInt8;
  Len, MinDestLen           : TCCInt32;
begin
  DestLen := 0;
  FirstByte := true;
  OK := true;
  CountFFBytes := 0;
  Range := $FFFFFFFF;
  Code := 0;
  for Len := 0 to SizeModels - 1 do
    begin
      Model[Len] := 2048;
    end;
  CurrentPointer := aInData;
  EndPointer := TCCPointer(TCCPtrUInt(TCCPtrUInt(CurrentPointer) + TCCPtrUInt(aInSize)));
  while TCCPtrUInt(CurrentPointer) < TCCPtrUInt(EndPointer) do
    begin
      EncodeBit(FlagModel, 1, 1);
      EncodeTree(LiteralModel, 8, 4, PCCUInt8(CurrentPointer)^);
      if not OK then
        begin
          break;
        end;
      inc(CurrentPointer);
    end;
  EncodeBit(FlagModel, 1, 0);
  MinDestLen := Max(2, DestLen + 1);
  EncoderFlush;
  if OK then
    begin
      while (DestLen > MinDestLen) and (PCCUInt8Array(aOutData)^[DestLen - 1] = 0) do
          dec(DestLen);
      result := DestLen;
    end
  else
      result := 0;
end;

function TCompressorBRRC.Decompress(const aInData: TCCPointer; const aInSize: TCCSizeUInt; const aOutData: TCCPointer; const aOutLimit: TCCSizeUInt): TCCSizeUInt;
var
  Code, Range, Position: TCCUInt32;
  Model                : array [0 .. SizeModels - 1] of TCCUInt32;
  OK                   : boolean;
  function DecodeBit(ModelIndex, Move: TCCInt32): TCCInt32;
  var
    Bound: TCCUInt32;
  begin
    Bound := (Range shr 12) * Model[ModelIndex];
    if Code < Bound then
      begin
        Range := Bound;
        inc(Model[ModelIndex], (4096 - Model[ModelIndex]) shr Move);
        result := 0;
      end
    else
      begin
        dec(Code, Bound);
        dec(Range, Bound);
        dec(Model[ModelIndex], Model[ModelIndex] shr Move);
        result := 1;
      end;
    while Range < $1000000 do
      begin
        if Position < aInSize then
            Code := (Code shl 8) or PCCUInt8Array(aInData)^[Position]
        else
          begin
            if Position < (aInSize + 4 + 5) then
                Code := Code shl 8
            else
              begin
                OK := false;
                break;
              end;
          end;
        inc(Position);
        Range := Range shl 8;
      end;
  end;
  function DecodeTree(ModelIndex, MaxValue, Move: TCCInt32): TCCInt32;
  begin
    result := 1;
    while OK and (result < MaxValue) do
        result := (result shl 1) or DecodeBit(ModelIndex + result, Move);
    dec(result, MaxValue);
  end;

var
  DestLen, Value: TCCInt32;
begin
  result := 0;
  if aInSize >= 3 then
    begin
      OK := true;
      Code := (PCCUInt8Array(aInData)^[0] shl 24) or
        (PCCUInt8Array(aInData)^[1] shl 16) or
        (PCCUInt8Array(aInData)^[2] shl 8) or
        (PCCUInt8Array(aInData)^[3] shl 0);
      Position := 4;
      Range := $FFFFFFFF;
      for Value := 0 to SizeModels - 1 do
        begin
          Model[Value] := 2048;
        end;
      DestLen := 0;
      repeat
        Value := DecodeBit(FlagModel, 1);
        if OK then
          begin
            if Value <> 0 then
              begin
                Value := DecodeTree(LiteralModel, 256, 4);
                if OK and (TCCSizeUInt(DestLen) < TCCSizeUInt(aOutLimit)) then
                  begin
                    PCCUInt8Array(aOutData)^[DestLen] := Value;
                    inc(DestLen);
                  end
                else
                    exit;
              end
            else
                break;
          end
        else
            exit;
      until false;

      result := DestLen;
    end;
end;

end.
