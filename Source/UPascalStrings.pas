{ ***************************************************************************** }
{ * fpc Unicode string support,writen by QQ 600585@qq.com                      * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }

unit UPascalStrings;

interface

{$INCLUDE zDefine.inc}


uses SysUtils, PascalStrings;

type
{$IFDEF FPC}
  USystemChar   = UnicodeChar;
  USystemString = UnicodeString;
{$ELSE FPC}
  USystemChar   = PascalStrings.SystemChar;
  USystemString = PascalStrings.SystemString;
{$ENDIF FPC}
  PUSystemString = ^USystemString;
  PUPascalString = ^TUPascalString;
  TUArrayChar    = packed array of USystemChar;
  TUOrdChar      = (uc0to9, uc1to9, uc0to32, uc0to32no10, ucLoAtoF, ucHiAtoF, ucLoAtoZ, ucHiAtoZ, ucHex, ucAtoF, ucAtoZ);
  TUOrdChars     = set of TUOrdChar;
  TUHash         = Cardinal;
  TUHash64       = UInt64;

  TUPascalString = record
  private
    function GetText: USystemString;
    procedure SetText(const Value: USystemString);
    function GetLen: Integer;
    procedure SetLen(const Value: Integer);
    function GetChars(index: Integer): USystemChar;
    procedure SetChars(index: Integer; const Value: USystemChar);
    function GetBytes: TBytes;
    procedure SetBytes(const Value: TBytes);
    function GetLast: USystemChar;
    procedure SetLast(const Value: USystemChar);
    function GetFirst: USystemChar;
    procedure SetFirst(const Value: USystemChar);
  public
    buff: TUArrayChar;

{$IFDEF DELPHI}
    class operator Equal(const Lhs, Rhs: TUPascalString): Boolean;
    class operator NotEqual(const Lhs, Rhs: TUPascalString): Boolean;
    class operator GreaterThan(const Lhs, Rhs: TUPascalString): Boolean;
    class operator GreaterThanOrEqual(const Lhs, Rhs: TUPascalString): Boolean;
    class operator LessThan(const Lhs, Rhs: TUPascalString): Boolean;
    class operator LessThanOrEqual(const Lhs, Rhs: TUPascalString): Boolean;

    class operator Add(const Lhs, Rhs: TUPascalString): TUPascalString;
    class operator Add(const Lhs: USystemString; const Rhs: TUPascalString): TUPascalString;
    class operator Add(const Lhs: TUPascalString; const Rhs: USystemString): TUPascalString;
    class operator Add(const Lhs: USystemChar; const Rhs: TUPascalString): TUPascalString;
    class operator Add(const Lhs: TUPascalString; const Rhs: USystemChar): TUPascalString;

    class operator Implicit(Value: TPascalString): TUPascalString;
    class operator Implicit(Value: Variant): TUPascalString;
    class operator Implicit(Value: USystemString): TUPascalString;
    class operator Implicit(Value: USystemChar): TUPascalString;
    class operator Implicit(Value: TUPascalString): USystemString;
    class operator Implicit(Value: TUPascalString): Variant;

    class operator Explicit(Value: TUPascalString): TPascalString;
    class operator Explicit(Value: TUPascalString): USystemString;
    class operator Explicit(Value: TUPascalString): Variant;
    class operator Explicit(Value: USystemString): TUPascalString;
    class operator Explicit(Value: Variant): TUPascalString;
    class operator Explicit(Value: USystemChar): TUPascalString;
{$ENDIF}
    function Copy(index, Count: nativeInt): TUPascalString;
    function Same(const p: PUPascalString): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Same(const T: TUPascalString): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Same(const IgnoreCase: Boolean; const T: TUPascalString): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ComparePos(const Offset: Integer; const p: PUPascalString): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ComparePos(const Offset: Integer; const T: TUPascalString): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function GetPos(const s: TUPascalString; const Offset: Integer = 1): Integer; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function GetPos(const s: PUPascalString; const Offset: Integer = 1): Integer; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Exists(C: USystemChar): Boolean; overload;
    function Exists(C: array of USystemChar): Boolean; overload;
    function Exists(const s: TUPascalString): Boolean; overload;
    function GetCharCount(C: USystemChar): Integer;
    //
    function hash: TUHash; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Hash64: TUHash64; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    //
    property Last: USystemChar read GetLast write SetLast;
    property First: USystemChar read GetFirst write SetFirst;

    procedure DeleteLast; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure DeleteFirst; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure Delete(idx, cnt: Integer); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure Clear; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure Append(T: TUPascalString); overload;
    procedure Append(C: USystemChar); overload;
    function GetString(bPos, ePos: nativeInt): TUPascalString; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure Insert(AText: USystemString; idx: Integer); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    //
    procedure FastAsText(var output: USystemString);
    procedure FastGetBytes(var output: TBytes); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    //
    property Text: USystemString read GetText write SetText;
    function LowerText: USystemString;
    function UpperText: USystemString;
    function Invert: TUPascalString;
    function TrimChar(const Chars: TUPascalString): TUPascalString;
    function DeleteChar(const Chars: TUPascalString): TUPascalString; overload;
    function DeleteChar(const Chars: TUOrdChars): TUPascalString; overload;
    function ReplaceChar(const Chars: TUPascalString; const newChar: USystemChar): TUPascalString; overload;
    function ReplaceChar(const Chars, newChar: USystemChar): TUPascalString; overload;
    function ReplaceChar(const Chars: TUOrdChars; const newChar: USystemChar): TUPascalString; overload;

    { https://en.wikipedia.org/wiki/Smith%E2%80%93Waterman_algorithm }
    function SmithWaterman(const p: PUPascalString): Double; overload;
    function SmithWaterman(const s: TUPascalString): Double; overload;

    property Len: Integer read GetLen write SetLen;
    property Chars[index: Integer]: USystemChar read GetChars write SetChars; default;
    property Bytes: TBytes read GetBytes write SetBytes;
    function BOMBytes: TBytes;
  end;

  TUArrayPascalString = array of TUPascalString;
  PUArrayPascalString = ^TUArrayPascalString;

  TUArrayPascalStringPtr = array of PUPascalString;
  PUArrayPascalStringPtr = ^TUArrayPascalStringPtr;

function UCharIn(C: USystemChar; const SomeChars: array of USystemChar): Boolean; overload;
function UCharIn(C: USystemChar; const SomeChar: USystemChar): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function UCharIn(C: USystemChar; const s: TUPascalString): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function UCharIn(C: USystemChar; const p: PUPascalString): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function UCharIn(C: USystemChar; const SomeCharsets: TUOrdChars): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function UCharIn(C: USystemChar; const SomeCharset: TUOrdChar): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function UCharIn(C: USystemChar; const SomeCharsets: TUOrdChars; const SomeChars: TUPascalString): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function UCharIn(C: USystemChar; const SomeCharsets: TUOrdChars; const p: PUPascalString): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function UFastHashSystemString(const s: PSystemString): TUHash; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function UFastHash64SystemString(const s: PSystemString): TUHash64; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function UFastHashSystemString(const s: SystemString): TUHash; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function UFastHash64SystemString(const s: SystemString): TUHash64; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function UFastHashPascalString(const s: PPascalString): TUHash; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function UFastHash64PascalString(const s: PPascalString): TUHash64; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function UFormat(const Fmt: USystemString; const Args: array of const): USystemString;

{$IFDEF FPC}

operator := (const s: Variant)R: TUPascalString;
operator := (const s: AnsiString)R: TUPascalString;
operator := (const s: UnicodeString)R: TUPascalString;
operator := (const s: WideString)R: TUPascalString;
operator := (const s: ShortString)R: TUPascalString;
operator := (const C: USystemChar)R: TUPascalString;
operator := (const C: TPascalString)R: TUPascalString;

operator := (const s: TUPascalString)R: AnsiString;
operator := (const s: TUPascalString)R: UnicodeString;
operator := (const s: TUPascalString)R: WideString;
operator := (const s: TUPascalString)R: ShortString;
operator := (const s: TUPascalString)R: Variant;
operator := (const s: TUPascalString)R: TPascalString;

operator = (const A: TUPascalString; const b: TUPascalString): Boolean;
operator <> (const A: TUPascalString; const b: TUPascalString): Boolean;
operator > (const A: TUPascalString; const b: TUPascalString): Boolean;
operator >= (const A: TUPascalString; const b: TUPascalString): Boolean;
operator < (const A: TUPascalString; const b: TUPascalString): Boolean;
operator <= (const A: TUPascalString; const b: TUPascalString): Boolean;

operator + (const A: TUPascalString; const b: TUPascalString): TUPascalString;
operator + (const A: TUPascalString; const b: USystemString): TUPascalString;
operator + (const A: USystemString; const b: TUPascalString): TUPascalString;
operator + (const A: TUPascalString; const b: USystemChar): TUPascalString;
operator + (const A: USystemChar; const b: TUPascalString): TUPascalString;

{$ENDIF}

{ https://en.wikipedia.org/wiki/Smith%E2%80%93Waterman_algorithm }

// short string likeness and out diff
function USmithWatermanCompare(const seq1, seq2: PUPascalString; var diff1, diff2: TUPascalString;
  const NoDiffChar: Boolean; const diffChar: USystemChar): Double; overload;
function USmithWatermanCompare(const seq1, seq2: PUPascalString; var diff1, diff2: TUPascalString): Double; overload;
function USmithWatermanCompare(const seq1, seq2: TUPascalString; var diff1, diff2: TUPascalString;
  const NoDiffChar: Boolean; const diffChar: USystemChar): Double; overload;
function USmithWatermanCompare(const seq1, seq2: TUPascalString; var diff1, diff2: TUPascalString): Double; overload;

// short string likeness
function USmithWatermanCompare(const seq1, seq2: PUPascalString; out Same, Diff: Integer): Double; overload;
function USmithWatermanCompare(const seq1, seq2: PUPascalString): Double; overload;
function USmithWatermanCompare(const seq1, seq2: TUPascalString): Double; overload;
function USmithWatermanCompare(const seq1: TUArrayPascalString; const seq2: TUPascalString): Double; overload;

// memory likeness
function USmithWatermanCompare(const seq1: Pointer; siz1: Integer; const seq2: Pointer; siz2: Integer;
  out Same, Diff: Integer): Double; overload;
function USmithWatermanCompare(const seq1: Pointer; siz1: Integer; const seq2: Pointer; siz2: Integer): Double; overload;

// long string likeness
function USmithWatermanCompareLongString(const t1, t2: TUPascalString; const MinDiffCharWithPeerLine: Integer; out Same, Diff: Integer): Double; overload;
function USmithWatermanCompareLongString(const t1, t2: TUPascalString): Double; overload;

var
  USystemCharSize: nativeInt = SizeOf(USystemChar);
{$IFDEF CPU64}
  UMaxSmithWatermanMatrix: nativeInt = 10000 * 10;
{$ELSE}
  UMaxSmithWatermanMatrix: nativeInt = 8192;
{$ENDIF}


const
{$IFDEF FirstCharInZero}
  UFirstCharPos = 0;
{$ELSE}
  UFirstCharPos = 1;
{$ENDIF}

implementation

uses CoreClasses, Variants;

procedure CombineCharsPP(const c1, c2: TUArrayChar; var output: TUArrayChar); {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  LL, rl: Integer;
begin
  LL := length(c1);
  rl := length(c2);
  SetLength(output, LL + rl);
  if LL > 0 then
      CopyPtr(@c1[0], @output[0], LL * USystemCharSize);
  if rl > 0 then
      CopyPtr(@c2[0], @output[LL], rl * USystemCharSize);
end;

procedure CombineCharsSP(const c1: USystemString; const c2: TUArrayChar; var output: TUArrayChar); {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  LL, rl: Integer;
begin
  LL := length(c1);
  rl := length(c2);
  SetLength(output, LL + rl);
  if LL > 0 then
      CopyPtr(@c1[UFirstCharPos], @output[0], LL * USystemCharSize);
  if rl > 0 then
      CopyPtr(@c2[0], @output[LL], rl * USystemCharSize);
end;

procedure CombineCharsPS(const c1: TUArrayChar; const c2: USystemString; var output: TUArrayChar); {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  LL, rl: Integer;
begin
  LL := length(c1);
  rl := length(c2);
  SetLength(output, LL + rl);
  if LL > 0 then
      CopyPtr(@c1[0], @output[0], LL * USystemCharSize);
  if rl > 0 then
      CopyPtr(@c2[UFirstCharPos], @output[LL], rl * USystemCharSize);
end;

procedure CombineCharsCP(const c1: USystemChar; const c2: TUArrayChar; var output: TUArrayChar); {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  rl: Integer;
begin
  rl := length(c2);
  SetLength(output, rl + 1);
  output[0] := c1;
  if rl > 0 then
      CopyPtr(@c2[0], @output[1], rl * USystemCharSize);
end;

procedure CombineCharsPC(const c1: TUArrayChar; const c2: USystemChar; var output: TUArrayChar); {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  LL: Integer;
begin
  LL := length(c1);
  SetLength(output, LL + 1);
  if LL > 0 then
      CopyPtr(@c1[0], @output[0], LL * USystemCharSize);
  output[LL] := c2;
end;

function UCharIn(C: USystemChar; const SomeChars: array of USystemChar): Boolean;
var
  AChar: USystemChar;
begin
  Result := True;
  for AChar in SomeChars do
    if AChar = C then
        Exit;
  Result := False;
end;

function UCharIn(C: USystemChar; const SomeChar: USystemChar): Boolean;
begin
  Result := C = SomeChar;
end;

function UCharIn(C: USystemChar; const s: TUPascalString): Boolean;
begin
  Result := s.Exists(C);
end;

function UCharIn(C: USystemChar; const p: PUPascalString): Boolean;
begin
  Result := p^.Exists(C);
end;

function UCharIn(C: USystemChar; const SomeCharset: TUOrdChar): Boolean;
const
  ord0  = Ord('0');
  ord1  = Ord('1');
  ord9  = Ord('9');
  ordLA = Ord('a');
  ordHA = Ord('A');
  ordLF = Ord('f');
  ordHF = Ord('F');
  ordLZ = Ord('z');
  ordHZ = Ord('Z');

var
  v: Word;
begin
  v := Ord(C);
  case SomeCharset of
    uc0to9: Result := (v >= ord0) and (v <= ord9);
    uc1to9: Result := (v >= ord1) and (v <= ord9);
    uc0to32: Result := ((v >= 0) and (v <= 32));
    uc0to32no10: Result := ((v >= 0) and (v <= 32) and (v <> 10));
    ucLoAtoF: Result := (v >= ordLA) and (v <= ordLF);
    ucHiAtoF: Result := (v >= ordHA) and (v <= ordHF);
    ucLoAtoZ: Result := (v >= ordLA) and (v <= ordLZ);
    ucHiAtoZ: Result := (v >= ordHA) and (v <= ordHZ);
    ucHex: Result := ((v >= ordLA) and (v <= ordLF)) or ((v >= ordHA) and (v <= ordHF)) or ((v >= ord0) and (v <= ord9));
    ucAtoF: Result := ((v >= ordLA) and (v <= ordLF)) or ((v >= ordHA) and (v <= ordHF));
    ucAtoZ: Result := ((v >= ordLA) and (v <= ordLZ)) or ((v >= ordHA) and (v <= ordHZ));
    else Result := False;
  end;
end;

function UCharIn(C: USystemChar; const SomeCharsets: TUOrdChars): Boolean;
var
  i: TUOrdChar;
begin
  Result := True;
  for i in SomeCharsets do
    if UCharIn(C, i) then
        Exit;
  Result := False;
end;

function UCharIn(C: USystemChar; const SomeCharsets: TUOrdChars; const SomeChars: TUPascalString): Boolean;
begin
  if UCharIn(C, SomeCharsets) then
      Result := True
  else
      Result := UCharIn(C, SomeChars);
end;

function UCharIn(C: USystemChar; const SomeCharsets: TUOrdChars; const p: PUPascalString): Boolean;
begin
  if UCharIn(C, SomeCharsets) then
      Result := True
  else
      Result := UCharIn(C, p);
end;

function UFastHashSystemString(const s: PSystemString): TUHash;
var
  i: Integer;
  C: USystemChar;
begin
  Result := 0;

{$IFDEF FirstCharInZero}
  for i := 0 to length(s^) - 1 do
{$ELSE}
  for i := 1 to length(s^) do
{$ENDIF}
    begin
      C := s^[i];
      if UCharIn(C, ucHiAtoZ) then
          Inc(C, 32);
      Result := ((Result shl 7) or (Result shr 25)) + TUHash(C);
    end;
end;

function UFastHash64SystemString(const s: PSystemString): TUHash64;
var
  i: Integer;
  C: USystemChar;
begin
  Result := 0;

{$IFDEF FirstCharInZero}
  for i := 0 to length(s^) - 1 do
{$ELSE}
  for i := 1 to length(s^) do
{$ENDIF}
    begin
      C := s^[i];
      if UCharIn(C, ucHiAtoZ) then
          Inc(C, 32);
      Result := ((Result shl 7) or (Result shr 57)) + TUHash64(C);
    end;
end;

function UFastHashSystemString(const s: SystemString): TUHash;
begin
  Result := UFastHashSystemString(@s);
end;

function UFastHash64SystemString(const s: SystemString): TUHash64;
begin
  Result := UFastHash64SystemString(@s);
end;

function UFastHashPascalString(const s: PPascalString): TUHash;
var
  i: Integer;
  C: USystemChar;
begin
  Result := 0;
  for i := 1 to s^.Len do
    begin
      C := s^[i];
      if UCharIn(C, ucHiAtoZ) then
          Inc(C, 32);
      Result := ((Result shl 7) or (Result shr 25)) + TUHash(C);
    end;
end;

function UFastHash64PascalString(const s: PPascalString): TUHash64;
var
  i: Integer;
  C: USystemChar;
begin
  Result := 0;
  for i := 1 to s^.Len do
    begin
      C := s^[i];
      if UCharIn(C, ucHiAtoZ) then
          Inc(C, 32);
      Result := ((Result shl 7) or (Result shr 57)) + TUHash64(C);
    end;
end;

function UFormat(const Fmt: USystemString; const Args: array of const): USystemString;
begin
{$IFDEF FPC}
  Result := UnicodeFormat(Fmt, Args);
{$ELSE FPC}
  Result := Format(Fmt, Args);
{$ENDIF FPC}
end;

function BytesOfPascalString(const s: TUPascalString): TBytes;
begin
  Result := s.Bytes;
end;

function PascalStringOfBytes(const s: TBytes): TUPascalString;
begin
  Result.Bytes := s;
end;

function GetSWMVMemory(const xLen, yLen: nativeInt): Pointer; inline;
{ optimized matrix performance }
begin
  Result := System.AllocMem((xLen + 1) * (yLen + 1) * SizeOf(nativeInt));
end;

function GetSWMV(const p: Pointer; const w, X, Y: nativeInt): nativeInt; inline;
{ optimized matrix performance }
begin
  Result := PNativeInt(nativeUInt(p) + ((X + Y * (w + 1)) * SizeOf(nativeInt)))^;
end;

procedure SetSWMV(const p: Pointer; const w, X, Y: nativeInt; const v: nativeInt); inline;
{ optimized matrix performance }
begin
  PNativeInt(nativeUInt(p) + ((X + Y * (w + 1)) * SizeOf(nativeInt)))^ := v;
end;

function GetMax(const i1, i2: nativeInt): nativeInt; inline;
begin
  if i1 > i2 then
      Result := i1
  else
      Result := i2;
end;

const
  SmithWaterman_MatchOk = 1;
  mismatch_penalty      = -1;
  gap_penalty           = -1;

function USmithWatermanCompare(const seq1, seq2: PUPascalString; var diff1, diff2: TUPascalString;
  const NoDiffChar: Boolean; const diffChar: USystemChar): Double;

  function InlineMatch(alphaC, betaC: USystemChar; const diffC: USystemChar): Integer; inline;
  begin
    if UCharIn(alphaC, ucLoAtoZ) then
        Dec(alphaC, 32);
    if UCharIn(betaC, ucLoAtoZ) then
        Dec(betaC, 32);

    if alphaC = betaC then
        Result := SmithWaterman_MatchOk
    else if (alphaC = diffC) or (betaC = diffC) then
        Result := gap_penalty
    else
        Result := mismatch_penalty;
  end;

var
  swMatrixPtr: Pointer;
  i, J, L1, l2: nativeInt;
  matched, deleted, inserted: nativeInt;
  score_current, score_diagonal, score_left, score_right: nativeInt;
  identity: nativeInt;
  align1, align2: TUPascalString;
begin
  L1 := seq1^.Len;
  l2 := seq2^.Len;

  if (L1 = 0) or (l2 = 0) or (L1 > UMaxSmithWatermanMatrix) or (l2 > UMaxSmithWatermanMatrix) then
    begin
      Result := -1;
      Exit;
    end;

  { fast build matrix }
  swMatrixPtr := GetSWMVMemory(L1, l2);
  if swMatrixPtr = nil then
    begin
      diff1 := '';
      diff2 := '';
      Result := -1;
      Exit;
    end;

  i := 0;
  while i <= L1 do
    begin
      SetSWMV(swMatrixPtr, L1, i, 0, gap_penalty * i);
      Inc(i);
    end;

  J := 0;
  while J <= l2 do
    begin
      SetSWMV(swMatrixPtr, L1, 0, J, gap_penalty * J);
      Inc(J);
    end;

  { compute matrix }
  i := 1;
  while i <= L1 do
    begin
      J := 1;
      while J <= l2 do
        begin
          matched := GetSWMV(swMatrixPtr, L1, i - 1, J - 1) + InlineMatch(seq1^[i], seq2^[J], diffChar);
          deleted := GetSWMV(swMatrixPtr, L1, i - 1, J) + gap_penalty;
          inserted := GetSWMV(swMatrixPtr, L1, i, J - 1) + gap_penalty;
          SetSWMV(swMatrixPtr, L1, i, J, GetMax(matched, GetMax(deleted, inserted)));
          Inc(J);
        end;
      Inc(i);
    end;

  { compute align }
  i := L1;
  J := l2;
  align1 := '';
  align2 := '';
  identity := 0;
  while (i > 0) and (J > 0) do
    begin
      score_current := GetSWMV(swMatrixPtr, L1, i, J);
      score_diagonal := GetSWMV(swMatrixPtr, L1, i - 1, J - 1);
      score_left := GetSWMV(swMatrixPtr, L1, i - 1, J);
      score_right := GetSWMV(swMatrixPtr, L1, i, J - 1);

      matched := InlineMatch(seq1^[i], seq2^[J], diffChar);

      if score_current = score_diagonal + matched then
        begin
          if matched = SmithWaterman_MatchOk then
            begin
              Inc(identity);
              align1.Append(seq1^[i]);
              align2.Append(seq2^[J]);
            end
          else if NoDiffChar then
            begin
              align1.Append(diffChar);
              align2.Append(diffChar);
            end
          else
            begin
              align1.Append(seq1^[i]);
              align2.Append(seq2^[J]);
            end;
          Dec(i);
          Dec(J);
        end
      else if score_current = score_left + gap_penalty then
        begin
          if NoDiffChar then
              align1.Append(diffChar)
          else
              align1.Append(seq1^[i]);
          align2.Append(diffChar);
          Dec(i);
        end
      else if score_current = score_right + gap_penalty then
        begin
          if NoDiffChar then
              align2.Append(diffChar)
          else
              align2.Append(seq2^[J]);
          align1.Append(diffChar);
          Dec(J);
        end
      else
          raise Exception.Create('matrix error'); // matrix debug time
    end;

  System.FreeMemory(swMatrixPtr);

  while i > 0 do
    begin
      if NoDiffChar then
          align1.Append(diffChar)
      else
          align1.Append(seq1^[i]);
      align2.Append(diffChar);
      Dec(i);
    end;

  while J > 0 do
    begin
      if NoDiffChar then
          align2.Append(diffChar)
      else
          align2.Append(seq2^[J]);
      align1.Append(diffChar);
      Dec(J);
    end;

  if identity > 0 then
      Result := identity / align1.Len
  else
      Result := -1;

  diff1 := align1.Invert;
  diff2 := align2.Invert;
end;

function USmithWatermanCompare(const seq1, seq2: PUPascalString; var diff1, diff2: TUPascalString): Double;
begin
  Result := USmithWatermanCompare(seq1, seq2, diff1, diff2, False, '-');
end;

function USmithWatermanCompare(const seq1, seq2: TUPascalString; var diff1, diff2: TUPascalString;
  const NoDiffChar: Boolean; const diffChar: USystemChar): Double;
begin
  Result := USmithWatermanCompare(@seq1, @seq2, diff1, diff2, NoDiffChar, diffChar);
end;

function USmithWatermanCompare(const seq1, seq2: TUPascalString; var diff1, diff2: TUPascalString): Double;
begin
  Result := USmithWatermanCompare(seq1, seq2, diff1, diff2, False, '-');
end;

function USmithWatermanCompare(const seq1, seq2: PUPascalString; out Same, Diff: Integer): Double;

  function InlineMatch(alphaC, betaC: USystemChar): nativeInt; inline;
  begin
    if UCharIn(alphaC, ucLoAtoZ) then
        Dec(alphaC, 32);
    if UCharIn(betaC, ucLoAtoZ) then
        Dec(betaC, 32);

    if alphaC = betaC then
        Result := SmithWaterman_MatchOk
    else
        Result := mismatch_penalty;
  end;

var
  swMatrixPtr: Pointer;
  i, J, L1, l2: nativeInt;
  matched, deleted, inserted: nativeInt;
  score_current, score_diagonal, score_left, score_right: nativeInt;
  identity, L: nativeInt;
begin
  L1 := seq1^.Len;
  l2 := seq2^.Len;

  if (L1 = 0) or (l2 = 0) or (L1 > UMaxSmithWatermanMatrix) or (l2 > UMaxSmithWatermanMatrix) then
    begin
      Result := -1;
      Same := 0;
      Diff := L1 + l2;
      Exit;
    end;

  { fast build matrix }
  swMatrixPtr := GetSWMVMemory(L1, l2);
  if swMatrixPtr = nil then
    begin
      Result := -1;
      Exit;
    end;

  i := 0;
  while i <= L1 do
    begin
      SetSWMV(swMatrixPtr, L1, i, 0, gap_penalty * i);
      Inc(i);
    end;

  J := 0;
  while J <= l2 do
    begin
      SetSWMV(swMatrixPtr, L1, 0, J, gap_penalty * J);
      Inc(J);
    end;

  { compute matrix }
  i := 1;
  while i <= L1 do
    begin
      J := 1;
      while J <= l2 do
        begin
          matched := GetSWMV(swMatrixPtr, L1, i - 1, J - 1) + InlineMatch(seq1^[i], seq2^[J]);
          deleted := GetSWMV(swMatrixPtr, L1, i - 1, J) + gap_penalty;
          inserted := GetSWMV(swMatrixPtr, L1, i, J - 1) + gap_penalty;
          SetSWMV(swMatrixPtr, L1, i, J, GetMax(matched, GetMax(deleted, inserted)));
          Inc(J);
        end;
      Inc(i);
    end;

  { compute align }
  i := L1;
  J := l2;
  identity := 0;
  L := 0;
  while (i > 0) and (J > 0) do
    begin
      score_current := GetSWMV(swMatrixPtr, L1, i, J);
      score_diagonal := GetSWMV(swMatrixPtr, L1, i - 1, J - 1);
      score_left := GetSWMV(swMatrixPtr, L1, i - 1, J);
      score_right := GetSWMV(swMatrixPtr, L1, i, J - 1);
      matched := InlineMatch(seq1^[i], seq2^[J]);

      if score_current = score_diagonal + matched then
        begin
          if matched = SmithWaterman_MatchOk then
              Inc(identity);

          Inc(L);
          Dec(i);
          Dec(J);
        end
      else if score_current = score_left + gap_penalty then
        begin
          Inc(L);
          Dec(i);
        end
      else if score_current = score_right + gap_penalty then
        begin
          Inc(L);
          Dec(J);
        end
      else
          raise Exception.Create('matrix error'); // matrix debug time
    end;

  System.FreeMemory(swMatrixPtr);

  if identity > 0 then
    begin
      Result := identity / (L + i + J);
      Same := identity;
      Diff := (L + i + J) - identity;
    end
  else
    begin
      Result := -1;
      Same := 0;
      Diff := L + i + J;
    end;
end;

function USmithWatermanCompare(const seq1, seq2: PUPascalString): Double;
var
  Same, Diff: Integer;
begin
  Result := USmithWatermanCompare(seq1, seq2, Same, Diff);
end;

function USmithWatermanCompare(const seq1, seq2: TUPascalString): Double;
begin
  Result := USmithWatermanCompare(@seq1, @seq2);
end;

function USmithWatermanCompare(const seq1: TUArrayPascalString; const seq2: TUPascalString): Double;
var
  i: Integer;
  R: Double;
begin
  Result := -1;
  for i := 0 to length(seq1) - 1 do
    begin
      R := USmithWatermanCompare(seq1[i], seq2);
      if R > Result then
          Result := R;
    end;
end;

function USmithWatermanCompare(const seq1: Pointer; siz1: Integer; const seq2: Pointer; siz2: Integer;
  out Same, Diff: Integer): Double;

  function InlineMatch(const alphaB, betaB: Byte): nativeInt; inline;
  begin
    if alphaB = betaB then
        Result := SmithWaterman_MatchOk
    else
        Result := mismatch_penalty;
  end;

var
  swMatrixPtr: Pointer;
  i, J, L1, l2: nativeInt;
  matched, deleted, inserted: nativeInt;
  score_current, score_diagonal, score_left, score_right: nativeInt;
  identity, L: nativeInt;
begin
  L1 := siz1;
  l2 := siz2;

  if (L1 = 0) or (l2 = 0) or (L1 > UMaxSmithWatermanMatrix) or (l2 > UMaxSmithWatermanMatrix) then
    begin
      Result := -1;
      Same := 0;
      Diff := L1 + l2;
      Exit;
    end;

  { fast build matrix }
  swMatrixPtr := GetSWMVMemory(L1, l2);
  if swMatrixPtr = nil then
    begin
      Result := -1;
      Exit;
    end;

  i := 0;
  while i <= L1 do
    begin
      SetSWMV(swMatrixPtr, L1, i, 0, gap_penalty * i);
      Inc(i);
    end;

  J := 0;
  while J <= l2 do
    begin
      SetSWMV(swMatrixPtr, L1, 0, J, gap_penalty * J);
      Inc(J);
    end;

  { compute matrix }
  i := 1;
  while i <= L1 do
    begin
      J := 1;
      while J <= l2 do
        begin
          matched := GetSWMV(swMatrixPtr, L1, i - 1, J - 1) + InlineMatch(PByte(nativeUInt(seq1) + (i - 1))^, PByte(nativeUInt(seq2) + (J - 1))^);
          deleted := GetSWMV(swMatrixPtr, L1, i - 1, J) + gap_penalty;
          inserted := GetSWMV(swMatrixPtr, L1, i, J - 1) + gap_penalty;
          SetSWMV(swMatrixPtr, L1, i, J, GetMax(matched, GetMax(deleted, inserted)));
          Inc(J);
        end;
      Inc(i);
    end;

  { compute align }
  i := L1;
  J := l2;
  identity := 0;
  L := 0;
  while (i > 0) and (J > 0) do
    begin
      score_current := GetSWMV(swMatrixPtr, L1, i, J);
      score_diagonal := GetSWMV(swMatrixPtr, L1, i - 1, J - 1);
      score_left := GetSWMV(swMatrixPtr, L1, i - 1, J);
      score_right := GetSWMV(swMatrixPtr, L1, i, J - 1);
      matched := InlineMatch(PByte(nativeUInt(seq1) + (i - 1))^, PByte(nativeUInt(seq2) + (J - 1))^);

      if score_current = score_diagonal + matched then
        begin
          if matched = SmithWaterman_MatchOk then
              Inc(identity);

          Inc(L);
          Dec(i);
          Dec(J);
        end
      else if score_current = score_left + gap_penalty then
        begin
          Inc(L);
          Dec(i);
        end
      else if score_current = score_right + gap_penalty then
        begin
          Inc(L);
          Dec(J);
        end
      else
          raise Exception.Create('matrix error'); // matrix debug time
    end;

  System.FreeMemory(swMatrixPtr);

  if identity > 0 then
    begin
      Result := identity / (L + i + J);
      Same := identity;
      Diff := (L + i + J) - identity;
    end
  else
    begin
      Result := -1;
      Same := 0;
      Diff := L + i + J;
    end;
end;

function USmithWatermanCompare(const seq1: Pointer; siz1: Integer; const seq2: Pointer; siz2: Integer): Double;
var
  Same, Diff: Integer;
begin
  Result := USmithWatermanCompare(seq1, siz1, seq2, siz2, Same, Diff);
end;

function USmithWatermanCompareLongString(const t1, t2: TUPascalString; const MinDiffCharWithPeerLine: Integer; out Same, Diff: Integer): Double;
type
  PSRec = ^TSRec;

  TSRec = packed record
    s: TUPascalString;
  end;

  procedure _FillText(psPtr: PUPascalString; outLst: TCoreClassList);
  var
    L, i: Integer;
    n: TUPascalString;
    p: PSRec;
  begin
    L := psPtr^.Len;
    i := 1;
    n := '';
    while i <= L do
      begin
        if UCharIn(psPtr^[i], [#13, #10]) then
          begin
            n := n.DeleteChar(#32#9);
            if n.Len > 0 then
              begin
                new(p);
                p^.s := n;
                outLst.Add(p);
                n := '';
              end;
            repeat
                Inc(i);
            until (i > L) or (not UCharIn(psPtr^[i], [#13, #10, #32, #9]));
          end
        else
          begin
            n.Append(psPtr^[i]);
            Inc(i);
          end;
      end;

    n := n.DeleteChar(#32#9);
    if n.Len > 0 then
      begin
        new(p);
        p^.s := n;
        outLst.Add(p);
      end;
  end;

  function InlineMatch(const alpha, beta: PSRec; const MinDiffCharWithPeerLine: Integer; var cSame, cDiff: Integer): nativeInt; inline;
  begin
    if USmithWatermanCompare(@alpha^.s, @beta^.s, cSame, cDiff) > 0 then
      begin
        if cDiff < MinDiffCharWithPeerLine then
            Result := SmithWaterman_MatchOk
        else
            Result := mismatch_penalty;
      end
    else
        Result := mismatch_penalty;
  end;

var
  lst1, lst2: TCoreClassList;

  procedure _Init;
  begin
    lst1 := TCoreClassList.Create;
    lst2 := TCoreClassList.Create;
    _FillText(@t1, lst1);
    _FillText(@t2, lst2);
  end;

  procedure _Free;
  var
    i: Integer;
  begin
    for i := 0 to lst1.Count - 1 do
        Dispose(PSRec(lst1[i]));
    for i := 0 to lst2.Count - 1 do
        Dispose(PSRec(lst2[i]));
    DisposeObject([lst1, lst2]);
  end;

var
  swMatrixPtr: Pointer;
  i, J, L1, l2: nativeInt;
  matched, deleted, inserted: nativeInt;
  score_current, score_diagonal, score_left, score_right: nativeInt;
  cSame, cDiff, TotalSame, TotalDiff: Integer;
begin
  _Init;
  L1 := lst1.Count;
  l2 := lst2.Count;

  if (L1 = 0) or (l2 = 0) or (L1 > UMaxSmithWatermanMatrix) or (l2 > UMaxSmithWatermanMatrix) then
    begin
      Result := -1;
      Same := 0;
      Diff := L1 + l2;
      _Free;
      Exit;
    end;

  { fast build matrix }
  swMatrixPtr := GetSWMVMemory(L1, l2);
  if swMatrixPtr = nil then
    begin
      Result := -1;
      _Free;
      Exit;
    end;

  i := 0;
  while i <= L1 do
    begin
      SetSWMV(swMatrixPtr, L1, i, 0, gap_penalty * i);
      Inc(i);
    end;

  J := 0;
  while J <= l2 do
    begin
      SetSWMV(swMatrixPtr, L1, 0, J, gap_penalty * J);
      Inc(J);
    end;

  { compute matrix }
  i := 1;
  while i <= L1 do
    begin
      J := 1;
      while J <= l2 do
        begin
          matched := GetSWMV(swMatrixPtr, L1, i - 1, J - 1) + InlineMatch(PSRec(lst1[i - 1]), PSRec(lst2[J - 1]), MinDiffCharWithPeerLine, cSame, cDiff);
          deleted := GetSWMV(swMatrixPtr, L1, i - 1, J) + gap_penalty;
          inserted := GetSWMV(swMatrixPtr, L1, i, J - 1) + gap_penalty;
          SetSWMV(swMatrixPtr, L1, i, J, GetMax(matched, GetMax(deleted, inserted)));
          Inc(J);
        end;
      Inc(i);
    end;

  { compute align }
  i := L1;
  J := l2;
  TotalSame := 0;
  TotalDiff := 0;
  while (i > 0) and (J > 0) do
    begin
      score_current := GetSWMV(swMatrixPtr, L1, i, J);
      score_diagonal := GetSWMV(swMatrixPtr, L1, i - 1, J - 1);
      score_left := GetSWMV(swMatrixPtr, L1, i - 1, J);
      score_right := GetSWMV(swMatrixPtr, L1, i, J - 1);
      matched := InlineMatch(PSRec(lst1[i - 1]), PSRec(lst2[J - 1]), MinDiffCharWithPeerLine, cSame, cDiff);

      Inc(TotalSame, cSame);
      Inc(TotalDiff, cDiff);

      if score_current = score_diagonal + matched then
        begin
          Dec(i);
          Dec(J);
        end
      else if score_current = score_left + gap_penalty then
        begin
          Dec(i);
        end
      else if score_current = score_right + gap_penalty then
        begin
          Dec(J);
        end
      else
          raise Exception.Create('matrix error'); // matrix debug time
    end;

  System.FreeMemory(swMatrixPtr);
  _Free;

  if TotalSame > 0 then
    begin
      Result := TotalSame / (TotalSame + TotalDiff);
      Same := TotalSame;
      Diff := TotalDiff;
    end
  else
    begin
      Result := -1;
      Same := 0;
      Diff := t2.Len + t1.Len;
    end;
end;

function USmithWatermanCompareLongString(const t1, t2: TUPascalString): Double;
var
  Same, Diff: Integer;
begin
  Result := USmithWatermanCompareLongString(t1, t2, 5, Same, Diff);
end;

{$IFDEF FPC}


operator := (const s: Variant)R: TUPascalString;
begin
  R.Text := s;
end;

operator := (const s: AnsiString)R: TUPascalString;
begin
  R.Text := s;
end;

operator := (const s: UnicodeString)R: TUPascalString;
begin
  R.Text := s;
end;

operator := (const s: WideString)R: TUPascalString;
begin
  R.Text := s;
end;

operator := (const s: ShortString)R: TUPascalString;
begin
  R.Text := s;
end;

operator := (const C: USystemChar)R: TUPascalString;
begin
  R.Text := C;
end;

operator := (const C: TPascalString)R: TUPascalString;
begin
  Result.Bytes := C.Bytes;
end;

operator := (const s: TUPascalString)R: AnsiString;
begin
  R := s.Text;
end;

operator := (const s: TUPascalString)R: UnicodeString;
begin
  R := s.Text;
end;

operator := (const s: TUPascalString)R: WideString;
begin
  R := s.Text;
end;

operator := (const s: TUPascalString)R: ShortString;
begin
  R := s.Text;
end;

operator := (const s: TUPascalString)R: Variant;
begin
  R := s.Text;
end;

operator := (const s: TUPascalString)R: TPascalString;
begin
  Result.Bytes := s.Bytes;
end;

operator = (const A: TUPascalString; const b: TUPascalString): Boolean;
begin
  Result := A.Text = b.Text;
end;

operator <> (const A: TUPascalString; const b: TUPascalString): Boolean;
begin
  Result := A.Text <> b.Text;
end;

operator > (const A: TUPascalString; const b: TUPascalString): Boolean;
begin
  Result := A.Text > b.Text;
end;

operator >= (const A: TUPascalString; const b: TUPascalString): Boolean;
begin
  Result := A.Text >= b.Text;
end;

operator < (const A: TUPascalString; const b: TUPascalString): Boolean;
begin
  Result := A.Text < b.Text;
end;

operator <= (const A: TUPascalString; const b: TUPascalString): Boolean;
begin
  Result := A.Text <= b.Text;
end;

operator + (const A: TUPascalString; const b: TUPascalString): TUPascalString;
begin
  CombineCharsPP(A.buff, b.buff, Result.buff);
end;

operator + (const A: TUPascalString; const b: USystemString): TUPascalString;
begin
  CombineCharsPS(A.buff, b, Result.buff);
end;

operator + (const A: USystemString; const b: TUPascalString): TUPascalString;
begin
  CombineCharsSP(A, b.buff, Result.buff);
end;

operator + (const A: TUPascalString; const b: USystemChar): TUPascalString;
begin
  CombineCharsPC(A.buff, b, Result.buff);
end;

operator + (const A: USystemChar; const b: TUPascalString): TUPascalString;
begin
  CombineCharsCP(A, b.buff, Result.buff);
end;

{$ENDIF}


function TUPascalString.GetText: USystemString;
begin
  SetLength(Result, length(buff));
  if length(buff) > 0 then
      CopyPtr(@buff[0], @Result[UFirstCharPos], length(buff) * USystemCharSize);
end;

procedure TUPascalString.SetText(const Value: USystemString);
begin
  SetLength(buff, length(Value));

  if length(buff) > 0 then
      CopyPtr(@Value[UFirstCharPos], @buff[0], length(buff) * USystemCharSize);
end;

function TUPascalString.GetLen: Integer;
begin
  Result := length(buff);
end;

procedure TUPascalString.SetLen(const Value: Integer);
begin
  SetLength(buff, Value);
end;

function TUPascalString.GetChars(index: Integer): USystemChar;
begin
  if (index > length(buff)) or (index <= 0) then
      Result := #0
  else
      Result := buff[index - 1];
end;

procedure TUPascalString.SetChars(index: Integer; const Value: USystemChar);
begin
  buff[index - 1] := Value;
end;

procedure TUPascalString.SetBytes(const Value: TBytes);
begin
  SetLength(buff, 0);
  try
      Text := SysUtils.TEncoding.UTF8.GetString(Value);
  except
      SetLength(buff, 0);
  end;
end;

function TUPascalString.GetBytes: TBytes;
begin
{$IFDEF FPC}
  Result := SysUtils.TEncoding.UTF8.GetBytes(buff);
{$ELSE}
  Result := SysUtils.TEncoding.UTF8.GetBytes(buff);
{$ENDIF}
end;

function TUPascalString.GetLast: USystemChar;
begin
  Result := buff[length(buff) - 1];
end;

procedure TUPascalString.SetLast(const Value: USystemChar);
begin
  buff[length(buff) - 1] := Value;
end;

function TUPascalString.GetFirst: USystemChar;
begin
  Result := buff[0];
end;

procedure TUPascalString.SetFirst(const Value: USystemChar);
begin
  buff[0] := Value;
end;

{$IFDEF DELPHI}


class operator TUPascalString.Equal(const Lhs, Rhs: TUPascalString): Boolean;
begin
  Result := (Lhs.Len = Rhs.Len) and (Lhs.Text = Rhs.Text);
end;

class operator TUPascalString.NotEqual(const Lhs, Rhs: TUPascalString): Boolean;
begin
  Result := not(Lhs = Rhs);
end;

class operator TUPascalString.GreaterThan(const Lhs, Rhs: TUPascalString): Boolean;
begin
  Result := Lhs.Text > Rhs.Text;
end;

class operator TUPascalString.GreaterThanOrEqual(const Lhs, Rhs: TUPascalString): Boolean;
begin
  Result := Lhs.Text >= Rhs.Text;
end;

class operator TUPascalString.LessThan(const Lhs, Rhs: TUPascalString): Boolean;
begin
  Result := Lhs.Text < Rhs.Text;
end;

class operator TUPascalString.LessThanOrEqual(const Lhs, Rhs: TUPascalString): Boolean;
begin
  Result := Lhs.Text <= Rhs.Text;
end;

class operator TUPascalString.Add(const Lhs, Rhs: TUPascalString): TUPascalString;
begin
  CombineCharsPP(Lhs.buff, Rhs.buff, Result.buff);
end;

class operator TUPascalString.Add(const Lhs: USystemString; const Rhs: TUPascalString): TUPascalString;
begin
  CombineCharsSP(Lhs, Rhs.buff, Result.buff);
end;

class operator TUPascalString.Add(const Lhs: TUPascalString; const Rhs: USystemString): TUPascalString;
begin
  CombineCharsPS(Lhs.buff, Rhs, Result.buff);
end;

class operator TUPascalString.Add(const Lhs: USystemChar; const Rhs: TUPascalString): TUPascalString;
begin
  CombineCharsCP(Lhs, Rhs.buff, Result.buff);
end;

class operator TUPascalString.Add(const Lhs: TUPascalString; const Rhs: USystemChar): TUPascalString;
begin
  CombineCharsPC(Lhs.buff, Rhs, Result.buff);
end;

class operator TUPascalString.Implicit(Value: TPascalString): TUPascalString;
begin
  Result.Bytes := Value.Bytes;
end;

class operator TUPascalString.Implicit(Value: Variant): TUPascalString;
begin
  Result.Text := VarToStr(Value);
end;

class operator TUPascalString.Implicit(Value: USystemString): TUPascalString;
begin
  Result.Text := Value;
end;

class operator TUPascalString.Implicit(Value: USystemChar): TUPascalString;
begin
  Result.Len := 1;
  Result.buff[0] := Value;
end;

class operator TUPascalString.Implicit(Value: TUPascalString): USystemString;
begin
  Result := Value.Text;
end;

class operator TUPascalString.Implicit(Value: TUPascalString): Variant;
begin
  Result := Value.Text;
end;

class operator TUPascalString.Explicit(Value: TUPascalString): TPascalString;
begin
  Result.Bytes := Value.Bytes;
end;

class operator TUPascalString.Explicit(Value: TUPascalString): USystemString;
begin
  Result := Value.Text;
end;

class operator TUPascalString.Explicit(Value: TUPascalString): Variant;
begin
  Result := Value.Text;
end;

class operator TUPascalString.Explicit(Value: USystemString): TUPascalString;
begin
  Result.Text := Value;
end;

class operator TUPascalString.Explicit(Value: Variant): TUPascalString;
begin
  Result.Text := VarToStr(Value);
end;

class operator TUPascalString.Explicit(Value: USystemChar): TUPascalString;
begin
  Result.Len := 1;
  Result.buff[0] := Value;
end;

{$ENDIF}


function TUPascalString.Copy(index, Count: nativeInt): TUPascalString;
var
  L: nativeInt;
begin
  L := length(buff);

  if (index - 1) + Count > L then
      Count := L - (index - 1);

  SetLength(Result.buff, Count);
  if Count > 0 then
      CopyPtr(@buff[index - 1], @Result.buff[0], USystemCharSize * Count);
end;

function TUPascalString.Same(const p: PUPascalString): Boolean;
var
  i: Integer;
  s, d: USystemChar;
begin
  Result := (p^.Len = Len);
  if not Result then
      Exit;
  for i := 0 to Len - 1 do
    begin
      s := buff[i];
      if UCharIn(s, ucHiAtoZ) then
          Inc(s, 32);
      d := p^.buff[i];
      if UCharIn(d, ucHiAtoZ) then
          Inc(d, 32);
      if s <> d then
          Exit(False);
    end;
end;

function TUPascalString.Same(const T: TUPascalString): Boolean;
var
  i: Integer;
  s, d: USystemChar;
begin
  Result := (T.Len = Len);
  if not Result then
      Exit;
  for i := 0 to Len - 1 do
    begin
      s := buff[i];
      if UCharIn(s, ucHiAtoZ) then
          Inc(s, 32);
      d := T.buff[i];
      if UCharIn(d, ucHiAtoZ) then
          Inc(d, 32);
      if s <> d then
          Exit(False);
    end;
end;

function TUPascalString.Same(const IgnoreCase: Boolean; const T: TUPascalString): Boolean;
var
  i: Integer;
  s, d: USystemChar;
begin
  Result := (T.Len = Len);
  if not Result then
      Exit;
  for i := 0 to Len - 1 do
    begin

      s := buff[i];
      if IgnoreCase then
        if UCharIn(s, ucHiAtoZ) then
            Inc(s, 32);

      d := T.buff[i];
      if IgnoreCase then
        if UCharIn(d, ucHiAtoZ) then
            Inc(d, 32);

      if s <> d then
          Exit(False);
    end;
end;

function TUPascalString.ComparePos(const Offset: Integer; const p: PUPascalString): Boolean;
var
  i, L: Integer;
  sourChar, destChar: USystemChar;
begin
  Result := False;
  i := 1;
  L := p^.Len;
  if (Offset + L - 1) > Len then
      Exit;
  while i <= L do
    begin
      sourChar := GetChars(Offset + i - 1);
      destChar := p^[i];

      if UCharIn(sourChar, ucLoAtoZ) then
          Dec(sourChar, 32);
      if UCharIn(destChar, ucLoAtoZ) then
          Dec(destChar, 32);

      if sourChar <> destChar then
          Exit;
      Inc(i);
    end;
  Result := True;
end;

function TUPascalString.ComparePos(const Offset: Integer; const T: TUPascalString): Boolean;
var
  i, L: Integer;
  sourChar, destChar: USystemChar;
begin
  Result := False;
  i := 1;
  L := T.Len;
  if (Offset + L) > Len then
      Exit;
  while i <= L do
    begin
      sourChar := GetChars(Offset + i - 1);
      destChar := T[i];

      if UCharIn(sourChar, ucLoAtoZ) then
          Dec(sourChar, 32);
      if UCharIn(destChar, ucLoAtoZ) then
          Dec(destChar, 32);

      if sourChar <> destChar then
          Exit;
      Inc(i);
    end;
  Result := True;
end;

function TUPascalString.GetPos(const s: TUPascalString; const Offset: Integer = 1): Integer;
var
  i: Integer;
begin
  Result := 0;
  if s.Len > 0 then
    for i := Offset to Len - s.Len + 1 do
      if ComparePos(i, @s) then
          Exit(i);
end;

function TUPascalString.GetPos(const s: PUPascalString; const Offset: Integer = 1): Integer;
var
  i: Integer;
begin
  Result := 0;
  if s^.Len > 0 then
    for i := Offset to Len - s^.Len + 1 do
      if ComparePos(i, s) then
          Exit(i);
end;

function TUPascalString.Exists(C: USystemChar): Boolean;
var
  i: Integer;
begin
  for i := low(buff) to high(buff) do
    if buff[i] = C then
        Exit(True);
  Result := False;
end;

function TUPascalString.Exists(C: array of USystemChar): Boolean;
var
  i: Integer;
begin
  for i := low(buff) to high(buff) do
    if UCharIn(buff[i], C) then
        Exit(True);
  Result := False;
end;

function TUPascalString.Exists(const s: TUPascalString): Boolean;
begin
  Result := GetPos(@s, 1) > 0;
end;

function TUPascalString.hash: TUHash;
begin
  Result := UFastHashPascalString(@Self);
end;

function TUPascalString.Hash64: TUHash64;
begin
  Result := UFastHash64PascalString(@Self);
end;

function TUPascalString.GetCharCount(C: USystemChar): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := low(buff) to high(buff) do
    if UCharIn(buff[i], C) then
        Inc(Result);
end;

procedure TUPascalString.DeleteLast;
begin
  if Len > 0 then
      SetLength(buff, length(buff) - 1);
end;

procedure TUPascalString.DeleteFirst;
begin
  if Len > 0 then
      buff := System.Copy(buff, 1, Len);
end;

procedure TUPascalString.Delete(idx, cnt: Integer);
begin
  if (idx + cnt <= Len) then
      Text := GetString(1, idx) + GetString(idx + cnt, Len + 1)
  else
      Text := GetString(1, idx);
end;

procedure TUPascalString.Clear;
begin
  SetLength(buff, 0);
end;

procedure TUPascalString.Append(T: TUPascalString);
var
  R, L: Integer;
begin
  L := length(T.buff);
  if L > 0 then
    begin
      R := length(buff);
      SetLength(buff, R + L);
      CopyPtr(@T.buff[0], @buff[R], L * USystemCharSize);
    end;
end;

procedure TUPascalString.Append(C: USystemChar);
begin
  SetLength(buff, length(buff) + 1);
  buff[length(buff) - 1] := C;
end;

function TUPascalString.GetString(bPos, ePos: nativeInt): TUPascalString;
begin
  if ePos > length(buff) then
      Result := Self.Copy(bPos, length(buff) - bPos + 1)
  else
      Result := Self.Copy(bPos, (ePos - bPos));
end;

procedure TUPascalString.Insert(AText: USystemString; idx: Integer);
begin
  Text := GetString(1, idx) + AText + GetString(idx + 1, Len);
end;

procedure TUPascalString.FastAsText(var output: USystemString);
begin
  SetLength(output, length(buff));
  if length(buff) > 0 then
      CopyPtr(@buff[0], @output[UFirstCharPos], length(buff) * USystemCharSize);
end;

procedure TUPascalString.FastGetBytes(var output: TBytes);
begin
{$IFDEF FPC}
  output := SysUtils.TEncoding.UTF8.GetBytes(buff);
{$ELSE}
  output := SysUtils.TEncoding.UTF8.GetBytes(buff);
{$ENDIF}
end;

function TUPascalString.LowerText: USystemString;
begin
  Result := LowerCase(Text);
end;

function TUPascalString.UpperText: USystemString;
begin
  Result := UpperCase(Text);
end;

function TUPascalString.Invert: TUPascalString;
var
  i, J: Integer;
begin
  SetLength(Result.buff, length(buff));
  J := low(Result.buff);
  for i := high(buff) downto low(buff) do
    begin
      Result.buff[J] := buff[i];
      Inc(J);
    end;
end;

function TUPascalString.TrimChar(const Chars: TUPascalString): TUPascalString;
var
  L, bp, EP: Integer;
begin
  Result := '';
  L := Len;
  if L > 0 then
    begin
      bp := 1;
      while UCharIn(GetChars(bp), @Chars) do
        begin
          Inc(bp);
          if (bp > L) then
            begin
              Result := '';
              Exit;
            end;
        end;
      if bp > L then
          Result := ''
      else
        begin
          EP := L;

          while UCharIn(GetChars(EP), @Chars) do
            begin
              Dec(EP);
              if (EP < 1) then
                begin
                  Result := '';
                  Exit;
                end;
            end;
          Result := GetString(bp, EP + 1);
        end;
    end;
end;

function TUPascalString.DeleteChar(const Chars: TUPascalString): TUPascalString;
var
  C: USystemChar;
begin
  Result := '';
  for C in buff do
    if not UCharIn(C, @Chars) then
        Result.Append(C);
end;

function TUPascalString.DeleteChar(const Chars: TUOrdChars): TUPascalString;
var
  C: USystemChar;
begin
  Result := '';
  for C in buff do
    if not UCharIn(C, Chars) then
        Result.Append(C);
end;

function TUPascalString.ReplaceChar(const Chars: TUPascalString; const newChar: USystemChar): TUPascalString;
var
  i: Integer;
begin
  Result.Len := Len;
  for i := low(buff) to high(buff) do
    if UCharIn(buff[i], Chars) then
        Result.buff[i] := newChar
    else
        Result.buff[i] := buff[i];
end;

function TUPascalString.ReplaceChar(const Chars, newChar: USystemChar): TUPascalString;
var
  i: Integer;
begin
  Result.Len := Len;
  for i := low(buff) to high(buff) do
    if UCharIn(buff[i], Chars) then
        Result.buff[i] := newChar
    else
        Result.buff[i] := buff[i];
end;

function TUPascalString.ReplaceChar(const Chars: TUOrdChars; const newChar: USystemChar): TUPascalString;
var
  i: Integer;
begin
  Result.Len := Len;
  for i := low(buff) to high(buff) do
    if UCharIn(buff[i], Chars) then
        Result.buff[i] := newChar
    else
        Result.buff[i] := buff[i];
end;

function TUPascalString.SmithWaterman(const p: PUPascalString): Double;
begin
  Result := USmithWatermanCompare(@Self, @p);
end;

function TUPascalString.SmithWaterman(const s: TUPascalString): Double;
begin
  Result := USmithWatermanCompare(@Self, @s);
end;

function TUPascalString.BOMBytes: TBytes;
begin
{$IFDEF FPC}
  Result := GetBytes;
{$ELSE}
  Result := SysUtils.TEncoding.UTF8.GetPreamble + GetBytes;
{$ENDIF}
end;

initialization

finalization

end.
 
