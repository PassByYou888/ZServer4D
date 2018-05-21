{ ***************************************************************************** }
{ * fpc Unicode string support,writen by QQ 600585@qq.com                      * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ ****************************************************************************** }

unit UPascalStrings;

interface

{$I zDefine.inc}


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
  TUPascalChars  = packed array of USystemChar;
  TUOrdChar      = (uc0to9, uc1to9, uc0to32, uc0to32no10, ucLoAtoF, ucHiAtoF, ucLoAtoZ, ucHiAtoZ, ucHex, ucAtoF, ucAtoZ);
  TUOrdChars     = set of TUOrdChar;
  TUHash         = Cardinal;
  TUHash64       = UInt64;

  TUPascalString = record
  private
    function GetText: USystemString; inline;
    procedure SetText(const Value: USystemString); inline;
    function GetLen: Integer; inline;
    procedure SetLen(const Value: Integer); inline;
    function GetChars(index: Integer): USystemChar; inline;
    procedure SetChars(index: Integer; const Value: USystemChar); inline;
    function GetBytes: TBytes; inline;
    procedure SetBytes(const Value: TBytes); inline;
    function GetLast: USystemChar; inline;
    procedure SetLast(const Value: USystemChar); inline;
    function GetFirst: USystemChar; inline;
    procedure SetFirst(const Value: USystemChar); inline;
  public
    Buff: TUPascalChars;

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
    function copy(index, count: NativeInt): TUPascalString;
    function Same(const p: PUPascalString): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Same(const t: TUPascalString): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Same(const IgnoreCase: Boolean; const t: TUPascalString): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ComparePos(const Offset: Integer; const p: PUPascalString): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ComparePos(const Offset: Integer; const t: TUPascalString): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function GetPos(const s: TUPascalString; const Offset: Integer = 1): Integer; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function GetPos(const s: PUPascalString; const Offset: Integer = 1): Integer; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Exists(c: USystemChar): Boolean; overload;
    function Exists(c: array of USystemChar): Boolean; overload;
    function Exists(const s: TUPascalString): Boolean; overload;
    function GetCharCount(c: USystemChar): Integer;
    //
    function Hash: TUHash; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Hash64: TUHash64; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    //
    property Last: USystemChar read GetLast write SetLast;
    property First: USystemChar read GetFirst write SetFirst;

    procedure DeleteLast; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure DeleteFirst; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure Delete(idx, cnt: Integer); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure Clear; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure Append(t: TUPascalString); overload;
    procedure Append(c: USystemChar); overload;
    function GetString(bPos, ePos: NativeInt): TUPascalString; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure Insert(AText: USystemString; idx: Integer); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    //
    procedure FastAsText(var Output: USystemString);
    procedure FastGetBytes(var Output: TBytes); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    //
    property Text: USystemString read GetText write SetText;
    function LowerText: USystemString;
    function UpperText: USystemString;
    function Invert: TUPascalString;
    function TrimChar(const charS: TUPascalString): TUPascalString;
    function DeleteChar(const charS: TUPascalString): TUPascalString; overload;
    function DeleteChar(const charS: TUOrdChars): TUPascalString; overload;
    function ReplaceChar(const charS: TUPascalString; const newChar: USystemChar): TUPascalString; overload;
    function ReplaceChar(const charS, newChar: USystemChar): TUPascalString; overload;
    function ReplaceChar(const charS: TUOrdChars; const newChar: USystemChar): TUPascalString; overload;

    { https://en.wikipedia.org/wiki/Smith%E2%80%93Waterman_algorithm }
    function SmithWaterman(const p: PUPascalString): Double; overload;
    function SmithWaterman(const s: TUPascalString): Double; overload;

    property Len: Integer read GetLen write SetLen;
    property charS[index: Integer]: USystemChar read GetChars write SetChars; default;
    property Bytes: TBytes read GetBytes write SetBytes;
    function BOMBytes: TBytes;
  end;

  TUArrayPascalString = array of TUPascalString;
  PUArrayPascalString = ^TUArrayPascalString;

  TUArrayPascalStringPtr = array of PUPascalString;
  PUArrayPascalStringPtr = ^TUArrayPascalStringPtr;

function UCharIn(c: USystemChar; const SomeChars: array of USystemChar): Boolean; overload;
function UCharIn(c: USystemChar; const SomeChar: USystemChar): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function UCharIn(c: USystemChar; const s: TUPascalString): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function UCharIn(c: USystemChar; const p: PUPascalString): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function UCharIn(c: USystemChar; const SomeCharsets: TUOrdChars): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function UCharIn(c: USystemChar; const SomeCharset: TUOrdChar): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function UCharIn(c: USystemChar; const SomeCharsets: TUOrdChars; const SomeChars: TUPascalString): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function UCharIn(c: USystemChar; const SomeCharsets: TUOrdChars; const p: PUPascalString): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function UFastHashSystemString(const s: PSystemString): TUHash; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function UFastHash64SystemString(const s: PSystemString): TUHash64; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function UFastHashSystemString(const s: SystemString): TUHash; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function UFastHash64SystemString(const s: SystemString): TUHash64; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function UFastHashPascalString(const s: PPascalString): TUHash; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function UFastHash64PascalString(const s: PPascalString): TUHash64; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function UFormat(const Fmt: USystemString; const Args: array of const): USystemString;

{$IFDEF FPC}

operator := (const s: Variant)r: TUPascalString;
operator := (const s: AnsiString)r: TUPascalString;
operator := (const s: UnicodeString)r: TUPascalString;
operator := (const s: WideString)r: TUPascalString;
operator := (const s: ShortString)r: TUPascalString;
operator := (const c: USystemChar)r: TUPascalString;
operator := (const c: TPascalString)r: TUPascalString;

operator := (const s: TUPascalString)r: AnsiString;
operator := (const s: TUPascalString)r: UnicodeString;
operator := (const s: TUPascalString)r: WideString;
operator := (const s: TUPascalString)r: ShortString;
operator := (const s: TUPascalString)r: Variant;
operator := (const s: TUPascalString)r: TPascalString;

operator = (const A: TUPascalString; const B: TUPascalString): Boolean;
operator <> (const A: TUPascalString; const B: TUPascalString): Boolean;
operator > (const A: TUPascalString; const B: TUPascalString): Boolean;
operator >= (const A: TUPascalString; const B: TUPascalString): Boolean;
operator < (const A: TUPascalString; const B: TUPascalString): Boolean;
operator <= (const A: TUPascalString; const B: TUPascalString): Boolean;

operator + (const A: TUPascalString; const B: TUPascalString): TUPascalString;
operator + (const A: TUPascalString; const B: USystemString): TUPascalString;
operator + (const A: USystemString; const B: TUPascalString): TUPascalString;
operator + (const A: TUPascalString; const B: USystemChar): TUPascalString;
operator + (const A: USystemChar; const B: TUPascalString): TUPascalString;

{$ENDIF}

{ https://en.wikipedia.org/wiki/Smith%E2%80%93Waterman_algorithm }

// short string likeness and out diff
function USmithWatermanCompare(const seq1, seq2: PUPascalString; var diff1, diff2: TUPascalString;
  const NoDiffChar: Boolean = False; const diffChar: USystemChar = '-'): Double; overload;
function USmithWatermanCompare(const seq1, seq2: TUPascalString; var diff1, diff2: TUPascalString;
  const NoDiffChar: Boolean = False; const diffChar: USystemChar = '-'): Double; overload;

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
  USystemCharSize: NativeInt = SizeOf(USystemChar);
  {$IFDEF CPU64}
  UMaxSmithWatermanMatrix: NativeInt = 10000 * 10;
  {$ELSE}
  UMaxSmithWatermanMatrix: NativeInt = 8192;
  {$ENDIF}


const
  {$IFDEF FirstCharInZero}
  UFirstCharPos = 0;
  {$ELSE}
  UFirstCharPos = 1;
  {$ENDIF}

implementation

uses CoreClasses, Variants;

procedure CombineCharsPP(const c1, c2: TUPascalChars; var Output: TUPascalChars); {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  ll, rl: Integer;
begin
  ll := Length(c1);
  rl := Length(c2);
  SetLength(Output, ll + rl);
  if ll > 0 then
      CopyPtr(@c1[0], @Output[0], ll * USystemCharSize);
  if rl > 0 then
      CopyPtr(@c2[0], @Output[ll], rl * USystemCharSize);
end;

procedure CombineCharsSP(const c1: USystemString; const c2: TUPascalChars; var Output: TUPascalChars); {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  ll, rl: Integer;
begin
  ll := Length(c1);
  rl := Length(c2);
  SetLength(Output, ll + rl);
  if ll > 0 then
      CopyPtr(@c1[UFirstCharPos], @Output[0], ll * USystemCharSize);
  if rl > 0 then
      CopyPtr(@c2[0], @Output[ll], rl * USystemCharSize);
end;

procedure CombineCharsPS(const c1: TUPascalChars; const c2: USystemString; var Output: TUPascalChars); {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  ll, rl: Integer;
begin
  ll := Length(c1);
  rl := Length(c2);
  SetLength(Output, ll + rl);
  if ll > 0 then
      CopyPtr(@c1[0], @Output[0], ll * USystemCharSize);
  if rl > 0 then
      CopyPtr(@c2[UFirstCharPos], @Output[ll], rl * USystemCharSize);
end;

procedure CombineCharsCP(const c1: USystemChar; const c2: TUPascalChars; var Output: TUPascalChars); {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  rl: Integer;
begin
  rl := Length(c2);
  SetLength(Output, rl + 1);
  Output[0] := c1;
  if rl > 0 then
      CopyPtr(@c2[0], @Output[1], rl * USystemCharSize);
end;

procedure CombineCharsPC(const c1: TUPascalChars; const c2: USystemChar; var Output: TUPascalChars); {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  ll: Integer;
begin
  ll := Length(c1);
  SetLength(Output, ll + 1);
  if ll > 0 then
      CopyPtr(@c1[0], @Output[0], ll * USystemCharSize);
  Output[ll] := c2;
end;

function UCharIn(c: USystemChar; const SomeChars: array of USystemChar): Boolean;
var
  AChar: USystemChar;
begin
  Result := True;
  for AChar in SomeChars do
    if AChar = c then
        Exit;
  Result := False;
end;

function UCharIn(c: USystemChar; const SomeChar: USystemChar): Boolean;
begin
  Result := c = SomeChar;
end;

function UCharIn(c: USystemChar; const s: TUPascalString): Boolean;
begin
  Result := s.Exists(c);
end;

function UCharIn(c: USystemChar; const p: PUPascalString): Boolean;
begin
  Result := p^.Exists(c);
end;

function UCharIn(c: USystemChar; const SomeCharset: TUOrdChar): Boolean;
const
  ord0  = ord('0');
  ord1  = ord('1');
  ord9  = ord('9');
  ordLA = ord('a');
  ordHA = ord('A');
  ordLF = ord('f');
  ordHF = ord('F');
  ordLZ = ord('z');
  ordHZ = ord('Z');

var
  v: NativeInt;
begin
  v := ord(c);
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

function UCharIn(c: USystemChar; const SomeCharsets: TUOrdChars): Boolean;
var
  i: TUOrdChar;
begin
  Result := True;
  for i in SomeCharsets do
    if UCharIn(c, i) then
        Exit;
  Result := False;
end;

function UCharIn(c: USystemChar; const SomeCharsets: TUOrdChars; const SomeChars: TUPascalString): Boolean;
begin
  if UCharIn(c, SomeCharsets) then
      Result := True
  else
      Result := UCharIn(c, SomeChars);
end;

function UCharIn(c: USystemChar; const SomeCharsets: TUOrdChars; const p: PUPascalString): Boolean;
begin
  if UCharIn(c, SomeCharsets) then
      Result := True
  else
      Result := UCharIn(c, p);
end;

function UFastHashSystemString(const s: PSystemString): TUHash;
var
  i: Integer;
  c: USystemChar;
begin
  Result := 0;

  {$IFDEF FirstCharInZero}
  for i := 0 to Length(s^) - 1 do
  {$ELSE}
  for i := 1 to Length(s^) do
    {$ENDIF}
    begin
      c := s^[i];
      if UCharIn(c, ucHiAtoZ) then
          inc(c, 32);
      Result := ((Result shl 7) or (Result shr 25)) + TUHash(c);
    end;
end;

function UFastHash64SystemString(const s: PSystemString): TUHash64;
var
  i: Integer;
  c: USystemChar;
begin
  Result := 0;

  {$IFDEF FirstCharInZero}
  for i := 0 to Length(s^) - 1 do
  {$ELSE}
  for i := 1 to Length(s^) do
    {$ENDIF}
    begin
      c := s^[i];
      if UCharIn(c, ucHiAtoZ) then
          inc(c, 32);
      Result := ((Result shl 7) or (Result shr 57)) + TUHash64(c);
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
  c: USystemChar;
begin
  Result := 0;
  for i := 1 to s^.Len do
    begin
      c := s^[i];
      if UCharIn(c, ucHiAtoZ) then
          inc(c, 32);
      Result := ((Result shl 7) or (Result shr 25)) + TUHash(c);
    end;
end;

function UFastHash64PascalString(const s: PPascalString): TUHash64;
var
  i: Integer;
  c: USystemChar;
begin
  Result := 0;
  for i := 1 to s^.Len do
    begin
      c := s^[i];
      if UCharIn(c, ucHiAtoZ) then
          inc(c, 32);
      Result := ((Result shl 7) or (Result shr 57)) + TUHash64(c);
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

function GetSWMVMemory(const xLen, yLen: NativeInt): Pointer; inline;
{ optimized matrix performance }
begin
  Result := System.AllocMem((xLen + 1) * (yLen + 1) * SizeOf(NativeInt));
end;

function GetSWMV(const p: Pointer; const w, x, y: NativeInt): NativeInt; inline;
{ optimized matrix performance }
begin
  Result := PNativeInt(NativeUInt(p) + ((x + y * (w + 1)) * SizeOf(NativeInt)))^;
end;

procedure SetSWMV(const p: Pointer; const w, x, y: NativeInt; const v: NativeInt); inline;
{ optimized matrix performance }
begin
  PNativeInt(NativeUInt(p) + ((x + y * (w + 1)) * SizeOf(NativeInt)))^ := v;
end;

function GetMax(const i1, i2: NativeInt): NativeInt; inline;
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
        dec(alphaC, 32);
    if UCharIn(betaC, ucLoAtoZ) then
        dec(betaC, 32);

    if alphaC = betaC then
        Result := SmithWaterman_MatchOk
    else if (alphaC = diffC) or (betaC = diffC) then
        Result := gap_penalty
    else
        Result := mismatch_penalty;
  end;

var
  swMatrixPtr: Pointer;
  i, j, l1, l2: NativeInt;
  matched, deleted, inserted: NativeInt;
  score_current, score_diagonal, score_left, score_right: NativeInt;
  identity: NativeInt;
  align1, align2: TUPascalString;
begin
  l1 := seq1^.Len;
  l2 := seq2^.Len;

  if (l1 = 0) or (l2 = 0) or (l1 > UMaxSmithWatermanMatrix) or (l2 > UMaxSmithWatermanMatrix) then
    begin
      Result := -1;
      Exit;
    end;

  { fast build matrix }
  swMatrixPtr := GetSWMVMemory(l1, l2);
  if swMatrixPtr = nil then
    begin
      diff1 := '';
      diff2 := '';
      Result := -1;
      Exit;
    end;

  i := 0;
  while i <= l1 do
    begin
      SetSWMV(swMatrixPtr, l1, i, 0, gap_penalty * i);
      inc(i);
    end;

  j := 0;
  while j <= l2 do
    begin
      SetSWMV(swMatrixPtr, l1, 0, j, gap_penalty * j);
      inc(j);
    end;

  { compute matrix }
  i := 1;
  while i <= l1 do
    begin
      j := 1;
      while j <= l2 do
        begin
          matched := GetSWMV(swMatrixPtr, l1, i - 1, j - 1) + InlineMatch(seq1^[i], seq2^[j], diffChar);
          deleted := GetSWMV(swMatrixPtr, l1, i - 1, j) + gap_penalty;
          inserted := GetSWMV(swMatrixPtr, l1, i, j - 1) + gap_penalty;
          SetSWMV(swMatrixPtr, l1, i, j, GetMax(matched, GetMax(deleted, inserted)));
          inc(j);
        end;
      inc(i);
    end;

  { compute align }
  i := l1;
  j := l2;
  align1 := '';
  align2 := '';
  identity := 0;
  while (i > 0) and (j > 0) do
    begin
      score_current := GetSWMV(swMatrixPtr, l1, i, j);
      score_diagonal := GetSWMV(swMatrixPtr, l1, i - 1, j - 1);
      score_left := GetSWMV(swMatrixPtr, l1, i - 1, j);
      score_right := GetSWMV(swMatrixPtr, l1, i, j - 1);

      matched := InlineMatch(seq1^[i], seq2^[j], diffChar);

      if score_current = score_diagonal + matched then
        begin
          if matched = SmithWaterman_MatchOk then
            begin
              inc(identity);
              align1.Append(seq1^[i]);
              align2.Append(seq2^[j]);
            end
          else if NoDiffChar then
            begin
              align1.Append(diffChar);
              align2.Append(diffChar);
            end
          else
            begin
              align1.Append(seq1^[i]);
              align2.Append(seq2^[j]);
            end;
          dec(i);
          dec(j);
        end
      else if score_current = score_left + gap_penalty then
        begin
          if NoDiffChar then
              align1.Append(diffChar)
          else
              align1.Append(seq1^[i]);
          align2.Append(diffChar);
          dec(i);
        end
      else if score_current = score_right + gap_penalty then
        begin
          if NoDiffChar then
              align2.Append(diffChar)
          else
              align2.Append(seq2^[j]);
          align1.Append(diffChar);
          dec(j);
        end
      else
          raise exception.Create('matrix error'); // matrix debug time
    end;

  System.FreeMemory(swMatrixPtr);

  while i > 0 do
    begin
      if NoDiffChar then
          align1.Append(diffChar)
      else
          align1.Append(seq1^[i]);
      align2.Append(diffChar);
      dec(i);
    end;

  while j > 0 do
    begin
      if NoDiffChar then
          align2.Append(diffChar)
      else
          align2.Append(seq2^[j]);
      align1.Append(diffChar);
      dec(j);
    end;

  if identity > 0 then
      Result := identity / align1.Len
  else
      Result := -1;

  diff1 := align1.Invert;
  diff2 := align2.Invert;
end;

function USmithWatermanCompare(const seq1, seq2: TUPascalString; var diff1, diff2: TUPascalString;
  const NoDiffChar: Boolean; const diffChar: USystemChar): Double;
begin
  Result := USmithWatermanCompare(@seq1, @seq2, diff1, diff2, NoDiffChar, diffChar);
end;

function USmithWatermanCompare(const seq1, seq2: PUPascalString; out Same, Diff: Integer): Double;

  function InlineMatch(alphaC, betaC: USystemChar): NativeInt; inline;
  begin
    if UCharIn(alphaC, ucLoAtoZ) then
        dec(alphaC, 32);
    if UCharIn(betaC, ucLoAtoZ) then
        dec(betaC, 32);

    if alphaC = betaC then
        Result := SmithWaterman_MatchOk
    else
        Result := mismatch_penalty;
  end;

var
  swMatrixPtr: Pointer;
  i, j, l1, l2: NativeInt;
  matched, deleted, inserted: NativeInt;
  score_current, score_diagonal, score_left, score_right: NativeInt;
  identity, l: NativeInt;
begin
  l1 := seq1^.Len;
  l2 := seq2^.Len;

  if (l1 = 0) or (l2 = 0) or (l1 > UMaxSmithWatermanMatrix) or (l2 > UMaxSmithWatermanMatrix) then
    begin
      Result := -1;
      Same := 0;
      Diff := l1 + l2;
      Exit;
    end;

  { fast build matrix }
  swMatrixPtr := GetSWMVMemory(l1, l2);
  if swMatrixPtr = nil then
    begin
      Result := -1;
      Exit;
    end;

  i := 0;
  while i <= l1 do
    begin
      SetSWMV(swMatrixPtr, l1, i, 0, gap_penalty * i);
      inc(i);
    end;

  j := 0;
  while j <= l2 do
    begin
      SetSWMV(swMatrixPtr, l1, 0, j, gap_penalty * j);
      inc(j);
    end;

  { compute matrix }
  i := 1;
  while i <= l1 do
    begin
      j := 1;
      while j <= l2 do
        begin
          matched := GetSWMV(swMatrixPtr, l1, i - 1, j - 1) + InlineMatch(seq1^[i], seq2^[j]);
          deleted := GetSWMV(swMatrixPtr, l1, i - 1, j) + gap_penalty;
          inserted := GetSWMV(swMatrixPtr, l1, i, j - 1) + gap_penalty;
          SetSWMV(swMatrixPtr, l1, i, j, GetMax(matched, GetMax(deleted, inserted)));
          inc(j);
        end;
      inc(i);
    end;

  { compute align }
  i := l1;
  j := l2;
  identity := 0;
  l := 0;
  while (i > 0) and (j > 0) do
    begin
      score_current := GetSWMV(swMatrixPtr, l1, i, j);
      score_diagonal := GetSWMV(swMatrixPtr, l1, i - 1, j - 1);
      score_left := GetSWMV(swMatrixPtr, l1, i - 1, j);
      score_right := GetSWMV(swMatrixPtr, l1, i, j - 1);
      matched := InlineMatch(seq1^[i], seq2^[j]);

      if score_current = score_diagonal + matched then
        begin
          if matched = SmithWaterman_MatchOk then
              inc(identity);

          inc(l);
          dec(i);
          dec(j);
        end
      else if score_current = score_left + gap_penalty then
        begin
          inc(l);
          dec(i);
        end
      else if score_current = score_right + gap_penalty then
        begin
          inc(l);
          dec(j);
        end
      else
          raise exception.Create('matrix error'); // matrix debug time
    end;

  System.FreeMemory(swMatrixPtr);

  if identity > 0 then
    begin
      Result := identity / (l + i + j);
      Same := identity;
      Diff := (l + i + j) - identity;
    end
  else
    begin
      Result := -1;
      Same := 0;
      Diff := l + i + j;
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
  r: Double;
begin
  Result := -1;
  for i := 0 to Length(seq1) - 1 do
    begin
      r := USmithWatermanCompare(seq1[i], seq2);
      if r > Result then
          Result := r;
    end;
end;

function USmithWatermanCompare(const seq1: Pointer; siz1: Integer; const seq2: Pointer; siz2: Integer;
  out Same, Diff: Integer): Double;

  function InlineMatch(const alphaB, betaB: Byte): NativeInt; inline;
  begin
    if alphaB = betaB then
        Result := SmithWaterman_MatchOk
    else
        Result := mismatch_penalty;
  end;

var
  swMatrixPtr: Pointer;
  i, j, l1, l2: NativeInt;
  matched, deleted, inserted: NativeInt;
  score_current, score_diagonal, score_left, score_right: NativeInt;
  identity, l: NativeInt;
begin
  l1 := siz1;
  l2 := siz2;

  if (l1 = 0) or (l2 = 0) or (l1 > UMaxSmithWatermanMatrix) or (l2 > UMaxSmithWatermanMatrix) then
    begin
      Result := -1;
      Same := 0;
      Diff := l1 + l2;
      Exit;
    end;

  { fast build matrix }
  swMatrixPtr := GetSWMVMemory(l1, l2);
  if swMatrixPtr = nil then
    begin
      Result := -1;
      Exit;
    end;

  i := 0;
  while i <= l1 do
    begin
      SetSWMV(swMatrixPtr, l1, i, 0, gap_penalty * i);
      inc(i);
    end;

  j := 0;
  while j <= l2 do
    begin
      SetSWMV(swMatrixPtr, l1, 0, j, gap_penalty * j);
      inc(j);
    end;

  { compute matrix }
  i := 1;
  while i <= l1 do
    begin
      j := 1;
      while j <= l2 do
        begin
          matched := GetSWMV(swMatrixPtr, l1, i - 1, j - 1) + InlineMatch(PByte(NativeUInt(seq1) + (i - 1))^, PByte(NativeUInt(seq2) + (j - 1))^);
          deleted := GetSWMV(swMatrixPtr, l1, i - 1, j) + gap_penalty;
          inserted := GetSWMV(swMatrixPtr, l1, i, j - 1) + gap_penalty;
          SetSWMV(swMatrixPtr, l1, i, j, GetMax(matched, GetMax(deleted, inserted)));
          inc(j);
        end;
      inc(i);
    end;

  { compute align }
  i := l1;
  j := l2;
  identity := 0;
  l := 0;
  while (i > 0) and (j > 0) do
    begin
      score_current := GetSWMV(swMatrixPtr, l1, i, j);
      score_diagonal := GetSWMV(swMatrixPtr, l1, i - 1, j - 1);
      score_left := GetSWMV(swMatrixPtr, l1, i - 1, j);
      score_right := GetSWMV(swMatrixPtr, l1, i, j - 1);
      matched := InlineMatch(PByte(NativeUInt(seq1) + (i - 1))^, PByte(NativeUInt(seq2) + (j - 1))^);

      if score_current = score_diagonal + matched then
        begin
          if matched = SmithWaterman_MatchOk then
              inc(identity);

          inc(l);
          dec(i);
          dec(j);
        end
      else if score_current = score_left + gap_penalty then
        begin
          inc(l);
          dec(i);
        end
      else if score_current = score_right + gap_penalty then
        begin
          inc(l);
          dec(j);
        end
      else
          raise exception.Create('matrix error'); // matrix debug time
    end;

  System.FreeMemory(swMatrixPtr);

  if identity > 0 then
    begin
      Result := identity / (l + i + j);
      Same := identity;
      Diff := (l + i + j) - identity;
    end
  else
    begin
      Result := -1;
      Same := 0;
      Diff := l + i + j;
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
    l, i: Integer;
    n: TUPascalString;
    p: PSRec;
  begin
    l := psPtr^.Len;
    i := 1;
    n := '';
    while i <= l do
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
                inc(i);
            until (i > l) or (not UCharIn(psPtr^[i], [#13, #10, #32, #9]));
          end
        else
          begin
            n.Append(psPtr^[i]);
            inc(i);
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

  function InlineMatch(const alpha, beta: PSRec; const MinDiffCharWithPeerLine: Integer; var cSame, cDiff: Integer): NativeInt; inline;
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
    for i := 0 to lst1.count - 1 do
        dispose(PSRec(lst1[i]));
    for i := 0 to lst2.count - 1 do
        dispose(PSRec(lst2[i]));
    disposeObject([lst1, lst2]);
  end;

var
  swMatrixPtr: Pointer;
  i, j, l1, l2: NativeInt;
  matched, deleted, inserted: NativeInt;
  score_current, score_diagonal, score_left, score_right: NativeInt;
  cSame, cDiff, TotalSame, TotalDiff: Integer;
begin
  _Init;
  l1 := lst1.count;
  l2 := lst2.count;

  if (l1 = 0) or (l2 = 0) or (l1 > UMaxSmithWatermanMatrix) or (l2 > UMaxSmithWatermanMatrix) then
    begin
      Result := -1;
      Same := 0;
      Diff := l1 + l2;
      _Free;
      Exit;
    end;

  { fast build matrix }
  swMatrixPtr := GetSWMVMemory(l1, l2);
  if swMatrixPtr = nil then
    begin
      Result := -1;
      _Free;
      Exit;
    end;

  i := 0;
  while i <= l1 do
    begin
      SetSWMV(swMatrixPtr, l1, i, 0, gap_penalty * i);
      inc(i);
    end;

  j := 0;
  while j <= l2 do
    begin
      SetSWMV(swMatrixPtr, l1, 0, j, gap_penalty * j);
      inc(j);
    end;

  { compute matrix }
  i := 1;
  while i <= l1 do
    begin
      j := 1;
      while j <= l2 do
        begin
          matched := GetSWMV(swMatrixPtr, l1, i - 1, j - 1) + InlineMatch(PSRec(lst1[i - 1]), PSRec(lst2[j - 1]), MinDiffCharWithPeerLine, cSame, cDiff);
          deleted := GetSWMV(swMatrixPtr, l1, i - 1, j) + gap_penalty;
          inserted := GetSWMV(swMatrixPtr, l1, i, j - 1) + gap_penalty;
          SetSWMV(swMatrixPtr, l1, i, j, GetMax(matched, GetMax(deleted, inserted)));
          inc(j);
        end;
      inc(i);
    end;

  { compute align }
  i := l1;
  j := l2;
  TotalSame := 0;
  TotalDiff := 0;
  while (i > 0) and (j > 0) do
    begin
      score_current := GetSWMV(swMatrixPtr, l1, i, j);
      score_diagonal := GetSWMV(swMatrixPtr, l1, i - 1, j - 1);
      score_left := GetSWMV(swMatrixPtr, l1, i - 1, j);
      score_right := GetSWMV(swMatrixPtr, l1, i, j - 1);
      matched := InlineMatch(PSRec(lst1[i - 1]), PSRec(lst2[j - 1]), MinDiffCharWithPeerLine, cSame, cDiff);

      inc(TotalSame, cSame);
      inc(TotalDiff, cDiff);

      if score_current = score_diagonal + matched then
        begin
          dec(i);
          dec(j);
        end
      else if score_current = score_left + gap_penalty then
        begin
          dec(i);
        end
      else if score_current = score_right + gap_penalty then
        begin
          dec(j);
        end
      else
          raise exception.Create('matrix error'); // matrix debug time
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


operator := (const s: Variant)r: TUPascalString;
begin
  r.Text := s;
end;

operator := (const s: AnsiString)r: TUPascalString;
begin
  r.Text := s;
end;

operator := (const s: UnicodeString)r: TUPascalString;
begin
  r.Text := s;
end;

operator := (const s: WideString)r: TUPascalString;
begin
  r.Text := s;
end;

operator := (const s: ShortString)r: TUPascalString;
begin
  r.Text := s;
end;

operator := (const c: USystemChar)r: TUPascalString;
begin
  r.Text := c;
end;

operator := (const c: TPascalString)r: TUPascalString;
begin
  Result.Bytes := c.Bytes;
end;

operator := (const s: TUPascalString)r: AnsiString;
begin
  r := s.Text;
end;

operator := (const s: TUPascalString)r: UnicodeString;
begin
  r := s.Text;
end;

operator := (const s: TUPascalString)r: WideString;
begin
  r := s.Text;
end;

operator := (const s: TUPascalString)r: ShortString;
begin
  r := s.Text;
end;

operator := (const s: TUPascalString)r: Variant;
begin
  r := s.Text;
end;

operator := (const s: TUPascalString)r: TPascalString;
begin
  Result.Bytes := s.Bytes;
end;

operator = (const A: TUPascalString; const B: TUPascalString): Boolean;
begin
  Result := A.Text = B.Text;
end;

operator <> (const A: TUPascalString; const B: TUPascalString): Boolean;
begin
  Result := A.Text <> B.Text;
end;

operator > (const A: TUPascalString; const B: TUPascalString): Boolean;
begin
  Result := A.Text > B.Text;
end;

operator >= (const A: TUPascalString; const B: TUPascalString): Boolean;
begin
  Result := A.Text >= B.Text;
end;

operator < (const A: TUPascalString; const B: TUPascalString): Boolean;
begin
  Result := A.Text < B.Text;
end;

operator <= (const A: TUPascalString; const B: TUPascalString): Boolean;
begin
  Result := A.Text <= B.Text;
end;

operator + (const A: TUPascalString; const B: TUPascalString): TUPascalString;
begin
  CombineCharsPP(A.Buff, B.Buff, Result.Buff);
end;

operator + (const A: TUPascalString; const B: USystemString): TUPascalString;
begin
  CombineCharsPS(A.Buff, B, Result.Buff);
end;

operator + (const A: USystemString; const B: TUPascalString): TUPascalString;
begin
  CombineCharsSP(A, B.Buff, Result.Buff);
end;

operator + (const A: TUPascalString; const B: USystemChar): TUPascalString;
begin
  CombineCharsPC(A.Buff, B, Result.Buff);
end;

operator + (const A: USystemChar; const B: TUPascalString): TUPascalString;
begin
  CombineCharsCP(A, B.Buff, Result.Buff);
end;

{$ENDIF}


function TUPascalString.GetText: USystemString;
begin
  SetLength(Result, Length(Buff));
  if Length(Buff) > 0 then
      CopyPtr(@Buff[0], @Result[UFirstCharPos], Length(Buff) * USystemCharSize);
end;

procedure TUPascalString.SetText(const Value: USystemString);
begin
  SetLength(Buff, Length(Value));

  if Length(Buff) > 0 then
      CopyPtr(@Value[UFirstCharPos], @Buff[0], Length(Buff) * USystemCharSize);
end;

function TUPascalString.GetLen: Integer;
begin
  Result := Length(Buff);
end;

procedure TUPascalString.SetLen(const Value: Integer);
begin
  SetLength(Buff, Value);
end;

function TUPascalString.GetChars(index: Integer): USystemChar;
begin
  if (index > Length(Buff)) or (index <= 0) then
      Result := #0
  else
      Result := Buff[index - 1];
end;

procedure TUPascalString.SetChars(index: Integer; const Value: USystemChar);
begin
  Buff[index - 1] := Value;
end;

procedure TUPascalString.SetBytes(const Value: TBytes);
begin
  SetLength(Buff, 0);
  try
      Text := SysUtils.TEncoding.UTF8.GetString(Value);
  except
      SetLength(Buff, 0);
  end;
end;

function TUPascalString.GetBytes: TBytes;
begin
  {$IFDEF FPC}
  Result := SysUtils.TEncoding.UTF8.GetBytes(Buff);
  {$ELSE}
  Result := SysUtils.TEncoding.UTF8.GetBytes(Buff);
  {$ENDIF}
end;

function TUPascalString.GetLast: USystemChar;
begin
  Result := Buff[Length(Buff) - 1];
end;

procedure TUPascalString.SetLast(const Value: USystemChar);
begin
  Buff[Length(Buff) - 1] := Value;
end;

function TUPascalString.GetFirst: USystemChar;
begin
  Result := Buff[0];
end;

procedure TUPascalString.SetFirst(const Value: USystemChar);
begin
  Buff[0] := Value;
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
  CombineCharsPP(Lhs.Buff, Rhs.Buff, Result.Buff);
end;

class operator TUPascalString.Add(const Lhs: USystemString; const Rhs: TUPascalString): TUPascalString;
begin
  CombineCharsSP(Lhs, Rhs.Buff, Result.Buff);
end;

class operator TUPascalString.Add(const Lhs: TUPascalString; const Rhs: USystemString): TUPascalString;
begin
  CombineCharsPS(Lhs.Buff, Rhs, Result.Buff);
end;

class operator TUPascalString.Add(const Lhs: USystemChar; const Rhs: TUPascalString): TUPascalString;
begin
  CombineCharsCP(Lhs, Rhs.Buff, Result.Buff);
end;

class operator TUPascalString.Add(const Lhs: TUPascalString; const Rhs: USystemChar): TUPascalString;
begin
  CombineCharsPC(Lhs.Buff, Rhs, Result.Buff);
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
  Result.Buff[0] := Value;
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
  Result.Buff[0] := Value;
end;

{$ENDIF}


function TUPascalString.copy(index, count: NativeInt): TUPascalString;
var
  l: NativeInt;
begin
  l := Length(Buff);

  if (index - 1) + count > l then
      count := l - (index - 1);

  SetLength(Result.Buff, count);
  if count > 0 then
      CopyPtr(@Buff[index - 1], @Result.Buff[0], USystemCharSize * count);
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
      s := Buff[i];
      if UCharIn(s, ucHiAtoZ) then
          inc(s, 32);
      d := p^.Buff[i];
      if UCharIn(d, ucHiAtoZ) then
          inc(d, 32);
      if s <> d then
          Exit(False);
    end;
end;

function TUPascalString.Same(const t: TUPascalString): Boolean;
var
  i: Integer;
  s, d: USystemChar;
begin
  Result := (t.Len = Len);
  if not Result then
      Exit;
  for i := 0 to Len - 1 do
    begin
      s := Buff[i];
      if UCharIn(s, ucHiAtoZ) then
          inc(s, 32);
      d := t.Buff[i];
      if UCharIn(d, ucHiAtoZ) then
          inc(d, 32);
      if s <> d then
          Exit(False);
    end;
end;

function TUPascalString.Same(const IgnoreCase: Boolean; const t: TUPascalString): Boolean;
var
  i: Integer;
  s, d: USystemChar;
begin
  Result := (t.Len = Len);
  if not Result then
      Exit;
  for i := 0 to Len - 1 do
    begin

      s := Buff[i];
      if IgnoreCase then
        if UCharIn(s, ucHiAtoZ) then
            inc(s, 32);

      d := t.Buff[i];
      if IgnoreCase then
        if UCharIn(d, ucHiAtoZ) then
            inc(d, 32);

      if s <> d then
          Exit(False);
    end;
end;

function TUPascalString.ComparePos(const Offset: Integer; const p: PUPascalString): Boolean;
var
  i, l: Integer;
  sourChar, destChar: USystemChar;
begin
  Result := False;
  i := 1;
  l := p^.Len;
  if (Offset + l - 1) > Len then
      Exit;
  while i <= l do
    begin
      sourChar := GetChars(Offset + i - 1);
      destChar := p^[i];

      if UCharIn(sourChar, ucLoAtoZ) then
          dec(sourChar, 32);
      if UCharIn(destChar, ucLoAtoZ) then
          dec(destChar, 32);

      if sourChar <> destChar then
          Exit;
      inc(i);
    end;
  Result := True;
end;

function TUPascalString.ComparePos(const Offset: Integer; const t: TUPascalString): Boolean;
var
  i, l: Integer;
  sourChar, destChar: USystemChar;
begin
  Result := False;
  i := 1;
  l := t.Len;
  if (Offset + l) > Len then
      Exit;
  while i <= l do
    begin
      sourChar := GetChars(Offset + i - 1);
      destChar := t[i];

      if UCharIn(sourChar, ucLoAtoZ) then
          dec(sourChar, 32);
      if UCharIn(destChar, ucLoAtoZ) then
          dec(destChar, 32);

      if sourChar <> destChar then
          Exit;
      inc(i);
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

function TUPascalString.Exists(c: USystemChar): Boolean;
var
  i: Integer;
begin
  for i := low(Buff) to high(Buff) do
    if Buff[i] = c then
        Exit(True);
  Result := False;
end;

function TUPascalString.Exists(c: array of USystemChar): Boolean;
var
  i: Integer;
begin
  for i := low(Buff) to high(Buff) do
    if UCharIn(Buff[i], c) then
        Exit(True);
  Result := False;
end;

function TUPascalString.Exists(const s: TUPascalString): Boolean;
begin
  Result := GetPos(@s, 1) > 0;
end;

function TUPascalString.Hash: TUHash;
begin
  Result := UFastHashPascalString(@Self);
end;

function TUPascalString.Hash64: TUHash64;
begin
  Result := UFastHash64PascalString(@Self);
end;

function TUPascalString.GetCharCount(c: USystemChar): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := low(Buff) to high(Buff) do
    if UCharIn(Buff[i], c) then
        inc(Result);
end;

procedure TUPascalString.DeleteLast;
begin
  if Len > 0 then
      SetLength(Buff, Length(Buff) - 1);
end;

procedure TUPascalString.DeleteFirst;
begin
  if Len > 0 then
      Buff := System.copy(Buff, 1, Len);
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
  SetLength(Buff, 0);
end;

procedure TUPascalString.Append(t: TUPascalString);
var
  r, l: Integer;
begin
  l := Length(t.Buff);
  if l > 0 then
    begin
      r := Length(Buff);
      SetLength(Buff, r + l);
      CopyPtr(@t.Buff[0], @Buff[r], l * USystemCharSize);
    end;
end;

procedure TUPascalString.Append(c: USystemChar);
begin
  SetLength(Buff, Length(Buff) + 1);
  Buff[Length(Buff) - 1] := c;
end;

function TUPascalString.GetString(bPos, ePos: NativeInt): TUPascalString;
begin
  if ePos > Length(Buff) then
      Result := Self.copy(bPos, Length(Buff) - bPos + 1)
  else
      Result := Self.copy(bPos, (ePos - bPos));
end;

procedure TUPascalString.Insert(AText: USystemString; idx: Integer);
begin
  Text := GetString(1, idx) + AText + GetString(idx + 1, Len);
end;

procedure TUPascalString.FastAsText(var Output: USystemString);
begin
  SetLength(Output, Length(Buff));
  if Length(Buff) > 0 then
      CopyPtr(@Buff[0], @Output[UFirstCharPos], Length(Buff) * USystemCharSize);
end;

procedure TUPascalString.FastGetBytes(var Output: TBytes);
begin
  {$IFDEF FPC}
  Output := SysUtils.TEncoding.UTF8.GetBytes(Buff);
  {$ELSE}
  Output := SysUtils.TEncoding.UTF8.GetBytes(Buff);
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
  i, j: Integer;
begin
  SetLength(Result.Buff, Length(Buff));
  j := low(Result.Buff);
  for i := high(Buff) downto low(Buff) do
    begin
      Result.Buff[j] := Buff[i];
      inc(j);
    end;
end;

function TUPascalString.TrimChar(const charS: TUPascalString): TUPascalString;
var
  l, bp, ep: Integer;
begin
  Result := '';
  l := Len;
  if l > 0 then
    begin
      bp := 1;
      while UCharIn(GetChars(bp), @charS) do
        begin
          inc(bp);
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

          while UCharIn(GetChars(ep), @charS) do
            begin
              dec(ep);
              if (ep < 1) then
                begin
                  Result := '';
                  Exit;
                end;
            end;
          Result := GetString(bp, ep + 1);
        end;
    end;
end;

function TUPascalString.DeleteChar(const charS: TUPascalString): TUPascalString;
var
  c: USystemChar;
begin
  Result := '';
  for c in Buff do
    if not UCharIn(c, @charS) then
        Result.Append(c);
end;

function TUPascalString.DeleteChar(const charS: TUOrdChars): TUPascalString;
var
  c: USystemChar;
begin
  Result := '';
  for c in Buff do
    if not UCharIn(c, charS) then
        Result.Append(c);
end;

function TUPascalString.ReplaceChar(const charS: TUPascalString; const newChar: USystemChar): TUPascalString;
var
  i: Integer;
begin
  Result.Len := Len;
  for i := low(Buff) to high(Buff) do
    if UCharIn(Buff[i], charS) then
        Result.Buff[i] := newChar
    else
        Result.Buff[i] := Buff[i];
end;

function TUPascalString.ReplaceChar(const charS, newChar: USystemChar): TUPascalString;
var
  i: Integer;
begin
  Result.Len := Len;
  for i := low(Buff) to high(Buff) do
    if UCharIn(Buff[i], charS) then
        Result.Buff[i] := newChar
    else
        Result.Buff[i] := Buff[i];
end;

function TUPascalString.ReplaceChar(const charS: TUOrdChars; const newChar: USystemChar): TUPascalString;
var
  i: Integer;
begin
  Result.Len := Len;
  for i := low(Buff) to high(Buff) do
    if UCharIn(Buff[i], charS) then
        Result.Buff[i] := newChar
    else
        Result.Buff[i] := Buff[i];
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
