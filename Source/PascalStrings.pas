{ ***************************************************************************** }
{ * string support,writen by QQ 600585@qq.com                                 * }
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
  2017-11-26
  fixed UnicodeString in FPC
*)

unit PascalStrings;

interface

{$INCLUDE zDefine.inc}


uses SysUtils;

type
  SystemChar    = Char;
  SystemString  = string;
  THash         = Cardinal;
  THash64       = UInt64;
  PSystemString = ^SystemString;
  PPascalString = ^TPascalString;
  TArrayChar    = packed array of SystemChar;
  TOrdChar      = (c0to9, c1to9, c0to32, c0to32no10, cLoAtoF, cHiAtoF, cLoAtoZ, cHiAtoZ, cHex, cAtoF, cAtoZ);
  TOrdChars     = set of TOrdChar;

  TPascalString = record
  private
    function GetText: SystemString;
    procedure SetText(const Value: SystemString);
    function GetLen: Integer;
    procedure SetLen(const Value: Integer);
    function GetChars(index: Integer): SystemChar;
    procedure SetChars(index: Integer; const Value: SystemChar);
    function GetBytes: TBytes;
    procedure SetBytes(const Value: TBytes);
    function GetLast: SystemChar;
    procedure SetLast(const Value: SystemChar);
    function GetFirst: SystemChar;
    procedure SetFirst(const Value: SystemChar);
  public
    buff: TArrayChar;

{$IFDEF DELPHI}
    class operator Equal(const Lhs, Rhs: TPascalString): Boolean;
    class operator NotEqual(const Lhs, Rhs: TPascalString): Boolean;
    class operator GreaterThan(const Lhs, Rhs: TPascalString): Boolean;
    class operator GreaterThanOrEqual(const Lhs, Rhs: TPascalString): Boolean;
    class operator LessThan(const Lhs, Rhs: TPascalString): Boolean;
    class operator LessThanOrEqual(const Lhs, Rhs: TPascalString): Boolean;

    class operator Add(const Lhs, Rhs: TPascalString): TPascalString;
    class operator Add(const Lhs: SystemString; const Rhs: TPascalString): TPascalString;
    class operator Add(const Lhs: TPascalString; const Rhs: SystemString): TPascalString;
    class operator Add(const Lhs: SystemChar; const Rhs: TPascalString): TPascalString;
    class operator Add(const Lhs: TPascalString; const Rhs: SystemChar): TPascalString;

    class operator Implicit(Value: Variant): TPascalString;
    class operator Implicit(Value: SystemString): TPascalString;
    class operator Implicit(Value: SystemChar): TPascalString;
    class operator Implicit(Value: TPascalString): SystemString;
    class operator Implicit(Value: TPascalString): Variant;

    class operator Explicit(Value: TPascalString): SystemString;
    class operator Explicit(Value: SystemString): TPascalString;
    class operator Explicit(Value: SystemChar): TPascalString;
    class operator Explicit(Value: Variant): TPascalString;
    class operator Explicit(Value: TPascalString): Variant;
{$ENDIF}
    function Copy(index, Count: nativeInt): TPascalString;
    function Same(const p: PPascalString): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Same(const T: TPascalString): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Same(const IgnoreCase: Boolean; const T: TPascalString): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ComparePos(const Offset: Integer; const p: PPascalString): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ComparePos(const Offset: Integer; const T: TPascalString): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function GetPos(const s: TPascalString; const Offset: Integer = 1): Integer; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function GetPos(const s: PPascalString; const Offset: Integer = 1): Integer; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Exists(C: SystemChar): Boolean; overload;
    function Exists(C: array of SystemChar): Boolean; overload;
    function Exists(const s: TPascalString): Boolean; overload;
    function GetCharCount(C: SystemChar): Integer;
    //
    function hash: THash; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Hash64: THash64; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    //
    property Last: SystemChar read GetLast write SetLast;
    property First: SystemChar read GetFirst write SetFirst;

    procedure DeleteLast; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure DeleteFirst; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure Delete(idx, cnt: Integer); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure Clear; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure Append(T: TPascalString); overload;
    procedure Append(C: SystemChar); overload;
    function GetString(bPos, ePos: nativeInt): TPascalString; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure Insert(AText: SystemString; idx: Integer); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    //
    procedure FastAsText(var output: SystemString);
    procedure FastGetBytes(var output: TBytes); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    //
    property Text: SystemString read GetText write SetText;
    function LowerText: SystemString;
    function UpperText: SystemString;
    function Invert: TPascalString;
    function TrimChar(const Chars: TPascalString): TPascalString;
    function DeleteChar(const Chars: TPascalString): TPascalString; overload;
    function DeleteChar(const Chars: TOrdChars): TPascalString; overload;
    function ReplaceChar(const Chars: TPascalString; const newChar: SystemChar): TPascalString; overload;
    function ReplaceChar(const Chars, newChar: SystemChar): TPascalString; overload;
    function ReplaceChar(const Chars: TOrdChars; const newChar: SystemChar): TPascalString; overload;

    { https://en.wikipedia.org/wiki/Smith%E2%80%93Waterman_algorithm }
    function SmithWaterman(const p: PPascalString): Double; overload;
    function SmithWaterman(const s: TPascalString): Double; overload;

    property Len: Integer read GetLen write SetLen;
    property Chars[index: Integer]: SystemChar read GetChars write SetChars; default;
    property Bytes: TBytes read GetBytes write SetBytes;
    function BOMBytes: TBytes;
  end;

  TArrayPascalString = array of TPascalString;
  PArrayPascalString = ^TArrayPascalString;

  TArrayPascalStringPtr = array of PPascalString;
  PArrayPascalStringPtr = ^TArrayPascalStringPtr;

function CharIn(C: SystemChar; const SomeChars: array of SystemChar): Boolean; overload;
function CharIn(C: SystemChar; const SomeChar: SystemChar): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function CharIn(C: SystemChar; const s: TPascalString): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function CharIn(C: SystemChar; const p: PPascalString): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function CharIn(C: SystemChar; const SomeCharsets: TOrdChars): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function CharIn(C: SystemChar; const SomeCharset: TOrdChar): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function CharIn(C: SystemChar; const SomeCharsets: TOrdChars; const SomeChars: TPascalString): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function CharIn(C: SystemChar; const SomeCharsets: TOrdChars; const p: PPascalString): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function FastHashSystemString(const s: PSystemString): THash; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function FastHash64SystemString(const s: PSystemString): THash64; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function FastHashSystemString(const s: SystemString): THash; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function FastHash64SystemString(const s: SystemString): THash64; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function FastHashPascalString(const s: PPascalString): THash; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function FastHash64PascalString(const s: PPascalString): THash64; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function PFormat(const Fmt: SystemString; const Args: array of const): SystemString;

{$IFDEF FPC}

operator := (const s: Variant)R: TPascalString;
operator := (const s: AnsiString)R: TPascalString;
operator := (const s: UnicodeString)R: TPascalString;
operator := (const s: WideString)R: TPascalString;
operator := (const s: ShortString)R: TPascalString;
operator := (const C: SystemChar)R: TPascalString;

operator := (const s: TPascalString)R: AnsiString;
operator := (const s: TPascalString)R: UnicodeString;
operator := (const s: TPascalString)R: WideString;
operator := (const s: TPascalString)R: ShortString;
operator := (const s: TPascalString)R: Variant;

operator = (const A: TPascalString; const b: TPascalString): Boolean;
operator <> (const A: TPascalString; const b: TPascalString): Boolean;
operator > (const A: TPascalString; const b: TPascalString): Boolean;
operator >= (const A: TPascalString; const b: TPascalString): Boolean;
operator < (const A: TPascalString; const b: TPascalString): Boolean;
operator <= (const A: TPascalString; const b: TPascalString): Boolean;

operator + (const A: TPascalString; const b: TPascalString): TPascalString;
operator + (const A: TPascalString; const b: SystemString): TPascalString;
operator + (const A: SystemString; const b: TPascalString): TPascalString;
operator + (const A: TPascalString; const b: SystemChar): TPascalString;
operator + (const A: SystemChar; const b: TPascalString): TPascalString;

{$ENDIF FPC}

{ https://en.wikipedia.org/wiki/Smith%E2%80%93Waterman_algorithm }

// short string likeness and out diff
function SmithWatermanCompare(const seq1, seq2: PPascalString; var diff1, diff2: TPascalString;
  const NoDiffChar: Boolean; const diffChar: SystemChar): Double; overload;
function SmithWatermanCompare(const seq1, seq2: PPascalString; var diff1, diff2: TPascalString): Double; overload;
function SmithWatermanCompare(const seq1, seq2: TPascalString; var diff1, diff2: TPascalString;
  const NoDiffChar: Boolean; const diffChar: SystemChar): Double; overload;
function SmithWatermanCompare(const seq1, seq2: TPascalString; var diff1, diff2: TPascalString): Double; overload;

// short string likeness
function SmithWatermanCompare(const seq1, seq2: PPascalString; out Same, Diff: Integer): Double; overload;
function SmithWatermanCompare(const seq1, seq2: PPascalString): Double; overload;
function SmithWatermanCompare(const seq1, seq2: TPascalString): Double; overload;
function SmithWatermanCompare(const seq1: TArrayPascalString; const seq2: TPascalString): Double; overload;

// memory likeness
function SmithWatermanCompare(const seq1: Pointer; siz1: Integer; const seq2: Pointer; siz2: Integer;
  out Same, Diff: Integer): Double; overload;
function SmithWatermanCompare(const seq1: Pointer; siz1: Integer; const seq2: Pointer; siz2: Integer): Double; overload;

// long string likeness
function SmithWatermanCompareLongString(const t1, t2: TPascalString; const MinDiffCharWithPeerLine: Integer; out Same, Diff: Integer): Double; overload;
function SmithWatermanCompareLongString(const t1, t2: TPascalString): Double; overload;

var
  SystemCharSize: nativeInt = SizeOf(SystemChar);
{$IFDEF CPU64}
  MaxSmithWatermanMatrix: nativeInt = 10000 * 10;
{$ELSE}
  MaxSmithWatermanMatrix: nativeInt = 8192;
{$ENDIF}


const
{$IFDEF FirstCharInZero}
  FirstCharPos = 0;
{$ELSE}
  FirstCharPos = 1;
{$ENDIF}

implementation

uses CoreClasses, Variants;

procedure CombineCharsPP(const c1, c2: TArrayChar; var output: TArrayChar); {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  LL, rl: Integer;
begin
  LL := length(c1);
  rl := length(c2);
  SetLength(output, LL + rl);
  if LL > 0 then
      CopyPtr(@c1[0], @output[0], LL * SystemCharSize);
  if rl > 0 then
      CopyPtr(@c2[0], @output[LL], rl * SystemCharSize);
end;

procedure CombineCharsSP(const c1: SystemString; const c2: TArrayChar; var output: TArrayChar); {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  LL, rl: Integer;
begin
  LL := length(c1);
  rl := length(c2);
  SetLength(output, LL + rl);
  if LL > 0 then
      CopyPtr(@c1[FirstCharPos], @output[0], LL * SystemCharSize);
  if rl > 0 then
      CopyPtr(@c2[0], @output[LL], rl * SystemCharSize);
end;

procedure CombineCharsPS(const c1: TArrayChar; const c2: SystemString; var output: TArrayChar); {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  LL, rl: Integer;
begin
  LL := length(c1);
  rl := length(c2);
  SetLength(output, LL + rl);
  if LL > 0 then
      CopyPtr(@c1[0], @output[0], LL * SystemCharSize);
  if rl > 0 then
      CopyPtr(@c2[FirstCharPos], @output[LL], rl * SystemCharSize);
end;

procedure CombineCharsCP(const c1: SystemChar; const c2: TArrayChar; var output: TArrayChar); {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  rl: Integer;
begin
  rl := length(c2);
  SetLength(output, rl + 1);
  output[0] := c1;
  if rl > 0 then
      CopyPtr(@c2[0], @output[1], rl * SystemCharSize);
end;

procedure CombineCharsPC(const c1: TArrayChar; const c2: SystemChar; var output: TArrayChar); {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  LL: Integer;
begin
  LL := length(c1);
  SetLength(output, LL + 1);
  if LL > 0 then
      CopyPtr(@c1[0], @output[0], LL * SystemCharSize);
  output[LL] := c2;
end;

function CharIn(C: SystemChar; const SomeChars: array of SystemChar): Boolean;
var
  AChar: SystemChar;
begin
  Result := True;
  for AChar in SomeChars do
    if AChar = C then
        Exit;
  Result := False;
end;

function CharIn(C: SystemChar; const SomeChar: SystemChar): Boolean;
begin
  Result := C = SomeChar;
end;

function CharIn(C: SystemChar; const s: TPascalString): Boolean;
begin
  Result := s.Exists(C);
end;

function CharIn(C: SystemChar; const p: PPascalString): Boolean;
begin
  Result := p^.Exists(C);
end;

function CharIn(C: SystemChar; const SomeCharset: TOrdChar): Boolean;
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
    c0to9: Result := (v >= ord0) and (v <= ord9);
    c1to9: Result := (v >= ord1) and (v <= ord9);
    c0to32: Result := ((v >= 0) and (v <= 32));
    c0to32no10: Result := ((v >= 0) and (v <= 32) and (v <> 10));
    cLoAtoF: Result := (v >= ordLA) and (v <= ordLF);
    cHiAtoF: Result := (v >= ordHA) and (v <= ordHF);
    cLoAtoZ: Result := (v >= ordLA) and (v <= ordLZ);
    cHiAtoZ: Result := (v >= ordHA) and (v <= ordHZ);
    cHex: Result := ((v >= ordLA) and (v <= ordLF)) or ((v >= ordHA) and (v <= ordHF)) or ((v >= ord0) and (v <= ord9));
    cAtoF: Result := ((v >= ordLA) and (v <= ordLF)) or ((v >= ordHA) and (v <= ordHF));
    cAtoZ: Result := ((v >= ordLA) and (v <= ordLZ)) or ((v >= ordHA) and (v <= ordHZ));
    else Result := False;
  end;
end;

function CharIn(C: SystemChar; const SomeCharsets: TOrdChars): Boolean;
var
  i: TOrdChar;
begin
  Result := True;
  for i in SomeCharsets do
    if CharIn(C, i) then
        Exit;
  Result := False;
end;

function CharIn(C: SystemChar; const SomeCharsets: TOrdChars; const SomeChars: TPascalString): Boolean;
begin
  if CharIn(C, SomeCharsets) then
      Result := True
  else
      Result := CharIn(C, SomeChars);
end;

function CharIn(C: SystemChar; const SomeCharsets: TOrdChars; const p: PPascalString): Boolean;
begin
  if CharIn(C, SomeCharsets) then
      Result := True
  else
      Result := CharIn(C, p);
end;

function BytesOfPascalString(const s: TPascalString): TBytes;
begin
  Result := s.Bytes;
end;

function PascalStringOfBytes(const s: TBytes): TPascalString;
begin
  Result.Bytes := s;
end;

function FastHashSystemString(const s: PSystemString): THash;
var
  i: Integer;
  C: SystemChar;
begin
  Result := 0;

{$IFDEF FirstCharInZero}
  for i := 0 to length(s^) - 1 do
{$ELSE}
  for i := 1 to length(s^) do
{$ENDIF}
    begin
      C := s^[i];
      if CharIn(C, cHiAtoZ) then
          Inc(C, 32);
      Result := ((Result shl 7) or (Result shr 25)) + THash(C);
    end;
end;

function FastHash64SystemString(const s: PSystemString): THash64;
var
  i: Integer;
  C: SystemChar;
begin
  Result := 0;

{$IFDEF FirstCharInZero}
  for i := 0 to length(s^) - 1 do
{$ELSE}
  for i := 1 to length(s^) do
{$ENDIF}
    begin
      C := s^[i];
      if CharIn(C, cHiAtoZ) then
          Inc(C, 32);
      Result := ((Result shl 7) or (Result shr 57)) + THash64(C);
    end;
end;

function FastHashSystemString(const s: SystemString): THash;
begin
  Result := FastHashSystemString(@s);
end;

function FastHash64SystemString(const s: SystemString): THash64;
begin
  Result := FastHash64SystemString(@s);
end;

function FastHashPascalString(const s: PPascalString): THash;
var
  i: Integer;
  C: SystemChar;
begin
  Result := 0;
  for i := 1 to s^.Len do
    begin
      C := s^[i];
      if CharIn(C, cHiAtoZ) then
          Inc(C, 32);
      Result := ((Result shl 7) or (Result shr 25)) + THash(C);
    end;
end;

function FastHash64PascalString(const s: PPascalString): THash64;
var
  i: Integer;
  C: SystemChar;
begin
  Result := 0;
  for i := 1 to s^.Len do
    begin
      C := s^[i];
      if CharIn(C, cHiAtoZ) then
          Inc(C, 32);
      Result := ((Result shl 7) or (Result shr 57)) + THash64(C);
    end;
end;

function PFormat(const Fmt: SystemString; const Args: array of const): SystemString;
begin
  Result := Format(Fmt, Args);
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

function SmithWatermanCompare(const seq1, seq2: PPascalString; var diff1, diff2: TPascalString;
  const NoDiffChar: Boolean; const diffChar: SystemChar): Double;

  function InlineMatch(alphaC, betaC: SystemChar; const diffC: SystemChar): Integer; inline;
  begin
    if CharIn(alphaC, cLoAtoZ) then
        Dec(alphaC, 32);
    if CharIn(betaC, cLoAtoZ) then
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
  align1, align2: TPascalString;
begin
  L1 := seq1^.Len;
  l2 := seq2^.Len;

  if (L1 = 0) or (l2 = 0) or (L1 > MaxSmithWatermanMatrix) or (l2 > MaxSmithWatermanMatrix) then
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

function SmithWatermanCompare(const seq1, seq2: PPascalString; var diff1, diff2: TPascalString): Double;
begin
  Result := SmithWatermanCompare(seq1, seq2, diff1, diff2, False, '-');
end;

function SmithWatermanCompare(const seq1, seq2: TPascalString; var diff1, diff2: TPascalString;
  const NoDiffChar: Boolean; const diffChar: SystemChar): Double;
begin
  Result := SmithWatermanCompare(@seq1, @seq2, diff1, diff2, NoDiffChar, diffChar);
end;

function SmithWatermanCompare(const seq1, seq2: TPascalString; var diff1, diff2: TPascalString): Double;
begin
  Result := SmithWatermanCompare(seq1, seq2, diff1, diff2, False, '-');
end;

function SmithWatermanCompare(const seq1, seq2: PPascalString; out Same, Diff: Integer): Double;

  function InlineMatch(alphaC, betaC: SystemChar): nativeInt; inline;
  begin
    if CharIn(alphaC, cLoAtoZ) then
        Dec(alphaC, 32);
    if CharIn(betaC, cLoAtoZ) then
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

  if (L1 = 0) or (l2 = 0) or (L1 > MaxSmithWatermanMatrix) or (l2 > MaxSmithWatermanMatrix) then
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

function SmithWatermanCompare(const seq1, seq2: PPascalString): Double;
var
  Same, Diff: Integer;
begin
  Result := SmithWatermanCompare(seq1, seq2, Same, Diff);
end;

function SmithWatermanCompare(const seq1, seq2: TPascalString): Double;
begin
  Result := SmithWatermanCompare(@seq1, @seq2);
end;

function SmithWatermanCompare(const seq1: TArrayPascalString; const seq2: TPascalString): Double;
var
  i: Integer;
  R: Double;
begin
  Result := -1;
  for i := 0 to length(seq1) - 1 do
    begin
      R := SmithWatermanCompare(seq1[i], seq2);
      if R > Result then
          Result := R;
    end;
end;

function SmithWatermanCompare(const seq1: Pointer; siz1: Integer; const seq2: Pointer; siz2: Integer;
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

  if (L1 = 0) or (l2 = 0) or (L1 > MaxSmithWatermanMatrix) or (l2 > MaxSmithWatermanMatrix) then
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

function SmithWatermanCompare(const seq1: Pointer; siz1: Integer; const seq2: Pointer; siz2: Integer): Double;
var
  Same, Diff: Integer;
begin
  Result := SmithWatermanCompare(seq1, siz1, seq2, siz2, Same, Diff);
end;

function SmithWatermanCompareLongString(const t1, t2: TPascalString; const MinDiffCharWithPeerLine: Integer; out Same, Diff: Integer): Double;
type
  PSRec = ^TSRec;

  TSRec = packed record
    s: TPascalString;
  end;

  procedure _FillText(psPtr: PPascalString; outLst: TCoreClassList);
  var
    L, i: Integer;
    n: TPascalString;
    p: PSRec;
  begin
    L := psPtr^.Len;
    i := 1;
    n := '';
    while i <= L do
      begin
        if CharIn(psPtr^[i], [#13, #10]) then
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
            until (i > L) or (not CharIn(psPtr^[i], [#13, #10, #32, #9]));
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
    if SmithWatermanCompare(@alpha^.s, @beta^.s, cSame, cDiff) > 0 then
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

  if (L1 = 0) or (l2 = 0) or (L1 > MaxSmithWatermanMatrix) or (l2 > MaxSmithWatermanMatrix) then
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

function SmithWatermanCompareLongString(const t1, t2: TPascalString): Double;
var
  Same, Diff: Integer;
begin
  Result := SmithWatermanCompareLongString(t1, t2, 5, Same, Diff);
end;

{$IFDEF FPC}


operator := (const s: Variant)R: TPascalString;
begin
  R.Text := s;
end;

operator := (const s: AnsiString)R: TPascalString;
begin
  R.Text := s;
end;

operator := (const s: UnicodeString)R: TPascalString;
begin
  R.Text := s;
end;

operator := (const s: WideString)R: TPascalString;
begin
  R.Text := s;
end;

operator := (const s: ShortString)R: TPascalString;
begin
  R.Text := s;
end;

operator := (const C: SystemChar)R: TPascalString;
begin
  R.Text := C;
end;

operator := (const s: TPascalString)R: AnsiString;
begin
  R := s.Text;
end;

operator := (const s: TPascalString)R: UnicodeString;
begin
  R := s.Text;
end;

operator := (const s: TPascalString)R: WideString;
begin
  R := s.Text;
end;

operator := (const s: TPascalString)R: ShortString;
begin
  R := s.Text;
end;

operator := (const s: TPascalString)R: Variant;
begin
  R := s.Text;
end;

operator = (const A: TPascalString; const b: TPascalString): Boolean;
begin
  Result := A.Text = b.Text;
end;

operator <> (const A: TPascalString; const b: TPascalString): Boolean;
begin
  Result := A.Text <> b.Text;
end;

operator > (const A: TPascalString; const b: TPascalString): Boolean;
begin
  Result := A.Text > b.Text;
end;

operator >= (const A: TPascalString; const b: TPascalString): Boolean;
begin
  Result := A.Text >= b.Text;
end;

operator < (const A: TPascalString; const b: TPascalString): Boolean;
begin
  Result := A.Text < b.Text;
end;

operator <= (const A: TPascalString; const b: TPascalString): Boolean;
begin
  Result := A.Text <= b.Text;
end;

operator + (const A: TPascalString; const b: TPascalString): TPascalString;
begin
  CombineCharsPP(A.buff, b.buff, Result.buff);
end;

operator + (const A: TPascalString; const b: SystemString): TPascalString;
begin
  CombineCharsPS(A.buff, b, Result.buff);
end;

operator + (const A: SystemString; const b: TPascalString): TPascalString;
begin
  CombineCharsSP(A, b.buff, Result.buff);
end;

operator + (const A: TPascalString; const b: SystemChar): TPascalString;
begin
  CombineCharsPC(A.buff, b, Result.buff);
end;

operator + (const A: SystemChar; const b: TPascalString): TPascalString;
begin
  CombineCharsCP(A, b.buff, Result.buff);
end;

{$ENDIF}


function TPascalString.GetText: SystemString;
begin
  SetLength(Result, length(buff));
  if length(buff) > 0 then
      CopyPtr(@buff[0], @Result[FirstCharPos], length(buff) * SystemCharSize);
end;

procedure TPascalString.SetText(const Value: SystemString);
begin
  SetLength(buff, length(Value));

  if length(buff) > 0 then
      CopyPtr(@Value[FirstCharPos], @buff[0], length(buff) * SystemCharSize);
end;

function TPascalString.GetLen: Integer;
begin
  Result := length(buff);
end;

procedure TPascalString.SetLen(const Value: Integer);
begin
  SetLength(buff, Value);
end;

function TPascalString.GetChars(index: Integer): SystemChar;
begin
  if (index > length(buff)) or (index <= 0) then
      Result := #0
  else
      Result := buff[index - 1];
end;

procedure TPascalString.SetChars(index: Integer; const Value: SystemChar);
begin
  buff[index - 1] := Value;
end;

procedure TPascalString.SetBytes(const Value: TBytes);
begin
  SetLength(buff, 0);
  try
      Text := SysUtils.TEncoding.UTF8.GetString(Value);
  except
      SetLength(buff, 0);
  end;
end;

function TPascalString.GetBytes: TBytes;
begin
{$IFDEF FPC}
  Result := SysUtils.TEncoding.UTF8.GetBytes(Text);
{$ELSE}
  Result := SysUtils.TEncoding.UTF8.GetBytes(buff);
{$ENDIF}
end;

function TPascalString.GetLast: SystemChar;
begin
  Result := buff[length(buff) - 1];
end;

procedure TPascalString.SetLast(const Value: SystemChar);
begin
  buff[length(buff) - 1] := Value;
end;

function TPascalString.GetFirst: SystemChar;
begin
  Result := buff[0];
end;

procedure TPascalString.SetFirst(const Value: SystemChar);
begin
  buff[0] := Value;
end;

{$IFDEF DELPHI}


class operator TPascalString.Equal(const Lhs, Rhs: TPascalString): Boolean;
begin
  Result := (Lhs.Len = Rhs.Len) and (Lhs.Text = Rhs.Text);
end;

class operator TPascalString.NotEqual(const Lhs, Rhs: TPascalString): Boolean;
begin
  Result := not(Lhs = Rhs);
end;

class operator TPascalString.GreaterThan(const Lhs, Rhs: TPascalString): Boolean;
begin
  Result := Lhs.Text > Rhs.Text;
end;

class operator TPascalString.GreaterThanOrEqual(const Lhs, Rhs: TPascalString): Boolean;
begin
  Result := Lhs.Text >= Rhs.Text;
end;

class operator TPascalString.LessThan(const Lhs, Rhs: TPascalString): Boolean;
begin
  Result := Lhs.Text < Rhs.Text;
end;

class operator TPascalString.LessThanOrEqual(const Lhs, Rhs: TPascalString): Boolean;
begin
  Result := Lhs.Text <= Rhs.Text;
end;

class operator TPascalString.Add(const Lhs, Rhs: TPascalString): TPascalString;
begin
  CombineCharsPP(Lhs.buff, Rhs.buff, Result.buff);
end;

class operator TPascalString.Add(const Lhs: SystemString; const Rhs: TPascalString): TPascalString;
begin
  CombineCharsSP(Lhs, Rhs.buff, Result.buff);
end;

class operator TPascalString.Add(const Lhs: TPascalString; const Rhs: SystemString): TPascalString;
begin
  CombineCharsPS(Lhs.buff, Rhs, Result.buff);
end;

class operator TPascalString.Add(const Lhs: SystemChar; const Rhs: TPascalString): TPascalString;
begin
  CombineCharsCP(Lhs, Rhs.buff, Result.buff);
end;

class operator TPascalString.Add(const Lhs: TPascalString; const Rhs: SystemChar): TPascalString;
begin
  CombineCharsPC(Lhs.buff, Rhs, Result.buff);
end;

class operator TPascalString.Implicit(Value: Variant): TPascalString;
begin
  Result.Text := VarToStr(Value);
end;

class operator TPascalString.Implicit(Value: SystemString): TPascalString;
begin
  Result.Text := Value;
end;

class operator TPascalString.Implicit(Value: SystemChar): TPascalString;
begin
  Result.Len := 1;
  Result.buff[0] := Value;
end;

class operator TPascalString.Implicit(Value: TPascalString): SystemString;
begin
  Result := Value.Text;
end;

class operator TPascalString.Implicit(Value: TPascalString): Variant;
begin
  Result := Value.Text;
end;

class operator TPascalString.Explicit(Value: TPascalString): SystemString;
begin
  Result := Value.Text;
end;

class operator TPascalString.Explicit(Value: TPascalString): Variant;
begin
  Result := Value.Text;
end;

class operator TPascalString.Explicit(Value: SystemString): TPascalString;
begin
  Result.Text := Value;
end;

class operator TPascalString.Explicit(Value: Variant): TPascalString;
begin
  Result.Text := VarToStr(Value);
end;

class operator TPascalString.Explicit(Value: SystemChar): TPascalString;
begin
  Result.Len := 1;
  Result.buff[0] := Value;
end;

{$ENDIF}


function TPascalString.Copy(index, Count: nativeInt): TPascalString;
var
  L: nativeInt;
begin
  L := length(buff);

  if (index - 1) + Count > L then
      Count := L - (index - 1);

  SetLength(Result.buff, Count);
  if Count > 0 then
      CopyPtr(@buff[index - 1], @Result.buff[0], SystemCharSize * Count);
end;

function TPascalString.Same(const p: PPascalString): Boolean;
var
  i: Integer;
  s, d: SystemChar;
begin
  Result := (p^.Len = Len);
  if not Result then
      Exit;
  for i := 0 to Len - 1 do
    begin
      s := buff[i];
      if CharIn(s, cHiAtoZ) then
          Inc(s, 32);
      d := p^.buff[i];
      if CharIn(d, cHiAtoZ) then
          Inc(d, 32);
      if s <> d then
          Exit(False);
    end;
end;

function TPascalString.Same(const T: TPascalString): Boolean;
var
  i: Integer;
  s, d: SystemChar;
begin
  Result := (T.Len = Len);
  if not Result then
      Exit;
  for i := 0 to Len - 1 do
    begin
      s := buff[i];
      if CharIn(s, cHiAtoZ) then
          Inc(s, 32);
      d := T.buff[i];
      if CharIn(d, cHiAtoZ) then
          Inc(d, 32);
      if s <> d then
          Exit(False);
    end;
end;

function TPascalString.Same(const IgnoreCase: Boolean; const T: TPascalString): Boolean;
var
  i: Integer;
  s, d: SystemChar;
begin
  Result := (T.Len = Len);
  if not Result then
      Exit;
  for i := 0 to Len - 1 do
    begin

      s := buff[i];
      if IgnoreCase then
        if CharIn(s, cHiAtoZ) then
            Inc(s, 32);

      d := T.buff[i];
      if IgnoreCase then
        if CharIn(d, cHiAtoZ) then
            Inc(d, 32);

      if s <> d then
          Exit(False);
    end;
end;

function TPascalString.ComparePos(const Offset: Integer; const p: PPascalString): Boolean;
var
  i, L: Integer;
  sourChar, destChar: SystemChar;
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

      if CharIn(sourChar, cLoAtoZ) then
          Dec(sourChar, 32);
      if CharIn(destChar, cLoAtoZ) then
          Dec(destChar, 32);

      if sourChar <> destChar then
          Exit;
      Inc(i);
    end;
  Result := True;
end;

function TPascalString.ComparePos(const Offset: Integer; const T: TPascalString): Boolean;
var
  i, L: Integer;
  sourChar, destChar: SystemChar;
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

      if CharIn(sourChar, cLoAtoZ) then
          Dec(sourChar, 32);
      if CharIn(destChar, cLoAtoZ) then
          Dec(destChar, 32);

      if sourChar <> destChar then
          Exit;
      Inc(i);
    end;
  Result := True;
end;

function TPascalString.GetPos(const s: TPascalString; const Offset: Integer = 1): Integer;
var
  i: Integer;
begin
  Result := 0;
  if s.Len > 0 then
    for i := Offset to Len - s.Len + 1 do
      if ComparePos(i, @s) then
          Exit(i);
end;

function TPascalString.GetPos(const s: PPascalString; const Offset: Integer = 1): Integer;
var
  i: Integer;
begin
  Result := 0;
  if s^.Len > 0 then
    for i := Offset to Len - s^.Len + 1 do
      if ComparePos(i, s) then
          Exit(i);
end;

function TPascalString.Exists(C: SystemChar): Boolean;
var
  i: Integer;
begin
  for i := low(buff) to high(buff) do
    if buff[i] = C then
        Exit(True);
  Result := False;
end;

function TPascalString.Exists(C: array of SystemChar): Boolean;
var
  i: Integer;
begin
  for i := low(buff) to high(buff) do
    if CharIn(buff[i], C) then
        Exit(True);
  Result := False;
end;

function TPascalString.Exists(const s: TPascalString): Boolean;
begin
  Result := GetPos(@s, 1) > 0;
end;

function TPascalString.GetCharCount(C: SystemChar): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := low(buff) to high(buff) do
    if CharIn(buff[i], C) then
        Inc(Result);
end;

function TPascalString.hash: THash;
begin
  Result := FastHashPascalString(@Self);
end;

function TPascalString.Hash64: THash64;
begin
  Result := FastHash64PascalString(@Self);
end;

procedure TPascalString.DeleteLast;
begin
  if Len > 0 then
      SetLength(buff, length(buff) - 1);
end;

procedure TPascalString.DeleteFirst;
begin
  if Len > 0 then
      buff := System.Copy(buff, 1, Len);
end;

procedure TPascalString.Delete(idx, cnt: Integer);
begin
  if (idx + cnt <= Len) then
      Text := GetString(1, idx) + GetString(idx + cnt, Len + 1)
  else
      Text := GetString(1, idx);
end;

procedure TPascalString.Clear;
begin
  SetLength(buff, 0);
end;

procedure TPascalString.Append(T: TPascalString);
var
  R, L: Integer;
begin
  L := length(T.buff);
  if L > 0 then
    begin
      R := length(buff);
      SetLength(buff, R + L);
      CopyPtr(@T.buff[0], @buff[R], L * SystemCharSize);
    end;
end;

procedure TPascalString.Append(C: SystemChar);
begin
  SetLength(buff, length(buff) + 1);
  buff[length(buff) - 1] := C;
end;

function TPascalString.GetString(bPos, ePos: nativeInt): TPascalString;
begin
  if ePos > length(buff) then
      Result := Self.Copy(bPos, length(buff) - bPos + 1)
  else
      Result := Self.Copy(bPos, (ePos - bPos));
end;

procedure TPascalString.Insert(AText: SystemString; idx: Integer);
begin
  Text := GetString(1, idx) + AText + GetString(idx + 1, Len);
end;

procedure TPascalString.FastAsText(var output: SystemString);
begin
  SetLength(output, length(buff));
  if length(buff) > 0 then
      CopyPtr(@buff[0], @output[FirstCharPos], length(buff) * SystemCharSize);
end;

procedure TPascalString.FastGetBytes(var output: TBytes);
begin
{$IFDEF FPC}
  output := SysUtils.TEncoding.UTF8.GetBytes(Text);
{$ELSE}
  output := SysUtils.TEncoding.UTF8.GetBytes(buff);
{$ENDIF}
end;

function TPascalString.LowerText: SystemString;
begin
  Result := LowerCase(Text);
end;

function TPascalString.UpperText: SystemString;
begin
  Result := UpperCase(Text);
end;

function TPascalString.Invert: TPascalString;
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

function TPascalString.TrimChar(const Chars: TPascalString): TPascalString;
var
  L, bp, EP: Integer;
begin
  Result := '';
  L := Len;
  if L > 0 then
    begin
      bp := 1;
      while CharIn(GetChars(bp), @Chars) do
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

          while CharIn(GetChars(EP), @Chars) do
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

function TPascalString.DeleteChar(const Chars: TPascalString): TPascalString;
var
  C: SystemChar;
begin
  Result := '';
  for C in buff do
    if not CharIn(C, @Chars) then
        Result.Append(C);
end;

function TPascalString.DeleteChar(const Chars: TOrdChars): TPascalString;
var
  C: SystemChar;
begin
  Result := '';
  for C in buff do
    if not CharIn(C, Chars) then
        Result.Append(C);
end;

function TPascalString.ReplaceChar(const Chars: TPascalString; const newChar: SystemChar): TPascalString;
var
  i: Integer;
begin
  Result.Len := Len;
  for i := low(buff) to high(buff) do
    if CharIn(buff[i], Chars) then
        Result.buff[i] := newChar
    else
        Result.buff[i] := buff[i];
end;

function TPascalString.ReplaceChar(const Chars, newChar: SystemChar): TPascalString;
var
  i: Integer;
begin
  Result.Len := Len;
  for i := low(buff) to high(buff) do
    if CharIn(buff[i], Chars) then
        Result.buff[i] := newChar
    else
        Result.buff[i] := buff[i];
end;

function TPascalString.ReplaceChar(const Chars: TOrdChars; const newChar: SystemChar): TPascalString;
var
  i: Integer;
begin
  Result.Len := Len;
  for i := low(buff) to high(buff) do
    if CharIn(buff[i], Chars) then
        Result.buff[i] := newChar
    else
        Result.buff[i] := buff[i];
end;

function TPascalString.SmithWaterman(const p: PPascalString): Double;
begin
  Result := SmithWatermanCompare(@Self, @p);
end;

function TPascalString.SmithWaterman(const s: TPascalString): Double;
begin
  Result := SmithWatermanCompare(@Self, @s);
end;

function TPascalString.BOMBytes: TBytes;
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
 
