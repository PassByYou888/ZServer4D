{ ***************************************************************************** }
{ * string support,writen by QQ 600585@qq.com                                 * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ ****************************************************************************** }

(*
  update history
  2017-11-26
  fixed UnicodeString in FPC
*)

unit PascalStrings;

{$I ZDefine.inc}

interface

uses SysUtils;

type
  SystemChar    = Char;
  SystemString  = string;
  THash         = Cardinal;
  THash64       = UInt64;
  PSystemString = ^SystemString;

  PPascalString = ^TPascalString;

  TPascalChars = array of Char;

  TPascalString = packed record
  private
    function GetText: SystemString; inline;
    procedure SetText(const Value: SystemString); inline;
    function GetLen: Integer; inline;
    procedure SetLen(const Value: Integer); inline;
    function GetChars(index: Integer): SystemChar; inline;
    procedure SetChars(index: Integer; const Value: SystemChar); inline;
    function GetBytes: TBytes; inline;
    procedure SetBytes(const Value: TBytes); inline;
    function GetLast: SystemChar; inline;
    procedure SetLast(const Value: SystemChar); inline;
    function GetFirst: SystemChar; inline;
    procedure SetFirst(const Value: SystemChar); inline;
  public
    Buff: TPascalChars;

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
    class operator Explicit(Value: TPascalString): Variant;
    class operator Explicit(Value: SystemString): TPascalString;
    class operator Explicit(Value: Variant): TPascalString;
    class operator Explicit(Value: SystemChar): TPascalString;
    {$ENDIF}
    function copy(index, count: NativeInt): TPascalString;
    function Same(const p: PPascalString): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Same(const t: TPascalString): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Same(const IgnoreCase: Boolean; const t: TPascalString): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ComparePos(const Offset: Integer; const p: PPascalString): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ComparePos(const Offset: Integer; const t: TPascalString): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function GetPos(const SubStr: TPascalString; const Offset: Integer = 1): Integer; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function GetPos(const SubStr: PPascalString; const Offset: Integer = 1): Integer; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Exists(c: SystemChar): Boolean; overload;
    function Exists(c: array of SystemChar): Boolean; overload;
    function Exists(const SubStr: TPascalString): Boolean; overload;
    //
    function Hash: THash; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Hash64: THash64; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    //
    property Last: SystemChar read GetLast write SetLast;
    property First: SystemChar read GetFirst write SetFirst;

    procedure DeleteLast; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure DeleteFirst; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure Delete(idx, cnt: Integer); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure Clear; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure Append(t: TPascalString); overload;
    procedure Append(c: SystemChar); overload;
    function GetString(bPos, ePos: NativeInt): TPascalString; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure Insert(AText: SystemString; idx: Integer); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    //
    procedure FastAsText(var Output: SystemString);
    procedure FastGetBytes(var Output: TBytes); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    //
    property Text: SystemString read GetText write SetText;
    function LowerText: SystemString;
    function UpperText: SystemString;
    function Invert: TPascalString;
    function TrimChar(const charS: TPascalString): TPascalString;
    function DeleteChar(const charS: TPascalString): TPascalString;

    { https://en.wikipedia.org/wiki/Smith%E2%80%93Waterman_algorithm }
    function SmithWaterman(const p: PPascalString): Double; overload;
    function SmithWaterman(const s: TPascalString): Double; overload;

    property Len: Integer read GetLen write SetLen;
    property charS[index: Integer]: SystemChar read GetChars write SetChars; default;
    property Bytes: TBytes read GetBytes write SetBytes;
    function BOMBytes: TBytes;
  end;

  TArrayPascalString = array of TPascalString;

  TOrdChar  = (c0to9, c1to9, c0to32, c0to32no10, cLoAtoF, cHiAtoF, cLoAtoZ, cHiAtoZ, cHex, cAtoF, cAtoZ);
  TOrdChars = set of TOrdChar;

function CharIn(c: SystemChar; const SomeChars: array of SystemChar): Boolean; overload;
function CharIn(c: SystemChar; const SomeChar: SystemChar): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function CharIn(c: SystemChar; const s: TPascalString): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function CharIn(c: SystemChar; const p: PPascalString): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function CharIn(c: SystemChar; const SomeCharsets: TOrdChars): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function CharIn(c: SystemChar; const SomeCharset: TOrdChar): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function CharIn(c: SystemChar; const SomeCharsets: TOrdChars; const SomeChars: TPascalString): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function CharIn(c: SystemChar; const SomeCharsets: TOrdChars; const p: PPascalString): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function BytesOfPascalString(const s: TPascalString): TBytes; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function PascalStringOfBytes(const s: TBytes): TPascalString; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function FastHashSystemString(const s: PSystemString): THash; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function FastHash64SystemString(const s: PSystemString): THash64; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function FastHashSystemString(const s: SystemString): THash; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function FastHash64SystemString(const s: SystemString): THash64; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function FastHashPascalString(const s: PPascalString): THash; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function FastHash64PascalString(const s: PPascalString): THash64; {$IFDEF INLINE_ASM} inline; {$ENDIF}

{$IFDEF FPC}

operator := (const s: Variant)r: TPascalString;
operator := (const s: AnsiString)r: TPascalString;
operator := (const s: Unicodestring)r: TPascalString;
operator := (const s: WideString)r: TPascalString;
operator := (const s: ShortString)r: TPascalString;
operator := (const c: SystemChar)r: TPascalString;

operator := (const s: TPascalString)r: AnsiString;
operator := (const s: TPascalString)r: Unicodestring;
operator := (const s: TPascalString)r: WideString;
operator := (const s: TPascalString)r: ShortString;
operator := (const s: TPascalString)r: Variant;

operator = (const A: TPascalString; const B: TPascalString): Boolean;
operator <> (const A: TPascalString; const B: TPascalString): Boolean;
operator > (const A: TPascalString; const B: TPascalString): Boolean;
operator >= (const A: TPascalString; const B: TPascalString): Boolean;
operator < (const A: TPascalString; const B: TPascalString): Boolean;
operator <= (const A: TPascalString; const B: TPascalString): Boolean;

operator + (const A: TPascalString; const B: TPascalString): TPascalString;
operator + (const A: TPascalString; const B: SystemString): TPascalString;
operator + (const A: SystemString; const B: TPascalString): TPascalString;
operator + (const A: TPascalString; const B: SystemChar): TPascalString;
operator + (const A: SystemChar; const B: TPascalString): TPascalString;

{$ENDIF}

{ https://en.wikipedia.org/wiki/Smith%E2%80%93Waterman_algorithm }

// short string likeness and out diff
function SmithWatermanCompare(const seq1, seq2: PPascalString; var diff1, diff2: TPascalString;
  const NoDiffChar: Boolean = False; const diffChar: SystemChar = '-'): Double; overload;
function SmithWatermanCompare(const seq1, seq2: TPascalString; var diff1, diff2: TPascalString;
  const NoDiffChar: Boolean = False; const diffChar: SystemChar = '-'): Double; overload;

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

const
  SystemCharSize = SizeOf(SystemChar);
  {$IFDEF CPU64}
  MaxSmithWatermanMatrix = 10000 * 10;
  {$ELSE}
  MaxSmithWatermanMatrix = 8192;
  {$ENDIF}

implementation

uses CoreClasses, Variants;

const
  {$IFDEF FirstCharInZero}
  FirstCharPos = 0;
  {$ELSE}
  FirstCharPos = 1;
  {$ENDIF}


procedure CombineCharsPP(const c1, c2: TPascalChars; var Output: TPascalChars); {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  ll, rl: Integer;
begin
  ll := Length(c1);
  rl := Length(c2);
  SetLength(Output, ll + rl);
  if ll > 0 then
      CopyPtr(@c1[0], @Output[0], ll * SystemCharSize);
  if rl > 0 then
      CopyPtr(@c2[0], @Output[ll], rl * SystemCharSize);
end;

procedure CombineCharsSP(const c1: SystemString; const c2: TPascalChars; var Output: TPascalChars); {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  ll, rl: Integer;
begin
  ll := Length(c1);
  rl := Length(c2);
  SetLength(Output, ll + rl);
  if ll > 0 then
      CopyPtr(@c1[FirstCharPos], @Output[0], ll * SystemCharSize);
  if rl > 0 then
      CopyPtr(@c2[0], @Output[ll], rl * SystemCharSize);
end;

procedure CombineCharsPS(const c1: TPascalChars; const c2: SystemString; var Output: TPascalChars); {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  ll, rl: Integer;
begin
  ll := Length(c1);
  rl := Length(c2);
  SetLength(Output, ll + rl);
  if ll > 0 then
      CopyPtr(@c1[0], @Output[0], ll * SystemCharSize);
  if rl > 0 then
      CopyPtr(@c2[FirstCharPos], @Output[ll], rl * SystemCharSize);
end;

procedure CombineCharsCP(const c1: SystemChar; const c2: TPascalChars; var Output: TPascalChars); {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  rl: Integer;
begin
  rl := Length(c2);
  SetLength(Output, rl + 1);
  Output[0] := c1;
  if rl > 0 then
      CopyPtr(@c2[0], @Output[1], rl * SystemCharSize);
end;

procedure CombineCharsPC(const c1: TPascalChars; const c2: SystemChar; var Output: TPascalChars); {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  ll: Integer;
begin
  ll := Length(c1);
  SetLength(Output, ll + 1);
  if ll > 0 then
      CopyPtr(@c1[0], @Output[0], ll * SystemCharSize);
  Output[ll] := c2;
end;

function CharIn(c: SystemChar; const SomeChars: array of SystemChar): Boolean;
var
  AChar: SystemChar;
begin
  Result := True;
  for AChar in SomeChars do
    if AChar = c then
        Exit;
  Result := False;
end;

function CharIn(c: SystemChar; const SomeChar: SystemChar): Boolean;
begin
  Result := c = SomeChar;
end;

function CharIn(c: SystemChar; const s: TPascalString): Boolean;
begin
  Result := s.Exists(c);
end;

function CharIn(c: SystemChar; const p: PPascalString): Boolean;
begin
  Result := p^.Exists(c);
end;

function CharIn(c: SystemChar; const SomeCharset: TOrdChar): Boolean;
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

function CharIn(c: SystemChar; const SomeCharsets: TOrdChars): Boolean;
var
  i: TOrdChar;
begin
  Result := True;
  for i in SomeCharsets do
    if CharIn(c, i) then
        Exit;
  Result := False;
end;

function CharIn(c: SystemChar; const SomeCharsets: TOrdChars; const SomeChars: TPascalString): Boolean;
begin
  if CharIn(c, SomeCharsets) then
      Result := True
  else
      Result := CharIn(c, SomeChars);
end;

function CharIn(c: SystemChar; const SomeCharsets: TOrdChars; const p: PPascalString): Boolean;
begin
  if CharIn(c, SomeCharsets) then
      Result := True
  else
      Result := CharIn(c, p);
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
  c: SystemChar;
begin
  Result := 0;

  {$IFDEF FirstCharInZero}
  for i := 0 to Length(s^) - 1 do
  {$ELSE}
  for i := 1 to Length(s^) do
    {$ENDIF}
    begin
      c := s^[i];
      if CharIn(c, cHiAtoZ) then
          inc(c, 32);
      Result := ((Result shl 7) or (Result shr 25)) + Cardinal(c);
    end;
end;

function FastHash64SystemString(const s: PSystemString): THash64;
var
  i: Integer;
  c: SystemChar;
begin
  Result := 0;

  {$IFDEF FirstCharInZero}
  for i := 0 to Length(s^) - 1 do
  {$ELSE}
  for i := 1 to Length(s^) do
    {$ENDIF}
    begin
      c := s^[i];
      if CharIn(c, cHiAtoZ) then
          inc(c, 32);
      Result := ((Result shl 7) or (Result shr 57)) + Cardinal(c);
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
  c: SystemChar;
begin
  Result := 0;
  for i := 1 to s^.Len do
    begin
      c := s^[i];
      if CharIn(c, cHiAtoZ) then
          inc(c, 32);
      Result := ((Result shl 7) or (Result shr 25)) + Cardinal(c);
    end;
end;

function FastHash64PascalString(const s: PPascalString): THash64;
var
  i: Integer;
  c: SystemChar;
begin
  Result := 0;
  for i := 1 to s^.Len do
    begin
      c := s^[i];
      if CharIn(c, cHiAtoZ) then
          inc(c, 32);
      Result := ((Result shl 7) or (Result shr 57)) + Cardinal(c);
    end;
end;

function GetSWMVMemory(const xLen, yLen: NativeUInt): Pointer; inline;
{ optimized matrix performance }
begin
  Result := System.AllocMem((xLen + 1) * (yLen + 1) * SizeOf(NativeInt));
end;

function GetSWMV(const p: Pointer; const w, x, y: NativeUInt): NativeInt; inline;
{ optimized matrix performance }
begin
  Result := PNativeInt(NativeUInt(p) + ((x + y * (w + 1)) * SizeOf(NativeInt)))^;
end;

procedure SetSWMV(const p: Pointer; const w, x, y: NativeUInt; const v: NativeInt); inline;
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

function SmithWatermanCompare(const seq1, seq2: PPascalString; var diff1, diff2: TPascalString;
  const NoDiffChar: Boolean; const diffChar: SystemChar): Double;

  function InlineMatch(alphaC, betaC: SystemChar; const diffC: SystemChar): Integer; inline;
  begin
    if CharIn(alphaC, cLoAtoZ) then
        dec(alphaC, 32);
    if CharIn(betaC, cLoAtoZ) then
        dec(betaC, 32);

    if alphaC = betaC then
        Result := SmithWaterman_MatchOk
    else if (alphaC = diffC) or (betaC = diffC) then
        Result := gap_penalty
    else
        Result := mismatch_penalty;
  end;

var
  swMatrixPtr                                           : Pointer;
  i, j, l1, l2                                          : NativeUInt;
  matched, deleted, inserted                            : NativeInt;
  score_current, score_diagonal, score_left, score_right: NativeInt;
  identity                                              : NativeUInt;
  align1, align2                                        : TPascalString;
begin
  l1 := seq1^.Len;
  l2 := seq2^.Len;

  if (l1 = 0) or (l2 = 0) or (l1 > MaxSmithWatermanMatrix) or (l2 > MaxSmithWatermanMatrix) then
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

function SmithWatermanCompare(const seq1, seq2: TPascalString; var diff1, diff2: TPascalString;
  const NoDiffChar: Boolean; const diffChar: SystemChar): Double;
begin
  Result := SmithWatermanCompare(@seq1, @seq2, diff1, diff2, NoDiffChar, diffChar);
end;

function SmithWatermanCompare(const seq1, seq2: PPascalString; out Same, Diff: Integer): Double;

  function InlineMatch(alphaC, betaC: SystemChar): NativeInt; inline;
  begin
    if CharIn(alphaC, cLoAtoZ) then
        dec(alphaC, 32);
    if CharIn(betaC, cLoAtoZ) then
        dec(betaC, 32);

    if alphaC = betaC then
        Result := SmithWaterman_MatchOk
    else
        Result := mismatch_penalty;
  end;

var
  swMatrixPtr                                           : Pointer;
  i, j, l1, l2                                          : NativeUInt;
  matched, deleted, inserted                            : NativeInt;
  score_current, score_diagonal, score_left, score_right: NativeInt;
  identity, l                                           : NativeUInt;
begin
  l1 := seq1^.Len;
  l2 := seq2^.Len;

  if (l1 = 0) or (l2 = 0) or (l1 > MaxSmithWatermanMatrix) or (l2 > MaxSmithWatermanMatrix) then
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
  r: Double;
begin
  r := -1;
  Result := -1;
  for i := 0 to Length(seq1) - 1 do
    begin
      r := SmithWatermanCompare(seq1[i], seq2);
      if r > Result then
          Result := r;
    end;
end;

function SmithWatermanCompare(const seq1: Pointer; siz1: Integer; const seq2: Pointer; siz2: Integer;
  out Same, Diff: Integer): Double;

  function InlineMatch(const alphaB, betaB: Byte): NativeInt; inline;
  begin
    if alphaB = betaB then
        Result := SmithWaterman_MatchOk
    else
        Result := mismatch_penalty;
  end;

var
  swMatrixPtr                                           : Pointer;
  i, j, l1, l2                                          : NativeUInt;
  matched, deleted, inserted                            : NativeInt;
  score_current, score_diagonal, score_left, score_right: NativeInt;
  identity, l                                           : NativeUInt;
begin
  l1 := siz1;
  l2 := siz2;

  if (l1 = 0) or (l2 = 0) or (l1 > MaxSmithWatermanMatrix) or (l2 > MaxSmithWatermanMatrix) then
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
    l, i: Integer;
    n   : TPascalString;
    p   : PSRec;
  begin
    l := psPtr^.Len;
    i := 1;
    n := '';
    while i <= l do
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
                inc(i);
            until (i > l) or (not CharIn(psPtr^[i], [#13, #10, #32, #9]));
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
    for i := 0 to lst1.count - 1 do
        dispose(PSRec(lst1[i]));
    for i := 0 to lst2.count - 1 do
        dispose(PSRec(lst2[i]));
    disposeObject([lst1, lst2]);
  end;

var
  swMatrixPtr                                           : Pointer;
  i, j, l1, l2                                          : NativeUInt;
  matched, deleted, inserted                            : NativeInt;
  score_current, score_diagonal, score_left, score_right: NativeInt;
  cSame, cDiff, TotalSame, TotalDiff                    : Integer;
begin
  _Init;
  l1 := lst1.count;
  l2 := lst2.count;

  if (l1 = 0) or (l2 = 0) or (l1 > MaxSmithWatermanMatrix) or (l2 > MaxSmithWatermanMatrix) then
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

function SmithWatermanCompareLongString(const t1, t2: TPascalString): Double;
var
  Same, Diff: Integer;
begin
  Result := SmithWatermanCompareLongString(t1, t2, 5, Same, Diff);
end;

{$IFDEF FPC}


operator := (const s: Variant)r: TPascalString;
begin
  r.Text := s;
end;

operator := (const s: AnsiString)r: TPascalString;
begin
  r.Text := s;
end;

operator := (const s: Unicodestring)r: TPascalString;
begin
  r.Text := s;
end;

operator := (const s: WideString)r: TPascalString;
begin
  r.Text := s;
end;

operator := (const s: ShortString)r: TPascalString;
begin
  r.Text := s;
end;

operator := (const c: SystemChar)r: TPascalString;
begin
  r.Text := c;
end;

operator := (const s: TPascalString)r: AnsiString;
begin
  r := s.Text;
end;

operator := (const s: TPascalString)r: Unicodestring;
begin
  r := s.Text;
end;

operator := (const s: TPascalString)r: WideString;
begin
  r := s.Text;
end;

operator := (const s: TPascalString)r: ShortString;
begin
  r := s.Text;
end;

operator := (const s: TPascalString)r: Variant;
begin
  r := s.Text;
end;

operator = (const A: TPascalString; const B: TPascalString): Boolean;
begin
  Result := A.Text = B.Text;
end;

operator <> (const A: TPascalString; const B: TPascalString): Boolean;
begin
  Result := A.Text <> B.Text;
end;

operator > (const A: TPascalString; const B: TPascalString): Boolean;
begin
  Result := A.Text > B.Text;
end;

operator >= (const A: TPascalString; const B: TPascalString): Boolean;
begin
  Result := A.Text >= B.Text;
end;

operator < (const A: TPascalString; const B: TPascalString): Boolean;
begin
  Result := A.Text < B.Text;
end;

operator <= (const A: TPascalString; const B: TPascalString): Boolean;
begin
  Result := A.Text <= B.Text;
end;

operator + (const A: TPascalString; const B: TPascalString): TPascalString;
begin
  CombineCharsPP(A.Buff, B.Buff, Result.Buff);
end;

operator + (const A: TPascalString; const B: SystemString): TPascalString;
begin
  CombineCharsPS(A.Buff, B, Result.Buff);
end;

operator + (const A: SystemString; const B: TPascalString): TPascalString;
begin
  CombineCharsSP(A, B.Buff, Result.Buff);
end;

operator + (const A: TPascalString; const B: SystemChar): TPascalString;
begin
  CombineCharsPC(A.Buff, B, Result.Buff);
end;

operator + (const A: SystemChar; const B: TPascalString): TPascalString;
begin
  CombineCharsCP(A, B.Buff, Result.Buff);
end;

{$ENDIF}


function TPascalString.GetText: SystemString;
begin
  SetLength(Result, Length(Buff));
  CopyPtr(@Buff[0], @Result[FirstCharPos], Length(Buff) * SystemCharSize);
end;

procedure TPascalString.SetText(const Value: SystemString);
begin
  SetLength(Buff, Length(Value));
  CopyPtr(@Value[FirstCharPos], @Buff[0], Length(Buff) * SystemCharSize);
end;

function TPascalString.GetLen: Integer;
begin
  Result := Length(Buff);
end;

procedure TPascalString.SetLen(const Value: Integer);
begin
  SetLength(Buff, Value);
end;

function TPascalString.GetChars(index: Integer): SystemChar;
begin
  Result := Buff[index - 1];
end;

procedure TPascalString.SetChars(index: Integer; const Value: SystemChar);
begin
  Buff[index - 1] := Value;
end;

procedure TPascalString.SetBytes(const Value: TBytes);
begin
  SetLength(Buff, 0);
  try
      Text := StringOf(SysUtils.TEncoding.Convert(SysUtils.TEncoding.UTF8, SysUtils.TEncoding.Default, Value));
  except
      SetLength(Buff, 0);
  end;
end;

function TPascalString.GetBytes: TBytes;
begin
  {$IFDEF FPC}
  Result := SysUtils.TEncoding.UTF8.GetBytes(Text);
  {$ELSE}
  Result := SysUtils.TEncoding.UTF8.GetBytes(Buff);
  {$ENDIF}
end;

function TPascalString.GetLast: SystemChar;
begin
  Result := Buff[high(Buff)];
end;

procedure TPascalString.SetLast(const Value: SystemChar);
begin
  Buff[high(Buff)] := Value;
end;

function TPascalString.GetFirst: SystemChar;
begin
  Result := Buff[0];
end;

procedure TPascalString.SetFirst(const Value: SystemChar);
begin
  Buff[0] := Value;
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
  CombineCharsPP(Lhs.Buff, Rhs.Buff, Result.Buff);
end;

class operator TPascalString.Add(const Lhs: SystemString; const Rhs: TPascalString): TPascalString;
begin
  CombineCharsSP(Lhs, Rhs.Buff, Result.Buff);
end;

class operator TPascalString.Add(const Lhs: TPascalString; const Rhs: SystemString): TPascalString;
begin
  CombineCharsPS(Lhs.Buff, Rhs, Result.Buff);
end;

class operator TPascalString.Add(const Lhs: SystemChar; const Rhs: TPascalString): TPascalString;
begin
  CombineCharsCP(Lhs, Rhs.Buff, Result.Buff);
end;

class operator TPascalString.Add(const Lhs: TPascalString; const Rhs: SystemChar): TPascalString;
begin
  CombineCharsPC(Lhs.Buff, Rhs, Result.Buff);
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
  Result.Buff[0] := Value;
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
  Result.Buff[0] := Value;
end;

{$ENDIF}


function TPascalString.copy(index, count: NativeInt): TPascalString;
var
  l: NativeInt;
begin
  l := Length(Buff);

  if (index - 1) + count > l then
      count := l - (index - 1);

  SetLength(Result.Buff, count);
  CopyPtr(@Buff[index - 1], @Result.Buff[0], SystemCharSize * count);

  // Result.Buff := System.copy(Buff, index - 1, count);
end;

function TPascalString.Same(const p: PPascalString): Boolean;
var
  i   : Integer;
  s, d: SystemChar;
begin
  Result := (p^.Len = Len);
  if not Result then
      Exit;
  for i := 0 to Len - 1 do
    begin
      s := Buff[i];
      if CharIn(s, cHiAtoZ) then
          inc(s, 32);
      d := p^.Buff[i];
      if CharIn(d, cHiAtoZ) then
          inc(d, 32);
      if s <> d then
          Exit(False);
    end;
end;

function TPascalString.Same(const t: TPascalString): Boolean;
var
  i   : Integer;
  s, d: SystemChar;
begin
  Result := (t.Len = Len);
  if not Result then
      Exit;
  for i := 0 to Len - 1 do
    begin
      s := Buff[i];
      if CharIn(s, cHiAtoZ) then
          inc(s, 32);
      d := t.Buff[i];
      if CharIn(d, cHiAtoZ) then
          inc(d, 32);
      if s <> d then
          Exit(False);
    end;
end;

function TPascalString.Same(const IgnoreCase: Boolean; const t: TPascalString): Boolean;
var
  i   : Integer;
  s, d: SystemChar;
begin
  Result := (t.Len = Len);
  if not Result then
      Exit;
  for i := 0 to Len - 1 do
    begin

      s := Buff[i];
      if IgnoreCase then
        if CharIn(s, cHiAtoZ) then
            inc(s, 32);

      d := t.Buff[i];
      if IgnoreCase then
        if CharIn(d, cHiAtoZ) then
            inc(d, 32);

      if s <> d then
          Exit(False);
    end;
end;

function TPascalString.ComparePos(const Offset: Integer; const p: PPascalString): Boolean;
var
  i, l              : Integer;
  sourChar, destChar: SystemChar;
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

      if CharIn(sourChar, cLoAtoZ) then
          dec(sourChar, 32);
      if CharIn(destChar, cLoAtoZ) then
          dec(destChar, 32);

      if sourChar <> destChar then
          Exit;
      inc(i);
    end;
  Result := True;
end;

function TPascalString.ComparePos(const Offset: Integer; const t: TPascalString): Boolean;
var
  i, l              : Integer;
  sourChar, destChar: SystemChar;
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

      if CharIn(sourChar, cLoAtoZ) then
          dec(sourChar, 32);
      if CharIn(destChar, cLoAtoZ) then
          dec(destChar, 32);

      if sourChar <> destChar then
          Exit;
      inc(i);
    end;
  Result := True;
end;

function TPascalString.GetPos(const SubStr: TPascalString; const Offset: Integer = 1): Integer;
var
  i: Integer;
begin
  Result := 0;
  if SubStr.Len > 0 then
    for i := Offset to Len - SubStr.Len + 1 do
      if ComparePos(i, @SubStr) then
          Exit(i);
end;

function TPascalString.GetPos(const SubStr: PPascalString; const Offset: Integer = 1): Integer;
var
  i: Integer;
begin
  Result := 0;
  if SubStr^.Len > 0 then
    for i := Offset to Len - SubStr^.Len + 1 do
      if ComparePos(i, SubStr) then
          Exit(i);
end;

function TPascalString.Exists(c: SystemChar): Boolean;
var
  i: Integer;
begin
  for i := low(Buff) to high(Buff) do
    if Buff[i] = c then
        Exit(True);
  Result := False;
end;

function TPascalString.Exists(c: array of SystemChar): Boolean;
var
  i: Integer;
begin
  for i := low(Buff) to high(Buff) do
    if CharIn(Buff[i], c) then
        Exit(True);
  Result := False;
end;

function TPascalString.Exists(const SubStr: TPascalString): Boolean;
begin
  Result := GetPos(@SubStr, 1) > 0;
end;

function TPascalString.Hash: THash;
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
      SetLength(Buff, Length(Buff) - 1);
end;

procedure TPascalString.DeleteFirst;
begin
  if Len > 0 then
      Buff := System.copy(Buff, 1, Len);
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
  SetLength(Buff, 0);
end;

procedure TPascalString.Append(t: TPascalString);
var
  r, l: Integer;
begin
  l := Length(t.Buff);
  if l > 0 then
    begin
      r := Length(Buff);
      SetLength(Buff, r + l);
      CopyPtr(@t.Buff[0], @Buff[r], l * SystemCharSize);
    end;
end;

procedure TPascalString.Append(c: SystemChar);
begin
  SetLength(Buff, Length(Buff) + 1);
  Buff[Length(Buff) - 1] := c;
end;

function TPascalString.GetString(bPos, ePos: NativeInt): TPascalString;
begin
  if ePos > Length(Buff) then
      Result.Text := Self.copy(bPos, Length(Buff) - bPos + 1)
  else
      Result.Text := Self.copy(bPos, (ePos - bPos));
end;

procedure TPascalString.Insert(AText: SystemString; idx: Integer);
begin
  Text := GetString(1, idx) + AText + GetString(idx + 1, Len);
end;

procedure TPascalString.FastAsText(var Output: SystemString);
begin
  SetLength(Output, Length(Buff));
  CopyPtr(@Buff[0], @Output[FirstCharPos], Length(Buff) * SystemCharSize);
end;

procedure TPascalString.FastGetBytes(var Output: TBytes);
begin
  {$IFDEF FPC}
  Output := SysUtils.TEncoding.UTF8.GetBytes(Text);
  {$ELSE}
  Output := SysUtils.TEncoding.UTF8.GetBytes(Buff);
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

function TPascalString.TrimChar(const charS: TPascalString): TPascalString;
var
  l, bp, ep: Integer;
begin
  Result := '';
  l := Len;
  if l > 0 then
    begin
      bp := 1;
      while CharIn(GetChars(bp), @charS) do
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

          while CharIn(GetChars(ep), @charS) do
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

function TPascalString.DeleteChar(const charS: TPascalString): TPascalString;
var
  i: Integer;
  c: SystemChar;
begin
  Result := '';
  for c in Buff do
    if not CharIn(c, @charS) then
        Result.Append(c);
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
