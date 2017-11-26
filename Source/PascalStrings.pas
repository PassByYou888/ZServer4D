{ ***************************************************************************** }
{ * string support,writen by QQ 600585@qq.com                                 * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
(* https://github.com/PassByYou888/ZServer4D *)
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
  {$IFDEF FPC}
  SystemChar = UnicodeChar;
  SystemString = Unicodestring;
  {$ELSE}
  SystemString = string;
  SystemChar = char;
  {$ENDIF}
  THash         = Cardinal;
  THash64       = UInt64;
  PSystemString = ^SystemString;

  PPascalString = ^TPascalString;

  TPascalString = packed record
  private
    function GetText: SystemString;
    procedure SetText(const Value: SystemString);
    function GetLen: Integer;
    procedure SetLen(const Value: Integer);
    function GetItems(index: Integer): SystemChar;
    procedure SetItems(index: Integer; const Value: SystemChar);
    function GetBytes: TBytes;
    procedure SetBytes(const Value: TBytes);
    function GetLast: SystemChar;
    procedure SetLast(const Value: SystemChar);
    function GetFirst: SystemChar;
    procedure SetFirst(const Value: SystemChar);
  public
    Buff: array of SystemChar;

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
    function copy(index, count: Integer): TPascalString; inline;
    function Same(const t: TPascalString): Boolean; overload; inline;
    function Same(const IgnoreCase: Boolean; const t: TPascalString): Boolean; overload; inline;
    function ComparePos(Offset: Integer; const t: PPascalString): Boolean; overload; inline;
    function ComparePos(Offset: Integer; const t: TPascalString): Boolean; overload; inline;
    function GetPos(const SubStr: TPascalString; const Offset: Integer = 1): Integer; inline;
    function Exists(c: SystemChar): Boolean; overload;

    property Last: SystemChar read GetLast write SetLast;
    property First: SystemChar read GetFirst write SetFirst;

    procedure DeleteLast; inline;
    procedure DeleteFirst; inline;
    procedure Delete(idx, cnt: Integer); inline;
    procedure Clear; inline;

    procedure Append(t: TPascalString); inline;
    function GetString(bPos, ePos: Integer): TPascalString; inline;
    procedure Insert(AText: SystemString; idx: Integer); inline;

    procedure AsText(var Output: SystemString); inline;
    procedure FastGetBytes(var Output: TBytes); inline;

    property Text: SystemString read GetText write SetText;
    function LowerText: SystemString;
    function UpperText: SystemString;
    property len: Integer read GetLen write SetLen;
    property Items[index: Integer]: SystemChar read GetItems write SetItems; default;
    property Bytes: TBytes read GetBytes write SetBytes;
    function BOMBytes: TBytes;
  end;

  TOrdChar  = (c0to9, c1to9, c0to32, c0to32no10, cLoAtoF, cHiAtoF, cLoAtoZ, cHiAtoZ, cHex, cAtoF, cAtoZ);
  TOrdChars = set of TOrdChar;

function CharIn(c: SystemChar; const SomeChars: array of SystemChar): Boolean; overload;
function CharIn(c: SystemChar; const SomeChars: TPascalString): Boolean; overload;
function CharIn(c: SystemChar; const SomeCharsets: TOrdChars): Boolean; overload;
function CharIn(c: SystemChar; const SomeCharset: TOrdChar): Boolean; overload;
function CharIn(c: SystemChar; const SomeCharsets: TOrdChars; const SomeChars: TPascalString): Boolean; overload;

function BytesOfPascalString(var s: TPascalString): TBytes; overload; inline;
function PascalStringOfBytes(var s: TBytes): TPascalString; overload; inline;

function FastHashSystemString(s: PSystemString): THash; overload; inline;
function FastHash64SystemString(s: PSystemString): THash64; overload; inline;

function FastHashSystemString(s: SystemString): THash; overload; inline;
function FastHash64SystemString(s: SystemString): THash64; overload; inline;

function FastHashPascalString(s: PPascalString): THash; inline;
function FastHash64PascalString(s: PPascalString): THash64; inline;

{$IFDEF FPC}

operator := (const s: Variant)r: TPascalString;

operator := (const s: AnsiString)r: TPascalString;
operator := (const s: UnicodeString)r: TPascalString;
operator := (const s: WideString)r: TPascalString;
operator := (const s: ShortString)r: TPascalString;

operator := (const c: SystemChar)r: TPascalString;

operator := (const s: TPascalString)r: AnsiString;
operator := (const s: TPascalString)r: UnicodeString;
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
{$ENDIF}

implementation

uses Variants;

function CharIn(c: SystemChar; const SomeChars: array of SystemChar): Boolean;
var
  AChar: SystemChar;
begin
  for AChar in SomeChars do
    if AChar = c then
        Exit(True);
  Result := False;
end;

function CharIn(c: SystemChar; const SomeChars: TPascalString): Boolean;
var
  i: Integer;
begin
  for i := 1 to SomeChars.len do
    if SomeChars[i] = c then
        Exit(True);
  Result := False;
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
  Result := False;
  for i in SomeCharsets do
    if CharIn(c, i) then
        Exit(True);
end;

function CharIn(c: SystemChar; const SomeCharsets: TOrdChars; const SomeChars: TPascalString): Boolean;
begin
  if CharIn(c, SomeCharsets) then
      Result := True
  else
      Result := CharIn(c, SomeChars);
end;

function BytesOfPascalString(var s: TPascalString): TBytes;
begin
  Result := s.Bytes;
end;

function PascalStringOfBytes(var s: TBytes): TPascalString;
begin
  Result.Bytes := s;
end;

function FastHashSystemString(s: PSystemString): THash;
const
  A = ord('A');
  Z = ord('Z');
var
  i: Integer;
  c: Cardinal;
begin
  Result := 0;

  {$IFDEF FirstCharInZero}
  for i := 0 to Length(s^) - 1 do
  {$ELSE}
  for i := 1 to Length(s^) do
    {$ENDIF}
    begin
      c := ord(s^[i]);
      if (c >= A) and (c <= Z) then
          c := c + $20;
      Result := ((Result shl 7) or (Result shr 25)) + c;
    end;
end;

function FastHash64SystemString(s: PSystemString): THash64;
const
  A = ord('A');
  Z = ord('Z');
var
  i: Integer;
  c: Cardinal;
begin
  Result := 0;

  {$IFDEF FirstCharInZero}
  for i := 0 to Length(s^) - 1 do
  {$ELSE}
  for i := 1 to Length(s^) do
    {$ENDIF}
    begin
      c := ord(s^[i]);
      if (c >= A) and (c <= Z) then
          c := c + $20;
      Result := ((Result shl 7) or (Result shr 25)) + c;
    end;
end;

function FastHashSystemString(s: SystemString): THash;
begin
  Result := FastHashSystemString(@s);
end;

function FastHash64SystemString(s: SystemString): THash64;
begin
  Result := FastHash64SystemString(@s);
end;

function FastHashPascalString(s: PPascalString): THash;
const
  A = ord('A');
  Z = ord('Z');
var
  i: Integer;
  c: Cardinal;
begin
  Result := 0;

  for i := 1 to s^.len do
    begin
      c := ord(s^[i]);
      if (c >= A) and (c <= Z) then
          c := c + $20;
      Result := ((Result shl 7) or (Result shr 25)) + c;
    end;
end;

function FastHash64PascalString(s: PPascalString): THash64;
const
  A = ord('A');
  Z = ord('Z');
var
  i: Integer;
  c: Cardinal;
begin
  Result := 0;

  for i := 1 to s^.len do
    begin
      c := ord(s^[i]);
      if (c >= A) and (c <= Z) then
          c := c + $20;
      Result := ((Result shl 7) or (Result shr 25)) + c;
    end;
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

operator := (const s: UnicodeString)r: TPascalString;
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

operator := (const s: TPascalString)r: UnicodeString;
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
  Result.Text := A.Text + B.Text;
end;

operator + (const A: TPascalString; const B: SystemString): TPascalString;
begin
  Result.Text := A.Text + B;
end;

operator + (const A: SystemString; const B: TPascalString): TPascalString;
begin
  Result.Text := A + B.Text;
end;

{$ENDIF}


function TPascalString.GetText: SystemString;
var
  i: Integer;
begin
  SetLength(Result, len);
  for i := 0 to len - 1 do
    begin
      {$IFDEF FirstCharInZero}
      Result[i] := Buff[i];
      {$ELSE}
      Result[i + 1] := Buff[i];
      {$ENDIF}
    end;
end;

procedure TPascalString.SetText(const Value: SystemString);
var
  i: Integer;
begin
  SetLength(Buff, Length(Value));

  {$IFDEF FirstCharInZero}
  for i := 0 to Length(Value) - 1 do
    try
        Buff[i] := Value[i];
    except
    end;
  {$ELSE}
  for i := 1 to Length(Value) do
    try
        Buff[i - 1] := Value[i];
    except
    end;
  {$ENDIF}
end;

function TPascalString.GetLen: Integer;
begin
  Result := Length(Buff);
end;

procedure TPascalString.SetLen(const Value: Integer);
begin
  SetLength(Buff, Value);
end;

function TPascalString.GetItems(index: Integer): SystemChar;
begin
  Result := Buff[index - 1];
end;

procedure TPascalString.SetItems(index: Integer; const Value: SystemChar);
begin
  Buff[index - 1] := Value;
end;

procedure TPascalString.SetBytes(const Value: TBytes);
var
  i: Integer;
begin
  SetLength(Buff, 0);
  try
      Text := StringOf(SysUtils.TEncoding.Convert(SysUtils.TEncoding.UTF8, SysUtils.TEncoding.Default, Value));
  except
    SetLength(Buff, Length(Value));
    for i := 0 to Length(Value) - 1 do
        Buff[i] := char(Value[i]);
  end;
end;

function TPascalString.GetBytes: TBytes;
begin
  Result := SysUtils.TEncoding.UTF8.GetBytes(Buff);
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
  Result := (Lhs.len = Rhs.len) and (Lhs.Text = Rhs.Text);
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
  Result.Buff := Lhs.Buff + Rhs.Buff;
end;

class operator TPascalString.Add(const Lhs: SystemString; const Rhs: TPascalString): TPascalString;
begin
  Result.Buff := TPascalString(Lhs).Buff + Rhs.Buff;
end;

class operator TPascalString.Add(const Lhs: TPascalString; const Rhs: SystemString): TPascalString;
begin
  Result.Buff := Lhs.Buff + TPascalString(Rhs).Buff;
end;

class operator TPascalString.Add(const Lhs: SystemChar; const Rhs: TPascalString): TPascalString;
begin
  Result.Buff := [Lhs] + Rhs.Buff;
end;

class operator TPascalString.Add(const Lhs: TPascalString; const Rhs: SystemChar): TPascalString;
begin
  Result.Buff := Lhs.Buff + [Rhs];
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
  Result.len := 1;
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
  Result.len := 1;
  Result.Buff[0] := Value;
end;

{$ENDIF}


function TPascalString.copy(index, count: Integer): TPascalString;
begin
  Result.Buff := System.copy(Buff, index - 1, count);
end;

function TPascalString.Same(const t: TPascalString): Boolean;
var
  i   : Integer;
  s, d: SystemChar;
begin
  Result := (t.len = len);
  if not Result then
      Exit;
  for i := 0 to len - 1 do
    begin
      s := Buff[i];
      if (s >= 'A') and (s <= 'Z') then
          s := SystemChar(ord(s) + $20);
      d := t.Buff[i];
      if (d >= 'A') and (d <= 'Z') then
          d := SystemChar(ord(d) + $20);
      if s <> d then
          Exit(False);
    end;
end;

function TPascalString.Same(const IgnoreCase: Boolean; const t: TPascalString): Boolean;
var
  i   : Integer;
  s, d: SystemChar;
begin
  Result := (t.len = len);
  if not Result then
      Exit;
  for i := 0 to len - 1 do
    begin

      s := Buff[i];
      if IgnoreCase then
        if (s >= 'A') and (s <= 'Z') then
            s := SystemChar(ord(s) + $20);

      d := t.Buff[i];
      if IgnoreCase then
        if (d >= 'A') and (d <= 'Z') then
            d := SystemChar(ord(d) + $20);

      if s <> d then
          Exit(False);
    end;
end;

function TPascalString.ComparePos(Offset: Integer; const t: PPascalString): Boolean;
var
  i, l              : Integer;
  sourChar, destChar: SystemChar;
begin
  Result := False;
  i := 1;
  l := t^.len;
  if (Offset + l - 1) > len then
      Exit;
  while i <= l do
    begin
      sourChar := Items[Offset + i - 1];
      destChar := t^[i];

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

function TPascalString.ComparePos(Offset: Integer; const t: TPascalString): Boolean;
var
  i, l              : Integer;
  sourChar, destChar: SystemChar;
begin
  Result := False;
  i := 1;
  l := t.len;
  if (Offset + l) > len then
      Exit;
  while i <= l do
    begin
      sourChar := Items[Offset + i - 1];
      destChar := t[i];

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

function TPascalString.GetPos(const SubStr: TPascalString; const Offset: Integer = 1): Integer;
var
  i: Integer;
begin
  Result := 0;
  if SubStr.len > 0 then
    for i := Offset to len - SubStr.len + 1 do
      if ComparePos(i, @SubStr) then
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

procedure TPascalString.DeleteLast;
begin
  if len > 0 then
      Buff := System.copy(Buff, 0, len - 1);
end;

procedure TPascalString.DeleteFirst;
begin
  if len > 0 then
      Buff := System.copy(Buff, 1, len);
end;

procedure TPascalString.Delete(idx, cnt: Integer);
begin
  if (idx + cnt <= len) then
      Text := GetString(1, idx) + GetString(idx + cnt, len)
  else
      Text := GetString(1, idx);
end;

procedure TPascalString.Clear;
begin
  SetLength(Buff, 0);
end;

procedure TPascalString.Append(t: TPascalString);
begin
  {$IFDEF FPC}
  Text := Text + t.Text;
  {$ELSE}
  Buff := Buff + t.Buff;
  {$ENDIF}
end;

function TPascalString.GetString(bPos, ePos: Integer): TPascalString;
begin
  Result.Text := Self.copy(bPos, ePos - bPos);
end;

procedure TPascalString.Insert(AText: SystemString; idx: Integer);
begin
  Text := GetString(1, idx) + AText + GetString(idx + 1, len);
end;

procedure TPascalString.AsText(var Output: SystemString);
var
  i: Integer;
begin
  SetLength(Output, len);
  for i := 0 to len - 1 do
    begin
      {$IFDEF FirstCharInZero}
      Output[i] := Buff[i];
      {$ELSE}
      Output[i + 1] := Buff[i];
      {$ENDIF}
    end;
end;

procedure TPascalString.FastGetBytes(var Output: TBytes);
begin
  Output := SysUtils.TEncoding.UTF8.GetBytes(Buff);
end;

function TPascalString.LowerText: SystemString;
begin
  Result := LowerCase(Text);
end;

function TPascalString.UpperText: SystemString;
begin
  Result := UpperCase(Text);
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

end.
