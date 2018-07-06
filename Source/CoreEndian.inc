function Endian(const AValue: SmallInt): SmallInt; overload; inline;
begin
  { the extra Word type cast is necessary because the "AValue shr 8" }
  { is turned into "Integer(AValue) shr 8", so if AValue < 0 then }
  { the sign bits from the upper 16 bits are shifted in rather than }
  { zeroes. }
  Result := SmallInt((Word(AValue) shr 8) or (Word(AValue) shl 8));
end;

function Endian(const AValue: Word): Word; overload; inline;
begin
  Result := Word((AValue shr 8) or (AValue shl 8));
end;

function Endian(const AValue: Integer): Integer; overload; inline;
begin
  Result := ((Cardinal(AValue) shl 8) and $FF00FF00) or ((Cardinal(AValue) shr 8) and $00FF00FF);
  Result := (Cardinal(Result) shl 16) or (Cardinal(Result) shr 16);
end;

function Endian(const AValue: Cardinal): Cardinal; overload; inline;
begin
  Result := ((AValue shl 8) and $FF00FF00) or ((AValue shr 8) and $00FF00FF);
  Result := (Result shl 16) or (Result shr 16);
end;

function Endian(const AValue: Int64): Int64; overload; inline;
begin
  Result := ((UInt64(AValue) shl 8) and $FF00FF00FF00FF00) or ((UInt64(AValue) shr 8) and $00FF00FF00FF00FF);
  Result := ((UInt64(Result) shl 16) and $FFFF0000FFFF0000) or ((UInt64(Result) shr 16) and $0000FFFF0000FFFF);
  Result := (UInt64(Result) shl 32) or ((UInt64(Result) shr 32));
end;

function Endian(const AValue: UInt64): UInt64; overload; inline;
begin
  Result := ((AValue shl 8) and $FF00FF00FF00FF00) or ((AValue shr 8) and $00FF00FF00FF00FF);
  Result := ((Result shl 16) and $FFFF0000FFFF0000) or ((Result shr 16) and $0000FFFF0000FFFF);
  Result := (Result shl 32) or ((Result shr 32));
end;

function BE2N(const AValue: SmallInt): SmallInt; overload; inline;
begin
{$IFDEF BIG_ENDIAN}
  Result := AValue;
{$ELSE}
  Result := Endian(AValue);
{$ENDIF}
end;

function BE2N(const AValue: Word): Word; overload; inline;
begin
{$IFDEF BIG_ENDIAN}
  Result := AValue;
{$ELSE}
  Result := Endian(AValue);
{$ENDIF}
end;

function BE2N(const AValue: Integer): Integer; overload; inline;
begin
{$IFDEF BIG_ENDIAN}
  Result := AValue;
{$ELSE}
  Result := Endian(AValue);
{$ENDIF}
end;

function BE2N(const AValue: Cardinal): Cardinal; overload; inline;
begin
{$IFDEF BIG_ENDIAN}
  Result := AValue;
{$ELSE}
  Result := Endian(AValue);
{$ENDIF}
end;

function BE2N(const AValue: Int64): Int64; overload; inline;
begin
{$IFDEF BIG_ENDIAN}
  Result := AValue;
{$ELSE}
  Result := Endian(AValue);
{$ENDIF}
end;

function BE2N(const AValue: UInt64): UInt64; overload; inline;
begin
{$IFDEF BIG_ENDIAN}
  Result := AValue;
{$ELSE}
  Result := Endian(AValue);
{$ENDIF}
end;

function LE2N(const AValue: SmallInt): SmallInt; overload; inline;
begin
{$IFDEF LITTLE_ENDIAN}
  Result := AValue;
{$ELSE}
  Result := Endian(AValue);
{$ENDIF}
end;

function LE2N(const AValue: Word): Word; overload; inline;
begin
{$IFDEF LITTLE_ENDIAN}
  Result := AValue;
{$ELSE}
  Result := Endian(AValue);
{$ENDIF}
end;

function LE2N(const AValue: Integer): Integer; overload; inline;
begin
{$IFDEF LITTLE_ENDIAN}
  Result := AValue;
{$ELSE}
  Result := Endian(AValue);
{$ENDIF}
end;

function LE2N(const AValue: Cardinal): Cardinal; overload; inline;
begin
{$IFDEF LITTLE_ENDIAN}
  Result := AValue;
{$ELSE}
  Result := Endian(AValue);
{$ENDIF}
end;

function LE2N(const AValue: Int64): Int64; overload; inline;
begin
{$IFDEF LITTLE_ENDIAN}
  Result := AValue;
{$ELSE}
  Result := Endian(AValue);
{$ENDIF}
end;

function LE2N(const AValue: UInt64): UInt64; overload; inline;
begin
{$IFDEF LITTLE_ENDIAN}
  Result := AValue;
{$ELSE}
  Result := Endian(AValue);
{$ENDIF}
end;

function N2BE(const AValue: SmallInt): SmallInt; overload; inline;
begin
{$IFDEF BIG_ENDIAN}
  Result := AValue;
{$ELSE}
  Result := Endian(AValue);
{$ENDIF}
end;

function N2BE(const AValue: Word): Word; overload; inline;
begin
{$IFDEF BIG_ENDIAN}
  Result := AValue;
{$ELSE}
  Result := Endian(AValue);
{$ENDIF}
end;

function N2BE(const AValue: Integer): Integer; overload; inline;
begin
{$IFDEF BIG_ENDIAN}
  Result := AValue;
{$ELSE}
  Result := Endian(AValue);
{$ENDIF}
end;

function N2BE(const AValue: Cardinal): Cardinal; overload; inline;
begin
{$IFDEF BIG_ENDIAN}
  Result := AValue;
{$ELSE}
  Result := Endian(AValue);
{$ENDIF}
end;

function N2BE(const AValue: Int64): Int64; overload; inline;
begin
{$IFDEF BIG_ENDIAN}
  Result := AValue;
{$ELSE}
  Result := Endian(AValue);
{$ENDIF}
end;

function N2BE(const AValue: UInt64): UInt64; overload; inline;
begin
{$IFDEF BIG_ENDIAN}
  Result := AValue;
{$ELSE}
  Result := Endian(AValue);
{$ENDIF}
end;

function N2LE(const AValue: SmallInt): SmallInt; overload; inline;
begin
{$IFDEF LITTLE_ENDIAN}
  Result := AValue;
{$ELSE}
  Result := Endian(AValue);
{$ENDIF}
end;

function N2LE(const AValue: Word): Word; overload; inline;
begin
{$IFDEF LITTLE_ENDIAN}
  Result := AValue;
{$ELSE}
  Result := Endian(AValue);
{$ENDIF}
end;

function N2LE(const AValue: Integer): Integer; overload; inline;
begin
{$IFDEF LITTLE_ENDIAN}
  Result := AValue;
{$ELSE}
  Result := Endian(AValue);
{$ENDIF}
end;

function N2LE(const AValue: Cardinal): Cardinal; overload; inline;
begin
{$IFDEF LITTLE_ENDIAN}
  Result := AValue;
{$ELSE}
  Result := Endian(AValue);
{$ENDIF}
end;

function N2LE(const AValue: Int64): Int64; overload; inline;
begin
{$IFDEF LITTLE_ENDIAN}
  Result := AValue;
{$ELSE}
  Result := Endian(AValue);
{$ENDIF}
end;

function N2LE(const AValue: UInt64): UInt64; overload; inline;
begin
{$IFDEF LITTLE_ENDIAN}
  Result := AValue;
{$ELSE}
  Result := Endian(AValue);
{$ENDIF}
end;
