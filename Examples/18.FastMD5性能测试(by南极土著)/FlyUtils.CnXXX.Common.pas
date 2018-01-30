(* ************************************************ *)
(* *)
(* 修改：爱吃猪头肉 & Flying Wang 2015-04-24 *)
(* 上面的版权声明请不要移除。 *)
(* *)
(* 禁止发布到城通网盘。 *)
(* *)
(* ************************************************ *)
unit FlyUtils.CnXXX.Common;

interface

uses
  System.SysUtils, System.Classes;

type
  TProcessProc = procedure(ProcMin, ProcMax, CurrDone: UInt64;
    var Cancel: Boolean);
  TOnProcessProc = procedure(Sender: TObject;
    ProcMin, ProcMax, CurrDone: UInt64; var Cancel: Boolean) of Object;

type
  TKeyBit = (kb128, kb192, kb256);
  TCRLFMode = (rlNoChange, rlCRLF, rlLF, rlCR, rlNONE);
  TPaddingMode = (pmZeroPadding, pmPKCS5or7Padding, pmISO10126,
    pmPKCS5or7RandomPadding);
  TCipherMode = (ECB, CBC, CFB,
    //OFB, // i donit like OFB
    CTR);
  TStringMode = (smHex, smEncode, smNormal);

const
  HmacPadLen = 64;

const
{$IFDEF MSWINDOWS}
  DefaultCRLFMode: TCRLFMode = rlCRLF;
{$ELSE}
{$IFDEF MACOS}
  DefaultCRLFMode: TCRLFMode = rlCR;
{$ELSE}
{$IFDEF IOS}
  DefaultCRLFMode: TCRLFMode = rlCR;
{$ELSE}
  DefaultCRLFMode: TCRLFMode = rlLF;
{$ENDIF IOS}
{$ENDIF MACOS}
{$ENDIF MSWINDOWS}
  {
    TStringStream = class(TStream)
    private
    FDataString: string;
    FPosition: Integer;
    protected
    procedure SetSize(NewSize: Integer); override;
    public
    constructor Create(const AString: string);
    function Read(var Buffer; Count: Integer): Integer; override;
    function ReadString(Count: Integer): string;
    function Seek(Offset: Integer; Origin: Word): Integer; override;
    function Write(const Buffer; Count: Integer): Integer; override;
    procedure WriteString(const AString: string);
    property DataString: string read FDataString;
    end;
  }
  ///<summary>
  ///StrToHex aka BytesToHex、Bytes2Hex、Str2Hex
  ///</summary>
function StrToHex(Value: TBytes): string; //aka BytesToHex
///<summary>
///HexToStr aka HexToBytes、Hex2Bytes、Hex2Str
///</summary>
function HexToStr(Value: string): TBytes; //aka HexToBytes

function ChangCRLFType(Value: string; CRLFMode: TCRLFMode): string;

function MarshaledAString2TBytes(Str: MarshaledAString): TBytes;
function TBytes2MarshaledAString(Bytes: TBytes): MarshaledAString;
function TBytes2TBytes(Bytes: TBytes): TBytes;

function EncodeBase64Bytes(InputBytes: TBytes): string;
function DecodeBase64Bytes(InputString: string): TBytes;

implementation

uses
  System.NetEncoding;

function EncodeBase64Bytes(InputBytes: TBytes): string;
begin
  Result := TNetEncoding.Base64.EncodeBytesToString(InputBytes);
end;

function DecodeBase64Bytes(InputString: string): TBytes;
begin
  Result := TNetEncoding.Base64.DecodeStringToBytes(InputString);
end;

function TBytes2TBytes(Bytes: TBytes): TBytes;
begin
  Result := Bytes;
  SetLength(Result, Length(MarshaledAString(Bytes)));
end;

function MarshaledAString2TBytes(Str: MarshaledAString): TBytes;
begin
  Result := TBytes(Str);
  SetLength(Result, Length(Str));
end;

function TBytes2MarshaledAString(Bytes: TBytes): MarshaledAString;
begin
  Result := MarshaledAString(Bytes);
end;

{
  constructor TStringStream.Create(const AString: string);
  begin
  inherited Create;
  FDataString := AString;
  end;

  function TStringStream.Read(var Buffer; Count: Integer): Integer;
  begin
  Result := Length(FDataString) - FPosition;
  if Result > Count then Result := Count;
  Move(PAnsiChar(@FDataString[FPosition + SizeOf(Char)])^, Buffer, Result * SizeOf(Char));
  Inc(FPosition, Result);
  end;

  function TStringStream.Write(const Buffer; Count: Integer): Integer;
  begin
  Result := Count;
  SetLength(FDataString, (FPosition + Result));
  Move(Buffer, PAnsiChar(@FDataString[FPosition + SizeOf(Char)])^, Result * SizeOf(Char));
  Inc(FPosition, Result);
  end;

  function TStringStream.Seek(Offset: Integer; Origin: Word): Integer;
  begin
  case Origin of
  soFromBeginning: FPosition := Offset;
  soFromCurrent: FPosition := FPosition + Offset;
  soFromEnd: FPosition := Length(FDataString) - Offset;
  end;
  if FPosition > Length(FDataString) then
  FPosition := Length(FDataString)
  else if FPosition < 0 then FPosition := 0;
  Result := FPosition;
  end;

  function TStringStream.ReadString(Count: Integer): string;
  var
  Len: Integer;
  begin
  Len := Length(FDataString) - FPosition;
  if Len > Count then Len := Count;
  SetString(Result, PAnsiChar(@FDataString[FPosition + SizeOf(Char)]), Len);
  Inc(FPosition, Len);
  end;

  procedure TStringStream.WriteString(const AString: string);
  begin
  Write(PAnsiChar(AString)^, Length(AString));
  end;

  procedure TStringStream.SetSize(NewSize: Integer);
  begin
  SetLength(FDataString, NewSize);
  if FPosition > NewSize then FPosition := NewSize;
  end;
}

function ChangCRLFType(Value: string; CRLFMode: TCRLFMode): string;
var
  HasCRLF: Boolean;
begin
  Result := Value;
  if CRLFMode = rlNoChange then
    exit;
  HasCRLF := Pos(#10, Result) > 0;
  if not HasCRLF then
  begin
    HasCRLF := Pos(#13, Result) > 0;
  end;
  if HasCRLF then
  begin
    Result := StringReplace(Result, #13#10, #10, [rfReplaceAll]);
    Result := StringReplace(Result, #10#13, #10, [rfReplaceAll]);
    Result := StringReplace(Result, #13, #10, [rfReplaceAll]);
    if CRLFMode = rlCRLF then
    begin
      Result := StringReplace(Result, #10, #13#10, [rfReplaceAll]);
    end
    else if CRLFMode = rlCR then
    begin
      Result := StringReplace(Result, #10, #13, [rfReplaceAll]);
    end
    else if CRLFMode = rlNONE then
    begin
      Result := StringReplace(Result, #10, '', [rfReplaceAll]);
    end;
  end;
end;

function StrToHex(Value: TBytes): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Length(Value) - 1 do
    Result := Result + IntToHex(Ord(Value[I]), 2);
end;

function HexToStr(Value: string): TBytes;
var
  ArrLen, ValueLen,
    I: Integer;
begin
  ValueLen := Length(Value);
  ArrLen := (ValueLen div 2) + (ValueLen mod 2);
  SetLength(Result, ArrLen);
  //for I := 0 to Len do
  //Result[I] := #0;
  //Result[Len] := #0;
  ArrLen := (ValueLen div 2);
  for I := 0 to ArrLen - 1 do
  begin
    //Copy based 1
    Byte(Result[I]) := Byte(StrToInt('0x' + Copy(Value, I * 2 + 1, 2)));
  end;
end;

end.
