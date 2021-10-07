{ ****************************************************************************** }
{ CnPack For Delphi/C++Builder }
{  Chinese people's own open source third-party development package  }
{  (C) Copyright 2001-2015 CnPack development team  }
{ ------------------------------------ }
{ }
{  This development package is open source free software. You can repair it in accordance with the release agreement of CnPack  }
{  Revise and reissue the program.  }
{ }
{  This development package was released in the hope that it would be useful, but there is no guarantee. Not even  }
{  Implied warranty for a particular purpose. For more details, see the CnPack publishing protocol.  }
{ }
{  You should have received a copy of the CnPack distribution agreement with the development package. If  }
{  Not yet, visit our website:  }
{ }
{  Website address: http://www.cnpack.org  }
{  E-mail: master@cnpack.org  }
{ }
{ ****************************************************************************** }

{ ****************************************************************************** }
{  This unit is rewritten based on md5.pas of Ronald L. Rivest. The following is the declaration of md5.pas:  }
{ ----------------------------------------------------------------------------- }
{ }
{ MD5 Message-Digest for Delphi 4 }
{ }
{ Delphi 4 Unit implementing the }
{ RSA Data Security, Inc. MD5 Message-Digest Algorithm }
{ }
{ Implementation of Ronald L. Rivest's RFC 1321 }
{ }
{ Copyright ?1997-1999 Medienagentur Fichtner & Meyer }
{ Written by Matthias Fichtner }
{ }
{ ----------------------------------------------------------------------------- }
{ See RFC 1321 for RSA Data Security's copyright and license notice! }
{ ----------------------------------------------------------------------------- }
{ The latest release of md5.pas will always be available from }
{ the distribution site at: http://www.fichtner.net/delphi/md5/ }
{ ----------------------------------------------------------------------------- }
{ Please send questions, bug reports and suggestions }
{ regarding this code to: mfichtner@fichtner-meyer.com }
{ ----------------------------------------------------------------------------- }
{ This code is provided "as is" without express or }
{ implied warranty of any kind. Use it at your own risk. }
{ ****************************************************************************** }

(* ************************************************ *)
(* *)
{  Modified: love to eat pig head meat & flying Wang 2015-08-05  }
{  Please do not remove the above copyright notice.  }
(* *)
{  It is forbidden to publish to citcom network disk.  }
(* *)
(* ************************************************ *)

unit FlyUtils.CnMD5;
{ * |<PRE>
  ================================================================================
  * 软件名称：开发包基础库
  * 单元名称：MD5算法单元
  * 单元作者：何清（QSoft） hq.com@263.net; http://qsoft.51.net
  * 备    注：
  * 开发平台：PWin2000Pro + Delphi 5.0
  * 兼容测试：PWin9X/2000/XP + Delphi 5/6
  * 本 地 化：该单元中的字符串均符合本地化处理方式
  * 单元标识：$Id$
  * 修改记录：2014.11.14 V1.2
  *               汇编切换至 Pascal 以支持跨平台
  *           2003.09.18 V1.1
  *               好不容易找到了该单元原作者的版权声明
  *           2003.09.18 V1.0
  *               创建单元
  ================================================================================
  |</PRE> }


{  You can compare it with the original version to see what I have changed.  }
{  Idiots always think Delphi is different from other languages.  }
{  However, with the correct code, the results of various encryption and decryption are the same.  }
{  If you don't like hex format, you can return to Base64 format.  }
{  On xe7, you can use system.netencoding  }
{  Call system.netencoding.base64.encode (use the parameters of tbytes version);  }

interface

//{$I CnPack.inc}

{$OVERFLOWCHECKS OFF}
{$RANGECHECKS OFF}


uses
  System.Classes, System.SysUtils, FlyUtils.CnXXX.Common;

const
  DefaultMD5BufSize = 32 * 1024;

var
  BufSizeForString: UInt32 = DefaultMD5BufSize;

type
  TMD5Count = array [0 .. 1] of UInt32;
  TMD5State = array [0 .. 3] of UInt32;
  TMD5Block = array [0 .. 15] of UInt32;
  TMD5CBits = array [0 .. 7] of byte;
  TMD5Digest = array [0 .. 15] of byte;
  TMD5Buffer = array [0 .. 63] of byte;

  TMD5Context = record
    State: TMD5State;
    Count: TMD5Count;
    Buffer: TMD5Buffer;
    Ipad: array [0 .. HmacPadLen - 1] of byte; { !< HMAC: inner padding }
    Opad: array [0 .. HmacPadLen - 1] of byte; { !< HMAC: outer padding }
  end;

procedure MD5Init(var Context: TMD5Context);
procedure MD5Update(var Context: TMD5Context; Input: array of byte;
  Length: UInt32);
procedure MD5Final(var Context: TMD5Context; var Digest: TMD5Digest);

function MD5(Input: TBytes; BufLen: UInt32): TMD5Digest;

procedure MD5HmacInit(var Ctx: TMD5Context; Key: TBytes; KeyLength: Integer);

procedure MD5HmacUpdate(var Ctx: TMD5Context; Input: array of byte;
  Length: UInt32);

procedure MD5HmacFinal(var Ctx: TMD5Context; var Output: TMD5Digest);

procedure MD5Hmac(Key: TBytes; KeyLength: Integer; Input: TBytes;
  Length: UInt32; var Output: TMD5Digest);
{ * Hash-based Message Authentication Code (based on MD5) }

//----------------------------------------------------------------
{  User API function definition  }
//----------------------------------------------------------------

function MD5Buffer(const Buffer; Count: UInt32): TMD5Digest;
{  *MD5 conversion of data blocks
|<PRE>
Const buffer - data block to be calculated
Count: uint32 - data block length
|</PRE>  }

function MD5Print(const Digest: TMD5Digest): string;
{  *Output MD5 calculated value in hexadecimal format
|<PRE>
Digest: tmd5digest - specified MD5 calculated value
|</PRE>  }

function MD5Match(const D1, D2: TMD5Digest): Boolean;
{  *Compare whether the two MD5 calculated values are equal
|<PRE>
D1: tmd5digest - MD5 calculated value to be compared
D2: tmd5digest - MD5 calculated value to be compared
|</PRE>  }

function MD5DigestToStr(aDig: TMD5Digest): string;
{  *MD5 calculated value to string
|<PRE>
ADIG: tmd5digest - MD5 calculated value to be converted
|</PRE>  }

{  All encoding values are not assigned. The default value is utf8.  }

function MD5Stream_BytesKey(Stream: TStream; const ByteCount: UInt64;
  HmacKeyBytes: TBytes;
  const BufSize: UInt32 = DefaultMD5BufSize;
  HmacMode: Boolean = False;
  OnProcessProc: TOnProcessProc = nil; ProcessProc: TProcessProc = nil)
  : TMD5Digest;

function MD5Stream(Stream: TStream; const ByteCount: UInt64;
  const BufSize: UInt32 = DefaultMD5BufSize;
  HmacMode: Boolean = False; HmacKey: string = ''; KeyEncoding: TEncoding = nil;
  KeyCRLFMode: TCRLFMode = rlCRLF;
  OnProcessProc: TOnProcessProc = nil; ProcessProc: TProcessProc = nil)
  : TMD5Digest;
{  *MD5 transform the specified stream data
|<PRE>
Stream: tsstream - stream content to be calculated
Callback: tmd5calcprogressfunc - progress callback function, which is empty by default
|</PRE>  }

function MD5File(const FileName: string;
  const BufSize: UInt32 = DefaultMD5BufSize;
  HmacMode: Boolean = False; HmacKey: string = ''; KeyEncoding: TEncoding = nil;
  OnProcessProc: TOnProcessProc = nil; ProcessProc: TProcessProc = nil)
  : TMD5Digest;
{  *MD5 conversion of specified file data
|<PRE>
Filename: String - the file name to calculate
Callback: tmd5calcprogressfunc - progress callback function, which is empty by default
|</PRE>  }

function MD5String(Value: string; StrEncoding: TEncoding = nil;
  ValueCRLFMode: TCRLFMode = rlCRLF;
  HmacMode: Boolean = False; HmacKey: string = ''; KeyEncoding: TEncoding = nil;
  KeyCRLFMode: TCRLFMode = rlCRLF;
  OnProcessProc: TOnProcessProc = nil;
  ProcessProc: TProcessProc = nil): TMD5Digest;
{  *Incoming plaintext and encryption key, MD5 encrypts and returns ciphertext,
Note: since the ciphertext may contain extended ASCII characters, in Delphi 2009 or above, please use
Variables of type tbytes receive the return value to avoid unnecessary Unicode conversion leading to decryption errors  }

function MD5StringToHex(Value: string; StrEncoding: TEncoding = nil;
  ValueCRLFMode: TCRLFMode = rlCRLF;
  HmacMode: Boolean = False; HmacKey: string = ''; KeyEncoding: TEncoding = nil;
  KeyCRLFMode: TCRLFMode = rlCRLF;
  OnProcessProc: TOnProcessProc = nil;
  ProcessProc: TProcessProc = nil): string;

function MD5StreamToHex(Stream: TStream; const ByteCount: UInt64; const
  BufSize: UInt32 = DefaultMD5BufSize; HmacMode: Boolean = False; HmacKey:
  string = ''; KeyEncoding: TEncoding = nil; KeyCRLFMode: TCRLFMode = rlCRLF;
  OnProcessProc: TOnProcessProc = nil; ProcessProc: TProcessProc = nil):
  String;

{  *MD5 transform the specified stream data
|<PRE>
Stream: tsstream - stream content to be calculated
Callback: tmd5calcprogressfunc - progress callback function, which is empty by default
|</PRE>  }

function MD5FileToHex(const FileName: string; const BufSize: UInt32 =
  DefaultMD5BufSize; HmacMode: Boolean = False; HmacKey: string = '';
  KeyEncoding: TEncoding = nil; OnProcessProc: TOnProcessProc = nil;
  ProcessProc: TProcessProc = nil): String;

{  *Pass in plaintext and encryption key, and MD5 encrypts and returns the ciphertext converted into hexadecimal  }

implementation

//uses FMX.Dialogs;

type
  PUInt32 = ^UInt32;

var
  PADDING: TMD5Buffer = (
    $80, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00
    );

function F(x, y, z: UInt32): UInt32;
begin
  Result := (x and y) or ((not x) and z);
  //AND EDX, EAX
  //NOT EAX
  //AND EAX, ECX
  //OR EAX, EDX
end;

function G(x, y, z: UInt32): UInt32;
begin
  Result := (x and z) or (y and (not z));
  //AND EAX, ECX
  //NOT ECX
  //AND EDX, ECX
  //OR EAX, EDX
end;

function H(x, y, z: UInt32): UInt32;
begin
  Result := x xor y xor z;
  //XOR EAX, EDX
  //XOR EAX, ECX
end;

function I(x, y, z: UInt32): UInt32;
begin
  Result := y xor (x or (not z));
  //NOT ECX
  //OR EAX, ECX
  //XOR EAX, EDX
end;

procedure ROT(var x: UInt32; n: byte);
begin
  x := (x shl n) or (x shr (32 - n));
  //PUSH EBX
  //MOV CL, $20
  //SUB CL, DL
  //MOV EBX, [EAX]
  //SHR EBX, CL
  //MOV ECX, EDX
  //MOV EDX, [EAX]
  //SHL EDX, CL
  //OR EBX, EDX
  //MOV [EAX], EBX
  //POP EBX
end;

procedure FF(var a: UInt32; b, c, d, x: UInt32; s: byte; ac: UInt32);
begin
  Inc(a, F(b, c, d) + x + ac);
  ROT(a, s);
  Inc(a, b);
end;

procedure GG(var a: UInt32; b, c, d, x: UInt32; s: byte; ac: UInt32);
begin
  Inc(a, G(b, c, d) + x + ac);
  ROT(a, s);
  Inc(a, b);
end;

procedure HH(var a: UInt32; b, c, d, x: UInt32; s: byte; ac: UInt32);
begin
  Inc(a, H(b, c, d) + x + ac);
  ROT(a, s);
  Inc(a, b);
end;

procedure II(var a: UInt32; b, c, d, x: UInt32; s: byte; ac: UInt32);
begin
  Inc(a, I(b, c, d) + x + ac);
  ROT(a, s);
  Inc(a, b);
end;

//Encode Count bytes at Source into (Count / 4) UInt32s at Target
procedure Encode(Source, Target: pointer; Count: UInt32);
var
  s: PByte;
  T: PUInt32;
  I: UInt32;
begin
  s := Source;
  T := Target;
  for I := 1 to Count div 4 do
  begin
    T^ := s^;
    Inc(s);
    T^ := T^ or (s^ shl 8);
    Inc(s);
    T^ := T^ or (s^ shl 16);
    Inc(s);
    T^ := T^ or (s^ shl 24);
    Inc(s);
    Inc(T);
  end;
end;

//Decode Count UInt32s at Source into (Count * 4) Bytes at Target
procedure Decode(Source, Target: pointer; Count: UInt32);
var
  s: PUInt32;
  T: PByte;
  I: UInt32;
begin
  s := Source;
  T := Target;
  for I := 1 to Count do
  begin
    T^ := s^ and $FF;
    Inc(T);
    T^ := (s^ shr 8) and $FF;
    Inc(T);
    T^ := (s^ shr 16) and $FF;
    Inc(T);
    T^ := (s^ shr 24) and $FF;
    Inc(T);
    Inc(s);
  end;
end;

//Transform State according to first 64 bytes at Buffer
procedure Transform(Buffer: pointer; var State: TMD5State);
var
  a, b, c, d: UInt32;
  Block: TMD5Block;
begin
  Encode(Buffer, @Block, 64);
  a := State[0];
  b := State[1];
  c := State[2];
  d := State[3];
  FF(a, b, c, d, Block[0], 7, $D76AA478);
  FF(d, a, b, c, Block[1], 12, $E8C7B756);
  FF(c, d, a, b, Block[2], 17, $242070DB);
  FF(b, c, d, a, Block[3], 22, $C1BDCEEE);
  FF(a, b, c, d, Block[4], 7, $F57C0FAF);
  FF(d, a, b, c, Block[5], 12, $4787C62A);
  FF(c, d, a, b, Block[6], 17, $A8304613);
  FF(b, c, d, a, Block[7], 22, $FD469501);
  FF(a, b, c, d, Block[8], 7, $698098D8);
  FF(d, a, b, c, Block[9], 12, $8B44F7AF);
  FF(c, d, a, b, Block[10], 17, $FFFF5BB1);
  FF(b, c, d, a, Block[11], 22, $895CD7BE);
  FF(a, b, c, d, Block[12], 7, $6B901122);
  FF(d, a, b, c, Block[13], 12, $FD987193);
  FF(c, d, a, b, Block[14], 17, $A679438E);
  FF(b, c, d, a, Block[15], 22, $49B40821);
  GG(a, b, c, d, Block[1], 5, $F61E2562);
  GG(d, a, b, c, Block[6], 9, $C040B340);
  GG(c, d, a, b, Block[11], 14, $265E5A51);
  GG(b, c, d, a, Block[0], 20, $E9B6C7AA);
  GG(a, b, c, d, Block[5], 5, $D62F105D);
  GG(d, a, b, c, Block[10], 9, $2441453);
  GG(c, d, a, b, Block[15], 14, $D8A1E681);
  GG(b, c, d, a, Block[4], 20, $E7D3FBC8);
  GG(a, b, c, d, Block[9], 5, $21E1CDE6);
  GG(d, a, b, c, Block[14], 9, $C33707D6);
  GG(c, d, a, b, Block[3], 14, $F4D50D87);
  GG(b, c, d, a, Block[8], 20, $455A14ED);
  GG(a, b, c, d, Block[13], 5, $A9E3E905);
  GG(d, a, b, c, Block[2], 9, $FCEFA3F8);
  GG(c, d, a, b, Block[7], 14, $676F02D9);
  GG(b, c, d, a, Block[12], 20, $8D2A4C8A);
  HH(a, b, c, d, Block[5], 4, $FFFA3942);
  HH(d, a, b, c, Block[8], 11, $8771F681);
  HH(c, d, a, b, Block[11], 16, $6D9D6122);
  HH(b, c, d, a, Block[14], 23, $FDE5380C);
  HH(a, b, c, d, Block[1], 4, $A4BEEA44);
  HH(d, a, b, c, Block[4], 11, $4BDECFA9);
  HH(c, d, a, b, Block[7], 16, $F6BB4B60);
  HH(b, c, d, a, Block[10], 23, $BEBFBC70);
  HH(a, b, c, d, Block[13], 4, $289B7EC6);
  HH(d, a, b, c, Block[0], 11, $EAA127FA);
  HH(c, d, a, b, Block[3], 16, $D4EF3085);
  HH(b, c, d, a, Block[6], 23, $4881D05);
  HH(a, b, c, d, Block[9], 4, $D9D4D039);
  HH(d, a, b, c, Block[12], 11, $E6DB99E5);
  HH(c, d, a, b, Block[15], 16, $1FA27CF8);
  HH(b, c, d, a, Block[2], 23, $C4AC5665);
  II(a, b, c, d, Block[0], 6, $F4292244);
  II(d, a, b, c, Block[7], 10, $432AFF97);
  II(c, d, a, b, Block[14], 15, $AB9423A7);
  II(b, c, d, a, Block[5], 21, $FC93A039);
  II(a, b, c, d, Block[12], 6, $655B59C3);
  II(d, a, b, c, Block[3], 10, $8F0CCC92);
  II(c, d, a, b, Block[10], 15, $FFEFF47D);
  II(b, c, d, a, Block[1], 21, $85845DD1);
  II(a, b, c, d, Block[8], 6, $6FA87E4F);
  II(d, a, b, c, Block[15], 10, $FE2CE6E0);
  II(c, d, a, b, Block[6], 15, $A3014314);
  II(b, c, d, a, Block[13], 21, $4E0811A1);
  II(a, b, c, d, Block[4], 6, $F7537E82);
  II(d, a, b, c, Block[11], 10, $BD3AF235);
  II(c, d, a, b, Block[2], 15, $2AD7D2BB);
  II(b, c, d, a, Block[9], 21, $EB86D391);
  Inc(State[0], a);
  Inc(State[1], b);
  Inc(State[2], c);
  Inc(State[3], d);
end;

procedure MD5InitAndClear(var Ctx: TMD5Context);
begin
  FillChar(Ctx.Ipad, SizeOf(Ctx.Ipad), 0);
  FillChar(Ctx.Opad, SizeOf(Ctx.Opad), 0);
  MD5Init(Ctx);
end;

//Initialize given Context
procedure MD5Init(var Context: TMD5Context);
begin
  with Context do
  begin
    State[0] := $67452301;
    State[1] := $EFCDAB89;
    State[2] := $98BADCFE;
    State[3] := $10325476;
    Count[0] := 0;
    Count[1] := 0;
    FillChar(Buffer, SizeOf(TMD5Buffer), 0);
  end;
end;

//Update given Context to include Length bytes of Input
procedure MD5Update(var Context: TMD5Context; Input: array of byte;
  Length: UInt32);
var
  Index: UInt32;
  PartLen: UInt32;
  I: UInt32;
begin
  with Context do
  begin
    Index := (Count[0] shr 3) and $3F;
    Inc(Count[0], Length shl 3);
    if Count[0] < (Length shl 3) then
      Inc(Count[1]);
    Inc(Count[1], Length shr 29);
  end;
  if Index > 64 then
    Index := 64;

  PartLen := 64 - Index;
  if Length >= PartLen then
  begin
    Move(Input[0], Context.Buffer[Index], PartLen);
    Transform(@Context.Buffer, Context.State);
    I := PartLen;
    while I + 63 < Length do
    begin
      Transform(@Input[I], Context.State);
      Inc(I, 64);
    end;
    Index := 0;
  end
  else
    I := 0;
  PartLen := Length - I;
  if PartLen > 0 then
  begin
    Move(Input[I], Context.Buffer[Index], PartLen);
  end;
end;

//Finalize given Context, create Digest and zeroize Context
procedure MD5Final(var Context: TMD5Context; var Digest: TMD5Digest);
var
  Bits: TMD5CBits;
  Index: UInt32;
  PadLen: UInt32;
begin
  Decode(@Context.Count, @Bits, 2);
  Index := (Context.Count[0] shr 3) and $3F;
  if Index < 0 then
    Index := 0;
  if Index < 56 then
    PadLen := 56 - Index
  else
    PadLen := 120 - Index;
  if PadLen > 64 then
    PadLen := 64;
  if PadLen < 0 then
    PadLen := 0;
  MD5Update(Context, PADDING, PadLen);
  MD5Update(Context, Bits, 8);
  Decode(@Context.State, @Digest, 4);
  MD5Init(Context);
  //FillChar(Context, SizeOf(TMD5Context), 0); // delete for Hmac;
end;

function MD5(Input: TBytes; BufLen: UInt32): TMD5Digest;
var
  Context: TMD5Context;
begin
  MD5InitAndClear(Context);
  MD5Update(Context, Input, BufLen);
  MD5Final(Context, Result);
end;

procedure MD5HmacInit(var Ctx: TMD5Context; Key: TBytes; KeyLength: Integer);
var
  I: Integer;
  Sum: TMD5Digest;
begin
  if KeyLength > HmacPadLen then
  begin
    Sum := MD5(Key, KeyLength);
    KeyLength := SizeOf(Sum);
    //Key := @Sum;
    Move(Sum[0], Key[0], SizeOf(Sum));
  end;

  FillChar(Ctx.Ipad, SizeOf(Ctx.Ipad), 0);
  FillChar(Ctx.Opad, SizeOf(Ctx.Opad), 0);
  FillChar(Ctx.Ipad, HmacPadLen, $36);
  FillChar(Ctx.Opad, HmacPadLen, $5C);
  //
  for I := 0 to KeyLength - 1 do
  begin
    Ctx.Ipad[I] := byte(Ctx.Ipad[I] xor byte(Key[I]));
    Ctx.Opad[I] := byte(Ctx.Opad[I] xor byte(Key[I]));
  end;

  /// /http://www.ietf.org/rfc/rfc2104.txt
  //Move(Key[0], Ctx.Ipad[0], KeyLength);
  //Move(Key[0], Ctx.Opad[0], KeyLength);
  //for I := 0 to HmacPadLen -1 do
  //begin
  //Ctx.Ipad[I] := Ctx.Ipad[I] xor $36;
  //Ctx.Opad[I] := Ctx.Opad[I] xor $5C;
  //end;

  MD5Init(Ctx);
  MD5Update(Ctx, Ctx.Ipad, HmacPadLen);
end;

procedure MD5HmacUpdate(var Ctx: TMD5Context; Input: array of byte;
  Length: UInt32);
begin
  MD5Update(Ctx, Input, Length);
end;

procedure MD5HmacFinal(var Ctx: TMD5Context; var Output: TMD5Digest);
var
  Len: Integer;
  TmpBuf: TMD5Digest;
begin
  Len := SizeOf(TmpBuf);
  MD5Final(Ctx, TmpBuf);
  MD5Init(Ctx);
  MD5Update(Ctx, Ctx.Opad, HmacPadLen);
  MD5Update(Ctx, TmpBuf, Len);
  MD5Final(Ctx, Output);
end;

procedure MD5Hmac(Key: TBytes; KeyLength: Integer; Input: TBytes;
  Length: UInt32; var Output: TMD5Digest);
var
  Ctx: TMD5Context;
begin
  MD5HmacInit(Ctx, Key, KeyLength);
  MD5HmacUpdate(Ctx, Input, Length);
  MD5HmacFinal(Ctx, Output);
end;

function InternalMD5Stream(Stream: TStream; const ByteCount: UInt64;
  const BufSize: UInt32;
  var d: TMD5Digest; HmacKeyBytes: TBytes;
  HmacMode: Boolean = False;
  OnProcessProc: TOnProcessProc = nil; ProcessProc: TProcessProc = nil)
  : Boolean;
var
  Context: TMD5Context;
  Buf: TBytes;
  BufLen: Cardinal;
  Size: Int64;
  ReadBytes: Cardinal;
  TotalBytes: Int64;
  //SavePos: Int64;
  CancelCalc: Boolean;
begin
  Result := False;
  if ByteCount = 0 then
  begin
    Stream.Position := 0;
    Size := Stream.Size;
  end
  else
  begin
    Size := Stream.Size - Stream.Position;
    if ByteCount < Size then
      Size := ByteCount;
  end;
  //SavePos := Stream.Position;
  TotalBytes := 0;
  if Size = 0 then
    Exit;
  if Size < BufSize then
    BufLen := Size
  else
    BufLen := BufSize;

  CancelCalc := False;
  if HmacMode then
    MD5HmacInit(Context, HmacKeyBytes, Length(HmacKeyBytes))
  else
    MD5InitAndClear(Context);
  SetLength(Buf, BufLen);
  FillChar(Buf[0], BufLen, 0);
  try
    repeat
      if TotalBytes > Size - BufLen then
      begin
        BufLen := Size - TotalBytes;
      end;
      ReadBytes := Stream.Read(Buf, BufLen);
      if ReadBytes <> 0 then
      begin
        Inc(TotalBytes, ReadBytes);
        if HmacMode then
          MD5HmacUpdate(Context, Buf, ReadBytes)
        else
          MD5Update(Context, Buf, ReadBytes);
        if Assigned(OnProcessProc) then
          OnProcessProc(nil, 0, Size, TotalBytes, CancelCalc);
        if Assigned(ProcessProc) then
          ProcessProc(0, Size, TotalBytes, CancelCalc);
        if CancelCalc then
          Exit;
      end;
    until (ReadBytes <= 0) or (TotalBytes >= Size);
    if HmacMode then
      MD5HmacFinal(Context, d)
    else
      MD5Final(Context, d);
    Result := True;
  finally
    SetLength(Buf, 0);
    //Stream.Position := SavePos;
  end;
end;

//----------------------------------------------------------------
{  User API function implementation  }
//----------------------------------------------------------------

{  MD5 conversion of data blocks  }
function MD5Buffer(const Buffer; Count: UInt32): TMD5Digest;
var
  Context: TMD5Context;
begin
  MD5Init(Context);
  MD5Update(Context, TBytes(Buffer), Count);
  MD5Final(Context, Result);
end;

{  Output MD5 calculated value in hexadecimal format  }
function MD5Print(const Digest: TMD5Digest): string;
var
  I: byte;
const
  Digits: array [0 .. 15] of Char = ('0', '1', '2', '3', '4', '5', '6', '7',
    '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
begin
  Result := '';
  for I := 0 to 15 do
    //Result := Result + {$IFDEF DELPHI12_UP}string{$ENDIF}(Digits[(Digest[I] shr 4) and $0f] +
    //Digits[Digest[I] and $0f]);
    Result := Result + IntToHex(Ord(Digest[I]), 2);
end;

{  Compare whether the two MD5 calculated values are equal  }
function MD5Match(const D1, D2: TMD5Digest): Boolean;
var
  I: byte;
begin
  I := 0;
  Result := True;
  while Result and (I < 16) do
  begin
    Result := D1[I] = D2[I];
    Inc(I);
  end;
end;

{  MD5 calculated value to string  }
function MD5DigestToStr(aDig: TMD5Digest): string;
var
  I: Integer;
begin
  SetLength(Result, 16);
  for I := 1 to 16 do
    Result[Low(Result) + I - 1] := Chr(aDig[I - 1]);
end;

function MD5Stream_BytesKey(Stream: TStream; const ByteCount: UInt64;
  HmacKeyBytes: TBytes;
  const BufSize: UInt32 = DefaultMD5BufSize;
  HmacMode: Boolean = False;
  OnProcessProc: TOnProcessProc = nil; ProcessProc: TProcessProc = nil)
  : TMD5Digest;
begin
  InternalMD5Stream(Stream, ByteCount, BufSize, Result, HmacKeyBytes,
    HmacMode, OnProcessProc, ProcessProc);
end;

{  SHA1 conversion of specified file data  }
function MD5File(const FileName: string;
  const BufSize: UInt32 = DefaultMD5BufSize;
  HmacMode: Boolean = False; HmacKey: string = ''; KeyEncoding: TEncoding = nil;
  OnProcessProc: TOnProcessProc = nil; ProcessProc: TProcessProc = nil)
  : TMD5Digest;
var
  Stream: TStream;
begin
  {  Files larger than 2G may fail to map, and the flow method is adopted for cyclic processing  }
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := MD5Stream(Stream, 0, DefaultMD5BufSize,
      HmacMode, HmacKey, KeyEncoding, rlNoChange, OnProcessProc, ProcessProc);
  finally
    Stream.Free;
  end;
end;

{  Perform MD5 calculation on the specified flow  }
function MD5Stream(Stream: TStream; const ByteCount: UInt64;
  const BufSize: UInt32 = DefaultMD5BufSize;
  HmacMode: Boolean = False; HmacKey: string = ''; KeyEncoding: TEncoding = nil;
  KeyCRLFMode: TCRLFMode = rlCRLF;
  OnProcessProc: TOnProcessProc = nil; ProcessProc: TProcessProc = nil)
  : TMD5Digest;
var
  HmacKeyBytes: TBytes;
begin
  if KeyEncoding = nil then
    KeyEncoding := TEncoding.UTF8;
  if KeyCRLFMode <> rlNoChange then
  begin
    HmacKey := ChangCRLFType(HmacKey, KeyCRLFMode);
  end;
  HmacKeyBytes := KeyEncoding.GetBytes(HmacKey);
  InternalMD5Stream(Stream, ByteCount, BufSize, Result, HmacKeyBytes,
    HmacMode, OnProcessProc, ProcessProc);
end;

function MD5StringToHex(Value: string; StrEncoding: TEncoding = nil;
  ValueCRLFMode: TCRLFMode = rlCRLF;
  HmacMode: Boolean = False; HmacKey: string = ''; KeyEncoding: TEncoding = nil;
  KeyCRLFMode: TCRLFMode = rlCRLF;
  OnProcessProc: TOnProcessProc = nil;
  ProcessProc: TProcessProc = nil): string;
begin
  Result := MD5Print(MD5String(Value, StrEncoding,
    ValueCRLFMode, HmacMode, HmacKey, KeyEncoding, KeyCRLFMode, OnProcessProc,
    ProcessProc));
end;

function MD5String(Value: string; StrEncoding: TEncoding = nil;
  ValueCRLFMode: TCRLFMode = rlCRLF;
  HmacMode: Boolean = False; HmacKey: string = ''; KeyEncoding: TEncoding = nil;
  KeyCRLFMode: TCRLFMode = rlCRLF;
  OnProcessProc: TOnProcessProc = nil;
  ProcessProc: TProcessProc = nil): TMD5Digest;
var
  TestByte: byte;
  InStream: TStringStream;
  //Context: TMD5Context;
  //Input: TBytes;
begin
  if ValueCRLFMode <> rlNoChange then
  begin
    Value := ChangCRLFType(Value, ValueCRLFMode);
  end;
  if StrEncoding = nil then
    StrEncoding := TEncoding.UTF8;
  InStream := TStringStream.Create(Value, StrEncoding, False);
  try
    //if (InStream.Size > 0) then
    //begin
    //InStream.Seek(-1, soFromEnd);
    //InStream.Read(TestByte, 1);
    //if TestByte = 0 then
    //raise Exception.Create('Error: the last char is NULL char.');
    //end;
    InStream.Position := 0;
    Result := MD5Stream(InStream, 0, DefaultMD5BufSize,
      HmacMode, HmacKey, KeyEncoding, KeyCRLFMode, OnProcessProc, ProcessProc);
  finally
    FreeAndNil(InStream);
  end;
end;

{  Perform MD5 calculation on the specified flow  }
function MD5StreamToHex(Stream: TStream; const ByteCount: UInt64; const
  BufSize: UInt32 = DefaultMD5BufSize; HmacMode: Boolean = False; HmacKey:
  string = ''; KeyEncoding: TEncoding = nil; KeyCRLFMode: TCRLFMode = rlCRLF;
  OnProcessProc: TOnProcessProc = nil; ProcessProc: TProcessProc = nil):
  String;
begin
  Result := MD5Print(MD5Stream(Stream, ByteCount, BufSize, HmacMode, HmacKey,
    KeyEncoding, KeyCRLFMode,
    OnProcessProc, ProcessProc));
end;

{  SHA1 conversion of specified file data  }
function MD5FileToHex(const FileName: string; const BufSize: UInt32 =
  DefaultMD5BufSize; HmacMode: Boolean = False; HmacKey: string = '';
  KeyEncoding: TEncoding = nil; OnProcessProc: TOnProcessProc = nil;
  ProcessProc: TProcessProc = nil): String;
begin
  Result := MD5Print(MD5File(FileName, BufSize, HmacMode, HmacKey, KeyEncoding,
    OnProcessProc, ProcessProc));
end;

end.
