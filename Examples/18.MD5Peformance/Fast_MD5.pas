(*
  fastMD5 algorithm by Maxim Masiutin
  https://github.com/maximmasiutin/MD5_Transform-x64

  delphi implementation by 600585@qq.com
  https://github.com/PassByYou888/FastMD5/
*)


(*
processor architecture: x86,x64
supports platform: Win32,Win64
compatible:	https and openssl
*)

unit Fast_MD5;

interface

uses Classes;

{$IF Defined(MSWINDOWS)}

type
  TMD5 = array [0 .. 15] of byte;
  PMD5 = ^TMD5;
function FastMD5(const BuffPtr: PBYTE; BufSiz: NativeUInt): TMD5; overload;
function FastMD5(Stream: TStream; const StartPos, EndPos: Int64): TMD5; overload;
{$IFEND}

implementation

{$IF Defined(MSWINDOWS)}

{$IF Defined(WIN32)}
(*
  ; ==============================================================
  ;
  ; MD5_386.Asm   -  386 optimized helper routine for calculating
  ;                  MD Message-Digest values
  ; written 2/2/94 by
  ;
  ; Peter Sawatzki
  ; Buchenhof 3
  ; D58091 Hagen, Germany Fed Rep
  ;
  ; EMail: Peter@Sawatzki.de
  ; EMail: 100031.3002@compuserve.com
  ; WWW:   http://www.sawatzki.de
  ;
  ;
  ; original C Source was found in Dr. Dobbs Journal Sep 91
  ; MD5 algorithm from RSA Data Security, Inc.
*)
{$LINK MD5_32.obj}
{$ELSEIF Defined(WIN64)}
(*
  ; MD5_Transform-x64
  ; MD5 transform routine oprimized for x64 processors
  ; Copyright 2018 Ritlabs, SRL
  ; The 64-bit version is written by Maxim Masiutin <max@ritlabs.com>

  ; The main advantage of this 64-bit version is that
  ; it loads 64 bytes of hashed message into 8 64-bit registers
  ; (RBP, R8, R9, R10, R11, R12, R13, R14) at the beginning,
  ; to avoid excessive memory load operations
  ; througout the routine.

  ; To operate with 32-bit values store in higher bits
  ; of a 64-bit register (bits 32-63) uses "Ror" by 32;
  ; 8 macro variables (M1-M8) are used to keep record
  ; or corrent state of whether the register has been
  ; Ror'ed or not.

  ; It also has an ability to use Lea instruction instead
  ; of two sequental Adds (uncomment UseLea=1), but it is
  ; slower on Skylake processors. Also, Intel in the
  ; Optimization Reference Maual discourages us of
  ; Lea as a replacement of two adds, since it is slower
  ; on the Atom processors.

  ; MD5_Transform-x64 is released under a dual license,
  ; and you may choose to use it under either the
  ; Mozilla Public License 2.0 (MPL 2.1, available from
  ; https://www.mozilla.org/en-US/MPL/2.0/) or the
  ; GNU Lesser General Public License Version 3,
  ; dated 29 June 2007 (LGPL 3, available from
  ; https://www.gnu.org/licenses/lgpl.html).

  ; MD5_Transform-x64 is based
  ; on the following code by Peter Sawatzki.

  ; The original notice by Peter Sawatzki follows.
*)
{$LINK MD5_64.obj}
{$IFEND}
procedure MD5_Transform(var Accu; const Buf); register; external;

function FastMD5(const BuffPtr: PBYTE; BufSiz: NativeUInt): TMD5;
var
  digest : TMD5;
  Lo, Hi : Cardinal;
  p      : PBYTE;
  WorkLen: Byte;
  WorkBuf: array [0 .. 63] of Byte;
begin
  Lo := 0;
  Hi := 0;
  PCardinal(@digest[0])^ := $67452301;
  PCardinal(@digest[4])^ := $EFCDAB89;
  PCardinal(@digest[8])^ := $98BADCFE;
  PCardinal(@digest[12])^ := $10325476;

  if BufSiz shl 3 < 0 then
      Inc(Hi);

  Inc(Lo, BufSiz shl 3);
  Inc(Hi, BufSiz shr 29);

  p := BuffPtr;

  while BufSiz >= $40 do
    begin
      MD5_Transform(digest, p^);
      Inc(p, $40);
      Dec(BufSiz, $40);
    end;
  if BufSiz > 0 then
      move(p^, WorkBuf[0], BufSiz);

  Result := PMD5(@digest[0])^;
  WorkBuf[BufSiz] := $80;
  WorkLen := BufSiz + 1;
  if WorkLen > $38 then
    begin
      if WorkLen < $40 then
          FillChar(WorkBuf[WorkLen], $40 - WorkLen, 0);
      MD5_Transform(Result, WorkBuf);
      WorkLen := 0
    end;
  FillChar(WorkBuf[WorkLen], $38 - WorkLen, 0);
  PCardinal(@WorkBuf[$38])^ := Lo;
  PCardinal(@WorkBuf[$3C])^ := Hi;
  MD5_Transform(Result, WorkBuf);
end;

function FastMD5(Stream: TStream; const StartPos, EndPos: Int64): TMD5;
const
  deltaSize = $40 * $FFFF;

var
  digest  : TMD5;
  Lo, Hi  : Cardinal;
  DeltaBuf: Pointer;
  BufSiz  : Int64;
  rest    : Cardinal;
  p       : PBYTE;
  WorkLen : Byte;
  WorkBuf : array [0 .. 63] of Byte;
begin
  Lo := 0;
  Hi := 0;
  PCardinal(@digest[0])^ := $67452301;
  PCardinal(@digest[4])^ := $EFCDAB89;
  PCardinal(@digest[8])^ := $98BADCFE;
  PCardinal(@digest[12])^ := $10325476;

  BufSiz := EndPos - StartPos;
  rest := 0;

  if BufSiz shl 3 < 0 then
      Inc(Hi);

  Inc(Lo, BufSiz shl 3);
  Inc(Hi, BufSiz shr 29);

  DeltaBuf := GetMemory(deltaSize);
  Stream.Position := StartPos;

  if BufSiz < $40 then
    begin
      Stream.Read(DeltaBuf^, BufSiz);
      p := DeltaBuf;
    end
  else
    while BufSiz >= $40 do
      begin
        if rest = 0 then
          begin
            if BufSiz >= deltaSize then
                rest := Stream.Read(DeltaBuf^, deltaSize)
            else
                rest := Stream.Read(DeltaBuf^, BufSiz);

            p := DeltaBuf;
          end;
        MD5_Transform(digest, p^);
        Inc(p, $40);
        Dec(BufSiz, $40);
        Dec(rest, $40);
      end;

  if BufSiz > 0 then
      move(p^, WorkBuf[0], BufSiz);

  FreeMemory(DeltaBuf);

  Result := PMD5(@digest[0])^;
  WorkBuf[BufSiz] := $80;
  WorkLen := BufSiz + 1;
  if WorkLen > $38 then
    begin
      if WorkLen < $40 then
          FillChar(WorkBuf[WorkLen], $40 - WorkLen, 0);
      MD5_Transform(Result, WorkBuf);
      WorkLen := 0
    end;
  FillChar(WorkBuf[WorkLen], $38 - WorkLen, 0);
  PCardinal(@WorkBuf[$38])^ := Lo;
  PCardinal(@WorkBuf[$3C])^ := Hi;
  MD5_Transform(Result, WorkBuf);
end;

{$IFEND}

end.
