{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ ****************************************************************************** }

(*
  Optimize.Move -- optimizing System.Move routine for SIMD
  instruction sets at runtime.

  Copyright (C) 2008 Ciobanu Alexandru, MMX/SSE/SSE2/SSE3 versions
  of Move routine originally written by Seth and taken from
  YAWE project: http://code.google.com/p/yawe-mmorpg/

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*)

unit Optimize.Move.Win32;

interface

implementation

{$IFNDEF FPC}
{$IFDEF WIN32}
uses
  Windows, SysUtils;

{ SIMD Detection code }
type
  TSimdInstructionSupport    = (sisMMX, sisSSE, sisSSE2, sisSSE3);
  TSimdInstructionSupportSet = set of TSimdInstructionSupport;

  TCPUIDRegs = record
    rEAX, rEBX, rECX, rEDX: Integer;
  end;

var
  SimdSupported: TSimdInstructionSupportSet;
  CacheLimit   : Integer;
  NbrOfCPUs    : Cardinal;

function CPUID(const FuncId: Integer): TCPUIDRegs;
var
  Regs: TCPUIDRegs;

begin
  { Request the opcode support }
  asm
    { Save registers }
    Push eax
    Push ebx
    Push ecx
    Push edx

    { Set the function 1 and clear others }
    mov eax, [FuncId]
    xor ebx, ebx
    xor ecx, ecx
    xor edx, edx

    { Call CPUID }
    CPUID

    { Store the registers we need }
    mov [Regs.rEAX], eax
    mov [Regs.rEBX], ebx
    mov [Regs.rECX], ecx
    mov [Regs.rEDX], edx

    { Restore registers }
    Pop edx
    Pop ecx
    Pop ebx
    Pop eax
  end;

  Result := Regs;
end;

function CPUIIDSupports(const FuncId: Integer; const Extended: Boolean = False): Boolean;
begin
  if Extended then
      Result := CPUID($80000000).rEAX >= FuncId
  else
      Result := CPUID($00000000).rEAX >= FuncId;
end;

function GetSupportedSimdInstructionSets: TSimdInstructionSupportSet;
var
  Regs: TCPUIDRegs;
begin
  Result := [];

  { Check for iset support }
  if not CPUIIDSupports($00000001) then
      Exit;

  { Request SIMD support }
  Regs := CPUID($00000001);

  if ((Regs.rECX and 1) <> 0) then
      Result := Result + [sisSSE3];

  if ((Regs.rEDX and (1 shl 23)) <> 0) then
      Result := Result + [sisMMX];

  if ((Regs.rEDX and (1 shl 25)) <> 0) then
      Result := Result + [sisSSE];

  if ((Regs.rEDX and (1 shl 26)) <> 0) then
      Result := Result + [sisSSE2];

  {
    Check if Windows supports XMM registers - run an instruction and check
    for exceptions.
  }
  try
    asm
      ORPS XMM0, XMM0
    end
  except
    begin
      { Exclude SSE instructions! }
      Result := Result - [sisSSE, sisSSE2, sisSSE3];
    end;
  end;
end;

function GetL2CacheSize: Integer;
var
  { Variables used for config description }
  Regs: TCPUIDRegs;
  CfgD: packed array [0 .. 15] of Byte absolute Regs;

  i, J      : Integer;
  QueryCount: Byte;
begin
  { Unknown cache size }
  Result := 0;

  { Check for cache support }
  if not CPUIIDSupports($00000002) then
      Exit;

  { Request cache support }
  Regs := CPUID($00000002);

  { Query count }
  QueryCount := Regs.rEAX and $FF;

  for i := 1 to QueryCount do
    begin
      for J := 1 to 15 do
        begin
          case CfgD[J] of
            $39: Result := 128;
            $3B: Result := 128;
            $3C: Result := 256;
            $41: Result := 128;
            $42: Result := 256;
            $43: Result := 512;
            $44: Result := 1024;
            $45: Result := 2048;
            $78: Result := 1024;
            $79: Result := 128;
            $7A: Result := 256;
            $7B: Result := 512;
            $7C: Result := 1024;
            $7D: Result := 2048;
            $7F: Result := 512;
            $82: Result := 256;
            $83: Result := 512;
            $84: Result := 1024;
            $85: Result := 2048;
            $86: Result := 512;
            $87: Result := 1024;
          end;
        end;

      { Re-Request cache support }
      if i < QueryCount then
          Regs := CPUID($00000002);

    end;
end;

function GetExtendedL2CacheSize: Integer;
var
  Regs: TCPUIDRegs;
begin
  Result := 0;

  { Check for cache support }
  if not CPUIIDSupports($80000006, True) then
      Exit;

  { CPUID: $80000005 }
  Regs := CPUID($80000006);

  { L2 Cache size }
  Result := Regs.rECX shr 16;
end;

procedure GetCPUInfo();
var
  CacheSize1, CacheSize2: Integer;
  SysInfo               : TSystemInfo;
begin
  { Detect supported SIMD sets }
  SimdSupported := GetSupportedSimdInstructionSets();

  CacheSize1 := GetL2CacheSize();
  CacheSize2 := GetExtendedL2CacheSize();

  { Get the cache limit }
  if CacheSize2 > 0 then
      CacheLimit := CacheSize2 * -512
  else
      CacheLimit := CacheSize1 * -512;

  { If the cache size is unknown use a hack }
  if CacheLimit = 0 then
      CacheLimit := -$7FFFFFFF;

  { Get core count }
  GetSystemInfo(SysInfo);
  NbrOfCPUs := SysInfo.dwNumberOfProcessors;
end;

function PatchMethod(OldMethod, NewMethod: Pointer): Boolean;
const
  SizeOfJump = 5;

var
  OldFlags: Cardinal;
  __Jump  : ^Byte;
  __Addr  : ^Integer;

begin

  Result := False;

  try
    { Change the code protection to write }
      VirtualProtect(OldMethod, SizeOfJump, PAGE_READWRITE, OldFlags);
  except
    begin Exit;
    end;
  end;

  try
    { Insert the jump instruction }
    __Jump := OldMethod;
    __Jump^ := $E9;

    { Insert the jump address = (OLD - NEW - SIZEOFJUMP) }
    __Addr := PTR(Integer(OldMethod) + 1);
    __Addr^ := Integer(NewMethod) - Integer(OldMethod) - SizeOfJump;
  finally
    { Change the protection back to what it was }
      VirtualProtect(OldMethod, SizeOfJump, OldFlags, OldFlags);
  end;

  { Set status to success }
  Result := True;
end;

const
  TINYSIZE = 36;

procedure SmallForwardMove;
asm
  jmp     DWord PTR [@@FwdJumpTable+ecx*4]
  Nop { Align Jump Table }
@@FwdJumpTable:
  DD      @@Done { Removes need to test for zero size move }
  DD      @@Fwd01, @@Fwd02, @@Fwd03, @@Fwd04, @@Fwd05, @@Fwd06, @@Fwd07, @@Fwd08
  DD      @@Fwd09, @@Fwd10, @@Fwd11, @@Fwd12, @@Fwd13, @@Fwd14, @@Fwd15, @@Fwd16
  DD      @@Fwd17, @@Fwd18, @@Fwd19, @@Fwd20, @@Fwd21, @@Fwd22, @@Fwd23, @@Fwd24
  DD      @@Fwd25, @@Fwd26, @@Fwd27, @@Fwd28, @@Fwd29, @@Fwd30, @@Fwd31, @@Fwd32
  DD      @@Fwd33, @@Fwd34, @@Fwd35, @@Fwd36
@@Fwd36:
  mov     ecx, [eax-36]
  mov     [edx-36], ecx
@@Fwd32:
  mov     ecx, [eax-32]
  mov     [edx-32], ecx
@@Fwd28:
  mov     ecx, [eax-28]
  mov     [edx-28], ecx
@@Fwd24:
  mov     ecx, [eax-24]
  mov     [edx-24], ecx
@@Fwd20:
  mov     ecx, [eax-20]
  mov     [edx-20], ecx
@@Fwd16:
  mov     ecx, [eax-16]
  mov     [edx-16], ecx
@@Fwd12:
  mov     ecx, [eax-12]
  mov     [edx-12], ecx
@@Fwd08:
  mov     ecx, [eax-8]
  mov     [edx-8], ecx
@@Fwd04:
  mov     ecx, [eax-4]
  mov     [edx-4], ecx
  ret
  Nop
@@Fwd35:
  mov     ecx, [eax-35]
  mov     [edx-35], ecx
@@Fwd31:
  mov     ecx, [eax-31]
  mov     [edx-31], ecx
@@Fwd27:
  mov     ecx, [eax-27]
  mov     [edx-27], ecx
@@Fwd23:
  mov     ecx, [eax-23]
  mov     [edx-23], ecx
@@Fwd19:
  mov     ecx, [eax-19]
  mov     [edx-19], ecx
@@Fwd15:
  mov     ecx, [eax-15]
  mov     [edx-15], ecx
@@Fwd11:
  mov     ecx, [eax-11]
  mov     [edx-11], ecx
@@Fwd07:
  mov     ecx, [eax-7]
  mov     [edx-7], ecx
  mov     ecx, [eax-4]
  mov     [edx-4], ecx
  ret
  Nop
@@Fwd03:
  movzx   ecx, Word PTR [eax-3]
  mov     [edx-3], Cx
  movzx   ecx, Byte PTR [eax-1]
  mov     [edx-1], cl
  ret
@@Fwd34:
  mov     ecx, [eax-34]
  mov     [edx-34], ecx
@@Fwd30:
  mov     ecx, [eax-30]
  mov     [edx-30], ecx
@@Fwd26:
  mov     ecx, [eax-26]
  mov     [edx-26], ecx
@@Fwd22:
  mov     ecx, [eax-22]
  mov     [edx-22], ecx
@@Fwd18:
  mov     ecx, [eax-18]
  mov     [edx-18], ecx
@@Fwd14:
  mov     ecx, [eax-14]
  mov     [edx-14], ecx
@@Fwd10:
  mov     ecx, [eax-10]
  mov     [edx-10], ecx
@@Fwd06:
  mov     ecx, [eax-6]
  mov     [edx-6], ecx
@@Fwd02:
  movzx   ecx, Word PTR [eax-2]
  mov     [edx-2], Cx
  ret
  Nop
  Nop
  Nop
@@Fwd33:
  mov     ecx, [eax-33]
  mov     [edx-33], ecx
@@Fwd29:
  mov     ecx, [eax-29]
  mov     [edx-29], ecx
@@Fwd25:
  mov     ecx, [eax-25]
  mov     [edx-25], ecx
@@Fwd21:
  mov     ecx, [eax-21]
  mov     [edx-21], ecx
@@Fwd17:
  mov     ecx, [eax-17]
  mov     [edx-17], ecx
@@Fwd13:
  mov     ecx, [eax-13]
  mov     [edx-13], ecx
@@Fwd09:
  mov     ecx, [eax-9]
  mov     [edx-9], ecx
@@Fwd05:
  mov     ecx, [eax-5]
  mov     [edx-5], ecx
@@Fwd01:
  movzx   ecx, Byte PTR [eax-1]
  mov     [edx-1], cl
  ret
@@Done:
end;

{ ------------------------------------------------------------------------- }
{ Perform Backward Move of 0..36 Bytes }
{ On Entry, ECX = Count, EAX = Source, EDX = Dest.  Destroys ECX }
procedure SmallBackwardMove;
asm
  jmp     DWord PTR [@@BwdJumpTable+ecx*4]
  Nop { Align Jump Table }
@@BwdJumpTable:
  DD      @@Done { Removes need to test for zero size move }
  DD      @@Bwd01, @@Bwd02, @@Bwd03, @@Bwd04, @@Bwd05, @@Bwd06, @@Bwd07, @@Bwd08
  DD      @@Bwd09, @@Bwd10, @@Bwd11, @@Bwd12, @@Bwd13, @@Bwd14, @@Bwd15, @@Bwd16
  DD      @@Bwd17, @@Bwd18, @@Bwd19, @@Bwd20, @@Bwd21, @@Bwd22, @@Bwd23, @@Bwd24
  DD      @@Bwd25, @@Bwd26, @@Bwd27, @@Bwd28, @@Bwd29, @@Bwd30, @@Bwd31, @@Bwd32
  DD      @@Bwd33, @@Bwd34, @@Bwd35, @@Bwd36
@@Bwd36:
  mov     ecx, [eax+32]
  mov     [edx+32], ecx
@@Bwd32:
  mov     ecx, [eax+28]
  mov     [edx+28], ecx
@@Bwd28:
  mov     ecx, [eax+24]
  mov     [edx+24], ecx
@@Bwd24:
  mov     ecx, [eax+20]
  mov     [edx+20], ecx
@@Bwd20:
  mov     ecx, [eax+16]
  mov     [edx+16], ecx
@@Bwd16:
  mov     ecx, [eax+12]
  mov     [edx+12], ecx
@@Bwd12:
  mov     ecx, [eax+8]
  mov     [edx+8], ecx
@@Bwd08:
  mov     ecx, [eax+4]
  mov     [edx+4], ecx
@@Bwd04:
  mov     ecx, [eax]
  mov     [edx], ecx
  ret
  Nop
  Nop
  Nop
@@Bwd35:
  mov     ecx, [eax+31]
  mov     [edx+31], ecx
@@Bwd31:
  mov     ecx, [eax+27]
  mov     [edx+27], ecx
@@Bwd27:
  mov     ecx, [eax+23]
  mov     [edx+23], ecx
@@Bwd23:
  mov     ecx, [eax+19]
  mov     [edx+19], ecx
@@Bwd19:
  mov     ecx, [eax+15]
  mov     [edx+15], ecx
@@Bwd15:
  mov     ecx, [eax+11]
  mov     [edx+11], ecx
@@Bwd11:
  mov     ecx, [eax+7]
  mov     [edx+7], ecx
@@Bwd07:
  mov     ecx, [eax+3]
  mov     [edx+3], ecx
  mov     ecx, [eax]
  mov     [edx], ecx
  ret
  Nop
  Nop
  Nop
@@Bwd03:
  movzx   ecx, Word PTR [eax+1]
  mov     [edx+1], Cx
  movzx   ecx, Byte PTR [eax]
  mov     [edx], cl
  ret
  Nop
  Nop
@@Bwd34:
  mov     ecx, [eax+30]
  mov     [edx+30], ecx
@@Bwd30:
  mov     ecx, [eax+26]
  mov     [edx+26], ecx
@@Bwd26:
  mov     ecx, [eax+22]
  mov     [edx+22], ecx
@@Bwd22:
  mov     ecx, [eax+18]
  mov     [edx+18], ecx
@@Bwd18:
  mov     ecx, [eax+14]
  mov     [edx+14], ecx
@@Bwd14:
  mov     ecx, [eax+10]
  mov     [edx+10], ecx
@@Bwd10:
  mov     ecx, [eax+6]
  mov     [edx+6], ecx
@@Bwd06:
  mov     ecx, [eax+2]
  mov     [edx+2], ecx
@@Bwd02:
  movzx   ecx, Word PTR [eax]
  mov     [edx], Cx
  ret
  Nop
@@Bwd33:
  mov     ecx, [eax+29]
  mov     [edx+29], ecx
@@Bwd29:
  mov     ecx, [eax+25]
  mov     [edx+25], ecx
@@Bwd25:
  mov     ecx, [eax+21]
  mov     [edx+21], ecx
@@Bwd21:
  mov     ecx, [eax+17]
  mov     [edx+17], ecx
@@Bwd17:
  mov     ecx, [eax+13]
  mov     [edx+13], ecx
@@Bwd13:
  mov     ecx, [eax+9]
  mov     [edx+9], ecx
@@Bwd09:
  mov     ecx, [eax+5]
  mov     [edx+5], ecx
@@Bwd05:
  mov     ecx, [eax+1]
  mov     [edx+1], ecx
@@Bwd01:
  movzx   ecx, Byte PTR[eax]
  mov     [edx], cl
  ret
@@Done:
end;

{ ------------------------------------------------------------------------- }
{ Move ECX Bytes from EAX to EDX, where EAX > EDX and ECX > 36 (TINYSIZE) }
procedure Forwards_IA32;
asm
  fild    qword PTR [eax] { First 8 }
  lea     eax, [eax+ecx-8]
  lea     ecx, [edx+ecx-8]
  Push    edx
  Push    ecx
  fild    qword PTR [eax] { Last 8 }
  Neg     ecx             { QWORD Align Writes }
  and     edx, -8
  lea     ecx, [ecx+edx+8]
  Pop     edx
@@Loop:
  fild    qword PTR [eax+ecx]
  fistp   qword PTR [edx+ecx]
  Add     ecx, 8
  jl      @@Loop
  Pop     eax
  fistp   qword PTR [edx] { Last 8 }
  fistp   qword PTR [eax] { First 8 }
end;

{ ------------------------------------------------------------------------- }
{ Move ECX Bytes from EAX to EDX, where EAX < EDX and ECX > 36 (TINYSIZE) }
procedure Backwards_IA32;
asm
  Sub     ecx, 8
  fild    qword PTR [eax+ecx] { Last 8 }
  fild    qword PTR [eax]     { First 8 }
  Add     ecx, edx            { QWORD Align Writes }
  Push    ecx
  and     ecx, -8
  Sub     ecx, edx
@@Loop:
  fild    qword PTR [eax+ecx]
  fistp   qword PTR [edx+ecx]
  Sub     ecx, 8
  jg      @@Loop
  Pop     eax
  fistp   qword PTR [edx] { First 8 }
  fistp   qword PTR [eax] { Last 8 }
end;

{ ------------------------------------------------------------------------- }
{ Move ECX Bytes from EAX to EDX, where EAX > EDX and ECX > 36 (TINYSIZE) }
procedure Forwards_MMX;
const
  SMALLSIZE = 64;
  LARGESIZE = 2048;
  asm
    cmp     ecx, SMALLSIZE { Size at which using MMX becomes worthwhile }
    jl      Forwards_IA32
    cmp     ecx, LARGESIZE
    jge     @@FwdLargeMove
    Push    ebx
    mov     ebx, edx
    movq    MM0, [eax] { First 8 Bytes }
    Add     eax, ecx   { QWORD Align Writes }
    Add     ecx, edx
    and     edx, -8
    Add     edx, 40
    Sub     ecx, edx
    Add     edx, ecx
    Neg     ecx
    Nop { Align Loop }
  @@FwdLoopMMX:
    movq    MM1, [eax+ecx-32]
    movq    mm2, [eax+ecx-24]
    movq    mm3, [eax+ecx-16]
    movq    mm4, [eax+ecx- 8]
    movq    [edx+ecx-32], MM1
    movq    [edx+ecx-24], mm2
    movq    [edx+ecx-16], mm3
    movq    [edx+ecx- 8], mm4
    Add     ecx, 32
    jle     @@FwdLoopMMX
    movq    [ebx], MM0 { First 8 Bytes }
    emms
    Pop     ebx
    Neg     ecx
    Add     ecx, 32
    jmp     SmallForwardMove
    Nop { Align Loop }
    Nop
  @@FwdLargeMove:
    Push    ebx
    mov     ebx, ecx
    Test    edx, 15
    jz      @@FwdAligned
    lea     ecx, [edx+15] { 16 byte Align Destination }
    and     ecx, -16
    Sub     ecx, edx
    Add     eax, ecx
    Add     edx, ecx
    Sub     ebx, ecx
    call    SmallForwardMove
  @@FwdAligned:
    mov     ecx, ebx
    and     ecx, -16
    Sub     ebx, ecx { EBX = Remainder }
    Push    esi
    Push    edi
    mov     esi, eax          { ESI = Source }
    mov     edi, edx          { EDI = Dest }
    mov     eax, ecx          { EAX = Count }
    and     eax, -64          { EAX = No of Bytes to Blocks Moves }
    and     ecx, $3F          { ECX = Remaining Bytes to Move (0..63) }
    Add     esi, eax
    Add     edi, eax
    Neg     eax
  @@MMXcopyloop:
    movq    MM0, [esi+eax   ]
    movq    MM1, [esi+eax+ 8]
    movq    mm2, [esi+eax+16]
    movq    mm3, [esi+eax+24]
    movq    mm4, [esi+eax+32]
    movq    mm5, [esi+eax+40]
    movq    mm6, [esi+eax+48]
    movq    mm7, [esi+eax+56]
    movq    [edi+eax   ], MM0
    movq    [edi+eax+ 8], MM1
    movq    [edi+eax+16], mm2
    movq    [edi+eax+24], mm3
    movq    [edi+eax+32], mm4
    movq    [edi+eax+40], mm5
    movq    [edi+eax+48], mm6
    movq    [edi+eax+56], mm7
    Add     eax, 64
    jnz     @@MMXcopyloop
    emms                   { Empty MMX State }
    Add     ecx, ebx
    shr     ecx, 2
    Rep     movsd
    mov     ecx, ebx
    and     ecx, 3
    Rep     movsb
    Pop     edi
    Pop     esi
    Pop     ebx
end;

{ ------------------------------------------------------------------------- }
{ Move ECX Bytes from EAX to EDX, where EAX < EDX and ECX > 36 (TINYSIZE) }
procedure Backwards_MMX;
const
  SMALLSIZE = 64;
  asm
    cmp     ecx, SMALLSIZE { Size at which using MMX becomes worthwhile }
    jl      Backwards_IA32
    Push    ebx
    movq    MM0, [eax+ecx-8] { Get Last QWORD }
    lea     ebx, [edx+ecx]   { QWORD Align Writes }
    and     ebx, 7
    Sub     ecx, ebx
    Add     ebx, ecx
    Sub     ecx, 32
  @@BwdLoopMMX:
    movq    MM1, [eax+ecx   ]
    movq    mm2, [eax+ecx+ 8]
    movq    mm3, [eax+ecx+16]
    movq    mm4, [eax+ecx+24]
    movq    [edx+ecx+24], mm4
    movq    [edx+ecx+16], mm3
    movq    [edx+ecx+ 8], mm2
    movq    [edx+ecx   ], MM1
    Sub     ecx, 32
    jge     @@BwdLoopMMX
    movq    [edx+ebx-8], MM0 { Last QWORD }
    emms
    Add     ecx, 32
    Pop     ebx
    jmp     SmallBackwardMove
end;

{ ------------------------------------------------------------------------- }
procedure LargeAlignedSSEMove;
asm
@@Loop:
  movaps  XMM0, [eax+ecx]
  movaps  xmm1, [eax+ecx+16]
  movaps  xmm2, [eax+ecx+32]
  movaps  xmm3, [eax+ecx+48]
  movaps  [edx+ecx], XMM0
  movaps  [edx+ecx+16], xmm1
  movaps  [edx+ecx+32], xmm2
  movaps  [edx+ecx+48], xmm3
  movaps  xmm4, [eax+ecx+64]
  movaps  xmm5, [eax+ecx+80]
  movaps  xmm6, [eax+ecx+96]
  movaps  xmm7, [eax+ecx+112]
  movaps  [edx+ecx+64], xmm4
  movaps  [edx+ecx+80], xmm5
  movaps  [edx+ecx+96], xmm6
  movaps  [edx+ecx+112], xmm7
  Add     ecx, 128
  js      @@Loop
end;

{ ------------------------------------------------------------------------- }
procedure LargeUnalignedSSEMove;
asm
@@Loop:
  movups  XMM0, [eax+ecx]
  movups  xmm1, [eax+ecx+16]
  movups  xmm2, [eax+ecx+32]
  movups  xmm3, [eax+ecx+48]
  movaps  [edx+ecx], XMM0
  movaps  [edx+ecx+16], xmm1
  movaps  [edx+ecx+32], xmm2
  movaps  [edx+ecx+48], xmm3
  movups  xmm4, [eax+ecx+64]
  movups  xmm5, [eax+ecx+80]
  movups  xmm6, [eax+ecx+96]
  movups  xmm7, [eax+ecx+112]
  movaps  [edx+ecx+64], xmm4
  movaps  [edx+ecx+80], xmm5
  movaps  [edx+ecx+96], xmm6
  movaps  [edx+ecx+112], xmm7
  Add     ecx, 128
  js      @@Loop
end;

{ ------------------------------------------------------------------------- }
procedure HugeAlignedSSEMove;
const
  Prefetch = 512;
  asm
  @@Loop:
    prefetchnta [eax+ecx+Prefetch]
    prefetchnta [eax+ecx+Prefetch+64]
    movaps  XMM0, [eax+ecx]
    movaps  xmm1, [eax+ecx+16]
    movaps  xmm2, [eax+ecx+32]
    movaps  xmm3, [eax+ecx+48]
    movntps [edx+ecx], XMM0
    movntps [edx+ecx+16], xmm1
    movntps [edx+ecx+32], xmm2
    movntps [edx+ecx+48], xmm3
    movaps  xmm4, [eax+ecx+64]
    movaps  xmm5, [eax+ecx+80]
    movaps  xmm6, [eax+ecx+96]
    movaps  xmm7, [eax+ecx+112]
    movntps [edx+ecx+64], xmm4
    movntps [edx+ecx+80], xmm5
    movntps [edx+ecx+96], xmm6
    movntps [edx+ecx+112], xmm7
    Add     ecx, 128
    js      @@Loop
    sfence
end;

{ ------------------------------------------------------------------------- }
procedure HugeUnalignedSSEMove;
const
  Prefetch = 512;
  asm
  @@Loop:
    prefetchnta [eax+ecx+Prefetch]
    prefetchnta [eax+ecx+Prefetch+64]
    movups  XMM0, [eax+ecx]
    movups  xmm1, [eax+ecx+16]
    movups  xmm2, [eax+ecx+32]
    movups  xmm3, [eax+ecx+48]
    movntps [edx+ecx], XMM0
    movntps [edx+ecx+16], xmm1
    movntps [edx+ecx+32], xmm2
    movntps [edx+ecx+48], xmm3
    movups  xmm4, [eax+ecx+64]
    movups  xmm5, [eax+ecx+80]
    movups  xmm6, [eax+ecx+96]
    movups  xmm7, [eax+ecx+112]
    movntps [edx+ecx+64], xmm4
    movntps [edx+ecx+80], xmm5
    movntps [edx+ecx+96], xmm6
    movntps [edx+ecx+112], xmm7
    Add     ecx, 128
    js      @@Loop
    sfence
end;

{ ------------------------------------------------------------------------- }
{ Dest MUST be 16-Byes Aligned, Count MUST be multiple of 16 }
procedure LargeSSEMove;
asm
  Push    ebx
  mov     ebx, ecx
  and     ecx, -128             { No of Bytes to Block Move (Multiple of 128) }
  Add     eax, ecx              { End of Source Blocks }
  Add     edx, ecx              { End of Dest Blocks }
  Neg     ecx
  cmp     ecx, CacheLimit       { Count > Limit - Use Prefetch }
  jl      @@Huge
  Test    eax, 15               { Check if Both Source/Dest are Aligned }
  jnz     @@LargeUnaligned
  call    LargeAlignedSSEMove   { Both Source and Dest 16-Byte Aligned }
  jmp     @@Remainder
@@LargeUnaligned:               { Source Not 16-Byte Aligned }
  call    LargeUnalignedSSEMove
  jmp     @@Remainder
@@Huge:
  Test    eax, 15               { Check if Both Source/Dest Aligned }
  jnz     @@HugeUnaligned
  call    HugeAlignedSSEMove    { Both Source and Dest 16-Byte Aligned }
  jmp     @@Remainder
@@HugeUnaligned:                { Source Not 16-Byte Aligned }
  call    HugeUnalignedSSEMove
@@Remainder:
  and     ebx, $7F              { Remainder (0..112 - Multiple of 16) }
  jz      @@Done
  Add     eax, ebx
  Add     edx, ebx
  Neg     ebx
@@RemainderLoop:
  movups  XMM0, [eax+ebx]
  movaps  [edx+ebx], XMM0
  Add     ebx, 16
  jnz     @@RemainderLoop
@@Done:
  Pop     ebx
end;

{ ------------------------------------------------------------------------- }
{ Move ECX Bytes from EAX to EDX, where EAX > EDX and ECX > 36 (TINYSIZE) }
procedure Forwards_SSE;
const
  SMALLSIZE = 64;
  LARGESIZE = 2048;
  asm
    cmp     ecx, SMALLSIZE
    jle     Forwards_IA32
    Push    ebx
    cmp     ecx, LARGESIZE
    jge     @@FwdLargeMove
    movups  XMM0, [eax] { First 16 Bytes }
    mov     ebx, edx
    Add     eax, ecx { Align Writes }
    Add     ecx, edx
    and     edx, -16
    Add     edx, 48
    Sub     ecx, edx
    Add     edx, ecx
    Neg     ecx
    Nop { Align Loop }
  @@FwdLoopSSE:
    movups  xmm1, [eax+ecx-32]
    movups  xmm2, [eax+ecx-16]
    movaps  [edx+ecx-32], xmm1
    movaps  [edx+ecx-16], xmm2
    Add     ecx, 32
    jle     @@FwdLoopSSE
    movups  [ebx], XMM0 { First 16 Bytes }
    Neg     ecx
    Add     ecx, 32
    Pop     ebx
    jmp     SmallForwardMove
  @@FwdLargeMove:
    mov     ebx, ecx
    Test    edx, 15
    jz      @@FwdLargeAligned
    lea     ecx, [edx+15] { 16 byte Align Destination }
    and     ecx, -16
    Sub     ecx, edx
    Add     eax, ecx
    Add     edx, ecx
    Sub     ebx, ecx
    call    SmallForwardMove
    mov     ecx, ebx
  @@FwdLargeAligned:
    and     ecx, -16
    Sub     ebx, ecx { EBX = Remainder }
    Push    edx
    Push    eax
    Push    ecx
    call    LargeSSEMove
    Pop     ecx
    Pop     eax
    Pop     edx
    Add     ecx, ebx
    Add     eax, ecx
    Add     edx, ecx
    mov     ecx, ebx
    Pop     ebx
    jmp     SmallForwardMove
end;

{ ------------------------------------------------------------------------- }
{ Move ECX Bytes from EAX to EDX, where EAX < EDX and ECX > 36 (TINYSIZE) }
procedure Backwards_SSE;
const
  SMALLSIZE = 64;
  asm
    cmp     ecx, SMALLSIZE
    jle     Backwards_IA32
    Push    ebx
    movups  XMM0, [eax+ecx-16] { Last 16 Bytes }
    lea     ebx, [edx+ecx]     { Align Writes }
    and     ebx, 15
    Sub     ecx, ebx
    Add     ebx, ecx
    Sub     ecx, 32
  @@BwdLoop:
    movups  xmm1, [eax+ecx]
    movups  xmm2, [eax+ecx+16]
    movaps  [edx+ecx], xmm1
    movaps  [edx+ecx+16], xmm2
    Sub     ecx, 32
    jge     @@BwdLoop
    movups  [edx+ebx-16], XMM0  { Last 16 Bytes }
    Add     ecx, 32
    Pop     ebx
    jmp     SmallBackwardMove
end;

{ ------------------------------------------------------------------------- }
procedure LargeAlignedSSE2Move; { Also used in SSE3 Move }
asm
@@Loop:
  movdqa  XMM0, [eax+ecx]
  movdqa  xmm1, [eax+ecx+16]
  movdqa  xmm2, [eax+ecx+32]
  movdqa  xmm3, [eax+ecx+48]
  movdqa  [edx+ecx], XMM0
  movdqa  [edx+ecx+16], xmm1
  movdqa  [edx+ecx+32], xmm2
  movdqa  [edx+ecx+48], xmm3
  movdqa  xmm4, [eax+ecx+64]
  movdqa  xmm5, [eax+ecx+80]
  movdqa  xmm6, [eax+ecx+96]
  movdqa  xmm7, [eax+ecx+112]
  movdqa  [edx+ecx+64], xmm4
  movdqa  [edx+ecx+80], xmm5
  movdqa  [edx+ecx+96], xmm6
  movdqa  [edx+ecx+112], xmm7
  Add     ecx, 128
  js      @@Loop
end;

{ ------------------------------------------------------------------------- }
procedure LargeUnalignedSSE2Move;
asm
@@Loop:
  movdqu  XMM0, [eax+ecx]
  movdqu  xmm1, [eax+ecx+16]
  movdqu  xmm2, [eax+ecx+32]
  movdqu  xmm3, [eax+ecx+48]
  movdqa  [edx+ecx], XMM0
  movdqa  [edx+ecx+16], xmm1
  movdqa  [edx+ecx+32], xmm2
  movdqa  [edx+ecx+48], xmm3
  movdqu  xmm4, [eax+ecx+64]
  movdqu  xmm5, [eax+ecx+80]
  movdqu  xmm6, [eax+ecx+96]
  movdqu  xmm7, [eax+ecx+112]
  movdqa  [edx+ecx+64], xmm4
  movdqa  [edx+ecx+80], xmm5
  movdqa  [edx+ecx+96], xmm6
  movdqa  [edx+ecx+112], xmm7
  Add     ecx, 128
  js      @@Loop
end;

{ ------------------------------------------------------------------------- }
procedure HugeAlignedSSE2Move; { Also used in SSE3 Move }
const
  Prefetch = 512;
  asm
  @@Loop:
    prefetchnta [eax+ecx+Prefetch]
    prefetchnta [eax+ecx+Prefetch+64]
    movdqa  XMM0, [eax+ecx]
    movdqa  xmm1, [eax+ecx+16]
    movdqa  xmm2, [eax+ecx+32]
    movdqa  xmm3, [eax+ecx+48]
    movntdq [edx+ecx], XMM0
    movntdq [edx+ecx+16], xmm1
    movntdq [edx+ecx+32], xmm2
    movntdq [edx+ecx+48], xmm3
    movdqa  xmm4, [eax+ecx+64]
    movdqa  xmm5, [eax+ecx+80]
    movdqa  xmm6, [eax+ecx+96]
    movdqa  xmm7, [eax+ecx+112]
    movntdq [edx+ecx+64], xmm4
    movntdq [edx+ecx+80], xmm5
    movntdq [edx+ecx+96], xmm6
    movntdq [edx+ecx+112], xmm7
    Add     ecx, 128
    js      @@Loop
    sfence
end;

{ ------------------------------------------------------------------------- }
procedure HugeUnalignedSSE2Move;
const
  Prefetch = 512;
  asm
  @@Loop:
    prefetchnta [eax+ecx+Prefetch]
    prefetchnta [eax+ecx+Prefetch+64]
    movdqu  XMM0, [eax+ecx]
    movdqu  xmm1, [eax+ecx+16]
    movdqu  xmm2, [eax+ecx+32]
    movdqu  xmm3, [eax+ecx+48]
    movntdq [edx+ecx], XMM0
    movntdq [edx+ecx+16], xmm1
    movntdq [edx+ecx+32], xmm2
    movntdq [edx+ecx+48], xmm3
    movdqu  xmm4, [eax+ecx+64]
    movdqu  xmm5, [eax+ecx+80]
    movdqu  xmm6, [eax+ecx+96]
    movdqu  xmm7, [eax+ecx+112]
    movntdq [edx+ecx+64], xmm4
    movntdq [edx+ecx+80], xmm5
    movntdq [edx+ecx+96], xmm6
    movntdq [edx+ecx+112], xmm7
    Add     ecx, 128
    js      @@Loop
    sfence
end; { HugeUnalignedSSE2Move }

{ ------------------------------------------------------------------------- }
{ Dest MUST be 16-Byes Aligned, Count MUST be multiple of 16 }
procedure LargeSSE2Move;
asm
  Push    ebx
  mov     ebx, ecx
  and     ecx, -128             { No of Bytes to Block Move (Multiple of 128) }
  Add     eax, ecx              { End of Source Blocks }
  Add     edx, ecx              { End of Dest Blocks }
  Neg     ecx
  cmp     ecx, CacheLimit       { Count > Limit - Use Prefetch }
  jl      @@Huge
  Test    eax, 15               { Check if Both Source/Dest are Aligned }
  jnz     @@LargeUnaligned
  call    LargeAlignedSSE2Move  { Both Source and Dest 16-Byte Aligned }
  jmp     @@Remainder
@@LargeUnaligned:               { Source Not 16-Byte Aligned }
  call    LargeUnalignedSSE2Move
  jmp     @@Remainder
@@Huge:
  Test    eax, 15               { Check if Both Source/Dest Aligned }
  jnz     @@HugeUnaligned
  call    HugeAlignedSSE2Move   { Both Source and Dest 16-Byte Aligned }
  jmp     @@Remainder
@@HugeUnaligned:                { Source Not 16-Byte Aligned }
  call    HugeUnalignedSSE2Move
@@Remainder:
  and     ebx, $7F              { Remainder (0..112 - Multiple of 16) }
  jz      @@Done
  Add     eax, ebx
  Add     edx, ebx
  Neg     ebx
@@RemainderLoop:
  movdqu  XMM0, [eax+ebx]
  movdqa  [edx+ebx], XMM0
  Add     ebx, 16
  jnz     @@RemainderLoop
@@Done:
  Pop     ebx
end;

{ ------------------------------------------------------------------------- }
{ Move ECX Bytes from EAX to EDX, where EAX > EDX and ECX > 36 (TINYSIZE) }
procedure Forwards_SSE2;
const
  SMALLSIZE = 64;
  LARGESIZE = 2048;
  asm
    cmp     ecx, SMALLSIZE
    jle     Forwards_IA32
    Push    ebx
    cmp     ecx, LARGESIZE
    jge     @@FwdLargeMove
    movdqu  XMM0, [eax] { First 16 Bytes }
    mov     ebx, edx
    Add     eax, ecx { Align Writes }
    Add     ecx, edx
    and     edx, -16
    Add     edx, 48
    Sub     ecx, edx
    Add     edx, ecx
    Neg     ecx
  @@FwdLoopSSE2:
    movdqu  xmm1, [eax+ecx-32]
    movdqu  xmm2, [eax+ecx-16]
    movdqa  [edx+ecx-32], xmm1
    movdqa  [edx+ecx-16], xmm2
    Add     ecx, 32
    jle     @@FwdLoopSSE2
    movdqu  [ebx], XMM0 { First 16 Bytes }
    Neg     ecx
    Add     ecx, 32
    Pop     ebx
    jmp     SmallForwardMove
  @@FwdLargeMove:
    mov     ebx, ecx
    Test    edx, 15
    jz      @@FwdLargeAligned
    lea     ecx, [edx+15] { 16 byte Align Destination }
    and     ecx, -16
    Sub     ecx, edx
    Add     eax, ecx
    Add     edx, ecx
    Sub     ebx, ecx
    call    SmallForwardMove
    mov     ecx, ebx
  @@FwdLargeAligned:
    and     ecx, -16
    Sub     ebx, ecx { EBX = Remainder }
    Push    edx
    Push    eax
    Push    ecx
    call    LargeSSE2Move
    Pop     ecx
    Pop     eax
    Pop     edx
    Add     ecx, ebx
    Add     eax, ecx
    Add     edx, ecx
    mov     ecx, ebx
    Pop     ebx
    jmp     SmallForwardMove
end;

{ ------------------------------------------------------------------------- }
{ Move ECX Bytes from EAX to EDX, where EAX < EDX and ECX > 36 (TINYSIZE) }
procedure Backwards_SSE2;
const
  SMALLSIZE = 64;
  asm
    cmp     ecx, SMALLSIZE
    jle     Backwards_IA32
    Push    ebx
    movdqu  XMM0, [eax+ecx-16] { Last 16 Bytes }
    lea     ebx, [edx+ecx]     { Align Writes }
    and     ebx, 15
    Sub     ecx, ebx
    Add     ebx, ecx
    Sub     ecx, 32
    Add     edi, 0 { 3-Byte NOP Equivalent to Align Loop }
  @@BwdLoop:
    movdqu  xmm1, [eax+ecx]
    movdqu  xmm2, [eax+ecx+16]
    movdqa  [edx+ecx], xmm1
    movdqa  [edx+ecx+16], xmm2
    Sub     ecx, 32
    jge     @@BwdLoop
    movdqu  [edx+ebx-16], XMM0  { Last 16 Bytes }
    Add     ecx, 32
    Pop     ebx
    jmp     SmallBackwardMove
end;

{ ------------------------------------------------------------------------- }
procedure LargeUnalignedSSE3Move;
asm
@@Loop:
  lddqu   XMM0, [eax+ecx]
  lddqu   xmm1, [eax+ecx+16]
  lddqu   xmm2, [eax+ecx+32]
  lddqu   xmm3, [eax+ecx+48]
  movdqa  [edx+ecx], XMM0
  movdqa  [edx+ecx+16], xmm1
  movdqa  [edx+ecx+32], xmm2
  movdqa  [edx+ecx+48], xmm3
  lddqu   xmm4, [eax+ecx+64]
  lddqu   xmm5, [eax+ecx+80]
  lddqu   xmm6, [eax+ecx+96]
  lddqu   xmm7, [eax+ecx+112]
  movdqa  [edx+ecx+64], xmm4
  movdqa  [edx+ecx+80], xmm5
  movdqa  [edx+ecx+96], xmm6
  movdqa  [edx+ecx+112], xmm7
  Add     ecx, 128
  js      @@Loop
end;

{ ------------------------------------------------------------------------- }
procedure HugeUnalignedSSE3Move;
const
  Prefetch = 512;
  asm
  @@Loop:
    prefetchnta [eax+ecx+Prefetch]
    prefetchnta [eax+ecx+Prefetch+64]
    lddqu   XMM0, [eax+ecx]
    lddqu   xmm1, [eax+ecx+16]
    lddqu   xmm2, [eax+ecx+32]
    lddqu   xmm3, [eax+ecx+48]
    movntdq [edx+ecx], XMM0
    movntdq [edx+ecx+16], xmm1
    movntdq [edx+ecx+32], xmm2
    movntdq [edx+ecx+48], xmm3
    lddqu   xmm4, [eax+ecx+64]
    lddqu   xmm5, [eax+ecx+80]
    lddqu   xmm6, [eax+ecx+96]
    lddqu   xmm7, [eax+ecx+112]
    movntdq [edx+ecx+64], xmm4
    movntdq [edx+ecx+80], xmm5
    movntdq [edx+ecx+96], xmm6
    movntdq [edx+ecx+112], xmm7
    Add     ecx, 128
    js      @@Loop
    sfence
end;

{ ------------------------------------------------------------------------- }
{ Dest MUST be 16-Byes Aligned, Count MUST be multiple of 16 }
procedure LargeSSE3Move(const Source; var dest; Count: Integer);
asm
  Push    ebx
  mov     ebx, ecx
  and     ecx, -128             { No of Bytes to Block Move (Multiple of 128) }
  Add     eax, ecx              { End of Source Blocks }
  Add     edx, ecx              { End of Dest Blocks }
  Neg     ecx
  cmp     ecx, CacheLimit       { Count > Limit - Use Prefetch }
  jl      @@Huge
  Test    eax, 15               { Check if Both Source/Dest are Aligned }
  jnz     @@LargeUnaligned
  call    LargeAlignedSSE2Move  { Both Source and Dest 16-Byte Aligned }
  jmp     @@Remainder
@@LargeUnaligned:               { Source Not 16-Byte Aligned }
  call    LargeUnalignedSSE3Move
  jmp     @@Remainder
@@Huge:
  Test    eax, 15               { Check if Both Source/Dest Aligned }
  jnz     @@HugeUnaligned
  call    HugeAlignedSSE2Move   { Both Source and Dest 16-Byte Aligned }
  jmp     @@Remainder
@@HugeUnaligned:                { Source Not 16-Byte Aligned }
  call    HugeUnalignedSSE3Move
@@Remainder:
  and     ebx, $7F              { Remainder (0..112 - Multiple of 16) }
  jz      @@Done
  Add     eax, ebx
  Add     edx, ebx
  Neg     ebx
@@RemainderLoop:
  lddqu   XMM0, [eax+ebx]
  movdqa  [edx+ebx], XMM0
  Add     ebx, 16
  jnz     @@RemainderLoop
@@Done:
  Pop     ebx
end;

{ ------------------------------------------------------------------------- }
{ Move ECX Bytes from EAX to EDX, where EAX > EDX and ECX > 36 (TINYSIZE) }
procedure Forwards_SSE3;
const
  SMALLSIZE = 64;
  LARGESIZE = 2048;
  asm
    cmp     ecx, SMALLSIZE
    jle     Forwards_IA32
    Push    ebx
    cmp     ecx, LARGESIZE
    jge     @@FwdLargeMove
    lddqu   XMM0, [eax] { First 16 Bytes }
    mov     ebx, edx
    Add     eax, ecx { Align Writes }
    Add     ecx, edx
    and     edx, -16
    Add     edx, 48
    Sub     ecx, edx
    Add     edx, ecx
    Neg     ecx
  @@FwdLoopSSE3:
    lddqu   xmm1, [eax+ecx-32]
    lddqu   xmm2, [eax+ecx-16]
    movdqa  [edx+ecx-32], xmm1
    movdqa  [edx+ecx-16], xmm2
    Add     ecx, 32
    jle     @@FwdLoopSSE3
    movdqu  [ebx], XMM0 { First 16 Bytes }
    Neg     ecx
    Add     ecx, 32
    Pop     ebx
    jmp     SmallForwardMove
  @@FwdLargeMove:
    mov     ebx, ecx
    Test    edx, 15
    jz      @@FwdLargeAligned
    lea     ecx, [edx+15] { 16 byte Align Destination }
    and     ecx, -16
    Sub     ecx, edx
    Add     eax, ecx
    Add     edx, ecx
    Sub     ebx, ecx
    call    SmallForwardMove
    mov     ecx, ebx
  @@FwdLargeAligned:
    and     ecx, -16
    Sub     ebx, ecx { EBX = Remainder }
    Push    edx
    Push    eax
    Push    ecx
    call    LargeSSE3Move
    Pop     ecx
    Pop     eax
    Pop     edx
    Add     ecx, ebx
    Add     eax, ecx
    Add     edx, ecx
    mov     ecx, ebx
    Pop     ebx
    jmp     SmallForwardMove
end;

{ ------------------------------------------------------------------------- }
{ Move ECX Bytes from EAX to EDX, where EAX < EDX and ECX > 36 (TINYSIZE) }
procedure Backwards_SSE3;
const
  SMALLSIZE = 64;
  asm
    cmp     ecx, SMALLSIZE
    jle     Backwards_IA32
    Push    ebx
    lddqu   XMM0, [eax+ecx-16] { Last 16 Bytes }
    lea     ebx, [edx+ecx]     { Align Writes }
    and     ebx, 15
    Sub     ecx, ebx
    Add     ebx, ecx
    Sub     ecx, 32
    Add     edi, 0 { 3-Byte NOP Equivalent to Align Loop }
  @@BwdLoop:
    lddqu   xmm1, [eax+ecx]
    lddqu   xmm2, [eax+ecx+16]
    movdqa  [edx+ecx], xmm1
    movdqa  [edx+ecx+16], xmm2
    Sub     ecx, 32
    jge     @@BwdLoop
    movdqu  [edx+ebx-16], XMM0  { Last 16 Bytes }
    Add     ecx, 32
    Pop     ebx
    jmp     SmallBackwardMove
end;

procedure MoveMMX(const Source; var dest; Count: Integer);
asm
  cmp     ecx, TINYSIZE
  ja      @@Large { Count > TINYSIZE or Count < 0 }
  cmp     eax, edx
  jbe     @@SmallCheck
  Add     eax, ecx
  Add     edx, ecx
  jmp     SmallForwardMove
@@SmallCheck:
  jne     SmallBackwardMove
  ret { For Compatibility with Delphi's move for Source = Dest }
@@Large:
  jng     @@Done { For Compatibility with Delphi's move for Count < 0 }
  cmp     eax, edx
  ja      Forwards_MMX
  JE      @@Done { For Compatibility with Delphi's move for Source = Dest }
  Sub     edx, ecx
  cmp     eax, edx
  lea     edx, [edx+ecx]
  jna     Forwards_MMX
  jmp     Backwards_MMX { Source/Dest Overlap }
@@Done:
end;

procedure MoveSSE(const Source; var dest; Count: Integer);
asm
  cmp     ecx, TINYSIZE
  ja      @@Large { Count > TINYSIZE or Count < 0 }
  cmp     eax, edx
  jbe     @@SmallCheck
  Add     eax, ecx
  Add     edx, ecx
  jmp     SmallForwardMove
@@SmallCheck:
  jne     SmallBackwardMove
  ret { For Compatibility with Delphi's move for Source = Dest }
@@Large:
  jng     @@Done { For Compatibility with Delphi's move for Count < 0 }
  cmp     eax, edx
  ja      Forwards_SSE
  JE      @@Done { For Compatibility with Delphi's move for Source = Dest }
  Sub     edx, ecx
  cmp     eax, edx
  lea     edx, [edx+ecx]
  jna     Forwards_SSE
  jmp     Backwards_SSE { Source/Dest Overlap }
@@Done:
end;

procedure MoveSSE2(const Source; var dest; Count: Integer);
asm
  cmp     ecx, TINYSIZE
  ja      @@Large { Count > TINYSIZE or Count < 0 }
  cmp     eax, edx
  jbe     @@SmallCheck
  Add     eax, ecx
  Add     edx, ecx
  jmp     SmallForwardMove
@@SmallCheck:
  jne     SmallBackwardMove
  ret { For Compatibility with Delphi's move for Source = Dest }
@@Large:
  jng     @@Done { For Compatibility with Delphi's move for Count < 0 }
  cmp     eax, edx
  ja      Forwards_SSE2
  JE      @@Done { For Compatibility with Delphi's move for Source = Dest }
  Sub     edx, ecx
  cmp     eax, edx
  lea     edx, [edx+ecx]
  jna     Forwards_SSE2
  jmp     Backwards_SSE2 { Source/Dest Overlap }
@@Done:
end;

procedure MoveSSE3(const Source; var dest; Count: Integer);
asm
  cmp     ecx, TINYSIZE
  ja      @@Large { Count > TINYSIZE or Count < 0 }
  cmp     eax, edx
  jbe     @@SmallCheck
  Add     eax, ecx
  Add     edx, ecx
  jmp     SmallForwardMove
@@SmallCheck:
  jne     SmallBackwardMove
  ret { For Compatibility with Delphi's move for Source = Dest }
@@Large:
  jng     @@Done { For Compatibility with Delphi's move for Count < 0 }
  cmp     eax, edx
  ja      Forwards_SSE3
  JE      @@Done { For Compatibility with Delphi's move for Source = Dest }
  Sub     edx, ecx
  cmp     eax, edx
  lea     edx, [edx+ecx]
  jna     Forwards_SSE3
  jmp     Backwards_SSE3 { Source/Dest Overlap }
@@Done:
end;

initialization

{ Initialize internals }
GetCPUInfo();

{ Patch the RTL Move method }
if sisSSE3 in SimdSupported then
    PatchMethod(@System.Move, @MoveSSE3) { SSE3 version }
else if sisSSE2 in SimdSupported then
    PatchMethod(@System.Move, @MoveSSE2) { SSE2 version }
else if sisSSE in SimdSupported then
    PatchMethod(@System.Move, @MoveSSE) { SSE version }
else if sisMMX in SimdSupported then
    PatchMethod(@System.Move, @MoveMMX); { MMX version }

{$ENDIF}
{$ENDIF FPC}

end. 
 
