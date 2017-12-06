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

unit Optimize.Move;

interface

implementation

uses
  smmFunctions;

//uses
//  Windows, SysUtils;

{ SIMD Detection code }
type
  TSimdInstructionSupport = (sisMMX, sisSSE, sisSSE2, sisSSE3);
  TSimdInstructionSupportSet = set of TSimdInstructionSupport;

  TCPUIDRegs = record
    rEAX, rEBX, rECX, rEDX : Cardinal;
  end;

var
  SimdSupported : TSimdInstructionSupportSet;
  CacheLimit    : Integer;
  NbrOfCPUs     : Cardinal;

function CPUID(const FuncId : Cardinal) : TCPUIDRegs;
var
  Regs : TCPUIDRegs;

begin
  { Request the opcode support }
  asm
    { Save registers }
    PUSH EAX
    PUSH EBX
    PUSH ECX
    PUSH EDX

    { Set the function 1 and clear others }
    MOV EAX, [FuncId]
    XOR EBX, EBX
    XOR ECX, ECX
    XOR EDX, EDX

    { Call CPUID }
    CPUID

    { Store the registers we need }
    MOV [Regs.rEAX], EAX
    MOV [Regs.rEBX], EBX
    MOV [Regs.rECX], ECX
    MOV [Regs.rEDX], EDX

    { Restore registers }
    POP EDX
    POP ECX
    POP EBX
    POP EAX
  end;

  Result := Regs;
end;

function CPUIIDSupports(const FuncId : Cardinal; const Extended : Boolean = false) : Boolean;
begin
  if Extended then
    Result := CPUID($80000000).rEAX >= FuncId
  else
    Result := CPUID($00000000).rEAX >= FuncId;
end;

function GetSupportedSimdInstructionSets : TSimdInstructionSupportSet;
var
  Regs : TCPUIDRegs;
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

function GetL2CacheSize : Integer;
var
  { Variables used for config description }
  Regs: TCPUIDRegs;
  CfgD: packed array[0..15] of Byte absolute Regs;

  I, J: Integer;
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

  for I := 1 to QueryCount do
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
    if I < QueryCount then
      Regs := CPUID($00000002);

  end;
end;

function GetExtendedL2CacheSize : Integer;
var
  Regs: TCPUIDRegs;
begin
  Result := 0;

  { Check for cache support }
  if not CPUIIDSupports($80000006, true) then
    Exit;

  { CPUID: $80000005 }
  Regs := CPUID($80000006);

  { L2 Cache size }
  Result := Regs.rECX shr 16;
end;


procedure GetCPUInfo();
var
  CacheSize1, CacheSize2 : Integer;
  SysInfo : TSystemInfo;
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

function PatchMethod(OldMethod, NewMethod : Pointer) : Boolean;
const
  SizeOfJump = 5;

var
  OldFlags : Cardinal;
  __Jump   : ^Byte;
  __Addr   : ^Integer;

begin

  Result := False;

  try
    { Change the code protection to write }
    VirtualProtect(OldMethod, SizeOfJump, PAGE_READWRITE, OldFlags);
  except
    begin Exit; end;
  end;

  try
    { Insert the jump instruction }
    __Jump := OldMethod;
    __Jump^ := $E9;

    { Insert the jump address = (OLD - NEW - SIZEOFJUMP)}
    __Addr := Ptr(Integer(OldMethod) + 1);
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
  jmp     dword ptr [@@FwdJumpTable+ecx*4]
  nop {Align Jump Table}
@@FwdJumpTable:
  dd      @@Done {Removes need to test for zero size move}
  dd      @@Fwd01, @@Fwd02, @@Fwd03, @@Fwd04, @@Fwd05, @@Fwd06, @@Fwd07, @@Fwd08
  dd      @@Fwd09, @@Fwd10, @@Fwd11, @@Fwd12, @@Fwd13, @@Fwd14, @@Fwd15, @@Fwd16
  dd      @@Fwd17, @@Fwd18, @@Fwd19, @@Fwd20, @@Fwd21, @@Fwd22, @@Fwd23, @@Fwd24
  dd      @@Fwd25, @@Fwd26, @@Fwd27, @@Fwd28, @@Fwd29, @@Fwd30, @@Fwd31, @@Fwd32
  dd      @@Fwd33, @@Fwd34, @@Fwd35, @@Fwd36
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
  nop
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
  nop
@@Fwd03:
  movzx   ecx, word ptr [eax-3]
  mov     [edx-3], cx
  movzx   ecx, byte ptr [eax-1]
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
  movzx   ecx, word ptr [eax-2]
  mov     [edx-2], cx
  ret
  nop
  nop
  nop
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
  movzx   ecx, byte ptr [eax-1]
  mov     [edx-1], cl
  ret
@@Done:
end;

{-------------------------------------------------------------------------}
{Perform Backward Move of 0..36 Bytes}
{On Entry, ECX = Count, EAX = Source, EDX = Dest.  Destroys ECX}
procedure SmallBackwardMove;
asm
  jmp     dword ptr [@@BwdJumpTable+ecx*4]
  nop {Align Jump Table}
@@BwdJumpTable:
  dd      @@Done {Removes need to test for zero size move}
  dd      @@Bwd01, @@Bwd02, @@Bwd03, @@Bwd04, @@Bwd05, @@Bwd06, @@Bwd07, @@Bwd08
  dd      @@Bwd09, @@Bwd10, @@Bwd11, @@Bwd12, @@Bwd13, @@Bwd14, @@Bwd15, @@Bwd16
  dd      @@Bwd17, @@Bwd18, @@Bwd19, @@Bwd20, @@Bwd21, @@Bwd22, @@Bwd23, @@Bwd24
  dd      @@Bwd25, @@Bwd26, @@Bwd27, @@Bwd28, @@Bwd29, @@Bwd30, @@Bwd31, @@Bwd32
  dd      @@Bwd33, @@Bwd34, @@Bwd35, @@Bwd36
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
  nop
  nop
  nop
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
  nop
  nop
  nop
@@Bwd03:
  movzx   ecx, word ptr [eax+1]
  mov     [edx+1], cx
  movzx   ecx, byte ptr [eax]
  mov     [edx], cl
  ret
  nop
  nop
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
  movzx   ecx, word ptr [eax]
  mov     [edx], cx
  ret
  nop
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
  movzx   ecx, byte ptr[eax]
  mov     [edx], cl
  ret
@@Done:
end;

{-------------------------------------------------------------------------}
{Move ECX Bytes from EAX to EDX, where EAX > EDX and ECX > 36 (TINYSIZE)}
procedure Forwards_IA32;
asm
  fild    qword ptr [eax] {First 8}
  lea     eax, [eax+ecx-8]
  lea     ecx, [edx+ecx-8]
  push    edx
  push    ecx
  fild    qword ptr [eax] {Last 8}
  neg     ecx {QWORD Align Writes}
  and     edx, -8
  lea     ecx, [ecx+edx+8]
  pop     edx
@@Loop:
  fild    qword ptr [eax+ecx]
  fistp   qword ptr [edx+ecx]
  add     ecx, 8
  jl      @@Loop
  pop     eax
  fistp   qword ptr [edx] {Last 8}
  fistp   qword ptr [eax] {First 8}
end;

{-------------------------------------------------------------------------}
{Move ECX Bytes from EAX to EDX, where EAX < EDX and ECX > 36 (TINYSIZE)}
procedure Backwards_IA32;
asm
  sub     ecx, 8
  fild    qword ptr [eax+ecx] {Last 8}
  fild    qword ptr [eax] {First 8}
  add     ecx, edx {QWORD Align Writes}
  push    ecx
  and     ecx, -8
  sub     ecx, edx
@@Loop:
  fild    qword ptr [eax+ecx]
  fistp   qword ptr [edx+ecx]
  sub     ecx, 8
  jg      @@Loop
  pop     eax
  fistp   qword ptr [edx] {First 8}
  fistp   qword ptr [eax] {Last 8}
end;


{-------------------------------------------------------------------------}
{Move ECX Bytes from EAX to EDX, where EAX > EDX and ECX > 36 (TINYSIZE)}
procedure Forwards_MMX;
const
  SMALLSIZE = 64;
  LARGESIZE = 2048;
asm
  cmp     ecx, SMALLSIZE {Size at which using MMX becomes worthwhile}
  jl      Forwards_IA32
  cmp     ecx, LARGESIZE
  jge     @@FwdLargeMove
  push    ebx
  mov     ebx, edx
  movq    mm0, [eax] {First 8 Bytes}
  add     eax, ecx {QWORD Align Writes}
  add     ecx, edx
  and     edx, -8
  add     edx, 40
  sub     ecx, edx
  add     edx, ecx
  neg     ecx
  nop {Align Loop}
@@FwdLoopMMX:
  movq    mm1, [eax+ecx-32]
  movq    mm2, [eax+ecx-24]
  movq    mm3, [eax+ecx-16]
  movq    mm4, [eax+ecx- 8]
  movq    [edx+ecx-32], mm1
  movq    [edx+ecx-24], mm2
  movq    [edx+ecx-16], mm3
  movq    [edx+ecx- 8], mm4
  add     ecx, 32
  jle     @@FwdLoopMMX
  movq    [ebx], mm0 {First 8 Bytes}
  emms
  pop     ebx
  neg     ecx
  add     ecx, 32
  jmp     SmallForwardMove
  nop {Align Loop}
  nop
@@FwdLargeMove:
  push    ebx
  mov     ebx, ecx
  test    edx, 15
  jz      @@FwdAligned
  lea     ecx, [edx+15] {16 byte Align Destination}
  and     ecx, -16
  sub     ecx, edx
  add     eax, ecx
  add     edx, ecx
  sub     ebx, ecx
  call    SmallForwardMove
@@FwdAligned:
  mov     ecx, ebx
  and     ecx, -16
  sub     ebx, ecx {EBX = Remainder}
  push    esi
  push    edi
  mov     esi, eax          {ESI = Source}
  mov     edi, edx          {EDI = Dest}
  mov     eax, ecx          {EAX = Count}
  and     eax, -64          {EAX = No of Bytes to Blocks Moves}
  and     ecx, $3F          {ECX = Remaining Bytes to Move (0..63)}
  add     esi, eax
  add     edi, eax
  neg     eax
@@MMXcopyloop:
  movq    mm0, [esi+eax   ]
  movq    mm1, [esi+eax+ 8]
  movq    mm2, [esi+eax+16]
  movq    mm3, [esi+eax+24]
  movq    mm4, [esi+eax+32]
  movq    mm5, [esi+eax+40]
  movq    mm6, [esi+eax+48]
  movq    mm7, [esi+eax+56]
  movq    [edi+eax   ], mm0
  movq    [edi+eax+ 8], mm1
  movq    [edi+eax+16], mm2
  movq    [edi+eax+24], mm3
  movq    [edi+eax+32], mm4
  movq    [edi+eax+40], mm5
  movq    [edi+eax+48], mm6
  movq    [edi+eax+56], mm7
  add     eax, 64
  jnz     @@MMXcopyloop
  emms                   {Empty MMX State}
  add     ecx, ebx
  shr     ecx, 2
  rep     movsd
  mov     ecx, ebx
  and     ecx, 3
  rep     movsb
  pop     edi
  pop     esi
  pop     ebx
end;

{-------------------------------------------------------------------------}
{Move ECX Bytes from EAX to EDX, where EAX < EDX and ECX > 36 (TINYSIZE)}
procedure Backwards_MMX;
const
  SMALLSIZE = 64;
asm
  cmp     ecx, SMALLSIZE {Size at which using MMX becomes worthwhile}
  jl      Backwards_IA32
  push    ebx
  movq    mm0, [eax+ecx-8] {Get Last QWORD}
  lea     ebx, [edx+ecx] {QWORD Align Writes}
  and     ebx, 7
  sub     ecx, ebx
  add     ebx, ecx
  sub     ecx, 32
@@BwdLoopMMX:
  movq    mm1, [eax+ecx   ]
  movq    mm2, [eax+ecx+ 8]
  movq    mm3, [eax+ecx+16]
  movq    mm4, [eax+ecx+24]
  movq    [edx+ecx+24], mm4
  movq    [edx+ecx+16], mm3
  movq    [edx+ecx+ 8], mm2
  movq    [edx+ecx   ], mm1
  sub     ecx, 32
  jge     @@BwdLoopMMX
  movq    [edx+ebx-8], mm0 {Last QWORD}
  emms
  add     ecx, 32
  pop     ebx
  jmp     SmallBackwardMove
end;

{-------------------------------------------------------------------------}
procedure LargeAlignedSSEMove;
asm
@@Loop:
  movaps  xmm0, [eax+ecx]
  movaps  xmm1, [eax+ecx+16]
  movaps  xmm2, [eax+ecx+32]
  movaps  xmm3, [eax+ecx+48]
  movaps  [edx+ecx], xmm0
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
  add     ecx, 128
  js      @@Loop
end;

{-------------------------------------------------------------------------}
procedure LargeUnalignedSSEMove;
asm
@@Loop:
  movups  xmm0, [eax+ecx]
  movups  xmm1, [eax+ecx+16]
  movups  xmm2, [eax+ecx+32]
  movups  xmm3, [eax+ecx+48]
  movaps  [edx+ecx], xmm0
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
  add     ecx, 128
  js      @@Loop
end;

{-------------------------------------------------------------------------}
procedure HugeAlignedSSEMove;
const
  Prefetch = 512;
asm
@@Loop:
  prefetchnta [eax+ecx+Prefetch]
  prefetchnta [eax+ecx+Prefetch+64]
  movaps  xmm0, [eax+ecx]
  movaps  xmm1, [eax+ecx+16]
  movaps  xmm2, [eax+ecx+32]
  movaps  xmm3, [eax+ecx+48]
  movntps [edx+ecx], xmm0
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
  add     ecx, 128
  js      @@Loop
  sfence
end;

{-------------------------------------------------------------------------}
procedure HugeUnalignedSSEMove;
const
  Prefetch = 512;
asm
@@Loop:
  prefetchnta [eax+ecx+Prefetch]
  prefetchnta [eax+ecx+Prefetch+64]
  movups  xmm0, [eax+ecx]
  movups  xmm1, [eax+ecx+16]
  movups  xmm2, [eax+ecx+32]
  movups  xmm3, [eax+ecx+48]
  movntps [edx+ecx], xmm0
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
  add     ecx, 128
  js      @@Loop
  sfence
end;

{-------------------------------------------------------------------------}
{Dest MUST be 16-Byes Aligned, Count MUST be multiple of 16 }
procedure LargeSSEMove;
asm
  push    ebx
  mov     ebx, ecx
  and     ecx, -128             {No of Bytes to Block Move (Multiple of 128)}
  add     eax, ecx              {End of Source Blocks}
  add     edx, ecx              {End of Dest Blocks}
  neg     ecx
  cmp     ecx, CacheLimit       {Count > Limit - Use Prefetch}
  jl      @@Huge
  test    eax, 15               {Check if Both Source/Dest are Aligned}
  jnz     @@LargeUnaligned
  call    LargeAlignedSSEMove   {Both Source and Dest 16-Byte Aligned}
  jmp     @@Remainder
@@LargeUnaligned:               {Source Not 16-Byte Aligned}
  call    LargeUnalignedSSEMove
  jmp     @@Remainder
@@Huge:
  test    eax, 15               {Check if Both Source/Dest Aligned}
  jnz     @@HugeUnaligned
  call    HugeAlignedSSEMove    {Both Source and Dest 16-Byte Aligned}
  jmp     @@Remainder
@@HugeUnaligned:                {Source Not 16-Byte Aligned}
  call    HugeUnalignedSSEMove
@@Remainder:
  and     ebx, $7F              {Remainder (0..112 - Multiple of 16)}
  jz      @@Done
  add     eax, ebx
  add     edx, ebx
  neg     ebx
@@RemainderLoop:
  movups  xmm0, [eax+ebx]
  movaps  [edx+ebx], xmm0
  add     ebx, 16
  jnz     @@RemainderLoop
@@Done:
  pop     ebx
end;

{-------------------------------------------------------------------------}
{Move ECX Bytes from EAX to EDX, where EAX > EDX and ECX > 36 (TINYSIZE)}
procedure Forwards_SSE;
const
  SMALLSIZE = 64;
  LARGESIZE = 2048;
asm
  cmp     ecx, SMALLSIZE
  jle     Forwards_IA32
  push    ebx
  cmp     ecx, LARGESIZE
  jge     @@FwdLargeMove
  movups  xmm0, [eax] {First 16 Bytes}
  mov     ebx, edx
  add     eax, ecx {Align Writes}
  add     ecx, edx
  and     edx, -16
  add     edx, 48
  sub     ecx, edx
  add     edx, ecx
  neg     ecx
  nop {Align Loop}
@@FwdLoopSSE:
  movups  xmm1, [eax+ecx-32]
  movups  xmm2, [eax+ecx-16]
  movaps  [edx+ecx-32], xmm1
  movaps  [edx+ecx-16], xmm2
  add     ecx, 32
  jle     @@FwdLoopSSE
  movups  [ebx], xmm0 {First 16 Bytes}
  neg     ecx
  add     ecx, 32
  pop     ebx
  jmp     SmallForwardMove
@@FwdLargeMove:
  mov     ebx, ecx
  test    edx, 15
  jz      @@FwdLargeAligned
  lea     ecx, [edx+15] {16 byte Align Destination}
  and     ecx, -16
  sub     ecx, edx
  add     eax, ecx
  add     edx, ecx
  sub     ebx, ecx
  call    SmallForwardMove
  mov     ecx, ebx
@@FwdLargeAligned:
  and     ecx, -16
  sub     ebx, ecx {EBX = Remainder}
  push    edx
  push    eax
  push    ecx
  call    LargeSSEMove
  pop     ecx
  pop     eax
  pop     edx
  add     ecx, ebx
  add     eax, ecx
  add     edx, ecx
  mov     ecx, ebx
  pop     ebx
  jmp     SmallForwardMove
end;

{-------------------------------------------------------------------------}
{Move ECX Bytes from EAX to EDX, where EAX < EDX and ECX > 36 (TINYSIZE)}
procedure Backwards_SSE;
const
  SMALLSIZE = 64;
asm
  cmp     ecx, SMALLSIZE
  jle     Backwards_IA32
  push    ebx
  movups  xmm0, [eax+ecx-16] {Last 16 Bytes}
  lea     ebx, [edx+ecx] {Align Writes}
  and     ebx, 15
  sub     ecx, ebx
  add     ebx, ecx
  sub     ecx, 32
@@BwdLoop:
  movups  xmm1, [eax+ecx]
  movups  xmm2, [eax+ecx+16]
  movaps  [edx+ecx], xmm1
  movaps  [edx+ecx+16], xmm2
  sub     ecx, 32
  jge     @@BwdLoop
  movups  [edx+ebx-16], xmm0  {Last 16 Bytes}
  add     ecx, 32
  pop     ebx
  jmp     SmallBackwardMove
end;

{-------------------------------------------------------------------------}
procedure LargeAlignedSSE2Move; {Also used in SSE3 Move}
asm
@@Loop:
  movdqa  xmm0, [eax+ecx]
  movdqa  xmm1, [eax+ecx+16]
  movdqa  xmm2, [eax+ecx+32]
  movdqa  xmm3, [eax+ecx+48]
  movdqa  [edx+ecx], xmm0
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
  add     ecx, 128
  js      @@Loop
end;

{-------------------------------------------------------------------------}
procedure LargeUnalignedSSE2Move;
asm
@@Loop:
  movdqu  xmm0, [eax+ecx]
  movdqu  xmm1, [eax+ecx+16]
  movdqu  xmm2, [eax+ecx+32]
  movdqu  xmm3, [eax+ecx+48]
  movdqa  [edx+ecx], xmm0
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
  add     ecx, 128
  js      @@Loop
end;

{-------------------------------------------------------------------------}
procedure HugeAlignedSSE2Move; {Also used in SSE3 Move}
const
  Prefetch = 512;
asm
@@Loop:
  prefetchnta [eax+ecx+Prefetch]
  prefetchnta [eax+ecx+Prefetch+64]
  movdqa  xmm0, [eax+ecx]
  movdqa  xmm1, [eax+ecx+16]
  movdqa  xmm2, [eax+ecx+32]
  movdqa  xmm3, [eax+ecx+48]
  movntdq [edx+ecx], xmm0
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
  add     ecx, 128
  js      @@Loop
  sfence
end;

{-------------------------------------------------------------------------}
procedure HugeUnalignedSSE2Move;
const
  Prefetch = 512;
asm
@@Loop:
  prefetchnta [eax+ecx+Prefetch]
  prefetchnta [eax+ecx+Prefetch+64]
  movdqu  xmm0, [eax+ecx]
  movdqu  xmm1, [eax+ecx+16]
  movdqu  xmm2, [eax+ecx+32]
  movdqu  xmm3, [eax+ecx+48]
  movntdq [edx+ecx], xmm0
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
  add     ecx, 128
  js      @@Loop
  sfence
end; {HugeUnalignedSSE2Move}

{-------------------------------------------------------------------------}
{Dest MUST be 16-Byes Aligned, Count MUST be multiple of 16 }
procedure LargeSSE2Move;
asm
  push    ebx
  mov     ebx, ecx
  and     ecx, -128             {No of Bytes to Block Move (Multiple of 128)}
  add     eax, ecx              {End of Source Blocks}
  add     edx, ecx              {End of Dest Blocks}
  neg     ecx
  cmp     ecx, CacheLimit       {Count > Limit - Use Prefetch}
  jl      @@Huge
  test    eax, 15               {Check if Both Source/Dest are Aligned}
  jnz     @@LargeUnaligned
  call    LargeAlignedSSE2Move  {Both Source and Dest 16-Byte Aligned}
  jmp     @@Remainder
@@LargeUnaligned:               {Source Not 16-Byte Aligned}
  call    LargeUnalignedSSE2Move
  jmp     @@Remainder
@@Huge:
  test    eax, 15               {Check if Both Source/Dest Aligned}
  jnz     @@HugeUnaligned
  call    HugeAlignedSSE2Move   {Both Source and Dest 16-Byte Aligned}
  jmp     @@Remainder
@@HugeUnaligned:                {Source Not 16-Byte Aligned}
  call    HugeUnalignedSSE2Move
@@Remainder:
  and     ebx, $7F              {Remainder (0..112 - Multiple of 16)}
  jz      @@Done
  add     eax, ebx
  add     edx, ebx
  neg     ebx
@@RemainderLoop:
  movdqu  xmm0, [eax+ebx]
  movdqa  [edx+ebx], xmm0
  add     ebx, 16
  jnz     @@RemainderLoop
@@Done:
  pop     ebx
end;

{-------------------------------------------------------------------------}
{Move ECX Bytes from EAX to EDX, where EAX > EDX and ECX > 36 (TINYSIZE)}
procedure Forwards_SSE2;
const
  SMALLSIZE = 64;
  LARGESIZE = 2048;
asm
  cmp     ecx, SMALLSIZE
  jle     Forwards_IA32
  push    ebx
  cmp     ecx, LARGESIZE
  jge     @@FwdLargeMove
  movdqu  xmm0, [eax] {First 16 Bytes}
  mov     ebx, edx
  add     eax, ecx {Align Writes}
  add     ecx, edx
  and     edx, -16
  add     edx, 48
  sub     ecx, edx
  add     edx, ecx
  neg     ecx
@@FwdLoopSSE2:
  movdqu  xmm1, [eax+ecx-32]
  movdqu  xmm2, [eax+ecx-16]
  movdqa  [edx+ecx-32], xmm1
  movdqa  [edx+ecx-16], xmm2
  add     ecx, 32
  jle     @@FwdLoopSSE2
  movdqu  [ebx], xmm0 {First 16 Bytes}
  neg     ecx
  add     ecx, 32
  pop     ebx
  jmp     SmallForwardMove
@@FwdLargeMove:
  mov     ebx, ecx
  test    edx, 15
  jz      @@FwdLargeAligned
  lea     ecx, [edx+15] {16 byte Align Destination}
  and     ecx, -16
  sub     ecx, edx
  add     eax, ecx
  add     edx, ecx
  sub     ebx, ecx
  call    SmallForwardMove
  mov     ecx, ebx
@@FwdLargeAligned:
  and     ecx, -16
  sub     ebx, ecx {EBX = Remainder}
  push    edx
  push    eax
  push    ecx
  call    LargeSSE2Move
  pop     ecx
  pop     eax
  pop     edx
  add     ecx, ebx
  add     eax, ecx
  add     edx, ecx
  mov     ecx, ebx
  pop     ebx
  jmp     SmallForwardMove
end;

{-------------------------------------------------------------------------}
{Move ECX Bytes from EAX to EDX, where EAX < EDX and ECX > 36 (TINYSIZE)}
procedure Backwards_SSE2;
const
  SMALLSIZE = 64;
asm
  cmp     ecx, SMALLSIZE
  jle     Backwards_IA32
  push    ebx
  movdqu  xmm0, [eax+ecx-16] {Last 16 Bytes}
  lea     ebx, [edx+ecx] {Align Writes}
  and     ebx, 15
  sub     ecx, ebx
  add     ebx, ecx
  sub     ecx, 32
  add     edi, 0 {3-Byte NOP Equivalent to Align Loop}
@@BwdLoop:
  movdqu  xmm1, [eax+ecx]
  movdqu  xmm2, [eax+ecx+16]
  movdqa  [edx+ecx], xmm1
  movdqa  [edx+ecx+16], xmm2
  sub     ecx, 32
  jge     @@BwdLoop
  movdqu  [edx+ebx-16], xmm0  {Last 16 Bytes}
  add     ecx, 32
  pop     ebx
  jmp     SmallBackwardMove
end;

{-------------------------------------------------------------------------}
procedure LargeUnalignedSSE3Move;
asm
@@Loop:
  lddqu   xmm0, [eax+ecx]
  lddqu   xmm1, [eax+ecx+16]
  lddqu   xmm2, [eax+ecx+32]
  lddqu   xmm3, [eax+ecx+48]
  movdqa  [edx+ecx], xmm0
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
  add     ecx, 128
  js      @@Loop
end;

{-------------------------------------------------------------------------}
procedure HugeUnalignedSSE3Move;
const
  Prefetch = 512;
asm
@@Loop:
  prefetchnta [eax+ecx+Prefetch]
  prefetchnta [eax+ecx+Prefetch+64]
  lddqu   xmm0, [eax+ecx]
  lddqu   xmm1, [eax+ecx+16]
  lddqu   xmm2, [eax+ecx+32]
  lddqu   xmm3, [eax+ecx+48]
  movntdq [edx+ecx], xmm0
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
  add     ecx, 128
  js      @@Loop
  sfence
end;

{-------------------------------------------------------------------------}
{Dest MUST be 16-Byes Aligned, Count MUST be multiple of 16 }
procedure LargeSSE3Move(const Source; var Dest; Count: Integer);
asm
  push    ebx
  mov     ebx, ecx
  and     ecx, -128             {No of Bytes to Block Move (Multiple of 128)}
  add     eax, ecx              {End of Source Blocks}
  add     edx, ecx              {End of Dest Blocks}
  neg     ecx
  cmp     ecx, CacheLimit       {Count > Limit - Use Prefetch}
  jl      @@Huge
  test    eax, 15               {Check if Both Source/Dest are Aligned}
  jnz     @@LargeUnaligned
  call    LargeAlignedSSE2Move  {Both Source and Dest 16-Byte Aligned}
  jmp     @@Remainder
@@LargeUnaligned:               {Source Not 16-Byte Aligned}
  call    LargeUnalignedSSE3Move
  jmp     @@Remainder
@@Huge:
  test    eax, 15               {Check if Both Source/Dest Aligned}
  jnz     @@HugeUnaligned
  call    HugeAlignedSSE2Move   {Both Source and Dest 16-Byte Aligned}
  jmp     @@Remainder
@@HugeUnaligned:                {Source Not 16-Byte Aligned}
  call    HugeUnalignedSSE3Move
@@Remainder:
  and     ebx, $7F              {Remainder (0..112 - Multiple of 16)}
  jz      @@Done
  add     eax, ebx
  add     edx, ebx
  neg     ebx
@@RemainderLoop:
  lddqu   xmm0, [eax+ebx]
  movdqa  [edx+ebx], xmm0
  add     ebx, 16
  jnz     @@RemainderLoop
@@Done:
  pop     ebx
end;

{-------------------------------------------------------------------------}
{Move ECX Bytes from EAX to EDX, where EAX > EDX and ECX > 36 (TINYSIZE)}
procedure Forwards_SSE3;
const
  SMALLSIZE = 64;
  LARGESIZE = 2048;
asm
  cmp     ecx, SMALLSIZE
  jle     Forwards_IA32
  push    ebx
  cmp     ecx, LARGESIZE
  jge     @@FwdLargeMove
  lddqu   xmm0, [eax] {First 16 Bytes}
  mov     ebx, edx
  add     eax, ecx {Align Writes}
  add     ecx, edx
  and     edx, -16
  add     edx, 48
  sub     ecx, edx
  add     edx, ecx
  neg     ecx
@@FwdLoopSSE3:
  lddqu   xmm1, [eax+ecx-32]
  lddqu   xmm2, [eax+ecx-16]
  movdqa  [edx+ecx-32], xmm1
  movdqa  [edx+ecx-16], xmm2
  add     ecx, 32
  jle     @@FwdLoopSSE3
  movdqu  [ebx], xmm0 {First 16 Bytes}
  neg     ecx
  add     ecx, 32
  pop     ebx
  jmp     SmallForwardMove
@@FwdLargeMove:
  mov     ebx, ecx
  test    edx, 15
  jz      @@FwdLargeAligned
  lea     ecx, [edx+15] {16 byte Align Destination}
  and     ecx, -16
  sub     ecx, edx
  add     eax, ecx
  add     edx, ecx
  sub     ebx, ecx
  call    SmallForwardMove
  mov     ecx, ebx
@@FwdLargeAligned:
  and     ecx, -16
  sub     ebx, ecx {EBX = Remainder}
  push    edx
  push    eax
  push    ecx
  call    LargeSSE3Move
  pop     ecx
  pop     eax
  pop     edx
  add     ecx, ebx
  add     eax, ecx
  add     edx, ecx
  mov     ecx, ebx
  pop     ebx
  jmp     SmallForwardMove
end;

{-------------------------------------------------------------------------}
{Move ECX Bytes from EAX to EDX, where EAX < EDX and ECX > 36 (TINYSIZE)}
procedure Backwards_SSE3;
const
  SMALLSIZE = 64;
asm
  cmp     ecx, SMALLSIZE
  jle     Backwards_IA32
  push    ebx
  lddqu   xmm0, [eax+ecx-16] {Last 16 Bytes}
  lea     ebx, [edx+ecx] {Align Writes}
  and     ebx, 15
  sub     ecx, ebx
  add     ebx, ecx
  sub     ecx, 32
  add     edi, 0 {3-Byte NOP Equivalent to Align Loop}
@@BwdLoop:
  lddqu   xmm1, [eax+ecx]
  lddqu   xmm2, [eax+ecx+16]
  movdqa  [edx+ecx], xmm1
  movdqa  [edx+ecx+16], xmm2
  sub     ecx, 32
  jge     @@BwdLoop
  movdqu  [edx+ebx-16], xmm0  {Last 16 Bytes}
  add     ecx, 32
  pop     ebx
  jmp     SmallBackwardMove
end;

procedure MoveMMX(const Source; var Dest; Count: Integer);
asm
  cmp     ecx, TINYSIZE
  ja      @@Large { Count > TINYSIZE or Count < 0 }
  cmp     eax, edx
  jbe     @@SmallCheck
  add     eax, ecx
  add     edx, ecx
  jmp     SmallForwardMove
@@SmallCheck:
  jne     SmallBackwardMove
  ret { For Compatibility with Delphi's move for Source = Dest }
@@Large:
  jng     @@Done { For Compatibility with Delphi's move for Count < 0 }
  cmp     eax, edx
  ja      Forwards_MMX
  je      @@Done { For Compatibility with Delphi's move for Source = Dest }
  sub     edx, ecx
  cmp     eax, edx
  lea     edx, [edx+ecx]
  jna     Forwards_MMX
  jmp     Backwards_MMX { Source/Dest Overlap }
@@Done:
end;

procedure MoveSSE(const Source; var Dest; Count: Integer);
asm
  cmp     ecx, TINYSIZE
  ja      @@Large { Count > TINYSIZE or Count < 0 }
  cmp     eax, edx
  jbe     @@SmallCheck
  add     eax, ecx
  add     edx, ecx
  jmp     SmallForwardMove
@@SmallCheck:
  jne     SmallBackwardMove
  ret { For Compatibility with Delphi's move for Source = Dest }
@@Large:
  jng     @@Done { For Compatibility with Delphi's move for Count < 0 }
  cmp     eax, edx
  ja      Forwards_SSE
  je      @@Done { For Compatibility with Delphi's move for Source = Dest }
  sub     edx, ecx
  cmp     eax, edx
  lea     edx, [edx+ecx]
  jna     Forwards_SSE
  jmp     Backwards_SSE { Source/Dest Overlap }
@@Done:
end;

procedure MoveSSE2(const Source; var Dest; Count: Integer);
asm
  cmp     ecx, TINYSIZE
  ja      @@Large {Count > TINYSIZE or Count < 0}
  cmp     eax, edx
  jbe     @@SmallCheck
  add     eax, ecx
  add     edx, ecx
  jmp     SmallForwardMove
@@SmallCheck:
  jne     SmallBackwardMove
  ret {For Compatibility with Delphi's move for Source = Dest}
@@Large:
  jng     @@Done {For Compatibility with Delphi's move for Count < 0}
  cmp     eax, edx
  ja      Forwards_SSE2
  je      @@Done {For Compatibility with Delphi's move for Source = Dest}
  sub     edx, ecx
  cmp     eax, edx
  lea     edx, [edx+ecx]
  jna     Forwards_SSE2
  jmp     Backwards_SSE2 {Source/Dest Overlap}
@@Done:
end;

procedure MoveSSE3(const Source; var Dest; Count: Integer);
asm
  cmp     ecx, TINYSIZE
  ja      @@Large { Count > TINYSIZE or Count < 0 }
  cmp     eax, edx
  jbe     @@SmallCheck
  add     eax, ecx
  add     edx, ecx
  jmp     SmallForwardMove
@@SmallCheck:
  jne     SmallBackwardMove
  ret { For Compatibility with Delphi's move for Source = Dest }
@@Large:
  jng     @@Done { For Compatibility with Delphi's move for Count < 0}
  cmp     eax, edx
  ja      Forwards_SSE3
  je      @@Done { For Compatibility with Delphi's move for Source = Dest }
  sub     edx, ecx
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
     PatchMethod(@System.Move, @MoveSSE3)  { SSE3 version }
  else if sisSSE2 in SimdSupported then
    PatchMethod(@System.Move, @MoveSSE2)  { SSE2 version }
  else if sisSSE in SimdSupported then
    PatchMethod(@System.Move, @MoveSSE)   { SSE version }
  else if sisMMX in SimdSupported then
    PatchMethod(@System.Move, @MoveMMX);  { MMX version }
end.
