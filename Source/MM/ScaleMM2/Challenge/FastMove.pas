unit FastMove;

(*
Copyright (c) 2005, John O'Harrow (john@almcrest.demon.co.uk)

This software is provided 'as-is', without any express or implied warranty.
In no event will the authors be held liable for any damages arising from the
use of this software.

Permission is granted to anyone to use this software for any purpose, including
commercial applications, and to alter it and redistribute it freely, subject to
the following restrictions:

1. The origin of this software must not be misrepresented; you must not claim
   that you wrote the original software. If you use this software in a product,
   an acknowledgment in the product documentation would be appreciated but is
   not required.

2. Altered source versions must be plainly marked as such, and must not be
   misrepresented as being the original software.

3. This notice may not be removed or altered from any source distribution.

-------------------------------------------------------------------------------

Version: 2.30 : 12-FEB-2005

How to use:

  Include this unit in any uses clause - Thats It!

What is Does:

  This unit replaces all calls to system.move with calls to a faster move
  procedure (Up to 3 times faster).  The code will automatically detect and
  use MMX or SSE where available.

Version  Change
-------  ------
2.00     Updated all Moves to use Move_JOH_XXX_6 (better code alignment)
2.10     VirtualProtect modified to use PAGE_EXECUTE_READWRITE to prevent
         error under WinXP SP2 with AMD64 running DEP
2.20     Added Check for "Already Patched" to prevent error when using
         Packages in both DLL's and EXE
2.30     PrefetchLimit initialization moved outside of ReplaceMove
*)

interface

{Procedures only interfaced for testing and validation purposes}
  procedure MoveJOH_PAS_6 (const Source; var Dest; Count : Integer);
  procedure MoveJOH_IA32_6(const Source; var Dest; Count : Integer);
  procedure MoveJOH_MMX_6 (const Source; var Dest; Count : Integer);
  procedure MoveJOH_SSE_6 (const Source; var Dest; Count : Integer);

  procedure ReplaceMove;

implementation

uses
  Windows, SysUtils, FastcodeCPUID;

var
  PrefetchLimit : Integer; {Used within SSE2 Move}

{-------------------------------------------------------------------------}
{Move without using any BASM Code}
procedure MoveJOH_PAS_6(const Source; var Dest; Count : Integer);
var
  S, D       : Cardinal;
  Temp, C, I : Integer;
  L          : PInteger;
begin
  S := Cardinal(@Source);
  D := Cardinal(@Dest);
  if S = D then
    Exit;
  if Count <= 4 then           
    case Count of
      1 : PByte(@Dest)^ := PByte(S)^;
      2 : PWord(@Dest)^ := PWord(S)^;
      3 : if D > S then
            begin
              PByte(Integer(@Dest)+2)^ := PByte(S+2)^;
              PWord(@Dest)^ := PWord(S)^;
            end
          else
            begin
              PWord(@Dest)^ := PWord(S)^;
              PByte(Integer(@Dest)+2)^ := PByte(S+2)^;
            end;
      4 : PInteger(@Dest)^ := PInteger(S)^
      else Exit; {Count <= 0}
    end
  else
    if D > S then
      begin
        Temp := PInteger(S)^;
        I := Integer(@Dest);
        C := Count - 4;
        L := PInteger(Integer(@Dest) + C);
        Inc(S, C);
        repeat
          L^ := PInteger(S)^;
          if Count <= 8 then
            Break;
          Dec(Count, 4);
          Dec(S, 4);
          Dec(L);
        until False;
        PInteger(I)^ := Temp;
      end
    else
      begin
        C := Count - 4;
        Temp := PInteger(S + Cardinal(C))^;
        I := Integer(@Dest) + C;
        L := @Dest;
        repeat
          L^ := PInteger(S)^;
          if Count <= 8 then
            Break;
          Dec(Count, 4);
          Inc(S, 4);
          Inc(L);
        until False;
        PInteger(I)^ := Temp;
      end;              
end; {MoveJOH_PAS}

const
  SMALLMOVESIZE = 36;

{-------------------------------------------------------------------------}
{Perform Forward Move of 0..36 Bytes}
{On Entry, ECX = Count, EAX = Source+Count, EDX = Dest+Count.  Destroys ECX}
procedure SmallForwardMove_6;
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
  movzx   ecx,  word ptr [eax-3]
  mov     [edx-3], cx
  movzx   ecx,  byte ptr [eax-1]
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
  movzx   ecx,  word ptr [eax-2]
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
  movzx   ecx,  byte ptr [eax-1]
  mov     [edx-1], cl
  ret
@@Done:
end; {SmallForwardMove}

{-------------------------------------------------------------------------}
{Perform Backward Move of 0..36 Bytes}
{On Entry, ECX = Count, EAX = Source, EDX = Dest.  Destroys ECX}
procedure SmallBackwardMove_6;
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
  movzx   ecx,  word ptr [eax+1]
  mov     [edx+1], cx
  movzx   ecx,  byte ptr [eax]
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
  movzx   ecx,  word ptr [eax]
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
  movzx   ecx,  byte ptr[eax]
  mov     [edx], cl
  ret
  nop
  nop
@@Done:
end; {SmallBackwardMove}

{-------------------------------------------------------------------------}
{Move ECX Bytes from EAX to EDX, where EAX > EDX and ECX > 36 (SMALLMOVESIZE)}
procedure Forwards_IA32_6;
asm
  push    ebx
  mov     ebx, edx
  fild    qword ptr [eax]
  add     eax, ecx {QWORD Align Writes}
  add     ecx, edx
  add     edx, 7
  and     edx, -8
  sub     ecx, edx
  add     edx, ecx {Now QWORD Aligned}
  sub     ecx, 16
  neg     ecx
@FwdLoop:
  fild    qword ptr [eax+ecx-16]
  fistp   qword ptr [edx+ecx-16]
  fild    qword ptr [eax+ecx-8]
  fistp   qword ptr [edx+ecx-8]
  add     ecx, 16
  jle     @FwdLoop
  fistp   qword ptr [ebx]
  neg     ecx
  add     ecx, 16
  pop     ebx
  jmp     SmallForwardMove_6
end; {Forwards_IA32}

{-------------------------------------------------------------------------}
{Move ECX Bytes from EAX to EDX, where EAX < EDX and ECX > 36 (SMALLMOVESIZE)}
procedure Backwards_IA32_6;
asm
  push    ebx
  fild    qword ptr [eax+ecx-8]
  lea     ebx, [edx+ecx] {QWORD Align Writes}
  and     ebx, 7
  sub     ecx, ebx
  add     ebx, ecx {Now QWORD Aligned, EBX = Original Length}
  sub     ecx, 16
@BwdLoop:
  fild    qword ptr [eax+ecx]
  fild    qword ptr [eax+ecx+8]
  fistp   qword ptr [edx+ecx+8]
  fistp   qword ptr [edx+ecx]
  sub     ecx, 16
  jge     @BwdLoop
  fistp   qword ptr [edx+ebx-8]
  add     ecx, 16
  pop     ebx
  jmp     SmallBackwardMove_6
end; {Backwards_IA32}

{-------------------------------------------------------------------------}
{Move ECX Bytes from EAX to EDX, where EAX > EDX and ECX > 36 (SMALLMOVESIZE)}
procedure Forwards_MMX_6;
const
  LARGESIZE = 1024;
asm
  cmp     ecx, LARGESIZE
  jge     @FwdLargeMove
  cmp     ecx, 72 {Size at which using MMX becomes worthwhile}
  jl      Forwards_IA32_6
  push    ebx
  mov     ebx, edx
  movq    mm0, [eax] {First 8 Bytes}
  {QWORD Align Writes}
  add     eax, ecx
  add     ecx, edx
  add     edx, 7
  and     edx, -8
  sub     ecx, edx
  add     edx, ecx
  {Now QWORD Aligned}
  sub     ecx, 32
  neg     ecx
@FwdLoopMMX:
  movq    mm1, [eax+ecx-32]
  movq    mm2, [eax+ecx-24]
  movq    mm3, [eax+ecx-16]
  movq    mm4, [eax+ecx- 8]
  movq    [edx+ecx-32], mm1
  movq    [edx+ecx-24], mm2
  movq    [edx+ecx-16], mm3
  movq    [edx+ecx- 8], mm4
  add     ecx, 32
  jle     @FwdLoopMMX
  movq    [ebx], mm0 {First 8 Bytes}
  emms
  pop     ebx
  neg     ecx
  add     ecx, 32
  jmp     SmallForwardMove_6
@FwdLargeMove:
  push    ebx
  mov     ebx, ecx
  test    edx, 15
  jz      @FwdAligned
  {16 byte Align Destination}
  mov     ecx, edx
  add     ecx, 15
  and     ecx, -16
  sub     ecx, edx
  add     eax, ecx
  add     edx, ecx
  sub     ebx, ecx
  {Destination now 16 Byte Aligned}
  call    SmallForwardMove_6
@FwdAligned:
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
  shr     eax, 3            {EAX = No of QWORD's to Block Move}
  neg     eax
@MMXcopyloop:
  movq    mm0, [esi+eax*8   ]
  movq    mm1, [esi+eax*8+ 8]
  movq    mm2, [esi+eax*8+16]
  movq    mm3, [esi+eax*8+24]
  movq    mm4, [esi+eax*8+32]
  movq    mm5, [esi+eax*8+40]
  movq    mm6, [esi+eax*8+48]
  movq    mm7, [esi+eax*8+56]
  movq    [edi+eax*8   ], mm0
  movq    [edi+eax*8+ 8], mm1
  movq    [edi+eax*8+16], mm2
  movq    [edi+eax*8+24], mm3
  movq    [edi+eax*8+32], mm4
  movq    [edi+eax*8+40], mm5
  movq    [edi+eax*8+48], mm6
  movq    [edi+eax*8+56], mm7
  add     eax, 8
  jnz     @MMXcopyloop
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
end; {Forwards_MMX}

{-------------------------------------------------------------------------}
{Move ECX Bytes from EAX to EDX, where EAX < EDX and ECX > 36 (SMALLMOVESIZE)}
procedure Backwards_MMX_6;
asm
  cmp     ecx, 72 {Size at which using MMX becomes worthwhile}
  jl      Backwards_IA32_6
  push    ebx
  movq    mm0, [eax+ecx-8] {Get Last QWORD}
  {QWORD Align Writes}
  lea     ebx, [edx+ecx]
  and     ebx, 7
  sub     ecx, ebx
  add     ebx, ecx
  {Now QWORD Aligned}
  sub     ecx, 32
@BwdLoopMMX:
  movq    mm1, [eax+ecx   ]
  movq    mm2, [eax+ecx+ 8]
  movq    mm3, [eax+ecx+16]
  movq    mm4, [eax+ecx+24]
  movq    [edx+ecx+24], mm4
  movq    [edx+ecx+16], mm3
  movq    [edx+ecx+ 8], mm2
  movq    [edx+ecx   ], mm1
  sub     ecx, 32
  jge     @BwdLoopMMX
  movq    [edx+ebx-8],  mm0 {Last QWORD}
  emms
  add     ecx, 32
  pop     ebx
  jmp     SmallBackwardMove_6
end; {Backwards_MMX}

{-------------------------------------------------------------------------}                  
{Dest MUST be 16-Byes Aligned, Count MUST be multiple of 16 }
procedure AlignedFwdMoveSSE_6(const Source; var Dest; Count: Integer);
const
  Prefetch = 512;
asm
  push    ebx
  mov     ebx, eax             {ebx = Source}
  mov     eax, ecx             {EAX = Count}
  and     eax, -128            {EAX = No of Bytes to Block Move}
  add     ebx, eax                                           
  add     edx, eax
  shr     eax, 3               {EAX = No of QWORD's to Block Move}
  neg     eax
  cmp     eax, PrefetchLimit   {Count > Limit - Use Prefetch}
  jl      @Large
@Small:
  test    ebx, 15              {Check if Both Source/Dest are Aligned}
  jnz     @SmallUnaligned
@SmallAligned:                 {Both Source and Dest 16-Byte Aligned}

  nop                          {Align Loops}
  nop
  nop

@SmallAlignedLoop:
  movaps  xmm0, [ebx+8*eax]
  movaps  xmm1, [ebx+8*eax+16]
  movaps  xmm2, [ebx+8*eax+32]
  movaps  xmm3, [ebx+8*eax+48]
  movaps  [edx+8*eax], xmm0
  movaps  [edx+8*eax+16], xmm1
  movaps  [edx+8*eax+32], xmm2
  movaps  [edx+8*eax+48], xmm3
  movaps  xmm4, [ebx+8*eax+64]
  movaps  xmm5, [ebx+8*eax+80]
  movaps  xmm6, [ebx+8*eax+96]
  movaps  xmm7, [ebx+8*eax+112]
  movaps  [edx+8*eax+64], xmm4
  movaps  [edx+8*eax+80], xmm5
  movaps  [edx+8*eax+96], xmm6
  movaps  [edx+8*eax+112], xmm7
  add     eax, 16
  js      @SmallAlignedLoop
  jmp     @Remainder

@SmallUnaligned:               {Source Not 16-Byte Aligned}
@SmallUnalignedLoop:
  movups  xmm0, [ebx+8*eax]
  movups  xmm1, [ebx+8*eax+16]
  movups  xmm2, [ebx+8*eax+32]
  movups  xmm3, [ebx+8*eax+48]
  movaps  [edx+8*eax], xmm0
  movaps  [edx+8*eax+16], xmm1
  movaps  [edx+8*eax+32], xmm2
  movaps  [edx+8*eax+48], xmm3
  movups  xmm4, [ebx+8*eax+64]
  movups  xmm5, [ebx+8*eax+80]
  movups  xmm6, [ebx+8*eax+96]
  movups  xmm7, [ebx+8*eax+112]
  movaps  [edx+8*eax+64], xmm4
  movaps  [edx+8*eax+80], xmm5
  movaps  [edx+8*eax+96], xmm6
  movaps  [edx+8*eax+112], xmm7
  add     eax, 16
  js      @SmallUnalignedLoop
  jmp     @Remainder

@Large:
  test    ebx, 15              {Check if Both Source/Dest Aligned}
  jnz     @LargeUnaligned
@LargeAligned:                 {Both Source and Dest 16-Byte Aligned}
@LargeAlignedLoop:
  prefetchnta [ebx+8*eax+Prefetch]
  prefetchnta [ebx+8*eax+Prefetch+64]
  movaps  xmm0, [ebx+8*eax]
  movaps  xmm1, [ebx+8*eax+16]
  movaps  xmm2, [ebx+8*eax+32]
  movaps  xmm3, [ebx+8*eax+48]
  movntps [edx+8*eax], xmm0
  movntps [edx+8*eax+16], xmm1
  movntps [edx+8*eax+32], xmm2
  movntps [edx+8*eax+48], xmm3
  movaps  xmm4, [ebx+8*eax+64]
  movaps  xmm5, [ebx+8*eax+80]
  movaps  xmm6, [ebx+8*eax+96]
  movaps  xmm7, [ebx+8*eax+112]
  movntps [edx+8*eax+64], xmm4
  movntps [edx+8*eax+80], xmm5
  movntps [edx+8*eax+96], xmm6
  movntps [edx+8*eax+112], xmm7
  add     eax, 16
  js      @LargeAlignedLoop
  sfence
  jmp     @Remainder

@LargeUnaligned:              {Source Not 16-Byte Aligned}
@LargeUnalignedLoop:
  prefetchnta [ebx+8*eax+Prefetch]
  prefetchnta [ebx+8*eax+Prefetch+64]
  movups  xmm0, [ebx+8*eax]
  movups  xmm1, [ebx+8*eax+16]
  movups  xmm2, [ebx+8*eax+32]
  movups  xmm3, [ebx+8*eax+48]
  movntps [edx+8*eax], xmm0
  movntps [edx+8*eax+16], xmm1
  movntps [edx+8*eax+32], xmm2
  movntps [edx+8*eax+48], xmm3
  movups  xmm4, [ebx+8*eax+64]
  movups  xmm5, [ebx+8*eax+80]
  movups  xmm6, [ebx+8*eax+96]
  movups  xmm7, [ebx+8*eax+112]
  movntps [edx+8*eax+64], xmm4
  movntps [edx+8*eax+80], xmm5
  movntps [edx+8*eax+96], xmm6
  movntps [edx+8*eax+112], xmm7
  add     eax, 16
  js      @LargeUnalignedLoop
  sfence

@Remainder:
  and     ecx, $7F {ECX = Remainder (0..112 - Multiple of 16)}
  jz      @Done
  add     ebx, ecx
  add     edx, ecx
  neg     ecx
@RemainderLoop:
  movups  xmm0, [ebx+ecx]
  movaps  [edx+ecx], xmm0
  add     ecx, 16
  jnz     @RemainderLoop
@Done:
  pop     ebx
end; {AlignedFwdMoveSSE}

{-------------------------------------------------------------------------}
{Move ECX Bytes from EAX to EDX, where EAX > EDX and ECX > 36 (SMALLMOVESIZE)}
procedure Forwards_SSE_6;
const
  LARGESIZE = 2048;
asm
  cmp     ecx, LARGESIZE
  jge     @FwdLargeMove
  cmp     ecx, SMALLMOVESIZE+32
  movups  xmm0, [eax]
  jg      @FwdMoveSSE
  movups  xmm1, [eax+16]
  movups  [edx], xmm0
  movups  [edx+16], xmm1
  add     eax, ecx
  add     edx, ecx
  sub     ecx, 32
  jmp     SmallForwardMove_6
@FwdMoveSSE:
  push    ebx
  mov     ebx, edx
  {Align Writes}
  add     eax, ecx
  add     ecx, edx
  add     edx, 15                   
  and     edx, -16
  sub     ecx, edx
  add     edx, ecx
  {Now Aligned}
  sub     ecx, 32
  neg     ecx
@FwdLoopSSE:
  movups  xmm1, [eax+ecx-32]
  movups  xmm2, [eax+ecx-16]
  movaps  [edx+ecx-32], xmm1
  movaps  [edx+ecx-16], xmm2
  add     ecx, 32
  jle     @FwdLoopSSE
  movups  [ebx], xmm0 {First 16 Bytes}
  neg     ecx
  add     ecx, 32
  pop     ebx
  jmp     SmallForwardMove_6
@FwdLargeMove:
  push    ebx
  mov     ebx, ecx
  test    edx, 15
  jz      @FwdLargeAligned
  {16 byte Align Destination}
  mov     ecx, edx
  add     ecx, 15
  and     ecx, -16
  sub     ecx, edx
  add     eax, ecx
  add     edx, ecx
  sub     ebx, ecx
  {Destination now 16 Byte Aligned}
  call    SmallForwardMove_6
  mov     ecx, ebx
@FwdLargeAligned:
  and     ecx, -16
  sub     ebx, ecx {EBX = Remainder}
  push    edx
  push    eax
  push    ecx
  call    AlignedFwdMoveSSE_6
  pop     ecx
  pop     eax
  pop     edx
  add     ecx, ebx
  add     eax, ecx
  add     edx, ecx
  mov     ecx, ebx
  pop     ebx
  jmp     SmallForwardMove_6
end; {Forwards_SSE}

{-------------------------------------------------------------------------}
{Move ECX Bytes from EAX to EDX, where EAX < EDX and ECX > 36 (SMALLMOVESIZE)}
procedure Backwards_SSE_6;
asm
  cmp     ecx, SMALLMOVESIZE+32
  jg      @BwdMoveSSE
  sub     ecx, 32
  movups  xmm1, [eax+ecx]
  movups  xmm2, [eax+ecx+16]
  movups  [edx+ecx], xmm1
  movups  [edx+ecx+16], xmm2
  jmp     SmallBackwardMove_6
@BwdMoveSSE:
  push    ebx
  movups  xmm0, [eax+ecx-16] {Last 16 Bytes}
  {Align Writes}
  lea     ebx, [edx+ecx]
  and     ebx, 15
  sub     ecx, ebx
  add     ebx, ecx
  {Now Aligned}
  sub     ecx, 32
@BwdLoop:
  movups  xmm1, [eax+ecx]
  movups  xmm2, [eax+ecx+16]
  movaps  [edx+ecx], xmm1
  movaps  [edx+ecx+16], xmm2
  sub     ecx, 32
  jge     @BwdLoop
  movups  [edx+ebx-16], xmm0  {Last 16 Bytes}
  add     ecx, 32
  pop     ebx
  jmp     SmallBackwardMove_6
end; {Backwards_SSE}

{For Patching, The following Move Procedures must have Identical Layouts}
{The only Differences are the Jump Destinations (for IA32/MMX/SSE)}

{-------------------------------------------------------------------------}
{Move using IA32 Instruction Set Only}
procedure MoveJOH_IA32_6(const Source; var Dest; Count : Integer);
asm
  cmp     ecx, SMALLMOVESIZE
  ja      @Large {Count > SMALLMOVESIZE or Count < 0}
  cmp     eax, edx
  jbe     @SmallCheck
  add     eax, ecx
  add     edx, ecx
  jmp     SmallForwardMove_6
@SmallCheck:
  jne     SmallBackwardMove_6
  ret {For Compatibility with Delphi's move for Source = Dest}
@Large:
  jng     @Done {For Compatibility with Delphi's move for Count < 0}
  cmp     eax, edx
  ja      Forwards_IA32_6
  je      @Done {For Compatibility with Delphi's move for Source = Dest}
  sub     edx, ecx
  cmp     eax, edx
  lea     edx, [edx+ecx]
  jna     Forwards_IA32_6
  jmp     Backwards_IA32_6 {Source/Dest Overlap}
@Done:
end; {MoveJOH_IA32}

{-------------------------------------------------------------------------}
{Move using MMX Instruction Set}
procedure MoveJOH_MMX_6(const Source; var Dest; Count : Integer);
asm
  cmp     ecx, SMALLMOVESIZE
  ja      @Large {Count > SMALLMOVESIZE or Count < 0}
  cmp     eax, edx
  jbe     @SmallCheck
  add     eax, ecx
  add     edx, ecx
  jmp     SmallForwardMove_6
@SmallCheck:
  jne     SmallBackwardMove_6
  ret {For Compatibility with Delphi's move for Source = Dest}
@Large:
  jng     @Done {For Compatibility with Delphi's move for Count < 0}
  cmp     eax, edx
  ja      Forwards_MMX_6
  je      @Done {For Compatibility with Delphi's move for Source = Dest}
  sub     edx, ecx
  cmp     eax, edx
  lea     edx, [edx+ecx]
  jna     Forwards_MMX_6
  jmp     Backwards_MMX_6 {Source/Dest Overlap}
@Done:
end; {MoveJOH_MMX}

{-------------------------------------------------------------------------}
{Move using SSE Instruction Set}
procedure MoveJOH_SSE_6(const Source; var Dest; Count : Integer);
asm
  cmp     ecx, SMALLMOVESIZE
  ja      @Large {Count > SMALLMOVESIZE or Count < 0}
  cmp     eax, edx
  jbe     @SmallCheck
  add     eax, ecx
  add     edx, ecx
  jmp     SmallForwardMove_6
@SmallCheck:
  jne     SmallBackwardMove_6
  ret {For Compatibility with Delphi's move for Source = Dest}
@Large:
  jng     @Done {For Compatibility with Delphi's move for Count < 0}
  cmp     eax, edx
  ja      Forwards_SSE_6
  je      @Done {For Compatibility with Delphi's move for Source = Dest}
  sub     edx, ecx
  cmp     eax, edx
  lea     edx, [edx+ecx]
  jna     Forwards_SSE_6
  jmp     Backwards_SSE_6 {Source/Dest Overlap}
@Done:
end; {MoveJOH_SSE}

{-------------------------------------------------------------------------}
{$IFDEF PurePascal}

procedure ReplaceMove;
const
  JumpFarId = $E9;
var
  OldProtect,
  Protect   : DWORD;
begin
  VirtualProtect(@System.Move, 256, PAGE_EXECUTE_READWRITE, @OldProtect);
  if PByte(@System.Move)^ <> JumpFarId then {Check if Already Patched}
    begin
      PByte(@System.Move)^ := JumpFarId;
      PInteger(Integer(@System.Move)+1)^ :=
        Integer(@MoveJOH_PAS_6) - Integer(@System.Move)-5; {Change Destination}
    end;
  VirtualProtect(@System.Move, 256, OldProtect, @Protect);
end;

{$ELSE}

procedure ReplaceMove;
const
  JumpFarId = $E9;
  JumpPtrId = $25FF;
type {Jump Positions in Move Prodedures Above - Really Horrible but it Works}
  NewMoveType = packed record {Size = 56 Bytes, System.Move Size = 64 Bytes}
    Padding1  : array[1..14] of Byte;
    Jump1Dest : Integer; {jmp SmallForwardMove}
    Padding2  : array[1.. 2] of Byte;
    Jump2Dest : Integer; {jmp SmallBackwardMove}
    Padding3  : array[1.. 7] of Byte;
    Jump3Dest : Integer; {jg  Forwards_XXX}
    Padding4  : array[1..11] of Byte;
    Jump4Dest : Integer; {jg  Backwards_XXX}
    Padding5  : array[1.. 1] of Byte;
    Jump5Dest : Integer; {jmp Forwards_XXX}
    Padding6  : array[1.. 1] of Byte;
  end;
var
  I, Offset : Integer;
  SrcProc   : Pointer;
  Src, Dest : PByteArray;
  NewMove   : NewMoveType;
  OldProtect,
  Protect   : DWORD;
begin
  VirtualProtect(@System.Move, 256, PAGE_EXECUTE_READWRITE, @OldProtect);
  if isSSE in CPU.InstructionSupport then
    SrcProc := @MoveJOH_SSE_6 {Processor Supports SSE}
  else
    if isMMX in CPU.InstructionSupport then
      SrcProc := @MoveJOH_MMX_6 {Processor Supports MMX}
    else
      SrcProc := @MoveJOH_IA32_6; {Processor does not Support MMX or SSE}
  VirtualProtect(@System.Move, 256, PAGE_EXECUTE_READWRITE, @OldProtect);
  if PByte(@System.Move)^ <> JumpFarId then {Check if Already Patched}
    if PWord(@System.Move)^ = JumpPtrId then
      begin {System.Move Starts JMP DWORD PTR [XXXXXXXX] (ie. Using Packages)}
        PByte(@System.Move)^ := JumpFarId;
        PInteger(Integer(@System.Move)+1)^ :=
          Integer(SrcProc) - Integer(@System.Move)-5; {Change Destination}
      end
    else
      begin {Patch system.move}
        Move(SrcProc^, NewMove, SizeOf(NewMove));
        {Adjust Jump Destinations in Copied Procedure}
        Offset := Integer(SrcProc) - Integer(@System.Move);
        Inc(NewMove.Jump1Dest, Offset);
        Inc(NewMove.Jump2Dest, Offset);
        Inc(NewMove.Jump3Dest, Offset);
        Inc(NewMove.Jump4Dest, Offset);
        Inc(NewMove.Jump5Dest, Offset);
        Src  := @NewMove;
        Dest := @System.Move;
        for I := 0 to SizeOf(NewMove) - 1 do
          Dest[I] := Src[I]; {Overwrite System.Move}
      end;
  VirtualProtect(@System.Move, 256, OldProtect, @Protect);
  FlushInstructionCache(GetCurrentProcess, @System.Move, SizeOf(NewMove));
end;

{$ENDIF}

initialization
  PrefetchLimit := (CPU.L2CacheSize div 16) * -1024; {Used within SSE Move}
{.$IFOPT D-} {Prevent continually stepping into FastMove code while Debugging}
  ReplaceMove; {Patch Delphi's System.Move}
{.$ENDIF}
end.

