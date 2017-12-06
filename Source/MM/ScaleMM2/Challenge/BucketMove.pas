unit BucketMove;

(*
A simplified version of FastMove 2.20 for inclusion with BucketMem
  1) Only forward moves (no overlapping possible)
  2) All moves multiple of 4
  3) All pointers 4-byte aligned
  4) No System.Move patching, the fastest Move function is exported using SelectBestMoveFunction 
Robert Houdart, 2005/02/21
All credit for the original code to John O'Harrow
*)


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

Version: 2.20 - 11-FEB-2005
*)

interface

function GetBestMoveFunction: pointer;  // returns the fastest Move function for the hardware


implementation

uses
  FastcodeCPUID;

var
  PrefetchLimit : Integer; {Used within SSE2 Move}

const
  SMALLMOVESIZE = 36;

{-------------------------------------------------------------------------}
{Perform Forward Move of 0..36 Bytes}
{On Entry, ECX = Count, EAX = Source+Count, EDX = Dest+Count.  Destroys ECX}
procedure SmallForwardMove_6;
asm
  jmp     dword ptr [@@FwdJumpTable+ecx]
  nop {Align Jump Table}
  nop {Align Jump Table}
@@FwdJumpTable:
  dd      @@Done {Removes need to test for zero size move}
  dd      @@Fwd04, @@Fwd08, @@Fwd12, @@Fwd16
  dd      @@Fwd20, @@Fwd24, @@Fwd28, @@Fwd32, @@Fwd36
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
@@Done:
end; {SmallForwardMove}

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
  pop     edi
  pop     esi
  pop     ebx
end; {Forwards_MMX}

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
{Move using IA32 Instruction Set Only}
procedure MoveJOH_IA32_6(const Source; var Dest; Count : Integer);
asm
  cmp     ecx, SMALLMOVESIZE
  ja      @Large {Count > SMALLMOVESIZE or Count < 0}
  cmp     eax, edx
  je      @Done
  add     eax, ecx
  add     edx, ecx
  jmp     SmallForwardMove_6
@Large:
  jng     @Done {For Compatibility with Delphi's move for Count < 0}
  cmp     eax, edx
  jne     Forwards_IA32_6
  je      @Done {For Compatibility with Delphi's move for Source = Dest}
@Done:
end; {MoveJOH_IA32}

{-------------------------------------------------------------------------}
{Move using MMX Instruction Set}
procedure MoveJOH_MMX_6(const Source; var Dest; Count : Integer);
asm
  cmp     ecx, SMALLMOVESIZE
  ja      @Large {Count > SMALLMOVESIZE or Count < 0}
  cmp     eax, edx
  je      @Done
  add     eax, ecx
  add     edx, ecx
  jmp     SmallForwardMove_6
@Large:
  jng     @Done {For Compatibility with Delphi's move for Count < 0}
  cmp     eax, edx
  jne     Forwards_MMX_6
@Done:
end; {MoveJOH_MMX}

{-------------------------------------------------------------------------}
{Move using SSE Instruction Set}
procedure MoveJOH_SSE_6(const Source; var Dest; Count : Integer);
asm
  cmp     ecx, SMALLMOVESIZE
  ja      @Large {Count > SMALLMOVESIZE or Count < 0}
  cmp     eax, edx
  je      @Done
  add     eax, ecx
  add     edx, ecx
  jmp     SmallForwardMove_6
@Large:
  jng     @Done {For Compatibility with Delphi's move for Count < 0}
  cmp     eax, edx
  jne     Forwards_SSE_6
@Done:
end; {MoveJOH_SSE}

{-------------------------------------------------------------------------}

function GetBestMoveFunction: pointer;
begin
  PrefetchLimit := (CPU.L2CacheSize div 16) * -1024; // Used within SSE Move
  if isSSE in CPU.InstructionSupport then
    Result := @MoveJOH_SSE_6   // Processor Supports SSE
  else if isMMX in CPU.InstructionSupport then
    Result := @MoveJOH_MMX_6   // Processor Supports MMX
  else
    Result := @MoveJOH_IA32_6; // Processor does not Support MMX or SSE
end;

end.

