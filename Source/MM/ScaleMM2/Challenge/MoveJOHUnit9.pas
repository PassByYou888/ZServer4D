unit MoveJOHUnit9;

interface

  procedure MoveJOH_PAS_9 (const Source; var Dest; Count : Integer);
  procedure MoveJOH_IA32_9(const Source; var Dest; Count : Integer);
  procedure MoveJOH_MMX_9 (const Source; var Dest; Count : Integer);
  procedure MoveJOH_SSE_9 (const Source; var Dest; Count : Integer);
  procedure MoveJOH_SSE2_9(const Source; var Dest; Count : Integer);
  procedure MoveJOH_SSE3_9(const Source; var Dest; Count : Integer);

  {RTL Replacement Candidate}
  procedure MoveJOH_RTL_1(const Source; var Dest; Count : Integer);

implementation

uses
  SysUtils, FastcodeCPUID;

var
  PrefetchLimit : Integer; {Used with SSE Moves}

{-------------------------------------------------------------------------}
procedure MoveJOH_PAS_9(const Source; var Dest; Count : Integer);
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
procedure SmallForwardMove_9;
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
end; {SmallForwardMove}

{-------------------------------------------------------------------------}
{Perform Backward Move of 0..36 Bytes}
{On Entry, ECX = Count, EAX = Source, EDX = Dest.  Destroys ECX}
procedure SmallBackwardMove_9;
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
  nop
  nop
@@Done:
end; {SmallBackwardMove}

{-------------------------------------------------------------------------}
{Move ECX Bytes from EAX to EDX, where EAX > EDX and ECX > 36 (SMALLMOVESIZE)}
procedure Forwards_IA32_9;
asm                              
  push    edx
  fild    qword ptr [eax]
  lea     eax, [eax+ecx-8]
  lea     ecx, [ecx+edx-8]
  fild    qword ptr [eax]
  push    ecx
  neg     ecx
  and     edx, -8
  lea     ecx, [ecx+edx+8]
  pop     edx
@FwdLoop:
  fild    qword ptr [eax+ecx]
  fistp   qword ptr [edx+ecx]
  add     ecx, 8
  jl      @FwdLoop
  fistp   qword ptr [edx]
  pop     edx
  fistp   qword ptr [edx]
end; {Forwards_IA32}

{-------------------------------------------------------------------------}
{Move ECX Bytes from EAX to EDX, where EAX < EDX and ECX > 36 (SMALLMOVESIZE)}
procedure Backwards_IA32_9;
asm
  sub     ecx, 8
  push    ecx
  fild    qword ptr [eax+ecx] {Last 8}
  fild    qword ptr [eax] {First 8}
  add     ecx, edx
  and     ecx, -8
  sub     ecx, edx
@BwdLoop:
  fild    qword ptr [eax+ecx]
  fistp   qword ptr [edx+ecx]
  sub     ecx, 8
  jg      @BwdLoop
  pop     ecx
  fistp   qword ptr [edx] {First 8}
  fistp   qword ptr [edx+ecx] {Last 8}
end; {Backwards_IA32}

{-------------------------------------------------------------------------}
{Move ECX Bytes from EAX to EDX, where EAX > EDX and ECX > 36 (SMALLMOVESIZE)}
procedure Forwards_MMX_9;
const
  LARGESIZE = 1024;
asm                     
  cmp     ecx, LARGESIZE
  jge     @FwdLargeMove
  cmp     ecx, 72 {Size at which using MMX becomes worthwhile}
  jl      Forwards_IA32_9
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
  nop
  nop
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
  jmp     SmallForwardMove_9
  nop
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
  call    SmallForwardMove_9
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
procedure Backwards_MMX_9;
asm                                 
  cmp     ecx, 72 {Size at which using MMX becomes worthwhile}
  jl      Backwards_IA32_9
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
  jmp     SmallBackwardMove_9
end; {Backwards_MMX}

{-------------------------------------------------------------------------}
procedure AlignedSSEMove;
asm
@@Loop:
  movaps  xmm0, [eax+8*ecx]
  movaps  xmm1, [eax+8*ecx+16]
  movaps  xmm2, [eax+8*ecx+32]
  movaps  xmm3, [eax+8*ecx+48]
  movaps  [edx+8*ecx], xmm0
  movaps  [edx+8*ecx+16], xmm1
  movaps  [edx+8*ecx+32], xmm2
  movaps  [edx+8*ecx+48], xmm3
  movaps  xmm4, [eax+8*ecx+64]
  movaps  xmm5, [eax+8*ecx+80]
  movaps  xmm6, [eax+8*ecx+96]
  movaps  xmm7, [eax+8*ecx+112]
  movaps  [edx+8*ecx+64], xmm4
  movaps  [edx+8*ecx+80], xmm5
  movaps  [edx+8*ecx+96], xmm6
  movaps  [edx+8*ecx+112], xmm7
  add     ecx, 16
  js      @@Loop
end;

{-------------------------------------------------------------------------}
procedure UnalignedSSEMove;
asm
@@Loop:
  movups  xmm0, [eax+8*ecx]
  movups  xmm1, [eax+8*ecx+16]
  movups  xmm2, [eax+8*ecx+32]
  movups  xmm3, [eax+8*ecx+48]
  movaps  [edx+8*ecx], xmm0
  movaps  [edx+8*ecx+16], xmm1
  movaps  [edx+8*ecx+32], xmm2
  movaps  [edx+8*ecx+48], xmm3
  movups  xmm4, [eax+8*ecx+64]
  movups  xmm5, [eax+8*ecx+80]
  movups  xmm6, [eax+8*ecx+96]
  movups  xmm7, [eax+8*ecx+112]
  movaps  [edx+8*ecx+64], xmm4
  movaps  [edx+8*ecx+80], xmm5
  movaps  [edx+8*ecx+96], xmm6
  movaps  [edx+8*ecx+112], xmm7
  add     ecx, 16
  js      @@Loop
end;

{-------------------------------------------------------------------------}
procedure LargeAlignedSSEMove;
const
  Prefetch = 512;
asm
@@Loop:
  prefetchnta [eax+8*ecx+Prefetch]
  prefetchnta [eax+8*ecx+Prefetch+64]
  movaps  xmm0, [eax+8*ecx]
  movaps  xmm1, [eax+8*ecx+16]
  movaps  xmm2, [eax+8*ecx+32]
  movaps  xmm3, [eax+8*ecx+48]
  movntps [edx+8*ecx], xmm0
  movntps [edx+8*ecx+16], xmm1
  movntps [edx+8*ecx+32], xmm2
  movntps [edx+8*ecx+48], xmm3
  movaps  xmm4, [eax+8*ecx+64]
  movaps  xmm5, [eax+8*ecx+80]
  movaps  xmm6, [eax+8*ecx+96]
  movaps  xmm7, [eax+8*ecx+112]
  movntps [edx+8*ecx+64], xmm4
  movntps [edx+8*ecx+80], xmm5
  movntps [edx+8*ecx+96], xmm6
  movntps [edx+8*ecx+112], xmm7
  add     ecx, 16
  js      @@Loop
  sfence
end;

{-------------------------------------------------------------------------}
procedure LargeUnalignedSSEMove;
const
  Prefetch = 512;
asm
@@Loop:
  prefetchnta [eax+8*ecx+Prefetch]
  prefetchnta [eax+8*ecx+Prefetch+64]
  movups  xmm0, [eax+8*ecx]
  movups  xmm1, [eax+8*ecx+16]
  movups  xmm2, [eax+8*ecx+32]
  movups  xmm3, [eax+8*ecx+48]
  movntps [edx+8*ecx], xmm0
  movntps [edx+8*ecx+16], xmm1
  movntps [edx+8*ecx+32], xmm2
  movntps [edx+8*ecx+48], xmm3
  movups  xmm4, [eax+8*ecx+64]
  movups  xmm5, [eax+8*ecx+80]
  movups  xmm6, [eax+8*ecx+96]
  movups  xmm7, [eax+8*ecx+112]
  movntps [edx+8*ecx+64], xmm4
  movntps [edx+8*ecx+80], xmm5
  movntps [edx+8*ecx+96], xmm6
  movntps [edx+8*ecx+112], xmm7
  add     ecx, 16
  js      @@Loop
  sfence
end;

{-------------------------------------------------------------------------}
{Dest MUST be 16-Byes Aligned, Count MUST be multiple of 16 }
procedure AlignedFwdMoveSSE_9(const Source; var Dest; Count: Integer);
asm
  push    ebx
  mov     ebx, ecx
  and     ecx, -128             {No of Bytes to Block Move (Multiple of 128)}
  add     eax, ecx              {End of Source Blocks}
  add     edx, ecx              {End of Dest Blocks}
  shr     ecx, 3                {No of QWORD's to Block Move}
  neg     ecx
  cmp     ecx, PrefetchLimit    {Count > Limit - Use Prefetch}
  jl      @@Large
  test    eax, 15               {Check if Both Source/Dest are Aligned}
  jnz     @@SmallUnaligned
  call    AlignedSSEMove        {Both Source and Dest 16-Byte Aligned}
  jmp     @Remainder
@@SmallUnaligned:               {Source Not 16-Byte Aligned}
  call    UnalignedSSEMove
  jmp     @Remainder
@@Large:
  test    eax, 15               {Check if Both Source/Dest Aligned}
  jnz     @@LargeUnaligned
  call    LargeAlignedSSEMove   {Both Source and Dest 16-Byte Aligned}
  jmp     @Remainder
@@LargeUnaligned:               {Source Not 16-Byte Aligned}
  call    LargeUnalignedSSEMove
@Remainder:
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
end; {AlignedFwdMoveSSE}

{-------------------------------------------------------------------------}
{Move ECX Bytes from EAX to EDX, where EAX > EDX and ECX > 36 (SMALLMOVESIZE)}
procedure Forwards_SSE_9;
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
  jmp     SmallForwardMove_9
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
  jmp     SmallForwardMove_9
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
  call    SmallForwardMove_9
  mov     ecx, ebx
@FwdLargeAligned:
  and     ecx, -16
  sub     ebx, ecx {EBX = Remainder}
  push    edx
  push    eax
  push    ecx
  call    AlignedFwdMoveSSE_9
  pop     ecx
  pop     eax
  pop     edx
  add     ecx, ebx
  add     eax, ecx
  add     edx, ecx
  mov     ecx, ebx
  pop     ebx
  jmp     SmallForwardMove_9
end; {Forwards_SSE}

{-------------------------------------------------------------------------}
{Move ECX Bytes from EAX to EDX, where EAX < EDX and ECX > 36 (SMALLMOVESIZE)}
procedure Backwards_SSE_9;
asm
  cmp     ecx, SMALLMOVESIZE+32
  jg      @BwdMoveSSE
  sub     ecx, 32
  movups  xmm1, [eax+ecx]
  movups  xmm2, [eax+ecx+16]
  movups  [edx+ecx], xmm1
  movups  [edx+ecx+16], xmm2
  jmp     SmallBackwardMove_9
  nop
  nop
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
  jmp     SmallBackwardMove_9
end; {Backwards_SSE}

{-------------------------------------------------------------------------}
procedure AlignedSSE2Move;
asm
@@Loop:
  movdqa  xmm0, [eax+8*ecx]
  movdqa  xmm1, [eax+8*ecx+16]
  movdqa  xmm2, [eax+8*ecx+32]
  movdqa  xmm3, [eax+8*ecx+48]
  movdqa  [edx+8*ecx], xmm0
  movdqa  [edx+8*ecx+16], xmm1
  movdqa  [edx+8*ecx+32], xmm2
  movdqa  [edx+8*ecx+48], xmm3
  movdqa  xmm4, [eax+8*ecx+64]
  movdqa  xmm5, [eax+8*ecx+80]
  movdqa  xmm6, [eax+8*ecx+96]
  movdqa  xmm7, [eax+8*ecx+112]
  movdqa  [edx+8*ecx+64], xmm4
  movdqa  [edx+8*ecx+80], xmm5
  movdqa  [edx+8*ecx+96], xmm6
  movdqa  [edx+8*ecx+112], xmm7
  add     ecx, 16
  js      @@Loop
end;

{-------------------------------------------------------------------------}
procedure UnalignedSSE2Move;
asm
@@Loop:
  movdqu  xmm0, [eax+8*ecx]
  movdqu  xmm1, [eax+8*ecx+16]
  movdqu  xmm2, [eax+8*ecx+32]
  movdqu  xmm3, [eax+8*ecx+48]
  movdqa  [edx+8*ecx], xmm0
  movdqa  [edx+8*ecx+16], xmm1
  movdqa  [edx+8*ecx+32], xmm2
  movdqa  [edx+8*ecx+48], xmm3
  movdqu  xmm4, [eax+8*ecx+64]
  movdqu  xmm5, [eax+8*ecx+80]
  movdqu  xmm6, [eax+8*ecx+96]
  movdqu  xmm7, [eax+8*ecx+112]
  movdqa  [edx+8*ecx+64], xmm4
  movdqa  [edx+8*ecx+80], xmm5
  movdqa  [edx+8*ecx+96], xmm6
  movdqa  [edx+8*ecx+112], xmm7
  add     ecx, 16
  js      @@Loop
end;

{-------------------------------------------------------------------------}
procedure LargeAlignedSSE2Move;
const
  Prefetch = 512;
asm
@@Loop:
  prefetchnta [eax+8*ecx+Prefetch]
  prefetchnta [eax+8*ecx+Prefetch+64]
  movdqa  xmm0, [eax+8*ecx]
  movdqa  xmm1, [eax+8*ecx+16]
  movdqa  xmm2, [eax+8*ecx+32]
  movdqa  xmm3, [eax+8*ecx+48]
  movntdq [edx+8*ecx], xmm0
  movntdq [edx+8*ecx+16], xmm1
  movntdq [edx+8*ecx+32], xmm2
  movntdq [edx+8*ecx+48], xmm3
  movdqa  xmm4, [eax+8*ecx+64]
  movdqa  xmm5, [eax+8*ecx+80]
  movdqa  xmm6, [eax+8*ecx+96]
  movdqa  xmm7, [eax+8*ecx+112]
  movntdq [edx+8*ecx+64], xmm4
  movntdq [edx+8*ecx+80], xmm5
  movntdq [edx+8*ecx+96], xmm6
  movntdq [edx+8*ecx+112], xmm7
  add     ecx, 16
  js      @@Loop
  sfence
end;

{-------------------------------------------------------------------------}
procedure LargeUnalignedSSE2Move;
const
  Prefetch = 512;
asm
@@Loop:
  prefetchnta [eax+8*ecx+Prefetch]
  prefetchnta [eax+8*ecx+Prefetch+64]
  movdqu  xmm0, [eax+8*ecx]
  movdqu  xmm1, [eax+8*ecx+16]
  movdqu  xmm2, [eax+8*ecx+32]
  movdqu  xmm3, [eax+8*ecx+48]
  movntdq [edx+8*ecx], xmm0
  movntdq [edx+8*ecx+16], xmm1
  movntdq [edx+8*ecx+32], xmm2
  movntdq [edx+8*ecx+48], xmm3
  movdqu  xmm4, [eax+8*ecx+64]
  movdqu  xmm5, [eax+8*ecx+80]
  movdqu  xmm6, [eax+8*ecx+96]
  movdqu  xmm7, [eax+8*ecx+112]
  movntdq [edx+8*ecx+64], xmm4
  movntdq [edx+8*ecx+80], xmm5
  movntdq [edx+8*ecx+96], xmm6
  movntdq [edx+8*ecx+112], xmm7
  add     ecx, 16
  js      @@Loop
  sfence
end;

{-------------------------------------------------------------------------}
{Dest MUST be 16-Byes Aligned, Count MUST be multiple of 16 }
procedure AlignedFwdMoveSSE2_9(const Source; var Dest; Count: Integer);
asm
  push    ebx
  mov     ebx, ecx
  and     ecx, -128             {No of Bytes to Block Move (Multiple of 128)}
  add     eax, ecx              {End of Source Blocks}
  add     edx, ecx              {End of Dest Blocks}
  shr     ecx, 3                {No of QWORD's to Block Move}
  neg     ecx
  cmp     ecx, PrefetchLimit    {Count > Limit - Use Prefetch}
  jl      @@Large
  test    eax, 15               {Check if Both Source/Dest are Aligned}
  jnz     @@SmallUnaligned
  call    AlignedSSE2Move       {Both Source and Dest 16-Byte Aligned}
  jmp     @Remainder
@@SmallUnaligned:               {Source Not 16-Byte Aligned}
  call    UnalignedSSE2Move
  jmp     @Remainder
@@Large:
  test    eax, 15               {Check if Both Source/Dest Aligned}
  jnz     @@LargeUnaligned
  call    LargeAlignedSSE2Move  {Both Source and Dest 16-Byte Aligned}
  jmp     @Remainder
@@LargeUnaligned:               {Source Not 16-Byte Aligned}
  call    LargeUnalignedSSE2Move
@Remainder:
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
end; {AlignedFwdMoveSSE2}

{-------------------------------------------------------------------------}
{Move ECX Bytes from EAX to EDX, where EAX > EDX and ECX > 36 (SMALLMOVESIZE)}
procedure Forwards_SSE2_9;
const
  LARGESIZE = 2048;
asm
  cmp     ecx, LARGESIZE
  jge     @FwdLargeMove
  cmp     ecx, SMALLMOVESIZE+32
  movdqu  xmm0, [eax]
  jg      @FwdMoveSSE2
  movdqu  xmm1, [eax+16]
  movdqu  [edx], xmm0
  movdqu  [edx+16], xmm1
  add     eax, ecx
  add     edx, ecx
  sub     ecx, 32
  jmp     SmallForwardMove_9
  nop
  nop
  nop
@FwdMoveSSE2:
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
@FwdLoopSSE2:
  movdqu  xmm1, [eax+ecx-32]
  movdqu  xmm2, [eax+ecx-16]
  movdqa  [edx+ecx-32], xmm1
  movdqa  [edx+ecx-16], xmm2
  add     ecx, 32
  jle     @FwdLoopSSE2
  movdqu  [ebx], xmm0 {First 16 Bytes}
  neg     ecx
  add     ecx, 32
  pop     ebx
  jmp     SmallForwardMove_9
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
  call    SmallForwardMove_9
  mov     ecx, ebx
@FwdLargeAligned:
  and     ecx, -16
  sub     ebx, ecx {EBX = Remainder}
  push    edx
  push    eax
  push    ecx
  call    AlignedFwdMoveSSE2_9
  pop     ecx
  pop     eax
  pop     edx
  add     ecx, ebx
  add     eax, ecx
  add     edx, ecx
  mov     ecx, ebx
  pop     ebx
  jmp     SmallForwardMove_9
end; {Forwards_SSE2}

{-------------------------------------------------------------------------}
{Move ECX Bytes from EAX to EDX, where EAX < EDX and ECX > 36 (SMALLMOVESIZE)}
procedure Backwards_SSE2_9;
asm
  cmp     ecx, SMALLMOVESIZE+32
  jg      @BwdMoveSSE2
  sub     ecx, 32
  movdqu  xmm1, [eax+ecx]
  movdqu  xmm2, [eax+ecx+16]
  movdqu  [edx+ecx], xmm1
  movdqu  [edx+ecx+16], xmm2
  jmp     SmallBackwardMove_9
  nop
@BwdMoveSSE2:                     
  push    ebx
  movdqu  xmm0, [eax+ecx-16] {Last 16 Bytes}
  {Align Writes}
  lea     ebx, [edx+ecx]
  and     ebx, 15
  sub     ecx, ebx
  add     ebx, ecx
  {Now Aligned}
  sub     ecx, 32
@BwdLoop:
  movdqu  xmm1, [eax+ecx]
  movdqu  xmm2, [eax+ecx+16]
  movdqa  [edx+ecx], xmm1
  movdqa  [edx+ecx+16], xmm2
  sub     ecx, 32
  jge     @BwdLoop
  movdqu  [edx+ebx-16], xmm0  {Last 16 Bytes}
  add     ecx, 32
  pop     ebx
  jmp     SmallBackwardMove_9
end; {Backwards_SSE2}

{-------------------------------------------------------------------------}
procedure AlignedSSE3Move; {Same as SSE2 Version}
asm
@@Loop:
  movdqa  xmm0, [eax+8*ecx]
  movdqa  xmm1, [eax+8*ecx+16]
  movdqa  xmm2, [eax+8*ecx+32]
  movdqa  xmm3, [eax+8*ecx+48]
  movdqa  [edx+8*ecx], xmm0
  movdqa  [edx+8*ecx+16], xmm1
  movdqa  [edx+8*ecx+32], xmm2
  movdqa  [edx+8*ecx+48], xmm3
  movdqa  xmm4, [eax+8*ecx+64]
  movdqa  xmm5, [eax+8*ecx+80]
  movdqa  xmm6, [eax+8*ecx+96]
  movdqa  xmm7, [eax+8*ecx+112]
  movdqa  [edx+8*ecx+64], xmm4
  movdqa  [edx+8*ecx+80], xmm5
  movdqa  [edx+8*ecx+96], xmm6
  movdqa  [edx+8*ecx+112], xmm7
  add     ecx, 16
  js      @@Loop
end;

{-------------------------------------------------------------------------}
procedure UnalignedSSE3Move;
asm
@@Loop:
{$IFDEF VER170}
  lddqu   xmm0, [eax+8*ecx]
  lddqu   xmm1, [eax+8*ecx+16]
  lddqu   xmm2, [eax+8*ecx+32]
  lddqu   xmm3, [eax+8*ecx+48]
{$ELSE}
  DB      $F2,$0F,$F0,$04,$C8
  DB      $F2,$0F,$F0,$4C,$C8,$10
  DB      $F2,$0F,$F0,$54,$C8,$20
  DB      $F2,$0F,$F0,$5C,$C8,$30
{$ENDIF}
  movdqa  [edx+8*ecx], xmm0
  movdqa  [edx+8*ecx+16], xmm1
  movdqa  [edx+8*ecx+32], xmm2
  movdqa  [edx+8*ecx+48], xmm3
{$IFDEF VER170}
  lddqu   xmm4, [eax+8*ecx+64]
  lddqu   xmm5, [eax+8*ecx+80]
  lddqu   xmm6, [eax+8*ecx+96]
  lddqu   xmm7, [eax+8*ecx+112]
{$ELSE}
  DB      $F2,$0F,$F0,$64,$C8,$40
  DB      $F2,$0F,$F0,$6C,$C8,$50
  DB      $F2,$0F,$F0,$74,$C8,$60
  DB      $F2,$0F,$F0,$7C,$C8,$70
{$ENDIF}
  movdqa  [edx+8*ecx+64], xmm4
  movdqa  [edx+8*ecx+80], xmm5
  movdqa  [edx+8*ecx+96], xmm6
  movdqa  [edx+8*ecx+112], xmm7
  add     ecx, 16
  js      @@Loop
end;

{-------------------------------------------------------------------------}
procedure LargeAlignedSSE3Move; {Same as SSE2 Version}
const
  Prefetch = 512;
asm
@@Loop:
  prefetchnta [eax+8*ecx+Prefetch]
  prefetchnta [eax+8*ecx+Prefetch+64]
  movdqa  xmm0, [eax+8*ecx]
  movdqa  xmm1, [eax+8*ecx+16]
  movdqa  xmm2, [eax+8*ecx+32]
  movdqa  xmm3, [eax+8*ecx+48]
  movntdq [edx+8*ecx], xmm0
  movntdq [edx+8*ecx+16], xmm1
  movntdq [edx+8*ecx+32], xmm2
  movntdq [edx+8*ecx+48], xmm3
  movdqa  xmm4, [eax+8*ecx+64]
  movdqa  xmm5, [eax+8*ecx+80]
  movdqa  xmm6, [eax+8*ecx+96]
  movdqa  xmm7, [eax+8*ecx+112]
  movntdq [edx+8*ecx+64], xmm4
  movntdq [edx+8*ecx+80], xmm5
  movntdq [edx+8*ecx+96], xmm6
  movntdq [edx+8*ecx+112], xmm7
  add     ecx, 16
  js      @@Loop
  sfence
end;

{-------------------------------------------------------------------------}
procedure LargeUnalignedSSE3Move;
const
  Prefetch = 512;
asm
@@Loop:
  prefetchnta [eax+8*ecx+Prefetch]
  prefetchnta [eax+8*ecx+Prefetch+64]
{$IFDEF VER170}
  lddqu   xmm0, [eax+8*ecx]
  lddqu   xmm1, [eax+8*ecx+16]
  lddqu   xmm2, [eax+8*ecx+32]
  lddqu   xmm3, [eax+8*ecx+48]
{$ELSE}
  DB      $F2,$0F,$F0,$04,$C8
  DB      $F2,$0F,$F0,$4C,$C8,$10
  DB      $F2,$0F,$F0,$54,$C8,$20
  DB      $F2,$0F,$F0,$5C,$C8,$30
{$ENDIF}
  movntdq [edx+8*ecx], xmm0
  movntdq [edx+8*ecx+16], xmm1
  movntdq [edx+8*ecx+32], xmm2
  movntdq [edx+8*ecx+48], xmm3
{$IFDEF VER170}
  lddqu   xmm4, [eax+8*ecx+64]
  lddqu   xmm5, [eax+8*ecx+80]
  lddqu   xmm6, [eax+8*ecx+96]
  lddqu   xmm7, [eax+8*ecx+112]
{$ELSE}
  DB      $F2,$0F,$F0,$64,$C8,$40
  DB      $F2,$0F,$F0,$6C,$C8,$50
  DB      $F2,$0F,$F0,$74,$C8,$60
  DB      $F2,$0F,$F0,$7C,$C8,$70
{$ENDIF}
  movntdq [edx+8*ecx+64], xmm4
  movntdq [edx+8*ecx+80], xmm5
  movntdq [edx+8*ecx+96], xmm6
  movntdq [edx+8*ecx+112], xmm7
  add     ecx, 16
  js      @@Loop
  sfence
end;

{-------------------------------------------------------------------------}
{Dest MUST be 16-Byes Aligned, Count MUST be multiple of 16 }
procedure AlignedFwdMoveSSE3_9(const Source; var Dest; Count: Integer);
asm
  push    ebx
  mov     ebx, ecx
  and     ecx, -128             {No of Bytes to Block Move (Multiple of 128)}
  add     eax, ecx              {End of Source Blocks}
  add     edx, ecx              {End of Dest Blocks}
  shr     ecx, 3                {No of QWORD's to Block Move}
  neg     ecx
  cmp     ecx, PrefetchLimit    {Count > Limit - Use Prefetch}
  jl      @@Large
  test    eax, 15               {Check if Both Source/Dest are Aligned}
  jnz     @@SmallUnaligned
  call    AlignedSSE3Move       {Both Source and Dest 16-Byte Aligned}
  jmp     @Remainder
@@SmallUnaligned:               {Source Not 16-Byte Aligned}
  call    UnalignedSSE3Move
  jmp     @Remainder
@@Large:
  test    eax, 15               {Check if Both Source/Dest Aligned}
  jnz     @@LargeUnaligned
  call    LargeAlignedSSE3Move  {Both Source and Dest 16-Byte Aligned}
  jmp     @Remainder
@@LargeUnaligned:               {Source Not 16-Byte Aligned}
  call    LargeUnalignedSSE3Move
@Remainder:
  and     ebx, $7F              {Remainder (0..112 - Multiple of 16)}
  jz      @@Done
  add     eax, ebx
  add     edx, ebx
  neg     ebx
@@RemainderLoop:
{$IFDEF VER170}
  lddqu   xmm0, [eax+ebx]
{$ELSE}
  DB      $F2,$0F,$F0,$04,$03
{$ENDIF}
  movdqa  [edx+ebx], xmm0
  add     ebx, 16
  jnz     @@RemainderLoop
@@Done:
  pop     ebx
end; {AlignedFwdMoveSSE3}

{-------------------------------------------------------------------------}
{Move ECX Bytes from EAX to EDX, where EAX > EDX and ECX > 36 (SMALLMOVESIZE)}
procedure Forwards_SSE3_9;
const
  LARGESIZE = 2048;
asm
  cmp     ecx, LARGESIZE
  jge     @FwdLargeMove
  cmp     ecx, SMALLMOVESIZE+32
{$IFDEF VER170}
  lddqu   xmm0, [eax]
{$ELSE}
  DB      $F2,$0F,$F0,$00
{$ENDIF}
  jg      @FwdMoveSSE3
{$IFDEF VER170}
  lddqu   xmm1, [eax+16]
{$ELSE}
  DB      $F2,$0F,$F0,$48,$10
{$ENDIF}
  movdqu  [edx], xmm0
  movdqu  [edx+16], xmm1
  add     eax, ecx
  add     edx, ecx
  sub     ecx, 32
  jmp     SmallForwardMove_9
  nop
  nop
  nop
@FwdMoveSSE3:
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
@FwdLoopSSE3:
{$IFDEF VER170}
  lddqu   xmm1, [eax+ecx-32]
  lddqu   xmm2, [eax+ecx-16]
{$ELSE}
  DB      $F2,$0F,$F0,$4C,$01,$E0
  DB      $F2,$0F,$F0,$54,$01,$F0
{$ENDIF}
  movdqa  [edx+ecx-32], xmm1
  movdqa  [edx+ecx-16], xmm2
  add     ecx, 32
  jle     @FwdLoopSSE3
  movdqu  [ebx], xmm0 {First 16 Bytes}
  neg     ecx
  add     ecx, 32
  pop     ebx
  jmp     SmallForwardMove_9
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
  call    SmallForwardMove_9
  mov     ecx, ebx
@FwdLargeAligned:
  and     ecx, -16
  sub     ebx, ecx {EBX = Remainder}
  push    edx
  push    eax
  push    ecx
  call    AlignedFwdMoveSSE3_9
  pop     ecx
  pop     eax
  pop     edx
  add     ecx, ebx
  add     eax, ecx
  add     edx, ecx
  mov     ecx, ebx
  pop     ebx
  jmp     SmallForwardMove_9
end; {Forwards_SSE3}

{-------------------------------------------------------------------------}
{Move ECX Bytes from EAX to EDX, where EAX < EDX and ECX > 36 (SMALLMOVESIZE)}
procedure Backwards_SSE3_9;
asm
  cmp     ecx, SMALLMOVESIZE+32
  jg      @BwdMoveSSE3
  sub     ecx, 32
{$IFDEF VER170}
  lddqu   xmm1, [eax+ecx]
  lddqu   xmm2, [eax+ecx+16]
{$ELSE}
  DB      $F2,$0F,$F0,$0C,$01
  DB      $F2,$0F,$F0,$54,$01,$10
{$ENDIF}
  movdqu  [edx+ecx], xmm1
  movdqu  [edx+ecx+16], xmm2
  jmp     SmallBackwardMove_9
  nop
@BwdMoveSSE3:
  push    ebx
{$IFDEF VER170}
  lddqu   xmm0, [eax+ecx-16] {Last 16 Bytes}
{$ELSE}
  DB      $F2,$0F,$F0,$44,$01,$F0
{$ENDIF}
  {Align Writes}
  lea     ebx, [edx+ecx]
  and     ebx, 15
  sub     ecx, ebx
  add     ebx, ecx
  {Now Aligned}
  sub     ecx, 32
@BwdLoop:
{$IFDEF VER170}
  lddqu   xmm1, [eax+ecx]
  lddqu   xmm2, [eax+ecx+16]
{$ELSE}
  DB      $F2,$0F,$F0,$0C,$01
  DB      $F2,$0F,$F0,$54,$01,$10
{$ENDIF}
  movdqa  [edx+ecx], xmm1
  movdqa  [edx+ecx+16], xmm2
  sub     ecx, 32
  jge     @BwdLoop
  movdqu  [edx+ebx-16], xmm0  {Last 16 Bytes}
  add     ecx, 32
  pop     ebx
  jmp     SmallBackwardMove_9
end; {Backwards_SSE3}

{-------------------------------------------------------------------------}
{Move using IA32 Instruction Set Only}
procedure MoveJOH_IA32_9(const Source; var Dest; Count : Integer);
asm
  cmp     ecx, SMALLMOVESIZE
  ja      @Large {Count > SMALLMOVESIZE or Count < 0}
  cmp     eax, edx
  jbe     @SmallCheck
  add     eax, ecx
  add     edx, ecx
  jmp     SmallForwardMove_9
@SmallCheck:
  jne     SmallBackwardMove_9
  ret {For Compatibility with Delphi's move for Source = Dest}
@Large:
  jng     @Done {For Compatibility with Delphi's move for Count < 0}
  cmp     eax, edx
  ja      Forwards_IA32_9 
  je      @Done {For Compatibility with Delphi's move for Source = Dest}
  sub     edx, ecx
  cmp     eax, edx
  lea     edx, [edx+ecx]
  jna     Forwards_IA32_9
  jmp     Backwards_IA32_9 {Source/Dest Overlap}
@Done:
end; {MoveJOH_IA32}

{-------------------------------------------------------------------------}
{Move using MMX Instruction Set}
procedure MoveJOH_MMX_9(const Source; var Dest; Count : Integer);
asm
  cmp     ecx, SMALLMOVESIZE
  ja      @Large {Count > SMALLMOVESIZE or Count < 0}
  cmp     eax, edx
  jbe     @SmallCheck
  add     eax, ecx
  add     edx, ecx
  jmp     SmallForwardMove_9
@SmallCheck:
  jne     SmallBackwardMove_9
  ret {For Compatibility with Delphi's move for Source = Dest}
@Large:
  jng     @Done {For Compatibility with Delphi's move for Count < 0}
  cmp     eax, edx
  ja      Forwards_MMX_9
  je      @Done {For Compatibility with Delphi's move for Source = Dest}
  sub     edx, ecx
  cmp     eax, edx
  lea     edx, [edx+ecx]
  jna     Forwards_MMX_9
  jmp     Backwards_MMX_9 {Source/Dest Overlap}
@Done:
end; {MoveJOH_MMX}

{-------------------------------------------------------------------------}
{Move using SSE Instruction Set}
procedure MoveJOH_SSE_9(const Source; var Dest; Count : Integer);
asm
  cmp     ecx, SMALLMOVESIZE
  ja      @Large {Count > SMALLMOVESIZE or Count < 0}
  cmp     eax, edx
  jbe     @SmallCheck
  add     eax, ecx
  add     edx, ecx
  jmp     SmallForwardMove_9
@SmallCheck:
  jne     SmallBackwardMove_9
  ret {For Compatibility with Delphi's move for Source = Dest}
@Large:
  jng     @Done {For Compatibility with Delphi's move for Count < 0}
  cmp     eax, edx
  ja      Forwards_SSE_9
  je      @Done {For Compatibility with Delphi's move for Source = Dest}
  sub     edx, ecx
  cmp     eax, edx
  lea     edx, [edx+ecx]
  jna     Forwards_SSE_9
  jmp     Backwards_SSE_9 {Source/Dest Overlap}
@Done:
end; {MoveJOH_SSE}

{-------------------------------------------------------------------------}
{Move using SSE2 Instruction Set}
procedure MoveJOH_SSE2_9(const Source; var Dest; Count : Integer);
asm
  cmp     ecx, SMALLMOVESIZE
  ja      @Large {Count > SMALLMOVESIZE or Count < 0}
  cmp     eax, edx
  jbe     @SmallCheck
  add     eax, ecx
  add     edx, ecx
  jmp     SmallForwardMove_9
@SmallCheck:
  jne     SmallBackwardMove_9
  ret {For Compatibility with Delphi's move for Source = Dest}
@Large:
  jng     @Done {For Compatibility with Delphi's move for Count < 0}
  cmp     eax, edx
  ja      Forwards_SSE2_9
  je      @Done {For Compatibility with Delphi's move for Source = Dest}
  sub     edx, ecx
  cmp     eax, edx
  lea     edx, [edx+ecx]
  jna     Forwards_SSE2_9
  jmp     Backwards_SSE2_9 {Source/Dest Overlap}
@Done:
end; {MoveJOH_SSE2}

{-------------------------------------------------------------------------}
{Move using SSE3 Instruction Set}
procedure MoveJOH_SSE3_9(const Source; var Dest; Count : Integer);
asm
  cmp     ecx, SMALLMOVESIZE
  ja      @Large {Count > SMALLMOVESIZE or Count < 0}
  cmp     eax, edx
  jbe     @SmallCheck
  add     eax, ecx
  add     edx, ecx
  jmp     SmallForwardMove_9
@SmallCheck:
  jne     SmallBackwardMove_9
  ret {For Compatibility with Delphi's move for Source = Dest}
@Large:
  jng     @Done {For Compatibility with Delphi's move for Count < 0}
  cmp     eax, edx
  ja      Forwards_SSE3_9
  je      @Done {For Compatibility with Delphi's move for Source = Dest}
  sub     edx, ecx
  cmp     eax, edx
  lea     edx, [edx+ecx]
  jna     Forwards_SSE3_9
  jmp     Backwards_SSE3_9 {Source/Dest Overlap}
@Done:
end; {MoveJOH_SSE3}

procedure MoveJOH_RTL_1(const Source; var Dest; Count : Integer);
asm  {Small (253 Byte) RTL Replacement Candidate}
  cmp     eax, edx
  je      @@Exit {Source = Dest}
  cmp     ecx, 32
  ja      @@LargeMove {Count > 32 or Count < 0}
  sub     ecx, 8
  jg      @@SmallMove
@@TinyMove: {0..8 Byte Move}
  jmp     dword ptr [@@JumpTable+32+ecx*4]
@@SmallMove: {9..32 Byte Move}
  fild    qword ptr [eax+ecx] {Load Last 8}
  fild    qword ptr [eax] {Load First 8}
  cmp     ecx, 8
  jle     @@Small16
  fild    qword ptr [eax+8] {Load Second 8}
  cmp     ecx, 16
  jle     @@Small24
  fild    qword ptr [eax+16] {Load Third 8}
  fistp   qword ptr [edx+16] {Save Third 8}
@@Small24:
  fistp   qword ptr [edx+8] {Save Second 8}
@@Small16:
  fistp   qword ptr [edx] {Save First 8}
  fistp   qword ptr [edx+ecx] {Save Last 8}
@@Exit:
  ret
  nop {4-Byte Align JumpTable}
  nop
@@JumpTable: {4-Byte Aligned}
  dd      @@Exit, @@M01, @@M02, @@M03, @@M04, @@M05, @@M06, @@M07, @@M08
@@LargeForwardMove: {4-Byte Aligned}
  push    edx
  fild    qword ptr [eax] {First 8}
  lea     eax, [eax+ecx-8]
  lea     ecx, [ecx+edx-8]
  fild    qword ptr [eax] {Last 8}
  push    ecx
  neg     ecx
  and     edx, -8 {8-Byte Align Writes}
  lea     ecx, [ecx+edx+8]
  pop     edx
@FwdLoop:
  fild    qword ptr [eax+ecx]
  fistp   qword ptr [edx+ecx]
  add     ecx, 8
  jl      @FwdLoop
  fistp   qword ptr [edx] {Last 8}
  pop     edx
  fistp   qword ptr [edx] {First 8}
  ret
@@LargeMove:
  jng     @@LargeDone {Count < 0}
  cmp     eax, edx
  ja      @@LargeForwardMove
  sub     edx, ecx
  cmp     eax, edx
  lea     edx, [edx+ecx]
  jna     @@LargeForwardMove
  sub     ecx, 8 {Backward Move}
  push    ecx
  fild    qword ptr [eax+ecx] {Last 8}
  fild    qword ptr [eax] {First 8}
  add     ecx, edx
  and     ecx, -8 {8-Byte Align Writes}
  sub     ecx, edx
@BwdLoop:
  fild    qword ptr [eax+ecx]
  fistp   qword ptr [edx+ecx]
  sub     ecx, 8
  jg      @BwdLoop
  pop     ecx
  fistp   qword ptr [edx] {First 8}
  fistp   qword ptr [edx+ecx] {Last 8}
@@LargeDone:
  ret
@@M01:
  movzx   ecx, [eax]
  mov     [edx], cl
  ret
@@M02:
  movzx   ecx, word ptr [eax]
  mov     [edx], cx
  ret
@@M03:
  mov     cx, [eax]
  mov     al, [eax+2]
  mov     [edx], cx
  mov     [edx+2], al
  ret
@@M04:
  mov     ecx, [eax]
  mov     [edx], ecx
  ret
@@M05:
  mov     ecx, [eax]
  mov     al, [eax+4]
  mov     [edx], ecx
  mov     [edx+4], al
  ret
@@M06:
  mov     ecx, [eax]
  mov     ax, [eax+4]
  mov     [edx], ecx
  mov     [edx+4], ax
  ret
@@M07:
  mov     ecx, [eax]
  mov     eax, [eax+3]
  mov     [edx], ecx
  mov     [edx+3], eax
  ret
@@M08:
  fild    qword ptr [eax]
  fistp   qword ptr [edx]
end; {MoveJOH_IA32}

initialization
  PrefetchLimit := (CPU.L2CacheSize div 16) * -1024; {Used within SSE Moves}
end.

