{****************************************************************************************

  TOPLIBRARY v3.53 - MemoryCopy and MemoryZero SSE2 Optimized Routines

****************************************************************************************}
unit TOPLib_SSE2;

interface

{$IFNDEF TOPDEBUG} // Debugging off unless you use the TOPDEBUG directive
{$D-,L-}
{$ENDIF}

{$IFOPT R+}{$DEFINE RPLUS}{$ENDIF} // Make sure we can set Range Checking back to the user-selected setting in the IDE

procedure SSE2MemoryCopy(dst, src: pointer; Size: Cardinal); // Upwards copy from src to src+size for SIZES > 15 BYTES!!
procedure SSE2MemoryZero(Src: pointer; Size: Cardinal); // Zero memory
procedure ForceSSE2(const SSE2On: Boolean);

var
  IsSSE2supported: Boolean = False;

implementation

type
  TByteArray = array of Byte;

procedure ForceSSE2(const SSE2On: Boolean);
begin
  IsSSE2supported := SSE2On;
end;


procedure SSE2AlignedMemCopy128(dst, src: pointer; A128bytesmultiple: Integer);
asm
     @Copy_loop:
     PREFETCHNTA 32[EDX] // Pre-fetch data
     PREFETCHNTA 64[EDX] // Pre-fetch data
     PREFETCHNTA 96[EDX] // Pre-fetch data
     Movdqa xmm0,[edx]
     Movdqa xmm1,[edx+16]
     Movdqa xmm2,[edx+32]
     Movdqa xmm3,[edx+48]
     Movdqa xmm4,[edx+64]
     Movdqa xmm5,[edx+80]
     Movdqa xmm6,[edx+96]
     Movdqa xmm7,[edx+112]
     Movdqu [eax],xmm0
     Movdqu [eax+16],xmm1
     Movdqu [eax+32],xmm2
     Movdqu [eax+48],xmm3
     Movdqu [eax+64],xmm4
     Movdqu [eax+80],xmm5
     Movdqu [eax+96],xmm6
     Movdqu [eax+112],xmm7
     Add edx,128
     Add eax,128
     Dec ecx
     Jnz @Copy_loop
     emms
end;


procedure SSE2DoubleAlignedMemCopy128(dst, src: pointer; A128bytesmultiple: Integer);
asm
     @Copy_loop:
     PREFETCHNTA 32[EDX] // Pre-fetch data
     PREFETCHNTA 64[EDX] // Pre-fetch data
     PREFETCHNTA 96[EDX] // Pre-fetch data
     Movdqa xmm0,[edx]
     Movdqa xmm1,[edx+16]
     Movdqa xmm2,[edx+32]
     Movdqa xmm3,[edx+48]
     Movdqa xmm4,[edx+64]
     Movdqa xmm5,[edx+80]
     Movdqa xmm6,[edx+96]
     Movdqa xmm7,[edx+112]
     Movdqa [eax],xmm0
     Movdqa [eax+16],xmm1
     Movdqa [eax+32],xmm2
     Movdqa [eax+48],xmm3
     Movdqa [eax+64],xmm4
     Movdqa [eax+80],xmm5
     Movdqa [eax+96],xmm6
     Movdqa [eax+112],xmm7
     Add edx,128
     Add eax,128
     Dec ecx
     Jnz @Copy_loop
     emms
end;

procedure SSE2AlignedMemCopy16(dst, src: pointer; A16bytesmultiple: Integer);
asm
     @Copy_loop:
     Movdqa xmm0,[edx]
     Movdqu [eax],xmm0
     Add edx,16
     Add eax,16
     Dec ecx
     Jnz @Copy_loop
     emms
end;

procedure SSE2MemoryCopy(dst, src: pointer; Size: Cardinal);
var
  Unaligned: Cardinal;
  Chunks: Cardinal;
  Aligned128Size: Cardinal;
  Idx: Integer;
  AlignedSrc, NewDst: Pointer;
begin
{$R-} // Buggy ranche checker off
  // Unaligned bytes for reading
  Unaligned := (16 - (Cardinal(Src) and 15)) and 15;
  // Copy First Max 15 Bytes of Unaligned Data
  if Unaligned>0 then for Idx := 0 to UnAligned - 1 do   TByteArray(dst)[Idx] := TByteArray(src)[Idx];
  // Do the heavy 128 byte Chunks
  Chunks := (Size - Unaligned) shr 7;
  NewDst := Pointer(Cardinal(dst) + UnAligned);
  AlignedSrc := Pointer(Cardinal(src) + UnAligned);
    // Copy 128 bytes chunks Aligned, or even double Aligned (read and write aligned)
  if Chunks > 0 then
  begin
    if Cardinal(NewDst) and 15 = 0 then
      SSE2DoubleAlignedMemCopy128(NewDst, AlignedSrc, Chunks)
    else
      SSE2AlignedMemCopy128(NewDst, AlignedSrc, Chunks);
  end;
  // Copy remainder in 16 byte chunks
  Aligned128Size := Chunks shl 7;
  NewDst := Pointer(Cardinal(Newdst) + Aligned128Size);
  AlignedSrc := Pointer(Cardinal(AlignedSrc) + Aligned128Size);
  Chunks := (Size - Unaligned - Aligned128Size) shr 4;
  if Chunks > 0 then
    SSE2AlignedMemCopy16(NewDst, AlignedSrc, Chunks);
  // Copy Remaining in Byte chunks
  NewDst := Pointer(Cardinal(Newdst) + (Chunks shl 4));
  AlignedSrc := Pointer(Cardinal(AlignedSrc) + (Chunks shl 4));
  for Idx := 0 to Integer(Cardinal(Size - (Cardinal(AlignedSrc) - Cardinal(Src)))) - 1 do   TByteArray(NewDst)[Idx] := TByteArray(AlignedSrc)[Idx];
{$IFDEF RPLUS}{$R+}{$ENDIF}
end;

procedure SSE2AlignedMemFill16(dst: pointer; A16bytesmultiple: Integer);
asm
     PXOR XMM0,XMM0 // All zeros in XMM0

     @Copy_loop:
      Movntdq [eax],xmm0
      Add eax,16
      Dec edx
     Jnz @Copy_loop
     sfence
     emms
end;

procedure SSE2AlignedMemFill128(dst: pointer; A128bytesmultiple: Integer);
asm
     PXOR XMM0,XMM0 // All zeros in XMM0

     @Copy_loop:
     Movntdq [eax],xmm0
     Movntdq [eax+16],xmm0
     Movntdq [eax+32],xmm0
     Movntdq [eax+48],xmm0
     Movntdq [eax+64],xmm0
     Movntdq [eax+80],xmm0
     Movntdq [eax+96],xmm0
     Movntdq [eax+112],xmm0
     Add eax,128
     Dec edx
     Jnz @Copy_loop
     sfence
     emms
end;

procedure SSE2MemoryZero(src: pointer; Size: Cardinal);
var
  Unaligned: Cardinal;
  Chunks: Cardinal;
  Aligned128Size: Cardinal;
  Idx: Integer;
  AlignedSrc: Pointer;
begin
{$R-}
// Buggy ranche checker off
  // Unaligned bytes for reading
  Unaligned := (16 - (Cardinal(Src) and 15)) and 15;
  // Copy First Max 15 Bytes of Unaligned Data
  if UnAligned>0 then for Idx := 0 to UnAligned - 1 do  TByteArray(src)[Idx] := 0;
  // Do the heavy 128 byte Chunks
  Chunks := (Size - Unaligned) shr 7;
  AlignedSrc := Pointer(Cardinal(src) + UnAligned);
    // Fill 128 bytes chunks Aligned
  if Chunks > 0 then SSE2AlignedMemFill128(AlignedSrc, Chunks);
  // Fill remainder in 16 byte chunks
  Aligned128Size := Chunks shl 7;
  AlignedSrc := Pointer(Cardinal(AlignedSrc) + Aligned128Size);
  Chunks := (Size - Unaligned - Aligned128Size) shr 4;
  if Chunks > 0 then SSE2AlignedMemFill16(AlignedSrc, Chunks);
  AlignedSrc := Pointer(Cardinal(AlignedSrc) + (Chunks shl 4));
  for Idx := 0 to Integer(Cardinal(Size - (Cardinal(AlignedSrc) - Cardinal(Src)))) - 1 do TByteArray(AlignedSrc)[Idx] := 0;
{$IFDEF RPLUS}{$R+}{$ENDIF}
end;


function CPUSupportsSSE2: Boolean;
begin
  Result := False;
  try
    asm
      mov eax, 1
      db $0F,$A2  // CPUId
      Test  edx,(1 shl 26)
      jz    @Exit
      mov   Result,1
    @Exit:
    end;
  except
    // Ignore errors and assume NO SSE
  end;
end;

initialization
{$IFNDEF TOP_BLOCK_SSE2}
  IsSSE2Supported := CPUSupportsSSE2;
{$ENDIF}



end.

