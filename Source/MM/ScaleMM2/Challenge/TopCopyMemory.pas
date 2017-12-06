{****************************************************************************************

  TOPUNITS v1.51 - HIGH PERFORMANCE DELPHI UNITS  (C) 2004 Ivo Tops, Topsoftware

  TopMoveMemory contains the fastest ways I have been able to come up with to move memory around

****************************************************************************************}
unit TopCopyMemory;

interface

uses windows;

// Regular Win32 API call. Allows memory to be overlapped
procedure WinMoveMemory(Destination: Pointer; Source: Pointer; Length: DWord); stdcall; external Kernel32 name 'RtlMoveMemory';
// Use this for large, non overlapping, block memory copies. If MMX not supported WinMoveMemory is used
procedure MMXCopyMemory(const Destination, Source: Pointer; const Size: Integer);
// Use FPU registers, allow overlap
procedure FPUMoveMemory(const Destination, Source: Pointer; const Size: Integer);

type
  TByteArray = array[0..MaxInt div (SizeOf(Byte)) - 1] of Byte;
  PByteArray = ^TByteArray;

implementation
{$IFNDEF VER130} // Works in Delphi 6 and higher, but not in 5
var
  MMXCapable: Boolean = False;

{$ENDIF}

procedure FPUCopyMemoryUpwardsASM(const Destination, Source: Pointer; const ChunksOf64: Integer);
asm
@Copy64Loop:
  prefetchnta [edx+1024]
  fild qword ptr[edx]
  fild qword ptr[edx + 8]
  fild qword ptr[edx + 16]
  fild qword ptr[edx + 24]
  fild qword ptr[edx + 32]
  fild qword ptr[edx + 40]
  fild qword ptr[edx + 48]
  fild qword ptr[edx + 56]
  fxch(st(7))
  fistp qword ptr[eax]
  fxch(st(5))
  fistp qword ptr[eax + 8]
  fxch(st(3))
  fistp qword ptr[eax + 16]
  fxch(st(1))
  fistp qword ptr[eax + 24]
  fistp qword ptr[eax + 32]
  fistp qword ptr[eax + 40]
  fistp qword ptr[eax + 48]
  fistp qword ptr[eax + 56]
  lea eax,[eax+ 64]
  lea edx,[edx+ 64]
  dec ecx
  jnz @Copy64Loop
end;

procedure FPUCopyMemoryDownwardsASM(const Destination, Source: Pointer; const ChunksOf64: Integer);
asm
@Copy64Loop:
  prefetchnta [edx-1024]
  fild qword ptr[edx-64]
  fild qword ptr[edx -56]
  fild qword ptr[edx -48]
  fild qword ptr[edx -40]
  fild qword ptr[edx - 32]
  fild qword ptr[edx - 24]
  fild qword ptr[edx - 16]
  fild qword ptr[edx - 8]
  fxch(st(7))
  fistp qword ptr[eax-64]
  fxch(st(5))
  fistp qword ptr[eax -56]
  fxch(st(3))
  fistp qword ptr[eax -48]
  fxch(st(1))
  fistp qword ptr[eax -40]
  fistp qword ptr[eax -32]
  fistp qword ptr[eax -24]
  fistp qword ptr[eax -16]
  fistp qword ptr[eax -8]
  lea eax,[eax- 64]
  lea edx,[edx- 64]
  dec ecx
  jnz @Copy64Loop
end;

procedure MMXCopyMemory(const Destination, Source: Pointer; const Size: Integer);
{$IFNDEF VER130}
var
  B,R: Integer;

  procedure MMXCopyMemoryUpwardsASM(const Destination, Source: Pointer; const ChunksOf64: Integer);
  asm
@Copy64Loop:
  prefetchnta [edx+1024]
  movq mm0,qword ptr[edx]
  movq mm1,qword ptr[edx+8]
  movq mm2,qword ptr[edx+16]
  movq mm3,qword ptr[edx+24]
  movq mm4,qword ptr[edx+32]
  movq mm5,qword ptr[edx+40]
  movq mm6,qword ptr[edx+48]
  movq mm7,qword ptr[edx+56]
  movntq qword ptr[eax],mm0
  movntq qword ptr[eax+8],mm1
  movntq qword ptr[eax+16],mm2
  movntq qword ptr[eax+24],mm3
  movntq qword ptr[eax+32],mm4
  movntq qword ptr[eax+40],mm5
  movntq qword ptr[eax+48],mm6
  movntq qword ptr[eax+56],mm7
  lea edx,[edx+64]
  lea eax,[eax+64]
  dec ecx
  jnz @Copy64Loop
  sfence
  emms
  end;
{$ENDIF}

begin
{$IFNDEF VER130} // Works in Delphi 6 and higher, but not in 5
  if MMXCapable and (Size > 512) then
  begin
    // Calcs
    B := Size shr 6; // Number of 64 byte blocks;
    R := B shl 6; // Number of bytes Copied with MMX
    // Do all 64 blocks with MMX
    MMXCopyMemoryUpwardsASM(Destination, Source, B);
    // Remaining bytes
    for B := 0 to Size-R-1  do
      TByteArray(Destination^)[R + B] := TByteArray(Source^)[R+B];
  end
  else
{$ENDIF}
    WinMoveMemory(Destination, Source, Size);
end;

procedure FPUCopyMemoryUpwards(const Destination, Source: Pointer; const Size: Integer);
var
  B: Integer;
  R: Cardinal;
begin
  B := Size div 64; // Number of 64 byte blocks;
  R := B * 64; // Number of bytes Copied with ASM
  // Major block in FPU prefetch ASM
  if B > 0 then
    FPUCopyMemoryUpwardsASM(Destination, Source, B);
  // Remaining bytes
  if Cardinal(Size) - R > 0 then
    WinMoveMemory(Pointer(Cardinal(Destination) + R), Pointer(Cardinal(Source) + R), Cardinal(Size) - R);
end;

procedure FPUCopyMemoryDownwards(const Destination, Source: Pointer; const Size: Integer);
var
  B: Integer;
  R: Cardinal;
begin
  B := Size div 64; // Number of 64 byte blocks;
  R := Size - B * 64; // Number of bytes Copied with ASM
  // Major block in FPU prefetch ASM
  if B > 0 then
    FPUCopyMemoryDownwardsASM(Pointer(Cardinal(Destination) + Cardinal(Size)), Pointer(Cardinal(Source) + Cardinal(Size)), B);
  // Remaining bytes
  if R > 0 then
    WinMoveMemory(Destination, Source, R);
end;

procedure FPUMoveMemory(const Destination, Source: Pointer; const Size: Integer);
begin
  if Cardinal(Destination) < Cardinal(Source) then
    FPUCopyMemoryUpwards(Destination, Source, Size)
  else
    if Cardinal(Destination) > Cardinal(Source) then
      FPUCopyMemoryDownwards(Destination, Source, Size);
end;

{$IFNDEF VER130} // Works in Delphi 6 and higher, but not in 5

function CanDoMMX: Boolean; // By Eric Nowinski [win32asm newsgroup may 2001]
begin
  Result := True;
  try
    asm
      db $0F,$77 // emms
    end;
  except
    Result := False;
  end;
end;

initialization
  begin
    // Check once for MMX support
    MMXCapable := CanDoMMX;
  end;
{$ENDIF}

end.

