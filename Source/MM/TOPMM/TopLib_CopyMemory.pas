{****************************************************************************************

  TOPUNITS v3.53 - HIGH PERFORMANCE DELPHI UNITS  (C) 2004 Ivo Tops, Topsoftware

  TopMoveMemory contains the fastest ways I have been able to come up with to move memory around

****************************************************************************************}
unit TopLib_CopyMemory;

interface

{$IFNDEF TOPDEBUG} // Debugging off unless you use the TOPDEBUG directive
{$D-,L-}
{$ENDIF}

uses
  windows;

// Use this for non overlapping, block memory copies. If SSE2 not supported WinMoveMemory is used
procedure TopMoveMemory(const Destination, Source: Pointer; const Size: Integer); {$IF COMPILERVERSION>=18}inline; {$IFEND}
// Faster for Zeroing blocks of memory. If not Zero then normall FillChar is used
procedure TopFillMemory(const Destination: Pointer; const Size: Integer; const FillWith: Byte = 0); {$IF COMPILERVERSION>=18}inline; {$IFEND}
// Windows method
procedure WinMoveMemory(Destination: Pointer; Source: Pointer; Length: DWord); stdcall; external Kernel32 name 'RtlMoveMemory';

implementation

uses
  TopLib_SSE2;

// Regular Win32 API call. Allows memory to be overlapped


procedure TopMoveMemory(const Destination, Source: Pointer; const Size: Integer);
begin
  if IsSSE2Supported and (Size >= 128) and ((Cardinal(Source) - Cardinal(Size) >= Cardinal(Destination)) or ((Cardinal(Source) + Cardinal(Size)) <= Cardinal(Destination))) then
    SSE2MemoryCopy(Destination, Source, Size)
  else
{$IF COMPILERVERSION>=18}
    Move(Source^, Destination^, Size);
{$ELSE}
  WinMoveMemory(Destination, Source, Size);
{$IFEND}
end;

procedure TopFillMemory(const Destination: Pointer; const Size: Integer; const FillWith: Byte);
begin
  if IsSSE2Supported and (FillWith = 0) and (Size >= 128) then SSE2MemoryZero(Destination, Size) else FillChar(Destination^, Size, FillWith);
end;



end.

