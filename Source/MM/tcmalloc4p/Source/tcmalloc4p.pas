{ ****************************************************************************** }
{ * jemalloc for pascal library  written by QQ 600585@qq.com                   * }
{ ****************************************************************************** }
{ * https://zpascal.net                                                        * }
{ * https://github.com/PassByYou888/zAI                                        * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/PascalString                               * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zChinese                                   * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/FFMPEG-Header                              * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/InfiniteIoT                                * }
{ * https://github.com/PassByYou888/FastMD5                                    * }
{ ****************************************************************************** }
unit tcmalloc4p;

{$IFDEF FPC}
{$MODE objfpc}
{$NOTES OFF}
{$STACKFRAMES OFF}
{$COPERATORS OFF}
{$GOTO OFF}
{$INLINE ON}
{$MACRO ON}
{$HINTS ON}
{$IEEEERRORS ON}
{$ENDIF FPC}

{$R-}
{$I-}
{$S-}
{$D-}
{$OPTIMIZATION ON}

interface

implementation

const
{$IF Defined(WIN32)}
  jemalloc4p_Lib = 'libtcmalloc_minimal_ia32.dll';
  C_FuncPre = 'tc_';
{$ELSEIF Defined(WIN64)}
  jemalloc4p_Lib = 'libtcmalloc_minimal_x64.dll';
  C_FuncPre = 'tc_';
{$ELSEIF Defined(OSX)}
  jemalloc4p_Lib = 'libtcmalloc_minimal.dylib';
  C_FuncPre = '_tc_';
{$ELSEIF Defined(IOS)}
  jemalloc4p_Lib = 'libtcmalloc_minimal.a';
  C_FuncPre = '_tc_';
{$ELSEIF Defined(ANDROID)}
  jemalloc4p_Lib = 'libtcmalloc_minimal.so';
  C_FuncPre = 'tc_';
{$ELSEIF Defined(Linux)}
  jemalloc4p_Lib = 'libtcmalloc_minimal.so';
  C_FuncPre = 'tc_';
{$ELSE}
{$MESSAGE FATAL 'unknow system.'}
{$IFEND}

function tc_malloc(Size: NativeUInt): Pointer; cdecl; external jemalloc4p_Lib Name C_FuncPre + 'malloc';
procedure tc_free(P: Pointer); cdecl; external jemalloc4p_Lib Name C_FuncPre + 'free';
function tc_realloc(P: Pointer; Size: NativeUInt): Pointer; cdecl; external jemalloc4p_Lib Name C_FuncPre + 'realloc';

procedure Fast_FillByte(const dest: Pointer; Count: NativeUInt; const Value: byte); inline;
var
  d: PByte;
  v: UInt64;
begin
  if Count <= 0 then
      Exit;
  v := Value or (Value shl 8) or (Value shl 16) or (Value shl 24);
  v := v or (v shl 32);
  d := dest;
  while Count >= 8 do
    begin
      PUInt64(d)^ := v;
      Dec(Count, 8);
      Inc(d, 8);
    end;
  if Count >= 4 then
    begin
      PCardinal(d)^ := PCardinal(@v)^;
      Dec(Count, 4);
      Inc(d, 4);
    end;
  if Count >= 2 then
    begin
      PWORD(d)^ := PWORD(@v)^;
      Dec(Count, 2);
      Inc(d, 2);
    end;
  if Count > 0 then
      d^ := Value;
end;

{$IFDEF FPC}

(*
  Pointer math is simply treating any given typed pointer in some narrow,
  instances as a scaled ordinal where you can perform simple arithmetic operations directly on the pointer variable.
*)
{$POINTERMATH ON}


var
  OriginMM: TMemoryManager;
  HookMM: TMemoryManager;

function do_GetMem(Size: PtrUInt): Pointer;
begin
  Result := tc_malloc(Size + SizeOf(PtrUInt));

  if (Result <> nil) then
    begin
      PPtrUInt(Result)^ := Size;
      Inc(Result, SizeOf(PtrUInt));
    end;
end;

function do_FreeMem(P: Pointer): PtrUInt;
begin
  if (P <> nil) then
      Dec(P, SizeOf(PtrUInt));

  tc_free(P);
  Result := 0;
end;

function do_FreememSize(P: Pointer; Size: PtrUInt): PtrUInt;
begin
  Result := 0;
  if Size = 0 then
      Exit;

  if P <> nil then
    begin
      Dec(P, SizeOf(PtrUInt));
      tc_free(P);
    end;
end;

function do_AllocMem(Size: PtrUInt): Pointer;
var
  TotalSize: PtrUInt;
begin
  TotalSize := Size + SizeOf(PtrUInt);

  Result := tc_malloc(TotalSize);

  if Result <> nil then
    begin
      Fast_FillByte(Result, TotalSize, 0);
      PPtrUInt(Result)^ := Size;
      Inc(Result, SizeOf(PtrUInt));
    end;
end;

function do_ReallocMem(var P: Pointer; Size: PtrUInt): Pointer;
begin
  if Size = 0 then
    begin
      if P <> nil then
        begin
          Dec(P, SizeOf(PtrUInt));

          tc_free(P);
          P := nil;
        end;
    end
  else
    begin
      Inc(Size, SizeOf(PtrUInt));
      if P = nil then
          P := tc_malloc(Size)
      else
        begin
          Dec(P, SizeOf(PtrUInt));
          P := tc_realloc(P, Size);
        end;

      if P <> nil then
        begin
          PPtrUInt(P)^ := Size - SizeOf(PtrUInt);
          Inc(P, SizeOf(PtrUInt));
        end;
    end;

  Result := P;
end;

function do_MemSize(P: Pointer): PtrUInt;
begin
  Result := PPtrUInt(P - SizeOf(PtrUInt))^;
end;

function do_GetHeapStatus: THeapStatus;
begin
  Fast_FillByte(@Result, SizeOf(Result), 0);
end;

function do_GetFPCHeapStatus: TFPCHeapStatus;
begin
  Fast_FillByte(@Result, SizeOf(Result), 0);
end;

procedure InstallMemoryHook;
const
  C_: TMemoryManager =
    (
    NeedLock: False;
    GetMem: @do_GetMem;
    FreeMem: @do_FreeMem;
    FreeMemSize: @do_FreememSize;
    AllocMem: @do_AllocMem;
    ReallocMem: @do_ReallocMem;
    MemSize: @do_MemSize;
    InitThread: nil;
    DoneThread: nil;
    RelocateHeap: nil;
    GetHeapStatus: @do_GetHeapStatus;
    GetFPCHeapStatus: @do_GetFPCHeapStatus;
  );
begin
  GetMemoryManager(OriginMM);
  HookMM := C_;
  SetMemoryManager(HookMM);
end;

procedure UnInstallMemoryHook;
begin
  SetMemoryManager(OriginMM);
end;

{$ELSE FPC}


var
  OriginMM: TMemoryManagerEx;
  HookMM: TMemoryManagerEx;

function do_GetMem(Size: NativeInt): Pointer;
begin
  Result := tc_malloc(Size);
end;

function do_FreeMem(P: Pointer): integer;
begin
  tc_free(P);
  Result := 0;
end;

function do_ReallocMem(P: Pointer; Size: NativeInt): Pointer;
begin
  Result := tc_realloc(P, Size);
end;

function do_AllocMem(Size: NativeInt): Pointer;
begin
  Result := tc_malloc(Size);
  Fast_FillByte(Result, Size, 0);
end;

function do_RegisterExpectedMemoryLeak(P: Pointer): Boolean;
begin
  Result := False;
end;

function do_UnregisterExpectedMemoryLeak(P: Pointer): Boolean;
begin
  Result := False;
end;

procedure InstallMemoryHook;
const
  C_: TMemoryManagerEx =
    (
    GetMem: do_GetMem;
    FreeMem: do_FreeMem;
    ReallocMem: do_ReallocMem;
    AllocMem: do_AllocMem;
    RegisterExpectedMemoryLeak: do_RegisterExpectedMemoryLeak;
    UnregisterExpectedMemoryLeak: do_UnregisterExpectedMemoryLeak;
  );
begin
  GetMemoryManager(OriginMM);
  HookMM := C_;
  SetMemoryManager(HookMM);
end;

procedure UnInstallMemoryHook;
begin
  SetMemoryManager(OriginMM);
end;

{$ENDIF FPC}

initialization

InstallMemoryHook;

finalization

UnInstallMemoryHook;

end.
