unit TCMalloc;

{$O+}

interface


implementation

const
  {$ifdef CPUX86}
    libtcmalloc = 'libtcmalloc_x86.dll';
  {$else .CPUX64}
    libtcmalloc = 'libtcmalloc_x64.dll';
  {$endif}

function tc_malloc(Size: NativeUInt): Pointer; cdecl; external libtcmalloc;
function tc_realloc(P: Pointer; Size: NativeUInt): Pointer; cdecl; external libtcmalloc;
procedure tc_free(P: Pointer); cdecl; external libtcmalloc;


function TCMGetMem(Size: NativeInt): Pointer;
begin
  Result := tc_malloc(Size);
end;

function TCMFreeMem(P: Pointer): Integer;
begin
  tc_free(P);
  Result := 0;
end;

function TCMReAllocMem(P: Pointer; Size:NativeInt): Pointer;
begin
  Result := tc_realloc(P, Size);
end;

function TCMAllocMem(Size: NativeInt): Pointer;
begin
  Result := tc_malloc(Size);

  if (Result <> nil) then
    FillChar(Result^, Size, #0);
end;

function RegisterExpectedMemoryLeak(P: Pointer): Boolean;
begin
  Result := False;
end;

function UnregisterExpectedMemoryLeak(P: Pointer): Boolean;
begin
  Result := False;
end;

const
  MemoryManager: TMemoryManagerEx =
  (
    GetMem: TCMGetmem;
    FreeMem: TCMFreeMem;
    ReallocMem: TCMReAllocMem;
    AllocMem: TCMAllocMem;
    RegisterExpectedMemoryLeak: RegisterExpectedMemoryLeak;
    UnregisterExpectedMemoryLeak: RegisterExpectedMemoryLeak
  );

initialization
  SetMemoryManager(MemoryManager);

end.
