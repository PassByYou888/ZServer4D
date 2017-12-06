unit IntelTBB;

{$O+}

interface


implementation

const
  {$ifdef CPUX86}
    tbbmalloc = 'tbbmalloc_x86.dll';
  {$else .CPUX64}
    tbbmalloc = 'tbbmalloc_x64.dll';  
  {$endif}

function scalable_getmem(Size: NativeUInt): Pointer; cdecl; external tbbmalloc name 'scalable_malloc';
procedure scalable_freemem(P: Pointer); cdecl; external tbbmalloc name 'scalable_free';
function scalable_realloc(P: Pointer; Size: NativeUInt): Pointer; cdecl; external tbbmalloc name 'scalable_realloc';


function CGetMem(Size: NativeInt): Pointer;
begin
  Result := scalable_getmem(Size);
end;

function CFreeMem(P: Pointer): Integer;
begin
  scalable_freemem(P);
  Result := 0;
end;

function CReAllocMem(P: Pointer; Size:NativeInt): Pointer;
begin
  Result := scalable_realloc(P, Size);
end;

function CAllocMem(Size: NativeInt): Pointer;
begin
  Result := scalable_getmem(Size);
  
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
    GetMem: CGetmem;
    FreeMem: CFreeMem;
    ReallocMem: CReAllocMem;
    AllocMem: CAllocMem;
    RegisterExpectedMemoryLeak: RegisterExpectedMemoryLeak;
    UnregisterExpectedMemoryLeak: RegisterExpectedMemoryLeak
  );

initialization
  SetMemoryManager(MemoryManager);

end.
