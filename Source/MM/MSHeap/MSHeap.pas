unit MSHeap;

{$O+}

interface
  uses Windows;

implementation

var
  ProcessHeap: THandle;  
  
function SysGetMem(Size: NativeInt): Pointer;
begin
  Result := HeapAlloc(ProcessHeap, 0, Size);
end;

function SysFreeMem(P: Pointer): Integer;
begin
  HeapFree(ProcessHeap, 0, P);
  Result := 0;
end;

function SysReallocMem(P: Pointer; Size: NativeInt): Pointer;
begin
  Result := HeapReAlloc(ProcessHeap, 0, P, Size);
end;

function SysAllocMem(Size: NativeInt): Pointer;
begin
  Result := HeapAlloc(ProcessHeap, 0, Size);
  
  if (Result <> nil) then
    FillChar(Result^, Size, #0);  
end;

function SysRegisterExpectedMemoryLeak(P: Pointer): Boolean;
begin
  Result := False;
end;

function SysUnregisterExpectedMemoryLeak(P: Pointer): Boolean;
begin
  Result := False;
end;

const
  MemoryManager: TMemoryManagerEx =
  (
    GetMem: SysGetmem;
    FreeMem: SysFreeMem;
    ReallocMem: SysReAllocMem;
    AllocMem: SysAllocMem;
    RegisterExpectedMemoryLeak: SysRegisterExpectedMemoryLeak;
    UnregisterExpectedMemoryLeak: SysUnregisterExpectedMemoryLeak
  );

initialization
  ProcessHeap := GetProcessHeap;
  SetMemoryManager(MemoryManager);

end.
