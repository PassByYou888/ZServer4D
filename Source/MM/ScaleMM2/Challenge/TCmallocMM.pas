unit TCmallocMM;

interface

implementation

type
  size_t = Cardinal;

const
  msvcrtDLL = 'libtcmalloc_minimal.dll';

function tc_malloc(Size: size_t): Pointer; cdecl; external msvcrtDLL;
function tc_realloc(P: Pointer; Size: size_t): Pointer; cdecl; external msvcrtDLL;
procedure tc_free(P: Pointer); cdecl; external msvcrtDLL;

function GetMem(Size: Integer): Pointer;
begin
  Result := tc_malloc(size);
end;

function FreeMem(P: Pointer): Integer;
begin
  tc_free(P);
  Result := 0;
end;

function ReallocMem(P: Pointer; Size: Integer): Pointer;
begin
  Result := tc_realloc(P, Size);
end;

function AllocMem(Size: Cardinal): Pointer; 
begin
  Result := GetMem(Size);
  if Assigned(Result) then begin
    FillChar(Result^, Size, 0);
  end;
end;

function RegisterUnregisterExpectedMemoryLeak(P: Pointer): Boolean;
begin
  Result := False;
end;

const
  MemoryManager: TMemoryManagerEx = (
    GetMem: GetMem;
    FreeMem: FreeMem;
    ReallocMem: ReallocMem;
    AllocMem: AllocMem;
    RegisterExpectedMemoryLeak: RegisterUnregisterExpectedMemoryLeak;
    UnregisterExpectedMemoryLeak: RegisterUnregisterExpectedMemoryLeak
  );

initialization
  SetMemoryManager(MemoryManager);

end.