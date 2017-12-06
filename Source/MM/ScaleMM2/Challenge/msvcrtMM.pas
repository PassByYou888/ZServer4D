unit msvcrtMM;

interface

implementation

type
  size_t = Cardinal;

const
  msvcrtDLL = 'msvcrt.dll';

function malloc(Size: size_t): Pointer; cdecl; external msvcrtDLL;
function realloc(P: Pointer; Size: size_t): Pointer; cdecl; external msvcrtDLL;
procedure free(P: Pointer); cdecl; external msvcrtDLL;

function GetMem(Size: Integer): Pointer;
begin
  Result := malloc(size);
end;

function FreeMem(P: Pointer): Integer;
begin
  free(P);
  Result := 0;
end;

function ReallocMem(P: Pointer; Size: Integer): Pointer;
begin
  Result := realloc(P, Size);
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