unit nedmallocMM;

interface

implementation

type
  size_t = Cardinal;

const
  msvcrtDLL = 'nedmalloc_ptchg_v110.dll';

var
  malloc : function(Size: size_t): Pointer; cdecl;
  free   : procedure(P: Pointer); cdecl;
  realloc: function(P: Pointer; Size: size_t): Pointer; cdecl;
//function malloc(Size: size_t): Pointer; cdecl; external msvcrtDLL;
//function realloc(P: Pointer; Size: size_t): Pointer; cdecl; external msvcrtDLL;
//procedure free(P: Pointer); cdecl; external msvcrtDLL;

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
  kernel32  = 'kernel32.dll';

type
  LPCSTR = PAnsiChar;
  FARPROC = Pointer;

function LoadLibrary(lpLibFileName: PWideChar): HMODULE; stdcall; external kernel32 name 'LoadLibraryW';
function GetProcAddress(hModule: HMODULE; lpProcName: LPCSTR): FARPROC; external kernel32 name 'GetProcAddress';

var
  h: THandle;

initialization
//  h := LoadLibrary('msvcrt.DLL');
  h := LoadLibrary('c:\-Andre-\-Projects-\scalemm\Challenge\MSVCRT.DLL');
  h := LoadLibrary(msvcrtDLL);
  malloc  := GetProcAddress(h, 'malloc');
  free    := GetProcAddress(h, 'free');
  realloc := GetProcAddress(h, 'realloc');
  SetMemoryManager(MemoryManager);

end.