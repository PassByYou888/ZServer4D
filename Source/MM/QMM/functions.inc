
const
  user32 = 'user32.dll';
  kernel32  = 'kernel32.dll';

type
  BOOL = LongBool;
  DWORD = Cardinal;
{$if CompilerVersion < 22}
  THandle = Cardinal;
  ULONG_PTR = Cardinal;
  SIZE_T = Cardinal;
  DWORD_PTR = Cardinal;
{$else}
  THandle = NativeUInt;
  ULONG_PTR = NativeUInt;
  SIZE_T = NativeUInt;
  DWORD_PTR = NativeUInt;
{$ifend}

  TSystemTime = record
    wYear: Word;
    wMonth: Word;
    wDayOfWeek: Word;
    wDay: Word;
    wHour: Word;
    wMinute: Word;
    wSecond: Word;
    wMilliseconds: Word;
  end;

  PRTLCriticalSection = ^TRTLCriticalSection;
  TRTLCriticalSection = record
    DebugInfo: Pointer;
    LockCount: Longint;
    RecursionCount: Longint;
    OwningThread: THandle;
    LockSemaphore: THandle;
    Reserved: ULONG_PTR;
  end;

  PMemoryBasicInformation = ^TMemoryBasicInformation;
  TMemoryBasicInformation = record
    BaseAddress : Pointer;
    AllocationBase : Pointer;
    AllocationProtect : DWORD;
    RegionSize : SIZE_T;
    State : DWORD;
    Protect : DWORD;
    Type_9 : DWORD;
  end;

  PSystemInfo = ^TSystemInfo;
  TSystemInfo = record
    case Integer of
      0: (
        dwOemId: DWORD);
      1: (
        wProcessorArchitecture: Word;
        wReserved: Word;
        dwPageSize: DWORD;
        lpMinimumApplicationAddress: Pointer;
        lpMaximumApplicationAddress: Pointer;
        dwActiveProcessorMask: DWORD_PTR;
        dwNumberOfProcessors: DWORD;
        dwProcessorType: DWORD;
        dwAllocationGranularity: DWORD;
        wProcessorLevel: Word;
        wProcessorRevision: Word);
  end;

  _OSVERSIONINFOA = record
    dwOSVersionInfoSize: DWORD;
    dwMajorVersion: DWORD;
    dwMinorVersion: DWORD;
    dwBuildNumber: DWORD;
    dwPlatformId: DWORD;
    szCSDVersion: array[0..127] of AnsiChar; { Maintenance AnsiString for PSS usage }
  end;
  TOSVersionInfo = _OSVERSIONINFOA;

function GetCurrentProcess: THandle; stdcall; external kernel32;
function GetModuleHandleA(lpModuleName: PAnsiChar): HMODULE; stdcall; external kernel32;
function GetProcAddress(hModule: HMODULE; lpProcName: PAnsiChar): Pointer; stdcall; external kernel32;

function GetVersionExA(var lpVersionInformation: TOSVersionInfo): BOOL; stdcall; external kernel32;
function InitializeCriticalSectionAndSpinCount(var lock: TRTLCriticalSection;
  spin: DWORD): BOOL; stdcall; external kernel32;
procedure EnterCriticalSection(var lock: TRTLCriticalSection); stdcall;
  external kernel32;
procedure LeaveCriticalSection(var lock: TRTLCriticalSection); stdcall;
  external kernel32;
procedure DeleteCriticalSection(var lock: TRTLCriticalSection); stdcall;
  external kernel32;

function TlsAlloc: DWORD; stdcall;  external kernel32;
function TlsGetValue(dwTlsIndex: DWORD): Pointer; stdcall; external kernel32;
function TlsSetValue(dwTlsIndex: DWORD; lpTlsValue: Pointer): BOOL; stdcall; external kernel32;
function TlsFree(dwTlsIndex: DWORD): BOOL; stdcall; external kernel32;

function VirtualAlloc(addr: Pointer; size: SIZE_T; allocation_type,
  protect: DWORD): Pointer; stdcall; external kernel32;
function VirtualFree(addr: Pointer; size: SIZE_T; free_type: DWORD): BOOL; stdcall;
external kernel32;
function VirtualProtect(addr: Pointer; size: SIZE_T; new_protect: DWORD;
  var old_protect: DWORD): BOOL; stdcall; external kernel32;
function VirtualQuery(addr: Pointer;
  var info: TMemoryBasicInformation; len: DWORD): DWORD; stdcall; external kernel32;
procedure Sleep(dwMilliseconds: DWORD); stdcall; external kernel32;

function CreateFileA(lpFileName: PAnsiChar; dwDesiredAccess, dwShareMode: DWORD;
  lpSecurityAttributes: Pointer; dwCreationDisposition, dwFlagsAndAttributes: DWORD;
  hTemplateFile: THandle): THandle; stdcall; external kernel32;
function WriteFile(hFile: THandle; const Buffer; nNumberOfBytesToWrite: DWORD;
  var lpNumberOfBytesWritten: DWORD; lpOverlapped: Pointer): BOOL; stdcall;
  external kernel32;
function SetFilePointer(hFile: THandle; lDistanceToMove: Longint;
  lpDistanceToMoveHigh: Pointer; dwMoveMethod: DWORD): DWORD; stdcall;
  external kernel32;
function CloseHandle(hObject: THandle): BOOL; stdcall; external kernel32;
function OpenFileMappingA(dwDesiredAccess: DWORD; bInheritHandle: BOOL;
  lpName: PAnsiChar): THandle; stdcall; external kernel32;
function CreateFileMappingA(hFile: THandle; lpFileMappingAttributes: Pointer;
  flProtect, dwMaximumSizeHigh, dwMaximumSizeLow: DWORD;
  lpName: PAnsiChar): THandle; stdcall; external kernel32;
function MapViewOfFile(hFileMappingObject: THandle; dwDesiredAccess: DWORD;
  dwFileOffsetHigh, dwFileOffsetLow: DWORD;
  dwNumberOfBytesToMap: SIZE_T): Pointer; stdcall; external kernel32;
function UnmapViewOfFile(lpBaseAddress: Pointer): BOOL; stdcall; external kernel32;
procedure GetSystemInfo(var lpSystemInfo: TSystemInfo); stdcall; external kernel32;
function wvsprintfA(Output, Format, arglist: PAnsiChar): Integer; stdcall; external user32;
procedure OutputDebugStringA(lpOutputString: PAnsiChar); stdcall; external kernel32;
function GetCurrentThreadId: DWORD; stdcall; external kernel32;
function GetModuleFileNameA(hModule: HINST; lpFilename: PAnsiChar;
  nSize: DWORD): DWORD; stdcall; external kernel32;

function lstrcatA(lpString1, lpString2: PAnsiChar): PAnsiChar; stdcall; external kernel32;
function lstrlenA(lpString: PAnsiChar): Integer; stdcall; external kernel32;
function GetCurrentProcessId: DWORD; stdcall; external kernel32;
function MessageBoxA(hWnd: THandle; lpText, lpCaption: PAnsiChar;
  uType: DWORD): Integer; stdcall; external user32;
function WideCharToMultiByte(CodePage, dwFlags: DWORD;
  lpWideCharStr: PWideChar; cchWideChar: Integer; lpMultiByteStr: PAnsiChar;
  cchMultiByte: Integer; lpDefaultChar: PAnsiChar;
  lpUsedDefaultChar: Pointer): Integer; stdcall; external kernel32;
procedure DebugBreak; external kernel32;
procedure GetLocalTime(var lpSystemTime: TSystemTime); stdcall;
external kernel32;
function SwitchToThread: BOOL; stdcall; external kernel32;

const
  PAGE_READONLY = 2;
  PAGE_READWRITE = 4;
  PAGE_EXECUTE = $10;
  PAGE_EXECUTE_READ = $20;
  PAGE_EXECUTE_READWRITE = $40;
  PAGE_EXECUTE_WRITECOPY = $80;
  PAGE_GUARD = $100;

  MEM_COMMIT = $1000;
  MEM_RESERVE = $2000;
  MEM_DECOMMIT = $4000;
  MEM_RELEASE = $8000;

const
  MAX_PATH = 260;
  INVALID_HANDLE_VALUE = THandle(-1);
  FILE_BEGIN = 0;
  FILE_CURRENT = 1;
  FILE_END = 2;

  GENERIC_WRITE = $40000000;
  OPEN_ALWAYS = 4;
  FILE_SHARE_READ = $00000001;
  FILE_MAP_WRITE = 2;
  FILE_MAP_READ = 4;

  FILE_ATTRIBUTE_NORMAL = $00000080;
  MB_ICONWARNING = $00000030;
  CP_ACP = 0;

