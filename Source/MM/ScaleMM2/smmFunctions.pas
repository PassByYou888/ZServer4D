unit smmFunctions;

interface

{$Include smmOptions.inc}

uses
  smmTypes;

type
  DWORD  = LongWord;
  BOOL   = LongBool;
  ULONG_PTR = NativeUInt;
  {$if CompilerVersion >= 23}  //Delphi XE2
  UINT_PTR  = System.UIntPtr;
  {$else}
  UINT_PTR  = NativeUInt;
  {$ifend}
  SIZE_T    = ULONG_PTR;
  UINT   = LongWord;
  HWND   = type UINT_PTR;

  PMemoryBasicInformation = ^TMemoryBasicInformation;
  _MEMORY_BASIC_INFORMATION = record
    BaseAddress : Pointer;
    AllocationBase : Pointer;
    AllocationProtect : DWORD;
    RegionSize : SIZE_T;
    State : DWORD;
    Protect : DWORD;
    Type_9 : DWORD;
  end;
  TMemoryBasicInformation = _MEMORY_BASIC_INFORMATION;

  PSecurityAttributes = Pointer;

  PSystemInfo = ^TSystemInfo;
  _SYSTEM_INFO = record
    case Integer of
      0: (
        dwOemId: DWORD);
      1: (
        wProcessorArchitecture: Word;
        wReserved: Word;
        dwPageSize: DWORD;
        lpMinimumApplicationAddress: Pointer;
        lpMaximumApplicationAddress: Pointer;
        dwActiveProcessorMask: DWORD;
        dwNumberOfProcessors: DWORD;
        dwProcessorType: DWORD;
        dwAllocationGranularity: DWORD;
        wProcessorLevel: Word;
        wProcessorRevision: Word);
  end;
  TSystemInfo = _SYSTEM_INFO;

const
  kernel32  = 'kernel32.dll';
  user32    = 'user32.dll';
  PAGE_EXECUTE_READWRITE = $40;
  PAGE_NOACCESS  = 1;
  PAGE_READONLY  = 2;
  PAGE_READWRITE = 4;
  MEM_COMMIT     = $1000;
  MEM_RESERVE    = $2000;
  MEM_DECOMMIT   = $4000;
  MEM_RELEASE    = $8000;
  MEM_FREE       = $10000;
  MEM_RESET      = $80000;
  MEM_TOP_DOWN   = $100000;
  FILE_MAP_WRITE = 2;
  FILE_MAP_READ  = 4;
  INVALID_HANDLE_VALUE = THandle(-1);

  {$IFnDEF PURE_PASCAL}
  function  TlsAlloc: DWORD; stdcall; external kernel32 name 'TlsAlloc';
//  function  TlsGetValue(dwTlsIndex: DWORD): Pointer; stdcall; external kernel32 name 'TlsGetValue';
  function  TlsSetValue(dwTlsIndex: DWORD; lpTlsValue: Pointer): BOOL; stdcall; external kernel32 name 'TlsSetValue';
//  function  TlsFree(dwTlsIndex: DWORD): BOOL; stdcall; external kernel32 name 'TlsFree';
  function  SetPermission(Code: Pointer; Size, Permission: Cardinal): Cardinal;
//  function  Scale_VirtualProtect(lpAddress: Pointer; dwSize, flNewProtect: DWORD;
//              var OldProtect: DWORD): BOOL; stdcall; overload; external kernel32 name 'VirtualProtect';
  {$ENDIF}

  procedure Sleep(dwMilliseconds: DWORD); stdcall; external kernel32 name 'Sleep';
  function  SwitchToThread: BOOL; stdcall; external kernel32 name 'SwitchToThread';
  function  FlushInstructionCache(hProcess: THandle; const lpBaseAddress: Pointer; dwSize: SIZE_T): BOOL; stdcall; external kernel32 name 'FlushInstructionCache';
  function  GetCurrentProcess: THandle; stdcall; external kernel32 name 'GetCurrentProcess';
  function  GetCurrentProcessId: DWORD; stdcall; external kernel32 name 'GetCurrentProcessId';
  function  GetCurrentThreadId: DWORD; stdcall; external kernel32 name 'GetCurrentThreadId';
//  function  GetCurrentThread: THandle; stdcall; external kernel32 name 'GetCurrentThread';
  procedure ExitThread(dwExitCode: DWORD); stdcall; external kernel32 name 'ExitThread';

  function  OpenFileMappingA(dwDesiredAccess: DWORD; bInheritHandle: BOOL; lpName: PAnsiChar): THandle; stdcall; external kernel32 name 'OpenFileMappingA';
  function  CreateFileMappingA(hFile: THandle; lpFileMappingAttributes: PSecurityAttributes; flProtect, dwMaximumSizeHigh, dwMaximumSizeLow: DWORD; lpName: PAnsiChar): THandle; stdcall; external kernel32 name 'CreateFileMappingA';
  function  MapViewOfFile(hFileMappingObject: THandle; dwDesiredAccess: DWORD; dwFileOffsetHigh, dwFileOffsetLow, dwNumberOfBytesToMap: SIZE_T): Pointer; stdcall; external kernel32 name 'MapViewOfFile';
  function  UnmapViewOfFile(lpBaseAddress: Pointer): BOOL; stdcall; external kernel32 name 'UnmapViewOfFile';
  function  CloseHandle(hObject: THandle): BOOL; stdcall; external kernel32 name 'CloseHandle';

  function  VirtualAlloc(lpvAddress: Pointer; dwSize: SIZE_T; flAllocationType, flProtect: DWORD): Pointer; stdcall; external kernel32 name 'VirtualAlloc';
  function  VirtualFree(lpAddress: Pointer; dwSize: SIZE_T; dwFreeType: DWORD): BOOL; stdcall; external kernel32 name 'VirtualFree';
  function  VirtualQuery(lpAddress: Pointer; var lpBuffer: TMemoryBasicInformation; dwLength: SIZE_T): SIZE_T; stdcall; external kernel32 name 'VirtualQuery';
  function  VirtualProtect(lpAddress: Pointer; dwSize: SIZE_T; flNewProtect: DWORD; var flOldProtect: DWORD): BOOL; stdcall; external kernel32 name 'VirtualProtect';

//  procedure OutputDebugString(lpOutputString: PWideChar); stdcall; external kernel32 name 'OutputDebugStringW';
//  function  IntToStr(Value: Integer): string;overload;
//  function  IntToStr(Value: Pointer): string;overload;
  procedure DebugBreak; external kernel32 name 'DebugBreak';

  function  CAS32(aOldValue, aNewValue: Byte; aDestination: Pointer): boolean;overload;
  function  CAS32(aOldValue, aNewValue: NativeUInt; aDestination: Pointer): boolean;overload;
  function  CAS32(aOldValue, aNewValue: NativeInt; aDestination: Pointer): boolean;overload;
  function  CAS32(aOldValue, aNewValue: Pointer; aDestination: Pointer): boolean;overload;

  function CAS64(const oldData, newData: int64; var destination): boolean;
  //either 8-byte or 16-byte CAS, depending on the platform; destination must be propely aligned (8- or 16-byte)
  function CAS(const oldData: pointer; oldReference: NativeInt; newData: pointer;
               newReference: NativeInt; var destination): boolean; overload;

  function  BitScanLast (aValue: NativeInt): NativeUInt;
  function  BitScanFirst(aValue: NativeInt): NativeUInt;

  {$ifopt C+} //assertions?
  procedure Assert(aCondition: boolean);
  {$ENDIF}
  function MessageBoxW(hWnd: HWND; lpText, lpCaption: PWideChar; uType: UINT): Integer; stdcall; external user32 name 'MessageBoxW';

  {$ifndef PURE_PASCAL}
  {$if CompilerVersion < 19}
  procedure Move(const Source; var Dest; Count: Integer);
  {$ifend}
  {$endif PURE_PASCAL}

  procedure GetSystemInfo(var lpSystemInfo: TSystemInfo); stdcall; external kernel32 name 'GetSystemInfo';

  function  WriteFile(hFile: THandle; const Buffer; nNumberOfBytesToWrite: DWORD;
                      var lpNumberOfBytesWritten: DWORD; lpOverlapped: pointer): BOOL; stdcall; external kernel32 name 'WriteFile';
  procedure WriteToFile(hFile: THandle; const aString: AnsiString);

  //from FastMM4.pas
  procedure WriteNativeUIntToStrBuf(hFile: THandle; ANum: NativeUInt);
  procedure WriteNativeUIntToHexBuf(hFile: THandle; ANum: NativeUInt; aDigits: Integer = 8);


implementation

//uses
//  SysUtils, Windows;    do not use these files!

{$IFnDEF PURE_PASCAL}
function SetPermission(Code: Pointer; Size, Permission: Cardinal): Cardinal;
begin
  Assert(Assigned(Code) and (Size > 0));
  { Flush the instruction cache so changes to the code page are effective immediately }
  if Permission <> 0 then
    if FlushInstructionCache(GetCurrentProcess, Code, Size) then
      VirtualProtect(Code, Size, Permission, Longword(Result));
end;
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
//Assembly functions

function  CAS32(aOldValue, aNewValue: Pointer; aDestination: Pointer): boolean;overload;
asm
  //                          Win32 Win64
  // aOldValue     : byte    EAX   RCX
  // aNewValue     : byte    EDX   RDX
  // aDestination  : pointer ECX   R8
{$IFDEF CPU386}
  lock cmpxchg [aDestination], aNewValue
  setz al
{$ELSE} .NOFRAME
  mov  rax, aOldValue
  lock cmpxchg [aDestination], aNewValue
  setz  al
{$endif}
end;

function  CAS32(aOldValue, aNewValue: NativeUInt; aDestination: Pointer): boolean;overload;
asm
  //                          Win32 Win64
  // aOldValue     : byte    EAX   RCX
  // aNewValue     : byte    EDX   RDX
  // aDestination  : pointer ECX   R8
{$IFDEF CPU386}
  lock cmpxchg [aDestination], aNewValue
  setz al
{$ELSE} .NOFRAME
  mov  rax, aOldValue
  lock cmpxchg [aDestination], aNewValue
  setz  al
{$endif}
end;

function  CAS32(aOldValue, aNewValue: NativeInt; aDestination: Pointer): boolean;overload;
asm
  //                          Win32 Win64
  // aOldValue     : byte    EAX   RCX
  // aNewValue     : byte    EDX   RDX
  // aDestination  : pointer ECX   R8
{$IFDEF CPU386}
  lock cmpxchg [aDestination], aNewValue
  setz al
{$ELSE} .NOFRAME
  mov  rax, aOldValue
  lock cmpxchg [aDestination], aNewValue
  setz  al
{$endif}
end;

function CAS32(aOldValue: Byte; aNewValue: Byte; aDestination: Pointer): boolean;overload;
asm
  //                          Win32 Win64
  // aOldValue     : byte    AL    CL
  // aNewValue     : byte    DL    DL
  // aDestination  : pointer ECX   R8
{$IFDEF CPU386}
  lock cmpxchg [aDestination],dl
  setz al
{$ELSE} .NOFRAME
  mov  al, cl
  lock cmpxchg [aDestination], aNewValue
  setz  al
{$endif}
end;

//http://code.google.com/p/omnithreadlibrary/source/browse/branches/x64/OtlSync.pas?r=1071
function CAS64(const oldData, newData: int64; var destination): boolean;
asm
{$IFNDEF CPUX64}
  push  edi
  push  ebx
  mov   edi, destination
  mov   ebx, low newData
  mov   ecx, high newData
  mov   eax, low oldData
  mov   edx, high oldData
  lock cmpxchg8b [edi]
  pop   ebx
  pop   edi
{$ELSE CPUX64}
  mov   rax, oldData
  lock cmpxchg [destination], newData
{$ENDIF ~CPUX64}
  setz  al
end; { CAS64 }

//either 8-byte or 16-byte CAS, depending on the platform; destination must be propely aligned (8- or 16-byte)
function CAS(const oldData: pointer; oldReference: NativeInt; newData: pointer;
  newReference: NativeInt; var destination): boolean; overload;
asm
{$IFNDEF CPUX64}
  push  edi
  push  ebx
  mov   ebx, newData
  mov   ecx, newReference
  mov   edi, destination
  lock cmpxchg8b qword ptr [edi]
  pop   ebx
  pop   edi
{$ELSE CPUX64}
  .noframe
  push  rbx
  mov   rax, oldData
  mov   rbx, newData
  mov   rcx, newReference
  mov   r8, [destination]
  mov   r8, [rsp + $30]
  lock cmpxchg16b [r8]
  pop   rbx
{$ENDIF CPUX64}
  setz  al
end; { CAS }

(*
function CAS(const oldData: pointer; oldReference: NativeInt; newData: pointer;
  newReference: NativeInt; var destination): boolean; overload;
var p: Pointer;
begin
  p := Pointer(destination);
  {$IFNDEF CPUX64}
  Assert( NativeUInt(p) AND 7 = 0);  //destination must be propely aligned (8- or 16-byte)
  {$ELSE CPUX64}
  Assert( NativeUInt(p) AND 15 = 0);  //destination must be propely aligned (8- or 16-byte)
  {$ENDIF CPUX64}
  Result := CAS_(oldData, oldReference, newData, newReference, destination);
end;
*)

(*
procedure InterlockedIncrement(var Value: Byte);
asm
  lock inc byte [Value]
end;

procedure InterlockedDecrement(var Value: Byte);
asm
  lock dec byte [Value]
end;

procedure InterlockedIncrement(var Value: Integer);
asm
  lock inc [Value]
end;

function InterlockedAdd(var Addend: Integer): Integer;
{ @Addend: EAX }
{ Result:  EAX }
asm
      MOV  EDX, EAX
      MOV  EAX, 1
 LOCK XADD [EDX], EAX
      INC  EAX
end;

procedure InterlockedDecrement(var Value: Integer);
asm
  lock dec [Value]
end;
*)

//find first bit (0..31)
//http://www.arl.wustl.edu/~lockwood/class/cs306/books/artofasm/Chapter_6/CH06-4.html
function BitScanFirst(aValue: NativeInt): NativeUInt;
asm
{$IFDEF CPU386}
  BSF	EAX, aValue;
{$ELSE} .NOFRAME
  BSF	RAX, aValue;
{$ENDIF}
end;

function BitScanLast(aValue: NativeInt): NativeUInt;
asm
{$IFDEF CPU386}
  BSR	AX, aValue;
{$ELSE} .NOFRAME
  BSR	RAX, aValue;
{$ENDIF}
end;

////////////////////////////////////////////////////////////////////////////////

{$ifopt C-} //no assertions
procedure DummyAssert(aBoolean: boolean);
begin
  //
end;
{$ELSE}  //assertions
procedure Assert(aCondition: boolean);
begin
  if not aCondition then
  begin
    Sleep(0);  // no exception, just dummy for breakpoint
    {$IFDEF CPU386}
    asm
      int 3;   // breakpoint
    end;
    {$ELSE}
    DebugBreak;
    {$ENDIF}
    Sleep(0);  // no exception, just dummy for breakpoint
    {$WARN SYMBOL_PLATFORM OFF}
    if DebugHook = 0 then
      //Error(reInvalidPtr);
      MessageBoxW(0, 'Assertion','Assertion',0);
  end;
end;
{$ENDIF}


{$ifndef PURE_PASCAL}
{$if CompilerVersion < 19}
procedure Move(const Source; var Dest; Count: Integer);
asm // eax=source edx=dest ecx=count
  // original code by John O'Harrow - included since Delphi 2007
  cmp     ecx, 32
  ja      @@LargeMove {Count > 32 or Count < 0}
  sub     ecx, 8
  jg      @@SmallMove
  jmp     dword ptr [@@JumpTable+32+ecx*4] {0..8 Byte Move}
@@SmallMove: {9..32 Byte Move}
  fild    qword ptr [eax+ecx] {Load Last 8}
  fild    qword ptr [eax] {Load First 8}
  cmp     ecx, 8
  jle     @@Small16
  fild    qword ptr [eax+8] {Load Second 8}
  cmp     ecx, 16
  jle     @@Small24
  fild    qword ptr [eax+16] {Load Third 8}
  fistp   qword ptr [edx+16] {Save Third 8}
@@Small24:
  fistp   qword ptr [edx+8] {Save Second 8}
@@Small16:
  fistp   qword ptr [edx] {Save First 8}
  fistp   qword ptr [edx+ecx] {Save Last 8}
@@Exit:
  ret
  lea eax,eax+0 // for alignment of @@JumpTable
@@JumpTable: {4-Byte Aligned}
  dd      @@Exit, @@M01, @@M02, @@M03, @@M04, @@M05, @@M06, @@M07, @@M08
@@LargeForwardMove: {4-Byte Aligned}
  push    edx
  fild    qword ptr [eax] {First 8}
  lea     eax, [eax+ecx-8]
  lea     ecx, [ecx+edx-8]
  fild    qword ptr [eax] {Last 8}
  push    ecx
  neg     ecx
  and     edx, -8 {8-Byte Align Writes}
  lea     ecx, [ecx+edx+8]
  pop     edx
@FwdLoop:
  fild    qword ptr [eax+ecx]
  fistp   qword ptr [edx+ecx]
  add     ecx, 8
  jl      @FwdLoop
  fistp   qword ptr [edx] {Last 8}
  pop     edx
  fistp   qword ptr [edx] {First 8}
  ret
@@LargeMove:
  jng     @@LargeDone {Count < 0}
  cmp     eax, edx
  ja      @@LargeForwardMove
  sub     edx, ecx
  cmp     eax, edx
  lea     edx, [edx+ecx]
  jna     @@LargeForwardMove
  sub     ecx, 8 {Backward Move}
  push    ecx
  fild    qword ptr [eax+ecx] {Last 8}
  fild    qword ptr [eax] {First 8}
  add     ecx, edx
  and     ecx, -8 {8-Byte Align Writes}
  sub     ecx, edx
@BwdLoop:
  fild    qword ptr [eax+ecx]
  fistp   qword ptr [edx+ecx]
  sub     ecx, 8
  jg      @BwdLoop
  pop     ecx
  fistp   qword ptr [edx] {First 8}
  fistp   qword ptr [edx+ecx] {Last 8}
@@LargeDone:
  ret
@@M01:
  movzx   ecx, [eax]
  mov     [edx], cl
  ret
@@M02:
  movzx   ecx, word ptr [eax]
  mov     [edx], cx
  ret
@@M03:
  mov     cx, [eax]
  mov     al, [eax+2]
  mov     [edx], cx
  mov     [edx+2], al
  ret
@@M04:
  mov     ecx, [eax]
  mov     [edx], ecx
  ret
@@M05:
  mov     ecx, [eax]
  mov     al, [eax+4]
  mov     [edx], ecx
  mov     [edx+4], al
  ret
@@M06:
  mov     ecx, [eax]
  mov     ax, [eax+4]
  mov     [edx], ecx
  mov     [edx+4], ax
  ret
@@M07:
  mov     ecx, [eax]
  mov     eax, [eax+3]
  mov     [edx], ecx
  mov     [edx+3], eax
  ret
@@M08:
  fild    qword ptr [eax]
  fistp   qword ptr [edx]
end;
{$ifend}
{$endif PURE_PASCAL}

procedure WriteToFile(hFile: THandle; const aString: AnsiString);
var icount: DWORD;
begin
  icount := Length(aString);
  WriteFile(hFile, aString[1], icount, icount, nil);
end;

{Converts an unsigned integer to string at the buffer location, returning the
 new buffer position. Note: The 32-bit asm version only supports numbers up to
 2^31 - 1.}
procedure WriteNativeUIntToStrBuf(hFile: THandle; ANum: NativeUInt);
const
  MaxDigits = 20 + (20 div 3);
var
  LDigitBuffer: array[0..MaxDigits - 1] of AnsiChar;
  LCount: Cardinal;
  LDigit: NativeUInt;
begin
  {Generate the digits in the local buffer}
  LCount := 0;
  repeat
    if (LCount = 3) or (LCount = 7) or (LCount = 11)  then
    begin
      Inc(LCount);
      LDigitBuffer[MaxDigits - LCount] := '.';
    end;
    LDigit := ANum;
    ANum   := ANum div 10;
    LDigit := LDigit - ANum * 10;
    Inc(LCount);
    LDigitBuffer[MaxDigits - LCount] := AnsiChar(Ord('0') + LDigit);
  until ANum = 0;

  WriteFile(hFile, LDigitBuffer[MaxDigits - LCount], LCount, LCount, nil);
  //{Copy the digits to the output buffer and advance it}
  //System.Move(LDigitBuffer[MaxDigits - LCount], APBuffer^, LCount);
  //Result := APBuffer + LCount;
end;

{Converts an unsigned integer to a hexadecimal string at the buffer location,
 returning the new buffer position.}
procedure WriteNativeUIntToHexBuf(hFile: THandle; ANum: NativeUInt; aDigits: Integer = 8);
const
  MaxDigits = 16;
  {Hexadecimal characters}
  HexTable: array[0..15] of AnsiChar = ('0', '1', '2', '3', '4', '5', '6', '7',
    '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
var
  LDigitBuffer: array[0..MaxDigits - 1] of AnsiChar;
  LCount: Integer;
  LWritten: Cardinal;
  LDigit: NativeUInt;
begin
  {Generate the digits in the local buffer}
  LCount := 0;
  repeat
    LDigit := ANum;
    ANum := ANum div 16;
    LDigit := LDigit - ANum * 16;
    Inc(LCount);
    LDigitBuffer[MaxDigits - LCount] := HexTable[LDigit];
  until ANum = 0;

  while LCount < aDigits do
  begin
    Inc(LCount);
    LDigitBuffer[MaxDigits - LCount] := ' ';
  end;

  WriteFile(hFile, LDigitBuffer[MaxDigits - LCount], LCount, LWritten, nil);
//  {Copy the digits to the output buffer and advance it}
//  System.Move(LDigitBuffer[MaxDigits - LCount], APBuffer^, LCount);
//  Result := APBuffer + LCount;
end;

end.
