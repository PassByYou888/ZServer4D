{****************************************************************************************

  TOPMEMORY v1.5 - HIGH PERFORMANCE DELPHI MEMORY MANAGER  (C) 2004 Ivo Tops, Topsoftware

  TopInstall Hooks and Unhooks the Memory Manager and receives the hooked calls

****************************************************************************************}
unit TopInstall;

interface

uses
  TopManagers,
  Windows;

var
  TopMM: TThreadManagerList;
  OldMM: TMemoryManager;

  {$IFDEF TOPDEBUG}
procedure DumpAllocStructureToDisk;
procedure DebugError(const S: string);

var
  Log: string;
  {$ENDIF}

implementation

uses
  TopLocalObjects
  {$IFDEF TOPDEBUG}
  ,
  SysUtils,
  TopPointerLookupList
  {$ENDIF};

threadvar
  ThreadManager: TThreadManager;
  ThreadManagerIndex: Integer;

procedure GetThreadManager;
begin
  ThreadManager := TopMM.ReserveThreadManager(ThreadManagerIndex);
end;

function TopGetMem(Size: Integer): Pointer;
begin
  {$IFDEF TOPBLOCK}
  TopMM.Lock;
  try
    {$ENDIF}
    if ThreadManager = nil then
      GetThreadManager;
    if ThreadManager.MarkedBlocksPresent then
      Threadmanager.FreeDataInMarkedBlocks;
    Result := TopMM.TMLGetMem(ThreadManager, Size)
      {$IFDEF TOPBLOCK}
  finally
    TopMM.Unlock;
  end;
  {$ENDIF}
end;

function TopFreeMem(P: Pointer): Integer;
begin
  {$IFDEF TOPBLOCK}
  TopMM.Lock;
  try
    {$ENDIF}
    if ThreadManager = nil then
      GetThreadManager;
    if ThreadManager.MarkedBlocksPresent then
      Threadmanager.FreeDataInMarkedBlocks;
    if not TopMM.TMLFreeMem(ThreadManager, P, Result) then
      Result := OldMM.FreeMem(P);
    {$IFDEF TOPBLOCK}
  finally
    TopMM.Unlock;
  end;
  {$ENDIF}
end;

function TopReallocMem(P: Pointer; Size: Integer): Pointer;
begin
  {$IFDEF TOPBLOCK}
  TopMM.Lock;
  try
    {$ENDIF}
    if ThreadManager = nil then
      GetThreadManager;
    if ThreadManager.MarkedBlocksPresent then
      Threadmanager.FreeDataInMarkedBlocks;
    if not TopMM.TMLReallocMem(ThreadManager, P, Size, Result) then
      Result := OldMM.ReAllocMem(P, Size);
    {$IFDEF TOPBLOCK}
  finally
    TopMM.Unlock;
  end;
  {$ENDIF}
end;

{ The method to hitchhike a thread was copied from the HPMM freeware memory manager by Robert Lee (www.optimalcode.com)}

procedure NewEndThread(ExitCode: Integer); register // ensure that calling convension matches EndThread
begin
  {$IFDEF TOPBLOCK}
  TopMM.Lock;
  try
    {$ENDIF}
    // Free up Manager assigned to thread
    if ThreadManager <> nil then
      TopMM.ReleaseThreadManager(ThreadManager, ThreadManagerIndex);
    {$IFDEF TOPBLOCK}
  finally
    TopMM.Unlock;
  end;
  {$ENDIF}
  // code of original EndThread;
  ExitThread(ExitCode);
end;

type
  PJump = ^TJump;
  TJump = packed record
    OpCode: Byte;
    Distance: Integer;
  end;

var
  OldCode: TJump;
  NewCode: TJump = (OpCode: $E9; Distance: 0);

procedure PatchEndThread; // redirect calls to System.EndThread to NewEndThread
var
  EndThreadAddr: PJump;
  OldProtect, Protect: DWord;
begin
  EndThreadAddr := Pointer(@EndThread);
  NewCode.Distance := Integer(@NewEndThread) - (Integer(@EndThread) + 5);
  VirtualProtect(EndThreadAddr, 5, PAGE_READWRITE, OldProtect);
  OldCode := EndThreadAddr^;
  EndThreadAddr^ := NewCode;
  VirtualProtect(EndThreadAddr, 5, OldProtect, Protect);
  FlushInstructionCache(GetCurrentProcess, EndThreadAddr, 5);
end;

procedure UnPatchEndThread;
var
  EndThreadAddr: PJump;
  OldProtect, Protect: DWord;
begin
  EndThreadAddr := Pointer(@EndThread);
  NewCode.Distance := Integer(@NewEndThread) - (Integer(@EndThread) + 5);
  VirtualProtect(EndThreadAddr, 5, PAGE_READWRITE, OldProtect);
  EndThreadAddr^ := OldCode;
  VirtualProtect(EndThreadAddr, 5, OldProtect, Protect);
  FlushInstructionCache(GetCurrentProcess, EndThreadAddr, 5);
end;

const
  TopManager: TMemoryManager = (
    GetMem: TopGetMem;
    FreeMem: TopFreeMem;
    ReallocMem: TopReallocMem);

procedure TopMMInstall;
begin
  // Hook memory Manager
  GetMemoryManager(OldMM);
  if @OldMM <> @TopManager then
  begin
    TopMM := TThreadManagerList.Create;
    PatchEndThread;
    SetMemoryManager(TopManager);
  end;
end;

procedure TopMMUnInstall;
begin
  // This procedure is not called because some threads might still be running and using
  // TopMemoryManager. Windows will dispose of everything when app closes
  //
  SetMemoryManager(OldMM);
  UnPatchEndThread;
  TopMM.Free;
end;

{$IFDEF TOPDEBUG}
{ Code to work around ntdll debug breakpoint (only occurs in delphi debugging mode). Found on the Internet, author unknown
  This prevents delphi from breaking on the debugbreakpoint in ntdll.dll}
procedure PatchINT3;
var
  NOP: Byte;
  BytesWritten: DWORD;
  NtDll: THandle;
  P: Pointer;
begin
  if Win32Platform <> VER_PLATFORM_WIN32_NT then
    Exit;

  NtDll := GetModuleHandle('NTDLL.DLL');
  if NtDll = 0 then
    Exit;

  P := GetProcAddress(NtDll, 'DbgBreakPoint');
  if P = nil then
    Exit;

  try
    if Char(P^) <> #$CC then
      Exit;

    NOP := $90;
    if WriteProcessMemory(GetCurrentProcess, P, @NOP, 1, BytesWritten) and (BytesWritten = 1) then
      FlushInstructionCache(GetCurrentProcess, P, 1);
  except
    //Do not panic if you see an EAccessViolation here,
    // it is perfectly harmless!
    on EAccessViolation do
      ;
  else
    raise;
  end;
end;
{$ENDIF}

{$IFDEF TOPDEBUG}
procedure DebugError(const S: string);
// Set BreakPoint here and compile with TOPDEBUG
// Assertions just give weird errors in a memmanager
var
  F: Textfile;
begin
  TopMM.Lock;
  AssignFile(F, 'TOPMMDEBUG.ERR');
  ReWrite(F);
  Writeln(F, S);
  CloseFile(F);
  Sleep(1000);
  Halt(0);
end;
{$ENDIF}

{$IFDEF TOPDEBUG}
procedure DumpAllocStructureToDisk;
var
  S: string;
  F: Textfile;
begin
  S := TopMM.GlobalPLL.GetInfo;
  AssignFile(F, 'TOPMMSTRUCT.PRN');
  ReWrite(F);
  Writeln(F, S);
  CloseFile(F);
end;
{$ENDIF}

initialization
  {$IFDEF TOPDEBUG}
  PatchINT3;
  {$ENDIF}
  TopMMInstall;


// This procedure is not called because some threads might still be running and using
// TopMemoryManager. Windows will dispose of everything when application closes
{finalization
  TopMMUninstall;}

end.

