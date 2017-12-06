{****************************************************************************************

  TOPMEMORY v3.55 - HIGH PERFORMANCE DELPHI MEMORY MANAGER  (C) 2008 Ivo Tops, Topsoftware

  TopInstall Hooks and Unhooks the Memory Manager and receives the hooked calls

****************************************************************************************}
unit TopInstall;

interface

{$IFNDEF TOPDEBUG} // Debugging off unless you use the TOPDEBUG directive
//{$D-,L-}
{$ENDIF}
{$X+}

uses
  TopManagers,
  TopLib_CopyMemory,
  TopBlocks,
  Windows;


type
{$if CompilerVersion >= 22.0}
  MSIZE = NativeInt;
  MSIZE2 = NativeInt;
{$else}
  MSIZE = Integer;
  MSIZE2 = Cardinal;
{$ifend}

function TopGetMem(Size: MSIZE): Pointer;
function TopAllocMem(Size: MSIZE2): Pointer;
function TopReallocMem(P: Pointer; Size: MSIZE): Pointer;
function TopFreeMem(P: Pointer): Integer;

var
  TopMM: TThreadManagerList = nil;
{$IF COMPILERVERSION>=18}
  OldMM: TMemoryManagerEx;
{$ELSE}
  OldMM: TMemoryManager;
{$IFEND}

{$IFDEF TOPDEBUG}
procedure DebugError(const S: string);

var
  Log: string;
{$ENDIF}


implementation

uses
{$IF COMPILERVERSION<18}Classes, {$IFEND} // Needs to be here so with D7 it is loaded first
  TopLocalObjects,
  TopLib_SSE2
{$IFDEF TOPDEBUG}
  , SysUtils
{$ENDIF};


var
  cTLSThreadManagers: Cardinal;
{$IF COMPILERVERSION<18}cIgnoreFree: Boolean = False; {$IFEND}

procedure TLSSetThreadManager(const AThreadManager: TThreadManager); {$IF COMPILERVERSION>=18}inline; {$IFEND}
begin
  TlsSetValue(cTLSThreadManagers, AThreadManager);
end;

function CreateThreadManager: TThreadManager;
begin
  // Get A Manager
  if MainThreadID = GetCurrentThreadId then
    Result := TopMM.ReserveThreadManager(True)
  else
    Result := TopMM.ReserveThreadManager(False);
   // Keep it
  TLSSetThreadManager(Result);
end;

function TLSGetThreadManager: TThreadManager; {$IF COMPILERVERSION>=18}inline; {$IFEND}
begin
  Result := TlsGetValue(cTLSThreadManagers);
  if Result = nil then Result := CreateThreadManager;
end;

function CorrectPointer(const APointer: Pointer): Pointer; {$IF COMPILERVERSION>=18}inline; {$IFEND}
begin
  if APointer <> nil then
    Result := Pointer(Cardinal(APointer) + cAppBlockHeaderSize)
  else
    Result := nil;
end;

function TopGetMem(Size: MSIZE): Pointer;
begin
{$IFDEF TOPBLOCK}
  TopMM.Lock;
  try
{$ENDIF}
    Result := CorrectPointer(TLSGetThreadManager.TMGetMem(Cardinal(Size), False));
{$IFDEF TOPBLOCK}
  finally
    TopMM.Unlock;
  end;
{$ENDIF}
end;

function TopAllocMem(Size: MSIZE2): Pointer;
begin
{$IFDEF TOPBLOCK}
  TopMM.Lock;
  try
{$ENDIF}
    Result := CorrectPointer(TLSGetThreadManager.TMGetMem(Cardinal(Size), True));
{$IFDEF TOPBLOCK}
  finally
    TopMM.Unlock;
  end;
{$ENDIF}
end;


function TopFreeMem(P: Pointer): Integer;
var
  OSBlock: TOSBlock;
  lFSMIndex: Byte;
begin
{$IF COMPILERVERSION<18}if not cIgnoreFree then begin{$IFEND}
{$IFDEF TOPBLOCK}
    TopMM.Lock;
    try
{$ENDIF}
      try
        lFSMIndex := TByteArray(Pointer(Cardinal(P) - cAppBlockHeaderSize)^)[0];
        if Assigned(P) and (lFSMIndex <= cMaxManagers) then
        begin
          OSBlock := TOSBlock(Pointer(Cardinal(P) - cAppBlockHeaderSize - cBlockHeaderSize - cSMIndexSizes[lFSMIndex] * TByteArray(Pointer(Cardinal(P) - cAppBlockHeaderSize)^)[1])^);
          //
          if (TSizeManager(OSBlock.SizeManager).ThreadManager = TLSGetThreadManager) then
          begin
            TSizeManager(OSBlock.Sizemanager).SMFreeMem(Pointer(Cardinal(P) - cAppBlockHeaderSize), OSBlock);
            Result := 0; // No Error result for Delphi
          end else
          begin
            TopMM.FreeAppBlockFromOtherThread(OSBlock, Pointer(Cardinal(P) - cAppBlockHeaderSize));
            Result := 0;
          end;
        end
        else Result := OldMM.FreeMem(P);
      except
        Result := OldMM.FreeMem(P);
      end;
{$IFDEF TOPBLOCK}
    finally
      TopMM.Unlock;
    end;
{$ENDIF}
{$IF COMPILERVERSION<18} end else result := 0; {$IFEND}
end;

function TopReallocMem(P: Pointer; Size: MSIZE): Pointer;
var
  OSBlock: TOSBlock;
  lFSMIndex: Byte;
begin
{$IFDEF TOPBLOCK}
  TopMM.Lock;
  try
{$ENDIF}
    // ReAlloc can be misued as GetMem or FreeMem (documented in delphi help) so check what the user wants
    if (P <> nil) and (Size > 0) then
    begin
      try
        // Normal realloc of exisiting data
        lFSMIndex := TByteArray(Pointer(Cardinal(P) - cAppBlockHeaderSize)^)[0];
        if lFSMIndex <= cMaxManagers then
        begin
          OSBlock := TOSBlock(Pointer(Cardinal(P) - cAppBlockHeaderSize - cBlockHeaderSize - cSMIndexSizes[lFSMIndex] * TByteArray(Pointer(Cardinal(P) - cAppBlockHeaderSize)^)[1])^);
          if (Cardinal(Size) > OSBlock.AppBlockSize - cAppBlockHeaderSize) or (Cardinal(Size) < cSMSizeStop[lFSMIndex - 1]) or (OSBlock.UniqueMode) then
          begin
            TopMM.TMLReallocMem(TLSGetThreadManager, OSBlock, Pointer(Cardinal(P) - cAppBlockHeaderSize), Cardinal(Size), Result);
            Result := CorrectPointer(Result);
          end else Result := P;
        end else Result := OldMM.ReAllocMem(P, Size)
      except
        Result := OldMM.ReAllocMem(P, Size)
      end;
    end
    else
    begin
      if (P = nil) and (Size > 0) then
      begin // GetMem disguised as ReAlloc
        Result := TopGetMem(Size);
      end
      else
      begin // FreeMem disguised as ReAlloc
        Result := nil;
        TopFreeMem(P);
      end;
    end;
{$IFDEF TOPBLOCK}
  finally
    TopMM.Unlock;
  end;
{$ENDIF}
end;


{$IF COMPILERVERSION <18}
const
  TopManager: TMemoryManager = (
    GetMem: TopGetMem;
    FreeMem: TopFreeMem;
    ReallocMem: TopReallocMem);

{ The method to hitchhike a thread was copied from the HPMM freeware memory manager by Robert Lee (www.optimalcode.com)}

procedure NewEndThread(ExitCode: Integer); register; // ensure that calling convension matches EndThread
begin
{$IFDEF TOPBLOCK}
  TopMM.Lock;
  try
{$ENDIF}
    // Free up Manager assigned to thread
    TopMM.ReleaseThreadManager(TLSGetThreadManager);
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


procedure PatchThread; // redirect calls to System.EndThread to NewEndThread
var
  EndThreadAddr: PJump;
  OldProtect, Protect: DWord;
begin
  EndThreadAddr := Pointer(@EndThread);
  NewCode.Distance := Cardinal(@NewEndThread) - (Cardinal(@EndThread) + 5);
  VirtualProtect(EndThreadAddr, 5, PAGE_READWRITE, OldProtect);
  OldCode := EndThreadAddr^;
  EndThreadAddr^ := NewCode;
  VirtualProtect(EndThreadAddr, 5, OldProtect, Protect);
  FlushInstructionCache(GetCurrentProcess, EndThreadAddr, 5);
end;

procedure UnPatchThread;
var
  EndThreadAddr: PJump;
  OldProtect, Protect: DWord;
begin
  EndThreadAddr := Pointer(@EndThread);
  NewCode.Distance := Cardinal(@NewEndThread) - (Cardinal(@EndThread) + 5);
  VirtualProtect(EndThreadAddr, 5, PAGE_READWRITE, OldProtect);
  EndThreadAddr^ := OldCode;
  VirtualProtect(EndThreadAddr, 5, OldProtect, Protect);
  FlushInstructionCache(GetCurrentProcess, EndThreadAddr, 5);
end;

procedure ReleaseThreadManager(ExitCode: Integer);
begin
{$IFDEF TOPBLOCK}
  TopMM.Lock;
  try
{$ENDIF}
    // Free up Manager assigned to thread
    TopMM.ReleaseThreadManager(TLSGetThreadManager);
{$IFDEF TOPBLOCK}
  finally
    TopMM.Unlock;
  end;
{$ENDIF}
end;

{$ELSE}

function TopRegisterMemoryLeak(P: Pointer): Boolean;
begin
  Result := TopMM.RegisterMemoryLeak(P);
end;

function TopUnregisterMemoryLeak(P: Pointer): Boolean;
begin
  Result := TopMM.UnregisterMemoryLeak(P);
end;

const
  TopManager: TMemoryManagerEx = (
    GetMem: TopGetMem;
    FreeMem: TopFreeMem;
    ReallocMem: TopReallocMem;
    AllocMem: TopAllocMem;
    RegisterExpectedMemoryLeak: TopRegisterMemoryLeak;
    UnregisterExpectedMemoryLeak: TopUnregisterMemoryLeak);

type
  PThreadRec = ^TThreadRec;
  TThreadRec = record
    {
      WARNING: Don't change these fields without also changing them in
      the C++ RTL : winrtl/source/vcl/crtlvcl.cpp
    }
    Func: TThreadFunc;
    Parameter: Pointer;
    OrigFunc: TThreadFunc;
  end;

var
  TThreadThreadFunc: TThreadFunc = nil;
  PreviousEndHook: TSystemThreadEndProc = nil;

{StackAlloc is from Borland Grids.pas
 StackAlloc allocates a 'small' block of memory from the stack by
 decrementing SP.  This provides the allocation speed of a local variable,
 but the runtime size flexibility of heap allocated memory. }

function StackAlloc(Size: Integer): Pointer; register;
asm
  POP   ECX          { return address }
  MOV   EDX, ESP
  ADD   EAX, 3
  AND   EAX, not 3   // round up to keep ESP dword aligned
  CMP   EAX, 4092
  JLE   @@2
@@1:
  SUB   ESP, 4092
  PUSH  EAX          { make sure we touch guard page, to grow stack }
  SUB   EAX, 4096
  JNS   @@1
  ADD   EAX, 4096
@@2:
  SUB   ESP, EAX
  MOV   EAX, ESP     { function result = low memory address of block }
  PUSH  EDX          { save original SP, for cleanup }
  MOV   EDX, ESP
  SUB   EDX, 4
  PUSH  EDX          { save current SP, for sanity check  (sp = [sp]) }
  PUSH  ECX          { return to caller }
end;

const
  cStackOffsetMaxInBytes: Cardinal = 32768;

function AllocSize(const AID: Cardinal): Integer;
var
  lBlockSize: Cardinal;
begin
  lBlockSize := cStackOffsetMaxInBytes div 32;
  Result := Integer((AID mod 2) * 15 * lBlockSize + (AID mod 4) * 3 * lBlockSize + (AID mod 8) * lBlockSize + (Cardinal(Random(lBlockSize))));
  Result := 16 + Result and ($FFFFFFFF - $3);
end;

function DoThreadFunc(Parameter: Pointer): Integer;
begin
  // Fat chance we get here first and we can reserve the manager
  TopMM.MarkAsDelphiThread(TLSGetThreadManager);
  // Move Stack so Threads are not 64K aligned (64K alisaing slowdown problem). Allocating a variable memory amount
  StackAlloc(AllocSize(TLSGetThreadManager.SequenceID));
  //
  Result := TThreadThreadFunc(Parameter);
end;

procedure ReleaseThreadManager(ExitCode: Integer);
begin
{$IFDEF TOPBLOCK}
  TopMM.Lock;
  try
{$ENDIF}
    // call the previous hook, if any
    if Assigned(PreviousEndHook) then PreviousEndHook(ExitCode);
    // Free up Manager assigned to thread
    TopMM.ReleaseThreadManager(TLSGetThreadManager);
{$IFDEF TOPBLOCK}
  finally
    TopMM.Unlock;
  end;
{$ENDIF}
end;

function HookThreadFunc(ThreadFunc: TThreadFunc; Parameter: Pointer): Pointer;
var P: PThreadRec;
begin
   // First one is Topmaintenance. Use this as TThread recognizer (All will be TThreads descendants anyway)
  if @TThreadThreadFunc = nil then
    TThreadThreadFunc := ThreadFunc;
  New(P);
  if @ThreadFunc <> @TThreadThreadFunc then
    P.Func := ThreadFunc
  else
    P.Func := DoThreadFunc;
  P.Parameter := Parameter;
  Result := P;
end;

procedure PatchThread;
begin
  // Start
  // SystemThreadFuncProc should be NIL !!
  SystemThreadFuncProc := HookThreadFunc;
  // end
  PreviousEndHook := SystemThreadEndProc;
  SystemThreadEndProc := @ReleaseThreadManager;
end;

procedure UnPatchThread;
begin
  // end
  SystemThreadEndProc := PreviousEndHook;
  // start
  SystemThreadFuncProc := nil;
end;
{$IFEND}

procedure TopMMInstall;
begin                 
  cTLSThreadManagers := TlsAlloc;
  TopMM := TThreadManagerList.Create;
  PatchThread;

{$warn symbol_deprecated off}
{$warn symbol_platform off}
  if System.IsMemoryManagerSet or
    (System.GetHeapStatus.TotalAllocated > 0) then exit;
{$warn symbol_platform on}
{$warn symbol_deprecated on}

  // Hook memory Manager
  GetMemoryManager(OldMM);
  if @OldMM <> @TopManager then
  begin
    SetMemoryManager(TopManager);
  end;
end;

procedure TopMMUnInstall;
begin
  // Some threads might still be running. Therefore we do not undo our works. Windows will clean it all
  // up in a single blinding flash. We do however report leaks at this point in time
  //
  if Assigned(TopMM) then
  begin
    TopMM.ReportLeaks;
   //
{$IF COMPILERVERSION<18}cIgnoreFree := True; {$IFEND} // Fixes D7 and before mixing up free'ing in the wrong memory manager
   //
   {SetMemoryManager(OldMM);
   UnPatchThread;
   TopMM.Free;}
  end;
  //
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

initialization
{$IFDEF TOPDEBUG}
  PatchINT3;
{$ENDIF}
  TopMMInstall;

finalization
  TopMMUninstall;

end.

