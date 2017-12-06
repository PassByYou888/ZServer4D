{****************************************************************************************

TOPMEMORY v3.55 - HIGH PERFORMANCE DELPHI MEMORY MANAGER  (C) 2008 Ivo Tops, Topsoftware

  TopLocalObjects does all low-level memory related routines

****************************************************************************************}
unit TopLocalObjects;

interface

{$IFNDEF TOPDEBUG} // Debugging off unless you use the TOPDEBUG directive
{$D-,L-}
{$ENDIF}
{$X+}

uses
  Windows,
  TopLib_CopyMemory;

var
  cVirtualAbove: Cardinal = 0; // dynamically determined at startup
  cWinAllocSize: Cardinal = 0; // dynamically determined at startup

// Memory Allocation wrappers
function TopLocalMemAlloc(const Size: Cardinal): Pointer; {$IF COMPILERVERSION>=18}inline; {$IFEND}
function TopLocalMemZeroAlloc(const Size: Cardinal): Pointer; {$IF COMPILERVERSION>=18}inline; {$IFEND}
function TopLocalMemReAlloc(const Loc: Pointer; const NewSize: Cardinal; const AOldSize: Cardinal = 0): Pointer; {$IF COMPILERVERSION>=18}inline; {$IFEND}
procedure TopLocalMemFree(const Loc: Pointer); {$IF COMPILERVERSION>=18}inline; {$IFEND}
function TopVirtualMemAlloc(const Size: Cardinal): Pointer; {$IF COMPILERVERSION>=18}inline; {$IFEND}
function TopVirtualMemReAlloc(const Loc: Pointer; const OldSize, NewSize: Cardinal): Pointer;
procedure TopVirtualMemFree(const Loc: Pointer); {$IF COMPILERVERSION>=18}inline; {$IFEND}
// Reallocate a List to New Capacity
function FixCapacity(const Loc: Pointer; var Capacity: Integer; const Rowsize: Integer; const ZeroNewMemory: Boolean = False): Pointer;
function FixedCapacity(const Loc: Pointer; const OldCapacity, NewCapacity: Integer; const Rowsize: Integer; const ZeroNewMemory: Boolean): Pointer;

// Quick way to find out if a thread is still alive
function IsThreadAlive(AThreadHandle: THandle): Boolean;

type
  TLocalObject = class
  public
    class function NewInstance: TObject; override;
    procedure FreeInstance; override;
  end;

var
  TopHeap: THandle = 0;

implementation

uses
{$IFDEF TOPDEBUG}
  TopInstall,
{$ENDIF}
  TopLib_SSE2,
  SysUtils;

  {const // Windows Heap Constants
    HEAP_GENERATE_EXCEPTIONS = 4;
      //The operating-system raises an exception to indicate a function failure, such as an out-of-memory condition, instead of returning NULL.
    HEAP_NO_SERIALIZE = 1;
      // Serialized access will not be used.
      // To ensure that serialized access is disabled for all calls to this function, specify HEAP_NO_SERIALIZE in the call to HeapCreate. In this case, it is not necessary to additionally specify HEAP_NO_SERIALIZE in this function call.
      // This value should not be specified when accessing the process heap. The system may create additional threads within the application's process, such as a CTRL+C handler, that simultaneously access the process heap.
    HEAP_REALLOC_IN_PLACE_ONLY = 16;
      //There can be no movement when reallocating a memory block. If this value is not specified, the function may move the block to a new location. If this value is specified and the block cannot be resized without moving, the function fails, leaving the original memory block unchanged.
    HEAP_ZERO_MEMORY = 8;
      //  If the reallocation request is for a larger size, the additional region of memory beyond the original size be initialized to zero. The contents of the memory block up to its original size are unaffected.
  }

procedure TopLocalMemFree(const Loc: Pointer);
begin
  HeapFree(Topheap, 0, Loc);
end;

procedure TopVirtualMemFree(const Loc: Pointer);
begin
  VirtualFree(Loc, 0, MEM_RELEASE);
end;

function TopLocalMemAlloc(const Size: Cardinal): Pointer;
begin
  Result := HeapAlloc(TopHeap, 0, Size);
end;

function TopLocalMemZeroAlloc(const Size: Cardinal): Pointer;
begin
  Result := TopLocalMemAlloc(Size);
  if Result <> nil then TopFillMemory(Result, Size, 0);
end;

function TopLocalMemReAlloc(const Loc: Pointer; const NewSize: Cardinal; const AOldSize: Cardinal): Pointer;
begin
  if AOldSize > 0 then
  begin
    Result := HeapReAlloc(TopHeap, $10 {HEAP_REALLOC_IN_PLACE_ONLY -> Unknown in older Delphi versions}, Loc, NewSize);
    // not in place?
    if Result = nil then
    begin
      Result := TopLocalMemAlloc(NewSize);
      if Result <> nil then
      begin
        TopMoveMemory(Result, Loc, AOldSize);
        TopLocalMemFree(Loc);
      end;
    end;
  end else
  begin
    Result := HeapReAlloc(TopHeap, 0, Loc, NewSize);
  end;
{$IFDEF TOPDEBUG}
  if Result = nil then DebugError('Local Memory re-allocation failed');
{$ENDIF}

end;


function TopVirtualMemAlloc(const Size: Cardinal): Pointer;
begin
  Result := VirtualAlloc(nil, size, MEM_COMMIT, PAGE_READWRITE);
end;

function TopVirtualMemReAlloc(const Loc: Pointer; const OldSize, NewSize: Cardinal): Pointer;
var
  CopySize: Cardinal;
begin
  // From Virtual To Virtual
  Result := TopVirtualMemAlloc(NewSize);
  if Result <> nil then
  begin
    // Copy as much as possible
    if OldSize < NewSize then
      CopySize := OldSize
    else
      CopySize := NewSize;
    // Non overlapping large block, use SSE if possible
    TopMoveMemory(Result, Loc, CopySize);
    // Free old data
    TopVirtualMemFree(Loc);
  end;
{$IFDEF TOPDEBUG}
  if Result = nil then
    DebugError('Virtual Memory re-allocation failed');
{$ENDIF}
end;


procedure TLocalObject.FreeInstance;
begin
  CleanupInstance;
  TopLocalMemFree(Self);
end;

class function TLocalObject.NewInstance: TObject;
begin
  Result := InitInstance(TopLocalMemAlloc(InstanceSize));
{$IFDEF TOPDEBUG}
  if Result = nil then
    DebugError('Local Instance creation failed');
{$ENDIF}
end;

// Allocate more size in a list

function FixCapacity(const Loc: Pointer; var Capacity: Integer; const Rowsize: Integer; const ZeroNewMemory: Boolean): Pointer;
var
  NewCapacity: Integer;
begin
  if Loc = nil then
  begin
    NewCapacity := 2;
    Result := TopLocalMemAlloc(NewCapacity * RowSize);
  end
  else
  begin
    NewCapacity := Capacity * 2;
    Result := TopLocalMemReAlloc(Loc, NewCapacity * RowSize);
  end;
{$IFDEF TOPDEBUG}
  if Result = nil then
    DebugError('List Alloc/ReAlloc failure');
{$ENDIF}
  if Result <> nil then
  begin
    if ZeroNewMemory then
      FillChar(Pointer(Cardinal(Result) + Cardinal(Capacity * RowSize))^, (NewCapacity - Capacity) * RowSize, 0);
    Capacity := NewCapacity;
  end
  else
    Result := Loc;
end;

// Allocate specific size in list

function FixedCapacity(const Loc: Pointer; const OldCapacity, NewCapacity: Integer; const Rowsize: Integer; const ZeroNewMemory: Boolean): Pointer;
begin
  if Loc = nil then
  begin
    Result := TopLocalMemAlloc(NewCapacity * RowSize);
  end
  else
  begin
    Result := TopLocalMemReAlloc(Loc, NewCapacity * RowSize);
  end;
{$IFDEF TOPDEBUG}
  if Result = nil then
    DebugError('List Alloc/ReAlloc failure');
{$ENDIF}
  if Result <> nil then
  begin
    if ZeroNewMemory then TopFillMemory(Pointer(Cardinal(Result) + Cardinal(OldCapacity * RowSize)), (NewCapacity - OldCapacity) * RowSize, 0);
//      FillChar(Pointer(Cardinal(Result) + Cardinal(OldCapacity * RowSize))^, (NewCapacity - OldCapacity) * RowSize, 0);
  end
  else
    Result := Loc;
end;


// TODO: Deze functie zit alleen in Win2K+SP? en WinXP. Dynamisch checken of de functie er is en alleen aanroepen indien aanwezig
//function HeapSetInformation(hHeap: THandle; dwFlags: DWORD; lpMem: Pointer; dwBytes: DWORD): BOOL; external kernel32 name 'HeapSetInformation';
//const LowFragHeap:DWord =2;

procedure TopCreateHeap;
begin
  if TopHeap = 0 then
  begin
    TopHeap := HeapCreate(0, 256 * 1024, 0); // Start with 256 KB, grow forever
//   HeapSetInformation(TopHeap,0,@LowFragHeap,SizeOf(LowFragHeap));
  end;
end;


procedure TopDestroyHeap;
begin
  if TopHeap <> 0 then
  begin
    HeapDestroy(TopHeap);
    TopHeap := 0;
  end;
end;


function IsThreadAlive(AThreadHandle: THandle): Boolean;
begin
   // 0 Handle must return TRUE
  Result := (AThreadHandle = 0) or (GetThreadPriority(AThreadHandle) <> THREAD_PRIORITY_ERROR_RETURN);
end;

procedure TopInitSizes;
var
  lInfo: _SYSTEM_INFO;
begin
  GetSystemInfo(lInfo);
  //
  cWinAllocSize := lInfo.dwAllocationGranularity;
  // $7FFF8 is windows documented limit where Heap will go to virtualalloc
  if cWinAllocSize * 4 <$7FFF8 then  cVirtualAbove := cWinAllocSize * 4  else cVirtualAbove := $7FFF8;
end;

initialization
  TopInitSizes;
  TopCreateHeap;

finalization
  // We do not destroy the heap as threads might still be running and using TopMemoryManager
  // Windows will dispose of the heap when application closes
  //
  // TopDestroyHeap;

end.

