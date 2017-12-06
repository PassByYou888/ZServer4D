{****************************************************************************************

  TOPMEMORY v1.42 - HIGH PERFORMANCE DELPHI MEMORY MANAGER  (C) 2004 Ivo Tops, Topsoftware

  TopLocalObjects does all low-level memory related routines

****************************************************************************************}
unit TopLocalObjects;

interface

uses Windows,TopCopyMemory;

const
  cWinPageSize = 64 * 1024; // 64 KB
  cVirtualAbove = MaxInt-1; // cWinPageSize; // WinHeapManager is very fast and support in place re-alloc, no need to use virtualalloc.

  // Memory Allocation wrappers
function TopLocalMemAlloc(const Size: Integer): Pointer;
function TopLocalMemZeroAlloc(const Size: Integer): Pointer;
function TopLocalMemReAlloc(const Loc: Pointer; const NewSize: Integer): Pointer;
procedure TopLocalMemFree(const Loc: Pointer);
function TopVirtualMemAlloc(const Size: Integer): Pointer;
function TopVirtualMemReAlloc(const Loc: Pointer; const OldSize, NewSize: Integer): Pointer;
procedure TopVirtualMemFree(const Loc: Pointer);
// Reallocate a List to New Capacity
function FixCapacity(const Loc: Pointer; var Capacity: Integer; const Rowsize: Integer; const ZeroNewMemory: Boolean = False): Pointer;
function FixedCapacity(const Loc: Pointer; const OldCapacity, NewCapacity: Integer; const Rowsize: Integer; const ZeroNewMemory: Boolean): Pointer;

type
  TLocalObject = class
  public
    class function NewInstance: TObject; override;
    procedure FreeInstance; override;
  end;

implementation

uses TopInstall;

{$IFNDEF TOPUSEOLDMM}
var
  TopHeap: THandle = 0;
{$ENDIF}

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
      //	If the reallocation request is for a larger size, the additional region of memory beyond the original size be initialized to zero. The contents of the memory block up to its original size are unaffected.
  }

function TopLocalMemZeroAlloc(const Size: Integer): Pointer;
begin
  Result := TopLocalMemAlloc(Size);
  if Result <> nil then
    ZeroMemory(Result, Size);
end;

function TopLocalMemAlloc(const Size: Integer): Pointer;
begin
{$IFNDEF TOPUSEOLDMM}
  Result := HeapAlloc(TopHeap, 0, Size);
  // Check if we get way too much back from the OS
  // if HeapSize(TopHeap,0,Result)>Cardinal(Size) then
  //   DebugError('Heap returned more the needed');
{$ELSE}
  Result := OldMM.GetMem(Size);
{$ENDIF}
{$IFDEF TOPDEBUG}
  if Result = nil then
    DebugError('Local Memory allocation failed');
{$ENDIF}
end;

function TopLocalMemReAlloc(const Loc: Pointer; const NewSize: Integer): Pointer;
begin
{$IFNDEF TOPUSEOLDMM}
  Result := HeapReAlloc(TopHeap, 0, Loc, NewSize);
{$ELSE}
  Result := OldMM.ReallocMem(Loc, NewSize);
{$ENDIF}
{$IFDEF TOPDEBUG}
  if Result = nil then
    DebugError('Local Memory re-allocation failed');
{$ENDIF}
end;

procedure TopLocalMemFree(const Loc: Pointer);
begin
  if Loc <> nil then
  begin
{$IFNDEF TOPUSEOLDMM}
    HeapFree(Topheap, 0, Loc);
{$ELSE}
    OldMM.FreeMem(Loc);
{$ENDIF}
  end;
end;

function TopVirtualMemAlloc(const Size: Integer): Pointer;
begin
{$IFNDEF TOPUSEOLDMM}
  Result := VirtualAlloc(nil, size, MEM_COMMIT, PAGE_READWRITE);
{$ELSE}
  Result := TopLocalMemAlloc(Size);
{$ENDIF}
end;

function TopVirtualMemReAlloc(const Loc: Pointer; const OldSize, NewSize: Integer): Pointer;
{$IFNDEF TOPUSEOLDMM}
var
  CopySize: Integer;
{$ENDIF}
begin
{$IFNDEF TOPUSEOLDMM}
  // From Virtual To Virtual
  Result := TopVirtualMemAlloc(NewSize);
  if Result <> nil then
  begin
    // Copy as much as possible
    if OldSize < NewSize then
      CopySize := OldSize
    else
      CopySize := NewSize;
    // Non overlapping large block, use MMX if possible
    MMXCopyMemory(Result, Loc, CopySize);
    // Free old data
    TopVirtualMemFree(Loc);
  end;
{$ELSE}
  Result := TopLocalMemRealloc(Loc, NewSize);
{$ENDIF}
{$IFDEF TOPDEBUG}
  if Result = nil then
    DebugError('Virtual Memory re-allocation failed');
{$ENDIF}
end;

procedure TopVirtualMemFree(const Loc: Pointer);
begin
  if Loc <> nil then
  begin
{$IFNDEF TOPUSEOLDMM}
    VirtualFree(Loc, 0, MEM_RELEASE);
{$ELSE}
    TopLocalMemFree(Loc);
{$ENDIF}
  end;
end;

procedure TLocalObject.FreeInstance;
begin
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
      ZeroMemory(Pointer(Cardinal(Result) + Cardinal(Capacity * RowSize)), (NewCapacity - Capacity) * RowSize);
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
    if ZeroNewMemory then
      ZeroMemory(Pointer(Cardinal(Result) + Cardinal(OldCapacity * RowSize)), (NewCapacity - OldCapacity) * RowSize);
  end
  else
    Result := Loc;
end;


// TODO: Deze functie zit alleen in Win2K+SP? en WinXP. Dynamisch checken of de functie er is en alleen aanroepen indien aanwezig
// function HeapSetInformation(hHeap: THandle; dwFlags: DWORD; lpMem: Pointer; dwBytes: DWORD): BOOL; external kernel32 name 'HeapSetInformation';
procedure TopCreateHeap;
begin
{$IFNDEF TOPUSEOLDMM}
  if TopHeap = 0 then
    TopHeap := HeapCreate(0, 512 * 1024, 0); // Start with 512 KB, grow forever, Serialized by OS
{$ENDIF}
end;

procedure TopDestroyHeap;
begin
{$IFNDEF TOPUSEOLDMM}
  if TopHeap <> 0 then
  begin
    HeapDestroy(TopHeap);
    TopHeap := 0;
  end;
{$ENDIF}
end;

initialization
  TopCreateHeap;

finalization
  // We do not destroy the heap as threads might still be running and using TopMemoryManager
  // Windows will dispose of the heap when application closes
  //
  // TopDestroyHeap;

end.

