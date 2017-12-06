unit WinMem;

// WinMem - replacement memory manager that uses the Windows Heap functions
// usage: add WinMem as the first unit in the project's USES declaration.
//        WinMem must be first, and must be used by the project.
//
// In some circumstances, the Windows memory manager will offer better
// performance than Delphi's default memory manager. In other situations,
// the performance will be worse. Try both and compare.
//
// Copyright (c) 2000 Tempest Software, Inc.
// You can use this unit in any application or other project for free.
// This software is offered as is, without support. Share and enjoy!

interface

implementation

// Do not use any units that allocate memory. This unit must set
// up its memory manager before any memory allocation request, otherwise
// the memory would be allocated by one memory manager and freed
// by another: a sure recipe for disaster.
uses Windows;

// The Windows unit does not declare the Heap flags, for some reason.
const
  Heap_No_Serialize = 1; // from Winnt.h

// Create a private heap just for the memory manager. A single-threaded
// application can use the Heap_No_Serialize flag to improve performance
// somewhat, something that you can't do if you just use the default
// process heap.
var
  Heap: THandle;

// Allocate Size bytes and return a pointer to uninitialized memory,
// or nil for a failure.
function HeapGetMem(Size: Integer): Pointer;
var
  Flags: Cardinal;
begin
  if IsMultiThread then
    Flags := 0
  else
    Flags := Heap_No_Serialize;

  Result := HeapAlloc(Heap, Flags, Size);
end;

// Free the pointer. Return 0 for success, and 1 for failure.
function HeapFreeMem(Ptr: Pointer): Integer;
var
  Flags: Cardinal;
begin
  if IsMultiThread then
    Flags := 0
  else
    Flags := Heap_No_Serialize;

  if HeapFree(Heap, Flags, Ptr) then
    Result := 0
  else
    Result := 1;
end;

// Reallocate Size bytes, and return the new pointer,
// or nil for a failure.
function HeapReallocMem(Ptr: Pointer; Size: Integer): Pointer;
var
  Flags: Cardinal;
begin
  if IsMultiThread then
    Flags := 0
  else
    Flags := Heap_No_Serialize;

  Result := HeapRealloc(Heap, Flags, Ptr, Size);
end;

// Setup the new memory manager.
procedure InitMemoryManager;
resourcestring
  sError = 'Programmer error: WinMem unit must be first unit used by the project';
var
  MemMgr: TMemoryManager;
begin
  // First make sure no memory has been allocated with another memory manager.
  // It would be very bad if that memory were freed with this manager.
  Assert(AllocMemCount = 0, sError);

  Heap := HeapCreate(0, 0, 0);
  if Heap = 0 then
    // Because this unit is the first one, the exception-handling system
    // is not yet set up, so raise a runtime error instead.
    RunError(203); // out of memory

  MemMgr.GetMem := HeapGetMem;
  MemMgr.FreeMem := HeapFreeMem;
  MemMgr.ReallocMem := HeapReallocMem;
  SetMemoryManager(MemMgr);
end;

initialization
  InitMemoryManager;
finalization
  if Heap <> 0 then
    HeapDestroy(Heap);
end.
