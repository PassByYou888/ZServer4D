unit VirtualMM;

interface

uses windows;

type
  TMemHeader = object
    Size: NativeUInt;
  end;
  PMemHeader = ^TMemHeader;

implementation

// Allocate memory block.
function VirtualGetMem(aSize: Integer): Pointer;
var
  iSize: Integer;
begin
  iSize  := aSize + SizeOf(TMemHeader) + (aSize shr 3); {1/8 extra}
  Result := VirtualAlloc(nil, iSize,
                         MEM_COMMIT {$ifdef AlwaysAllocateTopDown} or MEM_TOP_DOWN{$endif},
                         PAGE_READWRITE);
  TMemHeader(Result^).Size := iSize;
  Result := Pointer(NativeUInt(Result) + SizeOf(TMemHeader));
end;

// Deallocate memory block.
function VirtualFreeMem(aMemory: Pointer): Integer;
var
  ph: PMemHeader;
begin
  ph     := PMemHeader(NativeUInt(aMemory) - SizeOf(TMemHeader));
  if VirtualFree(ph, 0, MEM_RELEASE) then
    Result := 0
  else
    Result := -1;
end;

// Resize memory block.
function VirtualReallocMem(aMemory: Pointer; aSize: Integer): Pointer;
var
  ph: PMemHeader;
  iSize: Integer;
begin
  ph    := PMemHeader(NativeUInt(aMemory) - SizeOf(TMemHeader));
  iSize := aSize + SizeOf(TMemHeader);

//  iSize := iSize + (aSize shr 3); {1/8 extra}

  // new size smaller than current size?
  if (NativeUInt(iSize) <= ph.Size) then
  begin
    if NativeUInt(iSize) >= (ph.Size shr 2) then
    begin
      Result := aMemory; // no resize needed up to half the current item size -> now a quarter, benchmark optimization...
    end
    // too much downscaling: use move
    else
    begin
      Result := VirtualGetMem(aSize); // new mem
      if aMemory <> Result then
      begin
        Move(aMemory^, Result^, aSize); // copy (use smaller new size)
        VirtualFreeMem(aMemory); // free old mem
      end;
    end;
  end
  else
  begin
    // new size bigger than current size
    Result := VirtualGetMem(aSize); // new mem
    if aMemory <> Result then
    begin
      Move(aMemory^, Result^, ph.Size); // copy (use smaller old size)
      VirtualFreeMem(aMemory); // free old mem
    end;
  end;

  {
  Result := HeapRealloc( HeapHandle, HeapFlags and (HEAP_NO_SERIALIZE and
         HEAP_GENERATE_EXCEPTIONS and HEAP_ZERO_MEMORY),
         // (Prevent using flag HEAP_REALLOC_IN_PLACE_ONLY here - to allow
         // system to move the block if necessary).
         ph, iSize );
  TMemHeader(Result^).Size := iSize;
  Result := Pointer(NativeUInt(Result) + SizeOf(TMemHeader));
  }
end;

const
  HeapMemoryManager: TMemoryManager = (
    GetMem: VirtualGetMem;
    FreeMem: VirtualFreeMem;
    ReallocMem: VirtualReallocMem);

var OldMM: TMemoryManager;

initialization
  GetMemoryManager( OldMM );
  SetMemoryManager( HeapMemoryManager );

finalization
  SetMemoryManager( OldMM );

end.
