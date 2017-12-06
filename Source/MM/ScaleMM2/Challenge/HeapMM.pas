{
  Alternative memory manager. To use it, just place a reference to this
  unit *FIRST* in the uses clause of your project (dpr-file). It is a good idea
  to use this memory manager with system dcu replacement by Vladimir Kladov.

  Heap API used, which is fast and very effective (allocated block granularity
  is 16 bytes). One additional benefit is that some proofing tools (MemProof)
  do not detect API failures, which those can find when standard Delphi memory
  manager used.
  =====================================================================
  Copyright (C) by Vladimir Kladov, 2001
  ---------------------------------------------------------------------
  http://xcl.cjb.net
  mailto: bonanzas@xcl.cjb.net
}

unit HeapMM;

interface

uses windows;

const
  HEAP_NO_SERIALIZE              = $00001;
  HEAP_GROWABLE                  = $00002;
  HEAP_GENERATE_EXCEPTIONS       = $00004;
  HEAP_ZERO_MEMORY               = $00008;
  HEAP_REALLOC_IN_PLACE_ONLY     = $00010;
  HEAP_TAIL_CHECKING_ENABLED     = $00020;
  HEAP_FREE_CHECKING_ENABLED     = $00040;
  HEAP_DISABLE_COALESCE_ON_FREE  = $00080;
  HEAP_CREATE_ALIGN_16           = $10000;
  HEAP_CREATE_ENABLE_TRACING     = $20000;
  HEAP_MAXIMUM_TAG               = $00FFF;
  HEAP_PSEUDO_TAG_FLAG           = $08000;
  HEAP_TAG_SHIFT                 =  16   ;

{$DEFINE USE_PROCESS_HEAP}

var
  HeapHandle: THandle;
  {* Global handle to the heap. Do not change it! }

  HeapFlags: DWORD = 0;
  {* Possible flags are:
     HEAP_GENERATE_EXCEPTIONS - system will raise an exception to indicate a
                              function failure, such as an out-of-memory
                              condition, instead of returning NULL.
     HEAP_NO_SERIALIZE - mutual exclusion will not be used while the HeapAlloc
                       function is accessing the heap. Be careful!
                       Not recommended for multi-thread applications.
                       But faster.
     HEAP_ZERO_MEMORY - obviously. (Slower!)
  }

  { Note from MSDN:
    The granularity of heap allocations in Win32 is 16 bytes. So if you
    request a global memory allocation of 1 byte, the heap returns a pointer
    to a chunk of memory, guaranteeing that the 1 byte is available. Chances
    are, 16 bytes will actually be available because the heap cannot allocate
    less than 16 bytes at a time.
  }

type
  TMemHeader = object
    /// the memory block handler which owns this memory block
    //Owner: PMemBlock;
    /// linked to next single memory item (other thread freem mem)
    //NextMem: PMemHeader;
    Size: NativeUInt;
  end;
  PMemHeader = ^TMemHeader;

implementation

// Allocate memory block.
function HeapGetMem(aSize: Integer): Pointer;
var
  iSize: Integer;
begin
  iSize  := aSize + SizeOf(TMemHeader) + (aSize shr 3); {1/8 extra}
  Result := HeapAlloc( HeapHandle, HeapFlags, iSize );
  TMemHeader(Result^).Size := iSize;
  Result := Pointer(NativeUInt(Result) + SizeOf(TMemHeader));
end;

// Deallocate memory block.
function HeapFreeMem(aMemory: Pointer): Integer;
var
  ph: PMemHeader;
begin
  ph     := PMemHeader(NativeUInt(aMemory) - SizeOf(TMemHeader));
  Result := Integer(not HeapFree(HeapHandle, HeapFlags and HEAP_NO_SERIALIZE, ph) );
end;

// Resize memory block.
function HeapReallocMem(aMemory: Pointer; aSize: Integer): Pointer;
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
      Exit;
    end
  end;
  {
    // too much downscaling: use move
    else
    begin
      Result := HeapGetMem(aSize); // new mem
      if aMemory <> Result then
      begin
        Move(aMemory^, Result^, aSize); // copy (use smaller new size)
        HeapFreeMem(aMemory); // free old mem
      end;
    end;
  end
  else
  begin
    // new size bigger than current size
    Result := HeapGetMem(aSize); // new mem
    if aMemory <> Result then
    begin
      Move(aMemory^, Result^, ph.Size); // copy (use smaller old size)
      HeapFreeMem(aMemory); // free old mem
    end;
  end;
  }

  Result := HeapRealloc( HeapHandle, HeapFlags and (HEAP_NO_SERIALIZE and
         HEAP_GENERATE_EXCEPTIONS and HEAP_ZERO_MEMORY),
         // (Prevent using flag HEAP_REALLOC_IN_PLACE_ONLY here - to allow
         // system to move the block if necessary).
         ph, iSize );
  TMemHeader(Result^).Size := iSize;
  Result := Pointer(NativeUInt(Result) + SizeOf(TMemHeader));
end;

const
  HeapMemoryManager: TMemoryManager = (
    GetMem: HeapGetMem;
    FreeMem: HeapFreeMem;
    ReallocMem: HeapReallocMem);

var OldMM: TMemoryManager;

initialization
  {$IFDEF USE_PROCESS_HEAP}
  HeapHandle := GetProcessHeap;
  {$ELSE}
  HeapHandle := HeapCreate( 0, 0, 0 );
  {$ENDIF}
  GetMemoryManager( OldMM );
  SetMemoryManager( HeapMemoryManager );

finalization
  SetMemoryManager( OldMM );
  {$IFNDEF USE_PROCESS_HEAP}
  HeapDestroy( HeapHandle );
  {$ENDIF}

end.
