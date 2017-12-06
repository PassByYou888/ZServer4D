unit BucketMem;

(*
  (c) 2001-2005 Robert Houdart, Link Software
  This code can be freely used and/or modified, as long as you kindly acknowledge the origin.

  BucketMem is a replacement Memory Manager for Delphi 5,6,7,2005 and Kylix that does not suffer
  from fragmentation and delivers much better performance in a multi-threaded application.

  HOW TO USE: Simply add BucketMem as the very first unit in your project (dpr) file.

  The memory management is performed at 3 levels:
    1) the V-Block level that allocates OS memory chunks of 256 kB..1024 kB of committed memory
    2) each V-Block is divided into "Sheets" of 16 kB..144 kB
    3) each "Sheet" is used to create memory blocks of a given size (16 bytes up to 144 kB)
  The sheets are managed by "buckets".
  BucketMem uses about 50 buckets that each deliver memory blocks of a given size.

  A Bucket
   - manages a number of Sheets in a double-linked list
   - has an ActiveSheet out of which memory blocks are served until exhaustion
   - if ActiveSheet has been used up, the Bucket continues with the next Sheet that has free blocks;
     if none is available, a new Sheet is created

  A Sheet
   - is a 16 kB..144 kB memory zone divided in BlockCount equal blocks with size BlockSize
   - memory blocks are served either linearly or from a single-linked list of free blocks
   - Sheets are allocated from from V-Blocks

  A Memory Block
   - 4 bytes overhead to store the Sheet to which the memory block belongs
   - detects freeing an invalid block or twice the same memory block

  A V-Block
   - is a 256 kB..1024 kB zone of committed Virtual Memory (Windows)
   - will be sub-divided into a number of Sheets

  History:
    7/01/2001 first production version for eLink
   17/07/2004 Kylix compatibility (without using $IFDEF MSWINDOWS to remain Delphi 5 compatible)
   18/01/2005 BucketMem 1.0 First version sent to FastCode MM Challenge using spinlocks instead of
              critical sections
    1/05/2005 BucketMem 1.6 Cleaned-up version with better cache associativity and buckets up to 144 kB
    7/05/2005 BucketMem 1.6.1 Restored Kylix compatibility; Fixed Windows 98 VirtualFree problem
*)


{$O+,R-,Q-,I-,B-,D-,WARNINGS OFF}
// optimization, no range check, no overflow check, no I/O check,
// shortcut boolean evaluation, no debug information (to avoid tracing into this unit), no warnings

{.$D+,WARNINGS ON}            // activate to debug this unit
{$define SPINLOCK}            // activate to use spinlocks instead of critical sections
{$define USEISMULTITHREAD}    // activate to take into account the variable IsMultiThread
{$define ALIGN16BYTE}         // activate to align all memory allocations to 16-byte boundaries
{$define USECACHEOFFSET}      // give random offset to large blocks to improve cache associativity
{.$define USESYSTEMMOVE}      // activate when you're using FastMove in your project

{$ifdef LINUX}
  {$undef USECACHEOFFSET}     // cache offset is not useful in Linux
  {$undef SPINLOCK}           // use critical sections in Linux
{$endif}

interface

implementation

{$ifdef LINUX}
uses
  Libc;
{$else}
uses
  Windows;
{$endif}

type
  PBlockUsed = ^TBlockUsed;
  PHeapBlockUsed = ^THeapBlockUsed;
  PBlockFree = ^TBlockFree;
  PSheet = ^TSheet;
  PBucket = ^TBucket;

  TBucket = record
    {$ifdef SPINLOCK}
    SpinLock: Integer;
    {$else}
    CriticalSection: TRTLCriticalSection;
    {$endif}
    BlockSize: Cardinal;
    ActiveSheet: PSheet;
    CurAddr: Cardinal;
    MaxAddr: Cardinal;
    SheetCount: Cardinal;
    FirstSheet: PSheet;
    FirstFreeSheet: PSheet;
  end;

  PVirtualBlock = ^TVirtualBlock;
  PVBlockDescriptor = ^TVBlockDescriptor;

  TVirtualBlock = record
    P: Pointer;
    Available: Cardinal;
    VBlockDescriptor: PVBlockDescriptor;
    NextFreeBlock: PVirtualBlock;
    PrevFreeBlock: PVirtualBlock;
  end;

  TVBlockDescriptor = record
    SheetSize: Cardinal;
    VMSize: Cardinal;
    AvailableBits: Cardinal;
    FirstFreeVBlock: PVirtualBlock;
    {$ifdef SPINLOCK}
    SpinLock: Integer;
    {$else}
    CriticalSection: TRTLCriticalSection;
    {$endif}
    VBlockCount: Integer;
  end;

  TSheet = record  {size: 40 bytes; max 44}
    Magic: Cardinal;
    Bucket: PBucket;
    NextSheet: PSheet;
    PrevSheet: PSheet;
    NextFreeSheet: PSheet;
    PrevFreeSheet: PSheet;
    BlockUsedCount: Cardinal;
    FirstFree: PBlockFree;
    VirtualBlock: PVirtualBlock;
    VBlockIndex: Cardinal;
  end;

  TBlockUsed = record
    Sheet: PSheet;
  end;

  THeapBlockUsed = record
    n1: Cardinal;
    ReallocCount: integer;
    Size: Cardinal;
    Sheet: PSheet;
  end;

  TBlockFree = record
    NextBlock: PBlockFree;
  end;

const
  MAX_BUCKET_MEM = $23EC0;              // largest block allocated by buckets
  MAGIC_BLOCK_HEAP_ALLOC = $48454151;   // identifies a large (heap) block
  MAGIC_SHEET = $19680807;              // identifies a valid sheet
  EXACT_REALLOC_COUNT = 2;              // Number of Reallocs that will return exact requested size


// block sizes should be as close as possible to integer dividers of (SheetSize - $40)
const
  Buckets: array[0..53] of TBucket =
    ( (BlockSize: $0010), (BlockSize: $0020), (BlockSize: $0030), (BlockSize: $0040),
      (BlockSize: $0050), (BlockSize: $0060), (BlockSize: $0070), (BlockSize: $0080),
      (BlockSize: $0090), (BlockSize: $00A0), (BlockSize: $00B0), (BlockSize: $00C0),
      (BlockSize: $00D0), (BlockSize: $00E0), (BlockSize: $00F0), (BlockSize: $0100),
      (BlockSize: $0120), (BlockSize: $0140), (BlockSize: $0160), (BlockSize: $0180),
      (BlockSize: $01A0), (BlockSize: $01C0), (BlockSize: $01E0), (BlockSize: $0200),
      (BlockSize: $0240), (BlockSize: $0280), (BlockSize: $02C0), (BlockSize: $0300),
      (BlockSize: $0340), (BlockSize: $0380), (BlockSize: $03C0), (BlockSize: $0400),
      (BlockSize: $0510), (BlockSize: $0620), (BlockSize: $0750), (BlockSize: $08B0),      // $35, $2C, $25, $1F
      (BlockSize: $0A60), (BlockSize: $0C40), (BlockSize: $0E40), (BlockSize: $10E0),      // $1A, $16, $13, $10
      (BlockSize: $1350), (BlockSize: $1690), (BlockSize: $1B10), (BlockSize: $21D0),      // $0E, $0C, $0A, $08
      (BlockSize: $26A0), (BlockSize: $2D20), (BlockSize: $3620), (BlockSize: $43B0),      // $07, $06, $05, $04
      (BlockSize: $5A40),                                                                  // $03
      (BlockSize: $72F0), (BlockSize: $8FB0), (BlockSize: $BF90), (BlockSize: $11F60),     // $05, $04, $03, $02
      (BlockSize: $23EC0)                                                                  // $01
      );

var
  BucketIndexForSize: array[0..MAX_BUCKET_MEM div 16 - 1] of byte;    // bucket index for different memory sizes
  FastCodeMove: procedure(const Source; var Dest; Count : Integer);   // move function used by reallocation
  VDestroying: boolean;                                               // flag to indicate unit finalising

const
  VBlockDescriptors: array[0..2] of TVBlockDescriptor =
    ( (SheetSize: $04F00; VMSize: 16 * $04F00; AvailableBits: $0000FFFF),
      (SheetSize: $10F00; VMSize: 15 * $10F00; AvailableBits: $00007FFF),
      (SheetSize: $23F00; VMSize:  4 * $23F00; AvailableBits: $0000000F) );


(********************************************************************)
// Some utility functions requiring assembler
(********************************************************************)

{$ifdef SPINLOCK}
// InterlockedCompareExchange for Delphi 5 (avoids ifdef VER130} and Kylix compatibility
function InterlockedCompareExchange(var Destination: Longint; Exchange: Longint; Comperand: Longint): LongInt;
asm
  xchg ecx, eax
  lock cmpxchg [ecx], edx
end;
{$endif}

// bit functions for which no efficient equivalent exists in Pascal
function GetBitIndexAndClear(var Value: Cardinal): Cardinal;
asm
    mov edx, [eax]
    bsf ecx, edx
    btr edx, ecx
    mov [eax], edx
    mov eax, ecx
end;

procedure SetBitIndex(var Value: Cardinal; Index: Cardinal);
asm
    bts [eax], edx
end;


(********************************************************************)
// Faster Move Function (based on MoveJOH_IA32_9)
(********************************************************************)

procedure Move_IA32(const Source; var Dest; Count : Integer);
asm
  cmp     eax, edx
  je      @Done
  cmp     ecx, 28
  ja      @Large {Count > SMALLMOVESIZE or Count < 0}
  add     eax, ecx
  add     edx, ecx

@@SmallForwardMove_6:
  jmp     dword ptr [@@FwdJumpTable+ecx]
  nop {Align Jump Table}
@@FwdJumpTable:
  dd      @Done {Removes need to test for zero size move}
  dd      @@Fwd04, @@Fwd08, @@Fwd12, @@Fwd16
  dd      @@Fwd20, @@Fwd24, @@Fwd28
@@Fwd28:
  mov     ecx, [eax-28]
  mov     [edx-28], ecx
@@Fwd24:
  mov     ecx, [eax-24]
  mov     [edx-24], ecx
@@Fwd20:
  mov     ecx, [eax-20]
  mov     [edx-20], ecx
@@Fwd16:
  mov     ecx, [eax-16]
  mov     [edx-16], ecx
@@Fwd12:
  mov     ecx, [eax-12]
  mov     [edx-12], ecx
@@Fwd08:
  mov     ecx, [eax-8]
  mov     [edx-8], ecx
@@Fwd04:
  mov     ecx, [eax-4]
  mov     [edx-4], ecx
  ret

@Large:
  jng     @Done    {Count < 0}  {For Compatibility with Delphi's move for Count < 0}

  fild    qword ptr [eax]
  lea     eax, [eax+ecx-8]
  lea     ecx, [ecx+edx-8]
  fistp   qword ptr [edx]
  push    ecx
  neg     ecx
  and     edx, -8
  lea     ecx, [ecx+edx+8]
  pop     edx
@FwdLoop:
  fild    qword ptr [eax+ecx]
  fistp   qword ptr [edx+ecx]
  add     ecx, 8
  jl      @FwdLoop
  fild    qword ptr [eax]
  fistp   qword ptr [edx]
@Done:
  ret
end;


(********************************************************************)
// inline OS Interface to facilitate Windows/Linux compatibility
(********************************************************************)

{$ifndef LINUX}
var
  HeapHandle: THandle;    // handle for Windows heap operations
{$endif}

procedure OSHeapInit; {$ifdef ver170}inline; {$endif}
begin
  {$ifndef LINUX}
  HeapHandle := GetProcessHeap;
  {$endif}
end;

function OSHeapAlloc(Size: Cardinal): Pointer; {$ifdef ver170}inline; {$endif}
begin
  {$ifdef LINUX}
  Result := Libc.malloc(Size);
  {$else}
  Result := HeapAlloc(HeapHandle, 0, Size);
  {$endif}
end;

function OSHeapFree(P: Pointer): boolean; {$ifdef ver170}inline; {$endif}
begin
  {$ifdef LINUX}
  Libc.free(P);
  Result := True;
  {$else}
  Result := HeapFree(HeapHandle, 0, P);
  {$endif}
end;

function OSVirtualAlloc(Size: Cardinal): Pointer; {$ifdef ver170}inline; {$endif}
begin
  {$ifdef LINUX}
  Result := Libc.malloc(Size);
  {$else}
  Result := VirtualAlloc(nil, Size, MEM_COMMIT, PAGE_READWRITE);
  {$endif}
end;

function OSVirtualFree(P: Pointer): boolean; {$ifdef ver170}inline; {$endif}
begin
  {$ifdef LINUX}
  Libc.free(P);
  Result := True;
  {$else}
  Result := VirtualFree(P, 0, MEM_RELEASE);
  {$endif}
end;


(********************************************************************)
// Virtual Memory Management
(********************************************************************)

function CreateVBlock(VBlockDescriptor: PVBlockDescriptor): PVirtualBlock;
begin
  Result := OSHeapAlloc(SizeOf(TVirtualBlock));
  if Result = nil then
    Exit;
  Result.Available := VBlockDescriptor.AvailableBits;
  Result.P := OSVirtualAlloc(VBlockDescriptor.VMSize);
  if Result.P = nil then begin
    OSHeapFree(Result);
    Result := nil;
    Exit;
  end;

  Result.VBlockDescriptor := VBlockDescriptor;
  Result.PrevFreeBlock := nil;
  Result.NextFreeBlock := VBlockDescriptor.FirstFreeVBlock;
  if VBlockDescriptor.FirstFreeVBlock <> nil then
    VBlockDescriptor.FirstFreeVBlock.PrevFreeBlock := Result;
  VBlockDescriptor.FirstFreeVBlock := Result;
  inc(VBlockDescriptor.VBlockCount);
end;

procedure DestroyVBlock(VirtualBlock: PVirtualBlock);
begin
  if not VDestroying and (VirtualBlock.VBlockDescriptor.VBlockCount <= 1) then
    Exit;
  dec(VirtualBlock.VBlockDescriptor.VBlockCount);
  if VirtualBlock.NextFreeBlock <> nil then
    VirtualBlock.NextFreeBlock.PrevFreeBlock := VirtualBlock.PrevFreeBlock;
  if VirtualBlock.PrevFreeBlock <> nil then
    VirtualBlock.PrevFreeBlock.NextFreeBlock := VirtualBlock.NextFreeBlock
  else
    VirtualBlock.VBlockDescriptor.FirstFreeVBlock := VirtualBlock.NextFreeBlock;
  OSVirtualFree(VirtualBlock.P);
  OSHeapFree(VirtualBlock);
end;

procedure LockVBlocks(VBlockDescriptor: PVBlockDescriptor);
begin
  {$ifdef SPINLOCK}
  if {$ifdef USEISMULTITHREAD}IsMultiThread and{$endif} (InterLockedCompareExchange(VBlockDescriptor.SpinLock, 1, 0) <> 0) then begin
    Sleep(0);
    while InterLockedCompareExchange(VBlockDescriptor.SpinLock, 1, 0) <> 0 do
      Sleep(1);
  end;
  {$else}{$ifdef USEISMULTITHREAD}if IsMultiThread then{$endif}
  EnterCriticalSection(VBlockDescriptor.CriticalSection);
  {$endif}
end;

procedure ReleaseVBlocks(VBlockDescriptor: PVBlockDescriptor); {$ifdef ver170}inline; {$endif}
begin
  {$ifdef SPINLOCK}
  VBlockDescriptor.SpinLock := 0;
  {$else}{$ifdef USEISMULTITHREAD}if IsMultiThread then{$endif}
  LeaveCriticalSection(VBlockDescriptor.CriticalSection);
  {$endif}
end;

function VAllocSheet(VBlockDescriptor: PVBlockDescriptor): PSheet;
var
  VirtualBlock: PVirtualBlock;
  Index: Cardinal;
begin
  LockVBlocks(VBlockDescriptor);
  VirtualBlock := VBlockDescriptor.FirstFreeVBlock;
  if VirtualBlock = nil then begin
    VirtualBlock := CreateVBlock(VBlockDescriptor);
    if VirtualBlock = nil then begin
      Result := nil;
      ReleaseVBlocks(VBlockDescriptor);
      Exit;
    end;
  end;
  Index := GetBitIndexAndClear(VirtualBlock.Available);
  Result := PSheet(Cardinal(VirtualBlock.P) + Index * VBlockDescriptor.SheetSize);
  Result.VirtualBlock := VirtualBlock;
  Result.VBlockIndex := Index;
  if VirtualBlock.Available = 0 then begin
    VBlockDescriptor.FirstFreeVBlock := VirtualBlock.NextFreeBlock;
    if VBlockDescriptor.FirstFreeVBlock <> nil then
      VBlockDescriptor.FirstFreeVBlock.PrevFreeBlock := nil;
  end;
  ReleaseVBlocks(VBlockDescriptor);
end;

procedure VFreeSheet(Sheet: PSheet);
var
  VirtualBlock: PVirtualBlock;
  VBlockDescriptor: PVBlockDescriptor;
begin
  VirtualBlock := Sheet.VirtualBlock;
  VBlockDescriptor := VirtualBlock.VBlockDescriptor;
  LockVBlocks(VBlockDescriptor);
  if VirtualBlock.Available = 0 then begin
    VirtualBlock.NextFreeBlock := VBlockDescriptor.FirstFreeVBlock;
    VirtualBlock.PrevFreeBlock := nil;
    if VBlockDescriptor.FirstFreeVBlock <> nil then
      VBlockDescriptor.FirstFreeVBlock.PrevFreeBlock := VirtualBlock;
    VBlockDescriptor.FirstFreeVBlock := VirtualBlock;
  end;
  SetBitIndex(VirtualBlock.Available, Sheet.VBlockIndex);
  if VirtualBlock.Available = VBlockDescriptor.AvailableBits then
    DestroyVBlock(VirtualBlock);
  ReleaseVBlocks(VBlockDescriptor);
end;


(********************************************************************)
// Sheet Management
(********************************************************************)

function CreateSheet(Bucket: PBucket): PSheet;
var
  Size: Cardinal;
  VBlockDescriptor: PVBlockDescriptor;
begin
  if Bucket.BlockSize <= $280 then              // use 17 kB sheets for block sizes up to 640 bytes
    VBlockDescriptor := @VBlockDescriptors[0]
  else if Bucket.BlockSize <= $5A40 then        // use 68 kB sheets for block sizes up to 22 kB
    VBlockDescriptor := @VBlockDescriptors[1]
  else                                          // use 144 kB sheets for larger blocks
    VBlockDescriptor := @VBlockDescriptors[2];
  Size := VBlockDescriptor.SheetSize;
  Result := VAllocSheet(VBlockDescriptor);

  if Result = nil then
    Exit;
  Bucket.ActiveSheet := Result;
  {$ifdef ALIGN16BYTE}
  Bucket.CurAddr := Cardinal(Result) + 44;  // + 44 so with 4 byte overhead all final blocks are 16-byte aligned
  {$else}
  Bucket.CurAddr := Cardinal(Result) + 64;  // + 64 no 16-byte alignment, but slightly better cache associativity
  {$endif}
  Bucket.MaxAddr := Cardinal(Result) + Size - Bucket.BlockSize;
  inc(Bucket.SheetCount);

  Result.Magic := MAGIC_SHEET;
  Result.Bucket := Bucket;
  if Bucket.FirstSheet = nil then begin
    Bucket.FirstSheet := Result;
    Result.PrevSheet := nil;
    Result.NextSheet := nil;
  end
  else begin
    Result.NextSheet := Bucket.FirstSheet.NextSheet;
    Result.PrevSheet := Bucket.FirstSheet;
    if Bucket.FirstSheet.NextSheet <> nil then
      Bucket.FirstSheet.NextSheet.PrevSheet := Result;
    Bucket.FirstSheet.NextSheet := Result;
  end;
  Result.BlockUsedCount := 0;
  Result.FirstFree := nil;
  Result.NextFreeSheet := nil;
  Result.PrevFreeSheet := nil;
end;

procedure DestroySheet(Sheet: PSheet);
var
  Bucket: PBucket;
begin
  Bucket := Sheet.Bucket;
  if Sheet.NextSheet <> nil then
    Sheet.NextSheet.PrevSheet := Sheet.PrevSheet;
  if Sheet.PrevSheet <> nil then
    Sheet.PrevSheet.NextSheet := Sheet.NextSheet
  else
    Bucket.FirstSheet := Sheet.NextSheet;
  if Sheet.NextFreeSheet <> nil then
    Sheet.NextFreeSheet.PrevFreeSheet := Sheet.PrevFreeSheet;
  if Sheet.PrevFreeSheet <> nil then
    Sheet.PrevFreeSheet.NextFreeSheet := Sheet.NextFreeSheet
  else
    Bucket.FirstFreeSheet := Sheet.NextFreeSheet;

  dec(Bucket.SheetCount);
  if Bucket.ActiveSheet = Sheet then begin
    Bucket.ActiveSheet := nil;
    Bucket.CurAddr := $100000;
    Bucket.MaxAddr := 0;
  end;
  VFreeSheet(Sheet);
end;


(********************************************************************)
// Bucket Management
(********************************************************************)

procedure InitBuckets;
// initialization of Buckets
var
  i, n: Cardinal;
  Bucket: PBucket;
  {$ifndef SPINLOCK}
  VBlockDescriptor: PVBlockDescriptor;
  {$endif}
begin
  for i := 0 to High(Buckets) do begin
    Bucket := @Buckets[i];
    {$ifndef SPINLOCK}
    InitializeCriticalSection(Bucket.CriticalSection);
    {$endif}
    Bucket.CurAddr := $100000;
    Bucket.MaxAddr := 0;
  end;
  n := 0;
  for i := Low(BucketIndexForSize) to High(BucketIndexForSize) do begin
    if i*16 + 1 > Buckets[n].BlockSize then
      inc(n);
    BucketIndexForSize[i] := n;
  end;
  {$ifndef SPINLOCK}
  for i := 0 to High(VBlockDescriptors) do begin
    VBlockDescriptor := @VBlockDescriptors[i];
    InitializeCriticalSection(VBlockDescriptor.CriticalSection);
  end;
  {$endif}
  OSHeapInit;
  VDestroying := False;
end;

procedure FinishBuckets;
// clean up memory
var
  i: integer;
  Bucket: PBucket;
  Sheet, NextSheet: PSheet;
  {$ifndef SPINLOCK}
  VBlockDescriptor: PVBlockDescriptor;
  {$endif}
begin
  VDestroying := True;
  for i := 0 to High(Buckets) do begin
    Bucket := @Buckets[i];
    {$ifndef SPINLOCK}
    DeleteCriticalSection(Bucket.CriticalSection);
    {$endif}
    Sheet := Bucket.FirstSheet;
    while Sheet <> nil do begin
      NextSheet := Sheet.NextSheet;
      VFreeSheet(Sheet);
      Sheet := NextSheet;
    end;
  end;
  {$ifndef SPINLOCK}
  for i := 0 to High(VBlockDescriptors) do begin
    VBlockDescriptor := @VBlockDescriptors[i];
    DeleteCriticalSection(VBlockDescriptor.CriticalSection);
  end;
  {$endif}
end;

function GetMemBlock(Bucket: PBucket): Pointer;
// get the first available block from the ActiveSheet of the Bucket corresponding to Size
var
  Sheet: PSheet;
begin
  {$ifdef SPINLOCK}
  if {$ifdef USEISMULTITHREAD}IsMultiThread and{$endif} (InterLockedCompareExchange(Bucket.SpinLock, 1, 0) <> 0) then begin
    if Bucket.BlockSize < MAX_BUCKET_MEM then begin
      inc(Bucket);
      Result := GetMemBlock(Bucket);
      Exit;
    end;
    Sleep(0);
    while InterLockedCompareExchange(Bucket.SpinLock, 1, 0) <> 0 do
      Sleep(1);
  end;
  {$else}{$ifdef USEISMULTITHREAD}if IsMultiThread then{$endif}
  EnterCriticalSection(Bucket.CriticalSection);
  {$endif}

  Sheet := Bucket.FirstFreeSheet;
  if Sheet <> nil then begin
    Result := PBlockUsed(Sheet.FirstFree);
    Sheet.FirstFree := PBlockFree(Result).NextBlock;
    if Sheet.FirstFree = nil then begin
      if Sheet.NextFreeSheet <> nil then
        Sheet.NextFreeSheet.PrevFreeSheet := nil;
      Bucket.FirstFreeSheet := Sheet.NextFreeSheet;
    end;
  end
  else begin
    if Bucket.CurAddr <= Bucket.MaxAddr then begin
      Sheet := Bucket.ActiveSheet;
      Result := PBlockUsed(Bucket.CurAddr);
      inc(Bucket.CurAddr, Bucket.BlockSize);
    end
    else begin
      Sheet := CreateSheet(Bucket);
      if Sheet = nil then begin
        Result := nil;
        {$ifdef SPINLOCK}
        Bucket.SpinLock := 0;
        {$else}{$ifdef USEISMULTITHREAD}if IsMultiThread then{$endif}
        LeaveCriticalSection(Bucket.CriticalSection);
        {$endif}
        Exit;
      end
      else begin
        Result := PBlockUsed(Bucket.CurAddr);
        inc(Bucket.CurAddr, Bucket.BlockSize);
      end;
    end;
  end;
  inc(Sheet.BlockUsedCount);
  PBlockUsed(Result).Sheet := Sheet;
  inc(Integer(Result), 4);
  {$ifdef SPINLOCK}
  Bucket.SpinLock := 0;
  {$else}{$ifdef USEISMULTITHREAD}if IsMultiThread then{$endif}
  LeaveCriticalSection(Bucket.CriticalSection);
  {$endif}
end;

function FreeMemBlock(Pntr: PBlockUsed): Integer;
// add the memory block to the Free blocks linked list of its Sheet
var
  Sheet: PSheet;
  Bucket: PBucket;
begin
  Sheet := Pntr.Sheet;
  {$ifdef SPINLOCK}
  if {$ifdef USEISMULTITHREAD}IsMultiThread and{$endif} (InterLockedCompareExchange(Sheet.Bucket.SpinLock, 1, 0) <> 0) then begin
    Sleep(0);
    while InterLockedCompareExchange(Sheet.Bucket.SpinLock, 1, 0) <> 0 do
      Sleep(1);
  end;
  {$else}{$ifdef USEISMULTITHREAD}if IsMultiThread then{$endif}
  EnterCriticalSection(Sheet.Bucket.CriticalSection);
  {$endif}
  Bucket := Sheet.Bucket;
  if Sheet.FirstFree = nil then begin
    if Bucket.FirstFreeSheet <> nil then
      Bucket.FirstFreeSheet.PrevFreeSheet := Sheet;
    Sheet.PrevFreeSheet := nil;
    Sheet.NextFreeSheet := Bucket.FirstFreeSheet;
    Bucket.FirstFreeSheet := Sheet;
  end;
  PBlockFree(Pntr).NextBlock := Sheet.FirstFree;
  Sheet.FirstFree := PBlockFree(Pntr);
  dec(Sheet.BlockUsedCount);
  if Sheet.BlockUsedCount = 0 then begin
    DestroySheet(Sheet);
  end;
  {$ifdef SPINLOCK}
  Bucket.SpinLock := 0;
  {$else}{$ifdef USEISMULTITHREAD}if IsMultiThread then{$endif}
  LeaveCriticalSection(Bucket.CriticalSection);
  {$endif}
  Result := 0;
end;

function BucketGetMem(Size: Integer): Pointer; forward;

function ReallocMemBlock(Pntr: PBlockUsed; Size, OldSize: Cardinal): Pointer;
// reallocate a memory block by creating a new one and freeing the old one
// with Buckets you cannot grow in place !
begin
  if Size > OldSize then begin
    // upsize...  anticipate future upsizes
    if Size <= 128 then
      Size := (Size + $3F) and $FFFFFFE0
    else if Size <= 256 then
      Size := (Size + $7F) and $FFFFFFC0
    else
      inc(Size, Size div 4);
  end;
  Result := BucketGetMem(Size - 4);
  if OldSize < Size then
    Size := OldSize;
  Size := (Size + $00000003) and $FFFFFFFC;  // round for Move operation
  FastCodeMove(Pointer(Integer(Pntr) + 4)^, Result^, Size - 4);
  FreeMemBlock(Pntr);
end;

function ReallocHeapBlock(Pntr: PHeapBlockUsed; Size, OldSize: Cardinal): Pointer;
// reallocate a Heap memory block by creating a new one and freeing the old one
begin
  if Size > OldSize then begin
    // upsize...  anticipate future upsizes if already resized at least EXACT_REALLOC_COUNT times
    if (Pntr.ReallocCount >= EXACT_REALLOC_COUNT) and (Size < 2 * OldSize) then
      inc(Size, Size div 2);
  end
  else begin
    if Pntr.ReallocCount < EXACT_REALLOC_COUNT then begin
      // possibly small downsize... check whether the original block would be fine
      if (Size > MAX_BUCKET_MEM) and ((Size + $0000FFFF) and $FFFF0000 = OldSize) then begin
        inc(Pntr.ReallocCount);
        Result := Pointer(Integer(Pntr) + 16);
        Exit;
      end;
    end;
  end;
  Result := BucketGetMem(Size - 16);
  if OldSize < Size then
    Size := OldSize;
  Size := (Size + $00000003) and $FFFFFFFC;  // round for Move operation
  if Integer(PBlockUsed(Integer(Result) - 4).Sheet) = MAGIC_BLOCK_HEAP_ALLOC then
    PHeapBlockUsed(Integer(Result) - 16).ReallocCount := Pntr.ReallocCount + 1;   // increment Realloc count
  FastCodeMove(Pointer(Integer(Pntr) + 16)^, Result^, Size - 16);
  {$ifdef USECACHEOFFSET}  // avoid Win98 VirtualFree problem
  Pntr := PHeapBlockUsed(Integer(Pntr) and $FFFF0000);
  {$endif}
  OSVirtualFree(Pntr);
end;


{$ifdef USECACHEOFFSET}
const
  CacheOffSetMask = $7C0;
  CacheOffSetInc = $5F0;
var
  CacheOffSet: integer;
{$endif}

function GetHeapBlock(Size: Integer): Pointer;
{$ifdef USECACHEOFFSET}
var
  OffSet: integer;
{$endif}
begin
  inc(Size, 12);
  {$ifdef USECACHEOFFSET}
  inc(CacheOffset, CacheOffSetInc);
  OffSet := CacheOffset and CacheOffSetMask;
  if (Size and $FFFF) + OffSet >= $10000 then
    OffSet := 0;
  Size := (Size + OffSet + $FFFF) and $FFFF0000;
  Result := OSVirtualAlloc(Size);
  inc(Integer(Result), OffSet);
  Dec(Size, OffSet);
  {$else}
  Size := (Size + $FFFF) and $FFFF0000;
  Result := OSVirtualAlloc(Size);
  {$endif}
  if Result <> nil then begin
    PHeapBlockUsed(Result).ReallocCount := 0;
    PHeapBlockUsed(Result).Size := Size;
    PHeapBlockUsed(Result).Sheet := PSheet(MAGIC_BLOCK_HEAP_ALLOC);
    inc(Integer(Result), 16);
  end;
end;


(********************************************************************)
// Global Memory Management Functions
(********************************************************************)

function BucketGetMem(Size: Integer): Pointer;
// Return nil = failure
begin
  inc(Size, 4);
  if Size <= MAX_BUCKET_MEM then begin
    // for blocks <= MAX_BUCKET_MEM use bucket
    Result := GetMemBlock(@Buckets[BucketIndexForSize[(Cardinal(Size) - 1) div 16]]);
  end
  else begin
    // for blocks > MAX_BUCKET_MEM use OS functions
    Result := GetHeapBlock(Size);
  end;
end;

function BucketFreeMem(Pntr: Pointer): Integer;
// Return 0 = success, 1 = failure
begin
  dec(Integer(Pntr), 4);
  if Integer(PBlockUsed(Pntr).Sheet) = MAGIC_BLOCK_HEAP_ALLOC then begin
    dec(Integer(Pntr), 12);
    {$ifdef USECACHEOFFSET}  // avoid Win98 VirtualFree problem
    Pntr := Pointer(Integer(Pntr) and $FFFF0000);
    {$endif}
    if OSVirtualFree(Pntr) then
      Result := 0
    else
      Result := 1;
  end
  else begin
    if PBlockUsed(Pntr).Sheet.Magic = MAGIC_SHEET then
      Result := FreeMemBlock(Pntr)
    else  // incorrect block
      Result := 1;
  end;
end;

function BucketReallocMem(Pntr: Pointer; Size: Integer): Pointer;
// Return nil = failure
var
  OldSize: Cardinal;
begin
  dec(Integer(Pntr), 4);
  if Integer(PBlockUsed(Pntr).Sheet) = MAGIC_BLOCK_HEAP_ALLOC then begin
    dec(Integer(Pntr), 12);
    inc(Size, 16);
    OldSize := PHeapBlockUsed(Pntr).Size;
    // only perform downsize when really worthwile (NewSize < OldSize div 4)
    // if resized less than EXACT_REALLOC_COUNT times always return the exact size
    if (PHeapBlockUsed(Pntr).ReallocCount >= EXACT_REALLOC_COUNT) and
       (Cardinal(Size) <= OldSize) and ((Cardinal(Size) >= OldSize div 4)) then
      Result := Pointer(Integer(Pntr) + 16)
    else
      Result := ReallocHeapBlock(Pntr, Size, OldSize);
  end
  else if PBlockUsed(Pntr).Sheet.Magic = MAGIC_SHEET then begin
    inc(Size, 4);
    OldSize := PBlockUsed(Pntr).Sheet.Bucket.BlockSize;
    // only perform downsize when really worthwile (OldSize > 128 bytes and NewSize < OldSize div 4)
    if (Cardinal(Size) <= OldSize) and ((OldSize <= 128) or (Cardinal(Size) >= OldSize div 4)) then
      Result := Pointer(Integer(Pntr) + 4)
    else
      Result := ReallocMemBlock(Pntr, Size, OldSize);
  end
  else  // incorrect block
    Result := nil;
end;


(********************************************************************)
// unit initialization functions
(********************************************************************)

const
  MemMgr: TMemoryManager =
  ( GetMem: BucketGetMem;
    FreeMem: BucketFreeMem;
    ReallocMem: BucketReallocMem );
var
  SysMemMgr: TMemoryManager;


procedure InitMemoryManager;
begin
  InitBuckets;
  GetMemoryManager(SysMemMgr);
  SetMemoryManager(MemMgr);
  {$ifdef USESYSTEMMOVE}
  FastCodeMove := System.Move;   // will be patched by FastMove
  {$else}
  FastCodeMove := Move_IA32;
  {$endif}
end;

procedure FinishMemoryManager;
begin
  SetMemoryManager(SysMemMgr);
  FinishBuckets;
end;

initialization
  InitMemoryManager;

finalization
  FinishMemoryManager;

end.
