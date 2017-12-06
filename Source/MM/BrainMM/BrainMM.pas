unit BrainMM;

{******************************************************************************}
{ Copyright (c) 2016 Dmitry Mozulyov                                           }
{                                                                              }
{ Permission is hereby granted, free of charge, to any person obtaining a copy }
{ of this software and associated documentation files (the "Software"), to deal}
{ in the Software without restriction, including without limitation the rights }
{ to use, copy, modify, merge, publish, distribute, sublicense, and/or sell    }
{ copies of the Software, and to permit persons to whom the Software is        }
{ furnished to do so, subject to the following conditions:                     }
{                                                                              }
{ The above copyright notice and this permission notice shall be included in   }
{ all copies or substantial portions of the Software.                          }
{                                                                              }
{ THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR   }
{ IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,     }
{ FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE  }
{ AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER       }
{ LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,}
{ OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN    }
{ THE SOFTWARE.                                                                }
{                                                                              }
{ email: softforyou@inbox.ru                                                   }
{ skype: dimandevil                                                            }
{ repository: https://github.com/d-mozulyov/BrainMM                            }
{******************************************************************************}


// compiler directives
{$ifdef FPC}
  {$mode delphi}
  {$asmmode intel}
  {$define INLINESUPPORT}
  {$define INLINESUPPORTSIMPLE}
  {$ifdef CPU386}
    {$define CPUX86}
  {$endif}
  {$ifdef CPUX86_64}
    {$define CPUX64}
  {$endif}
{$else}
  {$ifdef CONDITIONALEXPRESSIONS}
    {$if CompilerVersion >= 24}
      {$LEGACYIFEND ON}
    {$ifend}
    {$if CompilerVersion >= 15}
      {$WARN UNSAFE_CODE OFF}
      {$WARN UNSAFE_TYPE OFF}
      {$WARN UNSAFE_CAST OFF}
    {$ifend}
    {$if CompilerVersion >= 20}
      {$define INLINESUPPORT}
    {$ifend}
    {$if CompilerVersion >= 17}
      {$define INLINESUPPORTSIMPLE}
    {$ifend}
    {$if CompilerVersion >= 18}
      {$define MEMORYMANAGEREX}
    {$ifend}
    {$if CompilerVersion < 23}
      {$define CPUX86}
    {$ifend}
    {$if CompilerVersion >= 23}
      {$define UNITSCOPENAMES}
      {$define RETURNADDRESS}
    {$ifend}
    {$if CompilerVersion >= 21}
      {$WEAKLINKRTTI ON}
      {$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
    {$ifend}
    {$if (not Defined(NEXTGEN)) and (CompilerVersion >= 20)}
      {$define INTERNALCODEPAGE}
    {$ifend}
  {$else}
    {$define CPUX86}
  {$endif}
{$endif}
{$U-}{$V+}{$B-}{$X+}{$T+}{$P+}{$H+}{$J-}{$Z1}
{$ifdef CONDITIONALEXPRESSIONS}{$A4}{$else}{$A+}{$endif}
{$O+}{$R-}{$I-}{$Q-}{$W-}
{$ifdef CONDITIONALEXPRESSIONS}
  {$if Defined(CPUX86) or Defined(CPUX64)}
    {$define CPUINTEL}
  {$ifend}
  {$if Defined(CPUX64) or Defined(CPUARM64)}
    {$define LARGEINT}
  {$ifend}
  {$if (not Defined(CPUX64)) and (not Defined(CPUARM64))}
    {$define SMALLINT}
  {$ifend}
  {$if Defined(FPC) or (CompilerVersion >= 18)}
    {$define OPERATORSUPPORT}
  {$ifend}
  {$ifdef MSWINDOWS}
    {$SETPEFLAGS $20}
  {$endif}
{$else}
  {$define CPUINTEL}
  {$define SMALLINT}
  {$define MSWINDOWS}
{$endif}
{$ifdef KOL_MCK}
  {$define KOL}
{$endif}

{$ifNdef MSWINDOWS}
  {$define PUREPASCAL}
{$endif}

{$ifdef BRAINMM_NOREDIRECT}
  {$undef BRAINMM_REDIRECT}
{$else}
  {$ifNdef BRAINMM_REDIRECT}
    {$ifNdef DEBUG}
      {$define BRAINMM_REDIRECT}
    {$endif}
  {$endif}
  {$ifdef PUREPASCAL}
    {$undef BRAINMM_REDIRECT}
  {$endif}
  {$ifNdef CPUINTEL}
    {$undef BRAINMM_REDIRECT}
  {$endif}
  {$ifNdef BRAINMM_REDIRECT}
    {$define BRAINMM_NOREDIRECT}
  {$endif}
{$endif}

{$ifdef BRAINMM_NOFIXFINALIZES}
  {$undef BRAINMM_FIXFINALIZES}
{$else}
  {$ifdef CONDITIONALEXPRESSIONS}
    {$if (not Defined(FPC)) and (CompilerVersion >= 23) and Defined(MSWINDOWS)}
      {$define BRAINMM_FIXFINALIZES}
    {$ifend}
  {$endif}
{$endif}

{$ifdef CONDITIONALEXPRESSIONS}
  {$if Defined(FPC) or (CompilerVersion < 18)}
    {$define MANAGERFLAGSEMULATE}
  {$ifend}
{$else}
  {$define MANAGERFLAGSEMULATE}
{$endif}

interface
  uses {$ifdef CONDITIONALEXPRESSIONS}
         {$ifdef UNITSCOPENAMES}System.Types{$else}Types{$endif},
         {$ifdef MSWINDOWS}{$ifdef UNITSCOPENAMES}Winapi.Windows{$else}Windows{$endif}{$endif}
         {$ifdef POSIX}Posix.Base, Posix.String_, Posix.Unistd, Posix.SysTypes, Posix.PThread{$endif};
       {$else}
         Windows;
       {$endif}

type
  // standard types
  {$ifdef CONDITIONALEXPRESSIONS}
    {$ifdef FPC}
      PUInt64 = ^UInt64;
    {$else}
      {$if CompilerVersion < 16}
        UInt64 = Int64;
        PUInt64 = ^UInt64;
      {$ifend}
      {$if CompilerVersion < 21}
        NativeInt = Integer;
        NativeUInt = Cardinal;
      {$ifend}
      {$if CompilerVersion < 22}
        PNativeInt = ^NativeInt;
        PNativeUInt = ^NativeUInt;
      {$ifend}
    {$endif}
  {$else}
    PBoolean = ^Boolean;
    PPointer = ^Pointer;  
    UInt64 = Int64;
    PUInt64 = ^UInt64;
    NativeInt = Integer;
    NativeUInt = Cardinal;
    PNativeInt = ^NativeInt;
    PNativeUInt = ^NativeUInt;
  {$endif}


  (* Extended memory API *)

  TMemoryAlign = (ma16Bytes, ma32Bytes, ma64Bytes, ma128Bytes, ma256Bytes,
    ma512Bytes, ma1024Bytes, ma2048Bytes);
  PMemoryAlign = ^TMemoryAlign;

  MemoryBlock = type Pointer;
  PMemoryBlock = ^MemoryBlock;

  TMemoryBlockSize = (BLOCK_4K, BLOCK_16K, BLOCK_64K, BLOCK_256K, BLOCK_1MB,
    BLOCK_4MB, BLOCK_16MB, BLOCK_64MB, BLOCK_256MB);
  PMemoryBlockSize = ^TMemoryBlockSize;

  MemoryPages = type Pointer;
  PMemoryPages = ^MemoryPages;

  TMemoryAccessRight = (marRead, marWrite, marExecute);
  PMemoryAccessRight = ^TMemoryAccessRight;
  TMemoryAccessRights = set of TMemoryAccessRight;
  PMemoryAccessRights = ^TMemoryAccessRights;

  TMemoryKind = (mkSmall, mkMedium, mkBig, mkLarge, mkPages, mkBlock, mkJIT);
  PMemoryKind = ^TMemoryKind;

  TMemoryOptions = packed record
    Kind: TMemoryKind;
    Align: TMemoryAlign;
    BlockSize: TMemoryBlockSize;
    AccessRights: TMemoryAccessRights;
    ThreadId: NativeUInt;
    Size: NativeUInt;
  end;
  PMemoryOptions = ^TMemoryOptions;

  // additional memory functions
  procedure GetMemAligned(var P: Pointer; Align: TMemoryAlign; Size: NativeInt);
  procedure RegetMem(var P: Pointer; NewSize: NativeInt);
  {$ifNdef MEMORYMANAGEREX}
  function AllocMem(Size: NativeInt): Pointer;
  {$endif}

  // block routine
  procedure GetMemoryBlock(var Block: MemoryBlock; BlockSize: TMemoryBlockSize);
  procedure FreeMemoryBlock(Block: MemoryBlock);

  // 4kb-pages routine
  procedure GetMemoryPages(var Pages: MemoryPages; Count: NativeInt);
  procedure RegetMemoryPages(var Pages: MemoryPages; NewCount: NativeInt);
  procedure ReallocMemoryPages(var Pages: MemoryPages; NewCount: NativeInt);
  procedure FreeMemoryPages(Pages: MemoryPages);

  // any 4kb-aligned memory
  procedure ChangeMemoryAccessRights(Pages: MemoryPages; Count: NativeInt; Rights: TMemoryAccessRights);
  function GetMemoryAccessRights(Pages: MemoryPages): TMemoryAccessRights;

  // low level routine
  function GetMemoryOptions(const P: Pointer; var Options: TMemoryOptions): Boolean;
  function ThreadHeapMinimize: Boolean;

  // fast SSE-based 16-aligned memory move
  procedure MoveB16Aligned(const Source; var Dest; const B16Count: NativeInt); {$ifNdef CPUINTEL}inline;{$endif}


type

{ TJITHeap class }
{ Just-In-Time memory heap: READ | WRITE | EXECUTE }

  IJITHeap = interface
    procedure Clear; stdcall;
    function GetMemory(Size: NativeInt): Pointer; stdcall;
    procedure FreeMemory(P: Pointer); stdcall;
    function SyncGetMemory(Size: NativeInt): Pointer; stdcall;
    procedure SyncFreeMemory(P: Pointer); stdcall;
  end;

  TJITHeap = class(TInterfacedObject, IJITHeap)
  protected
    FSpin: NativeUInt;
    FHeapBuffer: array[0..64 * {$ifdef LARGEINT}7{$else}4{$endif} - 1 + 64] of Byte;
    FBigOrLargeHash: array[0..63] of Pointer;

    function HeapInstance: Pointer; {$ifdef INLINESUPPORT}inline;{$endif}
    procedure EnqueueBigOrLarge(Pages: MemoryPages);
    function DequeueBigOrLarge(Pages: MemoryPages): Boolean;
  {$ifNdef AUTOREFCOUNT}
  public
  {$endif}
    destructor Destroy; override;
  public
    constructor Create;
    procedure Clear; stdcall;

    // memory management
    function GetMemory(Size: NativeInt): Pointer; stdcall;
    procedure FreeMemory(P: Pointer); stdcall;

    // synchronization (spin lock) + memory management
    function SyncGetMemory(Size: NativeInt): Pointer; stdcall;
    procedure SyncFreeMemory(P: Pointer); stdcall;
  end;


{ Winapi.ActiveX.IMalloc implementation (TMalloc class)
  Unfortunately standard IMalloc interface can not be used, because
  adding Winapi.ActiveX module leads to automatic linking System.Variants

  To use IMalloc interface, call the following:
    TMalloc.Create as IMalloc; }

  _IMalloc = interface
    ['{00000002-0000-0000-C000-000000000046}']
    function Alloc(cb: Longint): Pointer; stdcall;
    function Realloc(pv: Pointer; cb: Longint): Pointer; stdcall;
    procedure Free(pv: Pointer); stdcall;
    function GetSize(pv: Pointer): Longint; stdcall;
    function DidAlloc(pv: Pointer): Integer; stdcall;
    procedure HeapMinimize; stdcall;
  end;

  TMalloc = class(TInterfacedObject, _IMalloc)
    function Alloc(cb: Longint): Pointer; stdcall;
    function Realloc(pv: Pointer; cb: Longint): Pointer; stdcall;
    procedure Free(pv: Pointer); stdcall;
    function GetSize(pv: Pointer): Longint; stdcall;
    function DidAlloc(pv: Pointer): Integer; stdcall;
    procedure HeapMinimize; stdcall;
  end;


{$ifdef MANAGERFLAGSEMULATE}
var
  ReportMemoryLeaksOnShutdown: Boolean;
  NeverSleepOnMMThreadContention: Boolean;
{$endif}

{$ifNdef BRAINMM_UNITTEST}
implementation
{$endif}


 (* Header types and contants *)

const
  SIZE_16 = 16;
  MASK_16_CLEAR = -SIZE_16;
  MASK_16_TEST = SIZE_16 - 1;
  SIZE_64 = 64;
  MASK_64_CLEAR = -SIZE_64;
  MASK_64_TEST = SIZE_64 - 1;
  SIZE_K1 = 1024;
  MASK_K1_CLEAR = -SIZE_K1;
  MASK_K1_TEST = SIZE_K1 - 1;
  SIZE_K4 = 4 * 1024;
  MASK_K4_CLEAR = -SIZE_K4;
  MASK_K4_TEST = SIZE_K4 - 1;
  SIZE_K16 = 16 * 1024;
  MASK_K16_CLEAR = -SIZE_K16;
  MASK_K16_TEST = SIZE_K16 - 1;
  SIZE_K32 = 32 * 1024;
  MASK_K32_CLEAR = -SIZE_K32;
  MASK_K32_TEST = SIZE_K32 - 1;
  SIZE_K64 = 64 * 1024;
  MASK_K64_CLEAR = -SIZE_K64;
  MASK_K64_TEST = SIZE_K64 - 1;

  B16_PER_PAGE = SIZE_K4 div SIZE_16;
  B16_PER_PAGE_SHIFT = 12 - 4;

  MAX_SMALL_SIZE = 128;
  MAX_SMALL_B16COUNT = MAX_SMALL_SIZE div 16;
  MIDDLE_MEDIUM_SIZE = 6 * SIZE_K1;
  MIDDLE_MEDIUM_B16COUNT = MIDDLE_MEDIUM_SIZE div 16;
  MAX_MEDIUM_INTERNAL_GROW_LIMIT = 32;
  MAX_MEDIUM_SIZE = (32 * SIZE_K1) - 16 - MAX_MEDIUM_INTERNAL_GROW_LIMIT;
  MAX_MEDIUM_B16COUNT = MAX_MEDIUM_SIZE div 16;
  MAX_BIG_SIZE = 8 * SIZE_K64;
  MAX_BIG_B16COUNT = MAX_BIG_SIZE div 16;

  PAGESMODE_USER = 0;
  PAGESMODE_SYSTEM = 1;
  PAGESMODE_JIT = 2;

  THREAD_HEAP_LOCKABLE = 2;
  THREAD_HEAP_LOCKED_BIT = 1;
  THREAD_HEAP_LOCKED = THREAD_HEAP_LOCKABLE or THREAD_HEAP_LOCKED_BIT;

  PTR_INVALID = Pointer(1);
  FREEMEM_INVALID = Integer({$ifdef FPC}0{$else}-1{$endif});
  FREEMEM_DONE = Integer({$ifdef FPC}-1{$else}0{$endif});
  JITHEAP_MARKER = {PThreadHeap}Pointer(NativeInt(-1));

  {$ifdef LARGEINT}
    HIGH_NATIVE_BIT = 63;
  {$else}
    HIGH_NATIVE_BIT = 31;
  {$endif}
  MASK_HIGH_NATIVE_BIT = NativeInt(1) shl HIGH_NATIVE_BIT;
  MASK_HIGH_NATIVE_TEST = NativeInt(-1) shr 1;

  DEFAULT_BITSETS_SMALL: array[0..15] of Int64 = (
    {16}   Int64($FFFFFFFFFFFFFFFC),
          {32}   $5555555555555554,
          {48}   $2492492492492490,
          {64}   $1111111111111110,
          {80}   $0842108421084210,
          {96}   $0410410410410410,
          {112}  $0204081020408100,
          {128}  $0101010101010100,
    {16-}  Int64($FFFFFFFFFFFFFFF0),
          {32-}  $5555555555555550,
          {48-}  $2492492492492490,
          {64-}  $1111111111111110,
          {80-}  $0842108421084210,
          {96-}  $0410410410410410,
          {112-} $0204081020408100,
          {128-} $0101010101010100
  );

  FIRST_BITSETS_SMALL: array[0..15] of Cardinal = (
  (*
      BitMask: Word;
      PtrOffset: Word(Byte);
  *)
      $00200004,
      $00200004,
      $00400010,
      $00400010,
      $00400010,
      $00400010,
      $00800100,
      $00800100,
      $00400010,
      $00400010,
      $00400010,
      $00400010,
      $00400010,
      $00400010,
      $00800100,
      $00800100
  );

  OFFSET_MEDIUM_ALIGN = 24;
  MASK_MEDIUM_ALIGN = {7}NativeUInt(High(TMemoryAlign)) shl OFFSET_MEDIUM_ALIGN;
  MASK_MEDIUM_ALLOCATED = NativeUInt(1 shl 16);

  MASK_MEDIUM_EMPTY_TEST = not NativeUInt(NativeInt(High(Word)) and -16);
  MASK_MEDIUM_EMPTY_VALUE = 0;
  MASK_MEDIUM_ALLOCATED_TEST = not NativeUInt((MAX_MEDIUM_SIZE + MAX_MEDIUM_INTERNAL_GROW_LIMIT) or MASK_MEDIUM_ALIGN);
  MASK_MEDIUM_ALLOCATED_VALUE = MASK_MEDIUM_ALLOCATED;

  FLAG_MEDIUM_COPY_SHIFT = 16;
  FLAG_MEDIUM_COPY = NativeUInt(1 shl FLAG_MEDIUM_COPY_SHIFT);
  FULL_MEDIUM_EMPTY_SIZE = SIZE_K64 - 48{internal usage} - 2 * 16{start/finish};

  RESIZE_PAGES_COPY_SHIFT = 1;
  RESIZE_PAGES_COPY = 1 shl RESIZE_PAGES_COPY_SHIFT;
  RESIZE_PAGES_USER_REGET = 0 + PAGESMODE_USER;
  RESIZE_PAGES_USER_REALLOC = RESIZE_PAGES_COPY + PAGESMODE_USER;
  RESIZE_PAGES_SYSTEM_REGET = 0 + PAGESMODE_SYSTEM;
  RESIZE_PAGES_SYSTEM_REALLOC = RESIZE_PAGES_COPY + PAGESMODE_SYSTEM;

var
  (*
     -1 shl 0,
     -1 shl 1,
     -1 shl 2,
     -1 shl 3,
     -1 shl 4,
     -1 shl 5,
     -1 shl 6,
     -1 shl 7,
     -1 shl 8,
     -1 shl 9,
     -1 shl 10,
     -1 shl 11,
     -1 shl 12,
     -1 shl 13,
     -1 shl 14,
     -1 shl 15,
     -1 shl 16,
     -1 shl 17,
     -1 shl 18,
     -1 shl 19,
     -1 shl 20,
     -1 shl 21,
     -1 shl 22,
     -1 shl 23,
     -1 shl 24,
     -1 shl 25,
     -1 shl 26,
     -1 shl 27,
     -1 shl 28,
     -1 shl 29,
     -1 shl 30,
     -1 shl 31
  *)
  MEDIUM_MASKS_CLEAR: array[0-1..31-1] of NativeInt{Cardinal};

  (*
      0: 16..48 - 1
      1: 48..64 - 1
      2: 64..80 - 1
      3: 80..96 - 1
      4: 96..128 - 1
      5: 128..160 - 1
      6: 160..192 - 1
      7: 192..224 - 1
      8: 224..272 - 1
      9: 272..320 - 1
     10: 320..384 - 1
     11: 384..448 - 1
     12: 448..512 - 1
     13: 512..576 - 1
     14: 576..672 - 1
     15: 672..800 - 1
     16: 800..1024 - 1
     17: 1024..1360 - 1
     18: 1360..1696 - 1
     19: 1696..2144 - 1
     20: 2144..2592 - 1
     21: 2592..3040 - 1
     22: 3040..3712 - 1
     23: 3712..4384 - 1
     24: 4384..5056 - 1
     25: 5056..5728 - 1
     26: 5728..6144 - 1

     27: 6144..8192 - 1
     28: 8192..12288 - 1
     29: 12288..16384 - 1
     30: 16384..24576 - 1
     31: 24576..65536 - 1
  *)
  MEDIUM_INDEXES: array[0..64 * 1024 shr 4 - 1] of ShortInt;

type
  PThreadHeap = ^TThreadHeap;
  SupposedPtr = type NativeUInt;
  PSupposedPtr = ^SupposedPtr;

  TBitSet8 = packed record
  case Integer of
    0: (V64: Int64);
    1: (VLow32, VHigh32: Integer);
    2: (VIntegers: array[0..1] of Integer);
    3: (VBytes: array[0..7] of Byte);
  end;
  PBitSet8 = ^TBitSet8;

  B16 = array[0..15] of Byte;
  P16 = ^B16;

  TQueuePrevNext = packed record
    Prev: Pointer;
    Next: Pointer;
    {$ifdef SMALLINT}_Padding: array[0..1] of Cardinal;{$endif}
  end;
  PQueuePrevNext = ^TQueuePrevNext;

  TK1LineSmallHeader = packed record
    ItemSet: TBitSet8;
    (*
       FullQueue: Boolean:1;
       Reserved: Boolean:1;
    *)
    case Integer of
      0: (_Padding: Int64; Queue: TQueuePrevNext);
      1: (ModeSize{+0..15}: Byte; Reserved: Word; {FirstLine}InQK64PoolSmallFull: Boolean);
      2: (Flags: NativeUInt);
      (*
         Index: 0..15:3+1{FirstLine};
         ModeSizeBits: Byte:4;
         Reserved: Word;
         InQK64PoolSmallFull: Boolean;
      *)
  end;
  PK1LineSmallHeader = ^TK1LineSmallHeader;

  TK1LineSmall = packed record
  case Integer of
    0: (Header: TK1LineSmallHeader);
    1: (Items: array[0{2}..63] of B16);
  end;
  PK1LineSmall = ^TK1LineSmall;

  TK64PoolSmall = packed record
  case Boolean of
    True: (
             Header: TK1LineSmallHeader;
             ThreadHeap: PThreadHeap;
             {$ifdef SMALLINT}_Padding1: Cardinal;{$endif}
             LineSet: TBitSet8;
             Queue: TQueuePrevNext;
          );
   False: (Lines: array[0..63] of TK1LineSmall);
  end;
  PK64PoolSmall = ^TK64PoolSmall;

  THeaderMedium = packed record
    PreviousSize: NativeUInt;
    {$ifdef SMALLINT}_Padding: array[0..1] of Cardinal;{$endif}
    case Integer of
     0: (WSize: Word; Allocated: Boolean; Align: TMemoryAlign);
     1: (Flags: NativeUInt);
  end;
  PHeaderMedium = ^THeaderMedium;

  THeaderMediumList = array[0..(16{start} + FULL_MEDIUM_EMPTY_SIZE + 16{finish}) div 16 - 1] of THeaderMedium;
  PHeaderMediumList = ^THeaderMediumList;

  PHeaderMediumEmpty = ^THeaderMediumEmpty;
  THeaderMediumEmpty = packed record
    Prev, Next: PHeaderMediumEmpty;
    {$ifdef SMALLINT}_Padding: array[0..1] of Cardinal;{$endif}
  case Integer of
    0: (Size: NativeUInt{Next.Header.PreviousSize});
    1: (SiblingHeaderMedium: THeaderMedium; SiblingPtr: Byte);
  end;

  TK64PoolMedium = packed record
    Queue: TQueuePrevNext;
    Reserved: Pointer;
    {$ifdef SMALLINT}_Padding1: Cardinal;{$endif}
    ThreadHeap: PThreadHeap;
    {$ifdef SMALLINT}_Padding2: Cardinal;{$endif}
    MarkerNil: Pointer;
    {$ifdef SMALLINT}_Padding3: Cardinal;{$endif}
    {$ifdef SMALLINT}_Padding4: Cardinal;{$endif}
    FlagsFakeAllocated: NativeUInt;
    Items: THeaderMediumList;
  end;
  PK64PoolMedium = ^TK64PoolMedium;

  TSyncStack = object
  private
    F: packed record
    case Integer of
      0: (Handle: SupposedPtr; x86RefCounter: NativeUInt);
      1: (Assigned4Bytes: LongBool);
    end;
    {$ifNdef SMALLINT}
    function GetAssigned: Boolean; inline;
    {$endif}
  public
    procedure Push(const Value: Pointer); {$ifdef INLINESUPPORT}inline;{$endif}
    function Pop: Pointer;

    procedure PushList(const First, Last: Pointer);
    function PopList: Pointer;

    {$ifdef SMALLINT}
    property Assigned: LongBool read F.Assigned4Bytes;
    {$else .LARGEINT}
    property Assigned: Boolean read GetAssigned;
    {$endif}
  end;
  PSyncStack = ^TSyncStack;

  TSyncStack64 = object(TSyncStack)
  protected
    _Padding: array[1..64 - SizeOf(TSyncStack)] of Byte;
  end;
  PSyncStack64 = ^TSyncStack64;

  TMediumManager = {$ifdef OPERATORSUPPORT}packed record{$else}object{$endif}
  public
    // errors redirect
    function ErrorOutOfMemory: Pointer;
    function ErrorInvalidPtr: Integer;
    function RaiseOutOfMemory: Pointer;
    function RaiseInvalidPtr: Integer;
  public
    // penalty routine
    function ExcludeEmpty(LastIndex: NativeUInt; Empty: PHeaderMediumEmpty): Pointer;
    function ChangeEmptyIndex(Flags: NativeUInt{LastIndex, NewIndex:5}; Empty: PHeaderMediumEmpty): Pointer;
    function AlignOffsetEmpty(Header: PHeaderMedium; AlignOffset: NativeUInt): Pointer;
    function GrowPenalty(P: Pointer; GrowFlags: NativeUInt{NewSize: Word; shifted Copy: Boolean}): Pointer;
    function FreeDisposePenalty(Header: PHeaderMedium; FlagIsTop: Boolean): Integer;
  public
    FEmpties: array[0..31] of PHeaderMediumEmpty;
    FMask: NativeInt;
    QK64PoolMedium: PK64PoolMedium;

    function Get(B16Count: NativeUInt): Pointer;
    function GetAdvanced(B16Count: NativeUInt; Align: NativeUInt{TMemoryAlign}): Pointer;
    function Reduce(P: Pointer; NewB16Count: NativeUInt{Word}): Pointer;
    function Grow(P: Pointer; GrowFlags: NativeUInt{NewB16Count: Word; Copy: Boolean}): Pointer;
    function Free(P: Pointer): Integer;
  end;
  PMediumManager = ^TMediumManager;

  TThreadHeap = {$ifdef OPERATORSUPPORT}packed record{$else}object{$endif}
  public
    FNextHeap: PThreadHeap;
    FK1LineSmalls: array[1..8] of PK1LineSmall;
    FMarkerNotSelf: SupposedPtr;

  public
    // 1kb-lines (small) + 64kb pool (small) routine
    QK1LineFull: PK1LineSmall;
    QK64PoolSmall: PK64PoolSmall;
    QK64PoolSmallFull: PK64PoolSmall;

    function NewK64PoolSmall: PK64PoolSmall;
    function DisposeK64PoolSmall(PoolSmall: PK64PoolSmall): Integer;
    function GetNewK1LineSmall(B16Count: NativeUInt): Pointer;
    function DisposeK1LineSmall(Line: PK1LineSmall): Integer;
    {$ifNdef PUREPASCAL}function PenaltyGrowSmallToSmall(P: Pointer; Dest: Pointer): Pointer;{$endif}
  public
    // 64kb medium pieces management
    FMedium: TMediumManager;

    function NewK64PoolMedium: PK64PoolMedium;
    function DisposeK64PoolMedium(PoolMedium: PK64PoolMedium): Integer;
  public
    // errors
    ErrorAddr: Pointer {address/nil};

    function ErrorOutOfMemory: Pointer;
    function ErrorInvalidPtr: Integer;
    function RaiseOutOfMemory: Pointer;
    function RaiseInvalidPtr: Integer;
  public
    // thread deffered (free) memory pieces: small or medium
    Deferreds: TSyncStack;

    procedure PushThreadDeferred(P: Pointer; ReturnAddress: Pointer; IsSmall: Boolean);
    procedure ProcessThreadDeferred;
  public
    // additional information
    ThreadId: NativeUInt;
    LockFlags: SupposedPtr;
    _Padding: array[1..64 - SizeOf(TSyncStack) - SizeOf(NativeUInt) - SizeOf(SupposedPtr)] of Byte;
  public
    // most frequently used
    function GetSmall(B16Count: NativeUInt): Pointer;
    function FreeSmall(P: Pointer): Integer;
    function RegrowSmallToSmall(P: Pointer; NewB16Count: NativeUInt): Pointer;
    function GrowSmallToSmall(P: Pointer; NewB16Count: NativeUInt): Pointer;

    // difficult routine
    function FreeDifficult(P: Pointer; ReturnAddress: Pointer): Integer;
    function ResizeDifficult(PFlags: NativeInt{Copy: Boolean; P: Pointer}; NewB16Count: NativeUInt; ReturnAddress: Pointer): Pointer;
  end;


 (* Extended memory API *)

type
  TBrainMMOptions = packed record
    RecordSize: Integer;
    Version: Integer;
    // version = 1
    FreePacalCompiler: Boolean;
    PurePascal: Boolean;
    Redirect: Boolean;
    ReportMemoryLeaksOnShutdown: PBoolean;
  end;
  PBrainMMOptions = ^TBrainMMOptions;

  TBrainMemoryManager = packed record
    BrainMM: packed record
      Options: PBrainMMOptions;
      {$ifdef MSWINDOWS}
      ThreadFuncEvent: function(ThreadFunc: TThreadFunc; Parameter: Pointer): Pointer;
      {$else .POSIX}
      ThreadFuncEvent: function(Attribute: PThreadAttr; ThreadFunc: TThreadFunc; Parameter: Pointer; var ThreadId: NativeUInt): Integer;
      {$endif}
      EndThreadEvent: procedure(ExitCode: Integer);
      GetMemoryBlock: function(BlockSize: TMemoryBlockSize; PagesMode: NativeUInt): MemoryBlock;
      FreeMemoryBlock: function(Block: MemoryBlock; PagesMode: NativeUInt): Boolean;
      GetMemoryPages: function(Count: NativeUInt; PagesMode: NativeUInt): MemoryPages;
      FreeMemoryPages: function(Pages: MemoryPages; PagesMode: NativeUInt): Boolean;
      ResizeMemoryPages: function(Pages: MemoryPages; NewCount: NativeUInt; ResizePagesFlags: NativeUInt): MemoryPages;
      Reserved: array[1..2] of Pointer;
      GetMemoryOptions: function(const P: Pointer; var Options: TMemoryOptions): Boolean;
      ThreadHeapMinimize: function: Boolean;
      GetMemAligned: function(Align: TMemoryAlign; Size: NativeUInt): Pointer;
      RegetMem: function(P: Pointer; NewSize: NativeUInt): Pointer;
    end;
    Standard: packed record
      GetMem: function(Size: NativeUInt): Pointer;
      FreeMem: function(P: Pointer): Integer;
      ReallocMem: function(P: Pointer; NewSize: NativeUInt): Pointer;
      AllocMem: function(Size: NativeUInt): Pointer;
      RegisterExpectedMemoryLeak: function(P: Pointer): Boolean;
      UnregisterExpectedMemoryLeak: function(P: Pointer): Boolean;
      Reserved: array[1..4] of Pointer;
    end;
  end;

  PMbPoolBigMask = ^TMbPoolBigMask;
  TMbPoolBigMask = packed record
  case Integer of
    0: (Bytes: array[0..31] of Byte);
    1: (Integers: array[0..7] of Integer);
    2: (Int64s: array[0..4] of Int64);
  end;

  PMbPoolBig = ^TMbPoolBig;
  TMbPoolBig = packed record
    Pages: PMemoryPages;
    Prev, Next: PMbPoolBig;
    Mask: PMbPoolBigMask;
  end;

  PPartLarge = ^TPartLarge;
  TPartLarge = packed record
    Prev: PPartLarge;
    Pages: PMemoryPages;
    ReservedPagesCount: NativeUInt;
    CommitedPagesCount: NativeUInt;
  end;

  // internal memory: 4 natives/pointers
  PInternalRecord = ^TInternalRecord;
  TInternalRecord = packed record
    Next: PInternalRecord;
    case Integer of
      0: (SupposedPages: SupposedPtr;
          (*
             PagesMode: Byte:2;
             IsBlock: Boolean:1;
             IsLarge: Boolean:1;
             Align: TMemoryAlign:3;
             Reserved: Byte:5;
             Pages: MemoryPages:High(Pointer)-12;
          *)
          PagesCount: NativeUInt;
          case Integer of
            0: (BigPool: PMbPoolBig);
            1: (TopPart: PPartLarge);
          );
      1: (ThreadHeap: PThreadHeap);
      2: (K64Block: MemoryBlock);
  end;

  // 64 bytes aligned global storage
  TGlobalStorage = object
  public
    PagesItems: array[0..1023] of SupposedPtr;

    CoreThreadHeaps: TSyncStack64;
    CoreInternalRecords: TSyncStack64;

    HeapsDeferred: TSyncStack;
    ThreadsCount: NativeUInt;
    PagesAllocated: NativeUInt;
    _Padding: array[1..64 - SizeOf(TSyncStack) - 2 * SizeOf(NativeUInt)] of Byte;

    K64BlockCache: packed record
      Stack: TSyncStack;
      Count: NativeUInt;
      _Padding: array[1..64 - SizeOf(TSyncStack) - SizeOf(NativeUInt)] of Byte;
    end;
  private
    {class} function GrowThreadHeaps: PThreadHeap;
    {class} function GrowInternalRecords: PInternalRecord;
  public
    {class} function InternalRecordPop: PInternalRecord; {$ifdef INLINESUPPORT}inline;{$endif}
    {class} procedure InternalRecordPush(const InternalRecord: PInternalRecord); {$ifdef INLINESUPPORT}inline;{$endif}

    {class} function K64BlockCachePop: MemoryBlock;
    {class} function K64BlockCachePush(const K64Block: MemoryBlock): Boolean;

    {class} function ExcludePagesRecord(const SupposedPages: SupposedPtr): PInternalRecord;
    {class} procedure IncludePagesRecord(const PagesRecord: PInternalRecord);
  end;
  PGlobalStorage = ^TGlobalStorage;


var
  MemoryManager: TBrainMemoryManager;

  UnknownThreadHeap: PThreadHeap;
  MainThreadHeap: PThreadHeap;
  ThreadHeapList: PThreadHeap;
  GLOBAL_STORAGE: array[0..SizeOf(TGlobalStorage) + 63] of Byte;

{$ifdef BRAINMM_UNITTEST}
  function CurrentThreadHeap: PThreadHeap;
implementation
{$endif}


procedure GetMemAligned(var P: Pointer; Align: TMemoryAlign; Size: NativeInt);
var
  Value: Pointer;
begin
  if (Byte(Align) <= Byte(High(TMemoryAlign))) then
  begin
    if (Size > 0) then
    begin
      Value := MemoryManager.BrainMM.GetMemAligned(Align, NativeUInt(Size));
      P := Value;
      if (Value = nil) then
        {$ifdef CONDITIONALEXPRESSIONS}System.Error(reOutOfMemory){$else}System.RunError(203){$endif};
    end else
    begin
      P := nil;
    end;
  end else
  begin
    {$ifdef CONDITIONALEXPRESSIONS}System.Error(reInvalidCast){$else}System.RunError(219){$endif};
  end;
end;

function RecallGetMem(None: Pointer; Size: NativeInt; var P: Pointer): Pointer;
begin
  if (Size > 0) then
  begin
    Result := MemoryManager.Standard.GetMem(NativeUInt(Size));
    if (Result = nil) then
      {$ifdef CONDITIONALEXPRESSIONS}System.Error(reOutOfMemory){$else}System.RunError(203){$endif};
  end else
  begin
    Result := nil;
  end;

  P := Result;
end;

function RecallFreeMem(Value: Pointer; None: NativeInt; var P: Pointer): {nil}Pointer;
begin
  if (Value <> nil) then
  begin
    if (MemoryManager.Standard.FreeMem(Value) {$ifdef FPC}={$else}<>{$endif} 0) then
      {$ifdef CONDITIONALEXPRESSIONS}System.Error(reInvalidPtr){$else}System.RunError(204){$endif};
  end;

  P := nil;
  Result := nil;
end;

procedure RegetMem(var P: Pointer; NewSize: NativeInt);
var
  Value: Pointer;
begin
  Value := P;
  if (NewSize > 0) then
  begin
    if (Value <> nil) then
    begin
      Value := MemoryManager.BrainMM.RegetMem(Value, NativeUInt(NewSize));
      P := Value;
      if (Value = nil) then
        {$ifdef CONDITIONALEXPRESSIONS}System.Error(reOutOfMemory){$else}System.RunError(203){$endif};
    end else
    begin
      RecallGetMem(nil, NewSize, P);
    end;
  end else
  begin
    RecallFreeMem(Value, 0, P);
  end;
end;

{$ifNdef MEMORYMANAGEREX}
function AllocMem(Size: NativeInt): Pointer;
begin
  if (Size > 0) then
  begin
    Result := MemoryManager.Standard.AllocMem(Size);
    if (Result = nil) then
      {$ifdef CONDITIONALEXPRESSIONS}System.Error(reOutOfMemory){$else}System.RunError(203){$endif};
  end else
  begin
    Result := nil;
  end;
end;
{$endif}

procedure GetMemoryBlock(var Block: MemoryBlock; BlockSize: TMemoryBlockSize);
var
  Value: MemoryBlock;
begin
  if (Byte(BlockSize) <= Byte(High(TMemoryBlockSize))) then
  begin
    Value := MemoryManager.BrainMM.GetMemoryBlock(BlockSize, PAGESMODE_USER);
    Block := Value;
    if (Value = nil) then
      {$ifdef CONDITIONALEXPRESSIONS}System.Error(reOutOfMemory){$else}System.RunError(203){$endif};
  end else
  begin
    {$ifdef CONDITIONALEXPRESSIONS}System.Error(reInvalidCast){$else}System.RunError(219){$endif};
  end;
end;

procedure FreeMemoryBlock(Block: MemoryBlock);
begin
  if (Block <> nil) then
  begin
    if (not MemoryManager.BrainMM.FreeMemoryBlock(Block, PAGESMODE_USER)) then
      {$ifdef CONDITIONALEXPRESSIONS}System.Error(reInvalidPtr){$else}System.RunError(204){$endif};
  end;
end;

procedure GetMemoryPages(var Pages: MemoryPages; Count: NativeInt);
var
  Value: MemoryPages;
begin
  if (Count > 0) then
  begin
    Value := MemoryManager.BrainMM.GetMemoryPages(NativeUInt(Count), PAGESMODE_USER);
    Pages := Value;
    if (Value = nil) then
      {$ifdef CONDITIONALEXPRESSIONS}System.Error(reOutOfMemory){$else}System.RunError(203){$endif};
  end else
  begin
    Pages := nil;
  end;
end;

procedure RegetMemoryPages(var Pages: MemoryPages; NewCount: NativeInt);
var
  Value: Pointer;
begin
  Value := Pages;
  if (NewCount > 0) then
  begin
    if (Value <> nil) then
    begin
      Value := MemoryManager.BrainMM.ResizeMemoryPages(Value, NativeUInt(NewCount), RESIZE_PAGES_USER_REGET);
      if (Value = PTR_INVALID) then
        {$ifdef CONDITIONALEXPRESSIONS}System.Error(reInvalidPtr){$else}System.RunError(204){$endif}
      else
      if (Value = nil) then
        {$ifdef CONDITIONALEXPRESSIONS}System.Error(reOutOfMemory){$else}System.RunError(203){$endif}
      else
        Pages := Value;
    end else
    begin
      GetMemoryPages(Pages, NewCount);
    end;
  end else
  begin
    Pages := nil;
    FreeMemoryPages(Value);
  end;
end;

procedure ReallocMemoryPages(var Pages: MemoryPages; NewCount: NativeInt);
var
  Value: Pointer;
begin
  Value := Pages;
  if (NewCount > 0) then
  begin
    if (Value <> nil) then
    begin
      Value := MemoryManager.BrainMM.ResizeMemoryPages(Value, NativeUInt(NewCount), RESIZE_PAGES_USER_REALLOC);
      if (Value = PTR_INVALID) then
        {$ifdef CONDITIONALEXPRESSIONS}System.Error(reInvalidPtr){$else}System.RunError(204){$endif}
      else
      if (Value = nil) then
        {$ifdef CONDITIONALEXPRESSIONS}System.Error(reOutOfMemory){$else}System.RunError(203){$endif}
      else
        Pages := Value;
    end else
    begin
      GetMemoryPages(Pages, NewCount);
    end;
  end else
  begin
    Pages := nil;
    FreeMemoryPages(Value);
  end;
end;

procedure FreeMemoryPages(Pages: MemoryPages);
begin
  if (Pages <> nil) then
  begin
    if (NativeInt(Pages) and (4 * 1024 - 1) <> 0) or
      (not MemoryManager.BrainMM.FreeMemoryPages(Pages, PAGESMODE_USER)) then
      {$ifdef CONDITIONALEXPRESSIONS}System.Error(reInvalidPtr){$else}System.RunError(204){$endif};
  end;
end;


 (* POSIX API *)

{$ifdef POSIX}
const
  PROT_READ       = $1;         { Page can be read.  }
  PROT_WRITE      = $2;         { Page can be written.  }
  PROT_EXEC       = $4;         { Page can be executed.  }
  PROT_NONE       = $0;         { Page can not be accessed.  }
  MAP_SHARED      = $01;        { Share changes.  }
  MAP_PRIVATE     = $02;        { Changes are private.  }
  MAP_TYPE        = $0F;        { Mask for type of mapping.  }
  MAP_FIXED       = $10;        { Interpret addr exactly.  }
  MAP_FILE        = $00;
  MAP_ANONYMOUS   = $20;        { Don't use a file.  }
  MAP_GROWSDOWN   = $0100;      { Stack-like segment.  }
  MAP_DENYWRITE   = $0800;      { ETXTBSY }
  MAP_EXECUTABLE  = $1000;      { Mark it as an executable.  }
  MAP_LOCKED      = $2000;      { Lock the mapping.  }
  MAP_NORESERVE   = $4000;      { Don't check for reservations.  }

  MAP_FAILED = Pointer(-1);

function memalign(alignment, size: NativeUInt): Pointer; cdecl;
  external libc name _PU + 'memalign';

procedure free(P: Pointer); cdecl;
  external libc name _PU + 'free';

function mmap(address: Pointer; length: NativeUInt; protect, flags, filedes: Integer;
  offset: NativeUInt): Pointer; cdecl;
  external libc name _PU + 'mmap';

function munmap(address: Pointer; length: NativeUInt): Integer; cdecl;
  external libc name _PU + 'munmap';

function mprotect(address: Pointer; length: NativeUInt; protect: Integer): Integer; cdecl;
  external libc name _PU + 'mprotect';

function mlock(address: Pointer; length: NativeUInt): Integer; cdecl;
  external libc name _PU + 'mlock';

function munlock(address: Pointer; length: NativeUInt): Integer; cdecl;
  external libc name _PU + 'munlock';

function mremap(address: Pointer; length, new_length: NativeUInt;
  flag: Integer): Pointer; cdecl;
  external libc name _PU + 'mremap';
{$endif}


 (* Helpfull routine *)

{$ifdef CONDITIONALEXPRESSIONS}
  {$if Defined(FPC) or (CompilerVersion < 24)}
    {$define ATOMICSEMULATE}
  {$ifend}
{$else}
  {$define ATOMICSEMULATE}
{$endif}

{$ifdef ATOMICSEMULATE}
{$ifdef FPC}
function AtomicCmpExchange(var Target: SupposedPtr; NewValue: SupposedPtr; Comparand: SupposedPtr): SupposedPtr; inline;
begin
  Result := InterlockedCompareExchange(Target, NewValue, Comparand);
end;
function AtomicIncrement(var Target: NativeUInt): NativeUInt; overload; inline;
begin
  Result := InterLockedExchangeAdd(Target, 1);
end;
function AtomicIncrement(var Target: NativeUInt; Increment: NativeUInt): NativeUInt; overload; inline;
begin
  Result := InterLockedExchangeAdd(Target, Increment);
end;
function AtomicDecrement(var Target: NativeUInt): NativeUInt; overload; inline;
begin
  Result := InterLockedExchangeAdd(Target, -1);
end;
function AtomicDecrement(var Target: NativeUInt; Increment: NativeUInt): NativeUInt; overload; inline;
begin
  Result := InterLockedExchangeAdd(Target, -Increment);
end;
{$else}
function AtomicCmpExchange(var Target: SupposedPtr; NewValue: SupposedPtr; Comparand: SupposedPtr): SupposedPtr;
asm
  {$ifdef CPUX86}
    xchg eax, ecx
    lock cmpxchg [ecx], edx
  {$else .CPUX64}
    mov rax, r8
    lock cmpxchg [rcx], rdx
  {$endif}
end;
function AtomicIncrement(var Target: NativeUInt): NativeUInt; overload;
asm
  {$ifdef CPUX86}
    mov edx, 1
    lock xadd [eax], edx
    lea eax, [edx + 1]
  {$else .CPUX64}
    mov edx, 1
    lock xadd [rcx], rdx
    lea rax, [rdx + 1]
  {$endif}
end;
function AtomicIncrement(var Target: NativeUInt; Increment: NativeUInt): NativeUInt; overload;
asm
  {$ifdef CPUX86}
    mov ecx, edx
    lock xadd [eax], edx
    lea eax, [edx + ecx]
  {$else .CPUX64}
    mov rax, rdx
    lock xadd [rcx], rdx
    add rax, rdx
  {$endif}
end;
function AtomicDecrement(var Target: NativeUInt): NativeUInt; overload;
asm
  {$ifdef CPUX86}
    or edx, -1
    lock xadd [eax], edx
    lea eax, [edx - 1]
  {$else .CPUX64}
    or rdx, -1
    lock xadd [rcx], rdx
    lea rax, [rdx - 1]
  {$endif}
end;
function AtomicDecrement(var Target: NativeUInt; Increment: NativeUInt): NativeUInt; overload;
asm
  {$ifdef CPUX86}
    neg edx
    mov ecx, edx
    lock xadd [eax], edx
    lea eax, [edx + ecx]
  {$else .CPUX64}
    neg rdx
    mov rax, rdx
    lock xadd [rcx], rdx
    add rax, rdx
  {$endif}
end;
{$endif}
{$endif}

{$ifdef BRAINMM_UNITTEST}
procedure FillRandomBytes(Memory: PByte; Count: NativeUInt);
var
  i: NativeUInt;
begin
  for i := 1 to Count do
  begin
    Memory^ := Random(256);
    Inc(Memory);
  end;
end;
{$endif}

procedure ChangeMemoryAccessRights(Pages: MemoryPages; Count: NativeInt;
  Rights: TMemoryAccessRights);
const
  HIGH_ACCESS_RIGHT = Ord(High(TMemoryAccessRight));
{$ifdef MSWINDOWS}
  ACCESS_RIGHTS: array[0.. 1 shl (HIGH_ACCESS_RIGHT + 1) - 1] of Cardinal = (
  { Execute | Write | Read }
    {000} PAGE_NOACCESS,
    {001} PAGE_READONLY,
    {010} PAGE_WRITECOPY,
    {011} PAGE_READWRITE,
    {100} PAGE_EXECUTE,
    {101} PAGE_EXECUTE_READ,
    {110} PAGE_EXECUTE_WRITECOPY,
    {111} PAGE_EXECUTE_READWRITE
  );
var
  Protect: Cardinal;
{$endif}
begin
  if (Byte(Rights) < (1 shl (HIGH_ACCESS_RIGHT + 1))) then
  begin
    if (Pages <> nil) and (NativeInt(Pages) and MASK_K4_TEST = 0) then
    begin
      if (Count > 0) then
      begin
        {$ifdef MSWINDOWS}
          if (not VirtualProtect(Pages, SIZE_K4 * Count, ACCESS_RIGHTS[Byte(Rights)], Protect)) then
            {$ifdef CONDITIONALEXPRESSIONS}System.Error(reInvalidPtr){$else}System.RunError(204){$endif};
        {$else .POSIX}
          // Prot = Byte(Rights)
          {$MESSAGE 'ToDo'}
        {$endif}
      end;
    end else
    begin
      {$ifdef CONDITIONALEXPRESSIONS}System.Error(reInvalidPtr){$else}System.RunError(204){$endif};
    end;
  end else
  begin
    {$ifdef CONDITIONALEXPRESSIONS}System.Error(reInvalidCast){$else}System.RunError(219){$endif};
  end;
end;

function GetMemoryAccessRights(Pages: MemoryPages): TMemoryAccessRights;
{$ifdef MSWINDOWS}
var
  MBI: TMemoryBasicInformation;
{$else}

{$endif}
begin
  if (Pages <> nil) and (NativeInt(Pages) and MASK_K4_TEST = 0) then
  begin
    {$ifdef MSWINDOWS}
      Result := [];
      if (VirtualQuery(Pages, MBI, SizeOf(MBI)) <> 0) then
      begin
        case (MBI.Protect) of
           PAGE_READONLY: Result := [marRead];
          PAGE_WRITECOPY: Result := [marWrite];
          PAGE_READWRITE: Result := [marRead, marWrite];
            PAGE_EXECUTE: Result := [marExecute];
       PAGE_EXECUTE_READ: Result := [marRead, marExecute];
  PAGE_EXECUTE_WRITECOPY: Result := [marWrite, marExecute];
  PAGE_EXECUTE_READWRITE: Result := [marRead, marWrite, marExecute];
        end;
      end;
    {$else}
      Result := [];
    {$endif}
  end else
  begin
    {$ifdef CONDITIONALEXPRESSIONS}System.Error(reInvalidPtr){$else}System.RunError(204){$endif};
  end;
end;

function GetMemoryOptions(const P: Pointer; var Options: TMemoryOptions): Boolean;
{$ifdef PUREPASCAL}
begin
  Result := MemoryManager.BrainMM.GetMemoryOptions(P, Options);
end;
{$else}
asm
  jmp [MemoryManager.BrainMM.GetMemoryOptions]
end;
{$endif}

function ThreadHeapMinimize: Boolean;
{$ifdef PUREPASCAL}
begin
  Result := MemoryManager.BrainMM.ThreadHeapMinimize;
end;
{$else}
asm
  jmp [MemoryManager.BrainMM.ThreadHeapMinimize]
end;
{$endif}


{$ifdef MSWINDOWS}
var
  SwitchToThreadFunc: function: BOOL; stdcall;
{$endif}

procedure ThreadSleepLong;
begin
  {$ifdef MSWINDOWS}
    Sleep(0);
  {$else .POSIX}
    usleep(0 shl 10);
  {$endif}
end;

procedure ThreadSleep;
begin
  {$ifdef MSWINDOWS}
    Sleep(1);
  {$else .POSIX}
    usleep(1 shl 10);
  {$endif}
end;

procedure ThreadSwitch;
begin
  {$ifdef MSWINDOWS}
  if (Assigned(SwitchToThreadFunc)) then
  begin
    SwitchToThreadFunc;
    Exit;
  end;
  {$endif}

  ThreadSleep;
end;

procedure ThreadPause;
{$ifdef CPUINTEL}
asm
  DB $F3, $90 // pause
end;
{$else}
begin
  ThreadSwitch;
end;
{$endif}

function SpinWait(var Spin: SupposedPtr; const Mask: NativeUInt): SupposedPtr;
var
  K: NativeUInt;
begin
  K := NativeUInt(-1);
  repeat
    Result := Spin;
    if (Result and Mask = 0) then Exit;
    
    K := (K + 1) and 7;
    case (K) of
   3..5: ThreadSwitch;
      6: ThreadSleep;
      7: ThreadSleepLong;
    else
      ThreadPause;
    end;
  until (False);
end;


{$ifdef CPUX86}
var
  SSE_SUPPORT: Integer{Boolean};

procedure CheckSSESupport;
asm
  push ebx
  mov eax, 1
  DB $0F, $A2 // cpuid
  test edx, 02000000h
  setnz byte ptr [SSE_SUPPORT]
  pop ebx
end;
{$endif}

{$ifdef CPUINTEL}
procedure BackwardSSEMove(const Source; var Dest; const B16Count: NativeInt);
const
  COUNT_8 = 8 * 8;
asm
  // x86: check SSE availability
  // eax/rax = Src + Size, edx/rdx = Dest + Size, ecx/rcx = -(8 * B16Count)
  {$ifdef CPUX86}
    cmp SSE_SUPPORT, 0
    jz @x86_non_SSE
    lea ecx, [ecx * 8]
    lea eax, [eax + ecx * 2]
    lea edx, [edx + ecx * 2]
    neg ecx
  {$else .CPUX64}
    lea r8, [r8 * 8]
    lea rax, [rcx + r8 * 2]
    lea rdx, [rdx + r8 * 2]
    xor rcx, rcx
    sub rcx, r8
  {$endif}

@move_next_16128:
  {$ifdef CPUX86}
    cmp ecx, -COUNT_8
    jle @move_128

    lea eax, [eax + ecx * 2]
    lea edx, [edx + ecx * 2]
    lea ecx, [ecx + @done]
    jmp ecx
  @move_128:
    lea eax, [eax - 128]
    lea edx, [edx - 128]
    lea ecx, [ecx + COUNT_8]
  {$else .CPUX64}
    cmp rcx, -COUNT_8
    jle @move_128

    lea rax, [rax + rcx * 2]
    lea rdx, [rdx + rcx * 2]
    lea r9, @done
    lea r9, [r9 + rcx]
    jmp r9
    nop
    nop
  @move_128:
    lea rax, [rax - 128]
    lea rdx, [rdx - 128]
    lea rcx, [rcx + COUNT_8]
  {$endif}

  {$ifdef CPUX86}
    DB $0F, $28, $48, $70 // movaps xmm1, [eax + 7*16]
    DB $0F, $29, $4A, $70 // movaps [edx + 7*16], xmm1
    DB $0F, $28, $40, $60 // movaps xmm0, [eax + 6*16]
    DB $0F, $29, $42, $60 // movaps [edx + 6*16], xmm0
    DB $0F, $28, $68, $50 // movaps xmm5, [eax + 5*16]
    DB $0F, $29, $6A, $50 // movaps [edx + 5*16], xmm5
    DB $0F, $28, $60, $40 // movaps xmm4, [eax + 4*16]
    DB $0F, $29, $62, $40 // movaps [edx + 4*16], xmm4
    DB $0F, $28, $58, $30 // movaps xmm3, [eax + 3*16]
    DB $0F, $29, $5A, $30 // movaps [edx + 3*16], xmm3
    DB $0F, $28, $50, $20 // movaps xmm2, [eax + 2*16]
    DB $0F, $29, $52, $20 // movaps [edx + 2*16], xmm2
    DB $0F, $28, $48, $10 // movaps xmm1, [eax + 1*16]
    DB $0F, $29, $4A, $10 // movaps [edx + 1*16], xmm1
    DB $0F, $28, $00      // movaps xmm0, [eax]
    DB $90                // nop
    DB $0F, $29, $02      // movaps [edx], xmm0
    DB $90                // nop
  {$else .CPUX64}
    movaps xmm1, [rax + 7*16]
    movaps [rdx + 7*16], xmm1
    movaps xmm0, [rax + 6*16]
    movaps [rdx + 6*16], xmm0
    movaps xmm5, [rax + 5*16]
    movaps [rdx + 5*16], xmm5
    movaps xmm4, [rax + 4*16]
    movaps [rdx + 4*16], xmm4
    movaps xmm3, [rax + 3*16]
    movaps [rdx + 3*16], xmm3
    movaps xmm2, [rax + 2*16]
    movaps [rdx + 2*16], xmm2
    movaps xmm1, [rax + 1*16]
    movaps [rdx + 1*16], xmm1
    movaps xmm0, [rax]
    nop
    movaps [rdx], xmm0
    nop
  {$endif}
@done:
  jl @move_next_16128
  ret

  {$ifdef CPUX86}
  @x86_non_SSE:
    shl ecx, 4
    jmp Move
  {$endif}
end;
{$endif}

procedure NcMoveB16(var Source, Dest: B16; B16Count: NativeUInt); forward;

procedure MoveB16Aligned(const Source; var Dest; const B16Count: NativeInt);
{$ifNdef CPUINTEL}
begin
  Move(Source, Dest, B16Count shl 4);
end;
{$else}
asm
  // if (B16Count <= 0) then Exit
  {$ifdef CPUX86}
    test ecx, ecx
  {$else .CPUX64}
    test r8, r8
  {$endif}
  jle @none

  // if (Dest = Source) then Exit
  // if (Dest < Source) then NcMoveB16(Source, Dest, B16Count)
  {$ifdef CPUX86}
    cmp edx, eax
  {$else .CPUX64}
    cmp rdx, rcx
  {$endif}
  je @none
  jb NcMoveB16

  // if (Dest >= Source + 16 * B16Count) then NcMoveB16(Source, Dest, B16Count)
  {$ifdef CPUX86}
    push eax
    lea eax, [eax + 8 * ecx]
    lea eax, [eax + 8 * ecx]
    cmp edx, eax
    pop eax
  {$else .CPUX64}
    lea r9, [rax + 8 * r8]
    lea r9, [r9 + 8 * r8]
    cmp rdx, r9
  {$endif}
  jae NcMoveB16

  // BackwardSSEMove(Source, Dest, B16Count)
  jmp BackwardSSEMove

@none:
end;
{$endif}

procedure NcMoveB16(var Source, Dest: B16; B16Count: NativeUInt);
{$ifNdef CPUINTEL} inline;
begin
  {$ifdef POSIX}
     memcpy(Dest, Source, B16Count shl 4);
  {$else}
     Move(Source, Dest, B16Count shl 4);
  {$endif}
end;
{$else}
asm
  {$ifdef CPUX86}
    // check SSE availability
    cmp SSE_SUPPORT, 0
    jz @x86_non_SSE
  {$else .CPUX64}
    // rax = Src, rdx = Dest, rcx = B16Count
    mov rax, rcx
    mov rcx, r8
  {$endif}

@move_next_16128:
  {$ifdef CPUX86}
    cmp ecx, 8
    jae @move_128

    lea ecx, [ecx * 8]
    lea eax, [eax + ecx * 2]
    lea edx, [edx + ecx * 2]
    neg ecx
    lea ecx, [ecx + @done]
    jmp ecx
  @move_128:
    lea eax, [eax + 128]
    lea edx, [edx + 128]
    lea ecx, [ecx - 8]
  {$else .CPUX64}
    cmp rcx, 8
    jae @move_128

    lea rcx, [rcx * 8]
    lea rax, [rax + rcx * 2]
    lea rdx, [rdx + rcx * 2]
    lea r9, @done
    neg rcx
    lea r9, [r9 + rcx]
    jmp r9
    nop
  @move_128:
    lea rax, [rax + 128]
    lea rdx, [rdx + 128]
    lea rcx, [rcx - 8]
  {$endif}

  {$ifdef CPUX86}
    DB $0F, $28, $48, $80 // movaps xmm1, [eax - 7*16 - 16]
    DB $0F, $29, $4A, $80 // movaps [edx - 7*16 - 16], xmm1
    DB $0F, $28, $40, $90 // movaps xmm0, [eax - 6*16 - 16]
    DB $0F, $29, $42, $90 // movaps [edx - 6*16 - 16], xmm0
    DB $0F, $28, $68, $A0 // movaps xmm5, [eax - 5*16 - 16]
    DB $0F, $29, $6A, $A0 // movaps [edx - 5*16 - 16], xmm5
    DB $0F, $28, $60, $B0 // movaps xmm4, [eax - 4*16 - 16]
    DB $0F, $29, $62, $B0 // movaps [edx - 4*16 - 16], xmm4
    DB $0F, $28, $58, $C0 // movaps xmm3, [eax - 3*16 - 16]
    DB $0F, $29, $5A, $C0 // movaps [edx - 3*16 - 16], xmm3
    DB $0F, $28, $50, $D0 // movaps xmm2, [eax - 2*16 - 16]
    DB $0F, $29, $52, $D0 // movaps [edx - 2*16 - 16], xmm2
    DB $0F, $28, $48, $E0 // movaps xmm1, [eax - 1*16 - 16]
    DB $0F, $29, $4A, $E0 // movaps [edx - 1*16 - 16], xmm1
    DB $0F, $28, $40, $F0 // movaps xmm0, [eax - 0*16 - 16]
    DB $0F, $29, $42, $F0 // movaps [edx - 0*16 - 16], xmm0
  {$else .CPUX64}
    movaps xmm1, [rax - 7*16 - 16]
    movaps [rdx - 7*16 - 16], xmm1
    movaps xmm0, [rax - 6*16 - 16]
    movaps [rdx - 6*16 - 16], xmm0
    movaps xmm5, [rax - 5*16 - 16]
    movaps [rdx - 5*16 - 16], xmm5
    movaps xmm4, [rax - 4*16 - 16]
    movaps [rdx - 4*16 - 16], xmm4
    movaps xmm3, [rax - 3*16 - 16]
    movaps [rdx - 3*16 - 16], xmm3
    movaps xmm2, [rax - 2*16 - 16]
    movaps [rdx - 2*16 - 16], xmm2
    movaps xmm1, [rax - 1*16 - 16]
    movaps [rdx - 1*16 - 16], xmm1
    movaps xmm0, [rax - 0*16 - 16]
    movaps [rdx - 0*16 - 16], xmm0
  {$endif}
@done:
  ja @move_next_16128
  ret

  {$ifdef CPUX86}
  @x86_non_SSE:
    shl ecx, 4
    jmp Move
  {$endif}
end;
{$endif}

function NcMoveB16Small(var Source, Dest: B16; B16Count{0..8}: NativeUInt): {Dest}P16;
{$ifNdef CPUINTEL} inline;
begin
  {$ifdef POSIX}
     memcpy(Dest, Source, B16Count shl 4);
  {$else}
     Move(Source, Dest, B16Count shl 4);
  {$endif}

  Result := @Dest;
end;
{$else}
asm
  {$ifdef CPUX86}
    // check SSE availability
    cmp SSE_SUPPORT, 0
    jz @x86_non_SSE
  {$else .CPUX64}
    // rax = Src, rdx = Dest, rcx = B16Count
    mov rax, rcx
    mov rcx, r8
  {$endif}

  {$ifdef CPUX86}
    lea ecx, [ecx * 8]
    lea eax, [eax + ecx * 2]
    lea edx, [edx + ecx * 2]
    neg ecx
    lea ecx, [ecx + @done]
    jmp ecx
  {$else .CPUX64}
    lea rcx, [rcx * 8]
    lea rax, [rax + rcx * 2]
    lea rdx, [rdx + rcx * 2]
    lea r9, @done
    neg rcx
    lea r9, [r9 + rcx]
    jmp r9
    nop
  {$endif}

  {$ifdef CPUX86}
    DB $0F, $28, $48, $80 // movaps xmm1, [eax - 7*16 - 16]
    DB $0F, $29, $4A, $80 // movaps [edx - 7*16 - 16], xmm1
    DB $0F, $28, $40, $90 // movaps xmm0, [eax - 6*16 - 16]
    DB $0F, $29, $42, $90 // movaps [edx - 6*16 - 16], xmm0
    DB $0F, $28, $68, $A0 // movaps xmm5, [eax - 5*16 - 16]
    DB $0F, $29, $6A, $A0 // movaps [edx - 5*16 - 16], xmm5
    DB $0F, $28, $60, $B0 // movaps xmm4, [eax - 4*16 - 16]
    DB $0F, $29, $62, $B0 // movaps [edx - 4*16 - 16], xmm4
    DB $0F, $28, $58, $C0 // movaps xmm3, [eax - 3*16 - 16]
    DB $0F, $29, $5A, $C0 // movaps [edx - 3*16 - 16], xmm3
    DB $0F, $28, $50, $D0 // movaps xmm2, [eax - 2*16 - 16]
    DB $0F, $29, $52, $D0 // movaps [edx - 2*16 - 16], xmm2
    DB $0F, $28, $48, $E0 // movaps xmm1, [eax - 1*16 - 16]
    DB $0F, $29, $4A, $E0 // movaps [edx - 1*16 - 16], xmm1
    DB $0F, $28, $40, $F0 // movaps xmm0, [eax - 0*16 - 16]
    DB $0F, $29, $42, $F0 // movaps [edx - 0*16 - 16], xmm0
  {$else .CPUX64}
    movaps xmm1, [rax - 7*16 - 16]
    movaps [rdx - 7*16 - 16], xmm1
    movaps xmm0, [rax - 6*16 - 16]
    movaps [rdx - 6*16 - 16], xmm0
    movaps xmm5, [rax - 5*16 - 16]
    movaps [rdx - 5*16 - 16], xmm5
    movaps xmm4, [rax - 4*16 - 16]
    movaps [rdx - 4*16 - 16], xmm4
    movaps xmm3, [rax - 3*16 - 16]
    movaps [rdx - 3*16 - 16], xmm3
    movaps xmm2, [rax - 2*16 - 16]
    movaps [rdx - 2*16 - 16], xmm2
    movaps xmm1, [rax - 1*16 - 16]
    movaps [rdx - 1*16 - 16], xmm1
    movaps xmm0, [rax - 0*16 - 16]
    movaps [rdx - 0*16 - 16], xmm0
  {$endif}
@done:
  {$ifdef CPUX86}
    sub ecx, offset @done
    lea eax, [edx + ecx * 2]
  {$else .CPUX64}
    lea rax, [rdx + rcx * 2]
  {$endif}
  ret

  {$ifdef CPUX86}
  @x86_non_SSE:
  push eax
    shl ecx, 4
    call Move
  pop eax
  {$endif}
end;
{$endif}


const
  BIT_SCANS: array[Byte] of Byte = ({failure}8, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0,
    2, 0, 1, 0, 4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 5, 0, 1, 0, 2,
    0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0,
    1, 0, 6, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, 0, 1, 0, 2, 0, 1,
    0, 3, 0, 1, 0, 2, 0, 1, 0, 5, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0,
    4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 7, 0, 1, 0, 2, 0, 1, 0, 3,
    0, 1, 0, 2, 0, 1, 0, 4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 5, 0,
    1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1,
    0, 2, 0, 1, 0, 6, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, 0, 1, 0,
    2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 5, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2,
    0, 1, 0, 4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0
  );


function BitReserve(var BitSet: TBitSet8): NativeInt;
{$ifdef PUREPASCAL}
var
  P: PByte;
  VInteger: Integer;
  PVInteger: PInteger;
begin
  P := @BitSet.VBytes[0];
  Inc(PInteger(P), Byte(PInteger(P)^ = 0));

  PVInteger := Pointer(P);
  VInteger := PVInteger^;
  if (VInteger <> 0) then
  begin
    Inc(PWord(P), Byte(VInteger and $ffff = 0));
    Inc(P, Byte(P^ = 0));
    Result := (NativeUInt(P) - NativeUInt(@BitSet)) shl 3 + BIT_SCANS[P^];
    PVInteger^ := VInteger xor (1 shl (Result and 31));
    Exit;
  end else
  begin
    Result := -1;
  end;
end;
{$else}
asm
  {$ifdef CPUX86}
    mov edx, [eax]
    lea ecx, [eax + 4]
    test edx, edx
    DB $0F, $44, $C1 // cmovz eax, ecx
    lea ecx, [ecx - 4]
    mov edx, [eax]
    push ecx
    rep bsf ecx, edx
    test edx, edx
    jz @failure
    btr edx, ecx
    mov [eax], edx
    pop edx
    sub edx, eax
    and edx, 32
    lea eax, [edx + ecx]
  {$else .CPUX64}
    mov rdx, [rcx]
    rep bsf rax, rdx
    test rdx, rdx
    jz @failure
    btr rdx, rax
    mov [rcx], rdx
  {$endif}

  ret
@failure:
  {$ifdef CPUX86}
    pop ecx
    or eax, -1
  {$else .CPUX64}
    or rax, -1
  {$endif}
end;
{$endif}

function BitUnreserve(var BitSet: TBitSet8; const Index: NativeUInt): Boolean;
{$ifdef PUREPASCAL}
var
  Mask: NativeInt;
  Value: NativeInt;
  {$ifdef SMALLINT}
  PVInteger: PInteger;
  {$endif}
begin
  Mask := 1;
  {$ifdef LARGEINT}
    Mask := Mask shl Index;
    Value := BitSet.V64;
  {$else .SMALLINT}
    Mask := Mask shl (Index and 31);
    PVInteger := @BitSet.VIntegers[Byte(Index > 31)];
    Value := PVInteger^;
  {$endif}
  if (Value and Mask = 0) then
  begin
    Inc(Value, Mask);
    {$ifdef LARGEINT}
      BitSet.V64 := Value;
    {$else .SMALLINT}
      PVInteger^ := Value;
    {$endif}
    Result := True;
    Exit;
  end else
  begin
    Result := False;
  end;
end;
{$else}
asm
  {$ifdef CPUX86}
    lea ecx, [eax + 4]
    test edx, 32
    DB $0F, $45, $C1 // cmovnz eax, ecx
    and edx, 31
    mov ecx, [eax]
    bts ecx, edx
    mov [eax], ecx
    setnc al
  {$else .CPUX64}
    mov rax, [rcx]
    bts rax, rdx
    mov [rcx], rax
    setnc al
  {$endif}
end;
{$endif}


{ TSyncStack }

{$ifdef CPUX64}
const
  X64_SYNCSTACK_MASK = ((NativeUInt(1) shl 48) - 1) and -16;
  X64_SYNCSTACK_CLEAR = not X64_SYNCSTACK_MASK;
{$endif}

{$ifNdef SMALLINT}
function TSyncStack.GetAssigned: Boolean;
begin
  {$ifdef CPUINTEL}
    Result := (F.Handle and (((NativeUInt(1) shl 48) - 1) and -16){X64_SYNCSTACK_MASK} <> 0);
  {$else .CPUARM64}
    Result := (F.Handle <> 0);
  {$endif}
end;
{$endif}

procedure TSyncStack.Push(const Value: Pointer);
{$ifdef INLINESUPPORT}
begin
  Self.PushList(Value, Value);
end;
{$else .CPUX86}
asm
  mov ecx, edx
  jmp TSyncStack.PushList
end;
{$endif}

procedure TSyncStack.PushList(const First, Last: Pointer);
{$ifNdef CPUX86}
var
  Item, NewItem: SupposedPtr;
begin
  repeat
    Item := F.Handle;
    PSupposedPtr(Last)^ := Item {$ifdef CPUX64}and X64_SYNCSTACK_MASK{$endif};
    NewItem := SupposedPtr(First) {$ifdef CPUX64}+ (((Item or X64_SYNCSTACK_MASK) + 1) and X64_SYNCSTACK_CLEAR){$endif};
  until (Item = AtomicCmpExchange(F.Handle, NewItem, Item));
end;
{$else .CPUX86}
asm
  push esi
  push edi
  push ebp
  push ebx
  mov esi, eax // Self
  mov edi, edx // First
  mov ebp, ecx // Last

  // Item := F.Handle
@move_8:
  mov eax, [esi]
  mov edx, [esi + 4]
  cmp eax, [esi]
  jne @move_8

  // lock-free loop
  @repeat:
    // PStackItem(Last).Next := Item
    mov [ebp], eax

    // NewItem := First | Counter++
    mov ebx, edi
    lea ecx, [edx + 1]

    // Item := AtomicCmpExchangeInt64(F.Handle, NewItem, Item)
    DB $F0, $0F, $C7, $0E // lock cmpxchg8b [esi]
  jnz @repeat

@done:
  pop ebx
  pop ebp
  pop edi
  pop esi
end;
{$endif}

function TSyncStack.Pop: Pointer;
{$ifNdef CPUX86}
var
  Item, NewItem: SupposedPtr;
begin
  repeat
    Item := F.Handle;
    Result := Pointer(Item {$ifdef CPUX64}and X64_SYNCSTACK_MASK{$endif});
    if (Result = nil) then Exit;

    NewItem := PSupposedPtr(Result)^ {$ifdef CPUX64}+ (Item and X64_SYNCSTACK_CLEAR){$endif};
  until (Item = AtomicCmpExchange(F.Handle, NewItem, Item));
end;
{$else .CPUX86}
asm
  push esi
  push ebx
  mov esi, eax // Self

  // Item := F.Handle
@move_8:
  mov eax, [esi]
  mov edx, [esi + 4]
  cmp eax, [esi]
  jne @move_8

  // lock-free loop
  @repeat:
    // if (Result(Item) = nil) then Exit;
    test eax, eax
    jz @done

    // NewItem := PStackItem(Item).Next | Counter
    mov ebx, [eax]
    mov ecx, edx

    // Item := AtomicCmpExchangeInt64(F.Handle, NewItem, Item)
    DB $F0, $0F, $C7, $0E // lock cmpxchg8b [esi]
  jnz @repeat

@done:
  pop ebx
  pop esi
end;
{$endif}

function TSyncStack.PopList: Pointer;
{$ifNdef CPUX86}
var
  Item, NewItem: SupposedPtr;
begin
  repeat
    Item := F.Handle;
    Result := Pointer(Item {$ifdef CPUX64}and X64_SYNCSTACK_MASK{$endif});
    if (Result = nil) then Exit;

    NewItem := {nil}0 {$ifdef CPUX64}+ (Item and X64_SYNCSTACK_CLEAR){$endif};
  until (Item = AtomicCmpExchange(F.Handle, NewItem, Item));
end;
{$else .CPUX86}
asm
  push esi
  push ebx
  mov esi, eax // Self

  // Item := F.Handle
@move_8:
  mov eax, [esi]
  mov edx, [esi + 4]
  cmp eax, [esi]
  jne @move_8

  // lock-free loop
  @repeat:
    // if (Result(Item) = nil) then Exit;
    test eax, eax
    jz @done

    // NewItem := 0 | Counter
    xor ebx, ebx
    mov ecx, edx

    // Item := AtomicCmpExchangeInt64(F.Handle, NewItem, Item)
    DB $F0, $0F, $C7, $0E // lock cmpxchg8b [esi]
  jnz @repeat

@done:
  pop ebx
  pop esi
end;
{$endif}


{ TGlobalStorage }

{class} function TGlobalStorage.GrowThreadHeaps: PThreadHeap;
type
  TCoreThreadHeapList = array[0..SIZE_K64 div SizeOf(TThreadHeap) - 1] of TThreadHeap;
  PCoreThreadHeapList = ^TCoreThreadHeapList;
var
  i: NativeInt;
  Instance: PGlobalStorage;
  CoreThreadHeapList: PCoreThreadHeapList;
begin
  Instance := Pointer(NativeInt(@GLOBAL_STORAGE[63]) and MASK_64_CLEAR);

  {$ifdef MSWINDOWS}
    CoreThreadHeapList := VirtualAlloc(nil, SIZE_K64, MEM_COMMIT or MEM_TOP_DOWN, PAGE_READWRITE);
  {$else .POSIX}
    CoreThreadHeapList := memalign(SIZE_K64, SIZE_K64);
  {$endif}
  if (CoreThreadHeapList = nil) then
  begin
    Result := nil;
    Exit;
  end;

  {$ifdef BRAINMM_UNITTEST}
  FillRandomBytes(Pointer(CoreThreadHeapList), SIZE_K64);
  {$endif}

  for i := 1 to High(TCoreThreadHeapList) - 1 do
    CoreThreadHeapList[i].FNextHeap := @CoreThreadHeapList[i + 1];

  Instance.CoreThreadHeaps.PushList(@CoreThreadHeapList[1],
    @CoreThreadHeapList[High(TCoreThreadHeapList)]);

  Result := @CoreThreadHeapList[0];
end;

{class} function TGlobalStorage.GrowInternalRecords: PInternalRecord;
type
  TCoreInternalRecordList = array[0..SizeOf(TThreadHeap) div SizeOf(TInternalRecord) - 1] of TInternalRecord;
  PCoreInternalRecordList = ^TCoreInternalRecordList;
var
  i: NativeInt;
  Instance: PGlobalStorage;
  CoreInternalRecordList: PCoreInternalRecordList;
begin
  Instance := Pointer(NativeInt(@GLOBAL_STORAGE[63]) and MASK_64_CLEAR);
  CoreInternalRecordList := Instance.CoreThreadHeaps.Pop;
  if (CoreInternalRecordList = nil) then
  begin
    CoreInternalRecordList := Pointer(TGlobalStorage(nil^).GrowThreadHeaps);
    if (CoreInternalRecordList = nil) then
    begin
      Result := nil;
      Exit;
    end;
  end;

  for i := 1 to High(TCoreInternalRecordList) - 1 do
    CoreInternalRecordList[i].Next := @CoreInternalRecordList[i + 1];

  Instance := Pointer(NativeInt(@GLOBAL_STORAGE[63]) and MASK_64_CLEAR);
  Instance.CoreInternalRecords.PushList(@CoreInternalRecordList[1],
    @CoreInternalRecordList[High(TCoreInternalRecordList)]);

  Result := @CoreInternalRecordList[0];
end;

{class} function TGlobalStorage.InternalRecordPop: PInternalRecord;
begin
  Result := PGlobalStorage(NativeInt(@GLOBAL_STORAGE[63]) and MASK_64_CLEAR).CoreInternalRecords.Pop;
  if (Result = nil) then
    Result := PGlobalStorage(Result).GrowInternalRecords;
end;

{class} procedure TGlobalStorage.InternalRecordPush(const InternalRecord: PInternalRecord);
begin
  PGlobalStorage(NativeInt(@GLOBAL_STORAGE[63]) and MASK_64_CLEAR).
    CoreInternalRecords.PushList(InternalRecord, InternalRecord);
end;

{class} function TGlobalStorage.K64BlockCachePop: MemoryBlock;
var
  Instance: PGlobalStorage;
  InternalRecord: PInternalRecord;
begin
  Instance := Pointer(NativeInt(@GLOBAL_STORAGE[63]) and MASK_64_CLEAR);
  if (Instance.K64BlockCache.Stack.Assigned) then
  begin
    InternalRecord := Instance.K64BlockCache.Stack.Pop;
    if (InternalRecord <> nil) then
    begin
      Instance := Pointer(NativeInt(@GLOBAL_STORAGE[63]) and MASK_64_CLEAR);
      AtomicDecrement(Instance.K64BlockCache.Count);

      Result := InternalRecord.K64Block;
      Instance.CoreInternalRecords.Push(InternalRecord);

      Exit;
    end;
  end;

  Result := nil;
end;

{class} function TGlobalStorage.K64BlockCachePush(const K64Block: MemoryBlock): Boolean;
const
  CACHE_LIMIT = 64 {4Mb cache};
var
  Instance: PGlobalStorage;
  InternalRecord: PInternalRecord;
begin
  Instance := Pointer(NativeInt(@GLOBAL_STORAGE[63]) and MASK_64_CLEAR);
  if (Instance.K64BlockCache.Count < CACHE_LIMIT) then
  begin
    InternalRecord := TGlobalStorage(nil^).InternalRecordPop;
    if (InternalRecord <> nil) then
    begin
      InternalRecord.K64Block := K64Block;

      Instance := Pointer(NativeInt(@GLOBAL_STORAGE[63]) and MASK_64_CLEAR);
      AtomicIncrement(Instance.K64BlockCache.Count);
      Instance.K64BlockCache.Stack.Push(InternalRecord);

      Result := True;
      Exit;
    end;
  end;

  Result := False;
end;

{class} function TGlobalStorage.ExcludePagesRecord(const SupposedPages: SupposedPtr): PInternalRecord;
const
  // clear IsLarge/Align
  PAGES_MASK_CLEAR = SupposedPtr(-1) xor ((1 shl 4 - 1) shl 3);
var
  Hash, PagesValue: NativeUInt;
  Instance: PGlobalStorage;
  PItem: PSupposedPtr;
  Item: SupposedPtr;
  Prev: PInternalRecord;
  Store: record
    PItem: PSupposedPtr;
    Item: SupposedPtr;
    Result: PInternalRecord;
  end;
begin
  // item
  Hash := (SupposedPages shr 16) xor ((SupposedPages shr 6) and ($f shl 6));
  Instance := Pointer(NativeInt(@GLOBAL_STORAGE[63]) and MASK_64_CLEAR);
  PItem := @Instance.PagesItems[Hash and High(PagesItems)];

  // pages bits
  PagesValue := SupposedPages and PAGES_MASK_CLEAR;

  // inline SpinLock
  Store.PItem := PItem;
  repeat
    Item := PItem^;
    if (Item and 1 <> 0) then
    begin
      Item := SpinWait(PItem^, 1);
      PItem := Store.PItem;
    end;
  until (Item = AtomicCmpExchange(PItem^, Item + 1, Item));
  Store.Item := Item;

  // find and exclude
  Result := Pointer(Item);
  if (Item <> 0) then
  try
    if (Result.SupposedPages and PAGES_MASK_CLEAR = PagesValue) then
    begin
      Store.Item := 0;
    end else
    begin
      repeat
        Prev := Result;
        Result := Result.Next;
        if (Result = nil) then Break;

        if (Result.SupposedPages and PAGES_MASK_CLEAR = PagesValue) then
        begin
          Prev.Next := Result.Next;
          Break;
        end;
      until (False);
    end;

    Store.Result := Result;
  finally
    Store.PItem^ := Store.Item; // inline SpinUnlock
    Result := Store.Result;
  end;
end;

{class} procedure TGlobalStorage.IncludePagesRecord(const PagesRecord: PInternalRecord);
var
  Hash: NativeUInt;
  Instance: PGlobalStorage;
  PItem: PSupposedPtr;
  Item: SupposedPtr;
begin
  // item
  Hash := PagesRecord.SupposedPages;
  Hash := (Hash{SupposedPages} shr 16) xor ((Hash{SupposedPages} shr 6) and ($f shl 6));
  Instance := Pointer(NativeInt(@GLOBAL_STORAGE[63]) and MASK_64_CLEAR);
  PItem := @Instance.PagesItems[Hash and High(PagesItems)];

  // wait and include
  repeat
    Item := PItem^;
    if (Item and 1 <> 0) then
    begin
      Item := SpinWait(PItem^, 1);
    end;
    PagesRecord.Next := Pointer(Item);
  until (Item = AtomicCmpExchange(PItem^, SupposedPtr(PagesRecord), Item))
end;


 (* Local thread heap initialization *)

{$ifdef PUREPASCAL}
threadvar
  ThreadHeapInstance: PThreadHeap;
{$endif}

function CreateThreadHeap(Store: Boolean): PThreadHeap;
var
  GlobalStorage: PGlobalStorage;
  InternalRecord: PInternalRecord;
  Flags: SupposedPtr;
  NextHeap: PThreadHeap;
begin
  GlobalStorage := Pointer(NativeInt(@GLOBAL_STORAGE[63]) and MASK_64_CLEAR);

  InternalRecord := GlobalStorage.HeapsDeferred.Pop;
  if (InternalRecord <> nil) then
  begin
    Result := InternalRecord.ThreadHeap;

    // make unlokable unlocked
    repeat
      Flags := Result.LockFlags;
      if (Flags <> THREAD_HEAP_LOCKABLE) then
      begin
        if (Flags = THREAD_HEAP_LOCKED) then
        begin
          SpinWait(Result.LockFlags, THREAD_HEAP_LOCKED_BIT);
        end else
        begin
          {$ifdef CONDITIONALEXPRESSIONS}System.Error(reInvalidCast){$else}System.RunError(219){$endif};
        end;
      end;
    until (THREAD_HEAP_LOCKABLE = AtomicCmpExchange(Result.LockFlags, 0, THREAD_HEAP_LOCKABLE));

    // deferred
    if (Result.Deferreds.Assigned) then
    begin
      Result.ProcessThreadDeferred;
    end;

    // make internal record free
    GlobalStorage.CoreInternalRecords.Push(InternalRecord);
  end else
  begin
    // new instance
    Result := GlobalStorage.CoreThreadHeaps.Pop;
    if (Result = nil) then
    begin
      Result := TGlobalStorage(nil^).GrowThreadHeaps;
      if (Result = nil) then
        {$ifdef CONDITIONALEXPRESSIONS}System.Error(reOutOfMemory){$else}System.RunError(203){$endif};
    end;
    FillChar(Result^, SizeOf(TThreadHeap), #0);
    Result.FMarkerNotSelf := not SupposedPtr(Result);

    // include to heap list
    repeat
      NextHeap := ThreadHeapList;
      Result.FNextHeap := NextHeap;
    until (SupposedPtr(NextHeap) = AtomicCmpExchange(SupposedPtr(ThreadHeapList),
      SupposedPtr(Result), SupposedPtr(NextHeap)));
  end;

  Result.ThreadId := GetCurrentThreadId;
  {$ifdef PUREPASCAL}
  if (Store) then
    ThreadHeapInstance := Result;
  {$endif}
end;

{$ifNdef PUREPASCAL}
const
  THREAD_HEAP = {$ifdef CPUX86}$14{$else}$28{$endif};

function ThreadHeapInstance: PThreadHeap;
asm
  // Get
  {$ifdef CPUX86}
    mov eax, fs:[THREAD_HEAP]
    test eax, eax
  {$else .CPUX64}
    mov rax, gs:[abs THREAD_HEAP]
    test rax, rax
  {$endif}
  jz @create_thread_heap
  ret

@create_thread_heap:
  {$ifdef CPUX86}
    xor eax, eax
    call CreateThreadHeap
  {$else .CPUX64}
    {stack align} push rcx
    xor rcx, rcx
    call CreateThreadHeap
    {stack align} pop rcx
  {$endif}

  // Set
  {$ifdef CPUX86}
    mov fs:[THREAD_HEAP], eax
  {$else .CPUX64}
    mov gs:[abs THREAD_HEAP], rax
  {$endif}
end;

function SafeProcessThreadDeferred(Self: PThreadHeap
  {, v2, v3, ReturnAddress, x64_v5: Pointer}): PThreadHeap;
asm
  {$ifdef CPUX86}
  push eax
    push edx
    push ecx
      call TThreadHeap.ProcessThreadDeferred
    pop ecx
    pop edx
  pop eax
  jmp ebx
  {$else .CPUX64}
  push rcx
  push r9
    push rdx
    push r8
    push r10
      call TThreadHeap.ProcessThreadDeferred
    pop r10
    pop r8
    pop rdx
  pop r9
  pop rcx
  jmp r9
  {$endif}
end;
{$endif}

function CurrentThreadHeap: PThreadHeap;
{$ifdef PUREPASCAL}
begin
  Result := ThreadHeapInstance;
  if (Result = nil) then
    Result := UnknownThreadHeap;
end;
{$else}
asm
  {$ifdef CPUX86}
    mov eax, fs:[THREAD_HEAP]
    mov edx, UnknownThreadHeap
    test eax, eax
    DB $0F, $44, $C2 // cmovz eax, edx
  {$else .CPUX64}
    mov rax, gs:[abs THREAD_HEAP]
    mov rdx, UnknownThreadHeap
    test rax, rax
    cmovz rax, rdx
  {$endif}
end;
{$endif}


type
  PThreadRec = ^TThreadRec;
  TThreadRec = record
    Func: TThreadFunc;
    Parameter: Pointer;
  end;

procedure BrainMMEndThreadEvent(ExitCode: Integer);
var
  ThreadHeap: PThreadHeap;
  GlobalStorage: PGlobalStorage;
  InternalRecord: PInternalRecord;
begin
  ThreadHeap := ThreadHeapInstance;
  {$ifdef PUREPASCAL}
    ThreadHeapInstance := nil;
  {$endif}

  if (0 = AtomicCmpExchange(ThreadHeap.LockFlags, THREAD_HEAP_LOCKED, 0)) then
  begin
    GlobalStorage := Pointer(NativeInt(@GLOBAL_STORAGE[63]) and MASK_64_CLEAR);
    AtomicDecrement(GlobalStorage.ThreadsCount);
    try
      if (ThreadHeap.Deferreds.Assigned) then
      begin
        ThreadHeap.ProcessThreadDeferred;
      end;
    finally
      ThreadHeap.LockFlags := THREAD_HEAP_LOCKABLE;
    end;

    // push heaps deffered
    InternalRecord := TGlobalStorage(nil^).InternalRecordPop;
    if (InternalRecord <> nil) then
    begin
      InternalRecord.ThreadHeap := ThreadHeap;
      GlobalStorage := Pointer(NativeInt(@GLOBAL_STORAGE[63]) and MASK_64_CLEAR);
      GlobalStorage.HeapsDeferred.Push(InternalRecord);
    end else
    begin
      {$ifdef CONDITIONALEXPRESSIONS}System.Error(reOutOfMemory){$else}System.RunError(203){$endif};
    end;
  end else
  begin
    // dirty lock flags
    {$ifdef CONDITIONALEXPRESSIONS}System.Error(reInvalidCast){$else}System.RunError(219){$endif};
  end;
end;

function BrainMMThreadProxy(Parameter: Pointer): Integer;
var
  ThreadRec: TThreadRec;
  GlobalStorage: PGlobalStorage;
begin
  // parameters
  {$ifdef PUREPASCAL}
    CreateThreadHeap(True);
  {$else}
    ThreadHeapInstance;
  {$endif}
  ThreadRec := PThreadRec(Parameter)^;
  GlobalStorage := Pointer(NativeInt(@GLOBAL_STORAGE[63]) and MASK_64_CLEAR);
  GlobalStorage.CoreInternalRecords.Push(Parameter);
  AtomicIncrement(GlobalStorage.ThreadsCount);

  try
    // thread function
    Result := ThreadRec.Func(ThreadRec.Parameter);
  finally
    // emergency cleanup
    BrainMMEndThreadEvent(0);
  end;
end;

{$ifdef FPC}
  {$MESSAGE ERROR 'THREAD ROUTINE NOT YET IMPLEMENTED'}
{$else}
  {$ifdef MSWINDOWS}
    {$ifdef CONDITIONALEXPRESSIONS}
       {$if CompilerVersion < 18}
         {$define THREAD_FUNCS_EMULATE}
         {$define THREAD_FUNCS_WRAPPER}
       {$ifend}
    {$else}
       {$define THREAD_FUNCS_EMULATE}
       {$define THREAD_FUNCS_WRAPPER}
    {$endif}
  {$endif}
{$endif}

{$ifdef POSIX}
  {$define THREAD_FUNCS_WRAPPER}
{$endif}

{$ifdef THREAD_FUNCS_WRAPPER}
procedure _FpuInit;
{$WARNINGS OFF}
{$ifdef CPUX86}
  {$ifdef IOS}
  begin
    FSetExceptMask((DefaultMXCSR and $1F80) shr 7);
    Set8087CW(Default8087CW);
    SetMXCSR(DefaultMXCSR);
  end;
  {$else}
  const
    Default8087CW: Word = $1332;
  asm
    FNINIT
    FWAIT
    FLDCW Default8087CW
  end;
  {$endif}
{$else}
  {$ifdef CPUARM}
    const
      ValidMask = $07C0009F;
    begin
      SetFPSCR(DefaultFPSCR and ValidMask);
      FSetExceptMask(DefaultFPSCR);
    end;
  {$else .CPUX64}
     {$ifdef MSWINDOWS}
     asm
        LDMXCSR DefaultMXCSR
     end;
     {$endif}
  {$endif}
{$endif}
{$WARNINGS ON}

procedure RunErrorAt(ErrCode: Integer; ErrorAtAddr: Pointer);
begin
  ErrorAddr := ErrorAtAddr;
  Halt(ErrCode);
end;

procedure _UnhandledException;
type TExceptProc = procedure (Obj: TObject; Addr: Pointer);
begin
  {$ifdef CONDITIONALEXPRESSIONS}
  if Assigned(ExceptProc) then
    TExceptProc(ExceptProc)(ExceptObject, ExceptAddr)
  else
    RunErrorAt(230, ExceptAddr);
  {$else}
    RunErrorAt(230, ErrorAddr);
  {$endif}
end;

function __ThreadWrapper(Parameter: Pointer):
  {$ifdef MSWINDOWS}Integer; stdcall;{$else .POSIX}NativeInt; cdecl;{$endif}
var
  ThreadRec: TThreadRec;
begin
  Result := 0;
  try
    _FpuInit;
    ThreadRec := PThreadRec(Parameter)^;
    FreeMem(PThreadRec(Parameter));
    Result := ThreadRec.Func(ThreadRec.Parameter);
  except
    _UnhandledException;
  end;
end;
{$endif}


{$ifdef MSWINDOWS}
function BrainMMThreadFuncEvent(ThreadFunc: TThreadFunc; Parameter: Pointer): Pointer;
var
  Proxy, P: PThreadRec;
begin
  Proxy := Pointer(TGlobalStorage(nil^).InternalRecordPop);
  if (Proxy = nil) then
    {$ifdef CONDITIONALEXPRESSIONS}System.Error(reOutOfMemory){$else}System.RunError(203){$endif};

  Proxy.Func := ThreadFunc;
  Proxy.Parameter := Parameter;

  New(P);
  P.Func := BrainMMThreadProxy;
  P.Parameter := Proxy;

  Result := P;
end;
{$else .POSIX}
function posix_pthread_create(var Thread: pthread_t; Attr: Ppthread_attr_t;
  TFunc: Pointer; Arg: Pointer): Integer; cdecl;
  external libpthread name _PU + 'pthread_create';

function BrainMMThreadFuncEvent(Attribute: PThreadAttr;
    ThreadFunc: TThreadFunc; Parameter: Pointer;
    var ThreadId: NativeUInt): Integer;
var
  Proxy, P: PThreadRec;
begin
  Proxy := Pointer(TGlobalStorage(nil^).InternalRecordPop);
  if (Proxy = nil) then
    {$ifdef CONDITIONALEXPRESSIONS}System.Error(reOutOfMemory){$else}System.RunError(203){$endif};

  Proxy.Func := ThreadFunc;
  Proxy.Parameter := Parameter;

  New(P);
  P.Func := BrainMMThreadProxy;
  P.Parameter := Proxy;

  IsMultiThread := True;
  Result := posix_pthread_create(pthread_t(ThreadID), Ppthread_attr_t(Attribute),
    @__ThreadWrapper, P);

  if Result <> 0 then
    Dispose(P);
end;
{$endif}

{$ifdef THREAD_FUNCS_EMULATE} // CompilerVersion <= Delphi 2005
var
  SystemThreadFuncProc: function(ThreadFunc: TThreadFunc; Parameter: Pointer): Pointer;
  SystemThreadEndProc: procedure(ExitCode: Integer);

function __BeginThread(SecurityAttributes: Pointer; StackSize: LongWord;
  ThreadFunc: TThreadFunc; Parameter: Pointer; CreationFlags: LongWord;
  var ThreadId: NativeUInt): THandle;
var
  P: PThreadRec;
begin
  if Assigned(SystemThreadFuncProc) then
    P := PThreadRec(SystemThreadFuncProc(ThreadFunc, Parameter))
  else
  begin
    New(P);
    P.Func := ThreadFunc;
    P.Parameter := Parameter;
  end;

  IsMultiThread := TRUE;

  Result := CreateThread(SecurityAttributes, StackSize, @__ThreadWrapper, P,
    CreationFlags, ThreadID);

  if Result = 0 then
    Dispose(P);
end;

procedure __EndThread(ExitCode: Integer);
begin
  if Assigned(SystemThreadEndProc) then
    SystemThreadEndProc(ExitCode);
  ExitThread(ExitCode);
end;
{$endif}


 (* Global pages/blocks routine *)

function BrainMMGetMemoryBlock(BlockSize: TMemoryBlockSize;
  PagesMode: NativeUInt): MemoryBlock;
var
  GlobalStorage: PGlobalStorage;
begin
  // todo
  if (BlockSize <> BLOCK_64K) or (PagesMode = PAGESMODE_USER) then
  begin
    Result := nil;
    Exit;
  end;

  {$ifNdef BRAINMM_UNITTEST}
  Result := TGlobalStorage(nil^).K64BlockCachePop;
  if (Result = nil) then
  {$endif}
  begin
    {$ifdef MSWINDOWS}
      Result := VirtualAlloc(nil, SIZE_K64, MEM_COMMIT, PAGE_READWRITE);
    {$else .POSIX}
      Result := memalign(SIZE_K64, SIZE_K64);
    {$endif}

    {$ifdef BRAINMM_UNITTEST}
    if (Result <> nil) then
      FillRandomBytes(Result, SIZE_K64);
    {$endif}
  end;

  if (Result <> nil) then
  begin
    GlobalStorage := Pointer(NativeInt(@GLOBAL_STORAGE[63]) and MASK_64_CLEAR);
    AtomicIncrement(GlobalStorage.PagesAllocated, SIZE_K64 div SIZE_K4);

    if (PagesMode = PAGESMODE_JIT) then
      ChangeMemoryAccessRights(Result, 64 div 4, [marRead, marWrite, marExecute]);
  end;
end;

function BrainMMFreeMemoryBlock(Block: MemoryBlock; PagesMode: NativeUInt): Boolean;
var
  GlobalStorage: PGlobalStorage;
begin
  // todo
  if (PagesMode = PAGESMODE_USER) then
  begin
    Result := False;
    Exit;
  end;

  {$ifNdef BRAINMM_UNITTEST}
  Result := TGlobalStorage(nil^).K64BlockCachePush(Block);
  if (not Result) then
  {$endif}
  begin
    {$ifdef MSWINDOWS}
      Result := VirtualFree(Block, 0, MEM_RELEASE);
    {$else .POSIX}
      free(Block);
      Result := True;
    {$endif}
  end;

  if (Result) then
  begin
    GlobalStorage := Pointer(NativeInt(@GLOBAL_STORAGE[63]) and MASK_64_CLEAR);
    AtomicIncrement{AtomicDecrement}(GlobalStorage.PagesAllocated, NativeUInt(-NativeInt(SIZE_K64 div SIZE_K4)));
  end;
end;

function BrainMMGetMemoryPages(Count: NativeUInt; PagesMode: NativeUInt): MemoryPages;
var
  GlobalStorage: PGlobalStorage;
  K64Count: NativeUInt;
begin
  // todo
  if (PagesMode = PAGESMODE_USER) then
  begin
    Result := nil;
    Exit;
  end;

  K64Count := ((Count + 1) + 15) shr 4;
  {$ifdef MSWINDOWS}
    Result := VirtualAlloc(nil, K64Count * SIZE_K64, MEM_COMMIT, PAGE_READWRITE);
  {$else .POSIX}
    Result := memalign(SIZE_K4, K64Count * SIZE_K64);
  {$endif}
  if (Result <> nil) then
  begin
    GlobalStorage := Pointer(NativeInt(@GLOBAL_STORAGE[63]) and MASK_64_CLEAR);
    AtomicIncrement(GlobalStorage.PagesAllocated, Count);

    {$ifdef BRAINMM_UNITTEST}
    FillRandomBytes(Result, K64Count * SIZE_K64);
    {$endif}

    Inc(NativeInt(Result), SIZE_K4);
    PNativeUInt(NativeInt(Result) - SizeOf(NativeUInt))^ := Count;

    if (PagesMode = PAGESMODE_JIT) then
      ChangeMemoryAccessRights(Result, Count, [marRead, marWrite, marExecute])
  end;
end;

function BrainMMFreeMemoryPages(Pages: MemoryPages; PagesMode: NativeUInt): Boolean;
var
  Count: NativeUInt;
  GlobalStorage: PGlobalStorage;
begin
  // todo
  if (PagesMode = PAGESMODE_USER) then
  begin
    Result := False;
    Exit;
  end;

  Count := PNativeUInt(NativeInt(Pages) - SizeOf(NativeUInt))^;
  Dec(NativeInt(Pages), SIZE_K4);
  {$ifdef MSWINDOWS}
    Result := VirtualFree(Pages, 0, MEM_RELEASE);
  {$else .POSIX}
    free(Pages);
    Result := True;
  {$endif}

  if (Result) then
  begin
    GlobalStorage := Pointer(NativeInt(@GLOBAL_STORAGE[63]) and MASK_64_CLEAR);
    AtomicDecrement(GlobalStorage.PagesAllocated, Count);
  end;
end;

function BrainMMResizeMemoryPages(Pages: MemoryPages; NewCount: NativeUInt;
  ResizePagesFlags: NativeUInt): MemoryPages;
var
  K64Count: NativeUInt;
  LastCount, LastK64Count: NativeUInt;
  GlobalStorage: PGlobalStorage;
begin
  // todo
  if (ResizePagesFlags and PAGESMODE_SYSTEM = 0) then
  begin
    Result := nil;
    Exit;
  end;

  K64Count := ((NewCount + 1) + 15) shr 4;
  LastCount := PNativeUInt(NativeInt(Pages) - SizeOf(NativeUInt))^;
  LastK64Count := ((LastCount + 1) + 15) shr 4;

  if (LastK64Count = K64Count) then
  begin
    PNativeUInt(NativeInt(Pages) - SizeOf(NativeUInt))^ := NewCount;

    Dec(NewCount, LastCount);
    GlobalStorage := Pointer(NativeInt(@GLOBAL_STORAGE[63]) and MASK_64_CLEAR);
    AtomicIncrement(GlobalStorage.PagesAllocated, NewCount);

    Result := Pages;
  end else
  if (ResizePagesFlags and RESIZE_PAGES_COPY <> 0) then
  begin
    // Resize
    ResizePagesFlags := ResizePagesFlags and 1;

    if (LastCount > NewCount) then LastCount := NewCount;
    Result := BrainMMGetMemoryPages(NewCount, {PagesMode}ResizePagesFlags);
    if (Result <> nil) then
      NcMoveB16(P16(Pages)^, P16(Result)^, LastCount * B16_PER_PAGE);

    if (not BrainMMFreeMemoryPages(Pages, {PagesMode}ResizePagesFlags)) then
      {$ifdef CONDITIONALEXPRESSIONS}System.Error(reInvalidPtr){$else}System.RunError(204){$endif};
  end else
  begin
    // Reget
    ResizePagesFlags := ResizePagesFlags and 1;
    LastCount := NewCount;

    if (not BrainMMFreeMemoryPages(Pages, {PagesMode}ResizePagesFlags)) then
      {$ifdef CONDITIONALEXPRESSIONS}System.Error(reInvalidPtr){$else}System.RunError(204){$endif};

    Result := BrainMMGetMemoryPages({stored NewCount}LastCount, {PagesMode}ResizePagesFlags);
  end;
end;


{ TThreadHeap }

function TThreadHeap.ErrorOutOfMemory: Pointer;
{$ifNdef CONDITIONALEXPRESSIONS}
type
  TErrorProc = procedure(ErrorCode: Integer; ErrorAddr: Pointer);
{$endif}
begin
  if (ErrorAddr <> nil) then
  begin
    if Assigned(System.ErrorProc) then
    {$ifdef CONDITIONALEXPRESSIONS}
      System.ErrorProc(Byte(reOutOfMemory), ErrorAddr);
    {$else}
      TErrorProc(System.ErrorProc)(1, ErrorAddr);
    {$endif}
      
    System.ErrorAddr := ErrorAddr;
    if (System.ExitCode = 0) then System.ExitCode := 203{reOutOfMemory};
    System.Halt;
  end;

  Result := {failure}nil;
end;

function TThreadHeap.ErrorInvalidPtr: Integer;
{$ifNdef CONDITIONALEXPRESSIONS}
type
  TErrorProc = procedure(ErrorCode: Integer; ErrorAddr: Pointer);
{$endif}
begin
  if (ErrorAddr <> nil) then
  begin
    if Assigned(System.ErrorProc) then
    {$ifdef CONDITIONALEXPRESSIONS}
      System.ErrorProc(Byte(reInvalidPtr), ErrorAddr);
    {$else}
      TErrorProc(System.ErrorProc)(2, ErrorAddr);
    {$endif}      

    System.ErrorAddr := ErrorAddr;
    if (System.ExitCode = 0) then System.ExitCode := 204{reInvalidPtr};
    System.Halt;
  end;

  Result := {failure}FREEMEM_INVALID;
end;

function TThreadHeap.RaiseOutOfMemory: Pointer;
{$ifdef CPUINTEL}
asm
  {$ifdef CPUX86}
     mov edx, [esp]
     cmp [EAX].TThreadHeap.ErrorAddr, 0
     jnz TThreadHeap.ErrorOutOfMemory
     mov [EAX].TThreadHeap.ErrorAddr, edx
  {$else .CPUX64}
     mov rdx, [rsp]
     cmp [RCX].TThreadHeap.ErrorAddr, 0
     jnz TThreadHeap.ErrorOutOfMemory
     mov [RCX].TThreadHeap.ErrorAddr, rdx
  {$endif}
  jmp TThreadHeap.ErrorOutOfMemory
end;
{$else}
{$ifNdef RETURNADDRESS}
const
  ReturnAddress: Pointer = @TThreadHeap.RaiseOutOfMemory;
{$endif}
begin
  if (Self.ErrorAddr = nil) then Self.ErrorAddr := ReturnAddress;
  Result := Self.ErrorOutOfMemory;
end;
{$endif}

function TThreadHeap.RaiseInvalidPtr: Integer;
{$ifdef CPUINTEL}
asm
  {$ifdef CPUX86}
     mov edx, [esp]
     cmp [EAX].TThreadHeap.ErrorAddr, 0
     jnz TThreadHeap.ErrorInvalidPtr
     mov [EAX].TThreadHeap.ErrorAddr, edx
  {$else .CPUX64}
     mov rdx, [rsp]
     cmp [RCX].TThreadHeap.ErrorAddr, 0
     jnz TThreadHeap.ErrorInvalidPtr
     mov [RCX].TThreadHeap.ErrorAddr, rdx
  {$endif}
  jmp TThreadHeap.ErrorInvalidPtr
end;
{$else}
{$ifNdef RETURNADDRESS}
const
  ReturnAddress: Pointer = @TThreadHeap.RaiseInvalidPtr;
{$endif}
begin
  if (Self.ErrorAddr = nil) then Self.ErrorAddr := ReturnAddress;
  Result := Self.ErrorInvalidPtr;
end;
{$endif}

type
  PThreadDeferred = ^TThreadDeferred;
  TThreadDeferred = packed record
    Next: PThreadDeferred;
    {$ifdef LARGEINT}
      SupposedReturnAddress: SupposedPtr{high bit: IsSmall};
    {$else .SMALLINT}
      ReturnAddress: Pointer;
      IsSmall: Boolean;
    {$endif}
  end;

procedure TThreadHeap.PushThreadDeferred(P: Pointer; ReturnAddress: Pointer;
  IsSmall: Boolean);
label
  lock_free;
var
  Flags: SupposedPtr;
  LastErrorAddr: Pointer;
begin
  if (Self.LockFlags = 0) then
  begin
  lock_free:
    {$ifdef LARGEINT}
      PThreadDeferred(P).SupposedReturnAddress := SupposedPtr(ReturnAddress) +
        (SupposedPtr(IsSmall) shl HIGH_NATIVE_BIT);
    {$else .SMALLINT}
      PThreadDeferred(P).ReturnAddress := ReturnAddress;
      PThreadDeferred(P).IsSmall := IsSmall;
    {$endif}
    Deferreds.Push(P);
  end else
  begin
    // inline SpinLock
    repeat
      Flags := Self.LockFlags;
      if (Flags = 0) then goto lock_free;
      if (Flags <> THREAD_HEAP_LOCKABLE) then
      begin
        if (Flags = THREAD_HEAP_LOCKED) then
        begin
          SpinWait(Self.LockFlags, THREAD_HEAP_LOCKED_BIT);
        end else
        begin
          {$ifdef CONDITIONALEXPRESSIONS}System.Error(reInvalidCast){$else}System.RunError(219){$endif};
        end;
      end;
    until (THREAD_HEAP_LOCKABLE = AtomicCmpExchange(Self.LockFlags, THREAD_HEAP_LOCKED, THREAD_HEAP_LOCKABLE));
    LastErrorAddr := Self.ErrorAddr;
    try
      Self.ErrorAddr := ReturnAddress;
      if (IsSmall) then
      begin
        Self.FreeSmall(P);
      end else
      begin
        Self.FMedium.Free(P);
      end;
    finally
      Self.ErrorAddr := LastErrorAddr;
      Self.LockFlags := THREAD_HEAP_LOCKABLE; // inline SpinUnlock
    end;
  end;
end;

procedure TThreadHeap.ProcessThreadDeferred;
var
  LastErrorAddr: Pointer;
  ReturnAddress: Pointer;
  Counter: NativeUInt;
  ThreadDeferred, Next: PThreadDeferred;
begin
  ThreadDeferred := Deferreds.PopList;
  if (ThreadDeferred <> nil) then
  begin
    LastErrorAddr := Self.ErrorAddr;
    try
      // check duplicates
      Next := ThreadDeferred.Next;
      if (Next <> nil) then
      begin
        Counter := 0;
        repeat
          if (NativeInt(Next) and MASK_16_TEST <> 0) or (ThreadDeferred = Next) then
          begin
            {$ifdef LARGEINT}
              Self.ErrorAddr := Pointer(NativeInt(ThreadDeferred.SupposedReturnAddress) and MASK_HIGH_NATIVE_TEST);
            {$else .SMALLINT}
              Self.ErrorAddr := ThreadDeferred.ReturnAddress;
            {$endif}
            Self.RaiseInvalidPtr;
          end;
          Next := Next.Next;
          Inc(Counter);
        until ((Next = nil) or (Counter = 16));
      end;

      // free small/medium 
      repeat
        Next := ThreadDeferred.Next;
        ReturnAddress := Pointer(ThreadDeferred.{$ifdef LARGEINT}SupposedReturnAddress{$else}ReturnAddress{$endif});

        {$ifdef LARGEINT}
          Self.ErrorAddr := Pointer(NativeInt(ReturnAddress) and MASK_HIGH_NATIVE_TEST);
        {$else .SMALLINT}
          Self.ErrorAddr := ReturnAddress;
        {$endif}

        if ({$ifdef LARGEINT}NativeInt(ReturnAddress) < 0{$else}ThreadDeferred.IsSmall{$endif}) then
        begin
          Self.FreeSmall(ThreadDeferred);
        end else
        begin
          Self.FMedium.Free(ThreadDeferred);
        end;

        ThreadDeferred := Next;
      until (ThreadDeferred = nil);
    finally
      Self.ErrorAddr := LastErrorAddr;
    end;
  end;
end;


function BrainMMUnknownGetMem(None: Pointer; B16Count: NativeUInt;
  ErrorAddr: Pointer): Pointer;
var
  ThreadHeap: PThreadHeap;
  Flags: SupposedPtr;
begin
  // inline SpinLock
  ThreadHeap := UnknownThreadHeap;
  repeat
    Flags := ThreadHeap.LockFlags;
    if (Flags <> THREAD_HEAP_LOCKABLE) then
    begin
      if (Flags = THREAD_HEAP_LOCKED) then
      begin
        SpinWait(ThreadHeap.LockFlags, THREAD_HEAP_LOCKED_BIT);
      end else
      begin
        {$ifdef CONDITIONALEXPRESSIONS}System.Error(reInvalidCast){$else}System.RunError(219){$endif};
      end;
    end;
  until (THREAD_HEAP_LOCKABLE = AtomicCmpExchange(ThreadHeap.LockFlags, THREAD_HEAP_LOCKED, THREAD_HEAP_LOCKABLE));
  try
    // allocation
    ThreadHeap.ErrorAddr := ErrorAddr;
    case (B16Count) of
      0..MAX_SMALL_B16COUNT: Result := ThreadHeap.GetSmall(B16Count);
      MAX_SMALL_B16COUNT+1..MIDDLE_MEDIUM_B16COUNT:
        Result := ThreadHeap.FMedium.Get(B16Count);
      MIDDLE_MEDIUM_B16COUNT+1..MAX_MEDIUM_B16COUNT:
        Result := ThreadHeap.FMedium.GetAdvanced(B16Count, Ord(ma16Bytes));
    else
      Result := BrainMMGetMemoryPages(
        (B16Count + (B16_PER_PAGE - 1)) shr B16_PER_PAGE_SHIFT,
        PAGESMODE_SYSTEM);
      if (Result = nil) then
        Result := ThreadHeap.ErrorOutOfMemory;
    end;
  finally
    // inline SpinUnlock
    ThreadHeap.LockFlags := THREAD_HEAP_LOCKABLE;
  end;
end;

function BrainMMGetMem(Size: NativeUInt): Pointer;
{$ifdef PUREPASCAL}
label
  thread_deferreds_done;
const
  ErrorAddr = nil;
var
  ThreadHeap: PThreadHeap;
  B16Count: NativeUInt;
begin
  B16Count := (Size + 15) shr 4;
  ThreadHeap := ThreadHeapInstance;
  if (ThreadHeap <> nil) then
  begin
    ThreadHeap.ErrorAddr := ErrorAddr;

    if (not ThreadHeap.Deferreds.Assigned) then
    begin
    thread_deferreds_done:
      case (B16Count) of
        0..MAX_SMALL_B16COUNT: Result := ThreadHeap.GetSmall(B16Count);
        MAX_SMALL_B16COUNT+1..MIDDLE_MEDIUM_B16COUNT:
          Result := ThreadHeap.FMedium.Get(B16Count);
        MIDDLE_MEDIUM_B16COUNT+1..MAX_MEDIUM_B16COUNT:
          Result := ThreadHeap.FMedium.GetAdvanced(B16Count, Ord(ma16Bytes));
      else
        Result := BrainMMGetMemoryPages(
          (B16Count + (B16_PER_PAGE - 1)) shr B16_PER_PAGE_SHIFT,
          PAGESMODE_SYSTEM);
        { if (Result = nil) then
          Result := ThreadHeap.ErrorOutOfMemory; }
      end;
    end else
    begin
      ThreadHeap.ProcessThreadDeferred;
      goto thread_deferreds_done;
    end;
  end else
  begin
    Result := BrainMMUnknownGetMem(nil, B16Count, ErrorAddr);
  end;
end;
{$else}
asm
  // ErrorAddr(v3) := nil
  {$ifdef CPUX86}
    xor ecx, ecx
    nop
    nop
  {$else .CPUX64}
    xor r8, r8
    nop
  {$endif}
@redirect:
  // v2 := (Size(v1) + 15) div 16
  {$ifdef CPUX86}
    lea edx, [eax + 15]
    shr edx, 4
  {$else .CPUX64}
    lea rdx, [rcx + 15]
    shr rdx, 4
  {$endif}

  // v1 := actual ThreadHeapInstance
  {$ifdef CPUX86}
    mov eax, fs:[THREAD_HEAP]
    test eax, eax
  {$else .CPUX64}
    mov rcx, gs:[abs THREAD_HEAP]
    test rcx, rcx
  {$endif}
  jz BrainMMUnknownGetMem
  // ThreadHeap(v1).ErrorAddr := ErrorAddr(v3)
  {$ifdef CPUX86}
    mov [EAX].TThreadHeap.ErrorAddr, ecx
  {$else .CPUX64}
    mov [RCX].TThreadHeap.ErrorAddr, r8
  {$endif}

  // if (ThreadHeap.Deferreds.Assigned) then ThreadHeap(v1).ProcessThreadDeferred;
  {$ifdef CPUX86}
    cmp [EAX].TThreadHeap.Deferreds.F.Handle, 0
  {$else .CPUX64}
    mov r8, X64_SYNCSTACK_MASK
    test [RCX].TThreadHeap.Deferreds.F.Handle, r8
  {$endif}
  jnz @penalty_thread_deferreds
@thread_deferreds_done:

  // case (B16Count) of
  //   Exit ThreadHeap.GetSmall(B16Count)
  //   Exit ThreadHeap.FMedium.Get(B16Count, Ord(ma16Bytes))
  //   Exit ThreadHeap.FMedium.GetAdvanced(B16Count, Ord(ma16Bytes))
  //   Exit BrainMM.etMemoryPages(PagesOf(B16Count), PAGESMODE_SYSTEM)
  {$ifdef CPUX86}
    xor ecx, ecx
    cmp edx, MAX_SMALL_B16COUNT
    jbe TThreadHeap.GetSmall
    lea eax, [EAX].TThreadHeap.FMedium
    cmp edx, MIDDLE_MEDIUM_B16COUNT
    jbe TMediumManager.Get
    cmp edx, MAX_MEDIUM_B16COUNT
    jbe TMediumManager.GetAdvanced
  {$else .CPUX64}
    xor r8, r8
    cmp rdx, MAX_SMALL_B16COUNT
    jbe TThreadHeap.GetSmall
    lea rcx, [RCX].TThreadHeap.FMedium
    cmp rdx, MIDDLE_MEDIUM_B16COUNT
    jbe TMediumManager.Get
    cmp rdx, MAX_MEDIUM_B16COUNT
    jbe TMediumManager.GetAdvanced
  {$endif}

  {$ifdef CPUX86}
    lea eax, [edx + B16_PER_PAGE - 1]
    mov edx, PAGESMODE_SYSTEM
    shr eax, B16_PER_PAGE_SHIFT
  {$else .CPUX64}
    lea rcx, [rdx + B16_PER_PAGE - 1]
    mov edx, PAGESMODE_SYSTEM
    shr rcx, B16_PER_PAGE_SHIFT
  {$endif}
  call BrainMMGetMemoryPages
  {$ifdef CPUX86}
    test eax, eax
  {$else .CPUX64}
    test rax, rax
  {$endif}
  jz @error_outof_memory
  ret

@error_outof_memory:
  {$ifdef CPUX86}
    mov eax, fs:[THREAD_HEAP]
  {$else .CPUX64}
    mov rcx, gs:[abs THREAD_HEAP]
  {$endif}
  jmp TThreadHeap.ErrorOutOfMemory

@penalty_thread_deferreds:
  {$ifdef CPUX86}
    push ebx
    lea ebx, @x86_thread_deferreds_ret
    jmp SafeProcessThreadDeferred
  @x86_thread_deferreds_ret:
    pop ebx
    jmp @thread_deferreds_done
  {$else .CPUX64}
    lea r9, @thread_deferreds_done
    jmp SafeProcessThreadDeferred
  {$endif}
end;
{$endif}

function TThreadHeap.GetSmall(B16Count: NativeUInt): Pointer;
{$ifdef PUREPASCAL}
label
  item_reserve, new_k1line;
var
  Line, Next, N: PK1LineSmall;
  Index: NativeInt;
begin
  Line := Self.FK1LineSmalls[B16Count];

  if (Line <> nil) then
  begin
    if (Line.Header.ItemSet.VLow32 and 3 = 0{not FullQueue}) then
    begin
    item_reserve:
      Index := BitReserve(Line.Header.ItemSet);
      if (Index > 0) then
      begin
        Result := @Line.Items[Index];
        Exit;
      end else
      begin
        repeat
          Line.Header.ItemSet.VLow32 := 3{FullQueue := True};

          // Dequeue
          Next := Line.Header.Queue.Next;
          Self.FK1LineSmalls[B16Count] := Next;

          // Enqueue
          N := Self.QK1LineFull;
          Self.QK1LineFull := Line;
          Line.Header.Queue.Prev := nil;
          Line.Header.Queue.Next := N;
          if (N <> nil) then N.Header.Queue.Prev := Line;

          Line := Next;
          if (Next = nil) then goto new_k1line;
          Next.Header.Queue.Prev := nil;
        until (
          {$ifdef SMALLINT}
            Line.Header.ItemSet.VLow32 or Line.Header.ItemSet.VHigh32 <> 0
          {$else .LARGEINT}
            Line.Header.ItemSet.V64 <> 0
          {$endif}
          );
        goto item_reserve;
      end;
    end else
    begin
      Result := Pointer(Self.RaiseInvalidPtr);
      Exit;
    end;
  end else
  begin
  new_k1line:
    Result := Self.GetNewK1LineSmall(B16Count);
  end;
end;
{$else}
asm
  // Line := FK1LineSmalls[B16Count]
  // if (Line = nil) then Exit Self.GetNewK1LineSmall(B16Count)
  {$ifdef CPUX86}
    mov ecx, [EAX + edx * 4]
    test ecx, ecx
  {$else .CPUX64}
    mov r8, [RCX + rdx * 8]
    test r8, r8
  {$endif}
  jz TThreadHeap.GetNewK1LineSmall

  // if (Line.FullQueue) then Exit RaiseInvalidPtr;
  {$ifdef CPUX86}
    mov edx, [ECX].TK1LineSmall.Header.ItemSet.VLow32
    test edx, 3
  {$else .CPUX64}
    mov r9, [R8].TK1LineSmall.Header.ItemSet.V64
    test r9, 3
  {$endif}
  jnz TThreadHeap.RaiseInvalidPtr

  // if (Line.Header.ItemSet.VInt64 <> 0{deferred Full}) then Index := BitReserve(Line.ItemSet)
  // else RequeueLine(FK1LineSmalls, QK1LineFull), FindNextLine;
@item_reserve:
  {$ifdef CPUX86}
    lea eax, [ECX].TK1LineSmall.Header.ItemSet.VHigh32
    test edx, edx
    DB $0F, $44, $C8 // cmovz ecx, eax
    mov edx, [ecx]
    rep bsf eax, edx
    test edx, edx
    jz @requeue_line_loop_prefix
    btr edx, eax
    mov [ecx], edx
    lea edx, [eax + 32]
    test ecx, MASK_K1_TEST
    DB $0F, $45, $C2 // cmovnz eax, edx
  {$else .CPUX64}
    rep bsf rax, r9
    test r9, r9
    jz @requeue_line_loop_prefix
    btr r9, rax
    mov [R8].TK1LineSmall.Header.ItemSet.V64, r9
  {$endif}

  // Result := @Line.Items[Index]
  {$ifdef CPUX86}
    add eax, eax
    and ecx, MASK_K1_CLEAR
    lea eax, [ecx + eax * 8]
  {$else .CPUX64}
    add rax, rax
    lea rax, [r8 + rax * 8]
  {$endif}
  ret
@requeue_line_loop_prefix:
{$ifdef CPUX86}
  // store ebx/esi, retrieve Line, ThreadHeap, B16Count
  mov [esp - 8], ebx
  mov [esp - 4], esi
  and ecx, MASK_K1_CLEAR
  movzx edx, byte ptr [ECX].TK1LineSmall.Header.Flags
  shr edx, 4
  mov eax, MASK_K64_CLEAR
  and eax, ecx
  mov eax, [EAX].TK64PoolSmall.ThreadHeap
{$endif}
@requeue_line_loop:
  // mark as FullQueue
  {$ifdef CPUX86}
    mov [ECX].TK1LineSmall.Header.ItemSet.VLow32, 3
  {$else .CPUX64}
    mov [R8].TK1LineSmall.Header.ItemSet.VLow32, 3
  {$endif}

  // dequeue, enqueue (during optional fake next LineFull)
  {$ifdef CPUX86}
    mov ebx, [ECX].TK1LineSmall.Header.Queue.Next
    mov esi, [EAX].TThreadHeap.QK1LineFull
    mov [EAX + edx * 4], ebx
    mov [EAX].TThreadHeap.QK1LineFull, ecx

    mov [ECX].TK1LineSmall.Header.Queue.Prev, 0
    mov [ECX].TK1LineSmall.Header.Queue.Next, esi
    lea esp, [esp - 64]
    test esi, esi
    DB $0F, $44, $F4 // cmovz esi, esp
    lea esp, [esp + 64]
    mov [ESI].TK1LineSmall.Header.Queue.Prev, ecx
  {$else .CPUX64}
    mov r9, [R8].TK1LineSmall.Header.Queue.Next
    mov r10, [RCX].TThreadHeap.QK1LineFull
    mov [RCX + rdx * 8], r9
    mov [RCX].TThreadHeap.QK1LineFull, R8

    xor rax, rax
    mov [R8].TK1LineSmall.Header.Queue.Prev, rax
    mov [R8].TK1LineSmall.Header.Queue.Next, r10
    lea rax, [rsp - 64]
    test r10, r10
    cmovz r10, rax
    mov [R10].TK1LineSmall.Header.Queue.Prev, r8
  {$endif}

  // Line := Next;
  // if (Line = nil) then Exit Self.GetNewK1LineSmall(B16Count);
  {$ifdef CPUX86}
    test ebx, ebx
    xchg ecx, ebx
    mov ebx, [esp - 8]
    mov esi, [esp - 4]
  {$else .CPUX64}
    test r9, r9
    xchg r8, r9
  {$endif}
  jz TThreadHeap.GetNewK1LineSmall

  // Line.Header.Queue.Prev := nil;
  // if (Line.Header.ItemSet.V64 = 0) then Continue;
  {$ifdef CPUX86}
    mov [ECX].TK1LineSmall.Header.Queue.Prev, 0
    mov ebx, [ECX].TK1LineSmall.Header.ItemSet.VLow32
    mov esi, [ECX].TK1LineSmall.Header.ItemSet.VHigh32
    or esi, ebx
  {$else .CPUX64}
    xor rax, rax
    mov [R8].TK1LineSmall.Header.Queue.Prev, rax
    mov r9, [R8].TK1LineSmall.Header.ItemSet.V64
    test r9, r9
  {$endif}
  jz @requeue_line_loop

  // if (Line.FullQueue) then Exit RaiseInvalidPtr;
  // goto @item_reserve;
  {$ifdef CPUX86}
    test ebx, 3
    xchg edx, ebx
    mov ebx, [esp - 8]
    mov esi, [esp - 4]
  {$else .CPUX64}
    test r9, 3
  {$endif}
  jz @item_reserve
  jmp TThreadHeap.RaiseInvalidPtr
end;
{$endif}

function BrainMMUnknownAllocMem(None: Pointer; B16Count: NativeUInt;
  ErrorAddr: Pointer): Pointer;
begin
  Result := BrainMMUnknownGetMem(None, B16Count, ErrorAddr);
  if (Result <> nil) then
    System.FillChar(Result^, B16Count shl 4, #0);
end;

function BrainMMAllocMem(Size: NativeUInt): Pointer;
{$ifdef PUREPASCAL}
label
  thread_deferreds_done;
const
  ErrorAddr = nil;
var
  ThreadHeap: PThreadHeap;
  B16Count: NativeUInt;
begin
  B16Count := (Size + 15) shr 4;
  ThreadHeap := ThreadHeapInstance;
  if (ThreadHeap <> nil) then
  begin
    ThreadHeap.ErrorAddr := ErrorAddr;

    if (not ThreadHeap.Deferreds.Assigned) then
    begin
    thread_deferreds_done:
      case (B16Count) of
        0..MAX_SMALL_B16COUNT: Result := ThreadHeap.GetSmall(B16Count);
        MAX_SMALL_B16COUNT+1..MIDDLE_MEDIUM_B16COUNT:
          Result := ThreadHeap.FMedium.Get(B16Count);
        MIDDLE_MEDIUM_B16COUNT+1..MAX_MEDIUM_B16COUNT:
          Result := ThreadHeap.FMedium.GetAdvanced(B16Count, Ord(ma16Bytes));
      else
        Result := BrainMMGetMemoryPages(
          (B16Count + (B16_PER_PAGE - 1)) shr B16_PER_PAGE_SHIFT,
          PAGESMODE_SYSTEM);
        { if (Result = nil) then
          Result := ThreadHeap.ErrorOutOfMemory; }
      end;

      if (Result <> nil) then
        System.FillChar(Result^, B16Count shl 4, #0);
    end else
    begin
      ThreadHeap.ProcessThreadDeferred;
      goto thread_deferreds_done;
    end;
  end else
  begin
    Result := BrainMMUnknownAllocMem(nil, B16Count, ErrorAddr);
  end;
end;
{$else}
asm
  // ErrorAddr(v3) := nil
  {$ifdef CPUX86}
    xor ecx, ecx
    nop
    nop
  {$else .CPUX64}
    xor r8, r8
    nop
  {$endif}
@redirect:
  // v2 := (Size(v1) + 15) div 16
  {$ifdef CPUX86}
    lea edx, [eax + 15]
    shr edx, 4
  {$else .CPUX64}
    lea rdx, [rcx + 15]
    shr rdx, 4
  {$endif}

  // v1 := actual ThreadHeapInstance
  {$ifdef CPUX86}
    mov eax, fs:[THREAD_HEAP]
    test eax, eax
  {$else .CPUX64}
    mov rcx, gs:[abs THREAD_HEAP]
    test rcx, rcx
  {$endif}
  jz BrainMMUnknownAllocMem

  // ThreadHeap(v1).ErrorAddr := ErrorAddr(v3)
  {$ifdef CPUX86}
    mov [EAX].TThreadHeap.ErrorAddr, ecx
  {$else .CPUX64}
    mov [RCX].TThreadHeap.ErrorAddr, r8
  {$endif}

  // if (ThreadHeap.Deferreds.Assigned) then ThreadHeap(v1).ProcessThreadDeferred;
  {$ifdef CPUX86}
    cmp [EAX].TThreadHeap.Deferreds.F.Handle, 0
  {$else .CPUX64}
    mov r8, X64_SYNCSTACK_MASK
    test [RCX].TThreadHeap.Deferreds.F.Handle, r8
  {$endif}
  jnz @penalty_thread_deferreds
@thread_deferreds_done:

  // store B16Count
  // case (B16Count) of
  //   call ThreadHeap.GetSmall(B16Count)
  //   call ThreadHeap.FMedium.Get(B16Count, Ord(ma16Bytes))
  //   call ThreadHeap.FMedium.GetAdvanced(B16Count, Ord(ma16Bytes))
  //   call BrainMMGetMemoryPages(PagesOf(B16Count), PAGESMODE_SYSTEM)
  {$ifdef CPUX86}
    push edx
    push offset @fill_zero
    xor ecx, ecx
    cmp edx, MAX_SMALL_B16COUNT
    jbe TThreadHeap.GetSmall
    lea eax, [EAX].TThreadHeap.FMedium
    cmp edx, MIDDLE_MEDIUM_B16COUNT
    jbe TMediumManager.Get
    cmp edx, MAX_MEDIUM_B16COUNT
    jbe TMediumManager.GetAdvanced
  {$else .CPUX64}
    push rdx
    lea r9, @fill_zero
    push r9
    xor r8, r8
    cmp rdx, MAX_SMALL_B16COUNT
    jbe TThreadHeap.GetSmall
    lea rcx, [RCX].TThreadHeap.FMedium
    cmp rdx, MIDDLE_MEDIUM_B16COUNT
    jbe TMediumManager.Get
    cmp rdx, MAX_MEDIUM_B16COUNT
    jbe TMediumManager.GetAdvanced
  {$endif}

  {$ifdef CPUX86}
    lea eax, [edx + B16_PER_PAGE - 1]
    mov edx, PAGESMODE_SYSTEM
    shr eax, B16_PER_PAGE_SHIFT
    pop ecx
  {$else .CPUX64}
    lea rcx, [rdx + B16_PER_PAGE - 1]
    mov edx, PAGESMODE_SYSTEM
    shr rcx, B16_PER_PAGE_SHIFT
    pop r8
  {$endif}
  call BrainMMGetMemoryPages
  {$ifdef CPUX86}
    test eax, eax
  {$else .CPUX64}
    test rax, rax
  {$endif}
  jnz @fill_zero
  {$ifdef CPUX86}
    mov eax, fs:[THREAD_HEAP]
  {$else .CPUX64}
    mov rcx, gs:[abs THREAD_HEAP]
  {$endif}
  call TThreadHeap.ErrorOutOfMemory

@fill_zero:
  // retrieve B16Count
  // if (Result = nil) then Exit
  {$ifdef CPUX86}
    pop ecx
    test eax, eax
  {$else .CPUX64}
    pop rcx
    test rax, rax
  {$endif}
  jz @done

  // FillChar(Result^, B16Count * 16, #0);
  {$ifdef CPUX86}
    mov edx, edi
    xchg eax, edi
    shl ecx, 2
    xor eax, eax
    rep STOSD
    mov ecx, [esp - 4]
  {$else .CPUX64}
    mov r8, rcx
    mov rdx, rdi
    xchg rax, rdi
    add rcx, rcx
    xor rax, rax
    rep STOSQ
  {$endif}

  // Result := edi/rdi - B16Count * 16, retrieve edi/rdi (from edx/rdx)
  {$ifdef CPUX86}
    shl ecx, 4
    xchg eax, edx
    sub edi, ecx
    xchg eax, edi
  {$else .CPUX64}
    shl r8, 4
    xchg rax, rdx
    sub rdi, r8
    xchg rax, rdi
  {$endif}

@done:
  ret

@penalty_thread_deferreds:
  {$ifdef CPUX86}
    push ebx
    lea ebx, @x86_thread_deferreds_ret
    jmp SafeProcessThreadDeferred
  @x86_thread_deferreds_ret:
    pop ebx
    jmp @thread_deferreds_done
  {$else .CPUX64}
    lea r9, @thread_deferreds_done
    jmp SafeProcessThreadDeferred
  {$endif}
end;
{$endif}

function BrainMMUnknownGetMemAligned(Align: TMemoryAlign; B16Count: NativeUInt;
  ErrorAddr: Pointer): Pointer;
label
  medium_advanced;
var
  ThreadHeap: PThreadHeap;
  Flags: SupposedPtr;
begin
  // inline SpinLock
  ThreadHeap := UnknownThreadHeap;
  repeat
    Flags := ThreadHeap.LockFlags;
    if (Flags <> THREAD_HEAP_LOCKABLE) then
    begin
      if (Flags = THREAD_HEAP_LOCKED) then
      begin
        SpinWait(ThreadHeap.LockFlags, THREAD_HEAP_LOCKED_BIT);
      end else
      begin
        {$ifdef CONDITIONALEXPRESSIONS}System.Error(reInvalidCast){$else}System.RunError(219){$endif};
      end;
    end;
  until (THREAD_HEAP_LOCKABLE = AtomicCmpExchange(ThreadHeap.LockFlags, THREAD_HEAP_LOCKED, THREAD_HEAP_LOCKABLE));
  try
    // allocation
    ThreadHeap.ErrorAddr := ErrorAddr;
    case (B16Count) of
      0..MAX_SMALL_B16COUNT:
      begin
        if (Align <> ma16Bytes) then goto medium_advanced;
        Result := ThreadHeap.GetSmall(B16Count);
      end;
      MAX_SMALL_B16COUNT+1..MIDDLE_MEDIUM_B16COUNT:
      begin
        if (Align <> ma16Bytes) then goto medium_advanced;
        Result := ThreadHeap.FMedium.Get(B16Count);
      end;
      MIDDLE_MEDIUM_B16COUNT+1..MAX_MEDIUM_B16COUNT:
      begin
      medium_advanced:
        Result := ThreadHeap.FMedium.GetAdvanced(B16Count, Ord(Align));
      end;
    else
      Result := BrainMMGetMemoryPages(
        (B16Count + (B16_PER_PAGE - 1)) shr B16_PER_PAGE_SHIFT,
        PAGESMODE_SYSTEM);
      if (Result = nil) then
        Result := ThreadHeap.ErrorOutOfMemory;
    end;
  finally
    // inline SpinUnlock
    ThreadHeap.LockFlags := THREAD_HEAP_LOCKABLE;
  end;
end;

function BrainMMGetMemAligned(Align: TMemoryAlign; Size: NativeUInt): Pointer;
label
  thread_deferreds_done, medium_advanced;
const
  ErrorAddr = nil;
var
  ThreadHeap: PThreadHeap;
  B16Count: NativeUInt;
begin
  B16Count := (Size + 15) shr 4;
  ThreadHeap := ThreadHeapInstance;
  if (ThreadHeap <> nil) then
  begin
    ThreadHeap.ErrorAddr := ErrorAddr;

    if (not ThreadHeap.Deferreds.Assigned) then
    begin
    thread_deferreds_done:
      case (B16Count) of
        0..MAX_SMALL_B16COUNT:
        begin
          if (Align <> ma16Bytes) then goto medium_advanced;
          Result := ThreadHeap.GetSmall(B16Count);
        end;
        MAX_SMALL_B16COUNT+1..MIDDLE_MEDIUM_B16COUNT:
        begin
          if (Align <> ma16Bytes) then goto medium_advanced;
          Result := ThreadHeap.FMedium.Get(B16Count);
        end;
        MIDDLE_MEDIUM_B16COUNT+1..MAX_MEDIUM_B16COUNT:
        begin
        medium_advanced:
          Result := ThreadHeap.FMedium.GetAdvanced(B16Count, Ord(Align));
        end;
      else
        Result := BrainMMGetMemoryPages(
          (B16Count + (B16_PER_PAGE - 1)) shr B16_PER_PAGE_SHIFT,
          PAGESMODE_SYSTEM);
        { if (Result = nil) then
          Result := ThreadHeap.ErrorOutOfMemory; }
      end;
    end else
    begin
      ThreadHeap.ProcessThreadDeferred;
      goto thread_deferreds_done;
    end;
  end else
  begin
    Result := BrainMMUnknownGetMemAligned(Align, B16Count, ErrorAddr);
  end;
end;

function BrainMMUnknownFreeMem(None: Pointer; P: Pointer;
  ErrorAddr: Pointer): Integer;
label
  medium, error_invalid_ptr;
var
  ThreadHeap: PThreadHeap;
  Flags: SupposedPtr;
  Pool: Pointer{PK64PoolSmall/PK64PoolMedium};
begin
  // inline SpinLock
  ThreadHeap := UnknownThreadHeap;
  repeat
    Flags := ThreadHeap.LockFlags;
    if (Flags <> THREAD_HEAP_LOCKABLE) then
    begin
      if (Flags = THREAD_HEAP_LOCKED) then
      begin
        SpinWait(ThreadHeap.LockFlags, THREAD_HEAP_LOCKED_BIT);
      end else
      begin
        {$ifdef CONDITIONALEXPRESSIONS}System.Error(reInvalidCast){$else}System.RunError(219){$endif};
      end;
    end;
  until (THREAD_HEAP_LOCKABLE = AtomicCmpExchange(ThreadHeap.LockFlags, THREAD_HEAP_LOCKED, THREAD_HEAP_LOCKABLE));
  try
    // dispose
    ThreadHeap.ErrorAddr := ErrorAddr;
    if (NativeInt(P) and MASK_16_TEST = 0) then
    begin
      Pool := Pointer(NativeInt(P) and MASK_K64_CLEAR);

      if (NativeInt(P) and MASK_K1_TEST <> 0) then
      begin
        // pool: small or medium
        if (PK64PoolSmall(Pool).ThreadHeap = ThreadHeap) then
        begin
          // pool small
          Result := ThreadHeap.FreeSmall(P);
          Exit;
        end else
        if (PK64PoolSmall(Pool).ThreadHeap = nil) then
        begin
          // pool medium
        medium:
          if (PK64PoolMedium(Pool).ThreadHeap = ThreadHeap) then
          begin
            Result := ThreadHeap.FMedium.Free(P);
            Exit;
          end;
        end;
      end else
      begin
        if (NativeInt(P) and MASK_K4_TEST <> 0) then
        begin
          // medium or invalid pointer
          if (PK64PoolSmall(Pool).ThreadHeap = nil) then goto medium;
          goto error_invalid_ptr;
        end else
        begin
          // big or large
          if (not BrainMMFreeMemoryPages(MemoryPages(P), PAGESMODE_SYSTEM)) then
            goto error_invalid_ptr;

          Result := FREEMEM_DONE;
          Exit;
        end;
      end;

      // "default" method
      Result := ThreadHeap.FreeDifficult(P, ErrorAddr);
      Exit;
    end else
    begin
    error_invalid_ptr:
      Result := ThreadHeap.ErrorInvalidPtr;
    end;
  finally
    // inline SpinUnlock
    ThreadHeap.LockFlags := THREAD_HEAP_LOCKABLE;
  end;
end;

function BrainMMFreeMem(P: Pointer): Integer;
{$ifdef PUREPASCAL}
{$ifNdef RETURNADDRESS}
const
  ReturnAddress: Pointer = @BrainMMFreeMem;
{$endif}
label
  thread_deferreds_done, medium, error_invalid_ptr;
const
  ErrorAddr = nil;
var
  ThreadHeap: PThreadHeap;
  Pool: Pointer{PK64PoolSmall/PK64PoolMedium};
begin
  ThreadHeap := ThreadHeapInstance;
  if (ThreadHeap <> nil) then
  begin
    ThreadHeap.ErrorAddr := ErrorAddr;

    if (not ThreadHeap.Deferreds.Assigned) then
    begin
    thread_deferreds_done:
      if (NativeInt(P) and MASK_16_TEST = 0) then
      begin
        Pool := Pointer(NativeInt(P) and MASK_K64_CLEAR);

        if (NativeInt(P) and MASK_K1_TEST <> 0) then
        begin
          // pool: small or medium
          if (PK64PoolSmall(Pool).ThreadHeap = ThreadHeap) then
          begin
            // pool small
            Result := ThreadHeap.FreeSmall(P);
            Exit;
          end else
          if (PK64PoolSmall(Pool).ThreadHeap = nil) then
          begin
            // pool medium
          medium:
            if (PK64PoolMedium(Pool).ThreadHeap = ThreadHeap) then
            begin
              Result := ThreadHeap.FMedium.Free(P);
              Exit;
            end;
          end;
        end else
        begin
          if (NativeInt(P) and MASK_K4_TEST <> 0) then
          begin
            // medium or invalid pointer
            if (PK64PoolSmall(Pool).ThreadHeap = nil) then goto medium;
            goto error_invalid_ptr;
          end else
          begin
            // big or large
            if (not BrainMMFreeMemoryPages(MemoryPages(P), PAGESMODE_SYSTEM)) then
              goto error_invalid_ptr;

            Result := FREEMEM_DONE;
            Exit;
          end;
        end;

        // "default" method
        Result := ThreadHeap.FreeDifficult(P, ReturnAddress);
        Exit;
      end else
      begin
      error_invalid_ptr:
        Result := ThreadHeap.ErrorInvalidPtr;
      end;
    end else
    begin
      ThreadHeap.ProcessThreadDeferred;
      goto thread_deferreds_done;
    end;
  end else
  begin
    Result := BrainMMUnknownFreeMem(nil, P, ErrorAddr);
  end;
end;
{$else}
asm
  // ErrorAddr(v3) := nil
  {$ifdef CPUX86}
    xor ecx, ecx
    nop
    nop
  {$else .CPUX64}
    xor r8, r8
    nop
  {$endif}
@redirect:
  // v2 := P (v1)
  {$ifdef CPUX86}
    xchg eax, edx
  {$else .CPUX64}
    xchg rcx, rdx
  {$endif}

  // v1 := actual ThreadHeapInstance
  {$ifdef CPUX86}
    mov eax, fs:[THREAD_HEAP]
    test eax, eax
  {$else .CPUX64}
    mov rcx, gs:[abs THREAD_HEAP]
    test rcx, rcx
  {$endif}
  jz BrainMMUnknownFreeMem

  // ThreadHeap(v1).ErrorAddr := ErrorAddr(v3)
  {$ifdef CPUX86}
    mov [EAX].TThreadHeap.ErrorAddr, ecx
  {$else .CPUX64}
    mov [RCX].TThreadHeap.ErrorAddr, r8
  {$endif}

  // if (ThreadHeap.Deferreds.Assigned) then ThreadHeap(v1).ProcessThreadDeferred;
  {$ifdef CPUX86}
    cmp [EAX].TThreadHeap.Deferreds.F.Handle, 0
  {$else .CPUX64}
    mov r8, X64_SYNCSTACK_MASK
    test [RCX].TThreadHeap.Deferreds.F.Handle, r8
  {$endif}
  jnz @penalty_thread_deferreds
@thread_deferreds_done:

  // if (P (v2) and MASK_16_TEST <> 0) then Exit ThreadHeap.ErrorInvalidPtr
  {$ifdef CPUX86}
    test edx, MASK_16_TEST
  {$else .CPUX64}
    test rdx, MASK_16_TEST
  {$endif}
  jnz TThreadHeap.ErrorInvalidPtr

  // v3 := PK64PoolSmall/PK64PoolMedium(P)
  {$ifdef CPUX86}
    mov ecx, edx
    and ecx, MASK_K64_CLEAR
  {$else .CPUX64}
    mov r8, rdx
    and r8, MASK_K64_CLEAR
  {$endif}

  // if (P(v2) and MASK_K1_TEST = 0) then Not Small
  {$ifdef CPUX86}
    test edx, MASK_K1_TEST
  {$else .CPUX64}
    test rdx, MASK_K1_TEST
  {$endif}
  jz @not_small

  // if (PK64PoolSmall(P).ThreadHeap = @Self) then Exit ThreadHeap.FreeSmall(P)
  {$ifdef CPUX86}
    cmp [ECX].TK64PoolSmall.ThreadHeap, eax
  {$else .CPUX64}
    cmp [R8].TK64PoolSmall.ThreadHeap, rcx
  {$endif}
  je TThreadHeap.FreeSmall

  // if (PK64PoolSmall(P).ThreadHeap <> nil) then Exit ThreadHeap.FreeDifficult(P, ReturnAddress)
  {$ifdef CPUX86}
    cmp [ECX].TK64PoolSmall.ThreadHeap, 0
    jne @free_difficult
  {$else .CPUX64}
    cmp [R8].TK64PoolSmall.ThreadHeap, 0
    jne @free_difficult
  {$endif}

@medium:
  // if (PK64PoolMedium(P).ThreadHeap = @Self) then Exit ThreadHeap.FMedium.FreeMedium(P)
  // else Exit ThreadHeap.FreeDifficult
  {$ifdef CPUX86}
    cmp [ECX].TK64PoolMedium.ThreadHeap, eax
    lea eax, [EAX].TThreadHeap.FMedium
    je TMediumManager.Free
    lea eax, [EAX - TThreadHeap.FMedium]
  {$else .CPUX64}
    cmp [R8].TK64PoolMedium.ThreadHeap, rcx
    lea rcx, [RCX].TThreadHeap.FMedium
    je TMediumManager.Free
    lea rcx, [RCX - TThreadHeap.FMedium]
  {$endif}
  jmp @free_difficult

@not_small:
  // if (P(v2) and MASK_K4_TEST = 0) then
  //   BrainMMFreeMemoryPages(P, PAGESMODE_SYSTEM)
  {$ifdef CPUX86}
    test edx, MASK_K4_TEST
  {$else .CPUX64}
    test rdx, MASK_K4_TEST
  {$endif}
  jz @free_big_large

  // if (PK64PoolSmall(P).ThreadHeap = 0) then May be medium
  // else ThreadHeap.ErrorInvalidPtr
  {$ifdef CPUX86}
    cmp [ECX].TK64PoolSmall.ThreadHeap, 0
  {$else .CPUX64}
    cmp [R8].TK64PoolSmall.ThreadHeap, 0
  {$endif}
  je @medium
  jmp TThreadHeap.ErrorInvalidPtr

@free_big_large:
  {$ifdef CPUX86}
    mov eax, edx
  {$else .CPUX64}
    mov rcx, rdx
  {$endif}
  mov edx, PAGESMODE_SYSTEM
  call BrainMMFreeMemoryPages
  test al, al
  jz @error_invalid_ptr
  {$ifdef CPUX86}
    mov eax, FREEMEM_DONE
  {$else .CPUX64}
    mov rax, FREEMEM_DONE
  {$endif}
  ret

@error_invalid_ptr:
  {$ifdef CPUX86}
    mov eax, fs:[THREAD_HEAP]
  {$else .CPUX64}
    mov rcx, gs:[abs THREAD_HEAP]
  {$endif}
  jmp TThreadHeap.ErrorInvalidPtr

@free_difficult:
  {$ifdef CPUX86}
    mov ecx, [esp]
  {$else .CPUX64}
    mov r8, [rsp]
  {$endif}
  jmp TThreadHeap.FreeDifficult
@penalty_thread_deferreds:
  {$ifdef CPUX86}
    push ebx
    lea ebx, @x86_thread_deferreds_ret
    jmp SafeProcessThreadDeferred
  @x86_thread_deferreds_ret:
    pop ebx
    jmp @thread_deferreds_done
  {$else .CPUX64}
    lea r9, @thread_deferreds_done
    jmp SafeProcessThreadDeferred
  {$endif}
end;
{$endif}

function TThreadHeap.FreeSmall(P: Pointer): Integer;
{$ifdef PUREPASCAL}
var
  Line, Prev, Next: PK1LineSmall;
  Index, B16Count: NativeInt;

  Mask: NativeInt;
  {$ifdef SMALLINT}
  PVInteger: PInteger;
  {$endif}
begin
  Line := PK1LineSmall(NativeInt(P) and MASK_K1_CLEAR);
  Index := (NativeInt(P) and MASK_K1_TEST) shr 4;

  if (Line.Header.ItemSet.VLow32 and 3 = 0{not FullQueue}) then
  begin
    if (BitUnreserve(Line.Header.ItemSet, Index)) then
    begin
      if (Line.Header.ItemSet.V64 <> DEFAULT_BITSETS_SMALL[Line.Header.Flags and 15]) then
      begin
        Result := FREEMEM_DONE;
        Exit;
      end else
      begin
        Result := Self.DisposeK1LineSmall(Line);
        Exit;
      end;
    end else
    begin
      Result := Self.ErrorInvalidPtr;
      Exit;
    end;
  end else
  begin
    // Dequeue QK1LineFull
    Prev := Line.Header.Queue.Prev;
    Next := Line.Header.Queue.Next;
    if (Prev = nil) then
    begin
      Self.QK1LineFull := Next;
    end else
    begin
      Prev.Header.Queue.Next := Next;
    end;
    if (Next <> nil) then
    begin
      Next.Header.Queue.Prev := Prev;
    end;

    // Enqueue
    B16Count := NativeUInt(Line.Header.ModeSize) shr 4;
    Next := Self.FK1LineSmalls[B16Count];
    Self.FK1LineSmalls[B16Count] := Line;
    Line.Header.Queue.Prev := nil;
    Line.Header.Queue.Next := Next;
    if (Next <> nil) then Next.Header.Queue.Prev := Line;

    // full bitset, unreserve
    Mask := 1;
    {$ifdef LARGEINT}
      Mask := Mask shl Index;
      Line.Header.ItemSet.V64 := Mask;
    {$else .SMALLINT}
      Mask := Mask shl (Index and 31);
      PVInteger := @Line.Header.ItemSet.VIntegers[Byte(Index > 31)];
      Line.Header.ItemSet.VLow32{V64} := 0;
      PVInteger^ := Mask;
    {$endif}
    Result := FREEMEM_DONE;
  end;
end;
{$else}
asm
  // Line := LineOf(P)
  // Index := IndexOf(Line, P)
  {$ifdef CPUX86}
    mov ecx, edx
    and edx, MASK_K1_CLEAR
    and ecx, MASK_K1_TEST
    shr ecx, 4
  {$else .CPUX64}
    mov r8, rdx
    and rdx, MASK_K1_CLEAR
    and r8, MASK_K1_TEST
    shr r8, 4
  {$endif}

  // if (Line.FullQueue) then Exit Self.PenaltyFreeSmall(@Line.Items[Index])
  // if (not BitUnreserve(Index)) then Exit Self.ErrorInvalidPtr
  {$ifdef CPUX86}
    test [EDX].TK1LineSmall.Header.ItemSet.VLow32, 3
    jnz @penalty_requeue
    lea eax, [EDX].TK1LineSmall.Header.ItemSet.VHigh32
    test ecx, 32
    DB $0F, $45, $D0 // cmovnz edx, eax
    and ecx, 31
    mov eax, [edx]
    bts eax, ecx
    mov [edx], eax
    jc @penalty_error
  {$else .CPUX64}
    mov r9, [RDX].TK1LineSmall.Header.ItemSet.V64
    test r9, 3
    jnz @penalty_requeue
    bts r9, r8
    mov [RDX].TK1LineSmall.Header.ItemSet.V64, r9
    jc @penalty_error
  {$endif}

  // if (Line.Header.ItemSet.V64 = DEFAULT_BITSETS_SMALL[BitSetKind(Line)]) then
  // Exit Self.DisposeK1LineSmall(Line)
  {$ifdef CPUX86}
    and edx, MASK_K1_CLEAR
    mov ecx, 15
    and ecx, [EDX].TK1LineSmall.Header.Flags
    mov eax, [offset DEFAULT_BITSETS_SMALL + ecx * 8]
    mov ecx, [offset DEFAULT_BITSETS_SMALL + ecx * 8 + 4]
    sub eax, [edx]
    sub ecx, [edx + 4]
    or eax, ecx
    jz @penalty_dispose_line
  {$else .CPUX64}
    mov rax, [RDX].TK1LineSmall.Header.Flags
    and rax, 15
    lea r8, DEFAULT_BITSETS_SMALL
    cmp r9, [r8 + rax * 8]
    je @penalty_dispose_line
  {$endif}

  // Result
  mov eax, FREEMEM_DONE
  ret

@penalty_error:
  {$ifdef CPUX86}
    and edx, MASK_K64_CLEAR
    mov eax, [EDX].TK64PoolSmall.ThreadHeap
  {$endif}
  jmp TThreadHeap.ErrorInvalidPtr
@penalty_dispose_line:
  {$ifdef CPUX86}
    mov eax, edx
    and eax, MASK_K64_CLEAR
    mov eax, [EAX].TK64PoolSmall.ThreadHeap
  {$endif}
  jmp TThreadHeap.DisposeK1LineSmall
@penalty_requeue:
  // store Self, x86: store esi/edi
  // dequeue Self.QK1LineFull
  {$ifdef CPUX86}
    push esi
    push edi
    push eax

    mov esi, [EDX].TK1LineSmall.Header.Queue.Prev
    mov edi, [EDX].TK1LineSmall.Header.Queue.Next
    lea eax, [EAX + TThreadHeap.QK1LineFull - TK1LineSmall.Header.Queue.Next]
    test esi, esi
    DB $0F, $45, $C6 // cmovnz eax, esi
    mov [EAX].TK1LineSmall.Header.Queue.Next, edi
    lea eax, [esp - 64]
    test edi, edi
    DB $0F, $45, $C7 // cmovnz eax, edi
    mov [EAX].TK1LineSmall.Header.Queue.Prev, esi
  {$else .CPUX64}
    mov rax, rcx

    mov r9, [RDX].TK1LineSmall.Header.Queue.Prev
    mov r10, [RDX].TK1LineSmall.Header.Queue.Next
    lea rcx, [RCX + TThreadHeap.QK1LineFull - TK1LineSmall.Header.Queue.Next]
    test r9, r9
    cmovnz rcx, r9
    mov [RCX].TK1LineSmall.Header.Queue.Next, r10
    lea rcx, [rsp - 64]
    test r10, r10
    cmovnz rcx, r10
    mov [RCX].TK1LineSmall.Header.Queue.Prev, r9
  {$endif}

  // retrieve Self
  // enqueue Self.FK1LineSmalls
  // v4 := 0
  {$ifdef CPUX86}
    pop eax
    add eax, 4
    mov esi, [EDX].TK1LineSmall.Header.Flags
    and esi, 7
    mov edi, [EAX + esi * 4]
    mov [EAX + esi * 4], edx
    xor esi, esi
    mov [EDX].TK1LineSmall.Header.Queue.Prev, esi
    mov [EDX].TK1LineSmall.Header.Queue.Next, edi
    lea eax, [esp - 64]
    test edi, edi
    DB $0F, $45, $C7 // cmovnz eax, edi
    mov [EAX].TK1LineSmall.Header.Queue.Prev, edx
  {$else .CPUX64}
    lea rcx, [rax + 8]
    mov r9, [RDX].TK1LineSmall.Header.Flags
    and r9, 7
    mov r10, [RCX + r9 * 8]
    mov [RCX + r9 * 8], rdx
    xor r9, r9
    mov [RDX].TK1LineSmall.Header.Queue.Prev, r9
    mov [RDX].TK1LineSmall.Header.Queue.Next, r10
    lea rcx, [rsp - 64]
    test r10, r10
    cmovnz rcx, r10
    mov [RCX].TK1LineSmall.Header.Queue.Prev, rdx
  {$endif}

  // full bitset, unreserve (v4 = 0)
  {$ifdef CPUX86}
    mov [EDX].TK1LineSmall.Header.ItemSet.VLow32, esi
    lea edi, [EDX].TK1LineSmall.Header.ItemSet.VHigh32
    { lea edx, [EDX].TK1LineSmall.Header.ItemSet.VLow32 }
    test ecx, 32
    DB $0F, $45, $D7 // cmovnz edx, edi
    and ecx, 31
    mov edi, [edx]
    bts edi, ecx
    mov [edx], edi
    pop edi
    pop esi
  {$else .CPUX64}
    bts r9, r8
    mov [RDX].TK1LineSmall.Header.ItemSet.V64, r9
  {$endif}

  // Result
  mov eax, FREEMEM_DONE
end;
{$endif}

function BrainMMUnknownResizeMem(ErrorAddr: Pointer; P: Pointer;
  FlagsNewB16Count: NativeUInt{NewB16Count, High Copy}; ReturnAddress: Pointer): Pointer;
label
  return_made_none, medium, raise_invalid_ptr;
var
  NewB16Count, FlagCopy: NativeUInt;
  ThreadHeap: PThreadHeap;
  Flags: SupposedPtr;
  Pool: Pointer{PK64PoolSmall/PK64PoolMedium};
  LastB16Count: NativeUInt;
begin
  // unpack parameters
  NewB16Count := FlagsNewB16Count and MASK_HIGH_NATIVE_TEST;
  FlagCopy := FlagsNewB16Count shr HIGH_NATIVE_BIT;

  // inline SpinLock
  ThreadHeap := UnknownThreadHeap;
  repeat
    Flags := ThreadHeap.LockFlags;
    if (Flags <> THREAD_HEAP_LOCKABLE) then
    begin
      if (Flags = THREAD_HEAP_LOCKED) then
      begin
        SpinWait(ThreadHeap.LockFlags, THREAD_HEAP_LOCKED_BIT);
      end else
      begin
        {$ifdef CONDITIONALEXPRESSIONS}System.Error(reInvalidCast){$else}System.RunError(219){$endif};
      end;
    end;
  until (THREAD_HEAP_LOCKABLE = AtomicCmpExchange(ThreadHeap.LockFlags, THREAD_HEAP_LOCKED, THREAD_HEAP_LOCKABLE));
  try
    // resize
    ThreadHeap.ErrorAddr := ErrorAddr;
    if (NativeInt(P) and MASK_16_TEST = 0) then
    begin
      Pool := Pointer(NativeInt(P) and MASK_K64_CLEAR);

      if (NativeInt(P) and MASK_K1_TEST <> 0) then
      begin
        // pool: small or medium
        if (PK64PoolSmall(Pool).ThreadHeap = ThreadHeap) then
        begin
          // pool small
          NewB16Count := {NewSize}NewB16Count shl 4;
          if (PK1LineSmall(NativeInt(P) and MASK_K1_CLEAR).Header.ModeSize >= {NewSize}NewB16Count) then
          begin
          return_made_none:
            Result := P;
            Exit;
          end else
          begin
            NewB16Count := {retrieve}NewB16Count shr 4;
            if (NewB16Count <= MAX_SMALL_B16COUNT) then
            begin
              if (FlagCopy <> 0) then
              begin
                Result := ThreadHeap.GrowSmallToSmall(P, NewB16Count);
              end else
              begin
                Result := ThreadHeap.RegrowSmallToSmall(P, NewB16Count);
              end;

              Exit;
            end;
          end;
        end else
        if (PK64PoolSmall(Pool).ThreadHeap = nil) then
        begin
          // pool medium
        medium:
          if (PK64PoolMedium(Pool).ThreadHeap = ThreadHeap) then
          begin
            LastB16Count := NativeUInt(PHeaderMedium(NativeInt(P) - SizeOf(THeaderMedium)).WSize) shr 4;
            if (NewB16Count <= LastB16Count) then
            begin
              if (NewB16Count >= LastB16Count - 1) then goto return_made_none;
              Result := ThreadHeap.FMedium.Reduce(P, NewB16Count);
              Exit;
            end;
            if (NewB16Count <= MAX_MEDIUM_B16COUNT) then
            begin
              Result := ThreadHeap.FMedium.Grow(P, (FlagCopy shl FLAG_MEDIUM_COPY_SHIFT) + NewB16Count);
              Exit;
            end;
          end;
        end;
      end else
      begin
        if (NativeInt(P) and MASK_K4_TEST <> 0) then
        begin
          // medium or invalid pointer
          if (PK64PoolSmall(Pool).ThreadHeap = nil) then goto medium;
          goto raise_invalid_ptr;
        end else
        begin
          // big or large
          Result := BrainMMResizeMemoryPages(MemoryPages(P),
            (NewB16Count + (B16_PER_PAGE - 1)) shr B16_PER_PAGE_SHIFT,
            (FlagCopy shl RESIZE_PAGES_COPY_SHIFT) + PAGESMODE_SYSTEM);
          if (NativeUInt(Result) <= NativeUInt(PTR_INVALID)) then
          begin
            if (Result = PTR_INVALID) then goto raise_invalid_ptr;
            Result := ThreadHeap.ErrorOutOfMemory;
          end;

          Exit;
        end;
      end;

      // "default" method
      Result := ThreadHeap.ResizeDifficult(NativeUInt(P) + FlagCopy, NewB16Count, ReturnAddress);
      Exit;
    end else
    begin
    raise_invalid_ptr:
      Result := Pointer(ThreadHeap.RaiseInvalidPtr);
    end;
  finally
    // inline SpinUnlock
    ThreadHeap.LockFlags := THREAD_HEAP_LOCKABLE;
  end;
end;

function BrainMMRegetMem(P: Pointer; NewSize: NativeUInt): Pointer;
{$ifdef PUREPASCAL}
{$ifNdef RETURNADDRESS}
const
  ReturnAddress: Pointer = @BrainMMRegetMem;
{$endif}
label
  thread_deferreds_done, return_made_none,
  medium, raise_invalid_ptr;
const
  ErrorAddr = nil;
var
  ThreadHeap: PThreadHeap;
  Pool: Pointer{PK64PoolSmall/PK64PoolMedium};
  LastB16Count, NewB16Count: NativeUInt;
begin
  NewB16Count := (NewSize + 15) shr 4;
  ThreadHeap := ThreadHeapInstance;
  if (ThreadHeap <> nil) then
  begin
    ThreadHeap.ErrorAddr := ErrorAddr;

    if (not ThreadHeap.Deferreds.Assigned) then
    begin
    thread_deferreds_done:
      if (NativeInt(P) and MASK_16_TEST = 0) then
      begin
        Pool := Pointer(NativeInt(P) and MASK_K64_CLEAR);

        if (NativeInt(P) and MASK_K1_TEST <> 0) then
        begin
          // pool: small or medium
          if (PK64PoolSmall(Pool).ThreadHeap = ThreadHeap) then
          begin
            // pool small
            NewB16Count := {NewSize}NewB16Count shl 4;
            if (PK1LineSmall(NativeInt(P) and MASK_K1_CLEAR).Header.ModeSize >= {NewSize}NewB16Count) then
            begin
            return_made_none:
              Result := P;
              Exit;
            end else
            begin
              NewB16Count := {retrieve}NewB16Count shr 4;
              if (NewB16Count <= MAX_SMALL_B16COUNT) then
              begin
                Result := ThreadHeap.RegrowSmallToSmall(P, NewB16Count);
                Exit;
              end;
            end;
          end else
          if (PK64PoolSmall(Pool).ThreadHeap = nil) then
          begin
            // pool medium
          medium:
            if (PK64PoolMedium(Pool).ThreadHeap = ThreadHeap) then
            begin
              LastB16Count := NativeUInt(PHeaderMedium(NativeInt(P) - SizeOf(THeaderMedium)).WSize) shr 4;
              if (NewB16Count <= LastB16Count) then
              begin
                if (NewB16Count >= LastB16Count - 1) then goto return_made_none;
                Result := ThreadHeap.FMedium.Reduce(P, NewB16Count);
                Exit;
              end;
              if (NewB16Count <= MAX_MEDIUM_B16COUNT) then
              begin
                Result := ThreadHeap.FMedium.Grow(P, 0 + NewB16Count);
                Exit;
              end;
            end;
          end;
        end else
        begin
          if (NativeInt(P) and MASK_K4_TEST <> 0) then
          begin
            // medium or invalid pointer
            if (PK64PoolSmall(Pool).ThreadHeap = nil) then goto medium;
            goto raise_invalid_ptr;
          end else
          begin
            // big or large
            Result := BrainMMResizeMemoryPages(MemoryPages(P),
              (NewB16Count + (B16_PER_PAGE - 1)) shr B16_PER_PAGE_SHIFT, RESIZE_PAGES_SYSTEM_REGET);
            if (NativeUInt(Result) <= NativeUInt(PTR_INVALID)) then
            begin
              if (Result = PTR_INVALID) then goto raise_invalid_ptr;
              Result := ThreadHeap.ErrorOutOfMemory;
            end;

            Exit;
          end;
        end;

        // "default" method
        Result := ThreadHeap.ResizeDifficult(NativeInt(P) + Ord(False), NewB16Count, ReturnAddress);
        Exit;
      end else
      begin
      raise_invalid_ptr:
        Result := Pointer(ThreadHeap.RaiseInvalidPtr);
      end;
    end else
    begin
      ThreadHeap.ProcessThreadDeferred;
      goto thread_deferreds_done;
    end;
  end else
  begin
    Result := BrainMMUnknownResizeMem(ErrorAddr, P, 0 + NewB16Count, ReturnAddress);
  end;
end;
{$else}
asm
  // var P(v3) := fake
  // ErrorAddr(v4) := nil
  {$ifdef CPUX86}
    lea ecx, [esp - 8]
    push ebx
    xor ebx, ebx
    nop
  {$else .CPUX64}
    lea r8, [rsp - 8]
    xor r9, r9
  {$endif}
@redirect:
  // store var P (v3), v3 := (NewSize(v2) + 15) div 16, v2 := const P (v1)
  {$ifdef CPUX86}
    // stack := var P
    push ecx
    lea ecx, [edx + 15]
    xchg edx, eax
    shr ecx, 4
  {$else .CPUX64}
    // v5 := var P
    mov r10, r8
    lea r8, [rdx + 15]
    xchg rdx, rcx
    shr r8, 4
  {$endif}

  // v1 := actual ThreadHeapInstance
  {$ifdef CPUX86}
    mov eax, fs:[THREAD_HEAP]
    test eax, eax
  {$else .CPUX64}
    mov rcx, gs:[abs THREAD_HEAP]
    test rcx, rcx
  {$endif}
  jz @return_unknown

  // ThreadHeap(v1).ErrorAddr := ErrorAddr(v4)
  {$ifdef CPUX86}
    mov [EAX].TThreadHeap.ErrorAddr, ebx
  {$else .CPUX64}
    mov [RCX].TThreadHeap.ErrorAddr, r9
  {$endif}

  // if (ThreadHeap.Deferreds.Assigned) then ThreadHeap(v1).ProcessThreadDeferred;
  {$ifdef CPUX86}
    cmp [EAX].TThreadHeap.Deferreds.F.Handle, 0
  {$else .CPUX64}
    mov r9, X64_SYNCSTACK_MASK
    test [RCX].TThreadHeap.Deferreds.F.Handle, r9
  {$endif}
  jnz @penalty_thread_deferreds
@thread_deferreds_done:

  // if (P (v2) and MASK_16_TEST <> 0) then ThreadHeap.RaiseInvalidPtr
  {$ifdef CPUX86}
    test edx, MASK_16_TEST
  {$else .CPUX64}
    test rdx, MASK_16_TEST
  {$endif}
  jnz @raise_invalid_ptr

  // v4 := PK64PoolSmall/PK64PoolMedium(P)
  {$ifdef CPUX86}
    mov ebx, edx
    and ebx, MASK_K64_CLEAR
  {$else .CPUX64}
    mov r9, rdx
    and r9, MASK_K64_CLEAR
  {$endif}

  // if (P(v2) and MASK_K1_TEST = 0) then Not Small
  {$ifdef CPUX86}
    test edx, MASK_K1_TEST
  {$else .CPUX64}
    test rdx, MASK_K1_TEST
  {$endif}
  jz @not_small

  // if (PK64PoolSmall(P).ThreadHeap <> @Self) then Not Fast
  {$ifdef CPUX86}
    cmp [EBX].TK64PoolSmall.ThreadHeap, eax
  {$else .CPUX64}
    cmp [R9].TK64PoolSmall.ThreadHeap, rcx
  {$endif}
  jne @not_fast

  // if (PK1LineSmall(P).ModeSize >= {NewSize}NewB16Count * 16) then Exit P (return made none)
  {$ifdef CPUX86}
    mov ebx, edx
    and ebx, MASK_K1_CLEAR
    shl ecx, 4
    movzx ebx, byte ptr TK1LineSmall[EBX].Header.ModeSize
    cmp ebx, ecx
  {$else .CPUX64}
    mov r9, rdx
    and r9, MASK_K1_CLEAR
    shl r8, 4
    movzx r9, byte ptr TK1LineSmall[R9].Header.ModeSize
    cmp r9, r8
  {$endif}
  jb @small_grow
@return_made_none:
  // Result := const P (v2)
  {$ifdef CPUX86}
    mov eax, edx
    pop ecx
    pop ebx
  {$else .CPUX64}
    mov rax, rdx
  {$endif}
  ret

@small_grow:
  // retrieve NewB16Count
  // if (NewSize > MAX_SMALL_SIZE) then Exit ThreadHeap.ResizeDifficult(False)
  {$ifdef CPUX86}
    mov ebx, ecx
    shr ecx, 4
    cmp ebx, MAX_SMALL_SIZE
  {$else .CPUX64}
    mov r9, r8
    shr r8, 4
    cmp r9, MAX_SMALL_SIZE
  {$endif}
  ja @reget_difficult

  // Exit (recall) ThreadHeap.RegrowSmallToSmall
  {$ifdef CPUX86}
    cmp esp, [esp]
    mov ebx, [esp + 4]
    lea esp, [esp + 8]
  {$else .CPUX64}
    lea r9, [rsp - 8]
    cmp r9, r10
  {$endif}
  je TThreadHeap.RegrowSmallToSmall
  {$ifdef CPUX86}
    lea esp, [esp - 8]
    push offset @return_var_p
  {$else .CPUX64}
    push r10
    lea r9, @return_var_p
    push r9
  {$endif}
  jmp TThreadHeap.RegrowSmallToSmall

@not_fast:
  // if (PK64PoolSmall(P).ThreadHeap = 0) then May be medium
  // else Exit ThreadHeap.ResizeDifficult(False)
  {$ifdef CPUX86}
    cmp [EBX].TK64PoolSmall.ThreadHeap, 0
  {$else .CPUX64}
    cmp [R9].TK64PoolSmall.ThreadHeap, 0
  {$endif}
  jnz @reget_difficult

@medium:
  // if (PK64PoolMedium(P).ThreadHeap <> @Self) then Exit ThreadHeap.ResizeDifficult(False)
  {$ifdef CPUX86}
    cmp [EBX].TK64PoolMedium.ThreadHeap, eax
  {$else .CPUX64}
    cmp [R9].TK64PoolMedium.ThreadHeap, rcx
  {$endif}
  jne @reget_difficult

  // if (NewB16Count > MAX_MEDIUM_B16COUNT) then Exit ThreadHeap.ResizeDifficult(False)
  {$ifdef CPUX86}
    cmp ecx, MAX_MEDIUM_B16COUNT
  {$else .CPUX64}
    cmp r8, MAX_MEDIUM_B16COUNT
  {$endif}  
  ja @reget_difficult

  // if (NewB16Count > LastB16Count) then Exit TMediumManager.Grow()
  // if (NewB16Count >= LastB16Count - 1) then Exit P (return made none)
  // else Exit TMediumManager.Reduce()
  {$ifdef CPUX86}
    shl ecx, 4
    movzx ebx, [EDX - 16].THeaderMedium.WSize
    sub ebx, ecx
    shr ecx, 4
    lea eax, [EAX].TThreadHeap.FMedium
    cmp ebx, 16
  {$else .CPUX64}
    shl r8, 4
    movzx r9, [RDX - 16].THeaderMedium.WSize
    sub r9, r8
    shr r8, 4
    lea rcx, [RCX].TThreadHeap.FMedium
    cmp r9, 16
  {$endif}
  jbe @return_made_none
  {$ifdef CPUX86}
    push offset @return_var_p
  {$else .CPUX64}
    push r10
    lea r9, @return_var_p
    push r9
  {$endif}
  jl TMediumManager.Grow
  jmp TMediumManager.Reduce

@not_small:
  // if (P(v2) and MASK_K4_TEST = 0) then BigOrLarge
  {$ifdef CPUX86}
    test edx, MASK_K4_TEST
  {$else .CPUX64}
    test rdx, MASK_K4_TEST
  {$endif}
  jz @reget_big_large

  // if (PK64PoolSmall(P).ThreadHeap = 0) then May be medium
  // else ThreadHeap.RaiseInvalidPtr
  {$ifdef CPUX86}
    cmp [EBX].TK64PoolSmall.ThreadHeap, 0
  {$else .CPUX64}
    cmp [R9].TK64PoolSmall.ThreadHeap, 0
  {$endif}
  je @medium
  jmp @raise_invalid_ptr

@reget_big_large:
  // Exit BrainMM"RegetMemoryPages"(P, PagesOf(NewB16Count), PAGESMODE_SYSTEM);
  {$ifdef CPUX86}
    mov eax, edx
    lea edx, [ecx + B16_PER_PAGE - 1]
    mov ecx, RESIZE_PAGES_SYSTEM_REGET
    shr edx, B16_PER_PAGE_SHIFT
  {$else .CPUX64}
    mov rcx, rdx
    lea rdx, [r8 + B16_PER_PAGE - 1]
    mov r8d, RESIZE_PAGES_SYSTEM_REGET
    shr rdx, B16_PER_PAGE_SHIFT
    push r10
  {$endif}
  call BrainMMResizeMemoryPages
  {$ifdef CPUX86}
    cmp eax, 1 // PTR_INVALID
  {$else .CPUX64}
    cmp rax, 1 // PTR_INVALID
  {$endif}
  ja @return_var_p
  je @raise_invalid_pages
  {$ifdef CPUX86}
    push offset @return_var_p
    mov eax, fs:[THREAD_HEAP]
  {$else .CPUX64}
    lea r9, @return_var_p
    push r9
    mov rcx, gs:[abs THREAD_HEAP]
  {$endif}
  jmp TThreadHeap.ErrorOutOfMemory

@reget_difficult:
  {$ifdef CPUX86}
    push dword ptr [esp + 8]
  {$else .CPUX64}
    mov r9, [rsp]
    push r10
  {$endif}
  call TThreadHeap.ResizeDifficult
@return_var_p:
  {$ifdef CPUX86}
    pop ecx
    pop ebx
    mov [ecx], eax
  {$else .CPUX64}
    pop rcx
    mov [rcx], rax
  {$endif}
  ret
@return_unknown:
  {$ifdef CPUX86}
    mov eax, ebx
    push dword ptr [esp + 8]
    push offset @return_var_p
  {$else .CPUX64}
    mov rcx, r9
    mov r9, [rsp]
    push r10
    lea r10, @return_var_p
    push r10
  {$endif}
  jmp BrainMMUnknownResizeMem
@raise_invalid_pages:
  {$ifdef CPUX64}
    pop r10
  {$endif}
@raise_invalid_ptr:
  {$ifdef CPUX86}
    pop ecx
    pop ebx
  {$endif}
  jmp TThreadHeap.RaiseInvalidPtr
@penalty_thread_deferreds:
  {$ifdef CPUX86}
    lea ebx, @thread_deferreds_done
  {$else .CPUX64}
    lea r9, @thread_deferreds_done
  {$endif}
  jmp SafeProcessThreadDeferred
end;
{$endif}

function TThreadHeap.RegrowSmallToSmall(P: Pointer;
  NewB16Count: NativeUInt): Pointer;
{$ifdef PUREPASCAL}
begin
  if (Self.FreeSmall(P) {$ifdef FPC}={$else}<>{$endif} 0) then
  begin
    Result := Pointer(RaiseInvalidPtr);
  end else
  begin
    Result := Self.GetSmall(NewB16Count);
  end;
end;
{$else}
asm
  {$ifdef CPUX86}
    push eax
    push ecx
      call TThreadHeap.FreeSmall
    test eax, eax
    pop edx
    pop eax
  {$else .CPUX64}
    push rcx
    push r8
    {stack align} push rcx
       call TThreadHeap.FreeSmall
    {stack align} pop rcx
    test eax, eax
    pop rdx
    pop rcx
  {$endif}

  {$ifNdef FPC}
    jz TThreadHeap.GetSmall
  {$else .FPC}
    jnz TThreadHeap.GetSmall
  {$endif}

  jmp TThreadHeap.RaiseInvalidPtr
end;
{$endif}

function BrainMMReallocMem(P: Pointer; NewSize: NativeUInt): Pointer;
{$ifdef PUREPASCAL}
{$ifNdef RETURNADDRESS}
const
  ReturnAddress: Pointer = @BrainMMReallocMem;
{$endif}
label
  thread_deferreds_done, return_made_none,
  medium, raise_invalid_ptr;
const
  ErrorAddr = nil;
var
  ThreadHeap: PThreadHeap;
  Pool: Pointer{PK64PoolSmall/PK64PoolMedium};
  LastB16Count, NewB16Count: NativeUInt;
begin
  NewB16Count := (NewSize + 15) shr 4;
  ThreadHeap := ThreadHeapInstance;
  if (ThreadHeap <> nil) then
  begin
    ThreadHeap.ErrorAddr := ErrorAddr;

    if (not ThreadHeap.Deferreds.Assigned) then
    begin
    thread_deferreds_done:
      if (NativeInt(P) and MASK_16_TEST = 0) then
      begin
        Pool := Pointer(NativeInt(P) and MASK_K64_CLEAR);

        if (NativeInt(P) and MASK_K1_TEST <> 0) then
        begin
          // pool: small or medium
          if (PK64PoolSmall(Pool).ThreadHeap = ThreadHeap) then
          begin
            // pool small
            NewB16Count := {NewSize}NewB16Count shl 4;
            if (PK1LineSmall(NativeInt(P) and MASK_K1_CLEAR).Header.ModeSize >= {NewSize}NewB16Count) then
            begin
            return_made_none:
              Result := P;
              Exit;
            end else
            begin
              NewB16Count := {retrieve}NewB16Count shr 4;
              if (NewB16Count <= MAX_SMALL_B16COUNT) then
              begin
                Result := ThreadHeap.GrowSmallToSmall(P, NewB16Count);
                Exit;
              end;
            end;
          end else
          if (PK64PoolSmall(Pool).ThreadHeap = nil) then
          begin
            // pool medium
          medium:
            if (PK64PoolMedium(Pool).ThreadHeap = ThreadHeap) then
            begin
              LastB16Count := NativeUInt(PHeaderMedium(NativeInt(P) - SizeOf(THeaderMedium)).WSize) shr 4;
              if (NewB16Count <= LastB16Count) then
              begin
                if (NewB16Count >= LastB16Count - 1) then goto return_made_none;
                Result := ThreadHeap.FMedium.Reduce(P, NewB16Count);
                Exit;
              end;
              if (NewB16Count <= MAX_MEDIUM_B16COUNT) then
              begin
                Result := ThreadHeap.FMedium.Grow(P, FLAG_MEDIUM_COPY + NewB16Count);
                Exit;
              end;
            end;
          end;
        end else
        begin
          if (NativeInt(P) and MASK_K4_TEST <> 0) then
          begin
            // medium or invalid pointer
            if (PK64PoolSmall(Pool).ThreadHeap = nil) then goto medium;
            goto raise_invalid_ptr;
          end else
          begin
            // big or large
            Result := BrainMMResizeMemoryPages(MemoryPages(P),
              (NewB16Count + (B16_PER_PAGE - 1)) shr B16_PER_PAGE_SHIFT, RESIZE_PAGES_SYSTEM_REALLOC);
            if (NativeUInt(Result) <= NativeUInt(PTR_INVALID)) then
            begin
              if (Result = PTR_INVALID) then goto raise_invalid_ptr;
              Result := ThreadHeap.ErrorOutOfMemory;
            end;

            Exit;
          end;
        end;

        // "default" method
        Result := ThreadHeap.ResizeDifficult(NativeInt(P) + Ord(True), NewB16Count, ReturnAddress);
        Exit;
      end else
      begin
      raise_invalid_ptr:
        Result := Pointer(ThreadHeap.RaiseInvalidPtr);
      end;
    end else
    begin
      ThreadHeap.ProcessThreadDeferred;
      goto thread_deferreds_done;
    end;
  end else
  begin
    Result := BrainMMUnknownResizeMem(ErrorAddr, P, NativeUInt(MASK_HIGH_NATIVE_BIT) + NewB16Count, ReturnAddress);
  end;
end;
{$else}
asm
  // var P(v3) := fake
  // ErrorAddr(v4) := nil
  {$ifdef CPUX86}
    lea ecx, [esp - 8]
    push ebx
    xor ebx, ebx
    nop
  {$else .CPUX64}
    lea r8, [rsp - 8]
    xor r9, r9
  {$endif}
@redirect:
  // store var P (v3), v3 := (NewSize(v2) + 15) div 16, v2 := const P (v1)
  {$ifdef CPUX86}
    // stack := var P
    push ecx
    lea ecx, [edx + 15]
    xchg edx, eax
    shr ecx, 4
  {$else .CPUX64}
    // v5 := var P
    mov r10, r8
    lea r8, [rdx + 15]
    xchg rdx, rcx
    shr r8, 4
  {$endif}

  // v1 := actual ThreadHeapInstance
  {$ifdef CPUX86}
    mov eax, fs:[THREAD_HEAP]
    test eax, eax
  {$else .CPUX64}
    mov rcx, gs:[abs THREAD_HEAP]
    test rcx, rcx
  {$endif}
  jz @return_unknown

  // ThreadHeap(v1).ErrorAddr := ErrorAddr(v4)
  {$ifdef CPUX86}
    mov [EAX].TThreadHeap.ErrorAddr, ebx
  {$else .CPUX64}
    mov [RCX].TThreadHeap.ErrorAddr, r9
  {$endif}

  // if (ThreadHeap.Deferreds.Assigned) then ThreadHeap(v1).ProcessThreadDeferred;
  {$ifdef CPUX86}
    cmp [EAX].TThreadHeap.Deferreds.F.Handle, 0
  {$else .CPUX64}
    mov r9, X64_SYNCSTACK_MASK
    test [RCX].TThreadHeap.Deferreds.F.Handle, r9
  {$endif}
  jnz @penalty_thread_deferreds
@thread_deferreds_done:

  // if (P (v2) and MASK_16_TEST <> 0) then ThreadHeap.RaiseInvalidPtr
  {$ifdef CPUX86}
    test edx, MASK_16_TEST
  {$else .CPUX64}
    test rdx, MASK_16_TEST
  {$endif}
  jnz @raise_invalid_ptr

  // v4 := PK64PoolSmall/PK64PoolMedium(P)
  {$ifdef CPUX86}
    mov ebx, edx
    and ebx, MASK_K64_CLEAR
  {$else .CPUX64}
    mov r9, rdx
    and r9, MASK_K64_CLEAR
  {$endif}

  // if (P(v2) and MASK_K1_TEST = 0) then Not Small
  {$ifdef CPUX86}
    test edx, MASK_K1_TEST
  {$else .CPUX64}
    test rdx, MASK_K1_TEST
  {$endif}
  jz @not_small

  // if (PK64PoolSmall(P).ThreadHeap <> @Self) then Not Fast
  {$ifdef CPUX86}
    cmp [EBX].TK64PoolSmall.ThreadHeap, eax
  {$else .CPUX64}
    cmp [R9].TK64PoolSmall.ThreadHeap, rcx
  {$endif}
  jne @not_fast

  // if (PK1LineSmall(P).ModeSize >= {NewSize}NewB16Count * 16) then Exit P (return made none)
  {$ifdef CPUX86}
    mov ebx, edx
    and ebx, MASK_K1_CLEAR
    shl ecx, 4
    movzx ebx, byte ptr TK1LineSmall[EBX].Header.ModeSize
    cmp ebx, ecx
  {$else .CPUX64}
    mov r9, rdx
    and r9, MASK_K1_CLEAR
    shl r8, 4
    movzx r9, byte ptr TK1LineSmall[R9].Header.ModeSize
    cmp r9, r8
  {$endif}
  jb @small_grow
@return_made_none:
  // Result := const P (v2)
  {$ifdef CPUX86}
    mov eax, edx
    pop ecx
    pop ebx
  {$else .CPUX64}
    mov rax, rdx
  {$endif}
  ret

@small_grow:
  // retrieve NewB16Count
  // if (NewSize > MAX_SMALL_SIZE) then Exit ThreadHeap.ResizeDifficult(True)
  {$ifdef CPUX86}
    mov ebx, ecx
    shr ecx, 4
    cmp ebx, MAX_SMALL_SIZE
  {$else .CPUX64}
    mov r9, r8
    shr r8, 4
    cmp r9, MAX_SMALL_SIZE
  {$endif}
  ja @realloc_difficult

  // Exit (recall) ThreadHeap.GrowSmallToSmall
  {$ifdef CPUX86}
    cmp esp, [esp]
    mov ebx, [esp + 4]
    lea esp, [esp + 8]
  {$else .CPUX64}
    lea r9, [rsp - 8]
    cmp r9, r10
  {$endif}
  je TThreadHeap.GrowSmallToSmall
  {$ifdef CPUX86}
    lea esp, [esp - 8]
    push offset @return_var_p
  {$else .CPUX64}
    push r10
    lea r9, @return_var_p
    push r9
  {$endif}
  jmp TThreadHeap.GrowSmallToSmall

@not_fast:
  // if (PK64PoolSmall(P).ThreadHeap = 0) then May be medium
  // else Exit ThreadHeap.ResizeDifficult(True)
  {$ifdef CPUX86}
    cmp [EBX].TK64PoolSmall.ThreadHeap, 0
  {$else .CPUX64}
    cmp [R9].TK64PoolSmall.ThreadHeap, 0
  {$endif}
  jnz @realloc_difficult

@medium:
  // if (PK64PoolMedium(P).ThreadHeap <> @Self) then Exit ThreadHeap.ResizeDifficult(True)
  {$ifdef CPUX86}
    cmp [EBX].TK64PoolMedium.ThreadHeap, eax
  {$else .CPUX64}
    cmp [R9].TK64PoolMedium.ThreadHeap, rcx
  {$endif}
  jne @realloc_difficult

  // if (NewB16Count > MAX_MEDIUM_B16COUNT) then Exit ThreadHeap.ResizeDifficult(True)
  {$ifdef CPUX86}
    cmp ecx, MAX_MEDIUM_B16COUNT
  {$else .CPUX64}
    cmp r8, MAX_MEDIUM_B16COUNT
  {$endif}  
  ja @realloc_difficult
  
  // if (NewB16Count > LastB16Count) then Exit TMediumManager.Grow(FLAG_MEDIUM_COPY)
  // if (NewB16Count >= LastB16Count - 1) then Exit P (return made none)
  // else Exit TMediumManager.Reduce(FLAG_MEDIUM_COPY)
  {$ifdef CPUX86}
    shl ecx, 4
    movzx ebx, [EDX - 16].THeaderMedium.WSize
    sub ebx, ecx
    shr ecx, 4
    lea eax, [EAX].TThreadHeap.FMedium
    cmp ebx, 16
  {$else .CPUX64}
    shl r8, 4
    movzx r9, [RDX - 16].THeaderMedium.WSize
    sub r9, r8
    shr r8, 4
    lea rcx, [RCX].TThreadHeap.FMedium
    cmp r9, 16
  {$endif}
  jbe @return_made_none
  {$ifdef CPUX86}
    push offset @return_var_p
    DB $8D, $89 // lea ecx, [ecx + FLAG_MEDIUM_COPY]
    DD FLAG_MEDIUM_COPY
  {$else .CPUX64}
    push r10
    lea r9, @return_var_p
    push r9
    lea r8, [r8 + FLAG_MEDIUM_COPY]
  {$endif}
  jl TMediumManager.Grow
  jmp TMediumManager.Reduce

@not_small:
  // if (P(v2) and MASK_K4_TEST = 0) then BigOrLarge
  {$ifdef CPUX86}
    test edx, MASK_K4_TEST
  {$else .CPUX64}
    test rdx, MASK_K4_TEST
  {$endif}
  jz @realloc_big_large

  // if (PK64PoolSmall(P).ThreadHeap = 0) then May be medium
  // else ThreadHeap.RaiseInvalidPtr
  {$ifdef CPUX86}
    cmp [EBX].TK64PoolSmall.ThreadHeap, 0
  {$else .CPUX64}
    cmp [R9].TK64PoolSmall.ThreadHeap, 0
  {$endif}
  je @medium
  jmp @raise_invalid_ptr

@realloc_big_large:
  // Exit BrainMM"ReallocMemoryPages"(P, PagesOf(NewB16Count), PAGESMODE_SYSTEM);
  {$ifdef CPUX86}
    mov eax, edx
    lea edx, [ecx + B16_PER_PAGE - 1]
    mov ecx, RESIZE_PAGES_SYSTEM_REALLOC
    shr edx, B16_PER_PAGE_SHIFT
  {$else .CPUX64}
    mov rcx, rdx
    lea rdx, [r8 + B16_PER_PAGE - 1]
    mov r8d, RESIZE_PAGES_SYSTEM_REALLOC
    shr rdx, B16_PER_PAGE_SHIFT
    push r10
  {$endif}
  call BrainMMResizeMemoryPages
  {$ifdef CPUX86}
    cmp eax, 1 // PTR_INVALID
  {$else .CPUX64}
    cmp rax, 1 // PTR_INVALID
  {$endif}
  ja @return_var_p
  je @raise_invalid_pages
  {$ifdef CPUX86}
    push offset @return_var_p
    mov eax, fs:[THREAD_HEAP]
  {$else .CPUX64}
    lea r9, @return_var_p
    push r9
    mov rcx, gs:[abs THREAD_HEAP]
  {$endif}
  jmp TThreadHeap.ErrorOutOfMemory

@realloc_difficult:
  {$ifdef CPUX86}
    push dword ptr [esp + 8]
    add edx, 1
  {$else .CPUX64}
    mov r9, [rsp]
    push r10
    add rdx, 1
  {$endif}
  call TThreadHeap.ResizeDifficult
@return_var_p:
  {$ifdef CPUX86}
    pop ecx
    pop ebx
    mov [ecx], eax
  {$else .CPUX64}
    pop rcx
    mov [rcx], rax
  {$endif}
  ret
@return_unknown:
  {$ifdef CPUX86}
    mov eax, ebx
    push dword ptr [esp + 8]
    push offset @return_var_p
    add ecx, MASK_HIGH_NATIVE_BIT
  {$else .CPUX64}
    mov rcx, r9
    mov r9, [rsp]
    push r10
    lea r10, @return_var_p
    push r10
    mov r10, MASK_HIGH_NATIVE_BIT
    add r8, r10
  {$endif}
  jmp BrainMMUnknownResizeMem
@raise_invalid_pages:
  {$ifdef CPUX64}
    pop r10
  {$endif}
@raise_invalid_ptr:
  {$ifdef CPUX86}
    pop ecx
    pop ebx
  {$endif}
  jmp TThreadHeap.RaiseInvalidPtr
@penalty_thread_deferreds:
  {$ifdef CPUX86}
    lea ebx, @thread_deferreds_done
  {$else .CPUX64}
    lea r9, @thread_deferreds_done
  {$endif}
  jmp SafeProcessThreadDeferred
end;
{$endif}

function TThreadHeap.GrowSmallToSmall(P: Pointer;
  NewB16Count: NativeUInt): Pointer;
{$ifdef PUREPASCAL}
var
  LastB16Count: NativeUInt;
begin
  Result := Self.GetSmall(NewB16Count);
  if (Result <> nil) then
  begin
    LastB16Count := NativeUInt(PK1LineSmall(NativeInt(P) and MASK_K1_CLEAR).Header.ModeSize) shr 4;
    Result := NcMoveB16Small(P16(P)^, P16(Result)^, LastB16Count);
  end;

  if (Self.FreeSmall(P) = FREEMEM_INVALID) then
    Self.RaiseInvalidPtr;
end;
{$else}
asm
  // store P
  // Result := Self.GetSmall(NewB16Count);
  // if (Result = nil) then Exit
  {$ifdef CPUX86}
    push ebx
    mov ebx, edx
    mov edx, ecx
    call TThreadHeap.GetSmall
    test eax, eax
  {$else .CPUX64}
    push rdx
    mov rdx, r8
    call TThreadHeap.GetSmall
    test rax, rax
  {$endif}
  jz @none_allocated

  // store Result
  // Line := LineOf(stored P)
  // Index := IndexOf(Line, stored P)
  {$ifdef CPUX86}
    push eax
    mov edx, ebx
    mov ecx, MASK_K1_TEST
    and ecx, ebx
    and edx, MASK_K1_CLEAR
    shr ecx, 4
  {$else .CPUX64}
    mov r10, rax
    mov rdx, [rsp]
    mov eax, MASK_K1_TEST
    and rax, rdx
    and rdx, MASK_K1_CLEAR
    shr rax, 4
  {$endif}

  // if (Line.FullQueue) then Exit Self.PenaltyGrowSmallToSmall(mode FullQueue)
  // if (not BitUnreserve(Index)) then Exit Self.ErrorInvalidPtr
  {$ifdef CPUX86}
    test [EDX].TK1LineSmall.Header.ItemSet.VLow32, 3
    jnz @penalty_copy_freemem
    lea eax, [EDX].TK1LineSmall.Header.ItemSet.VHigh32
    test ecx, 32
    DB $0F, $45, $D0 // cmovnz edx, eax
    and ecx, 31
    mov eax, [edx]
    bts eax, ecx
    mov [edx], eax
    jc @penalty_error
  {$else .CPUX64}
    mov r8, [RDX].TK1LineSmall.Header.ItemSet.V64
    test r8, 3
    jnz @penalty_copy_freemem
    bts r8, rax
    mov [RDX].TK1LineSmall.Header.ItemSet.V64, r8
    jc @penalty_error
  {$endif}

  // if (Line.Header.ItemSet.V64 = DEFAULT_BITSETS_SMALL[BitSetKind(Line)]) then
  // Exit Self.PenaltyGrowSmallToSmall(mode DisposeK1LineSmall)
  {$ifdef CPUX86}
    and edx, MASK_K1_CLEAR
    mov ecx, 15
    and ecx, [EDX].TK1LineSmall.Header.Flags
    mov eax, [offset DEFAULT_BITSETS_SMALL + ecx * 8]
    mov ecx, [offset DEFAULT_BITSETS_SMALL + ecx * 8 + 4]
    sub eax, [edx]
    sub ecx, [edx + 4]
    or eax, ecx
    jz @penalty_copy_freemem
  {$else .CPUX64}
    mov rax, [RDX].TK1LineSmall.Header.Flags
    and rax, 15
    lea r9, DEFAULT_BITSETS_SMALL
    cmp r8, [r9 + rax * 8]
    je @penalty_copy_freemem
  {$endif}

  // Result := NcMoveB16Small(P16(P)^, P16(Result)^, LineOf(P).B16Count)
  {$ifdef CPUX86}
    pop edx
    mov eax, ebx
    and ebx, MASK_K1_CLEAR
    movzx ecx, [EBX].TK1LineSmall.Header.ModeSize
    shr ecx, 4
    pop ebx
  {$else .CPUX64}
    pop r8
    mov rdx, r10
    mov rcx, r8
    and r8, MASK_K1_CLEAR
    movzx r8, [R8].TK1LineSmall.Header.ModeSize
    shr r8, 4
  {$endif}
  jmp NcMoveB16Small

@penalty_copy_freemem:
  {$ifdef CPUX86}
    pop ecx
    mov eax, ebx
    mov edx, ebx
    pop ebx
    and eax, MASK_K64_CLEAR
    mov eax, [EAX].TK64PoolSmall.ThreadHeap
  {$else .CPUX64}
    pop rdx
    mov r8, r10
    mov rcx, rdx
    and rcx, MASK_K64_CLEAR
    mov rcx, [RCX].TK64PoolSmall.ThreadHeap
  {$endif}
  jmp TThreadHeap.PenaltyGrowSmallToSmall
@penalty_error:
  {$ifdef CPUX86}
    mov edx, ebx
    pop ebx
    and edx, MASK_K64_CLEAR
    mov eax, [EDX].TK64PoolSmall.ThreadHeap
  {$else .CPUX64}
    pop rdx
    and rdx, MASK_K64_CLEAR
    mov rcx, [RDX].TK64PoolSmall.ThreadHeap
  {$endif}
  jmp TThreadHeap.RaiseInvalidPtr
@none_allocated:
  // Self.FreeSmall(P)
  {$ifdef CPUX86}
    mov eax, ebx
    mov edx, ebx
    pop ebx
    and eax, MASK_K64_CLEAR
    mov eax, [EAX].TK64PoolSmall.ThreadHeap
    push eax
  {$else .CPUX64}
    pop rdx
    mov rcx, rdx
    and rcx, MASK_K64_CLEAR
    mov rcx, [RCX].TK64PoolSmall.ThreadHeap
    push rcx
  {$endif}
  call TThreadHeap.FreeSmall

  // if (Ret = FREEMEM_INVALID) then RaiseInvalidPtr
  cmp eax, FREEMEM_INVALID
  {$ifdef CPUX86}
    pop eax
  {$else .CPUX64}
    pop rcx
  {$endif}
  je TThreadHeap.RaiseInvalidPtr

  // Result := nil
  {$ifdef CPUX86}
    xor eax, eax
  {$else .CPUX64}
    xor rax, rax
  {$endif}
end;
{$endif}

{$ifNdef PUREPASCAL}
function TThreadHeap.PenaltyGrowSmallToSmall(P: Pointer; Dest: Pointer): Pointer;
var
  Line, Prev, Next: PK1LineSmall;
  Index, B16Count: NativeInt;

  Mask: NativeInt;
  {$ifdef SMALLINT}
  PVInteger: PInteger;
  {$endif}
begin
  Line := PK1LineSmall(NativeInt(P) and MASK_K1_CLEAR);
  Result := NcMoveB16Small(P16(P)^, P16(Dest)^, {LastB16Count}NativeUInt(Line.Header.ModeSize) shr 4);

  if (Line.Header.ItemSet.VLow32 and 3 <> 0{FullQueue}) then
  begin
    // Dequeue QK1LineFull
    Prev := Line.Header.Queue.Prev;
    Next := Line.Header.Queue.Next;
    if (Prev = nil) then
    begin
      Self.QK1LineFull := Next;
    end else
    begin
      Prev.Header.Queue.Next := Next;
    end;
    if (Next <> nil) then
    begin
      Next.Header.Queue.Prev := Prev;
    end;

    // Enqueue
    B16Count := NativeUInt(Line.Header.ModeSize) shr 4;
    Next := FK1LineSmalls[B16Count];
    FK1LineSmalls[B16Count] := Line;
    Line.Header.Queue.Prev := nil;
    Line.Header.Queue.Next := Next;
    if (Next <> nil) then Next.Header.Queue.Prev := Line;

    // full bitset, unreserve
    Index := (NativeInt(P) and MASK_K1_TEST) shr 4;
    Mask := 1;
    {$ifdef LARGEINT}
      Mask := Mask shl Index;
      Line.Header.ItemSet.V64 := Mask;
    {$else .SMALLINT}
      Mask := Mask shl (Index and 31);
      PVInteger := @Line.Header.ItemSet.VIntegers[Byte(Index > 31)];
      Line.Header.ItemSet.VLow32{V64} := 0;
      PVInteger^ := Mask;
    {$endif}
  end else
  begin
    if (Self.DisposeK1LineSmall(Line) = FREEMEM_INVALID) then
      Result := Pointer(Self.RaiseInvalidPtr);
  end;
end;
{$endif}


procedure InitializeMediumIndexes;
var
  i: NativeInt;
begin
  for i := 0 to 31 do
  begin
    MEDIUM_MASKS_CLEAR[i - 1] := Cardinal(NativeInt(-1) shl i);
  end;

  MEDIUM_INDEXES[0] := -1;
  FillChar(MEDIUM_INDEXES[16 div 16], -(16 - 48) div 16, 0);
  FillChar(MEDIUM_INDEXES[48 div 16], -(48 - 64) div 16, 1);
  FillChar(MEDIUM_INDEXES[64 div 16], -(64 - 80) div 16, 2);
  FillChar(MEDIUM_INDEXES[80 div 16], -(80 - 96) div 16, 3);
  FillChar(MEDIUM_INDEXES[96 div 16], -(96 - 128) div 16, 4);
  FillChar(MEDIUM_INDEXES[128 div 16], -(128 - 160) div 16, 5);
  FillChar(MEDIUM_INDEXES[160 div 16], -(160 - 192) div 16, 6);
  FillChar(MEDIUM_INDEXES[192 div 16], -(192 - 224) div 16, 7);
  FillChar(MEDIUM_INDEXES[224 div 16], -(224 - 272) div 16, 8);
  FillChar(MEDIUM_INDEXES[272 div 16], -(272 - 320) div 16, 9);
  FillChar(MEDIUM_INDEXES[320 div 16], -(320 - 384) div 16, 10);
  FillChar(MEDIUM_INDEXES[384 div 16], -(384 - 448) div 16, 11);
  FillChar(MEDIUM_INDEXES[448 div 16], -(448 - 512) div 16, 12);
  FillChar(MEDIUM_INDEXES[512 div 16], -(512 - 576) div 16, 13);
  FillChar(MEDIUM_INDEXES[576 div 16], -(576 - 672) div 16, 14);
  FillChar(MEDIUM_INDEXES[672 div 16], -(672 - 800) div 16, 15);
  FillChar(MEDIUM_INDEXES[800 div 16], -(800 - 1024) div 16, 16);
  FillChar(MEDIUM_INDEXES[1024 div 16], -(1024 - 1360) div 16, 17);
  FillChar(MEDIUM_INDEXES[1360 div 16], -(1360 - 1696) div 16, 18);
  FillChar(MEDIUM_INDEXES[1696 div 16], -(1696 - 2144) div 16, 19);
  FillChar(MEDIUM_INDEXES[2144 div 16], -(2144 - 2592) div 16, 20);
  FillChar(MEDIUM_INDEXES[2592 div 16], -(2592 - 3040) div 16, 21);
  FillChar(MEDIUM_INDEXES[3040 div 16], -(3040 - 3712) div 16, 22);
  FillChar(MEDIUM_INDEXES[3712 div 16], -(3712 - 4384) div 16, 23);
  FillChar(MEDIUM_INDEXES[4384 div 16], -(4384 - 5056) div 16, 24);
  FillChar(MEDIUM_INDEXES[5056 div 16], -(5056 - 5728) div 16, 25);
  FillChar(MEDIUM_INDEXES[5728 div 16], -(5728 - 6144) div 16, 26);
  FillChar(MEDIUM_INDEXES[6144 div 16], -(6144 - 8192) div 16, 27);
  FillChar(MEDIUM_INDEXES[8192 div 16], -(8192 - 12288) div 16, 28);
  FillChar(MEDIUM_INDEXES[12288 div 16], -(12288 - 16384) div 16, 29);
  FillChar(MEDIUM_INDEXES[16384 div 16], -(16384 - 24576) div 16, 30);
  FillChar(MEDIUM_INDEXES[24576 div 16], -(24576 - 65536) div 16, 31);
end;

type
  TMediumAlignOffsets = packed record
    Values: array[0..255] of Word;
  case Integer of
    0: (AllocatedFlags: Cardinal);
    1: (Null: Word; Allocated: Boolean; Align: TMemoryAlign);
  end;
  PMediumAlignOffsets = ^TMediumAlignOffsets;

var
  MEDIUM_ALIGN_OFFSETS{from Header}: array[0..Ord(High(TMemoryAlign))] of TMediumAlignOffsets;

procedure InitializeMediumOffsets;
const
  MASK_MEDIUM_ALIGNS: array[0..Ord(High(TMemoryAlign))] of NativeUInt = (
    {ma16Bytes}   16 - 1,
    {ma32Bytes}   32 - 1,
    {ma64Bytes}   64 - 1,
    {ma128Bytes}  128 - 1,
    {ma256Bytes}  256 - 1,
    {ma512Bytes}  512 - 1,
    {ma1024Bytes} 1024 - 1,
    {ma2048Bytes} 2048 - 1
  );
var
  Align, AlignMaskTest, AlignMaskClear: NativeUInt;
  AlignOffsets: PMediumAlignOffsets;
  X, Value: NativeUInt;
begin
  for Align := 0 to Ord(High(TMemoryAlign)) do
  begin
    AlignOffsets := @MEDIUM_ALIGN_OFFSETS[Align];
    AlignOffsets.AllocatedFlags := (Align shl OFFSET_MEDIUM_ALIGN) + MASK_MEDIUM_ALLOCATED;
    AlignMaskTest := MASK_MEDIUM_ALIGNS[Align];
    AlignMaskClear := (not AlignMaskTest);

    for X := 0 to 255 do
    begin
      Value := (X + 1) shl 4;
      Value := (Value + AlignMaskTest) and AlignMaskClear;
      Value := (Value + Byte(Value and MASK_K4_TEST = 0) + AlignMaskTest) and AlignMaskClear;
      AlignOffsets.Values[X] := Value - (X shl 4) - 16;
    end;
  end;
end;

function TMediumManager.ErrorOutOfMemory: Pointer;
{$ifdef CPUINTEL}
asm
  {$ifdef CPUX86}
    lea eax, [EAX - TThreadHeap.FMedium]
  {$else .CPUX64}
    lea rcx, [RCX - TThreadHeap.FMedium]
  {$endif}
  jmp TThreadHeap.ErrorOutOfMemory
end;
{$else}
begin
  Result := PThreadHeap(NativeInt(@Self) - NativeInt(@PThreadHeap(nil).FMedium)).ErrorOutOfMemory;
end;
{$endif}

function TMediumManager.ErrorInvalidPtr: Integer;
{$ifdef CPUINTEL}
asm
  {$ifdef CPUX86}
    lea eax, [EAX - TThreadHeap.FMedium]
  {$else .CPUX64}
    lea rcx, [RCX - TThreadHeap.FMedium]
  {$endif}
  jmp TThreadHeap.ErrorInvalidPtr
end;
{$else}
begin
  Result := PThreadHeap(NativeInt(@Self) - NativeInt(@PThreadHeap(nil).FMedium)).ErrorInvalidPtr;
end;
{$endif}

function TMediumManager.RaiseOutOfMemory: Pointer;
{$ifdef CPUINTEL}
asm
  {$ifdef CPUX86}
    lea eax, [EAX - TThreadHeap.FMedium]
  {$else .CPUX64}
    lea rcx, [RCX - TThreadHeap.FMedium]
  {$endif}
  jmp TThreadHeap.RaiseOutOfMemory
end;
{$else .CPUARM.CPUARM64}
{$ifNdef RETURNADDRESS}
const
  ReturnAddress: Pointer = @TMediumManager.RaiseOutOfMemory;
{$endif}
var
  ThreadHeap: PThreadHeap;
begin
  ThreadHeap := PThreadHeap(NativeInt(@Self) - NativeInt(@PThreadHeap(nil).FMedium));
  if (ThreadHeap.ErrorAddr = nil) then ThreadHeap.ErrorAddr := ReturnAddress;
  Result := Self.ErrorOutOfMemory;
end;
{$endif}

function TMediumManager.RaiseInvalidPtr: Integer;
{$ifdef CPUINTEL}
asm
  {$ifdef CPUX86}
    lea eax, [EAX - TThreadHeap.FMedium]
  {$else .CPUX64}
    lea rcx, [RCX - TThreadHeap.FMedium]
  {$endif}
  jmp TThreadHeap.RaiseInvalidPtr
end;
{$else .CPUARM.CPUARM64}
{$ifNdef RETURNADDRESS}
const
  ReturnAddress: Pointer = @TMediumManager.RaiseInvalidPtr;
{$endif}
var
  ThreadHeap: PThreadHeap;
begin
  ThreadHeap := PThreadHeap(NativeInt(@Self) - NativeInt(@PThreadHeap(nil).FMedium));
  if (ThreadHeap.ErrorAddr = nil) then ThreadHeap.ErrorAddr := ReturnAddress;
  Result := Self.ErrorInvalidPtr;
end;
{$endif}

// returns last empty Pointer
// empty size should be actual filled
function TMediumManager.ExcludeEmpty(LastIndex: NativeUInt; Empty: PHeaderMediumEmpty): Pointer;
{$ifdef PUREPASCAL}
var
  Prev, Next: PHeaderMediumEmpty;
begin
  {$ifdef DEBUG}
  if (Self.FMask and (1 shl LastIndex) = 0) then
  begin
    Result := Pointer(Self.RaiseInvalidPtr);
    Exit;
  end;
  {$endif}

  // dequeue
  Prev := Empty.Prev;
  Next := Empty.Next;
  if (NativeInt(Prev) or NativeInt(Next) = 0) then
  begin
    Self.FEmpties[LastIndex] := Next{nil};
    Self.FMask := Self.FMask and (not (1 shl LastIndex));
  end else
  begin
    if (Prev = nil) then
    begin
      Self.FEmpties[LastIndex] := Next;
    end else
    begin
      Prev.Next := Next;
    end;
    if (Next <> nil) then
    begin
      Next.Prev := Prev;
    end;
  end;

  // result
  Result := Pointer(NativeUInt(@Empty.SiblingHeaderMedium) - Empty.Size);
end;
{$else}
asm
  {$ifdef DEBUG}
    // if (not Self.FMask.Checked(LastIndex)) then Exit Self.RaiseInvalidPtr
    {$ifdef CPUX86}
      push ecx
      mov ecx, [EAX].TMediumManager.FMask
      bt ecx, edx
      pop ecx
    {$else .CPUX64}
      mov rax, [RCX].TMediumManager.FMask
      bt rax, rdx
    {$endif}
    jnc TMediumManager.RaiseInvalidPtr
  {$endif}

  // prev(v4)/next(v5)
  // if (Prev = nil) and (Next = nil) then goto index_clear
  {$ifdef CPUX86}
    push ebx
    push esi
    mov ebx, [ECX].THeaderMediumEmpty.Prev
    mov esi, [ECX].THeaderMediumEmpty.Next
    or esi, ebx
    mov esi, [ECX].THeaderMediumEmpty.Next
  {$else .CPUX64}
    mov rax, [R8].THeaderMediumEmpty.Prev
    mov r10, [R8].THeaderMediumEmpty.Next
    mov r9, rax
    or rax, r10
  {$endif}
  jz @index_clear

  // (Prev(v4) = nil)? Self.FEmpties[LastIndex]/Prev(v4).Next := Next(v5)
  {$ifdef CPUX86}
    lea eax, [EAX + edx * 4]
    lea edx, [EBX].THeaderMediumEmpty.Next
    test ebx, ebx
    DB $0F, $45, $C2 // cmovnz eax, edx
    mov [eax], esi
  {$else .CPUX64}
    lea rcx, [RCX + rdx * 8]
    lea rdx, [R9].THeaderMediumEmpty.Next
    test r9, r9
    cmovnz rcx, rdx
    mov [rcx], r10
  {$endif}

  // (Next(v5) <> nil)? Next.Prev := Prev(v4)
  {$ifdef CPUX86}
    lea eax, [esp - 4]
    lea edx, [ESI].THeaderMediumEmpty.Prev
    test esi, esi
    DB $0F, $45, $C2 // cmovnz eax, edx
    mov [eax], ebx
  {$else .CPUX64}
    lea rcx, [rsp - 8]
    lea rdx, [R10].THeaderMediumEmpty.Prev
    test r10, r10
    cmovnz rcx, rdx
    mov [rcx], r9
  {$endif}

  // result: Pointer(NativeUInt(@Empty.SiblingHeaderMedium) - Empty.Size)
  {$ifdef CPUX86}
    lea eax, [ECX].THeaderMediumEmpty.SiblingHeaderMedium
    mov ecx, [ECX].THeaderMediumEmpty.Size
    sub eax, ecx
    pop esi
    pop ebx
  {$else .CPUX64}
    lea rax, [R8].THeaderMediumEmpty.SiblingHeaderMedium
    mov r8, [R8].THeaderMediumEmpty.Size
    sub rax, r8
  {$endif}
  ret

@index_clear:
  // Self.FEmpties[LastIndex] := nil / Self.FMask.Uncheck(LastIndex)
  {$ifdef CPUX86}
     mov [EAX + edx * 4], ebx
     mov ebx, [EAX].TMediumManager.FMask
     btr ebx, edx
     mov [EAX].TMediumManager.FMask, ebx
  {$else .CPUX64}
     mov [RCX + rdx * 8], r9
     mov r9, [RCX].TMediumManager.FMask
     btr r9, rdx
     mov [RCX].TMediumManager.FMask, r9
  {$endif}

  // result (duplicate): Pointer(NativeUInt(@Empty.SiblingHeaderMedium) - Empty.Size)
  {$ifdef CPUX86}
    lea eax, [ECX].THeaderMediumEmpty.SiblingHeaderMedium
    mov ecx, [ECX].THeaderMediumEmpty.Size
    sub eax, ecx
    pop esi
    pop ebx
  {$else .CPUX64}
    lea rax, [R8].THeaderMediumEmpty.SiblingHeaderMedium
    mov r8, [R8].THeaderMediumEmpty.Size
    sub rax, r8
  {$endif}
end;
{$endif}

// returns allocated Pointer before empty area
// empty size/prevous should be actual filled
function TMediumManager.ChangeEmptyIndex(Flags: NativeUInt{LastIndex, NewIndex:5}; Empty: PHeaderMediumEmpty): Pointer;
{$ifdef PUREPASCAL}
label
  set_b16count;
var
  LastIndex, NewIndex: NativeInt;
  Prev, Next: PHeaderMediumEmpty;
begin
  LastIndex := Flags and 31;

  // dequeue
  {$ifdef DEBUG}
  if (Self.FMask and (1 shl LastIndex) = 0) then
  begin
    Result := Pointer(Self.RaiseInvalidPtr);
    Exit;
  end;
  {$endif}
  Prev := Empty.Prev;
  Next := Empty.Next;
  if (NativeInt(Prev) or NativeInt(Next) = 0) then
  begin
    Self.FEmpties[LastIndex] := Next{nil};
    Self.FMask := Self.FMask and (not (1 shl LastIndex));
  end else
  begin
    if (Prev = nil) then
    begin
      Self.FEmpties[LastIndex] := Next;
    end else
    begin
      Prev.Next := Next;
    end;
    if (Next <> nil) then
    begin
      Next.Prev := Prev;
    end;
  end;

  // enqueue
  NewIndex := Flags shr 5;
  with Self do
  begin
    Next := FEmpties[NewIndex];
    {$ifdef DEBUG}
    if ((Self.FMask shr NewIndex) and 1 <> Byte(Next <> nil)) then
    begin
      Result := Pointer(Self.RaiseInvalidPtr);
      Exit;
    end;
    {$endif}
    FEmpties[NewIndex] := Empty;
    FMask := FMask or (1 shl NewIndex);
  end;
  Empty.Next := Next;
  if (Next <> nil) then
  begin
    Next.Prev := Empty;
  end;
  Empty.Prev := nil;

  // result
  Dec(NativeUInt(Empty), Empty.Size);
  Result := Pointer(NativeUInt(Empty) - PHeaderMedium(Empty).PreviousSize);
end;
{$else}
asm
  // x64: Flags --> LastIndex(v2), NewIndex(v6)
  {$ifdef CPUX64}
    mov r11, rdx
    and rdx, 31
    shr r11, 5
  {$endif}

  {$ifdef DEBUG}
    // if (not Self.FMask.Checked(LastIndex)) then Exit Self.RaiseInvalidPtr
    {$ifdef CPUX86}
      push ecx
      mov ecx, [EAX].TMediumManager.FMask
      bt ecx, edx // 5 bits: 0..31
      pop ecx
    {$else .CPUX64}
      mov rax, [RCX].TMediumManager.FMask
      bt rax, rdx
    {$endif}
    jnc TMediumManager.RaiseInvalidPtr
  {$endif}

  // prev(v4)/next(v5), x86: store flags(v6)
  // if (Prev = nil) and (Next = nil) then goto index_clear
  {$ifdef CPUX86}
    push ebx
    push esi
    push edi
    mov ebx, [ECX].THeaderMediumEmpty.Prev
    mov esi, [ECX].THeaderMediumEmpty.Next
    mov edi, ebx
    or edi, esi
    mov edi, edx
  {$else .CPUX64}
    mov rax, [R8].THeaderMediumEmpty.Prev
    mov r10, [R8].THeaderMediumEmpty.Next
    mov r9, rax
    or rax, r10
  {$endif}
  jz @index_clear

  // x86: Flags --> LastIndex(edx)
  // (Prev(v4) = nil)? Self.FEmpties[LastIndex]/Prev(v4).Next := Next(v5)
  {$ifdef CPUX86}
    and edx, 31
    test ebx, ebx
    lea edx, [EAX + edx * 4]
    lea ebx, [EBX + THeaderMediumEmpty.Next]
    DB $0F, $45, $D3 // cmovnz edx, ebx
    lea ebx, [EBX - THeaderMediumEmpty.Next]
    mov [edx], esi
  {$else .CPUX64}
    test r9, r9
    lea rdx, [RCX + rdx * 8]
    lea rax, [R9].THeaderMediumEmpty.Next
    cmovnz rdx, rax
    mov [rdx], r10
  {$endif}

  // (Next(v5) <> nil)? Next.Prev := Prev(v4)
  {$ifdef CPUX86}
    test esi, esi
    lea edx, [esp - THeaderMediumEmpty.Prev - 4]
    DB $0F, $44, $F2 // cmovz esi, edx
    mov [ESI].THeaderMediumEmpty.Prev, ebx
  {$else .CPUX64}
    test r10, r10
    lea rdx, [rsp - THeaderMediumEmpty.Prev - 8]
    cmovz r10, rdx
    mov [r10], r9
  {$endif}

@enqueue:
  // x86: Flags(v6) --> NewIndex(v6)
  // Next(v5) := Self.FEmpties[NewIndex(v6)]
  {$ifdef CPUX86}
    shr edi, 5
    mov esi, [EAX + edi * 4]
  {$else .CPUX64}
    mov r10, [RCX + r11 * 8]
  {$endif}

  {$ifdef DEBUG}
    // if ((Self.FMask shr NewIndex) and 1 <> Byte(Next <> nil)) then Exit Self.RaiseInvalidPtr
    {$ifdef CPUX86}
      push ecx
      mov ebx, [EAX].TMediumManager.FMask
      mov ecx, edi
      shr ebx, cl
      and ebx, 1
      test esi, esi
      setnz cl
      movzx ecx, cl
      cmp ebx, ecx
      pop ecx
      je @correctly
      pop edi
      pop esi
      pop ebx
    {$else .CPUX64}
      push rcx
      mov r9, [RCX].TMediumManager.FMask
      mov rcx, r11
      shr r9, cl
      and r9, 1
      test r10, r10
      setnz cl
      movzx rcx, cl
      cmp r9, rcx
      pop rcx
      je @correctly
    {$endif}
    jmp TMediumManager.RaiseInvalidPtr
  @correctly:
  {$endif}

  // Self.FEmpties[NewIndex] := Empty, Self.FMask.SetChecked(NewIndex), Empty.Next := Next
  {$ifdef CPUX86}
     mov [EAX + edi * 4], ecx
     mov ebx, [EAX].TMediumManager.FMask
     bts ebx, edi
     mov [EAX].TMediumManager.FMask, ebx
     mov [ECX].THeaderMediumEmpty.Next, esi
  {$else .CPUX64}
     mov [RCX + r11 * 8], r8
     mov r9, [RCX].TMediumManager.FMask
     bts r9, r11
     mov [RCX].TMediumManager.FMask, r9
     mov [R8].THeaderMediumEmpty.Next, r10
  {$endif}

  // (Next(v5) <> nil)? Next.Prev := Empty(v3), Empty.Prev := nil
  {$ifdef CPUX86}
    test esi, esi
    lea edx, [esp - THeaderMediumEmpty.Prev - 4]
    DB $0F, $44, $F2 // cmovz esi, edx
    mov [ESI].THeaderMediumEmpty.Prev, ecx
    xor edx, edx
    mov [ECX].THeaderMediumEmpty.Prev, edx
  {$else .CPUX64}
    test r10, r10
    lea rdx, [rsp - THeaderMediumEmpty.Prev - 8]
    cmovz r10, rdx
    mov [r10], r8
    xor rdx, rdx
    mov [R8].THeaderMediumEmpty.Prev, rdx
  {$endif}

  // result: empty - empty.size - prev_empty.size
  {$ifdef CPUX86}
    mov edx, [ECX].THeaderMediumEmpty.Size
    sub ecx, edx
    mov edx, [ECX].THeaderMedium.PreviousSize
    sub ecx, edx
    pop edi
    pop esi
    pop ebx
    xchg eax, ecx
  {$else .CPUX64}
    mov rdx, [R8].THeaderMediumEmpty.Size
    sub r8, rdx
    mov rdx, [R8].THeaderMedium.PreviousSize
    sub r8, rdx
    xchg rax, r8
  {$endif}
  ret

@index_clear:
  // x86: Flags --> LastIndex(edx)
  // Self.FEmpties[LastIndex] := nil / Self.FMask.Uncheck(LastIndex)
  {$ifdef CPUX86}
     and edx, 31
     mov [EAX + edx * 4], ebx
     mov ebx, [EAX].TMediumManager.FMask
     btr ebx, edx
     mov [EAX].TMediumManager.FMask, ebx
  {$else .CPUX64}
     mov [RCX + rdx * 8], r9
     mov r9, [RCX].TMediumManager.FMask
     btr r9, rdx
     mov [RCX].TMediumManager.FMask, r9
  {$endif}
  jmp @enqueue
end;
{$endif}

// returns Pointer after aligning erea
// called in .GetAdvanced and .GrowPenalty
// (before left) must be allocated
function TMediumManager.AlignOffsetEmpty(Header: PHeaderMedium; AlignOffset: NativeUInt): Pointer;
const
  MASK_PREVIOUS_TEST = MASK_MEDIUM_EMPTY_TEST and MASK_MEDIUM_ALLOCATED_TEST;
{$ifdef PUREPASCAL}
label
  invalid_ptr;
var
  Flags: NativeUInt;
  Empty, Next: PHeaderMediumEmpty;
begin
  Dec(AlignOffset, 16);
  Dec(Header);
  if (AlignOffset <> 0) then
  begin
    { new empty }
    Empty := Pointer(Header);
    Dec(NativeUInt(Header), AlignOffset);
    Empty.Size := AlignOffset;
    Header.Flags := AlignOffset;

    // inline IncludeEmpty
    begin
      AlignOffset := Byte(MEDIUM_INDEXES[AlignOffset shr 4]);
      with Self do
      begin
        Next := FEmpties[AlignOffset];
        FEmpties[AlignOffset] := Empty;
        FMask := FMask or (1 shl AlignOffset);
      end;
      Empty.Next := Next;
      if (Next <> nil) then
      begin
        Next.Prev := Empty;
      end;
      Empty.Prev := nil;
    end;

    Result := @Empty.SiblingPtr;
    Exit;
  end else
  begin
    { grow allocated 16 bytes }
    AlignOffset := Header.PreviousSize;
    Dec(Header);
    if (AlignOffset and MASK_PREVIOUS_TEST <> 0) then goto invalid_ptr;
    Dec(NativeUInt(Header), AlignOffset);
    Flags := Header.Flags;
    if (Word(Flags) <> Word(AlignOffset)) then goto invalid_ptr;
    Flags := Flags and MASK_MEDIUM_ALLOCATED_TEST;
    Inc(AlignOffset, 16);
    if (Flags <> MASK_MEDIUM_ALLOCATED_VALUE) then goto invalid_ptr;

    Header.WSize := AlignOffset;
    Inc(NativeUInt(Header), AlignOffset);
    PHeaderMediumEmpty(Header).Size := AlignOffset;
    Result := @PHeaderMediumEmpty(Header).SiblingPtr;
    Exit;

  invalid_ptr:
    Result := Pointer(Self.RaiseInvalidPtr);
  end;
end;
{$else}
asm
  // check align offset, decrement header, x86: store v4
  {$ifdef CPUX86}
    push ebx
    sub ecx, 16
    lea edx, [edx - 16]
  {$else .CPUX64}
    sub r8, 16
    lea rdx, [rdx - 16]
  {$endif}
  jz @grow_allocated_16

  // new empty (v4)
  {$ifdef CPUX86}
    mov [EDX].THeaderMediumEmpty.Size, ecx
    mov ebx, edx
    sub edx, ecx
    mov [EDX].THeaderMedium.Flags, ecx
  {$else .CPUX64}
    mov [RDX].THeaderMediumEmpty.Size, r8
    mov r9, rdx
    sub rdx, r8
    mov [RDX].THeaderMedium.Flags, r8
  {$endif}

  // AlignOffset --> Index
  {$ifdef CPUX86}
    shr ecx, 4
    movzx ecx, byte ptr [MEDIUM_INDEXES + ecx]
  {$else .CPUX64}
    shr r8, 4
    lea r10, MEDIUM_INDEXES
    movzx r8, byte ptr [r10 + r8]
  {$endif}

  // Self.FMask.SetChecked(AlignOffset)
  {$ifdef CPUX86}
    mov edx, [EAX].TMediumManager.FMask
    bts edx, ecx
    mov [EAX].TMediumManager.FMask, edx
  {$else .CPUX64}
    mov rdx, [RCX].TMediumManager.FMask
    bts rdx, r8
    mov [RCX].TMediumManager.FMask, rdx
  {$endif}

  // Next(v2) := Self.FEmpties[AlignOffset] / Self.FEmpties[AlignOffset] := Empty(v4)
  // Empty.Prev := nil / Empty.Next := Next
  {$ifdef CPUX86}
    mov edx, [EAX + ecx * 4]
    mov [EAX + ecx * 4], ebx
    xor ecx, ecx
    mov [EBX].THeaderMediumEmpty.Prev, ecx
    mov [EBX].THeaderMediumEmpty.Next, edx
  {$else .CPUX64}
    mov rdx, [RCX + r8 * 8]
    mov [RCX + r8 * 8], r9
    xor r8, r8
    mov [R9].THeaderMediumEmpty.Prev, r8
    mov [R9].THeaderMediumEmpty.Next, rdx
  {$endif}

  // (Next(v2) <> nil)? Next.Prev := Empty(v4)
  {$ifdef CPUX86}
    test edx, edx
    lea ecx, [esp - THeaderMediumEmpty.Prev - 4]
    DB $0F, $44, $D1 // cmovz edx, ecx
    mov [EDX].THeaderMediumEmpty.Prev, ebx
  {$else .CPUX64}
    test rdx, rdx
    lea r8, [rsp - THeaderMediumEmpty.Prev - 8]
    cmovz rdx, r8
    mov [RDX].THeaderMediumEmpty.Prev, r9
  {$endif}

  // result
  {$ifdef CPUX86}
    lea eax, [EBX].THeaderMediumEmpty.SiblingPtr
    pop ebx
  {$else .CPUX64}
    lea rax, [R9].THeaderMediumEmpty.SiblingPtr
  {$endif}
  ret

@grow_allocated_16:
  // header, new size, checks
  {$ifdef CPUX86}
    mov ecx, [EDX].THeaderMedium.PreviousSize
    sub edx, 16
    test ecx, MASK_PREVIOUS_TEST
    jnz @invalid_ptr
    sub edx, ecx
    mov ebx, [EDX].THeaderMedium.Flags
    cmp bx, cx
    jne @invalid_ptr
    and ebx, MASK_MEDIUM_ALLOCATED_TEST
    add ecx, 16
    cmp ebx, MASK_MEDIUM_ALLOCATED_VALUE
    jne  @invalid_ptr
  {$else .CPUX64}
    mov r8, [RDX].THeaderMedium.PreviousSize
    sub rdx, 16
    test r8, MASK_PREVIOUS_TEST
    jnz @invalid_ptr
    sub rdx, r8
    mov r9, [RDX].THeaderMedium.Flags
    cmp r9w, r8w
    jne @invalid_ptr
    and r9, MASK_MEDIUM_ALLOCATED_TEST
    add r8, 16
    cmp r9, MASK_MEDIUM_ALLOCATED_VALUE
    jne  @invalid_ptr
  {$endif}

  // new size, result
  {$ifdef CPUX86}
     mov [EDX].THeaderMedium.WSize, cx
     add edx, ecx
     mov [EDX].THeaderMediumEmpty.Size, ecx
     lea eax, [EDX].THeaderMediumEmpty.SiblingPtr
     pop ebx
  {$else .CPUX64}
     mov [RDX].THeaderMedium.WSize, r8w
     add rdx, r8
     mov [RDX].THeaderMediumEmpty.Size, r8
     lea rax, [RDX].THeaderMediumEmpty.SiblingPtr
  {$endif}
  ret

@invalid_ptr:
  {$ifdef CPUX86}
     pop ebx
  {$endif}
  jmp TMediumManager.RaiseInvalidPtr
end;
{$endif}

function TMediumManager.Get(B16Count: NativeUInt): Pointer;
{$ifdef PUREPASCAL}
label
  index_done, invalid_ptr, retrieve_result;
var
  Mask, Index: NativeInt;
  P: PByte;
  Empty, Next: PHeaderMediumEmpty;
  Header: PHeaderMedium;
  Size, Last: NativeUInt;
begin
  Index{Mask} := MEDIUM_MASKS_CLEAR[NativeInt(MEDIUM_INDEXES[B16Count - 1])] and Self.FMask;

  if (Index{Mask} <> 0) then
  begin
    // Index := FirstBit(Mask)
    Mask := Index{Mask};
    P := Pointer(@Mask);
    Inc(PWord(P), Byte(PWord(P)^ = 0));
    Inc(P, Byte(P^ = 0));
    Index := (NativeUInt(P) - NativeUInt(@Mask)) shl 3 + BIT_SCANS[P^];

  index_done:
    Empty := Self.FEmpties[Index];
    {$ifdef DEBUG}
    if (Empty = nil) then goto invalid_ptr;
    {$endif}

    Size := B16Count shl 4;
    Last := Empty.Size;
    {$ifdef DEBUG}
    if (Last and MASK_MEDIUM_EMPTY_TEST <> MASK_MEDIUM_EMPTY_VALUE) then goto invalid_ptr;
    if (Index <> MEDIUM_INDEXES[Last shr 4]) then goto invalid_ptr;
    {$endif}
    Header := Pointer(@Empty.Size);
    Dec(NativeUInt(Header), Last);
    if (NativeInt(Header) and MASK_K4_TEST <> 0) then
    begin
      // store Result
      Pointer(Mask) := Header;

      // Header actualize, Vacant := Last - Size
      Dec(Header);
      {$ifdef DEBUG}
      if (Header.Flags <> Last) then goto invalid_ptr;
      {$endif}
      Dec(Last, Size);
      {$ifdef DEBUG}
      if (NativeInt(Last) < 0) then goto invalid_ptr;
      {$endif}

      if (Last{Vacant} > 32) then
      begin
        // mark allocated (Result)
        begin
          // [left...
          Inc(Size, MASK_MEDIUM_ALLOCATED);
          Header.Flags := Size;
          Dec(Size, MASK_MEDIUM_ALLOCATED);

          // ...right]
          Inc(Header);
          Inc(NativeUInt(Header), Size);
          Header.PreviousSize := Size;
        end;

        // mark new empty (Vacant - SizeOf(Header))
        begin
          // [left...
          Dec(Last, 16);
          Header.Flags := Last;

          // ...right]
          Inc(NativeUInt(Header), Last);
          PHeaderMediumEmpty(Header).Size := Last;
        end;

        // detect last(Size+1/16) and new(Last) indexes
        begin
          Inc(Size, Last);
          Last := Last shr 4;
          Size := Size shr 4;
          Last{new} := MEDIUM_INDEXES[Last];
          Size{last} := MEDIUM_INDEXES[Size + 1];
        end;
        if (Size = Last) then goto retrieve_result{in asm: inline ret};

        // inline ChangeEmptyIndex
        begin
          // dequeue
          Empty := PHeaderMediumEmpty(Header);
          Next := Empty.Next;
          Self.FEmpties[Size{last}] := Next;

          // optional clear prev, exclude bit
          if (Next <> nil) then
          begin
            Next.Prev := nil;
            Size{NewFMask} := 0;
          end else
          begin
            Size{NewFMask} := (1 shl Size{lasr});
          end;
          Size{NewFMask} := Size{NewFMask} xor NativeUInt(Self.FMask);

          // enqueue
          Next := Self.FEmpties[Last{new}];
          Empty.Next := Next;
          Self.FEmpties[Last{new}] := Empty;
          Self.FMask := Size{NewFMask} or (1 shl Last{new});
          if (Next <> nil) then
          begin
            Next.Prev := Empty;
          end;
        end;

        goto retrieve_result{in asm: inline ret};
      end else
      begin
        // full: left allocated
        Inc(Size, Last);
        Header.Flags := Size + MASK_MEDIUM_ALLOCATED;
        Inc(NativeUInt(Header), Size);
        { PHeaderMediumEmpty(Header).Size := Size; }

        // exclude empty
        Result := Self.ExcludeEmpty(MEDIUM_INDEXES[Size shr 4], Pointer(Header));
      end;
    end else
    begin
      // 4Kb alignment penalty
      Result := Self.GetAdvanced(Size shr 4, 0);
    end;

    Exit;
  end else
  begin
    if (PThreadHeap(NativeInt(@Self) - NativeInt(@PThreadHeap(nil).FMedium)).NewK64PoolMedium <> nil) then
    begin
      Index := 31;
      goto index_done;
    end else
    begin
      Result := Self.ErrorOutOfMemory;
      Exit;
    end;
  end;

invalid_ptr:
  Result := Pointer(Self.RaiseInvalidPtr);
  Exit;

retrieve_result:
  Result := Pointer(Mask);
end;
{$else}
asm
  // x86: store v4
  // Mask := MEDIUM_MASKS_CLEAR[NativeInt(MEDIUM_INDEXES[B16Count - 1])] and Self.FMask
  // if (Mask = 0) then goto NewK64PoolMedium
  {$ifdef CPUX86}
    push ebx
    movsx ebx, byte ptr [MEDIUM_INDEXES + edx - 1]
    mov ecx, [EAX].TMediumManager.FMask
    mov ebx, dword ptr [MEDIUM_MASKS_CLEAR + ebx * 4 + 4]
    and ebx, ecx
  {$else .CPUX64}
    lea rax, [MEDIUM_INDEXES - 1]
    movsx r9, byte ptr [rax + rdx]
    mov r8, [RCX].TMediumManager.FMask
    lea rax, [MEDIUM_MASKS_CLEAR + 8]
    mov r9, qword ptr [rax + r9 * 8]
    and r9, r8
  {$endif}
  jz @new_pool

  // Index(v3) := FirstBit(Mask(v4))
  {$ifdef CPUX86}
    rep bsf ecx, ebx
  {$else .CPUX64}
    rep bsf r8, r9
  {$endif}

@index_done:
  // Empty; Debug: store Index, check Empty
  {$ifdef DEBUG}
    {$ifdef CPUX86}
       mov [esp - 4], ecx
       mov ecx, [EAX + ecx * 4]
       test ecx, ecx
    {$else .CPUX64}
       mov r10, r8
       mov r8, [RCX + r8 * 8]
       test r8, r8
    {$endif}
    jz @invalid_ptr
  {$else .RELEASE}
    {$ifdef CPUX86}
      mov ecx, [EAX + ecx * 4]
    {$else .CPUX64}
      mov r8, [RCX + r8 * 8]
    {$endif}
  {$endif}

  // Size, Last; Debug: check Last
  {$ifdef CPUX86}
    shl edx, 4
    mov ebx, [ECX].THeaderMediumEmpty.Size
    {$ifdef DEBUG}
      test ebx, MASK_MEDIUM_EMPTY_TEST
      jnz @invalid_ptr
      mov [esp - 8], ebx
      shr ebx, 4
      movzx ebx, byte ptr [MEDIUM_INDEXES + ebx]
      cmp ebx, [esp - 4]
      mov ebx, [esp - 8]
      jne @invalid_ptr
    {$endif}
  {$else .CPUX64}
    shl rdx, 4
    mov r9, [R8].THeaderMediumEmpty.Size
    {$ifdef DEBUG}
      test r9, MASK_MEDIUM_EMPTY_TEST
      jnz @invalid_ptr
      mov r11, r9
      shr r9, 4
      lea rax, MEDIUM_INDEXES
      movzx r9, byte ptr [rax + r9]
      cmp r9, r10
      mov r9, r11
      jne @invalid_ptr
    {$endif}
  {$endif}

  // Result ptr; if (Alignment = 4kb) then Exit Self.GetAdvanced
  {$ifdef CPUX86}
    lea ecx, [ecx + THeaderMediumEmpty.Size]
    sub ecx, ebx
    test ecx, MASK_K4_TEST
  {$else .CPUX64}
    lea r8, [r8 + THeaderMediumEmpty.Size]
    sub r8, r9
    test r8, MASK_K4_TEST
  {$endif}
  jz @penalty

  // vacant size
  // Debug: check flags/vacant
  {$ifdef CPUX86}
    {$ifdef DEBUG}
      cmp [ECX - 16].THeaderMedium.Flags, ebx
      jne @invalid_ptr
    {$endif}
    sub ebx, edx
    {$ifdef DEBUG}
      jl @invalid_ptr
    {$endif}
  {$else .CPUX64}
    {$ifdef DEBUG}
      cmp [R8 - 16].THeaderMedium.Flags, r9
      jne @invalid_ptr
    {$endif}
    sub r9, rdx
    {$ifdef DEBUG}
      jl @invalid_ptr
    {$endif}
  {$endif}

  // if (Last{Vacant} <= 32) then FullAllocated
  {$ifdef CPUX86}
    cmp ebx, 32
  {$else .CPUX64}
    cmp r9, 32
  {$endif}
  jbe @full_allocated

  // store result
  {$ifdef CPUX86}
    mov [esp - 16], ecx
  {$else .CPUX64}
    mov r10, r8
  {$endif}

  // mark allocated (Result)
  {$ifdef CPUX86}
    add edx, MASK_MEDIUM_ALLOCATED
    mov [ecx - 16].THeaderMedium.Flags, edx
    sub edx, MASK_MEDIUM_ALLOCATED
    mov [ecx + edx].THeaderMedium.PreviousSize, edx
    add ecx, edx
  {$else .CPUX64}
    add rdx, MASK_MEDIUM_ALLOCATED
    mov [r8 - 16].THeaderMedium.Flags, rdx
    sub rdx, MASK_MEDIUM_ALLOCATED
    mov [r8 + rdx].THeaderMedium.PreviousSize, rdx
    add r8, rdx
  {$endif}

  // mark new empty (Vacant - SizeOf(Header))
  {$ifdef CPUX86}
    sub ebx, 16
    mov [ECX].THeaderMedium.Flags, ebx
    add ecx, ebx
    mov [ECX].THeaderMediumEmpty.Size, ebx
  {$else .CPUX64}
    sub r9, 16
    mov [R8].THeaderMedium.Flags, r9
    add r8, r9
    mov [R8].THeaderMediumEmpty.Size, r9
  {$endif}

  // detect last(v2) and new(v4) indexes
  {$ifdef CPUX86}
    add edx, ebx
    shr ebx, 4
    shr edx, 4
    movzx ebx, byte ptr [MEDIUM_INDEXES + ebx]
    movzx edx, byte ptr [MEDIUM_INDEXES + edx + 1]
  {$else .CPUX64}
    add rdx, r9
    shr r9, 4
    shr rdx, 4
    lea rax, [MEDIUM_INDEXES]
    movzx r9, byte ptr [rax + r9]
    lea rax, [MEDIUM_INDEXES + 1]
    movzx rdx, byte ptr [rax + rdx]
  {$endif}

  // if (v2 <> v4) then goto inline ChangeEmptyIndex
  {$ifdef CPUX86}
    cmp edx, ebx
  {$else .CPUX64}
    cmp rdx, r9
  {$endif}
  jne @change_empty_index

  // Result
  {$ifdef CPUX86}
    mov eax, [esp - 16]
    pop ebx
  {$else .CPUX64}
    xchg rax, r10
  {$endif}
  ret

@change_empty_index:
  // x86: store v5, v6
  // x64: re-store Result (rax)
  {$ifdef CPUX86}
    push esi
    push edi
  {$else .CPUX64}
    xchg rax, r10
  {$endif}

  // Next(v5), set next empties[Index(v2)]
  {$ifdef CPUX86}
    mov esi, [ECX].THeaderMediumEmpty.Next
    mov [EAX + edx * 4], esi
  {$else .CPUX64}
    mov r10, [R8].THeaderMediumEmpty.Next
    mov [RCX + rdx * 8], r10
  {$endif}

  // NewFMask(v6) := BitChecked(v2)
  {$ifdef CPUX86}
    xor edi, edi
    bts edi, edx
  {$else .CPUX64}
    xor r11, r11
    bts r11, rdx
  {$endif}

  // (Next(v5) <> nil)? Next(v5).Prev := nil, NewFMask(v6) := 0
  {$ifdef CPUX86}
    xor edx, edx
    test esi, esi
    DB $0F, $45, $FA // cmovnz edi, edx
    lea esp, [esp - THeaderMediumEmpty.Prev - 4]
    DB $0F, $44, $F4 // cmovz esi, esp
    lea esp, [esp + THeaderMediumEmpty.Prev + 4]
    mov [ESI].THeaderMediumEmpty.Prev, edx
  {$else .CPUX64}
    xor rdx, rdx
    test r10, r10
    cmovnz r11, rdx
    lea rsp, [rsp - THeaderMediumEmpty.Prev - 8]
    cmovz r10, rsp
    lea rsp, [rsp + THeaderMediumEmpty.Prev + 8]
    mov [R10].THeaderMediumEmpty.Prev, rdx
  {$endif}

  // include another NewFMask bits
  {$ifdef CPUX86}
    mov edx, [EAX].TMediumManager.FMask
    xor edi, edx
  {$else .CPUX64}
    mov rdx, [RCX].TMediumManager.FMask
    xor r11, rdx
  {$endif}

  // Self.FMask := NewFMask + CheckBit(v4)
  {$ifdef CPUX86}
    bts edi, ebx
    mov [EAX].TMediumManager.FMask, edi
  {$else .CPUX64}
    bts r11, r9
    mov [RCX].TMediumManager.FMask, r11
  {$endif}

  // enqueue
  {$ifdef CPUX86}
    mov esi, [EAX + ebx * 4]
    mov [EAX + ebx * 4], ecx
    mov [ECX].THeaderMediumEmpty.Next, esi
  {$else .CPUX64}
    mov r10, [RCX + r9 * 8]
    mov [RCX + r9 * 8], r8
    mov [R8].THeaderMediumEmpty.Next, r10
  {$endif}

  // (Next(v5) <> nil)? Next(v5).Prev := Empty(v3)
  {$ifdef CPUX86}
    test esi, esi
    lea edx, [esp - THeaderMediumEmpty.Prev - 4]
    DB $0F, $44, $F2 // cmovz esi, edx
    mov [ESI].THeaderMediumEmpty.Prev, ecx
  {$else .CPUX64}
    test r10, r10
    lea rdx, [rsp - THeaderMediumEmpty.Prev - 8]
    cmovz r10, rdx
    mov [r10].THeaderMediumEmpty.Prev, r8
  {$endif}

  // Result
  {$ifdef CPUX86}
    mov eax, [esp - 8]
    pop edi
    pop esi
    pop ebx
  {$endif}
  ret

@full_allocated:
  {$ifdef CPUX86}
    add ebx, edx
    sub ecx, 16
    mov edx, ebx
    add ebx, MASK_MEDIUM_ALLOCATED
    mov [ECX].THeaderMedium.Flags, ebx
    add ecx, edx
    shr edx, 4
    movzx edx, byte ptr [MEDIUM_INDEXES + edx]
    pop ebx
  {$else .CPUX64}
    add r9, rdx
    sub r8, 16
    mov rdx, r9
    add r9, MASK_MEDIUM_ALLOCATED
    mov [R8].THeaderMedium.Flags, r9
    add r8, rdx
    shr rdx, 4
    lea rax, MEDIUM_INDEXES
    movzx rdx, byte ptr [rax + rdx]
  {$endif}
  jmp TMediumManager.ExcludeEmpty

@penalty:
  {$ifdef CPUX86}
    pop ebx
    shr edx, 4
    xor ecx, ecx
  {$else .CPUX64}
    shr rdx, 4
    xor r8, r8
  {$endif}
  jmp TMediumManager.GetAdvanced

@new_pool:
  // if (HeapOf(Self).NewK64PoolMedium) then IndexDone(31)
  {$ifdef CPUX86}
    push eax
    push edx
    lea eax, [eax - TThreadHeap.FMedium]
    call TThreadHeap.NewK64PoolMedium
    test eax, eax
    pop edx
    pop eax
    mov ecx, 31
  {$else .CPUX64}
    push rcx
    push rdx
    {stack align} push rax
    lea rcx, [rcx - TThreadHeap.FMedium]
    call TThreadHeap.NewK64PoolMedium
    test rax, rax
    {stack align} pop rax
    pop rdx
    pop rcx
    mov r8d, 31
  {$endif}
  jnz @index_done

  // Exit Self.ErrorOutOfMemory
  {$ifdef CPUX86}
    pop ebx
  {$endif}
  jmp TMediumManager.ErrorOutOfMemory

@invalid_ptr:
  {$ifdef CPUX86}
    pop ebx
  {$endif}
  jmp TMediumManager.RaiseInvalidPtr
end;
{$endif}

function TMediumManager.GetAdvanced(B16Count: NativeUInt; Align: NativeUInt{TMemoryAlign}): Pointer;
label
  index_done, new_k64pool, invalid_ptr, retrieve_result;
var
  ALIGN_OFFSETS: PMediumAlignOffsets;
  Mask, Index, Counter: NativeInt;
  P: PByte;
  Empty: PHeaderMediumEmpty;
  Header: PHeaderMedium;
  Last, Size, AlignOffset: NativeUInt;
  _Self, StoredSelf: PMediumManager;
begin
  // basic parameters
  ALIGN_OFFSETS := @MEDIUM_ALIGN_OFFSETS[Align];
  StoredSelf := @Self;
  Size := B16Count shl 4;
  _Self := @Self;

  // basic mask (another clear algorithm), allocation
  Index{Mask} := MEDIUM_MASKS_CLEAR[NativeUInt(MEDIUM_INDEXES[B16Count]) - 1] and _Self.FMask;
  if (Index{Mask} <> 0) then
  begin
    Mask := Index{Mask};
    repeat
      // Index := FirstBit(Mask)
      // Mask.ExcludeBit(Index)
      P := Pointer(@Mask);
      Inc(PWord(P), Byte(PWord(P)^ = 0));
      Inc(P, Byte(P^ = 0));
      Index := (NativeUInt(P) - NativeUInt(@Mask)) shl 3 + BIT_SCANS[P^];
      Mask := Mask xor (1 shl Index);

    index_done:
      Empty := _Self.FEmpties[Index];
      {$ifdef DEBUG}
      if (Empty = nil) then goto invalid_ptr;
      {$endif}

      // check each empty (-counter limit)
      Counter := ((Index + (32 - 27)) and 32) shl (16 - 5) + (-16);
      repeat
        // take piece header
        begin
          Last := Empty.Size;
          {$ifdef DEBUG}
          if (Last and MASK_MEDIUM_EMPTY_TEST <> MASK_MEDIUM_EMPTY_VALUE) then goto invalid_ptr;
          if (Index <> MEDIUM_INDEXES[Last shr 4]) then goto invalid_ptr;
          {$endif}
          Header := Pointer(Empty);
          Dec(NativeUInt(Header), Last);
          {$ifdef DEBUG}
          if (Header.Flags <> Last) then goto invalid_ptr;
          {$endif}
        end;

        // align offset
        AlignOffset := ALIGN_OFFSETS.Values[(NativeInt(Header) and MASK_K4_TEST) shr 4];

        // is size suitable
        if (Last{Header.Size} >= (Size + AlignOffset){Requirement}) then
        begin
          // actualize header, Vacant := Last - AlignOffset - Size, store (Result header)
          Dec(Last, AlignOffset);
          Inc(NativeUInt(Header), AlignOffset);
          Dec(Last, Size);
          Pointer(Mask) := Header;

          if (Last{Vacant} > 32) then
          begin
            // mark allocated (Result)
            begin
              // [left...
              Header.Flags := Size + ALIGN_OFFSETS.AllocatedFlags;

              // ...right]
              Inc(Header);
              Inc(NativeUInt(Header), Size);
              Header.PreviousSize := Size;
            end;

            // mark new empty (Vacant - SizeOf(Header))
            begin
              // [left...
              Dec(Last, 16);
              Header.Flags := Last;

              // ...right]
              Inc(NativeUInt(Header), Last);
              PHeaderMediumEmpty(Header).Size := Last;
            end;

            // detect last(Size + 16) and new(Last) indexes
            begin
              Inc(Size, AlignOffset);
              Inc(Size, Last);
              Last := Last shr 4;
              Size := Size shr 4;
              Last{new} := MEDIUM_INDEXES[Last];
              Size{last} := MEDIUM_INDEXES[Size + 1];
            end;

            // optional empty change index
            if (Size = Last) then
            begin
              // old index
              if (AlignOffset = 0) then goto retrieve_result;
            end else
            begin
              // change index
              StoredSelf.ChangeEmptyIndex((Last{new} shl 5) + Size{last}, Pointer(Header));
              if (AlignOffset = 0) then goto retrieve_result;
            end;
          end else
          begin
            // full: left allocated
            Inc(Size, Last);
            Header.Flags := Size + ALIGN_OFFSETS.AllocatedFlags;
            Inc(NativeUInt(Header), Size);
            PHeaderMediumEmpty(Header).Size := Size;

            // exclude empty
            StoredSelf.ExcludeEmpty(MEDIUM_INDEXES[(Size + AlignOffset) shr 4], PHeaderMediumEmpty(Header));
            if (AlignOffset = 0) then goto retrieve_result;
          end;

          // align offset
          Result := StoredSelf.AlignOffsetEmpty(Pointer(Mask), AlignOffset);
          Exit;
        end;

        // next empty/counter
        Empty := PHeaderMediumEmpty(NativeUInt(Header) + Last){Empty}.Next;
        Counter := Counter + 2;
      until (NativeInt(Empty) and Counter = 0);

      _Self := StoredSelf;
    until (Mask = 0);
    goto new_k64pool;
  end else
  begin
  new_k64pool:
    if (PThreadHeap(NativeInt(_Self) - NativeInt(@PThreadHeap(nil).FMedium)).NewK64PoolMedium <> nil) then
    begin
      _Self := StoredSelf;
      Index := 31;
      goto index_done;
    end else
    begin
      Result := StoredSelf.ErrorOutOfMemory;
      Exit;
    end;
  end;

invalid_ptr:
  Result := Pointer(StoredSelf.RaiseInvalidPtr);
  Exit;

retrieve_result:
  Result := Pointer(Mask + SizeOf(THeaderMedium));
end;

function TMediumManager.Reduce(P: Pointer; NewB16Count: NativeUInt{Word}): Pointer;
{$ifdef PUREPASCAL}
label
  invalid_ptr_retrieve_flags, invalid_ptr_retrieve_last,
  invalid_ptr_retrieve, invalid_ptr;
var
  Header: PHeaderMedium;
  Last, Size, Flags: NativeUInt;
  Empty, Next: PHeaderMediumEmpty;
begin
  // actualize
  Size := NativeUInt(Word(NewB16Count)) shl 4;

  // last size, next header
  Header := Pointer(NativeInt(P) - SizeOf(THeaderMedium));
  Last := Header.Flags;
  if (Last and MASK_MEDIUM_ALLOCATED_TEST <> MASK_MEDIUM_ALLOCATED_VALUE) then goto invalid_ptr;
  Last := Word(Last);
  Inc(Header);
  Inc(NativeUInt(Header), Last);
  if (Header.PreviousSize <> Last) then goto invalid_ptr_retrieve;

  // vacant, reduce + empty optional: grow/new, Result
  Dec(Last, Size);
  Flags := Header.Flags;
  if (Flags and MASK_MEDIUM_EMPTY_TEST = MASK_MEDIUM_EMPTY_VALUE) then
  begin
    { grow empty piece }

    // check empty previous size
    begin
      Inc(NativeUInt(Header), Flags);
      if (PHeaderMediumEmpty(Header).Size <> Flags) then goto invalid_ptr_retrieve_flags;
    end;

    // mark new empty
    begin
      Inc(Flags, Last);
      PHeaderMediumEmpty(Header).Size := Flags;
      Dec(NativeUInt(Header), Flags);
      Header.Flags := Flags;
    end;

    // mark new allocated (Result)
    begin
      Header.PreviousSize := Size;
      Dec(Header);
      Dec(NativeUInt(Header), Size);
      Header.WSize := Size;
      Inc(Header);
    end;

    // store full size (Result + Empty), empty indexes
    begin
      Inc(Size, Flags);
      Last := NativeUInt(-(NativeInt(Last) - NativeInt(Flags)));
      Flags := Byte(MEDIUM_INDEXES[Flags shr 4]);
      Last := Byte(MEDIUM_INDEXES[Last shr 4]);
    end;

    // Result (optional call empty change index)
    if (Flags = Last) then
    begin
      Result := Pointer(Header);
    end else
    begin
      Inc(NativeUInt(Header), Size);
      Result :=
      {$ifdef CPUX86}
      PK64PoolMedium(NativeInt(Header) and MASK_K64_CLEAR).ThreadHeap.FMedium
      {$else}
      Self
      {$endif}
        .ChangeEmptyIndex((Flags shl 5) + Last, Pointer(Header));
    end;
    Exit;
  end else
  begin
    // check allocated
    if (Flags and MASK_MEDIUM_ALLOCATED_TEST <> MASK_MEDIUM_ALLOCATED_VALUE) then
      goto invalid_ptr_retrieve_last;

    { make empty piece: vacant - 16 }

    // mark new empty
    begin
      Dec(Last, 16);
      Header.PreviousSize := Last;
      Dec(Header);
      Dec(NativeUInt(Header), Last);
      Header.Flags := Last;
    end;

    // mark new allocated (Result)
    begin
      Header.PreviousSize := Size;
      Dec(Header);
      Dec(NativeUInt(Header), Size);
      Header.WSize := Size;
      Inc(Header);
    end;

    // inline IncludeEmpty
    begin
      Inc(Size, Last);
      Empty := Pointer(Header);
      Inc(NativeUInt(Empty), Size);
      Last := Byte(MEDIUM_INDEXES[Last shr 4]);
      with {$ifdef CPUX86}
             PK64PoolMedium(NativeInt(Header) and MASK_K64_CLEAR).ThreadHeap.FMedium
           {$else}
             Self
           {$endif} do
      begin
        Next := FEmpties[Last];
        FEmpties[Last] := Empty;
        FMask := FMask or (1 shl Last);
      end;
      Empty.Next := Next;
      if (Next <> nil) then
      begin
        Next.Prev := Empty;
      end;
      Empty.Prev := nil;
    end;

    // done
    Result := Pointer(Header);
    Exit;
  end;

invalid_ptr_retrieve_flags:
  {$ifdef CPUX86}
  Dec(NativeUInt(Header), Flags);
  {$endif}

invalid_ptr_retrieve_last:
  {$ifdef CPUX86}
  Inc(Last, Size);
  {$endif}

invalid_ptr_retrieve:
  {$ifdef CPUX86}
  Dec(NativeUInt(Header), Last);
  {$endif}

invalid_ptr:
  Result := Pointer(
    {$ifdef CPUX86}
    PK64PoolMedium(NativeInt(Header) and MASK_K64_CLEAR).ThreadHeap.FMedium
    {$else}
    Self
    {$endif}
    .RaiseInvalidPtr
  );
end;
{$else}
asm
  // x86: store v4, x64: store Self (rax)
  // retrieve Size
  {$ifdef CPUX86}
    push ebx
    movzx ecx, cx
    shl ecx, 4
  {$else .CPUX64}
    xchg rax, rcx
    movzx r8, r8w
    shl r8, 4
  {$endif}

  // last size, next header
  {$ifdef CPUX86}
    mov eax, [EDX - 16].THeaderMedium.Flags
    mov ebx, MASK_MEDIUM_ALLOCATED_TEST
    and ebx, eax
    cmp ebx, MASK_MEDIUM_ALLOCATED_VALUE
    jne @invalid_ptr
    movzx eax, ax
    cmp [edx + eax].THeaderMedium.PreviousSize, eax
    lea edx, [edx + eax]
    jne @invalid_ptr_retrieve
  {$else .CPUX64}
    mov rcx, [RDX - 16].THeaderMedium.Flags
    mov r9, MASK_MEDIUM_ALLOCATED_TEST
    and r9, rcx
    cmp r9, MASK_MEDIUM_ALLOCATED_VALUE
    jne @invalid_ptr
    movzx rcx, cx
    cmp [rdx + rcx].THeaderMedium.PreviousSize, rcx
    lea rdx, [rdx + rcx]
    jne @invalid_ptr_retrieve
  {$endif}

  // vacant, check right empty
  {$ifdef CPUX86}
    mov ebx, [EDX].THeaderMedium.Flags
    sub eax, ecx
    test ebx, MASK_MEDIUM_EMPTY_TEST
  {$else .CPUX64}
    mov r9, [RDX].THeaderMedium.Flags
    sub rcx, r8
    test r9, MASK_MEDIUM_EMPTY_TEST
  {$endif}
  jnz @right_not_empty

  // check empty previous size
  {$ifdef CPUX86}
    add edx, ebx
    cmp [EDX].THeaderMediumEmpty.Size, ebx
  {$else .CPUX64}
    add rdx, r9
    cmp [RDX].THeaderMediumEmpty.Size, r9
  {$endif}
  jne @invalid_ptr_retrieve_flags

  // mark new empty
  {$ifdef CPUX86}
    add ebx, eax
    mov [EDX].THeaderMediumEmpty.Size, ebx
    sub edx, ebx
    mov [EDX].THeaderMedium.Flags, ebx
  {$else .CPUX64}
    add r9, rcx
    mov [RDX].THeaderMediumEmpty.Size, r9
    sub rdx, r9
    mov [RDX].THeaderMedium.Flags, r9
  {$endif}

  // mark new allocated (Result)
  {$ifdef CPUX86}
    mov [EDX].THeaderMedium.PreviousSize, ecx
    sub edx, ecx
    mov [EDX - 16].THeaderMedium.WSize, cx
  {$else .CPUX64}
    mov [RDX].THeaderMedium.PreviousSize, r8
    sub rdx, r8
    mov [RDX - 16].THeaderMedium.WSize, r8w
  {$endif}

  // store full size (Result + Empty), empty indexes
  {$ifdef CPUX86}
    sub eax, ebx
    add ecx, ebx
    neg eax
    shr ebx, 4
    shr eax, 4
    movzx ebx, byte ptr [MEDIUM_INDEXES + ebx]
    movzx eax, byte ptr [MEDIUM_INDEXES + eax]
  {$else .CPUX64}
    sub rcx, r9
    add r8, r9
    neg rcx
    shr r9, 4
    shr rcx, 4
    lea r10, [MEDIUM_INDEXES]
    movzx r9, byte ptr [r10 + r9]
    movzx rcx, byte ptr [r10 + rcx]
  {$endif}

  // Result (optional call empty change index)
  {$ifdef CPUX86}
    add ecx, edx
    cmp eax, ebx
    jne @change_empty_index
    pop ebx
    xchg eax, edx
    ret
  @change_empty_index:
    shl ebx, 5
    and edx, MASK_K64_CLEAR
    mov edx, [EDX].TK64PoolMedium.ThreadHeap
    add eax, ebx
    pop ebx
    lea edx, [EDX + TThreadHeap.FMedium]
    xchg eax, edx
    jmp TMediumManager.ChangeEmptyIndex
  {$else .CPUX64}
    add r8, rdx
    xchg rax, rdx
    cmp rcx, r9
    lea r9, [r9 * 8]
    lea rcx, [rcx + r9 * 4]
    xchg rcx, rdx
    jne TMediumManager.ChangeEmptyIndex
    ret
  {$endif}

@right_not_empty:
  // check allocated
  {$ifdef CPUX86}
    and ebx, MASK_MEDIUM_ALLOCATED_TEST
    cmp ebx, MASK_MEDIUM_ALLOCATED_VALUE
  {$else .CPUX64}
    and r9, MASK_MEDIUM_ALLOCATED_TEST
    cmp r9, MASK_MEDIUM_ALLOCATED_VALUE
  {$endif}
  jne @invalid_ptr_retrieve_last

  // mark new empty
  {$ifdef CPUX86}
    sub eax, 16
    mov [EDX].THeaderMedium.PreviousSize, eax
    sub edx, 16
    sub edx, eax
    mov [EDX].THeaderMedium.Flags, eax
  {$else .CPUX64}
    sub rcx, 16
    mov [RDX].THeaderMedium.PreviousSize, rcx
    sub rdx, 16
    sub rdx, rcx
    mov [RDX].THeaderMedium.Flags, rcx
  {$endif}

  // mark new allocated (Result)
  {$ifdef CPUX86}
    mov [EDX].THeaderMedium.PreviousSize, ecx
    sub edx, ecx
    mov [EDX - 16].THeaderMedium.WSize, cx
  {$else .CPUX64}
    mov [RDX].THeaderMedium.PreviousSize, r8
    sub rdx, r8
    mov [RDX - 16].THeaderMedium.WSize, r8w
  {$endif}

  // Result (v2), Empty (v3), Index (v1), Self (v4)
  {$ifdef CPUX86}
    mov ebx, MASK_K64_CLEAR
    and ebx, edx
    mov ebx, [EBX].TK64PoolMedium.ThreadHeap
    lea ebx, [EBX].TThreadHeap.FMedium

    add ecx, edx
    add ecx, eax
    shr eax, 4
    movzx eax, byte ptr [MEDIUM_INDEXES + eax]
  {$else .CPUX64}
    xchg rax, r9

    add r8, rdx
    add r8, rcx
    shr rcx, 4
    lea rax, [MEDIUM_INDEXES]
    movzx rcx, byte ptr [rax + rcx]
  {$endif}

  // SetChecked, Next(v1), Empty
  {$ifdef CPUX86}
    push esi
    mov esi, [EBX].TMediumManager.FMask
    bts esi, eax
    mov [EBX].TMediumManager.FMask, esi
    pop esi

    lea ebx, [EBX + eax * 4]
    mov eax, [ebx]
    mov [ebx], ecx
  {$else .CPUX64}
    mov r10, [R9].TMediumManager.FMask
    bts r10, rcx
    mov [R9].TMediumManager.FMask, r10

    lea r9, [R9 + rcx * 8]
    mov rcx, [R9]
    mov [R9], r8
  {$endif}

  // Empty = (nil, Next); (Next <> nil)? Next.Prev := Empty
  // v1: Next, v2: Result, v3: Empty, v4: none
  {$ifdef CPUX86}
    xor ebx, ebx
    mov [ECX].THeaderMediumEmpty.Prev, ebx
    mov [ECX].THeaderMediumEmpty.Next, eax

    lea ebx, [esp - THeaderMediumEmpty.Prev - 4]
    test eax, eax
    DB $0F, $44, $C3 // cmovz eax, ebx
    mov [EAX].THeaderMediumEmpty.Prev, ecx
  {$else .CPUX64}
    xor r9, r9
    mov [R8].THeaderMediumEmpty.Prev, r9
    mov [R8].THeaderMediumEmpty.Next, rcx

    lea r9, [rsp - THeaderMediumEmpty.Prev - 8]
    test rcx, rcx
    cmovz rcx, r9
    mov [RCX].THeaderMediumEmpty.Prev, r8
  {$endif}

  // Result
  {$ifdef CPUX86}
    xchg eax, edx
    pop ebx
  {$else .CPUX64}
    xchg rax, rdx
  {$endif}
  ret

@invalid_ptr_retrieve_flags:
  {$ifdef CPUX86}
  sub edx, ebx
  {$endif}

@invalid_ptr_retrieve_last:
  {$ifdef CPUX86}
  add eax, ecx
  {$endif}

@invalid_ptr_retrieve:
  {$ifdef CPUX86}
  sub edx, eax
  {$endif}

@invalid_ptr:
  {$ifdef CPUX86}
    and edx, MASK_K64_CLEAR
    mov eax, [EDX].TK64PoolMedium.ThreadHeap
    lea eax, [EAX].TThreadHeap.FMedium
  {$else .CPUX64}
    lea rcx, [RAX].TThreadHeap.FMedium
  {$endif}
  jmp TMediumManager.RaiseInvalidPtr
end;
{$endif}

function TMediumManager.Grow(P: Pointer; GrowFlags: NativeUInt{NewB16Count: Word; Copy: Boolean}): Pointer;
{$ifdef PUREPASCAL}
label
  invalid_ptr_retrieve_empty, invalid_ptr_retrieve, invalid_ptr, penalty;
var
  Header: PHeaderMedium;
  Flags, EmptyFlags: NativeUInt;
begin
  // header(P), flags
  begin
    Header := P;
    Flags := PHeaderMedium(NativeInt(P) - SizeOf(THeaderMedium)).Flags;
    GrowFlags := GrowFlags shl 4;
  end;

  // check allocated, next header, growing size
  begin
    EmptyFlags{Buffer} := Flags and MASK_MEDIUM_ALLOCATED_TEST;
    Flags := Word(Flags);
    if (EmptyFlags{Buffer} <> MASK_MEDIUM_ALLOCATED_VALUE) then goto invalid_ptr;
    Inc(NativeUInt(Header), Flags);
    Dec(GrowFlags, Flags);
    if (Header.PreviousSize <> Flags) then goto invalid_ptr_retrieve;
  end;

  // is right header empty
  EmptyFlags := Header.Flags;
  if (EmptyFlags and MASK_MEDIUM_EMPTY_TEST = MASK_MEDIUM_EMPTY_VALUE) then
  begin
    // is empty size enough
    begin
      Inc(EmptyFlags, 16);
      if (Word(EmptyFlags) < Word(GrowFlags)) then goto penalty;
      Dec(EmptyFlags, 16);
    end;

    // growing size
    GrowFlags := Word(GrowFlags);

    // check empty
    begin
      Inc(NativeUInt(Header), EmptyFlags);
      if (PHeaderMediumEmpty(Header).Size <> EmptyFlags) then goto invalid_ptr_retrieve_empty;
    end;

    // is (empty size + 16 - growing size > 32) --> (empty size - growing size > 16)
    Dec(EmptyFlags, GrowFlags);
    if (NativeInt(EmptyFlags) > 16) then
    begin
      { reduce empty }

      // new allocted size, store last empty size
      begin
        Inc(Flags, GrowFlags);
        Inc(GrowFlags, EmptyFlags);
      end;

      // mark new empty
      begin
        PHeaderMediumEmpty(Header).Size := EmptyFlags;
        Dec(NativeUInt(Header), EmptyFlags);
        Header.Flags := EmptyFlags;
      end;

      // mark new allocated (Result)
      begin
        Header.PreviousSize := Flags;
        Dec(Header);
        Dec(NativeUInt(Header), Flags);
        Header.WSize := Flags;
        Inc(Header);
      end;

      // store full size (Result + Empty), empty indexes
      begin
        Inc(Flags, EmptyFlags);
        GrowFlags := Byte(MEDIUM_INDEXES[GrowFlags shr 4]);
        EmptyFlags := Byte(MEDIUM_INDEXES[EmptyFlags shr 4]);
      end;

      // Result (optional call empty change index)
      if (GrowFlags = EmptyFlags) then
      begin
        Result := Pointer(Header);
      end else
      begin
        Inc(NativeUInt(Header), Flags);
        Result :=
        {$ifdef CPUX86}
        PK64PoolMedium(NativeInt(Header) and MASK_K64_CLEAR).ThreadHeap.FMedium
        {$else}
        Self
        {$endif}
          .ChangeEmptyIndex((EmptyFlags shl 5) + GrowFlags, Pointer(Header));
      end;
      Exit;
    end else
    begin
      { exclude empty }

      // empty index, new allocated size
      begin
        Inc(EmptyFlags, GrowFlags);
        Inc(Flags, 16);
        Inc(Flags, EmptyFlags);
        EmptyFlags := Byte(MEDIUM_INDEXES[EmptyFlags shr 4]);
      end;

      // mark allocated
      begin
        PHeaderMediumEmpty(Header).Size := Flags;
        Dec(NativeUInt(Header), Flags);
        Header.WSize := Flags;
        Inc(NativeUInt(Header), Flags);
      end;

      // exclude empty
      Result :=
      {$ifdef CPUX86}
      PK64PoolMedium(NativeInt(Header) and MASK_K64_CLEAR).ThreadHeap.FMedium
      {$else}
      Self
      {$endif}
        .ExcludeEmpty(EmptyFlags, Pointer(Header));
      Exit;
    end;
  end else
  begin
    if (EmptyFlags and MASK_MEDIUM_ALLOCATED_TEST <> MASK_MEDIUM_ALLOCATED_VALUE) then
      goto invalid_ptr_retrieve;

  penalty:
    Dec(NativeUInt(Header), Flags);
    Inc(GrowFlags, Flags);
    Result := PK64PoolMedium(NativeInt(Header) and MASK_K64_CLEAR).ThreadHeap.FMedium
      .GrowPenalty(Pointer(Header), GrowFlags);
    Exit;
  end;

invalid_ptr_retrieve_empty:
  Dec(NativeUInt(Header), EmptyFlags);

invalid_ptr_retrieve:
  Dec(NativeUInt(Header), Flags);

invalid_ptr:
  Result := Pointer(PK64PoolMedium(NativeInt(Header) and MASK_K64_CLEAR).ThreadHeap.
    FMedium.RaiseInvalidPtr);
end;
{$else}
asm
  // x86: store v4, x64: store Self (rax)
  // current Flags(v3), grow Size/Flags(v1)
  {$ifdef CPUX86}
    push ebx
    mov eax, [EDX - 16].THeaderMedium.Flags
    shl ecx, 4
    xchg eax, ecx
  {$else .CPUX64}
    xchg rax, rcx
    mov rcx, [RDX - 16].THeaderMedium.Flags
    shl r8, 4
    xchg rcx, r8
  {$endif}

  // check allocated, next header, growing size
  {$ifdef CPUX86}
    mov ebx, MASK_MEDIUM_ALLOCATED_TEST
    and ebx, ecx
    movzx ecx, cx
    cmp ebx, MASK_MEDIUM_ALLOCATED_VALUE
    jne @invalid_ptr
    sub eax, ecx
    cmp [EDX + ECX].THeaderMedium.PreviousSize, ecx
    lea edx, [EDX + ECX]
  {$else .CPUX64}
    mov r9, MASK_MEDIUM_ALLOCATED_TEST
    and r9, r8
    movzx r8, r8w
    cmp r9, MASK_MEDIUM_ALLOCATED_VALUE
    jne @invalid_ptr
    sub rcx, r8
    cmp [RDX + R8].THeaderMedium.PreviousSize, r8
    lea rdx, [RDX + R8]
  {$endif}
  jne @invalid_ptr_retrieve

  // is right header empty
  {$ifdef CPUX86}
    mov ebx, [EDX].THeaderMedium.Flags
    test ebx, MASK_MEDIUM_EMPTY_TEST
  {$else .CPUX64}
    mov r9, [RDX].THeaderMedium.Flags
    test r9, MASK_MEDIUM_EMPTY_TEST
  {$endif}
  jnz @right_not_empty

  // is empty size enough
  {$ifdef CPUX86}
    lea eax, [eax - 16]
    cmp bx, ax
    lea eax, [eax + 16]
  {$else .CPUX64}
    lea rcx, [rcx - 16]
    cmp r9w, cx
    lea rcx, [rcx + 16]
  {$endif}
  jb @penalty_grow

  // growing size
  {$ifdef CPUX86}
    movzx eax, ax
  {$else .CPUX64}
    movzx rcx, cx
  {$endif}

  // check empty
  {$ifdef CPUX86}
    add edx, ebx
    cmp [EDX].THeaderMediumEmpty.Size, ebx
  {$else .CPUX64}
    add rdx, r9
    cmp [RDX].THeaderMediumEmpty.Size, r9
  {$endif}
  jne @invalid_ptr_retrieve_empty

  // is (empty size + 16 - growing size > 32) --> (empty size - growing size > 16)
  {$ifdef CPUX86}
    sub ebx, eax
    cmp ebx, 16
  {$else .CPUX64}
    sub r9, rcx
    cmp r9, 16
  {$endif}
  jle @full_allocated

  // new allocted size, store last empty size
  {$ifdef CPUX86}
    add ecx, eax
    add eax, ebx
  {$else .CPUX64}
    add r8, rcx
    add rcx, r9
  {$endif}

  // mark new empty
  {$ifdef CPUX86}
    mov [EDX].THeaderMediumEmpty.Size, ebx
    sub edx, ebx
    mov [EDX].THeaderMedium.Flags, ebx
  {$else .CPUX64}
    mov [RDX].THeaderMediumEmpty.Size, r9
    sub rdx, r9
    mov [RDX].THeaderMedium.Flags, r9
  {$endif}

  // mark new allocated (Result)
  {$ifdef CPUX86}
    mov [EDX].THeaderMedium.PreviousSize, ecx
    sub edx, ecx
    mov [EDX - 16].THeaderMedium.WSize, cx
  {$else .CPUX64}
    mov [RDX].THeaderMedium.PreviousSize, r8
    sub rdx, r8
    mov [RDX - 16].THeaderMedium.WSize, r8w
  {$endif}

  // store full size (Result + Empty), empty indexes
  {$ifdef CPUX86}
    add ecx, ebx
    shr eax, 4
    shr ebx, 4
    movzx eax, byte ptr [MEDIUM_INDEXES + eax]
    movzx ebx, byte ptr [MEDIUM_INDEXES + ebx]
  {$else .CPUX64}
    add r8, r9
    shr rcx, 4
    shr r9, 4
    lea r10, [MEDIUM_INDEXES]
    movzx rcx, byte ptr [r10 + rcx]
    movzx r9, byte ptr [r10 + r9]
  {$endif}

  // Result (optional call empty change index)
  {$ifdef CPUX86}
    add ecx, edx
    cmp eax, ebx
    jne @change_empty_index
    pop ebx
    xchg eax, edx
    ret
  @change_empty_index:
    shl ebx, 5
    and edx, MASK_K64_CLEAR
    mov edx, [EDX].TK64PoolMedium.ThreadHeap
    add eax, ebx
    pop ebx
    lea edx, [EDX + TThreadHeap.FMedium]
    xchg eax, edx
    jmp TMediumManager.ChangeEmptyIndex
  {$else .CPUX64}
    add r8, rdx
    xchg rax, rdx
    cmp rcx, r9
    lea r9, [r9 * 8]
    lea rcx, [rcx + r9 * 4]
    xchg rcx, rdx
    jne TMediumManager.ChangeEmptyIndex
    ret
  {$endif}

@full_allocated:
  // empty index, new allocated size
  {$ifdef CPUX86}
    add ebx, eax
    add ecx, 16
    add ecx, ebx
    shr ebx, 4
    movzx ebx, byte ptr [MEDIUM_INDEXES + ebx]
  {$else .CPUX64}
    add r9, rcx
    add r8, 16
    add r8, r9
    shr r9, 4
    lea r10, [MEDIUM_INDEXES]
    movzx r9, byte ptr [r10 + r9]
  {$endif}

  // mark allocated
  {$ifdef CPUX86}
    mov [EDX].THeaderMediumEmpty.Size, ecx
    sub edx, ecx
    mov [EDX].THeaderMedium.WSize, cx
  {$else .CPUX64}
    mov [RDX].THeaderMediumEmpty.Size, r8
    sub rdx, r8
    mov [RDX].THeaderMedium.WSize, r8w
  {$endif}

  // exclude empty
  {$ifdef CPUX86}
    add ecx, edx
    and edx, MASK_K64_CLEAR
    mov eax, [EDX].TK64PoolMedium.ThreadHeap
    pop edx
    lea eax, [EAX].TThreadHeap.FMedium
    xchg ebx, edx
  {$else .CPUX64}
    add r8, rdx
    xchg rdx, r9
    xchg rax, rcx
  {$endif}
  jmp TMediumManager.ExcludeEmpty

@right_not_empty:
  // check allocated flags
  {$ifdef CPUX86}
    and ebx, MASK_MEDIUM_ALLOCATED_TEST
    cmp ebx, MASK_MEDIUM_ALLOCATED_VALUE
  {$else .CPUX64}
    and r9, MASK_MEDIUM_ALLOCATED_TEST
    cmp r9, MASK_MEDIUM_ALLOCATED_VALUE
  {$endif}
  jne @invalid_ptr_retrieve

@penalty_grow:
  {$ifdef CPUX86}
    sub edx, ecx
    add ecx, eax
    mov eax, MASK_K64_CLEAR
    and eax, edx
    mov eax, [EAX].TK64PoolMedium.ThreadHeap
    pop ebx
    lea eax, [EAX].TThreadHeap.FMedium
  {$else .CPUX64}
    sub rdx, r8
    add r8, rcx
    xchg rax, rcx
  {$endif}
  jmp TMediumManager.GrowPenalty

@invalid_ptr_retrieve_empty:
  {$ifdef CPUX86}
  sub edx, ebx
  {$endif}

@invalid_ptr_retrieve:
  {$ifdef CPUX86}
  sub edx, ecx
  {$endif}

@invalid_ptr:
  {$ifdef CPUX86}
    and edx, MASK_K64_CLEAR
    mov eax, [EDX].TK64PoolMedium.ThreadHeap
    lea eax, [EAX].TThreadHeap.FMedium
  {$else .CPUX64}
    lea rcx, [RAX].TThreadHeap.FMedium
  {$endif}
  jmp TMediumManager.RaiseInvalidPtr
end;
{$endif}

function TMediumManager.GrowPenalty(P: Pointer; GrowFlags: NativeUInt{NewSize: Word; shifted Copy: Boolean}): Pointer;
var
  _Self: PMediumManager;
  ALIGN_OFFSETS: PMediumAlignOffsets;
  Header, Ptr: PHeaderMedium;
  CurrentSize, TotalSize, LeftSize, AlignOffset: NativeUInt;
  Empty, Next, RightEmpty: PHeaderMediumEmpty;
  Stored: record
    P: Pointer;
    B16Count: NativeUInt;
    RightEmpty: PHeaderMediumEmpty;
  end;
begin
  // stored, current size, total size
  begin
    Header := P;
    Stored.P := Header;
    Dec(Header);
    CurrentSize := Header.Flags;
    ALIGN_OFFSETS := @MEDIUM_ALIGN_OFFSETS[CurrentSize shr OFFSET_MEDIUM_ALIGN];
    Inc(Header);
    CurrentSize := Word(CurrentSize);
    Inc(NativeUInt(Header), CurrentSize);
    TotalSize{RightSize} := Header.Flags;
    AlignOffset{Mask} := NativeUInt(-NativeInt(Byte(TotalSize and MASK_MEDIUM_EMPTY_TEST = MASK_MEDIUM_EMPTY_VALUE)));
    Stored.RightEmpty := Pointer((NativeUInt(Header) + TotalSize{RightSize}) and AlignOffset{Mask});
    Dec(NativeUInt(Header), CurrentSize);
    Dec(Header);
    Inc(TotalSize{RightSize}, 16);
    TotalSize{RightSize} := TotalSize{RightSize} and AlignOffset{Mask};
    Inc(TotalSize, CurrentSize);
    Stored.B16Count := CurrentSize shr 4;
  end;

  // left size, total size, actual header
  begin
    LeftSize := Header.PreviousSize;
    AlignOffset{Mask} := NativeUInt(-NativeInt(Byte(LeftSize = PHeaderMedium(NativeUInt(Header) - 16 - LeftSize).Flags)));
    Inc(LeftSize, 16);
    LeftSize := LeftSize and AlignOffset{Mask};
    Dec(NativeUInt(Header), LeftSize);
    Inc(TotalSize, LeftSize);
  end;

  // align offset, is size not suitable
  begin
    AlignOffset := ALIGN_OFFSETS.Values[(NativeInt(Header) and MASK_K4_TEST) shr 4];
    Inc(AlignOffset, GrowFlags);
    if (Word(TotalSize) < Word(AlignOffset){Requirement}) then
    begin
      GrowFlags := GrowFlags shr 4;
      AlignOffset{Align} := ALIGN_OFFSETS.AllocatedFlags shr OFFSET_MEDIUM_ALIGN;
      if (Word(GrowFlags) <= MIDDLE_MEDIUM_B16COUNT) and (AlignOffset{Align} = 0) then
      begin
        Result := Self.Get(GrowFlags and $ffff);
      end else
      begin
        Result := Self.GetAdvanced(GrowFlags and $ffff, AlignOffset{Align});
      end;

      if (GrowFlags and FLAG_MEDIUM_COPY <> 0) and (Result <> nil) then
        NcMoveB16(P16(Stored.P)^, P16(Result)^, Stored.B16Count);

      Self.Free(Stored.P);
      Exit;
    end;
    Dec(AlignOffset, GrowFlags);
  end;

  // exclude left empty (guaranteed empty), align offset
  begin
    Dec(LeftSize, 16);
    Inc(NativeUInt(Header), LeftSize);
    Header := Self.ExcludeEmpty(MEDIUM_INDEXES[LeftSize shr 4], Pointer(Header));
    if (AlignOffset <> 0) then
    begin
      Dec(Header);
      Dec(TotalSize, AlignOffset);
      Inc(NativeUInt(Header), AlignOffset);
      Header := Self.AlignOffsetEmpty(Header, AlignOffset);
    end;
  end;

  // flags, optional copy
  begin
    Ptr := Stored.P;
    Dec(Ptr);
    Ptr.Flags := 0;
    if (GrowFlags > NativeUInt(High(Word))) then
    begin
      Inc(Ptr);
      NcMoveB16(P16(Ptr)^, P16(Header)^, Stored.B16Count);
    end;
    Dec(Header);
    GrowFlags := Word(GrowFlags);
  end;

  // optional right exclude
  RightEmpty := Stored.RightEmpty;
  if (RightEmpty <> nil) then
  begin
    Self.ExcludeEmpty(MEDIUM_INDEXES[RightEmpty.Size shr 4], RightEmpty);
  end;

  // mark allocated: full/empty
  Dec(TotalSize, GrowFlags);
  if (TotalSize{Vacant} > 32) then
  begin
    // mark allocated (Result)
    Stored.P := Pointer(NativeUInt(Header) + 16);
    begin
      // [left...
      Header.Flags := ALIGN_OFFSETS.AllocatedFlags + GrowFlags;

      // ...right]
      Inc(Header);
      Inc(NativeUInt(Header), GrowFlags);
      Header.PreviousSize := GrowFlags;
    end;

    // mark new empty (Vacant - SizeOf(Header))
    begin
      // [left...
      Dec(TotalSize, 16);
      Header.Flags := TotalSize;

      // ...right]
      Empty := Pointer(NativeUInt(Header) + TotalSize);
      Empty.Size := TotalSize;
    end;

    // inline IncludeEmpty
    begin
      AlignOffset := Byte(MEDIUM_INDEXES[TotalSize shr 4]);
      _Self := @Self;
      with _Self^ do
      begin
        Next := FEmpties[AlignOffset];
        FEmpties[AlignOffset] := Empty;
        FMask := FMask or (1 shl AlignOffset);
      end;
      Empty.Next := Next;
      if (Next <> nil) then
      begin
        Next.Prev := Empty;
      end;
      Empty.Prev := nil;
    end;

    // result
    Result := Stored.P;
  end else
  begin
    // full: left allocated
    Inc(GrowFlags, TotalSize);
    Header.Flags := ALIGN_OFFSETS.AllocatedFlags + GrowFlags;
    Inc(Header);
    PHeaderMedium(NativeUInt(Header) + GrowFlags).PreviousSize := GrowFlags;

    // result
    Result := Pointer(Header);
  end;
end;

function TMediumManager.FreeDisposePenalty(Header: PHeaderMedium; FlagIsTop: Boolean): Integer;
const
  TOP_MEDIUM_EMPTY_INDEX = High(THeaderMediumList) - 1;
var
  Pool: PK64PoolMedium;
  Empty: PHeaderMediumEmpty;
begin
  // parameters
  Pool := PK64PoolMedium(NativeInt(Header) and MASK_K64_CLEAR);
  if (FlagIsTop) then
  begin
    Empty := Pointer(@Pool.Items[TOP_MEDIUM_EMPTY_INDEX]);
  end else
  begin
    Empty := PHeaderMediumEmpty(NativeUInt(Header) - SizeOf(THeaderMedium));
  end;

  // exclude
  Self.ExcludeEmpty(MEDIUM_INDEXES[Empty.Size shr 4], Empty);

  // cleanup flags
  Header.Flags := 0;

  // dispose
  Result := Pool.ThreadHeap.DisposeK64PoolMedium(Pool);
end;

function TMediumManager.Free(P: Pointer): Integer;
const
  INCREMENT_16_16 = 16 * ((1 shl 16) + 1);
{$ifdef PUREPASCAL}
label
  exclude, include, done, dispose_left, dispose_top, invalid_ptr;
var
  Empty, Prev, Next: PHeaderMediumEmpty;
  Flags, Index, LeftFlags, RightFlags: NativeUInt;
  {$ifdef CPUX86}
  Store: record
    Flags: NativeUInt;
  end;
  {$endif}
begin
  // check allocated (flags only), size, EmptyOf(P)
  begin
    Empty := Pointer(NativeInt(P) - 32);
    Flags := Empty.SiblingHeaderMedium.Flags;
    if (Flags and MASK_MEDIUM_ALLOCATED_TEST <> MASK_MEDIUM_ALLOCATED_VALUE) then goto invalid_ptr;
    Flags := Word(Flags);
  end;

  // is left empty
  begin
    LeftFlags := Empty.SiblingHeaderMedium.PreviousSize;
    Dec(NativeUInt(Empty), LeftFlags);
    Index{Buffer} := Byte(PHeaderMedium(Empty).Flags = LeftFlags);
    Inc(NativeUInt(Empty), LeftFlags);
  end;

  // check allocated size, is right empty, modes
  begin
    Inc(NativeUInt(Empty), Flags);
    if (PHeaderMediumList(Empty)[2].PreviousSize <> Flags) then goto invalid_ptr;
    Index := (Index shl 1) + Byte(PHeaderMediumList(Empty)[2].Flags and MASK_MEDIUM_EMPTY_TEST = MASK_MEDIUM_EMPTY_VALUE);
    Inc(NativeUInt(Empty), 16);
  end;

  // different modes (empty is current)
  if (Index <> 0) then
  begin
    if (Index <> 1) then
    begin
      if (Index <> 3) then
      begin
        // left, current
        Dec(NativeUInt(Empty), Flags);
        Inc(Flags, Flags shl 16);
        Index := PHeaderMedium(Empty).PreviousSize;
        Inc(Flags, INCREMENT_16_16);
        Inc(Flags, Index);
        Dec(NativeUInt(Empty), 16);

        Index := Byte(MEDIUM_INDEXES[Index shr 4]);
        if (Word(Flags) <> FULL_MEDIUM_EMPTY_SIZE) then
        begin
          goto exclude;
        end else
        begin
        dispose_left:
          Result := Self.FreeDisposePenalty(@Empty.SiblingHeaderMedium, False);
          Exit;
        end;
      end else
      begin
        // left, current, right

        // right size
        begin
          RightFlags := PHeaderMediumList(Empty)[1].Flags;
          Inc(NativeUInt(Empty), RightFlags);
          if (PHeaderMediumList(Empty)[2].PreviousSize <> RightFlags) then goto invalid_ptr;
          Inc(RightFlags, 16);
          Dec(NativeUInt(Empty), Flags);
          Inc(Flags, RightFlags);
          Dec(NativeUInt(Empty), RightFlags);
        end;

        // left size, left exclude
        begin
          Index := Empty.Size;
          Inc(Flags, 16);
          Inc(Flags, Index);
          Empty := Self.ExcludeEmpty(Byte(MEDIUM_INDEXES[Index shr 4]), Empty);
          Inc(NativeUInt(Empty), Flags);
          Dec(NativeUInt(Empty), 16);
        end;

        Index := Byte(MEDIUM_INDEXES[Empty.Size shr 4]);
        if (Flags <> FULL_MEDIUM_EMPTY_SIZE) then
        begin
          goto exclude;
        end else
        begin
          Dec(NativeUInt(Empty), Flags);
          Inc(NativeUInt(Empty), PHeaderMedium(Empty).Flags);
        dispose_top:
          Result := Self.FreeDisposePenalty(@Empty.SiblingHeaderMedium, True);
          Exit;
        end;
      end;
    end else
    begin
      // current, right

      // right size
      begin
        RightFlags := PHeaderMediumList(Empty)[1].Flags;
        Inc(Flags, 16);
        Inc(NativeUInt(Empty), RightFlags);
        if (PHeaderMediumList(Empty)[2].PreviousSize <> RightFlags) then goto invalid_ptr;
        Dec(NativeUInt(Empty), Flags);
        Dec(NativeUInt(Empty), RightFlags);
        Inc(Flags, RightFlags);
      end;
      if (Flags = FULL_MEDIUM_EMPTY_SIZE) then goto dispose_top;

      // compare indexes
      begin
        Inc(NativeUInt(Empty), Flags);
        Index := PHeaderMediumList(Empty)[2].PreviousSize;
        Inc(NativeUInt(Empty), 16);
        Index := Index shr 4;
        Index := Byte(MEDIUM_INDEXES[Index]);

        RightFlags := Byte(MEDIUM_INDEXES[Flags shr 4]);
        if (Index = RightFlags) then goto done;
      end;

    exclude:
      // inline ExcludeEmpty (Index)
      {$ifdef DEBUG}
      if (Self.FMask and (1 shl Index) = 0) then
      begin
        Result := Self.RaiseInvalidPtr;
        Exit;
      end;
      {$endif}
      begin
        {$ifdef CPUX86}Store.Flags := Flags;{$endif}
        Prev := Empty.Prev;
        Next := Empty.Next;
        if (NativeInt(Prev) or NativeInt(Next) = 0) then
        begin
          Self.FEmpties[Index] := Next{nil};
          Self.FMask := Self.FMask and (not (1 shl Index));
        end else
        begin
          if (Prev = nil) then
          begin
            Self.FEmpties[Index] := Next;
          end else
          begin
            Prev.Next := Next;
          end;
          if (Next <> nil) then
          begin
            Next.Prev := Prev;
          end;
        end;
        {$ifdef CPUX86}Flags := Store.Flags;{$endif}
      end;

      // retrieve Empty, Flags
      begin
        Inc(NativeUInt(Empty), Flags shr 16);
        Flags := Word(Flags);
      end;

      goto include;
    end;
  end else
  begin
  include:
    // inline IncludeEmpty
    begin
      Index := Byte(MEDIUM_INDEXES[Flags shr 4]);
      begin
        Next := Self.FEmpties[Index];
        {$ifdef DEBUG}
        if ((Self.FMask shr Index) and 1 <> Byte(Next <> nil)) then
        begin
          Result := Self.RaiseInvalidPtr;
          Exit;
        end;
        {$endif}
        Self.FEmpties[Index] := Empty;
        Self.FMask := Self.FMask or (1 shl Index);
      end;
      Empty.Next := Next;
      if (Next <> nil) then
      begin
        Next.Prev := Empty;
      end;
      Empty.Prev := nil;
    end;

  done:
    // current: mark empty
    begin
      Empty.Size := Flags;
      Dec(NativeUInt(Empty), Flags);
      PHeaderMedium(Empty).Flags := Flags;
    end;
  end;

  // result
  Result := FREEMEM_DONE;
  Exit;

invalid_ptr:
  Result := Self.ErrorInvalidPtr;
end;
{$else}
asm
  // check allocated flags, size, empty
  {$ifdef CPUX86}
    mov ecx, [EDX - 16].THeaderMedium.Flags
    push ebx
    push esi
    push edi
    mov esi, MASK_MEDIUM_ALLOCATED_TEST
    and esi, ecx
    lea edx, [edx - 32]
    movzx ecx, cx
    cmp esi, MASK_MEDIUM_ALLOCATED_VALUE
  {$else .CPUX64}
    mov r8, [RDX - 16].THeaderMedium.Flags
    mov r10, MASK_MEDIUM_ALLOCATED_TEST
    and r10, r8
    lea rdx, [rdx - 32]
    movzx r8, r8w
    cmp r10, MASK_MEDIUM_ALLOCATED_VALUE
  {$endif}
  jne @invalid_ptr

  // is left empty
  {$ifdef CPUX86}
    mov ebx, [EDX].THeaderMediumEmpty.SiblingHeaderMedium.PreviousSize
    sub edx, ebx
    cmp [EDX].THeaderMedium.Flags, ebx
    lea edx, [EDX + ebx]
    sete bl
    movzx ebx, bl
  {$else .CPUX64}
    mov r9, [RDX].THeaderMediumEmpty.SiblingHeaderMedium.PreviousSize
    sub rdx, r9
    cmp [RDX].THeaderMedium.Flags, r9
    lea rdx, [RDX + r9]
    sete al
    movzx r9, al
  {$endif}

  // check allocated size, is right empty, modes
  {$ifdef CPUX86}
    add edx, ecx
    add ebx, ebx
    cmp [EDX + 32].THeaderMedium.PreviousSize, ecx
    jne @invalid_ptr
    lea esi, [ebx + 1]
    test [EDX + 32].THeaderMedium.Flags, MASK_MEDIUM_EMPTY_TEST
    DB $0F, $44, $DE // cmovz ebx, esi
    add edx, 16
  {$else .CPUX64}
    add rdx, r8
    add r9, r9
    cmp [RDX + 32].THeaderMedium.PreviousSize, r8
    jne @invalid_ptr
    lea r10, [r9 + 1]
    test [RDX + 32].THeaderMedium.Flags, MASK_MEDIUM_EMPTY_TEST
    cmovz r9, r10
    add rdx, 16
  {$endif}

  // case Index of
  {$ifdef CPUX86}
    jmp dword ptr [@case_ptrs + ebx * 4]
    @case_ptrs: DD @include, @current_right, @left_current, @left_current_right
  {$else .CPUX64}
    lea rax, [@case_ptrs]
    jmp qword ptr [rax + r9 * 8]
    @case_ptrs: DQ @include, @current_right, @left_current, @left_current_right
  {$endif}
@left_current:
  // parameters, check full empty, jump exclude/enclude
  {$ifdef CPUX86}
    mov esi, ecx
    sub edx, ecx
    shl esi, 16
    mov ebx, [EDX].THeaderMedium.PreviousSize
    add ecx, esi
    add ecx, INCREMENT_16_16
    sub edx, 16
    add ecx, ebx
    shr ebx, 4
    movzx ebx, byte ptr [MEDIUM_INDEXES + ebx]
    cmp cx, FULL_MEDIUM_EMPTY_SIZE
  {$else .CPUX64}
    mov r10, r8
    sub rdx, r8
    shl r10, 16
    mov r9, [RDX].THeaderMedium.PreviousSize
    add r8, r10
    add r8, INCREMENT_16_16
    sub rdx, 16
    add r8, r9
    shr r9, 4
    lea rax, [MEDIUM_INDEXES]
    movzx r9, byte ptr [rax + r9]
    cmp r8w, FULL_MEDIUM_EMPTY_SIZE
  {$endif}
  jne @exclude

  // dispose left
  {$ifdef CPUX86}
    pop edi
    pop esi
    pop ebx
    lea edx, [EDX].THeaderMediumEmpty.SiblingHeaderMedium
    xor ecx, ecx
  {$else .CPUX64}
    lea rdx, [RDX].THeaderMediumEmpty.SiblingHeaderMedium
    xor r8, r8
  {$endif}
  jmp TMediumManager.FreeDisposePenalty

@left_current_right:
  // right size
  {$ifdef CPUX86}
    mov esi, [EDX].THeaderMediumEmpty.SiblingHeaderMedium.Flags
    add edx, esi
    cmp [EDX + 32].THeaderMedium.PreviousSize, esi
    jne @invalid_ptr
    add esi, 16
    sub edx, ecx
    add ecx, esi
    sub edx, esi
  {$else .CPUX64}
    mov r10, [RDX].THeaderMediumEmpty.SiblingHeaderMedium.Flags
    add rdx, r10
    cmp [RDX + 32].THeaderMedium.PreviousSize, r10
    jne @invalid_ptr
    add r10, 16
    sub rdx, r8
    add r8, r10
    sub rdx, r10
  {$endif}

  // left size, left exclude
  {$ifdef CPUX86}
    mov ebx, [EDX].THeaderMediumEmpty.Size
    add ecx, 16
    add ecx, ebx
    shr ebx, 4
    movzx ebx, byte ptr [MEDIUM_INDEXES + ebx]
    push eax
    push ecx
    mov ecx, edx
    mov edx, ebx
      call TMediumManager.ExcludeEmpty
    pop ecx
    add eax, ecx
    pop edx
    mov ebx, [EAX - 16].THeaderMediumEmpty.Size
    shr ebx, 4
    sub eax, 16
    xchg eax, edx
    cmp ecx, FULL_MEDIUM_EMPTY_SIZE
    movzx ebx, byte ptr [MEDIUM_INDEXES + ebx]
  {$else .CPUX64}
    mov r9, [RDX].THeaderMediumEmpty.Size
    add r8, 16
    add r8, r9
    shr r9, 4
    lea rax, [MEDIUM_INDEXES]
    movzx r9, byte ptr [rax + r9]
    push rcx
    {stack align}push r9
    push r8
    mov r8, rdx
    mov rdx, r9
      call TMediumManager.ExcludeEmpty
    pop r8
    {stack align}pop r9
    add rax, r8
    pop rdx
    mov r9, [RAX - 16].THeaderMediumEmpty.Size
    shr r9, 4
    lea rcx, [rax - 16]
    xchg rcx, rdx
    cmp r8, FULL_MEDIUM_EMPTY_SIZE
    lea rax, [MEDIUM_INDEXES]
    movzx r9, byte ptr [rax + r9]
  {$endif}
  jne @exclude

  // empty correction
  {$ifdef CPUX86}
    sub edx, ecx
    add edx, [EDX].THeaderMedium.Flags
  {$else .CPUX64}
    sub rdx, r8
    add rdx, [RDX].THeaderMedium.Flags
  {$endif}

@dispose_top:
  {$ifdef CPUX86}
    lea edx, [EDX].THeaderMediumEmpty.SiblingHeaderMedium
    mov ecx, 1
    pop edi
    pop esi
    pop ebx
  {$else .CPUX64}
    lea rdx, [RDX].THeaderMediumEmpty.SiblingHeaderMedium
    mov r8d, 1
  {$endif}
  jmp TMediumManager.FreeDisposePenalty

@current_right:
  // right size, check full empty
  {$ifdef CPUX86}
    mov esi, [EDX].THeaderMediumEmpty.SiblingHeaderMedium.Flags
    add ecx, 16
    add edx, esi
    cmp [EDX + 32].THeaderMedium.PreviousSize, esi
    jne @invalid_ptr
    sub edx, ecx
    sub edx, esi
    add ecx, esi
    cmp ecx, FULL_MEDIUM_EMPTY_SIZE
  {$else .CPUX64}
    mov r10, [RDX].THeaderMediumEmpty.SiblingHeaderMedium.Flags
    add r8, 16
    add rdx, r10
    cmp [RDX + 32].THeaderMedium.PreviousSize, r10
    jne @invalid_ptr
    sub rdx, r8
    sub rdx, r10
    add r8, r10
    cmp r8, FULL_MEDIUM_EMPTY_SIZE
  {$endif}
  je @dispose_top

  // compare indexes
  {$ifdef CPUX86}
    add edx, ecx
    mov ebx, [EDX + 32].THeaderMedium.PreviousSize
    add edx, 16
    mov esi, ecx
    shr ebx, 4
    shr esi, 4
    movzx ebx, byte ptr [MEDIUM_INDEXES + ebx]
    movzx esi, byte ptr [MEDIUM_INDEXES + esi]
    cmp ebx, esi
  {$else .CPUX64}
    add rdx, r8
    mov r9, [RDX + 32].THeaderMedium.PreviousSize
    add rdx, 16
    mov r10, r8
    shr r9, 4
    shr r10, 4
    lea rax, [MEDIUM_INDEXES]
    movzx r9, byte ptr [rax + r9]
    movzx r10, byte ptr [rax + r10]
    cmp r9, r10
  {$endif}
  je @done

@exclude:
  {$ifdef DEBUG}
     // if (not Self.FMask.Checked(Index)) then Exit Self.RaiseInvalidPtr
    {$ifdef CPUX86}
      mov esi, [EAX].TMediumManager.FMask
      bt esi, ebx
    {$else .CPUX64}
      mov r10, [RCX].TMediumManager.FMask
      bt r10, r9
    {$endif}
    jnc @raise_invalid_ptr
  {$endif}

  // prev, next, optional uncheck bit
  {$ifdef CPUX86}
    mov esi, [EDX].THeaderMediumEmpty.Prev
    mov edi, [EDX].THeaderMediumEmpty.Next
    or edi, esi
    mov edi, [EDX].THeaderMediumEmpty.Next
    jnz @exclude_swap_links
    mov [EAX + ebx * 4], esi
    mov edi, [EAX].TMediumManager.FMask
    btr edi, ebx
    mov [EAX].TMediumManager.FMask, edi
  {$else .CPUX64}
    mov r10, [RDX].THeaderMediumEmpty.Prev
    mov r11, [RDX].THeaderMediumEmpty.Next
    mov rax, r10
    or rax, r11
    jnz @exclude_swap_links
    mov [RCX + r9 * 8], r10
    mov r11, [RCX].TMediumManager.FMask
    btr r11, r9
    mov [RCX].TMediumManager.FMask, r11
  {$endif}
  jmp @exclude_done

@exclude_swap_links:
  // (Prev(v5) = nil)? Self.FEmpties[Index(v4)]/Prev(v5).Next := Next(v6)
  {$ifdef CPUX86}
    lea ebx, [EAX + ebx * 4]
    test esi, esi
    lea ebx, [EBX - THeaderMediumEmpty.Next]
    DB $0F, $45, $DE // cmovnz ebx, esi
    mov [EBX].THeaderMediumEmpty.Next, edi
  {$else .CPUX64}
    lea r9, [RCX + r9 * 8]
    test r10, r10
    lea r9, [R9 - THeaderMediumEmpty.Next]
    cmovnz r9, r10
    mov [R9].THeaderMediumEmpty.Next, r11
  {$endif}

  // (Next(v6) <> nil)? Next.Prev := Prev(v5)
  {$ifdef CPUX86}
    lea ebx, [esp - THeaderMediumEmpty.Prev - 4]
    test edi, edi
    DB $0F, $44, $FB // cmovz edi, ebx
    mov [edi].THeaderMediumEmpty.Prev, esi
  {$else .CPUX64}
    lea r9, [rsp - THeaderMediumEmpty.Prev - 8]
    test r11, r11
    cmovz r11, r9
    mov [r11].THeaderMediumEmpty.Prev, r10
  {$endif}

@exclude_done:
  // retrieve Empty, Flags
  {$ifdef CPUX86}
    mov ebx, ecx
    shr ebx, 16
    movzx ecx, cx
    add edx, ebx
  {$else .CPUX64}
    mov r9, r8
    shr r9, 16
    movzx r8, r8w
    add rdx, r9
  {$endif}

@include:
  // Index(v4), Next(v6)
  {$ifdef CPUX86}
    mov ebx, ecx
    shr ebx, 4
    movzx ebx, byte ptr [MEDIUM_INDEXES + ebx]
    mov edi, [EAX + ebx * 4]
  {$else .CPUX64}
    mov r9, r8
    shr r9, 4
    lea rax, [MEDIUM_INDEXES]
    movzx r9, byte ptr [rax + r9]
    mov r11, [RCX + r9 * 8]
  {$endif}

  // Debug: if ((Self.FMask shr Index) and 1 <> Byte(Next <> nil)) then Exit Self.RaiseInvalidPtr
  {$ifdef DEBUG}
    {$ifdef CPUX86}
      push ecx
      mov esi, [EAX].TMediumManager.FMask
      mov ecx, ebx
      shr esi, cl
      and esi, 1
      test edi, edi
      setnz cl
      cmp esi, ecx
      pop ecx
    {$else .CPUX64}
      push rcx
      mov r10, [RCX].TMediumManager.FMask
      mov rcx, r9
      shr r10, cl
      and r10, 1
      test r11, r11
      setnz cl
      cmp r10, rcx
      pop rcx
    {$endif}
    jne @raise_invalid_ptr
  {$endif}

   // Self.FEmpties[Index] := Empty, Self.FMask.SetChecked(Index), Empty.Next := Next
  {$ifdef CPUX86}
     mov [EAX + ebx * 4], edx
     mov esi, [EAX].TMediumManager.FMask
     bts esi, ebx
     mov [EAX].TMediumManager.FMask, esi
     mov [EDX].THeaderMediumEmpty.Next, edi
  {$else .CPUX64}
     mov [RCX + r9 * 8], rdx
     mov r10, [RCX].TMediumManager.FMask
     bts r10, r9
     mov [RCX].TMediumManager.FMask, r10
     mov [RDX].THeaderMediumEmpty.Next, r11
  {$endif}

  // (Next(v6) <> nil)? Next.Prev := Empty(v2), Empty.Prev := nil
  {$ifdef CPUX86}
    lea esi, [esp - THeaderMediumEmpty.Prev - 4]
    test edi, edi
    DB $0F, $44, $FE // cmovz edi, esi
    mov [EDI].THeaderMediumEmpty.Prev, edx
    xor eax, eax
    mov [EDX].THeaderMediumEmpty.Prev, eax
  {$else .CPUX64}
    lea r10, [rsp - THeaderMediumEmpty.Prev - 8]
    test r11, r11
    cmovz r11, r10
    mov [R11].THeaderMediumEmpty.Prev, rdx
    xor rcx, rcx
    mov [RDX].THeaderMediumEmpty.Prev, rcx
  {$endif}

@done:
  // current: mark empty
  {$ifdef CPUX86}
    mov [EDX].THeaderMediumEmpty.Size, ecx
    sub edx, ecx
    mov [EDX].THeaderMedium.Flags, ecx
  {$else .CPUX64}
    mov [RDX].THeaderMediumEmpty.Size, r8
    sub rdx, r8
    mov [RDX].THeaderMedium.Flags, r8
  {$endif}

  // result
  {$ifdef CPUX86}
    pop edi
    pop esi
    pop ebx
  {$endif}
  mov eax, FREEMEM_DONE
  ret

@invalid_ptr:
  {$ifdef CPUX86}
    pop edi
    pop esi
    pop ebx
  {$endif}
  jmp TMediumManager.ErrorInvalidPtr

{$ifdef DEBUG}
@raise_invalid_ptr:
  {$ifdef CPUX86}
    pop edi
    pop esi
    pop ebx
  {$endif}
  jmp TMediumManager.RaiseInvalidPtr
{$endif}
end;
{$endif}

function TThreadHeap.FreeDifficult(P: Pointer; ReturnAddress: Pointer): Integer;
var
  Pool: Pointer;
  PoolThreadHeap: PThreadHeap;
  Line: PK1LineSmall;
  ItemSet: TBitSet8;
  Index: NativeInt;
  Flags, Size: NativeUInt;
begin
  Pool := Pointer(NativeInt(P) and MASK_K64_CLEAR);
  PoolThreadHeap := TK64PoolSmall(Pool^).ThreadHeap;
  if (PoolThreadHeap <> nil) then
  begin
    // pool small
    if (NativeInt(PoolThreadHeap) and MASK_64_TEST <> 0) or
      (PoolThreadHeap <> Pointer(not PoolThreadHeap.FMarkerNotSelf)) then
    begin
      Result := Self.ErrorInvalidPtr;
      Exit;
    end;

    // check pointer
    Line := PK1LineSmall(NativeInt(P) and MASK_K1_CLEAR);
    repeat
      ItemSet := Line.Header.ItemSet;
    until (ItemSet.V64 = Line.Header.ItemSet.V64);
    Index := (NativeInt(P) and MASK_K1_TEST) shr 4;
    if (ItemSet.VLow32 and 3 = 0{not FullQueue}) and
      (ItemSet.VIntegers[Index shr 5] and (1 shl (Index and 31)) <> 0{not Allocated}) then
    begin
      Result := Self.ErrorInvalidPtr;
      Exit;
    end;

    // thread deffered
    PoolThreadHeap.PushThreadDeferred(P, ReturnAddress, True);
  end else
  begin
    // pool medium
    PoolThreadHeap := TK64PoolMedium(Pool^).ThreadHeap;
    if (PoolThreadHeap = nil) or (NativeInt(PoolThreadHeap) and MASK_64_TEST <> 0) or
      (PoolThreadHeap <> Pointer(not PoolThreadHeap.FMarkerNotSelf)) then
    begin
      Result := Self.ErrorInvalidPtr;
      Exit;
    end;

    // check pointer
    Flags := PHeaderMedium(NativeInt(P) - SizeOf(THeaderMedium)).Flags;
    Size := Word(Flags);
    if (Flags and MASK_MEDIUM_ALLOCATED_TEST <> MASK_MEDIUM_ALLOCATED_VALUE) or
      (PHeaderMedium(NativeUInt(P) + Size).PreviousSize <> Size) then
    begin
      Result := Self.ErrorInvalidPtr;
      Exit;
    end;

    // thread deffered
    PoolThreadHeap.PushThreadDeferred(P, ReturnAddress, False);
  end;

  // after thread deffered result
  Result := FREEMEM_DONE;
end;

function TThreadHeap.ResizeDifficult(PFlags: NativeInt{Copy: Boolean; P: Pointer};
  NewB16Count: NativeUInt; ReturnAddress: Pointer): Pointer;
const
  MASK_P_CLEAR = -4;
  FLAG_IS_SMALL = 2;
label
  medium_advanced, return_made_none;
var
  LastB16Count: NativeUInt;
  Pool: Pointer;
  PoolThreadHeap: PThreadHeap;
  Line: PK1LineSmall;
  ItemSet: TBitSet8;
  Index: NativeInt;
  Align: NativeUInt{TMemoryAlign};
  R: Integer;
  Flags, Size: NativeUInt;
  P: Pointer;
begin
  Pool := Pointer(PFlags and MASK_K64_CLEAR);
  PoolThreadHeap := TK64PoolSmall(Pool^).ThreadHeap;
  if (PoolThreadHeap <> nil) then
  begin
    // pool small
    if (NativeInt(PoolThreadHeap) and MASK_64_TEST <> 0) or
      (PoolThreadHeap <> Pointer(not PoolThreadHeap.FMarkerNotSelf)) then
      Self.RaiseInvalidPtr;

    // check pointer
    Line := PK1LineSmall(PFlags and MASK_K1_CLEAR);
    repeat
      ItemSet := Line.Header.ItemSet;
    until (ItemSet.V64 = Line.Header.ItemSet.V64);
    Index := (PFlags and (MASK_K1_TEST and MASK_P_CLEAR)) shr 4;
    if (ItemSet.VLow32 and 3 = 0{not FullQueue}) and
      (ItemSet.VIntegers[Index shr 5] and (1 shl (Index and 31)) <> 0{not Allocated}) then
      Self.RaiseInvalidPtr;

    // small size/flags
    Align{LastB16Count} := NativeUInt(Line.Header.ModeSize) shr 4;
    if (Align{LastB16Count} >= NewB16Count) then goto return_made_none;
    LastB16Count := Align;
    Align := Ord(ma16Bytes);
    Inc(PFlags, FLAG_IS_SMALL);
  end else
  begin
    // pool medium
    PoolThreadHeap := TK64PoolMedium(Pool^).ThreadHeap;
    if (PoolThreadHeap = nil) or (NativeInt(PoolThreadHeap) and MASK_64_TEST <> 0) or
      (PoolThreadHeap <> Pointer(not PoolThreadHeap.FMarkerNotSelf)) then
      Self.RaiseInvalidPtr;

    // check pointer
    Flags := PHeaderMedium((PFlags and MASK_P_CLEAR) - SizeOf(THeaderMedium)).Flags;
    Size := Word(Flags);
    if (Flags and MASK_MEDIUM_ALLOCATED_TEST <> MASK_MEDIUM_ALLOCATED_VALUE) or
      (PHeaderMedium(PFlags and MASK_P_CLEAR + NativeInt(Size)).PreviousSize <> Size) then
      Self.RaiseInvalidPtr;

    // medium size/flags
    Align{LastB16Count} := NativeUInt(PHeaderMedium((PFlags and MASK_P_CLEAR) - SizeOf(THeaderMedium)).WSize) shr 4;
    if (NativeUInt(Align{LastB16Count} - NewB16Count) <= 1) then goto return_made_none;
    LastB16Count := Align;
    Align := Flags shr OFFSET_MEDIUM_ALIGN;
  end;

  // allocate
  case (NewB16Count) of
    0..MAX_SMALL_B16COUNT:
    begin
      if (Align <> NativeUInt(ma16Bytes)) then goto medium_advanced;
      Result := Self.GetSmall(NewB16Count)
    end;
    MAX_SMALL_B16COUNT+1..MIDDLE_MEDIUM_B16COUNT:
    begin
      if (Align <> NativeUInt(ma16Bytes)) then goto medium_advanced;
      Result := Self.FMedium.Get(NewB16Count);
    end;
    MIDDLE_MEDIUM_B16COUNT+1..MAX_MEDIUM_B16COUNT:
    begin
    medium_advanced:
      Result := Self.FMedium.GetAdvanced(NewB16Count, Align{TMemoryAlign});
    end;
  else
    Result := BrainMMGetMemoryPages(
      (NewB16Count + (B16_PER_PAGE - 1)) shr B16_PER_PAGE_SHIFT,
      PAGESMODE_SYSTEM);
    if (Result = nil) then
      Result := Self.ErrorOutOfMemory;
  end;

  // copy (minimum)
  if (NativeInt(Result) and (-(PFlags and 1)) <> 0) then
  begin
    if (LastB16Count > NewB16Count) then LastB16Count := NewB16Count;
    NcMoveB16(P16(PFlags and MASK_P_CLEAR)^, P16(Result)^, LastB16Count);
  end;

  // free small/medium
  P := Pointer(PFlags and MASK_P_CLEAR);
  if (PoolThreadHeap = @Self) then
  begin
    if (PFlags and FLAG_IS_SMALL <> 0) then R := Self.FreeSmall(P)
    else R := Self.FMedium.Free(P);

    if (R = FREEMEM_INVALID) then
      Self.RaiseInvalidPtr;
  end else
  begin
    PoolThreadHeap.PushThreadDeferred(P, ReturnAddress, (PFlags and FLAG_IS_SMALL <> 0));
  end;
  Exit;

return_made_none:
  Result := Pointer(PFlags and MASK_P_CLEAR);
end;

function TThreadHeap.NewK64PoolSmall: PK64PoolSmall;
var
  PagesMode: NativeUInt;
  Next: PK64PoolSmall;
begin
  PagesMode := PAGESMODE_SYSTEM + NativeUInt(FNextHeap = JITHEAP_MARKER);
  Result := BrainMMGetMemoryBlock(BLOCK_64K, PagesMode);
  if (Result = nil) then
  begin
    Result := Self.ErrorOutOfMemory;
    Exit;
  end;

  Result.Header.Flags := 0{not FullQueue};
  Result.ThreadHeap := @Self;
  Result.LineSet.V64 := -1{Empty};

  // Enqueue
  Next := Self.QK64PoolSmall;
  Self.QK64PoolSmall := Result;
  Result.Queue.Prev := nil;
  Result.Queue.Next := Next;
  if (Next <> nil) then Next.Queue.Prev := Result;
end;

function TThreadHeap.DisposeK64PoolSmall(PoolSmall: PK64PoolSmall): Integer;
var
  PagesMode: NativeUInt;
  Prev, Next: PK64PoolSmall;
begin
  // Dequeue
  Prev := PoolSmall.Queue.Prev;
  Next := PoolSmall.Queue.Next;
  if (Prev = nil) then
  begin
    Self.QK64PoolSmall := Next;
  end else
  begin
    Prev.Queue.Next := Next;
  end;
  if (Next <> nil) then
  begin
    Next.Queue.Prev := Prev;
  end;

  PoolSmall.LineSet.V64 := 0{none available lines};
  PoolSmall.ThreadHeap := nil;
  PK64PoolMedium(PoolSmall).ThreadHeap := nil;

  PagesMode := PAGESMODE_SYSTEM + NativeUInt(FNextHeap = JITHEAP_MARKER);
  if (BrainMMFreeMemoryBlock(PoolSmall, PagesMode)) then
  begin
    Result := FREEMEM_DONE;
  end else
  begin
    Result := Self.ErrorInvalidPtr;
  end;
end;

function TThreadHeap.GetNewK1LineSmall(B16Count: NativeUInt): Pointer;
label
  allocated_pool, new_k64pool;
var
  Index: NativeUInt;
  Line: PK1LineSmall;
  PoolSmall, Next, NextFull: PK64PoolSmall;

  P: PInteger;
  VInteger: Integer;
  BitSet: PBitSet8;

  {$ifdef CPUX86}
  ThreadHeap: PThreadHeap;
  {$endif}
begin
  // take small pool, reserve line, get first
  Next := Self.QK64PoolSmall;
  if (Next <> nil) then
  begin
    repeat
      PoolSmall := Next;
    allocated_pool:
      P := Pointer(@PoolSmall.LineSet);
      P := @PBitSet8(P).VIntegers[Byte(PoolSmall.LineSet.VLow32 = 0)];
      VInteger := P^;
      if ({has available lines}VInteger <> 0) then
      begin
        // reserve line
        Inc(PWord(P), Byte(VInteger and $ffff = 0));
        Inc(NativeUInt(P), Byte(PByte(P)^ = 0));
        Index := (NativeUInt(P) - NativeUInt(@PoolSmall.LineSet)) shl 3 + BIT_SCANS[PByte(P)^];
        PInteger(NativeInt(P) and -4)^ := VInteger xor (1 shl (Index and 31));

        // store line
        {$ifdef CPUX86}
        ThreadHeap := PoolSmall.ThreadHeap;
        {$endif}
        Index := Index shl 10;
        Line := Pointer(PoolSmall);
        Inc(NativeUInt(Line), Index);
        {$ifdef CPUX86}
          ThreadHeap.FK1LineSmalls[B16Count] := Line;
        {$else}
          Self.FK1LineSmalls[B16Count] := Line;
        {$endif}
        Line.Header.Queue.Prev := nil;
        Line.Header.Queue.Next := nil;

        // parameters
        Index := {8 * IsFirst: Boolean}((Index - 1) shr ({$ifdef LARGEINT}63{$else}31{$endif} - 3)) and 8;
        Index := Index + {BaseIndex}(B16Count - 1);
        Line.Header.Flags := {ModeSizeBits}(B16Count shl 4) + Index;

        // item set, get first
        BitSet := Pointer(@DEFAULT_BITSETS_SMALL[Index]);
        Index := FIRST_BITSETS_SMALL[Index];
        {$ifdef LARGEINT}
          Line.Header.ItemSet.V64 := BitSet.V64 xor NativeInt(Word(Index));
        {$else .SMALLINT}
          Line.Header.ItemSet.VHigh32 := BitSet.VHigh32;
          Line.Header.ItemSet.VLow32 := BitSet.VLow32 xor NativeInt(Word(Index));
        {$endif}
        Inc(NativeUInt(Line), Index shr 16);
        Result := Line;

        Exit;
      end;

      // Full (QK64PoolSmall) --> (QK64PoolSmallFull)
      begin
        PoolSmall.Header.InQK64PoolSmallFull := True;
        // Dequeue
        Next := {Self.QK64}PoolSmall.Queue.Next;
        Self.QK64PoolSmall := Next;
        // Enqueue
        NextFull := Self.QK64PoolSmallFull;
        Self.QK64PoolSmallFull := PoolSmall;
        { PoolSmall.Queue.Prev := already nil; }
        PoolSmall.Queue.Next := NextFull;
        if (Next <> nil) then
        begin
          Next.Queue.Prev := nil;
          if (NextFull <> nil) then NextFull.Queue.Prev := PoolSmall;
          // Continue;
        end else
        begin
          if (NextFull <> nil) then NextFull.Queue.Prev := PoolSmall;
          goto new_k64pool;
        end;
      end;
    until (False)
  end else
  begin
  new_k64pool:
    PoolSmall := Self.NewK64PoolSmall;
    if (PoolSmall <> nil) then goto allocated_pool;
  end;

  // failure
  Result := nil;
end;

function TThreadHeap.DisposeK1LineSmall(Line: PK1LineSmall): Integer;
{$ifdef PUREPASCAL}
var
  Index: NativeUInt;
  PoolSmall: PK64PoolSmall;
  Prev, Next: Pointer;
begin
  // Dequeue
  Prev := Line.Header.Queue.Prev;
  Next := Line.Header.Queue.Next;
  if (Prev = nil) then
  begin
    Self.FK1LineSmalls[(Line.Header.Flags and 7) + 1] := Next;
  end else
  begin
    PK1LineSmall(Prev).Header.Queue.Next := Next;
  end;
  if (Next <> nil) then
  begin
    PK1LineSmall(Next).Header.Queue.Prev := Prev;
  end;

  PoolSmall := Pointer(NativeUInt(Line) and MASK_K64_CLEAR);
  if (PoolSmall.Header.InQK64PoolSmallFull) then
  begin
    // QK64PoolSmallFull --> QK64PoolSmall
    PoolSmall.Header.InQK64PoolSmallFull := False;
    // Dequeue
    Prev := PoolSmall.Queue.Prev;
    Next := PoolSmall.Queue.Next;
    if (Prev = nil) then
    begin
      Self.QK64PoolSmallFull := Next;
    end else
    begin
      PK64PoolSmall(Prev).Queue.Next := Next;
    end;
    if (Next <> nil) then
    begin
      PK64PoolSmall(Next).Queue.Prev := Prev;
    end;
    // Enqueue
    Next := Self.QK64PoolSmall;
    Self.QK64PoolSmall := PoolSmall;
    PoolSmall.Queue.Prev := nil;
    PoolSmall.Queue.Next := Next;
    if (Next <> nil) then PK64PoolSmall(Next).Queue.Prev := PoolSmall;
  end;

  Line.Header.ItemSet.V64 := 0{non available items};
  Line.Header.Flags := 0;
  Index := (NativeUInt(Line) and MASK_K64_TEST) shr 10;

  if (not BitUnreserve(PoolSmall.LineSet, Index)) then
  begin
    Result := Self.ErrorInvalidPtr;
    Exit;
  end;

  if (PoolSmall.LineSet.V64 = -1{Empty}) then
  begin
    Result := DisposeK64PoolSmall(PoolSmall);
  end else
  begin
    Result := FREEMEM_DONE;
  end;
end;
{$else}
asm
  // v1 := @Self.FK1LineSmalls[(Line.Header.Flags and 7) + 1] - TK1LineSmall.Header.Queue.Next
  // x86: store (v4)
  {$ifdef CPUX86}
    mov ecx, [EDX].TK1LineSmall.Header.Flags
    and ecx, 7
    lea eax, [eax + TThreadHeap.FK1LineSmalls - TK1LineSmall.Header.Queue.Next]
    push ebx
    lea eax, [eax + 4 * ecx]
  {$else .CPUX64}
    mov r8, [RDX].TK1LineSmall.Header.Flags
    and r8, 7
    lea rcx, [rcx + TThreadHeap.FK1LineSmalls - TK1LineSmall.Header.Queue.Next]
    lea rcx, [rcx + 8 * r8]
  {$endif}

  // dequeue
  {$ifdef CPUX86}
    mov ecx, [EDX].TK1LineSmall.Header.Queue.Prev
    mov ebx, [EDX].TK1LineSmall.Header.Queue.Next
    test ecx, ecx
    DB $0F, $45, $C1 // cmovnz eax, ecx
    mov [EAX].TK1LineSmall.Header.Queue.Next, ebx
    lea eax, [esp - 64]
    test ebx, ebx
    DB $0F, $45, $C3 // cmovnz eax, ebx
    mov [EAX].TK1LineSmall.Header.Queue.Prev, ecx
  {$else .CPUX64}
    mov r8, [RDX].TK1LineSmall.Header.Queue.Prev
    mov r9, [RDX].TK1LineSmall.Header.Queue.Next
    test r8, r8
    cmovnz rcx, r8
    mov [RCX].TK1LineSmall.Header.Queue.Next, r9
    lea rcx, [rsp - 64]
    test r9, r9
    cmovnz rcx, r9
    mov [RCX].TK1LineSmall.Header.Queue.Prev, r8
  {$endif}

  // v3 := PoolSmalOf(Line)
  // if PoolSmall(v3).Header.InQK64PoolSmallFull then goto Requeue(QK64PoolSmallFull, QK64PoolSmall);
  {$ifdef CPUX86}
    mov ecx, MASK_K64_CLEAR
    and ecx, edx
    cmp [ECX].TK1LineSmall.Header.Flags, $00ffffff
  {$else .CPUX64}
    mov r8, MASK_K64_CLEAR
    and r8, rdx
    cmp [R8].TK1LineSmall.Header.Flags, $00ffffff
  {$endif}
  ja @pool_small_requeue

  // clear line flags
  // Index (v2) := IndexOf(Line)
@dispose_line:
  {$ifdef CPUX86}
    xor eax, eax
    mov [EDX].TK1LineSmall.Header.ItemSet.VLow32, eax
    mov [EDX].TK1LineSmall.Header.ItemSet.VHigh32, eax
    mov [EDX].TK1LineSmall.Header.Flags, eax
    and edx, MASK_K64_TEST
    shr edx, 10
  {$else .CPUX64}
    xor rcx, rcx
    mov [RDX].TK1LineSmall.Header.ItemSet.V64, rcx
    mov [RDX].TK1LineSmall.Header.Flags, rcx
    and rdx, MASK_K64_TEST
    shr rdx, 10
  {$endif}

  // if (not BitUnreserve(PoolSmall.LineSet, Index)) then Exit Self.ErrorInvalidPtr;
  {$ifdef CPUX86}
    lea eax, [ECX].TK64PoolSmall.LineSet.VLow32
    lea ebx, [ECX].TK64PoolSmall.LineSet.VHigh32
    test edx, 32
    DB $0F, $45, $C3 // cmovnz eax, ebx
    and edx, 31
    mov ebx, [eax]
    bts ebx, edx
    mov [eax], ebx
    pop ebx
  {$else .CPUX64}
    mov rax, [R8].TK64PoolSmall.LineSet.V64
    bts rax, rdx
    mov [R8].TK64PoolSmall.LineSet.V64, rax
  {$endif}
  jc @error_invalid_ptr

  // if (PoolSmall(v3).LineSet.V64 = -1{Empty}) then Exit Self.DisposeK64PoolSmall(PoolSmall);
  {$ifdef CPUX86}
    mov edx, [ECX].TK64PoolSmall.LineSet.VLow32
    mov eax, [ECX].TK64PoolSmall.LineSet.VHigh32
    add edx, 1
    add eax, 1
    or edx, eax
    mov eax, [ECX].TK64PoolSmall.ThreadHeap
    xchg edx, ecx
  {$else .CPUX64}
    cmp rax, -1
    mov rcx, [R8].TK64PoolSmall.ThreadHeap
    xchg rdx, r8
  {$endif}
  je TThreadHeap.DisposeK64PoolSmall

  // result
  mov eax, FREEMEM_DONE
  ret

@pool_small_requeue:
  // PoolSmall.Header.InQK64PoolSmallFull := False
  {$ifdef CPUX86}
    mov [ECX].TK64PoolSmall.Header.InQK64PoolSmallFull, 0
  {$else .CPUX64}
    mov [R8].TK64PoolSmall.Header.InQK64PoolSmallFull, 0
  {$endif}

  // retrieve Self, store line
  // x64: store Self
  {$ifdef CPUX86}
    mov eax, [ECX].TK64PoolSmall.ThreadHeap
    push edx
  {$else .CPUX64}
    mov rcx, [R8].TK64PoolSmall.ThreadHeap
    xchg rax, rdx
    mov r11, rcx
  {$endif}

  // dequeue
  {$ifdef CPUX86}
    lea eax, [EAX + TK64PoolSmall.QK64PoolSmallFull - TK64PoolSmall.Queue.Next]
    mov edx, [ECX].TK64PoolSmall.Queue.Prev
    mov ebx, [ECX].TK64PoolSmall.Queue.Next
    test edx, edx
    DB $0F, $45, $C2 // cmovnz eax, edx
    mov [EAX].TK64PoolSmall.Queue.Next, ebx
    lea eax, [esp - 64]
    test ebx, ebx
    DB $0F, $45, $C3 // cmovnz eax, ebx
    mov [EAX].TK64PoolSmall.Queue.Prev, edx
  {$else .CPUX64}
    lea rcx, [RCX + TK64PoolSmall.QK64PoolSmallFull - TK64PoolSmall.Queue.Next]
    mov rdx, [R8].TK64PoolSmall.Queue.Prev
    mov r9, [R8].TK64PoolSmall.Queue.Next
    test rdx, rdx
    cmovnz rcx, rdx
    mov [RCX].TK64PoolSmall.Queue.Next, r9
    lea rcx, [rsp - 64]
    test r9, r9
    cmovnz rcx, r9
    mov [RCX].TK64PoolSmall.Queue.Prev, rdx
  {$endif}

  // retrieve Self, enqueue, retrieve Line
  {$ifdef CPUX86}
    mov eax, [ECX].TK64PoolSmall.ThreadHeap
    mov edx, [EAX].TThreadHeap.QK64PoolSmall
    mov [EAX].TThreadHeap.QK64PoolSmall, ecx
    mov [ECX].TK64PoolSmall.Queue.Prev, 0
    mov [ECX].TK64PoolSmall.Queue.Next, edx
    lea ebx, [esp - 64]
    test edx, edx
    DB $0F, $45, $DA // cmovnz ebx, edx
    mov [EBX].TK64PoolSmall.Queue.Prev, ecx
    pop edx
  {$else .CPUX64}
    xchg rcx, r11
    mov rdx, [RCX].TThreadHeap.QK64PoolSmall
    mov [RCX].TThreadHeap.QK64PoolSmall, r8
    xor r11, r11
    mov [R8].TK64PoolSmall.Queue.Prev, r11
    mov [R8].TK64PoolSmall.Queue.Next, rdx
    lea r9, [rsp - 64]
    test rdx, rdx
    cmovnz r9, rdx
    mov [R9].TK64PoolSmall.Queue.Prev, r8
    xchg rdx, rax
  {$endif}

  jmp @dispose_line
@error_invalid_ptr:
  {$ifdef CPUX86}
    mov eax, [ECX].TK64PoolSmall.ThreadHeap
  {$else .CPUX64}
    mov rcx, [R8].TK64PoolSmall.ThreadHeap
  {$endif}
  jmp TThreadHeap.ErrorInvalidPtr
end;
{$endif}

function TThreadHeap.NewK64PoolMedium: PK64PoolMedium;
const
  HIGH_EMPTY_INDEX = High(PThreadHeap(nil).FMedium.FEmpties);
var
  PagesMode: NativeUInt;
  Start, Finish: PHeaderMedium;
  Empty: PHeaderMediumEmpty;
  Next: Pointer;
begin
  // allocate
  PagesMode := PAGESMODE_SYSTEM + NativeUInt(FNextHeap = JITHEAP_MARKER);
  Result := BrainMMGetMemoryBlock(BLOCK_64K, PagesMode);
  if (Result = nil) then
  begin
    Result := Self.ErrorOutOfMemory;
    Exit;
  end;

  // mark
  begin
    Result.ThreadHeap := @Self;
    Result.MarkerNil := nil;
    Result.FlagsFakeAllocated := MASK_MEDIUM_ALLOCATED;

    Start := @Result.Items[Low(Result.Items)];
    Start.PreviousSize := 0;
    Start.Flags := FULL_MEDIUM_EMPTY_SIZE + {Align: ma16Bytes, Allocated: False}0;

    Finish := @Result.Items[High(Result.Items)];
    Finish.PreviousSize := FULL_MEDIUM_EMPTY_SIZE;
    Finish.Flags := MASK_MEDIUM_ALLOCATED;
  end;

  // QK64PoolMedium enqueue
  Next := Self.FMedium.QK64PoolMedium;
  Self.FMedium.QK64PoolMedium := Result;
  Result.Queue.Prev := nil;
  Result.Queue.Next := Next;
  if (Next <> nil) then PK64PoolMedium(Next).Queue.Prev := Result;

  // empty enque
  Empty := Pointer(@Result.Items[High(Result.Items) - 1]);
  Next := Self.FMedium.FEmpties[HIGH_EMPTY_INDEX];
  Self.FMedium.FEmpties[HIGH_EMPTY_INDEX] := Empty;
  Self.FMedium.FMask := Self.FMedium.FMask or (NativeInt(1) shl HIGH_EMPTY_INDEX);
  Empty.Prev := nil;
  Empty.Next := Next;
  if (Next <> nil) then PHeaderMediumEmpty(Next).Prev := Empty;
end;

function TThreadHeap.DisposeK64PoolMedium(PoolMedium: PK64PoolMedium): Integer;
var
  PagesMode: NativeUInt;
  Prev, Next: PK64PoolMedium;
begin
  // Dequeue
  Prev := PoolMedium.Queue.Prev;
  Next := PoolMedium.Queue.Next;
  if (Prev = nil) then
  begin
    FMedium.QK64PoolMedium := Next;
  end else
  begin
    Prev.Queue.Next := Next;
  end;
  if (Next <> nil) then
  begin
    Next.Queue.Prev := Prev;
  end;

  PoolMedium.ThreadHeap := nil;
  PK64PoolSmall(PoolMedium).ThreadHeap := nil;

  PagesMode := PAGESMODE_SYSTEM + NativeUInt(FNextHeap = JITHEAP_MARKER);
  if (BrainMMFreeMemoryBlock(PoolMedium, PagesMode)) then
  begin
    Result := FREEMEM_DONE;
  end else
  begin
    Result := Self.ErrorInvalidPtr;
  end;
end;

function BrainMMGetMemoryOptions(const P: Pointer; var Options: TMemoryOptions): Boolean;
label
  small_pointer, medium_pointer, fail;
var
  X: NativeInt;
  Pool: Pointer;
  ThreadHeap: PThreadHeap;
  Line: PK1LineSmall;
  Index: NativeInt;
  Flags, Size: NativeUInt;
begin
  if (P <> nil) and (NativeUInt(P) and 15 = 0) then
  try
    // basics
    X := NativeInt(P);
    PCardinal(@Options)^ := (3{marRead/marWrite} shl 24);

    // modes
    if (X and MASK_K4_TEST <> 0) then
    begin
      Pool := Pointer(X and MASK_K64_CLEAR);
      ThreadHeap := TK64PoolSmall(Pool^).ThreadHeap;
      if (ThreadHeap <> nil) then
      begin
        // pool small
        if (X and MASK_K1_TEST = 0) or (NativeInt(ThreadHeap) and MASK_64_TEST <> 0) then goto fail;
        if (ThreadHeap.FNextHeap <> JITHEAP_MARKER) then
        begin
          if (ThreadHeap <> Pointer(not ThreadHeap.FMarkerNotSelf)) then goto fail;

          // small pointer
          Options.Kind := mkSmall;
        small_pointer:
          Options.ThreadId := ThreadHeap.ThreadId;
          Line := PK1LineSmall(X and MASK_K1_CLEAR);
          Options.Size := Line.Header.Flags and $f0;
          Index := (X and MASK_K1_TEST) shr 4;
          if (Line.Header.ItemSet.VLow32 and 3 = 0{not FullQueue}) and
            (Line.Header.ItemSet.VIntegers[Index shr 5] and (1 shl (Index and 31)) <> 0{not Allocated}) then goto fail;

          // done
          Result := True;
          Exit;
        end else
        begin
          Options.Kind := mkJIT;
          if (ThreadHeap = Pointer(ThreadHeap.FMarkerNotSelf xor NativeUInt(-2))) then goto small_pointer;
          goto fail;
        end;
      end else
      begin
        // pool medium
        ThreadHeap := TK64PoolMedium(Pool^).ThreadHeap;
        if (ThreadHeap = nil) or (NativeInt(ThreadHeap) and MASK_64_TEST <> 0) then goto fail;
        if (ThreadHeap.FNextHeap <> JITHEAP_MARKER) then
        begin
          if (ThreadHeap <> Pointer(not ThreadHeap.FMarkerNotSelf)) then goto fail;

          // medium pointer
          Options.Kind := mkMedium;
        medium_pointer:
          Options.ThreadId := ThreadHeap.ThreadId;
          Flags := PHeaderMedium(X - SizeOf(THeaderMedium)).Flags;
          Options.Align := TMemoryAlign((Flags and MASK_MEDIUM_ALIGN) shr OFFSET_MEDIUM_ALIGN);
          Size := Word(Flags);
          Options.Size := Size;
          if (Flags and MASK_MEDIUM_ALLOCATED_TEST <> MASK_MEDIUM_ALLOCATED_VALUE) or
            (PHeaderMedium(NativeUInt(X) + Size).PreviousSize <> Size) then goto fail;

          // done
          Result := True;
          Exit;
        end else
        begin
          Options.Kind := mkJIT;
          if (ThreadHeap = Pointer(ThreadHeap.FMarkerNotSelf xor NativeUInt(-2))) then goto medium_pointer;
          goto fail;
        end;
      end;
    end else
    begin
      // blocks, big or large
      // ToDo

      Options.Kind := mkLarge;
      Options.ThreadId := 0;
      Options.Size :=  PNativeUInt(X - SizeOf(NativeUInt))^ * SIZE_K4;

      // done
      Result := True;
      Exit;
    end;

  fail:
  except
  end;

  // fail
  Result := False;
end;

function BrainMMThreadHeapMinimize: Boolean;
var
  ThreadHeap: PThreadHeap;
begin
  {$ifdef PUREPASCAL}
  ThreadHeap := ThreadHeapInstance;
  if (ThreadHeap <> nil) and (ThreadHeap.Deferreds.Assigned) then
  {$else}
  ThreadHeap := CurrentThreadHeap;
  if (ThreadHeap.Deferreds.Assigned) then
  {$endif}
  begin
    ThreadHeap.ProcessThreadDeferred;
    Result := True;
    Exit;
  end;

  Result := False;
end;


{ TJITHeap }

type
  PJITHashItem = ^TJITHashItem;
  TJITHashItem = packed record
    Next: PJITHashItem;
    Pages: MemoryPages;
  end;

function TJITHeap.HeapInstance: {PThreadHeap}Pointer;
begin
  Result := Pointer(NativeInt(@FHeapBuffer[63]) and MASK_64_CLEAR);
end;

constructor TJITHeap.Create;
var
  ThreadHeap: PThreadHeap;
begin
  inherited;
  ThreadHeap := HeapInstance;
  ThreadHeap.FNextHeap := JITHEAP_MARKER;
  ThreadHeap.FMarkerNotSelf := (not SupposedPtr(ThreadHeap)) xor 1;
  ThreadHeap.ThreadId := GetCurrentThreadId;
end;

destructor TJITHeap.Destroy;
begin
  Clear;
  inherited;
end;

procedure TJITHeap.EnqueueBigOrLarge(Pages: MemoryPages);
var
  Heap: PThreadHeap;
  Index: NativeUInt;
  HashItem: PJITHashItem;
begin
  Heap := HeapInstance;
  Index := (NativeUInt(Pages) shr (10 + 2)) and High(FBigOrLargeHash);

  HashItem := Heap.GetSmall(1);
  HashItem.Next := FBigOrLargeHash[Index];
  HashItem.Pages := Pages;
  FBigOrLargeHash[Index] := HashItem;
end;

function TJITHeap.DequeueBigOrLarge(Pages: MemoryPages): Boolean;
var
  Heap: PThreadHeap;
  Index: NativeUInt;
  HashItem, Next: PJITHashItem;
begin
  Heap := HeapInstance;
  Index := (NativeUInt(Pages) shr (10 + 2)) and High(FBigOrLargeHash);

  HashItem := FBigOrLargeHash[Index];
  if (HashItem <> nil) then
  begin
    Next := HashItem.Next;
    if (HashItem.Pages = Pages) then
    begin
      FBigOrLargeHash[Index] := Next;
      Heap.FreeSmall(HashItem);
      Result := True;
      Exit;
    end else
    begin
      repeat
        if (Next = nil) then Break;

        if (Next.Pages = Pages) then
        begin
          HashItem.Next := Next.Next;
          Heap.FreeSmall(Next);
          Result := True;
          Exit;
        end;

        HashItem := Next;
        Next := Next.Next;
      until (False);
    end;
  end;

  Result := False;
end;

{$ifdef CPUINTEL}
procedure JITHeapClear(Self: TJITHeap; ReturnAddress: Pointer); forward;
procedure TJITHeap.Clear; stdcall;
asm
  {$ifdef CPUX86}
    pop ebp
    pop edx
    pop eax
    push edx
  {$else .CPUX64}
    mov rdx, [rsp]
  {$endif}
  jmp JITHeapClear
end;
{$endif}

{$ifdef CPUINTEL}
procedure JITHeapClear(Self: TJITHeap; ReturnAddress: Pointer);
{$else}
procedure TJITHeap.Clear; stdcall;
{$endif}
var
  Heap: PThreadHeap;
  Index: NativeUInt;
  HashItem: PJITHashItem;
  PoolSmall: PK64PoolSmall;
  PoolMedium: PK64PoolMedium;
  Next: Pointer;
begin
  Heap := {HeapInstance}Pointer(NativeInt(@Self.FHeapBuffer[63]) and MASK_64_CLEAR);
  Heap.ErrorAddr := ReturnAddress;

  // clear big or larges
  for Index := Low(Self.FBigOrLargeHash) to High(Self.FBigOrLargeHash) do
  begin
    HashItem := Self.FBigOrLargeHash[Index];
    if (HashItem <> nil) then
    begin
      Self.FBigOrLargeHash[Index] := nil;
      repeat
        if (not BrainMMFreeMemoryPages(HashItem.Pages, PAGESMODE_JIT)) then
        begin
          Heap.RaiseInvalidPtr;
        end;

        HashItem := HashItem.Next;
      until (HashItem = nil);
    end;
  end;

  // 1kb-lines (small)
  Heap.QK1LineFull := nil;
  for Index := Low(Heap.FK1LineSmalls) to High(Heap.FK1LineSmalls) do
    Heap.FK1LineSmalls[Index] := nil;

  // pool small
  PoolSmall := Heap.QK64PoolSmall;
  if (PoolSmall <> nil) then
  begin
    Heap.QK64PoolSmall := nil;
    repeat
      Next := PoolSmall.Queue.Next;

      if (not MemoryManager.BrainMM.FreeMemoryBlock(PoolSmall, PAGESMODE_JIT)) then
      begin
        Heap.RaiseInvalidPtr;
      end;

      PoolSmall := Next;
    until (PoolSmall = nil);
  end;

  // pool small (full)
  PoolSmall := Heap.QK64PoolSmallFull;
  if (PoolSmall <> nil) then
  begin
    Heap.QK64PoolSmallFull := nil;
    repeat
      Next := PoolSmall.Queue.Next;

      if (not MemoryManager.BrainMM.FreeMemoryBlock(PoolSmall, PAGESMODE_JIT)) then
      begin
        Heap.RaiseInvalidPtr;
      end;

      PoolSmall := Next;
    until (PoolSmall = nil);
  end;

  // pool medium
  PoolMedium := Heap.FMedium.QK64PoolMedium;
  if (PoolMedium <> nil) then
  begin
    Heap.FMedium.QK64PoolMedium := nil;
    repeat
      Next := PoolMedium.Queue.Next;

      if (not MemoryManager.BrainMM.FreeMemoryBlock(PoolMedium, PAGESMODE_JIT)) then
      begin
        Heap.RaiseInvalidPtr;
      end;

      PoolMedium := Next;
    until (PoolMedium = nil);
  end;
end;

{$ifdef CPUINTEL}
function JITHeapGetMemory(Self: TJITHeap; Size: NativeInt; ReturnAddress: Pointer): Pointer; forward;
function TJITHeap.GetMemory(Size: NativeInt): Pointer; stdcall;
asm
  {$ifdef CPUX86}
    pop ebp
    pop ecx
    pop eax
    pop edx
    push ecx
  {$else .CPUX64}
    mov r8, [rsp]
  {$endif}
  jmp JITHeapGetMemory
end;
{$endif}

{$ifdef CPUINTEL}
function JITHeapGetMemory(Self: TJITHeap; Size: NativeInt; ReturnAddress: Pointer): Pointer;
{$else}
function TJITHeap.GetMemory(Size: NativeInt): Pointer; stdcall;
{$endif}
var
  Heap: PThreadHeap;
  B16Count: NativeUInt;
begin
  if (Size > 0) then
  begin
    Heap := {HeapInstance}Pointer(NativeInt(@Self.FHeapBuffer[63]) and MASK_64_CLEAR);
    Heap.ErrorAddr := ReturnAddress;

    B16Count := (Size + 15) shr 4;
    case (B16Count) of
      MAX_SMALL_B16COUNT+1..MIDDLE_MEDIUM_B16COUNT:
        Result := Heap.FMedium.Get(B16Count);
      MIDDLE_MEDIUM_B16COUNT+1..MAX_MEDIUM_B16COUNT:
        Result := Heap.FMedium.GetAdvanced(B16Count, Ord(ma16Bytes));
    else
      Result := MemoryManager.BrainMM.GetMemoryPages(
        (B16Count + (B16_PER_PAGE - 1)) shr B16_PER_PAGE_SHIFT, PAGESMODE_JIT);
      if (Result = nil) then
      begin
        Heap.RaiseOutOfMemory;
      end;

      Self.EnqueueBigOrLarge(MemoryPages(Result));
    end;
  end else
  begin
    Result := nil;
  end;
end;

{$ifdef CPUINTEL}
procedure JITHeapFreeMemory(Self: TJITHeap; P: Pointer; ReturnAddress: Pointer); forward;
procedure TJITHeap.FreeMemory(P: Pointer); stdcall;
asm
  {$ifdef CPUX86}
    pop ebp
    pop ecx
    pop eax
    pop edx
    push ecx
  {$else .CPUX64}
    mov r8, [rsp]
  {$endif}
  jmp JITHeapFreeMemory
end;
{$endif}

{$ifdef CPUINTEL}
procedure JITHeapFreeMemory(Self: TJITHeap; P: Pointer; ReturnAddress: Pointer);
{$else}
procedure TJITHeap.FreeMemory(P: Pointer); stdcall;
{$endif}
label
  medium;
var
  Heap: PThreadHeap;
  Pool: Pointer{PK64PoolSmall/PK64PoolMedium};
begin
  if (P <> nil) then
  begin
    Heap := {HeapInstance}Pointer(NativeInt(@Self.FHeapBuffer[63]) and MASK_64_CLEAR);
    Heap.ErrorAddr := ReturnAddress;

    if (NativeInt(P) and MASK_16_TEST = 0) then
    begin
      Pool := Pointer(NativeInt(P) and MASK_K64_CLEAR);

      if (NativeInt(P) and MASK_K1_TEST <> 0) then
      begin
        // pool: small or medium
        if (PK64PoolSmall(Pool).ThreadHeap = Heap) then
        begin
          // pool small
          Heap.FreeSmall(P);
          Exit;
        end else
        if (PK64PoolSmall(Pool).ThreadHeap = nil) then
        begin
          // pool medium
        medium:
          if (PK64PoolMedium(Pool).ThreadHeap = Heap) then
          begin
            Heap.FMedium.Free(P);
            Exit;
          end;
        end;
      end else
      begin
        if (NativeInt(P) and MASK_K4_TEST = 0) then
        begin
          // big or large
          if (Self.DequeueBigOrLarge(MemoryPages(P))) then
          begin
            if (MemoryManager.BrainMM.FreeMemoryPages(MemoryPages(P), PAGESMODE_JIT)) then
              Exit;
          end;
        end else
        begin
          // medium or invalid pointer
          if (PK64PoolSmall(Pool).ThreadHeap = nil) then goto medium;
        end;
      end;
    end;

    // "default" method
    Heap.RaiseInvalidPtr;
  end;
end;

function TJITHeap.SyncGetMemory(Size: NativeInt): Pointer; stdcall;
begin
  if (Size > 0) then
  begin
    // inline SpinLock
    repeat
      if (Self.FSpin <> 0) then SpinWait(SupposedPtr(Self.FSpin), High(NativeUInt));
    until (0 = AtomicCmpExchange(SupposedPtr(Self.FSpin), 1, 0));
    try
      Result := Self.GetMemory(Size);
    finally
      FSpin := 0; // inline SpinUnlock
    end;
  end else
  begin
    Result := nil;
  end;
end;

procedure TJITHeap.SyncFreeMemory(P: Pointer); stdcall;
begin
  if (P <> nil) then
  begin
    // inline SpinLock
    repeat
      if (Self.FSpin <> 0) then SpinWait(SupposedPtr(Self.FSpin), High(NativeUInt));
    until (0 = AtomicCmpExchange(SupposedPtr(Self.FSpin), 1, 0));
    try
      Self.FreeMemory(P);
    finally
      FSpin := 0; // inline SpinUnlock
    end;
  end;
end;


{ TMalloc }

function TMalloc.Alloc(cb: Longint): Pointer; stdcall;
begin
  {$ifdef BRAINMM_REDIRECT}
    GetMem(Result, cb);
  {$else}
    if (cb > 0) then
    begin
      Result := MemoryManager.Standard.GetMem(NativeUInt(cb));
      if (Result = nil) then
        {$ifdef CONDITIONALEXPRESSIONS}System.Error(reOutOfMemory){$else}System.RunError(203){$endif};
    end else
    begin
      Result := nil;
    end;
  {$endif}
end;

function TMalloc.Realloc(pv: Pointer; cb: Longint): Pointer; stdcall;
begin
  {$ifdef BRAINMM_REDIRECT}
    Result := pv;
    ReallocMem(Result, cb);
  {$else}
    Result := pv;
    if (cb > 0) then
    begin
      if (Result <> nil) then
      begin
        Result := MemoryManager.Standard.ReallocMem(Result, NativeUInt(cb));
      end else
      begin
        Result := MemoryManager.Standard.GetMem(NativeUInt(cb));
      end;
      if (Result = nil) then
        {$ifdef CONDITIONALEXPRESSIONS}System.Error(reOutOfMemory){$else}System.RunError(203){$endif};
    end else
    begin
      if (Result <> nil) then
      begin
        if (MemoryManager.Standard.FreeMem(Result) {$ifdef FPC}={$else}<>{$endif} 0) then
          {$ifdef CONDITIONALEXPRESSIONS}System.Error(reInvalidPtr){$else}System.RunError(204){$endif};
        Result := nil;
      end;
    end;
  {$endif}
end;

procedure TMalloc.Free(pv: Pointer); stdcall;
begin
  {$ifdef BRAINMM_REDIRECT}
    FreeMem(pv);
  {$else}
    if (pv <> nil) then
    begin
      if (MemoryManager.Standard.FreeMem(pv) {$ifdef FPC}={$else}<>{$endif} 0) then
        {$ifdef CONDITIONALEXPRESSIONS}System.Error(reInvalidPtr){$else}System.RunError(204){$endif};
    end;
  {$endif}
end;

function TMalloc.GetSize(pv: Pointer): Longint; stdcall;
var
  Options: TMemoryOptions;
begin
  if (not MemoryManager.BrainMM.GetMemoryOptions(pv, Options)) then
  begin
    Result := -1;
  end else
  begin
    Result := Options.Size;
  end;
end;

function TMalloc.DidAlloc(pv: Pointer): Integer; stdcall;
var
  Options: TMemoryOptions;
begin
  Result := Byte(MemoryManager.BrainMM.GetMemoryOptions(pv, Options));
end;

procedure TMalloc.HeapMinimize; stdcall;
begin
  MemoryManager.BrainMM.ThreadHeapMinimize;
end;


 (* Debug memory routine  *)

function BrainMMRegisterExpectedMemoryLeak(P: Pointer): Boolean;
begin
  // todo
  Result := False;
end;

function BrainMMUnregisterExpectedMemoryLeak(P: Pointer): Boolean;
begin
  // todo
  Result := False;
end;


const
  LINE_BREAK: PWideChar = {$ifdef MSWINDOWS}#13#10{$else .POSIX}#10{$endif};
  TEXT_BUFFER_SIZE = 32768;

function WStrLen(S: PWideChar): NativeUInt;
begin
  Result := 0;

  if (S <> nil) then
  while (S^ <> #0) do
  begin
    Inc(Result);
    Inc(S);
  end;
end;

function AppendString(S: PWideChar; Value: PWideChar): PWideChar;
begin
  Result := S;

  if (Value <> nil) then
  while (Value^ <> #0) do
  begin
    Result^ := Value^;
    Inc(Result);
    Inc(Value);
  end;
end;

function AppendBoolean(S: PWideChar; Value: Boolean): PWideChar;
const
  VALUES: array[Boolean] of PWideChar = ('False', 'True');
begin
  Result := AppendString(S, VALUES[Value]);
end;

function AppendUnsigned(S: PWideChar; Value: NativeUInt): PWideChar;
var
  I: Integer;
  Buffer: array[0..18] of WideChar;
begin
  I := 0;
  repeat
    Buffer[I] := WideChar((Value mod 10) + Ord('0'));
    Value := Value div 10;
    Inc(I);
  until (Value = 0);

  Result := S;
  Dec(I);
  repeat
    Result^ := Buffer[I];
    Inc(Result);
    Dec(I);
  until (I < 0);
end;

function AppendSigned(S: PWideChar; Value: NativeInt): PWideChar;
begin
  if (Value < 0) then
  begin
    S^ := '-';
    Inc(S);
    Value := -Value;
  end;

  Result := AppendUnsigned(S, Value);
end;

procedure ShowMessage(const AMessage, ATitle: PWideChar; const AErrorMode: Boolean);
{$ifdef MSWINDOWS}
const
  MB_ICONS: array[Boolean] of Integer = (MB_ICONINFORMATION, MB_ICONERROR);
{$endif}
begin
  {$ifNdef EMBEDDED}
  if (IsConsole) then
  {$endif}
  begin
    {$ifdef MSWINDOWS}
      WriteConsoleW(GetStdHandle(STD_ERROR_HANDLE), ATitle, WStrLen(ATitle), DWORD(nil^), nil);
      WriteConsoleW(GetStdHandle(STD_ERROR_HANDLE), LINE_BREAK, WStrLen(LINE_BREAK), DWORD(nil^), nil);
      WriteConsoleW(GetStdHandle(STD_ERROR_HANDLE), AMessage, WStrLen(AMessage), DWORD(nil^), nil);
    {$else}
      Writeln(ATitle);
      Write(AMessage);
    {$endif}
    Exit;
  end;

  {$ifdef MSWINDOWS}
  MessageBoxW(0, AMessage, ATitle, MB_OK or MB_ICONS[AErrorMode] or MB_TASKMODAL);
  {$endif}
end;


const
  BRAINMM_OPTIONS_VERSION = 1;
  BrainMMOptions: TBrainMMOptions = (
    RecordSize: SizeOf(TBrainMMOptions);
    Version: BRAINMM_OPTIONS_VERSION;
    FreePacalCompiler: {$ifdef FPC}True{$else}False{$endif};
    Redirect: {$ifdef BRAINMM_REDIRECT}True{$else}False{$endif};
    ReportMemoryLeaksOnShutdown: @{$ifdef MANAGERFLAGSEMULATE}BrainMM{$else}System{$endif}.ReportMemoryLeaksOnShutdown;
  );

var
  BrainMMRegistered: ^TBrainMemoryManager;
  BrainMMFreeMemOriginal: function(P: Pointer): Integer;
  {$ifdef MSWINDOWS}
  BrainMMRegisteredHandle: THandle;
  {$endif}

function BrainMMFreeMemInverter(P: Pointer): Integer;
begin
  Result := BrainMMFreeMemOriginal(P);
  Result := Byte(Result = 0);
end;

function BrainMMGetMemoryOptionsEmulate(const P: Pointer; var Options: TMemoryOptions): Boolean;
begin
  Result := False;
end;

function BrainMMThreadHeapMinimizeEmulate: Boolean;
begin
  Result := False;
end;


{$ifdef MSWINDOWS}
type
  TJumpInfo = packed record
    CodeOffset: Cardinal;
    Jump: {P}Pointer;
    JumpOffset: Cardinal;
  end;

procedure PatchCode(const AddrProc: Pointer; const CodeSize: NativeUInt;
  const Code: Pointer);
var
  OldProtect: Cardinal;
begin
  VirtualProtect(AddrProc, CodeSize, PAGE_EXECUTE_READWRITE, OldProtect);
  Move(Code^, AddrProc^, CodeSize);
  VirtualProtect(AddrProc, CodeSize, OldProtect, OldProtect);
end;

procedure PatchRedirect(const AddrProc: Pointer; const CodeSize: NativeUInt;
  const Code: Pointer; const Jumps: array of TJumpInfo);
var
  i: Integer;
  Buffer: array[0..33] of Byte;
begin
  Move(Code^, Buffer, CodeSize);

  for i := Low(Jumps) to High(Jumps) do
  with Jumps[i] do
  begin
    PInteger(@Buffer[CodeOffset])^ := NativeInt(PPointer(Jump)^) -
      (NativeInt(AddrProc) + NativeInt(CodeOffset) + 4) + NativeInt(JumpOffset);
  end;

  PatchCode(AddrProc, CodeSize, @Buffer);
end;
{$endif}

{$ifdef BRAINMM_REDIRECT}
// Size: 14/17
// BrainMMGetMem: 7/9
function RedirectGetMem(Size: NativeInt): Pointer;
asm
  {$ifdef CPUX86}
    mov ecx, [esp]
    test eax, eax
  {$else .CPUX64}
    mov r8, [rsp]
    test rcx, rcx
  {$endif}
  jg BrainMMGetMem // + 4
  {$ifdef CPUX86}
    xor eax, eax
  {$else .CPUX64}
    xor rax, rax
  {$endif}
end;

// Size: 14/17
// BrainMMAllocMem: 7/9
function RedirectAllocMem(Size: NativeInt): Pointer;
asm
  {$ifdef CPUX86}
    mov ecx, [esp]
    test eax, eax
  {$else .CPUX64}
    mov r8, [rsp]
    test rcx, rcx
  {$endif}
  jg BrainMMAllocMem // + 4
  {$ifdef CPUX86}
    xor eax, eax
  {$else .CPUX64}
    xor rax, rax
  {$endif}
end;

// Size: 17/19
// BrainMMFreeMem: 7/9
function RedirectFreeMem(P: Pointer): Integer;
asm
  {$ifdef CPUX86}
    mov ecx, [esp]
    test eax, eax
  {$else .CPUX64}
    mov r8, [rsp]
    test rcx, rcx
  {$endif}
  jnz BrainMMFreeMem // + 4
  mov eax, FREEMEM_DONE
end;

// Size: 30/33
// RecallFreeMem: 8/11
// RecallGetMem: 16/20
// BrainMMReallocMem(BrainMMRegetMem): 26/29
function RedirectReallocRegetMem(var P: Pointer; NewSize: NativeInt): Pointer;
asm
  // Value := P
  // if (NewSize <= 0) then Exit RecallFreeMem(Value, 0, P)
  {$ifdef CPUX86}
    mov ecx, eax
    mov eax, [eax]
    test edx, edx
  {$else .CPUX64}
    mov r8, rcx
    mov rcx, [rcx]
    test rdx, rdx
  {$endif}
  jle RecallFreeMem

  // if (Value = nil) Exit RecallGetMem(nil, NewSize, P)
  {$ifdef CPUX86}
    test eax, eax
  {$else .CPUX64}
    test rcx, rcx
  {$endif}
  jz RecallGetMem

  // Exit BrainMMReallocMem+8(Value, NewSize, P, ReturnAddress)
  {$ifdef CPUX86}
    push ebx
    mov ebx, [esp + 4]
  {$else .CPUX64}
    mov r9, [rsp]
  {$endif}
  jmp BrainMMReallocMem // + 8
end;

function AddrGetMem: Pointer;
asm
  {$ifdef CPUX86}
    lea eax, System.@GetMem
  {$else .CPUX64}
    lea rax, System.@GetMem
  {$endif}
end;

function AddrAllocMem: Pointer;
asm
  {$ifdef MEMORYMANAGEREX}
    {$ifdef CPUX86}
      lea eax, System.AllocMem
    {$else .CPUX64}
      lea rax, System.AllocMem
    {$endif}
  {$else}
    {$ifdef CPUX86}
      lea eax, BrainMM.AllocMem
    {$else .CPUX64}
      lea rax, BrainMM.AllocMem
    {$endif}
  {$endif}
end;

function AddrFreeMem: Pointer;
asm
  {$ifdef CPUX86}
    lea eax, System.@FreeMem
  {$else .CPUX64}
    lea rax, System.@FreeMem
  {$endif}
end;

function AddrReallocMem: Pointer;
asm
  {$ifdef CPUX86}
    lea eax, System.@ReallocMem
  {$else .CPUX64}
    lea rax, System.@ReallocMem
  {$endif}
end;

procedure BrainMMRedirectInitialize;
const
  PRecallFreeMem: Pointer = @RecallFreeMem;
  PRecallGetMem: Pointer = @RecallGetMem;

  JUMPS_GETMEM: array[0..0] of TJumpInfo = (
    ( CodeOffset: {$ifdef CPUX86}7{$else .CPUX64}9{$endif};
      Jump: @@MemoryManager.Standard.GetMem; JumpOffset: 4)
  );
  JUMPS_ALLOCMEM: array[0..0] of TJumpInfo = (
    ( CodeOffset: {$ifdef CPUX86}7{$else .CPUX64}9{$endif};
      Jump: @@MemoryManager.Standard.AllocMem; JumpOffset: 4)
  );
  JUMPS_FREEMEM: array[0..0] of TJumpInfo = (
    ( CodeOffset: {$ifdef CPUX86}7{$else .CPUX64}9{$endif};
      Jump: @@MemoryManager.Standard.FreeMem; JumpOffset: 4)
  );
  JUMPS_REALLOCMEM: array[0..2] of TJumpInfo = (
    ( CodeOffset: {$ifdef CPUX86}8{$else .CPUX64}11{$endif};
      Jump: @PRecallFreeMem; JumpOffset: 0),
    ( CodeOffset: {$ifdef CPUX86}16{$else .CPUX64}20{$endif};
      Jump: @PRecallGetMem; JumpOffset: 0),
    ( CodeOffset: {$ifdef CPUX86}26{$else .CPUX64}29{$endif};
      Jump: @@MemoryManager.Standard.ReallocMem; JumpOffset: 8)
  );
  JUMPS_REGETMEM: array[0..2] of TJumpInfo = (
    ( CodeOffset: {$ifdef CPUX86}8{$else .CPUX64}11{$endif};
      Jump: @PRecallFreeMem; JumpOffset: 0),
    ( CodeOffset: {$ifdef CPUX86}16{$else .CPUX64}20{$endif};
      Jump: @PRecallGetMem; JumpOffset: 0),
    ( CodeOffset: {$ifdef CPUX86}26{$else .CPUX64}29{$endif};
      Jump: @@MemoryManager.BrainMM.RegetMem; JumpOffset: 8)
  );
begin
  PatchRedirect(AddrGetMem, {$ifdef CPUX86}14{$else .CPUX64}17{$endif},
    @RedirectGetMem, JUMPS_GETMEM);
  PatchRedirect(AddrAllocMem, {$ifdef CPUX86}14{$else .CPUX64}17{$endif},
    @RedirectAllocMem, JUMPS_ALLOCMEM);
  if (BrainMMRegistered = nil) or (
   (BrainMMRegistered.BrainMM.Options.Version = BRAINMM_OPTIONS_VERSION) and
   (BrainMMRegistered.BrainMM.Options.FreePacalCompiler = {$ifdef FPC}True{$else}False{$endif})) then
  begin
    PatchRedirect(AddrFreeMem, {$ifdef CPUX86}17{$else .CPUX64}19{$endif},
      @RedirectFreeMem, JUMPS_FREEMEM);
  end;
  PatchRedirect(AddrReallocMem, {$ifdef CPUX86}30{$else .CPUX64}33{$endif},
    @RedirectReallocRegetMem, JUMPS_REALLOCMEM);
  PatchRedirect(@RegetMem, {$ifdef CPUX86}30{$else .CPUX64}33{$endif},
    @RedirectReallocRegetMem, JUMPS_REGETMEM);
end;
{$endif}

procedure BrainMMInitialize;
{$ifdef THREAD_FUNCS_EMULATE}
const
  P__BeginThread: Pointer = @__BeginThread;
  P__EndThread: Pointer = @__EndThread;
  JUMP_BYTES: array[1..5] of Byte = ($E9, 0, 0, 0, 0);
  JUMP_BEGIN_THREAD: array[0..0] of TJumpInfo = (
    (CodeOffset: 1; Jump: @P__BeginThread; JumpOffset: 0)
  );
  JUMP_END_THREAD: array[0..0] of TJumpInfo = (
    (CodeOffset: 1; Jump: @P__EndThread; JumpOffset: 0)
  );
{$endif}
{$ifdef BRAINMM_REDIRECT}{$ifdef CPUX86}
const
  NOP_9BYTES: array[1..9] of Byte = ($66, $0F, $1F, $84, $00, $00, $00, $00, $00);
{$endif}{$endif}
type
  PMemoryMgr = {$ifdef MEMORYMANAGEREX}^TMemoryManagerEx{$else}^TMemoryManager{$endif};
{$ifdef MSWINDOWS}
const
  BRAINMM_MARKER: array[1..18] of WideChar = ('L','o','c','a','l','\','B','r','a','i','n','M','M','_','P','I','D','_');
  HEX_CHARS: array[0..15] of WideChar = ('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');
var
  i: Integer;
  ProcessId: Cardinal;
  Buffer: array[0..SizeOf(BRAINMM_MARKER) + 8] of WideChar;
  MapAddress: Pointer;
{$endif}
begin
  {$ifdef MSWINDOWS}
  begin
    Move(BRAINMM_MARKER, Buffer, SizeOf(BRAINMM_MARKER));
    ProcessId := GetCurrentProcessId;
    for i := 0 to 7 do
    begin
      Buffer[High(Buffer) - 1 - i] := HEX_CHARS[ProcessId and $f];
      ProcessId := ProcessId shr 4;
    end;
    Buffer[High(Buffer)] := #0;

    BrainMMRegisteredHandle := OpenFileMappingW(FILE_MAP_READ, False, Buffer);
    if (BrainMMRegisteredHandle = 0) then
    begin
      BrainMMRegisteredHandle := CreateFileMappingW(INVALID_HANDLE_VALUE, nil, PAGE_READWRITE, 0, SizeOf(Pointer), Buffer);
      MapAddress := MapViewOfFile(BrainMMRegisteredHandle, FILE_MAP_WRITE, 0, 0, 0);
      PPointer(MapAddress)^ := @MemoryManager;
      UnmapViewOfFile(MapAddress);
    end else
    begin
      MapAddress := MapViewOfFile(BrainMMRegisteredHandle, FILE_MAP_READ, 0, 0, 0);
      BrainMMRegistered := PPointer(MapAddress)^;
      UnmapViewOfFile(MapAddress);
      CloseHandle(BrainMMRegisteredHandle);
      BrainMMRegisteredHandle := 0;
    end;
  end;
  {$else .POSIX}
  begin
    // ToDo
  end;
  {$endif}

  if (not Assigned(BrainMMRegistered)) then
  begin
    InitializeMediumIndexes;
    InitializeMediumOffsets;

    MemoryManager.BrainMM.Options := @BrainMMOptions;
    MemoryManager.BrainMM.ThreadFuncEvent := BrainMMThreadFuncEvent;
    MemoryManager.BrainMM.EndThreadEvent := BrainMMEndThreadEvent;
    MemoryManager.BrainMM.GetMemoryBlock := BrainMMGetMemoryBlock;
    MemoryManager.BrainMM.FreeMemoryBlock := BrainMMFreeMemoryBlock;
    MemoryManager.BrainMM.GetMemoryPages := BrainMMGetMemoryPages;
    MemoryManager.BrainMM.FreeMemoryPages := BrainMMFreeMemoryPages;
    MemoryManager.BrainMM.ResizeMemoryPages := BrainMMResizeMemoryPages;
    MemoryManager.BrainMM.GetMemoryOptions := BrainMMGetMemoryOptions;
    MemoryManager.BrainMM.ThreadHeapMinimize := BrainMMThreadHeapMinimize;
    MemoryManager.BrainMM.GetMemAligned := BrainMMGetMemAligned;
    MemoryManager.BrainMM.RegetMem := BrainMMRegetMem;
    MemoryManager.Standard.GetMem := BrainMMGetMem;
    MemoryManager.Standard.FreeMem := BrainMMFreeMem;
    MemoryManager.Standard.ReallocMem := BrainMMReallocMem;
    MemoryManager.Standard.AllocMem := BrainMMAllocMem;
    MemoryManager.Standard.RegisterExpectedMemoryLeak := BrainMMRegisterExpectedMemoryLeak;
    MemoryManager.Standard.UnregisterExpectedMemoryLeak := BrainMMUnregisterExpectedMemoryLeak;

    UnknownThreadHeap := CreateThreadHeap(False);
    UnknownThreadHeap.ThreadId := 0;
    UnknownThreadHeap.LockFlags := THREAD_HEAP_LOCKABLE;
    {$ifdef PUREPASCAL}
      MainThreadHeap := CreateThreadHeap(True);
    {$else}
      MainThreadHeap := ThreadHeapInstance;
    {$endif}

    {$ifdef BRAINMM_REDIRECT}
      BrainMMRedirectInitialize;
    {$endif}
  end else
  begin
    MemoryManager := BrainMMRegistered^;

    if (BrainMMRegistered.BrainMM.Options.FreePacalCompiler <> {$ifdef FPC}True{$else}False{$endif}) then
    begin
      BrainMMFreeMemOriginal := MemoryManager.Standard.FreeMem;
      MemoryManager.Standard.FreeMem := BrainMMFreeMemInverter;
    end;

    if (not Assigned(BrainMMRegistered.BrainMM.GetMemoryOptions)) then
    begin
      MemoryManager.BrainMM.GetMemoryOptions := BrainMMGetMemoryOptionsEmulate;
    end;

    if (not Assigned(BrainMMRegistered.BrainMM.ThreadHeapMinimize)) then
    begin
      MemoryManager.BrainMM.ThreadHeapMinimize := BrainMMThreadHeapMinimizeEmulate;
    end;

    {$ifdef BRAINMM_REDIRECT}
    if (BrainMMRegistered.BrainMM.Options.Version = BRAINMM_OPTIONS_VERSION) and
      (not BrainMMRegistered.BrainMM.Options.PurePascal) then
      BrainMMRedirectInitialize;
    {$endif}
  end;

  {$ifdef MSWINDOWS}
    {$ifdef THREAD_FUNCS_EMULATE}
      PatchRedirect(@System.BeginThread, SizeOf(JUMP_BYTES), @JUMP_BYTES, JUMP_BEGIN_THREAD);
      PatchRedirect(@System.EndThread, SizeOf(JUMP_BYTES), @JUMP_BYTES, JUMP_END_THREAD);
    {$endif}
    SystemThreadFuncProc := MemoryManager.BrainMM.ThreadFuncEvent;
    SystemThreadEndProc := MemoryManager.BrainMM.EndThreadEvent;
  {$else .POSIX}
    BeginThreadProc := MemoryManager.BrainMM.ThreadFuncEvent;
    EndThreadProc := MemoryManager.BrainMM.EndThreadEvent;
  {$endif}

  {$ifdef BRAINMM_REDIRECT}{$ifdef CPUX86}
  if (SSE_SUPPORT <> 0) then
  begin
    PatchCode(@BackwardSSEMove, SizeOf(NOP_9BYTES), @NOP_9BYTES);
    PatchCode(@NcMoveB16, SizeOf(NOP_9BYTES), @NOP_9BYTES);
    PatchCode(@NcMoveB16Small, SizeOf(NOP_9BYTES), @NOP_9BYTES);
  end;
  {$endif}{$endif}

  SetMemoryManager(PMemoryMgr(@MemoryManager.Standard)^);
end;


procedure BrainMMFinalize;
var
  GlobalStorage: PGlobalStorage;
  Text: PWideChar;
  Buffer: array[0..TEXT_BUFFER_SIZE - 1] of WideChar;
  PagesAllocated, ThreadsCount: NativeUInt;
begin
  {$ifdef MSWINDOWS}
  if (BrainMMRegisteredHandle <> 0) then
    CloseHandle(BrainMMRegisteredHandle);
  {$endif}

  if (not ReportMemoryLeaksOnShutdown) then Exit;
  if (Assigned(BrainMMRegistered)) then
  begin
    BrainMMRegistered.BrainMM.Options.ReportMemoryLeaksOnShutdown^ := True;
    Exit;
  end;

  GlobalStorage := Pointer(NativeInt(@GLOBAL_STORAGE[63]) and MASK_64_CLEAR);
  if (GlobalStorage.PagesAllocated = 0) then Exit;
  BrainMMThreadHeapMinimize;
  PagesAllocated := GlobalStorage.PagesAllocated;
  ThreadsCount := GlobalStorage.ThreadsCount;
  if (PagesAllocated = 0) then Exit;

  Text := @Buffer[0];
  if (ThreadsCount <> 0) then
  begin
    // unterminated threads, compact information
    Text := AppendString(Text, 'Unterminated threads: ');
    Text := AppendUnsigned(Text, ThreadsCount);
    Text := AppendString(Text, LINE_BREAK);
    Text := AppendString(Text, 'Leaked system memory size: ');
    Text := AppendUnsigned(Text, PagesAllocated * (SIZE_K4 div SIZE_K1));
    Text := AppendString(Text, 'Kb');
  end else
  begin
    // detailed leaked memory information
    // todo

    Text := AppendString(Text, 'Leaked system memory size: ');
    Text := AppendUnsigned(Text, PagesAllocated * (SIZE_K4 div SIZE_K1));
    Text := AppendString(Text, 'Kb');
  end;

  Text^ := #0;
  ShowMessage(@Buffer[0], 'Unexpected Memory Leak', True);
end;


{$ifdef BRAINMM_FIXFINALIZES}
procedure BrainMMFixFinalizes;
var
  i: NativeInt;
  InitProc: Pointer;
  LibModule: PLibModule;
  Info: PackageInfo;
  OldProtect: Cardinal;
  Temp: PackageUnitEntry;
begin
  InitProc := Pointer(NativeUInt(ReturnAddress) - {$ifdef CPUX86}24{$else .CPUX64}37{$endif});

  LibModule := LibModuleList;
  if (Assigned(LibModule)) then
  begin
    while Assigned(LibModule.Next) do
      LibModule := LibModule.Next;

    if Assigned(LibModule.TypeInfo) then
    begin
      Info := PackageInfo(PPackageTypeInfo(NativeUInt(LibModule.TypeInfo) -
        NativeUInt(@PackageInfoTable(nil^).TypeInfo)));

      for i := 0 to Info.UnitCount - 1 do
      if (NativeUInt(NativeUInt(InitProc) - NativeUInt(Info.UnitInfo[i].Init) + 4) < 8) then
      begin
        if (i <> 0) then
        begin
          VirtualProtect(Info.UnitInfo, (i + 1) * SizeOf(PackageUnitEntry), PAGE_EXECUTE_READWRITE, OldProtect);
          begin
            Temp := Info.UnitInfo[i];
            Move(Info.UnitInfo[0], Info.UnitInfo[1], (i - 1 + 1) * SizeOf(PackageUnitEntry));
            Info.UnitInfo[0] := Temp;
          end;
          VirtualProtect(Info.UnitInfo, (i + 1) * SizeOf(PackageUnitEntry), OldProtect, OldProtect);
        end;
        Exit;
      end;

      // unit not found
      {$ifdef CONDITIONALEXPRESSIONS}System.Error(reInvalidCast){$else}System.RunError(219){$endif};
    end;
  end;
end;
{$endif}


initialization
  {$ifdef BRAINMM_FIXFINALIZES}
    BrainMMFixFinalizes({@initialization});
  {$endif}

  {$ifdef CPUX86}
    CheckSSESupport;
  {$endif}
  {$ifdef MSWINDOWS}
    @SwitchToThreadFunc := GetProcAddress(LoadLibrary(kernel32), 'SwitchToThread');
  {$endif}
  BrainMMInitialize;

finalization
  BrainMMFinalize;

end.
