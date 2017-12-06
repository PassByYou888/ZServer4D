(*

Fast Memory Manager 4.20

Description:
 A fast replacement memory manager for Borland Delphi Win32 applications that
 scales well under multi-threaded usage, is not prone to memory fragmentation,
 and supports shared memory without the use of external .DLL files.

Homepage:
 http://fastmm.sourceforge.net

Advantages:
 - Fast
 - Low overhead. FastMM is designed for an average of 5% and maximum of 10%
   overhead per block.
 - Supports up to 3GB of user mode address space under Windows 32-bit and 4GB
   under Windows 64-bit. Add the "$SetPEFlags $20" option (in curly braces)
   to your .dpr to enable this.
 - Highly aligned memory blocks. Can be configured for either 8-byte or 16-byte
   alignment.
 - Good scaling under multi-threaded applications
 - Intelligent reallocations. Avoids slow memory move operations through
   not performing unneccesary downsizes and by having a minimum percentage
   block size growth factor when an in-place block upsize is not possible.
 - Resistant to address space fragmentation
 - No external DLL required when sharing memory between the application and
   external libraries (provided both use this memory manager)
 - Optionally reports memory leaks on program shutdown. (This check can be set
   to be performed only if Delphi is currently running on the machine, so end
   users won't be bothered by the error message.)
 - Supports Delphi 5 (or later), C++ Builder 6 (or later), Kylix 3.

Usage:
 Delphi:
  Place this unit as the very first unit under the "uses" section in your
  project's .dpr file. When sharing memory between an application and a DLL
  (e.g. when passing a long string or dynamic array to a DLL function), both the
  main application and the DLL must be compiled using this memory manager with
  the required conditional defines set. The are some conditional defines
  (inside FastMM4Options.inc) which may be used to tweak the memory manager. To
  enable support for a user mode address space greater than 2GB you will have to
  use the EditBin* tool to set the LARGE_ADDRESS_AWARE flag in the EXE header.
  This informs Windows x64 or Windows 32-bit (with the /3GB option set) that the
  application supports an address space larger than 2GB (up to 4GB). In Delphi 7
  and later you can also specify this flag through the compiler directive
  {$SetPEFlags $20}
  *The EditBin tool ships with the MS Visual C compiler.
 C++ Builder 6:
  Refer to the instructions inside FastMM4BCB.cpp.

License:
 This work is copyright Professional Software Development / Pierre le Riche. It
 is released under the Mozilla Public Licence 1.1 (MPL 1.1). The licence terms
 are described on this page: http://www.mozilla.org/MPL/MPL-1.1.html. If you
 find FastMM useful or you would like to support further development, a donation
 would be much appreciated. My banking details are:
   Country: South Africa
   Bank: ABSA Bank Ltd
   Branch: Somerset West
   Branch Code: 334-712
   Account Name: PSD (Distribution)
   Account No.: 4041827693
   Swift Code: ABSAZAJJ
 My PayPal account is:
   bof@psd.co.za

Contact Details:
 Below are my contact details if you would like to get in touch with me. If you
 use this memory manager I would like to hear from you: please e-mail me your
 comments - good and bad.
 Snailmail:
   PO Box 2514
   Somerset West
   7129
   South Africa
 E-mail:
   plr@psd.co.za

Support:
 If you have trouble using FastMM, you are welcome to drop me an e-mail at the
 address above, or you may post your questions in the BASM newsgroup on the
 Borland news server (which is where I hang out quite frequently).

Disclaimer:
 FastMM has been tested extensively with both single and muiltithreaded
 applications on various hardware platforms, but unfortunately I am not in a
 position to make any guarantees. Use it at your own risk.

Acknowledgements:
 - Eric Grange for his RecyclerMM on which the earlier versions of FastMM were
   based. RecyclerMM was what inspired me to try and write my own memory
   manager back in early 2004.
 - Pierre Y. for his suggestions regarding the extension of the memory leak
   checking options.
 - Anders Isaksson and Greg for finding and identifying the "DelphiIsRunning"
   bug under Delphi 5.
 - Francois Malan for finding the bug that caused compilation to fail when
   both the "AssumeMultiThreaded" and "CheckHeapForCorruption" options were set.
 - Craig Peterson for helping me identify the cache associativity issues that
   could arise due to medium blocks always being an exact multiple of 256 bytes.
   Also for various other bug reports and enhancement suggestions.
 - Jarek Karciarz, Vladimir Ulchenko (Vavan) and Bob Gonder for their help in
   implementing the BCB support.
 - Ben Taylor for his suggestion to display the object class of all memory
   leaks.
 - Jean Marc Eber and Vincent Mahon (the Memcheck guys) for the call stack
   trace code and also the method used to catch virtual method calls on freed
   objects.
 - Nahan Hyn for the suggestion to be able to enable or disable memory leak
   reporting through a global variable (the "ManualLeakReportingControl"
   option.)
 - Leonel Togniolli for various suggestions with regard to enhancing the bug
   tracking features of FastMM and other helpful advice.
 - Joe Bain and Leonel Togniolli for the workaround to QC#10922 affecting
   compilation under Delphi 2005.
 - Robert Marquardt for the suggestion to make localisation of FastMM easier by
   having all string constants together.
 - Simon Kissel and Fikret Hasovic for their help in implementing Kylix support.
 - Matthias Thoma, Petr Vones, Robert Rossmair and the rest of the JCL crew for
   their debug info library used in the debug info support DLL.
 - Andreas Hausladen for the suggestion to use an external DLL to enable the
   reporting of debug information.
 - Alexander Tabakov for various good suggestions regarding the debugging
   facilities of FastMM.
 - M. Skloff for bringing to my attention some compiler warnings.
 - Martin Aignesberger for the code to use madExcept instead of the JCL library
   inside the debug info support DLL.
 - Diederik and Dennis Passmore for the suggestion to be able to register
   expected leaks.
 - Dario Tiraboschi and Mark Gebauer for pointing out the problems that occur
   when range checking and complete boolean evaluation is turned on.
 - Hanspeter Widmer for his suggestion to have an option to display install and
   uninstall debug messages and moving options to a separate file.
 - Arthur Hoornweg for notifying me of the image base being incorrect for
   borlndmm.dll.
 - Theo Carr-Brion and Hanspeter Widmer for finding the false alarm error
   message "Block Header Has Been Corrupted" bug in FullDebugMode.

Change log:
 Version 1.00 (28 June 2004):
  - First version (called PSDMemoryManager). Based on RecyclerMM (free block
    stack approach) by Eric Grange.
 Version 2.00 (3 November 2004):
  - Complete redesign and rewrite from scratch. Name changed to FastMM to
    reflect this fact. Uses a linked-list approach. Is faster, has less memory
    overhead, and will now catch most bad pointers on FreeMem calls.
 Version 3.00 (1 March 2005):
  - Another rewrite. Reduced the memory overhead by: (a) not having a separate
    memory area for the linked list of free blocks (uses space inside free
    blocks themselves) (b) batch managers are allocated as part of chunks (c)
    block size lookup table size reduced. This should make FastMM more CPU
    cache friendly.
 Version 4.00 (7 June 2005):
  - Yet another rewrite. FastMM4 is in fact three memory managers in one: Small
    blocks (up to a few KB) are managed through the binning model in the same
    way as previous versions, medium blocks (from a few KB up to approximately
    256K) are allocated in a linked-list fashion, and large blocks are grabbed
    directly from the system through VirtualAlloc. This 3-layered design allows
    very fast operation with the most frequently used block sizes (small
    blocks), while also minimizing fragmentation and imparting significant
    overhead savings with blocks larger than a few KB.
 Version 4.01 (8 June 2005):
  - Added the options "RequireDebugInfoForLeakReporting" and
    "RequireIDEPresenceForLeakReporting" as suggested by Pierre Y.
  - Fixed the "DelphiIsRunning" function not working under Delphi 5, and
    consequently no leak checking. (Reported by Anders Isaksson and Greg.)
 Version 4.02 (8 June 2005):
  - Fixed the compilation error when both the "AssumeMultiThreaded" and
    "CheckHeapForCorruption options were set. (Reported by Francois Malan.)
 Version 4.03 (9 June 2005):
  - Added descriptive error messages when FastMM4 cannot be installed because
    another MM has already been installed or memory has already been allocated.
 Version 4.04 (13 June 2005):
  - Added a small fixed offset to the size of medium blocks (previously always
    exact multiples of 256 bytes). This makes performance problems due to CPU
    cache associativity limitations much less likely. (Reported by Craig
    Peterson.)
 Version 4.05 (17 June 2005):
  - Added the Align16Bytes option. Disable this option to drop the 16 byte
    alignment restriction and reduce alignment to 8 bytes for the smallest
    block sizes. Disabling Align16Bytes should lower memory consumption at the
    cost of complicating the use of aligned SSE move instructions. (Suggested
    by Craig Peterson.)
  - Added a support unit for C++ Builder 6 - Add FastMM4BCB.cpp and
    FastMM4.pas to your BCB project to use FastMM instead of the RTL MM. Memory
    leak checking is not supported because (unfortunately) once an MM is
    installed under BCB you cannot uninstall it... at least not without
    modifying the RTL code in exit.c or patching the RTL code runtime. (Thanks
    to Jarek Karciarz, Vladimir Ulchenko and Bob Gonder.)
 Version 4.06 (22 June 2005):
  - Displays the class of all leaked objects on the memory leak report and also
    tries to identify leaked long strings. Previously it only displayed the
    sizes of all leaked blocks. (Suggested by Ben Taylor.)
  - Added support for displaying the sizes of medium and large block memory
    leaks. Previously it only displayed details for small block leaks.
 Version 4.07 (22 June 2005):
  - Fixed the detection of the class of leaked objects not working under
    Windows 98/Me.
 Version 4.08 (27 June 2005):
  - Added a BorlndMM.dpr project to allow you to build a borlndmm.dll that uses
    FastMM4 instead of the default memory manager. You may replace the old
    DLL in the Delphi \Bin directory to make the IDE use this memory manager
    instead.
 Version 4.09 (30 June 2005):
  - Included a patch fix for the bug affecting replacement borlndmm.dll files
    with Delphi 2005 (QC#14007). Compile the patch, close Delphi, and run it
    once to patch your vclide90.bpl. You will now be able to use the
    replacement borlndmm.dll to speed up the Delphi 2005 IDE as well.
 Version 4.10 (7 July 2005):
  - Due to QC#14070 ("Delphi IDE attempts to free memory after the shutdown
    code of borlndmm.dll has been called"), FastMM cannot be uninstalled
    safely when used inside a replacement borlndmm.dll for the IDE. Added a
    conditional define "NeverUninstall" for this purpose.
  - Added the "FullDebugMode" option to pad all blocks with a header and footer
    to help you catch memory overwrite bugs in your applications. All blocks
    returned to freemem are also zeroed out to help catch bugs involving the
    use of previously freed blocks. Also catches attempts at calling virtual
    methods of freed objects provided the block in question has not been reused
    since the object was freed. Displays stack traces on error to aid debugging.
  - Added the "LogErrorsToFile" option to log all errors to a text file in the
    same folder as the application.
  - Added the "ManualLeakReportingControl" option (suggested by Nahan Hyn) to
    enable control over whether the memory leak report should be done or not
    via a global variable.
 Version 4.11 (7 July 2005):
  - Fixed a compilation error under Delphi 2005 due to QC#10922. (Thanks to Joe
    Bain and Leonel Togniolli.)
  - Fixed leaked object classes not displaying in the leak report in
    "FullDebugMode".
  Version 4.12 (8 July 2005):
  - Moved all the string constants to one place to make it easier to do
    translations into other languages. (Thanks to Robert Marquardt.)
  - Added support for Kylix. Some functionality is currently missing: No
    support for detecting the object class on leaks and also no MM sharing.
    (Thanks to Simon Kissel and Fikret Hasovic).
  Version 4.13 (11 July 2005):
  - Added the FastMM_DebugInfo.dll support library to display debug info for
    stack traces.
  - Stack traces for the memory leak report is now logged to the log file in
    "FullDebugMode".
  Version 4.14 (14 July 2005):
  - Fixed string leaks not being detected as such in "FullDebugMode". (Thanks
    to Leonel Togniolli.)
  - Fixed the compilation error in "FullDebugMode" when "LogErrorsToFile" is
    not set. (Thanks to Leonel Togniolli.)
  - Added a "Release" option to allow the grouping of various options and to
    make it easier to make debug and release builds. (Thanks to Alexander
    Tabakov.)
  - Added a "HideMemoryLeakHintMessage" option to not display the hint below
    the memory leak message. (Thanks to Alexander Tabakov.)
  - Changed the fill character for "FullDebugMode" from zero to $80 to be able
    to differentiate between invalid memory accesses using nil pointers to
    invalid memory accesses using fields of freed objects. FastMM tries to
    reserve the 64K block starting at $80800000 at startup to ensure that an
    A/V will occur when this block is accessed. (Thanks to Alexander Tabakov.)
  - Fixed some compiler warnings. (Thanks to M. Skloff)
  - Fixed some display bugs in the memory leak report. (Thanks to Leonel
    Togniolli.)
  - Added a "LogMemoryLeakDetailToFile" option. Some applications leak a lot of
    memory and can make the log file grow very large very quickly.
  - Added the option to use madExcept instead of the JCL Debug library in the
    debug info support DLL. (Thanks to Martin Aignesberger.)
  - Added procedures "GetMemoryManagerState" and "GetMemoryMap" to retrieve
    statistics about the current state of the memory manager and memory pool.
    (A usage tracker form together with a demo is also available.)
  Version 4.15 (14 July 2005):
  - Fixed a false 4GB(!) memory leak reported in some instances.
  Version 4.16 (15 July 2005):
  - Added the "CatchUseOfFreedInterfaces" option to catch the use of interfaces
    of freed objects. This option is not compatible with checking that a freed
    block has not been modified, so enable this option only when hunting an
    invalid interface reference. (Only relevant if "FullDebugMode" is set.)
  - During shutdown FastMM now checks that all free blocks have not been
    modified since being freed. (Only when "FullDebugMode" is set and
    "CatchUseOfFreedInterfaces" is disabled.)
  Version 4.17 (15 July 2005):
 - Added the AddExpectedMemoryLeaks and RemoveExpectedMemoryLeaks procedures to
   register/unregister expected leaks, thus preventing the leak report from
   displaying if only expected leaks occurred. (Thanks to Diederik and Dennis
   Passmore for the suggestion.)
 - Fixed the "LogMemoryLeakDetailToFile" not logging memory leak detail to file
   as it is supposed to. (Thanks to Leonel Togniolli.)
 Version 4.18 (18 July 2005):
 - Fixed some issues when range checking or complete boolean evaluation is
   switched on. (Thanks to Dario Tiraboschi and Mark Gebauer.)
 - Added the "ShowInstallUninstallDebugString" option to display a message when
   FastMM is installed or uninstalled. (Thanks to Hanspeter Widmer.)
 - Moved the options to a separate include file. (Thanks to Hanspeter Widmer.)
 - Moved message strings to a separate file for easy translation.
 Version 4.19 (19 July 2005):
 - Fixed Kylix support that was broken in 4.14.
 Version 4.20 (20 July 2005):
 - Fixed a false memory overwrite report at shutdown in "FullDebugMode". If you
   consistently got a "Block Header Has Been Corrupted" error message during
   shutdown at address $xxxx0070 then it was probably a false alarm. (Thanks to
   Theo Carr-Brion and Hanspeter Widmer.}

*)

unit FastMM4_16;

interface

{$Include FastMM4_16Options.inc}

{$RANGECHECKS OFF}
{$BOOLEVAL OFF}
{$OVERFLOWCHECKS OFF}
{$OPTIMIZATION ON}
{$TYPEDADDRESS OFF}

{Some features not currently supported under Kylix}
{$ifdef Linux}
  {$undef LogErrorsToFile}
  {$undef LogMemoryLeakDetailToFile}
  {$undef ShareMM}
  {$undef AttemptToUseSharedMM}
  {$undef RequireIDEPresenceForLeakReporting}
{$endif}

{Do we require debug info for leak checking?}
{$ifdef RequireDebugInfoForLeakReporting}
  {$ifopt D-}
    {$undef EnableMemoryLeakReporting}
  {$endif}
{$endif}

{Enable heap checking and leak reporting in full debug mode}
{$ifdef FullDebugMode}
  {$STACKFRAMES ON}
  {$define CheckHeapForCorruption}
  {$ifndef CatchUseOfFreedInterfaces}
    {$define CheckUseOfFreedBlocksOnShutdown}
  {$endif}
{$else}
  {Error logging requires FullDebugMode}
  {$undef LogMemoryLeakDetailToFile}
  {$undef CatchUseOfFreedInterfaces}
{$endif}

{Only the pascal version supports extended heap corruption checking.}
{$ifdef CheckHeapForCorruption}
  {$undef ASMVersion}
{$endif}

{Delphi versions}
{$ifndef BCB}
  {$ifdef ver130}
    {$define Delphi5}
  {$endif}
  {$ifdef ver140}
    {$define Delphi6}
  {$endif}
{$endif}

{$ifndef Delphi5}
  {$ifndef BCB}
    {$define Delphi6AndUp}
  {$endif}
  {$ifndef Delphi6}
    {$define BCB6OrDelphi7AndUp}
    {$ifndef BCB}
      {$define Delphi7AndUp}
    {$endif}
  {$endif}
{$endif}

{$ifdef Delphi6AndUp}
  {$WARN SYMBOL_PLATFORM OFF}
{$endif}

{$ifdef BCB}
  {Cannot uninstall safely under BCB}
  {$define NeverUninstall}
{$endif}

{Leak detail logging requires error logging}
{$ifndef LogErrorsToFile}
  {$undef LogMemoryLeakDetailToFile}
{$endif}

{Manual leak reporting control requires leak reporting to be enabled}
{$ifndef EnableMemoryLeakReporting}
  {$undef ManualLeakReportingControl}
{$endif}

{-------------------------Public constants-----------------------------}
const
  {The number of small block types}
{$ifdef Align16Bytes}
  NumSmallBlockTypes = 46;
{$else}
  NumSmallBlockTypes = 55;
{$endif}

{----------------------------Public types------------------------------}
type
  TSmallBlockTypeState = packed record
    {The internal size of the block type}
    InternalBlockSize: Cardinal;
    {Useable block size: The number of non-reserved bytes inside the block.}
    UseableBlockSize: Cardinal;
    {The number of allocated blocks}
    AllocatedBlockCount: Cardinal;
    {The total address space reserved for this block type (both allocated and
     free blocks)}
    ReservedAddressSpace: Cardinal;
  end;
  TSmallBlockTypeStates = array[0..NumSmallBlockTypes - 1] of TSmallBlockTypeState;

  TMemoryManagerState = packed record
    {Small block type states}
    SmallBlockTypeStates: TSmallBlockTypeStates;
    {Medium block stats}
    AllocatedMediumBlockCount: Cardinal;
    TotalAllocatedMediumBlockSize: Cardinal;
    ReservedMediumBlockAddressSpace: Cardinal;
    {Large block stats}
    AllocatedLargeBlockCount: Cardinal;
    TotalAllocatedLargeBlockSize: Cardinal;
    ReservedLargeBlockAddressSpace: Cardinal;
  end;

  {Memory map}
  TChunkStatus = (csUnallocated, csAllocated, csReserved,
    csSysAllocated, csSysReserved);
  TMemoryMap = array[0..65535] of TChunkStatus;

{--------------------------Public variables----------------------------}
{$ifdef ManualLeakReportingControl}
var
  ReportMemoryLeaks: Boolean = True;
{$endif}

{-------------------------Public procedures----------------------------}
{Installation procedures must be exposed for the BCB helper unit FastMM4BCB.cpp}
{$ifdef BCB}
function CheckCanInstallMemoryManager: boolean;
procedure InstallMemoryManager;
{$endif}

{$ifndef FullDebugMode}
{The standard memory manager functions}
function FastFreeMem(APointer: Pointer): Integer;
function FastGetMem(ASize: Integer): Pointer;
function FastReallocMem(APointer: Pointer; ANewSize: Integer): Pointer;
{$else}
{The FullDebugMode memory manager functions}
function DebugFreeMem(APointer: Pointer): Integer;
function DebugGetMem(ASize: Integer): Pointer;
function DebugReallocMem(APointer: Pointer; ANewSize: Integer): Pointer;
{$endif}

{Releases all allocated memory (use with extreme care)}
procedure FreeAllMemory;

{Returns statistics about the current state of the memory manager}
procedure GetMemoryManagerState(var AMemoryManagerState: TMemoryManagerState);
{$ifndef LINUX}
{Gets the state of every 64K block in the 4GB address space}
procedure GetMemoryMap(var AMemoryMap: TMemoryMap);
{$endif}

{$ifdef EnableMemoryLeakReporting}
{Registers expected memory leaks. Returns true on success. The list of leaked
 blocks is limited to 64K, so failure is possible if you have a ridiculous
 number of expected leaks.}
function AddExpectedMemoryLeaks(ALeakSize: Cardinal; ACount: Cardinal): Boolean;
{Removes expected memory leaks. Returns true on success.}
function RemoveExpectedMemoryLeaks(ALeakSize: Cardinal; ACount: Cardinal): Boolean;
{$endif}

implementation

uses
{$ifndef Linux}
  Windows,
{$else}
  Libc,
{$endif}
  FastMM4Messages;

{Fixed size move procedures}
procedure Move12(const ASource; var ADest; ACount: Integer); forward;
procedure Move20(const ASource; var ADest; ACount: Integer); forward;
procedure Move28(const ASource; var ADest; ACount: Integer); forward;
procedure Move36(const ASource; var ADest; ACount: Integer); forward;
procedure Move44(const ASource; var ADest; ACount: Integer); forward;
procedure Move52(const ASource; var ADest; ACount: Integer); forward;
procedure Move60(const ASource; var ADest; ACount: Integer); forward;
procedure Move68(const ASource; var ADest; ACount: Integer); forward;

{-------------------------Private constants----------------------------}
const
  {The size of a medium block pool. This is allocated through
   VirtualAlloc and is used to serve medium blocks. In Full Debug mode we leave
   a trailing 256 bytes to be able to safely do a memory dump.}
  MediumBlockPoolSize = 20 * 64 * 1024{$ifdef FullDebugMode} - 256{$endif};
  {The granularity of small blocks}
{$ifdef Align16Bytes}
  SmallBlockGranularity = 16;
{$else}
  SmallBlockGranularity = 8;
{$endif}
  {The granularity of medium blocks. Newly allocated medium blocks are
   a multiple of this size plus MediumBlockSizeOffset, to avoid cache line
   conflicts}
  MediumBlockGranularity = 256;
  MediumBlockSizeOffset = 48;
  {The granularity of large blocks}
  LargeBlockGranularity = 65536;
  {The maximum size of a small block. Blocks Larger than this are either
   medium or large blocks.}
  MaximumSmallBlockSize = 2608;
  {The smallest medium block size. (Medium blocks are rounded up to the nearest
   multiple of MediumBlockGranularity plus MediumBlockSizeOffset)}
  MinimumMediumBlockSize = 11 * 256 + MediumBlockSizeOffset;
  {The number of bins reserved for medium blocks}
  MediumBlockBinsPerGroup = 32;
  MediumBlockBinGroupCount = 32;
  MediumBlockBinCount = MediumBlockBinGroupCount * MediumBlockBinsPerGroup;
  {The maximum size allocatable through medium blocks. Blocks larger than this
   fall through to VirtualAlloc ( = large blocks).}
  MaximumMediumBlockSize = MinimumMediumBlockSize + (MediumBlockBinCount - 1) * MediumBlockGranularity;
  {The target number of small blocks per pool. The actual number of blocks per
   pool may be much greater for very small sizes and less for larger sizes. The
   cost of allocating the small block pool is amortized across all the small
   blocks in the pool, however the blocks may not all end up being used so they
   may be lying idle.}
  TargetSmallBlocksPerPool = 48;
  {The minimum number of small blocks per pool. Any available medium block must
   have space for roughly this many small blocks (or more) to be useable as a
   small block pool.}
  MinimumSmallBlocksPerPool = 12;
  {The lower and upper limits for the optimal small block pool size}
  OptimalSmallBlockPoolSizeLowerLimit = 29 * 1024 - MediumBlockGranularity + MediumBlockSizeOffset;
  OptimalSmallBlockPoolSizeUpperLimit = 64 * 1024 - MediumBlockGranularity + MediumBlockSizeOffset;
  {The maximum small block pool size. If a free block is this size or larger
   then it will be split.}
  MaximumSmallBlockPoolSize = OptimalSmallBlockPoolSizeUpperLimit + MinimumMediumBlockSize;
  {The signature value for a small block pool - to be able to distinguish
   between small block pools and ordinary medium blocks (used by the the
   GetMemoryManagerState procedure)}
  SmallBlockPoolSignatureValue = ord('S') + ord('M') shl 8 + ord('B') shl 16 + ord('P') shl 24;
  {-------------Block type flags--------------}
  {The lower 3 bits in the dword header of small blocks (4 bits in medium and
   large blocks) are used as flags to indicate the state of the block}
  {Set if the block is not in use}
  IsFreeBlockFlag = 1;
  {Set if this is a medium block}
  IsMediumBlockFlag = 2;
  {Set if it is a large block}
  IsLargeBlockFlag = 4;
  {Is the medium block preceding this block available?}
  PreviousMediumBlockIsFreeFlag = 8;
  {The flags masks for small blocks}
  DropSmallFlagsMask = -8;
  ExtractSmallFlagsMask = 7;
  {The flags masks for medium and large blocks}
  DropMediumAndLargeFlagsMask = -16;
  ExtractMediumAndLargeFlagsMask = 15;
  {-------------Block resizing constants---------------}
  SmallBlockDownsizeCheckAdder = 64;
  SmallBlockUpsizeAdder = 32;
  {-------------FullDebugMode constants---------------}
{$ifdef FullDebugMode}
  {The stack trace depth}
  StackTraceDepth = 9;
  {The number of fake VMT entries - used to track virtual method calls on
   freed objects. Do not change this value without also updating TFreedObject.GetVirtualMethodIndex}
  MaxFakeVMTEntries = 200;
  {The pattern used to fill unused memory}
  DebugFillByte = $80;
  DebugFillDWord = $01010101 * Cardinal(DebugFillByte);
  {The address that is reserved so that accesses to the address of the fill
   pattern will result in an A/V}
  DebugReservedAddress = $01010000 * Cardinal(DebugFillByte);
{$endif}
  {-------------Other constants---------------}
  {Sleep time when a resource (small/medium/large block manager) is in use}
  InitialSleepTime = 0;
  {Used when the resource is still in use after the first sleep}
  AdditionalSleepTime = 10;
  {Hexadecimal characters}
  HexTable: array[0..15] of char = '0123456789ABCDEF';
  {Copyright message - not used anywhere in the code}
  Copyright: string = 'FastMM4 © 2004, 2005 Pierre le Riche / Professional Software Development';

{-------------------------Private types----------------------------}
type

{$ifdef Delphi5}
  {Delphi 5 Compatibility}
  PCardinal = ^Cardinal;
  PPointer = ^Pointer;
{$endif}

  {Move procedure type}
  TMoveProc = procedure(const ASource; var ADest; ACount: Integer);

  {---------------Small block structures-------------}

  {Pointer to the header of a small block pool}
  PSmallBlockPoolHeader = ^TSmallBlockPoolHeader;

  {Small block type (Size = 32)}
  PSmallBlockType = ^TSmallBlockType;
  TSmallBlockType = packed record
    {True = Block type is locked}
    BlockTypeLocked: boolean;
    {Bitmap indicating which of the first 8 medium block groups contain blocks
     of a suitable size for a block pool.}
    AllowedGroupsForBlockPoolBitmap: byte;
    {The block size for this block type}
    BlockSize: Word;
    {The first partially free pool for the given small block type (offset = +4
     for typecast compatibility with TSmallBlockPoolHeader). This is a circular
     buffer.}
    NextPartiallyFreePool: PSmallBlockPoolHeader;
    {The offset of the last block that was served sequentially (0ffset = +8 to
     to be at the same offset as the "FirstFreeBlock" of TSmallBlockPoolHeader}
    NextSequentialFeedBlockAddress: Pointer;
    {The last block that can be served sequentially. Offset is at +12 to be
     at the same address as the "BlocksInUse" field of TSmallBlockPoolHeader}
    MaxSequentialFeedBlockAddress: Pointer;
    {The pool that is current being used to serve blocks in sequential order}
    CurrentSequentialFeedPool: PSmallBlockPoolHeader;
    {The previous partially free pool for the small block type (offset = +20
     for typecast compatibility with TSmallBlockPoolHeader)}
    PreviousPartiallyFreePool: PSmallBlockPoolHeader;
    {The minimum and optimal size of a small block pool for this block type}
    MinimumBlockPoolSize: Word;
    OptimalBlockPoolSize: Word;
{$ifdef UseCustomFixedSizeMoveRoutines}
    {The fixed size move procedure used to move data for this block size when
     it is upsized. When a block is downsized (which usually does not occur
     that often) the variable size move routine is used.}
    UpsizeMoveProcedure: TMoveProc;
{$else}
    Reserved1: Cardinal;
{$endif}
  end;

  {Small block pool (Size = 32 bytes)}
  TSmallBlockPoolHeader = packed record
    {BlockType}
    BlockType: PSmallBlockType;
    {The next pool that has free blocks of this size. Must be at offset +4
     to be typecast compatible with TSmallBlockType}
    NextPartiallyFreePool: PSmallBlockPoolHeader;
    {Pointer to the first free block inside this pool. Must be at offset + 8
     to be at the same offset as "NextSequentialFeedBlockAddress" of
     TSmallBlockType}
    FirstFreeBlock: Pointer;
    {The number of blocks allocated in this pool. Must be at offset + 12
     to be at the same offset as "MaxSequentialFeedBlockAddress" of
     TSmallBlockType}
    BlocksInUse: Cardinal;
    {Small block pool signature. Used by the leak checking mechanism to
     determine whether a medium block is a small block pool or a regular medium
     block.}
    SmallBlockPoolSignature: Cardinal;
    {The previous pool that has free blocks of this size. Must be at offset +20
     to be compatible with TSmallBlockType}
    PreviousPartiallyFreePool: PSmallBlockPoolHeader;
    {Reserved}
    Reserved1: Cardinal;
    {The pool pointer and flags of the first block}
    FirstBlockPoolPointerAndFlags: Cardinal;
  end;

  {Small block layout:
   Offset: -4 = Flags + address of the small block pool
   Offset: BlockSize - 4 = Flags + address of the small block pool for the next small block
  }

  {----------------------Medium block structures----------------------}

  {The medium block pool from which medium blocks are drawn}
  PMediumBlockPoolHeader = ^TMediumBlockPoolHeader;
  TMediumBlockPoolHeader = packed record
    {Points to the previous and next medium block pools. This circular linked
     list is used to track memory leaks on program shutdown.}
    PreviousMediumBlockPoolHeader: PMediumBlockPoolHeader;
    NextMediumBlockPoolHeader: PMediumBlockPoolHeader;
    {Unused dword}
    Reserved: Cardinal;
    {The block size and flags of the first medium block in the block pool}
    FirstMediumBlockSizeAndFlags: Cardinal;
  end;

  {Medium block layout:
   Offset: -8 = Previous Block Size (only if the previous block is free)
   Offset: -4 = This block size and flags
   Offset: 0 = User data / Previous Free Block (if this block is free)
   Offset: 4 = Next Free Block (if this block is free)
   Offset: BlockSize - 8 = Size of this block (if this block is free)
   Offset: BlockSize - 4 = Size of the next block and flags

  {A medium block that is unused}
  PMediumFreeBlock = ^TMediumFreeBlock;
  TMediumFreeBlock = packed record
    PreviousFreeBlock: PMediumFreeBlock;
    NextFreeBlock: PMediumFreeBlock;
  end;

  {-------------------------Large block structures--------------------}

  {Large block header record (size = 16)}
  PLargeBlockHeader = ^TLargeBlockHeader;
  TLargeBlockHeader = packed record
    {Points to the previous and next large blocks. This circular linked
     list is used to track memory leaks on program shutdown.}
    PreviousLargeBlockHeader: PLargeBlockHeader;
    NextLargeBlockHeader: PLargeBlockHeader;
    {The user allocated size of the Large block}
    UserAllocatedSize: Cardinal;
    {The size of this block plus the flags}
    BlockSizeAndFlags: Cardinal;
  end;

  {-------------------------Expected Memory Leak Structures--------------------}
{$ifdef EnableMemoryLeakReporting}

  TExpectedMemoryLeak = packed record
    LeakSize: Cardinal;
    LeakCount: Cardinal;
  end;

  TExpectedMemoryLeaks = packed record
    {The number of entries registered}
    NumEntries: Cardinal;
    {The array of leaks}
    ExpectedLeaks: array[0..(65536 - 4) div SizeOf(TExpectedMemoryLeak) - 1] of TExpectedMemoryLeak;
  end;
  PExpectedMemoryLeaks = ^TExpectedMemoryLeaks;

{$endif}

  {-------------------------Full Debug Mode Structures--------------------}
{$ifdef FullDebugMode}

  PStackTrace = ^TStackTrace;
  TStackTrace = array[0..StackTraceDepth - 1] of Cardinal;

  TBlockOperation = (boBlockCheckOnShutdown, boGetMem, boFreeMem, boReallocMem);

  PFullDebugBlockHeader = ^TFullDebugBlockHeader;
  TFullDebugBlockHeader = packed record
    {Space used by the medium block manager for previous/next block management.
     If a medium block is binned then these two dwords will be modified.}
    Reserved1: Cardinal;
    Reserved2: Cardinal;
    {Is the block currently allocated?}
    BlockInUse: LongBool;
    {The call stack when the block was allocated}
    AllocationStackTrace: TStackTrace;
    {The call stack when the block was freed}
    FreeStackTrace: TStackTrace;
    {The user requested size for the block. 0 if this is the first time the
     block is used.}
    UserSize: Cardinal;
    {The object class this block was used for the previous time it was
     allocated. When a block is freed, the dword that would normally be in the
     space of the class pointer is copied here, so if it is detected that
     the block was used after being freed we have an idea what class it is.}
    PreviouslyUsedByClass: Cardinal;
    {The sum of all the dwords excluding reserved dwords.}
    HeaderCheckSum: Cardinal;
  end;
  {The last four bytes of the actual allocated block is the inverse of the
   header checksum}

  {The class used to catch attempts to execute a virtual method of a freed
   object}
  TFreedObject = class
  public
    procedure GetVirtualMethodIndex;
    procedure VirtualMethodError;
{$ifdef CatchUseOfFreedInterfaces}
    procedure InterfaceError;
{$endif}
  end;

  {The exported procedure in the FastMM_DebugInfo.dll library used to obtain
   detailed debug info for stack traces, etc.}
  TDebugInfoProc = procedure(AAddress: Pointer; APDebugInfo: PChar;
    var ANumChars: integer);

{$endif}

{-------------------------Private constants----------------------------}
const
{$ifndef BCB6OrDelphi7AndUp}
  reInvalidPtr = 2;
{$endif}
  {The size of the block header in front of small and medium blocks}
  BlockHeaderSize = 4;
  {The size of a small block pool header}
  SmallBlockPoolHeaderSize = SizeOf(TSmallBlockPoolHeader);
  {The size of a medium block pool header}
  MediumBlockPoolHeaderSize = SizeOf(TMediumBlockPoolHeader);
  {The size of the header in front of Large blocks}
  LargeBlockHeaderSize = SizeOf(TLargeBlockHeader);
{$ifdef FullDebugMode}
  {We need space for the header. 4 bytes for the trailer and 4 bytes for the
   trailing block size when then block is free}
  FullDebugBlockOverhead = SizeOf(TFullDebugBlockHeader) + 2 * SizeOf(Pointer);
{$endif}

{-------------------------Private variables----------------------------}
var
  {-----------------Small block management------------------}
  {The small block types. Sizes include the leading 4-byte overhead. Sizes are
   picked to limit maximum wastage to about 10% or 256 bytes (whichever is
   less) where possible.}
  SmallBlockTypes: packed array[0..NumSmallBlockTypes - 1] of TSmallBlockType =(
    {8/16 byte jumps}
    (BlockSize: 16 {$ifdef UseCustomFixedSizeMoveRoutines}; UpsizeMoveProcedure: Move12{$endif}),
{$ifndef Align16Bytes}
    (BlockSize: 24 {$ifdef UseCustomFixedSizeMoveRoutines}; UpsizeMoveProcedure: Move20{$endif}),
{$endif}
    (BlockSize: 32 {$ifdef UseCustomFixedSizeMoveRoutines}; UpsizeMoveProcedure: Move28{$endif}),
{$ifndef Align16Bytes}
    (BlockSize: 40 {$ifdef UseCustomFixedSizeMoveRoutines}; UpsizeMoveProcedure: Move36{$endif}),
{$endif}
    (BlockSize: 48 {$ifdef UseCustomFixedSizeMoveRoutines}; UpsizeMoveProcedure: Move44{$endif}),
{$ifndef Align16Bytes}
    (BlockSize: 56 {$ifdef UseCustomFixedSizeMoveRoutines}; UpsizeMoveProcedure: Move52{$endif}),
{$endif}
    (BlockSize: 64 {$ifdef UseCustomFixedSizeMoveRoutines}; UpsizeMoveProcedure: Move60{$endif}),
{$ifndef Align16Bytes}
    (BlockSize: 72 {$ifdef UseCustomFixedSizeMoveRoutines}; UpsizeMoveProcedure: Move68{$endif}),
{$endif}
    (BlockSize: 80),
{$ifndef Align16Bytes}
    (BlockSize: 88),
{$endif}
    (BlockSize: 96),
{$ifndef Align16Bytes}
    (BlockSize: 104),
{$endif}
    (BlockSize: 112),
{$ifndef Align16Bytes}
    (BlockSize: 120),
{$endif}
    (BlockSize: 128),
{$ifndef Align16Bytes}
    (BlockSize: 136),
{$endif}
    (BlockSize: 144),
{$ifndef Align16Bytes}
    (BlockSize: 152),
{$endif}
    (BlockSize: 160),
    {16 byte jumps}
    (BlockSize: 176),
    (BlockSize: 192),
    (BlockSize: 208),
    (BlockSize: 224),
    (BlockSize: 240),
    (BlockSize: 256),
    (BlockSize: 272),
    (BlockSize: 288),
    (BlockSize: 304),
    (BlockSize: 320),
    {32 byte jumps}
    (BlockSize: 352),
    (BlockSize: 384),
    (BlockSize: 416),
    (BlockSize: 448),
    (BlockSize: 480),
    {48 byte jumps}
    (BlockSize: 528),
    (BlockSize: 576),
    (BlockSize: 624),
    (BlockSize: 672),
    {64 byte jumps}
    (BlockSize: 736),
    (BlockSize: 800),
    {80 byte jumps}
    (BlockSize: 880),
    (BlockSize: 960),
    {96 byte jumps}
    (BlockSize: 1056),
    (BlockSize: 1152),
    {112 byte jumps}
    (BlockSize: 1264),
    (BlockSize: 1376),
    {128 byte jumps}
    (BlockSize: 1504),
    {144 byte jumps}
    (BlockSize: 1648),
    {160 byte jumps}
    (BlockSize: 1808),
    {176 byte jumps}
    (BlockSize: 1984),
    {192 byte jumps}
    (BlockSize: 2176),
    {208 byte jumps}
    (BlockSize: 2384),
    {224 byte jumps}
    (BlockSize: MaximumSmallBlockSize),
    {The last block size occurs three times. If, during a GetMem call, the
     requested block size is already locked by another thread then up to two
     larger block sizes may be used instead. Having the last block size occur
     three times avoids the need to have a size overflow check.}
    (BlockSize: MaximumSmallBlockSize),
    (BlockSize: MaximumSmallBlockSize));
  {Size to small block type translation table}
  AllocSize2SmallBlockTypeIndX4: packed array[0..(MaximumSmallBlockSize - 1) div SmallBlockGranularity] of byte;
  {-----------------Medium block management------------------}
  {A dummy medium block pool header: Maintains a circular list of all medium
   block pools to enable memory leak detection on program shutdown.}
  MediumBlockPoolsCircularList: TMediumBlockPoolHeader;
  {Are medium blocks locked?}
  MediumBlocksLocked: boolean;
  {The sequential feed medium block pool.}
  LastSequentiallyFedMediumBlock: Pointer;
  MediumSequentialFeedBytesLeft: Cardinal;
  {The medium block bins are divided into groups of 32 bins. If a bit
   is set in this group bitmap, then at least one bin in the group has free
   blocks.}
  MediumBlockBinGroupBitmap: Cardinal;
  {The medium block bins: total of 32 * 32 = 1024 bins of a certain
   minimum size.}
  MediumBlockBinBitmaps: packed array[0..MediumBlockBinGroupCount - 1] of Cardinal;
  {The medium block bins. There are 1024 LIFO circular linked lists each
   holding blocks of a specified minimum size. The sizes vary in size from
   MinimumMediumBlockSize to MaximumMediumBlockSize. The bins are treated as
   type TMediumFreeBlock to avoid pointer checks.}
  MediumBlockBins: packed array[0..MediumBlockBinCount - 1] of TMediumFreeBlock;
  {-----------------Large block management------------------}
  {Are large blocks locked?}
  LargeBlocksLocked: boolean;
  {A dummy large block header: Maintains a list of all allocated large blocks
   to enable memory leak detection on program shutdown.}
  LargeBlocksCircularList: TLargeBlockHeader;
  {-------------------------Expected Memory Leak Structures--------------------}
{$ifdef EnableMemoryLeakReporting}
  {The expected memory leaks}
  ExpectedMemoryLeaks: PExpectedMemoryLeaks;
{$endif}
  {---------------------Full Debug Mode structures--------------------}
{$ifdef FullDebugMode}
  {The 64K block of reserved memory used to trap invalid memory accesses using
   fields in a freed object.}
  ReservedBlock: Pointer;
  {The virtual method index count - used to get the virtual method index for a
   virtual method call on a freed object.}
  VMIndex: Integer;
  {The fake VMT used to catch virtual method calls on freed objects.}
  FreedObjectVMT: packed record
    VMTData: array[vmtSelfPtr .. vmtParent + 3] of byte;
    VMTMethods: array[4 + vmtParent .. MaxFakeVMTEntries * 4 + vmtParent + 3] of Byte;
  end;
  {$ifdef CatchUseOfFreedInterfaces}
  VMTBadInterface: array[0..MaxFakeVMTEntries - 1] of Pointer;
  {$endif}
  {The debug info library units}
  DebugInfoDllAvailable: Boolean = True;
  DebugInfoDll: Cardinal;
  DebugInfoProc: TDebugInfoProc;
{$endif}
  {--------------Other info--------------}
  {The memory manager that was replaced}
  OldMemoryManager: TMemoryManager;
  {The replacement memory manager}
  NewMemoryManager: TMemoryManager;
  {A string uniquely identifying the current process (for sharing the memory
   manager between DLLs and the main application)}
  UniqueProcessIDString: String[20] = '????????_PID_FastMM'#0;
{$ifdef ShareMM}
  {$ifndef Linux}
  {The handle of the MM window}
  MMWindow: HWND;
  {$endif}
{$endif}
  {Has FastMM been installed?}
  FastMMIsInstalled: Boolean;
  {Is the MM in place a shared memory manager?}
  IsMemoryManagerOwner: Boolean;

{----------------Utility Functions------------------}

{Compare [AAddress], CompareVal:
 If Equal: [AAddress] := NewVal and result = CompareVal
 If Unequal: Result := [AAddress]}
function LockCmpxchg(CompareVal, NewVal: byte; AAddress: PByte): Byte;
asm
  {On entry:
    al = CompareVal,
    dl = NewVal,
    ecx = AAddress}
{$ifndef LINUX}
  lock cmpxchg [ecx], dl
{$else}
  {Workaround for Kylix compiler bug}
  db $F0, $0F, $B0, $11
{$endif}
end;

{$ifndef AsmVersion}
{Gets the first set bit and resets it, returning the bit index}
function FindFirstSetBit(ACardinal: Cardinal): Cardinal;
asm
  {On entry:
    eax = ACardinal}
  bsf eax, eax
end;
{$endif}

{----------------Faster Move Procedures-------------------}

{Fixed size move operations ignore the size parameter. All moves are assumed to
 be non-overlapping.}

procedure Move12(const ASource; var ADest; ACount: Integer);
asm
  mov ecx, [eax]
  mov [edx], ecx
  mov ecx, [eax + 4]
  mov eax, [eax + 8]
  mov [edx + 4], ecx
  mov [edx + 8], eax
end;

procedure Move20(const ASource; var ADest; ACount: Integer);
asm
  mov ecx, [eax]
  mov [edx], ecx
  mov ecx, [eax + 4]
  mov [edx + 4], ecx
  mov ecx, [eax + 8]
  mov [edx + 8], ecx
  mov ecx, [eax + 12]
  mov eax, [eax + 16]
  mov [edx + 12], ecx
  mov [edx + 16], eax
end;

procedure Move28(const ASource; var ADest; ACount: Integer);
asm
  mov ecx, [eax]
  mov [edx], ecx
  mov ecx, [eax + 4]
  mov [edx + 4], ecx
  mov ecx, [eax + 8]
  mov [edx + 8], ecx
  mov ecx, [eax + 12]
  mov [edx + 12], ecx
  mov ecx, [eax + 16]
  mov [edx + 16], ecx
  mov ecx, [eax + 20]
  mov eax, [eax + 24]
  mov [edx + 20], ecx
  mov [edx + 24], eax
end;

procedure Move36(const ASource; var ADest; ACount: Integer);
asm
  fild qword ptr [eax]
  fild qword ptr [eax + 8]
  fild qword ptr [eax + 16]
  fild qword ptr [eax + 24]
  mov ecx, [eax + 32]
  mov [edx + 32], ecx
  fistp qword ptr [edx + 24]
  fistp qword ptr [edx + 16]
  fistp qword ptr [edx + 8]
  fistp qword ptr [edx]
end;

procedure Move44(const ASource; var ADest; ACount: Integer);
asm
  fild qword ptr [eax]
  fild qword ptr [eax + 8]
  fild qword ptr [eax + 16]
  fild qword ptr [eax + 24]
  fild qword ptr [eax + 32]
  mov ecx, [eax + 40]
  mov [edx + 40], ecx
  fistp qword ptr [edx + 32]
  fistp qword ptr [edx + 24]
  fistp qword ptr [edx + 16]
  fistp qword ptr [edx + 8]
  fistp qword ptr [edx]
end;

procedure Move52(const ASource; var ADest; ACount: Integer);
asm
  fild qword ptr [eax]
  fild qword ptr [eax + 8]
  fild qword ptr [eax + 16]
  fild qword ptr [eax + 24]
  fild qword ptr [eax + 32]
  fild qword ptr [eax + 40]
  mov ecx, [eax + 48]
  mov [edx + 48], ecx
  fistp qword ptr [edx + 40]
  fistp qword ptr [edx + 32]
  fistp qword ptr [edx + 24]
  fistp qword ptr [edx + 16]
  fistp qword ptr [edx + 8]
  fistp qword ptr [edx]
end;

procedure Move60(const ASource; var ADest; ACount: Integer);
asm
  fild qword ptr [eax]
  fild qword ptr [eax + 8]
  fild qword ptr [eax + 16]
  fild qword ptr [eax + 24]
  fild qword ptr [eax + 32]
  fild qword ptr [eax + 40]
  fild qword ptr [eax + 48]
  mov ecx, [eax + 56]
  mov [edx + 56], ecx
  fistp qword ptr [edx + 48]
  fistp qword ptr [edx + 40]
  fistp qword ptr [edx + 32]
  fistp qword ptr [edx + 24]
  fistp qword ptr [edx + 16]
  fistp qword ptr [edx + 8]
  fistp qword ptr [edx]
end;

procedure Move68(const ASource; var ADest; ACount: Integer);
asm
  fild qword ptr [eax]
  fild qword ptr [eax + 8]
  fild qword ptr [eax + 16]
  fild qword ptr [eax + 24]
  fild qword ptr [eax + 32]
  fild qword ptr [eax + 40]
  fild qword ptr [eax + 48]
  fild qword ptr [eax + 56]
  mov ecx, [eax + 64]
  mov [edx + 64], ecx
  fistp qword ptr [edx + 56]
  fistp qword ptr [edx + 48]
  fistp qword ptr [edx + 40]
  fistp qword ptr [edx + 32]
  fistp qword ptr [edx + 24]
  fistp qword ptr [edx + 16]
  fistp qword ptr [edx + 8]
  fistp qword ptr [edx]
end;

{Variable size move procedure: Assumes ACount is 4 less than a multiple of 16.
 Always moves at least 12 bytes, irrespective of ACount.}
{$ifndef EnableMMX}
procedure MoveX16L4(const ASource; var ADest; ACount: Integer);
asm
  {Make the counter negative based: The last 12 bytes are moved separately}
  sub ecx, 12
  add eax, ecx
  add edx, ecx
  neg ecx
  jns @MoveLast12
@MoveLoop:
  {Move a 16 byte block}
  fild qword ptr [eax + ecx]
  fild qword ptr [eax + ecx + 8]
  fistp qword ptr [edx + ecx + 8]
  fistp qword ptr [edx + ecx]
  {Are there another 16 bytes to move?}
  add ecx, 16
  js @MoveLoop
@MoveLast12:
  {Do the last 12 bytes}
  fild qword ptr [eax + ecx]
  fistp qword ptr [edx + ecx]
  mov eax, [eax + ecx + 8]
  mov [edx + ecx + 8], eax
end;
{$else}
procedure MoveX16L4(const ASource; var ADest; ACount: Integer);
asm
  {Make the counter negative based: The last 12 bytes are moved separately}
  sub ecx, 12
  add eax, ecx
  add edx, ecx
  neg ecx
  jns @MoveLast12
@MoveLoop:
  {Move a 16 byte block}
{$ifdef Delphi5}
  {Delphi 5 compatibility}
  db $0f, $6f, $04, $01
  db $0f, $6f, $4c, $01, $08
  db $0f, $7f, $04, $11
  db $0f, $7f, $4c, $11, $08
{$else}
  movq mm0, [eax + ecx]
  movq mm1, [eax + ecx + 8]
  movq [edx + ecx], mm0
  movq [edx + ecx + 8], mm1
{$endif}
  {Are there another 16 bytes to move?}
  add ecx, 16
  js @MoveLoop
@MoveLast12:
  {Do the last 12 bytes}
{$ifdef Delphi5}
  {Delphi 5 compatibility}
  db $0f, $6f, $04, $01
{$else}
  movq mm0, [eax + ecx]
{$endif}
  mov eax, [eax + ecx + 8]
{$ifdef Delphi5}
  {Delphi 5 compatibility}
  db $0f, $7f, $04, $11
{$else}
  movq [edx + ecx], mm0
{$endif}
  mov [edx + ecx + 8], eax
  {Exit MMX state}
{$ifdef Delphi5}
  {Delphi 5 compatibility}
  db $0f, $77
{$else}
  emms
{$endif}
end;
{$endif}

{$ifndef EnableMMX}
{Variable size move procedure: Assumes ACount is 4 less than a multiple of 8.
 Always moves at least 12 bytes, irrespective of ACount.}
procedure MoveX8L4(const ASource; var ADest; ACount: Integer);
asm
  {Make the counter negative based: The last 4 bytes are moved separately}
  sub ecx, 4
  add eax, ecx
  add edx, ecx
  neg ecx
@MoveLoop:
  {Move an 8 byte block}
  fild qword ptr [eax + ecx]
  fistp qword ptr [edx + ecx]
  {Are there another 8 bytes to move?}
  add ecx, 8
  js @MoveLoop
  {Do the last 4 bytes}
  mov eax, [eax + ecx]
  mov [edx + ecx], eax
end;
{$else}
procedure MoveX8L4(const ASource; var ADest; ACount: Integer);
asm
  {Make the counter negative based: The last 4 bytes are moved separately}
  sub ecx, 4
  add eax, ecx
  add edx, ecx
  neg ecx
@MoveLoop:
  {Move an 8 byte block}
{$ifdef Delphi5}
  {Delphi 5 compatibility}
  db $0f, $6f, $04, $01
  db $0f, $7f, $04, $11
{$else}
  movq mm0, [eax + ecx]
  movq [edx + ecx], mm0
{$endif}
  {Are there another 8 bytes to move?}
  add ecx, 8
  js @MoveLoop
  {Exit MMX state}
{$ifdef Delphi5}
  {Delphi 5 compatibility}
  db $0f, $77
{$else}
  emms
{$endif}
  {Do the last 4 bytes}
  mov eax, [eax + ecx]
  mov [edx + ecx], eax
end;
{$endif}

{----------------Windows Emulation Functions for Kylix Support-----------------}

{$ifdef Linux}

const
  {Messagebox constants}
  MB_OK = 0;
  MB_ICONERROR = $10;
  MB_TASKMODAL = $2000;
  {Virtual memory constants}
  MEM_COMMIT = $1000;
  MEM_RELEASE = $8000;
  MEM_TOP_DOWN = $100000;
  PAGE_READWRITE = 4;

procedure MessageBox(hWnd: Cardinal; AMessageText, AMessageTitle: PChar; uType: Cardinal); stdcall;
begin
  writeln(AMessageText);
end;

function VirtualAlloc(lpvAddress: Pointer; dwSize, flAllocationType, flProtect: Cardinal): Pointer; stdcall;
begin
  Result := valloc(dwSize);
end;

function VirtualFree(lpAddress: Pointer; dwSize, dwFreeType: Cardinal): LongBool; stdcall;
begin
  free(lpAddress);
  Result := True;
end;

procedure Sleep(dwMilliseconds: Cardinal); stdcall;
begin
  usleep(dwMilliseconds * 1000);
end;
{$endif}

{-----------------Debugging Support Functions and Procedures------------------}

{$ifndef Linux}
function DelphiIsRunning: boolean;
begin
  Result := FindWindow('TAppBuilder', nil) <> 0;
end;
{$endif}

{Converts a cardinal to string at the buffer location, returning the new
 buffer position.}
function CardinalToStrBuf(ACardinal: Cardinal; ABuffer: PChar): PChar;
asm
  {On entry: eax = ACardinal, edx = ABuffer}
  push edi
  mov  edi, edx                //Pointer to the first character in edi
  //Calculate leading digit: divide the number by 1e9
  add  eax, 1                  //Increment the number
  mov  edx, $89705F41          //1e9 reciprocal
  mul  edx                     //Multplying with reciprocal
  shr  eax, 30                 //Save fraction bits
  mov  ecx, edx                //First digit in bits <31:29>
  and  edx, $1FFFFFFF          //Filter fraction part edx<28:0>
  shr  ecx, 29                 //Get leading digit into accumulator
  lea  edx, [edx+4*edx]        //Calculate ...
  add  edx, eax                //... 5*fraction
  mov  eax, ecx                //Copy leading digit
  or   eax, '0'                //Convert digit to ASCII
  mov  [edi], al               //Store digit out to memory
  //Calculate digit #2
  mov  eax, edx                //Point format such that 1.0 = 2^28
  cmp  ecx, 1                  //Any non-zero digit yet ?
  sbb  edi, -1                 //Yes->increment ptr, No->keep old ptr
  shr  eax, 28                 //Next digit
  and  edx, $0fffffff          //Fraction part edx<27:0>
  or   ecx, eax                //Accumulate next digit
  or   eax, '0'                //Convert digit to ASCII
  mov  [edi], al               //Store digit out to memory
  //Calculate digit #3
  lea  eax, [edx*4+edx]        //5*fraction, new digit eax<31:27>
  lea  edx, [edx*4+edx]        //5*fraction, new fraction edx<26:0>
  cmp  ecx, 1                  //Any non-zero digit yet ?
  sbb  edi, -1                 //Yes->increment ptr, No->keep old ptr
  shr  eax, 27                 //Next digit
  and  edx, $07ffffff          //Fraction part
  or   ecx, eax                //Accumulate next digit
  or   eax, '0'                //Convert digit to ASCII
  mov  [edi], al               //Store digit out to memory
  //Calculate digit #4
  lea  eax, [edx*4+edx]        //5*fraction, new digit eax<31:26>
  lea  edx, [edx*4+edx]        //5*fraction, new fraction edx<25:0>
  cmp  ecx, 1                  //Any non-zero digit yet ?
  sbb  edi, -1                 //Yes->increment ptr, No->keep old ptr
  shr  eax, 26                 //Next digit
  and  edx, $03ffffff          //Fraction part
  or   ecx, eax                //Accumulate next digit
  or   eax, '0'                //Convert digit to ASCII
  mov  [edi], al               //Store digit out to memory
  //Calculate digit #5
  lea  eax, [edx*4+edx]        //5*fraction, new digit eax<31:25>
  lea  edx, [edx*4+edx]        //5*fraction, new fraction edx<24:0>
  cmp  ecx, 1                  //Any non-zero digit yet ?
  sbb  edi, -1                 //Yes->increment ptr, No->keep old ptr
  shr  eax, 25                 //Next digit
  and  edx, $01ffffff          //Fraction part
  or   ecx, eax                //Accumulate next digit
  or   eax, '0'                //Convert digit to ASCII
  mov  [edi], al               //Store digit out to memory
  //Calculate digit #6
  lea  eax, [edx*4+edx]        //5*fraction, new digit eax<31:24>
  lea  edx, [edx*4+edx]        //5*fraction, new fraction edx<23:0>
  cmp  ecx, 1                  //Any non-zero digit yet ?
  sbb  edi, -1                 //Yes->increment ptr, No->keep old ptr
  shr  eax, 24                 //Next digit
  and  edx, $00ffffff          //Fraction part
  or   ecx, eax                //Accumulate next digit
  or   eax, '0'                //Convert digit to ASCII
  mov  [edi], al               //Store digit out to memory
  //Calculate digit #7
  lea  eax, [edx*4+edx]        //5*fraction, new digit eax<31:23>
  lea  edx, [edx*4+edx]        //5*fraction, new fraction edx<31:23>
  cmp  ecx, 1                  //Any non-zero digit yet ?
  sbb  edi, -1                 //Yes->increment ptr, No->keep old ptr
  shr  eax, 23                 //Next digit
  and  edx, $007fffff          //Fraction part
  or   ecx, eax                //Accumulate next digit
  or   eax, '0'                //Convert digit to ASCII
  mov  [edi], al               //Store digit out to memory
  //Calculate digit #8
  lea  eax, [edx*4+edx]        //5*fraction, new digit eax<31:22>
  lea  edx, [edx*4+edx]        //5*fraction, new fraction edx<22:0>
  cmp  ecx, 1                  //Any non-zero digit yet ?
  sbb  edi, -1                 //Yes->increment ptr, No->keep old ptr
  shr  eax, 22                 //Next digit
  and  edx, $003fffff          //Fraction part
  or   ecx, eax                //Accumulate next digit
  or   eax, '0'                //Convert digit to ASCII
  mov  [edi], al               //Store digit out to memory
  //Calculate digit #9
  lea  eax, [edx*4+edx]        //5*fraction, new digit eax<31:21>
  lea  edx, [edx*4+edx]        //5*fraction, new fraction edx<21:0>
  cmp  ecx, 1                  //Any non-zero digit yet ?
  sbb  edi, -1                 //Yes->increment ptr, No->keep old ptr
  shr  eax, 21                 //Next digit
  and  edx, $001fffff          //Fraction part
  or   ecx, eax                //Accumulate next digit
  or   eax, '0'                //Convert digit to ASCII
  mov  [edi], al               //Store digit out to memory
  //Calculate digit #10
  lea  eax, [edx*4+edx]        //5*fraction, new digit eax<31:20>
  cmp  ecx, 1                  //Any-non-zero digit yet ?
  sbb  edi, -1                 //Yes->increment ptr, No->keep old ptr
  shr  eax, 20                 //Next digit
  or   eax, '0'                //Convert digit to ASCII
  mov  [edi], al               //Store last digit and end marker out to memory
  {Return a pointer to the next character}
  lea eax, [edi + 1]
  {Restore edi}
  pop  edi
end;

{Converts a cardinal to a hexadecimal string at the buffer location, returning
 the new buffer position.}
function CardinalToHexBuf(ACardinal: integer; ABuffer: PChar): PChar;
asm
  {On entry:
    eax = ACardinal
    edx = ABuffer}
  push ebx
  push edi
  {Save ACardinal in ebx}
  mov ebx, eax
  {Get a pointer to the first character in edi}
  mov edi, edx
  {Get the number in ecx as well}
  mov ecx, eax
  {Keep the low nibbles in ebx and the high nibbles in ecx}
  and ebx, $0f0f0f0f
  and ecx, $f0f0f0f0
  {Swap the bytes into the right order}
  ror ebx, 16
  ror ecx, 20
  {Get nibble 7}
  movzx eax, ch
  mov dl, ch
  mov al, byte ptr HexTable[eax]
  mov [edi], al
  cmp dl, 1
  sbb edi, -1
  {Get nibble 6}
  movzx eax, bh
  or dl, bh
  mov al, byte ptr HexTable[eax]
  mov [edi], al
  cmp dl, 1
  sbb edi, -1
  {Get nibble 5}
  movzx eax, cl
  or dl, cl
  mov al, byte ptr HexTable[eax]
  mov [edi], al
  cmp dl, 1
  sbb edi, -1
  {Get nibble 4}
  movzx eax, bl
  or dl, bl
  mov al, byte ptr HexTable[eax]
  mov [edi], al
  cmp dl, 1
  sbb edi, -1
  {Rotate ecx and ebx so we get access to the rest}
  shr ebx, 16
  shr ecx, 16
  {Get nibble 3}
  movzx eax, ch
  or dl, ch
  mov al, byte ptr HexTable[eax]
  mov [edi], al
  cmp dl, 1
  sbb edi, -1
  {Get nibble 2}
  movzx eax, bh
  or dl, bh
  mov al, byte ptr HexTable[eax]
  mov [edi], al
  cmp dl, 1
  sbb edi, -1
  {Get nibble 1}
  movzx eax, cl
  or dl, cl
  mov al, byte ptr HexTable[eax]
  mov [edi], al
  cmp dl, 1
  sbb edi, -1
  {Get nibble 0}
  movzx eax, bl
  mov al, byte ptr HexTable[eax]
  mov [edi], al
  {Return a pointer to the end of the string}
  lea eax, [edi + 1]
  {Restore registers}
  pop edi
  pop ebx
end;

{Appends the source text to the destination and returns the new destination
 position}
function AppendStringToBuffer(const ASource, ADestination: PChar; ACount: Cardinal): PChar;
begin
  System.Move(ASource^, ADestination^, ACount);
  Result := Pointer(Cardinal(ADestination) + ACount);
end;

{$ifndef LINUX}
procedure AppendEventLog(ABuffer: Pointer; ACount: Cardinal);
var
  LModuleName: array[0..1023] of char;
  LModuleNameLength, LFileHandle, LBytesWritten: Cardinal;
  LEventHeader: array[0..1023] of char;
  LMsgPtr: PChar;
  LSystemTime: TSystemTime;
begin
  {Get the name of the application}
  LModuleNameLength := GetModuleFileName(0, @LModuleName[0], length(LModuleName) - 100);
  {Replace the last few characters}
  if LModuleNameLength > 0 then
  begin
    {Change the filename}
    System.Move(LogFileExtension, LModuleName[LModuleNameLength - 4], Length(LogFileExtension));
    {Append the file}
    LFileHandle := CreateFile(LModuleName, GENERIC_READ or GENERIC_WRITE,
      0, nil, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
    if LFileHandle <> 0 then
    begin
      {Seek to the end of the file}
      SetFilePointer(LFileHandle, 0, nil, FILE_END);
      {Set the separator}
      LMsgPtr := AppendStringToBuffer(CRLF, @LEventHeader[0], length(CRLF));
      LMsgPtr := AppendStringToBuffer(EventSeparator, LMsgPtr, length(EventSeparator));
      {Set the date & time}
      GetLocalTime(LSystemTime);
      LMsgPtr := CardinalToStrBuf(LSystemTime.wYear, LMsgPtr);
      LMsgPtr^ := '/';
      Inc(LMsgPtr);
      LMsgPtr := CardinalToStrBuf(LSystemTime.wMonth, LMsgPtr);
      LMsgPtr^ := '/';
      Inc(LMsgPtr);
      LMsgPtr := CardinalToStrBuf(LSystemTime.wDay, LMsgPtr);
      LMsgPtr^ := ' ';
      Inc(LMsgPtr);
      LMsgPtr := CardinalToStrBuf(LSystemTime.wHour, LMsgPtr);
      LMsgPtr^ := ':';
      Inc(LMsgPtr);
      if LSystemTime.wMinute < 10 then
      begin
        LMsgPtr^ := '0';
        Inc(LMsgPtr);
      end;
      LMsgPtr := CardinalToStrBuf(LSystemTime.wMinute, LMsgPtr);
      LMsgPtr^ := ':';
      Inc(LMsgPtr);
      if LSystemTime.wSecond < 10 then
      begin
        LMsgPtr^ := '0';
        Inc(LMsgPtr);
      end;
      LMsgPtr := CardinalToStrBuf(LSystemTime.WSecond, LMsgPtr);
      {Write the header}
      LMsgPtr := AppendStringToBuffer(EventSeparator, LMsgPtr, length(EventSeparator));
      LMsgPtr := AppendStringToBuffer(CRLF, LMsgPtr, length(CRLF));
      WriteFile(LFileHandle, LEventHeader[0], Cardinal(LMsgPtr) - Cardinal(@LEventHeader[0]), LBytesWritten, nil);
      {Write the data}
      WriteFile(LFileHandle, ABuffer^, ACount, LBytesWritten, nil);
      {Close the file}
      CloseHandle(LFileHandle);
    end;
  end;
end;
{$endif}

{Dumps the call stack trace to the given address. Fills the list with the
 addresses where the called addresses can be found.}
procedure GetStackTrace(AReturnAddresses: PCardinal; AMaxDepth, ASkipLevels: Cardinal);
var
  LStackTop, LStackBottom, LCurrentFrame: Cardinal;
begin
  {Get the call stack top and current bottom}
  asm
    mov eax, FS:[4]
    mov LStackTop, eax
    mov LStackBottom, ebp
  end;
  {Get the current frame start}
  LCurrentFrame := LStackBottom;
  {Fill the call stack}
  while (AMaxDepth > 0)
    and (LCurrentFrame >= LStackBottom)
    and (LCurrentFrame < LStackTop) do
  begin
    {Ignore the requested number of levels}
    if ASkipLevels = 0 then
    begin
      AReturnAddresses^ := PCardinal(LCurrentFrame + 4)^;
      Inc(AReturnAddresses);
      Dec(AMaxDepth);
    end
    else
      Dec(ASkipLevels);
    {Get the next frame}
    LCurrentFrame := PCardinal(LCurrentFrame)^;
  end;
  {Clear the remaining dwords}
  while (AMaxDepth > 0) do
  begin
    AReturnAddresses^ := 0;
    Inc(AReturnAddresses);
    Dec(AMaxDepth);
  end;
end;


{Returns the class for a memory block. Returns nil if it is not a valid class}
function GetObjectClass(APointer: Pointer): TClass;
{$ifndef Linux}
var
  LMemInfo: TMemoryBasicInformation;

  function InternalIsValidClass(APossibleClass: Pointer; ADepth: Integer = 0): Boolean;
  var
    LParentClass: Pointer;
  begin
    {Do we need to recheck the VM?}
    if (Cardinal(LMemInfo.BaseAddress) > (Cardinal(APossibleClass) + Cardinal(vmtSelfPtr)))
      or ((Cardinal(LMemInfo.BaseAddress) + LMemInfo.RegionSize) < (Cardinal(APossibleClass) + Cardinal(vmtParent + 3))) then
    begin
      {Get the VM status for the pointer}
      VirtualQuery(Pointer(Cardinal(APossibleClass) + Cardinal(vmtSelfPtr)), LMemInfo,
        SizeOf(LMemInfo));
    end;
    {Get the result, while checking for recursion}
    Result := (ADepth < 1000)
      {The required info must fit inside the region}
      and ((Cardinal(LMemInfo.BaseAddress) + LMemInfo.RegionSize) > (Cardinal(APossibleClass) + Cardinal(vmtParent + 3)))
      {Memory must be committed}
      and (LMemInfo.State = MEM_COMMIT)
      {Memory must be readable}
      and (LMemInfo.Protect and
       (PAGE_READONLY or PAGE_READWRITE or PAGE_EXECUTE_READ or PAGE_EXECUTE_READWRITE or PAGE_EXECUTE_WRITECOPY) <> 0)
      {All class fields must fit inside the block}
      {The self pointer must be valid}
      and (PPointer(Cardinal(APossibleClass) + Cardinal(vmtSelfPtr))^ = APossibleClass);
    {Check the parent class}
    if Result then
    begin
      LParentClass := PPointer(Cardinal(APossibleClass) + Cardinal(vmtParent))^;
      {The parent must also be a valid class}
      Result := (LParentClass = nil) or
        InternalIsValidClass(Pointer(Cardinal(LParentClass) - Cardinal(vmtSelfPtr)), ADepth + 1)
    end;
  end;

begin
  {Get the class pointer from the (suspected) object}
  Result := TClass(PCardinal(APointer)^);
  {No VM info yet}
  LMemInfo.RegionSize := 0;
  {Check the block}
  if (Cardinal(Result) < 65536)
    or (not InternalIsValidClass(Result, 0)) then
  begin
    Result := nil;
  end;
end;
{$else}
begin
  {Not currently supported under Linux}
  Result := nil;
end;
{$endif}

{Fills a block of memory with the given dword. Always fills a multiple of 4 bytes}
procedure FillDWord(var AAddress; AByteCount: integer; ADWordFillValue: Cardinal);
asm
  {On Entry: eax = AAddress
   edx = AByteCount
   ecx = ADWordFillValue}
  add eax, edx
  neg edx
  jns @Done
@FillLoop:
  mov [eax + edx], ecx
  add edx, 4
  js @FillLoop
@Done:
end;

{-----------------Medium Block Management------------------}

{Locks the medium blocks}
procedure LockMediumBlocks;
begin
  {Lock the medium blocks}
{$ifndef AssumeMultiThreaded}
  if IsMultiThread then
{$endif}
  begin
    while LockCmpxchg(0, 1, @MediumBlocksLocked) <> 0 do
    begin
      Sleep(InitialSleepTime);
      if LockCmpxchg(0, 1, @MediumBlocksLocked) = 0 then
        break;
      Sleep(AdditionalSleepTime);
    end;
  end;
end;

{$ifndef AsmVersion}
{Removes a medium block from the circular linked list of free blocks.
 Does not change any header flags. Medium blocks should be locked
 before calling this procedure.}
procedure RemoveMediumFreeBlock(APMediumFreeBlock: PMediumFreeBlock);
var
  LPreviousFreeBlock, LNextFreeBlock: PMediumFreeBlock;
  LBinNumber, LBinGroupNumber: Cardinal;
begin
  {Get the current previous and next blocks}
  LNextFreeBlock := APMediumFreeBlock.NextFreeBlock;
  LPreviousFreeBlock := APMediumFreeBlock.PreviousFreeBlock;
  {Remove this block from the linked list}
  LPreviousFreeBlock.NextFreeBlock := LNextFreeBlock;
  LNextFreeBlock.PreviousFreeBlock := LPreviousFreeBlock;
  {Is this bin now empty? If the previous and next free block pointers are
   equal, they must point to the bin.}
  if LPreviousFreeBlock = LNextFreeBlock then
  begin
    {Get the bin number for this block size}
    LBinNumber := (Cardinal(LNextFreeBlock) - Cardinal(@MediumBlockBins)) div SizeOf(TMediumFreeBlock);
    LBinGroupNumber := LBinNumber div 32;
    {Flag this bin as empty}
    MediumBlockBinBitmaps[LBinGroupNumber] := MediumBlockBinBitmaps[LBinGroupNumber]
      and (not (1 shl (LBinNumber and 31)));
    {Is the group now entirely empty?}
    if MediumBlockBinBitmaps[LBinGroupNumber] = 0 then
    begin
      {Flag this group as empty}
      MediumBlockBinGroupBitmap := MediumBlockBinGroupBitmap
        and (not (1 shl LBinGroupNumber));
    end;
  end;
end;
{$else}
{Removes a medium block from the circular linked list of free blocks.
 Does not change any header flags. Medium blocks should be locked
 before calling this procedure.}
procedure RemoveMediumFreeBlock(APMediumFreeBlock: PMediumFreeBlock);
asm
  {On entry: eax = APMediumFreeBlock}
  {Get the current previous and next blocks}
  mov ecx, TMediumFreeBlock[eax].NextFreeBlock
  mov edx, TMediumFreeBlock[eax].PreviousFreeBlock
  {Is this bin now empty? If the previous and next free block pointers are
   equal, they must point to the bin.}
  cmp ecx, edx
  {Remove this block from the linked list}
  mov TMediumFreeBlock[ecx].PreviousFreeBlock, edx
  mov TMediumFreeBlock[edx].NextFreeBlock, ecx
  {Is this bin now empty? If the previous and next free block pointers are
   equal, they must point to the bin.}
  je @BinIsNowEmpty
@Done:
  db $F3
  ret
  {*Align branch target*}
@BinIsNowEmpty:
  {Get the bin number for this block size in ecx}
  sub ecx, offset MediumBlockBins
  mov edx, ecx
  shr ecx, 3
  {Get the group number in edx}
  movzx edx, dh
  {Flag this bin as empty}
  mov eax, -2
  rol eax, cl
  and dword ptr [MediumBlockBinBitmaps + edx * 4], eax
  jnz @Done
  {Flag this group as empty}
  mov eax, -2
  mov ecx, edx
  rol eax, cl
  and MediumBlockBinGroupBitmap, eax
  db $F3
end;
{$endif}

{$ifndef AsmVersion}
{Inserts a medium block into the appropriate medium block bin.}
procedure InsertMediumBlockIntoBin(APMediumFreeBlock: PMediumFreeBlock; AMediumBlockSize: Cardinal);
var
  LBinNumber, LBinGroupNumber: Cardinal;
  LPBin, LPFirstFreeBlock: PMediumFreeBlock;
begin
  {Get the bin number for this block size. Get the bin that holds blocks of at
   least this size.}
  LBinNumber := (AMediumBlockSize - MinimumMediumBlockSize) div MediumBlockGranularity;
  if LBinNumber >= MediumBlockBinCount then
    LBinNumber := MediumBlockBinCount - 1;
  {Get the bin}
  LPBin := @MediumBlockBins[LBinNumber];
  {Bins are LIFO, se we insert this block as the first free block in the bin}
  LPFirstFreeBlock := LPBin.NextFreeBlock;
  APMediumFreeBlock.PreviousFreeBlock := LPBin;
  APMediumFreeBlock.NextFreeBlock := LPFirstFreeBlock;
  LPFirstFreeBlock.PreviousFreeBlock := APMediumFreeBlock;
  LPBin.NextFreeBlock := APMediumFreeBlock;
  {Was this bin empty?}
  if LPFirstFreeBlock = LPBin then
  begin
    {Get the group number}
    LBinGroupNumber := LBinNumber div 32;
    {Flag this bin as used}
    MediumBlockBinBitmaps[LBinGroupNumber] := MediumBlockBinBitmaps[LBinGroupNumber]
      or (1 shl (LBinNumber and 31));
    {Flag the group as used}
    MediumBlockBinGroupBitmap := MediumBlockBinGroupBitmap
      or (1 shl LBinGroupNumber);
  end;
end;
{$else}
{Inserts a medium block into the appropriate medium block bin.}
procedure InsertMediumBlockIntoBin(APMediumFreeBlock: PMediumFreeBlock; AMediumBlockSize: Cardinal);
asm
  {On entry: eax = APMediumFreeBlock, edx = AMediumBlockSize}
  {Get the bin number for this block size. Get the bin that holds blocks of at
   least this size.}
  sub edx, MinimumMediumBlockSize
  shr edx, 8
  {Validate the bin number}
  sub edx, MediumBlockBinCount - 1
  sbb ecx, ecx
  and edx, ecx
  add edx, MediumBlockBinCount - 1
  {Get the bin in ecx}
  lea ecx, [MediumBlockBins + edx * 8]
  {Bins are LIFO, se we insert this block as the first free block in the bin}
  mov edx, TMediumFreeBlock[ecx].NextFreeBlock
  {Was this bin empty?}
  cmp edx, ecx
  mov TMediumFreeBlock[eax].PreviousFreeBlock, ecx
  mov TMediumFreeBlock[eax].NextFreeBlock, edx
  mov TMediumFreeBlock[edx].PreviousFreeBlock, eax
  mov TMediumFreeBlock[ecx].NextFreeBlock, eax
  {Was this bin empty?}
  je @BinWasEmpty
  db $F3
  ret
  {*Align branch target*}
@BinWasEmpty:
  {Get the bin number in ecx}
  sub ecx, offset MediumBlockBins
  mov edx, ecx
  shr ecx, 3
  {Get the group number in edx}
  movzx edx, dh
  {Flag this bin as not empty}
  mov eax, 1
  shl eax, cl
  or dword ptr [MediumBlockBinBitmaps + edx * 4], eax
  {Flag the group as not empty}
  mov eax, 1
  mov ecx, edx
  shl eax, cl
  or MediumBlockBinGroupBitmap, eax
  {Done}
  db $F3
end;
{$endif}

{$ifndef AsmVersion}
{Bins what remains in the current sequential feed medium block pool. Medium
 blocks must be locked.}
procedure BinMediumSequentialFeedRemainder;
var
  LSequentialFeedFreeSize, LNextBlockSizeAndFlags: Cardinal;
  LPRemainderBlock, LNextMediumBlock: Pointer;
begin
  LSequentialFeedFreeSize := MediumSequentialFeedBytesLeft;
  if LSequentialFeedFreeSize > 0 then
  begin
    {Get the block after the open space}
    LNextMediumBlock := LastSequentiallyFedMediumBlock;
    LNextBlockSizeAndFlags := PCardinal(Cardinal(LNextMediumBlock) - BlockHeaderSize)^;
    {Point to the remainder}
    LPRemainderBlock := Pointer(Cardinal(LNextMediumBlock) - LSequentialFeedFreeSize);
{$ifndef FullDebugMode}
    {Can the next block be combined with the remainder?}
    if (LNextBlockSizeAndFlags and IsFreeBlockFlag) <> 0 then
    begin
      {Increase the size of this block}
      Inc(LSequentialFeedFreeSize, LNextBlockSizeAndFlags and DropMediumAndLargeFlagsMask);
      {Remove the next block as well}
      if (LNextBlockSizeAndFlags and DropMediumAndLargeFlagsMask) >= MinimumMediumBlockSize then
        RemoveMediumFreeBlock(LNextMediumBlock);
    end
    else
    begin
{$endif}
      {Set the "previous block is free" flag of the next block}
      PCardinal(Cardinal(LNextMediumBlock) - BlockHeaderSize)^ := LNextBlockSizeAndFlags or PreviousMediumBlockIsFreeFlag;
{$ifndef FullDebugMode}
    end;
{$endif}
    {Store the size of the block as well as the flags}
    PCardinal(Cardinal(LPRemainderBlock) - BlockHeaderSize)^ := LSequentialFeedFreeSize or IsMediumBlockFlag or IsFreeBlockFlag;
    {Store the trailing size marker}
    PCardinal(Cardinal(LPRemainderBlock) + LSequentialFeedFreeSize - 8)^ := LSequentialFeedFreeSize;
{$ifdef FullDebugMode}
    {In full debug mode the sequential feed remainder will never be too small to
     fit a full debug header.}
    {Clear the user area of the block}
    FillDWord(Pointer(Cardinal(LPRemainderBlock) + SizeOf(TFullDebugBlockHeader) + 4)^,
      LSequentialFeedFreeSize - FullDebugBlockOverhead - 4,
      {$ifndef CatchUseOfFreedInterfaces}DebugFillDWord{$else}Cardinal(@VMTBadInterface){$endif});
    {We need to set a valid debug header and footer in the remainder}
    PFullDebugBlockHeader(LPRemainderBlock).HeaderCheckSum := Cardinal(LPRemainderBlock);
    PCardinal(Cardinal(LPRemainderBlock) + SizeOf(TFullDebugBlockHeader))^ := not Cardinal(LPRemainderBlock);
{$endif}
    {Bin this medium block}
    if LSequentialFeedFreeSize >= MinimumMediumBlockSize then
      InsertMediumBlockIntoBin(LPRemainderBlock, LSequentialFeedFreeSize);
  end;
end;
{$else}
{Bins what remains in the current sequential feed medium block pool. Medium
 blocks must be locked.}
procedure BinMediumSequentialFeedRemainder;
asm
  cmp MediumSequentialFeedBytesLeft, 0
  jne @MustBinMedium
  {Nothing to bin}
  db $f3
  ret
  {*Align branch target*}
@MustBinMedium:
  {Get a pointer to the last sequentially allocated medium block}
  mov eax, LastSequentiallyFedMediumBlock
  {Is the block that was last fed sequentially free?}
  test byte ptr [eax - 4], IsFreeBlockFlag
  jnz @LastBlockFedIsFree
  {Set the "previous block is free" flag in the last block fed}
  or dword ptr [eax - 4], PreviousMediumBlockIsFreeFlag
  {Get the remainder in edx}
  mov edx, MediumSequentialFeedBytesLeft
  {Point eax to the start of the remainder}
  sub eax, edx
@BinTheRemainder:
  {Status: eax = start of remainder, edx = size of remainder}
  {Store the size of the block as well as the flags}
  lea ecx, [edx + IsMediumBlockFlag + IsFreeBlockFlag]
  mov [eax - 4], ecx
  {Store the trailing size marker}
  mov [eax + edx - 8], edx
  {Bin this medium block}
  cmp edx, MinimumMediumBlockSize
  jnb InsertMediumBlockIntoBin
  db $f3
  ret
  {*Align branch target*}
  db $66
  nop
@LastBlockFedIsFree:
  {Drop the flags}
  mov edx, DropMediumAndLargeFlagsMask
  and edx, [eax - 4]
  {Free the last block fed}
  cmp edx, MinimumMediumBlockSize
  jb @DontRemoveLastFed
  {Last fed block is free - remove it from its size bin}
  call RemoveMediumFreeBlock
  {Re-read eax and edx}
  mov eax, LastSequentiallyFedMediumBlock
  mov edx, DropMediumAndLargeFlagsMask
  and edx, [eax - 4]
@DontRemoveLastFed:
  {Get the number of bytes left in ecx}
  mov ecx, MediumSequentialFeedBytesLeft
  {Point eax to the start of the remainder}
  sub eax, ecx
  {edx = total size of the remainder}
  add edx, ecx
  jmp @BinTheRemainder
end;
{$endif}

{Allocates a new sequential feed medium block pool and immediately splits off a
 block of the requested size. The block size must be a multiple of 16 and
 medium blocks must be locked.}
function AllocNewSequentialFeedMediumPool(AFirstBlockSize: Cardinal): Pointer;
var
  LOldFirstMediumBlockPool: PMediumBlockPoolHeader;
  LNewPool: Pointer;
begin
  {Bin the current sequential feed remainder}
  BinMediumSequentialFeedRemainder;
  {Allocate a new sequential feed block pool}
  LNewPool := VirtualAlloc(nil, MediumBlockPoolSize, MEM_COMMIT, PAGE_READWRITE);
  if LNewPool <> nil then
  begin
    {Insert this block pool into the list of block pools}
    LOldFirstMediumBlockPool := MediumBlockPoolsCircularList.NextMediumBlockPoolHeader;
    PMediumBlockPoolHeader(LNewPool).PreviousMediumBlockPoolHeader := @MediumBlockPoolsCircularList;
    MediumBlockPoolsCircularList.NextMediumBlockPoolHeader := LNewPool;
    PMediumBlockPoolHeader(LNewPool).NextMediumBlockPoolHeader := LOldFirstMediumBlockPool;
    LOldFirstMediumBlockPool.PreviousMediumBlockPoolHeader := LNewPool;
    {Store the sequential feed pool trailer}
    PCardinal(Cardinal(LNewPool) + MediumBlockPoolSize - BlockHeaderSize)^ := IsMediumBlockFlag;
    {Get the number of bytes still available}
    MediumSequentialFeedBytesLeft := (MediumBlockPoolSize - MediumBlockPoolHeaderSize) - AFirstBlockSize;
    {Get the result}
    Result := Pointer(Cardinal(LNewPool) + MediumBlockPoolSize - AFirstBlockSize);
    LastSequentiallyFedMediumBlock := Result;
    {Store the block header}
    PCardinal(Cardinal(Result) - BlockHeaderSize)^ := AFirstBlockSize or IsMediumBlockFlag;
  end
  else
  begin
    {Out of memory}
    MediumSequentialFeedBytesLeft := 0;
    Result := nil;
  end;
end;

{Frees a medium block pool. Medium blocks must be locked on entry.}
procedure FreeMediumBlockPool(AMediumBlockPool: PMediumBlockPoolHeader);
var
  LPPreviousMediumBlockPoolHeader, LPNextMediumBlockPoolHeader: PMediumBlockPoolHeader;
begin
  {Remove this medium block pool from the linked list}
  LPPreviousMediumBlockPoolHeader := AMediumBlockPool.PreviousMediumBlockPoolHeader;
  LPNextMediumBlockPoolHeader := AMediumBlockPool.NextMediumBlockPoolHeader;
  LPPreviousMediumBlockPoolHeader.NextMediumBlockPoolHeader := LPNextMediumBlockPoolHeader;
  LPNextMediumBlockPoolHeader.PreviousMediumBlockPoolHeader := LPPreviousMediumBlockPoolHeader;
  {Free the medium block pool}
  VirtualFree(AMediumBlockPool, 0, MEM_RELEASE);
end;

{-----------------Large Block Management------------------}

{Locks the large blocks}
procedure LockLargeBlocks;
begin
  {Lock the large blocks}
{$ifndef AssumeMultiThreaded}
  if IsMultiThread then
{$endif}
  begin
    while LockCmpxchg(0, 1, @LargeBlocksLocked) <> 0 do
    begin
      Sleep(InitialSleepTime);
      if LockCmpxchg(0, 1, @LargeBlocksLocked) = 0 then
        break;
      Sleep(AdditionalSleepTime);
    end;
  end;
end;

{Allocates a Large block of at least ASize (actual size may be Larger to
 allow for alignment etc.). ASize must be the actual user requested size. This
 procedure will pad it to the appropriate page boundary and also add the space
 required by the header.}
function AllocateLargeBlock(ASize: Cardinal): Pointer;
var
  LLargeUsedBlockSize: Cardinal;
  LOldFirstLargeBlock: PLargeBlockHeader;
begin
  {Pad the block size to include the header and granularity. We also add a
   4-byte overhead so a huge block size is a multiple of 16 bytes less 4 (so we
   can use a single move function for reallocating all block types)}
  LLargeUsedBlockSize := (ASize + LargeBlockHeaderSize + LargeBlockGranularity - 1 + BlockHeaderSize)
    and -LargeBlockGranularity;
  {Get the Large block}
  Result := VirtualAlloc(nil, LLargeUsedBlockSize, MEM_COMMIT or MEM_TOP_DOWN,
    PAGE_READWRITE);
  {Set the Large block fields}
  if Result <> nil then
  begin
    {Set the large block size and flags}
    PLargeBlockHeader(Result).UserAllocatedSize := ASize;
    PLargeBlockHeader(Result).BlockSizeAndFlags := LLargeUsedBlockSize or IsLargeBlockFlag;
    {Insert the large block into the linked list of large blocks}
    LockLargeBlocks;
    LOldFirstLargeBlock := LargeBlocksCircularList.NextLargeBlockHeader;
    PLargeBlockHeader(Result).PreviousLargeBlockHeader := @LargeBlocksCircularList;
    LargeBlocksCircularList.NextLargeBlockHeader := Result;
    PLargeBlockHeader(Result).NextLargeBlockHeader := LOldFirstLargeBlock;
    LOldFirstLargeBlock.PreviousLargeBlockHeader := Result;
    LargeBlocksLocked := False;
    {Add the size of the header}
    Inc(Cardinal(Result), LargeBlockHeaderSize);
{$ifdef FullDebugMode}
    {Clear the user area of the block}
    FillDWord(Pointer(Cardinal(Result) + SizeOf(TFullDebugBlockHeader) + 4)^,
      LLargeUsedBlockSize - LargeBlockHeaderSize - FullDebugBlockOverhead - 4,
      {$ifndef CatchUseOfFreedInterfaces}DebugFillDWord{$else}Cardinal(@VMTBadInterface){$endif});
    {Set the debug header and footer}
    PFullDebugBlockHeader(Result).HeaderCheckSum := Cardinal(Result);
    PCardinal(Cardinal(Result) + SizeOf(TFullDebugBlockHeader))^ := not Cardinal(Result);
{$endif}
  end;
end;

{Frees a Large block, returning 0 on success, -1 otherwise}
function FreeLargeBlock(APointer: Pointer): Integer;
var
  LPreviousLargeBlockHeader, LNextLargeBlockHeader: PLargeBlockHeader;
begin
  {Point to the start of the Large block (always 64K aligned)}
  APointer := Pointer(Cardinal(APointer) - LargeBlockHeaderSize);
  {Get the previous and next large blocks}
  LockLargeBlocks;
  LPreviousLargeBlockHeader := PLargeBlockHeader(APointer).PreviousLargeBlockHeader;
  LNextLargeBlockHeader := PLargeBlockHeader(APointer).NextLargeBlockHeader;
  {Try to free the Large block}
  if VirtualFree(APointer, 0, MEM_RELEASE) then
  begin
    {Remove the large block from the linked list}
    LNextLargeBlockHeader.PreviousLargeBlockHeader := LPreviousLargeBlockHeader;
    LPreviousLargeBlockHeader.NextLargeBlockHeader := LNextLargeBlockHeader;
    {All OK}
    Result := 0;
  end
  else
    Result := -1;
  LargeBlocksLocked := False;
end;

{---------------------Replacement Memory Manager Interface---------------------}

{$ifndef ASMVersion}
{Replacement for SysGetMem (pascal version)}
function FastGetMem(ASize: Integer): Pointer;
var
  LMediumBlock{$ifndef FullDebugMode}, LNextFreeBlock, LSecondSplit{$endif}: PMediumFreeBlock;
  LNextMediumBlockHeader: PCardinal;
  LBlockSize, LAvailableBlockSize{$ifndef FullDebugMode}, LSecondSplitSize{$endif}: Cardinal;
  LPSmallBlockType: PSmallBlockType;
  LPSmallBlockPool, LPNewFirstPool: PSmallBlockPoolHeader;
  LBinNumber: Cardinal;
  LNewFirstFreeBlock: Pointer;
  LPMediumBin: PMediumFreeBlock;
  LSequentialFeedFreeSize: Cardinal;
  {$ifndef FullDebugMode}LBinGroupsMasked, {$endif}LBinGroupMasked, LBinGroupNumber: Cardinal;
begin
  {Is it a small block? -> Take the header size into account when
   determining the required block size}
  if Cardinal(ASize) <= (MaximumSmallBlockSize - BlockHeaderSize) then
  begin
    {-------------------------Allocate a small block---------------------------}
    {Get the block type from the size}
    LPSmallBlockType := PSmallBlockType(AllocSize2SmallBlockTypeIndX4[
      (Cardinal(ASize) + (BlockHeaderSize - 1)) div SmallBlockGranularity] * 8
      + Cardinal(@SmallBlockTypes));
    {Lock the block type}
{$ifndef AssumeMultiThreaded}
    if IsMultiThread then
{$endif}
    begin
      while True do
      begin
        {Try to lock the small block type}
        if LockCmpxchg(0, 1, @LPSmallBlockType.BlockTypeLocked) = 0 then
          break;
        {Try the next block type}
        Inc(Cardinal(LPSmallBlockType), SizeOf(TSmallBlockType));
        if LockCmpxchg(0, 1, @LPSmallBlockType.BlockTypeLocked) = 0 then
          break;
        {Try up to two sizes past the requested size}
        Inc(Cardinal(LPSmallBlockType), SizeOf(TSmallBlockType));
        if LockCmpxchg(0, 1, @LPSmallBlockType.BlockTypeLocked) = 0 then
          break;
        {All three sizes locked - given up and sleep}
        Dec(Cardinal(LPSmallBlockType), 2 * SizeOf(TSmallBlockType));
        {Both this block type and the next is in use: sleep}
        Sleep(InitialSleepTime);
        {Try the lock again}
        if LockCmpxchg(0, 1, @LPSmallBlockType.BlockTypeLocked) = 0 then
          break;
        {Sleep longer}
        Sleep(AdditionalSleepTime);
      end;
    end;
    {Get the first pool with free blocks}
    LPSmallBlockPool := LPSmallBlockType.NextPartiallyFreePool;
    {Is the pool valid?}
    if Cardinal(LPSmallBlockPool) <> Cardinal(LPSmallBlockType) then
    begin
      {Get the first free offset}
      Result := LPSmallBlockPool.FirstFreeBlock;
      {Get the new first free block}
      LNewFirstFreeBlock := PPointer(Cardinal(Result) - 4)^;
{$ifdef CheckHeapForCorruption}
      {The block should be free}
      if (Cardinal(LNewFirstFreeBlock) and ExtractSmallFlagsMask) <> IsFreeBlockFlag then
  {$ifdef BCB6OrDelphi7AndUp}
        System.Error(reInvalidPtr);
  {$else}
        System.RunError(reInvalidPtr);
  {$endif}
{$endif}
      LNewFirstFreeBlock := Pointer(Cardinal(LNewFirstFreeBlock) and DropSmallFlagsMask);
      {Increment the number of used blocks}
      Inc(LPSmallBlockPool.BlocksInUse);
      {Set the new first free block}
      LPSmallBlockPool.FirstFreeBlock := LNewFirstFreeBlock;
      {Is the pool now full?}
      if LNewFirstFreeBlock = nil then
      begin
        {Pool is full - remove it from the partially free list}
        LPNewFirstPool := LPSmallBlockPool.NextPartiallyFreePool;
        LPSmallBlockType.NextPartiallyFreePool := LPNewFirstPool;
        LPNewFirstPool.PreviousPartiallyFreePool := PSmallBlockPoolHeader(LPSmallBlockType);
      end;
    end
    else
    begin
      {Try to feed a small block sequentially}
      Result := LPSmallBlockType.NextSequentialFeedBlockAddress;
      {Can another block fit?}
      if Cardinal(Result) <= Cardinal(LPSmallBlockType.MaxSequentialFeedBlockAddress) then
      begin
        {Get the sequential feed block pool}
        LPSmallBlockPool := LPSmallBlockType.CurrentSequentialFeedPool;
        {Increment the number of used blocks in the sequential feed pool}
        Inc(LPSmallBlockPool.BlocksInUse);
        {Store the next sequential feed block address}
        LPSmallBlockType.NextSequentialFeedBlockAddress := Pointer(Cardinal(Result) + LPSmallBlockType.BlockSize);
      end
      else
      begin
        {Need to allocate a pool: Lock the medium blocks}
        LockMediumBlocks;
{$ifndef FullDebugMode}
        {Are there any available blocks of a suitable size?}
        LBinGroupsMasked := MediumBlockBinGroupBitmap and ($ffffff00 or LPSmallBlockType.AllowedGroupsForBlockPoolBitmap);
        if LBinGroupsMasked <> 0 then
        begin
          {Get the bin group with free blocks}
          LBinGroupNumber := FindFirstSetBit(LBinGroupsMasked);
          {Get the bin in the group with free blocks}
          LBinNumber := FindFirstSetBit(MediumBlockBinBitmaps[LBinGroupNumber])
            + LBinGroupNumber * 32;
          LPMediumBin := @MediumBlockBins[LBinNumber];
          {Get the first block in the bin}
          LMediumBlock := LPMediumBin.NextFreeBlock;
          {Remove the first block from the linked list (LIFO)}
          LNextFreeBlock := LMediumBlock.NextFreeBlock;
          LPMediumBin.NextFreeBlock := LNextFreeBlock;
          LNextFreeBlock.PreviousFreeBlock := LPMediumBin;
          {Is this bin now empty?}
          if LNextFreeBlock = LPMediumBin then
          begin
            {Flag this bin as empty}
            MediumBlockBinBitmaps[LBinGroupNumber] := MediumBlockBinBitmaps[LBinGroupNumber]
              and (not (1 shl (LBinNumber and 31)));
            {Is the group now entirely empty?}
            if MediumBlockBinBitmaps[LBinGroupNumber] = 0 then
            begin
              {Flag this group as empty}
              MediumBlockBinGroupBitmap := MediumBlockBinGroupBitmap
                and (not (1 shl LBinGroupNumber));
            end;
          end;
          {Get the size of the available medium block}
          LBlockSize := PCardinal(Cardinal(LMediumBlock) - BlockHeaderSize)^ and DropMediumAndLargeFlagsMask;
  {$ifdef CheckHeapForCorruption}
          {Check that this block is actually free and the next and previous blocks
           are both in use (except in full debug mode).}
          if ((PCardinal(Cardinal(LMediumBlock) - BlockHeaderSize)^ and ExtractMediumAndLargeFlagsMask) <> (IsMediumBlockFlag or IsFreeBlockFlag))
            or ((PCardinal(Cardinal(LMediumBlock) + (PCardinal(Cardinal(LMediumBlock) - BlockHeaderSize)^ and DropMediumAndLargeFlagsMask) - BlockHeaderSize)^ and IsFreeBlockFlag) <> 0)
          then
          begin
    {$ifdef BCB6OrDelphi7AndUp}
            System.Error(reInvalidPtr);
    {$else}
            System.RunError(reInvalidPtr);
    {$endif}
          end;
  {$endif}
          {Medium blocks are never split or coalesced in full debug mode}
          {Should the block be split?}
          if LBlockSize >= MaximumSmallBlockPoolSize then
          begin
            {Get the size of the second split}
            LSecondSplitSize := LBlockSize - LPSmallBlockType.OptimalBlockPoolSize;
            {Adjust the block size}
            LBlockSize := LPSmallBlockType.OptimalBlockPoolSize;
            {Split the block in two}
            LSecondSplit := PMediumFreeBlock(Cardinal(LMediumBlock) + LBlockSize);
            PCardinal(Cardinal(LSecondSplit) - BlockHeaderSize)^ := LSecondSplitSize or (IsMediumBlockFlag or IsFreeBlockFlag);
            {Store the size of the second split as the second last dword}
            PCardinal(Cardinal(LSecondSplit) + LSecondSplitSize - 8)^ := LSecondSplitSize;
            {Put the remainder in a bin (it will be big enough)}
            InsertMediumBlockIntoBin(LSecondSplit, LSecondSplitSize);
          end
          else
          begin
            {Mark this block as used in the block following it}
            LNextMediumBlockHeader := PCardinal(Cardinal(LMediumBlock) + LBlockSize - BlockHeaderSize);
            LNextMediumBlockHeader^ := LNextMediumBlockHeader^ and (not PreviousMediumBlockIsFreeFlag);
          end;
        end
        else
        begin
{$endif}
          {Check the sequential feed medium block pool for space}
          LSequentialFeedFreeSize := MediumSequentialFeedBytesLeft;
          if LSequentialFeedFreeSize >= LPSmallBlockType.MinimumBlockPoolSize then
          begin
            {Enough sequential feed space: Will the remainder be usable?}
            if LSequentialFeedFreeSize >= (LPSmallBlockType.OptimalBlockPoolSize + MinimumMediumBlockSize) then
            begin
              LBlockSize := LPSmallBlockType.OptimalBlockPoolSize;
            end
            else
              LBlockSize := LSequentialFeedFreeSize;
            {Get the block}
            LMediumBlock := Pointer(Cardinal(LastSequentiallyFedMediumBlock) - LBlockSize);
            {Update the sequential feed parameters}
            LastSequentiallyFedMediumBlock := LMediumBlock;
            MediumSequentialFeedBytesLeft := LSequentialFeedFreeSize - LBlockSize;
          end
          else
          begin
            {Need to allocate a new sequential feed medium block pool: use the
             optimal size for this small block pool}
            LBlockSize := LPSmallBlockType.OptimalBlockPoolSize;
            {Allocate the medium block pool}
            LMediumBlock := AllocNewSequentialFeedMediumPool(LBlockSize);
            if LMediumBlock = nil then
            begin
              {Out of memory}
              {Unlock the medium blocks}
              MediumBlocksLocked := False;
              {Unlock the block type}
              LPSmallBlockType.BlockTypeLocked := False;
              {Failed}
              Result := nil;
              {done}
              exit;
            end;
          end;
{$ifndef FullDebugMode}
        end;
{$endif}
        {Mark this block as in use}
        {Set the size and flags for this block}
        PCardinal(Cardinal(LMediumBlock) - BlockHeaderSize)^ := LBlockSize or IsMediumBlockFlag;
        {Unlock medium blocks}
        MediumBlocksLocked := False;
        {Set up the block pool}
        LPSmallBlockPool := PSmallBlockPoolHeader(LMediumBlock);
        LPSmallBlockPool.BlockType := LPSmallBlockType;
        LPSmallBlockPool.FirstFreeBlock := nil;
        LPSmallBlockPool.BlocksInUse := 1;
        LPSmallBlockPool.SmallBlockPoolSignature := SmallBlockPoolSignatureValue;
        {Set it up for sequential block serving}
        LPSmallBlockType.CurrentSequentialFeedPool := LPSmallBlockPool;
        Result := Pointer(Cardinal(LPSmallBlockPool) + SmallBlockPoolHeaderSize);
        LPSmallBlockType.NextSequentialFeedBlockAddress := Pointer(Cardinal(Result) + LPSmallBlockType.BlockSize);
        LPSmallBlockType.MaxSequentialFeedBlockAddress := Pointer(Cardinal(LPSmallBlockPool) + LBlockSize - LPSmallBlockType.BlockSize);
      end;
{$ifdef FullDebugMode}
      {Clear the user area of the block}
      FillDWord(Pointer(Cardinal(Result) + (SizeOf(TFullDebugBlockHeader) + 4))^,
        LPSmallBlockType.BlockSize - FullDebugBlockOverhead - 4,
        {$ifndef CatchUseOfFreedInterfaces}DebugFillDWord{$else}Cardinal(@VMTBadInterface){$endif});
      {Block was fed sequentially - we need to set a valid debug header}
      PFullDebugBlockHeader(Result).HeaderCheckSum := Cardinal(Result);
      PCardinal(Cardinal(Result) + SizeOf(TFullDebugBlockHeader))^ := not Cardinal(Result);
{$endif}
    end;
    {Unlock the block type}
    LPSmallBlockType.BlockTypeLocked := False;
    {Set the block header}
    PCardinal(Cardinal(Result) - BlockHeaderSize)^ := Cardinal(LPSmallBlockPool);
  end
  else
  begin
    {Medium block or Large block?}
    if Cardinal(ASize) <= (MaximumMediumBlockSize - BlockHeaderSize) then
    begin
      {------------------------Allocate a medium block--------------------------}
      {Get the block size and bin number for this block size. Block sizes are
       rounded up to the next bin size.}
      LBlockSize := ((Cardinal(ASize) + (MediumBlockGranularity - 1 + BlockHeaderSize - MediumBlockSizeOffset))
        and -MediumBlockGranularity) + MediumBlockSizeOffset;
      {Get the bin number}
      LBinNumber := (LBlockSize - MinimumMediumBlockSize) div MediumBlockGranularity;
      {Lock the medium blocks}
      LockMediumBlocks;
      {Calculate the bin group}
      LBinGroupNumber := LBinNumber div 32;
      {Is there a suitable block inside this group?}
      LBinGroupMasked := MediumBlockBinBitmaps[LBinGroupNumber] and -(1 shl (LBinNumber and 31));
      if LBinGroupMasked <> 0 then
      begin
        {Get the actual bin number}
        LBinNumber := FindFirstSetBit(LBinGroupMasked) + LBinGroupNumber * 32;
      end
      else
      begin
{$ifndef FullDebugMode}
        {Try all groups greater than this group}
        LBinGroupsMasked := MediumBlockBinGroupBitmap and -(2 shl LBinGroupNumber);
        if LBinGroupsMasked <> 0 then
        begin
          {There is a suitable group with space: get the bin number}
          LBinGroupNumber := FindFirstSetBit(LBinGroupsMasked);
          {Get the bin in the group with free blocks}
          LBinNumber := FindFirstSetBit(MediumBlockBinBitmaps[LBinGroupNumber])
            + LBinGroupNumber * 32;
        end
        else
        begin
{$endif}
          {There are no bins with a suitable block: Sequentially feed the required block}
          LSequentialFeedFreeSize := MediumSequentialFeedBytesLeft;
          if LSequentialFeedFreeSize >= LBlockSize then
          begin
{$ifdef FullDebugMode}
            {In full debug mode a medium block must have enough bytes to fit
             all the debug info, so we must make sure there are no tiny medium
             blocks at the start of the pool.}
            if LSequentialFeedFreeSize - LBlockSize < (FullDebugBlockOverhead + BlockHeaderSize) then
              LBlockSize := LSequentialFeedFreeSize;
{$endif}
            {Block can be fed sequentially}
            Result := Pointer(Cardinal(LastSequentiallyFedMediumBlock) - LBlockSize);
            {Store the last sequentially fed block}
            LastSequentiallyFedMediumBlock := Result;
            {Store the remaining bytes}
            MediumSequentialFeedBytesLeft := LSequentialFeedFreeSize - LBlockSize;
            {Set the flags for the block}
            PCardinal(Cardinal(Result) - BlockHeaderSize)^ := LBlockSize or IsMediumBlockFlag;
          end
          else
          begin
            {Need to allocate a new sequential feed block}
            Result := AllocNewSequentialFeedMediumPool(LBlockSize);
          end;
{$ifdef FullDebugMode}
          {Block was fed sequentially - we need to set a valid debug header}
          if Result <> nil then
          begin
            PFullDebugBlockHeader(Result).HeaderCheckSum := Cardinal(Result);
            PCardinal(Cardinal(Result) + SizeOf(TFullDebugBlockHeader))^ := not Cardinal(Result);
            {Clear the user area of the block}
            FillDWord(Pointer(Cardinal(Result) + SizeOf(TFullDebugBlockHeader) + 4)^,
              LBlockSize - FullDebugBlockOverhead - 4,
              {$ifndef CatchUseOfFreedInterfaces}DebugFillDWord{$else}Cardinal(@VMTBadInterface){$endif});
          end;
{$endif}
          {Done}
          MediumBlocksLocked := False;
          exit;
{$ifndef FullDebugMode}
        end;
{$endif}
      end;
      {If we get here we have a valid LBinGroupNumber and LBinNumber:
       Use the first block in the bin, splitting it if necessary}
      {Get a pointer to the bin}
      LPMediumBin := @MediumBlockBins[LBinNumber];
      {Get the result}
      Result := LPMediumBin.NextFreeBlock;
{$ifdef CheckHeapForCorruption}
      {Check that this block is actually free and the next and previous blocks
       are both in use (except in full debug mode).}
      if ((PCardinal(Cardinal(Result) - BlockHeaderSize)^ and {$ifndef FullDebugMode}ExtractMediumAndLargeFlagsMask{$else}(IsMediumBlockFlag or IsFreeBlockFlag){$endif}) <> (IsFreeBlockFlag or IsMediumBlockFlag))
  {$ifndef FullDebugMode}
        or ((PCardinal(Cardinal(Result) + (PCardinal(Cardinal(Result) - BlockHeaderSize)^ and DropMediumAndLargeFlagsMask) - BlockHeaderSize)^ and ExtractMediumAndLargeFlagsMask) <> (IsMediumBlockFlag or PreviousMediumBlockIsFreeFlag))
  {$endif}
      then
      begin
  {$ifdef BCB6OrDelphi7AndUp}
        System.Error(reInvalidPtr);
  {$else}
        System.RunError(reInvalidPtr);
  {$endif}
      end;
{$endif}
      {Remove the block from the bin containing it}
      RemoveMediumFreeBlock(Result);
      {Get the block size}
      LAvailableBlockSize := PCardinal(Cardinal(Result) - BlockHeaderSize)^ and DropMediumAndLargeFlagsMask;
{$ifndef FullDebugMode}
      {Is it an exact fit or not?}
      LSecondSplitSize := LAvailableBlockSize - LBlockSize;
      if LSecondSplitSize <> 0 then
      begin
        {Split the block in two}
        LSecondSplit := PMediumFreeBlock(Cardinal(Result) + LBlockSize);
        {Set the size of the second split}
        PCardinal(Cardinal(LSecondSplit) - BlockHeaderSize)^ := LSecondSplitSize or (IsMediumBlockFlag or IsFreeBlockFlag);
        {Store the size of the second split as the second last dword}
        PCardinal(Cardinal(LSecondSplit) + LSecondSplitSize - 8)^ := LSecondSplitSize;
        {Put the remainder in a bin if it is big enough}
        if LSecondSplitSize >= MinimumMediumBlockSize then
          InsertMediumBlockIntoBin(LSecondSplit, LSecondSplitSize);
      end
      else
      begin
{$else}
        {In full debug mode blocks are never split or coalesced}
        LBlockSize := LAvailableBlockSize;
{$endif}
        {Mark this block as used in the block following it}
        LNextMediumBlockHeader := Pointer(Cardinal(Result) + LBlockSize - BlockHeaderSize);
{$ifndef FullDebugMode}
  {$ifdef CheckHeapForCorruption}
        {The next block must be in use}
        if (LNextMediumBlockHeader^ and ExtractMediumAndLargeFlagsMask) <> (IsMediumBlockFlag or PreviousMediumBlockIsFreeFlag) then
    {$ifdef BCB6OrDelphi7AndUp}
        System.Error(reInvalidPtr);
    {$else}
        System.RunError(reInvalidPtr);
    {$endif}
  {$endif}
{$endif}
        LNextMediumBlockHeader^ :=
          LNextMediumBlockHeader^ and (not PreviousMediumBlockIsFreeFlag);
{$ifndef FullDebugMode}
      end;
      {Set the size and flags for this block}
      PCardinal(Cardinal(Result) - BlockHeaderSize)^ := LBlockSize or IsMediumBlockFlag;
{$else}
      {In full debug mode blocks are never split or coalesced}
      Dec(PCardinal(Cardinal(Result) - BlockHeaderSize)^, IsFreeBlockFlag);
{$endif}
      {Unlock the medium blocks}
      MediumBlocksLocked := False;
    end
    else
    begin
      {Allocate a Large block}
      if ASize > 0 then
        Result := AllocateLargeBlock(ASize)
      else
        Result := nil;
    end;
  end;
end;
{$else}
{Replacement for SysGetMem (asm version)}
function FastGetMem(ASize: Integer): Pointer;
asm
  {On entry:
    eax = ASize}
  {Since most allocations are for small blocks, determine the small block type
   index so long}
  lea edx, [eax + BlockHeaderSize - 1]
{$ifdef Align16Bytes}
  shr edx, 4
{$else}
  shr edx, 3
{$endif}
  {Is it a small block?}
  cmp eax, (MaximumSmallBlockSize - BlockHeaderSize)
  {Save ebx}
  push ebx
  {Get the IsMultiThread variable so long}
{$ifndef AssumeMultiThreaded}
  mov cl, IsMultiThread
{$endif}
  {Is it a small block?}
  ja @NotASmallBlock
  {Do we need to lock the block type?}
{$ifndef AssumeMultiThreaded}
  test cl, cl
{$endif}
  {Get the small block type in ebx}
  movzx eax, byte ptr [AllocSize2SmallBlockTypeIndX4 + edx]
  lea ebx, [SmallBlockTypes + eax * 8]
  {Do we need to lock the block type?}
{$ifndef AssumeMultiThreaded}
  jnz @LockBlockTypeLoop
{$else}
  jmp @LockBlockTypeLoop
{$endif}
@GotLockOnSmallBlockType:
  {Find the next free block: Get the first pool with free blocks in edx}
  mov edx, TSmallBlockType[ebx].NextPartiallyFreePool
  {Get the first free block (or the next sequential feed address if edx = ebx)}
  mov eax, TSmallBlockPoolHeader[edx].FirstFreeBlock
  {Get the drop flags mask in ecx so long}
  mov ecx, DropSmallFlagsMask
  {Is there a pool with free blocks?}
  cmp edx, ebx
  je @TrySmallSequentialFeed
  {Increment the number of used blocks}
  add TSmallBlockPoolHeader[edx].BlocksInUse, 1
  {Get the new first free block}
  and ecx, [eax - 4]
  {Set the new first free block}
  mov TSmallBlockPoolHeader[edx].FirstFreeBlock, ecx
  {Set the block header}
  mov [eax - 4], edx
  {Is the chunk now full?}
  jz @RemoveSmallPool
  {Unlock the block type}
  mov TSmallBlockType[ebx].BlockTypeLocked, False
  {Restore ebx}
  pop ebx
  {All done}
  db $f3
  ret
  {**Align branch target**}
  db $66
  nop
@TrySmallSequentialFeed:
  {Try to feed a small block sequentially: Get the sequential feed block pool}
  mov edx, TSmallBlockType[ebx].CurrentSequentialFeedPool
  {Get the next sequential feed address so long}
  movzx ecx, TSmallBlockType[ebx].BlockSize
  add ecx, eax
  {Can another block fit?}
  cmp eax, TSmallBlockType[ebx].MaxSequentialFeedBlockAddress
  {Can another block fit?}
  ja @AllocateSmallBlockPool
  {Increment the number of used blocks in the sequential feed pool}
  add TSmallBlockPoolHeader[edx].BlocksInUse, 1
  {Store the next sequential feed block address}
  mov TSmallBlockType[ebx].NextSequentialFeedBlockAddress, ecx
  {Unlock the block type}
  mov TSmallBlockType[ebx].BlockTypeLocked, False
  {Set the block header}
  mov [eax - 4], edx
  {Restore ebx}
  pop ebx
  {All done}
  db $f3
  ret
  {**Align branch target**}
  db $66
  nop
@RemoveSmallPool:
  {Pool is full - remove it from the partially free list}
  mov ecx, TSmallBlockPoolHeader[edx].NextPartiallyFreePool
  mov TSmallBlockPoolHeader[ecx].PreviousPartiallyFreePool, ebx
  mov TSmallBlockType[ebx].NextPartiallyFreePool, ecx
  {Unlock the block type}
  mov TSmallBlockType[ebx].BlockTypeLocked, False
  {Restore ebx}
  pop ebx
  {All done}
  db $f3
  ret
  {**Align branch target**}
  nop
@LockBlockTypeLoop:
  mov eax, $100
  {Attempt to grab the block type}
  lock cmpxchg TSmallBlockType([ebx]).BlockTypeLocked, ah
  je @GotLockOnSmallBlockType
  {Try the next size}
  add ebx, Type(TSmallBlockType)
  mov eax, $100
  lock cmpxchg TSmallBlockType([ebx]).BlockTypeLocked, ah
  je @GotLockOnSmallBlockType
  {Try the next size (up to two sizes larger)}
  add ebx, Type(TSmallBlockType)
  mov eax, $100
  lock cmpxchg TSmallBlockType([ebx]).BlockTypeLocked, ah
  je @GotLockOnSmallBlockType
  {Block type and two sizes larger are all locked - give up and sleep}
  sub ebx, 2 * Type(TSmallBlockType)
  {Couldn't grab the block type - sleep and try again}
  push InitialSleepTime
  call Sleep
  {Try again}
  mov eax, $100
  {Attempt to grab the block type}
  lock cmpxchg TSmallBlockType([ebx]).BlockTypeLocked, ah
  je @GotLockOnSmallBlockType
  {Couldn't grab the block type - sleep and try again}
  push AdditionalSleepTime
  call Sleep
  {Try again}
  jmp @LockBlockTypeLoop
  {**Align branch target**}
  db $66, $66
  nop
@AllocateSmallBlockPool:
  {save additional registers}
  push esi
  push edi
  {Do we need to lock the medium blocks?}
{$ifndef AssumeMultiThreaded}
  cmp IsMultiThread, False
  je @MediumBlocksLockedForPool
{$endif}
@LockMediumBlocksForPool:
  mov eax, $100
  {Attempt to lock the medium blocks}
  lock cmpxchg MediumBlocksLocked, ah
  je @MediumBlocksLockedForPool
  {Couldn't lock the medium blocks - sleep and try again}
  push InitialSleepTime
  call Sleep
  {Try again}
  mov eax, $100
  {Attempt to grab the block type}
  lock cmpxchg MediumBlocksLocked, ah
  je @MediumBlocksLockedForPool
  {Couldn't lock the medium blocks - sleep and try again}
  push AdditionalSleepTime
  call Sleep
  {Try again}
  jmp @LockMediumBlocksForPool
  {*Align branch target*}
  db $66, $66
  nop
@MediumBlocksLockedForPool:
  {Are there any available blocks of a suitable size?}
  movsx esi, TSmallBlockType[ebx].AllowedGroupsForBlockPoolBitmap
  and esi, MediumBlockBinGroupBitmap
  jz @NoSuitableMediumBlocks
  {Get the bin group number with free blocks in eax}
  bsf eax, esi
  {Get the bin number in ecx}
  lea esi, [eax * 8]
  mov ecx, dword ptr [MediumBlockBinBitmaps + eax * 4]
  bsf ecx, ecx
  lea ecx, [ecx + esi * 4]
  {Get a pointer to the bin in edi}
  lea edi, [MediumBlockBins + ecx * 8]
  {Get the free block in esi}
  mov esi, TMediumFreeBlock[edi].NextFreeBlock
  {Remove the first block from the linked list (LIFO)}
  mov edx, TMediumFreeBlock[esi].NextFreeBlock
  mov TMediumFreeBlock[edi].NextFreeBlock, edx
  mov TMediumFreeBlock[edx].PreviousFreeBlock, edi
  {Is this bin now empty?}
  cmp edi, edx
  jne @MediumBinNotEmpty
  {eax = bin group number, ecx = bin number, edi = @bin, esi = free block, ebx = block type}
  {Flag this bin as empty}
  mov edx, -2
  rol edx, cl
  and dword ptr [MediumBlockBinBitmaps + eax * 4], edx
  jnz @MediumBinNotEmpty
  {Flag the group as empty}
  btr MediumBlockBinGroupBitmap, eax
@MediumBinNotEmpty:
  {esi = free block, ebx = block type}
  {Get the size of the available medium block in edi}
  mov edi, DropMediumAndLargeFlagsMask
  and edi, [esi - 4]
  cmp edi, MaximumSmallBlockPoolSize
  jb @UseWholeBlock
  {Split the block: get the size of the second part, new block size is the
   optimal size}
  mov edx, edi
  movzx edi, TSmallBlockType[ebx].OptimalBlockPoolSize
  sub edx, edi
  {Split the block in two}
  lea eax, [esi + edi]
  lea ecx, [edx + IsMediumBlockFlag + IsFreeBlockFlag]
  mov [eax - 4], ecx
  {Store the size of the second split as the second last dword}
  mov [eax + edx - 8], edx
  {Put the remainder in a bin (it will be big enough)}
  call InsertMediumBlockIntoBin
  jmp @GotMediumBlock
  {*Align branch target*}
@NoSuitableMediumBlocks:
  {Check the sequential feed medium block pool for space}
  movzx ecx, TSmallBlockType[ebx].MinimumBlockPoolSize
  mov edi, MediumSequentialFeedBytesLeft
  cmp edi, ecx
  jb @AllocateNewSequentialFeed
  {Get the address of the last block that was fed}
  mov esi, LastSequentiallyFedMediumBlock
  {Enough sequential feed space: Will the remainder be usable?}
  movzx ecx, TSmallBlockType[ebx].OptimalBlockPoolSize
  lea edx, [ecx + MinimumMediumBlockSize]
  cmp edi, edx
  jb @NotMuchSpace
  mov edi, ecx
@NotMuchSpace:
  sub esi, edi
  {Update the sequential feed parameters}
  sub MediumSequentialFeedBytesLeft, edi
  mov LastSequentiallyFedMediumBlock, esi
  {Get the block pointer}
  jmp @GotMediumBlock
  {*Align branch target*}
@AllocateNewSequentialFeed:
  {Need to allocate a new sequential feed medium block pool: use the
   optimal size for this small block pool}
  movzx eax, TSmallBlockType[ebx].OptimalBlockPoolSize
  mov edi, eax
  {Allocate the medium block pool}
  call AllocNewSequentialFeedMediumPool
  mov esi, eax
  test eax, eax
  jnz @GotMediumBlock
  mov MediumBlocksLocked, al
  mov TSmallBlockType[ebx].BlockTypeLocked, al
  pop edi
  pop esi
  pop ebx
  db $F3
  ret
  {*Align branch target*}
@UseWholeBlock:
  {esi = free block, ebx = block type, edi = block size}
  {Mark this block as used in the block following it}
  and byte ptr [esi + edi - 4], not PreviousMediumBlockIsFreeFlag
@GotMediumBlock:
  {esi = free block, ebx = block type, edi = block size}
  {Set the size and flags for this block}
  lea ecx, [edi + IsMediumBlockFlag]
  mov [esi - 4], ecx
  {Unlock medium blocks}
  xor eax, eax
  mov MediumBlocksLocked, al
  {Set up the block pool}
  mov TSmallBlockPoolHeader[esi].BlockType, ebx
  mov TSmallBlockPoolHeader[esi].FirstFreeBlock, eax
  mov TSmallBlockPoolHeader[esi].BlocksInUse, 1
  mov TSmallBlockPoolHeader[esi].SmallBlockPoolSignature, SmallBlockPoolSignatureValue
  {Set it up for sequential block serving}
  mov TSmallBlockType[ebx].CurrentSequentialFeedPool, esi
  {Return the pointer to the first block}
  lea eax, [esi + SmallBlockPoolHeaderSize]
  movzx ecx, TSmallBlockType[ebx].BlockSize
  lea edx, [eax + ecx]
  mov TSmallBlockType[ebx].NextSequentialFeedBlockAddress, edx
  add edi, esi
  sub edi, ecx
  mov TSmallBlockType[ebx].MaxSequentialFeedBlockAddress, edi
  {Unlock the small block type}
  mov TSmallBlockType[ebx].BlockTypeLocked, False
  {Set the small block header}
  mov [eax - 4], esi
  {Restore registers}
  pop edi
  pop esi
  pop ebx
  {Done}
  db $f3
  ret
{-------------------Medium block allocation-------------------}
  {*Align branch target*}
@LockMediumBlocks:
  mov eax, $100
  {Attempt to lock the medium blocks}
  lock cmpxchg MediumBlocksLocked, ah
  je @MediumBlocksLocked
  {Couldn't lock the medium blocks - sleep and try again}
  push InitialSleepTime
  call Sleep
  {Try again}
  mov eax, $100
  {Attempt to lock the medium blocks}
  lock cmpxchg MediumBlocksLocked, ah
  je @MediumBlocksLocked
  {Couldn't lock the medium blocks - sleep and try again}
  push AdditionalSleepTime
  call Sleep
  {Try again}
  jmp @LockMediumBlocks
  {**Align branch target here**}
  db $66
  nop
@NotASmallBlock:
  cmp eax, (MaximumMediumBlockSize - BlockHeaderSize)
  ja @IsALargeBlockRequest
  {Get the bin size for this block size. Block sizes are
   rounded up to the next bin size.}
  lea ebx, [eax + MediumBlockGranularity - 1 + BlockHeaderSize - MediumBlockSizeOffset]
  and ebx, -MediumBlockGranularity
  add ebx, MediumBlockSizeOffset
  {Do we need to lock the medium blocks?}
{$ifndef AssumeMultiThreaded}
  test cl, cl
  jnz @LockMediumBlocks
{$else}
  jmp @LockMediumBlocks
{$endif}
@MediumBlocksLocked:
  {Get the bin number in ecx and the group number in edx}
  lea edx, [ebx - MinimumMediumBlockSize]
  mov ecx, edx
  shr edx, 8 + 5
  shr ecx, 8
  {Is there a suitable block inside this group?}
  mov eax, -1
  shl eax, cl
  and eax, dword ptr [MediumBlockBinBitmaps + edx * 4]
  jz @GroupIsEmpty
  {Get the actual bin number}
  and ecx, -32
  bsf eax, eax
  or ecx, eax
  jmp @GotBinAndGroup
  {**Align branch target**}
  db $66
  nop
@GroupIsEmpty:
  {Try all groups greater than this group}
  mov eax, -2
  mov ecx, edx
  shl eax, cl
  and eax, MediumBlockBinGroupBitmap
  jz @TrySequentialFeedMedium
  {There is a suitable group with space: get the bin number}
  bsf edx, eax
  {Get the bin in the group with free blocks}
  mov eax, dword ptr [MediumBlockBinBitmaps + edx * 4]
  bsf ecx, eax
  mov eax, edx
  shl eax, 5
  or ecx, eax
  jmp @GotBinAndGroup
  {**Align branch target**}
  nop
@TrySequentialFeedMedium:
  mov ecx, MediumSequentialFeedBytesLeft
  {Block can be fed sequentially?}
  sub ecx, ebx
  jc @AllocateNewSequantialFeedForMedium
  {Get the block address}
  mov eax, LastSequentiallyFedMediumBlock
  sub eax, ebx
  mov LastSequentiallyFedMediumBlock, eax
  {Store the remaining bytes}
  mov MediumSequentialFeedBytesLeft, ecx
  {Set the flags for the block}
  or ebx, IsMediumBlockFlag
  mov [eax - 4], ebx
  jmp @MediumBlockGetDone
  {**Align branch target**}
@AllocateNewSequantialFeedForMedium:
  mov eax, ebx
  call AllocNewSequentialFeedMediumPool
@MediumBlockGetDone:
  mov MediumBlocksLocked, False
  pop ebx
  ret
  {** Align branch target **}
@GotBinAndGroup:
  {ebx = block size, ecx = bin number, edx = group number}
  push esi
  push edi
  {Get a pointer to the bin in edi}
  lea edi, [MediumBlockBins + ecx * 8]
  {Get the free block in esi}
  mov esi, TMediumFreeBlock[edi].NextFreeBlock
  {Remove the first block from the linked list (LIFO)}
  mov eax, TMediumFreeBlock[esi].NextFreeBlock
  mov TMediumFreeBlock[edi].NextFreeBlock, eax
  mov TMediumFreeBlock[eax].PreviousFreeBlock, edi
  {Is this bin now empty?}
  cmp edi, eax
  jne @MediumBinNotEmptyForMedium
  {eax = bin group number, ecx = bin number, edi = @bin, esi = free block, ebx = block size}
  {Flag this bin as empty}
  mov eax, -2
  rol eax, cl
  and dword ptr [MediumBlockBinBitmaps + edx * 4], eax
  jnz @MediumBinNotEmptyForMedium
  {Flag the group as empty}
  btr MediumBlockBinGroupBitmap, edx
@MediumBinNotEmptyForMedium:
  {esi = free block, ebx = block size}
  {Get the size of the available medium block in edi}
  mov edi, DropMediumAndLargeFlagsMask
  and edi, [esi - 4]
  {Get the size of the second split in edx}
  mov edx, edi
  sub edx, ebx
  jz @UseWholeBlockForMedium
  {Split the block in two}
  lea eax, [esi + ebx]
  lea ecx, [edx + IsMediumBlockFlag + IsFreeBlockFlag]
  mov [eax - 4], ecx
  {Store the size of the second split as the second last dword}
  mov [eax + edx - 8], edx
  {Put the remainder in a bin}
  cmp edx, MinimumMediumBlockSize
  jb @GotMediumBlockForMedium
  call InsertMediumBlockIntoBin
  jmp @GotMediumBlockForMedium
@UseWholeBlockForMedium:
  {Mark this block as used in the block following it}
  and byte ptr [esi + edi - 4], not PreviousMediumBlockIsFreeFlag
@GotMediumBlockForMedium:
  {Set the size and flags for this block}
  lea ecx, [ebx + IsMediumBlockFlag]
  mov [esi - 4], ecx
  {Unlock medium blocks}
  mov MediumBlocksLocked, False
  mov eax, esi
  pop edi
  pop esi
  pop ebx
  db $F3
  ret
{-------------------Large block allocation-------------------}
  {** Align branch target **}
@IsALargeBlockRequest:
  pop ebx
  test eax, eax
  jns AllocateLargeBlock
  xor eax, eax
end;
{$endif}

{$ifndef ASMVersion}
{Replacement for SysFreeMem (pascal version)}
function FastFreeMem(APointer: Pointer): Integer;
var
  LNextMediumBlock{$ifndef FullDebugMode}, LPreviousMediumBlock{$endif}: PMediumFreeBlock;
  LNextMediumBlockSizeAndFlags: Cardinal;
  LBlockSize{$ifndef FullDebugMode}, LPreviousMediumBlockSize{$endif}: Cardinal;
  LPSmallBlockPool{$ifndef FullDebugMode}, LPPreviousPool, LPNextPool{$endif},
    LPOldFirstPool: PSmallBlockPoolHeader;
  LPSmallBlockType: PSmallBlockType;
  LOldFirstFreeBlock: Pointer;
  LBlockHeader: Cardinal;
{$ifndef FullDebugMode}
  LPPreviousMediumBlockPoolHeader, LPNextMediumBlockPoolHeader: PMediumBlockPoolHeader;
{$endif}
begin
  {Get the small block header: Is it actually a small block?}
  LBlockHeader := PCardinal(Cardinal(APointer) - BlockHeaderSize)^;
  {Is it a small block that is in use?}
  if LBlockHeader and (IsFreeBlockFlag or IsMediumBlockFlag or IsLargeBlockFlag) = 0 then
  begin
    {Get a pointer to the block pool}
    LPSmallBlockPool := PSmallBlockPoolHeader(LBlockHeader);
    {Get the block type}
    LPSmallBlockType := LPSmallBlockPool.BlockType;
    {Lock the block type}
{$ifndef AssumeMultiThreaded}
    if IsMultiThread then
{$endif}
    begin
      while (LockCmpxchg(0, 1, @LPSmallBlockType.BlockTypeLocked) <> 0) do
      begin
        Sleep(InitialSleepTime);
        if LockCmpxchg(0, 1, @LPSmallBlockType.BlockTypeLocked) = 0 then
          break;
        Sleep(AdditionalSleepTime);
      end;
    end;
    {Get the old first free block}
    LOldFirstFreeBlock := LPSmallBlockPool.FirstFreeBlock;
    {Was the pool manager previously full?}
    if LOldFirstFreeBlock = nil then
    begin
      {Insert this as the first partially free pool for the block size}
      LPOldFirstPool := LPSmallBlockType.NextPartiallyFreePool;
      LPSmallBlockPool.NextPartiallyFreePool := LPOldFirstPool;
      LPOldFirstPool.PreviousPartiallyFreePool := LPSmallBlockPool;
      LPSmallBlockPool.PreviousPartiallyFreePool := PSmallBlockPoolHeader(LPSmallBlockType);
      LPSmallBlockType.NextPartiallyFreePool := LPSmallBlockPool;
    end;
    {Store the old first free block}
    PCardinal(Cardinal(APointer) - BlockHeaderSize)^ := Cardinal(LOldFirstFreeBlock) or IsFreeBlockFlag;
    {Store this as the new first free block}
    LPSmallBlockPool.FirstFreeBlock := APointer;
    {Decrement the number of allocated blocks}
    Dec(LPSmallBlockPool.BlocksInUse);
    {Small block pools are never freed in full debug mode. This increases the
     likehood of success in catching objects still being used after being
     destroyed.}
{$ifndef FullDebugMode}
    {Is the entire pool now free? -> Free it.}
    if LPSmallBlockPool.BlocksInUse = 0 then
    begin
      {Get the previous and next chunk managers}
      LPPreviousPool := LPSmallBlockPool.PreviousPartiallyFreePool;
      LPNextPool := LPSmallBlockPool.NextPartiallyFreePool;
      {Remove this manager}
      LPPreviousPool.NextPartiallyFreePool := LPNextPool;
      LPNextPool.PreviousPartiallyFreePool := LPPreviousPool;
      {Is this the sequential feed pool? If so, stop sequential feeding}
      if (LPSmallBlockType.CurrentSequentialFeedPool = LPSmallBlockPool) then
        LPSmallBlockType.MaxSequentialFeedBlockAddress := nil;
      {Unlock this block type}
      LPSmallBlockType.BlockTypeLocked := False;
      {Release this pool}
      FastFreeMem(LPSmallBlockPool);
    end
    else
    begin
{$endif}
      {Unlock this block type}
      LPSmallBlockType.BlockTypeLocked := False;
{$ifndef FullDebugMode}
    end;
{$endif}
    {No error}
    Result := 0;
  end
  else
  begin
    {Is this a medium block or a large block?}
    if LBlockHeader and (IsFreeBlockFlag or IsLargeBlockFlag) = 0 then
    begin
      {Get the medium block size}
      LBlockSize := LBlockHeader and DropMediumAndLargeFlagsMask;
      {Lock the medium blocks}
      LockMediumBlocks;
      {Can we combine this block with the next free block?}
      LNextMediumBlock := PMediumFreeBlock(Cardinal(APointer) + LBlockSize);
      LNextMediumBlockSizeAndFlags := PCardinal(Cardinal(LNextMediumBlock) - BlockHeaderSize)^;
{$ifndef FullDebugMode}
  {$ifdef CheckHeapForCorruption}
      {Check that this block was flagged as in use in the next block}
      if (LNextMediumBlockSizeAndFlags and PreviousMediumBlockIsFreeFlag) <> 0 then
    {$ifdef BCB6OrDelphi7AndUp}
        System.Error(reInvalidPtr);
    {$else}
        System.RunError(reInvalidPtr);
    {$endif}
  {$endif}
      if (LNextMediumBlockSizeAndFlags and IsFreeBlockFlag) <> 0 then
      begin
        {Increase the size of this block}
        Inc(LBlockSize, LNextMediumBlockSizeAndFlags and DropMediumAndLargeFlagsMask);
        {Remove the next block as well}
        if LNextMediumBlockSizeAndFlags >= MinimumMediumBlockSize then
          RemoveMediumFreeBlock(LNextMediumBlock);
      end
      else
      begin
{$endif}
        {Reset the "previous in use" flag of the next block}
        PCardinal(Cardinal(LNextMediumBlock) - BlockHeaderSize)^ := LNextMediumBlockSizeAndFlags or PreviousMediumBlockIsFreeFlag;
{$ifndef FullDebugMode}
      end;
      {Can we combine this block with the previous free block? We need to
       re-read the flags since it could have changed before we could lock the
       medium blocks.}
      if (PCardinal(Cardinal(APointer) - BlockHeaderSize)^ and PreviousMediumBlockIsFreeFlag) <> 0 then
      begin
        {Get the size of the free block just before this one}
        LPreviousMediumBlockSize := PCardinal(Cardinal(APointer) - 8)^;
        {Get the start of the previous block}
        LPreviousMediumBlock := PMediumFreeBlock(Cardinal(APointer) - LPreviousMediumBlockSize);
  {$ifdef CheckHeapForCorruption}
        {Check that the previous block is actually free}
        if (PCardinal(Cardinal(LPreviousMediumBlock) - BlockHeaderSize)^ and ExtractMediumAndLargeFlagsMask) <> (IsMediumBlockFlag or IsFreeBlockFlag) then
    {$ifdef BCB6OrDelphi7AndUp}
        System.Error(reInvalidPtr);
    {$else}
        System.RunError(reInvalidPtr);
    {$endif}
  {$endif}
        {Set the new block size}
        Inc(LBlockSize, LPreviousMediumBlockSize);
        {This is the new current block}
        APointer := LPreviousMediumBlock;
        {Remove the previous block from the linked list}
        if LPreviousMediumBlockSize >= MinimumMediumBlockSize then
          RemoveMediumFreeBlock(LPreviousMediumBlock);
      end;
  {$ifdef CheckHeapForCorruption}
      {Check that the previous block is currently flagged as in use}
      if (PCardinal(Cardinal(APointer) - BlockHeaderSize)^ and PreviousMediumBlockIsFreeFlag) <> 0 then
    {$ifdef BCB6OrDelphi7AndUp}
        System.Error(reInvalidPtr);
    {$else}
        System.RunError(reInvalidPtr);
    {$endif}
  {$endif}
      {Is the entire medium block pool free, and there are other free blocks
       that can fit the largest possible medium block? -> free it. (Except in
       full debug mode where medium pools are never freed.)}
      if (LBlockSize <> (MediumBlockPoolSize - MediumBlockPoolHeaderSize)) then
      begin
        {Store the size of the block as well as the flags}
        PCardinal(Cardinal(APointer) - BlockHeaderSize)^ := LBlockSize or (IsMediumBlockFlag or IsFreeBlockFlag);
{$else}
        {Mark the block as free}
        Inc(PCardinal(Cardinal(APointer) - BlockHeaderSize)^, IsFreeBlockFlag);
{$endif}
        {Store the trailing size marker}
        PCardinal(Cardinal(APointer) + LBlockSize - 8)^ := LBlockSize;
        {Insert this block back into the bins: Size check not required here,
         since medium blocks that are in use are not allowed to be
         shrunk smaller than MinimumMediumBlockSize}
        InsertMediumBlockIntoBin(APointer, LBlockSize);
{$ifndef FullDebugMode}
  {$ifdef CheckHeapForCorruption}
        {Check that this block is actually free and the next and previous blocks are both in use.}
        if ((PCardinal(Cardinal(APointer) - BlockHeaderSize)^ and ExtractMediumAndLargeFlagsMask) <> (IsMediumBlockFlag or IsFreeBlockFlag))
          or ((PCardinal(Cardinal(APointer) + (PCardinal(Cardinal(APointer) - BlockHeaderSize)^ and DropMediumAndLargeFlagsMask) - BlockHeaderSize)^ and IsFreeBlockFlag) <> 0) then
        begin
    {$ifdef BCB6OrDelphi7AndUp}
        System.Error(reInvalidPtr);
    {$else}
        System.RunError(reInvalidPtr);
    {$endif}
        end;
  {$endif}
{$endif}
        {Unlock medium blocks}
        MediumBlocksLocked := False;
        {All OK}
        Result := 0;
{$ifndef FullDebugMode}
      end
      else
      begin
        {Should this become the new sequential feed?}
        if MediumSequentialFeedBytesLeft <> MediumBlockPoolSize - MediumBlockPoolHeaderSize then
        begin
          {Bin the current sequential feed}
          BinMediumSequentialFeedRemainder;
          {Set this medium pool up as the new sequential feed pool:
           Store the sequential feed pool trailer}
          PCardinal(Cardinal(APointer) + LBlockSize - BlockHeaderSize)^ := IsMediumBlockFlag;
          {Store the number of bytes available in the sequential feed chunk}
          MediumSequentialFeedBytesLeft := MediumBlockPoolSize - MediumBlockPoolHeaderSize;
          {Set the last sequentially fed block}
          LastSequentiallyFedMediumBlock := Pointer(Cardinal(APointer) + LBlockSize);
          {Unlock medium blocks}
          MediumBlocksLocked := False;
          {Success}
          Result := 0;
        end
        else
        begin
          {Remove this medium block pool from the linked list}
          Dec(Cardinal(APointer), MediumBlockPoolHeaderSize);
          LPPreviousMediumBlockPoolHeader := PMediumBlockPoolHeader(APointer).PreviousMediumBlockPoolHeader;
          LPNextMediumBlockPoolHeader := PMediumBlockPoolHeader(APointer).NextMediumBlockPoolHeader;
          LPPreviousMediumBlockPoolHeader.NextMediumBlockPoolHeader := LPNextMediumBlockPoolHeader;
          LPNextMediumBlockPoolHeader.PreviousMediumBlockPoolHeader := LPPreviousMediumBlockPoolHeader;
          {Unlock medium blocks}
          MediumBlocksLocked := False;
          {Free the medium block pool}
          if VirtualFree(APointer, 0, MEM_RELEASE) then
            Result := 0
          else
            Result := -1;
        end;
      end;
{$endif}
    end
    else
    begin
      {Validate: Is this actually a Large block, or is it an attempt to free an
       already freed small block?}
      if LBlockHeader and (IsFreeBlockFlag or IsMediumBlockFlag) = 0 then
        Result := FreeLargeBlock(APointer)
      else
        Result := -1;
    end;
  end;
end;
{$else}
{Replacement for SysFreeMem (pascal version)}
function FastFreeMem(APointer: Pointer): Integer;
asm
  {Get the block header in edx}
  mov edx, [eax - 4]
  {Is it a small block in use?}
  test dl, IsFreeBlockFlag + IsMediumBlockFlag + IsLargeBlockFlag
  {Save the pointer in ecx}
  mov ecx, eax
  {Save ebx}
  push ebx
  {Get the IsMultiThread variable in bl}
{$ifndef AssumeMultiThreaded}
  mov bl, IsMultiThread
{$endif}
  {Is it a small block that is in use?}
  jnz @NotSmallBlockInUse
  {Do we need to lock the block type?}
{$ifndef AssumeMultiThreaded}
  test bl, bl
{$endif}
  {Get the small block type in ebx}
  mov ebx, TSmallBlockPoolHeader[edx].BlockType
  {Do we need to lock the block type?}
{$ifndef AssumeMultiThreaded}
  jnz @LockBlockTypeLoop
{$else}
  jmp @LockBlockTypeLoop
{$endif}
@GotLockOnSmallBlockType:
  {Current state: edx = @SmallBlockPoolHeader, ecx = APointer, ebx = @SmallBlockType}
  {Decrement the number of blocks in use}
  sub TSmallBlockPoolHeader[edx].BlocksInUse, 1
  {Get the old first free block}
  mov eax, TSmallBlockPoolHeader[edx].FirstFreeBlock
  {Is the pool now empty?}
  jz @PoolIsNowEmpty
  {Was the pool full?}
  test eax, eax
  {Store this as the new first free block}
  mov TSmallBlockPoolHeader[edx].FirstFreeBlock, ecx
  {Store the previous first free block as the block header}
  lea eax, [eax + IsFreeBlockFlag]
  mov [ecx - 4], eax
  {Insert the pool back into the linked list if it was full}
  jz @SmallPoolWasFull
  {All ok}
  xor eax, eax
  {Unlock the block type}
  mov TSmallBlockType[ebx].BlockTypeLocked, al
  {Restore registers}
  pop ebx
  {Done}
  db $f3
  ret
  {** Align branch target **}
@SmallPoolWasFull:
  {Insert this as the first partially free pool for the block size}
  mov ecx, TSmallBlockType[ebx].NextPartiallyFreePool
  mov TSmallBlockPoolHeader[edx].PreviousPartiallyFreePool, ebx
  mov TSmallBlockPoolHeader[edx].NextPartiallyFreePool, ecx
  mov TSmallBlockPoolHeader[ecx].PreviousPartiallyFreePool, edx
  mov TSmallBlockType[ebx].NextPartiallyFreePool, edx
  {Unlock the block type}
  mov TSmallBlockType[ebx].BlockTypeLocked, False
  {All ok}
  xor eax, eax
  {Restore registers}
  pop ebx
  {Done}
  db $F3
  ret
  {**Align branch target**}
  nop
@PoolIsNowEmpty:
  {Was this pool actually in the linked list of pools with space? If not, it
   can only be the sequential feed pool (it is the only pool that may contain
   only one block, i.e. other blocks have not been split off yet)}
  test eax, eax
  jz @IsSequentialFeedPool
  {Pool is now empty: Remove it from the linked list and free it}
  mov eax, TSmallBlockPoolHeader[edx].PreviousPartiallyFreePool
  mov ecx, TSmallBlockPoolHeader[edx].NextPartiallyFreePool
  {Remove this manager}
  mov TSmallBlockPoolHeader[eax].NextPartiallyFreePool, ecx
  mov TSmallBlockPoolHeader[ecx].PreviousPartiallyFreePool, eax
  {Zero out eax}
  xor eax, eax
  {Is this the sequential feed pool? If so, stop sequential feeding}
  cmp TSmallBlockType[ebx].CurrentSequentialFeedPool, edx
  jne @NotSequentialFeedPool
@IsSequentialFeedPool:
  mov TSmallBlockType[ebx].MaxSequentialFeedBlockAddress, eax
@NotSequentialFeedPool:
  {Unlock the block type}
  mov TSmallBlockType[ebx].BlockTypeLocked, al
  {Release this pool}
  mov eax, edx
  mov edx, [edx - 4]
{$ifndef AssumeMultiThreaded}
  mov bl, IsMultiThread
{$endif}
  jmp @FreeMediumBlock
  {**Align branch target**}
  db $66, $66
  nop
@LockBlockTypeLoop:
  mov eax, $100
  {Attempt to grab the block type}
  lock cmpxchg TSmallBlockType([ebx]).BlockTypeLocked, ah
  je @GotLockOnSmallBlockType
  {Couldn't grab the block type - sleep and try again}
  push ecx
  push edx
  push InitialSleepTime
  call Sleep
  pop edx
  pop ecx
  {Try again}
  mov eax, $100
  {Attempt to grab the block type}
  lock cmpxchg TSmallBlockType([ebx]).BlockTypeLocked, ah
  je @GotLockOnSmallBlockType
  {Couldn't grab the block type - sleep and try again}
  push ecx
  push edx
  push AdditionalSleepTime
  call Sleep
  pop edx
  pop ecx
  {Try again}
  jmp @LockBlockTypeLoop
  {*Align branch target*}
  db $66
  nop
  {---------------------Medium blocks------------------------------}
@LockMediumBlocks:
  mov eax, $100
  {Attempt to lock the medium blocks}
  lock cmpxchg MediumBlocksLocked, ah
  je @MediumBlocksLocked
  {Couldn't lock the medium blocks - sleep and try again}
  push InitialSleepTime
  call Sleep
  {Try again}
  mov eax, $100
  {Attempt to lock the medium blocks}
  lock cmpxchg MediumBlocksLocked, ah
  je @MediumBlocksLocked
  {Couldn't lock the medium blocks - sleep and try again}
  push AdditionalSleepTime
  call Sleep
  {Try again}
  jmp @LockMediumBlocks
  {**Align branch target here**}
  db $66
  nop
@NotSmallBlockInUse:
  {Not a small block in use: is it a medium or large block?}
  test dl, IsFreeBlockFlag + IsLargeBlockFlag
  jnz @NotASmallOrLargeBlock
@FreeMediumBlock:
  {Drop the flags}
  and edx, DropMediumAndLargeFlagsMask
  {Free the large block pointed to by eax, header in edx, bl = IsMultiThread}
{$ifndef AssumeMultiThreaded}
  {Do we need to lock the medium blocks?}
  test bl, bl
{$endif}
  {Block size in ebx}
  mov ebx, edx
  {Save registers}
  push esi
  {Pointer in esi}
  mov esi, eax
  {Do we need to lock the medium blocks?}
{$ifndef AssumeMultiThreaded}
  jnz @LockMediumBlocks
{$else}
  jmp @LockMediumBlocks
{$endif}
@MediumBlocksLocked:
  {Can we combine this block with the next free block?}
  test dword ptr [esi + ebx - 4], IsFreeBlockFlag
  {Get the next block size and flags in ecx}
  mov ecx, [esi + ebx - 4]
  jnz @NextBlockIsFree
  {Set the "PreviousIsFree" flag in the next block}
  or ecx, PreviousMediumBlockIsFreeFlag
  mov [esi + ebx - 4], ecx
@NextBlockChecked:
  {Can we combine this block with the previous free block? We need to
   re-read the flags since it could have changed before we could lock the
   medium blocks.}
  test byte ptr [esi - 4], PreviousMediumBlockIsFreeFlag
  jnz @PreviousBlockIsFree
@PreviousBlockChecked:
  {Is the entire medium block pool free, and there are other free blocks
   that can fit the largest possible medium block -> free it.}
  cmp ebx, (MediumBlockPoolSize - MediumBlockPoolHeaderSize)
  je @EntireMediumPoolFree
@BinFreeMediumBlock:
  {Store the size of the block as well as the flags}
  lea eax, [ebx + IsMediumBlockFlag + IsFreeBlockFlag]
  mov [esi - 4], eax
  {Store the trailing size marker}
  mov [esi + ebx - 8], ebx
  {Insert this block back into the bins: Size check not required here,
   since medium blocks that are in use are not allowed to be
   shrunk smaller than MinimumMediumBlockSize}
  mov eax, esi
  mov edx, ebx
  {Insert into bin}
  call InsertMediumBlockIntoBin
  {Unlock medium blocks}
  mov MediumBlocksLocked, False;
  {All OK}
  xor eax, eax
  {Restore registers}
  pop esi
  pop ebx
  {Return}
  ret
  {**Align branch target here**}
  nop
@NextBlockIsFree:
  {Get the next block address in eax}
  lea eax, [esi + ebx]
  {Increase the size of this block}
  and ecx, DropMediumAndLargeFlagsMask
  add ebx, ecx
  {Was the block binned?}
  cmp ecx, MinimumMediumBlockSize
  jb @NextBlockChecked
  call RemoveMediumFreeBlock
  jmp @NextBlockChecked
  {**Align branch target here**}
  nop
@PreviousBlockIsFree:
  {Get the size of the free block just before this one}
  mov ecx, [esi - 8]
  {Include the previous block}
  sub esi, ecx
  {Set the new block size}
  add ebx, ecx
  {Remove the previous block from the linked list}
  cmp ecx, MinimumMediumBlockSize
  jb @PreviousBlockChecked
  mov eax, esi
  call RemoveMediumFreeBlock
  jmp @PreviousBlockChecked
  {**Align branch target here**}
@EntireMediumPoolFree:
  {Should we make this the new sequential feed medium block pool? If the
   current sequential feed pool is not entirely free, we make this the new
   sequential feed pool.}
  cmp MediumSequentialFeedBytesLeft, MediumBlockPoolSize - MediumBlockPoolHeaderSize
  jne @MakeEmptyMediumPoolSequentialFeed
  {Point esi to the medium block pool header}
  sub esi, MediumBlockPoolHeaderSize
  {Remove this medium block pool from the linked list}
  mov eax, TMediumBlockPoolHeader[esi].PreviousMediumBlockPoolHeader
  mov edx, TMediumBlockPoolHeader[esi].NextMediumBlockPoolHeader
  mov TMediumBlockPoolHeader[eax].NextMediumBlockPoolHeader, edx
  mov TMediumBlockPoolHeader[edx].PreviousMediumBlockPoolHeader, eax
  {Unlock medium blocks}
  mov MediumBlocksLocked, False;
  {Free the medium block pool}
  push MEM_RELEASE
  push 0
  push esi
  call VirtualFree
  {VirtualFree returns >0 if all is ok}
  cmp eax, 1
  {Return 0 on all ok}
  sbb eax, eax
  {Restore registers}
  pop esi
  pop ebx
  db $F3
  ret
  {**Align branch target here**}
  db $66
  nop
@MakeEmptyMediumPoolSequentialFeed:
  {Get a pointer to the end-marker block}
  lea ebx, [esi + MediumBlockPoolSize - MediumBlockPoolHeaderSize]
  {Bin the current sequential feed pool}
  call BinMediumSequentialFeedRemainder
  {Set this medium pool up as the new sequential feed pool:
   Store the sequential feed pool trailer}
  mov dword ptr [ebx - BlockHeaderSize], IsMediumBlockFlag
  {Store the number of bytes available in the sequential feed chunk}
  mov MediumSequentialFeedBytesLeft, MediumBlockPoolSize - MediumBlockPoolHeaderSize
  {Set the last sequentially fed block}
  mov LastSequentiallyFedMediumBlock, ebx
  {Unlock medium blocks}
  mov MediumBlocksLocked, False;
  {Success}
  xor eax, eax
  {Restore registers}
  pop esi
  pop ebx
  db $F3
  ret
  {**Align branch target here**}
  nop
@NotASmallOrLargeBlock:
  {Restore ebx}
  pop ebx
  {Is it in fact a large block?}
  test dl, IsFreeBlockFlag + IsMediumBlockFlag
  jz FreeLargeBlock
  {Attempt to free an already free block}
  mov eax, -1
  db $f3
end;
{$endif}

{$ifndef ASMVersion}
{Replacement for SysReallocMem (pascal version)}
function FastReallocMem(APointer: Pointer; ANewSize: Integer): Pointer;
var
  LBlockHeader, LBlockFlags, LOldAvailableSize, LNewAllocSize,
    LNextBlockSizeAndFlags, LNextBlockSize, LNewAvailableSize,
    LMinimumUpsize, LOldUserSize, LSecondSPlitSize, LNewBlockSize: Cardinal;
  LPSmallBlockType: PSmallBlockType;
  LPNextBlock, LPNextBlockHeader: Pointer;

  {Upsizes a large block in-place. The following variables are assumed correct:
    LBlockFlags, LOldAvailableSize, LPNextBlock, LNextBlockSizeAndFlags,
    LNextBlockSize, LNewAvailableSize. Medium blocks must be locked on entry if
    required.}
  procedure MediumBlockInPlaceUpsize;
  begin
    {Remove the next block}
    if LNextBlockSizeAndFlags >= MinimumMediumBlockSize then
      RemoveMediumFreeBlock(LPNextBlock);
    {Add 25% for medium block in-place upsizes}
    LMinimumUpsize := LOldAvailableSize + (LOldAvailableSize shr 2);
    if Cardinal(ANewSize) < LMinimumUpsize then
      LNewAllocSize := LMinimumUpsize
    else
      LNewAllocSize := ANewSize;
    {Round up to the nearest block size granularity}
    LNewBlockSize := ((LNewAllocSize + (BlockHeaderSize + MediumBlockGranularity - 1 - MediumBlockSizeOffset)) and -MediumBlockGranularity) + MediumBlockSizeOffset;
    {Calculate the size of the second split}
    LSecondSplitSize := LNewAvailableSize + BlockHeaderSize - LNewBlockSize;
    {Does it fit?}
    if Integer(LSecondSplitSize) <= 0 then
    begin
      {The block size is the full available size plus header}
      LNewBlockSize := LNewAvailableSize + BlockHeaderSize;
      {Grab the whole block: Mark it as used in the block following it}
      LPNextBlockHeader := Pointer(Cardinal(APointer) + LNewAvailableSize);
      PCardinal(LPNextBlockHeader)^ :=
        PCardinal(LPNextBlockHeader)^ and (not PreviousMediumBlockIsFreeFlag);
    end
    else
    begin
      {Split the block in two}
      LPNextBlock := PMediumFreeBlock(Cardinal(APointer) + LNewBlockSize);
      {Set the size of the second split}
      PCardinal(Cardinal(LPNextBlock) - BlockHeaderSize)^ := LSecondSplitSize or (IsMediumBlockFlag or IsFreeBlockFlag);
      {Store the size of the second split as the second last dword}
      PCardinal(Cardinal(LPNextBlock) + LSecondSplitSize - 8)^ := LSecondSplitSize;
      {Put the remainder in a bin if it is big enough}
      if LSecondSplitSize >= MinimumMediumBlockSize then
        InsertMediumBlockIntoBin(LPNextBlock, LSecondSplitSize);
    end;
    {Set the size and flags for this block}
    PCardinal(Cardinal(APointer) - BlockHeaderSize)^ := LNewBlockSize or LBlockFlags;
  end;

  {In-place downsize of a medium block. On entry ANewSize must be less than half
   of LOldAvailableSize.}
  procedure MediumBlockInPlaceDownsize;
  begin
    {Round up to the next medium block size}
    LNewBlockSize := ((ANewSize + (BlockHeaderSize + MediumBlockGranularity - 1 - MediumBlockSizeOffset)) and -MediumBlockGranularity) + MediumBlockSizeOffset;
    {Get the size of the second split}
    LSecondSplitSize := (LOldAvailableSize + BlockHeaderSize) - LNewBlockSize;
    {Lock the medium blocks}
    LockMediumBlocks;
    {Set the new size}
    PCardinal(Cardinal(APointer) - BlockHeaderSize)^ :=
      (PCardinal(Cardinal(APointer) - BlockHeaderSize)^ and ExtractMediumAndLargeFlagsMask)
      or LNewBlockSize;
    {Is the next block in use?}
    LPNextBlock := PCardinal(Cardinal(APointer) + LOldAvailableSize + BlockHeaderSize);
    LNextBlockSizeAndFlags := PCardinal(Cardinal(LPNextBlock) - BlockHeaderSize)^;
    if LNextBlockSizeAndFlags and IsFreeBlockFlag = 0 then
    begin
      {The next block is in use: flag its previous block as free}
      PCardinal(Cardinal(LPNextBlock) - BlockHeaderSize)^ :=
        LNextBlockSizeAndFlags or PreviousMediumBlockIsFreeFlag;
    end
    else
    begin
      {The next block is free: combine it}
      LNextBlockSizeAndFlags := LNextBlockSizeAndFlags and DropMediumAndLargeFlagsMask;
      Inc(LSecondSplitSize, LNextBlockSizeAndFlags);
      if LNextBlockSizeAndFlags >= MinimumMediumBlockSize then
        RemoveMediumFreeBlock(LPNextBlock);
    end;
    {Set the split}
    LPNextBlock := PCardinal(Cardinal(APointer) + LNewBlockSize);
    {Store the free part's header}
    PCardinal(Cardinal(LPNextBlock) - BlockHeaderSize)^ := LSecondSplitSize or (IsMediumBlockFlag or IsFreeBlockFlag);
    {Store the trailing size field}
    PCardinal(Cardinal(LPNextBlock) + LSecondSplitSize - 8)^ := LSecondSplitSize;
    {Bin this free block}
    if LSecondSplitSize >= MinimumMediumBlockSize then
      InsertMediumBlockIntoBin(LPNextBlock, LSecondSplitSize);
    {Unlock the medium blocks}
    MediumBlocksLocked := False;
  end;

begin
  {Get the block header: Is it actually a small block?}
  LBlockHeader := PCardinal(Cardinal(APointer) - BlockHeaderSize)^;
  {Is it a small block that is in use?}
  if LBlockHeader and (IsFreeBlockFlag or IsMediumBlockFlag or IsLargeBlockFlag) = 0 then
  begin
    {-----------------------------------Small block-------------------------------------}
    {The block header is a pointer to the block pool: Get the block type}
    LPSmallBlockType := PSmallBlockPoolHeader(LBlockHeader).BlockType;

    {Get the available size inside blocks of this type.}
    LOldAvailableSize := LPSmallBlockType.BlockSize - BlockHeaderSize;
    {Is it an upsize or a downsize?}
    if LOldAvailableSize >= Cardinal(ANewSize) then
    begin
      {It's a downsize. Do we need to allocate a smaller block? Only if the new
       block size is less than a quarter of the available size less
       SmallBlockDownsizeCheckAdder bytes}
      if (Cardinal(ANewSize) * 4 + SmallBlockDownsizeCheckAdder) >= LOldAvailableSize then
      begin
        {In-place downsize - return the pointer}
        Result := APointer;
        exit;
      end
      else
      begin
        {Allocate a smaller block}
        Result := FastGetMem(ANewSize);
        {Allocated OK?}
        if Result <> nil then
        begin
          {Move the data across}
{$ifdef UseCustomVariableSizeMoveRoutines}
  {$ifdef Align16Bytes}
          MoveX16L4(APointer^, Result^, ANewSize);
  {$else}
          MoveX8L4(APointer^, Result^, ANewSize);
  {$endif}
{$else}
          System.Move(APointer^, Result^, ANewSize);
{$endif}
          {Free the old pointer}
          FastFreeMem(APointer);
        end;
      end;
    end
    else
    begin
      {This pointer is being reallocated to a larger block and therefore it is
       logical to assume that it may be enlarged again. Since reallocations are
       expensive, there is a minimum upsize percentage to avoid unnecessary
       future move operations.}
      {Must grow with at least 100% + x bytes}
      LNewAllocSize := LOldAvailableSize * 2 + SmallBlockUpsizeAdder;
      {Still not large enough?}
      if LNewAllocSize < Cardinal(ANewSize) then
        LNewAllocSize := ANewSize;
      {Allocate the new block}
      Result := FastGetMem(LNewAllocSize);
      {Allocated OK?}
      if Result <> nil then
      begin
        {Do we need to store the requested size? Only large blocks store the
         requested size.}
        if LNewAllocSize > (MaximumMediumBlockSize - BlockHeaderSize) then
          PLargeBlockHeader(Cardinal(Result) - LargeBlockHeaderSize).UserAllocatedSize := ANewSize;
        {Move the data across}
{$ifdef UseCustomFixedSizeMoveRoutines}
        LPSmallBlockType.UpsizeMoveProcedure(APointer^, Result^, LOldAvailableSize);
{$else}
        System.Move(APointer^, Result^, LOldAvailableSize);
{$endif}
        {Free the old pointer}
        FastFreeMem(APointer);
      end;
    end;
  end
  else
  begin
    {Is this a medium block or a large block?}
    if LBlockHeader and (IsFreeBlockFlag or IsLargeBlockFlag) = 0 then
    begin
      {-------------------------------Medium block--------------------------------------}
      {What is the available size in the block being reallocated?}
      LOldAvailableSize := (LBlockHeader and DropMediumAndLargeFlagsMask);
      {Get a pointer to the next block}
      LPNextBlock := PCardinal(Cardinal(APointer) + LOldAvailableSize);
      {Subtract the block header size from the old available size}
      Dec(LOldAvailableSize, BlockHeaderSize);
      {Is it an upsize or a downsize?}
      if Cardinal(ANewSize) > LOldAvailableSize then
      begin
        {Can we do an in-place upsize?}
        LNextBlockSizeAndFlags := PCardinal(Cardinal(LPNextBlock) - BlockHeaderSize)^;
        {Is the next block free?}
        if LNextBlockSizeAndFlags and IsFreeBlockFlag <> 0 then
        begin
          LNextBlockSize := LNextBlockSizeAndFlags and DropMediumAndLargeFlagsMask;
          {The available size including the next block}
          LNewAvailableSize := LOldAvailableSize + LNextBlockSize;
          {Can the block fit?}
          if Cardinal(ANewSize) <= LNewAvailableSize then
          begin
            {The next block is free and there is enough space to grow this
             block in place.}
{$ifndef AssumeMultiThreaded}
            if IsMultiThread then
            begin
{$endif}
              {Multi-threaded application - lock medium blocks and re-read the
               information on the blocks.}
              LockMediumBlocks;
              {Re-read the info for this block}
              LBlockFlags := PCardinal(Cardinal(APointer) - BlockHeaderSize)^ and ExtractMediumAndLargeFlagsMask;
              {Re-read the info for the next block}
              LNextBlockSizeAndFlags := PCardinal(Cardinal(LPNextBlock) - BlockHeaderSize)^;
              {Recalculate the next block size}
              LNextBlockSize := LNextBlockSizeAndFlags and DropMediumAndLargeFlagsMask;
              {The available size including the next block}
              LNewAvailableSize := LOldAvailableSize + LNextBlockSize;
              {Is the next block still free and the size still sufficient?}
              if (LNextBlockSizeAndFlags and IsFreeBlockFlag <> 0)
                and (Cardinal(ANewSize) <= LNewAvailableSize) then
              begin
                {Upsize the block in-place}
                MediumBlockInPlaceUpsize;
                {Unlock the medium blocks}
                MediumBlocksLocked := False;
                {Return the result}
                Result := APointer;
                {Done}
                exit;
              end;
              {Couldn't use the block: Unlock the medium blocks}
              MediumBlocksLocked := False;
{$ifndef AssumeMultiThreaded}
            end
            else
            begin
              {Extract the block flags}
              LBlockFlags := ExtractMediumAndLargeFlagsMask and LBlockHeader;
              {Upsize the block in-place}
              MediumBlockInPlaceUpsize;
              {Return the result}
              Result := APointer;
              {Done}
              exit;
            end;
{$endif}
          end;
        end;
        {Couldn't upsize in place. Grab a new block and move the data across:
         If we have to reallocate and move medium blocks, we grow by at
         least 25%}
        LMinimumUpsize := LOldAvailableSize + (LOldAvailableSize shr 2);
        if Cardinal(ANewSize) < LMinimumUpsize then
          LNewAllocSize := LMinimumUpsize
        else
          LNewAllocSize := ANewSize;
        {Allocate the new block}
        Result := FastGetMem(LNewAllocSize);
        if Result <> nil then
        begin
          {If its a Large block - store the actual user requested size}
          if LNewAllocSize > (MaximumMediumBlockSize - BlockHeaderSize) then
            PLargeBlockHeader(Cardinal(Result) - LargeBlockHeaderSize).UserAllocatedSize := ANewSize;
          {Move the data across}
{$ifdef UseCustomVariableSizeMoveRoutines}
          MoveX16L4(APointer^, Result^, LOldAvailableSize);
{$else}
          System.Move(APointer^, Result^, LOldAvailableSize);
{$endif}
          {Free the old block}
          FastFreeMem(APointer);
        end;
      end
      else
      begin
        {Must be less than half the current size or we don't bother resizing.}
        if Cardinal(ANewSize * 2) >= LOldAvailableSize then
        begin
          Result := APointer;
        end
        else
        begin
          {In-place downsize? Balance the cost of moving the data vs. the cost of
           fragmenting the memory pool. Medium blocks in use may never be smaller
           than MinimumMediumBlockSize.}
          if ANewSize >= (MinimumMediumBlockSize - BlockHeaderSize) then
          begin
            MediumBlockInPlaceDownsize;
            Result := APointer;
          end
          else
          begin
            {The requested size is less than the minimum medium block size. If
             the requested size is less than half of the minimum -> Allocate a small
             block and move, otherwise downsize to the minimum medium block size.}
            if Cardinal(ANewSize * 2) > (MinimumMediumBlockSize - BlockHeaderSize) then
            begin
              {Resize to the minimum medium block size}
              ANewSize := MinimumMediumBlockSize - BlockHeaderSize;
              MediumBlockInPlaceDownsize;
              Result := APointer;
            end
            else
            begin
              {Allocate the new block}
              Result := FastGetMem(ANewSize);
              if Result <> nil then
              begin
                {Move the data across}
{$ifdef UseCustomVariableSizeMoveRoutines}
  {$ifdef Align16Bytes}
                MoveX16L4(APointer^, Result^, ANewSize);
  {$else}
                MoveX8L4(APointer^, Result^, ANewSize);
  {$endif}
{$else}
                System.Move(APointer^, Result^, ANewSize);
{$endif}
                {Free the old block}
                FastFreeMem(APointer);
              end;
            end;
          end;
        end;
      end;
    end
    else
    begin
      {Is this a valid large block?}
      if LBlockHeader and (IsFreeBlockFlag or IsMediumBlockFlag) = 0 then
      begin
        {-----------------------Large block------------------------------}
        {Large block - size is (16 + 4) less than the allocated size}
        LOldAvailableSize := LBlockHeader - (LargeBlockHeaderSize + BlockHeaderSize + IsLargeBlockFlag);
        {The user allocated size is stored for Large blocks}
        LOldUserSize := PLargeBlockHeader(Cardinal(APointer) - LargeBlockHeaderSize).UserAllocatedSize;
        {Is it an upsize or a downsize?}
        if Cardinal(ANewSize) > LOldAvailableSize then
        begin
          {This pointer is being reallocated to a larger block and therefore it is
           logical to assume that it may be enlarged again. Since reallocations are
           expensive, there is a minimum upsize percentage to avoid unnecessary
           future move operations.}
          {Add 25% for large block upsizes}
          LMinimumUpsize := Cardinal(LOldAvailableSize)
            + (Cardinal(LOldAvailableSize) shr 2);
          if Cardinal(ANewSize) < LMinimumUpsize then
            LNewAllocSize := LMinimumUpsize
          else
            LNewAllocSize := ANewSize;
          {Allocate the new block}
          Result := FastGetMem(LNewAllocSize);
          if Result <> nil then
          begin
            {If its a large block - store the actual user requested size (it may
             not be if the block that is being reallocated from was previously
             downsized)}
            if LNewAllocSize > (MaximumMediumBlockSize - BlockHeaderSize) then
              PLargeBlockHeader(Cardinal(Result) - LargeBlockHeaderSize).UserAllocatedSize := ANewSize;
            {The number of bytes to move is the old user size.}
{$ifdef UseCustomVariableSizeMoveRoutines}
            MoveX16L4(APointer^, Result^, LOldUserSize);
{$else}
            System.Move(APointer^, Result^, LOldUserSize);
{$endif}
            {Free the old block}
            FastFreeMem(APointer);
          end;
        end
        else
        begin
          {It's a downsize: do we need to reallocate? Only if the new size is less
           than a quarter of the old size}
          if Cardinal(ANewSize) >= (LOldAvailableSize shr 2) then
          begin
            {No need to reallocate}
            Result := APointer;
            {Update the requested size}
            PLargeBlockHeader(Cardinal(APointer) - LargeBlockHeaderSize).UserAllocatedSize := ANewSize;
          end
          else
          begin
            {The block is less than a quarter of the old size, and the current size is
             greater than the minimum block size allowing a downsize: reallocate}
            Result := FastGetMem(ANewSize);
            if Result <> nil then
            begin
              {Still a large block? -> Set the user size}
              if ANewSize > (MaximumMediumBlockSize - BlockHeaderSize) then
                PLargeBlockHeader(Cardinal(APointer) - LargeBlockHeaderSize).UserAllocatedSize := ANewSize;
              {Move the data across}
{$ifdef UseCustomVariableSizeMoveRoutines}
  {$ifdef Align16Bytes}
              MoveX16L4(APointer^, Result^, ANewSize);
  {$else}
              MoveX8L4(APointer^, Result^, ANewSize);
  {$endif}
{$else}
              System.Move(APointer^, Result^, ANewSize);
{$endif}
              {Free the old block}
              FastFreeMem(APointer);
            end;
          end;
        end;
      end
      else
      begin
        {-----------------------Invalid block------------------------------}
        {Bad pointer: probable attempt to reallocate a free memory block.}
        Result := nil;
      end;
    end;
  end;
end;
{$else}
{Replacement for SysReallocMem (asm version)}
function FastReallocMem(APointer: Pointer; ANewSize: Integer): Pointer;
asm
  {On entry: eax = APointer; edx = ANewSize}
  {Get the block header: Is it actually a small block?}
  mov ecx, [eax - 4]
  {Is it a small block?}
  test cl, IsFreeBlockFlag + IsMediumBlockFlag + IsLargeBlockFlag
  {Save ebx}
  push ebx
  {Save esi}
  push esi
  {Save the original pointer in esi}
  mov esi, eax
  {Is it a small block?}
  jnz @NotASmallBlock
  {-----------------------------------Small block-------------------------------------}
  {Get the block type in ebx}
  mov ebx, TSmallBlockPoolHeader[ecx].BlockType
  {Get the available size inside blocks of this type.}
  movzx ecx, TSmallBlockType[ebx].BlockSize
  sub ecx, 4
  {Is it an upsize or a downsize?}
  cmp ecx, edx
  jb @SmallUpsize
  {It's a downsize. Do we need to allocate a smaller block? Only if the new
   size is less than a quarter of the available size less
   SmallBlockDownsizeCheckAdder bytes}
  lea ebx, [edx * 4 + SmallBlockDownsizeCheckAdder]
  cmp ebx, ecx
  jb @NotSmallInPlaceDownsize
  {In-place downsize - return the original pointer}
  pop esi
  pop ebx
  db $f3
  ret
  {**align branch target**}
@NotSmallInPlaceDownsize:
  {Save the requested size}
  mov ebx, edx
  {Allocate a smaller block}
  mov eax, edx
  call FastGetMem
  {Allocated OK?}
  test eax, eax
  jz @SmallDownsizeDone
  {Move data across: count in ecx}
  mov ecx, ebx
  {Destination in edx}
  mov edx, eax
  {Save the result in ebx}
  mov ebx, eax
  {Original pointer in eax}
  mov eax, esi
  {Move the data across}
{$ifdef UseCustomVariableSizeMoveRoutines}
  {$ifdef Align16Bytes}
  call MoveX16L4
  {$else}
  call MoveX8L4
  {$endif}
{$else}
  call System.Move
{$endif}
  {Free the original pointer}
  mov eax, esi
  call FastFreeMem
  {Return the pointer}
  mov eax, ebx
@SmallDownsizeDone:
  pop esi
  pop ebx
  db $f3
  ret
  {**align branch target**}
  nop
@SmallUpsize:
  {State: esi = APointer, edx = ANewSize, ecx = Current Block Size, ebx = Current Block Type}
  {This pointer is being reallocated to a larger block and therefore it is
   logical to assume that it may be enlarged again. Since reallocations are
   expensive, there is a minimum upsize percentage to avoid unnecessary
   future move operations.}
  {Small blocks always grow with at least 100% + SmallBlockUpsizeAdder bytes}
  lea ecx, [ecx + ecx + SmallBlockUpsizeAdder]
  {save edi}
  push edi
  {Save the requested size in edi}
  mov edi, edx
  {New allocated size is the maximum of the requested size and the minimum
   upsize}
  xor eax, eax
  sub ecx, edx
  adc eax, -1
  and eax, ecx
  add eax, edx
  {Allocate the new block}
  call FastGetMem
  {Allocated OK?}
  test eax, eax
  jz @SmallUpsizeDone
  {Do we need to store the requested size? Only large blocks store the
   requested size.}
  cmp edi, MaximumMediumBlockSize - BlockHeaderSize
  jbe @NotSmallUpsizeToLargeBlock
  {Store the user requested size}
  mov [eax - 8], edi
@NotSmallUpsizeToLargeBlock:
  {Get the size to move across}
  movzx ecx, TSmallBlockType[ebx].BlockSize
  sub ecx, BlockHeaderSize
  {Move to the new block}
  mov edx, eax
  {Save the result in edi}
  mov edi, eax
  {Move from the old block}
  mov eax, esi
  {Move the data across}
{$ifdef UseCustomFixedSizeMoveRoutines}
  call TSmallBlockType[ebx].UpsizeMoveProcedure
{$else}
  call System.Move
{$endif}
  {Free the old pointer}
  mov eax, esi
  call FastFreeMem
  {Done}
  mov eax, edi
@SmallUpsizeDone:
  pop edi
  pop esi
  pop ebx
  db $f3
  ret
  {**align branch target**}
@NotASmallBlock:
  {Is this a medium block or a large block?}
  test cl, IsFreeBlockFlag + IsLargeBlockFlag
  jnz @PossibleLargeBlock
  {-------------------------------Medium block--------------------------------------}
  {Status: ecx = Current Block Size + Flags, eax/esi = APointer,
   edx = Requested Size}
  mov ebx, ecx
  {Drop the flags from the header}
  and ecx, DropMediumAndLargeFlagsMask
  {Save edi}
  push edi
  {Get a pointer to the next block in edi}
  lea edi, [eax + ecx]
  {Subtract the block header size from the old available size}
  sub ecx, BlockHeaderSize
  {Get the complete flags in ebx}
  and ebx, ExtractMediumAndLargeFlagsMask
  {Is it an upsize or a downsize?}
  cmp edx, ecx
  {Save ebp}
  push ebp
  {Is it an upsize or a downsize?}
  ja @MediumBlockUpsize
  {Status: ecx = Current Block Size - 4, bl = Current Block Flags,
   edi = @Next Block, eax/esi = APointer, edx = Requested Size}
  {Must be less than half the current size or we don't bother resizing.}
  lea ebp, [edx + edx]
  cmp ebp, ecx
  jb @MediumMustDownsize
  {Restore registers}
  pop ebp
  pop edi
  pop esi
  pop ebx
  {Return}
  db $f3
  ret
  {**Align branch target**}
  db $66
  nop
@MediumMustDownsize:
  {In-place downsize? Balance the cost of moving the data vs. the cost of
   fragmenting the memory pool. Medium blocks in use may never be smaller
   than MinimumMediumBlockSize.}
  cmp edx, MinimumMediumBlockSize - BlockHeaderSize
  jae @MediumBlockInPlaceDownsize
  {The requested size is less than the minimum medium block size. If
   the requested size is less than half of the minimum -> Allocate a small
   block and move, otherwise downsize to the minimum medium block size.}
  cmp ebp, MinimumMediumBlockSize - BlockHeaderSize
  jb @MediumDownsizeRealloc
  {Resize to the minimum medium block size}
  mov edx, MinimumMediumBlockSize - BlockHeaderSize
@MediumBlockInPlaceDownsize:
  {Round up to the next medium block size}
  lea ebp, [edx + BlockHeaderSize + MediumBlockGranularity - 1 - MediumBlockSizeOffset]
  and ebp, -MediumBlockGranularity;
  add ebp, MediumBlockSizeOffset
  {Get the size of the second split}
  add ecx, BlockHeaderSize
  sub ecx, ebp
  {Lock the medium blocks}
{$ifndef AssumeMultiThreaded}
  cmp IsMultiThread, False
  je @DoMediumInPlaceDownsize
{$endif}
  {We have to re-read the flags}
@DoMediumLockForDownsize:
  {Lock the medium blocks}
  mov eax, $100
  {Attempt to lock the medium blocks}
  lock cmpxchg MediumBlocksLocked, ah
  je @MediumDownsizeRereadFlags
  {Couldn't lock the medium blocks - sleep and try again}
  push ecx
  push InitialSleepTime
  call Sleep
  pop ecx
  {Try again}
  mov eax, $100
  {Attempt to grab the block type}
  lock cmpxchg MediumBlocksLocked, ah
  je @MediumDownsizeRereadFlags
  {Couldn't lock the medium blocks - sleep and try again}
  push ecx
  push AdditionalSleepTime
  call Sleep
  pop ecx
  {Try again}
  jmp @DoMediumLockForDownsize
  {**Align branch target**}
@MediumDownsizeRereadFlags:
  mov ebx, ExtractMediumAndLargeFlagsMask
  and ebx, [esi - 4]
@DoMediumInPlaceDownsize:
  {Set the new size}
  or ebx, ebp
  mov [esi - 4], ebx
  {Get the second split size in ebx}
  mov ebx, ecx
  {Is the next block in use?}
  mov edx, [edi - 4]
  test dl, IsFreeBlockFlag
  jnz @MediumDownsizeNextBlockFree
  {The next block is in use: flag its previous block as free}
  or edx, PreviousMediumBlockIsFreeFlag
  mov [edi - 4], edx
  jmp @MediumDownsizeDoSplit
  {**Align branch target**}
  nop
@MediumDownsizeNextBlockFree:
  {The next block is free: combine it}
  mov eax, edi
  and edx, DropMediumAndLargeFlagsMask
  add ebx, edx
  add edi, edx
  cmp edx, MinimumMediumBlockSize
  jb @MediumDownsizeDoSplit
  call RemoveMediumFreeBlock
@MediumDownsizeDoSplit:
  {Store the trailing size field}
  mov [edi - 8], ebx
  {Store the free part's header}
  lea eax, [ebx + IsMediumBlockFlag + IsFreeBlockFlag];
  mov [esi + ebp - 4], eax
  {Bin this free block}
  cmp ebx, MinimumMediumBlockSize
  jb @MediumBlockDownsizeDone
  lea eax, [esi + ebp]
  mov edx, ebx
  call InsertMediumBlockIntoBin
@MediumBlockDownsizeDone:
  {Unlock the medium blocks}
  mov MediumBlocksLocked, False
  {Result = old pointer}
  mov eax, esi
  {Restore registers}
  pop ebp
  pop edi
  pop esi
  pop ebx
  {Return}
  ret
  {**Align branch target**}
@MediumDownsizeRealloc:
  {Save the requested size}
  mov edi, edx
  mov eax, edx
  {Allocate the new block}
  call FastGetMem
  test eax, eax
  jz @MediumBlockDownsizeExit
  {Save the result}
  mov ebp, eax
  mov edx, eax
  mov eax, esi
  mov ecx, edi
  {Move the data across}
{$ifdef UseCustomVariableSizeMoveRoutines}
  {$ifdef Align16Bytes}
  call MoveX16L4
  {$else}
  call MoveX8L4
  {$endif}
{$else}
  call System.Move
{$endif}
  mov eax, esi
  call FastFreeMem
  {Return the result}
  mov eax, ebp
@MediumBlockDownsizeExit:
  pop ebp
  pop edi
  pop esi
  pop ebx
  ret
  {**align branch target**}
@MediumBlockUpsize:
  {Status: ecx = Current Block Size - 4, bl = Current Block Flags,
   edi = @Next Block, eax/esi = APointer, edx = Requested Size}
  {Can we do an in-place upsize?}
  mov eax, [edi - 4]
  test al, IsFreeBlockFlag
  jz @CannotUpsizeMediumBlockInPlace
  {Get the total available size including the next block}
  and eax, DropMediumAndLargeFlagsMask
  {ebp = total available size including the next block (excluding the header)}
  lea ebp, [eax + ecx]
  {Can the block fit?}
  cmp edx, ebp
  ja @CannotUpsizeMediumBlockInPlace
  {The next block is free and there is enough space to grow this
   block in place.}
{$ifndef AssumeMultiThreaded}
  cmp IsMultiThread, False
  je @DoMediumInPlaceUpsize
{$endif}
@DoMediumLockForUpsize:
  {Lock the medium blocks}
  mov eax, $100
  {Attempt to lock the medium blocks}
  lock cmpxchg MediumBlocksLocked, ah
  je @RecheckMediumInPlaceUpsize
  {Couldn't lock the medium blocks - sleep and try again}
  push ecx
  push edx
  push InitialSleepTime
  call Sleep
  pop edx
  pop ecx
  {Try again}
  mov eax, $100
  {Attempt to grab the block type}
  lock cmpxchg MediumBlocksLocked, ah
  je @RecheckMediumInPlaceUpsize
  {Couldn't lock the medium blocks - sleep and try again}
  push ecx
  push edx
  push AdditionalSleepTime
  call Sleep
  pop edx
  pop ecx
  {Try again}
  jmp @DoMediumLockForUpsize
  {**Align branch target**}
  nop
@RecheckMediumInPlaceUpsize:
  {Re-read the info for this block}
  mov ebx, ExtractMediumAndLargeFlagsMask
  and ebx, [esi - 4]
  {Re-read the info for the next block}
  mov eax, [edi - 4]
  {Next block still free?}
  test al, IsFreeBlockFlag
  jz @NextMediumBlockChanged
  {Recalculate the next block size}
  and eax, DropMediumAndLargeFlagsMask
  {The available size including the next block}
  lea ebp, [eax + ecx]
  {Can the block still fit?}
  cmp edx, ebp
  ja @NextMediumBlockChanged
@DoMediumInPlaceUpsize:
  {Is the next block binnable?}
  cmp eax, MinimumMediumBlockSize
  {Remove the next block}
  jb @MediumInPlaceNoNextRemove
  mov eax, edi
  push ecx
  push edx
  call RemoveMediumFreeBlock
  pop edx
  pop ecx
@MediumInPlaceNoNextRemove:
  {Medium blocks grow a minimum of 25% in in-place upsizes}
  mov eax, ecx
  shr eax, 2
  add eax, ecx
  {Get the maximum of the requested size and the minimum growth size}
  xor edi, edi
  sub eax, edx
  adc edi, -1
  and eax, edi
  {Round up to the nearest block size granularity}
  lea eax, [eax + edx + BlockHeaderSize + MediumBlockGranularity - 1 - MediumBlockSizeOffset]
  and eax, -MediumBlockGranularity
  add eax, MediumBlockSizeOffset
  {Calculate the size of the second split}
  lea edx, [ebp + BlockHeaderSize]
  sub edx, eax
  {Does it fit?}
  ja @MediumInPlaceUpsizeSplit
  {Grab the whole block: Mark it as used in the block following it}
  and dword ptr [esi + ebp], not PreviousMediumBlockIsFreeFlag
  {The block size is the full available size plus header}
  add ebp, 4
  {Upsize done}
  jmp @MediumUpsizeInPlaceDone
  {*Align branch target*}
  nop
@MediumInPlaceUpsizeSplit:
  {Store the size of the second split as the second last dword}
  mov [esi + ebp - 4], edx
  {Set the second split header}
  lea edi, [edx + IsMediumBlockFlag + IsFreeBlockFlag]
  mov [esi + eax - 4], edi
  mov ebp, eax
  cmp edx, MinimumMediumBlockSize
  jb @MediumUpsizeInPlaceDone
  add eax, esi
  call InsertMediumBlockIntoBin
@MediumUpsizeInPlaceDone:
  {Set the size and flags for this block}
  or ebp, ebx
  mov [esi - 4], ebp
  {Unlock the medium blocks}
  mov MediumBlocksLocked, False
  {Result = old pointer}
  mov eax, esi
@MediumBlockResizeDone2:
  {Restore registers}
  pop ebp
  pop edi
  pop esi
  pop ebx
  {Return}
  db $f3
  ret
  {**Align branch target for "@CannotUpsizeMediumBlockInPlace"**}
  nop
@NextMediumBlockChanged:
  {The next medium block changed while the medium blocks were being locked}
  mov MediumBlocksLocked, False
@CannotUpsizeMediumBlockInPlace:
  {Couldn't upsize in place. Grab a new block and move the data across:
   If we have to reallocate and move medium blocks, we grow by at
   least 25%}
  mov eax, ecx
  shr eax, 2
  add eax, ecx
  {Get the maximum of the requested size and the minimum growth size}
  xor edi, edi
  sub eax, edx
  adc edi, -1
  and eax, edi
  add eax, edx
  {Save the size to allocate}
  mov ebp, eax
  {Save the size to move across}
  mov edi, ecx
  {Get the block}
  push edx
  call FastGetMem
  pop edx
  {Success?}
  test eax, eax
  jz @MediumBlockResizeDone2
  {If it's a Large block - store the actual user requested size}
  cmp ebp, MaximumMediumBlockSize - BlockHeaderSize
  jbe @MediumUpsizeNotLarge
  mov [eax - 8], edx
@MediumUpsizeNotLarge:
  {Save the result}
  mov ebp, eax
  {Move the data across}
  mov edx, eax
  mov eax, esi
  mov ecx, edi
{$ifdef UseCustomVariableSizeMoveRoutines}
  call MoveX16L4
{$else}
  call System.Move
{$endif}
  {Free the old block}
  mov eax, esi
  call FastFreeMem
  {Restore the result}
  mov eax, ebp
  {Restore registers}
  pop ebp
  pop edi
  pop esi
  pop ebx
  {Return}
  db $f3
  ret
  {**Align branch target**}
@PossibleLargeBlock:
  {Is this a valid large block?}
  test cl, IsFreeBlockFlag + IsMediumBlockFlag
  jnz @BadBlock
  {-----------------------Large block------------------------------}
  {State: ecx = Block Size + Flags, eax/esi = APointer, edx = ANewSize}
  {Large block available size is (16 + 4) less than the allocated size: get in ecx}
  sub ecx, LargeBlockHeaderSize + BlockHeaderSize + IsLargeBlockFlag
  {Is it an upsize or a downsize?}
  cmp edx, ecx
  jbe @LargeDownsize
  {This pointer is being reallocated to a larger block and therefore it is
   logical to assume that it may be enlarged again. Since reallocations are
   expensive, there is a minimum upsize percentage to avoid unnecessary
   future move operations.}
  {Add 25% for large block upsizes}
  mov eax, ecx
  shr ecx, 2
  add ecx, eax
  {Get the maximum of the requested size and the minimum upsize}
  xor eax, eax
  sub ecx, edx
  adc eax, -1
  and eax, ecx
  add eax, edx
  {Save the allocated size}
  mov ebx, eax
  {Save the requested size}
  push edx
  {Allocate the new block}
  call FastGetMem
  {Restore requested size}
  pop edx
  {Allocation OK?}
  test eax, eax
  jz @LargeResizeDone
  {If its a large block - store the actual user requested size (it may
   not be if the block that is being reallocated from was previously
   downsized)}
  cmp ebx, MaximumMediumBlockSize - BlockHeaderSize
  jbe @LargeUpsizeNotLarge
  mov [eax - 8], edx
@LargeUpsizeNotLarge:
  {Get the number of bytes to move in ecx (the old user size)}
  mov ecx, [esi - 8]
  {Save the result in ebx}
  mov ebx, eax
  {New pointer in edx}
  mov edx, eax
  {Original pointer in eax}
  mov eax, esi
{$ifdef UseCustomVariableSizeMoveRoutines}
  call MoveX16L4
{$else}
  call System.Move
{$endif}
  {Free the old block}
  mov eax, esi
  call FastFreeMem
  {Return the new pointer}
  mov eax, ebx
@LargeResizeDone:
  {Restore registers}
  pop esi
  pop ebx
  {Done}
  ret
  {**Align branch target**}
@LargeDownsize:
  {It's a downsize: do we need to reallocate? Only if the new size is less
   than a quarter of the old size}
  shr ecx, 2
  cmp edx, ecx
  jb @LargeNotInPlaceDownsize
  {Store the new user size}
  mov [eax - 8], edx
  {Restore registers}
  pop esi
  pop ebx
  {Done}
  db $f3
  ret
  {**Align branch target**}
  db $66
  nop
@LargeNotInPlaceDownsize:
  {The block is less than a quarter of the old size, and the current size is
   greater than the minimum block size allowing a downsize: reallocate}
  {Save the requested size}
  mov ebx, edx
  {Get the new block}
  mov eax, edx
  call FastGetMem
  test eax, eax
  jz @LargeResizeDone
  {Still a large block? -> Set the user size}
  cmp ebx, MaximumMediumBlockSize - BlockHeaderSize
  jbe @LargeNotInPlaceNotALargeBlock
  mov [eax - 8], ebx
@LargeNotInPlaceNotALargeBlock:
  {Bytes to move = new size}
  mov ecx, ebx
  {Save the pointer}
  mov ebx, eax
  {Move to the new pointer}
  mov edx, eax
  {Move from the old pointer}
  mov eax, esi
{$ifdef UseCustomVariableSizeMoveRoutines}
  {$ifdef Align16Bytes}
  call MoveX16L4
  {$else}
  call MoveX8L4
  {$endif}
{$else}
  call System.Move
{$endif}
  {Free the old block}
  mov eax, esi
  call FastFreeMem
  {Return the new block}
  mov eax, ebx
  {Restore registers}
  pop esi
  pop ebx
  {Done}
  db $f3
  ret
  {**Align branch target**}
  db $66
  nop
@BadBlock:
  {-----------------------Invalid block------------------------------}
  xor eax, eax
  pop esi
  pop ebx
  db $f3
end;
{$endif}

{-----------------Full Debug Mode Memory Manager Interface--------------------}

{$ifdef FullDebugMode}

{Wrapper for the procedure inside the FastMM_DebugInfo.dll file.}
procedure GetDebugInfoForAddress(AAddress: Pointer;
  APDebugInfo: PChar; var ANumChars: integer);
begin
  if DebugInfoDllAvailable and (not Assigned(DebugInfoProc)) then
  begin
    {Load the DLL}
    DebugInfoDll := LoadLibrary(DebugInfoLibraryName);
    if DebugInfoDll <> 0 then
    begin
      {Find the exported function}
      DebugInfoProc := GetProcAddress(DebugInfoDLL, 'GetDebugInfoForAddress');
    end;
    {Debug info is not available}
    if not Assigned(DebugInfoProc) then
      DebugInfoDllAvailable := False;
  end;
  {Get the debug info}
  if DebugInfoDllAvailable then
  begin
    DebugInfoProc(AAddress, APDebugInfo, ANumChars);
  end
  else
    ANumChars := 0;
end;

{Sums all the dwords starting at the given address.}
function SumCardinals(AStartValue: Cardinal; APointer: PCardinal; ACount: Cardinal): Cardinal;
asm
  {On entry: eax = AStartValue, edx = APointer; ecx = ACount}
  add edx, ecx
  neg ecx
@AddLoop:
  add eax, [edx + ecx]
  add edx, 4
  js @AddLoop
end;

{Sums all the dwords starting at the given address for the fill pattern.
 Returns true if they are all valid}
function CheckFillPattern(APointer: PCardinal; ACount: Cardinal): boolean;
asm
  {On entry: eax = APointer; edx = ACount}
  add eax, edx
  neg edx
@CheckLoop:
  cmp dword ptr [eax + edx], DebugFillDWord
  jne @Done
  add edx, 4
  js @CheckLoop
@Done:
  sete al
  db $f3
end;

{Gets the available size inside a block}
function GetAvailableSpaceInBlock(APointer: PFullDebugBlockHeader): Cardinal;
var
  LBlockHeader: Cardinal;
  LPSmallBlockPool: PSmallBlockPoolHeader;
begin
  LBlockHeader := PCardinal(Cardinal(APointer) - 4)^;
  if LBlockHeader and (IsMediumBlockFlag or IsLargeBlockFlag) = 0 then
  begin
    LPSmallBlockPool := PSmallBlockPoolHeader(LBlockHeader and DropSmallFlagsMask);
    Result := LPSmallBlockPool.BlockType.BlockSize - BlockHeaderSize;
  end
  else
  begin
    Result := (LBlockHeader and DropMediumAndLargeFlagsMask) - BlockHeaderSize;
    if (LBlockHeader and IsLargeBlockFlag) <> 0 then
      Dec(Result, LargeBlockHeaderSize);
  end;
end;

{Calculates the checksum for the debug header. Adds all dwords in the debug
 header to the start address of the block.}
function CalculateHeaderCheckSum(APointer: PFullDebugBlockHeader): Cardinal;
begin
  Result := SumCardinals(Cardinal(APointer),
    PCardinal(Cardinal(APointer) + 8),
    SizeOf(TFullDebugBlockHeader) - 8 - 4);
end;

procedure UpdateHeaderAndFooterCheckSums(APointer: PFullDebugBlockHeader);
var
  LHeaderCheckSum: Cardinal;
begin
  LHeaderCheckSum := CalculateHeaderCheckSum(APointer);
  APointer.HeaderCheckSum := LHeaderCheckSum;
  PCardinal(Cardinal(APointer) + SizeOf(TFullDebugBlockHeader) + APointer.UserSize)^ := not LHeaderCheckSum;
end;

function LogStackTrace(AStackTrace: PStackTrace; ABuffer: PChar): PChar;
var
  LInd, LAddress: Cardinal;
  LNumChars: Integer;
begin
  Result := ABuffer;
  for LInd := 0 to StackTraceDepth - 1 do
  begin
    LAddress := AStackTrace[LInd];
    if LAddress = 0 then
      exit;
    Result^ := #13;
    Inc(Result);
    Result^ := #10;
    Inc(Result);
    Result := CardinalToHexBuf(LAddress, Result);
    LNumChars := 512;
    GetDebugInfoForAddress(Pointer(LAddress), Result, LNumChars);
    Inc(Result, LNumChars);
  end;
end;

function LogCurrentStackTrace(ASkipLevels: Cardinal; ABuffer: PChar): PChar;
var
  LCurrentStackTrace: TStackTrace;
begin
  {Get the current call stack}
  GetStackTrace(@LCurrentStackTrace[0], StackTraceDepth, ASkipLevels);
  {List it}
  Result := AppendStringToBuffer(CurrentStackTraceMsg, ABuffer, length(CurrentStackTraceMsg));
  Result := LogStackTrace(@LCurrentStackTrace, Result);
end;

function LogMemoryDump(APointer: PFullDebugBlockHeader; ABuffer: PChar): PChar;
var
  LByteNum, LVal: Cardinal;
  LDataPtr: PByte;
begin
  Result := AppendStringToBuffer(MemoryDumpMsg, ABuffer, Length(MemoryDumpMsg));
  Result := CardinalToHexBuf(Cardinal(APointer) + SizeOf(TFullDebugBlockHeader), Result);
  Result^ := ':';
  Inc(Result);
  {Add the bytes}
  LDataPtr := PByte(Cardinal(APointer) + SizeOf(TFullDebugBlockHeader));
  for LByteNum := 0 to 255 do
  begin
    if LByteNum and 31 = 0 then
    begin
      Result^ := #13;
      Inc(Result);
      Result^ := #10;
      Inc(Result);
    end
    else
    begin
      Result^ := ' ';
      Inc(Result);
    end;
    {Set the hex data}
    LVal := LDataPtr^;
    Result^ := HexTable[LVal shr 4];
    Inc(Result);
    Result^ := HexTable[LVal and $f];
    Inc(Result);
    {Next byte}
    Inc(LDataPtr);
  end;
end;

procedure LogBlockError(APointer: PFullDebugBlockHeader; AOperation: TBlockOperation; LHeaderValid, LFooterValid: Boolean);
var
  LMsgPtr: PChar;
  LErrorMessage: array[0..32767] of char;
  LClass: TClass;
  LClassName: ShortString;
begin
  {Display the error header and the operation type.}
  LMsgPtr := AppendStringToBuffer(ErrorMsgHeader, @LErrorMessage[0], Length(ErrorMsgHeader));
  case AOperation of
    boGetMem: LMsgPtr := AppendStringToBuffer(GetMemMsg, LMsgPtr, Length(GetMemMsg));
    boFreeMem: LMsgPtr := AppendStringToBuffer(FreeMemMsg, LMsgPtr, Length(FreeMemMsg));
    boReallocMem: LMsgPtr := AppendStringToBuffer(ReallocMemMsg, LMsgPtr, Length(ReallocMemMsg));
    boBlockCheckOnShutdown: LMsgPtr := AppendStringToBuffer(BlockCheckMsg, LMsgPtr, Length(BlockCheckMsg));
  end;
  LMsgPtr := AppendStringToBuffer(OperationMsg, LMsgPtr, Length(OperationMsg));
  {Is the header still intact?}
  if LHeaderValid then
  begin
    {Is the footer still valid?}
    if LFooterValid then
    begin
      {A freed block has been modified, or a double free has occurred}
      if AOperation <= boGetMem then
        LMsgPtr := AppendStringToBuffer(FreeModifiedErrorMsg, LMsgPtr, Length(FreeModifiedErrorMsg))
      else
        LMsgPtr := AppendStringToBuffer(DoubleFreeErrorMsg, LMsgPtr, Length(DoubleFreeErrorMsg));
    end
    else
    begin
      LMsgPtr := AppendStringToBuffer(BlockFooterCorruptedMsg, LMsgPtr, Length(BlockFooterCorruptedMsg))
    end;
    {Set the block size message}
    if AOperation <= boGetMem then
      LMsgPtr := AppendStringToBuffer(PreviousBlockSizeMsg, LMsgPtr, Length(PreviousBlockSizeMsg))
    else
      LMsgPtr := AppendStringToBuffer(CurrentBlockSizeMsg, LMsgPtr, Length(CurrentBlockSizeMsg));
    LMsgPtr := CardinalToStrBuf(APointer.UserSize, LMsgPtr);
    {The header is still intact - display info about the this/previous allocation}
    if APointer.AllocationStackTrace[0] <> 0 then
    begin
      if AOperation <= boGetMem then
        LMsgPtr := AppendStringToBuffer(StackTraceAtPrevAllocMsg, LMsgPtr, Length(StackTraceAtPrevAllocMsg))
      else
        LMsgPtr := AppendStringToBuffer(StackTraceAtAllocMsg, LMsgPtr, Length(StackTraceAtAllocMsg));
      LMsgPtr := LogStackTrace(@APointer.AllocationStackTrace, LMsgPtr);
    end;
    {Get the class this block was used for previously}
    LClass := GetObjectClass(@APointer.PreviouslyUsedByClass);
    if (LClass <> nil) and (Cardinal(LClass) <> Cardinal(@FreedObjectVMT.VMTMethods[0])) then
    begin
      LClassName := LClass.ClassName;
      LMsgPtr := AppendStringToBuffer(PreviousObjectClassMsg, LMsgPtr, Length(PreviousObjectClassMsg));
      LMsgPtr := AppendStringToBuffer(@LClassName[1], LMsgPtr, Length(LClassName));
    end;
    {Get the current class for this block}
    if (AOperation > boGetMem) and (not LFooterValid) then
    begin
      LClass := GetObjectClass(Pointer(Cardinal(APointer) + SizeOf(TFullDebugBlockHeader)));
      if (LClass <> nil) and (Cardinal(LClass) <> Cardinal(@FreedObjectVMT.VMTMethods[0])) then
        LClassName := LClass.ClassName
      else
        LClassName := UnknownClassNameMsg;
      LMsgPtr := AppendStringToBuffer(CurrentObjectClassMsg, LMsgPtr, Length(CurrentObjectClassMsg));
      LMsgPtr := AppendStringToBuffer(@LClassName[1], LMsgPtr, Length(LClassName));
    end;
    {Get the call stack for the previous free}
    if APointer.FreeStackTrace[0] <> 0 then
    begin
      LMsgPtr := AppendStringToBuffer(StackTraceAtFreeMsg, LMsgPtr, Length(StackTraceAtFreeMsg));
      LMsgPtr := LogStackTrace(@APointer.FreeStackTrace, LMsgPtr);
    end;
  end
  else
  begin
    {Header has been corrupted}
    LMsgPtr := AppendStringToBuffer(BlockHeaderCorruptedMsg, LMsgPtr, Length(BlockHeaderCorruptedMsg));
  end;
  {Add the current stack trace}
  LMsgPtr := LogCurrentStackTrace(3 + ord(AOperation <> boGetMem) + ord(AOperation = boReallocMem), LMsgPtr);
  {Add the memory dump}
  LMsgPtr := LogMemoryDump(APointer, LMsgPtr);
  {Debug info not available?}
  if not DebugInfoDllAvailable then
    LMsgPtr := AppendStringToBuffer(DebugInfoDllNotAvailableMsg, LMsgPtr, Length(DebugInfoDllNotAvailableMsg));
  {Trailing CRLF}
  LMsgPtr^ := #13;
  Inc(LMsgPtr);
  LMsgPtr^ := #10;
  Inc(LMsgPtr);
  {Trailing #0}
  LMsgPtr^ := #0;
{$ifdef LogErrorsToFile}
  {Log the error}
  AppendEventLog(@LErrorMessage[0], Cardinal(LMsgPtr) - Cardinal(@LErrorMessage[0]));
{$endif}
  {Show the message}
  MessageBox(0, LErrorMessage, BlockErrorMsgTitle,
    MB_OK or MB_ICONERROR or MB_TASKMODAL);
end;

{Logs the stack traces for a memory leak to file}
procedure LogMemoryLeak(APointer: PFullDebugBlockHeader);
var
  LHeaderValid: boolean;
  LMsgPtr: PChar;
  LErrorMessage: array[0..32767] of char;
  LClass: TClass;
  LClassName: ShortString;
begin
  {Display the error header and the operation type.}
  LMsgPtr := AppendStringToBuffer(LeakLogHeader, @LErrorMessage[0], Length(LeakLogHeader));
  LMsgPtr := CardinalToStrBuf(GetAvailableSpaceInBlock(APointer) - FullDebugBlockOverhead, LMsgPtr);
  {Is the debug info surrounding the block valid?}
  LHeaderValid := CalculateHeaderCheckSum(APointer) = APointer.HeaderCheckSum;
  {Is the header still intact?}
  if LHeaderValid then
  begin
    {The header is still intact - display info about this/previous allocation}
    if APointer.AllocationStackTrace[0] <> 0 then
    begin
      LMsgPtr := AppendStringToBuffer(StackTraceAtAllocMsg, LMsgPtr, Length(StackTraceAtAllocMsg));
      LMsgPtr := LogStackTrace(@APointer.AllocationStackTrace, LMsgPtr);
    end;
    {Get the current class for this block}
    LClass := GetObjectClass(Pointer(Cardinal(APointer) + SizeOf(TFullDebugBlockHeader)));
    if (LClass <> nil) and (Cardinal(LClass) <> Cardinal(@FreedObjectVMT.VMTMethods[0])) then
      LClassName := LClass.ClassName
    else
      LClassName := UnknownClassNameMsg;
    LMsgPtr := AppendStringToBuffer(CurrentObjectClassMsg, LMsgPtr, Length(CurrentObjectClassMsg));
    LMsgPtr := AppendStringToBuffer(@LClassName[1], LMsgPtr, Length(LClassName));
  end
  else
  begin
    {Header has been corrupted}
    LMsgPtr^ := '.';
    Inc(LMsgPtr);
    LMsgPtr^ := ' ';
    Inc(LMsgPtr);
    LMsgPtr := AppendStringToBuffer(BlockHeaderCorruptedMsg, LMsgPtr, Length(BlockHeaderCorruptedMsg));
  end;
  {Add the memory dump}
  LMsgPtr := LogMemoryDump(APointer, LMsgPtr);
  {Debug info not available?}
  if not DebugInfoDllAvailable then
    LMsgPtr := AppendStringToBuffer(DebugInfoDllNotAvailableMsg, LMsgPtr, Length(DebugInfoDllNotAvailableMsg));
  {Trailing CRLF}
  LMsgPtr^ := #13;
  Inc(LMsgPtr);
  LMsgPtr^ := #10;
  Inc(LMsgPtr);
  {Trailing #0}
  LMsgPtr^ := #0;
  {Log the error}
  AppendEventLog(@LErrorMessage[0], Cardinal(LMsgPtr) - Cardinal(@LErrorMessage[0]));
end;

function DebugGetMem(ASize: Integer): Pointer;
var
  LHeaderCheckSum: Cardinal;
  LHeaderValid, LFooterValid{$ifndef CatchUseOfFreedInterfaces}, LBlockUnmodified{$endif}: boolean;
begin
  {We need extra space for (a) The debug header, (b) the block debug trailer
   and (c) the trailing block size pointer for free blocks}
  Result := FastGetMem(ASize + FullDebugBlockOverhead);
  if Result <> nil then
  begin
    LHeaderCheckSum := CalculateHeaderCheckSum(Result);
    LHeaderValid := LHeaderCheckSum = PFullDebugBlockHeader(Result).HeaderCheckSum;
    {Is the footer itself still in place}
    LFooterValid := LHeaderValid
      and (PCardinal(Cardinal(Result) + SizeOf(TFullDebugBlockHeader) + PFullDebugBlockHeader(Result).UserSize)^ = (not LHeaderCheckSum));
{$ifndef CatchUseOfFreedInterfaces}
    if LFooterValid then
    begin
      {Clear the old footer}
      PCardinal(Cardinal(Result)  + SizeOf(TFullDebugBlockHeader) + PFullDebugBlockHeader(Result).UserSize)^ := DebugFillDWord;
      {Check that all the filler bytes are valid inside the block, except for the four byte "dummy" class header}
      LBlockUnmodified := CheckFillPattern(PCardinal(Cardinal(Result) + SizeOf(TFullDebugBlockHeader) + 4),
        GetAvailableSpaceInBlock(Result) - FullDebugBlockOverhead);
      {Reset the old footer}
      PCardinal(Cardinal(Result) + SizeOf(TFullDebugBlockHeader) + PFullDebugBlockHeader(Result).UserSize)^ := not LHeaderCheckSum;
    end
    else
      LBlockUnmodified := False;
{$endif}
    if LHeaderValid and LFooterValid{$ifndef CatchUseOfFreedInterfaces} and LBlockUnmodified{$endif} then
    begin
      {Set the allocation call stack}
      GetStackTrace(@PFullDebugBlockHeader(Result).AllocationStackTrace, StackTraceDepth, 1);
      {Block is now in use}
      PFullDebugBlockHeader(Result).BlockInUse := True;
      {Clear the previous block trailer}
      PCardinal(Cardinal(Result) + SizeOf(TFullDebugBlockHeader) + PFullDebugBlockHeader(Result).UserSize)^ := DebugFillDWord;
      {Set the user size for the block}
      PFullDebugBlockHeader(Result).UserSize := ASize;
      {Set the checksums}
      UpdateHeaderAndFooterCheckSums(Result);
      {Return the start of the actual block}
      Result := Pointer(Cardinal(Result) + SizeOf(TFullDebugBlockHeader));
    end
    else
    begin
      LogBlockError(Result, boGetMem, LHeaderValid, LFooterValid);
      Result := nil;
    end;
  end;
end;

function CheckBlockBeforeFreeOrRealloc(APointer: PFullDebugBlockHeader; AOperation: TBlockOperation): boolean;
var
  LHeaderValid, LFooterValid: boolean;
begin
  {Is the debug info surrounding the block valid?}
  LHeaderValid := CalculateHeaderCheckSum(APointer) = APointer.HeaderCheckSum;
  LFooterValid := LHeaderValid
    and (APointer.HeaderCheckSum = (not PCardinal(Cardinal(APointer) + SizeOf(TFullDebugBlockHeader) + PFullDebugBlockHeader(APointer).UserSize)^));
  if LHeaderValid and LFooterValid and APointer.BlockInUse then
  begin
    Result := True;
  end
  else
  begin
    {Log the error}
    LogBlockError(APointer, AOperation, LHeaderValid, LFooterValid);
    {Return an error}
    Result := False;
  end;
end;

function DebugFreeMem(APointer: Pointer): Integer;
var
  LActualBlock: PFullDebugBlockHeader;
begin
  {Get a pointer to the start of the actual block}
  LActualBlock := PFullDebugBlockHeader(Cardinal(APointer)
    - SizeOf(TFullDebugBlockHeader));
  {Is the debug info surrounding the block valid?}
  if CheckBlockBeforeFreeOrRealloc(LActualBlock, boFreeMem) then
  begin
    {Get the class the block was used for}
    LActualBlock.PreviouslyUsedByClass := PCardinal(APointer)^;
    {Set the free call stack}
    GetStackTrace(@LActualBlock.FreeStackTrace, StackTraceDepth, 1);
    {Block is now free}
    LActualBlock.BlockInUse := False;
    {Clear the user area of the block}
    FillDWord(APointer^, LActualBlock.UserSize,
      {$ifndef CatchUseOfFreedInterfaces}DebugFillDWord{$else}Cardinal(@VMTBadInterface){$endif});
    {Set a pointer to the dummy VMT}
    PCardinal(APointer)^ := Cardinal(@FreedObjectVMT.VMTMethods[0]);
    {Recalculate the checksums}
    UpdateHeaderAndFooterCheckSums(LActualBlock);
    {Free the actual block}
    Result := FastFreeMem(LActualBlock);
  end
  else
  begin
    Result := -1;
  end;
end;

{In debug mode we never do an in-place resize, data is always moved. This
 increases the likelihood of catching memory overwrite bugs.}
function DebugReallocMem(APointer: Pointer; ANewSize: Integer): Pointer;
var
  LMoveSize, LBlockSpace: Cardinal;
  LActualBlock: PFullDebugBlockHeader;
begin
  {Get a pointer to the start of the actual block}
  LActualBlock := PFullDebugBlockHeader(Cardinal(APointer)
    - SizeOf(TFullDebugBlockHeader));
  {Is the debug info surrounding the block valid?}
  if CheckBlockBeforeFreeOrRealloc(LActualBlock, boReallocMem) then
  begin
    {Get the current block size}
    LBlockSpace := GetAvailableSpaceInBlock(LActualBlock);
    {Can the block fit? We need space for the debug overhead and the block header
     of the next block}
    if LBlockSpace < (Cardinal(ANewSize) + FullDebugBlockOverhead) then
    begin
      {Get a new block of the requested size}
      Result := DebugGetMem(ANewSize);
      if Result <> nil then
      begin
        {How many bytes to move?}
        LMoveSize := LActualBlock.UserSize;
        if LMoveSize > Cardinal(ANewSize) then
          LMoveSize := ANewSize;
        {Move the data across}
        System.Move(APointer^, Result^, LMoveSize);
        {Free the old block}
        DebugFreeMem(APointer);
      end
      else
      begin
        Result := nil;
      end;
    end
    else
    begin
      {Clear all data after the new end of the block up to the old end of the
       block, including the trailer}
      FillDWord(Pointer(Cardinal(APointer) + Cardinal(ANewSize) + 4)^,
        Integer(LActualBlock.UserSize) - ANewSize,
        {$ifndef CatchUseOfFreedInterfaces}DebugFillDWord{$else}Cardinal(@VMTBadInterface){$endif});
      {Update the user size}
      LActualBlock.UserSize := ANewSize;
      {Set the new checksums}
      UpdateHeaderAndFooterCheckSums(LActualBlock);
      {Return the old pointer}
      Result := APointer;
    end;
  end
  else
  begin
    Result := nil;
  end;
end;

{-----------------------Invalid Virtual Method Calls-------------------------}

{ TFreedObject }

{Used to determine the index of the virtual method call on the freed object.
 Do not change this without updating MaxFakeVMTEntries. Currently 200.}
procedure TFreedObject.GetVirtualMethodIndex;
asm
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);

  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);

  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);

  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);

  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);

  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);

  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);

  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);
  Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex); Inc(VMIndex);

  jmp TFreedObject.VirtualMethodError
end;

procedure TFreedObject.VirtualMethodError;
var
  LVMOffset: Integer;
  LMsgPtr: PChar;
  LErrorMessage: array[0..32767] of char;
  LClass: TClass;
  LClassName: ShortString;
  LActualBlock: PFullDebugBlockHeader;
begin
  {Get the offset of the virtual method}
  LVMOffset := (MaxFakeVMTEntries - VMIndex) * 4 + vmtParent + 4;
  {Reset the index for the next error}
  VMIndex := 0;
  {Get the address of the actual block}
  LActualBlock := PFullDebugBlockHeader(Cardinal(Self) - SizeOf(TFullDebugBlockHeader));
  {Display the error header}
  LMsgPtr := AppendStringToBuffer(VirtualMethodErrorHeader, @LErrorMessage[0], Length(VirtualMethodErrorHeader));
  {Is the debug info surrounding the block valid?}
  if CalculateHeaderCheckSum(LActualBlock) = LActualBlock.HeaderCheckSum then
  begin
    {Get the class this block was used for previously}
    LClass := GetObjectClass(@LActualBlock.PreviouslyUsedByClass);
    if (LClass <> nil) and (Cardinal(LClass) <> Cardinal(@FreedObjectVMT.VMTMethods[0])) then
    begin
      LClassName := LClass.ClassName;
      LMsgPtr := AppendStringToBuffer(FreedObjectClassMsg, LMsgPtr, Length(FreedObjectClassMsg));
      LMsgPtr := AppendStringToBuffer(@LClassName[1], LMsgPtr, Length(LClassName));
    end;
    {Get the virtual method name}
    LMsgPtr := AppendStringToBuffer(VirtualMethodName, LMsgPtr, Length(VirtualMethodName));
    if LVMOffset < 0 then
    begin
      LMsgPtr := AppendStringToBuffer(StandardVirtualMethodNames[LVMOffset div 4], LMsgPtr, Length(StandardVirtualMethodNames[LVMOffset div 4]));
    end
    else
    begin
      LMsgPtr := AppendStringToBuffer(VirtualMethodOffset, LMsgPtr, Length(VirtualMethodOffset));
      LMsgPtr := CardinalToStrBuf(LVMOffset, LMsgPtr);
    end;
    {Virtual method address}
    if (LClass <> nil) and (Cardinal(LClass) <> Cardinal(@FreedObjectVMT.VMTMethods[0])) then
    begin
      LMsgPtr := AppendStringToBuffer(VirtualMethodAddress, LMsgPtr, Length(VirtualMethodAddress));
      LMsgPtr := CardinalToHexBuf(PCardinal(Integer(LClass) + LVMOffset)^, LMsgPtr);
    end;
    {The header is still intact - display info about the this/previous allocation}
    if LActualBlock.AllocationStackTrace[0] <> 0 then
    begin
      LMsgPtr := AppendStringToBuffer(StackTraceAtObjectAllocMsg, LMsgPtr, Length(StackTraceAtObjectAllocMsg));
      LMsgPtr := LogStackTrace(@LActualBlock.AllocationStackTrace, LMsgPtr);
    end;
    {Get the call stack for the previous free}
    if LActualBlock.FreeStackTrace[0] <> 0 then
    begin
      LMsgPtr := AppendStringToBuffer(StackTraceAtObjectFreeMsg, LMsgPtr, Length(StackTraceAtObjectFreeMsg));
      LMsgPtr := LogStackTrace(@LActualBlock.FreeStackTrace, LMsgPtr);
    end;
  end
  else
  begin
    {Header has been corrupted}
    LMsgPtr := AppendStringToBuffer(BlockHeaderCorruptedNoHistoryMsg, LMsgPtr, Length(BlockHeaderCorruptedNoHistoryMsg));
  end;
  {Add the current stack trace}
  LMsgPtr := LogCurrentStackTrace(2, LMsgPtr);
  {Add the pointer address}
  LMsgPtr := LogMemoryDump(LActualBlock, LMsgPtr);
  {Trailing CRLF}
  LMsgPtr^ := #13;
  Inc(LMsgPtr);
  LMsgPtr^ := #10;
  Inc(LMsgPtr);
  {Trailing #0}
  LMsgPtr^ := #0;
{$ifdef LogErrorsToFile}
  {Log the error}
  AppendEventLog(@LErrorMessage[0], Cardinal(LMsgPtr) - Cardinal(@LErrorMessage[0]));
{$endif}
  {Show the message}
  MessageBox(0, LErrorMessage, BlockErrorMsgTitle,
    MB_OK or MB_ICONERROR or MB_TASKMODAL);
  {Raise an access violation}
  RaiseException(EXCEPTION_ACCESS_VIOLATION, 0, 0, nil);
end;

{$ifdef CatchUseOfFreedInterfaces}
procedure TFreedObject.InterfaceError;
var
  LMsgPtr: PChar;
  LErrorMessage: array[0..4000] of char;
begin
  {Display the error header}
  LMsgPtr := AppendStringToBuffer(InterfaceErrorHeader, @LErrorMessage[0], Length(InterfaceErrorHeader));
  {Add the current stack trace}
  LMsgPtr := LogCurrentStackTrace(2, LMsgPtr);
  {Trailing CRLF}
  LMsgPtr^ := #13;
  Inc(LMsgPtr);
  LMsgPtr^ := #10;
  Inc(LMsgPtr);
  {Trailing #0}
  LMsgPtr^ := #0;
{$ifdef LogErrorsToFile}
  {Log the error}
  AppendEventLog(@LErrorMessage[0], Cardinal(LMsgPtr) - Cardinal(@LErrorMessage[0]));
{$endif}
  {Show the message}
  MessageBox(0, LErrorMessage, BlockErrorMsgTitle,
    MB_OK or MB_ICONERROR or MB_TASKMODAL);
  {Raise an access violation}
  RaiseException(EXCEPTION_ACCESS_VIOLATION, 0, 0, nil);
end;
{$endif}

{$endif}

{----------------------------Memory Leak Checking-----------------------------}


{$ifdef EnableMemoryLeakReporting}

{Registers expected memory leaks. Returns true on success. The list of leaked
 blocks is limited to 64K, so failure is possible if you have a ridiculous
 number of expected leaks.}
function AddExpectedMemoryLeaks(ALeakSize: Cardinal; ACount: Cardinal): Boolean;
var
  LIndex: Cardinal;
begin
  {Default to error}
  Result := false;
  {Must be the MM owner to be able to register leaks}
  if IsMemoryManagerOwner then
  begin
    if ExpectedMemoryLeaks = nil then
    begin
      ExpectedMemoryLeaks := VirtualAlloc(nil, SizeOf(TExpectedMemoryLeaks),
        MEM_COMMIT, PAGE_READWRITE);
    end;
    {Add the entry}
    if (ExpectedMemoryLeaks <> nil)
      and (ExpectedMemoryLeaks.NumEntries < Cardinal(length(ExpectedMemoryLeaks.ExpectedLeaks))) then
    begin
      {Try to add to a previous entry}
      LIndex := 0;
      while (LIndex < ExpectedMemoryLeaks.NumEntries)
        and (ExpectedMemoryLeaks.ExpectedLeaks[LIndex].LeakSize <> ALeakSize) do
      begin
        Inc(LIndex);
      end;
      {New entry?}
      if LIndex = ExpectedMemoryLeaks.NumEntries then
      begin
        Inc(ExpectedMemoryLeaks.NumEntries);
        ExpectedMemoryLeaks.ExpectedLeaks[LIndex].LeakSize := ALeakSize;
      end;
      {Add the number of leaks}
      Inc(ExpectedMemoryLeaks.ExpectedLeaks[LIndex].LeakCount, ACount);
      {Success}
      Result := True;
    end;
  end;
end;

{Removes expected memory leaks. Returns true on success.}
function RemoveExpectedMemoryLeaks(ALeakSize: Cardinal; ACount: Cardinal): Boolean;
var
  LIndex: Cardinal;
begin
  {Default to failure}
  Result := False;
  {Must be the MM owner to be able to register leaks}
  if IsMemoryManagerOwner and (ExpectedMemoryLeaks <> nil) then
  begin
    {Find the leak size in the list}
    for LIndex := 0 to ExpectedMemoryLeaks.NumEntries - 1 do
    begin
      {Is the size a match?}
      if ExpectedMemoryLeaks.ExpectedLeaks[LIndex].LeakSize = ALeakSize then
      begin
        {Sufficient number of registered leaks left?}
        if ExpectedMemoryLeaks.ExpectedLeaks[LIndex].LeakCount >= ACount then
        begin
          Dec(ExpectedMemoryLeaks.ExpectedLeaks[LIndex].LeakCount, ACount);
          Result := True;
        end;
        break;
      end;
    end;
  end;
end;
{$endif}

{Advances to the next medium block. Returns nil if the end of the medium block
 pool has been reached}
function NextMediumBlock(APMediumBlock: Pointer): Pointer;
var
  LBlockSize: Cardinal;
begin
  {Get the size of this block}
  LBlockSize := PCardinal(Cardinal(APMediumBlock) - 4)^ and DropMediumAndLargeFlagsMask;
  {Advance the pointer}
  Result := Pointer(Cardinal(APMediumBlock) + LBlockSize);
  {Is the next block the end of medium pool marker?}
  LBlockSize := PCardinal(Cardinal(Result) - 4)^ and DropMediumAndLargeFlagsMask;
  if LBlockSize = 0 then
    Result := nil;
end;

{Gets the first medium block in the medium block pool}
function GetFirstMediumBlockInPool(APMediumBlockPoolHeader: PMediumBlockPoolHeader): Pointer;
begin
  if (MediumSequentialFeedBytesLeft = 0)
    or (Cardinal(LastSequentiallyFedMediumBlock) < Cardinal(APMediumBlockPoolHeader))
    or (Cardinal(LastSequentiallyFedMediumBlock) > Cardinal(APMediumBlockPoolHeader) + MediumBlockPoolSize) then
  begin
    Result := Pointer(Cardinal(APMediumBlockPoolHeader) + MediumBlockPoolHeaderSize);
  end
  else
  begin
    {Is the sequential feed pool empty?}
    if MediumSequentialFeedBytesLeft <> MediumBlockPoolSize - MediumBlockPoolHeaderSize then
      Result := LastSequentiallyFedMediumBlock
    else
      Result := nil;
  end;
end;

{Gets the first and last block pointer for a small block pool}
procedure GetFirstAndLastSmallBlockInPool(APSmallBlockPool: PSmallBlockPoolHeader;
  var AFirstPtr, ALastPtr: Pointer);
var
  LBlockSize: Cardinal;
begin
  {Get the pointer to the first block}
  AFirstPtr := Pointer(Cardinal(APSmallBlockPool) + SmallBlockPoolHeaderSize);
  {Get a pointer to the last block}
  if (APSmallBlockPool.BlockType.CurrentSequentialFeedPool <> APSmallBlockPool)
    or (Cardinal(APSmallBlockPool.BlockType.NextSequentialFeedBlockAddress) > Cardinal(APSmallBlockPool.BlockType.MaxSequentialFeedBlockAddress)) then
  begin
    {Not the sequential feed - point to the end of the block}
    LBlockSize := PCardinal(Cardinal(APSmallBlockPool) - 4)^ and DropMediumAndLargeFlagsMask;
    ALastPtr := Pointer(Cardinal(APSmallBlockPool) + LBlockSize - APSmallBlockPool.BlockType.BlockSize);
  end
  else
  begin
    {The sequential feed pool - point to before the next sequential feed block}
    ALastPtr := Pointer(Cardinal(APSmallBlockPool.BlockType.NextSequentialFeedBlockAddress) - 1);
  end;
end;

{Returns true if the medium block pointed to by APMediumBlock is a small block
 pool}
function IsSmallBlockPool(APMediumBlock: PSmallBlockPoolHeader): boolean;
var
  LBlockTypeOffset: Integer;
  LBlockHeader: Cardinal;
  LCurPtr, LEndPtr: Pointer;
begin
  {Check the signature}
  if APMediumBlock.SmallBlockPoolSignature = SmallBlockPoolSignatureValue then
  begin
    {Does it point to a valid small block type?}
    LBlockTypeOffset := Integer(APMediumBlock.BlockType) - Integer(@SmallBlockTypes[0]);
    if (LBlockTypeOffset >= 0)
      and (LBlockTypeOffset < NumSmallBlockTypes * SizeOf(TSmallBlockType))
      and (LBlockTypeOffset and (SizeOf(TSmallBlockType) - 1) = 0) then
    begin
      {Check all the small blocks inside the pool}
      GetFirstAndLastSmallBlockInPool(APMediumBlock, LCurPtr, LEndPtr);
      Result := True;
      while Cardinal(LCurPtr) <= Cardinal(LEndPtr) do
      begin
        LBlockHeader := PCardinal(Cardinal(LCurPtr) - 4)^;
        if ((LBlockHeader and ExtractSmallFlagsMask) = 0) then
        begin
          {Small block is in use - the header must be a pointer to the pool}
          if LBlockHeader <> Cardinal(APMediumBlock) then
          begin
            Result := False;
            exit;
          end;
        end
        else
        begin
          {Must be a small free block}
          if LBlockHeader and ExtractSmallFlagsMask <> IsFreeBlockFlag then
          begin
            Result := False;
            exit;
          end;
          {Drop the flags}
          LBlockHeader := LBlockHeader and DropSmallFlagsMask;
          {Free: Must either point to another block in the pool or nil (plus the free block flag)}
          if (LBlockHeader <> 0)
            and ((LBlockHeader > Cardinal(LEndPtr))
              or (LBlockHeader <= Cardinal(APMediumBlock))) then
          begin
            Result := False;
            exit;
          end;
        end;
        {Next block}
        Inc(PCardinal(LCurPtr), APMediumBlock.BlockType.BlockSize);
      end;
    end
    else
      Result := False;
  end
  else
    Result := False;
end;

{Checks blocks on shutdown for modification after free and also for memory
 leaks}
procedure CheckBlocksOnShutdown(AReportLeaks: Boolean);
{$ifdef EnableMemoryLeakReporting}
type
  {Leaked class type}
  TLeakedClass = packed record
    ClassPointer: TClass;
    NumLeaks: Cardinal;
  end;
  TLeakedClasses = array[0..255] of TLeakedClass;
  PLeakedClasses = ^TLeakedClasses;
  {Leak statistics for a small block type}
  TSmallBlockLeaks = array[0..NumSmallBlockTypes - 1] of TLeakedClasses;
  {A leaked medium or large block}
  TMediumAndLargeBlockLeaks = array[0..4095] of Cardinal;
{$endif}
var
{$ifdef EnableMemoryLeakReporting}
  {The leaked classes for small blocks}
  LSmallBlockLeaks: TSmallBlockLeaks;
  LMediumAndLargeBlockLeaks: TMediumAndLargeBlockLeaks;
  LNumMediumAndLargeLeaks: Integer;
  LPLargeBlock: PLargeBlockHeader;
  LLeakMessage: array[0..32767] of char;
  LMsgPtr: PChar;
  LClassName: ShortString;
  LExpectedLeaksOnly, LSmallLeakHeaderAdded, LBlockSizeHeaderAdded: Boolean;
  LBlockTypeInd, LMediumBlockSize, LLargeBlockSize,
    LClassInd, LPreviousBlockSize, LThisBlockSize, LBlockInd: Cardinal;
{$endif}
  LPMediumBlock: Pointer;
  LPMediumBlockPoolHeader: PMediumBlockPoolHeader;
  LMediumBlockHeader: Cardinal;

{$ifdef CheckUseOfFreedBlocksOnShutdown}
  {Checks that a free block is unmodified}
  procedure CheckFreeBlockUnmodified(APBlock: PFullDebugBlockHeader; ABlockSize: Cardinal);
  var
    LHeaderCheckSum: Cardinal;
    LHeaderValid, LFooterValid, LBlockUnmodified: boolean;
  begin
    LHeaderCheckSum := CalculateHeaderCheckSum(APBlock);
    LHeaderValid := LHeaderCheckSum = PFullDebugBlockHeader(APBlock).HeaderCheckSum;
    {Is the footer itself still in place}
    LFooterValid := LHeaderValid
      and (PCardinal(Cardinal(APBlock) + SizeOf(TFullDebugBlockHeader) + PFullDebugBlockHeader(APBlock).UserSize)^ = (not LHeaderCheckSum));
    if LFooterValid then
    begin
      {Clear the old footer}
      PCardinal(Cardinal(APBlock)  + SizeOf(TFullDebugBlockHeader) + PFullDebugBlockHeader(APBlock).UserSize)^ := DebugFillDWord;
      {Check that all the filler bytes are valid inside the block, except for the four byte "dummy" class header}
      LBlockUnmodified := CheckFillPattern(PCardinal(Cardinal(APBlock) + SizeOf(TFullDebugBlockHeader) + 4),
        ABlockSize - (BlockHeaderSize + FullDebugBlockOverhead));
      {Reset the old footer}
      PCardinal(Cardinal(APBlock) + SizeOf(TFullDebugBlockHeader) + PFullDebugBlockHeader(APBlock).UserSize)^ := not LHeaderCheckSum;
    end
    else
      LBlockUnmodified := False;
    if (not LHeaderValid)
      or (not LFooterValid)
      or (not LBlockUnmodified) then
    begin
      LogBlockError(APBlock, boBlockCheckOnShutdown, LHeaderValid, LFooterValid);
    end;
  end;
{$endif}

{$ifdef EnableMemoryLeakReporting}
  {Tries to account for a memory leak. Returns true if the leak is expected and
   decrements the count for that leak size}
  function CheckExpectedLeak(AMaxLeakSize: Cardinal): Boolean;
  var
    LIndex, LBestIndex, LBiggest: Cardinal;
  begin
    {Default to not found}
    Result := False;
    if ExpectedMemoryLeaks <> nil then
    begin
      {Find the largest registered leak that is less than or equal to this size}
      LBiggest := 0;
      LBestIndex := 0;
      for LIndex := 0 to ExpectedMemoryLeaks.NumEntries - 1 do
      begin
        with ExpectedMemoryLeaks.ExpectedLeaks[LIndex] do
        begin
          if (LeakSize <= AMaxLeakSize)
            and (LeakSize > LBiggest)
            and (LeakCount > 0) then
          begin
            LBiggest := LeakSize;
            LBestIndex := LIndex;
          end;
        end;
      end;
      {Found a registered leak that fits inside the block size?}
      if LBiggest > 0 then
      begin
        Dec(ExpectedMemoryLeaks.ExpectedLeaks[LBestIndex].LeakCount);
        Result := True;
      end;
    end;
  end;

  {Checks the small block pool for leaks.}
  procedure CheckSmallBlockPoolForLeaks(APSmallBlockPool: PSmallBlockPoolHeader);
  var
    LLeakedClass: TClass;
    LCharInd, LClassIndex, LStringLength: Integer;
    LPStr: PChar;
    LPossibleString: boolean;
    LCurPtr, LEndPtr, LDataPtr: Pointer;
    LBlockTypeIndex: Cardinal;
    LPLeakedClasses: PLeakedClasses;
    LSmallBlockSize: Cardinal;
  begin
    {Get the useable size inside a block}
    LSmallBlockSize := APSmallBlockPool.BlockType.BlockSize - BlockHeaderSize;
  {$ifdef FullDebugMode}
    Dec(LSmallBlockSize, FullDebugBlockOverhead);
  {$endif}
    {Get the block type index}
    LBlockTypeIndex := (Cardinal(APSmallBlockPool.BlockType) - Cardinal(@SmallBlockTypes[0])) div SizeOf(TSmallBlockType);
    LPLeakedClasses := @LSmallBlockLeaks[LBlockTypeIndex];
    {Get the first and last pointer for the pool}
    GetFirstAndLastSmallBlockInPool(APSmallBlockPool, LCurPtr, LEndPtr);
    {Step through all blocks}
    while Cardinal(LCurPtr) <= Cardinal(LEndPtr) do
    begin
      {Is this block in use?}
      if (PCardinal(Cardinal(LCurPtr) - 4)^ and IsFreeBlockFlag) = 0 then
      begin
        {Get a pointer to the user data}
  {$ifdef LogMemoryLeakDetailToFile}
        LogMemoryLeak(LCurPtr);
  {$endif}
  {$ifndef FullDebugMode}
        LDataPtr := LCurPtr;
  {$else}
        LDataPtr := Pointer(Cardinal(LCurPtr) + SizeOf(TFullDebugBlockHeader));
  {$endif}
        {Is it an expected leak?}
        LExpectedLeaksOnly := LExpectedLeaksOnly and CheckExpectedLeak(LSmallBlockSize);
        {Default to an unknown block}
        LClassIndex := 0;
        {Get the class contained by the block}
        LLeakedClass := GetObjectClass(LDataPtr);
        {Not a class? -> is it perhaps a string?}
        if LLeakedClass = nil then
        begin
          {Reference count < 256}
          if (PCardinal(LDataPtr)^ < 256) then
          begin
            LStringLength := PCardinal(Cardinal(LDataPtr) + 4)^;
            {Does the string fit?}
            if (LStringLength > 0)
              and (LStringLength < (APSmallBlockPool.BlockType.BlockSize - (8 + 1 + 4 {$ifdef FullDebugMode} + FullDebugBlockOverhead{$endif}))) then
            begin
              {Check that all characters are in range #32..#127}
              LPStr := PChar(Cardinal(LDataPtr) + 8);
              LPossibleString := True;
              for LCharInd := 1 to LStringLength do
              begin
                LPossibleString := LPossibleString and (LPStr^ >= #32) and (LPStr^ < #128);
                Inc(LPStr);
              end;
              {Must have a trailing #0}
              if LPossibleString and (LPStr^ = #0) then
              begin
                LClassIndex := 1;
              end;
            end;
          end;
        end
        else
        begin
          LClassIndex := 2;
          while LClassIndex <= High(TLeakedClasses) do
          begin
            if (LPLeakedClasses[LClassIndex].ClassPointer = LLeakedClass)
              or (LPLeakedClasses[LClassIndex].ClassPointer = nil) then
            begin
              break;
            end;
            Inc(LClassIndex);
          end;
          if LClassIndex <= High(TLeakedClasses) then
            LPLeakedClasses[LClassIndex].ClassPointer := LLeakedClass
          else
            LClassIndex := 0;
        end;
        {Add to the number of leaks for the class}
        Inc(LPLeakedClasses[LClassIndex].NumLeaks);
      end
      else
      begin
  {$ifdef CheckUseOfFreedBlocksOnShutdown}
        {Check that the block has not been modified since being freed}
        CheckFreeBlockUnmodified(LCurPtr, APSmallBlockPool.BlockType.BlockSize);
  {$endif}
      end;
      {Next block}
      Inc(Cardinal(LCurPtr), APSmallBlockPool.BlockType.BlockSize);
    end;
  end;
{$endif}

begin
{$ifdef EnableMemoryLeakReporting}
  {Clear the leak arrays}
  FillChar(LSmallBlockLeaks, SizeOf(LSmallBlockLeaks), 0);
  FillChar(LMediumAndLargeBlockLeaks, SizeOf(LMediumAndLargeBlockLeaks), 0);
  {Step through all the medium block pools}
  LNumMediumAndLargeLeaks := 0;
  {No unexpected leaks so far}
  LExpectedLeaksOnly := True;
{$endif}
  {Step through all the medium block pools}
  LPMediumBlockPoolHeader := MediumBlockPoolsCircularList.NextMediumBlockPoolHeader;
  while LPMediumBlockPoolHeader <> @MediumBlockPoolsCircularList do
  begin
    LPMediumBlock := GetFirstMediumBlockInPool(LPMediumBlockPoolHeader);
    while LPMediumBlock <> nil do
    begin
      LMediumBlockHeader := PCardinal(Cardinal(LPMediumBlock) - 4)^;
      {Is the block in use?}
      if LMediumBlockHeader and IsFreeBlockFlag = 0 then
      begin
{$ifdef EnableMemoryLeakReporting}
        if AReportLeaks then
        begin
          if IsSmallBlockPool(LPMediumBlock) then
          begin
            {Get all the leaks for the small block pool}
            CheckSmallBlockPoolForLeaks(LPMediumBlock);
          end
          else
          begin
            if LNumMediumAndLargeLeaks < length(LMediumAndLargeBlockLeaks) then
            begin
  {$ifdef LogMemoryLeakDetailToFile}
              LogMemoryLeak(LPMediumBlock);
  {$endif}
              LMediumBlockSize := (LMediumBlockHeader and DropMediumAndLargeFlagsMask) - BlockHeaderSize;
  {$ifdef FullDebugMode}
              Dec(LMediumBlockSize, FullDebugBlockOverhead);
  {$endif}
              {Is it an expected leak?}
              LExpectedLeaksOnly := LExpectedLeaksOnly and CheckExpectedLeak(LMediumBlockSize);
              {Add the leak to the list}
              LMediumAndLargeBlockLeaks[LNumMediumAndLargeLeaks] := LMediumBlockSize;
              Inc(LNumMediumAndLargeLeaks);
            end;
          end;
        end;
{$endif}
      end
      else
      begin
{$ifdef CheckUseOfFreedBlocksOnShutdown}
        {Check that the block has not been modified since being freed}
        CheckFreeBlockUnmodified(LPMediumBlock, LMediumBlockHeader and DropMediumAndLargeFlagsMask);
{$endif}
      end;
      {Next medium block}
      LPMediumBlock := NextMediumBlock(LPMediumBlock);
    end;
    {Get the next medium block pool}
    LPMediumBlockPoolHeader := LPMediumBlockPoolHeader.NextMediumBlockPoolHeader;
  end;
{$ifdef EnableMemoryLeakReporting}
  if AReportLeaks then
  begin
    {Get all leaked large blocks}
    LPLargeBlock := LargeBlocksCircularList.NextLargeBlockHeader;
    while (LPLargeBlock <> @LargeBlocksCircularList)
      and (LNumMediumAndLargeLeaks < length(LMediumAndLargeBlockLeaks)) do
    begin
      LLargeBlockSize := (LPLargeBlock.BlockSizeAndFlags and DropMediumAndLargeFlagsMask) - BlockHeaderSize - LargeBlockHeaderSize;
  {$ifdef LogMemoryLeakDetailToFile}
      LogMemoryLeak(Pointer(Cardinal(LPLargeBlock) + LargeBlockHeaderSize));
  {$endif}
  {$ifdef FullDebugMode}
      Dec(LLargeBlockSize, FullDebugBlockOverhead);
  {$endif}
      {Is it an expected leak?}
      LExpectedLeaksOnly := LExpectedLeaksOnly and CheckExpectedLeak(LLargeBlockSize);
      {Add the leak}
      LMediumAndLargeBlockLeaks[LNumMediumAndLargeLeaks] := LLargeBlockSize;
      Inc(LNumMediumAndLargeLeaks);
      {Get the next large block}
      LPLargeBlock := LPLargeBlock.NextLargeBlockHeader;
    end;
    {Display the leak message if required}
    if not LExpectedLeaksOnly then
    begin
      {Small leak header has not been added}
      LSmallLeakHeaderAdded := False;
      LPreviousBlockSize := 0;
      {Set up the leak message header so long}
      LMsgPtr := AppendStringToBuffer(LeakMessageHeader, @LLeakMessage[0], length(LeakMessageHeader));
      {Step through all the small block types}
      for LBlockTypeInd := 0 to NumSmallBlockTypes - 1 do
      begin
        LThisBlockSize := SmallBlockTypes[LBlockTypeInd].BlockSize - BlockHeaderSize;
  {$ifdef FullDebugMode}
        Dec(LThisBlockSize, FullDebugBlockOverhead);
        if Integer(LThisBlockSize) < 0 then
          LThisBlockSize := 0;
  {$endif}
        LBlockSizeHeaderAdded := False;
        {Any leaks?}
        for LClassInd := high(LSmallBlockLeaks[LBlockTypeInd]) downto 0 do
        begin
          {Is there still space in the message buffer? Reserve space for the message
           footer.}
          if LMsgPtr > @LLeakMessage[high(LLeakMessage) - 2048] then
            break;
          {Check the count}
          if LSmallBlockLeaks[LBlockTypeInd][LClassInd].NumLeaks > 0 then
          begin
            {Need to add the header?}
            if not LSmallLeakHeaderAdded then
            begin
              LMsgPtr := AppendStringToBuffer(SmallLeakDetail, LMsgPtr, Length(SmallLeakDetail));
              LSmallLeakHeaderAdded := True;
            end;
            {Need to add the size header?}
            if not LBlockSizeHeaderAdded then
            begin
              LMsgPtr^ := #13;
              Inc(LMsgPtr);
              LMsgPtr^ := #10;
              Inc(LMsgPtr);
              LMsgPtr := CardinalToStrBuf(LPreviousBlockSize + 1, LMsgPtr);
              LMsgPtr^ := ' ';
              Inc(LMsgPtr);
              LMsgPtr^ := '-';
              Inc(LMsgPtr);
              LMsgPtr^ := ' ';
              Inc(LMsgPtr);
              LMsgPtr := CardinalToStrBuf(LThisBlockSize, LMsgPtr);
              LMsgPtr := AppendStringToBuffer(BytesMessage, LMsgPtr, Length(BytesMessage));
              LBlockSizeHeaderAdded := True;
            end
            else
            begin
              LMsgPtr^ := ',';
              Inc(LMsgPtr);
              LMsgPtr^ := ' ';
              Inc(LMsgPtr);
            end;
            {Show the count}
            case LClassInd of
              {Unknown}
              0:
              begin
                LMsgPtr := AppendStringToBuffer(UnknownClassNameMsg, LMsgPtr, Length(UnknownClassNameMsg));
              end;
              {Strings}
              1:
              begin
                LMsgPtr := AppendStringToBuffer(StringBlockMessage, LMsgPtr, Length(StringBlockMessage));
              end;
              {Classes}
            else
              begin
                LClassName := LSmallBlockLeaks[LBlockTypeInd][LClassInd].ClassPointer.ClassName;
                LMsgPtr := AppendStringToBuffer(@LClassName[1], LMsgPtr, Length(LClassName));
              end;
            end;
            {Add the count}
            LMsgPtr^ := ' ';
            Inc(LMsgPtr);
            LMsgPtr^ := 'x';
            Inc(LMsgPtr);
            LMsgPtr^ := ' ';
            Inc(LMsgPtr);
            LMsgPtr := CardinalToStrBuf(LSmallBlockLeaks[LBlockTypeInd][LClassInd].NumLeaks, LMsgPtr);
          end;
        end;
        LPreviousBlockSize := LThisBlockSize;
      end;
      {Add the medium/large block leak message}
      if LNumMediumAndLargeLeaks > 0 then
      begin
        {Any non-small leaks?}
        if LSmallLeakHeaderAdded then
        begin
          LMsgPtr^ := #13;
          Inc(LMsgPtr);
          LMsgPtr^ := #10;
          Inc(LMsgPtr);
          LMsgPtr^ := #13;
          Inc(LMsgPtr);
          LMsgPtr^ := #10;
          Inc(LMsgPtr);
        end;
        {Add the medium/large block leak message}
        LMsgPtr := AppendStringToBuffer(LargeLeakDetail, LMsgPtr, Length(LargeLeakDetail));
        {List all the blocks}
        for LBlockInd := 0 to LNumMediumAndLargeLeaks - 1 do
        begin
          if LBlockInd <> 0 then
          begin
            LMsgPtr^ := ',';
            Inc(LMsgPtr);
            LMsgPtr^ :=  ' ';
            Inc(LMsgPtr);
          end;
          LMsgPtr := CardinalToStrBuf(LMediumAndLargeBlockLeaks[LBlockInd], LMsgPtr);
          {Is there still space in the message buffer? Reserve space for the
           message footer.}
          if LMsgPtr > @LLeakMessage[high(LLeakMessage) - 2048] then
            break;
        end;
      end;
  {$ifdef LogErrorsToFile}
     {Set the message footer}
      LMsgPtr := AppendStringToBuffer(LeakMessageFooter, LMsgPtr, Length(LeakMessageFooter));
      {Append the message to the memory errors file}
      AppendEventLog(@LLeakMessage[0], Cardinal(LMsgPtr) - Cardinal(@LLeakMessage[1]));
  {$else}
      {Set the message footer}
      AppendStringToBuffer(LeakMessageFooter, LMsgPtr, Length(LeakMessageFooter));
  {$endif}
      {Show the message}
      MessageBox(0, LLeakMessage, LeakMessageTitle,
        MB_OK or MB_ICONERROR or MB_TASKMODAL);
    end;
  end;
{$endif}
end;

{Returns statistics about the current state of the memory manager}
procedure GetMemoryManagerState(var AMemoryManagerState: TMemoryManagerState);
var
  LPMediumBlockPoolHeader: PMediumBlockPoolHeader;
  LPMediumBlock: Pointer;
  LInd: Integer;
  LBlockTypeIndex, LMediumBlockSize, LMediumBlockHeader, LLargeBlockSize: Cardinal;
  LPLargeBlock: PLargeBlockHeader;
begin
  {Clear the results}
  FillChar(AMemoryManagerState, SizeOf(AMemoryManagerState), 0);
  {Set the small block size stats}
  for LInd := 0 to NumSmallBlockTypes - 1 do
  begin
    AMemoryManagerState.SmallBlockTypeStates[LInd].InternalBlockSize :=
      SmallBlockTypes[LInd].BlockSize;
    AMemoryManagerState.SmallBlockTypeStates[LInd].UseableBlockSize :=
      SmallBlockTypes[LInd].BlockSize - BlockHeaderSize{$ifdef FullDebugMode} - FullDebugBlockOverhead{$endif};
    if Integer(AMemoryManagerState.SmallBlockTypeStates[LInd].UseableBlockSize) < 0 then
      AMemoryManagerState.SmallBlockTypeStates[LInd].UseableBlockSize := 0;
  end;
  {Lock the medium blocks}
  LockMediumBlocks;
  {Step through all the medium block pools}
  LPMediumBlockPoolHeader := MediumBlockPoolsCircularList.NextMediumBlockPoolHeader;
  while LPMediumBlockPoolHeader <> @MediumBlockPoolsCircularList do
  begin
    {Add to the medium block used space}
    Inc(AMemoryManagerState.ReservedMediumBlockAddressSpace, MediumBlockPoolSize);
    LPMediumBlock := GetFirstMediumBlockInPool(LPMediumBlockPoolHeader);
    while LPMediumBlock <> nil do
    begin
      LMediumBlockHeader := PCardinal(Cardinal(LPMediumBlock) - 4)^;
      {Is the block in use?}
      if LMediumBlockHeader and IsFreeBlockFlag = 0 then
      begin
        {Get the block size}
        LMediumBlockSize := LMediumBlockHeader and DropMediumAndLargeFlagsMask;
        if IsSmallBlockPool(LPMediumBlock) then
        begin
          {Get the block type index}
          LBlockTypeIndex := (Cardinal(PSmallBlockPoolHeader(LPMediumBlock).BlockType) - Cardinal(@SmallBlockTypes[0])) div SizeOf(TSmallBlockType);
          {Subtract from medium block usage}
          Dec(AMemoryManagerState.ReservedMediumBlockAddressSpace, LMediumBlockSize);
          {Add it to the reserved space for the block size}
          Inc(AMemoryManagerState.SmallBlockTypeStates[LBlockTypeIndex].ReservedAddressSpace, LMediumBlockSize);
          {Add the usage for the pool}
          Inc(AMemoryManagerState.SmallBlockTypeStates[LBlockTypeIndex].AllocatedBlockCount,
            PSmallBlockPoolHeader(LPMediumBlock).BlocksInUse);
        end
        else
        begin
{$ifdef FullDebugMode}
          Dec(LMediumBlockSize, FullDebugBlockOverhead);
{$endif}
          Inc(AMemoryManagerState.AllocatedMediumBlockCount);
          Inc(AMemoryManagerState.TotalAllocatedMediumBlockSize, LMediumBlockSize - BlockHeaderSize);
        end;
      end;
      {Next medium block}
      LPMediumBlock := NextMediumBlock(LPMediumBlock);
    end;
    {Get the next medium block pool}
    LPMediumBlockPoolHeader := LPMediumBlockPoolHeader.NextMediumBlockPoolHeader;
  end;
  MediumBlocksLocked := False;
  {Step through all the large blocks}
  LockLargeBlocks;
  LPLargeBlock := LargeBlocksCircularList.NextLargeBlockHeader;
  while (LPLargeBlock <> @LargeBlocksCircularList) do
  begin
    LLargeBlockSize := LPLargeBlock.BlockSizeAndFlags and DropMediumAndLargeFlagsMask;
    Inc(AMemoryManagerState.AllocatedLargeBlockCount);
    Inc(AMemoryManagerState.ReservedLargeBlockAddressSpace, LLargeBlockSize);
    Inc(AMemoryManagerState.TotalAllocatedLargeBlockSize, LPLargeBlock.UserAllocatedSize);
    {Get the next large block}
    LPLargeBlock := LPLargeBlock.NextLargeBlockHeader;
  end;
  LargeBlocksLocked := False;
end;

{$ifndef LINUX}
{Gets the state of every 64K block in the 4GB address space}
procedure GetMemoryMap(var AMemoryMap: TMemoryMap);
var
  LPMediumBlockPoolHeader: PMediumBlockPoolHeader;
  LPLargeBlock: PLargeBlockHeader;
  LLargeBlockSize, LChunkIndex, LInd: Cardinal;
  LMBI: TMemoryBasicInformation;
begin
  {Clear the map}
  FillChar(AMemoryMap, SizeOf(AMemoryMap), ord(csUnallocated));
  {Step through all the medium block pools}
  LockMediumBlocks;
  LPMediumBlockPoolHeader := MediumBlockPoolsCircularList.NextMediumBlockPoolHeader;
  while LPMediumBlockPoolHeader <> @MediumBlockPoolsCircularList do
  begin
    {Add to the medium block used space}
    LChunkIndex := Cardinal(LPMediumBlockPoolHeader) shr 16;
    for LInd := 0 to (MediumBlockPoolSize - 1) shr 16 do
      AMemoryMap[LChunkIndex + LInd] := csAllocated;
    {Get the next medium block pool}
    LPMediumBlockPoolHeader := LPMediumBlockPoolHeader.NextMediumBlockPoolHeader;
  end;
  MediumBlocksLocked := False;
  {Step through all the large blocks}
  LockLargeBlocks;
  LPLargeBlock := LargeBlocksCircularList.NextLargeBlockHeader;
  while (LPLargeBlock <> @LargeBlocksCircularList) do
  begin
    LChunkIndex := Cardinal(LPLargeBlock) shr 16;
    LLargeBlockSize := LPLargeBlock.BlockSizeAndFlags and DropMediumAndLargeFlagsMask;
    for LInd := 0 to (LLargeBlockSize - 1) shr 16 do
      AMemoryMap[LChunkIndex + LInd] := csAllocated;
    {Get the next large block}
    LPLargeBlock := LPLargeBlock.NextLargeBlockHeader;
  end;
  LargeBlocksLocked := False;
  {Fill in the rest of the map}
  for LInd := 0 to 65535 do
  begin
    {If the chunk is not allocated by this MM, what is its status?}
    if AMemoryMap[LInd] = csUnallocated then
    begin
      {Get all the reserved memory blocks and windows allocated memory blocks, etc.}
      VirtualQuery(Pointer(LInd * 65536), LMBI, SizeOf(LMBI));
      if LMBI.State = MEM_COMMIT then
        AMemoryMap[LInd] := csSysAllocated
      else
        if LMBI.State = MEM_RESERVE then
          AMemoryMap[LInd] := csSysReserved;
    end;
  end;
end;
{$endif}

{Frees all allocated memory.}
procedure FreeAllMemory;
var
  LPMediumBlockPoolHeader, LPNextMediumBlockPoolHeader: PMediumBlockPoolHeader;
  LPMediumFreeBlock: PMediumFreeBlock;
  LPLargeBlock, LPNextLargeBlock: PLargeBlockHeader;
  LInd: integer;
begin
  {Free all block pools}
  LPMediumBlockPoolHeader := MediumBlockPoolsCircularList.NextMediumBlockPoolHeader;
  while LPMediumBlockPoolHeader <> @MediumBlockPoolsCircularList do
  begin
    {Get the next medium block pool so long}
    LPNextMediumBlockPoolHeader := LPMediumBlockPoolHeader.NextMediumBlockPoolHeader;
    {Free this pool}
    VirtualFree(LPMediumBlockPoolHeader, 0, MEM_RELEASE);
    {Next pool}
    LPMediumBlockPoolHeader := LPNextMediumBlockPoolHeader;
  end;
  {Clear all small block types}
  for LInd := 0 to high(SmallBlockTypes) do
  begin
    SmallBlockTypes[Lind].PreviousPartiallyFreePool := @SmallBlockTypes[Lind];
    SmallBlockTypes[Lind].NextPartiallyFreePool := @SmallBlockTypes[Lind];
    SmallBlockTypes[Lind].NextSequentialFeedBlockAddress := pointer(1);
    SmallBlockTypes[Lind].MaxSequentialFeedBlockAddress := nil;
  end;
  {Clear all medium block pools}
  MediumBlockPoolsCircularList.PreviousMediumBlockPoolHeader := @MediumBlockPoolsCircularList;
  MediumBlockPoolsCircularList.NextMediumBlockPoolHeader := @MediumBlockPoolsCircularList;
  {All medium bins are empty}
  for LInd := 0 to high(MediumBlockBins) do
  begin
    LPMediumFreeBlock := @MediumBlockBins[LInd];
    LPMediumFreeBlock.PreviousFreeBlock := LPMediumFreeBlock;
    LPMediumFreeBlock.NextFreeBlock := LPMediumFreeBlock;
  end;
  {Free all large blocks}
  LPLargeBlock := LargeBlocksCircularList.NextLargeBlockHeader;
  while LPLargeBlock <> @LargeBlocksCircularList do
  begin
    {Get the next large block}
    LPNextLargeBlock := LPLargeBlock.NextLargeBlockHeader;
    {Free this large block}
    VirtualFree(LPLargeBlock, 0, MEM_RELEASE);
    {Next large block}
    LPLargeBlock := LPNextLargeBlock;
  end;
  {There are no large blocks allocated}
  LargeBlocksCircularList.PreviousLargeBlockHeader := @LargeBlocksCircularList;
  LargeBlocksCircularList.NextLargeBlockHeader := @LargeBlocksCircularList;
end;

{----------------------------Memory Manager Setup-----------------------------}

{Checks that no other memory manager has been installed after the RTL MM and
 that there are currently no live pointers allocated through the RTL MM.}
function CheckCanInstallMemoryManager: boolean;
begin
  {Default to error}
  Result := False;
  {Is FastMM already installed?}
  if FastMMIsInstalled then
  begin
    MessageBox(0, AlreadyInstalledMsg, AlreadyInstalledTitle,
      MB_OK or MB_ICONERROR or MB_TASKMODAL);
    exit;
  end;
  {Has another MM been set, or has the Borland MM been used? If so, this file
   is not the first unit in the uses clause of the project's .dpr file.}
  if IsMemoryManagerSet then
  begin
    {Another memory manager has been set.}
    MessageBox(0, OtherMMInstalledMsg, OtherMMInstalledTitle,
      MB_OK or MB_ICONERROR or MB_TASKMODAL);
    exit;
  end;
{$ifndef Linux}
  if (GetHeapStatus.TotalAllocated <> 0) then
  begin
    {Memory has been already been allocated with the RTL MM}
    MessageBox(0, MemoryAllocatedMsg, MemoryAllocatedTitle,
      MB_OK or MB_ICONERROR or MB_TASKMODAL);
    exit;
  end;
{$endif}
  {All OK}
  Result := True;
end;

{Initializes the lookup tables for the memory manager}
procedure InitializeMemoryManager;
var
  i, LSizeInd, LMinimumPoolSize, LOptimalPoolSize, LGroupNumber,
    LBlocksPerPool, LPreviousBlockSize: Cardinal;
  LPMediumFreeBlock: PMediumFreeBlock;
begin
  {Initialize the memory manager}
  {-------------Set up the small block types-------------}
  LPreviousBlockSize := 0;
  for i := 0 to high(SmallBlockTypes) do
  begin
    {Set the move procedure}
{$ifdef UseCustomFixedSizeMoveRoutines}
    {The upsize move procedure may move chunks in 16 bytes even with 8-byte
    alignment, since the new size will always be at least 8 bytes bigger than
    the old size.}
    if not Assigned(SmallBlockTypes[i].UpsizeMoveProcedure) then
  {$ifdef UseCustomVariableSizeMoveRoutines}
      SmallBlockTypes[i].UpsizeMoveProcedure := MoveX16L4;
  {$else}
      SmallBlockTypes[i].UpsizeMoveProcedure := @System.Move;
  {$endif}
{$endif}
    {Set the first "available pool" to the block type itself, so that the
     allocation routines know that there are currently no pools with free
     blocks of this size.}
    SmallBlockTypes[i].PreviousPartiallyFreePool := @SmallBlockTypes[i];
    SmallBlockTypes[i].NextPartiallyFreePool := @SmallBlockTypes[i];
    {Set the block size to block type index translation table}
    for LSizeInd := (LPreviousBlockSize div SmallBlockGranularity) to ((SmallBlockTypes[i].BlockSize - 1) div SmallBlockGranularity) do
      AllocSize2SmallBlockTypeIndX4[LSizeInd] := i * 4;
    {Cannot sequential feed yet: Ensure that the next address is greater than
     the maximum address}
    SmallBlockTypes[i].MaxSequentialFeedBlockAddress := pointer(0);
    SmallBlockTypes[i].NextSequentialFeedBlockAddress := pointer(1);
    {Get the mask to use for finding a medium block suitable for a block pool}
    LMinimumPoolSize :=
      ((SmallBlockTypes[i].BlockSize * MinimumSmallBlocksPerPool
        + SmallBlockPoolHeaderSize + MediumBlockGranularity - 1 - MediumBlockSizeOffset)
      and -MediumBlockGranularity) + MediumBlockSizeOffset;
    if LMinimumPoolSize < MinimumMediumBlockSize then
      LMinimumPoolSize := MinimumMediumBlockSize;
    {Get the closest group number for the minimum pool size}
    LGroupNumber := (LMinimumPoolSize - MinimumMediumBlockSize + MediumBlockBinsPerGroup * MediumBlockGranularity div 2)
      div (MediumBlockBinsPerGroup * MediumBlockGranularity);
    {Too large?}
    if LGroupNumber > 7 then
      LGroupNumber := 7;
    {Set the bitmap}
    SmallBlockTypes[i].AllowedGroupsForBlockPoolBitmap := Byte(-(1 shl LGroupNumber));
    {Set the minimum pool size}
    SmallBlockTypes[i].MinimumBlockPoolSize := MinimumMediumBlockSize + LGroupNumber * (MediumBlockBinsPerGroup * MediumBlockGranularity);
    {Get the optimal block pool size}
    LOptimalPoolSize := ((SmallBlockTypes[i].BlockSize * TargetSmallBlocksPerPool
        + SmallBlockPoolHeaderSize + MediumBlockGranularity - 1 - MediumBlockSizeOffset)
      and -MediumBlockGranularity) + MediumBlockSizeOffset;
    {Limit the optimal pool size to within range}
    if LOptimalPoolSize < OptimalSmallBlockPoolSizeLowerLimit then
      LOptimalPoolSize := OptimalSmallBlockPoolSizeLowerLimit;
    if LOptimalPoolSize > OptimalSmallBlockPoolSizeUpperLimit then
      LOptimalPoolSize := OptimalSmallBlockPoolSizeUpperLimit;
    {How many blocks will fit in the adjusted optimal size?}
    LBlocksPerPool := (LOptimalPoolSize - SmallBlockPoolHeaderSize) div SmallBlockTypes[i].BlockSize;
    {Recalculate the optimal pool size to minimize wastage due to a partial
     last block.}
    SmallBlockTypes[i].OptimalBlockPoolSize :=
      ((LBlocksPerPool * SmallBlockTypes[i].BlockSize + SmallBlockPoolHeaderSize + MediumBlockGranularity - 1 - MediumBlockSizeOffset) and -MediumBlockGranularity) + MediumBlockSizeOffset;
{$ifdef CheckHeapForCorruption}
    {Debug checks}
    if (SmallBlockTypes[i].OptimalBlockPoolSize < MinimumMediumBlockSize)
      or (SmallBlockTypes[i].BlockSize div SmallBlockGranularity * SmallBlockGranularity <> SmallBlockTypes[i].BlockSize) then
    begin
  {$ifdef BCB6OrDelphi7AndUp}
      System.Error(reInvalidPtr);
  {$else}
      System.RunError(reInvalidPtr);
  {$endif}
    end;
{$endif}
    {Set the previous small block size}
    LPreviousBlockSize := SmallBlockTypes[i].BlockSize;
  end;
  {-------------------Set up the medium blocks-------------------}
{$ifdef CheckHeapForCorruption}
  {Check that there are no gaps between where the small blocks end and the
   medium blocks start}
  if (((MaximumSmallBlockSize - 3) + (MediumBlockGranularity - 1 + BlockHeaderSize - MediumBlockSizeOffset))
    and -MediumBlockGranularity) + MediumBlockSizeOffset < MinimumMediumBlockSize then
  begin
  {$ifdef BCB6OrDelphi7AndUp}
    System.Error(reInvalidPtr);
  {$else}
    System.RunError(reInvalidPtr);
  {$endif}
  end;
{$endif}
  {There are currently no medium block pools}
  MediumBlockPoolsCircularList.PreviousMediumBlockPoolHeader := @MediumBlockPoolsCircularList;
  MediumBlockPoolsCircularList.NextMediumBlockPoolHeader := @MediumBlockPoolsCircularList;
  {All medium bins are empty}
  for i := 0 to high(MediumBlockBins) do
  begin
    LPMediumFreeBlock := @MediumBlockBins[i];
    LPMediumFreeBlock.PreviousFreeBlock := LPMediumFreeBlock;
    LPMediumFreeBlock.NextFreeBlock := LPMediumFreeBlock;
  end;
  {------------------Set up the large blocks---------------------}
  LargeBlocksCircularList.PreviousLargeBlockHeader := @LargeBlocksCircularList;
  LargeBlocksCircularList.NextLargeBlockHeader := @LargeBlocksCircularList;
  {------------------Set up the debugging structures---------------------}
{$ifdef FullDebugMode}
  {Set up the fake VMT}
  {Copy the basic info from the TFreedObject class}
  System.Move(Pointer(Integer(TFreedObject) + vmtSelfPtr + 4)^,
    FreedObjectVMT.VMTData[vmtSelfPtr + 4], vmtParent - vmtSelfPtr);
  PCardinal(@FreedObjectVMT.VMTData[vmtSelfPtr])^ := Cardinal(@FreedObjectVMT.VMTMethods[0]);
  {Set up the virtual method table}
  for i := 0 to MaxFakeVMTEntries - 1 do
  begin
    PCardinal(@FreedObjectVMT.VMTMethods[low(FreedObjectVMT.VMTMethods) + Integer(i * 4)])^ :=
      Cardinal(@TFreedObject.GetVirtualMethodIndex) + i * 6;
  {$ifdef CatchUseOfFreedInterfaces}
    VMTBadInterface[i] := @TFreedObject.InterfaceError;
  {$endif}
  end;
{$endif}
end;

{Installs the memory manager (InitializeMemoryManager should be called first)}
procedure InstallMemoryManager;
{$ifndef Linux}
var
  i, LCurrentProcessID: Cardinal;
{$endif}
begin
  if not FastMMIsInstalled then
  begin
{$ifndef Linux}
  {$ifdef FullDebugMode}
    {Try to reserve the 64K block}
    ReservedBlock := VirtualAlloc(Pointer(DebugReservedAddress), 65536, MEM_RESERVE, PAGE_NOACCESS);
  {$endif}
    {Build a string identifying the current process}
    LCurrentProcessID := GetCurrentProcessId;
    for i := 0 to 7 do
    begin
      UniqueProcessIDString[8 - i] :=
        HexTable[((LCurrentProcessID shr (i * 4)) and $F)];
    end;
{$endif}
{$ifdef AttemptToUseSharedMM}
    {Is the replacement memory manager already installed for this process?}
    MMWindow := FindWindow('STATIC', PChar(@UniqueProcessIDString[1]));
    if MMWindow = 0 then
    begin
{$endif}
{$ifdef ShareMM}
      {Share the MM with other DLLs? - if this DLL is unloaded, then
       dependent DLLs will cause a crash.}
  {$ifndef ShareMMIfLibrary}
      if not IsLibrary then
  {$endif}
      begin
        {No memory manager installed yet - create the invisible window}
        MMWindow := CreateWindow('STATIC',
          PChar(@UniqueProcessIDString[1]),
          WS_POPUP, 0, 0, 0, 0,
          0, 0, LCurrentProcessID, nil);
        {The window data is a pointer to this memory manager}
        SetWindowLong(MMWindow, GWL_USERDATA, Integer(@NewMemoryManager));
      end;
{$endif}
      {We will be using this memory manager}
{$ifndef FullDebugMode}
      NewMemoryManager.GetMem := FastGetMem;
      NewMemoryManager.FreeMem := FastFreeMem;
      NewMemoryManager.ReallocMem := FastReallocMem;
{$else}
      NewMemoryManager.GetMem := DebugGetMem;
      NewMemoryManager.FreeMem := DebugFreeMem;
      NewMemoryManager.ReallocMem := DebugReallocMem;
{$endif}
      {Owns the MMWindow}
      IsMemoryManagerOwner := True;
{$ifdef AttemptToUseSharedMM}
    end
    else
    begin
      {Get the address of the shared memory manager}
      NewMemoryManager := PMemoryManager(GetWindowLong(MMWindow, GWL_USERDATA))^;
      {The MMWindow is owned by the main program (not this DLL)}
      IsMemoryManagerOwner := False;
    end;
{$endif}
    {Save the old memory manager}
    GetMemoryManager(OldMemoryManager);
    {Replace the memory manager with either this one or the shared one.}
    SetMemoryManager(NewMemoryManager);
    {FastMM is now installed}
    FastMMIsInstalled := True;
{$ifdef ShowInstallUninstallDebugString}
    if IsMemoryManagerOwner then
    begin
      MessageBox(0, FastMMInstallMsg, FastMMInstallUninstallTitle,
        MB_OK or MB_TASKMODAL);
    end
    else
    begin
      MessageBox(0, FastMMInstallSharedMsg, FastMMInstallUninstallTitle,
        MB_OK or MB_TASKMODAL);
    end;
{$endif}
  end;
end;

procedure UninstallMemoryManager;
begin
  {Is this the owner of the shared MM window?}
  if IsMemoryManagerOwner then
  begin
{$ifdef ShareMM}
    {Destroy the window}
    if MMWindow <> 0 then
    begin
      DestroyWindow(MMWindow);
      MMWindow := 0;
    end;
{$endif}
{$ifdef FullDebugMode}
    {Release the reserved block}
    if ReservedBlock <> nil then
    begin
      VirtualFree(ReservedBlock, 0, MEM_RELEASE);
      ReservedBlock := nil;
    end;
{$endif}
  end;
  {Restore the old memory manager}
  SetMemoryManager(OldMemoryManager);
  {Memory manager has been uninstalled}
  FastMMIsInstalled := False;
{$ifdef ShowInstallUninstallDebugString}
  if IsMemoryManagerOwner then
  begin
    MessageBox(0, FastMMuninstallMsg, FastMMInstallUninstallTitle,
      MB_OK or MB_TASKMODAL);
  end
  else
  begin
    MessageBox(0, FastMMUninstallSharedMsg, FastMMInstallUninstallTitle,
      MB_OK or MB_TASKMODAL);
  end;
{$endif}
end;

initialization
  {Initialize all the lookup tables, etc. for the memory manager}
  InitializeMemoryManager;
{$ifndef BCB}
  {Has another MM been set, or has the Borland MM been used? If so, this file
   is not the first unit in the uses clause of the project's .dpr file.}
  if CheckCanInstallMemoryManager then
    InstallMemoryManager;
{$endif}

finalization
{$ifdef FullDebugMode}
  {Unload the debug info library}
  if DebugInfoDll <> 0 then
  begin
    FreeLibrary(DebugInfoDll);
    DebugInfoDll := 0;
    DebugInfoProc := nil;
  end;
{$endif}
  {Restore the old memory manager if FastMM has been installed}
  if FastMMIsInstalled then
  begin
{$ifndef NeverUninstall}
    {Uninstall FastMM}
    UninstallMemoryManager;
{$endif}
    {Do we own the memory manager, or are we just sharing it?}
    if IsMemoryManagerOwner then
    begin
{$ifdef CheckUseOfFreedBlocksOnShutdown}
      CheckBlocksOnShutdown(
  {$ifdef EnableMemoryLeakReporting}
        True
    {$ifdef RequireIDEPresenceForLeakReporting}
        and DelphiIsRunning
    {$endif}
    {$ifdef ManualLeakReportingControl}
        and ReportMemoryLeaks
    {$endif}
  {$else}
        False
  {$endif}
      );
{$else}
  {$ifdef EnableMemoryLeakReporting}
      if True
    {$ifdef RequireIDEPresenceForLeakReporting}
        and DelphiIsRunning
    {$endif}
    {$ifdef ManualLeakReportingControl}
        and ReportMemoryLeaks
    {$endif}
      then
        CheckBlocksOnShutdown(True);
  {$endif}
{$endif}
{$ifdef EnableMemoryLeakReporting}
      {Free the expected memory leaks list}
      if ExpectedMemoryLeaks <> nil then
      begin
        VirtualFree(ExpectedMemoryLeaks, 0, MEM_RELEASE);
        ExpectedMemoryLeaks := nil;
      end;
{$endif}
{$ifndef NeverUninstall}
      {Clean up: Free all memory. If this is a .DLL that owns its own MM, then
       it is necessary to prevent the main application from running out of
       address space.}
      FreeAllMemory;
{$endif}
    end;
  end;

end.
