unit smmTypes;

interface

{$Include smmOptions.inc}

const
  /// alloc memory blocks with 64 memory items each time
  //  64 = 1 shl 6, therefore any multiplication compiles into nice shl opcode
  C_ARRAYSIZE         = 32;  //32 instead of 64 -> smaller overhead
  /// Maximum index of 256 bytes granularity Small blocks
  MAX_SMALLMEMBLOCK   = 8;
  /// 0 - 2048 bytes
  C_MAX_SMALLMEM_SIZE = MAX_SMALLMEMBLOCK * 256; //=2048;

type
  {$if CompilerVersion <= 20}
  // from Delphi 6 up to Delphi 2007
  // also for 2009: http://code.google.com/p/scalemm/issues/detail?id=1
  NativeUInt = Cardinal;
  NativeInt  = Integer;
  Int16      = Smallint;
  Int32      = Integer;
  {$ifend}

  PBaseMemHeader     = ^TBaseMemHeader;
  PBaseFreeMemHeader = ^TBaseFreeMemHeader;
  PBaseBlockMemory   = ^TBaseBlockMemory;
  PBaseSizeManager   = ^TBaseSizeManager;
  PBaseThreadManager = ^TBaseThreadManager;
  PBaseThreadManagerOffset = ^TBaseThreadManagerOffset;

  TBaseMemHeader = {$if CompilerVersion >= 18} //Delphi 2007 and higher?
                   record {$ELSE} object {$ifend}
    {$IFDEF SCALEMM_MAGICTEST}
    Magic1     : NativeInt;
    Magic2     : NativeInt;  //8byte aligned
    {$ELSE}
      {$IFDEF Align16Bytes}
        {$ifndef CPUX64}
        Filler1: Pointer;  // 16 bytes aligned for 32 bit compiler
        Filler2: Pointer;
        {$endif}
      {$ENDIF}
    {$ENDIF}

    //small, medium and large mem can add extra stuff IN FRONT
    Size      : NativeUInt;
    //must be last of "universal" header!
    OwnerBlock: PBaseBlockMemory;
  end;

  TBaseFreeMemHeader = {$if CompilerVersion >= 18} //Delphi 2007 and higher?
                       record {$ELSE} object {$ifend}
    {$IFDEF SCALEMM_MAGICTEST}
    Magic1     : NativeInt;
    Magic2     : NativeInt;  //8byte aligned
    {$ELSE}
      {$IFDEF Align16Bytes}
        {$ifndef CPUX64}
        Filler1: Pointer;  // 16 bytes aligned for 32 bit compiler
        Filler2: Pointer;
        {$endif}
      {$ENDIF}
    {$ENDIF}

    //small, medium and large mem can add extra stuff IN FRONT
    Size       : NativeUInt;
    OwnerBlock : PBaseBlockMemory;

    //Extra data of free item:---------------------------------
    NextThreadFree: PBaseFreeMemHeader;  //linked list of interthread memory
  end;

  TSizeType = (stSmall, stMedium, stLarge);

  TBaseBlockMemory = {$if CompilerVersion >= 18} //Delphi 2007 and higher?
                     record {$ELSE} object {$ifend}
    //SizeType   : TSizeType;
    OwnerManager: PBaseSizeManager;
    //small, medium and large mem can add extra stuff BEHIND
  end;

  TBaseSizeManager = {$if CompilerVersion >= 18} //Delphi 2007 and higher?
                     record {$ELSE} object {$ifend}
    SizeType   : TSizeType;
    OwnerThread: PBaseThreadManager;
    //small, medium and large mem can add extra stuff BEHIND
  end;

  TBaseThreadManager = {$if CompilerVersion >= 18} //Delphi 2007 and higher?
                       record {$ELSE} object {$ifend}
    FOtherThreadFreedMemory: PBaseFreeMemHeader;
    FOtherThreadFreeLock: Boolean;
    FOtherThreadFreeLockRecursion: NativeUInt;

    FThreadId: NativeUInt;
    FThreadTerminated: Boolean;
    //extra stuff BEHIND
  end;

  TScanDirection = (sdNone, sdPrevious, sdNext, sdBoth);

  {$A1}
  TBaseThreadManagerOffset = //{$if CompilerVersion <  23} //Delphi XE2 and below? do not pack!
                             //packed {$ifend}
                             {$if CompilerVersion >= 18} //Delphi 2007 and higher?
                             record {$ELSE} object {$ifend}
  public
    //FOtherThreadFreedMemory: PBaseFreeMemHeader;
    //Filler0: Byte;   //1 or 2 (lowest bits) = medium or large
    Filler1, Filler2, Filler3: Byte;  //offset of 1 to distinguish of being medium or large block
    {$IFDEF CPUX64}
    Filler4: Int32;  //extra offset so we have 8 bytes again
    {$ENDIF}
    FOtherThreadFreeLock: NativeInt;
    FOtherThreadFreeLockRecursion: NativeUInt;

    FThreadId: NativeUInt;
    FThreadTerminated: NativeInt;
    //extra stuff BEHIND
  end;

implementation

{$IFOPT C+}  //assertions
var
  test  : TBaseThreadManager;
  offset: PBaseThreadManagerOffset;
{$ENDIF}

initialization
  {$IFDEF Align8Bytes}
  Assert( SizeOf(TBaseMemHeader) AND 7 = 0);
  {$ENDIF}
  {$IFDEF Align16Bytes}
  Assert( SizeOf(TBaseMemHeader) AND 15 = 0);
  {$ENDIF}

  {$IFOPT C+}  //assertions
  test.FOtherThreadFreedMemory := nil;
  test.FOtherThreadFreeLock    := True;
  test.FThreadId               := MaxInt;
  test.FThreadTerminated       := False;
  offset := PBaseThreadManagerOffset(NativeUInt(@test) or 1);
  //check offset
  Assert(offset.FThreadId = test.FThreadId);
  {$ENDIF}

end.
