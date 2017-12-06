
//////////////////////////////////////////////////
//                                              //
//   QMemory 2.01                               //
//                                              //
//   The alternative quick memory manager       //
//                                              //
//   Copyright (c) 2000,2001 Andrew Driazgov    //
//   e-mail: andrey@asp.tstu.ru                 //
//                                              //
//   Last updated: January 24, 2001             //
//                                              //
//////////////////////////////////////////////////

unit QMemory;

interface

{ QMemory is a new memory manager. You can use it as a replacement of the
  default system memory manager. To do this simply add QMemory unit in your
  project (as the first unit listed in the project file). Don't forget to call
  the QMemDecommitOverstock function when your application is idle. This
  subroutine decommits the unused memory blocks (it's only way for program to
  return the memory to the operation system). All allocated memory blocks are
  32 byte aligned. The minimum size of the block is 32 bytes. As it is
  necessary to store some information with each block a dword is attached to
  the front of each block at -4 the aligned  address. Thus, memory request for
  up to 28 bytes allocates a 32-bytes block, request for 29 to 60 bytes
  allocates a 64-bytes block, etc (as power of 2). This idea was adopted from
  HPMM project of Robert Lee (rhlee@optimalcode.com). The memory is committed
  and decommitted in 64K blocks. The maximum amount of the memory is specified
  when QMemInstall function is called (from the initialization section of the
  unit). You can't change this value later. If some parts of your program
  implemented as DLLs you have to use ShareQmm instead of QMemory unit. }

(*
  QMemory should always be the first unit listed in program uses clause:

  program Project1;

  uses
    QMemory in 'QMemory.pas',        // first unit !!!
    Forms,
    MainUnit in 'MainUnit.pas' {MainForm},
    ...

  {$R *.RES}

  begin
    Application.Initialize;
    Application.Title := 'Project1';
    Application.CreateForm(TMainForm, MainForm);
    ...
    Application.Run;
  end.
*)

{ QMemInstall creates a custom heap and sets the entry points of the memory
  manager to the three functions of this unit which work with a created heap.
  The function reserves space in the virtual address space of the process and
  allocates physical storage for a specified initial portion of this block.
  InitialSize specifies the initial size, in bytes, of the heap. This value
  determines the initial amount of physical storage that is allocated for the
  heap. The value is rounded up to the next 64-kilobyte boundary. MaximumSize
  specifies the maximum size, in bytes, of the heap. The QMemInstall function
  rounds MaximumSize up to the next 512-kilobyte boundary, and then reserves
  a block of that size in the process's virtual address space for the heap.
  If allocation requests made by the GetMem or ReallocMem (or New) exceed the
  initial amount of physical storage specified by InitialSize, the system
  allocates additional pages of physical storage for the heap, up to the
  heap's maximum size. If the function succeeds, the return value is 0. If
  the function fails, it returns -1.

  QMemInstall is called from the initialization section of this unit. }

function QMemInstall(InitialSize, MaximumSize: Integer): Integer;

{ QMemRelease destroys the custom heap and restores the previous memory
  manager. QMemRelease decommits and releases all the pages of a heap. If the
  function succeeds, the return value is 0. If the function fails, the return
  value is -1.

  QMemRelease is called from the finalization section of this unit. }

function QMemRelease: Integer;

{ QMemDecommitOverstock decommits large free blocks of the memory. You may
  want to call the function from Application.Idle event handler. This function
  is only way (except QMemRelease) for decommit pages of the physical storage.
  If the function succeeds, the return value is 0, otherwise it returns -1. }

function QMemDecommitOverstock: Integer;

{ QMemSize returns the size, in bytes, of a memory block allocated from a
  custom heap. P is a pointer to the memory block whose size the function
  will obtain. The custom heap has to be installed. If the function succeeds,
  the return value is the size, in bytes, of the allocated memory block.
  If the function fails, the return value is -1. }

function QMemSize(P: Pointer): Integer;

{ QMemTotalAddrSpace returns the total address space of the custom heap,
  in bytes. This is fixed and will not grow as your program's dynamic memory
  usage grows. TotalUncommitted + TotalCommitted = TotalAddrSpace. The value
  is equal to MaximumSize, which you have specified when called QMemInstall
  function. If QMemTotalAddrSpace function fails, the return value is -1. }

function QMemTotalAddrSpace: Integer;

{ QMemTotalCommitted returns the total number of bytes (of TotalAddrSpace)
  for which space has been allocated in the swap file. If the function fails,
  the return value is -1. }

function QMemTotalCommitted: Integer;

{ QMemTotalUncommitted returns the total number of bytes (of TotalAddrSpace)
  for which space has not been allocated in the swap file. If the function
  fails, the return value is -1. }

function QMemTotalUncommitted: Integer;

{ QMemTotalAllocated returns the total number of bytes dynamically allocated
  by your program. It includes 4 bytes at the beginning of each memory block
  and the trailing bytes for maintain of 32-bytes align of a memory blocks.
  If the function fails, the return value is -1. }

function QMemTotalAllocated: Integer;

{ QMemTotalFree returns the total number of free bytes available in the
  custom heap for allocation by your program. If the function fails, the
  return value is -1. }

function QMemTotalFree: Integer;

{ QMemMaxFreeBlock returns the size, in bytes, of the maximum memory block
  which you can allocate in the custom heap. You can pass this value to
  the GetMem procedure (if you have physical storage of enought size).
  If the function fails, the return value is -1. }

function QMemMaxFreeBlock: Integer;

{ QMemCountOfFreeBlocks returns the total number of free blocks in the custom
  heap address space. If the function fails, the return value is -1. }

function QMemCountOfFreeBlocks: Integer;

{ QMemOverhead returns the total number of bytes required by the heap manager
  to manage all the blocks dynamically allocated by your program. More
  precisely, it returns the total size, in bytes, of additional committed
  space. If the function fails, the return value is -1. }

function QMemOverhead: Integer;

{ QMemGetHeapStatus returns the current status of the custom memory manager
  in a TQMemHeapStatus record. The fields of this record have been described
  above (they are analogues of the corresponding functions). If the function
  fails, all fields are 0. }

type
  TQMemHeapStatus = record
    TotalAddrSpace: Cardinal;
    TotalCommitted: Cardinal;
    TotalUncommitted: Cardinal;
    TotalAllocated: Cardinal;
    TotalFree: Cardinal;
    MaxFreeBlock: Cardinal;
    CountOfFreeBlocks: Cardinal;
    Overhead: Cardinal;
  end;

function QMemGetHeapStatus: TQMemHeapStatus;

{ QMemSetMaxECount assigns a new value to the internal variable which limits
  maximum number of free fragments in the custom heap. The default value is
  65536. If you want to save some virtual address space you may set this
  variable less (or more, if it is necessary). This function should be called
  before QMemInstall (when the memory manager is not installed yet). If the
  function succeeds, it returns 0. If the function fails, it returns -1. }

function QMemSetMaxECount(Value: Integer): Integer;

implementation

uses Windows;

type
  PEntryPoint = ^TEntryPoint;
  TEntryPoint = packed record
    Address: LongWord;
    Space: LongWord;
    AdrLeft: PEntryPoint;
    AdrRight: PEntryPoint;
    SpLeft: PEntryPoint;
    SpRight: PEntryPoint;
  end;

  PEFreeArr = ^TEFreeArr;
  TEFreeArr = array[0..$7FFFFF] of PEntryPoint;

var
  lpCriticalSection:_RTL_CRITICAL_SECTION;

  ListLeft: PEntryPoint;
  ListRight: PEntryPoint;

  SizeTable: array[0..30] of PEntryPoint;

  EFreeCount: Integer;
  EFreeArr: PEFreeArr;

  StartAddr: LongWord;
  SpaceBegin: LongWord;

  MaxECount: Integer = $10000;

  QMemIsInstalled: Boolean = False;

  OldMemManager: TMemoryManager;

function GetNormalSize(Size: Integer): Integer;
asm
        ADD     EAX,3
        TEST    EAX,$FFFFFFE0
        JE      @@sm
        BSR     ECX,EAX
        MOV     EAX,2
        SHL     EAX,CL
        RET
@@sm:   MOV     EAX,32
end;

function GetRegionOfSize(Size: Integer): Pointer;
asm
        BSF     ECX,EAX
        LEA     EDX,[ECX*4+SizeTable]
        MOV     ECX,[EDX]
        TEST    ECX,ECX
        JE      @@nx
        MOV     EAX,ECX
        MOV     ECX,[ECX].TEntryPoint.SpRight
        MOV     [EDX],ECX
        TEST    ECX,ECX
        JE      @@qm
        XOR     EDX,EDX
        MOV     [ECX].TEntryPoint.SpLeft,EDX
@@qm:   RET
@@nx:   MOV     EDX,EAX
        MOV     EAX,ListLeft
        TEST    EAX,EAX
        JE      @@qt
@@lp:   CMP     EDX,[EAX].TEntryPoint.Space
        JLE     @@qt
        MOV     EAX,[EAX].TEntryPoint.AdrRight
        TEST    EAX,EAX
        JNE     @@lp
@@qt:
end;

function IntBitTest(P: Pointer; Index: Integer): Boolean;
asm
        BT      [EAX],EDX
        SETC    AL
end;

procedure IntBitSet(P: Pointer; Index: Integer);
asm
        BTS     [EAX],EDX
end;

function IntFreeBitScanForward(P: Pointer; FirstBit, LastBit: Integer): Integer;
asm
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        LEA     ESI,[EDX+8]
        CMP     ECX,ESI
        JL      @@ut
        MOV     EBX,$FFFFFFFF
        MOV     ESI,ECX
        MOV     EDI,$0000001F
        AND     ECX,EDI
        AND     ESI,$FFFFFFE0
        SUB     EDI,ECX
        SHR     ESI,5
        MOV     ECX,EDI
        MOV     EDI,EBX
        SHR     EDI,CL
        MOV     ECX,EDX
        AND     EDX,$FFFFFFE0
        AND     ECX,$0000001F
        SHR     EDX,5
        SHL     EBX,CL
        MOV     ECX,[EAX+EDX*4]
        NOT     ECX
        AND     EBX,ECX
        SUB     ESI,EDX
        JE      @@nq
        TEST    EBX,EBX
        JNE     @@ne
        INC     EDX
        DEC     ESI
        JE      @@xx
@@lp:   MOV     EBX,[EAX+EDX*4]
        NOT     EBX
        TEST    EBX,EBX
        JNE     @@ne
        INC     EDX
        DEC     ESI
        JNE     @@lp
@@xx:   MOV     EBX,[EAX+EDX*4]
        NOT     EBX
@@nq:   AND     EBX,EDI
        JE      @@zq
@@ne:   BSF     ECX,EBX
@@qt:   SHL     EDX,5
        LEA     EAX,[ECX+EDX]
        POP     EDI
        POP     ESI
        POP     EBX
        RET
@@ut:   SUB     ECX,EDX
        JS      @@zq
@@uk:   BT      [EAX],EDX
        JNC     @@iq
        INC     EDX
        DEC     ECX
        JNS     @@uk
@@zq:   MOV     EAX,$FFFFFFFF
        POP     EDI
        POP     ESI
        POP     EBX
        RET
@@iq:   MOV     EAX,EDX
        POP     EDI
        POP     ESI
        POP     EBX
end;

function IntFreeBitScanReverse(P: Pointer; FirstBit, LastBit: Integer): Integer;
asm
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        LEA     ESI,[EDX+8]
        CMP     ECX,ESI
        JL      @@ut
        MOV     EBX,$FFFFFFFF
        MOV     ESI,ECX
        MOV     EDI,$0000001F
        AND     ECX,EDI
        AND     ESI,$FFFFFFE0
        SUB     EDI,ECX
        SHR     ESI,5
        MOV     ECX,EDI
        MOV     EDI,EBX
        SHR     EDI,CL
        MOV     ECX,EDX
        AND     EDX,$FFFFFFE0
        AND     ECX,$0000001F
        SHR     EDX,5
        SHL     EBX,CL
        MOV     ECX,[EAX+ESI*4]
        NOT     ECX
        AND     EDI,ECX
        SUB     EDX,ESI
        JE      @@nq
        TEST    EDI,EDI
        JNE     @@ne
        NEG     EDX
        DEC     ESI
        DEC     EDX
        JE      @@xx
@@lp:   MOV     EDI,[EAX+ESI*4]
        NOT     EDI
        TEST    EDI,EDI
        JNE     @@ne
        DEC     ESI
        DEC     EDX
        JNE     @@lp
@@xx:   MOV     EDI,[EAX+ESI*4]
        NOT     EDI
@@nq:   AND     EDI,EBX
        JE      @@zq
@@ne:   BSR     ECX,EDI
@@qt:   SHL     ESI,5
        LEA     EAX,[ECX+ESI]
        POP     EDI
        POP     ESI
        POP     EBX
        RET
@@ut:   SUB     EDX,ECX
        JG      @@zq
@@uk:   BT      [EAX],ECX
        JNC     @@iq
        DEC     ECX
        INC     EDX
        JNG     @@uk
@@zq:   MOV     EAX,$FFFFFFFF
        POP     EDI
        POP     ESI
        POP     EBX
        RET
@@iq:   MOV     EAX,ECX
        POP     EDI
        POP     ESI
        POP     EBX
end;

procedure IntSetBits(P: Pointer; FirstBit, LastBit: Integer);
asm
        PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        LEA     ESI,[EDX+8]
        CMP     ECX,ESI
        JL      @@ut
        MOV     EBX,$FFFFFFFF
        MOV     ESI,ECX
        MOV     EDI,$0000001F
        AND     ECX,EDI
        AND     ESI,$FFFFFFE0
        SUB     EDI,ECX
        SHR     ESI,5
        MOV     ECX,EDI
        MOV     EDI,EBX
        SHR     EDI,CL
        MOV     ECX,EDX
        AND     EDX,$FFFFFFE0
        AND     ECX,$0000001F
        SHR     EDX,5
        SHL     EBX,CL
        SUB     ESI,EDX
        JE      @@xx
        OR      [EAX+EDX*4],EBX
        INC     EDX
        DEC     ESI
        JE      @@ne
        MOV     EBX,$FFFFFFFF
@@lp:   MOV     [EAX+EDX*4],EBX
        INC     EDX
        DEC     ESI
        JNE     @@lp
@@xx:   AND     EDI,EBX
@@ne:   OR      [EAX+EDX*4],EDI
        POP     EBX
        POP     ESI
        POP     EDI
        RET
@@ut:   SUB     ECX,EDX
        JS      @@qt
@@uk:   BTS     [EAX],EDX
        INC     EDX
        DEC     ECX
        JNS     @@uk
@@qt:   POP     EBX
        POP     ESI
        POP     EDI
end;

procedure DelFromSizeTable(E: PEntryPoint);
asm
        MOV     EDX,[EAX].TEntryPoint.SpLeft
        TEST    EDX,EDX
        JNE     @@nx
        MOV     EDX,[EAX].TEntryPoint.Space
        BSF     ECX,EDX
        LEA     EDX,[ECX*4+SizeTable]
        CMP     EAX,[EDX]
        JNE     @@qt
        MOV     ECX,[EAX].TEntryPoint.SpRight
        MOV     [EDX],ECX
        TEST    ECX,ECX
        JE      @@qt
        XOR     EDX,EDX
        MOV     [ECX].TEntryPoint.SpLeft,EDX
@@qt:   RET
@@nx:   MOV     ECX,[EAX].TEntryPoint.SpRight
        MOV     [EDX].TEntryPoint.SpRight,ECX
        TEST    ECX,ECX
        JE      @@qx
        MOV     [ECX].TEntryPoint.SpLeft,EDX
@@qx:
end;

function IntGetMem(Size: Integer): Pointer;
label
  99;
var
  E: PEntryPoint;
  I,J: Integer;
begin
  try
    if IsMultiThread then
      EnterCriticalSection(lpCriticalSection);
    Size := GetNormalSize(Size);
    E := GetRegionOfSize(Size);
    if E <> nil then
    begin
      J := E^.Address-StartAddr;
      I := J;
      Inc(J,Size);
      I := LongWord(I) shr 16;
      Dec(J);
      J := LongWord(J) shr 16;
      if I = J then
      begin
        if not IntBitTest(Pointer(StartAddr),I) then
        begin
          if VirtualAlloc(Pointer(StartAddr+LongWord(I) shl 16), $10000,
            MEM_COMMIT, PAGE_READWRITE) = nil then
          begin
            Result := nil;
            goto 99;
          end;
          IntBitSet(Pointer(StartAddr),I);
        end;
      end else
      begin
        I := IntFreeBitScanForward(Pointer(StartAddr),I,J);
        if I >= 0 then
        begin
          J := IntFreeBitScanReverse(Pointer(StartAddr),I,J);
          if VirtualAlloc(Pointer(StartAddr+LongWord(I) shl 16),
            LongWord(J-I+1) shl 16, MEM_COMMIT, PAGE_READWRITE) = nil then
          begin
            Result := nil;
            goto 99;
          end;
          IntSetBits(Pointer(StartAddr),I,J);
        end;
      end;
      with E^ do
      begin
        Result := Pointer(Address+4);
        PInteger(Address)^ := Size;
        I := Integer(Space)-Size;
        if I = 0 then
        begin
          EFreeArr^[EFreeCount] := E;
          Inc(EFreeCount);
          if AdrLeft <> nil then
            AdrLeft^.AdrRight := AdrRight
          else
            ListLeft := AdrRight;
          if AdrRight <> nil then
            AdrRight^.AdrLeft := AdrLeft
          else
            ListRight := AdrLeft;
        end else
        begin
          DelFromSizeTable(E);
          Space := I;
          Inc(Address,Size);
          SpLeft := nil;
        end;
      end;
    end else
      Result := nil;
  99:
  finally
    if IsMultiThread then
      LeaveCriticalSection(lpCriticalSection);
  end;
end;

function SearchPointerPlace(P: Pointer): PEntryPoint;
asm
        MOV     EDX,EAX
        MOV     EAX,ListRight
        TEST    EAX,EAX
        JE      @@qt
@@lp:   CMP     EDX,[EAX].TEntryPoint.Address
        JAE     @@qt
        MOV     EAX,[EAX].TEntryPoint.AdrLeft
        TEST    EAX,EAX
        JNE     @@lp
@@qt:
end;

procedure SetInSizeTable(E: PEntryPoint; L: LongWord);
asm
        BSF     ECX,EDX
        LEA     EDX,[ECX*4+SizeTable]
        MOV     ECX,[EDX]
        MOV     [EDX],EAX
        MOV     [EAX].TEntryPoint.SpRight,ECX
        TEST    ECX,ECX
        JE      @@qt
        MOV     [ECX].TEntryPoint.SpLeft,EAX
@@qt:
end;

function IntFreeMem(P: Pointer): Integer;
label
  99;
var
  E,E1: PEntryPoint;
  J: LongWord;
begin
  Dec(LongWord(P),4);
  if (LongWord(P)<SpaceBegin) or
    (PLongWord(P)^+LongWord(P)>LongWord(EFreeArr)) then
  begin
    Inc(LongWord(P),4);
    Result := OldMemManager.FreeMem(P);
    Exit;
  end;
  try
    if IsMultiThread then
      EnterCriticalSection(lpCriticalSection);
    E := SearchPointerPlace(P);
    if E <> nil then
    begin
      J := E^.Address+E^.Space;
      if LongWord(P) <= J then
      begin
        if LongWord(P) = J then
        begin
          J := PLongWord(P)^;
          if E <> ListRight then
          begin
            Inc(LongWord(P),J);
            E1 := E^.AdrRight;
            if LongWord(P) >= E1.Address then
            begin
              if LongWord(P) = E1.Address then
              begin
                DelFromSizeTable(E1);
                Inc(J,E^.Space);
                EFreeArr^[EFreeCount] := E;
                Inc(EFreeCount);
                DelFromSizeTable(E);
                with E1^ do
                begin
                  Dec(Address,J);
                  Inc(Space,J);
                  AdrLeft := E^.AdrLeft;
                  if AdrLeft <> nil then
                    AdrLeft^.AdrRight := E1
                  else
                    ListLeft := E1;
                  SpLeft := nil;
                end;
                Result := 0;
              end else
                Result := -1;
              goto 99;
            end;
          end;
          DelFromSizeTable(E);
          Inc(E^.Space,J);
          E^.SpLeft := nil;
          Result := 0;
        end else
          Result := -1;
        goto 99;
      end;
      E := E^.AdrRight;
    end else
      E := ListLeft;
    if E <> nil then
    begin
      J := PLongWord(P)^;
      Inc(J,LongWord(P));
      with E^ do
        if J >= Address then
        begin
          if J = Address then
          begin
            DelFromSizeTable(E);
            Address := LongWord(P);
            Inc(Space,PLongWord(P)^);
            SpLeft := nil;
            Result := 0;
          end else
            Result := -1;
          goto 99;
        end;
    end;
    if EFreeCount > 0 then
    begin
      Dec(EFreeCount);
      E1 := EFreeArr^[EFreeCount];
      with E1^ do
      begin
        Address := LongWord(P);
        Space := PLongWord(P)^;
        AdrRight := E;
        if E <> nil then
        begin
          AdrLeft := E^.AdrLeft;
          E^.AdrLeft := E1;
        end else
        begin
          AdrLeft := ListRight;
          ListRight := E1;
        end;
        if AdrLeft <> nil then
          AdrLeft^.AdrRight := E1
        else
          ListLeft := E1;
        SetInSizeTable(E1,Space);
        SpLeft := nil;
      end;
      Result := 0;
    end else
      Result := -1;
  99:
  finally
    if IsMultiThread then
      LeaveCriticalSection(lpCriticalSection);
  end;
end;

procedure IntCopyMem(Source, Dest: Pointer; L: Cardinal);
asm
        PUSH    EBX
        SUB     EDX,4
        SHR     ECX,5
        JMP     @@fs
@@lp:   MOV     EBX,[EAX]
        MOV     [EDX],EBX
@@fs:   MOV     EBX,[EAX+4]
        MOV     [EDX+4],EBX
        MOV     EBX,[EAX+8]
        MOV     [EDX+8],EBX
        MOV     EBX,[EAX+12]
        MOV     [EDX+12],EBX
        MOV     EBX,[EAX+16]
        MOV     [EDX+16],EBX
        MOV     EBX,[EAX+20]
        MOV     [EDX+20],EBX
        MOV     EBX,[EAX+24]
        MOV     [EDX+24],EBX
        MOV     EBX,[EAX+28]
        MOV     [EDX+28],EBX
        ADD     EAX,32
        ADD     EDX,32
        DEC     ECX
        JNE     @@lp
@@qt:   POP     EBX
end;

function IntReallocMem(P: Pointer; Size: Integer): Pointer;
label
  99;
var
  E,E1: PEntryPoint;
  J,K: Integer;
begin
  Dec(LongWord(P),4);
  if (LongWord(P)<SpaceBegin) or
    (PLongWord(P)^+LongWord(P)>LongWord(EFreeArr)) then
  begin
    Inc(LongWord(P),4);
    Result := OldMemManager.ReallocMem(P,Size);
    Exit;
  end;
  Size := GetNormalSize(Size);
  J := PInteger(P)^;
  if Size = J then
  begin
    Result := Pointer(LongWord(P)+4);
    Exit;
  end;
  try
    if IsMultiThread then
      EnterCriticalSection(lpCriticalSection);
    E := SearchPointerPlace(P);
    if E <> nil then
    begin
      if LongWord(P) < E^.Address+E^.Space then
      begin
        Result := nil;
        goto 99;
      end;
      E := E^.AdrRight;
    end else
      E := ListLeft;
    if E <> nil then
      with E^ do
        if LongWord(J)+LongWord(P) >= Address then
        begin
          if LongWord(J)+LongWord(P) = Address then
          begin
            if Size <= J+Integer(Space) then
            begin
              if Size > J then
              begin
                J := (Address-StartAddr) shr 16;
                K := (LongWord(P)+LongWord(Size)-StartAddr-1) shr 16;
                if J = K then
                begin
                  if not IntBitTest(Pointer(StartAddr),J) then
                  begin
                    if VirtualAlloc(Pointer(StartAddr+LongWord(J) shl 16), $10000,
                      MEM_COMMIT, PAGE_READWRITE) = nil then
                    begin
                      Result := nil;
                      goto 99;
                    end;
                    IntBitSet(Pointer(StartAddr),J);
                  end;
                end else
                begin
                  J := IntFreeBitScanForward(Pointer(StartAddr),J,K);
                  if J >= 0 then
                  begin
                    K := IntFreeBitScanReverse(Pointer(StartAddr),J,K);
                    if VirtualAlloc(Pointer(StartAddr+LongWord(J) shl 16),
                      LongWord(K-J+1) shl 16, MEM_COMMIT, PAGE_READWRITE) = nil then
                    begin
                      Result := nil;
                      goto 99;
                    end;
                    IntSetBits(Pointer(StartAddr),J,K);
                  end;
                end;
              end;
              DelFromSizeTable(E);
              Inc(Space,PLongWord(P)^-LongWord(Size));
              PInteger(P)^ := Size;
              if Space <> 0 then
              begin
                Address := LongWord(P)+LongWord(Size);
                SpLeft := nil;
              end else
              begin
                EFreeArr^[EFreeCount] := E;
                Inc(EFreeCount);
                if AdrLeft <> nil then
                  AdrLeft^.AdrRight := AdrRight
                else
                  ListLeft := AdrRight;
                if AdrRight <> nil then
                  AdrRight^.AdrLeft := AdrLeft
                else
                  ListRight := AdrLeft;
              end;
              Result := Pointer(LongWord(P)+4);
            end else
            begin
              Result := IntGetMem(Size-4);
              if Result <> nil then
              begin
                IntCopyMem(P,Result,PLongWord(P)^);
                Inc(LongWord(P),4);
                IntFreeMem(P);
              end;
            end;
          end else
            Result := nil;
          goto 99;
        end;
    if Size > J then
    begin
      Result := IntGetMem(Size-4);
      if Result <> nil then
      begin
        IntCopyMem(P,Result,J);
        Inc(LongWord(P),4);
        IntFreeMem(P);
      end;
    end
    else if EFreeCount > 0 then
    begin
      Dec(EFreeCount);
      E1 := EFreeArr^[EFreeCount];
      with E1^ do
      begin
        Address := LongWord(P)+LongWord(Size);
        Space := PLongWord(P)^-LongWord(Size);
        AdrRight := E;
        if E <> nil then
        begin
          AdrLeft := E^.AdrLeft;
          E^.AdrLeft := E1;
        end else
        begin
          AdrLeft := ListRight;
          ListRight := E1;
        end;
        if AdrLeft <> nil then
          AdrLeft^.AdrRight := E1
        else
          ListLeft := E1;
        SpLeft := nil;
      end;
      PInteger(P)^ := Size;
      Result := Pointer(LongWord(P)+4);
    end else
      Result := nil;
  99:
  finally
    if IsMultiThread then
      LeaveCriticalSection(lpCriticalSection);
  end;
end;

function IntSetBitScanForward(P: Pointer; FirstBit, LastBit: Integer): Integer;
asm
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        LEA     ESI,[EDX+8]
        CMP     ECX,ESI
        JL      @@ut
        MOV     EBX,$FFFFFFFF
        MOV     ESI,ECX
        MOV     EDI,$0000001F
        AND     ECX,EDI
        AND     ESI,$FFFFFFE0
        SUB     EDI,ECX
        SHR     ESI,5
        MOV     ECX,EDI
        MOV     EDI,EBX
        SHR     EDI,CL
        MOV     ECX,EDX
        AND     EDX,$FFFFFFE0
        AND     ECX,$0000001F
        SHR     EDX,5
        SHL     EBX,CL
        AND     EBX,[EAX+EDX*4]
        SUB     ESI,EDX
        JE      @@nq
        TEST    EBX,EBX
        JNE     @@ne
        INC     EDX
        DEC     ESI
        JE      @@xx
@@lp:   OR      EBX,[EAX+EDX*4]
        JNE     @@ne
        INC     EDX
        DEC     ESI
        JNE     @@lp
@@xx:   MOV     EBX,[EAX+EDX*4]
@@nq:   AND     EBX,EDI
        JE      @@zq
@@ne:   BSF     ECX,EBX
@@qt:   SHL     EDX,5
        LEA     EAX,[ECX+EDX]
        POP     EDI
        POP     ESI
        POP     EBX
        RET
@@ut:   SUB     ECX,EDX
        JS      @@zq
@@uk:   BT      [EAX],EDX
        JC      @@iq
        INC     EDX
        DEC     ECX
        JNS     @@uk
@@zq:   MOV     EAX,$FFFFFFFF
        POP     EDI
        POP     ESI
        POP     EBX
        RET
@@iq:   MOV     EAX,EDX
        POP     EDI
        POP     ESI
        POP     EBX
end;

function IntSetBitScanReverse(P: Pointer; FirstBit, LastBit: Integer): Integer;
asm
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        LEA     ESI,[EDX+8]
        CMP     ECX,ESI
        JL      @@ut
        MOV     EBX,$FFFFFFFF
        MOV     ESI,ECX
        MOV     EDI,$0000001F
        AND     ECX,EDI
        AND     ESI,$FFFFFFE0
        SUB     EDI,ECX
        SHR     ESI,5
        MOV     ECX,EDI
        MOV     EDI,EBX
        SHR     EDI,CL
        MOV     ECX,EDX
        AND     EDX,$FFFFFFE0
        AND     ECX,$0000001F
        SHR     EDX,5
        SHL     EBX,CL
        AND     EDI,[EAX+ESI*4]
        SUB     EDX,ESI
        JE      @@nq
        TEST    EDI,EDI
        JNE     @@ne
        NEG     EDX
        DEC     ESI
        DEC     EDX
        JE      @@xx
@@lp:   OR      EDI,[EAX+ESI*4]
        JNE     @@ne
        DEC     ESI
        DEC     EDX
        JNE     @@lp
@@xx:   MOV     EDI,[EAX+ESI*4]
@@nq:   AND     EDI,EBX
        JE      @@zq
@@ne:   BSR     ECX,EDI
@@qt:   SHL     ESI,5
        LEA     EAX,[ECX+ESI]
        POP     EDI
        POP     ESI
        POP     EBX
        RET
@@ut:   SUB     EDX,ECX
        JG      @@zq
@@uk:   BT      [EAX],ECX
        JC      @@iq
        DEC     ECX
        INC     EDX
        JNG     @@uk
@@zq:   MOV     EAX,$FFFFFFFF
        POP     EDI
        POP     ESI
        POP     EBX
        RET
@@iq:   MOV     EAX,ECX
        POP     EDI
        POP     ESI
        POP     EBX
end;

procedure IntResetBits(P: Pointer; FirstBit, LastBit: Integer);
asm
        PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        LEA     ESI,[EDX+8]
        CMP     ECX,ESI
        JL      @@ut
        MOV     EBX,$FFFFFFFF
        MOV     ESI,ECX
        MOV     EDI,$0000001F
        AND     ECX,EDI
        AND     ESI,$FFFFFFE0
        SUB     EDI,ECX
        SHR     ESI,5
        MOV     ECX,EDI
        MOV     EDI,EBX
        SHR     EDI,CL
        MOV     ECX,EDX
        AND     EDX,$FFFFFFE0
        AND     ECX,$0000001F
        SHR     EDX,5
        SHL     EBX,CL
        NOT     EDI
        NOT     EBX
        SUB     ESI,EDX
        JE      @@xx
        AND     [EAX+EDX*4],EBX
        INC     EDX
        DEC     ESI
        JE      @@ne
        XOR     EBX,EBX
@@lp:   MOV     [EAX+EDX*4],EBX
        INC     EDX
        DEC     ESI
        JNE     @@lp
@@xx:   OR      EDI,EBX
@@ne:   AND     [EAX+EDX*4],EDI
        POP     EBX
        POP     ESI
        POP     EDI
        RET
@@ut:   SUB     ECX,EDX
        JS      @@qt
@@uk:   BTR     [EAX],EDX
        INC     EDX
        DEC     ECX
        JNS     @@uk
@@qt:   POP     EBX
        POP     ESI
        POP     EDI
end;

function QMemDecommitOverstock: Integer;
label
  99;
var
  E: PEntryPoint;
  L,R: Integer;
begin
  if QMemIsInstalled then
  begin
    try
      if IsMultiThread then
        EnterCriticalSection(lpCriticalSection);
      E := ListLeft;
      while E <> nil do
      begin
        if E^.Space shr 17 > 0 then
        begin
          L := (E^.Address+$FFFF-StartAddr) shr 16;
          R := (E^.Address+E^.Space-$10000-StartAddr) shr 16;
          L := IntSetBitScanForward(Pointer(StartAddr),L,R);
          if L >= 0 then
          begin
            R := IntSetBitScanReverse(Pointer(StartAddr),L,R);
            if not VirtualFree(Pointer(StartAddr+LongWord(L) shl 16),
              LongWord(R-L+1) shl 16, MEM_DECOMMIT) then
            begin
              Result := -1;
              goto 99;
            end;
            IntResetBits(Pointer(StartAddr),L,R);
          end;
        end;
        E := E^.AdrRight;
      end;
      Result := 0;
    99:
    finally
      if IsMultiThread then
        LeaveCriticalSection(lpCriticalSection);
    end;
  end else
    Result := -1;
end;

function QMemSize(P: Pointer): Integer;
begin
  Dec(LongWord(P),4);
  if (LongWord(P)>=SpaceBegin) and
      (PLongWord(P)^+LongWord(P)<=LongWord(EFreeArr)) then
    Result := PInteger(P)^-4
  else
    Result := -1;
end;

function QMemTotalAddrSpace: Integer;
begin
  if QMemIsInstalled then
    Result := LongWord(EFreeArr)-StartAddr
  else
    Result := -1;
end;

const
  BitTable: array[0..255] of Byte =
    (0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4,1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,
     1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
     1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
     2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
     1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
     2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
     2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
     3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,4,5,5,6,5,6,6,7,5,6,6,7,6,7,7,8);

function IntCountOfSetBits(P: Pointer; L: Cardinal): Cardinal;
asm
        PUSH    EBX
        PUSH    ESI
        MOV     EBX,EAX
        XOR     EAX,EAX
        SUB     EDX,2
        JS      @@nx
@@lp:   MOVZX   ECX,BYTE PTR [EBX+EDX]
        MOVZX   ESI,BYTE PTR [EBX+EDX+1]
        MOVZX   ECX,BYTE PTR [ECX+BitTable]
        ADD     EAX,ECX
        MOVZX   ESI,BYTE PTR [ESI+BitTable]
        ADD     EAX,ESI
        SUB     EDX,2
        JNS     @@lp
@@nx:   INC     EDX
        JZ      @@qt2
        POP     ESI
        POP     EBX
        RET
@@qt2:  MOVZX   ECX,BYTE PTR [EBX]
        MOVZX   ECX,BYTE PTR [ECX+BitTable]
        ADD     EAX,ECX
        POP     ESI
        POP     EBX
end;

function IntCountOfFreeBits(P: Pointer; L: Cardinal): Cardinal;
asm
        PUSH    EDX
        CALL    IntCountOfSetBits
        NEG     EAX
        POP     EDX
        LEA     EAX,[EAX+EDX*8]
end;

function QMemTotalCommitted: Integer;
begin
  if QMemIsInstalled then
  begin
    try
      if IsMultiThread then
        EnterCriticalSection(lpCriticalSection);
      Result := IntCountOfSetBits(Pointer(StartAddr),
        (LongWord(EFreeArr)-StartAddr) shr 19) shl 16;
    finally
      if IsMultiThread then
        LeaveCriticalSection(lpCriticalSection);
    end;
  end else
    Result := -1;
end;

function QMemTotalUncommitted: Integer;
begin
  if QMemIsInstalled then
  begin
    try
      if IsMultiThread then
        EnterCriticalSection(lpCriticalSection);
      Result := IntCountOfFreeBits(Pointer(StartAddr),
        (LongWord(EFreeArr)-StartAddr) shr 19) shl 16;
    finally
      if IsMultiThread then
        LeaveCriticalSection(lpCriticalSection);
    end;
  end else
    Result := -1;
end;

function QMemTotalAllocated: Integer;
var
  E: PEntryPoint;
begin
  if QMemIsInstalled then
  begin
    try
      if IsMultiThread then
        EnterCriticalSection(lpCriticalSection);
      Result := LongWord(EFreeArr)-SpaceBegin;
      E := ListLeft;
      while E <> nil do
      begin
        Dec(Result,E^.Space);
        E := E^.AdrRight;
      end;
    finally
      if IsMultiThread then
        LeaveCriticalSection(lpCriticalSection);
    end;
  end else
    Result := -1;
end;

function QMemTotalFree: Integer;
var
  E: PEntryPoint;
begin
  if QMemIsInstalled then
  begin
    try
      if IsMultiThread then
        EnterCriticalSection(lpCriticalSection);
      Result := 0;
      E := ListLeft;
      while E <> nil do
      begin
        Inc(Result,E^.Space);
        E := E^.AdrRight;
      end;
    finally
      if IsMultiThread then
        LeaveCriticalSection(lpCriticalSection);
    end;
  end else
    Result := -1;
end;

function QMemMaxFreeBlock: Integer;
var
  E: PEntryPoint;
begin
  if QMemIsInstalled then
  begin
    try
      if IsMultiThread then
        EnterCriticalSection(lpCriticalSection);
      Result := 0;
      E := ListLeft;
      while E <> nil do
      begin
        if E^.Space > LongWord(Result) then
          Result := E^.Space;
        E := E^.AdrRight;
      end;
    finally
      if IsMultiThread then
        LeaveCriticalSection(lpCriticalSection);
    end;
  end else
    Result := -1;
end;

function QMemCountOfFreeBlocks: Integer;
begin
  if QMemIsInstalled then
  begin
    try
      if IsMultiThread then
        EnterCriticalSection(lpCriticalSection);
      Result := MaxECount-EFreeCount;
    finally
      if IsMultiThread then
        LeaveCriticalSection(lpCriticalSection);
    end;
  end else
    Result := -1;
end;

function QMemOverhead: Integer;
begin
  if QMemIsInstalled then
    Result := MaxECount*28
  else
    Result := -1;
end;

function QMemGetHeapStatus: TQMemHeapStatus;
var
  E: PEntryPoint;
  X1,X2: LongWord;
begin
  if QMemIsInstalled then
    try
      if IsMultiThread then
        EnterCriticalSection(lpCriticalSection);
      with Result do
      begin
        TotalAddrSpace := LongWord(EFreeArr)-StartAddr;
        TotalCommitted := IntCountOfSetBits(Pointer(StartAddr),
          TotalAddrSpace shr 19) shl 16;
        TotalUncommitted := TotalAddrSpace - TotalCommitted;
        X1 := 0;
        X2 := 0;
        E := ListLeft;
        while E <> nil do
        begin
          Inc(X1,E^.Space);
          if E^.Space > X2 then
            X2 := E^.Space;
          E := E^.AdrRight;
        end;
        TotalFree := X1;
        TotalAllocated := LongWord(EFreeArr)-SpaceBegin-X1;
        MaxFreeBlock := X2;
        CountOfFreeBlocks := MaxECount-EFreeCount;
        Overhead := MaxECount*28;;
      end;
    finally
      if IsMultiThread then
        LeaveCriticalSection(lpCriticalSection);
    end
  else
    with Result do
    begin
      TotalAddrSpace := 0;
      TotalCommitted := 0;
      TotalUncommitted := 0;
      TotalAllocated := 0;
      TotalFree := 0;
      MaxFreeBlock := 0;
      CountOfFreeBlocks := 0;
      Overhead := 0;
    end;
end;

function QMemSetMaxECount(Value: Integer): Integer;
begin
  if (not QMemIsInstalled) and (Value>0) then
  begin
    MaxECount := (Value+$3FFF) and $FFFFC000;
    Result := 0;
  end else
    Result := -1;
end;

procedure IntFillLong(Value: LongWord; P: Pointer; Count: Cardinal);
asm
        XCHG    EDI,EDX
        REP     STOSD
        MOV     EDI,EDX
end;

const
  QMemManager: TMemoryManager = (
    GetMem: IntGetMem;
    FreeMem: IntFreeMem;
    ReallocMem: IntReallocMem);

function QMemInstall(InitialSize, MaximumSize: Integer): Integer;
var
  L: LongWord;
  E: PEntryPoint;
  P: PLongWord;
begin
  if (not QMemIsInstalled) and (MaximumSize>0) then
  begin
    Inc(MaximumSize,$7FFFF);
    MaximumSize := MaximumSize and $FFF80000;
    L := MaxECount*28;
    StartAddr := LongWord(VirtualAlloc(nil, L+LongWord(MaximumSize),
      MEM_RESERVE or MEM_TOP_DOWN, PAGE_READWRITE));
    if StartAddr = 0 then
    begin
      Result := -1;
      Exit;
    end;
    EFreeArr := Pointer(StartAddr+LongWord(MaximumSize));
    if VirtualAlloc(EFreeArr, L, MEM_COMMIT, PAGE_READWRITE) = nil then
    begin
      VirtualFree(Pointer(StartAddr),0,MEM_RELEASE);
      Result := -1;
      Exit;
    end;
    EFreeCount := MaxECount;
    E := Pointer(LongWord(EFreeArr)+LongWord(MaxECount) shl 2);
    P := @EFreeArr^[EFreeCount];
    for L := 1 to EFreeCount do
    begin
      P^ := LongWord(E);
      Inc(E);
      Dec(P);
    end;
    if InitialSize < $10000 then
      InitialSize := $10000
    else if InitialSize > MaximumSize then
      InitialSize := MaximumSize
    else
    begin
      Inc(InitialSize,$FFFF);
      InitialSize := InitialSize and $FFFF0000;
    end;
    if VirtualAlloc(Pointer(StartAddr), InitialSize, MEM_COMMIT,
      PAGE_READWRITE) = nil then
    begin
      VirtualFree(Pointer(StartAddr),0,MEM_RELEASE);
      Result := -1;
      Exit;
    end;
    L := LongWord(MaximumSize) shr 19;
    IntFillLong(0,Pointer(StartAddr),(L+3) shr 2);
    IntSetBits(Pointer(StartAddr),0,InitialSize shr 16 - 1);
    SpaceBegin := (L+35) and $FFFFFFE0 - 4;
    Inc(SpaceBegin,StartAddr);
    Dec(EFreeCount);
    E := EFreeArr^[EFreeCount];
    ListLeft := E;
    ListRight := E;
    with E^ do
    begin
      Address := SpaceBegin;
      Space := LongWord(EFreeArr)-SpaceBegin;
      AdrLeft := nil;
      AdrRight := nil;
      SpLeft := nil;
      SpRight := nil;
    end;
    IntFillLong(0,@SizeTable,31);
    InitializeCriticalSection(lpCriticalSection);
    GetMemoryManager(OldMemManager);
    QMemIsInstalled := True;
    SetMemoryManager(QMemManager);
    Result := 0;
  end else
    Result := -1;
end;

function QMemRelease: Integer;
begin
  if QMemIsInstalled then
  begin
    SetMemoryManager(OldMemManager);
    QMemIsInstalled := False;
    DeleteCriticalSection(lpCriticalSection);
    if VirtualFree(Pointer(StartAddr), 0, MEM_RELEASE) then
    begin
      Result := 0;
      Exit;
    end;
  end;
  Result := -1;
end;

initialization
  QMemInstall(65536,268435456);

finalization
  QMemRelease;

end.

