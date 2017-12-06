unit DKC_IA32_MM_Unit;

//Version 0.16 3-9-2005

interface

implementation

{.$define ALIGN16}

uses
 Windows;

type

 TAllocType = record
  PBlock : Pointer;
  InternalpBlock : Pointer;
  InternalSize : Integer;
  ExternalSize : Integer;
  SmallAlloc : Boolean;
 end;

 TAllocTypeArray = array[0..100000000] of TAllocType;
 PAllocTypeArray = ^TAllocTypeArray;

var
 Heap: THandle;
 RTLCriticalSection : TRTLCriticalSection;
 LastUsedIndexGlobal : Integer;
 AllocArraySize : integer;
 NoOfLivePointers: integer;
 AllocArray : PAllocTypeArray;

const
 HEAP_NO_SERIALIZE = $00000001;
 SPLITSIZE : Integer = 1*1024*1024;
 FLAGS : Cardinal = 1;//HEAP_NO_SERIALIZE;
 SMALLMOVESIZE = 36;//For JOH Move
 SHRINKSIZE : Integer = 100;
 GROWSIZE : Integer = 100;
 MAXNOOFDEFRAGROUNDS : Integer = 0;
 OVERALLOCEXTRA : Integer = 16;
 OVERALLOCFACTORSMALLUPSIZE : Double = 2;
 OVERALLOCPERCENTAGESMALLDOWNSIZE : Double = 1/2;
 OVERALLOCPERCENTAGEBIGUPSIZE : Double = 1.2;
 OVERALLOCPERCENTAGEBIGDOWNSIZE : Double = 1/1.2;
 ALIGNSPACE : Integer = 16; //For 16 byte alignment
{-------------------------------------------------------------------------}
{Perform Forward Move of 0..36 Bytes}
{On Entry, ECX = Count, EAX = Source+Count, EDX = Dest+Count.  Destroys ECX}
procedure SmallForwardMove_9;
asm
  jmp     dword ptr [@@FwdJumpTable+ecx*4]
  nop {Align Jump Table}
@@FwdJumpTable:
  dd      @@Done {Removes need to test for zero size move}
  dd      @@Fwd01, @@Fwd02, @@Fwd03, @@Fwd04, @@Fwd05, @@Fwd06, @@Fwd07, @@Fwd08
  dd      @@Fwd09, @@Fwd10, @@Fwd11, @@Fwd12, @@Fwd13, @@Fwd14, @@Fwd15, @@Fwd16
  dd      @@Fwd17, @@Fwd18, @@Fwd19, @@Fwd20, @@Fwd21, @@Fwd22, @@Fwd23, @@Fwd24
  dd      @@Fwd25, @@Fwd26, @@Fwd27, @@Fwd28, @@Fwd29, @@Fwd30, @@Fwd31, @@Fwd32
  dd      @@Fwd33, @@Fwd34, @@Fwd35, @@Fwd36
@@Fwd36:
  mov     ecx, [eax-36]
  mov     [edx-36], ecx
@@Fwd32:
  mov     ecx, [eax-32]
  mov     [edx-32], ecx
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
  nop
@@Fwd35:
  mov     ecx, [eax-35]
  mov     [edx-35], ecx
@@Fwd31:
  mov     ecx, [eax-31]
  mov     [edx-31], ecx
@@Fwd27:
  mov     ecx, [eax-27]
  mov     [edx-27], ecx
@@Fwd23:
  mov     ecx, [eax-23]
  mov     [edx-23], ecx
@@Fwd19:
  mov     ecx, [eax-19]
  mov     [edx-19], ecx
@@Fwd15:
  mov     ecx, [eax-15]
  mov     [edx-15], ecx
@@Fwd11:
  mov     ecx, [eax-11]
  mov     [edx-11], ecx
@@Fwd07:
  mov     ecx, [eax-7]
  mov     [edx-7], ecx
  mov     ecx, [eax-4]
  mov     [edx-4], ecx
  ret
  nop
@@Fwd03:
  movzx   ecx, word ptr [eax-3]
  mov     [edx-3], cx
  movzx   ecx, byte ptr [eax-1]
  mov     [edx-1], cl
  ret
@@Fwd34:
  mov     ecx, [eax-34]
  mov     [edx-34], ecx
@@Fwd30:
  mov     ecx, [eax-30]
  mov     [edx-30], ecx
@@Fwd26:
  mov     ecx, [eax-26]
  mov     [edx-26], ecx
@@Fwd22:
  mov     ecx, [eax-22]
  mov     [edx-22], ecx
@@Fwd18:
  mov     ecx, [eax-18]
  mov     [edx-18], ecx
@@Fwd14:
  mov     ecx, [eax-14]
  mov     [edx-14], ecx
@@Fwd10:
  mov     ecx, [eax-10]
  mov     [edx-10], ecx
@@Fwd06:
  mov     ecx, [eax-6]
  mov     [edx-6], ecx
@@Fwd02:
  movzx   ecx, word ptr [eax-2]
  mov     [edx-2], cx
  ret
  nop
  nop
  nop
@@Fwd33:
  mov     ecx, [eax-33]
  mov     [edx-33], ecx
@@Fwd29:
  mov     ecx, [eax-29]
  mov     [edx-29], ecx
@@Fwd25:
  mov     ecx, [eax-25]
  mov     [edx-25], ecx
@@Fwd21:
  mov     ecx, [eax-21]
  mov     [edx-21], ecx
@@Fwd17:
  mov     ecx, [eax-17]
  mov     [edx-17], ecx
@@Fwd13:
  mov     ecx, [eax-13]
  mov     [edx-13], ecx
@@Fwd09:
  mov     ecx, [eax-9]
  mov     [edx-9], ecx
@@Fwd05:
  mov     ecx, [eax-5]
  mov     [edx-5], ecx
@@Fwd01:
  movzx   ecx, byte ptr [eax-1]
  mov     [edx-1], cl
  ret
@@Done:
end; {SmallForwardMove}

{-------------------------------------------------------------------------}
{Perform Backward Move of 0..36 Bytes}
{On Entry, ECX = Count, EAX = Source, EDX = Dest.  Destroys ECX}
procedure SmallBackwardMove_9;
asm
  jmp     dword ptr [@@BwdJumpTable+ecx*4]
  nop {Align Jump Table}
@@BwdJumpTable:
  dd      @@Done {Removes need to test for zero size move}
  dd      @@Bwd01, @@Bwd02, @@Bwd03, @@Bwd04, @@Bwd05, @@Bwd06, @@Bwd07, @@Bwd08
  dd      @@Bwd09, @@Bwd10, @@Bwd11, @@Bwd12, @@Bwd13, @@Bwd14, @@Bwd15, @@Bwd16
  dd      @@Bwd17, @@Bwd18, @@Bwd19, @@Bwd20, @@Bwd21, @@Bwd22, @@Bwd23, @@Bwd24
  dd      @@Bwd25, @@Bwd26, @@Bwd27, @@Bwd28, @@Bwd29, @@Bwd30, @@Bwd31, @@Bwd32
  dd      @@Bwd33, @@Bwd34, @@Bwd35, @@Bwd36
@@Bwd36:
  mov     ecx, [eax+32]
  mov     [edx+32], ecx
@@Bwd32:
  mov     ecx, [eax+28]
  mov     [edx+28], ecx
@@Bwd28:
  mov     ecx, [eax+24]
  mov     [edx+24], ecx
@@Bwd24:
  mov     ecx, [eax+20]
  mov     [edx+20], ecx
@@Bwd20:
  mov     ecx, [eax+16]
  mov     [edx+16], ecx
@@Bwd16:
  mov     ecx, [eax+12]
  mov     [edx+12], ecx
@@Bwd12:
  mov     ecx, [eax+8]
  mov     [edx+8], ecx
@@Bwd08:
  mov     ecx, [eax+4]
  mov     [edx+4], ecx
@@Bwd04:
  mov     ecx, [eax]
  mov     [edx], ecx
  ret
  nop
  nop
  nop
@@Bwd35:
  mov     ecx, [eax+31]
  mov     [edx+31], ecx
@@Bwd31:
  mov     ecx, [eax+27]
  mov     [edx+27], ecx
@@Bwd27:
  mov     ecx, [eax+23]
  mov     [edx+23], ecx
@@Bwd23:
  mov     ecx, [eax+19]
  mov     [edx+19], ecx
@@Bwd19:
  mov     ecx, [eax+15]
  mov     [edx+15], ecx
@@Bwd15:
  mov     ecx, [eax+11]
  mov     [edx+11], ecx
@@Bwd11:
  mov     ecx, [eax+7]
  mov     [edx+7], ecx
@@Bwd07:
  mov     ecx, [eax+3]
  mov     [edx+3], ecx
  mov     ecx, [eax]
  mov     [edx], ecx
  ret
  nop
  nop
  nop
@@Bwd03:
  movzx   ecx, word ptr [eax+1]
  mov     [edx+1], cx
  movzx   ecx, byte ptr [eax]
  mov     [edx], cl
  ret
  nop
  nop
@@Bwd34:
  mov     ecx, [eax+30]
  mov     [edx+30], ecx
@@Bwd30:
  mov     ecx, [eax+26]
  mov     [edx+26], ecx
@@Bwd26:
  mov     ecx, [eax+22]
  mov     [edx+22], ecx
@@Bwd22:
  mov     ecx, [eax+18]
  mov     [edx+18], ecx
@@Bwd18:
  mov     ecx, [eax+14]
  mov     [edx+14], ecx
@@Bwd14:
  mov     ecx, [eax+10]
  mov     [edx+10], ecx
@@Bwd10:
  mov     ecx, [eax+6]
  mov     [edx+6], ecx
@@Bwd06:
  mov     ecx, [eax+2]
  mov     [edx+2], ecx
@@Bwd02:
  movzx   ecx, word ptr [eax]
  mov     [edx], cx
  ret
  nop
@@Bwd33:
  mov     ecx, [eax+29]
  mov     [edx+29], ecx
@@Bwd29:
  mov     ecx, [eax+25]
  mov     [edx+25], ecx
@@Bwd25:
  mov     ecx, [eax+21]
  mov     [edx+21], ecx
@@Bwd21:
  mov     ecx, [eax+17]
  mov     [edx+17], ecx
@@Bwd17:
  mov     ecx, [eax+13]
  mov     [edx+13], ecx
@@Bwd13:
  mov     ecx, [eax+9]
  mov     [edx+9], ecx
@@Bwd09:
  mov     ecx, [eax+5]
  mov     [edx+5], ecx
@@Bwd05:
  mov     ecx, [eax+1]
  mov     [edx+1], ecx
@@Bwd01:
  movzx   ecx, byte ptr[eax]
  mov     [edx], cl
  ret
  nop
  nop
@@Done:
end; {SmallBackwardMove}

{-------------------------------------------------------------------------}
{Move ECX Bytes from EAX to EDX, where EAX > EDX and ECX > 36 (SMALLMOVESIZE)}
procedure Forwards_IA32_9;
asm
  push    edx
  fild    qword ptr [eax]
  lea     eax, [eax+ecx-8]
  lea     ecx, [ecx+edx-8]
  fild    qword ptr [eax]
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
  fistp   qword ptr [edx]
  pop     edx
  fistp   qword ptr [edx]
end; {Forwards_IA32}

{-------------------------------------------------------------------------}
{Move ECX Bytes from EAX to EDX, where EAX < EDX and ECX > 36 (SMALLMOVESIZE)}
procedure Backwards_IA32_9;
asm
  sub     ecx, 8
  push    ecx
  fild    qword ptr [eax+ecx] {Last 8}
  fild    qword ptr [eax] {First 8}
  add     ecx, edx
  and     ecx, -8
  sub     ecx, edx
@BwdLoop:
  fild    qword ptr [eax+ecx]
  fistp   qword ptr [edx+ecx]
  sub     ecx, 8
  jg      @BwdLoop
  pop     ecx
  fistp   qword ptr [edx] {First 8}
  fistp   qword ptr [edx+ecx] {Last 8}
end; {Backwards_IA32}

 {-------------------------------------------------------------------------}
{Move using IA32 Instruction Set Only}
procedure MoveJOH_IA32_9(const Source; var Dest; Count : Integer);
asm
  cmp     ecx, SMALLMOVESIZE
  ja      @Large {Count > SMALLMOVESIZE or Count < 0}
  cmp     eax, edx
  jbe     @SmallCheck
  add     eax, ecx
  add     edx, ecx
  jmp     SmallForwardMove_9
@SmallCheck:
  jne     SmallBackwardMove_9
  ret {For Compatibility with Delphi's move for Source = Dest}
@Large:
  jng     @Done {For Compatibility with Delphi's move for Count < 0}
  cmp     eax, edx
  ja      Forwards_IA32_9
  je      @Done {For Compatibility with Delphi's move for Source = Dest}
  sub     edx, ecx
  cmp     eax, edx
  lea     edx, [edx+ecx]
  jna     Forwards_IA32_9
  jmp     Backwards_IA32_9 {Source/Dest Overlap}
@Done:
end; {MoveJOH_IA32}

procedure InitializeAllocArray(StartIndex, StopIndex : Integer);
var
 Index : Integer;

begin
 Assert(StartIndex >= 0);
 Assert(StartIndex <= AllocArraySize-1);
 Assert(StopIndex >= 0);
 Assert(StopIndex <= AllocArraySize-1);
 for Index := StartIndex to StopIndex do
  begin
   AllocArray[Index].PBlock := nil;
   AllocArray[Index].InternalpBlock := nil;
   AllocArray[Index].InternalSize := 0;
   AllocArray[Index].ExternalSize := 0;
   AllocArray[Index].SmallAlloc := True
  end;
end;

//Returns True if successfull

function GrowAllocArray(var AllocArray : PAllocTypeArray; var AllocArraySize : Integer) : Boolean;
var
 OldAllocArray, NewAllocArray : PAllocTypeArray;
 OldSize, NewSize : Integer;

begin
 OldAllocArray := AllocArray;
 OldSize := AllocArraySize;
 NewSize := OldSize + GROWSIZE;
 NewAllocArray := VirtualAlloc(nil, NewSize * SizeOf(TAllocType),MEM_COMMIT+MEM_TOP_DOWN, PAGE_READWRITE);
 if NewAllocArray = nil then
  begin
   Result := False;
  end
 else
  begin
   Move(OldAllocArray[0], NewAllocArray[0], OldSize * SizeOf(TAllocType));
   AllocArray := NewAllocArray;
   AllocArraySize := NewSize;
   InitializeAllocArray(OldSize, NewSize-1);
   if VirtualFree(OldAllocArray, 0, MEM_RELEASE) then
    Result := True
   else
    Result := False;
  end;
end;

function ShrinkAllocArray(var AllocArray : PAllocTypeArray; var AllocArraySize : Integer) : Boolean;
var
 HighIndex, NewSize, Index, LowIndex, NoOfDefragRounds, OldSize : Integer;
 OldAllocArray, NewAllocArray : PAllocTypeArray;

begin
 Result := True;
 OldSize := AllocArraySize;
 NoOfDefragRounds := 0;
 HighIndex := AllocArraySize;
 LowIndex := -1;
 if MAXNOOFDEFRAGROUNDS > 0 then
  begin
   repeat
    //Find highest used index
    repeat
     Dec(HighIndex);
    until(AllocArray[HighIndex].PBlock <> nil);
    //Find lowest unused index
    repeat
     Inc(LowIndex);
    until(AllocArray[LowIndex].PBlock = nil);
    if LowIndex < HighIndex then
     begin
      //Copy HighIndex to LowIndex
      AllocArray[LowIndex].PBlock := AllocArray[HighIndex].PBlock;
      AllocArray[LowIndex].InternalSize := AllocArray[HighIndex].InternalSize;
      AllocArray[LowIndex].ExternalSize := AllocArray[HighIndex].ExternalSize;
      AllocArray[LowIndex].SmallAlloc := AllocArray[HighIndex].SmallAlloc;
      //Clear HighIndex
      AllocArray[HighIndex].PBlock := nil;
      AllocArray[HighIndex].InternalSize := 0;
      AllocArray[HighIndex].ExternalSize := 0;
      AllocArray[HighIndex].SmallAlloc := True;
      Index := HighIndex;
     end
    else
     begin
      //No more to defrag
      Index := LowIndex;
      Break;
     end;
    Inc(NoOfDefragRounds)
   until(NoOfDefragRounds >= MAXNOOFDEFRAGROUNDS);
  end
 else
  Index := AllocArraySize;//Safe and slow!!!!
 //Find highest used index
 repeat
  Dec(Index);
  if Index < 0 then
   Break;//No used indexes
 until(AllocArray[Index].PBlock <> nil);
 NewSize := OldSize - SHRINKSIZE;
 //Do not shrink below any used entries
 if NewSize < Index+1 then
  NewSize := Index+1;
 if NewSize < OldSize then
  begin
   //Shrink AllocArray
   OldAllocArray := AllocArray;
   NewAllocArray := VirtualAlloc(nil, NewSize * SizeOf(TAllocType), MEM_COMMIT+MEM_TOP_DOWN, PAGE_READWRITE);
   AllocArray := NewAllocArray;
   if AllocArray = nil then
    Result := False
   else
    begin
     Move(OldAllocArray[0], AllocArray[0], NewSize * SizeOf(TAllocType));
     if not VirtualFree(OldAllocArray, 0, MEM_RELEASE) then
      Result := False
     else
      begin
       AllocArraySize := NewSize;
       LastUsedIndexGlobal := NewSize-1;
      end;
    end;
  end;
end;

function GetPInternal(Index : Integer) : Pointer;
begin
 Assert(Index >= 0);
 Assert(Index <= AllocArraySize-1);
 Result := AllocArray[Index].InternalpBlock;
end;

function GetSize(Index : Integer) : Integer; overload;
begin
 Assert(Index >= 0);
 Assert(Index <= AllocArraySize-1);
 Result := AllocArray[Index].InternalSize;
end;

function GetExternalSize(Index : Integer) : Integer;
begin
 Assert(Index >= 0);
 Assert(Index <= AllocArraySize-1);
 Result := AllocArray[Index].ExternalSize;
end;

procedure SetSize(Index, NewSize, ExternalSize : Integer);
begin
 Assert(Index >= 0);
 Assert(Index <= AllocArraySize-1);
 AllocArray[Index].InternalSize := NewSize;
 AllocArray[Index].ExternalSize := ExternalSize;
end;

function GetSmallAlloc(Index : Integer) : Boolean; overload;
begin
 Assert(Index >= 0);
 Assert(Index <= AllocArraySize-1);
 Result := AllocArray[Index].SmallAlloc;
end;

//Returns true if succesfull

function AddToAllocTypeArray(P : Pointer;
                             PInternal : Pointer;
                             SmallAlloc : Boolean;
                             Size, ExternalSize : Integer;
                             var AllocArraySize : Integer) : Boolean;
var
 Index1, Index2 : Integer;

begin
 Assert(Cardinal(P) >= Cardinal(PInternal));
 Result := True;
 Inc(NoOfLivePointers);
 if NoOfLivePointers > AllocArraySize then
  begin
   Result := GrowAllocArray(AllocArray, AllocArraySize);
   Assert(Result);
   if not Result then
    Exit;
  end;
 Index1 := LastUsedIndexGlobal;
 Index2 := LastUsedIndexGlobal+1;
 if Index1 < 0 then
  Index1 := 0;
 if Index2 > AllocArraySize-1 then
  Index2 := AllocArraySize-1;
 repeat
  if (AllocArray[Index1].PBlock = nil) then
   begin
    LastUsedIndexGlobal := Index1;
    AllocArray[Index1].PBlock := P;
    AllocArray[Index1].InternalpBlock := PInternal;
    AllocArray[Index1].InternalSize := Size;
    AllocArray[Index1].ExternalSize := ExternalSize;
    AllocArray[Index1].SmallAlloc := SmallAlloc;
    Exit;
   end;
  if (AllocArray[Index2].PBlock = nil) then
   begin
    LastUsedIndexGlobal := Index2;
    AllocArray[Index2].PBlock := P;
    AllocArray[Index2].InternalpBlock := PInternal;
    AllocArray[Index2].InternalSize := Size;
    AllocArray[Index2].ExternalSize := ExternalSize;
    AllocArray[Index2].SmallAlloc := SmallAlloc;
    Exit;
   end;
  Dec(Index1);
  Inc(Index2);
  if (Index1 < 0) and (Index2 > AllocArraySize-1) then
   begin
    //Did not find space for pointer
    Result := False;
    Exit;
   end;
  if Index1 < 0 then
   Index1 := 0;
  if Index2 > AllocArraySize-1 then
   Index2 := AllocArraySize-1;
 until(False);
end;

function RemoveFromAllocTypeArray(Index : Integer; var AllocArraySize : Integer) : Boolean; overload;
begin
 Assert(Index >= 0);
 Assert(Index <= AllocArraySize-1);
 Result := True;
 AllocArray[Index].PBlock := nil;
 AllocArray[Index].InternalpBlock := nil;
 AllocArray[Index].InternalSize := 0;
 AllocArray[Index].ExternalSize := 0;
 AllocArray[Index].SmallAlloc := True;
 Dec(NoOfLivePointers);
 if NoOfLivePointers < AllocArraySize-SHRINKSIZE then
  Result := ShrinkAllocArray(AllocArray, AllocArraySize);
end;

function GetIndex(P : Pointer) : Integer;
var
 Index1, Index2 : Integer;

begin
 if LastUsedIndexGlobal > AllocArraySize-2 then
  LastUsedIndexGlobal := AllocArraySize-2;
 Index1 := LastUsedIndexGlobal;
 Index2 := LastUsedIndexGlobal+1;
 repeat
  if (AllocArray[Index1].PBlock = P) then
   begin
    LastUsedIndexGlobal := Index1;
    Result := Index1;
    Exit;
   end;
  if (AllocArray[Index2].PBlock = P) then
   begin
    LastUsedIndexGlobal := Index2;
    Result := Index2;
    Exit;
   end;
  Dec(Index1);
  Inc(Index2);
  if (Index1 < 0) and (Index2 > AllocArraySize-1) then
   begin
    //Did not find pointer
    //RunError(203);//Ignore double free attempts
    Result := -1;
    Exit;
   end;
  if Index1 < 0 then
   Index1 := 0;
  if Index2 > AllocArraySize-1 then
   Index2 := AllocArraySize-1;
 until(False);
end;

function DKCGetMem(Size: Integer): Pointer;
var
 OverSize, MisAlign : Integer;
 TempResult : Pointer;

begin
 if IsMultiThread then
  EnterCriticalSection(RTLCriticalSection);
 if Size < SPLITSIZE then
  begin
   OverSize := Size + OVERALLOCEXTRA + ALIGNSPACE;
   TempResult := HeapAlloc(Heap, FLAGS, OverSize);
   {$ifdef ALIGN16}
   MisAlign := Integer(TempResult) and 15;
   {$else}
   MisAlign := 0;
   {$endif}
   Assert(MisAlign >= 0);
   Result := Pointer(Integer(TempResult) + MisAlign);
   if not AddToAllocTypeArray(Result, TempResult, True, OverSize, Size, AllocArraySize) then
    Result := nil;
  end
 else
  begin
   Result := VirtualAlloc(nil, Size, MEM_COMMIT+MEM_TOP_DOWN, PAGE_READWRITE);
   if not AddToAllocTypeArray(Result, Result, False, Size, Size, AllocArraySize) then
    begin
     Result := nil;
    end;
  end;
 if IsMultiThread then
  LeaveCriticalSection(RTLCriticalSection);
end;

//Returns 0 on success
//Returns 1 on error

function DKCFreeMem(Ptr: Pointer): Integer;
var
 Res, Success : Boolean;
 Index : Integer;
 InternalPtr : Pointer;

begin
 if IsMultiThread then
  EnterCriticalSection(RTLCriticalSection);
 Result := 0;//Assume success
 Index := GetIndex(Ptr);
 InternalPtr := GetPInternal(Index);
 if GetSmallAlloc(Index) then
  begin
   if HeapFree(Heap, FLAGS, InternalPtr) then
    begin
     Success := RemoveFromAllocTypeArray(Index, AllocArraySize);
     if not Success then
      Result := 1
     else
      begin
       if HeapCompact(Heap, FLAGS) = 0 then
        Result := 1;
      end;
    end
   else
    Result := 1;
  end
 else
  begin
   Res := VirtualFree(InternalPtr, 0, MEM_RELEASE);
   if Res then
    begin
     if not RemoveFromAllocTypeArray(Index, AllocArraySize) then
      Result := 1;
    end
   else
    Result := 1;
  end;
 if IsMultiThread then
  LeaveCriticalSection(RTLCriticalSection);
end;

function DKCReallocMem(Ptr: Pointer; Size: Integer): Pointer;
var
 OldIndex, OldSize, NewSize, OldExternalSize, NewOverSize : Integer;
 OldInternalPtr, NewPtr, OldPtr : Pointer;

begin
 if IsMultiThread then
  EnterCriticalSection(RTLCriticalSection);
 NewSize := Size;
 OldPtr := Ptr;
 OldIndex := GetIndex(Ptr);
 OldSize := GetSize(OldIndex);
 if GetSmallAlloc(OldIndex) then
  begin
   if NewSize < SPLITSIZE then
    begin
     if OldIndex = 214 then
      OldIndex := 214;
     //Realloc small as small
     if (NewSize > OldSize - ALIGNSPACE) then //Upsize
      begin
       //Alloc more than requested
       NewOverSize := Round(NewSize * OVERALLOCFACTORSMALLUPSIZE) + OVERALLOCEXTRA + ALIGNSPACE;
       OldInternalPtr := GetPInternal(OldIndex);
       Result := HeapRealloc(Heap, FLAGS, OldInternalPtr, NewOverSize);
      end
     else if (NewSize < Round(OldSize * OVERALLOCPERCENTAGESMALLDOWNSIZE) - OVERALLOCEXTRA - ALIGNSPACE) then //Downsize
      begin
       //Allocate requested size
       NewOverSize := NewSize + ALIGNSPACE;
       OldInternalPtr := GetPInternal(OldIndex);
       Result := HeapRealloc(Heap, FLAGS, OldInternalPtr, NewOverSize);
      end
     else
      begin
       //OverSize did not change because no realloc took place
       NewOverSize := OldSize;
       Result := OldPtr;
      end;
     if Result = OldPtr then
      begin
       SetSize(OldIndex, NewOverSize, NewSize);
      end
     else
      begin
       RemoveFromAllocTypeArray(OldIndex, AllocArraySize);
       if not AddToAllocTypeArray(Result, Result, True, NewOverSize, NewSize, AllocArraySize) then
        Result := nil;
      end;
    end
   else
    begin
     //Realloc small as big
     //Get new block
     Result := VirtualAlloc(nil, NewSize, MEM_COMMIT+MEM_TOP_DOWN, PAGE_READWRITE);
     NewPtr := Result;
     if Result <> nil then
      begin
       AddToAllocTypeArray(NewPtr, NewPtr, False, NewSize, NewSize, AllocArraySize);
       OldExternalSize := GetExternalSize(OldIndex);
       MoveJOH_IA32_9(OldPtr^, NewPtr^, OldExternalSize);
       if not HeapFree(Heap, FLAGS, OldPtr) then
        Result := nil
       else
        RemoveFromAllocTypeArray(OldIndex, AllocArraySize);
      end
     else
      Result := nil;
    end;
  end
 else
  begin
   if Size > SPLITSIZE then
    begin
     //Realloc big as big
     //Is realloc needed
     if NewSize > OldSize then   //Upsize
      begin
       //Alloc more than requested
       NewOverSize := Round(NewSize * OVERALLOCPERCENTAGEBIGUPSIZE);
       Result := VirtualAlloc(OldPtr, NewOverSize, MEM_COMMIT or MEM_TOP_DOWN, PAGE_READWRITE);
      end
     else if (NewSize < Round(OldSize * OVERALLOCPERCENTAGEBIGDOWNSIZE)) then //Downsize
      begin
       //Alloc more than requested
       NewOverSize := Round(NewSize * OVERALLOCPERCENTAGEBIGUPSIZE);
       Result := VirtualAlloc(OldPtr, NewOverSize, MEM_COMMIT or MEM_TOP_DOWN, PAGE_READWRITE);
      end
     else
      begin
       //OverSize did not change because no realloc took place
       NewOverSize := OldSize;
       Result := OldPtr;
      end;
     if Result <> nil then
      begin
       if Result = OldPtr then
        begin
         SetSize(OldIndex, NewOverSize, NewSize);
        end
       else
        begin
         NewPtr := Result;
         if NewSize > OldSize then
          begin
           OldExternalSize := GetExternalSize(OldIndex);
           Move(OldPtr^, NewPtr^, OldExternalSize);
          end;
         RemoveFromAllocTypeArray(OldIndex, AllocArraySize);
         AddToAllocTypeArray(Result, Result, False, NewOverSize, NewSize, AllocArraySize);
        end;
      end
     else
      begin
       //Realloc failed. Try get a new block
       Result := VirtualAlloc(nil, NewOverSize, MEM_COMMIT+MEM_TOP_DOWN, PAGE_READWRITE);
       NewPtr := Result;
       if Result <> nil then
        begin
         AddToAllocTypeArray(NewPtr, NewPtr, False, NewOverSize, NewSize, AllocArraySize);
         OldExternalSize := GetExternalSize(OldIndex);
         MoveJOH_IA32_9(OldPtr^, NewPtr^, OldExternalSize);
         if not VirtualFree(OldPtr, 0, MEM_RELEASE) then
          Result := nil
         else
          RemoveFromAllocTypeArray(OldIndex, AllocArraySize);
        end
       else
        Result := nil;
      end;
    end
   else
    begin
     //Realloc big as small
     Result := HeapAlloc(Heap, FLAGS, NewSize);
     NewPtr := Result;
     AddToAllocTypeArray(NewPtr, NewPtr, True, NewSize, NewSize, AllocArraySize);
     Move(OldPtr^, NewPtr^, NewSize);
     if not VirtualFree(OldPtr, 0, MEM_RELEASE) then
      Result := nil
     else
      RemoveFromAllocTypeArray(OldIndex, AllocArraySize);
    end;
  end;
 if IsMultiThread then
  LeaveCriticalSection(RTLCriticalSection);
end;

procedure InitMemoryManager;
//resourcestring
 //sError = 'DKC_IA32_MM_Unit must be the first unit used by the project';
var
 MemMgr: TMemoryManager;

begin
 NoOfLivePointers := 0;
 Heap := HeapCreate(FLAGS, 0, 0);
 if Heap = 0 then
  RunError(203); // out of memory
 //Assert(AllocMemCount <> 0);
 MemMgr.GetMem := DKCGetMem;
 MemMgr.FreeMem := DKCFreeMem;
 MemMgr.ReallocMem := DKCReallocMem;
 SetMemoryManager(MemMgr);
 InitializeCriticalSection(RTLCriticalSection);
 EnterCriticalSection(RTLCriticalSection);
 AllocArraySize := GROWSIZE;
 AllocArray := VirtualAlloc(nil, AllocArraySize * SizeOf(TAllocType),MEM_COMMIT+MEM_TOP_DOWN, PAGE_READWRITE);
 InitializeAllocArray(0, AllocArraySize-1);
 LeaveCriticalSection(RTLCriticalSection);
end;

initialization

 InitMemoryManager;

finalization

 DeleteCriticalSection(RTLCriticalSection);
 if Heap <> 0 then
  HeapDestroy(Heap);
 VirtualFree(AllocArray, 0, MEM_RELEASE);
 AllocArray := nil;

end.
