unit EWCMM;

//***************************************************************
//General Info:
//
//  Memory should be allocated and aligned on 16 bit boundaries.
//  Allocations should end at a 4 byte aligned address.
//  There is no CPU based selection of code. 
//***************************************************************

//***************************************************************
//Credits:
//---------------------------------------------------------------
//The Red-Black tree was translated and mashed together with some elements
// found in Julian Bucknall's RB Tree from his A&DS Book.
// However, the primary work was a translation of an implementation whose
// copyright is detailed below.
//
// Note: as the source of the rb tree is derived from a GNU licensed library
//       this MM is not suitable for many uses.  I haven't had the opportunity
//       to locate other RB implementations that could be used in its stead.
//
//   libavl - library for manipulation of binary trees.
//   Copyright (C) 1998-2002 Free Software Foundation, Inc.
//
//   This program is free software; you can redistribute it and/or
//   modify it under the terms of the GNU General Public License as
//   published by the Free Software Foundation; either version 2 of the
//   License, or (at your option) any later version.
//
//   This program is distributed in the hope that it will be useful, but
//   WITHOUT ANY WARRANTY; without even the implied warranty of
//   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//   See the GNU General Public License for more details.
//
//   You should have received a copy of the GNU General Public License
//   along with this program; if not, write to the Free Software
//   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
//   02111-1307, USA.
//
//   The author may be contacted at <blp@gnu.org> on the Internet, or
//   write to Ben Pfaff, Stanford University, Computer Science Dept., 353
//   Serra Mall, Stanford CA 94305, USA.
//
//---------------------------------------------------------------
//
//The spin lock code is a combination of stuff I learned from the discussions
// on the basm news group when this was discussed and further assisted from
// the BucketMM/FastMM units which served as an example of how this could be done.
//
//---------------------------------------------------------------
//
//The IA32 FastMove function from John O'Harrow is copied here (v2.1).
//
//  Copyright (c) 2005, John O'Harrow (john@almcrest.demon.co.uk)
//
//  This software is provided 'as-is', without any express or implied warranty.
//  In no event will the authors be held liable for any damages arising from the
//  use of this software.
//
//  Permission is granted to anyone to use this software for any purpose, including
//  commercial applications, and to alter it and redistribute it freely, subject to
//  the following restrictions:
//
//  1. The origin of this software must not be misrepresented; you must not claim
//     that you wrote the original software. If you use this software in a product,
//     an acknowledgment in the product documentation would be appreciated but is
//     not required.
//
//  2. Altered source versions must be plainly marked as such, and must not be
//     misrepresented as being the original software.
//
//  3. This notice may not be removed or altered from any source distribution.
//
//---------------------------------------------------------------
//
//The IA32 FastCode FillChar function from John O'Harrow is copied here.
//
//***************************************************************

//***************************************************************
//Disclaimer:
//
//  Use this at your own risk!!!!
//
//  There may be remnants of failed experiments remaining in the
//  code.  With any luck, they have been cleaned up.
//
//  There is probably a lot of room for improvement in record structures
//  and variable alignment.  All suggestions are welcome.
//
//  The default memory manager is allocating some things prior to
//  my attempt to swap memory managers.  This probably has to do with
//  my variable/constant declarations, but until I figure it out, the
//  default memory manager will still be called when deallocating memory
//  that I can't locate.
//***************************************************************

{$ASSERTIONS OFF}

{$OPTIMIZATION ON}
{$RANGECHECKS OFF}
{$OVERFLOWCHECKS OFF}
{$IOCHECKS OFF}
{$MINENUMSIZE 1}

{.$DEFINE USECRITSECT}
{$DEFINE USEFASTMOVE}

interface

implementation

uses SysUtils, Windows;

type
  //Red-Black Tree Type Definitions
  TtdChildType = (      {types of children}
         ctLeft,        {..left child}
         ctRight);      {..right child}

  TtdRBColor = (        {colors for the red-black tree}
         rbBlack,       {..black}
         rbRed);        {..red}

  //Pointer to physical memory allocated.  Min size is 1 MB.
//  pMemorySegment = ^TMemorySegment;
  TMemorySegment = record
    Physical:    Pointer;
    Next:        Integer;
    RefCount:    Integer;
    SegmentType: Integer;
  end;

  pLargeMemorySegment = ^TLargeMemorySegment;
  TLargeMemorySegment = record
    Size:        Cardinal;
    Physical:    Pointer;
    Reserved1:   Cardinal;
    Reserved2:   Cardinal;
  end;

  //need different name.  Too easy to confuse with tmemoryspacegroup
  TMemorySpaceGroups = (msz_16, msz_32, msz_64, msz_128, msz_256, msz_512,
                 msz_1024, msz_2048, msz_4096, msz_8192, msz_16384, msz_32768,
                 msz_65536, msz_Infinite);

  //Pointer to allocated Memory requested by the user.
  pMemoryItem = ^TMemoryItem;
  TMemoryItem = record
    Memory:      Pointer;
    Parent:      pMemoryItem;
    btChild:     array [TtdChildType] of pMemoryItem;
    MemorySpace: Integer;
    MemoryGroup: TMemorySpaceGroups;
    btColor:     TtdRBColor;
    Reserved1:   Byte;
    Reserved2:   Byte;
    Reserved3:   Cardinal;
    MemorySeg:   Integer;
  end;

  //Pointer to portions of Memory Segment reserved for a collection of Memory Items.
  pMemorySpace = ^TMemorySpace;
  TMemorySpace = record
    pSpace:      Pointer;
    Prev:        Integer;
    Next:        Integer;
    RefCount:    Integer;
    MemoryGroup: TMemorySpaceGroups;
  end;

  //Pointer to portions of Memory Segment reserved for a collection of Memory Items.
  TMemorySpaceGroup = record
    Size:        Cardinal;
    FreeMemItem: pMemoryItem;
    UsedSpace:   Integer;
    Reserved:    Integer;
  end;

const
  MemorySpaceGroupSize: array[TMemorySpaceGroups] of Integer =
    (16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768, 65536, 0);

  MemoryItemsPerSpace: array[TMemorySpaceGroups] of Integer =
    (4096, 2048, 1024, 512, 256, 128, 64, 32, 16, 8, 4, 2, 1, 0);

const
  MAX_MEMSEG = 2048;   //Max Number of 1 MB Memory Segments that can fit in 2G.
                       // Minimum size of memory segments is 1 MB.

  MAX_SPACES = 32768;  //Max Number of 64k Memory Spaces that will fit in 2G.

  MEM_SEGMENT_SIZE = 1048576;

  MEM_SPACES_PER_SEGMENT = 16;
  MEM_SPACE_SIZE = 65536;

  MEM_ITEM_POINTERS = MEM_SEGMENT_SIZE div SizeOf(TMemoryItem);
var
  EWCMemoryManager: TMemoryManager;
  OldMemoryManager: TMemoryManager;

  IdxMemSpaceGroups: array[1..65536] of TMemorySpaceGroups;

  MemorySegments: array[0..MAX_MEMSEG - 1] of TMemorySegment;
  MemorySpaces: array[0..MAX_SPACES - 1] of TMemorySpace;

  LargeMemorySpaces: array[0..MAX_SPACES - 1] of Boolean;  //In use if true

  MemorySpaceSizeLookup: array[0..MAX_SPACES - 1] of TMemorySpaceGroups;

  UsedMemorySegments: Integer;
  OpenMemorySegments: Integer;       //These do not yet point to allocated memory.

  MemorySpaceGroups: array[TMemorySpaceGroups] of TMemorySpaceGroup;

  FreeMemorySpace:    Integer;
  OpenMemorySpace:    Integer;  //Counter to 32K.  When zero, no more memory to allocate.

  UsedMemoryItems:    TMemoryItem;  //Special Case Node.  Need to deal with that.
  OpenMemoryItems:    TMemoryItem;  //Special Case Node.

  //Critical Section
{$IFDEF USECRITSECT}
  LockAccess:         TRTLCriticalSection;
{$ELSE}
  LockAccess:         Byte;
{$ENDIF}

//***** IA32 Only Fast FillChar - John O'Harrow *****

procedure FillCharJOH_IA32_c(var Dest; count: Integer; Value: Char);
asm {Size = 161 Bytes}
  cmp   edx, 32
  mov   ch, cl                    {Copy Value into both Bytes of CX}
  jl    @@Small
  push  ebx
  sub   edx, 16
  mov   bx, cx                    {Copy Value into each Byte of ECX}
  shl   ecx, 16
  mov   cx, bx
  mov   [eax], ecx                {Fill First 4 Bytes}
  mov   [eax+edx], ecx            {Fill Last 16 Bytes}
  mov   [eax+edx+4], ecx
  mov   [eax+edx+8], ecx
  mov   [eax+edx+12], ecx
  mov   ebx, eax                  {4-Byte Align Writes}
  and   ebx, 3
  sub   ebx, 4
  sub   eax, ebx
  add   edx, ebx
  add   eax, edx
  neg   edx
@@Loop:
  mov   [eax+edx], ecx            {Fill 16 Bytes per Loop}
  mov   [eax+edx+4], ecx
  mov   [eax+edx+8], ecx
  mov   [eax+edx+12], ecx
  add   edx, 16
  jl    @@Loop
  pop   ebx
  ret
  nop
@@Small:
  test  edx, edx
  jle   @@Done
  mov   [eax+edx-1], cl       {Fill Last Byte}
  and   edx, -2               {No. of Words to Fill}
  neg   edx
  lea   edx, [@@SmallFill + 60 + edx * 2]
  jmp   edx
  nop                             {Align Jump Destinations}
  nop
@@SmallFill:
  mov   [eax+28], cx
  mov   [eax+26], cx
  mov   [eax+24], cx
  mov   [eax+22], cx
  mov   [eax+20], cx
  mov   [eax+18], cx
  mov   [eax+16], cx
  mov   [eax+14], cx
  mov   [eax+12], cx
  mov   [eax+10], cx
  mov   [eax+ 8], cx
  mov   [eax+ 6], cx
  mov   [eax+ 4], cx
  mov   [eax+ 2], cx
  mov   [eax   ], cx
  ret {DO NOT REMOVE - This is for Alignment}
@@Done:
end;

//***** IA32 Only Fast Move - John O'Harrow *****

const
  SMALLMOVESIZE = 36;

{-------------------------------------------------------------------------}
{Perform Forward Move of 0..36 Bytes}
{On Entry, ECX = Count, EAX = Source+Count, EDX = Dest+Count.  Destroys ECX}
procedure SmallForwardMove_6;
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
  movzx   ecx,  word ptr [eax-3]
  mov     [edx-3], cx
  movzx   ecx,  byte ptr [eax-1]
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
  movzx   ecx,  word ptr [eax-2]
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
  movzx   ecx,  byte ptr [eax-1]
  mov     [edx-1], cl
  ret
@@Done:
end; {SmallForwardMove}

{-------------------------------------------------------------------------}
{Perform Backward Move of 0..36 Bytes}
{On Entry, ECX = Count, EAX = Source, EDX = Dest.  Destroys ECX}
procedure SmallBackwardMove_6;
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
  movzx   ecx,  word ptr [eax+1]
  mov     [edx+1], cx
  movzx   ecx,  byte ptr [eax]
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
  movzx   ecx,  word ptr [eax]
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
  movzx   ecx,  byte ptr[eax]
  mov     [edx], cl
  ret
  nop
  nop
@@Done:
end; {SmallBackwardMove}

{-------------------------------------------------------------------------}
{Move ECX Bytes from EAX to EDX, where EAX > EDX and ECX > 36 (SMALLMOVESIZE)}
procedure Forwards_IA32_6;
asm
  push    ebx
  mov     ebx, edx
  fild    qword ptr [eax]
  add     eax, ecx {QWORD Align Writes}
  add     ecx, edx
  add     edx, 7
  and     edx, -8
  sub     ecx, edx
  add     edx, ecx {Now QWORD Aligned}
  sub     ecx, 16
  neg     ecx
@FwdLoop:
  fild    qword ptr [eax+ecx-16]
  fistp   qword ptr [edx+ecx-16]
  fild    qword ptr [eax+ecx-8]
  fistp   qword ptr [edx+ecx-8]
  add     ecx, 16
  jle     @FwdLoop
  fistp   qword ptr [ebx]
  neg     ecx
  add     ecx, 16
  pop     ebx
  jmp     SmallForwardMove_6
end; {Forwards_IA32}

{-------------------------------------------------------------------------}
{Move ECX Bytes from EAX to EDX, where EAX < EDX and ECX > 36 (SMALLMOVESIZE)}
procedure Backwards_IA32_6;
asm
  push    ebx
  fild    qword ptr [eax+ecx-8]
  lea     ebx, [edx+ecx] {QWORD Align Writes}
  and     ebx, 7
  sub     ecx, ebx
  add     ebx, ecx {Now QWORD Aligned, EBX = Original Length}
  sub     ecx, 16
@BwdLoop:
  fild    qword ptr [eax+ecx]
  fild    qword ptr [eax+ecx+8]
  fistp   qword ptr [edx+ecx+8]
  fistp   qword ptr [edx+ecx]
  sub     ecx, 16
  jge     @BwdLoop
  fistp   qword ptr [edx+ebx-8]
  add     ecx, 16
  pop     ebx
  jmp     SmallBackwardMove_6
end; {Backwards_IA32}

{-------------------------------------------------------------------------}
{Move using IA32 Instruction Set Only}
procedure MoveJOH_IA32_6(const Source; var Dest; Count : Integer);
asm
  cmp     ecx, SMALLMOVESIZE
  ja      @Large {Count > SMALLMOVESIZE or Count < 0}
  cmp     eax, edx
  jbe     @SmallCheck
  add     eax, ecx
  add     edx, ecx
  jmp     SmallForwardMove_6
@SmallCheck:
  jne     SmallBackwardMove_6
  ret {For Compatibility with Delphi's move for Source = Dest}
@Large:
  jng     @Done {For Compatibility with Delphi's move for Count < 0}
  cmp     eax, edx
  ja      Forwards_IA32_6
  je      @Done {For Compatibility with Delphi's move for Source = Dest}
  sub     edx, ecx
  cmp     eax, edx
  lea     edx, [edx+ecx]
  jna     Forwards_IA32_6
  jmp     Backwards_IA32_6 {Source/Dest Overlap}
@Done:
end; {MoveJOH_IA32}

{$IFNDEF USECRITSECT}
procedure Do_LockAccess;
//
//  function AttemptLock: Boolean;
//  asm
//    xor eax, eax
//    mov dl, 1
//    lock cmpxchg LockAccess, dl
//    jnz @ReturnFalse
//    mov eax, 1
//    jmp @Done
//  @ReturnFalse:
//    xor eax, eax
//  @Done:
//  end;
//
//begin
//  if IsMultiThread then
//    if not AttemptLock then
//      begin
//        Windows.SleepEx(0, False);
//        while not AttemptLock do
//          begin
//            Windows.SleepEx(1, False);
//          end;
//      end;
//end;
asm
//  cmp IsMultiThread, $00
//  je @Done
  xor ecx, ecx  //Sleep Time = 0
@AttemptLock:
  xor eax, eax
  mov dl, 1
  lock cmpxchg LockAccess, dl
  jz @Done

  //Sleep
  push 0
  push ecx
  call Windows.SleepEx

  pause          

  mov ecx, 1  //Change Sleep Time to 1
  jmp @AttemptLock

@Done:
end;

{$ENDIF}

function GetMemSpaceGroup(Size: Integer): TMemorySpaceGroups;
begin
  if Size <= MEM_SPACE_SIZE then
    Result := IdxMemSpaceGroups[Size]
  else
    Result := msz_Infinite;
end;

procedure ReturnMemoryItemSegment(SegmentIdx: Integer);
var
  SegAddr:  Integer;
  ParItem:  pMemoryItem;
  MemItem:  pMemoryItem;
  ParSpace: Integer;
begin
  //For each MemoryItem associated with this memory segment, remove it from
  // the OpenMemoryItems list.
  ParItem := nil;
  MemItem := OpenMemoryItems.btChild[ctLeft];
  while MemItem <> nil do
    begin
      if MemItem.MemorySeg = SegmentIdx then
        begin
          //Remove it from the list
          if ParItem = nil then
            OpenMemoryItems.btChild[ctLeft] := MemItem.btChild[ctLeft]
          else
            ParItem.btChild[ctLeft]         := MemItem.btChild[ctLeft];

          //Advance to the next item
          MemItem        := MemItem.btChild[ctLeft];
        end
      else
        begin
          ParItem  := MemItem;
          MemItem        := MemItem.btChild[ctLeft];
        end;
    end;

  //Remove Memory Segment from UsedMemorySegments list.
  ParSpace := -1;
  SegAddr  := UsedMemorySegments;
  while SegAddr <> -1 do
    begin
      if SegAddr = SegmentIdx then
        begin
          if ParSpace = -1 then
            begin
              UsedMemorySegments    := MemorySegments[SegAddr].Next;
            end
          else
            begin
              MemorySegments[ParSpace].Next := MemorySegments[SegAddr].Next;
            end;

          Break;
        end
      else
        begin
          ParSpace := SegAddr;
          SegAddr  := MemorySegments[SegAddr].Next;
        end;
    end;

  //Add Memory Segment to the OpenMemorySegments list.
  MemorySegments[SegmentIdx].Next := OpenMemorySegments;
  OpenMemorySegments              := SegmentIdx;

  //Physically release the Memory.
  VirtualFree(MemorySegments[SegmentIdx].Physical, 0, MEM_RELEASE);
//  if not VirtualFree(MemorySegments[SegmentIdx].Physical, 0, MEM_RELEASE) then
//    SysErrorMessage(GetLastError);
end;

procedure ReturnMemorySegment(SegmentIdx: Integer);
var
  SegAddr:  Integer;
  SpaceIdx: Integer;
  ParSpace: Integer;
  NxtSpace: Integer;
begin
  //For each MemorySpace associated with this memory segment, move it from
  // the FreeMemorySpace list to the OpenMemorySpace list.
  ParSpace := -1;
  SpaceIdx := FreeMemorySpace;
  while SpaceIdx <> -1 do
    begin
      if (Cardinal(MemorySpaces[SpaceIdx].pSpace) >= Cardinal(MemorySegments[SegmentIdx].Physical)) and
         (Cardinal(MemorySpaces[SpaceIdx].pSpace) < (Cardinal(MemorySegments[SegmentIdx].Physical) + MEM_SEGMENT_SIZE)) then
        begin
          NxtSpace := MemorySpaces[SpaceIdx].Next;
          if ParSpace = -1 then
            begin
              //Add to OpenMemorySpaces
              Inc(OpenMemorySpace);

              //Remove from FreeMemoryList
              FreeMemorySpace             := NxtSpace;

              SpaceIdx                    := NxtSpace;
            end
          else
            begin
              //Add to OpenMemorySpaces
              Inc(OpenMemorySpace);

              //Remove from FreeMemoryList
              MemorySpaces[ParSpace].Next := NxtSpace;

              SpaceIdx                    := NxtSpace;
            end;
        end
      else
        begin
          ParSpace := SpaceIdx;
          SpaceIdx := MemorySpaces[SpaceIdx].Next;
        end;
    end;

  //Remove Memory Segment from UsedMemorySegments list.
  ParSpace := -1;
  SegAddr  := UsedMemorySegments;
  while SegAddr <> -1 do
    begin
      if SegAddr = SegmentIdx then
        begin
          if ParSpace = -1 then
            begin
              UsedMemorySegments    := MemorySegments[SegAddr].Next;
            end
          else
            begin
              MemorySegments[ParSpace].Next := MemorySegments[SegAddr].Next;
            end;

          Break;
        end
      else
        begin
          ParSpace := SegAddr;
          SegAddr  := MemorySegments[SegAddr].Next;
        end;
    end;

  //Add Memory Segment to the OpenMemorySegments list.
  MemorySegments[SegmentIdx].Next := OpenMemorySegments;
  OpenMemorySegments              := SegmentIdx;

  //Physically release the Memory.
  VirtualFree(MemorySegments[SegmentIdx].Physical, 0, MEM_RELEASE);
//  if not VirtualFree(MemorySegments[SegmentIdx].Physical, 0, MEM_RELEASE) then
//    SysErrorMessage(GetLastError);
end;

procedure DecrementMemorySegmentRefCount(Addr: Cardinal);
var
  SegmentIdx: Integer;
begin
  SegmentIdx := UsedMemorySegments;
  while SegmentIdx <> -1 do
    begin
      if (Cardinal(Addr) >= Cardinal(MemorySegments[SegmentIdx].Physical)) and
         (Cardinal(Addr) < (Cardinal(MemorySegments[SegmentIdx].Physical) + MEM_SEGMENT_SIZE)) then
        Break;

      SegmentIdx := MemorySegments[SegmentIdx].Next;
    end;

  Assert(SegmentIdx <> -1, 'Could not find Memory Segment to decrement refcount');

  Dec(MemorySegments[SegmentIdx].RefCount);
  if MemorySegments[SegmentIdx].RefCount = 0 then
    ReturnMemorySegment(SegmentIdx);
end;

procedure IncrementMemorySegmentRefCount(const Addr: Cardinal);
var
  SegmentIdx: Integer;
begin
  SegmentIdx := UsedMemorySegments;
  while SegmentIdx <> -1 do
    begin
      if (Cardinal(Addr) >= Cardinal(MemorySegments[SegmentIdx].Physical)) and
         (Cardinal(Addr) < (Cardinal(MemorySegments[SegmentIdx].Physical) + MEM_SEGMENT_SIZE)) then
        Break;

      SegmentIdx := MemorySegments[SegmentIdx].Next;
    end;

  Assert(SegmentIdx <> -1, 'Could not find Memory Segment to increment refcount');

  Inc(MemorySegments[SegmentIdx].RefCount);
end;

procedure ReturnMemorySpace(Space: Integer);
var
  ParItem: pMemoryItem;
  MemItem: pMemoryItem;
  NextItem: pMemoryItem;
  Group: TMemorySpaceGroups;
begin
  Group    := MemorySpaces[Space].MemoryGroup;

  //Return all associated Memory Items to the open list.
  ParItem := nil;
  MemItem := MemorySpaceGroups[Group].FreeMemItem;
  while MemItem <> nil do
    begin
      if MemItem.MemorySpace = Space then
        begin
          //This item is part of the memory space being returned and needs to
          // be added back to the open list.

          //Remove from the group free list.
          NextItem := MemItem.btChild[ctLeft];
          if ParItem = nil then
            MemorySpaceGroups[Group].FreeMemItem := MemItem.btChild[ctLeft]
          else
            ParItem.btChild[ctLeft] := MemItem.btChild[ctLeft];

          //Add the memory item back to the open list.
          MemItem.btChild[ctLeft] := OpenMemoryItems.btChild[ctLeft];
          OpenMemoryItems.btChild[ctLeft] := MemItem;

          Dec(MemorySegments[MemItem.MemorySeg].RefCount);
          if MemorySegments[MemItem.MemorySeg].RefCount <= 0 then
            ReturnMemoryItemSegment(MemItem.MemorySeg);

          MemItem := NextItem;
        end
      else
        begin
          ParItem := MemItem;
          MemItem := MemItem.btChild[ctLeft];
        end;
    end;

  //Remove Memory Space from the Group Used Memory List.
  if MemorySpaces[Space].Prev = -1 then
    begin
      MemorySpaceGroups[Group].UsedSpace := MemorySpaces[Space].Next;
      if MemorySpaces[Space].Next <> -1 then
        MemorySpaces[MemorySpaces[Space].Next].Prev := -1;
    end
  else
    begin
      MemorySpaces[MemorySpaces[Space].Prev].Next := MemorySpaces[Space].Next;
      if MemorySpaces[Space].Next <> -1 then
        MemorySpaces[MemorySpaces[Space].Next].Prev := MemorySpaces[Space].Prev;
    end;

  //Return Memory Space to the Open list.
  MemorySpaces[Space].Next := FreeMemorySpace;
  FreeMemorySpace          := Space;

  //Reset Lookup list
  MemorySpaceSizeLookup[Space] := msz_Infinite;

  //Decrement Reference Count for Associated Memory Segment
  DecrementMemorySegmentRefCount(Cardinal(MemorySpaces[Space].pSpace));
end;

//***** Implement Red-Black Tree Algorithms

procedure rbInsert(aItem: pMemoryItem);
var
  pa: array[0..47] of pMemoryItem; //RB_MAX_HEIGHT = 48
  da: array[0..47] of TtdChildType;
  k:  Integer; //StackHeight

  p:  pMemoryItem;  //Insertion Point?
  n:  pMemoryItem;  //Newly inserted node?
  y:  pMemoryItem;
  x:  pMemoryItem;

  cmp: Integer;
begin
  pa[0] := @UsedMemoryItems;
  da[0] := ctLeft;
  k := 1;

  p := UsedMemoryItems.btChild[ctLeft];

  while p <> nil do
    begin
      cmp := Cardinal(aItem.Memory) - Cardinal(p^.Memory);

      //Assume that the Node is not in the tree.
      if cmp < 0 then
        begin
          pa[k] := p;
          da[k] := ctLeft;
        end
      else
//      else if cmp > 0 then
        begin
          pa[k] := p;
          da[k] := ctRight;
        end;
//      else
//        raise Exception.Create('Node already in tree');

      Inc(k);
      p := p^.btChild[da[k - 1]];
    end;

  pa[k - 1]^.btChild[da[k-1]] := aItem;
  n := aItem;
  n^.btChild[ctLeft] := nil;
  n^.btChild[ctRight] := nil;
  n^.btColor := rbRed;

  while (k >= 3) and (pa[k - 1]^.btColor = rbRed) do
    begin
      if da[k - 2] = ctLeft then
        begin
          y := pa[k - 2]^.btChild[ctRight];
          if (y <> nil) and (y^.btColor = rbRed {IsRed(y)}) then
            begin
              pa[k - 1]^.btColor := rbBlack;
              y^.btColor         := rbBlack;
              pa[k - 2]^.btColor := rbRed;
              Dec(k, 2);
            end
          else
            begin
              if da[k - 1] = ctLeft then
                begin
                  y := pa[k - 1];
                end
              else
                begin
                  x := pa[k - 1];
                  y := x^.btChild[ctRight];
                  x^.btChild[ctRight] := y^.btChild[ctLeft];
                  y^.btChild[ctLeft] := x;
                  pa[k - 2]^.btChild[ctLeft] := y;
                end;

              x := pa[k - 2];
              x^.btColor := rbRed;
              y^.btColor := rbBlack;

              x^.btChild[ctLeft] := y^.btChild[ctRight];
              y^.btChild[ctRight] := x;
              pa[k - 3]^.btChild[da[k - 3]] := y;

              Break;
            end;
        end
      else
        begin
          y := pa[k - 2]^.btChild[ctLeft];
          if (y <> nil) and (y^.btColor = rbRed) then
            begin
              pa[k - 1]^.btColor := rbBlack;
              y^.btColor := rbBlack;
              pa[k - 2]^.btColor := rbRed;
              Dec(k, 2);
            end
          else
            begin
              if da[k - 1] = ctRight then
                y := pa[k - 1]
              else
                begin
                  x := pa[k - 1];
                  y := x^.btChild[ctLeft];
                  x^.btChild[ctLeft] := y^.btChild[ctRight];
                  y^.btChild[ctRight] := x;
                  pa[k - 2]^.btChild[ctRight] := y;
                end;

              x := pa[k - 2];
              x^.btColor := rbRed;
              y^.btColor := rbBlack;

              x^.btChild[ctRight] := y^.btChild[ctLeft];
              y^.btChild[ctLeft] := x;
              pa[k - 3]^.btChild[da[k - 3]] := y;

              Break;
            end;
        end;
    end;

  UsedMemoryItems.btChild[ctLeft]^.btColor := rbBlack;

  Inc(MemorySpaces[aItem.MemorySpace].RefCount);
end;

function rbDelete(aItem: Pointer): Boolean;
var
  pa: array[0..47] of pMemoryItem; //RB_MAX_HEIGHT = 48
  da: array[0..47] of TtdChildType;
  k:  Integer; //StackHeight
  j:  Integer;

  p:  pMemoryItem;  //Insertion Point?
  y:  pMemoryItem;
  x:  pMemoryItem;
  r:  pMemoryItem;
  s:  pMemoryItem;
  w:  pMemoryItem;

  cmp: Integer;
  t: TtdRbColor;
begin
  Result := False;
  s := nil;

  k := 0;
  p := @UsedMemoryItems;

  cmp := -1;
  while (cmp <> 0) do
    begin
      pa[k] := p;
      if cmp > 0 then
        begin
          da[k] := ctRight;
          p     := p.btChild[ctRight];
        end
      else
        begin
          da[k] := ctLeft;
          p     := p.btChild[ctLeft];
        end;

      Inc(k);

      if p = nil then
        Exit;

      cmp := Cardinal(aItem) - Cardinal(p^.Memory);
    end;

  if p^.btChild[ctRight] = nil then
    pa[k - 1]^.btChild[da[k - 1]] := p^.btChild[ctLeft]
  else
    begin
      r := p^.btChild[ctRight];
      if r.btChild[ctLeft] = nil then
        begin
          r.btChild[ctLeft] := p.btChild[ctLeft];
          t := r.btColor;
          r.btColor := p.btColor;
          p.btColor := t;
          pa[k - 1].btChild[da[k  -1]] := r;
          da[k] := ctRight;
          pa[k] := r;
          Inc(k);
        end
      else
        begin
          j := k;
          Inc(k);

          while True do
            begin
              da[k] := ctLeft;
              pa[k] := r;
              Inc(k);
              s := r.btChild[ctLeft];
              if (s.btChild[ctLeft] = nil) then
                Break;

              r := s;
            end;

          da[j] := ctRight;
          pa[j] := s;
          pa[j - 1].btChild[da[j - 1]] := s;

          s.btChild[ctLeft] := p.btChild[ctLeft];
          r.btChild[ctLeft] := s.btChild[ctRight];
          s.btChild[ctRight] := p.btChild[ctRight];

          t := s.btColor;
          s.btColor := p.btColor;
          p.btColor := t;
        end;
    end;

  if p.btColor = rbBlack then
    begin

      while True do
        begin
          x := pa[k - 1].btChild[da[k - 1]];

          if (x <> nil) and (x.btColor = rbRed) then
            begin
              x.btColor := rbBlack;
              Break;
            end;

          if k < 2 then
            Break;

          if da[k - 1] = ctLeft then
            begin
              w := pa[k - 1].btChild[ctRight];

              if w.btColor = rbRed then
                begin
                  w.btColor := rbBLACK;
                  pa[k - 1].btColor := rbRED;

                  pa[k - 1].btChild[ctRight] := w.btChild[ctLeft];
                  w.btChild[ctLeft] := pa[k - 1];
                  pa[k - 2].btChild[da[k - 2]] := w;

                  pa[k] := pa[k - 1];
                  da[k] := ctLeft;
                  pa[k - 1] := w;
                  Inc(k);

                  w := pa[k - 1].btChild[ctRight];
                end;

              if ((w.btChild[ctLeft] = nil) or
                    (w.btChild[ctLeft].btColor = rbBlack)) and
                  ((w.btChild[ctRight] = nil) or
                      (w.btChild[ctRight].btColor = rbBlack)) then
                w.btColor := rbRed
              else
                begin
                  if (w.btChild[ctRight] = nil) or
                      (w.btChild[ctRight].btColor = rbBlack) then
                    begin
                      y := w.btChild[ctLeft];
                      y.btColor := rbBlack;
                      w.btColor := rbRed;
                      w.btChild[ctLeft] := y.btChild[ctRight];
                      y.btChild[ctRight] := w;
                      w := y;
                      pa[k - 1].btChild[ctRight] := y;
                    end;

                  w.btColor := pa[k - 1].btColor;
                  pa[k - 1].btColor := rbBlack;
                  w.btChild[ctRight].btColor := rbBlack;

                  pa[k - 1].btChild[ctRight] := w.btChild[ctLeft];
                  w.btChild[ctLeft] := pa[k - 1];
                  pa[k - 2].btChild[da[k - 2]] := w;

                  Break;
                end;

            end
          else
            begin
              w := pa[k - 1].btChild[ctLeft];

              if (w.btColor = rbRed) then
                begin
                  w.btColor := rbBlack;
                  pa[k - 1].btColor := rbRed;

                  pa[k - 1].btChild[ctLeft] := w.btChild[ctRight];
                  w.btChild[ctRight] := pa[k - 1];
                  pa[k - 2].btChild[da[k - 2]] := w;

                  pa[k] := pa[k - 1];
                  da[k] := ctRight;
                  pa[k - 1] := w;
                  Inc(k);

                  w := pa[k - 1].btChild[ctLeft];
                end;

              if ((w.btChild[ctLeft] = nil) or
                  (w.btChild[ctLeft].btColor = rbBlack)) and
                 ((w.btChild[ctRight] = nil) or
                  (w.btChild[ctRight].btColor = rbBlack)) then
                w.btColor := rbRed
              else
                begin
                  if (w.btChild[ctLeft] = nil) or
                     (w.btChild[ctLeft].btColor = rbBlack) then
                    begin
                      y := w.btChild[ctRight];
                      y.btColor := rbBlack;
                      w.btColor := rbRed;
                      w.btChild[ctRight] := y.btChild[ctLeft];
                      y.btChild[ctLeft] := w;
                      w := y;
                      pa[k - 1].btChild[ctLeft] := y;
                    end;

                  w.btColor := pa[k - 1].btColor;
                  pa[k - 1].btColor := rbBlack;
                  w.btChild[ctLeft].btColor := rbBlack;

                  pa[k - 1].btChild[ctLeft] := w.btChild[ctRight];
                  w.btChild[ctRight] := pa[k - 1];
                  pa[k - 2].btChild[da[k - 2]] := w;

                  Break;
                end;
            end;

          Dec(k);
        end;
    end;

  //Add the free'd item to the free list
  p.btChild[ctLeft] := MemorySpaceGroups[p.MemoryGroup].FreeMemItem;
  MemorySpaceGroups[p.MemoryGroup].FreeMemItem := p;

  //Decrement Reference Count
  Dec(MemorySpaces[p^.MemorySpace].RefCount);
  if MemorySpaces[p^.MemorySpace].RefCount = 0 then
    ReturnMemorySpace(p^.MemorySpace);

  Result := True;
end;

procedure InitializeMemoryMap;
var
  i: Integer;
  iGrp: TMemorySpaceGroups;
begin
  FillCharJOH_IA32_c(IdxMemSpaceGroups[1],        16, char(msz_16));
  FillCharJOH_IA32_c(IdxMemSpaceGroups[17],       16, char(msz_32));
  FillCharJOH_IA32_c(IdxMemSpaceGroups[33],       32, char(msz_64));
  FillCharJOH_IA32_c(IdxMemSpaceGroups[65],       64, char(msz_128));
  FillCharJOH_IA32_c(IdxMemSpaceGroups[129],     128, char(msz_256));
  FillCharJOH_IA32_c(IdxMemSpaceGroups[257],     256, char(msz_512));
  FillCharJOH_IA32_c(IdxMemSpaceGroups[513],     512, char(msz_1024));
  FillCharJOH_IA32_c(IdxMemSpaceGroups[1025],   1024, char(msz_2048));
  FillCharJOH_IA32_c(IdxMemSpaceGroups[2049],   2048, char(msz_4096));
  FillCharJOH_IA32_c(IdxMemSpaceGroups[4097],   4096, char(msz_8192));
  FillCharJOH_IA32_c(IdxMemSpaceGroups[8193],   8192, char(msz_16384));
  FillCharJOH_IA32_c(IdxMemSpaceGroups[16385], 16384, char(msz_32768));
  FillCharJOH_IA32_c(IdxMemSpaceGroups[32769], 32768, char(msz_65536));

  //Initialize Large Memory Segments to Not in Use
  FillCharJOH_IA32_c(LargeMemorySpaces[0], SizeOf(LargeMemorySpaces), #0);

  //Initialize the MemorySpaceSizeLookup table to mszInfinite
  FillCharJOH_IA32_c(MemorySpaceSizeLookup[0], SizeOf(MemorySpaceSizeLookup), char(msz_Infinite));

  //Place all Memory Segments on the Open list at this time.
  UsedMemorySegments := -1;
  OpenMemorySegments := 0;
  for i := Low(MemorySegments) to Pred(High(MemorySegments)) do
    begin
      MemorySegments[i].Physical := nil;
      MemorySegments[i].Next     := i + 1;
    end;
  MemorySegments[High(MemorySegments)].Next   := -1;


  //Place all Memory Spaces on the Open list at this time.
  FreeMemorySpace := -1;
  OpenMemorySpace := MAX_SPACES;

  //The Memory Space Groups are initialized with zero groups allocated.
  UsedMemoryItems.btChild[ctLeft]  := nil;
  UsedMemoryItems.btChild[ctRight] := nil;
  UsedMemoryItems.Parent           := nil;
  UsedMemoryItems.Memory           := nil;

  OpenMemoryItems.btChild[ctLeft]  := nil;
  for iGrp := Low(MemorySpaceGroups) to High(MemorySpaceGroups) do
    begin
      MemorySpaceGroups[iGrp].Size        := MemorySpaceGroupSize[iGrp];
      MemorySpaceGroups[iGrp].FreeMemItem := nil;
      MemorySpaceGroups[iGrp].UsedSpace   := -1;
    end;
end;

procedure CleanUpMemorySegments;
var
  iPar: Integer;
  iSeg: Integer;
begin
  //Clean up Memory Item Segment Types
  iSeg := UsedMemorySegments;
  iPar := -1;
  while iSeg <> -1 do
    begin
      if MemorySegments[iSeg].SegmentType = 1 then
        begin
          if not VirtualFree(MemorySegments[iSeg].Physical, 0, MEM_RELEASE) then
            begin
              //do something with this error message????
//              syserrormessage(getlasterror);
              Break;
            end
          else
            begin
              //Segment Free'd OK
              if iPar = -1 then
                begin
                  UsedMemorySegments := MemorySegments[iSeg].Next;
                  iSeg := UsedMemorySegments;
                end
              else
                begin
                  MemorySegments[iPar].Next := MemorySegments[iSeg].Next;
                  iSeg := MemorySegments[iPar].Next;
                end;
            end;
        end
      else
        begin
          iPar := iSeg;
          iSeg := MemorySegments[iSeg].Next;
        end;
    end;
end;

function AddNewMemorySegment: Boolean;
var
  MemSeg: Pointer;
  iSeg:   Integer;
  iSpace: Integer;
  NewSpace: Integer;
begin
  if OpenMemorySegments = -1 then
    begin
      Result := False;
      Exit;
    end;

  iSeg := OpenMemorySegments;

  MemSeg := VirtualAlloc(nil, MEM_SEGMENT_SIZE,
              MEM_COMMIT or MEM_TOP_DOWN, PAGE_READWRITE);

  if MemSeg = nil then
    begin
      Result := False;
      Exit;
    end;

  MemorySegments[iSeg].Physical := MemSeg;
  OpenMemorySegments            := MemorySegments[iSeg].Next;
  MemorySegments[iSeg].Next     := UsedMemorySegments;
  MemorySegments[iSeg].SegmentType := 0; //Normal User Segment
  UsedMemorySegments            := iSeg;

  //Add Free Memory Spaces
  for iSpace := 0 to MEM_SPACES_PER_SEGMENT - 1 do
    begin
      if OpenMemorySpace <= 0 then
        begin
          Result := False;
          Exit;
        end;

      Dec(OpenMemorySpace);

      NewSpace := Cardinal(MemSeg) + Cardinal(MEM_SPACE_SIZE * iSpace);
      NewSpace := NewSpace shr 16;

      //Add the new Memory Space to the Free Memory Space list.
      MemorySpaces[NewSpace].Next := FreeMemorySpace;
      FreeMemorySpace := NewSpace;

      //Point the Memory Space to its own segment of the allocated memory.
      MemorySpaces[NewSpace].pSpace := Pointer(Integer(MemSeg) + (MEM_SPACE_SIZE * iSpace));
    end;

  Result := True;
end;

function AddNewMemoryItems: Boolean;
var
  MemSeg: Pointer;
  iSeg:   Integer;
  iItem:  Integer;
begin
  if OpenMemorySegments = -1 then
    begin
      Result := False;
      Exit;
    end;

  iSeg := OpenMemorySegments;

  MemSeg := VirtualAlloc(nil, MEM_SEGMENT_SIZE,
              MEM_COMMIT or MEM_TOP_DOWN, PAGE_READWRITE);

  if MemSeg = nil then
    begin
      Result := False;
      Exit;
    end;

  MemorySegments[iSeg].Physical := MemSeg;
  OpenMemorySegments            := MemorySegments[iSeg].Next;
  MemorySegments[iSeg].Next     := UsedMemorySegments;
  MemorySegments[iSeg].SegmentType := 1; //Memory Items Segment
  UsedMemorySegments            := iSeg;

  //Add Free Memory Item Pointers
  for iItem := 1 to MEM_ITEM_POINTERS do
    begin
      TMemoryItem(MemSeg^).Memory := nil;
      TMemoryItem(MemSeg^).Parent := nil;
      TMemoryItem(MemSeg^).btChild[ctLeft]   := OpenMemoryItems.btChild[ctLeft];
      OpenMemoryItems.btChild[ctLeft]        := MemSeg;
      TMemoryItem(MemSeg^).MemorySeg         := iSeg;
      Inc(Cardinal(MemSeg), SizeOf(TMemoryItem));
    end;

  MemorySegments[iSeg].RefCount := 0; //MEM_ITEM_POINTERS;

  Result := True;
end;

function AddNewMemorySpace(Group: TMemorySpaceGroups): Boolean;
var
  iSpace: Integer;
  iItem:  Integer;
  MemItem: pMemoryItem;
begin
  if FreeMemorySpace = -1 then
    begin
      //Get a new Memory Segment
      if not AddNewMemorySegment then
        begin
          Result := False;
          Exit;
        end;
    end;

  //Get Free Memory Space
  iSpace                             := FreeMemorySpace;
  FreeMemorySpace                    := MemorySpaces[iSpace].Next;

  if MemorySpaceGroups[Group].UsedSpace <> -1 then
    MemorySpaces[MemorySpaceGroups[Group].UsedSpace].Prev := iSpace;

  MemorySpaces[iSpace].Next          := MemorySpaceGroups[Group].UsedSpace;
  MemorySpaces[iSpace].Prev          := -1;
  MemorySpaceGroups[Group].UsedSpace := iSpace;

  MemorySpaces[iSpace].MemoryGroup   := Group;
  MemorySpaces[iSpace].RefCount      := 0;

  IncrementMemorySegmentRefCount(Cardinal(MemorySpaces[iSpace].pSpace));

  MemorySpaceSizeLookup[iSpace] := Group;

  //Add Memory Pointers to Groups FreeSpace
  for iItem := 0 to MemoryItemsPerSpace[Group] - 1 do
    begin
      if OpenMemoryItems.btChild[ctLeft] = nil then
        begin
          if not AddNewMemoryItems then
            begin
              Result := False;
              Exit;
            end;
        end;

      //Get Next Open Memory Item
      MemItem := OpenMemoryItems.btChild[ctLeft];
      OpenMemoryItems.btChild[ctLeft] := MemItem.btChild[ctLeft];

      //Add it to the free list for this group
      MemItem.btChild[ctLeft] := MemorySpaceGroups[Group].FreeMemItem;
      MemorySpaceGroups[Group].FreeMemItem := MemItem;

      MemItem.Memory := Pointer(Cardinal(MemorySpaces[iSpace].pSpace) +
                        Cardinal(MemorySpaceGroupSize[Group] * iItem));
      MemItem.MemoryGroup := Group;
      MemItem.MemorySpace := iSpace;
      Inc(MemorySegments[MemItem.MemorySeg].RefCount);
    end;

  Result := True;
end;

function FindLargeMemoryItem(MemPtr: Pointer): pLargeMemorySegment; //Pointer;
begin
  Result := nil;
  if Cardinal(Cardinal(MemPtr) and $0000FFFF) <> SizeOf(TLargeMemorySegment) then
    Exit;

  Result := Pointer(Cardinal(MemPtr) and $FFFF0000);

  if not LargeMemorySpaces[Cardinal(Result) shr 16] then
    begin
      Result := nil;
    end;
end;

function Do_GetMemLarge(Size: Integer): Pointer;
var
  LgMem: pLargeMemorySegment;
begin
  //Add 25% to the size of the allocated block for growth purposes
  Size := Trunc(Size * 1.25);

  LgMem := VirtualAlloc(nil, Size + SizeOf(TLargeMemorySegment),
             MEM_COMMIT or MEM_TOP_DOWN, PAGE_READWRITE);
  LgMem.Physical := Pointer(Cardinal(LgMem) + SizeOf(TLargeMemorySegment));
  LgMem.Size := Size + SizeOf(TLargeMemorySegment);
  LargeMemorySpaces[Cardinal(LgMem) shr 16] := True;

  Result := LgMem.Physical;
end;

function Do_GetMemSmall(TgtGroup: TMemorySpaceGroups): Pointer;
var
  MemItem: pMemoryItem;
begin
  Assert(TgtGroup <> High(TMemorySpaceGroups), 'This method for small allocations only.');

  if MemorySpaceGroups[TgtGroup].FreeMemItem = nil then
    begin
      if not AddNewMemorySpace(TgtGroup) then
        begin
          Result := nil;
          Exit;
        end;
    end;

  //alloc memory
  MemItem := MemorySpaceGroups[TgtGroup].FreeMemItem;
  MemorySpaceGroups[TgtGroup].FreeMemItem := MemItem^.btChild[ctLeft];

  rbInsert(MemItem);

  Result := MemItem.Memory;
end;

function Do_GetMem(Size: Integer): Pointer;
var
  TgtGroup: TMemorySpaceGroups;
begin
  TgtGroup := GetMemSpaceGroup(Size);

  if TgtGroup = msz_Infinite then
    Result := Do_GetMemLarge(Size)
  else
    Result := Do_GetMemSmall(TgtGroup);
end;

function EWCGetMem(Size: Integer): Pointer;
var
  Rem: Integer;
begin
  //Ensure Size is a multiple of 4 which will allow for routines to read the
  // the last fullword at the end of the allocated memory.
  Rem := Size mod 4;
  if Rem > 0 then
    Inc(Size, 4 - Rem);

{$IFDEF USECRITSECT}
  EnterCriticalSection(LockAccess);
{$ELSE}
  Do_LockAccess;
{$ENDIF}

  Result := Do_GetMem(Size);

{$IFDEF USECRITSECT}
  LeaveCriticalSection(LockAccess);
{$ELSE}
  LockAccess := 0;
{$ENDIF}
end;

function Do_FreeLargeMem(LgMem: pLargeMemorySegment): Boolean;
begin
  LargeMemorySpaces[Cardinal(LgMem) shr 16] := False;
  Result := VirtualFree(LgMem, 0, MEM_RELEASE);
end;

function Do_FreeMem(P: Pointer): Integer;
var
  LgMem: pLargeMemorySegment;
begin
  if not rbDelete(P) then
    begin
      LgMem := FindLargeMemoryItem(P);
      if LgMem <> nil then
        begin
          if not Do_FreeLargeMem(LgMem) then
            begin
              Result := 1;
            end
          else
            begin
              Result := 0;
            end;
        end
      else
        Result := 1;
    end
  else
    begin
      Result := 0;
    end;
end;

function EWCFreeMem(P: Pointer): Integer;
begin
{$IFDEF USECRITSECT}
  EnterCriticalSection(LockAccess);
{$ELSE}
  Do_LockAccess;
{$ENDIF}

  Result := Do_FreeMem(P);

{$IFDEF USECRITSECT}
  LeaveCriticalSection(LockAccess);
{$ELSE}
  LockAccess := 0;
{$ENDIF}
end;

function Do_ReallocLarge(P: Pointer; Size: Integer): Pointer;
var
  LgMem: pLargeMemorySegment;
  MoveSz: Integer;
  TgtGroup: TMemorySpaceGroups;
begin
{$IFDEF USECRITSECT}
  EnterCriticalSection(LockAccess);
{$ELSE}
  Do_LockAccess;
{$ENDIF}

  LgMem := FindLargeMemoryItem(P);
  if LgMem <> nil then
    begin
      MoveSz := LgMem.Size - SizeOf(TLargeMemorySegment);

      if MoveSz < Size then
        begin
          //Upsize

          //Realloc is necessary
          Result := Do_GetMem(Size);
          if Result <> nil then
            begin
{$IFDEF USEFASTMOVE}
              MoveJOH_IA32_6(P^, Result^, MoveSz);
{$ELSE}
              Move(P^, Result^, MoveSz);
{$ENDIF}

              if not Do_FreeLargeMem(LgMem) then
               Result := nil;
            end;
        end
      else
        begin
          //Downsize
          //If this downsizes to a small block, resize the memory.
          TgtGroup := GetMemSpaceGroup(Size);
          if TgtGroup <> msz_Infinite then
            begin
              Result := Do_GetMemSmall(TgtGroup);
              if Result <> nil then
                begin
{$IFDEF USEFASTMOVE}
                  MoveJOH_IA32_6(P^, Result^, Size); //Smaller of the size
{$ELSE}
                  Move(P^, Result^, Size); //Smaller of the size
{$ENDIF}

                  if not Do_FreeLargeMem(LgMem) then
                    Result := nil;
                end;
            end
          else
            begin
              //Doesn't downsize Large allocations when target and source are
              // msz_Infinite....  Need to address this
//if size difference greater than some threshhold, then getmem, move size, free large mem
              Result := P;
            end;
        end;

    end //end large mem
  else
    Result := nil;

{$IFDEF USECRITSECT}
  LeaveCriticalSection(LockAccess);
{$ELSE}
  LockAccess := 0;
{$ENDIF}
end;

function EWCReallocMem(P: Pointer; Size: Integer): Pointer;

  function ReallocSmall(P: Pointer; Size: Integer;
    SrcGroup, TgtGroup: TMemorySpaceGroups): Pointer;
  var
    MoveSz: Integer;
  begin
{$IFDEF USECRITSECT}
    EnterCriticalSection(LockAccess);
{$ELSE}
    Do_LockAccess;
{$ENDIF}

    MoveSz := MemorySpaceGroupSize[SrcGroup];
    if MoveSz > Size then
      MoveSz := Size;

    //Realloc is necessary.
    if TgtGroup = msz_Infinite then
      Result := Do_GetMemLarge(Size)
    else
      Result := Do_GetMemSmall(TgtGroup);
    if Result <> nil then
      begin
{$IFDEF USEFASTMOVE}
        MoveJOH_IA32_6(P^, Result^, MoveSz);
{$ELSE}
        Move(P^, Result^, MoveSz);
{$ENDIF}

        if Do_FreeMem(P) <> 0 then
         Result := nil;
      end;

{$IFDEF USECRITSECT}
    LeaveCriticalSection(LockAccess);
{$ELSE}
    LockAccess := 0;
{$ENDIF}
  end;

var
  TgtGroup: TMemorySpaceGroups;
  SrcGroup: TMemorySpaceGroups;
  Rem: Integer;
begin
  //Ensure Size is a multiple of 4 which will allow for routines to read the
  // the last fullword at the end of the allocated memory.
  Rem := Size mod 4;
  if Rem > 0 then
    Inc(Size, 4 - Rem);

  SrcGroup := MemorySpaceSizeLookup[(Cardinal(P) shr 16)];

  if SrcGroup = msz_Infinite then
    begin
      Result := Do_ReallocLarge(P, Size);
    end
  else
    begin
      TgtGroup := GetMemSpaceGroup(Size);
      if TgtGroup = SrcGroup then
        begin
          Result := P;
        end
      else if (SrcGroup < msz_16384) and (TgtGroup = Pred(SrcGroup)) then
        begin
          //Don't reallocate if the target is just one bucket below the source.
          // But only for allocations smaller than 16K or too much memory might
          // be wasted.
          Result := P;
        end
      else
        begin
          Result := ReallocSmall(P, Size, SrcGroup, TgtGroup);
        end;
    end;
end;

initialization

  EWCMemoryManager.GetMem     := EWCGetMem;
  EWCMemoryManager.FreeMem    := EWCFreeMem;
  EWCMemoryManager.ReallocMem := EWCReallocMem;

  GetMemoryManager(OldMemoryManager);
  SetMemoryManager(EWCMemoryManager);

{$IFDEF USECRITSECT}
  InitializeCriticalSection(LockAccess);
{$ELSE}
  LockAccess := 0;
{$ENDIF}

  InitializeMemoryMap;

finalization

  SetMemoryManager(OldMemoryManager);

  CleanUpMemorySegments;

end.
