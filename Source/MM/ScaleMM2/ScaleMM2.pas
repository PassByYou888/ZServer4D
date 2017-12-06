// fast scaling memory manager for Delphi
// licensed under a MPL/GPL/LGPL tri-license; version 0.1
unit ScaleMM2;

{$Include smmOptions.inc}

{
Fast Scaling Memory Manager 2.4 for Delphi

Description:
  Simple, small and compact Memory Manager. Architectured in order to scale on
  multi core CPU's (which is what FastMM4 is lacking).

Homepage:
  http://code.google.com/p/scalemm
  by AndrMussche (andre.mussche@gmail.com)

Usage:
 - Place ScaleMM2 as the very first unit under the "uses" clause of your
   project's .dpr file.

License:
  Released under Mozilla Public License 1.1

  Modifications by A.Bouchez - http://synopse.info:
  - Compiles from Delphi 6 up to Delphi XE;
  - Some pascal code converted to faster asm;
  - Some code refactoring, a lot of comments added;
  - Added medium block handling from 2048 bytes up to 16384;
  - Released under MPL 1.1/GPL 2.0/LGPL 2.1 tri-license.

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  Portions created by the Initial Developer are Copyright (C) 2010
  the Initial Developer. All Rights Reserved.

  Contributor(s):
  - Arnaud Bouchez http://synopse.info
  Portions created by each contributor are Copyright (C) 2010
  each contributor. All Rights Reserved.

  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. If you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. If you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****

Change log:
 Version 1.0 (3 December 2010):
  - First stable version
 Version 1.1 (6 December 2010):
  - Some optimizations for better "Fast Code MM Challenge Benchmark" results
    (lower memory overhead, more memory reuse, less locking)
 Version 2.0.2a (25 Januari 2011):
  - added medium memory handling (<1Mb), large memory is direct done via VirtualAlloc etc
  - splitted in seperate units to make developing/testing easier
  - empty units for stats and logging (will be implemented later)
 Version 2.0.4b (23 Februari 2011):
  - realloc optimizations
  - lots of internal CheckMem's (for validation)
  - interthread memory is now handled (alloc in thread 1, free in thread 2)
  - small (difficult to find) bugs fixed and other optimalizations
  - check for 8byte alignment (needed for OmniThreadLibrary etc)
 Version 2.0.5 (19 March 2011):
  - small size realloc bug fixed, now passes FastCode validations (D2007 and D2010)
 Version 2.1 (06 March 2012):
  - small bugs fixed
  - many additional checks added
  - interthread memory finally stable
  Note: can leak memory (or have increased mem usage) over time
 Version 2.1.1 (08 March 2012):
  - 64bit version (Delphi XE2)
 Version 2.1.2 (13 March 2012):
  - Initial code for releasing (and checking) all mem at shutdown
  - fixed big mem leak in small memory manager (interthread mem was never freed)
  - optimization for interthread memory in small mem manager (lock-free with CAS)
 Version 2.1.3 (28 March 2012):
  - shared memory implementation (thanks to FastMM for the code, Maxx xxaM for testing)
  - Realloc bug fix with "large mem" (thanks to Maxx xxaM)
 Version 2.1.4 (21 May 2012):
 - fixed issue 6: AV when using SetLocaleOverride (bug in DXE2?) (thanks to Maxx xxaM)
 Version 2.1.5 (27 July till 27 Sept 2012), thanks to Maxx xxaM:
 - 64bit issues fixed
 - inplace expanded virtualmem (realloc) was not properly freed
 - realloc of large memory, size was increased twice with 25% (=50%)
 - memleak + CAS hang (due to integer overflow)
 - large interthread memory was not correctly freed
 Version 2.1.6 (9-3-2013), thanks to Maxx xxaM and Thiago:
 - 64bit wrong alignment can give AV's (issue 11)
 Version 2.2 (6-8-2013)
 - make it possible to use more than 2gb in 32bit (thanks to Maxx xxaM)
 	{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE
 - rare AV with interthread (small) memory and heavy load (pdeusp)
 Version 2.3 (22-10-2013)
 - interthread memory was not always released to Windows, giving out of memory (thanks to Maxx xxaM)
 - AV when doing realloc or free on nil pointer (thanks to qiu.songlin)
 - fixed issue 16: alloc of large 2 dimensional array was getting slower and slower (thanks to tf.rangel)
 Version 2.4 (12-12-2013)
 - interthread memory fixes (stability)
 Version 2.4.1 (16-12-2013)
 - optimize.move included (much faster Move() function due to SSE3)
 Version 2.5 (10-11-2015)
 - rare AV fixed in 64bit with high load due to misalignment
 Version 2.5.1 (16-3-2016)
 - rare AV fixed in TGlobalMemManager.GetNewThreadManager (thanks to Molnár Attila)
 Version 2.6 (27-4-2016)
 - increasing memory usage with lot of short living threads (DataSnap, Indy) (thanks to Hans Wendel)
}

interface

uses
  smmStatistics, smmLogging,
  smmTypes,
  smmSmallMemory, smmMediumMemory, smmLargeMemory;

type
  PThreadMemManager = ^TThreadMemManager;

  /// handles per-thread memory managment
  TThreadMemManager = object
  public
    /// link to the list of mem freed in other thread
    FOtherThreadFreedMemory: PBaseFreeMemHeader;
    FOtherThreadFreeLock: NativeUInt;
    FOtherThreadFreeLockRecursion: NativeUInt;

    function  TryLock: boolean;
    procedure Lock;
    procedure UnLock;
  public
    FThreadId: NativeUint;
    FThreadTerminated: Boolean;  //is this thread memory available to new thread?

    // link to list of items to reuse after thread terminated
    FNextThreadManager: PThreadMemManager;
    FNextFreeThreadManager: PThreadMemManager;

    //procedure AddFreeMemToOwnerThread(aFirstMem, aLastMem: PBaseFreeMemHeader);
  public
    {$IFDEF Align8Bytes}
      {$ifndef CPUX64}    //32bit
      Filer1: Int32;
      {$endif}
    {$ENDIF}
    {$IFDEF Align16Bytes}
      {$ifndef CPUX64}    //32bit
      Filer1: Pointer;
      //Filer2: Pointer;
      {$else CPUX64}      //64bit
      Filer1: Pointer;
      //Filer2: Pointer;
      {$endif}
    {$ENDIF}

    FSmallMemManager : TSmallMemThreadManager;
    FMediumMemManager: TMediumThreadManager;
    FLargeMemManager : TLargeMemThreadManager;
  protected
    {$IFDEF SCALEMM_STATISTICS}
    FStatistic: TMemoryStatistics;
    {$ENDIF}
    {$IFDEF SCALEMM_LOGGING}
    FLogging: TMemoryLogging;
    {$ENDIF}

    {$IFDEF Align8Bytes}
      {$ifndef CPUX64}    //32bit
      Filer_1: Int32;
      {$endif}
    {$ENDIF}
    {$IFDEF Align16Bytes}
      {$ifndef CPUX64}    //32bit
      Filer_1: Pointer;
      {$else CPUX64}      //64bit
      Filer_1: Pointer;
      {$endif}
    {$ENDIF}
  protected
    procedure FreeMemOfOtherThread(aMemory: PBaseMemHeader);
    function  ReallocMemOfOtherThread(aMemory: Pointer; aSize: NativeUInt): Pointer;

    function  FreeMemFromOtherThread(aMemory: PBaseMemHeader): NativeInt;
  public
    procedure Init;
    procedure Reset;

    procedure ReleaseAllFreeMem;
    procedure CheckMem(aMemory: Pointer);
    procedure DumpToFile(aFile: THandle; aTotalStats, aSingleStats: PThreadMemManagerStats);

    function  IsMemoryFromOtherThreadsPresent: Boolean;
    procedure ProcessFreedMemFromOtherThreads(aSkipSmall: boolean);

    function GetMem(aSize: NativeInt) : Pointer;                       {$ifdef HASINLINE}inline;{$ENDIF}
    function FreeMem(aMemory: Pointer): NativeInt;                     {$ifdef HASINLINE}inline;{$ENDIF}
    function ReallocMem(aMemory: Pointer; aSize: NativeUInt): Pointer; {$ifdef HASINLINE}inline;{$ENDIF}
  end;

  {$if CompilerVersion >= 23}  //Delphi XE2
  function Scale_GetMem(aSize: NativeInt)   : Pointer;
  function Scale_AllocMem(aSize: NativeInt): Pointer;
  function Scale_ReallocMem(aMemory: Pointer; aSize: NativeInt): Pointer;
  {$else}
  function Scale_GetMem(aSize: Integer)   : Pointer;
  function Scale_AllocMem(aSize: Cardinal): Pointer;
  function Scale_ReallocMem(aMemory: Pointer; aSize: Integer): Pointer;
  {$ifend}
  function Scale_FreeMem(aMemory: Pointer): Integer;

  procedure Scale_CheckMem(aMemory: Pointer);

  function GetThreadMemManager: PThreadMemManager;
  function CreateMemoryManager: PThreadMemManager;

  procedure ScaleMMInstall;
  procedure ScaleMMUnInstall;

{$IFDEF PURE_PASCAL}
threadvar
  GCurrentThreadManager: PThreadMemManager;
{$ENDIF}

implementation

// Windows.pas unit dependency should be not used -> seperate file
uses
  {$IFDEF CPUX86}
  Optimize.Move,
  {$ENDIF}
  smmFunctions, smmGlobal;

{$IFDEF PURE_PASCAL}
function GetThreadMemManager: PThreadMemManager; {$ifdef HASINLINE}inline;{$ENDIF}
begin
  Result := GCurrentThreadManager;
  if Result = nil then
  begin
    Result := CreateMemoryManager;
    Assert(not Result.FThreadTerminated);
    Assert(not Result.FSmallMemManager.OwnerThread.FThreadTerminated);
  end;
  if Result.FThreadTerminated then
    //Assert(Result.FThreadId = 1);
  else
  begin
    Assert(Result.FThreadId = GetCurrentThreadId);
    Assert(Result.FSmallMemManager.OwnerThread.FThreadId = GetCurrentThreadId);
  end;
  Assert(Result.FSmallMemManager.OwnerThread = PBaseThreadManager(Result));
  Assert(Result.FMediumMemManager.OwnerThread = PBaseThreadManager(Result));
end;
{$ELSE}
var
  GOwnTlsIndex,
  GOwnTlsOffset: NativeUInt;

function GetThreadMemManager: PThreadMemManager;
asm
{$IFDEF SCALE_INJECT_OFFSET}
  mov eax,123456789        // dummy value: calc once and inject at runtime
{$ELSE}
  mov eax,GOwnTlsOffset    // 2% slower, so we default use injected offset
{$ENDIF}
  mov ecx,fs:[$00000018]
  mov eax,[ecx+eax]      // fixed offset, calculated only once
  or eax,eax
  jz CreateMemoryManager
end;

procedure _FixedOffset;
{$IFDEF SCALE_INJECT_OFFSET}
var p: PAnsiChar;
{$ENDIF}
begin
  GOwnTlsOffset := GOwnTlsIndex * 4 + $0e10;
  {$IFDEF SCALE_INJECT_OFFSET}
  p  := @GetThreadMemManager;
  SetPermission(p, 5, PAGE_EXECUTE_READWRITE);
  PCardinal(p+1)^ := GOwnTlsOffset;  // write fixed offset
  {$ENDIF}
end;
{$ENDIF PURE_PASCAL}

function CreateMemoryManager: PThreadMemManager;
begin
  Result := smmGlobal.GlobalManager.GetNewThreadManager;
//  if Result = nil then
//  begin
//    Result := VirtualAlloc( nil,
//                            64 * 1024,
//                            SizeOf(TThreadMemManager),
//                            MEM_COMMIT {$ifdef AlwaysAllocateTopDown} or MEM_TOP_DOWN{$endif},
//                            PAGE_READWRITE);
//    Result.Init;
//  end
//  else
  begin
    Result.FThreadId := GetCurrentThreadId;
    Result.FThreadTerminated := False;
  end;

  {$IFDEF SCALEMM_DEBUG}
  Result.CheckMem(nil);
  {$ENDIF}

  {$IFDEF PURE_PASCAL}
  GCurrentThreadManager := Result;
  {$ELSE}
  TlsSetValue(GOwnTLSIndex, Result);
  {$ENDIF}
end;

{ TThreadMemManager }

procedure TThreadMemManager.ProcessFreedMemFromOtherThreads(aSkipSmall: boolean);
var
  pcurrentmem, ptempmem: PBaseFreeMemHeader;
begin
  if FOtherThreadFreedMemory = nil then Exit;

  //Assert(Self.FThreadId > 1);
  //LOCK
  if not TryLock then Exit;

  if not aSkipSmall and FSmallMemManager.IsMemoryFromOtherThreadsPresent then
    FSmallMemManager.FreeThreadFreedMem;

  pcurrentmem := FOtherThreadFreedMemory;
  FOtherThreadFreedMemory := nil;

  //UNLOCK
  UnLock;

  //free all mem in linked list
  while pcurrentmem <> nil do
  begin
    ptempmem    := pcurrentmem;
    pcurrentmem := pcurrentmem.NextThreadFree;

    //free
    Self.FreeMemFromOtherThread( PBaseMemHeader(ptempmem) );
  end;
end;

function TThreadMemManager.ReallocMem(aMemory: Pointer;
  aSize: NativeUInt): Pointer;
var
  pm: PBaseMemHeader;
  ot: PBaseSizeManager;
begin
  if FOtherThreadFreedMemory <> nil then
    ProcessFreedMemFromOtherThreads(True);

  pm := PBaseMemHeader(NativeUInt(aMemory) - SizeOf(TBaseMemHeader));
  //check realloc of freed mem
  if (pm.Size and 1 = 0) then //not free?
  begin
    //medium+large mem has ownerthread instead of ownerblock (optimization)
    if NativeUInt(pm.OwnerBlock) and 3 <> 0 then
    begin
      //other thread?
      if PThreadMemManager( NativeUInt(pm.OwnerBlock) and -4) <> @Self then
      begin
        Result := ReallocMemOfOtherThread(aMemory, aSize);
        Exit;
      end;

      //large or medium?
      if NativeUInt(pm.OwnerBlock) and 2 = 0 then
        Result := FMediumMemManager.ReallocMem(aMemory, aSize)
      else
        Result := FLargeMemManager.ReallocMemWithHeader(aMemory, aSize)
    end
    else
    //small mem
    begin
      ot := pm.OwnerBlock.OwnerManager;

      if ot = @FSmallMemManager then
        Result := FSmallMemManager.ReallocMem(aMemory, aSize)
      else
        Result := ReallocMemOfOtherThread(aMemory, aSize);
    end
  end
  else
  begin
    Result := nil;
    Error(reInvalidPtr);  //double free?
  end;

  {$IFDEF SCALEMM_DEBUG}
  CheckMem(nil);
  {$ENDIF}
end;

function TThreadMemManager.ReallocMemOfOtherThread(aMemory: Pointer;
  aSize: NativeUInt): Pointer;
var
  pm: PBaseMemHeader;
begin
  Result := Self.GetMem(aSize);

  pm := PBaseMemHeader(NativeUInt(aMemory) - SizeOf(TBaseMemHeader));
  if aSize > pm.Size then
    Move(aMemory^, Result^, pm.Size)  // copy (use smaller old size)
  else
    Move(aMemory^, Result^, aSize);   // copy (use smaller new size)

  Self.FreeMemOfOtherThread(pm);
end;

procedure TThreadMemManager.ReleaseAllFreeMem;
begin
  ProcessFreedMemFromOtherThreads(False);

  FSmallMemManager.ReleaseAllFreeMem;
  FMediumMemManager.ReleaseAllFreeMem;
end;

procedure TThreadMemManager.Reset;
begin
  FThreadId := 0;
  FThreadTerminated := True;
  //FOtherThreadFreedMemory := nil;
  FNextFreeThreadManager := nil;

  FSmallMemManager.Reset;
  FMediumMemManager.Reset;
end;

function TThreadMemManager.TryLock: boolean;
var
  iCurrentThreadId: NativeUInt;
begin
  iCurrentThreadId := GetCurrentThreadId;
  if (FOtherThreadFreeLock = iCurrentThreadId) and
     (FOtherThreadFreeLockRecursion > 0) then
  begin
    Assert( CAS32(iCurrentThreadId, iCurrentThreadId, @FOtherThreadFreeLock) );
    inc(FOtherThreadFreeLockRecursion);
    Result := True;
    Exit;
  end;

  //LOCK: no threads may be removed/freed now
  Result := CAS32(0, iCurrentThreadId, @FOtherThreadFreeLock);
  if Result then
    inc(FOtherThreadFreeLockRecursion);
  //Result := CAS32(0, 1, @FOtherThreadFreeLock);
end;

procedure TThreadMemManager.UnLock;
begin
  //if not CAS32(1, 0, @FOtherThreadFreeLock) then
  //  Assert(False);
  //FOtherThreadFreeLock := False;

  dec(FOtherThreadFreeLockRecursion);
  if FOtherThreadFreeLockRecursion = 0 then
    FOtherThreadFreeLock := 0;
end;

procedure TThreadMemManager.Lock;
var
  iCurrentThreadId: NativeUInt;
begin
  iCurrentThreadId := GetCurrentThreadId;
  if (FOtherThreadFreeLock = iCurrentThreadId) and
     (FOtherThreadFreeLockRecursion > 0) then
  begin
    Assert( CAS32(iCurrentThreadId, iCurrentThreadId, @FOtherThreadFreeLock) );
    inc(FOtherThreadFreeLockRecursion);
    Exit;
  end;

  //LOCK: no threads may be removed/freed now
  while not CAS32(0, iCurrentThreadId, @FOtherThreadFreeLock) do
  begin
    //small wait: try to swith to other pending thread (if any) else direct continue
    if not SwitchToThread then
      sleep(0);
    //try again
    if CAS32(0, iCurrentThreadId, @FOtherThreadFreeLock) then
      Break;
    //wait some longer: force swith to any other thread
    sleep(1);
  end;

  inc(FOtherThreadFreeLockRecursion);

  {
  //unlock
  repeat
    if CAS32(0, 1, @FOtherThreadFreeLock) then
      Break;
    //small wait: try to swith to other pending thread (if any) else direct continue
    if not SwitchToThread then
      sleep(0);

    //try again
    if CAS32(0, 1, @FOtherThreadFreeLock) then
      Break;
    //wait some longer: force swith to any other thread
    sleep(1);
  until False;
  }
end;

procedure TThreadMemManager.FreeMemOfOtherThread(aMemory: PBaseMemHeader);
var
  p: Pointer;
  pm: PMediumHeader;
begin
  //large mem can be direct freed
  if NativeUInt(aMemory.OwnerBlock) and 2 <> 0 then
  //if aMemory.OwnerBlock.OwnerThread.SizeType = stLarge then
  begin
    //convert to "client" pointer again to be able to use the normal functions
    p  := Pointer(NativeUInt(aMemory) + SizeOf(TBaseMemHeader));
    FLargeMemManager.FreeMemWithHeader(p);
    Exit;
  end
  //medium mem
  else if NativeUInt(aMemory.OwnerBlock) and 3 <> 0 then
  begin
    pm := PMediumHeader( NativeUInt(aMemory) + SizeOf(TBaseMemHeader) - SizeOf(TMediumHeader));
    pm.ThreadFree;
  end
  //small mem
  else
    PSmallMemHeader(aMemory).OwnerBlock.ThreadFreeMem(PSmallMemHeader(aMemory));
end;

procedure TThreadMemManager.CheckMem(aMemory: Pointer);
var
  pm: PBaseMemHeader;
  ot: PBaseSizeManager;
  tm: PThreadMemManager;
begin
  if aMemory = nil then
  begin
    FSmallMemManager.CheckAllMem;
    FMediumMemManager.CheckMem(nil);
    Exit;
  end;

  Assert(aMemory <> nil);
  pm := PBaseMemHeader(NativeUInt(aMemory) - SizeOf(TBaseMemHeader));
  Assert(pm.OwnerBlock <> nil);  

  //medium or large mem?
  if NativeUInt(pm.OwnerBlock) and 3 <> 0 then
  begin
    //other thread?
    tm := PThreadMemManager( NativeUInt(pm.OwnerBlock) and -4);
    if tm <> @Self then
      Exit;  //cannot check mem of other thread!
    Assert(tm <> nil);

    //large or medium?
    if NativeUInt(pm.OwnerBlock) and 2 = 0 then
      tm.FMediumMemManager.CheckMem(aMemory)
    else
      tm.FLargeMemManager.CheckMem(aMemory);
  end
  else
  //small mem
  begin
    ot := pm.OwnerBlock.OwnerManager;
    PThreadMemManager(ot.OwnerThread).FSmallMemManager.CheckMem(aMemory);
  end;
end;

procedure TThreadMemManager.DumpToFile(aFile: THandle; aTotalStats,
  aSingleStats: PThreadMemManagerStats);
begin
  FSmallMemManager.DumpToFile(aFile, aTotalStats, aSingleStats);
  FMediumMemManager.DumpToFile(aFile, aTotalStats, aSingleStats);
  FLargeMemManager.DumpToFile(aFile, aTotalStats, aSingleStats);
end;

function TThreadMemManager.FreeMem(aMemory: Pointer): NativeInt;
var
  pm: PBaseMemHeader;
  ot: PBaseSizeManager;
  pt: PThreadMemManager;
begin
  //AV when doing realloc or free on nil pointer, thanks to qiu.songlin
  if aMemory = nil then
  begin
    Result := 0;
    Exit;
  end;

  if FOtherThreadFreedMemory <> nil then
    ProcessFreedMemFromOtherThreads(True);

  pm := PBaseMemHeader(NativeUInt(aMemory) - SizeOf(TBaseMemHeader));
  //check double free
  if (pm.Size and 1 <> 0) then
    Error(reInvalidPtr);

  //medium or large mem?
  if NativeUInt(pm.OwnerBlock) and 3 <> 0 then
  begin
    pt := PThreadMemManager( NativeUInt(pm.OwnerBlock) and -4);
    if pt <> @Self then
    //other thread?
    begin
      FreeMemOfOtherThread(pm);
      Result := 0;
      Exit;
    end;

    //large or medium?
    if NativeUInt(pm.OwnerBlock) and 2 = 0 then
      Result := FMediumMemManager.FreeMem(aMemory)
    else
      Result := FLargeMemManager.FreeMemWithHeader(aMemory)
  end
  else
  //small mem
  begin
    ot := pm.OwnerBlock.OwnerManager;

    if ot = @FSmallMemManager then
      Result := FSmallMemManager.FreeMem(aMemory)
    else
    begin
      FreeMemOfOtherThread(pm);
      Result := 0;
    end;
  end;

  {$IFDEF SCALEMM_DEBUG}
  CheckMem(nil);
  {$ENDIF}
end;

function TThreadMemManager.FreeMemFromOtherThread(
  aMemory: PBaseMemHeader): NativeInt;
var
  ot: PBaseSizeManager;
  op: PThreadMemManager;
  p:  Pointer;
begin
  //check double free
  if (aMemory.Size and 1 <> 0) then
    Error(reInvalidPtr);

  //convert to "client" pointer again to be able to use the normal functions
  p  := Pointer(NativeUInt(aMemory) + SizeOf(TBaseMemHeader));

  //large or medium?
  if NativeUInt(aMemory.OwnerBlock) and 3 <> 0 then
  begin
    op := PThreadMemManager( NativeUInt(aMemory.OwnerBlock) and -4);
    //check owner (can be changed in the meantime!)
    if op <> @Self then
    begin
      FreeMemOfOtherThread(aMemory);
      Result := 0;
      Exit;
    end;

    //large or medium?
    if NativeUInt(aMemory.OwnerBlock) and 2 = 0 then
      Result := FMediumMemManager.FreeMem(p)
    else
      Result := FLargeMemManager.FreeMemWithHeader(p)
  end
  else
  begin
    ot := aMemory.OwnerBlock.OwnerManager;
    //check owner (can be changed in the meantime!)
    if ot = @FSmallMemManager then
      Result := FSmallMemManager.FreeMem(p)
    else
    begin
      FreeMemOfOtherThread(aMemory);
      Result := 0;
    end;
  end;

  {$ifdef SCALEMM_DEBUG}
  if not Self.FThreadTerminated then
    Self.CheckMem(nil);
  {$ENDIF}
end;

{
procedure TThreadMemManager.AddFreeMemToOwnerThread(aFirstMem,
  aLastMem: PBaseFreeMemHeader);
begin
  //LOCK
  Lock;

  //put new mem to front of linked list
  aLastMem.NextThreadFree := FOtherThreadFreedMemory;
  FOtherThreadFreedMemory := aFirstMem;

  //UNLOCK
  Unlock;
end;
}

function TThreadMemManager.GetMem(aSize: NativeInt): Pointer;
begin
  if FOtherThreadFreedMemory <> nil then
    ProcessFreedMemFromOtherThreads(True);

  if aSize <= C_MAX_SMALLMEM_SIZE then   //-1 till 2048
  begin
    if aSize > 0 then
      Result := FSmallMemManager.GetMem(aSize)
    else
    begin
      Result := nil;
      Exit;
    end
  end
  else if aSize <= C_MAX_MEDIUMMEM_SIZE - SizeOf(TMediumHeader) then  //till 1Mb
    Result := FMediumMemManager.GetMem(aSize)
  else
  begin
    Result := FLargeMemManager.GetMemWithHeader(aSize);
  end;

  Assert( NativeUInt(Result) AND 3 = 0);
  {$IFDEF Align8Bytes}
  Assert( NativeUInt(Result) AND 7 = 0);
  {$ENDIF}
  {$IFDEF Align16Bytes}
  Assert( NativeUInt(Result) AND 15 = 0);
  {$ENDIF}

  {$IFDEF SCALEMM_DEBUG}
  CheckMem(nil);
  {$ENDIF}
end;

procedure TThreadMemManager.Init;
begin
  FThreadId := GetCurrentThreadId;

  FSmallMemManager.Init;
  FSmallMemManager.OwnerThread  := @Self;

  FMediumMemManager.Init;
  FMediumMemManager.OwnerThread := @Self;

  FLargeMemManager.Init;
  FLargeMemManager.OwnerThread  := @Self;

  {$IFDEF SCALEMM_DEBUG}
  CheckMem(nil);
  {$ENDIF}
end;

function TThreadMemManager.IsMemoryFromOtherThreadsPresent: Boolean;
begin
  Result := (FOtherThreadFreedMemory <> nil) or
            FSmallMemManager.IsMemoryFromOtherThreadsPresent;
end;

{$if CompilerVersion >= 23}  //Delphi XE2
function Scale_ReallocMem(aMemory: Pointer; aSize: NativeInt): Pointer;
{$else}
function Scale_ReallocMem(aMemory: Pointer; aSize: Integer): Pointer;
{$ifend}
var
  pm: PBaseMemHeader;
  iSize: NativeUInt;
begin
  // ReAlloc can be misued as GetMem or FreeMem (documented in delphi help) so check what the user wants
  // Normal realloc of exisiting data?
  if (aMemory <> nil) and (aSize > 0) then
  begin
    //general resize: if size within 1/4 we do nothing (also possible in other thread!)
    //iSize := NativeUInt(Pointer(NativeUInt(aMemory) - SizeOf(TBaseMemHeader))^);
    pm    := PBaseMemHeader(NativeUInt(aMemory) - SizeOf(TBaseMemHeader));
    iSize := pm.Size;

    //downsize...
    if (NativeUInt(aSize) <= iSize) then
    begin
      Result := aMemory;
      if iSize <= C_MAX_SMALLMEM_SIZE then  //small mem?
      begin
        //within 1/4?
        if (NativeUInt(aSize) + 32 > iSize shr 2) then
          Exit;
      end
      else
      begin
        //medium + large mem has included their header size in the size too
        if iSize <= C_MAX_MEDIUMMEM_SIZE then  //medium mem?
        begin
          Assert( NativeUInt(PBaseMemHeader(NativeUInt(aMemory) - SizeOf(TBaseMemHeader)).OwnerBlock) and 2 = 0 ); //must be marked as medium!
          //within 1/2?
          if (NativeUInt(aSize) + SizeOf(TMediumHeader) <= iSize) then
          begin
             if (NativeUInt(aSize) > iSize shr 1) then
               Exit
          end
          else
          begin
            Result := GetThreadMemManager.ReallocMem(aMemory, aSize + (aSize shr 3));  //add extra size (12,5%)
            Exit;
          end;
        end
        else                                  //large mem
        begin
          Assert( NativeUInt(PBaseMemHeader(NativeUInt(aMemory) - SizeOf(TBaseMemHeader)).OwnerBlock) and 2 <> 0); //must marked as large!
          //within 1/2?
          if (NativeUInt(aSize) + SizeOf(TLargeHeader) {+ SizeOf(TLargeBlockMemory)} <= iSize) then
          begin
            if (NativeUInt(aSize) > iSize shr 1) then
              Exit
          end
          else
          begin
            Result := GetThreadMemManager.ReallocMem(aMemory, aSize + (aSize shr 4));  //add extra size (1/16, 6,25%) for large mem
            Exit;
          end;
        end;
      end;

      //too much downsize: realloc anyway
      Result := GetThreadMemManager.GetMem(aSize);
      Move(aMemory^, Result^, aSize); // copy (use smaller new size)
      Scale_FreeMem(aMemory);  //free mem (possible from other thread!)
      Exit;
    end;

    //normal realloc
    if iSize <= C_MAX_MEDIUMMEM_SIZE then  //small or medium mem?
      Result := GetThreadMemManager.ReallocMem(aMemory, aSize + (aSize shr 2) )  //add extra size (1/4, 25%)
    else
      Result := GetThreadMemManager.ReallocMem(aMemory, aSize + (aSize shr 4) )  //add extra size (1/16, 6,25%) for large mem
  end
  else
  begin
    if (aMemory = nil) and (aSize > 0) then
      // GetMem disguised as ReAlloc
      Result := Scale_GetMem(aSize)
    else
    begin
      // FreeMem disguised as ReAlloc
      Result := nil;
      Scale_FreeMem(aMemory);
    end;
  end;
end;

{$if CompilerVersion >= 23}  //Delphi XE2
function Scale_GetMem(aSize: NativeInt): Pointer;
{$else}
function Scale_GetMem(aSize: Integer): Pointer;
{$ifend}
{$IFDEF HASINLINE}
begin
  Result := GetThreadMemManager.GetMem(aSize);
end;
{$ELSE}
  {$IFDEF PURE_PASCAL}
  begin
    Result := GetThreadMemManager.GetMem(aSize);
  end;
  {$ELSE}
  asm
    {$IFDEF INLINEGOWN}
    mov edx,eax
    mov eax,GOwnTlsOffset
    mov ecx,fs:[$00000018]
    mov eax,[ecx+eax]      // fixed offset, calculated only once
    or eax,eax
    jnz TThreadMemManager.GetMem
    push edx
    call CreateMemoryManager
    pop edx
    jmp TThreadMemManager.GetMem
    {$ELSE}
    push eax
    call GetThreadMemManager
    pop edx
    jmp TThreadMemManager.GetMem
    {$endif}
  end;
  {$ENDIF}
{$ENDIF}

{$if CompilerVersion >= 23}  //Delphi XE2
function Scale_AllocMem(aSize: NativeInt): Pointer;
{$else}
function Scale_AllocMem(aSize: Cardinal): Pointer;
{$ifend}
begin
  Result := Scale_GetMem(aSize);
  fillchar(Result^, aSize, 0); // AllocMem() = GetMem()+ZeroMemory()
end;

function Scale_FreeMem(aMemory: Pointer): Integer;
{$IFDEF HASINLINE}
begin
  Result := GetThreadMemManager.FreeMem(aMemory);
end;
{$ELSE}
  {$IFDEF PURE_PASCAL}
  begin
    Result := GetThreadMemManager.FreeMem(aMemory);
  end;
  {$ELSE}
  asm
    {$IFDEF INLINEGOWN}
    mov edx,eax
    mov eax,GOwnTlsOffset
    mov ecx,fs:[$00000018]
    mov eax,[ecx+eax]      // fixed offset, calculated only once
    or eax,eax
    jnz TThreadMemManager.FreeMem
    push edx
    call CreateMemoryManager
    pop edx
    jmp TThreadMemManager.FreeMem
    {$ELSE}
    push eax
    call GetThreadMemManager
    pop edx
    jmp TThreadMemManager.FreeMem
    {$endif}
  end;
  {$ENDIF}
{$ENDIF}

procedure Scale_CheckMem(aMemory: Pointer);
begin
  GetThreadMemManager.CheckMem(aMemory);
end;

{$ifdef USEMEMMANAGEREX}
function Scale_RegisterMemoryLeak(P: Pointer): Boolean;
begin
  { TODO : implement memory leak checking }
//  Result := OldMM.RegisterExpectedMemoryLeak(p);
  Result := True;
end;

function Scale_UnregisterMemoryLeak(P: Pointer): Boolean;
begin
//  Result := OldMM.UnregisterExpectedMemoryLeak(p);
  Result := True;
end;
{$endif}

type
  TEndThread = procedure(ExitCode: Integer);
  PEndThread = ^TEndThread;
var
//  OldEndThread: TEndThread;
  NewEndThreadProc: PEndThread;

procedure NewEndThread(ExitCode: Integer); //register; // ensure that calling convension matches EndThread
begin
  // free all thread mem
  GlobalManager.FreeThreadManager( GetThreadMemManager );
  // OldEndThread(ExitCode);  todo: make trampoline with original begin etc
  // code of original EndThread;
  ExitThread(ExitCode);
end;

type
  PJump = ^TJump;
  TJump = packed record
    OpCode  : Byte;
    Distance: Integer;
  end;

procedure FastcodeAddressPatch(const ASource, ADestination: Pointer);
const
  Size: NativeInt = SizeOf(TJump);
var
  NewJump: PJump;
  OldProtect: Cardinal;
begin
  if VirtualProtect(ASource, Size, PAGE_EXECUTE_READWRITE, OldProtect) then
  begin
    NewJump := PJump(ASource);
    NewJump.OpCode   := $E9;  // jmp <Displacement>        jmp -$00001234
    NewJump.Distance := NativeInt(ADestination) - NativeInt(ASource) - Size;

    FlushInstructionCache(GetCurrentProcess, ASource, SizeOf(TJump));
    VirtualProtect(ASource, Size, OldProtect, OldProtect);
  end;
end;

// redirect calls to System.EndThread to NewEndThread
procedure PatchThread;
begin
  FastcodeAddressPatch(@EndThread, NewEndThreadProc);
end;

const
{$ifdef USEMEMMANAGEREX}
  ScaleMM_Ex: TMemoryManagerEx = (
    GetMem    : Scale_GetMem;
    FreeMem   : Scale_FreeMem;
    ReallocMem: Scale_ReallocMem;
    AllocMem  : Scale_AllocMem;
    RegisterExpectedMemoryLeak  : Scale_RegisterMemoryLeak;
    UnregisterExpectedMemoryLeak: Scale_UnregisterMemoryLeak );
{$else}
  ScaleMM_Ex: TMemoryManager = (
    GetMem    : Scale_GetMem;
    FreeMem   : Scale_FreeMem;
    ReallocMem: Scale_ReallocMem );
{$endif}

var
{$ifdef USEMEMMANAGEREX}
  NewMM: TMemoryManagerEx;
  OldMM: TMemoryManagerEx;
{$else}
  NewMM: TMemoryManager;
  OldMM: TMemoryManager;
{$endif}
  ScaleMMIsInstalled  : Boolean = False;
  {Is the MM in place a shared memory manager?}
  IsMemoryManagerOwner: Boolean = True;

{$ifdef MMSharingEnabled}
//code below is copied from FastMM4
const
  {Hexadecimal characters}
  C_HexTable: array[0..15] of AnsiChar = ('0', '1', '2', '3', '4', '5', '6', '7',
    '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
var
  {A string uniquely identifying the current process (for sharing the memory
   manager between DLLs and the main application)}
  MappingObjectName: array[0..26] of AnsiChar = ('L', 'o', 'c', 'a', 'l', '\',
    'S', 'c', 'a', 'l', 'e', 'M', 'M', '_', 'P', 'I', 'D', '_', '?', '?', '?', '?',
    '?', '?', '?', '?', #0);
  {The handle of the memory mapped file}
  MappingObjectHandle: Cardinal;
type
  TSharedRecord = record
    {$ifdef USEMEMMANAGEREX}
    SharedMM: PMemoryManagerEx;
    {$else}
    SharedMM: PMemoryManager;
    {$endif}
    NewEndThread: PEndThread;
  end;
  PSharedRecord = ^TSharedRecord;
var
  SharedRecord: TSharedRecord;

function TryUseExistingSharedManager: Boolean;
var
  i, LCurrentProcessID: Cardinal;
  LPMapAddress: PPointer;
  LChar: AnsiChar;
begin
  {Build a string identifying the current process}
  LCurrentProcessID := GetCurrentProcessId;
  for i := 0 to 7 do
  begin
    LChar := C_HexTable[((LCurrentProcessID shr (i * 4)) and $F)];
    MappingObjectName[(High(MappingObjectName) - 1) - i] := LChar;
  end;
  MappingObjectHandle := OpenFileMappingA(FILE_MAP_READ, False, MappingObjectName);
  {Is no MM being shared?}
  if MappingObjectHandle = 0 then
  begin
    {Share the MM with other DLLs? - if this DLL is unloaded, then
     dependent DLLs will cause a crash.}
    if not IsLibrary then
    begin
      {Create the memory mapped file}
      MappingObjectHandle := CreateFileMappingA(INVALID_HANDLE_VALUE, nil, PAGE_READWRITE, 0, 4,
                                                MappingObjectName);
      {Map a view of the memory}
      LPMapAddress  := MapViewOfFile(MappingObjectHandle, FILE_MAP_WRITE, 0, 0, 0);

      SharedRecord.SharedMM     := @NewMM;
      SharedRecord.NewEndThread := NewEndThreadProc;
      {Set a pointer to the new memory manager}
      LPMapAddress^ := @SharedRecord;
      {Unmap the file}
      UnmapViewOfFile(LPMapAddress);
    end;

    {Owns the memory manager}
    IsMemoryManagerOwner := True;
  end
  else
  begin
    {Map a view of the memory}
    LPMapAddress := MapViewOfFile(MappingObjectHandle, FILE_MAP_READ, 0, 0, 0);
    {get shared data}
    SharedRecord     := PSharedRecord(LPMapAddress^)^;
    NewEndThreadProc := SharedRecord.NewEndThread;
    {Set the new memory manager}
    NewMM            := SharedRecord.SharedMM^;

    {Unmap the file}
    UnmapViewOfFile(LPMapAddress);

    {Close the file mapping handle}
    CloseHandle(MappingObjectHandle);
    MappingObjectHandle  := 0;

    {The memory manager is not owned by this module}
    IsMemoryManagerOwner := False;
  end;

  Result := IsMemoryManagerOwner;
end;
{$endif}

{$if CompilerVersion >= 23}
// issue 6: Delphi XE2 has annoying bug when using SetLocaleOverride -> AV in finalization
// of System.pas due to freemem(PreferredLanguagesOverride)
// So in case of Delphi XE2 (and higher, till it's fixed) we use a different approach (thanks to Maxx xxaM)
var
  _PrevExitProcessProc: procedure;
procedure ScaleMMExitProcessProc;
begin
  //first call previous attached procedure
  if Assigned(_PrevExitProcessProc) then
    _PrevExitProcessProc();
  ScaleMMUninstall;
end;
{$ifend}

procedure ScaleMMInstall;
begin
  if ScaleMMIsInstalled then Exit;

  if IsMemoryManagerSet then
    Error(reAssertionFailed);  //ScaleMM2 is NOT the FIRST unit in dpr!?
  {$WARN SYMBOL_DEPRECATED OFF}
  {$WARN SYMBOL_PLATFORM OFF}
  //todo: GetMemoryManagerState(state);
  if GetHeapStatus.TotalAllocated <> 0 then
    Error(reAssertionFailed);  //Memory has been already been allocated with the RTL MM
  {$WARN SYMBOL_PLATFORM ON}
  {$WARN SYMBOL_DEPRECATED ON}

  NewMM := ScaleMM_Ex;
  NewEndThreadProc := @NewEndThread;
  {$ifdef MMSharingEnabled}
  TryUseExistingSharedManager;
  {$endif}

  // Hook memory Manager
  GetMemoryManager(OldMM);
  if @OldMM <> @NewMM then
    SetMemoryManager(NewMM);
  ScaleMMIsInstalled := True;

  if IsMemoryManagerOwner then
  begin
    {$IFnDEF PURE_PASCAL}
    // get TLS slot
    GOwnTlsIndex  := TlsAlloc;
    // write fixed offset to TLS slot (instead calc via GOwnTlsIndex)
    _FixedOffset;
    {$ENDIF}

    // init main thread manager
    GlobalManager.Init;
  end;
  // we need to patch System.EndThread to properly mark memory to be freed
  // note: must also done in dll (again)?
  PatchThread;

  {$if CompilerVersion >= 23}
  // issue 6: Delphi XE2 has annoying bug when using SetLocaleOverride -> AV in finalization of System.pas due to freemem(PreferredLanguagesOverride)
  // So in case of Delphi XE2 (and higher, till it's fixed) we use a different approach (thanks to Maxx xxaM)
  _PrevExitProcessProc := ExitProcessProc;
  ExitProcessProc      := ScaleMMExitProcessProc;
  {$ifend}
end;

procedure ScaleMMUninstall;
begin
  if not ScaleMMIsInstalled then Exit;

  {Is this the owner of the shared MM window?}
  if IsMemoryManagerOwner then
  begin
    {$ifdef MMSharingEnabled}
    {Destroy the memory mapped file handle}
    if MappingObjectHandle <> 0 then
    begin
      CloseHandle(MappingObjectHandle);
      MappingObjectHandle := 0;
    end;
    {$endif}

    { TODO : check for memory leaks }
    //GlobalManager.FreeAllMemory;
  end;

  SetMemoryManager(OldMM);
  {Memory manager has been uninstalled}
  ScaleMMIsInstalled := False;
end;

initialization
  ScaleMMInstall;

  {$IFDEF Align8Bytes}
    {$IF (SizeOf(TThreadMemManager) AND 7 <> 0) }
        {$MESSAGE ERROR 'not aligned'}
    {$IFEND}
  {$ENDIF}

  {$IFDEF Align16Bytes}
    {$IF (SizeOf(TThreadMemManager) AND 15 <> 0) }
        {$MESSAGE ERROR 'not aligned'}
    {$IFEND}
  {$ENDIF}

finalization
  {$if CompilerVersion < 23}
  // issue 6: Delphi XE2 has annoying bug when using SetLocaleOverride -> AV in finalization of System.pas due to freemem(PreferredLanguagesOverride)
  // So in case of Delphi XE and lower, we use the normal method
  ScaleMMUninstall;
  {$ifend}

end.
