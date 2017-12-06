{****************************************************************************************

  SAPMM v1.01 /17.06.2013/

  SapMM installation unit
  
  Include this unit as the first unit of your project to activate SapMM

****************************************************************************************}

unit SAPInstall;

interface

{$INCLUDE SAPOptions.inc}

uses
  Windows, SAPDefs;

implementation

uses
  SAPThreadMM,
  SAPSmallBlocks,
  SAPNormalBlocks,
  SAPSystem,
  SAPReportMemoryLeaks,
  SAPUtils
//, SAPDebugUtils
;

var
  OldMM: TMemoryManagerEx;
  SAPIsActive: Boolean = False;

procedure InterlockedIncrement(var Value: LongWord);
asm
  lock inc [Value]
end;

function SAPGetMem(Size: {$IF CompilerVersion >= 23} NativeInt {$ELSE} Integer {$IFEND}): Pointer;
var
  mm: PSAPThreadMM;
begin
  // Next asm block is an inline for: tmm := GetThreadMM;
  asm
    mov eax,TLSOffset
    mov ecx,fs:[$00000018]
    mov eax,[ecx+eax]
    mov mm, eax
  end;

  if mm = nil then
  begin
    mm := CreateThreadMM;
  end;

  Result := ThreadGetMem(mm, Size);

(*
  //!!!
  stats[stats_count].op := 'G';
  stats[stats_count].size := size;
  stats[stats_count].adr := Result;
  Inc(stats_count);
*)

end;

function SAPAllocMem(Size: {$IF CompilerVersion >= 23} NativeInt {$ELSE} Cardinal {$IFEND}): Pointer;
var
  mm: PSAPThreadMM;
begin
  // Next asm block is an inline for: tmm := GetThreadMM;
  asm
    mov eax,TLSOffset
    mov ecx,fs:[$00000018]
    mov eax,[ecx+eax]
    mov mm, eax
  end;

  if mm = nil then
  begin
    mm := CreateThreadMM;
  end;

  Result := ThreadGetMem(mm, Size);

  if (Result <> nil) then FillChar(Result^, size, 0);

end;

// Get MM for allocated block
function GetBlockMM(p: Pointer): Pointer; inline;
var
  s: PSAPSmallAllocatedBlock;
  h: PSAPSmallBlocksHeader;
  b: PSAPAllocatedBlock;
begin
  s := Pointer(LongWord(p) - SizeOf(TSAPSmallAllocatedBlock));

  if s.magic <> cMagic then
  begin
    Result := nil;
  end
  else if sap_small_block in s.tags then
  begin
    h := Pointer(LongWord(s) - s.ofs);

    {$IFDEF SAP_CHECKMAGIC}
    if h.header_magic <> cSmallBlockHeaderMagic then
    begin
      Result := nil;
      Exit;
    end;
    {$ENDIF SAP_CHECKMAGIC}

    Result := h.block_mm;
  end
  else begin
    b := Pointer(LongWord(p) - SizeOf(TSAPAllocatedBlock));

    {$IFDEF SAP_CHECKMAGIC}
    if b.block_magic <> cNormalBlockMagic then
    begin
      Result := nil;
      Exit;
    end;
    {$ENDIF SAP_CHECKMAGIC}

    Result := b.block_mm;
  end;
end;

function SAPFreeMem(p: Pointer): Integer;
var
  mm: PSAPThreadMM;  // 面 for allocated memory block
  tmm: PSAPThreadMM; // MM for current thread
begin
  Result := 0;

  if p = nil then 
    Exit;

  mm := GetBlockMM(p);

  if mm = nil then // block was allocated by previous MM (not by SapMM)
  begin
    Result := OldMM.FreeMem(P);
    Inc(sap_no_free_oldMM);
    Exit;
  end;

  // Next asm block is an inline for: tmm := GetThreadMM;
  asm
    mov eax,TLSOffset
    mov ecx,fs:[$00000018]
    mov eax,[ecx+eax]
    mov tmm, eax
  end;

  if mm = tmm then // block was allocated in this thread, free immediately
  begin
    Result := ThreadFreeMem(mm, p);
    Exit;
  end;

  // Interthread memory block is not freed in current thread, it must be freed in the thread of MM, which allocated it
  // Instead of freeing the block it is put into allocating thread MM's special list for delayed release
  {$IFDEF SAP_STAT}
  if tmm <> nil then
    InterlockedIncrement(tmm.no_inter_free);
  {$ENDIF SAP_STAT}

  LockForFreeFromOtherThread(mm);
    PSAPBlocksFreedInOtherThreads(p).next := mm.blocks_to_free;
    mm.blocks_to_free := p;
  UnlockForFreeFromOtherThread(mm);
  Result := 0;
end;

//------------------------------------------------
// Realloc

function InterThreadRealloc(tmm, mm: PSAPThreadMM; p: Pointer; size: LongWord): Pointer;
forward;

function SAPReallocMem(p: Pointer; size: {$IF CompilerVersion >= 23} NativeInt {$ELSE} Integer {$IFEND}): Pointer;
var
  mm: PSAPThreadMM;  // 面 for the allocated memory block
  tmm: PSAPThreadMM; // MM for current thread
begin
  if size <= 0 then // deallocate mem
  begin
    Result := nil;
    SAPFreeMem(P);
    Exit;
  end;

  if p = nil then // allocate mem
  begin
    Result := SAPGetMem(size);
    Exit;
  end;

  // Actual reallocate
  mm := GetBlockMM(p);

  if mm = nil then // Block was allocated by previous MM (not by SapMM)
  begin
    Result := OldMM.ReallocMem(p, size);
    Exit;
  end;

  // Next asm block is an inline for: tmm := GetThreadMM;
  asm
    mov eax,TLSOffset
    mov ecx,fs:[$00000018]
    mov eax,[ecx+eax]
    mov tmm, eax
  end;

  if mm = tmm then // Realloc inside one thread MM
  begin
    ThreadReallocMem(mm, p, size);
    Result := p;
    Exit;
  end;

  // Interthread realloc
  {$IFDEF SAP_STAT}
  InterlockedIncrement(tmm.no_inter_realloc);
  {$ENDIF SAP_STAT}
  Result := InterThreadRealloc(tmm, mm, p, size);
end;

// tmm - 面 for the thread
// mm - 面, which allocated memory block
function InterThreadRealloc(tmm, mm: PSAPThreadMM; p: Pointer; size: LongWord): Pointer;
var
  new_p: Pointer;
  block_size: LongWord;
  move_size: LongWord;
begin
  {$IFDEF SAP_STAT}
  Inc(tmm.no_realloc);
  {$ENDIF SAP_STAT}

  block_size := GetAllocatedSize(p);

  new_p := ThreadGetMem(tmm, size);

  if new_p <> nil then
  begin
    if size > block_size then
      move_size := block_size
    else
      move_size := size;

    {$IFDEF SAP_STAT}
    Inc(tmm.no_realloc_copy_bytes, move_size);
    {$ENDIF SAP_STAT}

    // System.Move(p^, new_p^, move_size);
    SAPMoveMemory(new_p, p, move_size);
  end;

  // Interthread memory block is not freed in current thread, it must be freed in the thread of MM, which allocated it
  // Instead of freeing the block it is put into allocating thread MM's special list for delayed release
  LockForFreeFromOtherThread(mm);
    PSAPBlocksFreedInOtherThreads(p).next := mm.blocks_to_free;
    mm.blocks_to_free := p;
  UnlockForFreeFromOtherThread(mm);

  Result := new_p;
end;

function SAPRegisterExpectedMemoryLeak(P: Pointer): Boolean;
begin
  Result := True;
end;

function SAPUnregisterExpectedMemoryLeak(P: Pointer): Boolean;
begin
  Result := True;
end;

const
  SAPManager: TMemoryManagerEx = (
    GetMem: SAPGetMem;
    FreeMem: SAPFreeMem;
    ReallocMem: SAPReallocMem;
    AllocMem: SAPAllocMem;
    RegisterExpectedMemoryLeak: SAPRegisterExpectedMemoryLeak;
    UnregisterExpectedMemoryLeak: SAPUnregisterExpectedMemoryLeak);

// hook for thread termination
var
  PreviousEndHook: TSystemThreadEndProc = nil;
  PreviousExitProcessProc: procedure = nil;

procedure ReleaseThreadMM(ExitCode: Integer);
var
  mm: PSAPThreadMM;
begin
  // Call previous "hook"
  if Assigned(PreviousEndHook) then 
    PreviousEndHook(ExitCode);

  // If process is terminated, no need to do anything, 
  // all allocated memory will be released by Windows
  if not SAPIsActive then 
    Exit;

  try
    mm := GetThreadMM;
    if mm = nil then 
      Exit;
    if mm.magic <> cMagicMM then
      Exit;
  except
    Exit;
  end;

  // Free thread 面
  ThreadReleaseMM(mm);
end;

procedure SetThreadEndProc;
begin
  PreviousEndHook := SystemThreadEndProc;
  System.SystemThreadEndProc := @ReleaseThreadMM;
end;

procedure SAPMMInstall;
begin
  GetMemoryManager(OldMM);

  SetThreadEndProc;

  SetMemoryManager(SAPManager);

  SAPIsActive := True;
end;

//--------------------------------------

var
  {$IFDEF SAP_MEMORYLEAKS}
  LogFileName: string;
  {$ENDIF SAP_MEMORYLEAKS}
  LogFile: TextFile;

procedure FileOutput(s: string);
begin
  writeln(LogFile, s);
end;

(*
procedure ShowOpStats;
var
  i: Integer;
  s: string;
begin
  for i := 0 to stats_count - 1 do
  begin
    if stats[i].op = 'G' then
      s := 'get(' + IntToStr(stats[i].Size) + '): ' // + IntToHex(LongWord(stats[i].adr), 8)

//    else if stats[i].op = 'S' then
//      s := 'small(' + IntToStr(stats[i].Size) + '): ' + IntToHex(LongWord(stats[i].adr), 8)

    else if stats[i].op = 'X' then
      s := 'sap_free: ' + IntToHex(LongWord(stats[i].adr), 8)
    else if stats[i].op = '1' then
      s := 'realloc: ' + IntToHex(LongWord(stats[i].adr), 8)
    else if stats[i].op = '2' then
      s := 'realloc(' + IntToStr(stats[i].Size) + ') '
          + ' -> '
          + IntToHex(LongWord(stats[i].adr), 8)
    else if stats[i].op = '0' then
      s := 'get0(' + IntToStr(stats[i].Size) + '): ' + IntToHex(LongWord(stats[i].adr), 8)
//    else
//      s := '?' + stats[i].op + '??(' + IntToStr(stats[i].Size) + '): ' + IntToHex(LongWord(stats[i].adr), 8)
    ;
    FileOutput(IntToStr(i) + '. ' + s);
  end;
end;
*)

{$IFDEF SAP_MEMORYLEAKS}
procedure SaveMemoryLeaksReport(x: PSAPThreadMM);
begin
  LogFileName := 'leaks.log';
  AssignFile(LogFile, LogFileName);

  if FileExists(LogFileName) then
    Erase(LogFile);
  ReWrite(LogFile);

//  SAPDebugUtils.output := FileOutput;
  PrepareMemoryLeaksReport(x, FileOutput);
//  CheckOSBlocks(x, true);
//  ShowOpStats;
  CloseFile(LogFile);
end;
{$ENDIF}

procedure ReportMemoryLeaks;

  function CheckWorkMMs: Boolean;
  var
    x: PSAPThreadMM;
  begin
    x := work_mm;
    while x <> nil do
    begin
      Result := x.no_free < x.no_get;
      if Result then
      begin
        {$IFDEF SAP_MEMORYLEAKS}
        SaveMemoryLeaksReport(x);
        {$ENDIF SAP_MEMORYLEAKS}
        Exit;
      end;
      x := x.next;
    end;
    Result := false;
  end;

var
  leaks: Boolean;
begin
  if not ReportMemoryLeaksOnShutdown then 
    Exit;

  ReportMemoryLeaksOnShutdown := False;

  // Waiting for the threads to terminate
  Sleep(250);

  // SapMM is stopped, from now on memory is allocated via old 面
  SetMemoryManager(OldMM);

  memory_leaks_add := [sap_used_in_leaks_reporting];

  CheckAndReleaseZombiMM;

  LockMMsList;

    leaks := zombi_mm <> nil;
    if leaks then
    begin
      {$IFDEF SAP_MEMORYLEAKS}
      SaveMemoryLeaksReport(zombi_mm);
      {$ENDIF SAP_MEMORYLEAKS}
    end
    else begin
      leaks := CheckWorkMMs;
    end;

  UnlockMMsList;

  if leaks then
    Windows.MessageBox(0,'Memory leak detected: not freed blocks found', 'Memory leaks', 0);
end;

initialization
  SAPMMInstall;

finalization
  SAPIsActive := False;
  ReportMemoryLeaks;
end.