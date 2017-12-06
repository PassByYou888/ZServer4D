Unit fpc_tcmalloc;

Interface

{.$LINKLIB tcmalloc_minimal}

const
  {$ifdef CPUX86}
    libtcmalloc = 'libtcmalloc_x86.dll';
  {$else .CPUX64}
    libtcmalloc = 'libtcmalloc_x64.dll';
  {$endif}

function tc_malloc(Size: NativeUInt): Pointer; cdecl; external libtcmalloc;
function tc_realloc(P: Pointer; Size: NativeUInt): Pointer; cdecl; external libtcmalloc;
procedure tc_free(P: Pointer); cdecl; external libtcmalloc;

Implementation

type pptruint = ^ptruint;

Function TCGetMem(Size: ptruint): Pointer;
begin
  TCGetMem := tc_malloc(Size + sizeof(ptruint));

  if (TCGetMem <> nil) then
  begin
    pptrint(TCGetMem)^ := Size;
    Inc(TCGetMem, sizeof(ptruint));
  end;
end;

Function TCFreeMem(P: Pointer): ptruint;
begin
  If (P <> nil) then
    Dec(P, sizeof(ptruint));

  tc_free(P);
  TCFreeMem := 0;
end;

Function TCFreeMemSize(P: Pointer; Size: ptruint): ptruint;
begin
  if Size <= 0 then
  begin
    if Size = 0 then
      exit;
    runerror(204);
  end;

  if P <> nil then
  begin
    if Size <> pptruint(P - sizeof(ptruint))^ then
      runerror(204);
  end;

  TCFreeMemSize := TCFreeMem(P);
end;

Function TCAllocMem(Size: ptruint): Pointer;
var
  TotalSize: ptruint;
begin
  TotalSize := Size + sizeof(ptruint);

  TCAllocMem := tc_malloc(TotalSize);

  if TCAllocMem <> nil then
  begin
    FillByte(TCAllocMem, TotalSize, 0);

    pptruint(TCAllocMem)^ := Size;
    Inc(TCAllocMem, sizeof(ptruint));
  end;
end;

Function TCReAllocMem(var P: Pointer; Size: ptruint): Pointer;
begin
  if Size = 0 then
  begin
    if P <> nil then
    begin
      dec(P, sizeof(ptruint));

      tc_free(P);
      P := nil;
    end;
  end
  else
  begin
    Inc(Size, sizeof(ptruint));
    if P = nil then
      P := tc_malloc(Size)
    else
    begin
      Dec(P, sizeof(ptruint));
      P := tc_realloc(P, Size);
    end;

    if P <> nil then
    begin
      pptruint(p)^ := Size - sizeof(ptruint);
      Inc(P, sizeof(ptruint));
    end;
  end;

  TCReallocMem := P;
end;

Function TCMemSize(P: Pointer): ptruint;
begin
  TCMemSize := pptruint(P - sizeof(ptruint))^;
end;

(* TODO! *)
Function TCGetHeapStatus: THeapStatus;
begin
  FillChar(TCGetHeapStatus, sizeof(TCGetHeapStatus), 0);
end;

Function TCGetFPCHeapStatus: TFPCHeapStatus;
begin
  FillChar(TCGetFPCHeapStatus, sizeof(TCGetFPCHeapStatus), 0);
end;

Const TCMemoryManager: TMemoryManager =
  (
    NeedLock: false;
    GetMem: @TCGetMem;
    FreeMem: @TCFreeMem;
    FreeMemSize: @TCFreeMemSize;
    AllocMem: @TCAllocMem;
    ReallocMem: @TCReAllocMem;
    MemSize: @TCMemSize;
    InitThread: Nil;
    DoneThread: Nil;
    RelocateHeap: Nil;
    GetHeapStatus: @TCGetHeapStatus;
    GetFPCHeapStatus: @TCGetFPCHeapStatus
  );

Var PreviousMemoryManager: TMemoryManager;

Initialization
  GetMemoryManager(PreviousMemoryManager);
  SetMemoryManager(TCMemoryManager);
Finalization
  SetMemoryManager(PreviousMemoryManager);
end.

