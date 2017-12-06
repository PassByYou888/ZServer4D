unit BenchmarkUtilities;

interface

{$I FASTCODE_MM.INC}

type
  {Virtual memory state}
  TVMState = record
    {The total VM size allocated or reserved}
    TotalVMAllocated: integer;
    {The number of free space fragments}
    FreeSpaceFragments: integer;
    {The largest free space fragment}
    LargestFreeSpaceFragment: integer;
  end;

function GetCompilerAbbr: string;
function GetCompilerName: string;
{Gets the CPU tick count}
function GetCPUTicks: Int64;
function GetMMName: string;
{Gets the current state of the virtual memory pool}
function GetVMState: TVMState;
{Gets the number of bytes of virtual memory either reserved or committed by this
 process}
function GetAddressSpaceUsed: Cardinal;

var
  {The address space that was in use when the application started}
  InitialAddressSpaceUsed: Cardinal;

implementation

uses
  Windows;

function GetCompilerAbbr: string;
begin
  Result := 'undefined';

  {$IFDEF VER80}Result := 'D1';{$ENDIF}
  {$IFDEF VER90}Result := 'D2';{$ENDIF}
  {$IFDEF VER100}Result := 'D3';{$ENDIF}
  {$IFDEF VER120}Result := 'D4';{$ENDIF}
  {$IFDEF VER130}Result := 'D5';{$ENDIF}
  {$IFDEF VER140}Result := 'D6';{$ENDIF}
  {$IFDEF VER150}Result := 'D7';{$ENDIF}
  {$IFDEF VER160}Result := 'D8';{$ENDIF}
  {$IFDEF VER170}Result := 'D2005';{$ENDIF}
  // thinking ahead :)
  {$IFDEF VER180}Result := 'DeXter';{$ENDIF}
  {$IFDEF VER190}Result := 'Highlander';{$ENDIF}
end;

function GetCompilerName: string;
begin
  Result := 'undefined';

  {$IFDEF VER80}Result := 'Delphi 1';{$ENDIF}
  {$IFDEF VER90}Result := 'Delphi 2';{$ENDIF}
  {$IFDEF VER100}Result := 'Delphi 3';{$ENDIF}
  {$IFDEF VER120}Result := 'Delphi 4';{$ENDIF}
  {$IFDEF VER130}Result := 'Delphi 5';{$ENDIF}
  {$IFDEF VER140}Result := 'Delphi 6';{$ENDIF}
  {$IFDEF VER150}Result := 'Delphi 7';{$ENDIF}
  {$IFDEF VER160}Result := 'Delphi 8';{$ENDIF}
  {$IFDEF VER170}Result := 'Delphi 2005';{$ENDIF}
  // thinking ahead :)
  {$IFDEF VER180}Result := 'DeXter';{$ENDIF}
  {$IFDEF VER190}Result := 'Highlander';{$ENDIF}
end;

function GetCPUTicks: Int64;
asm
  rdtsc;
end;

// Alternate implementation for Delphi5 compatibility
// function GetCPUTicks: Int64;
// asm
//  db $0F; db $31;  //rdtsc
//end;

function GetMMName: string;
begin
  Result := 'undefined';

  {$IFDEF MM_BUCKETMM}Result := 'BUCKETMM';{$ENDIF}
  {$IFDEF MM_BUCKETMM_ASM}Result := 'BUCKETMM_ASM';{$ENDIF}
  {$IFDEF MM_DKCIA32MM}Result := 'DKCIA32MM';{$ENDIF}
  {$IFDEF MM_EWCMM}Result := 'EWCMM';{$ENDIF}
  {$IFDEF MM_FASTMM2}Result := 'FASTMM2';{$ENDIF}
  {$IFDEF MM_FASTMM3}Result := 'FASTMM3';{$ENDIF}
  {$IFDEF MM_FASTMM4}Result := 'FASTMM4';{$ENDIF}
  {$IFDEF MM_FASTMM4_16}Result := 'FASTMM4_16';{$ENDIF}
  {$IFDEF MM_NEXUSMM}Result := 'NEXUSMM';{$ENDIF}
  {$IFDEF MM_QMEMORY}Result := 'QMEMORY';{$ENDIF}
  {$IFDEF MM_RECYCLERMM}Result := 'RECYCLERMM';{$ENDIF}
  {$IFDEF MM_RTLMM}Result := 'RTLMM';{$ENDIF}
  {$IFDEF MM_TOPMM}Result := 'TOPMM';{$ENDIF}
  {$IFDEF MM_WINMEM}Result := 'WINMEM';{$ENDIF}
end;

function GetVMState: TVMState;
var
  LChunkIndex, LFreeBlockCount: integer;
  LMBI: TMemoryBasicInformation;

  procedure AddFreeBlock;
  begin
    if LFreeBlockCount > 0 then
    begin
      Inc(Result.FreeSpaceFragments);
      if LFreeBlockCount > (Result.LargestFreeSpaceFragment shr 16) then
        Result.LargestFreeSpaceFragment := LFreeBlockCount shl 16;
      LFreeBlockCount := 0;
    end;
  end;

begin
  Result.TotalVMAllocated := 0;
  Result.FreeSpaceFragments := 0;
  Result.LargestFreeSpaceFragment := 0;
  LFreeBlockCount := 0;
  for LChunkIndex := 0 to 32767 do
  begin
    {Get the state of each 64K chunk}
    VirtualQuery(Pointer(LChunkIndex shl 16), LMBI, SizeOf(LMBI));
    if LMBI.State = MEM_FREE then
    begin
      Inc(LFreeBlockCount);
    end
    else
    begin
      AddFreeBlock;
      Inc(Result.TotalVMAllocated, 65536);
    end;
  end;
end;

{Gets the number of bytes of virtual memory either reserved or committed by this
 process in K}
function GetAddressSpaceUsed: Cardinal;
var
  LMemoryStatus: TMemoryStatus;
begin
  {Set the structure size}
  LMemoryStatus.dwLength := SizeOf(LMemoryStatus);
  {Get the memory status}
  GlobalMemoryStatus(LMemoryStatus);
  {The result is the total address space less the free address space}
  Result := (LMemoryStatus.dwTotalVirtual - LMemoryStatus.dwAvailVirtual) shr 10;
end;

initialization
  {Get the initial VM Usage}
  InitialAddressSpaceUsed := GetAddressSpaceUsed;

end.
