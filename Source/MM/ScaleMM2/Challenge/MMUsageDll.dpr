library MMUsageDll;

{$I FASTCODE_MM.INC}

uses
  {$IFDEF MM_BUCKETMM}
    {Robert Houdart's BucketMM}
    BucketMem,
  {$ENDIF}
  {$IFDEF MM_BUCKETMM_ASM}
    {Robert Houdart's BucketMM}
    BucketMem_Asm,
  {$ENDIF}
  {$IFDEF MM_DKCIA32MM}
  {Dennis Christensen slowcode entry version 0.16}
    DKC_IA32_MM_Unit,
  {$ENDIF}
  {$IFDEF MM_EWCMM}
    {Eric Carman's EWCMM}
    EWCMM,
  {$ENDIF}
  {$IFDEF MM_FASTMM2}
    {Pierre le Riche's Fastcode challenge entry v2.05}
    FastMM,
  {$ENDIF}
  {$IFDEF MM_FASTMM3}
    {Pierre le Riche's Fastcode challenge entry v3.01}
    FastMM3,
  {$ENDIF}
  {$IFDEF MM_FASTMM4}
    {Pierre le Riche's Fastcode challenge entry v4.xx}
    FastMM4,
  {$ENDIF}
  {$IFDEF MM_FASTMM4_16}
    {Pierre le Riche's Fastcode challenge entry v4.xx}
    FastMM4_16,
  {$ENDIF}
  {$IFDEF MM_MULTIMM}
    {Robert Lee's HPMM}
    MultiMM,
  {$ENDIF}
  {$IFDEF MM_NEXUSMM}
    {NexusDB Memory Manager}
    nxReplacementMemoryManager,
  {$ENDIF}
  {$IFDEF MM_PSDMM}
    {PSDMemoryManager v1.0}
    PSDMemoryManager,
  {$ENDIF}
  {$IFDEF MM_QMEMORY}
    {Andrew Driazgov's QMemory}
    QMemory,
  {$ENDIF}
  {$IFDEF MM_RECYCLERMM}
    {Eric Grange's RecyclerMM}
    RecyclerMM,
  {$ENDIF}
  {$IFDEF MM_RTLMM}
    {Borland Delphi RTL Memory Manager}
  {$ENDIF}
  {$IFDEF MM_TOPMM}
    {Ivo Top's TopMM}
    TopMemory,
  {$ENDIF}
  {$IFDEF MM_WINMEM}
    {Mike Lischke's WinMem (Uses the windows heap)}
    WinMem,
  {$ENDIF}

  MMUseMemory in 'MMUseMemory.pas';

{$R *.RES}

exports UseSomeMemory;
exports LeakSomeMemory;

begin
end.
