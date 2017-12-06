program FastcodeMMChallenge_ScaleMM;

{$I FASTCODE_MM.INC}

uses
  {Must be first to measure the initial VM Usage}
  BenchmarkUtilities in 'BenchmarkUtilities.pas',
{$IFDEF MM_BUCKETMM}
  {Robert Houdart's BucketMM}
  BucketMem,
{$ENDIF}
{$IFDEF MM_BUCKETMM_ASM}
  {Robert Houdart's BucketBASMMM}
  BucketMem_ASM,
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
  {Pierre le Riche's FastMM v2.xx}
  FastMM,
{$ENDIF}
{$IFDEF MM_FASTMM3}
  {Pierre le Riche's FastMM v3.xx}
  FastMM3,
{$ENDIF}
{$IFDEF MM_FASTMM4}
  {Pierre le Riche's FastMM v4.27}
  FastMM4,
{$ENDIF}
{$IFDEF MM_FASTMM4_16}
  {Pierre le Riche's FastMM v4.27 with 16 byte alignment}
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
  {Pierre le Riche's PSDMemoryManager v1.xx}
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


{$IFDEF MM_LOCALHEAP}
  {Carsten Zeumer's LocalHeapMM (Uses the windows heap)}
  LocalHeapMM,
{$ENDIF}
{$IFDEF MM_HEAPMM}
  {Vladimir Kladov's HeapMM}
  HeapMM,
{$ENDIF}

{$IFDEF MM_SCALEMM}
  ScaleMM in '..\..\ScaleMM.pas',
{$ENDIF}

  // FastMove,   // uncomment if you want to benchmark with FastMove

  {Other units}
  Forms,
  BenchmarkForm in 'BenchmarkForm.pas' {fBenchmark},
  RenameMMForm in 'RenameMMForm.pas' {fRenameMM},
  FragmentationTestUnit in 'FragmentationTestUnit.pas',
  NexusDBBenchmarkUnit in 'NexusDBBenchmarkUnit.pas',
  ReallocMemBenchmark in 'ReallocMemBenchmark.pas',
  DownsizeTestUnit in 'DownsizeTestUnit.pas',
  ReplayBenchmarkUnit in 'ReplayBenchmarkUnit.pas',
  WildThreadsBenchmarkUnit in 'WildThreadsBenchmarkUnit.pas',
  BlockSizeSpreadBenchmark in 'BlockSizeSpreadBenchmark.pas',
  SmallUpsizeBenchmark in 'SmallUpsizeBenchmark.pas',
  SmallDownsizeBenchmark in 'SmallDownsizeBenchmark.pas',
  RawPerformanceSingleThread in 'RawPerformanceSingleThread.pas',
  RawPerformanceMultiThread in 'RawPerformanceMultiThread.pas',
  GraphsForm in 'GraphsForm.pas' {fGraphs},
  AddressSpaceCreepBenchmark in 'AddressSpaceCreepBenchmark.pas',
  LargeBlockSpreadBenchmark in 'LargeBlockSpreadBenchmark.pas',
  StringThread in 'StringThread.pas',
  StringThreadTestUnit in 'StringThreadTestUnit.pas',
  ArrayUpsizeSingleThread in 'ArrayUpsizeSingleThread.pas',
  SingleFPBenchmark1Unit in 'SingleFPBenchmark1Unit.pas',
  DoubleFPBenchmark1Unit in 'DoubleFPBenchmark1Unit.pas',
  DoubleFPBenchmark2Unit in 'DoubleFPBenchmark2Unit.pas',
  DoubleFPBenchmark3Unit in 'DoubleFPBenchmark3Unit.pas',
  MoveBenchmark1Unit in 'MoveBenchmark1Unit.pas',
  MoveBenchmark2Unit in 'MoveBenchmark2Unit.pas',
  AddressSpaceCreepBenchmarkLarge in 'AddressSpaceCreepBenchmarkLarge.pas',
  LinkedListBenchmark in 'LinkedListBenchmark.pas',
  BenchmarkClassUnit in 'BenchmarkClassUnit.pas',
  MultiThreadedAllocAndFree in 'MultiThreadedAllocAndFree.pas',
  MultiThreadedReallocate in 'MultiThreadedReallocate.pas',
  SingleThreadedAllocAndFree in 'SingleThreadedAllocAndFree.pas',
  SingleThreadedReallocate in 'SingleThreadedReallocate.pas',
  SortIntArrayBenchmark2Unit in 'SortIntArrayBenchmark2Unit.pas',
  SortExtendedArrayBenchmark2Unit in 'SortExtendedArrayBenchmark2Unit.pas',
  SingleThreadedAllocMem in 'SingleThreadedAllocMem.pas',
  MMvalidation in 'MMvalidation.pas',
  GeneralFunctions in 'GeneralFunctions.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfBenchmark, fBenchmark);
  Application.Run;
end.

