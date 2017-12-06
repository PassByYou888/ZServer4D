program FastcodeMMChallenge_JemallocFFMM;

{$I FASTCODE_MM.INC}

uses
  JemallocFFMM,

  BenchmarkUtilities in 'BenchmarkUtilities.pas',
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
  MMvalidation in 'MMvalidation.pas';
//  _uAsmProfDllLoader in '..\..\asmprofiler\DllVersion\_uAsmProfDllLoader.pas';
{$R *.res}

begin
//  if LoadProfilerDll then ShowProfileForm;

  Application.Initialize;
  Application.CreateForm(TfBenchmark, fBenchmark);
  Application.Run;
end.

