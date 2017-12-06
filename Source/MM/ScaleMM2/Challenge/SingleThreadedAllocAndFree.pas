{A benchmark that creates and frees many objects in a multi-threaded
 environment}

unit SingleThreadedAllocAndFree;

interface

uses Windows, BenchmarkClassUnit, Classes, Math;

type

  TSingleThreadAllocateAndFreeBenchmark = class(TFastcodeMMBenchmark)
  public
    procedure RunBenchmark; override;
    class function GetBenchmarkName: string; override;
    class function GetBenchmarkDescription: string; override;
    class function GetCategory: TBenchmarkCategory; override;
    class function GetSpeedWeight: Double; override;
  end;

implementation

{ TSingleThreadAllocateAndFreeBenchmark }

class function TSingleThreadAllocateAndFreeBenchmark.GetBenchmarkDescription: string;
begin
  Result := 'A single-threaded benchmark that allocates and frees memory blocks. The usage of different block sizes approximates real-world usage as seen in various replays. Allocated memory is actually "used", i.e. written to and read.';
end;

class function TSingleThreadAllocateAndFreeBenchmark.GetBenchmarkName: string;
begin
  Result := 'Single-threaded allocate, use and free';
end;

class function TSingleThreadAllocateAndFreeBenchmark.GetCategory: TBenchmarkCategory;
begin
  Result := bmSingleThreadAllocAndFree;
end;

class function TSingleThreadAllocateAndFreeBenchmark.GetSpeedWeight: Double;
begin
  Result := 0.6;
end;

procedure TSingleThreadAllocateAndFreeBenchmark.RunBenchmark;
const
  RepeatCount = 100;
  PointerCount = 75000;
var
  i, j, k: Integer;
  LPointers: array[0..PointerCount - 1] of Pointer;
  LMax, LSize, LSum: Integer;
begin
  inherited;
  {We want predictable results}
  RandSeed := 0;
  {Allocate the initial pointers}
  for i := 0 to PointerCount - 1 do
  begin
    {Rough breakdown: 50% of pointers are <=64 bytes, 95% < 1K, 99% < 4K, rest < 256K}
    if i and 1 <> 0 then
      LMax := 64
    else
      if i and 15 <> 0 then
        LMax := 1024
      else
        if i and 255 <> 0 then
          LMax := 4 * 1024
        else
          LMax := 256 * 1024;
    {Get the size, minimum 1}
    LSize := Random(LMax) + 1;
    {Get the pointer}
    GetMem(LPointers[i], LSize);
  end;
  {Free and allocate in a loop}
  for j := 1 to RepeatCount do
  begin
    {Update usage statistics}
    UpdateUsageStatistics;
    for i := 0 to PointerCount - 1 do
    begin
      {Free the pointer}
      FreeMem(LPointers[i]);
      {Rough breakdown: 50% of pointers are <=64 bytes, 95% < 1K, 99% < 4K, rest < 256K}
      if i and 1 <> 0 then
        LMax := 64
      else
        if i and 15 <> 0 then
          LMax := 1024
        else
          if i and 255 <> 0 then
            LMax := 4 * 1024
          else
            LMax := 256 * 1024;
      {Get the size, minimum 1}
      LSize := Random(LMax) + 1;
      {Get the pointer}
      GetMem(LPointers[i], LSize);
      {Write the memory}
      for k := 0 to (LSize - 1) div 32 do
      begin
        PByte(Integer(LPointers[i]) + k * 32)^ := byte(i);
      end;
      {Read the memory}
      LSum := 0;
      if LSize > 15 then
      begin
        for k := 0 to (LSize - 16) div 32 do
        begin
          Inc(LSum, PShortInt(Integer(LPointers[i]) + k * 32 + 15)^);
        end;
      end;
      {"Use" the sum to suppress the compiler warning}
      if LSum > 0 then;
    end;
  end;
  {Free all the objects}
  for i := 0 to PointerCount - 1 do
    FreeMem(LPointers[i]);
end;

end.
