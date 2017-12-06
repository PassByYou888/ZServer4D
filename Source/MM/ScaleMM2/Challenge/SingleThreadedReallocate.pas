{A single-threaded benchmark that reallocates and uses memory blocks.}

unit SingleThreadedReallocate;

interface

uses Windows, BenchmarkClassUnit, Classes, Math;

type

  TSingleThreadReallocateBenchmark = class(TFastcodeMMBenchmark)
  public
    procedure RunBenchmark; override;
    class function GetBenchmarkName: string; override;
    class function GetBenchmarkDescription: string; override;
    class function GetCategory: TBenchmarkCategory; override;
    class function GetSpeedWeight: Double; override;
  end;

implementation

{ TSingleThreadReallocateBenchmark }

class function TSingleThreadReallocateBenchmark.GetBenchmarkDescription: string;
begin
  Result := 'A single-threaded benchmark that allocates and reallocates memory blocks. The usage of different block sizes approximates real-world usage as seen in various replays. Allocated memory is actually "used", i.e. written to and read.';
end;

class function TSingleThreadReallocateBenchmark.GetBenchmarkName: string;
begin
  Result := 'Single-threaded reallocate and use';
end;

class function TSingleThreadReallocateBenchmark.GetCategory: TBenchmarkCategory;
begin
  Result := bmSingleThreadRealloc;
end;

class function TSingleThreadReallocateBenchmark.GetSpeedWeight: Double;
begin
  Result := 0.6;
end;

procedure TSingleThreadReallocateBenchmark.RunBenchmark;
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
  {Reallocate in a loop}
  for j := 1 to RepeatCount do
  begin
    {Update usage statistics}
    UpdateUsageStatistics;
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
      {Reallocate the pointer}
      ReallocMem(LPointers[i], LSize);
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
