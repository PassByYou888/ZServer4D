{A simple benchmark with lots of reallocmem calls}

unit ReallocMemBenchmark;

interface

uses
  BenchmarkClassUnit, Math;

const
  {The number of pointers}
  NumPointers = 10000;
  {The maximum change in a block size per iteration}
  BlockSizeDelta = 2048;

type
  TReallocBench = class(TFastcodeMMBenchmark)
  protected
    FPointers: array[0..NumPointers - 1] of PAnsiChar;
    FBlockSizes: array[0..NumPointers - 1] of integer;
  public
    constructor CreateBenchmark; override;
    destructor Destroy; override;
    procedure RunBenchmark; override;
    class function GetBenchmarkName: string; override;
    class function GetBenchmarkDescription: string; override;
    class function GetCategory: TBenchmarkCategory; override;
  end;

implementation

{ TReallocBench }

constructor TReallocBench.CreateBenchmark;
begin
  inherited;
  {Clear all the pointers}
  FillChar(FPointers, SizeOf(FPointers), 0);
  {Clear all the block sizes}
  FillChar(FBlockSizes, SizeOf(FBlockSizes), 0);
end;

destructor TReallocBench.Destroy;
var
  i: integer;
begin
  {Free all residual pointers}
  for i := 0 to NumPointers - 1 do
    ReallocMem(FPointers[i], 0);
  inherited;
end;

class function TReallocBench.GetBenchmarkDescription: string;
begin
  Result := 'Allocates lots of pointers of random sizes and continues to '
    + 'grow/shrink them randomly in a loop.';
end;

class function TReallocBench.GetBenchmarkName: string;
begin
  Result := 'ReallocMem benchmark';
end;

class function TReallocBench.GetCategory: TBenchmarkCategory;
begin
  Result := bmSingleThreadRealloc;
end;

procedure TReallocBench.RunBenchmark;
var
  i, LPointerNumber: integer;
begin
  {Call the inherited handler}
  inherited;
  {Do the benchmark}
  for i := 1 to 1000000 * 10 do
  begin
    {Get a random pointer number}
    LPointerNumber := Random(NumPointers);
    {Adjust the current block size up or down by up to BlockSizeDelta}
    FBlockSizes[LPointerNumber] := abs(FBlockSizes[LPointerNumber]
      + Random(BlockSizeDelta) - BlockSizeDelta shr 1);
    {Reallocate the pointer}
    ReallocMem(FPointers[LPointerNumber], FBlockSizes[LPointerNumber]);
    {Touch the memory}
    if FBlockSizes[LPointerNumber] > 0 then
    begin
      FPointers[LPointerNumber]^ := #1;
      //check
      ReallocMem(FPointers[LPointerNumber], FBlockSizes[LPointerNumber]);

      PAnsiChar(Integer(FPointers[LPointerNumber]) + FBlockSizes[LPointerNumber] - 1)^ := #2;
    end;
  end;
  {What we end with should be close to the peak usage}
  UpdateUsageStatistics;
end;

end.
