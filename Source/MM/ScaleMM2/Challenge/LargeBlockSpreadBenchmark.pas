unit LargeBlockSpreadBenchmark;

interface

uses
  BenchmarkClassUnit, Math;

const
  {The number of pointers}
  NumPointers = 2000;
  {The block size}
  BlockSize = 65537;

type

  TLargeBlockSpreadBench = class(TFastcodeMMBenchmark)
  protected
    FPointers: array[0..NumPointers - 1] of PAnsiChar;
  public
    constructor CreateBenchmark; override;
    destructor Destroy; override;
    procedure RunBenchmark; override;
    class function GetBenchmarkName: string; override;
    class function GetBenchmarkDescription: string; override;
    class function GetCategory: TBenchmarkCategory; override;
  end;

implementation

{ TSmallResizeBench }

constructor TLargeBlockSpreadBench.CreateBenchmark;
begin
  inherited;
end;

destructor TLargeBlockSpreadBench.Destroy;
begin
  inherited;
end;

class function TLargeBlockSpreadBench.GetBenchmarkDescription: string;
begin
  Result := 'Allocates a few random sized large blocks (>64K), checking that the MM manages large blocks efficiently.';
end;

class function TLargeBlockSpreadBench.GetBenchmarkName: string;
begin
  Result := 'Large block spread benchmark';
end;

class function TLargeBlockSpreadBench.GetCategory: TBenchmarkCategory;
begin
  Result := bmSingleThreadAllocAndFree;
end;

procedure TLargeBlockSpreadBench.RunBenchmark;
var
  i, j, k, LSize: integer;
begin
  {Call the inherited handler}
  inherited;
  {Do the benchmark}
  for j := 1 to 5 do
  begin
    for i := 0 to high(FPointers) do
    begin
      {Get the block}
      LSize := (1 + Random(3)) * BlockSize;
      GetMem(FPointers[i], LSize);
      {Fill the memory}
      for k := 0 to LSize - 1 do
        FPointers[i][k] := ansichar(k);
    end;
    {What we end with should be close to the peak usage}
    UpdateUsageStatistics;
    {Free the pointers}
    for i := 0 to high(FPointers) do
      FreeMem(FPointers[i]);
  end;
end;

end.
