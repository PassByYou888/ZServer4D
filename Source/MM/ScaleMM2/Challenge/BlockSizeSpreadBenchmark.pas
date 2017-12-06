unit BlockSizeSpreadBenchmark;

interface

uses
  BenchmarkClassUnit, Math;

const
  {The number of pointers}
  NumPointers = 2000000;
  {The maximum block size}
  MaxBlockSize = 25; // *4

type

  TBlockSizeSpreadBench = class(TFastcodeMMBenchmark)
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

constructor TBlockSizeSpreadBench.CreateBenchmark;
begin
  inherited;
end;

destructor TBlockSizeSpreadBench.Destroy;
begin
  inherited;
end;

class function TBlockSizeSpreadBench.GetBenchmarkDescription: string;
begin
  Result := 'Allocates millions of small objects, checking that the MM has a decent block size spread.';
end;

class function TBlockSizeSpreadBench.GetBenchmarkName: string;
begin
  Result := 'Block size spread benchmark';
end;

class function TBlockSizeSpreadBench.GetCategory: TBenchmarkCategory;
begin
  Result := bmSingleThreadAllocAndFree;
end;

procedure TBlockSizeSpreadBench.RunBenchmark;
var
  i, n, LSize: integer;
begin
  {Call the inherited handler}
  inherited;

  for n := 1 to 3 do     // loop added to have more than 1000 MTicks for this benchmark
  begin
    {Do the benchmark}
    for i := 0 to high(FPointers) do
    begin
      {Get the initial block size, assume object sizes are 4-byte aligned}
      LSize := (1 + random(MaxBlockSize)) * 4;
      GetMem(FPointers[i], LSize);
      FPointers[i][0] := #13;
      FPointers[i][LSize - 1] := #13;
    end;
    {What we end with should be close to the peak usage}
    UpdateUsageStatistics;
    {Free the pointers}
    for i := 0 to high(FPointers) do
      FreeMem(FPointers[i]);
  end;
end;

end.
