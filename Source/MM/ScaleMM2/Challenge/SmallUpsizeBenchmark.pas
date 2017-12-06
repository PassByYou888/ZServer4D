unit SmallUpsizeBenchmark;

interface

uses
  BenchmarkClassUnit, Math;

const
  {The number of pointers}
  NumPointers = 2000000;
  {The maximum block size}
  MaxBlockSize = 32;

type

  TSmallUpsizeBench = class(TFastcodeMMBenchmark)
  protected
    FPointers: array[0..NumPointers - 1] of PAnsiChar;
  public
    constructor CreateBenchmark; override;
    destructor Destroy; override;
    procedure RunBenchmark; override;
    class function GetBenchmarkName: string; override;
    class function GetBenchmarkDescription: string; override;
    class function GetSpeedWeight: Double; override;
    class function GetCategory: TBenchmarkCategory; override;
  end;

implementation

{ TSmallResizeBench }

constructor TSmallUpsizeBench.CreateBenchmark;
begin
  inherited;
end;

destructor TSmallUpsizeBench.Destroy;
begin
  inherited;
end;

class function TSmallUpsizeBench.GetBenchmarkDescription: string;
begin
  Result := 'Allocates a small block and immediately resizes it to a slightly bigger size. This checks '
    + ' that the block upsizing behaviour of the MM is acceptable.';
end;

class function TSmallUpsizeBench.GetBenchmarkName: string;
begin
  Result := 'Small upsize benchmark';
end;

class function TSmallUpsizeBench.GetCategory: TBenchmarkCategory;
begin
  Result := bmSingleThreadRealloc;
end;

class function TSmallUpsizeBench.GetSpeedWeight: Double;
begin
  {Speed is not important here. It is just to check that the bahaviour of the
   MM is acceptable.}
  Result := 0.2;
end;

procedure TSmallUpsizeBench.RunBenchmark;
var
  i, j, LSize: integer;
begin
  {Call the inherited handler}
  inherited;
  {Do the benchmark}
  for i := 0 to high(FPointers) do
  begin
    {Get the initial block size}
    LSize := 1 + Random(MaxBlockSize);
    GetMem(FPointers[i], LSize);
    FPointers[i][0] := #13;
    FPointers[i][LSize - 1] := #13;
    {Reallocate it a few times}
    for j := 1 to 3 do
    begin
      LSize := LSize + Random(MaxBlockSize);
      ReallocMem(FPointers[i], LSize);
      FPointers[i][LSize - 1] := #13;
    end;
  end;
  {What we end with should be close to the peak usage}
  UpdateUsageStatistics;
  {Free the pointers}
  for i := 0 to high(FPointers) do
    FreeMem(FPointers[i]);
end;

end.
