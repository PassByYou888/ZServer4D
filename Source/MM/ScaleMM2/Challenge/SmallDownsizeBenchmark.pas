unit SmallDownsizeBenchmark;

interface

uses
  BenchmarkClassUnit, Math;

const
  {The number of pointers}
  NumPointers = 2000000;
  {The maximum block size}
  MaxBlockSize = 64;

type

  TSmallDownsizeBench = class(TFastcodeMMBenchmark)
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

constructor TSmallDownsizeBench.CreateBenchmark;
begin
  inherited;
end;

destructor TSmallDownsizeBench.Destroy;
begin
  inherited;
end;

class function TSmallDownsizeBench.GetBenchmarkDescription: string;
begin
  Result := 'Allocates a small block and immediately resizes it to a smaller size. This checks '
    + ' that the block downsizing behaviour of the MM is acceptable.';
end;

class function TSmallDownsizeBench.GetBenchmarkName: string;
begin
  Result := 'Small downsize benchmark';
end;

class function TSmallDownsizeBench.GetCategory: TBenchmarkCategory;
begin
  Result := bmSingleThreadRealloc;
end;

class function TSmallDownsizeBench.GetSpeedWeight: Double;
begin
  {Speed is not important here. It is just to check that the behaviour of the
   MM is acceptable.}
  Result := 0.2;
end;

procedure TSmallDownsizeBench.RunBenchmark;
var
  i, j, LSize: integer;
begin
  {Call the inherited handler}
  inherited;
  {Do the benchmark}
  for i := 0 to high(FPointers) do
  begin
    {Get the initial block size}
    LSize := MaxBlockSize + Random(3 * MaxBlockSize);
    GetMem(FPointers[i], LSize);
    FPointers[i][0] := #13;
    FPointers[i][LSize - 1] := #13;
    {Reallocate it a few times}
    for j := 1 to 5 do
    begin
      LSize := Max(1, LSize - Random(MaxBlockSize));
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
