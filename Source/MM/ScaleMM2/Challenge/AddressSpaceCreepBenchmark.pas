unit AddressSpaceCreepBenchmark;

interface

uses
  BenchmarkClassUnit, Math;

const
  {The number of pointers}
  NumPointers = 1000000;
  {The maximum block size}
  MaxBlockSize = 256;

type

  TAddressSpaceCreepBench = class(TFastcodeMMBenchmark)
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

constructor TAddressSpaceCreepBench.CreateBenchmark;
begin
  inherited;
end;

destructor TAddressSpaceCreepBench.Destroy;
begin
  inherited;
end;

class function TAddressSpaceCreepBench.GetBenchmarkDescription: string;
begin
  Result := 'Allocates and deallocates millions of pointers in a loop, checking that the MM address space usage does not grow unbounded.';
end;

class function TAddressSpaceCreepBench.GetBenchmarkName: string;
begin
  Result := 'Address space creep benchmark';
end;

class function TAddressSpaceCreepBench.GetCategory: TBenchmarkCategory;
begin
  Result := bmSingleThreadAllocAndFree;
end;

procedure TAddressSpaceCreepBench.RunBenchmark;
var
  i, j, LSize: integer;
begin
  {Call the inherited handler}
  inherited;
 {Allocate the pointers}
  for i := 0 to high(FPointers) do
  begin
    {Get an initial size}
    LSize := 1 + random(MaxBlockSize);
    {Allocate the pointer}
    GetMem(FPointers[i], LSize);
    {Touch the memory}
    FPointers[i][0] := ansichar(byte(i));
    FPointers[i][LSize - 1] := ansichar(byte(i));
  end;
  {Free and get new pointers in a loop}
  for j := 1 to 50 do
  begin
    for i := 0 to high(FPointers) do
    begin
      {Free the pointer}
      FreeMem(FPointers[i]);
      {Get the new size}
      LSize := 1 + random(MaxBlockSize);
      {Allocate the pointer}
      GetMem(FPointers[i], LSize);
      {Touch the memory}
      FPointers[i][0] := ansichar(byte(i));
      FPointers[i][LSize - 1] := ansichar(byte(i));
    end;
  end;
  {What we end with should be close to the peak usage}
  UpdateUsageStatistics;
  {Free the pointers}
  for i := 0 to high(FPointers) do
    FreeMem(FPointers[i], 1);
end;

end.
