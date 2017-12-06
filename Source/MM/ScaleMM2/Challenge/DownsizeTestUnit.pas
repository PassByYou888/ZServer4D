{A benchmark demonstrating that never downsizing memory blocks can lead to
 problems.}

unit DownsizeTestUnit;

interface

uses BenchmarkClassUnit, Math;

type

  TDownsizeTest = class(TFastcodeMMBenchmark)
  protected
    FStrings: array of string;
  public
    procedure RunBenchmark; override;
    class function GetBenchmarkName: string; override;
    class function GetBenchmarkDescription: string; override;
    class function GetSpeedWeight: Double; override;
    class function GetCategory: TBenchmarkCategory; override;
  end;

implementation

{ TDownsizeTest }

class function TDownsizeTest.GetBenchmarkDescription: string;
begin
  Result := 'Allocates large blocks and immediately resizes them to a '
    + 'much smaller size. This checks whether the memory manager downsizes '
    + 'memory blocks correctly. '
    + 'Benchmark submitted by Pierre le Riche.';
end;

class function TDownsizeTest.GetBenchmarkName: string;
begin
  Result := 'Block downsize';
end;

class function TDownsizeTest.GetCategory: TBenchmarkCategory;
begin
  Result := bmSingleThreadRealloc;
end;

class function TDownsizeTest.GetSpeedWeight: Double;
begin
  {Speed is not important here. It is just to check that the behaviour of the
   MM is acceptable.}
  Result := 0.2;
end;

procedure TDownsizeTest.RunBenchmark;
var
  i, n, LOffset: integer;
begin
  inherited;

//  for n := 1 to 50 do     // loop added to have more than 1000 MTicks for this benchmark
  for n := 1 to 10 do     // loop added to have more than 1000 MTicks for this benchmark
  begin
    {Allocate a lot of strings}
    SetLength(FStrings, 3000000);
    for i := 0 to high(FStrings) do
    begin
      {Grab a 20K block}
      SetLength(FStrings[i], 20000);
      {Touch memory}
      LOffset := 1;
      while LOffset <= 20000 do
      begin
        FStrings[i][LOffset] := #1;
        Inc(LOffset, 4096);
      end;
      {Reduce the size to 1 byte}
      SetLength(FStrings[i], 1);
    end;
    {Update the peak address space usage}
    UpdateUsageStatistics;
  end;
end;

end.
