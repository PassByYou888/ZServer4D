{A benchmark demonstrating how the RTL Delphi MM fragments the virtual address
 space}

unit FragmentationTestUnit;

interface

uses BenchmarkClassUnit, Math;

type

  TFragmentationTest = class(TFastcodeMMBenchmark)
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

uses
  Windows;

{ TFragmentationTest }

class function TFragmentationTest.GetBenchmarkDescription: string;
begin
  Result := 'A benchmark that intersperses large block allocations with the '
    + 'allocation of smaller blocks to test how resistant the memory manager '
    + 'is to address space fragmentation that may eventually lead to '
    + '"out of memory" errors.';
end;

class function TFragmentationTest.GetBenchmarkName: string;
begin
  Result := 'Fragmentation Test';
end;

class function TFragmentationTest.GetCategory: TBenchmarkCategory;
begin
  Result := bmSingleThreadRealloc;
end;

class function TFragmentationTest.GetSpeedWeight: Double;
begin
  {This benchmark is just to show how the RTL MM fragments. No real-world
   significance apart from that, so we ignore the speed.}
  Result := 0.2;
end;

procedure TFragmentationTest.RunBenchmark;
var
  i, n: integer;
begin
  inherited;

  try
  //for n := 1 to 3 do     // loop added to have more than 1000 MTicks for this benchmark
  for n := 1 to 15 do     // loop added to have more than 1000 MTicks for this benchmark
  begin
    SetLength(FStrings, 0);
    for i := 1 to 90 do
    begin
      //add 100000 elements
      SetLength(FStrings, length(FStrings) + 100000);
      //allocate a 1 length string
      SetLength(FStrings[high(FStrings)], 1);
    end;
    {Update the peak address space usage}
    UpdateUsageStatistics;
  end;
  except
    sleep(0);
  end;
end;

end.
