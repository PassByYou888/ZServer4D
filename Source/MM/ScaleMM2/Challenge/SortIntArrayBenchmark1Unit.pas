unit SortIntArrayBenchmark1Unit;

interface

uses Windows, BenchmarkClassUnit, Classes, Math;

type

  TSortIntArrayThreads = class(TFastcodeMMBenchmark)
  public
    procedure RunBenchmark; override;
    class function GetBenchmarkName: string; override;
    class function GetBenchmarkDescription: string; override;
    class function GetSpeedWeight: Double; override;
    class function GetCategory: TBenchmarkCategory; override;
  end;

implementation

uses SysUtils;

type

  TSortIntArrayThread = class(TThread)
     FBenchmark: TFastcodeMMBenchmark;
     procedure Execute; override;
  end;

procedure TSortIntArrayThread.Execute;
var
 IntArray : array of Integer;
 Size, I1, I2, I3, IndexMax, Temp, Max : Integer;
const
 MINSIZE : Integer = 500;
 MAXSIZE : Integer = 1500;

begin
 for Size := MINSIZE to MAXSIZE do
  begin
   SetLength(IntArray, Size);
   //Fill array with random values
   for I1 := 0 to Size-1 do
    begin
     IntArray[I1] := Random(100);
    end;
   //Sort array just to create an acces pattern
   for I2 := 0 to Size-2 do
    begin
     //Find biggest element in unsorted part of array
     Max := IntArray[I2];
     IndexMax := I2;
     for I3 := I2+1 to Size-1 do
      begin
       if IntArray[I3] > Max then
        begin
         Max := IntArray[I3];
         IndexMax := I3;
        end;
      end;
     //Swap current element with biggest remaining element
     Temp := IntArray[I2];
     IntArray[I2] := IntArray[IndexMax];
     IntArray[IndexMax] := Temp;
    end;
  end;
 //"Free" array
 SetLength(IntArray, 0);
 FBenchmark.UpdateUsageStatistics;
end;

class function TSortIntArrayThreads.GetBenchmarkDescription: string;
begin
  Result := 'A benchmark that measures read and write speed to an array of Integer. '
          + 'Access pattern is created by  selection sorting array of random values. '
          + 'Measures memory usage after all blocks have been freed. '
          + 'Benchmark submitted by Dennis Kjaer Christensen.';
end;

class function TSortIntArrayThreads.GetBenchmarkName: string;
begin
  Result := 'SortIntegerArrayBenchmark';
end;

class function TSortIntArrayThreads.GetCategory: TBenchmarkCategory;
begin
  Result := bmMemoryAccessSpeed;
end;

class function TSortIntArrayThreads.GetSpeedWeight: Double;
begin
  Result := 0.75;
end;

procedure TSortIntArrayThreads.RunBenchmark;
var
 SortIntArrayThread : TSortIntArrayThread;

begin
  inherited;
  SortIntArrayThread := TSortIntArrayThread.Create(True);
  SortIntArrayThread.FreeOnTerminate := False;
  SortIntArrayThread.FBenchmark := Self;
  SortIntArrayThread.Resume;
  SortIntArrayThread.WaitFor;
  SortIntArrayThread.Free;
end;

end.
