unit MemFreeBenchmark2Unit;

interface

uses Windows, BenchmarkClassUnit, Classes, Math;

type

  TMemFreeThreads2 = class(TFastcodeMMBenchmark)
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

  TMemFreeThread2 = class(TThread)
     FBenchmark: TFastcodeMMBenchmark;
     procedure Execute; override;
  end;

procedure TMemFreeThread2.Execute;
begin
 Sleep(100); //Do not run in zero ticks !!!!!
 FBenchmark.UpdateUsageStatistics;
end;

class function TMemFreeThreads2.GetBenchmarkDescription: string;
begin
  Result := 'A benchmark that measures how much memory the MM allocates when doing close to nothing'
    + 'Benchmark submitted by Dennis Kjaer Christensen.';
end;

class function TMemFreeThreads2.GetBenchmarkName: string;
begin
  Result := 'MemFree2';
end;

class function TMemFreeThreads2.GetCategory: TBenchmarkCategory;
begin
  Result := bmSingleThreadAllocAndFree;
end;

class function TMemFreeThreads2.GetSpeedWeight: Double;
begin
  Result := 0.0;
end;

procedure TMemFreeThreads2.RunBenchmark;
var
  MemFreeThread2 : TMemFreeThread2;

begin
  inherited;
  MemFreeThread2 := TMemFreeThread2.Create(True);
  MemFreeThread2.FreeOnTerminate := False;
  MemFreeThread2.FBenchmark := Self;
  MemFreeThread2.Resume;
  MemFreeThread2.WaitFor;
  MemFreeThread2.Free;
end;

end.
