// A benchmark to measure raw performance and fragmentation resistance.
// Alternates large number of small string and small number of large string allocations.
// Pure GetMem / FreeMem benchmark without reallocations, similar to WildThreads Benchmark.
// 8-thread version that has approx. same memory footprint and CPU load as single-thread version.

unit RawPerformanceMultiThread;

interface

uses
  Windows, BenchmarkClassUnit, Classes, Math;

type
  TRawPerformanceMultiThread = class(TFastcodeMMBenchmark)
  private
  public
    procedure RunBenchmark; override;
    class function GetBenchmarkName: string; override;
    class function GetBenchmarkDescription: string; override;
    class function GetCategory: TBenchmarkCategory; override;
  end;

implementation

uses
  SysUtils;

type
  TRawPerformanceThread = class(TThread)
  public
    FBenchmark: TFastcodeMMBenchmark;
    procedure Execute; override;
  end;

const
  THREADCOUNT = 8;
//  THREADCOUNT = 2;

procedure TRawPerformanceThread.Execute;
const
  POINTERS = 2039;  // take prime just below 2048  (scaled down 8x from single-thread)
  MAXCHUNK = 1024;  // take power of 2
  REPEATCOUNT = 4;
//  REPEATCOUNT = 2;
var
  i, j, n, Size, LIndex: Cardinal;
  s: array [0..POINTERS - 1] of string;
begin
  for j := 1 to REPEATCOUNT do
  begin
    n := Low(s);
    //for i := 1 to 1 * 1024 * 1024 do begin
    for i := 1 to 1 * 1024 * 1024 do begin
      if i and $FF < $F0 then         // 240 times out of 256 ==> chunk < 1 kB
        Size := (4 * i) and (MAXCHUNK-1) + 1
      else if i and $FF <> $FF then   //  15 times out of 256 ==> chunk < 32 kB
        Size := 16 * n + 1
      else                            //   1 time  out of 256 ==> chunk < 256 kB
        Size := 128 * n + 1;
      s[n] := '';
      SetLength(s[n], Size);
      //start and end of string are already assigned, access every 4K page in the middle
      LIndex := 1;
      while LIndex <= Size do
      begin
        s[n][LIndex] := #1;
        Inc(LIndex, 4096);
      end;
      Inc(n);
      if n > High(s) then
        n := Low(s);
      if i and $FFFF = 0 then
        FBenchmark.UpdateUsageStatistics;
    end;
    FBenchmark.UpdateUsageStatistics;
    for n := Low(s) to High(s) do
      s[n] := '';
  end;
end;

{ TRawPerformanceMultiThread }

class function TRawPerformanceMultiThread.GetBenchmarkDescription: string;
begin
  Result := 'A benchmark to measure raw performance and fragmentation resistance. ' +
    'Allocates large number of small strings (< 1 kB) and small number of larger ' +
    '(< 32 kB) to very large (< 256 kB) strings. 8-thread version.';
end;

class function TRawPerformanceMultiThread.GetBenchmarkName: string;
begin
  Result := 'Raw Performance 8 threads';
end;

class function TRawPerformanceMultiThread.GetCategory: TBenchmarkCategory;
begin
  Result := bmMultiThreadAllocAndFree;
end;

procedure TRawPerformanceMultiThread.RunBenchmark;
var
  Threads: array [0..THREADCOUNT - 1] of TRawPerformanceThread;
  i: integer;
begin
  inherited;
  for i := 0 to THREADCOUNT - 1 do begin
    Threads[i] := TRawPerformanceThread.Create(True);
    Threads[i].FreeOnTerminate := False;
    Threads[i].FBenchmark := Self;
  end;
  Sleep(0);
  SetThreadPriority(GetCurrentThread, THREAD_PRIORITY_ABOVE_NORMAL);
  for i := 0 to THREADCOUNT - 1 do
    Threads[i].Resume;
  SetThreadPriority(GetCurrentThread, THREAD_PRIORITY_NORMAL);
  for i := 0 to THREADCOUNT - 1 do begin
    Threads[i].WaitFor;
    Threads[i].Free;
  end;
end;

end.
