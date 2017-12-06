// A benchmark to measure raw performance and fragmentation resistance.
// Alternates large number of small string and small number of large string allocations.
// Pure GetMem / FreeMem benchmark without reallocations, similar to WildThreads Benchmark.
// Single-thread version.

unit RawPerformanceSingleThread;

interface

uses
  Windows, BenchmarkClassUnit, Classes, Math;

type
  TRawPerformanceSingleThread = class(TFastcodeMMBenchmark)
  private
    procedure Execute;
  public
    procedure RunBenchmark; override;
    class function GetBenchmarkName: string; override;
    class function GetBenchmarkDescription: string; override;
    class function GetCategory: TBenchmarkCategory; override;
  end;

implementation

uses
  SysUtils;

procedure TRawPerformanceSingleThread.Execute;
const
  POINTERS = 16361; // take prime just below 16384
  MAXCHUNK = 1024;  // take power of 2
var
  i, n, Size, LIndex: Cardinal;
  s: array [0..POINTERS - 1] of string;
begin
  n := Low(s);
  for i := 1 to 8 * 1024 * 1024 do
  begin
    if i and $FF < $F0 then         // 240 times out of 256 ==> chunk < 1 kB
      Size := (4 * i) and (MAXCHUNK-1) + 1
    else if i and $FF <> $FF then   //  15 times out of 256 ==> chunk < 32 kB
      Size := 2 * n + 1
    else                            //   1 time  out of 256 ==> chunk < 256 kB
      Size := 16 * n + 1;
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
      UpdateUsageStatistics;
  end;
  UpdateUsageStatistics;
  for n := Low(s) to High(s) do
    s[n] := '';
end;

{ TRawPerformanceSingleThread }

class function TRawPerformanceSingleThread.GetBenchmarkDescription: string;
begin
  Result := 'A benchmark to measure raw performance and fragmentation resistance. ' +
    'Allocates large number of small strings (< 1 kB) and small number of larger ' +
    '(< 32 kB) to very large (< 256 kB) strings. Single-thread version.';
end;

class function TRawPerformanceSingleThread.GetBenchmarkName: string;
begin
  Result := 'Raw Performance 1 thread';
end;

class function TRawPerformanceSingleThread.GetCategory: TBenchmarkCategory;
begin
  Result := bmSingleThreadAllocAndFree;
end;

procedure TRawPerformanceSingleThread.RunBenchmark;
begin
  inherited;
  Execute;
end;

end.
