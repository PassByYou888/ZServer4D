{ A benchmark that tests many short-lived threads with many transient small objects. }

unit WildThreadsBenchmarkUnit;

interface

uses Windows, BenchmarkClassUnit, Classes, Math;

type

  TWildThreads = class(TFastcodeMMBenchmark)
  public
    procedure RunBenchmark; override;
    class function GetBenchmarkName: string; override;
    class function GetBenchmarkDescription: string; override;
    class function GetCategory: TBenchmarkCategory; override;
  end;

implementation

uses SysUtils;

type
  TAWildThread = class(TThread)
     procedure Execute; override;
  end;

procedure TAWildThread.Execute;
const
  REPEATCOUNT = 4;
var
  i, j, k, n : Integer;
  p : array [1..151] of Pointer;
begin
  for j := 1 to REPEATCOUNT do
  begin
    // 151 is a prime number, so no trivial cyclicity with 256 (the size)
    for i:=Low(p) to High(p) do
       GetMem(p[i], i);
    k:=Low(p);
    for i:=1 to 100000 do begin
       n:=(i and 255)+1;
       FreeMem(p[k]);
       GetMem(p[k], n);
       // use memory
       PAnsiChar(p[k])[n-1]:=#0;
       PAnsiChar(p[k])[0]:=#0;
       Inc(k);
       if k>High(p) then k:=Low(p);
    end;
    for i:=Low(p) to High(p) do
       FreeMem(p[i]);
  end;
end;

{ TWildThreads }

class function TWildThreads.GetBenchmarkDescription: string;
begin
  Result := 'A benchmark that tests many short-lived threads with many '
    + 'transient small objects. For meaningful results, do not run this '
    + 'benchmark in the Delphi IDE. Benchmark submitted by Eric Grange.';
end;

class function TWildThreads.GetBenchmarkName: string;
begin
  Result := 'Transient threaded objects';
end;

class function TWildThreads.GetCategory: TBenchmarkCategory;
begin
  Result := bmMultiThreadAllocAndFree;
end;

procedure TWildThreads.RunBenchmark;
var
  n : Integer;
  wild : TAWildThread;
  threads : TList;
begin
  inherited;
  
  RandSeed:=0;
  threads:=TList.Create;
  // create threads - would be a thread pool in RealWorld (tm)
  for n:=1 to 96 do begin
     wild:=TAWildThread.Create(True);
     wild.FreeOnTerminate:=False;
     threads.Add(wild);
  end;
  // start all threads at the same time
  Sleep(0); // ensure timeslice is ours
  SetThreadPriority(GetCurrentThread, THREAD_PRIORITY_ABOVE_NORMAL);
  for n:=0 to threads.Count-1 do begin
     wild:=TAWildThread(threads.Items[n]);
     wild.Resume;
  end;
  SetThreadPriority(GetCurrentThread, THREAD_PRIORITY_NORMAL);
  // wait for completion of the threads
  for n:=0 to threads.Count-1 do begin
     UpdateUsageStatistics;
     wild:=TAWildThread(threads.Items[n]);
     wild.WaitFor;
     wild.Free;
  end;
  threads.Free;
end;

end.
