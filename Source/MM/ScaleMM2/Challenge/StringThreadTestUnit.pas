{****************************************************************************************

   StringTestBenchMark & ManyThreadsTestBenchMark v1.0

   By Ivo Tops for FastCode Memory Manager BenchMark & Validation

****************************************************************************************}
unit StringThreadTestUnit;

interface

uses BenchmarkClassUnit;

type

   TStringThreadTest = class(TFastcodeMMBenchmark)
   protected
   public
     procedure RunBenchmark; override;
     class function GetBenchmarkName: string; override;
     class function GetBenchmarkDescription: string; override;
     class function GetSpeedWeight: Double; override;
     class function GetCategory: TBenchmarkCategory; override;
   end;

   TManyThreadsTest = class(TFastcodeMMBenchmark)
   protected
   public
     procedure RunBenchmark; override;
     class function GetBenchmarkName: string; override;
     class function GetBenchmarkDescription: string; override;
     class function GetSpeedWeight: Double; override;
     class function GetCategory: TBenchmarkCategory; override;
   end;

// Counters for thread running
procedure IncRunningThreads;
procedure DecRunningThreads;
procedure NotifyThreadError;
procedure NotifyValidationError;

implementation

uses Math, StringThread, windows, sysutils;

var RunningThreads: Integer;
   ThreadError, ValidationError, ThreadMaxReached, ZeroThreadsReached:
Boolean;

procedure InitTest;
begin
   RunningThreads := 0;
   ZeroThreadsReached := False;
   ThreadMaxReached := False;
   ThreadError := False;
end;

procedure ExitTest;
begin
   // If Thread had error raise exception
   if ThreadError then raise Exception.Create('TestThread failed with an Error');
   // If Thread had validate raise exception
   if ValidationError then raise Exception.Create('TestThread failed Validation');
end;

{ TStringThreadTest }

class function TStringThreadTest.GetBenchmarkDescription: string;
begin
   Result := 'A benchmark that does string manipulations concurrently in 8 different threads';
end;

class function TStringThreadTest.GetBenchmarkName: string;
begin
   Result := 'StringThreadTest';
end;

class function TStringThreadTest.GetCategory: TBenchmarkCategory;
begin
  Result := bmMultiThreadRealloc;
end;

class function TStringThreadTest.GetSpeedWeight: Double;
begin
  {We're testing speed here, not memory usage}
  Result := 0.8;
end;

procedure TStringThreadTest.RunBenchmark;
var I, J: Integer;
begin
   inherited;
   InitTest;
   for J := 1 to 4 do
   begin
     for I := 1 to 8 do // Create a loose new thread that does stringactions
       TStringThread.Create(25, 2000, 4096, False);
     // Simply wait for all threads to finish
     while not ZeroThreadsReached do sleep(10);
   end;
   {Update the peak address space usage}
   UpdateUsageStatistics;
   // Done
   ExitTest;
end;

procedure IncRunningThreads;
var RT: Integer;
begin
   RT := InterlockedExchangeAdd(@RunningThreads, 1);
   ZeroThreadsReached := False;
   ThreadMaxReached := RT > 1250;
end;

procedure DecRunningThreads;
var RT: Integer;
begin
   RT := InterlockedExchangeAdd(@RunningThreads, -1);
   ThreadMaxReached := RT > 1250;
   ZeroThreadsReached := RT = 1; // Old value is 1, so new value is zero
end;

{ TManyThreadsTest }

class function TManyThreadsTest.GetBenchmarkDescription: string;
begin
   Result := 'A benchmark that has many temporary threads, each doing a little string processing. ';
   Result := Result + 'This test exposes possible multithreading issues in a memory manager and large per-thread ';
   Result := Result + 'memory requirements.';
end;

class function TManyThreadsTest.GetBenchmarkName: string;
begin
   Result := 'ManyShortLivedThreads';
end;

class function TManyThreadsTest.GetCategory: TBenchmarkCategory;
begin
  Result := bmMultiThreadRealloc;
end;

class function TManyThreadsTest.GetSpeedWeight: Double;
begin
  {We're testing speed here, not memory usage}
  Result := 0.8;
end;

procedure TManyThreadsTest.RunBenchmark;
var
   I: Integer;
begin
   inherited;
   InitTest;
   // Launch a lot of threads
//   for I := 1 to 100 do
   for I := 1 to 50 do
   begin
     TStringThread.Create(1000, 10, 512, False);
     TStringThread.Create(10, 2, 4096, False);
     TStringThread.Create(10, 2, 1024*1024, False);
   end;
   // Launch a lot of threads keeping threadmax in account
   //for I := 1 to 500 do
   for I := 1 to 100 do
   begin
     TStringThread.Create(100, 1, 512, False);
     TStringThread.Create(100, 100, 512, False);
     TStringThread.Create(100, 1, 512, False);
     while ThreadMaxReached do sleep(1);
   end;
   // Wait for all threads to finish
   while not ZeroThreadsReached do sleep(50);
   {Update the peak address space usage}
   UpdateUsageStatistics;
   // Done
   ExitTest;
end;

procedure NotifyThreadError;
begin
   ThreadError := True;
end;

procedure NotifyValidationError;
begin
   ValidationError := True;
end;

end.


