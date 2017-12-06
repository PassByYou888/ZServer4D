unit MemFreeBenchmark1Unit;

interface

uses Windows, BenchmarkClassUnit, Classes, Math;

type

  TMemFreeThreads1 = class(TFastcodeMMBenchmark)
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

  TMemFreeThread1 = class(TThread)
     FBenchmark: TFastcodeMMBenchmark;
     procedure Execute; override;
  end;

procedure TMemFreeThread1.Execute;
var
 PointerArray : array of Pointer;
 I, AllocSize : Integer;
 AllocSizeFP : Double;
const
 NOOFPOINTERS : Integer = 600000;
 ALLOCGROWSTEPSIZE : Double = 0.001;
 SLEEPTIMEAFTERFREE : Integer = 10;//Seconds to free

begin
 //Allocate
 SetLength(PointerArray, NOOFPOINTERS);
 AllocSizeFP := 1;
 for I:= 0 to Length(PointerArray)-1 do
  begin
   AllocSizeFP := AllocSizeFP + ALLOCGROWSTEPSIZE;
   AllocSize := Round(AllocSizeFP);
   GetMem(PointerArray[I], AllocSize);
  end;
 //Free
 for I:= 0 to Length(PointerArray)-1 do
  FreeMem(PointerArray[I]);
 SetLength(PointerArray, 0);
 //Give a little time to free
 Sleep(SLEEPTIMEAFTERFREE*1000);
 FBenchmark.UpdateUsageStatistics;
end;

class function TMemFreeThreads1.GetBenchmarkDescription: string;
begin
  Result := 'A benchmark that measures how much memory is left unfreed after heavy work '
    + 'Benchmark submitted by Dennis Kjaer Christensen.';
end;

class function TMemFreeThreads1.GetBenchmarkName: string;
begin
  Result := 'MemFree1';
end;

class function TMemFreeThreads1.GetCategory: TBenchmarkCategory;
begin
  Result := bmSingleThreadAllocAndFree;
end;

class function TMemFreeThreads1.GetSpeedWeight: Double;
begin
  Result := 0.0;
end;

procedure TMemFreeThreads1.RunBenchmark;
var
  MemFreeThread1 : TMemFreeThread1;

begin
  inherited;
  MemFreeThread1 := TMemFreeThread1.Create(True);
  MemFreeThread1.FreeOnTerminate := False;
  MemFreeThread1.FBenchmark := Self;
  MemFreeThread1.Resume;
  MemFreeThread1.WaitFor;
  MemFreeThread1.Free;
end;

end.
