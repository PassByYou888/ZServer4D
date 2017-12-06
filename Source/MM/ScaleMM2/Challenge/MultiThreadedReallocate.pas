{A multi-threaded benchmark that reallocates memory blocks and uses them.}

unit MultiThreadedReallocate;

interface

uses Windows, BenchmarkClassUnit, Classes, Math;

type

  TMultiThreadReallocateBenchmark = class(TFastcodeMMBenchmark)
  public
    procedure RunBenchmark; override;
    class function GetBenchmarkName: string; override;
    class function GetBenchmarkDescription: string; override;
    class function GetCategory: TBenchmarkCategory; override;
    class function GetSpeedWeight: Double; override;
  end;

implementation

uses SysUtils;

type
  TCreateAndFreeThread = class(TThread)
  public
    procedure Execute; override;
  end;

procedure TCreateAndFreeThread.Execute;
const
  RepeatCount = 250;
  PointerCount = 2500;
var
  i, j, k: Integer;
  LPointers: array[0..PointerCount - 1] of Pointer;
  LMax, LSize, LSum: Integer;
begin
  {Allocate the initial pointers}
  for i := 0 to PointerCount - 1 do
  begin
    {Rough breakdown: 50% of pointers are <=64 bytes, 95% < 1K, 99% < 4K, rest < 256K}
    if i and 1 <> 0 then
      LMax := 64
    else
      if i and 15 <> 0 then
        LMax := 1024
      else
        if i and 255 <> 0 then
          LMax := 4 * 1024
        else
          LMax := 256 * 1024;
    {Get the size, minimum 1}
    LSize := Random(LMax) + 1;
    {Get the pointer}
    GetMem(LPointers[i], LSize);
  end;
  {Reallocate in a loop}
  for j := 1 to RepeatCount do
  begin
    for i := 0 to PointerCount - 1 do
    begin
      {Rough breakdown: 50% of pointers are <=64 bytes, 95% < 1K, 99% < 4K, rest < 256K}
      if i and 1 <> 0 then
        LMax := 64
      else
        if i and 15 <> 0 then
          LMax := 1024
        else
          if i and 255 <> 0 then
            LMax := 4 * 1024
          else
            LMax := 256 * 1024;
      {Get the size, minimum 1}
      LSize := Random(LMax) + 1;
      {Reallocate the pointer}
      ReallocMem(LPointers[i], LSize);
      {Write the memory}
      for k := 0 to (LSize - 1) div 32 do
      begin
        PByte(Integer(LPointers[i]) + k * 32)^ := byte(i);
      end;
      {Read the memory}
      LSum := 0;
      if LSize > 15 then
      begin
        for k := 0 to (LSize - 16) div 32 do
        begin
          Inc(LSum, PShortInt(Integer(LPointers[i]) + k * 32 + 15)^);
        end;
      end;
      {"Use" the sum to suppress the compiler warning}
      if LSum > 0 then;
    end;
  end;
  {Free all the pointers}
  for i := 0 to PointerCount - 1 do
    FreeMem(LPointers[i]);
  {Set the return value}
  ReturnValue := 1;
end;

{ TMultiThreadReallocateBenchmark }

class function TMultiThreadReallocateBenchmark.GetBenchmarkDescription: string;
begin
  Result := 'A multi-threaded benchmark that allocates and reallocates memory blocks. The usage of different block sizes approximates real-world usage as seen in various replays. Allocated memory is actually "used", i.e. written to and read.';
end;

class function TMultiThreadReallocateBenchmark.GetBenchmarkName: string;
begin
  Result := 'Multi-threaded reallocate and use';
end;

class function TMultiThreadReallocateBenchmark.GetCategory: TBenchmarkCategory;
begin
  Result := bmMultiThreadRealloc;
end;

class function TMultiThreadReallocateBenchmark.GetSpeedWeight: Double;
begin
  Result := 0.6;
end;

procedure TMultiThreadReallocateBenchmark.RunBenchmark;
var
  n: Integer;
  LCreateAndFree: TCreateAndFreeThread;
  threads: TList;
  LFinished: Boolean;
begin
  inherited;

  RandSeed:=0;
  threads:=TList.Create;
  {create threads}
  for n := 1 to 10 do begin
    LCreateAndFree:=TCreateAndFreeThread.Create(True);
    LCreateAndFree.FreeOnTerminate:=False;
    threads.Add(LCreateAndFree);
  end;
  {start all threads at the same time}
  SetThreadPriority(GetCurrentThread, THREAD_PRIORITY_ABOVE_NORMAL);
  for n:=0 to threads.Count-1 do begin
    LCreateAndFree:=TCreateAndFreeThread(threads.Items[n]);
    LCreateAndFree.Resume;
  end;
  SetThreadPriority(GetCurrentThread, THREAD_PRIORITY_NORMAL);
  {wait for completion of the threads}
  repeat
    {Any threads still running?}
    LFinished := True;
    for n := 0 to threads.Count - 1 do
    begin
      LCreateAndFree := TCreateAndFreeThread(threads.Items[n]);
      LFinished := LFinished and (LCreateAndFree.ReturnValue <> 0);
    end;
    {Update usage statistics}
    UpdateUsageStatistics;
    {Sleep}
    sleep(10);
  until LFinished;
  {Free the threads}
  for n := 0 to threads.Count - 1 do
  begin
    LCreateAndFree := TCreateAndFreeThread(threads.Items[n]);
    LCreateAndFree.Free;
  end;
  threads.Free;
end;

end.
