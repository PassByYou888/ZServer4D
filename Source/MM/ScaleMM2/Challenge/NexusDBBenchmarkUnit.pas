{The memory manager benchmark taken from the NexusDB website (www.nexusdb.com).
 Two changes: (1) Monitoring of memory usage was added (shouldn't impact scores
   much) and (2) lowered max items from 4K to 2K to lower memory consumption so
   that the higher thread count tests can be run on computers with 256M RAM.
 Changes from the original benchmark are indicated by a "PLR" in the comment}

// RH 17/04/2005
// class function IterationCount in order to have reasonable benchmark execution times
// IterationCount can be defined in each individual benchmark

unit NexusDBBenchmarkUnit;

interface

uses Windows, SysUtils, Classes, BenchmarkClassUnit, Math;

type

  TNexusBenchmark = class(TFastcodeMMBenchmark)
  protected
    FSemaphore: THandle;
  public
    constructor CreateBenchmark; override;
    destructor Destroy; override;
    class function GetBenchmarkName: string; override;
    class function GetBenchmarkDescription: string; override;
    class function NumThreads: integer; virtual; abstract;
    class function GetBenchmarkWeight: Double; override;
    procedure RunBenchmark; override;
    class function GetCategory: TBenchmarkCategory; override;
    class function IterationCount: integer; virtual;
  end;

  TNexusBenchmark1Thread = class(TNexusBenchmark)
  public
    class function NumThreads: integer; override;
  end;

  TNexusBenchmark2Threads = class(TNexusBenchmark)
  public
    class function NumThreads: integer; override;
    class function IterationCount: integer; override;
  end;

  TNexusBenchmark4Threads = class(TNexusBenchmark)
  public
    class function NumThreads: integer; override;
    class function IterationCount: integer; override;
  end;

  TNexusBenchmark8Threads = class(TNexusBenchmark)
  public
    class function NumThreads: integer; override;
    class function IterationCount: integer; override;
  end;

  TNexusBenchmark12Threads = class(TNexusBenchmark)
  public
    class function NumThreads: integer; override;
    class function RunByDefault: boolean; override;
  end;

  TNexusBenchmark16Threads = class(TNexusBenchmark)
  public
    class function NumThreads: integer; override;
    class function RunByDefault: boolean; override;
  end;

  TNexusBenchmark32Threads = class(TNexusBenchmark)
  public
    class function NumThreads: integer; override;
    class function RunByDefault: boolean; override;
  end;

implementation

const
  MaxItems = 7;
  TestClass : array [1..MaxItems] of TClass =
    (TStringlist, TObject, Tlist, TBits, TCollectionItem, TCollection, TStream);

type
  TTestThread = class(TThread)
  protected
    FBenchmark: TNexusBenchmark;
  public
    procedure Execute; override;
    constructor Create(ABenchmark: TNexusBenchmark);
  end;

{ TTestThread }

constructor TTestThread.Create(ABenchmark: TNexusBenchmark);
begin
  inherited Create(False);
  FreeOnTerminate := True;
  Priority := tpNormal;
  FBenchmark := ABenchmark;
end;

procedure TTestThread.Execute;
var
  k : Integer;
  i, j : Integer;
  p    : Pointer;
  aString: string;
begin
  // direct memory allocation
  with TList.Create do
  try
    for i := 0 to FBenchmark.IterationCount do   // RH replaced 1 * 1000 * 1000 by FBenchmark.IterationCount
    begin
      j:=Random(16*1024);
      GetMem(p, j);
      k:=0;
      while k<j do begin
        PAnsiChar(p)[k]:=#13;
        inc(k,4*1024);
      end;
      if j>0 then
        PAnsiChar(p)[j-1]:=#13;
      Add(p);
      //PLR - Reduced the count from 4K to 2K to lower memory usage
      j := Random(2 * 1024);
      if j < Count then begin
        p := List[j];
        Delete(j);
        FreeMem(p);
      end;

      //PLR - Added to measure usage every 64K iterations
      if i and $ffff = 0 then
        FBenchmark.UpdateUsageStatistics;

    end;
    for i := Pred(Count) downto 0 do
    begin
      FreeMem(List[i]);
    end;
  finally
    Free;
  end;
  // component creation
  with TList.Create do
  try
    for i := 0 to FBenchmark.IterationCount do   // RH replaced 1 * 1000 * 1000 by FBenchmark.IterationCount
    begin
      j := Random(MaxItems);
      Add(TestClass[j+1].Create());
      //PLR - Reduced the count from 4K to 2K to lower memory usage
      j := Random(2 * 1024);
      if j < Count then
      begin
        TComponent(List[j]).Free;
        Delete(j);
      end;

      //PLR - Added to measure usage every 64K iterations
      if i and $ffff = 0 then
        FBenchmark.UpdateUsageStatistics;

    end;
    for i := Pred(Count) downto 0 do
    begin
      TComponent(List[i]).Free;
    end;
  finally
    Free;
  end;
  // strings and stringlist
  with TStringList.Create do
  try
    for i := 0 to FBenchmark.IterationCount do   // RH replaced 1 * 1000 * 1000 by FBenchmark.IterationCount
    begin
      aString:='';
      for j := 0 to Random(250) do
      begin    // Iterate
        aString:=aString+'A';
      end;    // for
      Add(aString);

      //PLR - Added to measure usage every 4K iterations
      if i and $fff = 0 then
        FBenchmark.UpdateUsageStatistics;

      //PLR - Reduced the count from 4K to 2K to lower memory usage
      j := Random(2 * 1024);
      if j < Count then
      begin
        Delete(j);
      end;
    end;
  finally
    Free;
  end;
  {Notify that the thread is done}
  ReleaseSemaphore(FBenchmark.FSemaphore, 1, nil);
end;

{ TNexusBenchmark }

constructor TNexusBenchmark.CreateBenchmark;
begin
  inherited;
  FSemaphore := CreateSemaphore(nil, 0, 9999, nil);
end;

destructor TNexusBenchmark.Destroy;
begin
  CloseHandle(FSemaphore);
  inherited;
end;

class function TNexusBenchmark.GetBenchmarkDescription: string;
begin
  Result := 'The benchmark taken from www.nexusdb.com. Memory usage was '
    + 'slightly reduced to accommodate machines with 256MB RAM with up to 8 threads.';
end;

class function TNexusBenchmark.GetBenchmarkName: string;
begin
  Result := 'NexusDB with ' + IntToStr(NumThreads) + ' thread(s)';
end;

class function TNexusBenchmark.GetBenchmarkWeight: Double;
begin
  {The Nexus benchmark is represented four times, so reduce weighting}
  Result := 0.5;
end;

class function TNexusBenchmark.GetCategory: TBenchmarkCategory;
begin
  Result := bmMultiThreadRealloc;
end;

class function TNexusBenchmark.IterationCount: integer;
begin
  Result := 1 * 1000 * 1000;
end;

procedure TNexusBenchmark.RunBenchmark;
var
  i : Integer;
begin
  {Call the inherited method to reset the peak usage}
  inherited;
  {Create and start the threads}
  for i := 1 to NumThreads do
    TTestThread.Create(Self);
  {Wait for threads to finish}
  for i := 1 to NumThreads do
    WaitForSingleObject(FSemaphore, INFINITE);
end;

{ TNexusBenchmark1Thread }

class function TNexusBenchmark1Thread.NumThreads: integer;
begin
  Result := 1;
end;

{ TNexusBenchmark2Threads }

class function TNexusBenchmark2Threads.IterationCount: integer;
begin
  Result := 500 * 1000;    // RH 50% of the original value
end;

class function TNexusBenchmark2Threads.NumThreads: integer;
begin
  Result := 2;
end;

{ TNexusBenchmark4Threads }

class function TNexusBenchmark4Threads.IterationCount: integer;
begin
  Result := 250 * 1000;    // RH 50% of the original value
end;

class function TNexusBenchmark4Threads.NumThreads: integer;
begin
  Result := 4;
end;

{ TNexusBenchmark8Threads }

class function TNexusBenchmark8Threads.IterationCount: integer;
begin
  Result := 125 * 1000;    // RH 25% of the original value
end;

class function TNexusBenchmark8Threads.NumThreads: integer;
begin
  Result := 8;
end;

{ TNexusBenchmark12Threads }

class function TNexusBenchmark12Threads.NumThreads: integer;
begin
  Result := 12;
end;

class function TNexusBenchmark12Threads.RunByDefault: boolean;
begin
  Result := False;
end;

{ TNexusBenchmark16Threads }

class function TNexusBenchmark16Threads.NumThreads: integer;
begin
  Result := 16;
end;

class function TNexusBenchmark16Threads.RunByDefault: boolean;
begin
  Result := False;
end;

{ TNexusBenchmark32Threads }

class function TNexusBenchmark32Threads.NumThreads: integer;
begin
  Result := 32;
end;

class function TNexusBenchmark32Threads.RunByDefault: boolean;
begin
  Result := False;
end;

end.
