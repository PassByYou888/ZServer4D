unit SortExtendedArrayBenchmark1Unit;

interface

uses Windows, BenchmarkClassUnit, Classes, Math;

type

  TSortExtendedArrayThreads = class(TFastcodeMMBenchmark)
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

  TSortExtendedArrayThread = class(TThread)
     FBenchmark: TFastcodeMMBenchmark;
     procedure Execute; override;
  end;

  TExtended = record
   X : Extended;
   Pad1, Pad2, Pad3, Pad4, Pad5, Pad6 : Byte;
  end;

 TExtendedArray = array[0..1000000] of TExtended;
 PExtendedArray = ^TExtendedArray;

procedure TSortExtendedArrayThread.Execute;
var
 ExtArray :  PExtendedArray;
 Size, I1, I2, I3, IndexMax, RunNo, LowIndex, HighIndex : Integer;
 Temp, Max : Extended;
const
 MAXRUNNO : Integer = 8;
 MAXELEMENTVALUE : Integer = MAXINT;
 MINSIZE : Integer = 100;
 MAXSIZE : Integer = 10000;

begin
 GetMem(ExtArray, MINSIZE * SizeOf(TExtended));
 for RunNo := 1 to MAXRUNNO do
  begin
   Size := Random(MAXSIZE-MINSIZE) + MINSIZE;
   //SetLength(ExtArray, Size);
   ReallocMem(ExtArray, Size * SizeOf(TExtended));
   //Fill array with random values
   for I1 := 0 to Size-1 do
    begin
     ExtArray[I1].X := Random(MAXELEMENTVALUE);
    end;
   //Sort array just to create an acces pattern
   //Using some weird DKC sort algorithm
   LowIndex := 0;
   HighIndex := Size-1;
   repeat
    if ExtArray[LowIndex].X > ExtArray[HighIndex].X then
     begin
      //Swap
      Temp := ExtArray[LowIndex].X;
      ExtArray[LowIndex].X := ExtArray[HighIndex].X;
      ExtArray[HighIndex].X := Temp;
     end;
    Inc(LowIndex);
    Dec(HighIndex);
   until(LowIndex >= HighIndex);
   for I2 := Size-1 downto 1 do
    begin
     //Find biggest element in unsorted part of array
     Max := ExtArray[I2].X;
     IndexMax := I2;
     for I3 := I2-1 downto 0 do
      begin
       if ExtArray[I3].X > Max then
        begin
         Max := ExtArray[I3].X;
         IndexMax := I3;
        end;
      end;
     //Swap current element with biggest remaining element
     Temp := ExtArray[I2].X;
     ExtArray[I2].X := ExtArray[IndexMax].X;
     ExtArray[IndexMax].X := Temp;
    end;
  end;
 //Free array
 FreeMem(ExtArray);
 FBenchmark.UpdateUsageStatistics;
end;

class function TSortExtendedArrayThreads.GetBenchmarkDescription: string;
begin
  Result := 'A benchmark that measures read and write speed to an array of Extendeds. '
          + 'The Extended type is padded to be 16 byte. '
          + 'Bonus is given for 16 byte alignment of array '
          + 'Will also reveil cache set associativity related issues. '
          + 'Access pattern is created by X sorting array of random values. '
          + 'Measures memory usage after all blocks have been freed. '
          + 'Benchmark submitted by Dennis Kjaer Christensen.';
end;

class function TSortExtendedArrayThreads.GetBenchmarkName: string;
begin
  Result := 'SortExtendedArrayBenchmark';
end;

class function TSortExtendedArrayThreads.GetCategory: TBenchmarkCategory;
begin
  Result := bmMemoryAccessSpeed;
end;

class function TSortExtendedArrayThreads.GetSpeedWeight: Double;
begin
  Result := 0.75;
end;

procedure TSortExtendedArrayThreads.RunBenchmark;
var
 SortExtendedArrayThread1, SortExtendedArrayThread2 : TSortExtendedArrayThread;
 SortExtendedArrayThread3, SortExtendedArrayThread4 : TSortExtendedArrayThread;

begin
  inherited;
  SortExtendedArrayThread1 := TSortExtendedArrayThread.Create(True);
  SortExtendedArrayThread2 := TSortExtendedArrayThread.Create(True);
  SortExtendedArrayThread3 := TSortExtendedArrayThread.Create(True);
  SortExtendedArrayThread4 := TSortExtendedArrayThread.Create(True);
  SortExtendedArrayThread1.FreeOnTerminate := False;
  SortExtendedArrayThread2.FreeOnTerminate := False;
  SortExtendedArrayThread3.FreeOnTerminate := False;
  SortExtendedArrayThread4.FreeOnTerminate := False;
  SortExtendedArrayThread1.Priority := tpLower;
  SortExtendedArrayThread2.Priority := tpNormal;
  SortExtendedArrayThread3.Priority := tpHigher;
  SortExtendedArrayThread4.Priority := tpHighest;
  SortExtendedArrayThread1.FBenchmark := Self;
  SortExtendedArrayThread2.FBenchmark := Self;
  SortExtendedArrayThread3.FBenchmark := Self;
  SortExtendedArrayThread4.FBenchmark := Self;
  SortExtendedArrayThread1.Resume;
  SortExtendedArrayThread2.Resume;
  SortExtendedArrayThread3.Resume;
  SortExtendedArrayThread4.Resume;
  SortExtendedArrayThread1.WaitFor;
  SortExtendedArrayThread2.WaitFor;
  SortExtendedArrayThread3.WaitFor;
  SortExtendedArrayThread4.WaitFor;
  SortExtendedArrayThread1.Free;
  SortExtendedArrayThread2.Free;
  SortExtendedArrayThread3.Free;
  SortExtendedArrayThread4.Free;
end;

end.
