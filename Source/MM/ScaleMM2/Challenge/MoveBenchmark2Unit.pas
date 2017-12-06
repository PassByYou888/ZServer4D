unit MoveBenchmark2Unit;

interface

uses Windows, BenchmarkClassUnit, Classes, Math;

type

  TMoveThreads2 = class(TFastcodeMMBenchmark)
  public
    procedure RunBenchmark; override;
    class function GetBenchmarkName: string; override;
    class function GetBenchmarkDescription: string; override;
    class function GetSpeedWeight: Double; override;
    class function GetCategory: TBenchmarkCategory; override;
  end;

implementation

uses SysUtils, MoveJOHUnit9;

type

  TMoveThread2 = class(TThread)
   FBenchmark: TFastcodeMMBenchmark;
   procedure Execute; override;
  end;

procedure TMoveThread2.Execute;
var
 I1, I2, I3, I4, I5 : Integer;
 //Need many arrays because a 4 byte aligned array can be 16 byte aligned by pure chance
 //Reallocs migth change alignment
 SrcArray1 : array of Byte;
 DestArray1 : array of Byte;
 SrcArray2 : array of Byte;
 DestArray2 : array of Byte;
 SrcArray3 : array of Byte;
 DestArray3 : array of Byte;
 SrcArray4 : array of Byte;
 DestArray4 : array of Byte;
 SrcArray5 : array of Byte;
 DestArray5 : array of Byte;
 SrcArray6 : array of Byte;
 DestArray6 : array of Byte;
 SrcArray7 : array of Byte;
 DestArray7 : array of Byte;
 SrcArray8 : array of Byte;
 DestArray8 : array of Byte;
 BenchArraySize : Integer;
const
 NOOFRUNS : Integer = 2;
 MINBENCHARRAYSIZE : Integer = 2000;//16K
 MAXBENCHARRAYSIZE : Integer = 2000000;//1M
 STEPSIZE : Integer = 2;
 NOOFMOVESPERRUN : Integer = 250;

begin
 for I1 := 0 to NOOFRUNS - 1do
  begin
   BenchArraySize := MINBENCHARRAYSIZE;
   while BenchArraySize <= MAXBENCHARRAYSIZE do
    begin
     SetLength(SrcArray1, BenchArraySize+8);
     SetLength(DestArray1, BenchArraySize+8);
     SetLength(SrcArray2, BenchArraySize+8);
     SetLength(DestArray2, BenchArraySize+8);
     SetLength(SrcArray3, BenchArraySize+8);
     SetLength(DestArray3, BenchArraySize+8);
     SetLength(SrcArray4, BenchArraySize+8);
     SetLength(DestArray4, BenchArraySize+8);
     SetLength(SrcArray5, BenchArraySize+8);
     SetLength(DestArray5, BenchArraySize+8);
     SetLength(SrcArray6, BenchArraySize+8);
     SetLength(DestArray6, BenchArraySize+8);
     SetLength(SrcArray7, BenchArraySize+8);
     SetLength(DestArray7, BenchArraySize+8);
     SetLength(SrcArray8, BenchArraySize+8);
     SetLength(DestArray8, BenchArraySize+8);
     for I2 := 1 to NOOFMOVESPERRUN do
      begin
       MoveJOH_SSE_9(SrcArray1[8], DestArray1[8], BenchArraySize);
       MoveJOH_SSE_9(DestArray2[8], SrcArray2[8], BenchArraySize);
      end;
     for I3 := 1 to NOOFMOVESPERRUN do
      begin
       MoveJOH_SSE_9(SrcArray3[8], DestArray3[8], BenchArraySize);
       MoveJOH_SSE_9(DestArray4[8], SrcArray4[8], BenchArraySize);
      end;
     for I4 := 1 to NOOFMOVESPERRUN do
      begin
       MoveJOH_SSE_9(SrcArray5[8], DestArray5[8], BenchArraySize);
       MoveJOH_SSE_9(DestArray6[8], SrcArray6[8], BenchArraySize);
      end;
     for I5 := 1 to NOOFMOVESPERRUN do
      begin
       MoveJOH_SSE_9(SrcArray7[8], DestArray7[8], BenchArraySize);
       MoveJOH_SSE_9(DestArray8[8], SrcArray8[8], BenchArraySize);
      end;
     BenchArraySize := BenchArraySize * STEPSIZE;
     FBenchmark.UpdateUsageStatistics;
    end;
  end;
end;

class function TMoveThreads2.GetBenchmarkDescription: string;
begin
  Result := 'A benchmark that tests high speed Move with SSE. '
    + 'Gives bonus for 16 byte aligned blocks. '
    + 'Benchmark submitted by Dennis Kjaer Christensen.';
end;

class function TMoveThreads2.GetBenchmarkName: string;
begin
  Result := 'Move Benchmark2 4 arrays at a time';
end;

class function TMoveThreads2.GetCategory: TBenchmarkCategory;
begin
  Result := bmMemoryAccessSpeed;
end;

class function TMoveThreads2.GetSpeedWeight: Double;
begin
  Result := 0.9;
end;

procedure TMoveThreads2.RunBenchmark;
var
  MoveThread2 : TMoveThread2;

begin
  inherited;
  MoveThread2 := TMoveThread2.Create(True);
  MoveThread2.FreeOnTerminate := False;
  MoveThread2.FBenchmark := Self;
  MoveThread2.Resume;
  MoveThread2.WaitFor;
  MoveThread2.Free;
end;

end.
