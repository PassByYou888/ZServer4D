unit SingleFPBenchmark1Unit;

interface

uses Windows, BenchmarkClassUnit, Classes, Math;

type

  TSingleFPThreads = class(TFastcodeMMBenchmark)
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

  TSingleFPThread = class(TThread)
     FBenchmark: TFastcodeMMBenchmark;
     procedure Execute; override;
  end;

  TRegtangularComplexS = packed record
   RealPart, ImaginaryPart : Single;
  end;

//Loading some Single values

procedure TestFunction(var Res : TRegtangularComplexS; const X, Y : TRegtangularComplexS);
begin
 Res.RealPart := X.RealPart + Y.RealPart
               + X.RealPart + Y.RealPart
               + X.RealPart + Y.RealPart
               + X.RealPart + Y.RealPart
               + X.RealPart + Y.RealPart
               + X.RealPart + Y.RealPart
               + X.RealPart + Y.RealPart;
 Res.ImaginaryPart := X.ImaginaryPart + Y.ImaginaryPart
                    + X.ImaginaryPart + Y.ImaginaryPart
                    + X.ImaginaryPart + Y.ImaginaryPart
                    + X.ImaginaryPart + Y.ImaginaryPart
                    + X.ImaginaryPart + Y.ImaginaryPart
                    + X.ImaginaryPart + Y.ImaginaryPart
                    + X.ImaginaryPart + Y.ImaginaryPart;
end;

procedure TSingleFPThread.Execute;
var
 I1, I2, I5: Integer;
 Src1Array1 : array of TRegtangularComplexS;
 Src2Array1 : array of TRegtangularComplexS;
 ResultArray1 : array of TRegtangularComplexS;
 Src1Array2 : array of TRegtangularComplexS;
 Src2Array2 : array of TRegtangularComplexS;
 ResultArray2 : array of TRegtangularComplexS;
 BenchArraySize, PrevArraySize : Integer;
const
 MINBENCHARRAYSIZE : Integer = 1000;
 MAXBENCHARRAYSIZE : Integer = 6000;
 ARRAYSIZEINCREMENT = 53;
 NOOFRUNS : Integer = 500;

begin
 PrevArraySize := 0;
 BenchArraySize := MINBENCHARRAYSIZE;
 while BenchArraySize < MAXBENCHARRAYSIZE do
  begin
   SetLength(Src1Array1, BenchArraySize);
   SetLength(Src2Array1, BenchArraySize);
   SetLength(ResultArray1, BenchArraySize);
   SetLength(Src1Array2, BenchArraySize);
   SetLength(Src2Array2, BenchArraySize);
   SetLength(ResultArray2, BenchArraySize);
   FBenchmark.UpdateUsageStatistics;
   //Fill source arrays
   for I1 := PrevArraySize to BenchArraySize-1 do
    begin
     Src1Array1[I1].RealPart := 1;
     Src1Array1[I1].ImaginaryPart := 1;
     Src2Array1[I1].RealPart := 1;
     Src2Array1[I1].ImaginaryPart := 1;
     Src1Array2[I1].RealPart := 1;
     Src1Array2[I1].ImaginaryPart := 1;
     Src2Array2[I1].RealPart := 1;
     Src2Array2[I1].ImaginaryPart := 1;
    end;
   //This is the real botleneck and the performance we want to measure
   for I5 := 0 to BenchArraySize-1 do
    begin
     for I2 := 0 to NOOFRUNS do
      begin
       TestFunction(ResultArray1[I5], Src1Array1[I5], Src2Array1[I5]);
       TestFunction(ResultArray2[I5], Src1Array2[I5], Src2Array2[I5]);
      end;
    end;
   PrevArraySize := BenchArraySize;
   inc(BenchArraySize, ARRAYSIZEINCREMENT);
  end;
end;

class function TSingleFPThreads.GetBenchmarkDescription: string;
begin
  Result := 'A benchmark that tests access to Single FP variables '
    + 'in a dynamic array. '
    + 'Reveals set associativity related issues.'
    + 'Benchmark submitted by Dennis Kjaer Christensen.';
end;

class function TSingleFPThreads.GetBenchmarkName: string;
begin
  Result := 'Single Variables Access 6 arrays at a time';
end;

class function TSingleFPThreads.GetCategory: TBenchmarkCategory;
begin
  Result := bmMemoryAccessSpeed;
end;

class function TSingleFPThreads.GetSpeedWeight: Double;
begin
  Result := 0.9;
end;

procedure TSingleFPThreads.RunBenchmark;
var
  SingleFPThread : TSingleFPThread;

begin
  inherited;
  SingleFPThread := TSingleFPThread.Create(True);
  SingleFPThread.FreeOnTerminate := False;
  SingleFPThread.FBenchmark := Self;
  SingleFPThread.Resume;
  SingleFPThread.WaitFor;
  SingleFPThread.Free;
end;

end.