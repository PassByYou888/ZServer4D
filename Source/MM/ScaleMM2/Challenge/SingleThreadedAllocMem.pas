{A benchmark that tests the speed and validity of allocmem}

unit SingleThreadedAllocMem;

interface

uses Windows, SysUtils, BenchmarkClassUnit, Classes, Math;

type

  TSingleThreadAllocMemBenchmark = class(TFastcodeMMBenchmark)
  public
    procedure RunBenchmark; override;
    class function GetBenchmarkName: string; override;
    class function GetBenchmarkDescription: string; override;
    class function GetCategory: TBenchmarkCategory; override;
    class function GetSpeedWeight: Double; override;
  end;

implementation

{ TSingleThreadAllocMemBenchmark }

class function TSingleThreadAllocMemBenchmark.GetBenchmarkDescription: string;
begin
  Result := 'A single-threaded benchmark that tests the speed and validity of AllocMem.';
end;

class function TSingleThreadAllocMemBenchmark.GetBenchmarkName: string;
begin
  Result := 'Single-threaded AllocMem';
end;

class function TSingleThreadAllocMemBenchmark.GetCategory: TBenchmarkCategory;
begin
  Result := bmSingleThreadAllocAndFree;
end;

class function TSingleThreadAllocMemBenchmark.GetSpeedWeight: Double;
begin
  Result := 0.5;
end;

procedure BadAllocMemError;
begin
  raise Exception.Create('AllocMem returned a block that was not zero initialized.');
end;

procedure TSingleThreadAllocMemBenchmark.RunBenchmark;
const
  RepeatCount = 100;
  PointerCount = 1000;
  MaxBlockSize = 300000;
type
  TByteArray = array[0..MaxBlockSize - 1] of Byte;
  PByteArray = ^TByteArray;
var
  i, j, k: Integer;
  LPointers: array[0..PointerCount - 1] of Pointer;
  LSize: Integer;
  LCheck: Byte;
begin
  inherited;
  {We want predictable results}
  RandSeed := 0;
  FillChar(LPointers, SizeOf(LPointers), 0);
  {FreeMem and AllocMem in a loop}
  for j := 1 to RepeatCount do
  begin
    {Update usage statistics}
    UpdateUsageStatistics;
    for i := 0 to PointerCount - 1 do
    begin
      {Free the pointer}
      FreeMem(LPointers[i]);
      {Get the size, minimum 1}
      LSize := Random(MaxBlockSize) + 1;
      {Get the pointer}
      LPointers[i] := AllocMem(LSize);
      {Read the memory}
      LCheck := 0;
      for k := 0 to LSize - 1 do
      begin
        {Check the memory}
        LCheck := LCheck or PByteArray(LPointers[i])[k];
        {Modify it}
        PByteArray(LPointers[i])[k] := byte(k);
      end;
      {Is the check valid?}
      if LCheck <> 0 then
        BadAllocMemError;
    end;
  end;
  {Free all the objects}
  for i := 0 to PointerCount - 1 do
    FreeMem(LPointers[i]);
end;

end.
