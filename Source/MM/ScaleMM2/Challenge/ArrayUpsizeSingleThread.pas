unit ArrayUpsizeSingleThread;

interface

uses
  Windows, BenchmarkClassUnit, Classes, Math;

type
  TArrayUpsizeSingleThread = class(TFastcodeMMBenchmark)
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

{ TArrayUpsizeSingleThread }

procedure TArrayUpsizeSingleThread.Execute;
var
  i: Integer;
  x: array of Int64;
begin
  //for i := 1 to 10 * 1024 * 1024 do begin
  for i := 1 to 100 * 1024 do begin   //0 - 800kb
    SetLength(x, i);
    x[i - 1] := i;
  end;
  UpdateUsageStatistics;
end;

class function TArrayUpsizeSingleThread.GetBenchmarkDescription: string;
begin
  Result := 'Constantly resize a dynamic array in single steps upward to reproduce JclDebug behaviour when loading debug information';
end;

class function TArrayUpsizeSingleThread.GetBenchmarkName: string;
begin
  Result := 'Array Upsize 1 thread';
end;

class function TArrayUpsizeSingleThread.GetCategory: TBenchmarkCategory;
begin
  Result := bmSingleThreadRealloc;
end;

procedure TArrayUpsizeSingleThread.RunBenchmark;
begin
  inherited;
  Execute;
end;

end.
