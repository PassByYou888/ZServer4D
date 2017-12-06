unit MMUseMemory;

interface

{Leaks some memory}
function LeakSomeMemory: Boolean;
{Exercises the MM}
function UseSomeMemory: Boolean;

implementation

// ----------------------------------------------------------------------------
function LeakSomeMemory: Boolean;
var
  i, j: integer;
  P: Pointer;
begin
  Result := False;
  try
    {Allocate a couple of blocks and never free them}
    j := 10;
    for i := 1 to 7 do
    begin
      GetMem(P, j);
      j := j * 10;
    end;

    {"Use" P to suppress a compiler warning}
    if P <> nil then;

    Result := True;
  except
  end;
end;

// ----------------------------------------------------------------------------
function UseSomeMemory: Boolean;
// this code is taken from the RawPerformanceBenchmark
const
  POINTERS = 16361; // take prime just below 16384
  MAXCHUNK = 1024;  // take power of 2
var
  i, n, Size: Cardinal;
  s: array [0..POINTERS - 1] of string;
begin
  Result := False;
  try
    n := Low(s);
    for i := 1 to 512 * 1024 do begin
      if i and $FF < $F0 then         // 240 times out of 256 ==> chunk < 1 kB
        Size := (4 * i) and (MAXCHUNK-1) + 1
      else if i and $FF <> $FF then   //  15 times out of 256 ==> chunk < 32 kB
        Size := 2 * n + 1
      else                            //   1 time  out of 256 ==> chunk < 256 kB
        Size := 16 * n + 1;
      s[n] := '';
      SetLength(s[n], Size);
      // start and end of string are already assigned, let's do the middle
      s[n][Size+1 shr 1] := #1;
      Inc(n);
      if n > High(s) then
        n := Low(s);
    end;

    for n := Low(s) to High(s) do
      s[n] := '';

    Result := True;
  except
  end;
end;

end.
