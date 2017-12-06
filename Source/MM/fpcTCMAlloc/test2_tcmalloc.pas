program test2;

uses fpc_tcmalloc;

var m: array[1..4096] of PChar;
    i: integer;

begin
  for i := 0 to Length(m) do
  begin
    m[i] := GetMem(1024*1024*1024)
  end;

  for i := Length(m) downto 0 do
  begin
    FreeMem(m[i]);
  end;
end.

