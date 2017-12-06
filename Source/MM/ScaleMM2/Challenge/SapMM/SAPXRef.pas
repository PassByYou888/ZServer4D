{****************************************************************************************

  SAPMM v1.01 /17.06.2013/

  Map file routines (for extended memory leak report)

****************************************************************************************}

unit SAPXRef;

interface

// Attaches map file
procedure AttachMapFile;

// Founds unit name and line number by code offset
procedure GetXRef(ofs: LongWord; var name: string; var no: Integer);

implementation

uses SAPUtils;
//StrUtils, SysUtils, Classes;

type
  TSList = array of string;
  TRef = record
    unit_inx: Integer;
    line_no: Integer;
    code_ofs: LongWord;
  end;

var
  attached: Boolean;
  units: TSList;
  unit_count: Integer;

  refs: array of TRef;
  ref_count: Integer;

  list: TSList;
  lcount: Integer;

//-----------------------------------------------

procedure SClear(var list: TSList; var count: Integer);
begin
  SetLength(list, 0);
  count := 0;
end;

function SAdd(var list: TSList; var count: Integer; const s: string): Integer;
begin
  if count = Length(list) then
  begin
    SetLength(list, Length(list) + 10000);
  end;

  list[count] := s;
  Result := count;

  Inc(count);
end;


function SIndexOf(list: TSList; count: Integer; const s: string): integer;
  var i: integer;
begin
  Result := -1;

  for i := 0 to count - 1 do begin
    if SameStr(list[i], s) then
    begin
      Result := i;
      Exit;
    end;
  end;
end;


//-----------------------------------------------

function FindStringFrom(start: string): Integer;
forward;

function ParseUnitSection(var lno: Integer): Boolean;
forward;

procedure AttachMapFile;
  var
    exe_name: string;
    map_name: string;
    MapFile: TextFile;
    s: string;
    lno: Integer;
begin
  if attached then 
    Exit;

  SClear(units, unit_count);

  SetLength(refs, 0);
  ref_count := 0;

  exe_name := ParamStr(0);
  map_name := Copy(exe_name, 1, Length(exe_name) - 3 ) + 'map';

  if not FileExists(map_name) then 
    Exit;

  AssignFile(MapFile, map_name);
  Reset(MapFile);

  SClear(list, lcount);

  while not EOF(MapFile) do
  begin
    readln(MapFile, s);
    SAdd(list, lcount, s);
  end;

  lno := FindStringFrom('Line numbers for');
  if lno >= 0 then
  begin
    while ParseUnitSection(lno) do;
  end;

  SClear(list, lcount);

  attached := True;
end;


function FindStringFrom(start: string): Integer;
var
  i: Integer;
begin
  for i := 0 to lcount - 1 do
  begin
    if StartsStr(start, list[i]) then
    begin
      Result := i;
      Exit;
    end;
  end;

  Result := -1;
end;

function ParseUnitSection(var lno: Integer): Boolean;

  function ParseUnitHeader(s: string): Integer;
  var
    pp: Integer;
    n: Integer;
    name: string;
  begin
    Result := 0;

    pp := Pos(' segment', s);
    if pp = 0 then 
      Exit;

    n := Length('Line numbers for ');

    name := Copy(s, n + 1, pp - n - 1);

    Result := SIndexOf(units, unit_count, name);
    if Result >= 0 then 
      Exit;

    Result := SAdd(units, unit_count, name);
  end;

  function GetWord(const s: string; len: Integer; var i: Integer; var w: string): Boolean;
  var
    bp: Integer;
  begin
    Result := False;

    while (i <= len) and (s[i] = ' ') do Inc(i);

    if i = len then 
      Exit;

    bp := i;
    while (i <= len) and (s[i] <> ' ') do Inc(i);

    w := Copy(s, bp, i - bp);

    Inc(i);

    Result := Length(w) > 0;
  end;

  function GetOffset(w: string): LongWord;
  var
    i: Integer;
    c: Char;
    n: Integer;
  begin
    Result := 0;

    i := Pos(':', w);
    if i < 0 then 
      Exit;
    Inc(i);

    while i <= Length(w) do
    begin
      c := w[i];
      if (c >= '0') and (c <= '9') then n := Ord(c) - Ord('0')
      else if (c >= 'A') and (c <= 'F') then n := Ord(c) - Ord('A') + 10
      else n := 0
      ;

      Result := Result * 16 + n;
      Inc(i);
    end;
  end;

  procedure AddRefToList(unit_inx, line_no: Integer; ofs: LongWord);
  begin
    if ref_count = Length(refs) then
    begin
      SetLength(refs, Length(refs) + 10000);
    end;

    refs[ref_count].unit_inx := unit_inx;
    refs[ref_count].line_no := line_no;
    refs[ref_count].code_ofs := ofs;

    Inc(ref_count);
  end;

  function AddRefsFromLine(s: string; unit_inx: Integer): Boolean;
  var
    len: Integer;
    i: Integer;
    w: string;
    line_no: Integer;
    ofs: Integer;
  begin
    Result := False;
    len := Length(s);

    if len = 0 then 
      Exit;

    Result := True;

    i := 1;
    while True do
    begin
      if not GetWord(s, len, i, w) then 
        Exit;

      line_no := StrToInt(w);

      if not GetWord(s, len, i, w) then 
        Exit;

      ofs := GetOffset(w);

      AddRefToList(unit_inx, line_no, ofs);

    end;

  end;

  procedure AddRefs(unit_inx: Integer; var lno: Integer);
  begin
    while true do
    begin
      if not AddRefsFromLine(list[lno], unit_inx) then 
        Exit;

      Inc(lno);
    end;
  end;

var
  unit_inx: Integer;

begin
  Result := false;

  if not StartsStr('Line numbers for', list[lno]) then 
    Exit;

  unit_inx := ParseUnitHeader(list[lno]);

  Inc(lno, 2);

  AddRefs(unit_inx, lno);
  Inc(lno);

  Result := lno < lcount;
end;

//--------------------------------------------------

procedure GetXRef(ofs: LongWord; var name: string; var no: Integer);
var
  i: Integer;
begin
  name := '';
  no := 0;

  for i := 0 to ref_count - 1 do
  begin
    if refs[i].code_ofs >= ofs then
    begin
      name := units[refs[i].unit_inx];
      no := refs[i].line_no;
      Exit;
    end;
  end;
end;

initialization
  attached := False;
finalization
end.
