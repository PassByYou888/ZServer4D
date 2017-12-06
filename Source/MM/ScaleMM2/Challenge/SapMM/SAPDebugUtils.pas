{****************************************************************************************

  SAPMM v1.01 /17.06.2013/

  SapMM debug routines

****************************************************************************************}

unit SAPDebugUtils;

interface

{$Include SAPOptions.inc}

uses SAPDefs;

var
  output: procedure (s: string);


(*
var
  stats: array [0..999999] of
          record
            op: Char;
            size: LongWord;
            adr: Pointer;
            adr1: Pointer;
          end;
  stats_count: Integer;
*)

{$IFDEF SAP_STAT}
procedure ShowStatistics(mm: PSAPThreadMM);
procedure ShowStatisticsForMMsInList(x: PSAPThreadMM);
procedure ShowSmallBlocksStatistics(mm: PSAPThreadMM);

procedure ShowOSBlocks(mm: PSAPThreadMM);
procedure ShowFreeBlocks(mm: PSAPThreadMM);
procedure ShowSmallBlocks(mm: PSAPThreadMM);

procedure ShowAllocatedBlock(p: Pointer);

procedure CheckAndShowFreeBlock(p: PSAPFreeBlock);

function CheckOSBlocks(mm: PSAPThreadMM; show: Boolean): Boolean;

function TagsToStr(tags: TSAPTags): string;
{$ENDIF SAP_STAT}

implementation

uses Windows, SAPUtils;

{$IFDEF SAP_STAT}
procedure CalculateOSMemory(mm: PSAPThreadMM; var no, total: LongWord);
var
  x: PSAPOSBlock;
begin
  no := 0;
  total := 0;

  x := mm.os_blocks;
  while x <> nil do
  begin
    Inc(no);
    Inc(total, x.size);

    x := x.next;
  end;
end;

procedure ShowStatistics(mm: PSAPThreadMM);
var
  no, total: LongWord;
begin
  if mm = nil then 
    Exit;

  output('------------- Statistics: $' + IntToHex(LongWord(mm), 0)
        + ' thread id: ' + IntToStr(mm.thread_id)
  );
  output('Number of calls: ');
  output('  mem alloc calls: ' + IntToStr(mm.no_get)
         + ' among them:'
         + ' small blocks: ' + IntToStr(mm.no_get_small)
         + ' normal blocks: ' + IntToStr(mm.no_get_normal)
         + ' large (OS) blocks: ' + IntToStr(mm.no_get_os)
         );
  output('   alloc search cycles: ' + IntToStr(mm.no_alloc_find_cycles));

  output('  free mem calls: ' + IntToStr(mm.no_free));
  output('    freed from other threads: ' + IntToStr(mm.no_free_from_other_thread));

  output('  realloc calls: ' + IntToStr(mm.no_realloc)
         + ' among them:'
         + ' extend calls: ' + IntToStr(mm.no_realloc_extend)
         + ' shrink calls: ' + IntToStr(mm.no_realloc_shrink)
         + ' at place calls : ' + IntToStr(mm.no_realloc_at_place)
         );
  output('Bytes copied: ' + IntToStr(mm.no_realloc_copy_bytes));

  CalculateOSMemory(mm, no, total);
  output('---- OS blocks stats: '
        + ' total OS blocks allocated: ' + IntToStr(total) + ' (' + IntToStr(no) + ' блоков)'
        + ' alloc OS block calls: ' + IntToStr(mm.no_os_alloc)
        + ' free OS block calls: ' + IntToStr(mm.no_os_free)
        );
  output('---- Interthread calls stats ---- ');
  output('  interthread free mem calls: ' + IntToStr(mm.no_inter_free));
  output('  interthread realloc calls: ' + IntToStr(mm.no_inter_realloc));

//  output('------ Small blocks stats -----');
//  ShowSmallBlocksStatistics(mm);
  output('------------------------------------');
end;

procedure ShowStatisticsForMMsInList(x: PSAPThreadMM);
begin
  while x <> nil do
  begin
    ShowStatistics(x);
    output('');
    x := x.next;
  end;
end;

//--------------------------------------------------------------

procedure ShowOSBlocks(mm: PSAPThreadMM);

  procedure CheckAndShowOSBlock(p: PSAPOSBlock);
  begin

    if (p.next <> nil) and (p.next.prev <> p) then
    begin
      output('! CheckAndShowOSBlock: p.next.prev <> p');
      Exit;
    end;

    if p.prev = nil then
    begin
      if p <> mm.os_blocks then
      begin
        output('! CheckAndShowOSBlock: p.prev = nil & p <> mm.os_blocks');
        Exit;
      end
    end
    else if p.prev.next <> p then
    begin
      output('! CheckAndShowOSBlock: p.prev.next <> p');
      Exit;
    end;

    output('[' + IntToHex(LongWord(p),5) + '] ' + IntToStr(p.size));
  end;

  var
    p: PSAPOSBlock;
    no: Integer;
    total: LongWord;
begin
  if (mm = nil) or (mm.os_blocks = nil) then begin
    output('----- OS blocks not allocated ----');
    Exit;
  end;

  output('----- OS blocks ----');

  no := 0;
  total := 0;

  p := mm.os_blocks;
  while p <> nil do
  begin
    CheckAndShowOSBlock(p);
    Inc(no);
    Inc(total, p.size);
    p := p.next;
  end;

  output('total OS blocks number: ' + IntToStr(no) + ', total allocated size: ' + IntToStr(total));
end;

function TagsToStr(tags: TSAPTags): string;
begin
  Result := '';
  if sap_allocated in tags then Result := Result + ' allocated';
  if sap_small_block in tags then Result := Result + ' small_block';
  if sap_os_block_first in tags then Result := Result + ' os_block_first';
  if sap_os_block_last in tags then Result := Result + ' os_block_last';
  if sap_small_blocks_piece in tags then Result := Result + ' small_block_piece';
end;

procedure CheckAndShowFreeBlock(p: PSAPFreeBlock);
begin
  if p.next.prev <> p then
  begin
    output('! CheckAndShowFreeBlock: p.next.prev <> p');
  end;

  if p.prev.next <> p then
  begin
    output('! CheckAndShowFreeBlock: p.prev.next <> p');
  end;

  output('[' + IntToHex(LongWord(p),5) + '] ' + IntToStr(p.block.size) + ' ' + TagsToStr(p.block.tags));
end;

procedure ShowFreeBlocks(mm: PSAPThreadMM);
  var
    p: PSAPFreeBlock;
    no: Integer;
    total: LongWord;
begin
  if mm.os_blocks = nil then begin
    output('----- no free blocks ----');
    Exit;
  end;

  output('----- free blocks ----');

  no := 0;
  total := 0;

  p := mm.free_blocks;
  repeat
    CheckAndShowFreeBlock(p);
    Inc(no);
    Inc(total, p.block.size);
    p := p.next;
  until p = mm.free_blocks;

  output('Total free blocks number: ' + IntToStr(no) + ', total free blocks size: ' + IntToStr(total));
end;

procedure ShowAllocatedBlock(p: Pointer);
var
  small: PSAPSmallAllocatedBlock;
  block: PSAPAllocatedBlock;
begin
  if p = nil then
    Exit;

  small := Pointer(LongWord(p) - SizeOf(TSAPSmallAllocatedBlock));
  if sap_small_block in small.tags then
  begin
    output('small[' + IntToHex(LongWord(p),5) + '] ' {+ IntToStr(block.size)} + ' ' + TagsToStr(small.tags));

    Exit;
  end;

  block := Pointer(LongWord(p) - SizeOf(TSAPAllocatedBlock));
  output('[' + IntToHex(LongWord(p),5) + '] ' + IntToStr(block.size) + ' ' + TagsToStr(block.tags));
end;


//------------------------------------------------------

function CheckOneOSBlock(mm: PSAPThreadMM; os: PSAPOSBlock; show: Boolean): Boolean;
forward;

function CheckSmallBlocksPiece(mm: PSAPThreadMM; h: PSAPSmallBlocksHeader; inx: Integer; var s: string): Boolean;
forward;

function CheckMediumBlocks(mm: PSAPThreadMM; show: Boolean): Boolean;
forward;

// Checks one by one all OS blocks
// For each block checks the correctness of division into smaller blocks
// (must have correct size at the begging and at the end of each block)
function CheckOSBlocks(mm: PSAPThreadMM; show: Boolean): Boolean;
var
  x: PSAPOSBlock;
begin
  Result := True;
  if mm = nil then 
    Exit;

  x := mm.os_blocks;
  if x = nil then 
    Exit;

  while x <> nil do
  begin
    Result := CheckOneOSBlock(mm, x, show);
    if not Result then 
      Exit;

    x := x.next;
  end;

  CheckMediumBlocks(mm, show);
end;

function CheckOneOSBlock(mm: PSAPThreadMM; os: PSAPOSBlock; show: Boolean): Boolean;

  function FindInFreeList(mm: PSAPThreadMM; f: PSAPFreeBlock): Boolean;
  var
    list: PSAPFreeBlock;
    x: PSAPFreeBlock;
  begin
    if f.block.size <= cMaxMediumBlock then
      list := mm.medium_table[f.block.size div cMediumBlockStep]
    else
      list := mm.free_blocks;

    if list = nil then
      ShowAllocatedBlock(Pointer(f));

    x := list;
    repeat
      if x = f then
      begin
        Result := True;
        Exit;
      end;
      if x = nil then
      begin
        Result := False;
        Exit;
      end;

      x := x.next;
    until x = list;

    Result := False;
  end;

  function CheckFree(f: PSAPFreeBlock; var s: string): Boolean;
  begin
    Result := False;

    if f.next = nil then
    begin
      s := 'error in list: f.next = nil';
      Exit;
    end;

    if f.prev = nil then
    begin
      s := 'error in list: f.prev = nil';
      Exit;
    end;

    if f.next.prev <> f then
    begin
      s := 'error in list: f.next.prev <> f';
      Exit;
    end;

    if f.prev.next <> f then
    begin
      s := 'error in list: f.prev.next <> f';
      Exit;
    end;

    Result := FindInFreeList(mm, f);
    if not Result then
    begin
      s := 'free blocks not found in free blocks list, size=' + IntToStr(f.block.size);
      Exit;
    end;

    Result := True;
  end;

  function CheckBlock(x: PSAPAllocatedBlock; stop: LongWord; var s: string): Boolean;
  var
    psize: ^ LongWord;
  begin
    Result := False;

    if x.magic <> cMagic then
    begin
      s := 'wrong magic number in block header (block): ' + IntToHex(x.magic, 2);
      Exit;
    end;

    if x.block_magic <> cNormalBlockMagic then
    begin
      s := 'wrong magic number in block header (block_magic): ' + IntToHex(x.block_magic, 0);
      Exit;
    end;

    if x.size and (cAlignment - 1) <> 0 then
    begin
      s := 'wrong alignment at the beginning of the block: ' + IntToStr(x.size);
      Exit;
    end;

    if LongWord(x) + x.size > stop then
    begin
      s := 'wrong blocks size: exceeds the limit of OS block: ' + IntToStr(x.size);
      Exit;
    end;

    psize := Pointer(LongWord(x) + x.size - SizeOf(LongWord));

    if psize^ <> x.size then
    begin
      s := 'the size at the end of the block ' + IntToStr(psize^) + ' does not match the size at the beginning of the block: ' + IntToStr(x.size);
      Exit;
    end;

    if not (sap_allocated in x.tags) then
    begin
      Result := CheckFree(PSAPFreeBlock(x), s);
      Exit;
    end;

    if sap_small_blocks_piece in x.tags then
    begin
      Result := CheckSmallBlocksPiece(mm, Pointer(LongWord(x) + SizeOf(TSAPAllocatedBlock)), -1, s);
      Exit;
    end;

    Result := True;
  end;

  function PrepareSmallBlocksPieceInfo(x: PSAPAllocatedBlock): string;
  var
    h: PSAPSmallBlocksHeader;
  begin
    Result := '';
    if not (sap_small_blocks_piece in x.tags) then Exit;

    h := Pointer(LongWord(x) + SizeOf(TSAPAllocatedBlock));

    Result := ' <size=' + IntToStr(h.block_size)
            + ' free=' + IntToStr(h.free_count) + '>'
            ;
  end;


var
  x: PSAPAllocatedBlock;
  stop: LongWord;
  s: string;
begin
  if show then
    output('----- CheckOSBlock: ' + IntToHex(LongWord(os),5));

  x := Pointer(LongWord(os) + SizeOf(TSAPOSBlock));
  stop := LongWord(os) + os.size;
  while LongWord(x) < stop do
  begin

    if show then
      output('  check: ['
          + IntToHex(LongWord(x),5) + '] '
          + IntToStr(x.size) + ' '
          + TagsToStr(x.tags)
          + PrepareSmallBlocksPieceInfo(x)
          );

    Result := CheckBlock(x, stop, s);
    if not Result then
    begin
      output('!Ошибка [' + IntToHex(LongWord(x),5) + '] ' + s + ' ' + TagsToStr(x.tags));
      Exit;
    end;

    x := Pointer(LongWord(x) + x.size);
  end;

  Result := True;
end;

//----------------------------------------
procedure ShowSmallBlocksForSize(mm: PSAPThreadMM; inx: Integer);
forward;

procedure CheckAndShowSmallBlocksPiece(mm: PSAPThreadMM; h: PSAPSmallBlocksHeader; inx: Integer);
forward;

//-----------------------------------------

procedure ShowSmallBlocksStatistics(mm: PSAPThreadMM);

  procedure ShowSmallBlocksStatisticsForSize(mm: PSAPThreadMM; inx: Integer);
  var
    h: PSAPSmallBlocksHeader;
    free, all: LongWord;
    pieces: LongWord;
    size: LongWord;
  begin
    h := mm.small_table[inx].headers;
    if h = nil then 
      Exit;

    free := 0;
    all := 0;
    pieces := 0;
    size := h.block_size;

    while h <> nil do
    begin
      Inc(free, h.free_count);
      Inc(all, h.blocks_no);
      Inc(pieces);

      h := h.next;
    end;

    output(IntToStr(size) + ': '
      + IntToStr(pieces) + ' pieces,'
      + ' free ' + IntToStr(free) + ' from total of ' + IntToStr(all));

  end;

var
  i: Integer;
begin
  for i := Low(mm.small_table) to High(mm.small_table) do
  begin
    ShowSmallBlocksStatisticsForSize(mm, i);
  end;
end;

//-----------------------------------------

procedure ShowSmallBlocks(mm: PSAPThreadMM);
var
  i: Integer;
begin
  for i := Low(mm.small_table) to High(mm.small_table) do
  begin
    ShowSmallBlocksForSize(mm, i);
  end;
end;

procedure ShowSmallBlocksForSize(mm: PSAPThreadMM; inx: Integer);
var
  h: PSAPSmallBlocksHeader;
begin
  h := mm.small_table[inx].headers;
  if h = nil then 
    Exit;

  output('---- small blocks with size of: ' + IntToStr((inx + 1) * cSmallBlockStep));

  while h <> nil do
  begin
    CheckAndShowSmallBlocksPiece(mm, h, inx);
    h := h.next;
  end;

end;

procedure CheckAndShowSmallBlocksPiece(mm: PSAPThreadMM; h: PSAPSmallBlocksHeader; inx: Integer);
var
  s: string;
begin
  output('h[' + IntToHex(LongWord(h),0) + '] '
    + 'free ' + IntToStr(h.free_count) + ' from ' + IntToStr(h.blocks_no));

  if not CheckSmallBlocksPiece(mm, h, inx, s) then
  begin
    output('!Error in group of small blocks [' + IntToHex(LongWord(h),0) + '] ' + s);
  end;
end;

function CheckSmallBlocksPiece(mm: PSAPThreadMM; h: PSAPSmallBlocksHeader; inx: Integer; var s: string): Boolean;

  function CalculateFreeBlocks(h: PSAPSmallBlocksHeader): LongWord;
  var
    no: Integer;
    f: PSAPSmallFreeBlock;
  begin
    no := 0;
    f := h.blocks;

    while f <> nil do
    begin
      Inc(no);
      f := f.next;
    end;

    Result := no;
  end;

  function FindBlockInList(mm: PSAPThreadMM; h: PSAPSmallBlocksHeader): Boolean;
  var
    inx: Integer;
    x: PSAPSmallBlocksHeader;
  begin
    inx := h.block_size div cSmallBlockStep - 1;
    x := mm.small_table[inx].headers;

    while x <> nil do
    begin
      if x = h then
      begin
        Result := true;
        Exit;
      end;

      x := x.next;
    end;

    Result := false;
  end;

  procedure ShowHeadersList(h: PSAPSmallBlocksHeader);
  begin
    while h <> nil do
    begin

      output(
          IntToStr(h.block_size) + ': '
        + '<' + IntToHex(LongWord(h.prev),8)
        + ' [' + IntToHex(LongWord(h),8) + ']'
        + ' >' + IntToHex(LongWord(h.next),8)
        + ' list: ' + IntToHex(LongWord(h.blocks),8)
        + ' free: ' + IntToStr(h.free_count)
      );

      h := h.next;
    end;
  end;

var
  no: Integer;
begin
  Result := false;

  if h.header_magic <> cSmallBlockHeaderMagic then
  begin
    s := 'wrong magic number in block header: ' + IntToHex(h.header_magic, 0);
    Exit;
  end;

  if inx >= 0 then
  begin
    if h.block_size <> (inx + 1) * cSmallBlockStep then
    begin
      s := 'wrong block size in block header: ' + IntToStr(h.block_size)
        + ' must be: ' + IntToStr((inx + 1) * cSmallBlockStep);
      Exit;
    end;
  end;

  no := CalculateFreeBlocks(h);
  if no <> h.free_count then
  begin
    s := 'wrong number of small blocks: ' + IntToStr(h.free_count)
        + ' in list: ' + IntToStr(no);
    Exit;
  end;

  // list check
  if h.free_count = 0 then
  begin
    // block must not be in list
    if FindBlockInList(mm, h) then
    begin
      s := 'fully used piece exists in list';
      Exit;
    end;

    Result := true;
    Exit;
  end;

  if not FindBlockInList(mm, h) then
  begin
    s := 'piece with free blocks not found in list';
    Exit;
  end;

  if h.prev = nil then
  begin
    inx := h.block_size div cSmallBlockStep - 1;

    if h <> mm.small_table[inx].headers then
    begin
      s := 'h.prev = nil, but piece is not first in list';
      Exit;
    end;

  end
  else if h.prev.next <> h then
  begin
    s := 'h.prev.next <> h';
    Exit;
  end;

  if (h.next <> nil) and (h.next.prev <> h) then
  begin

    inx := h.block_size div cSmallBlockStep - 1;
    ShowHeadersList(mm.small_table[inx].headers);

    s := 'h.next.prev <> h';
    Exit;
  end;

  Result := true;
end;

//----------------------------------------------------------

function CheckMediumBlockList(mm: PSAPThreadMM; inx: Integer; show: Boolean): Boolean;
forward;

function CheckMediumBlocks(mm: PSAPThreadMM; show: Boolean): Boolean;
var
  i: Integer;
begin
  Result := false;

  for i := Low(mm.medium_table) to High(mm.medium_table) do
  begin
    if mm.medium_table[i] <> nil then
    begin
      if not CheckMediumBlockList(mm, i, show) then 
        Exit;
    end;
  end;

  Result := true;
end;

function CheckMediumBlockList(mm: PSAPThreadMM; inx: Integer; show: Boolean): Boolean;

  function CheckOneMedium(x: PSAPFreeBlock; var s: string): Boolean;
  var
    psize: ^ LongWord;
  begin
    Result := false;

    if x.block.magic <> cMagic then
    begin
      s := 'wrong magic number in header (block): ' + IntToHex(x.block.magic, 2);
      Exit;
    end;

    if x.block.block_magic <> cNormalBlockMagic then
    begin
      s := 'wrong magic number in header (block_magic): ' + IntToHex(x.block.block_magic, 0);
      Exit;
    end;

    if x.block.size and (cAlignment - 1) <> 0 then
    begin
      s := 'wrong alignment at the beginning of the block: ' + IntToStr(x.block.size);
      Exit;
    end;

(*
    if LongWord(x) + x.size > stop then
    begin
      s := 'wrong block size (OS block size limit exceeded): ' + IntToStr(x.size);
      Exit;
    end;
*)

    psize := Pointer(LongWord(x) + x.block.size - SizeOf(LongWord));

    if psize^ <> x.block.size then
    begin
      s := 'size at the end of the blocks ' + IntToStr(psize^) + ' not matches the size at the beginning of the block: ' + IntToStr(x.block.size);
      Exit;
    end;

    if sap_allocated in x.block.tags then
    begin
      s := 'must be free block ';
      Exit;
    end;

    if x.next = nil then
    begin
      s := 'error in list: x.next = nil';
      Exit;
    end;

    if x.prev = nil then
    begin
      s := 'error in list: x.prev = nil';
      Exit;
    end;

    if x.next.prev <> x then
    begin
      s := 'error in list: x.next.prev <> x';
      Exit;
    end;

    if x.prev.next <> x then
    begin
      s := 'error in list: x.prev.next <> x';
      Exit;
    end;

    Result := true;
  end;

var
  list: PSAPFreeBlock;
  x: PSAPFreeBlock;
  s: string;
begin
  Result := false;

  if show then
    output(
      '----  medium blocks --- N' + IntToStr(inx) + ': ' + IntToStr(inx * cMediumBlockStep)
      );

  list := mm.medium_table[inx];

  x := list;
  repeat
    if not CheckOneMedium(x, s) then begin
      output('!Error medium block [' + IntToHex(LongWord(x),0) + '] ' + s + ' ' + TagsToStr(x.block.tags));
      Exit;
    end;

    x := x.next;
  until x = list;

  Result := true;
end;
{$ENDIF SAP_STAT}

end.
