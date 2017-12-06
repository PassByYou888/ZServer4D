{****************************************************************************************

  SAPMM v1.01 /17.06.2013/

  Memory leaks reporting unit

****************************************************************************************}

unit SAPReportMemoryLeaks;

interface

{$Include SAPOptions.inc}

// Saving IP does not work when optimization is ON
{$OPTIMIZATION OFF}
{$STACKFRAMES  ON}

uses SAPDefs;

{$IFDEF SAP_STATIP}
procedure SaveIP(var ips: TIPs);
{$ENDIF}

{$IFDEF SAP_MEMORYLEAKS}
type
  TOutputProc = procedure (s: string);

procedure PrepareMemoryLeaksReport(mm: PSAPThreadMM; output: TOutputProc);
{$ENDIF SAP_MEMORYLEAKS}

implementation


{$IFDEF SAP_MEMORYLEAKS}

uses SAPXRef, SAPDebugUtils, SAPUtils;

var
  line_no: Integer;

//------------------------------------------------------

procedure ReportForOneOSBlock(os: PSAPOSBlock; output: TOutputProc);
forward;

procedure ReportForBlock(x: PSAPAllocatedBlock; output: TOutputProc);
forward;

procedure ReportForSmallBlocks(block: PSAPAllocatedBlock; output: TOutputProc);
forward;

procedure AddToReport(p: LongWord; tags: TSAPTags; size: LongWord;
{$IFDEF SAP_STATIP}
          const ips: TIPs;
{$ENDIF}
          output: TOutputProc);
forward;

//------------------------------------------------------

procedure PrepareMemoryLeaksReport(mm: PSAPThreadMM; output: TOutputProc);
var
  x: PSAPOSBlock;
begin
{$IFDEF SAP_SHOWCALLSTACK}
  AttachMapFile;
{$ENDIF}

  line_no := 0;

  if mm = nil then 
    Exit;

  x := mm.os_blocks;
  if x = nil then 
    Exit;

  while x <> nil do
  begin
    ReportForOneOSBlock(x, output);

    x := x.next;
  end;
end;

procedure ReportForOneOSBlock(os: PSAPOSBlock; output: TOutputProc);
var
  x: PSAPAllocatedBlock;
  stop: LongWord;
begin
  x := Pointer(LongWord(os) + SizeOf(TSAPOSBlock));
  stop := LongWord(os) + os.size;

  while LongWord(x) < stop do
  begin

    ReportForBlock(x, output);

    x := Pointer(LongWord(x) + x.size);
  end;

end;

procedure ReportForBlock(x: PSAPAllocatedBlock; output: TOutputProc);
begin
  if not (sap_allocated in x.tags) then 
    Exit;

  if sap_small_blocks_piece in x.tags then
  begin
    ReportForSmallBlocks(x, output);
    Exit;
  end;

  AddToReport(LongWord(x) + SizeOf(TSAPAllocatedBlock), x.tags, x.size,
{$IFDEF SAP_STATIP}
    x.ips,
{$ENDIF}
    output);

end;

procedure ReportForSmallBlocks(block: PSAPAllocatedBlock; output: TOutputProc);
var
  h: PSAPSmallBlocksHeader;
  x: PSAPSmallAllocatedBlock;
  i: Integer;
begin
  h := Pointer(LongWord(block) + SizeOf(TSAPAllocatedBlock));
  assert(h.header_magic = cSmallBlockHeaderMagic);

  x := Pointer(LongWord(h) + SizeOf(TSAPSmallBlocksHeader));

  for i := 1 to h.blocks_no do
  begin

    assert(x.magic = cMagic);

    if sap_allocated in x.tags then
    begin
      AddToReport(LongWord(x) + SizeOf(TSAPSmallAllocatedBlock), x.tags, h.block_size,
{$IFDEF SAP_STATIP}
        x.ips,
{$ENDIF}
        output);

    end;

    x := Pointer(LongWord(x) + h.block_size);
  end;

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

procedure AddToReport(p: LongWord; tags: TSAPTags; size: LongWord;
{$IFDEF SAP_STATIP}
          const ips: TIPs;
{$ENDIF SAP_STATIP}
          output: TOutputProc);

const
  code_start = $401000; // standard
var
  s: string;
{$IFDEF SAP_SHOWCALLSTACK}
  i: Integer;
  name: string;
  no: Integer;
{$ENDIF SAP_SHOWCALLSTACK}
begin
  if sap_used_in_leaks_reporting in tags then 
    Exit;

  Inc(line_no);

  s := IntToStr(line_no) + ') '
      + IntToHex(p,8) + ': '
      + 'size=' + IntToStr(size)
      + TagsToStr(tags)
      ;

  {$IFDEF SAP_STATIP}
  s := s + ' [';

  for i := Low(ips) to High(ips) do
  begin
    if ips[i] = 0 then break;

    s := s + ' ' + IntToHex(ips[i] - code_start, 8);
  end;

  s := s + ']';
  {$ENDIF SAP_STATIP}

  output(s);

  {$IFDEF SAP_SHOWCALLSTACK}

  for i := Low(ips) to High(ips) do
  begin
    if ips[i] = 0 then break;

    GetXRef(ips[i] - code_start, name, no);
    s := name + ' ' + IntToStr(no);
    output(s);
  end;

  {$ENDIF SAP_SHOWCALLSTACK}
end;

{$ENDIF}

//--------------------------------------------------

{$IFDEF SAP_STATIP}

procedure GetStackRange(var stack_base, stack_pointer: LongWord);
asm
  mov ecx, fs:[4]
  mov [eax], ecx
  mov [edx], ebp
end;

procedure SaveIP(var ips: TIPs);
type PLongWord = ^ LongWord;
var
  stack_base, stack_pointer: LongWord;
  cur_bp: LongWord;
  prev_bp: LongWord;
  p_ip: PLongWord;
  i: Integer;
begin
  asm
    mov cur_bp, ebp;
  end;

  GetStackRange(stack_base, stack_pointer);

  i := -1;

  while (i <= High(ips))
    and (cur_bp >= stack_pointer)
    and (cur_bp < stack_base) do
  begin

    if i >= Low(ips) then
      ips[i] := PLongWord(LongWord(cur_bp) + 4)^;

    Inc(i);
    cur_bp := PLongWord(cur_bp)^

  end;

  while i <= High(ips) do begin ips[i] := 0; Inc(i); end;

end;
{$ENDIF SAP_MEMORYLEAKS}

end.
