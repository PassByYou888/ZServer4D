{****************************************************************************************

  SAPMM v1.01 /17.06.2013/

****************************************************************************************}

unit SAPRelease;

interface

uses SAPDefs;

function TryToReleaseAllOSBlocks(mm: PSAPThreadMM): Boolean;

implementation

uses SAPOSBlocks, SAPSmallBlocks, SAPList;

procedure ReleaseFreeSmallBlocks(mm: PSAPThreadMM); forward;

function TryToReleaseOSBlock(mm: PSAPThreadMM; os: PSAPOSBlock): Boolean; forward;

function TryToReleaseAllOSBlocks(mm: PSAPThreadMM): Boolean;
var
  x: PSAPOSBlock;
  next: PSAPOSBlock;
begin
  Result := True;

  ReleaseFreeSmallBlocks(mm);

  x := mm.os_blocks;
  if x = nil then 
    Exit;

  while x <> nil do
  begin
    next := x.next;

    TryToReleaseOSBlock(mm, x);

    x := next;
  end;

  Result := mm.os_blocks = nil;
end;

function TryToReleaseOSBlock(mm: PSAPThreadMM; os: PSAPOSBlock): Boolean;
const
  cOSFree = [sap_os_block_first, sap_os_block_last];
var
  x: PSAPAllocatedBlock;
begin
  Result := False;

  x := Pointer(LongWord(os) + SizeOf(TSAPOSBlock));

  if sap_allocated in x.tags then 
    Exit;

  if x.tags * cOSFree <> cOSFree then 
    Exit;

  UntieFreeBlock(mm.free_blocks, PSAPFreeBlock(x));

  // Whole block is free
  SAPFreeOSBlock(mm, os);
end;

//------------------------------------------------------------

procedure ReleaseFreeSmallBlocks(mm: PSAPThreadMM);
var
  i: Integer;
  x: PSAPSmallBlocksHeader;
  next: PSAPSmallBlocksHeader;
begin
  for i := Low(mm.small_table) to High(mm.small_table) do
  begin
    x := mm.small_table[i].headers;
    while x <> nil do
    begin
      next := x.next;
      if x.free_count = x.blocks_no then 
        FreeSmallBlocks(mm, x);
      x := next;
    end;
  end;
end;

end.
