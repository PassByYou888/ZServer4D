{****************************************************************************************

  SAPMM v1.01 /17.06.2013/

  SapMM list routines

****************************************************************************************}

unit SAPList;

interface

uses SAPDefs;

procedure TieFreeBlock(var list: PSAPFreeBlock; x: PSAPFreeBlock);
procedure UntieFreeBlock(var list: PSAPFreeBlock; x: PSAPFreeBlock);

procedure TieFreeBlockToSizeList(mm: PSAPThreadMM; x: PSAPFreeBlock; size: LongWord); inline;
procedure UntieFreeBlockFromSizeList(mm: PSAPThreadMM; x: PSAPFreeBlock; size: LongWord); inline;

procedure MoveFreeBlockToSizeList(mm: PSAPThreadMM; x: PSAPFreeBlock; pred_size: LongWord); inline;

implementation

procedure TieFreeBlockToSizeList(mm: PSAPThreadMM; x: PSAPFreeBlock; size: LongWord);
begin
  if size <= cMaxMediumBlock then
    TieFreeBlock(mm.medium_table[size div cMediumBlockStep], x)
  else
    TieFreeBlock(mm.free_blocks, x);
end;

procedure UntieFreeBlockFromSizeList(mm: PSAPThreadMM; x: PSAPFreeBlock; size: LongWord);
begin
  if size <= cMaxMediumBlock then
    UntieFreeBlock(mm.medium_table[size div cMediumBlockStep], x)
  else
    UntieFreeBlock(mm.free_blocks, x);
end;

procedure MoveFreeBlockToSizeList(mm: PSAPThreadMM; x: PSAPFreeBlock; pred_size: LongWord);
begin
  // If block already was in _large_ blocks list, it stays there, because after merge the block may only grow
  if pred_size > cMaxMediumBlock then 
    Exit;

  UntieFreeBlockFromSizeList(mm, x, pred_size);
  TieFreeBlockToSizeList(mm, x, x.block.size);
end;

//-----------------------------------------------

procedure TieFreeBlock(var list: PSAPFreeBlock; x: PSAPFreeBlock);
begin
  if list = nil then
  begin
    list := x;
    x.next := x;
    x.prev := x;

    Exit;
  end;

  x.next := list;
  x.prev := list.prev;

  x.next.prev := x;
  x.prev.next := x;
end;

procedure UntieFreeBlock(var list: PSAPFreeBlock; x: PSAPFreeBlock);
begin
  if x.next = x then
  begin
    list := nil;
    Exit;
  end;

  x.prev.next := x.next;
  x.next.prev := x.prev;

  if list = x then 
    list := x.next;
end;

end.