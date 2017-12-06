unit HPMM;

// version 1.2 - 9/19/01
// modified on 9/19/01 to make compatible with C++ Builder.  old style "object"
// were converted to records.


// version 1.1 - 5/9/01
// modified on 5/8/01 to change reallocMem algorithm.  Define "Resize" and set
// ToleratedShrinkIndex=5 to revert to previous version.


interface

type
  DWord=Cardinal;
  PDWord=^Cardinal;

const
  LastBlockFlag=DWord(-1);
  MinTableIndex=5;
  MaxTableIndex=29; // = 1 GB max allocation.
  MinAlignedSize=1 shl MinTableIndex;

  MinAllocateSize=1024*1024;
  MaxAlignedIndex=6+10;
  MaxAlignedSize=1 shl MaxAlignedIndex;

  ToleratedShrinkIndex=MaxTableIndex; // changed from 5 on 5/8/01

  AggregationFactor=4;

function RoundUpAlign(Size:integer):integer;

type
  TBlockType=(btFree,btUsed,btLoRemnant,btHiRemnant);

  PMemBlock=^TMemBlock;
  TMemBlock=record
    SizeIndex:byte;
    BlockType:TBlockType;
    PrevIndex:byte;
    UsedSpace:byte;
  end;

    procedure SetSize(Self:PMemBlock; value,AlignedSize:integer);
    procedure MakeHighRemnant(Self:PMemBlock);
    function BlockSize(Self:PMemBlock):integer;
    function Prev(Self:PMemBlock):PMemBlock;
    function Next(Self:PMemBlock):PMemBlock;
    function IsValid(Self:PMemBlock):boolean;

type
  PFreeBlock=^TFreeBlock;
  TFreeBlock=record
    SizeIndex:byte;
    BlockType:TBlockType;
    PrevIndex:byte;
    UsedSpace:byte;
    NextBlock:PFreeBlock;
  end;

    procedure Init(Self:PFreeBlock; aSizeIndex: integer; PrevBlock: PMemBlock); overload;

type
  PAlloc=^TAlloc;
  TAlloc=record
    LoRemnantBlock:TMemBlock;
    size:DWord;
    Next:PAlloc;
    // unused space
    // start of Memblocks at MinAlignSize - 4;
  end;

    procedure init(Self:PAlloc; aSize:integer); overload;
    function HiRemnant(Self:PAlloc):PMemBlock;
    function LoRemnant(Self:PAlloc):PMemBlock;

type
  PBlockPool=^TBlockPool;
  TBlockPool=record
    Blocks:PFreeBlock;
    Count:integer;
    MinCount:integer;
    MaxCount:integer;
    HiLimit:integer;
    ExpansionIndex:integer;
  end;

  TBlockPools=array[0..31] of TBlockPool;

  TTransferRec=record
    FirstBlock,LastBlock:PFreeBlock;
    count:integer;
  end;

  TTransferArray=array[0..31] of TTransferRec;

type
  TLocalObject=class
  public
    class function NewInstance:TObject; override;
    procedure FreeInstance; override;
  end;

  TBlockManager=class(TLocalObject)
  private
    FAllocCount:integer;
    FAllocSize:Cardinal;
    function FindBlock(SizeIndex: integer): PFreeBlock;
  protected
    BlockPools:TBlockPools;
    procedure GiveTransfer(var TransferArray: TTransferArray);
    procedure TakeTransfer(var TransferArray: TTransferArray);
    procedure ReturnToPool(Block:PFreeBlock); virtual;
    function IsAvailable(sizeIndex: integer): Boolean; virtual; abstract;
    procedure Allocate(Size:integer); virtual; abstract;
    function GetBlock(SizeIndex: integer): PFreeBlock;
    function GetMemByIndex(SizeIndex,Size:integer):Pointer;
    procedure Resize(Block:PMemBlock; NewSizeIndex:integer);
    procedure MakeAvailable(SizeIndex:integer; var count:integer); virtual; abstract;
  public
    constructor Create;
    function GetMem(MinSize:integer):Pointer; virtual;
    function FreeMem(addr:Pointer):integer; virtual;
    function ReAllocMem(addr:Pointer; Size:integer):Pointer; virtual;
    property AllocCount:integer read FAllocCount;
    property AllocSize:Cardinal read FAllocSize;
  end;

  TMemManager=class(TBlockManager)
    // allocates from OS to make blocks
  private
    Allocs:PAlloc;
    procedure AddAlloc(NewAlloc: PAlloc);
    procedure FormatSpace(PrevBlock, NextBlock: PMemBlock);
  protected
    function IsAvailable(sizeIndex: integer): Boolean; override;
    procedure Allocate(Size:integer); override;
    procedure MakeAvailable(SizeIndex:integer; var count:integer); override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  TotalAlloc:integer;

implementation

{ Basic Functions }

function HighOrderBit(Value:integer):integer;
asm
  bsr edx,eax  // find high order bit
  mov eax,edx
end;

function LowOrderBit(Value:integer):integer;
asm
  bsf edx,eax  // find low order bit
  mov eax,edx
end;

procedure MoveMem32(D,S:Pointer;Size:integer);
asm
	push edi
	push esi
	mov edi,eax
	mov esi,edx
	rep movsd
	pop esi
	pop edi
end;

function RoundUpAlign(Size:integer):integer;
// this routine is in asm due to the need for bsr
asm
  cmp eax,MinAlignedSize
  jg @@Greater
  mov eax,MinTableIndex
  ret
@@Greater:
  bsr ecx,eax  // find high order bit
  mov edx,1
  shl edx,cl
  cmp edx,eax
  jne @@NotEqual
  mov eax,ecx
  ret
@@NotEqual:
  lea eax,[ecx+1]
end;


{ TMemBlock }

const  LoRemnantFlag=0;
var  SizeValidTable,PrevValidTable:array[0..255] of byte;

procedure InitializeValidationTables;
var i:integer;
begin
  for i:=MinTableindex to MaxTableIndex do
  begin
    SizeValidTable[i]:=1;
    PrevValidTable[i]:=1;
  end;
  PrevValidTable[LoRemnantFlag]:=1;
end;

function IsValid(Self:PMemBlock): boolean;
begin // 22.5 bits of error checking
  Result:=(SizeValidTable[Self.SizeIndex] and PrevValidTable[Self.prevIndex] and byte(Self.BlockType))=1;
end;

function Next(Self:PMemBlock): PMemBlock;
begin
//  if BlockType<>btHiRemnant then
    result:=Pointer(DWord(Self)+DWord(1 shl Self.SizeIndex))
//  else
//    result:=Pointer(DWord(Self)+4);
end;

function Prev(Self:PMemBlock): PMemBlock;
begin
  if Self.PrevIndex<>LoRemnantFlag then
    result:=Pointer(DWord(Self)-DWord(1 shl Self.PrevIndex))
  else
    result:=Pointer(DWord(Self)-(MinAlignedSize-4));
end;

function BlockSize(Self:PMemBlock): integer;
begin
  if Self.BlockType<=btUsed then
    if Self.SizeIndex>7 then
      Result:=Self.UsedSpace shl (Self.SizeIndex-7)
    else
      Result:=Self.UsedSpace
  else if Self.BlockType=btLoRemnant then
    Result:=MinAlignedSize - 4
  else
    Result:=4;
end;

procedure SetSize(Self:PMemBlock; value,AlignedSize:integer);
var
  shiftIndex:integer;
begin
  shiftIndex:=Self.sizeIndex-7;
  if shiftIndex>0 then
  begin
    value:=value + (AlignedSize-1) shr 7;
    Self.UsedSpace:=value shr shiftIndex;
  end
  else
    Self.UsedSpace:=value;
end;

procedure MakeHighRemnant(Self:PMemBlock);
begin

end;

{ TFreeBlock }

procedure Init(Self:PFreeBlock; aSizeIndex: integer; PrevBlock: PMemBlock);
type
  PInteger=^Integer;
begin
  PInteger(Self)^:=aSizeIndex;
  Self.PrevIndex:=PrevBlock.SizeIndex;
end;



{ TAlloc }

function HiRemnant(Self:PAlloc): PMemBlock;
begin
  Result:=Pointer(DWord(Self)+Self.Size-SizeOf(TMemBlock));
end;

procedure init(Self:PAlloc; aSize: integer);
const
  LoRemnantConst:TMemBlock=(SizeIndex:LoRemnantFlag; BlockType:btLoRemnant; PrevIndex:0; UsedSpace:0);
  HiRemnantConst:TMemBlock=(SizeIndex:0; BlockType:btHiRemnant; PrevIndex:0; UsedSpace:0);
begin
  Self.Size:=aSize;
  Self.LoRemnantBlock:=LoRemnantConst;
  HiRemnant(Self)^:=HiRemnantConst;
  Self.Next:=Pointer(LastBlockFlag);
end;

function LoRemnant(Self:PAlloc): PMemBlock;
begin
  Result:=@Self.LoRemnantBlock;
end;

{ TLocalObject }

const
  kernel = 'kernel32.dll';
  LMEM_FIXED = 0;
  LMEM_MOVEABLE = 2;
  LMEM_ZEROINIT = $40;


function LocalAlloc(flags, size: Integer): Pointer; stdcall;
  external kernel name 'LocalAlloc';
function LocalReAlloc(addr:Pointer; size,flags : Integer): Pointer; stdcall;
  external kernel name 'LocalReAlloc';
function LocalFree(addr: Pointer): Pointer; stdcall;
  external kernel name 'LocalFree';

procedure TLocalObject.FreeInstance;
begin
  LocalFree(Self);
end;

class function TLocalObject.NewInstance: TObject;
begin
  Result:=InitInstance(LocalAlloc(LMEM_FIXED,InstanceSize));
end;

{ TBlockManager }

procedure TBlockManager.Resize(Block:PMemBlock; NewSizeIndex:integer);
// splits off extra to reduce Block to NewSizeIndex
// Block is not returned to the pool.
var
  NextBlock,PrevBlock:PMemBlock;
  i:integer;
  Pool:PBlockPool;
  OriginalIndex,Index:integer;
begin
  Pool:=@BlockPools[NewSizeIndex];
  OriginalIndex:=Block.SizeIndex;
  Index:=OriginalIndex-NewSizeIndex;  // pow 2 number of blocks possible
  if Index>Pool.ExpansionIndex then
    Index:=Pool.ExpansionIndex;
//  assert(Index>0,'Invalid Resize');
  // Adjust Block
  NextBlock:=Next(Block);
  Block.SizeIndex:=NewSizeIndex;
  // Make 2^Index -1 extra blocks
  PrevBlock:=Block;
  for i:=1 to (1 shl Index)-1 do
  begin
    Block:=Next(Block);
    init(PFreeBlock(Block),NewSizeIndex,PrevBlock);
    ReturnToPool(PFreeBlock(Block));
    PrevBlock:=Block;
  end;
  // Deal with remainder
  for i:=Index+NewSizeIndex to OriginalIndex-1 do
  begin
    Block:=Next(Block);
    Init(PFreeBlock(Block),i,PrevBlock);
    ReturnToPool(PFreeBlock(Block));
    PrevBlock:=Block;
  end;
  NextBlock.PrevIndex:=Block.SizeIndex;
end;

function TBlockManager.FindBlock(SizeIndex:integer):PFreeBlock;
var
  i:integer;
  Pool:PBlockPool;
begin
  Pool:=@BlockPools[SizeIndex];
  if IsAvailable(SizeIndex) then
  begin
    Result:=Pool.Blocks;
    Dec(Pool.Count);
    Pool.Blocks:=Result.NextBlock;
  end
  else // ok to Resize a block;
  begin // find a bigger block
    i:=SizeIndex;
    while (i<=MaxTableIndex) and (BlockPools[i].count=0) do
      inc(i);
    if i<=MaxTableIndex then // found one
    begin
      Result:=BlockPools[i].Blocks;
      BlockPools[i].Blocks:=Result.NextBlock;
      Dec(BlockPools[i].Count);
      Resize(PMemBlock(Result),SizeIndex);
    end
    else
      Result:=nil;
  end;
end;

function TBlockManager.GetBlock(SizeIndex:integer):PFreeBlock;
begin
  Result:=FindBlock(SizeIndex);
  while not Assigned(Result) do
  begin
    Allocate(1 shl SizeIndex);
    Result:=FindBlock(SizeIndex);
  end;
end;

function TBlockManager.GetMemByIndex(SizeIndex,Size:integer):Pointer;
var
  Block:PMemBlock;
begin
  Block:=PMemBlock(GetBlock(SizeIndex));
  Block.BlockType:=btUsed;
  SetSize(Block,Size,1 shl SizeIndex);
  Result:=PMemBlock(DWord(Block)+4);
end;


function TBlockManager.GetMem(MinSize: integer): Pointer;
var Size,SizeIndex:integer;
begin
  Size:=(MinSize+SizeOf(DWord)+ 3) and (not 3);  // DWord round
  SizeIndex:=RoundUpAlign(Size);
  {$IfDef Analysis}
  inc(FAllocSize,1 shl SizeIndex);
  inc(FAllocCount);
  {$Endif}
  Result:=GetMemByIndex(SizeIndex,Size);
end;

function TBlockManager.ReAllocMem(addr: Pointer; Size: integer): Pointer;
var
  Block:PMemBlock;
  NewSizeIndex,SizeIndex:integer;
  AlignedSize:integer;
begin
  Block:=Pointer(DWord(Addr)-SizeOf(DWord));
  if (SizeValidTable[Block.SizeIndex] and PrevValidTable[Block.prevIndex] and byte(Block.BlockType))=1 then
  begin
    Size:=(Size+SizeOf(DWord)+ 3) and (not 3);  // DWord round
    NewSizeIndex:=RoundUpAlign(Size);
    SizeIndex:=Block.SizeIndex;
    AlignedSize:=1 shl SizeIndex;
    if NewSizeIndex<=SizeIndex then
    begin
      if NewSizeIndex+ToleratedShrinkIndex>HighOrderBit(BlockSize(Block)) then
      begin  // keep extra space
        Result:=Addr;
        SetSize(Block,Size,AlignedSize);
        exit;
      end
{$IFDef Resize}
      else  // removed 5/8/01
      begin // Resize
        Result:=addr;
        Resize(Block,NewSizeIndex+ToleratedShrinkIndex);
        {$IfDef Analysis}
        inc(FAllocSize,(1 shl (NewSizeIndex+ToleratedShrinkIndex))-AlignedSize);
        {$Endif}
        Block.SetSize(Size,AlignedSize);
        exit;
      end
{$endif}
    end;
    // move
    Result:=GetMemByIndex(NewSizeIndex,Size);
    if NewSizeIndex<SizeIndex then
      MoveMem32(Result,Addr,(Size-SizeOf(DWord)+3) shr 2) // copy only remaining data
    else
      MoveMem32(Result,Addr,(BlockSize(Block)-SizeOf(DWord)+3) shr 2); // all orig data
    Block.BlockType:=btFree;
    ReturnToPool(PFreeBlock(Block));
    {$IfDef Analysis}
    dec(FAllocSize,AlignedSize);
    inc(FAllocSize,1 shl NewSizeIndex);
    {$Endif}
  end
  else
    Result:=nil;
end;

var
  DefaultPools:TBlockPools=(
  (Count:0; MinCount:0; MaxCount:0; HiLimit:0), // 0 = 1
  (Count:0; MinCount:0; MaxCount:0; HiLimit:0), // 1
  (Count:0; MinCount:0; MaxCount:0; HiLimit:0), // 2 = 4
  (Count:0; MinCount:0; MaxCount:0; HiLimit:0), // 3
  (Count:0; MinCount:0; MaxCount:0; HiLimit:0), // 4
  (Count:0; MinCount:50; MaxCount:200; HiLimit:400), // 5 =32
  (Count:0; MinCount:50; MaxCount:200; HiLimit:400), // 6
  (Count:0; MinCount:50; MaxCount:200; HiLimit:400), // 7
  (Count:0; MinCount:20; MaxCount:100; HiLimit:200), // 8 = 256
  (Count:0; MinCount:20; MaxCount:100; HiLimit:200), // 9
  (Count:0; MinCount:10; MaxCount:50; HiLimit:100), // 10 =1k
  (Count:0; MinCount:10; MaxCount:50; HiLimit:100), // 11
  (Count:0; MinCount:5; MaxCount:20; HiLimit:50), // 12 = 4k
  (Count:0; MinCount:5; MaxCount:20; HiLimit:50), // 13
  (Count:0; MinCount:2; MaxCount:10; HiLimit:20), // 14 = 16k
  (Count:0; MinCount:2; MaxCount:10; HiLimit:20), // 15
  (Count:0; MinCount:1; MaxCount:5; HiLimit:10), // 16 = 64k
  (Count:0; MinCount:1; MaxCount:5; HiLimit:10), // 17
  (Count:0; MinCount:0; MaxCount:3; HiLimit:6), // 18 = 256k
  (Count:0; MinCount:0; MaxCount:3; HiLimit:6), // 19
  (Count:0; MinCount:0; MaxCount:3; HiLimit:6), // 20 = 1M
  (Count:0; MinCount:0; MaxCount:3; HiLimit:6), // 21
  (Count:0; MinCount:0; MaxCount:3; HiLimit:6), // 22 = 4M
  (Count:0; MinCount:0; MaxCount:3; HiLimit:6), // 23
  (Count:0; MinCount:0; MaxCount:3; HiLimit:6), // 24 = 16M
  (Count:0; MinCount:0; MaxCount:2; HiLimit:3), // 25
  (Count:0; MinCount:0; MaxCount:2; HiLimit:3), // 26 = 64M
  (Count:0; MinCount:0; MaxCount:1; HiLimit:2), // 27
  (Count:0; MinCount:0; MaxCount:0; HiLimit:1), // 28 = 256M
  (Count:0; MinCount:0; MaxCount:0; HiLimit:1), // 29
  (Count:0; MinCount:0; MaxCount:0; HiLimit:0), // 30 = 1G
  (Count:0; MinCount:0; MaxCount:0; HiLimit:0)); // 31 = 2G

constructor TBlockManager.Create;
var I:integer;
begin
  BlockPools:=DefaultPools;
  for i:=0 to 31 do
  begin
    DWord(BlockPools[i].Blocks):=LastBlockFlag;
    BlockPools[i].ExpansionIndex:=1;
  end;
end;

procedure TBlockManager.TakeTransfer(var TransferArray: TTransferArray);
var
  j:integer;
  Pool:PBlockPool;
begin
  for j:=MinTableIndex to MaxTableIndex do
  begin
    Pool:=@BlockPools[j];
    with TransferArray[j] do
      if Count>0 then
      begin
        LastBlock.NextBlock:=Pool.Blocks;
        Pool.Blocks:=Firstblock;
        inc(Pool.Count,count);
        Count:=0;
      end;
  end;
end;

procedure TBlockManager.GiveTransfer(var TransferArray: TTransferArray);
var
  i,j:integer;
  Pool:PBlockPool;
  Block:PFreeBlock;
begin
  for j:=MaxTableIndex downto MinTableIndex do with TransferArray[j] do
  // do in reverse order to prevent splitting a block that will be needed
  begin
    Pool:=@BlockPools[j];
    if Count<0 then
    begin
      Count:=-Count;
      if Count>Pool.Count then
        MakeAvailable(j,Count);
      Block:=Pool.Blocks;
      FirstBlock:=Block;
      for i:=1 to Count-1 do
        Block:=Block.NextBlock;
      LastBlock:=Block;
      Pool.Blocks:=Block.NextBlock;
      dec(Pool.Count,count);
    end
  end;
end;

procedure TBlockManager.ReturnToPool(Block: PFreeBlock);
var Pool:PBlockPool;
begin
  Pool:=@BlockPools[Block.SizeIndex];
  Block.NextBlock:=Pool.Blocks;
  Pool.Blocks:=Block;
  inc(Pool.Count);
end;

{ TMemManager }

function TMemManager.IsAvailable(sizeIndex: integer): Boolean;
begin
  result:=BlockPools[SizeIndex].Count>0;
end;

constructor TMemManager.Create;
begin
  inherited Create;
  Allocs:=Pointer(LastBlockFlag);
end;

const
  MEM_COMMIT   = $1000;
  MEM_RESERVE  = $2000;
  MEM_DECOMMIT = $4000;
  MEM_RELEASE  = $8000;

  PAGE_NOACCESS  = 1;
  PAGE_READWRITE = 4;

type
  BOOL  = LongBool;

function VirtualAlloc(lpAddress: Pointer;
  dwSize, flAllocationType, flProtect: DWORD): Pointer; stdcall;
  external kernel name 'VirtualAlloc';
function VirtualFree(lpAddress: Pointer; dwSize, dwFreeType: DWORD): BOOL; stdcall;
  external kernel name 'VirtualFree';

procedure TMemManager.FormatSpace(PrevBlock,NextBlock:PMemBlock);
Var
  Addr,Size:DWord;
  AlignedSize:DWord;
  SizeIndex,AddrIndex,Index:integer;
  Block:PFreeBlock;
begin
  if PrevBlock.BlockType=btLoRemnant then
    addr:=DWord(PrevBlock)+(MinAlignedSize-4)
  else
    addr:=DWord(Next(PrevBlock));
  Size:=DWord(NextBlock)-Addr;
  while Size>=MinAlignedSize do
  begin
    SizeIndex:=HighOrderBit(Size);
    AddrIndex:=LowOrderBit(Addr+SizeOf(DWord));
    if (AddrIndex<SizeIndex) and (AddrIndex<=MaxAlignedIndex) then
      Index:=AddrIndex
    else
      Index:=SizeIndex;
    AlignedSize:=1 shl Index;
    Block:=PFreeBlock(Addr);
    Init(Block,Index,PrevBlock);
    ReturnToPool(Block);
    inc(Addr,AlignedSize);
    dec(Size,AlignedSize);
    PrevBlock:=PMemBlock(Block);
  end;
  NextBlock.PrevIndex:=PrevBlock.SizeIndex;
end;

procedure TMemManager.AddAlloc(NewAlloc:PAlloc);
var
  Alloc,NextAlloc:PAlloc;
  NextBlock,PrevBlock:PMemBlock;
begin
  Alloc:=nil;
  NextAlloc:=Allocs;
  while DWord(NextAlloc)<DWord(NewAlloc) do
  begin
    Alloc:=NextAlloc;
    NextAlloc:=NextAlloc.Next;
  end;
  if DWord(NewAlloc)+NewAlloc.Size=DWord(NextAlloc) then // merge with nextAlloc
  begin
    NextBlock:=Next(LoRemnant(NextAlloc));
    inc(NewAlloc.Size,NextAlloc.Size);
    NewAlloc.Next:=NextAlloc.Next;
  end
  else // insert in list in from of NextAlloc
  begin
    NextBlock:=HiRemnant(NewAlloc);
    NewAlloc.Next:=NextAlloc;
  end;
  if not assigned(Alloc) then // First in list
  begin
    Allocs:=NewAlloc;
    PrevBlock:=LoRemnant(NewAlloc);
  end
  else if DWord(Alloc)+Alloc.Size=DWord(NewAlloc) then // merge with Alloc
  begin
    PrevBlock:=Prev(HiRemnant(Alloc));
    inc(Alloc.Size,NewAlloc.Size);
  end
  else // insert in list in back of Alloc
  begin
    PrevBlock:=LoRemnant(NewAlloc);
    Alloc.Next:=NewAlloc;
  end;
  FormatSpace(PrevBlock,NextBlock);
end;

procedure TMemManager.Allocate(Size:integer);
// Make at least one Free Block that can hold Size
Var
  NewAlloc:PAlloc;
begin
  if Size<MinAllocateSize then
    Size:=MinAllocateSize;
  Size:=1 shl RoundUpAlign(Size);
  if Size<MaxAlignedSize then  // Guarantee that Size can be aligned
    Size:=2*Size
  else
    inc(Size,MaxAlignedSize);
  NewAlloc := PAlloc(VirtualAlloc(nil, Size, MEM_COMMIT, PAGE_READWRITE));
  Init(NewAlloc,Size);
  inc(TotalAlloc,Size);
  AddAlloc(NewAlloc);
end;

procedure TMemManager.MakeAvailable(sizeIndex: integer; var count: integer);
// make sure that Count blocks of SizeIndex are in the pool
var
  Pool:PBlockPool;
  ShortCount:integer;
  ExpansionIndex,HoldIndex:integer;
  Block:PFreeBlock;
begin
  Pool:=@BlockPools[sizeIndex];
  ShortCount:=(Count-Pool.Count);
  if ShortCount=1 then
    ExpansionIndex:=1
  else
  begin
    ExpansionIndex:=HighOrderBit(ShortCount);
    if ((1 shl ExpansionIndex)<ShortCount) then
      inc(ExpansionIndex);
  end;
  // use Expansion Mechanism to produce extra blocks.
  HoldIndex:=Pool.ExpansionIndex;
  Pool.ExpansionIndex:=ExpansionIndex;
  Block:=GetBlock(ExpansionIndex+SizeIndex);
  Resize(PMemBlock(Block),SizeIndex);
  ReturnToPool(Block);
  Pool.ExpansionIndex:=HoldIndex;
end;


destructor TMemManager.Destroy;
var
  Alloc,PrevAlloc:PAlloc;
begin
  Alloc:=Allocs;
  while DWord(Alloc)<>LastBlockFlag do
  begin
    PrevAlloc:=Alloc;
    Alloc:=Alloc.Next;
    VirtualFree(PrevAlloc,0,MEM_RELEASE);
  end;
  inherited Destroy;
end;

function TBlockManager.FreeMem(addr: Pointer): integer;
var
  Block:PMemBlock;
begin
  Block:=Pointer(DWord(Addr)-SizeOf(DWord));
  if (SizeValidTable[Block.SizeIndex] and PrevValidTable[Block.prevIndex] and byte(Block.BlockType))=1 then
  begin
    {$IfDef Analysis}
    dec(FAllocSize,1 shl Block.SizeIndex);
    dec(FAllocCount);
    {$Endif}
    Block.BlockType:=btFree;
    ReturnToPool(PFreeBlock(Block));
    Result:=0;
  end
  else
    Result:=-1;
end;


initialization
  InitializeValidationTables;
end.
