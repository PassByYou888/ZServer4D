unit smmStatistics;

interface

{$Include smmOptions.inc}

uses
  smmTypes;

type
  TMemoryStatistics = record
    //number of allocs, reallocs, free's
    //total memory allocated, header overhead, unused size overhead, etc
  end;

  TSmallMemBlockListStats = object
    //size of memory items (32, 64 etc bytes)
    ItemSize : NativeUInt;
    //total size of one array block
    ArrayBlockSize: NativeUInt;

    //how much array blocks are allocated (PSmallMemBlock)
    ArrayBlockCount,
    //how much array items are used
    TotalUsageCount: NativeUInt;
    //how much array items are allocated/buffered
    function TotalAllocCount: NativeUInt;
    //how much array items are free/available
    function TotalFreeCount: NativeUInt;

    //how much raw memory is allocated (including headers/overhead)
    function TotalArrayBlockSize: NativeUInt;
  end;
  PSmallMemBlockListStats = ^TSmallMemBlockListStats;

  TSmallMemThreadManagerStats = object
    /// array with memory per block size of 32 bytes (mini blocks)
    // - i.e. 32, 64, 96, 128, 160, 192, 224 bytes
    FMiniMemoryBlocks: array[0..6] of TSmallMemBlockListStats;
    /// array with memory per block size of 256 bytes (small blocks)
    // - i.e. 256,512,768,1024,1280,1536,1792[,2048] bytes
    FSmallMemoryBlocks: array[0..MAX_SMALLMEMBLOCK] of TSmallMemBlockListStats;

    function TotalAlloc: NativeUInt;
    function TotalUsed: NativeUInt;
    function TotalFree: NativeUInt;
    function TotalSize: NativeUInt;

    procedure DumpToFile(aFile: THandle);
  end;

  TMediumMemThreadManagerStats = object
    MemBySize: array[0..16] of record
      Size,
      Used, Free: NativeUInt;
    end;
    FreeMask : NativeUInt; //word, 16 bit, 65535

    BlockSize, BlockCount,
    TotalUsed, TotalFree: NativeUInt;
    function TotalAlloc: NativeUInt;

    procedure DumpToFile(aFile: THandle);
  end;

  TLargeMemThreadManagerStats = object
    AllocCount, AllocSize: NativeUInt;

    procedure DumpToFile(aFile: THandle);
  end;

  TThreadMemManagerStats = object
    SmallMemoryStats: TSmallMemThreadManagerStats;
    MediumMemoryStats: TMediumMemThreadManagerStats;
    LargeMemoryStats: TLargeMemThreadManagerStats;

    procedure DumpToFile(aFile: THandle);
  end;
  PThreadMemManagerStats = ^TThreadMemManagerStats;

implementation

uses
  smmFunctions;

{ TSmallMemBlockListStats }

function TSmallMemBlockListStats.TotalAllocCount: NativeUInt;
begin
  Result := ArrayBlockCount * C_ARRAYSIZE;
end;

function TSmallMemBlockListStats.TotalArrayBlockSize: NativeUInt;
begin
  Result := ArrayBlockSize * ArrayBlockCount;
end;

function TSmallMemBlockListStats.TotalFreeCount: NativeUInt;
begin
  Result := TotalAllocCount - TotalUsageCount;
end;

{ TThreadMemManagerStats }

procedure TThreadMemManagerStats.DumpToFile(aFile: THandle);
begin
  WriteToFile(aFile, 'TThreadMemManagerStats'#13#10);

  SmallMemoryStats.DumpToFile(aFile);

  MediumMemoryStats.DumpToFile(aFile);
  WriteToFile(aFile, '  * Real medium mem (minus small)'#13#10'  - TotalAlloc: ');
  WriteNativeUIntToStrBuf(aFile, MediumMemoryStats.TotalAlloc - SmallMemoryStats.TotalAlloc);
  WriteToFile(aFile, ', TotalUsed: ');
  WriteNativeUIntToStrBuf(aFile, MediumMemoryStats.TotalUsed - SmallMemoryStats.TotalAlloc);
  WriteToFile(aFile, #13#10);

  LargeMemoryStats.DumpToFile(aFile);
end;

{ TSmallMemThreadManagerStats }

procedure TSmallMemThreadManagerStats.DumpToFile(aFile: THandle);
var
  i: Integer;
begin
  WriteToFile(aFile, '- SmallMemoryStats'#13#10);
  for i := 0 to High(Self.FMiniMemoryBlocks) do
  begin
    WriteToFile(aFile, '  - SmallBlockList');
    WriteToFile(aFile, ', itemsize = ');
    WriteNativeUIntToStrBuf(aFile, Self.FMiniMemoryBlocks[i].ItemSize);
    WriteToFile(aFile, ', blocks = ');
    WriteNativeUIntToStrBuf(aFile, Self.FMiniMemoryBlocks[i].ArrayBlockCount);
    WriteToFile(aFile, ', item count = ');
    WriteNativeUIntToStrBuf(aFile, Self.FMiniMemoryBlocks[i].TotalAllocCount);
    WriteToFile(aFile, ', items used = ');
    WriteNativeUIntToStrBuf(aFile, Self.FMiniMemoryBlocks[i].TotalUsageCount);
    WriteToFile(aFile, ', item free = ');
    WriteNativeUIntToStrBuf(aFile, Self.FMiniMemoryBlocks[i].TotalFreeCount);
    WriteToFile(aFile, ', total mem allocated = ');
    WriteNativeUIntToStrBuf(aFile, Self.FMiniMemoryBlocks[i].TotalArrayBlockSize);
    WriteToFile(aFile, #13#10);
  end;
  for i := 0 to High(Self.FSmallMemoryBlocks) do
  begin
    WriteToFile(aFile, '  - SmallBlockList');
    WriteToFile(aFile, ', itemsize = ');
    WriteNativeUIntToStrBuf(aFile, Self.FSmallMemoryBlocks[i].ItemSize);
    WriteToFile(aFile, ', blocks = ');
    WriteNativeUIntToStrBuf(aFile, Self.FSmallMemoryBlocks[i].ArrayBlockCount);
    WriteToFile(aFile, ', item count = ');
    WriteNativeUIntToStrBuf(aFile, Self.FSmallMemoryBlocks[i].TotalAllocCount);
    WriteToFile(aFile, ', items used = ');
    WriteNativeUIntToStrBuf(aFile, Self.FSmallMemoryBlocks[i].TotalUsageCount);
    WriteToFile(aFile, ', item free = ');
    WriteNativeUIntToStrBuf(aFile, Self.FSmallMemoryBlocks[i].TotalFreeCount);
    WriteToFile(aFile, ', total mem allocated = ');
    WriteNativeUIntToStrBuf(aFile, Self.FSmallMemoryBlocks[i].TotalArrayBlockSize);
    WriteToFile(aFile, #13#10);
  end;

  WriteToFile(aFile, '  * TotalAllocated: ');
  WriteNativeUIntToStrBuf(aFile, Self.TotalAlloc);
  WriteToFile(aFile, ', TotalUsed: ');
  WriteNativeUIntToStrBuf(aFile, Self.TotalUsed);
  WriteToFile(aFile, ', TotalFree: ');
  WriteNativeUIntToStrBuf(aFile, Self.TotalFree);
  WriteToFile(aFile, ', Total mem allocated: ');
  WriteNativeUIntToStrBuf(aFile, Self.TotalSize);
  WriteToFile(aFile, #13#10);
end;

function TSmallMemThreadManagerStats.TotalAlloc: NativeUInt;
var i: Integer;
begin
  Result := 0;
  for i := 0 to High(Self.FMiniMemoryBlocks) do
    Inc(Result, FMiniMemoryBlocks[i].TotalAllocCount * FMiniMemoryBlocks[i].ItemSize);
  for i := 0 to High(Self.FSmallMemoryBlocks) do
    Inc(Result, FSmallMemoryBlocks[i].TotalAllocCount * FSmallMemoryBlocks[i].ItemSize);
end;

function TSmallMemThreadManagerStats.TotalFree: NativeUInt;
var i: Integer;
begin
  Result := 0;
  for i := 0 to High(Self.FMiniMemoryBlocks) do
    Inc(Result, FMiniMemoryBlocks[i].TotalFreeCount * FMiniMemoryBlocks[i].ItemSize);
  for i := 0 to High(Self.FSmallMemoryBlocks) do
    Inc(Result, FSmallMemoryBlocks[i].TotalFreeCount * FSmallMemoryBlocks[i].ItemSize);
end;

function TSmallMemThreadManagerStats.TotalSize: NativeUInt;
var i: Integer;
begin
  Result := 0;
  for i := 0 to High(Self.FMiniMemoryBlocks) do
    Inc(Result, FMiniMemoryBlocks[i].TotalArrayBlockSize);
  for i := 0 to High(Self.FSmallMemoryBlocks) do
    Inc(Result, FSmallMemoryBlocks[i].TotalArrayBlockSize);
end;

function TSmallMemThreadManagerStats.TotalUsed: NativeUInt;
var i: Integer;
begin
  Result := 0;
  for i := 0 to High(Self.FMiniMemoryBlocks) do
    Inc(Result, FMiniMemoryBlocks[i].TotalUsageCount * FMiniMemoryBlocks[i].ItemSize);
  for i := 0 to High(Self.FSmallMemoryBlocks) do
    Inc(Result, FSmallMemoryBlocks[i].TotalUsageCount * FSmallMemoryBlocks[i].ItemSize);
end;

{ TMediumMemThreadManagerStats }

procedure TMediumMemThreadManagerStats.DumpToFile(aFile: THandle);
var i: Integer;
begin
  WriteToFile(aFile, '- MediumMemoryStats'#13#10);

  for i := 0 to High(MemBySize) do
  begin
    WriteToFile(aFile, '  - Medium items');
    WriteToFile(aFile, ', itemsize(min) = ');
    WriteNativeUIntToStrBuf(aFile, MemBySize[i].Size);
    WriteToFile(aFile, ', used = ');
    WriteNativeUIntToStrBuf(aFile, MemBySize[i].Used);
    WriteToFile(aFile, ', free = ');
    WriteNativeUIntToStrBuf(aFile, MemBySize[i].Free);
    WriteToFile(aFile, #13#10);
  end;

  WriteToFile(aFile, '  * Blocksize = ');
  WriteNativeUIntToStrBuf(aFile, BlockSize);
  WriteToFile(aFile, ', block count = ');
  WriteNativeUIntToStrBuf(aFile, BlockCount);
  WriteToFile(aFile, ', total allocated = ');
  WriteNativeUIntToStrBuf(aFile, BlockCount * BlockSize);
  WriteToFile(aFile, #13#10);

  WriteToFile(aFile, '  * TotalAlloc: ');
  WriteNativeUIntToStrBuf(aFile, TotalAlloc);
  WriteToFile(aFile, ', TotalUsed: ');
  WriteNativeUIntToStrBuf(aFile, TotalUsed);
  WriteToFile(aFile, ', TotalFree: ');
  WriteNativeUIntToStrBuf(aFile, TotalFree);
  WriteToFile(aFile, #13#10);
end;

function TMediumMemThreadManagerStats.TotalAlloc: NativeUInt;
begin
  Result := TotalUsed + TotalFree;
end;

{ TLargeMemThreadManagerStats }

procedure TLargeMemThreadManagerStats.DumpToFile(aFile: THandle);
begin
  WriteToFile(aFile, '- LargeMemoryStats'#13#10);
  WriteToFile(aFile, '  * TotalCount: ');
  WriteNativeUIntToStrBuf(aFile, AllocCount);
  WriteToFile(aFile, ', TotalSize: ');
  WriteNativeUIntToStrBuf(aFile, AllocSize);
end;

end.
