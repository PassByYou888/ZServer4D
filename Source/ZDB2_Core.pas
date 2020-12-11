{ ****************************************************************************** }
{ * ZDB 2.0 Core, create by.qq600585                                           * }
{ * https://zpascal.net                                                        * }
{ * https://github.com/PassByYou888/zAI                                        * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/PascalString                               * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zChinese                                   * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/FFMPEG-Header                              * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/InfiniteIoT                                * }
{ * https://github.com/PassByYou888/FastMD5                                    * }
{ ****************************************************************************** }
unit ZDB2_Core;

{$INCLUDE zDefine.inc}

interface

uses CoreClasses,
{$IFDEF FPC}
  FPCGenericStructlist,
{$ENDIF FPC}
  PascalStrings, UnicodeMixedLib,
{$IFNDEF ZDB2_Core_Used_Mem64}
  MemoryStream64,
{$ENDIF ZDB2_Core_Used_Mem64}
  DoStatusIO;

const
  C_ZDB2_FileHead = $90909090;
  C_ZDB2_MinBlockSize = $40;

type
  TZDB2_Core_Space = class;

{$IFDEF ZDB2_Core_Used_Mem64}
  TZDB2_Core_Mem64 = class;
  TMem64 = TZDB2_Core_Mem64;
{$ELSE ZDB2_Core_Used_Mem64}
  TMem64 = TMemoryStream64;
{$ENDIF ZDB2_Core_Used_Mem64}

  TZDB2_Core_BlockData = packed record
    Position: Int64;
    Size: Word;
    UsedSpace: Word;
    ID: Integer;
    Prev, Next: Integer;
  end;

  PZDB2_Core_BlockData = ^TZDB2_Core_BlockData;

  TZDB2_Core_BlockCache = record
    Mem: TMem64;
    FlushThisCacheToFile: Boolean;
  end;

  TZDB2_Core_CustomFileHeader = array [0 .. $FF - 1] of Byte;
  PZDB2_Core_CustomFileHeader = ^TZDB2_Core_CustomFileHeader;
  TZDB2_Core_BlockPtrList = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<PZDB2_Core_BlockData>;
  TZDB2_Core_BlockHnd = array of Integer;
  TZDB2_Core_PhysicsBlock = array of TZDB2_Core_BlockData;
  TZDB2_Core_PhysicsBlockWriteCache = array of TZDB2_Core_BlockCache;
  TZDB2_Core_OnProgress = procedure(Total_, current_: Integer) of object;
  TZDB2_Core_OnNoSpace = procedure(Siz_: Int64; var retry: Boolean) of object;

  { stmBigData: DB Size > 10G, < 130TB, block number < 1000*10000, no cache }
  { stmNormal: DB size > 1G, < 10G, block number < 100*10000, open write cache }
  { stmFast: DB size > 100M, < 1G, block number < 10*10000, open Next/w cache }
  TZDB2_Core_SpaceMode = (stmBigData, stmNormal, stmFast);

  TZDB2_Core_SpaceState = record
    Physics: Int64;
    FreeSpace: Int64;
    Cache: Int64;
    ReadNum: Int64;
    ReadSize: Int64;
    WriteNum: Int64;
    WriteSize: Int64;
  end;

  PZDB2_Core_SpaceState = ^TZDB2_Core_SpaceState;

  TZDB2_Core_Space = class
  private
    FAutoCloseIOHnd: Boolean;
    FAutoFreeIOHnd: Boolean;
    FSpace_IOHnd: PIOHnd;
    FCustomFileHeader: TZDB2_Core_CustomFileHeader;
    FFreeSpaceIndexProbe: Integer;
    FPhyBlockNum: Integer;
    FPhyBlock: TZDB2_Core_PhysicsBlock;
    FMaxCacheMemory: Int64;
    FUsedReadCache: Boolean;
    FUsedWriteCache: Boolean;
    FPhyBlockWriteCache: TZDB2_Core_PhysicsBlockWriteCache;
    FMode: TZDB2_Core_SpaceMode;
    FState: TZDB2_Core_SpaceState;
    FOnProgress: TZDB2_Core_OnProgress;
    FOnNoSpace: TZDB2_Core_OnNoSpace;

    { cache }
    function ReadCacheBlock(buff: Pointer; ID: Integer): Boolean;
    function WriteCacheBlock(buff: Pointer; siz: Integer; ID: Integer; FlushThisCache_: Boolean): Boolean;
    { Calculate storage size of space table }
    function GetTableSize: Int64; overload;
    function GetTableSize(PhyBlockNum_: Integer): Int64; overload;
  public
    constructor Create(IOHnd_: PIOHnd);
    destructor Destroy; override;

    property AutoCloseIOHnd: Boolean read FAutoCloseIOHnd write FAutoCloseIOHnd;
    property AutoFreeIOHnd: Boolean read FAutoFreeIOHnd write FAutoFreeIOHnd;
    property Space_IOHnd: PIOHnd read FSpace_IOHnd write FSpace_IOHnd;
    function GetCustomFileHeader: PZDB2_Core_CustomFileHeader;
    property CustomFileHeader: PZDB2_Core_CustomFileHeader read GetCustomFileHeader;
    property PhyBlockNum: Integer read FPhyBlockNum;
    property PhyBlock: TZDB2_Core_PhysicsBlock read FPhyBlock;
    property MaxCacheMemory: Int64 read FMaxCacheMemory write FMaxCacheMemory;
    property UsedReadCache: Boolean read FUsedReadCache write FUsedReadCache;
    property UsedWriteCache: Boolean read FUsedWriteCache write FUsedWriteCache;
    { stmBigData: DB Size > 10G, < 130TB, block number < 1000*10000, no cache }
    { stmNormal: DB size > 1G, < 10G, block number < 100*10000, open write cache }
    { stmFast: DB size > 100M, < 1G, block number < 10*10000, open Next/w cache }
    procedure SetMode(const Value: TZDB2_Core_SpaceMode);
    property Mode: TZDB2_Core_SpaceMode read FMode write SetMode;
    { state }
    function GetState: PZDB2_Core_SpaceState;
    property State: PZDB2_Core_SpaceState read GetState;
    { event }
    property OnProgress: TZDB2_Core_OnProgress read FOnProgress write FOnProgress;
    property OnNoSpace: TZDB2_Core_OnNoSpace read FOnNoSpace write FOnNoSpace;

    { error }
    procedure ErrorInfo(const Text_: SystemString);
    { cache }
    procedure DeleteCache(ID: Integer);
    procedure ClearCache;
    procedure FlushCache;
    { Write space table to physical IO }
    procedure WriteTable();
    { Write space table to physical IO, and clear write back buffer + preread buffer + flush cache }
    procedure Save();
    { Read space table + scan fragment space }
    procedure Open();
    { Scanning space state }
    procedure ScanSpace;
    { Prepare data structure space }
    procedure PreparePhyBlock(PhyBlockNum_: Int64);
    { build fixed size storage space }
    procedure BuildSpace(PhySpaceSize: Int64; BlockSize_: Word);
    { append of fixed size storage space }
    procedure AppendSpace(Dest: TZDB2_Core_Space; DestPhySpaceSize: Int64; DestBlockSize_: Word);
    { Optimize storage space seealso to SaveAs }
    procedure OptimizeInstance(Dest: TZDB2_Core_Space);

    { Take id as parameter to merge ID into TZDB2_Core_BlockHnd }
    function Check(ID_: Integer): Boolean;
    function GetSpaceHndID(ID_: Integer): Integer;
    function GetSpaceHnd(ID_: Integer): TZDB2_Core_BlockHnd;
    { Write a buffer to automatically fill the space block }
    function CheckWriteSpace(Siz_: Int64): Boolean; overload;
    function CheckWriteSpace(Siz_: Int64; Space_: TZDB2_Core_BlockPtrList): Boolean; overload;
    function WriteData(buff: TMem64; var SpaceHnd: TZDB2_Core_BlockHnd): Boolean; overload;
    function WriteData(buff: TMem64; var ID: Integer): Boolean; overload;
    { read the fragment space and merge the data to buff }
    function ReadData(buff: TMem64; SpaceHnd: TZDB2_Core_BlockHnd): Boolean; overload;
    function ReadData(buff: TMem64; ID: Integer): Boolean; overload;
    { remove the fragments. This operation will not reconstruct freespace. our need call ScanSpace to rebuild freespace struct. }
    function RemoveData(SpaceHnd: TZDB2_Core_BlockHnd; SafeClean_: Boolean): Boolean; overload;
    function RemoveData(ID: Integer; SafeClean_: Boolean): Boolean; overload;
    { data size }
    function GetDataSize(SpaceHnd: TZDB2_Core_BlockHnd): Int64; overload;
    function GetDataSize(ID: Integer): Int64; overload;
    { physics size }
    function GetDataPhysics(SpaceHnd: TZDB2_Core_BlockHnd): Int64; overload;
    function GetDataPhysics(ID: Integer): Int64; overload;

    class procedure Test();
  end;

{$IFDEF ZDB2_Core_Used_Mem64}

  TZDB2_Core_Mem64 = class
  private
    FDelta: NativeInt;
    FMemory: Pointer;
    FSize: Int64;
    FPosition: Int64;
    FCapacity: Int64;
    FProtectedMode: Boolean;
  protected
    procedure SetPointer(buffPtr: Pointer; const BuffSize: Int64);
    procedure SetCapacity(NewCapacity: Int64);
    function Realloc(var NewCapacity: Int64): Pointer;
    property Capacity: Int64 read FCapacity write SetCapacity;
    function GetPosition: Int64;
    procedure SetPosition(const Value: Int64);
    function GetSize: Int64;
    procedure SetSize(const NewSize: Int64);
  public
    constructor Create;
    constructor CustomCreate(const customDelta: NativeInt);
    destructor Destroy; override;

    procedure DiscardMemory;
    procedure Clear;
    procedure NewParam(source: TZDB2_Core_Mem64);
    procedure SwapInstance(source: TZDB2_Core_Mem64);

    property Delta: NativeInt read FDelta write FDelta;
    property Memory: Pointer read FMemory;
    property Position: Int64 read GetPosition write SetPosition;
    property Size: Int64 read GetSize write SetSize;

    procedure SetPointerWithProtectedMode(buffPtr: Pointer; const BuffSize: Int64);
    procedure Mapping(buffPtr: Pointer; const BuffSize: Int64);
    function PositionAsPtr(const Position_: Int64): Pointer; overload;
    function PositionAsPtr: Pointer; overload;
    function PosAsPtr(const Position_: Int64): Pointer; overload;
    function PosAsPtr: Pointer; overload;

    procedure LoadFromStream(stream: TCoreClassStream);
    procedure LoadFromFile(FileName: SystemString);
    procedure SaveToStream(stream: TCoreClassStream);
    procedure SaveToFile(FileName: SystemString);

    function Write64(const buffer; Count: Int64): Int64;
    function WritePtr(const p: Pointer; Count: Int64): Int64;
    function Write(const buffer; Count: Int64): Int64;
    function Read64(var buffer; Count: Int64): Int64;
    function ReadPtr(const p: Pointer; Count: Int64): Int64;
    function Read(var buffer; Count: Int64): Int64;
    function Seek(const Offset: Int64; origin: TSeekOrigin): Int64;

    // Serialized writer
    procedure WriteBool(const buff: Boolean);
    procedure WriteInt8(const buff: ShortInt);
    procedure WriteInt16(const buff: SmallInt);
    procedure WriteInt32(const buff: Integer);
    procedure WriteInt64(const buff: Int64);
    procedure WriteUInt8(const buff: Byte);
    procedure WriteUInt16(const buff: Word);
    procedure WriteUInt32(const buff: Cardinal);
    procedure WriteUInt64(const buff: UInt64);
    procedure WriteSingle(const buff: Single);
    procedure WriteDouble(const buff: Double);
    procedure WriteCurrency(const buff: Currency);
    procedure WriteString(const buff: TPascalString);
    procedure WriteANSI(const buff: TPascalString); overload;
    procedure WriteANSI(const buff: TPascalString; const L: Integer); overload;
    procedure WriteMD5(const buff: TMD5);

    // Serialized reader
    function ReadBool: Boolean;
    function ReadInt8: ShortInt;
    function ReadInt16: SmallInt;
    function ReadInt32: Integer;
    function ReadInt64: Int64;
    function ReadUInt8: Byte;
    function ReadUInt16: Word;
    function ReadUInt32: Cardinal;
    function ReadUInt64: UInt64;
    function ReadSingle: Single;
    function ReadDouble: Double;
    function ReadCurrency: Currency;
    function PrepareReadString: Boolean;
    function ReadString: TPascalString;
    function ReadANSI(L: Integer): TPascalString;
    function ReadMD5: TMD5;
  end;
{$ENDIF ZDB2_Core_Used_Mem64}

implementation

var
  V_NULLData: array [Word] of Byte;

function TZDB2_Core_Space.ReadCacheBlock(buff: Pointer; ID: Integer): Boolean;
var
  p: PZDB2_Core_BlockData;
begin
  Result := False;
  p := @FPhyBlock[ID];
  with FPhyBlockWriteCache[p^.ID] do
    if (FUsedReadCache) and (p^.UsedSpace > 0) and (Mem <> nil) then
      begin
        Mem.Position := 0;
        Mem.ReadPtr(buff, p^.UsedSpace);
        Mem.Position := 0;
        Result := True;
      end;
end;

function TZDB2_Core_Space.WriteCacheBlock(buff: Pointer; siz: Integer; ID: Integer; FlushThisCache_: Boolean): Boolean;
var
  p: PZDB2_Core_BlockData;
begin
  Result := False;
  if not FUsedWriteCache then
      exit;
  p := @FPhyBlock[ID];
  with FPhyBlockWriteCache[ID] do
    begin
      if Mem = nil then
        begin
          Mem := TMem64.Create;
          Mem.Size := p^.Size;
          FillPtr(Mem.Memory, p^.Size, 0);
          inc(FState.Cache, p^.Size);
        end;
      Mem.Position := 0;
      Mem.WritePtr(buff, siz);
      Mem.Position := 0;
      FlushThisCacheToFile := FlushThisCacheToFile or FlushThisCache_;
    end;
  Result := True;
  if FState.Cache > FMaxCacheMemory then
    begin
      FlushCache();
    end;
end;

function TZDB2_Core_Space.GetTableSize: Int64;
begin
  Result := GetTableSize(FPhyBlockNum);
end;

function TZDB2_Core_Space.GetTableSize(PhyBlockNum_: Integer): Int64;
begin
  Result := 16 + SizeOf(TZDB2_Core_CustomFileHeader) + SizeOf(TZDB2_Core_BlockData) * PhyBlockNum_;
end;

constructor TZDB2_Core_Space.Create(IOHnd_: PIOHnd);
begin
  inherited Create;
  FAutoCloseIOHnd := False;
  FAutoFreeIOHnd := False;
  FSpace_IOHnd := IOHnd_;
  FillPtr(@FCustomFileHeader, SizeOf(TZDB2_Core_CustomFileHeader), 0);
  FFreeSpaceIndexProbe := 0;
  FPhyBlockNum := 0;
  SetLength(FPhyBlock, 0);
  FMaxCacheMemory := 1024 * 1024 * 64;
  FUsedReadCache := False;
  FUsedWriteCache := True;
  SetLength(FPhyBlockWriteCache, 0);
  FMode := stmNormal;

  FState.Physics := 0;
  FState.FreeSpace := 0;
  FState.Cache := 0;
  FState.ReadNum := 0;
  FState.ReadSize := 0;
  FState.WriteNum := 0;
  FState.WriteSize := 0;

  FOnProgress := nil;
  FOnNoSpace := nil;
end;

destructor TZDB2_Core_Space.Destroy;
begin
  SetLength(FPhyBlock, 0);
  ClearCache;
  SetLength(FPhyBlockWriteCache, 0);

  if FAutoCloseIOHnd then
      umlFileClose(FSpace_IOHnd^);
  if FAutoFreeIOHnd then
      Dispose(FSpace_IOHnd);
  inherited Destroy;
end;

function TZDB2_Core_Space.GetCustomFileHeader: PZDB2_Core_CustomFileHeader;
begin
  Result := @FCustomFileHeader;
end;

procedure TZDB2_Core_Space.SetMode(const Value: TZDB2_Core_SpaceMode);
begin
  FMode := Value;
  case FMode of
    stmBigData:
      begin
        FUsedReadCache := False;
        FUsedWriteCache := False;
      end;
    stmNormal:
      begin
        FUsedReadCache := False;
        FUsedWriteCache := True;
      end;
    stmFast:
      begin
        FUsedReadCache := True;
        FUsedWriteCache := True;
      end;
  end;
end;

function TZDB2_Core_Space.GetState: PZDB2_Core_SpaceState;
begin
  Result := @FState;
end;

procedure TZDB2_Core_Space.ErrorInfo(const Text_: SystemString);
begin
  DoStatus('ZDB2 Core failed - ' + Text_);
end;

procedure TZDB2_Core_Space.DeleteCache(ID: Integer);
begin
  with FPhyBlockWriteCache[ID] do
    begin
      if Mem <> nil then
        begin
          Dec(FState.Cache, Mem.Size);
          DisposeObjectAndNil(Mem);
        end;
      FlushThisCacheToFile := False;
    end;
end;

procedure TZDB2_Core_Space.ClearCache;
var
  i: Int64;
begin
  i := 0;
  while i < Length(FPhyBlockWriteCache) do
    with FPhyBlockWriteCache[i] do
      begin
        DisposeObjectAndNil(Mem);
        FlushThisCacheToFile := False;
        inc(i);
      end;
  FState.Cache := 0;
end;

procedure TZDB2_Core_Space.FlushCache;
var
  i: Integer;
begin
  i := 0;
  while i < FPhyBlockNum do
    begin
      with FPhyBlock[i], FPhyBlockWriteCache[i] do
        if Mem <> nil then
          begin
            if FlushThisCacheToFile then
              begin
                if not umlFileSeek(FSpace_IOHnd^, Position) then
                  begin
                    ErrorInfo('FlushCache: umlFileSeek error.');
                    exit;
                  end;
                if not umlBlockWrite(FSpace_IOHnd^, Mem.Memory^, UsedSpace) then
                  begin
                    ErrorInfo('FlushCache: umlBlockWrite error.');
                    exit;
                  end;
                FlushThisCacheToFile := False;
              end;
            DisposeObjectAndNil(Mem);
          end;
      inc(i);
    end;
  FState.Cache := 0;
end;

procedure TZDB2_Core_Space.WriteTable();
type
  THead_ = packed record
    head: Cardinal;
    major, minor: Word;
    num: Integer;
  end;
var
  h: THead_;
begin
  h.head := C_ZDB2_FileHead;
  h.major := 1;
  h.minor := 2;
  h.num := FPhyBlockNum;
  if not umlFileSeek(FSpace_IOHnd^, 0) then
    begin
      ErrorInfo('WriteTable: umlFileSeek error.');
      exit;
    end;
  if not umlBlockWrite(FSpace_IOHnd^, h, 12) then
    begin
      ErrorInfo('WriteTable: umlBlockWrite h error.');
      exit;
    end;
  if not umlBlockWrite(FSpace_IOHnd^, FCustomFileHeader, SizeOf(TZDB2_Core_CustomFileHeader)) then
    begin
      ErrorInfo('WriteTable: umlBlockWrite CustomFileHeader error.');
      exit;
    end;

  if FPhyBlockNum > 0 then
    if not umlBlockWrite(FSpace_IOHnd^, FPhyBlock[0], SizeOf(TZDB2_Core_BlockData) * FPhyBlockNum) then
      begin
        ErrorInfo('WriteTable: umlBlockWrite FPhyBlock error.');
        exit;
      end;
end;

procedure TZDB2_Core_Space.Save;
begin
  WriteTable;
  FlushCache;
  umlFileUpdate(FSpace_IOHnd^);
end;

procedure TZDB2_Core_Space.Open();
type
  THead_ = packed record
    head: Cardinal;
    major, minor: Word;
  end;
var
  h: THead_;
  num: Integer;
begin
  umlFileSeek(FSpace_IOHnd^, 0);
  umlBlockRead(FSpace_IOHnd^, h, 8);
  if h.head <> C_ZDB2_FileHead then
    begin
      ErrorInfo('Open: header token error.');
      exit;
    end;
  if (h.major = 1) and (h.minor = 2) then
    begin
      if not umlBlockRead(FSpace_IOHnd^, num, 4) then
        begin
          ErrorInfo('Open: umlBlockRead num error.');
          exit;
        end;
      if not umlBlockRead(FSpace_IOHnd^, FCustomFileHeader, SizeOf(TZDB2_Core_CustomFileHeader)) then
        begin
          ErrorInfo('Open: umlBlockRead FCustomFileHeader error.');
          exit;
        end;

      PreparePhyBlock(num);
      if FPhyBlockNum > 0 then
        if not umlBlockRead(FSpace_IOHnd^, FPhyBlock[0], SizeOf(TZDB2_Core_BlockData) * FPhyBlockNum) then
          begin
            ErrorInfo('Open: umlBlockRead FPhyBlock error.');
            exit;
          end;

      ScanSpace();
    end
  else
    begin
      ErrorInfo('Open: major/minor info error.');
      exit;
    end;
end;

procedure TZDB2_Core_Space.ScanSpace;
var
  i: Integer;
begin
  FFreeSpaceIndexProbe := FPhyBlockNum;
  FState.Physics := 0;
  FState.FreeSpace := 0;
  i := 0;
  while i < FPhyBlockNum do
    begin
      with FPhyBlock[i] do
        begin
          inc(FState.Physics, Size);
          if UsedSpace = 0 then
            begin
              inc(FState.FreeSpace, Size);
              if i < FFreeSpaceIndexProbe then
                  FFreeSpaceIndexProbe := i;
            end;
        end;
      inc(i);
    end;
end;

procedure TZDB2_Core_Space.PreparePhyBlock(PhyBlockNum_: Int64);
var
  i: Integer;
begin
  FFreeSpaceIndexProbe := 0;
  FPhyBlockNum := PhyBlockNum_;
  SetLength(FPhyBlock, FPhyBlockNum);
  ClearCache;
  SetLength(FPhyBlockWriteCache, FPhyBlockNum);
  FState.Physics := 0;
  FState.FreeSpace := 0;
  FState.Cache := 0;
  FState.ReadNum := 0;
  FState.ReadSize := 0;
  FState.WriteNum := 0;
  FState.WriteSize := 0;

  i := 0;
  while i < FPhyBlockNum do
    with FPhyBlock[i], FPhyBlockWriteCache[i] do
      begin
        // block
        ID := i;
        Prev := -1;
        Next := -1;
        // cache
        Mem := nil;
        FlushThisCacheToFile := False;

        inc(i);
      end;
end;

procedure TZDB2_Core_Space.BuildSpace(PhySpaceSize: Int64; BlockSize_: Word);
var
  BlockSize: Word;
  m64: TMem64;
  i: Integer;
begin
  if not umlFileSeek(FSpace_IOHnd^, 0) then
    begin
      ErrorInfo('BuildSpace: umlFileSeek 0 error.');
      exit;
    end;

  BlockSize := umlMax(BlockSize_, C_ZDB2_MinBlockSize);
  PreparePhyBlock((PhySpaceSize - GetTableSize(PhySpaceSize div BlockSize)) div BlockSize);
  m64 := TMem64.Create;
  m64.Size := GetTableSize;
  m64.Position := 0;
  FillPtr(m64.Memory, m64.Size, 0);
  if not umlBlockWrite(FSpace_IOHnd^, m64.Memory^, m64.Size) then
    begin
      ErrorInfo('BuildSpace: umlBlockWrite zero head error.');
      exit;
    end;
  DisposeObject(m64);

  i := 0;
  while i < FPhyBlockNum do
    begin
      FPhyBlock[i].Position := umlFilePOS(FSpace_IOHnd^);
      FPhyBlock[i].Size := BlockSize;
      FPhyBlock[i].UsedSpace := 0;
      FPhyBlock[i].ID := i;
      FPhyBlock[i].Next := -1;
      FPhyBlock[i].Prev := -1;
      if not umlBlockWrite(FSpace_IOHnd^, V_NULLData, BlockSize) then
        begin
          ErrorInfo('BuildSpace: umlBlockWrite NullData error.');
          exit;
        end;
      inc(i);
      if Assigned(FOnProgress) then
          FOnProgress(FPhyBlockNum, i);
    end;

  WriteTable();
  ScanSpace();
end;

procedure TZDB2_Core_Space.AppendSpace(Dest: TZDB2_Core_Space; DestPhySpaceSize: Int64; DestBlockSize_: Word);
var
  DestBlockSize: Word;
  m64: TMem64;
  buff: Pointer;
  i: Integer;
begin
  if not umlFileSeek(Dest.FSpace_IOHnd^, 0) then
    begin
      ErrorInfo('AppendSpace: umlFileSeek 0 error.');
      exit;
    end;

  DestBlockSize := umlMax(DestBlockSize_, C_ZDB2_MinBlockSize);
  Dest.PreparePhyBlock(FPhyBlockNum + (DestPhySpaceSize div DestBlockSize));
  m64 := TMem64.Create;
  m64.Size := Dest.GetTableSize;
  m64.Position := 0;
  FillPtr(m64.Memory, m64.Size, 0);
  if not umlBlockWrite(Dest.FSpace_IOHnd^, m64.Memory^, m64.Size) then
    begin
      ErrorInfo('AppendSpace: umlBlockWrite null head error.');
      exit;
    end;
  DisposeObject(m64);

  buff := System.GetMemory($FFFF);
  i := 0;
  while i < FPhyBlockNum do
    begin
      Dest.FPhyBlock[i] := FPhyBlock[i];
      Dest.FPhyBlock[i].Position := umlFilePOS(Dest.FSpace_IOHnd^);
      if not umlFileSeek(FSpace_IOHnd^, FPhyBlock[i].Position) then
        begin
          ErrorInfo('AppendSpace: umlFileSeek PhyPos error.');
          exit;
        end;
      if not umlBlockRead(FSpace_IOHnd^, buff^, FPhyBlock[i].Size) then
        begin
          ErrorInfo('AppendSpace: umlBlockRead Phy error.');
          exit;
        end;
      if not umlBlockWrite(Dest.FSpace_IOHnd^, buff^, FPhyBlock[i].Size) then
        begin
          ErrorInfo('AppendSpace: umlBlockWrite Phy error.');
          exit;
        end;
      inc(i);
      if Assigned(FOnProgress) then
          FOnProgress(FPhyBlockNum, i);
    end;
  i := FPhyBlockNum;
  FillPtr(buff, $FFFF, 0);
  while i < Dest.FPhyBlockNum do
    begin
      Dest.FPhyBlock[i].Position := umlFilePOS(Dest.FSpace_IOHnd^);
      Dest.FPhyBlock[i].Size := DestBlockSize;
      Dest.FPhyBlock[i].UsedSpace := 0;
      Dest.FPhyBlock[i].ID := i;
      Dest.FPhyBlock[i].Next := -1;
      Dest.FPhyBlock[i].Prev := -1;
      if not umlBlockWrite(Dest.FSpace_IOHnd^, V_NULLData, DestBlockSize) then
        begin
          ErrorInfo('AppendSpace: umlBlockWrite NullData error.');
          exit;
        end;
      inc(i);
      if Assigned(FOnProgress) then
          FOnProgress(FPhyBlockNum, i);
    end;
  System.FreeMemory(buff);

  Dest.WriteTable();
  Dest.ScanSpace();
end;

procedure TZDB2_Core_Space.OptimizeInstance(Dest: TZDB2_Core_Space);
var
  m64: TMem64;
  buff: Pointer;
  i: Integer;
begin
  if not umlFileSeek(Dest.FSpace_IOHnd^, 0) then
    begin
      ErrorInfo('OptimizeInstance: umlFileSeek 0 error.');
      exit;
    end;
  Dest.PreparePhyBlock(FPhyBlockNum);
  m64 := TMem64.Create;
  m64.Size := GetTableSize;
  m64.Position := 0;
  FillPtr(m64.Memory, m64.Size, 0);
  if not umlBlockWrite(Dest.FSpace_IOHnd^, m64.Memory^, m64.Size) then
    begin
      ErrorInfo('OptimizeInstance: umlBlockWrite Null Head error.');
      exit;
    end;
  DisposeObject(m64);

  buff := System.GetMemory($FFFF);
  i := 0;
  while i < FPhyBlockNum do
    begin
      Dest.FPhyBlock[i] := FPhyBlock[i];
      Dest.FPhyBlock[i].Position := umlFilePOS(Dest.FSpace_IOHnd^);
      if not umlFileSeek(FSpace_IOHnd^, FPhyBlock[i].Position) then
        begin
          ErrorInfo('OptimizeInstance: umlFileSeek Phy error.');
          exit;
        end;
      if not umlBlockRead(FSpace_IOHnd^, buff^, FPhyBlock[i].Size) then
        begin
          ErrorInfo('OptimizeInstance: umlBlockRead Phy error.');
          exit;
        end;
      if not umlBlockWrite(Dest.FSpace_IOHnd^, buff^, FPhyBlock[i].Size) then
        begin
          ErrorInfo('OptimizeInstance: umlBlockWrite Phy error.');
          exit;
        end;
      inc(i);
      if Assigned(FOnProgress) then
          FOnProgress(FPhyBlockNum, i);
    end;
  System.FreeMemory(buff);

  Dest.WriteTable();
  Dest.ScanSpace();
end;

function TZDB2_Core_Space.Check(ID_: Integer): Boolean;
var
  ID, i, num: Integer;
begin
  Result := False;

  if FPhyBlockNum = 0 then
      exit;

  ID := ID_;
  if (ID < 0) or (ID >= FPhyBlockNum) then
      exit;

  while FPhyBlock[ID].Prev >= 0 do
    if (ID >= 0) and (ID < FPhyBlockNum) and (FPhyBlock[ID].UsedSpace > 0) then
        ID := FPhyBlock[ID].Prev
    else
        exit;

  num := 0;
  i := ID;
  repeat
    if FPhyBlock[i].UsedSpace = 0 then
        exit;
    inc(num);
    i := FPhyBlock[i].Next;
  until i < 0;

  Result := True;
end;

function TZDB2_Core_Space.GetSpaceHndID(ID_: Integer): Integer;
var
  ID, i, num: Integer;
begin
  Result := -1;

  if FPhyBlockNum = 0 then
      exit;

  ID := ID_;
  if (ID < 0) or (ID >= FPhyBlockNum) then
      exit;

  while FPhyBlock[ID].Prev >= 0 do
    if (ID >= 0) and (ID < FPhyBlockNum) and (FPhyBlock[ID].UsedSpace > 0) then
        ID := FPhyBlock[ID].Prev
    else
        exit;

  num := 0;
  i := ID;
  repeat
    if FPhyBlock[i].UsedSpace = 0 then
        exit;
    inc(num);
    i := FPhyBlock[i].Next;
  until i < 0;

  Result := ID;
end;

function TZDB2_Core_Space.GetSpaceHnd(ID_: Integer): TZDB2_Core_BlockHnd;
var
  ID, i, num: Integer;
begin
  SetLength(Result, 0);

  ID := ID_;
  if ID < 0 then
      exit;

  while FPhyBlock[ID].Prev >= 0 do
    if (ID >= 0) and (ID < FPhyBlockNum) and (FPhyBlock[ID].UsedSpace > 0) then
        ID := FPhyBlock[ID].Prev
    else
        exit;

  num := 0;
  i := ID;
  repeat
    if FPhyBlock[i].UsedSpace = 0 then
        exit;
    inc(num);
    i := FPhyBlock[i].Next;
  until i < 0;

  SetLength(Result, num);
  i := ID;
  num := 0;
  repeat
    Result[num] := i;
    inc(num);
    i := FPhyBlock[i].Next;
  until i < 0;
end;

function TZDB2_Core_Space.CheckWriteSpace(Siz_: Int64): Boolean;
begin
  Result := CheckWriteSpace(Siz_, nil);
end;

function TZDB2_Core_Space.CheckWriteSpace(Siz_: Int64; Space_: TZDB2_Core_BlockPtrList): Boolean;
var
  tmp: Int64;
  i: Integer;
begin
  if Space_ <> nil then
      Space_.Clear;
  Result := False;
  if Siz_ = 0 then
      exit;
  tmp := 0;
  i := FFreeSpaceIndexProbe;
  while (i < FPhyBlockNum) and (tmp < Siz_) do
    with FPhyBlock[i] do
      begin
        if UsedSpace = 0 then
          begin
            inc(tmp, Size);
            if Space_ <> nil then
                Space_.Add(@FPhyBlock[i]);
          end;
        inc(i);
      end;
  Result := tmp >= Siz_;
end;

function TZDB2_Core_Space.WriteData(buff: TMem64; var SpaceHnd: TZDB2_Core_BlockHnd): Boolean;
var
  Space_: TZDB2_Core_BlockPtrList;
  tmp: Int64;
  i, j: Integer;
  p: Pointer;
  n: TZDB2_Core_BlockPtrList;
  retry: Boolean;
begin
  Result := False;

  if buff.Size = 0 then
    begin
      ErrorInfo('WriteData: buff size 0.');
      exit;
    end;

  { compute space }
  Space_ := TZDB2_Core_BlockPtrList.Create;
  if not CheckWriteSpace(buff.Size, Space_) then
    begin
      DisposeObject(Space_);
      retry := False;
      if Assigned(FOnNoSpace) then
          FOnNoSpace(buff.Size, retry);
      if retry then
          Result := WriteData(buff, SpaceHnd)
      else
          ErrorInfo('WriteData: No Space.');
      exit;
    end;

  SetLength(SpaceHnd, Space_.Count);

  { fill block }
  tmp := buff.Size;
  p := buff.Memory;
  i := 0;
  while i < Space_.Count do
    with Space_[i]^ do
      begin
        if tmp > Size then
          begin
            if not WriteCacheBlock(p, Size, ID, True) then
              begin
                if not umlFileSeek(FSpace_IOHnd^, Position) then
                  begin
                    ErrorInfo('WriteData: umlFileSeek Block error.');
                    exit;
                  end;
                if not umlBlockWrite(FSpace_IOHnd^, p^, Size) then
                  begin
                    ErrorInfo('WriteData: umlBlockWrite Block error.');
                    exit;
                  end;
              end;

            Dec(FState.FreeSpace, Size);

            UsedSpace := Size;
            Dec(tmp, Size);
            p := GetOffset(p, Size);
            SpaceHnd[i] := ID;
            inc(i);
          end
        else
          begin
            if not WriteCacheBlock(p, tmp, ID, True) then
              begin
                if not umlFileSeek(FSpace_IOHnd^, Position) then
                  begin
                    ErrorInfo('WriteData: umlFileSeek trail Block error.');
                    exit;
                  end;
                if not umlBlockWrite(FSpace_IOHnd^, p^, tmp) then
                  begin
                    ErrorInfo('WriteData: umlBlockWrite trail Block error.');
                    exit;
                  end;
              end;

            Dec(FState.FreeSpace, Size);

            UsedSpace := tmp;
            SpaceHnd[i] := ID;
            inc(i);
            Result := True;
            break;
          end;
      end;

  DisposeObject(Space_);

  // fill link
  j := 0;
  FPhyBlock[SpaceHnd[0]].Prev := -1;
  while j < Length(SpaceHnd) do
    begin
      if j > 0 then
        begin
          FPhyBlock[SpaceHnd[j - 1]].Next := SpaceHnd[j];
          FPhyBlock[SpaceHnd[j]].Prev := SpaceHnd[j - 1];
        end;
      inc(j);
    end;
  FPhyBlock[SpaceHnd[j - 1]].Next := -1;

  // chagne state
  inc(FState.WriteNum);
  inc(FState.WriteSize, buff.Size);

  // prepare probe for next
  FFreeSpaceIndexProbe := FPhyBlockNum;
  i := FPhyBlock[SpaceHnd[j - 1]].ID + 1;
  while i < FPhyBlockNum do
    with FPhyBlock[i] do
      begin
        if UsedSpace = 0 then
          begin
            FFreeSpaceIndexProbe := i;
            break;
          end
        else
            inc(i);
      end;
end;

function TZDB2_Core_Space.WriteData(buff: TMem64; var ID: Integer): Boolean;
var
  SpaceHnd: TZDB2_Core_BlockHnd;
begin
  Result := WriteData(buff, SpaceHnd);
  if Result then
      ID := SpaceHnd[0];
  SetLength(SpaceHnd, 0);
end;

function TZDB2_Core_Space.ReadData(buff: TMem64; SpaceHnd: TZDB2_Core_BlockHnd): Boolean;
var
  i: Integer;
  Siz_: Int64;
  p: Pointer;
begin
  Result := False;

  if Length(SpaceHnd) = 0 then
    begin
      ErrorInfo('ReadData: SpaceHnd null error.');
      exit;
    end;

  { compute queue space }
  i := 0;
  Siz_ := 0;
  while i < Length(SpaceHnd) do
    with FPhyBlock[SpaceHnd[i]] do
      begin
        inc(Siz_, UsedSpace);
        inc(i);
      end;

  { prepare memory }
  buff.Size := Siz_;

  if Siz_ = 0 then
      exit;

  { read }
  i := 0;
  p := buff.Memory;
  while i < Length(SpaceHnd) do
    with FPhyBlock[SpaceHnd[i]] do
      begin
        if not ReadCacheBlock(p, ID) then
          begin
            if not umlFileSeek(FSpace_IOHnd^, Position) then
              begin
                ErrorInfo('ReadData: umlFileSeek error.');
                exit;
              end;
            if not umlBlockRead(FSpace_IOHnd^, p^, UsedSpace) then
              begin
                ErrorInfo('ReadData: umlBlockRead error.');
                exit;
              end;
            if FUsedReadCache then
                WriteCacheBlock(p, UsedSpace, ID, False);
          end;
        p := GetOffset(p, UsedSpace);
        inc(i);
      end;

  inc(FState.ReadNum);
  inc(FState.ReadSize, Siz_);
  Result := True;
end;

function TZDB2_Core_Space.ReadData(buff: TMem64; ID: Integer): Boolean;
begin
  Result := ReadData(buff, GetSpaceHnd(ID));
end;

function TZDB2_Core_Space.RemoveData(SpaceHnd: TZDB2_Core_BlockHnd; SafeClean_: Boolean): Boolean;
var
  i: Integer;
begin
  Result := (Length(SpaceHnd) > 0) and Check(SpaceHnd[0]);
  i := 0;
  while i < Length(SpaceHnd) do
    with FPhyBlock[SpaceHnd[i]] do
      begin
        DeleteCache(ID);
        UsedSpace := 0;
        Prev := -1;
        Next := -1;

        if ID < FFreeSpaceIndexProbe then
            FFreeSpaceIndexProbe := ID;
        inc(FState.FreeSpace, Size);

        { safe remove }
        if SafeClean_ then
          if not WriteCacheBlock(@V_NULLData, Size, ID, True) then
            begin
              if not umlFileSeek(FSpace_IOHnd^, Position) then
                begin
                  ErrorInfo('RemoveData: umlFileSeek error.');
                  exit;
                end;
              if not umlBlockWrite(FSpace_IOHnd^, V_NULLData, Size) then
                begin
                  ErrorInfo('RemoveData: umlBlockWrite error.');
                  exit;
                end;
            end;

        inc(i);
      end;
end;

function TZDB2_Core_Space.RemoveData(ID: Integer; SafeClean_: Boolean): Boolean;
begin
  Result := RemoveData(GetSpaceHnd(ID), SafeClean_);
end;

function TZDB2_Core_Space.GetDataSize(SpaceHnd: TZDB2_Core_BlockHnd): Int64;
var
  i: Integer;
begin
  Result := 0;
  i := 0;
  while i < Length(SpaceHnd) do
    with FPhyBlock[SpaceHnd[i]] do
      begin
        inc(Result, UsedSpace);
        inc(i);
      end;
end;

function TZDB2_Core_Space.GetDataSize(ID: Integer): Int64;
begin
  Result := GetDataSize(GetSpaceHnd(ID));
end;

function TZDB2_Core_Space.GetDataPhysics(SpaceHnd: TZDB2_Core_BlockHnd): Int64;
var
  i: Integer;
begin
  Result := 0;
  i := 0;
  while i < Length(SpaceHnd) do
    with FPhyBlock[SpaceHnd[i]] do
      begin
        inc(Result, Size);
        inc(i);
      end;
end;

function TZDB2_Core_Space.GetDataPhysics(ID: Integer): Int64;
begin
  Result := GetDataPhysics(GetSpaceHnd(ID));
end;

class procedure TZDB2_Core_Space.Test;
var
  st1, st2, st3: TZDB2_Core_Space;
  hnd1, hnd2, hnd3: TIOHnd;

  q, q2: TZDB2_Core_BlockHnd;
  m64: TMem64;
begin
  InitIOHnd(hnd1);
  InitIOHnd(hnd2);
  InitIOHnd(hnd3);
  umlFileCreateAsMemory(hnd1);
  umlFileCreateAsMemory(hnd2);
  umlFileCreateAsMemory(hnd3);

  st1 := TZDB2_Core_Space.Create(@hnd1);
  st2 := TZDB2_Core_Space.Create(@hnd2);
  st3 := TZDB2_Core_Space.Create(@hnd3);

  st1.BuildSpace(1024 * 1024 * 50, 512);
  st1.AppendSpace(st2, 1024 * 1024 * 2, 1024);
  st1.OptimizeInstance(st3);

  m64 := TMem64.Create;
  m64.Size := 1024 * 1024 * 20;
  st1.WriteData(m64, q2);
  st1.Save;
  st1.WriteData(m64, q);
  st1.Save;
  m64.Clear;
  st1.Open;
  st1.ReadData(m64, q);
  st1.ReadData(m64, q[2]);
  st1.RemoveData(q, True);
  st1.ScanSpace;
  DisposeObject(m64);

  m64 := TMem64.Create;
  m64.Size := 64;
  st1.WriteData(m64, q);
  st1.Save;
  m64.Clear;
  st1.Open;
  st1.ReadData(m64, q);
  st1.ReadData(m64, q[0]);
  st1.RemoveData(q, True);
  st1.ScanSpace;
  DisposeObject(m64);

  m64 := TMem64.Create;
  m64.Size := 1024;
  st1.WriteData(m64, q);
  st1.Save;
  m64.Clear;
  st1.Open;
  st1.ReadData(m64, q);
  st1.ReadData(m64, q[0]);
  st1.RemoveData(q, True);
  st1.ScanSpace;
  DisposeObject(m64);

  st1.RemoveData(q2, True);
  st1.ScanSpace;

  DisposeObject(st1);
  DisposeObject(st2);
  DisposeObject(st3);
  umlFileClose(hnd1);
  umlFileClose(hnd2);
  umlFileClose(hnd3);

  DoStatus('TZDB2_Core_Space.Test passed.');
end;

{$IFDEF ZDB2_Core_Used_Mem64}


procedure TZDB2_Core_Mem64.SetPointer(buffPtr: Pointer; const BuffSize: Int64);
begin
  FMemory := buffPtr;
  FSize := BuffSize;
end;

procedure TZDB2_Core_Mem64.SetCapacity(NewCapacity: Int64);
begin
  if FProtectedMode then
      exit;
  SetPointer(Realloc(NewCapacity), FSize);
  FCapacity := NewCapacity;
end;

function TZDB2_Core_Mem64.Realloc(var NewCapacity: Int64): Pointer;
begin
  if FProtectedMode then
      exit(nil);

  if (NewCapacity > 0) and (NewCapacity <> FSize) then
      NewCapacity := DeltaStep(NewCapacity, FDelta);
  Result := Memory;
  if NewCapacity <> FCapacity then
    begin
      if NewCapacity = 0 then
        begin
          System.FreeMemory(Memory);
          Result := nil;
        end
      else
        begin
          if Capacity = 0 then
              Result := System.GetMemory(NewCapacity)
          else
              Result := System.ReallocMemory(Result, NewCapacity);
          if Result = nil then
              RaiseInfo('Out of memory while expanding memory stream');
        end;
    end;
end;

function TZDB2_Core_Mem64.GetPosition: Int64;
begin
  Result := Seek(0, TSeekOrigin.soCurrent);
end;

procedure TZDB2_Core_Mem64.SetPosition(const Value: Int64);
begin
  Seek(Value, TSeekOrigin.soBeginning);
end;

function TZDB2_Core_Mem64.GetSize: Int64;
var
  Pos: Int64;
begin
  Pos := Seek(0, TSeekOrigin.soCurrent);
  Result := Seek(0, TSeekOrigin.soEnd);
  Seek(Pos, TSeekOrigin.soBeginning);
end;

procedure TZDB2_Core_Mem64.SetSize(const NewSize: Int64);
var
  OldPosition: Int64;
begin
  if FProtectedMode then
      exit;

  OldPosition := FPosition;
  SetCapacity(NewSize);
  FSize := NewSize;
  if OldPosition > NewSize then
      Seek(0, TSeekOrigin.soEnd);
end;

constructor TZDB2_Core_Mem64.Create;
begin
  CustomCreate(256);
end;

constructor TZDB2_Core_Mem64.CustomCreate(const customDelta: NativeInt);
begin
  inherited Create;
  FDelta := customDelta;
  FMemory := nil;
  FSize := 0;
  FPosition := 0;
  FCapacity := 0;
  FProtectedMode := False;
end;

destructor TZDB2_Core_Mem64.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TZDB2_Core_Mem64.DiscardMemory;
begin
  if FProtectedMode then
      exit;
  FMemory := nil;
  FSize := 0;
  FPosition := 0;
  FCapacity := 0;
end;

procedure TZDB2_Core_Mem64.Clear;
begin
  if FProtectedMode then
      exit;
  SetCapacity(0);
  FSize := 0;
  FPosition := 0;
end;

procedure TZDB2_Core_Mem64.NewParam(source: TZDB2_Core_Mem64);
begin
  Clear;
  FDelta := source.FDelta;
  FMemory := source.FMemory;
  FSize := source.FSize;
  FPosition := source.FPosition;
  FCapacity := source.FCapacity;
  FProtectedMode := source.FProtectedMode;
end;

procedure TZDB2_Core_Mem64.SwapInstance(source: TZDB2_Core_Mem64);
var
  FDelta_: NativeInt;
  FMemory_: Pointer;
  FSize_: Int64;
  FPosition_: Int64;
  FCapacity_: Int64;
  FProtectedMode_: Boolean;
begin
  FDelta_ := FDelta;
  FMemory_ := FMemory;
  FSize_ := FSize;
  FPosition_ := FPosition;
  FCapacity_ := FCapacity;
  FProtectedMode_ := FProtectedMode;

  FDelta := source.FDelta;
  FMemory := source.FMemory;
  FSize := source.FSize;
  FPosition := source.FPosition;
  FCapacity := source.FCapacity;
  FProtectedMode := source.FProtectedMode;

  source.FDelta := FDelta_;
  source.FMemory := FMemory_;
  source.FSize := FSize_;
  source.FPosition := FPosition_;
  source.FCapacity := FCapacity_;
  source.FProtectedMode := FProtectedMode_;
end;

procedure TZDB2_Core_Mem64.SetPointerWithProtectedMode(buffPtr: Pointer; const BuffSize: Int64);
begin
  Clear;
  FMemory := buffPtr;
  FSize := BuffSize;
  FPosition := 0;
  FProtectedMode := True;
end;

procedure TZDB2_Core_Mem64.Mapping(buffPtr: Pointer; const BuffSize: Int64);
begin
  SetPointerWithProtectedMode(buffPtr, BuffSize);
end;

function TZDB2_Core_Mem64.PositionAsPtr(const Position_: Int64): Pointer;
begin
  Result := GetOffset(FMemory, Position_);
end;

function TZDB2_Core_Mem64.PositionAsPtr: Pointer;
begin
  Result := GetOffset(FMemory, FPosition);
end;

function TZDB2_Core_Mem64.PosAsPtr(const Position_: Int64): Pointer;
begin
  Result := PositionAsPtr(Position_);
end;

function TZDB2_Core_Mem64.PosAsPtr: Pointer;
begin
  Result := PositionAsPtr();
end;

procedure TZDB2_Core_Mem64.LoadFromStream(stream: TCoreClassStream);
const
  ChunkSize = 64 * 1024 * 1024;
var
  p: Pointer;
  j: NativeInt;
  num: NativeInt;
  Rest: NativeInt;
begin
  if FProtectedMode then
      exit;

  stream.Position := 0;
  SetSize(stream.Size);
  if stream.Size > 0 then
    begin
      p := FMemory;
      if stream.Size > ChunkSize then
        begin
          { Calculate number of full chunks that will fit into the buffer }
          num := stream.Size div ChunkSize;
          { Calculate remaining bytes }
          Rest := stream.Size mod ChunkSize;

          { Process full chunks }
          for j := 0 to num - 1 do
            begin
              stream.ReadBuffer(p^, ChunkSize);
              p := GetOffset(p, ChunkSize);
            end;

          { Process remaining bytes }
          if Rest > 0 then
            begin
              stream.ReadBuffer(p^, Rest);
              p := GetOffset(p, Rest);
            end;
        end
      else
          stream.ReadBuffer(p^, stream.Size);
    end;
end;

procedure TZDB2_Core_Mem64.LoadFromFile(FileName: SystemString);
var
  stream: TCoreClassStream;
begin
  stream := TCoreClassFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
      LoadFromStream(stream);
  finally
      DisposeObject(stream);
  end;
end;

procedure TZDB2_Core_Mem64.SaveToStream(stream: TCoreClassStream);
const
  ChunkSize = 64 * 1024 * 1024;
var
  p: Pointer;
  j: NativeInt;
  num: NativeInt;
  Rest: NativeInt;
begin
  if Size > 0 then
    begin
      p := FMemory;
      if Size > ChunkSize then
        begin
          { Calculate number of full chunks that will fit into the buffer }
          num := Size div ChunkSize;
          { Calculate remaining bytes }
          Rest := Size mod ChunkSize;

          { Process full chunks }
          for j := 0 to num - 1 do
            begin
              stream.WriteBuffer(p^, ChunkSize);
              p := GetOffset(p, ChunkSize);
            end;

          { Process remaining bytes }
          if Rest > 0 then
            begin
              stream.WriteBuffer(p^, Rest);
              p := GetOffset(p, Rest);
            end;
        end
      else
          stream.WriteBuffer(p^, Size);
    end;
end;

procedure TZDB2_Core_Mem64.SaveToFile(FileName: SystemString);
var
  stream: TCoreClassStream;
begin
  stream := TCoreClassFileStream.Create(FileName, fmCreate);
  try
      SaveToStream(stream);
  finally
      DisposeObject(stream);
  end;
end;

function TZDB2_Core_Mem64.Write64(const buffer; Count: Int64): Int64;
var
  p: Int64;
begin
  if (Count > 0) then
    begin
      p := FPosition;
      p := p + Count;
      if p > 0 then
        begin
          if p > FSize then
            begin
              if FProtectedMode then
                begin
                  Result := 0;
                  exit;
                end;
              if p > FCapacity then
                  SetCapacity(p);
              FSize := p;
            end;
          CopyPtr(@buffer, GetOffset(FMemory, FPosition), Count);
          FPosition := p;
          Result := Count;
          exit;
        end;
    end;
  Result := 0;
end;

function TZDB2_Core_Mem64.WritePtr(const p: Pointer; Count: Int64): Int64;
begin
  Result := Write64(p^, Count);
end;

function TZDB2_Core_Mem64.Write(const buffer; Count: Int64): Int64;
begin
  Result := Write64(buffer, Count);
end;

function TZDB2_Core_Mem64.Read64(var buffer; Count: Int64): Int64;
begin
  if Count > 0 then
    begin
      Result := FSize;
      Result := Result - FPosition;
      if Result > 0 then
        begin
          if Result > Count then
              Result := Count;
          CopyPtr(GetOffset(FMemory, FPosition), @buffer, Result);
          inc(FPosition, Result);
          exit;
        end;
    end;
  Result := 0;
end;

function TZDB2_Core_Mem64.ReadPtr(const p: Pointer; Count: Int64): Int64;
begin
  Result := Read64(p^, Count);
end;

function TZDB2_Core_Mem64.Read(var buffer; Count: Int64): Int64;
begin
  Result := Read64(buffer, Count);
end;

function TZDB2_Core_Mem64.Seek(const Offset: Int64; origin: TSeekOrigin): Int64;
begin
  case origin of
    TSeekOrigin.soBeginning: FPosition := Offset;
    TSeekOrigin.soCurrent: inc(FPosition, Offset);
    TSeekOrigin.soEnd: FPosition := FSize + Offset;
  end;
  Result := FPosition;
end;

procedure TZDB2_Core_Mem64.WriteBool(const buff: Boolean);
begin
  WritePtr(@buff, 1);
end;

procedure TZDB2_Core_Mem64.WriteInt8(const buff: ShortInt);
begin
  WritePtr(@buff, 1);
end;

procedure TZDB2_Core_Mem64.WriteInt16(const buff: SmallInt);
begin
  WritePtr(@buff, 2);
end;

procedure TZDB2_Core_Mem64.WriteInt32(const buff: Integer);
begin
  WritePtr(@buff, 4);
end;

procedure TZDB2_Core_Mem64.WriteInt64(const buff: Int64);
begin
  WritePtr(@buff, 8);
end;

procedure TZDB2_Core_Mem64.WriteUInt8(const buff: Byte);
begin
  WritePtr(@buff, 1);
end;

procedure TZDB2_Core_Mem64.WriteUInt16(const buff: Word);
begin
  WritePtr(@buff, 2);
end;

procedure TZDB2_Core_Mem64.WriteUInt32(const buff: Cardinal);
begin
  WritePtr(@buff, 4);
end;

procedure TZDB2_Core_Mem64.WriteUInt64(const buff: UInt64);
begin
  WritePtr(@buff, 8);
end;

procedure TZDB2_Core_Mem64.WriteSingle(const buff: Single);
begin
  WritePtr(@buff, 4);
end;

procedure TZDB2_Core_Mem64.WriteDouble(const buff: Double);
begin
  WritePtr(@buff, 8);
end;

procedure TZDB2_Core_Mem64.WriteCurrency(const buff: Currency);
begin
  WriteDouble(buff);
end;

procedure TZDB2_Core_Mem64.WriteString(const buff: TPascalString);
var
  b: TBytes;
begin
  b := buff.Bytes;
  WriteUInt32(Length(b));
  if Length(b) > 0 then
    begin
      WritePtr(@b[0], Length(b));
      SetLength(b, 0);
    end;
end;

procedure TZDB2_Core_Mem64.WriteANSI(const buff: TPascalString);
var
  b: TBytes;
begin
  b := buff.ANSI;
  if Length(b) > 0 then
    begin
      WritePtr(@b[0], Length(b));
      SetLength(b, 0);
    end;
end;

procedure TZDB2_Core_Mem64.WriteANSI(const buff: TPascalString; const L: Integer);
var
  b: TBytes;
begin
  b := buff.ANSI;
  if L > 0 then
    begin
      WritePtr(@b[0], L);
      SetLength(b, 0);
    end;
end;

procedure TZDB2_Core_Mem64.WriteMD5(const buff: TMD5);
begin
  WritePtr(@buff, 16);
end;

function TZDB2_Core_Mem64.ReadBool: Boolean;
begin
  ReadPtr(@Result, 1);
end;

function TZDB2_Core_Mem64.ReadInt8: ShortInt;
begin
  ReadPtr(@Result, 1);
end;

function TZDB2_Core_Mem64.ReadInt16: SmallInt;
begin
  ReadPtr(@Result, 2);
end;

function TZDB2_Core_Mem64.ReadInt32: Integer;
begin
  ReadPtr(@Result, 4);
end;

function TZDB2_Core_Mem64.ReadInt64: Int64;
begin
  ReadPtr(@Result, 8);
end;

function TZDB2_Core_Mem64.ReadUInt8: Byte;
begin
  ReadPtr(@Result, 1);
end;

function TZDB2_Core_Mem64.ReadUInt16: Word;
begin
  ReadPtr(@Result, 2);
end;

function TZDB2_Core_Mem64.ReadUInt32: Cardinal;
begin
  ReadPtr(@Result, 4);
end;

function TZDB2_Core_Mem64.ReadUInt64: UInt64;
begin
  ReadPtr(@Result, 8);
end;

function TZDB2_Core_Mem64.ReadSingle: Single;
begin
  ReadPtr(@Result, 4);
end;

function TZDB2_Core_Mem64.ReadDouble: Double;
begin
  ReadPtr(@Result, 8);
end;

function TZDB2_Core_Mem64.ReadCurrency: Currency;
begin
  Result := ReadDouble();
end;

function TZDB2_Core_Mem64.PrepareReadString: Boolean;
begin
  Result := (Position + 4 <= Size) and (Position + 4 + PCardinal(PositionAsPtr())^ <= Size);
end;

function TZDB2_Core_Mem64.ReadString: TPascalString;
var
  L: Cardinal;
  b: TBytes;
begin
  L := ReadUInt32;
  if L > 0 then
    begin
      SetLength(b, L);
      ReadPtr(@b[0], L);
      Result.Bytes := b;
      SetLength(b, 0);
    end;
end;

function TZDB2_Core_Mem64.ReadANSI(L: Integer): TPascalString;
var
  b: TBytes;
begin
  if L > 0 then
    begin
      SetLength(b, L);
      ReadPtr(@b[0], L);
      Result.ANSI := b;
      SetLength(b, 0);
    end;
end;

function TZDB2_Core_Mem64.ReadMD5: TMD5;
begin
  ReadPtr(@Result, 16);
end;

{$ENDIF ZDB2_Core_Used_Mem64}

initialization

FillPtr(@V_NULLData, $FFFF, 0);

finalization

end.
