{ ****************************************************************************** }
{ * support > 2G TMemoryStream64, writen by QQ 600585@qq.com                   * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }

unit MemoryStream64;

{$INCLUDE zDefine.inc}

{
  create by passbyyou
  first 2011-10

  last 2017-11-2 added x64 memory interface
  2017-12-29 added newCompressor
}

interface

uses
  SysUtils, ZLib,
{$IFDEF FPC}
  zstream,
{$ENDIF}
  CoreClasses, PascalStrings, UnicodeMixedLib;

type
  TMemoryStream64 = class(TCoreClassStream)
  private
    FDelta: NativeInt;
    FMemory: Pointer;
    FSize: NativeUInt;
    FPosition: NativeUInt;
    FCapacity: NativeUInt;
    FProtectedMode: Boolean;
  protected
    procedure SetPointer(buffPtr: Pointer; const BuffSize: NativeUInt);
    procedure SetCapacity(NewCapacity: NativeUInt);
    function Realloc(var NewCapacity: NativeUInt): Pointer; virtual;
    property Capacity: NativeUInt read FCapacity write SetCapacity;
  public
    constructor Create;
    constructor CustomCreate(const customDelta: NativeInt);
    destructor Destroy; override;
    procedure Clear;

    property Delta: NativeInt read FDelta write FDelta;
    procedure SetPointerWithProtectedMode(buffPtr: Pointer; const BuffSize: NativeUInt);
    function PositionAsPtr(const APosition: Int64): Pointer; overload;
    function PositionAsPtr: Pointer; overload;

    procedure LoadFromStream(stream: TCoreClassStream); virtual;
    procedure LoadFromFile(const FileName: SystemString);
    procedure SaveToStream(stream: TCoreClassStream); virtual;
    procedure SaveToFile(const FileName: SystemString);

    procedure SetSize(const NewSize: Int64); overload; override;
    procedure SetSize(NewSize: longint); overload; override;

    function Write64(const buffer; Count: Int64): Int64; virtual;
    function WritePtr(const p: Pointer; Count: Int64): Int64;
    function write(const buffer; Count: longint): longint; overload; override;
{$IFNDEF FPC} function write(const buffer: TBytes; Offset, Count: longint): longint; overload; override; {$ENDIF}
    //
    function Read64(var buffer; Count: Int64): Int64; virtual;
    function ReadPtr(const p: Pointer; Count: Int64): Int64;
    function read(var buffer; Count: longint): longint; overload; override;
{$IFNDEF FPC} function read(buffer: TBytes; Offset, Count: longint): longint; overload; override; {$ENDIF}
    //
    function Seek(const Offset: Int64; origin: TSeekOrigin): Int64; override;
    property Memory: Pointer read FMemory;

    function CopyFrom(const Source: TCoreClassStream; CCount: Int64): Int64; virtual;

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
    procedure WriteMD5(const buff: TMD5);

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
    function ReadMD5: TMD5;
  end;

  IMemoryStream64WriteTrigger = interface
    procedure TriggerWrite64(Count: Int64);
  end;

  TMemoryStream64OfWriteTrigger = class(TMemoryStream64)
  private
  public
    Trigger: IMemoryStream64WriteTrigger;
    constructor Create(ATrigger: IMemoryStream64WriteTrigger);
    function Write64(const buffer; Count: Int64): Int64; override;
  end;

  IMemoryStream64ReadTrigger = interface
    procedure TriggerRead64(Count: Int64);
  end;

  TMemoryStream64OfReadTrigger = class(TMemoryStream64)
  private
  public
    Trigger: IMemoryStream64ReadTrigger;
    constructor Create(ATrigger: IMemoryStream64ReadTrigger);
    function Read64(var buffer; Count: Int64): Int64; override;
  end;

  IMemoryStream64ReadWriteTrigger = interface
    procedure TriggerWrite64(Count: Int64);
    procedure TriggerRead64(Count: Int64);
  end;

  TMemoryStream64OfReadWriteTrigger = class(TMemoryStream64)
  private
  public
    Trigger: IMemoryStream64ReadWriteTrigger;
    constructor Create(ATrigger: IMemoryStream64ReadWriteTrigger);
    function Read64(var buffer; Count: Int64): Int64; override;
    function Write64(const buffer; Count: Int64): Int64; override;
  end;

{$IFDEF FPC}

  TDecompressionStream = class(zstream.TDecompressionStream)
  public
  end;

  { TCompressionStream }

  TCompressionStream = class(zstream.TCompressionStream)
  public
    constructor Create(stream: TCoreClassStream); overload;
    constructor Create(level: Tcompressionlevel; stream: TCoreClassStream); overload;
  end;
{$ELSE}

  TDecompressionStream = ZLib.TZDecompressionStream;
  TCompressionStream = ZLib.TZCompressionStream;
{$ENDIF}
  //
  // zlib
function MaxCompressStream(sour: TCoreClassStream; ComTo: TCoreClassStream): Boolean;
function FastCompressStream(sour: TCoreClassStream; ComTo: TCoreClassStream): Boolean;
function CompressStream(sour: TCoreClassStream; ComTo: TCoreClassStream): Boolean;

function DecompressStream(DataPtr: Pointer; siz: NativeInt; dest: TCoreClassStream): Boolean; overload;
function DecompressStream(sour: TCoreClassStream; dest: TCoreClassStream): Boolean; overload;
function DecompressStreamToPtr(sour: TCoreClassStream; var dest: Pointer): Boolean;

procedure DoStatus(const v: TMemoryStream64); overload;

implementation

uses DoStatusIO;

procedure TMemoryStream64.SetPointer(buffPtr: Pointer; const BuffSize: NativeUInt);
begin
  FMemory := buffPtr;
  FSize := BuffSize;
end;

procedure TMemoryStream64.SetCapacity(NewCapacity: NativeUInt);
begin
  if FProtectedMode then
      Exit;
  SetPointer(Realloc(NewCapacity), FSize);
  FCapacity := NewCapacity;
end;

function TMemoryStream64.Realloc(var NewCapacity: NativeUInt): Pointer;
begin
  if FProtectedMode then
      Exit(nil);

  if (NewCapacity > 0) and (NewCapacity <> FSize) then
      NewCapacity := umlDeltaNumber(NewCapacity, FDelta);
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

constructor TMemoryStream64.Create;
begin
  CustomCreate(256);
end;

constructor TMemoryStream64.CustomCreate(const customDelta: NativeInt);
begin
  inherited Create;
  FDelta := customDelta;
  FMemory := nil;
  FSize := 0;
  FPosition := 0;
  FCapacity := 0;
  FProtectedMode := False;
end;

destructor TMemoryStream64.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TMemoryStream64.Clear;
begin
  if FProtectedMode then
      Exit;
  SetCapacity(0);
  FSize := 0;
  FPosition := 0;
end;

procedure TMemoryStream64.SetPointerWithProtectedMode(buffPtr: Pointer; const BuffSize: NativeUInt);
begin
  Clear;
  FMemory := buffPtr;
  FSize := BuffSize;
  FPosition := 0;
  FProtectedMode := True;
end;

function TMemoryStream64.PositionAsPtr(const APosition: Int64): Pointer;
begin
  Result := Pointer(NativeUInt(FMemory) + APosition);
end;

function TMemoryStream64.PositionAsPtr: Pointer;
begin
  Result := Pointer(NativeUInt(FMemory) + FPosition);
end;

procedure TMemoryStream64.LoadFromStream(stream: TCoreClassStream);
const
  ChunkSize = 64 * 1024 * 1024;
var
  p: Pointer;
  j: NativeInt;
  Num: NativeInt;
  Rest: NativeInt;
begin
  if FProtectedMode then
      Exit;

  stream.Position := 0;
  SetSize(stream.Size);
  if stream.Size > 0 then
    begin
      p := FMemory;
      if stream.Size > ChunkSize then
        begin
          { Calculate number of full chunks that will fit into the buffer }
          Num := stream.Size div ChunkSize;
          { Calculate remaining bytes }
          Rest := stream.Size mod ChunkSize;

          { Process full chunks }
          for j := 0 to Num - 1 do
            begin
              stream.ReadBuffer(p^, ChunkSize);
              p := Pointer(NativeUInt(p) + ChunkSize);
            end;

          { Process remaining bytes }
          if Rest > 0 then
            begin
              stream.ReadBuffer(p^, Rest);
              p := Pointer(NativeUInt(p) + Rest);
            end;
        end
      else
          stream.ReadBuffer(p^, stream.Size);
    end;
end;

procedure TMemoryStream64.LoadFromFile(const FileName: SystemString);
var
  stream: TCoreClassStream;
begin
  stream := TCoreClassFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
      LoadFromStream(stream);
  finally
      DisposeObject(stream);
  end;
end;

procedure TMemoryStream64.SaveToStream(stream: TCoreClassStream);
const
  ChunkSize = 64 * 1024 * 1024;
var
  p: Pointer;
  j: NativeInt;
  Num: NativeInt;
  Rest: NativeInt;
begin
  if Size > 0 then
    begin
      p := FMemory;
      if Size > ChunkSize then
        begin
          { Calculate number of full chunks that will fit into the buffer }
          Num := Size div ChunkSize;
          { Calculate remaining bytes }
          Rest := Size mod ChunkSize;

          { Process full chunks }
          for j := 0 to Num - 1 do
            begin
              stream.WriteBuffer(p^, ChunkSize);
              p := Pointer(NativeUInt(p) + ChunkSize);
            end;

          { Process remaining bytes }
          if Rest > 0 then
            begin
              stream.WriteBuffer(p^, Rest);
              p := Pointer(NativeUInt(p) + Rest);
            end;
        end
      else
          stream.WriteBuffer(p^, Size);
    end;
end;

procedure TMemoryStream64.SaveToFile(const FileName: SystemString);
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

procedure TMemoryStream64.SetSize(const NewSize: Int64);
var
  OldPosition: Int64;
begin
  if FProtectedMode then
      Exit;

  OldPosition := FPosition;
  SetCapacity(NewSize);
  FSize := NewSize;
  if OldPosition > NewSize then
      Seek(0, TSeekOrigin.soEnd);
end;

procedure TMemoryStream64.SetSize(NewSize: longint);
begin
  SetSize(Int64(NewSize));
end;

function TMemoryStream64.Write64(const buffer; Count: Int64): Int64;
var
  p: Int64;
begin
  if FProtectedMode then
    begin
      Result := 0;
      Exit;
    end;

  if (Count > 0) then
    begin
      p := FPosition;
      p := p + Count;
      if p > 0 then
        begin
          if p > FSize then
            begin
              if p > FCapacity then
                  SetCapacity(p);
              FSize := p;
            end;
          CopyPtr(@buffer, PByte(NativeUInt(FMemory) + FPosition), Count);
          FPosition := p;
          Result := Count;
          Exit;
        end;
    end;
  Result := 0;
end;

function TMemoryStream64.WritePtr(const p: Pointer; Count: Int64): Int64;
begin
  Result := Write64(p^, Count);
end;

function TMemoryStream64.write(const buffer; Count: longint): longint;
begin
  Result := Write64(buffer, Count);
end;

{$IFNDEF FPC}


function TMemoryStream64.write(const buffer: TBytes; Offset, Count: longint): longint;
var
  p: Int64;
begin
  if Count > 0 then
    begin
      p := FPosition;
      p := p + Count;
      if p > 0 then
        begin
          if p > FSize then
            begin
              if p > FCapacity then
                  SetCapacity(p);
              FSize := p;
            end;
          CopyPtr(@buffer[Offset], PByte(NativeUInt(FMemory) + FPosition), Count);
          FPosition := p;
          Result := Count;
          Exit;
        end;
    end;
  Result := 0;
end;
{$ENDIF}


function TMemoryStream64.Read64(var buffer; Count: Int64): Int64;
begin
  if Count > 0 then
    begin
      Result := FSize;
      Result := Result - FPosition;
      if Result > 0 then
        begin
          if Result > Count then
              Result := Count;
          CopyPtr(PByte(NativeUInt(FMemory) + FPosition), @buffer, Result);
          inc(FPosition, Result);
          Exit;
        end;
    end;
  Result := 0;
end;

function TMemoryStream64.ReadPtr(const p: Pointer; Count: Int64): Int64;
begin
  Result := Read64(p^, Count);
end;

function TMemoryStream64.read(var buffer; Count: longint): longint;
begin
  Result := Read64(buffer, Count);
end;

{$IFNDEF FPC}


function TMemoryStream64.read(buffer: TBytes; Offset, Count: longint): longint;
var
  p: Int64;
begin
  if Count > 0 then
    begin
      p := FSize;
      p := p - FPosition;
      if p > 0 then
        begin
          if p > Count then
              p := Count;

          CopyPtr(PByte(NativeUInt(FMemory) + FPosition), @buffer[Offset], p);
          inc(FPosition, p);
          Result := p;
          Exit;
        end;
    end;
  Result := 0;
end;
{$ENDIF}


function TMemoryStream64.Seek(const Offset: Int64; origin: TSeekOrigin): Int64;
begin
  case origin of
    TSeekOrigin.soBeginning: FPosition := Offset;
    TSeekOrigin.soCurrent: inc(FPosition, Offset);
    TSeekOrigin.soEnd: FPosition := FSize + Offset;
  end;
  Result := FPosition;
end;

function TMemoryStream64.CopyFrom(const Source: TCoreClassStream; CCount: Int64): Int64;
const
  MaxBufSize = $F000;
var
  BufSize, n: Int64;
  buffer: TBytes;
begin
  if FProtectedMode then
      RaiseInfo('protected mode');

  if Source is TMemoryStream64 then
    begin
      WritePtr(TMemoryStream64(Source).PositionAsPtr, CCount);
      TMemoryStream64(Source).Position := TMemoryStream64(Source).FPosition + CCount;
      Result := CCount;
      Exit;
    end;

  if CCount <= 0 then
    begin
      Source.Position := 0;
      CCount := Source.Size;
    end;

  Result := CCount;
  if CCount > MaxBufSize then
      BufSize := MaxBufSize
  else
      BufSize := CCount;
  SetLength(buffer, BufSize);
  try
    while CCount <> 0 do
      begin
        if CCount > BufSize then
            n := BufSize
        else
            n := CCount;
        Source.read((@buffer[0])^, n);
        WritePtr((@buffer[0]), n);
        dec(CCount, n);
      end;
  finally
      SetLength(buffer, 0);
  end;
end;

procedure TMemoryStream64.WriteBool(const buff: Boolean);
begin
  WritePtr(@buff, 1);
end;

procedure TMemoryStream64.WriteInt8(const buff: ShortInt);
begin
  WritePtr(@buff, 1);
end;

procedure TMemoryStream64.WriteInt16(const buff: SmallInt);
begin
  WritePtr(@buff, 2);
end;

procedure TMemoryStream64.WriteInt32(const buff: Integer);
begin
  WritePtr(@buff, 4);
end;

procedure TMemoryStream64.WriteInt64(const buff: Int64);
begin
  WritePtr(@buff, 8);
end;

procedure TMemoryStream64.WriteUInt8(const buff: Byte);
begin
  WritePtr(@buff, 1);
end;

procedure TMemoryStream64.WriteUInt16(const buff: Word);
begin
  WritePtr(@buff, 2);
end;

procedure TMemoryStream64.WriteUInt32(const buff: Cardinal);
begin
  WritePtr(@buff, 4);
end;

procedure TMemoryStream64.WriteUInt64(const buff: UInt64);
begin
  WritePtr(@buff, 8);
end;

procedure TMemoryStream64.WriteSingle(const buff: Single);
begin
  WritePtr(@buff, 4);
end;

procedure TMemoryStream64.WriteDouble(const buff: Double);
begin
  WritePtr(@buff, 8);
end;

procedure TMemoryStream64.WriteCurrency(const buff: Currency);
begin
  WriteDouble(buff);
end;

procedure TMemoryStream64.WriteString(const buff: TPascalString);
var
  b: TBytes;
begin
  b := buff.Bytes;
  WriteUInt32(length(b));
  WritePtr(@b[0], length(b));
  SetLength(b, 0);
end;

procedure TMemoryStream64.WriteMD5(const buff: TMD5);
begin
  WritePtr(@buff, 16);
end;

function TMemoryStream64.ReadBool: Boolean;
begin
  ReadPtr(@Result, 1);
end;

function TMemoryStream64.ReadInt8: ShortInt;
begin
  ReadPtr(@Result, 1);
end;

function TMemoryStream64.ReadInt16: SmallInt;
begin
  ReadPtr(@Result, 2);
end;

function TMemoryStream64.ReadInt32: Integer;
begin
  ReadPtr(@Result, 4);
end;

function TMemoryStream64.ReadInt64: Int64;
begin
  ReadPtr(@Result, 8);
end;

function TMemoryStream64.ReadUInt8: Byte;
begin
  ReadPtr(@Result, 1);
end;

function TMemoryStream64.ReadUInt16: Word;
begin
  ReadPtr(@Result, 2);
end;

function TMemoryStream64.ReadUInt32: Cardinal;
begin
  ReadPtr(@Result, 4);
end;

function TMemoryStream64.ReadUInt64: UInt64;
begin
  ReadPtr(@Result, 8);
end;

function TMemoryStream64.ReadSingle: Single;
begin
  ReadPtr(@Result, 4);
end;

function TMemoryStream64.ReadDouble: Double;
begin
  ReadPtr(@Result, 8);
end;

function TMemoryStream64.ReadCurrency: Currency;
begin
  Result := ReadDouble();
end;

function TMemoryStream64.PrepareReadString: Boolean;
begin
  Result := (Position + 4 <= Size) and (Position + 4 + PCardinal(PositionAsPtr())^ <= Size);
end;

function TMemoryStream64.ReadString: TPascalString;
var
  l: Cardinal;
  b: TBytes;
begin
  l := ReadUInt32;
  SetLength(b, l);
  ReadPtr(@b[0], l);
  Result.Bytes := b;
  SetLength(b, 0);
end;

function TMemoryStream64.ReadMD5: TMD5;
begin
  ReadPtr(@Result, 16);
end;

constructor TMemoryStream64OfWriteTrigger.Create(ATrigger: IMemoryStream64WriteTrigger);
begin
  inherited Create;
  Trigger := ATrigger;
end;

function TMemoryStream64OfWriteTrigger.Write64(const buffer; Count: Int64): Int64;
begin
  Result := inherited Write64(buffer, Count);
  if Assigned(Trigger) then
      Trigger.TriggerWrite64(Count);
end;

constructor TMemoryStream64OfReadTrigger.Create(ATrigger: IMemoryStream64ReadTrigger);
begin
  inherited Create;
  Trigger := ATrigger;
end;

function TMemoryStream64OfReadTrigger.Read64(var buffer; Count: Int64): Int64;
begin
  Result := inherited Read64(buffer, Count);
  if Assigned(Trigger) then
      Trigger.TriggerRead64(Count);
end;

constructor TMemoryStream64OfReadWriteTrigger.Create(ATrigger: IMemoryStream64ReadWriteTrigger);
begin
  inherited Create;
  Trigger := ATrigger;
end;

function TMemoryStream64OfReadWriteTrigger.Read64(var buffer; Count: Int64): Int64;
begin
  Result := inherited Read64(buffer, Count);
  if Assigned(Trigger) then
      Trigger.TriggerRead64(Count);
end;

function TMemoryStream64OfReadWriteTrigger.Write64(const buffer; Count: Int64): Int64;
begin
  Result := inherited Write64(buffer, Count);
  if Assigned(Trigger) then
      Trigger.TriggerWrite64(Count);
end;

{$IFDEF FPC}


constructor TCompressionStream.Create(stream: TCoreClassStream);
begin
  inherited Create(clFastest, stream);
end;

constructor TCompressionStream.Create(level: Tcompressionlevel; stream: TCoreClassStream);
begin
  inherited Create(level, stream);
end;
{$ENDIF}


function MaxCompressStream(sour: TCoreClassStream; ComTo: TCoreClassStream): Boolean;
var
  cStream: TCompressionStream;
  sizevalue: Int64;
begin
  Result := False;
  try
    sizevalue := sour.Size;
    ComTo.WriteBuffer(sizevalue, 8);
    if sour.Size > 0 then
      begin
        sour.Position := 0;
        cStream := TCompressionStream.Create(clMax, ComTo);
        Result := cStream.CopyFrom(sour, sizevalue) = sizevalue;
        DisposeObject(cStream);
      end;
  except
  end;
end;

function FastCompressStream(sour: TCoreClassStream; ComTo: TCoreClassStream): Boolean;
var
  cStream: TCompressionStream;
  sizevalue: Int64;
begin
  Result := False;
  try
    sizevalue := sour.Size;
    ComTo.WriteBuffer(sizevalue, 8);
    if sour.Size > 0 then
      begin
        sour.Position := 0;
        cStream := TCompressionStream.Create(clFastest, ComTo);
        Result := cStream.CopyFrom(sour, sizevalue) = sizevalue;
        DisposeObject(cStream);
      end;
  except
  end;
end;

function CompressStream(sour: TCoreClassStream; ComTo: TCoreClassStream): Boolean;
var
  cStream: TCompressionStream;
  sizevalue: Int64;
begin
  Result := False;
  try
    sizevalue := sour.Size;
    ComTo.WriteBuffer(sizevalue, 8);
    if sour.Size > 0 then
      begin
        sour.Position := 0;
        cStream := TCompressionStream.Create(clDefault, ComTo);
        Result := cStream.CopyFrom(sour, sizevalue) = sizevalue;
        DisposeObject(cStream);
      end;
  except
  end;
end;

function DecompressStream(DataPtr: Pointer; siz: NativeInt; dest: TCoreClassStream): Boolean;
var
  m64: TMemoryStream64;
begin
  m64 := TMemoryStream64.Create;
  m64.SetPointer(DataPtr, siz);
  Result := DecompressStream(m64, dest);
  DisposeObject(m64);
end;

function DecompressStream(sour: TCoreClassStream; dest: TCoreClassStream): Boolean;
var
  dcStream: TDecompressionStream;
  dSiz: Int64;
  iPos: Int64;
begin
  Result := False;
  sour.ReadBuffer(dSiz, 8);
  if dSiz > 0 then
    begin
      iPos := dest.Position;
      dest.Size := iPos + dSiz;
      dest.Position := iPos;
      try
        dcStream := TDecompressionStream.Create(sour);
        Result := dest.CopyFrom(dcStream, dSiz) = dSiz;
        DisposeObject(dcStream);
      except
      end;
    end;
end;

function DecompressStreamToPtr(sour: TCoreClassStream; var dest: Pointer): Boolean;
var
  dcStream: TDecompressionStream;
  dSiz: Int64;
begin
  Result := False;
  try
    sour.ReadBuffer(dSiz, 8);
    if dSiz > 0 then
      begin
        dcStream := TDecompressionStream.Create(sour);
        dest := System.GetMemory(dSiz);
        Result := dcStream.read(dest^, dSiz) = dSiz;
        DisposeObject(dcStream);
      end;
  except
  end;
end;

procedure DoStatus(const v: TMemoryStream64);
var
  p: PByte;
  i: Integer;
  n: SystemString;
begin
  p := v.Memory;
  for i := 0 to v.Size - 1 do
    begin
      if n <> '' then
          n := n + ',' + IntToStr(p^)
      else
          n := IntToStr(p^);
      inc(p);
    end;
  DoStatus(IntToHex(NativeInt(v), SizeOf(Pointer)) + ':' + n);
end;

initialization

finalization

end.
