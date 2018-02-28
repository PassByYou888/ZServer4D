{ ****************************************************************************** }
{ * support > 2G TMemoryStream64, writen by QQ 600585@qq.com                   * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ ****************************************************************************** }

unit MemoryStream64;

{$I zDefine.inc}

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
  CoreClasses, PascalStrings;

type
  TMemoryStream64 = class(TCoreClassStream)
  private
    FMemory       : Pointer;
    FSize         : NativeUInt;
    FPosition     : NativeUInt;
    FCapacity     : NativeUInt;
    FProtectedMode: Boolean;
  protected
    procedure SetPointer(BuffPtr: Pointer; const BuffSize: NativeUInt);
    procedure SetCapacity(NewCapacity: NativeUInt);
    function Realloc(var NewCapacity: NativeUInt): Pointer; virtual;
    property Capacity: NativeUInt read FCapacity write SetCapacity;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;

    procedure SetPointerWithProtectedMode(BuffPtr: Pointer; const BuffSize: NativeUInt); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function PositionAsPtr(const APosition: Int64): Pointer; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function PositionAsPtr: Pointer; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    //
    procedure LoadFromStream(Stream: TCoreClassStream); virtual;
    procedure LoadFromFile(const FileName: SystemString);
    procedure SaveToStream(Stream: TCoreClassStream); virtual;
    procedure SaveToFile(const FileName: SystemString);

    procedure SetSize(const NewSize: Int64); overload; override;
    procedure SetSize(NewSize: Longint); overload; override;

    function Write64(const Buffer; Count: Int64): Int64; virtual;
    function WritePtr(const p: Pointer; Count: Int64): Int64; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Write(const Buffer; Count: Longint): Longint; overload; override;
    {$IFNDEF FPC} function Write(const Buffer: TBytes; Offset, Count: Longint): Longint; overload; override; {$ENDIF}
    //
    function Read64(var Buffer; Count: Int64): Int64; virtual;
    function ReadPtr(const p: Pointer; Count: Int64): Int64; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Read(var Buffer; Count: Longint): Longint; overload; override;
    {$IFNDEF FPC} function Read(Buffer: TBytes; Offset, Count: Longint): Longint; overload; override; {$ENDIF}
    //
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    property Memory: Pointer read FMemory;

    function CopyFrom(const Source: TCoreClassStream; cCount: Int64): Int64; virtual;
  end;

  IMemoryStream64WriteTrigger = interface
    procedure TriggerWrite64(Count: Int64);
  end;

  TMemoryStream64OfWriteTrigger = class(TMemoryStream64)
  private
  public
    Trigger: IMemoryStream64WriteTrigger;
    constructor Create(ATrigger: IMemoryStream64WriteTrigger);
    function Write64(const Buffer; Count: Int64): Int64; override;
  end;

  IMemoryStream64ReadTrigger = interface
    procedure TriggerRead64(Count: Int64);
  end;

  TMemoryStream64OfReadTrigger = class(TMemoryStream64)
  private
  public
    Trigger: IMemoryStream64ReadTrigger;
    constructor Create(ATrigger: IMemoryStream64ReadTrigger);
    function Read64(var Buffer; Count: Int64): Int64; override;
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
    function Read64(var Buffer; Count: Int64): Int64; override;
    function Write64(const Buffer; Count: Int64): Int64; override;
  end;

  {$IFDEF FPC}

  TDecompressionStream = class(zstream.TDecompressionStream)
  public
  end;

  { TCompressionStream }

  TCompressionStream = class(zstream.TCompressionStream)
  public
    constructor Create(Stream: TCoreClassStream); overload;
    constructor Create(level: Tcompressionlevel; Stream: TCoreClassStream); overload;
  end;
  {$ELSE}

  TDecompressionStream = TZDecompressionStream;
  TCompressionStream   = TZCompressionStream;
  {$ENDIF}
  //
  // zlib
function MaxCompressStream(Sour: TCoreClassStream; ComTo: TCoreClassStream): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function FastCompressStream(Sour: TCoreClassStream; ComTo: TCoreClassStream): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function CompressStream(Sour: TCoreClassStream; ComTo: TCoreClassStream): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function DecompressStream(Sour: TCoreClassStream; DeTo: TCoreClassStream): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function DecompressStreamToPtr(Sour: TCoreClassStream; var DeTo: Pointer): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}


implementation

uses UnicodeMixedLib;

procedure TMemoryStream64.SetPointer(BuffPtr: Pointer; const BuffSize: NativeUInt);
begin
  FMemory := BuffPtr;
  FSize := BuffSize;
end;

procedure TMemoryStream64.SetCapacity(NewCapacity: NativeUInt);
begin
  if FProtectedMode then
      exit;
  SetPointer(Realloc(NewCapacity), FSize);
  FCapacity := NewCapacity;
end;

function TMemoryStream64.Realloc(var NewCapacity: NativeUInt): Pointer;
begin
  if FProtectedMode then
      exit(nil);

  if (NewCapacity > 0) and (NewCapacity <> FSize) then
      NewCapacity := umlDeltaNumber(NewCapacity, 256);
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
              raiseInfo('Out of memory while expanding memory stream');
        end;
    end;
end;

constructor TMemoryStream64.Create;
begin
  inherited Create;
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
      exit;
  SetCapacity(0);
  FSize := 0;
  FPosition := 0;
end;

procedure TMemoryStream64.SetPointerWithProtectedMode(BuffPtr: Pointer; const BuffSize: NativeUInt);
begin
  Clear;
  FMemory := BuffPtr;
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

procedure TMemoryStream64.LoadFromStream(Stream: TCoreClassStream);
const
  ChunkSize = 64 * 1024 * 1024;
var
  p   : Pointer;
  j   : NativeInt;
  Num : NativeInt;
  Rest: NativeInt;
begin
  if FProtectedMode then
      exit;

  Stream.Position := 0;
  SetSize(Stream.Size);
  if Stream.Size > 0 then
    begin
      p := FMemory;
      if Stream.Size > ChunkSize then
        begin
          { Calculate number of full chunks that will fit into the buffer }
          Num := Stream.Size div ChunkSize;
          { Calculate remaining bytes }
          Rest := Stream.Size mod ChunkSize;

          { Process full chunks }
          for j := 0 to Num - 1 do
            begin
              Stream.ReadBuffer(p^, ChunkSize);
              p := Pointer(NativeUInt(p) + ChunkSize);
            end;

          { Process remaining bytes }
          if Rest > 0 then
            begin
              Stream.ReadBuffer(p^, Rest);
              p := Pointer(NativeUInt(p) + Rest);
            end;
        end
      else
          Stream.ReadBuffer(p^, Stream.Size);
    end;
end;

procedure TMemoryStream64.LoadFromFile(const FileName: SystemString);
var
  Stream: TCoreClassStream;
begin
  Stream := TCoreClassFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
      LoadFromStream(Stream);
  finally
      DisposeObject(Stream);
  end;
end;

procedure TMemoryStream64.SaveToStream(Stream: TCoreClassStream);
const
  ChunkSize = 64 * 1024 * 1024;
var
  p   : Pointer;
  j   : NativeInt;
  Num : NativeInt;
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
              Stream.WriteBuffer(p^, ChunkSize);
              p := Pointer(NativeUInt(p) + ChunkSize);
            end;

          { Process remaining bytes }
          if Rest > 0 then
            begin
              Stream.WriteBuffer(p^, Rest);
              p := Pointer(NativeUInt(p) + Rest);
            end;
        end
      else
          Stream.WriteBuffer(p^, Size);
    end;
end;

procedure TMemoryStream64.SaveToFile(const FileName: SystemString);
var
  Stream: TCoreClassStream;
begin
  Stream := TCoreClassFileStream.Create(FileName, fmCreate);
  try
      SaveToStream(Stream);
  finally
      DisposeObject(Stream);
  end;
end;

procedure TMemoryStream64.SetSize(const NewSize: Int64);
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

procedure TMemoryStream64.SetSize(NewSize: Longint);
begin
  SetSize(Int64(NewSize));
end;

function TMemoryStream64.Write64(const Buffer; Count: Int64): Int64;
var
  p: Int64;
begin
  if FProtectedMode then
    begin
      Result := 0;
      exit;
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
          System.Move(Buffer, PByte(NativeUInt(FMemory) + FPosition)^, Count);
          FPosition := p;
          Result := Count;
          exit;
        end;
    end;
  Result := 0;
end;

function TMemoryStream64.WritePtr(const p: Pointer; Count: Int64): Int64;
begin
  Result := Write64(p^, Count);
end;

function TMemoryStream64.Write(const Buffer; Count: Longint): Longint;
begin
  Result := Write64(Buffer, Count);
end;

{$IFNDEF FPC}


function TMemoryStream64.Write(const Buffer: TBytes; Offset, Count: Longint): Longint;
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
          System.Move(Buffer[Offset], PByte(NativeUInt(FMemory) + FPosition)^, Count);
          FPosition := p;
          Result := Count;
          exit;
        end;
    end;
  Result := 0;
end;
{$ENDIF}


function TMemoryStream64.Read64(var Buffer; Count: Int64): Int64;
begin
  if Count > 0 then
    begin
      Result := FSize;
      Result := Result - FPosition;
      if Result > 0 then
        begin
          if Result > Count then
              Result := Count;
          System.Move(PByte(NativeUInt(FMemory) + FPosition)^, Buffer, Result);
          Inc(FPosition, Result);
          exit;
        end;
    end;
  Result := 0;
end;

function TMemoryStream64.ReadPtr(const p: Pointer; Count: Int64): Int64;
begin
  Result := Read64(p^, Count);
end;

function TMemoryStream64.Read(var Buffer; Count: Longint): Longint;
begin
  Result := Read64(Buffer, Count);
end;

{$IFNDEF FPC}


function TMemoryStream64.Read(Buffer: TBytes; Offset, Count: Longint): Longint;
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

          System.Move(PByte(NativeUInt(FMemory) + FPosition)^, Buffer[Offset], p);
          Inc(FPosition, p);
          Result := p;
          exit;
        end;
    end;
  Result := 0;
end;
{$ENDIF}


function TMemoryStream64.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  case Origin of
    TSeekOrigin.soBeginning: FPosition := Offset;
    TSeekOrigin.soCurrent: Inc(FPosition, Offset);
    TSeekOrigin.soEnd: FPosition := FSize + Offset;
  end;
  Result := FPosition;
end;

function TMemoryStream64.CopyFrom(const Source: TCoreClassStream; cCount: Int64): Int64;
const
  MaxBufSize = $F000;
var
  BufSize, N: Int64;
  Buffer    : TBytes;
begin
  if FProtectedMode then
      raiseInfo('protected mode');

  if Source is TMemoryStream64 then
    begin
      WritePtr(TMemoryStream64(Source).PositionAsPtr, cCount);
      TMemoryStream64(Source).Position := TMemoryStream64(Source).FPosition + cCount;
      Result := cCount;
      exit;
    end;

  if cCount <= 0 then
    begin
      Source.Position := 0;
      cCount := Source.Size;
    end;

  Result := cCount;
  if cCount > MaxBufSize then
      BufSize := MaxBufSize
  else
      BufSize := cCount;
  SetLength(Buffer, BufSize);
  try
    while cCount <> 0 do
      begin
        if cCount > BufSize then
            N := BufSize
        else
            N := cCount;
        Source.Read((@Buffer[0])^, N);
        WritePtr((@Buffer[0]), N);
        Dec(cCount, N);
      end;
  finally
      SetLength(Buffer, 0);
  end;
end;

constructor TMemoryStream64OfWriteTrigger.Create(ATrigger: IMemoryStream64WriteTrigger);
begin
  inherited Create;
  Trigger := ATrigger;
end;

function TMemoryStream64OfWriteTrigger.Write64(const Buffer; Count: Int64): Int64;
begin
  Result := inherited Write64(Buffer, Count);
  if Assigned(Trigger) then
      Trigger.TriggerWrite64(Count);
end;

constructor TMemoryStream64OfReadTrigger.Create(ATrigger: IMemoryStream64ReadTrigger);
begin
  inherited Create;
  Trigger := ATrigger;
end;

function TMemoryStream64OfReadTrigger.Read64(var Buffer; Count: Int64): Int64;
begin
  Result := inherited Read64(Buffer, Count);
  if Assigned(Trigger) then
      Trigger.TriggerRead64(Count);
end;

constructor TMemoryStream64OfReadWriteTrigger.Create(ATrigger: IMemoryStream64ReadWriteTrigger);
begin
  inherited Create;
  Trigger := ATrigger;
end;

function TMemoryStream64OfReadWriteTrigger.Read64(var Buffer; Count: Int64): Int64;
begin
  Result := inherited Read64(Buffer, Count);
  if Assigned(Trigger) then
      Trigger.TriggerRead64(Count);
end;

function TMemoryStream64OfReadWriteTrigger.Write64(const Buffer; Count: Int64): Int64;
begin
  Result := inherited Write64(Buffer, Count);
  if Assigned(Trigger) then
      Trigger.TriggerWrite64(Count);
end;

{$IFDEF FPC}


constructor TCompressionStream.Create(Stream: TCoreClassStream);
begin
  inherited Create(clFastest, Stream);
end;

constructor TCompressionStream.Create(level: Tcompressionlevel; Stream: TCoreClassStream);
begin
  inherited Create(level, Stream);
end;
{$ENDIF}


function MaxCompressStream(Sour: TCoreClassStream; ComTo: TCoreClassStream): Boolean;
var
  cp       : TCompressionStream;
  sizevalue: Int64;
begin
  Result := False;
  try
    sizevalue := Sour.Size;
    ComTo.WriteBuffer(sizevalue, 8);
    if Sour.Size > 0 then
      begin
        Sour.Position := 0;
        cp := TCompressionStream.Create(clMax, ComTo);
        Result := cp.CopyFrom(Sour, sizevalue) = sizevalue;
        DisposeObject(cp);
      end;
  except
  end;
end;

function FastCompressStream(Sour: TCoreClassStream; ComTo: TCoreClassStream): Boolean;
var
  cp       : TCompressionStream;
  sizevalue: Int64;
begin
  Result := False;
  try
    sizevalue := Sour.Size;
    ComTo.WriteBuffer(sizevalue, 8);
    if Sour.Size > 0 then
      begin
        Sour.Position := 0;
        cp := TCompressionStream.Create(clFastest, ComTo);
        Result := cp.CopyFrom(Sour, sizevalue) = sizevalue;
        DisposeObject(cp);
      end;
  except
  end;
end;

function CompressStream(Sour: TCoreClassStream; ComTo: TCoreClassStream): Boolean;
var
  cp       : TCompressionStream;
  sizevalue: Int64;
begin
  Result := False;
  try
    sizevalue := Sour.Size;
    ComTo.WriteBuffer(sizevalue, 8);
    if Sour.Size > 0 then
      begin
        Sour.Position := 0;
        cp := TCompressionStream.Create(clDefault, ComTo);
        Result := cp.CopyFrom(Sour, sizevalue) = sizevalue;
        DisposeObject(cp);
      end;
  except
  end;
end;

function DecompressStream(Sour: TCoreClassStream; DeTo: TCoreClassStream): Boolean;
var
  DC    : TDecompressionStream;
  DeSize: Int64;
begin
  Result := False;
  Sour.ReadBuffer(DeSize, 8);
  if DeSize > 0 then
    begin
      try
        DC := TDecompressionStream.Create(Sour);
        Result := DeTo.CopyFrom(DC, DeSize) = DeSize;
        DisposeObject(DC);
      except
      end;
    end;
end;

function DecompressStreamToPtr(Sour: TCoreClassStream; var DeTo: Pointer): Boolean;
var
  DC    : TDecompressionStream;
  DeSize: Int64;
begin
  Result := False;
  try
    Sour.ReadBuffer(DeSize, 8);
    if DeSize > 0 then
      begin
        DC := TDecompressionStream.Create(Sour);
        DeTo := System.GetMemory(DeSize);
        Result := DC.Read(DeTo^, DeSize) = DeSize;
        DisposeObject(DC);
      end;
  except
  end;
end;

initialization

finalization

end.
