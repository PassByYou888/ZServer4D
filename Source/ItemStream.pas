{ ****************************************************************************** }
{ * ObjectDB Stream         by qq600585                                        * }
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
(*
  update history
*)

unit ItemStream;

{$INCLUDE zDefine.inc}

interface

uses SysUtils, CoreClasses, Classes, UnicodeMixedLib, ObjectData, ObjectDataManager, PascalStrings;

type
  TItemStream = class(TCoreClassStream)
  private
    DB_Engine: TObjectDataManager;
    ItemHnd_Ptr: PItemHandle;
    AutoFreeHnd: Boolean;
  protected
    function GetSize: Int64; override;
  public
    constructor Create(eng_: TObjectDataManager; DBPath, DBItem: SystemString); overload;
    constructor Create(eng_: TObjectDataManager; var ItemHnd: TItemHandle); overload;
    constructor Create(eng_: TObjectDataManager; const ItemHeaderPos: Int64); overload;
    destructor Destroy; override;

    procedure SaveToFile(fn: SystemString);
    procedure LoadFromFile(fn: SystemString);

    function read(var buffer; Count: longint): longint; override;
    function write(const buffer; Count: longint): longint; override;

    function Seek(Offset: longint; origin: Word): longint; overload; override;
    function Seek(const Offset: Int64; origin: TSeekOrigin): Int64; overload; override;

    function CopyFrom64(const Source: TCoreClassStream; Count: Int64): Int64;

    procedure SeekStart;
    procedure SeekLast;
    function UpdateHandle: Boolean;
    function CloseHandle: Boolean;
    property Hnd: PItemHandle read ItemHnd_Ptr;
  end;

implementation

function TItemStream.GetSize: Int64;
begin
  Result := DB_Engine.ItemGetSize(ItemHnd_Ptr^);
end;

constructor TItemStream.Create(eng_: TObjectDataManager; DBPath, DBItem: SystemString);
begin
  inherited Create;
  DB_Engine := eng_;
  New(ItemHnd_Ptr);
  eng_.ItemAutoOpenOrCreate(DBPath, DBItem, DBItem, ItemHnd_Ptr^);
  AutoFreeHnd := True;
end;

constructor TItemStream.Create(eng_: TObjectDataManager; var ItemHnd: TItemHandle);
begin
  inherited Create;
  DB_Engine := eng_;
  ItemHnd_Ptr := @ItemHnd;
  AutoFreeHnd := False;
end;

constructor TItemStream.Create(eng_: TObjectDataManager; const ItemHeaderPos: Int64);
begin
  inherited Create;
  DB_Engine := eng_;
  New(ItemHnd_Ptr);
  eng_.ItemFastOpen(ItemHeaderPos, ItemHnd_Ptr^);
  AutoFreeHnd := True;
end;

destructor TItemStream.Destroy;
begin
  UpdateHandle();
  if AutoFreeHnd then
    begin
      Dispose(ItemHnd_Ptr);
      ItemHnd_Ptr := nil;
    end;
  inherited Destroy;
end;

procedure TItemStream.SaveToFile(fn: SystemString);
var
  stream: TCoreClassStream;
begin
  stream := TCoreClassFileStream.Create(fn, fmCreate);
  try
      stream.CopyFrom(Self, Size);
  finally
      DisposeObject(stream);
  end;
end;

procedure TItemStream.LoadFromFile(fn: SystemString);
var
  stream: TCoreClassStream;
begin
  stream := TCoreClassFileStream.Create(fn, fmOpenRead or fmShareDenyNone);
  try
      CopyFrom(stream, stream.Size);
  finally
      DisposeObject(stream);
  end;
end;

function TItemStream.read(var buffer; Count: longint): longint;
var
  _P: Int64;
  _Size: Int64;
begin
  Result := 0;
  if (Count > 0) then
    begin
      _P := DB_Engine.ItemGetPos(ItemHnd_Ptr^);
      _Size := DB_Engine.ItemGetSize(ItemHnd_Ptr^);
      if _P + Count <= _Size then
        begin
          if DB_Engine.ItemRead(ItemHnd_Ptr^, Count, PByte(@buffer)^) then
              Result := Count;
        end
      else if DB_Engine.ItemRead(ItemHnd_Ptr^, _Size - _P, PByte(@buffer)^) then
          Result := _Size - _P;
    end;
end;

function TItemStream.write(const buffer; Count: longint): longint;
begin
  Result := Count;
  if (Count > 0) then
    if not DB_Engine.ItemWrite(ItemHnd_Ptr^, Count, PByte(@buffer)^) then
      begin
        Result := 0;
      end;
end;

function TItemStream.Seek(Offset: longint; origin: Word): longint;
begin
  case origin of
    soFromBeginning:
      begin
        DB_Engine.ItemSeek(ItemHnd_Ptr^, Offset);
      end;
    soFromCurrent:
      begin
        if Offset <> 0 then
            DB_Engine.ItemSeek(ItemHnd_Ptr^, DB_Engine.ItemGetPos(ItemHnd_Ptr^) + Offset);
      end;
    soFromEnd:
      begin
        DB_Engine.ItemSeek(ItemHnd_Ptr^, DB_Engine.ItemGetSize(ItemHnd_Ptr^) + Offset);
      end;
  end;
  Result := DB_Engine.ItemGetPos(ItemHnd_Ptr^);
end;

function TItemStream.Seek(const Offset: Int64; origin: TSeekOrigin): Int64;
begin
  case origin of
    TSeekOrigin.soBeginning:
      begin
        DB_Engine.ItemSeek(ItemHnd_Ptr^, Offset);
      end;
    TSeekOrigin.soCurrent:
      begin
        if Offset <> 0 then
            DB_Engine.ItemSeek(ItemHnd_Ptr^, DB_Engine.ItemGetPos(ItemHnd_Ptr^) + Offset);
      end;
    TSeekOrigin.soEnd:
      begin
        DB_Engine.ItemSeek(ItemHnd_Ptr^, DB_Engine.ItemGetSize(ItemHnd_Ptr^) + Offset);
      end;
  end;
  Result := DB_Engine.ItemGetPos(ItemHnd_Ptr^);
end;

function TItemStream.CopyFrom64(const Source: TCoreClassStream; Count: Int64): Int64;
const
  MaxBufSize = $F000;
var
  BufSize, N: Int64;
  buffer: PByte;
begin
  if Count <= 0 then
    begin
      Source.Position := 0;
      Count := Source.Size;
    end;
  Result := Count;
  if Count > MaxBufSize then
      BufSize := MaxBufSize
  else
      BufSize := Count;
  buffer := System.GetMemory(BufSize);
  try
    while Count <> 0 do
      begin
        if Count > BufSize then
            N := BufSize
        else
            N := Count;
        Source.ReadBuffer(buffer^, N);
        WriteBuffer(buffer^, N);
        Dec(Count, N);
      end;
  finally
      System.FreeMem(buffer);
  end;
end;

procedure TItemStream.SeekStart;
begin
  DB_Engine.ItemSeekStart(ItemHnd_Ptr^);
end;

procedure TItemStream.SeekLast;
begin
  DB_Engine.ItemSeekLast(ItemHnd_Ptr^);
end;

function TItemStream.UpdateHandle: Boolean;
begin
  if DB_Engine.IsOnlyRead then
      Result := False
  else
      Result := DB_Engine.ItemUpdate(ItemHnd_Ptr^);
end;

function TItemStream.CloseHandle: Boolean;
begin
  Result := DB_Engine.ItemClose(ItemHnd_Ptr^);
end;

end.
