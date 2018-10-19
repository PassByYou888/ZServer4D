{ ****************************************************************************** }
{ * ObjectDB Stream support,                                                   * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }
(*
  update history
*)

unit ItemStream;

{$INCLUDE zDefine.inc}

interface

uses SysUtils, CoreClasses, Classes, UnicodeMixedLib, ObjectData, ObjectDataManager,
  PascalStrings;

type
  TItemStream = class(TCoreClassStream)
  private
    FItemHnd: TItemHandle;
    FDBEngine: TObjectDataManager;
  protected
    function GetSize: Int64; override;
  public
    constructor Create(DBEngine: TObjectDataManager; DBPath, DBItem: SystemString); overload;
    constructor Create(DBEngine: TObjectDataManager; var ItemHnd: TItemHandle); overload;
    constructor Create(DBEngine: TObjectDataManager; var ItemHeaderPos: Int64); overload;
    constructor Create; overload;
    destructor Destroy; override;
    procedure ChangeHandle(DBEngine: TObjectDataManager; var ItemHnd: TItemHandle);

    procedure SaveToFile(fn: SystemString);
    procedure LoadFromFile(fn: SystemString);

    function read(var buffer; Count: longint): longint; override;
    function write(const buffer; Count: longint): longint; override;

    function Seek(Offset: longint; origin: Word): longint; overload; override;
    function Seek(const Offset: Int64; origin: TSeekOrigin): Int64; overload; override;

    procedure SeekStart;
    procedure SeekLast;
    function UpdateHandle: Boolean;
    function CloseHandle: Boolean;
    function Hnd: PItemHandle;
  end;

  TItemStreamEngine = class(TItemStream)
  public
    destructor Destroy; override;
  end;

implementation

function TItemStream.GetSize: Int64;
begin
  Result := FDBEngine.ItemGetSize(FItemHnd);
end;

constructor TItemStream.Create;
begin
  inherited Create;
  FDBEngine := nil;
  Init_TTMDBItemHandle(FItemHnd);
end;

constructor TItemStream.Create(DBEngine: TObjectDataManager; DBPath, DBItem: SystemString);
var
  ihnd: TItemHandle;
begin
  inherited Create;
  DBEngine.ItemAutoOpenOrCreate(DBPath, DBItem, DBItem, ihnd);
  ChangeHandle(DBEngine, ihnd);
end;

constructor TItemStream.Create(DBEngine: TObjectDataManager; var ItemHnd: TItemHandle);
begin
  inherited Create;
  ChangeHandle(DBEngine, ItemHnd);
end;

constructor TItemStream.Create(DBEngine: TObjectDataManager; var ItemHeaderPos: Int64);
var
  ItemHnd: TItemHandle;
begin
  DBEngine.ItemFastOpen(ItemHeaderPos, ItemHnd);
  Create(DBEngine, ItemHnd);
end;

destructor TItemStream.Destroy;
begin
  inherited Destroy;
end;

procedure TItemStream.ChangeHandle(DBEngine: TObjectDataManager; var ItemHnd: TItemHandle);
begin
  FDBEngine := DBEngine;
  FItemHnd := ItemHnd;
  FDBEngine.ItemSeek(FItemHnd, 0);
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
  stream := TCoreClassFileStream.Create(fn, fmOpenRead or fmShareDenyWrite);
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
      _P := FDBEngine.ItemGetPos(FItemHnd);
      _Size := FDBEngine.ItemGetSize(FItemHnd);
      if _P + Count <= _Size then
        begin
          if FDBEngine.ItemRead(FItemHnd, Count, PByte(@buffer)^) then
              Result := Count;
        end
      else if FDBEngine.ItemRead(FItemHnd, _Size - _P, PByte(@buffer)^) then
          Result := _Size - _P;
    end;
end;

function TItemStream.write(const buffer; Count: longint): longint;
begin
  Result := Count;
  if (Count > 0) then
    if not FDBEngine.ItemWrite(FItemHnd, Count, PByte(@buffer)^) then
      begin
        Result := 0;
      end;
end;

function TItemStream.Seek(Offset: longint; origin: Word): longint;
begin
  case origin of
    soFromBeginning:
      begin
        FDBEngine.ItemSeek(FItemHnd, Offset);
      end;
    soFromCurrent:
      begin
        if Offset <> 0 then
            FDBEngine.ItemSeek(FItemHnd, FDBEngine.ItemGetPos(FItemHnd) + Offset);
      end;
    soFromEnd:
      begin
        FDBEngine.ItemSeek(FItemHnd, FDBEngine.ItemGetSize(FItemHnd) + Offset);
      end;
  end;
  Result := FDBEngine.ItemGetPos(FItemHnd);
end;

function TItemStream.Seek(const Offset: Int64; origin: TSeekOrigin): Int64;
begin
  case origin of
    TSeekOrigin.soBeginning:
      begin
        FDBEngine.ItemSeek(FItemHnd, Offset);
      end;
    TSeekOrigin.soCurrent:
      begin
        if Offset <> 0 then
            FDBEngine.ItemSeek(FItemHnd, FDBEngine.ItemGetPos(FItemHnd) + Offset);
      end;
    TSeekOrigin.soEnd:
      begin
        FDBEngine.ItemSeek(FItemHnd, FDBEngine.ItemGetSize(FItemHnd) + Offset);
      end;
  end;
  Result := FDBEngine.ItemGetPos(FItemHnd);
end;

procedure TItemStream.SeekStart;
begin
  FDBEngine.ItemSeekStart(FItemHnd);
end;

procedure TItemStream.SeekLast;
begin
  FDBEngine.ItemSeekLast(FItemHnd);
end;

function TItemStream.UpdateHandle: Boolean;
begin
  Result := FDBEngine.ItemUpdate(FItemHnd);
end;

function TItemStream.CloseHandle: Boolean;
begin
  Result := FDBEngine.ItemClose(FItemHnd);
end;

function TItemStream.Hnd: PItemHandle;
begin
  Result := @FItemHnd;
end;

destructor TItemStreamEngine.Destroy;
begin
  UpdateHandle;
  inherited Destroy;
end;

end.
