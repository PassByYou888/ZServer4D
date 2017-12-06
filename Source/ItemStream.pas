{******************************************************************************}
{* ObjectDB Stream support,                                                   *}
{* https://github.com/PassByYou888/CoreCipher                                 *}
(* https://github.com/PassByYou888/ZServer4D                                  *)
{******************************************************************************}
(*
  update history
*)

unit ItemStream;

{$I zDefine.inc}

interface

uses SysUtils, CoreClasses, Classes, UnicodeMixedLib, ObjectData, ObjectDataManager;

type
  TItemStream = class(TCoreClassStream)
  private
    FItemHnd : TItemHandle;
    FDBEngine: TObjectDataManager;
  protected
    function GetSize: Int64; override;
  public
    constructor Create(DBEngine: TObjectDataManager; DBPath, DBItem: string); overload;
    constructor Create(DBEngine: TObjectDataManager; var ItemHnd: TItemHandle); overload;
    constructor Create; overload;
    destructor Destroy; override;
    procedure ChangeHandle(DBEngine: TObjectDataManager; var ItemHnd: TItemHandle);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; overload; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; overload; override;
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
  InitTTMDBItemHandle(FItemHnd);
end;

constructor TItemStream.Create(DBEngine: TObjectDataManager; DBPath, DBItem: string);
var
  ihnd: TItemHandle;
begin
  inherited Create;
  DBEngine.ItemAutoConnect(DBPath, DBItem, DBItem, ihnd);
  ChangeHandle(DBEngine, ihnd);
end;

constructor TItemStream.Create(DBEngine: TObjectDataManager; var ItemHnd: TItemHandle);
begin
  inherited Create;
  ChangeHandle(DBEngine, ItemHnd);
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

function TItemStream.Read(var Buffer; Count: Longint): Longint;
var
  _P   : Int64;
  _Size: Int64;
begin
  Result := 0;
  if (Count > 0) then
    begin
      _P := FDBEngine.ItemGetPos(FItemHnd);
      _Size := FDBEngine.ItemGetSize(FItemHnd);
      if _P + Count <= _Size then
        begin
          if FDBEngine.ItemRead(FItemHnd, Count, PByte(@Buffer)^) then
              Result := Count;
        end
      else if FDBEngine.ItemRead(FItemHnd, _Size - _P, PByte(@Buffer)^) then
          Result := _Size - _P;
    end;
end;

function TItemStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := Count;
  if (Count > 0) then
    if not FDBEngine.ItemWrite(FItemHnd, Count, PByte(@Buffer)^) then
      begin
        Result := 0;
      end;
end;

function TItemStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  case Origin of
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

function TItemStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  case Origin of
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
