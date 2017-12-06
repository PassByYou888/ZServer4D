{******************************************************************************}
{* fast File query in Package                                                 *}
{* https://github.com/PassByYou888/CoreCipher                                 *}
(* https://github.com/PassByYou888/ZServer4D                                  *)
{******************************************************************************}
(*
  update history
*)


unit LibraryManager;

{$I zDefine.inc}

interface

uses ObjectDataManager, StreamList, CoreClasses;

type
  TLibraryManager = class(TCoreClassObject)
  private
    FList: TCoreClassListForObj;
    FDBEngine: TObjectDataManager;
    FRoot: THashStreamList;
    FRootDir: string;
  private
  protected
    function GetItems(Index: Integer): THashStreamList;
    function GetNameItems(aName: string): THashStreamList;
    function GetPathItems(aPath: string): PHashStreamListData;
  public
    constructor Create(aDataEngine: TObjectDataManager; aRootDir: string);
    destructor Destroy; override;
    function Clone: TLibraryManager;
    function Count: Integer;
    procedure Clear;
    procedure Refresh;
    procedure ChangeRoot(NewRoot: string);

    function TotalCount: Integer;
    function New(aName, aDescription: string): THashStreamList;
    function Delete(aName: string; ForceRefresh: Boolean): Boolean;
    function ReName(aOLDName, NewName, aDescription: string; ForceRefresh: Boolean): Boolean;
    function Exists(aName: string): Boolean;

    property Items[index: Integer]: THashStreamList read GetItems;
    property NameItems[aName: string]: THashStreamList read GetNameItems; default;
    property PathItems[aPath: string]: PHashStreamListData read GetPathItems;
    property DBEngine: TObjectDataManager read FDBEngine;
    property Root: THashStreamList read FRoot;
  end;

implementation

uses PascalStrings, UnicodeMixedLib;

const
  PathDelim = ':\/';

var
  _LibManCloneAutoFreeList: TCoreClassListForObj = nil;

function LibManCloneAutoFreeList: TCoreClassListForObj;
begin
  if _LibManCloneAutoFreeList = nil then
      _LibManCloneAutoFreeList := TCoreClassListForObj.Create;
  Result := _LibManCloneAutoFreeList;
end;

procedure FreeLibManCloneAutoFreeList;
var
  i: Integer;
begin
  if _LibManCloneAutoFreeList = nil then
      Exit;
  i := 0;
  while i < _LibManCloneAutoFreeList.Count do
      DisposeObject(TLibraryManager(_LibManCloneAutoFreeList[i]));
  DisposeObject(_LibManCloneAutoFreeList);
  _LibManCloneAutoFreeList := nil;
end;

procedure DeleteLibManCloneFromAutoFreeList(p: TLibraryManager);
var
  i: Integer;
begin
  if _LibManCloneAutoFreeList = nil then
      Exit;
  i := 0;
  while i < _LibManCloneAutoFreeList.Count do
    begin
      if _LibManCloneAutoFreeList[i] = p then
          _LibManCloneAutoFreeList.Delete(i)
      else
          Inc(i);
    end;
end;

function TLibraryManager.GetItems(Index: Integer): THashStreamList;
begin
  Result := THashStreamList(FList[index]);
end;

function TLibraryManager.GetNameItems(aName: string): THashStreamList;
var
  i: Integer;
begin
  Result := Root;
  if Count > 0 then
    for i := 0 to Count - 1 do
      if umlMultipleMatch(True, aName, Items[i].Name) then
        begin
          Result := Items[i];
          Break;
        end;
end;

function TLibraryManager.GetPathItems(aPath: string): PHashStreamListData;
var
  i: Integer;
  slst: THashStreamList;
  PhPrefix, phPostfix: string;
begin
  Result := nil;
  if Count > 0 then
    begin
      if umlGetIndexStrCount(aPath, PathDelim) > 1 then
          PhPrefix := umlGetFirstStr(aPath, PathDelim).Text
      else
          PhPrefix := '';
      phPostfix := umlGetLastStr(aPath, PathDelim).Text;
      for i := 0 to Count - 1 do
        begin
          if umlMultipleMatch(True, PhPrefix, Items[i].Name) then
            begin
              slst := Items[i];
              if slst <> nil then
                begin
                  Result := slst.Names[phPostfix];
                  if Result <> nil then
                      Break;
                end;
            end;
        end;
    end;
end;

constructor TLibraryManager.Create(aDataEngine: TObjectDataManager; aRootDir: string);
begin
  inherited Create;
  FList := TCoreClassListForObj.Create;
  FDBEngine := aDataEngine;
  FRoot := nil;
  FRootDir := aRootDir;
  Refresh;
  LibManCloneAutoFreeList.Add(Self);
end;

destructor TLibraryManager.Destroy;
begin
  DeleteLibManCloneFromAutoFreeList(Self);
  while FList.Count > 0 do
    begin
      DisposeObject(THashStreamList(FList[0]));
      FList.Delete(0);
    end;
  DisposeObject(FList);
  inherited Destroy;
end;

function TLibraryManager.Clone: TLibraryManager;
begin
  Result := TLibraryManager.Create(DBEngine, FRootDir);
end;

function TLibraryManager.Count: Integer;
begin
  Result := FList.Count;
end;

procedure TLibraryManager.Clear;
begin
  while Count > 0 do
    begin
      DisposeObject(THashStreamList(FList[0]));
      FList.Delete(0);
    end;
end;

procedure TLibraryManager.Refresh;
var
  fPos: Int64;
  hsList: THashStreamList;
  n, d: string;
  fSearchHnd: TFieldSearch;
begin
  if FDBEngine.isAbort then
      Exit;
  while FList.Count > 0 do
    begin
      DisposeObject(THashStreamList(FList[0]));
      FList.Delete(0);
    end;

  if FDBEngine.FieldFindFirst(FRootDir, '*', fSearchHnd) then
    begin
      repeat
        n := fSearchHnd.Name;
        d := fSearchHnd.Description;
        fPos := fSearchHnd.HeaderPOS;
        hsList := THashStreamList.Create(FDBEngine, fPos);
        hsList.Name := n;
        hsList.Description := d;
        FList.Add(hsList);
      until not FDBEngine.FieldFindNext(fSearchHnd);
    end;

  if FDBEngine.GetPathField(FRootDir, fPos) then
    begin
      n := 'Root';
      d := '....';
      hsList := THashStreamList.Create(FDBEngine, fPos);
      hsList.Name := n;
      hsList.Description := d;
      if FList.Count > 0 then
          FList.Insert(0, hsList)
      else
          FList.Add(hsList);
      FRoot := hsList;
    end;
end;

procedure TLibraryManager.ChangeRoot(NewRoot: string);
begin
  FRootDir := NewRoot;
  Refresh;
end;

function TLibraryManager.TotalCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  if Count > 0 then
    for i := 0 to Count - 1 do
        Result := Result + Items[i].Count;
end;

function TLibraryManager.New(aName, aDescription: string): THashStreamList;
var
  fPos: Int64;
  n, d: string;
  fSearchHnd: TFieldSearch;
begin
  Result := nil;
  if not umlMultipleMatch(True, aName, Root.Name) then
    begin
      if FDBEngine.CreateField((FRootDir + '/' + aName), aDescription) then
        begin
          if FDBEngine.FieldFindFirst(FRootDir, aName, fSearchHnd) then
            begin
              n := fSearchHnd.Name;
              d := fSearchHnd.Description;
              fPos := fSearchHnd.HeaderPOS;
              Result := THashStreamList.Create(FDBEngine, fPos);
              Result.Name := n;
              Result.Description := d;
              FList.Add(Result);
            end;
        end;
    end
  else
    begin
      Result := Root;
    end;
end;

function TLibraryManager.Delete(aName: string; ForceRefresh: Boolean): Boolean;
begin
  Result := FDBEngine.FieldDelete(FRootDir, aName);
  if (ForceRefresh) and (Result) then
      Refresh;
end;

function TLibraryManager.ReName(aOLDName, NewName, aDescription: string; ForceRefresh: Boolean): Boolean;
var
  fPos: Int64;
begin
  Result := False;
  if FDBEngine.FieldExists(FRootDir, NewName) then
      Exit;
  if FDBEngine.GetPathField(FRootDir + '/' + aOLDName, fPos) then
    begin
      Result := FDBEngine.FieldReName(fPos, NewName, aDescription);
      if (Result) and (ForceRefresh) then
          Refresh;
    end;
end;

function TLibraryManager.Exists(aName: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  if Count > 0 then
    for i := 0 to Count - 1 do
      begin
        if umlMultipleMatch(True, aName, Items[i].Name) then
          begin
            Result := True;
            Exit;
          end;
      end;
end;

initialization

finalization

FreeLibManCloneAutoFreeList;

end.
