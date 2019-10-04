{ ****************************************************************************** }
{ * fast File query in Package                                                 * }
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

unit ObjectDataHashField;

{$INCLUDE zDefine.inc}

interface

uses ObjectDataManager, ObjectDataHashItem, CoreClasses, PascalStrings, UnicodeMixedLib;

type
  TObjectDataHashField = class(TCoreClassObject)
  private
    FList: TCoreClassListForObj;
    FDBEngine: TObjectDataManager;
    FRoot: TObjectDataHashItem;
    FRootDir: SystemString;
    FAutoFreeDataEngine: Boolean;
  protected
    function GetItems(index: Integer): TObjectDataHashItem;
    function GetNameItems(Name_: SystemString): TObjectDataHashItem;
    function GetPathItems(Path_: SystemString): PHashItemData;
  public
    constructor Create(DataEngine_: TObjectDataManager; RootDir_: SystemString);
    destructor Destroy; override;
    function Clone: TObjectDataHashField;
    function Count: Integer;
    procedure Clear;
    procedure Refresh;
    procedure ChangeRoot(NewRoot_: SystemString);

    function TotalCount: Integer;
    function New(Name_, Description_: SystemString): TObjectDataHashItem;
    function Delete(Name_: SystemString; ForceRefresh: Boolean): Boolean;
    function ReName(OLDName_, NewName_, Description_: SystemString; ForceRefresh: Boolean): Boolean;
    function Exists(Name_: SystemString): Boolean;

    property Items[index: Integer]: TObjectDataHashItem read GetItems;
    property NameItems[Name_: SystemString]: TObjectDataHashItem read GetNameItems; default;
    property PathItems[Path_: SystemString]: PHashItemData read GetPathItems;
    property DBEngine: TObjectDataManager read FDBEngine;
    property ROOT: TObjectDataHashItem read FRoot;
    property AutoFreeDataEngine: Boolean read FAutoFreeDataEngine write FAutoFreeDataEngine;
  end;

implementation

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
      DisposeObject(TObjectDataHashField(_LibManCloneAutoFreeList[i]));
  DisposeObject(_LibManCloneAutoFreeList);
  _LibManCloneAutoFreeList := nil;
end;

procedure DeleteLibManCloneFromAutoFreeList(p: TObjectDataHashField);
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
          inc(i);
    end;
end;

function TObjectDataHashField.GetItems(index: Integer): TObjectDataHashItem;
begin
  Result := TObjectDataHashItem(FList[index]);
end;

function TObjectDataHashField.GetNameItems(Name_: SystemString): TObjectDataHashItem;
var
  i: Integer;
begin
  Result := ROOT;
  if Count > 0 then
    for i := 0 to Count - 1 do
      if umlMultipleMatch(True, Name_, Items[i].Name) then
        begin
          Result := Items[i];
          Break;
        end;
end;

function TObjectDataHashField.GetPathItems(Path_: SystemString): PHashItemData;
var
  i: Integer;
  slst: TObjectDataHashItem;
  PhPrefix, phPostfix: SystemString;
begin
  Result := nil;
  if Count > 0 then
    begin
      if umlGetIndexStrCount(Path_, PathDelim) > 1 then
          PhPrefix := umlGetFirstStr(Path_, PathDelim).Text
      else
          PhPrefix := '';
      phPostfix := umlGetLastStr(Path_, PathDelim).Text;
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

constructor TObjectDataHashField.Create(DataEngine_: TObjectDataManager; RootDir_: SystemString);
begin
  inherited Create;
  FList := TCoreClassListForObj.Create;
  FDBEngine := DataEngine_;
  FRoot := nil;
  FRootDir := RootDir_;
  Refresh;
  LibManCloneAutoFreeList.Add(Self);
  FAutoFreeDataEngine := False;
end;

destructor TObjectDataHashField.Destroy;
begin
  DeleteLibManCloneFromAutoFreeList(Self);
  while FList.Count > 0 do
    begin
      DisposeObject(TObjectDataHashItem(FList[0]));
      FList.Delete(0);
    end;
  DisposeObject(FList);
  if FAutoFreeDataEngine then
      DisposeObject(FDBEngine);
  inherited Destroy;
end;

function TObjectDataHashField.Clone: TObjectDataHashField;
begin
  Result := TObjectDataHashField.Create(DBEngine, FRootDir);
end;

function TObjectDataHashField.Count: Integer;
begin
  Result := FList.Count;
end;

procedure TObjectDataHashField.Clear;
begin
  while Count > 0 do
    begin
      DisposeObject(TObjectDataHashItem(FList[0]));
      FList.Delete(0);
    end;
end;

procedure TObjectDataHashField.Refresh;
var
  fPos: Int64;
  hsList: TObjectDataHashItem;
  n, d: SystemString;
  fSearchHnd: TFieldSearch;
begin
  if FDBEngine.isAbort then
      Exit;
  while FList.Count > 0 do
    begin
      DisposeObject(TObjectDataHashItem(FList[0]));
      FList.Delete(0);
    end;

  if FDBEngine.FieldFindFirst(FRootDir, '*', fSearchHnd) then
    begin
      repeat
        n := fSearchHnd.Name;
        d := fSearchHnd.Description;
        fPos := fSearchHnd.HeaderPOS;
        hsList := TObjectDataHashItem.Create(FDBEngine, fPos);
        hsList.Name := n;
        hsList.Description := d;
        FList.Add(hsList);
      until not FDBEngine.FieldFindNext(fSearchHnd);
    end;

  if FDBEngine.GetPathField(FRootDir, fPos) then
    begin
      n := 'Root';
      d := '....';
      hsList := TObjectDataHashItem.Create(FDBEngine, fPos);
      hsList.Name := n;
      hsList.Description := d;
      if FList.Count > 0 then
          FList.Insert(0, hsList)
      else
          FList.Add(hsList);
      FRoot := hsList;
    end;
end;

procedure TObjectDataHashField.ChangeRoot(NewRoot_: SystemString);
begin
  FRootDir := NewRoot_;
  Refresh;
end;

function TObjectDataHashField.TotalCount: Integer;
var
  i: Integer;
begin
  Result := 0;
  if Count > 0 then
    for i := 0 to Count - 1 do
        Result := Result + Items[i].Count;
end;

function TObjectDataHashField.New(Name_, Description_: SystemString): TObjectDataHashItem;
var
  fPos: Int64;
  n, d: SystemString;
  fSearchHnd: TFieldSearch;
begin
  Result := nil;
  if not umlMultipleMatch(True, Name_, ROOT.Name) then
    begin
      if FDBEngine.CreateField((FRootDir + '/' + Name_), Description_) then
        begin
          if FDBEngine.FieldFindFirst(FRootDir, Name_, fSearchHnd) then
            begin
              n := fSearchHnd.Name;
              d := fSearchHnd.Description;
              fPos := fSearchHnd.HeaderPOS;
              Result := TObjectDataHashItem.Create(FDBEngine, fPos);
              Result.Name := n;
              Result.Description := d;
              FList.Add(Result);
            end;
        end;
    end
  else
    begin
      Result := ROOT;
    end;
end;

function TObjectDataHashField.Delete(Name_: SystemString; ForceRefresh: Boolean): Boolean;
begin
  Result := FDBEngine.FieldDelete(FRootDir, Name_);
  if (ForceRefresh) and (Result) then
      Refresh;
end;

function TObjectDataHashField.ReName(OLDName_, NewName_, Description_: SystemString; ForceRefresh: Boolean): Boolean;
var
  fPos: Int64;
begin
  Result := False;
  if FDBEngine.FieldExists(FRootDir, NewName_) then
      Exit;
  if FDBEngine.GetPathField(FRootDir + '/' + OLDName_, fPos) then
    begin
      Result := FDBEngine.FieldReName(fPos, NewName_, Description_);
      if (Result) and (ForceRefresh) then
          Refresh;
    end;
end;

function TObjectDataHashField.Exists(Name_: SystemString): Boolean;
var
  i: Integer;
begin
  Result := False;
  if Count > 0 then
    for i := 0 to Count - 1 do
      begin
        if umlMultipleMatch(True, Name_, Items[i].Name) then
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
