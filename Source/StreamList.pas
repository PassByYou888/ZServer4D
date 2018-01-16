{*****************************************************************************}
{* fast StreamQuery,writen by QQ 600585@qq.com                               *}
{* https://github.com/PassByYou888/CoreCipher                                *}
(* https://github.com/PassByYou888/ZServer4D                                 *)
{*****************************************************************************}
(*
  update history
*)

unit StreamList;

{$I zDefine.inc}

interface

uses SysUtils, ObjectDataManager, ItemStream, CoreClasses, PascalStrings,
  ListEngine;

type
  THashStreamList = class;

  THashStreamListData = record
    qHash: THash;
    LowerCaseName, OriginName: SystemString;
    Stream: TItemStream;
    ItemHnd: TItemHandle;
    CallCount: Integer;
    ForceFreeCustomObject: Boolean;
    CustomObject: TCoreClassObject;
    id: Integer;
    Owner: THashStreamList;
  end;

  PHashStreamListData = ^THashStreamListData;

  THashStreamList = class(TCoreClassObject)
  protected
    FCounter    : Boolean;
    FCount      : Integer;
    FName       : SystemString;
    FDescription: SystemString;
    FDBEngine   : TObjectDataManager;
    FFieldPos   : Int64;
    FAryList    : array of TCoreClassList;
    FData       : Pointer;

    function GetListTable(Hash: THash; AutoCreate: Boolean): TCoreClassList;
    procedure RefreshDBLst(aDBEngine: TObjectDataManager; var aFieldPos: Int64);
  public
    constructor Create(aDBEngine: TObjectDataManager; aFieldPos: Int64);
    destructor Destroy; override;
    procedure Clear;
    procedure Refresh;
    procedure GetOriginNameListFromFilter(aFilter: SystemString; OutputList: TCoreClassStrings);
    procedure GetListFromFilter(aFilter: SystemString; OutputList: TCoreClassList);
    procedure GetOriginNameList(OutputList: TCoreClassStrings); overload;
    procedure GetOriginNameList(OutputList: TListString); overload;
    procedure GetList(OutputList: TCoreClassList);
    function Find(aName: SystemString): PHashStreamListData;
    function Exists(aName: SystemString): Boolean;

    procedure SetHashBlockCount(cnt: Integer);

    function GetItem(aName: SystemString): PHashStreamListData;
    property Names[aName: SystemString]: PHashStreamListData read GetItem; default;

    property DBEngine: TObjectDataManager read FDBEngine;
    property FieldPos: Int64 read FFieldPos;
    property name: SystemString read FName write FName;
    property Description: SystemString read FDescription write FDescription;
    property Counter: Boolean read FCounter write FCounter;
    property Count: Integer read FCount;
    property Data: Pointer read FData write FData;
  end;

implementation

uses UnicodeMixedLib;

constructor THashStreamList.Create(aDBEngine: TObjectDataManager; aFieldPos: Int64);
begin
  inherited Create;
  FCounter := True;
  FCount := 0;
  FData := nil;
  SetLength(FAryList, 0);
  SetHashBlockCount(100);
  RefreshDBLst(aDBEngine, aFieldPos);
end;

destructor THashStreamList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure THashStreamList.Clear;
var
  i            : Integer;
  Rep_Int_Index: Integer;
begin
  FCount := 0;
  if Length(FAryList) = 0 then
      Exit;

  for i := low(FAryList) to high(FAryList) do
    begin
      if FAryList[i] <> nil then
        begin
          with FAryList[i] do
            begin
              if Count > 0 then
                begin
                  for Rep_Int_Index := 0 to Count - 1 do
                    begin
                      with PHashStreamListData(Items[Rep_Int_Index])^ do
                        begin
                          if Stream <> nil then
                            begin
                              DisposeObject(Stream);
                              if (ForceFreeCustomObject) and (CustomObject <> nil) then
                                begin
                                  try
                                    DisposeObject(CustomObject);
                                    CustomObject := nil;
                                  except
                                  end;
                                end;
                            end;
                        end;
                      try
                          Dispose(PHashStreamListData(Items[Rep_Int_Index]));
                      except
                      end;
                    end;
                end;
            end;

          DisposeObject(FAryList[i]);
          FAryList[i] := nil;
        end;
    end;
end;

function THashStreamList.GetListTable(Hash: THash; AutoCreate: Boolean): TCoreClassList;
var
  Idx: Integer;
begin
  Idx := Hash mod Length(FAryList);

  if (AutoCreate) and (FAryList[Idx] = nil) then
      FAryList[Idx] := TCoreClassList.Create;
  Result := FAryList[Idx];
end;

procedure THashStreamList.RefreshDBLst(aDBEngine: TObjectDataManager; var aFieldPos: Int64);
var
  ItemSearchHnd: TItemSearch;
  iCnt         : Integer;

  procedure AddLstItem(_ItemPos: Int64);
  var
    NewHash: THash;
    p      : PHashStreamListData;
    idxLst : TCoreClassList;
    lName  : SystemString;
    ItemHnd: TItemHandle;
  begin
    if FDBEngine.ItemFastOpen(_ItemPos, ItemHnd) then
      if umlGetLength(ItemHnd.Name) > 0 then
        begin
          lName := ItemHnd.Name.LowerText;
          NewHash := MakeHash(lName);
          idxLst := GetListTable(NewHash, True);
          New(p);
          p^.qHash := NewHash;
          p^.LowerCaseName := lName;
          p^.OriginName := ItemHnd.Name;
          p^.Stream := nil;
          p^.ItemHnd := ItemHnd;
          p^.CallCount := 0;
          p^.ForceFreeCustomObject := False;
          p^.CustomObject := nil;
          p^.id := iCnt;
          p^.Owner := Self;
          idxLst.Add(p);
          Inc(iCnt);
        end;
  end;

begin
  FDBEngine := aDBEngine;
  FFieldPos := aFieldPos;
  FCount := 0;
  iCnt := 0;
  if FDBEngine.ItemFastFindFirst(FFieldPos, '*', ItemSearchHnd) then
    begin
      repeat
        Inc(FCount);
        AddLstItem(ItemSearchHnd.HeaderPOS);
      until not FDBEngine.ItemFastFindNext(ItemSearchHnd);
    end;
end;

procedure THashStreamList.Refresh;
begin
  Clear;
  RefreshDBLst(FDBEngine, FFieldPos);
end;

procedure THashStreamList.GetOriginNameListFromFilter(aFilter: SystemString; OutputList: TCoreClassStrings);
var
  i: Integer;
  L: TCoreClassList;
  p: PHashStreamListData;
begin
  L := TCoreClassList.Create;
  GetList(L);

  OutputList.Clear;
  if L.Count > 0 then
    for i := 0 to L.Count - 1 do
      begin
        p := PHashStreamListData(L[i]);
        if umlMultipleMatch(aFilter, p^.OriginName) then
            OutputList.Add(p^.OriginName);
      end;

  DisposeObject(L);
end;

procedure THashStreamList.GetListFromFilter(aFilter: SystemString; OutputList: TCoreClassList);
var
  i: Integer;
  L: TCoreClassList;
  p: PHashStreamListData;
begin
  L := TCoreClassList.Create;
  GetList(L);

  OutputList.Clear;
  if L.Count > 0 then
    for i := 0 to L.Count - 1 do
      begin
        p := PHashStreamListData(L[i]);
        if umlMultipleMatch(aFilter, p^.OriginName) then
            OutputList.Add(p);
      end;

  DisposeObject(L);
end;

procedure THashStreamList.GetOriginNameList(OutputList: TCoreClassStrings);
var
  i: Integer;
  L: TCoreClassList;
begin
  L := TCoreClassList.Create;
  GetList(L);

  OutputList.Clear;
  if L.Count > 0 then
    for i := 0 to L.Count - 1 do
        OutputList.Add(PHashStreamListData(L[i])^.OriginName);

  DisposeObject(L);
end;

procedure THashStreamList.GetOriginNameList(OutputList: TListString);
var
  i: Integer;
  L: TCoreClassList;
begin
  L := TCoreClassList.Create;
  GetList(L);

  OutputList.Clear;
  if L.Count > 0 then
    for i := 0 to L.Count - 1 do
        OutputList.Add(PHashStreamListData(L[i])^.OriginName);

  DisposeObject(L);
end;

procedure THashStreamList.GetList(OutputList: TCoreClassList);
  function ListSortCompare(Item1, Item2: Pointer): Integer;
    function aCompareValue(const A, B: Int64): Integer; inline;
    begin
      if A = B then
          Result := 0
      else if A < B then
          Result := -1
      else
          Result := 1;
    end;

  begin
    Result := aCompareValue(PHashStreamListData(Item1)^.id, PHashStreamListData(Item2)^.id);
  end;

  procedure QuickSortList(var SortList: TCoreClassPointerList; L, r: Integer);
  var
    i, j: Integer;
    p, t: Pointer;
  begin
    repeat
      i := L;
      j := r;
      p := SortList[(L + r) shr 1];
      repeat
        while ListSortCompare(SortList[i], p) < 0 do
            Inc(i);
        while ListSortCompare(SortList[j], p) > 0 do
            Dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortList[i];
                SortList[i] := SortList[j];
                SortList[j] := t;
              end;
            Inc(i);
            Dec(j);
          end;
      until i > j;
      if L < j then
          QuickSortList(SortList, L, j);
      L := i;
    until i >= r;
  end;

var
  i            : Integer;
  Rep_Int_Index: Integer;
begin
  OutputList.Clear;

  if Count > 0 then
    begin
      OutputList.Capacity := Count;

      for i := low(FAryList) to high(FAryList) do
        begin
          if FAryList[i] <> nil then
            begin
              with FAryList[i] do
                if Count > 0 then
                  begin
                    for Rep_Int_Index := 0 to Count - 1 do
                      with PHashStreamListData(Items[Rep_Int_Index])^ do
                        begin
                          if Stream = nil then
                              Stream := TItemStream.Create(FDBEngine, ItemHnd)
                          else
                              Stream.SeekStart;
                          if FCounter then
                              Inc(CallCount);
                          OutputList.Add(Items[Rep_Int_Index]);
                        end;
                  end;
            end;
        end;

      if OutputList.Count > 1 then
          QuickSortList(OutputList.ListData^, 0, OutputList.Count - 1);
    end;
end;

function THashStreamList.Find(aName: SystemString): PHashStreamListData;
var
  i            : Integer;
  Rep_Int_Index: Integer;
begin
  Result := nil;
  for i := low(FAryList) to high(FAryList) do
    begin
      if FAryList[i] <> nil then
        begin
          with FAryList[i] do
            if Count > 0 then
              begin
                for Rep_Int_Index := 0 to Count - 1 do
                  begin
                    if umlMultipleMatch(True, aName, PHashStreamListData(Items[Rep_Int_Index])^.OriginName) then
                      begin
                        Result := Items[Rep_Int_Index];
                        if Result^.Stream = nil then
                            Result^.Stream := TItemStream.Create(FDBEngine, Result^.ItemHnd)
                        else
                            Result^.Stream.SeekStart;
                        if FCounter then
                            Inc(Result^.CallCount);
                        Exit;
                      end;
                  end;
              end;
        end;
    end;
end;

function THashStreamList.Exists(aName: SystemString): Boolean;
var
  NewHash: THash;
  i      : Integer;
  idxLst : TCoreClassList;
  lName  : SystemString;
begin
  Result := False;
  if umlGetLength(aName) > 0 then
    begin
      lName := LowerCase(aName);
      NewHash := MakeHash(lName);
      idxLst := GetListTable(NewHash, False);
      if idxLst <> nil then
        if idxLst.Count > 0 then
          for i := 0 to idxLst.Count - 1 do
            if (NewHash = PHashStreamListData(idxLst[i])^.qHash) and (PHashStreamListData(idxLst[i])^.LowerCaseName = lName) then
                Exit(True);
    end;
end;

procedure THashStreamList.SetHashBlockCount(cnt: Integer);
var
  i: Integer;
begin
  Clear;
  SetLength(FAryList, cnt);
  for i := low(FAryList) to high(FAryList) do
      FAryList[i] := nil;
end;

function THashStreamList.GetItem(aName: SystemString): PHashStreamListData;
var
  NewHash: THash;
  i      : Integer;
  idxLst : TCoreClassList;
  lName  : SystemString;
begin
  Result := nil;
  if umlGetLength(aName) > 0 then
    begin
      lName := LowerCase(aName);
      NewHash := MakeHash(lName);
      idxLst := GetListTable(NewHash, False);
      if idxLst <> nil then
        if idxLst.Count > 0 then
          for i := 0 to idxLst.Count - 1 do
            begin
              if (NewHash = PHashStreamListData(idxLst[i])^.qHash) and (PHashStreamListData(idxLst[i])^.LowerCaseName = lName) then
                begin
                  Result := idxLst[i];
                  if Result^.Stream = nil then
                      Result^.Stream := TItemStream.Create(FDBEngine, Result^.ItemHnd)
                  else
                      Result^.Stream.SeekStart;
                  if FCounter then
                      Inc(Result^.CallCount);
                  Exit;
                end;
            end;
    end;
end;

end.
