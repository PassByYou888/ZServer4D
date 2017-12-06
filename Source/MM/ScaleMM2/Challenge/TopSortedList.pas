{****************************************************************************************

  TOPMEMORY v1.42 - HIGH PERFORMANCE DELPHI MEMORY MANAGER  (C) 2004 Ivo Tops, Topsoftware

  TopSortedList is a regular list (both sorted and non-sorted) using local memory

****************************************************************************************}
unit TopSortedList;

interface

uses TopLocalObjects,
TopCopyMemory;

type
  TIndexedObject = packed record
    Index: Integer;
    Obj: Pointer;
  end;
  TIndexedObjectsArray = array[0..MaxInt div (SizeOf(TIndexedObject)) - 1] of TIndexedObject;
  PIndexedObjectsArray = ^TIndexedObjectsArray;

type
  TTopSortedList = class(TLocalObject)
  private
    FInitialCapacity: Integer;
    FListCount: Integer;
    FList: PIndexedObjectsArray;
    FListCapacity: Integer;
    FSorted: Boolean;
    FFlag: Boolean;
    function Get(Index: Integer): TIndexedObject;
    procedure Put(Index: Integer; const Value: TIndexedObject);
    procedure SetCapacity(const Value: Integer);
  protected
    procedure CheckCapacity;
  public
    constructor Create(const Sorted: Boolean = False; const InitialSize: Integer = 9);
    destructor Destroy; override;
    //
    procedure Clear;
    //
    function Add(const Value: Integer; const AssociatedObject: Pointer = nil): Integer; overload;
    function Add(const Value: Cardinal; const AssociatedObject: Pointer = nil): Integer; overload;
    function Add(const Value: Pointer; const AssociatedObject: Pointer = nil): Integer; overload;
    procedure Delete(const Value: Integer); overload;
    procedure Delete(const Value: Cardinal); overload;
    procedure Delete(const Value: Pointer); overload;
    procedure DeleteByIndex(const Index: Integer);
    function Exists(const Value: Integer): Boolean; overload;
    function Exists(const Value: Cardinal): Boolean; overload;
    function Exists(const Value: Pointer): Boolean; overload;

    function Find(const Value: Cardinal; out Index: Integer; out AssociatedObject: Pointer): Boolean; overload;
    function Find(const Value: Pointer; out Index: Integer; out AssociatedObject: Pointer): Boolean; overload;
    function Find(const Value: Integer; out Index: Integer; out AssociatedObject: Pointer): Boolean; overload;
    //
    function Find(const Value: Integer; out Index: Integer): Boolean; overload;
    function Find(const Value: Pointer; out Index: Integer): Boolean; overload;
    function Find(const Value: Cardinal; out Index: Integer): Boolean; overload;
    //
    function Find(const Value: Integer; out AssociatedObject: Pointer): Boolean; overload;
    function Find(const Value: Cardinal; out AssociatedObject: Pointer): Boolean; overload;
    function Find(const Value: Pointer; out AssociatedObject: Pointer): Boolean; overload;
    //
    procedure SetValue(const Index: Integer; const Value: Integer; const AssociatedObject: Pointer = nil); overload;
    procedure SetValue(const Index: Integer; const Value: Cardinal; const AssociatedObject: Pointer = nil); overload;
    procedure SetValue(const Index: Integer; const Value: Pointer; const AssociatedObject: Pointer = nil); overload;
    //
    procedure GetValue(const Index: Integer; out Value: Integer; out AssociatedObject: Pointer); overload;
    procedure GetValue(const Index: Integer; out Value: Cardinal; out AssociatedObject: Pointer); overload;
    procedure GetValue(const Index: Integer; out Value: Pointer; out AssociatedObject: Pointer); overload;
    //
    function GetCardinal(const Index: Integer): Cardinal;
    function GetInteger(const Index: Integer): Integer;
    function GetPointer(const Index: Integer): Pointer;
    //
    property Count: Integer read FListCount;
    property Capacity: Integer read FListCapacity write SetCapacity;
    //
    property Items[Index: Integer]: TIndexedObject read Get write Put; default;
    //
    // Boolean for external use
    property Flag: Boolean read FFLag write FFlag;
    //
  end;

implementation

{ TopSortedList }
{$IFDEF TOPDEBUG}
uses TopInstall;
{$ENDIF}

function TTopSortedList.Add(const Value: Cardinal; const AssociatedObject: Pointer): Integer;
begin
  Result := Add(Integer(Value), AssociatedObject);
end;

function TTopSortedList.Add(const Value: Integer; const AssociatedObject: Pointer): Integer;
var
  Index: Integer;
  Dummy: Pointer;
begin
  // Empty List?, Add and Exit
  if FListCount = 0 then
  begin
    Result := 0;
    FListCount := 1;
    CheckCapacity;
    FList[0].Index := Value;
    FList[0].Obj := AssociatedObject;
    Exit;
  end;
  // Not sorted list does not check for duplicates, simply adds new entry at end
  if not FSorted then
  begin
    Inc(FListCount);
    CheckCapacity;
    FList[FListCount - 1].Index := Value;
    FList[FListCount - 1].Obj := AssociatedObject;
    Result := FListCount - 1;
  end
  else
  begin
    // Sorted list does not allow duplicated and replaces object with newly passed object (modify command)
    if Find(Value, Index, Dummy) then
    begin
      FList[Index].Obj := AssociatedObject;
      Result := Index;
      Exit;
    end;
    // Add new value on correct position
    Inc(FListCount);
    CheckCapacity;
    //
    // Check upper add
    if FList[FListCount - 2].Index < Value then
    begin
      FList[FListCount - 1].Index := Value;
      FList[FListCount - 1].Obj := AssociatedObject;
      Result := FListCount - 1;
      Exit;
    end;
    // check bottom add
    if FList[0].Index > Value then
    begin
      WinMoveMemory(Pointer(Cardinal(FList) + SizeOf(TIndexedObject)), FList, SizeOf(TIndexedObject) * (FListCount));
      FList[0].Index := Value;
      FList[0].Obj := AssociatedObject;
      Result := 0;
      Exit;
    end;
    // Add in between
    WinMoveMemory(Pointer(Cardinal(FList) + Cardinal(SizeOf(TIndexedObject) * (Index + 1))), Pointer(Cardinal(FList) + Cardinal(SizeOf(TIndexedObject) * (Index))), SizeOf(TIndexedObject) * (FListCount - Index));
    // Set new value
    FList[Index].Index := Value;
    FList[Index].Obj := AssociatedObject;
    Result := Index;
  end;
end;

function TTopSortedList.Add(const Value: Pointer; const AssociatedObject: Pointer): Integer;
begin
  Result := Add(Integer(Value), AssociatedObject);
end;

procedure TTopSortedList.CheckCapacity;
begin
  // Increase array for new Values
  if FListCount >= FListCapacity then
    FList := FixCapacity(FList, FListCapacity, SizeOf(TIndexedObject), True);
end;

procedure TTopSortedList.Clear;
begin
  FListCount := 0;
  FListCapacity := FInitialCapacity;
  TopLocalMemFree(FList);
  FList := TopLocalMemZeroAlloc(FListCapacity * SizeOf(TIndexedObject));
  FFlag := False;
end;

constructor TTopSortedList.Create(const Sorted: Boolean; const InitialSize: Integer);
begin
  inherited Create;
  FFlag := False;
  FListCount := 0;
  FInitialCapacity := InitialSize;
  FListCapacity := InitialSize;
  FList := TopLocalMemZeroAlloc(FListCapacity * SizeOf(TIndexedObject));
  FSorted := Sorted;
end;

procedure TTopSortedList.Delete(const Value: Integer);
var
  Index: Integer;
  Dummy: Pointer;
begin
  if Find(Value, Index, Dummy) then
    DeleteByIndex(Index);
end;

procedure TTopSortedList.Delete(const Value: Cardinal);
begin
  Delete(Integer(Value));
end;

procedure TTopSortedList.Delete(const Value: Pointer);
begin
  Delete(Integer(Value));
end;

procedure TTopSortedList.DeleteByIndex(const Index: Integer);
var
  I: Integer;
begin
  for I := Index to FListCount - 2 do
    FList[I] := FList[I + 1];
  Dec(FListCount);
end;

destructor TTopSortedList.Destroy;
begin
  TopLocalMemFree(FList);
  inherited Destroy;
end;

function TTopSortedList.Exists(const Value: Integer): Boolean;
var
  X: Integer;
  Y: Pointer;
begin
  Result := Find(Value, X, Y);
end;

function TTopSortedList.Exists(const Value: Pointer): Boolean;
begin
  Result := Exists(Integer(Value));
end;

function TTopSortedList.Exists(const Value: Cardinal): Boolean;
begin
  Result := Exists(Integer(Value));
end;

function TTopSortedList.Find(const Value: Cardinal; out Index: Integer; out AssociatedObject: Pointer): Boolean;
begin
  Result := Find(Integer(Value), Index, AssociatedObject);
end;

function TTopSortedList.Find(const Value: Pointer; out Index: Integer; out AssociatedObject: Pointer): Boolean;
begin
  Result := Find(Integer(Value), Index, AssociatedObject);
end;

function TTopSortedList.Find(const Value: Integer; out Index: Integer; out AssociatedObject: Pointer): Boolean;
var
  Low, High, I: Integer;
begin
  Index := 0;
  AssociatedObject := nil;
  Result := False;
  //
  if FListCount = 0 then Exit;
  //
  if (FListCount = 1) then
  begin
    if FList[Index].Index = Value then
    begin
      AssociatedObject := FList[Index].Obj;
      Result := True;
    end;
    Exit;
  end;
  //
  if FSorted then
  begin
    Low := 0;
    High := FListCount - 1;
    while Low <= High do
    begin
      I := (Low + High) shr 1;
      if FList[I].Index < Value then
        Low := I + 1
      else
      begin
        High := I - 1;
        if FList[I].Index = Value then
        begin
          Low := I;
          Result := True;
        end;
      end;
    end;
    // Guard for Index always being within boundaries even if result=false!
    if Low >= FListCount then
      Index := FListCount - 1
    else
    begin
      if Low < 0 then
        Index := 0
      else
        Index := Low;
    end;
    //
  end
  else
  begin
    for I := 0 to FListCount - 1 do
      if FList[I].Index = Value then
      begin
        Index := I;
        Result := True;
        Break;
      end;
  end;
  // Return object associated with index even if result is false (needed in PLL)
  if FListCount > 0 then
    AssociatedObject := FList[Index].Obj;
end;

function TTopSortedList.GetInteger(const Index: Integer): Integer;
begin
  Result := FList[Index].Index;
end;

function TTopSortedList.GetCardinal(const Index: Integer): Cardinal;
begin
  Result := Cardinal(GetInteger(Index));
end;

function TTopSortedList.GetPointer(const Index: Integer): Pointer;
begin
  Result := Pointer(GetInteger(Index));
end;

function TTopSortedList.Find(const Value: Integer; out AssociatedObject: Pointer): Boolean;
var
  Index: Integer;
begin
  Result := Find(Value, Index, AssociatedObject);
end;

function TTopSortedList.Find(const Value: Cardinal;
  out AssociatedObject: Pointer): Boolean;
begin
  Result := Find(Integer(Value), AssociatedObject);
end;

function TTopSortedList.Find(const Value: Pointer;
  out AssociatedObject: Pointer): Boolean;
begin
  Result := Find(Integer(Value), AssociatedObject);
end;

procedure TTopSortedList.SetValue(const Index, Value: Integer;
  const AssociatedObject: Pointer);
begin
  // In sorted mode we do not support this and add it sorted
  if FSorted then
    Add(Value, AssociatedObject)
  else
  begin
    // in non sorted mode we do it neatly
    if Index > (FlistCount - 1) then
    begin
      FListCount := Index + 1;
      CheckCapacity;
    end;
    //
    FList[Index].Index := Value;
    FList[Index].Obj := AssociatedObject;
  end;
end;

procedure TTopSortedList.SetValue(const Index: Integer; const Value: Cardinal; const AssociatedObject: Pointer);
begin
  SetValue(Index, Integer(Value), AssociatedObject);
end;

procedure TTopSortedList.SetValue(const Index: Integer; const Value, AssociatedObject: Pointer);
begin
  SetValue(Index, Integer(Value), AssociatedObject);
end;

function TTopSortedList.Find(const Value: Integer; out Index: Integer): Boolean;
var
  Dummy: Pointer;
begin
  Result := Find(Value, Index, Dummy);
end;

function TTopSortedList.Find(const Value: Pointer; out Index: Integer): Boolean;
var
  Dummy: Pointer;
begin
  Result := Find(Integer(Value), Index, Dummy);
end;

function TTopSortedList.Find(const Value: Cardinal; out Index: Integer): Boolean;
var
  Dummy: Pointer;
begin
  Result := Find(Integer(Value), Index, Dummy);
end;

procedure TTopSortedList.GetValue(const Index: Integer; out Value: Integer; out AssociatedObject: Pointer);
begin
  Value := FList[Index].Index;
  AssociatedObject := FList[Index].Obj;
end;

procedure TTopSortedList.GetValue(const Index: Integer; out Value: Cardinal; out AssociatedObject: Pointer);
begin
  Value := Cardinal(FList[Index].Index);
  AssociatedObject := FList[Index].Obj;
end;

procedure TTopSortedList.GetValue(const Index: Integer; out Value: Pointer; out AssociatedObject: Pointer);
begin
  Value := Pointer(FList[Index].Index);
  AssociatedObject := FList[Index].Obj;
end;

function TTopSortedList.Get(Index: Integer): TIndexedObject;
begin
  Result := FList[Index];
end;

procedure TTopSortedList.Put(Index: Integer; const Value: TIndexedObject);
begin
  SetValue(Index, Value.Index, Value.Obj);
end;

procedure TTopSortedList.SetCapacity(const Value: Integer);
begin
  if FListCapacity > FListCount then
  begin
    FList := FixedCapacity(FList, FListCapacity, Value, SizeOf(TIndexedObject), True);
    FListCapacity := Value;
  end;
end;

end.

