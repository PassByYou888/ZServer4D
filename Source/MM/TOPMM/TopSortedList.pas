{****************************************************************************************

  TOPMEMORY v3.53 - HIGH PERFORMANCE DELPHI MEMORY MANAGER  (C) 2008 Ivo Tops, Topsoftware

  TopSortedList is a regular list (both sorted and non-sorted) using local memory

****************************************************************************************}
unit TopSortedList;

interface

{$IFNDEF TOPDEBUG} // Debugging off unless you use the TOPDEBUG directive
{$D-,L-}
{$ENDIF}
{$X+} 

uses
  TopLocalObjects,
  TopLib_CopyMemory;

type
  TIndexedObject = packed record
    Index: Integer;
    Obj: TObject;
  end;
  TIndexedObjectsArray = array[0..MaxInt div (SizeOf(TIndexedObject)) - 1] of TIndexedObject;
  PIndexedObjectsArray = ^TIndexedObjectsArray;

type
  TTopSortedList = class(TLocalObject)
  private
    FInitialCapacity: Integer;
    FListCount: Integer;
    FList: PIndexedObjectsArray;
    FFlag: Boolean;
    FListCapacity: Integer;
    FSorted: Boolean;
    FDupesAllowed: Boolean;
    function Get(Index: Integer): TIndexedObject; {$IF COMPILERVERSION>=18}inline; {$IFEND}
    procedure Put(Index: Integer; const Value: TIndexedObject); {$IF COMPILERVERSION>=18}inline; {$IFEND}
    procedure SetCapacity(const Value: Integer);
    procedure QuickSort(const ALeft, ARight: Integer);
    procedure SetSorted(const Value: Boolean);
  protected
    procedure CheckCapacity;
  public
    //
    constructor Create(const Sorted: Boolean = False; const DuplicatesAllowed: Boolean = False; const InitialSize: Integer = 9);
    destructor Destroy; override;
    //
    procedure Clear;
    //
    function Add(const Value: Integer; const AssociatedObject: Pointer = nil): Integer; overload;
    function Add(const Value: Cardinal; const AssociatedObject: Pointer = nil): Integer; overload; {$IF COMPILERVERSION>=18}inline; {$IFEND}
    function Add(const Value: Pointer; const AssociatedObject: Pointer = nil): Integer; overload; {$IF COMPILERVERSION>=18}inline; {$IFEND}
    function Delete(const Value: Integer): Boolean; overload; // Deze niet inline, te groot
    function Delete(const Value: Cardinal): Boolean; overload; {$IF COMPILERVERSION>=18}inline; {$IFEND}
    function Delete(const Value: Pointer): Boolean; overload; {$IF COMPILERVERSION>=18}inline; {$IFEND}
    procedure DeleteByIndex(const Index: Integer); {$IF COMPILERVERSION>=18}inline; {$IFEND}
    function Exists(const Value: Integer): Boolean; overload; {$IF COMPILERVERSION>=18}inline; {$IFEND}
    function Exists(const Value: Cardinal): Boolean; overload; {$IF COMPILERVERSION>=18}inline; {$IFEND}
    function Exists(const Value: Pointer): Boolean; overload; {$IF COMPILERVERSION>=18}inline; {$IFEND}

    function Find(const Value: Cardinal; out Index: Integer; out AssociatedObject: Pointer): Boolean; overload;{$IF COMPILERVERSION>=18}inline; {$IFEND}
    function Find(const Value: Pointer; out Index: Integer; out AssociatedObject: Pointer): Boolean; overload;{$IF COMPILERVERSION>=18}inline; {$IFEND}
    function Find(const Value: Integer; out Index: Integer; out AssociatedObject: Pointer): Boolean; overload;// Do not inline this one, too larg
    //
    function Find(const Value: Integer; out Index: Integer): Boolean; overload; {$IF COMPILERVERSION>=18}inline; {$IFEND}
    function Find(const Value: Pointer; out Index: Integer): Boolean; overload; {$IF COMPILERVERSION>=18}inline; {$IFEND}
    function Find(const Value: Cardinal; out Index: Integer): Boolean; overload; {$IF COMPILERVERSION>=18}inline; {$IFEND}
    //
    function Find(const Value: Integer; out AssociatedObject: Pointer): Boolean; overload; {$IF COMPILERVERSION>=18}inline; {$IFEND}
    function Find(const Value: Cardinal; out AssociatedObject: Pointer): Boolean; overload; {$IF COMPILERVERSION>=18}inline; {$IFEND}
    function Find(const Value: Pointer; out AssociatedObject: Pointer): Boolean; overload; {$IF COMPILERVERSION>=18}inline; {$IFEND}
    //
    procedure SetValue(const Index: Integer; const Value: Integer; const AssociatedObject: Pointer = nil); overload; // Do not inline this one, too large
    procedure SetValue(const Index: Integer; const Value: Cardinal; const AssociatedObject: Pointer = nil); overload; {$IF COMPILERVERSION>=18}inline; {$IFEND}
    procedure SetValue(const Index: Integer; const Value: Pointer; const AssociatedObject: Pointer = nil); overload; {$IF COMPILERVERSION>=18}inline; {$IFEND}
    //
    procedure SetObj(const Index: Integer; const AssociatedObject: Pointer);
    //
    procedure GetValue(const Index: Integer; out Value: Integer; out AssociatedObject: Pointer); overload; {$IF COMPILERVERSION>=18}inline; {$IFEND}
    procedure GetValue(const Index: Integer; out Value: Cardinal; out AssociatedObject: Pointer); overload; {$IF COMPILERVERSION>=18}inline; {$IFEND}
    procedure GetValue(const Index: Integer; out Value: Pointer; out AssociatedObject: Pointer); overload; {$IF COMPILERVERSION>=18}inline; {$IFEND}
    //
    function GetCardinal(const Index: Integer): Cardinal; {$IF COMPILERVERSION>=18}inline; {$IFEND}
    function GetInteger(const Index: Integer): Integer; {$IF COMPILERVERSION>=18}inline; {$IFEND}
    function GetPointer(const Index: Integer): Pointer; {$IF COMPILERVERSION>=18}inline; {$IFEND}
    //
    property Count: Integer read FListCount;
    property Capacity: Integer read FListCapacity write SetCapacity;
    property Sorted: Boolean read FSorted write SetSorted;
    //
    property Items[Index: Integer]: TIndexedObject read Get write Put; default;
    //
    property Flag: Boolean read FFlag write FFlag;
  end;

implementation

uses
  TopLib_SSE2,
  Windows;

procedure TTopSortedList.DeleteByIndex(const Index: Integer);
//var
//  I: Integer;
begin
//  for I := Index to FListCount - 2 do   FList[I] := FList[I + 1];
  TopMoveMemory(Pointer(Cardinal(@FList[Index])), Pointer(Cardinal(@FList[Index]) + SizeOf(TIndexedObject)), SizeOf(TIndexedObject) * (FListCount-Index-1));
  Dec(FListCount);
end;


function TTopSortedList.Add(const Value: Cardinal; const AssociatedObject: Pointer): Integer;
begin
  Result := Add(Integer(Value), AssociatedObject);
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
  if Assigned(FList) then TopLocalMemFree(FList);
  FList := TopLocalMemZeroAlloc(FListCapacity * SizeOf(TIndexedObject));
  Flag := False;
end;

constructor TTopSortedList.Create(const Sorted: Boolean; const DuplicatesAllowed: Boolean; const InitialSize: Integer);
begin
  inherited Create;
  Flag := False;
  FListCount := 0;
  FInitialCapacity := InitialSize;
  FListCapacity := InitialSize;
  FList := TopLocalMemZeroAlloc(FListCapacity * SizeOf(TIndexedObject));
  FSorted := Sorted;
  FDupesAllowed := DuplicatesAllowed;
end;

function TTopSortedList.Delete(const Value: Integer): Boolean;
var
  Index: Integer;
  Dummy: Pointer;
begin
  Result := Find(Value, Index, Dummy);
  if Result then
    DeleteByIndex(Index);
end;

function TTopSortedList.Delete(const Value: Cardinal): boolean;
begin
  Result := Delete(Integer(Value));
end;

function TTopSortedList.Delete(const Value: Pointer): boolean;
begin
  Result := Delete(Integer(Value));
end;

destructor TTopSortedList.Destroy;
begin
  if Assigned(FList) then TopLocalMemFree(FList);
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


function TTopSortedList.GetInteger(const Index: Integer): Integer;
begin
  Result := FList[Index].Index;
end;

function TTopSortedList.GetCardinal(const Index: Integer): Cardinal;
begin
  Result := Cardinal(FList[Index].Index);
end;

function TTopSortedList.GetPointer(const Index: Integer): Pointer;
begin
  Result := Pointer(FList[Index].Index);
end;

function TTopSortedList.Find(const Value: Integer; out AssociatedObject: Pointer): Boolean;
var
  Index: Integer;
begin
  Result := Find(Value, Index, AssociatedObject);
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
  if not FSorted then
  begin
    // in non sorted mode we do it neatly
    if Index >= FlistCount then
    begin
      FListCount := Index + 1;
      CheckCapacity;
    end;
    // Only Obj Changed means same place
    if FList[Index].Index = Value then
    begin
      FList[Index].Obj := AssociatedObject;
    end else // Also Value changes (which is sorted) requires remove, add
    begin
      DeleteByIndex(Index);
      Add(Value, AssociatedObject);
    end;
  end else
  begin
    if Index >= FlistCount then
      Add(Value, AssociatedObject)
    else // For sorted lists WE DO NOT CHANGE THE INDEX
      FList[Index].Obj := AssociatedObject;
  end;
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
    AssociatedObject := FList[Index].Obj;
    if FList[Index].Index = Value then
      Result := True;
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
    // todo: unrolled loop for performance
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


procedure TTopSortedList.SetCapacity(const Value: Integer);
begin
  if FListCapacity > FListCount then
  begin
    FList := FixedCapacity(FList, FListCapacity, Value, SizeOf(TIndexedObject), True);
    FListCapacity := Value;
  end;
end;

procedure TTopSortedList.SetObj(const Index: Integer; const AssociatedObject: Pointer);
begin
  FList[Index].Obj := AssociatedObject;
end;

procedure TTopSortedList.SetSorted(const Value: Boolean);
begin
  if FSorted <> Value then
  begin
    if Value then QuickSort(0, Count - 1);
    FSorted := Value;
  end;
end;

procedure TTopSortedList.QuickSort(const ALeft, ARight: Integer);
var
  I, J, L: Integer;
  P, T: TIndexedObject;
begin
  if ARight > ALeft then
  begin
    L := ALeft;
    repeat
      I := L;
      J := ARight;
      P := FList[(L + ARight) shr 1];
      repeat
        while FList[I].Index < P.Index do Inc(I);
        while FList[J].Index > P.Index do Dec(J);
        if I <= J then
        begin
          T := FList[I];
          FList[I] := FList[J];
          FList[J] := T;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      // recursive sort subarea
      if L < J then QuickSort(L, J);
      L := I;
    until I >= ARight;
  end;
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
    // Dupe?
    if not FDupesAllowed then
      for Index := 0 to FListCount - 1 do if FList[Index].Index = Value then
        begin
          Result := Index;
          Exit;
        end;
    // No Dupe, add it
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
      if not FDupesAllowed then
      begin
        FList[Index].Obj := AssociatedObject;
        Result := Index;
        Exit;
      end;
    end;
    // Add new value on correct position
    Inc(FListCount);
    CheckCapacity;
    //
    // Check upper add
    if FList[FListCount - 2].Index <= Value then
    begin
      FList[FListCount - 1].Index := Value;
      FList[FListCount - 1].Obj := AssociatedObject;
      Result := FListCount - 1;
      Exit;
    end;
    // check bottom add
    if FList[0].Index >= Value then
    begin
      TopMoveMemory(Pointer(Cardinal(FList) + SizeOf(TIndexedObject)), FList, SizeOf(TIndexedObject) * (FListCount));
      FList[0].Index := Value;
      FList[0].Obj := AssociatedObject;
      Result := 0;
      Exit;
    end;
    // Add in between
    TopMoveMemory(Pointer(Cardinal(FList) + Cardinal(SizeOf(TIndexedObject) * (Index + 1))), Pointer(Cardinal(FList) + Cardinal(SizeOf(TIndexedObject) * (Index))), SizeOf(TIndexedObject) * (FListCount - Index));
    // Set new value
    FList[Index].Index := Value;
    FList[Index].Obj := AssociatedObject;
    Result := Index;
  end;
end;


end.

