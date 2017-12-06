{****************************************************************************************

  TOPMEMORY v3.55 - HIGH PERFORMANCE DELPHI MEMORY MANAGER  (C) 2008 Ivo Tops, Topsoftware

  TopPointerList is a local memory using list for pointers

****************************************************************************************}
unit TopPointerList;

interface

{$IFNDEF TOPDEBUG} // Debugging off unless you use the TOPDEBUG directive
{$D-,L-}
{$ENDIF}
{$X+}

uses
  TopLocalObjects,
  TopLib_CopyMemory;

type
  TPointerArray = array[0..MaxInt div (SizeOf(Pointer)) - 1] of Pointer;
  PPointerArray = ^TPointerArray;

type
  TTopPointerList = class(TLocalObject)
  private
    FInitialCapacity: Integer;
    FCount: Integer;
    FList: PPointerArray;
    FFlag: Boolean;
    FListCapacity: Integer;
    FSorted: Boolean;
    FDupesAllowed: Boolean;
    procedure FixCapacity(const MinimumNewCapacity: Integer; const ZeroNewMemory: Boolean);
    procedure CheckCapacity; {$IF COMPILERVERSION>=18}inline; {$IFEND}
    procedure SetCapacity(const ANewCapacity: Integer);
    procedure QuickSort(const ALeft, ARight: Integer);
    procedure SetSorted(const Value: Boolean);
  protected
    function Get(Index: Integer): Pointer; {$IF COMPILERVERSION>=18}inline; {$IFEND}
    procedure Put(Index: Integer; const Value: Pointer); {$IF COMPILERVERSION>=18}inline; {$IFEND}
  public
    //
    constructor Create(const Sorted: Boolean = False; const DuplicatesAllowed: Boolean = False; const InitialSize: Integer = 9);
    destructor Destroy; override;
    //
    procedure Clear(const AResize: Boolean = True);
    //
    function Add(const Value: Pointer): Integer;
    function Delete(const Value: Pointer): Boolean;
    procedure DeleteByIndex(const Index: Integer);
    function Exists(const Value: Pointer): Boolean; {$IF COMPILERVERSION>=18}inline; {$IFEND}
    function Find(const Value: Pointer): Boolean; overload; {$IF COMPILERVERSION>=18}inline; {$IFEND}
    function Find(const Value: Pointer; out Index: Integer): Boolean; overload;
    //
    property Count: Integer read FCount;
    property Sorted: Boolean read FSorted write SetSorted;
    property Capacity: Integer read FListCapacity write SetCapacity;
    //
    property Items[Index: Integer]: Pointer read Get write Put; default;
    //
    property Flag: Boolean read FFlag write FFlag;
  end;

implementation

uses
  TopLib_SSE2,
  Windows;

function TTopPointerList.Add(const Value: Pointer): Integer;
var
  Index: Integer;
begin
  // Empty List?, Add and Exit
  if FCount = 0 then
  begin
    Result := 0;
    FCount := 1;
    CheckCapacity;
    FList[0] := Value;
    Exit;
  end;
  // Not sorted
  if not FSorted then
  begin
    // Dupe?
    if not FDupesAllowed then
      for Index := 0 to FCount - 1 do if FList[Index] = Value then
        begin
          Result := Index;
          Exit;
        end;
    // Ok, Add
    Inc(FCount);
    CheckCapacity;
    FList[FCount - 1] := Value;
    Result := FCount - 1;
  end
  else
  begin
    // Sorted
    // Dupe?
    if Find(Value, Index) then
    begin
      if not FDupesAllowed then
      begin
        Result := Index;
        Exit;
      end;
    end;
    // Add new value on correct position
    Inc(FCount);
    CheckCapacity;
    //
    // Check upper add (count has been checked to be >0  and we Incremented)
    if Cardinal(FList[FCount - 2]) <= Cardinal(Value) then
    begin
      FList[FCount - 1] := Value;
      Result := FCount - 1;
      Exit;
    end;
    // check bottom add (count has been checked to be >0)
    if Cardinal(FList[0]) >= Cardinal(Value) then
    begin
      TopMoveMemory(Pointer(Cardinal(FList) + SizeOf(Pointer)), FList, SizeOf(Pointer) * (FCount - 1));
      FList[0] := Value;
      Result := 0;
      Exit;
    end;
    // Add in between
    TopMoveMemory(Pointer(Cardinal(FList) + Cardinal(SizeOf(Pointer) * (Index + 1))), Pointer(Cardinal(FList) + Cardinal(SizeOf(Pointer) * (Index))), SizeOf(Pointer) * (FCount - Index - 1));
    // Set new value
    FList[Index] := Value;
    Result := Index;
  end;
end;

procedure TTopPointerList.CheckCapacity;
begin
  // Increase array for new Values
  if FCount >= FListCapacity then FixCapacity(FCount, True);
end;

procedure TTopPointerList.Clear(const AResize: Boolean);
begin
  if AResize then
  begin
    FListCapacity := FInitialCapacity;
    if assigned(FList) then TopLocalMemFree(FList);
    FList := TopLocalMemZeroAlloc(FListCapacity * SizeOf(Pointer));
  end;
  FCount := 0;
  Flag := False;
end;

constructor TTopPointerList.Create(const Sorted: Boolean; const DuplicatesAllowed: Boolean; const InitialSize: Integer);
begin
  inherited Create;
  Flag := False;
  FCount := 0;
  FInitialCapacity := InitialSize;
  FListCapacity := InitialSize;
  FList := TopLocalMemZeroAlloc(FListCapacity * SizeOf(Pointer));
  FSorted := Sorted;
  FDupesAllowed := DuplicatesAllowed;
end;

function TTopPointerList.Delete(const Value: Pointer): Boolean;
var
  Index: Integer;
begin
  Result := Find(Value, Index);
  if Result then DeleteByIndex(Index);
end;

procedure TTopPointerList.DeleteByIndex(const Index: Integer);
begin
  if (Index >= 0) or (Index < FCount) then
  begin
    if Index < FCount - 1 then TopMoveMemory(Pointer(Cardinal(FList) + Cardinal(SizeOf(Pointer) * (Index))), Pointer(Cardinal(FList) + Cardinal(SizeOf(Pointer) * (Index + 1))), SizeOf(Pointer) * (FCount - Index - 1));
    Dec(FCount);
  end; // exceptions do not work in a memory manager; else raise exception.create('Index value outside current boundaries of list (Index <=0 or Index >=Count-1)');
end;

destructor TTopPointerList.Destroy;
begin
  if Assigned(FList) then TopLocalMemFree(FList);
  inherited Destroy;
end;

function TTopPointerList.Exists(const Value: Pointer): Boolean;
begin
  Result := Find(Value);
end;

function TTopPointerList.Find(const Value: Pointer; out Index: Integer): Boolean;
var
  Low, High, I: Integer;
begin
  Index := 0;
  Result := False;
  //
  if FCount = 0 then Exit;
  //
  if (FCount = 1) then
  begin
    if FList[Index] = Value then Result := True;
    Exit;
  end;
  //
  if FSorted then
  begin
    Low := 0;
    High := FCount - 1;
    while Low <= High do
    begin
      I := (Low + High) shr 1;
      if Cardinal(FList[I]) < Cardinal(Value) then
        Low := I + 1
      else
      begin
        High := I - 1;
        if FList[I] = Value then
        begin
          Low := I;
          Result := True;
        end;
      end;
    end;
    // Guard for Index always being within boundaries even if result=false!
    if Low >= FCount then
      Index := FCount - 1
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
    for I := 0 to FCount - 1 do
      if FList[I] = Value then
      begin
        Index := I;
        Result := True;
        Break;
      end;
  end;
end;

function TTopPointerList.Find(const Value: Pointer): Boolean;
var
  Index: Integer;
begin
  Result := Find(Value, Index);
end;

procedure TTopPointerList.Put(Index: Integer; const Value: Pointer);
begin
  if (Index >= 0) or (Index < FCount) then  if not FSorted then  FList[Index] := Value;
  // exceptions do not work in a memory manager -    raise exception.create('Setting values for an Index not supported in sorted lists');
  // exceptions do not work in a memory manager -   raise exception.create('Index value outside current boundaries of list (Index <=0 or Index >=Count-1)');
end;

procedure TTopPointerList.SetCapacity(const ANewCapacity: Integer);
begin
  FixCapacity(ANewCapacity, True);
end;

function TTopPointerList.Get(Index: Integer): Pointer;
begin
  Result := FList[Index];
end;

procedure TTopPointerList.FixCapacity(const MinimumNewCapacity: Integer; const ZeroNewMemory: Boolean);
var
  lOldCapacity: Integer;
begin
  lOldCapacity := FListCapacity;
  // Determine new capacity
  if FListCapacity < 1 then FListCapacity := FInitialCapacity;
  while FListCapacity <= MinimumNewCapacity do FListCapacity := FListCapacity * 2;
  //
  if FList = nil then
  begin
    FList := TopLocalMemZeroAlloc(FListCapacity * SizeOf(Pointer));
  end
  else
  begin
    FList := TopLocalMemReAlloc(FList, FListCapacity * SizeOf(Pointer), lOldCapacity * SizeOf(Pointer));
  end;
  if (FList <> nil) and (FListCapacity > lOldCapacity) then
    if ZeroNewMemory then TopFillMemory(Pointer(Cardinal(FList) + Cardinal(lOldCapacity * SizeOf(Pointer))), (FListCapacity - lOldCapacity) * SizeOf(Pointer), 0);
    //FillChar(Pointer(Cardinal(FList) + Cardinal(lOldCapacity * SizeOf(Pointer)))^, (FListCapacity - lOldCapacity) * SizeOf(Pointer),0);
end;

procedure TTopPointerList.SetSorted(const Value: Boolean);
begin
  if FSorted <> Value then
  begin
    if Value then QuickSort(0, Count - 1);
    FSorted := Value;
  end;
end;

procedure TTopPointerList.QuickSort(const ALeft, ARight: Integer);
var
  I, J, L: Integer;
  P: Cardinal;
  T: Pointer;
begin
  if ARight > ALeft then
  begin
    L := ALeft;
    repeat
      I := L;
      J := ARight;
      P := Cardinal(FList[(L + ARight) shr 1]);
      repeat
        while Cardinal(FList[I]) < P do Inc(I);
        while Cardinal(FList[J]) > P do Dec(J);
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




end.

