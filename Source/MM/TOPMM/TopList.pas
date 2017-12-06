{****************************************************************************************

  TOPLIST TopList is a regular list (both sorted and non-sorted)

****************************************************************************************}
unit TopList;

interface

uses
  TopLib_CopyMemory;

type
  TIntegerArray = array[0..MaxInt div (SizeOf(Integer)) - 1] of Integer;
  PIntegerArray = ^TIntegerArray;

type
  TTopList = class
  private
    FInitialCapacity: Integer;
    FCount: Integer;
    FList: PIntegerArray;
    FFlag: Boolean;
    FListCapacity: Integer;
    FSorted: Boolean;
    FDupesAllowed: Boolean;
    function Get(Index: Integer): Integer; {$if CompilerVersion >= 17.0}inline;{$ifend}
    procedure Put(Index: Integer; const Value: Integer); {$if CompilerVersion >= 17.0}inline;{$ifend}
    procedure FixCapacity(const MinimumNewCapacity: Integer; const ZeroNewMemory: Boolean);
    procedure CheckCapacity; {$if CompilerVersion >= 17.0}inline;{$ifend}
    procedure SetCapacity(const Value: Integer);
    procedure QuickSort(const ALeft, ARight: Integer);
    procedure SetSorted(const Value: Boolean);
  public
    //
    constructor Create(const Sorted: Boolean = False; const DuplicatesAllowed: Boolean = False; const InitialSize: Integer = 8);
    destructor Destroy; override;
    //
    procedure Clear(const AResize: Boolean = True);
    //
    function Add(const Value: Integer): Integer;
    function Delete(const Value: Integer): Boolean;
    procedure DeleteByIndex(const Index: Integer);
    function Exists(const Value: Integer): Boolean; {$if CompilerVersion >= 17.0}inline;{$ifend}
    function Find(const Value: Integer): Boolean; overload; {$if CompilerVersion >= 17.0}inline;{$ifend}
    function Find(const Value: Integer; out Index: Integer): Boolean; overload;
    //
    property Count: Integer read FCount;
    property Sorted: Boolean read FSorted write SetSorted;
    property Capacity:Integer read FListCapacity write SetCapacity;
    //
    property Items[Index: Integer]: Integer read Get write Put; default;
    //
    property Flag: Boolean read FFlag write FFlag;
  end;

implementation

uses
  SysUtils,
  TopLib_SSE2,
  Windows;

function TTopList.Add(const Value: Integer): Integer;
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
    if FList[FCount - 2] <= Value then
    begin
      FList[FCount - 1] := Value;
      Result := FCount - 1;
      Exit;
    end;
    // check bottom add (count has been checked to be >0)
    if FList[0] >= Value then
    begin
      TopMoveMemory(Pointer(Cardinal(FList) + SizeOf(Integer)), FList, SizeOf(Integer) * (FCount - 1));
      FList[0] := Value;
      Result := 0;
      Exit;
    end;
    // Add in between
    TopMoveMemory(Pointer(Cardinal(FList) + Cardinal(SizeOf(Integer) * (Index+1))), Pointer(Cardinal(FList) + Cardinal(SizeOf(Integer) * (Index ))), SizeOf(Integer) * (FCount - Index - 1));
    // Set new value
    FList[Index] := Value;
    Result := Index;
  end;
end;

procedure TTopList.CheckCapacity;
begin
  // Increase array for new Values
  if FCount >= FListCapacity then FixCapacity(FCount, True);
end;

procedure TTopList.Clear(const AResize: Boolean);
begin
  if AResize then
  begin
    FListCapacity := FInitialCapacity;
    FList := ReAllocMemory(FList, FListCapacity * SizeOf(Integer));
  end;
  FCount := 0;
  Flag := False;
end;

constructor TTopList.Create(const Sorted: Boolean; const DuplicatesAllowed: Boolean; const InitialSize: Integer);
begin
  inherited Create;
  Flag := False;
  FCount := 0;
  FInitialCapacity := InitialSize;
  FListCapacity := InitialSize;
  FList := AllocMem(FListCapacity * SizeOf(Integer));
  FSorted := Sorted;
  FDupesAllowed := DuplicatesAllowed;
end;

function TTopList.Delete(const Value: Integer): Boolean;
var
  Index: Integer;
begin
  Result := Find(Value, Index);
  if Result then DeleteByIndex(Index);
end;

procedure TTopList.DeleteByIndex(const Index: Integer);
begin
  if (Index >= 0) or (Index < FCount) then
  begin
    if Index < FCount - 1 then TopMoveMemory(Pointer(Cardinal(FList) + Cardinal(SizeOf(Integer) * (Index))), Pointer(Cardinal(FList) + Cardinal(SizeOf(Integer) * (Index + 1))), SizeOf(Integer) * (FCount - Index - 1));
    Dec(FCount);
  end else raise exception.create('Index value outside current boundaries of list (Index <=0 or Index >=Count-1)');
end;

destructor TTopList.Destroy;
begin
  if Assigned(FList) then FreeMem(FList);
  inherited Destroy;
end;

function TTopList.Exists(const Value: Integer): Boolean;
begin
  Result := Find(Value);
end;

function TTopList.Find(const Value: Integer; out Index: Integer): Boolean;
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
      if FList[I] < Value then
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

function TTopList.Find(const Value: Integer): Boolean;
var
  Index: Integer;
begin
  Result := Find(Value, Index);
end;

procedure TTopList.Put(Index: Integer; const Value: Integer);
begin
  if (Index >= 0) or (Index < FCount) then
  begin
    if not FSorted then
      FList[Index] := Value
    else
      raise exception.create('Setting values for an Index not supported in sorted lists');
  end else raise exception.create('Index value outside current boundaries of list (Index <=0 or Index >=Count-1)');
end;

procedure TTopList.SetCapacity(const Value: Integer);
begin
  FixCapacity(Value,True);
end;

function TTopList.Get(Index: Integer): Integer;
begin
  Result := FList[Index];
end;

procedure TTopList.FixCapacity(const MinimumNewCapacity: Integer; const ZeroNewMemory: Boolean);
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
    FList := AllocMem(FListCapacity * SizeOf(Integer));
  end
  else
  begin
    FList := ReallocMemory(FList, FListCapacity * SizeOf(Integer));
  end;
  if (FList <> nil) and (FListCapacity>lOldCapacity) then
    if ZeroNewMemory then TopFillMemory(Pointer(Cardinal(FList) + Cardinal(lOldCapacity * SizeOf(Integer))), (FListCapacity - lOldCapacity) * SizeOf(Integer));
end;

procedure TTopList.SetSorted(const Value: Boolean);
begin
  if FSorted <> Value then
  begin
    if Value then QuickSort(0, Count - 1);
    FSorted := Value;
  end;
end;

procedure TTopList.QuickSort(const ALeft, ARight: Integer);
var
  I, J, L: Integer;
  P, T: Integer;
begin
  if ARight > ALeft then
  begin
    L := ALeft;
    repeat
      I := L;
      J := ARight;
      P := FList[(L + ARight) shr 1];
      repeat
        while FList[I]< P do Inc(I);
        while FList[J]> P do Dec(J);
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

