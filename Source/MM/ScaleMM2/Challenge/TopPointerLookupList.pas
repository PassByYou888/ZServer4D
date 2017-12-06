{****************************************************************************************

  TOPMEMORY v1.52 - HIGH PERFORMANCE DELPHI MEMORY MANAGER  (C) 2004 Ivo Tops, Topsoftware

  TopPointerLookupList is used to find blocks quickly based on pointers to somewhere in their OSData

****************************************************************************************}
unit TopPointerLookupList;

interface

uses
  TopBlocks,
  TopCS,
  TopSortedList,
  TopLocalObjects;

type
  TPointerLookupList = class(TLocalObject)
  private
    FPLLOSBlockList: TTopsortedList;
  public
    constructor Create;
    destructor Destroy; override;
    //
    procedure AddBlock(const OSBlock: TOSBlock; const SkipMaster: Boolean);
    procedure RemoveBlock(const OSBlockPointer: Pointer; const SkipMaster: Boolean);
    function FindBlock(const AppBlockPointer: Cardinal; out OSBlock: TOSBlock): Boolean;
    //
    procedure Clear;
{$IFDEF TOPDEBUG}
    function GetInfo: string;
{$ENDIF}
  end;

type
  TMasterPLL = class(TCSObject)
  private
    FPLL: TPointerlookuplist;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    //
    procedure AddBlock(const OSBlock: TOSBlock);
    procedure RemoveBlock(const OSBlockPointer: Pointer);
    function FindBlock(const AppBlockPointer: Cardinal; out OSBlock: TOSBlock): Boolean;
    procedure Clear;
    //
{$IFDEF TOPDEBUG}
    function GetInfo: string;
{$ENDIF}
  end;

implementation

uses
  TopInstall
{$IFDEF TOPDEBUG}
  , SysUtils, TopManagers
{$ENDIF};

{ TPointerLookupList }
procedure TPointerLookupList.AddBlock(const OSBlock: TOSBlock; const SkipMaster: Boolean);
begin
  // Put new block on right spot in list
  FPLLOSBlockList.Add(OSBlock.OSBlockPointer, OSBlock);
  // Change Master List
  if (not SkipMaster) then
    TopMM.GlobalPLL.AddBlock(OSBlock);
end;

procedure TPointerLookupList.Clear;
begin
  FPLLOSBlockList.Clear;
end;

constructor TPointerLookupList.Create;
begin
  inherited Create;
  FPLLOSBlockList := TTopSortedList.Create(True);
end;

destructor TPointerLookupList.Destroy;
begin
  FPLLOSBlockList.Free;
  inherited Destroy;
end;

function TPointerLookupList.FindBlock(const AppBlockPointer: Cardinal; out OSBlock: TOSBlock): Boolean;
var Block: Pointer;
  I: Integer;
  OldOSPointer: Cardinal;
begin
  //
  if FPLLOSBlockList.Count > 0 then
  begin
   // Find AppBlock. If exact match exit straigh away
    if FPLLOSBlockList.Find(AppBlockPointer, I, Block) then
    begin
      OSBlock := TOSBlock(Block);
      Result := True;
      Exit;
    end;
  // We need the block before the one returned unless we are out of bounds
    if Cardinal(FPLLOSBlockList[I].Index) > AppBlockPointer then
    begin
      Dec(I);
       // Check lowerbound
      if I < 0 then
      begin
        OSBlock := nil;
        Result := False;
        Exit;
      end;
    end;
    //
    FPLLOSBlockList.GetValue(I,OldOSPointer,Block);
    //
    OSBlock:=TOSBlock(Block);
    //
    // We cannot check OSBlockSize in uniquemode blocks (might change if we look are looking from outside thread though masterPLL)
    // But if this is a unique block we would have had a perfect match and exited at the first lines of this function
    // So first check is for uniquemode to prevent from checking them. All other blocks do not change osblocksize
    // (This will only happen if app checks a pointer that erroneouslt sits inside a uniqueblock OSBlock region)
    Result := (OSBlock.UniqueMode = False) and (AppBlockPointer >= OldOSPointer) and (AppBlockPointer < (OldOSPointer + Cardinal(OSBlock.OSBlockSize)));
    //
    if not Result then OSBlock := nil;
  end else
  begin
    OSBlock := nil;
    Result := False;
    Exit;
  end;
end;

procedure TMasterPLL.AddBlock(const OSBlock: TOSBlock);
begin
  Lock;
  try
    FPLL.AddBlock(OSBlock, True);
  finally
    UnLock;
  end;
end;

procedure TMasterPLL.Clear;
begin
  Lock;
  try
    FPLL.Clear;
  finally
    UnLock;
  end;
end;

function TMasterPLL.FindBlock(const AppBlockPointer: Cardinal; out OSBlock: TOSBlock): Boolean;
begin
  Lock;
  try
    Result := FPLL.FindBlock(AppBlockPointer, OSBlock);
  finally
    UnLock;
  end;
end;

procedure TMasterPLL.RemoveBlock(const OSBlockPointer: Pointer);
begin
  Lock;
  try
    FPLL.RemoveBlock(OSBlockPointer, True);
  finally
    UnLock;
  end;
end;

{$IFDEF TOPDEBUG}
function TPointerLookupList.GetInfo: string;
var
  I: Integer;
begin
  Result := 'PL,TM,SM,OSBLS,APBLS,ABB,FRB,ISA' + Chr(13) + Chr(10);
  for I := 0 to FPLLOSBlockList.COunt - 1 do
  begin
    Result := Result + IntToStr(Ord(TThreadManager(TSizeManager(TOSBlock(FPLLOSBlockList[I].Obj).SizeManager).ThreadManager).IsPoolManager)) + ',';
    Result := Result + IntToStr(Cardinal(TSizeManager(TOSBlock(FPLLOSBlockList[I].Obj).SizeManager).ThreadManager)) + ',';
    Result := Result + IntToStr(Cardinal((TOSBlock(FPLLOSBlockList[I].Obj).SizeManager))) + ',';
    Result := Result + IntToStr((TOSBlock(FPLLOSBlockList[I].Obj).OSBlockSize)) + ',';
    Result := Result + IntToStr((TOSBlock(FPLLOSBlockList[I].Obj).AppBlockSize)) + ',';
    Result := Result + IntToStr((TOSBlock(FPLLOSBlockList[I].Obj).AppBlocks)) + ',';
    Result := Result + IntToStr((TOSBlock(FPLLOSBlockList[I].Obj).AppBlocks - TOSBlock(FPLLOSBlockList[I].Obj).FreeListStart)) + ',';
    Result := Result + IntToStr(Ord((TOSBlock(FPLLOSBlockList[I].Obj).IsAllocated))) + Chr(13) + Chr(10);
  end;
end;
{$ENDIF}

procedure TPointerLookupList.RemoveBlock(const OSBlockPointer: Pointer; const SkipMaster: Boolean);
begin
  FPLLOSBlockList.Delete(OSBlockPointer);
  // Change Master List
  if (not SkipMaster) then
    TopMM.GlobalPLL.RemoveBlock(OSBlockPointer);
end;

constructor TMasterPLL.Create;
begin
  inherited Create;
  FPLL := TPointerLookupList.Create;
end;

destructor TMasterPLL.Destroy;
begin
  FPLL.Free;
  inherited;
end;

{$IFDEF TOPDEBUG}
function TMasterPLL.GetInfo: string;
begin
  Lock;
  try
    Result := FPLL.GetInfo;
  finally
    UnLock;
  end;
end;
{$ENDIF}

end.

