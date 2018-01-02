{$I zDefine.inc}

interface

uses ListEngine, CoreClasses;

procedure BeginMemoryHook; overload;
procedure BeginMemoryHook(cacheLen: Integer); overload;
procedure EndMemoryHook;
function GetHookMemorySize: NativeUInt;

threadvar
  MemoryHooked: Boolean;

var
  HookPtrList: TPointerHashNativeUIntList;

implementation

type
  MPtrUInt  = NativeUInt;
  MPtr      = Pointer;
  PMPtrUInt = ^MPtrUInt;

var
  OriginMM         : TMemoryManagerEx;
  HookMM           : TMemoryManagerEx;
  CurrentHookThread: TCoreClassThread;

procedure BeginMemoryHook;
begin
  if MemoryHooked then
      RaiseInfo('already Hooking');

  CurrentHookThread := TCoreClassThread.CurrentThread;
  HookPtrList.FastClear;
  MemoryHooked := True;
end;

procedure BeginMemoryHook(cacheLen: Integer);
begin
  if MemoryHooked then
      RaiseInfo('already Hooking');

  try
    CurrentHookThread := TCoreClassThread.CurrentThread;
    if Length(HookPtrList.ListBuffer^) <> cacheLen then
        HookPtrList.SetHashBlockCount(cacheLen)
    else
        HookPtrList.FastClear;
  except
  end;
  MemoryHooked := True;
end;

procedure EndMemoryHook;
begin
  if not MemoryHooked then
      RaiseInfo('before hook');

  MemoryHooked := False;
  CurrentHookThread := nil;
end;

function GetHookMemorySize: NativeUInt;
var
  bak: Boolean;
  i  : Integer;
  p  : PPointerHashListNativeUIntStruct;
begin
  bak := MemoryHooked;
  MemoryHooked := False;
  Result := 0;
  try
    if HookPtrList.Count > 0 then
      begin
        i := 0;
        p := HookPtrList.FirstPtr;
        while i < HookPtrList.Count do
          begin
            Inc(Result, p^.Data);
            Inc(i);
            p := p^.next;
          end;
      end;
  finally
      MemoryHooked := bak;
  end;
end;

function Hash_GetMem(Size: NativeInt): MPtr;
begin
  Result := OriginMM.GetMem(Size);
  if (not MemoryHooked) or (Result = nil) then
      exit;
  MemoryHooked := False;
  try
    if CurrentHookThread = TCoreClassThread.CurrentThread then
        HookPtrList.Add(Result, Size, False);
  finally
      MemoryHooked := True;
  end;
end;

function Hash_FreeMem(p: MPtr): Integer;
begin
  Result := OriginMM.FreeMem(p);
  if (not MemoryHooked) or (p = nil) then
      exit;
  MemoryHooked := False;
  try
    if CurrentHookThread = TCoreClassThread.CurrentThread then
        HookPtrList.Delete(p);
  finally
      MemoryHooked := True;
  end;
end;

function Hash_ReallocMem(p: MPtr; Size: NativeInt): MPtr;
begin
  Result := OriginMM.ReallocMem(p, Size);
  if (not MemoryHooked) then
      exit;
  MemoryHooked := False;
  try
    if CurrentHookThread = TCoreClassThread.CurrentThread then
      begin
        if p <> nil then
            HookPtrList.Delete(p);
        if Result <> nil then
            HookPtrList.Add(Result, Size, False);
      end;
  finally
      MemoryHooked := True;
  end;
end;

function Hash_AllocMem(Size: NativeInt): MPtr;
begin
  Result := OriginMM.AllocMem(Size);
  if (not MemoryHooked) or (Result = nil) then
      exit;
  MemoryHooked := False;
  try
    if CurrentHookThread = TCoreClassThread.CurrentThread then
        HookPtrList.Add(Result, Size);
  finally
      MemoryHooked := True;
  end;
end;

procedure InstallMemoryHook;
begin
  HookPtrList := TPointerHashNativeUIntList.Create(32);
  CurrentHookThread := nil;

  GetMemoryManager(OriginMM);
  HookMM := OriginMM;

  MemoryHooked := False;

  HookMM.GetMem := Hash_GetMem;
  HookMM.FreeMem := Hash_FreeMem;
  HookMM.ReallocMem := Hash_ReallocMem;
  HookMM.AllocMem := Hash_AllocMem;

  SetMemoryManager(HookMM);
end;

procedure UnInstallMemoryHook;
begin
  MemoryHooked := False;
  SetMemoryManager(OriginMM);
  DisposeObject(HookPtrList);
end;

initialization

InstallMemoryHook;

finalization

UnInstallMemoryHook;

end.
