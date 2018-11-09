{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }
uses Classes;

type
  MPtrUInt  = ptruint;
  MPtr      = Pointer;
  PMPtrUInt = ^MPtrUInt;

var
  OriginMM         : TMemoryManager;
  HookMM           : TMemoryManager;
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
    if length(HookPtrList.ListBuffer^) <> cacheLen then
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

function GetHookMemorySize: nativeUInt;
var
  bak: Boolean;
begin
  bak := MHGlobalHookEnabled;
  MHGlobalHookEnabled := False;
  try
      Result := HookPtrList.Total;
  finally
      MHGlobalHookEnabled := bak;
  end;
end;

function GetHookMemorySize(p: Pointer): nativeUInt;
var
  bak: Boolean;
begin
  bak := MHGlobalHookEnabled;
  MHGlobalHookEnabled := False;
  try
      Result := HookPtrList[p];
  finally
      MHGlobalHookEnabled := bak;
  end;
end;

function GetHookMemoryMinimizePtr: Pointer;
var
  bak: Boolean;
begin
  bak := MHGlobalHookEnabled;
  MHGlobalHookEnabled := False;
  try
      Result := HookPtrList.MinimizePtr;
  finally
      MHGlobalHookEnabled := bak;
  end;
end;

function GetHookMemoryMaximumPtr: Pointer;
var
  bak: Boolean;
begin
  bak := MHGlobalHookEnabled;
  MHGlobalHookEnabled := False;
  try
      Result := HookPtrList.MaximumPtr;
  finally
      MHGlobalHookEnabled := bak;
  end;
end;

function Hash_GetMem(Size: ptruint): Pointer;
begin
  Result := OriginMM.GetMem(Size);
  if (not MemoryHooked) or (not MHGlobalHookEnabled) or (Result = nil) then
      Exit;
  MemoryHooked := False;
  MHGlobalHookEnabled := False;
  try
    if CurrentHookThread = TCoreClassThread.CurrentThread then
        HookPtrList.Add(Result, Size, False);
  finally
    MemoryHooked := True;
    MHGlobalHookEnabled := True;
  end;
end;

function Hash_FreeMem(p: Pointer): ptruint;
begin
  Result := OriginMM.FreeMem(p);
  if (not MemoryHooked) or (not MHGlobalHookEnabled) or (p = nil) then
      Exit;
  MemoryHooked := False;
  MHGlobalHookEnabled := False;
  try
    if CurrentHookThread = TCoreClassThread.CurrentThread then
        HookPtrList.Delete(p);
  finally
    MemoryHooked := True;
    MHGlobalHookEnabled := True;
  end;
end;

function Hash_FreememSize(p: Pointer; Size: ptruint): ptruint;
begin
  Result := OriginMM.FreememSize(p, Size);
  if (not MemoryHooked) or (not MHGlobalHookEnabled) or (p = nil) then
      Exit;
  MemoryHooked := False;
  MHGlobalHookEnabled := False;
  try
    if CurrentHookThread = TCoreClassThread.CurrentThread then
        HookPtrList.Delete(p);
  finally
    MemoryHooked := True;
    MHGlobalHookEnabled := True;
  end;
end;

function Hash_AllocMem(Size: ptruint): Pointer;
begin
  Result := OriginMM.AllocMem(Size);
  if (not MemoryHooked) or (not MHGlobalHookEnabled) or (Result = nil) then
      Exit;
  MemoryHooked := False;
  MHGlobalHookEnabled := False;
  try
    if CurrentHookThread = TCoreClassThread.CurrentThread then
        HookPtrList.Add(Result, Size, True);
  finally
    MemoryHooked := True;
    MHGlobalHookEnabled := True;
  end;
end;

function Hash_ReallocMem(var p: Pointer; Size: ptruint): Pointer;
begin
  Result := OriginMM.ReallocMem(p, Size);
  if (not MemoryHooked) or (not MHGlobalHookEnabled) then
      Exit;
  MemoryHooked := False;
  MHGlobalHookEnabled := False;
  try
    if CurrentHookThread = TCoreClassThread.CurrentThread then
      begin
        if p <> nil then
          begin
            if HookPtrList.Delete(p) then
              if Result <> nil then
                  HookPtrList.Add(Result, Size, False);
          end
        else if Result <> nil then
            HookPtrList.Add(Result, Size, False);
      end;
  finally
    MemoryHooked := True;
    MHGlobalHookEnabled := True;
  end;
end;

procedure InstallMemoryHook;
begin
  HookPtrList := TPointerHashNativeUIntList.CustomCreate(32);
  CurrentHookThread := nil;

  GetMemoryManager(OriginMM);
  HookMM := OriginMM;

  MemoryHooked := False;

  HookMM.GetMem := @Hash_GetMem;
  HookMM.FreeMem := @Hash_FreeMem;
  HookMM.FreememSize := @Hash_FreememSize;
  HookMM.AllocMem := @Hash_AllocMem;
  HookMM.ReallocMem := @Hash_ReallocMem;

  SetMemoryManager(HookMM);
end;

procedure UnInstallMemoryHook;
begin
  MemoryHooked := False;
  SetMemoryManager(OriginMM);
  DisposeObject(HookPtrList);
end; 
 
