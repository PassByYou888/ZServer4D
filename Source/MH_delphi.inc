{ ****************************************************************************** }
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
type
  MPtrUInt = nativeUInt;
  MPtr = Pointer;
  PMPtrUInt = ^MPtrUInt;

var
  OriginMM: TMemoryManagerEx;
  HookMM: TMemoryManagerEx;
  CurrentHookThread: TCoreClassThread;

procedure BeginMemoryHook;
begin
  if (MemoryHooked) or (CurrentHookThread <> nil) then
      RaiseInfo('illegal BeginMemoryHook');

  CurrentHookThread := TCoreClassThread.CurrentThread;
  HookPtrList.FastClear;
  MemoryHooked := True;
end;

procedure BeginMemoryHook(cacheLen: Integer);
begin
  if (MemoryHooked) or (CurrentHookThread <> nil) then
      RaiseInfo('illegal BeginMemoryHook');

  CurrentHookThread := TCoreClassThread.CurrentThread;
  if length(HookPtrList.ListBuffer^) <> cacheLen then
      HookPtrList.SetHashBlockCount(cacheLen)
  else
      HookPtrList.FastClear;

  MemoryHooked := True;
end;

procedure EndMemoryHook;
begin
  if not MemoryHooked then
      RaiseInfo('illegal EndMemoryHook');

  MemoryHooked := False;
  CurrentHookThread := nil;
end;

function GetHookMemorySize: nativeUInt;
begin
  Result := HookPtrList.Total;
end;

function GetHookMemorySize(p: Pointer): nativeUInt;
begin
  Result := HookPtrList[p];
end;

function GetHookMemoryMinimizePtr: Pointer;
begin
  Result := HookPtrList.MinimizePtr;
end;

function GetHookMemoryMaximumPtr: Pointer;
begin
  Result := HookPtrList.MaximumPtr;
end;

function Hash_GetMem(Size: NativeInt): MPtr;
begin
  Result := OriginMM.GetMem(Size);
  if (not MemoryHooked) or (not GlobalMemoryHook) or (Result = nil) or (CurrentHookThread <> TCoreClassThread.CurrentThread) then
      Exit;
  MemoryHooked := False;
  GlobalMemoryHook := False;
  HookPtrList.Add(Result, Size, False);
  MemoryHooked := True;
  GlobalMemoryHook := True;
end;

function Hash_FreeMem(p: MPtr): Integer;
begin
  Result := OriginMM.FreeMem(p);
  if (not MemoryHooked) or (not GlobalMemoryHook) or (p = nil) or (CurrentHookThread <> TCoreClassThread.CurrentThread) then
      Exit;
  MemoryHooked := False;
  GlobalMemoryHook := False;
  HookPtrList.Delete(p);
  MemoryHooked := True;
  GlobalMemoryHook := True;
end;

function Hash_ReallocMem(p: MPtr; Size: NativeInt): MPtr;
begin
  Result := OriginMM.ReallocMem(p, Size);
  if (not MemoryHooked) or (not GlobalMemoryHook) or (CurrentHookThread <> TCoreClassThread.CurrentThread) then
      Exit;
  MemoryHooked := False;
  GlobalMemoryHook := False;
  if p <> nil then
    begin
      if HookPtrList.Delete(p) then
        if Result <> nil then
            HookPtrList.Add(Result, Size, False);
    end
  else if Result <> nil then
      HookPtrList.Add(Result, Size, False);
  MemoryHooked := True;
  GlobalMemoryHook := True;
end;

function Hash_AllocMem(Size: NativeInt): MPtr;
begin
  Result := OriginMM.AllocMem(Size);
  if (not MemoryHooked) or (not GlobalMemoryHook) or (Result = nil) or (CurrentHookThread <> TCoreClassThread.CurrentThread) then
      Exit;
  MemoryHooked := False;
  GlobalMemoryHook := False;
  HookPtrList.Add(Result, Size, False);
  MemoryHooked := True;
  GlobalMemoryHook := True;
end;

procedure InstallMemoryHook;
begin
  HookPtrList := TPointerHashNativeUIntList.CustomCreate(32);
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
