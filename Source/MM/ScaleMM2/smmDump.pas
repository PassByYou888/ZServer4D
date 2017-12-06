unit smmDump;

interface

  procedure DumpScaleMMStateToFile(const aFilename: string);

implementation

uses
  Windows, ImageHlp, TlHelp32,
  ScaleMM2, smmSmallMemory, smmGlobal, smmStatistics, smmFunctions;

type
  TCardinalArray = array of NativeUInt;

function BuildThreadsList(ProcessID: DWORD): TCardinalArray;
var
  SnapProcHandle: THandle;
  ThreadEntry: TThreadEntry32;
  Next: Boolean;
begin
  Result := nil;
  SnapProcHandle := CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, 0);
  if SnapProcHandle <> THandle(-1) then
  begin
    ThreadEntry.dwSize := Sizeof(ThreadEntry);
    Next := Thread32First(SnapProcHandle, ThreadEntry);
    while Next do
    begin
      if ThreadEntry.th32OwnerProcessID = ProcessID then
        with ThreadEntry do
        begin
          SetLength(Result, length(Result)+1);
          Result[High(Result)] := th32ThreadID;
        end;
      Next := Thread32Next(SnapProcHandle, ThreadEntry);
    end;
    CloseHandle(SnapProcHandle);
  end;
end;

function OpenThread(dwDesiredAccess: DWORD; bInheritHandle: BOOL;
  dwThreadId: DWORD): THandle; stdcall; external 'kernel32.dll';

function SuspendAllThreads: TCardinalArray;
var
  i: Integer;
  h: thandle;
const
  THREAD_SUSPEND_RESUME    = $0002;
begin
  Result := BuildThreadsList(GetCurrentProcessId);
  for i := 0 to High(Result) do
  begin
    if Result[i] <> GetCurrentThreadId then
    begin
      h := OpenThread(THREAD_SUSPEND_RESUME, False, Result[i]);
      SuspendThread(h);
      Result[i] := h;
    end
    else
      Result[i] := 0;
  end;
end;

procedure ResumeAllThreads(const aPreviousThreads: TCardinalArray);
var
  i: Integer;
begin
  for i := low(aPreviousThreads) to high(aPreviousThreads) do
  begin
    if aPreviousThreads[i] > 0 then
    begin
      ResumeThread(aPreviousThreads[i]);
      CloseHandle(aPreviousThreads[i]);
    end;
  end;
end;

procedure DumpScaleMMStateToFile(const aFilename: string);
var
  hfile: THandle;
  threadmem: PThreadMemManager;
  totalstats: TThreadMemManagerStats;
  threadstats: TThreadMemManagerStats;
  threads: TCardinalArray;
begin
  hfile := CreateFile(PChar(aFilename),
      GENERIC_READ or GENERIC_WRITE,           { access (read-write) mode }
      FILE_SHARE_READ or FILE_SHARE_WRITE,     { share mode }
      nil,                                     { pointer to security attributes }
      CREATE_ALWAYS,                           { how to create }
      FILE_ATTRIBUTE_TEMPORARY, //max caching  { file attributes }
      0);
  try
    smmGlobal.GlobalManager.ThreadLock;

    threads := SuspendAllThreads;
    try
      threadmem := smmGlobal.GlobalManager.GetFirstThreadMemory;
      FillChar(totalstats, SizeOf(TThreadMemManagerStats), 0);
      while threadmem <> nil do
      begin
        FillChar(threadstats, SizeOf(TThreadMemManagerStats), 0);

        WriteToFile(hfile, '----------------------------------------------');
        WriteToFile(hfile, #13#10'ThreadID: ');
        WriteNativeUIntToStrBuf(hfile, threadmem.FThreadId);
        WriteToFile(hfile, #13#10);

        threadmem.DumpToFile(hfile, @totalstats, @threadstats);

        WriteToFile(hfile, #13#10'--------');
        WriteToFile(hfile, #13#10'Summary: ');
        WriteToFile(hfile, #13#10);
        threadstats.DumpToFile(hfile);

        threadmem := threadmem.FNextThreadManager;
      end;

      WriteToFile(hfile, #13#10'----------------------------------------------');
      WriteToFile(hfile, #13#10'Total Summary: ');
      totalstats.DumpToFile(hfile);
    finally
      ResumeAllThreads(threads);
      smmGlobal.GlobalManager.ThreadUnLock;
    end;
  finally
    CloseHandle(hfile);
  end;
end;

end.
