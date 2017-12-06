{****************************************************************************************

  TOPMEMORY v3.53 - HIGH PERFORMANCE DELPHI MEMORY MANAGER  (C) 2008 Ivo Tops, Topsoftware

  Routines to report debug output through the DebuggerOutput interface of windows

****************************************************************************************}
unit TopReporting;

interface

{$IFNDEF TOPDEBUG} // Debugging off unless you use the TOPDEBUG directive
{$D-,L-}
{$ENDIF}
{$X+}

{$B-}

uses
  Windows,
  TopSortedList;

// make out exitreport logfile go to some special place and name
// set this anywhere in your code before application closes
procedure SetMemoryLeaksLogFile(const FileName: ShortString);
// are we running with a debugger attached?
function RunningFromIDE: Boolean;
// report a string to the debugger
procedure ReportString(const S: ShortString);
// Sent a list of leaks to report
procedure OutputLeaks(ALeaks: TTopsortedList);
// test if a pointer is a delphi tobject or descendant
function IsObject(APossibleObject: Pointer): Boolean;

var
{$IFNDEF VER180}
  ReportMemoryLeaksOnShutdown: Boolean;
{$ENDIF}
  ReportMemoryLeaksToLogFile: Boolean;
  ReportMemoryLeaksToIDE: Boolean;

// windows api call definition
{$EXTERNALSYM IsDebuggerPresent}
function IsDebuggerPresent: BOOL; stdcall;
function IsDebuggerPresent; external kernel32 Name 'IsDebuggerPresent';

const
  MaxCard = $FFFFFFFF;

implementation


uses
  TopInstall,
  SysUtils;

var
  DebuggerFound: Boolean = False;
  LogFileName: ShortString;
  LogFile: TextFile;
  Logging: Boolean = False;
  LogOk: Boolean = False;


function LibraryFileName: ShortString;
begin
  if GetModuleFileName(hInstance, PChar(Cardinal(@Result) + 1), 255) = 0 then
    Result := '';
end;

function GetFullFileName: ShortString;
begin
  if IsLibrary then
    Result := LibraryFileName
  else
    Result := ParamStr(0);
end;

function GetFileName: string;
begin
  Result := ExtractFileName(GetFullFileName);
end;

function GetDefaultLogFileName: Shortstring;
begin
  Result := ChangeFileExt(GetFullFileName, '_MemoryLeaks.TXT');
end;

procedure SetMemoryLeaksLogFile(const FileName: ShortString);
begin
  LogFileName := FileName;
end;

function OpenLogFile: Boolean;
begin
  try
    if LogFileName = '' then
      LogFileName := GetDefaultLogFileName;
    AssignFile(LogFile, LogFileName);
    if not FileExists(LogFileName) then
      ReWrite(LogFile);
    Append(LogFile);
    Writeln(LogFile, 'TopMemory ExitReport for ' + GetFileName + ' on ' + DateTimeToStr(Now));
    Writeln(LogFile, '============================================================================');
    Result := True;
    Logging := True;
  except
    Result := False;
  end;
end;

procedure CloseLogFile;
begin
  if LogOk and Logging then
  begin
    Writeln(LogFile, '============================================================================');
    CloseFile(LogFile);
    Logging := False;
  end;
end;


procedure LogToFile(const Text: ShortString);
begin
  // Lazy create of file (only when we have somehting to log)
  if ReportMemoryLeaksToLogFile and (not Logging) then
    LogOk := OpenLogFile;
  // Log it
  if LogOk then
  begin
    try
      Writeln(LogFile, Text);
    except
      LogOk := False;
    end;
  end;
end;

procedure ReportString(const S: ShortString);
var
  Display: ShortString;
begin
 // optionally output to IDE
  if RunningFromIDE and ReportMemoryLeaksToIDE then
  begin
    Display := S + Chr(0);
    OutputDebugString(PChar(Cardinal(@Display) + 1));
  end;
  // File?
  if ReportMemoryLeaksToLogFile then
    LogToFile(S);
end;

function IsObject(APossibleObject: Pointer): Boolean;
var
  lVMTClassName: Cardinal;
  Temp: Pointer;
  M: _MEMORY_BASIC_INFORMATION;

  // Using Win IsBadReadPtr gives debugger errors when done too many times. VirtualQuery does not
  function IsBadPointer(const P: Pointer): Boolean;
  begin
    // Doing VirtualQuery here many times very fast will result in AV
    if (Cardinal(P) > 0) and (Cardinal(P) < MaxCard - 3) then
      Result := (VirtualQuery(P, M, SizeOf(M)) = 0) or (Cardinal(M.BaseAddress) >= Cardinal(P)) or (M.BaseAddress=nil) or (M.AllocationBase=nil) or (M.AllocationProtect=0) or (M.Protect=0) //IsBadReadPtr(P,4)   //InterlockedExchangeAdd(P,0)
    else
      Result := True;
  end;

  function IsValidShortStringPtr(P: Pointer): Boolean;
  var
    Len: Byte;
    B: Byte;
    I: Byte;
  begin
    // Check if we have valid chars in the Len
    Len := TByteArray(P^)[0];
    // At end of len we require a zero
    Result := Len > 1;
    if Result then
    begin
      for I := 1 to Len do
      begin
        B := TByteArray(P^)[I];
        if (B < 14) or (B > 127) then
        begin
          Result := False;
          Break;
        end;
      end;
    end;
  end;

begin
  Temp := Pointer(APossibleObject^);
  // We base our result on 4 linked pointers (Object-ClassRef-Classname-PChar). If all 4 are valid
  // pointers to our processes memory and at the end we have a text of chars then we are very
  // certain this was an object
  try
    lVMTClassName := ABS(vmtClassName);
    Result := not IsBadPointer(Temp);
    if Result then
      Result := not IsBadPointer(Pointer(Cardinal(Temp) - lvmtClassName));
    if Result then
    begin
      Result := not IsBadPointer(Pointer(Pointer(Cardinal(Temp) - lvmtClassName)^));
    end;
    if Result then
      Result := not IsBadPointer(PPointer(Cardinal(Temp) - lvmtClassName)^);
    if Result then
      Result := not IsBadPointer(Pointer(PPointer(Cardinal(Temp) - lvmtClassName)^));
    // Check if we point to a valid shortstring
    if Result then
      Result := IsValidShortStringPtr(Pointer(PPointer(Cardinal(Temp) - lvmtClassName)^));
  except
    // Ignore exceptions. If we guess wrong about an object then the exception is correct.
    // They will only come at Application Close when your are debugging. We now set to false and report it later
    // as a block instead of an object
    Result := False;
  end;
end;

function RunningFromIDE: Boolean;
begin
  Result := DebuggerFound = True;
end;

type
  TLeak = packed record
    ClassRef: Pointer;
    Count: Integer;
    LeakSize: Cardinal;
  end;
  TLeakPtr = ^TLeak;

procedure OutputLeaks(ALeaks: TTopsortedList);
var
  ObjectList, BlockList: TTopsortedList;
  I, Idx: Integer;
  Size: Cardinal;
  ObjectClassRef: TClass;
  KnownObject, KnownBlock: Boolean;
  Name: ShortString;
  Number: Cardinal;
  BlockSize: Cardinal;
begin
  try
    //
    // Report all objects grouped by class
    //
    ObjectList := TTopsortedList.Create(True);
    try
      BlockList := TTopsortedList.Create(True);
      try
        for I := 0 to ALeaks.Count - 1 do
        begin
          // Check if object in ObjectList
          KnownBlock := False;
          KnownObject := ObjectList.Find(TClass(Pointer(ALeaks[I].Index)^), Idx);
          // check if block in blocklist
          if not KnownObject then
            KnownBlock := BlockList.Find(TClass(Pointer(ALeaks[I].Index)^), Idx);
          if not KnownBlock then
          begin
            KnownObject := IsObject(Pointer(ALeaks[I].Index));
            if KnownObject then ObjectClassRef := TClass(Pointer(Pointer(ALeaks[I].Index)^)) else ObjectClassRef := nil;
          end
          else
            ObjectClassRef := TClass(Pointer(ALeaks[I].Index)^);
          if KnownObject then
          begin
            // Make list with 1 entry for each class and counter of how many times
            // Add it if new, otherwise increment the count
            if ObjectList.Find(ObjectClassRef, Idx) then
              TLeakPtr(ObjectList.Items[Idx].Obj).Count := TLeakPtr(ObjectList.Items[Idx].Obj).Count + 1
            else
            begin
              Size := Cardinal(ALeaks[I].Obj);
              Idx := ObjectList.Add(ObjectClassRef, New(TLeakPtr));
              TLeakPtr(ObjectList.Items[Idx].Obj).LeakSize := Size;
              TLeakPtr(ObjectList.Items[Idx].Obj).Count := 1;
            end;
          end
          else
          begin
            // Make list with 1 entry for each class and counter of how many times
            // Add it if new, otherwise increment the count
            if BlockList.Find(ALeaks[I].Obj, Idx) then
              BlockList.SetObj(Idx, Pointer(Cardinal(BlockList.Items[Idx].Obj) + 1))
            else
              Idx := BlockList.Add(ALeaks[I].Obj, Pointer(1));
          end;
        end;
        //
        // Report Objects
        //
        for I := 0 to ObjectList.Count - 1 do
        begin
          try
            Name := PShortString(PPointer(ObjectList[I].Index + vmtClassName)^)^;
          except
            // Ignore errors here
          end;
          Number := TLeakPtr(ObjectList.Items[I].Obj)^.Count;
          ReportString(IntToStr(Number) + ' instance(s) of ' + Name + ' not Freed, for a total of ' + IntToStr(TLeakPtr(ObjectList.Items[I].Obj)^.LeakSize * Number) + ' Bytes.');
          Dispose(TLeakPtr(ObjectList.Items[I].Obj));
        end;
        //
        // Report Blocks that are not objects grouped by size
        //
        for I := 0 to BlockList.Count - 1 do
        begin
          Number := Cardinal(BlockList[I].Obj);
          BlockSize := BlockList[I].Index;
          ReportString(IntToStr(Number) + ' block(s) of max. ' + IntToStr(BlockSize) + ' Bytes not freed, for a total of ' + IntToStr(BlockSize * Number) + ' Bytes');
        end;
      finally
        BlockList.Free;
      end;
    finally
      ObjectList.Free;
    end;
  finally
    CloseLogFile;
  end;
end;



initialization
  DebuggerFound := IsDebuggerPresent;

end.

