{****************************************************************************************

  TOPMEMORY v3.54 - HIGH PERFORMANCE DELPHI MEMORY MANAGER  (C) 2008 Ivo Tops, Topsoftware

  TopMaintenance is the thread that flushes memory in the pool after a period.

****************************************************************************************}
unit TopMaintenance;

interface

{$IFNDEF TOPDEBUG} // Debugging off unless you use the TOPDEBUG directive
{$D-,L-}
{$ENDIF}
{$X+}

uses
  TopPointerList,
  TopInstall,
  Classes;

type
  TTopMaintenance = class(TThread)
  private
    FCurrentManager: Byte;
    FLowMemory:Boolean;
  protected
    procedure CheckOverallMemory;
    procedure CheckManager(const SMIndex: Byte;const AFreeAll:Boolean=False);
    procedure Nextmanager;
    procedure Execute; override;
  public
    constructor Create;
  end;

var
  TopThread: TTopMaintenance = nil;

implementation

uses
  TopManagers,
  TopLocalObjects,
  Windows,
  SysUtils;

{ Detect installing/registering instead of running}

function NotReallyRunning: Boolean;
var DLLFileName: string;

  function FindSwitch(const Switch: string): Boolean;
  begin
    Result := FindCmdLineSwitch(Switch, ['-', '/'], True);
  end;

begin
  if not IsLibrary then
  begin
    // Normal application with special commandline
    Result :=
      FindSwitch('REGSERVER') or
      FindSwitch('UNREGSERVER') or
      FindSwitch('INSTALL') or
      FindSwitch('UNINSTALL');
  end else
  begin
    // DLL with special commandline
    DLLFileName := UpperCase(ExtractFileName(ParamStr(0)));
    Result := (DLLFileName = 'REGSVR32.EXE') or (DLLFileName = 'RUNDLL32.EXE');
  end;
end;

{ TopMaintenance }

procedure TTopMaintenance.CheckManager(const SMIndex: Byte;const AFreeAll:Boolean);
begin
  TopMM.GlobalPool.GetSizeManagerByIndex(SMIndex).ManagePoolSize(AFreeAll);
end;

procedure TTopMaintenance.CheckOverallMemory;
 var
   lInfo:_MEMORYSTATUS;
begin
  lInfo.dwLength := SizeOf(_MEMORYSTATUS);
  GlobalMemoryStatus(lInfo);
  FLowMemory:=lInfo.dwMemoryLoad>=90;
end;

constructor TTopMaintenance.Create;
begin
  inherited Create(False);
  FreeOnTerminate := False;
  FCurrentManager := 0;
  FLowMemory:=False;
end;

procedure TTopMaintenance.Execute;
begin
  while (not Terminated) do
  begin
    try
      // once every while detect the dead non delphi threads and check memory status
      if FCurrentManager < 2 then
      begin
        TopMM.DetectDeadThreads;
        CheckOverallMemory;
        // Enable the next line for running memory allocation reports. Not threadsafe and not to be used in production environments
        // Will write a report of what is currently in use, enabling you to see all allocated objects with counters
        // TopMM.ReportLeaks; // Enable this statement to write a report every couple of seconds
      end;
      //
      CheckManager(FCurrentManager,FLowMemory);
      NextManager;
      CheckManager(FCurrentManager,FLowMemory);
      NextManager;
      //
      Sleep(333);
      //
    except
       // Catch unexpected Errors so thread will continue and not just disappear into thin air
    end;
  end;
end;

procedure TTopMaintenance.Nextmanager;
begin
  // Move to next
  // Cycle to first if last was reached
  if FCurrentManager = cMaxManagers then
    FCurrentManager := 0
  else
    Inc(FCurrentManager);
end;

procedure StopTopThread;
begin
  try
    TOpThread.Terminate;
    TopThread.WaitFor;
    // Free the maintenance thread
    TopThread.Free;
    // before reporting leaks, detect the deadies once more
    TopMM.DetectDeadThreads;
  except
   // Ignore any possible errors in this part as we are closing down anyway
  end;
end;
{$D+}
initialization
  // Do not start a thread when installing/registering as a server. This will otherwise crash the COM+
  // installation of any program using this unit
  if Assigned(TopMM) and (not NotReallyRunning) then TopThread := TTopMaintenance.Create;

finalization
  if Assigned(TopThread) then StopTopThread;


end.

