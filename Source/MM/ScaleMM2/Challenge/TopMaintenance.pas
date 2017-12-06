{****************************************************************************************

  TOPMEMORY v1.51 - HIGH PERFORMANCE DELPHI MEMORY MANAGER  (C) 2004 Ivo Tops, Topsoftware

  TopMaintenance is the thread that flushes memory in the pool after a period.

****************************************************************************************}
unit TopMaintenance;

interface

uses
  Classes,
  Windows;

type
  TTopMaintenance = class(TThread)
  private
    FCurrentManager: Byte;
    FStop, FBusy: Boolean;
  protected
    procedure CheckManager(const SMIndex: Byte);
    procedure Nextmanager;
    procedure Execute; override;
  public
    constructor Create;
    //
    procedure Stop;
    property Busy: Boolean read FBusy;
  end;

var
  TopThread: TTopMaintenance = nil;

implementation

uses TopManagers,
  TopInstall,
  SysUtils;

{ Detect installing/registering instead of running}

function NotReallyRunning: Boolean;
  function FindSwitch(const Switch: string): Boolean;
  begin
    Result := FindCmdLineSwitch(Switch, ['-', '/'], True);
  end;
begin
  Result := FindSwitch('REGSERVER') or
    FindSwitch('UNREGSERVER') or
    FindSwitch('INSTALL') or
    FindSwitch('UNINSTALL');
end;

{ TopMaintenance }

procedure TTopMaintenance.CheckManager(const SMIndex: Byte);
begin
  TThreadManagerList(TopMM).GlobalPool.GetSizeManagerByIndex(SMIndex).ManagePoolSize;
end;

constructor TTopMaintenance.Create;
begin
  inherited Create(False);
  FreeOnTerminate := True;
  FCurrentManager := 0;
  FStop := False;
  FBusy := True;
end;

procedure TTopMaintenance.Execute;
begin
  while (not Terminated) do
  begin
    while not FStop do
    begin
      try
        // Check two managers every .25 second so each one is visited 12 /2 = every 3 seconds
        CheckManager(FCurrentManager);
        NextManager;
        CheckManager(FCurrentManager);
        NextManager;
        // Wait some time before checking next managers
{$IFDEF TOPSPEED}
        Sleep(1000); // Every 12 seconds means more pooling
{$ELSE}
        Sleep(250);
{$ENDIF}
      except
        // Catch unexpected Errors so thread will continue and not just disappear into thin air
      end;
    end;
    FStop := False;
    Sleep(0);
  end;
end;

procedure TTopMaintenance.Nextmanager;
begin
  // Move to next
  Inc(FCurrentManager);
  // Cycle to first if last was reached
  if FCurrentManager > CMaxManagers then
    FCurrentManager := 0;
end;

procedure TTopMaintenance.Stop;
begin
  FStop := True;
end;

initialization
  // Do not start a thread when installing/registering as a server. This will otherwise crash the COM+
  // installation of any program using this unit
 if not NotReallyRunning then
  TopThread := TTopMaintenance.Create;

finalization
  if Assigned(TopThread) then
  begin
    // Make sure thread has stopped messing with TopMM before leaving finalization
    TopThread.Stop;
    while TopThread.FStop do
      Sleep(0);
    TopThread.Terminate;
  end;

end.

