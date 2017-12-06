{****************************************************************************************

  TOPMEMORY v3.53 - HIGH PERFORMANCE DELPHI MEMORY MANAGER  (C) 2008 Ivo Tops, Topsoftware

  TopCS is a simple Base for a Class using a critical section and using local memory

****************************************************************************************}
unit TopCS;

interface

{$IFNDEF TOPDEBUG} // Debugging off unless you use the TOPDEBUG directive
{$D-,L-}
{$ENDIF}
{$X+} 

uses
  Windows,
  TopLocalObjects;

type
  TCSObject = class(TLocalObject)
  private
    FCritSect: _RTL_CRITICAL_SECTION;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    //
    procedure Lock; {$IF COMPILERVERSION>=18}inline; {$IFEND}
    procedure Unlock; {$IF COMPILERVERSION>=18}inline; {$IFEND}
  end;

implementation

constructor TCSObject.Create;
begin
  inherited Create;
  InitializeCriticalSection(FCritSect);
end;

destructor TCSObject.Destroy;
begin
  DeleteCriticalSection(FCritsect);
  inherited Destroy;
end;

procedure TCSObject.Lock;
begin
  EnterCriticalSection(FCritSect);
end;

procedure TCSObject.unlock;
begin
  LeaveCriticalSection(FCritSect);
end;

end.




