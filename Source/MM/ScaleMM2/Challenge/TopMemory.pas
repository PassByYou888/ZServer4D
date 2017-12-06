{****************************************************************************************

  TOPMEMORY v1.5 - HIGH PERFORMANCE DELPHI MEMORY MANAGER  (C) 2004 Ivo Tops, Topsoftware

  TopMemory unit installs both the manager and maintenance thread in your app

****************************************************************************************}
unit TopMemory;

// TopMemory should be the first unit listed in program uses clause:

{ program Project1;

uses
  TopMemory,
  Forms,
  Main in 'Main.pas',
  ..;
}


interface

uses TopInstall, // TopInstall has to be first
     TopMaintenance;

implementation

end.
 