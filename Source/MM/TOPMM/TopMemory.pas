{****************************************************************************************

  TOPMEMORY v3.54 - HIGH PERFORMANCE DELPHI MEMORY MANAGER  (C) 2008 Ivo Tops, Topsoftware

  TopMemory unit installs both the manager and maintenance thread in your app

****************************************************************************************}
unit TopMemory;

// TopMemory should be the first unit listed in program uses clause in the project file. e.g. Project1.dpr:

{ program Project1;

uses
  TopMemory,
  Forms,
  Main in 'Main.pas',
  ..;
}


interface

uses    
  Windows,
  TopInstall,
  TopReporting,
  TopMaintenance;

implementation

initialization
  // Set leaklogging on exit default to true (you can change this here without problem)
  ReportMemoryLeaksOnShutdown := True;
  // Specify if you want the leaklogging appended to a file. Use TopReporing.SetMemoryLeaksLogFile to specify a different file from the default file (which is \apppath\appname_memoryleaks.txt)
  ReportMemoryLeaksToLogFile := False;
  // Specify if you want the leaklogging on exit passed to the attached debugger if any (to Delphi IDE Eventlog window!)
  ReportMemoryLeaksToIDE := True;
  // Copy the line below to your project file and enable it to use 4GB support (see TopMemory.doc for details)
  //{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}// Use Memory From 2GB-4GB is now Possible. 32 Bit windows versions need /3GB switch in Boot.ini to use this  

end.

