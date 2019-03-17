program DLL_Test;

{$APPTYPE CONSOLE}


uses Windows;

procedure DLL_Init_Proc(); stdcall; external 'zsLib.DLL';
procedure DLL_Exit_Proc; stdcall; external 'zsLib.DLL';
procedure DLL_ThreadSync_Proc(); stdcall; external 'zsLib.DLL';
procedure DLL_Demo_Proc(); stdcall; external 'zsLib.DLL';
procedure DLL_DemoAsyncThread_Proc(); stdcall; external 'zsLib.DLL';

begin
  DLL_Init_Proc();

  DLL_Demo_Proc();

  DLL_DemoAsyncThread_Proc;
  Sleep(1000);
  DLL_ThreadSync_Proc;

  DLL_Exit_Proc();

  writeln('dll call over.');
  writeln('press return to exit.');
  readln;

end.
