program DLL_Test;

{$APPTYPE CONSOLE}


uses Windows, Classes;

procedure Test2;
var
  hnd: HMODULE;

  DLL_Init_Proc: procedure(); stdcall;
  DLL_Exit_Proc: procedure(); stdcall;
  DLL_ThreadSync_Proc: procedure(); stdcall;
  DLL_Demo_Proc: procedure(); stdcall;
  DLL_DemoAsyncThread_Proc: procedure(); stdcall;

begin
  hnd := LoadLibrary('zsLib.DLL');

  DLL_Init_Proc := GetProcAddress(hnd, 'DLL_Init_Proc');
  DLL_Exit_Proc := GetProcAddress(hnd, 'DLL_Exit_Proc');
  DLL_ThreadSync_Proc := GetProcAddress(hnd, 'DLL_ThreadSync_Proc');
  DLL_Demo_Proc := GetProcAddress(hnd, 'DLL_Demo_Proc');
  DLL_DemoAsyncThread_Proc := GetProcAddress(hnd, 'DLL_DemoAsyncThread_Proc');

  DLL_Init_Proc();

  DLL_Demo_Proc();

  DLL_DemoAsyncThread_Proc;
  Sleep(1000);
  DLL_ThreadSync_Proc;

  DLL_Exit_Proc();

  writeln('dll call over.');
  writeln('press return to exit.');
  readln;

  freeLibrary(hnd);
end;

begin
  Test2;

end.
