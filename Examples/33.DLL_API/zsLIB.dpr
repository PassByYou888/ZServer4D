library zsLIB;

uses
  CoreClasses,
  PascalStrings,
  DoStatusIO,
  PhysicsIO,
  CommunicationFramework,
  CommunicationTest;

procedure DoStatus_BackCall(AText: SystemString; const ID: Integer);
begin
  Writeln(AText);
end;

procedure DLL_Init_Proc(); stdcall;
begin
  AddDoStatusHookC(nil, DoStatus_BackCall);
end;

procedure DLL_Exit_Proc(); stdcall;
begin
  DeleteDoStatusHook(nil);
end;

procedure DLL_ThreadSync_Proc(); stdcall;
begin
  CheckThreadSynchronize();
end;

procedure DLL_Demo_Proc(); stdcall;
var
  cli: TCommunicationFrameworkClient;
  test: TCommunicationTestIntf;
begin
  cli := TPhysicsClient.Create;
  test := TCommunicationTestIntf.Create;
  test.RegCmd(cli);
  if cli.Connect('127.0.0.1', 8191) then
    begin
      test.ExecuteTest(cli.ClientIO);
      test.ExecuteAsyncTest(cli.ClientIO);
      test.ExecuteAsyncTestWithBigStream(cli.ClientIO);
      cli.Wait(5000);
    end;
  disposeObject(cli);
  disposeObject(test);
end;

procedure DLL_DemoAsyncThread_Proc(); stdcall;
begin
  TComputeThread.RunP(nil, nil, procedure(Sender: TComputeThread)
    begin
      DoStatus('Thread async on dll');
    end,
    procedure(Sender: TComputeThread)
    begin
      DoStatus('Thread sync on dll');
    end);
end;

exports
  DLL_Init_Proc,
  DLL_Exit_Proc,
  DLL_ThreadSync_Proc,
  DLL_Demo_Proc,
  DLL_DemoAsyncThread_Proc;

begin

end.
