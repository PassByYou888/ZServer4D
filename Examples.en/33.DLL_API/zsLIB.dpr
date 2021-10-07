library zsLIB;

uses
  CoreClasses,
  PascalStrings,
  DoStatusIO,
  PhysicsIO,
  CommunicationFramework_Client_CrossSocket,
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
  {  When we are in freelibrary, any call using syncevent will get stuck  }
  {  Open debug and trace out two places where syncevent is used  }
  {  Find a place to use syncevent for synchronization, and we can slowly catch up in the finalization event  }

  {  The interface of crosssocket has a clientpool, which is released during finalization and will use syncevent  }
  DisposeObject(ClientPool);
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
  DisposeObject(cli);
  DisposeObject(test);
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
