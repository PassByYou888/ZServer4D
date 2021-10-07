program AsyncStateDemo;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  Variants,
  CoreClasses,
  DoStatusIO;

{  A simple demonstration of using a safe state machine in a thread  }
procedure StateDemo();
var
  State: TAtomBool;
begin
  {  Thread safety: state variables  }
  State := TAtomBool.Create(False);
  {  Worker thread  }
  TCompute.RunP_NP(procedure
    var
      i: Cardinal;
    begin
      for i := 0 to $FFFFFFFF do
          nop;
      DoStatus('Complete a cycle');
      {  Change state variable  }
      State.V := True;
    end);
  DoStatus('Wait for the cycle to complete');
  while not State.V do
    begin
      DoStatus();
      TCompute.Sleep(1);
    end;
  DisposeObject(State);
end;

{  Simple demonstration of using post mechanism in thread  }
{  Post is equivalent to sending code to a target thread we build for execution. Post does not select linear types, does not distinguish between main threads / sub threads, and can be used by any thread  }
procedure PostDemo();
var
  P: TThreadPost; {  Tthreadpost, post mechanism support  }
  done: TAtomBool;        {  Thread safety: state variables  }
  over: boolean;          {  The local variable will be automatically destroyed after the call is completed. Therefore, we must do a monitoring work here_ End state  }
begin
  {  Thread safety: state variables  }
  done := TAtomBool.Create(False);
  P := TThreadPost.Create(0);
  {  Worker thread  }
  TCompute.RunP_NP(procedure
    begin
      P.ThreadID := TCompute.CurrentThread.ThreadID;
      while not done.V do
        begin
          P.Progress(); {  Post main loop  }
          TCompute.Sleep(1);
        end;
      over := True;
    end);
  {  Send task to worker thread  }
  {  These tasks are performed in strict order of input  }
  P.PostP1(procedure
    begin
      DoStatus(1);
    end);
  P.PostP1(procedure
    begin
      DoStatus(2);
    end);
  TCompute.RunP_NP(procedure
    var
      i: Integer;
    begin
      for i := 3 to 20 do
        begin
          TCompute.Sleep(100);
          P.PostP3(nil, nil, i, procedure(Data1: Pointer; Data2: TCoreClassObject; Data3: Variant)
            begin
              DoStatus(VarToStr(Data3));
            end);
        end;
      {  End of worker thread  }
      P.PostP1(procedure
        begin
          done.V := True;
        end);
    end);
  {  After execution  }
  while not over do
    begin
      DoStatus();
      TCompute.Sleep(1);
    end;
  DisposeObjectAndNil(P);
  DisposeObjectAndNil(done);
end;

begin
  StateDemo();
  PostDemo();
  DoStatus('Press enter to end the program');
  readln;

end.
