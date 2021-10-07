program automatedP2PVM_HPC_Serv;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  CoreClasses,
  PascalStrings,
  UnicodeMixedLib,
  DoStatusIO,
  CommunicationFramework,
  PhysicsIO,
  DataFrameEngine,
  TextParsing,
  zExpression,
  opCode;

{  Here is a demonstration of how HPC can load large-scale computing  }
procedure cmd_runExp(Sender: TPeerIO; InData, OutData: TDataFrameEngine);
begin
  {  The NOP function is in the main thread section  }
  nop;
  {  RunHPC_ When streamp is called, IO pauses feedback and immediately sends the request to the child thread for execution  }
  {  RunHPC_ Streamp is scheduled by HPC's dedicated kernel thread pool and is safely scheduled, not Delphi / FPC's built-in thread pool  }
  RunHPC_StreamP(Sender, nil, nil, InData, OutData, procedure(ThSender: THPC_Stream; ThInData, ThOutData: TDataFrameEngine)
    var
      num: Integer;
      exp: U_String;
      i: Integer;
      tk: TTimeTick;
      op: TOpCode;
    begin
      {  The following code is executed in the thread section  }
      tk := GetTimeTick;
      num := ThInData.Reader.ReadInteger;
      exp := ThInData.Reader.ReadString;
      op := BuildAsOpCode(tsPascal, exp);
      for i := 0 to num - 1 do
          op.Execute();
      {  Dropping data into thoutdata means that feedback is required. Here is the expression time of the number of num feedback execution  }
      ThOutData.WriteUInt64(GetTimeTick - tk);
    end); {  After this step is completed, IO will resume feedback  }
end;

{  Automated p2pvm is a very simplified p2pvm application paradigm, which has the ability to drive the p2pvm framework with very short code  }
{  Automated P2P VM is also the communication foundation of ECS 3.0  }
procedure RunServ;
var
  phyServ: TPhysicsServer;
  vm_serv1, vm_serv2, vm_serv3, vm_serv4: TCommunicationFrameworkWithP2PVM_Server; {  The communication tunnel driven by p2pvm can be set with various similar dual channel, database and file transmission applications. See the relevant demo of VM  }
begin
  vm_serv1 := TCommunicationFrameworkWithP2PVM_Server.Create;
  vm_serv2 := TCommunicationFrameworkWithP2PVM_Server.Create;
  vm_serv3 := TCommunicationFrameworkWithP2PVM_Server.Create;
  vm_serv4 := TCommunicationFrameworkWithP2PVM_Server.Create;

  phyServ := TPhysicsServer.Create;
  phyServ.AutomatedP2PVMServiceBind.AddService(vm_serv1, '::', 99);    {  : equivalent to 0:0:0:0:0:0 is a virtual IPv6 in p2pvm, not a real physical address. This address can be given arbitrarily  }
  phyServ.AutomatedP2PVMServiceBind.AddService(vm_serv2, '::', 199);   {  : equivalent to 0:0:0:0:0:0 is a virtual IPv6 in p2pvm, not a real physical address. This address can be given arbitrarily  }
  phyServ.AutomatedP2PVMServiceBind.AddService(vm_serv3, '::', 299);   {  : equivalent to 0:0:0:0:0:0 is a virtual IPv6 in p2pvm, not a real physical address. This address can be given arbitrarily  }
  phyServ.AutomatedP2PVMServiceBind.AddService(vm_serv4, '88::', 399); {  88:: equivalent to 88:0:0:0:0: 0 is a virtual IPv6 in p2pvm, not a real physical address. This address is given arbitrarily  }
  phyServ.AutomatedP2PVMService := True;
  phyServ.AutomatedP2PVMAuthToken := '123456';
  vm_serv4.RegisterStream('runExp').OnExecuteCall := cmd_runExp; {  Registration instruction  }
  phyServ.StartService('', 9799);

  while True do
    begin
      phyServ.Progress;
      DoStatus();
      CheckThreadSynchronize(1);
    end;
end;

begin
  RunServ;

end.
