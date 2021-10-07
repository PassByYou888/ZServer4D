program automatedP2PVM_HPC_Cli;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  CoreClasses,
  PascalStrings,
  UnicodeMixedLib,
  DoStatusIO,
  DataFrameEngine,
  CommunicationFramework,
  PhysicsIO;

{  Automated p2pvm is a very simplified p2pvm application paradigm, which has the ability to drive the p2pvm framework with very short code  }
{  Automated P2P VM is also the communication foundation of ECS 3.0  }
procedure RunAutomatedP2PVM_Client;
var
  phyCli: TPhysicsClient;
  vm_cli1, vm_cli2, vm_cli3, vm_cli4: TCommunicationFrameworkWithP2PVM_Client; {  The communication tunnel driven by p2pvm can be set with various similar dual channel, database and file transmission applications. See the relevant demo of VM  }
begin
  vm_cli1 := TCommunicationFrameworkWithP2PVM_Client.Create;
  vm_cli2 := TCommunicationFrameworkWithP2PVM_Client.Create;
  vm_cli3 := TCommunicationFrameworkWithP2PVM_Client.Create;
  vm_cli4 := TCommunicationFrameworkWithP2PVM_Client.Create;

  phyCli := TPhysicsClient.Create;
  phyCli.AutomatedP2PVMClientBind.AddClient(vm_cli1, '::', 99);
  phyCli.AutomatedP2PVMClientBind.AddClient(vm_cli2, '::', 199);
  phyCli.AutomatedP2PVMClientBind.AddClient(vm_cli3, '::', 299);
  phyCli.AutomatedP2PVMClientBind.AddClient(vm_cli4, '88::', 399); {  88:: equivalent to 88:0:0:0:0: 0 is the virtual IPv6 in p2pvm, not the real physical address. This address needs to be matched with the p2pvm service listening address  }
  phyCli.AutomatedP2PVMClient := True;
  phyCli.AutomatedP2PVMAuthToken := '123456';
  phyCli.Connect('127.0.0.1', 9799);

  phyCli.QuietMode := True;
  vm_cli1.QuietMode := True;
  vm_cli2.QuietMode := True;
  vm_cli3.QuietMode := True;
  vm_cli4.QuietMode := True;

  {  Here is a demonstration of how HPC can load large-scale computing  }
  {  The demo client of the HPC can be opened more, and the better the server CPU, the better the computing power  }
  phyCli.OnAutomatedP2PVMClientConnectionDone_P := procedure(Sender: TCommunicationFramework; P_IO: TPeerIO)
    var
      de, tmp: TDataFrameEngine;
    begin
      if phyCli.AutomatedP2PVMClientConnectionDone(phyCli.ClientIO) then
        begin
          phyCli.Print('All virtual tunnels have been connected');
          de := TDataFrameEngine.Create;
          de.WriteInteger(100 * 10000);
          de.WriteString('1+1=2');

          {  Sending feedback in blocking mode  }
          tmp := TDFE.Create;
          vm_cli4.WaitSendStreamCmd('runExp', de, tmp, 5000);
          DoStatus('It took %d milliseconds to execute the expression 1 million times', [tmp.Reader.ReadUInt64]);
          disposeObject(tmp);

          {  Send feedback asynchronously  }
          vm_cli4.SendStreamCmdP('runExp', de, procedure(Sender: TPeerIO; ResultData: TDataFrameEngine)
            begin
              DoStatus('It took %d milliseconds to execute the expression 1 million times', [ResultData.Reader.ReadUInt64]);
              {  Loop call  }
              TCompute.PostP1(procedure
                begin
                  phyCli.OnAutomatedP2PVMClientConnectionDone_P(phyCli, phyCli.ClientIO);
                end);
            end);
          disposeObject(de);
        end;
    end;

  while True do
    begin
      phyCli.Progress;
      DoStatus();
      CheckThreadSynchronize(1);
    end;

  phyCli.Disconnect;
  disposeObject(vm_cli1);
  disposeObject(vm_cli2);
  disposeObject(vm_cli3);
  disposeObject(phyCli);
end;

begin
  RunAutomatedP2PVM_Client();

end.
