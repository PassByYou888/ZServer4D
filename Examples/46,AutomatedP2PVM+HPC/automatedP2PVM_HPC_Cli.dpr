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

// AutomatedP2PVM是一种极简化的p2pVM应用范式,它有能力用极短代码驱动p2pVM框架
// AutomatedP2PVM也是云服务器3.0的通讯地基
procedure RunAutomatedP2PVM_Client;
var
  phyCli: TPhysicsClient;
  vm_cli1, vm_cli2, vm_cli3, vm_cli4: TCommunicationFrameworkWithP2PVM_Client; // 由p2pVM驱动的通讯隧道,可以在上面套各种类似双通道,数据库,文件传输应用,见VM的相关demo
begin
  vm_cli1 := TCommunicationFrameworkWithP2PVM_Client.Create;
  vm_cli2 := TCommunicationFrameworkWithP2PVM_Client.Create;
  vm_cli3 := TCommunicationFrameworkWithP2PVM_Client.Create;
  vm_cli4 := TCommunicationFrameworkWithP2PVM_Client.Create;

  phyCli := TPhysicsClient.Create;
  phyCli.AutomatedP2PVMClientBind.AddClient(vm_cli1, '::', 99);
  phyCli.AutomatedP2PVMClientBind.AddClient(vm_cli2, '::', 199);
  phyCli.AutomatedP2PVMClientBind.AddClient(vm_cli3, '::', 299);
  phyCli.AutomatedP2PVMClientBind.AddClient(vm_cli4, '88::', 399); // 88::等同于88:0:0:0:0:0 是p2pVM中的虚拟Ipv6,不是真实物理地址,该地址需要与p2pVM服务侦听地址对号
  phyCli.AutomatedP2PVMClient := True;
  phyCli.AutomatedP2PVMAuthToken := '123456';
  phyCli.Connect('127.0.0.1', 9799);

  phyCli.QuietMode := True;
  vm_cli1.QuietMode := True;
  vm_cli2.QuietMode := True;
  vm_cli3.QuietMode := True;
  vm_cli4.QuietMode := True;

  // 这里演示了hpc怎样负载大规模计算
  // 该hpc的demo客户端可以多开,服务器cpu越好,计算能力越好
  phyCli.OnAutomatedP2PVMClientConnectionDone_P := procedure(Sender: TCommunicationFramework; P_IO: TPeerIO)
    var
      de: TDataFrameEngine;
    begin
      if phyCli.AutomatedP2PVMClientConnectionDone(phyCli.ClientIO) then
        begin
          phyCli.Print('所有虚拟隧道已连接完成');
          de := TDataFrameEngine.Create;
          de.WriteInteger(100 * 10000);
          de.WriteString('1+1=2');
          vm_cli4.SendStreamCmdP('runExp', de, procedure(Sender: TPeerIO; ResultData: TDataFrameEngine)
            begin
              DoStatus('执行100万次表达式耗时 %d 毫秒', [ResultData.Reader.ReadUInt64]);
              // 循环调用
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
