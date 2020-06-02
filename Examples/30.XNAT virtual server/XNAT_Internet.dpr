program XNAT_Internet;

{$APPTYPE CONSOLE}

{$R *.res}

{
  XNAT的公网服务器，用于公网协议接口
  XNAT的MobileServer是工作于手机或则IOT设备的服务器模型
  在公网协议接口中，XNAT使用了P2PVM技术，演示了如何让手机带起大量物理连接的服务

  XNAT的物理并发连接上限是4000
}

uses
  SysUtils,
  CoreClasses,
  PascalStrings,
  UnicodeMixedLib,
  CommunicationFramework,
  XNATService,
  DoStatusIO;

var
  XServ: TXNATService;

begin
  try
    XServ := TXNATService.Create;
    {
      穿透协议压缩选项
      建议使用场景:
      如果代理的数据已经压缩过，或则使用https这类方式加密过，压缩会无效，甚至压缩后数据更大
      如果时裸数据协议，比如ftp,不带s的http,tennet，压缩开关可以打开，可以小幅提速

      性能优化思路，ZLib的压缩算法特征是压缩慢，解压非常快，让服务器发送数据时不做压缩，让客户端发送数据全部压缩
      在TXServiceListen实例中调整 SendTunnel.CompleteBufferCompressed:=False;
      在TXClientMapping实例中调整 SendTunnel.CompleteBufferCompressed:=True;
      在TXNAT_MappingOnVirutalServer实例中调整 SendTunnel.CompleteBufferCompressed:=True;
    }
    XServ.ProtocolCompressed := True;

    XServ.Host := '0.0.0.0';     // 与内网服务器的通讯参数：协议隧道绑定地址为所有网卡的ipv4，如果是ipv6，写'::'
    XServ.Port := '7890';        // 与内网服务器的通讯参数：协议端口
    XServ.AuthToken := '123456'; // 与内网服务器的通讯参数：协议验证字符串(该标识符使用了抗量子密码模型，相关技术请自行研究代码)

    {
      侦听配置
    }
    // 在服务器端需要映射的端口8000，绑定地址为所有网卡的ipv4，因为挂载长连接的自定义服务，当连接空闲10分钟超时后会自动释放socket
    XServ.AddMapping('0.0.0.0', '18888', 'my18888', 10 * 60 * 1000);

    XServ.OpenTunnel;

    while True do
      begin
        XServ.Progress;
        try
            CoreClasses.CheckThreadSynchronize(1);
        except
        end;
      end;

  except
    on E: Exception do
        Writeln(E.ClassName, ': ', E.Message);
  end;

end.
 
