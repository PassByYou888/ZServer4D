program XInternet;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,
  CoreClasses,
  PascalStrings,
  UnicodeMixedLib,
  CommunicationFramework,
  XNATPhysics,
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
      如果是裸数据协议，比如ftp,不带s的http,tennet，压缩开关可以打开，可以小幅提速

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
    // 在服务器端需要映射的端口8000，绑定地址为所有网卡的ipv4，因为挂载短连接的http，当连接空闲1分钟超时后会自动释放socket
    XServ.AddMapping('0.0.0.0', '8000', 'web8000', 60 * 1000);

    {
      在内网服务器未连接,临时断线,未请求mapping "ftp8021"，8021端口都是非侦听状态，只有当内网服务器全部正常工作,这个8021才会开始工作
    }
    // 在服务器端需要映射的端口8021，绑定地址为所有网卡的ipv4，因为挂载的是长连接的ftp，当连接空闲15分钟超时后会自动释放socket
    XServ.AddMapping('0.0.0.0', '8021', 'ftp8021', 15 * 60 * 1000);
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
 
