program XInternet;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,
  CoreClasses,
  PascalStrings,
  UnicodeMixedLib,
  CommunicationFramework,
  xNATService,
  DoStatusIO;

var
  XServ: TXNATService;

begin
  try
    XServ := TXNATService.Create;

    XServ.TunnelListenAddr := '0.0.0.0'; // 与内网服务器的通讯参数：协议隧道绑定地址为所有网卡的ipv4，如果是ipv6，写'::'
    XServ.TunnelListenPort := '7890';    // 与内网服务器的通讯参数：协议端口
    XServ.AuthToken := '123456';         // 与内网服务器的通讯参数：协议验证字符串(该标识符使用了抗量子密码模型，相关技术请自行研究代码)

    {
      侦听配置
    }
    XServ.AddMapping('0.0.0.0', '8000', 'web8000'); // 在服务器端需要映射的端口8000，绑定地址为所有网卡的ipv4

    {
      在内网服务器未连接,临时断线,未请求mapping "ftp8021"，8021端口都是非侦听状态，只有当内网服务器全部正常工作,这个8021才会开始工作
    }
    XServ.AddMapping('0.0.0.0', '8021', 'ftp8021'); // 在服务器端需要映射的端口8021，绑定地址为所有网卡的ipv4
    XServ.OpenTunnel;

    while true do
      begin
        XServ.Progress;
        try
            CoreClasses.CheckThreadSynchronize;
        except
        end;
      end;

  except
    on E: Exception do
        Writeln(E.ClassName, ': ', E.Message);
  end;

end.
