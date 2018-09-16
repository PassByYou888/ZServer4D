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
    XServ.TunnelListenAddr := '0.0.0.0';            // 协议隧道绑定地址为所有网卡的ipv4，如果是ipv6，写'::'
    XServ.TunnelListenPort := '7890';               // 协议端口
    XServ.AuthToken := '123456';                    // 协议验证字符串
    XServ.AddMapping('0.0.0.0', '8000', 'web8000'); // 在服务器端需要映射的端口8000，绑定地址为所有网卡的ipv4
    XServ.OpenTunnel;

    while true do
        XServ.Progress;

  except
    on E: Exception do
        Writeln(E.ClassName, ': ', E.Message);
  end;

end.
