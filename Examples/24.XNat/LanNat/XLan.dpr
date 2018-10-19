program XLan;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,
  CoreClasses,
  PascalStrings,
  UnicodeMixedLib,
  CommunicationFramework,
  xNATClient,
  DoStatusIO;

var
  XCli: TXNATClient;

begin
  {
    TXNatClient能够部署到主工程，它只占用主工程10%以下的cpu使用
    假如有多个公网服务器需要穿透，创建多个TXNATClient即可
  }
  try
    XCli := TXNATClient.Create;
    XCli.RemoteTunnelAddr := '127.0.0.1'; // 公网服务器的IP
    XCli.RemoteTunnelPort := '7890';      // 公网服务器的端口号
    XCli.AuthToken := '123456';           // 协议验证字符串

    // 127.0.0.1是内网服务器的IP
    XCli.AddMapping('127.0.0.1', '80', 'web8000'); // 将公网服务器的8000端口反向代理到本地80端口

    XCli.OpenTunnel; // 启动内网穿透

    while True do
      begin
        XCli.Progress;
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
