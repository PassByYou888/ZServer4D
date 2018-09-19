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
  try
    XCli := TXNATClient.Create;
    XCli.RemoteTunnelAddr := '127.0.0.1';          // 公网服务器的IP
    XCli.RemoteTunnelPort := '7890';               // 公网服务器的端口号
    XCli.AuthToken := '123456';                    // 协议验证字符串
    XCli.AddMapping('127.0.0.1', '80', 'web8000'); // 将公网服务器的8000端口反向代理到本地80端口
    XCli.OpenTunnel;                               // 启动内网穿透
    while True do
      begin
        XCli.Progress;
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
