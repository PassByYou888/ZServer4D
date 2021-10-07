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
    {  Penetration protocol compression options
Recommended usage scenarios:
If the agent's data has been compressed or encrypted using HTTPS, the compression will be invalid, and even the compressed data will be larger
If it is a bare data protocol, such as FTP, HTTP without s, TenneT, the compression switch can be turned on and the speed can be increased slightly
Performance optimization idea: zlib's compression algorithm is characterized by slow compression and very fast decompression. The server does not compress the data when sending it, and the client compresses all the data sent
Adjust SendTunnel.CompleteBufferCompressed:=False in TXServiceListen instance;
Adjust SendTunnel.CompleteBufferCompressed:=True in TXClientMapping instance;
In txnat_ SendTunnel.CompleteBufferCompressed:=True is adjusted in MappingOnVirutalServer instance.  }
    XServ.ProtocolCompressed := True;

    XServ.Host := '0.0.0.0';     {  Communication parameters with intranet server: the protocol tunnel binding address is IPv4 of all network cards. If it is IPv6, write '::'  }
    XServ.Port := '7890';        {  Communication parameters with intranet server: protocol port  }
    XServ.AuthToken := '123456'; {  Communication parameters with intranet server: protocol verification string (the identifier uses anti quantum cryptography model, please study the code by yourself for related technologies)  }

    {  Listening configuration  }
    {  Port 8000 needs to be mapped on the server side, and the binding address is IPv4 of all network cards. Because the HTTP of the short connection is attached, the socket will be automatically released when the connection is idle for 1 minute and times out  }
    XServ.AddMapping('0.0.0.0', '8000', 'web8000', 60 * 1000);

    {  When the intranet server is not connected, temporarily disconnected, and mapping "ftp8021" is not requested, the 8021 port is in a non listening state. This 8021 will start to work only when all the intranet servers are working normally  }
    {  On the server side, the port 8021 to be mapped is bound to the IPv4 address of all network cards. Because the FTP with a long connection is mounted, the socket will be automatically released when the connection is idle for 15 minutes and times out  }
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
 
