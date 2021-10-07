program XNAT_Internet;

{$APPTYPE CONSOLE}

{$R *.res}

{  The public network server of xnat is used for the public network protocol interface
The mobile server of xnat is a server model that works on mobile phones or IOT devices
In the public network protocol interface, xnat uses P2P VM technology to demonstrate how to make mobile phones carry a large number of physically connected services
The maximum physical concurrent connection limit for xnat is 4000  }

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
    {  Penetration protocol compression options
Recommended usage scenarios:
If the agent's data has been compressed or encrypted using HTTPS, the compression will be invalid, and even the compressed data will be larger
If the time raw data protocol, such as FTP, HTTP without s, TenneT, the compression switch can be turned on and the speed can be increased slightly
Performance optimization idea: zlib's compression algorithm is characterized by slow compression and very fast decompression. The server does not compress the data when sending it, and the client compresses all the data sent
Adjust SendTunnel.CompleteBufferCompressed:=False in TXServiceListen instance;
Adjust SendTunnel.CompleteBufferCompressed:=True in TXClientMapping instance;
In txnat_ SendTunnel.CompleteBufferCompressed:=True is adjusted in MappingOnVirutalServer instance.  }
    XServ.ProtocolCompressed := True;

    XServ.Host := '0.0.0.0';     {  Communication parameters with intranet server: the protocol tunnel binding address is IPv4 of all network cards. If it is IPv6, write '::'  }
    XServ.Port := '7890';        {  Communication parameters with intranet server: protocol port  }
    XServ.AuthToken := '123456'; {  Communication parameters with intranet server: protocol verification string (the identifier uses anti quantum cryptography model, please study the code by yourself for related technologies)  }

    {  Listening configuration  }
    {  Port 8000 needs to be mapped on the server side, and the binding address is IPv4 of all network cards. Because the user-defined service of long connection is mounted, the socket will be automatically released when the connection is idle for 10 minutes and times out  }
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
 
