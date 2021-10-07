unit VirtualServFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls,
  CoreClasses, PascalStrings, UnicodeMixedLib, CommunicationFramework,
  XNATMappingOnVirutalService, XNATPhysics, CommunicationTest, DoStatusIO, NotifyObjectBase,
  FMX.Memo.Types;

type
  TForm2 = class(TForm)
    Memo1: TMemo;
    Timer1: TTimer;
    TestButton: TButton;
    OpenButton: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure OpenButtonClick(Sender: TObject);
    procedure TestButtonClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    XCli: TXNAT_VS_Mapping;
    server: TCommunicationFrameworkServer;
    server_test: TCommunicationTestIntf;

    {  Simulator test client  }
    {  We can open two apps for simulation testing, which is convenient for direct built-in implementation  }
    client: TCommunicationFrameworkClient;
    client_test: TCommunicationTestIntf;
    procedure DoStatusIntf(AText: SystemString; const ID: Integer);
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}


procedure TForm2.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(Self, DoStatusIntf);

  XCli := TXNAT_VS_Mapping.Create;

  {  Penetration protocol compression options
Recommended usage scenarios:
If the agent's data has been compressed or encrypted using HTTPS, the compression will be invalid, and even the compressed data will be larger
If it is a bare data protocol, such as FTP, HTTP without s, TenneT, the compression switch can be turned on and the speed can be increased slightly  }
  XCli.ProtocolCompressed := True;

  XCli.Host := '127.0.0.1';       {  IP address of public network server  }
  XCli.Port := '7890';                  {  Port number of public network server  }
  XCli.AuthToken := '123456';                       {  Protocol Validation string  }
  server := XCli.AddMappingServer('web8000', 1000); {  Reverse proxy port 8000 of the public network server to become the local server  }

  server_test := TCommunicationTestIntf.Create;
  server_test.RegCmd(server);

  client := TXPhysicsClient.Create;
  client_test := TCommunicationTestIntf.Create;
  client_test.RegCmd(client);
end;

procedure TForm2.DoStatusIntf(AText: SystemString; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
  Memo1.GoToTextEnd;
end;

procedure TForm2.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  DisposeObject(XCli);
end;

procedure TForm2.OpenButtonClick(Sender: TObject);
begin
  {  Start intranet penetration  }
  {  After the intranet penetration server is started, the local server will automatically start service, and the local server will not listen on any port  }
  XCli.OpenTunnel;
end;

procedure TForm2.TestButtonClick(Sender: TObject);
begin
  {  Simulation test: connect to public network server  }

  {  This is a built-in client accessing the built-in server. The client has a blocking mechanism. Pay attention to the dead loop  }
  {  The method of avoiding dead loop directly uses asynchronous mode  }
  client.AsyncConnectP('127.0.0.1', 8000, procedure(const cState: Boolean)
    begin
      if cState then
        begin
          client_test.ExecuteAsyncTestWithBigStream(client.ClientIO);
        end;
    end);
end;

procedure TForm2.Timer1Timer(Sender: TObject);
begin
  if XCli <> nil then
    begin
      XCli.Progress;
      client.Progress;
    end;
end;

end.
 
