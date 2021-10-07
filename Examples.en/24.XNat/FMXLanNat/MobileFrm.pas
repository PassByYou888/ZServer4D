unit MobileFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  CoreClasses, PascalStrings, UnicodeMixedLib, CommunicationFramework,
  XNATPhysics, XNATClient, DoStatusIO;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Timer1: TTimer;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    XCli: TXNATClient;
    procedure DoStatusIntf(AText: SystemString; const ID: Integer);
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}


procedure TForm1.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(Self, DoStatusIntf);

  XCli := TXNATClient.Create;

  {  Penetration protocol compression options
Recommended usage scenarios:
If the agent's data has been compressed or encrypted using HTTPS, the compression will be invalid, and even the compressed data will be larger
If it is a bare data protocol, such as FTP, HTTP without s, TenneT, the compression switch can be turned on and the speed can be increased slightly  }
  XCli.ProtocolCompressed := True;

  XCli.Host := '127.0.0.1';                           {  IP address of public network server  }
  XCli.Port := '7890';                                {  Port number of public network server  }
  XCli.AuthToken := '123456';                         {  Protocol Validation string  }
  XCli.AddMapping('127.0.0.1', '80', 'web8000', 100); {  Reverse proxy port 8000 of the public server to port 80 of the local server  }
  XCli.OpenTunnel;                                    {  Start intranet penetration  }
end;

procedure TForm1.DoStatusIntf(AText: SystemString; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
  Memo1.GoToLineEnd;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  DisposeObject(XCli);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if XCli <> nil then
      XCli.Progress;
end;

end.
 
