unit DT_P2PVM_VirtualAuth_ServerFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,

  CoreClasses, PascalStrings, DoStatusIO, DataFrameEngine,
  CommunicationFramework,
  CommunicationFrameworkDoubleTunnelIO_VirtualAuth;

type
  TDT_P2PVM_VirtualAuth_ServerForm = class(TForm)
    Memo: TMemo;
    startservButton: TButton;
    netTimer: TTimer;
    stopservButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure netTimerTimer(Sender: TObject);
    procedure startservButtonClick(Sender: TObject);
    procedure stopservButtonClick(Sender: TObject);
  private
    procedure DoStatus_backcall(Text_: SystemString; const ID: Integer);
  public
  end;

  TMyVirtualAuth_Service = class(TDTService_VirtualAuth)
  protected
    procedure UserAuth(Sender: TVirtualAuthIO); override;
    procedure UserLinkSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth); override;
    procedure UserOut(UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth); override;
  public
    constructor Create(RecvTunnel_, SendTunnel_: TCommunicationFrameworkServer); override;
    destructor Destroy; override;
  end;

var
  DT_P2PVM_VirtualAuth_ServerForm: TDT_P2PVM_VirtualAuth_ServerForm;
  Serv: TDT_P2PVM_VirtualAuth_Service;

implementation

{$R *.dfm}


procedure TMyVirtualAuth_Service.UserAuth(Sender: TVirtualAuthIO);
begin
  {  Do not authenticate, accept all user login  }
  Sender.Accept;
end;

procedure TMyVirtualAuth_Service.UserLinkSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth);
begin
  inherited UserLinkSuccess(UserDefineIO);
  DoStatus('User %s logged in successfully', [UserDefineIO.UserID]);
end;

procedure TMyVirtualAuth_Service.UserOut(UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth);
begin
  inherited UserOut(UserDefineIO);
  DoStatus('User %s is offline', [UserDefineIO.UserID]);
end;

constructor TMyVirtualAuth_Service.Create(RecvTunnel_, SendTunnel_: TCommunicationFrameworkServer);
begin
  inherited Create(RecvTunnel_, SendTunnel_);
end;

destructor TMyVirtualAuth_Service.Destroy;
begin
  inherited Destroy;
end;

procedure TDT_P2PVM_VirtualAuth_ServerForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatus_backcall);
  Serv := TDT_P2PVM_VirtualAuth_Service.Create(TMyVirtualAuth_Service);
  startservButtonClick(startservButton);
end;

procedure TDT_P2PVM_VirtualAuth_ServerForm.FormDestroy(Sender: TObject);
begin
  DeleteDoStatusHook(self);
end;

procedure TDT_P2PVM_VirtualAuth_ServerForm.netTimerTimer(Sender: TObject);
begin
  CheckThread;
  Serv.Progress;
end;

procedure TDT_P2PVM_VirtualAuth_ServerForm.startservButtonClick(Sender: TObject);
begin
  Serv.StartService('0.0.0.0', '11938', '123456');
end;

procedure TDT_P2PVM_VirtualAuth_ServerForm.stopservButtonClick(Sender: TObject);
begin
  Serv.StopService;
end;

procedure TDT_P2PVM_VirtualAuth_ServerForm.DoStatus_backcall(Text_: SystemString; const ID: Integer);
begin
  Memo.Lines.Add(Text_);
end;

end.
