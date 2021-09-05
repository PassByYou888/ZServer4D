unit DT_P2PVM_VirtualAuth_CustomServerFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,

  CoreClasses, PascalStrings, DoStatusIO, DataFrameEngine,
  CommunicationFramework, PhysicsIO,
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
  PhysicsTunnel: TPhysicsServer;
  Serv1: TDT_P2PVM_VirtualAuth_Custom_Service;
  Serv2: TDT_P2PVM_VirtualAuth_Custom_Service;
  Serv3: TDT_P2PVM_VirtualAuth_Custom_Service;

implementation

{$R *.dfm}

procedure TMyVirtualAuth_Service.UserAuth(Sender: TVirtualAuthIO);
begin
  // 不验证身份,接受一切用户登录
  Sender.Accept;
end;

procedure TMyVirtualAuth_Service.UserLinkSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth);
begin
  inherited UserLinkSuccess(UserDefineIO);
  DoStatus('用户 %s 登录成功', [UserDefineIO.UserID]);
end;

procedure TMyVirtualAuth_Service.UserOut(UserDefineIO: TPeerClientUserDefineForRecvTunnel_VirtualAuth);
begin
  inherited UserOut(UserDefineIO);
  DoStatus('用户 %s 离线', [UserDefineIO.UserID]);
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

  PhysicsTunnel := TPhysicsServer.Create;
  Serv1 := TDT_P2PVM_VirtualAuth_Custom_Service.Create(TMyVirtualAuth_Service, PhysicsTunnel, 'R1', '::', '99', 'S1', '::', '100');
  Serv2 := TDT_P2PVM_VirtualAuth_Custom_Service.Create(TMyVirtualAuth_Service, PhysicsTunnel, 'R2', '::', '199', 'S2', '::', '200');
  Serv3 := TDT_P2PVM_VirtualAuth_Custom_Service.Create(TMyVirtualAuth_Service, PhysicsTunnel, 'R3', '::', '299', 'S3', '::', '300');

  Serv1.QuietMode := False;
  Serv2.QuietMode := False;
  Serv3.QuietMode := False;
end;

procedure TDT_P2PVM_VirtualAuth_ServerForm.FormDestroy(Sender: TObject);
begin
  DeleteDoStatusHook(self);
end;

procedure TDT_P2PVM_VirtualAuth_ServerForm.netTimerTimer(Sender: TObject);
begin
  CheckThread;
  Serv1.Progress;
  Serv2.Progress;
  Serv3.Progress;
end;

procedure TDT_P2PVM_VirtualAuth_ServerForm.startservButtonClick(Sender: TObject);
begin
  if PhysicsTunnel.StartService('0.0.0.0', 11938) then
    begin
      Serv1.StartService;
      Serv2.StartService;
      Serv3.StartService;
      DoStatus('侦听成功');
    end;
end;

procedure TDT_P2PVM_VirtualAuth_ServerForm.stopservButtonClick(Sender: TObject);
begin
  Serv1.StopService;
  Serv2.StopService;
  Serv3.StopService;
  PhysicsTunnel.StopService;
end;

procedure TDT_P2PVM_VirtualAuth_ServerForm.DoStatus_backcall(Text_: SystemString; const ID: Integer);
begin
  Memo.Lines.Add(Text_);
end;

end.
