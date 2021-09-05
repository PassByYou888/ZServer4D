unit DT_P2PVM_CustomServerFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,

  System.IOUtils,

  CoreClasses, PascalStrings, DoStatusIO, DataFrameEngine,
  CommunicationFramework, PhysicsIO,
  CommunicationFrameworkDoubleTunnelIO,
  CommunicationFrameworkDoubleTunnelIO_NoAuth;

type
  TDT_P2PVM_CustomServerForm = class(TForm)
    Memo: TMemo;
    startservButton: TButton;
    netTimer: TTimer;
    stopservButton: TButton;
    UserEdit: TLabeledEdit;
    PasswdEdit: TLabeledEdit;
    regUsrButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure netTimerTimer(Sender: TObject);
    procedure regUsrButtonClick(Sender: TObject);
    procedure startservButtonClick(Sender: TObject);
    procedure stopservButtonClick(Sender: TObject);
  private
    procedure DoStatus_backcall(Text_: SystemString; const ID: Integer);
  public
  end;

var
  DT_P2PVM_CustomServerForm: TDT_P2PVM_CustomServerForm;
  PhysicsTunnel: TPhysicsServer;
  AuthServ: TDT_P2PVM_Custom_Service;
  NoAuthServ1: TDT_P2PVM_NoAuth_Custom_Service;
  NoAuthServ2: TDT_P2PVM_NoAuth_Custom_Service;

implementation

{$R *.dfm}


procedure TDT_P2PVM_CustomServerForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatus_backcall);

  PhysicsTunnel := TPhysicsServer.Create;

  AuthServ := TDT_P2PVM_Custom_Service.Create(TDTService, PhysicsTunnel, 'AuthR', '::', '100', 'AuthS', '::', '101');
  AuthServ.QuietMode := False;
  AuthServ.DTService.RootPath := TPath.GetLibraryPath;
  AuthServ.DTService.PublicPath := TPath.GetLibraryPath;
  AuthServ.DTService.LoadUserDB;
  AuthServ.DTService.AllowRegisterNewUser := False;
  AuthServ.DTService.AllowSaveUserInfo := True;

  NoAuthServ1 := TDT_P2PVM_NoAuth_Custom_Service.Create(TDTService_NoAuth, PhysicsTunnel, 'NAR1', '::', '200', 'NAS1', '::', '201');
  NoAuthServ1.QuietMode := False;

  NoAuthServ2 := TDT_P2PVM_NoAuth_Custom_Service.Create(TDTService_NoAuth, PhysicsTunnel, 'NAR2', '::', '300', 'NAS2', '::', '301');
  NoAuthServ2.QuietMode := False;
end;

procedure TDT_P2PVM_CustomServerForm.FormDestroy(Sender: TObject);
begin
  DeleteDoStatusHook(self);
end;

procedure TDT_P2PVM_CustomServerForm.netTimerTimer(Sender: TObject);
begin
  CheckThread;
  AuthServ.Progress;
  NoAuthServ1.Progress;
  NoAuthServ2.Progress;
end;

procedure TDT_P2PVM_CustomServerForm.startservButtonClick(Sender: TObject);
begin
  AuthServ.StartService();
  NoAuthServ1.StartService;
  NoAuthServ2.StartService;
  if PhysicsTunnel.StartService('0.0.0.0', 11938) then
      DoStatus('侦听物理IP %s:%d 成功!', [TranslateBindAddr('0.0.0.0'), 11938]);
end;

procedure TDT_P2PVM_CustomServerForm.stopservButtonClick(Sender: TObject);
begin
  AuthServ.StopService();
  NoAuthServ1.StopService;
  NoAuthServ2.StopService;
  PhysicsTunnel.StopService;
end;

procedure TDT_P2PVM_CustomServerForm.DoStatus_backcall(Text_: SystemString; const ID: Integer);
begin
  Memo.Lines.Add(Text_);
end;

procedure TDT_P2PVM_CustomServerForm.regUsrButtonClick(Sender: TObject);
begin
  if AuthServ.DTService.RegUser(UserEdit.Text, PasswdEdit.Text, nil) then
      DoStatus('register %s successed.', [UserEdit.Text])
  else
      DoStatus('register %s failed!', [UserEdit.Text]);
end;

end.
