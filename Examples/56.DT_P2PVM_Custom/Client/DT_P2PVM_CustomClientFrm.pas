unit DT_P2PVM_CustomClientFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,

  CoreClasses, PascalStrings, DoStatusIO, DataFrameEngine,
  CommunicationFramework, PhysicsIO,
  CommunicationFrameworkDoubleTunnelIO,
  CommunicationFrameworkDoubleTunnelIO_NoAuth;

type
  TDT_P2PVM_CustomClientForm = class(TForm)
    Memo: TMemo;
    netTimer: TTimer;
    connButton: TButton;
    disButton: TButton;
    UserEdit: TLabeledEdit;
    PasswdEdit: TLabeledEdit;
    procedure connButtonClick(Sender: TObject);
    procedure disButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure netTimerTimer(Sender: TObject);
  private
    procedure DoStatus_backcall(Text_: SystemString; const ID: Integer);
  public
  end;

var
  DT_P2PVM_CustomClientForm: TDT_P2PVM_CustomClientForm;
  PhysicsTunnel: TPhysicsClient;
  AuthCli: TDT_P2PVM_Custom_Client;
  NoAuthCli1: TDT_P2PVM_NoAuth_Custom_Client;
  NoAuthCli2: TDT_P2PVM_NoAuth_Custom_Client;

implementation

{$R *.dfm}


procedure TDT_P2PVM_CustomClientForm.connButtonClick(Sender: TObject);
begin
  PhysicsTunnel.OnAutomatedP2PVMClientConnectionDone_P := procedure(Sender: TCommunicationFramework; P_IO: TPeerIO)
    begin
      AuthCli.Connect_P(UserEdit.Text, PasswdEdit.Text, procedure(const cState: Boolean)
        begin
          if cState then
              DoStatus('用户 %s 双通道已建立', [UserEdit.Text]);
        end);
      NoAuthCli1.Connect_P(procedure(const cState: Boolean)
        begin
          if cState then
              DoStatus('NoAuth1 双通道已建立', []);
        end);
      NoAuthCli2.Connect_P(procedure(const cState: Boolean)
        begin
          if cState then
              DoStatus('NoAuth2 双通道已建立', []);
        end);
    end;
  PhysicsTunnel.AsyncConnectP('127.0.0.1', 11938, procedure(const cState: Boolean)
    begin
      DoStatus('物理链接已建立');
    end);
end;

procedure TDT_P2PVM_CustomClientForm.disButtonClick(Sender: TObject);
begin
  AuthCli.Disconnect;
  NoAuthCli1.Disconnect;
  NoAuthCli2.Disconnect;
  PhysicsTunnel.Disconnect;
end;

procedure TDT_P2PVM_CustomClientForm.FormDestroy(Sender: TObject);
begin
  DeleteDoStatusHook(self);
end;

procedure TDT_P2PVM_CustomClientForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatus_backcall);
  PhysicsTunnel := TPhysicsClient.Create;

  AuthCli := TDT_P2PVM_Custom_Client.Create(TDTClient, PhysicsTunnel, 'ACR', '::', '101', 'ACS', '::', '100');
  AuthCli.QuietMode := False;

  NoAuthCli1 := TDT_P2PVM_NoAuth_Custom_Client.Create(TDTClient_NoAuth, PhysicsTunnel, 'NACR', '::', '201', 'NACS', '::', '200');
  NoAuthCli1.QuietMode := False;

  NoAuthCli2 := TDT_P2PVM_NoAuth_Custom_Client.Create(TDTClient_NoAuth, PhysicsTunnel, 'NACR', '::', '301', 'NACS', '::', '300');
  NoAuthCli2.QuietMode := False;
end;

procedure TDT_P2PVM_CustomClientForm.DoStatus_backcall(Text_: SystemString; const ID: Integer);
begin
  Memo.Lines.Add(Text_);
end;

procedure TDT_P2PVM_CustomClientForm.netTimerTimer(Sender: TObject);
begin
  CheckThread;
  AuthCli.Progress;
  NoAuthCli1.Progress;
  NoAuthCli2.Progress;
end;

end.
