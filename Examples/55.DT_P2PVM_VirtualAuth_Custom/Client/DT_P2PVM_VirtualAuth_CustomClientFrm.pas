unit DT_P2PVM_VirtualAuth_CustomClientFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,

  CoreClasses, PascalStrings, DoStatusIO, DataFrameEngine,
  CommunicationFramework, PhysicsIO,
  CommunicationFrameworkDoubleTunnelIO_VirtualAuth;

type
  TDT_P2PVM_VirtualAuth_ClientForm = class(TForm)
    netTimer: TTimer;
    connButton: TButton;
    disButton: TButton;
    Memo: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure netTimerTimer(Sender: TObject);
    procedure connButtonClick(Sender: TObject);
    procedure disButtonClick(Sender: TObject);
  private
    procedure DoStatus_backcall(Text_: SystemString; const ID: Integer);
  public
  end;

var
  DT_P2PVM_VirtualAuth_ClientForm: TDT_P2PVM_VirtualAuth_ClientForm;
  PhysicsTunnel: TPhysicsClient;
  Cli1: TDT_P2PVM_VirtualAuth_Custom_Client;
  Cli2: TDT_P2PVM_VirtualAuth_Custom_Client;
  Cli3: TDT_P2PVM_VirtualAuth_Custom_Client;

implementation

{$R *.dfm}


procedure TDT_P2PVM_VirtualAuth_ClientForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatus_backcall);
  PhysicsTunnel := TPhysicsClient.Create;
  Cli1 := TDT_P2PVM_VirtualAuth_Custom_Client.Create(TDTClient_VirtualAuth, PhysicsTunnel, 'R1', '::', '100', 'S1', '::', '99');
  Cli2 := TDT_P2PVM_VirtualAuth_Custom_Client.Create(TDTClient_VirtualAuth, PhysicsTunnel, 'R2', '::', '200', 'S2', '::', '199');
  Cli3 := TDT_P2PVM_VirtualAuth_Custom_Client.Create(TDTClient_VirtualAuth, PhysicsTunnel, 'R3', '::', '300', 'S3', '::', '299');

  Cli1.QuietMode := False;
  Cli2.QuietMode := False;
  Cli3.QuietMode := False;
end;

procedure TDT_P2PVM_VirtualAuth_ClientForm.FormDestroy(Sender: TObject);
begin
  DeleteDoStatusHook(self);
end;

procedure TDT_P2PVM_VirtualAuth_ClientForm.netTimerTimer(Sender: TObject);
begin
  CheckThread;
  Cli1.Progress;
  Cli2.Progress;
  Cli3.Progress;
end;

procedure TDT_P2PVM_VirtualAuth_ClientForm.connButtonClick(Sender: TObject);
begin
  PhysicsTunnel.AsyncConnectP('127.0.0.1', 11938, procedure(const cState: Boolean)
    begin
      if cState then
          DoStatus('完成物理隧道链接');
    end);
  PhysicsTunnel.OnAutomatedP2PVMClientConnectionDone_P := procedure(Sender: TCommunicationFramework; P_IO: TPeerIO)
    begin
      Cli1.Connect_P('user1', '123456', procedure(const cState: Boolean)
        begin
          if cState then
              DoStatus('user1 双通道构建完成.');
        end);
      Cli2.Connect_P('user2', '123456', procedure(const cState: Boolean)
        begin
          if cState then
              DoStatus('user2 双通道构建完成.');
        end);
      Cli3.Connect_P('user3', '123456', procedure(const cState: Boolean)
        begin
          if cState then
              DoStatus('user3 双通道构建完成.');
        end);
    end;
end;

procedure TDT_P2PVM_VirtualAuth_ClientForm.disButtonClick(Sender: TObject);
begin
  Cli1.Disconnect;
  Cli2.Disconnect;
  Cli3.Disconnect;
  PhysicsTunnel.Progress;
  PhysicsTunnel.Disconnect;
end;

procedure TDT_P2PVM_VirtualAuth_ClientForm.DoStatus_backcall(Text_: SystemString; const ID: Integer);
begin
  Memo.Lines.Add(Text_);
end;

end.
