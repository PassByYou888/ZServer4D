unit DT_P2PVM_NoAuth_CustomClientFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,

  CoreClasses, PascalStrings, DoStatusIO, DataFrameEngine,
  CommunicationFramework, PhysicsIO,
  CommunicationFrameworkDoubleTunnelIO_NoAuth;

type
  TDT_P2PVM_NoAuth_ClientForm = class(TForm)
    Memo: TMemo;
    netTimer: TTimer;
    connButton: TButton;
    disButton: TButton;
    phyconnButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure netTimerTimer(Sender: TObject);
    procedure connButtonClick(Sender: TObject);
    procedure disButtonClick(Sender: TObject);
    procedure phyconnButtonClick(Sender: TObject);
  private
    procedure DoStatus_backcall(Text_: SystemString; const ID: Integer);
  public
  end;

var
  DT_P2PVM_NoAuth_ClientForm: TDT_P2PVM_NoAuth_ClientForm;
  PhysicsTunnel: TPhysicsClient;
  Cli1: TDT_P2PVM_NoAuth_Custom_Client;
  Cli2: TDT_P2PVM_NoAuth_Custom_Client;
  Cli3: TDT_P2PVM_NoAuth_Custom_Client;

implementation

{$R *.dfm}

procedure TDT_P2PVM_NoAuth_ClientForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatus_backcall);
  PhysicsTunnel := TPhysicsClient.Create;
  Cli1 := TDT_P2PVM_NoAuth_Custom_Client.Create(TDTClient_NoAuth, PhysicsTunnel, 'R1', '::', '100', 'S1', '::', '99');
  Cli2 := TDT_P2PVM_NoAuth_Custom_Client.Create(TDTClient_NoAuth, PhysicsTunnel, 'R2', '::', '200', 'S2', '::', '199');
  Cli3 := TDT_P2PVM_NoAuth_Custom_Client.Create(TDTClient_NoAuth, PhysicsTunnel, 'R3', '::', '300', 'S3', '::', '299');

  Cli1.QuietMode := False;
  Cli2.QuietMode := False;
  Cli3.QuietMode := False;
end;

procedure TDT_P2PVM_NoAuth_ClientForm.FormDestroy(Sender: TObject);
begin
  DeleteDoStatusHook(self);
end;

procedure TDT_P2PVM_NoAuth_ClientForm.netTimerTimer(Sender: TObject);
begin
  CheckThread;
  Cli1.Progress;
  Cli2.Progress;
  Cli3.Progress;
end;

procedure TDT_P2PVM_NoAuth_ClientForm.connButtonClick(Sender: TObject);
begin
  PhysicsTunnel.OnAutomatedP2PVMClientConnectionDone_P := procedure(Sender: TCommunicationFramework; P_IO: TPeerIO)
    begin
      Cli1.Connect_P(procedure(const cState: Boolean)
        begin
          if cState then
              DoStatus('cli1 双通道构建完成.');
        end);
      Cli2.Connect_P(procedure(const cState: Boolean)
        begin
          if cState then
              DoStatus('cli2 双通道构建完成.');
        end);
      Cli3.Connect_P(procedure(const cState: Boolean)
        begin
          if cState then
              DoStatus('cli3 双通道构建完成.');
        end);
    end;
  PhysicsTunnel.AsyncConnectP('127.0.0.1', 11938, procedure(const cState: Boolean)
    begin
      if cState then
          DoStatus('完成物理隧道链接');
    end);
end;

procedure TDT_P2PVM_NoAuth_ClientForm.disButtonClick(Sender: TObject);
begin
  Cli1.Disconnect;
  Cli2.Disconnect;
  Cli3.Disconnect;
  PhysicsTunnel.Progress;
  PhysicsTunnel.Disconnect;
end;

procedure TDT_P2PVM_NoAuth_ClientForm.DoStatus_backcall(Text_: SystemString; const ID: Integer);
begin
  Memo.Lines.Add(Text_);
end;

procedure TDT_P2PVM_NoAuth_ClientForm.phyconnButtonClick(Sender: TObject);
begin
  PhysicsTunnel.AsyncConnectP('127.0.0.1', 11938, procedure(const cState: Boolean)
    begin
      if cState then
          DoStatus('完成物理隧道链接');
    end);
end;

end.
