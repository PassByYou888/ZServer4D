unit DT_P2PVM_NoAuth_CustomServerFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,

  CoreClasses, PascalStrings, DoStatusIO, DataFrameEngine,
  CommunicationFramework, PhysicsIO,
  CommunicationFrameworkDoubleTunnelIO_NoAuth;

type
  TDT_P2PVM_NoAuth_ServerForm = class(TForm)
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

var
  DT_P2PVM_NoAuth_ServerForm: TDT_P2PVM_NoAuth_ServerForm;
  PhysicsTunnel: TPhysicsServer;
  Serv1: TDT_P2PVM_NoAuth_Custom_Service;
  Serv2: TDT_P2PVM_NoAuth_Custom_Service;
  Serv3: TDT_P2PVM_NoAuth_Custom_Service;

implementation

{$R *.dfm}


procedure TDT_P2PVM_NoAuth_ServerForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatus_backcall);

  PhysicsTunnel := TPhysicsServer.Create;
  Serv1 := TDT_P2PVM_NoAuth_Custom_Service.Create(TDTService_NoAuth, PhysicsTunnel, 'R1', '::', '99', 'S1', '::', '100');
  Serv2 := TDT_P2PVM_NoAuth_Custom_Service.Create(TDTService_NoAuth, PhysicsTunnel, 'R2', '::', '199', 'S2', '::', '200');
  Serv3 := TDT_P2PVM_NoAuth_Custom_Service.Create(TDTService_NoAuth, PhysicsTunnel, 'R3', '::', '299', 'S3', '::', '300');

  Serv1.QuietMode := False;
  Serv2.QuietMode := False;
  Serv3.QuietMode := False;
end;

procedure TDT_P2PVM_NoAuth_ServerForm.FormDestroy(Sender: TObject);
begin
  DeleteDoStatusHook(self);
end;

procedure TDT_P2PVM_NoAuth_ServerForm.netTimerTimer(Sender: TObject);
begin
  CheckThread;
  Serv1.Progress;
  Serv2.Progress;
  Serv3.Progress;
end;

procedure TDT_P2PVM_NoAuth_ServerForm.startservButtonClick(Sender: TObject);
begin
  if PhysicsTunnel.StartService('0.0.0.0', 11938) then
    begin
      Serv1.StartService;
      Serv2.StartService;
      Serv3.StartService;
      DoStatus('Listening succeeded');
    end;
end;

procedure TDT_P2PVM_NoAuth_ServerForm.stopservButtonClick(Sender: TObject);
begin
  Serv1.StopService;
  Serv2.StopService;
  Serv3.StopService;
  PhysicsTunnel.StopService;
end;

procedure TDT_P2PVM_NoAuth_ServerForm.DoStatus_backcall(Text_: SystemString; const ID: Integer);
begin
  Memo.Lines.Add(Text_);
end;

end.
