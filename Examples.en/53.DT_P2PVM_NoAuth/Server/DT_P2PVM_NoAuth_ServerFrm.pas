unit DT_P2PVM_NoAuth_ServerFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,

  CoreClasses, PascalStrings, DoStatusIO, DataFrameEngine,
  CommunicationFramework,
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
  Serv: TDT_P2PVM_NoAuth_Service;

implementation

{$R *.dfm}


procedure TDT_P2PVM_NoAuth_ServerForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatus_backcall);
  Serv := TDT_P2PVM_NoAuth_Service.Create(TDTService_NoAuth);
  startservButtonClick(startservButton);
end;

procedure TDT_P2PVM_NoAuth_ServerForm.FormDestroy(Sender: TObject);
begin
  DeleteDoStatusHook(self);
end;

procedure TDT_P2PVM_NoAuth_ServerForm.netTimerTimer(Sender: TObject);
begin
  CheckThread;
  Serv.Progress;
end;

procedure TDT_P2PVM_NoAuth_ServerForm.startservButtonClick(Sender: TObject);
begin
  Serv.StartService('0.0.0.0', '11938', '123456');
end;

procedure TDT_P2PVM_NoAuth_ServerForm.stopservButtonClick(Sender: TObject);
begin
  Serv.StopService;
end;

procedure TDT_P2PVM_NoAuth_ServerForm.DoStatus_backcall(Text_: SystemString; const ID: Integer);
begin
  Memo.Lines.Add(Text_);
end;

end.
