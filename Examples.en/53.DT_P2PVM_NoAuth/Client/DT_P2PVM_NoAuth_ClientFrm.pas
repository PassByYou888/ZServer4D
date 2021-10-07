unit DT_P2PVM_NoAuth_ClientFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,

  CoreClasses, PascalStrings, DoStatusIO, DataFrameEngine,
  CommunicationFramework,
  CommunicationFrameworkDoubleTunnelIO_NoAuth;

type
  TDT_P2PVM_NoAuth_ClientForm = class(TForm)
    Memo: TMemo;
    netTimer: TTimer;
    connButton: TButton;
    disButton: TButton;
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
  DT_P2PVM_NoAuth_ClientForm: TDT_P2PVM_NoAuth_ClientForm;
  Cli: TDT_P2PVM_NoAuth_Client;

implementation

{$R *.dfm}


procedure TDT_P2PVM_NoAuth_ClientForm.connButtonClick(Sender: TObject);
begin
  Cli.Connect_P('127.0.0.1', '11938', '123456', procedure(const cState: Boolean)
    begin
      if cState then
          DoStatus('The dual channel construction is completed');
    end);
end;

procedure TDT_P2PVM_NoAuth_ClientForm.disButtonClick(Sender: TObject);
begin
  Cli.Disconnect;
end;

procedure TDT_P2PVM_NoAuth_ClientForm.FormDestroy(Sender: TObject);
begin
  DeleteDoStatusHook(self);
end;

procedure TDT_P2PVM_NoAuth_ClientForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatus_backcall);
  Cli := TDT_P2PVM_NoAuth_Client.Create(TDTClient_NoAuth);
end;

procedure TDT_P2PVM_NoAuth_ClientForm.DoStatus_backcall(Text_: SystemString; const ID: Integer);
begin
  Memo.Lines.Add(Text_);
end;

procedure TDT_P2PVM_NoAuth_ClientForm.netTimerTimer(Sender: TObject);
begin
  CheckThread;
  Cli.Progress;
end;

end.
