unit DT_P2PVM_ServerFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,

  System.IOUtils,

  CoreClasses, PascalStrings, DoStatusIO, DataFrameEngine,
  CommunicationFramework,
  CommunicationFrameworkDoubleTunnelIO;

type
  TDT_P2PVM_ServerForm = class(TForm)
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
  DT_P2PVM_ServerForm: TDT_P2PVM_ServerForm;
  Serv: TDT_P2PVM_Service;

implementation

{$R *.dfm}


procedure TDT_P2PVM_ServerForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatus_backcall);
  Serv := TDT_P2PVM_Service.Create(TDTService);
  Serv.QuietMode := False;
  Serv.DTService.RootPath := TPath.GetLibraryPath;
  Serv.DTService.PublicPath := TPath.GetLibraryPath;
  Serv.DTService.LoadUserDB;
  Serv.DTService.AllowRegisterNewUser := False;
  Serv.DTService.AllowSaveUserInfo := True;
end;

procedure TDT_P2PVM_ServerForm.FormDestroy(Sender: TObject);
begin
  DeleteDoStatusHook(self);
end;

procedure TDT_P2PVM_ServerForm.netTimerTimer(Sender: TObject);
begin
  CheckThread;
  Serv.Progress;
end;

procedure TDT_P2PVM_ServerForm.startservButtonClick(Sender: TObject);
begin
  Serv.StartService('0.0.0.0', '11938', '123456');
end;

procedure TDT_P2PVM_ServerForm.stopservButtonClick(Sender: TObject);
begin
  Serv.StopService;
end;

procedure TDT_P2PVM_ServerForm.DoStatus_backcall(Text_: SystemString; const ID: Integer);
begin
  Memo.Lines.Add(Text_);
end;

procedure TDT_P2PVM_ServerForm.regUsrButtonClick(Sender: TObject);
begin
  if Serv.DTService.RegUser(UserEdit.Text, PasswdEdit.Text, nil) then
      DoStatus('register %s successed.', [UserEdit.Text])
  else
      DoStatus('register %s failed!', [UserEdit.Text]);
end;

end.
