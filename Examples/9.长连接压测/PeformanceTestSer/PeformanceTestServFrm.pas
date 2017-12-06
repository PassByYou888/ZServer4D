unit PeformanceTestServFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  CommunicationFramework,
  CommunicationFramework_Server_ICS,
  CommunicationFramework_Server_Indy,
  CommunicationFramework_Server_CrossSocket, DoStatusIO, CoreClasses,
  DataFrameEngine, UnicodeMixedLib, MemoryStream64, CommunicationTest;

type
  TEZServerForm = class(TForm)
    Memo1: TMemo;
    StartServiceButton: TButton;
    Timer1: TTimer;
    Timer2: TTimer;
    procedure StartServiceButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
  private
    { Private declarations }
    tempStream: TMemoryStream64;

    procedure DoStatusNear(AText: string; const ID: Integer);
  public
    { Public declarations }
    server: TCommunicationFramework_Server_CrossSocket;
    test  : TCommunicationTestIntf;
  end;

var
  EZServerForm: TEZServerForm;

implementation

{$R *.dfm}


procedure TEZServerForm.DoStatusNear(AText: string; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
end;

procedure TEZServerForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatusNear);
  server := TCommunicationFramework_Server_CrossSocket.Create;
  test := TCommunicationTestIntf.Create;

  server.AllowPrintCommand := False;
  server.SwitchMaxPerformance;

  test.RegCmd(server);
end;

procedure TEZServerForm.FormDestroy(Sender: TObject);
begin
  DisposeObject([test, server]);
  DeleteDoStatusHook(self);
end;

procedure TEZServerForm.StartServiceButtonClick(Sender: TObject);
begin
  // 基于CrosssSocket官方文档，绑定字符串如果为空，绑定IPV6+IPV4
  if server.StartService('', 9818) then
      DoStatus('start service success')
  else
      DoStatus('start service failed!')
end;

procedure TEZServerForm.Timer1Timer(Sender: TObject);
begin
  server.ProgressBackground;
  Caption := Format('online client:%d', [server.Count]);
end;

procedure TEZServerForm.Timer2Timer(Sender: TObject);
begin
  if Memo1.Lines.Count > 5000 then
    Memo1.Clear;
end;

end.
