unit DServFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  CommunicationFramework,
  CommunicationFramework_Server_DIOCP,
  CommunicationFramework_Server_CrossSocket, DoStatusIO, CoreClasses,
  DataFrameEngine, CommunicationFrameworkDoubleTunnelIO_NoAuth;

type
  TDoubleServerForm = class;

  TMyService = class(TCommunicationFramework_DoubleTunnelService_NoAuth)
  private
    f: TDoubleServerForm;
  protected
    procedure UserLinkSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth); override;
    procedure UserOut(UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth); override;
  protected
    // reg cmd
    procedure cmd_helloWorld_Console(Sender: TPeerClient; InData: string);
    procedure cmd_helloWorld_Stream(Sender: TPeerClient; InData: TDataFrameEngine);
    procedure cmd_helloWorld_Stream_Result(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
  public
    procedure RegisterCommand; override;
    procedure UnRegisterCommand; override;
  end;

  TDoubleServerForm = class(TForm)
    Memo1: TMemo;
    StartServiceButton: TButton;
    Timer1: TTimer;
    ChangeCaptionButton: TButton;
    GetClientValueButton: TButton;
    procedure StartServiceButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ChangeCaptionButtonClick(Sender: TObject);
    procedure GetClientValueButtonClick(Sender: TObject);
  private
    { Private declarations }
    procedure DoStatusNear(AText: string; const ID: Integer);
  public
    { Public declarations }
    RecvTunnel: TCommunicationFramework_Server_DIOCP;
    SendTunnel: TCommunicationFramework_Server_DIOCP;
    Service   : TMyService;
  end;

var
  DoubleServerForm: TDoubleServerForm;

implementation

{$R *.dfm}


procedure TMyService.UserLinkSuccess(UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth);
begin
  inherited UserLinkSuccess(UserDefineIO);
  DoStatus('user link success!');
end;

procedure TMyService.UserOut(UserDefineIO: TPeerClientUserDefineForRecvTunnel_NoAuth);
begin
  inherited UserOut(UserDefineIO);
  DoStatus('user out!');
end;

procedure TMyService.cmd_helloWorld_Console(Sender: TPeerClient; InData: string);
begin
  DoStatus('client: %s', [InData]);
end;

procedure TMyService.cmd_helloWorld_Stream(Sender: TPeerClient; InData: TDataFrameEngine);
begin
  DoStatus('client: %s', [InData.Reader.ReadString]);
end;

procedure TMyService.cmd_helloWorld_Stream_Result(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
begin
  OutData.WriteString('result 654321');
end;

procedure TMyService.RegisterCommand;
begin
  inherited RegisterCommand;
  RecvTunnel.RegisterDirectConsole('helloWorld_Console').OnExecute := cmd_helloWorld_Console;
  RecvTunnel.RegisterDirectStream('helloWorld_Stream').OnExecute := cmd_helloWorld_Stream;
  RecvTunnel.RegisterStream('helloWorld_Stream_Result').OnExecute := cmd_helloWorld_Stream_Result;
end;

procedure TMyService.UnRegisterCommand;
begin
  inherited UnRegisterCommand;
  RecvTunnel.UnRegisted('helloWorld_Console');
  RecvTunnel.UnRegisted('helloWorld_Stream');
  RecvTunnel.UnRegisted('helloWorld_Stream_Result');
end;

procedure TDoubleServerForm.ChangeCaptionButtonClick(Sender: TObject);
var
  de: TDataFrameEngine;
begin
  de := TDataFrameEngine.Create;
  de.WriteString('change caption as hello World,from server!');
  SendTunnel.BroadcastDirectStreamCmd('ChangeCaption', de);
  disposeObject(de);
end;

procedure TDoubleServerForm.DoStatusNear(AText: string; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
end;

procedure TDoubleServerForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatusNear);

  RecvTunnel := TCommunicationFramework_Server_DIOCP.Create;
  SendTunnel := TCommunicationFramework_Server_DIOCP.Create;
  Service := TMyService.Create(RecvTunnel, SendTunnel);
  Service.f := self;
end;

procedure TDoubleServerForm.FormDestroy(Sender: TObject);
begin
  disposeObject([RecvTunnel, SendTunnel, Service]);
  DeleteDoStatusHook(self);
end;

procedure TDoubleServerForm.GetClientValueButtonClick(Sender: TObject);
begin
  SendTunnel.ProgressPeerIOP(procedure(PeerClient: TPeerClient)
    var
      c: TPeerClient;
      de: TDataFrameEngine;
    begin
      c := PeerClient;
      de := TDataFrameEngine.Create;
      de.WriteString('change caption as hello World,from server!');
      c.SendStreamCmdP('GetClientValue', de,
        procedure(Sender: TPeerClient; ResultData: TDataFrameEngine)
        begin
          if ResultData.Count > 0 then
              DoStatus('getClientValue [%s] response:%s', [c.GetPeerIP, ResultData.Reader.ReadString]);
        end);
      disposeObject(de);
    end);
end;

procedure TDoubleServerForm.StartServiceButtonClick(Sender: TObject);
begin
  // 基于CrosssSocket官方文档，绑定字符串如果为空，绑定IPV6+IPV4
  if SendTunnel.StartService('', 9816) then
      DoStatus('listen send service success')
  else
      DoStatus('listen send service failed!');
  SendTunnel.IDCounter := 100;

  if RecvTunnel.StartService('', 9815) then
      DoStatus('listen Recv service success')
  else
      DoStatus('listen Recv service failed!');

  Service.RegisterCommand;
end;

procedure TDoubleServerForm.Timer1Timer(Sender: TObject);
begin
  Service.Progress;
end;

end.
