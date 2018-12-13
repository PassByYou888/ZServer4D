unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  DoStatusIO, CoreClasses,
  CommunicationFramework,
  CommunicationFramework_Client_CrossSocket, CommunicationFrameworkDoubleTunnelIO_VirtualAuth,
  CommunicationFrameworkDoubleTunnelIO_NoAuth,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.ScrollBox, FMX.Memo;

type
  TMainForm = class(TForm)
    Timer: TTimer;
    Button1: TButton;
    Memo1: TMemo;
    procedure TimerTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    procedure DoStatusNear(AText: string; const ID: Integer);
    procedure ConnectServer();
  public
    { Public declarations }
    MyVMClient: TCommunicationFramework_DoubleTunnelClient_NoAuth;
    MyClient: TCommunicationFramework_DoubleTunnelClient_VirtualAuth;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}


procedure TMainForm.Button1Click(Sender: TObject);
begin
  ConnectServer;
end;

procedure TMainForm.DoStatusNear(AText: string; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
  Memo1.GoToTextEnd;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatusNear);
  MyVMClient := TCommunicationFramework_DoubleTunnelClient_NoAuth.Create(TCommunicationFrameworkWithP2PVM_Client.Create,
    TCommunicationFrameworkWithP2PVM_Client.Create);
  MyClient := TCommunicationFramework_DoubleTunnelClient_VirtualAuth.Create
    (TCommunicationFramework_Client_CrossSocket.Create, TCommunicationFramework_Client_CrossSocket.Create);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  DisposeObject([MyClient, MyVMClient]);
  DeleteDoStatusHook(self);
end;

procedure TMainForm.ConnectServer;
begin
  MyClient.AsyncConnectP('127.0.0.1', 10001, 10002,
    procedure(const cState: Boolean)
    begin
      if cState then
        begin
          MyClient.UserLoginP('Test', 'Test',
            procedure(const lState: Boolean)
            begin
              if lState then
                begin
                  MyClient.TunnelLinkP(
                    procedure(const tState: Boolean)
                    begin
                      if tState then
                        begin
                          MyClient.SendTunnel.ClientIO.BuildP2PAuthTokenP(
                            procedure
                            begin
                              MyClient.SendTunnel.ClientIO.OpenP2PVMTunnelP(True, '',
                                procedure(const VMauthState: Boolean)
                                begin
                                  if VMauthState then
                                    begin
                                      MyClient.SendTunnel.ClientIO.p2pVMTunnel.InstallLogicFramework(MyVMClient.SendTunnel);
                                      MyClient.SendTunnel.ClientIO.p2pVMTunnel.InstallLogicFramework(MyVMClient.RecvTunnel);
                                      MyVMClient.AsyncConnectP('::', 1, 2,
                                        procedure(const ccState: Boolean)
                                        begin
                                        if ccState then
                                        MyVMClient.TunnelLinkP(
                                        procedure(const lState: Boolean)
                                        begin
                                        if lState then
                                        begin
                                        DoStatus('chat client connected!');
                                        MyVMClient.SendTunnel.SendDirectConsoleCmd('helloWorld_Console', 'hello world baby,');
                                        end;
                                        end);
                                        end);
                                    end
                                end);
                            end);
                        end
                      else
                        begin
                          MyClient.Disconnect;
                          DoStatus('无法建立传输通道');
                        end;
                    end)
                end
              else
                begin
                  MyClient.Disconnect;
                  DoStatus('用户名密码错误');
                end;
            end);
        end
      else
        begin
          DoStatus('无法连接服务器');
        end;
    end);
end;

procedure TMainForm.TimerTimer(Sender: TObject);
begin
  MyClient.Progress;
  MyVMClient.Progress;
end;

end.
