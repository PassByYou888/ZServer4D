unit FMXAuthDoubleTunnelCliFrm;


interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Edit,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,

  CommunicationFramework_Client_Indy, DataFrameEngine,
  CommunicationFramework, CoreClasses, DoStatusIO,
  CommunicationFrameworkDoubleTunnelIO;

type
  TFMXAuthDoubleClientForm = class(TForm)
    Memo1: TMemo;
    connectButton: TButton;
    HostEdit: TEdit;
    HelloWorldBtn: TButton;
    Timer1: TTimer;
    UserEdit: TEdit;
    PasswdEdit: TEdit;
    RegUserButton: TButton;
    AsyncButton: TButton;
    timeLabel: TLabel;
    FixedTimeButton: TButton;
    connectTunnelButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure connectButtonClick(Sender: TObject);
    procedure HelloWorldBtnClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure RegUserButtonClick(Sender: TObject);
    procedure AsyncButtonClick(Sender: TObject);
    procedure FixedTimeButtonClick(Sender: TObject);
    procedure connectTunnelButtonClick(Sender: TObject);
  private
    { Private declarations }
    procedure DoStatusNear(AText: string; const ID: Integer);

    procedure cmd_ChangeCaption(Sender: TPeerClient; InData: TDataFrameEngine);
    procedure cmd_GetClientValue(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
  public
    { Public declarations }
    {  VM tunnel  }
    {  The VM tunnel can take up the work of the whole protocol stack while working normally  }
    {  Here, we will bind recvtunnel + sendtunnel in vmtunnel at the same time, and use only one link to implement dual channel service  }
    {  VM tunnel can be any socket, bedstead, Indy, ICs, crosssocket, etc. all support VM tunnel  }
    VMTunnel: TCommunicationFramework_Client_Indy;

    {  ZS normal communication framework  }
    RecvTunnel: TCommunicationFrameworkWithP2PVM_Client;
    SendTunnel: TCommunicationFrameworkWithP2PVM_Client;
    client: TCommunicationFramework_DoubleTunnelClient;
  end;

var
  FMXAuthDoubleClientForm: TFMXAuthDoubleClientForm;

implementation

{$R *.fmx}

{ TFMXClientForm }

procedure TFMXAuthDoubleClientForm.cmd_ChangeCaption(Sender: TPeerClient; InData: TDataFrameEngine);
begin
  DoStatus('Change Caption:%s', [InData.Reader.ReadString]);
end;

procedure TFMXAuthDoubleClientForm.cmd_GetClientValue(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
begin
  OutData.WriteString('getclientvalue:abc');
end;

procedure TFMXAuthDoubleClientForm.connectButtonClick(Sender: TObject);
begin
  SendTunnel.Connect('::', 2);
  RecvTunnel.Connect('::', 1);

  {  Check whether the dual channels have been successfully linked, and ensure that the initialization of symmetric encryption and so on has been completed  }
  while (not client.RemoteInited) and (client.Connected) do
    begin
      TThread.Sleep(10);
      client.Progress;
    end;

  if client.Connected then
    begin
      {  Nested anonymous function support  }
      client.UserLoginP(UserEdit.Text, PasswdEdit.Text,
        procedure(const State: Boolean)
        begin
          if State then
              client.TunnelLinkP(
              procedure(const State: Boolean)
              begin
                DoStatus('double tunnel link success!');
              end)
        end);
    end;
end;

procedure TFMXAuthDoubleClientForm.connectTunnelButtonClick(
  Sender: TObject);
begin
  if not VMTunnel.Connect(HostEdit.Text, 9899) then
      exit;

  VMTunnel.ClientIO.BuildP2PAuthTokenP(procedure
    begin
      VMTunnel.ClientIO.OpenP2PVMTunnelP(True, '', procedure(const VMauthState: Boolean)
        begin
          {  If VM tunnel handshake succeeds  }
          if VMauthState then
            begin
              {  Bind the client framework to the tunnel  }
              {  There are two clients here. We bind them  }
              VMTunnel.ClientIO.p2pVMTunnel.InstallLogicFramework(SendTunnel);
              VMTunnel.ClientIO.p2pVMTunnel.InstallLogicFramework(RecvTunnel);
            end
        end);
    end);
end;

procedure TFMXAuthDoubleClientForm.DoStatusNear(AText: string; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
  Memo1.GoToTextEnd;
end;

procedure TFMXAuthDoubleClientForm.FixedTimeButtonClick(Sender: TObject);
begin
  {  High speed synchronization without progress trigger  }
  {  This is done to minimize the delay rate of time  }
  client.SendTunnel.SyncOnResult := True;
  client.SyncCadencer;
  client.SendTunnel.WaitP(1000, procedure(const cState: Boolean)
    begin
      {  Because after synconresult is opened, nested deadlocks will occur in anonymous functions  }
      {  We now turn it off to ensure nested execution of anonymous functions  }
      client.SendTunnel.SyncOnResult := False;
    end);
end;

procedure TFMXAuthDoubleClientForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatusNear);

  {  VM tunnel  }
  {  The VM tunnel can take up the work of the whole protocol stack while working normally  }
  {  Here, we will bind recvtunnel + sendtunnel in vmtunnel at the same time, and use only one link to implement dual channel service  }
  VMTunnel := TCommunicationFramework_Client_Indy.Create;

  {  ZS normal communication framework  }
  RecvTunnel := TCommunicationFrameworkWithP2PVM_Client.Create;
  SendTunnel := TCommunicationFrameworkWithP2PVM_Client.Create;
  client := TCommunicationFramework_DoubleTunnelClient.Create(RecvTunnel, SendTunnel);

  client.RegisterCommand;

  {  Register communication instructions that can be initiated by the server  }
  client.RecvTunnel.RegisterDirectStream('ChangeCaption').OnExecute := cmd_ChangeCaption;
  client.RecvTunnel.RegisterStream('GetClientValue').OnExecute := cmd_GetClientValue;
end;

procedure TFMXAuthDoubleClientForm.FormDestroy(Sender: TObject);
begin
  DisposeObject(client);
  DeleteDoStatusHook(self);
end;

procedure TFMXAuthDoubleClientForm.HelloWorldBtnClick(Sender: TObject);
var
  SendDe, ResultDE: TDataFrameEngine;
begin
  {  Send a hello world command in the form of console to the server  }
  client.SendTunnel.SendDirectConsoleCmd('helloWorld_Console', '');

  {  Send a hello world instruction in the form of stream to the server  }
  SendDe := TDataFrameEngine.Create;
  SendDe.WriteString('directstream 123456');
  client.SendTunnel.SendDirectStreamCmd('helloWorld_Stream', SendDe);
  DisposeObject([SendDe]);

  {  The stream instruction is sent asynchronously and received, and the feedback is triggered by proc callback  }
  SendDe := TDataFrameEngine.Create;
  SendDe.WriteString('123456');
  client.SendTunnel.SendStreamCmdP('helloWorld_Stream_Result', SendDe,
    procedure(Sender: TPeerClient; ResultData: TDataFrameEngine)
    begin
      if ResultData.Count > 0 then
          DoStatus('server response:%s', [ResultData.Reader.ReadString]);
    end);
  DisposeObject([SendDe]);

  {  Send in blocking mode and receive stream instruction  }
  SendDe := TDataFrameEngine.Create;
  ResultDE := TDataFrameEngine.Create;
  SendDe.WriteString('123456');
  client.SendTunnel.WaitSendStreamCmd('helloWorld_Stream_Result', SendDe, ResultDE, 5000);
  if ResultDE.Count > 0 then
      DoStatus('server response:%s', [ResultDE.Reader.ReadString]);
  DisposeObject([SendDe, ResultDE]);
end;

procedure TFMXAuthDoubleClientForm.RegUserButtonClick(Sender: TObject);
begin
  SendTunnel.Connect('::', 2);
  RecvTunnel.Connect('::', 1);

  client.RegisterUserP(UserEdit.Text, PasswdEdit.Text, procedure(const rState: Boolean)
    begin
      client.Disconnect;
    end);
end;

procedure TFMXAuthDoubleClientForm.AsyncButtonClick(Sender: TObject);
begin
  {  Asynchronous dual channel link  }
  client.AsyncConnectP('::', 1, 2,
    procedure(const cState: Boolean)
    begin
      if cState then
        begin
          DoStatus('connected success!');
          {  Nested anonymous function support  }
          client.UserLoginP(UserEdit.Text, PasswdEdit.Text,
            procedure(const lState: Boolean)
            begin
              if lState then
                begin
                  DoStatus('login successed!');
                  client.TunnelLinkP(
                    procedure(const tState: Boolean)
                    begin
                      if tState then
                          DoStatus('double tunnel link success!')
                      else
                          DoStatus('double tunnel link failed!');
                    end)
                end
              else
                begin
                  DoStatus('login failed!');
                end;
            end);
        end
      else
        begin
          DoStatus('connected failed!');
        end;
    end);
end;

procedure TFMXAuthDoubleClientForm.Timer1Timer(Sender: TObject);
begin
  VMTunnel.Progress;
  client.Progress;
  timeLabel.Text := Format('sync time:%f', [client.CadencerEngine.UpdateCurrentTime]);
end;

end.

