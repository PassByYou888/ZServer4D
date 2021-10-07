unit FMXDoubleTunnelCliFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Edit,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,

  CommunicationFramework_Client_Indy, DataFrameEngine,
  CommunicationFramework, CoreClasses, DoStatusIO,
  CommunicationFrameworkDoubleTunnelIO_NoAuth, FMX.Memo.Types;

type
  TFMXDoubleClientForm = class(TForm)
    Memo1: TMemo;
    connectButton: TButton;
    HostEdit: TEdit;
    HelloWorldBtn: TButton;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure connectButtonClick(Sender: TObject);
    procedure HelloWorldBtnClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    procedure DoStatusNear(AText: string; const ID: Integer);

    procedure cmd_ChangeCaption(Sender: TPeerClient; InData: TDataFrameEngine);
    procedure cmd_GetClientValue(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
  public
    { Public declarations }
    RecvTunnel: TCommunicationFramework_Client_Indy;
    SendTunnel: TCommunicationFramework_Client_Indy;
    client    : TCommunicationFramework_DoubleTunnelClient_NoAuth;
  end;

var
  FMXDoubleClientForm: TFMXDoubleClientForm;

implementation

{$R *.fmx}

{ TFMXClientForm }

procedure TFMXDoubleClientForm.cmd_ChangeCaption(Sender: TPeerClient; InData: TDataFrameEngine);
begin
  DoStatus('Change Caption:%s', [InData.Reader.ReadString]);
end;

procedure TFMXDoubleClientForm.cmd_GetClientValue(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
begin
  OutData.WriteString('getclientvalue:abc');
end;

procedure TFMXDoubleClientForm.connectButtonClick(Sender: TObject);
begin
  SendTunnel.Connect(HostEdit.Text, 9815);
  RecvTunnel.Connect(HostEdit.Text, 9816);

  {  Check whether the dual channels have been successfully linked, and ensure that the initialization of symmetric encryption and so on has been completed  }
  while (not client.RemoteInited) and (client.Connected) do
    begin
      TThread.Sleep(10);
      client.Progress;
    end;

  if client.Connected then
    begin
      {  Merge two channels asynchronously  }
      client.TunnelLinkP(
        procedure(const State: Boolean)
        begin
          if State then
            begin
              {  Dual channel link succeeded  }
              DoStatus('double tunnel link success!');
            end;
        end);
      {  Merge two channels synchronously  }
      // if client.TunnelLink then
      // begin
      {  //Dual channel link succeeded  }
      // end;
    end;
end;

procedure TFMXDoubleClientForm.DoStatusNear(AText: string; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
  Memo1.GoToTextEnd;
end;

procedure TFMXDoubleClientForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatusNear);

  RecvTunnel := TCommunicationFramework_Client_Indy.Create;
  SendTunnel := TCommunicationFramework_Client_Indy.Create;
  client := TCommunicationFramework_DoubleTunnelClient_NoAuth.Create(RecvTunnel, SendTunnel);

  client.RegisterCommand;

  {  Register communication instructions that can be initiated by the server  }
  client.RecvTunnel.RegisterDirectStream('ChangeCaption').OnExecute := cmd_ChangeCaption;
  client.RecvTunnel.RegisterStream('GetClientValue').OnExecute := cmd_GetClientValue;
end;

procedure TFMXDoubleClientForm.FormDestroy(Sender: TObject);
begin
  DisposeObject(client);
  DeleteDoStatusHook(self);
end;

procedure TFMXDoubleClientForm.HelloWorldBtnClick(Sender: TObject);
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

procedure TFMXDoubleClientForm.Timer1Timer(Sender: TObject);
begin
  client.Progress;
end;

end.
