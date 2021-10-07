unit HPCServFrm;


interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Threading,
  CommunicationFramework,
  CommunicationFramework_Server_ICS,
  CommunicationFramework_Server_Indy,
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
    RecvTunnel: TCommunicationFramework_Server_CrossSocket;
    SendTunnel: TCommunicationFramework_Server_CrossSocket;
    Service: TMyService;
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

procedure TMyService.cmd_helloWorld_Stream_Result(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
begin
  {  HPC delay background operation mechanism demonstration, the mechanism is very simple, and can stack engineering code on a large scale  }
  CommunicationFramework.RunHPC_StreamP(Sender, nil, nil, InData, OutData,
    procedure(ThSender: THPC_Stream; ThInData, ThOutData: TDataFrameEngine)
    begin
      {  If there is a dispatch center server in your background server framework: managerserver  }
      TCompute.sync(procedure
        begin
          {  We need to tell the dispatch center server in the running area of the main process that I have started large-scale computing  }
        end);

      {  The difference between HPC delay background operation and traditional processing is that we need to use thindata and thoutdata to identify and feed back data  }

      {  Dostatus can be used in HPC background calculation  }
      DoStatus('run compute thread');

      {  The code here works in the thread zone  }
      ThOutData.WriteString('result 654321');

      {  If you need to synchronize to the main thread, you need to use  }
      TCompute.sync(procedure
        begin
          {  This is the synchronization zone of the main process, such as file operation, ZDB database operation, etc  }
        end);

      {  Parallelization is safe in the background deferred threads of HPC  }
      {  Parallelfor is better than Delphi's built-in tparallel.for  }
      {  Parallelfor is better than mtprocs built into FPC  }
      ParallelFor(0, 10000, procedure(pass: Integer)
        begin
          {  In parallel processing, we cannot use the TThread. Synchronize method because of the design problem of EMB  }
          {  In parallel processing, we can use zserver kernel atomic locks  }
          LockObject(Sender);
          UnLockObject(Sender);

          {  Dostatus can be used in HPC background calculation  }
          if pass mod 1000 = 1 then
              DoStatus('run compute thread:%d', [pass]);
        end);

      {  If there is a dispatch center server in your background server framework: managerserver  }
      TCompute.sync(procedure
        begin
          {  We need to tell the dispatch center server in the running area of the main process that my large-scale computing work is finished  }
        end);

      {  Finally, at the end of the deferred run, thoutdata will be sent to the client, and then the temporary memory will be released  }
    end);
end;

procedure TMyService.RegisterCommand;
begin
  inherited RegisterCommand;
  RecvTunnel.RegisterStream('helloWorld_Stream_Result').OnExecute := cmd_helloWorld_Stream_Result;
end;

procedure TMyService.UnRegisterCommand;
begin
  inherited UnRegisterCommand;
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

  RecvTunnel := TCommunicationFramework_Server_CrossSocket.Create;
  SendTunnel := TCommunicationFramework_Server_CrossSocket.Create;
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
  {  Based on the official crosssocket document, if the binding string is empty, bind IPv6 + IPv4  }
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

