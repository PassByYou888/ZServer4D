unit AFDRServFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  CommunicationFramework,
  CommunicationFramework_Server_ICS,
  CommunicationFramework_Server_Indy,
  CommunicationFramework_Server_CrossSocket, DoStatusIO, CoreClasses,
  DataFrameEngine, Cadencer, NotifyObjectBase;

type
  TDRServerForm = class(TForm)
    Memo1: TMemo;
    StartServiceButton: TButton;
    Timer1: TTimer;
    procedure StartServiceButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    procedure DoStatusNear(AText: string; const ID: Integer);

    procedure PostExecute_DelayResponse(Sender: TNPostExecute);
    procedure cmd_DelayResponse(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
  public
    { Public declarations }

    {  One way server framework  }
    server: TCommunicationFramework_Server_CrossSocket;

    {  Precise physical beat time engine to support delay processing engine  }
    cadencerEng: TCadencer;

    {  The delay event processing engine is used to simulate the asynchronous delay of the server  }
    ProgressPost: TNProgressPost;

    procedure CadencerProgress(Sender: TObject; const deltaTime, newTime: Double);
  end;

var
  DRServerForm: TDRServerForm;

implementation

{$R *.dfm}

procedure TDRServerForm.CadencerProgress(Sender: TObject; const deltaTime, newTime: Double);
begin
  ProgressPost.Progress(deltaTime);
end;

procedure TDRServerForm.PostExecute_DelayResponse(Sender: TNPostExecute);
var
  ID: Cardinal;
  c : TPeerClient;
begin
  {  Find the ID from the client linked list. If the client does not exist, return the nil value  }
  ID := Sender.Data3;
  c := server.PeerIO[ID];
  {  During the delay, the client may have been disconnected  }
  if c = nil then
      exit;

  c.OutDataFrame.WriteString('Command execution time:' + TimeToStr(time));

  {  Immediately feed back the response data to the client and continue to process the internal waiting queue status  }
  c.ContinueResultSend;
end;

procedure TDRServerForm.cmd_DelayResponse(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
begin
  {  After the delayresponse command is executed, it will not give feedback to the client immediately  }
  {  The delayed response mechanism is implemented by state machine. Once the response stops, the instructions in the queue will be in the waiting state  }
  {  The delay mechanism is mainly used for cross server communication or nonlinear processes  }
  Sender.PauseResultSend;

  OutData.WriteString('Command received:' + TimeToStr(time));

  {  Throw a one-time event to be executed after 1.5 seconds to the delayed event engine  }
  {  This event is used to asynchronously simulate the communication delay with another server on the server  }
  {  Suppose another server responds to the data after 1.5 seconds. At this time, it processes the command asynchronously and then continues to feed back to the client  }
  {  During the delay, the instructions in the queue will be waiting  }
  with ProgressPost.PostExecuteM(1.5, PostExecute_DelayResponse) do
    begin
      {  Delay needs to record the unique ID of the current client  }
      Data3 := Sender.ID;
    end;
end;

procedure TDRServerForm.DoStatusNear(AText: string; const ID: Integer);
begin
  Memo1.Lines.Add(AText);
end;

procedure TDRServerForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatusNear);
  server := TCommunicationFramework_Server_CrossSocket.Create;

  server.RegisterStream('DelayResponse').OnExecute := cmd_DelayResponse;

  cadencerEng := TCadencer.Create;
  cadencerEng.OnProgress := CadencerProgress;
  ProgressPost := TNProgressPost.Create;
end;

procedure TDRServerForm.FormDestroy(Sender: TObject);
begin
  DisposeObject([server, cadencerEng, ProgressPost]);
  DeleteDoStatusHook(self);
end;

procedure TDRServerForm.StartServiceButtonClick(Sender: TObject);
begin
  {  Based on the official crosssocket document, if the binding string is empty, bind IPv6 + IPv4  }
  if server.StartService('', 9818) then
      DoStatus('start service success')
  else
      DoStatus('start service failed!')
end;

procedure TDRServerForm.Timer1Timer(Sender: TObject);
begin
  server.Progress;
  cadencerEng.Progress;
end;

end.
