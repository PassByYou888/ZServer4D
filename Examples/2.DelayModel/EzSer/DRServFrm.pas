unit DRServFrm;

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

    // 单向模式的服务器框架
    server: TCommunicationFramework_Server_CrossSocket;

    // 精确物理节拍时间引擎，用于支持延迟处理引擎
    cadencerEng: TCadencer;

    // 延迟事件处理引擎，用于模拟服务器的异步延迟
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
  // 从客户端链表查找ID，如果客户端不存在，返回nil值
  ID := Sender.Data3;
  c := server.PeerIO[ID];
  // 在延迟期间，客户端有可能已经断线
  if c = nil then
      exit;

  c.OutDataFrame.WriteString('执行命令时间:' + TimeToStr(time));

  // 立即往客户端反馈响应数据，并且继续处理内部的等待队列状态
  c.ContinueResultSend;
end;

procedure TDRServerForm.cmd_DelayResponse(Sender: TPeerClient; InData, OutData: TDataFrameEngine);
begin
  // DelayResponse命令被执行完成后，不会立即给客户端反馈
  // 延迟响应机制都采用状态机实现，一旦停止响应，队列中的指令都会处于等待状态
  // 延迟机制主要用于跨服通讯或非线性流程
  Sender.PauseResultSend;

  OutData.WriteString('收到命令时间:' + TimeToStr(time));

  // 往延迟事件引擎抛一个3.5秒以后执行的一次性事件
  // 此事件用于在服务器异步模拟与另一台服务器的通讯延迟
  // 假设另一台服务器在3.5秒以后，才响应了数据，这时候以异步方式处理完成命令后再继续给客户端反馈回去
  // 在延迟过程中队列中的指令都会处于等待状态
  with ProgressPost.PostExecuteM(3.5, PostExecute_DelayResponse) do
    begin
      // 延迟需要记录下当前客户端的唯一ID
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
  // 基于CrosssSocket官方文档，绑定字符串如果为空，绑定IPV6+IPV4
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
