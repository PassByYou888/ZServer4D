unit FragmentClientFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,

  CoreClasses, PascalStrings, UnicodeMixedLib, MemoryStream64,
  DoStatusIO,
  CommunicationFramework,
  PhysicsIO,
  CommunicationFrameworkDoubleTunnelIO_NoAuth, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  // 多线下载以标准数据结构梳理而出,思路简洁明了

  // TaskData是每个碎片的预置下载参数,包括远程文件名,碎片的起始位置,结束位置,碎片md5验证码,状态机等等
  TTaskData = record
    FileName: SystemString;
    StartPos, EndPos: Int64;
    Data: TMemoryStream64;
    MD5: TMD5;
    Done: Boolean;
    procedure Init;
    procedure Free;
  end;

  PTaskData = ^TTaskData;

  // 泛型TaskData链表
  TDownloadTask_ = TGenericsList<PTaskData>;

  // 默认泛型链表不会自动释放,这里需要做一个自动释放处理
  // 用方法代替状态机释放是出于兼容fpc的泛型机制,fpc的链表没有释放时的触发事件
  TDownloadTask = class(TDownloadTask_)
  public
    destructor Destroy; override;
    procedure Remove(p: PTaskData);
    procedure Delete(index: Integer);
    procedure Clear;
    function Done: Boolean;
  end;

  // 以session概念来定义多链接
  // 这里的多链接使用了p2pVM
  // 这里的p2pVM构建方式为极简机制的automatedP2PVM
  TP2PVM_Session = class
  private
    // 这里使用了AutomatedP2PVM支持技术,具体细节请参考相关demo
    // logic开头的都是虚拟连接,双通道那些东西
    logic_recv, logic_send: TCommunicationFrameworkWithP2PVM_Client;
    logic: TCommunicationFramework_DoubleTunnelClient_NoAuth;
    // phyCli是物理连接
    phyCli: TPhysicsClient;
    // 文件流下载计数
    total: Int64;
    // 碎片下载请求的临时暂存队列
    queue: TDownloadTask_;
    procedure autoP2PVM_Done(Sender: TCommunicationFramework; P_IO: TPeerIO);
    procedure Download_Backcall(const UserData: Pointer; const UserObject: TCoreClassObject;
      const FileName: SystemString; const StartPos, EndPos: Int64; const DataPtr: Pointer; const DataSize: Int64; const MD5: TMD5);
  public
    // 在create构造函数实现了automatedP2PVM
    constructor Create;
    destructor Destroy; override;
    procedure Progress;
    procedure Connect(Host: SystemString);
    procedure Download(p: PTaskData);
  end;

  TSessionList_ = TGenericsList<TP2PVM_Session>;

  // 默认泛型链表不会自动释放,这里需要做一个自动释放处理
  // 用方法代替状态机释放是出于兼容fpc的泛型机制,fpc的链表没有释放时的触发事件
  TSessionList = class(TSessionList_)
  private
    id: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Remove(obj: TP2PVM_Session);
    procedure Delete(index: Integer);
    procedure Clear;
    procedure Progress;
    function Pick_NextSession: TP2PVM_Session;
  end;

  TFragmentClientForm = class(TForm)
    Timer1: TTimer;
    Memo: TMemo;
    HostEdit: TLabeledEdit;
    connButton: TButton;
    downloadButton: TButton;
    checkDownTimer: TTimer;
    stateMemo: TMemo;
    procedure checkDownTimerTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure downloadButtonClick(Sender: TObject);
    procedure connButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Timer1Timer(Sender: TObject);
  private
    procedure DoStatus_Backcall(Text_: SystemString; const id: Integer);
  public
    Session: TSessionList;
    DownloadTask: TDownloadTask;
    targetStream: TStream64;
  end;

var
  FragmentClientForm: TFragmentClientForm;

const
  demoFile = 'FragmentServiceDemo.exe'; // 需要下载的远程文件名
  bloackSize = 8192;                    // 分块下载大小

implementation

{$R *.dfm}


procedure TTaskData.Init;
begin
  FileName := '';
  StartPos := 0;
  EndPos := 0;
  Data := TMemoryStream64.Create;
  MD5 := NULLMD5;
  Done := False;
end;

procedure TTaskData.Free;
begin
  FileName := '';
  DisposeObjectAndNIl(Data);
end;

destructor TDownloadTask.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TDownloadTask.Remove(p: PTaskData);
begin
  Dispose(p);
  inherited Remove(p);
end;

procedure TDownloadTask.Delete(index: Integer);
begin
  if (index >= 0) and (index < Count) then
    begin
      Dispose(Items[index]);
      inherited Delete(index);
    end;
end;

procedure TDownloadTask.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Dispose(Items[i]);
  inherited Clear;
end;

function TDownloadTask.Done: Boolean;
var
  i: Integer;
begin
  Result := Count > 0;
  for i := 0 to Count - 1 do
      Result := Result and Items[i]^.Done;
end;

procedure TP2PVM_Session.autoP2PVM_Done(Sender: TCommunicationFramework; P_IO: TPeerIO);
begin
  // 该事件是在所有的p2pVM握手完成时触发

  // 建立双通道
  logic.TunnelLinkP(procedure(const lState: Boolean)
    begin
      if lState then
          DoStatus('双通道链接完成.');
    end);
end;

procedure TP2PVM_Session.Download_Backcall(const UserData: Pointer; const UserObject: TCoreClassObject;
const FileName: SystemString; const StartPos, EndPos: Int64; const DataPtr: Pointer; const DataSize: Int64; const MD5: TMD5);
var
  p: PTaskData;
begin
  p := UserData;
  p^.Data.Clear;
  p^.Data.WritePtr(DataPtr, DataSize);
  p^.Data.Position := 0;
  p^.Done := True;
  p^.MD5 := MD5;

  Inc(total, DataSize);

  // 避免让服务器把文件全部暂存到内存
  // 这里的机制为,完成一个碎片块以后,再请求下载另一个碎片块
  if queue.Count > 0 then
    begin
      p := queue[0];
      queue.Delete(0);
      // GetFileFragmentDataM为原子功能,实现了远程文件的数据碎片下载
      logic.GetFileFragmentDataM(p^.FileName, p^.StartPos, p^.EndPos, p, nil, Download_Backcall);
    end;
end;

constructor TP2PVM_Session.Create;
begin
  inherited Create;
  // 这一部分参看AutomatedP2PVM相关demo
  logic_recv := TCommunicationFrameworkWithP2PVM_Client.Create;
  logic_send := TCommunicationFrameworkWithP2PVM_Client.Create;
  logic_recv.QuietMode := True;
  logic_send.QuietMode := True;
  logic := TCommunicationFramework_DoubleTunnelClient_NoAuth.Create(logic_recv, logic_send);
  logic.RegisterCommand;

  phyCli := TPhysicsClient.Create;
  phyCli.QuietMode := True;
  phyCli.AutomatedP2PVMClientBind.AddClient(logic_recv, '::', 98);
  phyCli.AutomatedP2PVMClientBind.AddClient(logic_send, '::', 99);
  phyCli.AutomatedP2PVMClient := True;
  phyCli.AutomatedP2PVMAuthToken := '123456';

  // AutomatedP2PVM 在链接成功后,会使用使用p2pVM握手,该事件是在所有的p2pVM握手完成时触发
  phyCli.OnAutomatedP2PVMClientConnectionDone_M := autoP2PVM_Done;

  total := 0;
  queue := TDownloadTask_.Create;
end;

destructor TP2PVM_Session.Destroy;
begin
  DisposeObject([logic, logic_recv, logic_send, phyCli]);
  DisposeObject(queue);
  inherited Destroy;
end;

procedure TP2PVM_Session.Progress;
begin
  phyCli.Progress;
  logic.Progress;
end;

procedure TP2PVM_Session.Connect(Host: SystemString);
begin
  phyCli.AsyncConnectP(Host, 9799, procedure(const cState: Boolean)
    begin
    end);
end;

procedure TP2PVM_Session.Download(p: PTaskData);
begin
  if not phyCli.Connected then
      exit;

  // 避免让服务器把文件全部暂存到内存
  // 这里的机制为,完成一个碎片块以后,再请求下载另一个碎片块
  if logic_send.QueueCmdCount = 0 then
    // GetFileFragmentDataM为原子功能,实现了远程文件的数据碎片下载
      logic.GetFileFragmentDataM(p^.FileName, p^.StartPos, p^.EndPos, p, nil, Download_Backcall)
  else
      queue.Add(p);
end;

constructor TSessionList.Create;
begin
  inherited Create;
  id := 0;
end;

destructor TSessionList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TSessionList.Remove(obj: TP2PVM_Session);
begin
  DisposeObject(obj);
  inherited Remove(obj);
end;

procedure TSessionList.Delete(index: Integer);
begin
  if (index >= 0) and (index < Count) then
    begin
      DisposeObject(Items[index]);
      inherited Delete(index);
    end;
end;

procedure TSessionList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      DisposeObject(Items[i]);
  inherited Clear;
end;

procedure TSessionList.Progress;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      Items[i].Progress;
end;

function TSessionList.Pick_NextSession: TP2PVM_Session;
begin
  if Count = 0 then
      exit(nil);

  if id >= Count then
      id := 0;

  Result := Items[id];
  Inc(id);
end;

procedure TFragmentClientForm.checkDownTimerTimer(Sender: TObject);
var
  i: Integer;
begin
  if (DownloadTask.Count > 0) and (DownloadTask.Done) then
    begin
      DoStatus('完成碎片下载任务.');

      // 合并碎片
      for i := 0 to DownloadTask.Count - 1 do
        begin
          targetStream.Position := DownloadTask[i]^.StartPos;
          targetStream.CopyFrom(DownloadTask[i]^.Data, DownloadTask[i]^.Data.Size);
        end;

      DoStatus(demoFile + ' 本地文件md5: ' + umlStreamMD5String(targetStream));
      DownloadTask.Clear;
    end;

  stateMemo.Lines.BeginUpdate;
  stateMemo.Lines.Clear;
  for i := 0 to Session.Count - 1 do
      stateMemo.Lines.Add(Format('连接%d 下载:%d 下行流量:%d 上行流量:%d', [i + 1, Session[i].total,
      Session[i].phyCli.Statistics[stReceiveSize], Session[i].phyCli.Statistics[stSendSize]]));
  stateMemo.Lines.EndUpdate;
end;

procedure TFragmentClientForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(Self, DoStatus_Backcall);
  Session := TSessionList.Create;
  DownloadTask := TDownloadTask.Create;
  targetStream := TStream64.Create;
end;

procedure TFragmentClientForm.FormDestroy(Sender: TObject);
begin
  DisposeObject(Session);
  DisposeObject(DownloadTask);
  DisposeObject(targetStream);
end;

procedure TFragmentClientForm.downloadButtonClick(Sender: TObject);
begin
  if Session.Count = 0 then
      exit;

  // 1,先获取远程文件大小
  Session.First.logic.GetFileInfoP(demoFile, nil, nil,
    procedure(const UserData: Pointer; const UserObject: TCoreClassObject;
      const FileName: SystemString; const Existed: Boolean; const fSiz: Int64)
    var
      i: Integer;
      step: Int64;
      p: PTaskData;
    begin
      if not Existed then
          exit;

      targetStream.Size := fSiz;

      // 2,构建多线下载任务
      DownloadTask.Clear;
      step := 0;
      while step + bloackSize < fSiz do
        begin
          new(p);
          p^.Init;
          p^.FileName := FileName;
          p^.StartPos := step;
          p^.EndPos := step + bloackSize;
          DownloadTask.Add(p);
          Inc(step, bloackSize);
        end;
      if step < fSiz then
        begin
          new(p);
          p^.Init;
          p^.FileName := FileName;
          p^.StartPos := step;
          p^.EndPos := fSiz;
          DownloadTask.Add(p);
        end;

      // 3,计算远程文件的md5用于验证下载正确性
      Session.First.logic.GetFileMD5P(FileName, 0, fSiz, nil, nil,
        procedure(const UserData: Pointer; const UserObject: TCoreClassObject;
          const FileName: SystemString; const StartPos, EndPos: Int64; const MD5: UnicodeMixedLib.TMD5)
        begin
          DoStatus(FileName + ' 远程文件md5: ' + umlMD5ToStr(MD5));
        end);

      // 4,逐任务添加到下载队列
      for i := 0 to DownloadTask.Count - 1 do
          Session.Pick_NextSession.Download(DownloadTask[i]);
    end);
end;

procedure TFragmentClientForm.connButtonClick(Sender: TObject);
var
  i: Integer;
  p: TP2PVM_Session;
begin
  for i := 0 to 10 - 1 do
    begin
      p := TP2PVM_Session.Create;
      p.Connect(HostEdit.Text);
      Session.Add(p);
    end;
end;

procedure TFragmentClientForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  DeleteDoStatusHook(Self);
end;

procedure TFragmentClientForm.Timer1Timer(Sender: TObject);
begin
  Session.Progress;
  DoStatus();
  CheckThreadSynchronize();
end;

procedure TFragmentClientForm.DoStatus_Backcall(Text_: SystemString; const id: Integer);
begin
  Memo.Lines.Add(Text_);
end;

end.
