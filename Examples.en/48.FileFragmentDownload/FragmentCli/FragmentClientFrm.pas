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
  {  Multi line download is sorted out with standard data structure, and the idea is simple and clear  }

  {  Taskdata is the preset download parameter of each fragment, including remote file name, fragment start location, end location, fragment MD5 verification code, state machine, etc  }
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

  {  Generic taskdata linked list  }
  TDownloadTask_ = TGenericsList<PTaskData>;

  {  By default, the generic linked list will not be automatically released. An automatic release process is required here  }
  {  Instead of releasing the state machine, the method is compatible with the generic mechanism of FPC. The linked list of FPC does not trigger the event when it is released  }
  TDownloadTask = class(TDownloadTask_)
  public
    destructor Destroy; override;
    procedure Remove(p: PTaskData);
    procedure Delete(index: Integer);
    procedure Clear;
    function Done: Boolean;
  end;

  {  Define multiple links with the concept of session  }
  {  The multiple links here use p2pvm  }
  {  The p2pvm construction method here is automated p2pvm with minimalist mechanism  }
  TP2PVM_Session = class
  private
    {  The automated P2P VM support technology is used here. Please refer to the relevant demo for details  }
    {  Logic starts with virtual connections and dual channels  }
    logic_recv, logic_send: TCommunicationFrameworkWithP2PVM_Client;
    logic: TCommunicationFramework_DoubleTunnelClient_NoAuth;
    {  Phycli is a physical connection  }
    phyCli: TPhysicsClient;
    {  File stream download count  }
    total: Int64;
    {  Temporary staging queue for fragment download requests  }
    queue: TDownloadTask_;
    procedure autoP2PVM_Done(Sender: TCommunicationFramework; P_IO: TPeerIO);
    procedure Download_Backcall(const UserData: Pointer; const UserObject: TCoreClassObject;
      const FileName: SystemString; const StartPos, EndPos: Int64; const DataPtr: Pointer; const DataSize: Int64; const MD5: TMD5);
  public
    {  Automatedp2pvm is implemented in the create constructor  }
    constructor Create;
    destructor Destroy; override;
    procedure Progress;
    procedure Connect(Host: SystemString);
    procedure Download(p: PTaskData);
  end;

  TSessionList_ = TGenericsList<TP2PVM_Session>;

  {  By default, the generic linked list will not be automatically released. An automatic release process is required here  }
  {  Instead of releasing the state machine, the method is compatible with the generic mechanism of FPC. The linked list of FPC does not trigger the event when it is released  }
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
  demoFile = 'FragmentServiceDemo.exe'; {  Remote file name to download  }
  bloackSize = 8192;                    {  Block download size  }

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
  {  This event is triggered when all p2pvm handshakes are completed  }

  {  Establish dual channels  }
  logic.TunnelLinkP(procedure(const lState: Boolean)
    begin
      if lState then
          DoStatus('Dual channel link complete');
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

  {  Avoid allowing the server to temporarily store all files in memory  }
  {  The mechanism here is to request to download another fragment block after completing one fragment block  }
  if queue.Count > 0 then
    begin
      p := queue[0];
      queue.Delete(0);
      {  Getfilefragmentdatam is an atomic function, which realizes the data fragment download of remote files  }
      logic.GetFileFragmentDataM(p^.FileName, p^.StartPos, p^.EndPos, p, nil, Download_Backcall);
    end;
end;

constructor TP2PVM_Session.Create;
begin
  inherited Create;
  {  For this part, please refer to the relevant demo of automated P2P VM  }
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

  {  After the link is successful, the automated p2pvm will use the p2pvm handshake. This event is triggered when all p2pvm handshakes are completed  }
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

  {  Avoid allowing the server to temporarily store all files in memory  }
  {  The mechanism here is to request to download another fragment block after completing one fragment block  }
  if logic_send.QueueCmdCount = 0 then
    {  Getfilefragmentdatam is an atomic function, which realizes the data fragment download of remote files  }
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
      DoStatus('Complete the fragment download task');

      {  Merge fragments  }
      for i := 0 to DownloadTask.Count - 1 do
        begin
          targetStream.Position := DownloadTask[i]^.StartPos;
          targetStream.CopyFrom(DownloadTask[i]^.Data, DownloadTask[i]^.Data.Size);
        end;

      DoStatus(demoFile + 'Local file MD5:' + umlStreamMD5String(targetStream));
      DownloadTask.Clear;
    end;

  stateMemo.Lines.BeginUpdate;
  stateMemo.Lines.Clear;
  for i := 0 to Session.Count - 1 do
      stateMemo.Lines.Add(Format('Connection %d Download: %d downlink traffic: %d uplink traffic: %d', [i + 1, Session[i].total,
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

  {  1. Get the remote file size first  }
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

      {  2. Build multi line download task  }
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

      {  3. Calculate the MD5 of the remote file to verify the correctness of the download  }
      Session.First.logic.GetFileMD5P(FileName, 0, fSiz, nil, nil,
        procedure(const UserData: Pointer; const UserObject: TCoreClassObject;
          const FileName: SystemString; const StartPos, EndPos: Int64; const MD5: UnicodeMixedLib.TMD5)
        begin
          DoStatus(FileName + 'Remote file MD5:' + umlMD5ToStr(MD5));
        end);

      {  4. Add to the download queue task by task  }
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
