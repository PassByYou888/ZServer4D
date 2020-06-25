unit DBStoreServiceFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, System.Math,
  UnicodeMixedLib,
  CommunicationFrameworkDoubleTunnelIO,
  CommunicationFrameworkDoubleTunnelIO_NoAuth, DoStatusIO,
  CommunicationFrameworkDataStoreService, ZDBEngine, DataFrameEngine,
  ZS_JsonDataObjects, CommunicationFramework_Client_ICS,
  CommunicationFramework_Server_ICS,
  CommunicationFramework_Server_ICSCustomSocket, CommunicationFramework,
  CommunicationFramework_Server_CrossSocket,
  CommunicationFramework_Server_Indy, CoreClasses, ZDBLocalManager,
  CommunicationFrameworkDataStoreService_NoAuth,
  CommunicationFrameworkDataStoreServiceCommon;

type
  TDBStoreServiceForm = class(TForm)
    Memo: TMemo;
    ProgressTimer: TTimer;
    Timer1: TTimer;
    ListBox: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ProgressTimerTimer(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    RecvTunnel, SendTunnel: TCommunicationFrameworkServer;
    DataStoreService      : TDataStoreService_NoAuth;
  public
    { Public declarations }

    procedure Query_G300(dPipe: TZDBPipeline; var qState: TQueryState; var Allowed: Boolean);
    procedure Query_G700(dPipe: TZDBPipeline; var qState: TQueryState; var Allowed: Boolean);
    procedure DoStatusNear(AText: string; const ID: Integer);
  end;

var
  DBStoreServiceForm: TDBStoreServiceForm;

implementation

{$R *.dfm}


procedure TDBStoreServiceForm.Query_G300(dPipe: TZDBPipeline; var qState: TQueryState; var Allowed: Boolean);
begin
  if qState.IsDF then
      Allowed := InRange(qState.Eng.GetDF(qState).ReadDouble(0), -300.0, 300.0);
end;

procedure TDBStoreServiceForm.Query_G700(dPipe: TZDBPipeline; var qState: TQueryState; var Allowed: Boolean);
begin
  if qState.IsDF then
      Allowed := InRange(qState.Eng.GetDF(qState).ReadDouble(0), -710.0, -700.0);
end;

procedure TDBStoreServiceForm.Timer1Timer(Sender: TObject);
var
  i  : Integer;
  lst: TCoreClassListForObj;
  db : TZDBLMStore;
  pl : TTDataStoreService_DBPipeline;
  lip: string;
begin
  ListBox.Items.BeginUpdate;
  ListBox.Clear;

  i := Round(DataStoreService.PostCounterOfPerSec);

  if i > 0 then
      ListBox.Items.Add(Format('Post Data Of Per Sec:%d', [i]));

  lst := TCoreClassListForObj.Create;
  DataStoreService.ZDBLocal.GetDBList(lst);

  if lst.Count > 0 then
    begin
      ListBox.Items.Add(Format('database(%d)...', [lst.Count]));
      for i := 0 to lst.Count - 1 do
        begin
          db := TZDBLMStore(lst[i]);
          ListBox.Items.Add(Format('db: %s total items:%d size:%s %s', [db.name, db.Count, umlSizeToStr(db.DBEngine.Size).Text, db.CacheAnnealingState]));
        end;
    end;

  lst.Clear;
  DataStoreService.ZDBLocal.GetPipeList(lst);
  if lst.Count > 0 then
    begin
      ListBox.Items.Add(Format('query pipeline(%d)...', [lst.Count]));
      for i := 0 to lst.Count - 1 do
        begin
          pl := TTDataStoreService_DBPipeline(lst[i]);
          if DataStoreService.SendTunnel.Exists(pl.SendTunnel) then
              lip := pl.SendTunnel.Owner.GetPeerIP
          else
              lip := '';
          ListBox.Items.Add(Format('%s name: %s query performance:%d', [lip, pl.PipelineName, Round(pl.QueryCounterOfPerSec)]));
        end;
    end;

  DisposeObject(lst);
  ListBox.Items.EndUpdate;
end;

procedure TDBStoreServiceForm.DoStatusNear(AText: string; const ID: Integer);
begin
  Memo.Lines.Add(AText);
end;

procedure TDBStoreServiceForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(Self, DoStatusNear);
  RecvTunnel := TCommunicationFramework_Server_CrossSocket.Create;
  SendTunnel := TCommunicationFramework_Server_CrossSocket.Create;

  RecvTunnel.StartService('', 13887);
  SendTunnel.StartService('', 13888);
  DataStoreService := TDataStoreService_NoAuth.Create(RecvTunnel, SendTunnel);
  DataStoreService.RegisterCommand;
  DataStoreService.ZDBLocal.LoadDB(False);

  DataStoreService.RegisterQueryCall('G300').OnPipelineQuery := Query_G300;
  DataStoreService.RegisterQueryCall('G700').OnPipelineQuery := Query_G700;

  RecvTunnel.QuietMode := True;
  SendTunnel.QuietMode := True;
end;

procedure TDBStoreServiceForm.FormDestroy(Sender: TObject);
begin
  DeleteDoStatusHook(Self);
  DisposeObject([DataStoreService, RecvTunnel, SendTunnel]);
end;

procedure TDBStoreServiceForm.ProgressTimerTimer(Sender: TObject);
begin
  DataStoreService.Progress;
end;

end.
