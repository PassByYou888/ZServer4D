unit InternetCliFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,

  ZS_JsonDataObjects,
  CoreClasses,
  PascalStrings,
  DoStatusIO,
  UnicodeMixedLib,
  MemoryStream64,
  ListEngine,
  DataFrameEngine,
  NotifyObjectBase,
  CommunicationFramework,
  PhysicsIO,
  CommunicationFrameworkDoubleTunnelIO_NoAuth;

type
  TInternetCliForm = class(TForm)
    HostEdit: TLabeledEdit;
    goButton: TButton;
    Memo: TMemo;
    fpsTimer: TTimer;
    Label1: TLabel;
    newCardButton: TButton;
    DBNameEdit: TLabeledEdit;
    QueryButton: TButton;
    procedure newCardButtonClick(Sender: TObject);
    procedure fpsTimerTimer(Sender: TObject);
    procedure goButtonClick(Sender: TObject);
    procedure QueryButtonClick(Sender: TObject);
  private
    procedure DoStatus_Backcall(Text_: SystemString; const ID: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  // 服务器可以直接写实例，但是客户端最好封装一下，养成习惯，否则项目堆大以后维护很麻烦
  TOnSaveResult = reference to procedure(DataStorePos: Int64);
  TOnSaveStateResult = reference to procedure(DataStoreArry: TArrayInt64);

  TInternetClient = class(TCommunicationFramework_DoubleTunnelClient_NoAuth)
  public
    procedure MyAutoSave(dbName: U_String; dataID: Cardinal; stream: TMS64; OnResult: TOnSaveResult); overload;
    procedure MyAutoSave(dbName: U_String; dataID: Cardinal; stream: TMS64); overload;
    procedure MyBeginSave();
    procedure MySave(dbName: U_String; dataID: Cardinal; stream: TMS64);
    procedure MySaveState(OnResult: TOnSaveStateResult);
    procedure MyEndSave();
    procedure MyQuery(dbName: U_String; Reverse: Boolean; MaxResult: Int64; VL: THashVariantList; OnResult: TStreamMethod);
    procedure MyDownload(dbName: U_String; StorePos: Int64; OnResult: TStreamMethod);
    procedure MyDelete(dbName: U_String; StorePos: Int64);
  end;

const
  IsQuiet: Boolean = False;

var
  InternetCliForm: TInternetCliForm;
  InternetPhyCli: TPhysicsClient;
  InternetRecvTunnel, InternetSendTunnel: TCommunicationFrameworkWithP2PVM_Client;
  InternetClient: TInternetClient;
  InternetConnecting: Boolean;

procedure InitInternetCli;

implementation

{$R *.dfm}


uses CardFrm, QueryFrm;

procedure TInternetClient.MyAutoSave(dbName: U_String; dataID: Cardinal; stream: TMS64; OnResult: TOnSaveResult);
var
  d: TDFE;
begin
  d := TDFE.Create;
  d.WriteString(dbName);
  d.WriteCardinal(dataID);
  d.WriteStream(stream);
  SendTunnel.SendStreamCmdP('MyAutoSave', d, procedure(Sender: TPeerIO; ResultData: TDataFrameEngine)
    var
      tmp: TDFE;
      info: TBigStreamBatchPostData;
    begin
      if ResultData.Count = 1 then
        begin
          tmp := TDFE.Create;
          ResultData.ReadDataFrame(0, tmp);
          info.Init;
          info.Decode(tmp);
          if Assigned(OnResult) then
              OnResult(info.DBStorePos);
          tmp.Free;
        end
      else
          DoStatus('MyAutoSave Result error!');
    end);
  d.Free;
end;

procedure TInternetClient.MyAutoSave(dbName: U_String; dataID: Cardinal; stream: TMS64);
begin
  MyAutoSave(dbName, dataID, stream, nil);
end;

procedure TInternetClient.MyBeginSave;
begin
  SendTunnel.SendDirectStreamCmd('MyBeginSave');
end;

procedure TInternetClient.MySave(dbName: U_String; dataID: Cardinal; stream: TMS64);
var
  d: TDFE;
begin
  d := TDFE.Create;
  d.WriteString(dbName);
  d.WriteCardinal(dataID);
  d.WriteStream(stream);
  SendTunnel.SendDirectStreamCmd('MySave', d);
  d.Free;
end;

procedure TInternetClient.MySaveState(OnResult: TOnSaveStateResult);
begin
  SendTunnel.SendStreamCmdP('MySaveState', nil, procedure(Sender: TPeerIO; ResultData: TDataFrameEngine)
    var
      tmp: TDFE;
      info: TBigStreamBatchPostData;
      DataStoreArry: TArrayInt64;
      i: Integer;
    begin
      SetLength(DataStoreArry, ResultData.Count);
      for i := 0 to ResultData.Count - 1 do
        begin
          tmp := TDFE.Create;
          ResultData.ReadDataFrame(i, tmp);
          info.Init;
          info.Decode(tmp);
          DataStoreArry[i] := info.DBStorePos;
          tmp.Free;
        end;
      if Assigned(OnResult) then
          OnResult(DataStoreArry);
      SetLength(DataStoreArry, 0);
    end);
end;

procedure TInternetClient.MyEndSave;
begin
  SendTunnel.SendDirectStreamCmd('MyEndSave');
end;

procedure TInternetClient.MyQuery(dbName: U_String; Reverse: Boolean; MaxResult: Int64; VL: THashVariantList; OnResult: TStreamMethod);
var
  d: TDFE;
begin
  d := TDFE.Create;
  d.WriteString(dbName);
  d.WriteBool(Reverse);
  d.WriteInt64(MaxResult);
  d.WriteVariantList(VL);
  SendTunnel.SendStreamCmdM('MyQuery', d, OnResult);
  d.Free;
end;

procedure TInternetClient.MyDownload(dbName: U_String; StorePos: Int64; OnResult: TStreamMethod);
var
  d: TDFE;
begin
  d := TDFE.Create;
  d.WriteString(dbName);
  d.WriteInt64(StorePos);
  SendTunnel.SendStreamCmdM('MyDownload', d, OnResult);
  d.Free;
end;

procedure TInternetClient.MyDelete(dbName: U_String; StorePos: Int64);
var
  d: TDFE;
begin
  d := TDFE.Create;
  d.WriteString(dbName);
  d.WriteInt64(StorePos);
  SendTunnel.SendDirectStreamCmd('MyDelete', d);
  d.Free;
end;

procedure InitInternetCli;
begin
  InternetRecvTunnel := TCommunicationFrameworkWithP2PVM_Client.Create;
  InternetRecvTunnel.QuietMode := IsQuiet;

  InternetSendTunnel := TCommunicationFrameworkWithP2PVM_Client.Create;
  InternetSendTunnel.QuietMode := IsQuiet;

  InternetClient := TInternetClient.Create(InternetRecvTunnel, InternetSendTunnel);
  InternetClient.RegisterCommand;

  InternetPhyCli := TPhysicsClient.Create;
  InternetPhyCli.AutomatedP2PVMClientBind.AddClient(InternetSendTunnel, '::', 111);
  InternetPhyCli.AutomatedP2PVMClientBind.AddClient(InternetRecvTunnel, '::', 112);
  InternetPhyCli.AutomatedP2PVMAuthToken := 'internet';

  InternetConnecting := False;
end;

procedure ConnectToInternetService_TunnelStateEvent(const cState: Boolean);
begin
  if cState then
      DoStatus('connection for Internet done.', []);
  InternetConnecting := False;
end;

procedure ConnectToInternetPhysicsService_DoneEvent(Sender: TCommunicationFramework; P_IO: TPeerIO);
begin
  InternetClient.TunnelLinkC(ConnectToInternetService_TunnelStateEvent);
end;

procedure ConnectToInternetPhysicsService_StateEvent(const cState: Boolean);
begin
  if not cState then
      InternetConnecting := False;
end;

procedure DoDelayConnectToInternetService(host: U_String);
begin
  InternetPhyCli.OnAutomatedP2PVMClientConnectionDone_C := ConnectToInternetPhysicsService_DoneEvent;
  InternetPhyCli.AsyncConnectC(host, 11088, ConnectToInternetPhysicsService_StateEvent);
end;

procedure TInternetCliForm.DoStatus_Backcall(Text_: SystemString; const ID: Integer);
begin
  Memo.Lines.Add(Text_);
end;

constructor TInternetCliForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AddDoStatusHook(Self, DoStatus_Backcall);
  InitInternetCli;
end;

destructor TInternetCliForm.Destroy;
begin
  DeleteDoStatusHook(Self);
  inherited Destroy;
end;

procedure TInternetCliForm.newCardButtonClick(Sender: TObject);
begin
  if InternetClient.LinkOk then
      CardForm.Show;
end;

procedure TInternetCliForm.fpsTimerTimer(Sender: TObject);
begin
  InternetClient.Progress;
  InternetPhyCli.Progress;
  CheckThreadSynchronize;
end;

procedure TInternetCliForm.goButtonClick(Sender: TObject);
begin
  DoDelayConnectToInternetService(HostEdit.Text);
end;

procedure TInternetCliForm.QueryButtonClick(Sender: TObject);
begin
  if InternetClient.LinkOk then
      QueryForm.Show;
end;

end.
