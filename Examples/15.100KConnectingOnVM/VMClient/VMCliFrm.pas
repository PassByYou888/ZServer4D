unit VMCliFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, System.TypInfo,
  CommunicationFramework,
  CommunicationFrameworkIO,
  CommunicationFramework_Server_ICS,
  CommunicationFramework_Server_Indy,
  CommunicationFramework_Server_CrossSocket,
  CommunicationFramework_Client_ICS,
  CommunicationFramework_Client_Indy,
  CommunicationFramework_Client_CrossSocket,
  PascalStrings, DoStatusIO, CoreClasses, DataFrameEngine, UnicodeMixedLib, MemoryStream64,
  NotifyObjectBase, CommunicationTest, ListEngine;

const
  MaxClient = 50000;

type
  TMyClient = class(TCommunicationFrameworkWithP2PVM_Client)
  protected
  end;

  TClientArry = array [0 .. MaxClient - 1] of TMyClient;
  TTestArry = array [0 .. MaxClient - 1] of TCommunicationTestIntf;

  TVMCliForm = class(TForm)
    ProgressTimer: TTimer;
    Panel1: TPanel;
    AddrEdit: TLabeledEdit;
    CreateVMButton: TButton;
    VMAddrEdit: TLabeledEdit;
    ConnectVMButton: TButton;
    TestButton: TButton;
    DisconnectButton: TButton;
    Memo: TMemo;
    StateMemo: TMemo;
    PrintStateTimer: TTimer;
    StatusCheckBox: TCheckBox;
    OriginDataLabel: TLabel;
    MaxTestButton: TButton;
    procedure CreateVMButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ProgressTimerTimer(Sender: TObject);
    procedure ConnectVMButtonClick(Sender: TObject);
    procedure TestButtonClick(Sender: TObject);
    procedure DisconnectButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PrintStateTimerTimer(Sender: TObject);
    procedure MaxTestButtonClick(Sender: TObject);
  private
    procedure DoStatusMethod(AText: SystemString; const ID: Integer);
    { Private declarations }
  public
    { Public declarations }
    ClientTunnel: TCommunicationFramework_Client_ICS;
    ClientWithVM: TClientArry;
    ClientWithVMTest: TTestArry;

    procedure cmd_SimulateKeepAlivte(Sender: TPeerIO; InData: TDataFrameEngine);
  end;

var
  VMCliForm: TVMCliForm;

implementation

{$R *.dfm}


procedure TVMCliForm.CreateVMButtonClick(Sender: TObject);
begin
  // VM.CloseP2PVMTunnel;
  ClientTunnel.AsyncConnectP(AddrEdit.Text, 9988, procedure(const cState: Boolean)
    begin
      DoStatus('VM隧道已链接...');
      if cState then
        begin
          DoStatus('VM正在握手...');
          ClientTunnel.ClientIO.BuildP2PAuthTokenP(procedure
            begin
              ClientTunnel.ClientIO.OpenP2PVMTunnelP(10000 * 10, True, '',
                procedure(const vState: Boolean)
                var
                  i: Integer;
                begin
                  ClientTunnel.ClientIO.p2pVMTunnel.MaxVMFragmentSize := 8192;
                  ClientTunnel.ClientIO.p2pVMTunnel.MaxRealBuffer := 8 * 1024 * 1024;
                  ClientTunnel.ClientIO.p2pVMTunnel.QuietMode := False;

                  for i := low(ClientWithVM) to high(ClientWithVM) do
                    begin
                      ClientWithVM[i].QuietMode := False;
                      ClientTunnel.ClientIO.p2pVMTunnel.InstallLogicFramework(ClientWithVM[i]);
                    end;

                  DoStatus('VM已握手');
                end);
            end);
        end;
    end);
end;

procedure TVMCliForm.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  AddDoStatusHook(Self, DoStatusMethod);
  ClientTunnel := TCommunicationFramework_Client_ICS.Create;
  ClientTunnel.QuietMode := True;
  ClientTunnel.RegisterDirectStream('SimulateKeepAlivte').OnExecute := cmd_SimulateKeepAlivte;

  for i := low(ClientWithVM) to high(ClientWithVM) do
    begin
      ClientWithVM[i] := TMyClient.CustomCreate(i + 99);
      ClientWithVM[i].SwitchMaxPerformance;
      ClientWithVMTest[i] := TCommunicationTestIntf.Create;
      ClientWithVMTest[i].RegCmd(ClientWithVM[i]);
    end;
end;

procedure TVMCliForm.ProgressTimerTimer(Sender: TObject);
var
  i: Integer;
  TotalCli, connectingCli, ConnectedCli: Integer;
begin
  ClientTunnel.Progress;

  for i := low(ClientWithVM) to high(ClientWithVM) do
      ClientWithVM[i].Progress;

  TotalCli := 0;
  connectingCli := 0;
  ConnectedCli := 0;

  if ClientTunnel.Connected then
    if ClientTunnel.ClientIO.p2pVMTunnel <> nil then
        ClientTunnel.ClientIO.p2pVMTunnel.ProgressCommunicationFrameworkP(procedure(PeerFramework: TCommunicationFramework)
        begin
          if (PeerFramework is TCommunicationFrameworkWithP2PVM_Client) then
            begin
              inc(TotalCli);

              if TCommunicationFrameworkWithP2PVM_Client(PeerFramework).RemoteInited then
                  inc(ConnectedCli)
              else if TCommunicationFrameworkWithP2PVM_Client(PeerFramework).Connected then
                  inc(connectingCli);
            end;
        end);

  Caption := Format('VM客户端(%d)...半开链接中(%d) 链接完成(%d)', [TotalCli, connectingCli, ConnectedCli]);
end;

procedure TVMCliForm.ConnectVMButtonClick(Sender: TObject);
var
  i: Integer;
  cCount: Integer;
begin
  if not ClientTunnel.Connected then
      exit;

  StatusCheckBox.Checked := False;

  ClientTunnel.ClientIO.p2pVMTunnel.CloseAllClientIO;
  ClientTunnel.ClientIO.Progress;

  for i := low(ClientWithVM) to high(ClientWithVM) do
    begin
      ClientWithVM[i].AsyncConnectTimeout := 10 * 60 * 1000;
      ClientWithVM[i].AsyncConnect(VMAddrEdit.Text, 11139);
    end;
end;

procedure TVMCliForm.TestButtonClick(Sender: TObject);
var
  i: Integer;
begin
  if not ClientTunnel.Connected then
      exit;
  for i := low(ClientWithVM) to high(ClientWithVM) do
      ClientWithVMTest[i].ExecuteAsyncTest(ClientWithVM[i].ClientIO);
end;

procedure TVMCliForm.DisconnectButtonClick(Sender: TObject);
var
  i: Integer;
begin
  for i := low(ClientWithVM) to high(ClientWithVM) do
      ClientWithVM[i].Disconnect;
end;

procedure TVMCliForm.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  DeleteDoStatusHook(Self);
  for i := low(ClientWithVM) to high(ClientWithVM) do
    begin
      DisposeObject(ClientWithVMTest[i]);
      DisposeObject(ClientWithVM[i]);
    end;
  DisposeObject(ClientTunnel);
end;

procedure TVMCliForm.MaxTestButtonClick(Sender: TObject);
var
  i: Integer;
begin
  if not ClientTunnel.Connected then
      exit;
  for i := low(ClientWithVM) to high(ClientWithVM) do
      ClientWithVMTest[i].ExecuteAsyncTestWithBigStream(ClientWithVM[i].ClientIO);
end;

procedure TVMCliForm.PrintStateTimerTimer(Sender: TObject);
  procedure PrintServerState(var arry: TClientArry);
  var
    buff: array [TStatisticsType] of Int64;
    comm: TCommunicationFramework;
    st: TStatisticsType;
    i: Integer;
    v: Int64;
    n: string;
  begin
    for st := low(TStatisticsType) to high(TStatisticsType) do
        buff[st] := 0;

    for comm in arry do
      begin
        for st := low(TStatisticsType) to high(TStatisticsType) do
            buff[st] := buff[st] + comm.Statistics[st];
      end;

    StateMemo.Lines.BeginUpdate;
    StateMemo.Lines.Clear;
    StateMemo.Lines.Add('Statistics...');
    for st := low(TStatisticsType) to high(TStatisticsType) do
      begin
        v := buff[st];
        if v > 8192 then
            n := umlSizeToStr(v).Text
        else
            n := IntToStr(v);
        StateMemo.Lines.Add(GetEnumName(TypeInfo(TStatisticsType), Ord(st)) + ' : ' + n);
      end;
    StateMemo.Lines.EndUpdate;
  end;

begin
  PrintServerState(ClientWithVM);
end;

procedure TVMCliForm.DoStatusMethod(AText: SystemString; const ID: Integer);
begin
  if StatusCheckBox.Checked then
      Memo.Lines.Add(AText);
end;

procedure TVMCliForm.cmd_SimulateKeepAlivte(Sender: TPeerIO; InData: TDataFrameEngine);
begin
  OriginDataLabel.Caption := Format('心跳包:%s', [InData.Reader.ReadString]);
end;

end.
