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
  MaxClient = 10;

type
  TMyClient = class(TCommunicationFrameworkWithP2PVM_Client)
  protected
  end;

  TClientArry = array [0 .. MaxClient - 1] of TMyClient;
  TTestArry   = array [0 .. MaxClient - 1] of TCommunicationTestIntf;

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
    procedure CreateVMButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ProgressTimerTimer(Sender: TObject);
    procedure ConnectVMButtonClick(Sender: TObject);
    procedure TestButtonClick(Sender: TObject);
    procedure DisconnectButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PrintStateTimerTimer(Sender: TObject);
  private
    procedure DoStatusMethod(AText: SystemString; const ID: Integer);
    { Private declarations }
  public
    { Public declarations }
    ClientTunnel    : TCommunicationFramework_Client_CrossSocket;
    ClientWithVM    : TClientArry;
    ClientWithVMTest: TTestArry;

    procedure cmd_SimulateKeepAlivte(Sender: TPeerClient; InData: TDataFrameEngine);
  end;

var
  VMCliForm: TVMCliForm;

implementation

{$R *.dfm}


procedure TVMCliForm.DisconnectButtonClick(Sender: TObject);
var
  i: Integer;
begin
  for i := low(ClientWithVM) to high(ClientWithVM) do
      ClientWithVM[i].Disconnect;
end;

procedure TVMCliForm.DoStatusMethod(AText: SystemString; const ID: Integer);
begin
  if StatusCheckBox.Checked then
      Memo.Lines.Add(AText);
end;

procedure TVMCliForm.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  AddDoStatusHook(Self, DoStatusMethod);
  ClientTunnel := TCommunicationFramework_Client_CrossSocket.Create;
  ClientTunnel.QuietMode := True;
  ClientTunnel.RegisterDirectStream('SimulateKeepAlivte').OnExecute := cmd_SimulateKeepAlivte;

  for i := low(ClientWithVM) to high(ClientWithVM) do
    begin
      ClientWithVM[i] := TMyClient.Create(i + 99);
      ClientWithVM[i].SwitchMaxPerformance;
      ClientWithVMTest[i] := TCommunicationTestIntf.Create;
      ClientWithVMTest[i].RegCmd(ClientWithVM[i]);
    end;
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

procedure TVMCliForm.PrintStateTimerTimer(Sender: TObject);
  procedure PrintServerState(var arry: TClientArry);
  var
    buff: array [TStatisticsType] of Int64;
    comm: TCommunicationFramework;
    st  : TStatisticsType;
    i   : Integer;
    v   : Int64;
    n   : string;
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

procedure TVMCliForm.ProgressTimerTimer(Sender: TObject);
var
  i                                    : Integer;
  TotalCli, connectingCli, ConnectedCli: Integer;
begin
  ClientTunnel.ProgressBackground;

  for i := low(ClientWithVM) to high(ClientWithVM) do
      ClientWithVM[i].ProgressBackground;

  TotalCli := 0;
  connectingCli := 0;
  ConnectedCli := 0;

  if ClientTunnel.Connected then
    if ClientTunnel.ClientIO.p2pVMTunnel <> nil then
        ClientTunnel.ClientIO.p2pVMTunnel.ProgressCommunicationFramework(procedure(PeerFramework: TCommunicationFramework)
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

procedure TVMCliForm.TestButtonClick(Sender: TObject);
var
  i: Integer;
begin
  if not ClientTunnel.Connected then
      exit;
  for i := low(ClientWithVM) to high(ClientWithVM) do
      ClientWithVMTest[i].ExecuteAsyncTest(ClientWithVM[i].ClientIO);
end;

procedure TVMCliForm.ConnectVMButtonClick(Sender: TObject);
var
  i: Integer;
begin
  if not ClientTunnel.Connected then
      exit;
  ClientTunnel.ClientIO.p2pVMTunnel.CloseAllClientIO;

  for i := low(ClientWithVM) to high(ClientWithVM) do
    begin
      ClientWithVM[i].AsyncConnectTimeout := 5 * 60 * 1000;
      ClientWithVM[i].AsyncConnect(VMAddrEdit.Text, 11139);
    end;
end;

procedure TVMCliForm.CreateVMButtonClick(Sender: TObject);
begin
  // VM.CloseP2PVMTunnel;
  ClientTunnel.AsyncConnect(AddrEdit.Text, 9988, procedure(const cState: Boolean)
    var
      i: Integer;
    begin
      DoStatus('VM隧道已链接...');
      if cState then
        begin
          DoStatus('VM正在握手...');
          ClientTunnel.ClientIO.OpenP2PVMTunnel(10000 * 10, True);
          ClientTunnel.ClientIO.p2pVMTunnel.MaxRealBuffer := 16 * 1024 * 1024;
          ClientTunnel.ClientIO.p2pVMTunnel.QuietMode := False;

          for i := low(ClientWithVM) to high(ClientWithVM) do
            begin
              ClientWithVM[i].QuietMode := False;
              ClientTunnel.ClientIO.p2pVMTunnel.InstallLogicFramework(ClientWithVM[i]);
            end;

          DoStatus('VM已握手');
        end;
    end);
end;

procedure TVMCliForm.cmd_SimulateKeepAlivte(Sender: TPeerClient; InData: TDataFrameEngine);
begin
  OriginDataLabel.Caption := Format('心跳包:%s', [InData.Reader.ReadString]);
end;

end.
