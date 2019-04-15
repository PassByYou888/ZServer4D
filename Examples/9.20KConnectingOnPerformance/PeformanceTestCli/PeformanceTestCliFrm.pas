unit PeformanceTestCliFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  CommunicationFramework,
  DoStatusIO, CoreClasses,
  CommunicationFramework_Client_CrossSocket,
  CommunicationFramework_Client_ICS,
  Cadencer, DataFrameEngine, UnicodeMixedLib, CommunicationTest,
  CommunicationFramework_Client_Indy;

type
  TEZClientForm = class(TForm)
    ConnectButton: TButton;
    HostEdit: TLabeledEdit;
    Timer: TTimer;
    TestCommandButton: TButton;
    Memo: TMemo;
    disconnectButton: TButton;
    procedure ConnectButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure TestCommandButtonClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure disconnectButtonClick(Sender: TObject);
  private
    { Private declarations }
    procedure DoStatusNear(AText: string; const ID: Integer);
  public
    { Public declarations }
    client: array of TCommunicationFrameworkClient;
    test: array of TCommunicationTestIntf;
  end;

const
  MaxConn = 10000;

var
  EZClientForm: TEZClientForm;

implementation

{$R *.dfm}


procedure TEZClientForm.disconnectButtonClick(Sender: TObject);
begin
  ClientPool.CloseAllConnection;
  ConnectButton.Visible := True;
  TestCommandButton.Visible := False;
  disconnectButton.Visible := False;
end;

procedure TEZClientForm.DoStatusNear(AText: string; const ID: Integer);
begin
end;

procedure TEZClientForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  ClientPool.CloseAllConnection;
end;

procedure TEZClientForm.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  AddDoStatusHook(self, DoStatusNear);

  SetLength(client, MaxConn);
  SetLength(test, MaxConn);
  for i := low(client) to high(client) do
    begin
      client[i] := TCommunicationFramework_Client_CrossSocket.Create;
      client[i].QuietMode := True;
      client[i].SwitchMaxPerformance;
      client[i].SequencePacketActivted := True;
      test[i] := TCommunicationTestIntf.Create;
      test[i].RegCmd(client[i]);
    end;
end;

procedure TEZClientForm.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  for i := low(test) to high(test) do
      DisposeObject(test[i]);
  for i := low(client) to high(client) do
      DisposeObject(client[i]);
  DeleteDoStatusHook(self);
end;

procedure TEZClientForm.TimerTimer(Sender: TObject);
var
  i: Integer;
  c: Integer;
begin
  c := 0;
  for i := low(client) to high(client) do
    begin
      client[i].Progress;

      if client[i].RemoteInited then
          inc(c);
    end;
  Caption := Format('total connected:%d', [c]);
end;

procedure TEZClientForm.TestCommandButtonClick(Sender: TObject);
var
  i: Integer;
begin
  TestCommandButton.Visible := False;
  disconnectButton.Visible := False;
  try
    for i := low(test) to high(test) do
      begin
        try
          test[i].ExecuteAsyncTest(client[i].ClientIO);
          application.ProcessMessages;
        except
        end;
      end;
  finally
    TestCommandButton.Visible := True;
    disconnectButton.Visible := True;
  end;
end;

procedure TEZClientForm.ConnectButtonClick(Sender: TObject);
var
  i: Integer;
  allconnected: Integer;
begin
  ConnectButton.Visible := False;
  Timer.Enabled := False;
  TestCommandButton.Visible := False;
  for i := low(client) to high(client) do
    begin
      TCommunicationFramework_Client_CrossSocket(client[i]).AsyncConnectTimeout := 60 * 1000;
      TCommunicationFramework_Client_CrossSocket(client[i]).AsyncConnect(HostEdit.Text, 9818);
      application.ProcessMessages;
    end;

  Timer.Enabled := True;

  while True do
    begin
      allconnected := low(client);
      for i := low(client) to high(client) do
        begin
          if client[i].RemoteInited then
              inc(allconnected);
          application.ProcessMessages;
        end;
      if allconnected = length(client) then
          break;
    end;

  TestCommandButton.Visible := True;
  disconnectButton.Visible := True;
end;

end.
