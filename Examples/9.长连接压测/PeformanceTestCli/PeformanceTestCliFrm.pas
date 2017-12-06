unit PeformanceTestCliFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  CommunicationFramework,
  DoStatusIO, CoreClasses,
  CommunicationFramework_Client_CrossSocket,
  CommunicationFramework_Client_ICS,
  Cadencer, DataFrameEngine, UnicodeMixedLib, CommunicationTest;

type
  TEZClientForm = class(TForm)
    ConnectButton: TButton;
    HostEdit: TLabeledEdit;
    Timer1: TTimer;
    Button1: TButton;
    procedure ConnectButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    procedure DoStatusNear(AText: string; const ID: Integer);
  public
    { Public declarations }
    client: array of TCommunicationFrameworkClient;
    test  : array of TCommunicationTestIntf;
  end;

const
  MaxConn = 1000;

var
  EZClientForm: TEZClientForm;

implementation

{$R *.dfm}


procedure TEZClientForm.DoStatusNear(AText: string; const ID: Integer);
begin
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
      client[i] := TCommunicationFramework_Client_ICS.Create;
      client[i].AllowPrintCommand := False;
      client[i].SwitchMaxPerformance;
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

procedure TEZClientForm.Timer1Timer(Sender: TObject);
var
  i: Integer;
begin
  for i := low(client) to high(client) do
      client[i].ProgressBackground;
end;

procedure TEZClientForm.Button1Click(Sender: TObject);
var
  i: Integer;
begin
  for i := low(test) to high(test) do
    begin
      try
          test[i].ExecuteAsyncTest(client[i].ClientIO);
      except
      end;
    end;
end;

procedure TEZClientForm.ConnectButtonClick(Sender: TObject);
var
  i: Integer;
begin
  ConnectButton.Visible := False;
  for i := low(client) to high(client) do
      client[i].Connect(HostEdit.Text, 9818);
end;

end.
