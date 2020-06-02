unit UserClientFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ScrollBox, FMX.Memo,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.Edit, FMX.Layouts, System.TypInfo,
  FMX.ListBox,
  CommunicationFramework, DoStatusIO, CoreClasses, PhysicsIO,
  Cadencer, DataFrameEngine, UnicodeMixedLib, CommunicationTest;

type
  TForm2 = class(TForm)
    Timer1: TTimer;
    clientLayout: TLayout;
    connectionButton: TButton;
    Button1: TButton;
    TestButton: TButton;
    Layout1: TLayout;
    HostEdit: TEdit;
    Label1: TLabel;
    InfoListBox: TListBox;
    procedure Button1Click(Sender: TObject);
    procedure connectionButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TestButtonClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    client: array of TCommunicationFramework_StableClient;
    test: array of TCommunicationTestIntf;
    procedure DoStatusNear(AText: string; const ID: Integer);
  end;

var
  Form2: TForm2;

const
  MaxConn = 10;

implementation

{$R *.fmx}


procedure TForm2.Button1Click(Sender: TObject);
var
  i: Integer;
begin
  for i := low(client) to high(client) do
      client[i].Disconnect;
end;

procedure TForm2.connectionButtonClick(Sender: TObject);
var
  i: Integer;
begin
  for i := low(client) to high(client) do
      client[i].AsyncConnectM(HostEdit.Text, 18888, nil);
end;

procedure TForm2.DoStatusNear(AText: string; const ID: Integer);
begin

end;

procedure TForm2.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  AddDoStatusHook(self, DoStatusNear);

  SetLength(client, MaxConn);
  SetLength(test, MaxConn);
  for i := low(client) to high(client) do
    begin
      client[i] := TPhysicsClient.Create.StableIO;
      client[i].QuietMode := True;
      test[i] := TCommunicationTestIntf.Create;
      test[i].RegCmd(client[i]);
    end;
end;

procedure TForm2.TestButtonClick(Sender: TObject);
var
  i: Integer;
begin
  for i := low(test) to high(test) do
    begin
      test[i].ExecuteAsyncTest(client[i].ClientIO);
    end;
end;

procedure TForm2.Timer1Timer(Sender: TObject);
  procedure PrintServerState;
  var
    buff: array [TStatisticsType] of Int64;
    comm: TCommunicationFramework;
    st: TStatisticsType;
    i: Integer;
    v: Int64;
    n: string;
    lItm: TListBoxItem;
  begin
    for st := low(TStatisticsType) to high(TStatisticsType) do
        buff[st] := 0;

    for comm in client do
      begin
        for st := low(TStatisticsType) to high(TStatisticsType) do
            buff[st] := buff[st] + comm.Statistics[st];
      end;

    while InfoListBox.Count < Ord(high(TStatisticsType)) + 1 do
      begin
        lItm := TListBoxItem.Create(InfoListBox);
        lItm.Parent := InfoListBox;
      end;

    for st := low(TStatisticsType) to high(TStatisticsType) do
      begin
        v := buff[st];
        n := IntToStr(v);
        InfoListBox.ListItems[Ord(st)].Text := GetEnumName(TypeInfo(TStatisticsType), Ord(st)) + ' : ' + n;
      end;
  end;

var
  i, c: Integer;
begin
  c := 0;
  for i := low(client) to high(client) do
    begin
      client[i].Progress;

      if client[i].RemoteInited then
          inc(c);
    end;
  Caption := Format('total connected:%d', [c]);
  PrintServerState;
end;

end.
