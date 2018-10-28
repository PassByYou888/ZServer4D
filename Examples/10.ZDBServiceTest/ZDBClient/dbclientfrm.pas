unit dbclientfrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, System.Rtti,
  DoStatusIO, Vcl.ComCtrls, CommunicationFrameworkDataStoreService,
  CommunicationFramework, DataFrameEngine,
  CommunicationFramework_Client_CrossSocket,
  CommunicationFramework_Client_ICS, CommunicationFramework_Client_Indy,
  CoreClasses, ZDBEngine, ZDBLocalManager, MemoryStream64, UnicodeMixedLib,
  PascalStrings, CommunicationFrameworkDataStoreService_NoAuth,
  CommunicationFrameworkDataStoreServiceCommon;

type
  TForm1 = class(TForm)
    Memo: TMemo;
    TopPanel: TPanel;
    Button1: TButton;
    ProgressTimer: TTimer;
    build100DataButton: TButton;
    QueryG300Button: TButton;
    QueryG700Button: TButton;
    QueryG700AndDeleteButton: TButton;
    QueryG300AndDeleteButton: TButton;
    CompressButton: TButton;
    ResetDataButton: TButton;
    ModifyG300AsG700Button: TButton;
    ListBox1: TListBox;
    Timer1: TTimer;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ProgressTimerTimer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure build100DataButtonClick(Sender: TObject);
    procedure QueryG300ButtonClick(Sender: TObject);
    procedure QueryG700ButtonClick(Sender: TObject);
    procedure QueryG700AndDeleteButtonClick(Sender: TObject);
    procedure QueryG300AndDeleteButtonClick(Sender: TObject);
    procedure CompressButtonClick(Sender: TObject);
    procedure ResetDataButtonClick(Sender: TObject);
    procedure ModifyG300AsG700ButtonClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  public
    { Private declarations }
    RecvTunnel, SendTunnel: TCommunicationFrameworkClient;
    DataStoreClient: TDataStoreClient_NoAuth;
  public
    { Public declarations }
    procedure DoStatusNear(AText: string; const id: Integer);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}


procedure TForm1.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(Self, DoStatusNear);
  RecvTunnel := TCommunicationFramework_Client_CrossSocket.Create;
  SendTunnel := TCommunicationFramework_Client_CrossSocket.Create;
  DataStoreClient := TDataStoreClient_NoAuth.Create(RecvTunnel, SendTunnel);
  DataStoreClient.RegisterCommand;

  RecvTunnel.QuietMode := True;
  SendTunnel.QuietMode := True;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  DeleteDoStatusHook(Self);
  DisposeObject([DataStoreClient, RecvTunnel, SendTunnel]);
end;

procedure TForm1.ModifyG300AsG700ButtonClick(Sender: TObject);
begin
  DataStoreClient.QueryDBP('G300', True, False, True, False, 'LocalTestDB', '', 0.1, 0, 0, nil,
    procedure(dbN, pipeN: SystemString; StorePos: Int64; id: Cardinal; DataSour: TMemoryStream64)
    var
      de: TDataFrameEngine;
    begin
      de := TDataFrameEngine.Create;
      de.DecodeFrom(DataSour);
      TDataFrameDouble(de.Data[0]).Buffer := umlRandomRangeD(-710.0, -700.0);
      DataStoreClient.ModifyAssembleStream('LocalTestDB', StorePos, de);
      DisposeObject(de);
    end,
    procedure(dbN, outN, pipeN: string; TotalResult: Int64)
    begin
    end);
end;

procedure TForm1.ProgressTimerTimer(Sender: TObject);
begin
  DataStoreClient.Progress;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if not DataStoreClient.Connect('192.168.2.77', 13888, 13887) then
      exit;
  if DataStoreClient.TunnelLink then
    begin
      doStatus('connect ok!');
      while not DataStoreClient.DataCipherKeyFinished do
          DataStoreClient.Progress;
    end;
end;

procedure TForm1.CompressButtonClick(Sender: TObject);
begin
  DataStoreClient.CompressDB('LocalTestDB');
end;

procedure TForm1.build100DataButtonClick(Sender: TObject);
var
  de: TDataFrameEngine;
  i, j: Integer;
begin
  DataStoreClient.InitDB(False, 'LocalTestDB');
  DataStoreClient.BeginAssembleStream;
  for i := 1 to 100000 do
    begin
      de := TDataFrameEngine.Create;
      for j := 1 to umlRandomRange(10, 20) do
          de.WriteDouble(umlRandomRangeD(-50000, 50000));

      // PostAssembleStream 方法将会以BatchStream机制向服务器提交数据
      // PostAssembleStream 方法在提交数据前，会对数据做压缩及加密操作
      // DataStoreClient.PostAssembleStream('LocalTestDB', de);

      // FastPostCompleteBuffer 方法将会CompleteBuffer机制向服务器提交数据
      // PostAssembleStream 方法在提交数据前，不会对数据做任何操作操作，如果在公网传输重要数据，可能会造成机密数据泄漏
      DataStoreClient.FastPostCompleteBuffer('LocalTestDB', de);

      DataStoreClient.Progress;
      DisposeObject(de);
    end;
  DataStoreClient.EndAssembleStream;
end;

procedure TForm1.QueryG300AndDeleteButtonClick(Sender: TObject);
begin
  DataStoreClient.QueryDBP('G300', True, False, True, False, 'LocalTestDB', '', 0.1, 0, 0, nil,
    procedure(dbN, pipeN: SystemString; StorePos: Int64; id: Cardinal; DataSour: TMemoryStream64)
    begin
      DataStoreClient.DeleteData('LocalTestDB', StorePos);
    end,
    procedure(dbN, outN, pipeN: string; TotalResult: Int64)
    begin
      doStatus('%s deleted and query done! total delete Result:%d', [pipeN, TotalResult]);
    end);
end;

procedure TForm1.QueryG300ButtonClick(Sender: TObject);
begin
  DataStoreClient.QueryDBP('G300', True, True, True, False, 'LocalTestDB', '', 0.1, 0, 0, nil,
    procedure(dbN, pipeN: SystemString; StorePos: Int64; id: Cardinal; DataSour: TMemoryStream64)
    begin
    end,
    procedure(dbN, outN, pipeN: string; TotalResult: Int64)
    begin
      doStatus('%s query done! total Result:%d', [pipeN, TotalResult]);
    end);
end;

procedure TForm1.QueryG700AndDeleteButtonClick(Sender: TObject);
begin
  DataStoreClient.QueryDBP('G700', True, False, True, False, 'LocalTestDB', '', 0.1, 0, 0, nil,
    procedure(dbN, pipeN: SystemString; StorePos: Int64; id: Cardinal; DataSour: TMemoryStream64)
    begin
      DataStoreClient.DeleteData('LocalTestDB', StorePos);
    end,
    procedure(dbN, outN, pipeN: string; TotalResult: Int64)
    begin
      doStatus('%s deleted and query done! total delete Result:%d', [pipeN, TotalResult]);
    end);
end;

procedure TForm1.QueryG700ButtonClick(Sender: TObject);
begin
  DataStoreClient.QueryDBP('G700', True, True, True, True, 'LocalTestDB', '', 0.1, 0, 0, nil,
    procedure(dbN, pipeN: SystemString; StorePos: Int64; id: Cardinal; DataSour: TMemoryStream64)
    var
      de: TDataFrameEngine;
    begin
      de := TDataFrameEngine.Create;
      DataSour.Position := 0;
      de.DecodeFrom(DataSour, False);
      doStatus(de.ReadDouble(0));
      DisposeObject(de);
    end,
    procedure(dbN, outN, pipeN: string; TotalResult: Int64)
    begin
      doStatus('%s query done! total Result:%d', [pipeN, TotalResult]);
    end);
end;

procedure TForm1.ResetDataButtonClick(Sender: TObject);
begin
  DataStoreClient.ResetData('LocalTestDB');
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  DataStoreClient.GetQueryListP(
    procedure(Sender: TPeerClient; ResultData: TDataFrameEngine)
    begin
      ListBox1.Clear;
      while ResultData.Reader.NotEnd do
        begin
          DataStoreClient.GetQueryStateP(ResultData.Reader.ReadString,
            procedure(Sender: TPeerClient; ResultData: TDataFrameEngine)
            var
              t: TRttiRecordType;
              f: TRttiField;
              ps: TPipeState;
            begin
              ps.Decode(ResultData);
              // if ps.Activted then
              if True then
                begin
                  t := TRttiContext.Create.GetType(TypeInfo(TPipeState)).AsRecord;
                  for f in t.GetFields do
                    begin
                      ListBox1.Items.Add(Format('%s.%s = %s', [ps.RegistedQuery, f.Name, f.GetValue(@ps).ToString]));
                    end;
                  ListBox1.Items.Add('');
                end;
            end);
        end;
    end);
end;

procedure TForm1.DoStatusNear(AText: string; const id: Integer);
begin
  Memo.Lines.Add(AText);
end;

end.
