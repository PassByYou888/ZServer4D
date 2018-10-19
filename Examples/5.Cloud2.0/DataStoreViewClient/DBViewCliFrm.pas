unit DBViewCliFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  CommunicationFramework_Client_CrossSocket, DoStatusIO, PascalStrings,
  Vcl.ExtCtrls, ListEngine, MemoryStream64, CoreClasses,
  DataStoreClientIntf, CommunicationFrameworkDoubleTunnelIO_ServMan,
  Vcl.ComCtrls, Vcl.Grids, CommunicationFramework, DataFrameEngine,
  ZDBEngine, CommonServiceDefine;

type
  TDBViewCliForm = class(TForm)
    Memo: TMemo;
    connectButton: TButton;
    Timer1: TTimer;
    DisconnectButton: TButton;
    PageControl: TPageControl;
    LogTabSheet: TTabSheet;
    DBTabSheet: TTabSheet;
    DBListView: TListView;
    DBStringGrid: TStringGrid;
    Splitter1: TSplitter;
    procedure connectButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure DisconnectButtonClick(Sender: TObject);
    procedure DBListViewClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    cli: TDataStore_DoubleTunnelClient;
    procedure DoStatusNear(AText: SystemString; const ID: Integer);

    procedure RefreshDB;
  end;

var
  DBViewCliForm: TDBViewCliForm;

implementation

{$R *.dfm}


procedure TDBViewCliForm.connectButtonClick(Sender: TObject);
var
  vl: THashVariantList;
begin
  if cli.Connect('127.0.0.1', cDataStorePrimary_SendPort, cDataStorePrimary_RecvPort) then
      RefreshDB;
end;

procedure TDBViewCliForm.DBListViewClick(Sender: TObject);
var
  lst   : TDBListVL;
  maxCol: Integer;
  maxVL : TDBEngineVL;

begin
  if DBListView.SelCount <> 1 then      exit;
  if not cli.Connected then      exit;

  lst := TDBListVL.Create;
  maxCol := -1;
  DBStringGrid.ColCount := 1;
  DBStringGrid.RowCount := 1;
  DBStringGrid.Cells[0, 0] := Format('%s Downloading', [DBListView.Selected.Caption]);
  DBStringGrid.ColWidths[0] := 200;

  Enabled := False;

  cli.DownloadDBWithIDP(True, DBListView.Selected.Caption, ZDBEngine.c_VL,
    procedure(dbN, pipeN: SystemString; StorePos: Int64; ID: Cardinal; DataSour: TMemoryStream64)
    var
      vl: TDBEngineVL;
    begin
      vl := lst.Add;
      vl.LoadFromStream(DataSour);
      vl['__StorePos__'] := StorePos; vl['__Database__'] := dbN;
      if vl.Count > maxCol then
        begin
          maxCol := vl.Count;
          maxVL := vl;
        end;
    end,
    procedure(dbN, outN, pipeN: SystemString; TotalResult: Int64)
    var
      i, j: Integer;
      vl: TDBEngineVL;
    begin
      DBStringGrid.Cells[0, 0] := Format('download done', []);
      DBStringGrid.ColCount := maxCol;
      DBStringGrid.RowCount := lst.Count + 1;
      if DBStringGrid.RowCount > 1 then
          DBStringGrid.FixedRows := 1;
      DBStringGrid.ColWidths[0] := DBStringGrid.DefaultColWidth;
      if maxVL <> nil then
          maxVL.GetNameList(DBStringGrid.Rows[0]);

      for i := 0 to lst.Count - 1 do
        begin
          vl := lst[i];
          for j := 0 to DBStringGrid.Rows[0].Count - 1 do
            begin
              try
                if TPascalString(DBStringGrid.Rows[0][j]).GetPos('Time') > 0 then
                    DBStringGrid.Cells[j, i + 1] := TimeToStr(Double(vl[DBStringGrid.Rows[0][j]]))
                else if TPascalString(DBStringGrid.Rows[0][j]).GetPos('Date') > 0 then
                    DBStringGrid.Cells[j, i + 1] := DateToStr(Double(vl[DBStringGrid.Rows[0][j]]))
                else
                    DBStringGrid.Cells[j, i + 1] := VarToStr(vl[DBStringGrid.Rows[0][j]]);
              except
              end;
            end;
        end;

      lst.Clear;
      DisposeObject(lst);
      Enabled := True;
    end);
end;

procedure TDBViewCliForm.DisconnectButtonClick(Sender: TObject);
begin
  cli.Disconnect;
end;

procedure TDBViewCliForm.DoStatusNear(AText: SystemString; const ID: Integer);
begin
  Memo.Lines.Add(AText);
end;

procedure TDBViewCliForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(self, DoStatusNear);
  cli := TDataStore_DoubleTunnelClient.Create(TCommunicationFramework_Client_CrossSocket);
  cli.RegisterCommand;
end;

procedure TDBViewCliForm.FormDestroy(Sender: TObject);
begin
  DeleteDoStatusHook(self);
end;

procedure TDBViewCliForm.RefreshDB;
begin
  if not cli.Connected then
      exit;

  cli.GetDBListP(procedure(Sender: TPeerClient; ResultData: TDataFrameEngine)
    begin
      DBListView.Items.BeginUpdate;
      DBListView.Items.Clear;
      while ResultData.Reader.NotEnd do
        with DBListView.Items.Add do
          begin
            Caption := ResultData.Reader.ReadString;
            ImageIndex := -1;
          end;
      DBListView.Items.EndUpdate;
    end);
end;

procedure TDBViewCliForm.Timer1Timer(Sender: TObject);
begin
  cli.Progress;
end;

end.
