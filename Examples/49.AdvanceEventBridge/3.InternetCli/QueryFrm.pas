unit QueryFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.Imaging.jpeg, Vcl.ExtDlgs,

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
  TQueryForm = class(TForm)
    NameEdit: TLabeledEdit;
    PhoneEdit: TLabeledEdit;
    CommentEdit: TLabeledEdit;
    QueryButton: TButton;
    ListView: TListView;
    infoPanel: TPanel;
    Label1: TLabel;
    InfoNameEdit: TLabeledEdit;
    InfoPhoneEdit: TLabeledEdit;
    InfoCommentMemo: TMemo;
    PicturePanel: TPanel;
    Image: TImage;
    OpenPictureDialog: TOpenPictureDialog;
    Label2: TLabel;
    ReverseCheckBox: TCheckBox;
    jsonMemo: TMemo;
    Label3: TLabel;
    DeleteButton: TButton;
    procedure QueryButtonClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure ListViewCreateItemClass(Sender: TCustomListView; var ItemClass: TListItemClass);
    procedure ListViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure MyQueryResult(Sender: TPeerIO; ResultData: TDataFrameEngine);
    procedure MyDownloadPictureResult(Sender: TPeerIO; ResultData: TDataFrameEngine);
  end;

var
  QueryForm: TQueryForm;

implementation

{$R *.dfm}


uses InternetCliFrm;

type
  TMyListItem = class(TListItem)
  public
    json: TJsonObject;
    constructor Create(AOwner: TListItems); override;
    destructor Destroy; override;
  end;

constructor TMyListItem.Create(AOwner: TListItems);
begin
  inherited Create(AOwner);
  json := TJsonObject.Create;
end;

destructor TMyListItem.Destroy;
begin
  json.Free;
  inherited Destroy;
end;

procedure TQueryForm.QueryButtonClick(Sender: TObject);
var
  vl: THashVariantList;
begin
  vl := THashVariantList.Create;
  if NameEdit.Text <> '' then
      vl['Name'] := NameEdit.Text;
  if PhoneEdit.Text <> '' then
      vl['Phone'] := PhoneEdit.Text;
  if CommentEdit.Text <> '' then
      vl['Comment'] := CommentEdit.Text;
  InternetClient.MyQuery(InternetCliForm.DBNameEdit.Text, ReverseCheckBox.Checked, 0, vl, MyQueryResult);
  vl.Free;
end;

procedure TQueryForm.DeleteButtonClick(Sender: TObject);
var
  itm: TMyListItem;
begin
  if ListView.Selected = nil then
      exit;
  itm := ListView.Selected as TMyListItem;
  // …æ√˚∆¨’’∆¨
  InternetClient.MyDelete(InternetCliForm.DBNameEdit.Text, itm.json.I64['Picture']);
  // …æ√˚∆¨
  InternetClient.MyDelete(InternetCliForm.DBNameEdit.Text, itm.json.I64['Pos']);
end;

procedure TQueryForm.ListViewCreateItemClass(Sender: TCustomListView; var ItemClass: TListItemClass);
begin
  ItemClass := TMyListItem;
end;

procedure TQueryForm.ListViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
var
  itm: TMyListItem;
begin
  if not Selected then
      exit;
  itm := Item as TMyListItem;
  jsonMemo.Clear;
  itm.json.SaveToLines(jsonMemo.Lines);
  InfoNameEdit.Text := itm.json.S['Name'];
  InfoPhoneEdit.Text := itm.json.S['Phone'];
  InfoCommentMemo.Text := itm.json.S['Comment'];
  InternetClient.MyDownload(InternetCliForm.DBNameEdit.Text, itm.json.I64['Picture'], MyDownloadPictureResult);
end;

procedure TQueryForm.MyQueryResult(Sender: TPeerIO; ResultData: TDataFrameEngine);
var
  i: Integer;
  itm: TMyListItem;
begin
  ListView.Items.BeginUpdate;
  ListView.Items.Clear;

  while ResultData.R.NotEnd do
    begin
      itm := ListView.Items.Add as TMyListItem;
      ResultData.R.ReadJson(itm.json);
      itm.Caption := itm.json.S['Name'];
      itm.SubItems.Add(itm.json.S['Phone']);
    end;

  ListView.Items.EndUpdate;
end;

procedure TQueryForm.MyDownloadPictureResult(Sender: TPeerIO; ResultData: TDataFrameEngine);
var
  m64: TMS64;
begin
  m64 := TMS64.Create;
  ResultData.R.ReadStream(m64);
  m64.Position := 0;
  Image.Picture.LoadFromStream(m64);
  m64.Free;
end;

end.
