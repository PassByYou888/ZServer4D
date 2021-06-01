unit CardFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.Imaging.jpeg, Vcl.ExtDlgs,

  CoreClasses, MemoryStream64,
  ZS_JsonDataObjects;

type
  TCardForm = class(TForm)
    NameEdit: TLabeledEdit;
    PhoneEdit: TLabeledEdit;
    Label1: TLabel;
    commentMemo: TMemo;
    PicturePanel: TPanel;
    Label2: TLabel;
    SaveButton: TButton;
    CloseButton: TButton;
    Image: TImage;
    SetPictureButton: TButton;
    OpenPictureDialog: TOpenPictureDialog;
    procedure FormCreate(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure SetPictureButtonClick(Sender: TObject);
  private
  public
  end;

var
  CardForm: TCardForm;
  emptyPicture: TPicture;

implementation

{$R *.dfm}


uses InternetCliFrm;

procedure TCardForm.FormCreate(Sender: TObject);
begin
  emptyPicture := TPicture.Create;
end;

procedure TCardForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TCardForm.SaveButtonClick(Sender: TObject);
var
  m64: TMS64;
begin
  if (Image.Picture.Graphic = nil) or (Image.Picture.Graphic.Empty) then
    begin
      MessageDlg('picture is empty.', mtError, [mbYes], 0);
      exit;
    end;
  m64 := TMS64.Create;
  Image.Picture.SaveToStream(m64);
  InternetClient.MyAutoSave(InternetCliForm.DBNameEdit.Text, 0, m64, procedure(DataStorePos: Int64)
    var
      js: TJsonObject;
      tmp: TMS64;
    begin
      js := TJsonObject.Create;
      js.S['Name'] := NameEdit.Text;
      js.S['Phone'] := PhoneEdit.Text;
      js.S['Comment'] := commentMemo.Text;
      js.I64['Picture'] := DataStorePos;
      tmp := TMS64.Create;
      js.SaveToStream(tmp, False, TEncoding.UTF8);
      js.Free;
      InternetClient.MyAutoSave(InternetCliForm.DBNameEdit.Text, 1, tmp, nil);
      tmp.Free;

      NameEdit.Text := '';
      PhoneEdit.Text := '';
      commentMemo.Clear;
      Image.Picture.Assign(emptyPicture);
    end);
  m64.Free;
end;

procedure TCardForm.SetPictureButtonClick(Sender: TObject);
begin
  if not OpenPictureDialog.Execute then
      exit;
  Image.Picture.LoadFromFile(OpenPictureDialog.FileName);
end;

end.
