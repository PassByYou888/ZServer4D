unit BuildIndexPackageOptFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Buttons, Vcl.FileCtrl, System.IOUtils, UnicodeMixedLib, PascalStrings;

type
  TBuildIndexPackageOptForm = class(TForm)
    DestDBEdit: TLabeledEdit;
    DataPathEdit: TLabeledEdit;
    OkButton: TButton;
    CancelButton: TButton;
    BrowseDestButton: TSpeedButton;
    BrowseDataPathButton: TSpeedButton;
    SaveDialog: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure BrowseDataPathButtonClick(Sender: TObject);
    procedure BrowseDestButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  BuildIndexPackageOptForm: TBuildIndexPackageOptForm;

implementation

{$R *.dfm}


procedure TBuildIndexPackageOptForm.FormCreate(Sender: TObject);
begin
  DestDBEdit.Text := umlCombineFileName(TPath.GetDocumentsPath, 'temp.ox');
  DataPathEdit.Text := umlCombinePath(TPath.GetDocumentsPath, 'DataCache\');
end;

procedure TBuildIndexPackageOptForm.BrowseDataPathButtonClick(Sender: TObject);
var
  d: string;
begin
  d := DataPathEdit.Text;
  if not SelectDirectory('Data directory', '', d, [sdNewFolder, sdNewUI]) then
      Exit;
  DataPathEdit.Text := d;
end;

procedure TBuildIndexPackageOptForm.BrowseDestButtonClick(Sender: TObject);
begin
  SaveDialog.FileName := DestDBEdit.Text;
  if not SaveDialog.Execute() then
      Exit;
  DestDBEdit.Text := SaveDialog.FileName;
end;

procedure TBuildIndexPackageOptForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caHide;
end;

end.
