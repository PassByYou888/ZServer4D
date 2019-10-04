unit NewDBOptFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TNewDBOptForm = class(TForm)
    FixedStringEdit: TLabeledEdit;
    OkButton: TButton;
    CancelButton: TButton;
    Memo1: TMemo;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  NewDBOptForm: TNewDBOptForm;

implementation

{$R *.dfm}


procedure TNewDBOptForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caHide;
end;

procedure TNewDBOptForm.FormKeyUp(Sender: TObject; var Key: Word; Shift:
  TShiftState);
begin
  if Key = VK_Escape then
      ModalResult := mrCancel;
end;

end.
