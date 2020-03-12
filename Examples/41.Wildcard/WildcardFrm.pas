unit WildcardFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,

  PascalStrings, UnicodeMixedLib;

type
  TWildcardForm = class(TForm)
    Memo1: TMemo;
    SourEdit: TLabeledEdit;
    TargetEdit: TLabeledEdit;
    MatchButton: TButton;
    procedure MatchButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  WildcardForm: TWildcardForm;

implementation

{$R *.dfm}


procedure TWildcardForm.MatchButtonClick(Sender: TObject);
begin
  ShowMessage(umlBoolToStr(umlMultipleMatch(True, SourEdit.text, TargetEdit.text)));
end;

end.
