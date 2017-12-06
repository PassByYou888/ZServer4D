unit RenameMMForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TfRenameMM = class(TForm)
    Label1: TLabel;
    eMMName: TEdit;
    bOK: TBitBtn;
    bCancel: TBitBtn;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fRenameMM: TfRenameMM;

implementation

{$R *.dfm}

end.
