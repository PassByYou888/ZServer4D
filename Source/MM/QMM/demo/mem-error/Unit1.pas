{$WARN UNSAFE_TYPE off}
{$WARN UNSAFE_CODE off}
{$WARN UNSAFE_CAST off}
unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

type
  TErrorObject = class
    v: array [0..7] of Char;
  end;

procedure TForm1.Button1Click(Sender: TObject);
var
  i: Integer;
  p: Pointer;
  e: TErrorObject;
begin
  p := GetMemory(4);
  PMsg(p).message := 1;
  PMsg(p).wParam := 1;
  FreeMemory(p);

  // i := 8 <-> 16
  i := 10;
  e := TErrorObject.Create;
  e.v[i] := 'c';
  e.Free;
end;

end.
