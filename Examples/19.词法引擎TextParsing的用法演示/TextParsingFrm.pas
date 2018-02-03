unit TextParsingFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  PascalStrings, TextParsing, CoreClasses, UnicodeMixedLib, TypInfo,
  Vcl.ComCtrls;

type
  TForm1 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Memo1: TMemo;
    Button1: TButton;
    Memo2: TMemo;
    Button2: TButton;
    TabSheet2: TTabSheet;
    Memo3: TMemo;
    Button3: TButton;
    Memo4: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}


procedure TForm1.Button1Click(Sender: TObject);
var
  t : TTextParsing;
  i : Integer;
  pt: PTokenData;
begin
  t := TTextParsing.Create(Memo1.Text, TTextStyle.tsPascal);

  Memo2.Clear;

  for i := 0 to t.ParsingData.Cache.TokenDataList.Count - 1 do
    begin
      pt := t.ParsingData.Cache.TokenDataList[i];
      if pt^.tokenType <> TTokenType.ttUnknow then
          Memo2.Lines.Add(Format('索引 %d 类型:%s 值 %s', [i, GetEnumName(TypeInfo(TTokenType), Ord(pt^.tokenType)), pt^.Text.Text]));
    end;

  DisposeObject(t);
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  t : TTextParsing;
  i : Integer;
  pt: PTokenData;
begin
  t := TTextParsing.Create(Memo1.Text, TTextStyle.tsC);

  Memo2.Clear;

  for i := 0 to t.ParsingData.Cache.TokenDataList.Count - 1 do
    begin
      pt := t.ParsingData.Cache.TokenDataList[i];
      if pt^.tokenType <> TTokenType.ttUnknow then
          Memo2.Lines.Add(Format('索引 %d 类型:%s 值 %s', [i, GetEnumName(TypeInfo(TTokenType), Ord(pt^.tokenType)), pt^.Text.Text]));
    end;

  DisposeObject(t);
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  t : TTextParsing;
  i : Integer;
  pt: PTokenData;

  PrepareProc: Boolean;
begin
  t := TTextParsing.Create(Memo3.Text, TTextStyle.tsC);

  Memo2.Clear;
  PrepareProc := False;

  for i := 0 to t.ParsingData.Cache.TokenDataList.Count - 1 do
    begin
      pt := t.ParsingData.Cache.TokenDataList[i];

      if PrepareProc then
        begin
          if (pt^.tokenType = TTokenType.ttSymbol) then
              PrepareProc := False
          else if (pt^.tokenType = TTokenType.ttAscii) then
              Memo4.Lines.Add(Format('索引 %d 类型:%s 值 %s', [i, GetEnumName(TypeInfo(TTokenType), Ord(pt^.tokenType)), pt^.Text.Text]));
        end
      else
          PrepareProc := (pt^.tokenType = TTokenType.ttAscii) and (pt^.Text.Same('function') or pt^.Text.Same('procedure'));
    end;

  DisposeObject(t);
end;

end.
