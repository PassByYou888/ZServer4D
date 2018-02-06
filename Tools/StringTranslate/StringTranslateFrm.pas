unit StringTranslateFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  UnicodeMixedLib, PascalStrings, TextParsing;

type
  TStringTranslateForm = class(TForm)
    Memo1: TMemo;
    Memo2: TMemo;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  StringTranslateForm: TStringTranslateForm;

implementation

{$R *.dfm}


procedure TStringTranslateForm.Button1Click(Sender: TObject);
var
  s, n  : umlString;
  c     : Char;
  output: string;
begin
  s := umlDeleteChar(Memo1.Text, #32#9#13#10'()[]{}');
  output := '';

  while s <> '' do
    begin
      n := umlGetFirstStr(s, ',');
      s := umlDeleteFirstStr(s, ',');
      c := Char(umlStrToInt(n, 0));
      output := output + c;
    end;

  Memo2.Text := output;
end;

procedure TStringTranslateForm.Button2Click(Sender: TObject);
var
  s     : string;
  c     : Char;
  cnt   : Integer;
  output: string;
begin
  s := Memo2.Text;
  output := '';
  cnt := 0;
  for c in s do
    begin
      if cnt > 40 then
        begin
          output := Format('%s,' + #13#10 + '%s', [output, '$' + IntToHex(ord(c), 2)]);
          cnt := 0;
        end
      else
        begin
          if output <> '' then
              output := Format('%s, %s', [output, '$' + IntToHex(ord(c), 2)])
          else
              output := '$' + IntToHex(ord(c), 2);
        end;

      inc(cnt);
    end;

  Memo1.Text := output;
end;

procedure TStringTranslateForm.Button3Click(Sender: TObject);
var
  s     : string;
  c     : Char;
  cnt   : Integer;
  output: string;
begin
  s := Memo2.Text;
  output := '';
  cnt := 0;
  for c in s do
    begin
      if cnt > 40 then
        begin
          output := Format('%s' + #13#10 + '%s', [output, '#' + IntToStr(ord(c))]);
          cnt := 0;
        end
      else
        begin
          if output <> '' then
              output := Format('%s%s', [output, '#' + IntToStr(ord(c))])
          else
              output := '#' + IntToStr(ord(c));
        end;

      inc(cnt);
    end;

  Memo1.Text := output;
end;

procedure TStringTranslateForm.Button4Click(Sender: TObject);
var
  i: Integer;
begin
  Memo1.Clear;
  for i := 0 to Memo2.Lines.Count - 1 do
    begin
      if i = Memo2.Lines.Count - 1 then
          Memo1.Lines.Add(TTextParsing.TranslateTextToPascalDecl(Memo2.Lines[i] + #13#10) + ';')
      else
          Memo1.Lines.Add(TTextParsing.TranslateTextToPascalDecl(Memo2.Lines[i] + #13#10) + '+');
    end;
end;

procedure TStringTranslateForm.Button5Click(Sender: TObject);
var
  i: Integer;
begin
  Memo2.Clear;
  for i := 0 to Memo1.Lines.Count - 1 do
    begin
      Memo2.Lines.Add(TTextParsing.TranslatePascalDeclToText(Memo1.Lines[i]));
    end;
end;

procedure TStringTranslateForm.Button6Click(Sender: TObject);
var
  i: Integer;
begin
  Memo1.Clear;
  for i := 0 to Memo2.Lines.Count - 1 do
    begin
      if i = Memo2.Lines.Count - 1 then
          Memo1.Lines.Add(TTextParsing.TranslateTextToC_Decl(Memo2.Lines[i]) + ';')
      else
          Memo1.Lines.Add(TTextParsing.TranslateTextToC_Decl(Memo2.Lines[i] + #10) + '+');
    end;
end;

procedure TStringTranslateForm.Button7Click(Sender: TObject);
var
  i: Integer;
begin
  Memo2.Clear;
  for i := 0 to Memo1.Lines.Count - 1 do
    begin
      Memo2.Lines.Add(TTextParsing.TranslateC_DeclToText(Memo1.Lines[i]));
    end;
end;

end.
