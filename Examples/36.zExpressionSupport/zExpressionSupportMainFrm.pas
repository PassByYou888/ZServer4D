unit zExpressionSupportMainFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.StdCtrls, FMX.Edit, FMX.Layouts, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo,

  CoreClasses, PascalStrings, UnicodeMixedLib, DoStatusIO, TextParsing, zExpression, OpCode;

type
  TzExpressionSupportMainForm = class(TForm)
    ExpParsingMemo: TMemo;
    expEvaluateMemo: TMemo;
    Layout1: TLayout;
    Label3: TLabel;
    InputEdit: TEdit;
    EditButton1: TEditButton;
    ParsingButton: TButton;
    EvaluateButton: TButton;
    StatusMemo: TMemo;
    procedure EditButton1Click(Sender: TObject);
    procedure EvaluateButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ParsingButtonClick(Sender: TObject);
  private
    { Private declarations }
    procedure DoStatusMethod(AText: SystemString; const ID: Integer);
  public
    { Public declarations }
  end;

var
  zExpressionSupportMainForm: TzExpressionSupportMainForm;

implementation

{$R *.fmx}


function a(var Param: TOpParam): Variant;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to length(Param) - 1 do
      Result := Result + Param[i];
end;

procedure TzExpressionSupportMainForm.FormCreate(Sender: TObject);
begin
  AddDoStatusHook(Self, DoStatusMethod);

  DefaultOpRT.RegOpC('a', a);
end;

procedure TzExpressionSupportMainForm.DoStatusMethod(AText: SystemString; const ID: Integer);
begin
  StatusMemo.Lines.Add(AText);
  StatusMemo.GoToTextEnd;
end;

procedure TzExpressionSupportMainForm.EditButton1Click(Sender: TObject);
var
  v: Variant;
begin
  // 评估器，支持向量表达式：1+1,2+2,3+3
  v := EvaluateExpressionValue(False, InputEdit.Text);
  if not VarIsNull(v) then
      DoStatus(InputEdit.Text + ' = ' + VarToStr(v));
end;

procedure TzExpressionSupportMainForm.EvaluateButtonClick(Sender: TObject);
var
  i: Integer;
  v: Variant;
begin
  for i := 0 to expEvaluateMemo.Lines.Count - 1 do
    begin
      // 评估器，支持向量表达式：1+1,2+2,3+3
      v := EvaluateExpressionValue(False, nil, tsPascal, expEvaluateMemo.Lines[i], nil);
      if not VarIsNull(v) then
          DoStatus('%s = %s', [expEvaluateMemo.Lines[i], VarToStr(v)])
      else
          DoStatus('error: ' + expEvaluateMemo.Lines[i]);
    end;
end;

procedure TzExpressionSupportMainForm.ParsingButtonClick(Sender: TObject);
var
  i: Integer;
  E, e2: TSymbolExpression;
begin
  for i := 0 to ExpParsingMemo.Lines.Count - 1 do
    begin
      // 底层符号解析api，不支持向量表达式：1+1,2+2,3+3
      E := ParseTextExpressionAsSymbol_M(TTextParsing, tsPascal, '', ExpParsingMemo.Lines[i], nil, nil);
      if E <> nil then
        begin
          e2 := RebuildAllSymbol(E);
          if e2 <> nil then
            begin
              e2.PrintDebug(False);
              DisposeObject(e2);
            end;
          DisposeObject(E);
        end
      else
          DoStatus('error: ' + ExpParsingMemo.Lines[i]);
    end;
end;

end.
