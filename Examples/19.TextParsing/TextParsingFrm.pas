unit TextParsingFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  PascalStrings, TextParsing, CoreClasses, UnicodeMixedLib, zExpression, OpCode, MemoryStream64, ListEngine,
  DoStatusIO,
  TypInfo,
  Vcl.ComCtrls, Vcl.ExtCtrls;

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
    TabSheet3: TTabSheet;
    Memo5: TMemo;
    Panel1: TPanel;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
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
  t: TTextParsing;
  i: Integer;
  pt: PTokenData;
begin
  TextParsing.SpacerSymbol.V:=umlDeleteChar(TextParsing.SpacerSymbol.V, '.');

  t := TTextParsing.Create(Memo1.Text, TTextStyle.tsPascal, nil);

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
  t: TTextParsing;
  i: Integer;
  pt: PTokenData;
begin
  t := TTextParsing.Create(Memo1.Text, TTextStyle.tsC, nil);

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
  t: TTextParsing;
  i: Integer;
  pt: PTokenData;

  PrepareProc: Boolean;
begin
  t := TTextParsing.Create(Memo3.Text, TTextStyle.tsPascal, nil);

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

procedure TForm1.Button4Click(Sender: TObject);
var
  rt: TOpCustomRunTime;
  v: Variant;
begin
  Memo5.Lines.Add('基本使用demo');
  // rt为ze的运行函数支持库
  rt := TOpCustomRunTime.Create;
  rt.RegOpP('myAddFunction', function(var Param: TOpParam): Variant
    // (a+b)*0.5
    begin
      Result := (Param[0] + Param[1]) * 0.5;
    end);
  rt.RegOpP('myStringFunction', function(var Param: TOpParam): Variant
    begin
      Result := Format('字符串长度为:%d', [Length(VarToStr(Param[0]) + VarToStr(Param[1]))]);
    end);

  // 简单数学表达式
  v := EvaluateExpressionValue(False, '1000+{ 这里是备注 ze可以识别pascal和c的备注以及字符串写法 } myAddFunction(1+1/2*3/3.14*9999, 599+2+2*100 shl 3)', rt);
  Memo5.Lines.Add(VarToStr(v));

  // 简单字符串表达式，ze的默认文本处理格式为Pascal
  v := EvaluateExpressionValue(False, 'myStringFunction('#39'abc'#39', '#39'123'#39')', rt);
  Memo5.Lines.Add(VarToStr(v));

  // 简单字符串表达式，我们使用c的文本格式，c能支持单双引号，但是不支持#字符表达式
  v := EvaluateExpressionValue(tsC, 'myStringFunction("abc", "123")', rt);
  Memo5.Lines.Add(VarToStr(v));
  v := EvaluateExpressionValue(tsC, 'myStringFunction('#39'abc'#39', '#39'123'#39')', rt);
  Memo5.Lines.Add(VarToStr(v));

  DisposeObject(rt);
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  tmpSym: TSymbolExpression;
  op: TOpCode;
  rt: TOpCustomRunTime;
  m64: TMemoryStream64;
begin
  Memo5.Lines.Add('高速载入与执行demo');
  // rt为ze的运行函数支持库
  rt := TOpCustomRunTime.Create;
  rt.RegOpP('myAddFunction', function(var Param: TOpParam): Variant
    // (a+b)*0.5
    begin
      Result := (Param[0] + Param[1]) * 0.5;
    end);
  rt.RegOpP('myStringFunction', function(var Param: TOpParam): Variant
    begin
      Result := Format('字符串长度为:%d', [Length(VarToStr(Param[0]) + VarToStr(Param[1]))]);
    end);

  // 使用ParseTextExpressionAsSymbol函数，将表达式翻译成词法树
  tmpSym := ParseTextExpressionAsSymbol_M(TTextParsing, tsPascal, '', '1000+myAddFunction(1+1/2*3/3.14*9999, 599+2+2*100 shl 3)', nil, rt);
  // BuildAsOpCode会将词法树再次翻译成语法树，然后再基于语法树生成op代码
  op := BuildAsOpCode(tmpSym);
  DisposeObject(tmpSym);
  // 我们执行一次op
  Memo5.Lines.Add(Format('op运行返回值(正确值为4489.2962): %s', [VarToStr(op.Execute(rt))]));

  m64 := TMemoryStream64.Create;
  op.SaveToStream(m64);

  // 这里已经释放了op
  DisposeObject(op);

  // 从stream快速读取op，这便于我们在
  m64.Position := 0;
  if LoadOpFromStream(m64, op) then
    begin
      Memo5.Lines.Add(Format('op运行返回值(正确值为4489.2962): %s', [VarToStr(op.Execute(rt))]));
    end;

  DisposeObject([op, rt, m64]);

  Memo5.Lines.Add('高速载入与执行demo，运行完毕');
end;

procedure TForm1.Button6Click(Sender: TObject);
type
  TState = (sUnknow, sIF, sTrue, sFalse); // 解析用的简单状态机
label gFillStruct;
var
  t: TTextParsing;                                  // 词法解析引擎
  cp, ep: Integer;                                  // 字坐标
  wasNumber, wasText, wasAscii, wasSymbol: Boolean; // 解析文本状态机
  state: TState;                                    // 解析结构状态机
  decl: TPascalString;                              // 当前解析词法体，包括
  ifMatchBody: TPascalString;                       // 条件布尔判断运行体
  ifTrueBody: TPascalString;                        // 条件成立运行体
  ifFalseBody: TPascalString;                       // 条件不成立运行体
  rt: TOpCustomRunTime;                             // 运行函数库支持
begin
  // 由于pascal的字符串不便于书写在程序中，这里我们c风格字符串
  t := TTextParsing.Create('if 1+1=/* comment */2 then writeln/* comment */("if was true") else writeln/* comment */("if was false");', tsC, nil);
  cp := 1;
  ep := 1;
  state := sUnknow;
  ifMatchBody := '';
  ifTrueBody := '';
  ifFalseBody := '';

  // 解析主循环
  while cp < t.Len do
    begin
      // 如果是代码备注，跳过去
      if t.IsComment(cp) then
        begin
          ep := t.GetCommentEndPos(cp);
          cp := ep;
          continue;
        end;

      // 词法流程范式，这套此范式是以成熟词法解析为主，没有考虑性能，如果需要加速运行脚本，请考虑编译成数据结构存储再以高速方式载入运行
      wasNumber := t.IsNumber(cp);
      wasText := t.IsTextDecl(cp);
      wasAscii := t.IsAscii(cp);
      wasSymbol := t.IsSymbol(cp);

      if wasNumber then
        begin
          ep := t.GetNumberEndPos(cp);
          decl := t.GetStr(cp, ep);
          cp := ep;
          goto gFillStruct;
        end;

      if wasText then
        begin
          ep := t.GetTextDeclEndPos(cp);
          decl := t.GetStr(cp, ep);
          cp := ep;
          goto gFillStruct;
        end;

      if wasAscii then
        begin
          ep := t.GetAsciiEndPos(cp);
          decl := t.GetStr(cp, ep);
          cp := ep;
          goto gFillStruct;
        end;

      if wasSymbol then
        begin
          decl := t.ParsingData.Text[cp];
          inc(cp);
          ep := cp;
          goto gFillStruct;
        end;

      inc(cp);
      continue;
      // 词法流程范式结束，下面我们做结构体判断

    gFillStruct:

      if wasAscii then
        begin
          // 词法结构
          if decl.Same('if') then
            begin
              if state <> sUnknow then
                begin
                  Memo5.Lines.Add('if 格式解析错误');
                  break;
                end;
              state := sIF;
              continue;
            end;

          if decl.Same('then') then
            begin
              if state <> sIF then
                begin
                  Memo5.Lines.Add('then 格式解析错误');
                  break;
                end;
              state := sTrue;
              continue;
            end;

          if decl.Same('else') then
            begin
              if state <> sTrue then
                begin
                  Memo5.Lines.Add('else 书写格式解析错误');
                  break;
                end;
              state := sFalse;
              continue;
            end;
        end;

      case state of
        sIF: ifMatchBody.Append(decl);    // 在TPascalString中，使用Append方法，要比string:=string+string效率更高
        sTrue: ifTrueBody.Append(decl);   // 在TPascalString中，使用Append方法，要比string:=string+string效率更高
        sFalse: ifFalseBody.Append(decl); // 在TPascalString中，使用Append方法，要比string:=string+string效率更高
      end;
    end;

  // 到这一步，整个if结构体就已经解析成功了，我们直接运行程序即可
  if state = sFalse then
    begin
      rt := TOpCustomRunTime.Create;
      rt.RegOpP('writeln', function(var Param: TOpParam): Variant
        begin
          Memo5.Lines.Add(VarToStr(Param[0]));
          Result := 0;
        end);
      // 如果需要性能，这里的结构体你可以考虑用数据结构来存储，实现快速脚本
      // opCache.Clear;
      if EvaluateExpressionValue(tsC, ifMatchBody, rt) = True then
          EvaluateExpressionValue(tsC, ifTrueBody, rt)
      else
          EvaluateExpressionValue(tsC, ifFalseBody, rt);
      DisposeObject(rt);
    end;

  DisposeObject(t);
end;

procedure TForm1.Button7Click(Sender: TObject);

  function Macro(var AText: string; const HeadToken, TailToken: string; const rt: TOpCustomRunTime): TPascalString; inline;
  var
    sour: TPascalString;
    ht, tt: TPascalString;
    bPos, ePos: Integer;
    KeyText: SystemString;
    i: Integer;
    tmpSym: TSymbolExpression;
    op: TOpCode;
  begin
    Result := '';
    sour.Text := AText;
    ht.Text := HeadToken;
    tt.Text := TailToken;

    i := 1;

    while i <= sour.Len do
      begin
        if sour.ComparePos(i, @ht) then
          begin
            bPos := i;
            ePos := sour.GetPos(@tt, i + ht.Len);
            if ePos > 0 then
              begin
                KeyText := sour.copy(bPos + ht.Len, ePos - (bPos + ht.Len)).Text;

                // 在TPascalString中，使用Append方法，要比string:=string+string效率更高
                Result.Append(VarToStr(EvaluateExpressionValue(KeyText, rt)));
                i := ePos + tt.Len;
                continue;
              end;
          end;

        // 在TPascalString中，使用Append方法，要比string:=string+string效率更高
        Result.Append(sour[i]);
        inc(i);
      end;
  end;

var
  n: string;
  i: Integer;
  t: TTimeTick;
  rt: TOpCustomRunTime;
begin
  Memo5.Lines.Add('简单演示用脚本来封装zExpression');
  // rt为ze的运行函数支持库
  rt := TOpCustomRunTime.Create;
  rt.RegOpP('OverFunction', function(var Param: TOpParam): Variant
    begin
      Result := '谢谢';
    end);

  // 我们这里使用宏处理将1+1以表达式来翻译
  n := '这是1+1=<begin>1+1<end>，这是一个UInt48位整形:<begin>1<<48<end>，结束 <begin>OverFunction<end>';

  Memo5.Lines.Add('原型:' + n);
  Memo5.Lines.Add('计算结果' + Macro(n, '<begin>', '<end>', rt).Text);

  Memo5.Lines.Add('zExpression正在测试性能，对上列原型做10万次处理');

  t := GetTimeTick;

  // 重复做1万次句法表达式解析和处理
  for i := 1 to 10 * 10000 do
      Macro(n, '<begin>', '<end>', rt);

  Memo5.Lines.Add(Format('zExpression性能测试完成，耗时:%dms', [GetTimeTick - t]));

  DisposeObject([rt]);
end;

procedure TForm1.Button8Click(Sender: TObject);
// 高级Demo，实现内部变量的赋值
// 这是我从另一个脚本引擎拔出来的范例，内容有点多，但是原理只有三步
var
  sourTp, t: TTextParsing;            // 词法解析引擎
  setBefore, setAfter: TPascalString; // 赋值的前置申明，和赋值的后置申明
  splitVarDecl: TArrayPascalString;   // 切开的表达式体
  myvars: TArrayPascalString;         // 我们需要赋值的临时变量，以逗号分隔
  WasAssignment: Boolean;             // 在表达式中找到了赋值
  HashVars: THashVariantList;         // 变量的hash存储结构，这是可以存放到硬盘中的
  rt: TOpCustomRunTime;               // 运行函数库支持
  op: TOpCode;                        // 我们用来做cache的op变量
  i: Integer;                         // for使用
  dynvar: Integer;                    // 动态变量
begin
  // 这里有c和pascal两种写法，自行修改备注即可
  sourTp := TTextParsing.Create('myvar1/*这里是备注*/,myvar2,myvar3 = 123+456+" 变量: "+dynamic', tsC, nil); // 词法解析引擎，以c语法为例
  // sourTp := TTextParsing.Create('myvar1(*这里是备注*),myvar2,myvar3 := 123+456+'#39' 变量: '#39'+dynamic', tsPascal); // 词法解析引擎，以c语法为例
  // sourTp := TTextParsing.Create('123+456+dynamic', tsPascal); // 词法解析引擎，以c语法为例

  HashVars := THashVariantList.CustomCreate(16); // 16是hash的buff长度，数值越大加速度越快

  SetLength(splitVarDecl, 0);
  SetLength(myvars, 0);

  // 第一步，分析赋值符号
  case sourTp.TextStyle of
    tsPascal:
      begin
        // pascal的赋值符号为 :=
        WasAssignment := sourTp.SplitString(1, ':=', ';', splitVarDecl) = 2; // 以字符串作为切割记号，对带有:=记号的字符串进行切割
        if WasAssignment then
          begin
            setBefore := splitVarDecl[0];
            setAfter := splitVarDecl[1];

            t := TTextParsing.Create(setBefore, tsPascal, nil);
            t.DeletedComment;
            if t.SplitChar(1, ',', ';', myvars) = 0 then // 这里不是字符串，是以字符作为切割记号，对带有,的字符进行切割
                Memo5.Lines.Add(Format('变量赋值语法错误 %s', [setBefore.Text]));
            DisposeObject(t);
          end;
      end;
    tsC:
      begin
        // c的赋值符号为 =
        WasAssignment := sourTp.SplitChar(1, '=', ';', splitVarDecl) = 2; // 这里不是字符串，是以字符作为切割记号，对带有=的字符进行切割
        if WasAssignment then
          begin
            setBefore := splitVarDecl[0];
            setAfter := splitVarDecl[1];

            t := TTextParsing.Create(setBefore, tsC, nil);
            t.DeletedComment;
            if t.SplitChar(1, ',', ';', myvars) = 0 then // 这里不是字符串，是以字符作为切割记号，对带有,的字符进行切割
                Memo5.Lines.Add(Format('变量赋值语法错误 %s', [setBefore.Text]));
            DisposeObject(t);
          end;
      end;
    else
      begin
        Memo5.Lines.Add('不支持表达式');
        WasAssignment := False;
      end;
  end;

  rt := TOpCustomRunTime.Create;
  rt.RegOpP('dynamic', function(var Param: TOpParam): Variant
    begin
      Result := dynvar;
      inc(dynvar);
    end);
  rt.RegOpP('myvar1', function(var Param: TOpParam): Variant
    begin
      // 对myvar1进行动态复用
      Result := HashVars['myvar1'];
    end);

  dynvar := 1;

  // 第二步，如果找到了赋值符号
  if WasAssignment then
    begin
      Memo5.Lines.Add('发现了变量赋值表达式');

      op := BuildAsOpCode(sourTp.TextStyle, setAfter, rt);

      for i := low(myvars) to high(myvars) do
          HashVars[myvars[i].TrimChar(#32).Text] := op.Execute(rt); // 做一次首尾空格裁剪后，执行op，并且批量的赋值

      Memo5.Lines.Add('变量赋值内容');
      Memo5.Lines.Add(HashVars.AsText);

      // 第三步，让变量在表达式中的复用
      Memo5.Lines.Add('现在，我们开始静态复用我们刚才申明的变量，静态复用是将变量以const形式进行编译');

      // 由于opCache机制是自动化进行的，我们在任何时候以const复用变量时都要清空它
      CleanOpCache;

      Memo5.Lines.Add(VarToStr(EvaluateExpressionValue_P(False, nil, TTextParsing, tsC, '"静态复用 "+myvar1',
        procedure(const DeclName: SystemString; var ValType: TExpressionDeclType; var Value: Variant)
        begin
          if HashVars.Exists(DeclName) then
            begin
              Value := HashVars[DeclName];
              ValType := TExpressionDeclType.edtString; // 我们需要告诉编译器，该变量的类型
            end;
        end)));

      Memo5.Lines.Add(VarToStr(EvaluateExpressionValue_P(False, nil, TTextParsing, tsC, '"静态复用 "+myvar4',
        procedure(const DeclName: SystemString; var ValType: TExpressionDeclType; var Value: Variant)
        begin
          // myvar4是不存在的
          // 然后 我们以myvar2来代替
          Value := HashVars['myvar2'];
          ValType := TExpressionDeclType.edtString; // 我们需要告诉编译器，该变量的类型
        end)));

      Memo5.Lines.Add('现在，我们开始动态复用我们刚才申明的变量');
      Memo5.Lines.Add(VarToStr(EvaluateExpressionValue(tsC, '"动态复用 "+myvar1', rt)));

      HashVars['myvar1'] := 'abc';
      Memo5.Lines.Add(VarToStr(EvaluateExpressionValue(tsC, '"动态复用 "+myvar1', rt)));
    end
  else
    begin
      Memo5.Lines.Add('没有发现了变量赋值');
      Memo5.Lines.Add(Format('表达式 "%s"' + #13#10 + '运行结果 %s',
        [sourTp.ParsingData.Text.Text, VarToStr(EvaluateExpressionValue(sourTp.TextStyle, sourTp.ParsingData.Text, rt))]));
    end;

  DisposeObject([sourTp, HashVars, rt]);
end;

procedure TForm1.Button9Click(Sender: TObject);
// 特殊符号函数
var
  SpecialAsciiToken: TPascalStringList;
  rt: TOpCustomRunTime;
  v: Variant;
begin
  Memo5.Lines.Add('全局的词法探头前缀参量的使用');

  // 凡是前缀有@@符号,均作为ascii来处理
  SpecialAsciiToken := TPascalStringList.Create;
  SpecialAsciiToken.Add('@@');
  SpecialAsciiToken.Add('&&');

  // rt为ze的运行函数支持库
  rt := TOpCustomRunTime.Create;
  rt.RegOpP('@@a&&', function(var Param: TOpParam): Variant
    // (a+b)*0.5
    begin
      Result := (Param[0] + Param[1]) * 0.5;
    end);
  rt.RegOpP('@@combineString&&', function(var Param: TOpParam): Variant
    // (a+b)*0.5
    begin
      Result := VarToStr(Param[0]) + VarToStr(Param[1]);
    end);

  // 带有@@前缀的ascii也可以在后缀带有特殊符号，特殊符号长度不限制
  v := EvaluateExpressionValue(SpecialAsciiToken, False, '{ 备注 } @@a&&(1,2)', rt);
  Memo5.Lines.Add(VarToStr(v));

  // 简单字符串表达式，ze的默认文本处理格式为Pascal
  v := EvaluateExpressionValue(SpecialAsciiToken, False, '@@combineString&&('#39'abc'#39', '#39'123'#39')', rt);
  Memo5.Lines.Add(VarToStr(v));

  // 简单字符串表达式，我们使用c的文本格式
  v := EvaluateExpressionValue(SpecialAsciiToken, tsC, '@@combineString&&("abc", "123")', rt);
  Memo5.Lines.Add(VarToStr(v));
  v := EvaluateExpressionValue(SpecialAsciiToken, tsC, '@@combineString&&('#39'abc'#39', '#39'123'#39')', rt);
  Memo5.Lines.Add(VarToStr(v));

  DisposeObject(rt);

  DisposeObject(SpecialAsciiToken);
end;

end.
 
