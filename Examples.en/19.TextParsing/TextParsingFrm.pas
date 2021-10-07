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
          Memo2.Lines.Add(Format('Index %D type: %s value %s', [i, GetEnumName(TypeInfo(TTokenType), Ord(pt^.tokenType)), pt^.Text.Text]));
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
          Memo2.Lines.Add(Format('Index %D type: %s value %s', [i, GetEnumName(TypeInfo(TTokenType), Ord(pt^.tokenType)), pt^.Text.Text]));
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
              Memo4.Lines.Add(Format('Index %D type: %s value %s', [i, GetEnumName(TypeInfo(TTokenType), Ord(pt^.tokenType)), pt^.Text.Text]));
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
  Memo5.Lines.Add('Basic use demo');
  {  Runtime function support library with RT as Ze  }
  rt := TOpCustomRunTime.Create;
  rt.RegOpP('myAddFunction', function(var Param: TOpParam): Variant
    // (a+b)*0.5
    begin
      Result := (Param[0] + Param[1]) * 0.5;
    end);
  rt.RegOpP('myStringFunction', function(var Param: TOpParam): Variant
    begin
      Result := Format('String length: %d', [Length(VarToStr(Param[0]) + VarToStr(Param[1]))]);
    end);

  {  Simple mathematical expression  }
  v := EvaluateExpressionValue(False, '1000 + {here is the note Ze, which can recognize the notes of Pascal and C and the string writing method} myaddfunction (1 + 1 / 2 * 3 / 3.14 * 9999, 599 + 2 + 2 * 100 SHL 3)', rt);
  Memo5.Lines.Add(VarToStr(v));

  {  Simple string expression. The default text processing format of Ze is Pascal  }
  v := EvaluateExpressionValue(False, 'myStringFunction('#39'abc'#39', '#39'123'#39')', rt);
  Memo5.Lines.Add(VarToStr(v));

  {  For simple string expressions, we use the text format of C. C can support single and double quotation marks, but does not support # character expressions  }
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
  Memo5.Lines.Add('High speed loading and execution demo');
  {  Runtime function support library with RT as Ze  }
  rt := TOpCustomRunTime.Create;
  rt.RegOpP('myAddFunction', function(var Param: TOpParam): Variant
    // (a+b)*0.5
    begin
      Result := (Param[0] + Param[1]) * 0.5;
    end);
  rt.RegOpP('myStringFunction', function(var Param: TOpParam): Variant
    begin
      Result := Format('String length: %d', [Length(VarToStr(Param[0]) + VarToStr(Param[1]))]);
    end);

  {  Use the parsetextexpressionassymbol function to translate the expression into a lexical tree  }
  tmpSym := ParseTextExpressionAsSymbol_M(TTextParsing, tsPascal, '', '1000+myAddFunction(1+1/2*3/3.14*9999, 599+2+2*100 shl 3)', nil, rt);
  {  Buildasopcode will translate the lexical tree into the idiom tree again, and then generate op code based on the syntax tree  }
  op := BuildAsOpCode(tmpSym);
  DisposeObject(tmpSym);
  {  Let's do it once  }
  Memo5.Lines.Add(Format('OP run return value (correct value is 4489.2962): %s', [VarToStr(op.Execute(rt))]));

  m64 := TMemoryStream64.Create;
  op.SaveToStream(m64);

  {  The op has been released here  }
  DisposeObject(op);

  {  Quickly read OP from stream, which is convenient for us to  }
  m64.Position := 0;
  if LoadOpFromStream(m64, op) then
    begin
      Memo5.Lines.Add(Format('OP run return value (correct value is 4489.2962): %s', [VarToStr(op.Execute(rt))]));
    end;

  DisposeObject([op, rt, m64]);

  Memo5.Lines.Add('Load and execute demo at high speed, and the operation is completed');
end;

procedure TForm1.Button6Click(Sender: TObject);
type
  TState = (sUnknow, sIF, sTrue, sFalse); {  Simple state machine for parsing  }
label gFillStruct;
var
  t: TTextParsing;                                  {  Lexical parsing engine  }
  cp, ep: Integer;                                  {  Word coordinates  }
  wasNumber, wasText, wasAscii, wasSymbol: Boolean; {  Parsing text state machine  }
  state: TState;                                    {  Analytic structure state machine  }
  decl: TPascalString;                              {  Current parsed lexical aspect, including  }
  ifMatchBody: TPascalString;                       {  Conditional Boolean judgment running body  }
  ifTrueBody: TPascalString;                        {  Conditional running body  }
  ifFalseBody: TPascalString;                       {  The condition does not hold  }
  rt: TOpCustomRunTime;                             {  Runtime library support  }
begin
  {  Because Pascal string is not easy to write in the program, here we use C-style string  }
  t := TTextParsing.Create('if 1+1=/* comment */2 then writeln/* comment */("if was true") else writeln/* comment */("if was false");', tsC, nil);
  cp := 1;
  ep := 1;
  state := sUnknow;
  ifMatchBody := '';
  ifTrueBody := '';
  ifFalseBody := '';

  {  Parse main loop  }
  while cp < t.Len do
    begin
      {  If it is a code comment, skip to it  }
      if t.IsComment(cp) then
        begin
          ep := t.GetCommentEndPos(cp);
          cp := ep;
          continue;
        end;

      {  Lexical process paradigm, which focuses on mature lexical parsing without considering performance. If you need to speed up the running of scripts, please consider compiling them into data structure storage and then loading and running them in a high-speed way  }
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
      {  The lexical process paradigm ends. Let's judge the structure  }

    gFillStruct:

      if wasAscii then
        begin
          {  Lexical Structure   }
          if decl.Same('if') then
            begin
              if state <> sUnknow then
                begin
                  Memo5.Lines.Add('If format parsing error');
                  break;
                end;
              state := sIF;
              continue;
            end;

          if decl.Same('then') then
            begin
              if state <> sIF then
                begin
                  Memo5.Lines.Add('Then format parsing error');
                  break;
                end;
              state := sTrue;
              continue;
            end;

          if decl.Same('else') then
            begin
              if state <> sTrue then
                begin
                  Memo5.Lines.Add('Else writing format parsing error');
                  break;
                end;
              state := sFalse;
              continue;
            end;
        end;

      case state of
        sIF: ifMatchBody.Append(decl);    {  In tpascalstring, the append method is more efficient than string: = string + string  }
        sTrue: ifTrueBody.Append(decl);   {  In tpascalstring, the append method is more efficient than string: = string + string  }
        sFalse: ifFalseBody.Append(decl); {  In tpascalstring, the append method is more efficient than string: = string + string  }
      end;
    end;

  {  At this step, the entire if structure has been parsed successfully. We can run the program directly  }
  if state = sFalse then
    begin
      rt := TOpCustomRunTime.Create;
      rt.RegOpP('writeln', function(var Param: TOpParam): Variant
        begin
          Memo5.Lines.Add(VarToStr(Param[0]));
          Result := 0;
        end);
      {  If you need performance, you can consider using data structure to store the structure here to realize fast script  }
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

                {  In tpascalstring, the append method is more efficient than string: = string + string  }
                Result.Append(VarToStr(EvaluateExpressionValue(KeyText, rt)));
                i := ePos + tt.Len;
                continue;
              end;
          end;

        {  In tpascalstring, the append method is more efficient than string: = string + string  }
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
  Memo5.Lines.Add('A simple demonstration encapsulates zexpression with a script');
  {  Runtime function support library with RT as Ze  }
  rt := TOpCustomRunTime.Create;
  rt.RegOpP('OverFunction', function(var Param: TOpParam): Variant
    begin
      Result := 'thank you';
    end);

  {  Here we use macro processing to translate 1 + 1 as an expression  }
  n := 'This is 1 + 1 = < begin > 1 + 1 < end >, which is a uint48 bit integer: < begin > 1 < < 48 < end >, end < begin > overfunction < end >';

  Memo5.Lines.Add('Prototype:' + n);
  Memo5.Lines.Add('Calculation results' + Macro(n, '<begin>', '<end>', rt).Text);

  Memo5.Lines.Add('Zexpression is testing its performance and processing the above prototypes 100000 times');

  t := GetTimeTick;

  {  Repeat parsing and processing of syntactic expressions 10000 times  }
  for i := 1 to 10 * 10000 do
      Macro(n, '<begin>', '<end>', rt);

  Memo5.Lines.Add(Format('Zexpression performance test completed, time consuming: %DMS', [GetTimeTick - t]));

  DisposeObject([rt]);
end;

procedure TForm1.Button8Click(Sender: TObject);
{  Advanced demo to realize the assignment of internal variables  }
{  This is an example I pulled out from another script engine. There are a lot of contents, but the principle is only three steps  }
var
  sourTp, t: TTextParsing;            {  Lexical parsing engine  }
  setBefore, setAfter: TPascalString; {  Pre declaration of assignment and post declaration of assignment  }
  splitVarDecl: TArrayPascalString;   {  Cut expression body  }
  myvars: TArrayPascalString;         {  We need to assign temporary variables separated by commas  }
  WasAssignment: Boolean;             {  Assignment found in expression  }
  HashVars: THashVariantList;         {  Variable hash storage structure, which can be stored in the hard disk  }
  rt: TOpCustomRunTime;               {  Runtime library support  }
  op: TOpCode;                        {  The op variable we use as the cache  }
  i: Integer;                         {  For use  }
  dynvar: Integer;                    {  Dynamic variable  }
begin
  {  There are two ways to write here: C and Pascal. You can modify the remarks by yourself  }
  sourTp := TTextParsing.Create('myvar1,myvar2,myvar3 = 123+456+" variant:"+dynamic', tsC, nil);

  HashVars := THashVariantList.CustomCreate(16); {  16 is the buff length of the hash. The larger the value, the faster the acceleration  }

  SetLength(splitVarDecl, 0);
  SetLength(myvars, 0);

  {  The first step is to analyze the assignment symbols  }
  case sourTp.TextStyle of
    tsPascal:
      begin
        {  The assignment symbol of Pascal is:=  }
        WasAssignment := sourTp.SplitString(1, ':=', ';', splitVarDecl) = 2; {  Take the string as the cutting mark to cut the string with: = mark  }
        if WasAssignment then
          begin
            setBefore := splitVarDecl[0];
            setAfter := splitVarDecl[1];

            t := TTextParsing.Create(setBefore, tsPascal, nil);
            t.DeletedComment;
            if t.SplitChar(1, ',', ';', myvars) = 0 then {  This is not a string, but a character is used as a cutting mark to cut the character with  }
                Memo5.Lines.Add(Format('Variable assignment syntax error %s', [setBefore.Text]));
            DisposeObject(t);
          end;
      end;
    tsC:
      begin
        {  The assignment symbol of C is=  }
        WasAssignment := sourTp.SplitChar(1, '=', ';', splitVarDecl) = 2; {  This is not a string, but a character is used as a cutting mark to cut the character with =  }
        if WasAssignment then
          begin
            setBefore := splitVarDecl[0];
            setAfter := splitVarDecl[1];

            t := TTextParsing.Create(setBefore, tsC, nil);
            t.DeletedComment;
            if t.SplitChar(1, ',', ';', myvars) = 0 then {  This is not a string, but a character is used as a cutting mark to cut the character with  }
                Memo5.Lines.Add(Format('Variable assignment syntax error %s', [setBefore.Text]));
            DisposeObject(t);
          end;
      end;
    else
      begin
        Memo5.Lines.Add('Expressions are not supported');
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
      {  Dynamic multiplexing of myvar1  }
      Result := HashVars['myvar1'];
    end);

  dynvar := 1;

  {  Step 2, if the assignment symbol is found  }
  if WasAssignment then
    begin
      Memo5.Lines.Add('Variable assignment expression found');

      op := BuildAsOpCode(sourTp.TextStyle, setAfter, rt);

      for i := low(myvars) to high(myvars) do
          HashVars[myvars[i].TrimChar(#32).Text] := op.Execute(rt); {  After cutting the leading and trailing spaces once, execute OP and assign values in batches  }

      Memo5.Lines.Add('Variable assignment content');
      Memo5.Lines.Add(HashVars.AsText);

      {  The third step is to reuse variables in expressions  }
      Memo5.Lines.Add('Now, we start to statically reuse the variables we just declared. Static reuse is to compile variables in const form');

      {  Since the opcache mechanism is automated, we must clear it whenever we reuse variables with const  }
      CleanOpCache;

      Memo5.Lines.Add(VarToStr(EvaluateExpressionValue_P(False, nil, TTextParsing, tsC, '"Static reuse" + myvar1',
        procedure(const DeclName: SystemString; var ValType: TExpressionDeclType; var Value: Variant)
        begin
          if HashVars.Exists(DeclName) then
            begin
              Value := HashVars[DeclName];
              ValType := TExpressionDeclType.edtString; {  We need to tell the compiler the type of the variable  }
            end;
        end)));

      Memo5.Lines.Add(VarToStr(EvaluateExpressionValue_P(False, nil, TTextParsing, tsC, '"Static reuse" + myvar4',
        procedure(const DeclName: SystemString; var ValType: TExpressionDeclType; var Value: Variant)
        begin
          {  Myvar4 does not exist  }
          {  Then we replace it with myvar2  }
          Value := HashVars['myvar2'];
          ValType := TExpressionDeclType.edtString; {  We need to tell the compiler the type of the variable  }
        end)));

      Memo5.Lines.Add('Now we begin to dynamically reuse the variables we just declared');
      Memo5.Lines.Add(VarToStr(EvaluateExpressionValue(tsC, '"Dynamic reuse" + myvar1', rt)));

      HashVars['myvar1'] := 'abc';
      Memo5.Lines.Add(VarToStr(EvaluateExpressionValue(tsC, '"Dynamic reuse" + myvar1', rt)));
    end
  else
    begin
      Memo5.Lines.Add('Variable assignment not found');
      Memo5.Lines.Add(Format('Expression '#39' %s'#39 + #13#10 + 'Run result %s',
        [sourTp.ParsingData.Text.Text, VarToStr(EvaluateExpressionValue(sourTp.TextStyle, sourTp.ParsingData.Text, rt))]));
    end;

  DisposeObject([sourTp, HashVars, rt]);
end;

procedure TForm1.Button9Click(Sender: TObject);
{  Special symbolic function  }
var
  SpecialAsciiToken: TPascalStringList;
  rt: TOpCustomRunTime;
  v: Variant;
begin
  Memo5.Lines.Add('Use of global lexical probe prefix parameters');

  {  All prefixes with @ @ symbols are treated as ASCII  }
  SpecialAsciiToken := TPascalStringList.Create;
  SpecialAsciiToken.Add('@@');
  SpecialAsciiToken.Add('&&');

  {  Runtime function support library with RT as Ze  }
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

  {  ASCII with @ @ prefix can also have special symbols in the suffix, and the length of special symbols is not limited  }
  v := EvaluateExpressionValue(SpecialAsciiToken, False, '{remarks} @@a&& (1,2)', rt);
  Memo5.Lines.Add(VarToStr(v));

  {  Simple string expression. The default text processing format of Ze is Pascal  }
  v := EvaluateExpressionValue(SpecialAsciiToken, False, '@@combineString&&('#39'abc'#39', '#39'123'#39')', rt);
  Memo5.Lines.Add(VarToStr(v));

  {  For simple string expressions, we use the text format of C  }
  v := EvaluateExpressionValue(SpecialAsciiToken, tsC, '@@combineString&&("abc", "123")', rt);
  Memo5.Lines.Add(VarToStr(v));
  v := EvaluateExpressionValue(SpecialAsciiToken, tsC, '@@combineString&&('#39'abc'#39', '#39'123'#39')', rt);
  Memo5.Lines.Add(VarToStr(v));

  DisposeObject(rt);

  DisposeObject(SpecialAsciiToken);
end;

end.
 
