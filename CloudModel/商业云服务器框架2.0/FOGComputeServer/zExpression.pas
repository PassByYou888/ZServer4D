unit zExpression;

{$I ..\zDefine.inc}

(*

  zExpression 句法编译器+解释器

  技术体系解释：
  在编译原理的技术体系中，凡是处理文本化的代码前，都需要做一次预处理，其中我们常说的语法，语法糖，都是一种预处理程序
  词法：词法是对文本关键字，数字，符号，进行分类整理，最后形成词法树，并且严格遵循顺序化处理原则
  申明：在预处理代码中，申明部分，叫做申明树，申明树又依赖于词法顺序预处理，因为对词法预处理是一种简化手段
  句法：在经过了申明预处理以后，是对代码表达式的单行逻辑操作进行处理，这一步叫句法
  zExpression句法编译器是我从曾经撰写的编译器中特意剥离出来的解决方案
  它可以独立出来分发和使用，可以实用数字化预处理，图形图像，科学计算等等领域，也可以作为学习提高自己的手段

  核心思路
  实现zExpression采用的是对等复杂化原则，面向解决编译器问题而编写，复杂度相比于常规程序会高许多，因为解决了最终问题，代码在命名和堆结构上也看不出漏洞
  它是成熟句法解释器方案

  zExpression特点
  完整的单步原子化操作
  完整的符号优先级后处理
  能预处理字面错误，并反馈错误发生在哪
  能识别浮点和整数的自然数写法
  在编译以后，能形成原子化op代码，可以通过stream高速载入并运行，不限制cpu类型，可以兼容手机程序
  早年开发编译器时所编写的核心组件，套上不同的文本解析引擎，可以处理c/c++/pascal多种复杂句法


  更新日志
  首发代码创建 于2004年 创建人qq600585
  最后更新于2014年 可以兼容fpc编译器和最新的delphi xe，包括ios,osx,android,linux,win32

  有问题请来信
  by600585 qq邮箱

*)

interface

uses SysUtils, Variants, CoreClasses, OpCode, UnicodeMixedLib, TextParsing;

type
  // 文本符号操作类型，包括常用的数学逻辑类型缩进类型
  TSymbolOperation = (soAdd, soSub, soMul, soDiv, soMod, soIntDiv, soOr, soAnd, soXor,
    soEqual, soLessThan, soEqualOrLessThan, soGreaterThan, soEqualOrGreaterThan, soNotEqual,
    soShl, soShr,
    soBlockIndentBegin, soBlockIndentEnd,
    soPropParamIndentBegin, soPropParamIndentEnd,
    soDotSymbol, soCommaSymbol,
    soEolSymbol,
    soUnknow);
  TSymbolOperations = set of TSymbolOperation;

  TSymbolOperationType = record
    State: TSymbolOperation;
    Decl: umlSystemString;
  end;

  // 表达式数值类型
  // 解析器会识别写明的自然数，并且给自然数分类
  // 其次是操作符号，字符串，常量，变量，函数，嵌套
  TExpressionDeclType = (
    edtSymbol,
    edtBool, edtInt, edtInt64, edtUInt64, edtWord, edtByte, edtSmallInt, edtShortInt, edtUInt,
    edtSingle, edtDouble, edtCurrency,
    edtString,
    edtSpaceName,
    edtExpressionAsValue,

    edtUnknow);

  TExpressionDeclTypes = set of TExpressionDeclType;

  TSymbolExpression = class;

  TExpressionListData = record
    DeclType: TExpressionDeclType;
    charPos: Integer;
    Symbol: TSymbolOperation;
    Value: Variant;
    Expression: TSymbolExpression;
  end;

  PExpressionListData = ^TExpressionListData;

  // 已经剥离完成的核心数据结构
  TSymbolExpression = class(TCoreClassObject)
  protected
    FList: TCoreClassList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure PrintDebug(const detail: Boolean = False);
    function Decl: umlString;

    function GetCount(t: TExpressionDeclTypes): Integer;
    function GetSymbolCount(Operations: TSymbolOperations): Integer;
    function AvailValueCount: Integer;
    function Count: Integer;

    function InsertSymbol(idx: Integer; v: TSymbolOperation; charPos: Integer): PExpressionListData;
    function Insert(idx: Integer; v: TExpressionListData): PExpressionListData;
    procedure InsertExpression(idx: Integer; e: TSymbolExpression);
    procedure AddExpression(e: TSymbolExpression);

    function AddSymbol(v: TSymbolOperation; charPos: Integer): PExpressionListData;

    function AddBool(v: Boolean; charPos: Integer): PExpressionListData;
    function AddInt(v: Integer; charPos: Integer): PExpressionListData;
    function AddUInt(v: Cardinal; charPos: Integer): PExpressionListData;
    function AddInt64(v: int64; charPos: Integer): PExpressionListData;
    function AddUInt64(v: UInt64; charPos: Integer): PExpressionListData;
    function AddWord(v: Word; charPos: Integer): PExpressionListData;
    function AddByte(v: Byte; charPos: Integer): PExpressionListData;
    function AddSmallInt(v: SmallInt; charPos: Integer): PExpressionListData;
    function AddShortInt(v: ShortInt; charPos: Integer): PExpressionListData;
    function AddSingle(v: Single; charPos: Integer): PExpressionListData;
    function AddDouble(v: Double; charPos: Integer): PExpressionListData;
    function AddCurrency(v: Currency; charPos: Integer): PExpressionListData;
    function AddString(v: umlString; charPos: Integer): PExpressionListData;

    function AddSpaceName(v: umlString; charPos: Integer): PExpressionListData;

    function AddExpressionAsValue(Expression: TSymbolExpression; Symbol: TSymbolOperation; Value: Variant; charPos: Integer): PExpressionListData;
    function Add(v: TExpressionListData): PExpressionListData;

    procedure Delete(idx: Integer);

    function Last: PExpressionListData;
    function First: PExpressionListData;
    function IndexOf(p: PExpressionListData): Integer;

    function GetItems(index: Integer): PExpressionListData;
    property Items[index: Integer]: PExpressionListData read GetItems; default;
  end;

  // 用于获取代码中常量值的事件
  // 单独使用ParseTextExpressionAsSymbol时 将改事件设置为nil即可
  // 假如有pi,maxInteger这类需求时，根据DeclName判断名字，吻合时返回Value对应的3.141592654即可
  // 浮点类型ValType根据ieee规范，追求性能单浮点精度edtSingle，精确性双浮点edtDouble
  // RefObj和Obj是用于编译器知道当前蚂蚁爬到了依赖于哪个类的方法和变量上，并且返回迭代值，除非做编译器，否则单独使用时可以无视它们
  TExpressionGetValue = procedure(DeclName: umlString; RefObj: TCoreClassObject;
    var ValType: TExpressionDeclType; var Value: Variant; var Obj: TCoreClassObject) of object;

  // 核心函数：将文本表达式解析成符号表达式
  // 核心思路：采用双原子化处理，以蚂蚁化推进法处理字符和符号
  // 这里有两个原子点 其中 符号在前 字符在后 是一种情况，这是其中的原子1，第二种情况是字符在前，而符号再后，这是第二种原子2，两种情况相辅相成
  // 此函数复杂度偏高，遵循理论+学术所编写，无递归元素，且高效解析
  // zExpression 运行步骤的第一步就是得到一套符号表达，从而为下一步逻辑处理做出简化准备
  // TextEngClass 可以选择普通文本引擎，pascal文本引擎，c/c++文本引擎，它主要影响的是字符串的表达式，c的表示以"表示字符串，pascal表达式以'表示字符串
  // uName 是为上层编译器准备的，单元说明，类似unit name; include name; 编译时可以知道哪个原文件，便于编译预处理时查错和报错
  // ExpressionText 是表达式的文本内容
  // OnGetValue 在申明了常量和函数时，常量的值以此事件获取
  // 返回：符号表达式的TSymbolExpression类
function ParseTextExpressionAsSymbol(TextEngClass: TTextParsingClass; uName, ExpressionText: umlString; const OnGetValue: TExpressionGetValue): TSymbolExpression;

// zExpression 的核心逻辑第二步：对表达式做缩进优先级处理，这一步是为RebuildAllSymbol进行预处理，并不会得到最后符号顺序
// 将TSymbolExpression数据结构拆开，并且对带有符号优先级的符号做缩进预处理，该步骤带侦错功能
function RebuildLogicalPrioritySymbol(Exps: TSymbolExpression): TSymbolExpression;

// zExpression 的核心逻辑第三步，在符号缩进预处理完成以后，重新拆开表达式数据结构，并且重建一个带有缩进的严谨TSymbolExpression的缩进顺序，该步骤带侦错功能
function RebuildAllSymbol(Exps: TSymbolExpression): TSymbolExpression;

// zExpression 的核心逻辑第四步，根据RebuildAllSymbol后的严谨TSymbolExpression符号顺序，创建单步原子化执行
// 单步原子化执行的实现请参考opCode内容
function BuildAsOpCode(SymbExps: TSymbolExpression; uName: umlString; LineNo: Integer): TOpCode;

// 预处理一二三步以后输出opCode 并且运行opCode 最后返回一个值
// 该函数会消耗一定的硬件资源，高效运行建议一次性BuildAsOpCode，然后再用SaveToStream将opcode以二进制方式保存，使用时用LoadOpFromStream载入
function EvaluateExpressionValue(TextEngClass: TTextParsingClass; ExpressionText: umlString; const OnGetValue: TExpressionGetValue): Variant;

function VariantToExpressionDeclType(var v: Variant): TExpressionDeclType;

implementation

uses DoStatusIO, PascalStrings;

type
  TNumTextType = (nttBool, nttInt, nttInt64, nttUInt64, nttWord, nttByte, nttSmallInt, nttShortInt, nttUInt,
    nttSingle, nttDouble, nttCurrency, nttUnknow);

function GetNumTextType(s: umlString): TNumTextType; forward;
procedure _InitExpressionListData(var v: TExpressionListData); forward;
function D2Op(v: TExpressionDeclType): TOpValueType; forward;

const
  MethodFlags: TExpressionDeclTypes = ([edtSpaceName]);

  AllExpressionValueType: TExpressionDeclTypes = ([
    edtBool, edtInt, edtInt64, edtUInt64, edtWord, edtByte, edtSmallInt, edtShortInt, edtUInt,
    edtSingle, edtDouble, edtCurrency,
    edtString, edtSpaceName,
    edtExpressionAsValue]);

  SymbolOperationPriority: array [0 .. 4] of TSymbolOperations = (
    ([soOr, soAnd, soXor]),
    ([soEqual, soLessThan, soEqualOrLessThan, soGreaterThan, soEqualOrGreaterThan, soNotEqual]),
    ([soAdd, soSub]),
    ([soMul, soDiv, soMod, soIntDiv, soShl, soShr]),
    ([soDotSymbol, soCommaSymbol])
    );

  AllowPrioritySymbol: TSymbolOperations = ([
    soAdd, soSub, soMul, soDiv, soMod, soIntDiv, soOr, soAnd, soXor,
    soEqual, soLessThan, soEqualOrLessThan, soGreaterThan, soEqualOrGreaterThan, soNotEqual,
    soShl, soShr,
    soDotSymbol, soCommaSymbol]);

  OpLogicalSymbol: TSymbolOperations = ([
    soAdd, soSub, soMul, soDiv, soMod, soIntDiv, soOr, soAnd, soXor,
    soEqual, soLessThan, soEqualOrLessThan, soGreaterThan, soEqualOrGreaterThan, soNotEqual,
    soShl, soShr]);

  SymbolOperationTextDecl: array [TSymbolOperation] of TSymbolOperationType = (
    (State: soAdd; Decl: '+'),
    (State: soSub; Decl: '-'),
    (State: soMul; Decl: '*'),
    (State: soDiv; Decl: '/'),
    (State: soMod; Decl: ' mod '),
    (State: soIntDiv; Decl: ' div '),
    (State: soOr; Decl: ' or '),
    (State: soAnd; Decl: ' and '),
    (State: soXor; Decl: ' xor '),
    (State: soEqual; Decl: ' = '),
    (State: soLessThan; Decl: ' < '),
    (State: soEqualOrLessThan; Decl: ' <= '),
    (State: soGreaterThan; Decl: ' > '),
    (State: soEqualOrGreaterThan; Decl: ' => '),
    (State: soNotEqual; Decl: ' <> '),
    (State: soShl; Decl: ' shl '),
    (State: soShr; Decl: ' shr '),
    (State: soBlockIndentBegin; Decl: '('),
    (State: soBlockIndentEnd; Decl: ')'),
    (State: soPropParamIndentBegin; Decl: '['),
    (State: soPropParamIndentEnd; Decl: ']'),
    (State: soDotSymbol; Decl: '.'),
    (State: soCommaSymbol; Decl: ','),
    (State: soEolSymbol; Decl: ';'),
    (State: soUnknow; Decl: '?')
    );

  FlagTextDecl: array [TExpressionDeclType] of umlSystemString = (
    'Symbol',
    'bool', 'int', 'int64', 'UInt64', 'word', 'byte', 'smallInt', 'shortInt', 'uint',
    'float', 'double', 'Currency',
    'text', 'method',
    'Exps',
    'unknow'
    );

function GetNumTextType(s: umlString): TNumTextType;
type
  TValSym = (vsSymSub, vsSymAdd, vsSymAddSub, vsSymDollar, vsDot, vsDotBeforNum, vsDotAfterNum, vsNum, vsAtoF, vsE, vsUnknow);
var
  cnt: array [TValSym] of Integer;
  v  : TValSym;
  c  : umlChar;
  i  : Integer;
begin
  if umlSameText('true', s) or umlSameText('false', s) then
      Exit(nttBool);

  for v := low(TValSym) to high(TValSym) do
      cnt[v] := 0;

  for i := 1 to s.Len do
    begin
      c := s[i];
      if CharIn(c, [c0to9]) then
        begin
          Inc(cnt[vsNum]);
          if cnt[vsDot] > 0 then
              Inc(cnt[vsDotAfterNum]);
        end
      else if CharIn(c, [cLoAtoF, cHiAtoF]) then
        begin
          Inc(cnt[vsAtoF]);
          if CharIn(c, 'eE') then
              Inc(cnt[vsE]);
        end
      else if c = '.' then
        begin
          Inc(cnt[vsDot]);
          cnt[vsDotBeforNum] := cnt[vsNum];
        end
      else if CharIn(c, '-') then
        begin
          Inc(cnt[vsSymSub]);
          Inc(cnt[vsSymAddSub]);
        end
      else if CharIn(c, '+') then
        begin
          Inc(cnt[vsSymAdd]);
          Inc(cnt[vsSymAddSub]);
        end
      else if CharIn(c, '$') and (i = 1) then
          Inc(cnt[vsSymDollar])
      else
          Exit(nttUnknow);
    end;

  if cnt[vsDot] > 1 then
      Exit(nttUnknow);
  if cnt[vsSymDollar] > 1 then
      Exit(nttUnknow);
  if (cnt[vsSymDollar] = 0) and (cnt[vsNum] = 0) then
      Exit(nttUnknow);
  if (cnt[vsSymAdd] > 1) and (cnt[vsE] = 0) and (cnt[vsSymDollar] = 0) then
      Exit(nttUnknow);

  if (cnt[vsSymDollar] = 0) and
    ((cnt[vsDot] = 1) or ((cnt[vsE] = 1) and ((cnt[vsSymAddSub] >= 1) and (cnt[vsSymDollar] = 0)))) then
    begin
      if cnt[vsSymDollar] > 0 then
          Exit(nttUnknow);
      if (cnt[vsAtoF] <> cnt[vsE]) then
          Exit(nttUnknow);

      if cnt[vsE] = 1 then
        begin
          Result := nttDouble
        end
      else if ((cnt[vsDotBeforNum] > 0)) and (cnt[vsDotAfterNum] > 0) then
        begin
          if cnt[vsDotAfterNum] < 5 then
              Result := nttCurrency
          else if cnt[vsNum] > 7 then
              Result := nttDouble
          else
              Result := nttSingle;
        end
      else
          Exit(nttUnknow);
    end
  else
    begin
      if cnt[vsSymDollar] = 1 then
        begin
          if cnt[vsSymSub] > 0 then
            begin
              if cnt[vsNum] < 2 then
                  Result := nttShortInt
              else if cnt[vsNum] < 4 then
                  Result := nttSmallInt
              else if cnt[vsNum] < 7 then
                  Result := nttInt
              else if cnt[vsNum] < 13 then
                  Result := nttInt64
              else
                  Result := nttUnknow;
            end
          else
            begin
              if cnt[vsNum] < 3 then
                  Result := nttByte
              else if cnt[vsNum] < 5 then
                  Result := nttWord
              else if cnt[vsNum] < 8 then
                  Result := nttUInt
              else if cnt[vsNum] < 14 then
                  Result := nttUInt64
              else
                  Result := nttUnknow;
            end;
        end
      else if cnt[vsAtoF] > 0 then
          Exit(nttUnknow)
      else if cnt[vsSymSub] > 0 then
        begin
          if cnt[vsNum] < 3 then
              Result := nttShortInt
          else if cnt[vsNum] < 5 then
              Result := nttSmallInt
          else if cnt[vsNum] < 8 then
              Result := nttInt
          else if cnt[vsNum] < 15 then
              Result := nttInt64
          else
              Result := nttUnknow;
        end
      else
        begin
          if cnt[vsNum] < 3 then
              Result := nttByte
          else if cnt[vsNum] < 5 then
              Result := nttWord
          else if cnt[vsNum] < 8 then
              Result := nttUInt
          else if cnt[vsNum] < 16 then
              Result := nttUInt64
          else
              Result := nttUnknow;
        end;
    end;
end;

procedure _InitExpressionListData(var v: TExpressionListData);
begin
  v.DeclType := edtUnknow;
  v.charPos := -1;
  v.Symbol := soUnknow;
  v.Value := NULL;
  v.Expression := nil;
end;

function D2Op(v: TExpressionDeclType): TOpValueType;
begin
  case v of
    edtBool: Result := ovtBool;
    edtInt: Result := ovtInt;
    edtInt64: Result := ovtInt64;
    edtUInt64: Result := ovtUInt64;
    edtWord: Result := ovtWord;
    edtByte: Result := ovtByte;
    edtSmallInt: Result := ovtSmallInt;
    edtShortInt: Result := ovtShortInt;
    edtUInt: Result := ovtUInt;
    edtSingle: Result := ovtSingle;
    edtDouble: Result := ovtDouble;
    edtCurrency: Result := ovtCurrency;
    edtString: Result := ovtString;
    edtSpaceName: Result := ovtSpaceName;
    else Result := ovtUnknow;
  end;
end;

function ParseTextExpressionAsSymbol(TextEngClass: TTextParsingClass; uName, ExpressionText: umlString; const OnGetValue: TExpressionGetValue): TSymbolExpression;

type
  TExpressionParsingState = set of (esFirst, esWaitOp, esWaitIndentEnd, esWaitPropParamIndentEnd, esWaitValue);

var
  ParsingEng                   : TTextParsing;
  cPos, bPos, ePos             : Integer;
  State                        : TExpressionParsingState;
  BlockIndentCnt, PropIndentCnt: Integer;
  ParseSuccessed               : Boolean;
  Container                    : TSymbolExpression;

  RefObj: TCoreClassObject;

  procedure PrintError(const s: umlString);
  begin
    DoStatus(ParsingEng.ParsingData.Text.Text);
    if s = '' then
        DoStatus('expression error', [])
    else
        DoStatus('expression error %s', [s.Text]);
    DoStatus('');
  end;

  function GetDeclValue(DeclName: umlString; var v: Variant; var Obj: TCoreClassObject): TExpressionDeclType;
  begin
    if Assigned(OnGetValue) then
      begin
        OnGetValue(DeclName, RefObj, Result, v, Obj);
      end
    else
      begin
        v := DeclName.Text;
        Result := edtSpaceName;
        // Result:=edtUnknow;
      end;
  end;

  function ParseOperationState: TSymbolOperation;
  var
    c   : umlChar;
    Decl: umlString;
  begin
    Result := soUnknow;

    if not(esWaitOp in State) then
      begin
        PrintError('');
        Exit;
      end;

    while cPos <= ParsingEng.ParsingData.Text.Len do
      begin
        c := ParsingEng.ParsingData.Text[cPos];
        bPos := cPos;

        if CharIn(c, ')') then
          begin
            if (esWaitIndentEnd in State) then
              begin
                Dec(BlockIndentCnt);
                if BlockIndentCnt < 0 then
                  begin
                    PrintError('');
                    Exit;
                  end
                else if BlockIndentCnt = 0 then
                    State := State - [esWaitIndentEnd];
                State := State + [esWaitOp];
                Inc(cPos);
                Result := soBlockIndentEnd;
                Exit;
              end
            else
              begin
                PrintError('');
                Exit;
              end;
          end;

        if CharIn(c, '(') then
          begin
            Inc(BlockIndentCnt);
            Inc(cPos);
            Result := soBlockIndentBegin;
            State := State - [esWaitOp];
            State := State + [esWaitValue, esWaitIndentEnd];
            Exit;
          end;

        if (CharIn(c, ';')) then
          begin
            ParseSuccessed := (BlockIndentCnt = 0) and (PropIndentCnt = 0);
            if not ParseSuccessed then
                PrintError('');
            State := State - [esWaitOp];
            Result := soEolSymbol;
            Exit;
          end;

        if CharIn(c, '[') then
          begin
            Inc(PropIndentCnt);
            Inc(cPos);
            Result := soPropParamIndentBegin;
            State := State - [esWaitOp];
            State := State + [esWaitValue, esWaitPropParamIndentEnd];
            Exit;
          end;

        if CharIn(c, ']') then
          begin
            if (esWaitPropParamIndentEnd in State) then
              begin
                Dec(PropIndentCnt);
                if PropIndentCnt < 0 then
                  begin
                    PrintError('');
                    Exit;
                  end
                else if PropIndentCnt = 0 then
                    State := State - [esWaitPropParamIndentEnd];
                State := State + [esWaitOp];
                Inc(cPos);
                Result := soPropParamIndentEnd;
                Exit;
              end
            else
              begin
                PrintError('');
                Exit;
              end;
          end;

        if (ParsingEng.ComparePosStr(cPos, '>=')) or (ParsingEng.ComparePosStr(cPos, '=>')) then
          begin
            Result := soEqualOrGreaterThan;
            State := State - [esWaitOp];
            State := State + [esWaitValue];
            Inc(cPos, 2);
            Exit;
          end;
        if (ParsingEng.ComparePosStr(cPos, '<=')) or (ParsingEng.ComparePosStr(cPos, '=<')) then
          begin
            Result := soEqualOrLessThan;
            State := State - [esWaitOp];
            State := State + [esWaitValue];
            Inc(cPos, 2);
            Exit;
          end;
        if (ParsingEng.ComparePosStr(cPos, '<>')) or (ParsingEng.ComparePosStr(cPos, '><')) or (ParsingEng.ComparePosStr(cPos, '!=')) then
          begin
            Result := soNotEqual;
            State := State - [esWaitOp];
            State := State + [esWaitValue];
            Inc(cPos, 2);
            Exit;
          end;
        if (ParsingEng.ComparePosStr(cPos, '==')) then
          begin
            Result := soEqual;
            State := State - [esWaitOp];
            State := State + [esWaitValue];
            Inc(cPos, 2);
            Exit;
          end;
        if (ParsingEng.ComparePosStr(cPos, '&&')) then
          begin
            Result := soAnd;
            State := State - [esWaitOp];
            State := State + [esWaitValue];
            Inc(cPos, 2);
            Exit;
          end;
        if (ParsingEng.ComparePosStr(cPos, '||')) then
          begin
            Result := soOr;
            State := State - [esWaitOp];
            State := State + [esWaitValue];
            Inc(cPos, 2);
            Exit;
          end;

        if CharIn(c, '+-*/=><.,&|') then
          begin
            if c = '+' then
                Result := soAdd
            else if c = '-' then
                Result := soSub
            else if c = '*' then
                Result := soMul
            else if c = '/' then
                Result := soDiv
            else if c = '=' then
                Result := soEqual
            else if c = '>' then
                Result := soGreaterThan
            else if c = '<' then
                Result := soLessThan
            else if c = '.' then
                Result := soDotSymbol
            else if c = ',' then
                Result := soCommaSymbol
            else if c = '&' then
                Result := soAnd
            else if c = '|' then
                Result := soOr;
            State := State - [esWaitOp];
            State := State + [esWaitValue];
            Inc(cPos);
            Exit;
          end;

        if (ParsingEng.IsAscii(cPos)) then
          begin
            bPos := cPos;
            ePos := ParsingEng.GetAsciiEndPos(cPos);
            Decl := ParsingEng.GetStr(bPos, ePos);

            if umlSameText('or', Decl) then
                Result := soOr
            else if umlSameText('and', Decl) then
                Result := soAnd
            else if umlSameText('xor', Decl) then
                Result := soXor
            else if umlSameText('div', Decl) then
                Result := soIntDiv
            else if umlSameText('idiv', Decl) then
                Result := soIntDiv
            else if umlSameText('intdiv', Decl) then
                Result := soIntDiv
            else if umlSameText('fdiv', Decl) then
                Result := soDiv
            else if umlSameText('floatdiv', Decl) then
                Result := soDiv
            else if umlSameText('mod', Decl) then
                Result := soMod
            else if umlSameText('shl', Decl) then
                Result := soShl
            else if umlSameText('shr', Decl) then
                Result := soShr
            else
              begin
                PrintError('');
                Result := soUnknow;
                Exit;
              end;

            cPos := ePos;
            State := State - [esWaitOp];
            State := State + [esWaitValue];
            Exit;
          end;

        if ParsingEng.IsSymbol(cPos) or ParsingEng.IsNumber(cPos) then
          begin
            PrintError('no symbol');
            Exit;
          end;

        Inc(cPos);
      end;
  end;

  procedure ParseValue;
  label WaitSymbol;
  var
    c                                      : umlChar;
    Decl                                   : umlString;
    OpState                                : TSymbolOperation;
    IsNumber, IsAscii, IsTextDecl, IsSymbol: Boolean;
    rv                                     : Variant;
    robj                                   : TCoreClassObject;
  begin
    while cPos <= ParsingEng.Len do
      begin
        c := ParsingEng[cPos];
        IsNumber := ParsingEng.IsNumber(cPos);
        IsTextDecl := ParsingEng.IsTextDecl(cPos);
        IsAscii := ParsingEng.IsAscii(cPos);
        IsSymbol := ParsingEng.IsSymbol(cPos);

        if (not(esWaitOp in State)) and (IsNumber or IsTextDecl or IsAscii) then
          begin
            if not((esWaitValue in State) or (esFirst in State)) then
              begin
                PrintError('');
                Exit;
              end;
            bPos := cPos;
            if IsNumber then
                ePos := ParsingEng.GetNumberEndPos(cPos)
            else if IsTextDecl then
                ePos := ParsingEng.GetTextDeclEndPos(cPos)
            else
                ePos := ParsingEng.GetAsciiEndPos(cPos);
            cPos := ePos;

            Decl := umlTrimSpace(ParsingEng.GetStr(bPos, ePos));
            if IsNumber then
              begin
                case GetNumTextType(Decl) of
                  nttBool: Container.AddBool(StrToBool(Decl.Text), bPos);
                  nttInt: Container.AddInt(StrToInt(Decl.Text), bPos);
                  nttInt64: Container.AddInt64(StrToInt64(Decl.Text), bPos);
                  {$IFDEF FPC}
                  nttUInt64: Container.AddUInt64(StrToQWord(Decl.Text), bPos);
                  {$ELSE}
                  nttUInt64: Container.AddUInt64(StrToUInt64(Decl.Text), bPos);
                  {$ENDIF}
                  nttWord: Container.AddWord(StrToInt(Decl.Text), bPos);
                  nttByte: Container.AddByte(StrToInt(Decl.Text), bPos);
                  nttSmallInt: Container.AddSmallInt(StrToInt(Decl.Text), bPos);
                  nttShortInt: Container.AddShortInt(StrToInt(Decl.Text), bPos);
                  nttUInt: Container.AddUInt(StrToInt(Decl.Text), bPos);
                  nttSingle: Container.AddSingle(StrToFloat(Decl.Text), bPos);
                  nttDouble: Container.AddDouble(StrToFloat(Decl.Text), bPos);
                  nttCurrency: Container.AddCurrency(StrToFloat(Decl.Text), bPos);
                  else
                    begin
                      PrintError(Format('number expression "%s" Illegal', [Decl.Text]));
                      Exit;
                    end;
                end;
              end
            else if IsTextDecl then
              begin
                Container.AddString(ParsingEng.GetTextBody(Decl), bPos);
              end
            else
              begin
                case GetNumTextType(Decl) of
                  nttBool: Container.AddBool(StrToBool(Decl.Text), bPos);
                  nttInt: Container.AddInt(StrToInt(Decl.Text), bPos);
                  nttInt64: Container.AddInt64(StrToInt64(Decl.Text), bPos);
                  {$IFDEF FPC}
                  nttUInt64: Container.AddUInt64(StrToQWord(Decl.Text), bPos);
                  {$ELSE}
                  nttUInt64: Container.AddUInt64(StrToUInt64(Decl.Text), bPos);
                  {$ENDIF}
                  nttWord: Container.AddWord(StrToInt(Decl.Text), bPos);
                  nttByte: Container.AddByte(StrToInt(Decl.Text), bPos);
                  nttSmallInt: Container.AddSmallInt(StrToInt(Decl.Text), bPos);
                  nttShortInt: Container.AddShortInt(StrToInt(Decl.Text), bPos);
                  nttUInt: Container.AddUInt(StrToInt(Decl.Text), bPos);
                  nttSingle: Container.AddSingle(StrToFloat(Decl.Text), bPos);
                  nttDouble: Container.AddDouble(StrToFloat(Decl.Text), bPos);
                  nttCurrency: Container.AddCurrency(StrToFloat(Decl.Text), bPos);
                  else
                    begin
                      // add method/const,function.... to container
                      // wait imp
                      case GetDeclValue(Decl, rv, robj) of
                        edtBool: Container.AddBool(rv, bPos);
                        edtInt: Container.AddInt(rv, bPos);
                        edtInt64: Container.AddInt64(rv, bPos);
                        edtUInt64: Container.AddUInt64(rv, bPos);
                        edtWord: Container.AddWord(rv, bPos);
                        edtByte: Container.AddByte(rv, bPos);
                        edtSmallInt: Container.AddSmallInt(rv, bPos);
                        edtShortInt: Container.AddShortInt(rv, bPos);
                        edtUInt: Container.AddUInt(rv, bPos);
                        edtSingle: Container.AddSingle(rv, bPos);
                        edtDouble: Container.AddDouble(rv, bPos);
                        edtCurrency: Container.AddCurrency(rv, bPos);
                        edtString: Container.AddString(umlSystemString(rv), bPos);
                        edtSpaceName: Container.AddSpaceName(umlSystemString(rv), bPos);
                        else
                          begin
                            PrintError(Format('define "%s" Illegal', [Decl.Text]));
                            Exit;
                          end;
                      end;
                    end;
                end;
              end;

          WaitSymbol:
            State := State - [esWaitValue, esFirst];
            State := State + [esWaitOp];

            while True do
              begin
                OpState := ParseOperationState;

                if not(OpState in [soUnknow, soEolSymbol]) then
                    Container.AddSymbol(OpState, bPos);

                case OpState of
                  soEolSymbol:
                    Exit;
                  soDotSymbol:
                    begin
                      // dot
                      ParseValue;
                      Exit;
                    end;
                  soCommaSymbol:
                    begin
                      // comma
                      if (BlockIndentCnt = 0) and (PropIndentCnt = 0) then
                        begin
                          PrintError('"," only work in parameter');
                          Exit;
                        end;

                      ParseValue;
                      Exit;
                    end;
                  soPropParamIndentBegin:
                    begin
                      if IsAscii then
                        begin
                          // property[]
                          ParseValue;
                          Exit;
                        end
                      else if IsTextDecl then
                        begin
                          // 'text...'[]
                          ParseValue;
                          Exit;
                        end
                      else
                        begin
                          PrintError('property name Illegal ' + Decl);
                          Exit;
                        end;
                    end;
                  soPropParamIndentEnd, soBlockIndentEnd:
                    Continue;
                  soBlockIndentBegin:
                    begin
                      if IsAscii then
                        begin
                          // function()
                          ParseValue;
                          Exit;
                        end
                      else
                        begin
                          PrintError('function name Illegal ' + Decl);
                          Exit;
                        end;
                    end;
                  soUnknow:
                    begin
                      ParseSuccessed := (cPos >= ParsingEng.ParsingData.Len) and (BlockIndentCnt = 0) and (PropIndentCnt = 0);
                      if not ParseSuccessed then
                          PrintError('operaton symbol Illegal');
                      Exit;
                    end;
                  else
                    begin
                      ParseValue;
                      Exit;
                    end;
                end;
              end;
          end;

        if (IsSymbol) then
          begin
            if (CharIn(c, '(')) then
              begin
                if (esFirst in State) then
                  begin
                    Inc(BlockIndentCnt);
                    State := State + [esWaitIndentEnd];
                    Inc(cPos);

                    Container.AddSymbol(soBlockIndentBegin, cPos - 1);
                    ParseValue;
                    Exit;
                  end;

                if (esWaitValue in State) then
                  begin
                    Inc(BlockIndentCnt);
                    State := State + [esWaitIndentEnd];
                    Inc(cPos);

                    Container.AddSymbol(soBlockIndentBegin, cPos - 1);
                    ParseValue;
                    Exit;
                  end;

                PrintError('');
                Exit;
              end
            else if (CharIn(c, ')')) then
              begin
                if not(esWaitIndentEnd in State) then
                  begin
                    PrintError('Illegal symbol "' + c + '"');
                    Exit;
                  end;
                Dec(BlockIndentCnt);
                if BlockIndentCnt < 0 then
                  begin
                    PrintError('Illegal symbol "' + c + '"');
                    Exit;
                  end
                else if BlockIndentCnt = 0 then
                  begin
                    State := State - [esWaitIndentEnd];
                    if Container.AvailValueCount = 0 then
                        Container.Clear;
                  end;

                PrintError('Illegal symbol "' + c + '"');
                Exit;

                Inc(cPos);
                Container.AddSymbol(soBlockIndentEnd, cPos - 1);
                goto WaitSymbol;
                Exit;
              end
            else if CharIn(c, '[') then
              begin
                PrintError('Illegal symbol "' + c + '"');
                Exit;
              end
            else if CharIn(c, ']') then
              begin
                if not(esWaitPropParamIndentEnd in State) then
                  begin
                    PrintError('Illegal symbol "' + c + '"');
                    Exit;
                  end;
                Dec(PropIndentCnt);
                if PropIndentCnt < 0 then
                  begin
                    PrintError('Illegal symbol "' + c + '"');
                    Exit;
                  end
                else if PropIndentCnt = 0 then
                  begin
                    State := State - [esWaitPropParamIndentEnd];
                    if not(esFirst in State) then
                      begin
                        Inc(cPos);
                        Container.AddSymbol(soPropParamIndentEnd, cPos - 1);
                        goto WaitSymbol;
                      end;
                  end;

                Inc(cPos);

                Container.AddSymbol(soPropParamIndentEnd, cPos - 1);
                goto WaitSymbol;
              end
            else if (CharIn(c, ';')) then
              begin
                ParseSuccessed := (BlockIndentCnt = 0) and (PropIndentCnt = 0);
                if not ParseSuccessed then
                    PrintError('');
                Exit;
              end
            else
              begin
                PrintError('');
                Exit;
              end;
          end;

        Inc(cPos);
      end;
  end;

begin
  Result := nil;

  with TextEngClass.Create(ExpressionText, tsPascal) do
    begin
      ParsingEng := TextEngClass.Create(GetDeletedCommentText, tsPascal);
      Free;
    end;

  if ParsingEng.ParsingData.Len < 1 then
      Exit;

  cPos := 1;
  BlockIndentCnt := 0;
  PropIndentCnt := 0;
  ParseSuccessed := False;
  State := [esFirst];

  Container := TSymbolExpression.Create;

  ParseValue;

  if ParseSuccessed then
      Result := Container
  else
      DisposeObject(Container);
  DisposeObject(ParsingEng);
end;

function RebuildLogicalPrioritySymbol(Exps: TSymbolExpression): TSymbolExpression;
  function SymbolPriority(s1, s2: TSymbolOperation): Integer;

    function FindSymbol(s: TSymbolOperation): Integer;
    var
      i: Integer;
    begin
      for i := low(SymbolOperationPriority) to high(SymbolOperationPriority) do
        if s in SymbolOperationPriority[i] then
            Exit(i);
      raise Exception.Create('no define symbol');
    end;

  begin
    if (s1 in [soUnknow, soCommaSymbol]) or (s2 in [soUnknow, soCommaSymbol]) then
        Exit(0);
    Result := FindSymbol(s2) - FindSymbol(s1);
  end;

var
  SymbolIndex  : Integer;
  newExpression: TSymbolExpression;
  ParseAborted : Boolean;

  procedure PostError(const s: umlString);
  begin
    ParseAborted := True;
    if s <> '' then
        DoStatus(Format('Priority symbol failed : %s', [s.Text]))
    else
        DoStatus('Priority symbol failed');
  end;

  procedure ProcessSymbol(OwnerSym: TSymbolOperation);
  var
    p1, p2, startIndent, lastIndent            : PExpressionListData;
    lastSym, lastIndentSym                     : TSymbolOperation;
    LastSymbolPriority, LastOwnerSymbolPriority: Integer;
  begin
    if ParseAborted then
        Exit;
    if SymbolIndex >= Exps.Count then
        Exit;

    if newExpression.Count > 0 then
        startIndent := newExpression.Last
    else
        startIndent := nil;

    lastSym := OwnerSym;
    lastIndent := nil;
    lastIndentSym := OwnerSym;

    while True do
      begin
        if ParseAborted then
            Break;

        if SymbolIndex >= Exps.Count then
            Break;

        p1 := Exps[SymbolIndex];

        if (p1^.DeclType in AllExpressionValueType) then
          begin
            Inc(SymbolIndex);
            if SymbolIndex >= Exps.Count then
              begin
                newExpression.Add(p1^);
                Break;
              end;

            p2 := Exps[SymbolIndex];

            if (p1^.DeclType in MethodFlags) and (p2^.DeclType = edtExpressionAsValue) then
              begin
                newExpression.Add(p1^);
                newExpression.Add(p2^);
              end
            else if p2^.DeclType = edtSymbol then
              begin
                if p2^.Symbol in AllowPrioritySymbol then
                  begin
                    LastOwnerSymbolPriority := SymbolPriority(p2^.Symbol, OwnerSym);
                    LastSymbolPriority := SymbolPriority(p2^.Symbol, lastSym);

                    if LastOwnerSymbolPriority > 0 then
                      begin
                        newExpression.Add(p1^);
                        Break;
                      end;

                    if LastSymbolPriority < 0 then
                      begin
                        lastIndent := newExpression.AddSymbol(soBlockIndentBegin, p1^.charPos);
                        lastIndentSym := lastSym;
                        newExpression.Add(p1^);
                        newExpression.Add(p2^);

                        Inc(SymbolIndex);
                        ProcessSymbol(p2^.Symbol);
                        newExpression.AddSymbol(soBlockIndentEnd, p2^.charPos);

                        Continue;
                      end
                    else if LastSymbolPriority > 0 then
                      begin
                        if startIndent = nil then
                            startIndent := newExpression.First;

                        newExpression.InsertSymbol(newExpression.IndexOf(startIndent), soBlockIndentBegin, startIndent^.charPos);
                        newExpression.Add(p1^);
                        newExpression.AddSymbol(soBlockIndentEnd, p2^.charPos);
                        newExpression.Add(p2^);
                      end
                    else
                      begin
                        newExpression.Add(p1^);
                        newExpression.Add(p2^);
                      end;
                    lastSym := p2^.Symbol;
                  end
                else
                  begin
                    PostError(SymbolOperationTextDecl[p2^.Symbol].Decl);
                    Exit;
                  end;
              end;
          end
        else if (p1^.DeclType = edtSymbol) then
          begin
            Inc(SymbolIndex);
            if SymbolIndex >= Exps.Count then
              begin
                newExpression.Add(p1^);
                Break;
              end;

            p2 := Exps[SymbolIndex];

            if (p2^.DeclType in AllExpressionValueType) then
              begin
                if p1^.Symbol in AllowPrioritySymbol then
                  begin
                    LastSymbolPriority := SymbolPriority(p1^.Symbol, lastIndentSym);

                    if LastSymbolPriority < 0 then
                      begin
                        newExpression.InsertSymbol(newExpression.IndexOf(lastIndent), soBlockIndentBegin, lastIndent^.charPos);
                        newExpression.Add(p1^);
                        lastSym := p1^.Symbol;
                        ProcessSymbol(p1^.Symbol);
                        newExpression.AddSymbol(soBlockIndentEnd, p2^.charPos);
                        Continue;
                      end
                    else
                      begin
                        newExpression.Add(p1^);
                        Continue;
                      end;
                  end
                else
                  begin
                    PostError(SymbolOperationTextDecl[p1^.Symbol].Decl);
                    Exit;
                  end;
              end
            else
              begin
                PostError('expression structor Illegal');
                Exit;
              end;
          end;

        Inc(SymbolIndex);
      end;
  end;

begin
  Result := nil;
  if Exps.AvailValueCount = 0 then
      Exit;
  if Exps.GetSymbolCount([
    soBlockIndentBegin, soBlockIndentEnd,
    soPropParamIndentBegin, soPropParamIndentEnd,
    soEolSymbol, soUnknow]) > 0 then
    begin
      PostError('Illegal symbol');
      Exit;
    end;

  SymbolIndex := 0;
  newExpression := TSymbolExpression.Create;
  ParseAborted := False;

  ProcessSymbol(soUnknow);

  if ParseAborted then
      newExpression.Free
  else
      Result := newExpression;
end;

function RebuildAllSymbol(Exps: TSymbolExpression): TSymbolExpression;
var
  SymbolIndex    : Integer;
  ParseAborted   : Boolean;
  AutoFreeObjList: TCoreClassListForObj;

  procedure PostError(const s: umlString);
  begin
    ParseAborted := True;
    if s <> '' then
        DoStatus(Format('indent symbol failed : %s', [s.Text]))
    else
        DoStatus('indent symbol failed');
  end;

  function ProcessIndent(OwnerIndentSym: TSymbolOperation): TSymbolExpression;
  var
    p1, p2          : PExpressionListData;
    LocalExp, ResExp: TSymbolExpression;
  begin
    LocalExp := TSymbolExpression.Create;
    Result := LocalExp;
    AutoFreeObjList.Add(LocalExp);
    while True do
      begin
        if SymbolIndex >= Exps.Count then
            Break;

        p1 := Exps[SymbolIndex];

        if (p1^.DeclType in [edtSymbol]) then
          begin
            if p1^.Symbol in [soBlockIndentBegin, soPropParamIndentBegin] then
              begin
                Inc(SymbolIndex);

                ResExp := ProcessIndent(p1^.Symbol);
                LocalExp.AddExpressionAsValue(ResExp, p1^.Symbol, SymbolOperationTextDecl[p1^.Symbol].Decl, p1^.charPos);

                if SymbolIndex >= Exps.Count then
                  begin
                    PostError('indent Illegal');
                    Exit;
                  end;
              end
            else if ((OwnerIndentSym = soBlockIndentBegin) and (p1^.Symbol = soBlockIndentEnd)) or
              ((OwnerIndentSym = soPropParamIndentBegin) and (p1^.Symbol = soPropParamIndentEnd)) then
              begin
                Exit;
              end
            else if p1^.Symbol in [soCommaSymbol] then
              begin
                LocalExp.Add(p1^);
              end
            else
              begin
                LocalExp.Add(p1^);
              end;
          end
        else if (p1^.DeclType in AllExpressionValueType) then
          begin
            Inc(SymbolIndex);
            if SymbolIndex >= Exps.Count then
              begin
                LocalExp.Add(p1^);
                Break;
              end;

            p2 := Exps[SymbolIndex];

            if p2^.DeclType = edtSymbol then
              begin
                if (p2^.Symbol in [soBlockIndentBegin, soPropParamIndentBegin]) then
                  begin
                    if not(p1^.DeclType in MethodFlags) then
                      begin
                        PostError('method Illegal');
                        Exit;
                      end;

                    LocalExp.Add(p1^);
                    Inc(SymbolIndex);

                    ResExp := ProcessIndent(p2^.Symbol);
                    LocalExp.AddExpressionAsValue(ResExp, p2^.Symbol, SymbolOperationTextDecl[p2^.Symbol].Decl, p2^.charPos);

                    if SymbolIndex >= Exps.Count then
                      begin
                        PostError('indent Illegal');
                        Exit;
                      end;
                  end
                else if ((OwnerIndentSym = soBlockIndentBegin) and (p2^.Symbol = soBlockIndentEnd)) or
                  ((OwnerIndentSym = soPropParamIndentBegin) and (p2^.Symbol = soPropParamIndentEnd)) then
                  begin
                    LocalExp.Add(p1^);
                    Exit;
                  end
                else
                  begin
                    LocalExp.Add(p1^);
                    LocalExp.Add(p2^);
                  end;
              end
            else
              begin
                PostError('expression structor Illegal');
                Exit;
              end;
          end;

        Inc(SymbolIndex);
      end;
  end;

  function ProcessPriority(_e: TSymbolExpression): TSymbolExpression;
  var
    i        : Integer;
    e, ResExp: TSymbolExpression;
    p        : PExpressionListData;
  begin
    Result := TSymbolExpression.Create;
    AutoFreeObjList.Add(Result);
    e := RebuildLogicalPrioritySymbol(_e);
    if e = nil then
      begin
        PostError('parse priority failed');
        Exit;
      end;
    AutoFreeObjList.Add(e);

    for i := 0 to e.Count - 1 do
      begin
        p := e[i];
        if p^.DeclType = edtExpressionAsValue then
          begin
            case p^.Symbol of
              soBlockIndentBegin:
                begin
                  Result.AddSymbol(soBlockIndentBegin, p^.charPos);
                  ResExp := ProcessPriority(p^.Expression);
                  if ResExp <> nil then
                      Result.AddExpression(ResExp);
                  Result.AddSymbol(soBlockIndentEnd, p^.charPos);
                end;
              soPropParamIndentBegin:
                begin
                  Result.AddSymbol(soPropParamIndentBegin, p^.charPos);
                  ResExp := ProcessPriority(p^.Expression);
                  if ResExp <> nil then
                      Result.AddExpression(ResExp);
                  Result.AddSymbol(soPropParamIndentEnd, p^.charPos);
                end;
              else
                begin
                  Break;
                end;
            end;
          end
        else
          begin
            Result.Add(p^);
          end;
      end;
  end;

  procedure FreeAutoObj;
  var
    i: Integer;
  begin
    for i := 0 to AutoFreeObjList.Count - 1 do
        DisposeObject(TSymbolExpression(AutoFreeObjList[i]));
    AutoFreeObjList.Clear;
  end;

var
  rse: TSymbolExpression;
begin
  Result := nil;
  SymbolIndex := 0;
  ParseAborted := False;
  AutoFreeObjList := TCoreClassListForObj.Create;

  rse := ProcessIndent(soUnknow);
  // rse.PrintDebug(False);

  rse := ProcessPriority(rse);
  if not ParseAborted then
    begin
      // rse.PrintDebug;
      AutoFreeObjList.Remove(rse);
      Result := rse;
    end;

  FreeAutoObj;
  DisposeObject(AutoFreeObjList);
end;

function BuildAsOpCode(SymbExps: TSymbolExpression; uName: umlString; LineNo: Integer): TOpCode;
var
  NewSymbExps : TSymbolExpression;
  SymbolIndex : Integer;
  BuildAborted: Boolean;
  OpContainer : TCoreClassListForObj;

  procedure PostError(const s: umlString);
  begin
    BuildAborted := True;
    if s <> '' then
        DoStatus(Format('build op failed : %s', [s.Text]))
    else
        DoStatus('build op failed');
  end;

  function NewOpValue(uName: umlString): TOpCode;
  begin
    Result := op_Value.Create(False);
    Result.ParsedInfo := uName;
    Result.ParsedLineNo := LineNo;
    OpContainer.Add(Result);
  end;

  function NewOpFromSym(sym: TSymbolOperation; uName: umlString): TOpCode;
  begin
    case sym of
      soAdd: Result := op_Add.Create(False);
      soSub: Result := op_Sub.Create(False);
      soMul: Result := op_Mul.Create(False);
      soDiv: Result := op_Div.Create(False);
      soMod: Result := op_Mod.Create(False);
      soIntDiv: Result := op_IntDiv.Create(False);
      soOr: Result := op_Or.Create(False);
      soAnd: Result := op_And.Create(False);
      soXor: Result := op_Xor.Create(False);
      soEqual: Result := op_Equal.Create(False);
      soLessThan: Result := op_LessThan.Create(False);
      soEqualOrLessThan: Result := op_EqualOrLessThan.Create(False);
      soGreaterThan: Result := op_GreaterThan.Create(False);
      soEqualOrGreaterThan: Result := op_EqualOrGreaterThan.Create(False);
      soNotEqual: Result := op_NotEqual.Create(False);
      soShl: Result := op_Shl.Create(False);
      soShr: Result := op_Shr.Create(False);
      else
        Result := nil;
    end;
    if Result <> nil then
      begin
        Result.ParsedInfo := uName;
        Result.ParsedLineNo := LineNo;

        OpContainer.Add(Result);
      end;
  end;

  function ProcessIndent(OwnerIndentSym: TSymbolOperation): TOpCode;
  var
    p1, p2               : PExpressionListData;
    LocalOp, OldOp, ResOp: TOpCode;
  begin
    LocalOp := nil;
    OldOp := nil;
    ResOp := nil;
    Result := nil;

    while True do
      begin
        if SymbolIndex >= NewSymbExps.Count then
          begin
            if LocalOp <> nil then
                Result := LocalOp;
            Break;
          end;

        p1 := NewSymbExps[SymbolIndex];

        if (p1^.DeclType in [edtSymbol]) then
          begin
            if p1^.Symbol in [soBlockIndentBegin, soPropParamIndentBegin] then
              begin
                Inc(SymbolIndex);
                ResOp := ProcessIndent(p1^.Symbol);
                if ResOp <> nil then
                  begin
                    if LocalOp <> nil then
                      begin
                        LocalOp.AddLink(ResOp);
                      end
                    else
                      begin
                        LocalOp := NewOpValue(uName);
                        LocalOp.AddLink(ResOp);
                      end;
                  end
                else
                  begin
                    PostError('logical cperotion Illegal');
                    Break;
                  end;
              end
            else if ((OwnerIndentSym = soBlockIndentBegin) and (p1^.Symbol = soBlockIndentEnd)) or
              ((OwnerIndentSym = soPropParamIndentBegin) and (p1^.Symbol = soPropParamIndentEnd)) then
              begin
                Result := LocalOp;
                Break;
              end
            else if p1^.Symbol in OpLogicalSymbol then
              begin
                if LocalOp <> nil then
                  begin
                    OldOp := LocalOp;
                    LocalOp := NewOpFromSym(p1^.Symbol, uName);
                    LocalOp.AddLink(OldOp);
                  end
                else
                  begin
                    PostError('logical cperotion Illegal');
                    Break;
                  end;
              end
            else
              begin
                PostError('logical cperotion Illegal');
                Break;
              end;
          end
        else if (p1^.DeclType in AllExpressionValueType) then
          begin
            Inc(SymbolIndex);
            if SymbolIndex >= NewSymbExps.Count then
              begin
                if LocalOp <> nil then
                  begin
                    LocalOp.AddValue(p1^.Value, D2Op(p1^.DeclType));
                  end
                else
                  begin
                    LocalOp := NewOpValue(uName);
                    LocalOp.AddValue(p1^.Value, D2Op(p1^.DeclType));
                  end;
                Result := LocalOp;
                Break;
              end;

            p2 := NewSymbExps[SymbolIndex];

            if p2^.DeclType = edtSymbol then
              begin
                if (p2^.Symbol in [soBlockIndentBegin, soPropParamIndentBegin]) then
                  begin
                    // method call
                    if not(p1^.DeclType in MethodFlags) then
                      begin
                        PostError('method Illegal');
                        Break;
                      end;

                    Inc(SymbolIndex);
                    ResOp := ProcessIndent(p2^.Symbol);

                  end
                else if ((OwnerIndentSym = soBlockIndentBegin) and (p2^.Symbol = soBlockIndentEnd)) or
                  ((OwnerIndentSym = soPropParamIndentBegin) and (p2^.Symbol = soPropParamIndentEnd)) then
                  begin
                    if LocalOp <> nil then
                      begin
                        LocalOp.AddValue(p1^.Value, D2Op(p1^.DeclType));
                      end
                    else
                      begin
                        LocalOp := NewOpValue(uName);
                        LocalOp.AddValue(p1^.Value, D2Op(p1^.DeclType));
                      end;
                    Result := LocalOp;
                    Break;
                  end
                else if p2^.Symbol in OpLogicalSymbol then
                  begin
                    if LocalOp <> nil then
                      begin
                        OldOp := LocalOp;
                        OldOp.AddValue(p1^.Value, D2Op(p1^.DeclType));
                        LocalOp := NewOpFromSym(p2^.Symbol, uName);
                        LocalOp.AddLink(OldOp);
                      end
                    else
                      begin
                        LocalOp := NewOpFromSym(p2^.Symbol, uName);
                        LocalOp.AddValue(p1^.Value, D2Op(p1^.DeclType));
                      end;
                  end
                else
                  begin
                    PostError('Illegal');
                    Break;
                  end;
              end
            else
              begin
                PostError('Illegal');
                Break;
              end;
          end;

        Inc(SymbolIndex);
      end;
  end;

  procedure ProcessOpContainer(successed: Boolean);
  var
    i: Integer;
  begin
    for i := 0 to OpContainer.Count - 1 do
      if successed then
          TOpCode(OpContainer[i]).DestoryTimeFreeLink := True
      else
          DisposeObject(TOpCode(OpContainer[i]));
    OpContainer.Clear;
  end;

begin
  Result := nil;
  NewSymbExps := RebuildAllSymbol(SymbExps);
  if NewSymbExps <> nil then
    begin
      if NewSymbExps.GetSymbolCount([soBlockIndentBegin, soPropParamIndentBegin]) =
        NewSymbExps.GetSymbolCount([soBlockIndentEnd, soPropParamIndentEnd]) then
        begin
          OpContainer := TCoreClassListForObj.Create;

          SymbolIndex := 0;
          BuildAborted := False;
          // NewSymbExps.PrintDebug(False);
          Result := ProcessIndent(soUnknow);
          ProcessOpContainer(Result <> nil);
          DisposeObject(OpContainer);
        end;
      DisposeObject(NewSymbExps);
    end;
end;

function EvaluateExpressionValue(TextEngClass: TTextParsingClass; ExpressionText: umlString; const OnGetValue: TExpressionGetValue): Variant;
var
  sym: TSymbolExpression;
  op : TOpCode;
  i  : Integer;
begin
  Result := NULL;
  sym := ParseTextExpressionAsSymbol(TextEngClass, 'Main', ExpressionText, OnGetValue);

  if sym <> nil then
    begin
      op := BuildAsOpCode(sym, 'Main', -1);
      if op <> nil then
        begin
          try
              Result := op.Execute;
          except
              Result := NULL;
          end;
          DisposeObject(op);
        end;
      DisposeObject(sym);
    end;
end;

function VariantToExpressionDeclType(var v: Variant): TExpressionDeclType;
begin
  case VarType(v) of
    varSmallint: Result := edtSmallInt;
    varInteger: Result := edtInt;
    varSingle: Result := edtSingle;
    varDouble: Result := edtDouble;
    varCurrency: Result := edtCurrency;
    varBoolean: Result := edtBool;
    varShortInt: Result := edtShortInt;
    varByte: Result := edtByte;
    varWord: Result := edtWord;
    varLongWord: Result := edtUInt;
    varInt64: Result := edtInt64;
    varUInt64: Result := edtUInt64;
    else
      begin
        if VarIsStr(v) then
            Result := edtString
        else
            Result := edtUnknow;
      end;
  end;
end;

constructor TSymbolExpression.Create;
begin
  inherited Create;
  FList := TCoreClassList.Create;
end;

destructor TSymbolExpression.Destroy;
begin
  Clear;
  DisposeObject(FList);
  inherited;
end;

procedure TSymbolExpression.Clear;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
      Dispose(PExpressionListData(FList[i]));

  FList.Clear;
end;

procedure TSymbolExpression.PrintDebug(const detail: Boolean);
var
  so: TSymbolOperation;
  f : TExpressionDeclType;
  s : umlString;
  c : Integer;
begin
  DoStatus(Decl.Text);

  if detail then
    begin
      s := '';
      for so := low(TSymbolOperation) to high(TSymbolOperation) do
        begin
          c := GetSymbolCount([so]);
          if c > 0 then
              s := s + Format('%s:%d'#32, [Trim(SymbolOperationTextDecl[so].Decl), c]);
        end;

      for f := low(TExpressionDeclType) to high(TExpressionDeclType) do
        begin
          c := GetCount([f]);
          if c > 0 then
              s := s + Format('%s:%d'#32, [FlagTextDecl[f], c]);
        end;
      DoStatus(s.Text + #13#10);
    end;
end;

function TSymbolExpression.Decl: umlString;
var
  i: Integer;
  p: PExpressionListData;
begin
  Result := '';
  for i := 0 to FList.Count - 1 do
    begin
      p := FList[i];
      case p^.DeclType of
        edtSymbol:
          Result := Result + SymbolOperationTextDecl[p^.Symbol].Decl;
        edtSingle, edtDouble, edtCurrency:
          Result := Result + FloatToStr(p^.Value);
        edtExpressionAsValue:
          begin
            case p^.Symbol of
              soBlockIndentBegin:
                Result := Format('%s%s%s%s',
                  [Result.Text,
                  SymbolOperationTextDecl[soBlockIndentBegin].Decl,
                  p^.Expression.Decl.Text,
                  SymbolOperationTextDecl[soBlockIndentEnd].Decl
                  ]);
              soPropParamIndentBegin:
                Result := Format('%s%s%s%s',
                  [Result.Text,
                  SymbolOperationTextDecl[soPropParamIndentBegin].Decl,
                  p^.Expression.Decl.Text,
                  SymbolOperationTextDecl[soPropParamIndentEnd].Decl
                  ]);
              else
                Result := Result + ' !error! ';
            end;
          end;
        edtUnknow: Result := Result + ' !error! ';
        else
          Result := Result + umlSystemString(p^.Value);
      end;
    end;
end;

function TSymbolExpression.GetCount(t: TExpressionDeclTypes): Integer;
var
  i: Integer;
  p: PExpressionListData;
begin
  Result := 0;
  for i := 0 to FList.Count - 1 do
    begin
      p := FList[i];
      if p^.DeclType in t then
          Inc(Result);
    end;
end;

function TSymbolExpression.GetSymbolCount(Operations: TSymbolOperations): Integer;
var
  i: Integer;
  p: PExpressionListData;
begin
  Result := 0;
  for i := 0 to FList.Count - 1 do
    begin
      p := FList[i];
      if p^.DeclType = edtSymbol then
        begin
          if p^.Symbol in Operations then
              Inc(Result);
        end;
    end;
end;

function TSymbolExpression.AvailValueCount: Integer;
begin
  Result := GetCount(AllExpressionValueType);
end;

function TSymbolExpression.Count: Integer;
begin
  Result := FList.Count;
end;

function TSymbolExpression.InsertSymbol(idx: Integer; v: TSymbolOperation; charPos: Integer): PExpressionListData;
var
  p: PExpressionListData;
begin
  new(p);
  _InitExpressionListData(p^);
  p^.DeclType := edtSymbol;
  p^.charPos := charPos;
  p^.Symbol := v;
  p^.Value := v;
  FList.Insert(idx, p);
  Result := p;
end;

function TSymbolExpression.Insert(idx: Integer; v: TExpressionListData): PExpressionListData;
var
  p: PExpressionListData;
begin
  new(p);
  p^ := v;
  FList.Insert(idx, p);
  Result := p;
end;

procedure TSymbolExpression.InsertExpression(idx: Integer; e: TSymbolExpression);
var
  newList: TCoreClassList;
  i      : Integer;
  p      : PExpressionListData;
begin
  newList := TCoreClassList.Create;
  newList.Capacity := e.FList.Count + FList.Count;

  for i := 0 to idx do
      newList.Add(FList[i]);

  for i := 0 to e.FList.Count - 1 do
    begin
      new(p);
      p^ := PExpressionListData(e.FList[i])^;
      newList.Add(p);
    end;

  for i := idx to FList.Count - 1 do
      newList.Add(FList[i]);

  DisposeObject(FList);
  FList := newList;
end;

procedure TSymbolExpression.AddExpression(e: TSymbolExpression);
var
  i: Integer;
  p: PExpressionListData;
begin
  for i := 0 to e.FList.Count - 1 do
    begin
      new(p);
      p^ := PExpressionListData(e.FList[i])^;
      FList.Add(p);
    end;
end;

function TSymbolExpression.AddSymbol(v: TSymbolOperation; charPos: Integer): PExpressionListData;
var
  p: PExpressionListData;
begin
  new(p);
  _InitExpressionListData(p^);
  p^.DeclType := edtSymbol;
  p^.charPos := charPos;
  p^.Symbol := v;
  FList.Add(p);
  Result := p;
end;

function TSymbolExpression.AddBool(v: Boolean; charPos: Integer): PExpressionListData;
var
  p: PExpressionListData;
begin
  new(p);
  _InitExpressionListData(p^);
  p^.DeclType := edtBool;
  p^.charPos := charPos;
  p^.Value := v;
  FList.Add(p);
  Result := p;
end;

function TSymbolExpression.AddInt(v: Integer; charPos: Integer): PExpressionListData;
var
  p: PExpressionListData;
begin
  new(p);
  _InitExpressionListData(p^);
  p^.DeclType := edtInt;
  p^.charPos := charPos;
  p^.Value := v;
  FList.Add(p);
  Result := p;
end;

function TSymbolExpression.AddUInt(v: Cardinal; charPos: Integer): PExpressionListData;
var
  p: PExpressionListData;
begin
  new(p);
  _InitExpressionListData(p^);
  p^.DeclType := edtUInt;
  p^.charPos := charPos;
  p^.Value := v;
  FList.Add(p);
  Result := p;
end;

function TSymbolExpression.AddInt64(v: int64; charPos: Integer): PExpressionListData;
var
  p: PExpressionListData;
begin
  new(p);
  _InitExpressionListData(p^);
  p^.DeclType := edtInt64;
  p^.charPos := charPos;
  p^.Value := v;
  FList.Add(p);
  Result := p;
end;

function TSymbolExpression.AddUInt64(v: UInt64; charPos: Integer): PExpressionListData;
var
  p: PExpressionListData;
begin
  new(p);
  _InitExpressionListData(p^);
  p^.DeclType := edtUInt64;
  p^.charPos := charPos;
  p^.Value := v;
  FList.Add(p);
  Result := p;
end;

function TSymbolExpression.AddWord(v: Word; charPos: Integer): PExpressionListData;
var
  p: PExpressionListData;
begin
  new(p);
  _InitExpressionListData(p^);
  p^.DeclType := edtWord;
  p^.charPos := charPos;
  p^.Value := v;
  FList.Add(p);
  Result := p;
end;

function TSymbolExpression.AddByte(v: Byte; charPos: Integer): PExpressionListData;
var
  p: PExpressionListData;
begin
  new(p);
  _InitExpressionListData(p^);
  p^.DeclType := edtByte;
  p^.charPos := charPos;
  p^.Value := v;
  FList.Add(p);
  Result := p;
end;

function TSymbolExpression.AddSmallInt(v: SmallInt; charPos: Integer): PExpressionListData;
var
  p: PExpressionListData;
begin
  new(p);
  _InitExpressionListData(p^);
  p^.DeclType := edtSmallInt;
  p^.charPos := charPos;
  p^.Value := v;
  FList.Add(p);
  Result := p;
end;

function TSymbolExpression.AddShortInt(v: ShortInt; charPos: Integer): PExpressionListData;
var
  p: PExpressionListData;
begin
  new(p);
  _InitExpressionListData(p^);
  p^.DeclType := edtShortInt;
  p^.charPos := charPos;
  p^.Value := v;
  FList.Add(p);
  Result := p;
end;

function TSymbolExpression.AddSingle(v: Single; charPos: Integer): PExpressionListData;
var
  p: PExpressionListData;
begin
  new(p);
  _InitExpressionListData(p^);
  p^.DeclType := edtSingle;
  p^.charPos := charPos;
  p^.Value := v;
  FList.Add(p);
  Result := p;
end;

function TSymbolExpression.AddDouble(v: Double; charPos: Integer): PExpressionListData;
var
  p: PExpressionListData;
begin
  new(p);
  _InitExpressionListData(p^);
  p^.DeclType := edtDouble;
  p^.charPos := charPos;
  p^.Value := v;
  FList.Add(p);
  Result := p;
end;

function TSymbolExpression.AddCurrency(v: Currency; charPos: Integer): PExpressionListData;
var
  p: PExpressionListData;
begin
  new(p);
  _InitExpressionListData(p^);
  p^.DeclType := edtCurrency;
  p^.charPos := charPos;
  p^.Value := v;
  FList.Add(p);
  Result := p;
end;

function TSymbolExpression.AddString(v: umlString; charPos: Integer): PExpressionListData;
var
  p: PExpressionListData;
begin
  new(p);
  _InitExpressionListData(p^);
  p^.DeclType := edtString;
  p^.charPos := charPos;
  p^.Value := v.Text;
  FList.Add(p);
  Result := p;
end;

function TSymbolExpression.AddSpaceName(v: umlString; charPos: Integer): PExpressionListData;
var
  p: PExpressionListData;
begin
  new(p);
  _InitExpressionListData(p^);
  p^.DeclType := edtSpaceName;
  p^.charPos := charPos;
  p^.Value := v.Text;
  FList.Add(p);
  Result := p;
end;

function TSymbolExpression.AddExpressionAsValue(Expression: TSymbolExpression; Symbol: TSymbolOperation; Value: Variant; charPos: Integer): PExpressionListData;
var
  p: PExpressionListData;
begin
  new(p);
  _InitExpressionListData(p^);
  p^.DeclType := edtExpressionAsValue;
  p^.charPos := charPos;
  p^.Symbol := Symbol;
  p^.Value := Value;
  p^.Expression := Expression;
  FList.Add(p);
  Result := p;
end;

function TSymbolExpression.Add(v: TExpressionListData): PExpressionListData;
var
  p: PExpressionListData;
begin
  new(p);
  p^ := v;
  FList.Add(p);
  Result := p;
end;

procedure TSymbolExpression.Delete(idx: Integer);
var
  p: PExpressionListData;
begin
  p := FList[idx];
  Dispose(p);
  FList.Delete(idx);
end;

function TSymbolExpression.Last: PExpressionListData;
begin
  Result := FList.Last;
end;

function TSymbolExpression.First: PExpressionListData;
begin
  Result := FList.First;
end;

function TSymbolExpression.IndexOf(p: PExpressionListData): Integer;
var
  i: Integer;
begin
  for i := FList.Count - 1 downto 0 do
    if FList[i] = p then
        Exit(i);
  Exit(-1);
end;

function TSymbolExpression.GetItems(index: Integer): PExpressionListData;
begin
  Result := FList[index];
end;

end.
