unit zExpression;

{$I zDefine.inc}

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

uses SysUtils, Variants, CoreClasses, TypInfo, OpCode, TextParsing, PascalStrings, DoStatusIO;

type
  // 文本符号操作类型，包括常用的数学逻辑类型缩进类型
  TSymbolOperation = (soAdd, soSub, soMul, soDiv, soMod, soIntDiv, soPow, soOr, soAnd, soXor, // 数学操作
    soEqual, soLessThan, soEqualOrLessThan, soGreaterThan, soEqualOrGreaterThan, soNotEqual,  // 比较符操作
    soShl, soShr,                                                                             // 移位
    soBlockIndentBegin, soBlockIndentEnd,                                                     // 括号缩进符
    soPropParamIndentBegin, soPropParamIndentEnd,                                             // 方框缩紧符
    soDotSymbol, soCommaSymbol,                                                               // 点和逗号
    soEolSymbol,                                                                              // 终止符
    soProc, soParameter,                                                                      // 函数支持
    soUnknow);
  TSymbolOperations = set of TSymbolOperation;

  // 表达式数值类型
  // 解析器会识别写明的自然数，并且给自然数分类
  // 其次是操作符号，字符串，常量，变量，函数，嵌套
  TExpressionDeclType = (
    edtSymbol,                                                                                 // 符号
    edtBool, edtInt, edtInt64, edtUInt64, edtWord, edtByte, edtSmallInt, edtShortInt, edtUInt, // 编译器内置整数变量
    edtSingle, edtDouble, edtCurrency,                                                         // 浮点
    edtString,                                                                                 // 字符串
    edtProcExp,                                                                                // 函数支持
    edtExpressionAsValue,                                                                      // 镶嵌
    edtUnknow);

  TExpressionDeclTypes = set of TExpressionDeclType;

  TSymbolExpression = class;

  TExpressionListData = record
    DeclType: TExpressionDeclType; // 表达式申明类型
    charPos: Integer;              // 表达式字符坐标
    Symbol: TSymbolOperation;      // 符号
    Value: Variant;                // 值
    Expression: TSymbolExpression; // 镶嵌
    ExpressionAutoFree: Boolean;   // 自动释放
  end;

  PExpressionListData = ^TExpressionListData;

  // 已经剥离完成的核心数据结构
  TSymbolExpression = class(TCoreClassObject)
  protected
    FList: TCoreClassList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure PrintDebug(const detail: Boolean; const prefix: string = ''); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Decl: SystemString;

    function GetCount(t: TExpressionDeclTypes): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function GetSymbolCount(Operations: TSymbolOperations): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function AvailValueCount: Integer;
    function Count: Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    { }
    function InsertSymbol(idx: Integer; v: TSymbolOperation; charPos: Integer): PExpressionListData; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Insert(idx: Integer; v: TExpressionListData): PExpressionListData; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure InsertExpression(idx: Integer; e: TSymbolExpression); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure AddExpression(e: TSymbolExpression); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function AddSymbol(v: TSymbolOperation; charPos: Integer): PExpressionListData;
    function AddBool(v: Boolean; charPos: Integer): PExpressionListData; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function AddInt(v: Integer; charPos: Integer): PExpressionListData; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function AddUInt(v: Cardinal; charPos: Integer): PExpressionListData; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function AddInt64(v: int64; charPos: Integer): PExpressionListData; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function AddUInt64(v: UInt64; charPos: Integer): PExpressionListData; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function AddWord(v: Word; charPos: Integer): PExpressionListData; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function AddByte(v: Byte; charPos: Integer): PExpressionListData; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function AddSmallInt(v: SmallInt; charPos: Integer): PExpressionListData; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function AddShortInt(v: ShortInt; charPos: Integer): PExpressionListData; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function AddSingle(v: Single; charPos: Integer): PExpressionListData; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function AddDouble(v: Double; charPos: Integer): PExpressionListData; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function AddCurrency(v: Currency; charPos: Integer): PExpressionListData; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function AddString(v: SystemString; charPos: Integer): PExpressionListData; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function AddFunc(v: SystemString; charPos: Integer): PExpressionListData; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function AddExpressionAsValue(AutoFree: Boolean; Expression: TSymbolExpression; Symbol: TSymbolOperation; Value: Variant; charPos: Integer): PExpressionListData; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Add(v: TExpressionListData): PExpressionListData; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function AddCopy(v: TExpressionListData): PExpressionListData; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    { }
    procedure Delete(idx: Integer); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure DeleteLast; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    { }
    function Last: PExpressionListData; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function First: PExpressionListData; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    { }
    function IndexOf(p: PExpressionListData): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    { }
    function GetItems(index: Integer): PExpressionListData;
    property Items[index: Integer]: PExpressionListData read GetItems; default;
  end;

  // 用于获取代码中常量值的事件
  // 单独使用ParseTextExpressionAsSymbol时 将改事件设置为nil即可
  // 假如有pi,maxInteger这类需求时，根据DeclName判断名字，吻合时返回Value对应的3.141592654即可
  // 浮点类型ValType根据ieee规范，追求性能单浮点精度edtSingle，精确性双浮点edtDouble
  // RefObj和Obj是用于编译器知道当前蚂蚁爬到了依赖于哪个类的方法和变量上，并且返回迭代值，除非做编译器，否则单独使用时可以无视它们
  TExpressionGetValue = procedure(DeclName: SystemString; RefObj: TCoreClassObject;
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
function ParseTextExpressionAsSymbol(TextEngClass: TTextParsingClass; uName, ExpressionText: SystemString;
  const OnGetValue: TExpressionGetValue; RefrenceOpRT: TOpCustomRunTime): TSymbolExpression;

// zExpression 的核心逻辑第二步：对表达式做缩进优先级处理，这一步是为RebuildAllSymbol进行预处理，并不会得到最后符号顺序
// 将TSymbolExpression数据结构拆开，并且对带有符号优先级的符号做缩进预处理，该步骤带侦错功能
function RebuildLogicalPrioritySymbol(Exps: TSymbolExpression): TSymbolExpression;

// zExpression 的核心逻辑第三步，在符号缩进预处理完成以后，重新拆开表达式数据结构，并且重建一个带有缩进的严谨TSymbolExpression的缩进顺序，该步骤带侦错功能
function RebuildAllSymbol(Exps: TSymbolExpression): TSymbolExpression;

// zExpression 的核心逻辑第四步，根据RebuildAllSymbol后的严谨TSymbolExpression符号顺序，创建单步原子化执行
// 单步原子化执行的实现请参考opCode内容
function BuildAsOpCode(DebugMode: Boolean; SymbExps: TSymbolExpression; uName: SystemString; LineNo: Integer): TOpCode; overload;
function BuildAsOpCode(SymbExps: TSymbolExpression): TOpCode; overload;
function BuildAsOpCode(DebugMode: Boolean; SymbExps: TSymbolExpression): TOpCode; overload;

// 预处理一二三步以后输出opCode 并且运行opCode 最后返回一个值
// 该函数会消耗一定的硬件资源，高效运行建议一次性BuildAsOpCode，然后再用SaveToStream将opcode以二进制方式保存，使用时用LoadOpFromStream载入
function EvaluateExpressionValue(TextEngClass: TTextParsingClass; const ExpressionText: SystemString; const OnGetValue: TExpressionGetValue): Variant; overload;
function EvaluateExpressionValue(DebugMode: Boolean; const ExpressionText: SystemString; opRT: TOpCustomRunTime): Variant; overload;
function EvaluateExpressionValue(const ExpressionText: SystemString; opRT: TOpCustomRunTime): Variant; overload;
function EvaluateExpressionValue(DebugMode: Boolean; const ExpressionText: SystemString): Variant; overload;
function EvaluateExpressionValue(const ExpressionText: SystemString): Variant; overload;

function VariantToExpressionDeclType(var v: Variant): TExpressionDeclType; {$IFDEF INLINE_ASM} inline; {$ENDIF}


type
  TNumTextType = (nttBool, nttInt, nttInt64, nttUInt64, nttWord, nttByte, nttSmallInt, nttShortInt, nttUInt,
    nttSingle, nttDouble, nttCurrency, nttUnknow);

function NumTextType(s: TPascalString): TNumTextType; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure InitExp(var v: TExpressionListData); {$IFDEF INLINE_ASM} inline; {$ENDIF}
function dt2op(v: TExpressionDeclType): TOpValueType; {$IFDEF INLINE_ASM} inline; {$ENDIF}


implementation

const
  MethodFlags: TExpressionDeclTypes = ([edtProcExp]);

  AllExpressionValueType: TExpressionDeclTypes = ([
    edtBool, edtInt, edtInt64, edtUInt64, edtWord, edtByte, edtSmallInt, edtShortInt, edtUInt,
    edtSingle, edtDouble, edtCurrency,
    edtString, edtProcExp,
    edtExpressionAsValue]);

  SymbolOperationPriority: array [0 .. 3] of TSymbolOperations = (
    ([soOr, soAnd, soXor]),
    ([soEqual, soLessThan, soEqualOrLessThan, soGreaterThan, soEqualOrGreaterThan, soNotEqual]),
    ([soAdd, soSub]),
    ([soMul, soDiv, soMod, soIntDiv, soShl, soShr, soPow]));

  AllowPrioritySymbol: TSymbolOperations = ([
    soAdd, soSub, soMul, soDiv, soMod, soIntDiv, soPow, soOr, soAnd, soXor,
    soEqual, soLessThan, soEqualOrLessThan, soGreaterThan, soEqualOrGreaterThan, soNotEqual,
    soShl, soShr,
    soDotSymbol, soCommaSymbol]);

  OpLogicalSymbol: TSymbolOperations = ([
    soAdd, soSub, soMul, soDiv, soMod, soIntDiv, soPow, soOr, soAnd, soXor,
    soEqual, soLessThan, soEqualOrLessThan, soGreaterThan, soEqualOrGreaterThan, soNotEqual,
    soShl, soShr]);

  FlagTextDecl: array [TExpressionDeclType] of SystemString = (
    'Symbol',
    'bool', 'int', 'int64', 'UInt64', 'word', 'byte', 'smallInt', 'shortInt', 'uint',
    'float', 'double', 'Currency',
    'text', 'method',
    'Exps',
    'unknow'
    );

type
  TSymbolOperationType = record
    State: TSymbolOperation;
    Decl: SystemString;
  end;

const
  SymbolOperationTextDecl: array [TSymbolOperation] of TSymbolOperationType = (
    (State: soAdd; Decl: '+'),
    (State: soSub; Decl: '-'),
    (State: soMul; Decl: '*'),
    (State: soDiv; Decl: '/'),
    (State: soMod; Decl: ' mod '),
    (State: soIntDiv; Decl: ' div '),
    (State: soPow; Decl: '^'),
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
    (State: soProc; Decl: '|Proc|'),
    (State: soParameter; Decl: ','),
    (State: soUnknow; Decl: '?')
    );

function NumTextType(s: TPascalString): TNumTextType;
type
  TValSym = (vsSymSub, vsSymAdd, vsSymAddSub, vsSymDollar, vsDot, vsDotBeforNum, vsDotAfterNum, vsNum, vsAtoF, vsE, vsUnknow);
var
  cnt: array [TValSym] of Integer;
  v  : TValSym;
  c  : SystemChar;
  i  : Integer;
begin
  if s.Same('true') or s.Same('false') then
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

procedure InitExp(var v: TExpressionListData);
begin
  v.DeclType := edtUnknow;
  v.charPos := -1;
  v.Symbol := soUnknow;
  v.Value := NULL;
  v.Expression := nil;
  v.ExpressionAutoFree := False;
end;

function dt2op(v: TExpressionDeclType): TOpValueType;
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
    edtProcExp: Result := ovtProc;
    else Result := ovtUnknow;
  end;
end;

type
  TExpressionParsingState = set of (esFirst, esWaitOp, esWaitIndentEnd, esWaitPropParamIndentEnd, esWaitValue);
  PExpressionParsingState = ^TExpressionParsingState;

function ParseOperationState(ParsingEng: TTextParsing;
  var cPos, bPos, ePos, BlockIndent, PropIndent: Integer; var pStates: TExpressionParsingState): TSymbolOperation; {$IFDEF INLINE_ASM} inline; {$ENDIF}

var
  c   : SystemChar;
  Decl: TPascalString;
  p   : PExpressionListData;
begin
  Result := soUnknow;
  if not(esWaitOp in pStates) then
      Exit;

  while cPos <= ParsingEng.ParsingData.Text.Len do
    begin
      c := ParsingEng.ParsingData.Text[cPos];
      bPos := cPos;

      if (CharIn(c, ';')) then
        begin
          Inc(cPos);
          Result := soEolSymbol;
          Exit;
        end;

      if (CharIn(c, ',')) then
        begin
          Inc(cPos);
          pStates := pStates - [esWaitOp] + [esWaitValue];
          Result := soCommaSymbol;
          Exit;
        end;

      if CharIn(c, ')') then
        begin
          Inc(cPos);
          if (esWaitIndentEnd in pStates) then
            begin
              Dec(BlockIndent);
              if BlockIndent < 0 then
                begin
                  pStates := pStates - [esWaitOp, esWaitIndentEnd];
                  Result := soBlockIndentEnd;
                  Exit;
                end
              else if BlockIndent = 0 then
                  pStates := pStates - [esWaitIndentEnd];

              pStates := pStates + [esWaitOp];
              Result := soBlockIndentEnd;
              Exit;
            end
          else
            begin
              pStates := pStates - [esWaitOp, esWaitIndentEnd];
              Result := soBlockIndentEnd;
              Exit;
            end;
        end;

      if CharIn(c, ']') then
        begin
          Inc(cPos);
          if (esWaitPropParamIndentEnd in pStates) then
            begin
              Dec(PropIndent);
              if PropIndent < 0 then
                begin
                  pStates := pStates - [esWaitOp, esWaitPropParamIndentEnd];
                  Result := soPropParamIndentEnd;
                  Exit;
                end
              else if PropIndent = 0 then
                  pStates := pStates - [esWaitPropParamIndentEnd];

              pStates := pStates + [esWaitOp];
              Result := soPropParamIndentEnd;
              Exit;
            end
          else
            begin
              pStates := pStates - [esWaitOp, esWaitPropParamIndentEnd];
              Result := soPropParamIndentEnd;
              Exit;
            end;
        end;

      if CharIn(c, '(') then
        begin
          Inc(cPos);
          Inc(BlockIndent);
          pStates := pStates - [esWaitOp];
          pStates := pStates + [esWaitValue, esWaitIndentEnd];

          Result := soBlockIndentBegin;
          Exit;
        end;

      if CharIn(c, '[') then
        begin
          Inc(cPos);
          Inc(PropIndent);
          Result := soPropParamIndentBegin;
          pStates := pStates - [esWaitOp];
          pStates := pStates + [esWaitValue, esWaitPropParamIndentEnd];
          Exit;
        end;

      if (ParsingEng.ComparePosStr(cPos, '>=')) or (ParsingEng.ComparePosStr(cPos, '=>')) then
        begin
          Inc(cPos, 2);
          Result := soEqualOrGreaterThan;
          pStates := pStates - [esWaitOp];
          pStates := pStates + [esWaitValue];
          Exit;
        end;
      if (ParsingEng.ComparePosStr(cPos, '<=')) or (ParsingEng.ComparePosStr(cPos, '=<')) then
        begin
          Inc(cPos, 2);
          Result := soEqualOrLessThan;
          pStates := pStates - [esWaitOp];
          pStates := pStates + [esWaitValue];
          Exit;
        end;
      if (ParsingEng.ComparePosStr(cPos, '<>')) or (ParsingEng.ComparePosStr(cPos, '><')) or (ParsingEng.ComparePosStr(cPos, '!=')) then
        begin
          Inc(cPos, 2);
          Result := soNotEqual;
          pStates := pStates - [esWaitOp];
          pStates := pStates + [esWaitValue];
          Exit;
        end;
      if (ParsingEng.ComparePosStr(cPos, '==')) then
        begin
          Inc(cPos, 2);
          Result := soEqual;
          pStates := pStates - [esWaitOp];
          pStates := pStates + [esWaitValue];
          Exit;
        end;
      if (ParsingEng.ComparePosStr(cPos, '&&')) then
        begin
          Inc(cPos, 2);
          Result := soAnd;
          pStates := pStates - [esWaitOp];
          pStates := pStates + [esWaitValue];
          Exit;
        end;
      if (ParsingEng.ComparePosStr(cPos, '||')) then
        begin
          Inc(cPos, 2);
          Result := soOr;
          pStates := pStates - [esWaitOp];
          pStates := pStates + [esWaitValue];
          Exit;
        end;
      if (ParsingEng.ComparePosStr(cPos, '<<')) then
        begin
          Inc(cPos, 2);
          Result := soShl;
          pStates := pStates - [esWaitOp];
          pStates := pStates + [esWaitValue];
          Exit;
        end;
      if (ParsingEng.ComparePosStr(cPos, '>>')) then
        begin
          Inc(cPos, 2);
          Result := soShr;
          pStates := pStates - [esWaitOp];
          pStates := pStates + [esWaitValue];
          Exit;
        end;

      if CharIn(c, '+-*/^=><.,&|%') then
        begin
          if c = '+' then
              Result := soAdd
          else if c = '-' then
              Result := soSub
          else if c = '*' then
              Result := soMul
          else if c = '/' then
              Result := soDiv
          else if c = '^' then
              Result := soPow
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
              Result := soOr
          else if c = '%' then
              Result := soMod;
          Inc(cPos);
          pStates := pStates - [esWaitOp];
          pStates := pStates + [esWaitValue];
          Exit;
        end;

      if (ParsingEng.IsAscii(cPos)) then
        begin
          bPos := cPos;
          ePos := ParsingEng.GetAsciiEndPos(cPos);
          Decl := ParsingEng.GetStr(bPos, ePos);

          if Decl.Same('or') then
              Result := soOr
          else if Decl.Same('and') then
              Result := soAnd
          else if Decl.Same('xor') then
              Result := soXor
          else if Decl.Same('div') then
              Result := soIntDiv
          else if Decl.Same('idiv') then
              Result := soIntDiv
          else if Decl.Same('intdiv') then
              Result := soIntDiv
          else if Decl.Same('fdiv') then
              Result := soDiv
          else if Decl.Same('floatdiv') then
              Result := soDiv
          else if Decl.Same('mod') then
              Result := soMod
          else if Decl.Same('shl') then
              Result := soShl
          else if Decl.Same('shr') then
              Result := soShr
          else
            begin
              Result := soUnknow;
              Exit;
            end;

          cPos := ePos;
          pStates := pStates - [esWaitOp];
          pStates := pStates + [esWaitValue];
          Exit;
        end;

      if ParsingEng.IsNumber(cPos) then
        begin
          Result := soUnknow;
          Exit;
        end;

      Inc(cPos);
    end;
  pStates := [];
  Result := soEolSymbol;
end;

function ParseSymbol(ParsingEng: TTextParsing; WorkSym: TSymbolExpression;
  var cPos, bPos, ePos, BlockIndent, PropIndent: Integer; pStates: PExpressionParsingState): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
var
  bak_cPos: Integer;
  Decl    : SystemString;
  OpState : TSymbolOperation;
  rv      : Variant;
  robj    : TCoreClassObject;
  p       : PExpressionListData;
begin
  while cPos <= ParsingEng.Len do
    begin
      pStates^ := pStates^ - [esWaitValue, esFirst];
      pStates^ := pStates^ + [esWaitOp];

      bak_cPos := cPos;
      OpState := ParseOperationState(ParsingEng, cPos, bPos, ePos, BlockIndent, PropIndent, pStates^);

      case OpState of
        soUnknow, soEolSymbol:
          begin
            Result := False;
            Exit;
          end;
        soDotSymbol:
          begin
            Result := False;
            Exit;
          end;
        soCommaSymbol:
          begin
            WorkSym.AddSymbol(OpState, bak_cPos);
            Result := True;
            Exit;
          end;
        soPropParamIndentBegin:
          begin
            WorkSym.AddSymbol(OpState, bak_cPos);
            Result := True;
            Exit;
          end;
        soPropParamIndentEnd:
          begin
            WorkSym.AddSymbol(OpState, bak_cPos);
            Result := True;
            Exit;
          end;
        soBlockIndentEnd:
          begin
            WorkSym.AddSymbol(OpState, bak_cPos);
            Result := True;
            Exit;
          end;
        soBlockIndentBegin:
          begin
            WorkSym.AddSymbol(OpState, bak_cPos);
            Result := True;
            Exit;
          end;
        else
          begin
            WorkSym.AddSymbol(OpState, bak_cPos);
            Result := True;
            Exit;
          end;
      end;
    end;
  Result := False;
end;

function ParseTextExpressionAsSymbol(TextEngClass: TTextParsingClass; uName, ExpressionText: SystemString;
  const OnGetValue: TExpressionGetValue; RefrenceOpRT: TOpCustomRunTime): TSymbolExpression;

  procedure PrintError(const s: SystemString);
  begin
    if s = '' then
        DoStatus('declaration error', [])
    else
        DoStatus('declaration error %s', [s]);
    DoStatus('');
  end;

  function GetDeclValue(DeclName: SystemString; var v: Variant; var Obj: TCoreClassObject): TExpressionDeclType;
  begin
    if Assigned(OnGetValue) then
      begin
        OnGetValue(DeclName, nil, Result, v, Obj);
      end
    else
      begin
        v := DeclName;
        Result := edtProcExp;
      end;
  end;

  function FillProc(var ExpIndex: Integer; const Exps, procExp: TSymbolExpression): TSymbolExpression;
  var
    WasProc         : Boolean;
    LocalExp, ResExp: TSymbolExpression;
    p1, p2, p       : PExpressionListData;
  begin
    if ExpIndex >= Exps.Count then
      begin
        Result := nil;
        Exit;
      end;

    WasProc := procExp <> nil;

    if WasProc then
        LocalExp := procExp.AddExpressionAsValue(True, TSymbolExpression.Create, soParameter, 'param_1', Exps[ExpIndex]^.charPos)^.Expression
    else
        LocalExp := TSymbolExpression.Create;
    Result := LocalExp;

    while ExpIndex < Exps.Count do
      begin
        p1 := Exps[ExpIndex];

        if ExpIndex + 1 < Exps.Count then
            p2 := Exps[ExpIndex + 1]
        else
            p2 := nil;

        if (p1^.DeclType = edtProcExp) then
          begin
            if p2 <> nil then
              begin
                if (p2^.DeclType = edtSymbol) and (p2^.Symbol in [soBlockIndentBegin, soPropParamIndentBegin]) then
                  begin
                    p := Result.AddFunc(p1^.Value, p1^.charPos);
                    Inc(ExpIndex, 2);
                    ResExp := FillProc(ExpIndex, Exps, p^.Expression);
                    continue;
                  end;
              end
            else
              begin
                Result.AddFunc(p1^.Value, p1^.charPos);
                Inc(ExpIndex);
                continue;
              end;
          end;

        if (p1^.DeclType = edtSymbol) then
          begin
            if p1^.Symbol in [soBlockIndentBegin, soPropParamIndentBegin] then
              begin
                Inc(ExpIndex);
                ResExp := FillProc(ExpIndex, Exps, nil);
                if ResExp <> nil then
                    LocalExp.AddExpressionAsValue(True, ResExp, soBlockIndentBegin, p1^.Symbol, p1^.charPos);
                continue;
              end;
            if p1^.Symbol in [soBlockIndentEnd, soPropParamIndentEnd] then
              begin
                Inc(ExpIndex);
                Exit;
              end;
            if (p1^.Symbol in [soCommaSymbol]) then
              begin
                if not WasProc then
                  begin
                    PrintError('fillProc comma Illegal');
                    Exit;
                  end;
                LocalExp := procExp.AddExpressionAsValue(True, TSymbolExpression.Create, soParameter, 'param_' + IntToStr(procExp.Count + 1),
                  Exps[ExpIndex]^.charPos)^.Expression;
                Inc(ExpIndex);
                continue;
              end;
          end;

        LocalExp.AddCopy(p1^);
        Inc(ExpIndex);
      end;
  end;

var
  ParsingEng                             : TTextParsing;
  cPos, bPos, ePos, i                    : Integer;
  State                                  : TExpressionParsingState;
  BlockIndent, PropIndent                : Integer;
  Container                              : TSymbolExpression;
  te                                     : TTextParsing;
  c                                      : SystemChar;
  Decl                                   : SystemString;
  OpState                                : TSymbolOperation;
  IsNumber, IsAscii, IsTextDecl, IsSymbol: Boolean;
  rv                                     : Variant;
  robj                                   : TCoreClassObject;
  p                                      : PExpressionListData;
begin
  te := TextEngClass.Create(ExpressionText, tsPascal);
  ParsingEng := TextEngClass.Create(te.GetDeletedCommentText, tsPascal);
  DisposeObject(te);

  Result := nil;

  if ParsingEng.ParsingData.Len < 1 then
      Exit;

  cPos := 1;
  BlockIndent := 0;
  PropIndent := 0;
  State := [esFirst];
  Container := TSymbolExpression.Create;

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
              break;
            end;

          bPos := cPos;
          if IsNumber then
              ePos := ParsingEng.GetNumberEndPos(cPos)
          else if IsTextDecl then
              ePos := ParsingEng.GetTextDeclEndPos(cPos)
          else
              ePos := ParsingEng.GetAsciiEndPos(cPos);
          cPos := ePos;

          Decl := ParsingEng.GetStr(bPos, ePos);
          if IsNumber then
            begin
              case NumTextType(Decl) of
                nttBool: Container.AddBool(StrToBool(Decl), bPos);
                nttInt: Container.AddInt(StrToInt(Decl), bPos);
                nttInt64: Container.AddInt64(StrToInt64(Decl), bPos);
                {$IFDEF FPC}
                nttUInt64: Container.AddUInt64(StrToQWord(Decl), bPos);
                {$ELSE}
                nttUInt64: Container.AddUInt64(StrToUInt64(Decl), bPos);
                {$ENDIF}
                nttWord: Container.AddWord(StrToInt(Decl), bPos);
                nttByte: Container.AddByte(StrToInt(Decl), bPos);
                nttSmallInt: Container.AddSmallInt(StrToInt(Decl), bPos);
                nttShortInt: Container.AddShortInt(StrToInt(Decl), bPos);
                nttUInt: Container.AddUInt(StrToInt(Decl), bPos);
                nttSingle: Container.AddSingle(StrToFloat(Decl), bPos);
                nttDouble: Container.AddDouble(StrToFloat(Decl), bPos);
                nttCurrency: Container.AddCurrency(StrToFloat(Decl), bPos);
                else
                  begin
                    PrintError(Format('number expression "%s" Illegal', [Decl]));
                    break;
                  end;
              end;
            end
          else if IsTextDecl then
            begin
              Container.AddString(ParsingEng.GetTextBody(Decl), bPos);
            end
          else
            case NumTextType(Decl) of
              nttBool: Container.AddBool(StrToBool(Decl), bPos);
              nttInt: Container.AddInt(StrToInt(Decl), bPos);
              nttInt64: Container.AddInt64(StrToInt64(Decl), bPos);
              {$IFDEF FPC}
              nttUInt64: Container.AddUInt64(StrToQWord(Decl), bPos);
              {$ELSE}
              nttUInt64: Container.AddUInt64(StrToUInt64(Decl), bPos);
              {$ENDIF}
              nttWord: Container.AddWord(StrToInt(Decl), bPos);
              nttByte: Container.AddByte(StrToInt(Decl), bPos);
              nttSmallInt: Container.AddSmallInt(StrToInt(Decl), bPos);
              nttShortInt: Container.AddShortInt(StrToInt(Decl), bPos);
              nttUInt: Container.AddUInt(StrToInt(Decl), bPos);
              nttSingle: Container.AddSingle(StrToFloat(Decl), bPos);
              nttDouble: Container.AddDouble(StrToFloat(Decl), bPos);
              nttCurrency: Container.AddCurrency(StrToFloat(Decl), bPos);
              else
                begin
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
                    edtString: Container.AddString(rv, bPos);
                    edtProcExp:
                      begin
                        if (RefrenceOpRT <> nil) and (not RefrenceOpRT.ProcList.Exists(rv)) then
                          begin
                            PrintError(Format('function "%s" Illegal', [rv]));
                            break;
                          end;
                        Container.AddFunc(rv, bPos);
                      end;
                    else
                      begin
                        PrintError(Format('define "%s" Illegal', [Decl]));
                        break;
                      end;
                  end;
                end;
            end;
          if not ParseSymbol(ParsingEng, Container, cPos, bPos, ePos, BlockIndent, PropIndent, @State) then
              break
          else
              continue;
        end;

      if (IsSymbol) then
        begin
          if not ParseSymbol(ParsingEng, Container, cPos, bPos, ePos, BlockIndent, PropIndent, @State) then
              break
          else
              continue;
        end;

      Inc(cPos);
    end;

  if (BlockIndent + PropIndent = 0) then
    begin
      i := 0;
      Result := FillProc(i, Container, nil);
    end
  else
      DoStatus('indent error:%d,%d', [BlockIndent, PropIndent]);

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

  procedure PostError(const s: SystemString);
  begin
    ParseAborted := True;
    if s <> '' then
        DoStatus(Format('Priority symbol failed : %s', [s]))
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
            break;

        if SymbolIndex >= Exps.Count then
            break;

        p1 := Exps[SymbolIndex];

        if (p1^.DeclType in AllExpressionValueType) then
          begin
            Inc(SymbolIndex);
            if SymbolIndex >= Exps.Count then
              begin
                newExpression.Add(p1^);
                break;
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
                        break;
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

                        continue;
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
                break;
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
                        continue;
                      end
                    else
                      begin
                        newExpression.Add(p1^);
                        continue;
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
  SymbolIndex : Integer;
  ParseAborted: Boolean;

  procedure PostError(const s: SystemString);
  begin
    ParseAborted := True;
    if s <> '' then
        DoStatus(Format('indent symbol failed : %s', [s]))
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
    while True do
      begin
        if SymbolIndex >= Exps.Count then
            break;

        p1 := Exps[SymbolIndex];

        if (p1^.DeclType in [edtSymbol]) then
          begin
            if p1^.Symbol in [soBlockIndentBegin, soPropParamIndentBegin] then
              begin
                Inc(SymbolIndex);

                ResExp := ProcessIndent(p1^.Symbol);
                LocalExp.AddExpressionAsValue(True, ResExp, p1^.Symbol, SymbolOperationTextDecl[p1^.Symbol].Decl, p1^.charPos);

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
            if p1^.DeclType = edtProcExp then
              begin
                LocalExp.Add(p1^);
                Inc(SymbolIndex);
                continue;
              end;

            Inc(SymbolIndex);
            if SymbolIndex >= Exps.Count then
              begin
                LocalExp.Add(p1^);
                break;
              end;

            p2 := Exps[SymbolIndex];

            if p2^.DeclType = edtSymbol then
              begin
                if (p2^.Symbol in [soBlockIndentBegin, soPropParamIndentBegin]) then
                  begin
                    if (p1^.DeclType in MethodFlags) then
                      begin
                        PostError('method Illegal');
                        Exit;
                      end;

                    LocalExp.Add(p1^);
                    Inc(SymbolIndex);

                    ResExp := ProcessIndent(p2^.Symbol);
                    LocalExp.AddExpressionAsValue(True, ResExp, p2^.Symbol, SymbolOperationTextDecl[p2^.Symbol].Decl, p2^.charPos);

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
                else if p2^.Symbol = soCommaSymbol then
                  begin
                    PostError('Comma Illegal');
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
    i, j     : Integer;
    e, ResExp: TSymbolExpression;
    p, funcP : PExpressionListData;
  begin
    e := RebuildLogicalPrioritySymbol(_e);
    if e = nil then
      begin
        PostError('parse priority failed');
        Exit;
      end;

    Result := TSymbolExpression.Create;

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
                    begin
                      Result.AddExpression(ResExp);
                      DisposeObject(ResExp);
                    end;
                  Result.AddSymbol(soBlockIndentEnd, p^.charPos);
                end;
              soPropParamIndentBegin:
                begin
                  Result.AddSymbol(soPropParamIndentBegin, p^.charPos);
                  ResExp := ProcessPriority(p^.Expression);
                  if ResExp <> nil then
                    begin
                      Result.AddExpression(ResExp);
                      DisposeObject(ResExp);
                    end;
                  Result.AddSymbol(soPropParamIndentEnd, p^.charPos);
                end;
              else
                begin
                  break;
                end;
            end;
          end
        else if p^.DeclType = edtProcExp then
          begin
            funcP := Result.AddFunc(VarToStr(p^.Value), p^.charPos);
            if (p^.Expression.Count > 0) and (p^.Expression.First^.Expression.Count > 0) then
              for j := 0 to p^.Expression.Count - 1 do
                begin
                  ResExp := RebuildAllSymbol(p^.Expression[j]^.Expression);
                  if ResExp <> nil then
                      funcP^.Expression.AddExpressionAsValue(True, ResExp, soParameter, VarToStr(p^.Expression[j]^.Value), p^.Expression[j]^.charPos);
                end;
          end
        else
          begin
            Result.Add(p^);
          end;
      end;
    DisposeObject([e]);
  end;

var
  rse: TSymbolExpression;
begin
  Result := nil;
  SymbolIndex := 0;
  ParseAborted := False;

  rse := ProcessIndent(soUnknow);
  Result := ProcessPriority(rse);
  DisposeObject(rse);
end;

function BuildAsOpCode(DebugMode: Boolean; SymbExps: TSymbolExpression; uName: SystemString; LineNo: Integer): TOpCode;
var
  NewSymbExps : TSymbolExpression;
  SymbolIndex : Integer;
  BuildAborted: Boolean;
  OpContainer : TCoreClassListForObj;

  procedure PostError(const s: SystemString);
  begin
    BuildAborted := True;
    if s <> '' then
        DoStatus(Format('build op failed : %s', [s]))
    else
        DoStatus('build op failed');
  end;

  function NewOpValue(uName: SystemString): TOpCode;
  begin
    Result := op_Value.Create(False);
    Result.ParsedInfo := uName;
    Result.ParsedLineNo := LineNo;
    OpContainer.Add(Result);
  end;

  function NewOpProc(uName: SystemString): TOpCode;
  begin
    Result := op_Proc.Create(False);
    Result.ParsedInfo := uName;
    Result.ParsedLineNo := LineNo;
    OpContainer.Add(Result);
  end;

  function NewOpFromSym(sym: TSymbolOperation; uName: SystemString): TOpCode;
  begin
    case sym of
      soAdd: Result := op_Add.Create(False);
      soSub: Result := op_Sub.Create(False);
      soMul: Result := op_Mul.Create(False);
      soDiv: Result := op_Div.Create(False);
      soMod: Result := op_Mod.Create(False);
      soIntDiv: Result := op_IntDiv.Create(False);
      soPow: Result := op_Pow.Create(False);
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
    i                            : Integer;
    p1, p2                       : PExpressionListData;
    LocalOp, OldOp, ResOp, ProcOp: TOpCode;
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
            break;
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
                    break;
                  end;
              end
            else if ((OwnerIndentSym = soBlockIndentBegin) and (p1^.Symbol = soBlockIndentEnd)) or
              ((OwnerIndentSym = soPropParamIndentBegin) and (p1^.Symbol = soPropParamIndentEnd)) then
              begin
                Result := LocalOp;
                break;
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
                    break;
                  end;
              end
            else
              begin
                PostError('logical cperotion Illegal');
                break;
              end;
          end
        else if (p1^.DeclType in AllExpressionValueType) then
          begin
            if p1^.DeclType = edtProcExp then
              begin
                ProcOp := NewOpProc(uName);
                ProcOp.AddValue(p1^.Value);
                for i := 0 to p1^.Expression.Count - 1 do
                  begin
                    ResOp := BuildAsOpCode(DebugMode, p1^.Expression[i]^.Expression, uName, LineNo);
                    if ResOp <> nil then
                        ProcOp.AddLink(ResOp)
                    else
                      begin
                        PostError('method Illegal');
                        break;
                      end;
                  end;

                if LocalOp <> nil then
                  begin
                    LocalOp.AddLink(ProcOp);
                  end
                else
                  begin
                    LocalOp := NewOpValue(uName);
                    LocalOp.AddLink(ProcOp);
                  end;
                Inc(SymbolIndex);
                continue;
              end;

            Inc(SymbolIndex);
            if SymbolIndex >= NewSymbExps.Count then
              begin
                if LocalOp <> nil then
                  begin
                    LocalOp.AddValue(p1^.Value, dt2op(p1^.DeclType));
                  end
                else
                  begin
                    LocalOp := NewOpValue(uName);
                    LocalOp.AddValue(p1^.Value, dt2op(p1^.DeclType));
                  end;
                Result := LocalOp;
                break;
              end;

            p2 := NewSymbExps[SymbolIndex];

            if p2^.DeclType = edtSymbol then
              begin
                if (p2^.Symbol in [soBlockIndentBegin, soPropParamIndentBegin]) then
                  begin
                    // function call
                    if not(p1^.DeclType in MethodFlags) then
                      begin
                        PostError('method Illegal');
                        break;
                      end
                    else
                      begin
                      end;

                    Inc(SymbolIndex);
                    ResOp := ProcessIndent(p2^.Symbol);

                  end
                else if ((OwnerIndentSym = soBlockIndentBegin) and (p2^.Symbol = soBlockIndentEnd)) or
                  ((OwnerIndentSym = soPropParamIndentBegin) and (p2^.Symbol = soPropParamIndentEnd)) then
                  begin
                    if LocalOp <> nil then
                      begin
                        LocalOp.AddValue(p1^.Value, dt2op(p1^.DeclType));
                      end
                    else
                      begin
                        LocalOp := NewOpValue(uName);
                        LocalOp.AddValue(p1^.Value, dt2op(p1^.DeclType));
                      end;
                    Result := LocalOp;
                    break;
                  end
                else if p2^.Symbol in OpLogicalSymbol then
                  begin
                    if LocalOp <> nil then
                      begin
                        OldOp := LocalOp;
                        OldOp.AddValue(p1^.Value, dt2op(p1^.DeclType));
                        LocalOp := NewOpFromSym(p2^.Symbol, uName);
                        LocalOp.AddLink(OldOp);
                      end
                    else
                      begin
                        LocalOp := NewOpFromSym(p2^.Symbol, uName);
                        LocalOp.AddValue(p1^.Value, dt2op(p1^.DeclType));
                      end;
                  end
                else
                  begin
                    PostError('Illegal');
                    break;
                  end;
              end
            else
              begin
                PostError('Illegal');
                break;
              end;
          end;

        Inc(SymbolIndex);
      end;
  end;

  procedure ProcessOpContainer(Successed: Boolean);
  var
    i: Integer;
  begin
    for i := 0 to OpContainer.Count - 1 do
      if Successed then
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
      if DebugMode then
          NewSymbExps.PrintDebug(True);

      if NewSymbExps.GetSymbolCount([soBlockIndentBegin, soPropParamIndentBegin]) =
        NewSymbExps.GetSymbolCount([soBlockIndentEnd, soPropParamIndentEnd]) then
        begin
          OpContainer := TCoreClassListForObj.Create;

          SymbolIndex := 0;
          BuildAborted := False;
          Result := ProcessIndent(soUnknow);
          ProcessOpContainer(Result <> nil);
          DisposeObject(OpContainer);
        end;
      DisposeObject(NewSymbExps);
    end;
end;

function BuildAsOpCode(SymbExps: TSymbolExpression): TOpCode;
begin
  Result := BuildAsOpCode(False, SymbExps, '', 0);
end;

function BuildAsOpCode(DebugMode: Boolean; SymbExps: TSymbolExpression): TOpCode;
begin
  Result := BuildAsOpCode(DebugMode, SymbExps, '', 0);
end;

function EvaluateExpressionValue(TextEngClass: TTextParsingClass; const ExpressionText: SystemString; const OnGetValue: TExpressionGetValue): Variant;
var
  sym: TSymbolExpression;
  op : TOpCode;
  i  : Integer;
begin
  Result := NULL;
  sym := ParseTextExpressionAsSymbol(TextEngClass, 'Main', ExpressionText, OnGetValue, nil);

  if sym <> nil then
    begin
      op := BuildAsOpCode(False, sym, 'Main', -1);
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

function EvaluateExpressionValue(DebugMode: Boolean; const ExpressionText: SystemString; opRT: TOpCustomRunTime): Variant;
var
  sym: TSymbolExpression;
  op : TOpCode;
  i  : Integer;
begin
  Result := NULL;
  sym := ParseTextExpressionAsSymbol(TTextParsing, 'Main', ExpressionText, nil, opRT);

  if sym <> nil then
    begin
      op := BuildAsOpCode(DebugMode, sym, 'Main', -1);
      if op <> nil then
        begin
          try
              Result := op.Execute(opRT);
          except
              Result := NULL;
          end;
          DisposeObject(op);
        end;
      DisposeObject(sym);
    end;
end;

function EvaluateExpressionValue(const ExpressionText: SystemString; opRT: TOpCustomRunTime): Variant;
var
  sym: TSymbolExpression;
  op : TOpCode;
  i  : Integer;
begin
  Result := NULL;
  sym := ParseTextExpressionAsSymbol(TTextParsing, 'Main', ExpressionText, nil, opRT);

  if sym <> nil then
    begin
      op := BuildAsOpCode(False, sym, 'Main', -1);
      if op <> nil then
        begin
          try
              Result := op.Execute(opRT);
          except
              Result := NULL;
          end;
          DisposeObject(op);
        end;
      DisposeObject(sym);
    end;
end;

function EvaluateExpressionValue(DebugMode: Boolean; const ExpressionText: SystemString): Variant;
var
  sym: TSymbolExpression;
  op : TOpCode;
  i  : Integer;
begin
  Result := NULL;
  sym := ParseTextExpressionAsSymbol(TTextParsing, 'Main', ExpressionText, nil, nil);

  if sym <> nil then
    begin
      op := BuildAsOpCode(DebugMode, sym, 'Main', -1);
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

function EvaluateExpressionValue(const ExpressionText: SystemString): Variant;
var
  sym: TSymbolExpression;
  op : TOpCode;
  i  : Integer;
begin
  Result := NULL;
  sym := ParseTextExpressionAsSymbol(TTextParsing, 'Main', ExpressionText, nil, nil);

  if sym <> nil then
    begin
      op := BuildAsOpCode(False, sym, 'Main', -1);
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
    begin
      if (PExpressionListData(FList[i])^.ExpressionAutoFree) and (PExpressionListData(FList[i])^.Expression <> nil) then
          DisposeObject(PExpressionListData(FList[i])^.Expression);

      Dispose(PExpressionListData(FList[i]));
    end;

  FList.Clear;
end;

procedure TSymbolExpression.PrintDebug(const detail: Boolean; const prefix: string = '');
var
  i: Integer;
  p: PExpressionListData;
begin
  DoStatus(prefix + ' decl: ' + Decl);

  if detail then
    begin
      for i := 0 to Count - 1 do
        begin
          p := GetItems(i);

          DoStatus(prefix + ' id:%d exp:%s symbol:%s val:%s', [i,
            GetEnumName(TypeInfo(TExpressionDeclType), Ord(p^.DeclType)),
            GetEnumName(TypeInfo(TSymbolOperation), Ord(p^.Symbol)),
            VarToStr(p^.Value)]);

        end;

      DoStatus('');

      for i := 0 to Count - 1 do
        begin
          p := GetItems(i);
          if p^.Expression <> nil then
            if p^.Expression.Count > 0 then
                p^.Expression.PrintDebug(detail, prefix + ' -> ' + VarToStr(p^.Value));
        end;
    end;
end;

function TSymbolExpression.Decl: SystemString;
var
  i, j: Integer;
  p   : PExpressionListData;
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
        edtProcExp:
          begin
            Result := Result + VarToStr(p^.Value) + '(';
            for j := 0 to p^.Expression.Count - 1 do
              begin
                if j = 0 then
                    Result := Result + p^.Expression[j]^.Expression.Decl
                else
                    Result := Result + ',' + p^.Expression[j]^.Expression.Decl;
              end;
            Result := Result + ')';
          end;
        edtExpressionAsValue:
          begin
            case p^.Symbol of
              soBlockIndentBegin:
                Result := Format('%s%s%s%s',
                  [Result,
                  SymbolOperationTextDecl[soBlockIndentBegin].Decl,
                  p^.Expression.Decl,
                  SymbolOperationTextDecl[soBlockIndentEnd].Decl
                  ]);
              soPropParamIndentBegin:
                Result := Format('%s%s%s%s',
                  [Result,
                  SymbolOperationTextDecl[soPropParamIndentBegin].Decl,
                  p^.Expression.Decl,
                  SymbolOperationTextDecl[soPropParamIndentEnd].Decl
                  ]);
              soParameter:
                begin
                  Result := Format('%s%s%s%s',
                    [Result,
                    SymbolOperationTextDecl[soBlockIndentBegin].Decl,
                    p^.Expression.Decl,
                    SymbolOperationTextDecl[soBlockIndentEnd].Decl
                    ]);
                end;
              else
                Result := Result + ' !error! ';
            end;
          end;
        edtUnknow: Result := Result + ' !error! ';
        else
          Result := Result + VarToStr(p^.Value);
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
  InitExp(p^);
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
begin
  for i := 0 to e.Count - 1 do
      AddCopy(e[i]^);
end;

function TSymbolExpression.AddSymbol(v: TSymbolOperation; charPos: Integer): PExpressionListData;
var
  p: PExpressionListData;
begin
  new(p);
  InitExp(p^);
  p^.DeclType := edtSymbol;
  p^.charPos := charPos;
  p^.Symbol := v;
  p^.Value := SymbolOperationTextDecl[v].Decl;
  FList.Add(p);
  Result := p;
end;

function TSymbolExpression.AddBool(v: Boolean; charPos: Integer): PExpressionListData;
var
  p: PExpressionListData;
begin
  new(p);
  InitExp(p^);
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
  InitExp(p^);
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
  InitExp(p^);
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
  InitExp(p^);
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
  InitExp(p^);
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
  InitExp(p^);
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
  InitExp(p^);
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
  InitExp(p^);
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
  InitExp(p^);
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
  InitExp(p^);
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
  InitExp(p^);
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
  InitExp(p^);
  p^.DeclType := edtCurrency;
  p^.charPos := charPos;
  p^.Value := v;
  FList.Add(p);
  Result := p;
end;

function TSymbolExpression.AddString(v: SystemString; charPos: Integer): PExpressionListData;
var
  p: PExpressionListData;
begin
  new(p);
  InitExp(p^);
  p^.DeclType := edtString;
  p^.charPos := charPos;
  p^.Value := v;
  FList.Add(p);
  Result := p;
end;

function TSymbolExpression.AddFunc(v: SystemString; charPos: Integer): PExpressionListData;
var
  p: PExpressionListData;
begin
  new(p);
  InitExp(p^);
  p^.DeclType := edtProcExp;
  p^.charPos := charPos;
  p^.Symbol := soProc;
  p^.Value := v;
  p^.Expression := TSymbolExpression.Create;
  p^.ExpressionAutoFree := True;
  FList.Add(p);
  Result := p;
end;

function TSymbolExpression.AddExpressionAsValue(AutoFree: Boolean; Expression: TSymbolExpression; Symbol: TSymbolOperation; Value: Variant; charPos: Integer): PExpressionListData;
var
  p: PExpressionListData;
begin
  new(p);
  InitExp(p^);
  p^.DeclType := edtExpressionAsValue;
  p^.charPos := charPos;
  p^.Symbol := Symbol;
  p^.Value := Value;
  p^.Expression := Expression;
  p^.ExpressionAutoFree := AutoFree;
  FList.Add(p);
  Result := p;
end;

function TSymbolExpression.Add(v: TExpressionListData): PExpressionListData;
var
  p: PExpressionListData;
begin
  new(p);
  p^ := v;
  p^.ExpressionAutoFree := False;
  FList.Add(p);
  Result := p;
end;

function TSymbolExpression.AddCopy(v: TExpressionListData): PExpressionListData;
var
  p: PExpressionListData;
  i: Integer;
begin
  new(p);
  p^ := v;
  p^.ExpressionAutoFree := False;
  if v.Expression <> nil then
    begin
      p^.Expression := TSymbolExpression.Create;
      p^.ExpressionAutoFree := True;
      for i := 0 to v.Expression.Count - 1 do
          p^.Expression.AddCopy(v.Expression[i]^)
    end;
  FList.Add(p);
  Result := p;
end;

procedure TSymbolExpression.Delete(idx: Integer);
var
  p: PExpressionListData;
begin
  p := FList[idx];
  if (p^.ExpressionAutoFree) and (p^.Expression <> nil) then
      DisposeObject(p^.Expression);
  Dispose(p);
  FList.Delete(idx);
end;

procedure TSymbolExpression.DeleteLast;
begin
  Delete(Count - 1);
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
