unit opCode;

{$I zDefine.inc}

interface

uses SysUtils, Variants, Math, CoreClasses, PascalStrings, DoStatusIO,
  ListEngine, UnicodeMixedLib;

type
  TOpValueType = (
    ovtBool, ovtInt, ovtInt64, ovtUInt64, ovtWord, ovtByte, ovtSmallInt, ovtShortInt, ovtUInt,
    ovtSingle, ovtDouble, ovtCurrency,
    ovtString, ovtProc,
    ovtUnknow);

  TOpCode = class;

  TOpParam = array of Variant;

  POpData = ^opData;

  opData = record
    OnGet: TOpCode;
    Value: Variant;
    ValueType: TOpValueType;
  end;

  TOnOpCall               = function(Param: TOpParam)                           : Variant;
  TOnOpMethod             = function(Param: TOpParam)                         : Variant of object;
  {$IFNDEF FPC} TOnOpProc = reference to function(Param: TOpParam): Variant; {$ENDIF FPC}
  { }
  PopRTproc = ^TopRTproc;

  TopRTproc = record
    Name: SystemString;
    OnOpCall: TOnOpCall;
    OnOpMethod: TOnOpMethod;
    {$IFNDEF FPC} OnOpProc: TOnOpProc; {$ENDIF FPC}
    procedure Init;
  end;

  TOpCustomRunTime = class(TCoreClassObject)
  private
    procedure FreeNotifyProc(p: Pointer);

    function DoInt(Param: TOpParam): Variant;
    function DoFrac(Param: TOpParam): Variant;
    function DoExp(Param: TOpParam): Variant;
    function DoCos(Param: TOpParam): Variant;
    function DoSin(Param: TOpParam): Variant;
    function DoLn(Param: TOpParam): Variant;
    function DoArcTan(Param: TOpParam): Variant;
    function DoSqrt(Param: TOpParam): Variant;
    function DoTan(Param: TOpParam): Variant;

    function DoGetFirst(Param: TOpParam): Variant;
    function DoDeleteFirst(Param: TOpParam): Variant;
    function DoGetLast(Param: TOpParam): Variant;
    function DoDeleteLast(Param: TOpParam): Variant;
  public
    ProcList: THashList;

    constructor Create; virtual;
    destructor Destroy; override;
    function Execute(sender: TOpCode; const ProcName: SystemString; var Parameter: TOpParam): Variant; virtual;

    procedure RegOp(ProcName: string; OnProc: TOnOpCall); overload;
    procedure RegOp(ProcName: string; OnProc: TOnOpMethod); overload;
    {$IFNDEF FPC} procedure RegOp(ProcName: string; OnProc: TOnOpProc); overload; {$ENDIF FPC}
  end;

  opClass = class of TOpCode;

  TOpCode = class(TCoreClassObject)
  private
    FParam              : TCoreClassList;
    FDestoryTimeFreeLink: Boolean;
    function doExecute(opRT: TOpCustomRunTime): Variant; virtual;
    function GetParam(index: Integer): POpData;
    procedure EvaluateParam(opRT: TOpCustomRunTime); overload;
    procedure EvaluateParam(printLog: Boolean; opRT: TOpCustomRunTime); overload;
  public
    Owner       : TOpCode;
    ParsedInfo  : SystemString;
    ParsedLineNo: Integer;

    constructor Create; overload;
    constructor Create(AFreeLink: Boolean); overload;
    destructor Destroy; override;

    procedure SaveToStream(Stream: TCoreClassStream);

    function AddValue(v: Variant): Integer; overload;
    function AddValue(v: Variant; vt: TOpValueType): Integer; overload;
    function AddLink(obj: TOpCode): Integer;

    function CloneNewSelf: TOpCode;

    property Param[index: Integer]: POpData read GetParam; default;
    function Count: Integer;

    function Execute: Variant; overload;
    function Execute(opRT: TOpCustomRunTime): Variant; overload;

    property DestoryTimeFreeLink: Boolean read FDestoryTimeFreeLink write FDestoryTimeFreeLink;
  end;

  op_Value = class(TOpCode)
  private
    // a
    function doExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Proc = class(TOpCode)
  private
    // proc(a,b,c)
    function doExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Add = class(TOpCode)
  private
    // a + b + n...
    function doExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Sub = class(TOpCode)
  private
    // a - b - n...
    function doExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Mul = class(TOpCode)
  private
    // a * b * n...
    function doExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Div = class(TOpCode)
  private
    // a / b / n...
    function doExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_IntDiv = class(TOpCode)
  private
    // a div b div n...
    function doExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Pow = class(TOpCode)
  private
    // a pow b
    function doExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Mod = class(TOpCode)
  private
    // a mod b mod n...
    function doExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Or = class(TOpCode)
  private
    // a or b or n...
    function doExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_And = class(TOpCode)
  private
    // a and b and n...
    function doExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Xor = class(TOpCode)
  private
    // a xor b xor n...
    function doExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Shl = class(TOpCode)
  private
    // a shl b shl n...
    function doExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Shr = class(TOpCode)
  private
    // a shr b shr n...
    function doExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Equal = class(TOpCode)
  private
    // a = b
    function doExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_LessThan = class(TOpCode)
  private
    // a < b
    function doExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_EqualOrLessThan = class(TOpCode)
  private
    // a <= b
    function doExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_GreaterThan = class(TOpCode)
  private
    // a > b
    function doExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_EqualOrGreaterThan = class(TOpCode)
  private
    // a >= b
    function doExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_NotEqual = class(TOpCode)
  private
    // a <> b
    function doExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Symbol_Sub = class(TOpCode)
  private
    // -a
    function doExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

  op_Symbol_Add = class(TOpCode)
  private
    // +a
    function doExecute(opRT: TOpCustomRunTime): Variant; override;
  end;

function LoadOpFromStream(Stream: TCoreClassStream; out LoadedOp: TOpCode): Boolean;

var
  DefaultOpRT: TOpCustomRunTime;

implementation

uses DataFrameEngine;

type
  opRegData = record
    opClass: opClass;
    OpName: SystemString;
    Hash: Cardinal;
  end;

  POpRegData = ^opRegData;

var
  OpList: TCoreClassList;

function GetRegistedOp(Name: SystemString): POpRegData; inline;
var
  i   : Integer;
  p   : POpRegData;
  Hash: Cardinal;
begin
  Result := nil;
  Hash := FastHashPascalString(@name);
  for i := 0 to OpList.Count - 1 do
    begin
      p := OpList[i];
      if (p^.Hash = Hash) and (SameText(name, p^.OpName)) then
          Exit(p);
    end;
end;

procedure RegisterOp(c: opClass);
var
  p: POpRegData;
begin
  if GetRegistedOp(c.ClassName) <> nil then
      raise Exception.Create('same op ' + c.ClassName);
  New(p);
  p^.opClass := c;
  p^.OpName := p^.opClass.ClassName;
  p^.Hash := FastHashPascalString(@p^.OpName);
  OpList.Add(p);
end;

procedure _FreeOp;
var
  i: Integer;
  p: POpRegData;
begin
  for i := 0 to OpList.Count - 1 do
    begin
      p := OpList[i];
      Dispose(p);
    end;
  DisposeObject(OpList);
end;

function LoadOpFromStream(Stream: TCoreClassStream; out LoadedOp: TOpCode): Boolean;

  function LoadFromDataFrame_1(CurDataEng: TDataFrameEngine): TOpCode;
  var
    AName     : SystemString;
    RegPtr    : POpRegData;
    i, cnt    : Integer;
    NeedNewOp : Boolean;
    newDataEng: TDataFrameEngine;
    v         : Variant;
    vt        : TOpValueType;
  begin
    AName := CurDataEng.Reader.ReadString;
    RegPtr := GetRegistedOp(AName);
    if RegPtr <> nil then
      begin
        Result := RegPtr^.opClass.Create;
        Result.ParsedInfo := CurDataEng.Reader.ReadString;
        Result.ParsedLineNo := CurDataEng.Reader.ReadInteger;
        cnt := CurDataEng.Reader.ReadInteger;
        for i := 0 to cnt - 1 do
          begin
            NeedNewOp := CurDataEng.Reader.ReadBool;

            if NeedNewOp then
              begin
                // create new TOpCode
                newDataEng := TDataFrameEngine.Create;
                CurDataEng.Reader.ReadDataFrame(newDataEng);
                Result.AddLink(LoadFromDataFrame_1(newDataEng));
                DisposeObject(newDataEng);
              end
            else
              begin
                v := CurDataEng.Reader.ReadVariant;
                vt := TOpValueType(CurDataEng.Reader.ReadInteger);
                Result.AddValue(v, vt);
              end;
          end;
      end
    else
        raise Exception.Create('opCode failed');
  end;

var
  dataEng    : TDataFrameEngine;
  DataEdition: Integer;
begin
  Result := False;
  dataEng := TDataFrameEngine.Create;
  try
    dataEng.LoadFromStream(Stream);
    DataEdition := dataEng.Reader.ReadInteger;
    if DataEdition = 1 then
      begin
        LoadedOp := LoadFromDataFrame_1(dataEng);
        Result := True;
      end
    else
        LoadedOp := nil;
  except
  end;
  DisposeObject(dataEng);
end;

procedure TopRTproc.Init;
begin
  name := '';
  OnOpCall := nil;
  OnOpMethod := nil;
  {$IFNDEF FPC} OnOpProc := nil; {$ENDIF FPC}
end;

procedure TOpCustomRunTime.FreeNotifyProc(p: Pointer);
begin
  Dispose(PopRTproc(p));
end;

function TOpCustomRunTime.DoInt(Param: TOpParam): Variant;
var
  v: Variant;
  i: Integer;
begin
  v := 0;
  for i := low(Param) to high(Param) do
      v := v + Param[i];
  Result := Int(v);
end;

function TOpCustomRunTime.DoFrac(Param: TOpParam): Variant;
var
  v: Variant;
  i: Integer;
begin
  v := 0;
  for i := low(Param) to high(Param) do
      v := v + Param[i];
  Result := Frac(v);
end;

function TOpCustomRunTime.DoExp(Param: TOpParam): Variant;
var
  v: Variant;
  i: Integer;
begin
  v := 0;
  for i := low(Param) to high(Param) do
      v := v + Param[i];
  Result := Exp(v);
end;

function TOpCustomRunTime.DoCos(Param: TOpParam): Variant;
var
  v: Variant;
  i: Integer;
begin
  v := 0;
  for i := low(Param) to high(Param) do
      v := v + Param[i];
  Result := Cos(v);
end;

function TOpCustomRunTime.DoSin(Param: TOpParam): Variant;
var
  v: Variant;
  i: Integer;
begin
  v := 0;
  for i := low(Param) to high(Param) do
      v := v + Param[i];
  Result := Sin(v);
end;

function TOpCustomRunTime.DoLn(Param: TOpParam): Variant;
var
  v: Variant;
  i: Integer;
begin
  v := 0;
  for i := low(Param) to high(Param) do
      v := v + Param[i];
  Result := Ln(v);
end;

function TOpCustomRunTime.DoArcTan(Param: TOpParam): Variant;
var
  v: Variant;
  i: Integer;
begin
  v := 0;
  for i := low(Param) to high(Param) do
      v := v + Param[i];
  Result := ArcTan(v);
end;

function TOpCustomRunTime.DoSqrt(Param: TOpParam): Variant;
var
  v: Variant;
  i: Integer;
begin
  v := 0;
  for i := low(Param) to high(Param) do
      v := v + Param[i];
  Result := Sqrt(v);
end;

function TOpCustomRunTime.DoTan(Param: TOpParam): Variant;
var
  v: Variant;
  i: Integer;
begin
  v := 0;
  for i := low(Param) to high(Param) do
      v := v + Param[i];
  Result := Tan(v);
end;

function TOpCustomRunTime.DoGetFirst(Param: TOpParam): Variant;
begin
  if Length(Param) = 2 then
      Result := umlGetFirstStr(VarToStr(Param[0]), VarToStr(Param[1])).Text
  else
      Result := '';
end;

function TOpCustomRunTime.DoDeleteFirst(Param: TOpParam): Variant;
begin
  if Length(Param) = 2 then
      Result := umlDeleteFirstStr(VarToStr(Param[0]), VarToStr(Param[1])).Text
  else
      Result := '';
end;

function TOpCustomRunTime.DoGetLast(Param: TOpParam): Variant;
begin
  if Length(Param) = 2 then
      Result := umlGetLastStr(VarToStr(Param[0]), VarToStr(Param[1])).Text
  else
      Result := '';
end;

function TOpCustomRunTime.DoDeleteLast(Param: TOpParam): Variant;
begin
  if Length(Param) = 2 then
      Result := umlDeleteLastStr(VarToStr(Param[0]), VarToStr(Param[1])).Text
  else
      Result := '';
end;

constructor TOpCustomRunTime.Create;
begin
  inherited Create;
  ProcList := THashList.Create(256);
  ProcList.AutoFreeData := True;
  {$IFDEF FPC}
  ProcList.OnDataFreeProc := @FreeNotifyProc;
  RegOp('Int', @DoInt);
  RegOp('Frac', @DoFrac);
  RegOp('Exp', @DoExp);
  RegOp('Cos', @DoCos);
  RegOp('Sin', @DoSin);
  RegOp('Ln', @DoLn);
  RegOp('ArcTan', @DoArcTan);
  RegOp('Sqrt', @DoSqrt);
  RegOp('Tan', @DoTan);
  RegOp('GetFirst', @DoGetFirst);
  RegOp('DeleteFirst', @DoDeleteFirst);
  RegOp('GetLast', @DoGetLast);
  RegOp('DeleteLast', @DoDeleteLast);
  {$ELSE }
  ProcList.OnDataFreeProc := FreeNotifyProc;
  RegOp('Int', DoInt);
  RegOp('Frac', DoFrac);
  RegOp('Exp', DoExp);
  RegOp('Cos', DoCos);
  RegOp('Sin', DoSin);
  RegOp('Ln', DoLn);
  RegOp('ArcTan', DoArcTan);
  RegOp('Sqrt', DoSqrt);
  RegOp('Tan', DoTan);
  RegOp('GetFirst', DoGetFirst);
  RegOp('DeleteFirst', DoDeleteFirst);
  RegOp('GetLast', DoGetLast);
  RegOp('DeleteLast', DoDeleteLast);
  {$ENDIF FPC}
end;

destructor TOpCustomRunTime.Destroy;
begin
  DisposeObject(ProcList);
  inherited Destroy;
end;

function TOpCustomRunTime.Execute(sender: TOpCode; const ProcName: SystemString; var Parameter: TOpParam): Variant;
var
  p: PopRTproc;
begin
  Result := 0;
  p := ProcList[ProcName];
  if p <> nil then
    begin
      if Assigned(p^.OnOpCall) then
          Result := p^.OnOpCall(Parameter);
      if Assigned(p^.OnOpMethod) then
          Result := p^.OnOpMethod(Parameter);
      {$IFNDEF FPC}
      if Assigned(p^.OnOpProc) then
          Result := p^.OnOpProc(Parameter);
      {$ENDIF FPC}
    end;
end;

procedure TOpCustomRunTime.RegOp(ProcName: string; OnProc: TOnOpCall);
var
  p: PopRTproc;
begin
  New(p);
  p^.Init;
  p^.Name := ProcName;
  p^.OnOpCall := OnProc;
  ProcList.Add(ProcName, p);
end;

procedure TOpCustomRunTime.RegOp(ProcName: string; OnProc: TOnOpMethod);
var
  p: PopRTproc;
begin
  New(p);
  p^.Init;
  p^.Name := ProcName;
  p^.OnOpMethod := OnProc;
  ProcList.Add(ProcName, p);
end;

{$IFNDEF FPC}


procedure TOpCustomRunTime.RegOp(ProcName: string; OnProc: TOnOpProc);
var
  p: PopRTproc;
begin
  New(p);
  p^.Init;
  p^.Name := ProcName;
  p^.OnOpProc := OnProc;
  ProcList.Add(ProcName, p);
end;
{$ENDIF FPC}


function TOpCode.doExecute(opRT: TOpCustomRunTime): Variant;
begin
  Result := NULL;
end;

function TOpCode.GetParam(index: Integer): POpData;
begin
  Result := FParam[index];
end;

procedure TOpCode.EvaluateParam(opRT: TOpCustomRunTime);
begin
  EvaluateParam(False, opRT);
end;

procedure TOpCode.EvaluateParam(printLog: Boolean; opRT: TOpCustomRunTime);
var
  i: Integer;
  p: POpData;
begin
  for i := 0 to FParam.Count - 1 do
    begin
      p := FParam[i];
      if p^.OnGet <> nil then
        begin
          try
              p^.OnGet.EvaluateParam(printLog, opRT);
          except
          end;

          try
            p^.Value := p^.OnGet.doExecute(opRT);

            if printLog then
                DoStatus('%s value:%s', [ClassName, VarToStr(p^.Value)]);
          except
          end;
        end;
    end;
end;

constructor TOpCode.Create;
begin
  inherited Create;
  Owner := nil;
  FParam := TCoreClassList.Create;
  FDestoryTimeFreeLink := True;
  ParsedInfo := '';
  ParsedLineNo := 0;
end;

constructor TOpCode.Create(AFreeLink: Boolean);
begin
  inherited Create;
  Owner := nil;
  FParam := TCoreClassList.Create;
  FDestoryTimeFreeLink := AFreeLink;
  ParsedInfo := '';
  ParsedLineNo := 0;
end;

destructor TOpCode.Destroy;
var
  i: Integer;
  p: POpData;
begin
  if FParam <> nil then
    begin
      for i := 0 to FParam.Count - 1 do
        begin
          p := FParam[i];
          if (FDestoryTimeFreeLink) and (p^.OnGet <> nil) then
              DisposeObject(p^.OnGet);
          Dispose(p);
        end;
      FParam.Clear;
      DisposeObject(FParam);
    end;
  inherited Destroy;
end;

procedure TOpCode.SaveToStream(Stream: TCoreClassStream);
  procedure SaveToDataFrame(Op: TOpCode; CurDataEng: TDataFrameEngine);
  var
    i         : Integer;
    p         : POpData;
    newDataEng: TDataFrameEngine;
  begin
    CurDataEng.WriteString(Op.ClassName);
    CurDataEng.WriteString(Op.ParsedInfo);
    CurDataEng.WriteInteger(Op.ParsedLineNo);
    CurDataEng.WriteInteger(Op.Count);
    for i := 0 to Op.Count - 1 do
      begin
        p := Op[i];
        if p^.OnGet <> nil then
          begin
            CurDataEng.WriteBool(True);
            newDataEng := TDataFrameEngine.Create;
            SaveToDataFrame(p^.OnGet, newDataEng);
            CurDataEng.WriteDataFrame(newDataEng);
            DisposeObject(newDataEng);
          end
        else
          begin
            CurDataEng.WriteBool(False);
            CurDataEng.WriteVariant(p^.Value);
            CurDataEng.WriteInteger(Integer(p^.ValueType));
          end;
      end;
  end;

var
  dataEng: TDataFrameEngine;
begin
  dataEng := TDataFrameEngine.Create;
  dataEng.WriteInteger(1);
  SaveToDataFrame(Self, dataEng);
  dataEng.SaveToStream(Stream);
  DisposeObject(dataEng);
end;

function TOpCode.AddValue(v: Variant): Integer;
var
  p: POpData;
begin
  New(p);
  p^.OnGet := nil;

  p^.Value := v;

  case VarType(v) of
    varSmallint: p^.ValueType := ovtSmallInt;
    varInteger: p^.ValueType := ovtInt;
    varSingle: p^.ValueType := ovtSingle;
    varDouble: p^.ValueType := ovtDouble;
    varCurrency: p^.ValueType := ovtCurrency;
    varBoolean: p^.ValueType := ovtBool;
    varShortInt: p^.ValueType := ovtShortInt;
    varByte: p^.ValueType := ovtByte;
    varWord: p^.ValueType := ovtWord;
    varLongWord: p^.ValueType := ovtUInt;
    varInt64: p^.ValueType := ovtInt64;
    varUInt64: p^.ValueType := ovtUInt64;
    else
      begin
        if VarIsStr(v) then
            p^.ValueType := ovtString
        else
            p^.ValueType := ovtUnknow;
      end;
  end;

  Result := FParam.Add(p);
end;

function TOpCode.AddValue(v: Variant; vt: TOpValueType): Integer;
var
  p: POpData;
begin
  New(p);
  p^.OnGet := nil;
  p^.Value := v;
  p^.ValueType := vt;
  Result := FParam.Add(p);
end;

function TOpCode.AddLink(obj: TOpCode): Integer;
var
  p: POpData;
begin
  New(p);

  if obj.Owner <> nil then
      p^.OnGet := obj.CloneNewSelf
  else
      p^.OnGet := obj;

  p^.OnGet.Owner := Self;

  p^.Value := NULL;
  p^.ValueType := ovtUnknow;
  Result := FParam.Add(p);
end;

function TOpCode.CloneNewSelf: TOpCode;
var
  i: Integer;
  p: POpData;
begin
  Result := opClass(Self.ClassType).Create;
  Result.ParsedInfo := Self.ParsedInfo;
  Result.ParsedLineNo := Self.ParsedLineNo;

  for i := 0 to FParam.Count - 1 do
    begin
      p := FParam[i];
      if p^.OnGet <> nil then
          Result.AddLink(p^.OnGet.CloneNewSelf)
      else
          Result.AddValue(p^.Value, p^.ValueType);
    end;
end;

function TOpCode.Count: Integer;
begin
  Result := FParam.Count;
end;

function TOpCode.Execute: Variant;
begin
  Result := Execute(DefaultOpRT);
end;

function TOpCode.Execute(opRT: TOpCustomRunTime): Variant;
begin
  try
      EvaluateParam(opRT);
  except
      Exit(NULL);
  end;

  try
      Result := doExecute(opRT);
  except
      Result := NULL;
  end;
end;

{ op_Value }

function op_Value.doExecute(opRT: TOpCustomRunTime): Variant;
begin
  Result := Param[0]^.Value;
end;

{ op_Proc }
function op_Proc.doExecute(opRT: TOpCustomRunTime): Variant;
var
  arr: TOpParam;
  i  : Integer;
begin
  if opRT <> nil then
    begin
      SetLength(arr, Count - 1);
      for i := low(arr) to high(arr) do
          arr[i] := Param[i + 1]^.Value;
      Result := opRT.Execute(Self, SystemString(Param[0]^.Value), arr);
      SetLength(arr, 0);
    end
  else
      Result := 0;
end;

{ op_Add }

function op_Add.doExecute(opRT: TOpCustomRunTime): Variant;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;
  for i := 1 to Count - 1 do
    begin
      try
          Result := Result + Param[i]^.Value;
      except
      end;
    end;
end;

{ op_Sub }

function op_Sub.doExecute(opRT: TOpCustomRunTime): Variant;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;
  for i := 1 to Count - 1 do
      Result := Result - Param[i]^.Value;
end;

{ op_Mul }

function op_Mul.doExecute(opRT: TOpCustomRunTime): Variant;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;
  for i := 1 to Count - 1 do
      Result := Result * Param[i]^.Value;
end;

{ op_Div }

function op_Div.doExecute(opRT: TOpCustomRunTime): Variant;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;
  for i := 1 to Count - 1 do
      Result := Result / Param[i]^.Value;
end;

{ op_IntDiv }

function op_IntDiv.doExecute(opRT: TOpCustomRunTime): Variant;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;
  for i := 1 to Count - 1 do
      Result := Result div Param[i]^.Value;
end;

{ op_Pow }
function op_Pow.doExecute(opRT: TOpCustomRunTime): Variant;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;
  for i := 1 to Count - 1 do
      Result := Power(Result, Param[i]^.Value);
end;

{ op_Mod }

function op_Mod.doExecute(opRT: TOpCustomRunTime): Variant;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;
  for i := 1 to Count - 1 do
      Result := Result mod Param[i]^.Value;
end;

{ op_Or }

function op_Or.doExecute(opRT: TOpCustomRunTime): Variant;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;
  for i := 1 to Count - 1 do
      Result := Result or Param[i]^.Value;
end;

{ op_And }

function op_And.doExecute(opRT: TOpCustomRunTime): Variant;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;
  for i := 1 to Count - 1 do
      Result := Result and Param[i]^.Value;
end;

{ op_Xor }

function op_Xor.doExecute(opRT: TOpCustomRunTime): Variant;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;
  for i := 1 to Count - 1 do
      Result := Result xor Param[i]^.Value;
end;

{ op_shl }

function op_Shl.doExecute(opRT: TOpCustomRunTime): Variant;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;
  for i := 1 to Count - 1 do
      Result := UInt64(Result) shl UInt64(Param[i]^.Value);
end;

{ op_shr }

function op_Shr.doExecute(opRT: TOpCustomRunTime): Variant;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;
  for i := 1 to Count - 1 do
      Result := UInt64(Result) shr UInt64(Param[i]^.Value);
end;

{ op_Equal }

function op_Equal.doExecute(opRT: TOpCustomRunTime): Variant;
begin
  Result := Param[0]^.Value = Param[1]^.Value;
end;

{ op_LessThan }

function op_LessThan.doExecute(opRT: TOpCustomRunTime): Variant;
begin
  Result := Param[0]^.Value < Param[1]^.Value;
end;

{ op_EqualOrLessThan }

function op_EqualOrLessThan.doExecute(opRT: TOpCustomRunTime): Variant;
begin
  Result := Param[0]^.Value <= Param[1]^.Value;
end;

{ op_GreaterThan }

function op_GreaterThan.doExecute(opRT: TOpCustomRunTime): Variant;
begin
  Result := Param[0]^.Value > Param[1]^.Value;
end;

{ op_EqualOrGreaterThan }

function op_EqualOrGreaterThan.doExecute(opRT: TOpCustomRunTime): Variant;
begin
  Result := Param[0]^.Value >= Param[1]^.Value;
end;

{ op_NotEqual }

function op_NotEqual.doExecute(opRT: TOpCustomRunTime): Variant;
begin
  Result := Param[0]^.Value <> Param[1]^.Value;
end;

{ op_Symbol_Sub }

function op_Symbol_Sub.doExecute(opRT: TOpCustomRunTime): Variant;
begin
  Result := -Param[0]^.Value;
end;

{ op_Symbol_Add }

function op_Symbol_Add.doExecute(opRT: TOpCustomRunTime): Variant;
begin
  Result := (Param[0]^.Value);
end;

initialization

DefaultOpRT := TOpCustomRunTime.Create;
OleVariantInt64AsDouble := True;

OpList := TCoreClassList.Create;

RegisterOp(op_Value);
RegisterOp(op_Proc);
RegisterOp(op_Add);
RegisterOp(op_Sub);
RegisterOp(op_Mul);
RegisterOp(op_Div);
RegisterOp(op_IntDiv);
RegisterOp(op_Mod);
RegisterOp(op_Or);
RegisterOp(op_And);
RegisterOp(op_Xor);
RegisterOp(op_Shl);
RegisterOp(op_Shr);
RegisterOp(op_Equal);
RegisterOp(op_LessThan);
RegisterOp(op_EqualOrLessThan);
RegisterOp(op_GreaterThan);
RegisterOp(op_EqualOrGreaterThan);
RegisterOp(op_NotEqual);
RegisterOp(op_Symbol_Sub);
RegisterOp(op_Symbol_Add);

finalization

_FreeOp;
DisposeObject(DefaultOpRT);

end.
