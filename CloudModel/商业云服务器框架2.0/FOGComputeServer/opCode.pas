unit opCode;

{$I ..\zDefine.inc}

interface

uses SysUtils, Variants, CoreClasses, UnicodeMixedLib;

type
  TOpValueType = (
    ovtBool, ovtInt, ovtInt64, ovtUInt64, ovtWord, ovtByte, ovtSmallInt, ovtShortInt, ovtUInt,
    ovtSingle, ovtDouble, ovtCurrency,
    ovtString,
    ovtSpaceName,
    ovtUnknow);

  TOpCode = class;

  TOpGetSpaceNameAsValue = procedure(sender: TOpCode; SpaceNameInfo: umlString; out Value: Variant) of object;

  opClass = class of TOpCode;

  opData = record
    OnGet: TOpCode;
    Value: Variant;
    ValueType: TOpValueType;
    SpaceNameInfo: umlString;
  end;

  POpData = ^opData;

  TOpCode = class(TCoreClassObject)
  private
    FParam              : TCoreClassList;
    FDestoryTimeFreeLink: Boolean;
    FOnSpaceNameAsValue : TOpGetSpaceNameAsValue;
    function DoExecute: Variant; virtual;
    function GetParam(index: Integer): POpData;
    procedure SetOnSpaceNameAsValue(const Value: TOpGetSpaceNameAsValue);
  public
    Owner       : TOpCode;
    ParsedInfo  : umlString;
    ParsedLineNo: Integer;

    constructor Create; overload;
    constructor Create(AFreeLink: Boolean); overload;
    destructor Destroy; override;

    procedure SaveToStream(Stream: TCoreClassStream);

    function AddValue(v: Variant): Integer; overload;
    function AddValue(v: Variant; vt: TOpValueType): Integer; overload;
    function AddLink(obj: TOpCode): Integer;

    function CloneNewSelf: TOpCode;
    procedure EvaluateParam; overload;
    procedure EvaluateParam(printLog: Boolean); overload;

    property Param[index: Integer]: POpData read GetParam; default;
    function Count: Integer;

    function Execute: Variant;

    property DestoryTimeFreeLink: Boolean read FDestoryTimeFreeLink write FDestoryTimeFreeLink;
    property OnSpaceNameAsValue: TOpGetSpaceNameAsValue read FOnSpaceNameAsValue write SetOnSpaceNameAsValue;
  end;

  op_Value = class(TOpCode)
    // a
    function DoExecute: Variant; override;
  end;

  op_Add = class(TOpCode)
    // a + b + n...
    function DoExecute: Variant; override;
  end;

  op_Sub = class(TOpCode)
    // a - b - n...
    function DoExecute: Variant; override;
  end;

  op_Mul = class(TOpCode)
    // a * b * n...
    function DoExecute: Variant; override;
  end;

  op_Div = class(TOpCode)
    // a / b / n...
    function DoExecute: Variant; override;
  end;

  op_IntDiv = class(TOpCode)
    // a div b div n...
    function DoExecute: Variant; override;
  end;

  op_Mod = class(TOpCode)
    // a mod b mod n...
    function DoExecute: Variant; override;
  end;

  op_Or = class(TOpCode)
    // a or b or n...
    function DoExecute: Variant; override;
  end;

  op_And = class(TOpCode)
    // a and b and n...
    function DoExecute: Variant; override;
  end;

  op_Xor = class(TOpCode)
    // a xor b xor n...
    function DoExecute: Variant; override;
  end;

  op_Shl = class(TOpCode)
    // a shl b shl n...
    function DoExecute: Variant; override;
  end;

  op_Shr = class(TOpCode)
    // a shr b shr n...
    function DoExecute: Variant; override;
  end;

  op_Equal = class(TOpCode)
    // a = b
    function DoExecute: Variant; override;
  end;

  op_LessThan = class(TOpCode)
    // a < b
    function DoExecute: Variant; override;
  end;

  op_EqualOrLessThan = class(TOpCode)
    // a <= b
    function DoExecute: Variant; override;
  end;

  op_GreaterThan = class(TOpCode)
    // a > b
    function DoExecute: Variant; override;
  end;

  op_EqualOrGreaterThan = class(TOpCode)
    // a >= b
    function DoExecute: Variant; override;
  end;

  op_NotEqual = class(TOpCode)
    // a <> b
    function DoExecute: Variant; override;
  end;

  op_Symbol_Sub = class(TOpCode)
    // -a
    function DoExecute: Variant; override;
  end;

  op_Symbol_Add = class(TOpCode)
    // +a
    function DoExecute: Variant; override;
  end;

procedure RegisterOp(c: opClass);
function LoadOpFromStream(Stream: TCoreClassStream; out LoadedOp: TOpCode): Boolean;

implementation

uses DataFrameEngine, PascalStrings;

type
  opRegData = record
    opClass: opClass;
    OpName: umlString;
    Hash: Cardinal;
  end;

  POpRegData = ^opRegData;

var
  OpList: TCoreClassList;

function GetRegistedOp(Name: umlString): POpRegData; inline;
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
      if (p^.Hash = Hash) and (umlSameText(name, p^.OpName)) then
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
    AName     : umlString;
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

function TOpCode.DoExecute: Variant;
begin
  Result := NULL;
end;

function TOpCode.GetParam(index: Integer): POpData;
begin
  Result := FParam[index];
end;

procedure TOpCode.SetOnSpaceNameAsValue(const Value: TOpGetSpaceNameAsValue);
var
  i: Integer;
begin
  FOnSpaceNameAsValue := Value;
  for i := 0 to Count - 1 do
    if Param[i]^.OnGet <> nil then
        Param[i]^.OnGet.SetOnSpaceNameAsValue(Value);
end;

constructor TOpCode.Create;
begin
  inherited Create;
  Owner := nil;
  FParam := TCoreClassList.Create;
  FDestoryTimeFreeLink := True;
  ParsedInfo := '';
  ParsedLineNo := 0;
  FOnSpaceNameAsValue := nil;
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
    CurDataEng.WriteString(Op.ParsedInfo.Text);
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
            if p^.ValueType = ovtSpaceName then
                CurDataEng.WriteVariant(p^.SpaceNameInfo.Text)
            else
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

  p^.SpaceNameInfo := '';

  Result := FParam.Add(p);
end;

function TOpCode.AddValue(v: Variant; vt: TOpValueType): Integer;
var
  p: POpData;
begin
  New(p);
  p^.OnGet := nil;
  if vt = ovtSpaceName then
    begin
      p^.Value := NULL;
      p^.ValueType := vt;
      p^.SpaceNameInfo := VarToStr(v);
    end
  else
    begin
      p^.Value := v;
      p^.ValueType := vt;
      p^.SpaceNameInfo := '';
    end;
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
  p^.SpaceNameInfo := '';
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

procedure TOpCode.EvaluateParam;
begin
  EvaluateParam(False);
end;

procedure TOpCode.EvaluateParam(printLog: Boolean);
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
              p^.OnGet.EvaluateParam(printLog);
          except
          end;

          try
              p^.Value := p^.OnGet.DoExecute;
          except
          end;
        end
      else
        begin
          if (p^.ValueType = ovtSpaceName) and (Assigned(FOnSpaceNameAsValue)) then
              FOnSpaceNameAsValue(Self, p^.SpaceNameInfo, p^.Value);
        end;
    end;
end;

function TOpCode.Count: Integer;
begin
  Result := FParam.Count;
end;

function TOpCode.Execute: Variant;
begin
  try
      EvaluateParam;
  except
      Exit(NULL);
  end;

  try
      Result := DoExecute;
  except
      Result := NULL;
  end;
end;

{ op_Value }

function op_Value.DoExecute: Variant;
begin
  Result := Param[0]^.Value;
end;

{ op_Add }

function op_Add.DoExecute: Variant;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;
  for i := 1 to Count - 1 do
      Result := Result + Param[i]^.Value;
end;

{ op_Sub }

function op_Sub.DoExecute: Variant;
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

function op_Mul.DoExecute: Variant;
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

function op_Div.DoExecute: Variant;
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

function op_IntDiv.DoExecute: Variant;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;
  for i := 1 to Count - 1 do
      Result := Result div Param[i]^.Value;
end;

{ op_Mod }

function op_Mod.DoExecute: Variant;
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

function op_Or.DoExecute: Variant;
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

function op_And.DoExecute: Variant;
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

function op_Xor.DoExecute: Variant;
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

function op_Shl.DoExecute: Variant;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;
  for i := 1 to Count - 1 do
      Result := Result xor Param[i]^.Value;
end;

{ op_shr }

function op_Shr.DoExecute: Variant;
var
  i: Integer;
begin
  if Count = 0 then
      Exit(NULL);
  Result := Param[0]^.Value;
  for i := 1 to Count - 1 do
      Result := Result xor Param[i]^.Value;
end;

{ op_Equal }

function op_Equal.DoExecute: Variant;
begin
  Result := Param[0]^.Value = Param[1]^.Value;
end;

{ op_LessThan }

function op_LessThan.DoExecute: Variant;
begin
  Result := Param[0]^.Value < Param[1]^.Value;
end;

{ op_EqualOrLessThan }

function op_EqualOrLessThan.DoExecute: Variant;
begin
  Result := Param[0]^.Value <= Param[1]^.Value;
end;

{ op_GreaterThan }

function op_GreaterThan.DoExecute: Variant;
begin
  Result := Param[0]^.Value > Param[1]^.Value;
end;

{ op_EqualOrGreaterThan }

function op_EqualOrGreaterThan.DoExecute: Variant;
begin
  Result := Param[0]^.Value >= Param[1]^.Value;
end;

{ op_NotEqual }

function op_NotEqual.DoExecute: Variant;
begin
  Result := Param[0]^.Value <> Param[1]^.Value;
end;

{ op_Symbol_Sub }

function op_Symbol_Sub.DoExecute: Variant;
begin
  Result := -Param[0]^.Value;
end;

{ op_Symbol_Add }

function op_Symbol_Add.DoExecute: Variant;
begin
  Result := (Param[0]^.Value);
end;

initialization

OleVariantInt64AsDouble := True;

OpList := TCoreClassList.Create;

RegisterOp(op_Value);
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

end.
