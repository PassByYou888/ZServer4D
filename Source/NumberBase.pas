{ ****************************************************************************** }
{ * Number Module system, create by.qq600585                                   * }
{ ****************************************************************************** }
{ * https://zpascal.net                                                        * }
{ * https://github.com/PassByYou888/zAI                                        * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/PascalString                               * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zChinese                                   * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/FFMPEG-Header                              * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/InfiniteIoT                                * }
{ * https://github.com/PassByYou888/FastMD5                                    * }
{ ****************************************************************************** }
unit NumberBase;

{$INCLUDE zDefine.inc}

interface

uses
{$IFDEF FPC}
  FPCGenericStructlist,
{$ENDIF FPC}
  CoreClasses, GHashList, ListEngine, PascalStrings, TextParsing, zExpression, OpCode;

type
  TNumberModuleHookPool = class;
  TNumberModuleEventPool = class;
  TNumberModulePool = class;
  TNumberModule = class;
  TNumberModuleHookPoolList = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TNumberModuleHookPool>;
  TNumberModuleEventPoolList = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TNumberModuleEventPool>;
  TNumberModulePool_Decl = {$IFDEF FPC}specialize {$ENDIF FPC}TGenericHashList<TNumberModule>;

  TNumberModuleHook = procedure(Sender: TNumberModuleHookPool; OLD_: Variant; var New_: Variant) of object;

  TNumberModuleHookPool = class(TCoreClassObject)
  private
    FOwner: TNumberModule;
    FOwnerList: TNumberModuleHookPoolList;
    FOnCurrentDMHook: TNumberModuleHook;
    FTag: SystemString;
  protected
  public
    constructor Create(Owner_: TNumberModule; OwnerList_: TNumberModuleHookPoolList);
    destructor Destroy; override;
    property Owner: TNumberModule read FOwner;
    property OnCurrentDMHook: TNumberModuleHook read FOnCurrentDMHook write FOnCurrentDMHook;
    property Tag: SystemString read FTag write FTag;
  end;

  TNumberModuleEvent = procedure(Sender: TNumberModuleEventPool; New_: Variant) of object;

  TNumberModuleEventPool = class(TCoreClassObject)
  private
    FOwner: TNumberModule;
    FOwnerList: TNumberModuleEventPoolList;
    FOnCurrentDMEvent: TNumberModuleEvent;
    FTag: SystemString;
  protected
  public
    constructor Create(Owner_: TNumberModule; OwnerList_: TNumberModuleEventPoolList);
    destructor Destroy; override;
    property Owner: TNumberModule read FOwner;
    property OnCurrentDMEvent: TNumberModuleEvent read FOnCurrentDMEvent write FOnCurrentDMEvent;
    property Tag: SystemString read FTag write FTag;
  end;

  TNumberModuleChangeEvent = procedure(Sender: TNumberModule; OLD_, New_: Variant);

  TNumberModule = class(TCoreClassObject)
  private
    FOwner: TNumberModulePool;
    FName, FSymbolName, FDescription, FDetailDescription: SystemString;
    FCurrentValueHookPool: TNumberModuleHookPoolList;
    FCurrentValueChangeAfterEventPool: TNumberModuleEventPoolList;
    FCurrentValue: Variant;
    FOriginValue: Variant;
    FEnabledHook: Boolean;
    FEnabledEvent: Boolean;
    FOnChange: TNumberModuleChangeEvent;
  private
    // prop
    procedure SetName(const Value: SystemString);
    function GetCurrentValue: Variant;
    procedure SetCurrentValue(const Value: Variant);
    function GetOriginValue: Variant;
    procedure SetOriginValue(const Value: Variant);
    // change
    procedure DoCurrentValueHook(const OLD_, New_: Variant);
    procedure Clear;
    // opRunTime
    procedure DoRegOpProc();
    procedure DoRemoveOpProc();
    function OP_DoProc(Sender: TOpCustomRunTime; var OP_Param: TOpParam): Variant;
  private
    // get current
    function GetCurrentAsCardinal: Cardinal;
    function GetCurrentAsDouble: Double;
    function GetCurrentAsInt64: Int64;
    function GetCurrentAsInteger: Integer;
    function GetCurrentAsSingle: Single;
    function GetCurrentAsString: SystemString;
    function GetCurrentAsBool: Boolean;
    // set current
    procedure SetCurrentAsCardinal(const Value: Cardinal);
    procedure SetCurrentAsDouble(const Value: Double);
    procedure SetCurrentAsInt64(const Value: Int64);
    procedure SetCurrentAsInteger(const Value: Integer);
    procedure SetCurrentAsSingle(const Value: Single);
    procedure SetCurrentAsString(const Value: SystemString);
    procedure SetCurrentAsBool(const Value: Boolean);
    // get origin
    function GetOriginAsCardinal: Cardinal;
    function GetOriginAsDouble: Double;
    function GetOriginAsInt64: Int64;
    function GetOriginAsInteger: Integer;
    function GetOriginAsSingle: Single;
    function GetOriginAsString: SystemString;
    function GetOriginAsBool: Boolean;
    // set origin
    procedure SetOriginAsCardinal(const Value: Cardinal);
    procedure SetOriginAsDouble(const Value: Double);
    procedure SetOriginAsInt64(const Value: Int64);
    procedure SetOriginAsInteger(const Value: Integer);
    procedure SetOriginAsSingle(const Value: Single);
    procedure SetOriginAsString(const Value: SystemString);
    procedure SetOriginAsBool(const Value: Boolean);
  public
    UserObject: TCoreClassObject;
    UserData: Pointer;
    UserVariant: Variant;
    Tag: Integer;
    constructor Create(Owner_: TNumberModulePool);
    destructor Destroy; override;
    // base prop
    property OnChange: TNumberModuleChangeEvent read FOnChange write FOnChange;
    property EnabledHook: Boolean read FEnabledHook write FEnabledHook;
    property EnabledEvent: Boolean read FEnabledEvent write FEnabledEvent;
    property Owner: TNumberModulePool read FOwner;
    property Name: SystemString read FName write SetName;
    property SymbolName: SystemString read FSymbolName write FSymbolName;
    property Description: SystemString read FDescription write FDescription;
    property DetailDescription: SystemString read FDetailDescription write FDetailDescription;
    // api
    procedure DoChange;
    function RegisterCurrentValueHook: TNumberModuleHookPool;
    procedure CopyHookInterfaceFrom(sour: TNumberModule);
    function RegisterCurrentValueChangeAfterEvent: TNumberModuleEventPool;
    procedure CopyChangeAfterEventInterfaceFrom(sour: TNumberModule);
    procedure Assign(sour: TNumberModule);
    // current
    property AsValue: Variant read GetCurrentValue write SetCurrentValue;
    property AsSingle: Single read GetCurrentAsSingle write SetCurrentAsSingle;
    property AsDouble: Double read GetCurrentAsDouble write SetCurrentAsDouble;
    property AsInteger: Integer read GetCurrentAsInteger write SetCurrentAsInteger;
    property AsInt64: Int64 read GetCurrentAsInt64 write SetCurrentAsInt64;
    property AsCardinal: Cardinal read GetCurrentAsCardinal write SetCurrentAsCardinal;
    property AsString: SystemString read GetCurrentAsString write SetCurrentAsString;
    property AsBool: Boolean read GetCurrentAsBool write SetCurrentAsBool;
    property CurrentValue: Variant read GetCurrentValue write SetCurrentValue;
    property CurrentAsSingle: Single read GetCurrentAsSingle write SetCurrentAsSingle;
    property CurrentAsDouble: Double read GetCurrentAsDouble write SetCurrentAsDouble;
    property CurrentAsInteger: Integer read GetCurrentAsInteger write SetCurrentAsInteger;
    property CurrentAsInt64: Int64 read GetCurrentAsInt64 write SetCurrentAsInt64;
    property CurrentAsCardinal: Cardinal read GetCurrentAsCardinal write SetCurrentAsCardinal;
    property CurrentAsString: SystemString read GetCurrentAsString write SetCurrentAsString;
    property CurrentAsBool: Boolean read GetCurrentAsBool write SetCurrentAsBool;
    // origin
    property OriginValue: Variant read GetOriginValue write SetOriginValue;
    property OriginAsSingle: Single read GetOriginAsSingle write SetOriginAsSingle;
    property OriginAsDouble: Double read GetOriginAsDouble write SetOriginAsDouble;
    property OriginAsInteger: Integer read GetOriginAsInteger write SetOriginAsInteger;
    property OriginAsInt64: Int64 read GetOriginAsInt64 write SetOriginAsInt64;
    property OriginAsCardinal: Cardinal read GetOriginAsCardinal write SetOriginAsCardinal;
    property OriginAsString: SystemString read GetOriginAsString write SetOriginAsString;
    property OriginAsBool: Boolean read GetOriginAsBool write SetOriginAsBool;
    // direct,no trigger
    property DirectValue: Variant read FCurrentValue write FCurrentValue;
    property DirectCurrentValue: Variant read FCurrentValue write FCurrentValue;
    property DirectOriginValue: Variant read FOriginValue write FOriginValue;
  end;

  TGetDMAsString = function(key: SystemString; NM: TNumberModule): SystemString;

  TNumberModulePool = class(TCoreClassObject)
  protected
    FList: TNumberModulePool_Decl;
    FExpOpRunTime: TOpCustomRunTime;
    function OP_DoNewNM(Sender: TOpCustomRunTime; var OP_Param: TOpParam): Variant;
    procedure SwapInstance_Progress(const Name: PSystemString; Obj: TNumberModule);
    procedure RebuildOpRunTime_Progress(const Name: PSystemString; Obj: TNumberModule);
    procedure DoChangeAll_Progress(const Name: PSystemString; Obj: TNumberModule);
    function GetExpOpRunTime: TOpCustomRunTime;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    // instance
    procedure SwapInstance(source: TNumberModulePool);
    // script
    procedure RebuildOpRunTime;
    property ExpOpRunTime: TOpCustomRunTime read GetExpOpRunTime;
    function IsVectorScript(ExpressionText_: SystemString; TS_: TTextStyle): Boolean; overload;
    function IsVectorScript(ExpressionText_: SystemString): Boolean; overload;
    function RunScript(ExpressionText_: SystemString; TS_: TTextStyle): Variant; overload;
    function RunScript(ExpressionText_: SystemString): Variant; overload;
    function RunVectorScript(ExpressionText_: SystemString; TS_: TTextStyle): TExpressionValueVector; overload;
    function RunVectorScript(ExpressionText_: SystemString): TExpressionValueVector; overload;
    // trigger
    procedure DoNMChange(Sender: TNumberModule; OLD_, New_: Variant); virtual;
    // api
    procedure Delete(Name_: SystemString); virtual;
    function Exists(Name_: SystemString): Boolean; virtual;
    function ExistsIntf(NM_: TNumberModule): Boolean; virtual;
    procedure Clear; virtual;
    function Macro(const text_, HeadToken, TailToken, OwnerFlag: SystemString; out output: SystemString): Boolean; virtual;
    function ManualMacro(const text_, HeadToken, TailToken, OwnerFlag: SystemString; OnDM2Text: TGetDMAsString; out output: SystemString): Boolean; virtual;
    procedure Assign(source: TNumberModulePool); virtual;
    procedure DoChangeAll; virtual;
    // load and merge from DFE
    procedure LoadFromStream(stream: TCoreClassStream); virtual;
    // save
    procedure SaveToStream(stream: TCoreClassStream); virtual;
    // load merge current from HashVariant
    procedure LoadFromVariantList(L: THashVariantList); virtual;
    // save as text
    procedure SaveToVariantList(L: THashVariantList); virtual;
    function GetItems(Name_: SystemString): TNumberModule; virtual;
    property Items[Name_: SystemString]: TNumberModule read GetItems; default;
    property List: TNumberModulePool_Decl read FList;
    class procedure test;
  end;

implementation

uses SysUtils, Variants,
  UnicodeMixedLib, DataFrameEngine, DoStatusIO;

function __GetNMAsString(key: SystemString; NM: TNumberModule): SystemString;
begin
  if key = '' then
      Result := NM.CurrentAsString
  else if SameText(key, 'Value') then
      Result := NM.CurrentAsString
  else if SameText(key, 'Origin') then
      Result := NM.OriginAsString
  else if SameText(key, 'Name') then
      Result := NM.Name
  else if SameText(key, 'Symbol') then
      Result := NM.SymbolName
  else if SameText(key, 'Description') then
      Result := NM.Description
  else if SameText(key, 'Detail') then
      Result := NM.DetailDescription
  else
      Result := NM.CurrentAsString;
end;

constructor TNumberModuleHookPool.Create(Owner_: TNumberModule; OwnerList_: TNumberModuleHookPoolList);
begin
  inherited Create;
  FOwner := Owner_;
  FOwnerList := OwnerList_;
  FOnCurrentDMHook := nil;
  FTag := '';
  if FOwnerList <> nil then
      FOwnerList.Add(Self);
end;

destructor TNumberModuleHookPool.Destroy;
var
  i: Integer;
begin
  i := 0;
  if FOwnerList <> nil then
    while i < FOwnerList.Count do
      begin
        if FOwnerList[i] = Self then
            FOwnerList.Delete(i)
        else
            inc(i);
      end;
  inherited Destroy;
end;

constructor TNumberModuleEventPool.Create(Owner_: TNumberModule; OwnerList_: TNumberModuleEventPoolList);
begin
  inherited Create;
  FOwner := Owner_;
  FOwnerList := OwnerList_;
  FOnCurrentDMEvent := nil;
  FTag := '';
  if FOwnerList <> nil then
      FOwnerList.Add(Self);
end;

destructor TNumberModuleEventPool.Destroy;
var
  i: Integer;
begin
  i := 0;
  if FOwnerList <> nil then
    while i < FOwnerList.Count do
      begin
        if FOwnerList[i] = Self then
            FOwnerList.Delete(i)
        else
            inc(i);
      end;
  inherited Destroy;
end;

procedure TNumberModule.SetName(const Value: SystemString);
begin
  if Value <> FName then
    begin
      if FOwner <> nil then
        begin
          if FOwner.FList.ReName(FName, Value) then
              FName := Value;
          FOwner.RebuildOpRunTime;
        end
      else
          FName := Value;
    end;
end;

function TNumberModule.GetCurrentValue: Variant;
begin
  Result := FCurrentValue;
end;

procedure TNumberModule.SetCurrentValue(const Value: Variant);
begin
  if VarIsNull(FOriginValue) then
    begin
      FOriginValue := Value;
      DoCurrentValueHook(FCurrentValue, FOriginValue);
    end
  else
      DoCurrentValueHook(FCurrentValue, Value);
end;

function TNumberModule.GetOriginValue: Variant;
begin
  Result := FOriginValue;
end;

procedure TNumberModule.SetOriginValue(const Value: Variant);
begin
  FOriginValue := Value;
  DoCurrentValueHook(FCurrentValue, FOriginValue);
end;

procedure TNumberModule.DoCurrentValueHook(const OLD_, New_: Variant);
var
  i: Integer;
  H_: TNumberModuleHookPool;
  E_: TNumberModuleEventPool;
  N_: Variant;
begin
  N_ := New_;
  if (FEnabledHook) then
    for i := 0 to FCurrentValueHookPool.Count - 1 do
      begin
        H_ := TNumberModuleHookPool(FCurrentValueHookPool[i]);
        if Assigned(H_.FOnCurrentDMHook) then
          begin
            try
                H_.FOnCurrentDMHook(H_, FCurrentValue, N_);
            except
            end;
          end;
      end;
  FCurrentValue := N_;
  // trigger change event
  if (FEnabledEvent) then
    for i := 0 to FCurrentValueChangeAfterEventPool.Count - 1 do
      begin
        E_ := FCurrentValueChangeAfterEventPool[i];
        if Assigned(E_.FOnCurrentDMEvent) then
          begin
            try
                E_.FOnCurrentDMEvent(E_, FCurrentValue);
            except
            end;
          end;
      end;

  if Assigned(FOnChange) then
    begin
      try
          FOnChange(Self, OLD_, N_);
      except
      end;
    end;

  if FOwner <> nil then
      Owner.DoNMChange(Self, OLD_, N_);
end;

procedure TNumberModule.Clear;
begin
  while FCurrentValueHookPool.Count > 0 do
      DisposeObject(FCurrentValueHookPool[0]);
  while FCurrentValueChangeAfterEventPool.Count > 0 do
      DisposeObject(FCurrentValueChangeAfterEventPool[0]);
end;

procedure TNumberModule.DoRegOpProc;
begin
  if Owner <> nil then
      Owner.ExpOpRunTime.RegObjectOpM(Name, Description, {$IFDEF FPC}@{$ENDIF FPC}OP_DoProc);
end;

procedure TNumberModule.DoRemoveOpProc;
begin
  if Owner <> nil then
      Owner.ExpOpRunTime.ProcList.Delete(Name);
end;

function TNumberModule.OP_DoProc(Sender: TOpCustomRunTime; var OP_Param: TOpParam): Variant;
var
  i: Integer;
begin
  if Length(OP_Param) > 0 then
    begin
      Result := OP_Param[0];
      for i := 1 to Length(OP_Param) - 1 do
          Result := Result + OP_Param[i];
      AsValue := Result;
    end
  else
      Result := AsValue;
end;

function TNumberModule.GetCurrentAsCardinal: Cardinal;
begin
  Result := CurrentValue;
end;

function TNumberModule.GetCurrentAsDouble: Double;
begin
  Result := CurrentValue;
end;

function TNumberModule.GetCurrentAsInt64: Int64;
begin
  Result := CurrentValue;
end;

function TNumberModule.GetCurrentAsInteger: Integer;
begin
  Result := CurrentValue;
end;

function TNumberModule.GetCurrentAsSingle: Single;
begin
  Result := CurrentValue;
end;

function TNumberModule.GetCurrentAsString: SystemString;
begin
  Result := VarToStr(CurrentValue);
end;

function TNumberModule.GetCurrentAsBool: Boolean;
begin
  Result := CurrentValue;
end;

procedure TNumberModule.SetCurrentAsCardinal(const Value: Cardinal);
begin
  CurrentValue := Value;
end;

procedure TNumberModule.SetCurrentAsDouble(const Value: Double);
begin
  CurrentValue := Value;
end;

procedure TNumberModule.SetCurrentAsInt64(const Value: Int64);
begin
  CurrentValue := Value;
end;

procedure TNumberModule.SetCurrentAsInteger(const Value: Integer);
begin
  CurrentValue := Value;
end;

procedure TNumberModule.SetCurrentAsSingle(const Value: Single);
begin
  CurrentValue := Value;
end;

procedure TNumberModule.SetCurrentAsString(const Value: SystemString);
begin
  CurrentValue := Value;
end;

procedure TNumberModule.SetCurrentAsBool(const Value: Boolean);
begin
  CurrentValue := Value;
end;

function TNumberModule.GetOriginAsCardinal: Cardinal;
begin
  Result := OriginValue;
end;

function TNumberModule.GetOriginAsDouble: Double;
begin
  Result := OriginValue;
end;

function TNumberModule.GetOriginAsInt64: Int64;
begin
  Result := OriginValue;
end;

function TNumberModule.GetOriginAsInteger: Integer;
begin
  Result := OriginValue;
end;

function TNumberModule.GetOriginAsSingle: Single;
begin
  Result := OriginValue;
end;

function TNumberModule.GetOriginAsString: SystemString;
begin
  Result := VarToStr(OriginValue);
end;

function TNumberModule.GetOriginAsBool: Boolean;
begin
  Result := OriginAsInteger > 0;
end;

procedure TNumberModule.SetOriginAsCardinal(const Value: Cardinal);
begin
  OriginValue := Value;
end;

procedure TNumberModule.SetOriginAsDouble(const Value: Double);
begin
  OriginValue := Value;
end;

procedure TNumberModule.SetOriginAsInt64(const Value: Int64);
begin
  OriginValue := Value;
end;

procedure TNumberModule.SetOriginAsInteger(const Value: Integer);
begin
  OriginValue := Value;
end;

procedure TNumberModule.SetOriginAsSingle(const Value: Single);
begin
  OriginValue := Value;
end;

procedure TNumberModule.SetOriginAsString(const Value: SystemString);
begin
  OriginValue := Value;
end;

procedure TNumberModule.SetOriginAsBool(const Value: Boolean);
begin
  if Value then
      OriginAsInteger := 1
  else
      OriginAsInteger := 0;
end;

constructor TNumberModule.Create(Owner_: TNumberModulePool);
begin
  inherited Create;
  FOwner := Owner_;
  FName := '';
  FSymbolName := '';
  FDescription := '';
  FDetailDescription := '';
  FCurrentValueHookPool := TNumberModuleHookPoolList.Create;
  FCurrentValueChangeAfterEventPool := TNumberModuleEventPoolList.Create;
  FCurrentValue := NULL;
  FOriginValue := NULL;
  FEnabledHook := True;
  FEnabledEvent := True;
  FOnChange := nil;

  UserObject := nil;
  UserData := nil;
  UserVariant := NULL;
  Tag := 0;
end;

destructor TNumberModule.Destroy;
begin
  Clear;
  DisposeObject(FCurrentValueChangeAfterEventPool);
  DisposeObject(FCurrentValueHookPool);
  inherited Destroy;
end;

procedure TNumberModule.DoChange;
begin
  DoCurrentValueHook(FCurrentValue, FCurrentValue);
end;

function TNumberModule.RegisterCurrentValueHook: TNumberModuleHookPool;
begin
  Result := TNumberModuleHookPool.Create(Self, FCurrentValueHookPool);
end;

procedure TNumberModule.CopyHookInterfaceFrom(sour: TNumberModule);
var
  i: Integer;
begin
  // copy new interface
  for i := 0 to sour.FCurrentValueHookPool.Count - 1 do
      RegisterCurrentValueHook.OnCurrentDMHook := TNumberModuleHookPool(sour.FCurrentValueHookPool[i]).OnCurrentDMHook;
end;

function TNumberModule.RegisterCurrentValueChangeAfterEvent: TNumberModuleEventPool;
begin
  Result := TNumberModuleEventPool.Create(Self, FCurrentValueChangeAfterEventPool);
end;

procedure TNumberModule.CopyChangeAfterEventInterfaceFrom(sour: TNumberModule);
var
  i: Integer;
begin
  // copy new interface
  for i := 0 to sour.FCurrentValueChangeAfterEventPool.Count - 1 do
      RegisterCurrentValueChangeAfterEvent.OnCurrentDMEvent := sour.FCurrentValueChangeAfterEventPool[i].OnCurrentDMEvent;
end;

procedure TNumberModule.Assign(sour: TNumberModule);
begin
  FCurrentValue := sour.FCurrentValue;
  FOriginValue := sour.FOriginValue;
  FName := sour.FName;
  FSymbolName := sour.FSymbolName;
  FDescription := sour.FDescription;
  FDetailDescription := sour.FDetailDescription;
end;

function TNumberModulePool.OP_DoNewNM(Sender: TOpCustomRunTime; var OP_Param: TOpParam): Variant;
var
  N_: SystemString;
begin
  N_ := VarToStr(OP_Param[0]);
  if FList.Exists(N_) then
      Items[N_].AsValue := OP_Param[1]
  else
      Items[N_].OriginValue := OP_Param[1];
  Result := OP_Param[1];
end;

procedure TNumberModulePool.SwapInstance_Progress(const Name: PSystemString; Obj: TNumberModule);
begin
  Obj.FOwner := Self;
end;

procedure TNumberModulePool.RebuildOpRunTime_Progress(const Name: PSystemString; Obj: TNumberModule);
begin
  Obj.DoRegOpProc;
end;

procedure TNumberModulePool.DoChangeAll_Progress(const Name: PSystemString; Obj: TNumberModule);
begin
  Obj.DoChange;
end;

function TNumberModulePool.GetExpOpRunTime: TOpCustomRunTime;
begin
  if FExpOpRunTime = nil then
    begin
      FExpOpRunTime := TOpCustomRunTime.CustomCreate($FF);
      FExpOpRunTime.RegObjectOpM('Set', 'Init NM, Set(Name, Value)', {$IFDEF FPC}@{$ENDIF FPC}OP_DoNewNM);
      FExpOpRunTime.RegObjectOpM('Init', 'Init NM, Init(Name, Value)', {$IFDEF FPC}@{$ENDIF FPC}OP_DoNewNM);
      FExpOpRunTime.RegObjectOpM('New', 'Init NM, New(Name, Value)', {$IFDEF FPC}@{$ENDIF FPC}OP_DoNewNM);
      FList.ProgressM({$IFDEF FPC}@{$ENDIF FPC}RebuildOpRunTime_Progress);
    end;
  Result := FExpOpRunTime;
end;

constructor TNumberModulePool.Create;
begin
  inherited Create;
  FList := TNumberModulePool_Decl.Create(True, 1024, nil);
  FExpOpRunTime := nil;
end;

destructor TNumberModulePool.Destroy;
begin
  DisposeObject(FExpOpRunTime);
  DisposeObject(FList);
  inherited Destroy;
end;

procedure TNumberModulePool.SwapInstance(source: TNumberModulePool);
var
  tmp_FList: TNumberModulePool_Decl;
  tmp_FExpOpRunTime: TOpCustomRunTime;
begin
  tmp_FList := FList;
  tmp_FExpOpRunTime := FExpOpRunTime;
  FList := source.FList;
  FExpOpRunTime := source.FExpOpRunTime;
  source.FList := tmp_FList;
  source.FExpOpRunTime := tmp_FExpOpRunTime;
  // Update Owner
  FList.ProgressM({$IFDEF FPC}@{$ENDIF FPC}SwapInstance_Progress);
  source.FList.ProgressM({$IFDEF FPC}@{$ENDIF FPC}source.SwapInstance_Progress);
end;

procedure TNumberModulePool.RebuildOpRunTime;
begin
  DisposeObjectAndNil(FExpOpRunTime);
end;

function TNumberModulePool.IsVectorScript(ExpressionText_: SystemString; TS_: TTextStyle): Boolean;
begin
  Result := IsSymbolVectorExpression(ExpressionText_, TS_, nil);
end;

function TNumberModulePool.IsVectorScript(ExpressionText_: SystemString): Boolean;
begin
  Result := IsVectorScript(ExpressionText_, tsPascal);
end;

function TNumberModulePool.RunScript(ExpressionText_: SystemString; TS_: TTextStyle): Variant;
begin
  Result := EvaluateExpressionValue(True, TS_, ExpressionText_, ExpOpRunTime);
end;

function TNumberModulePool.RunScript(ExpressionText_: SystemString): Variant;
begin
  Result := RunScript(ExpressionText_, tsPascal);
end;

function TNumberModulePool.RunVectorScript(ExpressionText_: SystemString; TS_: TTextStyle): TExpressionValueVector;
begin
  Result := EvaluateExpressionVector(False, True, nil, TS_, ExpressionText_, ExpOpRunTime, nil);
end;

function TNumberModulePool.RunVectorScript(ExpressionText_: SystemString): TExpressionValueVector;
begin
  Result := RunVectorScript(ExpressionText_, tsPascal);
end;

procedure TNumberModulePool.DoNMChange(Sender: TNumberModule; OLD_, New_: Variant);
begin

end;

procedure TNumberModulePool.Delete(Name_: SystemString);
begin
  FList.Delete(Name_);
end;

function TNumberModulePool.Exists(Name_: SystemString): Boolean;
begin
  Result := FList.Exists(Name_);
end;

function TNumberModulePool.ExistsIntf(NM_: TNumberModule): Boolean;
begin
  Result := FList.ExistsObject(NM_);
end;

procedure TNumberModulePool.Clear;
begin
  FList.Clear;
end;

function TNumberModulePool.Macro(const text_, HeadToken, TailToken, OwnerFlag: SystemString; out output: SystemString): Boolean;
begin
  Result := ManualMacro(text_, HeadToken, TailToken, OwnerFlag, {$IFDEF FPC}@{$ENDIF FPC}__GetNMAsString, output);
end;

function TNumberModulePool.ManualMacro(const text_, HeadToken, TailToken, OwnerFlag: SystemString; OnDM2Text: TGetDMAsString; out output: SystemString): Boolean;
var
  lst: TCoreClassListForObj;

  function _GetDM(k: SystemString): TNumberModule;
  var
    i: Integer;
  begin
    for i := 0 to lst.Count - 1 do
      begin
        Result := TNumberModule(lst[i]);
        if (SameText(Result.FSymbolName, k)) or (SameText(Result.FName, k)) then
            exit;
      end;
    Result := nil;
  end;

var
  sour: U_String;
  hf, TF, owf: U_String;
  bPos, ePos, OwnerPos, nPos: Integer;
  KeyText, OwnerKey, SubKey: U_String;
  i: Integer;
  NM: TNumberModule;
begin
  lst := TCoreClassListForObj.Create;
  FList.GetAsList(lst);
  output := '';
  sour.Text := text_;
  hf.Text := HeadToken;
  TF.Text := TailToken;
  owf.Text := OwnerFlag;
  Result := True;

  i := 1;

  while i <= sour.Len do
    begin
      if sour.ComparePos(i, @hf) then
        begin
          bPos := i;
          ePos := sour.GetPos(TF, i + hf.Len);
          if ePos > 0 then
            begin
              KeyText := sour.Copy(bPos + hf.Len, ePos - (bPos + hf.Len));
              OwnerPos := KeyText.GetPos(owf);
              if OwnerPos > 0 then
                begin
                  OwnerKey := KeyText.Copy(1, OwnerPos - 1);
                  nPos := OwnerPos + owf.Len;
                  SubKey := KeyText.Copy(nPos, KeyText.Len - nPos + 1);

                  NM := _GetDM(OwnerKey.Text);
                  if NM <> nil then
                    begin
                      output := output + OnDM2Text(SubKey.Text, NM);
                      i := ePos + TF.Len;
                      Continue;
                    end
                  else
                    begin
                      Result := False;
                    end;
                end
              else
                begin
                  NM := _GetDM(KeyText.Text);
                  if NM <> nil then
                    begin
                      output := output + OnDM2Text('', NM);
                      i := ePos + TF.Len;
                      Continue;
                    end
                  else
                    begin
                      Result := False;
                    end;
                end;
            end;
        end;

      output := output + sour[i];
      inc(i);
    end;
  DisposeObject(lst);
end;

procedure TNumberModulePool.Assign(source: TNumberModulePool);
var
  lst: TCoreClassListForObj;
  i: Integer;
  NewDM, NM: TNumberModule;
begin
  lst := TCoreClassListForObj.Create;
  source.FList.GetAsList(lst);
  for i := 0 to lst.Count - 1 do
    begin
      NM := lst[i] as TNumberModule;
      NewDM := Items[NM.Name];
      NewDM.Assign(NM);
    end;

  for i := 0 to lst.Count - 1 do
    begin
      NM := lst[i] as TNumberModule;
      NM.DoChange;
    end;
  DisposeObject(lst);
end;

procedure TNumberModulePool.DoChangeAll;
begin
  FList.ProgressM({$IFDEF FPC}@{$ENDIF FPC}DoChangeAll_Progress);
end;

procedure TNumberModulePool.LoadFromStream(stream: TCoreClassStream);
var
  df: TDFE;
  NM: TNumberModule;
  n: SystemString;
  lst: TCoreClassListForObj;
  i: Integer;
begin
  // format
  // name,current value,origin value
  lst := TCoreClassListForObj.Create;
  df := TDFE.Create;
  df.DecodeFrom(stream);
  while not df.Reader.IsEnd do
    begin
      n := df.Reader.ReadString;
      NM := GetItems(n);
      NM.SymbolName := df.Reader.ReadString;
      NM.Description := df.Reader.ReadString;
      NM.DetailDescription := df.Reader.ReadString;
      NM.DirectOriginValue := df.Reader.ReadVariant;
      NM.DirectCurrentValue := df.Reader.ReadVariant;
      lst.Add(NM);
    end;
  DisposeObject(df);
  for i := 0 to lst.Count - 1 do
    begin
      NM := TNumberModule(lst[i]);
      NM.DoChange;
    end;
  DisposeObject(lst);
end;

procedure TNumberModulePool.SaveToStream(stream: TCoreClassStream);
var
  df: TDFE;
  lst: TCoreClassListForObj;
  i: Integer;
  NM: TNumberModule;
begin
  // format
  // name,current value,origin value
  lst := TCoreClassListForObj.Create;
  FList.GetAsList(lst);
  df := TDFE.Create;
  for i := 0 to lst.Count - 1 do
    begin
      NM := TNumberModule(lst[i]);
      df.WriteString(NM.Name);
      df.WriteString(NM.SymbolName);
      df.WriteString(NM.Description);
      df.WriteString(NM.DetailDescription);
      df.WriteVariant(NM.OriginValue);
      df.WriteVariant(NM.CurrentValue);
    end;
  df.FastEncodeTo(stream);
  DisposeObject(df);
  DisposeObject(lst);
end;

procedure TNumberModulePool.LoadFromVariantList(L: THashVariantList);
var
  NL: TPascalStringList;
  i: Integer;
  lst: TCoreClassListForObj;
  NM: TNumberModule;
begin
  lst := TCoreClassListForObj.Create;
  NL := TPascalStringList.Create;
  L.GetNameList(NL);

  for i := 0 to NL.Count - 1 do
    begin
      NM := GetItems(NL[i]);
      NM.DirectOriginValue := L[NM.Name];
      NM.DirectCurrentValue := NM.DirectOriginValue;
      lst.Add(NM);
    end;
  DisposeObject(NL);

  for i := 0 to lst.Count - 1 do
    begin
      NM := TNumberModule(lst[i]);
      NM.DoChange;
    end;
  DisposeObject(lst);
end;

procedure TNumberModulePool.SaveToVariantList(L: THashVariantList);
var
  lst: TCoreClassStringList;
  i: Integer;
  NM: TNumberModule;
begin
  lst := TCoreClassStringList.Create;
  FList.GetListData(lst);
  for i := 0 to lst.Count - 1 do
    begin
      NM := TNumberModule(lst.Objects[i]);
      L[NM.Name] := NM.OriginValue;
    end;
  DisposeObject(lst);
end;

function TNumberModulePool.GetItems(Name_: SystemString): TNumberModule;
begin
  Result := FList[Name_];
  if Result = nil then
    begin
      Result := TNumberModule.Create(Self);
      FList[Name_] := Result;
      Result.FName := Name_;
      Result.DoRegOpProc;
    end;
end;

class procedure TNumberModulePool.test;
var
  nmPool: TNumberModulePool;
begin
  nmPool := TNumberModulePool.Create;
  nmPool['a'].OriginValue := 33.14;
  nmPool['b'].OriginValue := 100;
  nmPool['c'].OriginValue := 200;

  DoStatus('NM test: %s', [VarToStr(nmPool.RunScript('a(a*100)*b+c', tsPascal))]);
  DoStatus('NM test: %s', [VarToStr(nmPool.RunScript('a(33.14)', tsPascal))]);
  DoStatus('NM vector test: %s', [ExpressionValueVectorToStr(nmPool.RunVectorScript('a(a*100)*b+c, a*c+99', tsPascal)).Text]);
  DisposeObject(nmPool);
end;

procedure test;
var
  NL: TNumberModulePool;
  n: SystemString;
begin
  NL := TNumberModulePool.Create;
  NL['a'].OriginValue := '123';
  NL['a'].Description := 'hahahaha';
  NL.Macro('hello <a.Description> world', '<', '>', '.', n);
  DisposeObject(NL);
  DoStatus(n);
end;

end.
