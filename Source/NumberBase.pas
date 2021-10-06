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
  CoreClasses, GHashList, ListEngine, PascalStrings,
  TextParsing, zExpression, OpCode;

type
  TNumberModuleHookPool = class;
  TNumberModuleEventPool = class;
  TNumberModulePool = class;
  TNumberModule = class;
  TNumberModuleHookPoolList = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TNumberModuleHookPool>;
  TNumberModuleEventPoolList = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TNumberModuleEventPool>;

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
    FName: SystemString;
    FCurrentValueHookPool: TNumberModuleHookPoolList;
    FCurrentValueChangeAfterEventPool: TNumberModuleEventPoolList;
    FCurrentValue: Variant;
    FOriginValue: Variant;
    FEnabledHook: Boolean;
    FEnabledEvent: Boolean;
    FOnChange: TNumberModuleChangeEvent;
  private
    procedure SetName(const Value: SystemString);
    function GetCurrentValue: Variant;
    procedure SetCurrentValue(const Value: Variant);
    function GetOriginValue: Variant;
    procedure SetOriginValue(const Value: Variant);
    procedure DoCurrentValueHook(const OLD_, New_: Variant);
    procedure Clear;
    procedure DoRegOpProc();
    function OP_DoProc(Sender: TOpCustomRunTime; var OP_Param: TOpParam): Variant;
  private
    function GetCurrentAsDouble: Double;
    procedure SetCurrentAsDouble(const Value: Double);
    function GetCurrentAsSingle: Single;
    procedure SetCurrentAsSingle(const Value: Single);
    function GetCurrentAsInt64: Int64;
    procedure SetCurrentAsInt64(const Value: Int64);
    function GetCurrentAsInteger: Integer;
    procedure SetCurrentAsInteger(const Value: Integer);
    function GetCurrentAsCardinal: Cardinal;
    procedure SetCurrentAsCardinal(const Value: Cardinal);
    procedure SetCurrentAsString(const Value: SystemString);
    function GetCurrentAsString: SystemString;
    function GetCurrentAsBool: Boolean;
    procedure SetCurrentAsBool(const Value: Boolean);
    function GetOriginAsDouble: Double;
    procedure SetOriginAsDouble(const Value: Double);
    function GetOriginAsSingle: Single;
    procedure SetOriginAsSingle(const Value: Single);
    function GetOriginAsInt64: Int64;
    procedure SetOriginAsInt64(const Value: Int64);
    function GetOriginAsInteger: Integer;
    procedure SetOriginAsInteger(const Value: Integer);
    function GetOriginAsCardinal: Cardinal;
    procedure SetOriginAsCardinal(const Value: Cardinal);
    function GetOriginAsString: SystemString;
    procedure SetOriginAsString(const Value: SystemString);
    function GetOriginAsBool: Boolean;
    procedure SetOriginAsBool(const Value: Boolean);
  public
    constructor Create(Owner_: TNumberModulePool);
    destructor Destroy; override;
    property OnChange: TNumberModuleChangeEvent read FOnChange write FOnChange;
    property EnabledHook: Boolean read FEnabledHook write FEnabledHook;
    property EnabledEvent: Boolean read FEnabledEvent write FEnabledEvent;
    property Owner: TNumberModulePool read FOwner;
    property Name: SystemString read FName write SetName;
    procedure DoChange;
    function RegisterCurrentValueHook: TNumberModuleHookPool;
    procedure CopyHookInterfaceFrom(sour: TNumberModule);
    function RegisterCurrentValueChangeAfterEvent: TNumberModuleEventPool;
    procedure CopyChangeAfterEventInterfaceFrom(sour: TNumberModule);
    procedure Assign(sour: TNumberModule);
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
    property OriginValue: Variant read GetOriginValue write SetOriginValue;
    property OriginAsSingle: Single read GetOriginAsSingle write SetOriginAsSingle;
    property OriginAsDouble: Double read GetOriginAsDouble write SetOriginAsDouble;
    property OriginAsInteger: Integer read GetOriginAsInteger write SetOriginAsInteger;
    property OriginAsInt64: Int64 read GetOriginAsInt64 write SetOriginAsInt64;
    property OriginAsCardinal: Cardinal read GetOriginAsCardinal write SetOriginAsCardinal;
    property OriginAsString: SystemString read GetOriginAsString write SetOriginAsString;
    property OriginAsBool: Boolean read GetOriginAsBool write SetOriginAsBool;
    property DirectValue: Variant read FCurrentValue write FCurrentValue;
    property DirectCurrentValue: Variant read FCurrentValue write FCurrentValue;
    property DirectOriginValue: Variant read FOriginValue write FOriginValue;
  end;

  TNumberModulePool_Decl = {$IFDEF FPC}specialize {$ENDIF FPC}TGenericHashList<TNumberModule>;
  TOnNMChange = procedure(Sender: TNumberModulePool; NM_: TNumberModule; OLD_, New_: Variant) of object;
  TOnNMCreateOpRunTime = procedure(Sender: TNumberModulePool; OP_: TOpCustomRunTime) of object;

  TNumberModulePool = class(TCoreClassObject)
  protected
    FList: TNumberModulePool_Decl;
    FIsChanged: Boolean;
    FExpOpRunTime: TOpCustomRunTime;
    function OP_DoNewNM(Sender: TOpCustomRunTime; var OP_Param: TOpParam): Variant;
    function OP_DoGetNM(Sender: TOpCustomRunTime; var OP_Param: TOpParam): Variant;
    procedure DoSwapInstance_Progress(const Name: PSystemString; Obj: TNumberModule);
    procedure DoRebuildOpRunTime_Progress(const Name: PSystemString; Obj: TNumberModule);
    procedure DoChangeAll_Progress(const Name: PSystemString; Obj: TNumberModule);
    function GetExpOpRunTime: TOpCustomRunTime;
  public
    OnNMChange: TOnNMChange;
    OnNMCreateOpRunTime: TOnNMCreateOpRunTime;

    constructor Create; virtual;
    destructor Destroy; override;
    procedure SwapInstance(source: TNumberModulePool);
    procedure DoNMChange(Sender: TNumberModule; OLD_, New_: Variant); virtual;
    procedure Delete(Name_: SystemString); virtual;
    function Exists(Name_: SystemString): Boolean; virtual;
    function ExistsIntf(NM_: TNumberModule): Boolean; virtual;
    procedure Clear; virtual;
    procedure Assign(source: TNumberModulePool); virtual;
    procedure DoChangeAll; virtual;
    procedure LoadFromStream(stream: TCoreClassStream); virtual;
    procedure SaveToStream(stream: TCoreClassStream); virtual;
    function GetItems(Name_: SystemString): TNumberModule; virtual;
    property Items[Name_: SystemString]: TNumberModule read GetItems; default;
    property List: TNumberModulePool_Decl read FList;
    property IsChanged: Boolean read FIsChanged write FIsChanged;
    // script
    procedure RebuildOpRunTime;
    property ExpOpRunTime: TOpCustomRunTime read GetExpOpRunTime;
    function IsVectorScript(ExpressionText_: SystemString; TS_: TTextStyle): Boolean; overload;
    function IsVectorScript(ExpressionText_: SystemString): Boolean; overload;
    function RunScript(ExpressionText_: SystemString; TS_: TTextStyle): Variant; overload;
    function RunScript(ExpressionText_: SystemString): Variant; overload;
    function RunVectorScript(ExpressionText_: SystemString; TS_: TTextStyle): TExpressionValueVector; overload;
    function RunVectorScript(ExpressionText_: SystemString): TExpressionValueVector; overload;

    class procedure test;
  end;

implementation

uses Variants, UnicodeMixedLib, DataFrameEngine, NotifyObjectBase, DoStatusIO;

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
    begin
      try
          Owner.DoNMChange(Self, OLD_, N_);
      except
      end;
    end;
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
      Owner.ExpOpRunTime.RegObjectOpM(Name, '', {$IFDEF FPC}@{$ENDIF FPC}OP_DoProc);
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

function TNumberModule.GetCurrentAsDouble: Double;
begin
  Result := CurrentValue;
end;

procedure TNumberModule.SetCurrentAsDouble(const Value: Double);
begin
  CurrentValue := Value;
end;

function TNumberModule.GetCurrentAsSingle: Single;
begin
  Result := CurrentValue;
end;

procedure TNumberModule.SetCurrentAsSingle(const Value: Single);
begin
  CurrentValue := Value;
end;

function TNumberModule.GetCurrentAsInt64: Int64;
begin
  Result := CurrentValue;
end;

procedure TNumberModule.SetCurrentAsInt64(const Value: Int64);
begin
  CurrentValue := Value;
end;

function TNumberModule.GetCurrentAsInteger: Integer;
begin
  Result := CurrentValue;
end;

procedure TNumberModule.SetCurrentAsInteger(const Value: Integer);
begin
  CurrentValue := Value;
end;

function TNumberModule.GetCurrentAsCardinal: Cardinal;
begin
  Result := CurrentValue;
end;

procedure TNumberModule.SetCurrentAsCardinal(const Value: Cardinal);
begin
  CurrentValue := Value;
end;

procedure TNumberModule.SetCurrentAsString(const Value: SystemString);
begin
  CurrentValue := Value;
end;

function TNumberModule.GetCurrentAsString: SystemString;
begin
  Result := VarToStr(CurrentValue);
end;

function TNumberModule.GetCurrentAsBool: Boolean;
begin
  Result := CurrentValue;
end;

procedure TNumberModule.SetCurrentAsBool(const Value: Boolean);
begin
  CurrentValue := Value;
end;

function TNumberModule.GetOriginAsDouble: Double;
begin
  Result := OriginValue;
end;

procedure TNumberModule.SetOriginAsDouble(const Value: Double);
begin
  OriginValue := Value;
end;

function TNumberModule.GetOriginAsSingle: Single;
begin
  Result := OriginValue;
end;

procedure TNumberModule.SetOriginAsSingle(const Value: Single);
begin
  OriginValue := Value;
end;

function TNumberModule.GetOriginAsInt64: Int64;
begin
  Result := OriginValue;
end;

procedure TNumberModule.SetOriginAsInt64(const Value: Int64);
begin
  OriginValue := Value;
end;

function TNumberModule.GetOriginAsInteger: Integer;
begin
  Result := OriginValue;
end;

procedure TNumberModule.SetOriginAsInteger(const Value: Integer);
begin
  OriginValue := Value;
end;

function TNumberModule.GetOriginAsCardinal: Cardinal;
begin
  Result := OriginValue;
end;

procedure TNumberModule.SetOriginAsCardinal(const Value: Cardinal);
begin
  OriginValue := Value;
end;

function TNumberModule.GetOriginAsString: SystemString;
begin
  Result := VarToStr(OriginValue);
end;

procedure TNumberModule.SetOriginAsString(const Value: SystemString);
begin
  OriginValue := Value;
end;

function TNumberModule.GetOriginAsBool: Boolean;
begin
  Result := OriginAsInteger > 0;
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
  FCurrentValueHookPool := TNumberModuleHookPoolList.Create;
  FCurrentValueChangeAfterEventPool := TNumberModuleEventPoolList.Create;
  FCurrentValue := NULL;
  FOriginValue := NULL;
  FEnabledHook := True;
  FEnabledEvent := True;
  FOnChange := nil;
end;

destructor TNumberModule.Destroy;
begin
  if (Name <> '') and (Owner <> nil) and (Owner.FExpOpRunTime <> nil) then
      Owner.FExpOpRunTime.ProcList.Delete(Name);
  Clear;
  DisposeObject(FCurrentValueChangeAfterEventPool);
  DisposeObject(FCurrentValueHookPool);
  inherited Destroy;
end;

procedure TNumberModule.DoChange;
begin
  DoCurrentValueHook(FCurrentValue, FCurrentValue);
  if Owner <> nil then
      Owner.FIsChanged := True;
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

function TNumberModulePool.OP_DoGetNM(Sender: TOpCustomRunTime; var OP_Param: TOpParam): Variant;
var
  N_: SystemString;
begin
  N_ := VarToStr(OP_Param[0]);
  if FList.Exists(N_) then
      Result := Items[N_].AsValue
  else if Length(OP_Param) = 2 then
      Result := OP_Param[1]
  else
      Result := 0;
end;

procedure TNumberModulePool.DoSwapInstance_Progress(const Name: PSystemString; Obj: TNumberModule);
begin
  Obj.FOwner := Self;
end;

procedure TNumberModulePool.DoRebuildOpRunTime_Progress(const Name: PSystemString; Obj: TNumberModule);
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
      FExpOpRunTime := TOpCustomRunTime.CustomCreate(1024);
      FExpOpRunTime.RegObjectOpM('Set', '', {$IFDEF FPC}@{$ENDIF FPC}OP_DoNewNM);
      FExpOpRunTime.RegObjectOpM('Get', '', {$IFDEF FPC}@{$ENDIF FPC}OP_DoGetNM);
      FList.ProgressM({$IFDEF FPC}@{$ENDIF FPC}DoRebuildOpRunTime_Progress);
      if Assigned(OnNMCreateOpRunTime) then
          OnNMCreateOpRunTime(Self, FExpOpRunTime);
    end;
  Result := FExpOpRunTime;
end;

constructor TNumberModulePool.Create;
begin
  inherited Create;
  FList := TNumberModulePool_Decl.Create(True, 1024, nil);
  FIsChanged := False;
  FExpOpRunTime := nil;
  OnNMChange := nil;
  OnNMCreateOpRunTime := nil;
end;

destructor TNumberModulePool.Destroy;
begin
  DisposeObjectAndNil(FList);
  DisposeObjectAndNil(FExpOpRunTime);
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
  FList.ProgressM({$IFDEF FPC}@{$ENDIF FPC}DoSwapInstance_Progress);
  source.FList.ProgressM({$IFDEF FPC}@{$ENDIF FPC}source.DoSwapInstance_Progress);
end;

procedure TNumberModulePool.DoNMChange(Sender: TNumberModule; OLD_, New_: Variant);
begin
  try
    if Assigned(OnNMChange) then
        OnNMChange(Self, Sender, OLD_, New_);
  except
  end;
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
  D: TDFE;
  NM: TNumberModule;
  n: SystemString;
  lst: TCoreClassListForObj;
  i: Integer;
begin
  // format
  // name,current value,origin value
  lst := TCoreClassListForObj.Create;
  D := TDFE.Create;
  D.DecodeFrom(stream);
  while not D.Reader.IsEnd do
    begin
      n := D.Reader.ReadString;
      NM := GetItems(n);
      NM.DirectOriginValue := D.Reader.ReadVariant;
      NM.DirectCurrentValue := D.Reader.ReadVariant;
      lst.Add(NM);
    end;
  DisposeObject(D);
  for i := 0 to lst.Count - 1 do
    begin
      NM := TNumberModule(lst[i]);
      NM.DoChange;
    end;
  DisposeObject(lst);
end;

procedure TNumberModulePool.SaveToStream(stream: TCoreClassStream);
var
  D: TDFE;
  lst: TCoreClassListForObj;
  i: Integer;
  NM: TNumberModule;
begin
  // format
  // name,current value,origin value
  lst := TCoreClassListForObj.Create;
  FList.GetAsList(lst);
  D := TDFE.Create;
  for i := 0 to lst.Count - 1 do
    begin
      NM := TNumberModule(lst[i]);
      D.WriteString(NM.Name);
      D.WriteVariant(NM.OriginValue);
      D.WriteVariant(NM.CurrentValue);
    end;
  D.FastEncodeTo(stream);
  DisposeObject(D);
  DisposeObject(lst);
end;

function TNumberModulePool.GetItems(Name_: SystemString): TNumberModule;
begin
  Result := FList[Name_];
  if Result = nil then
    begin
      Result := TNumberModule.Create(Self);
      FList.FastAdd(Name_, Result);
      Result.FName := Name_;
      if FExpOpRunTime <> nil then
          Result.DoRegOpProc;
      FIsChanged := True;
    end;
end;

procedure TNumberModulePool.RebuildOpRunTime;
begin
  DisposeObjectAndNil(FExpOpRunTime);
end;

function TNumberModulePool.IsVectorScript(ExpressionText_: SystemString; TS_: TTextStyle): Boolean;
begin
  try
      Result := IsSymbolVectorExpression(ExpressionText_, TS_, nil);
  except
      Result := False;
  end;
end;

function TNumberModulePool.IsVectorScript(ExpressionText_: SystemString): Boolean;
begin
  Result := IsVectorScript(ExpressionText_, tsPascal);
end;

function TNumberModulePool.RunScript(ExpressionText_: SystemString; TS_: TTextStyle): Variant;
begin
  try
      Result := EvaluateExpressionValue(True, TS_, ExpressionText_, ExpOpRunTime);
  except
      Result := NULL;
  end;
end;

function TNumberModulePool.RunScript(ExpressionText_: SystemString): Variant;
begin
  Result := RunScript(ExpressionText_, tsPascal);
end;

function TNumberModulePool.RunVectorScript(ExpressionText_: SystemString; TS_: TTextStyle): TExpressionValueVector;
begin
  try
      Result := EvaluateExpressionVector(False, True, nil, TS_, ExpressionText_, ExpOpRunTime, nil);
  except
      SetLength(Result, 0);
  end;
end;

function TNumberModulePool.RunVectorScript(ExpressionText_: SystemString): TExpressionValueVector;
begin
  Result := RunVectorScript(ExpressionText_, tsPascal);
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

end.
