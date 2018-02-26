{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ ****************************************************************************** }
unit NumberBase;

{$I zDefine.inc}

interface

uses ListEngine, CoreClasses, DataFrameEngine, PascalStrings;

type
  TNumberModuleHookInterface  = class;
  TNumberModuleEventInterface = class;
  TNumberModuleList           = class;
  TNumberModule               = class;

  TNumberModuleHook = procedure(Sender: TNumberModuleHookInterface; OldValue: Variant; var NewValue: Variant) of object;

  TNumberModuleHookInterface = class(TCoreClassObject)
  private
    FOwner          : TNumberModule;
    FOwnerList      : TCoreClassListForObj;
    FOnCurrentDMHook: TNumberModuleHook;
  protected
  public
    constructor Create(AOwner: TNumberModule; AOwnerList: TCoreClassListForObj);
    destructor Destroy; override;

    property Owner: TNumberModule read FOwner;
    property OnCurrentDMHook: TNumberModuleHook read FOnCurrentDMHook write FOnCurrentDMHook;
  end;

  TNumberModuleEvent = procedure(Sender: TNumberModuleEventInterface; NewValue: Variant) of object;

  TNumberModuleEventInterface = class(TCoreClassObject)
  private
    FOwner           : TNumberModule;
    FOwnerList       : TCoreClassListForObj;
    FOnCurrentDMEvent: TNumberModuleEvent;
  protected
  public
    constructor Create(AOwner: TNumberModule; AOwnerList: TCoreClassListForObj);
    destructor Destroy; override;

    property Owner: TNumberModule read FOwner;
    property OnCurrentDMEvent: TNumberModuleEvent read FOnCurrentDMEvent write FOnCurrentDMEvent;
  end;

  TNumberModuleNotifyEvent = procedure();
  TNumberModuleChangeEvent = procedure(OldValue, NewValue: Variant);

  TNumberModule = class(TCoreClassObject)
  private
    FOwner                                              : TNumberModuleList;
    FName, FSymbolName, FDescription, FDetailDescription: SystemString;

    FCurrentValueHookList            : TCoreClassListForObj;
    FCurrentValueChangeAfterEventList: TCoreClassListForObj;

    FCurrentValue: Variant;
    FOriginValue : Variant;

    FCustomObjects: THashObjectList;
    FCustomValues : THashVariantList;

    FEnabledHook : Boolean;
    FEnabledEvent: Boolean;

    FOnChange: TNumberModuleChangeEvent;
  private
    procedure SetName(const Value: SystemString);

    function GetCurrentValue: Variant;
    procedure SetCurrentValue(const Value: Variant);

    function GetOriginValue: Variant;
    procedure SetOriginValue(const Value: Variant);

    function GetCustomObjects: THashObjectList;
    function GetCustomValues: THashVariantList;
  private
    procedure DoCurrentValueHook(OldValue: Variant; NewValue: Variant);
    procedure Clear;
  private
    function GetCurrentAsCardinal: Cardinal;
    function GetCurrentAsDouble: Double;
    function GetCurrentAsInt64: Int64;
    function GetCurrentAsInteger: Integer;
    function GetCurrentAsSingle: Single;
    function GetCurrentAsString: SystemString;
    function GetCurrentAsBool: Boolean;

    procedure SetCurrentAsCardinal(const Value: Cardinal);
    procedure SetCurrentAsDouble(const Value: Double);
    procedure SetCurrentAsInt64(const Value: Int64);
    procedure SetCurrentAsInteger(const Value: Integer);
    procedure SetCurrentAsSingle(const Value: Single);
    procedure SetCurrentAsString(const Value: SystemString);
    procedure SetCurrentAsBool(const Value: Boolean);

    function GetOriginAsCardinal: Cardinal;
    function GetOriginAsDouble: Double;
    function GetOriginAsInt64: Int64;
    function GetOriginAsInteger: Integer;
    function GetOriginAsSingle: Single;
    function GetOriginAsString: SystemString;
    function GetOriginAsBool: Boolean;

    procedure SetOriginAsCardinal(const Value: Cardinal);
    procedure SetOriginAsDouble(const Value: Double);
    procedure SetOriginAsInt64(const Value: Int64);
    procedure SetOriginAsInteger(const Value: Integer);
    procedure SetOriginAsSingle(const Value: Single);
    procedure SetOriginAsString(const Value: SystemString);
    procedure SetOriginAsBool(const Value: Boolean);
  public
    constructor Create(AOwner: TNumberModuleList);
    destructor Destroy; override;

    // value changed
    procedure UpdateValue; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    // reg hook interface
    function RegisterCurrentValueHook: TNumberModuleHookInterface; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure CopyHookInterfaceFrom(Sour: TNumberModule); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    // reg change after event
    function RegisterCurrentValueChangeAfterEvent: TNumberModuleEventInterface; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure CopyChangeAfterEventInterfaceFrom(Sour: TNumberModule); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    // copy
    procedure Assign(Sour: TNumberModule); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    // use hook change
    property CurrentValue: Variant read GetCurrentValue write SetCurrentValue;
    property OriginValue: Variant read GetOriginValue write SetOriginValue;

    property CurrentAsSingle: Single read GetCurrentAsSingle write SetCurrentAsSingle;
    property CurrentAsDouble: Double read GetCurrentAsDouble write SetCurrentAsDouble;
    property CurrentAsInteger: Integer read GetCurrentAsInteger write SetCurrentAsInteger;
    property CurrentAsInt64: Int64 read GetCurrentAsInt64 write SetCurrentAsInt64;
    property CurrentAsCardinal: Cardinal read GetCurrentAsCardinal write SetCurrentAsCardinal;
    property CurrentAsString: SystemString read GetCurrentAsString write SetCurrentAsString;
    property CurrentAsBool: Boolean read GetCurrentAsBool write SetCurrentAsBool;

    property AsValue: Variant read GetCurrentValue write SetCurrentValue;
    property AsSingle: Single read GetCurrentAsSingle write SetCurrentAsSingle;
    property AsDouble: Double read GetCurrentAsDouble write SetCurrentAsDouble;
    property AsInteger: Integer read GetCurrentAsInteger write SetCurrentAsInteger;
    property AsInt64: Int64 read GetCurrentAsInt64 write SetCurrentAsInt64;
    property AsCardinal: Cardinal read GetCurrentAsCardinal write SetCurrentAsCardinal;
    property AsString: SystemString read GetCurrentAsString write SetCurrentAsString;
    property AsBool: Boolean read GetCurrentAsBool write SetCurrentAsBool;

    property OriginAsSingle: Single read GetOriginAsSingle write SetOriginAsSingle;
    property OriginAsDouble: Double read GetOriginAsDouble write SetOriginAsDouble;
    property OriginAsInteger: Integer read GetOriginAsInteger write SetOriginAsInteger;
    property OriginAsInt64: Int64 read GetOriginAsInt64 write SetOriginAsInt64;
    property OriginAsCardinal: Cardinal read GetOriginAsCardinal write SetOriginAsCardinal;
    property OriginAsString: SystemString read GetOriginAsString write SetOriginAsString;
    property OriginAsBool: Boolean read GetOriginAsBool write SetOriginAsBool;

    // skip hook
    property DirectCurrentValue: Variant read FCurrentValue write FCurrentValue;
    property DirectValue: Variant read FCurrentValue write FCurrentValue;
    property DirectOriginValue: Variant read FOriginValue write FOriginValue;
    // custom object
    property CustomObjects: THashObjectList read GetCustomObjects;
    // custom value
    property CustomValues: THashVariantList read GetCustomValues;

    property EnabledHook: Boolean read FEnabledHook write FEnabledHook;
    property EnabledEvent: Boolean read FEnabledEvent write FEnabledEvent;

    property Owner: TNumberModuleList read FOwner;
    property name: SystemString read FName write SetName;
    property SymbolName: SystemString read FSymbolName write FSymbolName;
    property Description: SystemString read FDescription write FDescription;
    property DetailDescription: SystemString read FDetailDescription write FDetailDescription;
  end;

  TGetDMAsString = function(key: SystemString; DM: TNumberModule): SystemString;

  TNumberModuleList = class(TCoreClassObject)
  protected
  private
    FList: THashObjectList;
    function GetItems(AName: SystemString): TNumberModule;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Delete(AName: SystemString); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Exists(AName: SystemString): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ExistsIntf(ADM: TNumberModule): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure Clear; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    //
    function Macro(const AText, HeadToken, TailToken, OwnerFlag: SystemString; out Output: SystemString): Boolean;
    function ManualMacro(const AText, HeadToken, TailToken, OwnerFlag: SystemString; OnDM2Text: TGetDMAsString; out Output: SystemString): Boolean;
    //
    procedure Assign(Source: TNumberModuleList); // create by 2011-6-17

    // can merge current
    procedure LoadFromStream(Stream: TCoreClassStream);
    // save
    procedure SaveToStream(Stream: TCoreClassStream);

    // laod form text, auto merge current
    procedure LoadFromVariantList(v: THashVariantList);
    // save as text
    procedure SaveToVariantList(v: THashVariantList);

    property Items[AName: SystemString]: TNumberModule read GetItems; default;
    property List: THashObjectList read FList;
  end;

  TNMAutomatedManager = class;

  TNumberProcessStyle = (npsInc, npsDec, npsIncMul, npsDecMul);

  TNumberProcessingData = packed record
    Flag: TCoreClassObject;
    SeedNumber: Variant;
    Style: TNumberProcessStyle;
    CancelDelayTime: Double;
    Overlap: Boolean;
    TypeID: Integer;
    Priority: Cardinal;
    Processed: Boolean;
  end;

  PDMProcessingData = ^TNumberProcessingData;

  TNMAutomated = class(TCoreClassPersistent)
  private
    FOwner               : TNMAutomatedManager;
    FDMSource            : TNumberModule;
    FCurrentValueHookIntf: TNumberModuleHookInterface;
    FCurrentValueCalcList: TCoreClassList;
  protected
    procedure DMCurrentValueHook(Sender: TNumberModuleHookInterface; OldValue: Variant; var NewValue: Variant);
  private
    function GetCurrentValueCalcData(Flag: TCoreClassObject): PDMProcessingData; {$IFDEF INLINE_ASM} inline; {$ENDIF}
  public
    constructor Create(AOwner: TNMAutomatedManager; ADMSource: TNumberModule);
    destructor Destroy; override;

    procedure Progress(deltaTime: Double); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure ChangeProcessStyle(Flag: TCoreClassObject; SeedNumber: Variant; Style: TNumberProcessStyle; CancelDelayTime: Double; Overlap: Boolean; TypeID: Integer; Priority: Cardinal); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure Cancel(Flag: TCoreClassObject); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure Clear; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    //
    property Owner: TNMAutomatedManager read FOwner write FOwner;
  end;

  TNMAutomatedManager = class(TCoreClassPersistent)
  private
    FList: TCoreClassListForObj;

    function GetOrCreate(ADMSource: TNumberModule): TNMAutomated;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Progress(deltaTime: Double); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure Clear;

    procedure PostAutomatedProcess(Style: TNumberProcessStyle;
      ADMSource: TNumberModule; Flag: TCoreClassPersistent;
      SeedNumber: Variant; Overlap: Boolean; TypeID: Integer; Priority: Cardinal); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure PostAutomatedDelayCancelProcess(Style: TNumberProcessStyle;
      ADMSource: TNumberModule; Flag: TCoreClassPersistent;
      SeedNumber: Variant; Overlap: Boolean; TypeID: Integer; Priority: Cardinal; CancelDelayTime: Double); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    //
    procedure Delete(ADMSource: TNumberModule; Flag: TCoreClassObject); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure Delete(Flag: TCoreClassObject); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
  end;

implementation

uses SysUtils, Variants, UnicodeMixedLib, DoStatusIO;

function __GetDMAsString(key: SystemString; DM: TNumberModule): SystemString;
begin
  if key = '' then
      Result := DM.CurrentAsString
  else if SameText(key, 'Value') then
      Result := DM.CurrentAsString
  else if SameText(key, 'Origin') then
      Result := DM.OriginAsString
  else if SameText(key, 'Name') then
      Result := DM.name
  else if SameText(key, 'Symbol') then
      Result := DM.SymbolName
  else if SameText(key, 'Description') then
      Result := DM.Description
  else if SameText(key, 'Detail') then
      Result := DM.DetailDescription
  else
      Result := DM.CurrentAsString;
end;

constructor TNumberModuleHookInterface.Create(AOwner: TNumberModule; AOwnerList: TCoreClassListForObj);
begin
  inherited Create;
  FOwner := AOwner;
  FOwnerList := AOwnerList;
  FOnCurrentDMHook := nil;
  if FOwnerList <> nil then
      FOwnerList.Add(Self);
end;

destructor TNumberModuleHookInterface.Destroy;
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

constructor TNumberModuleEventInterface.Create(AOwner: TNumberModule; AOwnerList: TCoreClassListForObj);
begin
  inherited Create;
  FOwner := AOwner;
  FOwnerList := AOwnerList;
  FOnCurrentDMEvent := nil;
  if FOwnerList <> nil then
      FOwnerList.Add(Self);
end;

destructor TNumberModuleEventInterface.Destroy;
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

function TNumberModule.GetCustomObjects: THashObjectList;
begin
  if FCustomObjects = nil then
      FCustomObjects := THashObjectList.Create(False);
  Result := FCustomObjects;
end;

function TNumberModule.GetCustomValues: THashVariantList;
begin
  if FCustomValues = nil then
      FCustomValues := THashVariantList.Create;
  Result := FCustomValues;
end;

procedure TNumberModule.DoCurrentValueHook(OldValue: Variant; NewValue: Variant);
var
  i : Integer;
  _H: TNumberModuleHookInterface;
  _E: TNumberModuleEventInterface;

  _New: Variant;
begin
  _New := NewValue;
  if (FEnabledHook) then
    for i := 0 to FCurrentValueHookList.Count - 1 do
      begin
        _H := TNumberModuleHookInterface(FCurrentValueHookList[i]);
        if Assigned(_H.FOnCurrentDMHook) then
          begin
            try
                _H.FOnCurrentDMHook(_H, FCurrentValue, _New);
            except
            end;
          end;
      end;
  FCurrentValue := _New;
  // trigger change after event
  if (FEnabledEvent) then
    for i := 0 to FCurrentValueChangeAfterEventList.Count - 1 do
      begin
        _E := TNumberModuleEventInterface(FCurrentValueChangeAfterEventList[i]);
        if Assigned(_E.FOnCurrentDMEvent) then
          begin
            try
                _E.FOnCurrentDMEvent(_E, FCurrentValue);
            except
            end;
          end;
      end;

  if Assigned(FOnChange) then
      FOnChange(OldValue, NewValue);
end;

procedure TNumberModule.Clear;
begin
  while FCurrentValueHookList.Count > 0 do
      DisposeObject(TNumberModuleHookInterface(FCurrentValueHookList[0]));
  while FCurrentValueChangeAfterEventList.Count > 0 do
      DisposeObject(TNumberModuleEventInterface(FCurrentValueChangeAfterEventList[0]));
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

constructor TNumberModule.Create(AOwner: TNumberModuleList);
begin
  inherited Create;
  FOwner := AOwner;
  FName := '';
  FSymbolName := '';
  FDescription := '';
  FDetailDescription := '';

  FCurrentValueHookList := TCoreClassListForObj.Create;

  FCurrentValueChangeAfterEventList := TCoreClassListForObj.Create;

  FCustomObjects := nil;
  FCustomValues := nil;

  FCurrentValue := NULL;
  FOriginValue := NULL;

  FEnabledHook := True;
  FEnabledEvent := True;

  FOnChange := nil;
end;

destructor TNumberModule.Destroy;
begin
  Clear;
  DisposeObject(FCurrentValueChangeAfterEventList);
  DisposeObject(FCurrentValueHookList);

  if FCustomObjects <> nil then
      DisposeObject(FCustomObjects);
  if FCustomValues <> nil then
      DisposeObject(FCustomValues);
  inherited Destroy;
end;

procedure TNumberModule.UpdateValue;
begin
  DoCurrentValueHook(FCurrentValue, FOriginValue);
end;

function TNumberModule.RegisterCurrentValueHook: TNumberModuleHookInterface;
begin
  Result := TNumberModuleHookInterface.Create(Self, FCurrentValueHookList);
end;

procedure TNumberModule.CopyHookInterfaceFrom(Sour: TNumberModule);
var
  i: Integer;
begin
  // copy new interface
  for i := 0 to Sour.FCurrentValueHookList.Count - 1 do
      RegisterCurrentValueHook.OnCurrentDMHook := TNumberModuleHookInterface(Sour.FCurrentValueHookList[i]).OnCurrentDMHook;
end;

function TNumberModule.RegisterCurrentValueChangeAfterEvent: TNumberModuleEventInterface;
begin
  Result := TNumberModuleEventInterface.Create(Self, FCurrentValueChangeAfterEventList);
end;

procedure TNumberModule.CopyChangeAfterEventInterfaceFrom(Sour: TNumberModule);
var
  i: Integer;
begin
  // copy new interface
  for i := 0 to Sour.FCurrentValueChangeAfterEventList.Count - 1 do
      RegisterCurrentValueChangeAfterEvent.OnCurrentDMEvent := TNumberModuleEventInterface(Sour.FCurrentValueChangeAfterEventList[i]).OnCurrentDMEvent;
end;

procedure TNumberModule.Assign(Sour: TNumberModule);
begin
  FCurrentValue := Sour.FCurrentValue;
  FOriginValue := Sour.FOriginValue;
  FName := Sour.FName;
  FSymbolName := Sour.FSymbolName;
  FDescription := Sour.FDescription;
  FDetailDescription := Sour.FDetailDescription;
end;

function TNumberModuleList.GetItems(AName: SystemString): TNumberModule;
begin
  Result := TNumberModule(FList[AName]);
  if Result = nil then
    begin
      Result := TNumberModule.Create(Self);
      FList[AName] := Result;
      Result.FName := AName;
    end;
end;

constructor TNumberModuleList.Create;
begin
  inherited Create;
  FList := THashObjectList.Create(True, 256);
end;

destructor TNumberModuleList.Destroy;
begin
  DisposeObject(FList);
  inherited Destroy;
end;

procedure TNumberModuleList.Delete(AName: SystemString);
begin
  FList.Delete(AName);
end;

function TNumberModuleList.Exists(AName: SystemString): Boolean;
begin
  Result := FList.Exists(AName);
end;

function TNumberModuleList.ExistsIntf(ADM: TNumberModule): Boolean;
begin
  Result := FList.ExistsObject(ADM);
end;

procedure TNumberModuleList.Clear;
begin
  FList.Clear;
end;

function TNumberModuleList.Macro(const AText, HeadToken, TailToken, OwnerFlag: SystemString; out Output: SystemString): Boolean;
begin
  {$IFDEF FPC}
  Result := ManualMacro(AText, HeadToken, TailToken, OwnerFlag, @__GetDMAsString, Output);
  {$ELSE}
  Result := ManualMacro(AText, HeadToken, TailToken, OwnerFlag, __GetDMAsString, Output);
  {$ENDIF}
end;

function TNumberModuleList.ManualMacro(const AText, HeadToken, TailToken, OwnerFlag: SystemString; OnDM2Text: TGetDMAsString; out Output: SystemString): Boolean;
var
  Lst: TCoreClassListForObj;

  function _GetDM(k: SystemString): TNumberModule;
  var
    i: Integer;
  begin
    for i := 0 to Lst.Count - 1 do
      begin
        Result := TNumberModule(Lst[i]);
        if (SameText(Result.FSymbolName, k)) or (SameText(Result.FName, k)) then
            Exit;
      end;
    Result := nil;
  end;

var
  Sour                      : umlString;
  hf, tf, owf               : umlString;
  bPos, ePos, OwnerPos, nPos: Integer;
  KeyText, OwnerKey, subKey : umlString;
  i                         : Integer;
  DM                        : TNumberModule;
begin
  Lst := TCoreClassListForObj.Create;
  FList.GetAsList(Lst);
  Output := '';
  Sour.Text := AText;
  hf.Text := HeadToken;
  tf.Text := TailToken;
  owf.Text := OwnerFlag;
  Result := True;

  i := 1;

  while i <= Sour.Len do
    begin
      if Sour.ComparePos(i, @hf) then
        begin
          bPos := i;
          ePos := Sour.GetPos(tf, i + hf.Len);
          if ePos > 0 then
            begin
              KeyText := Sour.copy(bPos + hf.Len, ePos - (bPos + hf.Len));
              OwnerPos := KeyText.GetPos(owf);
              if OwnerPos > 0 then
                begin
                  OwnerKey := KeyText.copy(1, OwnerPos - 1);
                  nPos := OwnerPos + owf.Len;
                  subKey := KeyText.copy(nPos, KeyText.Len - nPos + 1);

                  DM := _GetDM(OwnerKey.Text);
                  if DM <> nil then
                    begin
                      Output := Output + OnDM2Text(subKey.Text, DM);
                      i := ePos + tf.Len;
                      Continue;
                    end
                  else
                    begin
                      Result := False;
                    end;
                end
              else
                begin
                  DM := _GetDM(KeyText.Text);
                  if DM <> nil then
                    begin
                      Output := Output + OnDM2Text('', DM);
                      i := ePos + tf.Len;
                      Continue;
                    end
                  else
                    begin
                      Result := False;
                    end;
                end;
            end;
        end;

      Output := Output + Sour[i];
      inc(i);
    end;
  DisposeObject(Lst);
end;

procedure TNumberModuleList.Assign(Source: TNumberModuleList); // create by,zfy, 2011-6-17
var
  Lst      : TCoreClassListForObj;
  i        : Integer;
  newdm, DM: TNumberModule;
begin
  Lst := TCoreClassListForObj.Create;
  Source.FList.GetAsList(Lst);
  for i := 0 to Lst.Count - 1 do
    begin
      DM := Lst[i] as TNumberModule;
      newdm := Items[DM.name];
      newdm.Assign(DM);
    end;

  for i := 0 to Lst.Count - 1 do
    begin
      DM := Lst[i] as TNumberModule;
      DM.UpdateValue;
    end;
  DisposeObject(Lst);
end;

procedure TNumberModuleList.LoadFromStream(Stream: TCoreClassStream);
var
  df : TDataFrameEngine;
  DM : TNumberModule;
  n  : SystemString;
  Lst: TCoreClassListForObj;
  i  : Integer;
begin
  // format
  // name,current value,origin value
  Lst := TCoreClassListForObj.Create;
  df := TDataFrameEngine.Create;
  df.DecodeFrom(Stream);
  while not df.Reader.IsEnd do
    begin
      n := df.Reader.ReadString;
      DM := GetItems(n);
      DM.SymbolName := df.Reader.ReadString;
      DM.Description := df.Reader.ReadString;
      DM.DetailDescription := df.Reader.ReadString;
      DM.DirectOriginValue := df.Reader.ReadVariant;
      DM.DirectOriginValue := df.Reader.ReadVariant;
      Lst.Add(DM);
    end;
  DisposeObject(df);
  for i := 0 to Lst.Count - 1 do
    begin
      DM := TNumberModule(Lst[i]);
      DM.UpdateValue;
    end;
  DisposeObject(Lst);
end;

procedure TNumberModuleList.SaveToStream(Stream: TCoreClassStream);
var
  df : TDataFrameEngine;
  Lst: TCoreClassListForObj;
  i  : Integer;
  DM : TNumberModule;
begin
  // format
  // name,current value,origin value
  Lst := TCoreClassListForObj.Create;
  FList.GetAsList(Lst);
  df := TDataFrameEngine.Create;
  for i := 0 to Lst.Count - 1 do
    begin
      DM := TNumberModule(Lst[i]);
      df.WriteString(DM.name);
      df.WriteString(DM.SymbolName);
      df.WriteString(DM.Description);
      df.WriteString(DM.DetailDescription);
      df.WriteVariant(DM.OriginValue);
      df.WriteVariant(DM.CurrentValue);
    end;
  df.EncodeTo(Stream);
  DisposeObject(df);
  DisposeObject(Lst);
end;

procedure TNumberModuleList.LoadFromVariantList(v: THashVariantList);
var
  nl : TCoreClassStringList;
  i  : Integer;
  Lst: TCoreClassListForObj;
  DM : TNumberModule;
begin
  Lst := TCoreClassListForObj.Create;

  nl := TCoreClassStringList.Create;
  v.GetNameList(nl);

  for i := 0 to nl.Count - 1 do
    begin
      DM := GetItems(nl[i]);
      DM.DirectOriginValue := v[DM.name];
      DM.DirectCurrentValue := DM.DirectOriginValue;
      Lst.Add(DM);
    end;
  DisposeObject(nl);

  for i := 0 to Lst.Count - 1 do
    begin
      DM := TNumberModule(Lst[i]);
      DM.UpdateValue;
    end;
  DisposeObject(Lst);
end;

procedure TNumberModuleList.SaveToVariantList(v: THashVariantList);
var
  Lst: TCoreClassStringList;
  i  : Integer;
  DM : TNumberModule;
begin
  Lst := TCoreClassStringList.Create;
  FList.GetListData(Lst);
  for i := 0 to Lst.Count - 1 do
    begin
      DM := TNumberModule(Lst.Objects[i]);
      v[DM.name] := DM.OriginValue;
    end;
  DisposeObject(Lst);
end;

procedure TNMAutomated.DMCurrentValueHook(Sender: TNumberModuleHookInterface; OldValue: Variant; var NewValue: Variant);

  function IsMaxPriorityOverlap(ignore: PDMProcessingData): Boolean;
  var
    i: Integer;
    p: PDMProcessingData;
  begin
    Result := True;

    for i := 0 to FCurrentValueCalcList.Count - 1 do
      begin
        p := FCurrentValueCalcList[i];
        if (p <> ignore) and (p^.Processed) then
          if (p^.TypeID = ignore^.TypeID) and (p^.Priority >= ignore^.Priority) then
            begin
              Result := False;
              Exit;
            end;
      end;
  end;

  procedure ImpStyleValue(p: PDMProcessingData);
  begin
    case p^.Style of
      npsInc: NewValue := NewValue + p^.SeedNumber;
      npsDec: NewValue := NewValue - p^.SeedNumber;
      npsIncMul: NewValue := NewValue + FDMSource.OriginValue * p^.SeedNumber;
      npsDecMul: NewValue := NewValue - FDMSource.OriginValue * p^.SeedNumber;
      else
        Assert(False);
    end;
    p^.Processed := True;
  end;

var
  i: Integer;
  p: PDMProcessingData;
  b: Boolean;
begin
  for i := 0 to FCurrentValueCalcList.Count - 1 do
      PDMProcessingData(FCurrentValueCalcList[i])^.Processed := False;

  for i := 0 to FCurrentValueCalcList.Count - 1 do
    begin
      p := FCurrentValueCalcList[i];
      b := p^.Overlap;
      if not b then
          b := IsMaxPriorityOverlap(p);
      if b then
          ImpStyleValue(p);
    end;
end;

function TNMAutomated.GetCurrentValueCalcData(Flag: TCoreClassObject): PDMProcessingData;
var
  i: Integer;
begin
  for i := 0 to FCurrentValueCalcList.Count - 1 do
    if PDMProcessingData(FCurrentValueCalcList[i])^.Flag = Flag then
      begin
        Result := FCurrentValueCalcList[i];
        Exit;
      end;
  Result := nil;
end;

constructor TNMAutomated.Create(AOwner: TNMAutomatedManager; ADMSource: TNumberModule);
begin
  inherited Create;
  FOwner := AOwner;
  FDMSource := ADMSource;

  FCurrentValueHookIntf := FDMSource.RegisterCurrentValueHook;

  {$IFDEF FPC}
  FCurrentValueHookIntf.OnCurrentDMHook := @DMCurrentValueHook;
  {$ELSE}
  FCurrentValueHookIntf.OnCurrentDMHook := DMCurrentValueHook;
  {$ENDIF}
  FCurrentValueCalcList := TCoreClassList.Create;
end;

destructor TNMAutomated.Destroy;
begin
  Clear;
  DisposeObject(FCurrentValueCalcList);
  DisposeObject(FCurrentValueHookIntf);
  inherited Destroy;
end;

procedure TNMAutomated.Progress(deltaTime: Double);
var
  i: Integer;
  p: PDMProcessingData;
begin
  i := 0;
  while i < FCurrentValueCalcList.Count do
    begin
      p := FCurrentValueCalcList[i];
      if p^.CancelDelayTime > 0 then
        begin
          if p^.CancelDelayTime - deltaTime <= 0 then
            begin
              Dispose(p);
              FCurrentValueCalcList.Delete(i);
            end
          else
            begin
              p^.CancelDelayTime := p^.CancelDelayTime - deltaTime;
              inc(i);
            end;
        end
      else
          inc(i);
    end;
end;

procedure TNMAutomated.ChangeProcessStyle(Flag: TCoreClassObject; SeedNumber: Variant; Style: TNumberProcessStyle; CancelDelayTime: Double; Overlap: Boolean; TypeID: Integer;
  Priority: Cardinal);
var
  p: PDMProcessingData;
begin
  p := GetCurrentValueCalcData(Flag);
  if p = nil then
    begin
      New(p);
      FCurrentValueCalcList.Add(p);
    end;
  p^.Flag := Flag;
  p^.SeedNumber := SeedNumber;
  p^.Style := Style;
  p^.CancelDelayTime := CancelDelayTime;
  p^.Overlap := Overlap;
  p^.TypeID := TypeID;
  p^.Priority := Priority;
  p^.Processed := False;
  FDMSource.UpdateValue;
end;

procedure TNMAutomated.Cancel(Flag: TCoreClassObject);
var
  i          : Integer;
  p          : PDMProcessingData;
  ANeedUpdate: Boolean;
begin
  i := 0;
  ANeedUpdate := False;
  while i < FCurrentValueCalcList.Count do
    begin
      p := FCurrentValueCalcList[i];
      if p^.Flag = Flag then
        begin
          Dispose(p);
          FCurrentValueCalcList.Delete(i);
          ANeedUpdate := True;
        end
      else
          inc(i);
    end;
  if ANeedUpdate then
      FDMSource.UpdateValue;
end;

procedure TNMAutomated.Clear;
var
  i: Integer;
begin
  for i := 0 to FCurrentValueCalcList.Count - 1 do
      Dispose(PDMProcessingData(FCurrentValueCalcList[i]));
  FCurrentValueCalcList.Clear;
end;

function TNMAutomatedManager.GetOrCreate(ADMSource: TNumberModule): TNMAutomated;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    begin
      Result := TNMAutomated(FList[i]);
      if Result.FDMSource = ADMSource then
          Exit;
    end;
  Result := TNMAutomated.Create(Self, ADMSource);
  FList.Add(Result);
end;

constructor TNMAutomatedManager.Create;
begin
  inherited Create;
  FList := TCoreClassListForObj.Create;
end;

destructor TNMAutomatedManager.Destroy;
begin
  Clear;
  DisposeObject(FList);
  inherited Destroy;
end;

procedure TNMAutomatedManager.Progress(deltaTime: Double);
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
      TNMAutomated(FList[i]).Progress(deltaTime);
end;

procedure TNMAutomatedManager.Clear;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
      DisposeObject(TNMAutomated(FList[i]));
  FList.Clear;
end;

procedure TNMAutomatedManager.PostAutomatedProcess(Style: TNumberProcessStyle;
  ADMSource: TNumberModule; Flag: TCoreClassPersistent;
  SeedNumber: Variant; Overlap: Boolean; TypeID: Integer; Priority: Cardinal);
begin
  GetOrCreate(ADMSource).ChangeProcessStyle(Flag, SeedNumber, Style, 0, Overlap, TypeID, Priority);
end;

procedure TNMAutomatedManager.PostAutomatedDelayCancelProcess(Style: TNumberProcessStyle;
  ADMSource: TNumberModule; Flag: TCoreClassPersistent;
  SeedNumber: Variant; Overlap: Boolean; TypeID: Integer; Priority: Cardinal; CancelDelayTime: Double);
begin
  GetOrCreate(ADMSource).ChangeProcessStyle(Flag, SeedNumber, Style, CancelDelayTime, Overlap, TypeID, Priority);
end;

procedure TNMAutomatedManager.Delete(ADMSource: TNumberModule; Flag: TCoreClassObject);
begin
  GetOrCreate(ADMSource).Cancel(Flag);
end;

procedure TNMAutomatedManager.Delete(Flag: TCoreClassObject);
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
      TNMAutomated(FList[i]).Cancel(Flag);
end;

procedure test;
var
  nl: TNumberModuleList;
  n : SystemString;
begin
  nl := TNumberModuleList.Create;
  nl['a'].OriginValue := '123';
  nl['a'].Description := 'hahahaha';
  nl.Macro('hello <a.Description> world', '<', '>', '.', n);
  DisposeObject(nl);
  DoStatus(n);
end;

initialization

// test;

finalization

end.
