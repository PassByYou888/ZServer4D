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

uses ListEngine, CoreClasses, DataFrameEngine, PascalStrings;

type
  TNumberModuleHookInterface  = class;
  TNumberModuleEventInterface = class;
  TNumberModuleList           = class;
  TNumberModule               = class;

  TNumberModuleHook = procedure(Sender: TNumberModuleHookInterface; OldValue: Variant; var NewValue: Variant) of object;

  TNumberModuleHookInterface = class(TCoreClassObject)
  private
    FOwner: TNumberModule;
    FOwnerList: TCoreClassListForObj;
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
    FOwner: TNumberModule;
    FOwnerList: TCoreClassListForObj;
    FOnCurrentDMEvent: TNumberModuleEvent;
  protected
  public
    constructor Create(AOwner: TNumberModule; AOwnerList: TCoreClassListForObj);
    destructor Destroy; override;

    property Owner: TNumberModule read FOwner;
    property OnCurrentDMEvent: TNumberModuleEvent read FOnCurrentDMEvent write FOnCurrentDMEvent;
  end;

  TNumberModuleNotifyEvent = procedure();
  TNumberModuleChangeEvent = procedure(const OldValue, NewValue: Variant);

  TNumberModule = class(TCoreClassObject)
  private
    FOwner: TNumberModuleList;
    FName, FSymbolName, FDescription, FDetailDescription: SystemString;

    FCurrentValueHookList: TCoreClassListForObj;
    FCurrentValueChangeAfterEventList: TCoreClassListForObj;

    FCurrentValue: Variant;
    FOriginValue: Variant;

    FCustomObjects: THashObjectList;
    FCustomValues: THashVariantList;

    FEnabledHook: Boolean;
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
    procedure UpdateValue;
    // reg hook interface
    function RegisterCurrentValueHook: TNumberModuleHookInterface;
    procedure CopyHookInterfaceFrom(sour: TNumberModule);
    // reg change after event
    function RegisterCurrentValueChangeAfterEvent: TNumberModuleEventInterface;
    procedure CopyChangeAfterEventInterfaceFrom(sour: TNumberModule);
    // copy
    procedure Assign(sour: TNumberModule);
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
    property Name: SystemString read FName write SetName;
    property SymbolName: SystemString read FSymbolName write FSymbolName;
    property Description: SystemString read FDescription write FDescription;
    property DetailDescription: SystemString read FDetailDescription write FDetailDescription;
  end;

  TGetDMAsString = function(key: SystemString; DM: TNumberModule): SystemString;

  TNumberModuleList = class(TCoreClassObject)
  protected
    FList: THashObjectList;
    function GetItems(AName: SystemString): TNumberModule;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Delete(AName: SystemString);
    function Exists(AName: SystemString): Boolean;
    function ExistsIntf(ADM: TNumberModule): Boolean;
    procedure Clear;

    function Macro(const AText, HeadToken, TailToken, OwnerFlag: SystemString; out output: SystemString): Boolean;
    function ManualMacro(const AText, HeadToken, TailToken, OwnerFlag: SystemString; OnDM2Text: TGetDMAsString; out output: SystemString): Boolean;

    procedure Assign(Source: TNumberModuleList); // create by 2011-6-17

    // can merge current
    procedure LoadFromStream(stream: TCoreClassStream);
    // save
    procedure SaveToStream(stream: TCoreClassStream);

    // laod form text, auto merge current
    procedure LoadFromVariantList(v: THashVariantList);
    // save as text
    procedure SaveToVariantList(v: THashVariantList);

    property Items[AName: SystemString]: TNumberModule read GetItems; default;
    property List: THashObjectList read FList;
  end;

  TNMAutomatedManager = class;

  TNumberProcessStyle = (npsInc, npsDec, npsIncMul, npsDecMul);

  TNumberProcessingData = record
    token: TCoreClassObject;
    opValue: Variant;
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
    FOwner: TNMAutomatedManager;
    FDMSource: TNumberModule;
    FCurrentValueHookIntf: TNumberModuleHookInterface;
    FCurrentValueCalcList: TCoreClassList;
  protected
    procedure DMCurrentValueHook(Sender: TNumberModuleHookInterface; OldValue: Variant; var NewValue: Variant);
  private
    function GetCurrentValueCalcData(token: TCoreClassObject): PDMProcessingData;
  public
    constructor Create(AOwner: TNMAutomatedManager; ADMSource: TNumberModule);
    destructor Destroy; override;

    property Owner: TNMAutomatedManager read FOwner write FOwner;

    procedure Progress(deltaTime: Double);
    procedure ChangeProcessStyle(token: TCoreClassObject;
      opValue: Variant; Style: TNumberProcessStyle; CancelDelayTime: Double; Overlap: Boolean; TypeID: Integer; Priority: Cardinal);
    procedure Cancel(token: TCoreClassObject);
    procedure Clear;
  end;

  TNMAutomatedManager = class(TCoreClassPersistent)
  private
    FList: TCoreClassListForObj;

    function GetOrCreate(ADMSource: TNumberModule): TNMAutomated;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Progress(deltaTime: Double);
    procedure Clear;

    procedure PostAutomatedProcess(Style: TNumberProcessStyle;
      ADMSource: TNumberModule; token: TCoreClassPersistent;
      opValue: Variant; Overlap: Boolean; TypeID: Integer; Priority: Cardinal);
    procedure PostAutomatedDelayCancelProcess(Style: TNumberProcessStyle;
      ADMSource: TNumberModule; token: TCoreClassPersistent;
      opValue: Variant; Overlap: Boolean; TypeID: Integer; Priority: Cardinal; CancelDelayTime: Double);
    procedure Delete(ADMSource: TNumberModule; token: TCoreClassObject); overload;
    procedure Delete(token: TCoreClassObject); overload;
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
      Result := DM.Name
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
  i: Integer;
  _H: TNumberModuleHookInterface;
  _e: TNumberModuleEventInterface;

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
        _e := TNumberModuleEventInterface(FCurrentValueChangeAfterEventList[i]);
        if Assigned(_e.FOnCurrentDMEvent) then
          begin
            try
                _e.FOnCurrentDMEvent(_e, FCurrentValue);
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

  FCurrentValue := Null;
  FOriginValue := Null;

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

procedure TNumberModule.CopyHookInterfaceFrom(sour: TNumberModule);
var
  i: Integer;
begin
  // copy new interface
  for i := 0 to sour.FCurrentValueHookList.Count - 1 do
      RegisterCurrentValueHook.OnCurrentDMHook := TNumberModuleHookInterface(sour.FCurrentValueHookList[i]).OnCurrentDMHook;
end;

function TNumberModule.RegisterCurrentValueChangeAfterEvent: TNumberModuleEventInterface;
begin
  Result := TNumberModuleEventInterface.Create(Self, FCurrentValueChangeAfterEventList);
end;

procedure TNumberModule.CopyChangeAfterEventInterfaceFrom(sour: TNumberModule);
var
  i: Integer;
begin
  // copy new interface
  for i := 0 to sour.FCurrentValueChangeAfterEventList.Count - 1 do
      RegisterCurrentValueChangeAfterEvent.OnCurrentDMEvent := TNumberModuleEventInterface(sour.FCurrentValueChangeAfterEventList[i]).OnCurrentDMEvent;
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
  FList := THashObjectList.CustomCreate(True, 256);
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

function TNumberModuleList.Macro(const AText, HeadToken, TailToken, OwnerFlag: SystemString; out output: SystemString): Boolean;
begin
  Result := ManualMacro(AText, HeadToken, TailToken, OwnerFlag, {$IFDEF FPC}@{$ENDIF FPC}__GetDMAsString, output);
end;

function TNumberModuleList.ManualMacro(const AText, HeadToken, TailToken, OwnerFlag: SystemString; OnDM2Text: TGetDMAsString; out output: SystemString): Boolean;
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
            Exit;
      end;
    Result := nil;
  end;

var
  sour: U_String;
  hf, TF, owf: U_String;
  bPos, ePos, OwnerPos, nPos: Integer;
  KeyText, OwnerKey, SubKey: U_String;
  i: Integer;
  DM: TNumberModule;
begin
  lst := TCoreClassListForObj.Create;
  FList.GetAsList(lst);
  output := '';
  sour.Text := AText;
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

                  DM := _GetDM(OwnerKey.Text);
                  if DM <> nil then
                    begin
                      output := output + OnDM2Text(SubKey.Text, DM);
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
                  DM := _GetDM(KeyText.Text);
                  if DM <> nil then
                    begin
                      output := output + OnDM2Text('', DM);
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

procedure TNumberModuleList.Assign(Source: TNumberModuleList); // create by,zfy, 2011-6-17
var
  lst: TCoreClassListForObj;
  i: Integer;
  newdm, DM: TNumberModule;
begin
  lst := TCoreClassListForObj.Create;
  Source.FList.GetAsList(lst);
  for i := 0 to lst.Count - 1 do
    begin
      DM := lst[i] as TNumberModule;
      newdm := Items[DM.Name];
      newdm.Assign(DM);
    end;

  for i := 0 to lst.Count - 1 do
    begin
      DM := lst[i] as TNumberModule;
      DM.UpdateValue;
    end;
  DisposeObject(lst);
end;

procedure TNumberModuleList.LoadFromStream(stream: TCoreClassStream);
var
  df: TDataFrameEngine;
  DM: TNumberModule;
  n: SystemString;
  lst: TCoreClassListForObj;
  i: Integer;
begin
  // format
  // name,current value,origin value
  lst := TCoreClassListForObj.Create;
  df := TDataFrameEngine.Create;
  df.DecodeFrom(stream);
  while not df.Reader.IsEnd do
    begin
      n := df.Reader.ReadString;
      DM := GetItems(n);
      DM.SymbolName := df.Reader.ReadString;
      DM.Description := df.Reader.ReadString;
      DM.DetailDescription := df.Reader.ReadString;
      DM.DirectOriginValue := df.Reader.ReadVariant;
      DM.DirectOriginValue := df.Reader.ReadVariant;
      lst.Add(DM);
    end;
  DisposeObject(df);
  for i := 0 to lst.Count - 1 do
    begin
      DM := TNumberModule(lst[i]);
      DM.UpdateValue;
    end;
  DisposeObject(lst);
end;

procedure TNumberModuleList.SaveToStream(stream: TCoreClassStream);
var
  df: TDataFrameEngine;
  lst: TCoreClassListForObj;
  i: Integer;
  DM: TNumberModule;
begin
  // format
  // name,current value,origin value
  lst := TCoreClassListForObj.Create;
  FList.GetAsList(lst);
  df := TDataFrameEngine.Create;
  for i := 0 to lst.Count - 1 do
    begin
      DM := TNumberModule(lst[i]);
      df.WriteString(DM.Name);
      df.WriteString(DM.SymbolName);
      df.WriteString(DM.Description);
      df.WriteString(DM.DetailDescription);
      df.WriteVariant(DM.OriginValue);
      df.WriteVariant(DM.CurrentValue);
    end;
  df.EncodeTo(stream);
  DisposeObject(df);
  DisposeObject(lst);
end;

procedure TNumberModuleList.LoadFromVariantList(v: THashVariantList);
var
  NL: TCoreClassStringList;
  i: Integer;
  lst: TCoreClassListForObj;
  DM: TNumberModule;
begin
  lst := TCoreClassListForObj.Create;

  NL := TCoreClassStringList.Create;
  v.GetNameList(NL);

  for i := 0 to NL.Count - 1 do
    begin
      DM := GetItems(NL[i]);
      DM.DirectOriginValue := v[DM.Name];
      DM.DirectCurrentValue := DM.DirectOriginValue;
      lst.Add(DM);
    end;
  DisposeObject(NL);

  for i := 0 to lst.Count - 1 do
    begin
      DM := TNumberModule(lst[i]);
      DM.UpdateValue;
    end;
  DisposeObject(lst);
end;

procedure TNumberModuleList.SaveToVariantList(v: THashVariantList);
var
  lst: TCoreClassStringList;
  i: Integer;
  DM: TNumberModule;
begin
  lst := TCoreClassStringList.Create;
  FList.GetListData(lst);
  for i := 0 to lst.Count - 1 do
    begin
      DM := TNumberModule(lst.Objects[i]);
      v[DM.Name] := DM.OriginValue;
    end;
  DisposeObject(lst);
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
      npsInc: NewValue := NewValue + p^.opValue;
      npsDec: NewValue := NewValue - p^.opValue;
      npsIncMul: NewValue := NewValue + FDMSource.OriginValue * p^.opValue;
      npsDecMul: NewValue := NewValue - FDMSource.OriginValue * p^.opValue;
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

function TNMAutomated.GetCurrentValueCalcData(token: TCoreClassObject): PDMProcessingData;
var
  i: Integer;
begin
  for i := 0 to FCurrentValueCalcList.Count - 1 do
    if PDMProcessingData(FCurrentValueCalcList[i])^.token = token then
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

  FCurrentValueHookIntf.OnCurrentDMHook := {$IFDEF FPC}@{$ENDIF FPC}DMCurrentValueHook;
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

procedure TNMAutomated.ChangeProcessStyle(token: TCoreClassObject; opValue: Variant; Style: TNumberProcessStyle; CancelDelayTime: Double; Overlap: Boolean; TypeID: Integer;
  Priority: Cardinal);
var
  p: PDMProcessingData;
begin
  p := GetCurrentValueCalcData(token);
  if p = nil then
    begin
      new(p);
      FCurrentValueCalcList.Add(p);
    end;
  p^.token := token;
  p^.opValue := opValue;
  p^.Style := Style;
  p^.CancelDelayTime := CancelDelayTime;
  p^.Overlap := Overlap;
  p^.TypeID := TypeID;
  p^.Priority := Priority;
  p^.Processed := False;
  FDMSource.UpdateValue;
end;

procedure TNMAutomated.Cancel(token: TCoreClassObject);
var
  i: Integer;
  p: PDMProcessingData;
  ANeedUpdate: Boolean;
begin
  i := 0;
  ANeedUpdate := False;
  while i < FCurrentValueCalcList.Count do
    begin
      p := FCurrentValueCalcList[i];
      if p^.token = token then
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
  ADMSource: TNumberModule; token: TCoreClassPersistent;
  opValue: Variant; Overlap: Boolean; TypeID: Integer; Priority: Cardinal);
begin
  GetOrCreate(ADMSource).ChangeProcessStyle(token, opValue, Style, 0, Overlap, TypeID, Priority);
end;

procedure TNMAutomatedManager.PostAutomatedDelayCancelProcess(Style: TNumberProcessStyle;
  ADMSource: TNumberModule; token: TCoreClassPersistent;
  opValue: Variant; Overlap: Boolean; TypeID: Integer; Priority: Cardinal; CancelDelayTime: Double);
begin
  GetOrCreate(ADMSource).ChangeProcessStyle(token, opValue, Style, CancelDelayTime, Overlap, TypeID, Priority);
end;

procedure TNMAutomatedManager.Delete(ADMSource: TNumberModule; token: TCoreClassObject);
begin
  GetOrCreate(ADMSource).Cancel(token);
end;

procedure TNMAutomatedManager.Delete(token: TCoreClassObject);
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
      TNMAutomated(FList[i]).Cancel(token);
end;

procedure test;
var
  NL: TNumberModuleList;
  n: SystemString;
begin
  NL := TNumberModuleList.Create;
  NL['a'].OriginValue := '123';
  NL['a'].Description := 'hahahaha';
  NL.Macro('hello <a.Description> world', '<', '>', '.', n);
  DisposeObject(NL);
  DoStatus(n);
end;

initialization

// test;

finalization

end.
