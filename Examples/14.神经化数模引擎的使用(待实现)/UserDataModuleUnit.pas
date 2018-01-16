unit UserDataModuleUnit;

{$I zDefine.inc}

interface

uses CoreClasses, Geometry2DUnit, NumberBase;

type
  TBioBaseData = class;

  TDataItem = class(TCoreClassObject)
  private
    FOwner: TBioBaseData;
    FName : string;
    FValue: TNumberModule;

    FIncreaseFromCoreLogic          : TNumberModule;
    FIncreasePercentageFromCoreLogic: TNumberModule;
    FReduceFromCoreLogic            : TNumberModule;
    FReducePercentageFromCoreLogic  : TNumberModule;

    FIncreaseFromInternal          : TNumberModule;
    FIncreasePercentageFromInternal: TNumberModule;
    FReduceFromInternal            : TNumberModule;
    FReducePercentageFromInternal  : TNumberModule;

    FIncreaseFromDefine          : TNumberModule;
    FIncreasePercentageFromDefine: TNumberModule;
    FReduceFromDefine            : TNumberModule;
    FReducePercentageFromDefine  : TNumberModule;

    FLastFinalValue      : Variant;
    FNeedRecalcFinalValue: Boolean;

    procedure ChangeEvent(Sender: TNumberModuleEventInterface; NewValue: Variant);
  public
    constructor Create(AOwner: TBioBaseData; AName: string);
    destructor Destroy; override;

    property Owner: TBioBaseData read FOwner;
    property name: string read FName;

    function GetFinalValue: Variant;
    property FinalValue: Variant read GetFinalValue;

    property Value: TNumberModule read FValue;

    property IncreaseFromCoreLogic: TNumberModule read FIncreaseFromCoreLogic;
    property IncreasePercentageFromCoreLogic: TNumberModule read FIncreasePercentageFromCoreLogic;
    property ReduceFromCoreLogic: TNumberModule read FReduceFromCoreLogic;
    property ReducePercentageFromCoreLogic: TNumberModule read FReducePercentageFromCoreLogic;

    property IncreaseFromInternal: TNumberModule read FIncreaseFromInternal;
    property IncreasePercentageFromInternal: TNumberModule read FIncreasePercentageFromInternal;
    property ReduceFromInternal: TNumberModule read FReduceFromInternal;
    property ReducePercentageFromInternal: TNumberModule read FReducePercentageFromInternal;

    property IncreaseFromDefine: TNumberModule read FIncreaseFromDefine;
    property IncreasePercentageFromDefine: TNumberModule read FIncreasePercentageFromDefine;
    property ReduceFromDefine: TNumberModule read FReduceFromDefine;
    property ReducePercentageFromDefine: TNumberModule read FReducePercentageFromDefine;
  end;

  TBioBaseData = class(TCoreClassObject)
  private
    FDataItemList      : TCoreClassListForObj;
    FNMList            : TNumberModuleList;
    FNMAutomatedManager: TNMAutomatedManager;
    FUpdateCounter     : Integer;
  protected
    procedure NumberItemChange(Sender: TDataItem);
  public
    constructor Create;
    destructor Destroy; override;

    procedure InitData;

    procedure Progress(deltaTime: Double); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure BeginUpdate; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure EndUpdate; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure RebuildAssociate; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure Clear;

    procedure DeletePostFlag(Flag: TCoreClassPersistent); {$IFDEF INLINE_ASM} inline; {$ENDIF}
  end;

implementation

procedure TDataItem.ChangeEvent(Sender: TNumberModuleEventInterface; NewValue: Variant);
begin
  FNeedRecalcFinalValue := True;
  FOwner.NumberItemChange(Self);
end;

constructor TDataItem.Create(AOwner: TBioBaseData; AName: string);
begin
  Assert(not AOwner.FNMList.Exists(AName));
  inherited Create;
  FOwner := AOwner;
  FOwner.FDataItemList.Add(Self);
  FName := AName;
  FValue := FOwner.FNMList[FName];

  FIncreaseFromCoreLogic := FOwner.FNMList[FName + '.IncreaseFromCoreLogic'];
  FIncreasePercentageFromCoreLogic := FOwner.FNMList[FName + '.IncreasePercentageFromCoreLogic'];
  FReduceFromCoreLogic := FOwner.FNMList[FName + '.ReduceFromCoreLogic'];
  FReducePercentageFromCoreLogic := FOwner.FNMList[FName + '.ReducePercentageFromCoreLogic'];

  FIncreaseFromInternal := FOwner.FNMList[FName + '.IncreaseFromInternal'];
  FIncreasePercentageFromInternal := FOwner.FNMList[FName + '.IncreasePercentageFromInternal'];
  FReduceFromInternal := FOwner.FNMList[FName + '.ReduceFromInternal'];
  FReducePercentageFromInternal := FOwner.FNMList[FName + '.ReducePercentageFromInternal'];

  FIncreaseFromDefine := FOwner.FNMList[FName + '.IncreaseFromDefine'];
  FIncreasePercentageFromDefine := FOwner.FNMList[FName + '.IncreasePercentageFromDefine'];
  FReduceFromDefine := FOwner.FNMList[FName + '.ReduceFromDefine'];
  FReducePercentageFromDefine := FOwner.FNMList[FName + '.ReducePercentageFromDefine'];

  FValue.OriginValue := 0;

  FIncreaseFromCoreLogic.OriginValue := 0;
  FIncreasePercentageFromCoreLogic.OriginValue := 0;
  FReduceFromCoreLogic.OriginValue := 0;
  FReducePercentageFromCoreLogic.OriginValue := 0;

  FIncreaseFromInternal.OriginValue := 0;
  FIncreasePercentageFromInternal.OriginValue := 0;
  FReduceFromInternal.OriginValue := 0;
  FReducePercentageFromInternal.OriginValue := 0;

  FIncreaseFromDefine.OriginValue := 0;
  FIncreasePercentageFromDefine.OriginValue := 0;
  FReduceFromDefine.OriginValue := 0;
  FReducePercentageFromDefine.OriginValue := 0;

  {$IFDEF FPC}
  FValue.RegisterCurrentValueChangeAfterEvent.OnCurrentDMEvent := @ChangeEvent;

  FIncreaseFromCoreLogic.RegisterCurrentValueChangeAfterEvent.OnCurrentDMEvent := @ChangeEvent;
  FIncreasePercentageFromCoreLogic.RegisterCurrentValueChangeAfterEvent.OnCurrentDMEvent := @ChangeEvent;
  FReduceFromCoreLogic.RegisterCurrentValueChangeAfterEvent.OnCurrentDMEvent := @ChangeEvent;
  FReducePercentageFromCoreLogic.RegisterCurrentValueChangeAfterEvent.OnCurrentDMEvent := @ChangeEvent;

  FIncreaseFromInternal.RegisterCurrentValueChangeAfterEvent.OnCurrentDMEvent := @ChangeEvent;
  FIncreasePercentageFromInternal.RegisterCurrentValueChangeAfterEvent.OnCurrentDMEvent := @ChangeEvent;
  FReduceFromInternal.RegisterCurrentValueChangeAfterEvent.OnCurrentDMEvent := @ChangeEvent;
  FReducePercentageFromInternal.RegisterCurrentValueChangeAfterEvent.OnCurrentDMEvent := @ChangeEvent;

  FIncreaseFromDefine.RegisterCurrentValueChangeAfterEvent.OnCurrentDMEvent := @ChangeEvent;
  FIncreasePercentageFromDefine.RegisterCurrentValueChangeAfterEvent.OnCurrentDMEvent := @ChangeEvent;
  FReduceFromDefine.RegisterCurrentValueChangeAfterEvent.OnCurrentDMEvent := @ChangeEvent;
  FReducePercentageFromDefine.RegisterCurrentValueChangeAfterEvent.OnCurrentDMEvent := @ChangeEvent;
  {$ELSE}
  FValue.RegisterCurrentValueChangeAfterEvent.OnCurrentDMEvent := ChangeEvent;

  FIncreaseFromCoreLogic.RegisterCurrentValueChangeAfterEvent.OnCurrentDMEvent := ChangeEvent;
  FIncreasePercentageFromCoreLogic.RegisterCurrentValueChangeAfterEvent.OnCurrentDMEvent := ChangeEvent;
  FReduceFromCoreLogic.RegisterCurrentValueChangeAfterEvent.OnCurrentDMEvent := ChangeEvent;
  FReducePercentageFromCoreLogic.RegisterCurrentValueChangeAfterEvent.OnCurrentDMEvent := ChangeEvent;

  FIncreaseFromInternal.RegisterCurrentValueChangeAfterEvent.OnCurrentDMEvent := ChangeEvent;
  FIncreasePercentageFromInternal.RegisterCurrentValueChangeAfterEvent.OnCurrentDMEvent := ChangeEvent;
  FReduceFromInternal.RegisterCurrentValueChangeAfterEvent.OnCurrentDMEvent := ChangeEvent;
  FReducePercentageFromInternal.RegisterCurrentValueChangeAfterEvent.OnCurrentDMEvent := ChangeEvent;

  FIncreaseFromDefine.RegisterCurrentValueChangeAfterEvent.OnCurrentDMEvent := ChangeEvent;
  FIncreasePercentageFromDefine.RegisterCurrentValueChangeAfterEvent.OnCurrentDMEvent := ChangeEvent;
  FReduceFromDefine.RegisterCurrentValueChangeAfterEvent.OnCurrentDMEvent := ChangeEvent;
  FReducePercentageFromDefine.RegisterCurrentValueChangeAfterEvent.OnCurrentDMEvent := ChangeEvent;
  {$ENDIF}
  FLastFinalValue := 0;
  FNeedRecalcFinalValue := True;
end;

destructor TDataItem.Destroy;
begin
  inherited Destroy;
end;

function TDataItem.GetFinalValue: Variant;
var
  vInc, vDec, vIncP, vDecP, v: Variant;
begin
  if FNeedRecalcFinalValue then
    begin
      vInc := FIncreaseFromCoreLogic.AsValue +
        FIncreaseFromInternal.AsValue +
        FIncreaseFromDefine.AsValue;

      vDec := FReduceFromCoreLogic.AsValue +
        FReduceFromInternal.AsValue +
        FReduceFromDefine.AsValue;

      vIncP := FIncreasePercentageFromCoreLogic.AsValue +
        FIncreasePercentageFromInternal.AsValue +
        FIncreasePercentageFromDefine.AsValue;

      vDecP := FReducePercentageFromCoreLogic.AsValue +
        FReducePercentageFromInternal.AsValue +
        FReducePercentageFromDefine.AsValue;

      v := FValue.AsValue + (vInc - vDec);

      FLastFinalValue := v + v * ((vIncP - vDecP) * 0.01);

      FNeedRecalcFinalValue := False;
    end;
  Result := FLastFinalValue;
end;

procedure TBioBaseData.NumberItemChange(Sender: TDataItem);
begin
  if FUpdateCounter > 0 then
      Exit;
  RebuildAssociate;
end;

constructor TBioBaseData.Create;
begin
  inherited Create;
  FDataItemList := TCoreClassListForObj.Create;
  FNMList := TNumberModuleList.Create;
  FNMAutomatedManager := TNMAutomatedManager.Create;
  FUpdateCounter := 0;

  InitData;
  RebuildAssociate;
end;

destructor TBioBaseData.Destroy;
begin
  Clear;
  DisposeObject(FNMAutomatedManager);
  DisposeObject(FNMList);
  inherited Destroy;
end;

procedure TBioBaseData.InitData;
begin
end;

procedure TBioBaseData.Progress(deltaTime: Double);
begin
  FNMAutomatedManager.Progress(deltaTime);
end;

procedure TBioBaseData.BeginUpdate;
begin
  inc(FUpdateCounter);
end;

procedure TBioBaseData.EndUpdate;
begin
  Dec(FUpdateCounter);
  if FUpdateCounter <= 0 then
    begin
      FUpdateCounter := 0;
      RebuildAssociate;
    end;
end;

procedure TBioBaseData.RebuildAssociate;
var
  i: Integer;
begin
  for i := 0 to FDataItemList.Count - 1 do
      TDataItem(FDataItemList[i]).FNeedRecalcFinalValue := True;
end;

procedure TBioBaseData.Clear;
var
  i: Integer;
begin
  for i := 0 to FDataItemList.Count - 1 do
      DisposeObject(FDataItemList[i]);
  FDataItemList.Clear;

  FNMAutomatedManager.Clear;
  FNMList.Clear;
end;

procedure TBioBaseData.DeletePostFlag(Flag: TCoreClassPersistent);
begin
  FNMAutomatedManager.Delete(Flag);
end;

end.
