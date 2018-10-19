unit AC2KeepAwakeUnit;

interface

uses
  System.Classes,
  FMX.Platform;

type
  TOnAwakeEvent = procedure;

  TKeepAwake = class
  private
    FOnAllowSleeping: TOnAwakeEvent;
    FOnKeepAwake: TOnAwakeEvent;
    function HandleAppEvent(AEvent: TApplicationEvent; AContext: TObject): Boolean;
    procedure SetOnKeepAwake(const Value: TOnAwakeEvent);
    procedure SetOnAllowSleeping(const Value: TOnAwakeEvent);
  public
    property OnKeepAwake: TOnAwakeEvent read FOnKeepAwake write SetOnKeepAwake;
    property OnAllowSleeping: TOnAwakeEvent read FOnAllowSleeping write SetOnAllowSleeping;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TKeepAwake }

constructor TKeepAwake.Create;
var
  LService: IFMXApplicationEventService;
begin
  inherited Create;

  FOnKeepAwake := nil;
  FOnAllowSleeping := nil;

  if TPlatformServices.Current.SupportsPlatformService(IFMXApplicationEventService, IInterface(LService)) then
    LService.SetApplicationEventHandler(HandleAppEvent);
end;

destructor TKeepAwake.Destroy;
begin
  if Assigned(OnAllowSleeping) then
    OnAllowSleeping;
  inherited;
end;

function TKeepAwake.HandleAppEvent(AEvent: TApplicationEvent; AContext: TObject): Boolean;
begin
  Result := False;

  case AEvent of
    TApplicationEvent.FinishedLaunching,
    TApplicationEvent.BecameActive,
    TApplicationEvent.WillBecomeForeground:
      begin
        if Assigned(OnKeepAwake) then
          OnKeepAwake;
        Exit(True);
      end;
    TApplicationEvent.WillTerminate,
    TApplicationEvent.WillBecomeInactive,
    TApplicationEvent.EnteredBackground:
      begin
        if Assigned(OnAllowSleeping) then
          OnAllowSleeping;
        Exit(True);
      end;
  end;
end;

procedure TKeepAwake.SetOnKeepAwake(const Value: TOnAwakeEvent);
begin
  FOnKeepAwake := Value;
end;

procedure TKeepAwake.SetOnAllowSleeping(const Value: TOnAwakeEvent);
begin
  FOnAllowSleeping := Value;
end;

end.
