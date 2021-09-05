{ ****************************************************************************** }
{ * cadencer imp library  written by QQ 600585@qq.com                          * }
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

unit Cadencer;

{$INCLUDE zDefine.inc}

interface

uses CoreClasses;

type
  {
    Progression event for time-base animations/simulations.
    deltaTime is the time delta since last progress and newTime is the new
    time after the progress event is completed.
  }
  TCadencerProgressMethod = procedure(Sender: TObject; const deltaTime, newTime: Double) of object;
  TCadencerProgressCall = procedure(Sender: TObject; const deltaTime, newTime: Double);
{$IFDEF FPC}
  TCadencerProgressProc = procedure(Sender: TObject; const deltaTime, newTime: Double) is nested;
{$ELSE FPC}
  TCadencerProgressProc = reference to procedure(Sender: TObject; const deltaTime, newTime: Double);
{$ENDIF FPC}

  ICadencerProgressInterface = interface
    procedure CadencerProgress(const deltaTime, newTime: Double);
  end;

  {
    This component allows auto-progression of animation.
    Basicly dropping this component and linking it to your app will send
    it real-time progression events (time will be measured in seconds) while
    keeping the CPU 100% busy if possible (ie. if things change in your app).
    The progression time (the one you'll see in you progression events)
    is calculated using  (CurrentTime-OriginTime)*TimeMultiplier,
    CurrentTime being either manually or automatically updated using
    TimeReference (setting CurrentTime does NOT trigger progression).
  }
  TCadencer = class(TCoreClassObject)
  private
    { Private Declarations }
    FTimeMultiplier: Double;
    LastTime, DownTime, LastMultiplier: Double;
    FLastDeltaTime: Double;
    FEnabled: Boolean;
    FSleepLength: Integer;
    FCurrentTime: Double;
    FOriginTime: Double;
    FMaxDeltaTime, FMinDeltaTime, FFixedDeltaTime: Double;
    FOnProgress: TCadencerProgressMethod;
    FOnProgressCall: TCadencerProgressCall;
    FOnProgressProc: TCadencerProgressProc;
    FProgressing: Integer;
    FProgressIntf: ICadencerProgressInterface;
  protected
    function StoreTimeMultiplier: Boolean;
    procedure SetEnabled(const val_: Boolean);
    procedure SetTimeMultiplier(const val_: Double);
    procedure SetCurrentTime(const Value: Double);
    { Returns raw ref time (no multiplier, no offset) }
    function GetRawReferenceTime: Double;
  public
    constructor Create;
    destructor Destroy; override;

    { Allows to manually trigger a progression. Time stuff is handled automatically. If cadencer is disabled, this functions does nothing. }
    procedure Progress;

    { Adjusts CurrentTime if necessary, then returns its value. }
    function UpdateCurrentTime: Double;

    { Returns True if a "Progress" is underway. }
    function IsBusy: Boolean;

    { Reset the time parameters and returns to zero. }
    procedure Reset;

    { Value soustracted to current time to obtain progression time. }
    property OriginTime: Double read FOriginTime write FOriginTime;
    { Current time (manually or automatically set, see TimeReference). }
    property CurrentTime: Double read FCurrentTime write SetCurrentTime;

    { Enables/Disables cadencing.
      Disabling won't cause a jump when restarting, it is working like a play/pause (ie. may modify OriginTime to keep things smooth). }
    property Enabled: Boolean read FEnabled write SetEnabled default True;

    { Multiplier applied to the time reference. }
    property TimeMultiplier: Double read FTimeMultiplier write SetTimeMultiplier stored StoreTimeMultiplier;

    { Maximum value for deltaTime in progression events.
      If null or negative, no max deltaTime is defined, otherwise, whenever an event whose actual deltaTime would be superior to MaxDeltaTime occurs,
      deltaTime is clamped to this max, and the extra time is hidden by the cadencer (it isn't visible in CurrentTime either).
      This option allows to limit progression rate in simulations where high values would result in errors/random behaviour. }
    property MaxDeltaTime: Double read FMaxDeltaTime write FMaxDeltaTime;

    { Minimum value for deltaTime in progression events.
      If superior to zero, this value specifies the minimum time step between two progression events.
      This option allows to limit progression rate in simulations where low values would result in errors/random behaviour. }
    property MinDeltaTime: Double read FMinDeltaTime write FMinDeltaTime;

    { Fixed time-step value for progression events.
      If superior to zero, progression steps will happen with that fixed delta time.
      The progression remains time based,
      so zero to N events may be fired depending on the actual deltaTime (if deltaTime is inferior to FixedDeltaTime, no event will be fired,
      if it is superior to two times FixedDeltaTime, two events will be fired, etc.).
      This option allows to use fixed time steps in simulations (while the animation and rendering itself may happen at a lower or higher framerate). }
    property FixedDeltaTime: Double read FFixedDeltaTime write FFixedDeltaTime;

    { Allows relinquishing time to other threads/processes.
      A "sleep" is issued BEFORE each progress if SleepLength>=0 (see help for the "sleep" procedure in delphi for details). }
    property SleepLength: Integer read FSleepLength write FSleepLength default -1;

    { LastDeltaTime from progress. }
    property LastDeltaTime: Double read FLastDeltaTime;

    { backcall }
    property OnProgress: TCadencerProgressMethod read FOnProgress write FOnProgress;
    property OnProgressCall: TCadencerProgressCall read FOnProgressCall write FOnProgressCall;
    property OnProgressProc: TCadencerProgressProc read FOnProgressProc write FOnProgressProc;
    { interface }
    property ProgressInterface: ICadencerProgressInterface read FProgressIntf write FProgressIntf;
    property OnProgressInterface: ICadencerProgressInterface read FProgressIntf write FProgressIntf;
  end;

implementation

function TCadencer.StoreTimeMultiplier: Boolean;
begin
  Result := (FTimeMultiplier <> 1);
end;

procedure TCadencer.SetEnabled(const val_: Boolean);
begin
  if FEnabled <> val_ then
    begin
      FEnabled := val_;
      if Enabled then
          FOriginTime := FOriginTime + GetRawReferenceTime - DownTime
      else
          DownTime := GetRawReferenceTime;
    end;
end;

procedure TCadencer.SetTimeMultiplier(const val_: Double);
var
  rawRef: Double;
begin
  if val_ <> FTimeMultiplier then
    begin
      if val_ = 0 then
        begin
          LastMultiplier := FTimeMultiplier;
          Enabled := False;
        end
      else
        begin
          rawRef := GetRawReferenceTime;
          if FTimeMultiplier = 0 then
            begin
              Enabled := True;
              FOriginTime := rawRef - (rawRef - FOriginTime) * LastMultiplier / val_;
            end
          else
              FOriginTime := rawRef - (rawRef - FOriginTime) * FTimeMultiplier / val_;
        end;
      FTimeMultiplier := val_;
    end;
end;

procedure TCadencer.SetCurrentTime(const Value: Double);
begin
  LastTime := Value - (FCurrentTime - LastTime);
  FOriginTime := FOriginTime + (FCurrentTime - Value);
  FCurrentTime := Value;
end;

function TCadencer.GetRawReferenceTime: Double;
begin
  Result := GetTimeTick * 0.001;
end;

constructor TCadencer.Create;
begin
  inherited Create;
  DownTime := GetRawReferenceTime;
  FOriginTime := DownTime;
  FTimeMultiplier := 1;
  LastTime := 0;
  LastMultiplier := 0;
  FLastDeltaTime := 0;
  FSleepLength := -1;
  Enabled := True;
  FOnProgress := nil;
  FOnProgressCall := nil;
  FOnProgressProc := nil;
  FProgressIntf := nil;
end;

destructor TCadencer.Destroy;
begin
  while FProgressing > 0 do
      TCompute.Sleep(1);
  inherited Destroy;
end;

procedure TCadencer.Progress;
var
  deltaTime, newTime, totalDelta: Double;
begin
  { basic protection against infinite loops, }
  { shall never happen, unless there is a bug in user code }
  if FProgressing < 0 then
      Exit;
  if Enabled then
    begin
      { avoid stalling everything else... }
      if SleepLength >= 0 then
          TCoreClassThread.Sleep(SleepLength);
    end;
  AtomInc(FProgressing);
  try
    if Enabled then
      begin
        { One of the processed messages might have disabled us }
        if Enabled then
          begin
            { ...and progress ! }
            newTime := UpdateCurrentTime;
            deltaTime := newTime - LastTime;
            if (deltaTime >= MinDeltaTime) and (deltaTime >= FixedDeltaTime) then
              begin
                if FMaxDeltaTime > 0 then
                  begin
                    if deltaTime > FMaxDeltaTime then
                      begin
                        FOriginTime := FOriginTime + (deltaTime - FMaxDeltaTime) / FTimeMultiplier;
                        deltaTime := FMaxDeltaTime;
                        newTime := LastTime + deltaTime;
                      end;
                  end;
                totalDelta := deltaTime;
                if FixedDeltaTime > 0 then
                    deltaTime := FixedDeltaTime;
                while totalDelta >= deltaTime do
                  begin
                    LastTime := LastTime + deltaTime;
                    FLastDeltaTime := deltaTime;
                    try
                      if Assigned(FOnProgress) then
                          FOnProgress(Self, deltaTime, newTime);
                      if Assigned(FOnProgressCall) then
                          FOnProgressCall(Self, deltaTime, newTime);
                      if Assigned(FOnProgressProc) then
                          FOnProgressProc(Self, deltaTime, newTime);
                      if Assigned(FProgressIntf) then
                          FProgressIntf.CadencerProgress(deltaTime, newTime);
                    except
                    end;

                    if deltaTime <= 0 then
                        Break;
                    totalDelta := totalDelta - deltaTime;
                  end;
              end;
          end;
      end;
  finally
      AtomDec(FProgressing);
  end;
end;

function TCadencer.UpdateCurrentTime: Double;
begin
  Result := (GetRawReferenceTime - FOriginTime) * FTimeMultiplier;
  FCurrentTime := Result;
end;

function TCadencer.IsBusy: Boolean;
begin
  Result := (FProgressing > 0);
end;

procedure TCadencer.Reset;
begin
  LastTime := 0;
  DownTime := GetRawReferenceTime;
  FOriginTime := DownTime;
end;

initialization

finalization

end.
