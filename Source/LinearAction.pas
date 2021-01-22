{ ****************************************************************************** }
{ * linear action          written by QQ 600585@qq.com                         * }
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
unit LinearAction;

{$INCLUDE zDefine.inc}

interface

uses CoreClasses, DoStatusIO, PascalStrings, UnicodeMixedLib;

type
  TCoreActionID = Integer;
  TCoreActionString = SystemString;
  TCoreActionState = (asPlaying, asPause, asStop, asOver);
  TCoreActionStates = set of TCoreActionState;
  TCoreAction = class;
  TCoreActionList = class;
  TCoreActionLinear = class;

  TCoreAction = class(TCoreClassObject)
  public
    Owner: TCoreActionList;
    State: TCoreActionStates;
    ID: TCoreActionID;
    Desc: TCoreActionString;

    constructor Create(Owner_: TCoreActionList); virtual;
    destructor Destroy; override;

    procedure Run(); virtual;
    procedure Over(); virtual;
    procedure Stop(); virtual;
    procedure Pause(); virtual;
    procedure Progress(deltaTime: Double); virtual;
  end;

  TCoreActionClass = class of TCoreAction;

  TCoreActionList = class(TCoreClassObject)
  protected
    FSequenceList: TCoreClassListForObj;
    FFocusIndex: Integer;
    FLast: TCoreAction;
  public
    Owner: TCoreActionLinear;
    constructor Create(Owner_: TCoreActionLinear);
    destructor Destroy; override;
    procedure Clear;
    function Add(ActionClass_: TCoreActionClass): TCoreAction; overload;
    procedure Run();
    procedure Over();
    procedure Stop();
    function IsOver(): Boolean;
    function IsStop(): Boolean;
    property Last: TCoreAction read FLast;
    procedure Progress(deltaTime: Double);
  end;

  TCoreActionLinear = class(TCoreClassObject)
  protected
    FSequenceList: TCoreClassListForObj;
    FFocusIndex: Integer;
    FLast: TCoreActionList;
  public
    constructor Create();
    destructor Destroy; override;
    procedure Clear;
    function Add: TCoreActionList;
    procedure Run();
    procedure Stop();
    procedure Over();
    property Last: TCoreActionList read FLast;
    procedure Progress(deltaTime: Double);

    class procedure Test();
  end;

implementation

constructor TCoreAction.Create(Owner_: TCoreActionList);
begin
  inherited Create;
  Owner := Owner_;
  State := [];
  ID := 0;
  Desc := '';
end;

destructor TCoreAction.Destroy;
begin
  inherited Destroy;
end;

procedure TCoreAction.Run;
begin
  State := [asPlaying];
end;

procedure TCoreAction.Over;
begin
  if asPlaying in State then
      State := [asOver];
end;

procedure TCoreAction.Stop;
begin
  if asPlaying in State then
      State := [asStop];
end;

procedure TCoreAction.Pause;
begin
  if asPlaying in State then
      State := [asPlaying, asPause];
end;

procedure TCoreAction.Progress(deltaTime: Double);
begin

end;

constructor TCoreActionList.Create(Owner_: TCoreActionLinear);
begin
  inherited Create;
  FSequenceList := TCoreClassListForObj.Create;
  FFocusIndex := -1;
  FLast := nil;
  Owner := Owner_;
end;

destructor TCoreActionList.Destroy;
begin
  Clear;
  DisposeObject(FSequenceList);
  inherited Destroy;
end;

procedure TCoreActionList.Clear;
var
  i: Integer;
begin
  for i := FSequenceList.Count - 1 downto 0 do
      DisposeObject(FSequenceList[i]);
  FSequenceList.Clear;
end;

function TCoreActionList.Add(ActionClass_: TCoreActionClass): TCoreAction;
begin
  Result := ActionClass_.Create(Self);
  FSequenceList.Add(Result);
end;

procedure TCoreActionList.Run();
begin
  if FSequenceList.Count > 0 then
    begin
      FFocusIndex := 0;
      FLast := FSequenceList[FFocusIndex] as TCoreAction;
    end
  else
    begin
      FFocusIndex := -1;
      FLast := nil;
    end;
end;

procedure TCoreActionList.Over;
begin
  if FLast <> nil then
      FFocusIndex := FSequenceList.Count;
end;

procedure TCoreActionList.Stop;
begin
  if FLast <> nil then
      FFocusIndex := -1;
end;

function TCoreActionList.IsOver: Boolean;
begin
  Result := FFocusIndex >= FSequenceList.Count;
end;

function TCoreActionList.IsStop: Boolean;
begin
  Result := FFocusIndex < 0;
end;

procedure TCoreActionList.Progress(deltaTime: Double);
begin
  if (FFocusIndex < 0) or (FFocusIndex >= FSequenceList.Count) then
      Exit;

  FLast := FSequenceList[FFocusIndex] as TCoreAction;

  if FLast.State = [] then
    begin
      FLast.Run;
      Exit;
    end;

  if asPlaying in FLast.State then
    begin
      FLast.Progress(deltaTime);
      Exit;
    end;

  if asStop in FLast.State then
    begin
      FFocusIndex := -1;
      if Owner <> nil then
          Owner.Stop;
      Exit;
    end;

  if asOver in FLast.State then
    begin
      inc(FFocusIndex);
      if (FFocusIndex >= FSequenceList.Count) and (Owner <> nil) then
          Owner.Over;
      Exit;
    end;
end;

constructor TCoreActionLinear.Create();
begin
  inherited Create;
  FSequenceList := TCoreClassListForObj.Create;
  FFocusIndex := -1;
  FLast := nil;
end;

destructor TCoreActionLinear.Destroy;
begin
  Clear;
  DisposeObject(FSequenceList);
  inherited Destroy;
end;

procedure TCoreActionLinear.Clear;
var
  i: Integer;
begin
  for i := FSequenceList.Count - 1 downto 0 do
      DisposeObject(FSequenceList[i]);
  FSequenceList.Clear;
  FFocusIndex := -1;
  FLast := nil;
end;

function TCoreActionLinear.Add: TCoreActionList;
begin
  Result := TCoreActionList.Create(Self);
  FSequenceList.Add(Result);
end;

procedure TCoreActionLinear.Run;
begin
  if FSequenceList.Count > 0 then
    begin
      FFocusIndex := 0;
      FLast := FSequenceList[FFocusIndex] as TCoreActionList;
    end
  else
    begin
      FFocusIndex := -1;
      FLast := nil;
    end;
end;

procedure TCoreActionLinear.Stop;
begin
  Clear;
end;

procedure TCoreActionLinear.Over;
begin
  inc(FFocusIndex);
  if FFocusIndex < FSequenceList.Count then
    begin
      FLast := FSequenceList[FFocusIndex] as TCoreActionList;
    end
  else
    begin
      Clear;
    end;
end;

procedure TCoreActionLinear.Progress(deltaTime: Double);
begin
  if FLast <> nil then
      FLast.Progress(deltaTime);
end;

class procedure TCoreActionLinear.Test();
var
  al: TCoreActionList;
  i: Integer;
begin
  al := TCoreActionList.Create(nil);
  for i := 1 to 2 do
    with al.Add(TCoreAction) do
      begin
        ID := i;
        Desc := PFormat('description %d', [i]);
      end;
  al.Run;
  while True do
    begin
      al.Progress(0.1);
      al.Last.Over;
      if al.IsOver or al.IsStop then
          Break;
    end;

  DisposeObject(al);
end;

end.
