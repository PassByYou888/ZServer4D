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
  TCoreActionString = U_String;
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

    constructor Create(AOwner: TCoreActionList); virtual;
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
    FList: TCoreClassListForObj;
    FWorkIndex: Integer;
    FLast: TCoreAction;
  public
    Owner: TCoreActionLinear;
    constructor Create(AOwner: TCoreActionLinear); virtual;
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
    FList: TCoreClassListForObj;
    FWorkIndex: Integer;
    FLast: TCoreActionList;
  public
    Owner: TCoreClassObject;
    constructor Create(AOwner: TCoreClassObject); virtual;
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

constructor TCoreAction.Create(AOwner: TCoreActionList);
begin
  inherited Create;
  Owner := AOwner;
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

constructor TCoreActionList.Create(AOwner: TCoreActionLinear);
begin
  inherited Create;
  FList := TCoreClassListForObj.Create;
  FWorkIndex := -1;
  FLast := nil;
  Owner := AOwner;
end;

destructor TCoreActionList.Destroy;
begin
  Clear;
  DisposeObject(FList);
  inherited Destroy;
end;

procedure TCoreActionList.Clear;
var
  i: Integer;
begin
  for i := FList.Count - 1 downto 0 do
      DisposeObject(FList[i]);
  FList.Clear;
end;

function TCoreActionList.Add(ActionClass_: TCoreActionClass): TCoreAction;
begin
  Result := ActionClass_.Create(Self);
  FList.Add(Result);
end;

procedure TCoreActionList.Run();
begin
  if FList.Count > 0 then
    begin
      FWorkIndex := 0;
      FLast := FList[FWorkIndex] as TCoreAction;
    end
  else
    begin
      FWorkIndex := -1;
      FLast := nil;
    end;
end;

procedure TCoreActionList.Over;
begin
  if FLast <> nil then
      FWorkIndex := FList.Count;
end;

procedure TCoreActionList.Stop;
begin
  if FLast <> nil then
      FWorkIndex := -1;
end;

function TCoreActionList.IsOver: Boolean;
begin
  Result := FWorkIndex >= FList.Count;
end;

function TCoreActionList.IsStop: Boolean;
begin
  Result := FWorkIndex < 0;
end;

procedure TCoreActionList.Progress(deltaTime: Double);
begin
  if (FWorkIndex < 0) or (FWorkIndex >= FList.Count) then
      Exit;

  FLast := FList[FWorkIndex] as TCoreAction;

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
      FWorkIndex := -1;
      if Owner <> nil then
          Owner.Stop;
      Exit;
    end;

  if asOver in FLast.State then
    begin
      inc(FWorkIndex);
      if (FWorkIndex >= FList.Count) and (Owner <> nil) then
          Owner.Over;
      Exit;
    end;
end;

constructor TCoreActionLinear.Create(AOwner: TCoreClassObject);
begin
  inherited Create;
  FList := TCoreClassListForObj.Create;
  FWorkIndex := -1;
  FLast := nil;
  Owner := AOwner;
end;

destructor TCoreActionLinear.Destroy;
begin
  Clear;
  DisposeObject(FList);
  inherited Destroy;
end;

procedure TCoreActionLinear.Clear;
var
  i: Integer;
begin
  for i := FList.Count - 1 downto 0 do
      DisposeObject(FList[i]);
  FList.Clear;
  FWorkIndex := -1;
  FLast := nil;
end;

function TCoreActionLinear.Add: TCoreActionList;
begin
  Result := TCoreActionList.Create(Self);
  FList.Add(Result);
end;

procedure TCoreActionLinear.Run;
begin
  if FList.Count > 0 then
    begin
      FWorkIndex := 0;
      FLast := FList[FWorkIndex] as TCoreActionList;
    end
  else
    begin
      FWorkIndex := -1;
      FLast := nil;
    end;
end;

procedure TCoreActionLinear.Stop;
begin
  Clear;
end;

procedure TCoreActionLinear.Over;
begin
  inc(FWorkIndex);
  if FWorkIndex < FList.Count then
    begin
      FLast := FList[FWorkIndex] as TCoreActionList;
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
