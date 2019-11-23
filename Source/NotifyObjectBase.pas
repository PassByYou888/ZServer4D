{ ****************************************************************************** }
{ * Delay trigger              by qq600585                                     * }
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
(*
  update history
*)

unit NotifyObjectBase;

{$INCLUDE zDefine.inc}

interface

uses Variants, CoreClasses, DataFrameEngine, Cadencer;

type
  TNotifyBase = class(TCoreClassInterfacedObject)
  protected
    FNotifyList: TCoreClassListForObj;
    FSaveRegisted: TCoreClassListForObj;
    procedure DeleteSaveNotifyIntf(p: TNotifyBase);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure RegisterNotify(v: TNotifyBase);
    procedure UnRegisterNotify(v: TNotifyBase);

    // trigger
    procedure DoExecute(const State: Variant); virtual;

    // on execute
    procedure NotifyExecute(Sender: TNotifyBase; const State: Variant); virtual;
  end;

  TNProgressPost = class;
  TNPostExecute = class;

  TNPostExecuteCall = procedure(Sender: TNPostExecute);
  TNPostExecuteCall_NP = procedure();
  TNPostExecuteMethod = procedure(Sender: TNPostExecute) of object;
  TNPostExecuteMethod_NP = procedure() of object;
{$IFDEF FPC}
  TNPostExecuteProc = procedure(Sender: TNPostExecute) is nested;
  TNPostExecuteProc_NP = procedure() is nested;
{$ELSE FPC}
  TNPostExecuteProc = reference to procedure(Sender: TNPostExecute);
  TNPostExecuteProc_NP = reference to procedure();
{$ENDIF FPC}

  TNPostExecute = class(TCoreClassObject)
  private
    FOwner: TNProgressPost;
    FDataEng: TDataFrameEngine;
    ProcessedTime: Double;
  public
    Data1: TCoreClassObject;
    Data2: TCoreClassObject;
    Data3: Variant;
    Data4: Variant;
    Data5: Pointer;
    Delay: Double;

    OnExecuteCall: TNPostExecuteCall;
    OnExecuteCall_NP: TNPostExecuteCall_NP;
    OnExecuteMethod: TNPostExecuteMethod;
    OnExecuteMethod_NP: TNPostExecuteMethod_NP;
    OnExecuteProc: TNPostExecuteProc;
    OnExecuteProc_NP: TNPostExecuteProc_NP;
    property DataEng: TDataFrameEngine read FDataEng;
    property Owner: TNProgressPost read FOwner;

    constructor Create; virtual;
    destructor Destroy; override;
    procedure Execute; virtual;
  end;

  TNPostExecuteClass = class of TNPostExecute;

  TNProgressPost = class(TCoreClassInterfacedObject)
  protected
    FPostProcessIsRun: Boolean;
    FPostExecuteList: TCoreClassListForObj;
    FPostClass: TNPostExecuteClass;
    FBusy: Boolean;
    FCurrentExecute: TNPostExecute;
    FBreakProgress: Boolean;
    FPaused: Boolean;
    Critical: TCritical;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ResetPost;

    function PostExecute(): TNPostExecute; overload;

    function PostExecute(DataEng: TDataFrameEngine): TNPostExecute; overload;
    function PostExecute(Delay: Double): TNPostExecute; overload;
    function PostExecute(Delay: Double; DataEng: TDataFrameEngine): TNPostExecute; overload;

    function PostExecuteM(DataEng: TDataFrameEngine; OnExecuteMethod: TNPostExecuteMethod): TNPostExecute; overload;
    function PostExecuteM(Delay: Double; DataEng: TDataFrameEngine; OnExecuteMethod: TNPostExecuteMethod): TNPostExecute; overload;
    function PostExecuteM(Delay: Double; OnExecuteMethod: TNPostExecuteMethod): TNPostExecute; overload;
    function PostExecuteM_NP(Delay: Double; OnExecuteMethod: TNPostExecuteMethod_NP): TNPostExecute; overload;

    function PostExecuteC(DataEng: TDataFrameEngine; OnExecuteCall: TNPostExecuteCall): TNPostExecute; overload;
    function PostExecuteC(Delay: Double; DataEng: TDataFrameEngine; OnExecuteCall: TNPostExecuteCall): TNPostExecute; overload;
    function PostExecuteC(Delay: Double; OnExecuteCall: TNPostExecuteCall): TNPostExecute; overload;
    function PostExecuteC_NP(Delay: Double; OnExecuteCall: TNPostExecuteCall_NP): TNPostExecute; overload;

    function PostExecuteP(DataEng: TDataFrameEngine; OnExecuteProc: TNPostExecuteProc): TNPostExecute; overload;
    function PostExecuteP(Delay: Double; DataEng: TDataFrameEngine; OnExecuteProc: TNPostExecuteProc): TNPostExecute; overload;
    function PostExecuteP(Delay: Double; OnExecuteProc: TNPostExecuteProc): TNPostExecute; overload;
    function PostExecuteP_NP(Delay: Double; OnExecuteProc: TNPostExecuteProc_NP): TNPostExecute; overload;

    procedure Delete(p: TNPostExecute); overload; virtual;

    procedure Progress(deltaTime: Double);

    property Paused: Boolean read FPaused write FPaused;
    property Busy: Boolean read FBusy;
    property CurrentExecute: TNPostExecute read FCurrentExecute;
    property PostClass: TNPostExecuteClass read FPostClass write FPostClass;
  end;

  TNProgressPostWithCadencer = class(TNProgressPost, ICadencerProgressInterface)
  protected
    FCadencerEngine: TCadencer;
    procedure CadencerProgress(const deltaTime, newTime: Double);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Progress;

    property CadencerEngine: TCadencer read FCadencerEngine;
  end;

implementation


procedure TNotifyBase.DeleteSaveNotifyIntf(p: TNotifyBase);
var
  i: Integer;
begin
  i := 0;
  while i < FSaveRegisted.Count do
    begin
      if FSaveRegisted[i] = TNotifyBase(p) then
          FSaveRegisted.Delete(i)
      else
          inc(i);
    end;
end;

constructor TNotifyBase.Create;
begin
  inherited Create;
  FNotifyList := TCoreClassListForObj.Create;
  FSaveRegisted := TCoreClassListForObj.Create;
end;

destructor TNotifyBase.Destroy;
begin
  while FSaveRegisted.Count > 0 do
      TNotifyBase(FSaveRegisted[0]).UnRegisterNotify(Self);

  DisposeObject(FSaveRegisted);

  while FNotifyList.Count > 0 do
      UnRegisterNotify(TNotifyBase(FNotifyList[0]));

  DisposeObject(FNotifyList);
  inherited Destroy;
end;

procedure TNotifyBase.RegisterNotify(v: TNotifyBase);
begin
  UnRegisterNotify(v);
  FNotifyList.Add(v);
  v.FSaveRegisted.Add(Self);
end;

procedure TNotifyBase.UnRegisterNotify(v: TNotifyBase);
var
  i: Integer;
begin
  i := 0;
  while i < FNotifyList.Count do
    begin
      if FNotifyList[i] = TNotifyBase(v) then
          FNotifyList.Delete(i)
      else
          inc(i);
    end;
  v.DeleteSaveNotifyIntf(Self);
end;

procedure TNotifyBase.DoExecute(const State: Variant);
var
  i: Integer;
begin
  i := 0;
  while i < FNotifyList.Count do
    begin
      try
          TNotifyBase(FNotifyList[i]).NotifyExecute(Self, State);
      except
      end;
      inc(i);
    end;
end;

procedure TNotifyBase.NotifyExecute(Sender: TNotifyBase; const State: Variant);
begin

end;

constructor TNPostExecute.Create;
begin
  inherited Create;
  FDataEng := TDataFrameEngine.Create;
  ProcessedTime := 0;
  Data1 := nil;
  Data2 := nil;
  Data3 := Null;
  Data4 := Null;
  Data5 := nil;
  Delay := 0;

  OnExecuteCall := nil;
  OnExecuteCall_NP := nil;
  OnExecuteMethod := nil;
  OnExecuteMethod_NP := nil;
  OnExecuteProc := nil;
  OnExecuteProc_NP := nil;
end;

destructor TNPostExecute.Destroy;
var
  i: Integer;
begin
  if FOwner <> nil then
    begin
      if FOwner.CurrentExecute = Self then
          FOwner.FBreakProgress := True;

      i := 0;
      while i < FOwner.FPostExecuteList.Count do
        begin
          if FOwner.FPostExecuteList[i] = Self then
              FOwner.FPostExecuteList.Delete(i)
          else
              inc(i);
        end;
      FOwner := nil;
    end;
  DisposeObject(FDataEng);
  inherited Destroy;
end;

procedure TNPostExecute.Execute;
begin
  if Assigned(OnExecuteCall) then
    begin
      FDataEng.Reader.index := 0;
      try
          OnExecuteCall(Self);
      except
      end;
    end;

  if Assigned(OnExecuteCall_NP) then
    begin
      FDataEng.Reader.index := 0;
      try
          OnExecuteCall_NP();
      except
      end;
    end;

  if Assigned(OnExecuteMethod) then
    begin
      FDataEng.Reader.index := 0;
      try
          OnExecuteMethod(Self);
      except
      end;
    end;

  if Assigned(OnExecuteMethod_NP) then
    begin
      FDataEng.Reader.index := 0;
      try
          OnExecuteMethod_NP();
      except
      end;
    end;

  if Assigned(OnExecuteProc) then
    begin
      FDataEng.Reader.index := 0;
      try
          OnExecuteProc(Self);
      except
      end;
    end;
  if Assigned(OnExecuteProc_NP) then
    begin
      FDataEng.Reader.index := 0;
      try
          OnExecuteProc_NP();
      except
      end;
    end;
end;

constructor TNProgressPost.Create;
begin
  inherited Create;
  FPostProcessIsRun := False;
  FPostExecuteList := TCoreClassListForObj.Create;
  FPostClass := TNPostExecute;
  FBusy := False;
  FCurrentExecute := nil;
  FBreakProgress := False;
  FPaused := False;
  Critical := TCritical.Create;
end;

destructor TNProgressPost.Destroy;
begin
  ResetPost;
  DisposeObject(FPostExecuteList);
  DisposeObject(Critical);
  inherited Destroy;
end;

procedure TNProgressPost.ResetPost;
var
  i: Integer;
begin
  Critical.Acquire; // atom
  try
    try
      for i := 0 to FPostExecuteList.Count - 1 do
        begin
          TNPostExecute(FPostExecuteList[i]).FOwner := nil;
          DisposeObject(FPostExecuteList[i]);
        end;

      FPostExecuteList.Clear;
    except
    end;
  finally
      Critical.Release; // atom
  end;
  FBreakProgress := True;
end;

function TNProgressPost.PostExecute(): TNPostExecute;
begin
  Result := FPostClass.Create;
  Result.FOwner := Self;
  Critical.Acquire; // atom
  try
      FPostExecuteList.Add(Result);
  finally
      Critical.Release; // atom
  end;
end;

function TNProgressPost.PostExecute(DataEng: TDataFrameEngine): TNPostExecute;
begin
  Result := PostExecute();
  if DataEng <> nil then
      Result.FDataEng.Assign(DataEng);
end;

function TNProgressPost.PostExecute(Delay: Double): TNPostExecute;
begin
  Result := PostExecute();
  Result.Delay := Delay;
end;

function TNProgressPost.PostExecute(Delay: Double; DataEng: TDataFrameEngine): TNPostExecute;
begin
  Result := PostExecute(Delay);
  if DataEng <> nil then
      Result.FDataEng.Assign(DataEng);
end;

function TNProgressPost.PostExecuteM(DataEng: TDataFrameEngine; OnExecuteMethod: TNPostExecuteMethod): TNPostExecute;
begin
  Result := PostExecute(DataEng);
  Result.OnExecuteMethod := OnExecuteMethod;
end;

function TNProgressPost.PostExecuteM(Delay: Double; DataEng: TDataFrameEngine; OnExecuteMethod: TNPostExecuteMethod): TNPostExecute;
begin
  Result := PostExecute(Delay, DataEng);
  Result.OnExecuteMethod := OnExecuteMethod;
end;

function TNProgressPost.PostExecuteM(Delay: Double; OnExecuteMethod: TNPostExecuteMethod): TNPostExecute;
begin
  Result := PostExecute(Delay);
  Result.OnExecuteMethod := OnExecuteMethod;
end;

function TNProgressPost.PostExecuteM_NP(Delay: Double; OnExecuteMethod: TNPostExecuteMethod_NP): TNPostExecute;
begin
  Result := PostExecute(Delay);
  Result.OnExecuteMethod_NP := OnExecuteMethod;
end;

function TNProgressPost.PostExecuteC(DataEng: TDataFrameEngine; OnExecuteCall: TNPostExecuteCall): TNPostExecute;
begin
  Result := PostExecute(DataEng);
  Result.OnExecuteCall := OnExecuteCall;
end;

function TNProgressPost.PostExecuteC(Delay: Double; DataEng: TDataFrameEngine; OnExecuteCall: TNPostExecuteCall): TNPostExecute;
begin
  Result := PostExecute(Delay, DataEng);
  Result.OnExecuteCall := OnExecuteCall;
end;

function TNProgressPost.PostExecuteC(Delay: Double; OnExecuteCall: TNPostExecuteCall): TNPostExecute;
begin
  Result := PostExecute(Delay);
  Result.OnExecuteCall := OnExecuteCall;
end;

function TNProgressPost.PostExecuteC_NP(Delay: Double; OnExecuteCall: TNPostExecuteCall_NP): TNPostExecute;
begin
  Result := PostExecute(Delay);
  Result.OnExecuteCall_NP := OnExecuteCall;
end;

function TNProgressPost.PostExecuteP(DataEng: TDataFrameEngine; OnExecuteProc: TNPostExecuteProc): TNPostExecute;
begin
  Result := PostExecute(DataEng);
  Result.OnExecuteProc := OnExecuteProc;
end;

function TNProgressPost.PostExecuteP(Delay: Double; DataEng: TDataFrameEngine; OnExecuteProc: TNPostExecuteProc): TNPostExecute;
begin
  Result := PostExecute(Delay, DataEng);
  Result.OnExecuteProc := OnExecuteProc;
end;

function TNProgressPost.PostExecuteP(Delay: Double; OnExecuteProc: TNPostExecuteProc): TNPostExecute;
begin
  Result := PostExecute(Delay);
  Result.OnExecuteProc := OnExecuteProc;
end;

function TNProgressPost.PostExecuteP_NP(Delay: Double; OnExecuteProc: TNPostExecuteProc_NP): TNPostExecute;
begin
  Result := PostExecute(Delay);
  Result.OnExecuteProc_NP := OnExecuteProc;
end;

procedure TNProgressPost.Delete(p: TNPostExecute);
var
  i: Integer;
begin
  Critical.Acquire; // atom
  try
    i := 0;
    while i < FPostExecuteList.Count do
      begin
        if FPostExecuteList[i] = p then
          begin
            TNPostExecute(FPostExecuteList[i]).FOwner := nil;
            DisposeObject(FPostExecuteList[i]);
            FPostExecuteList.Delete(i);
          end
        else
            inc(i);
      end;
  finally
      Critical.Release; // atom
  end;
end;

procedure TNProgressPost.Progress(deltaTime: Double);
var
  i: Integer;
  L: TCoreClassListForObj;
  p: TNPostExecute;
begin
  if FPaused then
      Exit;
  if FPostProcessIsRun then
      Exit;

  FPostProcessIsRun := True;
  FBreakProgress := False;

  L := TCoreClassListForObj.Create;

  Critical.Acquire; // atom
  i := 0;
  try
    while i < FPostExecuteList.Count do
      begin
        p := FPostExecuteList[i] as TNPostExecute;
        p.ProcessedTime := p.ProcessedTime + deltaTime;
        if p.ProcessedTime >= p.Delay then
          begin
            L.Add(p);
            FPostExecuteList.Delete(i);
          end
        else
            inc(i);
      end;
  finally
      Critical.Release; // atom
  end;

  i := 0;
  while (i < L.Count) do
    begin
      FBusy := True;
      FCurrentExecute := TNPostExecute(L[i]);
      try
          FCurrentExecute.Execute;
      except
      end;
      FBusy := False;

      FCurrentExecute.FOwner := nil;

      try
          DisposeObject(FCurrentExecute);
      except
      end;

      inc(i);
      if FBreakProgress then
          Break;
    end;

  DisposeObject(L);

  FPostProcessIsRun := False;
end;

procedure TNProgressPostWithCadencer.CadencerProgress(const deltaTime, newTime: Double);
begin
  inherited Progress(deltaTime);
end;

constructor TNProgressPostWithCadencer.Create;
begin
  inherited Create;
  FCadencerEngine := TCadencer.Create;
  FCadencerEngine.ProgressInterface := Self;
end;

destructor TNProgressPostWithCadencer.Destroy;
begin
  DisposeObject(FCadencerEngine);
  inherited Destroy;
end;

procedure TNProgressPostWithCadencer.Progress;
begin
  FCadencerEngine.Progress;
end;

end.
