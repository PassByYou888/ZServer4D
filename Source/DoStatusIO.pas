{******************************************************************************}
{* Status Library, writen by QQ 600585@qq.com                                 *}
{* https://github.com/PassByYou888/CoreCipher                                 *}
(* https://github.com/PassByYou888/ZServer4D                                  *)
{******************************************************************************}

unit DoStatusIO;

{$I zDefine.inc}

interface

uses
  {$IF Defined(WIN32) or Defined(WIN64)}
  Windows,
  {$ELSE}
  FMX.Types,
  {$ENDIF}
  Sysutils, PascalStrings, CoreClasses, MemoryStream64;

type
  TDoStatusNear = procedure(AText: SystemString; const ID: Integer) of object;
  TDoStatusFar  = procedure(AText: SystemString; const ID: Integer);

procedure DoStatus(Text: SystemString; ID: Integer); overload;
procedure AddDoStatusHook(FlagObj: TCoreClassObject; CallProc: TDoStatusNear); overload;
procedure AddDoStatusHook(FlagObj: TCoreClassObject; CallProc: TDoStatusFar); overload;
procedure DeleteDoStatusHook(FlagObj: TCoreClassObject);
procedure DisableStatus;
procedure EnabledStatus;

procedure DoStatus(v: TMemoryStream64); overload;
procedure DoStatus(v: TCoreClassStrings); overload;
procedure DoStatus(v: Integer); overload;
procedure DoStatus(v: Single); overload;
procedure DoStatus(v: Double); overload;
procedure DoStatus(v: Pointer); overload;
procedure DoStatus(v: SystemString; const Args: array of const); overload;
procedure DoError(v: SystemString; const Args: array of const); overload;
procedure DoStatus(v: SystemString); overload;

var
  LastDoStatus : SystemString;
  IDEOutput    : Boolean;
  ConsoleOutput: Boolean;

implementation

procedure DoStatus(v: TMemoryStream64);
var
  p: PByte;
  i: Integer;
  n: SystemString;
begin
  p := v.Memory;
  for i := 0 to v.size - 1 do
    begin
      if n <> '' then
          n := n + ',' + IntToStr(p^)
      else
          n := IntToStr(p^);
      inc(p);
    end;
  DoStatus(IntToHex(NativeInt(v), SizeOf(Pointer)) + ':' + n);
end;

procedure DoStatus(v: TCoreClassStrings);
var
  i: Integer;
begin
  for i := 0 to v.Count - 1 do
      DoStatus(v[i]);
end;

procedure DoStatus(v: Integer);
begin
  DoStatus(IntToStr(v));
end;

procedure DoStatus(v: Single);
begin
  DoStatus(FloatToStr(v));
end;

procedure DoStatus(v: Double);
begin
  DoStatus(FloatToStr(v));
end;

procedure DoStatus(v: Pointer);
begin
  DoStatus(Format('0x%p', [v]));
end;

procedure DoStatus(v: SystemString; const Args: array of const);
begin
  DoStatus(Format(v, Args));
end;

procedure DoError(v: SystemString; const Args: array of const);
begin
  DoStatus(Format(v, Args), 2);
end;

procedure DoStatus(v: SystemString);
begin
  DoStatus(v, 0);
end;

type
  TDoStatusData = record
    FlagObj: TCoreClassObject;
    OnStatusNear: TDoStatusNear;
    OnStatusFar: TDoStatusFar;
  end;

  PDoStatusData = ^TDoStatusData;

var
  HookDoSatus : TCoreClassList = nil;
  StatusActive: Boolean        = True;

procedure DoStatus(Text: SystemString; ID: Integer);
var
  Rep_Int: Integer;
  p      : PDoStatusData;
begin
  try
    try
      if (StatusActive) and (HookDoSatus.Count > 0) then
        begin
          LastDoStatus := Text;
          for Rep_Int := HookDoSatus.Count - 1 downto 0 do
            begin
              p := HookDoSatus[Rep_Int];
              try
                if Assigned(p^.OnStatusNear) then
                    p^.OnStatusNear(Text, ID)
                else if Assigned(p^.OnStatusFar) then
                    p^.OnStatusFar(Text, ID);
              except
              end;
            end;
        end;
    except
    end;

    {$IFNDEF FPC}
    if ((IDEOutput) or (ID = 2)) and (DebugHook <> 0) then
      begin
        {$IF Defined(WIN32) or Defined(WIN64)}
        OutputDebugString(PWideChar(Text));
        {$ELSE}
        FMX.Types.Log.d(Text);
        {$ENDIF}
      end;
    {$ENDIF}
    if ((ConsoleOutput) or (ID = 2)) and (IsConsole) then
        Writeln(Text);
  finally
  end;
end;

procedure AddDoStatusHook(FlagObj: TCoreClassObject; CallProc: TDoStatusNear);
var
  _Data: PDoStatusData;
begin
  New(_Data);
  _Data^.FlagObj := FlagObj;
  _Data^.OnStatusNear := CallProc;
  _Data^.OnStatusFar := nil;
  HookDoSatus.Add(_Data);
end;

procedure AddDoStatusHook(FlagObj: TCoreClassObject; CallProc: TDoStatusFar);
var
  _Data: PDoStatusData;
begin
  New(_Data);
  _Data^.FlagObj := FlagObj;
  _Data^.OnStatusNear := nil;
  _Data^.OnStatusFar := CallProc;
  HookDoSatus.Add(_Data);
end;

procedure DeleteDoStatusHook(FlagObj: TCoreClassObject);
var
  Rep_Int: Integer;
  p      : PDoStatusData;
begin
  Rep_Int := 0;
  while Rep_Int < HookDoSatus.Count do
    begin
      p := HookDoSatus[Rep_Int];
      if p^.FlagObj = FlagObj then
        begin
          Dispose(p);
          HookDoSatus.Delete(Rep_Int);
        end
      else
          inc(Rep_Int);
    end;
end;

procedure DisableStatus;
begin
  StatusActive := False;
end;

procedure EnabledStatus;
begin
  StatusActive := True;
end;

initialization

HookDoSatus := TCoreClassList.Create;
StatusActive := True;
LastDoStatus := '';
IDEOutput := False;
ConsoleOutput := True;

finalization

while HookDoSatus.Count > 0 do
  begin
    Dispose(PDoStatusData(HookDoSatus[0]));
    HookDoSatus.Delete(0);
  end;
DisposeObject(HookDoSatus);

end.
