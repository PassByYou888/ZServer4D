{ ****************************************************************************** }
{ * Status IO          writen by QQ 600585@qq.com                              * }
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

unit DoStatusIO;

{$INCLUDE zDefine.inc}

interface

uses
{$IFNDEF FPC}
{$IF Defined(WIN32) or Defined(WIN64)}
  Windows,
{$ELSEIF not Defined(Linux)}
  FMX.Types,
{$IFEND}
{$IFEND FPC}
  SysUtils, Classes, SyncObjs,
{$IFDEF FPC}
  FPCGenericStructlist, fgl,
{$ELSE FPC}
  System.Generics.Collections,
{$ENDIF FPC}
  PascalStrings, UPascalStrings, UnicodeMixedLib, CoreClasses;

type
  TDoStatusMethod = procedure(Text_: SystemString; const ID: Integer) of object;
  TDoStatusCall = procedure(Text_: SystemString; const ID: Integer);

procedure AddDoStatusHook(TokenObj: TCoreClassObject; CallProc: TDoStatusMethod);
procedure AddDoStatusHookM(TokenObj: TCoreClassObject; CallProc: TDoStatusMethod);
procedure AddDoStatusHookC(TokenObj: TCoreClassObject; CallProc: TDoStatusCall);
procedure DeleteDoStatusHook(TokenObj: TCoreClassObject);
procedure DisableStatus;
procedure EnabledStatus;

procedure DoStatus(Text_: SystemString; const ID: Integer); overload;
procedure DoStatus(const v: Pointer; siz, width: NativeInt); overload;
procedure DoStatus(prefix: SystemString; v: Pointer; siz, width: NativeInt); overload;
procedure DoStatus(const v: TCoreClassStrings); overload;
procedure DoStatus(const v: Int64); overload;
procedure DoStatus(const v: Integer); overload;
procedure DoStatus(const v: Single); overload;
procedure DoStatus(const v: Double); overload;
procedure DoStatus(const v: Pointer); overload;
procedure DoStatus(const v: SystemString; const Args: array of const); overload;
procedure DoError(v: SystemString; const Args: array of const); overload;
procedure DoStatus(const v: SystemString); overload;
procedure DoStatus(const v: TPascalString); overload;
procedure DoStatus(const v: TUPascalString); overload;
procedure DoStatus(const v: TMD5); overload;
procedure DoStatus; overload;

procedure DoStatusNoLn(const v: TPascalString); overload;
procedure DoStatusNoLn(const v: SystemString; const Args: array of const); overload;
procedure DoStatusNoLn; overload;

function StrInfo(s: TPascalString): string; overload;
function StrInfo(s: TUPascalString): string; overload;
function BytesInfo(s: TBytes): string; overload;

var
  LastDoStatus: SystemString;
  IDEOutput: Boolean;
  ConsoleOutput: Boolean;
  OnDoStatusHook: TDoStatusCall;

implementation

procedure bufHashToString(hash: Pointer; Size: NativeInt; var output: TPascalString);
const
  HexArr: array [0 .. 15] of SystemChar = ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
var
  i: Integer;
begin
  output.Len := Size * 2;
  for i := 0 to Size - 1 do
    begin
      output.buff[i * 2] := HexArr[(PByte(nativeUInt(hash) + i)^ shr 4) and $0F];
      output.buff[i * 2 + 1] := HexArr[PByte(nativeUInt(hash) + i)^ and $0F];
    end;
end;

procedure DoStatus(Text_: SystemString; const ID: Integer);
begin
  try
      OnDoStatusHook(Text_, ID);
  except
  end;
end;

procedure DoStatus(const v: Pointer; siz, width: NativeInt);
var
  s: TPascalString;
  i: Integer;
  n: SystemString;
begin
  bufHashToString(v, siz, s);
  n := '';
  for i := 1 to s.Len div 2 do
    begin
      if n <> '' then
          n := n + #32 + s[i * 2 - 1] + s[i * 2]
      else
          n := s[i * 2 - 1] + s[i * 2];

      if i mod (width div 2) = 0 then
        begin
          DoStatus(n);
          n := '';
        end;
    end;
  if n <> '' then
      DoStatus(n);
end;

procedure DoStatus(prefix: SystemString; v: Pointer; siz, width: NativeInt);
var
  s: TPascalString;
  i: Integer;
  n: SystemString;
begin
  bufHashToString(v, siz, s);
  n := '';
  for i := 1 to s.Len div 2 do
    begin
      if n <> '' then
          n := n + #32 + s[i * 2 - 1] + s[i * 2]
      else
          n := s[i * 2 - 1] + s[i * 2];

      if i mod (width div 2) = 0 then
        begin
          DoStatus(prefix + n);
          n := '';
        end;
    end;
  if n <> '' then
      DoStatus(prefix + n);
end;

procedure DoStatus(const v: TCoreClassStrings);
var
  i: Integer;
  o: TCoreClassObject;
begin
  for i := 0 to v.Count - 1 do
    begin
      o := v.Objects[i];
      if o <> nil then
          DoStatus('%s<%s>', [v[i], o.ClassName])
      else
          DoStatus(v[i]);
    end;
end;

procedure DoStatus(const v: Int64);
begin
  DoStatus(IntToStr(v));
end;

procedure DoStatus(const v: Integer);
begin
  DoStatus(IntToStr(v));
end;

procedure DoStatus(const v: Single);
begin
  DoStatus(FloatToStr(v));
end;

procedure DoStatus(const v: Double);
begin
  DoStatus(FloatToStr(v));
end;

procedure DoStatus(const v: Pointer);
begin
  DoStatus(Format('0x%p', [v]));
end;

procedure DoStatus(const v: SystemString; const Args: array of const);
begin
  DoStatus(Format(v, Args));
end;

procedure DoError(v: SystemString; const Args: array of const);
begin
  DoStatus(Format(v, Args), 2);
end;

procedure DoStatus(const v: SystemString);
begin
  DoStatus(v, 0);
end;

procedure DoStatus(const v: TPascalString);
begin
  DoStatus(v.Text, 0);
end;

procedure DoStatus(const v: TUPascalString);
begin
  DoStatus(v.Text, 0);
end;

procedure DoStatus(const v: TMD5);
begin
  DoStatus(umlMD5ToString(v).Text);
end;

type
  TDoStatusData = record
    TokenObj: TCoreClassObject;
    OnStatusNear: TDoStatusMethod;
    OnStatusFar: TDoStatusCall;
  end;

  PDoStatusData = ^TDoStatusData;

  TDoStatusNoLnData = record
    s: TPascalString;
    th: TCoreClassThread;
    TriggerTime: TTimeTick;
  end;

  PDoStatusNoLnData = ^TDoStatusNoLnData;

  THookDoStatusDataList = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<PDoStatusData>;
  TReservedStatusDataList = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<PSystemString>;
  TDoStatusNoLnDataList = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<PDoStatusNoLnData>;

var
  StatusActive: Boolean;
  HookDoStatus: THookDoStatusDataList;
  ReservedStatus: TReservedStatusDataList;
  StatusCritical: TCriticalSection;
  StatusNoLnDataList: TDoStatusNoLnDataList;
  Hooked_OnCheckThreadSynchronize: TCheckThreadSynchronize;

function GetOrCreateStatusNoLnData_(th_: TCoreClassThread): PDoStatusNoLnData;
var
  tk: TTimeTick;
  i: Integer;
begin
  tk := GetTimeTick();
  Result := nil;
  i := 0;
  while i < StatusNoLnDataList.Count do
    begin
      if StatusNoLnDataList[i]^.th = th_ then
        begin
          Result := StatusNoLnDataList[i];
          Result^.TriggerTime := tk;

          if i > 0 then
              StatusNoLnDataList.Exchange(i, 0);
          inc(i);
        end
      else if tk - StatusNoLnDataList[i]^.TriggerTime > C_Tick_Minute then
        begin
          Dispose(StatusNoLnDataList[i]);
          StatusNoLnDataList.Delete(i);
        end
      else
          inc(i);
    end;

  if Result = nil then
    begin
      new(Result);
      Result^.s := '';
      Result^.th := th_;
      Result^.TriggerTime := tk;
      StatusNoLnDataList.Add(Result);
    end;
end;

function GetOrCreateStatusNoLnData(): PDoStatusNoLnData;
begin
  Result := GetOrCreateStatusNoLnData_(TCoreClassThread.CurrentThread);
end;

procedure DoStatusNoLn(const v: TPascalString);
var
  L, i: Integer;
  StatusNoLnData: PDoStatusNoLnData;
  ps: PSystemString;
begin
  StatusCritical.Acquire;
  StatusNoLnData := GetOrCreateStatusNoLnData();
  try
    L := v.Len;
    i := 1;
    while i <= L do
      begin
        if CharIn(v[i], [#13, #10]) then
          begin
            if StatusNoLnData^.s.Len > 0 then
              begin
                new(ps);
                ps^ := StatusNoLnData^.s.Text;
                StatusNoLnData^.s := '';
                ReservedStatus.Add(ps);
              end;
            repeat
                inc(i);
            until (i > L) or (not CharIn(v[i], [#13, #10]));
          end
        else
          begin
            StatusNoLnData^.s.Append(v[i]);
            inc(i);
          end;
      end;
  finally
      StatusCritical.Release;
  end;
end;

procedure DoStatusNoLn(const v: SystemString; const Args: array of const);
begin
  DoStatusNoLn(Format(v, Args));
end;

procedure DoStatusNoLn;
var
  StatusNoLnData: PDoStatusNoLnData;
  a: SystemString;
begin
  StatusCritical.Acquire;
  StatusNoLnData := GetOrCreateStatusNoLnData();
  a := StatusNoLnData^.s;
  StatusNoLnData^.s := '';
  StatusCritical.Release;
  if Length(a) > 0 then
      DoStatus(a);
end;

function StrInfo(s: TPascalString): string;
begin
  Result := BytesInfo(s.Bytes);
end;

function StrInfo(s: TUPascalString): string;
begin
  Result := BytesInfo(s.Bytes);
end;

function BytesInfo(s: TBytes): string;
begin
  Result := umlStringOf(s);
end;

procedure _InternalOutput(const Text_Ptr: PSystemString; const ID: Integer);
var
  i: Integer;
  p: PDoStatusData;
begin
  if (StatusActive) and (HookDoStatus.Count > 0) then
    begin
      LastDoStatus := Text_Ptr^;
      for i := HookDoStatus.Count - 1 downto 0 do
        begin
          p := HookDoStatus[i];
          try
            if Assigned(p^.OnStatusNear) then
                p^.OnStatusNear(Text_Ptr^, ID)
            else if Assigned(p^.OnStatusFar) then
                p^.OnStatusFar(Text_Ptr^, ID);
          except
          end;
        end;
    end;

{$IFNDEF FPC}
  if ((IDEOutput) or (ID = 2)) and (DebugHook <> 0) then
    begin
{$IF Defined(WIN32) or Defined(WIN64)}
      OutputDebugString(PWideChar('"' + Text_Ptr^ + '"'));
{$ELSEIF not Defined(Linux)}
      FMX.Types.Log.d('"' + Text_Ptr^ + '"');
{$IFEND}
    end;
{$IFEND FPC}
  if ((ConsoleOutput) or (ID = 2)) and (IsConsole) then
      Writeln(Text_Ptr^);
end;

procedure CheckDoStatus(th: TCoreClassThread);
var
  i: Integer;
begin
  if StatusCritical = nil then
      exit;
  if (th = nil) or (th.ThreadID <> MainThreadID) then
      exit;
  StatusCritical.Acquire;
  try
    if ReservedStatus.Count > 0 then
      begin
        for i := 0 to ReservedStatus.Count - 1 do
          begin
            _InternalOutput(PSystemString(ReservedStatus[i]), 0);
            Dispose(PSystemString(ReservedStatus[i]));
          end;
        ReservedStatus.Clear;
      end;
  finally
      StatusCritical.Release;
  end;
end;

procedure DoStatus;
begin
  CheckDoStatus(TCoreClassThread.CurrentThread);
end;

procedure InternalDoStatus(Text_: SystemString; const ID: Integer);
var
  th: TCoreClassThread;
  ps: PSystemString;
begin
  th := TCoreClassThread.CurrentThread;
  if (th = nil) or (th.ThreadID <> MainThreadID) then
    begin
      new(ps);
      ps^ := Text_;
      StatusCritical.Acquire;
      ReservedStatus.Add(ps);
      StatusCritical.Release;
      exit;
    end;

  CheckDoStatus(th);
  _InternalOutput(@Text_, ID);
end;

procedure AddDoStatusHook(TokenObj: TCoreClassObject; CallProc: TDoStatusMethod);
begin
  AddDoStatusHookM(TokenObj, CallProc);
end;

procedure AddDoStatusHookM(TokenObj: TCoreClassObject; CallProc: TDoStatusMethod);
var
  p: PDoStatusData;
begin
  new(p);
  p^.TokenObj := TokenObj;
  p^.OnStatusNear := CallProc;
  p^.OnStatusFar := nil;
  HookDoStatus.Add(p);
end;

procedure AddDoStatusHookC(TokenObj: TCoreClassObject; CallProc: TDoStatusCall);
var
  p: PDoStatusData;
begin
  new(p);
  p^.TokenObj := TokenObj;
  p^.OnStatusNear := nil;
  p^.OnStatusFar := CallProc;
  HookDoStatus.Add(p);
end;

procedure DeleteDoStatusHook(TokenObj: TCoreClassObject);
var
  i: Integer;
  p: PDoStatusData;
begin
  i := 0;
  while i < HookDoStatus.Count do
    begin
      p := HookDoStatus[i];
      if p^.TokenObj = TokenObj then
        begin
          Dispose(p);
          HookDoStatus.Delete(i);
        end
      else
          inc(i);
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

procedure DoCheckThreadSynchronize;
begin
  DoStatus();
  if Assigned(Hooked_OnCheckThreadSynchronize) then
      Hooked_OnCheckThreadSynchronize();
end;

procedure _DoInit;
begin
  HookDoStatus := THookDoStatusDataList.Create;
  ReservedStatus := TReservedStatusDataList.Create;
  StatusCritical := TCriticalSection.Create;
  StatusNoLnDataList := TDoStatusNoLnDataList.Create;

  StatusActive := True;
  LastDoStatus := '';
  IDEOutput := False;
  ConsoleOutput := True;
  OnDoStatusHook := {$IFDEF FPC}@{$ENDIF FPC}InternalDoStatus;

  Hooked_OnCheckThreadSynchronize := CoreClasses.OnCheckThreadSynchronize;
  CoreClasses.OnCheckThreadSynchronize := {$IFDEF FPC}@{$ENDIF FPC}DoCheckThreadSynchronize;
end;

procedure _DoFree;
var
  i: Integer;
begin
  for i := 0 to HookDoStatus.Count - 1 do
      Dispose(PDoStatusData(HookDoStatus[i]));
  DisposeObject(HookDoStatus);

  for i := 0 to ReservedStatus.Count - 1 do
      Dispose(PSystemString(ReservedStatus[i]));
  DisposeObject(ReservedStatus);

  for i := 0 to StatusNoLnDataList.Count - 1 do
      Dispose(StatusNoLnDataList[i]);
  DisposeObject(StatusNoLnDataList);

  DisposeObject(StatusCritical);

  StatusActive := True;
  StatusCritical := nil;
end;

initialization

_DoInit;

finalization

_DoFree;

end.
