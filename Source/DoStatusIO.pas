{ ****************************************************************************** }
{ * Status IO support, writen by QQ 600585@qq.com                              * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }

unit DoStatusIO;

{$INCLUDE zDefine.inc}

interface

uses
  {$IF Defined(WIN32) or Defined(WIN64)}
  Windows,
  {$ELSEIF not Defined(Linux)}
  {$IFNDEF FPC}
  FMX.Types,
  {$IFEND FPC}
  {$IFEND}
  SysUtils, Classes, PascalStrings, UPascalStrings, UnicodeMixedLib, CoreClasses, MemoryStream64;

type
  TDoStatusMethod = procedure(AText: SystemString; const ID: Integer) of object;
  TDoStatusCall   = procedure(AText: SystemString; const ID: Integer);

procedure DoStatus(Text: SystemString; const ID: Integer); overload;
procedure AddDoStatusHook(TokenObj: TCoreClassObject; CallProc: TDoStatusMethod); overload;
procedure AddDoStatusHook(TokenObj: TCoreClassObject; CallProc: TDoStatusCall); overload;
procedure DeleteDoStatusHook(TokenObj: TCoreClassObject);
procedure DisableStatus;
procedure EnabledStatus;

procedure DoStatus(const v: Pointer; siz, width: nativeInt); overload;
procedure DoStatus(prefix: SystemString; v: Pointer; siz, width: nativeInt); overload;
procedure DoStatus(const v: TMemoryStream64); overload;
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

procedure DoStatusNoLn(const v: TPascalString); overload;
procedure DoStatusNoLn(const v: SystemString; const Args: array of const); overload;
procedure DoStatusNoLn; overload;

var
  LastDoStatus: SystemString;
  IDEOutput: Boolean;
  ConsoleOutput: Boolean;
  OnDoStatusHook: TDoStatusCall;

implementation

procedure bufHashToString(hash: Pointer; Size: nativeInt; var output: TPascalString);
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

procedure DoStatus(const v: Pointer; siz, width: nativeInt);
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

procedure DoStatus(prefix: SystemString; v: Pointer; siz, width: nativeInt);
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

procedure DoStatus(const v: TMemoryStream64);
var
  p: PByte;
  i: Integer;
  n: SystemString;
begin
  p := v.Memory;
  for i := 0 to v.Size - 1 do
    begin
      if n <> '' then
          n := n + ',' + IntToStr(p^)
      else
          n := IntToStr(p^);
      Inc(p);
    end;
  DoStatus(IntToHex(nativeInt(v), SizeOf(Pointer)) + ':' + n);
end;

procedure DoStatus(const v: TCoreClassStrings);
var
  i: Integer;
begin
  for i := 0 to v.Count - 1 do
      DoStatus(v[i]);
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
  DoStatus(umlMD52String(v).Text);
end;

type
  TDoStatusData = packed record
    TokenObj: TCoreClassObject;
    OnStatusNear: TDoStatusMethod;
    OnStatusFar: TDoStatusCall;
  end;

  PDoStatusData = ^TDoStatusData;

threadvar StatusActive: Boolean;

var
  HookDoStatus: TCoreClassList;
  ReservedStatus: TCoreClassList;
  LastDoStatusNoLn: TPascalString;

procedure DoStatusNoLn(const v: TPascalString);
var
  L, i: Integer;
  ps: PSystemString;
begin
  LockObject(HookDoStatus);
  try
    L := v.Len;
    i := 1;
    while i <= L do
      begin
        if CharIn(v[i], [#13, #10]) then
          begin
            if LastDoStatusNoLn.Len > 0 then
              begin
                new(ps);
                ps^ := LastDoStatusNoLn.Text;
                LastDoStatusNoLn := '';
                ReservedStatus.Add(ps);
              end;
            repeat
                Inc(i);
            until (i > L) or (not CharIn(v[i], [#13, #10]));
          end
        else
          begin
            LastDoStatusNoLn.Append(v[i]);
            Inc(i);
          end;
      end;
  finally
      UnLockObject(HookDoStatus);
  end;
end;

procedure DoStatusNoLn(const v: SystemString; const Args: array of const);
begin
  DoStatusNoLn(Format(v, Args));
end;

procedure DoStatusNoLn;
begin
  if LastDoStatusNoLn.Len > 0 then
    begin
      DoStatus(LastDoStatusNoLn);
      LastDoStatusNoLn := '';
    end;
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

procedure InternalDoStatus(Text: SystemString; const ID: Integer);
var
  th: TCoreClassThread;
  ps: PSystemString;
  i: Integer;
begin
  th := TCoreClassThread.CurrentThread;
  if (th <> nil) and (th.ThreadID <> MainThreadID) then
    begin
      LockObject(HookDoStatus);
      try
        new(ps);
        ps^ := Text;
        ReservedStatus.Add(ps);
      finally
          UnLockObject(HookDoStatus);
      end;
      Exit;
    end;

  LockObject(HookDoStatus);
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

    _InternalOutput(@Text, ID);
  finally
      UnLockObject(HookDoStatus);
  end;
end;

procedure DoStatus(Text: SystemString; const ID: Integer);
begin
  OnDoStatusHook(Text, ID);
end;

procedure AddDoStatusHook(TokenObj: TCoreClassObject; CallProc: TDoStatusMethod);
var
  _Data: PDoStatusData;
begin
  new(_Data);
  _Data^.TokenObj := TokenObj;
  _Data^.OnStatusNear := CallProc;
  _Data^.OnStatusFar := nil;
  HookDoStatus.Add(_Data);
end;

procedure AddDoStatusHook(TokenObj: TCoreClassObject; CallProc: TDoStatusCall);
var
  _Data: PDoStatusData;
begin
  new(_Data);
  _Data^.TokenObj := TokenObj;
  _Data^.OnStatusNear := nil;
  _Data^.OnStatusFar := CallProc;
  HookDoStatus.Add(_Data);
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
          Inc(i);
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

type
  TOutputStatusCheckTh = class(TCoreClassThread)
  protected
    procedure Sync_CheckOutput;
    procedure Execute; override;
  end;

var
  _OutputStatusCheckTh: TOutputStatusCheckTh;
  _OutputStatusCheckTh_Runing: Boolean;

procedure TOutputStatusCheckTh.Sync_CheckOutput;
var
  i: Integer;
begin
  if ReservedStatus.Count > 0 then
    begin
      LockObject(HookDoStatus);
      try
        for i := 0 to ReservedStatus.Count - 1 do
          begin
            _InternalOutput(PSystemString(ReservedStatus[i]), 0);
            Dispose(PSystemString(ReservedStatus[i]));
          end;
        ReservedStatus.Clear;
      finally
          UnLockObject(HookDoStatus);
      end;
    end;
end;

procedure TOutputStatusCheckTh.Execute;
begin
  while _OutputStatusCheckTh_Runing do
    begin
      Sleep(10);
      if ReservedStatus.Count > 0 then
          Synchronize({$IFDEF FPC}@{$ENDIF FPC}Sync_CheckOutput);
    end;

  _OutputStatusCheckTh := nil;
end;

procedure _DoInit;
begin
  HookDoStatus := TCoreClassList.Create;
  ReservedStatus := TCoreClassList.Create;

  StatusActive := True;
  LastDoStatus := '';
  IDEOutput := False;
  ConsoleOutput := True;
  OnDoStatusHook := {$IFDEF FPC}@{$ENDIF FPC}InternalDoStatus;
  _OutputStatusCheckTh_Runing := True;

  _OutputStatusCheckTh := TOutputStatusCheckTh.Create(True);
  _OutputStatusCheckTh.FreeOnTerminate := True;
  _OutputStatusCheckTh.Suspended := False;
end;

procedure _DoFree;
var
  i: Integer;
begin
  _OutputStatusCheckTh_Runing := False;
  while _OutputStatusCheckTh <> nil do
      TThread.Sleep(1);

  for i := 0 to HookDoStatus.Count - 1 do
      Dispose(PDoStatusData(HookDoStatus[i]));
  DisposeObject(HookDoStatus);

  for i := 0 to ReservedStatus.Count - 1 do
      Dispose(PSystemString(ReservedStatus[i]));
  DisposeObject(ReservedStatus);

  StatusActive := True;
  LastDoStatusNoLn := '';
end;

initialization

_DoInit;

finalization

_DoFree;

end. 
