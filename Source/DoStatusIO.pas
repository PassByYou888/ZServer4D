{ ****************************************************************************** }
{ * Status Library, writen by QQ 600585@qq.com                                 * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ ****************************************************************************** }

unit DoStatusIO;

{$I zDefine.inc}

interface

uses
  {$IF Defined(WIN32) or Defined(WIN64)}
  Windows,
  {$ELSEIF not Defined(Linux)}
  FMX.Types,
  {$ENDIF}
  Sysutils, Classes, PascalStrings, UnicodeMixedLib, CoreClasses, MemoryStream64;

type
  TDoStatusMethod = procedure(AText: SystemString; const ID: Integer) of object;
  TDoStatusCall   = procedure(AText: SystemString; const ID: Integer);

procedure DoStatus(Text: SystemString; const ID: Integer); overload;
procedure AddDoStatusHook(TokenObj: TCoreClassObject; CallProc: TDoStatusMethod); overload;
procedure AddDoStatusHook(TokenObj: TCoreClassObject; CallProc: TDoStatusCall); overload;
procedure DeleteDoStatusHook(TokenObj: TCoreClassObject);
procedure DisableStatus;
procedure EnabledStatus;

procedure DoStatus(const v: Pointer; siz, width: NativeInt); overload;
procedure DoStatus(prefix: SystemString; v: Pointer; siz, width: NativeInt); overload;
procedure DoStatus(const v: TMemoryStream64); overload;
procedure DoStatus(const v: TCoreClassStrings); overload;
procedure DoStatus(const v: int64); overload;
procedure DoStatus(const v: Integer); overload;
procedure DoStatus(const v: Single); overload;
procedure DoStatus(const v: Double); overload;
procedure DoStatus(const v: Pointer); overload;
procedure DoStatus(const v: SystemString; const Args: array of const); overload;
procedure DoError(v: SystemString; const Args: array of const); overload;
procedure DoStatus(const v: SystemString); overload;
procedure DoStatus(const v: TPascalString); overload;
procedure DoStatus(const v: TMD5); overload;

procedure DoStatusNoLn(const v: TPascalString); overload;
procedure DoStatusNoLn(const v: SystemString; const Args: array of const); overload;
procedure DoStatusNoLn; overload;

var
  LastDoStatus  : SystemString;
  IDEOutput     : Boolean;
  ConsoleOutput : Boolean;
  OnDoStatusHook: TDoStatusCall;

implementation

procedure bufHashToString(hash: Pointer; Size: NativeInt; var Output: TPascalString);
const
  HexArr: array [0 .. 15] of SystemChar = ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
var
  i: Integer;
begin
  Output.Len := Size * 2;
  for i := 0 to Size - 1 do
    begin
      Output.buff[i * 2] := HexArr[(PByte(nativeUInt(hash) + i)^ shr 4) and $0F];
      Output.buff[i * 2 + 1] := HexArr[PByte(nativeUInt(hash) + i)^ and $0F];
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
      inc(p);
    end;
  DoStatus(IntToHex(NativeInt(v), SizeOf(Pointer)) + ':' + n);
end;

procedure DoStatus(const v: TCoreClassStrings);
var
  i: Integer;
begin
  for i := 0 to v.Count - 1 do
      DoStatus(v[i]);
end;

procedure DoStatus(const v: int64);
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

procedure DoStatus(const v: TMD5);
begin
  DoStatus(umlMD52String(v).Text);
end;

var
  LastDoStatusNoLn: TPascalString;

procedure DoStatusNoLn(const v: TPascalString);
var
  l, i: Integer;
begin
  l := v.Len;
  i := 1;
  while i <= l do
    begin
      if CharIn(v[i], [#13, #10]) then
        begin
          if LastDoStatusNoLn.Len > 0 then
            begin
              DoStatus(LastDoStatusNoLn);
              LastDoStatusNoLn := '';
            end;
          repeat
              inc(i);
          until (i > l) or (not CharIn(v[i], [#13, #10]));
        end
      else
        begin
          LastDoStatusNoLn.Append(v[i]);
          inc(i);
        end;
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

type
  TDoStatusData = packed record
    TokenObj: TCoreClassObject;
    OnStatusNear: TDoStatusMethod;
    OnStatusFar: TDoStatusCall;
  end;

  PDoStatusData = ^TDoStatusData;

  TThreadSyncIntf = class
  public
    Text: SystemString;
    ID  : Integer;
    th  : TCoreClassThread;
    procedure DoSync;
  end;

threadvar StatusActive: Boolean;

var
  HookDoSatus: TCoreClassList = nil;

procedure TThreadSyncIntf.DoSync;
var
  i: Integer;
  p: PDoStatusData;
begin
  try
    if (StatusActive) and (HookDoSatus.Count > 0) then
      begin
        LastDoStatus := Text;
        for i := HookDoSatus.Count - 1 downto 0 do
          begin
            p := HookDoSatus[i];
            try
              if Assigned(p^.OnStatusNear) then
                  p^.OnStatusNear(Text, ID)
              else if Assigned(p^.OnStatusFar) then
                  p^.OnStatusFar(Text, ID);
            except
            end;
          end;
      end;

    {$IFNDEF FPC}
    if ((IDEOutput) or (ID = 2)) and (DebugHook <> 0) then
      begin
        {$IF Defined(WIN32) or Defined(WIN64)}
        OutputDebugString(PWideChar('"' + Text + '"'));
        {$ELSEIF not Defined(Linux)}
        FMX.Types.Log.d('"' + Text + '"');
        {$ENDIF}
      end;
    {$ENDIF}
    if ((ConsoleOutput) or (ID = 2)) and (IsConsole) then
        Writeln(Text);
  finally
  end;
end;

procedure InternalDoStatus(Text: SystemString; const ID: Integer);
var
  th: TCoreClassThread;
  ts: TThreadSyncIntf;
  i : Integer;
  p : PDoStatusData;
begin
  th := TCoreClassThread.CurrentThread;
  if (th <> nil) and (th.ThreadID <> MainThreadID) then
    begin
      ts := TThreadSyncIntf.Create;
      ts.Text := Text;
      ts.ID := ID;
      ts.th := th;
      {$IFDEF FPC}
      TCoreClassThread.Synchronize(th, @ts.DoSync);
      {$ELSE}
      TCoreClassThread.Synchronize(th, ts.DoSync);
      {$ENDIF}
      DisposeObject(ts);
      exit;
    end;

  LockObject(HookDoSatus);
  try
    if (StatusActive) and (HookDoSatus.Count > 0) then
      begin
        LastDoStatus := Text;
        for i := HookDoSatus.Count - 1 downto 0 do
          begin
            p := HookDoSatus[i];
            try
              if Assigned(p^.OnStatusNear) then
                  p^.OnStatusNear(Text, ID)
              else if Assigned(p^.OnStatusFar) then
                  p^.OnStatusFar(Text, ID);
            except
            end;
          end;
      end;

    {$IFNDEF FPC}
    if ((IDEOutput) or (ID = 2)) and (DebugHook <> 0) then
      begin
        {$IF Defined(WIN32) or Defined(WIN64)}
        OutputDebugString(PWideChar('"' + Text + '"'));
        {$ELSEIF not Defined(Linux)}
        FMX.Types.Log.d('"' + Text + '"');
        {$ENDIF}
      end;
    {$ENDIF}
    if ((ConsoleOutput) or (ID = 2)) and (IsConsole) then
        Writeln(Text);
  finally
      UnLockObject(HookDoSatus);
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
  HookDoSatus.Add(_Data);
end;

procedure AddDoStatusHook(TokenObj: TCoreClassObject; CallProc: TDoStatusCall);
var
  _Data: PDoStatusData;
begin
  new(_Data);
  _Data^.TokenObj := TokenObj;
  _Data^.OnStatusNear := nil;
  _Data^.OnStatusFar := CallProc;
  HookDoSatus.Add(_Data);
end;

procedure DeleteDoStatusHook(TokenObj: TCoreClassObject);
var
  i: Integer;
  p: PDoStatusData;
begin
  i := 0;
  while i < HookDoSatus.Count do
    begin
      p := HookDoSatus[i];
      if p^.TokenObj = TokenObj then
        begin
          Dispose(p);
          HookDoSatus.Delete(i);
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

initialization

HookDoSatus := TCoreClassList.Create;
StatusActive := True;
LastDoStatus := '';
IDEOutput := False;
ConsoleOutput := True;
{$IFDEF FPC}
OnDoStatusHook := @InternalDoStatus;
{$ELSE}
OnDoStatusHook := InternalDoStatus;
{$ENDIF}

finalization

while HookDoSatus.Count > 0 do
  begin
    Dispose(PDoStatusData(HookDoSatus[0]));
    HookDoSatus.Delete(0);
  end;
DisposeObject(HookDoSatus);
StatusActive := True;
LastDoStatusNoLn := '';

end.
