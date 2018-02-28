{ ****************************************************************************** }
{ * Low MemoryHook  written by QQ 600585@qq.com                                * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ ****************************************************************************** }

(*
  update history
  2017-12-31
*)

unit MH;

{$I zDefine.inc}

interface

uses ListEngine;

procedure BeginMemoryHook_1;
procedure EndMemoryHook_1;
function GetHookMemorySize_1: NativeUInt;
function GetHookPtrList_1: TPointerHashNativeUIntList; inline;

procedure BeginMemoryHook_2;
procedure EndMemoryHook_2;
function GetHookMemorySize_2: NativeUInt;
function GetHookPtrList_2: TPointerHashNativeUIntList; inline;

procedure BeginMemoryHook_3;
procedure EndMemoryHook_3;
function GetHookMemorySize_3: NativeUInt;
function GetHookPtrList_3: TPointerHashNativeUIntList; inline;

type
  TMemoryHookedState = array [0 .. 3] of Boolean;

const
  cEnabled_MemoryHookedState: TMemoryHookedState = (True, True, True, True);
  cDisable_MemoryHookedState: TMemoryHookedState = (False, False, False, False);

function GetMHState: TMemoryHookedState;
procedure SetMHState(const m: TMemoryHookedState);

implementation

uses MH_ZDB, MH_1, MH_2, MH_3, DoStatusIO, PascalStrings;

procedure BeginMemoryHook_1;
begin
  MH_1.BeginMemoryHook($FFFF);
end;

procedure EndMemoryHook_1;
begin
  MH_1.EndMemoryHook;
end;

function GetHookMemorySize_1: NativeUInt;
begin
  Result := MH_1.GetHookMemorySize;
end;

function GetHookPtrList_1: TPointerHashNativeUIntList;
begin
  Result := MH_1.HookPtrList;
end;

procedure BeginMemoryHook_2;
begin
  MH_2.BeginMemoryHook($FFFF);
end;

procedure EndMemoryHook_2;
begin
  MH_2.EndMemoryHook;
end;

function GetHookMemorySize_2: NativeUInt;
begin
  Result := MH_2.GetHookMemorySize;
end;

function GetHookPtrList_2: TPointerHashNativeUIntList;
begin
  Result := MH_2.HookPtrList;
end;

procedure BeginMemoryHook_3;
begin
  MH_3.BeginMemoryHook($FFFF);
end;

procedure EndMemoryHook_3;
begin
  MH_3.EndMemoryHook;
end;

function GetHookMemorySize_3: NativeUInt;
begin
  Result := MH_3.GetHookMemorySize;
end;

function GetHookPtrList_3: TPointerHashNativeUIntList;
begin
  Result := MH_3.HookPtrList;
end;

function GetMHState: TMemoryHookedState;
begin
  Result[0] := MH_ZDB.MemoryHooked;
  Result[1] := MH_1.MemoryHooked;
  Result[2] := MH_2.MemoryHooked;
  Result[3] := MH_3.MemoryHooked;
end;

procedure SetMHState(const m: TMemoryHookedState);
begin
  MH_ZDB.MemoryHooked := m[0];
  MH_1.MemoryHooked := m[1];
  MH_2.MemoryHooked := m[2];
  MH_3.MemoryHooked := m[3];
end;

var
  OriginDoStatusHook: TDoStatusCall;

procedure InternalDoStatus(Text: SystemString; const ID: Integer);
var
  hs: TMemoryHookedState;
begin
  hs := GetMHState;
  SetMHState(cDisable_MemoryHookedState);
  try
      OriginDoStatusHook(Text, ID);
  finally
      SetMHState(hs);
  end;
end;

initialization

OriginDoStatusHook := OnDoStatusHook;
{$IFDEF FPC}
OnDoStatusHook := @InternalDoStatus;
{$ELSE}
OnDoStatusHook := InternalDoStatus;
{$ENDIF}

finalization

OnDoStatusHook := OriginDoStatusHook;

end.
