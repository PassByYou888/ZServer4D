{ ****************************************************************************** }
{ * Low MemoryHook  written by QQ 600585@qq.com                                * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }

(*
  update history
  2017-12-31
*)

unit MH;

{$INCLUDE zDefine.inc}

interface

uses CoreClasses, SyncObjs, ListEngine;

procedure BeginMemoryHook_1;
procedure EndMemoryHook_1;
function GetHookMemorySize_1: nativeUInt;
function GetHookPtrList_1: TPointerHashNativeUIntList;

procedure BeginMemoryHook_2;
procedure EndMemoryHook_2;
function GetHookMemorySize_2: nativeUInt;
function GetHookPtrList_2: TPointerHashNativeUIntList;

procedure BeginMemoryHook_3;
procedure EndMemoryHook_3;
function GetHookMemorySize_3: nativeUInt;
function GetHookPtrList_3: TPointerHashNativeUIntList;

type
  TMemoryHookedState = array [0 .. 3] of Boolean;

const
  cEnabled_MemoryHookedState: TMemoryHookedState = (True, True, True, True);
  cDisable_MemoryHookedState: TMemoryHookedState = (False, False, False, False);

function GetMHState: TMemoryHookedState;
procedure SetMHState(const M: TMemoryHookedState);

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

function GetHookMemorySize_1: nativeUInt;
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

function GetHookMemorySize_2: nativeUInt;
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

function GetHookMemorySize_3: nativeUInt;
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

procedure SetMHState(const M: TMemoryHookedState);
begin
  MH_ZDB.MemoryHooked := M[0];
  MH_1.MemoryHooked := M[1];
  MH_2.MemoryHooked := M[2];
  MH_3.MemoryHooked := M[3];
end;

var
  MHStatusCritical: TCriticalSection;
  OriginDoStatusHook: TDoStatusCall;

procedure InternalDoStatus(Text: SystemString; const ID: Integer);
var
  hs: TMemoryHookedState;
begin
  MHStatusCritical.Acquire;
  hs := GetMHState;
  SetMHState(cDisable_MemoryHookedState);
  try
      OriginDoStatusHook(Text, ID);
  finally
    SetMHState(hs);
    MHStatusCritical.Release;
  end;
end;

initialization

MHStatusCritical := TCriticalSection.Create;
OriginDoStatusHook := OnDoStatusHook;
{$IFDEF FPC}
OnDoStatusHook := @InternalDoStatus;
{$ELSE}
OnDoStatusHook := InternalDoStatus;
{$ENDIF}

finalization

DisposeObject(MHStatusCritical);
OnDoStatusHook := OriginDoStatusHook;

end.
