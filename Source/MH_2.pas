{ ****************************************************************************** }
{ * Low MemoryHook  written by QQ 600585@qq.com                                * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ ****************************************************************************** }

(*
  update history
  2017-12-31
*)

unit MH_2;

{$I zDefine.inc}

interface

uses ListEngine, CoreClasses;

procedure BeginMemoryHook; overload;
procedure BeginMemoryHook(cacheLen: Integer); overload;
procedure EndMemoryHook;
function GetHookMemorySize: NativeUInt; overload;
function GetHookMemorySize(p: Pointer): NativeUInt; overload;
function GetHookMemoryMinimizePtr: Pointer;
function GetHookMemoryMaximumPtr: Pointer;

threadvar
  MemoryHooked: Boolean;

var
  HookPtrList: TPointerHashNativeUIntList;

implementation

{$IFDEF FPC}
{$I MH_fpc.inc}
{$ELSE}
{$I MH_delphi.inc}
{$ENDIF}

initialization

InstallMemoryHook;

finalization

UnInstallMemoryHook;

end.

