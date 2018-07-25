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

unit MH_2;

{$INCLUDE zDefine.inc}

interface

uses ListEngine, CoreClasses;

procedure BeginMemoryHook; overload;
procedure BeginMemoryHook(cacheLen: Integer); overload;
procedure EndMemoryHook;
function GetHookMemorySize: nativeUInt; overload;
function GetHookMemorySize(p: Pointer): nativeUInt; overload;
function GetHookMemoryMinimizePtr: Pointer;
function GetHookMemoryMaximumPtr: Pointer;

threadvar
  MemoryHooked: Boolean;

var
  HookPtrList: TPointerHashNativeUIntList;

implementation

{$IFDEF FPC}
{$INCLUDE MH_fpc.inc}
{$ELSE}
{$INCLUDE MH_delphi.inc}
{$ENDIF}

initialization

InstallMemoryHook;

finalization

UnInstallMemoryHook;

end.
 
 
