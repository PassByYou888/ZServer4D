{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:
Creation:     April 2004
Version:      1.18
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2004-2011 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.
              <francois.piette@overbyte.be>

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.

History:
Aug 20, 2006 V1.01 Added IntToHex for .NET
Oct 28, 2006 V1.02 Added SysErrorMessage
Mar 10, 2008 V1.03 Made some changes to prepare code for Unicode
                   StrLen takes a PAnsiChar instead of Char argument
                   StrCopy takes a PAnsiChar instead of Char argument
                   StrPas takes a PAnsiChar instead of Char argument
Apr 15, 2008 V1.04 A. Garrels  - All Str.. functions take PAnsiChar argument
                   and are overloaded in D12 to also take a PWideChar argument
                   according to changes in SysUtils.pas.
Apr 20, 2008 V1.05 A. Garrels - Added own implementations for wide-versions of
                   StrLComp and StrLIComp. Added some other functions from
                   SysUtils.pas. Since the overloaded functions are included
                   you would have to reference the unit if both SysUtils.pas
                   and OverbyteIcsLibrary.pas are in the uses clause, i.e.
                   either SysUtils.StrLIComp() or OverbyteIcsLibrary.StrLIComp(),
                   not sure whether that breaks something.
Apr 20, 2008 V1.06 A. Garrels added GetACP and const CP_ACP.
Apr 30, 2008 V1.07 A. Garrels prefixed any Win32 functions with an underscore,
                   Added Ansi versions of Uppercase, Lowercase, Trim.
May 15, 2008 V1.08 A. Garrels optimized _UpperCase, _LowerCase, _Trim and
                   added IcsIntToStrA(), IcsIntToHexA() and _CompareText().
                   New define "USE_ICS_RTL" see below, added missing overloads
                   of _IntToStr.
May 23, 2008 V1.09 A. Garrels check for empty string in IcsLowerCaseA() and
                   IcsUpperCaseA().
Jul 01, 2008 V1.10 A. Garrels fixed a bug in IcsCompareTextA().
Jul 02, 2008 V1.11 A. Garrels optimized IcsCompareTextA() a bit.
Aug 03, 2008 V1.12 F. Piette made IcsUpperCaseA, IcsLowerCaseA, IcsTrimA and
                   IcsCompareTextA public. Added IcsSameTextA.
Aug 07, 2008 V1.13 F. Piette found a bug in some AnsiString-functions.
                   They always call SetLength() on the result now.
Aug 11, 2008 V1.14 A. Garrels added IcsCompareStrA() and an overload to
                   _CompareStr().
May 09, 2009 V1.15 Arno added const CP_UTF7, and procedure _ShowException
                   for future use.
May 15, 2009 V1.16 Arno added some EXTERNALSYM directives to make C++Builder
                   happy (Ambiguity between '_fastcall Application()' and
                   'Forms::Application')
Apr 15, 2011 V1.17 Arno prepared for 64-bit.
Aug 26, 2011 V1.18 Arno added 64-bit overloaded versions of _IntToHex.

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsLibrary deprecated;
{ Do no longer use this unit instead include Windows, SysUtils, Classes and }
{ OverbyteIcsUtils in your uses clause. ICS IPv6 doesn't use it anymore.    }
{ Search for "deprecated" to find the replacement routines. Those not       }
{ explicitly marked as deprecated are to be replaced by the original Delphi }
{ RTL rounines which means that in your code just remove the underscore     }
{ prefix.                                                                   }

interface
{$WARNINGS ON}
{$B-}             { Enable partial boolean evaluation   }
{$T-}             { Untyped pointers                    }
{$X+}             { Enable extended syntax              }
{$I Include\OverbyteIcsDefs.inc}
{$IFDEF COMPILER14_UP}
  {$IFDEF NO_EXTENDED_RTTI}
    {$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
  {$ENDIF}
{$ENDIF}
{ The following define works only for compiler versions < v120.               }
{ Comment next line if you want to use the original RTL including FastCode    }
{ patches. Otherwise ICS routines are called when they are faster either      }
{ faster than original RTL routines or not available in the RTL. Currently    }
{ only a few Ansi-versions are implemented. Should we use FastCode?           }
{#$DEFINE USE_ICS_RTL}

uses
{$IFDEF MSWINDOWS}
  Windows,
  Messages,
{$ENDIF}
{$IFDEF POSIX}
  Posix.Pthread,
  Ics.Posix.WinTypes,
  Ics.Posix.Messages,
{$ENDIF}
{$IFDEF MACOS}
  MacApi.CoreServices,
{$ENDIF}
  Classes,
{$IFNDEF NOFORMS}
  {$IFDEF FMX}
    FMX.Forms,
  {$ELSE}
    Forms,
  {$ENDIF}
{$ENDIF}
  SysUtils,
  OverbyteIcsTypes;

const
  OverbyteIcsLibraryVersion = 117;
  CopyRight : String        = ' OverbyteIcsLibrary (c) 2004-2011 F. Piette V1.17 ';


const
  {$EXTERNALSYM fmOpenRead}
  fmOpenRead       = SysUtils.fmOpenRead;
  {$EXTERNALSYM fmShareDenyWrite}
  fmShareDenyWrite = SysUtils.fmShareDenyWrite;
  {$EXTERNALSYM faAnyFile}
  faAnyFile        = SysUtils.faAnyFile;
  {$EXTERNALSYM faDirectory}
  faDirectory      = SysUtils.faDirectory;
//  {$EXTERNALSYM faVolumeID}
//  faVolumeID       = SysUtils.faVolumeID;  //deprecated;  // not used in Win32
  {$EXTERNALSYM faReadOnly}
  faReadOnly       = SysUtils.faReadOnly;
  {$EXTERNALSYM faSysFile}
  faSysFile        = SysUtils.faSysFile;
  {$EXTERNALSYM faHidden}
  faHidden         = SysUtils.faHidden;
  {$EXTERNALSYM rfReplaceAll}
  rfReplaceAll     = SysUtils.rfReplaceAll;
  {$EXTERNALSYM MinDateTime}
  MinDateTime      : TDateTime = -657434.0;
  {$EXTERNALSYM MaxDateTime}
{$HINTS OFF}
  MaxDateTime      : TDateTime = 2958465.99999;
{$HINTS ON}
  {$EXTERNALSYM opRemove}
  opRemove         = Classes.opRemove;
  {$EXTERNALSYM csDesigning}
  csDesigning      = Classes.csDesigning;
  {$EXTERNALSYM csDestroying}
  csDestroying     = Classes.csDestroying;
  {$EXTERNALSYM lnDeleted}
  lnDeleted        = Classes.lnDeleted;

 {$IFDEF MSWINDOWS}

  {$EXTERNALSYM WM_QUIT}
  WM_QUIT          = Messages.WM_QUIT;
  {$EXTERNALSYM WM_USER}
  WM_USER          = Messages.WM_USER;
  {$EXTERNALSYM WM_TIMER}
  WM_TIMER         = Messages.WM_TIMER;
  {$EXTERNALSYM PM_REMOVE}
  PM_REMOVE        = Windows.PM_REMOVE;
  {$EXTERNALSYM WS_EX_TOOLWINDOW}
  WS_EX_TOOLWINDOW = Windows.WS_EX_TOOLWINDOW;
  {$EXTERNALSYM WS_POPUP}
  WS_POPUP         = Windows.WS_POPUP;
  {$EXTERNALSYM TIME_ZONE_ID_DAYLIGHT}
  TIME_ZONE_ID_DAYLIGHT = Windows.TIME_ZONE_ID_DAYLIGHT;
  {$EXTERNALSYM TIME_ZONE_ID_STANDARD}
  TIME_ZONE_ID_STANDARD = Windows.TIME_ZONE_ID_STANDARD;
  {$EXTERNALSYM CP_ACP}
  CP_ACP           = Windows.CP_ACP;
  {$EXTERNALSYM CP_UTF8}
  CP_UTF8          = Windows.CP_UTF8;
  {$EXTERNALSYM CP_UTF7}
  CP_UTF7          = Windows.CP_UTF7;

 {$ENDIF MSWINDOWS}

function  _SysErrorMessage(ErrCode: Integer): String;
procedure _ShowException(ExceptObject: TObject; ExceptAddr: Pointer);
function  _BoolToStr(B: Boolean; UseBoolStrs: Boolean = False): String;
function  _IntToStr(const N : Integer) : String; overload;
function  _IntToStr(const N : Int64) : String; overload;

{$IFDEF COMPILER12_UP}
  function  _IntToStr(const N : Cardinal) : String; overload;
{$ENDIF}

function IcsIntToStrA(N : Integer): AnsiString; deprecated
          {$IFDEF COMPILER12_UP}'Use OverbyteIcsUtils.IcsIntToStrA'{$ENDIF};
function IcsIntToHexA(N : Integer; Digits: Byte) : AnsiString; deprecated
          {$IFDEF COMPILER12_UP}'Use OverbyteIcsUtils.IcsIntToHexA'{$ENDIF};
function  _IntToHex(Value: Integer; Digits: Integer): String; overload;
function  _IntToHex(Value: Int64; Digits: Integer): String; overload;

{$IFDEF COMPILER16_UP}
function  _IntToHex(Value: UInt64; Digits: Integer): String; overload;
{$ENDIF}

function  _StrToInt(const S: String): Integer;
function  _StrToInt64(const S: String): Int64;
function  _StrToIntDef(const S: String; ADefault: Integer): Integer; {$IFDEF USE_INLINE} inline; {$ENDIF}
function  _StrPas(const P : PAnsiChar) : AnsiString; {$IFDEF COMPILER12_UP} overload;
function  _StrPas(const P : PWideChar) : UnicodeString; overload;
                {$ENDIF}
function  _StrLen(const P : PAnsiChar) : Cardinal; {$IFDEF COMPILER12_UP} overload;
function  _StrLen(const P : PWideChar) : Cardinal; overload;
                {$ENDIF}
function  _StrCopy(Dest: PAnsiChar; const Source: PAnsiChar): PAnsiChar; {$IFDEF COMPILER12_UP} overload;
function  _StrCopy(Dest: PWideChar; const Source: PWideChar): PWideChar; overload;
                {$ENDIF}
function  _FloatToStr(Value: Extended): String;
function _Trim(const Str : AnsiString) : AnsiString; deprecated
          {$IFDEF COMPILER12_UP}'Use OverbyteIcsUtils.IcsTrim'{$ENDIF};
          {$IFDEF COMPILER12_UP} overload;
function _Trim(const Str : UnicodeString) : UnicodeString; deprecated
          {$IFDEF COMPILER12_UP}'Use OverbyteIcsUtils.IcsTrim'{$ENDIF}; overload;
                {$ENDIF}
function _StrLower(Str: PAnsiChar): PAnsiChar; {$IFDEF COMPILER12_UP} overload;
function _StrLower(Str: PWideChar): PWideChar; overload;
                {$ENDIF}
function _StrIComp(const Str1, Str2: PAnsiChar): Integer; {$IFDEF COMPILER12_UP} overload;
function _StrIComp(const Str1, Str2: PWideChar): Integer; overload;
                {$ENDIF}
function  _StrPCopy(Dest: PAnsiChar; const Source: AnsiString): PAnsiChar; {$IFDEF COMPILER12_UP} overload;
function  _StrPCopy(Dest: PWideChar; const Source: UnicodeString): PWideChar; overload;
                {$ENDIF}
function  _StrComp(const Str1, Str2: PAnsiChar): Integer; {$IFDEF COMPILER12_UP} overload;
function  _StrComp(const Str1, Str2: PWideChar): Integer; overload;
                {$ENDIF}
function  _StrLComp(const Str1, Str2: PAnsiChar; MaxLen: Cardinal): Integer; {$IFDEF COMPILER12_UP} overload;
function  _StrLComp(const Str1, Str2: PWideChar; MaxLen: Cardinal): Integer; overload;
                {$ENDIF}
function  _StrLIComp(const Str1, Str2: PAnsiChar; MaxLen: Cardinal): Integer; {$IFDEF COMPILER12_UP} overload;
function  _StrLIComp(const Str1, Str2: PWideChar; MaxLen: Cardinal): Integer;  overload;
                {$ENDIF}
function _StrToDateTime(const S: String): TDateTime; overload;
function _StrToDateTime(const S: String; const FormatSettings: TFormatSettings): TDateTime; overload;
function  _LowerCase(const S: AnsiString): AnsiString; deprecated
          {$IFDEF COMPILER12_UP}'Use OverbyteIcsUtils.IcsLowerCase'{$ENDIF};
           {$IFDEF COMPILER12_UP} overload;
function  _LowerCase(const S: UnicodeString): UnicodeString; deprecated
          {$IFDEF COMPILER12_UP}'Use OverbyteIcsUtils.IcsLowerCase'{$ENDIF}; overload;
                {$ENDIF}
function  _UpperCase(const S: AnsiString): AnsiString; deprecated
          {$IFDEF COMPILER12_UP}'Use OverbyteIcsUtils.IcsUpperCase'{$ENDIF};
          {$IFDEF COMPILER12_UP} overload;
function  _UpperCase(const S: UnicodeString): UnicodeString; deprecated
          {$IFDEF COMPILER12_UP}'Use OverbyteIcsUtils.IcsUpperCase'{$ENDIF}; overload;
                {$ENDIF}
function IcsUpperCaseA(const S: AnsiString): AnsiString; deprecated
          {$IFDEF COMPILER12_UP}'Use OverbyteIcsUtils.IcsUpperCaseA'{$ENDIF};
function IcsLowerCaseA(const S: AnsiString): AnsiString; deprecated
          {$IFDEF COMPILER12_UP}'Use OverbyteIcsUtils.IcsUpperCaseA'{$ENDIF};
function IcsCompareTextA(const S1, S2: AnsiString): Integer; deprecated
          {$IFDEF COMPILER12_UP}'Use OverbyteIcsUtils.IcsCompareTextA'{$ENDIF};
function IcsTrimA(const Str: AnsiString): AnsiString; deprecated
          {$IFDEF COMPILER12_UP}'Use OverbyteIcsUtils.IcsTrimA'{$ENDIF};
function IcsSameTextA(const S1, S2: AnsiString): Boolean; deprecated
          {$IFDEF COMPILER12_UP}'Use OverbyteIcsUtils.IcsSameTextA'{$ENDIF};
function  _CompareStr(const S1, S2: AnsiString): Integer; deprecated
          {$IFDEF COMPILER12_UP}'Use OverbyteIcsUtils.IcsCompareStr'{$ENDIF};
          {$IFDEF COMPILER12_UP} overload;
function  _CompareStr(const S1, S2: UnicodeString): Integer; deprecated
          {$IFDEF COMPILER12_UP}'Use OverbyteIcsUtils.IcsCompareStr'{$ENDIF}; overload;
                {$ENDIF}
function  _CompareText(const S1, S2: AnsiString): Integer; deprecated
          {$IFDEF COMPILER12_UP}'Use OverbyteIcsUtils.IcsCompareText'{$ENDIF};
          {$IFDEF COMPILER12_UP} overload;
function  _CompareText(const S1, S2: UnicodeString): Integer; deprecated
          {$IFDEF COMPILER12_UP}'Use OverbyteIcsUtils.IcsCompareText'{$ENDIF}; overload;
                {$ENDIF}
function _AnsiStrComp(S1, S2: PAnsiChar): Integer;
function  _FileExists(const FileName: String): Boolean;
function  _DeleteFile(const FileName: String): Boolean;
function  _ExtractFileExt(const FileName: String): String;
function  _ExtractFilePath(const FileName: String): String;
function  _DateTimeToStr(const DateTime: TDateTime): String;
procedure _DecodeDate(Date: TDateTime; var Year, Month, Day: Word);
procedure _DecodeTime(Time: TDateTime; var Hour, Min, Sec, MSec: Word);
function  _EncodeTime(Hour, Min, Sec, MSec: Word): TDateTime;
function  _DirectoryExists(const Name: String): Boolean;
function  _ExcludeTrailingPathDelimiter(const S: String): String;
function  _IncludeTrailingPathDelimiter(const S: String): String;
function  _Format(const Fmt: String; const Args: array of const): String;
function  _FindFirst(const Path: String; Attr: Integer; var F: TSearchRec): Integer;
function  _FindNext(var F: TSearchRec): Integer;
procedure _FindClose(var F: TSearchRec);
function  _FileDateToDateTime(FileDate: Integer): TDateTime;
procedure _FreeAndNil(var Obj);
function  _Date : TDateTime;
function  _Now: TDateTime;
function  _StringReplace(const S: String; const OldPattern: String;
    const NewPattern: String; Flags: TReplaceFlags): String;
function _GetTickCount: LongWord;
function  _GetCurrentThreadId: TThreadID;
function  _PostMessage(H: HWND; Msg: UINT; ParamW: WPARAM; ParamL: LPARAM): LongBooL;
function  _SendMessage(H: HWND; Msg: UINT; ParamW: WPARAM; ParamL: LPARAM): LRESULT;
procedure _Sleep(dwMilliseconds: LongWord);
function _FreeLibrary(hLibModule: HMODULE): LongBool;

{$IFDEF MSWINDOWS}
function  _GetTimeZoneInformation(var lpTimeZoneInformation: TTimeZoneInformation): DWORD;
function  _GetWindowLong(H: HWND; nIndex: Integer): Longint;
function  _DefWindowProc(H: HWND; Msg: UINT; ParamW: WPARAM; ParamL: LPARAM): LRESULT;
function  _GetMessage(var lpMsg: TMsg; H: HWND;
                     wMsgFilterMin, wMsgFilterMax: UINT): BOOL;
function  _TranslateMessage(const lpMsg: TMsg): BOOL;
function  _DispatchMessage(const lpMsg: TMsg): LongInt;
function  _PeekMessage(var lpMsg: TMsg; H: HWND;
               wMsgFilterMin, wMsgFilterMax, wRemoveMsg: UINT): BOOL;
procedure _EnterCriticalSection(var lpCriticalSection: TRTLCriticalSection);
function _TryEnterCriticalSection(var lpCriticalSection: TRTLCriticalSection): BOOL;
procedure _LeaveCriticalSection(var lpCriticalSection: TRTLCriticalSection);
procedure _InitializeCriticalSection(var lpCriticalSection: TRTLCriticalSection);
procedure _DeleteCriticalSection(var lpCriticalSection: TRTLCriticalSection);
function  _GetClassInfo(hInstance: HINST; lpClassName: PChar;
                       var lpWndClass: TWndClass): BOOL;
function _RegisterClass(const lpWndClass: TWndClass): ATOM;
function _UnregisterClass(lpClassName: PChar; hInstance: HINST): BOOL;
function _CreateWindowEx(dwExStyle: DWORD; lpClassName: PChar;
  lpWindowName: PChar; dwStyle: DWORD; X, Y, nWidth, nHeight: Integer;
  hWndParent: HWND; h_Menu: HMENU; hInstance: HINST; lpParam: Pointer): HWND;
function _DestroyWindow(H: HWND): BOOL;
function _SetWindowLong(H: HWND; nIndex: Integer; dwNewLong: Longint): Longint;
function _LoadLibrary(lpLibFileName: PChar): HMODULE;
function _GetProcAddress(hModule: HMODULE; lpProcName: LPCSTR): FARPROC;
function _GetACP: Cardinal;
procedure _OutputDebugString(lpOutputString: PChar);
function _IsWindow(hWnd: HWND): BOOL;
procedure _GetSystemInfo(var lpSystemInfo: TSystemInfo);
{$ENDIF MSWINDOWS}

{$IFNDEF NOFORMS}
{$EXTERNALSYM Application}
function Application : TApplication;
{$ENDIF}

implementation

{$IFDEF POSIX}
uses
    Posix.Unistd, Posix.String_;
{$ENDIF}

function _SysErrorMessage(ErrCode: Integer): String;
begin
    Result := SysUtils.SysErrorMessage(ErrCode);
end;

procedure _ShowException(ExceptObject: TObject; ExceptAddr: Pointer);
begin
    SysUtils.ShowException(ExceptObject, ExceptAddr);
end;

function _BoolToStr(B: Boolean; UseBoolStrs: Boolean = False): String;
begin
    Result := SysUtils.BoolToStr(B, UseBoolStrs);
end;

function _IntToStr(const N : Integer) : String;
begin
    Result := SysUtils.IntToStr(N);
end;

function _IntToStr(const N : Int64) : String;
begin
    Result := SysUtils.IntToStr(N);
end;
{$IFDEF COMPILER12_UP}
function _IntToStr(const N : Cardinal) : String;
begin
    Result := SysUtils.IntToStr(N);
end;
{$ENDIF}

{ Author Arno Garrels - Needs optimization!      }
{ It's a bit slower that the RTL routine.        }
{ We should realy use a FastCode function here.  }
function IntToStrA(N : Integer) : AnsiString;
var
    I : Integer;
    Buf : array [0..11] of AnsiChar;
    Sign : Boolean;
begin
    if N >= 0 then
        Sign := FALSE
    else begin
        Sign := TRUE;
        if N = Low(Integer) then
        begin
            Result := '-2147483648';
            Exit;
        end
        else
            N := Abs(N);
    end;
    I := Length(Buf);
    repeat
        Dec(I);
        Buf[I] := AnsiChar(N mod 10 + $30);
        N := N div 10;
    until N = 0;
    if Sign then begin
        Dec(I);
        Buf[I] := '-';
    end;
    SetLength(Result, Length(Buf) - I);
    Move(Buf[I], Pointer(Result)^, Length(Buf) - I);
end;

function IcsIntToStrA(N : Integer) : AnsiString;
begin
{$IFDEF USE_ICS_RTL}
    Result := IntToStrA(N);
{$ELSE}
{$IFNDEF COMPILER12_UP}
    Result := SysUtils.IntToStr(N);
{$ELSE}
    Result := IntToStrA(N);
{$ENDIF}
{$ENDIF}
end;

{ Author Arno Garrels - Feel free to optimize!                }
{ It's anyway faster than the RTL routine.                    }
function IntToHexA(N : Integer; Digits: Byte) : AnsiString;
var
    Buf : array [0..7] of Byte;
    V : Cardinal;
    I : Integer;
begin
    V := Cardinal(N);
    I := Length(Buf);
    if Digits > I then Digits := I;
    repeat
        Dec(I);
        Buf[I] := V mod 16;
        if Buf[I] < 10 then
            Inc(Buf[I], $30)
        else
            Inc(Buf[I], $37);
        V := V div 16;
    until V = 0;
    while Digits > Length(Buf) - I do begin
       Dec(I);
       Buf[I] := $30;
    end;
    SetLength(Result, Length(Buf) - I);
    Move(Buf[I], Pointer(Result)^, Length(Buf) - I);
end;

function IcsIntToHexA(N : Integer; Digits: Byte) : AnsiString;
begin
{$IFDEF USE_ICS_RTL}
    Result := IntToHexA(N, Digits);
{$ELSE}
{$IFNDEF COMPILER12_UP}
    Result := SysUtils.IntToHex(N, Digits);
{$ELSE}
    Result := IntToHexA(N, Digits);
{$ENDIF}
{$ENDIF}
end;

function _StrToInt(const S: String): Integer;
begin
    Result := SysUtils.StrToInt(S);
end;

function _StrToInt64(const S: String): Int64;
begin
    Result := SysUtils.StrToInt64(S);
end;

function  _StrToIntDef(const S: String; ADefault: Integer): Integer;
begin
    Result := SysUtils.StrToIntDef(S, ADefault);
end;

function _StrPas(const P : PAnsiChar) : AnsiString; // Unicode change
begin
    Result := SysUtils.StrPas(P);
end;

{$IFDEF COMPILER12_UP}
function _StrPas(const P : PWideChar): UnicodeString; // Unicode change
begin
    Result := SysUtils.StrPas(P);
end;
{$ENDIF}

function _StrLen(const P : PAnsiChar) : Cardinal;  // Unicode change
begin
    Result := SysUtils.StrLen(P);
end;

{$IFDEF COMPILER12_UP}
function _StrLen(const P : PWideChar) : Cardinal; // Unicode change
begin
    Result := SysUtils.StrLen(P);
end;
{$ENDIF}

function _StrCopy(Dest: PAnsiChar; const Source: PAnsiChar): PAnsiChar; // Unicode change
begin
    Result := SysUtils.StrCopy(Dest, Source);
end;

{$IFDEF COMPILER12_UP}
function  _StrCopy(Dest: PWideChar; const Source: PWideChar): PWideChar; // Unicode change
begin
    Result := SysUtils.StrCopy(Dest, Source);
end;
{$ENDIF}

{$IFDEF COMPILER12_UP}
function _TrimLeft(const Str: AnsiString): AnsiString;
var
    I, L : Integer;
begin
    L := Length(Str);
    I := 1;
    while (I <= L) and (Str[I] <= ' ') do
        Inc(I);
    Result := Copy(Str, I, Maxint);
end;

function _TrimRight(const Str: AnsiString): AnsiString;
var
    I : Integer;
begin
    I := Length(Str);
    while (I > 0) and (Str[I] <= ' ') do
        Dec(I);
    Result := Copy(Str, 1, I);
end;
{$ENDIF}

{ Author Arno Garrels - Feel free to optimize!                }
{ It's a bit faster than the RTL routine.                     }
function IcsTrimA(const Str: AnsiString): AnsiString;
var
    I, L : Integer;
begin
    L := Length(Str);
    I := 1;
    while (I <= L) and (Str[I] <= ' ') do
        Inc(I);
    if I > L then
        Result := ''
    else begin
        while Str[L] <= ' ' do
            Dec(L);
        SetLength(Result, L - I + 1);
        Move(Str[I], Pointer(Result)^, L - I + 1);
    end;
end;

function _Trim(const Str: AnsiString): AnsiString;
begin
{$IFDEF USE_ICS_RTL}
    Result := IcsTrimA(Str);
{$ELSE}
{$IFNDEF COMPILER12_UP}
    Result := SysUtils.Trim(Str);
{$ELSE}
    Result := IcsTrimA(Str);
{$ENDIF}
{$ENDIF}
end;


{$IFDEF COMPILER12_UP}
function _Trim(const Str : UnicodeString) : UnicodeString;
begin
    Result := SysUtils.Trim(Str);
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

function _StrLower(Str: PAnsiChar): PAnsiChar;
begin
    Result := SysUtils.StrLower(Str);
end;

{$IFDEF COMPILER12_UP}
function _StrLower(Str: PWideChar): PWideChar;
begin
    Result := SysUtils.StrLower(Str);
end;
{$ENDIF}

function _StrPCopy(Dest: PAnsiChar; const Source: AnsiString): PAnsiChar; // Unicode change
begin
    Result := SysUtils.StrPCopy(Dest, Source);
end;

{$IFDEF COMPILER12_UP}
function  _StrPCopy(Dest: PWideChar; const Source: UnicodeString): PWideChar;
begin
    Result := SysUtils.StrPCopy(Dest, Source);
end;
{$ENDIF}

function _StrComp(const Str1, Str2 : PAnsiChar): Integer;
begin
    Result := SysUtils.StrComp(Str1, Str2);
end;

{$IFDEF COMPILER12_UP}
function _StrComp(const Str1, Str2 : PWideChar): Integer;
begin
    Result := SysUtils.StrComp(Str1, Str2);
end;
{$ENDIF}

function _StrIComp(const Str1, Str2: PAnsiChar): Integer;
begin
    Result := SysUtils.StrIComp(Str1, Str2);
end;

{$IFDEF COMPILER12_UP}
function _StrIComp(const Str1, Str2: PWideChar): Integer;
begin
    Result := SysUtils.StrIComp(Str1, Str2);
end;
{$ENDIF}

function _StrLComp(const Str1, Str2: PAnsiChar; MaxLen: Cardinal): Integer;
begin
    Result := SysUtils.StrLComp(Str1, Str2, MaxLen);
end;

{$IFDEF COMPILER12_UP}
function  _StrLComp(const Str1, Str2: PWideChar; MaxLen: Cardinal): Integer;
(*
{ Currently buggy so use our own implementation // fixed in build 3047}
    function Min(I1, I2: Cardinal): Cardinal; inline;
    begin
        if I1 < I2 then
            Result := I1
        else
            Result := I2;
    end;
*)
begin
    Result :=  SysUtils.StrLComp(Str1, Str2, MaxLen);
  {  Result := CompareStringW(LOCALE_USER_DEFAULT,
                            NORM_IGNOREWIDTH,
                            Str1, Min(MaxLen, SysUtils.StrLen(Str1)),
                            Str2, Min(MaxLen, SysUtils.StrLen(Str2))) - 2; }
end;
{$ENDIF}

function _StrLIComp(const Str1, Str2: PAnsiChar; MaxLen: Cardinal): Integer;
begin
    Result := SysUtils.StrLIComp(Str1, Str2, MaxLen);
end;

{$IFDEF COMPILER12_UP}
function _StrLIComp(const Str1, Str2: PWideChar; MaxLen: Cardinal): Integer;
(*
{ Currently buggy so use our own implementation } // fixed in build 3047}
    function Min(I1, I2: Cardinal): Cardinal; inline;
    begin
        if I1 < I2 then
            Result := I1
        else
            Result := I2;
    end;
*)
begin
    Result :=  SysUtils.StrLIComp(Str1, Str2, MaxLen);
    { Result := CompareStringW(LOCALE_USER_DEFAULT,
                            NORM_IGNOREWIDTH or NORM_IGNORECASE,
                            Str1, Min(MaxLen, SysUtils.StrLen(Str1)),
                            Str2, Min(MaxLen, SysUtils.StrLen(Str2))) - 2;}
end;
{$ENDIF}

{ Author Arno Garrels - Feel free to optimize!                }
{ It's anyway faster than the RTL routine.                    }
function IcsLowerCaseA(const S: AnsiString): AnsiString;
var
    Ch : AnsiChar;
    L, I  : Integer;
    Source, Dest: PAnsiChar;
begin
    L := Length(S);
    if L = 0  then
        Result := ''
    else begin
        SetLength(Result, L);
        Source := Pointer(S);
        Dest := Pointer(Result);
        for I := 1 to L do begin
            Ch := Source^;
            if Ch in ['A'..'Z'] then Inc(Ch, 32);
            Dest^ := Ch;
            Inc(Source);
            Inc(Dest);
        end;
    end;
end;

function _LowerCase(const S: AnsiString): AnsiString;
begin
{$IFDEF USE_ICS_RTL}
    Result := IcsLowerCaseA(S);
{$ELSE}
{$IFNDEF COMPILER12_UP}
    Result := SysUtils.LowerCase(S);
{$ELSE}
    Result := IcsLowerCaseA(S);
{$ENDIF}
{$ENDIF}
end;

{$IFDEF COMPILER12_UP}
function _LowerCase(const S: UnicodeString): UnicodeString;
begin
    Result := SysUtils.LowerCase(S);
end;
{$ENDIF}

{ Author Arno Garrels - Feel free to optimize!                }
{ It's anyway faster than the RTL routine.                    }
function IcsUpperCaseA(const S: AnsiString): AnsiString;
var
    Ch : AnsiChar;
    L, I : Integer;
    Source, Dest: PAnsiChar;
begin
    L := Length(S);
    if L = 0  then
        Result := ''
    else begin
        SetLength(Result, L);
        Source := Pointer(S);
        Dest := Pointer(Result);
        for I := 1 to L do begin
            Ch := Source^;
            if Ch in ['a'..'z'] then Dec(Ch, 32);
            Dest^ := Ch;
            Inc(Source);
            Inc(Dest);
        end;
    end;
end;

function _UpperCase(const S: AnsiString): AnsiString;
begin
{$IFDEF USE_ICS_RTL}
    Result := IcsUpperCaseA(S);
{$ELSE}
{$IFNDEF COMPILER12_UP}
    Result := SysUtils.UpperCase(S);
{$ELSE}
    Result := IcsUpperCaseA(S);
{$ENDIF}
{$ENDIF}
end;

{$IFDEF COMPILER12_UP}
function _UpperCase(const S: UnicodeString): UnicodeString;
begin
    Result := SysUtils.UpperCase(S);
end;
{$ENDIF}

function IcsSameTextA(const S1, S2: AnsiString): Boolean;
begin
    Result := (IcsCompareTextA(S1, S2) = 0);
end;

{ Author Arno Garrels - Feel free to optimize!                }
{ It's anyway faster than the RTL routine.                    }
function IcsCompareTextA(const S1, S2: AnsiString): Integer;
var
    L1, L2, I : Integer;
    MinLen : Integer;
    Ch1, Ch2 : AnsiChar;
    P1, P2 : PAnsiChar;
begin
    L1 := Length(S1);
    L2 := Length(S2);
    if L1 > L2 then
        MinLen := L2
    else
        MinLen := L1;
    P1 := Pointer(S1);
    P2 := Pointer(S2);
    for I := 1 to MinLen do
    begin
        Ch1 := P1[I];
        Ch2 := P2[I];
        if (Ch1 <> Ch2) then
        begin
            { Strange, but this is how the original works, }
            { for instance, "a" is smaller than "[" .      }
            if (Ch1 > Ch2) then
            begin
                if Ch1 in ['a'..'z'] then
                    Dec(Byte(Ch1), 32);
            end
            else begin
                if Ch2 in ['a'..'z'] then
                    Dec(Byte(Ch2), 32);
            end;
        end;
        if (Ch1 <> Ch2) then
        begin
            Result := Byte(Ch1) - Byte(Ch2);
            Exit;
        end;
    end;
    Result := L1 - L2;
end;

function _CompareText(const S1, S2: AnsiString): Integer;
begin
{$IFDEF USE_ICS_RTL}
    Result := IcsCompareTextA(S1, S2);
{$ELSE}
{$IFNDEF COMPILER12_UP}
    Result := SysUtils.CompareText(S1, S2);
{$ELSE}
    Result := IcsCompareTextA(S1, S2);
{$ENDIF}
{$ENDIF}
end;

{$IFDEF COMPILER12_UP}
function _CompareText(const S1, S2: UnicodeString): Integer;
begin
    Result := SysUtils.CompareText(S1, S2);
end;
{$ENDIF}

function _AnsiStrComp(S1, S2: PAnsiChar): Integer;
begin
    Result := SysUtils.AnsiStrComp(S1, S2);
end;

function IcsCompareStrA(const S1, S2: AnsiString): Integer;
var
    L1, L2, I : Integer;
    MinLen    : Integer;
    P1, P2    : PAnsiChar;
begin
    L1 := Length(S1);
    L2 := Length(S2);
    if L1 > L2 then
        MinLen := L2
    else
        MinLen := L1;
    P1 := Pointer(S1);
    P2 := Pointer(S2);
    for I := 1 to MinLen do
    begin
        if (P1[I] <> P2[I]) then
        begin
            Result := Ord(P1[I]) - Ord(P2[I]);
            Exit;
        end;
    end;
    Result := L1 - L2;
end;

function _CompareStr(const S1, S2: AnsiString): Integer;
begin
{$IFDEF USE_ICS_RTL}
    Result := IcsCompareStrA(S1, S2);
{$ELSE}
{$IFNDEF COMPILER12_UP}
    Result := SysUtils.CompareStr(S1, S2);
{$ELSE}
    Result := IcsCompareStrA(S1, S2);
{$ENDIF}
{$ENDIF}
end;

{$IFDEF COMPILER12_UP}
function _CompareStr(const S1, S2: UnicodeString): Integer;
begin
    Result := SysUtils.CompareStr(S1, S2);
end;
{$ENDIF}

function _StringReplace(const S: String; const OldPattern: String;
    const NewPattern: String; Flags: TReplaceFlags): String;
begin
    Result := SysUtils.StringReplace(S, OldPattern, NewPattern, Flags);
end;

function _FileExists(const FileName: String): Boolean;
begin
    Result := SysUtils.FileExists(FileName);
end;

procedure _FreeAndNil(var Obj);
begin
    SysUtils.FreeAndNil(Obj);
end;

function _DeleteFile(const FileName: String): Boolean;
begin
    Result := SysUtils.DeleteFile(FileName);
end;

function _ExtractFileExt(const FileName: String): String;
begin
    Result := SysUtils.ExtractFileExt(FileName);
end;

function _DateTimeToStr(const DateTime: TDateTime): String;
begin
    Result := SysUtils.DateTimeToStr(DateTime);
end;

procedure _DecodeDate(Date: TDateTime; var Year, Month, Day: Word);
begin
    SysUtils.DecodeDate(Date, Year, Month, Day);
end;

procedure _DecodeTime(Time: TDateTime; var Hour, Min, Sec, MSec: Word);
begin
    SysUtils.DecodeTime(Time, Hour, Min, Sec, MSec);
end;

function _EncodeTime(Hour, Min, Sec, MSec: Word): TDateTime;
begin
    Result := SysUtils.EncodeTime(Hour, Min, Sec, MSec);
end;

function _StrToDateTime(const S: String): TDateTime;
begin
    Result := SysUtils.StrToDateTime(S);
end;

function _StrToDateTime(const S: String; const FormatSettings: TFormatSettings): TDateTime;
begin
    Result := SysUtils.StrToDateTime(S, FormatSettings);
end;

function _DirectoryExists(const Name: String): Boolean;
begin
    Result := SysUtils.DirectoryExists(Name);
end;

function _IncludeTrailingPathDelimiter(const S: String): String;
begin
    Result := SysUtils.IncludeTrailingPathDelimiter(S);
end;

function _ExcludeTrailingPathDelimiter(const S: String): String;
begin
    Result := SysUtils.ExcludeTrailingPathDelimiter(S);
end;

function _Now: TDateTime;
begin
    Result := SysUtils.Now;
end;

function _Format(const Fmt: String; const Args: array of const): String;
begin
    Result := SysUtils.Format(Fmt, Args);
end;

function _FindFirst(const Path: String; Attr: Integer; var F: TSearchRec): Integer;
begin
    Result := SysUtils.FindFirst(Path, Attr, F);
end;

function _FindNext(var F: TSearchRec): Integer;
begin
    Result := SysUtils.FindNext(F);
end;

procedure _FindClose(var F: TSearchRec);
begin
    SysUtils.FindClose(F);
end;

function _FileDateToDateTime(FileDate: Integer): TDateTime;
begin
    Result := SysUtils.FileDateToDateTime(FileDate);
end;

function _ExtractFilePath(const FileName: String): String;
begin
    Result := SysUtils.ExtractFilePath(FileName);
end;

function _Date : TDateTime;
begin
    Result := SysUtils.Date;
end;

function _IntToHex(Value: Integer; Digits: Integer): String;
begin
    Result := SysUtils.IntToHex(Value, Digits);
end;

function _IntToHex(Value: Int64; Digits: Integer): String;
begin
    Result := SysUtils.IntToHex(Value, Digits);
end;

{$IFDEF COMPILER16_UP}
function _IntToHex(Value: UInt64; Digits: Integer): String;
begin
    Result := SysUtils.IntToHex(Value, Digits);
end;
{$ENDIF}

function _FloatToStr(Value: Extended): String;
begin
    Result := SysUtils.FloatToStr(Value);
end;

function _GetTickCount: LongWord;
{$IFDEF MSWINDOWS}
begin
    Result := Windows.GetTickCount;
end;
{$ENDIF}
{$IFDEF MACOS}
begin
  Result := AbsoluteToNanoseconds(UpTime) div 1000000;
end;
{$ENDIF MACOS}

function _GetCurrentThreadId: TThreadID;
begin
  {$IFDEF MSWINDOWS}
    Result := Windows.GetCurrentThreadID;
  {$ENDIF}
  {$IFDEF POSIX}
    Result := Posix.Pthread.GetCurrentThreadID;
  {$ENDIF}
end;

function _PostMessage(H: HWND; Msg: UINT; ParamW: WPARAM; ParamL: LPARAM): LongBool;
begin
  {$IFDEF MSWINDOWS}
    Result := Windows.PostMessage(H, Msg, ParamW, ParamL);
  {$ENDIF}
  {$IFDEF POSIX}
    Result := Ics.Posix.Messages.PostMessage(H, Msg, ParamW, ParamL)
  {$ENDIF}
end;

function _SendMessage(H: HWND; Msg: UINT; ParamW: WPARAM; ParamL: LPARAM): LRESULT;
begin
  {$IFDEF MSWINDOWS}
    Result := Windows.SendMessage(H, Msg, ParamW, ParamL);
  {$ENDIF}
  {$IFDEF POSIX}
    Result := Ics.Posix.Messages.SendMessage(H, Msg, ParamW, ParamL)
  {$ENDIF}
end;

procedure _Sleep(dwMilliseconds: LongWord);
begin
  {$IFDEF MSWINDOWS}
    Windows.Sleep(dwMilliseconds);
  {$ENDIF}
  {$IFDEF POSIX}
    SysUtils.Sleep(dwMilliseconds);
  {$ENDIF}
end;

function _FreeLibrary(hLibModule: HMODULE): LongBool;
begin
  {$IFDEF MSWINDOWS}
    Result := Windows.FreeLibrary(hLibModule);
  {$ENDIF}
  {$IFDEF POSIX}
    Result := SysUtils.FreeLibrary(hLibModule);
  {$ENDIF}
end;

{$IFDEF MSWINDOWS}
function _GetTimeZoneInformation(var lpTimeZoneInformation: TTimeZoneInformation): DWORD;
begin
    Result := Windows.GetTimeZoneInformation(lpTimeZoneInformation);
end;

function _GetWindowLong(H: HWND; nIndex: Integer): Longint;
begin
    Result := Windows.GetWindowLong(H, nIndex);
end;

function _DefWindowProc(H: HWND; Msg: UINT; ParamW: WPARAM; ParamL: LPARAM): LRESULT;
begin
    Result := Windows.DefWindowProc(H, Msg, ParamW, ParamL);
end;

function _GetMessage(
    var lpMsg: TMsg; H: HWND;
    wMsgFilterMin, wMsgFilterMax: UINT): BOOL;
begin
    Result := Windows.GetMessage(lpMsg, H, wMsgFilterMin, wMsgFilterMax);
end;

function _TranslateMessage(const lpMsg: TMsg): BOOL;
begin
    Result := Windows.TranslateMessage(lpMsg);
end;

function _DispatchMessage(const lpMsg: TMsg): LongInt;
begin
    Result := Windows.DispatchMessage(lpMsg);
end;

function _PeekMessage(
    var lpMsg: TMsg; H: HWND;
    wMsgFilterMin, wMsgFilterMax, wRemoveMsg: UINT): BOOL;
begin
    Result := Windows.PeekMessage(lpMsg, H,
                                  wMsgFilterMin, wMsgFilterMax, wRemoveMsg);
end;

procedure _EnterCriticalSection(var lpCriticalSection: TRTLCriticalSection);
begin
    Windows.EnterCriticalSection(lpCriticalSection);
end;

function _TryEnterCriticalSection(var lpCriticalSection: TRTLCriticalSection): BOOL;
begin
    Result := Windows.TryEnterCriticalSection(lpCriticalSection);
end;

procedure _LeaveCriticalSection(var lpCriticalSection: TRTLCriticalSection);
begin
    Windows.LeaveCriticalSection(lpCriticalSection);
end;

procedure _InitializeCriticalSection(var lpCriticalSection: TRTLCriticalSection);
begin
    Windows.InitializeCriticalSection(lpCriticalSection);
end;

procedure _DeleteCriticalSection(var lpCriticalSection: TRTLCriticalSection);
begin
    Windows.DeleteCriticalSection(lpCriticalSection);
end;

function _GetClassInfo(
    hInstance: HINST; lpClassName: PChar;
    var lpWndClass: TWndClass): BOOL;
begin
    Result := Windows.GetClassInfo(hInstance, lpClassName, lpWndClass);
end;

function _RegisterClass(const lpWndClass: TWndClass): ATOM;
begin
    Result := Windows.RegisterClass(lpWndClass);
end;

function _CreateWindowEx(dwExStyle: DWORD; lpClassName: PChar;
  lpWindowName: PChar; dwStyle: DWORD; X, Y, nWidth, nHeight: Integer;
  hWndParent: HWND; h_Menu: HMENU; hInstance: HINST; lpParam: Pointer): HWND;
begin
    Result := Windows.CreateWindowEx(dwExStyle, lpClassName,
  lpWindowName, dwStyle, X, Y, nWidth, nHeight, hWndParent, h_Menu, hInstance,
  lpParam);
end;

function _DestroyWindow(H: HWND): BOOL;
begin
    Result := Windows.DestroyWindow(H);
end;

function _SetWindowLong(H: HWND; nIndex: Integer; dwNewLong: Longint): LongInt;
begin
    Result := Windows.SetWindowLong(H, nIndex, dwNewLong);
end;

function _UnregisterClass(lpClassName: PChar; hInstance: HINST): BOOL;
begin
    Result := Windows.UnregisterClass(lpClassName, hInstance);
end;

function _LoadLibrary(lpLibFileName: PChar): HMODULE;
begin
    Result := Windows.LoadLibrary(lpLibFileName);
end;

function _GetProcAddress(hModule: HMODULE; lpProcName: LPCSTR): FARPROC;
begin
    Result := Windows.GetProcAddress(hModule, lpProcName);
end;

function _GetACP: Cardinal;
begin
    Result := Windows.GetACP;
end;

procedure _OutputDebugString(lpOutputString: PChar);
begin
    Windows.OutputDebugString(lpOutputString);
end;

function _IsWindow(hWnd: HWND): BOOL;
begin
    Result := Windows.IsWindow(hWnd);
end;

procedure _GetSystemInfo(var lpSystemInfo: TSystemInfo);
begin
    Windows.GetSystemInfo(lpSystemInfo);
end;
{$ENDIF MSWINDOWS}

{$IFNDEF NOFORMS}
function Application : TApplication;
begin
{$IFNDEF FMX}
    Result := Forms.Application;
{$ELSE}
    Result := FMX.Forms.Application;
{$ENDIF}
end;
{$ENDIF}


end.
