unit OverbyteIcsWinCrypt;

{ June 2015 - Angus renamed from WinCrypt and moved to main source dir       }

{ note this unit includes a Jedi header file JwaWinCrypt rnmamed to }
{ OverbyteIcsJwaWinCrypt.inc with conditionals to make it independent of JCL }
{ and stop it being compiled alone }

{$WEAKPACKAGEUNIT}

interface

uses
  Windows;

type
  {$EXTERNALSYM LPBYTE}
  LPBYTE      = PByte;
  {$EXTERNALSYM GUID}
  GUID        = TGUID;
  {$EXTERNALSYM PVOID}
  PVOID       = Pointer;
  {$EXTERNALSYM LPVOID}
  LPVOID      = Pointer;
  {$EXTERNALSYM LPLPVOID}
  LPLPVOID    = PPointer;
  {$EXTERNALSYM LPLPSTR}
  LPLPSTR     = PLPSTR;
  {$EXTERNALSYM LPLPWSTR}
  LPLPWSTR    = PLPWSTR;
  {$EXTERNALSYM LPLPCSTR}
  LPLPCSTR    = ^LPCSTR;
  {$EXTERNALSYM LPLPCWSTR}
  LPLPCWSTR   = ^LPCWSTR;
  {$EXTERNALSYM LPLPCTSTR}
  LPLPCTSTR   = ^LPCTSTR;
{$IFNDEF WIN64}
  {$EXTERNALSYM ULONG_PTR}
  ULONG_PTR   = LongWord;
  {$EXTERNALSYM size_t}
  size_t      = LongWord;
{$ELSE}
  {$EXTERNALSYM ULONG_PTR}
  ULONG_PTR   = NativeUInt;
  {$EXTERNALSYM size_t}
  size_t      = NativeUInt;
{$ENDIF}
  {$EXTERNALSYM LPINT}
  LPINT       = ^Integer;
  {$EXTERNALSYM LPFILETIME}
  LPFILETIME  = PFileTime;
  {$EXTERNALSYM LONG}
  LONG        = Longint;
  {$EXTERNALSYM HANDLE}
  HANDLE      = THANDLE;

{$DEFINE JWA_INCLUDEMODE}
{$DEFINE JWA_INTERFACESECTION}
{$I OverbyteIcsJwaWinCrypt.inc}

implementation

const
  advapi32  = 'advapi32.dll';
  crypt32   = 'crypt32.dll';
  softpub   = 'softpub.dll';
{$IFDEF UNICODE}
  AWSuffix = 'W';
{$ELSE}
  AWSuffix = 'A';
{$ENDIF UNICODE}

{$UNDEF JWA_INTERFACESECTION}
{$DEFINE JWA_IMPLEMENTATIONSECTION}
{$I OverbyteIcsJwaWinCrypt.inc}

end.
