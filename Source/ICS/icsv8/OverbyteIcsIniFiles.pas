{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Arno Garrels <arno.garrels@gmx.de>
Creation:     Nov 10, 2008
Description:  Classes and little helpers for use with the ICS demo applications.
Version:      8.01
EMail:        http://www.overbyte.be       francois.piette@overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2015 by François PIETTE
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
                 to Francois PIETTE. Use a nice stamp and mention your name,
                 street address, EMail address and any comment you like to say.

        Contains TIcsUtf8IniFile a variant of TMemIniFile that is capable to
        handle UTF-8 encoded INI files. By default it attempts to preserve ANSI
        format.

History:
Dec 03, 2009 V7.01 Uses TIcsStreamReader and TIcsStreamWriter.
Oct 11, 2010 V7.02 Added methods ReadStrings and WriteStrings from MailSnd demo.
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory
June 2015 - V8.01 Angus moved to main source dir

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsIniFiles;

interface

{$Q-}           { Disable overflow checking           }
{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$X+}           { Enable extended syntax              }
{$H+}           { Use long Strings                    }
{$J+}           { Allow typed constant to be modified }
{$I ..\Source\Include\OverbyteIcsDefs.inc}
{$IFDEF DELPHI6_UP}
    {$WARN SYMBOL_PLATFORM   OFF}
    {$WARN SYMBOL_LIBRARY    OFF}
    {$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}
{$IFDEF BCB3_UP}
    {$ObjExportAll On}
{$ENDIF}

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  SysUtils, Classes, IniFiles,
  OverbyteIcsStreams,
  OverbyteIcsUtils;

type
  EIcsIniFile = class(Exception);
  THackHashedStringList = class(THashedStringList);
  TIcsUtf8IniFile = class(TCustomIniFile)
  private
    FFileName: String;
    FSections: TStringList;
    FPreserveAnsi: Boolean;
    function  AddSection(const Section: String): TStrings;
    function  GetCaseSensitive: Boolean;
    procedure SetCaseSensitive(Value: Boolean);
  protected
    procedure LoadValues; virtual;  
  public
    constructor Create(const FileName: String;
      PreserveAnsi: Boolean = TRUE);
    destructor Destroy; override;
    procedure Clear;
    procedure DeleteKey(const Section, Ident: String); override;
    procedure EraseSection(const Section: String); override;
    procedure GetStrings(List: TStrings);
    procedure ReadSection(const Section: String; Strings: TStrings); override;
    procedure ReadSections(Strings: TStrings); override;
    procedure ReadSectionValues(const Section: String; Strings: TStrings); override;
    function  ReadString(const Section, Ident, Default: String): String; override;
    procedure Rename(const FileName: String; Reload: Boolean);
    procedure SetStrings(List: TStrings);
    procedure UpdateFile; override;
    procedure WriteString(const Section, Ident, Value: String); override;
    function  ReadStrings(const Section, Ident: String; Strings: TStrings): Boolean;
    procedure WriteStrings(const Section, Ident: String; Strings: TStrings);
    property  CaseSensitive: Boolean read GetCaseSensitive write SetCaseSensitive;
  end;

  TIcsIniFile = TIcsUtf8IniFile;

{$IFDEF MSWINDOWS}
  function GetCommonAppDataFolder(const SubPath: String): String;
{$ENDIF}
  function GetIcsIniFileName: String;

implementation

{ TIcsUtf8IniFile }

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TIcsUtf8IniFile.Create(const FileName: String; PreserveAnsi: Boolean = TRUE);
begin
    inherited Create(FileName);
    FPreserveAnsi := PreserveAnsi;
    FSections := THackHashedStringList.Create;
    LoadValues;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TIcsUtf8IniFile.Destroy;
begin
    if FSections <> nil then
        Clear;
    FSections.Free;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsUtf8IniFile.AddSection(const Section: String): TStrings;
begin
    Result := THackHashedStringList.Create;
    try
        THackHashedStringList(Result).CaseSensitive := CaseSensitive;
        FSections.AddObject(Section, Result);
    except
        Result.Free;
        raise;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsUtf8IniFile.Clear;
var
    I: Integer;
begin
    for I := 0 to FSections.Count - 1 do
        TObject(FSections.Objects[I]).Free;
    FSections.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsUtf8IniFile.DeleteKey(const Section, Ident: String);
var
    I, J: Integer;
    Strings: TStrings;
begin
    I := FSections.IndexOf(Section);
    if I >= 0 then begin
        Strings := TStrings(FSections.Objects[I]);
        J := Strings.IndexOfName(Ident);
        if J >= 0 then
            Strings.Delete(J);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsUtf8IniFile.EraseSection(const Section: String);
var
    I: Integer;
begin
    I := FSections.IndexOf(Section);
    if I >= 0 then begin
        TStrings(FSections.Objects[I]).Free;
        FSections.Delete(I);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsUtf8IniFile.GetCaseSensitive: Boolean;
begin
    Result := FSections.CaseSensitive;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsUtf8IniFile.GetStrings(List: TStrings);
var
    I, J: Integer;
    Strings: TStrings;
begin
    List.BeginUpdate;
    try
        for I := 0 to FSections.Count - 1 do begin
            List.Add('[' + FSections[I] + ']');
            Strings := TStrings(FSections.Objects[I]);
            for J := 0 to Strings.Count - 1 do List.Add(Strings[J]);
                List.Add('');
        end;
    finally
        List.EndUpdate;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsUtf8IniFile.LoadValues;
var
    List   : TStringList;
    Reader : TIcsStreamReader;
    S      : String;
begin
    if (FileName <> '') and FileExists(FileName) then
    begin
        List := TStringList.Create;
        try
            Reader := TIcsStreamReader.Create(FileName);
            try
                while Reader.ReadLine(S) do begin
                {$IFNDEF UNICODE}
                    if (Reader.CurrentCodePage = CP_UTF8) then
                        List.Add(Utf8ToStringA(S))
                    else
                {$ENDIF}
                    List.Add(S);
                end;
            finally
                Reader.Free;
            end;
            SetStrings(List);
        finally
            List.Free;
        end;
    end
    else
        Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsUtf8IniFile.ReadSection(const Section: String;
  Strings: TStrings);
var
    I, J: Integer;
    SectionStrings: TStrings;
begin
    Strings.BeginUpdate;
    try
        Strings.Clear;
        I := FSections.IndexOf(Section);
        if I >= 0 then begin
            SectionStrings := TStrings(FSections.Objects[I]);
            for J := 0 to SectionStrings.Count - 1 do
                Strings.Add(SectionStrings.Names[J]);
        end;
    finally
        Strings.EndUpdate;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsUtf8IniFile.ReadSections(Strings: TStrings);
begin
    Strings.Assign(FSections);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsUtf8IniFile.ReadSectionValues(const Section: String;
  Strings: TStrings);
var
    I: Integer;
begin
    Strings.BeginUpdate;
    try
        Strings.Clear;
        I := FSections.IndexOf(Section);
        if I >= 0 then
            Strings.Assign(TStrings(FSections.Objects[I]));
    finally
        Strings.EndUpdate;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TIcsUtf8IniFile.ReadString(const Section, Ident,
  Default: String): String;
var
    I: Integer;
    Strings: TStrings;
begin
    I := FSections.IndexOf(Section);
    if I >= 0 then begin
        Strings := TStrings(FSections.Objects[I]);
        I := Strings.IndexOfName(Ident);
        if I >= 0 then begin
            Result := Copy(Strings[I], Length(Ident) + 2, Maxint);
            Exit;
        end;
    end;
    Result := Default;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsUtf8IniFile.WriteStrings(
    const Section : String;
    const Ident   : String;
    Strings       : TStrings);
var
    nItem   : Integer;
begin
    if (Section = '') or (Ident = '') or (not Assigned(Strings)) then
        Exit;
    EraseSection(Section);
    if Strings.Count <= 0 then
        WriteString(Section, Ident + 'EmptyFlag', 'Empty')
    else
        for nItem := 0 to Strings.Count - 1 do
            WriteString(Section,
                        Ident + IntToStr(nItem),
                        Strings.Strings[nItem]);
end;

function TIcsUtf8IniFile.ReadStrings(
    const Section  : String;
    const Ident    : String;
    Strings        : TStrings) : Boolean;
var
    nItem   : Integer;
    I       : Integer;
    Buf     : String;
    Temp    : TStrings;
begin
    Result := TRUE;
    if (Section = '') or (Ident = '') or (not Assigned(Strings)) then
        Exit;
    Strings.Clear;
    Temp := TStringList.Create;
    try
        if ReadString(Section, Ident + 'EmptyFlag', '') <> '' then
            Exit;
        ReadSectionValues(Section, Temp);
        nItem := Temp.Count - 1;
        while nItem >= 0 do begin
            Buf := Temp.Strings[nItem];
            if CompareText(Ident, Copy(Buf, 1, Length(Ident))) <> 0 then
                Temp.Delete(nItem)
            else begin
                if (Ord(Buf[Length(Ident) + 1]) < Ord('0')) or
                   (Ord(Buf[Length(Ident) + 1]) > Ord('9')) then
                    Temp.Delete(nItem)
                else begin
                    I := Pos('=', Buf);
                    Temp.Strings[nItem] := Copy(Buf, I + 1, Length(Buf));
                end;
            end;
            Dec(nItem);
        end;
        Strings.Assign(Temp);
        Result := (Temp.Count > 0);
    finally
        Temp.Free;
    end;
end;

procedure TIcsUtf8IniFile.Rename(const FileName: String; Reload: Boolean);
begin
  FFileName := FileName;
  if Reload then
    LoadValues;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsUtf8IniFile.SetCaseSensitive(Value: Boolean);
var
    I: Integer;
begin
    if Value <> FSections.CaseSensitive then begin
        FSections.CaseSensitive := Value;
        for I := 0 to FSections.Count - 1 do
            with THackHashedStringList(FSections.Objects[I]) do begin
                CaseSensitive := Value;
                Changed;
            end;
        THackHashedStringList(FSections).Changed;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsUtf8IniFile.SetStrings(List: TStrings);
var
    I, J: Integer;
    S: String;
    Strings: TStrings;
begin
    Clear;
    Strings := nil;
    for I := 0 to List.Count - 1 do begin
        S := Trim(List[I]);
        if (S <> '') and (S[1] <> ';') then
            if (S[1] = '[') and (S[Length(S)] = ']') then begin
                Delete(S, 1, 1);
                SetLength(S, Length(S)-1);
                Strings := AddSection(Trim(S));
            end
            else
            if Strings <> nil then begin
                J := Pos('=', S);
                if J > 0 then // remove spaces before and after '='
                    Strings.Add(Trim(Copy(S, 1, J-1)) + '=' + Trim(Copy(S, J+1, MaxInt)) )
                else
                    Strings.Add(S);
            end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsUtf8IniFile.UpdateFile;
var
    List   : TStringList;
    Writer : TIcsStreamWriter;
    I      : Integer;
begin
    List := TStringList.Create;
    try
        GetStrings(List);
        Writer := TIcsStreamWriter.Create(FileName, FALSE, FALSE);
        try
            if not FPreserveAnsi then begin
                { If plain ASCII text (<= #127) do not convert to UTF-8 }
                for I := 0 to List.Count - 1 do
                    if not IsUsAscii(List[I]) then begin
                        Writer.CurrentCodePage := CP_UTF8;
                        Break;
                    end;
            end
            else begin
            {$IFDEF UNICODE}
                { If the Unicode String can be represented in current ANSI }
                { code page do not convert to UTF-8.                       }
                for I := 0 to List.Count - 1 do
                    if not CheckUnicodeToAnsi(List[I]) then begin
                        Writer.CurrentCodePage := CP_UTF8;
                        Break;
                    end;
            {$ENDIF}
            end;
            if Writer.CurrentCodePage = CP_UTF8 then
                Writer.WriteBOM;
            for I := 0 to List.Count - 1 do
                Writer.WriteLine(List[I]);
        finally
            Writer.Free;
        end;
    finally
        List.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIcsUtf8IniFile.WriteString(const Section, Ident, Value: String);
var
    I: Integer;
    S: String;
    Strings: TStrings;
begin
    I := FSections.IndexOf(Section);
    if I >= 0 then
        Strings := TStrings(FSections.Objects[I])
    else
        Strings := AddSection(Section);
    S := Ident + '=' + Value;
    I := Strings.IndexOfName(Ident);
    if I >= 0 then
        Strings[I] := S
    else
        Strings.Add(S);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF MSWINDOWS}
function GetCommonAppDataFolder(const SubPath: String): String;
var
    hSHFolderDLL: HMODULE;
    f_SHGetFolderPath: function(hwndOwner: HWND; nFolder: Integer;
        hToken: THandle; dwFlags: DWORD; pszPath: PChar): HRESULT; stdcall;
    Buf: array[0..MAX_PATH - 1] of Char;
const
    CSIDL_LOCAL_APPDATA = $001C;
    SHGFP_TYPE_CURRENT  = 0;
begin
    Result := '';
    hSHFolderDLL := LoadLibrary('shfolder.dll');
    if hSHFolderDLL = 0 then
        Exit;
    try
    {$IFDEF UNICODE}
        @f_SHGetFolderPath := GetProcAddress(hSHFolderDLL, 'SHGetFolderPathW');
    {$ELSE}
        @f_SHGetFolderPath := GetProcAddress(hSHFolderDLL, 'SHGetFolderPathA');
    {$ENDIF}
        if @f_SHGetFolderPath = nil then
            Exit;
        if Succeeded(f_SHGetFolderPath(0, CSIDL_LOCAL_APPDATA, 0,
                                       SHGFP_TYPE_CURRENT, Buf)) then begin
            Result := ExpandFileName(Buf);
            Result := IncludeTrailingPathDelimiter(Result) + SubPath;
            try
                if not ForceDirectories(Result) then
                    Result := '';
            except
                Result := '';
            end;
        end;
    finally
        FreeLibrary(hSHFolderDLL);
    end;
end;
{$ENDIF MSWINDOWS}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetIcsIniFileName: String;
begin
  {$IFDEF MSWINDOWS}
    Result := GetCommonAppDataFolder('ICS');
  {$ELSE}
    Result := IncludeTrailingPathDelimiter(GetHomePath) + 'ICS';
    if not ForceDirectories(Result) then
        Result := '';
  {$ENDIF}
    if Result = '' then
        Result := ChangeFileExt(ParamStr(0), '.ini')
    else
        Result := IncludeTrailingPathDelimiter(Result) +
                        ExtractFileName(ChangeFileExt(ParamStr(0), '.ini'));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
end.
