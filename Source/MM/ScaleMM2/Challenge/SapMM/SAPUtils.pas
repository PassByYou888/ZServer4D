{****************************************************************************************

  SAPMM v1.01 /17.06.2013/

  SapMM misc utils

****************************************************************************************}

unit SAPUtils;

interface


function IntToStr(Value: Integer): string;
function IntToHex(Value: Integer; Digits: Integer): string;

function StrToInt(const s: string): Integer;

function SameStr(const s1, s2: string): Boolean;
function StartsStr(const ASubText, AText: string): Boolean;

function FileExists(const FileName: string; FollowLink: Boolean = True): Boolean;


implementation

uses Windows;

//--------------------SysUtils

procedure CvtIntW;
{ IN:
    EAX:  The integer value to be converted to text
    ESI:  Ptr to the right-hand side of the widechar output buffer:  LEA ESI, WStrBuf[32]
    ECX:  Base for conversion: 0 for signed decimal, 10 or 16 for unsigned
    EDX:  Precision: zero padded minimum field width
  OUT:
    ESI:  Ptr to start of converted widechar text (not start of buffer)
    ECX:  Character length of converted text
}
asm  // StackAlignSafe
        OR      CL,CL
        JNZ     @CvtLoop
@C1:    OR      EAX,EAX
        JNS     @C2
        NEG     EAX
        CALL    @C2
        MOV     AX,'-'
        MOV     [ESI-2],AX
        SUB     ESI, 2
        INC     ECX
        RET
@C2:    MOV     ECX,10

@CvtLoop:
        PUSH    EDX
        PUSH    ESI
@D1:    XOR     EDX,EDX
        DIV     ECX
        ADD     DX,'0'
        SUB     ESI,2
        CMP     DX,'0'+10
        JB      @D2
        ADD     DX,('A'-'0')-10
@D2:    MOV     [ESI],DX
        OR      EAX,EAX
        JNE     @D1
        POP     ECX
        POP     EDX
        SUB     ECX,ESI
        SHR     ECX, 1
        SUB     EDX,ECX
        JBE     @D5
        ADD     ECX,EDX
        SUB     ESI,EDX
        MOV     AX,'0'
        SUB     ESI,EDX
        JMP     @z
@zloop: MOV     [ESI+EDX*2],AX
@z:     DEC     EDX
        JNZ     @zloop
        MOV     [ESI],AX
@D5:
end;

function IntToStr(Value: Integer): string;
//  FmtStr(Result, '%d', [Value]);
asm //StackAligned
        PUSH    ESI
        MOV     ESI, ESP
        SUB     ESP, 16 * SizeOf(Char)
        XOR     ECX, ECX       // base: 0 for signed decimal
        PUSH    EDX            // Result ptr
        XOR     EDX, EDX       // zero filled field width: 0 for no leading zeros

{$IFDEF ALIGN_STACK}
        SUB     ESP, 4
{$ENDIF ALIGN_STACK}
        CALL    CvtIntW
{$IFDEF ALIGN_STACK}
        ADD     ESP, 4
{$ENDIF ALIGN_STACK}

        MOV     EDX, ESI
        POP     EAX            // Result ptr

{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        CALL    System.@UStrFromPWCharLen
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
        ADD     ESP, 16 * SizeOf(Char)
        POP     ESI
end;

function IntToHex(Value: Integer; Digits: Integer): string;
//  FmtStr(Result, '%.*x', [Digits, Value]);
asm //StackAligned
        CMP     EDX, 32        // Digits < buffer length?
        JBE     @A1
        XOR     EDX, EDX
@A1:    PUSH    ESI
        MOV     ESI, ESP
        SUB     ESP, 32 * SizeOf(Char)
        PUSH    ECX            // Result ptr
        MOV     ECX, 16        // base 16     EDX = Digits = field width
{$IFDEF ALIGN_STACK}
        SUB     ESP, 4
{$ENDIF ALIGN_STACK}
        CALL    CvtIntW
{$IFDEF ALIGN_STACK}
        ADD     ESP, 4
{$ENDIF ALIGN_STACK}
        MOV     EDX, ESI
        POP     EAX            // Result ptr
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        CALL    System.@UStrFromPWCharLen
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
        ADD     ESP, 32 * SizeOf(Char)
        POP     ESI
end;

function StrToInt(const s: string): Integer;
var
  E: Integer;
begin
  Val(S, Result, E);
  if E <> 0 then Result := 0;
end;


//--------------------------------------------


function SameStr(const s1, s2: string): Boolean;
begin
  Result := Windows.CompareStringW(
              Windows.LOCALE_USER_DEFAULT,
              Windows.NORM_IGNORECASE,
              PChar(S1), Length(S1),
              PChar(S2), Length(S2))
            = 2;
end;

function StartsStr(const ASubText, AText: string): Boolean;
begin
  Result := SameStr(ASubText, Copy(AText, 1, Length(ASubText)));
end;


//----------------------------------------------

const
  faDirectory   = $00000010;
  faSymLink     = $00000400 platform; // Only available on Vista and above


function FileExists(const FileName: string; FollowLink: Boolean = True): Boolean;

  function ExistsLockedOrShared(const Filename: string): Boolean;
  var
    FindData: TWin32FindData;
    LHandle: THandle;
  begin
    { Either the file is locked/share_exclusive or we got an access denied }
    LHandle := FindFirstFile(PChar(Filename), FindData);
    if LHandle <> INVALID_HANDLE_VALUE then
    begin
      Windows.FindClose(LHandle);
      Result := FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY = 0;
    end
    else
      Result := False;
  end;

var
  Flags: Cardinal;
  Handle: THandle;
  LastError: Cardinal;
begin
  Flags := GetFileAttributes(PChar(FileName));

  if Flags <> INVALID_FILE_ATTRIBUTES then
  begin
    if faSymLink and Flags <> 0 then
    begin
      if not FollowLink then
        Exit(True)
      else
      begin
        if faDirectory and Flags <> 0 then
          Exit(False)
        else
        begin
          Handle := CreateFile(PChar(FileName), GENERIC_READ, FILE_SHARE_READ, nil,
            OPEN_EXISTING, 0, 0);
          if Handle <> INVALID_HANDLE_VALUE then
          begin
            CloseHandle(Handle);
            Exit(True);
          end;
          LastError := GetLastError;
          Exit(LastError = ERROR_SHARING_VIOLATION);
        end;
      end;
    end;

    Exit(faDirectory and Flags = 0);
  end;

  LastError := GetLastError;
  Result := (LastError <> ERROR_FILE_NOT_FOUND) and
    (LastError <> ERROR_PATH_NOT_FOUND) and
    (LastError <> ERROR_INVALID_NAME) and ExistsLockedOrShared(Filename);
end;

end.