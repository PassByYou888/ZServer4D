{ ***************************************************************************** }
{ * parsing library,writen by QQ 600585@qq.com                                * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ ****************************************************************************** }

unit TextParsing;

{$INCLUDE zDefine.inc}

interface

uses SysUtils, Types, CoreClasses, PascalStrings, UnicodeMixedLib, ListEngine;

type
  TTextStyle = (tsPascal, tsC, tsText);

  TTokenType = (ttTextDecl, ttComment, ttNumber, ttSymbol, ttAscii, ttSpecialSymbol, ttUnknow);

  TTokenStatistics = array [TTokenType] of Integer;

  TTextPos = packed record
    bPos, ePos: Integer;
    Text: TPascalString;
  end;

  PTextPos = ^TTextPos;

  TTokenData = packed record
    bPos, ePos: Integer;
    Text: TPascalString;
    tokenType: TTokenType;
    idx: Integer;
  end;

  PTokenData = ^TTokenData;

  TTextParsingCache = packed record
    CommentDecls, TextDecls: TCoreClassList; // PTextPos
    TokenDataList: TCoreClassList;           // PTokenData
  end;

  TTextParsingData = packed record
    Cache: TTextParsingCache;
    Text: TPascalString;
    Len: Integer;
  end;

  TTextParsing = class(TCoreClassObject)
  public
    TextStyle: TTextStyle;
    ParsingData: TTextParsingData;
    SymbolTable: TPascalString;
    TokenStatistics: TTokenStatistics;
    SpecialSymbol: TListPascalString;

    function ComparePosStr(const cOffset: Integer; const T: TPascalString): Boolean; overload; inline;
    function ComparePosStr(const cOffset: Integer; const p: PPascalString): Boolean; overload; inline;
    { }
    function CompareCommentGetEndPos(const cOffset: Integer): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function CompareTextDeclGetEndPos(const cOffset: Integer): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure RebuildParsingCache;
    procedure RebuildText;
    procedure RebuildToken;
    { }
    function GetContextBeginPos(const cOffset: Integer): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function GetContextEndPos(const cOffset: Integer): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    { }
    function isSpecialSymbol(const cOffset: Integer): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function isSpecialSymbol(const cOffset: Integer; var speicalSymbolEndPos: Integer): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function GetSpecialSymbolEndPos(const cOffset: Integer): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    { }
    function isNumber(const cOffset: Integer): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function isNumber(const cOffset: Integer; var NumberBegin: Integer; var IsHex: Boolean): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function GetNumberEndPos(const cOffset: Integer): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    { }
    function isTextDecl(const cOffset: Integer): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function GetTextDeclEndPos(const cOffset: Integer): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function GetTextDeclBeginPos(const cOffset: Integer): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function GetTextBody(const AText: TPascalString): TPascalString; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function GetTextDeclPos(const cOffset: Integer; var charBeginPos, charEndPos: Integer): Boolean;
    { }
    function isSymbol(const cOffset: Integer): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function GetSymbolEndPos(const cOffset: Integer): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    { }
    function isAscii(const cOffset: Integer): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function GetAsciiBeginPos(const cOffset: Integer): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function GetAsciiEndPos(const cOffset: Integer): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    { }
    function isComment(const cOffset: Integer): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function GetCommentEndPos(const cOffset: Integer): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function GetCommentBeginPos(const cOffset: Integer): Integer; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function GetCommentPos(const cOffset: Integer; var charBeginPos, charEndPos: Integer): Boolean;
    function GetDeletedCommentText: TPascalString; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    { }
    function isTextOrComment(const cOffset: Integer): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function isCommentOrText(const cOffset: Integer): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    { }
    function isWordSplitChar(const C: SystemChar; SplitTokenC: TPascalString): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function isWordSplitChar(const C: SystemChar): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function isWordSplitChar(const C: SystemChar; DefaultChar: Boolean; SplitTokenC: TPascalString): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    { }
    function GetWordBeginPos(const cOffset: Integer; SplitTokenC: TPascalString): Integer; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function GetWordBeginPos(const cOffset: Integer): Integer; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function GetWordBeginPos(const cOffset: Integer; BeginDefaultChar: Boolean; SplitTokenC: TPascalString): Integer; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    { }
    function GetWordEndPos(const cOffset: Integer; SplitTokenC: TPascalString): Integer; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function GetWordEndPos(const cOffset: Integer): Integer; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function GetWordEndPos(const cOffset: Integer; BeginSplitCharSet, EndSplitCharSet: TPascalString): Integer; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function GetWordEndPos(const cOffset: Integer;
      BeginDefaultChar: Boolean; BeginSplitCharSet: TPascalString;
      EndDefaultChar: Boolean; EndSplitCharSet: TPascalString): Integer; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    { }
    function CompareTokenText(const cOffset: Integer; T: TPascalString): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function CompareTokenChar(const cOffset: Integer; const C: array of SystemChar): Boolean;
    function GetToken(const cOffset: Integer): PTokenData;
    property TokenPos[const cOffset: Integer]: PTokenData read GetToken;
    function GetTokenIndex(T: TTokenType; idx: Integer): PTokenData; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    property TokenIndex[T: TTokenType; idx: Integer]: PTokenData read GetTokenIndex;
    function TokenCount: Integer;
    function GetTokens(idx: Integer): PTokenData;
    property Tokens[idx: Integer]: PTokenData read GetTokens;
    { }
    function SniffingNextChar(const cOffset: Integer; declChar: TPascalString): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function SniffingNextChar(const cOffset: Integer; declChar: TPascalString; out OutPos: Integer): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    { }
    function SplitChar(const cOffset: Integer; var LastPos: Integer; const SplitTokenC, SplitEndTokenC: TPascalString; var SplitOutput: TArrayPascalString): Integer; overload;
    function SplitChar(const cOffset: Integer; const SplitTokenC, SplitEndTokenC: TPascalString; var SplitOutput: TArrayPascalString): Integer; overload;
    { }
    function SplitString(const cOffset: Integer; var LastPos: Integer; const SplitTokenS, SplitEndTokenS: TPascalString; var SplitOutput: TArrayPascalString): Integer; overload;
    function SplitString(const cOffset: Integer; const SplitTokenS, SplitEndTokenS: TPascalString; var SplitOutput: TArrayPascalString): Integer; overload;
    { }
    function GetStr(const bPos, ePos: Integer): TPascalString; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function GetStr(const tp: TTextPos): TPascalString; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    { }
    function GetWordStr(const cOffset: Integer): TPascalString; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    { }
    function GetPoint(const cOffset: Integer): TPoint; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function GetChar(const cOffset: Integer): SystemChar; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    property Len: Integer read ParsingData.Len;
    { }
    procedure DeletePos(const bPos, ePos: Integer); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure DeletePos(const tp: TTextPos); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    { }
    procedure DeletedComment; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    { }
    procedure InsertTextBlock(const bPos, ePos: Integer; AInsertText: TPascalString); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure InsertTextBlock(const tp: TTextPos; AInsertText: TPascalString); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    { }
    function SearchWordBody(initPos: Integer; wordInfo: TPascalString; var OutPos: TTextPos): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    { }
    { string declaration }
    class function TranslatePascalDeclToText(const Text: TPascalString): TPascalString;
    class function TranslateTextToPascalDecl(const Text: TPascalString): TPascalString;
    class function TranslateTextToPascalDeclWithUnicode(const Text: TPascalString): TPascalString;
    class function TranslateC_DeclToText(const Text: TPascalString): TPascalString;
    class function TranslateTextToC_Decl(const Text: TPascalString): TPascalString;
    { comment declaration }
    class function TranslatePascalDeclCommentToText(const Text: TPascalString): TPascalString;
    class function TranslateTextToPascalDeclComment(const Text: TPascalString): TPascalString;
    class function TranslateC_DeclCommentToText(const Text: TPascalString): TPascalString;
    class function TranslateTextToC_DeclComment(const Text: TPascalString): TPascalString;
    { }
    constructor Create(const AText: TPascalString; AStyle: TTextStyle; ASpecialSymbol: TListPascalString); overload; virtual;
    constructor Create(const AText: TPascalString; AStyle: TTextStyle); overload;
    constructor Create(const AText: TPascalString; AStyle: TTextStyle; ASpecialSymbol: TListPascalString; ASpacerSymbol: SystemString); overload;
    destructor Destroy; override;
    { }
    function Parsing: Boolean; virtual;
  end;

  TTextParsingClass = class of TTextParsing;

const
  C_SpacerSymbol = #44#46#43#45#42#47#40#41#59#58#61#35#64#94#38#37#33#34#91#93#60#62#63#123#125#39#36;

var
  V_SpacerSymbol: SystemString = C_SpacerSymbol;

implementation

const
  NullTokenStatistics: TTokenStatistics = (0, 0, 0, 0, 0, 0, 0);

type
  TCTranslateStruct = packed record
    s: SystemChar;
    C: SystemString;
  end;

const
  CTranslateTable: array [0 .. 11] of TCTranslateStruct = (
    (s: #007; C: '\a'),
    (s: #008; C: '\b'),
    (s: #012; C: '\f'),
    (s: #010; C: '\n'),
    (s: #013; C: '\r'),
    (s: #009; C: '\t'),
    (s: #011; C: '\v'),
    (s: #092; C: '\\'),
    (s: #063; C: '\?'),
    (s: #039; C: '\'#39),
    (s: #034; C: '\"'),
    (s: #000; C: '\0'));

function TTextParsing.ComparePosStr(const cOffset: Integer; const T: TPascalString): Boolean;
begin
  Result := ParsingData.Text.ComparePos(cOffset, T);
end;

function TTextParsing.ComparePosStr(const cOffset: Integer; const p: PPascalString): Boolean;
begin
  Result := ParsingData.Text.ComparePos(cOffset, p);
end;

function TTextParsing.CompareCommentGetEndPos(const cOffset: Integer): Integer;
var
  L: Integer;
  cPos: Integer;
begin
  L := ParsingData.Len;
  cPos := cOffset;
  if cPos < 1 then
      cPos := 1;
  if cPos > L then
      cPos := L;

  Result := cPos;

  if ComparePosStr(Result, '//') then
    begin
      Inc(Result, 2);
      while ParsingData.Text[Result] <> #10 do
        begin
          if Result + 1 > L then
              Break;
          Inc(Result);
        end;
    end
  else if (TextStyle = tsC) and (ComparePosStr(Result, '#')) then
    begin
      Inc(Result, 1);
      while ParsingData.Text[Result] <> #10 do
        begin
          if Result + 1 > L then
              Break;
          Inc(Result);
        end;
    end
  else if (TextStyle = tsC) and (ComparePosStr(Result, '/*')) then
    begin
      Inc(Result, 2);
      while not ComparePosStr(Result, '*/') do
        begin
          if Result + 1 > L then
              Break;
          Inc(Result);
        end;
      Inc(Result, 2);
    end
  else if (TextStyle = tsPascal) and (ComparePosStr(Result, '{')) then
    begin
      Inc(Result, 1);
      while ParsingData.Text[Result] <> '}' do
        begin
          if Result + 1 > L then
              Break;
          Inc(Result);
        end;
      Inc(Result, 1);
    end
  else if (TextStyle = tsPascal) and (ComparePosStr(Result, '(*')) then
    begin
      Inc(Result, 2);
      while not ComparePosStr(Result, '*)') do
        begin
          if Result + 1 > L then
              Break;
          Inc(Result);
        end;
      Inc(Result, 2);
    end;
end;

function TTextParsing.CompareTextDeclGetEndPos(const cOffset: Integer): Integer;
var
  L: Integer;
  cPos: Integer;
  tmpPos: Integer;
begin
  L := ParsingData.Len;
  cPos := cOffset;
  if cPos < 1 then
      cPos := 1;
  if cPos > L then
      cPos := L;

  if (cPos + 1 < L) and (TextStyle = tsPascal) and (ParsingData.Text[cPos] = #39) then
    begin
      if ComparePosStr(cPos, #39#39#39#39) then
        begin
          cPos := CompareTextDeclGetEndPos(cPos + 4);
          Exit;
        end;
      Inc(cPos, 1);
      while ParsingData.Text[cPos] <> #39 do
        begin
          if cPos + 1 > L then
              Break;
          if ParsingData.Text[cPos] = #10 then
              Exit(cPos);
          Inc(cPos);
        end;
      Inc(cPos, 1);
    end;

  if (cPos + 1 < L) and (TextStyle = tsC) and (ParsingData.Text[cPos] = #39) then
    begin
      Inc(cPos, 1);
      while ParsingData.Text[cPos] <> #39 do
        begin
          if ComparePosStr(cPos, '\' + #39) then
              Inc(cPos, 1);
          if cPos + 1 > L then
              Break;
          if ParsingData.Text[cPos] = #10 then
              Exit(cPos);
          Inc(cPos);
        end;
      Inc(cPos, 1);
    end;

  if (cPos + 1 < L) and (TextStyle = tsC) and (ParsingData.Text[cPos] = '"') then
    begin
      Inc(cPos, 1);
      while ParsingData.Text[cPos] <> '"' do
        begin
          if ComparePosStr(cPos, '\"') then
              Inc(cPos, 1);
          if cPos + 1 > L then
              Break;
          if ParsingData.Text[cPos] = #10 then
              Exit(cPos);
          Inc(cPos);
        end;
      Inc(cPos, 1);
    end;

  if (cPos + 1 < L) and (TextStyle = tsPascal) and (ParsingData.Text[cPos] = '#') then
    begin
      repeat
        Inc(cPos, 1);

        while isWordSplitChar(ParsingData.Text[cPos]) do
          begin
            if cPos + 1 > L then
                Exit(cPos);
            Inc(cPos);
          end;
        while CharIn(ParsingData.Text[cPos], [c0to9], '$') do
          begin
            if cPos + 1 > L then
                Exit(cPos);
            Inc(cPos);
          end;
        tmpPos := cPos;
        while isWordSplitChar(ParsingData.Text[cPos]) do
          begin
            if cPos + 1 > L then
                Exit(cPos);
            Inc(cPos);
          end;
      until not ComparePosStr(cPos, '#');
      cPos := CompareTextDeclGetEndPos(tmpPos);
    end;

  Result := cPos;
end;

procedure TTextParsing.RebuildParsingCache;
var
  i: Integer;
  L: Integer;
  bPos: Integer;
  ePos: Integer;
  textPosPtr: PTextPos;
  LastTokenData: PTokenData;
begin
  if ParsingData.Cache.CommentDecls <> nil then
    begin
      for i := 0 to ParsingData.Cache.CommentDecls.Count - 1 do
          Dispose(PTextPos(ParsingData.Cache.CommentDecls[i]));
      DisposeObject(ParsingData.Cache.CommentDecls);
      ParsingData.Cache.CommentDecls := nil;
    end;

  if ParsingData.Cache.TextDecls <> nil then
    begin
      for i := 0 to ParsingData.Cache.TextDecls.Count - 1 do
          Dispose(PTextPos(ParsingData.Cache.TextDecls[i]));
      DisposeObject(ParsingData.Cache.TextDecls);
      ParsingData.Cache.TextDecls := nil;
    end;

  if ParsingData.Cache.TokenDataList <> nil then
    begin
      for i := 0 to ParsingData.Cache.TokenDataList.Count - 1 do
          Dispose(PTokenData(ParsingData.Cache.TokenDataList[i]));
      DisposeObject(ParsingData.Cache.TokenDataList);
      ParsingData.Cache.TokenDataList := nil;
    end;

  TokenStatistics := NullTokenStatistics;

  ParsingData.Cache.CommentDecls := TCoreClassList.Create;
  ParsingData.Cache.TextDecls := TCoreClassList.Create;
  ParsingData.Cache.TokenDataList := TCoreClassList.Create;

  // rebuild comment and text
  L := ParsingData.Len;
  bPos := 1;
  ePos := bPos;
  while (bPos <= L) do
    begin
      ePos := CompareCommentGetEndPos(bPos);
      if ePos > bPos then
        begin
          new(textPosPtr);
          textPosPtr^.bPos := bPos;
          textPosPtr^.ePos := ePos;
          textPosPtr^.Text := GetStr(textPosPtr^);
          ParsingData.Cache.CommentDecls.Add(textPosPtr);
          bPos := ePos;
        end
      else
        begin
          ePos := CompareTextDeclGetEndPos(bPos);
          if ePos > bPos then
            begin
              new(textPosPtr);
              textPosPtr^.bPos := bPos;
              textPosPtr^.ePos := ePos;
              textPosPtr^.Text := GetStr(textPosPtr^);
              ParsingData.Cache.TextDecls.Add(textPosPtr);
              bPos := ePos;
            end
          else
            begin
              Inc(bPos);
              ePos := bPos;
            end;
        end;
    end;

  // rebuild token
  bPos := 1;
  ePos := bPos;
  LastTokenData := nil;
  while bPos <= L do
    begin
      if isSpecialSymbol(bPos, ePos) then
        begin
          new(LastTokenData);
          LastTokenData^.bPos := bPos;
          LastTokenData^.ePos := ePos;
          LastTokenData^.Text := GetStr(bPos, ePos);
          LastTokenData^.tokenType := ttSpecialSymbol;
          LastTokenData^.idx := ParsingData.Cache.TokenDataList.Count;
          ParsingData.Cache.TokenDataList.Add(LastTokenData);
          Inc(TokenStatistics[LastTokenData^.tokenType]);

          bPos := ePos
        end
      else if isTextDecl(bPos) then
        begin
          ePos := GetTextDeclEndPos(bPos);

          new(LastTokenData);
          LastTokenData^.bPos := bPos;
          LastTokenData^.ePos := ePos;
          LastTokenData^.Text := GetStr(bPos, ePos);
          LastTokenData^.tokenType := ttTextDecl;
          LastTokenData^.idx := ParsingData.Cache.TokenDataList.Count;
          ParsingData.Cache.TokenDataList.Add(LastTokenData);
          Inc(TokenStatistics[LastTokenData^.tokenType]);

          bPos := ePos
        end
      else if isComment(bPos) then
        begin
          ePos := GetCommentEndPos(bPos);

          new(LastTokenData);
          LastTokenData^.bPos := bPos;
          LastTokenData^.ePos := ePos;
          LastTokenData^.Text := GetStr(bPos, ePos);
          LastTokenData^.tokenType := ttComment;
          LastTokenData^.idx := ParsingData.Cache.TokenDataList.Count;
          ParsingData.Cache.TokenDataList.Add(LastTokenData);
          Inc(TokenStatistics[LastTokenData^.tokenType]);

          bPos := ePos;
        end
      else if isNumber(bPos) then
        begin
          ePos := GetNumberEndPos(bPos);

          new(LastTokenData);
          LastTokenData^.bPos := bPos;
          LastTokenData^.ePos := ePos;
          LastTokenData^.Text := GetStr(bPos, ePos);
          LastTokenData^.tokenType := ttNumber;
          LastTokenData^.idx := ParsingData.Cache.TokenDataList.Count;
          ParsingData.Cache.TokenDataList.Add(LastTokenData);
          Inc(TokenStatistics[LastTokenData^.tokenType]);

          bPos := ePos;
        end
      else if isSymbol(bPos) then
        begin
          ePos := GetSymbolEndPos(bPos);

          new(LastTokenData);
          LastTokenData^.bPos := bPos;
          LastTokenData^.ePos := ePos;
          LastTokenData^.Text := GetStr(bPos, ePos);
          LastTokenData^.tokenType := ttSymbol;
          LastTokenData^.idx := ParsingData.Cache.TokenDataList.Count;
          ParsingData.Cache.TokenDataList.Add(LastTokenData);
          Inc(TokenStatistics[LastTokenData^.tokenType]);

          bPos := ePos;
        end
      else if isAscii(bPos) then
        begin
          ePos := GetAsciiEndPos(bPos);

          new(LastTokenData);
          LastTokenData^.bPos := bPos;
          LastTokenData^.ePos := ePos;
          LastTokenData^.Text := GetStr(bPos, ePos);
          LastTokenData^.tokenType := ttAscii;
          LastTokenData^.idx := ParsingData.Cache.TokenDataList.Count;
          ParsingData.Cache.TokenDataList.Add(LastTokenData);
          Inc(TokenStatistics[LastTokenData^.tokenType]);

          bPos := ePos;
        end
      else
        begin
          ePos := bPos + 1;

          if (LastTokenData = nil) or (LastTokenData^.tokenType <> ttUnknow) then
            begin
              new(LastTokenData);
              LastTokenData^.bPos := bPos;
              LastTokenData^.ePos := ePos;
              LastTokenData^.Text := GetStr(bPos, ePos);
              LastTokenData^.tokenType := ttUnknow;
              LastTokenData^.idx := ParsingData.Cache.TokenDataList.Count;
              ParsingData.Cache.TokenDataList.Add(LastTokenData);
              Inc(TokenStatistics[LastTokenData^.tokenType]);
            end
          else
            begin
              LastTokenData^.ePos := ePos;
              LastTokenData^.Text.Append(GetChar(bPos));
            end;

          bPos := ePos;
        end;
    end;

end;

procedure TTextParsing.RebuildText;
  procedure Recompute(bPos, d: Integer);
  var
    i: Integer;
    p: PTextPos;
  begin
    for i := 0 to ParsingData.Cache.TextDecls.Count - 1 do
      begin
        p := PTextPos(ParsingData.Cache.TextDecls[i]);
        if bPos < p^.bPos then
          begin
            p^.bPos := p^.bPos - d;
            p^.ePos := p^.ePos - d;
          end;
      end;
    for i := 0 to ParsingData.Cache.CommentDecls.Count - 1 do
      begin
        p := PTextPos(ParsingData.Cache.CommentDecls[i]);
        if bPos < p^.bPos then
          begin
            p^.bPos := p^.bPos - d;
            p^.ePos := p^.ePos - d;
          end;
      end;
  end;

var
  p: PTextPos;
  i: Integer;
begin
  for i := 0 to ParsingData.Cache.TextDecls.Count - 1 do
    begin
      p := PTextPos(ParsingData.Cache.TextDecls[i]);
      if p^.ePos - p^.bPos <> (p^.Text.Len) then
          Recompute(p^.bPos, (p^.ePos - p^.bPos) - p^.Text.Len);

      ParsingData.Text := GetStr(1, p^.bPos) + p^.Text + GetStr(p^.ePos, ParsingData.Text.Len + 1);
      ParsingData.Len := ParsingData.Text.Len;
      p^.ePos := p^.bPos + p^.Text.Len;
    end;
  for i := 0 to ParsingData.Cache.CommentDecls.Count - 1 do
    begin
      p := PTextPos(ParsingData.Cache.CommentDecls[i]);
      if p^.ePos - p^.bPos <> (p^.Text.Len) then
          Recompute(p^.bPos, (p^.ePos - p^.bPos) - p^.Text.Len);

      ParsingData.Text := GetStr(1, p^.bPos) + p^.Text + GetStr(p^.ePos, ParsingData.Text.Len + 1);
      ParsingData.Len := ParsingData.Text.Len;
      p^.ePos := p^.bPos + p^.Text.Len;
    end;

  RebuildParsingCache;
end;

procedure TTextParsing.RebuildToken;
var
  p: PTokenData;
  i: Integer;
begin
  ParsingData.Text := '';
  for i := 0 to ParsingData.Cache.TokenDataList.Count - 1 do
    begin
      p := PTokenData(ParsingData.Cache.TokenDataList[i]);
      ParsingData.Text.Append(p^.Text);
    end;
  ParsingData.Len := ParsingData.Text.Len;
  RebuildParsingCache;
end;

function TTextParsing.GetContextBeginPos(const cOffset: Integer): Integer;
var
  cPos, ePos: Integer;
  L: Integer;
begin
  cPos := cOffset;
  L := ParsingData.Len;
  if cPos < 1 then
      cPos := 1;
  if cPos > L then
      cPos := L;

  while cPos < L do
    begin
      if isTextDecl(cPos) then
        begin
          cPos := GetTextDeclBeginPos(cPos);
          Break;
        end
      else if isComment(cPos) then
        begin
          cPos := GetCommentBeginPos(cPos);
          Break;
        end
      else if isNumber(cPos) then
        begin
          Break;
        end
      else if isSymbol(cPos) then
        begin
          Break;
        end
      else if isAscii(cPos) then
        begin
          cPos := GetAsciiBeginPos(cPos);
          Break;
        end;
      Inc(cPos);
    end;

  Result := cPos;
end;

function TTextParsing.GetContextEndPos(const cOffset: Integer): Integer;
var
  cPos: Integer;
  L: Integer;
begin
  cPos := cOffset;
  L := ParsingData.Len;
  if cPos < 1 then
      cPos := 1;
  if cPos > L then
      cPos := L;

  while cPos < L do
    begin
      if isTextDecl(cPos) then
        begin
          cPos := GetTextDeclEndPos(cPos);
          Break;
        end
      else if isComment(cPos) then
        begin
          cPos := GetCommentEndPos(cPos);
          Break;
        end
      else if isNumber(cPos) then
        begin
          cPos := GetNumberEndPos(cPos);
          Break;
        end
      else if isSymbol(cPos) then
        begin
          cPos := GetSymbolEndPos(cPos);
          Break;
        end
      else if isAscii(cPos) then
        begin
          cPos := GetAsciiEndPos(cPos);
          Break;
        end;
      Inc(cPos);
    end;

  Result := cPos;
end;

function TTextParsing.isSpecialSymbol(const cOffset: Integer): Boolean;
var
  ePos: Integer;
begin
  Result := isSpecialSymbol(cOffset, ePos);
end;

function TTextParsing.isSpecialSymbol(const cOffset: Integer; var speicalSymbolEndPos: Integer): Boolean;
var
  i, EP: Integer;
begin
  Result := False;
  speicalSymbolEndPos := cOffset;

  if SpecialSymbol.Count = 0 then
      Exit;

  if isComment(cOffset) then
      Exit;

  if isTextDecl(cOffset) then
      Exit;

  speicalSymbolEndPos := cOffset;
  for i := 0 to SpecialSymbol.Count - 1 do
    if ComparePosStr(cOffset, SpecialSymbol.Items_PPascalString[i]) then
      begin
        EP := cOffset + SpecialSymbol[i].Len;
        if EP > speicalSymbolEndPos then
            speicalSymbolEndPos := EP;
        Result := True;
      end;
end;

function TTextParsing.GetSpecialSymbolEndPos(const cOffset: Integer): Integer;
begin
  if not isSpecialSymbol(cOffset, Result) then
      Result := cOffset;
end;

function TTextParsing.isNumber(const cOffset: Integer): Boolean;
var
  tmp: Integer;
  IsHex: Boolean;
begin
  Result := isNumber(cOffset, tmp, IsHex);
end;

function TTextParsing.isNumber(const cOffset: Integer; var NumberBegin: Integer; var IsHex: Boolean): Boolean;
var
  C: SystemChar;
  L: Integer;
  cPos, bkPos: Integer;
  NC: Integer;
  dotCount: Integer;
  eCnt: Integer;
  AddSubSymCnt: Integer;
begin
  Result := False;

  cPos := cOffset;
  L := ParsingData.Len;
  if cPos < 1 then
      cPos := 1;
  if cPos > L then
      cPos := L;

  if cPos = L then
      Exit;

  IsHex := False;
  try
    if (CharIn(ParsingData.Text[cPos], '$')) then
      begin
        // pascal style hex
        IsHex := True;
        Inc(cPos);
        if cPos > L then
            Exit;
      end
    else if ComparePosStr(cPos, '0x') then
      begin
        // c style hex
        IsHex := True;
        Inc(cPos, 2);
        if cPos > L then
            Exit;
      end;
  except
  end;

  if IsHex then
    begin
      bkPos := cPos;
      NC := 0;
      while True do
        begin
          cPos := GetTextDeclEndPos(GetCommentEndPos(cPos));

          if cPos > L then
              Break;
          C := ParsingData.Text[cPos];

          if isWordSplitChar(C, True, SymbolTable) then
            begin
              if NC > 0 then
                  Break;
            end
          else if CharIn(C, cHex) then
              Inc(NC)
          else
            begin
              Result := False;
              Exit;
            end;

          Inc(cPos);
        end;

      Result := NC > 0;
      NumberBegin := bkPos;
      Exit;
    end;

  C := ParsingData.Text[cPos];
  if CharIn(C, c0to9) then
    begin
      bkPos := cPos;
      NC := 0;
      while True do
        begin
          cPos := GetTextDeclEndPos(GetCommentEndPos(cPos));

          if cPos > L then
              Break;
          C := ParsingData.Text[cPos];

          if isWordSplitChar(C, True, SymbolTable) then
            begin
              if NC > 0 then
                  Break;
            end
          else if CharIn(C, cAtoZ) then
            begin
              Result := False;
              Exit;
            end
          else if CharIn(C, c0to9) then
              Inc(NC);

          Inc(cPos);
        end;

      Result := NC > 0;
      NumberBegin := bkPos;
      Exit;
    end
  else if CharIn(C, '+-.') then
    begin
      bkPos := cPos;
      NC := 0;
      while True do
        begin
          cPos := GetTextDeclEndPos(GetCommentEndPos(cPos));

          if cPos > L then
              Break;
          C := ParsingData.Text[cPos];

          if CharIn(C, '+-') then
            begin
              if NC > 0 then
                  Break;
            end
          else if isWordSplitChar(C, True, SymbolTable) then
            begin
              if NC > 0 then
                  Break
            end
          else if CharIn(C, cAtoZ) then
            begin
              Result := False;
              Exit;
            end
          else if CharIn(C, c0to9) then
              Inc(NC);

          Inc(cPos);
        end;

      Result := NC > 0;
      NumberBegin := bkPos;
      Exit;
    end;
end;

function TTextParsing.GetNumberEndPos(const cOffset: Integer): Integer;
var
  IsHex: Boolean;
  L: Integer;
  cPos: Integer;
  C: SystemChar;
  NC: Integer;
  dotC: Integer;
  eC: Integer;
begin
  L := ParsingData.Len;
  cPos := cOffset;
  if cPos < 1 then
      cPos := 1;
  if cPos > L then
      cPos := L;

  if isNumber(cPos, Result, IsHex) then
    begin
      NC := 0;
      dotC := 0;
      eC := 0;
      while True do
        begin
          Result := GetTextDeclEndPos(GetCommentEndPos(Result));
          C := ParsingData.Text[Result];

          if (not CharIn(C, [c0to9])) then
            begin
              if CharIn(C, '+-') then
                begin
                  if NC > 0 then
                    begin
                      if eC = 1 then
                          Inc(eC)
                      else
                          Exit;
                    end;
                end
              else if (not IsHex) and CharIn(C, '.') then
                begin
                  if (dotC > 1) then
                      Exit;
                  Inc(dotC);
                end
              else if (not IsHex) and CharIn(C, 'eE') then
                begin
                  if (eC > 1) then
                      Exit;
                  Inc(eC);
                end
              else if (IsHex and (CharIn(C, [cLoAtoF, cHiAtoF]))) then
                  Inc(NC)
              else
                  Exit;
            end
          else
              Inc(NC);

          Inc(Result);
          if Result > L then
              Exit;
        end;
    end
  else
      Result := cPos;
end;

function TTextParsing.isTextDecl(const cOffset: Integer): Boolean;
var
  bPos, ePos: Integer;
begin
  Result := GetTextDeclPos(cOffset, bPos, ePos);
end;

function TTextParsing.GetTextDeclEndPos(const cOffset: Integer): Integer;
var
  bPos, ePos: Integer;
begin
  if GetTextDeclPos(cOffset, bPos, ePos) then
      Result := ePos
  else
      Result := cOffset;
end;

function TTextParsing.GetTextDeclBeginPos(const cOffset: Integer): Integer;
var
  bPos, ePos: Integer;
begin
  if GetTextDeclPos(cOffset, bPos, ePos) then
      Result := bPos
  else
      Result := cOffset;
end;

function TTextParsing.GetTextBody(const AText: TPascalString): TPascalString;
begin
  if TextStyle = tsPascal then
      Result := TranslatePascalDeclToText(AText)
  else if TextStyle = tsC then
      Result := TranslateC_DeclToText(AText)
  else
      Result := AText;
end;

function TTextParsing.GetTextDeclPos(const cOffset: Integer; var charBeginPos, charEndPos: Integer): Boolean;
  function CompLst(idx: Integer): Integer;
  begin
    with PTextPos(ParsingData.Cache.TextDecls[idx])^ do
      begin
        if (cOffset >= bPos) and (cOffset < ePos) then
            Result := 0
        else if (cOffset >= ePos) then
            Result := -1
        else if (cOffset < bPos) then
            Result := 1
        else
            Result := -2;
      end;
  end;

var
  cPos, L, R, M: Integer;
begin
  cPos := cOffset;
  if cPos < 1 then
      cPos := 1;
  if cPos > ParsingData.Len then
      cPos := ParsingData.Len;

  if ParsingData.Cache.TextDecls = nil then
      RebuildParsingCache;

  Result := False;

  L := 0;
  R := ParsingData.Cache.TextDecls.Count - 1;
  while L <= R do
    begin
      M := (L + R) div 2;
      case CompLst(M) of
        0:
          begin
            with PTextPos(ParsingData.Cache.TextDecls[M])^ do
              begin
                charBeginPos := bPos;
                charEndPos := ePos;
              end;
            Result := True;
            Exit;
          end;
        -1: L := M + 1;
        1: R := M - 1;
        else raise Exception.Create('struct error');
      end;
    end;
end;

function TTextParsing.isSymbol(const cOffset: Integer): Boolean;
begin
  Result := CharIn(ParsingData.Text[cOffset], SymbolTable)
end;

function TTextParsing.GetSymbolEndPos(const cOffset: Integer): Integer;
begin
  if isSymbol(cOffset) then
      Result := cOffset + 1
  else
      Result := cOffset;
end;

function TTextParsing.isAscii(const cOffset: Integer): Boolean;
begin
  Result := False;

  if isComment(cOffset) then
      Exit;

  if isTextDecl(cOffset) then
      Exit;

  if isSpecialSymbol(cOffset) then
      Exit;

  Result := (not isSymbol(cOffset)) and (not isWordSplitChar(ParsingData.Text[cOffset], True, SymbolTable)) and (not isNumber(cOffset));
end;

function TTextParsing.GetAsciiBeginPos(const cOffset: Integer): Integer;
begin
  Result := GetWordBeginPos(cOffset, True, SymbolTable);
end;

function TTextParsing.GetAsciiEndPos(const cOffset: Integer): Integer;
begin
  Result := GetWordEndPos(cOffset, True, SymbolTable, True, SymbolTable);
end;

function TTextParsing.isComment(const cOffset: Integer): Boolean;
var
  bPos, ePos: Integer;
begin
  Result := GetCommentPos(cOffset, bPos, ePos);
end;

function TTextParsing.GetCommentEndPos(const cOffset: Integer): Integer;
var
  bPos, ePos: Integer;
begin
  if GetCommentPos(cOffset, bPos, ePos) then
      Result := ePos
  else
      Result := cOffset;
end;

function TTextParsing.GetCommentBeginPos(const cOffset: Integer): Integer;
var
  bPos, ePos: Integer;
begin
  if GetCommentPos(cOffset, bPos, ePos) then
      Result := bPos
  else
      Result := cOffset;
end;

function TTextParsing.GetCommentPos(const cOffset: Integer; var charBeginPos, charEndPos: Integer): Boolean;
  function CompLst(idx: Integer): Integer;
  begin
    with PTextPos(ParsingData.Cache.CommentDecls[idx])^ do
      begin
        if (cOffset >= bPos) and (cOffset < ePos) then
            Result := 0
        else if (cOffset >= ePos) then
            Result := -1
        else if (cOffset < bPos) then
            Result := 1
        else
            Result := -2;
      end;
  end;

var
  cPos, L, R, M: Integer;
begin
  cPos := cOffset;
  if cPos < 1 then
      cPos := 1;
  if cPos > ParsingData.Len then
      cPos := ParsingData.Len;

  if ParsingData.Cache.CommentDecls = nil then
      RebuildParsingCache;

  Result := False;

  L := 0;
  R := ParsingData.Cache.CommentDecls.Count - 1;
  while L <= R do
    begin
      M := (L + R) div 2;
      case CompLst(M) of
        0:
          begin
            with PTextPos(ParsingData.Cache.CommentDecls[M])^ do
              begin
                charBeginPos := bPos;
                charEndPos := ePos;
              end;
            Result := True;
            Exit;
          end;
        -1: L := M + 1;
        1: R := M - 1;
        else raise Exception.Create('struct error');
      end;
    end;
end;

function TTextParsing.GetDeletedCommentText: TPascalString;
var
  oriPos, cPos, nPos: Integer;
begin
  Result := '';

  cPos := 1;
  oriPos := cPos;

  while cPos < ParsingData.Len do
    begin
      nPos := CompareCommentGetEndPos(cPos);
      if nPos > cPos then
        begin
          Result := Result + GetStr(oriPos, cPos);
          cPos := nPos;
          oriPos := cPos;
        end
      else
        begin
          Inc(cPos);
        end;
    end;
  if oriPos <= ParsingData.Len then
      Result := Result + GetStr(oriPos, ParsingData.Len + 1);
end;

function TTextParsing.isTextOrComment(const cOffset: Integer): Boolean;
begin
  Result := isTextDecl(cOffset) or isComment(cOffset);
end;

function TTextParsing.isCommentOrText(const cOffset: Integer): Boolean;
begin
  Result := isComment(cOffset) or isTextDecl(cOffset);
end;

function TTextParsing.isWordSplitChar(const C: SystemChar; SplitTokenC: TPascalString): Boolean;
begin
  Result := isWordSplitChar(C, True, SplitTokenC);
end;

function TTextParsing.isWordSplitChar(const C: SystemChar): Boolean;
begin
  Result := isWordSplitChar(C, True, '');
end;

function TTextParsing.isWordSplitChar(const C: SystemChar; DefaultChar: Boolean; SplitTokenC: TPascalString): Boolean;
begin
  if DefaultChar then
      Result := CharIn(C, [c0to32], SplitTokenC)
  else
      Result := CharIn(C, SplitTokenC);
end;

function TTextParsing.GetWordBeginPos(const cOffset: Integer; SplitTokenC: TPascalString): Integer;
begin
  Result := GetWordBeginPos(cOffset, True, SplitTokenC);
end;

function TTextParsing.GetWordBeginPos(const cOffset: Integer): Integer;
begin
  Result := GetWordBeginPos(cOffset, True, '');
end;

function TTextParsing.GetWordBeginPos(const cOffset: Integer; BeginDefaultChar: Boolean; SplitTokenC: TPascalString): Integer;
var
  L: Integer;
  cPos: Integer;
  tbPos: Integer;
begin
  L := ParsingData.Len;
  cPos := cOffset;
  if cPos < 1 then
      Exit(1);
  if cPos > L then
      Exit(L);

  repeat
    cPos := GetCommentEndPos(cPos);

    tbPos := GetTextDeclBeginPos(cPos);
    if tbPos <> cPos then
        Exit(tbPos);

    while (isWordSplitChar(ParsingData.Text[cPos], BeginDefaultChar, SplitTokenC)) do
      begin
        if cPos >= L then
            Break;
        Inc(cPos);
      end;
  until not isComment(cPos);

  Result := cPos;
  while (not isWordSplitChar(ParsingData.Text[Result], BeginDefaultChar, SplitTokenC)) do
    begin
      if Result - 1 <= 0 then
          Break;
      Dec(Result);
    end;

  if isWordSplitChar(ParsingData.Text[Result], SplitTokenC) then
      Inc(Result);
end;

function TTextParsing.GetWordEndPos(const cOffset: Integer; SplitTokenC: TPascalString): Integer;
begin
  Result := GetWordEndPos(cOffset, True, SplitTokenC, True, SplitTokenC);
end;

function TTextParsing.GetWordEndPos(const cOffset: Integer): Integer;
begin
  Result := GetWordEndPos(cOffset, True, '', True, '');
end;

function TTextParsing.GetWordEndPos(const cOffset: Integer; BeginSplitCharSet, EndSplitCharSet: TPascalString): Integer;
begin
  Result := GetWordEndPos(cOffset, True, BeginSplitCharSet, True, EndSplitCharSet);
end;

function TTextParsing.GetWordEndPos(const cOffset: Integer;
  BeginDefaultChar: Boolean; BeginSplitCharSet: TPascalString;
  EndDefaultChar: Boolean; EndSplitCharSet: TPascalString): Integer;
var
  L: Integer;
begin
  L := ParsingData.Len;
  if cOffset < 1 then
      Exit(1);
  if cOffset > L then
      Exit(L);

  Result := GetWordBeginPos(cOffset, BeginDefaultChar, BeginSplitCharSet);

  while (not isWordSplitChar(ParsingData.Text[Result], EndDefaultChar, EndSplitCharSet)) do
    begin
      Inc(Result);
      if Result > L then
          Break;
    end;
end;

function TTextParsing.CompareTokenText(const cOffset: Integer; T: TPascalString): Boolean;
var
  p: PTokenData;
begin
  Result := False;
  p := GetToken(cOffset);
  if p = nil then
      Exit;
  Result := p^.Text.Same(T);
end;

function TTextParsing.CompareTokenChar(const cOffset: Integer; const C: array of SystemChar): Boolean;
var
  p: PTokenData;
begin
  Result := False;
  p := GetToken(cOffset);
  if p = nil then
      Exit;
  if p^.Text.Len <> 1 then
      Exit;
  Result := CharIn(p^.Text.First, C);
end;

function TTextParsing.GetToken(const cOffset: Integer): PTokenData;
  function CompLst(idx: Integer): Integer;
  begin
    with PTokenData(ParsingData.Cache.TokenDataList[idx])^ do
      begin
        if (cOffset >= bPos) and (cOffset < ePos) then
            Result := 0
        else if (cOffset >= ePos) then
            Result := -1
        else if (cOffset < bPos) then
            Result := 1
        else
            Result := -2;
      end;
  end;

var
  L, R, M: Integer;
begin
  Result := nil;

  if (cOffset > Len) or (cOffset < 0) then
      Exit;

  if ParsingData.Cache.TokenDataList = nil then
      RebuildParsingCache;

  L := 0;
  R := ParsingData.Cache.TokenDataList.Count - 1;
  while L <= R do
    begin
      M := (L + R) div 2;
      case CompLst(M) of
        0: Exit(PTokenData(ParsingData.Cache.TokenDataList[M]));
        -1: L := M + 1;
        1: R := M - 1;
        else raise Exception.Create('struct error');
      end;
    end;
end;

function TTextParsing.GetTokenIndex(T: TTokenType; idx: Integer): PTokenData;
var
  i, C: Integer;
  p: PTokenData;
begin
  Result := nil;
  C := 0;
  for i := 0 to ParsingData.Cache.TokenDataList.Count - 1 do
    begin
      p := PTokenData(ParsingData.Cache.TokenDataList[i]);
      if p^.tokenType = T then
        begin
          if C = idx then
              Exit(p)
          else
              Inc(C);
        end;
    end;
end;

function TTextParsing.TokenCount: Integer;
begin
  Result := ParsingData.Cache.TokenDataList.Count;
end;

function TTextParsing.GetTokens(idx: Integer): PTokenData;
begin
  Result := PTokenData(ParsingData.Cache.TokenDataList[idx]);
end;

function TTextParsing.SniffingNextChar(const cOffset: Integer; declChar: TPascalString): Boolean;
var
  tmp: Integer;
begin
  Result := SniffingNextChar(cOffset, declChar, tmp);
end;

function TTextParsing.SniffingNextChar(const cOffset: Integer; declChar: TPascalString; out OutPos: Integer): Boolean;
var
  L: Integer;
  cPos: Integer;
begin
  L := ParsingData.Len;
  cPos := cOffset;
  if cPos < 1 then
      cPos := 1;
  if cPos > L then
      Exit(False);

  while isWordSplitChar(ParsingData.Text[cPos]) or (isTextOrComment(cPos)) do
    begin
      Inc(cPos);
      if cPos > L then
          Exit(False);
    end;

  if (cPos < L) then
      Result := CharIn(ParsingData.Text[cPos], declChar)
  else
      Result := False;

  if Result then
      OutPos := cPos;
end;

function TTextParsing.SplitChar(const cOffset: Integer; var LastPos: Integer; const SplitTokenC, SplitEndTokenC: TPascalString; var SplitOutput: TArrayPascalString): Integer;
  procedure AddS(s: TPascalString);
  var
    L: Integer;
  begin
    s := s.TrimChar(#32#0);
    if s.Len = 0 then
        Exit;
    L := length(SplitOutput);
    SetLength(SplitOutput, L + 1);
    SplitOutput[L] := s;
    Inc(Result);
  end;

type
  TLastSym = (lsBody, lsNone);

var
  L: Integer;
  C: SystemChar;
  cPos, bPos, ePos: Integer;
  LastSym: TLastSym;
begin
  Result := 0;
  SetLength(SplitOutput, 0);
  LastPos := cOffset;
  L := ParsingData.Len;
  cPos := cOffset;
  if cPos < 1 then
      cPos := 1;
  if cPos > L then
      Exit;

  bPos := cPos;
  ePos := bPos;
  LastSym := lsNone;
  while (cPos <= L) do
    begin
      if isComment(cPos) then
        begin
          cPos := GetCommentEndPos(cPos);
          Continue;
        end;
      if isTextDecl(cPos) then
        begin
          cPos := GetTextDeclEndPos(cPos);
          Continue;
        end;
      C := ParsingData.Text[cPos];
      if isWordSplitChar(C, True, SplitTokenC) then
        begin
          if LastSym = lsBody then
            begin
              ePos := cPos;
              AddS(GetStr(bPos, ePos));
              LastSym := lsNone;
            end;
          Inc(cPos);
          Continue;
        end;
      if (isWordSplitChar(C, False, SplitEndTokenC)) then
        begin
          if LastSym = lsBody then
            begin
              ePos := cPos;
              AddS(GetStr(bPos, ePos));
              LastSym := lsNone;
            end;
          LastPos := cPos;
          Exit;
        end;

      if LastSym = lsNone then
        begin
          bPos := cPos;
          LastSym := lsBody;
        end;
      Inc(cPos);
    end;

  if LastSym = lsBody then
    begin
      ePos := cPos;
      AddS(GetStr(bPos, ePos));
      LastSym := lsNone;
    end;
  LastPos := cPos;
end;

function TTextParsing.SplitChar(const cOffset: Integer; const SplitTokenC, SplitEndTokenC: TPascalString; var SplitOutput: TArrayPascalString): Integer;
var
  T: Integer;
begin
  Result := SplitChar(cOffset, T, SplitTokenC, SplitEndTokenC, SplitOutput);
end;

function TTextParsing.SplitString(const cOffset: Integer; var LastPos: Integer; const SplitTokenS, SplitEndTokenS: TPascalString; var SplitOutput: TArrayPascalString): Integer;
  procedure AddS(s: TPascalString);
  var
    L: Integer;
  begin
    s := s.TrimChar(#32#0);
    if s.Len = 0 then
        Exit;
    L := length(SplitOutput);
    SetLength(SplitOutput, L + 1);
    SplitOutput[L] := s;
    Inc(Result);
  end;

type
  TLastSym = (lsBody, lsNone);

var
  L: Integer;
  C: SystemChar;
  cPos, bPos, ePos: Integer;
  LastSym: TLastSym;
begin
  Result := 0;
  SetLength(SplitOutput, 0);
  LastPos := cOffset;
  L := ParsingData.Len;
  cPos := cOffset;
  if cPos < 1 then
      cPos := 1;
  if cPos > L then
      Exit;

  bPos := cPos;
  ePos := bPos;
  LastSym := lsNone;
  while (cPos <= L) do
    begin
      if isComment(cPos) then
        begin
          cPos := GetCommentEndPos(cPos);
          Continue;
        end;
      if isTextDecl(cPos) then
        begin
          cPos := GetTextDeclEndPos(cPos);
          Continue;
        end;
      if ComparePosStr(cPos, SplitTokenS) then
        begin
          if LastSym = lsBody then
            begin
              ePos := cPos;
              AddS(GetStr(bPos, ePos));
              LastSym := lsNone;
            end;
          Inc(cPos, SplitTokenS.Len);
          Continue;
        end;
      if ComparePosStr(cPos, SplitEndTokenS) then
        begin
          if LastSym = lsBody then
            begin
              ePos := cPos;
              AddS(GetStr(bPos, ePos));
              LastSym := lsNone;
            end;
          LastPos := cPos;
          Exit;
        end;

      if LastSym = lsNone then
        begin
          bPos := cPos;
          LastSym := lsBody;
        end;
      Inc(cPos);
    end;

  if LastSym = lsBody then
    begin
      ePos := cPos;
      AddS(GetStr(bPos, ePos));
      LastSym := lsNone;
    end;
  LastPos := cPos;
end;

function TTextParsing.SplitString(const cOffset: Integer; const SplitTokenS, SplitEndTokenS: TPascalString; var SplitOutput: TArrayPascalString): Integer;
var
  T: Integer;
begin
  Result := SplitString(cOffset, T, SplitTokenS, SplitEndTokenS, SplitOutput);
end;

function TTextParsing.GetStr(const bPos, ePos: Integer): TPascalString;
begin
  if ePos = ParsingData.Len then
      Result := ParsingData.Text.GetString(bPos, ePos + 1)
  else
      Result := ParsingData.Text.GetString(bPos, ePos);
end;

function TTextParsing.GetStr(const tp: TTextPos): TPascalString;
begin
  Result := GetStr(tp.bPos, tp.ePos);
end;

function TTextParsing.GetWordStr(const cOffset: Integer): TPascalString;
begin
  Result := GetStr(GetAsciiBeginPos(cOffset), GetAsciiEndPos(cOffset));
end;

function TTextParsing.GetPoint(const cOffset: Integer): TPoint;
var
  i: Integer;
  cPos: Integer;
begin
  cPos := cOffset;
  Result := Point(1, 1);
  if cPos > ParsingData.Len then
      cPos := ParsingData.Len;
  for i := 1 to cPos - 1 do
    begin
      if ParsingData.Text[i] = #10 then
        begin
          Inc(Result.Y);
          Result.X := 0;
        end
      else if not CharIn(ParsingData.Text[i], [#13]) then
        begin
          Inc(Result.X);
        end;
    end;
end;

function TTextParsing.GetChar(const cOffset: Integer): SystemChar;
begin
  Result := ParsingData.Text[cOffset];
end;

procedure TTextParsing.DeletePos(const bPos, ePos: Integer);
begin
  ParsingData.Text := GetStr(1, bPos) + GetStr(ePos, Len);
  ParsingData.Len := ParsingData.Text.Len;
  RebuildParsingCache;
end;

procedure TTextParsing.DeletePos(const tp: TTextPos);
begin
  DeletePos(tp.bPos, tp.ePos);
end;

procedure TTextParsing.DeletedComment;
begin
  ParsingData.Text := GetDeletedCommentText;
  ParsingData.Len := ParsingData.Text.Len;
  RebuildParsingCache;
end;

procedure TTextParsing.InsertTextBlock(const bPos, ePos: Integer; AInsertText: TPascalString);
begin
  ParsingData.Text := GetStr(1, bPos) + AInsertText + GetStr(ePos, Len + 1);
  ParsingData.Len := ParsingData.Text.Len;
  RebuildParsingCache;
end;

procedure TTextParsing.InsertTextBlock(const tp: TTextPos; AInsertText: TPascalString);
begin
  InsertTextBlock(tp.bPos, tp.ePos, AInsertText);
end;

function TTextParsing.SearchWordBody(initPos: Integer; wordInfo: TPascalString; var OutPos: TTextPos): Boolean;
var
  cp: Integer;
  ePos: Integer;
begin
  Result := False;

  cp := initPos;

  while cp <= ParsingData.Len do
    begin
      if isTextDecl(cp) then
        begin
          ePos := GetTextDeclEndPos(cp);
          cp := ePos;
        end
      else if isComment(cp) then
        begin
          ePos := GetCommentEndPos(cp);
          cp := ePos;
        end
      else if isNumber(cp) then
        begin
          ePos := GetNumberEndPos(cp);
          if GetStr(cp, ePos).Same(wordInfo) then
            begin
              OutPos.bPos := cp;
              OutPos.ePos := ePos;
              Result := True;
              Break;
            end;
          cp := ePos;
        end
      else if isSymbol(cp) then
        begin
          ePos := GetSymbolEndPos(cp);
          cp := ePos;
        end
      else if isAscii(cp) then
        begin
          ePos := GetAsciiEndPos(cp);
          if GetStr(cp, ePos).Same(wordInfo) then
            begin
              OutPos.bPos := cp;
              OutPos.ePos := ePos;
              Result := True;
              Break;
            end;
          cp := ePos;
        end
      else
          Inc(cp);
    end;
end;

class function TTextParsing.TranslatePascalDeclToText(const Text: TPascalString): TPascalString;
var
  cPos: Integer;

  // ext decl begin flag
  VIsTextDecl: Boolean;
  nText: TPascalString;
begin
  cPos := 1;
  VIsTextDecl := False;
  Result := '';
  while cPos <= Text.Len do
    begin
      if Text.ComparePos(cPos, #39#39#39#39) then
        begin
          Result.Append(#39);
          Inc(cPos, 4);
        end
      else if Text[cPos] = #39 then
        begin
          VIsTextDecl := not VIsTextDecl;
          Inc(cPos);
        end
      else
        begin
          if VIsTextDecl then
            begin
              Result.Append(Text[cPos]);
              Inc(cPos);
            end
          else if Text[cPos] = '#' then
            begin
              nText := '';
              Inc(cPos);
              while cPos <= Text.Len do
                begin
                  if not CharIn(Text[cPos], ' #' + #39#13#10) then
                    begin
                      nText.Append(Text[cPos]);
                      Inc(cPos);
                    end
                  else
                      Break;
                end;
              Result.Append(SystemChar(StrToIntDef(nText.Text, 0)));
            end
          else
              Inc(cPos);
        end;
    end;
end;

class function TTextParsing.TranslateTextToPascalDecl(const Text: TPascalString): TPascalString;
var
  cPos: Integer;
  C: SystemChar;
  LastIsOrdChar: Boolean;
  ordCharInfo: TPascalString;
begin
  if Text.Len = 0 then
    begin
      Result := #39#39;
      Exit;
    end;

  ordCharInfo.Len := 32;
  for cPos := 0 to 31 do
      ordCharInfo.buff[cPos] := SystemChar(Ord(cPos));
  ordCharInfo[32] := #39;

  Result := '';
  LastIsOrdChar := False;
  for cPos := 1 to Text.Len do
    begin
      C := Text[cPos];
      if CharIn(C, ordCharInfo) then
        begin
          if Result.Len = 0 then
              Result := '#' + IntToStr(Ord(C))
          else if LastIsOrdChar then
              Result.Append('#' + IntToStr(Ord(C)))
          else
              Result.Append(#39 + '#' + IntToStr(Ord(C)));
          LastIsOrdChar := True;
        end
      else
        begin
          if Result.Len = 0 then
              Result := #39 + C
          else if LastIsOrdChar then
              Result.Append(#39 + C)
          else
              Result.Append(C);

          LastIsOrdChar := False;
        end;
    end;

  if not LastIsOrdChar then
      Result.Append(#39);
end;

class function TTextParsing.TranslateTextToPascalDeclWithUnicode(const Text: TPascalString): TPascalString;
var
  cPos: Integer;
  C: SystemChar;
  LastIsOrdChar: Boolean;
  ordCharInfo: TPascalString;
begin
  if Text.Len = 0 then
    begin
      Result := #39#39;
      Exit;
    end;

  ordCharInfo.Len := 32;
  for cPos := 0 to 31 do
      ordCharInfo[cPos] := SystemChar(Ord(cPos));
  ordCharInfo[32] := #39;

  Result := '';
  LastIsOrdChar := False;
  for cPos := 1 to Text.Len do
    begin
      C := Text[cPos];
      if CharIn(C, ordCharInfo) or (Ord(C) >= $80) then
        begin
          if Result.Len = 0 then
              Result := '#' + IntToStr(Ord(C))
          else if LastIsOrdChar then
              Result.Append('#' + IntToStr(Ord(C)))
          else
              Result.Append(#39 + '#' + IntToStr(Ord(C)));
          LastIsOrdChar := True;
        end
      else
        begin
          if Result.Len = 0 then
              Result := #39 + C
          else if LastIsOrdChar then
              Result.Append(#39 + C)
          else
              Result.Append(C);

          LastIsOrdChar := False;
        end;
    end;

  if not LastIsOrdChar then
      Result.Append(#39);
end;

class function TTextParsing.TranslateC_DeclToText(const Text: TPascalString): TPascalString;
var
  cPos: Integer;
  i: Integer;

  // ext decl begin flag
  VIsTextDecl: Boolean;
  nText: TPascalString;
  wasC: Boolean;
begin
  cPos := 1;
  VIsTextDecl := False;
  Result := '';
  while cPos <= Text.Len do
    begin
      if Text[cPos] = '"' then
        begin
          VIsTextDecl := not VIsTextDecl;
          Inc(cPos);
        end
      else
        begin
          wasC := False;
          for i := low(CTranslateTable) to high(CTranslateTable) do
            begin
              if Text.ComparePos(cPos, CTranslateTable[i].C) then
                begin
                  Inc(cPos, length(CTranslateTable[i].C));
                  Result.Append(CTranslateTable[i].s);
                  wasC := True;
                  Break;
                end;
            end;
          if (not wasC) then
            begin
              if (VIsTextDecl) then
                  Result.Append(Text[cPos]);
              Inc(cPos);
            end;
        end;
    end;
end;

class function TTextParsing.TranslateTextToC_Decl(const Text: TPascalString): TPascalString;
  function GetCStyle(const C: SystemChar): SystemString; inline;
  var
    i: Integer;
  begin
    Result := '';
    for i := low(CTranslateTable) to high(CTranslateTable) do
      if C = CTranslateTable[i].s then
        begin
          Result := CTranslateTable[i].C;
          Break;
        end;
  end;

var
  cPos: Integer;
  C: SystemChar;
  LastIsOrdChar: Boolean;
  n: SystemString;
begin
  if Text.Len = 0 then
    begin
      Result := '""';
      Exit;
    end;

  Result := '';
  LastIsOrdChar := False;
  for cPos := 1 to Text.Len do
    begin
      C := Text[cPos];

      if Result.Len = 0 then
          Result := '"' + C
      else
        begin
          n := GetCStyle(C);
          if n <> '' then
              Result.Append(n)
          else
              Result.Append(C);
        end;
    end;

  if not LastIsOrdChar then
      Result.Append('"');
end;

class function TTextParsing.TranslatePascalDeclCommentToText(const Text: TPascalString): TPascalString;
begin
  Result := umlTrimSpace(Text);
  if umlMultipleMatch(False, '{*}', Result) then
    begin
      Result.DeleteFirst;
      Result.DeleteLast;
      if umlMultipleMatch(False, '$*', umlTrimSpace(Result)) then
          Result := Text;
    end
  else if umlMultipleMatch(False, '(*?*)', Result, '?', '') then
    begin
      Result.DeleteFirst;
      Result.DeleteFirst;
      Result.DeleteLast;
      Result.DeleteLast;
    end
  else if umlMultipleMatch(False, '////*', Result) then
    begin
      Result.DeleteFirst;
      Result.DeleteFirst;
      Result.DeleteFirst;
      Result.DeleteFirst;
    end
  else if umlMultipleMatch(False, '///*', Result) then
    begin
      Result.DeleteFirst;
      Result.DeleteFirst;
      Result.DeleteFirst;
    end
  else if umlMultipleMatch(False, '//*', Result) then
    begin
      Result.DeleteFirst;
      Result.DeleteFirst;
    end;
end;

class function TTextParsing.TranslateTextToPascalDeclComment(const Text: TPascalString): TPascalString;
var
  n: TPascalString;
begin
  n := umlTrimSpace(Text);
  if umlMultipleMatch(False, '{*}', n) then
      Result := Text
  else if umlMultipleMatch(False, '(*?*)', n, '?', '') then
      Result := Text
  else if n.Exists(['{', '}']) then
      Result := Text
  else
      Result := '{ ' + (Text) + ' }';
end;

class function TTextParsing.TranslateC_DeclCommentToText(const Text: TPascalString): TPascalString;
begin
  Result := umlTrimSpace(Text);
  if umlMultipleMatch(False, '#*', Result) then
    begin
      Result := Text;
    end
  else if umlMultipleMatch(False, '/*?*/', Result, '?', '') then
    begin
      Result.DeleteFirst;
      Result.DeleteFirst;
      Result.DeleteLast;
      Result.DeleteLast;
    end
  else if umlMultipleMatch(False, '////*', Result) then
    begin
      Result.DeleteFirst;
      Result.DeleteFirst;
      Result.DeleteFirst;
      Result.DeleteFirst;
    end
  else if umlMultipleMatch(False, '///*', Result) then
    begin
      Result.DeleteFirst;
      Result.DeleteFirst;
      Result.DeleteFirst;
    end
  else if umlMultipleMatch(False, '//*', Result) then
    begin
      Result.DeleteFirst;
      Result.DeleteFirst;
    end;
end;

class function TTextParsing.TranslateTextToC_DeclComment(const Text: TPascalString): TPascalString;
var
  n: TPascalString;
begin
  n := umlTrimSpace(Text);
  if umlMultipleMatch(False, '#*', n) then
      Result := Text
  else
      Result := '/* ' + n + ' */';
end;

constructor TTextParsing.Create(const AText: TPascalString; AStyle: TTextStyle; ASpecialSymbol: TListPascalString);
begin
  inherited Create;
  ParsingData.Cache.CommentDecls := nil;
  ParsingData.Cache.TextDecls := nil;
  ParsingData.Cache.TokenDataList := nil;
  if AText.Len = 0 then
      ParsingData.Text := #13#10
  else
      ParsingData.Text := AText + #32;
  ParsingData.Len := ParsingData.Text.Len + 1;
  TextStyle := AStyle;
  SymbolTable := V_SpacerSymbol;
  TokenStatistics := NullTokenStatistics;
  SpecialSymbol := TListPascalString.Create;
  if ASpecialSymbol <> nil then
      SpecialSymbol.Assign(ASpecialSymbol);

  RebuildParsingCache;
end;

constructor TTextParsing.Create(const AText: TPascalString; AStyle: TTextStyle);
begin
  inherited Create;
  ParsingData.Cache.CommentDecls := nil;
  ParsingData.Cache.TextDecls := nil;
  ParsingData.Cache.TokenDataList := nil;
  if AText.Len = 0 then
      ParsingData.Text := #13#10
  else
      ParsingData.Text := AText + #32;
  ParsingData.Len := ParsingData.Text.Len + 1;
  TextStyle := AStyle;
  SymbolTable := V_SpacerSymbol;
  TokenStatistics := NullTokenStatistics;
  SpecialSymbol := TListPascalString.Create;

  RebuildParsingCache;
end;

constructor TTextParsing.Create(const AText: TPascalString; AStyle: TTextStyle; ASpecialSymbol: TListPascalString; ASpacerSymbol: SystemString);
begin
  inherited Create;
  ParsingData.Cache.CommentDecls := nil;
  ParsingData.Cache.TextDecls := nil;
  ParsingData.Cache.TokenDataList := nil;
  if AText.Len = 0 then
      ParsingData.Text := #13#10
  else
      ParsingData.Text := AText + #32;
  ParsingData.Len := ParsingData.Text.Len + 1;
  TextStyle := AStyle;
  SymbolTable := ASpacerSymbol;
  TokenStatistics := NullTokenStatistics;
  SpecialSymbol := TListPascalString.Create;
  if ASpecialSymbol <> nil then
      SpecialSymbol.Assign(ASpecialSymbol);

  RebuildParsingCache;
end;

destructor TTextParsing.Destroy;
var
  i: Integer;
begin
  if ParsingData.Cache.CommentDecls <> nil then
    begin
      for i := 0 to ParsingData.Cache.CommentDecls.Count - 1 do
          Dispose(PTextPos(ParsingData.Cache.CommentDecls[i]));
      DisposeObject(ParsingData.Cache.CommentDecls);
      ParsingData.Cache.CommentDecls := nil;
    end;

  if ParsingData.Cache.TextDecls <> nil then
    begin
      for i := 0 to ParsingData.Cache.TextDecls.Count - 1 do
          Dispose(PTextPos(ParsingData.Cache.TextDecls[i]));
      DisposeObject(ParsingData.Cache.TextDecls);
      ParsingData.Cache.TextDecls := nil;
    end;

  if ParsingData.Cache.TokenDataList <> nil then
    begin
      for i := 0 to ParsingData.Cache.TokenDataList.Count - 1 do
          Dispose(PTokenData(ParsingData.Cache.TokenDataList[i]));
      DisposeObject(ParsingData.Cache.TokenDataList);
      ParsingData.Cache.TokenDataList := nil;
    end;

  TokenStatistics := NullTokenStatistics;
  DisposeObject(SpecialSymbol);
  inherited Destroy;
end;

function TTextParsing.Parsing: Boolean;
begin
  Result := False;
end;

end.
