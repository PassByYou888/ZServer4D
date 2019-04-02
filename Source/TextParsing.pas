{ ****************************************************************************** }
{ * parsing library,writen by QQ 600585@qq.com                                 * }
{ * https://zpascal.net                                                        * }
{ * https://github.com/PassByYou888/zAI                                        * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/PascalString                               * }
{ * https://github.com/PassByYou888/zRasterization                             * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/zSound                                     * }
{ * https://github.com/PassByYou888/zChinese                                   * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ * https://github.com/PassByYou888/zGameWare                                  * }
{ * https://github.com/PassByYou888/zAnalysis                                  * }
{ * https://github.com/PassByYou888/FFMPEG-Header                              * }
{ * https://github.com/PassByYou888/zTranslate                                 * }
{ * https://github.com/PassByYou888/InfiniteIoT                                * }
{ * https://github.com/PassByYou888/FastMD5                                    * }
{ ****************************************************************************** }

unit TextParsing;

{$INCLUDE zDefine.inc}

interface

uses Types, CoreClasses, PascalStrings, UnicodeMixedLib, ListEngine;

type
  TTextStyle = (tsPascal, tsC, tsText);

  TTokenType = (ttTextDecl, ttComment, ttNumber, ttSymbol, ttAscii, ttSpecialSymbol, ttUnknow);
  TTokenTypes = set of TTokenType;

  TTokenStatistics = array [TTokenType] of Integer;

  TTextPos = record
    bPos, ePos: Integer;
    Text: TPascalString;
  end;

  PTextPos = ^TTextPos;

  TTokenData = record
    bPos, ePos: Integer;
    Text: TPascalString;
    tokenType: TTokenType;
    Index: Integer;
  end;

  PTokenData = ^TTokenData;

  TTextParsingCache = record
    CommentDecls, TextDecls: TCoreClassList; // PTextPos
    TokenDataList: TCoreClassList;           // PTokenData
    CharToken: array of PTokenData;
  end;

  TTextParsingData = record
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
    RebuildCacheBusy: Boolean;

    { compare char }
    function ComparePosStr(const cOffset: Integer; const t: TPascalString): Boolean; overload; inline;
    function ComparePosStr(const cOffset: Integer; const p: PPascalString): Boolean; overload; inline;
    { compare comment and text declaration: TokenCache }
    function CompareCommentGetEndPos(const cOffset: Integer): Integer;
    function CompareTextDeclGetEndPos(const cOffset: Integer): Integer;
    { rebuild support }
    procedure RebuildParsingCache;
    procedure RebuildText;
    procedure RebuildToken;
    { automated context on pick: TokenCache }
    function GetContextBeginPos(const cOffset: Integer): Integer;
    function GetContextEndPos(const cOffset: Integer): Integer;
    { special symbol support: TokenCache }
    function isSpecialSymbol(const cOffset: Integer): Boolean; overload;
    function isSpecialSymbol(const cOffset: Integer; var speicalSymbolEndPos: Integer): Boolean; overload;
    function GetSpecialSymbolEndPos(const cOffset: Integer): Integer;
    { number decl support: TokenCache }
    function isNumber(const cOffset: Integer): Boolean; overload;
    function isNumber(const cOffset: Integer; var NumberBegin: Integer; var IsHex: Boolean): Boolean; overload;
    function GetNumberEndPos(const cOffset: Integer): Integer;
    { text support: TokenCache }
    function isTextDecl(const cOffset: Integer): Boolean;
    function GetTextDeclEndPos(const cOffset: Integer): Integer;
    function GetTextDeclBeginPos(const cOffset: Integer): Integer;
    function GetTextBody(const AText: TPascalString): TPascalString;
    function GetTextDeclPos(const cOffset: Integer; var charBeginPos, charEndPos: Integer): Boolean;
    { symbol support: TokenCache }
    function isSymbol(const cOffset: Integer): Boolean;
    function GetSymbolEndPos(const cOffset: Integer): Integer;
    { ascii support: TokenCache }
    function isAscii(const cOffset: Integer): Boolean;
    function GetAsciiBeginPos(const cOffset: Integer): Integer;
    function GetAsciiEndPos(const cOffset: Integer): Integer;
    { comment support: TokenCache }
    function isComment(const cOffset: Integer): Boolean;
    function GetCommentEndPos(const cOffset: Integer): Integer;
    function GetCommentBeginPos(const cOffset: Integer): Integer;
    function GetCommentPos(const cOffset: Integer; var charBeginPos, charEndPos: Integer): Boolean;
    function GetDeletedCommentText: TPascalString;
    { text support: TokenCache }
    function isTextOrComment(const cOffset: Integer): Boolean;
    function isCommentOrText(const cOffset: Integer): Boolean;
    { lowlevel word support: TokenCache no used }
    function isWordSplitChar(const c: SystemChar; SplitTokenC: TPascalString): Boolean; overload;
    function isWordSplitChar(const c: SystemChar): Boolean; overload;
    function isWordSplitChar(const c: SystemChar; DefaultChar: Boolean; SplitTokenC: TPascalString): Boolean; overload;
    function GetWordBeginPos(const cOffset: Integer; SplitTokenC: TPascalString): Integer; overload;
    function GetWordBeginPos(const cOffset: Integer): Integer; overload;
    function GetWordBeginPos(const cOffset: Integer; BeginDefaultChar: Boolean; SplitTokenC: TPascalString): Integer; overload;
    function GetWordEndPos(const cOffset: Integer; SplitTokenC: TPascalString): Integer; overload;
    function GetWordEndPos(const cOffset: Integer): Integer; overload;
    function GetWordEndPos(const cOffset: Integer; BeginSplitCharSet, EndSplitCharSet: TPascalString): Integer; overload;
    function GetWordEndPos(const cOffset: Integer;
      BeginDefaultChar: Boolean; BeginSplitCharSet: TPascalString;
      EndDefaultChar: Boolean; EndSplitCharSet: TPascalString): Integer; overload;
    { sniffing }
    function SniffingNextChar(const cOffset: Integer; declChar: TPascalString): Boolean; overload;
    function SniffingNextChar(const cOffset: Integer; declChar: TPascalString; out OutPos: Integer): Boolean; overload;
    { split char }
    function SplitChar(const cOffset: Integer; var LastPos: Integer; const SplitTokenC, SplitEndTokenC: TPascalString; var SplitOutput: TArrayPascalString): Integer; overload;
    function SplitChar(const cOffset: Integer; const SplitTokenC, SplitEndTokenC: TPascalString; var SplitOutput: TArrayPascalString): Integer; overload;
    { split string }
    function SplitString(const cOffset: Integer; var LastPos: Integer; const SplitTokenS, SplitEndTokenS: TPascalString; var SplitOutput: TArrayPascalString): Integer; overload;
    function SplitString(const cOffset: Integer; const SplitTokenS, SplitEndTokenS: TPascalString; var SplitOutput: TArrayPascalString): Integer; overload;
    { token }
    function CompareTokenText(const cOffset: Integer; t: TPascalString): Boolean;
    function CompareTokenChar(const cOffset: Integer; const c: array of SystemChar): Boolean;
    function GetToken(const cOffset: Integer): PTokenData;
    property TokenPos[const cOffset: Integer]: PTokenData read GetToken;
    function GetTokenIndex(t: TTokenType; idx: Integer): PTokenData;
    property TokenIndex[t: TTokenType; idx: Integer]: PTokenData read GetTokenIndex;
    function TokenCount: Integer; overload;
    function TokenCount(t: TTokenTypes): Integer; overload;
    function GetTokens(idx: Integer): PTokenData;
    property Tokens[idx: Integer]: PTokenData read GetTokens;
    property Token[idx: Integer]: PTokenData read GetTokens;
    function FirstToken: PTokenData;
    function LastToken: PTokenData;
    function TokenCombine(const bTokenI, eTokenI: Integer; const acceptT: TTokenTypes): TPascalString; overload;
    function TokenCombine(const bTokenI, eTokenI: Integer): TPascalString; overload;
    { token probe }
    function TokenProbeL(startI: Integer; const acceptT: TTokenTypes): PTokenData; overload;
    function TokenProbeL(startI: Integer; const t: TPascalString): PTokenData; overload;
    function TokenProbeL(startI: Integer; const acceptT: TTokenTypes; const t: TPascalString): PTokenData; overload;
    function TokenProbeR(startI: Integer; const acceptT: TTokenTypes): PTokenData; overload;
    function TokenProbeR(startI: Integer; const t: TPascalString): PTokenData; overload;
    function TokenProbeR(startI: Integer; const acceptT: TTokenTypes; const t: TPascalString): PTokenData; overload;
    { Free to match all strings from Token[StartIndex] left to right, including any symbols. return token }
    function TokenStringProbe(startI: Integer; const acceptT: TTokenTypes; const t: TPascalString): PTokenData;
    { misc }
    function GetText(const bPos, ePos: Integer): TPascalString; overload;
    function GetStr(const bPos, ePos: Integer): TPascalString; overload;
    function GetStr(const tp: TTextPos): TPascalString; overload;
    function GetWord(const cOffset: Integer): TPascalString; overload;
    function GetPoint(const cOffset: Integer): TPoint;
    function GetChar(const cOffset: Integer): SystemChar;
    property Len: Integer read ParsingData.Len;
    property ParseText: TPascalString read ParsingData.Text;
    property Text: TPascalString read ParsingData.Text;
    procedure DeletePos(const bPos, ePos: Integer); overload;
    procedure DeletePos(const tp: TTextPos); overload;
    procedure DeletedComment;
    procedure InsertTextBlock(const bPos, ePos: Integer; AInsertText: TPascalString); overload;
    procedure InsertTextBlock(const tp: TTextPos; AInsertText: TPascalString); overload;
    function SearchWordBody(initPos: Integer; wordInfo: TPascalString; var OutPos: TTextPos): Boolean;
    { string declaration }
    class function TranslatePascalDeclToText(const Decl: TPascalString): TPascalString;
    class function TranslateTextToPascalDecl(const Decl: TPascalString): TPascalString;
    class function TranslateTextToPascalDeclWithUnicode(const Decl: TPascalString): TPascalString;
    class function TranslateC_DeclToText(const Decl: TPascalString): TPascalString;
    class function TranslateTextToC_Decl(const Decl: TPascalString): TPascalString;
    { comment declaration }
    class function TranslatePascalDeclCommentToText(const Decl: TPascalString): TPascalString;
    class function TranslateTextToPascalDeclComment(const Decl: TPascalString): TPascalString;
    class function TranslateC_DeclCommentToText(const Decl: TPascalString): TPascalString;
    class function TranslateTextToC_DeclComment(const Decl: TPascalString): TPascalString;
    { structor }
    constructor Create(const AText: TPascalString; AStyle: TTextStyle; ASpecialSymbol: TListPascalString); overload;
    constructor Create(const AText: TPascalString; AStyle: TTextStyle); overload;
    constructor Create(const AText: TPascalString; AStyle: TTextStyle; ASpecialSymbol: TListPascalString; ASpacerSymbol: SystemString); overload;
    destructor Destroy; override;
    { external }
    procedure Init; virtual;
    function Parsing: Boolean; virtual;
    { debug }
    procedure Print;
  end;

  TTextParsingClass = class of TTextParsing;

const
  C_SpacerSymbol = #44#46#43#45#42#47#40#41#59#58#61#35#64#94#38#37#33#34#91#93#60#62#63#123#125#39#36;

var
  V_SpacerSymbol: SystemString = C_SpacerSymbol;

implementation

uses DoStatusIO, TypInfo;

const
  NullTokenStatistics: TTokenStatistics = (0, 0, 0, 0, 0, 0, 0);

type
  TCTranslateStruct = record
    s: SystemChar;
    c: SystemString;
  end;

const
  CTranslateTable: array [0 .. 11] of TCTranslateStruct = (
    (s: #007; c: '\a'),
    (s: #008; c: '\b'),
    (s: #012; c: '\f'),
    (s: #010; c: '\n'),
    (s: #013; c: '\r'),
    (s: #009; c: '\t'),
    (s: #011; c: '\v'),
    (s: #092; c: '\\'),
    (s: #063; c: '\?'),
    (s: #039; c: '\'#39),
    (s: #034; c: '\"'),
    (s: #000; c: '\0'));

function TTextParsing.ComparePosStr(const cOffset: Integer; const t: TPascalString): Boolean;
begin
  Result := ParsingData.Text.ComparePos(cOffset, t);
end;

function TTextParsing.ComparePosStr(const cOffset: Integer; const p: PPascalString): Boolean;
begin
  Result := ParsingData.Text.ComparePos(cOffset, p);
end;

function TTextParsing.CompareCommentGetEndPos(const cOffset: Integer): Integer;
var
  L: Integer;
  cPos: Integer;
  p: PTokenData;
begin
  if not RebuildCacheBusy then
    begin
      p := TokenPos[cOffset];
      if p <> nil then
        begin
          if p^.tokenType = TTokenType.ttComment then
              Result := p^.ePos
          else
              Result := cOffset;
          exit;
        end;
    end;

  L := ParsingData.Len;
  cPos := cOffset;
  if cPos < 1 then
      cPos := 1;
  if cPos > L then
      cPos := L;

  Result := cPos;

  if ComparePosStr(Result, '//') then
    begin
      inc(Result, 2);
      while ParsingData.Text[Result] <> #10 do
        begin
          if Result + 1 > L then
              Break;
          inc(Result);
        end;
    end
  else if (TextStyle = tsC) and (ComparePosStr(Result, '#')) then
    begin
      inc(Result, 1);
      while ParsingData.Text[Result] <> #10 do
        begin
          if Result + 1 > L then
              Break;
          inc(Result);
        end;
    end
  else if (TextStyle = tsC) and (ComparePosStr(Result, '/*')) then
    begin
      inc(Result, 2);
      while not ComparePosStr(Result, '*/') do
        begin
          if Result + 1 > L then
              Break;
          inc(Result);
        end;
      inc(Result, 2);
    end
  else if (TextStyle = tsPascal) and (ComparePosStr(Result, '{')) then
    begin
      inc(Result, 1);
      while ParsingData.Text[Result] <> '}' do
        begin
          if Result + 1 > L then
              Break;
          inc(Result);
        end;
      inc(Result, 1);
    end
  else if (TextStyle = tsPascal) and (ComparePosStr(Result, '(*')) then
    begin
      inc(Result, 2);
      while not ComparePosStr(Result, '*)') do
        begin
          if Result + 1 > L then
              Break;
          inc(Result);
        end;
      inc(Result, 2);
    end;
end;

function TTextParsing.CompareTextDeclGetEndPos(const cOffset: Integer): Integer;
var
  L: Integer;
  cPos: Integer;
  tmpPos: Integer;
  p: PTokenData;
begin
  if not RebuildCacheBusy then
    begin
      p := TokenPos[cOffset];
      if p <> nil then
        begin
          if p^.tokenType = TTokenType.ttTextDecl then
              Result := p^.ePos
          else
              Result := cOffset;
          exit;
        end;
    end;

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
          exit(cPos);
        end;
      inc(cPos, 1);
      while ParsingData.Text[cPos] <> #39 do
        begin
          if cPos + 1 > L then
              Break;
          if ParsingData.Text[cPos] = #10 then
              exit(cPos);
          inc(cPos);
        end;
      inc(cPos, 1);
    end;

  if (cPos + 1 < L) and (TextStyle = tsC) and (ParsingData.Text[cPos] = #39) then
    begin
      inc(cPos, 1);
      while ParsingData.Text[cPos] <> #39 do
        begin
          if ComparePosStr(cPos, '\' + #39) then
              inc(cPos, 1);
          if cPos + 1 > L then
              Break;
          if ParsingData.Text[cPos] = #10 then
              exit(cPos);
          inc(cPos);
        end;
      inc(cPos, 1);
    end;

  if (cPos + 1 < L) and (TextStyle = tsC) and (ParsingData.Text[cPos] = '"') then
    begin
      inc(cPos, 1);
      while ParsingData.Text[cPos] <> '"' do
        begin
          if ComparePosStr(cPos, '\"') then
              inc(cPos, 1);
          if cPos + 1 > L then
              Break;
          if ParsingData.Text[cPos] = #10 then
              exit(cPos);
          inc(cPos);
        end;
      inc(cPos, 1);
    end;

  if (cPos + 1 < L) and (TextStyle = tsPascal) and (ParsingData.Text[cPos] = '#') then
    begin
      repeat
        inc(cPos, 1);

        while isWordSplitChar(ParsingData.Text[cPos]) do
          begin
            if cPos + 1 > L then
                exit(cPos);
            inc(cPos);
          end;
        while CharIn(ParsingData.Text[cPos], [c0to9], '$') do
          begin
            if cPos + 1 > L then
                exit(cPos);
            inc(cPos);
          end;
        tmpPos := cPos;
        while isWordSplitChar(ParsingData.Text[cPos]) do
          begin
            if cPos + 1 > L then
                exit(cPos);
            inc(cPos);
          end;
      until not ComparePosStr(cPos, '#');
      cPos := CompareTextDeclGetEndPos(tmpPos);
    end;

  Result := cPos;
end;

procedure TTextParsing.RebuildParsingCache;
var
  i, j: Integer;
  L: Integer;
  bPos: Integer;
  ePos: Integer;
  textPosPtr: PTextPos;
  LastTokenData: PTokenData;
begin
  RebuildCacheBusy := True;
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
  SetLength(ParsingData.Cache.CharToken, 0);

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
              inc(bPos);
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
          LastTokenData^.Index := ParsingData.Cache.TokenDataList.Count;
          ParsingData.Cache.TokenDataList.Add(LastTokenData);
          inc(TokenStatistics[LastTokenData^.tokenType]);

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
          LastTokenData^.Index := ParsingData.Cache.TokenDataList.Count;
          ParsingData.Cache.TokenDataList.Add(LastTokenData);
          inc(TokenStatistics[LastTokenData^.tokenType]);

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
          LastTokenData^.Index := ParsingData.Cache.TokenDataList.Count;
          ParsingData.Cache.TokenDataList.Add(LastTokenData);
          inc(TokenStatistics[LastTokenData^.tokenType]);

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
          LastTokenData^.Index := ParsingData.Cache.TokenDataList.Count;
          ParsingData.Cache.TokenDataList.Add(LastTokenData);
          inc(TokenStatistics[LastTokenData^.tokenType]);

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
          LastTokenData^.Index := ParsingData.Cache.TokenDataList.Count;
          ParsingData.Cache.TokenDataList.Add(LastTokenData);
          inc(TokenStatistics[LastTokenData^.tokenType]);

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
          LastTokenData^.Index := ParsingData.Cache.TokenDataList.Count;
          ParsingData.Cache.TokenDataList.Add(LastTokenData);
          inc(TokenStatistics[LastTokenData^.tokenType]);

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
              LastTokenData^.Index := ParsingData.Cache.TokenDataList.Count;
              ParsingData.Cache.TokenDataList.Add(LastTokenData);
              inc(TokenStatistics[LastTokenData^.tokenType]);
            end
          else
            begin
              LastTokenData^.ePos := ePos;
              LastTokenData^.Text.Append(GetChar(bPos));
            end;

          bPos := ePos;
        end;
    end;

  SetLength(ParsingData.Cache.CharToken, L);
  for i := 0 to ParsingData.Cache.TokenDataList.Count - 1 do
    begin
      LastTokenData := PTokenData(ParsingData.Cache.TokenDataList[i]);
      for j := LastTokenData^.bPos to LastTokenData^.ePos - 1 do
          ParsingData.Cache.CharToken[j - 1] := LastTokenData;
    end;
  RebuildCacheBusy := False;
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
  p: PTokenData;
begin
  Result := cOffset;
  p := TokenPos[cOffset];
  if p = nil then
      exit;
  Result := p^.bPos;
end;

function TTextParsing.GetContextEndPos(const cOffset: Integer): Integer;
var
  p: PTokenData;
begin
  Result := cOffset;
  p := TokenPos[cOffset];
  if p = nil then
      exit;
  Result := p^.ePos;
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
  p: PTokenData;
begin
  if not RebuildCacheBusy then
    begin
      p := TokenPos[cOffset];
      if p <> nil then
        begin
          Result := p^.tokenType = TTokenType.ttSpecialSymbol;
          if Result then
              speicalSymbolEndPos := p^.ePos;
          exit;
        end;
    end;

  Result := False;
  speicalSymbolEndPos := cOffset;

  if SpecialSymbol.Count = 0 then
      exit;

  if isComment(cOffset) then
      exit;

  if isTextDecl(cOffset) then
      exit;

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
  c: SystemChar;
  L: Integer;
  cPos, bkPos: Integer;
  NC: Integer;
  dotCount: Integer;
  eCnt: Integer;
  AddSubSymCnt: Integer;
  p: PTokenData;
begin
  if not RebuildCacheBusy then
    begin
      p := TokenPos[cOffset];
      if p <> nil then
        begin
          Result := p^.tokenType = TTokenType.ttNumber;
          if Result then
            begin
              NumberBegin := p^.bPos;
              IsHex := p^.Text.ComparePos(1, '$') or p^.Text.ComparePos(1, '0x');
            end;
          exit;
        end;
    end;

  Result := False;

  cPos := cOffset;
  L := ParsingData.Len;
  if cPos < 1 then
      cPos := 1;
  if cPos > L then
      cPos := L;

  if cPos = L then
      exit;

  IsHex := False;
  try
    if (CharIn(ParsingData.Text[cPos], '$')) then
      begin
        // pascal style hex
        IsHex := True;
        inc(cPos);
        if cPos > L then
            exit;
      end
    else if ComparePosStr(cPos, '0x') then
      begin
        // c style hex
        IsHex := True;
        inc(cPos, 2);
        if cPos > L then
            exit;
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
          c := ParsingData.Text[cPos];

          if isWordSplitChar(c, True, SymbolTable) then
            begin
              if NC > 0 then
                  Break;
            end
          else if CharIn(c, cHex) then
              inc(NC)
          else
            begin
              Result := False;
              exit;
            end;

          inc(cPos);
        end;

      Result := NC > 0;
      NumberBegin := bkPos;
      exit;
    end;

  c := ParsingData.Text[cPos];
  if CharIn(c, c0to9) then
    begin
      bkPos := cPos;
      NC := 0;
      while True do
        begin
          cPos := GetTextDeclEndPos(GetCommentEndPos(cPos));

          if cPos > L then
              Break;
          c := ParsingData.Text[cPos];

          if isWordSplitChar(c, True, SymbolTable) then
            begin
              Break;
            end
          else if CharIn(c, cAtoZ) then
            begin
              Result := False;
              exit;
            end
          else if CharIn(c, c0to9) then
              inc(NC);

          inc(cPos);
        end;

      Result := NC > 0;
      NumberBegin := bkPos;
      exit;
    end
  else if CharIn(c, '+-.') then
    begin
      bkPos := cPos;
      NC := 0;
      while True do
        begin
          cPos := GetTextDeclEndPos(GetCommentEndPos(cPos));

          if cPos > L then
              Break;
          c := ParsingData.Text[cPos];

          if CharIn(c, '+-') then
            begin
              if NC > 0 then
                  Break;
            end
          else if isWordSplitChar(c, True, SymbolTable) then
            begin
              Break
            end
          else if CharIn(c, cAtoZ) then
            begin
              Result := False;
              exit;
            end
          else if CharIn(c, c0to9) then
              inc(NC);

          inc(cPos);
        end;

      Result := NC > 0;
      NumberBegin := bkPos;
      exit;
    end;
end;

function TTextParsing.GetNumberEndPos(const cOffset: Integer): Integer;
var
  IsHex: Boolean;
  L: Integer;
  cPos: Integer;
  c: SystemChar;
  NC: Integer;
  dotC: Integer;
  eC: Integer;
  p: PTokenData;
begin
  if not RebuildCacheBusy then
    begin
      p := TokenPos[cOffset];
      if p <> nil then
        begin
          if p^.tokenType = TTokenType.ttNumber then
              Result := p^.ePos
          else
              Result := cOffset;
          exit;
        end;
    end;

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
          if isComment(Result) or isTextDecl(Result) then
              Break;
          c := ParsingData.Text[Result];

          if (not CharIn(c, [c0to9])) then
            begin
              if CharIn(c, '+-') then
                begin
                  if NC > 0 then
                    begin
                      if eC = 1 then
                          inc(eC)
                      else
                          exit;
                    end;
                end
              else if (not IsHex) and CharIn(c, '.') then
                begin
                  if (dotC > 1) then
                      exit;
                  inc(dotC);
                end
              else if (not IsHex) and CharIn(c, 'eE') then
                begin
                  if (eC > 1) then
                      exit;
                  inc(eC);
                end
              else if (IsHex and (CharIn(c, [cLoAtoF, cHiAtoF]))) then
                  inc(NC)
              else
                  exit;
            end
          else
              inc(NC);

          inc(Result);
          if Result > L then
              exit;
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
  cPos, L, r, M: Integer;
  p: PTokenData;
begin
  if not RebuildCacheBusy then
    begin
      p := TokenPos[cOffset];
      if p <> nil then
        begin
          Result := p^.tokenType = TTokenType.ttTextDecl;
          if Result then
            begin
              charBeginPos := p^.bPos;
              charEndPos := p^.ePos;
            end;
          exit;
        end;
    end;

  cPos := cOffset;
  if cPos < 1 then
      cPos := 1;
  if cPos > ParsingData.Len then
      cPos := ParsingData.Len;

  if ParsingData.Cache.TextDecls = nil then
      RebuildParsingCache;

  Result := False;

  L := 0;
  r := ParsingData.Cache.TextDecls.Count - 1;
  while L <= r do
    begin
      M := (L + r) div 2;
      case CompLst(M) of
        0:
          begin
            with PTextPos(ParsingData.Cache.TextDecls[M])^ do
              begin
                charBeginPos := bPos;
                charEndPos := ePos;
              end;
            Result := True;
            exit;
          end;
        -1: L := M + 1;
        1: r := M - 1;
        else RaiseInfo('struct error');
      end;
    end;
end;

function TTextParsing.isSymbol(const cOffset: Integer): Boolean;
var
  p: PTokenData;
begin
  if not RebuildCacheBusy then
    begin
      p := TokenPos[cOffset];
      if p <> nil then
        begin
          Result := p^.tokenType = TTokenType.ttSymbol;
          exit;
        end;
    end;
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
var
  p: PTokenData;
begin
  if not RebuildCacheBusy then
    begin
      p := TokenPos[cOffset];
      if p <> nil then
        begin
          Result := p^.tokenType = TTokenType.ttAscii;
          exit;
        end;
    end;
  Result := False;

  if isComment(cOffset) then
      exit;

  if isTextDecl(cOffset) then
      exit;

  if isSpecialSymbol(cOffset) then
      exit;

  Result := (not isSymbol(cOffset)) and (not isWordSplitChar(ParsingData.Text[cOffset], True, SymbolTable)) and (not isNumber(cOffset));
end;

function TTextParsing.GetAsciiBeginPos(const cOffset: Integer): Integer;
var
  p: PTokenData;
begin
  if not RebuildCacheBusy then
    begin
      p := TokenPos[cOffset];
      if p <> nil then
        begin
          if p^.tokenType = TTokenType.ttAscii then
              Result := p^.bPos
          else
              Result := cOffset;
          exit;
        end;
    end;
  Result := GetWordBeginPos(cOffset, True, SymbolTable);
end;

function TTextParsing.GetAsciiEndPos(const cOffset: Integer): Integer;
var
  p: PTokenData;
begin
  if not RebuildCacheBusy then
    begin
      p := TokenPos[cOffset];
      if p <> nil then
        begin
          if p^.tokenType = TTokenType.ttAscii then
              Result := p^.ePos
          else
              Result := cOffset;
          exit;
        end;
    end;
  Result := GetWordEndPos(cOffset, True, SymbolTable, True, SymbolTable);
end;

function TTextParsing.isComment(const cOffset: Integer): Boolean;
var
  bPos, ePos: Integer;
  p: PTokenData;
begin
  if not RebuildCacheBusy then
    begin
      p := TokenPos[cOffset];
      if p <> nil then
        begin
          Result := p^.tokenType = TTokenType.ttComment;
          exit;
        end;
    end;
  Result := GetCommentPos(cOffset, bPos, ePos);
end;

function TTextParsing.GetCommentEndPos(const cOffset: Integer): Integer;
var
  bPos, ePos: Integer;
  p: PTokenData;
begin
  if not RebuildCacheBusy then
    begin
      p := TokenPos[cOffset];
      if p <> nil then
        begin
          if p^.tokenType = TTokenType.ttComment then
              Result := p^.ePos
          else
              Result := cOffset;
          exit;
        end;
    end;

  if GetCommentPos(cOffset, bPos, ePos) then
      Result := ePos
  else
      Result := cOffset;
end;

function TTextParsing.GetCommentBeginPos(const cOffset: Integer): Integer;
var
  bPos, ePos: Integer;
  p: PTokenData;
begin
  if not RebuildCacheBusy then
    begin
      p := TokenPos[cOffset];
      if p <> nil then
        begin
          if p^.tokenType = TTokenType.ttComment then
              Result := p^.bPos
          else
              Result := cOffset;
          exit;
        end;
    end;

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
  cPos, L, r, M: Integer;
  p: PTokenData;
begin
  if not RebuildCacheBusy then
    begin
      p := TokenPos[cOffset];
      if p <> nil then
        begin
          Result := p^.tokenType = TTokenType.ttComment;
          if Result then
            begin
              charBeginPos := p^.bPos;
              charEndPos := p^.ePos;
            end;
          exit;
        end;
    end;

  cPos := cOffset;
  if cPos < 1 then
      cPos := 1;
  if cPos > ParsingData.Len then
      cPos := ParsingData.Len;

  if ParsingData.Cache.CommentDecls = nil then
      RebuildParsingCache;

  Result := False;

  L := 0;
  r := ParsingData.Cache.CommentDecls.Count - 1;
  while L <= r do
    begin
      M := (L + r) div 2;
      case CompLst(M) of
        0:
          begin
            with PTextPos(ParsingData.Cache.CommentDecls[M])^ do
              begin
                charBeginPos := bPos;
                charEndPos := ePos;
              end;
            Result := True;
            exit;
          end;
        -1: L := M + 1;
        1: r := M - 1;
        else RaiseInfo('struct error');
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
          inc(cPos);
        end;
    end;
  if oriPos <= ParsingData.Len then
      Result := Result + GetStr(oriPos, ParsingData.Len + 1);

  Result := Result.TrimChar(#32);
end;

function TTextParsing.isTextOrComment(const cOffset: Integer): Boolean;
begin
  Result := isTextDecl(cOffset) or isComment(cOffset);
end;

function TTextParsing.isCommentOrText(const cOffset: Integer): Boolean;
begin
  Result := isComment(cOffset) or isTextDecl(cOffset);
end;

function TTextParsing.isWordSplitChar(const c: SystemChar; SplitTokenC: TPascalString): Boolean;
begin
  Result := isWordSplitChar(c, True, SplitTokenC);
end;

function TTextParsing.isWordSplitChar(const c: SystemChar): Boolean;
begin
  Result := isWordSplitChar(c, True, '');
end;

function TTextParsing.isWordSplitChar(const c: SystemChar; DefaultChar: Boolean; SplitTokenC: TPascalString): Boolean;
begin
  if DefaultChar then
      Result := CharIn(c, [c0to32], SplitTokenC)
  else
      Result := CharIn(c, SplitTokenC);
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
      exit(1);
  if cPos > L then
      exit(L);

  repeat
    cPos := GetCommentEndPos(cPos);

    tbPos := GetTextDeclBeginPos(cPos);
    if tbPos <> cPos then
        exit(tbPos);

    while (isWordSplitChar(ParsingData.Text[cPos], BeginDefaultChar, SplitTokenC)) do
      begin
        if cPos >= L then
            Break;
        inc(cPos);
      end;
  until not isComment(cPos);

  Result := cPos;
  while (not isWordSplitChar(ParsingData.Text[Result], BeginDefaultChar, SplitTokenC)) do
    begin
      if Result - 1 <= 0 then
          Break;
      dec(Result);
    end;

  if isWordSplitChar(ParsingData.Text[Result], SplitTokenC) then
      inc(Result);
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
      exit(1);
  if cOffset > L then
      exit(L);

  Result := GetWordBeginPos(cOffset, BeginDefaultChar, BeginSplitCharSet);

  while (not isWordSplitChar(ParsingData.Text[Result], EndDefaultChar, EndSplitCharSet)) do
    begin
      inc(Result);
      if Result > L then
          Break;
    end;
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
      exit(False);

  while isWordSplitChar(ParsingData.Text[cPos]) or (isTextOrComment(cPos)) do
    begin
      inc(cPos);
      if cPos > L then
          exit(False);
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
        exit;
    L := Length(SplitOutput);
    SetLength(SplitOutput, L + 1);
    SplitOutput[L] := s;
    inc(Result);
  end;

type
  TLastSym = (lsBody, lsNone);

var
  L: Integer;
  c: SystemChar;
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
      exit;

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
      c := ParsingData.Text[cPos];
      if isWordSplitChar(c, True, SplitTokenC) then
        begin
          if LastSym = lsBody then
            begin
              ePos := cPos;
              AddS(GetStr(bPos, ePos));
              LastSym := lsNone;
            end;
          inc(cPos);
          Continue;
        end;
      if (isWordSplitChar(c, False, SplitEndTokenC)) then
        begin
          if LastSym = lsBody then
            begin
              ePos := cPos;
              AddS(GetStr(bPos, ePos));
              LastSym := lsNone;
            end;
          LastPos := cPos;
          exit;
        end;

      if LastSym = lsNone then
        begin
          bPos := cPos;
          LastSym := lsBody;
        end;
      inc(cPos);
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
  t: Integer;
begin
  Result := SplitChar(cOffset, t, SplitTokenC, SplitEndTokenC, SplitOutput);
end;

function TTextParsing.SplitString(const cOffset: Integer; var LastPos: Integer; const SplitTokenS, SplitEndTokenS: TPascalString; var SplitOutput: TArrayPascalString): Integer;
  procedure AddS(s: TPascalString);
  var
    L: Integer;
  begin
    s := s.TrimChar(#32#0);
    if s.Len = 0 then
        exit;
    L := Length(SplitOutput);
    SetLength(SplitOutput, L + 1);
    SplitOutput[L] := s;
    inc(Result);
  end;

type
  TLastSym = (lsBody, lsNone);

var
  L: Integer;
  c: SystemChar;
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
      exit;

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
          inc(cPos, SplitTokenS.Len);
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
          exit;
        end;

      if LastSym = lsNone then
        begin
          bPos := cPos;
          LastSym := lsBody;
        end;
      inc(cPos);
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
  t: Integer;
begin
  Result := SplitString(cOffset, t, SplitTokenS, SplitEndTokenS, SplitOutput);
end;

function TTextParsing.CompareTokenText(const cOffset: Integer; t: TPascalString): Boolean;
var
  p: PTokenData;
begin
  Result := False;
  p := GetToken(cOffset);
  if p = nil then
      exit;
  Result := p^.Text.Same(t);
end;

function TTextParsing.CompareTokenChar(const cOffset: Integer; const c: array of SystemChar): Boolean;
var
  p: PTokenData;
begin
  Result := False;
  p := GetToken(cOffset);
  if p = nil then
      exit;
  if p^.Text.Len <> 1 then
      exit;
  Result := CharIn(p^.Text.First, c);
end;

function TTextParsing.GetToken(const cOffset: Integer): PTokenData;
begin
  if (cOffset - 1 >= 0) and (cOffset - 1 < Length(ParsingData.Cache.CharToken)) then
      Result := ParsingData.Cache.CharToken[cOffset - 1]
  else
      Result := nil;
end;

function TTextParsing.GetTokenIndex(t: TTokenType; idx: Integer): PTokenData;
var
  i, c: Integer;
  p: PTokenData;
begin
  Result := nil;
  c := 0;
  for i := 0 to ParsingData.Cache.TokenDataList.Count - 1 do
    begin
      p := PTokenData(ParsingData.Cache.TokenDataList[i]);
      if p^.tokenType = t then
        begin
          if c = idx then
              exit(p)
          else
              inc(c);
        end;
    end;
end;

function TTextParsing.TokenCount: Integer;
begin
  Result := ParsingData.Cache.TokenDataList.Count;
end;

function TTextParsing.TokenCount(t: TTokenTypes): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := ParsingData.Cache.TokenDataList.Count - 1 downto 0 do
    if GetTokens(i)^.tokenType in t then
        inc(Result);
end;

function TTextParsing.GetTokens(idx: Integer): PTokenData;
begin
  Result := PTokenData(ParsingData.Cache.TokenDataList[idx]);
end;

function TTextParsing.FirstToken: PTokenData;
begin
  Result := GetTokens(0);
end;

function TTextParsing.LastToken: PTokenData;
begin
  Result := GetTokens(TokenCount - 1);
end;

function TTextParsing.TokenCombine(const bTokenI, eTokenI: Integer; const acceptT: TTokenTypes): TPascalString;
var
  bi, ei: Integer;
  p: PTokenData;
begin
  if bTokenI > eTokenI then
    begin
      bi := eTokenI;
      ei := bTokenI;
    end
  else
    begin
      bi := bTokenI;
      ei := eTokenI;
    end;

  Result := '';
  while bi <= ei do
    begin
      p := Tokens[bi];
      if p^.tokenType in acceptT then
          Result.Append(p^.Text);
      inc(bi);
    end;
end;

function TTextParsing.TokenCombine(const bTokenI, eTokenI: Integer): TPascalString;
begin
  Result := TokenCombine(bTokenI, eTokenI, [ttTextDecl, ttComment, ttNumber, ttSymbol, ttAscii, ttSpecialSymbol, ttUnknow]);
end;

function TTextParsing.TokenProbeL(startI: Integer; const acceptT: TTokenTypes): PTokenData;
var
  idx: Integer;
  p: PTokenData;
begin
  Result := nil;
  if ParsingData.Cache.TokenDataList.Count <= 0 then
      exit;
  idx := startI;
  while idx >= 0 do
    begin
      p := PTokenData(ParsingData.Cache.TokenDataList[idx]);
      if (p^.tokenType in acceptT) then
        begin
          Result := p;
          exit;
        end
      else
          dec(idx);
    end;
end;

function TTextParsing.TokenProbeL(startI: Integer; const t: TPascalString): PTokenData;
var
  idx: Integer;
  p: PTokenData;
begin
  Result := nil;
  if ParsingData.Cache.TokenDataList.Count <= 0 then
      exit;
  idx := startI;
  while idx >= 0 do
    begin
      p := PTokenData(ParsingData.Cache.TokenDataList[idx]);
      if (p^.Text.Same(t)) then
        begin
          Result := p;
          exit;
        end
      else
          dec(idx);
    end;
end;

function TTextParsing.TokenProbeL(startI: Integer; const acceptT: TTokenTypes; const t: TPascalString): PTokenData;
var
  idx: Integer;
  p: PTokenData;
begin
  Result := nil;
  if ParsingData.Cache.TokenDataList.Count <= 0 then
      exit;
  idx := startI;
  while idx >= 0 do
    begin
      p := PTokenData(ParsingData.Cache.TokenDataList[idx]);
      if (p^.tokenType in acceptT) and (p^.Text.Same(t)) then
        begin
          Result := p;
          exit;
        end
      else
          dec(idx);
    end;
end;

function TTextParsing.TokenProbeR(startI: Integer; const acceptT: TTokenTypes): PTokenData;
var
  idx: Integer;
  p: PTokenData;
begin
  Result := nil;
  if ParsingData.Cache.TokenDataList.Count <= 0 then
      exit;
  idx := startI;
  while idx < ParsingData.Cache.TokenDataList.Count do
    begin
      p := PTokenData(ParsingData.Cache.TokenDataList[idx]);
      if (p^.tokenType in acceptT) then
        begin
          Result := p;
          exit;
        end
      else
          inc(idx);
    end;
end;

function TTextParsing.TokenProbeR(startI: Integer; const t: TPascalString): PTokenData;
var
  idx: Integer;
  p: PTokenData;
begin
  Result := nil;
  if ParsingData.Cache.TokenDataList.Count <= 0 then
      exit;
  idx := startI;
  while idx < ParsingData.Cache.TokenDataList.Count do
    begin
      p := PTokenData(ParsingData.Cache.TokenDataList[idx]);
      if (p^.Text.Same(t)) then
        begin
          Result := p;
          exit;
        end
      else
          inc(idx);
    end;
end;

function TTextParsing.TokenProbeR(startI: Integer; const acceptT: TTokenTypes; const t: TPascalString): PTokenData;
var
  idx: Integer;
  p: PTokenData;
begin
  Result := nil;
  if ParsingData.Cache.TokenDataList.Count <= 0 then
      exit;
  idx := startI;
  while idx < ParsingData.Cache.TokenDataList.Count do
    begin
      p := PTokenData(ParsingData.Cache.TokenDataList[idx]);
      if (p^.tokenType in acceptT) and (p^.Text.Same(t)) then
        begin
          Result := p;
          exit;
        end
      else
          inc(idx);
    end;
end;

function TTextParsing.TokenStringProbe(startI: Integer; const acceptT: TTokenTypes; const t: TPascalString): PTokenData;
var
  idx: Integer;
  p: PTokenData;
begin
  Result := nil;
  if ParsingData.Cache.TokenDataList.Count <= 0 then
      exit;
  idx := startI;
  while idx < ParsingData.Cache.TokenDataList.Count do
    begin
      p := PTokenData(ParsingData.Cache.TokenDataList[idx]);
      if (p^.tokenType in acceptT) and (ComparePosStr(p^.bPos, t)) then
        begin
          Result := p;
          exit;
        end
      else
          inc(idx);
    end;
end;

function TTextParsing.GetText(const bPos, ePos: Integer): TPascalString;
begin
  Result := GetStr(bPos, ePos);
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

function TTextParsing.GetWord(const cOffset: Integer): TPascalString;
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
          inc(Result.y);
          Result.x := 0;
        end
      else if not CharIn(ParsingData.Text[i], [#13]) then
          inc(Result.x);
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
  ParsingData.Text := GetDeletedCommentText.TrimChar(#32);
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
          inc(cp);
    end;
end;

class function TTextParsing.TranslatePascalDeclToText(const Decl: TPascalString): TPascalString;
var
  cPos: Integer;

  // ext decl begin flag
  VIsTextDecl: Boolean;
  nText: TPascalString;
begin
  cPos := 1;
  VIsTextDecl := False;
  Result := '';
  while cPos <= Decl.Len do
    begin
      if Decl.ComparePos(cPos, #39#39#39#39) then
        begin
          Result.Append(#39);
          inc(cPos, 4);
        end
      else if Decl[cPos] = #39 then
        begin
          VIsTextDecl := not VIsTextDecl;
          inc(cPos);
        end
      else
        begin
          if VIsTextDecl then
            begin
              Result.Append(Decl[cPos]);
              inc(cPos);
            end
          else if Decl[cPos] = '#' then
            begin
              nText := '';
              inc(cPos);
              while cPos <= Decl.Len do
                begin
                  if CharIn(Decl[cPos], [cHex], '$') then
                    begin
                      nText.Append(Decl[cPos]);
                      inc(cPos);
                    end
                  else
                      Break;
                end;
              Result.Append(SystemChar(umlStrToInt(nText, 0)));
            end
          else
              inc(cPos);
        end;
    end;
end;

class function TTextParsing.TranslateTextToPascalDecl(const Decl: TPascalString): TPascalString;
var
  cPos: Integer;
  c: SystemChar;
  LastIsOrdChar: Boolean;
  ordCharInfo: TPascalString;
begin
  if Decl.Len = 0 then
    begin
      Result := #39#39;
      exit;
    end;

  ordCharInfo.Len := 32;
  for cPos := 0 to 31 do
      ordCharInfo.buff[cPos] := SystemChar(Ord(cPos));
  ordCharInfo[32] := #39;

  Result := '';
  LastIsOrdChar := False;
  for cPos := 1 to Decl.Len do
    begin
      c := Decl[cPos];
      if CharIn(c, ordCharInfo) then
        begin
          if Result.Len = 0 then
              Result := '#' + umlIntToStr(Ord(c))
          else if LastIsOrdChar then
              Result.Append('#' + umlIntToStr(Ord(c)))
          else
              Result.Append(#39 + '#' + umlIntToStr(Ord(c)));
          LastIsOrdChar := True;
        end
      else
        begin
          if Result.Len = 0 then
              Result := #39 + c
          else if LastIsOrdChar then
              Result.Append(#39 + c)
          else
              Result.Append(c);

          LastIsOrdChar := False;
        end;
    end;

  if not LastIsOrdChar then
      Result.Append(#39);
end;

class function TTextParsing.TranslateTextToPascalDeclWithUnicode(const Decl: TPascalString): TPascalString;
var
  cPos: Integer;
  c: SystemChar;
  LastIsOrdChar: Boolean;
  ordCharInfo: TPascalString;
begin
  if Decl.Len = 0 then
    begin
      Result := #39#39;
      exit;
    end;

  ordCharInfo.Len := 32;
  for cPos := 0 to 31 do
      ordCharInfo[cPos] := SystemChar(Ord(cPos));
  ordCharInfo[32] := #39;

  Result := '';
  LastIsOrdChar := False;
  for cPos := 1 to Decl.Len do
    begin
      c := Decl[cPos];
      if CharIn(c, ordCharInfo) or (Ord(c) >= $80) then
        begin
          if Result.Len = 0 then
              Result := '#' + umlIntToStr(Ord(c))
          else if LastIsOrdChar then
              Result.Append('#' + umlIntToStr(Ord(c)))
          else
              Result.Append(#39 + '#' + umlIntToStr(Ord(c)));
          LastIsOrdChar := True;
        end
      else
        begin
          if Result.Len = 0 then
              Result := #39 + c
          else if LastIsOrdChar then
              Result.Append(#39 + c)
          else
              Result.Append(c);

          LastIsOrdChar := False;
        end;
    end;

  if not LastIsOrdChar then
      Result.Append(#39);
end;

class function TTextParsing.TranslateC_DeclToText(const Decl: TPascalString): TPascalString;
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
  while cPos <= Decl.Len do
    begin
      if Decl[cPos] = '"' then
        begin
          VIsTextDecl := not VIsTextDecl;
          inc(cPos);
        end
      else
        begin
          wasC := False;
          for i := low(CTranslateTable) to high(CTranslateTable) do
            begin
              if Decl.ComparePos(cPos, CTranslateTable[i].c) then
                begin
                  inc(cPos, Length(CTranslateTable[i].c));
                  Result.Append(CTranslateTable[i].s);
                  wasC := True;
                  Break;
                end;
            end;
          if (not wasC) then
            begin
              if (VIsTextDecl) then
                  Result.Append(Decl[cPos]);
              inc(cPos);
            end;
        end;
    end;
end;

class function TTextParsing.TranslateTextToC_Decl(const Decl: TPascalString): TPascalString;
  function GetCStyle(const c: SystemChar): SystemString; inline;
  var
    i: Integer;
  begin
    Result := '';
    for i := low(CTranslateTable) to high(CTranslateTable) do
      if c = CTranslateTable[i].s then
        begin
          Result := CTranslateTable[i].c;
          Break;
        end;
  end;

var
  cPos: Integer;
  c: SystemChar;
  LastIsOrdChar: Boolean;
  n: SystemString;
begin
  if Decl.Len = 0 then
    begin
      Result := '""';
      exit;
    end;

  Result := '';
  LastIsOrdChar := False;
  for cPos := 1 to Decl.Len do
    begin
      c := Decl[cPos];

      if Result.Len = 0 then
          Result := '"' + c
      else
        begin
          n := GetCStyle(c);
          if n <> '' then
              Result.Append(n)
          else
              Result.Append(c);
        end;
    end;

  if not LastIsOrdChar then
      Result.Append('"');
end;

class function TTextParsing.TranslatePascalDeclCommentToText(const Decl: TPascalString): TPascalString;
begin
  Result := umlTrimSpace(Decl);
  if umlMultipleMatch(False, '{*}', Result) then
    begin
      Result.DeleteFirst;
      Result.DeleteLast;
      if umlMultipleMatch(False, '$*', umlTrimSpace(Result)) then
          Result := Decl;
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

class function TTextParsing.TranslateTextToPascalDeclComment(const Decl: TPascalString): TPascalString;
var
  n: TPascalString;
begin
  n := umlTrimSpace(Decl);
  if umlMultipleMatch(False, '(*?*)', n, '?', '') then
      Result := Decl
  else if umlMultipleMatch(False, '{*}', n) then
      Result := Decl
  else if n.Exists(['{', '}']) then
      Result := '(* ' + Decl + ' *)'
  else
      Result := '{ ' + Decl + ' }';
end;

class function TTextParsing.TranslateC_DeclCommentToText(const Decl: TPascalString): TPascalString;
begin
  Result := umlTrimSpace(Decl);
  if umlMultipleMatch(False, '#*', Result) then
    begin
      Result := Decl;
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

class function TTextParsing.TranslateTextToC_DeclComment(const Decl: TPascalString): TPascalString;
var
  n: TPascalString;
begin
  n := umlTrimSpace(Decl);
  if umlMultipleMatch(False, '#*', n) then
      Result := Decl
  else
      Result := '/* ' + n + ' */';
end;

constructor TTextParsing.Create(const AText: TPascalString; AStyle: TTextStyle; ASpecialSymbol: TListPascalString);
begin
  inherited Create;
  ParsingData.Cache.CommentDecls := nil;
  ParsingData.Cache.TextDecls := nil;
  ParsingData.Cache.TokenDataList := nil;
  SetLength(ParsingData.Cache.CharToken, 0);
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
  RebuildCacheBusy := False;

  RebuildParsingCache;

  Init;
end;

constructor TTextParsing.Create(const AText: TPascalString; AStyle: TTextStyle);
begin
  inherited Create;
  ParsingData.Cache.CommentDecls := nil;
  ParsingData.Cache.TextDecls := nil;
  ParsingData.Cache.TokenDataList := nil;
  SetLength(ParsingData.Cache.CharToken, 0);
  if AText.Len = 0 then
      ParsingData.Text := #13#10
  else
      ParsingData.Text := AText + #32;
  ParsingData.Len := ParsingData.Text.Len + 1;
  TextStyle := AStyle;
  SymbolTable := V_SpacerSymbol;
  TokenStatistics := NullTokenStatistics;
  SpecialSymbol := TListPascalString.Create;
  RebuildCacheBusy := False;

  RebuildParsingCache;

  Init;
end;

constructor TTextParsing.Create(const AText: TPascalString; AStyle: TTextStyle; ASpecialSymbol: TListPascalString; ASpacerSymbol: SystemString);
begin
  inherited Create;
  ParsingData.Cache.CommentDecls := nil;
  ParsingData.Cache.TextDecls := nil;
  ParsingData.Cache.TokenDataList := nil;
  SetLength(ParsingData.Cache.CharToken, 0);
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
  RebuildCacheBusy := False;

  RebuildParsingCache;

  Init;
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
  SetLength(ParsingData.Cache.CharToken, 0);

  TokenStatistics := NullTokenStatistics;
  DisposeObject(SpecialSymbol);
  inherited Destroy;
end;

procedure TTextParsing.Init;
begin

end;

function TTextParsing.Parsing: Boolean;
begin
  Result := False;
end;

procedure TTextParsing.Print;
var
  i: Integer;
  pt: PTokenData;
begin
  for i := 0 to ParsingData.Cache.TokenDataList.Count - 1 do
    begin
      pt := ParsingData.Cache.TokenDataList[i];
      DoStatus(PFormat('index: %d type: %s value: %s', [i, GetEnumName(TypeInfo(TTokenType), Ord(pt^.tokenType)), pt^.Text.Text]));
    end;
end;

end.
