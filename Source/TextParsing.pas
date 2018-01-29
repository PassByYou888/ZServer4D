{ ***************************************************************************** }
{ * parsing library,writen by QQ 600585@qq.com                                * }
{ * https://github.com/PassByYou888/CoreCipher                                * }
(* https://github.com/PassByYou888/ZServer4D *)
{ ***************************************************************************** }

unit TextParsing;

{$I zDefine.inc}

interface

uses SysUtils, Types, CoreClasses, UnicodeMixedLib, PascalStrings;

type
  TTextStyle = (tsPascal, tsText);

  TTokenType       = (ttTextDecl, ttComment, ttNumber, ttSymbol, ttAscii, ttUnknow);
  TTokenStatistics = array [TTokenType] of Integer;

  TTextPos = record
    bPos, ePos: Integer;
    Text: umlString;
  end;

  PTextPos = ^TTextPos;

  TTokenData = record
    bPos, ePos: Integer;
    Text: umlString;
    tokenType: TTokenType;
  end;

  PTokenData = ^TTokenData;

  TTextParsingCache = record
    CommentData: TCoreClassList;
    TextData: TCoreClassList;
    TokenDataList: TCoreClassList;
  end;

  TTextParsingData = record
    Cache: TTextParsingCache;
    Text: umlString;
    Len: Integer;
  end;

  TTextParsing = class(TCoreClassObject)
  protected
    FTextStyle: TTextStyle;
  public
    ParsingData    : TTextParsingData;
    SymbolTable    : umlString;
    TokenStatistics: TTokenStatistics;

    property TextStyle: TTextStyle read FTextStyle;

    function ComparePosStr(charPos: Integer; t: umlString): Boolean; virtual;

    function CompareCommentGetEndPos(charPos: Integer): Integer; virtual;
    function CompareTextDeclGetEndPos(charPos: Integer): Integer; virtual;
    procedure RebuildParsingCache; virtual;
    procedure RebuildText;

    function GetContextBeginPos(const charPos: Integer): Integer; virtual;
    function GetContextEndPos(const charPos: Integer): Integer; virtual;

    function IsNumber(const charPos: Integer): Boolean; overload;
    function IsNumber(const charPos: Integer; var NumberBegin: Integer; var IsHex: Boolean): Boolean; overload; virtual;
    function GetNumberEndPos(charPos: Integer): Integer; virtual;

    function IsTextDecl(const charPos: Integer): Boolean; virtual;
    function GetTextDeclEndPos(const charPos: Integer): Integer;
    function GetTextDeclBeginPos(const charPos: Integer): Integer;
    function GetTextBody(const AText: umlString): umlString; virtual;
    function GetTextDeclPos(charPos: Integer; var charBeginPos, charEndPos: Integer): Boolean; virtual;

    function IsSymbol(const charPos: Integer): Boolean; virtual;
    function GetSymbolEndPos(charPos: Integer): Integer; virtual;

    function IsAscii(const charPos: Integer): Boolean; virtual;
    function GetAsciiBeginPos(const charPos: Integer): Integer; virtual;
    function GetAsciiEndPos(const charPos: Integer): Integer; virtual;

    function IsComment(charPos: Integer): Boolean; virtual;
    function GetCommentEndPos(charPos: Integer): Integer;
    function GetCommentBeginPos(charPos: Integer): Integer;
    function GetCommentPos(charPos: Integer; var charBeginPos, charEndPos: Integer): Boolean; virtual;
    function GetDeletedCommentText: umlString;

    function IsTextOrComment(charPos: Integer): Boolean;
    function IsCommentOrText(charPos: Integer): Boolean;

    function isWordSplitChar(c: umlChar; SplitCharSet: umlString): Boolean; overload;
    function isWordSplitChar(c: umlChar): Boolean; overload;
    function isWordSplitChar(c: umlChar; DefaultChar: Boolean; SplitCharSet: umlString): Boolean; overload; virtual;

    function GetWordBeginPos(charPos: Integer; SplitCharSet: umlString): Integer; overload;
    function GetWordBeginPos(charPos: Integer): Integer; overload;
    function GetWordBeginPos(charPos: Integer; BeginDefaultChar: Boolean; SplitCharSet: umlString): Integer; overload; virtual;

    function GetWordEndPos(charPos: Integer; SplitCharSet: umlString): Integer; overload;
    function GetWordEndPos(charPos: Integer): Integer; overload;
    function GetWordEndPos(charPos: Integer; BeginSplitCharSet, EndSplitCharSet: umlString): Integer; overload;
    function GetWordEndPos(charPos: Integer;
      BeginDefaultChar: Boolean; BeginSplitCharSet: umlString;
      EndDefaultChar: Boolean; EndSplitCharSet: umlString): Integer; overload; virtual;

    function GetToken(charPos: Integer): PTokenData;
    function GetTokenIndex(t: TTokenType; idx: Integer): PTokenData;
    property TokenIndex[t: TTokenType; idx: Integer]: PTokenData read GetTokenIndex;

    function SniffingNextChar(charPos: Integer; declChar: umlString): Boolean; overload;
    function SniffingNextChar(charPos: Integer; declChar: umlString; out OutPos: Integer): Boolean; overload; virtual;

    function SplitText(charPos: Integer; out LastPos: Integer; SplitCharSet, EndCharSet: umlString): umlArrayString;

    function GetStr(bPos, ePos: Integer): umlString; overload;
    function GetStr(const tp: TTextPos): umlString; overload;

    function GetWordStr(const charPos: Integer): umlString; overload;

    function GetPoint(charPos: Integer): TPoint;
    function GetChar(charPos: Integer): umlChar;
    property Len: Integer read ParsingData.Len;
    property TextChar[charPos: Integer]: umlChar read GetChar; default;

    procedure DeletePos(bPos, ePos: Integer); overload;
    procedure DeletePos(const tp: TTextPos); overload;

    procedure DeletedComment;

    procedure InsertTextBlock(bPos, ePos: Integer; AInsertText: umlString); overload;
    procedure InsertTextBlock(const tp: TTextPos; AInsertText: umlString); overload;

    function SearchWordInBody(initPos: Integer; wordInfo: umlString; var OutPos: TTextPos): Boolean;

    function GetTextData: umlString; virtual;
    procedure SetTextData(const Value: umlString); virtual;
    property TextData: umlString read GetTextData write SetTextData;

    class function TranslatePascalDeclToText(Text: umlString): umlString;
    class function TranslateTextToPascalDecl(Text: umlString): umlString;

    constructor Create(AText: umlString; AStyle: TTextStyle); virtual;
    destructor Destroy; override;

    function Parsing: Boolean; virtual;
  end;

  TTextParsingClass = class of TTextParsing;

const
  NullTokenStatistics: TTokenStatistics = (0, 0, 0, 0, 0, 0);
  // DefaultSymbol                         = ',.+-*/();:=#@^&%!"[]<>?{}'#39;
  DefaultSymbol = #44#46#43#45#42#47#40#41#59#58#61#35#64#94#38#37#33#34#91#93#60#62#63#123#125#39;

implementation

function TTextParsing.ComparePosStr(charPos: Integer; t: umlString): Boolean;
var
  i, l              : Integer;
  sourChar, destChar: umlChar;
begin
  Result := False;
  i := 1;
  l := t.Len;
  if (charPos + l) > ParsingData.Len then
      Exit;
  while (i <= l) do
    begin
      sourChar := ParsingData.Text[charPos + i - 1];
      destChar := t[i];
      if (sourChar >= 'a') and (sourChar <= 'z') then
          Dec(sourChar, 32);
      if (destChar >= 'a') and (destChar <= 'z') then
          Dec(destChar, 32);
      if sourChar <> destChar then
          Exit;
      Inc(i);
    end;
  Result := True;
end;

function TTextParsing.CompareCommentGetEndPos(charPos: Integer): Integer;
label goto1;
var
  l: Integer;
begin
  l := ParsingData.Len;
  if charPos < 1 then
      charPos := 1;
  if charPos > l then
      charPos := l;

  Result := charPos;

  if ComparePosStr(Result, '//') then
    begin
      Inc(Result, 2);
      while ParsingData.Text[Result] <> #10 do
        begin
          if Result + 1 > l then
              Break;
          Inc(Result);
        end;
    end
  else if (FTextStyle = tsPascal) and (ComparePosStr(Result, '{')) then
    begin
      Inc(Result, 1);
      while ParsingData.Text[Result] <> '}' do
        begin
          if Result + 1 > l then
              Break;
          Inc(Result);
        end;
      Inc(Result, 1);
    end
  else if (FTextStyle = tsPascal) and (ComparePosStr(Result, '(*')) then
    begin
      Inc(Result, 2);
      while not ComparePosStr(Result, '*)') do
        begin
          if Result + 1 > l then
              Break;
          Inc(Result);
        end;
      Inc(Result, 2);
    end;
end;

function TTextParsing.CompareTextDeclGetEndPos(charPos: Integer): Integer;
var
  l     : Integer;
  tmpPos: Integer;
begin
  l := ParsingData.Len;
  if charPos < 1 then
      charPos := 1;
  if charPos > l then
      charPos := l;

  Result := charPos;

  // #39 = '
  if (Result + 1 < l) and (FTextStyle = tsPascal) and (ComparePosStr(Result, #39)) then
    begin
      if ComparePosStr(Result, #39#39#39#39) then
        begin
          Result := CompareTextDeclGetEndPos(Result + 4);
          Exit;
        end;
      Inc(Result, 1);
      while ParsingData.Text[Result] <> #39 do
        begin
          if Result + 1 > l then
              Break;
          if ParsingData.Text[Result] = #10 then
              Exit(charPos);
          Inc(Result);
        end;
      Inc(Result, 1);
    end;

  if (Result + 1 < l) and (FTextStyle = tsPascal) and (ComparePosStr(Result, '#')) then
    begin
      repeat
        Inc(Result, 1);
        while isWordSplitChar(ParsingData.Text[Result]) do
          begin
            if Result + 1 > l then
                Exit;
            Inc(Result);
          end;
        while CharIn(ParsingData.Text[Result], c0to9) do
          begin
            if Result + 1 > l then
                Exit;
            Inc(Result);
          end;
        tmpPos := Result;
        while isWordSplitChar(ParsingData.Text[Result]) do
          begin
            if Result + 1 > l then
                Exit;
            Inc(Result);
          end;
      until not ComparePosStr(Result, '#');
      Result := CompareTextDeclGetEndPos(tmpPos);
    end;
end;

procedure TTextParsing.RebuildParsingCache;
var
  i, l, bPos, ePos: Integer;
  textPosPtr      : PTextPos;
  LastTokenData   : PTokenData;
begin
  if ParsingData.Cache.CommentData <> nil then
    begin
      for i := 0 to ParsingData.Cache.CommentData.Count - 1 do
          Dispose(PTextPos(ParsingData.Cache.CommentData[i]));
      DisposeObject(ParsingData.Cache.CommentData);
      ParsingData.Cache.CommentData := nil;
    end;

  if ParsingData.Cache.TextData <> nil then
    begin
      for i := 0 to ParsingData.Cache.TextData.Count - 1 do
          Dispose(PTextPos(ParsingData.Cache.TextData[i]));
      DisposeObject(ParsingData.Cache.TextData);
      ParsingData.Cache.TextData := nil;
    end;

  if ParsingData.Cache.TokenDataList <> nil then
    begin
      for i := 0 to ParsingData.Cache.TokenDataList.Count - 1 do
          Dispose(PTokenData(ParsingData.Cache.TokenDataList[i]));
      DisposeObject(ParsingData.Cache.TokenDataList);
      ParsingData.Cache.TokenDataList := nil;
    end;

  TokenStatistics := NullTokenStatistics;

  ParsingData.Cache.CommentData := TCoreClassList.Create;
  ParsingData.Cache.TextData := TCoreClassList.Create;
  ParsingData.Cache.TokenDataList := TCoreClassList.Create;

  // rebuild comment and text
  l := ParsingData.Len;
  bPos := 1;
  ePos := bPos;
  while (bPos <= l) do
    begin
      ePos := CompareCommentGetEndPos(bPos);
      if ePos > bPos then
        begin
          new(textPosPtr);
          textPosPtr^.bPos := bPos;
          textPosPtr^.ePos := ePos;
          textPosPtr^.Text := GetStr(textPosPtr^);
          ParsingData.Cache.CommentData.Add(textPosPtr);
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
              ParsingData.Cache.TextData.Add(textPosPtr);
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
  while bPos <= l do
    begin
      if IsTextDecl(bPos) then
        begin
          ePos := GetTextDeclEndPos(bPos);

          new(LastTokenData);
          LastTokenData^.bPos := bPos;
          LastTokenData^.ePos := ePos;
          LastTokenData^.Text := GetStr(bPos, ePos);
          LastTokenData^.tokenType := ttTextDecl;
          ParsingData.Cache.TokenDataList.Add(LastTokenData);
          Inc(TokenStatistics[LastTokenData^.tokenType]);

          bPos := ePos
        end
      else if IsComment(bPos) then
        begin
          ePos := GetCommentEndPos(bPos);

          new(LastTokenData);
          LastTokenData^.bPos := bPos;
          LastTokenData^.ePos := ePos;
          LastTokenData^.Text := GetStr(bPos, ePos);
          LastTokenData^.tokenType := ttComment;
          ParsingData.Cache.TokenDataList.Add(LastTokenData);
          Inc(TokenStatistics[LastTokenData^.tokenType]);

          bPos := ePos;
        end
      else if IsNumber(bPos) then
        begin
          ePos := GetNumberEndPos(bPos);

          new(LastTokenData);
          LastTokenData^.bPos := bPos;
          LastTokenData^.ePos := ePos;
          LastTokenData^.Text := GetStr(bPos, ePos);
          LastTokenData^.tokenType := ttNumber;
          ParsingData.Cache.TokenDataList.Add(LastTokenData);
          Inc(TokenStatistics[LastTokenData^.tokenType]);

          bPos := ePos;
        end
      else if IsSymbol(bPos) then
        begin
          ePos := GetSymbolEndPos(bPos);

          new(LastTokenData);
          LastTokenData^.bPos := bPos;
          LastTokenData^.ePos := ePos;
          LastTokenData^.Text := GetStr(bPos, ePos);
          LastTokenData^.tokenType := ttSymbol;
          ParsingData.Cache.TokenDataList.Add(LastTokenData);
          Inc(TokenStatistics[LastTokenData^.tokenType]);

          bPos := ePos;
        end
      else if IsAscii(bPos) then
        begin
          ePos := GetAsciiEndPos(bPos);

          new(LastTokenData);
          LastTokenData^.bPos := bPos;
          LastTokenData^.ePos := ePos;
          LastTokenData^.Text := GetStr(bPos, ePos);
          LastTokenData^.tokenType := ttAscii;
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
  procedure DoRecalcAllCache(bPos, d: Integer);
  var
    i: Integer;
    p: PTextPos;
  begin
    for i := 0 to ParsingData.Cache.TextData.Count - 1 do
      begin
        p := PTextPos(ParsingData.Cache.TextData[i]);
        if bPos < p^.bPos then
          begin
            p^.bPos := p^.bPos - d;
            p^.ePos := p^.ePos - d;
          end;
      end;
    for i := 0 to ParsingData.Cache.CommentData.Count - 1 do
      begin
        p := PTextPos(ParsingData.Cache.CommentData[i]);
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
  if ParsingData.Cache.TextData = nil then
      RebuildParsingCache;

  for i := 0 to ParsingData.Cache.TextData.Count - 1 do
    begin
      p := PTextPos(ParsingData.Cache.TextData[i]);
      if p^.ePos - p^.bPos <> (p^.Text.Len) then
          DoRecalcAllCache(p^.bPos, (p^.ePos - p^.bPos) - p^.Text.Len);

      ParsingData.Text := GetStr(1, p^.bPos) + p^.Text + GetStr(p^.ePos, ParsingData.Text.Len + 1);
      ParsingData.Len := ParsingData.Text.Len;
      p^.ePos := p^.bPos + p^.Text.Len;
    end;
  for i := 0 to ParsingData.Cache.CommentData.Count - 1 do
    begin
      p := PTextPos(ParsingData.Cache.CommentData[i]);
      if p^.ePos - p^.bPos <> (p^.Text.Len) then
          DoRecalcAllCache(p^.bPos, (p^.ePos - p^.bPos) - p^.Text.Len);

      ParsingData.Text := GetStr(1, p^.bPos) + p^.Text + GetStr(p^.ePos, ParsingData.Text.Len + 1);
      ParsingData.Len := ParsingData.Text.Len;
      p^.ePos := p^.bPos + p^.Text.Len;
    end;

  RebuildParsingCache;
end;

function TTextParsing.GetContextBeginPos(const charPos: Integer): Integer;
var
  cPos: Integer;
  l   : Integer;
begin
  cPos := charPos;
  l := ParsingData.Len;
  if cPos < 1 then
      cPos := 1;
  if cPos > l then
      cPos := l;

  while cPos < l do
    begin
      if IsTextDecl(cPos) then
        begin
          cPos := GetTextDeclBeginPos(cPos);
          Break;
        end
      else if IsComment(cPos) then
        begin
          cPos := GetCommentBeginPos(cPos);
          Break;
        end
      else if IsNumber(cPos) then
        begin
          Break;
        end
      else if IsSymbol(cPos) then
        begin
          Break;
        end
      else if IsAscii(cPos) then
        begin
          cPos := GetAsciiBeginPos(cPos);
          Break;
        end;
      Inc(cPos);
    end;

  Result := cPos;
end;

function TTextParsing.GetContextEndPos(const charPos: Integer): Integer;
var
  cPos: Integer;
  l   : Integer;
begin
  cPos := charPos;
  l := ParsingData.Len;
  if cPos < 1 then
      cPos := 1;
  if cPos > l then
      cPos := l;

  while cPos < l do
    begin
      if IsTextDecl(cPos) then
        begin
          cPos := GetTextDeclEndPos(cPos);
          Break;
        end
      else if IsComment(cPos) then
        begin
          cPos := GetCommentEndPos(cPos);
          Break;
        end
      else if IsNumber(cPos) then
        begin
          cPos := GetNumberEndPos(cPos);
          Break;
        end
      else if IsSymbol(cPos) then
        begin
          cPos := GetSymbolEndPos(cPos);
          Break;
        end
      else if IsAscii(cPos) then
        begin
          cPos := GetAsciiEndPos(cPos);
          Break;
        end;
      Inc(cPos);
    end;

  Result := cPos;
end;

function TTextParsing.IsNumber(const charPos: Integer): Boolean;
var
  tmp  : Integer;
  IsHex: Boolean;
begin
  Result := IsNumber(charPos, tmp, IsHex);
end;

function TTextParsing.IsNumber(const charPos: Integer; var NumberBegin: Integer; var IsHex: Boolean): Boolean;
var
  c   : umlChar;
  l   : Integer;
  cPos: Integer;
begin
  Result := False;

  cPos := charPos;
  l := ParsingData.Len;
  if cPos < 1 then
      cPos := 1;
  if cPos > l then
      cPos := l;

  c := ParsingData.Text[cPos];
  if CharIn(c, [c0to9]) then
    begin
      Result := True;
      NumberBegin := cPos;
      IsHex := False;
      Exit;
    end
  else if CharIn(c, '+-.$') then
    begin
      IsHex := c = '$';
      if CharIn(c, '.') then
        begin
          while True do
            begin
              Inc(cPos);
              if cPos > l then
                  Exit;

              cPos := GetTextDeclEndPos(GetCommentEndPos(cPos));

              if cPos > l then
                  Exit;

              c := ParsingData.Text[cPos];
              if CharIn(c, [c0to9]) or (IsHex and (CharIn(c, [cLoAtoF, cHiAtoF]))) then
                begin
                  Result := True;
                  NumberBegin := cPos;
                  Exit;
                end
              else
                begin
                  Result := False;
                  Exit;
                end;
            end;
        end
      else
        begin
          while True do
            begin
              Inc(cPos);
              if cPos > l then
                  Exit;

              cPos := GetTextDeclEndPos(GetCommentEndPos(cPos));

              if cPos > l then
                  Exit;
              c := ParsingData.Text[cPos];
              if CharIn(c, [c0to9]) or (IsHex and (CharIn(c, [cLoAtoF, cHiAtoF]))) then
                begin
                  Result := True;
                  NumberBegin := cPos;
                  Exit;
                end
              else
                if CharIn(c, '+-') then
                  Continue;
            end;
        end;
    end;
end;

function TTextParsing.GetNumberEndPos(charPos: Integer): Integer;
var
  IsHex       : Boolean;
  l           : Integer;
  c           : umlChar;
  dotCount    : Integer;
  eCnt        : Integer;
  AddSubSymCnt: Integer;
begin
  l := ParsingData.Len;
  if charPos < 1 then
      charPos := 1;
  if charPos > l then
      charPos := l;

  if IsNumber(charPos, Result, IsHex) then
    begin
      dotCount := 0;
      eCnt := 0;
      AddSubSymCnt := 0;
      while True do
        begin
          Inc(Result);
          if Result > l then
              Exit;

          Result := GetTextDeclEndPos(GetCommentEndPos(Result));

          if Result > l then
              Exit;

          c := ParsingData.Text[Result];

          if (not CharIn(c, [c0to9])) then
            begin
              if (not IsHex) and (dotCount <= 1) and (eCnt = 0) and (AddSubSymCnt = 0) and (CharIn(c, 'eE')) then
                begin
                  Inc(eCnt);
                end
              else if (not IsHex) and (dotCount <= 1) and (eCnt = 1) and (AddSubSymCnt = 0) and (CharIn(c, '+-')) then
                begin
                  Inc(AddSubSymCnt);
                end
              else if (not IsHex) and (dotCount = 0) and (eCnt <= 1) and (AddSubSymCnt <= 1) and (CharIn(c, '.')) then
                begin
                  if ((Result + 1 > l) or (not CharIn(ParsingData.Text[Result + 1], [c0to9]))) then
                      Exit
                  else
                      Inc(dotCount);
                end
              else if not(IsHex and (CharIn(c, [cLoAtoF, cHiAtoF]))) then
                  Exit;
            end;
        end;
    end
  else
      Result := charPos;
end;

function TTextParsing.IsTextDecl(const charPos: Integer): Boolean;
var
  bPos, ePos: Integer;
begin
  Result := GetTextDeclPos(charPos, bPos, ePos);
end;

function TTextParsing.GetTextDeclEndPos(const charPos: Integer): Integer;
var
  bPos, ePos: Integer;
begin
  if GetTextDeclPos(charPos, bPos, ePos) then
      Result := ePos
  else
      Result := charPos;
end;

function TTextParsing.GetTextDeclBeginPos(const charPos: Integer): Integer;
var
  bPos, ePos: Integer;
begin
  if GetTextDeclPos(charPos, bPos, ePos) then
      Result := bPos
  else
      Result := charPos;
end;

function TTextParsing.GetTextBody(const AText: umlString): umlString;
begin
  if FTextStyle = tsPascal then
      Result := TranslatePascalDeclToText(AText)
  else
      Result := AText;
end;

function TTextParsing.GetTextDeclPos(charPos: Integer; var charBeginPos, charEndPos: Integer): Boolean;
  function CompLst(idx: Integer): Integer;
  begin
    with PTextPos(ParsingData.Cache.TextData[idx])^ do
      begin
        if (charPos >= bPos) and (charPos < ePos) then
            Result := 0
        else if (charPos >= ePos) then
            Result := -1
        else if (charPos < bPos) then
            Result := 1
        else
            Result := -2;
      end;
  end;

var
  l, r, m: Integer;
begin
  if charPos < 1 then
      charPos := 1;
  if charPos > ParsingData.Len then
      charPos := ParsingData.Len;

  if ParsingData.Cache.TextData = nil then
      RebuildParsingCache;

  Result := False;

  l := 0;
  r := ParsingData.Cache.TextData.Count - 1;
  while l <= r do
    begin
      m := (l + r) div 2;
      case CompLst(m) of
        0:
          begin
            with PTextPos(ParsingData.Cache.TextData[m])^ do
              begin
                charBeginPos := bPos;
                charEndPos := ePos;
              end;
            Result := True;
            Exit;
          end;
        -1: l := m + 1;
        1: r := m - 1;
        else raise exception.Create('struct error');
      end;
    end;
end;

function TTextParsing.IsSymbol(const charPos: Integer): Boolean;
begin
  Result := CharIn(ParsingData.Text[charPos], SymbolTable);
end;

function TTextParsing.GetSymbolEndPos(charPos: Integer): Integer;
begin
  if IsSymbol(charPos) then
      Result := charPos + 1
  else
      Result := charPos;
end;

function TTextParsing.IsAscii(const charPos: Integer): Boolean;
begin
  Result := (not IsSymbol(charPos)) and (not IsNumber(charPos)) and (not IsComment(charPos)) and (not IsTextDecl(charPos)) and
    (not isWordSplitChar(ParsingData.Text[charPos], True, SymbolTable));
end;

function TTextParsing.GetAsciiBeginPos(const charPos: Integer): Integer;
begin
  Result := GetWordBeginPos(charPos, True, SymbolTable);
end;

function TTextParsing.GetAsciiEndPos(const charPos: Integer): Integer;
begin
  Result := GetWordEndPos(charPos, True, SymbolTable, True, SymbolTable);
end;

function TTextParsing.IsComment(charPos: Integer): Boolean;
var
  bPos, ePos: Integer;
begin
  Result := GetCommentPos(charPos, bPos, ePos);
end;

function TTextParsing.GetCommentEndPos(charPos: Integer): Integer;
var
  bPos, ePos: Integer;
begin
  if GetCommentPos(charPos, bPos, ePos) then
      Result := ePos
  else
      Result := charPos;
end;

function TTextParsing.GetCommentBeginPos(charPos: Integer): Integer;
var
  bPos, ePos: Integer;
begin
  if GetCommentPos(charPos, bPos, ePos) then
      Result := bPos
  else
      Result := charPos;
end;

function TTextParsing.GetCommentPos(charPos: Integer; var charBeginPos, charEndPos: Integer): Boolean;
  function CompLst(idx: Integer): Integer;
  begin
    with PTextPos(ParsingData.Cache.CommentData[idx])^ do
      begin
        if (charPos >= bPos) and (charPos < ePos) then
            Result := 0
        else if (charPos >= ePos) then
            Result := -1
        else if (charPos < bPos) then
            Result := 1
        else
            Result := -2;
      end;
  end;

var
  l, r, m: Integer;
begin
  if charPos < 1 then
      charPos := 1;
  if charPos > ParsingData.Len then
      charPos := ParsingData.Len;

  if ParsingData.Cache.CommentData = nil then
      RebuildParsingCache;

  Result := False;

  l := 0;
  r := ParsingData.Cache.CommentData.Count - 1;
  while l <= r do
    begin
      m := (l + r) div 2;
      case CompLst(m) of
        0:
          begin
            with PTextPos(ParsingData.Cache.CommentData[m])^ do
              begin
                charBeginPos := bPos;
                charEndPos := ePos;
              end;
            Result := True;
            Exit;
          end;
        -1: l := m + 1;
        1: r := m - 1;
        else raise exception.Create('struct error');
      end;
    end;
end;

function TTextParsing.GetDeletedCommentText: umlString;
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

function TTextParsing.IsTextOrComment(charPos: Integer): Boolean;
begin
  Result := IsTextDecl(charPos) or IsComment(charPos);
end;

function TTextParsing.IsCommentOrText(charPos: Integer): Boolean;
begin
  Result := IsComment(charPos) or IsTextDecl(charPos);
end;

function TTextParsing.isWordSplitChar(c: umlChar; SplitCharSet: umlString): Boolean;
begin
  Result := isWordSplitChar(c, True, SplitCharSet);
end;

function TTextParsing.isWordSplitChar(c: umlChar): Boolean;
begin
  Result := isWordSplitChar(c, True, '');
end;

function TTextParsing.isWordSplitChar(c: umlChar; DefaultChar: Boolean; SplitCharSet: umlString): Boolean;
begin
  if DefaultChar then
      Result := CharIn(c, [c0To32], SplitCharSet)
  else
      Result := CharIn(c, SplitCharSet);
end;

function TTextParsing.GetWordBeginPos(charPos: Integer; SplitCharSet: umlString): Integer;
begin
  Result := GetWordBeginPos(charPos, True, SplitCharSet);
end;

function TTextParsing.GetWordBeginPos(charPos: Integer): Integer;
begin
  Result := GetWordBeginPos(charPos, True, '');
end;

function TTextParsing.GetWordBeginPos(charPos: Integer; BeginDefaultChar: Boolean; SplitCharSet: umlString): Integer;
var
  l    : Integer;
  tbPos: Integer;
begin
  l := ParsingData.Len;
  if charPos < 1 then
      Exit(1);
  if charPos > l then
      Exit(l);

  repeat
    charPos := GetCommentEndPos(charPos);

    tbPos := GetTextDeclBeginPos(charPos);
    if tbPos <> charPos then
        Exit(tbPos);

    while (isWordSplitChar(ParsingData.Text[charPos], BeginDefaultChar, SplitCharSet)) do
      begin
        if charPos >= l then
            Break;
        Inc(charPos);
      end;
  until not IsComment(charPos);

  Result := charPos;
  while (IsTextOrComment(Result)) or (not isWordSplitChar(ParsingData.Text[Result], BeginDefaultChar, SplitCharSet)) do
    begin
      if Result - 1 <= 0 then
          Break;
      Dec(Result);
    end;

  if isWordSplitChar(ParsingData.Text[Result], SplitCharSet) then
      Inc(Result);
end;

function TTextParsing.GetWordEndPos(charPos: Integer; SplitCharSet: umlString): Integer;
begin
  Result := GetWordEndPos(charPos, True, SplitCharSet, True, SplitCharSet);
end;

function TTextParsing.GetWordEndPos(charPos: Integer): Integer;
begin
  Result := GetWordEndPos(charPos, True, '', True, '');
end;

function TTextParsing.GetWordEndPos(charPos: Integer; BeginSplitCharSet, EndSplitCharSet: umlString): Integer;
begin
  Result := GetWordEndPos(charPos, True, BeginSplitCharSet, True, EndSplitCharSet);
end;

function TTextParsing.GetWordEndPos(charPos: Integer;
  BeginDefaultChar: Boolean; BeginSplitCharSet: umlString;
  EndDefaultChar: Boolean; EndSplitCharSet: umlString): Integer;
var
  l: Integer;
begin
  l := ParsingData.Len;
  if charPos < 1 then
      Exit(1);
  if charPos > l then
      Exit(l);

  Result := GetWordBeginPos(charPos, BeginDefaultChar, BeginSplitCharSet);

  while (IsTextOrComment(Result)) or (not isWordSplitChar(ParsingData.Text[Result], EndDefaultChar, EndSplitCharSet)) do
    begin
      Inc(Result);
      if Result > l then
          Break;
    end;
end;

function TTextParsing.GetToken(charPos: Integer): PTokenData;
  function CompLst(idx: Integer): Integer;
  begin
    with PTokenData(ParsingData.Cache.TokenDataList[idx])^ do
      begin
        if (charPos >= bPos) and (charPos < ePos) then
            Result := 0
        else if (charPos >= ePos) then
            Result := -1
        else if (charPos < bPos) then
            Result := 1
        else
            Result := -2;
      end;
  end;

var
  l, r, m: Integer;
begin
  Result := nil;

  if (charPos > Len) or (charPos < 0) then
      Exit;

  if ParsingData.Cache.TokenDataList = nil then
      RebuildParsingCache;

  l := 0;
  r := ParsingData.Cache.TokenDataList.Count - 1;
  while l <= r do
    begin
      m := (l + r) div 2;
      case CompLst(m) of
        0: Exit(PTokenData(ParsingData.Cache.TokenDataList[m]));
        -1: l := m + 1;
        1: r := m - 1;
        else raise exception.Create('struct error');
      end;
    end;
end;

function TTextParsing.GetTokenIndex(t: TTokenType; idx: Integer): PTokenData;
var
  i, c: Integer;
  p   : PTokenData;
begin
  Result := nil;
  c := 0;
  for i := 0 to ParsingData.Cache.TokenDataList.Count - 1 do
    begin
      p := PTokenData(ParsingData.Cache.TokenDataList[i]);
      if p^.tokenType = t then
        begin
          if c = idx then
              Exit(p)
          else
              Inc(c);
        end;
    end;
end;

function TTextParsing.SniffingNextChar(charPos: Integer; declChar: umlString): Boolean;
var
  tmp: Integer;
begin
  Result := SniffingNextChar(charPos, declChar, tmp);
end;

function TTextParsing.SniffingNextChar(charPos: Integer; declChar: umlString; out OutPos: Integer): Boolean;
var
  l: Integer;
begin
  l := ParsingData.Len;
  if charPos < 1 then
      charPos := 1;
  if charPos > l then
      Exit(False);

  while isWordSplitChar(ParsingData.Text[charPos]) or (IsTextOrComment(charPos)) do
    begin
      Inc(charPos);
      if charPos > l then
          Exit(False);
    end;

  if (charPos < l) then
      Result := CharIn(ParsingData.Text[charPos], declChar)
  else
      Result := False;

  if Result then
      OutPos := charPos;
end;

function TTextParsing.SplitText(charPos: Integer; out LastPos: Integer; SplitCharSet, EndCharSet: umlString): umlArrayString;
  procedure AddS(s: umlString);
  var
    l: Integer;
  begin
    l := Length(Result);
    SetLength(Result, l + 1);
    Result[l] := s;
  end;

type
  TLastSym = (lsBody, lsNone);

var
  l         : Integer;
  c         : umlChar;
  bPos, ePos: Integer;
  LastSym   : TLastSym;
begin
  SetLength(Result, 0);
  l := ParsingData.Len;
  if charPos < 1 then
      charPos := 1;
  if charPos > l then
      Exit;

  bPos := charPos;
  ePos := bPos;
  LastSym := lsNone;
  while (charPos <= l) do
    begin
      if IsTextOrComment(charPos) then
        begin
          Inc(charPos);
          Continue;
        end;
      c := ParsingData.Text[charPos];
      if isWordSplitChar(c, True, SplitCharSet) then
        begin
          if LastSym = lsBody then
            begin
              ePos := charPos;
              AddS(GetStr(bPos, ePos));
              LastSym := lsNone;
            end;
          Inc(charPos);
          Continue;
        end;
      if (isWordSplitChar(c, False, EndCharSet)) then
        begin
          if LastSym = lsBody then
            begin
              ePos := charPos;
              AddS(GetStr(bPos, ePos));
              LastSym := lsNone;
            end;
          LastPos := charPos;
          Exit;
        end;

      if LastSym = lsNone then
        begin
          bPos := charPos;
          LastSym := lsBody;
        end;
      Inc(charPos);
    end;

  SetLength(Result, 0);
end;

function TTextParsing.GetStr(bPos, ePos: Integer): umlString;
begin
  Result := ParsingData.Text.copy(bPos, ePos - bPos);
end;

function TTextParsing.GetStr(const tp: TTextPos): umlString;
begin
  Result := GetStr(tp.bPos, tp.ePos);
end;

function TTextParsing.GetWordStr(const charPos: Integer): umlString;
begin
  Result := GetStr(GetAsciiBeginPos(charPos), GetAsciiEndPos(charPos));
end;

function TTextParsing.GetPoint(charPos: Integer): TPoint;
var
  i: Integer;
begin
  Result := Point(1, 1);
  if charPos > ParsingData.Len then
      charPos := ParsingData.Len;
  for i := 1 to charPos - 1 do
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

function TTextParsing.GetChar(charPos: Integer): umlChar;
begin
  Result := ParsingData.Text[charPos];
end;

procedure TTextParsing.DeletePos(bPos, ePos: Integer);
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

procedure TTextParsing.InsertTextBlock(bPos, ePos: Integer; AInsertText: umlString);
begin
  ParsingData.Text := GetStr(1, bPos) + AInsertText + GetStr(ePos, Len + 1);
  ParsingData.Len := ParsingData.Text.Len;
  RebuildParsingCache;
end;

procedure TTextParsing.InsertTextBlock(const tp: TTextPos; AInsertText: umlString);
begin
  InsertTextBlock(tp.bPos, tp.ePos, AInsertText);
end;

function TTextParsing.SearchWordInBody(initPos: Integer; wordInfo: umlString; var OutPos: TTextPos): Boolean;
var
  cp  : Integer;
  ePos: Integer;
begin
  Result := False;

  cp := initPos;

  while cp <= ParsingData.Len do
    begin
      if IsTextDecl(cp) then
        begin
          ePos := GetTextDeclEndPos(cp);
          cp := ePos;
        end
      else if IsComment(cp) then
        begin
          ePos := GetCommentEndPos(cp);
          cp := ePos;
        end
      else if IsNumber(cp) then
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
      else if IsSymbol(cp) then
        begin
          ePos := GetSymbolEndPos(cp);
          cp := ePos;
        end
      else if IsAscii(cp) then
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

function TTextParsing.GetTextData: umlString;
begin
  Result := ParsingData.Text;
end;

procedure TTextParsing.SetTextData(const Value: umlString);
begin
  if Value.Len = 0 then
      ParsingData.Text := #13#10
  else
      ParsingData.Text := Value;

  ParsingData.Len := ParsingData.Text.Len;
  RebuildParsingCache;
end;

class function TTextParsing.TranslatePascalDeclToText(Text: umlString): umlString;
var
  cPos: Integer;

  // ext decl begin flag
  VIsTextDecl: Boolean;
  nText      : umlString;
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
              Result.Append(umlChar(StrToIntDef(nText.Text, 0)));
            end;
        end;
    end;
end;

class function TTextParsing.TranslateTextToPascalDecl(Text: umlString): umlString;
var
  cPos         : Integer;
  c            : umlChar;
  LastIsOrdChar: Boolean;
  ordCharInfo  : umlString;
begin
  if Text.Len = 0 then
    begin
      Result := #39#39;
      Exit;
    end;

  ordCharInfo.Len := 32;
  for cPos := 0 to 31 do
      ordCharInfo[cPos] := umlChar(ord(cPos));
  ordCharInfo[32] := #39;

  Result := '';
  LastIsOrdChar := False;
  for cPos := 1 to Text.Len do
    begin
      c := Text[cPos];
      if CharIn(c, ordCharInfo) then
        begin
          if Result.Len = 0 then
              Result := '#' + IntToStr(ord(c))
          else if LastIsOrdChar then
              Result.Append('#' + IntToStr(ord(c)))
          else
              Result.Append(#39 + '#' + IntToStr(ord(c)));
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

constructor TTextParsing.Create(AText: umlString; AStyle: TTextStyle);
begin
  inherited Create;
  ParsingData.Cache.CommentData := nil;
  ParsingData.Cache.TextData := nil;
  ParsingData.Cache.TokenDataList := nil;
  if AText.Len = 0 then
      ParsingData.Text := #13#10
  else
      ParsingData.Text := AText;
  ParsingData.Len := ParsingData.Text.Len;
  FTextStyle := AStyle;
  SymbolTable := DefaultSymbol;
  TokenStatistics := NullTokenStatistics;
  RebuildParsingCache;
end;

destructor TTextParsing.Destroy;
var
  i: Integer;
begin
  if ParsingData.Cache.CommentData <> nil then
    begin
      for i := 0 to ParsingData.Cache.CommentData.Count - 1 do
          Dispose(PTextPos(ParsingData.Cache.CommentData[i]));
      DisposeObject(ParsingData.Cache.CommentData);
      ParsingData.Cache.CommentData := nil;
    end;

  if ParsingData.Cache.TextData <> nil then
    begin
      for i := 0 to ParsingData.Cache.TextData.Count - 1 do
          Dispose(PTextPos(ParsingData.Cache.TextData[i]));
      DisposeObject(ParsingData.Cache.TextData);
      ParsingData.Cache.TextData := nil;
    end;

  if ParsingData.Cache.TokenDataList <> nil then
    begin
      for i := 0 to ParsingData.Cache.TokenDataList.Count - 1 do
          Dispose(PTokenData(ParsingData.Cache.TokenDataList[i]));
      DisposeObject(ParsingData.Cache.TokenDataList);
      ParsingData.Cache.TokenDataList := nil;
    end;

  TokenStatistics := NullTokenStatistics;
  inherited Destroy;
end;

function TTextParsing.Parsing: Boolean;
begin
  Result := False;
end;

end.
