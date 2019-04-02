{ ****************************************************************************** }
{ * Transform TextTable         writen by QQ 600585@qq.com                     * }
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

unit TextTable;

{$INCLUDE zDefine.inc}

interface

uses SysUtils, CoreClasses, DataFrameEngine, ListEngine, UnicodeMixedLib,
  MemoryStream64, TextParsing, PascalStrings;

type
  TTranlateStyle = (tsPascalText, tsPascalComment, tsCText, tsCComment, tsNormalText, tsDFMText);

  TTextTableItem = record
    // origin info
    OriginText: SystemString;
    Category: SystemString;

    // ext pick info
    Picked: Boolean;

    // encode and import info
    index: Integer;
    DefineText: SystemString;

    // text style
    TextStyle: TTranlateStyle;

    // fast hash
    OriginHash: THash;
    DefineHash: THash;

    // project language
    originLanguage: Integer;
    DefineLanguage: Integer;

    RepCount: Integer;

    procedure InitSelf;
    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);
  end;

  PTextTableItem = ^TTextTableItem;

  TTextTable = class(TCoreClassObject)
  protected
    FList: TCoreClassList;

    function GetItems(index: Integer): PTextTableItem;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    function Count: Integer;
    property Items[index: Integer]: PTextTableItem read GetItems; default;
    procedure Delete(index: Integer);

    function GetMaxIndexNo: Integer;

    function GetOrigin(const s: SystemString): PTextTableItem;
    property origin[const s: SystemString]: PTextTableItem read GetOrigin;

    procedure AddCopy(var t: TTextTableItem);
    procedure AddText(AOriginText, ACategory: SystemString; APicked: Boolean);
    procedure AddPascalText(AOriginText, ACategory: SystemString; APicked: Boolean);
    procedure AddPascalComment(AOriginText, ACategory: SystemString; APicked: Boolean);
    procedure AddCText(AOriginText, ACategory: SystemString; APicked: Boolean);
    procedure AddCComment(AOriginText, ACategory: SystemString; APicked: Boolean);
    procedure AddDelphiFormText(AOriginText, ACategory: SystemString; APicked: Boolean);

    procedure ChangeDefineText(index: Integer; newDefine: U_String);
    function ExistsIndex(index: Integer): Boolean;

    function Search(AOriginText: SystemString): PTextTableItem;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure ExportToTextStream(stream: TCoreClassStream);
    procedure ImportFromTextStream(stream: TCoreClassStream);
  end;

implementation

procedure TTextTableItem.InitSelf;
begin
  OriginText := '';
  Category := '';
  Picked := False;
  index := -1;
  DefineText := '';
  TextStyle := tsNormalText;
  RepCount := 0;
  OriginHash := 0;
  DefineHash := 0;
end;

procedure TTextTableItem.LoadFromStream(stream: TCoreClassStream);
var
  df: TDataFrameEngine;
begin
  df := TDataFrameEngine.Create;
  df.DecodeFrom(stream);

  OriginText := df.Reader.ReadString;
  Category := df.Reader.ReadString;
  Picked := df.Reader.ReadBool;
  index := df.Reader.ReadInteger;
  DefineText := df.Reader.ReadString;
  TextStyle := TTranlateStyle(df.Reader.ReadInteger);
  RepCount := df.Reader.ReadInteger;

  OriginHash := df.Reader.ReadCardinal;
  DefineHash := df.Reader.ReadCardinal;

  originLanguage := df.Reader.ReadInteger;
  DefineLanguage := df.Reader.ReadInteger;

  DisposeObject(df);
end;

procedure TTextTableItem.SaveToStream(stream: TCoreClassStream);
var
  df: TDataFrameEngine;
begin
  df := TDataFrameEngine.Create;
  df.WriteString(OriginText);
  df.WriteString(Category);
  df.WriteBool(Picked);
  df.WriteInteger(index);
  df.WriteString(DefineText);
  df.WriteInteger(Integer(TextStyle));
  df.WriteInteger(RepCount);

  df.WriteCardinal(OriginHash);
  df.WriteCardinal(DefineHash);

  df.WriteInteger(originLanguage);
  df.WriteInteger(DefineLanguage);

  df.EncodeTo(stream);
  DisposeObject(df);
end;

function TTextTable.GetItems(index: Integer): PTextTableItem;
begin
  Result := FList[index];
end;

constructor TTextTable.Create;
begin
  inherited Create;
  FList := TCoreClassList.Create;
end;

destructor TTextTable.Destroy;
begin
  Clear;
  DisposeObject(FList);
  inherited Destroy;
end;

procedure TTextTable.Clear;
var
  i: Integer;
  p: PTextTableItem;
begin
  for i := 0 to FList.Count - 1 do
    begin
      p := FList[i];
      Dispose(p);
    end;
  FList.Clear;
end;

function TTextTable.Count: Integer;
begin
  Result := FList.Count;
end;

procedure TTextTable.Delete(index: Integer);
var
  p: PTextTableItem;
begin
  p := FList[index];
  Dispose(p);
  FList.Delete(index);
end;

function TTextTable.GetMaxIndexNo: Integer;
var
  i: Integer;
  p: PTextTableItem;
begin
  Result := 0;
  for i := 0 to FList.Count - 1 do
    begin
      p := PTextTableItem(FList[i]);
      if p^.index > Result then
          Result := p^.index;
    end;
end;

function TTextTable.GetOrigin(const s: SystemString): PTextTableItem;
var
  i: Integer;
  p: PTextTableItem;
begin
  Result := nil;
  for i := 0 to FList.Count - 1 do
    begin
      p := PTextTableItem(FList[i]);
      if (s = p^.OriginText) then
          Exit(p);
    end;
end;

procedure TTextTable.AddCopy(var t: TTextTableItem);
var
  p: PTextTableItem;
begin
  p := GetOrigin(t.OriginText);
  if p = nil then
    begin
      new(p);
      p^ := t;
      p^.RepCount := 1;
      FList.Add(p);
    end
  else
    begin
      p^.RepCount := p^.RepCount + 1;
    end;
end;

procedure TTextTable.AddText(AOriginText, ACategory: SystemString; APicked: Boolean);
var
  p: PTextTableItem;
begin
  p := GetOrigin(AOriginText);
  if p = nil then
    begin
      new(p);
      p^.OriginText := AOriginText;
      p^.Category := ACategory;
      p^.Picked := APicked;
      p^.index := GetMaxIndexNo + 1;
      p^.DefineText := AOriginText;
      p^.TextStyle := tsNormalText;
      p^.OriginHash := FastHashPSystemString(@AOriginText);
      p^.DefineHash := FastHashPSystemString(@p^.DefineText);
      p^.RepCount := 1;
      FList.Add(p);
    end
  else
    begin
      p^.RepCount := p^.RepCount + 1;
    end;
end;

procedure TTextTable.AddPascalText(AOriginText, ACategory: SystemString; APicked: Boolean);
var
  p: PTextTableItem;
begin
  p := GetOrigin(AOriginText);
  if p = nil then
    begin
      new(p);
      p^.OriginText := AOriginText;
      p^.Category := ACategory;
      p^.Picked := APicked;
      p^.index := GetMaxIndexNo + 1;
      p^.DefineText := AOriginText;
      p^.TextStyle := tsPascalText;
      p^.OriginHash := FastHashPSystemString(@AOriginText);
      p^.DefineHash := FastHashPSystemString(@p^.DefineText);
      p^.RepCount := 1;
      FList.Add(p);
    end
  else
    begin
      p^.RepCount := p^.RepCount + 1;
    end;
end;

procedure TTextTable.AddPascalComment(AOriginText, ACategory: SystemString; APicked: Boolean);
var
  p: PTextTableItem;
begin
  p := GetOrigin(AOriginText);
  if p = nil then
    begin
      new(p);
      p^.OriginText := AOriginText;
      p^.Category := ACategory;
      p^.Picked := APicked;
      p^.index := GetMaxIndexNo + 1;
      p^.DefineText := AOriginText;
      p^.TextStyle := tsPascalComment;
      p^.OriginHash := FastHashPSystemString(@AOriginText);
      p^.DefineHash := FastHashPSystemString(@p^.DefineText);
      p^.RepCount := 1;
      FList.Add(p);
    end
  else
    begin
      p^.RepCount := p^.RepCount + 1;
    end;
end;

procedure TTextTable.AddCText(AOriginText, ACategory: SystemString; APicked: Boolean);
var
  p: PTextTableItem;
begin
  p := GetOrigin(AOriginText);
  if p = nil then
    begin
      new(p);
      p^.OriginText := AOriginText;
      p^.Category := ACategory;
      p^.Picked := APicked;
      p^.index := GetMaxIndexNo + 1;
      p^.DefineText := AOriginText;
      p^.TextStyle := tsCText;
      p^.OriginHash := FastHashPSystemString(@AOriginText);
      p^.DefineHash := FastHashPSystemString(@p^.DefineText);
      p^.RepCount := 1;
      FList.Add(p);
    end
  else
    begin
      p^.RepCount := p^.RepCount + 1;
    end;
end;

procedure TTextTable.AddCComment(AOriginText, ACategory: SystemString; APicked: Boolean);
var
  p: PTextTableItem;
begin
  p := GetOrigin(AOriginText);
  if p = nil then
    begin
      new(p);
      p^.OriginText := AOriginText;
      p^.Category := ACategory;
      p^.Picked := APicked;
      p^.index := GetMaxIndexNo + 1;
      p^.DefineText := AOriginText;
      p^.TextStyle := tsCComment;
      p^.OriginHash := FastHashPSystemString(@AOriginText);
      p^.DefineHash := FastHashPSystemString(@p^.DefineText);
      p^.RepCount := 1;
      FList.Add(p);
    end
  else
    begin
      p^.RepCount := p^.RepCount + 1;
    end;
end;

procedure TTextTable.AddDelphiFormText(AOriginText, ACategory: SystemString; APicked: Boolean);
var
  p: PTextTableItem;
begin
  p := GetOrigin(AOriginText);
  if p = nil then
    begin
      new(p);
      p^.OriginText := AOriginText;
      p^.Category := ACategory;
      p^.Picked := APicked;
      p^.index := GetMaxIndexNo + 1;
      p^.DefineText := AOriginText;
      p^.TextStyle := tsDFMText;
      p^.OriginHash := FastHashPSystemString(@AOriginText);
      p^.DefineHash := FastHashPSystemString(@p^.DefineText);
      p^.RepCount := 1;
      FList.Add(p);
    end
  else
    begin
      p^.RepCount := p^.RepCount + 1;
    end;
end;

procedure TTextTable.ChangeDefineText(index: Integer; newDefine: U_String);
var
  i: Integer;
  p: PTextTableItem;
begin
  newDefine := umlCharReplace(newDefine, #9, #32).Text;

  while (newDefine.Len > 0) and (CharIn(newDefine.Last, [#13, #10])) do
      newDefine.DeleteLast;

  for i := 0 to FList.Count - 1 do
    begin
      p := FList[i];
      if (p^.Picked) and (p^.index = index) then
        begin
          case p^.TextStyle of
            tsPascalText: p^.DefineText := TTextParsing.TranslateTextToPascalDecl(newDefine);
            tsPascalComment: p^.DefineText := TTextParsing.TranslateTextToPascalDeclComment(newDefine);
            tsCText: p^.DefineText := TTextParsing.TranslateTextToC_Decl(newDefine);
            tsCComment: p^.DefineText := TTextParsing.TranslateTextToC_DeclComment(newDefine);
            tsDFMText: p^.DefineText := TTextParsing.TranslateTextToPascalDeclWithUnicode(newDefine);
            else p^.DefineText := newDefine;
          end;

          p^.DefineHash := FastHashPSystemString(@p^.DefineText);
        end;
    end;
end;

function TTextTable.ExistsIndex(index: Integer): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to FList.Count - 1 do
    if index = PTextTableItem(FList[i])^.index then
        Exit;
  Result := False;
end;

function TTextTable.Search(AOriginText: SystemString): PTextTableItem;
var
  hash: THash;
  i: Integer;
  p: PTextTableItem;
begin
  hash := FastHashPSystemString(@AOriginText);
  for i := 0 to FList.Count - 1 do
    begin
      p := FList[i];
      if (p^.OriginHash = hash) and (p^.OriginText = AOriginText) then
        begin
          Exit(p);
        end;
    end;
  Result := nil;
end;

procedure TTextTable.SaveToStream(stream: TCoreClassStream);
var
  ms: TMemoryStream64;
  df: TDataFrameEngine;
  i: Integer;
  p: PTextTableItem;
begin
  ms := TMemoryStream64.Create;
  df := TDataFrameEngine.Create;

  df.WriteInteger(FList.Count);

  for i := 0 to FList.Count - 1 do
    begin
      p := FList[i];
      p^.SaveToStream(ms);
      ms.Position := 0;
      df.WriteStream(ms);
      ms.Clear;
    end;

  df.EncodeAsBRRC(stream);

  DisposeObject(ms);
  DisposeObject(df);
end;

procedure TTextTable.LoadFromStream(stream: TCoreClassStream);
var
  ms: TMemoryStream64;
  df: TDataFrameEngine;
  i, c: Integer;
  p: PTextTableItem;
begin
  Clear;

  ms := TMemoryStream64.Create;
  df := TDataFrameEngine.Create;
  df.DecodeFrom(stream);

  c := df.Reader.ReadInteger;

  for i := 0 to c - 1 do
    begin
      new(p);
      df.Reader.ReadStream(ms);
      ms.Position := 0;
      p^.LoadFromStream(ms);
      ms.Clear;
      FList.Add(p);
    end;

  DisposeObject(ms);
  DisposeObject(df);
end;

procedure TTextTable.ExportToTextStream(stream: TCoreClassStream);
var
  expList: THashList;
  i: Integer;
  p: PTextTableItem;
  ns: TCoreClassStringList;
  n: TPascalString;
begin
  expList := THashList.Create;
  ns := TCoreClassStringList.Create;
  for i := 0 to Count - 1 do
    begin
      p := Items[i];
      if p^.Picked then
        if not expList.Exists(p^.OriginText) then
          begin
            expList.Add(p^.OriginText, p, False);
            case p^.TextStyle of
              tsPascalText: ns.Add(Format('%d=%s', [p^.index, TTextParsing.TranslatePascalDeclToText(p^.DefineText).Text]));
              tsCText: ns.Add(Format('%d=%s', [p^.index, TTextParsing.TranslateC_DeclToText(p^.DefineText).Text]));
              tsPascalComment: ns.Add(Format('%d=%s', [p^.index, TTextParsing.TranslatePascalDeclCommentToText(p^.DefineText).Text]));
              tsCComment: ns.Add(Format('%d=%s', [p^.index, TTextParsing.TranslateC_DeclCommentToText(p^.DefineText).Text]));
              tsDFMText: ns.Add(Format('%d=%s', [p^.index, TTextParsing.TranslatePascalDeclToText(p^.DefineText).Text]));
              else ns.Add(Format('%d=%s', [p^.index, p^.DefineText]));
            end;
          end;
    end;
  ns.SaveToStream(stream);
  DisposeObject(expList);
  DisposeObject(ns);
end;

procedure TTextTable.ImportFromTextStream(stream: TCoreClassStream);
var
  ns: TCoreClassStringList;
  t: TTextParsing;
  CurrentItem: Integer;
  cp: Integer;
  nbPos, nePos: Integer;
  numText: U_String;
  Num: Integer;
  n: U_String;
begin
  ns := TCoreClassStringList.Create;
  ns.LoadFromStream(stream);
  t := TTextParsing.Create(ns.Text, TTextStyle.tsText, nil);

  cp := 1;
  n := '';
  Num := -1;
  CurrentItem := -1;
  while cp <= t.Len do
    begin
      if ((cp = 1) or (CharIn(t.GetChar(cp - 1), ns.LineBreak))) and (t.isNumber(cp)) then
        begin
          nbPos := cp;
          nePos := t.GetNumberEndPos(nbPos);
          numText := t.GetStr(nbPos, nePos);
          if CharIn(t.GetChar(nePos), ':=') then
            case umlGetNumTextType(numText) of
              ntUInt64, ntWord, ntByte, ntUInt:
                begin
                  Num := umlStrToInt(numText.Text, 0);
                  if n.Len >= length(ns.LineBreak) then
                      n.Len := n.Len - length(ns.LineBreak);
                  ChangeDefineText(CurrentItem, n);
                  n := '';
                  CurrentItem := Num;
                  cp := nePos + 1;
                  Continue;
                end;
            end;
        end;
      n := n + t.GetChar(cp);
      inc(cp);
    end;
  if n.Len >= length(ns.LineBreak) then
      n.Len := n.Len - length(ns.LineBreak);
  ChangeDefineText(CurrentItem, n.Text);

  DisposeObject(ns);
  DisposeObject(t);
end;

end.
