{ ***************************************************************************** }
{ * Transform TextTable support,writen by QQ 600585@qq.com                    * }
{ * https://github.com/PassByYou888/CoreCipher                                 * }
{ * https://github.com/PassByYou888/ZServer4D                                  * }
{ * https://github.com/PassByYou888/zExpression                                * }
{ ****************************************************************************** }

unit TextTable;

{$I zDefine.inc}

interface

uses SysUtils, CoreClasses, DataFrameEngine, ListEngine, UnicodeMixedLib,
  MemoryStream64, TextParsing, PascalStrings;

type
  TTranlateStyle = (tsPascalText, tsPascalComment, tsCText, tsCComment, tsNormalText);

  TTextTableItem = record
    // origin info
    OriginText: SystemString;
    Category: SystemString;

    // ext pick info
    Picked: Boolean;

    // encode and import info
    Index: Integer;
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

    function GetItems(Index: Integer): PTextTableItem;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    function Count: Integer;
    property Items[index: Integer]: PTextTableItem read GetItems; default;
    procedure Delete(Index: Integer);

    function GetMaxIndexNo: Integer;

    function GetOrigin(const s: SystemString): PTextTableItem;
    property Origin[const s: SystemString]: PTextTableItem read GetOrigin;

    procedure AddCopy(var t: TTextTableItem);
    procedure AddText(AOriginText, ACategory: SystemString; APicked: Boolean);
    procedure AddPascalText(AOriginText, ACategory: SystemString; APicked: Boolean);
    procedure AddPascalComment(AOriginText, ACategory: SystemString; APicked: Boolean);
    procedure AddCText(AOriginText, ACategory: SystemString; APicked: Boolean);
    procedure AddCComment(AOriginText, ACategory: SystemString; APicked: Boolean);
    procedure ChangeDefineText(Index: Integer; newDefine: umlString);

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

function TTextTable.GetItems(Index: Integer): PTextTableItem;
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

procedure TTextTable.Delete(Index: Integer);
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
      if p^.Index > Result then
          Result := p^.Index;
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
      New(p);
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
      New(p);
      p^.OriginText := AOriginText;
      p^.Category := ACategory;
      p^.Picked := APicked;
      p^.Index := GetMaxIndexNo + 1;
      p^.DefineText := AOriginText;
      p^.TextStyle := tsNormalText;
      p^.OriginHash := FastHashSystemString(@AOriginText);
      p^.DefineHash := FastHashSystemString(@p^.DefineText);
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
      New(p);
      p^.OriginText := AOriginText;
      p^.Category := ACategory;
      p^.Picked := APicked;
      p^.Index := GetMaxIndexNo + 1;
      p^.DefineText := AOriginText;
      p^.TextStyle := tsPascalText;
      p^.OriginHash := FastHashSystemString(@AOriginText);
      p^.DefineHash := FastHashSystemString(@p^.DefineText);
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
      New(p);
      p^.OriginText := AOriginText;
      p^.Category := ACategory;
      p^.Picked := APicked;
      p^.Index := GetMaxIndexNo + 1;
      p^.DefineText := AOriginText;
      p^.TextStyle := tsPascalComment;
      p^.OriginHash := FastHashSystemString(@AOriginText);
      p^.DefineHash := FastHashSystemString(@p^.DefineText);
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
      New(p);
      p^.OriginText := AOriginText;
      p^.Category := ACategory;
      p^.Picked := APicked;
      p^.Index := GetMaxIndexNo + 1;
      p^.DefineText := AOriginText;
      p^.TextStyle := tsCText;
      p^.OriginHash := FastHashSystemString(@AOriginText);
      p^.DefineHash := FastHashSystemString(@p^.DefineText);
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
      New(p);
      p^.OriginText := AOriginText;
      p^.Category := ACategory;
      p^.Picked := APicked;
      p^.Index := GetMaxIndexNo + 1;
      p^.DefineText := AOriginText;
      p^.TextStyle := tsCComment;
      p^.OriginHash := FastHashSystemString(@AOriginText);
      p^.DefineHash := FastHashSystemString(@p^.DefineText);
      p^.RepCount := 1;
      FList.Add(p);
    end
  else
    begin
      p^.RepCount := p^.RepCount + 1;
    end;
end;

procedure TTextTable.ChangeDefineText(Index: Integer; newDefine: umlString);
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
      if (p^.Picked) and (p^.Index = index) then
        begin
          if p^.TextStyle = tsPascalText then
              p^.DefineText := TTextParsing.TranslateTextToPascalDecl(newDefine).Text
          else if p^.TextStyle = tsCText then
              p^.DefineText := TTextParsing.TranslateTextToC_Decl(newDefine).Text
          else if p^.TextStyle = tsPascalComment then
            begin
              if umlMultipleMatch(False, '{*}', umlTrimSpace(newDefine)) then
                  p^.DefineText := newDefine
              else
                  p^.DefineText := '{ ' + (newDefine) + ' }';
            end
          else if p^.TextStyle = tsCComment then
            begin
              if umlMultipleMatch(False, '#*', umlTrimSpace(newDefine)) then
                  p^.DefineText := newDefine
              else
                  p^.DefineText := '/* ' + (newDefine) + ' */';
            end
          else
              p^.DefineText := newDefine;

          p^.DefineHash := FastHashSystemString(@p^.DefineText);
        end;
    end;
end;

function TTextTable.Search(AOriginText: SystemString): PTextTableItem;
var
  hash: THash;
  i   : Integer;
  p   : PTextTableItem;
begin
  hash := FastHashSystemString(@AOriginText);
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
  i : Integer;
  p : PTextTableItem;
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
  ms  : TMemoryStream64;
  df  : TDataFrameEngine;
  i, c: Integer;
  p   : PTextTableItem;
begin
  Clear;

  ms := TMemoryStream64.Create;
  df := TDataFrameEngine.Create;
  df.DecodeFrom(stream);

  c := df.Reader.ReadInteger;

  for i := 0 to c - 1 do
    begin
      New(p);
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
  i      : Integer;
  p      : PTextTableItem;
  ns     : TCoreClassStringList;
  n      : TPascalString;
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
            if p^.TextStyle = tsPascalText then
                ns.Add(Format('%d=%s', [p^.Index, TTextParsing.TranslatePascalDeclToText(p^.DefineText).Text]))
            else if p^.TextStyle = tsCText then
                ns.Add(Format('%d=%s', [p^.Index, TTextParsing.TranslateC_DeclToText(p^.DefineText).Text]))
            else if p^.TextStyle = tsPascalComment then
              begin
                n := umlTrimSpace(p^.DefineText);
                if umlMultipleMatch(False, '{*}', n) then
                  begin
                    n.DeleteFirst;
                    n.DeleteLast;
                    if umlMultipleMatch(False, '$*', umlTrimSpace(n)) then
                        n := p^.OriginText;
                  end
                else if umlMultipleMatch(False, '(*?*)', n, '?', '') then
                  begin
                    n.DeleteFirst;
                    n.DeleteFirst;
                    n.DeleteLast;
                    n.DeleteLast;
                  end
                else if umlMultipleMatch(False, '////*', n) then
                  begin
                    n.DeleteFirst;
                    n.DeleteFirst;
                    n.DeleteFirst;
                    n.DeleteFirst;
                  end
                else if umlMultipleMatch(False, '///*', n) then
                  begin
                    n.DeleteFirst;
                    n.DeleteFirst;
                    n.DeleteFirst;
                  end
                else if umlMultipleMatch(False, '//*', n) then
                  begin
                    n.DeleteFirst;
                    n.DeleteFirst;
                  end;
                ns.Add(Format('%d=%s', [p^.Index, n.Text]));
              end
            else if p^.TextStyle = tsCComment then
              begin
                n := umlTrimSpace(p^.DefineText);
                if umlMultipleMatch(False, '#*', n) then
                  begin
                    n := p^.OriginText;
                  end
                else if umlMultipleMatch(False, '/*?*/', n, '?', '') then
                  begin
                    n.DeleteFirst;
                    n.DeleteFirst;
                    n.DeleteLast;
                    n.DeleteLast;
                  end
                else if umlMultipleMatch(False, '////*', n) then
                  begin
                    n.DeleteFirst;
                    n.DeleteFirst;
                    n.DeleteFirst;
                    n.DeleteFirst;
                  end
                else if umlMultipleMatch(False, '///*', n) then
                  begin
                    n.DeleteFirst;
                    n.DeleteFirst;
                    n.DeleteFirst;
                  end
                else if umlMultipleMatch(False, '//*', n) then
                  begin
                    n.DeleteFirst;
                    n.DeleteFirst;
                  end;
                ns.Add(Format('%d=%s', [p^.Index, p^.DefineText]));
              end
            else
                ns.Add(Format('%d=%s', [p^.Index, p^.DefineText]));
          end;
    end;
  ns.SaveToStream(stream);
  DisposeObject(expList);
  DisposeObject(ns);
end;

procedure TTextTable.ImportFromTextStream(stream: TCoreClassStream);
var
  ns          : TCoreClassStringList;
  t           : TTextParsing;
  CurrentItem : Integer;
  cp          : Integer;
  nbPos, nePos: Integer;
  numText     : umlString;
  num         : Integer;
  n           : umlString;
begin
  ns := TCoreClassStringList.Create;
  ns.LoadFromStream(stream);
  t := TTextParsing.Create(ns.Text, TTextStyle.tsText);

  cp := 1;
  n := '';
  num := -1;
  CurrentItem := -1;
  while cp <= t.Len do
    begin
      if ((cp = 1) or (CharIn(t.GetChar(cp - 1), ns.LineBreak))) and (t.IsNumber(cp)) then
        begin
          nbPos := cp;
          nePos := t.GetNumberEndPos(nbPos);
          numText := t.GetStr(nbPos, nePos);
          if CharIn(t.GetChar(nePos), ':=') then
            case umlGetNumTextType(numText) of
              ntUInt64, ntWord, ntByte, ntUInt:
                begin
                  num := StrToInt(numText.Text);
                  if n.Len >= Length(ns.LineBreak) then
                      n.Len := n.Len - Length(ns.LineBreak);
                  ChangeDefineText(CurrentItem, n.Text);
                  n := '';
                  CurrentItem := num;
                  cp := nePos + 1;
                  continue;
                end;
            end;
        end;
      n := n + t.GetChar(cp);
      Inc(cp);
    end;
  if n.Len >= Length(ns.LineBreak) then
      n.Len := n.Len - Length(ns.LineBreak);
  ChangeDefineText(CurrentItem, n.Text);

  DisposeObject(ns);
  DisposeObject(t);
end;

end.
