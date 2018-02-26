unit SortFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.StdCtrls, FMX.Layouts, FMX.Controls.Presentation, FMX.ScrollBox,
  FMX.Memo;

type
  TSortForm = class(TForm)
    Memo1: TMemo;
    Layout1: TLayout;
    SortButton: TButton;
    procedure SortButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SortForm: TSortForm;

implementation

{$R *.fmx}


uses PascalStrings, UnicodeMixedLib, CoreClasses, TextParsing;

procedure SortDPRUsesStrings(Code: TCoreClassStrings);
type
  TData = record
    Sour: umlString;
    SortKey: umlString;
  end;

  PData = ^TData;

  TValueRelationship = -1 .. 1;

  function ListSortCompare(Item1, Item2: Pointer): Integer; inline;
  const
    LessThanValue = low(TValueRelationship);
    EqualsValue = 0;
    GreaterThanValue = high(TValueRelationship);
  var
    p1, p2: PData;
    ph1, ph2: string;
  begin
    p1 := Item1;
    p2 := Item2;
    ph1 := p2^.SortKey;
    ph2 := p1^.SortKey;

    if ph1 = ph2 then
      begin
        Result := EqualsValue;
      end
    else if ph1 > ph2 then
        Result := LessThanValue
    else
        Result := GreaterThanValue;
  end;

  procedure QuickSortList(var SortList: TCoreClassPointerList; l, r: Integer);
  var
    i, j: Integer;
    p, t: Pointer;
  begin
    repeat
      i := l;
      j := r;
      p := SortList[(l + r) shr 1];
      repeat
        while ListSortCompare(SortList[i], p) < 0 do
            Inc(i);
        while ListSortCompare(SortList[j], p) > 0 do
            Dec(j);
        if i <= j then
          begin
            if i <> j then
              begin
                t := SortList[i];
                SortList[i] := SortList[j];
                SortList[j] := t;
              end;
            Inc(i);
            Dec(j);
          end;
      until i > j;
      if l < j then
          QuickSortList(SortList, l, j);
      l := i;
    until i >= r;
  end;

var
  i, j: Integer;
  p: PData;
  l: TCoreClassList;
  n: umlString;
  t: TTextParsing;
  LastKey: umlString;
  preProcessHeader: umlString;
  token: TTokenData;
begin
  preProcessHeader := '';
  l := TCoreClassList.Create;
  for i := 0 to Code.Count - 1 do
    begin
      n := umlTrimSpace(Code[i]);
      if (n.Len > 0) then
        begin
          t := TTextParsing.Create(n, tsPascal, nil);
          if (t.TokenStatistics[ttTextDecl] = 1) then
            begin
              new(p);
              p^.Sour := n;
              if p^.Sour.Last <> ',' then
                begin
                  if p^.Sour.Last = ';' then
                      p^.Sour.Last := ','
                  else
                      p^.Sour.Append(',');
                end;

              token := t.TokenIndex[ttTextDecl, 0]^;
              p^.SortKey := t.GetTextBody(t.GetStr(token.bPos, token.ePos));

              if umlExistsLimitChar(p^.SortKey, '\') then
                  p^.SortKey := umlDeleteLastStr(p^.SortKey, '\')
              else
                  p^.SortKey := '';

              l.Add(p);
            end
          else
            begin
              if t.TokenStatistics[ttAscii] > 0 then
                  preProcessHeader := preProcessHeader + '  ' + n + #13#10;
            end;
          DisposeObject(t);
        end;
    end;

  QuickSortList(l.ListData^, 0, l.Count - 1);

  Code.Clear;
  Code.BeginUpdate;
  LastKey := '';
  if preProcessHeader.Len > 0 then
    begin
      Code.Add('  (* system unit *)');
      Code.Add(preProcessHeader.Text);
    end;
  Code.Add('  (* project root unit *)');
  for i := 0 to l.Count - 1 do
    begin
      p := l[i];
      n := p^.SortKey;
      if not LastKey.Same(n) then
        begin
          Code.Add(Format('', []));
          Code.Add(Format('  (* %s *)', [n.Text]));
          LastKey := n;
        end;

      Code.Add('  ' + p^.Sour);
      Dispose(p);
    end;
  Code.EndUpdate;

  l.Free;
end;

function ParseAndSortDPRSource(sourceCode: umlString): umlString;
var
  SourceOutput, UsesOutput: umlString;
  InitedUnit, InitedUses: Boolean;

  procedure AppendCode(s: umlString);
  begin
    if InitedUses then
        UsesOutput.Append(s)
    else
        SourceOutput.Append(s);
  end;

  procedure BeginParseUses;
  begin
  end;

  procedure EndParseUses;
  var
    ns: TCoreClassStringList;
  begin
    ns := TCoreClassStringList.Create;
    ns.Text := UsesOutput.Text;
    SortDPRUsesStrings(ns);
    UsesOutput.Text := ns.Text;
    DisposeObject(ns);
    UsesOutput := umlDeleteLastStr(UsesOutput, ',');
    SourceOutput.Text := SourceOutput.Text + #13#10 + UsesOutput.Text;
  end;

var
  t: TTextParsing;
  cp: Integer;
  ePos: Integer;
  s: umlString;
begin
  SourceOutput := '';
  t := TTextParsing.Create(sourceCode, tsPascal, nil);

  InitedUnit := False;
  InitedUses := False;

  cp := 1;

  while cp <= t.ParsingData.Len do
    begin
      if t.IsTextDecl(cp) then
        begin
          ePos := t.GetTextDeclEndPos(cp);
          s := t.GetStr(cp, ePos);
          AppendCode(s);
          cp := ePos
        end
      else if t.IsComment(cp) then
        begin
          ePos := t.GetCommentEndPos(cp);
          s := t.GetStr(cp, ePos);
          AppendCode(s);
          cp := ePos;
        end
      else if t.IsNumber(cp) then
        begin
          ePos := t.GetNumberEndPos(cp);
          s := t.GetStr(cp, ePos);
          AppendCode(s);
          cp := ePos;
        end
      else if t.IsSymbol(cp) then
        begin
          ePos := t.GetSymbolEndPos(cp);
          if (InitedUnit) and (InitedUses) then
            if t[cp] = ';' then
              begin
                InitedUses := False;
                EndParseUses;
              end;

          s := t.GetStr(cp, ePos);
          AppendCode(s);
          cp := ePos;
        end
      else if t.IsAscii(cp) then
        begin
          ePos := t.GetAsciiEndPos(cp);
          s := t.GetStr(cp, ePos);
          AppendCode(s);
          cp := ePos;

          if not InitedUnit then
            begin
              if (s.Same('library')) or (s.Same('program')) then
                  InitedUnit := True;
            end;

          if (InitedUnit) and (not InitedUses) then
            begin
              if s.Same('uses') then
                begin
                  InitedUses := True;
                  BeginParseUses;
                end;
            end;
        end
      else
        begin
          AppendCode(t[cp]);
          Inc(cp);
        end;
    end;

  DisposeObject(t);
  Result := SourceOutput;
end;

procedure TSortForm.SortButtonClick(Sender: TObject);
begin
  Memo1.Text := ParseAndSortDPRSource(Memo1.Text);
end;

end.
