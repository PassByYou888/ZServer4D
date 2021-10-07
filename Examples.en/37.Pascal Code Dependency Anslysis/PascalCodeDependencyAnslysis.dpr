program PascalCodeDependencyAnslysis;

{$APPTYPE CONSOLE}

{$R *.res}


uses SysUtils,
{$IFDEF FPC}
  FPCGenericStructlist, {  This library is required for FPC compiler to support generics  }
{$ENDIF FPC}
  CoreClasses, PascalStrings, TextParsing, UnicodeMixedLib, DoStatusIO, ListEngine;

type
  TPascalUnit = class;
  PProbeRec = ^TProbeRec;

  TProbeRec = record
    Body: U_String;
    bPos, ePos: Integer;
    Index: Integer;
    procedure Init;
  end;

{$IFDEF FPC}

  {  The FPC compiler needs to use the specify modifier before declaring a generic type  }
  TProbeList = specialize TGenericsList<PProbeRec>;
  TPascalUnitList_Decl = specialize TGenericsList<TPascalUnit>;
{$ELSE FPC}
  {  Use Delphi's generic library and directly declare it with tgenericslist  }
  TProbeList = TGenericsList<PProbeRec>;
  TPascalUnitList_Decl = TGenericsList<TPascalUnit>;
{$ENDIF FPC}

  {  Parse Pascal structure, support all FMX, VCL, runtime, LCL, FPC basic library + advanced library, and support parsing Delphi advanced syntax  }
  {  Tpascalunit toolchain code from zexpression  }
  TPascalUnit = class(TCoreClassObject)
  public
    Parsing: TTextParsing;
    ProbeList: TProbeList;
    Intf_UsesList: TPascalStringList;
    IncludeList: TPascalStringList;
    Unit_Name: U_String;
    Unit_Token, Intf_Token, Imp_Token, Unit_End_Token, Init_Token, Final_Token: PProbeRec;
    Solve: Boolean;

    constructor CreateFromString(AText: U_String);
    constructor CreateFromFile(fn: U_String);
    destructor Destroy; override;
    procedure Fill;
    procedure Clear;
    function Combine(const bTokenI, eTokenI: Integer): U_String;
  end;

  TPascalUnitList = class(TPascalUnitList_Decl)
  public
    destructor Destroy; override;
    procedure AddPascalCode(code: U_String); overload;
    procedure AddPascalCode(code: TCoreClassStrings); overload;
    procedure AddPascalCodeFile(pasCodeFile: U_String);
    procedure DependencySort;

    {  Members need to be released in these three places. Notify is not used. It is a generic library compatible with FPC: the fpcgenericstructlist.pas library does not provide notify callback  }
    procedure remove(p: TPascalUnit);
    procedure Delete(Index: Integer);
    procedure Clear;
  end;

{$REGION 'PascalUnit Imp'}


procedure TProbeRec.Init;
begin
  Body := '';
  bPos := -1;
  ePos := -1;
  index := 0;
end;

constructor TPascalUnit.CreateFromString(AText: U_String);
var
  i: Integer;
begin
  inherited Create;
  Parsing := TTextParsing.Create(AText, tsPascal);

  ProbeList := TProbeList.Create;
  Intf_UsesList := TPascalStringList.Create;
  IncludeList := TPascalStringList.Create;
  Unit_Name := '';
  Unit_Token := nil;
  Intf_Token := nil;
  Imp_Token := nil;
  Unit_End_Token := nil;
  Init_Token := nil;
  Final_Token := nil;

  Solve := False;
  Fill;
end;

constructor TPascalUnit.CreateFromFile(fn: U_String);
var
  ns: TPascalStringList;
begin
  ns := TPascalStringList.Create;
  ns.LoadFromFile(fn);
  CreateFromString(ns.AsText);
  disposeObject(ns);
end;

destructor TPascalUnit.Destroy;
begin
  Clear;
  disposeObject(ProbeList);
  disposeObject(Intf_UsesList);
  disposeObject(IncludeList);
  inherited Destroy;
end;

procedure TPascalUnit.Fill;

  procedure FillUses(t: U_String);
  var
    np: TTextParsing;
    ns: TPascalStringList;
    n: U_String;
    i: Integer;
  begin
    np := TTextParsing.Create(t, tsPascal);
    np.DeletedComment;
    n := np.ParsingData.Text.DeleteChar(#13#10#9);
    disposeObject(np);

    ns := TPascalStringList.Create;
    umlSeparatorText(n, ns, ',');
    i := 0;
    while i < ns.Count do
      begin
        ns[i] := ns[i].TrimChar(#32);
        if ns[i].Len = 0 then
            ns.Delete(i)
        else
            inc(i);
      end;
    umlMergeStrings(ns, Intf_UsesList, True);
    disposeObject(ns);
  end;

type
  TCurrentSection = (csBeginUnit, csEndUnit, csIntf, csIntfUses, csImp);
  TCurrentSections = set of TCurrentSection;

var
  i: Integer;
  p1, p2, p3: PTokenData;
  p: PProbeRec;
  n: U_String;

  cS: TCurrentSections;
  Indent: Integer;
begin
  i := 0;
  cS := [];
  Indent := 0;

  while i < Parsing.TokenCount do
    begin
      p1 := Parsing.Tokens[i];
      new(p);
      p^.Init;
      p^.Body := p1^.Text;
      p^.bPos := p1^.bPos;
      p^.ePos := p1^.ePos;

      if (p1^.tokenType = ttComment) then
        begin
          if umlMultipleMatch(['{$I *', '{$Include *'], p1^.Text) then
            begin
              n := umlDeleteFirstStr(p1^.Text, ' ');
              if n.Last = '}' then
                  n.DeleteLast;
              IncludeList.Add(n);
            end;
        end;

      if (not(csEndUnit in cS)) and (csIntf in cS) and (not(csImp in cS)) and (Indent > 0) then
        begin
          if (p1^.tokenType = ttAscii) then
            begin
              if p1^.Text.Same('class') then
                begin
                  p2 := Parsing.TokenProbeL(p1^.Index - 1, [ttTextDecl, ttNumber, ttSymbol, ttAscii]);
                  if (p2 <> nil) and (p2^.Text.Same('=')) then
                    begin
                      p3 := Parsing.TokenProbeR(p1^.Index + 1, [ttTextDecl, ttNumber, ttSymbol, ttAscii]);
                      if not p3^.Text.Same('(', ';', 'of') then
                          inc(Indent)
                      else if p3^.Text.Same('(') then
                        begin
                          p3 := Parsing.TokenProbeR(Parsing.TokenProbeR(p3^.Index + 1, ')')^.Index + 1, [ttTextDecl, ttNumber, ttSymbol, ttAscii]);
                          if not p3^.Text.Same('(', ';') then
                              inc(Indent);
                        end;
                    end;
                end
              else if p1^.Text.Same('interface') then
                begin
                  p2 := Parsing.TokenProbeL(p1^.Index - 1, [ttTextDecl, ttNumber, ttSymbol, ttAscii]);
                  if (p2 <> nil) and (p2^.Text.Same('=')) then
                    begin
                      p3 := Parsing.TokenProbeR(p1^.Index + 1, [ttTextDecl, ttNumber, ttSymbol, ttAscii]);
                      if not p3^.Text.Same('(', ';') then
                          inc(Indent)
                      else if p3^.Text.Same('(') then
                        begin
                          p3 := Parsing.TokenProbeR(Parsing.TokenProbeR(p3^.Index + 1, ')')^.Index + 1, [ttTextDecl, ttNumber, ttSymbol, ttAscii]);
                          if not p3^.Text.Same('(', ';') then
                              inc(Indent);
                        end;
                    end;
                end
              else if p1^.Text.Same('record') then
                  inc(Indent)
              else if (p1^.Text.Same('end')) then
                begin
                  if Parsing.ComparePosStr(p1^.bPos, 'end;') or Parsing.ComparePosStr(p1^.bPos, 'end ') then
                      dec(Indent);
                end;
            end;
        end
      else if (not(csEndUnit in cS)) and (Indent = 0) and (p1^.tokenType = ttAscii) then
        begin
          if (not(csBeginUnit in cS)) and (p1^.Text.Same('unit')) then
            begin
              cS := [csBeginUnit];
              Unit_Token := p;
              p2 := Parsing.TokenProbeR(p1^.Index + 1, [ttSymbol], ';');
              if p2 = nil then
                  RaiseInfo('Illegal unit decl', []);

              Unit_Name := Parsing.TokenCombine(p1^.Index + 1, p2^.Index - 1).TrimChar(#32);
            end;

          if (csBeginUnit in cS) then
            begin
              if (not(csIntf in cS)) and (p1^.Text.Same('interface')) then
                begin
                  cS := cS + [csIntf];
                  Intf_Token := p;
                end
              else if (not(csImp in cS)) and (p1^.Text.Same('implementation')) then
                begin
                  cS := cS + [csImp];
                  Imp_Token := p;
                end
              else if (csImp in cS) and (not(csEndUnit in cS)) and (p1^.tokenType = ttAscii) then
                begin
                  if p1^.Text.Same('initialization') then
                      Init_Token := p
                  else if p1^.Text.Same('finalization') then
                      Final_Token := p
                  else if (Parsing.ComparePosStr(p1^.bPos, 'end.')) then
                    begin
                      cS := cS + [csEndUnit];
                      p^.Body := 'end.';
                      inc(i);
                      Unit_End_Token := p;
                    end;
                end
              else if (csIntf in cS) and (not(csImp in cS)) then
                begin
                  if (not(csIntfUses in cS)) and (p1^.Text.Same('uses')) then
                    begin
                      p2 := Parsing.TokenProbeR(p1^.Index + 1, [ttSymbol], ';');
                      if p2 = nil then
                          RaiseInfo('Illegal uses decl');
                      cS := cS + [csIntfUses];
                      FillUses(Parsing.TokenCombine(p1^.Index + 1, p2^.Index - 1));
                      i := p2^.Index + 1;
                      p^.Body := '';
                    end;
                  cS := cS + [csIntfUses];

                  if p1^.Text.Same('class') then
                    begin
                      p2 := Parsing.TokenProbeL(p1^.Index - 1, [ttTextDecl, ttNumber, ttSymbol, ttAscii]);
                      if (p2 <> nil) and (p2^.Text.Same('=')) then
                        begin
                          p3 := Parsing.TokenProbeR(p1^.Index + 1, [ttTextDecl, ttNumber, ttSymbol, ttAscii]);
                          if not p3^.Text.Same('(', ';', 'of') then
                            begin
                              inc(Indent);
                            end
                          else if p3^.Text.Same('(') then
                            begin
                              p3 := Parsing.TokenProbeR(Parsing.TokenProbeR(p3^.Index + 1, ')')^.Index + 1, [ttTextDecl, ttNumber, ttSymbol, ttAscii]);
                              if not p3^.Text.Same('(', ';') then
                                  inc(Indent);
                            end;
                        end;
                    end
                  else if p1^.Text.Same('interface') then
                    begin
                      p2 := Parsing.TokenProbeL(p1^.Index - 1, [ttTextDecl, ttNumber, ttSymbol, ttAscii]);
                      if (p2 <> nil) and (p2^.Text.Same('=')) then
                        begin
                          p3 := Parsing.TokenProbeR(p1^.Index + 1, [ttTextDecl, ttNumber, ttSymbol, ttAscii]);
                          if not p3^.Text.Same('(', ';') then
                              inc(Indent)
                          else if p3^.Text.Same('(') then
                            begin
                              p3 := Parsing.TokenProbeR(Parsing.TokenProbeR(p3^.Index + 1, ')')^.Index + 1, [ttTextDecl, ttNumber, ttSymbol, ttAscii]);
                              if not p3^.Text.Same('(', ';') then
                                  inc(Indent)
                            end;
                        end;
                    end
                  else if p1^.Text.Same('record') then
                    begin
                      inc(Indent);
                    end;
                end;
            end;
        end;
      p^.Index := ProbeList.Count;
      ProbeList.Add(p);
      inc(i);
    end;

  if Unit_Name.Len = 0 then
      DoStatus('not unit name');
  if Unit_Token = nil then
      DoStatus('not unit decl');
  if Intf_Token = nil then
      DoStatus('not interface decl');
  if Imp_Token = nil then
      DoStatus('not implementation decl');
  if Unit_End_Token = nil then
      DoStatus('not end. decl');
  Solve := (Unit_Name.Len > 0) and (Unit_Token <> nil) and (Intf_Token <> nil) and (Imp_Token <> nil) and (Unit_End_Token <> nil);
  if not Solve then
      RaiseInfo('failed: %s ', [Unit_Name.Text]);
end;

procedure TPascalUnit.Clear;
var
  i: Integer;
begin
  for i := ProbeList.Count - 1 downto 0 do
    begin
      PProbeRec(ProbeList[i])^.Init;
      Dispose(ProbeList[i]);
    end;
  ProbeList.Clear;
  Intf_UsesList.Clear;
  IncludeList.Clear;
end;

function TPascalUnit.Combine(const bTokenI, eTokenI: Integer): U_String;
var
  bi, ei: Integer;
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
      Result.Append(PProbeRec(ProbeList[bi])^.Body);
      inc(bi);
    end;
end;

destructor TPascalUnitList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TPascalUnitList.AddPascalCode(code: U_String);
begin
  Add(TPascalUnit.CreateFromString(code));
end;

procedure TPascalUnitList.AddPascalCode(code: TCoreClassStrings);
begin
  Add(TPascalUnit.CreateFromString(code.Text));
end;

procedure TPascalUnitList.AddPascalCodeFile(pasCodeFile: U_String);
begin
  Add(TPascalUnit.CreateFromFile(pasCodeFile));
end;

procedure TPascalUnitList.DependencySort;
  function UnitNeedExchange(const left, right: Integer; var conflictError: Boolean): Boolean;
  var
    left_t, right_t: TPascalUnit;
  begin
    left_t := Items[left];
    right_t := Items[right];

    conflictError := umlTextInStrings(left_t.Unit_Name, right_t.Intf_UsesList, True)
      and umlTextInStrings(right_t.Unit_Name, left_t.Intf_UsesList, True);

    if umlTextInStrings(left_t.Unit_Name, right_t.Intf_UsesList, True) then
        Result := left > right
    else if umlTextInStrings(right_t.Unit_Name, left_t.Intf_UsesList, True) then
        Result := right > left
    else
        Result := False;
  end;

var
  conflictError: Boolean;
  resort: Boolean;
  j, i: Integer;
begin
  j := 0;
  repeat
    i := 0;
    resort := False;
    while i < Count - 1 do
      begin
        conflictError := False;
        if (i <> j) and (UnitNeedExchange(i, j, conflictError)) then
          begin
            Exchange(i, j);
            i := 0;
            resort := True;
          end
        else
            inc(i);
        if conflictError then
            DoStatus('warning: uses conflict [%s] - [%s]', [Items[i].Unit_Name.Text, Items[j].Unit_Name.Text]);
      end;
    inc(j);
    if j >= Count then
        j := Count - 1
    else
        resort := True;
  until not resort;
end;

procedure TPascalUnitList.remove(p: TPascalUnit);
begin
  disposeObject(p);
  inherited remove(p);
end;

procedure TPascalUnitList.Delete(Index: Integer);
begin
  disposeObject(Items[index]);
  inherited Delete(index);
end;

procedure TPascalUnitList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
      disposeObject(Items[i]);
  inherited Clear;
end;

{$ENDREGION 'PascalUnit Imp'}


procedure demo;
var
  pl: TPascalUnitList;
  i: Integer;
begin
  {  Open source projects produced using this technology https://github.com/PassByYou888/FFMPEG-Header  }
  pl := TPascalUnitList.Create;

  {  PL can directly add code files on a large scale, and then make dependencies and automatic code base layering  }
  {  We can even use this technology to make code distribution tools  }

  pl.AddPascalCode(
    {  Code generated using tools  }
    'unit u1;'#13#10 +
    'interface'#13#10 +
    'uses u2,u3;'#13#10 +
    'implementation'#13#10 +
    '{$I test.inc}' + #13#10 +
    'end.'#13#10
    );
  pl.AddPascalCode(
    {  Code generated using tools  }
    'unit u2;'#13#10 +
    'interface'#13#10 +
    'uses u3;'#13#10 +
    'implementation'#13#10 +
    'end.'#13#10
    );
  pl.AddPascalCode(
    {  Code generated using tools  }
    'unit u3;'#13#10 +
    'interface'#13#10 +
    'uses u4;'#13#10 +
    'implementation'#13#10 +
    'end.'#13#10
    );
  pl.AddPascalCode(
    {  Code generated using tools  }
    'unit u4;'#13#10 +
    'interface'#13#10 +
    'uses SysUtils;'#13#10 +
    'implementation'#13#10 +
    'end.'#13#10
    );

  DoStatus('Before dependencies are sorted');
  for i := 0 to pl.Count - 1 do
      DoStatus(pl[i].Unit_Name);

  pl.DependencySort;

  DoStatus('After dependency sorting');
  for i := 0 to pl.Count - 1 do
      DoStatus(pl[i].Unit_Name);

  disposeObject(pl);
end;

begin
  demo;
  readln;

end.
