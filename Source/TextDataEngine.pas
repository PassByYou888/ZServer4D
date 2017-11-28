{*****************************************************************************}
{* section text library,writen by QQ 600585@qq.com                           *}
{* https://github.com/PassByYou888/CoreCipher                                *}
(* https://github.com/PassByYou888/ZServer4D                                 *)
{*****************************************************************************}

unit TextDataEngine;

{$I zDefine.inc}

interface

uses SysUtils, Variants,
  // Hash
  ListEngine, CoreClasses, MemoryStream64, PascalStrings;

type
  TSectionTextData = class;

  THashTextEngine = TSectionTextData;

  TSectionTextData = class(TCoreClassObject)
  private
    FComment                             : TCoreClassStrings;
    FSectionTextList, FSectionVariantList: THashObjectList;
    FAutoUpdateDefaultValue              : Boolean;
    FMaxHashBlock                        : Integer;

    function GetNames(aName: SystemString): TCoreClassStrings;
    procedure SetNames(aName: SystemString; const Value: TCoreClassStrings);
    function GetHit(aSectionName, _VariantName: SystemString): Variant;
    procedure SetHit(aSectionName, _VariantName: SystemString; const Value: Variant);
    function GetVariantList(aName: SystemString): THashVariantList;
    procedure SetVariantList(aName: SystemString; const Value: THashVariantList);
    // return override state
    procedure AddDataSection(aSection: SystemString; TextList: TCoreClassStrings);
  public
    constructor Create; overload;
    constructor Create(AHashBlock: Integer); overload;
    destructor Destroy; override;

    procedure ReBuildList;
    procedure Clear;
    procedure Delete(aName: SystemString);

    function Exists(aName: SystemString): Boolean;

    function GetDefaultValue(const SectionName, KeyName: SystemString; const DefaultValue: Variant): Variant;
    procedure SetDefaultValue(const SectionName, KeyName: SystemString; const Value: Variant);

    // import section
    function DataImport(TextList: TCoreClassStrings): Boolean;

    // export section
    procedure DataExport(TextList: TCoreClassStrings);

    procedure Merge(sour: TSectionTextData);
    procedure Assign(sour: TSectionTextData);
    function Same(sour: TSectionTextData): Boolean;

    procedure LoadFromStream(Stream: TCoreClassStream);
    procedure SaveToStream(Stream: TCoreClassStream);

    procedure LoadFromFile(FileName: SystemString);
    procedure SaveToFile(FileName: SystemString);

    function GetAsText: SystemString;
    procedure SetAsText(const Value: SystemString);
    property AsText: SystemString read GetAsText write SetAsText;

    procedure GetSectionList(Dest: TCoreClassStrings); overload;
    procedure GetSectionList(Dest: TListString); overload;
    procedure GetSectionList(Dest: TListPascalString); overload;
    function GetSectionObjectName(_Obj: THashVariantList): SystemString;

    property AutoUpdateDefaultValue: Boolean read FAutoUpdateDefaultValue write FAutoUpdateDefaultValue;
    property Comment: TCoreClassStrings read FComment write FComment;
    property Hit[aSectionName, _VariantName: SystemString]: Variant read GetHit write SetHit; default;
    property Names[aName: SystemString]: TCoreClassStrings read GetNames write SetNames;
    property Texts[aName: SystemString]: TCoreClassStrings read GetNames write SetNames;
    property Strings[aName: SystemString]: TCoreClassStrings read GetNames write SetNames;
    property VariantList[aName: SystemString]: THashVariantList read GetVariantList write SetVariantList;
  end;

  THashVariantTextStream = class(TCoreClassObject)
  private
    FVariantList: THashVariantList;

    function GetNames(aName: SystemString): Variant;
    procedure SetNames(aName: SystemString; const Value: Variant);
  public
    constructor Create(_VList: THashVariantList);
    destructor Destroy; override;
    procedure Clear;

    class function VToStr(v: Variant): SystemString; inline;
    class function StrToV(s: SystemString): Variant; inline;

    procedure DataImport(TextList: TCoreClassStrings);
    procedure DataExport(TextList: TCoreClassStrings);
    procedure LoadFromStream(Stream: TCoreClassStream);
    procedure SaveToStream(Stream: TCoreClassStream);
    procedure LoadFromFile(FileName: SystemString);
    procedure SaveToFile(FileName: SystemString);
    procedure LoadFromText(aText: SystemString);

    procedure SaveToText(var aText: SystemString); overload;
    function Text: SystemString;

    function GetValue(aName: SystemString; v: Variant): Variant;

    property Names[aName: SystemString]: Variant read GetNames write SetNames; default;
    property VariantList: THashVariantList read FVariantList write FVariantList;
  end;

implementation

uses UnicodeMixedLib;

function TSectionTextData.GetNames(aName: SystemString): TCoreClassStrings;
var
  h: THashVariantTextStream;
begin
  if not FSectionTextList.Exists(aName) then
      FSectionTextList[aName] := TCoreClassStringList.Create;

  if FSectionVariantList.Exists(aName) then
    begin
      Result := TCoreClassStringList.Create;
      h := THashVariantTextStream.Create(THashVariantList(FSectionVariantList[aName]));
      h.DataExport(Result);
      DisposeObject(h);

      FSectionTextList[aName] := Result;
    end;

  Result := TCoreClassStrings(FSectionTextList[aName]);
end;

procedure TSectionTextData.SetNames(aName: SystemString; const Value: TCoreClassStrings);
var
  ns: TCoreClassStrings;
begin
  ns := TCoreClassStringList.Create;
  ns.Assign(Value);
  FSectionTextList[aName] := ns;
  FSectionVariantList.Delete(aName);
end;

function TSectionTextData.GetHit(aSectionName, _VariantName: SystemString): Variant;
var
  nsl: TCoreClassStrings;
  vl : THashVariantList;
  vt : THashVariantTextStream;
begin
  Result := NULL;
  vl := THashVariantList(FSectionVariantList[aSectionName]);
  if vl = nil then
    begin
      nsl := Names[aSectionName];
      if nsl = nil then
          Exit;
      if nsl.Count = 0 then
          Exit;
      vl := THashVariantList.Create(FMaxHashBlock);
      vl.AutoUpdateDefaultValue := AutoUpdateDefaultValue;

      vt := THashVariantTextStream.Create(vl);
      vt.DataImport(nsl);
      DisposeObject(vt);

      FSectionVariantList[aSectionName] := vl;
    end;
  Result := vl[_VariantName];
end;

procedure TSectionTextData.SetHit(aSectionName, _VariantName: SystemString; const Value: Variant);
var
  nsl: TCoreClassStrings;
  vl : THashVariantList;
  vt : THashVariantTextStream;
begin
  vl := THashVariantList(FSectionVariantList[aSectionName]);
  if vl = nil then
    begin
      vl := THashVariantList.Create(FMaxHashBlock);
      vl.AutoUpdateDefaultValue := AutoUpdateDefaultValue;

      nsl := Names[aSectionName];
      if nsl <> nil then
        begin
          vt := THashVariantTextStream.Create(vl);
          vt.DataImport(nsl);
          DisposeObject(vt);
        end;
      FSectionVariantList[aSectionName] := vl;
    end;
  vl[_VariantName] := Value;
end;

function TSectionTextData.GetVariantList(aName: SystemString): THashVariantList;
var
  nsl: TCoreClassStrings;
  vt : THashVariantTextStream;
begin
  Result := THashVariantList(FSectionVariantList[aName]);
  if Result = nil then
    begin
      Result := THashVariantList.Create(FMaxHashBlock);
      Result.AutoUpdateDefaultValue := FAutoUpdateDefaultValue;
      nsl := Names[aName];
      if nsl <> nil then
        begin
          vt := THashVariantTextStream.Create(Result);
          vt.DataImport(nsl);
          DisposeObject(vt);
        end;

      FSectionVariantList[aName] := Result;
    end;
end;

procedure TSectionTextData.SetVariantList(aName: SystemString; const Value: THashVariantList);
var
  h: THashVariantTextStream;
begin
  FSectionVariantList[aName] := Value;
  if not FSectionTextList.Exists(aName) then
      FSectionTextList[aName] := TCoreClassStringList.Create;
  h := THashVariantTextStream.Create(Value);
  TCoreClassStrings(FSectionTextList[aName]).Clear;
  h.DataExport(TCoreClassStrings(FSectionTextList[aName]));
  DisposeObject(h);
end;

procedure TSectionTextData.AddDataSection(aSection: SystemString; TextList: TCoreClassStrings);
begin
  while (TextList.Count > 0) and (TextList[0] = '') do
      TextList.Delete(0);
  while (TextList.Count > 0) and (TextList[TextList.Count - 1] = '') do
      TextList.Delete(TextList.Count - 1);

  FSectionTextList.Add(aSection, TextList);
end;

constructor TSectionTextData.Create;
begin
  inherited Create;
  FMaxHashBlock := 10;
  FComment := TCoreClassStringList.Create;
  FSectionTextList := THashObjectList.Create(True, FMaxHashBlock);
  FSectionVariantList := THashObjectList.Create(True, FMaxHashBlock);
  FAutoUpdateDefaultValue := False;
end;

constructor TSectionTextData.Create(AHashBlock: Integer);
begin
  inherited Create;
  FMaxHashBlock := AHashBlock;
  FComment := TCoreClassStringList.Create;
  FSectionTextList := THashObjectList.Create(True, FMaxHashBlock);
  FSectionVariantList := THashObjectList.Create(True, FMaxHashBlock);
  FAutoUpdateDefaultValue := False;
end;

destructor TSectionTextData.Destroy;
begin
  Clear;
  DisposeObject(FSectionTextList);
  DisposeObject(FSectionVariantList);
  DisposeObject(FComment);
  inherited Destroy;
end;

procedure TSectionTextData.ReBuildList;
var
  i        : Integer;
  tmpSecLst: TCoreClassStrings;
  nsl      : TCoreClassStrings;
  h        : THashVariantTextStream;
begin
  tmpSecLst := TCoreClassStringList.Create;

  FSectionVariantList.GetListData(tmpSecLst);
  if tmpSecLst.Count > 0 then
    for i := 0 to tmpSecLst.Count - 1 do
      begin
        nsl := TCoreClassStringList.Create;
        h := THashVariantTextStream.Create(THashVariantList(tmpSecLst.Objects[i]));
        h.DataExport(nsl);
        DisposeObject(h);
        FSectionTextList[tmpSecLst[i]] := nsl;
      end;

  FSectionVariantList.Clear;
  DisposeObject(tmpSecLst);
end;

procedure TSectionTextData.Clear;
begin
  FSectionTextList.Clear;
  FSectionVariantList.Clear;
  FComment.Clear;
end;

procedure TSectionTextData.Delete(aName: SystemString);
begin
  FSectionTextList.Delete(aName);
  FSectionVariantList.Delete(aName);
end;

function TSectionTextData.Exists(aName: SystemString): Boolean;
begin
  Result := FSectionTextList.Exists(aName) or FSectionVariantList.Exists(aName);
end;

function TSectionTextData.GetDefaultValue(const SectionName, KeyName: SystemString; const DefaultValue: Variant): Variant;
begin
  Result := VariantList[SectionName].GetDefaultValue(KeyName, DefaultValue);
end;

procedure TSectionTextData.SetDefaultValue(const SectionName, KeyName: SystemString; const Value: Variant);
begin
  Hit[SectionName, KeyName] := Value;
end;

function TSectionTextData.DataImport(TextList: TCoreClassStrings): Boolean;
var
  i    : Integer;
  nsect: SystemString;
  ntLst: TCoreClassStrings;

  tmpSecLst: TCoreClassStrings;
  nsl      : TCoreClassStrings;
  vt       : THashVariantTextStream;
begin
  // merge section
  tmpSecLst := TCoreClassStringList.Create;
  FSectionVariantList.GetListData(tmpSecLst);
  if tmpSecLst.Count > 0 then
    for i := 0 to tmpSecLst.Count - 1 do
      begin
        vt := THashVariantTextStream.Create(THashVariantList(tmpSecLst.Objects[i]));
        nsl := TCoreClassStringList.Create;
        FSectionTextList[tmpSecLst[i]] := nsl;
        vt.DataExport(nsl);
        DisposeObject(vt);
      end;
  DisposeObject(tmpSecLst);
  FSectionVariantList.Clear;
  // import new section
  ntLst := nil;
  nsect := '';
  Result := False;
  if Assigned(TextList) then
    begin
      if TextList.Count > 0 then
        begin
          i := 0;
          while i < TextList.Count do
            begin
              if umlMultipleMatch(False, '[*]', umlTrimChar(TextList[i], ' ')) then
                begin
                  if Result then
                      AddDataSection(nsect, ntLst);
                  ntLst := TCoreClassStringList.Create;
                  nsect := umlGetFirstStr(TextList[i], '[]').Text;
                  Result := True;
                end
              else if Result then
                begin
                  ntLst.Append(TextList[i]);
                end
              else
                begin
                  if TextList[i] <> '' then
                      FComment.Append(TextList[i]);
                end;
              Inc(i);
            end;
          if Result then
              AddDataSection(nsect, ntLst);
        end;

      while (FComment.Count > 0) and (FComment[0] = '') do
          FComment.Delete(0);
      while (FComment.Count > 0) and (FComment[FComment.Count - 1] = '') do
          FComment.Delete(FComment.Count - 1);
    end;
end;

procedure TSectionTextData.DataExport(TextList: TCoreClassStrings);
var
  i        : Integer;
  tmpSecLst: TCoreClassStrings;
  nsl      : TCoreClassStrings;
  vt       : THashVariantTextStream;
begin
  TextList.AddStrings(FComment);
  if FComment.Count > 0 then
      TextList.Append('');
  tmpSecLst := TCoreClassStringList.Create;

  FSectionVariantList.GetListData(tmpSecLst);
  if tmpSecLst.Count > 0 then
    for i := 0 to tmpSecLst.Count - 1 do
      begin
        vt := THashVariantTextStream.Create(THashVariantList(tmpSecLst.Objects[i]));
        nsl := TCoreClassStringList.Create;
        FSectionTextList[tmpSecLst[i]] := nsl;
        vt.DataExport(nsl);
        DisposeObject(vt);
      end;

  FSectionTextList.GetListData(tmpSecLst);
  if tmpSecLst.Count > 0 then
    for i := 0 to tmpSecLst.Count - 1 do
      if (tmpSecLst.Objects[i] is TCoreClassStrings) then
        begin
          nsl := TCoreClassStrings(tmpSecLst.Objects[i]);
          if nsl <> nil then
            begin
              TextList.Append('[' + tmpSecLst[i] + ']');
              TextList.AddStrings(nsl);
              TextList.Append('');
            end;
        end;

  DisposeObject(tmpSecLst);
end;

procedure TSectionTextData.Merge(sour: TSectionTextData);
var
  ns: TCoreClassStringList;
begin
  try
    ReBuildList;
    ns := TCoreClassStringList.Create;
    sour.ReBuildList;
    sour.DataExport(ns);
    DataImport(ns);
    DisposeObject(ns);
    ReBuildList;
  except
  end;
end;

procedure TSectionTextData.Assign(sour: TSectionTextData);
var
  ns: TCoreClassStringList;
begin
  try
    ns := TCoreClassStringList.Create;
    sour.ReBuildList;
    sour.DataExport(ns);
    Clear;
    DataImport(ns);
    DisposeObject(ns);
  except
  end;
end;

function TSectionTextData.Same(sour: TSectionTextData): Boolean;
var
  i : Integer;
  ns: TCoreClassStringList;
  n : SystemString;
begin
  Result := False;
  ReBuildList;
  sour.ReBuildList;

  // if Comment.Text <> sour.Comment.Text then
  // Exit;

  if FSectionTextList.Count <> sour.FSectionTextList.Count then
      Exit;

  ns := TCoreClassStringList.Create;

  for i := 0 to ns.Count - 1 do
    begin
      n := ns[i];
      if not sour.Exists(n) then
        begin
          DisposeObject(ns);
          Exit;
        end;
    end;

  for i := 0 to ns.Count - 1 do
    begin
      n := ns[i];
      if not SameText(Strings[n].Text, sour.Strings[n].Text) then
        begin
          DisposeObject(ns);
          Exit;
        end;
    end;

  DisposeObject(ns);
  Result := True;
end;

procedure TSectionTextData.LoadFromStream(Stream: TCoreClassStream);
var
  n: TCoreClassStrings;
begin
  Clear;
  n := TCoreClassStringList.Create;
  {$IFDEF FPC}
  n.LoadFromStream(Stream);
  {$ELSE}
  n.LoadFromStream(Stream, TEncoding.UTF8);
  {$ENDIF}
  DataImport(n);
  DisposeObject(n);
end;

procedure TSectionTextData.SaveToStream(Stream: TCoreClassStream);
var
  n: TCoreClassStrings;
begin
  n := TCoreClassStringList.Create;
  DataExport(n);
  {$IFDEF FPC}
  n.SaveToStream(Stream);
  {$ELSE}
  n.SaveToStream(Stream, TEncoding.UTF8);
  {$ENDIF}
  DisposeObject(n);
end;

procedure TSectionTextData.LoadFromFile(FileName: SystemString);
var
  ns: TMemoryStream64;
begin
  try
    ns := TMemoryStream64.Create;
    ns.LoadFromFile(FileName);
  except
    DisposeObject(ns);
    Exit;
  end;

  try
      LoadFromStream(ns);
  finally
      DisposeObject(ns);
  end;
end;

procedure TSectionTextData.SaveToFile(FileName: SystemString);
var
  ns: TMemoryStream64;
begin
  ns := TMemoryStream64.Create;
  try
    SaveToStream(ns);
    ns.SaveToFile(FileName);
  finally
      DisposeObject(ns);
  end;
end;

function TSectionTextData.GetAsText: SystemString;
var
  ns: TCoreClassStringList;
begin
  ns := TCoreClassStringList.Create;
  DataExport(ns);
  Result := ns.Text;
  DisposeObject(ns);
end;

procedure TSectionTextData.SetAsText(const Value: SystemString);
var
  ns: TCoreClassStringList;
begin
  Clear;

  ns := TCoreClassStringList.Create;
  ns.Text := Value;
  DataImport(ns);
  DisposeObject(ns);
end;

procedure TSectionTextData.GetSectionList(Dest: TCoreClassStrings);
var
  i        : Integer;
  tmpSecLst: TCoreClassStrings;
  nsl      : TCoreClassStrings;
  vt       : THashVariantTextStream;
begin
  tmpSecLst := TCoreClassStringList.Create;
  FSectionVariantList.GetListData(tmpSecLst);
  if tmpSecLst.Count > 0 then
    for i := 0 to tmpSecLst.Count - 1 do
      begin
        vt := THashVariantTextStream.Create(THashVariantList(tmpSecLst.Objects[i]));
        nsl := TCoreClassStringList.Create;
        FSectionTextList[tmpSecLst[i]] := nsl;
        vt.DataExport(nsl);
        DisposeObject(vt);
      end;
  DisposeObject(tmpSecLst);

  FSectionTextList.GetListData(Dest);
end;

procedure TSectionTextData.GetSectionList(Dest: TListString);
var
  i        : Integer;
  tmpSecLst: TCoreClassStrings;
  nsl      : TCoreClassStrings;
  vt       : THashVariantTextStream;
begin
  tmpSecLst := TCoreClassStringList.Create;
  FSectionVariantList.GetListData(tmpSecLst);
  if tmpSecLst.Count > 0 then
    for i := 0 to tmpSecLst.Count - 1 do
      begin
        vt := THashVariantTextStream.Create(THashVariantList(tmpSecLst.Objects[i]));
        nsl := TCoreClassStringList.Create;
        FSectionTextList[tmpSecLst[i]] := nsl;
        vt.DataExport(nsl);
        DisposeObject(vt);
      end;
  DisposeObject(tmpSecLst);

  FSectionTextList.GetNameList(Dest);
end;

procedure TSectionTextData.GetSectionList(Dest: TListPascalString);
var
  i        : Integer;
  tmpSecLst: TCoreClassStrings;
  nsl      : TCoreClassStrings;
  vt       : THashVariantTextStream;
begin
  tmpSecLst := TCoreClassStringList.Create;
  FSectionVariantList.GetListData(tmpSecLst);
  if tmpSecLst.Count > 0 then
    for i := 0 to tmpSecLst.Count - 1 do
      begin
        vt := THashVariantTextStream.Create(THashVariantList(tmpSecLst.Objects[i]));
        nsl := TCoreClassStringList.Create;
        FSectionTextList[tmpSecLst[i]] := nsl;
        vt.DataExport(nsl);
        DisposeObject(vt);
      end;
  DisposeObject(tmpSecLst);

  FSectionTextList.GetNameList(Dest);
end;

function TSectionTextData.GetSectionObjectName(_Obj: THashVariantList): SystemString;
begin
  Result := FSectionVariantList.GetObjAsName(_Obj);
end;

function THashVariantTextStream.GetNames(aName: SystemString): Variant;
begin
  if FVariantList <> nil then
      Result := FVariantList[aName]
  else
      Result := NULL;
end;

procedure THashVariantTextStream.SetNames(aName: SystemString; const Value: Variant);
begin
  if FVariantList <> nil then
      FVariantList[aName] := Value;
end;

constructor THashVariantTextStream.Create(_VList: THashVariantList);
begin
  inherited Create;
  FVariantList := _VList;
end;

destructor THashVariantTextStream.Destroy;
begin
  inherited Destroy;
end;

procedure THashVariantTextStream.Clear;
begin
  if FVariantList <> nil then
      FVariantList.Clear;
end;

class function THashVariantTextStream.VToStr(v: Variant): SystemString;
var
  n, b64: umlString;
begin
  try
    case VarType(v) of
      varSmallint, varInteger, varShortInt, varByte, varWord, varLongWord:
        begin
          Result := IntToStr(v);
        end;
      varInt64:
        begin
          Result := IntToStr(int64(v));
        end;
      varUInt64:
        begin
          {$IFDEF FPC}
          Result := IntToStr(UInt64(v));
          {$ELSE}
          Result := UIntToStr(UInt64(v));
          {$ENDIF}
        end;
      varSingle, varDouble, varCurrency, varDate:
        begin
          Result := FloatToStr(v);
        end;
      varOleStr, varString, varUString:
        begin
          n.Text := VarToStr(v);

          if umlExistsLimitChar(n, #10#13#9#8#0) then
            begin
              umlEncodeLineBASE64(n, b64);
              Result := '___base64:' + b64.Text;
            end
          else
              Result := n.Text;
        end;
      varBoolean:
        begin
          Result := BoolToStr(v, True);
        end;
      else
        Result := VarToStr(v);
    end;
  except
    try
        Result := VarToStr(v);
    except
        Result := '';
    end;
  end;
end;

class function THashVariantTextStream.StrToV(s: SystemString): Variant;
var
  n,b64: umlString;
begin
  n := umlTrimSpace(s);
  try
    if n.ComparePos(1, '___base64:') then
      begin
        n := umlDeleteFirstStr(n, ':').Text;
        umlDecodeLineBASE64(n, b64);
        Result := b64.Text;
      end
    else
      begin
        case umlGetNumTextType(n) of
          ntBool: Result := StrToBool(n.Text);
          ntInt: Result := StrToInt(n.Text);
          ntInt64: Result := StrToInt64(n.Text);
          {$IFDEF FPC}
          ntUInt64: Result := StrToQWord(n.Text);
          {$ELSE}
          ntUInt64: Result := StrToUInt64(n.Text);
          {$ENDIF}
          ntWord: Result := StrToInt(n.Text);
          ntByte: Result := StrToInt(n.Text);
          ntSmallInt: Result := StrToInt(n.Text);
          ntShortInt: Result := StrToInt(n.Text);
          ntUInt: Result := StrToInt(n.Text);
          ntSingle: Result := StrToFloat(n.Text);
          ntDouble: Result := StrToFloat(n.Text);
          ntCurrency: Result := StrToFloat(n.Text);
          else Result := n.Text;
        end;
      end;
  except
      Result := n.Text;
  end;
end;

procedure THashVariantTextStream.DataImport(TextList: TCoreClassStrings);
var
  i                  : Integer;
  n                  : TPascalString;
  TextName, TextValue: TPascalString;
begin
  if FVariantList = nil then
      Exit;
  if TextList.Count > 0 then
    for i := 0 to TextList.Count - 1 do
      begin
        n := umlTrimSpace(TextList[i]);

        if ((n.Exists(':')) or (n.Exists('='))) and (not CharIn(n.First, [':', '='])) then
          begin
            TextName := umlGetFirstStr_M(n, ':=');
            if TextName.Len > 0 then
              begin
                TextValue := umlDeleteFirstStr_M(n, ':=');
                FVariantList[TextName.Text] := StrToV(TextValue.Text);
              end
            else
                FVariantList[n.Text] := '';
          end
        else
          begin
            FVariantList[n.Text] := '';
          end;
      end;
end;

procedure THashVariantTextStream.DataExport(TextList: TCoreClassStrings);
var
  i        : Integer;
  vl       : TCoreClassList;
  TextValue: SystemString;
begin
  if FVariantList = nil then
      Exit;
  vl := TCoreClassList.Create;
  FVariantList.HashList.GetListData(vl);
  if vl.Count > 0 then
    for i := 0 to vl.Count - 1 do
      begin
        TextValue := VToStr(PHashVariantListData(PHashListData(vl[i])^.Data)^.v);

        if TextValue <> '' then
            TextList.Add((PHashListData(vl[i])^.OriginName + '=' + TextValue))
        else
            TextList.Add(PHashListData(vl[i])^.OriginName);
      end;
  DisposeObject(vl);
end;

procedure THashVariantTextStream.LoadFromStream(Stream: TCoreClassStream);
var
  n: TCoreClassStrings;
begin
  if FVariantList = nil then
      Exit;
  n := TCoreClassStringList.Create;
  {$IFDEF FPC}
  n.LoadFromStream(Stream);
  {$ELSE}
  n.LoadFromStream(Stream, TEncoding.UTF8);
  {$ENDIF}
  DataImport(n);
  DisposeObject(n);
end;

procedure THashVariantTextStream.SaveToStream(Stream: TCoreClassStream);
var
  n: TCoreClassStrings;
begin
  if FVariantList = nil then
      Exit;
  n := TCoreClassStringList.Create;
  DataExport(n);
  {$IFDEF FPC}
  n.SaveToStream(Stream);
  {$ELSE}
  n.SaveToStream(Stream, TEncoding.UTF8);
  {$ENDIF}
  DisposeObject(n);
end;

procedure THashVariantTextStream.LoadFromFile(FileName: SystemString);
var
  ns: TCoreClassStream;
begin
  ns := TCoreClassFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
      LoadFromStream(ns);
  finally
      DisposeObject(ns);
  end;
end;

procedure THashVariantTextStream.SaveToFile(FileName: SystemString);
var
  ns: TCoreClassStream;
begin
  ns := TCoreClassFileStream.Create(FileName, fmCreate);
  try
      SaveToStream(ns);
  finally
      DisposeObject(ns);
  end;
end;

procedure THashVariantTextStream.LoadFromText(aText: SystemString);
var
  n: TCoreClassStrings;
begin
  if FVariantList = nil then
      Exit;
  n := TCoreClassStringList.Create;
  n.Text := aText;
  DataImport(n);
  DisposeObject(n);
end;

procedure THashVariantTextStream.SaveToText(var aText: SystemString);
var
  n: TCoreClassStrings;
begin
  if FVariantList = nil then
      Exit;
  n := TCoreClassStringList.Create;
  DataExport(n);
  aText := n.Text;
  DisposeObject(n);
end;

function THashVariantTextStream.Text: SystemString;
begin
  SaveToText(Result);
end;

function THashVariantTextStream.GetValue(aName: SystemString; v: Variant): Variant;
begin
  Result := Names[aName];
  if VarIsNull(Result) then
    begin
      Names[aName] := v;
      Result := v;
    end;
end;

end.
