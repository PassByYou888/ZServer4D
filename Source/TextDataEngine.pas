{ ****************************************************************************** }
{ * ini text library,writen by QQ 600585@qq.com                                * }
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

unit TextDataEngine;

{$INCLUDE zDefine.inc}

interface

uses SysUtils, Variants,
{$IFDEF FPC}
  FPCGenericStructlist,
{$ENDIF FPC}
  CoreClasses,
  UnicodeMixedLib,
  PascalStrings,
  ListEngine,
  MemoryStream64;

type
  THashTextEngine = class(TCoreClassObject)
  private
    FComment: TCoreClassStrings;
    FSectionList, FSectionHashVariantList, FSectionHashStringList: THashObjectList;
    FAutoUpdateDefaultValue: Boolean;
    FSectionPoolSize, FListPoolSize: Integer;
    FIsChanged: Boolean;

    function GetNames(N_: SystemString): TCoreClassStrings;
    procedure SetNames(N_: SystemString; const Value: TCoreClassStrings);
    function GetHitVariant(SName, VName: SystemString): Variant;
    procedure SetHitVariant(SName, VName: SystemString; const Value: Variant);
    function GetHitString(SName, VName: SystemString): SystemString;
    procedure SetHitString(SName, VName: SystemString; const Value: SystemString);

    // return override state
    function GetHVariantList(N_: SystemString): THashVariantList;
    function GetHStringList(N_: SystemString): THashStringList;
    procedure AddDataSection(Section_: SystemString; TextList_: TCoreClassStrings);
  public
    constructor Create; overload;
    constructor Create(SectionPoolSize_: Integer); overload;
    constructor Create(SectionPoolSize_, ListPoolSize_: Integer); overload;
    destructor Destroy; override;
    property IsChanged: Boolean read FIsChanged write FIsChanged;

    procedure Rebuild;
    procedure Clear;
    procedure Delete(N_: SystemString);

    function Exists(N_: SystemString): Boolean;

    function GetDefaultValue(const SectionName, KeyName: SystemString; const DefaultValue: Variant): Variant;
    procedure SetDefaultValue(const SectionName, KeyName: SystemString; const Value: Variant);

    function GetDefaultText(const SectionName, KeyName: SystemString; const DefaultValue: SystemString): SystemString;
    procedure SetDefaultText(const SectionName, KeyName: SystemString; const Value: SystemString);

    // import section
    function DataImport(TextList_: TCoreClassStrings): Boolean; overload;
    function DataImport(TextList_: TListPascalString): Boolean; overload;

    // export section
    procedure DataExport(TextList_: TCoreClassStrings); overload;
    procedure DataExport(TextList_: TListPascalString); overload;

    procedure Merge(sour: THashTextEngine);
    procedure Assign(sour: THashTextEngine);
    function Same(sour: THashTextEngine): Boolean;

    procedure LoadFromStream(stream: TCoreClassStream);
    procedure SaveToStream(stream: TCoreClassStream);

    procedure LoadFromFile(FileName: SystemString);
    procedure SaveToFile(FileName: SystemString);

    function TotalCount: NativeInt;
    function MaxSectionNameLen: Integer;
    function MinSectionNameLen: Integer;

    function GetAsText: SystemString;
    procedure SetAsText(const Value: SystemString);
    property AsText: SystemString read GetAsText write SetAsText;

    procedure GetSectionList(dest: TCoreClassStrings); overload;
    procedure GetSectionList(dest: TListString); overload;
    procedure GetSectionList(dest: TListPascalString); overload;
    function GetSectionObjectName(Obj_: THashVariantList): SystemString; overload;
    function GetSectionObjectName(Obj_: THashStringList): SystemString; overload;

    property AutoUpdateDefaultValue: Boolean read FAutoUpdateDefaultValue write FAutoUpdateDefaultValue;

    property Comment: TCoreClassStrings read FComment write FComment;

    property Hit[SName, VName: SystemString]: Variant read GetHitVariant write SetHitVariant; default;
    property HitVariant[SName, VName: SystemString]: Variant read GetHitVariant write SetHitVariant;
    property HitString[SName, VName: SystemString]: SystemString read GetHitString write SetHitString;
    property HitS[SName, VName: SystemString]: SystemString read GetHitString write SetHitString;
    property SHit[SName, VName: SystemString]: SystemString read GetHitString write SetHitString;

    property Names[N_: SystemString]: TCoreClassStrings read GetNames write SetNames;
    property Strings[N_: SystemString]: TCoreClassStrings read GetNames write SetNames;

    property VariantList[N_: SystemString]: THashVariantList read GetHVariantList;
    property HVariantList[N_: SystemString]: THashVariantList read GetHVariantList;
    property StringList[N_: SystemString]: THashStringList read GetHStringList;
    property HStringList[N_: SystemString]: THashStringList read GetHStringList;
  end;

  TTextDataEngine = THashTextEngine;
  TSectionTextData = THashTextEngine;

  THashTextEngineList_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<THashTextEngine>;

implementation

function THashTextEngine.GetNames(N_: SystemString): TCoreClassStrings;
var
  h: THashVariantTextStream;
begin
  if not FSectionList.Exists(N_) then
      FSectionList[N_] := TCoreClassStringList.Create;

  if FSectionHashVariantList.Exists(N_) then
    begin
      Result := TCoreClassStringList.Create;
      h := THashVariantTextStream.Create(THashVariantList(FSectionHashVariantList[N_]));
      h.DataExport(Result);
      DisposeObject(h);

      FSectionList[N_] := Result;
      FIsChanged := True;
    end;

  Result := TCoreClassStrings(FSectionList[N_]);
end;

procedure THashTextEngine.SetNames(N_: SystemString; const Value: TCoreClassStrings);
var
  ns: TCoreClassStrings;
begin
  ns := TCoreClassStringList.Create;
  ns.Assign(Value);
  FSectionList[N_] := ns;
  FSectionHashVariantList.Delete(N_);
  FIsChanged := True;
end;

function THashTextEngine.GetHitVariant(SName, VName: SystemString): Variant;
var
  nsl: TCoreClassStrings;
  vl: THashVariantList;
  vt: THashVariantTextStream;
begin
  Result := Null;
  vl := THashVariantList(FSectionHashVariantList[SName]);
  if vl = nil then
    begin
      nsl := Names[SName];
      if nsl = nil then
          Exit;
      if nsl.Count = 0 then
          Exit;
      vl := THashVariantList.CustomCreate(FListPoolSize);
      vl.AutoUpdateDefaultValue := AutoUpdateDefaultValue;

      vt := THashVariantTextStream.Create(vl);
      vt.DataImport(nsl);
      DisposeObject(vt);

      FSectionHashVariantList[SName] := vl;
      FIsChanged := True;
    end;
  Result := vl[VName];
end;

procedure THashTextEngine.SetHitVariant(SName, VName: SystemString; const Value: Variant);
var
  nsl: TCoreClassStrings;
  vl: THashVariantList;
  vt: THashVariantTextStream;
begin
  vl := THashVariantList(FSectionHashVariantList[SName]);
  if vl = nil then
    begin
      vl := THashVariantList.CustomCreate(FListPoolSize);
      vl.AutoUpdateDefaultValue := AutoUpdateDefaultValue;

      nsl := Names[SName];
      if nsl <> nil then
        begin
          vt := THashVariantTextStream.Create(vl);
          vt.DataImport(nsl);
          DisposeObject(vt);
        end;
      FSectionHashVariantList[SName] := vl;
    end;
  vl[VName] := Value;
  FIsChanged := True;
end;

function THashTextEngine.GetHitString(SName, VName: SystemString): SystemString;
var
  nsl: TCoreClassStrings;
  sl: THashStringList;
  st: THashStringTextStream;
begin
  Result := '';
  sl := THashStringList(FSectionHashStringList[SName]);
  if sl = nil then
    begin
      nsl := Names[SName];
      if nsl = nil then
          Exit;
      if nsl.Count = 0 then
          Exit;
      sl := THashStringList.CustomCreate(FListPoolSize);
      sl.AutoUpdateDefaultValue := AutoUpdateDefaultValue;

      st := THashStringTextStream.Create(sl);
      st.DataImport(nsl);
      DisposeObject(st);

      FSectionHashStringList[SName] := sl;
      FIsChanged := True;
    end;
  Result := sl[VName];
end;

procedure THashTextEngine.SetHitString(SName, VName: SystemString; const Value: SystemString);
var
  nsl: TCoreClassStrings;
  sl: THashStringList;
  st: THashStringTextStream;
begin
  sl := THashStringList(FSectionHashStringList[SName]);
  if sl = nil then
    begin
      sl := THashStringList.CustomCreate(FListPoolSize);
      sl.AutoUpdateDefaultValue := AutoUpdateDefaultValue;

      nsl := Names[SName];
      if nsl <> nil then
        begin
          st := THashStringTextStream.Create(sl);
          st.DataImport(nsl);
          DisposeObject(st);
        end;
      FSectionHashStringList[SName] := sl;
    end;
  sl[VName] := Value;
  FIsChanged := True;
end;

function THashTextEngine.GetHVariantList(N_: SystemString): THashVariantList;
var
  nsl: TCoreClassStrings;
  vt: THashVariantTextStream;
begin
  Result := THashVariantList(FSectionHashVariantList[N_]);
  if Result = nil then
    begin
      Result := THashVariantList.CustomCreate(FListPoolSize);
      Result.AutoUpdateDefaultValue := FAutoUpdateDefaultValue;
      nsl := Names[N_];
      if nsl <> nil then
        begin
          vt := THashVariantTextStream.Create(Result);
          vt.DataImport(nsl);
          DisposeObject(vt);
        end;

      FSectionHashVariantList[N_] := Result;
      FIsChanged := True;
    end;
end;

function THashTextEngine.GetHStringList(N_: SystemString): THashStringList;
var
  nsl: TCoreClassStrings;
  st: THashStringTextStream;
begin
  Result := THashStringList(FSectionHashStringList[N_]);
  if Result = nil then
    begin
      Result := THashStringList.CustomCreate(FListPoolSize);
      Result.AutoUpdateDefaultValue := FAutoUpdateDefaultValue;
      nsl := Names[N_];
      if nsl <> nil then
        begin
          st := THashStringTextStream.Create(Result);
          st.DataImport(nsl);
          DisposeObject(st);
        end;

      FSectionHashStringList[N_] := Result;
      FIsChanged := True;
    end;
end;

procedure THashTextEngine.AddDataSection(Section_: SystemString; TextList_: TCoreClassStrings);
begin
  while (TextList_.Count > 0) and (TextList_[0] = '') do
      TextList_.Delete(0);
  while (TextList_.Count > 0) and (TextList_[TextList_.Count - 1] = '') do
      TextList_.Delete(TextList_.Count - 1);

  FSectionList.Add(Section_, TextList_);
end;

constructor THashTextEngine.Create;
begin
  Create(10, 10);
end;

constructor THashTextEngine.Create(SectionPoolSize_: Integer);
begin
  Create(SectionPoolSize_, 16);
end;

constructor THashTextEngine.Create(SectionPoolSize_, ListPoolSize_: Integer);
begin
  inherited Create;
  FSectionPoolSize := SectionPoolSize_;
  FListPoolSize := ListPoolSize_;

  FComment := TCoreClassStringList.Create;
  FSectionList := THashObjectList.CustomCreate(True, FSectionPoolSize);
  FSectionHashVariantList := THashObjectList.CustomCreate(True, FSectionPoolSize);
  FSectionHashStringList := THashObjectList.CustomCreate(True, FSectionPoolSize);
  FAutoUpdateDefaultValue := False;

  FIsChanged := False;
end;

destructor THashTextEngine.Destroy;
begin
  Clear;
  DisposeObject(FSectionList);
  DisposeObject(FSectionHashVariantList);
  DisposeObject(FSectionHashStringList);
  DisposeObject(FComment);
  inherited Destroy;
end;

procedure THashTextEngine.Rebuild;
var
  i: Integer;
  tmpSecLst: TListPascalString;
  nsl: TCoreClassStrings;
  hv: THashVariantTextStream;
  hs: THashStringTextStream;
begin
  tmpSecLst := TListPascalString.Create;

  if FSectionHashVariantList.Count > 0 then
    begin
      FSectionHashVariantList.GetListData(tmpSecLst);
      for i := 0 to tmpSecLst.Count - 1 do
        begin
          nsl := TCoreClassStringList.Create;
          hv := THashVariantTextStream.Create(THashVariantList(tmpSecLst.Objects[i]));
          hv.DataExport(nsl);
          DisposeObject(hv);
          FSectionList[tmpSecLst[i]] := nsl;
        end;
      FSectionHashVariantList.Clear;
    end;

  if FSectionHashStringList.Count > 0 then
    begin
      FSectionHashStringList.GetListData(tmpSecLst);
      for i := 0 to tmpSecLst.Count - 1 do
        begin
          nsl := TCoreClassStringList.Create;
          hs := THashStringTextStream.Create(THashStringList(tmpSecLst.Objects[i]));
          hs.DataExport(nsl);
          DisposeObject(hs);
          FSectionList[tmpSecLst[i]] := nsl;
        end;
      FSectionHashStringList.Clear;
    end;

  DisposeObject(tmpSecLst);
end;

procedure THashTextEngine.Clear;
begin
  FSectionList.Clear;
  FSectionHashVariantList.Clear;
  FSectionHashStringList.Clear;
  FComment.Clear;
end;

procedure THashTextEngine.Delete(N_: SystemString);
begin
  FSectionList.Delete(N_);
  FSectionHashVariantList.Delete(N_);
  FSectionHashStringList.Delete(N_);
end;

function THashTextEngine.Exists(N_: SystemString): Boolean;
begin
  Result := FSectionList.Exists(N_) or FSectionHashVariantList.Exists(N_) or FSectionHashStringList.Exists(N_);
end;

function THashTextEngine.GetDefaultValue(const SectionName, KeyName: SystemString; const DefaultValue: Variant): Variant;
begin
  Result := VariantList[SectionName].GetDefaultValue(KeyName, DefaultValue);
end;

procedure THashTextEngine.SetDefaultValue(const SectionName, KeyName: SystemString; const Value: Variant);
begin
  Hit[SectionName, KeyName] := Value;
end;

function THashTextEngine.GetDefaultText(const SectionName, KeyName: SystemString; const DefaultValue: SystemString): SystemString;
begin
  Result := HStringList[SectionName].GetDefaultValue(KeyName, DefaultValue);
end;

procedure THashTextEngine.SetDefaultText(const SectionName, KeyName: SystemString; const Value: SystemString);
begin
  HitString[SectionName, KeyName] := Value;
end;

function THashTextEngine.DataImport(TextList_: TCoreClassStrings): Boolean;
var
  i: Integer;
  ln: U_String;
  nsect: SystemString;
  ntLst: TCoreClassStrings;
begin
  // merge section
  Rebuild;

  // import new section
  ntLst := nil;
  nsect := '';
  Result := False;
  if Assigned(TextList_) then
    begin
      if TextList_.Count > 0 then
        begin
          i := 0;
          while i < TextList_.Count do
            begin
              ln := umlTrimChar(TextList_[i], ' ');
              if (ln.Len > 0) and (ln.First = '[') and (ln.Last = ']') then
                begin
                  if Result then
                      AddDataSection(nsect, ntLst);
                  ntLst := TCoreClassStringList.Create;
                  nsect := umlGetFirstStr(ln, '[]').Text;
                  Result := True;
                end
              else if Result then
                begin
                  ntLst.Append(ln);
                end
              else
                begin
                  if (ln.Len > 0) and (not CharIn(ln.First, [';'])) then
                      FComment.Append(ln);
                end;
              inc(i);
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

function THashTextEngine.DataImport(TextList_: TListPascalString): Boolean;
var
  i: Integer;
  ln: U_String;
  nsect: SystemString;
  ntLst: TCoreClassStrings;
begin
  // merge section
  Rebuild;

  // import new section
  ntLst := nil;
  nsect := '';
  Result := False;
  if Assigned(TextList_) then
    begin
      if TextList_.Count > 0 then
        begin
          i := 0;
          while i < TextList_.Count do
            begin
              ln := TextList_[i].TrimChar(' ');
              if (ln.Len > 0) and (ln.First = '[') and (ln.Last = ']') then
                begin
                  if Result then
                      AddDataSection(nsect, ntLst);
                  ntLst := TCoreClassStringList.Create;
                  nsect := umlGetFirstStr(ln, '[]').Text;
                  Result := True;
                end
              else if Result then
                begin
                  ntLst.Append(ln);
                end
              else
                begin
                  if (ln.Len > 0) and (not CharIn(ln.First, [';'])) then
                      FComment.Append(ln);
                end;
              inc(i);
            end;
          if Result then
              AddDataSection(nsect, ntLst);
        end;

      while (FComment.Count > 0) and (FComment[0] = '') do
          FComment.Delete(0);
      while (FComment.Count > 0) and (FComment[FComment.Count - 1] = '') do
          FComment.Delete(FComment.Count - 1);
    end;
  FIsChanged := False;
end;

procedure THashTextEngine.DataExport(TextList_: TCoreClassStrings);
var
  i: Integer;
  tmpSecLst: TListPascalString;
  nsl: TCoreClassStrings;
begin
  Rebuild;

  TextList_.AddStrings(FComment);
  if FComment.Count > 0 then
      TextList_.Append('');

  tmpSecLst := TListPascalString.Create;

  FSectionList.GetListData(tmpSecLst);
  if tmpSecLst.Count > 0 then
    for i := 0 to tmpSecLst.Count - 1 do
      if (tmpSecLst.Objects[i] is TCoreClassStrings) then
        begin
          nsl := TCoreClassStrings(tmpSecLst.Objects[i]);
          if nsl <> nil then
            begin
              TextList_.Append('[' + tmpSecLst[i] + ']');
              TextList_.AddStrings(nsl);
              TextList_.Append('');
            end;
        end;

  DisposeObject(tmpSecLst);
end;

procedure THashTextEngine.DataExport(TextList_: TListPascalString);
var
  i: Integer;
  tmpSecLst: TListPascalString;
  nsl: TCoreClassStrings;
begin
  Rebuild;

  TextList_.AddStrings(FComment);
  if FComment.Count > 0 then
      TextList_.Append('');

  tmpSecLst := TListPascalString.Create;

  FSectionList.GetListData(tmpSecLst);
  if tmpSecLst.Count > 0 then
    for i := 0 to tmpSecLst.Count - 1 do
      if (tmpSecLst.Objects[i] is TCoreClassStrings) then
        begin
          nsl := TCoreClassStrings(tmpSecLst.Objects[i]);
          if nsl <> nil then
            begin
              TextList_.Append('[' + tmpSecLst[i].Text + ']');
              TextList_.AddStrings(nsl);
              TextList_.Append('');
            end;
        end;

  DisposeObject(tmpSecLst);
end;

procedure THashTextEngine.Merge(sour: THashTextEngine);
var
  ns: TCoreClassStringList;
begin
  try
    Rebuild;
    ns := TCoreClassStringList.Create;
    sour.Rebuild;
    sour.DataExport(ns);
    DataImport(ns);
    DisposeObject(ns);
    Rebuild;
    FIsChanged := True;
  except
  end;
end;

procedure THashTextEngine.Assign(sour: THashTextEngine);
var
  ns: TCoreClassStringList;
begin
  try
    ns := TCoreClassStringList.Create;
    sour.Rebuild;
    sour.DataExport(ns);
    Clear;
    DataImport(ns);
    DisposeObject(ns);
    FIsChanged := True;
  except
  end;
end;

function THashTextEngine.Same(sour: THashTextEngine): Boolean;
var
  i: Integer;
  ns: TCoreClassStringList;
  N_: SystemString;
begin
  Result := False;
  Rebuild;
  sour.Rebuild;

  if FSectionList.Count <> sour.FSectionList.Count then
      Exit;

  ns := TCoreClassStringList.Create;

  for i := 0 to ns.Count - 1 do
    begin
      N_ := ns[i];
      if not sour.Exists(N_) then
        begin
          DisposeObject(ns);
          Exit;
        end;
    end;

  for i := 0 to ns.Count - 1 do
    begin
      N_ := ns[i];
      if not SameText(Strings[N_].Text, sour.Strings[N_].Text) then
        begin
          DisposeObject(ns);
          Exit;
        end;
    end;

  DisposeObject(ns);
  Result := True;
end;

procedure THashTextEngine.LoadFromStream(stream: TCoreClassStream);
var
  N_: TListPascalString;
begin
  Clear;
  N_ := TListPascalString.Create;
  N_.LoadFromStream(stream);
  DataImport(N_);
  DisposeObject(N_);
end;

procedure THashTextEngine.SaveToStream(stream: TCoreClassStream);
var
  N_: TListPascalString;
begin
  N_ := TListPascalString.Create;
  DataExport(N_);
  N_.SaveToStream(stream);
  DisposeObject(N_);
end;

procedure THashTextEngine.LoadFromFile(FileName: SystemString);
var
  m64: TMS64;
begin
  m64 := TMS64.Create;
  try
      m64.LoadFromFile(FileName);
  except
    DisposeObject(m64);
    Exit;
  end;

  try
      LoadFromStream(m64);
  finally
      DisposeObject(m64);
  end;
end;

procedure THashTextEngine.SaveToFile(FileName: SystemString);
var
  m64: TMS64;
begin
  m64 := TMS64.Create;
  try
    SaveToStream(m64);
    m64.SaveToFile(FileName);
  finally
      DisposeObject(m64);
  end;
end;

function THashTextEngine.TotalCount: NativeInt;
var
  i: Integer;
  tmpSecLst: TListPascalString;
begin
  Result := 0;
  tmpSecLst := TListPascalString.Create;

  FSectionList.GetListData(tmpSecLst);
  if tmpSecLst.Count > 0 then
    for i := 0 to tmpSecLst.Count - 1 do
      if (not FSectionHashVariantList.Exists(tmpSecLst[i])) and (not FSectionHashStringList.Exists(tmpSecLst[i])) then
          inc(Result, TCoreClassStrings(tmpSecLst.Objects[i]).Count);

  FSectionHashVariantList.GetListData(tmpSecLst);
  if tmpSecLst.Count > 0 then
    for i := 0 to tmpSecLst.Count - 1 do
        inc(Result, THashVariantList(tmpSecLst.Objects[i]).Count);

  FSectionHashStringList.GetListData(tmpSecLst);
  if tmpSecLst.Count > 0 then
    for i := 0 to tmpSecLst.Count - 1 do
        inc(Result, THashStringList(tmpSecLst.Objects[i]).Count);

  DisposeObject(tmpSecLst);
end;

function THashTextEngine.MaxSectionNameLen: Integer;
begin
  Result := umlMax(FSectionList.HashList.MaxNameLen,
    umlMax(FSectionHashVariantList.HashList.MaxNameLen, FSectionHashStringList.HashList.MaxNameLen));
end;

function THashTextEngine.MinSectionNameLen: Integer;
begin
  Result := umlMin(FSectionList.HashList.MinNameLen,
    umlMin(FSectionHashVariantList.HashList.MinNameLen, FSectionHashStringList.HashList.MinNameLen));
end;

function THashTextEngine.GetAsText: SystemString;
var
  ns: TPascalStringList;
begin
  ns := TPascalStringList.Create;
  DataExport(ns);
  Result := ns.AsText;
  DisposeObject(ns);
end;

procedure THashTextEngine.SetAsText(const Value: SystemString);
var
  ns: TListPascalString;
begin
  Clear;

  ns := TListPascalString.Create;
  ns.AsText := Value;
  DataImport(ns);
  DisposeObject(ns);
  FIsChanged := True;
end;

procedure THashTextEngine.GetSectionList(dest: TCoreClassStrings);
begin
  Rebuild;
  FSectionList.GetListData(dest);
end;

procedure THashTextEngine.GetSectionList(dest: TListString);
begin
  Rebuild;
  FSectionList.GetListData(dest);
end;

procedure THashTextEngine.GetSectionList(dest: TListPascalString);
begin
  Rebuild;
  FSectionList.GetListData(dest);
end;

function THashTextEngine.GetSectionObjectName(Obj_: THashVariantList): SystemString;
begin
  Result := FSectionHashVariantList.GetObjAsName(Obj_);
end;

function THashTextEngine.GetSectionObjectName(Obj_: THashStringList): SystemString;
begin
  Result := FSectionHashStringList.GetObjAsName(Obj_);
end;

end.
