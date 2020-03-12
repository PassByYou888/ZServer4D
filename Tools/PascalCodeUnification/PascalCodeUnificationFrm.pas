unit PascalCodeUnificationFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,

  System.Math,
  CoreClasses, ListEngine, PascalStrings, TextParsing, UnicodeMixedLib, DoStatusIO, TextDataEngine;

type
  TPascalCodeUnificationForm = class(TForm)
    FileListMemo: TMemo;
    AddFileButton: TButton;
    FixedButton: TButton;
    StatusMemo: TMemo;
    StateLabel: TLabel;
    ProgressBar: TProgressBar;
    WordDefineMemo: TMemo;
    dictInputInfoLabel: TLabel;
    WordOutputMemo: TMemo;
    dictOutputInfoLabel: TLabel;
    FixedWordCheckBox: TCheckBox;
    OpenDialog: TFileOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure AddFileButtonClick(Sender: TObject);
    procedure FixedButtonClick(Sender: TObject);
    procedure FixedWordCheckBoxClick(Sender: TObject);
  private
    procedure DoStatusMethod(AText: SystemString; const ID: Integer);
  public
  end;

var
  PascalCodeUnificationForm: TPascalCodeUnificationForm;

implementation

{$R *.dfm}


procedure TPascalCodeUnificationForm.FormCreate(Sender: TObject);
var
  he: THashTextEngine;
begin
  AddDoStatusHook(Self, DoStatusMethod);

  if umlFileExists(umlCombineFileName(umlGetFilePath(Application.Exename), 'PascalCodeUnification.ini')) then
    begin
      he := THashTextEngine.Create;
      he.LoadFromFile(umlCombineFileName(umlGetFilePath(Application.Exename), 'PascalCodeUnification.ini'));

      FixedWordCheckBox.Checked := he.GetDefaultValue('options', 'fixedWord', FixedWordCheckBox.Checked);

      FileListMemo.Lines.BeginUpdate;
      FileListMemo.Lines.Assign(he.Names['Files']);
      FileListMemo.Lines.EndUpdate;

      WordDefineMemo.Lines.BeginUpdate;
      WordDefineMemo.Lines.Assign(he.Names['Words']);
      WordDefineMemo.Lines.EndUpdate;

      DisposeObject(he);

      FixedWordCheckBoxClick(FixedWordCheckBox);
    end;
end;

procedure TPascalCodeUnificationForm.FormDestroy(Sender: TObject);
var
  he: THashTextEngine;
begin
  DeleteDoStatusHook(Self);

  he := THashTextEngine.Create;
  he.SetDefaultValue('options', 'fixedWord', FixedWordCheckBox.Checked);
  he.Names['Files'].Assign(FileListMemo.Lines);
  he.Names['Words'].Assign(WordDefineMemo.Lines);
  he.SaveToFile(umlCombineFileName(umlGetFilePath(Application.Exename), 'PascalCodeUnification.ini'));
  DisposeObject(he);
end;

procedure TPascalCodeUnificationForm.AddFileButtonClick(Sender: TObject);
begin
  if not OpenDialog.Execute() then
      Exit;
  FileListMemo.Lines.AddStrings(OpenDialog.Files);
  StateLabel.Caption := Format('code files: %d, fixed Unit define: %d, fixed word : %d', [FileListMemo.Lines.Count, 0, 0]);
end;

procedure TPascalCodeUnificationForm.FixedButtonClick(Sender: TObject);
var
  fixedUName_Counter, fixedWord_Counter: Integer;
  uHash: THashList;
  nHash: THashVariantList;

  function FixedUnit(const fn, un: SystemString): Boolean;
  type
    TParsingState = (psUnit, psUses, psUnknow);
  var
    Code: TCoreClassStringList;
    u_TP, w_TP: TTextParsing;

    i: Integer;
    p: PTokenData;
    State: TParsingState;
    n, N2, I_Name, prefix: TPascalString;
    nComp: Boolean;
  begin
    Result := False;
    if not umlFileExists(fn) then
        Exit;
    DoStatus('prepare parsing %s...', [umlGetFileName(fn).Text]);
    Code := TCoreClassStringList.Create;
    try
        Code.LoadFromFile(fn);
    except
      DoStatus('%s encoding error!', [umlGetFileName(fn).Text]);
      DisposeObject(Code);
      Exit;
    end;

    w_TP := nil;

    if FixedWordCheckBox.Checked then
      begin
        w_TP := TTextParsing.Create(Code.Text, tsPascal);
        i := 0;
        while i < w_TP.TokenCount do
          begin
            p := w_TP.Tokens[i];
            if (p^.tokenType = ttAscii) then
              begin
                nHash.IncValue(p^.Text, 1);
                n := nHash.HashList.KeyData[p^.Text]^.OriginName;
                nComp := p^.Text <> n;
                Result := Result or nComp;
                if nComp then
                  begin
                    Inc(fixedWord_Counter);
                    p^.Text := n;
                  end;
              end;
            Inc(i);
          end;
        w_TP.RebuildToken;
        u_TP := TTextParsing.Create(w_TP.ParsingData.Text, tsPascal, nil, TPascalString(SpacerSymbol.V).DeleteChar('.'));
      end
    else
      begin
        u_TP := TTextParsing.Create(Code.Text, tsPascal, nil, TPascalString(SpacerSymbol.V).DeleteChar('.'));
      end;

    i := 0;
    State := psUnknow;
    while i < u_TP.TokenCount do
      begin
        p := u_TP.Tokens[i];
        if p^.tokenType = ttComment then
          begin
            I_Name := p^.Text;
            if umlMultipleMatch('{*}', I_Name) then
              begin
                I_Name.DeleteFirst;
                I_Name.DeleteLast;
                N2 := I_Name.TrimChar(#32#9);
                if umlMultipleMatch(['$I *', '$INCLUDE *'], N2) then
                  begin
                    I_Name := umlDeleteFirstStr(I_Name, ' ');
                    prefix := umlDeleteLastStr(I_Name, '/\');
                    I_Name := umlGetLastStr(I_Name, '/\');

                    if uHash.Exists(I_Name) then
                      begin
                        n := uHash.KeyData[I_Name]^.OriginName;
                        nComp := (I_Name <> n) or (umlMultipleMatch('$I *', N2));
                        if nComp then
                          begin
                            if prefix.Len > 0 then
                                prefix := prefix.ReplaceChar('/', '\') + '\';

                            I_Name := PFormat('{$INCLUDE %s}', [prefix.Text + n.Text]);
                            DoStatus('fixed Include : "%s" -> "%s"', [p^.Text.Text, I_Name.Text]);
                            p^.Text := I_Name;
                            Inc(fixedUName_Counter);
                          end;
                        Result := Result or nComp;
                      end
                    else
                      begin
                        uHash.Add(I_Name, nil, False);
                        if umlMultipleMatch('$I *', N2) then
                          begin
                            Result := True;
                            n := I_Name;
                            if prefix.Len > 0 then
                                prefix := prefix.ReplaceChar('/', '\') + '\';

                            I_Name := PFormat('{$INCLUDE %s}', [prefix.Text + n.Text]);
                            DoStatus('fixed Include : "%s" -> "%s"', [p^.Text.Text, I_Name.Text]);
                            p^.Text := I_Name;
                            Inc(fixedUName_Counter);
                          end;
                      end;
                  end;
              end;
          end
        else
          case State of
            psUnit: if (p^.tokenType = ttAscii) then
                begin
                  nComp := p^.Text <> un;
                  Result := Result or nComp;
                  if nComp then
                    begin
                      DoStatus('fixed unit (%s) define:%s -> %s', [umlGetFileName(fn).Text, p^.Text.Text, un]);
                      Inc(fixedUName_Counter);
                      p^.Text := un;
                    end;
                end
              else if (p^.tokenType = ttSymbol) and (p^.Text = ';') then
                  State := psUnknow;
            psUses: if (p^.tokenType = ttAscii) then
                begin
                  if (not p^.Text.Same('in')) then
                    begin
                      if uHash.Exists(p^.Text) then
                        begin
                          n := uHash.KeyData[p^.Text]^.OriginName;
                          nComp := p^.Text <> n;
                          Result := Result or nComp;
                          if nComp then
                            begin
                              DoStatus('fixed unit (%s) define: %s -> %s', [umlGetFileName(fn).Text, p^.Text.Text, n.Text]);
                              Inc(fixedUName_Counter);
                              p^.Text := n;
                            end;
                        end
                      else
                        uHash.Add(p^.Text, nil, False);
                    end;
                end
              else if (p^.tokenType = ttSymbol) and (p^.Text = ';') then
                  State := psUnknow;
            psUnknow: if (p^.tokenType = ttAscii) then
                begin
                  if (p^.Text.Same('unit')) then
                      State := psUnit
                  else if (p^.Text.Same('program')) then
                      State := psUnit
                  else if (p^.Text.Same('library')) then
                      State := psUnit
                  else if (p^.Text.Same('uses')) then
                      State := psUses;
                end;
          end;
        Inc(i);
      end;

    if Result then
      begin
        DoStatus('rebuild code %s...', [umlGetFileName(fn).Text]);
        u_TP.RebuildToken;
        Code.Text := u_TP.ParsingData.Text.Text;
        Code.SaveToFile(fn);
      end;

    DisposeObject([Code, u_TP, w_TP]);
  end;

  function ListSortCompare(const p1, p2: PListPascalStringData): Integer;
  begin
    Result := CompareValue(Integer(nHash[p2^.Data.Text]), Integer(nHash[p1^.Data.Text]));
  end;

  procedure QuickSortList(var SortList: TCoreClassPointerList; L, R: Integer); inline;
  var
    i, J: Integer;
    p, T: Pointer;
  begin
    repeat
      i := L;
      J := R;
      p := SortList[(L + R) shr 1];
      repeat
        while ListSortCompare(SortList[i], p) < 0 do
            Inc(i);
        while ListSortCompare(SortList[J], p) > 0 do
            Dec(J);
        if i <= J then
          begin
            if i <> J then
              begin
                T := SortList[i];
                SortList[i] := SortList[J];
                SortList[J] := T;
              end;
            Inc(i);
            Dec(J);
          end;
      until i > J;
      if L < J then
          QuickSortList(SortList, L, J);
      L := i;
    until i >= R;
  end;

var
  i: Integer;
  nList: TListPascalString;

begin
  FixedButton.Enabled := False;
  AddFileButton.Enabled := False;
  FileListMemo.Enabled := False;

  uHash := THashList.CustomCreate(8192);
  nHash := THashVariantList.CustomCreate($FFFF);

  ProgressBar.Max := FileListMemo.Lines.Count - 1;
  ProgressBar.Position := 0;

  if FixedWordCheckBox.Checked then
    begin
      nHash.IncValue('begin', 1);
      nHash.IncValue('end', 1);
      nHash.IncValue('index', 1);
      for i := 0 to WordDefineMemo.Lines.Count - 1 do
          nHash.IncValue(WordDefineMemo.Lines[i], 1);
    end;

  for i := 0 to FileListMemo.Lines.Count - 1 do
    if umlFileExists(FileListMemo.Lines[i]) then
      begin
        if umlMultipleMatch(['*.pas', '*.pp', '*.dpr'], FileListMemo.Lines[i]) then
            uHash.Add(umlChangeFileExt(umlGetFileName(FileListMemo.Lines[i]), ''), nil)
        else if umlMultipleMatch('*.inc', FileListMemo.Lines[i]) then
            uHash.Add(umlGetFileName(FileListMemo.Lines[i]), nil)
        else
            DoStatus('no support file: %s', [umlGetFileName(FileListMemo.Lines[i]).Text]);
      end;

  fixedUName_Counter := 0;
  fixedWord_Counter := 0;
  for i := 0 to FileListMemo.Lines.Count - 1 do
    if umlFileExists(FileListMemo.Lines[i]) then
      if umlMultipleMatch(['*.pas', '*.pp', '*.dpr', '*.inc'], FileListMemo.Lines[i]) then
        begin
          FixedUnit(FileListMemo.Lines[i], umlChangeFileExt(umlGetFileName(FileListMemo.Lines[i]), ''));
          ProgressBar.Position := i;
          StateLabel.Caption := Format('code files: %d, fixed Unit define: %d, fixed word : %d',
            [FileListMemo.Lines.Count, fixedUName_Counter, fixedWord_Counter]);
        end;

  if FixedWordCheckBox.Checked then
    begin
      DoStatus('build dict...');
      nList := TListPascalString.Create;
      nHash.GetNameList(nList);
      if nList.Count > 1 then
          QuickSortList(nList.List.ListData^, 0, nList.List.Count - 1);
      WordOutputMemo.Lines.BeginUpdate;
      nList.AssignTo(WordOutputMemo.Lines);
      WordOutputMemo.Lines.EndUpdate;
      DisposeObject(nList);
    end;

  ProgressBar.Position := 0;

  DoStatus('all done.');

  DisposeObject([uHash, nHash]);

  FixedButton.Enabled := True;
  AddFileButton.Enabled := True;
  FileListMemo.Enabled := True;
end;

procedure TPascalCodeUnificationForm.FixedWordCheckBoxClick(Sender: TObject);
var
  flag: Boolean;
begin
  flag := FixedWordCheckBox.Checked;
  dictInputInfoLabel.Visible := flag;
  WordDefineMemo.Visible := flag;
  dictOutputInfoLabel.Visible := flag;
  WordOutputMemo.Visible := flag;
end;

procedure TPascalCodeUnificationForm.DoStatusMethod(AText: SystemString; const ID: Integer);
begin
  StatusMemo.Lines.Add(AText);
  Application.ProcessMessages;
end;

end.
