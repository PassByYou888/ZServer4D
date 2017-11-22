unit Ics.Fmx.OverbyteIcsCharsetComboBox;

// March 2015 - removed sorting due to XE8 making FListBox private

interface

{$I Include\OverbyteIcsDefs.inc}

uses
  System.Classes, FMX.Types, FMX.ListBox,
  OverbyteIcsCharsetUtils;

type
  TIcsCharsetComboBox = class(TComboBox)
  private
    procedure ListBoxMouseLeave(Sender: TObject);
  protected
    FCharset      : String;
    FUserFriendly : Boolean;
    //FOnChange     : TNotifyEvent;
    FIncludeList  : TMimeCharsets;
    procedure PopulateItems; virtual;
    function  GetCharSet: String; virtual;
    procedure SetCharset(const Value: String); virtual;
    procedure SetIncludeList(const Value: TMimeCharsets); virtual;
    procedure SetUserFriendly(const Value: Boolean); virtual;
  {$IFDEF COMPILER17_UP}
    procedure DoChange; override;
  {$ELSE}
    procedure Change; override;
  {$ENDIF}
    //procedure TriggerChange; virtual;
    procedure Click; override;
  public
    //property PlacementRectangle;
    constructor Create(AOwner: TComponent); override;
    function IsCharsetSupported: Boolean;
    function GetCodePageDef: LongWord;
    function GetCodePage: LongWord;
    property IncludeList: TMimeCharsets read FIncludeList write SetIncludeList stored False;
  published
    property CharSet: String read GetCharSet write SetCharset;
    property UserFriendly : Boolean read FUserFriendly write SetUserFriendly default True;
  end;


implementation


{ TIcsCharsetComboBox }

procedure TIcsCharsetComboBox.ListBoxMouseLeave(Sender: TObject);
begin
  {$IFDEF COMPILER17_UP}
    DoChange;
  {$ELSE}
    Change;
  {$ENDIF}
end;

{$IFDEF COMPILER17_UP}
procedure TIcsCharsetComboBox.DoChange;
{$ELSE}
procedure TIcsCharsetComboBox.Change;
{$ENDIF}
var
    ACharSet: String;
begin
    ACharSet := GetCharSet;
    if FCharset <> ACharSet then
    begin
        FCharset := ACharSet;
        inherited;
    end;
end;

procedure TIcsCharsetComboBox.Click;
begin
   { if ItemIndex >= 0 then
        Charset := GetCharSet; }
    inherited Click;
end;

type
  //THackCustomListBox = class(TCustomListBox);
  THackListBox = class(TListBox);

constructor TIcsCharsetComboBox.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
//    Sorted        := True;
//    THackListBox(FListBox).Sorted := True;
    OnMouseLeave := ListBoxMouseLeave;
//   THackListBox(FListBox).OnMouseLeave := ListBoxMouseLeave;
    FUserFriendly := True;
    IncludeList  := [
    WIN_1250, WIN_1251, WIN_1252, WIN_1253, WIN_1254, WIN_1255, WIN_1256,
    WIN_1257, WIN_1258, ISO_8859_1, ISO_8859_2, ISO_8859_4, ISO_8859_5,
    ISO_8859_6, ISO_8859_7, ISO_8859_8, ISO_8859_8_i, ISO_8859_9, ISO_8859_13,
    ISO_8859_15, BIG_5, EUC_KR, GB_18030, GB_2312, HZ_GB_2312, KOI8_R, KOI8_U,
    KOREAN_HANGUL, SHIFT_JIS, UTF_8, WIN_874];
    Charset := '';
end;

function TIcsCharsetComboBox.GetCharSet: String;
{var
    I : Integer;}
begin
    if ItemIndex >= 0 then
        //Result := CodePageToMimeCharsetString(PLongWord(Items.Objects[ItemIndex])^)
        Result := CodePageToMimeCharsetString(ListBox.ItemByIndex(ItemIndex).Tag)
    else
        Result := FCharset;
end;

{ Returns the code page ID < $FFFF on success or ERR_CP_NOTMAPPED or        }
{ ERR_CP_NOTAVAILABLE on failure.                                           }
function TIcsCharsetComboBox.GetCodePage: LongWord;
begin
    MimeCharsetToCodePage(FCharset, Result);
end;

function TIcsCharsetComboBox.GetCodePageDef: LongWord;
begin
    Result := MimeCharsetToCodePageDef(FCharset);
end;

function TIcsCharsetComboBox.IsCharsetSupported: Boolean;
var
    CodePage : LongWord;
begin
    Result := MimeCharsetToCodePage(FCharset, CodePage);
end;

procedure TIcsCharsetComboBox.PopulateItems;
var
    I: Integer;
    LItems : TStringList;
    Item : TListBoxItem;
begin
    if FUpdating > 0 then Exit;
    if csDestroying in ComponentState then Exit;

    Items.BeginUpdate;
    try
        Clear;
        LItems := TStringList.Create;
        try
            if FUserFriendly then
                GetFriendlyCharsetList(LItems, FIncludeList, False)
            else
                GetMimeCharsetList(LItems, FIncludeList, False);

//            THackListBox(FListbox).FItemIndex := -1;
            for I := 0 to LItems.Count -1 do
            begin
                Item := TListBoxItem.Create(nil);
                Item.Parent := Self;
                Item.Stored := False;
                Item.Locked := True;
                Item.Text   := LItems[I];
                Item.Tag    := PLongWord(LItems.Objects[I])^;
            end;
//            THackListBox(FListBox).SortItems;
        finally
          LItems.Free;
        end;
    finally
        Items.EndUpdate;
    end;
end;

procedure TIcsCharsetComboBox.SetCharset(const Value: String);
var
    Item         : Integer;
    CurCodePage  : LongWord;
    PCsInfo      : PCharsetInfo;
begin
    if (ItemIndex < 0) or (Value <> FCharset) then
    begin
        if Value = '' then
            PCsInfo := GetMimeInfo(IcsSystemCodePage)
        else
            PCsInfo := GetMimeInfo(Value);
        if Assigned(PCsInfo) then
        begin
            if Value <> '' then    // Preserve alias names
                FCharSet := Value
            else
                FCharSet := ExtractMimeName(PCsInfo);
            CurCodePage := PCsInfo^.CodePage;
            { Change selected }
            for Item := 0 to ListBox.Items.Count -1 do
            begin
                if CurCodePage = LongWord(ListBox.ItemByIndex(Item).Tag) then
                begin
                    if (ItemIndex <> Item) then
                        ItemIndex := Item;
                    Exit;
                end;
            end;
        end
        else
            FCharset := Value;
            ItemIndex := -1;
    end;
end;

procedure TIcsCharsetComboBox.SetIncludeList(const Value: TMimeCharsets);
begin
    if Value <> FIncludeList then
    begin
        FIncludeList := Value;
        PopulateItems;
    end;
end;

procedure TIcsCharsetComboBox.SetUserFriendly(const Value: Boolean);
var
    LOldCharset: string;
begin
    if Value <> FUserFriendly then
    begin
        FUserFriendly := Value;
        PopulateItems;
        LOldCharset := FCharSet;
      {$IFDEF COMPILER17_UP}
        FCharSet := '';
      {$ENDIF}
        SetCharSet(LOldCharset);
    end;
end;

initialization
  RegisterFmxClasses([TIcsCharsetComboBox]);

end.
