unit ObjectDataManagerFrameUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, ActnList, Menus,
  ShellAPI, IOUtils,
  PascalStrings, ObjectData, ObjectDataManager, FileCtrl,
  ObjectDataTreeFrameUnit, ItemStream, System.Actions;

type
  TObjectDataManagerFrame = class(TFrame)
    Splitter: TSplitter;
    ListView: TListView;
    ActionList: TActionList;
    ActionCreateDir: TAction;
    ActionRemove: TAction;
    ActionAddResource: TAction;
    TreePanel: TPanel;
    OpenDialog: TOpenDialog;
    ActionRename: TAction;
    ActionExport: TAction;
    PopupMenu: TPopupMenu;
    CreateDirectory1: TMenuItem;
    Rename1: TMenuItem;
    Importfile1: TMenuItem;
    ExportTo1: TMenuItem;
    Remove1: TMenuItem;
    n3: TMenuItem;
    SaveDialog: TSaveDialog;
    Action_Open: TAction;
    Open1: TMenuItem;
    N1: TMenuItem;
    procedure ActionAddResourceExecute(Sender: TObject);
    procedure ActionCreateDirExecute(Sender: TObject);
    procedure ActionExportExecute(Sender: TObject);
    procedure ActionRemoveExecute(Sender: TObject);
    procedure ActionRenameExecute(Sender: TObject);
    procedure Action_OpenExecute(Sender: TObject);
    procedure ListViewEdited(Sender: TObject; Item: TListItem; var s: string);
    procedure ListViewEditing(Sender: TObject; Item: TListItem; var AllowEdit: Boolean);
    procedure ListViewKeyUp(Sender: TObject; var key: Word; Shift: TShiftState);
  private
    { Private declarations }
    FDefaultFolderImageIndex: Integer;
    FResourceData: TObjectDataManager;
    FResourceTreeFrame: TObjectDataTreeFrame;
    FIsModify: Boolean;

    FFileFilter: string;

    procedure SetResourceData(Value: TObjectDataManager);
    function GetCurrentObjectDataPath: string;
    procedure SetCurrentObjectDataPath(const Value: string);
    procedure OpenObjectDataPath(APath: string);

    procedure SetFileFilter(const Value: string);
    function GetMultiSelect: Boolean;
    procedure SetMultiSelect(const Value: Boolean);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateItemList(APath: string);

    procedure ExportToFile(aDBPath, aDBItem, aToDirectory, aToName: string; var aShowMsg: Boolean);
    procedure ImportFromFile(aFileName: string; var aShowMsg: Boolean);
    procedure ImportFromStreamData(aItemName: string; stream: TStream; var aShowMsg: Boolean);

    property ResourceData: TObjectDataManager read FResourceData write SetResourceData;

    property CurrentObjectDataPath: string read GetCurrentObjectDataPath write SetCurrentObjectDataPath;
    property ResourceTreeFrame: TObjectDataTreeFrame read FResourceTreeFrame write FResourceTreeFrame;
    property IsModify: Boolean read FIsModify write FIsModify;

    property fileFilter: string read FFileFilter write SetFileFilter;
    property MultiSelect: Boolean read GetMultiSelect write SetMultiSelect;
    property DefaultFolderImageIndex: Integer read FDefaultFolderImageIndex;
  end;

implementation

uses UnicodeMixedLib, DoStatusIO, LibraryManager, StreamList;

{$R *.dfm}


procedure TObjectDataManagerFrame.ActionAddResourceExecute(Sender: TObject);
var
  RepaInt: Integer;
  aShowMsg: Boolean;
begin
  if FResourceData = nil then
      Exit;
  if OpenDialog.Execute then
    begin
      if OpenDialog.Files.Count > 0 then
        begin
          aShowMsg := True;
          for RepaInt := 0 to OpenDialog.Files.Count - 1 do
              ImportFromFile(OpenDialog.Files[RepaInt], aShowMsg);
          UpdateItemList(CurrentObjectDataPath);
          FResourceTreeFrame.RefreshList;
          IsModify := True;
        end;
    end;
end;

procedure TObjectDataManagerFrame.ActionCreateDirExecute(Sender: TObject);
begin
  if FResourceData = nil then
      Exit;
  with ListView.Items.Add do
    begin
      Caption := '';
      ImageIndex := FDefaultFolderImageIndex;
      StateIndex := -1;
      Data := nil;
      EditCaption;
    end;
  IsModify := True;
end;

procedure TObjectDataManagerFrame.ActionExportExecute(Sender: TObject);
var
  aShowMsg: Boolean;
  RepaInt: Integer;
  aTargetDirectory: string;
begin
  if ListView.IsEditing then
      Exit;
  if FResourceData = nil then
      Exit;
  if ListView.SelCount = 1 then
    begin
      SaveDialog.FileName := ListView.Selected.Caption;
      if not SaveDialog.Execute() then
          Exit;
      aShowMsg := True;
      ExportToFile(CurrentObjectDataPath, ListView.Selected.Caption, umlGetFilePath(SaveDialog.FileName), umlGetFileName(SaveDialog.FileName), aShowMsg);
    end
  else
    begin
      if not SelectDirectory('export to', '', aTargetDirectory, [sdNewFolder, sdShowEdit, sdShowShares, sdNewUI]) then
          Exit;

      aShowMsg := True;
      for RepaInt := 0 to ListView.Items.Count - 1 do
        begin
          with ListView.Items[RepaInt] do
            begin
              if (Selected) or (ListView.SelCount = 0) then
                begin
                  if ImageIndex <> FDefaultFolderImageIndex then
                      ExportToFile(CurrentObjectDataPath, Caption, aTargetDirectory, Caption, aShowMsg);
                end;
            end;
        end;
    end;
end;

procedure TObjectDataManagerFrame.ActionRemoveExecute(Sender: TObject);
var
  RepaInt: Integer;
begin
  if ListView.IsEditing then
      Exit;
  if MessageDlg('remove?', mtWarning, [mbYes, mbNo], 0) <> mrYes then
      Exit;
  if FResourceData = nil then
      Exit;
  if ListView.SelCount > 0 then
    begin
      for RepaInt := 0 to ListView.Items.Count - 1 do
        begin
          with ListView.Items[RepaInt] do
            begin
              if Selected then
                begin
                  if ImageIndex = FDefaultFolderImageIndex then
                    begin
                      if FResourceData.FieldDelete(CurrentObjectDataPath, Caption) then
                          DoStatus(Format('delete Field "%s" success', [Caption]));
                    end
                  else if FResourceData.ItemDelete(CurrentObjectDataPath, Caption) then
                      DoStatus(Format('delete item "%s" success', [Caption]));
                end;
            end;
        end;
      UpdateItemList(CurrentObjectDataPath);
      FResourceTreeFrame.RefreshList;
      IsModify := True;
    end;
end;

procedure TObjectDataManagerFrame.ActionRenameExecute(Sender: TObject);
begin
  if ListView.IsEditing then
      Exit;
  if ListView.Selected <> nil then
      ListView.Selected.EditCaption;
end;

procedure TObjectDataManagerFrame.ListViewEdited(Sender: TObject; Item: TListItem; var s: string);
var
  aFieldPos: Int64;
  aItemHnd: TItemHandle;
begin
  if FResourceData = nil then
      Exit;
  if (Item.ImageIndex = FDefaultFolderImageIndex) and (Item.Caption = '') then
    begin
      if not FResourceData.CreateField(CurrentObjectDataPath + '/' + s, '') then
          Item.Free;
      DoStatus(Format('create new directory "%s"', [CurrentObjectDataPath + '/' + s]));
    end
  else if (Item.ImageIndex = FDefaultFolderImageIndex) and (FResourceData.GetPathField(CurrentObjectDataPath + '/' + Item.Caption, aFieldPos)) then
    begin
      DoStatus(Format('ReName directory "%s" to "%s" .', [Item.Caption, s]));
      if not FResourceData.FieldReName(aFieldPos, s, '') then
          Item.Free;
    end
  else if (FResourceData.GetPathField(CurrentObjectDataPath, aFieldPos)) then
    begin
      if FResourceData.ItemOpen(CurrentObjectDataPath, Item.Caption, aItemHnd) then
        begin
          DoStatus(Format('ReName Item "%s" to "%s" .', [Item.Caption, s]));
          if not FResourceData.ItemReName(aFieldPos, aItemHnd, s, '') then
              Item.Free;
        end;
    end;
  FResourceTreeFrame.RefreshList;
  UpdateItemList(CurrentObjectDataPath);
  IsModify := True;
end;

procedure TObjectDataManagerFrame.ListViewEditing(Sender: TObject; Item: TListItem; var AllowEdit: Boolean);
begin
  AllowEdit := True;
end;

procedure TObjectDataManagerFrame.ListViewKeyUp(Sender: TObject; var key: Word; Shift: TShiftState);
begin
  if (Sender as TListView).IsEditing then
      Exit;
  case key of
    VK_DELETE:
      ActionRemoveExecute(ActionRemove);
    VK_F5:
      FResourceTreeFrame.RefreshList;
    VK_F2:
      ActionRenameExecute(ActionRename);
  end;
end;

procedure TObjectDataManagerFrame.SetResourceData(Value: TObjectDataManager);
begin
  ListView.Items.BeginUpdate;
  ListView.Items.Clear;
  ListView.Items.EndUpdate;

  FResourceData := Value;
  FResourceTreeFrame.ObjectDataEngine := Value;
  FResourceTreeFrame.CurrentObjectDataPath := '/';
end;

function TObjectDataManagerFrame.GetCurrentObjectDataPath: string;
begin
  Result := FResourceTreeFrame.CurrentObjectDataPath;
end;

procedure TObjectDataManagerFrame.SetCurrentObjectDataPath(const Value: string);
begin
  FResourceTreeFrame.CurrentObjectDataPath := Value;
end;

procedure TObjectDataManagerFrame.OpenObjectDataPath(APath: string);
begin
  UpdateItemList(APath);
end;

procedure TObjectDataManagerFrame.SetFileFilter(const Value: string);
begin
  FFileFilter := Value;
  FResourceTreeFrame.RefreshList;
end;

function TObjectDataManagerFrame.GetMultiSelect: Boolean;
begin
  Result := ListView.MultiSelect;
end;

procedure TObjectDataManagerFrame.SetMultiSelect(const Value: Boolean);
begin
  ListView.MultiSelect := Value;
end;

constructor TObjectDataManagerFrame.Create(AOwner: TComponent);
begin
  inherited;
  FResourceData := nil;

  FResourceTreeFrame := TObjectDataTreeFrame.Create(nil);
  FResourceTreeFrame.Parent := TreePanel;
  FResourceTreeFrame.Align := alClient;
  FResourceTreeFrame.OnOpenObjectDataPath := OpenObjectDataPath;
  FResourceTreeFrame.CurrentObjectDataPath := '/';
  FResourceTreeFrame.ObjectDataEngine := nil;

  FDefaultFolderImageIndex := 100;
  FIsModify := False;

  FFileFilter := '*';
  MultiSelect := True;
end;

destructor TObjectDataManagerFrame.Destroy;
begin
  FResourceTreeFrame.Free;
  inherited;
end;

procedure TObjectDataManagerFrame.Action_OpenExecute(Sender: TObject);
var
  aShowMsg: Boolean;
  RepaInt: Integer;
  aTargetDirectory: string;
begin
  if ListView.IsEditing then
      Exit;
  if FResourceData = nil then
      Exit;
  if ListView.SelCount = 1 then
    begin
      aShowMsg := False;
      ExportToFile(CurrentObjectDataPath, ListView.Selected.Caption, TPath.GetTempPath, ListView.Selected.Caption, aShowMsg);

      ShellExecute(0, 'open', PWideChar(umlCombineFileName(TPath.GetTempPath, ListView.Selected.Caption).Text), '', PWideChar(TPath.GetTempPath), SW_SHOW);
    end;
end;

procedure TObjectDataManagerFrame.UpdateItemList(APath: string);
var
  aItemSR: TItemSearch;
  aFieldSR: TFieldSearch;
  Filter: TArrayPascalString;
begin
  umlGetSplitArray(FFileFilter, Filter, '|;');
  ListView.Items.BeginUpdate;
  ListView.Items.Clear;
  CurrentObjectDataPath := APath;
  if FResourceData <> nil then
    begin
      if FResourceData.FieldFindFirst(APath, '*', aFieldSR) then
        begin
          repeat
            with ListView.Items.Add do
              begin
                Caption := aFieldSR.Name;
                SubItems.Add('Field');
                SubItems.Add('Child : ' + umlIntToStr(aFieldSR.HeaderCount));
                ImageIndex := FDefaultFolderImageIndex;
                StateIndex := -1;
                Data := nil;
              end;
          until not FResourceData.FieldFindNext(aFieldSR);
        end;

      if FResourceData.ItemFindFirst(APath, '*', aItemSR) then
        begin
          repeat
            if umlMultipleMatch(Filter, aItemSR.Name) then
              begin
                with ListView.Items.Add do
                  begin
                    Caption := aItemSR.Name;
                    SubItems.Add(IntToHex(aItemSR.FieldSearch.RHeader.UserProperty, 8));
                    SubItems.Add(umlSizeToStr(aItemSR.Size));
                    SubItems.Add(DateTimeToStr(aItemSR.FieldSearch.RHeader.CreateTime));
                    SubItems.Add(DateTimeToStr(aItemSR.FieldSearch.RHeader.ModificationTime));
                    ImageIndex := -1;
                    StateIndex := -1;
                    Data := nil;
                  end;
              end;
          until not FResourceData.ItemFindNext(aItemSR);
        end;
    end;
  ListView.Items.EndUpdate;
end;

procedure TObjectDataManagerFrame.ExportToFile(aDBPath, aDBItem, aToDirectory, aToName: string; var aShowMsg: Boolean);
var
  aItemHnd: TItemHandle;
  s: TItemStream;
  aFS: TFileStream;
begin
  if FResourceData <> nil then
    begin
      if not umlDirectoryExists(aToDirectory) then
          umlCreateDirectory(aToDirectory);

      if (aShowMsg) and (umlFileExists(umlCombineFileName(aToDirectory, aToName))) then
        begin
          case MessageDlg(Format('File "%s" alread exists, overwirte?', [ExtractFilename(aToName)]), mtInformation, [mbYes, mbNo, mbAll], 0) of
            mrNo:
              Exit;
            mrAll:
              aShowMsg := False;
          end;
        end;
      if FResourceData.ItemOpen(aDBPath, aDBItem, aItemHnd) then
        begin
          s := TItemStream.Create(FResourceData, aItemHnd);
          aFS := TFileStream.Create(umlCombineFileName(aToDirectory, aToName), fmCreate);
          aFS.CopyFrom(s, s.Size);
          aFS.Free;
          s.Free;
          DoStatus('export file:%s', [umlCombineFileName(aToDirectory, aToName).Text]);
        end;
    end;
end;

procedure TObjectDataManagerFrame.ImportFromFile(aFileName: string; var aShowMsg: Boolean);
var
  aItemHnd: TItemHandle;
  aFS: TFileStream;
begin
  if FResourceData <> nil then
    begin
      if (aShowMsg) and (FResourceData.ItemExists(CurrentObjectDataPath, ExtractFilename(aFileName))) then
        begin
          case MessageDlg(Format('Item "%s" alread exists, overwirte?', [ExtractFilename(aFileName)]), mtInformation, [mbYes, mbNo, mbAll], 0) of
            mrNo:
              Exit;
            mrAll:
              aShowMsg := False;
          end;
        end;
      aFS := TFileStream.Create(aFileName, fmOpenRead);
      if FResourceData.ItemCreate(CurrentObjectDataPath, ExtractFilename(aFileName), '', aItemHnd) then
        begin
          with TItemStream.Create(FResourceData, aItemHnd) do
            begin
              CopyFrom(aFS, aFS.Size);
              CloseHandle;
              Free;
            end;
          DoStatus('import file:%s', [aItemHnd.Name.Text]);
        end;
      aFS.Free;
    end;
end;

procedure TObjectDataManagerFrame.ImportFromStreamData(aItemName: string; stream: TStream; var aShowMsg: Boolean);
var
  aItemHnd: TItemHandle;
begin
  if FResourceData <> nil then
    begin
      if (aShowMsg) and (FResourceData.ItemExists(CurrentObjectDataPath, aItemName)) then
        begin
          case MessageDlg(Format('Item "%s" alread exists, overwirte?', [aItemName]), mtInformation, [mbYes, mbNo, mbAll], 0) of
            mrNo:
              Exit;
            mrAll:
              aShowMsg := False;
          end;
        end;
      if FResourceData.ItemCreate(CurrentObjectDataPath, aItemName, '', aItemHnd) then
        begin
          stream.Position := 0;
          DoStatus(aItemHnd.Name);
          with TItemStream.Create(FResourceData, aItemHnd) do
            begin
              CopyFrom(stream, stream.Size);
              CloseHandle;
              Free;
            end;
          DoStatus('import stream', [aItemName]);
        end;
    end;
end;

initialization

finalization

end.
