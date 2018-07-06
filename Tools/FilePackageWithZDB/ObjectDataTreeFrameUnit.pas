unit ObjectDataTreeFrameUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, ComCtrls,
  UnicodeMixedLib,
  ObjectDataManager;

type
  TOpenObjectDataPath = procedure(_Path: string) of object;

  TObjectDataTreeFrame = class(TFrame)
    TreeView: TTreeView;
    procedure TreeViewExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
    procedure TreeViewChange(Sender: TObject; Node: TTreeNode);
    procedure TreeViewKeyUp(Sender: TObject; var key: Word; Shift: TShiftState);
  private
    DefaultFolderImageIndex: Integer;
    FCurrentObjectDataPath : string;
    FObjectDataEngine      : TObjectDataManager;
    FOnOpenObjectDataPath  : TOpenObjectDataPath;

    function GetObjectDataEngine: TObjectDataManager;
    procedure SetObjectDataEngine(const Value: TObjectDataManager);
    procedure SetCurrentObjectDataPath(const Value: string);

    function GetPathTreeNode(_Value, _Split: string; _TreeView: TTreeView; _RN: TTreeNode): TTreeNode;
    function GetNodeObjDataPath(_DestNode: TTreeNode; _Split: string): string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure UpdateFieldList(_OwnerNode: TTreeNode; _Path: string);
    procedure RefreshList;
    property CurrentObjectDataPath: string read FCurrentObjectDataPath write SetCurrentObjectDataPath;

    property ObjectDataEngine: TObjectDataManager read GetObjectDataEngine write SetObjectDataEngine;
    property OnOpenObjectDataPath: TOpenObjectDataPath read FOnOpenObjectDataPath write FOnOpenObjectDataPath;
  end;

implementation

{$R *.dfm}


procedure TObjectDataTreeFrame.TreeViewExpanding(Sender: TObject; Node: TTreeNode; var AllowExpansion: Boolean);
begin
  AllowExpansion := True;
  if Node.Count = 0 then
      UpdateFieldList(Node, GetNodeObjDataPath(Node, '/'));
end;

procedure TObjectDataTreeFrame.TreeViewChange(Sender: TObject; Node: TTreeNode);
begin
  if (Node.selected) and (not(Sender as TTreeView).IsEditing) then
    begin
      FCurrentObjectDataPath := GetNodeObjDataPath(Node, '/');
      if FCurrentObjectDataPath = '' then
          FCurrentObjectDataPath := '/';
      if Assigned(FOnOpenObjectDataPath) then
          FOnOpenObjectDataPath(FCurrentObjectDataPath);
    end;
end;

function TObjectDataTreeFrame.GetObjectDataEngine: TObjectDataManager;
begin
  Result := FObjectDataEngine;
end;

procedure TObjectDataTreeFrame.SetObjectDataEngine(const Value: TObjectDataManager);
var
  _RootNode: TTreeNode;
begin
  if FObjectDataEngine <> Value then
    begin
      FObjectDataEngine := Value;
    end;
  TreeView.Items.BeginUpdate;
  TreeView.Items.Clear;
  if FObjectDataEngine <> nil then
    begin
      _RootNode := TreeView.Items.AddFirst(nil, 'Root');
      with _RootNode do
        begin
          ImageIndex := DefaultFolderImageIndex;
          selectedIndex := DefaultFolderImageIndex;
          StateIndex := DefaultFolderImageIndex;
          selected := True;
          Data := nil;
        end;
      UpdateFieldList(_RootNode, '/');
    end;
  TreeView.Items.EndUpdate;
end;

procedure TObjectDataTreeFrame.SetCurrentObjectDataPath(const Value: string);
begin
  FCurrentObjectDataPath := Value;
  if ObjectDataEngine <> nil then
    begin
      if ObjectDataEngine.DirectoryExists(FCurrentObjectDataPath) then
        with GetPathTreeNode('/Root/' + Value, '/', TreeView, nil) do
            selected := True;
    end;
end;

function TObjectDataTreeFrame.GetPathTreeNode(_Value, _Split: string; _TreeView: TTreeView; _RN: TTreeNode): TTreeNode;
var
  Rep_Int : Integer;
  _Postfix: string;
begin
  _Postfix := umlGetFirstStr(_Value, _Split);
  if _Value = '' then
      Result := _RN
  else if _RN = nil then
    begin
      if _TreeView.Items.Count > 0 then
        begin
          for Rep_Int := 0 to _TreeView.Items.Count - 1 do
            begin
              if (_TreeView.Items[Rep_Int].Parent = _RN) and (umlMultipleMatch(True, _Postfix, _TreeView.Items[Rep_Int].Text)) then
                begin
                  Result := GetPathTreeNode(umlDeleteFirstStr(_Value, _Split), _Split, _TreeView, _TreeView.Items[Rep_Int]);
                  Result.Expand(False);
                  Exit;
                end;
            end;
        end;
      Result := _TreeView.Items.AddChild(_RN, _Postfix);
      with Result do
        begin
          ImageIndex := DefaultFolderImageIndex;
          StateIndex := -1;
          selectedIndex := DefaultFolderImageIndex;
          Data := nil;
        end;
      Result := GetPathTreeNode(umlDeleteFirstStr(_Value, _Split), _Split, _TreeView, Result);
    end
  else
    begin
      if (_RN.Count > 0) then
        begin
          for Rep_Int := 0 to _RN.Count - 1 do
            begin
              if (_RN.Item[Rep_Int].Parent = _RN) and (umlMultipleMatch(True, _Postfix, _RN.Item[Rep_Int].Text)) then
                begin
                  Result := GetPathTreeNode(umlDeleteFirstStr(_Value, _Split), _Split, _TreeView, _RN.Item[Rep_Int]);
                  Result.Expand(False);
                  Exit;
                end;
            end;
        end;
      Result := _TreeView.Items.AddChild(_RN, _Postfix);
      with Result do
        begin
          ImageIndex := DefaultFolderImageIndex;
          StateIndex := -1;
          selectedIndex := DefaultFolderImageIndex;
          Data := nil;
        end;
      Result := GetPathTreeNode(umlDeleteFirstStr(_Value, _Split), _Split, _TreeView, Result);
    end;
end;

function TObjectDataTreeFrame.GetNodeObjDataPath(_DestNode: TTreeNode; _Split: string): string;
begin
  if _DestNode.level > 0 then
      Result := GetNodeObjDataPath(_DestNode.Parent, _Split) + _Split + _DestNode.Text
  else
      Result := '';
end;

constructor TObjectDataTreeFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DefaultFolderImageIndex := -1;
  FObjectDataEngine := nil;
end;

destructor TObjectDataTreeFrame.Destroy;
begin
  inherited Destroy;
end;

procedure TObjectDataTreeFrame.UpdateFieldList(_OwnerNode: TTreeNode; _Path: string);
var
  _FieldSR: TFieldSearch;
  nd      : TTreeNode;
begin
  if ObjectDataEngine <> nil then
    begin
      if ObjectDataEngine.FieldFindFirst(_Path, '*', _FieldSR) then
        begin
          repeat
            nd := TreeView.Items.AddChild(_OwnerNode, _FieldSR.Name);
            with nd do
              begin
                HasChildren := ObjectDataEngine.FastFieldExists(_FieldSR.HeaderPOS, '*');
                ImageIndex := DefaultFolderImageIndex;
                selectedIndex := DefaultFolderImageIndex;
                StateIndex := DefaultFolderImageIndex;
                Data := nil;
              end;
            if nd.HasChildren then
                UpdateFieldList(nd, ObjectDataEngine.GetFieldPath(_FieldSR.HeaderPOS));
          until not ObjectDataEngine.FieldFindNext(_FieldSR);
        end;
    end;
end;

procedure TObjectDataTreeFrame.RefreshList;
var
  _N: string;
begin
  _N := CurrentObjectDataPath;
  SetObjectDataEngine(ObjectDataEngine);
  SetCurrentObjectDataPath(_N);
end;

procedure TObjectDataTreeFrame.TreeViewKeyUp(Sender: TObject; var key: Word; Shift: TShiftState);
begin
  case key of
    VK_F5:
      RefreshList;
  end;
end;

end. 
