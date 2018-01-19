object ObjectDataTreeFrame: TObjectDataTreeFrame
  Left = 0
  Top = 0
  Width = 451
  Height = 304
  Align = alClient
  TabOrder = 0
  object TreeView: TTreeView
    Left = 0
    Top = 0
    Width = 451
    Height = 304
    Align = alClient
    BorderStyle = bsNone
    HideSelection = False
    Indent = 19
    ReadOnly = True
    RightClickSelect = True
    RowSelect = True
    TabOrder = 0
    OnChange = TreeViewChange
    OnExpanding = TreeViewExpanding
    OnKeyUp = TreeViewKeyUp
  end
end
