object ObjectDataManagerFrame: TObjectDataManagerFrame
  Left = 0
  Top = 0
  Width = 810
  Height = 431
  TabOrder = 0
  object Splitter: TSplitter
    Left = 185
    Top = 0
    Height = 431
    ExplicitLeft = 200
    ExplicitTop = 64
    ExplicitHeight = 100
  end
  object ListView: TListView
    Left = 188
    Top = 0
    Width = 622
    Height = 431
    Align = alClient
    BorderStyle = bsNone
    Columns = <
      item
        AutoSize = True
        Caption = 'Name'
      end
      item
        Caption = 'Property'
        Width = 80
      end
      item
        Caption = 'Size'
        Width = 100
      end
      item
        Alignment = taRightJustify
        Caption = 'Date'
        Width = 100
      end
      item
        Caption = 'Time'
        Width = 100
      end>
    ColumnClick = False
    DoubleBuffered = True
    HideSelection = False
    IconOptions.AutoArrange = True
    MultiSelect = True
    RowSelect = True
    ParentDoubleBuffered = False
    PopupMenu = PopupMenu
    TabOrder = 0
    ViewStyle = vsReport
    OnEdited = ListViewEdited
    OnEditing = ListViewEditing
    OnKeyUp = ListViewKeyUp
    ExplicitWidth = 422
    ExplicitHeight = 316
  end
  object TreePanel: TPanel
    Left = 0
    Top = 0
    Width = 185
    Height = 431
    Align = alLeft
    BevelOuter = bvNone
    PopupMenu = PopupMenu
    TabOrder = 1
    ExplicitLeft = -3
    ExplicitHeight = 316
  end
  object ActionList: TActionList
    Left = 224
    Top = 80
    object ActionCreateDir: TAction
      Caption = 'New Directory...'
      OnExecute = ActionCreateDirExecute
    end
    object ActionRemove: TAction
      Caption = 'Delete...'
      OnExecute = ActionRemoveExecute
    end
    object ActionAddResource: TAction
      Caption = 'Import file...'
      OnExecute = ActionAddResourceExecute
    end
    object ActionRename: TAction
      Caption = 'Rename...'
      OnExecute = ActionRenameExecute
    end
    object ActionExport: TAction
      Caption = 'Export...'
      OnExecute = ActionExportExecute
    end
  end
  object OpenDialog: TOpenDialog
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 224
    Top = 128
  end
  object PopupMenu: TPopupMenu
    Left = 336
    Top = 128
    object CreateDirectory1: TMenuItem
      Action = ActionCreateDir
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object ImportFile1: TMenuItem
      Action = ActionAddResource
    end
    object ExportTo1: TMenuItem
      Action = ActionExport
    end
    object Remove1: TMenuItem
      Action = ActionRemove
    end
    object Rename1: TMenuItem
      Action = ActionRename
    end
  end
end
