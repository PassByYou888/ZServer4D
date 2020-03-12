object ObjectDataManagerFrame: TObjectDataManagerFrame
  Left = 0
  Top = 0
  Width = 821
  Height = 416
  TabOrder = 0
  object Splitter: TSplitter
    Left = 185
    Top = 0
    Width = 5
    Height = 416
    AutoSnap = False
    ResizeStyle = rsUpdate
  end
  object ListView: TListView
    Left = 190
    Top = 0
    Width = 631
    Height = 416
    Align = alClient
    BorderStyle = bsNone
    Columns = <
      item
        AutoSize = True
        Caption = 'Name'
      end
      item
        Caption = 'Property'
        Width = 100
      end
      item
        Caption = 'Size'
        Width = 100
      end
      item
        Alignment = taRightJustify
        Caption = 'Time'
        Width = 120
      end
      item
        Caption = 'Modification Time'
        Width = 120
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
    OnDblClick = Action_OpenExecute
    OnEdited = ListViewEdited
    OnEditing = ListViewEditing
    OnKeyUp = ListViewKeyUp
  end
  object TreePanel: TPanel
    Left = 0
    Top = 0
    Width = 185
    Height = 416
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
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
    object ActionImportFile: TAction
      Caption = 'Import file...'
      OnExecute = ActionImportFileExecute
    end
    object ActionRename: TAction
      Caption = 'Rename...'
      OnExecute = ActionRenameExecute
    end
    object ActionExport: TAction
      Caption = 'Export...'
      OnExecute = ActionExportExecute
    end
    object Action_Open: TAction
      Caption = 'Open'
      OnExecute = Action_OpenExecute
    end
    object ActionImportDirectory: TAction
      Caption = 'Import Directory..'
      OnExecute = ActionImportDirectoryExecute
    end
  end
  object PopupMenu: TPopupMenu
    Left = 224
    Top = 224
    object Open1: TMenuItem
      Action = Action_Open
    end
    object ExportTo1: TMenuItem
      Action = ActionExport
    end
    object ImportFile1: TMenuItem
      Action = ActionImportFile
    end
    object ImportDirectory1: TMenuItem
      Action = ActionImportDirectory
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object CreateDirectory1: TMenuItem
      Action = ActionCreateDir
    end
    object Remove1: TMenuItem
      Action = ActionRemove
    end
    object Rename1: TMenuItem
      Action = ActionRename
    end
  end
  object SaveDialog: TSaveDialog
    Left = 224
    Top = 176
  end
  object OpenDialog: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = [fdoAllowMultiSelect, fdoPathMustExist, fdoFileMustExist]
    Left = 224
    Top = 128
  end
end
