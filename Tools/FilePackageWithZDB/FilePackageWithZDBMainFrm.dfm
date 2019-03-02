object FilePackageWithZDBMainForm: TFilePackageWithZDBMainForm
  Left = 0
  Top = 0
  Caption = 'File Package.'
  ClientHeight = 513
  ClientWidth = 1167
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 389
    Width = 1167
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    AutoSnap = False
    ExplicitTop = 41
    ExplicitWidth = 407
  end
  object TopPanel: TPanel
    Left = 0
    Top = 0
    Width = 1167
    Height = 57
    Align = alTop
    BorderWidth = 5
    TabOrder = 0
    object Bevel1: TBevel
      Left = 176
      Top = 6
      Width = 10
      Height = 45
      Align = alLeft
      Shape = bsSpacer
      ExplicitLeft = 151
      ExplicitTop = 1
      ExplicitHeight = 39
    end
    object Bevel2: TBevel
      Left = 364
      Top = 6
      Width = 10
      Height = 45
      Align = alLeft
      Shape = bsSpacer
      ExplicitLeft = 339
      ExplicitTop = 1
      ExplicitHeight = 39
    end
    object Bevel3: TBevel
      Left = 236
      Top = 6
      Width = 10
      Height = 45
      Align = alLeft
      Shape = bsSpacer
      ExplicitLeft = 211
      ExplicitTop = 2
      ExplicitHeight = 39
    end
    object Bevel4: TBevel
      Left = 56
      Top = 6
      Width = 10
      Height = 45
      Align = alLeft
      Shape = bsSpacer
      ExplicitLeft = 38
      ExplicitTop = 8
      ExplicitHeight = 33
    end
    object Bevel5: TBevel
      Left = 116
      Top = 6
      Width = 10
      Height = 45
      Align = alLeft
      Shape = bsSpacer
      ExplicitLeft = 98
      ExplicitTop = 0
      ExplicitHeight = 33
    end
    object Bevel6: TBevel
      Left = 410
      Top = 6
      Width = 10
      Height = 45
      Align = alLeft
      Shape = bsSpacer
      ExplicitLeft = 408
      ExplicitTop = 4
      ExplicitHeight = 33
    end
    object Bevel7: TBevel
      Left = 645
      Top = 6
      Width = 10
      Height = 45
      Align = alLeft
      Shape = bsSpacer
      ExplicitLeft = 637
      ExplicitTop = 0
      ExplicitHeight = 33
    end
    object NewButton: TButton
      Left = 6
      Top = 6
      Width = 50
      Height = 45
      Align = alLeft
      Caption = 'New'
      TabOrder = 0
      OnClick = NewButtonClick
    end
    object OpenButton: TButton
      Left = 66
      Top = 6
      Width = 50
      Height = 45
      Align = alLeft
      Caption = 'Open'
      TabOrder = 1
      OnClick = OpenButtonClick
    end
    object SaveButton: TButton
      Left = 126
      Top = 6
      Width = 50
      Height = 45
      Align = alLeft
      Caption = 'Save'
      TabOrder = 2
      OnClick = SaveButtonClick
    end
    object SaveAsButton: TButton
      Left = 186
      Top = 6
      Width = 50
      Height = 45
      Align = alLeft
      Caption = 'Save as'
      TabOrder = 3
      OnClick = SaveAsButtonClick
    end
    object MD5Edit: TMemo
      Left = 420
      Top = 6
      Width = 225
      Height = 45
      Align = alLeft
      ParentColor = True
      TabOrder = 4
    end
    object CacheStateMemo: TMemo
      Left = 655
      Top = 6
      Width = 506
      Height = 45
      Align = alClient
      TabOrder = 5
    end
    object RecalcMD5Button: TButton
      Left = 374
      Top = 6
      Width = 36
      Height = 45
      Align = alLeft
      Caption = 'MD5'
      TabOrder = 6
      OnClick = RecalcMD5ButtonClick
    end
    object CompressAsButton: TButton
      Left = 246
      Top = 6
      Width = 118
      Height = 45
      Align = alLeft
      Caption = 'Save as .OXC'
      TabOrder = 7
      OnClick = CompressAsButtonClick
    end
  end
  object Memo: TMemo
    Left = 0
    Top = 392
    Width = 1167
    Height = 121
    Align = alBottom
    BorderStyle = bsNone
    TabOrder = 1
    WordWrap = False
  end
  object OpenDialog: TOpenDialog
    Filter = 
      'all files(*.OX;*.OXC)|*.OX;*.OXC|Object Data(*.OX)|*.OX|Compress' +
      'ed Object Data(*.OXC)|*.OXC|All(*.*)|*.*'
    Options = [ofPathMustExist, ofFileMustExist, ofShareAware, ofNoTestFileCreate, ofEnableSizing]
    Left = 56
    Top = 88
  end
  object SaveDialog: TSaveDialog
    DefaultExt = '.OX'
    Filter = 
      'Object Data(*.OX)|*.OX|Compressed Object Data(*.OXC)|*.OXC|All(*' +
      '.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 56
    Top = 160
  end
  object Timer: TTimer
    OnTimer = TimerTimer
    Left = 56
    Top = 232
  end
  object SaveAsCompressedDialog: TSaveDialog
    DefaultExt = '.OXC'
    Filter = 'Object Data(*.OXC)|*.OXC|All(*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 160
    Top = 160
  end
end
