object FilePackageWithZDBMainForm: TFilePackageWithZDBMainForm
  Left = 0
  Top = 0
  Caption = 'File Package.'
  ClientHeight = 430
  ClientWidth = 1082
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poDesktopCenter
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 304
    Width = 1082
    Height = 5
    Cursor = crVSplit
    Align = alBottom
    AutoSnap = False
    ResizeStyle = rsUpdate
    ExplicitTop = 389
    ExplicitWidth = 1167
  end
  object TopPanel: TPanel
    Left = 0
    Top = 0
    Width = 1082
    Height = 41
    Align = alTop
    BorderWidth = 5
    TabOrder = 0
    object Bevel3: TBevel
      Left = 303
      Top = 6
      Width = 10
      Height = 29
      Align = alLeft
      Shape = bsSpacer
      ExplicitLeft = 211
      ExplicitTop = 2
      ExplicitHeight = 39
    end
    object Bevel4: TBevel
      Left = 133
      Top = 6
      Width = 10
      Height = 29
      Align = alLeft
      Shape = bsSpacer
      ExplicitLeft = 38
      ExplicitTop = 8
      ExplicitHeight = 33
    end
    object Bevel5: TBevel
      Left = 193
      Top = 6
      Width = 10
      Height = 29
      Align = alLeft
      Shape = bsSpacer
      ExplicitLeft = 98
      ExplicitTop = 0
      ExplicitHeight = 33
    end
    object Bevel7: TBevel
      Left = 841
      Top = 6
      Width = 10
      Height = 29
      Align = alLeft
      Shape = bsSpacer
      ExplicitLeft = 637
      ExplicitTop = 0
      ExplicitHeight = 33
    end
    object Bevel8: TBevel
      Left = 606
      Top = 6
      Width = 10
      Height = 29
      Align = alLeft
      Shape = bsSpacer
      ExplicitLeft = 521
      ExplicitHeight = 21
    end
    object NewButton: TButton
      Left = 6
      Top = 6
      Width = 50
      Height = 29
      Align = alLeft
      Caption = 'New'
      TabOrder = 0
      OnClick = NewButtonClick
    end
    object OpenButton: TButton
      Left = 143
      Top = 6
      Width = 50
      Height = 29
      Align = alLeft
      Caption = 'Open'
      TabOrder = 2
      OnClick = OpenButtonClick
    end
    object SaveButton: TButton
      Left = 203
      Top = 6
      Width = 50
      Height = 29
      Align = alLeft
      Caption = 'Save'
      TabOrder = 3
      OnClick = SaveButtonClick
    end
    object SaveAsButton: TButton
      Left = 253
      Top = 6
      Width = 50
      Height = 29
      Align = alLeft
      Caption = 'Save as'
      TabOrder = 4
      OnClick = SaveAsButtonClick
    end
    object MD5Edit: TMemo
      Left = 616
      Top = 6
      Width = 225
      Height = 29
      Align = alLeft
      ParentColor = True
      TabOrder = 5
    end
    object CacheStateMemo: TMemo
      Left = 851
      Top = 6
      Width = 225
      Height = 29
      Align = alClient
      TabOrder = 7
    end
    object RecalcMD5Button: TButton
      Left = 313
      Top = 6
      Width = 36
      Height = 29
      Align = alLeft
      Caption = 'MD5'
      TabOrder = 8
      OnClick = RecalcMD5ButtonClick
    end
    object CompressAsButton: TButton
      Left = 349
      Top = 6
      Width = 70
      Height = 29
      Align = alLeft
      Caption = 'Build .OXC'
      TabOrder = 9
      OnClick = CompressAsButtonClick
    end
    object BuildIndexPackageButton: TButton
      Left = 489
      Top = 6
      Width = 117
      Height = 29
      Align = alLeft
      Caption = 'Build Index Package'
      TabOrder = 6
      OnClick = BuildIndexPackageButtonClick
    end
    object NewCustomButton: TButton
      Left = 56
      Top = 6
      Width = 77
      Height = 29
      Align = alLeft
      Caption = 'New Custom'
      TabOrder = 1
      OnClick = NewCustomButtonClick
    end
    object ParallelCompressAsButton: TButton
      Left = 419
      Top = 6
      Width = 70
      Height = 29
      Align = alLeft
      Caption = 'Build .OXP'
      TabOrder = 10
      OnClick = ParallelCompressAsButtonClick
    end
  end
  object Memo: TMemo
    Left = 0
    Top = 309
    Width = 1082
    Height = 121
    Align = alBottom
    BorderStyle = bsNone
    TabOrder = 1
    WordWrap = False
  end
  object OpenDialog: TOpenDialog
    Filter = 
      'all files(*.OX;*.OXC;*.OXP;*.ImgMat)|*.OX;*.OXC;*.OXP;*.ImgMat|O' +
      'bject Data(*.OX)|*.OX|Compressed Object Data(*.OXC)|*.OXC|Parall' +
      'el Compressed Object Data(*.OXP)|*.OXP|All(*.*)|*.*'
    Options = [ofPathMustExist, ofFileMustExist, ofShareAware, ofNoTestFileCreate, ofEnableSizing]
    Left = 56
    Top = 104
  end
  object SaveDialog: TSaveDialog
    DefaultExt = '.OX'
    Filter = 'Object Data(*.OX)|*.OX|All(*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 184
    Top = 112
  end
  object Timer: TTimer
    OnTimer = TimerTimer
    Left = 56
    Top = 160
  end
  object SaveAsCompressedDialog: TSaveDialog
    DefaultExt = '.OXC'
    Filter = 'Object Data(*.OXC)|*.OXC|All(*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 184
    Top = 160
  end
  object SaveAsParallelCompressedDialog: TSaveDialog
    DefaultExt = '.OXP'
    Filter = 'Object Data(*.OXP)|*.OXP|All(*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 184
    Top = 216
  end
end
