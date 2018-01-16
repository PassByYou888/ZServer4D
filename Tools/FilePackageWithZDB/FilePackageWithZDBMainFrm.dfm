object FilePackageWithZDBMainForm: TFilePackageWithZDBMainForm
  Left = 0
  Top = 0
  Caption = 'File Package WithZDB...'
  ClientHeight = 513
  ClientWidth = 897
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object TopPanel: TPanel
    Left = 0
    Top = 0
    Width = 897
    Height = 41
    Align = alTop
    TabOrder = 0
    object Bevel1: TBevel
      Left = 151
      Top = 1
      Width = 10
      Height = 39
      Align = alLeft
      Shape = bsSpacer
    end
    object Bevel2: TBevel
      Left = 339
      Top = 1
      Width = 10
      Height = 39
      Align = alLeft
      Shape = bsSpacer
    end
    object Bevel3: TBevel
      Left = 211
      Top = 1
      Width = 10
      Height = 39
      Align = alLeft
      Shape = bsSpacer
      ExplicitTop = 2
    end
    object NewButton: TButton
      Left = 1
      Top = 1
      Width = 50
      Height = 39
      Align = alLeft
      Caption = 'New'
      TabOrder = 0
      OnClick = NewButtonClick
    end
    object OpenButton: TButton
      Left = 51
      Top = 1
      Width = 50
      Height = 39
      Align = alLeft
      Caption = 'Open'
      TabOrder = 1
      OnClick = OpenButtonClick
    end
    object SaveButton: TButton
      Left = 101
      Top = 1
      Width = 50
      Height = 39
      Align = alLeft
      Caption = 'Save'
      TabOrder = 2
      OnClick = SaveButtonClick
    end
    object SaveAsButton: TButton
      Left = 161
      Top = 1
      Width = 50
      Height = 39
      Align = alLeft
      Caption = 'Save as'
      TabOrder = 3
      OnClick = SaveAsButtonClick
    end
    object MD5Edit: TMemo
      Left = 385
      Top = 1
      Width = 225
      Height = 39
      Align = alLeft
      ParentColor = True
      TabOrder = 4
      ExplicitLeft = 333
    end
    object CacheStateMemo: TMemo
      Left = 610
      Top = 1
      Width = 286
      Height = 39
      Align = alClient
      TabOrder = 5
      ExplicitLeft = 558
      ExplicitWidth = 338
    end
    object RecalcMD5Button: TButton
      Left = 349
      Top = 1
      Width = 36
      Height = 39
      Align = alLeft
      Caption = 'MD5'
      TabOrder = 6
      OnClick = RecalcMD5ButtonClick
      ExplicitLeft = 297
    end
    object CompressAsButton: TButton
      Left = 221
      Top = 1
      Width = 118
      Height = 39
      Align = alLeft
      Caption = 'Save as Compressed'
      TabOrder = 7
      OnClick = CompressAsButtonClick
      ExplicitLeft = 211
    end
  end
  object Memo: TMemo
    Left = 0
    Top = 448
    Width = 897
    Height = 65
    Align = alBottom
    TabOrder = 1
    WordWrap = False
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '.OX'
    Filter = 
      'Object Data(*.OX)|*.OX|Compressed Object Data(*.OXC)|*.OXC|All(*' +
      '.*)|*.*'
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
