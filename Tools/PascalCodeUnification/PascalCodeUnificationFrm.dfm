object PascalCodeUnificationForm: TPascalCodeUnificationForm
  Left = 0
  Top = 0
  AutoSize = True
  BorderWidth = 15
  Caption = 'Pascal code Unification...'
  ClientHeight = 689
  ClientWidth = 1123
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object StateLabel: TLabel
    Left = 96
    Top = 8
    Width = 36
    Height = 13
    Caption = 'Info:...'
  end
  object dictInputInfoLabel: TLabel
    Left = 712
    Top = 16
    Width = 48
    Height = 13
    Caption = 'dict input:'
  end
  object dictOutputInfoLabel: TLabel
    Left = 920
    Top = 12
    Width = 56
    Height = 13
    Caption = 'dict output:'
  end
  object FileListMemo: TMemo
    Left = 0
    Top = 31
    Width = 705
    Height = 370
    ScrollBars = ssVertical
    TabOrder = 0
    WordWrap = False
  end
  object AddFileButton: TButton
    Left = 0
    Top = 0
    Width = 75
    Height = 25
    Caption = 'Add files...'
    TabOrder = 1
    OnClick = AddFileButtonClick
  end
  object FixedButton: TButton
    Left = 0
    Top = 441
    Width = 169
    Height = 33
    Caption = 'Process...'
    TabOrder = 2
    OnClick = FixedButtonClick
  end
  object StatusMemo: TMemo
    Left = 0
    Top = 480
    Width = 705
    Height = 209
    ScrollBars = ssVertical
    TabOrder = 3
  end
  object ProgressBar: TProgressBar
    Left = 175
    Top = 449
    Width = 370
    Height = 17
    TabOrder = 4
  end
  object WordDefineMemo: TMemo
    Left = 711
    Top = 31
    Width = 203
    Height = 658
    ScrollBars = ssVertical
    TabOrder = 5
    WordWrap = False
  end
  object WordOutputMemo: TMemo
    Left = 920
    Top = 31
    Width = 203
    Height = 658
    ScrollBars = ssVertical
    TabOrder = 6
    WordWrap = False
  end
  object FixedWordCheckBox: TCheckBox
    Left = 0
    Top = 418
    Width = 265
    Height = 17
    Caption = 'Unification code case with C Style...'
    Checked = True
    State = cbChecked
    TabOrder = 7
    OnClick = FixedWordCheckBoxClick
  end
  object OpenDialog: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'All Pascal Unit(*.pas;*.pp;*.inc;*.dpr)'
        FileMask = '*.pas;*.pp;*.inc;*.dpr'
      end>
    Options = [fdoAllowMultiSelect, fdoPathMustExist, fdoFileMustExist]
    Left = 184
    Top = 176
  end
end
