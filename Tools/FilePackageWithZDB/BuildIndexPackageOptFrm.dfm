object BuildIndexPackageOptForm: TBuildIndexPackageOptForm
  Left = 0
  Top = 0
  AutoSize = True
  BorderStyle = bsDialog
  BorderWidth = 20
  Caption = 'build Index package.'
  ClientHeight = 97
  ClientWidth = 490
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object BrowseDestButton: TSpeedButton
    Left = 440
    Top = 0
    Width = 50
    Height = 22
    Caption = 'browse'
    OnClick = BrowseDestButtonClick
  end
  object BrowseDataPathButton: TSpeedButton
    Left = 344
    Top = 27
    Width = 57
    Height = 22
    Caption = 'browse'
    OnClick = BrowseDataPathButtonClick
  end
  object DestDBEdit: TLabeledEdit
    Left = 75
    Top = 0
    Width = 359
    Height = 21
    EditLabel.Width = 72
    EditLabel.Height = 13
    EditLabel.Caption = 'Dest package: '
    LabelPosition = lpLeft
    TabOrder = 0
  end
  object DataPathEdit: TLabeledEdit
    Left = 75
    Top = 27
    Width = 263
    Height = 21
    EditLabel.Width = 55
    EditLabel.Height = 13
    EditLabel.Caption = 'Data path: '
    LabelPosition = lpLeft
    TabOrder = 1
  end
  object OkButton: TButton
    Left = 3
    Top = 72
    Width = 75
    Height = 25
    Caption = '&OK'
    ModalResult = 1
    TabOrder = 2
  end
  object CancelButton: TButton
    Left = 84
    Top = 72
    Width = 75
    Height = 25
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object SaveDialog: TSaveDialog
    DefaultExt = '.OX'
    Filter = 'Object Data(*.OX)|*.OX|All(*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 176
    Top = 37
  end
end
