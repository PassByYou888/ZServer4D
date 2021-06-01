object CardForm: TCardForm
  Left = 0
  Top = 0
  Caption = 'Card'
  ClientHeight = 518
  ClientWidth = 588
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PopupMode = pmExplicit
  PopupParent = InternetCliForm.Owner
  Position = poMainFormCenter
  OnCreate = FormCreate
  DesignSize = (
    588
    518)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 19
    Top = 62
    Width = 47
    Height = 13
    Caption = 'comment:'
  end
  object Label2: TLabel
    Left = 29
    Top = 183
    Width = 37
    Height = 13
    Caption = 'Picture:'
  end
  object NameEdit: TLabeledEdit
    Left = 72
    Top = 8
    Width = 121
    Height = 21
    EditLabel.Width = 31
    EditLabel.Height = 13
    EditLabel.Caption = 'Name:'
    LabelPosition = lpLeft
    TabOrder = 0
  end
  object PhoneEdit: TLabeledEdit
    Left = 72
    Top = 35
    Width = 145
    Height = 21
    EditLabel.Width = 34
    EditLabel.Height = 13
    EditLabel.Caption = 'Phone:'
    LabelPosition = lpLeft
    TabOrder = 1
  end
  object commentMemo: TMemo
    Left = 72
    Top = 62
    Width = 289
    Height = 115
    ScrollBars = ssBoth
    TabOrder = 2
    WordWrap = False
  end
  object PicturePanel: TPanel
    Left = 72
    Top = 183
    Width = 486
    Height = 257
    Anchors = [akLeft, akTop, akRight, akBottom]
    BorderStyle = bsSingle
    TabOrder = 3
    TabStop = True
    object Image: TImage
      Left = 1
      Top = 1
      Width = 480
      Height = 251
      Align = alClient
      ParentShowHint = False
      Proportional = True
      ShowHint = False
      ExplicitLeft = 56
      ExplicitTop = 80
      ExplicitWidth = 105
      ExplicitHeight = 105
    end
    object SetPictureButton: TButton
      Left = 414
      Top = 1
      Width = 67
      Height = 25
      Caption = 'Set Picture'
      TabOrder = 0
      OnClick = SetPictureButtonClick
    end
  end
  object SaveButton: TButton
    Left = 72
    Top = 463
    Width = 89
    Height = 33
    Anchors = [akLeft, akBottom]
    Caption = 'Save'
    TabOrder = 4
    OnClick = SaveButtonClick
  end
  object CloseButton: TButton
    Left = 167
    Top = 463
    Width = 75
    Height = 33
    Anchors = [akLeft, akBottom]
    Caption = 'Close'
    TabOrder = 5
    OnClick = CloseButtonClick
  end
  object OpenPictureDialog: TOpenPictureDialog
    Left = 416
    Top = 215
  end
end
