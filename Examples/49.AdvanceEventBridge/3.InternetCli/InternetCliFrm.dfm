object InternetCliForm: TInternetCliForm
  Left = 0
  Top = 0
  AutoSize = True
  BorderStyle = bsDialog
  BorderWidth = 10
  Caption = 'Internet client. create by.qq600585'
  ClientHeight = 265
  ClientWidth = 817
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 0
    Top = 33
    Width = 18
    Height = 13
    Caption = 'log:'
  end
  object HostEdit: TLabeledEdit
    Left = 112
    Top = 2
    Width = 121
    Height = 21
    EditLabel.Width = 107
    EditLabel.Height = 13
    EditLabel.Caption = 'Internet Service Host:'
    LabelPosition = lpLeft
    TabOrder = 0
    Text = '127.0.0.1'
  end
  object goButton: TButton
    Left = 375
    Top = 0
    Width = 34
    Height = 25
    Caption = 'GO'
    TabOrder = 1
    OnClick = goButtonClick
  end
  object Memo: TMemo
    Left = 0
    Top = 52
    Width = 817
    Height = 213
    ScrollBars = ssBoth
    TabOrder = 2
    WordWrap = False
  end
  object newCardButton: TButton
    Left = 415
    Top = 0
    Width = 66
    Height = 25
    Caption = 'New Card'
    TabOrder = 3
    OnClick = newCardButtonClick
  end
  object DBNameEdit: TLabeledEdit
    Left = 296
    Top = 2
    Width = 73
    Height = 21
    EditLabel.Width = 50
    EditLabel.Height = 13
    EditLabel.Caption = 'Database:'
    LabelPosition = lpLeft
    TabOrder = 4
    Text = 'CardDB'
  end
  object QueryButton: TButton
    Left = 487
    Top = 0
    Width = 50
    Height = 25
    Caption = 'Query'
    TabOrder = 5
    OnClick = QueryButtonClick
  end
  object fpsTimer: TTimer
    Interval = 10
    OnTimer = fpsTimerTimer
    Left = 336
    Top = 40
  end
end
