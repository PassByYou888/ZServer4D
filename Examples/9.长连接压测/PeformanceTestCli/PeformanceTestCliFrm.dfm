object EZClientForm: TEZClientForm
  Left = 0
  Top = 0
  Caption = 'PeformanceTest Client'
  ClientHeight = 179
  ClientWidth = 378
  Color = clBtnFace
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
  object ConnectButton: TButton
    Left = 56
    Top = 46
    Width = 113
    Height = 35
    Caption = 'build 2000 connect'
    TabOrder = 0
    OnClick = ConnectButtonClick
  end
  object HostEdit: TLabeledEdit
    Left = 136
    Top = 8
    Width = 121
    Height = 21
    EditLabel.Width = 65
    EditLabel.Height = 13
    EditLabel.Caption = 'host address '
    LabelPosition = lpLeft
    TabOrder = 1
    Text = '127.0.0.1'
  end
  object Button1: TButton
    Left = 56
    Top = 88
    Width = 113
    Height = 41
    Caption = 'test command'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Timer1: TTimer
    Interval = 10
    OnTimer = Timer1Timer
    Left = 208
    Top = 40
  end
end
