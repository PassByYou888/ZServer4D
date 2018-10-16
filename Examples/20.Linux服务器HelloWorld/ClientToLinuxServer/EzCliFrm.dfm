object EZClientForm: TEZClientForm
  Left = 0
  Top = 0
  Caption = 'EZClient'
  ClientHeight = 384
  ClientWidth = 634
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
  object Memo1: TMemo
    Left = 136
    Top = 38
    Width = 457
    Height = 241
    Lines.Strings = (
      'ZServer4D'#26159#19968#27454#26381#21153#22120#20013#38388#20214
      #27492'Demo'#21482#28436#31034#22522#26412#38142#25509#36890#35759#22788#29702
      ''
      #38468#38543#30340#23458#25143#31471#65292#21487#20197#26159#31227#21160#24179#21488#65292#21450#20010#20154#30005#33041#24179#21488)
    TabOrder = 0
  end
  object ConnectButton: TButton
    Left = 8
    Top = 38
    Width = 113
    Height = 35
    Caption = 'connect'
    TabOrder = 1
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
    TabOrder = 2
    Text = '127.0.0.1'
  end
  object HelloWorldBtn: TButton
    Left = 8
    Top = 95
    Width = 113
    Height = 34
    Caption = 'hello world'
    TabOrder = 3
    OnClick = HelloWorldBtnClick
  end
  object sendMiniStreamButton: TButton
    Left = 8
    Top = 152
    Width = 113
    Height = 33
    Caption = 'send mini Stream'
    TabOrder = 4
    OnClick = sendMiniStreamButtonClick
  end
  object SendBigStreamButton: TButton
    Left = 8
    Top = 200
    Width = 113
    Height = 33
    Caption = 'send Big Stream'
    TabOrder = 5
    OnClick = SendBigStreamButtonClick
  end
  object SendCompletebufferButton: TButton
    Left = 8
    Top = 246
    Width = 113
    Height = 33
    Caption = 'send Buffer(fast)'
    TabOrder = 6
    OnClick = SendCompletebufferButtonClick
  end
  object Timer1: TTimer
    Interval = 1
    OnTimer = Timer1Timer
    Left = 344
    Top = 16
  end
end
