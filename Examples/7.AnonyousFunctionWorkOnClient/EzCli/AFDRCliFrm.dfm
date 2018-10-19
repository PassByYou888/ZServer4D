object DRClientForm: TDRClientForm
  Left = 0
  Top = 0
  Caption = 'DRClient'
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
    Left = 127
    Top = 35
    Width = 457
    Height = 241
    Lines.Strings = (
      'ZServer4D'#26159#19968#27454#26381#21153#22120#20013#38388#20214
      #27492'Demo'#28436#31034#20102#20351#29992#21311#21517#20989#25968#23454#29616#24310#36831#21709#24212#30340#22788#29702#26426#21046
      ''
      ' DelayResponse'#21629#20196#34987#25191#34892#23436#25104#21518#65292#19981#20250#31435#21363#32473#23458#25143#31471#21453#39304
      ' '#24310#36831#21709#24212#26426#21046#37117#37319#29992#29366#24577#26426#23454#29616#65292#19968#26086#20572#27490#21709#24212#65292#38431#21015#20013#30340#25351#20196#37117#20250#22788#20110#31561#24453#29366#24577
      #24310#36831#26426#21046#20027#35201#29992#20110#36328#26381#36890#35759#25110#38750#32447#24615#27969#31243
      ''
      #38468#38543#30340#23458#25143#31471#65292#21487#20197#26159#31227#21160#24179#21488#65292#21450#20010#20154#30005#33041#24179#21488
      ''
      '')
    TabOrder = 0
  end
  object ConnectButton: TButton
    Left = 32
    Top = 38
    Width = 89
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
  object DelayResponseBtn: TButton
    Left = 32
    Top = 95
    Width = 89
    Height = 34
    Caption = 'Send Request'
    TabOrder = 3
    OnClick = DelayResponseBtnClick
  end
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 344
    Top = 16
  end
end
