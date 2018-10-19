object DoubleServerForm: TDoubleServerForm
  Left = 0
  Top = 0
  Caption = 'Double Tunnel Server'
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
    Left = 144
    Top = 40
    Width = 457
    Height = 241
    Lines.Strings = (
      'ZServer4D'#26159#19968#27454#26381#21153#22120#20013#38388#20214
      #27492'Demo'#20026#21452#21521#27169#24335
      ''
      #21452#21521#27169#24335#35299#37322
      #26082#33021#30001#23458#25143#31471#20027#21160#21457#36865#21629#20196#21435#26381#21153#22120#25191#34892
      #20063#33021#30001#26381#21153#22120#20027#21160#21521#23458#25143#31471#21457#36215#21629#20196#25191#34892
      #22312#38142#25509#26426#21046#19978#20351#29992#20102#20004#20010#36890#36947#65292#19968#20010#29992#20110#25509#25910#65292#19968#20010#29992#20110#21457#36865
      #24403#20004#20010#36890#36947#37117#38142#25509#25104#21151#65292#20351#29992'TunnelLink'#26041#27861#21363#21487#23436#25104#21452#21521#26725#25509
      ''
      #21629#20196
      #21629#20196#31995#32479#21487#20197#26159#31616#21333#23383#31526#20018#21644#21508#31181#25968#25454#25171#21253#65292#20063#21487#20197#26159'Stream'#25171#21253
      #24635#20043#65292#20302#20110'500k'#30340#20219#20309#25968#25454#25910#21457#65292#21253#25324#23567#25991#20214#65292#37117#21487#20197#20351#29992#21629#20196
      ''
      #24403#25968#25454#38271#24230#38750#24120#22823#65292#22312'ZServer4D'#20013#65292#24517#39035#20351#29992'BigStream'#26426#21046#25165#33021#36827#34892#25910#21457
      ''
      #38468#38543#30340#23458#25143#31471#65292#21487#20197#26159#31227#21160#24179#21488#65292#21450#20010#20154#30005#33041#24179#21488
      '')
    TabOrder = 0
  end
  object StartServiceButton: TButton
    Left = 32
    Top = 38
    Width = 89
    Height = 35
    Caption = 'start service'
    TabOrder = 1
    OnClick = StartServiceButtonClick
  end
  object ChangeCaptionButton: TButton
    Left = 32
    Top = 104
    Width = 89
    Height = 33
    Caption = 'Change Caption'
    TabOrder = 2
    OnClick = ChangeCaptionButtonClick
  end
  object GetClientValueButton: TButton
    Left = 32
    Top = 143
    Width = 89
    Height = 33
    Caption = 'GetClient Value'
    TabOrder = 3
    OnClick = GetClientValueButtonClick
  end
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 360
    Top = 8
  end
end
