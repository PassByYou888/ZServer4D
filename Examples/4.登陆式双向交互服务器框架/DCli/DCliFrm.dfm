object AuthDoubleTunnelClientForm: TAuthDoubleTunnelClientForm
  Left = 0
  Top = 0
  Caption = 'Auth Double Tunnel Client'
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
  DesignSize = (
    634
    384)
  PixelsPerInch = 96
  TextHeight = 13
  object TimeLabel: TLabel
    Left = 32
    Top = 264
    Width = 47
    Height = 13
    Caption = 'TimeLabel'
  end
  object Memo1: TMemo
    Left = 136
    Top = 62
    Width = 481
    Height = 307
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'ZServer4D'#26159#19968#27454#26381#21153#22120#20013#38388#20214
      #27492'Demo'#20026#30331#24405#24335#21452#21521#27169#24335
      ''
      #30331#24405#35299#37322
      #21457#36215#36830#25509#26102#20505#24517#39035#36827#34892#36523#20221#39564#35777
      #22240#20026#30331#24405#23384#22312#29992#25143#36523#20221#65292#22312#30331#38470#30340#26381#21153#22120#31995#32479#20013#65292#27599#20010#29992#25143#37117#26377#33258#24049#30340#25991#20214#23384#20648#21644#25968
      #25454
      #23384#20648#31354#38388
      ''
      #21452#21521#27169#24335#35299#37322
      #26082#33021#30001#23458#25143#31471#20027#21160#21457#36865#21629#20196#21435#26381#21153#22120#25191#34892
      #20063#33021#30001#26381#21153#22120#20027#21160#21521#23458#25143#31471#21457#36215#21629#20196#25191#34892
      #22312#38142#25509#26426#21046#19978#20351#29992#20102#20004#20010#36890#36947#65292#19968#20010#29992#20110#25509#25910#65292#19968#20010#29992#20110#21457#36865
      #24403#20004#20010#36890#36947#37117#38142#25509#25104#21151#65292#20351#29992'TunnelLink'#26041#27861#21363#21487#23436#25104#21452#21521#26725#25509
      ''
      #30331#24405#24335#21452#21521#36890#35759#27169#24335#65292#30456#27604#27604#21333#32431#30340#21452#21521#24037#20316#27169#24335#22686#21152#20102#20197#19979#21151#33021
      #24322#27493#30331#24405#65288#23458#25143#31471#65289
      #22823#25991#20214#20256#36755#25903#25345#65288#26381#21153#22120#65292#23458#25143#31471#65289
      #22534#26632#24335#25351#20196#23384#20648#65288#26381#21153#22120#65289
      #23384#20648#31354#38388#21644#25968#25454#31649#29702#65288#26381#21153#22120#65289
      #25250#21344#24335#30331#24405#65288#26381#21153#22120#65289
      ''
      #21629#20196
      #21629#20196#31995#32479#21487#20197#26159#31616#21333#23383#31526#20018#21644#21508#31181#25968#25454#25171#21253#65292#20063#21487#20197#26159'Stream'#25171#21253
      #24635#20043#65292#20302#20110'500k'#30340#20219#20309#25968#25454#25910#21457#65292#21253#25324#23567#25991#20214#65292#37117#21487#20197#20351#29992#21629#20196
      ''
      #24403#25968#25454#38271#24230#38750#24120#22823#65292#22312'ZServer4D'#20013#65292#24517#39035#20351#29992'BigStream'#26426#21046#25165#33021#36827#34892#25910#21457
      ''
      #38468#38543#30340#23458#25143#31471#65292#21487#20197#26159#31227#21160#24179#21488#65292#21450#20010#20154#30005#33041#24179#21488)
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object ConnectButton: TButton
    Left = 32
    Top = 94
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
  object HelloWorldBtn: TButton
    Left = 32
    Top = 207
    Width = 89
    Height = 34
    Caption = 'hello world'
    TabOrder = 3
    OnClick = HelloWorldBtnClick
  end
  object UserEdit: TLabeledEdit
    Left = 136
    Top = 35
    Width = 97
    Height = 21
    EditLabel.Width = 22
    EditLabel.Height = 13
    EditLabel.Caption = 'User'
    LabelPosition = lpLeft
    TabOrder = 4
    Text = 'test'
  end
  object PasswdEdit: TLabeledEdit
    Left = 288
    Top = 35
    Width = 97
    Height = 21
    EditLabel.Width = 36
    EditLabel.Height = 13
    EditLabel.Caption = 'Passwd'
    LabelPosition = lpLeft
    TabOrder = 5
    Text = '123456'
  end
  object RegUserButton: TButton
    Left = 32
    Top = 53
    Width = 89
    Height = 35
    Caption = 'Reg user'
    TabOrder = 6
    OnClick = RegUserButtonClick
  end
  object AsyncConnectButton: TButton
    Left = 32
    Top = 135
    Width = 89
    Height = 35
    Caption = 'async connect'
    TabOrder = 7
    OnClick = AsyncConnectButtonClick
  end
  object fixedTimeButton: TButton
    Left = 32
    Top = 296
    Width = 89
    Height = 34
    Caption = 'Fixed time Sync'
    TabOrder = 8
    OnClick = fixedTimeButtonClick
  end
  object Timer1: TTimer
    Interval = 10
    OnTimer = Timer1Timer
    Left = 344
    Top = 48
  end
end
