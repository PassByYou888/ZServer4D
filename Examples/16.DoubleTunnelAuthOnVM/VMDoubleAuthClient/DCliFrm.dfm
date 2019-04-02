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
    Left = 8
    Top = 280
    Width = 47
    Height = 13
    Caption = 'TimeLabel'
  end
  object Memo1: TMemo
    Left = 112
    Top = 69
    Width = 481
    Height = 307
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'ZServer4D'#26159#19968#27454#26381#21153#22120#20013#38388#20214
      ''
      #38567#36947#23458#25143#31471#21407#29702':'
      #20808#36890#36807'VMTunnel'#24314#31435#24314#31435#65292#22312#21311#21517#20989#25968#20107#20214#20013#65292#33719#21462'VMTunnel'#34394#25311#38567#36947#30340#25569#25163#25104#21151#29366#24577
      #24403#38567#36947#25569#25163#25104#21151#65292#21518#32493#25805#20316#19982#39564#35777#24335#21452#36890#36947#19968#33268
      ''
      #22914#26524#32465#23450#30340#30340#36890#36947#26159#26381#21153#22120#31867#22411#65292#21487#20197#19968#23545#22810
      #19968#23545#22810#26159#21516#19968#20010#26381#21153#22120#65292#21487#20197#32465#23450#21040#22810#20010'vm'#38567#36947#20013#65292#20174#32780#23454#29616'vm'#34394#25311#38567#36947#26381#21153
      #32780'vm'#34394#25311#38567#36947#26381#21153#23545#38142#25509#25968#26159#19981#38480#21046#30340#65292#19968#20010#34394#25311#38567#36947#21487#20197#24102'100'#19975#20197#19978#38142#25509
      ''
      
        #38567#36947#19968#26086#24314#31435#25104#21151#65292'VMTunnel'#20063#33021#27491#24120#25910#21457#21629#20196#65292'RecvTunnel+SendTunnel'#30340#38567#36947#32465#23450#65292#24182#19981#20250#24433#21709'VMTunn' +
        'el'
      #38567#36947#19968#26086#24314#31435#25104#21151#65292'VMTunnel'#30340#21327#35758#23601#20250#21457#29983#21464#21270#65292#19981#26159#29305#27530#24773#20917#65292#19981#35201#36731#26131#35299#38500#38567#36947
      ''
      '')
    ScrollBars = ssVertical
    TabOrder = 0
    WordWrap = False
  end
  object ConnectButton: TButton
    Left = 8
    Top = 110
    Width = 98
    Height = 34
    Caption = '3.connect'
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
    Top = 223
    Width = 98
    Height = 34
    Caption = '4.hello world'
    TabOrder = 3
    OnClick = HelloWorldBtnClick
  end
  object UserEdit: TLabeledEdit
    Left = 136
    Top = 42
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
    Top = 42
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
    Left = 8
    Top = 69
    Width = 98
    Height = 34
    Caption = '2.Reg user'
    TabOrder = 6
    OnClick = RegUserButtonClick
  end
  object AsyncConnectButton: TButton
    Left = 8
    Top = 151
    Width = 98
    Height = 34
    Caption = '3.async connect'
    TabOrder = 7
    OnClick = AsyncConnectButtonClick
  end
  object fixedTimeButton: TButton
    Left = 8
    Top = 312
    Width = 98
    Height = 34
    Caption = '5.Fixed time Sync'
    TabOrder = 8
    OnClick = fixedTimeButtonClick
  end
  object connectTunnelButton: TButton
    Left = 263
    Top = 8
    Width = 106
    Height = 25
    Caption = '1.connect tunnel'
    TabOrder = 9
    OnClick = connectTunnelButtonClick
  end
  object Button1: TButton
    Left = 375
    Top = 6
    Width = 106
    Height = 25
    Caption = '1.disconnect tunnel'
    TabOrder = 10
    OnClick = Button1Click
  end
  object Timer1: TTimer
    Interval = 10
    OnTimer = Timer1Timer
    Left = 344
    Top = 48
  end
end
