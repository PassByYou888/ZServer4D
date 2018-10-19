object AuthDoubleServerForm: TAuthDoubleServerForm
  Left = 0
  Top = 0
  Caption = 'VM Auth Double Tunnel Server'
  ClientHeight = 456
  ClientWidth = 634
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
  DesignSize = (
    634
    456)
  PixelsPerInch = 96
  TextHeight = 13
  object TimeLabel: TLabel
    Left = 40
    Top = 168
    Width = 47
    Height = 13
    Caption = 'TimeLabel'
  end
  object Memo1: TMemo
    Left = 144
    Top = 8
    Width = 473
    Height = 425
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'ZServer4D'#26159#19968#27454#26381#21153#22120#20013#38388#20214
      ''
      #38567#36947#21407#29702':'
      'VMTunnel'#26159#25105#20204#20570#30340#38567#36947#26381#21153#22120
      #25105#20204#36890#36807#25130#33719#26381#21153#22120#20869#32622#30340#38567#36947#35302#21457#35831#27714#65292#22312#35831#27714#20013#65292#21160#24577#30340#32465#23450#25105#20204#30340'Recv+Send'#20004#20010#36890#36947#21363#21487
      ''
      ''
      #22914#26524#32465#23450#30340#30340#36890#36947#26159#26381#21153#22120#31867#22411#65292#21487#20197#19968#23545#22810
      #19968#23545#22810#26159#21516#19968#20010#26381#21153#22120#65292#21487#20197#32465#23450#21040#22810#20010'vm'#38567#36947#20013#65292#20174#32780#23454#29616'vm'#34394#25311#38567#36947#26381#21153
      #32780'vm'#34394#25311#38567#36947#26381#21153#23545#38142#25509#25968#26159#19981#38480#21046#30340#65292#19968#20010#34394#25311#38567#36947#21487#20197#24102'100'#19975#20197#19978#38142#25509
      ''
      
        #38567#36947#19968#26086#24314#31435#25104#21151#65292'VMTunnel'#20063#33021#27491#24120#25910#21457#21629#20196#65292'RecvTunnel+SendTunnel'#30340#38567#36947#32465#23450#65292#24182#19981#20250#24433#21709'VMTunn' +
        'el'
      #38567#36947#19968#26086#24314#31435#25104#21151#65292'VMTunnel'#30340#21327#35758#23601#20250#21457#29983#21464#21270#65292#19981#26159#29305#27530#24773#20917#65292#19981#35201#36731#26131#35299#38500#38567#36947
      '')
    ScrollBars = ssVertical
    TabOrder = 0
    WordWrap = False
  end
  object StartServiceButton: TButton
    Left = 32
    Top = 8
    Width = 89
    Height = 35
    Caption = 'start service'
    TabOrder = 1
    OnClick = StartServiceButtonClick
  end
  object ChangeCaptionButton: TButton
    Left = 32
    Top = 80
    Width = 89
    Height = 33
    Caption = 'Change Caption'
    TabOrder = 2
    OnClick = ChangeCaptionButtonClick
  end
  object GetClientValueButton: TButton
    Left = 32
    Top = 119
    Width = 89
    Height = 33
    Caption = 'GetClient Value'
    TabOrder = 3
    OnClick = GetClientValueButtonClick
  end
  object Timer1: TTimer
    Interval = 10
    OnTimer = Timer1Timer
    Left = 360
    Top = 8
  end
end
