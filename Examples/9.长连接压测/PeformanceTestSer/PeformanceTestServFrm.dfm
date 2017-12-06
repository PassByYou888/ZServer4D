object EZServerForm: TEZServerForm
  Left = 0
  Top = 0
  Caption = 'Peformance Test Service'
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
      #27492'Demo'#28436#31034#20102'2'#19975#38142#25509#30340#24615#33021#27979#35797
      ''
      #27979#35797#26041#27861#65306
      #23458#25143#31471#24320'20'#20010#65292#38142#25509#65292#28857#27979#35797
      ''
      #38142#25509#26102#22240#20026#35201#25569#25163#21327#21830#21152#23494#21644'hash'#21327#35758#65292#25152#20197#21019#24314#38142#25509#26102#36739#24930
      #38142#25509#21019#24314#25104#21151#21518#65292#22312#23458#25143#31471#21487#21457#21629#20196#27979#35797#65292#21709#24212#36895#24230#38750#24120#24555
      #24403#23458#25143#31471#37117#36864#20986#23436#25104#21518#65292#26381#21153#22120#27809#26377#20869#23384#27844#28431)
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
  object Timer1: TTimer
    Interval = 1
    OnTimer = Timer1Timer
    Left = 80
    Top = 144
  end
  object Timer2: TTimer
    OnTimer = Timer2Timer
    Left = 80
    Top = 200
  end
end
