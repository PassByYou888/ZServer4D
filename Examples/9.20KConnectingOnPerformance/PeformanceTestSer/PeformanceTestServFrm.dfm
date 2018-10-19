object EZServerForm: TEZServerForm
  Left = 0
  Top = 0
  Caption = 'Peformance Test Service'
  ClientHeight = 384
  ClientWidth = 1259
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
  PixelsPerInch = 96
  TextHeight = 13
  object readmeMemo: TMemo
    Left = 0
    Top = 0
    Width = 451
    Height = 384
    Align = alClient
    DoubleBuffered = True
    Lines.Strings = (
      'Zserver4D'#21387#27979#26381#21153#22120
      ''
      #27979#35797#26041#27861#65306
      '1'#65292#24320#21551#26381#21153
      '2'#65292#24320#21551#23458#25143#31471#65292#28857'build connect'#65292#22788#29702#27809#26377#36305#23436#20197#21069#65292#19981#35201#20851#38381
      ''
      #38142#25509#26102#22240#20026#35201#25569#25163#21327#21830#21152#23494#21644'hash'#21327#35758#65292#25152#20197#21019#24314#38142#25509#26102#36739#24930
      #38142#25509#21019#24314#25104#21151#21518#65292#22312#23458#25143#31471#21487#21457#21629#20196#27979#35797#65292#21709#24212#36895#24230#38750#24120#24555
      #24403#23458#25143#31471#37117#36864#20986#23436#25104#21518#65292#26381#21153#22120#27809#26377#20869#23384#27844#28431)
    ParentDoubleBuffered = False
    TabOrder = 0
  end
  object StateMemo: TMemo
    Left = 451
    Top = 0
    Width = 202
    Height = 384
    Align = alRight
    DoubleBuffered = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Lines.Strings = (
      'StateMemo')
    ParentDoubleBuffered = False
    ParentFont = False
    TabOrder = 1
    WordWrap = False
  end
  object ReceiveMemo: TMemo
    Left = 653
    Top = 0
    Width = 202
    Height = 384
    Align = alRight
    DoubleBuffered = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Lines.Strings = (
      'StateMemo')
    ParentDoubleBuffered = False
    ParentFont = False
    TabOrder = 2
    WordWrap = False
  end
  object SendMemo: TMemo
    Left = 855
    Top = 0
    Width = 202
    Height = 384
    Align = alRight
    DoubleBuffered = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Lines.Strings = (
      'StateMemo')
    ParentDoubleBuffered = False
    ParentFont = False
    TabOrder = 3
    WordWrap = False
  end
  object CpuMemo: TMemo
    Left = 1057
    Top = 0
    Width = 202
    Height = 384
    Align = alRight
    DoubleBuffered = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Lines.Strings = (
      'StateMemo')
    ParentDoubleBuffered = False
    ParentFont = False
    TabOrder = 4
    WordWrap = False
  end
  object Timer: TTimer
    OnTimer = TimerTimer
    Left = 80
    Top = 144
  end
  object RefStateTimer: TTimer
    Interval = 2000
    OnTimer = RefStateTimerTimer
    Left = 320
    Top = 152
  end
end
