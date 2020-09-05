object FragmentClientForm: TFragmentClientForm
  Left = 0
  Top = 0
  AutoSize = True
  BorderWidth = 20
  Caption = #22810#32447#25991#20214#19979#36733'demo'
  ClientHeight = 385
  ClientWidth = 944
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Memo: TMemo
    Left = 0
    Top = 40
    Width = 545
    Height = 345
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clLime
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Lines.Strings = (
      #35813'Demo'#28436#31034#20102#22810#32447#25991#20214#19979#36733'('#19982#36805#38647#31867#20284')'
      #35813'Demo'#30340#22810#32447#19979#36733#26426#21046#20026#26631#20934#26426#21046','#22810#30475#20195#30721','#23588#20854#26159#25968#25454#32467#26500#35774#35745
      #35813'Demo'#30340#20256#36755#26426#21046#20026'CompleteBuffer,'#36825#31181#26426#21046#38656#35201#25511#21046#20869#23384#24320#38144'.'
      #35813'Demo'#30340#26381#21153#22120#19981#20250#26242#23384#20840#37096#25991#20214','#26381#21153#22120#26242#23384#30340#25991#20214#25968#25454#26681#25454#19979#36733#32447#31243#32780#23450'.'#19981#20250#29190#28856'.'
      'by.qq600585'
      '')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object HostEdit: TLabeledEdit
    Left = 40
    Top = 4
    Width = 161
    Height = 21
    EditLabel.Width = 29
    EditLabel.Height = 13
    EditLabel.Caption = 'Host: '
    LabelPosition = lpLeft
    TabOrder = 1
    Text = '127.0.0.1'
  end
  object connButton: TButton
    Left = 207
    Top = 0
    Width = 107
    Height = 25
    Caption = #21019#24314'10'#20010#29289#29702#36830#25509
    TabOrder = 2
    OnClick = connButtonClick
  end
  object downloadButton: TButton
    Left = 320
    Top = 0
    Width = 129
    Height = 25
    Caption = #22810#32447#31243#19979#36733
    TabOrder = 3
    OnClick = downloadButtonClick
  end
  object stateMemo: TMemo
    Left = 551
    Top = 40
    Width = 393
    Height = 345
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clLime
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
  end
  object Timer1: TTimer
    Interval = 10
    OnTimer = Timer1Timer
    Left = 152
    Top = 120
  end
  object checkDownTimer: TTimer
    OnTimer = checkDownTimerTimer
    Left = 152
    Top = 192
  end
end
