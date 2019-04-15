object EZClientForm: TEZClientForm
  Left = 0
  Top = 0
  AutoSize = True
  BorderWidth = 20
  Caption = 'PeformanceTest Client'
  ClientHeight = 425
  ClientWidth = 409
  Color = clBtnFace
  DoubleBuffered = True
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
  object ConnectButton: TButton
    Left = 8
    Top = 27
    Width = 176
    Height = 35
    Caption = 'build 10000 connect'
    TabOrder = 0
    OnClick = ConnectButtonClick
  end
  object HostEdit: TLabeledEdit
    Left = 81
    Top = 0
    Width = 121
    Height = 21
    EditLabel.Width = 65
    EditLabel.Height = 13
    EditLabel.Caption = 'host address '
    LabelPosition = lpLeft
    TabOrder = 1
    Text = '127.0.0.1'
  end
  object TestCommandButton: TButton
    Left = 8
    Top = 68
    Width = 176
    Height = 66
    Caption = 'test command'
    TabOrder = 2
    Visible = False
    OnClick = TestCommandButtonClick
  end
  object Memo: TMemo
    Left = 0
    Top = 140
    Width = 409
    Height = 285
    Lines.Strings = (
      #28857'build 10000 connect'#21518' '#31245#31561#29255#21051
      #22914#26524#26426#22120#37197#32622#19981#39640#65292#23458#25143#31471#21345#39039#23601#20250#27604#36739#39057#32321
      ''
      '1'#65292#20013#38388#35831#25171#24320#36164#28304#30417#35270#22120#65292#23450#20301#21040#23458#25143#31471#36827#31243#20013
      '2'#65292#27880#24847#35266#23519#31995#32479#36164#28304#24320#38144
      '3'#65292#20013#36884#21387#27979#23458#25143#31471#20250#20986#29616#38142#25509#22833#36133#30340#21345#39039
      '4'#65292#26381#21153#22120#19981#20250#21345#39039
      '5'#65292#23436#25104#38142#25509#21518#65292'test command'#25353#38062#20250#26174#31034#20986#26469
      '6'#65292#28857#20987'test command'#65292#27979#35797'over'#21629#20196#20250#19982#23458#25143#31471'id'#19968#24182#25171#21360#20986#26469
      '7'#65292#26381#21153#22120#21453#39304#25171#21360#21629#20196#38656#35201#26102#38388
      ''
      #23458#25143#31471#26159#20351#29992#38142#25509#27744#36827#34892#30340#38142#25509
      #27599#27425#23458#25143#31471#38142#25509#65292#20250#19982#26381#21153#22120#25569#25163#21152#23494#21644'Hash'#21327#35758
      #36825#19968#27493#22312#23458#25143#31471#26159#38459#22622#26041#24335#65292#22312#23458#25143#31471#30340#38480#21046#20026#27599#31186'100'#24182#21457
      #26381#21153#22120#19981#21463#24182#21457#38480#21046
      ''
      'by 2017-12-16')
    ReadOnly = True
    TabOrder = 3
    WordWrap = False
  end
  object disconnectButton: TButton
    Left = 190
    Top = 27
    Width = 176
    Height = 35
    Caption = 'disconnect'
    TabOrder = 4
    Visible = False
    OnClick = disconnectButtonClick
  end
  object Timer: TTimer
    OnTimer = TimerTimer
    Left = 177
    Top = 160
  end
end
