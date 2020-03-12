object ParallelForm: TParallelForm
  Left = 0
  Top = 0
  AutoSize = True
  BorderWidth = 10
  Caption = 'Parallel Demo. Create by.qq600585'
  ClientHeight = 551
  ClientWidth = 721
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
  object StateLabel: TLabel
    Left = 113
    Top = 280
    Width = 25
    Height = 13
    Caption = 'state'
  end
  object ParaAddButton: TButton
    Left = 0
    Top = 14
    Width = 107
    Height = 25
    Caption = 'Parallel Add'
    TabOrder = 0
    OnClick = ParaAddButtonClick
  end
  object Memo1: TMemo
    Left = 113
    Top = 0
    Width = 608
    Height = 50
    Lines.Strings = (
      'Parallel Add'#28436#31034#20102'4'#31181#26041#24335#25805#20316#25972#25968#21407#23376#21464#37327)
    TabOrder = 1
  end
  object Memo2: TMemo
    Left = 113
    Top = 56
    Width = 608
    Height = 50
    Lines.Strings = (
      'Parallel Lock'#28436#31034#20102'2'#31181#23433#20840#29366#24577#26426','#23383#31526#20018)
    TabOrder = 2
  end
  object ParaLockButton: TButton
    Left = 0
    Top = 70
    Width = 107
    Height = 25
    Caption = 'Parallel lock'
    TabOrder = 3
    OnClick = ParaLockButtonClick
  end
  object Memo3: TMemo
    Left = 113
    Top = 112
    Width = 608
    Height = 50
    Lines.Strings = (
      'Parallel 19937'#28436#31034#20102'19937'#38543#26426#25968#22312#24182#34892#19982#22810#32447#31243#31243#24207#20013#30340#32479#19968#24615)
    TabOrder = 4
  end
  object Para19937Button: TButton
    Left = 0
    Top = 126
    Width = 107
    Height = 25
    Caption = 'Parallel 19937'
    TabOrder = 5
    OnClick = Para19937ButtonClick
  end
  object Memo4: TMemo
    Left = 113
    Top = 168
    Width = 608
    Height = 50
    Lines.Strings = (
      'Parallel TRandom'#28436#31034#20102'TRandom'#24182#34892#19982#22810#32447#31243#31243#24207#20013#30340#32479#19968#24615#65292#24615#33021#20248#20110#30452#25509#20351#29992'19937'#20989#25968)
    TabOrder = 6
  end
  object ParallelTRandomButton: TButton
    Left = 0
    Top = 182
    Width = 107
    Height = 25
    Caption = 'Parallel TRandom'
    TabOrder = 7
    OnClick = ParallelTRandomButtonClick
  end
  object Memo5: TMemo
    Left = 113
    Top = 224
    Width = 608
    Height = 50
    Lines.Strings = (
      'delphi TRandom'#28436#31034#20102#26367#20195'delphi'#20869#32622#30340'Random'#65292#20351#20182#20855#22791#24182#34892#19982#22810#32447#31243#31243#24207#20013#30340#32479#19968#24615#65292#23454#29616#22823#35268#27169#31639#27861#31227#26893
      #36825#31181#26041#24335#21482#25903#25345'XE10.3'#25110#21017#20197#21518#30340#29256#26412#65292'XE10.2'#37117#26159#19981#25903#25345#30340#65292#35814#35265#20195#30721#22791#27880)
    TabOrder = 8
  end
  object ParaDelphiRandomButton: TButton
    Left = 0
    Top = 238
    Width = 107
    Height = 25
    Caption = 'delphi Random'
    TabOrder = 9
    OnClick = ParaDelphiRandomButtonClick
  end
  object Memo6: TMemo
    Left = 113
    Top = 302
    Width = 608
    Height = 249
    Lines.Strings = (
      'TCompute'#26159#22823#35268#27169#35745#31639#30340#32447#31243#23454#20363#65292#33258#21160#21270#31649#29702#32447#31243#27744#65292#35745#31639#38431#21015#65292#31890#24230#24182#34892#31561#31561
      'TCompute'#32447#31243#27744#20013#30340#32447#31243#21482#20250#31354#38386'1'#31186#65292#24403#25105#20204#30340#32447#31243#21040#36798'1'#31186#65292#32447#31243#23558#20250#34987#37322#25918#25481
      'TCompute'#32447#31243#27744#21487#20197#23618#23618#23884#22871#65292#27604#22914#32447#31243#22871#24182#34892#20877#24182#34892#37324#38754#20877#22871#24182#34892
      'TCompute'#21644#24182#34892#31243#24207#20849#20139#32447#31243#27744#21644#32447#31243#23454#20363
      'TCompute'#29366#24577#26426#38750#24120#32321#22810#65292#22823#37096#20998#26159#20869#37096#29366#24577#26426#65292#36890#36807'TCompute'#25552#20379#30340#29366#24577#26426#20989#25968#26469#33719#21462
      'TCompute'#23545'FPC'#25552#20379'local neast procedure'#65292#23545'Delphi'#25552#20379'Anonymous procedure')
    TabOrder = 10
  end
  object ComputeThreadButton: TButton
    Left = 0
    Top = 302
    Width = 107
    Height = 25
    Caption = 'TCompute Thread'
    TabOrder = 11
    OnClick = ComputeThreadButtonClick
  end
  object Timer1: TTimer
    Interval = 10
    OnTimer = Timer1Timer
    Left = 232
    Top = 456
  end
end
