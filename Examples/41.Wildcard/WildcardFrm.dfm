object WildcardForm: TWildcardForm
  Left = 0
  Top = 0
  AutoSize = True
  BorderWidth = 10
  Caption = 'Wildcard Demo'
  ClientHeight = 188
  ClientWidth = 553
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 64
    Width = 553
    Height = 124
    Lines.Strings = (
      #36890#37197#31526#26159#19968#31181#29305#27530#35821#21477#65292#20027#35201#26377#26143#21495'(*)'#21644#38382#21495'(?)'#65292#29992#26469#27169#31946#25628#32034#25991#20214#12290
      #24403#26597#25214#25991#20214#22841#26102#65292#21487#20197#20351#29992#23427#26469#20195#26367#19968#20010#25110#22810#20010#30495#27491#23383#31526#65307
      #24403#19981#30693#36947#30495#27491#23383#31526#25110#32773#25042#24471#36755#20837#23436#25972#21517#23383#26102#65292#24120#24120#20351#29992#36890#37197#31526#20195#26367#19968#20010#25110#22810#20010#30495#27491#30340#23383#31526#12290
      '*'#21487#20197#20195#34920#20219#20309#23383#31526#20018#65307'?'#20165#20195#34920#21333#20010#23383#31526#20018#65292#20294#27492#21333#23383#24517#39035#23384#22312
      ''
      'umlMultipleMatch '#26159#36890#37197#31526#23383#31526#20018#21305#37197#20989#25968)
    TabOrder = 0
  end
  object SourEdit: TLabeledEdit
    Left = 88
    Top = 0
    Width = 153
    Height = 21
    EditLabel.Width = 78
    EditLabel.Height = 13
    EditLabel.Caption = 'wildchar source:'
    LabelPosition = lpLeft
    TabOrder = 1
    Text = '*3?5'
  end
  object TargetEdit: TLabeledEdit
    Left = 88
    Top = 27
    Width = 153
    Height = 21
    EditLabel.Width = 64
    EditLabel.Height = 13
    EditLabel.Caption = 'target string:'
    LabelPosition = lpLeft
    TabOrder = 2
    Text = '12345'
  end
  object MatchButton: TButton
    Left = 247
    Top = 25
    Width = 75
    Height = 25
    Caption = 'Match'
    TabOrder = 3
    OnClick = MatchButtonClick
  end
end
