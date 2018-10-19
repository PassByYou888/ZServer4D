object ZSClientForm: TZSClientForm
  Left = 0
  Top = 0
  AutoSize = True
  BorderWidth = 15
  Caption = 'ZS'#32763#35793#23458#25143#31471'...'
  ClientHeight = 548
  ClientWidth = 345
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 0
    Top = 51
    Width = 24
    Height = 13
    Caption = #21407#25991
  end
  object Label2: TLabel
    Left = 0
    Top = 179
    Width = 24
    Height = 13
    Caption = #32763#35793
  end
  object HostEdit: TLabeledEdit
    Left = 0
    Top = 16
    Width = 161
    Height = 21
    EditLabel.Width = 130
    EditLabel.Height = 13
    EditLabel.Caption = #26381#21153#22120'IP('#25903#25345'IPV4+IPV6)'
    TabOrder = 0
    Text = '127.0.0.1'
  end
  object SourMemo: TMemo
    Left = 0
    Top = 70
    Width = 345
    Height = 89
    Lines.Strings = (
      #20320#22909#19990#30028)
    TabOrder = 1
  end
  object DestMemo: TMemo
    Left = 0
    Top = 198
    Width = 345
    Height = 110
    TabOrder = 2
  end
  object SourComboBox: TComboBox
    Left = 40
    Top = 43
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 3
    Text = #33258#21160#26816#27979
    Items.Strings = (
      #33258#21160#26816#27979
      #20013#25991
      #33521#35821
      #31908#35821
      #25991#35328#25991
      #26085#35821
      #38889#35821
      #27861#35821
      #35199#29677#29273#35821
      #27888#35821
      #38463#25289#20271#35821
      #20420#35821
      #33889#33796#29273#35821
      #24503#35821
      #24847#22823#21033#35821
      #24076#33098#35821
      #33655#20848#35821
      #27874#20848#35821
      #20445#21152#21033#20122#35821
      #29233#27801#23612#20122#35821
      #20025#40614#35821
      #33452#20848#35821
      #25463#20811#35821
      #32599#39532#23612#20122#35821
      #26031#27931#25991#23612#20122#35821
      #29790#20856#35821
      #21256#29273#21033#35821
      #32321#20307#20013#25991
      #36234#21335#35821)
  end
  object DestComboBox: TComboBox
    Left = 40
    Top = 171
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 4
    Text = #33258#21160#26816#27979
    Items.Strings = (
      #33258#21160#26816#27979
      #20013#25991
      #33521#35821
      #31908#35821
      #25991#35328#25991
      #26085#35821
      #38889#35821
      #27861#35821
      #35199#29677#29273#35821
      #27888#35821
      #38463#25289#20271#35821
      #20420#35821
      #33889#33796#29273#35821
      #24503#35821
      #24847#22823#21033#35821
      #24076#33098#35821
      #33655#20848#35821
      #27874#20848#35821
      #20445#21152#21033#20122#35821
      #29233#27801#23612#20122#35821
      #20025#40614#35821
      #33452#20848#35821
      #25463#20811#35821
      #32599#39532#23612#20122#35821
      #26031#27931#25991#23612#20122#35821
      #29790#20856#35821
      #21256#29273#21033#35821
      #32321#20307#20013#25991
      #36234#21335#35821)
  end
  object TransButton: TButton
    Left = 0
    Top = 331
    Width = 75
    Height = 25
    Caption = #32763#35793
    TabOrder = 5
    OnClick = TransButtonClick
  end
  object LogMemo: TMemo
    Left = 0
    Top = 379
    Width = 345
    Height = 169
    TabOrder = 6
  end
  object UsedCacheCheckBox: TCheckBox
    Left = 167
    Top = 20
    Width = 170
    Height = 17
    Caption = #21152#36895#32763#35793' ('#30465#38065#26041#26696')'
    Checked = True
    State = cbChecked
    TabOrder = 7
  end
  object UpdateTranslateButton: TButton
    Left = 248
    Top = 171
    Width = 97
    Height = 21
    Caption = #32763#35793#20462#27491
    TabOrder = 8
    OnClick = UpdateTranslateButtonClick
  end
end
