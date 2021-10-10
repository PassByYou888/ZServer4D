object ZSClientForm: TZSClientForm
  Left = 0
  Top = 0
  AutoSize = True
  BorderWidth = 15
  Caption = 'ZS translation client'
  ClientHeight = 548
  ClientWidth = 424
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
    Left = 8
    Top = 46
    Width = 57
    Height = 13
    Caption = 'original text'
  end
  object Label2: TLabel
    Left = 8
    Top = 174
    Width = 43
    Height = 13
    Caption = 'translate'
  end
  object HostEdit: TLabeledEdit
    Left = 0
    Top = 16
    Width = 161
    Height = 21
    EditLabel.Width = 159
    EditLabel.Height = 13
    EditLabel.Caption = 'Server IP (supports IPv4 + IPv6)'
    TabOrder = 0
    Text = '127.0.0.1'
  end
  object SourMemo: TMemo
    Left = 0
    Top = 70
    Width = 424
    Height = 89
    Lines.Strings = (
      'Hello world')
    TabOrder = 1
  end
  object DestMemo: TMemo
    Left = 0
    Top = 198
    Width = 424
    Height = 110
    TabOrder = 2
  end
  object SourComboBox: TComboBox
    Left = 71
    Top = 43
    Width = 154
    Height = 21
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 3
    Text = 'automatic detection'
    Items.Strings = (
      'automatic detection'
      'chinese'
      'English'
      'Cantonese'
      'classical Chinese'
      'Japanese'
      'Korean'
      'French'
      'Spanish'
      'Thai'
      'Arabic'
      'Russian'
      'Portuguese'
      'German'
      'Italian'
      'Greek'
      'Dutch'
      'Polish'
      'Bulgarian'
      'Estonian'
      'Danish'
      'Finnish'
      'Czech'
      'romanian'
      'Slovenian'
      'Swedish'
      'Hungarian'
      'Traditional Chinese'
      'Vietnamese')
  end
  object DestComboBox: TComboBox
    Left = 57
    Top = 171
    Width = 128
    Height = 21
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 4
    Text = 'automatic detection'
    Items.Strings = (
      'automatic detection'
      'chinese'
      'English'
      'Cantonese'
      'classical Chinese'
      'Japanese'
      'Korean'
      'French'
      'Spanish'
      'Thai'
      'Arabic'
      'Russian'
      'Portuguese'
      'German'
      'Italian'
      'Greek'
      'Dutch'
      'Polish'
      'Bulgarian'
      'Estonian'
      'Danish'
      'Finnish'
      'Czech'
      'romanian'
      'Slovenian'
      'Swedish'
      'Hungarian'
      'Traditional Chinese'
      'Vietnamese')
  end
  object TransButton: TButton
    Left = 0
    Top = 321
    Width = 137
    Height = 25
    Caption = 'Do Translate'
    TabOrder = 5
    OnClick = TransButtonClick
  end
  object LogMemo: TMemo
    Left = 0
    Top = 352
    Width = 424
    Height = 196
    TabOrder = 6
  end
  object UsedCacheCheckBox: TCheckBox
    Left = 167
    Top = 20
    Width = 257
    Height = 17
    Caption = 'Accelerated translation (money saving scheme)'
    Checked = True
    State = cbChecked
    TabOrder = 7
  end
  object UpdateTranslateButton: TButton
    Left = 191
    Top = 171
    Width = 137
    Height = 21
    Caption = 'Translation correction'
    TabOrder = 8
    OnClick = UpdateTranslateButtonClick
  end
end
