object DT_P2PVM_CustomServerForm: TDT_P2PVM_CustomServerForm
  Left = 0
  Top = 0
  Caption = 
    'DT Framework Server - Custom p2pVM Double Tunnel, create by.qq60' +
    '0585'
  ClientHeight = 369
  ClientWidth = 852
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
  object Memo: TMemo
    Left = 168
    Top = 16
    Width = 673
    Height = 337
    Lines.Strings = (
      #22522#20110'p2pVM'#30340#21452#36890#36947#26497#31616#26694#26550'.')
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object startservButton: TButton
    Left = 24
    Top = 24
    Width = 129
    Height = 33
    Caption = 'Start service.'
    TabOrder = 1
    OnClick = startservButtonClick
  end
  object stopservButton: TButton
    Left = 24
    Top = 63
    Width = 129
    Height = 33
    Caption = 'Stop service.'
    TabOrder = 2
    OnClick = stopservButtonClick
  end
  object UserEdit: TLabeledEdit
    Left = 24
    Top = 152
    Width = 113
    Height = 21
    EditLabel.Width = 22
    EditLabel.Height = 13
    EditLabel.Caption = 'User'
    TabOrder = 3
    Text = 'testUser'
  end
  object PasswdEdit: TLabeledEdit
    Left = 24
    Top = 192
    Width = 113
    Height = 21
    EditLabel.Width = 46
    EditLabel.Height = 13
    EditLabel.Caption = 'Password'
    PasswordChar = '*'
    TabOrder = 4
    Text = 'testUser'
  end
  object regUsrButton: TButton
    Left = 24
    Top = 224
    Width = 129
    Height = 33
    Caption = 'Register user'
    TabOrder = 5
    OnClick = regUsrButtonClick
  end
  object netTimer: TTimer
    Interval = 10
    OnTimer = netTimerTimer
    Left = 224
    Top = 80
  end
end
