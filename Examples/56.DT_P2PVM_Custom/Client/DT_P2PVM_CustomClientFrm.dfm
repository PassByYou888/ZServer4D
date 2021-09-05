object DT_P2PVM_CustomClientForm: TDT_P2PVM_CustomClientForm
  Left = 0
  Top = 0
  Caption = 
    'DT Framework Client - Custom p2pVM Double Tunnel, create by.qq60' +
    '0585'
  ClientHeight = 381
  ClientWidth = 867
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
      #22522#20110'p2pVM'#30340#21452#36890#36947#26497#31616#26694#26550'.'
      ''
      '1,'#36991#20813#20351#29992#38459#22622#32593#32476#26426#21046
      '2,'#20013#23613#37327#20351#29992#24322#27493#20107#20214
      '3,'#33258#21160#21270#25903#25345#30701#32447#37325#36830
      ''
      'by.qq600585')
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object connButton: TButton
    Left = 16
    Top = 104
    Width = 129
    Height = 33
    Caption = 'Connection'
    TabOrder = 1
    OnClick = connButtonClick
  end
  object disButton: TButton
    Left = 16
    Top = 143
    Width = 129
    Height = 33
    Caption = 'Disconnect'
    TabOrder = 2
    OnClick = disButtonClick
  end
  object UserEdit: TLabeledEdit
    Left = 16
    Top = 24
    Width = 113
    Height = 21
    EditLabel.Width = 22
    EditLabel.Height = 13
    EditLabel.Caption = 'User'
    TabOrder = 3
    Text = 'testUser'
  end
  object PasswdEdit: TLabeledEdit
    Left = 16
    Top = 64
    Width = 113
    Height = 21
    EditLabel.Width = 46
    EditLabel.Height = 13
    EditLabel.Caption = 'Password'
    PasswordChar = '*'
    TabOrder = 4
    Text = 'testUser'
  end
  object netTimer: TTimer
    Interval = 10
    OnTimer = netTimerTimer
    Left = 224
    Top = 80
  end
end
