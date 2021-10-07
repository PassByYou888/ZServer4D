object DT_P2PVM_ClientForm: TDT_P2PVM_ClientForm
  Left = 0
  Top = 0
  Caption = 'DT Framework Client - p2pVM Double Tunnel, create by.qq600585'
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
      'Dual channel minimalist framework based on p2pvm'
      ''
      '1. Avoid using blocking network mechanism'
      '2. Try to use asynchronous events in'
      
        '3. The virtuaauth server accepts all login users and passwords. Even if there is no user, it will accept. The default login user name is testuser and password is tes' +
        'tUser'
      '4. Automation supports short-term reconnection'
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
