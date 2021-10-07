object AuthDoubleTunnelClientForm: TAuthDoubleTunnelClientForm
  Left = 0
  Top = 0
  Caption = 'Auth Double Tunnel Client'
  ClientHeight = 384
  ClientWidth = 634
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    634
    384)
  PixelsPerInch = 96
  TextHeight = 13
  object TimeLabel: TLabel
    Left = 32
    Top = 184
    Width = 47
    Height = 13
    Caption = 'TimeLabel'
  end
  object Memo1: TMemo
    Left = 136
    Top = 62
    Width = 481
    Height = 307
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Zsserver4d is a server middleware. The demo is a login two-way mode (a method to interface with a large authentication background)'
      '')
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object ConnectButton: TButton
    Left = 32
    Top = 94
    Width = 89
    Height = 35
    Caption = 'connect'
    TabOrder = 1
    OnClick = ConnectButtonClick
  end
  object HostEdit: TLabeledEdit
    Left = 136
    Top = 8
    Width = 121
    Height = 21
    EditLabel.Width = 65
    EditLabel.Height = 13
    EditLabel.Caption = 'host address '
    LabelPosition = lpLeft
    TabOrder = 2
    Text = '127.0.0.1'
  end
  object UserEdit: TLabeledEdit
    Left = 136
    Top = 35
    Width = 97
    Height = 21
    EditLabel.Width = 22
    EditLabel.Height = 13
    EditLabel.Caption = 'User'
    LabelPosition = lpLeft
    TabOrder = 3
    Text = 'test'
  end
  object PasswdEdit: TLabeledEdit
    Left = 288
    Top = 35
    Width = 97
    Height = 21
    EditLabel.Width = 36
    EditLabel.Height = 13
    EditLabel.Caption = 'Passwd'
    LabelPosition = lpLeft
    TabOrder = 4
    Text = 'test'
  end
  object AsyncConnectButton: TButton
    Left = 32
    Top = 135
    Width = 89
    Height = 35
    Caption = 'async connect'
    TabOrder = 5
    OnClick = AsyncConnectButtonClick
  end
  object fixedTimeButton: TButton
    Left = 32
    Top = 216
    Width = 89
    Height = 34
    Caption = 'Fixed time Sync'
    TabOrder = 6
    OnClick = fixedTimeButtonClick
  end
  object Timer1: TTimer
    Interval = 10
    OnTimer = Timer1Timer
    Left = 344
    Top = 48
  end
end
