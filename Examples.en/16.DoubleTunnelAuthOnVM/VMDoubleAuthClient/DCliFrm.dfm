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
    Left = 8
    Top = 280
    Width = 47
    Height = 13
    Caption = 'TimeLabel'
  end
  object Memo1: TMemo
    Left = 112
    Top = 69
    Width = 481
    Height = 307
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Zserver4d is a server middleware'
      ''
      'Principle of tunnel client:'
      'Firstly, it is established through vmtunnel. In the anonymous function event, the handshake success status of vmtunnel virtual tunnel is obtained. When the tunnel handshake is successful, the subsequent operations are consistent with the verified dual channel'
      ''
      'If the bound channel is a server type, one to many to many can be the same server, and can be bound to multiple VM tunnels to realize VM virtual tunnel service. VM virtual tunnel service has no limit on the number of links, and a virtual tunnel can carry more than 1 million links'
      ''
      
        'Once the tunnel is established successfully, vmtunnel can also send and receive commands normally. The tunnel binding of recvtunnel + sendtunnel will not affect vmtunn' +
        'el'
      'Once the tunnel is established successfully, the vmtunnel protocol will change. It is not a special case. Do not cancel the tunnel easily'
      ''
      '')
    ScrollBars = ssVertical
    TabOrder = 0
    WordWrap = False
  end
  object ConnectButton: TButton
    Left = 8
    Top = 110
    Width = 98
    Height = 34
    Caption = '3.connect'
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
  object HelloWorldBtn: TButton
    Left = 8
    Top = 223
    Width = 98
    Height = 34
    Caption = '4.hello world'
    TabOrder = 3
    OnClick = HelloWorldBtnClick
  end
  object UserEdit: TLabeledEdit
    Left = 136
    Top = 42
    Width = 97
    Height = 21
    EditLabel.Width = 22
    EditLabel.Height = 13
    EditLabel.Caption = 'User'
    LabelPosition = lpLeft
    TabOrder = 4
    Text = 'test'
  end
  object PasswdEdit: TLabeledEdit
    Left = 288
    Top = 42
    Width = 97
    Height = 21
    EditLabel.Width = 36
    EditLabel.Height = 13
    EditLabel.Caption = 'Passwd'
    LabelPosition = lpLeft
    TabOrder = 5
    Text = '123456'
  end
  object RegUserButton: TButton
    Left = 8
    Top = 69
    Width = 98
    Height = 34
    Caption = '2.Reg user'
    TabOrder = 6
    OnClick = RegUserButtonClick
  end
  object AsyncConnectButton: TButton
    Left = 8
    Top = 151
    Width = 98
    Height = 34
    Caption = '3.async connect'
    TabOrder = 7
    OnClick = AsyncConnectButtonClick
  end
  object fixedTimeButton: TButton
    Left = 8
    Top = 312
    Width = 98
    Height = 34
    Caption = '5.Fixed time Sync'
    TabOrder = 8
    OnClick = fixedTimeButtonClick
  end
  object connectTunnelButton: TButton
    Left = 263
    Top = 8
    Width = 106
    Height = 25
    Caption = '1.connect tunnel'
    TabOrder = 9
    OnClick = connectTunnelButtonClick
  end
  object Button1: TButton
    Left = 375
    Top = 6
    Width = 106
    Height = 25
    Caption = '1.disconnect tunnel'
    TabOrder = 10
    OnClick = Button1Click
  end
  object Timer1: TTimer
    Interval = 10
    OnTimer = Timer1Timer
    Left = 344
    Top = 48
  end
end
