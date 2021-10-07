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
    Top = 264
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
      'Zsserver4d is a server middleware. This demo is a login two-way mode'
      ''
      'Login explanation: authentication must be performed when initiating a connection, because there is user identity in the login. In the logged in server system, each user has its own file storage and data storage space'
      ''
      'The two-way mode interpretation can be explained by the client actively sending commands to the server for execution or the server actively initiating command execution to the client. In the link mechanism, two channels are used, one for receiving and one for sending. When both channels are successfully linked, the two-way bridge can be completed by using the tunnellink method'
      ''
      'Compared with the simple two-way working mode, the login two-way communication mode adds the following functions: asynchronous login (client) large file transfer support (server, client) stack instruction storage (server) storage space and data management (server) preemptive login (server)'
      ''
      'The command system can be a simple string and various data packages, or a stream package. In short, any data transceiver lower than 500K, including small files, can use the command'
      ''
      'When the data length is very large, the bigstream mechanism must be used in zsserver4d to send and receive data'
      ''
      'The attached client can be a mobile platform or a personal computer platform')
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
  object HelloWorldBtn: TButton
    Left = 32
    Top = 207
    Width = 89
    Height = 34
    Caption = 'hello world'
    TabOrder = 3
    OnClick = HelloWorldBtnClick
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
    TabOrder = 4
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
    TabOrder = 5
    Text = '123456'
  end
  object RegUserButton: TButton
    Left = 32
    Top = 53
    Width = 89
    Height = 35
    Caption = 'Reg user'
    TabOrder = 6
    OnClick = RegUserButtonClick
  end
  object AsyncConnectButton: TButton
    Left = 32
    Top = 135
    Width = 89
    Height = 35
    Caption = 'async connect'
    TabOrder = 7
    OnClick = AsyncConnectButtonClick
  end
  object fixedTimeButton: TButton
    Left = 32
    Top = 296
    Width = 89
    Height = 34
    Caption = 'Fixed time Sync'
    TabOrder = 8
    OnClick = fixedTimeButtonClick
  end
  object Timer1: TTimer
    Interval = 10
    OnTimer = Timer1Timer
    Left = 344
    Top = 48
  end
end
