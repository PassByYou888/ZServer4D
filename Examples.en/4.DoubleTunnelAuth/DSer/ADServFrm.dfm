object AuthDoubleServerForm: TAuthDoubleServerForm
  Left = 0
  Top = 0
  Caption = 'Auth Double Tunnel Server'
  ClientHeight = 456
  ClientWidth = 634
  Color = clBtnFace
  DoubleBuffered = True
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
    456)
  PixelsPerInch = 96
  TextHeight = 13
  object TimeLabel: TLabel
    Left = 40
    Top = 168
    Width = 47
    Height = 13
    Caption = 'TimeLabel'
  end
  object Memo1: TMemo
    Left = 144
    Top = 8
    Width = 473
    Height = 425
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
      'The attached client can be a mobile platform or a personal computer platform'
      '')
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object StartServiceButton: TButton
    Left = 32
    Top = 8
    Width = 89
    Height = 35
    Caption = 'start service'
    TabOrder = 1
    OnClick = StartServiceButtonClick
  end
  object ChangeCaptionButton: TButton
    Left = 32
    Top = 80
    Width = 89
    Height = 33
    Caption = 'Change Caption'
    TabOrder = 2
    OnClick = ChangeCaptionButtonClick
  end
  object GetClientValueButton: TButton
    Left = 32
    Top = 119
    Width = 89
    Height = 33
    Caption = 'GetClient Value'
    TabOrder = 3
    OnClick = GetClientValueButtonClick
  end
  object Timer1: TTimer
    Interval = 10
    OnTimer = Timer1Timer
    Left = 360
    Top = 8
  end
end
