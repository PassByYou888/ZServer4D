object DoubleServerForm: TDoubleServerForm
  Left = 0
  Top = 0
  Caption = 'Double Tunnel Server'
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
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 144
    Top = 40
    Width = 457
    Height = 241
    Lines.Strings = (
      'Zsserver4d is a server middleware, and the demo is in two-way mode'
      ''
      'The two-way mode interpretation can be explained by the client actively sending commands to the server for execution or the server actively initiating command execution to the client. In the link mechanism, two channels are used, one for receiving and one for sending. When both channels are successfully linked, the two-way bridge can be completed by using the tunnellink method'
      ''
      'The command system can be a simple string and various data packages, or a stream package. In short, any data transceiver lower than 500K, including small files, can use the command'
      ''
      'When the data length is very large, the bigstream mechanism must be used in zsserver4d to send and receive data'
      ''
      'The attached client can be a mobile platform or a personal computer platform'
      '')
    TabOrder = 0
  end
  object StartServiceButton: TButton
    Left = 32
    Top = 38
    Width = 89
    Height = 35
    Caption = 'start service'
    TabOrder = 1
    OnClick = StartServiceButtonClick
  end
  object ChangeCaptionButton: TButton
    Left = 32
    Top = 104
    Width = 89
    Height = 33
    Caption = 'Change Caption'
    TabOrder = 2
    OnClick = ChangeCaptionButtonClick
  end
  object GetClientValueButton: TButton
    Left = 32
    Top = 143
    Width = 89
    Height = 33
    Caption = 'GetClient Value'
    TabOrder = 3
    OnClick = GetClientValueButtonClick
  end
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 360
    Top = 8
  end
end
