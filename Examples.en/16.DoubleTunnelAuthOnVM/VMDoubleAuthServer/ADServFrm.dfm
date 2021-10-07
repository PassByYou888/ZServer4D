object AuthDoubleServerForm: TAuthDoubleServerForm
  Left = 0
  Top = 0
  Caption = 'VM Auth Double Tunnel Server'
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
      'Zserver4d is a server middleware'
      ''
      'Tunnel principle:'
      'Vmtunnel is our tunnel server. We trigger the request by intercepting the built-in tunnel of the server. In the request, we can dynamically bind our recv + send channels'
      ''
      ''
      'If the bound channel is a server type, one to many to many can be the same server, and can be bound to multiple VM tunnels to realize VM virtual tunnel service. VM virtual tunnel service has no limit on the number of links, and a virtual tunnel can carry more than 1 million links'
      ''
      
        'Once the tunnel is established successfully, vmtunnel can also send and receive commands normally. The tunnel binding of recvtunnel + sendtunnel will not affect vmtunn' +
        'el'
      'Once the tunnel is established successfully, the vmtunnel protocol will change. It is not a special case. Do not cancel the tunnel easily'
      '')
    ScrollBars = ssVertical
    TabOrder = 0
    WordWrap = False
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
