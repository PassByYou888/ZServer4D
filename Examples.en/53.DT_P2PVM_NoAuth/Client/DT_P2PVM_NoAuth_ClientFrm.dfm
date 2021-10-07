object DT_P2PVM_NoAuth_ClientForm: TDT_P2PVM_NoAuth_ClientForm
  Left = 0
  Top = 0
  Caption = 
    'DT Framework Client - p2pVM Double Tunnel NoAuth, create by.qq60' +
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
      'Dual channel minimalist framework based on p2pvm'
      ''
      'At TDT_ P2PVM_ NoAuth_ Avoid using blocking network mechanism in TDT in client_ P2PVM_ NoAuth_ Try to use asynchronous events in the client'
      ''
      'by.qq600585')
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object connButton: TButton
    Left = 16
    Top = 32
    Width = 129
    Height = 33
    Caption = 'Connection'
    TabOrder = 1
    OnClick = connButtonClick
  end
  object disButton: TButton
    Left = 16
    Top = 71
    Width = 129
    Height = 33
    Caption = 'Disconnect'
    TabOrder = 2
    OnClick = disButtonClick
  end
  object netTimer: TTimer
    Interval = 10
    OnTimer = netTimerTimer
    Left = 224
    Top = 80
  end
end
