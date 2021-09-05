object DT_P2PVM_NoAuth_ServerForm: TDT_P2PVM_NoAuth_ServerForm
  Left = 0
  Top = 0
  Caption = 
    'DT Framework Server - p2pVM Double Tunnel NoAuth, create by.qq60' +
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
    Top = 32
    Width = 129
    Height = 33
    Caption = 'Start service.'
    TabOrder = 1
    OnClick = startservButtonClick
  end
  object stopservButton: TButton
    Left = 24
    Top = 71
    Width = 129
    Height = 33
    Caption = 'Stop service.'
    TabOrder = 2
    OnClick = stopservButtonClick
  end
  object netTimer: TTimer
    Interval = 10
    OnTimer = netTimerTimer
    Left = 224
    Top = 80
  end
end
