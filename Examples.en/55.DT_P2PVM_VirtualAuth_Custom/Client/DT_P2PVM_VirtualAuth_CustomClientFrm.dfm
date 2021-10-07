object DT_P2PVM_VirtualAuth_ClientForm: TDT_P2PVM_VirtualAuth_ClientForm
  Left = 0
  Top = 0
  Caption = 
    'DT Framework Custom Client - p2pVM Double Tunnel VirtualAuth, cr' +
    'eate by.qq600585'
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
  object connButton: TButton
    Left = 16
    Top = 32
    Width = 129
    Height = 33
    Caption = 'Connection'
    TabOrder = 0
    OnClick = connButtonClick
  end
  object disButton: TButton
    Left = 16
    Top = 71
    Width = 129
    Height = 33
    Caption = 'Disconnect'
    TabOrder = 1
    OnClick = disButtonClick
  end
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
      '3. The virtuaauth server accepts all login users and passwords'
      '4. Automation supports short-term reconnection'
      ''
      'by.qq600585')
    ScrollBars = ssBoth
    TabOrder = 2
    WordWrap = False
  end
  object netTimer: TTimer
    Interval = 10
    OnTimer = netTimerTimer
    Left = 224
    Top = 80
  end
end
