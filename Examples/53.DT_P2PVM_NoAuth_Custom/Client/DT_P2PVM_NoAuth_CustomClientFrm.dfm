object DT_P2PVM_NoAuth_ClientForm: TDT_P2PVM_NoAuth_ClientForm
  Left = 0
  Top = 0
  Caption = 
    'DT Framework Custom Client - p2pVM Double Tunnel NoAuth, create ' +
    'by.qq600585'
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
      #22522#20110'p2pVM'#30340#21452#36890#36947#26497#31616#26694#26550'.'
      ''
      #22312'TDT_P2PVM_NoAuth_Client'#20013#36991#20813#20351#29992#38459#22622#32593#32476#26426#21046
      #22312'TDT_P2PVM_NoAuth_Client'#20013#23613#37327#20351#29992#24322#27493#20107#20214'.'
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
    Top = 119
    Width = 129
    Height = 33
    Caption = 'Disconnect'
    TabOrder = 2
    OnClick = disButtonClick
  end
  object phyconnButton: TButton
    Left = 16
    Top = 71
    Width = 129
    Height = 33
    Caption = 'Physics Connection'
    TabOrder = 3
    OnClick = phyconnButtonClick
  end
  object netTimer: TTimer
    Interval = 10
    OnTimer = netTimerTimer
    Left = 224
    Top = 80
  end
end
