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
      #22522#20110'p2pVM'#30340#21452#36890#36947#26497#31616#26694#26550'.'
      ''
      '1,'#36991#20813#20351#29992#38459#22622#32593#32476#26426#21046
      '2,'#20013#23613#37327#20351#29992#24322#27493#20107#20214
      '3,VirtualAuth'#26381#21153#31471#25509#21463#19968#20999#30331#24405#29992#25143#21644#23494#30721
      '4,'#33258#21160#21270#25903#25345#30701#32447#37325#36830
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
