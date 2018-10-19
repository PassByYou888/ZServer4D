object CustomProtocolForm: TCustomProtocolForm
  Left = 0
  Top = 0
  Caption = 'Custom Protocol support...'
  ClientHeight = 412
  ClientWidth = 852
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Memo: TMemo
    Left = 0
    Top = 57
    Width = 852
    Height = 355
    Align = alClient
    BorderStyle = bsNone
    Lines.Strings = (
      #21046#20316#22806#37096#23450#21046#21270#21327#35758#30340#26631#20934#26381#21153#22120#33539#24335
      #25105#20204#22522#20110#27492#33539#24335#21487#20197#33258#34892#24320#21457#23646#20110#33258#24049#30340'FTP,HTTP'#31561#31561#21327#35758
      #22522#20110'ZS'#24320#21457#33258#24049#30340#22806#37096#21327#35758#65292#21487#20197#22825#28982#25903#25345#39640#24182#21457#21644#20113#21518#21488
      ''
      #22522#20110'ZS'#24320#21457#22806#37096#21327#35758#65292#20027#35201#26159#20860#23481#20854#23427#19977#26041#36890#35759#31471
      #22240#27492#65292#23458#25143#31471#38656#35201#33258#34892#35299#20915#65292#27604#22914#36873#25321'Indy'#20316#20026#36890#35759#23458#25143#31471
      ''
      'by.qq 600585'
      '2018-1-24')
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 852
    Height = 57
    Align = alTop
    BorderStyle = bsSingle
    TabOrder = 1
    object connectOnIndyButton: TButton
      Left = 16
      Top = 15
      Width = 105
      Height = 25
      Caption = 'connect on indy'
      TabOrder = 0
      OnClick = connectOnIndyButtonClick
    end
    object SendDataOnIndyButton: TButton
      Left = 127
      Top = 15
      Width = 122
      Height = 25
      Caption = 'send data on indy'
      TabOrder = 1
      OnClick = SendDataOnIndyButtonClick
    end
    object connectOnZServerButton: TButton
      Left = 336
      Top = 15
      Width = 121
      Height = 25
      Caption = 'connect on ZServer'
      TabOrder = 2
      OnClick = connectOnZServerButtonClick
    end
    object SendDataOnZServerButton: TButton
      Left = 463
      Top = 15
      Width = 138
      Height = 25
      Caption = 'send data on ZServer'
      TabOrder = 3
      OnClick = SendDataOnZServerButtonClick
    end
  end
  object Timer: TTimer
    Interval = 100
    OnTimer = TimerTimer
    Left = 296
    Top = 168
  end
  object IdTCPClient1: TIdTCPClient
    ConnectTimeout = 0
    Host = '127.0.0.1'
    IPVersion = Id_IPv4
    Port = 9989
    ReadTimeout = -1
    Left = 424
    Top = 208
  end
end
