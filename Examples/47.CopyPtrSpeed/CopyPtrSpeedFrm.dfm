object CopyPtrSpeedForm: TCopyPtrSpeedForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  BorderWidth = 20
  Caption = 'CopyPtr Speed. Create by.qq600585'
  ClientHeight = 312
  ClientWidth = 870
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 55
    Width = 870
    Height = 257
    Align = alBottom
    Lines.Strings = (
      'Fill'#34920#31034#20869#23384#22635#20805#25805#20316',FillPtr'#26159#22522#30784#24211#25903#25345'API,FillChar'#26159'Delphi/FPC'#20869#32622#25903#25345'API'
      'Copy'#34920#31034#20869#23384#22797#21046#25805#20316',CopyPtr'#26159#22522#30784#24211#25903#25345'API,Move'#26159'Delphi/FPC'#20869#32622#25903#25345'API'
      ''
      #22240#20026'Fill'#19982'Copy'#20851#31995#21040#36890#35759','#22270#24418','#32479#35745#23398',AI'#31561#31561#39640#32423#24211#30340#24615#33021#38382#39064','#22240#27492#25105#32534#20889#20102#35813'Demo'
      ''
      
        #30001#20110#36827#31243#37117#26159'VM'#26426#21046','#27979#35797'Fill'#25805#20316#38656#35201#21306#20998#21021#22987#36895#24230#21644#36215#39134#36895#24230','#21021#22987#36895#24230#34920#31034#39318#27425#25191#34892'Fill,'#36215#39134#36895#24230#34920#31034#31532#20108#27425#25191#34892'Fill,' +
        #36215#39134#36895#24230#20063#34920#31034#27491#24120#36895#24230'.fill'#27979#35797#25353#31532#20108#27425
      #35745#31639'.'
      'Copy'#25805#20316#19981#38656#35201#21306#20998#21021#22987#36895#24230#21644#36215#39134#36895#24230
      #26412'Demo'#20026#21333#32447#25805#20316','#21482#35828#26126#22522#30784#24211'api,'#26412'Demo'#19981#33021#20195#34920#30828#20214#33021#21147'.'#35831#21247#20351#29992#26412'Demo'#35780#20272#30828#20214'.'
      ''
      'by.qq600585'
      '')
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Button8GReadWrite: TButton
    Left = 8
    Top = 8
    Width = 121
    Height = 33
    Caption = '8GB Read/Write'
    TabOrder = 1
    OnClick = Button8GReadWriteClick
  end
  object doStatusTimer: TTimer
    Interval = 100
    OnTimer = doStatusTimerTimer
    Left = 448
    Top = 176
  end
end
