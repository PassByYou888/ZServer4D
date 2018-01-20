object MHMainForm: TMHMainForm
  Left = 0
  Top = 0
  Caption = 'MemoryHook demo'
  ClientHeight = 680
  ClientWidth = 1080
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
    Top = 0
    Width = 1080
    Height = 600
    Align = alClient
    BorderStyle = bsNone
    Font.Charset = ANSI_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'Consolas'
    Font.Style = []
    Lines.Strings = (
      #22312'ZS'#24037#31243#20869#37096#26377'4'#27425#20869#23384#38057#23376#21103#26412#65292#21487#36328#25152#26377#24179#21488#65292#24182#19988#24615#33021#26497#22909
      #21407#29702#26159#36890#36807#21246#20303#20869#23384#31649#29702#21333#20803#65292#35760#24405#20998#37197#22320#22336#65292#20197#27492#26469#25511#21046#25105#20204#31243#24207#30340#20869#23384#24320#38144
      ''
      #22330#26223'1'#65306
      #24403#25105#20204#21019#24314#19968#20010#31867#26102#65292#25105#20204#21246#20303#23427#65292#24182#19988#30417#35270#35813#31867#30340#20869#23384#24320#38144
      ''
      #22330#26223'2'#65306
      #25105#20204#22312#26381#21153#22120#20013#65292#21487#20197#21246#20303#19968#20010#20989#25968#65292#24403#20989#25968#36864#20986#26102#65292#25105#20204#20877#36890#36807#35745#31639#27492#20989#25968#24050#20998#37197#30340#31354#38388#65292#26368#21518#65292#25105#20204#21487#20197#21028#26029#20986#27492#20989#25968#26159#21542#26377#27844#28431
      ''
      #22330#26223'3'#65306
      #22312'ZDBEngine'#20013#22823#37327#20351#29992#20102#20869#23384#38057#23376#26469#31649#29702'Cache'#20869#23384#24320#38144
      ''
      'MH.Pas '#29992#20110#25209#37327#31649#29702#30340#20869#23384#38057#23376
      'MH_1.pas '#31532#19968#27425#38057#20303
      'MH_2.pas '#31532#20108#27425#38057#20303
      'MH_3.pas '#31532#19977#27425#38057#20303
      ''
      #19979#38754#26159'DoStatus'#20449#24687)
    ParentColor = True
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object Panel1: TPanel
    Left = 0
    Top = 600
    Width = 1080
    Height = 80
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object Button1: TButton
      Left = 8
      Top = 16
      Width = 121
      Height = 41
      Caption = #20989#25968#35843#29992#30417#35270
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 135
      Top = 16
      Width = 266
      Height = 41
      Caption = #35745#31639'Record'#30340#30495#23454#20869#23384#23610#23544#24182#19988#26292#21147#37322#25918
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 407
      Top = 16
      Width = 257
      Height = 41
      Caption = #39640#24615#33021#38057#23376#20248#21270#65292#24212#29992#20110#39640#39057#29575#20869#23384#32479#35745
      TabOrder = 2
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 680
      Top = 16
      Width = 210
      Height = 41
      Caption = #35760#24405#22823#25209#37327#20869#23384#30003#35831
      TabOrder = 3
      OnClick = Button4Click
    end
    object Button5: TButton
      Left = 912
      Top = 16
      Width = 153
      Height = 41
      Caption = #20005#26684#20869#23384#30417#25511
      TabOrder = 4
      OnClick = Button5Click
    end
  end
end
