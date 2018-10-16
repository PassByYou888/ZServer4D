object DoubleServerForm: TDoubleServerForm
  Left = 0
  Top = 0
  Caption = 'HPC Server'
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
    Height = 305
    Lines.Strings = (
      'HPC'#21518#21488#36816#31639#26381#21153#22120#28436#31034
      ''
      #21518#21488#36816#31639#21344#29992#26381#21153#22120#36164#28304#26102#38388#20026#38646
      ''
      'HPC'#21518#21488#28436#31034#38750#24120#36866#21512#22823#35268#27169#36816#31639#38656#27714
      ''
      #25216#26415#26426#21046#20027#35201#38598#20013#20110#26381#21153#22120#31471#30340#22823#35268#27169#36816#31639
      #24403#26381#21153#22120#25910#21040#19968#26465'StreamCMD'#27169#24335#30340#21629#20196#26102#65292#20256#32479#30340#22788#29702#21150#27861#20026#22788#29702#21629#20196#20013#26381#21153#22120#26159
      #38459#22622#30340#65292#21035#30340#35831#27714#37117#20250#22312#31561#24453#20013
      #25105#20204#22312#27492#21051#19981#35753#26381#21153#22120#38459#22622#65292#25105#20204#20351#29992#24310#36831#25216#26415#65292#21578#35785#21518#21488#26242#20572#21453#39304
      '(PauseResultSend)'
      #28982#21518#65292#25105#20204#20877#24320#21551#19968#20010#21518#21488#32447#31243#65292#24453#32447#31243#25191#34892#23436#25104#21518#65292#20877#35753#21518#21488#32487#32493#21453#39304
      '(ContinueResultSend)'
      ''
      #22312'HPC'#21518#21488#28436#31034#31243#24207#20013#65292#20197#19978#27969#31243#26159#33258#21160#21270#36827#34892#30340#65292#23427#20351#29992#24456#31616#21333#65292#20351#29992#35813#27169#24335#21487#20197
      #26080#38480#22534#31215#20195#30721
      ''
      ''
      'by.600585'
      '2018-5-22')
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
