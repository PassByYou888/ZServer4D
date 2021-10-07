object FragmentClientForm: TFragmentClientForm
  Left = 0
  Top = 0
  AutoSize = True
  BorderWidth = 20
  Caption = 'Multi line file download demo'
  ClientHeight = 385
  ClientWidth = 944
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Memo: TMemo
    Left = 0
    Top = 40
    Width = 545
    Height = 345
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clLime
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Lines.Strings = (
      
        'The demo demonstrates multi line file download (similar to Xunle' +
        'i)'
      
        'The multi line download mechanism of the demo is the standard me' +
        'chanism. Look at the code more, especially the data structure de' +
        'sign. The transmission mechanism of the demo is completebuffer, ' +
        'which needs to control the memory overhead'
      
        'The demo server will not temporarily store all files. The file d' +
        'ata temporarily stored by the server depends on the download thr' +
        'ead. It will not explode'
      'by.qq600585'
      '')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object HostEdit: TLabeledEdit
    Left = 40
    Top = 4
    Width = 161
    Height = 21
    EditLabel.Width = 29
    EditLabel.Height = 13
    EditLabel.Caption = 'Host: '
    LabelPosition = lpLeft
    TabOrder = 1
    Text = '127.0.0.1'
  end
  object connButton: TButton
    Left = 207
    Top = 0
    Width = 170
    Height = 25
    Caption = 'Create 10 physical connections'
    TabOrder = 2
    OnClick = connButtonClick
  end
  object downloadButton: TButton
    Left = 383
    Top = 0
    Width = 129
    Height = 25
    Caption = 'Multithreaded Download'
    TabOrder = 3
    OnClick = downloadButtonClick
  end
  object stateMemo: TMemo
    Left = 551
    Top = 40
    Width = 393
    Height = 345
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clLime
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
  end
  object Timer1: TTimer
    Interval = 10
    OnTimer = Timer1Timer
    Left = 152
    Top = 120
  end
  object checkDownTimer: TTimer
    OnTimer = checkDownTimerTimer
    Left = 152
    Top = 192
  end
end
