object EZServerForm: TEZServerForm
  Left = 0
  Top = 0
  Caption = 'Peformance Test Service'
  ClientHeight = 384
  ClientWidth = 1259
  Color = clBtnFace
  DoubleBuffered = True
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
  object readmeMemo: TMemo
    Left = 0
    Top = 0
    Width = 451
    Height = 384
    Align = alClient
    DoubleBuffered = True
    Lines.Strings = (
      'Zserver4d pressure test server'
      ''
      'Test method:'
      '1. Start the service'
      '2. Open the client and click build connect. Do not close the client until the processing is finished'
      ''
      'During the link, it is slow to create the link because it needs to shake hands to negotiate encryption and hash protocol. After the link is created successfully, the client can send a command to test, and the response speed is very fast. When the client exits, the server has no memory leakage')
    ParentDoubleBuffered = False
    TabOrder = 0
  end
  object StateMemo: TMemo
    Left = 451
    Top = 0
    Width = 202
    Height = 384
    Align = alRight
    DoubleBuffered = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Lines.Strings = (
      'StateMemo')
    ParentDoubleBuffered = False
    ParentFont = False
    TabOrder = 1
    WordWrap = False
  end
  object ReceiveMemo: TMemo
    Left = 653
    Top = 0
    Width = 202
    Height = 384
    Align = alRight
    DoubleBuffered = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Lines.Strings = (
      'StateMemo')
    ParentDoubleBuffered = False
    ParentFont = False
    TabOrder = 2
    WordWrap = False
  end
  object SendMemo: TMemo
    Left = 855
    Top = 0
    Width = 202
    Height = 384
    Align = alRight
    DoubleBuffered = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Lines.Strings = (
      'StateMemo')
    ParentDoubleBuffered = False
    ParentFont = False
    TabOrder = 3
    WordWrap = False
  end
  object CpuMemo: TMemo
    Left = 1057
    Top = 0
    Width = 202
    Height = 384
    Align = alRight
    DoubleBuffered = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Lines.Strings = (
      'StateMemo')
    ParentDoubleBuffered = False
    ParentFont = False
    TabOrder = 4
    WordWrap = False
  end
  object Timer: TTimer
    OnTimer = TimerTimer
    Left = 80
    Top = 144
  end
  object RefStateTimer: TTimer
    Interval = 2000
    OnTimer = RefStateTimerTimer
    Left = 320
    Top = 152
  end
end
