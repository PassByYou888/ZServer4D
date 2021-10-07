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
  Position = poDesktopCenter
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
      'Fill indicates the memory filling operation, fillptr is the basic library support API, and fillchar is the Delphi / FPC built-in support API'
      'Copy refers to memory copy operation, copyptr is the basic library support API, and move is the Delphi / FPC built-in support API'
      ''
      'Because fill and copy are related to the performance of communication, graphics, statistics, AI and other advanced libraries, I wrote this demo'
      ''
      
        'Since the processes are all VM mechanisms, the test fill operation needs to distinguish between the initial speed and the take-off speed. The initial speed indicates that fill is executed for the first time, and the take-off speed indicates that fill is executed for the second time,' +
        'The takeoff speed also represents the normal speed. The fill test is calculated as the second time'
      'The copy operation does not need to distinguish between the initial speed and takeoff speed. This demo is a single line operation and only describes the basic library API. This demo cannot represent the hardware capability. Do not use this demo to evaluate the hardware'
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
