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
      'HPC background computing server demonstration'
      ''
      'The server resource time occupied by background operation is zero'
      ''
      'HPC background demonstration is very suitable for large-scale computing needs'
      ''
      'The technical mechanism mainly focuses on the large-scale operation on the server side. When the server receives a streamcmd command, the traditional processing method is that the server is blocked in the processing command, and other requests are waiting. We don'#39't let the server block at this moment. We use the delay technology to tell the background to suspend feedback'
      '(PauseResultSend)'
      'Then, we start a background thread and let the background continue to feed back after the thread execution is completed'
      '(ContinueResultSend)'
      ''
      'In the HPC background demonstration program, the above process is automated. It is very simple to use. Using this mode, you can accumulate code indefinitely'
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
