object DRServerForm: TDRServerForm
  Left = 0
  Top = 0
  Caption = 'Delay Response Server'
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
    Height = 336
    Lines.Strings = (
      'Zsserver4d is a server middleware. This demo only demonstrates the processing mechanism of delayed response'
      ''
      'After the delayresponse command is executed, it will not give feedback to the client immediately'
      'The delay response mechanism is implemented by state machine. Once the response stops, the instructions in the queue will be in the waiting state. The delay mechanism is mainly used for cross server communication or nonlinear process'
      ''
      'The attached client can be a mobile platform or a personal computer platform')
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
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 80
    Top = 144
  end
end
