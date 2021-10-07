object DRClientForm: TDRClientForm
  Left = 0
  Top = 0
  Caption = 'DRClient'
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
    Left = 127
    Top = 35
    Width = 457
    Height = 241
    Lines.Strings = (
      'Zsserver4d is a server middleware. This demo demonstrates the processing mechanism of delayed response using anonymous functions'
      ''
      'After the delayresponse command is executed, it will not give feedback to the client immediately'
      'The delay response mechanism is implemented by state machine. Once the response stops, the instructions in the queue will be in the waiting state. The delay mechanism is mainly used for cross server communication or nonlinear process'
      ''
      'The attached client can be a mobile platform or a personal computer platform'
      ''
      '')
    TabOrder = 0
  end
  object ConnectButton: TButton
    Left = 32
    Top = 38
    Width = 89
    Height = 35
    Caption = 'connect'
    TabOrder = 1
    OnClick = ConnectButtonClick
  end
  object HostEdit: TLabeledEdit
    Left = 136
    Top = 8
    Width = 121
    Height = 21
    EditLabel.Width = 65
    EditLabel.Height = 13
    EditLabel.Caption = 'host address '
    LabelPosition = lpLeft
    TabOrder = 2
    Text = '127.0.0.1'
  end
  object DelayResponseBtn: TButton
    Left = 32
    Top = 95
    Width = 89
    Height = 34
    Caption = 'Send Request'
    TabOrder = 3
    OnClick = DelayResponseBtnClick
  end
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 344
    Top = 16
  end
end
