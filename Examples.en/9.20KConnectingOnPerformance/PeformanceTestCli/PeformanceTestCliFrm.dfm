object EZClientForm: TEZClientForm
  Left = 0
  Top = 0
  AutoSize = True
  BorderWidth = 20
  Caption = 'PeformanceTest Client'
  ClientHeight = 425
  ClientWidth = 409
  Color = clBtnFace
  DoubleBuffered = True
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
  object ConnectButton: TButton
    Left = 8
    Top = 27
    Width = 176
    Height = 35
    Caption = 'build 5000 connect'
    TabOrder = 0
    OnClick = ConnectButtonClick
  end
  object HostEdit: TLabeledEdit
    Left = 81
    Top = 0
    Width = 121
    Height = 21
    EditLabel.Width = 65
    EditLabel.Height = 13
    EditLabel.Caption = 'host address '
    LabelPosition = lpLeft
    TabOrder = 1
    Text = '127.0.0.1'
  end
  object TestCommandButton: TButton
    Left = 8
    Top = 68
    Width = 176
    Height = 66
    Caption = 'test command'
    TabOrder = 2
    Visible = False
    OnClick = TestCommandButtonClick
  end
  object Memo: TMemo
    Left = 0
    Top = 140
    Width = 409
    Height = 285
    Lines.Strings = (
      'Click build 10000 connect and wait a moment. If the machine configuration is not high, the client will get stuck more frequently'
      ''
      '1. In the middle, please open the resource monitor and locate it in the client process'
      '2. Pay attention to the system resource overhead'
      '3. In the middle of the pressure test, the client will get stuck with link failure'
      '4. The server will not get stuck'
      '5. After the link is completed, the test command button will be displayed'
      '6. Click test command, and the test over command will be printed together with the client ID'
      '7. It takes time for the server to feed back the print command'
      ''
      'The client uses the link pool to link. Each time the client links, it will shake hands with the server. Encryption and hash protocol are blocked on the client. The limit on the client is 100 concurrent per second, and the server is not subject to the concurrency limit'
      ''
      'by 2017-12-16')
    ReadOnly = True
    TabOrder = 3
    WordWrap = False
  end
  object disconnectButton: TButton
    Left = 190
    Top = 27
    Width = 176
    Height = 35
    Caption = 'disconnect'
    TabOrder = 4
    Visible = False
    OnClick = disconnectButtonClick
  end
  object Timer: TTimer
    OnTimer = TimerTimer
    Left = 177
    Top = 160
  end
end
