object VMCliForm: TVMCliForm
  Left = 0
  Top = 0
  Caption = 'VM client'
  ClientHeight = 470
  ClientWidth = 797
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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 209
    Height = 470
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object OriginDataLabel: TLabel
      Left = 16
      Top = 390
      Width = 52
      Height = 13
      Caption = 'Heartbeat '
    end
    object CreateVMButton: TButton
      Left = 4
      Top = 51
      Width = 193
      Height = 25
      Caption = 'Create VM network'
      TabOrder = 0
      OnClick = CreateVMButtonClick
    end
    object AddrEdit: TLabeledEdit
      Left = 76
      Top = 24
      Width = 79
      Height = 21
      EditLabel.Width = 79
      EditLabel.Height = 13
      EditLabel.Caption = 'Physical address'
      LabelPosition = lpLeft
      TabOrder = 1
      Text = '127.0.0.1'
    end
    object VMAddrEdit: TLabeledEdit
      Left = 76
      Top = 128
      Width = 87
      Height = 21
      EditLabel.Width = 88
      EditLabel.Height = 13
      EditLabel.Caption = 'VM address (IPv6)'
      LabelPosition = lpLeft
      TabOrder = 2
      Text = '::99'
    end
    object ConnectVMButton: TButton
      Left = 4
      Top = 155
      Width = 193
      Height = 25
      Caption = 'Link VM address'
      TabOrder = 3
      OnClick = ConnectVMButtonClick
    end
    object TestButton: TButton
      Left = 4
      Top = 247
      Width = 193
      Height = 25
      Caption = 'Elegant test VM performance'
      TabOrder = 4
      OnClick = TestButtonClick
    end
    object DisconnectButton: TButton
      Left = 4
      Top = 182
      Width = 193
      Height = 25
      Caption = 'Disconnect VM address'
      TabOrder = 5
      OnClick = DisconnectButtonClick
    end
    object StatusCheckBox: TCheckBox
      Left = 16
      Top = 331
      Width = 97
      Height = 17
      Caption = 'state'
      Checked = True
      State = cbChecked
      TabOrder = 6
    end
    object MaxTestButton: TButton
      Left = 4
      Top = 278
      Width = 193
      Height = 25
      Caption = 'Over testing VM performance (x64)'
      TabOrder = 7
      OnClick = MaxTestButtonClick
    end
  end
  object Memo: TMemo
    Left = 209
    Top = 0
    Width = 386
    Height = 470
    Align = alClient
    ScrollBars = ssVertical
    TabOrder = 1
    WordWrap = False
    ExplicitLeft = 185
    ExplicitWidth = 410
  end
  object StateMemo: TMemo
    Left = 595
    Top = 0
    Width = 202
    Height = 470
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
  object ProgressTimer: TTimer
    Interval = 500
    OnTimer = ProgressTimerTimer
    Left = 296
    Top = 64
  end
  object PrintStateTimer: TTimer
    Interval = 2000
    OnTimer = PrintStateTimerTimer
    Left = 288
    Top = 128
  end
end
