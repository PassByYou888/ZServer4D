object VMCliForm: TVMCliForm
  Left = 0
  Top = 0
  Caption = 'VM'#23458#25143#31471'...'
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
    Width = 185
    Height = 470
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object OriginDataLabel: TLabel
      Left = 16
      Top = 390
      Width = 36
      Height = 13
      Caption = #24515#36339#21253
    end
    object CreateVMButton: TButton
      Left = 16
      Top = 51
      Width = 139
      Height = 25
      Caption = #21019#24314'VM'#32593#32476
      TabOrder = 0
      OnClick = CreateVMButtonClick
    end
    object AddrEdit: TLabeledEdit
      Left = 76
      Top = 24
      Width = 79
      Height = 21
      EditLabel.Width = 48
      EditLabel.Height = 13
      EditLabel.Caption = #29289#29702#22320#22336
      LabelPosition = lpLeft
      TabOrder = 1
      Text = '127.0.0.1'
    end
    object VMAddrEdit: TLabeledEdit
      Left = 76
      Top = 128
      Width = 87
      Height = 21
      EditLabel.Width = 66
      EditLabel.Height = 13
      EditLabel.Caption = 'VM'#22320#22336'(ipv6)'
      LabelPosition = lpLeft
      TabOrder = 2
      Text = '::99'
    end
    object ConnectVMButton: TButton
      Left = 16
      Top = 155
      Width = 139
      Height = 25
      Caption = #38142#25509'VM'#22320#22336
      TabOrder = 3
      OnClick = ConnectVMButtonClick
    end
    object TestButton: TButton
      Left = 16
      Top = 247
      Width = 139
      Height = 25
      Caption = #20248#38597#27979#35797'VM'#24615#33021
      TabOrder = 4
      OnClick = TestButtonClick
    end
    object DisconnectButton: TButton
      Left = 16
      Top = 182
      Width = 139
      Height = 25
      Caption = #26029#24320'VM'#22320#22336
      TabOrder = 5
      OnClick = DisconnectButtonClick
    end
    object StatusCheckBox: TCheckBox
      Left = 16
      Top = 331
      Width = 97
      Height = 17
      Caption = #29366#24577
      Checked = True
      State = cbChecked
      TabOrder = 6
    end
    object MaxTestButton: TButton
      Left = 16
      Top = 278
      Width = 139
      Height = 25
      Caption = #36807#20998#27979#35797'VM'#24615#33021'(x64)'
      TabOrder = 7
      OnClick = MaxTestButtonClick
    end
  end
  object Memo: TMemo
    Left = 185
    Top = 0
    Width = 410
    Height = 470
    Align = alClient
    ScrollBars = ssVertical
    TabOrder = 1
    WordWrap = False
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
